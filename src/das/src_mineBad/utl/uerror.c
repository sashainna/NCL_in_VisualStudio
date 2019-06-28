/*********************************************************************
**    NAME         :  uerror.c -- error handler
**       CONTAINS:
** 	uu_erlog(subsys,errno1,funcname,argptr) -- error logger.
** 	static int uerget(subsys,errno1,sever,erclass,str)
** 	uu_erprt(subsys,erclass,sever,errno1,funcname,str,argptr) -- error print.
**
**	The text of the error messages are stored on error message files, 
**	one file per sub-system. Sub-systems are GKS, DAS, etc.
**	To make an error message file:
**	1. Use any text editor and write the error messages, one per
**		line, onto a file. Start each line with two numbers,
**		1st=severity level, 2nd=error class (0-19).
**		After printing the error message, if
**		the severity level is greater than 8, uu_erlog aborts the process.
**		See UESLOG below for how to use error class numbers and specify your own
**		custom error logger. If you don't want to bother with this, just use
**		error class 0 for all your error messages.
**	2. Run the program "erindex textfile indexfile" (found in /usr/local/bin)
**		to create an indexed file for your sub-system.
**		Textfile is the name of your edited file of error messages.
**		The indexfile should be:
**		gkserrors for gks, daserrors for das,
**		moderrors for modelling,
**		featerrors for features, calcerrors for calculator,
**		conserrors for construction, dbmserrors for dbms.
**		If you need to add another subsystem, see Griff Hamlin.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uerror.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:53
*********************************************************************/
#include "udebug.h"
#include "umath.h"
#include <stdio.h>
#include "usysdef.h"
#include "dinput.h"
#include "uio.h"
#include "uerror.h"
#include "xenv1.h"
#include "xfsys0.h"
static int (*logentry[ERSYSNO][20])()={	/* pointers to user supplied loggers*/
			UU_NULL,NULL,NULL,NULL,NULL,NULL,NULL,
			UU_NULL,NULL,NULL,NULL,NULL,NULL,NULL,
			UU_NULL,NULL,NULL,NULL,NULL,NULL,NULL,
			UU_NULL,NULL,NULL,NULL,NULL,NULL,NULL,
			UU_NULL,NULL,NULL,NULL,NULL,NULL,NULL,
			UU_NULL,NULL,NULL,NULL,NULL};
/********************************************************************* 
**  E_FUNCTION: uu_erlog(subsys,errno1,funcname,argptr)
**
** description: UNICAD Error Logger. 
**		This is the single routine which reports all errors. 
**		uu_erlog first gets the error message index from the file. If
**		an application supplied error logger has been specified for
**		this class of error, uu_erlog calls it. Otherwise 
**		uu_erlog prints	the text of the error message on the UD_EO 
**		(error output) device, calling on the I/O RE-DIRECT system 
**		to actually do the output.  
**
**  PARAMETERS   
**      input:  SUBSYS -- which sub-system generated the error.
**									One of GKS, DAS, MOD, FEAT, CALC, CON, DBMS. 
**									These symbols are defined in "exp/lib/uerror.h"
**									which you should include.
**					errno1 -- the number of the error.
**					FUNCNAME -- the name of the function in which the
**									error was detected.
**					ARGPTR -- a pointer to an argument structure. This
**								pointer is passed to any application 
**								supplied error logger, but is not used by
**								UERLOG itself.
**
**      OUTPUT: none
**  RETURNS      :  severity of error.
**  SIDE EFFECTS :  displays the error message on the current UD_EO
**							device, or calls an application supplied error
**							logging routine.
**  WARNINGS     :  none
*********************************************************************/
int uu_erlog(subsys,errno1,funcname,argptr)
int subsys,			/* subsystem detecting the error */
	errno1;			/* error number */
char funcname[];	/* name of function detecting the error */
char *argptr;		/* pointer to error-specific arguments */
						/* don't know what argptr points to */
{
	int sever,erclass;		/* error severity and class */
	int i;
	char str[120];
	if ((subsys<0)||(subsys>=ERSYSNO)) {
		sprintf(str,"uu_erlog -- panic. subsys %d not valid\n",subsys);
		ud_write(UD_EO,strlen(str),str);
		return(12);
	}
	/* get severity, class, and text of message */
	i=uerget(subsys,errno1,&sever,&erclass,str);
	/* log the error message */
	if (i==1) uu_erprt(subsys,erclass,sever,errno1,funcname,str,argptr);
	return(sever);
}

/*********************************************************************
**    I_FUNCTION     :  static int uerget(subsys,errno1,sever,erclass,str)
**			get message number errno1 for subsystem subsys.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  int *sever -- severity level of error, or 12 if
**										uerget returns 0.
**    RETURNS      : 1 if all went OK, else return 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int uerget(subsys,errno1,sever,erclass,str)
/* get message number errno1 for subsystem subsys.  Return
	severity and class and message text */
int subsys,						/* sub-system */
	errno1,						/* error number */
	*sever,						/* severity */
	*erclass;					/* error class */
char str[];						/* message text */
{
	static long *indx[ERSYSNO]={ /* pointer to index for each sub-system*/
								UU_NULL,NULL,NULL,NULL,NULL};	
	static int indxlen[ERSYSNO]={0,0,0,0,0,0,0};	/* length of each index */
	static char fnames[ERSYSNO][20]={	/* file names of er index and text */
				"gkserrors","daserrors","moderrors","featerrors","calcerrors",
				"conserrors","dbmserrors"};
	static FILE *fp[ERSYSNO];			/* file descriptors of error files */
	char *ux_getenv();
	int fdp;
	int nfound;
	long *ptr;
	char *p;
	char str2[120];
	int offset,len,i,l;
	char us[120];

	*sever=12;
	if (indx[subsys]==UU_NULL) {	/* read in index for sub-system */

		/* Open the file, return 0 on error */
		if( ux_open_to_data( fnames[subsys], "r", "STREAM", 
							"ASCII", &fdp, UX_PRTERRS ) != UU_SUCCESS ) {
			sprintf(str,"ERROR uu_erlog. can't open file %s\n\0", fnames[subsys]);
			uu_dprint(-1,(us,"%s", str));
			ud_write(UD_EO,strlen(str),str);
			return(0);
		}

		/* Read length */
		ux_get_os_filedesc(fdp, &fp[subsys], UX_PRTERRS);
		UX_FSCANF0((fp[subsys],"%d",&indxlen[subsys]),nfound);

		/* allocate space for this index */
		indx[subsys]=
				(long *)(uu_malloc(indxlen[subsys]*sizeof(long)));
		ptr=indx[subsys];
		for (i=0; i<indxlen[subsys]; i++) {
			UX_FSCANF0((fp[subsys],"%d",ptr), nfound);
			ptr++;		/* C increments ptr by sizeof(long) */
		}
	}

	/* read the error message from the file */
	offset=(*(indx[subsys]+errno1));		/* C will mult errno1 by 
														sizeof(Uerindex) */
	len=(*(indx[subsys]+errno1+1))-offset-1;
	ux_fseek0(fp[subsys], offset, 0);
	ux_fread0(str2, sizeof(char), len, fp[subsys], &l);
	if (l!=len) {
		sprintf(str2,"ERROR uu_erlog. can't read error message\n\0");
		ud_write(UD_EO,strlen(str2),str2);
		return(0);
	}
	str2[len]='\n';
	str2[len+1]='\0';
	len++;
	sscanf(str2,"%d %d",sever,erclass);
	if ((*sever<0)||(*erclass<0)||(*erclass>19)) {
		sprintf(str2,"uu_erlog - panic. sever=%d, class=%d\n\0",sever,erclass);
		ud_write(UD_EO,strlen(str2),str2);
		return(0);
	}
	p=str2; i=0;
	while (i<2) { if (*p==' ') i++; p++;}	/* find 2nd blank */
	strcpy(str,p);				/* return msg text to caller */
	return(1);
}

/*********************************************************************
**    E_FUNCTION   :  uu_erprt(subsys,erclass,sever,errno1,funcname,str,argptr)
**			Log an error message.
**    PARAMETERS   
**       INPUT  : 
**				int subsys			Subsystem this error came from.          
**				int erclass			The error class of this error.  Users of
**										this function may provide a separate error
**										handler for each error class.
**				int sever			Error severity.  Passed on to user supplied
**										error handlers.
**				int errorno			Error number.  Indicates which text message
**										to be used for this error.
**				char functname[]  Name of the function which detected the error.
**				char str[]			Additional error message for this error.
**				int *argptr			Points to function argument which caused 
**										the error, this is passed on to user supplied
**										error handlers.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Prints the error message in the trace file,
**							or calls the user-supplied error logger.
**    WARNINGS     : none
*********************************************************************/
uu_erprt(subsys,erclass,sever,errno1,funcname,str,argptr)
/* log the error message */
int subsys,erclass,sever,errno1;
char funcname[],str[];
int *argptr;
{
	static char subsysname[ERSYSNO][5]={"gks","das","mod","feat","calc",
													"cons","dbms"};
	char str3[120];

	uu_denter(UU_UITRC,(us,"uu_erprt()"));

	/* see if a user-supplied logging routine has been supplied */
	if (logentry[subsys][erclass]==UU_NULL) {
		sprintf(str3,"%s error %d from %s: ",
					&subsysname[subsys][0],errno1,funcname);

		ud_write(UD_EO,strlen(str3),str3);	/* write to UD_EO */
		ud_write(UD_EO,strlen(str),str);
		ud_write(UD_EO,1,"\n");
	}
	else {			/* call user supplied logger */
		(*logentry[subsys][erclass])(errno1,str,sever,funcname,argptr);
	}
	uu_dexit;
}

/********************************************************************* 
**  NAME: UESLOG.C 
**
** description: 
**	This routine allows a sub-system to supply its own error logging
**	routine, to be used in place of UERLOG, for a certain class of
**	errors from that sub-system. UESLOG is found in "exp/lib/ulib.a",
**	as is UERLOG.
**
**  PARAMETERS   
**      input:  SUBSYS -- which sub-system generated the error.
**									Either GKS, DAS, MOD, etc. These symbols are
**									defined in "exp/lib/uerror.h" which you should
**									include.
**					ERCLASS -- class of error.
**					ENTRYPT -- address of your error logging routine. This will
**									be called whenever the proper class error 
**									occurs. This routine will be passed the following
**									5 arguments:
**									int errno1 -- number of the error.
**									char *STR -- pointer to text of the error message.
**									int SEVER -- severity of the error.
**									char *FUNCNAME -- name of the function detecting error.
**									int * ARGPTR -- pointer which was passed to
**										UERHND. This presumably points to a data
**										structure to be used by your error logger.
**
**      OUTPUT: none
**  RETURNS      :  severity of error.
**  SIDE EFFECTS :  Your ENTRYPT routine may be called any time.
**  WARNINGS     :  none
*********************************************************************/
uu_eslog(subsys,erclass,entrypt)	/* set or unset a user
												supplied error logger */
int subsys,					/* the sub-system */
	erclass;					/* the class of error (0-19) this entry handles*/
int (*entrypt)();			/* pointer to user supplied logger */
{
	char str[120];
	if ((subsys<0)||(subsys>1)||(erclass<0)||(erclass>19)) {
		sprintf(str,"uu_eslog. bad parameters. subsys=%d, class=%d\n\0",
						subsys,erclass);
		ud_write(UD_EO,strlen(str),str);
	}
	else {					/* parameters oK */
		logentry[subsys][erclass]=entrypt;
	}
}

