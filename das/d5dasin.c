/*********************************************************************
**    NAME         :  	d5dasin.c
**       CONTAINS:
**				ud_dasin		udi_dlxconread
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d5dasin.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:10
*********************************************************************/

#include 	"ustdio.h"
#include		"usysdef.h"
#include "umath.h"
#include		"calcom.h"
#include		"ddef.h"
#include		"uhep.h"
#include		"tlangtool.h"
#include 	"udebug.h"
#include 	"xenv1.h"

/*static int lexicon[3000];	/* big enough to hold the lexicon */
static	int *lexicon;
static int symbol[10]={' ','<','s','>'};		/* name of distinguished symbol, 
									<s>, in internal langpak code */
static int	symlen=3;				/* length of symbol */
static int trace;				/* 1=tracing on, 0=no tracing */
int  UDI_der1;					/* error flag for undefined symbol */
int  UDI_dtypeflag;			/* das input value type - distance, angle or 
									unitless  */

#include		"dassem.h"
#include		"dvmatch.h"

int *ut_lxread();
int *udi_dlxconread();

/*********************************************************************
**    I_FUNCTION     :  ud_dasin(ibuf, dsda, dasflag )
**       main subroutine of the das input. Parse the input and return
**       the result according to the input format.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ud_dasin(ibuf, dsda, dasflag )
char	ibuf[];
UD_DASDATA	 *dsda;
int  dasflag;

{
	static int dready = UU_FALSE;
	static int one=1;		/* static ok since never change value */
	int stack[300];		/* stack area for uti_parser to use */
	ITSTYPE its[300];		/* uti_parser returns translation sequence here*/
	int ltr;					/* length of returned translation sequence */
	int irc;					/* return code from ut_lxread or uti_parser */
	int in[200];			/* user input buffer */
	int len;					/* length of fname or input buffer */
	int irkf;				/* in[irkf-1] contains index of last
									matched character position of parse */
	int irtn;				/* return code */
	uu_denter(UU_DTRC,(us,"ud_dasin() starting, %s", ibuf));
	/*uu_dprint(UU_DTRC,(us,"ud_dasin() starting, %s", ibuf));*/
   if (dready==UU_FALSE)
		lexicon=udi_dlxconread( &dready,"M_DASGRAM" );
	symbol[1]='<'; symbol[2]='s'; symbol[3]='>';
	symlen=3;
	irc=0;
	UDI_dtypeflag = dasflag;
	len = strlen (ibuf);
	ut_cin_intern(in,len,ibuf,1);
   udi_dinitflag();
	udi_dresetstack();

				 /* call the langpak uti_parser routine to parse input */
	uti_parser(symbol,symlen,in,one,len,lexicon,stack,&trace,
				&irkf,its,&ltr,&irc,semmch,vmatch,NULL);

		/* irc=0 if parse successful, else see page 128 of langpak book*/
	if (irc == 1)
		/* derror (" error - syntax error "); */
		uu_uerror0(DASINERROR, 1);
	 if ((irc == 1) || (UDI_der1==UU_TRUE)) {
			irtn=UU_FALSE; goto rtn;
	}
	else if (irc != 0) {
			  irtn=UU_FALSE; goto rtn;
	}
	else {
			 udi_dgresult (dsda);
			 irtn=UU_TRUE; goto rtn;
			}
rtn: uu_dprint(UU_DTRC,(us,"%d=ud_dasin. UDI_der1=%d, irc=%d",
	irtn,UDI_der1,irc)); 
	uu_dexit;
	return(irtn);
}		/* ud_dasin */





/*********************************************************************
**    I_FUNCTION     :  int *udi_dlxconread ( dready,fnamevar )
**       read the lexicon file name and the lexicon file
**    PARAMETERS   
**       INPUT  : 	char *fnamevar; -- shell var containing .grm filename.
**       OUTPUT :  	int *dready; -- true or false.
**    RETURNS      : pointer to array holding the lexicon
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int *udi_dlxconread ( dready,fnamevar)
int  *dready;
char *fnamevar;		/* name of shell variable containing the .grm filename*/


{
	int *lexic;			/* pointer to lexicon array */
	char c[40];
	static int one=1;		/* static ok since never change value */
	int fname[UX_MAX_PATH_LEN];			/* lexicon file name */
	int irc;					/* return code from ut_lxread */
	int len;					/* length of fname */

	uu_denter(UU_DTRC,(us,"udi_dlxconread(%s)",fnamevar));
/*
 	 printf("enter lexicon file name: ");      
	 uti_tread(fname,one,&len);     	   
*/

	ut_dfnmread(fname,one,&len,fnamevar); 
												/* read filename into fname */
	lexic=ut_lxread(fname,len,&irc);	/* read the lexicon file */
	if (irc!=1) {
		ud_printmsg("error reading lexicon file ");
		uti_twrite(fname,&one,&len);
		exit(1);
	}
	else
		*dready = UU_TRUE;

/*
	printf(" tracing enabled(y/n)? ");
	gets(c);
	if (c[0]=='y') trace=1; else trace=0;

	printf(" ddebug enabled(y/n)? ");
	gets(c);
	if (c[0]=='y') udi_dsetdebug(1); else udi_dsetdebug(0);

*/
	udi_dsetdebug(0);
	trace = 0;

	uu_dexit;
	return(lexic);
}		/* udi_dlxconread */



