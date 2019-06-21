/*********************************************************************
**	FILENAME: lbatch.c
**	CONTAINS:		ul_display_batch
**				ul_que_ncl
**				ul_que_util
**				ul_set_ncl_mod
**				ul_status_batch
**				ul_submit_batch
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lubatch.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:21
*********************************************************************/

#include "usysdef.h"
#include "dtypes.h"
#include "lcom.h"
#include "udebug.h"
#include "udfdata.h"
#include "udforms.h"
#include "xenv1.h"
#include "xfsys1.h"

#define NODEFAULT 0
#define DEFAULT 1
#if UU_COMP == UU_VAXVMS
#define NCL_QUE "NC15:NCL.QUE"
#define UTIL_QUE "UTIL.QUE"
#define LOG_FILE "BAT.LOG"
#else
#define NCL_QUE "ncl.que"
#define UTIL_QUE "util.que"
#define LOG_FILE "bat.log"
#endif

/*********************************************************************
**	 E_FUNCTION : ul_display_batch(iarg)
**			This function opens a window and displays files
**			associated to batch runs.
**	 PARAMETERS	
**		 INPUT  :  iarg	= which file to display.
**				1 = NCL que
**				2 = Utility que
**				3 = Batch log file
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Opens a scrolling window and writes to it.
**	 WARNINGS: none.
*********************************************************************/

ul_display_batch(iarg)
	int iarg;
{
	UX_pathname que;
	char *p,*ux_getenv();
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_display_batch()"));
	switch (iarg)
	{
	case 1:
#if UU_COMP == UU_VAXVMS
		strcpy (que,NCL_QUE);
#else
		p = ux_getenv("nc15","UX_NPRTERRS");
		if (p != 0) sprintf (que,"%s%s",p,NCL_QUE);
		else sprintf (que,"%s",NCL_QUE);
#endif
		ul_display_file (que);
		break;
	case 2:
		ul_display_file (UTIL_QUE);
		break;
	case 3:
		ul_display_file (LOG_FILE);
		break;
	}
	uu_dexit;
	return;
}

/*********************************************************************
**	 E_FUNCTION : ul_que_ncl()
**			This function sets up a command line and spawns
**			NCQ502 to schedule batch runs.
**	 PARAMETERS	
**		 INPUT  :  none
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Spawns a sub-process.
**	 WARNINGS: none.
*********************************************************************/

ul_que_ncl()
{
	char *position,*rindex();
	UX_pathname com,buf,buf1;
	int numint,mode,file_status,stat;
	char *pointer;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_que_ncl()"));
/*
.....Build command line
.....First store current program name &
.....add file type
*/
	strcpy (com,UL_program);
	if (strlen(com) != 0)
	{
		strcat (com,".");
		strcat (com,UL_program_suffix);
	}
/*
.....Let the user change the program name
.....if they want to
*/
	for (;;)
	{
		ul_string_def ("Enter command line",com,sizeof(com),&numint,
				&file_status);
		ul_strip_blanks (com,&numint);
		if (numint == 0) goto done;
/*
.....Make sure part program file exists
*/
		strcpy (buf,com);
		position = rindex(buf,'-');
		if (position != 0) *position = '\0';
		mode = UX_EXISTS|UX_READ;
		stat = ux_file_inquire(UU_NULL,UU_NULL,buf,UU_NULL,UU_NULL,&mode,
		&file_status,buf1,UX_NPRTERRS);
		if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
		{
			sprintf (buf1,"%s does not exist.",buf);
			ud_wrerr (buf1);
			goto end_loop;
		}
/*
.....Check to see if switches
.....were added to the command line
.....If so, then do not add default switches
*/
		position = rindex(com,'-');
		if (position == 0)
		{
/*
.....Append CL switch
*/
			if (UL_ncq_cl == 0) strcat (com," -NOCL");
			else strcat (com,"-CL");
/*
.....Append AS switch
*/
			if (UL_ncq_as == 0) strcat (com," -NOAS");
			else strcat (com,"-AS");
/*
.....Append OP or LI switch
*/
			switch (UL_ncq_pr)
			{
			case 0:
				strcat (com," -NOOP");
				strcat (com," -NOLI");
				break;
			case 1:
				strcat (com," -OP");
				break;
			case 2:
				strcat (com," -NOLI");
				break;
			}
/*
.....Append UP switch
*/
			if (UL_ncq_pp == 0) strcat (com," -NOUP");
			else strcat (com," -UP");
/*
.....Append NL switch
*/
			sprintf (buf," -NL:%d",UL_ncq_nl);
			strcat (com,buf);
/*
.....Append PR switch
*/
			strcpy (buf,"A");
			pointer = buf;
			*pointer = *pointer + UL_ncq_pri;
			strcat (com," -PR:");
			strcat (com,buf);
/*
.....Append PP switch
*/
			if (UL_ncq_post == 0) strcat (com," -NOPP");
			else strcat (com," -PP");
		}
/*
.....Spawn NCQ to schedule part program file
*/
		strcpy (buf,"ncq ");
		strcat (buf,com);
		ul_spawn (buf,0);
		strcpy (com,"");
end_loop:;
	}
/*
.....End of routine
*/
failed:;
done:;
	uu_dexit;
	return;
}

/*********************************************************************
**	 E_FUNCTION : ul_que_util(com,flag)
**			This function sets up a command line and spawns
**			UTQ to schedule batch runs.
**	 PARAMETERS	
**		 INPUT  :  command =	command line to que (if any).
**			   flag =	0 = prompt user for command.
**					1 = use "command" for command.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Spawns a sub-process.
**	 WARNINGS: none.
*********************************************************************/

ul_que_util(command,flag)
	char *command;
	int flag;
{
	char *position,*rindex();
	char com[200],buf[200];
	int numint,mode,file_status,stat;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_que_util()"));
/*
.....Get command line
*/
	for (;;)
	{
		if (flag == 0)
		{
			ud_das (UD_DASSTRING,"Enter command line",com,sizeof(com),
				&numint);
		}
		else
		{
			strcpy (com,command);
			numint = strlen (com);
		}
		if (numint == 0) goto done;
/*
.....Spawn UTQ to schedule part program file
*/
		strcpy (buf,"utq ");
		strcat (buf,com);
		ul_spawn (buf,0);
		if (flag != 0) goto done;
	}
/*
.....End of routine
*/
failed:;
done:;
	uu_dexit;
	return;
}

/*********************************************************************
**	 E_FUNCTION : ul_set_ncl_mod()
**			This function sets the default options to use
**			when scheduling an NCL que program.
**	 PARAMETERS	
**		 INPUT  :  none
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Defines NCL que modals.
**	 WARNINGS: none.
*********************************************************************/

ul_set_ncl_mod()
{
	int cl[4],as[4],pr[8],pp[4],nl[3],pri[2],post[4];
	int stat;
	int *ans[7];
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_ncl_set_mod()"));
/*
.....Set up the defaults
.....Load the input values into
.....local storage area
*/
	cl[0] = UL_ncq_cl;
	as[0] = UL_ncq_as;
	pr[0] = UL_ncq_pr;
	pp[0] = UL_ncq_pp;
	nl[0] = UL_ncq_nl;
	pri[0] = UL_ncq_pri;
        post[0] = UL_ncq_post;
/*
.....Field 0 is CL file
*/
	ans[0] = (int *)cl;
/*
.....Field 1 is APT source file
*/
	ans[1] = (int *)as;
/*
.....Field 2 is Print file
*/
	ans[2] = (int *)pr;
/*
.....Field 3 is Update PP file
*/
	ans[3] = (int *)pp;
/*
.....Field 4 is Number of lines per page
*/
	ans[4] = (int *)nl;
/*
.....Field 5 is Priority
*/
	ans[5] = (int *)pri;
/*
.....Field 6 is Post Process
*/
	ans[6] = (int *)post;
/*
.....Get the Form input
*/
	stat = ud_form("lnclque.frm",ans,ans);
	if (stat==-1)
	{
		return;
	}
/*
.....Store NCL Que Modals
*/
	UL_ncq_cl = cl[0];
	UL_ncq_as = as[0];
	UL_ncq_pr = pr[0];
	UL_ncq_pp = pp[0];
	UL_ncq_nl = nl[0];
	UL_ncq_pri = pri[0];
	UL_ncq_post = post[0];
failed:;
done:;
	uu_dexit;
	return;
}
 
/*********************************************************************
**	 E_FUNCTION : ul_status_batch(flag)
**			This function shows the status of the operating
**			system batch que.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Opens a window and spawns a sub-process.
**	 WARNINGS: none.
*********************************************************************/

ul_status_batch()
{
#if UU_COMP == UU_VAXVMS
	ul_spawn ("SHOW QUE/BATCH",1);
#else
	ul_spawn ("ps",1);
#endif
	return;
}

/*********************************************************************
**	 E_FUNCTION : ul_submit_batch(flag)
**			This function gets an operating system command
**			from the user and submits it for background (batch)
**			processing.
**	 PARAMETERS	
**		 INPUT  :  flag	= which command line to submit to batch.
**				1 = NCL
**				2 = UTB
**				3 = Get command line from user
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Spawns a sub-process.
**	 WARNINGS: none.
*********************************************************************/

ul_submit_batch(flag)
	int flag;
{
	char *position,*rindex();
	char com[80],buf[80],buf1[80];
	int numint,mode,file_status,stat;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_submit_batch(flag)"));
	switch (flag)
	{
	case 1:
#if UU_COMP == UU_VAXVMS
		strcpy (com,"ncl");
#else
		strcpy (com,"nclbat");
#endif
		break;
	case 2:
		strcpy (com,"utb");
		break;
	case 3:
/*
.....Get command line
*/
		do
		{
			ud_das (UD_DASSTRING,"Enter command line",com,sizeof(com),
				&numint);
			if (numint == 0)
				ud_wrerr ("You must enter a command line");
		}
		while (numint == 0);
	}
/*
.....Submit command to batch
*/
#if UU_COMP == UU_VAXVMS
	strcpy (buf,"SUBT ");
	strcat (buf,com);
#else
	strcpy (buf,com);
	strcat (buf," > bat.log &");
#endif
	ul_spawn (buf,0);
	uu_dexit;
	return;
}
