/*********************************************************************
**    NAME         :  xuf1logf.c 
**       CONTAINS:
**
**			uxu_set_logfile()
**			uxu_reset_logfile()
**			uxu_log_time()
**			uxu_log_pid_size()	 not done 
**			uxu_kill_log_pid()
**			ux_write_logfile(label, message)
**			ux_get_log_pid(pidptr)
**
**			A collection of routines that can be called by debug menu 
**			selections and from MPE routines that will produce and 
**			write info to a log file intended for automatic testing.
**
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       xuf1logf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:33
*********************************************************************/
/*
** The log file name is kept in the Unicad symbol table, initialized by
**	the "init" file, as the symbol UU_LOGFILE. The uxu_set_logfile() call
**	may replace this symbol value with a new filename, or, if the name 
** is UU_NULL, there will be no output. There is only one logfile at a 
** time. If a file of this name exists, it is used, so remove old files
** before running!
*/
#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"			/* used by the debug system */
#include "derror.h"			/* needed for error system resolution */
#include "dasnog.h"
#include "dinput.h"
#include "uhep.h"				/* used by the debug system */
#include "xenv1.h"			/* include file for level 1 env system */
#include "xfsys0.h"
#include "xfsys1.h"
#include "utime.h"
#define TRACE UU_TRUE

/*********************************************************************
**    E_FUNCTION :  uxu_set_logfile()
**		Routine that direct the log file output to a given filename, or
**		can be used to turn off the output if it is given UU_NULL instead
**		of a string value. To be called from a menu selection. It uses
**		ux_modenv to replace (update) the value of the symbol UU_LOGFILE
**		in the Unicad symbol table.
**    PARAMETERS   
**       INPUT  : 
**				input
**       OUTPUT :  
**				output
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/

int uxu_set_logfile()
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UX_pathname filename;
	int numint;
	char *prompt, *uu_uprompt0();

	uu_denter(UU_XTRC, (us, "uxu_set_logfile()"));
	status = UU_SUCCESS; /* assume success */

	strcpy(filename,"");

	/* ask for the new log file name */
	prompt = uu_uprompt0(UX_UDOS, 5);
	ud_ddas(UD_DASSTRING,prompt,filename,UX_MAX_PATH_LEN,&numint,UD_DEFAULT);

/*	-- set DAS in proper state -- */

	if(filename == NULL)
		UD_autotest = UU_FALSE;
	else
		UD_autotest = UU_TRUE;

	/* set the symbol to be this current log file name */
	if (ux_modenv("replace","UU_LOGFILE",filename,UX_PRTERRS) != UU_SUCCESS)
		goto failed;

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexitstatus("uxu_set_logfile",status);
	return(status);
}	
/*********************************************************************
**    E_FUNCTION :  uxu_reset_logfile()
**		Routine that turns off the log file output to a given filename.
**		To be called from a menu selection. It uses
**		ux_modenv to replace (update) the value of the symbol UU_LOGFILE
**		in the Unicad symbol table.
**    PARAMETERS   
**       INPUT  : 
**				input
**       OUTPUT :  
**				output
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/

int uxu_reset_logfile()
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UX_pathname filename;

	uu_denter(UU_XTRC, (us, "uxu_reset_logfile()"));
	status = UU_SUCCESS; /* assume success */

	strcpy(filename,"");
	UD_autotest = UU_FALSE;

	/* set the symbol to be this current log file name */

	if (ux_modenv("replace","UU_LOGFILE",filename,UX_PRTERRS) != UU_SUCCESS)
		goto failed;

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexitstatus("uxu_reset_logfile",status);
	return(status);
}	
/*********************************************************************
**    E_FUNCTION :  uxu_log_time()
**		Routine that will stamp out the time to the log file, called
**		from a menu selection.
**			
**    PARAMETERS   
**       INPUT  : 
**				input
**       OUTPUT :  
**				output
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/

int uxu_log_time()
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int timval;
	/* struct tm *time_st; used to format the time, not being used now */
	char timebuff[UX_MAX_PATH_LEN];
	uu_denter(UU_XTRC, (us, "uxu_log_time()"));
	status = UU_SUCCESS; /* assume success */
	
	if(UD_autotest==UU_TRUE)
	{
		time(&timval);
		/* time_st = localtime(&timval); UX_SPRINTF0((timebuff, 
			"%2d:%2d\n", time_st->tm_hour, time_st->tm_min)); */
		UX_SPRINTF0((timebuff,"%d\n",timval));
		ux_write_logfile("TIME",timebuff);
	}
	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexitstatus("uxu_log_file",status);
	return(status);
}	
/*********************************************************************
**    E_FUNCTION :  uxu_log_pid_size()
**		Routine that will stamp out process information to the log file
**		called from a menu selection.
**			
**    PARAMETERS   
**       INPUT  : 
**				input
**       OUTPUT :  
**				output
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/

int uxu_log_pid_size()
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "uxu_log_pid_size()"));
	status = UU_SUCCESS; /* assume success */

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexitstatus("uxu_log_pid_size",status);
	return(status);
}	
/*********************************************************************
**    E_FUNCTION :  uxu_kill_log_pid()
**		Routine to kill the process i.d. that is found in the log file
**		header, the process now using the log file.
**			
**    PARAMETERS   
**       INPUT  : 
**				input
**       OUTPUT :  
**				output
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
#if UU_COMP!=UU_WIN2K
int uxu_kill_log_pid()
{
	long pid;				/* should this be a long or int ? */
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "uxu_kill_log_pid()"));
	status = UU_SUCCESS; /* assume success */

	if (ux_get_log_pid(&pid) != UU_SUCCESS)
		goto failed;

	uu_dprint(UU_XTRC,(us,"about to kill off process %d", pid));

	kill(pid,9); 

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexitstatus("uxu_kill_log_pid",status);
	return(status);
}	
#endif
/*********************************************************************
**    E_FUNCTION :  ux_write_logfile(label, message)
** Get the UU_LOGFILE filename, and expand the name, see if one exists.
** If so open it, or else create the new file, giving it a Unicad header
** including process i.d. number.
**			
**    PARAMETERS   
**       INPUT  : 
**				input
**       OUTPUT :  
**				output
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/

int ux_write_logfile(label, message)
char *label;
char *message;
{
	char *tempfile;
	UX_pathname fullname;
	UX_pathname databuf;
	char LocalBuffer[UX_MAX_PATH_LEN];
	char *cp;
	char *ux_getenv();
	long getpid();
	long pid;
	int fmode, istat;
	int logfile, nout;
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_write_logfile()"));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (label!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"label:%s", label));
		if (message!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"message:%s", message));
	}
#endif
	status = UU_SUCCESS; /* assume success */

/*	-- write out to autotest file if autotesting -- */

	if(UD_autotest==UU_TRUE)
	{

		/* compose the complete file name and create or open file */

		/* if there is a good value for the UU_LOGFILE symbol in the table,
		use it, else do nothing until the UU_LOGFILE gets a value from the
		user (calls ux_modenv and puts the filename into the table */

		tempfile = ux_getenv("UU_LOGFILE",UX_PRTERRS|UX_NQUOTES);
		if (tempfile != UU_NULL)
		{
			fmode = UX_EXISTS; 
			if (ux_mk_chk_syspath(UU_NULL, UU_NULL, "^UU_LOGFILE", UU_NULL, UU_NULL,
				&fmode,fullname,UX_PRTERRS) != UU_SUCCESS) goto failed;
			if (fmode == (fmode|UX_NEXISTS) )
			{
				pid = getpid();
				UX_SPRINTF0((databuf, "%ld\n", pid));

				/* the new file will get a UNICAD header and the process i.d. # */
				istat = ux_create_file(fullname,0644,UU_NULL,"STREAM","ASCII",
					databuf, &logfile, UX_PRTERRS);
				if (istat != UU_SUCCESS) 
					goto failed;
			}
			else
			{
				istat = ux_open(fullname,"a","STREAM","ASCII",&logfile,UX_PRTERRS);
				if (istat != UU_SUCCESS) 
					goto failed;
			}
		}
		else
			goto failed;
		
		/* compose the message if either string isn't empty */
			if (label != UU_NULL)
			{
				strcpy(LocalBuffer, label);
				/* remove trailing newline if present */
				cp = LocalBuffer + strlen(LocalBuffer) - 1;
				if (*cp == '\n')
					{*cp = '\0';}
				if (message != UU_NULL)
				{
					strcat(LocalBuffer," ");
					strcat(LocalBuffer, message);
					/* remove trailing newline if present */
					cp = LocalBuffer + strlen(LocalBuffer) - 1;
					if (*cp == '\n')
						{*cp = '\0';}
				}
			}
			else
			{
				if (message != UU_NULL)
				{
					strcpy(LocalBuffer, message);
					/* remove trailing newline if present */
					cp = LocalBuffer + strlen(LocalBuffer) - 1;
					if (*cp == '\n') 
						{*cp = '\0';}
				}
			}

		/* write to the log file */
		UX_FPRINTF0((otable[logfile].fileptr, "%s\n", LocalBuffer ),nout);

		/* flushes and closes the logfile up */
		ux_close(logfile,UX_PRTERRS);

		goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
		UX_IF_FAILURE_PRINT_IT
#endif
	}
done:;
	uu_dexitstatus("ux_write_logfile",status);
	return(status);
}	
/*********************************************************************
**    E_FUNCTION :  ux_get_log_pid(pidptr)
**		Routine to read the logfile header and return the process i.d.
**			
**    PARAMETERS   
**       INPUT  : 
**				input
**       OUTPUT :  
**				output
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/

int ux_get_log_pid(pidptr)
long *pidptr;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int logfile;
	UX_fheader hdr;
	int hstat;
	int fmode;
	int nread;
	char *cp;
	char *tempfile;
	char *ux_getenv();
	UX_pathname fullname;
	UX_pathname tempbuf;
	int istat;

	uu_denter(UU_XTRC, (us, "ux_get_log_pid(?)"));
	status = UU_SUCCESS; /* assume success */

	tempfile = ux_getenv("UU_LOGFILE",UX_PRTERRS|UX_NQUOTES);
	if (tempfile != UU_NULL)
	{
		fmode = UX_EXISTS; 
		if (ux_mk_chk_syspath(UU_NULL, UU_NULL, "^UU_LOGFILE", UU_NULL, UU_NULL,
			&fmode,fullname,UX_PRTERRS) != UU_SUCCESS) goto failed;
		if (fmode == (fmode|UX_NEXISTS) )
		{
			/* no log file, error */
			goto failed;
		}
		else
			istat = ux_open(fullname,"r+","STREAM","ASCII",&logfile,UX_PRTERRS);
		if (istat != UU_SUCCESS) 
			goto failed;
	}
	else
		{
			/* no log file, error */
			goto failed;
		}
	
	/* read the file header and return special information from it */
	if (ux_get_file_hdr(logfile, &hdr, &hstat, UX_PRTERRS) != UU_SUCCESS)
		goto failed;

	if (ux_close(logfile,UX_PRTERRS) == UX_FAILURE)
			goto failed;

	/* the pid is in field hdr->extra */
	uu_dprint(UU_XTRC,(us,"hdr.extra is %s", hdr.extra));
	uu_dprint(UU_XTRC,(us,"hdr.extra is %d", hdr.extra));
	strcpy(tempbuf,(hdr.extra));
	cp = (tempbuf) + strlen(tempbuf) - 1;
	if (*cp == '\n') 
		{*cp = '\0';}		/* remove the \n */
	UX_SSCANF0((tempbuf,"%d", pidptr), nread);
	uu_dprint(UU_XTRC,(us,"Log file pid is: %d", *pidptr));

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexitstatus("ux_get_log_pid",status);
	return(status);
}	

