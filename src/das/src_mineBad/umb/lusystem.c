/*********************************************************************
**	FILENAME: lsystem.c
**	CONTAINS:		ul_set_symbol
**				ul_system
**				ul_spawn
**				ul_spawn_unix
**				ul_spawn_wnt
**				ul_run_process
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lusystem.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:21
*********************************************************************/

#include "usysdef.h"
#include "dmark.h"
#include "driver.h"
#include "dtypes.h"
#include "gobndl.h"
#include "lumb.h"
#include "lcom.h"
#include "mfort.h"
#include "udebug.h"
#include "uhep.h"
#include "ws410x.h"
extern uw_41xxdat uw_41xx;

#include "xenv1.h"
#include "nclfc.h"

static void S_break_comlin();

/*********************************************************************
**	 E_FUNCTION : ul_set_symbol(sym,com)
**			This function defines an operating system symbol.
**	 PARAMETERS	
**		 INPUT  :  sym = symbol to define.
**			   com = definition of symbol.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/

void ul_set_symbol(sym,com)
	char *sym,*com;
{
#if UU_COMP == UU_VAXVMS
	int statvax;
	char buf[81],buf1[81];
	$DESCRIPTOR (bufd,buf);
	$DESCRIPTOR (buf1d,buf1);
#endif
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_set_symbol(sym,com)"));
/*
.....Define symbol
*/
#if UU_COMP == UU_VAXVMS
	strcpy (buf,sym);
	bufd.dsc$w_length = strlen(buf);
	strcpy (buf1,com);
	buf1d.dsc$w_length = strlen(buf1);
	statvax = lib$set_symbol (&bufd,&buf1d);
done:;
#endif
	uu_dexit;
	return;
}	

/*********************************************************************
**	 E_FUNCTION : ul_system(flag, parms)
**			This function allows the user to issue system
**			level command lines without leaving the program.
**	 PARAMETERS	
**		 INPUT  :  flag = specifies which type of system call to
**				make and can be one of the following values.
**				0 = Enter system & let the user make as
**					many calls as they want.
**				1 = Single system command.
**				2 = User interface menu (command file).
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Removes all display from terminal (flag=0,2) or opens a
**			a scrolling window and writes to it (flag=1).  Also
**			passes control to the operating system.
**	 WARNINGS: none.
*********************************************************************/
/*
.....changed for adding command parameters
......Yurong 12/1/00
*/
void ul_system(flag, parms)
int flag;
char *parms;
{
	char *menu,buf[UX_MAX_PATH_LEN+40],buf1[UX_MAX_PATH_LEN+40];
	char *ux_getenv();
	char *lpr1="Enter system command";
	int numint,markval;
	char *p, *index();
	int oflg;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_system(flag)"));
/*
.....Command Reject
*/
	UD_MARK (markval,UU_FALSE);
	if (markval != 0)
	{
		ul_close_window();
		UD_UNMARK (markval);
		return;
	}
/*
.....Multiple System Commands entry
*/
	switch (flag)
	{
	case 0:
/*		ud_wrerr ("Enter LOG to return to NCL");*/
		ul_spawn("",2);
/*		ud_wrerr (" ");*/
		break;
/*
.....Single Command entry
*/
	case 1:
/*
......if paramter  pass in is not NULL, use it
*/
		if ((parms==UU_NULL)||(parms[0]=='\0'))
		{
			ud_das (UD_DASSTRING,lpr1,buf,sizeof(buf),&numint);
			if (numint == 0) goto done;
			ul_spawn(buf,1);
		}
		else
		{
			numint = strlen(parms);
			strcpy(buf, parms);
			buf1[0] = '\0';
			p = (char *)index(buf,',');
			if (p!=UU_NULL)
			{
				*p = '\0';
				++p;
				while ((*p==' ') || (*p=='\t')) p++;
				strcpy(buf1, p);
			}		
			ul_to_upper(buf1);
			if (strcmp(buf1, "YES")==0)
				oflg = 2;
			else if (strcmp(buf1, "NO")==0)
				oflg = 0;
			else
				oflg = 2;
			ul_spawn(buf,oflg);
		}
		break;
/*
.....User Interface Menu
*/
	case 2:
		menu = ux_getenv ("UL_INTERFACE_MENU",UX_NPRTERRS);
		if (menu == 0)
		{
			ud_wrerr ("A user interface menu has not been defined.");;
			goto failed;
		}
		ul_spawn(menu,2);
		break;
	}
failed:;
done:;
	UD_UNMARK (markval);
	uu_dexit;
	return;
}

/*********************************************************************
**	 E_FUNCTION : ul_spawn(com,flag)
**			This function opens either a scrolling window
**			or opens the entire terminal and spawns a sub-
**			process.
**	 PARAMETERS	
**		 INPUT  :  com = CLI command line to spawn.
**			   flag = specifies what type of window to open.
**				0 = don't open a window.
**				1 = scrolling window UL_winrow lines X 80 cols
**				2 = entire terminal screen
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Removes all display from terminal (flag=1) or opens a
**			a scrolling window and writes to it (flag=0).  Also
**			passes control to the operating system.
**	 WARNINGS: none.
*********************************************************************/

void spwnit()
{
	ul_spawn ("",2);
	return;
}

int ulf_spawn(com,flag)
char *com;
int *flag;
{
	return ul_spawn(com,*flag);
}

int ul_spawn(com,flag)
char *com;
int flag;
{
	int statvax;
	int markval;
	UM_int2 ifl,batch;
	char msg[UX_MAX_PATH_LEN+40];
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_spawn(com,flag)"));
/*
.....Command Reject
*/
	ifl = 35;
	getifl (&ifl,&batch);
	UD_MARK (markval,UU_FALSE);
	if (markval != 0)
	{
		ul_close_window();
		UD_UNMARK (markval);
		return -1;
	}
/*
.....Spawn command
*/
#if UU_COMP == UU_WIN2K
		statvax = ul_spawn_wnt(com,flag,batch);
#else 
		statvax = ul_spawn_unix(com,flag,batch);
#endif
/*
.....Check for error from spawn
*/
	if (batch == 0) ud_prmerr ("");
	if (statvax != 1)
	{
		if (batch == 0)
			ud_wrerr ("Error trying to spawn subprocess.");
		else
		{
			sprintf (msg, "Error trying to spawn subprocess: %s.\n",com);
			ud_printmsg(msg);
		}
		goto failed;
	}
failed:;
	UD_UNMARK (markval);
	uu_dexit;
	return statvax;
}

/*********************************************************************
**	 E_FUNCTION : ul_spawn_unix(com,flag,batch)
**			Controls the spawning process on a Unix machine.
**	 PARAMETERS	
**		 INPUT  :  com = CLI command line to spawn.
**			   flag = specifies what type of window to open.
**				0 = don't open a window.
**				1 = scrolling window UL_winrow lines X 80 cols
**				2 = entire terminal screen
**			   batch = 1 = NCL is in batch mode.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Removes all display from terminal (flag=1) or opens a
**			a scrolling window and writes to it (flag=0).  Also
**			passes control to the operating system.
**	 WARNINGS: none.
*********************************************************************/

#if UU_COMP != UU_VAXVMS
ul_spawn_unix(com,flag,batch)
char *com;
int flag;
UM_int2 batch;
{
	char *ptr,*ux_getenv(),*index();
	int ied;
	int statvax,args[3],stat;
	char buf[UX_MAX_PATH_LEN+40],pbuf[UX_MAX_PATH_LEN+40];
	char cbuf[UX_MAX_PATH_LEN+40];
/*
.....Determine type of window to open
*/
	switch (flag)
	{
/*
........Do not need to open a window
*/
	case 0:
		break;
/*
........Open partial screen window
*/
	case 1:
		stat = UU_SUCCESS;
		args[1] = 0;
/*
...........Failed to open system window
*/
		if (stat != UU_SUCCESS)
		{
			ud_wrerr ("Error trying to open window");
			goto failed;
		}
		break;
/*
........Open full terminal window
*/
	case 2:
		stat = UU_SUCCESS;
/*
...........Failed to open system window
*/
		if (stat != UU_SUCCESS)
		{
			ud_wrerr ("Error trying to open window");
			goto failed;
		}
		break;
	}
/*
.....Don't use 'nclsystem' if user
.....is spawning the editor
.....Bobby  -  5/13/98
*/
	ied = 0;
	ptr = ux_getenv("UL_NCL_EDIT",UX_NPRTERRS);
	strcpy(pbuf,ptr);
	ptr = index(pbuf,' ');
	if (ptr != UU_NULL) *ptr = '\0';
	strcpy(cbuf,com);
	ptr = index(cbuf,' ');
	if (ptr != UU_NULL) *ptr = '\0';
	if (strcmp(pbuf,cbuf) == 0) ied = 1;
/*
.....Spawn subprocess
*/
	if (flag == 0)
	{
		statvax = system(com);
	}
	else
	{
		if (strlen(com) == 0)
		{
#ifdef UU_RS6000
			statvax = system("aixterm -W -T NCL_System");
#endif
#if UU_COMP == UU_SUN
			statvax = system("cmdtool");
#endif
#if UU_COMP == UU_IRIS4D
#ifndef UU_RS6000
			statvax = system("xwsh -title NCL_SYSTEM");
#endif
#endif
#if UU_COMP == UU_HPUX
			statvax = system("dtterm -title NCL_System");
#endif
#if UU_COMP == UU_WINNT
			statvax = system("wstart -w -t NCL_System 'ncenv.cmd && sh.exe'");
#endif
		}
		else
		{
#ifdef UU_RS6000
			if (ied == 1)
				sprintf(buf,"aixterm -W -T NCL_System -e %s",com);
			else
				sprintf(buf,"aixterm -W -T NCL_System -e nclsystem %s",com);
			statvax = system(buf);
#endif
#if UU_COMP == UU_SUN
			if (ied == 1)
				sprintf(buf,"cmdtool %s",com);
			else
				sprintf(buf,"cmdtool nclsystem %s",com);
			statvax = system(buf);
#endif
#if UU_COMP == UU_IRIS4D
#ifndef UU_RS6000
/*
.....'vi' through 'nclsystem' on SGI will dump
.....if the 'delete' (intr) key is hit during the session
.....Bobby  -  1/8/99
*/
			if (ied == 1)
				sprintf(buf,"xwsh -title NCL_System -e %s",com);
			else
				sprintf(buf,"xwsh -title NCL_System -e nclsystem %s",com);
			statvax = system(buf);
#endif
#endif
#if UU_COMP == UU_HPUX
			if (ied == 1)
				sprintf(buf,"dtterm -title NCL_System -e %s",com);
			else
				sprintf(buf,"dtterm -title NCL_System -e nclsystem %s",com);
			statvax = system(buf);
#endif
#if UU_COMP == UU_WINNT
			sprintf(buf,"wstart -w -t NCL_System 'ncenv.cmd && sh.exe -c \"%s\"'",com);
			statvax = system(buf);
#endif
		}
	}
#if UU_COMP == UU_SUN
	if (statvax != 127) statvax = 1;
#else
	if (statvax >= 0) statvax = 1;
#endif
failed:;
	return(statvax);
}
#endif
/*********************************************************************
**	 E_FUNCTION : ul_spawn_wnt(com,flag,batch)
**			Controls the spawning process on a WinNT machine.
**	 PARAMETERS	
**		 INPUT  :  com = CLI command line to spawn.
**			   flag = specifies what type of window to open.
**				0 = don't open a window.
**				1 = scrolling window UL_winrow lines X 80 cols
**				2 = entire terminal screen
**			   batch = 1 = NCL is in batch mode.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Removes all display from terminal (flag=1) or opens a
**			a scrolling window and writes to it (flag=0).  Also
**			passes control to the operating system.
**	 WARNINGS: none.
*********************************************************************/

#if UU_COMP == UU_WIN2K
ul_spawn_wnt(com,flag,batch)
char *com;
int flag;
UM_int2 batch;
{
	char *ptr,*ux_getenv(),*index();
	int ied;
	int stat,len,backrun;
	char buf[UX_MAX_PATH_LEN+40],pbuf[UX_MAX_PATH_LEN+40];
	char cbuf[UX_MAX_PATH_LEN+40],ldir[UX_MAX_PATH_LEN];
	char pcom[UX_MAX_PATH_LEN],ccom[UX_MAX_PATH_LEN];

	len = strlen(com);
	if (len==0)
	{
/*		sprintf(buf,"cmd.exe /K %s", com); 
		stat = system(buf);*/
		stat = system("start \"NCL_System\" cmd.exe");
		if (stat <= 0) stat = 1;
		return(stat);
	}
	if (com[len-1]=='&')
	{
		backrun = 1;
		com[len-1] = '\0';
/*
.....Must close the temporary work file here
.....otherwise if the spawned program is left open
.....when NCL exits, the file cannot be deleted
.....Bobby  -  07/7/11
*/
		ncl_close_tmpsrc();
	}
	else
		backrun = 0;
/*
.....Determine if using editor
*/
	ied = 0;
	ptr = ux_getenv("UL_NCL_EDIT",UX_NPRTERRS);
	if (ptr!=NULL)
	{
		S_break_comlin(ptr,pbuf,pcom);
		S_break_comlin(com,cbuf,ccom);
		if (strcmp(pbuf,cbuf) == 0) ied = 1;
	}
/*
.....Spawn editor
*/
	if (ied == 1)
	{
		ul_break_fname(cbuf,ldir,buf);
		stat = ul_run_process(ldir,buf,ccom);
	}
/*
.....Spawn subprocess
*/
	else if (flag == 0)
	{
/*
.....if sttaement like this, it will display command window
		stat = system(com);
*/
		if (backrun==1)
		{
			sprintf(buf,"start \"NCL_System\" /B %s", com);
			stat = system(buf);
		}
		else
			stat = system(com);
	}
	else
	{
		if (backrun==1)
		{
			if (strlen(com) == 0)
			{
				stat = system("start \"NCL_System\" cmd.exe");
			}
			else
			{
				sprintf(buf,"start \"NCL_System\" cmd.exe /K %s", com); 
				stat = system(buf);
			}
		}
		else
		{
			sprintf(buf,"cmd.exe /K %s", com); 
			stat = system(buf);
		}
	}
	if (backrun == 1) ncl_open_tmpsrc();
	if (stat <= 0) stat = 1;
	return(stat);
}

/*********************************************************************
**	 I_FUNCTION : S_break_comlin(inbuf,ebuf,cbuf)
**			Breaks a system command into the executable and input line,
**       allowing for quotes on executable path.
**	 PARAMETERS	
**		 INPUT  :
**			   inbuf = System command.
**		 OUTPUT :
**			   ebuf  = Command to be run.
**			   cbuf  = Input line to send to 'command'.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none.
*********************************************************************/
static void S_break_comlin(inbuf,ebuf,cbuf)
char *inbuf,*ebuf,*cbuf;
{
	char *ptr;
/*
.....Break out program name
*/
	if (inbuf[0] == '\"')
	{
		strcpy(ebuf,&inbuf[1]);
		ptr = index(ebuf,'\"');
	}
	else
	{
		strcpy(ebuf,inbuf);
		ptr = index(ebuf,' ');
	}
/*
.....Break out command
*/
	if (ptr != UU_NULL)
	{
		*ptr = '\0';
		ptr++;
		if (*ptr == '\0' || (*ptr+1) == '\0')
			cbuf[0] = '\0';
		else
		{
			if (*ptr == ' ') ptr++;
			strcpy(cbuf,ptr);
		}
	}
	else
		cbuf[0] = '\0';
}
#endif
		
/*********************************************************************
**	 E_FUNCTION : ul_run_process (dir, exe, cmd)
**			This function spawns a sub-process without open a window
**
**	 PARAMETERS	
**		 INPUT  :  dir = Full directory path which 'exe' resides in.
**					exe = Name of utility program to run. (have to be a executable, not batch file)
**					cmd = Command line to pass to the process.
**
**		 OUTPUT :  none.
**	 RETURNS: none
**	 WARNINGS: none.
*********************************************************************/
ul_run_process (dir, exe, cmd)
char *dir, *exe, *cmd;
{
#if UU_COMP == UU_WIN2K
	return(uw_run_process (dir, exe, cmd));
#else
	char com[UX_MAX_PATH_LEN+40];
	sprintf (com,"\"%s/%s\" %s",dir,exe,cmd);
	return(ul_spawn (com, 0));
#endif
}

void postmsg(msg,nc)
char *msg;
int *nc;
{
	msg[*nc] = '\0';
	ud_printmsg(msg);
}
