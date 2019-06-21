/***********************************************************************
**
**   FILE NAME: pwcfunc.cpp
**
**   CONTAINS:
**			Pw_dispmsg()
**			add1dispmsg()
**			Pw_getyesno()
**			getyesno()
**			pw_mk_pthdir()
**			pw_chdir()
**
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
**          All Rights Reserved
**     MODULE NAME AND RELEASE LEVEL
**			pwcfunc.cpp , 24.2
**    DATE AND TIME OF LAST  MODIFICATION
**			06/10/14 , 11:06:39
**
**********************************************************************/

#ifdef WNT
#include "pwstdafx.h"
#include <conio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "pwwindef.h"
#include "PWMessageBox.h"
#include "pwenv.h"

CWnd *Pw_maindlg = NULL;
int UU_BATCH = 0;
FILE *PW_logfile = NULL;
PWMessageBox PW_msgbox;
extern "C" int mkdir(const char *path, int mode);
extern "C" int chdir(const char *path);
extern "C" int access(const char *path, int mode);

/**********************************************************************
** Subroutine Pw_dispmsg(char *msgline, int flag)                    
**                                                                   
**	purpose: This routine normally called by C/C++ 
**			add 1 display message in message window
**			If there is no message window, display a message box      
**	Input: msgline: message to display
**			flag: 0: disoplay message box window with one message
**				1: add one line message into default Postworks 
**					message window. If default Postworks message
**					window is NULL, display a message box
**	Output: none
**	return: None
************************************************************************/
extern "C" void Pw_dispmsg(char *msgline, int flag)
{
	char *tempstr;
	int i,j,len;
	if (UU_BATCH==2)
	{
		if (PW_logfile==NULL)
			return;
/*
.....log file already opened as text format, so we don't need '\r'
.....remove it
*/
		len = strlen(msgline);
		tempstr = (char*)malloc((len+1)*sizeof(char));
		for (i=0,j=0; i<len; i++)
		{
			if (msgline[i]!='\r')
				tempstr[j++] = msgline[i];
		}
		tempstr[j] = '\0';
		fprintf(PW_logfile, "%s\n", tempstr);
		free(tempstr);
		return;
	}
/*
.....if UU_BATCH ==3, then the error message may not open yet, check it
.....and open it
*/
	if (UU_BATCH==3)
	{
		if (Pw_maindlg==NULL)
		{
			PW_msgbox.Create(IDD_BATCH_MESSAGE);
			PW_msgbox.ShowWindow(SW_SHOW);
			Pw_maindlg = &PW_msgbox;
		}
	}

	if (((flag==0) || (Pw_maindlg==NULL))&&(UU_BATCH==0))
	{
		MessageBox(NULL, msgline, "Postworks Message", MB_OK);
		return;
	}

	CEdit *edt;
	if (UU_BATCH==0)
		edt = (CEdit *) (Pw_maindlg->GetDlgItem(IDC_POSTWORKS_MESSAGE));
	else
		edt = (CEdit *) (Pw_maindlg->GetDlgItem(IDC_BATCH_MESSAGE));

	char *buf;
	CString wintxt;
	len = strlen(msgline);
	edt->GetWindowText(wintxt); 
	int pos = wintxt.GetLength();
	edt->SetSel(pos, pos);
	if (pos!=0)
	{
		edt->ReplaceSel("\r\n");
		edt->SetSel(pos+2, pos+2);
	}
	edt->ReplaceSel(msgline);	

	edt->UpdateWindow();
}
/**********************************************************************
** Subroutine add1dispmsg(msgline,nc,window)                            
**                                                                   
**	purpose: This routine normally called by Fortran 
**			add 1 display message in message window
**			If there is no message window, display a message box      
**	Input: msgline: message to display
**			nc: message length
**			flag: 0: disoplay message box window with one message
**				1: add one line message into default Postworks 
**					message window. If default Postworks message
**					window is NULL, display a message box
**	Output: none
**	return: None
************************************************************************/
extern "C" void add1dispmsg(char *msgline, int *nc, int *flag)  
{
	char cmsg[1000];
	if (*nc == 0) return;
/*
.....added cmsg string to avoid assignment to a constant string
.....msgline could be a constant string
*/
	strncpy(cmsg, msgline, *nc);
	cmsg[*nc] = '\0';
	Pw_dispmsg(cmsg, *flag);
}

/**********************************************************************
** Subroutine Pw_getyesno(msg, title, *ans);           
**                                                                   
**	purpose: This routine display a prompt yes/no box and get the answer     
**	Input: msg: prompt message
**			title: box title
**	Output: ans: 1 if YES
**					0: NO
**	return: None
************************************************************************/
extern "C" void Pw_getyesno(char *msg, char *title, int *ans)  
{
	if (MessageBox(NULL, msg, title, MB_ICONQUESTION | MB_YESNO)==IDYES)
		*ans = 1;
	else
		*ans = 0;
}

/**********************************************************************
** Subroutine getyesno(msg, nc, title, tnc, ans)                     
**                                                                   
**	purpose: This routine normally called by Fortran 
**			This routine display a prompt yes/no box and get the answer     
**	Input: msg: prompt message
**			nc: msg length
**			title: box title
**			tnc, title length
**	Output: ans: 1 if YES
**					0: NO
**	return: None
************************************************************************/
extern "C" void getyesno(char *msg, int *nc, char *title, int *tnc,  int *ans)  
{
	msg[*nc] = '\0';
	title[*tnc] = '\0';
	Pw_getyesno(msg, title, ans);
}

extern "C" void ifrunbatch(int *batch)
{
	*batch = UU_BATCH;
}

/*********************************************************************
**    E_FUNCTION :  pw_mk_pthdir(pathname)
**			Attempts to create a file area at the location
**			designated by "pathname". It wll create the whole path, but not
**			the top directory
**			pathname can't be envioment value
**		PARAMTERS:
**			pathname: full path name to be created.
**		RETURNS:
**			0 is returned if the path is created;
**			otherwise 
**				-1: something went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
extern "C" int pw_mk_pthdir(char *pathname)
{
	int status,len;
	char *index();
	char *rindex();
	char *str;
	FILE *fd;
	char top_dir[256], tempstr[256], orig_dir[256];

	status = 0;
/*
.....save current directory
*/
	GetCurrentDirectory(256, orig_dir);

	strcpy (tempstr, pathname);
/*
.....first switch to top-directory of the path
*/
	str = strchr (tempstr, '\\');
	if (str==NULL)
	{
/*
......NO PATH, JUST	make a directory
*/
		if ( access(pathname, 0) != -1 )
		{
			goto done;
		}
		if (mkdir(pathname, 755) != 0)
			goto failed;
		goto done;
	}
/*
.....we need keep the last PATH_SEP for the top directory
.....for example, "cd c:\" will change to "c:" but "cd c:" will 
.....doing nothing if we already in c: directory, also if we need
.....change to "/" will need keep UX_PATH_SEP too
*/ 
	len = str - tempstr + 1;
	strncpy(top_dir, tempstr, len);
	top_dir[len] = '\0';
	if (chdir (top_dir)!= 0)
		goto failed;
/*
.....then mkdir the following directory
*/
make_dir:;
	if (str==NULL)
		goto done;
	strcpy (tempstr, str+1);
	str = strchr (tempstr, '\\');
	if (str!=NULL)
		*str = '\0';
	strcpy(top_dir, tempstr);
/*
.....check if we have this dir already
*/
	if ( access(top_dir, 0) != -1 )
	{
		if (chdir (top_dir)!= 0)
			goto failed;
		goto make_dir;
	}
	if (mkdir(top_dir, 755) != 0)
		goto failed;
	if (chdir (top_dir)!= 0)
		goto failed;
	goto make_dir;
failed: status = -1;
done:;
	chdir(orig_dir);
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :  pw_chdir(pathname,nc)
**			Changes the default directory.
**		PARAMTERS:
**			pathname: full path name of directory to make the default.
**			nc:       Number of characters in 'pathname'.
**		RETURNS:
**			0 is returned if all went well, otherwise -1: something went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
extern "C" int pw_chdir(char *pathname,int *nc)
{
	int stat;
	char tempstr[256];
	strncpy(tempstr,pathname,*nc);
	tempstr[*nc] = '\0';
	stat = chdir(tempstr);
	return(stat);
}
/*******************************************************************
**   E_FUNCTION : pw_break_fname(fullname,dir,fname)
**              This function breaks a filename into two parts.
**                 1. device & directory spec
**                 2. filename
**   PARAMETERS  
**       INPUT  :  fullname  = full filename specification.
**       OUTPUT :  dir = directory specification of 'fullname'.
**                   Blank if no directory is specified.
**                 fname = filename specification of 'fullname'.
**                   Blank if no filename is specified.
**   RETURNS:    none.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
extern "C" void pw_break_fname(char *fullname, char *dir, char *fname)
{
	char *pt1;
	char *pointer;
	char buf[UX_MAX_PATH];
/*
.....Check for device or directory specification
*/
	*dir = '\0';
	strcpy (buf,fullname);
	pointer = strrchr (buf, '\\');
	pt1 = strrchr(buf,'/');
	if (pt1 > pointer) pointer = pt1;
	if (pointer != 0)
	{
		pointer++;
		strcpy (fname,pointer);
		pointer--;
		*pointer = '\0';
	}
	else
	{
		pointer = strrchr (buf,'\\');
		if (pointer == 0)
			strcpy (fname,buf);
		else
		{
			pointer++;
			strcpy (fname,pointer);
			pointer--;
			*pointer = '\0';
		}
	}
	strcpy(dir, buf);
}

#endif

