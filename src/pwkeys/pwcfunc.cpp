/***********************************************************************
**
**   FILE NAME: pwcfunc.cpp
**
**   CONTAINS:
**			add1dispmsg()
**			  Pw_dispmsg()
**
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
**          All Rights Reserved
**     MODULE NAME AND RELEASE LEVEL
**			pwcfunc.cpp , 23.1
**    DATE AND TIME OF LAST  MODIFICATION
**			05/22/12 , 11:15:15      
**
**********************************************************************/

#ifdef WNT
#include "pwstdafx.h"
#include <conio.h>
#include "pwwindef.h"
//#include <afxwin.h>
#include "PWMessageBox.h"

CWnd *Pw_maindlg = NULL;
int UU_BATCH = 0;
FILE *PW_logfile = NULL;
char PW_msgtitle[256] = "";
//CWnd *Pw_maindlg = NULL;
PWMessageBox PW_msgbox;
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
		len = (int)strlen(msgline);
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
		if (Pw_maindlg==NULL)
		{
			if (PW_msgtitle[0]=='\0')
				MessageBox(NULL, msgline, "Postworks Message", MB_OK);
			else
				MessageBox(NULL, msgline, PW_msgtitle, MB_OK);
		}
		else
		{
			CWnd* parent = Pw_maindlg->GetActiveWindow();
			if (PW_msgtitle[0]=='\0')
				parent->MessageBox(msgline, "Postworks Message", MB_OK);
			else
				parent->MessageBox(msgline, PW_msgtitle, MB_OK);
		}
		return;
	}

	CEdit *edt;
	if (UU_BATCH==0)
		edt = (CEdit *) (Pw_maindlg->GetDlgItem(IDC_POSTWORKS_MESSAGE));
	else
		edt = (CEdit *) (Pw_maindlg->GetDlgItem(IDC_BATCH_MESSAGE));

	//char *buf;
	CString wintxt;
	len = (int)strlen(msgline);
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

#endif

