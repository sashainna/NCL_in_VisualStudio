/************************************************************************
c
c   FILE NAME: plotglobal.cpp
c
c	 CONTAINS: 
c		Defines the functions that can call from C function
c			but these function here will use MFC
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        plotglobal.cpp , 25.1
c     DATE AND TIME OF LAST  MODIFICATION
c        04/29/15 , 15:13:20
c
c**********************************************************************
*/
#include "stdafx.h"
#include <conio.h>
#include <winspool.h>
#include <sys/stat.h>
#include <sys/types.h>
#include "plot.h"
#include "plotDlg.h"
extern "C" int WNT_Plot;

/*********************************************************************
**    I_FUNCTION :  utp_wnt_msg(CWnd *parent, char *title, char *msg)
**       print message to status area
**    PARAMETERS   
**       INPUT  : 
**			parent: parent window
**			title: message window title
**          msg: message to be displayed
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int utp_wnt_msg(CWnd *parent, char *title, char *msg)
{
	int result;
	if (parent==NULL)
	{
		CPlotApp *app = (CPlotApp*)AfxGetApp();
		CPlotDlg *MainDlg = (CPlotDlg*)(app->GetMainWnd());		
		result = MainDlg->MessageBox(msg, title, MB_OK);
	}
	else
		result = parent->MessageBox(msg, title, MB_OK);
	return result;
}


/*********************************************************************
**    I_FUNCTION :  plot_wntstr_out(msg)
**       print message to status area
**    PARAMETERS   
**       INPUT  : 
**          msg: message to be printed
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void plot_wntstr_out(char *msg)
{
	char disp_msg[500];
	int len = strlen(msg);
	if (len==0) return;
	CPlotApp *app = (CPlotApp*)AfxGetApp();
	CPlotDlg *MainDlg = (CPlotDlg*)(app->GetMainWnd());
	CEdit *ewin = (CEdit*)(MainDlg->GetDlgItem(IDC_STATUS));
	int spos = ewin->GetWindowTextLength() + 1;
	ewin->SetSel(spos, spos);
/*
.....replace "\n" with "\r\n"
.....before display
*/
	int i, j;
	for (i=0, j=0; i<len; i++, j++)
	{
		if (msg[i]!='\n')
			disp_msg[j] = msg[i];
		else
		{
			disp_msg[j++] = '\r';
			disp_msg[j] = msg[i];
		}
	}
	disp_msg[j] = '\0';
	ewin->ReplaceSel(disp_msg);

	return;
}


/*********************************************************************
**    I_FUNCTION :  utp_ermsg(str1,str2)
**       Handle the error message.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void utp_ermsg(char	*str1,char	*str2)
{
	char string[1000];
	static int win_pos = 0;
	if (WNT_Plot!=1)
		_cprintf(str1,str2);
	else
	{
		sprintf(string, str1,str2);
		utp_wnt_msg(NULL, "Error!", string);
	}
}	


/*********************************************************************
**    I_FUNCTION     : utp_strout(str) 
**          Output a string to main window's status area
**    PARAMETERS
**       INPUT  :
**          string                  string to output
**       OUTPUT :
**						nine
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void utp_strout(char	*str)
{
	static int win_pos = 0;
	if (WNT_Plot!=1)
		_cprintf(str);
	else
	{
		plot_wntstr_out(str);
	}
}	
	
extern "C" void Disp_Console()
{
	AllocConsole();
}

extern "C" void Free_Console()
{
	FreeConsole();	
}


