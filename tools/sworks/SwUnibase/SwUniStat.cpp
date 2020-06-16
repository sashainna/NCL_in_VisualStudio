/*********************************************************************
**  NAME:  SwUniStat.cpp
**
**       Implementation of Status Window class functions.
**
** CONTAINS: CUniStat class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwUniStat.cpp , 24.3
**    DATE AND TIME OF LAST  MODIFICATION
**       10/29/14 , 13:22:10
*********************************************************************/
// UniStat.cpp : Implementation of CUniStat
#include "SwStdAfx.h"
#include "SwUniStat.h"

/*********************************************************************
**    I_FUNCTION     :  AddList(str)
**       Outputs a line to the Status Window
**    PARAMETERS
**       INPUT  :
**          str      = Character string to output.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CUniStat::AddList(char *str)
{
	int i;
	TCHAR bstr[1024];
//
//...Append the string to the list box
//
	mbstowcs(bstr,str,strlen(str)+1);
	CWindow clist(GetDlgItem(IDC_UNISTAT_LIST1));
	i = clist.SendMessage(LB_ADDSTRING,0,(LPARAM)bstr);
	clist.SendMessage(LB_SETTOPINDEX,(WPARAM)i,0);
	UpdateWindow();
//
//...End of routine
//
	return;
}

/*********************************************************************
**    I_FUNCTION     :  ShowProgress(prog)
**       Updates the progress bar.
**    PARAMETERS
**       INPUT  :
**          prog     = Progress value from 1 to 100.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CUniStat::ShowProgress(int inprog)
{
	int prog;
//
//...Update the progress bar
//
	prog = inprog;
	if (prog < last_prog)
	{
		prog = inprog;
	}
	if (prog > 100) prog = 100;
	if (prog == 0 || prog == 100 || prog >= last_prog+2)
	{
		CWindow clist(GetDlgItem(IDC_UNISTAT_PROGRESS1));
		clist.SendMessage(PBM_SETPOS,(WPARAM)prog,0);
		UpdateWindow();
		last_prog = prog;
	}
//
//...End of routine
//
	return;
}

/*********************************************************************
**    I_FUNCTION     :  GetInterrupt()
**       Checks for a user interrupt.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      :
**          TRUE if the user interrupted the process.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
BOOL CUniStat::GetInterrupt()
{
	MSG msg;
	if (::PeekMessage(&msg,NULL,NULL,NULL,PM_NOREMOVE))
	{
		GetMessage(&msg,NULL,NULL,NULL);
		DispatchMessage(&msg);
	}
	return(interrupt);
}

/*********************************************************************
**    I_FUNCTION     :  Finished()
**       This routine should be called when the Unibase transfer is
**       finished. It disables teh Cancel button and Enables the OK button.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CUniStat::Finished()
{
	interrupt = FALSE;
	::EnableWindow(GetDlgItem(IDC_UNISTAT_OK),TRUE);
	::EnableWindow(GetDlgItem(IDC_UNISTAT_CANCEL),FALSE);
}

/*********************************************************************
**    I_FUNCTION     :  Wait()
**       This routine waits for the user to accept the active form.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CUniStat::Wait()
{
	MSG msg;
//
//...Wait for the user to press OK
//
	while (!interrupt)
	{
		GetMessage(&msg,NULL,NULL,NULL);
		DispatchMessage(&msg);
	}
}
