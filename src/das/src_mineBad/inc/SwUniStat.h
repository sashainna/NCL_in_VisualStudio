/*********************************************************************
**  NAME:  SwUniStat.cpp
**
**       Implementation of NCL/Solid Status Window functions.
**
** CONTAINS: CUniStat class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwUniStat.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:09
*********************************************************************/
// UniStat.h : Declaration of the CUniStat

#ifndef __UNISTAT_H_
#define __UNISTAT_H_

#include "Swresource.h"       // main symbols
#include <atlhost.h>

/////////////////////////////////////////////////////////////////////////////
// CUniStat
class CUniStat : 
	public CAxDialogImpl<CUniStat>
{
private:
	BOOL interrupt;
	int last_prog;

public:
	void AddList(char *str);
	void ShowProgress(int prog);
	BOOL GetInterrupt();
	void Finished();
	void Wait();

public:
	CUniStat()
	{
		interrupt = FALSE;
		last_prog = 0;
		Create(NULL,0);
	}

	~CUniStat()
	{
		DestroyWindow();
	}

	enum { IDD = IDD_UNISTAT };

BEGIN_MSG_MAP(CUniStat)
	MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
	COMMAND_ID_HANDLER(IDOK, OnOK)
	COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
END_MSG_MAP()
// Handler prototypes:
//  LRESULT MessageHandler(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
//  LRESULT CommandHandler(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled);
//  LRESULT NotifyHandler(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);

	LRESULT OnInitDialog(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		::EnableWindow(GetDlgItem(IDC_UNISTAT_OK),FALSE);
		ShowWindow(SW_SHOW);
		return 1;  // Let the system set the focus
	}

	LRESULT OnOK(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		interrupt = TRUE;
//		DestroyWindow();
//		EndDialog(wID);
		return 0;
	}

	LRESULT OnCancel(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		interrupt = TRUE;
//		DestroyWindow();
//		EndDialog(wID);
		return 0;
	}
};

#endif //__UNISTAT_H_
