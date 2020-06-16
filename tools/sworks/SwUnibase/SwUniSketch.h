/*********************************************************************
**  NAME:  SwUniSketch.h
**
**       Implementation of SolidWorks Attributes class functions.
**
** CONTAINS: CUniSketch class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwUniSketch.h , 24.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/13/14 , 14:54:33
*********************************************************************/
// UniSketch.h : Declaration of the CUniSketch

#ifndef __UNISKETCH_H_
#define __UNISKETCH_H_

#include "Swresource.h"       // main symbols
#include "nclxunib.h"
#include <atlhost.h>

/////////////////////////////////////////////////////////////////////////////
// CUniSketch
class CUniSketch : 
	public CAxDialogImpl<CUniSketch>
{
private:
	int m_type[4];
	char m_label[4][64];
	int *m_type_ptr[4];
	char *m_label_ptr[4];
	char c1str[2][20];
	int m_combo[4];
	int m_edit[4];

public:
	CUniSketch(NCLX_sw_options *swopt)
	{
		int i;
//
//...Store form structure
//
		m_type_ptr[0] = &swopt->ptlab_type;
		m_type_ptr[1] = &swopt->lnlab_type;
		m_type_ptr[2] = &swopt->cilab_type;
		m_type_ptr[3] = &swopt->kvlab_type;
		m_label_ptr[0] = swopt->ptlab;
		m_label_ptr[1] = swopt->lnlab;
		m_label_ptr[2] = swopt->cilab;
		m_label_ptr[3] = swopt->kvlab;
		for (i=0;i<4;i++)
		{
			strcpy(m_label[i],m_label_ptr[i]);
			m_type[i] = *m_type_ptr[i];
		}
//
//...Initialize variables
//

		strcpy(c1str[0],"Prefix");
		strcpy(c1str[1],"Subscript");

		m_combo[0] = IDC_UNISKETCH_COMBO1;
		m_combo[1] = IDC_UNISKETCH_COMBO2;
		m_combo[2] = IDC_UNISKETCH_COMBO3;
		m_combo[3] = IDC_UNISKETCH_COMBO4;

		m_edit[0] = IDC_UNISKETCH_EDIT1;
		m_edit[1] = IDC_UNISKETCH_EDIT2;
		m_edit[2] = IDC_UNISKETCH_EDIT3;
		m_edit[3] = IDC_UNISKETCH_EDIT4;
	}

	~CUniSketch()
	{
	}

	enum { IDD = IDD_UNISKETCH };

BEGIN_MSG_MAP(CUniSketch)
	MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
	COMMAND_ID_HANDLER(IDOK, OnOK)
	COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
	COMMAND_ID_HANDLER(IDC_UNISKETCH_COMBO1, OnCombo)
	COMMAND_ID_HANDLER(IDC_UNISKETCH_COMBO2, OnCombo)
	COMMAND_ID_HANDLER(IDC_UNISKETCH_COMBO3, OnCombo)
	COMMAND_ID_HANDLER(IDC_UNISKETCH_COMBO4, OnCombo)
END_MSG_MAP()
// Handler prototypes:
//  LRESULT MessageHandler(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
//  LRESULT CommandHandler(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled);
//  LRESULT NotifyHandler(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);

	LRESULT OnInitDialog(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{

	int i,j;
	CWindow combo;
	TCHAR bstr[20];
//
//...Initialize Form
//......Label types
//
		for (i=0;i<4;i++)
		{
			combo = GetDlgItem(m_combo[i]);
			for (j=0;j<2;j++)
			{
				mbstowcs(bstr,c1str[j],strlen(c1str[j])+1);
				combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
			}
			combo.SendMessage(CB_SETCURSEL,m_type[i],0);
		}
//
//......Labels
//
		for (i=0;i<4;i++)
		{
			SetDlgItemText(m_edit[i],(CComBSTR)m_label[i]);
		}
		return 1;  // Let the system set the focus
	}

	LRESULT OnOK(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		int i,nc;
		TCHAR bstr[64];
		for (i=0;i<4;i++)
		{
			GetDlgItemText(m_edit[i],bstr,63);
			nc = wcslen(bstr);
			wcstombs(m_label_ptr[i],bstr,nc+1);
			*m_type_ptr[i] = m_type[i];
		}
		
		EndDialog(wID);
		return 0;
	}

	LRESULT OnCancel(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		EndDialog(wID);
		return 0;
	}

	LRESULT OnCombo(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		int i,j,nc;
		char cstr[20];
		TCHAR bstr[20];
//
//...Get the toggle text
//
		GetDlgItemText(wID,bstr,20);
		nc = wcslen(bstr);
		wcstombs(cstr,bstr,nc+1);
//
//...Get active form field
//
		for (i=0;i<4;i++)
			if (wID == m_combo[i]) break;
//
//...Save the response
//
		if (i < 4)
		{
			for (j=0;j<2;j++)
			{
				if (strcmp(cstr,c1str[j]) == 0) m_type[i] = j;
			}
		}
	return 0;
	}
};

#endif //__UNISKETCH_H_
