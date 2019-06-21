/*********************************************************************
**  NAME:  SwUniAttr.cpp
**
**       Implementation of SolidWorks Attributes class functions.
**
** CONTAINS: CUniAttr class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwUniAttr.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:08
*********************************************************************/
// UniAttr.h : Declaration of the CUniAttr

#ifndef __UNIATTR_H_
#define __UNIATTR_H_

#include "Swresource.h"       // main symbols
#include <atlhost.h>

extern "C" int UIG_color_sec;
extern "C" int UIG_layer_sec;

/////////////////////////////////////////////////////////////////////////////
// CUniAttr
class CUniAttr : 
	public CAxDialogImpl<CUniAttr>
{
private:
	int m_layers[7];
	int m_colors[7];
	int *m_layer_ptr;
	int *m_color_ptr;
	char c1str[16][20];
	int m_combo[7];
	int m_edit[7];

public:
	CUniAttr(int *layers,int *colors)
	{
		int i;
//
//...Store form structure
//
		m_layer_ptr = layers;
		m_color_ptr = colors;
		for (i=0;i<6;i++)
		{
			m_layers[i] = layers[i];
			m_colors[i] = colors[i];
		}
		m_layers[6]=UIG_layer_sec;
		m_colors[6]=UIG_color_sec;

//
//...Initialize variables
//

		strcpy(c1str[0],"Default");
		strcpy(c1str[1],"White");
		strcpy(c1str[2],"Blue");
		strcpy(c1str[3],"Red");
		strcpy(c1str[4],"Green");
		strcpy(c1str[5],"Magenta");
		strcpy(c1str[6],"Yellow");
		strcpy(c1str[7],"Cyan");
		strcpy(c1str[8],"Brown");
		strcpy(c1str[9],"Tan");
		strcpy(c1str[10],"Lt Blue");
		strcpy(c1str[11],"Sea Green");
		strcpy(c1str[12],"Orange");
		strcpy(c1str[13],"Pink");
		strcpy(c1str[14],"Purple");
		strcpy(c1str[15],"Grey");

		m_combo[0] = IDC_UNIATTR_COMBO1;
		m_combo[1] = IDC_UNIATTR_COMBO2;
		m_combo[2] = IDC_UNIATTR_COMBO3;
		m_combo[3] = IDC_UNIATTR_COMBO4;
		m_combo[4] = IDC_UNIATTR_COMBO5;
		m_combo[5] = IDC_UNIATTR_COMBO6;
		m_combo[6] = IDC_UNIATTR_COMBO7;

		m_edit[0] = IDC_UNIATTR_EDIT1;
		m_edit[1] = IDC_UNIATTR_EDIT2;
		m_edit[2] = IDC_UNIATTR_EDIT3;
		m_edit[3] = IDC_UNIATTR_EDIT4;
		m_edit[4] = IDC_UNIATTR_EDIT5;
		m_edit[5] = IDC_UNIATTR_EDIT6;
		m_edit[6] = IDC_UNIATTR_EDIT7;
	}

	~CUniAttr()
	{
	}

	enum { IDD = IDD_UNIATTR };

BEGIN_MSG_MAP(CUniAttr)
	MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
	COMMAND_ID_HANDLER(IDOK, OnOK)
	COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
	COMMAND_ID_HANDLER(IDC_UNIATTR_COMBO1, OnCombo)
	COMMAND_ID_HANDLER(IDC_UNIATTR_COMBO2, OnCombo)
	COMMAND_ID_HANDLER(IDC_UNIATTR_COMBO3, OnCombo)
	COMMAND_ID_HANDLER(IDC_UNIATTR_COMBO4, OnCombo)
	COMMAND_ID_HANDLER(IDC_UNIATTR_COMBO5, OnCombo)
	COMMAND_ID_HANDLER(IDC_UNIATTR_COMBO6, OnCombo)
	COMMAND_ID_HANDLER(IDC_UNIATTR_COMBO7, OnCombo)
END_MSG_MAP()
// Handler prototypes:
//  LRESULT MessageHandler(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
//  LRESULT CommandHandler(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled);
//  LRESULT NotifyHandler(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);

	LRESULT OnInitDialog(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{

	int i,j;
	char sbuf[20];
	CWindow combo;
	TCHAR bstr[20];
//
//...Initialize Form
//......Layers
//
		for (i=0;i<7;i++)
		{
			sprintf(sbuf,"%d",m_layers[i]);
			SetDlgItemText(m_edit[i],(CComBSTR)sbuf);
		}
//
//......Colors
//
		for (i=0;i<7;i++)
		{
			combo = GetDlgItem(m_combo[i]);
			for (j=0;j<16;j++)
			{
				mbstowcs(bstr,c1str[j],strlen(c1str[j])+1);
				combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
			}
			combo.SendMessage(CB_SETCURSEL,m_colors[i],0);
		}
		return 1;  // Let the system set the focus
	}

	LRESULT OnOK(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		int i,nc;
		char cstr[20];
		TCHAR bstr[20];
		for (i=0;i<7;i++)
		{
			GetDlgItemText(m_edit[i],bstr,20);
			nc = wcslen(bstr);
			wcstombs(cstr,bstr,nc+1);
			sscanf(cstr,"%d",&m_layers[i]);
			if (m_layers[i] < 1 || m_layers[i] > 9999)
			{
				AfxMessageBox(_T("Layers must be between 1 and 9999."));
				return 1;
			}
		}
		for (i=0;i<6;i++)
		{
			m_layer_ptr[i] = m_layers[i];
			m_color_ptr[i] = m_colors[i];
		}
		UIG_layer_sec = m_layers[6];
		UIG_color_sec = m_colors[6];
		
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
		for (i=0;i<7;i++)
			if (wID == m_combo[i]) break;
//
//...Save the response
//
		if (i < 7)
		{
			for (j=0;j<16;j++)
			{
				if (strcmp(cstr,c1str[j]) == 0) m_colors[i] = j;
			}
		}
	return 0;
	}
};

#endif //__UNIATTR_H_
