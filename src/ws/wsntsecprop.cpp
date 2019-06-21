/********************************************************************* 
**  NAME:  wsntsecprop.cpp
**
**			Native WinNT section property functions
**			implementation of CNCLSecProp class functions
**	CONTAINS: CNCLSecProp class functions
**			all functions declared in wsnttxtwin.h
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntsecprop.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 18:01:56
*********************************************************************/
#include "stdafx.h"
#include <gdiplus.h>
#include "wsntctl.h"
#include "wsntsecprop.h"
#include "lcom.h"
#include "wsgl.h"
#include "wsntclrdlg.h"

/***********************************************************************
**
**   FUNCTION: CNCLSecProp(CWnd* pParentWnd, char *title)
**
**              Constructor of class CNCLSecProp
**
**   INPUT:  pParentWnd: parent window
**			title: title of window
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLSecProp::CNCLSecProp(CWnd* pParentWnd, char *title) : CDialog(CNCLSecProp::IDD, pParentWnd)
{
	if (title!=NULL)
		m_title = title;
	else
		m_title = "";
}

/***********************************************************************
**
**   FUNCTION: ~CNCLSecProp()
**
**              Destructor of class CNCLSecProp
**			
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLSecProp::~CNCLSecProp()
{
}


void CNCLSecProp::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNCLSecProp)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CNCLSecProp, CDialog)
	//{{AFX_MSG_MAP(CNCLSecProp)
	ON_COMMAND(IDC_FFG_CBUT, &CNCLSecProp::OnColorBut)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize window text
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CNCLSecProp::OnInitDialog()
{
	CDialog::OnInitDialog();
	m_button.SubclassDlgItem(IDC_FFG_CBUT,this);

	COLORREF bcolor;
	if (m_color<0)
	{
		bcolor = RGB(0,0,0); 
	}
	else
	{
		bcolor = RGB(uw_color_table[m_color][0], 
					uw_color_table[m_color][1], 
					uw_color_table[m_color][2]);
	}

	m_button.set_color(GetSysColor(COLOR_BTNFACE), bcolor);
	m_button.SetFont(GetFont());	
	m_button.ShowWindow(SW_SHOW);
	SetWindowText(m_title);
	m_button.SetWindowText(m_text);
	((CEdit*)GetDlgItem(IDC_EDIT1))->SetWindowText(m_text);
	((CEdit*)GetDlgItem(IDC_EDIT1))->SetFocus();
	return FALSE;
}	

/***********************************************************************
**
**   SUBROUTINE: OnOK()
**
**   FUNCTION:  callback for "OK" button
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLSecProp::OnOK()
{
	((CEdit*)GetDlgItem(IDC_EDIT1))->GetWindowText(m_text);
	CDialog::OnOK();
}

/***********************************************************************
**
**   SUBROUTINE: OnColorBut()
**
**   FUNCTION:  callback for Color button
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLSecProp::OnColorBut()
{
	GdiplusStartupInput gdiplusStartupInput;
	ULONG_PTR gdiplusToken;

	GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
	CNCLColorDlg dlg;
	dlg.SetColorIndex(m_color);
	dlg.SetColorDefault(1, "Default Color:");
	int nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
/*
.....set the field color to select color
*/
		m_color =  dlg.m_current_color;
	}
	GdiplusShutdown(gdiplusToken);

	COLORREF bcolor;
	if (m_color<0)
	{
		bcolor = RGB(0,0,0); 
	}
	else
	{
		bcolor = RGB(uw_color_table[m_color][0], 
					uw_color_table[m_color][1], 
					uw_color_table[m_color][2]);
	}
	m_button.set_color(GetSysColor(COLOR_BTNFACE), bcolor);
	m_button.Invalidate();
	m_button.UpdateWindow();
}
