/************************************************************************
**
**   FILE NAME: wsntclrdlg.h
**
**       Description - Functions and struct declarations for
**              CNCLColorDlg class
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntclrdlg.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:16
**********************************************************************
*/
#ifndef NCLCOLORDLG_H_
#define NCLCOLORDLG_H_

#pragma once
#include "wsntdrawbutton.h"
#include "wsntpalettewnd.h"
#include "wsntlumchooser.h"
#include "wsnthuesatchooser.h"

/////////////////////////////////////////////////////////////////////////////
// CNCLColorDlg dialog

class CNCLColorDlg : public CDialog
{
public:
	CNCLColorDlg(CWnd* pParent = NULL);	// standard constructor
	CNCLLumChooser		m_LumChooser;
	CNCLHueSatChooser	m_HueSatChooser;
	CNCLPaletteWnd	m_wndColor_basic;	
	CNCLPaletteWnd	m_wndColor_custom;	
	int m_current_color, m_saved_color;
	char m_color_cname[48][64];
	int m_cust_color[48][3];
	int m_addcolor;
	//{{AFX_DATA(CNCLColorDlg)
	enum { IDD = IDD_COLORPICKER_DIALOG };
	int	m_lum;
	int	m_hue;
	int	m_sat;
	int	m_red;
	int	m_green;
	int	m_blue;
	int m_defflag;
	char m_defstr[64];
	//}}AFX_DATA

	CNCLDrawButton		m_btnColor;	
	COLORREF	m_crColor;
	void	 SetColor(COLORREF cr)	{ m_crColor = cr; }
	COLORREF GetColor()				{ return m_crColor; }
	void SetColorIndex(int color) {m_current_color = color; }
	int GetColorIndex() { return m_current_color; }
	void SetColorDefault (int flag, char *defstr) 
	{
		m_defflag = flag;
		if (flag==1)
			sprintf_s(m_defstr, 64, "%s Color:", defstr);
	}
	void disable_addcolor() { m_addcolor = 0;}
	virtual void CreatePalette(BOOL bRedraw = TRUE);
	void SetHueSatBox(double h, double l, double s);
	int IsColorNameDefined(char *nstr);
	CPalette m_Palette;

protected:
	HCURSOR		m_hCursor;			// Select color cursor.
	BOOL		m_bPalette;
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLColorDlg)
	public:
	//{{AFX_VIRTUAL(CNCLColorDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

protected:
	HICON m_hIcon;
	// Generated message map functions
	//{{AFX_MSG(CNCLColorDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	virtual void OnOK();
	afx_msg void OnSysColorChange();
	afx_msg void OnDestroy();
	//}}AFX_MSG
	afx_msg LRESULT OnSelectColorOK(WPARAM wParam, LPARAM lParam);

	//}}AFX_MSG
	afx_msg void OnHueSatChange(NMHDR * pNotifyStruct, LRESULT * result );
	afx_msg void OnLumChange(NMHDR * pNotifyStruct, LRESULT * result );
	DECLARE_MESSAGE_MAP()
public:
	afx_msg void OnSatchanged();
	afx_msg void OnHueChanged();
	afx_msg void OnLumChanged();
	afx_msg void OnEnChangeRed();
	afx_msg void OnEnChangeGreen();
	afx_msg void OnEnChangeBlue();
	afx_msg void OnAddColor();
	afx_msg void OnCheckDef();
};
//{{AFX_INSERT_LOCATION}}
#endif
