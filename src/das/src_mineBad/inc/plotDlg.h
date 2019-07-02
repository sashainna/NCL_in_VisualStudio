/************************************************************************
c
c   FILE NAME: plotDlg.h
c
c	 CONTAINS: 
c		Header file for the class CplotDlg
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       plotDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:41               
c
c**********************************************************************
*/
// plotDlg.h : header file
//

#if !defined(AFX_PLOTDLG_H__EFC0F5DC_CEAD_11D3_8118_00C04F336F5E__INCLUDED_)
#define AFX_PLOTDLG_H__EFC0F5DC_CEAD_11D3_8118_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

/////////////////////////////////////////////////////////////////////////////
// CPlotDlg dialog

class CPlotDlg : public CDialog
{
// Construction
public:
	CPlotDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CPlotDlg)
	enum { IDD = IDD_PLOT_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPlotDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CPlotDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnBrowse1();
	afx_msg void OnBrowse2();
	afx_msg void OnSelchangeBypass();
	afx_msg void OnSelchangeOutput();
	afx_msg void OnSelchangePlotSize();
	afx_msg void OnSelchangePlotType();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PLOTDLG_H__EFC0F5DC_CEAD_11D3_8118_00C04F336F5E__INCLUDED_)
