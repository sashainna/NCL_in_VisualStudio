/************************************************************************
c
c   FILE NAME: CIgesOptDlg.h
c
c	 CONTAINS: 
c		Header file for the class CIgesOptDlg 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       IgesOptDlg.h , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/27/16 , 11:37:56
c
c**********************************************************************
*/
#if !defined(AFX_IGESOPTDLG_H__43B31D64_C2B1_11D3_810B_00C04F336F5E__INCLUDED_)
#define AFX_IGESOPTDLG_H__43B31D64_C2B1_11D3_810B_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// IgesOptDlg.h : header file
//

#include "xenv1.h"
#include "wsntclrbtn.h"
/////////////////////////////////////////////////////////////////////////////
// CIgesOptDlg dialog

class CIgesOptDlg : public CDialog
{
// Construction
public:
	CIgesOptDlg(CWnd* pParent = NULL, char *fname=NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CIgesOptDlg)
	enum { IDD = IDD_OPTIONS };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CIgesOptDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	char m_fname[UX_MAX_PATH_LEN];
	int m_edge_color;
	CNCLColorButton m_button;
	void InitRangeList();
	void SetButColor(int color);
	void InitColor();
	// Generated message map functions
	//{{AFX_MSG(CIgesOptDlg)
	afx_msg void OnSelectAll();
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	afx_msg void OnClearAll();
	afx_msg void OnSelchangeCoption();
	afx_msg void OnSelchangeLabelOption();
	afx_msg void OnBrowse();
	afx_msg void OnFilter();
	afx_msg void OnNameModals();
	afx_msg void OnAttr();
	afx_msg void OnEdgeColor();
	virtual void OnNoDups();
	afx_msg void OnEdgeColorSel();
	afx_msg void OnShade();
	afx_msg void OnDecompose(); 
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_IGESOPTDLG_H__43B31D64_C2B1_11D3_810B_00C04F336F5E__INCLUDED_)
