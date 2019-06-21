/************************************************************************
c
c   FILE NAME: CutDlg.h
c
c	 CONTAINS: 
c		Header file for CCutDlg class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       CutDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:04
c
c**********************************************************************
*/
#if !defined(AFX_CUTDLG_H__83942584_DE33_11D3_8136_00C04F336F5E__INCLUDED_)
#define AFX_CUTDLG_H__83942584_DE33_11D3_8136_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// CutDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CCutDlg dialog

class CCutDlg : public CDialog
{
// Construction
public:
	CCutDlg(CWnd* pParent = NULL, int type=0);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CCutDlg)
	enum { IDD = IDD_CUTDLG };
	CString	m_def1;
	CString	m_def2;
	CString	m_def3;
	CString	m_def4;
	CString	m_def5;
	CString	m_def6;
	CString	m_def7;
	CString	m_parm1;
	CString	m_parm2;
	CString	m_parm3;
	CString	m_parm4;
	CString	m_parm5;
	CString	m_parm6;
	CString	m_parm7;
	int m_type;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CCutDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CUTDLG_H__83942584_DE33_11D3_8136_00C04F336F5E__INCLUDED_)
