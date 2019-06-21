/************************************************************************
c
c   FILE NAME: DefineDlg.h
c
c	 CONTAINS: 
c		Header file for CDefineDlg class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       DefineDlg.h , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       12/01/15 , 08:02:02
c
c**********************************************************************
*/
#if !defined(AFX_DEFINEDLG_H__A4DBC7F4_DD6F_11D3_8135_00C04F336F5E__INCLUDED_)
#define AFX_DEFINEDLG_H__A4DBC7F4_DD6F_11D3_8135_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// DefineDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CDefineDlg dialog

class CDefineDlg : public CDialog
{
// Construction
public:
	CDefineDlg(CWnd* pParent = NULL, char *tlib = NULL, 
		char* disp=NULL, char* slib=NULL, char *plib=NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CDefineDlg)
	enum { IDD = IDD_DEFINEDLG };
	CString m_tlib, m_slib, m_disp, m_plib;
	// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDefineDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CDefineDlg)
	virtual void OnOK();
	afx_msg void OnBrowse();
	afx_msg void OnBrowse2();
	afx_msg void OnBrowse1();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DEFINEDLG_H__A4DBC7F4_DD6F_11D3_8135_00C04F336F5E__INCLUDED_)
