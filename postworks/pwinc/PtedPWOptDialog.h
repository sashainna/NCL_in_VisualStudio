/************************************************************************
c
c   FILE NAME: PtedPWOptDialog.h
c
c	 CONTAINS: 
c	 all PtedPWOptDialog class: a dialog with all the postworks options
c			variable and functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        PtedPWOptDialog.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:34
c
c**********************************************************************
*/
#if !defined(AFX_PTEDPWOPTDIALOG_H__DDF6E8D5_95F4_11D3_80E0_00C04F336F5E__INCLUDED_)
#define AFX_PTEDPWOPTDIALOG_H__DDF6E8D5_95F4_11D3_80E0_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// PtedPWOptDialog.h : header file
//
#include "ptedres.h"
/////////////////////////////////////////////////////////////////////////////
// PtedPWOptDialog dialog

class PtedPWOptDialog : public CDialog
{
// Construction
public:
	PtedPWOptDialog(CWnd* pParent = NULL, char *input = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(PtedPWOptDialog)
	enum { IDD = IDD_PWOPT_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	CString m_adjfile;
	int m_ftype, m_llen, m_warn, m_error, m_fatal, m_ident, m_list,
		m_plen, m_print, m_punch, m_simul;
	int m_cwarn, m_cerror, m_cfatal;
	CString m_clist, m_cprint, m_cpunch, m_csimul, m_option, m_machine;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(PtedPWOptDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual BOOL OnInitDialog();
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(PtedPWOptDialog)
	afx_msg void OnBrowse();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PTEDPWOPTDIALOG_H__DDF6E8D5_95F4_11D3_80E0_00C04F336F5E__INCLUDED_)
