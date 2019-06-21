/************************************************************************
c
c   FILE NAME: PmacroOptDlg.h
c
c	 CONTAINS: 
c		definitions of PMacroOptDlg
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PMacroOptDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:27 
c
c**********************************************************************
*/
#if !defined(AFX_PMICROOPTDLG_H__27241EF3_DDEF_11D5_909D_00C04F336F5E__INCLUDED_)
#define AFX_PMACROOPTDLG_H__27241EF3_DDEF_11D5_909D_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// PMacroOptDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// PMacroOptDlg dialog

class PMacroOptDlg : public CDialog
{
// Construction
public:
	PMacroOptDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(PMacroOptDlg)
	enum { IDD = IDD_OPTION_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(PMacroOptDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(PMacroOptDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnOk();
	afx_msg void OnCancel();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PMACROOPTDLG_H__27241EF3_DDEF_11D5_909D_00C04F336F5E__INCLUDED_)
