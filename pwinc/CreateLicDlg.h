/************************************************************************
**
**   FILE NAME: CreateLicDlg.cpp
**
**  Description - Functions and implementations for
**    CCreateLic Dlg class.
**
**  CONTAINS:
**
**    COPYRIGHT 2002 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       CreateLicDlg.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:25
*********************************************************************/
#if !defined(AFX_CREATELICDLG_H__F63A3381_F587_11D5_909D_00C04F336F5E__INCLUDED_)
#define AFX_CREATELICDLG_H__F63A3381_F587_11D5_909D_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// CreateLicDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CCreateLicDlg dialog

class CCreateLicDlg : public CDialog
{
// Construction
public:
	CCreateLicDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CCreateLicDlg)
	enum { IDD = IDD_MATCH_LISTS };
	CCheckListBox   m_lbLicense;
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCreateLicDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CCreateLicDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSelectall();
	afx_msg void OnCreate();
	virtual void OnCancel();
	afx_msg void SelectCallbacks();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CREATELICDLG_H__F63A3381_F587_11D5_909D_00C04F336F5E__INCLUDED_)
