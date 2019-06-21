/************************************************************************
c
c   FILE NAME: LicenseLstDlg.h
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        LicenseLstDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:26 
c
c**********************************************************************
*/
#if !defined(AFX_LICENSELSTDLG_H__F63A3382_F587_11D5_909D_00C04F336F5E__INCLUDED_)
#define AFX_LICENSELSTDLG_H__F63A3382_F587_11D5_909D_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CLicenseLstDlg dialog

class CLicenseLstDlg : public CDialog
{
// Construction
public:
	CLicenseLstDlg(CWnd* pParent = NULL);   // standard constructor
	~CLicenseLstDlg();
// Dialog Data
	//{{AFX_DATA(CLicenseLstDlg)
	enum { IDD = IDD_DLG_LISTCTRL };
	CListCtrl	m_LicenseList;
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLicenseLstDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	char *m_liclist[1000][10];
	// Generated message map functions
	//{{AFX_MSG(CLicenseLstDlg)
	afx_msg void OnCreate();
	virtual void OnCancel();
	afx_msg void OnSelectall();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_LICENSELSTDLG_H__F63A3382_F587_11D5_909D_00C04F336F5E__INCLUDED_)
