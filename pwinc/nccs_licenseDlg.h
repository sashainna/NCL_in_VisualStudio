/************************************************************************
c
c   FILE NAME: nccs_licenseDlg.h
c
c	 CONTAINS: 
c		definitions of CNccs_licenseDlg
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        nccs_licenseDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:40
c
c**********************************************************************
*/

#if !defined(AFX_NCCS_LICENSEDLG_H__DD043767_E371_11D5_909D_00C04F336F5E__INCLUDED_)
#define AFX_NCCS_LICENSEDLG_H__DD043767_E371_11D5_909D_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CNccs_licenseDlg dialog

class CNccs_licenseDlg : public CDialog
{
// Construction
public:
	CNccs_licenseDlg(CWnd* pParent = NULL);	// standard constructor
	int Get_user_input (char buf[9][132]);
// Dialog Data
	//{{AFX_DATA(CNccs_licenseDlg)
	enum { IDD = IDD_NCCS_LICENSE_DIALOG };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNccs_licenseDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;
	HACCEL m_accel;

	// Generated message map functions
	//{{AFX_MSG(CNccs_licenseDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnAdd();
	afx_msg void OnClear();
	afx_msg void OnDelete();
	afx_msg void OnSearch();
	afx_msg void OnExit();
	afx_msg void OnFileLoadlicense();
	afx_msg void OnShowSearch();
	afx_msg void OnCheck1();
	afx_msg void OnCheck2();
	afx_msg void OnCheck3();
	afx_msg void OnCheck4();
	afx_msg void OnCheck5();
	afx_msg void OnCheck6();
	afx_msg void OnCheck7();
	afx_msg void OnCheck8();
	afx_msg void OnCheck9();
	afx_msg void OnTsearch();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCCS_LICENSEDLG_H__DD043767_E371_11D5_909D_00C04F336F5E__INCLUDED_)
