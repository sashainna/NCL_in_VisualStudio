/************************************************************************
c
c   FILE NAME: nccs_authDlg.h
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        nccs_authDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:39 
c
c**********************************************************************
*/

#if !defined(AFX_NCCS_AUTHDLG_H__F63A3377_F587_11D5_909D_00C04F336F5E__INCLUDED_)
#define AFX_NCCS_AUTHDLG_H__F63A3377_F587_11D5_909D_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CNccs_authDlg dialog

class CNccs_authDlg : public CDialog
{
// Construction
public:
	CNccs_authDlg(CWnd* pParent = NULL);	// standard constructor
	void showsuper();

// Dialog Data
	//{{AFX_DATA(CNccs_authDlg)
	enum { IDD = IDD_NCCS_AUTH_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNccs_authDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;
	HACCEL m_accel;
	int m_sizechg;
	CRect m_origrect;
	int m_issuper;
	int Get_user_input (char buf[10][132], int parm[10]);
	void SizeDialogItem(int cx, int cy );

	// Generated message map functions
	//{{AFX_MSG(CNccs_authDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnAdd();
	afx_msg void OnClear();
	afx_msg void OnDelete();
	afx_msg void OnExit();
	afx_msg void OnFileLoadlicense();
	afx_msg void OnLicenseCalculatepassword();
	afx_msg void OnLicenseCreatelicensefile();
	afx_msg void OnSearch();
	afx_msg void OnFileAdministrator();
	afx_msg void OnFileCreatebatch();
	afx_msg void OnShowSearch();
	afx_msg void OnSize( UINT nType, int cx, int cy );
	afx_msg void OnEditPurge();
	afx_msg void OnCheck1();
	afx_msg void OnCheck2();
	afx_msg void OnCheck3();
	afx_msg void OnCheck4();
	afx_msg void OnCheck5();
	afx_msg void OnCheck6();
	afx_msg void OnCheck7();
	afx_msg void OnCheck8();
	afx_msg void OnCheck9();
	afx_msg void OnCheck10();
	afx_msg void OnTsearch();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCCS_AUTHDLG_H__F63A3377_F587_11D5_909D_00C04F336F5E__INCLUDED_)
