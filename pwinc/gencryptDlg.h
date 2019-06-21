/************************************************************************
c
c   FILE NAME: gencryptDlg.h
c
c   CONTAINS:
c     definitions of gencryptDlg
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        gencryptDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:38
c
c**********************************************************************
*/
// gencryptDlg.h : header file
//

#if !defined(AFX_GENCRYPTDLG_H__190C3529_4664_11D6_90AE_00C04F336F5E__INCLUDED_)
#define AFX_GENCRYPTDLG_H__190C3529_4664_11D6_90AE_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CGencryptDlg dialog

class CGencryptDlg : public CDialog
{
// Construction
public:
	CGencryptDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CGencryptDlg)
	enum { IDD = IDD_GENCRYPT_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CGencryptDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CGencryptDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_GENCRYPTDLG_H__190C3529_4664_11D6_90AE_00C04F336F5E__INCLUDED_)
