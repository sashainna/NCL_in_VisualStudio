/************************************************************************
c
c   FILE NAME: PworksOptDlg.h
c
c	 CONTAINS: 
c		definitions of PWorksOptDlg
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PWorksOptDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:27 
c
c**********************************************************************
*/
#if !defined(AFX_PWORKSOPTDLG_H__E033F571_C888_11D5_908A_00C04F336F5E__INCLUDED_)
#define AFX_PWORKSOPTDLG_H__E033F571_C888_11D5_908A_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// PWorksOptDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// PWorksOptDlg dialog

class PWorksOptDlg : public CDialog
{
// Construction
public:
	PWorksOptDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(PWorksOptDlg)
	enum { IDD = IDD_PWORKS_OPTIONS };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(PWorksOptDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(PWorksOptDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnOptionBrowse1();
	virtual void OnCancel();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PWORKSOPTDLG_H__E033F571_C888_11D5_908A_00C04F336F5E__INCLUDED_)
