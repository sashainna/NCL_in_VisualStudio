/************************************************************************
c
c   FILE NAME: StepFiltDlg.h
c
c	 CONTAINS: 
c		Header file for the class CStepFiltDlg
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       StepFiltDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:07
c
c**********************************************************************
*/
#if !defined(AFX_STEPFILTDLG_H__80605563_0D7C_11D5_9079_00C04F336F5E__INCLUDED_)
#define AFX_STEPFILTDLG_H__80605563_0D7C_11D5_9079_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// StepFiltDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CStepFiltDlg dialog

class CStepFiltDlg : public CDialog
{
// Construction
public:
	CStepFiltDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CStepFiltDlg)
	enum { IDD = IDD_FILTER_ENT };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CStepFiltDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CStepFiltDlg)
	afx_msg void OnAlloff();
	afx_msg void OnAllon();
	virtual void OnCancel();
	afx_msg void OnCheckSfCv();
	afx_msg void OnAccept();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	afx_msg void OnSetEntity(UINT id);
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STEPFILTDLG_H__80605563_0D7C_11D5_9079_00C04F336F5E__INCLUDED_)
