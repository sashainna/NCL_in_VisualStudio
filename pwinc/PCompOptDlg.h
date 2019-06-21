/************************************************************************
c
c   FILE NAME: PworksOptDlg.h
c
c	 CONTAINS: 
c		definitions of PCompOptDlg
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PCompOptDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:26 
c
c**********************************************************************
*/
#if !defined(AFX_PCOMPOPTDLG_H__7B2C9A71_E200_11D5_909D_00C04F336F5E__INCLUDED_)
#define AFX_PCOMPOPTDLG_H__7B2C9A71_E200_11D5_909D_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// PCompOptDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// PCompOptDlg dialog

class PCompOptDlg : public CDialog
{
// Construction
public:
	PCompOptDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(PCompOptDlg)
	enum { IDD = IDD_POSTCOMP_OPTIONS };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(PCompOptDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(PCompOptDlg)
	virtual BOOL OnInitDialog();
	virtual void OnCancel();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PCOMPOPTDLG_H__7B2C9A71_E200_11D5_909D_00C04F336F5E__INCLUDED_)
