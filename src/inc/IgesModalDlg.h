/************************************************************************
c
c   FILE NAME: IgesModalDlg.h
c
c	 CONTAINS: 
c		Header file for the class CIgesModalDlg
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       IgesModalDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:05               
c
c**********************************************************************
*/
#if !defined(AFX_IGESMODALDLG_H__80605567_0D7C_11D5_9079_00C04F336F5E__INCLUDED_)
#define AFX_IGESMODALDLG_H__80605567_0D7C_11D5_9079_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// IgesModalDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CIgesModalDlg dialog

class CIgesModalDlg : public CDialog
{
// Construction
public:
	CIgesModalDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CIgesModalDlg)
	enum { IDD = IDD_NAME_MODALS };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CIgesModalDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CIgesModalDlg)
	virtual void OnCancel();
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_IGESMODALDLG_H__80605567_0D7C_11D5_9079_00C04F336F5E__INCLUDED_)
