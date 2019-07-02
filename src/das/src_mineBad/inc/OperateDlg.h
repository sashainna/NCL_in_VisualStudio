/************************************************************************
c
c   FILE NAME: OperateDlg.h
c
c	 CONTAINS: 
c		Header file for COperateDlg class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       OperateDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:06               
c
c**********************************************************************
*/
#if !defined(AFX_OPERATEDLG_H__99CE12D3_2109_11D4_817F_00C04F336F5E__INCLUDED_)
#define AFX_OPERATEDLG_H__99CE12D3_2109_11D4_817F_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// OperateDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// COperateDlg dialog

class COperateDlg : public CDialog
{
// Construction
public:
	COperateDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(COperateDlg)
	enum { IDD = IDD_OPERATE };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(COperateDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(COperateDlg)
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_OPERATEDLG_H__99CE12D3_2109_11D4_817F_00C04F336F5E__INCLUDED_)
