/************************************************************************
c
c   FILE NAME: ComDlg.h
c
c	 CONTAINS: 
c		Header file for CComDlg class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       ComDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:04
c
c**********************************************************************
*/
#if !defined(AFX_COMDLG_H__83942585_DE33_11D3_8136_00C04F336F5E__INCLUDED_)
#define AFX_COMDLG_H__83942585_DE33_11D3_8136_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ComDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CComDlg dialog

class CComDlg : public CDialog
{
// Construction
public:
	CComDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CComDlg)
	enum { IDD = IDD_COMDLG };
	CString m_com1;
	CString m_com2;
	CString m_com3;
	CString m_com4;
	CString m_com5;
	CString m_com6;
	CString m_com7;
	CString m_com8;
	CString m_com9;
	CString m_com10;
	CString m_com11;
	CString m_com12;
	CString m_com13;
	CString m_com14;
	CString m_com15;
	CString m_com16;
	CString m_com17;
	CString m_com18;
	CString m_com19;
	CString m_com20;
	CString m_com21;
	CString m_com22;
	CString m_com23;
	CString m_com24;
	CString m_com25;
	CString m_com26;
	CString m_com27;
	CString m_com28;
	CString m_com29;
	CString m_com30;
	CString m_com31;
	CString m_com32;
	CString m_com33;
	CString m_com34;
	CString m_com35;
	CString m_com36;
	CString m_com37;
	CString m_com38;
	CString m_com39;
	CString m_com40;
	CString m_com41;
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CComDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CComDlg)
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_COMDLG_H__83942585_DE33_11D3_8136_00C04F336F5E__INCLUDED_)
