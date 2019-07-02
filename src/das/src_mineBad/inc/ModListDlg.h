/************************************************************************
c
c   FILE NAME: ModListDlg.h
c
c	 CONTAINS: 
c		Header file for CModListDlg class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       ModListDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:06
c
c**********************************************************************
*/
#if !defined(AFX_MODLISTDLG_H__2D37E884_E315_11D3_813E_00C04F336F5E__INCLUDED_)
#define AFX_MODLISTDLG_H__2D37E884_E315_11D3_813E_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ModListDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CModListDlg dialog

class CModListDlg : public CDialog
{
// Construction
public:
	CModListDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CModListDlg)
	enum { IDD = IDD_MODLIST };
	BOOL	m_cutparm;
	BOOL	m_dispparm;
	BOOL	m_heading;
	BOOL	m_ltool;
	BOOL	m_pcut;
	BOOL	m_tnumdes;
	BOOL	m_optcom;
	BOOL	m_paramslb;
	BOOL	m_psymbol;
	BOOL	m_pshank;
	BOOL	m_pholder;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CModListDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CModListDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MODLISTDLG_H__2D37E884_E315_11D3_813E_00C04F336F5E__INCLUDED_)
