/************************************************************************
c
c   FILE NAME: PtedSetDialog.h
c
c	 Description - Values defined for PtedSetDialog
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        PtedSetDialog.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:34 
c
c**********************************************************************
*/
#if !defined(AFX_PTEDSETDIALOG_H__3DAF0AB3_782C_11D3_80BE_00C04F336F5E__INCLUDED_)
#define AFX_PTEDSETDIALOG_H__3DAF0AB3_782C_11D3_80BE_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// PtedSetDialog.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// PtedSetDialog dialog
#include "ptedres.h"

class PtedSetDialog : public CDialog
{
// Construction
public:
	PtedSetDialog(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(PtedSetDialog)
	enum { IDD = IDD_PROMPT };
	CString	m_settext;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(PtedSetDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(PtedSetDialog)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PTEDSETDIALOG_H__3DAF0AB3_782C_11D3_80BE_00C04F336F5E__INCLUDED_)
