/************************************************************************
c
c   FILE NAME: PtedReseqDialog.h
c
c	 CONTAINS: 
c	 all PtedReseqDialog class: a dialog with all the Resequence input
c			variables and functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedReseqDialog.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:58:34
c
c**********************************************************************
*/
#if !defined(AFX_PTEDRESEQDIALOG_H__3F1F4163_7676_11D3_80BC_00C04F336F5E__INCLUDED_)
#define AFX_PTEDRESEQDIALOG_H__3F1F4163_7676_11D3_80BC_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// PtedReseqDialog.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// PtedReseqDialog dialog

#include "PtedRangeBox.h"

class PtedReseqDialog : public CDialog
{

public:
	PtedRangeStruct m_sRange;
	int m_bseq, m_seqinc, m_seqn, m_nonly;
	PtedReseqDialog(CWnd* pParent = NULL);   
// Dialog Data
	//{{AFX_DATA(PtedReseqDialog)
	enum { IDD = IDD_RESEQ_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(PtedReseqDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual BOOL OnInitDialog();
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(PtedReseqDialog)
	afx_msg void OnReqRange();
	virtual void OnOK();
	virtual void OnCancel();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PTEDRESEQDIALOG_H__3F1F4163_7676_11D3_80BC_00C04F336F5E__INCLUDED_)
