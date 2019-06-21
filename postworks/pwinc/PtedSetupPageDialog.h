/************************************************************************
c
c   FILE NAME: PtedSetupPageDialog.h
c
c	 CONTAINS: 
c	 all PtedSetupPageDialog class: print setup dialog class
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			 PtedSetupPageDialog.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			 09/11/13 , 12:58:34
c
c**********************************************************************
*/
#if !defined(AFX_PTEDSETUPPAGEDIALOG_H__3F1F4163_7676_11D3_80BC_00C04F336F5E__INCLUDED_)
#define AFX_PTEDSETUPPAGEDIALOG_H__3F1F4163_7676_11D3_80BC_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// PtedSetupPageDialog.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// PtedSetupPageDialog dialog

class PtedSetupPageDialog : public CDialog
{

public:
	int m_unit, m_header, m_footer;
	double m_left, m_right, m_top, m_bottom;
	char m_ftext[256], m_htext[256];
	PtedSetupPageDialog(int header, int footer, int left, int right,
		int top, int bottom, char *htext = NULL, char *ftext = NULL,
		CWnd* pParent = NULL);   
// Dialog Data
	//{{AFX_DATA(PtedReseqDialog)
	enum { IDD = IDD_PAGEDIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(PtedSetupPageDialog)
	protected:
	virtual BOOL OnInitDialog();
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(PtedSetupPageDialog)
	virtual void OnOK();
	afx_msg void OnHeader();
	afx_msg void Onfooter();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PTEDSETUPPAGEDIALOG_H__3F1F4163_7676_11D3_80BC_00C04F336F5E__INCLUDED_)
