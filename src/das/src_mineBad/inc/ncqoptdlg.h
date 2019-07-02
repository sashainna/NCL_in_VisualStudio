/************************************************************************
**
**   FILE NAME: NcqOptDlg.h
**	  
**   CONTAINS:
**		interface of the CMainFrame class
**
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqoptdlg.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:39
**
************************************************************************
*/
#if !defined(AFX_NCQOPTDLG_H__87770137_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
#define AFX_NCQOPTDLG_H__87770137_16D5_11D7_9C47_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// NcqOptDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// NcqOptDlg dialog

class NcqOptDlg : public CDialog
{
// Construction
public:
	NcqOptDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(NcqOptDlg)
	enum { IDD = IDD_OPT_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(NcqOptDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(NcqOptDlg)
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCQOPTDLG_H__87770137_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
