/************************************************************************
c
c   FILE NAME: ToolPromptDlg.h
c
c	 CONTAINS: 
c		Header file for the class ToolPromptDlg 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       ToolPromptDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:10               
c
c**********************************************************************
*/
#if !defined(AFX_TOOLPROMPTDLG_H__F3BA4564_DB16_11D3_8132_00C04F336F5E__INCLUDED_)
#define AFX_TOOLPROMPTDLG_H__F3BA4564_DB16_11D3_8132_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ToolPromptDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CToolPromptDlg dialog

class CToolPromptDlg : public CDialog
{
protected:

	CString m_msg, m_input, m_title;

// Construction
public:
	CToolPromptDlg(CWnd* pParent = NULL);   // standard constructor
	void SetDlgValue(char *title = NULL, char *msg=NULL, char*input=NULL);
	void GetInput(char *input, int maxnum = 80);

// Dialog Data
	//{{AFX_DATA(CToolPromptDlg)
	enum { IDD = IDD_PROMPT };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CToolPromptDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CToolPromptDlg)
		virtual BOOL OnInitDialog();
		virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TOOLPROMPTDLG_H__F3BA4564_DB16_11D3_8132_00C04F336F5E__INCLUDED_)
