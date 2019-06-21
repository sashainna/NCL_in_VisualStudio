/********************************************************************* 
**  NAME:  wsntdir.cpp
**
**			Toolib Search function class.
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			ToolFindDlg.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			10/12/15 , 17:14:00
*********************************************************************/
#if !defined(AFX_TOOLFINDDLG_H__A61C7F93_CA1F_11D5_908A_00C04F336F5E__INCLUDED_)
#define AFX_TOOLFINDDLG_H__A61C7F93_CA1F_11D5_908A_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ToolFindDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CToolFindDlg dialog

class CToolFindDlg : public CDialog
{
// Construction
public:
	CToolFindDlg(CWnd* pParent = NULL, CString defstr = _T(""));   // standard constructor

// Dialog Data
	int m_find_pos;
	CString m_find_str;
	CWnd *m_parent;
	//{{AFX_DATA(CToolFindDlg)
	enum { IDD = IDD_FIND_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CToolFindDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CToolFindDlg)
	afx_msg void OnToolFind();
	virtual void OnCancel();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TOOLFINDDLG_H__A61C7F93_CA1F_11D5_908A_00C04F336F5E__INCLUDED_)
