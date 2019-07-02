/************************************************************************
c
c   FILE NAME: ToolDispDlg.h
c
c	 CONTAINS: 
c		Header file all class CToolDispDlg
c
c    COPYRIGHT 2007 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       ToolDispDlg.h , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       12/08/15 , 13:59:51
c
c**********************************************************************
*/
#if !defined(AFX_TOOLDISPDLG_H__9C080ED3_8643_11DB_9CE7_00C04F336F5E__INCLUDED_)
#define AFX_TOOLDISPDLG_H__9C080ED3_8643_11DB_9CE7_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ToolDispDlg.h : header file
//
#include "ulist.h"

/////////////////////////////////////////////////////////////////////////////
// CToolDispDlg dialog

class CToolDispDlg : public CDialog
{
// Construction
public:
	CToolDispDlg(CWnd* pParent = NULL);   // standard constructor
	int m_type, m_created;
	CWnd *m_parent;
	void SetParent(CWnd* parent, int flag);
	int IsCreated() { return m_created; };
	void SetShade(int pos);
	void load_tool_symbols();
	void load_tool_prof();
	void adjust_data_field();

// Dialog Data
	//{{AFX_DATA(CToolDispDlg)
	enum { IDD = IDD_TOOL_DISPLAY };
	CString	m_class;
	double	m_value1;
	double	m_value2;
	double	m_value3;
	double	m_value4;
	CString		m_parm1;
	CString		m_parm2;
	CString		m_parm3;
	CString		m_parm4;
	CString	m_shade;
	CString	m_symbol;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CToolDispDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CToolDispDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	virtual void OnCancel();
	afx_msg void Onshadechange();
	afx_msg void OnSelchangeClass();
	afx_msg void OnSelchangeListProf();
	afx_msg void OnSymbolChanged();
	afx_msg void OnSelchangeListSymbols();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TOOLDISPDLG_H__9C080ED3_8643_11DB_9CE7_00C04F336F5E__INCLUDED_)
