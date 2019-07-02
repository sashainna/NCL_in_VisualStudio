/************************************************************************
**
**  FILE NAME: CIgesAttrDlg.h
**
**  CONTAINS: 
**    Header file for the class CIgesAttrDlg 
**
**    COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
**          All Rights Reserved
**   MODULE NAME AND RELEASE LEVEL
**      IgesAttrDlg.h , 25.1
**   DATE AND TIME OF LAST  MODIFICATION
**      04/29/15 , 15:06:05
**
************************************************************************/

#if !defined(AFX_IGESATTRDLG_H__819604AA_6E61_4804_B972_8C42CB81569D__INCLUDED_)
#define AFX_IGESATTRDLG_H__819604AA_6E61_4804_B972_8C42CB81569D__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// IgesAttrDlg.h : header file
//

#include "wsntclrbtn.h"
/////////////////////////////////////////////////////////////////////////////
// CIgesAttrDlg dialog

class CIgesAttrDlg : public CDialog
{
// Construction
public:
	CIgesAttrDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CIgesAttrDlg)
	enum { IDD = IDD_ATRIBUTES };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CIgesAttrDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	int m_color[7];
	CNCLColorButton m_button[7];
	void SetButColor(int indx, int color);
	void OnColorSel(int indx);

	// Generated message map functions
	//{{AFX_MSG(CIgesAttrDlg)
		// NOTE: the ClassWizard will add member functions here
//	virtual void OnCancel();
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	afx_msg void OnColorSel1();
	afx_msg void OnColorSel2();
	afx_msg void OnColorSel3();
	afx_msg void OnColorSel4();
	afx_msg void OnColorSel5();
	afx_msg void OnColorSel6();
	afx_msg void OnColorSel7();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_IGESATTRDLG_H__819604AA_6E61_4804_B972_8C42CB81569D__INCLUDED_)
