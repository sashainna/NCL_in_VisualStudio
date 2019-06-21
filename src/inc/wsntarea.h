/********************************************************************* 
**  NAME:  wsntarea.h
**
**			interface of CAreaEditDialog class functions
**
**	CONTAINS: CAreaEditDialog class functions and structure
**				declarations
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntarea.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:14      
*********************************************************************/

#ifndef AREAEDITDIALOG_H
#define AREAEDITDIALOG_H
#include "wsntres.h"

/////////////////////////////////////////////////////////////////////////////
// COptFileDialog dialog

class CAreaEditDialog : public CDialog
{

public:
	CAreaEditDialog();

	// Dialog Data
	//{{AFX_DATA(CAreaEditDialog)
	enum { IDD = IDD_MENUAREA };
	//}}AFX_DATA

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAreaEditDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

protected:
	int m_rows, m_cols, m_dir, m_width, m_height;
	CString m_areaname;
	//{{AFX_MSG(CAreaEditDialog)
	afx_msg void OnAdd();
	afx_msg void OnDelete();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif 
