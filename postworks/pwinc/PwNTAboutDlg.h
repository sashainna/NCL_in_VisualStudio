/********************************************************************* 
**  NAME:  PwNTAboutDlg.h
**
**			Defines the class behaviors 
**				for the About Dialog
**		CONTAINS: 
**			define CAboutDlg dialog
**
**    COPYRIGHT 2002 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			PwNTAboutDlg.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**			09/11/13 , 12:58:37
*********************************************************************/

#ifndef CPWABOUTDLG_H
#define CPWABOUTDLG_H

#include "pwwindef.h"

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
#endif
