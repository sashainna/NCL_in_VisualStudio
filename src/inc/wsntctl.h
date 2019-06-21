/********************************************************************* 
**  NAME:  wsntctl.h
**
**			functions and datas declaration for CNCLApp class
**
**		CONTAINS: 
**			functions and datas declaration for CNCLApp class
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntctl.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:16      
*********************************************************************/

#if !defined(AFX_NCL_H__3D6A2485_4608_11D4_81A6_00C04F336F5E__INCLUDED_)
#define AFX_NCL_H__3D6A2485_4608_11D4_81A6_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'wsntstdafx.h' before including this file for PCH
#endif

#include "wsntres.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////

class CNCLApp : public CWinApp
{
public:
	CNCLApp();
	CString m_strMyClassName;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLApp)
	public:
	virtual BOOL InitInstance();
	virtual int Run();
	//}}AFX_VIRTUAL

// Implementation

	void OnNCLAppExit();
	//{{AFX_MSG(CNCLApp)
	afx_msg void OnAppAbout();
	afx_msg void OnNCLSafeExit();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCL_H__3D6A2485_4608_11D4_81A6_00C04F336F5E__INCLUDED_)
