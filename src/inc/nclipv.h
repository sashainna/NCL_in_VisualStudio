/************************************************************************
c
c   FILE NAME: nclipv.h
c
c	 CONTAINS: 
c		Header file class CNclipvApp used by NCLIPV
c
c    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c         nclipv.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c         04/29/15 , 15:06:35
c**********************************************************************
*/
// nclipv.h : main header file for the NCLIPV application
//

#if !defined(AFX_NCLIPV_H__042C4F95_14B4_4357_A067_6E02C3945A46__INCLUDED_)
#define AFX_NCLIPV_H__042C4F95_14B4_4357_A067_6E02C3945A46__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "wsntres.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CNclipvApp:
// See nclipv.cpp for the implementation of this class
//

class CNclipvApp : public CWinApp
{
public:
	CNclipvApp();
	CString m_strMyClassName;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNclipvApp)
	public:
	virtual BOOL InitInstance();
	virtual int Run();
	//}}AFX_VIRTUAL

// Implementation
	//{{AFX_MSG(CNclipvApp)
	afx_msg void OnIPVSafeExit();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCLIPV_H__042C4F95_14B4_4357_A067_6E02C3945A46__INCLUDED_)
