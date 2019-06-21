/************************************************************************
c
c   FILE NAME: mslite.h
c
c	 CONTAINS: 
c		Header file class CMsliteApp used by MSLITE
c
c    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c         mslite.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c         04/29/15 , 15:06:33
c
c**********************************************************************
*/
// mslite.h : main header file for the MSLITE application
//

#if !defined(AFX_MSLITE_H__042C4F95_14B4_4357_A067_6E02C3945A46__INCLUDED_)
#define AFX_MSLITE_H__042C4F95_14B4_4357_A067_6E02C3945A46__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "wsntres.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CMsliteApp:
// See mslite.cpp for the implementation of this class
//

class CMsliteApp : public CWinApp
{
public:
	CMsliteApp();
	CString m_strMyClassName;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMsliteApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation
	//{{AFX_MSG(CMsliteApp)
	afx_msg void OnMSLSafeExit();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MSLITE_H__042C4F95_14B4_4357_A067_6E02C3945A46__INCLUDED_)
