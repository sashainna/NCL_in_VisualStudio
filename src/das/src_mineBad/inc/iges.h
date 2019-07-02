/************************************************************************
c
c   FILE NAME: iges.h
c
c	 CONTAINS: 
c		main header file for the IGES application
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       iges.h , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       01/20/17 , 09:46:04       
c
c**********************************************************************
*/
// iges.h : main header file for the IGES application
//

#if !defined(AFX_IGES_H__C82828D8_C207_11D3_810A_00C04F336F5E__INCLUDED_)
#define AFX_IGES_H__C82828D8_C207_11D3_810A_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "wsntres.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CIgesApp:
// See iges.cpp for the implementation of this class
//

class CIgesApp : public CWinApp
{
public:
	CIgesApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CIgesApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CIgesApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_IGES_H__C82828D8_C207_11D3_810A_00C04F336F5E__INCLUDED_)

