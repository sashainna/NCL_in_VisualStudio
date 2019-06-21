/************************************************************************
c
c   FILE NAME: postcomp.h
c
c	 CONTAINS: 
c		definitions of CPostcompApp
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        postcomp.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:40 
c
c**********************************************************************
*/


#if !defined(AFX_POSTCOMP_H__7B2C9A65_E200_11D5_909D_00C04F336F5E__INCLUDED_)
#define AFX_POSTCOMP_H__7B2C9A65_E200_11D5_909D_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "compres.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CPostcompApp:
// See postcomp.cpp for the implementation of this class
//

class CPostcompApp : public CWinApp
{
public:
	CPostcompApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPostcompApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CPostcompApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_POSTCOMP_H__7B2C9A65_E200_11D5_909D_00C04F336F5E__INCLUDED_)
