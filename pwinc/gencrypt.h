/************************************************************************
c
c   FILE NAME: gencrypt.h
c
c   CONTAINS:
c     definitions of GENCRYPT
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        gencrypt.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:38
c
c**********************************************************************
*/
// gencrypt.h : main header file for the GENCRYPT application
//

#if !defined(AFX_GENCRYPT_H__190C3527_4664_11D6_90AE_00C04F336F5E__INCLUDED_)
#define AFX_GENCRYPT_H__190C3527_4664_11D6_90AE_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "cryptres.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CGencryptApp:
// See gencrypt.cpp for the implementation of this class
//

class CGencryptApp : public CWinApp
{
public:
	CGencryptApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CGencryptApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CGencryptApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_GENCRYPT_H__190C3527_4664_11D6_90AE_00C04F336F5E__INCLUDED_)
