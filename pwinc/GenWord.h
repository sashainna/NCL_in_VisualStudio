/************************************************************************
c
c   FILE NAME: GenWord.h
c
c	 CONTAINS: 
c		definitions of CGenWordApp
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        GenWord.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:26 
c
c**********************************************************************
*/

#if !defined(AFX_GENWORD_H__478DE917_E2B0_11D5_909D_00C04F336F5E__INCLUDED_)
#define AFX_GENWORD_H__478DE917_E2B0_11D5_909D_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'pwstdafx.h' before including this file for PCH
#endif

#include "wordres.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CGenWordApp:
// See GenWord.cpp for the implementation of this class
//

class CGenWordApp : public CWinApp
{
public:
	CGenWordApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CGenWordApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CGenWordApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_GENWORD_H__478DE917_E2B0_11D5_909D_00C04F336F5E__INCLUDED_)
