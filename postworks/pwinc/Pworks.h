/************************************************************************
c
c   FILE NAME: Pworks.h
c
c	 CONTAINS: 
c		definitions of CPworksApp
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        Pworks.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:37 
c
c**********************************************************************
*/

#if !defined(AFX_PWORKS_H__E033F565_C888_11D5_908A_00C04F336F5E__INCLUDED_)
#define AFX_PWORKS_H__E033F565_C888_11D5_908A_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "Pworksres.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CPworksApp:
// See Pworks.cpp for the implementation of this class
//

class CPworksApp : public CWinApp
{
public:
	CPworksApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPworksApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CPworksApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PWORKS_H__E033F565_C888_11D5_908A_00C04F336F5E__INCLUDED_)
