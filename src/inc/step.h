/************************************************************************
c
c   FILE NAME: step.h
c
c	 CONTAINS: 
c		main header file for the STEP application
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       step.h , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       01/20/17 , 09:48:47
c
c**********************************************************************
*/
// step.h : main header file for the STEP application
//

#if !defined(AFX_STEP_H__C82828D8_C207_11D3_810A_00C04F336F5E__INCLUDED_)
#define AFX_STEP_H__C82828D8_C207_11D3_810A_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "wsntres.h"

/////////////////////////////////////////////////////////////////////////////
// CStepApp:
// See step.cpp for the implementation of this class
//

class CStepApp : public CWinApp
{
public:
	CStepApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CStepApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CStepApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STEP_H__C82828D8_C207_11D3_810A_00C04F336F5E__INCLUDED_)

