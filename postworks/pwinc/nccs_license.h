/************************************************************************
c
c   FILE NAME: nccs_license.h
c
c	 CONTAINS: 
c		definitions of CNccs_licenseApp
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        nccs_license.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:40
c
c**********************************************************************
*/
#if !defined(AFX_NCCS_LICENSE_H__DD043765_E371_11D5_909D_00C04F336F5E__INCLUDED_)
#define AFX_NCCS_LICENSE_H__DD043765_E371_11D5_909D_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "licres.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CNccs_licenseApp:
// See nccs_license.cpp for the implementation of this class
//

class CNccs_licenseApp : public CWinApp
{
public:
	CNccs_licenseApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNccs_licenseApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CNccs_licenseApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCCS_LICENSE_H__DD043765_E371_11D5_909D_00C04F336F5E__INCLUDED_)
