//
//     MODULE NAME AND RELEASE LEVEL
//       nccs_auth.h , 24.1
//    DATE AND TIME OF LAST  MODIFICATION
//       09/11/13 , 12:58:39
//
// nccs_auth.h : main header file for the NCCS_AUTH application
//

#if !defined(AFX_NCCS_AUTH_H__F63A3375_F587_11D5_909D_00C04F336F5E__INCLUDED_)
#define AFX_NCCS_AUTH_H__F63A3375_F587_11D5_909D_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "authres.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CNccs_authApp:
// See nccs_auth.cpp for the implementation of this class
//

class CNccs_authApp : public CWinApp
{
public:
	CNccs_authApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNccs_authApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CNccs_authApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCCS_AUTH_H__F63A3375_F587_11D5_909D_00C04F336F5E__INCLUDED_)
