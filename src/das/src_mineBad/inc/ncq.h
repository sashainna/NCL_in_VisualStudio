/************************************************************************
**
**   FILE NAME: Ncq.h
**	  
**   CONTAINS:
**		interface of the CNcqApp class
**
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncq.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:38
**
************************************************************************/
// ncq.h : main header file for the NCQ application
//

#if !defined(AFX_NCQ_H__87770127_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
#define AFX_NCQ_H__87770127_16D5_11D7_9C47_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "Ncqres.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CNcqApp:
// See ncq.cpp for the implementation of this class
//

class CNcqApp : public CWinApp
{
public:
	CNcqApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNcqApp)
	public:
	virtual BOOL InitInstance();
	virtual int ExitInstance();
	//}}AFX_VIRTUAL

// Implementation
	//{{AFX_MSG(CNcqApp)
	afx_msg void OnAppAbout();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCQ_H__87770127_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
