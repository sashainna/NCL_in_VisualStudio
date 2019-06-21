/************************************************************************
c
c   FILE NAME: mpost.h
c
c	 Description - Functions and struct declarations for
c		CMPostApp (MainApp)
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mpost.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:39 
c
c**********************************************************************
*/

#ifndef MPOST_H
#define MPOST_H
#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "mpostres.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////

class CMPostApp : public CWinApp
{
public:
	CMPostApp();
	CWnd *m_pageParent;
	int m_newpage;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMPostApp)
	public:
	virtual BOOL InitInstance();
	virtual int Run();
	//}}AFX_VIRTUAL
	DECLARE_MESSAGE_MAP()
// Implementation

};


/////////////////////////////////////////////////////////////////////////////
#endif
