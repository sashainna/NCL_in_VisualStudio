/************************************************************************
c
c   FILE NAME: Pted.h
c
c	 CONTAINS: 
c		main header file for the PTED application
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        Pted.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:33
c
c**********************************************************************
*/

#if !defined(AFX_PTED_H__D28F8FBB_0F00_11D3_85DD_0000F8D8A403__INCLUDED_)
#define AFX_PTED_H__D28F8FBB_0F00_11D3_85DD_0000F8D8A403__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "pwenv.h"
#include "Ptedres.h"		// main symbols
#include "PtedBatch.h"

/////////////////////////////////////////////////////////////////////////////
// CPtedApp:
// See Pted.cpp for the implementation of this class
//

class CPtedApp : public CWinApp
{
private:
	void ParseCmdLine(char *comstr,int *narg);
public:
	CPtedApp();
	char* m_argv[50];
	char m_flist_name[UX_MAX_PATH];
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPtedApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CPtedApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

extern PtedBatch *m_pBatch;

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PTED_H__D28F8FBB_0F00_11D3_85DD_0000F8D8A403__INCLUDED_)
