/************************************************************************
c
c   FILE NAME: GLtest.h
c
c	 CONTAINS: 
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        GLtest.h , 25.1
c     DATE AND TIME OF LAST  MODIFICATION
c        04/29/15 , 15:06:04
c
c**********************************************************************
*/
#if !defined TESTGL_H
#define TESTGL_H
#pragma once

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "glresource.h"

class COGApp : public CWinApp
{
public:
	COGApp();
	//{{AFX_VIRTUAL(COGApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

	//{{AFX_MSG(COGApp)
	afx_msg void OnAppAbout();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
//{{AFX_INSERT_LOCATION}}
#endif
