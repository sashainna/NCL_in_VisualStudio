/************************************************************************
c
c   FILE NAME: Toollib.h
c
c	 CONTAINS: 
c		Header file all class CToolibApp
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       toolib.h , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:15:28
c
c**********************************************************************
*/
#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "toolibres.h"       // main symbols

// CToolibApp:
// See toolib.cpp for the implementation of this class
//

class CToolibApp : public CWinAppEx
{
public:
	CToolibApp();


// Overrides
public:
	virtual BOOL InitInstance();

// Implementation

	afx_msg void OnAppAbout();
	DECLARE_MESSAGE_MAP()
};

extern CToolibApp theApp;
