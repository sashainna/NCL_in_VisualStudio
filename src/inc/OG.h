/*********************************************************************
**    NAME         :  OG.h
**       CONTAINS:
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       OG.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:06
*********************************************************************/

#if !defined(AFX_OG_H__19707EED_D3DB_4E02_83E1_88298D4135F0__INCLUDED_)
#define AFX_OG_H__19707EED_D3DB_4E02_83E1_88298D4135F0__INCLUDED_
#pragma once

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "ogresource.h"

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
