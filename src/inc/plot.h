/************************************************************************
c
c   FILE NAME: plot.h
c
c	 CONTAINS: 
c		Header file for the class CPlotApp
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       plot.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:41               
c
c**********************************************************************
*/
// plot.h : main header file for the PLOT application
//

#if !defined(AFX_PLOT_H__EFC0F5DA_CEAD_11D3_8118_00C04F336F5E__INCLUDED_)
#define AFX_PLOT_H__EFC0F5DA_CEAD_11D3_8118_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "plotres.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CPlotApp:
// See plot.cpp for the implementation of this class
//

class CPlotApp : public CWinApp
{
public:
	CPlotApp();
	void OnPrintPS();
	void OnPrint(CDC* pDC, CPrintInfo*);
	BOOL DoPreparePrinting(CPrintInfo* pInfo);
	void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPlotApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CPlotApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PLOT_H__EFC0F5DA_CEAD_11D3_8118_00C04F336F5E__INCLUDED_)
