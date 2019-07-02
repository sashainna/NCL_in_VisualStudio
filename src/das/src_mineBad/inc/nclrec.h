/********************************************************************* 
**  NAME:  nclrec.h
**
**			NCLREC main application class CNclrecApp
**			main header file for the NCLREC application
**			Defines the class behaviors for the application
**			define CNclrecApp class datas and functions
**	CONTAINS: 
**			functions and datas declaration for  class CNclrecApp
**    COPYRIGHT 2005 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			nclrec.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:06:36
*********************************************************************/
#if !defined(AFX_NCLREC_H__5D05A027_8B4E_11D9_A320_0800690F48C1__INCLUDED_)
#define AFX_NCLREC_H__5D05A027_8B4E_11D9_A320_0800690F48C1__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "nclrecres.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CNclrecApp:
// See nclrec.cpp for the implementation of this class
//

#define MAX_PATH_LEN 256
class CNclrecApp : public CWinApp
{
public:
	CNclrecApp();
	char m_inputfile[MAX_PATH_LEN], m_outputfile[MAX_PATH_LEN];

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNclrecApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CNclrecApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCLREC_H__5D05A027_8B4E_11D9_A320_0800690F48C1__INCLUDED_)
