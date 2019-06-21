/************************************************************************
c
c   FILE NAME: StdAfx.h
c
c	 CONTAINS: 
c		Defined library to use
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       StdAfx.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:07               
c
c**********************************************************************
*/
// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__9462807A_D8E7_11D3_812C_00C04F336F5E__INCLUDED_)
#define AFX_STDAFX_H__9462807A_D8E7_11D3_812C_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxdisp.h>        // MFC OLE automation classes
#ifndef _AFX_NO_AFXCMN_SUPPORT
#include <afxcmn.h>			// MFC support for Windows Common Controls
#endif // _AFX_NO_AFXCMN_SUPPORT
//added
// Windows Header Files:
#include <windows.h>

// C RunTime Header Files
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>

// Local Header Files

#include <commdlg.h>
#include <ocidl.h>
#include <olectl.h>
#include <crtdbg.h>


//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__9462807A_D8E7_11D3_812C_00C04F336F5E__INCLUDED_)
