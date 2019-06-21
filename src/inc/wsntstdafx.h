/************************************************************************
**
**   FILE NAME: StdAfx.h
**
**	 CONTAINS: 
**		Defined library to use
**		include file for standard system include files,
**		or project specific include files that are used frequently, but
**      are changed infrequently
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntstdafx.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			08/31/15 , 09:32:27
c**********************************************************************
*/

#if !defined(AFX_STDAFX_H__3D6A2487_4608_11D4_81A6_00C04F336F5E__INCLUDED_)
#define AFX_STDAFX_H__3D6A2487_4608_11D4_81A6_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxdisp.h>        // MFC OLE automation classes
#include <afxdtctl.h>		// MFC support for Internet Explorer 4 Common Controls
#include "afxvisualmanager.h"
#include "afxvisualmanagerwindows.h"
#ifndef _AFX_NO_AFXCMN_SUPPORT
#include <afxcmn.h>			// MFC support for Windows Common Controls
#endif // _AFX_NO_AFXCMN_SUPPORT
#include <gdiplus.h>

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.
#define AUTO_SUBCLASS
#include <wsnttoolbar.h>

#endif // !defined(AFX_STDAFX_H__3D6A2487_4608_11D4_81A6_00C04F336F5E__INCLUDED_)
