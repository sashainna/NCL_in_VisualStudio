/*********************************************************************
**    NAME         :  glStdAfx.h
**       CONTAINS:
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       glStdAfx.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:20
*********************************************************************/
#if !defined(AFX_STDAFX_H__3F2E3033_B181_4FBC_8E80_06A80C883ABD__INCLUDED_)
#define AFX_STDAFX_H__3F2E3033_B181_4FBC_8E80_06A80C883ABD__INCLUDED_
#pragma once

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxdtctl.h>		// MFC support for Internet Explorer 4 Common Controls
#ifndef _AFX_NO_AFXCMN_SUPPORT
#include <afxcmn.h>			// MFC support for Windows Common Controls
#endif // _AFX_NO_AFXCMN_SUPPORT

#include <afxdlgs.h>

#include <math.h>
#include <gl/gl.h>
#include <gl/glu.h>

#include <vector>
using namespace std;

//{{AFX_INSERT_LOCATION}}
#endif
