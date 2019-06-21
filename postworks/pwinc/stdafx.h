/************************************************************************
c
c   FILE NAME: stdafx.h
c
c	 Description - include file for standard system include files,
c		or project specific include files that are used frequently, but
c		are changed infrequently
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        stdafx.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:41
c
c**********************************************************************
*/

#if !defined(AFX_STDAFX_H__1E65F44C_9826_11D1_B7A5_00A0249C7E5A__INCLUDED_)
#define AFX_STDAFX_H__1E65F44C_9826_11D1_B7A5_00A0249C7E5A__INCLUDED_

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


//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately 
//before the previous line.

#endif // !defined(AFX_STDAFX_H__1E65F44C_9826_11D1_B7A5_00A0249C7E5A__INCLUDED_)
