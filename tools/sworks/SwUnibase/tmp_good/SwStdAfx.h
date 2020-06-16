// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently,
// but are changed infrequently

#pragma once

#ifndef STRICT
#define STRICT
#endif

// Modify the following defines if you have to target a platform prior to the ones specified below.
// Refer to MSDN for the latest info on corresponding values for different platforms.
#ifndef WINVER				// Allow use of features specific to Windows XP or later.
#define WINVER 0x0501		// Change this to the appropriate value to target other versions of Windows.
#endif

#ifndef _WIN32_WINNT		// Allow use of features specific to Windows XP or later.                   
#define _WIN32_WINNT 0x0501	// Change this to the appropriate value to target other versions of Windows.
#endif						

#ifndef _WIN32_WINDOWS		// Allow use of features specific to Windows 98 or later.
#define _WIN32_WINDOWS 0x0410 // Change this to the appropriate value to target Windows Me or later.
#endif

#ifndef _WIN32_IE			// Allow use of features specific to IE 6.0 or later.
#define _WIN32_IE 0x0600	// Change this to the appropriate value to target other versions of IE.
#endif

#define _ATL_APARTMENT_THREADED
#define _ATL_NO_AUTOMATIC_NAMESPACE

#define _ATL_CSTRING_EXPLICIT_CONSTRUCTORS	// some CString constructors will be explicit


#include <afxwin.h>
//# Unicode ifndef _AFX_NO_OLE_SUPPORT
#include <afxdisp.h>        // MFC Automation classes
// Unicode #endif // _AFX_NO_OLE_SUPPORT

#include "Swresource.h"
#include <atlbase.h>
#include <atlcom.h>
#include <atlwin.h>
#include <atltypes.h>
#include <atlctl.h>
#include <atlhost.h>

using namespace ATL;
#import "swpublished.tlb" raw_interfaces_only, raw_native_types, named_guids, auto_search
#import "sldworks.tlb" raw_interfaces_only, raw_native_types, named_guids, auto_search, rename("PropertySheet", "ShowPropertySheet"),rename("GetOpenFileName", "SWGetOpenFileName")
#import "swconst.tlb"

#define ID_SLDWORKS_TLB_MAJOR	14
#define ID_SLDWORKS_TLB_MINOR	0

#define ID_SWPUBLISHED_TLB_MAJOR	1
#define ID_SWPUBLISHED_TLB_MINOR	0

using namespace SldWorks;
using namespace SWPublished;
using namespace SwConst;

typedef IBody2 *LPBODY2;
typedef IConfiguration *LPCONFIGURATION;
typedef IComponent2 *LPCOMPONENT2;
typedef ICurve *LPCURVE;
typedef IEnumBodies2 *LPENUMBODIES2;
typedef IEnumFaces2 *LPENUMFACES2;
typedef IEnumSketchSegments *LPENUMSKETCHSEGMENTS;
typedef IFace2 *LPFACE2;
typedef IFeature *LPFEATURE;
typedef IMathTransform *LPMATHTRANSFORM;
typedef IModelDoc *LPMODELDOC;
typedef IModelDoc2 *LPMODELDOC2;
typedef IPartDoc *LPPARTDOC;
typedef ISketch *LPSKETCH;
typedef ISketchSegment *LPSKETCHSEGMENT;
typedef ISldWorks *LPSLDWORKS;
typedef ISurface *LPSURFACE;
typedef IUnknown *LPUNKNOWN;
