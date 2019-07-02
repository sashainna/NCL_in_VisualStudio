/********************************************************************* 
**  NAME:  wsnttoolbar.h
**
**			Header file for automatically use new classes
**	CONTAINS: 
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttoolbar.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:22      
*********************************************************************/
#ifndef WSNTTOOLBAR__H
#define WSNTTOOLBAR__H

#ifndef __AFXTEMPL_H__
#include <afxtempl.h>
#endif

#ifndef TBSTYLE_TRANSPARENT
#define TBSTYLE_TRANSPARENT     0x8000 
#endif

#include <wsncltoolbar.h>
#include <wsntdlgbar.h>
#include <wsnclframe.h>

#ifdef AUTO_SUBCLASS
// Automatically use new classes
#define CFrameWnd CNCLFrameWnd
#define CToolBar CNCLToolBar
//#define CDialogBar CNCLDialogBar
#endif

#endif
