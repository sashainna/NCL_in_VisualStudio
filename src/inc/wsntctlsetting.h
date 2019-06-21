/************************************************************************
**
**   FILE NAME: wsntcltsetting.h
**
**       Color Dialog ctl setting declarations
**              
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntctlsetting.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:16
**********************************************************************
*/
#ifndef __NCLCTLSETTING_H_
#define __NCLCTLSETTING_H_

#include "wsnthls.h"

namespace ColorPickCtrlSetting
{
	enum {TRANGLEHEIGHT		= 12};
	enum {TRANGLEWIDTH		= 10};
	enum {LUM_COLORBAR_WIDTH = 13};
	enum {BORDER_WIDTH		= 1};
	enum {PADDING			= TRANGLEHEIGHT/2+1 - BORDER_WIDTH*2};	// used by LumChooser and HueSatChooser
#define LUMCLASSNAME		"NCLLumChooserWnd"
#define HUESATCLASSNAME		"NCLHueSatChooserWnd"
}
#endif
