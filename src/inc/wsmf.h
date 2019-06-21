/********************************************************************* 
**  NAME:  wsmf.h
**
**      GKS workstation: Motif data definitions section.
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       wsmf.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:11
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
/*
......native WinNT can't include this file
......Yurong 10/20/00
*/
#if UU_COMP!=UU_WIN2K

#ifndef WSMFH
#define WSMFH

#include <Xm/Xm.h>
#include "gobas.h"
#include "dmotif.h"

#ifdef MFPGM
#define EXT
#else
#define EXT extern
#endif

typedef struct
{
	Widget prLabel;
	Widget prPlabel;
	Widget prompt;
	Widget erLabel;
} UWS_MFPROMPT;

typedef struct
{
	Widget area[20];
	int row,col;
} UWS_MFSTATUS;

typedef struct
{
	Widget parent;
	Widget graphic;
	Widget graphic_app;
	Widget prompt;
	Widget prompt_app;
	Widget status;
	Widget status_app;
	XtAppContext application;
	unsigned long overmask;
} UWS_MF;

typedef struct
{
	Widget menu_app[UDM_MAX_MENU];
} UWS_MFLAYOUT;

/*
.....Removed from Here, because Motif
.....and OpenGL both include this file
.....it will define twice. Move these
.....variable declared in wsmf*.c
*/
/* XVisualInfo vinf;
EXT UWS_MFPROMPT uw_mfprompt;
EXT UWS_MFSTATUS uw_mfstatus[5];
EXT UWS_MF uw_mf;
EXT UWS_MFLAYOUT uw_mflayout;
EXT int XBorder,YBorder; */

#undef EXT
#endif

#endif
