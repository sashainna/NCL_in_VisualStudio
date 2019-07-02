/*********************************************************************
**
**    NAME         :  driver.h
**
**       CONTAINS:
**       	mechanism to include and exclude GKS drivers 
**
** NOTE: module interface/zappinit.c must be recompiled to introduce
**       changes to this include file.
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       driver.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:14
*********************************************************************/

#ifndef DRIVERH
#include "usysdef.h"

#ifdef DPGM
#define EXT 
#else
#define EXT extern
#endif
/*
...Color map setting
...added by Yurong
*/
EXT int UD_overlay;  
EXT int UD_cmap_changed;
	typedef struct
	{
		char name[8];				/* name of driver */
		int (*ddentry)();			/* entry point of activation routine */
	} UD_DRIVER;


/*	-- GKS driver enabling table -- */

	extern int ud_ddopngl();
#define UD_GKSTLEN 1

#ifdef DPGM 
	int UD_gkstlen = UD_GKSTLEN;			/* length of choice table */ 
	UD_DRIVER UD_gkstable[UD_GKSTLEN] =
{
		{"openGL", 	ud_ddopngl}
}; 
#else
	EXT int UD_gkstlen;						/* length of choice table */ 
	EXT UD_DRIVER UD_gkstable[UD_GKSTLEN];	/* driver table */
#endif 

#undef EXT
#define DRIVERH

#endif

