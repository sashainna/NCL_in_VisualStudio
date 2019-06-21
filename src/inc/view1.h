/*********************************************************************
**
**    NAME         :  view1.h
**
**       CONTAINS:
**
**    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       view1.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:07
**
*********************************************************************/

#ifndef VIEW1H
#include "mdcoord.h"
#include "xenv1.h"

#ifdef VPGM
#define EXT
#else
#define EXT extern
#endif

typedef struct
{
	int shader;
	int bgcolor;
	int grad[2];
	int fcolor[4];
	UM_vector bgrgb;
	UM_vector grgb[2];
	UM_vector frgb[4];
	UM_vector colors[4];
	UX_pathname bgfile;
	int rotate;
	UU_LOGICAL stretch;
} UV_backgnd_struc;

EXT UV_backgnd_struc UV_background;

#undef EXT
#define VIEW1H

#endif

