
/*********************************************************************
**    NAME         :  mpocket.h
**
**       CONTAINS:
**				Pocket Window definitions
**
**    COPYRIGHT 2001 (c) NCCS, Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       mpocket.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:32
**
*********************************************************************/

#ifndef MPOCKET
#define MPOCKET

#undef EXT
#ifdef UM_MPGM
#define EXT
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

#define UM_MAX_POCKET 4
#define UM_POCKET_COLORS 64

char *um_get_pocket_window();

typedef enum
{
	UM_DRAWING_WINDOW,
	UM_IPV_WINDOW,
	UM_NCL_WINDOW,
	UM_GRAPHIC_WINDOW
} UM_pkwin_type;

typedef struct
{
	UU_REAL red;
	UU_REAL green;
	UU_REAL blue;
} UM_pkcolor_struct;

EXT UM_pkcolor_struct UM_pkcolors[UM_POCKET_COLORS];

#undef EXT
#endif
