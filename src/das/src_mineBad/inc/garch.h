/*********************************************************************
**    NAME         :  garch.h
**
**    CONTAINS: Variable definitions specific to digs archive routines.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       garch.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:16
*********************************************************************/
#ifndef GARCHH

#include "ualloc.h"
#include "gobas.h"
#include "gows.h"
#include "glsi2.h"

#define TOFCINC 100				/* increments of table of contents size */

#ifdef GARCHMAIN
#define EXT
#else
#define EXT extern
#endif

EXT UG_LSI ug_tofc;
EXT Gint ug_tofcmod;
EXT Gint ug_filelen;
EXT Gint ug_tofcsize;
EXT Gconres ug_conresflag;
EXT Gint UG_Archfid;
EXT Gint UG_NEWFILE;
EXT Gint UG_AROP;
extern UU_STORE *uu_toolstore;

typedef struct {					/* Table of contents entry for an archive */
			Gint id;					/* Changing this typdef changes all existing */
			Gint len;				/* Digs archives (icon files especially) */
			Gint offset;
} UG_Tofc;

#define GARCHH
#undef EXT
#endif
