
/*************************************************************************
**		NAME	:		udfrmdec.h
**			Contains:	external declaration of form struct ptr for all
**						form access routines.
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       udfrmdec.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:03
**************************************************************************/

#ifndef UDFRMDECH

#include "udforms.h"
#include "udfdata.h"

#ifdef __cplusplus 
#define EXT		extern "C"
#else
#define EXT		extern
#endif

EXT UD_FSTRUCT *UD_frm;
EXT UD_FDATA *UD_fdata;

#define UDFRMDECH
#endif
