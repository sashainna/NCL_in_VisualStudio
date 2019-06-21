/*********************************************************************
**    NAME         :  patsupp.h
**       PURPOSE: Include support structures for attributes and transformations
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tptsupp.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:57
*********************************************************************/

#ifndef TIGSUPPH


#include "mattr.h"

extern UU_REAL UM_idmat[4][3];

extern struct UM_crvattr_rec UM_crvattr;
extern struct UM_srfattr_rec UM_srfattr;
extern struct UM_solattr_rec UM_solattr;
extern struct UM_attrdata_rec UM_attr;

#define TIGSUPPH
#endif
