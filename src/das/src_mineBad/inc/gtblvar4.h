/*********************************************************************
**    NAME         :  gtblvar4.h
**       CONTAINS:
**       Gks state list.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblvar4.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:24
*********************************************************************/
#ifndef GKSTBLVAR4H

/* Necessary include files */
#include "gtblst.h"

#ifdef GMAIN
UG_gksstli ug_gksstli;					/* gks state list */
#endif

#ifndef GMAIN
extern UG_gksstli ug_gksstli;
#endif

#define GKSTBLVAR4H
#endif
