/*********************************************************************
**    NAME         :  gkstblvar1.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblvar1.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:24
*********************************************************************/
#ifndef GTBLVAR1H

#include "uhash.h"
#ifdef GMAIN
UU_HASH_TBL ug_seghash = {0,UU_NULL,0,UU_NULL}; /* hash table for segments */
#endif

#ifndef GMAIN
extern UU_HASH_TBL ug_seghash;
#endif

#define GTBLVAR1H
#endif
