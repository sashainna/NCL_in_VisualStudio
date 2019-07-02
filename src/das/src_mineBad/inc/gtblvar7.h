/*********************************************************************
**    NAME         :  gtblvar7.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblvar7.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:24
*********************************************************************/
#ifndef GTBLVAR7H

#include "gobas.h"

#ifdef GMAIN
#define UG_FALSE 0
/* whether cxinv is up to date */
Gint ug_cxchg[UG_MAXNTRAN]={UG_FALSE,UG_FALSE,UG_FALSE,UG_FALSE,UG_FALSE};	
Gint ug_ndcseg=0;			/* number of open workstations needing NDC boxes*/
#endif

#ifndef GMAIN
extern Gint ug_ndcseg;
#endif

#define GTBLVAR7H
#endif
