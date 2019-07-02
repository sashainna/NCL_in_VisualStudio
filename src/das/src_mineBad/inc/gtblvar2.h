/*********************************************************************
**    NAME         :  gtblvar2.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblvar2.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:24
*********************************************************************/
#ifndef GTBLVAR2H

#include "gobas.h"
#include "gtblopst.h"

#ifdef GMAIN
UG_gksos ug_gksos={UG_GKCL};	/* operating state table */
Gchar ug_wstypes[1][6]={"ridge"};			/* list of workstation types */
Gchar *ug_wsptr[1]={&ug_wstypes[0][0]};		/* pointers to ws types*/
Gnrect3 ug_wsldc[1]={0.,0.,0.,1.,1.,1.};	/* list of requested LDC space */
#endif

#ifndef GMAIN
extern UG_gksos ug_gksos;
extern Gchar ug_wstypes[1][6];
extern Gchar *ug_wsptr[1];
#endif

#define GTBLVAR2H
#endif
