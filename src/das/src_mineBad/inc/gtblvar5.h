/*********************************************************************
**    NAME         :  gtblvar5.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblvar5.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:24
*********************************************************************/
#ifndef GTBLVAR5H

#include "gows.h"
#include "gtblws.h"
#include "gtblopst.h"

#ifdef GMAIN
/*
.....change z value near-far to -1, 1 instead of 0, 1
.....Yurong 3/24/99
*/
UG_vtran3 ug_defvtran={		/* default (identity) viewing xform */
				0.,0.,1.,1.,1.,-1.,	/* window */
				0.,0.,1.,1.,1.,-1.,	/* viewport */
				UG_PARALLEL,				/* viewport type */
				0.,0.,0.,				/* view ref point */
				0.,0.,1.,				/* view plane normal */
				0.,1.,0.,				/* view up vector */
				0.,0.,0.,				/* proj ref pt. */
				UG_CLIP,UG_CLIP,UG_CLIP,		/* window, front, back clipping */
				0};						/* input prio */

UG_segat ug_defsegat={		/* default segment attributes */
				UG_VISIBLE,UG_NORMAL,		/* visibility, hi-lite */
				1,									/* highest output priority */
				UG_DETECTABLE};				/*  detectability */
				
Gwstran3 ug_defwsxform={	/* default workstation window/viewport */
				0.,0.,1.,1.,1.,-1.,	/* window */
				0.,0.,1.,1.,1.,-1.};	/* viewport--changed by gopenws */
#endif

#ifndef GMAIN
extern UG_vtran3 ug_defvtran;
extern UG_segat ug_defsegat;
extern Gwstran3 ug_defwsxform;
#endif

#define GTBLVAR5H
#endif
