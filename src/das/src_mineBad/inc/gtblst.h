/*********************************************************************
**    NAME         :  gkstblst.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblst.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:23
*********************************************************************/

#ifndef GKSTBLSTH

/* Necessary include files */
#include "gobas.h"
#include "gtblws.h"
#include "gtblopst.h"

/* define max no. of viewing (normalization) xforms */
/****** GKS STATE LIST *******/
typedef struct {
	Gint noldcdef;				/* number of pending LDC definition */
	Gint nowsopen;				/* number of open workstations */
	UG_wssl wsopen[UG_MAXOPWS];	/* array of open workstations (state lists)*/
									/* Gws is index into this array */
	Gint nowsact;				/* number active workstations (local addition) */
	UG_prat3 curprats;		/* current primitive attributes */
	UG_rendatt rendats;		/* current rendering attributes */
	Gtran curgmodtran;		/* current global modelling transformation */
	Gtran curlmodtran;		/* current local modeling transformation */
	Gindex curvwindex;		/* current view index (0..n) */
	Gint novtran;				/* number of viewing transformations */
	UG_vtran3 vtran[UG_MAXNTRAN];	/* array of up to UG_MAXNTRAN viewing xforms:
										view index, view xform matrix, window, 
										viewport, view type, view ref point,
										view plane normal, view up vector,
										window, back, front clipping indicator,
										shield prio, shield indicator, input prio */
	Gseg opnseg;				/* id of open segment */
	UG_elptr curelptr;			/* current element pointer */
	Gint nseguse;				/* number of segment names in use */
	Gseg *seguse;				/* pointer to set of segment ids in use */
	Gint nsegstli;				/* no of segment state lists */
} UG_gksstli;
#define GKSTBLSTH
#endif
