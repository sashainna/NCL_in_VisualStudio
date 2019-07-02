/*********************************************************************
**    NAME         :  gkstblvar6.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblvar6.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:24
*********************************************************************/
#ifndef GKSTBLVAR6H

#include "gobas.h"
#include "gtbluni.h"
#include "uiarray.h"

#ifdef GMAIN
UG_cmd ug_cmd;				/* storage for a segment command */
int ug_rratio;				/* sizeof(Gfloat)/sizeof(int). must be >=1 */
int ug_pratio;				/* sizeof(pointer)/sizeof(int). must be >=1 */
int ug_ndcset=0;			/* whether or not ndcspace has been set */
UG_findit ug_find;		/* if ug_find.find==UG_TRUE, dont draw anything but
                    	 		find the first coordinate of a line or text within
                 				ug_find.eps distance of (ug_find.x,ug_find.y), and 
									set ug_find.found=UG_TRUE. */
Gtran	ug_vxform[UG_MAXNTRAN],		/* viewing xform matrices (world->NDC) */
		ug_cxform[UG_MAXNTRAN],   	/* composite xform matrices (model->NDC) */
		ug_cxinv[UG_MAXNTRAN],  	/* inverse of cxform if ug_cxchg=UG_FALSE */
		ug_modxform,					/* global modelling xform matrix */
		ug_lmodxform;					/* local modelling xform matrix */
Gint ug_lmodidnt;						/* local modelling identity flag */
Gint ug_clip2flag;			/* 1=clip routines clip to ug_clip2rect[curvwindex] 
										instead of vport[curvwindex] */
Gnrect3 ug_clip2rect[UG_MAXNTRAN];	/* clip rect if ug_clip2flag==1 */
Gint ug_clip2null[UG_MAXNTRAN];		/* 1 iff ug_clip2rect[i] is null */
Gfloat ug_ndcmax[3];  			/* max ndc space limits */
Gint ug_wsopnum=0;				/* counts ws operations. Allows simulations not to
											sometimes be done twice for 2 workstations. */
UG_queue ug_inqueue[30];		/* input event queue */
Gint ug_inqlen=0;					/* input event queue length */
UG_queue ug_curevent;			/* current event report */
Gint ug_etime=0;					/* current event time */
Gfloat UG_farcerr=0.01;			/* font arc drawing error- % of radius*/
Gint ug_segerasecolor=0;		/* segment erase color, used by some drivers */
Gint ug_hidsurf=0;				/* 1=do hidden surface removal of fillareas */
unsigned long ug_ntranbox;		/* bit ntran 1 iff any ndcbox exist for ntran */
int ug_vislistok=0;				/* 1=ug_vislist is up to date */
UU_IARRAY ug_vislist;			/* list of segs to attempt picks on */
#endif

#ifndef GMAIN
extern UG_cmd ug_cmd;
extern int ug_rratio,ug_pratio,ug_ndcset;
extern UG_findit ug_find;
extern Gtran ug_vxform[UG_MAXNTRAN],
			ug_cxform[UG_MAXNTRAN],ug_cxinv[UG_MAXNTRAN],
			ug_modxform,ug_lmodxform;
extern Gint ug_lmodidnt;
extern Gint ug_clip2flag;
extern Gnrect3 ug_clip2rect[UG_MAXNTRAN];
extern Gint ug_clip2null[UG_MAXNTRAN];
extern Gfloat ug_ndcmax[3];
extern Gint ug_cxchg[UG_MAXNTRAN];
extern Gint ug_wsopnum;
extern UG_queue ug_inqueue[10];
extern Gint ug_inqlen;
extern UG_queue ug_curevent;
extern Gint ug_etime;
extern Gfloat UG_farcerr;
extern Gint ug_segerasecolor;	
extern Gint ug_hidsurf;
extern unsigned long ug_ntranbox;
extern int ug_vislistok;
extern UU_IARRAY ug_vislist;
#endif

#define GKSTBLVAR6H
#endif
