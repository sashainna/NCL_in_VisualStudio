/*********************************************************************
**    NAME         :  gtblseg.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblseg.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:23
*********************************************************************/

#ifndef GKSTBLSEGH
#include "usysdef.h"
#include "glsi2.h"
#include "gtblws.h"
#include "gtbldef.h"

/********* SEGMENT STATE LIST ******/
typedef struct Gsss {			/* one per defined segment */
	Gseg segid;						/* segment id */
	/*struct Gsss *next;*/			/* point to next link in chain */
	/*Gint nowspost;*/				/* number of workstations to which posted */
	/*Gws wspost[UG_MAXOPWS];*/	/* array of workstations to which posted */
	UG_LSI seglist;				/* indexed list of segment commands */
	unsigned long xforms;		/* Lots of one bit flags... 
										1<<i == 1 if seg contains output primitives 
										drawn under ith normtran (0<=i<=UG_MAXNTRAN-1). 
			(UG_SEGINNTRAN)		1<<UG_MAXNTRAN == 1 if anything drawn under
										inherited normtran. 
			(UG_SEGMODXF)			1<<UG_MAXNTRAN+1 == 1 if modelling xform
										changed in this seg. 
			(UG_SEGINMODXF)		1<<UG_MAXNTRAN+2 == 1 if anything drawn 
										under inherited modelling xform.
			(UG_SEGNDCBOX)			1<<UG_MAXNTRAN+3 == 1 if ndc coords have 
										changed. (Ndc box is out of date)
			(UG_SEGVISLIST)		1<<UG_MAXNTRAN+4 == 1 if this segment has 
										been added to visible list */
/*	Gnrect ndcbox;*/					/* ndc coordinate bounding box for this seg */
	Gwrect3 wcbox;					/* World coordinate bounding box for this seg */
	long userdata[UG_UDATA_SIZE];	/* user data, accessed via gsegrud,gsegwud */
	UG_LSI *rassegpt;				/* pointer to clipped raster seg, or NULL */
	UG_segat segatts;				/* segment attributes */
/*	unsigned char ndcboxok;*/		/* 1=ndcbox is up to date */
	unsigned char wcboxok;		/* 1=ndcbox is up to date */
} UG_segstli;


/* Macros used to mask off xforms bits */
#define UG_SEGINNTRAN	(1<<UG_MAXNTRAN)		/* Seg inherits ntran */
#define UG_SEGMODXF		(1<<UG_MAXNTRAN+1)	/* Seg changes model xform */
#define UG_SEGINMODXF	(1<<UG_MAXNTRAN+2)	/* Seg inherits model xform */
#define UG_SEGNDCBOX		(1<<UG_MAXNTRAN+3)	/* Segs ndc box out of date */
#define UG_SEGVISLIST	(1<<UG_MAXNTRAN+4)	/* Seg checked for vis list */

/*#define UG_RESET_NDCBOX(segptr) {segptr->ndcbox.ll.x = 1.0e+10;\
		segptr->ndcbox.ll.y = 1.0e+10; segptr->ndcbox.ur.x = -1.0e+10;\
		segptr->ndcbox.ur.y = -1.0e+10; segptr->ndcboxok = 0;}*/

#define UG_RESET_WCBOX(segptr) {segptr->wcbox.llf.x = 1.0e+10;\
		segptr->wcbox.llf.y = 1.0e+10; segptr->wcbox.llf.z = 1.0e+10;\
		segptr->wcbox.urb.x = -1.0e+10; segptr->wcbox.urb.y = -1.0e+10;\
		segptr->wcbox.urb.z = -1.0e+10; segptr->wcboxok = 0;}

/* this won't work when marker size is implemented */
#define UG_MKRSIZ (UU_REAL) .02		/* size of marker to calc ndcbox */
#define UG_SEGHASH_SIZE 256
#define UG_SEGACC_MASK  0xFF
#define GKSTBLSEGH
#endif
