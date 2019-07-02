/*********************************************************************
**    NAME         :  gtbluni.h
**       CONTAINS:
**       All DIGS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       gtbluni.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:23
*********************************************************************/
#ifndef GTBLUNIH

/***** TYPES Peculiar to this implementation ****/
#include "gobas.h"
#include "gtblopst.h"
#include "giloc.h"
#include "gistroke.h"
#include "gipick.h"
#include "gimisc.h"
#include "gsegop.h"

#define UG_lismaxsiz 16999
typedef union{			/* segment command */
	Segfloat gcreal[UG_lismaxsiz];
	Gint gcint[UG_lismaxsiz];
	Gchar gcchr[UG_lismaxsiz*sizeof(int)];
	Segfloat *gcptr[UG_lismaxsiz];
	Gint gclist[UG_lismaxsiz];
} UG_cmd;

typedef struct {
    int find;              /* 1=find the point eps dist from x,y */
    int found;					/* 1=found it. 0=didnt find it */
    Gfloat epsx,epsy;			/* pick half-aperture */
    Gfloat x,y;					/* ndc coords of point wanted */
	 Gfloat dist;				/* distance from thing found */
	 int pickid;				/* pickid of thing found */
	 int (*func)();			/* function to call when found something */
	 int pikmode;				/* 0=normal, 1=outside, 2=wholly in, 3=wholly out*/
	 int vis;					/* 0=visible or 1=invisible or 2=both */
} UG_findit;

typedef	union {				/* union of all types of events */
	Gloc locevent;
	Gstroke strokeevent;
	Gfloat valevent;
	Gint choiceevent;
	Gpicks pickevent;
	Gchar stringevent[100];
} UG_anyevent;

typedef struct {				/* input queue element */
	Gint time;					/* time stamp for this element */
	Gevent type;				/* ws, dev, and class of element */
	UG_anyevent event;			/* one of 6 classes of event */
} UG_queue;

typedef struct {				/* Xforms used in traversing which must be saved */
	UG_vtran3 vtran[UG_MAXNTRAN];		/* viewing transformations */
	Gtran cxform[UG_MAXNTRAN];			/* viewing matrices */
	Gtran cxinv[UG_MAXNTRAN];			/* inverses of matrices */
	Gint cxchg[UG_MAXNTRAN];			/* inverse flags */
	Gtran modxform;						/* global modelling xform */
	Gtran lmodxform;						/* local modelling xform */
	Gint lmodidnt;							/* local modelling identity flag */
} Gstate;
#define GTBLUNIH
#endif
