/*********************************************************************
**    NAME         :  gdidd2.h -- typedefs for workstation args.
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gdidd2.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:16
*********************************************************************/

#ifndef GDIDD2H

#include "gtblws.h"
#include "gi1.h"
#include "gobas.h"
#include "gows.h"
#include "nclcmd.h"
#define UG_TEXT_MAX_SIZE 1024

/*  parameters and reply for workstation op-codes */
typedef struct {				/* reply to GDINITOP */
	Gstatus stat;
	UG_wdt *wdtptr;
} UG_rwsinit;

typedef struct {				/* prms to gdtstdevop */
	Gint op;
	Gws id;
} UG_tstdev;

/* typedef struct {*/				/* reply to gdtstdevop */
/*	Gstatus stat;
	Giclass inclass;
	Gint devnum;
} UG_rtstdev; */

typedef struct {				/* parms to gdwaitdevop */
	Gint op;
	Gws id;						/* workstation id */
	Giclass iclass;
	Gint devno;					/* device number */
} UG_reqdev;

typedef struct {				/* reply to request LOCATOR */
	Gstatus stat;
	Gdpoint loc;
	Gint tran;					/* transformation number */
} UG_reqloc;

typedef struct {				/* reply to request STROKE */
	Gstatus stat;
	Gnpoint *buf;
	Gint n_points;
	Gint tran;					/* transformation number */
} UG_reqstroke;

typedef struct {				/* reply to request string */
	Gstatus stat;
	Gint len;
	Gchar str[NCL_MAX_COMLINE];
} UG_rreqstr;

typedef struct {				/* parms to draw line */
	Gint op;
	Gws id;
	Gnpoint p1,p2;				/* endpoints of line */
} UG_line;

typedef struct {				/* parms to marker */
	Gint op;
	Gws id;
	Gnpoint p;
	Gint type;					/* marker type */
} UG_marker;

typedef struct {				/* parms to 3D cell array */
	Gint op;
	Gws id;
	Gnpoint np,nq,nr;
	Gint dx,dy;
	Gcolor *a;
}	UG_cell3;

typedef struct {				/* parms to 2D cell array */
	Gint op;
	Gws id;
	Gnrect rect;
	Gint dx,dy;
	Gcolor *a;
}	UG_cell;

typedef struct {				/* parms to 2D cell run array */
	Gint op;
	Gws id;
	Gnrect rect;
	Gint dx,dy;
	Gint *nrunrow;
	Gint *lens;
	Gcolor *a;
}	UG_cellrun;

typedef struct {				/* parms to 3D cell run array */
	Gint op;
	Gws id;
	Gnpoint np,nq,nr;
	Gint dx,dy;
	Gint *nrunrow;
	Gint *lens;
	Gcolor *a;
}	UG_cellrun3;

typedef struct {				/* parms to raster cell array */
	Gint op;
	Gws id;
	Gipoint np,nq,nr;
	Gint dx,dy;
	Gint *nrunrow;
	Gint *lens;
	Gcolor *a;
}	UG_cellrunras;

typedef struct {				/* parms to raster cell array */
	Gint op;
	Gws id;
	Gipoint np,nq,nr;
	Gint dx,dy;
	Gcolor *a;
}	UG_cellras;

typedef struct {				/* parms to init VALUATOR device */
	Gint op;
	Gws id;
	Giclass iclass;
	Gint devno;
	Gfloat init;				/* initial value */
	Gpet pet;					/* prompt and echo type */
	Gdrect *area;				/* echo area */
	Gvalrec *record;			/* valuator data record (low,high) */
} UG_initval;

typedef struct {				/* parms to init LOCATOR device */
	Gint op;
	Gws id;
	Giclass iclass;
	Gint devno;
	Gloc *init;					/* initial location */
	Gpet pet;					/* prompt and echo type */
	Gdrect *area;				/* echo area */
	Glocrec *record;			/* locator data record */
} UG_initloc;

typedef struct {				/* parms to init a device */
	Gint op;
	Gws id;
	Giclass iclass;
	Gidevno devno;
} UG_initdev;

typedef struct {				/* parms to init PICK device */
	Gint op;
	Gws id;
	Giclass iclass;
	Gidevno devno;
	Gpick *init;				/* initial status, segment no. and pickid */
	Gpet pet;					/* prompt and echo type */
	Gdrect *area;				/* echo area */
	Gpickrec *record;			/* pick record */
} UG_initpick;

typedef struct {				/* parms to init STRING device */
	Gint op;
	Gws id;
	Giclass iclass;
	Gidevno devno;
	Gchar *init;				/* initial string */
	Gpet pet;					/* prompt and echo type */
	Gdrect *area;				/* echo area */
	Gstringrec *record;		/* string record */
} UG_initstr;

typedef struct {				/* parms for gdtextop */
	Gint op;
	Gws id;
	Gwpoint3 pos;
	Gint slen;
	Gchar s[UG_TEXT_MAX_SIZE];
} UG_dtext;


#define GDIDD2H
#endif
