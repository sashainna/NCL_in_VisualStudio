/*********************************************************************
**    NAME         :  mattr
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tzmattr.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:00
*********************************************************************/

#ifndef UM_ATTR

#include "usysdef.h"
#include "tzattrdd.h"


/*****************************************************************************
*
*                      Current Global Display Attributes
*
*****************************************************************************/

#ifdef  UM_MPGM
#define EXT
#else
#define EXT extern
#endif


struct UM_ptattr_rec {
	int markertype;					/* type of marker for DIGGS */
	UU_LOGICAL snap_node;			/* UU_TRUE iff new point is to be snap node */
	};

struct UM_crvattr_rec {
	UU_REAL relcirerr;				/* rel. circle error as % of radius */
	int maxpts;							/* maximum # of points per curve */
	};

struct UM_srfattr_rec {
	int numupaths;						/* number of u paths */
	int numvpaths;						/* number of v paths */
	int ptsperucrv;					/* number of points per u paths */
	int ptspervcrv;					/* number of points per v paths */
	int maxpaths;						/* maximum number of paths */
	int material;						/* index of material properties */
	};

struct UM_solattr_rec {
	UU_REAL pitch;						/* hatching pitch */
	};

EXT struct UM_ptattr_rec  UM_ptattr;		/* current point attributes */
EXT struct UM_crvattr_rec UM_crvattr;		/* current curve attributes */
EXT struct UM_srfattr_rec UM_srfattr;		/* current surface attributes */
EXT struct UM_solattr_rec UM_solattr;		/* current solid attributes */


#include "go.h"

/*--------------------------------------------------------------------
** Modelling text attribute external variables.  
** Text menu routines for text attributes will change these variables.
** When a text entity is created, it's attribute bundle is filled in
** from these external variables.
**------------------------------------------------------------------*/
struct UM_textattrs {             /* current text attributes */
		char     fontname[16];        /* font name */
		Gtxfont  font;                /* font index */
		Gtxprec  prec;                /* text precision */
		Gchrexp  expn;                 /* text character expansion */
		Gchrsp   spacing;             /* text character spacing */
		Gchrht   height;              /* text character height */
		UU_REAL  angle;               /* char rotate angle */
		Gtxpath  path;                /* text path */
		Gtxalign align;               /* text alignment - hor and ver */
		int      color;					/* text color */
		char     *precv[3];           /* precision values */
		char     *pathv[4];           /* path values */
		char     *halignv[4];          /* horiz alignment values */
		char     *valignv[5];          /* vert alignment values */
    };
#ifdef UM_MTEXT
struct UM_textattrs UM_textattr;
#else
extern struct UM_textattrs UM_textattr;
#endif

#undef EXT

#define UM_ATTR
#endif
