/*********************************************************************
**    NAME         :  mattr
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       mattr.h , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:28
*********************************************************************/

#ifndef UM_ATTR

#include "usysdef.h"
#include "mattrddl.h"


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
	int edge_disp;						/* display edges separately iff 1 */
	int edge_color;					/* color to display egdes,
											   used only when edge_disp == 1 */
	int shaded;							/* shaded iff 1 */
	int lucency;						/* translucency parameter */
	};

struct UM_solattr_rec {
	UU_REAL pitch;						/* hatching pitch */
	};

EXT struct UM_ptattr_rec  UM_ptattr;		/* current point attributes */
EXT struct UM_crvattr_rec UM_crvattr;		/* current curve attributes */
EXT struct UM_srfattr_rec UM_srfattr;		/* current surface attributes */
EXT struct UM_solattr_rec UM_solattr;		/* current solid attributes */


#include "go.h" 

#undef EXT

#define UM_ATTR
#endif
