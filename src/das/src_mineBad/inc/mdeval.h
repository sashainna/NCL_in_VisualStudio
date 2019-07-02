
/*********************************************************************
**    NAME         :  mdeval.h
**       CONTAINS: curve/surface evaluator definitions
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       mdeval.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:29
*********************************************************************/

#ifndef UM_MDEVAL

#include "usysdef.h"
#include "mdcoord.h"
#include "mdgenent.h"

#define UM_MAXORDER 99
#define UM_MAXARROWPTS 30  /*this is the max nbr of arrows that can be put */
								  /*on a crv to indicate its parameterization     */

/***************************************************************************
*
*	Definition of a rotation transformation matrix with each element named.
*
***************************************************************************/

struct  UM_rotmatrix {
    UU_REAL mat00;
    UU_REAL mat01;
    UU_REAL mat02;
    UU_REAL mat10;
    UU_REAL mat11;
    UU_REAL mat12;
    UU_REAL mat20;
    UU_REAL mat21;
    UU_REAL mat22;
    UU_REAL mat30;
    UU_REAL mat31;
    UU_REAL mat32;
    };

/***************************************************************************
*
*		The following define the amount of data which the curve/surface
*		evaluator will return.
*
***************************************************************************/

#define  UM_POINT       -1		/* evaluators: calculate point */
#define  UM_NORM         0    /* evaluators: calculate normal*/
#define  UM_FRSTDERIV    1		/* evaluators: calculate first derivative */
#define  UM_SECDERIV     2		/* evaluators: calculate second derivative */
#define  UM_CURVATURE	 3		/* evaluators: calculate curvature */
#define  UM_ALL          4    /* evaluators: set or interrogate all fields */

/***************************************************************************
*
*		The following define the status values which are returned by 
*		the evaluator related functions to indicate the reliability of
*		the returned data.
*
***************************************************************************/

#define  UM_VALID        0     /* indicates all fields returned are valid */
#define  UM_BADFIELDS    1     /* some fields are invalid */
#define  UM_BADRECORDS   2     /* an entire record is suspect */
#define  UM_INVALID      3     /* entire output is bad */

/**********************************************************************
*
*                CURVE EVALUATOR DATA TYPES
*
**********************************************************************/


struct  UM_evcrvout {
    int  status;						/* status word to determine which fields
												are valid; note, currently,  this should
												be either UM_VALID or UM_INVALID; however,
												in the future this will probably consist of
												a series of bits indicating the validity
												each field in the evaluation record. */
    UM_coord cp;						/* output point on the curve */
    UM_vector dcdu;					/* first derivative at cp along curve */
    UM_vector d2cdu2;				/* second derivative at cp along curve */
    UU_REAL curv;						/* curvature at cp */
	 UM_transf subcrv_tfmat;		/* curve transformation; used for storing the 
												transformation to orient a subcurve of the 
												composite curve associated with a curve
												evaluator record. */
	 UU_LOGICAL firsteval;			/* TRUE iff we are first starting to eval
												a curve; this is initialized by
												um_setup_evaluator */
	 struct UM_crvdatabag subcrv;	/* used in evaluating composite curves;
												stores a subcurve record of the composite*/	 
    };


/**********************************************************************
*
*                SURFACE EVALUATOR DATA TYPES
*
**********************************************************************/

#define  UM_REVSRF_UTRC 1
#define  UM_REVSRF_VTRC 2
#define  UM_TABCYL_UTRC 3
#define  UM_TABCYL_VTRC 4
#define  UM_RULSRF_UTRC 5
#define  UM_RULSRF_VTRC 6

#define UM_MAX_NBR_CRVS_N_SRF 4		/* maximum number of curves used
													to define a surface */

  struct  UM_evsrfout {
	  int status;							/* status word to determine which fields
													are valid; see comment on curve
													evaluation */
     UM_coord sp;							/* surface point */ 
     UM_vector snorm;					/* surface normal at pt "sp"*/
     UM_vector dsdu;						/* 1st deriv in "u" direction */
     UM_vector dsdv;						/* 1st deriv in "v" direction */
     UM_vector d2sdu2;					/* 2nd deriv in "u" direction */
     UM_vector d2sdv2;					/* 2nd deriv in "v" direction */
     UU_REAL ucurv;						/* curvature in "u" direction */
     UU_REAL vcurv;						/* curvature in "v" direction */
	  UU_LOGICAL firsteval;				/* TRUE iff we are just starting to
													evaluate the surface; initialized
													by call by um_setup_evaluator */
   };

#define UM_MDEVAL
#endif
