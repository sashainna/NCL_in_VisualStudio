
/*********************************************************************
**    NAME         :  m7mathc.c
**       CONTAINS:
**			um_closept(pt1, pt2, cpt)
**			um_projvect(vect1,nvect,vect2)
**			um_circpts(cptr,spt,ept)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m7mathc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:09
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "modef.h"

/*********************************************************************
**    E_FUNCTION     : int um_closept(pt1, pt2, cpt)
**      Find which point is closer to cpt and return 1 if it is pt1,
**      and 2 if it is pt2.
**  PARAMETERS   
**      INPUT  :
**				pt1							first point
**      		pt2							second point
**      		cpt							check point
**      OUTPUT :  none
**  RETURNS      :  				1	 	if pt1 closer to cpt
**  									2	 	if pt2 closer to cpt
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int um_closept(pt1, pt2, cpt)
	UM_coord pt1;					/* first point */
	UM_coord pt2;					/* second point */
	UM_coord cpt;					/* point to decide on */
	{
	UM_length	dist1;			/* distance from pt1 to cpt */
	UM_length	dist2;			/* distance from pt2 to cpt */

/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
**		compute distances and use to decide which point is closer
*/
	uu_denter( UU_MTRC,(us,"um_closept(?,?,?)"));

	/* compute distances */
	dist1 = um_dcccc(pt1, cpt);
	dist2 = um_dcccc(pt2, cpt);

	uu_dexit;
	if (dist1 < dist2)
		return(1);
	else
		return(2);
	}

/*********************************************************************
**    E_FUNCTION     : um_projvect(vect1,nvect,vect2)
**      Project vect1 onto the plane with normal nvect, and return
**      the unit vector result vect2
**  PARAMETERS   
**      INPUT  :
**				vect1	:			vector to be projected
**				nvect :			normal of plane vect1 to be projected on
**      OUTPUT :
**				vect2	:			projected unit vector
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int um_projvect(vect1, nvect, vect2)
	UM_vector vect1;							/* original vector */
	UM_vector nvect;							/* normal vector */
	UM_vector vect2;							/* unit projected vector */
	{
	UU_REAL dotprod;							/* dot product */
	UM_vector tempvec;						/* temporary vector */

/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
**		compute dot product of vect1 and nvect, multiply dotprod times nvect
**		and subtract off vect1 to get projected vector, then unitize
*/
	uu_denter(UU_MTRC,(us,"um_projvect(?,?,?)"));

	dotprod = um_dot(vect1, nvect);
	um_vctmsc(nvect, dotprod, tempvec);
	um_vcmnvc(vect1, tempvec, tempvec);
	um_unitvc(tempvec, vect2) ;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_circpts(cptr,spt,ept)
**      Compute end points of an arc
**  PARAMETERS   
**      INPUT  :
**				cptr	:			pointer to circle record containing arc
**      OUTPUT :
**				spt	:			start point of arc
**      	   ept	:			end point of arc
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int um_circpts(cptr,spt,ept)
	struct UM_circle_rec *cptr;		/* UM_circle_rec template containing arc */
	UM_coord spt;							/* start point of circle arc */
	UM_coord ept;							/* end point of circle arc */
	{
	UM_vector tempvec;					/* temporary vector */
	UM_transf rotmat;						/* rotation matrix */

/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
**		Compute end points of arc
*/
	uu_denter(UU_MTRC,(us,"um_circpts(?,?,?)"));

	/* compute start point of arc */
	um_vctmsc(cptr->svec, cptr->radius, tempvec);
	um_vcplvc(tempvec, cptr->center, spt) ;
	
	/* compute end point of arc */
	um_rottf(cptr->nvec, cptr->dang, rotmat);
	um_cctmtf(cptr->svec, rotmat, tempvec);
	um_vctmsc(tempvec, cptr->radius, tempvec);
	um_vcplvc(tempvec, cptr->center, ept) ;
	
	uu_dexit;
	}
