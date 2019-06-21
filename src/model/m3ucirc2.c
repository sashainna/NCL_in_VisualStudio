
/*********************************************************************
**    NAME         :  m3ucirc2
**       CONTAINS: user interface routines for arc creation
**			umu_c3_arc2pt()
**			umu_c3_arcapt()
**			umu_c3_arcaptn()
**			umu_c3_arccnaarb()
**			umu_c3_arccraa()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ucirc2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:57
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdcpln.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"

/*********************************************************************
**    E_FUNCTION     : umu_c3_arc2pt()
**			Create an arc from the center and two  points input by the user.
**			The center and first point determine the radius of the circle.
**			If the two points are identical, then a circle is defined
**			by projecting the first point onto the construction plane.
**			Otherwise, an arc is defined which goes from the first point
**			to the second point in a counter-clockwise direction relative
**			to the current viewing parameters.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_arc2pt()
	{
/**	UM_coord	pt[2];	**/				/* the points entered through das:
												point 0 is on circle and determines radius
												point 1 determines angle */
    UD_NDCLOCREC pt1,pt2;

	struct	UM_circle_rec	cptr;		/* circle  entity */
	int		numint;					/* the number of interactions input
												through das */
/**	UM_coord center; **/					/* center */
    UD_NDCLOCREC center;

	int	dir;
	int	status;

	uu_denter( UU_MTRC,(us,"umu_c3_arc2pt()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*arc center*/UM_MODEL, 22, &center, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		ud_ldas(UD_DASCART, /*first endpoint of arc*/UM_MODEL, 23, &pt1, 
			 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASCART, /*second endpoint of arc*/UM_MODEL, 24, &pt2, 
				 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		status = um_c3_arcc2p(&center, &pt1, &pt2, &cptr);
		if (status == 0)
			{
			um_create_geom(&cptr, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&cptr);
			}
repeat:;
		}
done:;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_c3_arcapt()
**			Prompt the user for the data to create an arc from a
**			center point, a point lying on the circumference of the
**			arc, and an angle.  The plane of the arc is defined by 
**			the center and the construction plane normal. The point 
**			on the circumference will be projected to this plane.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_arcapt()
	{
/**	UM_coord	pt;	**/					/* the first  point */
    UD_NDCLOCREC pt;

	UM_angle	angrad;					/* the arc angle in radians */
	struct UM_circle_rec	cptr;		/* circle entity */
	int numint;							/* the number of interactions input
												through das */
/**	UM_coord center; **/
    UD_NDCLOCREC center;

	int status;

	uu_denter( UU_MTRC,(us,"umu_c3_arcapt()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*circle center*/UM_MODEL, 14, &center, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		ud_ldas(UD_DASCART, /*point on circumference*/UM_MODEL, 17, 
			 &pt, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASANGLE,/*angle*/UM_MODEL, 13, &angrad, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		if(angrad < -UM_TWOPI || angrad > UM_TWOPI)
			{
			uu_uerror0(/*angle not in range -360..360*/UM_MODEL,28);
			goto repeat;
			}
		status = um_c3_arccpa(&center, &pt, angrad, &cptr);
		if (status == 0)
			{
			um_create_geom(&cptr, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&cptr);
			}
repeat:;
		}
done:;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_c3_arcaptn()
**			Create an arc from a normal, center point, a point lying on the
**			circumference of the circle (arc), and an angle.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_arcaptn()

	{
	struct	UM_circle_rec	cptr;		/* circle entity */
/**	UM_coord		center; **/					/* circle center */
    UD_NDCLOCREC center;

	UM_vector	normal;					/* circle normal */
/**	UM_coord		pt; **/				/* the first  point */
    UD_NDCLOCREC pt;
 
	UM_angle		angrad;					/* the arc angle in radians */
	int			numint;					/* the number of interactions input
													through das */
	int			status;

	uu_denter( UU_MTRC,(us,"umu_c3_arcaptn()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*circle center*/UM_MODEL, 14, &center, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		ud_ldas(UD_DASCART, /*point on circumference*/UM_MODEL, 17, 
			 &pt, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASANGLE,/*angle*/UM_MODEL, 13, &angrad, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASVEC,/*normal vector*/UM_MODEL, 182, normal, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		if(angrad < -UM_TWOPI || angrad > UM_TWOPI)
			uu_uerror0(/*um_angle not in range -360..360*/UM_MODEL,28);
		else if (um_cceqcc(&center,&pt))
			uu_uerror0(/*center identical to circumference point*/UM_MODEL,20);
		else
			{
			status = um_c3_arccpan(&center, &pt, angrad, normal, &cptr);
			if (status == 0)
				{
				um_create_geom(&cptr, UM_DEFAULT_TF, UM_CURRENT_ATTR);
				uc_display(&cptr);
				}
			}
repeat:;
		}
done:;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_c3_arccnaarb()
**			Create a circuluar arc from the center, normal, two angles, 
**			a base vector, and radius which are input by the user.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_arccnaarb()

	{
	struct	UM_circle_rec	cptr;		/* circle entity */
	UM_angle		angrad1;				/* the arc angle in radians from
												the base vector to the start vector*/
	UM_angle		angrad2;				/* the arc angle in radians from
												the base vector to the end vector */
	UM_vector	normal;
	UM_vector	base_vec;
	UM_length	radius;
/**	UM_coord		center; **/
    UD_NDCLOCREC center;

	int			status;
	int			numint;				/* the number of interactions input
												through das */

	uu_denter( UU_MTRC,(us,"umu_c3_arccnaarb()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*circle center*/UM_MODEL, 14, &center, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		ud_ldas(UD_DASVEC, /*normal vector :*/UM_MODEL, 182,
			normal, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASVEC, /*base vector :*/UM_MODEL, 195,
			 base_vec, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASDISTANCE, /*radius*/UM_MODEL, 25, &radius, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASANGLE,/*start angle*/UM_MODEL, 278, &angrad1, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASANGLE,/*end angle*/UM_MODEL, 279, &angrad2,
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		status = um_c3_arccnaarb(&center, normal, angrad1, angrad2, base_vec, radius, &cptr);
		if (status == 0)
			{
			um_create_geom(&cptr, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&cptr);
			}
repeat:;
		}
done:;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_c3_arccraa()
**			Create a circular arc defined by a center, radius, start angle
**			and end angle. The normal to the plane of the circle is the
**			construction plane z-axis and the angles are measured relative
**			to the construction plane x-axis (right hand rule).
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_arccraa()

	{
	struct	UM_circle_rec	cptr;		/* circle entity */
	UM_angle		angrad1;				/* the arc angle in radians from
												the base vector to the start vector*/
	UM_angle		angrad2;				/* the arc angle in radians from
												the base vector to the end vector */
	UM_length	radius;
/**	UM_coord		center; **/
    UD_NDCLOCREC center;
 
	int			status;
	int			numint;				/* the number of interactions input
												through das */

	uu_denter( UU_MTRC,(us,"umu_c3_arccraa()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*circle center*/UM_MODEL, 14, &center, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		ud_ldas(UD_DASDISTANCE, /*radius*/UM_MODEL, 25, &radius, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASANGLE,/*start angle*/UM_MODEL, 278, &angrad1, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASANGLE,/*end angle*/UM_MODEL, 279, &angrad2,
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		status = um_c3_arccnaarb(&center, UM_cpln.zaxis, angrad1, angrad2,
				UM_cpln.xaxis, radius, &cptr);
		if (status == 0)
			{
			um_create_geom(&cptr, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&cptr);
			}
repeat:;
		}
done:;
	uu_dexit;
	}
