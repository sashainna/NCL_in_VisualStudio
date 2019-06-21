
/*********************************************************************
**    NAME         :  m3ucirc1.c
**       CONTAINS: user interface circle creation routines
**			umu_c3_cr
**			umu_c3_rc
**			umu_c3_cp
**			umu_c3_crn
**			umu_c3_cpn
**			umu_c3_ct
**			umu_c3_2diampts
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ucirc1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:57
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdclass.h"
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdpick.h"

/*********************************************************************
**    E_FUNCTION     : umu_c3_cr()
**			Create concentric circles lying in a plane parallel to the
**			the xy construction plane, passing through a user specified
**			center point, and having various radii.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_cr()

	{
	struct UM_circle_rec cptr;
/**	UM_coord center; **/
    UD_NDCLOCREC center;

	UM_length radius;
	int status;
	int numint;

	uu_denter(UU_MTRC,(us,"umu_c3_cr()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*center*/UM_MODEL,14,&center, 1, 
			&numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		while (UU_TRUE)
			{
			ud_ldas(UD_DASDISTANCE, /*radius*/UM_MODEL, 15, &radius, 
				1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			status = um_c3_cr(&center, radius, &cptr);
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
**    E_FUNCTION     : umu_c3_rc()
**			Create circles lying in a plane parallel to the the xy
**			construction plane, having the same user specified radius,
**			and passing through a various center points.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_rc()

	{
	struct UM_circle_rec cptr;
/**	UM_coord center; **/
    UD_NDCLOCREC center;

	UM_length radius;
	int status;
	int numint;

	uu_denter(UU_MTRC,(us,"umu_c3_rc()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASDISTANCE, /*radius*/UM_MODEL, 15, &radius, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		while (UU_TRUE)
			{
			ud_ldas(UD_DASCART, /*center*/UM_MODEL,14,&center, 1, 
				&numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			status = um_c3_cr(&center, radius, &cptr);
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
**    E_FUNCTION     : umu_c3_cp()
**       Create a circle given the center point and a point on the 
**       circumference.  The normal to the plane of the circle is
**       the construction plane normal.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_cp()

	{
	struct UM_circle_rec cptr; 	/* circle entity ptr */
/**	UM_coord center; **/				/* center point */
/**	UM_coord cpt; **/					/* circumference point */
    UD_NDCLOCREC center, cpt;

	int numint;
	int status;

	uu_denter(UU_MTRC,(us,"umu_c3_cp()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*center*/UM_MODEL, 14, &center, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		while (UU_TRUE)
			{
			ud_ldas(UD_DASCART, /*point on circumference*/UM_MODEL, 17,&cpt, 
				 1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			status = um_c3_cp(&center, &cpt, &cptr);
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
**    E_FUNCTION     : umu_c3_crn()
**			Prompt the user for a center point, radius, and normal
**			and create a circle.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_crn()
	{
	struct UM_circle_rec	cptr;
/**	UM_coord	center; **/					/* center point */
    UD_NDCLOCREC center;

	UM_vector normal;						/* normal vector */
	UM_length radius;						/* radius */
	int status;
	int numint;

	uu_denter(UU_MTRC,(us,"umu_c3_crn()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*center*/UM_MODEL,14,&center, 1, 
			&numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		ud_ldas(UD_DASVEC, /*normal vector*/UM_MODEL,182,normal, 1, 
			&numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASDISTANCE, /*radius*/UM_MODEL, 15, &radius, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		if (radius < UM_FUZZ)
			uu_uerror0(/*radius is too small*/UM_MODEL,19);
		else
			{
			status = um_c3_crn(&center, radius, normal, &cptr);
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
**    E_FUNCTION     : umu_c3_cpn()
**			Prompt the user for a center point, a point on the circumference,
**			and the normal and create a circle.
**       circumference.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_cpn()

	{
	struct UM_circle_rec	cptr;
/**	UM_coord cpt;	**/						/* circumference point */
/**	UM_coord center;**/						/* center point */
    UD_NDCLOCREC cpt, center;

	UM_vector normal;						/* normal vector */
	int status;
	int numint;

	uu_denter(UU_MTRC,(us,"umu_c3_cpn()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*center*/UM_MODEL, 14, &center, 
			 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		ud_ldas(UD_DASVEC, /*normal vector*/UM_MODEL, 182, normal, 
			 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASCART, /*point on circumference*/UM_MODEL, 17,&cpt, 
				 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		if (um_cceqcc(&center,&cpt))
			uu_uerror0(/*center and circumference point are identical*/UM_MODEL,20);
		else
			{
			status = um_c3_cpn(&center, &cpt, normal, &cptr);
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
**    E_FUNCTION     : umu_c3_ct()
**       Create a circle given the center point and a line or circle
**       it is to be tangent to.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_ct()
	{
	struct UM_circle_rec c;			/* circle entity */
	struct UM_crvdatabag e;			/* entity to be tangent to */
	UM_PLOCREC pick;					/* pick information */
/**	UM_coord center; **/					/* circle center */
    UD_NDCLOCREC center;

	int numint;
	int status;

	uu_denter(UU_MTRC,(us,"umu_c3_ct()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*circle center*/UM_MODEL, 14,
			&center, 1, &numint, UD_NODEFAULT);
		if (numint <= 0)  goto done;
		um_dl_pldas(UD_DASPCKLOC, /*pick tangent line/circle*/ UM_MODEL,
			156, &pick, 1, &numint, 2);
		if (numint <= 0)  goto repeat;
		e.key = um_get_pickkey(&pick.pent, 2);
		ur_retrieve_data(&e,sizeof(struct UM_crvdatabag));
		switch (e.rel_num)
			{
			case  UM_LINE_REL:
				status = um_c3_ct_line(&e, &center, &pick.ploc, &c);
				break;
			case  UM_CIRCLE_REL:
				status = um_c3_ct_circle(&e, &center, &pick.ploc, &c);
				break;
			default:
				status = -1;
				uu_uerror0(/*you must pick a line or circle*/UM_MODEL,23);
				break;
			}
		if (status == 0)
			{
			um_create_geom(&c, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&c);
			}
repeat:;
		}
done:;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_c3_2diampts()
**			Create a circle given two diametrically opposite points.
**       The normal to the plane of the circle is the construction
**			plane normal.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_2diampts()

	{
	struct UM_circle_rec cptr; 	/* circle entity ptr */
/**	UM_coord diampt1;	**/				/* one point on circle */
/**	UM_coord diampt2;	**/				/* diametrically opposite point */
	UM_coord center;					/* center of circle */
    UD_NDCLOCREC diampt1, diampt2;

	int numint;
	int status;

	uu_denter(UU_MTRC,(us,"umu_c3_2diampts()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*point on circumference*/UM_MODEL, 17, &diampt1, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		ud_ldas(UD_DASCART, /*diametrically opposite point */UM_MODEL, 250,
			&diampt2, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		um_vcplvc(&diampt1, &diampt2, center);
		um_vctmsc(center, (UU_REAL) 0.5, center);
		status = um_c3_cp(center, &diampt1, &cptr);
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
