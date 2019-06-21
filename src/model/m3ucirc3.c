
/*********************************************************************
**    NAME         :  m3ucirc3
**       CONTAINS: user interface routines to create circle or arc
**			umu_c3_3pt(option)
**       umu_c3_ttr
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ucirc3.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:57
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdpick.h"
#include "misect.h"
#include "dselmask.h"

/*********************************************************************
**    E_FUNCTION     : umu_c3_3pt(option)
**			Create a circle or circular arc through three points.
**			The order of entry of the three points determines the
**			arc created (i.e. from first point, through the second
**			point, to the third point).
**    PARAMETERS   
**       INPUT  : 
**				option				UU_TRUE => create an arc
**										UU_FALSE => create a circle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_3pt(option)
	UU_LOGICAL option;

	{
/**	UM_coord	pt1;							* the three  points */
/**	UM_coord	pt2;							* the three  points */
/**	UM_coord	pt3;							* the three  points */
    UD_NDCLOCREC pt1, pt2, pt3;

	struct	UM_circle_rec	cptr;		/* circle entity */
	int		numint;
	int		status;

	uu_denter( UU_MTRC,(us,"umu_c3_3pt(%d)",option));
/*
.....initialized the key to 0 to avoid UMR in um_create_geom 
*/
	cptr.key = 0;

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*first point*/UM_MODEL, 19, 
			&pt1, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		ud_ldas(UD_DASCART, /*second point*/UM_MODEL, 20, 
			&pt2, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASCART, /*third point*/UM_MODEL, 21, 
			&pt3, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		if (option)
			status = um_c3_arc3pt(&pt1, &pt2, &pt3, &cptr);
		else
			status = um_c3_3pt(&pt1, &pt2, &pt3, &cptr);
		if (status == 0)
			{
			um_create_geom(&cptr, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&cptr);
			}
repeat:;
		}
done:;
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : umu_c3_ttr(option)
**			Create a circle (arc) tangent to two curves.
**    PARAMETERS   
**       INPUT  : 
**				option     		1 => circle;
**									2 => circlular arc;
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c3_ttr(option)
	int option;

	{
	int status;
	int numint;
	int bagsize;
	UM_PLOCREC pick;

	UM_length radius;

	struct UC_entitydatabag *crv1;
	UD_NDCLOCREC ploc1;
	UM_isect isect1;

	struct UC_entitydatabag *crv2;
	UD_NDCLOCREC ploc2;
	UM_isect isect2;

	struct UC_entitydatabag *fillet;
	struct UM_circle_rec *c;

	uu_denter( UU_MTRC,(us,"umu_c3_ttr(%d)",option));

	bagsize = sizeof(struct UC_entitydatabag);
	crv1 = (struct UC_entitydatabag *) uu_malloc(bagsize);
	crv2 = (struct UC_entitydatabag *) uu_malloc(bagsize);
	fillet = (struct UC_entitydatabag *) uu_malloc(bagsize);


	while (UU_TRUE)
		{
		ud_ldas(UD_DASDISTANCE, /*radius*/ UM_MODEL, 25, &radius, 
			 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		if (radius < UM_FUZZ)
			{
			uu_uerror0(/*radius is too small*/UM_MODEL,19);
			goto repeat;
			}

		while (UU_TRUE)
			{
   ud_lgeo(UU_TRUE, UD_ncl_lnci);
			um_dl_pldas(UD_DASPCKLOC, /*pick first line / circle*/ UM_MODEL, 157,
				&pick, 1, &numint, 2);
			if (numint < 1) goto repeat;
			crv1->key = um_get_pickkey(&pick.pent, 2);
			um_copyploc(&pick.ploc, &ploc1);

   ud_lgeo(UU_TRUE, UD_ncl_lnci);
			um_dl_pldas(UD_DASPCKLOC, /*pick second line / circle*/UM_MODEL, 158,
				&pick, 1, &numint, 2);
			if (numint <= 0) goto repeat;
			crv2->key = um_get_pickkey(&pick.pent, 2);
			um_copyploc(&pick.ploc, &ploc2);

			if (crv1->key == crv2->key)
				{
				uu_uerror0(/*picked entities are same*/UM_MODEL,35);
				goto repeat;
				}

			status = uc_retrieve_data(crv1, bagsize);
			if (status != UU_SUCCESS) goto repeat;

			status = uc_retrieve_data(crv2, bagsize);
			if (status != UU_SUCCESS) goto repeat;

			status = uc_crv_fillet(radius, crv1, &ploc1, crv2, &ploc2,
											&isect1, &isect2, fillet);
			if (status != UU_SUCCESS)
				{
				um_pscroll("unable to calculate fillet curve");
				uu_uerror0(/* unable to calculate fillet curve */
				UM_APPGEO, 3);
				goto repeat;
				}

			/* create a complete circle if required */
			if (option == 1)
				{
				c = (struct UM_circle_rec *) fillet;
				c->dang = UM_TWOPI;
				um_cir_svec(c);
				}

			/* create and display circle/arc curve */
			uc_create_data(fillet, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(fillet);
			}
repeat:;
		}
done:;
	uu_free(crv1);
	uu_free(crv2);
	uu_free(fillet);
	uu_dexit;
	return 0;
	}
