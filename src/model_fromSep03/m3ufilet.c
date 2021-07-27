/*********************************************************************
**    NAME         :   m3ufilet.c
**       CONTAINS:  User interface fillet routines
**			int umu_crv_fillet()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ufilet.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:59
*********************************************************************/
#include "zsysdep.h"
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mcrv.h"
#include	"mdpick.h"
#include "mdebug.h"
#include "mdeval.h"
#include "misect.h"

/*********************************************************************
**    E_FUNCTION     : umu_crv_fillet()
**       Prompt the user to
**				1. enter the radius of the fillet curve
**				2. pick two planar curves near the end points of 
**					the fillet curve
**			A fillet curve will be calculated, stored in UNIBASE, and
**			displayed in DIGS.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_crv_fillet()

	{
	int numint;
	int status;
	UM_length radius;
	UM_PLOCREC pick;

	struct UC_entitydatabag *crv1;
	UD_NDCLOCREC ploc1;
	UM_isect isect1;

	struct UC_entitydatabag *crv2;
	UD_NDCLOCREC ploc2;
	UM_isect isect2;

	struct UC_entitydatabag *fillet;
	struct UM_evcrvout *evcrv;
	UM_vector crv_tangent;
	UM_vector fillet_tangent;

	int bagsize;
	struct UC_entitydatabag *part1;
	struct UC_entitydatabag *part2;

	uu_denter(UU_MTRC,(us,"umu_crv_fillet()"));

	bagsize = sizeof(struct UC_entitydatabag);
	crv1 = (struct UC_entitydatabag *) uu_malloc(bagsize);
	crv2 = (struct UC_entitydatabag *) uu_malloc(bagsize);
	part1 = (struct UC_entitydatabag *) uu_malloc(bagsize);
	part2 = (struct UC_entitydatabag *) uu_malloc(bagsize);
	fillet = (struct UC_entitydatabag *) uu_malloc(bagsize);
	evcrv = (struct UM_evcrvout *) uu_malloc(sizeof(struct UM_evcrvout));

	ud_leditable(UU_TRUE);
	while (UU_TRUE)
		{
		/* prompt for fillet radius */
		ud_ldas(UD_DASDISTANCE, /* enter fillet radius */ UM_APPGEO, 28,
			&radius, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		while (UU_TRUE)
			{
   ud_lgeo(UU_TRUE, UD_ncl_lnci);
			/* prompt for first curve to fillet */
			um_dl_pldas(UD_DASPCKLOC,/* pick curve near start of fillet */UM_APPGEO,
					29, &pick, 1, &numint, 2);
			if (numint < 1) goto repeat;
			crv1->key = um_get_pickkey(&pick.pent, 2);
			uc_retrieve_data(crv1, bagsize);
			um_copyploc(&pick.ploc, &ploc1);
	
			/* prompt for  second curve to fillet */
   ud_lgeo(UU_TRUE, UD_ncl_lnci);
			um_dl_pldas(UD_DASPCKLOC,/* pick curve near end of fillet */UM_APPGEO,
					30, &pick, 1, &numint, 2);
			if (numint < 1) goto repeat;
			crv2->key = um_get_pickkey(&pick.pent, 2);
			uc_retrieve_data(crv2, bagsize);
			um_copyploc(&pick.ploc, &ploc2);
	
			/* calculate fillet curve */
			status = uc_crv_fillet(radius, crv1, &ploc1, crv2, &ploc2,
				&isect1, &isect2, fillet);
			if (status != UU_SUCCESS)
				{
				um_pscroll("unable to calculate fillet curve");
				uu_uerror0(/* unable to calculate fillet curve */
					UM_APPGEO, 3);
				goto repeat;
				}

			/* create and display fillet curve */
			uc_create_data(fillet, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(fillet);

			/* trim first curve to fillet curve */
         if (!um_is_curve_closed(crv1, UM_DEFAULT_TF))
			   status = umi_trim_crv_to_fillet(crv1, &ploc1, &isect1, fillet);
			if (status != UU_SUCCESS)
				{
				um_pscroll("unable to trim first curve");
				goto repeat;
				}

			/* trim second curve to fillet curve */
         if (!um_is_curve_closed(crv2, UM_DEFAULT_TF))
			status = umi_trim_crv_to_fillet(crv2, &ploc2, &isect2, fillet);
			if (status != UU_SUCCESS)
				{
				um_pscroll("unable to trim second curve");
				goto repeat;
				}
			}
repeat:;
		}
done:;
	uu_free(crv1);
	uu_free(crv2);
	uu_free(part1);
	uu_free(part2);
	uu_free(fillet);
	uu_free(evcrv);
	uu_dexit;
	}

