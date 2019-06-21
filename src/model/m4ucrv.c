/*********************************************************************
**    NAME         :  m4ucrv.c
**       CONTAINS: user interface routines for AG curve construction
**			umu_agcrv_cubic(dim, option)
**			umu_offset_curve()
**			umu_reverse_curve()
**			umu_pt_on_curve_at_pal()
**			umu_unicrv_to_agcrv()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4ucrv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:05
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "modef.h"
#include "mdcpln.h"
#include	"mdpick.h"
#include "mdebug.h"
#include "mdeval.h"
#include "mcrv.h"
#include "misect.h"
#include "mpopmenu.h"

#include "ag_incl.h"
#include "ag_global.h"
#include "ag_l_crv.h"

extern UU_LOGICAL UM_display_control_polygon;

/*********************************************************************
**    E_FUNCTION     : umu_reverse_curve()
**			Prompt the user to pick curves to reverse.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_reverse_curve()

	{
	struct UC_entitydatabag *e;
	int  numint;
	UU_LOGICAL initialize;
	int status;
	int bagsize;

	uu_denter(UU_MTRC,(us,"umu_reverse_curve()"));

	ud_lgeo(UU_TRUE, UD_allcurvess);

	bagsize = sizeof(struct UC_entitydatabag);
	e = (struct UC_entitydatabag *) uu_malloc(bagsize);

	ud_ldas(UD_DASSELECT, /*pick curve to reverse parameterization */UM_APPGEO,
		52, UU_NULL, 0, &numint, UD_NODEFAULT);
	if (numint < 1) goto done;

	initialize = UU_TRUE;
	while(ud_gnxt(initialize, UU_NULL, &e->key, 1) == UU_TRUE)
   	{                                              
		initialize = UU_FALSE;
		status = uc_retrieve_data(e, bagsize);
		if (status != UU_SUCCESS) goto next;

		status = uc_reverse_curve(e);
		if (status == UU_SUCCESS)
			{
				if (e->rel_num != UM_RBSPLCRV_REL && e->rel_num != UM_UVCVONSF_REL) 
					um_update_geom(e, UM_DEFAULT_TF);
				uc_display(e);
			}
next:;
		}
done:;
	uu_free(e);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_pt_on_curve_at_pal()
**			Create points along the curve (CRV) at points specified by
**			the user at percentage arc length along the curve.
**    PARAMETERS   
**       INPUT  : 
**          crv				pointer to curve entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_pt_on_curve_at_pal(crv)
	struct UM_agcrv_rec *crv;

	{
	int numint;
	int status = UU_FALSE;
	int err;
	int ag_len_crv();
	int ag_crvp_fs_crv();
	struct UM_evcrvout evcrv;
	struct UM_point_rec p;
	UM_param u;
	AG_LCRVP lcrvh;
	AG_LCRVP ag_bld_lcrv();
	AG_SPLINEP bs;
	UU_REAL t;
	UU_REAL ua;
	UU_REAL len;
	UU_REAL eps;

	uu_denter(UU_MTRC,(us,"ugu_pt_on_crv_at_pal(e(r,k)=(%d,%x)",
		crv->rel_num, crv->key));

	/* initialize point record */
	ur_setup_data(UM_POINT_REL, &p, sizeof(p));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (p.label, "");
	p.subscr = 0;

	/* determine the length of the curve */
	eps = 0.001;
	len = 0.00;
	lcrvh = ag_bld_lcrv(crv->crvaddr, eps, len, NULL);
	err = ag_len_crv(lcrvh);
	if (err) goto done;

	/* for each parameter value entered by the user, determine the 
		corresponding point along the curve; then create a point
		entity and display it */
	while (UU_TRUE)
      {
      ud_ldas(UD_DASUNITLESS, /* enter u arclength parameter on curve */
         UM_MODEL, 323, &u, 1, &numint, 2);
      if (numint <= 0) break;

		if (u < UM_FUZZ)
			{ /* close to start */
			um_agcrv_evaluate(UM_POINT, 0.0, crv, UM_idmat, &evcrv);
			um_vctovc(evcrv.cp, p.pt);
			}
		else if (u > (1.0 - UM_FUZZ))
			{ /* close to end */
			um_agcrv_evaluate(UM_POINT, 1.0, crv, UM_idmat, &evcrv);
			um_vctovc(evcrv.cp, p.pt);
			}
		else
			{ /* clearly in between */
			ua = u * lcrvh->length;
			err = ag_crvp_fs_crv(ua, lcrvh, p.pt, &bs, &t);
			if (err) goto done;
			}

      um_create_pt1(&p, UM_DEFAULT_TF, UM_CURRENT_ATTR);
      uc_display(&p);
      }

	status = UU_SUCCESS;

done:;

	/* deallocate curve length structure */
	ag_db_lcrv(&lcrvh);

	uu_dexitstatus("umu_pt_on_curve_at_pal", status);
	return (status);
	}
