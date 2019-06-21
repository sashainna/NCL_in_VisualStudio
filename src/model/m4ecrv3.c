/*********************************************************************
**    NAME         :  m4ecrv3.c
**       CONTAINS: AG curve support routines 
**			int um_agcrv_feature (eptr, tfmat, feature_order, dploc)
**			int um_agcrv_delete(key)
**			int um_agcrv_print(eptr)
**			int um_agcrv_dissolve(eptr)
**			int um_agcrv_span(eptr, dimptr, space)
**			int um_agcrv_reverse(eptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4ecrv3.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:02
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "class.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdeval.h"
#include "mcrv.h"
#include "mdebug.h"

#include "ag_incl.h"
#include "ag_global.h"

/*********************************************************************
**    E_FUNCTION     : um_agcrv_feature (eptr, tfmat, feature_order, dploc)
**       Calculate features for AG rational bspline curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to rational bspline curve
**				tfmat					transformation matrix
**				feature_order		order of feature to display
**				dploc					picked location on curve
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS iff on error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_agcrv_feature (eptr, tfmat, feature_order, dploc)
	struct UM_agcrv_rec *eptr;
	UM_transf tfmat;
	int feature_order;
	UD_PLOCREC *dploc;

	{
	int status;
	UM_coord vprefpt;
	UM_vector vptan, vpup, vpnorm;
	UM_length aperture;
	UU_REAL char_height;
	UM_coord cntlpt;
	UM_vector vec;
	UU_REAL len;
	struct UM_evcrvout evout;
	int i;
	int dim;
	AG_CURVEP crvp;
	AG_SPLINEP bsp;
	AG_CNODEP nodep;

	uu_denter(UU_MTRC,(us,"um_agcrv_feature(%d,tfmat:%x,%d)",
					eptr->key,tfmat,feature_order));

	/* get the view that the features will be displayed in) */
	um_get_current_viewplane(vprefpt, vptan, vpup, vpnorm, &aperture);
 
   /* set feature character height */
	um_set_feature_char_height(&char_height);

	/* calculate desired features */
	status = UU_SUCCESS;
	switch (feature_order)
		{
		case 1:
			/* show control points */
			crvp = (AG_CURVEP) eptr->crvaddr;
			dim = crvp->dim;
			bsp = crvp->bs0;
			for (i=0; i<crvp->nbs; i++)
				{
				nodep = bsp->node0;
				do
					{
					um_vctovc(nodep->Pw, cntlpt);
					if (bsp->rat) um_vctmsc(cntlpt, 1.0/nodep->Pw[dim], cntlpt);
					status = um_feacoord(cntlpt);
					if (status!=0)
						return UU_FAILURE;
					nodep = nodep->next;
					}
				while ((nodep != NULL) && (nodep != bsp->node0));
				bsp = bsp->next;
				}

			/* show derivatives */
			uc_init_evcrvout(eptr, &evout);
			len = 3.0 * char_height;

			/* scale tangent vector at start point */
			um_agcrv_evaluate(UM_FRSTDERIV, (UU_REAL) 0.0, eptr, tfmat, &evout);
			um_unitvc(evout.dcdu, vec);
			um_vctmsc(vec, len, vec);
			status = um_feavect(evout.cp, vec);
			if (status!=0)
				return UU_FAILURE;

			/* scale tangent vector at end point */
			um_agcrv_evaluate(UM_FRSTDERIV, (UU_REAL) 1.0, eptr, tfmat, &evout);
			um_unitvc(evout.dcdu, vec);
			um_vctmsc(vec, len, vec);
			status = um_feavect(evout.cp, vec);
			if (status!=0)
				return UU_FAILURE;

			break;

		case 2:
			/* tangents at knot values */
			/* length of spline */
			break;

		case 3:
			break;

		default:
			status = UU_FAILURE;

		}
	uu_dexitstatus("um_agcrv_feature", status);
	return(status);
	}
       
/*********************************************************************
**    E_FUNCTION     : int um_agcrv_delete(key)
**			Delete the specified curve from BOTH UNIBASE and AG.
**    PARAMETERS   
**       INPUT  : 
**				key						master tuple id of curve to be deleted
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_delete(key)
	UU_KEY_ID key;

	{
	struct UM_agcrv_rec crv;
	int status;

	uu_denter(UU_MTRC,(us,"um_agcrv_delete(key=%d)",key));

	crv.key = key;
	status = uc_retrieve_data(&crv, sizeof(crv));
	if (status != UU_SUCCESS) goto done;

	status = umi_agcrv_delete(&crv);
	if (status != UU_SUCCESS) goto done;

	ur_delete_all(key);

done:
	uu_dexitstatus("um_agcrv_delete",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_print(eptr)
**			Print a AG curve entity.
**    PARAMETERS   
**       INPUT  : 
**				eptr		pointer to an entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_print(eptr)
	struct UM_agcrv_rec  *eptr;

	{
	int status;
	AG_CURVEP bc;

	uu_denter(UU_MTRC,(us,"um_agcrv_print(key=%d)",eptr->key));
	status = UU_SUCCESS;

	sprintf(UM_sbuf, "AG CURVE: %x", eptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "  crvaddr  = %x", eptr->crvaddr);
	um_pscroll(UM_sbuf);

	bc = (AG_CURVEP) eptr->crvaddr;
	if (bc == NULL)
		um_pscroll("no curve defined in AG database");
	else
		ag_fw_crv(UM_psfile, bc, "agcrv", AG_ASCII);
	fflush(UM_psfile);

	uu_dexitstatus("um_agcrv_print",status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_dissolve(eptr)
**			If the given AG curve (EPTR) has more than one bspline
**			segment, a new curve will be created for each bspline
**			segment and the old curve will be deleted.
**    PARAMETERS   
**       INPUT  : 
**				eptr					pointer to entity data
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_dissolve(eptr)
	struct UM_agcrv_rec *eptr;

	{
	int status = UU_FAILURE;
	struct UC_entitydatabag unicrv;
	struct UM_agcrv_rec newcrv;
	struct UM_attrdata_rec attr;
	UM_transf tfmat;
	AG_CURVEP crv;
	AG_CURVEP ag_bld_crv();
	AG_SPLINEP bs;
	AG_SPLINEP bsn;
	int nbs;
	int i;

	uu_denter(UU_MTRC,(us,"um_agcrv_dissolve(e(r,k)=(%d,%x)", 
		eptr->rel_num, eptr->key));

	crv = (AG_CURVEP) eptr->crvaddr;
	if (crv != NULL)
		{
		nbs = crv->nbs;
		if (nbs > 1)
			{
			status = uc_retrieve_transf(eptr->key, tfmat);
			if (status != UU_SUCCESS) goto done;
			status = uc_retrieve_attr(eptr->key, &attr);
			if (status != UU_SUCCESS) goto done;
			bs = crv->bs0;
			crv->bs0 = NULL;
			crv->bs = NULL;
			uc_delete(eptr->key);
			uc_setup_data(UM_AGCRV_REL, &newcrv, sizeof(newcrv));
			for (i=0; i<nbs; i++)
				{
				bsn = bs->next;
				crv = ag_bld_crv(3);
				ag_crv_app_bs(crv, bs);
				newcrv.crvaddr = (int) crv;
				status = umi_agcv_to_unicrv(crv, &unicrv);
				if (status == UU_SUCCESS)
					{
					uc_create_data(&unicrv, tfmat, &attr);
					uc_display(&unicrv);
					umi_agcrv_delete(&newcrv);
					}
				else
					{
					uc_create_data(&newcrv, tfmat, &attr);
					uc_display(&newcrv);
					}
				bs = bsn;
				}
			}
		status = UU_SUCCESS;
		}

done:;
	uu_dexitstatus("um_agcrv_dissolve",status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int um_agcrv_span(eptr, dimptr, space)
**       A method-- returns the affine span of a curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr				curve
**       OUTPUT :  
**          *dimptr			dimension of affine space
**				space				definition of affine space (point/vector)
**    RETURNS      :
**			UU_SUCCESS iff no problem, -2 if not defined 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_span(eptr, dimptr, space)
	struct	UM_agcrv_rec	*eptr;
	int		*dimptr;
	UU_REAL	space[2][3];

	{
	int	status;
	int	badent;
	int	i;
	struct UM_evcrvout evcrv;
	UM_coord pt[11];
	UM_param u, deltau;

	uu_denter(UU_MTRC,(us,"um_agcrv_span(key=%x)", eptr->key));

	deltau = 1.0/10;
	for (i=0, u=0.0; i<11; i++, u=u+deltau)
		{
		um_agcrv_evaluate(UM_POINT, u, eptr, UM_DEFAULT_TF, &evcrv);
		um_vctovc(evcrv.cp, pt[i]);
		}
	status = um_span_ptlist(11, pt, dimptr, space, &badent);

	uu_dexitstatus("um_agcrv_span", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_reverse(eptr)
**			Reverse the parameterization of the curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr						pointer to curve
**       OUTPUT :  
**          eptr						pointer to curve
**    RETURNS      : 
**			UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_reverse(eptr)
	struct UM_agcrv_rec *eptr;

	{
	uu_denter(UU_MTRC,(us,"um_agcrv_reverse(key=%d)", eptr->key));
	
	ag_crv_rev_dir(eptr->crvaddr);

	uu_dexit;
	return (UU_SUCCESS);
	}
