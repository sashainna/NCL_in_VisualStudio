/*********************************************************************
**    NAME         :  f4conic.c 
**       CONTAINS: routines to display features on conics.
**			int um_f4_conic(ent, tfmat, feature_order, dploc)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       f4conic.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:44
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdeval.h"
#include "modef.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     :  int um_f4_conic(ent, tfmat, feature_order, dploc)
**			Calculate the desired features for a given order for a 
**			conic
**    PARAMETERS   
**       INPUT  : 
**				ent				pointer to the geometric information
**				tfmat				transformation matrix
**          feature_order	the order of the features to be calculated
**				dploc				picked location on conic
**       OUTPUT :  
**          none
**    RETURNS      : status - returned error statusus
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_f4_conic(ent, tfmat, feature_order, dploc)
	struct UM_conic_rec *ent;
	UM_transf tfmat;
	int feature_order;
	UD_PLOCREC *dploc;

	{
	struct UM_evcrvout evout;
	UU_REAL scale;
	UU_REAL char_height;
	UM_vector vect;
	UM_coord pos;
	UU_LOGICAL dflag;
	UU_LOGICAL status;

	UM_coord vporigin;
	UM_vector vpxaxis;
	UM_vector vpyaxis;
	UM_vector vpzaxis;
	UU_REAL aperture;

	UM_cn_defn conic;

	UM_vector major_axis, major_pos;
	UM_vector minor_axis, minor_pos;
	UM_length major_length, minor_length;
	
	uu_denter(UU_MTRC,(us,"um_f4_conic(key=%x,tfmat=%x,order=%d)", 
						ent->key, tfmat, feature_order));

	/* get current viewplane definition */
	um_get_current_viewplane(vporigin, vpxaxis, vpyaxis, vpzaxis, &aperture);

	/* set feature character height */
	um_set_feature_char_height(&char_height);

	/* get major conic definition */
	um_cn_defn(ent, tfmat, &conic);

	if (feature_order == 1)
		{
		/*	center point */
		status = um_feacoord (conic.center);
		if (status!=0)
			return UU_FAILURE;

		/*	major axis and length*/
		if (ent->type == UM_PARABOLA)
			{
			major_length = 1.0;
			minor_length = 1.0;
			}
		else
			{
			major_length = conic.major_length;
			minor_length = conic.minor_length;
			}
		if (conic.major_length > 0.0)
			{
			um_vctmsc(conic.major_axis, major_length, major_axis);
			um_vcplvc(conic.center, major_axis, major_pos);
			um_vctovc(vpyaxis, pos);
			um_vctmsc (pos, 2.0 * char_height, pos);
			um_vcplvc(major_pos, pos, pos);
			dflag = UU_TRUE;
			status = um_fealength(" Major length= ", conic.major_length, pos,
								major_pos, dflag);
			if (status!=0)
				return UU_FAILURE;
			status = um_feavect (conic.center, major_axis);
			if (status!=0)
				return UU_FAILURE;
			}

		/*	minor axis and length */
		if (conic.minor_length > 0.0)
			{
			um_vctmsc(conic.minor_axis, minor_length, minor_axis);
			um_vcplvc(conic.center, minor_axis, minor_pos);
			um_vctovc(vpyaxis, pos);
			um_vctmsc (pos, 2.0 * char_height, pos);
			um_vcplvc(minor_pos, pos, pos);
			dflag = UU_TRUE;
			um_fealength(" Minor length= ", conic.minor_length, pos,
								minor_pos, dflag);
			status = um_feavect (conic.center, minor_axis);
			if (status!=0)
				return UU_FAILURE;
			}

		/*	normal vector */
		um_vctmsc (conic.pln_normal, major_length, vect);
		status = um_feavect (conic.center, vect);
		if (status!=0)
			return UU_FAILURE;

		status = UU_SUCCESS;
		}
	
	if (feature_order > 1)
		{
		switch (ent->type)
			{
			case UM_ELLIPSE:
				{
				status = UU_SUCCESS;
				uc_init_evcrvout(ent, &evout);
				switch (feature_order)
					{
					case 2:								/* Foci, Quarter Points */
		
						scale = sqrt(ent->invariants[0]*ent->invariants[0] 
										- ent->invariants[1]*ent->invariants[1]);
						um_vctmsc(ent->tfmat[0], scale, vect);
						um_vcplvc(vect, ent->tfmat[3], vect);
						status = um_feacoord(vect);								/* focus point */
						if (status!=0)
							return UU_FAILURE;
		
						um_vctmsc(ent->tfmat[0], -scale, vect);
						um_vcplvc(vect, ent->tfmat[3], vect);
						status = um_feacoord(vect);								/* other focus point */
						if (status!=0)
							return UU_FAILURE;
		
						um_ev4_conic(UM_POINT,(UU_REAL) 0.,ent,tfmat,&evout);
						status = um_feacoord(evout.cp);
						if (status!=0)
							return UU_FAILURE;
		
						um_ev4_conic(UM_POINT,(UU_REAL) .25,ent,tfmat,&evout);
						status = um_feacoord(evout.cp);
						if (status!=0)
							return UU_FAILURE;
		
						um_ev4_conic(UM_POINT,(UU_REAL) .5,ent,tfmat,&evout);
						status = um_feacoord(evout.cp);
						if (status!=0)
							return UU_FAILURE;
		
						um_ev4_conic(UM_POINT,(UU_REAL) .75,ent,tfmat,&evout);
						status = um_feacoord(evout.cp);
						if (status!=0)
							return UU_FAILURE;
		
						break;
			
					default:
						uu_uerror0(UU_FEATERROR,1);
						status = UU_FAILURE;
						break;
					}
				break;
				}
			case UM_PARABOLA:
			case UM_HYPERBOLA:
			default:
				uu_uerror0(UU_FEATERROR, 3);
				status = UU_FAILURE;
				break;
			}
		}
	uu_dexit;
	return(status);
	}
