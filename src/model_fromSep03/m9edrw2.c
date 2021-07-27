/*********************************************************************
**    NAME         :  m9edrw2.c
**       CONTAINS: routines to project UNICAD modeling entities to
**						 a drawing.
**
**			int um_proj_to_drawing
**			int um_proj_geom_to_drawing
**       int um_proj_compcv_to_drawing
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m9edrw2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:12
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "umath.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "class.h"
#include "mdclass.h"
#include "mdattr.h"
#include "mattr.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "msol.h"
#include "mxxx.h"
#include "mdraw.h"
#include "mdebug.h"
#include "mdeval.h"
#include "nccs.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION     : int um_proj_to_drawing(eptr, tfmat, attrptr, 
**									drwmat, vrefpt, vpnorm,
**									keylist, render_solids)
**			Create a list of keys (KEYLIST) of entities to be included
**			in a drawing for the given entity (EPTR, TFMAT, ATTRPTR).
**			The transformation (DRWMAT) will position the viewplane
**			(VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**			scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**				eptr						pointer to entity data
**				tfmat						matrix to position entity in MCS
**				attrptr					pointer to attribute bundle 
**				drwmat					transformation to convert MCS to DCS
**				vrefpt					reference point of viewing plane
**				vpnorm					normal of viewing plane
**				render_solids			UU_TRUE => render solid entities
**       OUTPUT :  
**				keylist					keys of projected entities are pushed
**											onto this list of keys
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_proj_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
		keylist, render_solids)
	struct UM_entitydatabag *eptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;
	UM_transf drwmat;
	UM_coord vrefpt;
	UM_vector vpnorm;
	UU_LIST *keylist;
	UU_LOGICAL render_solids;

	{
	struct UC_entitydatabag *comp;
	struct UM_body_rec *body;
	int status;
	int i;

	uu_denter(UU_MTRC,(us,"um_proj_to_drawing(key=%x)", eptr->key));

	status = UU_FAILURE;
	comp = (struct UC_entitydatabag *)
					uu_malloc(sizeof(struct UC_entitydatabag));
	if (eptr->rel_num == UM_GROUP_REL)
		{
		struct UM_grouper_rec *group;
		struct UC_attributedatabag e_attr;
		UM_transf e_tfmat;

		group = (struct UM_grouper_rec *) eptr;
		for (i=0; i<group->no_member; i++)
			{
			comp->key = group->member[i];
			status = uc_retrieve_data(comp, sizeof(struct UC_entitydatabag));
			if (status == UU_SUCCESS)
				status = uc_retrieve_transf(comp->key, e_tfmat);
			if (status == UU_SUCCESS)
				status = uc_retrieve_attr(comp->key, &e_attr);
			if (status == UU_SUCCESS)
				um_tftmtf(e_tfmat, tfmat, e_tfmat);
			if (status == UU_SUCCESS)
				status = uc_proj_to_drawing(comp, e_tfmat, &e_attr,
								drwmat, vrefpt, vpnorm, keylist);
			}
		}
	else
		{
		switch (uc_super_class(eptr->rel_num))
			{
			case UM_POINT_CLASS:
				if (eptr->rel_num == UM_POINT_REL)
					status = um_proj_geom_to_drawing(eptr, attrptr,
										drwmat, vrefpt, vpnorm, keylist);
				break;
			case UM_SURFACE_CLASS:
				if (eptr->rel_num == UM_POLY_REL)
					status = um_proj_geom_to_drawing(eptr, attrptr,
										drwmat, vrefpt, vpnorm, keylist);
				break;
			case UM_CURVE_CLASS:
					status = um_proj_geom_to_drawing (eptr, attrptr,
										drwmat, vrefpt, vpnorm, keylist);
				   break;
			case UM_SOLID_CLASS:
				if (!render_solids)
					{
					body = (struct UM_body_rec *) eptr;
					for (i=0; i<body->no_edge; i++)
						{
						comp->key = (UU_KEY_ID) (0x80000000 | body->edge[i]);
						status = uc_retrieve_data(comp, sizeof(comp));
						if (status == UU_SUCCESS)
							status = um_proj_geom_to_drawing(comp, attrptr,
												drwmat, vrefpt, vpnorm, keylist);
						}
					}
				status = UU_SUCCESS;
				break;

			default:
				status = UU_FAILURE;
				break;
			}
		}

	uu_free(comp);
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_proj_geom_to_drawing(gptr, gattrptr 
**										drwmat, vrefpt, vpnorm, keylist)
**			Project a geometric entity (GPTR), which is assumed to be 
**			positioned in the modeling coordinate system (MCS), onto a
**			drawing, display the newly created entity using the
**			specified attribute bundle, and push the new entity key
**			onto the KEYLIST. The transformation matix DRWMAT
**			transforms the viewing plane (VREFPT, VPNORM) in MCS to
**			the XY  plane in the drawing coordinate system (DCS),
**			appropriately scaled.
**    PARAMETERS   
**       INPUT  : 
**				gptr						entity to project onto drawing
**				gattrptr					attribute bundle of entity
**				drwmat					transformation to convert MCS to DCS
**				vrefpt					reference point of viewing plane
**				vpnorm					normal of viewing plane
**       OUTPUT :  
**				keylist					list of keys of projected entities
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_proj_geom_to_drawing(gptr, gattrptr, drwmat, vrefpt, vpnorm, keylist)
struct UM_entitydatabag *gptr;
struct UM_attrdata_rec *gattrptr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
UU_LIST *keylist;
{
	UU_LIST cvpts;
	struct UM_entitydatabag *projgeom = UU_NULL;
	UU_KEY_ID newkey;
	UU_REAL savewidth;
	UM_vector linewidth;
	int status,savelabel, init = 0;

	uu_denter(UU_MTRC,(us,"um_proj_geom_to_drawing(key=%x)", gptr->key));
/* 
...temporarily, set line width in attribute bundle
*/
	um_xyztovc(gattrptr->line_width, (UU_REAL) 0.0, (UU_REAL) 0.0, linewidth);
	um_vctmtf(linewidth, drwmat, linewidth);
	savewidth = gattrptr->line_width;
	gattrptr->line_width = um_mag(linewidth);
/*
.....Save the label display status of the input entity .....Bobby  -  2/24/94
*/
	savelabel = gattrptr->label_on;

/*
... aak 09-apr-1998 : changes to include CVonSF's and composite curves
*/
	newkey = 0;
	status = UU_FAILURE;

	switch (gptr->rel_num)
	{
		case UM_LINE_REL:
		case UM_RBSPLCRV_REL:
		case UM_POLY_REL:
	   case UM_POINT_REL:
			um_alloc_eq_geom (gptr,&projgeom);
			if (!projgeom) return (UU_FAILURE);
			status = um_proj_geom_to_plane (gptr, vrefpt, vpnorm, projgeom);
			break;
		case UM_CIRCLE_REL:
		case UM_CONIC_REL:
			um_alloc_eq_curve (gptr,&projgeom);
			if (!projgeom) return (UU_FAILURE);
			status = um_proj_geom_to_plane (gptr, vrefpt, vpnorm, projgeom);
			break;
		case UM_UVCVONSF_REL:
			projgeom = (struct UM_entitydatabag *) uu_toolmalloc (sizeof (struct UM_polyline_rec));
			if (!projgeom) return (UU_FAILURE);
			init = 1;
			status = ncl_proj_cvonsf_to_plane (gptr, vrefpt, vpnorm, projgeom,&cvpts);
			break;
		case UM_COMPCRV_REL:
			status = um_proj_compcv_to_drawing (gptr, gattrptr, drwmat, vrefpt, vpnorm, keylist);
			goto Done;
		default:
			goto Done;
	}

	if (status == UU_SUCCESS)
	{
		ncl_proj_label_to_drw (gptr,projgeom,gattrptr,drwmat,vrefpt, vpnorm);
		status = ncl_create_geom_drw (projgeom, UM_DEFAULT_TF, gattrptr);
		uc_transform (projgeom, drwmat, UU_TRUE);
		uc_retrieve_data (projgeom, sizeof(struct UM_entitydatabag));

/*
..... aak 20-apr-1998: commented this out; function um_duplicate 
..... treats only lines; a line can be a component of a composite curve.
..... In this case, should keep its "duplicate"
.....
		if (um_duplicate (projgeom) == 0) 
			uc_delete (projgeom->key);
		else
*/
			newkey = projgeom->key;
	}
	status = UU_SUCCESS;
/* 
.....return keys of created entities
*/
	if (newkey != 0)
	{
		ur_update_displayable (projgeom->key, UM_DISPLAYABLE);
		uc_display (projgeom);
		uu_list_push (keylist, &newkey);
	}

	if (!projgeom) uu_toolfree (projgeom);
	if (init) uu_list_free (&cvpts);

/*
..... reset line width
.....Restore label display status
.....Bobby
*/
Done:;
	gattrptr->line_width = savewidth;
	gattrptr->label_on   = savelabel;

	uu_dexit;
	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int um_proj_compcv_to_drawing(eptr, attrptr, 
**									drwmat, vrefpt, vpnorm,keylist)
**			Create a list of keys (KEYLIST) of entities to be included
**			in a drawing for composite curve entity (EPTR, TFMAT, ATTRPTR).
**			The transformation (DRWMAT) will position the viewplane
**			(VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**			scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**				eptr						pointer to composite curve
**				attrptr					pointer to attribute bundle 
**				drwmat					transformation to convert MCS to DCS
**				vrefpt					reference point of viewing plane
**				vpnorm					normal of viewing plane
**				render_solids			UU_TRUE => render solid entities
**       OUTPUT :  
**				keylist					keys of projected entities are pushed
**											onto this list of keys
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_proj_compcv_to_drawing (eptr, attrptr, 
                               drwmat, vrefpt, vpnorm, keylist)
struct UM_compcrv_rec *eptr;
struct UM_attrdata_rec *attrptr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
UU_LIST *keylist;
{
/*
.....use bigger structure
.....Yurong 11/19/98
*/
/*
	struct UC_entitydatabag cv;
	struct UM_evcrvout evout;
*/
	struct NCL_fixed_databag cv;
	UM_transf tfmat;
	struct NCL_fixed_databag *pcv;
	int status,ncv,rev,savelabel,i;

	savelabel = attrptr->label_on;

	pcv = (struct NCL_fixed_databag *) &cv;
	ncl_compcrv_getnents (eptr, &ncv);

	uc_retrieve_transf (eptr->key,tfmat);

	for (i=0; i<ncv; i++)
	{
/*
......function ncl_compcrv_getelm third parameter is returned 
......a whole structure, not a key
......Yurong changed
*/
/*
		status = ncl_compcrv_getelm (eptr, i, &cv.key, &rev);

		if (status == UU_SUCCESS)
			status = uc_retrieve_data (&cv, sizeof(cv));
*/
		status = ncl_compcrv_getelm (eptr, i, &cv, &rev);

		if (i == 0 && status == UU_SUCCESS)
		{
			ncl_get_label (eptr,pcv->label);

/*
.....check if label display is on and altered
*/
			if (ncl_get_label_on(attrptr->label_on) && 
					ncl_get_label_alter(attrptr->label_on))
				ncl_retrieve_labloc (eptr, attrptr->label_on, pcv->labloc);
			else
			{
				ncl_default_labloc (eptr,tfmat,pcv->labloc);
			}
/*
.....set label display to on  and altered
*/
			ncl_set_label_on(&attrptr->label_on,1);
			ncl_set_label_alter(&attrptr->label_on,1);
		}

		status = uc_proj_to_drawing (pcv, tfmat,attrptr,
							 drwmat, vrefpt, vpnorm, keylist,UU_FALSE);
/*
.....set label display to off  and not altered
*/
		ncl_set_label_on(&attrptr->label_on,0);
		ncl_set_label_alter(&attrptr->label_on,0);
	}

	attrptr->label_on = savelabel;

	return (status);
}
