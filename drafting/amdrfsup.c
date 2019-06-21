/*********************************************************************
**    NAME         :  amdrfsup.c
**       CONTAINS:
**			um_p_transf
**			ua_text_size
**			ua_text_fontnum
**			ua_text_precision
**			ua_text_spacing
**       ua_ilnconic
**       ua_tan_conic
**			int ua_proj_to_drawing(eptr, tfmat, attrptr, 
**						drwmat, vrefpt, vpnorm,
**						keylist, render_solids)
**       ua_linear_modal(type)
**		   ua_note_msg (type)
**		   ua_get_endpts 
**			ua_regen_endpts
**			ua_pt_on_curve
**			ua_para_on_curve
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       amdrfsup.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:36
*********************************************************************/
#include "umath.h"
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "class.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mdcoord.h"
#include "mdmatrix.h"
#include "mcrv.h"
#include "modef.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mdeval.h"
#include "mdattr.h"
#include "mattr.h"
#include "msol.h"
#include "mxxx.h"
#include "mdraw.h"
#include "mdebug.h"
#include "adraft.h"
#include "adrfcom.h"

/*********************************************************************
**    E_FUNCTION     : um_p_transf(str, tf)
**       Print a transformation matrix using um_pscroll utility.
**    PARAMETERS   
**       INPUT  : 
**          str					title to print
**				tf						transformation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_p_transf(str, tf)
	char *str;
	UU_REAL tf[4][3];

	{
	int i;
	char um_sbuf[256];

	uu_denter(UU_MTRC,(us,"um_p_transf(%x,%x)",str,tf));
	um_pscroll("");
	sprintf(um_sbuf,"%s",str);
	um_pscroll(um_sbuf);
	for (i=0; i<4; i++)
		{
		sprintf(um_sbuf,"tf[%d]=(%f,%f,%f)",i,tf[i][0],tf[i][1],tf[i][2]);
		um_pscroll(um_sbuf);
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_text_size(size)
**      Return size of drafting text.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  
**          size						   drafting text size
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_text_size(size)
	UU_REAL *size;
	{
	UU_REAL scale;
	um_get_drwscale(&scale);
	*size = UA_char_size_note/scale;
	}
/*********************************************************************
**    E_FUNCTION     : ua_text_fontnum(fontnum)
**      Return font number for drafting text.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  
**          font						   drafting font number
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_text_fontnum(fontnum)
	int *fontnum;
	{
	*fontnum = UA_txt_fontnum_note;
	}
/*********************************************************************
**    E_FUNCTION     : ua_text_precision(precision)
**      Return text precision for drafting text.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  
**          precision						text precision
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_text_precision(precision)
	Gtxprec *precision;
	{
	*precision = UA_txt_precision_note;
	}
/*********************************************************************
**    E_FUNCTION     : ua_text_spacing(expansion, spacing)
**      Return character spacing information.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  
**          expansion							expansion factor
**          spacing								character spacing ratio
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_text_spacing(expansion, spacing)
	UU_REAL *expansion;
	UU_REAL *spacing;
	{
	*expansion = UA_char_expansion_note;
	*spacing = UA_char_space_note;
	}
/*********************************************************************
**    E_FUNCTION     : int ua_ilnconic(spt, normal, key, nint, int_pt)
**       Interface routine between SAL and modeling to intersect a line
**			with a conic.
**    PARAMETERS   
**       INPUT  : 
**				spt							point on the line
**				normal						unit vector along line
**          key							key of entity in UNIBASE
**       OUTPUT :  
**          nint							number of intersection points
**          int_pt						array of intersection points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_ilnconic(spt, normal, key, nint, int_pt)
	UU_REAL spt[3];
	UU_REAL normal[3];
	UU_KEY_ID  key;
	int 		*nint;
	UU_REAL int_pt[2][3];

	{
	struct UM_crvdatabag e;
	int rel_num;
	int status;
	int num,multi;

	uu_denter(UU_MTRC,(us,"ua_ilnconic(%d,,,)",key));
	e.key = key;
	um_get_all_geom(&e, sizeof(struct UM_crvdatabag));
	if (e.rel_num != UM_CONIC_REL) status = -1;
	else
		{
		um_ilnconic(spt, normal, &e, &num, int_pt, &multi);
		*nint = num;
		status = 0;
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int ua_tan_to_conic(unit_vec, conic_key, nint, int_pt)
**       Find a point on a conic the tangent is parallel to the give vector.
**    PARAMETERS   
**       INPUT  : 
**				unit_vec						unit vector 
**          key							key of entity in UNIBASE
**       OUTPUT :  
**          nint                    number of points found
**          int_pt						array of points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_tan_to_conic(unit_vec, key, nint, int_pt)
	UU_REAL unit_vec[3];
	UU_KEY_ID  key;
	int        *nint;
	UU_REAL int_pt[2][3];

	{
	struct UM_crvdatabag e;
	struct UM_conic_rec  *c;
	struct UM_evcrvout   evout;

	int rel_num;
	int status;
	int num,multi;
	UU_REAL u, t, sign, dot, u0, step, t_p, t_int, vec0[3], vec1[3], dir_vec[3];
	UU_REAL tfmat[4][3];
	UU_LOGICAL found;

	uu_denter(UU_MTRC,(us,"ua_tan_to_conic(%d,,,)",key));
	e.key = key;
	um_get_all_geom(&e, sizeof(struct UM_crvdatabag));
	*nint = 0;
	if (e.rel_num != UM_CONIC_REL) status = -1;
	else
		{
		c = (struct UM_conic_rec *) &e;
		um_tftotf(UM_idmat, tfmat);
		status = 0;
		u0 = 0.0;
		step = .1;
		found = UU_FALSE;
		uc_init_evcrvout(c, &evout);
		while(step > 0.001)
			{
			for(u=u0;u<(u0+10*step);(u=u+step))
				{
				uc_evcrv(UM_FRSTDERIV, u, c, UM_DEFAULT_TF, &evout);
				um_unitvc(evout.dcdu, dir_vec);
				if(u == u0)
					{
					dot = um_dot(unit_vec, dir_vec);

					if(fabs(fabs(dot) - 1.0) <= 1.0e-5)
						{
						found = UU_TRUE;
						goto end_loop;
						}
					um_cross(unit_vec, dir_vec, vec0);
					}
				else
					{
					um_cross(unit_vec, dir_vec, vec1);
					dot = um_dot(vec0,vec1);
					if( dot < 0.0)
						{
						found = UU_TRUE;
						u0 = u - step;
						step = step/10.0;
						goto lp1;
						}
					}
				}
	lp1:
			if(!found) 
				{
				status = -1;
				goto end_loop;
				}
			}
		}
end_loop:
	if(found)
		{
		int_pt[0][0] = evout.cp[0];
		int_pt[0][1] = evout.cp[1];
		int_pt[0][2] = evout.cp[2];
		um_conic_parameter(u0,c,&t_p,&t_int);
		t_p = t_p + 2;
		if(t_p > 2) t_p -=4;
		umi_cc4_ttou(t_p,c,&u0);
		uc_evcrv(UM_FRSTDERIV, u0, c, UM_DEFAULT_TF, &evout);
		int_pt[1][0] = evout.cp[0];
		int_pt[1][1] = evout.cp[1];
		int_pt[1][2] = evout.cp[2];
		*nint = 2;
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int ua_proj_to_drawing(eptr, tfmat, attrptr, 
**									drwmat, vrefpt, vpnorm,
**									keylist, render_solids)
**			Create a list of keys (KEYLIST) of entities to be included
**			in a drawing for the given drafting entity (EPTR, TFMAT, ATTRPTR).
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
int
ua_proj_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
		keylist, render_solids)
	struct UC_entitydatabag *eptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;
	UM_transf drwmat;
	UM_coord vrefpt;
	UM_vector vpnorm;
	UU_LIST *keylist;
	UU_LOGICAL render_solids;

	{
	struct UC_entitydatabag *projgeom;
	struct UA_generic_draft *drf;
	UU_KEY_ID drw_view, key;
	int status, i, num;

	uu_denter(UU_MTRC,(us,"ua_proj_to_drawing(key=%x)", eptr->key));

	projgeom = (struct UC_entitydatabag *)
						uu_malloc(sizeof(struct UC_entitydatabag));
	drf = (struct UA_generic_draft *)projgeom;

	status = UU_SUCCESS;

	/* copy and transform the drafting entity in UNIBASE */
	uc_copy(eptr, projgeom, sizeof(struct UC_entitydatabag));
	uc_transform(projgeom, drwmat, UU_TRUE);

	/* break associativity */
	if(drf->etype != UA_PL_DIM && drf->etype != UA_CROSSHATCH)
		{
		for(i=0;i<drf->asso_blk_use;i++)
					drf->asso_blk[i].key = 0;
		drf->asso_blk_use = 0;
		key = drf->key;
		ua_update_entity(key, drf);
		}

	/* update the view the new drafting entity is visible in, make
		sure it is displayable in this view, and display it */
	drw_view = ur_get_drwmdl_drwvw();
	ur_update_view_key(projgeom->key, drw_view);
	ur_update_displayable(projgeom->key, UM_DISPLAYABLE);
	uc_display(projgeom);

	/* push the new entity onto the list of entities to include in
		the drawing */
	uu_list_push(keylist, &projgeom->key);

	/* finally, free temporarly allocated space */
	uu_free(projgeom);

	uu_dexit;
	return (status);
	}


/*********************************************************************
**	E_FUNCTION:	 ua_linear_disp (type)
**
**	PURPOSE:
**			Switch between linear dimension types depending on MODAL setting
**
**    PARAMETERS   
**       INPUT  : 
**			  	type		-	dimension sub-type (vert., horz, parallel)
**
**       OUTPUT :  
**				none
**
**    RETURNS      :	
**				none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : 
**				none
**
*********************************************************************/

static char *msg1 = {"linear type modal set to STANDARD"};
static char *msg2 = {"linear type modal set to CHAINED"};
static char *msg3 = {"linear type modal set to BASELINE"};
static char *msg4 = {"linear type modal set to REPETITIVE"};

extern UA_linear_modal;

int ua_linear_disp(type)
int	type;
	{
	switch(UA_linear_modal)
		{
		case 0:
			ud_prmerr(msg1);
         ua_linear_dims(type);
			break;
		case 1:
			ud_prmerr(msg2);
 			ua_chain_dims(type);
			break;
		case 2:
			ud_prmerr(msg3);
 			ua_baseln(type);
			break;
		case 3:
			ud_prmerr(msg4);
 			ua_repeat_dims(type);
			break;
		}
	}
/*********************************************************************
**	E_FUNCTION:	 ua_note_msg (type)
**
**	PURPOSE:
**			Ouput note MODAL setting
**
**    PARAMETERS   
**       INPUT  : 
**			  	type		-	keyboard logical flag
**
**       OUTPUT :  
**				none
**
**    RETURNS      :	
**				none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : 
**				none
**
*********************************************************************/

static char *msg5 = {"Note input set to KEYBOARD"};
static char *msg6 = {"Note input set to FILE INPUT"};


int ua_note_msg(type)
UU_LOGICAL	type;
	{
	if(type) ud_prmerr(msg5);
	else ud_prmerr(msg6);
	}

/*********************************************************************
**    E_FUNCTION    : int ua_get_endpts(key, pt1, pt2)
**      Get endpts of entity specified by the key
**    PARAMETERS   
**       INPUT  : 
**				key							entity key
**       OUTPUT :  
**				pt1, pt2						entity endpoints
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_get_endpts(key, pt1, pt2)
	UU_KEY_ID  key;
	UM_coord pt1, pt2;

	{
	struct UC_entitydatabag e;				/* data for picked entity */
	UM_transf tfmat;						/* transformation matrix */
	int status;

	uu_denter(UU_STRC,(us,"ua_get_endpts(key = %d)", key));

	e.key =  key;
	um_retrieve_data_relnum(e.key, &e.rel_num);
	switch (uc_super_class(e.rel_num))
		{
		case UM_POINT_CLASS:
		case UM_CURVE_CLASS:
			status = uc_retrieve_data(&e, sizeof(e));
			if (status != UU_SUCCESS) break;
			status = uc_retrieve_transf(e.key, tfmat);
			if (status != UU_SUCCESS) break;
			switch (uc_super_class(e.rel_num))
				{
				case  UM_POINT_CLASS:
					{
					struct UM_point_rec *ptr;
					ptr = (struct UM_point_rec * ) &e;
					um_tf1_tranfpt(ptr, tfmat, UU_FALSE);
					um_vctovc(ptr->pt,pt1);
					um_vctovc(ptr->pt,pt2);
					}
					break;
				case UM_CURVE_CLASS:
					{
					status = um_get_endpts(&e, tfmat, pt1, pt2);
					}
					break;
				default:
					status = UU_FAILURE;
					um_vctovc(UM_zerovec, pt1);
					um_vctovc(UM_zerovec, pt2);
					break;
				}
			if (status != UU_SUCCESS) break;
			status = UU_SUCCESS;
			break;
		default:
			status = UU_FAILURE;
			break;
		}
	uu_dexitstatus("ua_get_endpts", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION    : int ua_regen_endpts(edrf, blk, close, distant)
**      Re-evaluate contents of association block
**    PARAMETERS   
**       INPUT  : 
**				key							entity key
**				blk							asso blk number
**       OUTPUT :  
**				close, distant
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_regen_endpts(edrf, blk, close, distant)
	struct UA_generic_draft	*edrf;
	int blk;
	UM_coord close, distant;

	{
	struct UA_generic_draft	e;
	int status, i, j, k;
	UM_coord pt1, pt2, end_pts[50], tmp_pts[10];

	uu_denter(UU_STRC,(us,"ua_regen_endpts(blk = %d)", blk));

	e.key =  edrf->asso_blk[blk].key;
	um_retrieve_data_relnum(e.key, &e.rel_num);
	if(e.rel_num == UA_LINEAR_DIM) 
		{
		status = uc_retrieve_data(&e, sizeof(e));
		for(i=0;i<e.line_blk_use;i++)
			{
			for(k=0;k<e.line_blk[i].num_pts;k++)
				um_vctovc(e.line_blk[i].line_seg[k], end_pts[k]);
			j = um_clospnt(e.line_blk[i].num_pts, end_pts,
													edrf->asso_blk[blk].location);
			um_vctovc(end_pts[j], tmp_pts[i]);
			}
		if(e.line_blk_use == 1) 
			um_vctovc(end_pts[1], close);
		else
			{
			for(i=0;i<e.line_blk_use;i++) um_vctovc(end_pts[i], tmp_pts[i]);
			j = um_clospnt(e.line_blk_use, tmp_pts, edrf->asso_blk[blk].location);
			um_vctovc(tmp_pts[j], close);
			}
		if(e.etype == UA_CENTERLINE)
			{
			switch(e.subtype)
				{
				case 1:
				case 2:
					for(i=0;i<e.asso_blk_use;i++)
						{
						um_vctovc(e.asso_blk[i].location, end_pts[i]);
						}
					j = um_clospnt(e.asso_blk_use, end_pts, close);
					um_vctovc(end_pts[j], distant);
					break;
				case 3:
				case 4:
					um_vctovc(e.arc_blk[0].center_pt,distant);
					break;
				}
			}
		else
			{
			um_vctovc(close, distant);
			}
		}
	else
		{
		ua_get_endpts(edrf->asso_blk[blk].key, pt1, pt2);
		j = um_closept(pt1, pt2, edrf->asso_blk[blk].location);
		if(j == 1)
			{
			um_vctovc(pt1, close);
			um_vctovc(pt2, distant);
			}
		else
			{
			um_vctovc(pt2, close);
			um_vctovc(pt1, distant);
			}
		}
	uu_dexitstatus("ua_get_endpts", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION    : int ua_pt_on_curve(key, uu, pt)
**      Get a curve point at at specific parameter value
**    PARAMETERS   
**       INPUT  : 
**				key							entity key
**				uu								parameter value
**       OUTPUT :  
**				pt								point on curve
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_pt_on_curve(key, uu, pt)
	UU_KEY_ID  key;
	UU_REAL    uu;
	UM_coord  pt;

	{
	struct UC_entitydatabag e;				/* data for picked entity */
	struct UM_evcrvout   evout;
	UM_transf tfmat;						/* transformation matrix */
	int status;

	uu_denter(UU_STRC,(us,"ua_pt_on_curve(key = %d)", key));

	e.key =  key;
	um_retrieve_data_relnum(e.key, &e.rel_num);
	switch (uc_super_class(e.rel_num))
		{
		case UM_POINT_CLASS:
		case UM_CURVE_CLASS:
			status = uc_retrieve_data(&e, sizeof(e));
			if (status != UU_SUCCESS) break;
			status = uc_retrieve_transf(e.key, tfmat);
			if (status != UU_SUCCESS) break;
			switch (uc_super_class(e.rel_num))
				{
				case  UM_POINT_CLASS:
					{
					struct UM_point_rec *ptr;
					ptr = (struct UM_point_rec * ) &e;
					um_tf1_tranfpt(ptr, tfmat, UU_FALSE);
					um_vctovc(ptr->pt,pt);
					}
					break;
				case UM_CURVE_CLASS:
					uc_init_evcrvout(&e, &evout);
					uc_evcrv(UM_FRSTDERIV, uu, &e, UM_DEFAULT_TF, &evout);
					um_vctovc(evout.cp, pt);
					break;
				default:
					status = UU_FAILURE;
					um_vctovc(UM_zerovec, pt);
					break;
				}
			if (status != UU_SUCCESS) break;
			status = UU_SUCCESS;
			break;
		default:
			status = UU_FAILURE;
			break;
		}
	uu_dexitstatus("ua_pt_on_curve", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION    : int ua_para_on_curve(key, pt, uu)
**      Get curve parameter value at point closest to pt.
**    PARAMETERS   
**       INPUT  : 
**				key							entity key
**				pt								point on curve
**       OUTPUT :  
**				uu								parameter value
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_para_on_curve(key, pt, uu)
	UU_KEY_ID  key;
	UM_coord  pt;
	UU_REAL    *uu;

	{
	struct UC_entitydatabag e;				/* data for picked entity */
	struct UM_evcrvout   evout;
	UM_transf tfmat;						/* transformation matrix */
	int status;
	UU_REAL distance;

	uu_denter(UU_STRC,(us,"ua_para_on_curve(key = %d)", key));

	e.key =  key;
	um_retrieve_data_relnum(e.key, &e.rel_num);
	switch (uc_super_class(e.rel_num))
		{
		case UM_POINT_CLASS:
		case UM_CURVE_CLASS:
			status = uc_retrieve_data(&e, sizeof(e));
			if (status != UU_SUCCESS) break;
			status = uc_retrieve_transf(e.key, tfmat);
			if (status != UU_SUCCESS) break;
			switch (uc_super_class(e.rel_num))
				{
				case  UM_POINT_CLASS:
					*uu = 1.0;
					break;
				case UM_CURVE_CLASS:
					um_cctou(&e, tfmat, pt, uu, &distance);
					break;
				default:
					status = UU_FAILURE;
					*uu = 0.0;
					break;
				}
			if (status != UU_SUCCESS) break;
			status = UU_SUCCESS;
			break;
		default:
			status = UU_FAILURE;
			break;
		}
	uu_dexitstatus("ua_para_on_curve", status);
	return (status);
	}
