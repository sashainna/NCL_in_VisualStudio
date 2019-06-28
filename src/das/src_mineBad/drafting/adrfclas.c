/*********************************************************************
**    NAME         :  adrfclas.c
**       CONTAINS:
**       Method routines for CLASS DRAFTING
**				int ua_init_drafting_rel ()
**				int ua_display_drafting ()
**				int ua_draw_drafting ()
**				int ua_print_drafting ()
**				int ua_delete_drafting ()
**				int ua_retrieve_drafting ()
**				int ua_retrieve_transf ()
**				int ua_retrieve_attr ()
**				int ua_copy_drafting (entity1, entity2)
**					 ua_create_entity (entity, key)
**				  	 ua_transform_drafting (entity, tf, upd)
**				int ua_translate_drafting (entity, offset)
**				int ua_rotate_drafting (entity, pt, dir, angle, rotmat)
**					 ua_set_drafting_view()
**					 ua_reset_drafting_view()
**				int ua_query
**              int ua_text_query
**				int ua_scale_drafting
**				int ua_mirror_drafting
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       adrfclas.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:33
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"				/* for error subsystem */
#include "class.h"
#include "mdcoord.h"
#include "mdebug.h"
#include "umath.h"
#include "mdattr.h"
#include "mdrel.h"
#include "mattr.h"
#include "modef.h"
#include "adraft.h"
#include "adrf.h"
#include "adrfcom.h"
#include "ulist.h"
#include "jquery.h"  /* query macros, etc. */
#include "atext.h"  

/* some GLOBAL drafting modals */

UU_KEY_ID UA_drafting_view = {0};      /* default drafting view */
UU_LOGICAL UA_note_keyboard = UU_TRUE; /* note source default keyboard */
int 	  	  UA_linear_modal  = 0;       /* linear form deafult standard */

/*********************************************************************
**    E_FUNCTION :  void ua_init_drafting_rel ()
**       Initialize the DRAFTING subsystem relations.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_init_drafting_rel()
	{
	ua_init_relations();
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION :  int ua_display_drafting (eptr)
**       Display a drafting entity
**    PARAMETERS   
**       INPUT  : 
**          eptr							drafting entity record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_display_drafting(eptr)
struct UC_entitydatabag *eptr;
{
	int ua_disp1_drafting();

	uu_denter(UU_STRC,(us,"ua_display_drafting(%x), rel_num", eptr,
		eptr->rel_num));
	switch (eptr->rel_num)
	{
		case UA_LINEAR_DIMS_REL:
			uv_disp_entity(eptr);
			break;
		case UA_HATCHING_REL:
			uu_denter2(UU_STRC,(us,"uc_disp called for xhatch"));
			uu_dexit;	
			break;	/* do nothing - handled in previous case */
	}
	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION :  int ua_draw_drafting (eptr, tfmat, attrptr)
**       Draw a drafting entity
**    PARAMETERS   
**       INPUT  : 
**          eptr							drafting entity record
**				tfmat							transformation
**				attrptr						attribute pointer
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_draw_drafting(eptr, tfmat, attrptr)
	struct UC_entitydatabag *eptr;
	UM_transf					tfmat;
	struct UM_attdata_rec	*attrptr;
	{
		char us[100];

		um_set_disp_attr(attrptr);
		switch (eptr->rel_num)
		{
			case UA_LINEAR_DIMS_REL:
				ua_disp1_drafting( eptr, tfmat);
				break;
			case UA_HATCHING_REL:
				uu_denter2(UU_STRC,(us,"uc_draw called for xhatch"));
				uu_dexit;
				break;	/* do nothing - handled in previous case */
		}
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION :  void ua_print_drafting ()
**       Print drafting entity
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_print_drafting(eptr)
	struct UC_entitydatabag *eptr;
	{
	ua_dump_set(0xffff);
	ua_dump_ent(3,eptr);
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION :  void ua_delete_drafting (key)
**       Delete drafting entity
**    PARAMETERS   
**       INPUT  : 
**          key							entity key
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_delete_drafting(key)
	UU_KEY_ID key;
	{
	struct UA_generic_draft e;
	UU_KEY_ID	hatch_key;
	int status, i;


	uu_denter(UU_STRC,(us,"entering ua_delete_drafting(%d)",key));
	e.key = key;
	e.rel_num = UA_LINEAR_DIMS_REL;
	ua_retrieve_data(&e,sizeof(struct UA_generic_draft));
	status = UU_SUCCESS;
	ur_delete_all(key);

	if (e.etype == UA_HATCHING_REL)
		{
		hatch_key = e.asso_blk[e.asso_blk_use -1].key;
		status = uc_delete(hatch_key);
		}

	if(e.etype == UA_PL_DIM)
		{
		for(i=0;i<e.asso_blk_use;i++)
			{
			ua_delete_text(e.asso_blk[i].key);
			}
		}

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  void ua_retrieve_data (eptr, entsize)
**       Entity data record
**    PARAMETERS   
**       INPUT  : 
**          eptr							entity record
**          entsize						record size
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_retrieve_data(eptr, entsize)
struct UC_entitydatabag *eptr;
int entsize;
{
	int status;
	char us[100];

	switch (eptr->rel_num)
	{
		case UA_LINEAR_DIMS_REL:
			status = ua_get_entity(eptr, entsize);
			break;
		case UA_HATCHING_REL:
			uu_denter2(UU_STRC,(us,"uc_retrieve called on xhatch"));
			uu_dexit;
			status = UU_SUCCESS;
			break;	/* do nothing */
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  void ua_retrieve_transf (key, tfmat)
**       Retrieve drafting transformation
**    PARAMETERS   
**       INPUT  : 
**          key							entity key
**          tfmat							transformation entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_retrieve_transf(key, tfmat)
	UU_KEY_ID key;
	UM_transf tfmat;
	{
	int status;

	status = um_get_transformation(key, tfmat);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int ua_retrieve_attr (key, attr)
**       Retrieve drafting attribute
**    PARAMETERS   
**       INPUT  : 
**          key							entity key
**          attr							attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_retrieve_attr(key, attr)
	UU_KEY_ID key;
	struct UC_attributedatabag *attr;
	{
	int status;

	status = um_get_disp_attr(key, attr);
	uu_denter2(UU_STRC,(us,"ua_retrieve_attr: key %d disp %d select %d",
		key,attr->displayable, attr->selectable));
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  ua_copy_drafting (entity1, entity2)
**       Copy a drafting entity.
**    PARAMETERS   
**       INPUT  : 
**          entity1 - drafting entity to be copied
**       OUTPUT :  
**          entity2  - copy of the drafting entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_copy_drafting(e1, e2)
	struct UA_generic_draft	*e1;	/*	entity to copy */
	struct UA_generic_draft	*e2;	/* db copy */

	{
	int status, i;
	struct UA_draft_rec		e;
	UU_KEY_ID	key1, key2, view_key;
	struct UM_attrdata_rec attrptr;
	struct   UA_txt_rec			*note;			/* text record */
	struct   UA_txt_rec			*note1;			/* text record */

	uu_denter(UU_MTRC,(us,"ua_copy_entity(%x,)",e1));

	switch (e1->rel_num)
	{
		case UA_HATCHING_REL:
			uu_denter2(UU_STRC,(us,"uc_copy called for xhatch"));
			uu_dexit;	
			break;	/* do nothing - handled in following case */

		case UA_LINEAR_DIMS_REL:
			/* setup target entity record */
			e.key = 0;
			e.rel_num = e1->rel_num;
			key1 = 0;

			if (e1->etype == UA_HATCHING_REL)
				{
				/* get keyid of Unibase copy of hatchline_rec */
				key1 = e1->asso_blk[e1->asso_blk_use - 1].key;
				ua_copy_hatch_rec(key1, &key2);

				/* put keyid of hatchline_rec copy in draft_rec copy */
				/*
				p = e.assoblk + e.no_assoblk - 1;
				(*p).asso_key = key2;
				*/
				e1->asso_blk[e1->asso_blk_use - 1].key = key2;
				}
			ur_setup_data(e1->rel_num,&e,sizeof(struct UA_draft_rec));
			ua_sal_to_uni(e1,&e);

			if(e1->etype == UA_PL_DIM)
				{
				note  = (struct UA_txt_rec *) uu_malloc(sizeof(struct UA_txt_rec));
				note1 = (struct UA_txt_rec *) uu_malloc(sizeof(struct UA_txt_rec));
				for(i=0;i<e1->asso_blk_use;i++)
					{
					note->key = e1->asso_blk[i].key;
					if(ua_get_text(note, sizeof(struct UA_txt_rec)) == UU_SUCCESS)
						{
						ua_cp_text(note, note1, sizeof(struct UA_txt_rec));
						e.assoblk[i].asso_key = note1->key;
						}
					}
				uu_free(note);
				uu_free(note1);
				}
			status = ur_create_data(&e);
			uc_retrieve_attr(e1->key, &attrptr);
			attrptr.key = e.key;
			status = ur_update_attr(&attrptr);
			ur_retrieve_view_key(e1->key, &view_key);
			status = ur_update_view_key(e.key, view_key);
			if(key1 != 0 ) e1->asso_blk[e1->asso_blk_use - 1].key = key1;

			/* copy record back to sal format */

			ua_uni_to_sal(&e,e2);
			break;
	}	/* end of switch */
	uu_dexit;

	status = UU_SUCCESS;
	return(status);
}


/*********************************************************************
**    E_FUNCTION :  ua_create_entity (entity, key)
**       Creates the entity given in the generic entity format in unibase.
**    PARAMETERS   
**       INPUT  : 
**          entity - generic entity to save
**       OUTPUT :  
**          key   - entity key
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_create_entity(entity, key)
	struct UA_generic_draft	*entity;	/*	entity to save			*/
	unsigned long		*key;	/* unibase entity key	*/

	{
	struct UA_draft_rec e;
	struct UM_transf_rec tran;
	int status;

	uu_denter(UU_MTRC,(us,"ua_create_entity(%x,)",entity));

	/* TEMP !! - can't switch on rel_num here because this call isn't made
	**	thru class dispatcher, rel_num hasn't been assigned yet 
	*/

	/* define the entity */
	e.key = 0;
	e.rel_num = UA_LINEAR_DIMS_REL;
	entity->key = 0;
	entity->rel_num =  UA_LINEAR_DIMS_REL;
		
	ur_setup_data(UA_LINEAR_DIMS_REL,&e,sizeof(struct UA_draft_rec));
	ua_sal_to_uni(entity,&e);
	status = ur_create_data(&e);

	/* UM_attr.key = e.key; */
	ur_put_attrmdl_key(e.key);
	ur_put_attrmdl_rel_num (UM_ATTR_REL);
	status = ur_update_attr(&UM_attrmdl);

	/* define default transformation */

	tran.key = e.key;
	tran.rel_num = UM_TRANSFORM_REL;
	um_tftotf(UM_idmat,tran.tfmat);
	status = ur_update_transf(&tran);

	if( entity->dims_display == 0 )
		{
		status = ur_update_view_key(e.key, UA_drafting_view);
		}
	else
		{
		status = ur_update_view_key(e.key, 0);
		}
	
	entity->key = e.key;
	entity->rel_num = e.rel_num;
	*key = e.key;

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION :  ua_transform_drafting (entity, tf, upd)
**       Transform a drafting entity
**    PARAMETERS   
**       INPUT  : 
**          entity			entity record
**          tf					transformation matrix
**          upd				update flag (TRUE - update Unibase)
**       OUTPUT :  
**          entity			modified entity record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_transform_drafting(e, tf, upd)
struct UA_generic_draft	*e;
UU_REAL						tf[4][3];
UU_LOGICAL					upd;

{
	int i, j;
	UU_KEY_ID key;
	UU_REAL scal,pt1[3],vtx[3],vty[3],vtz[3],pt3[3],zerov[3];
	struct   UA_txt_rec			note;			/* text record */

	uu_denter(UU_MTRC,(us,"ua_trans_draft(%x)",e));

	/* check transformation */

	if(tf == UM_DEFAULT_TF) goto fexit;

	switch (e->rel_num)
	{
		case UA_HATCHING_REL:
			uu_denter2(UU_STRC,(us,"uc_transform called for xhatch"));
			uu_dexit;	
			break;	/* do nothing - handled in following case */

		case UA_LINEAR_DIMS_REL:
		/* compute scale factor */

			zerov[0] = zerov[1] = zerov[2] = 0.0;
			um_vctmtf(e->cpln.yaxis,tf,pt3);
			scal = um_mag(pt3);

			/* modify scalers */

			e->txt_gap = e->txt_gap * scal;
			e->grid_dist = e->grid_dist * scal;
			e->char_size = e->char_size * scal;
			e->gap_to_geo = e->gap_to_geo * scal;
			e->ext_past_line = e->ext_past_line * scal;
			e->stub_length = e->stub_length * scal;
			e->arrow_size = e->arrow_size * scal;
			e->xh_spacing = e->xh_spacing * scal;

			/* modify dim_org */

			um_cctmtf(e->dim_origin, tf, e->dim_origin);

			/* transform text plane */

			um_vctmtf(e->cpln.xaxis, tf, vtx);
			um_vctmtf(e->cpln.yaxis, tf, vty);
			um_vctmtf(e->cpln.zaxis, tf, vtz);

			/* check vectors */

			if(um_cceqcc(vtz,zerov))
				{
				if((um_cceqcc(vtx,zerov)==UU_TRUE) ||
								(um_cceqcc(vty,zerov)==UU_TRUE)) goto lp1;
				um_cross(vtx,vty,vtz);
				}
			else
				{
				if(um_cceqcc(vty,zerov))
					{
					if(um_cceqcc(vtx,zerov)) goto lp1;
					um_cross(vtz,vtx,vty);
					}
				else
					{
					if(um_cceqcc(vtx,zerov))
						{
						um_cross(vty,vtz,vtx);
						}
					}
				}
			um_unitvc(vtx,vtx);
			um_unitvc(vty,vty);
			um_unitvc(vtz,vtz);
			um_vctovc(vtx,e->cpln.xaxis);
			um_vctovc(vty,e->cpln.yaxis);
			um_vctovc(vtz,e->cpln.zaxis);
	lp1:

			/* modify text blocks */

			for(i=0; i<e->txt_blk_use; i++)
				{
				um_vctmtf(e->txt_blk[i].origin, tf, e->txt_blk[i].origin);
				e->txt_blk[i].dx = e->txt_blk[i].dx * scal;
				e->txt_blk[i].dy = e->txt_blk[i].dy * scal;
				e->txt_blk[i].txt_size = e->txt_blk[i].txt_size * scal;
				}

			/* modify arc blocks */

			for(i=0; i<e->arc_blk_use; i++)
				{
				um_cctmtf(e->arc_blk[i].center_pt, tf, e->arc_blk[i].center_pt);
				e->arc_blk[i].radius = e->arc_blk[i].radius * scal;
				}

		 /* modify line blocks */

		 if (e->etype == UA_HATCHING_REL && upd == UU_TRUE)
		 	{
			 key = e->asso_blk[e->asso_blk_use -1].key;
			 ua_transf_hatch_rec(key, tf, upd);
			 }

		for(i=0; i<e->line_blk_use; i++)
			{
			for(j=0; j<e->line_blk[i].num_pts; j++)
				{
				pt1[0] = e->line_blk[i].line_seg[j][0];
				pt1[1] = e->line_blk[i].line_seg[j][1];
				pt1[2] = e->line_blk[i].line_seg[j][2];
				um_cctmtf(pt1, tf, pt1);
				e->line_blk[i].line_seg[j][0] = pt1[0];
				e->line_blk[i].line_seg[j][1] = pt1[1];
				e->line_blk[i].line_seg[j][2] = pt1[2];
				}
			}

			/* modify arrow blocks */

			for(i=0; i<e->arrow_blk_use; i++)
				{
				um_cctmtf(e->arrow_blk[i].location, tf, e->arrow_blk[i].location);
				e->arrow_blk[i].size = e->arrow_blk[i].size * scal;
				}

			/* modify asso blocks */

			if(e->etype == UA_PL_DIM)
				{
				for(i=0; i<e->asso_blk_use; i++)
					{
					note.key = e->asso_blk[i].key;
					if(e->asso_blk[i].modifier == -99)
						e->asso_blk[i].location[0] = e->asso_blk[i].location[0]/scal;
					if(ua_get_text(&note, sizeof(struct UA_txt_rec)) == UU_SUCCESS)
						{
						ua_tf_text(&note, tf, upd);
						}
					}
				}
			else
				{
				if(e->asso_blk_use > 0)
					{
					for(i=0; i<e->asso_blk_use; i++)
						{
						um_cctmtf(e->asso_blk[i].location, tf, e->asso_blk[i].location);
						}
					}
				}

			/* check update flag */

			if( upd == UU_TRUE)
				{
				key = e->key;
				ua_update_entity(key, e);
				}
			break;
		}	/* end of switch */

fexit:;
	uu_dexit;
	return(UU_SUCCESS);
	}


/*********************************************************************
**    E_FUNCTION     : ua_set_drafting_view(cpln_drw)
**       Set the view with which to associate drafting entities.
**    PARAMETERS   
**       INPUT  : 
**          cpln_drw						LOGICAL draw/don't draw
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_set_drafting_view(cpln_drw)
	UU_LOGICAL  cpln_drw;

	{
	uu_denter(UU_MTRC,(us,"ua_set_drafting_view()"));
	if( UA_dims_disp == 0 )
		um_set_drafting_view(&UA_drafting_view,cpln_drw);
	else
		{
		UA_drafting_view = 0; 
/*
.....09/25/92
.....Commented out by Paul to fix FSR 51190 problem ( Turning on Working
.....coordinate system when entering DRAFTING.) One more file was changed -
.....model/m9eplot.c
.....
.....     if(cpln_drw) um_drw_cpl_axis(UU_TRUE, UU_FALSE);
*/
		}
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : ua_reset_drafting_view()
**      Reset to drafting view to all.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_reset_drafting_view()

	{
	uu_denter(UU_MTRC,(us,"ua_reset_drafting_view()"));
	UA_drafting_view = ur_get_dispattr_view_in();
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : ua_translate_drafting(eptr, offset)
**      Translate a drafting entity
**    PARAMETERS   
**       INPUT  : 
**          eptr					generic drafting entity
**				offset				offset vector
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_translate_drafting(eptr, offset)
	struct UA_generic_draft *eptr;
	UM_vector	offset;

	{
	int i, j, status, key;
	UU_REAL pt[3];
	struct   UA_txt_rec			note;			/* text record */

	uu_denter(UU_MTRC,(us,"ua_translate_drafting()"));
	status = UU_SUCCESS;
	switch (eptr->rel_num)
		{
		case UA_HATCHING_REL:
			uu_denter2(UU_STRC,(us,"uc_translate called for xhatch"));
			uu_dexit;	
			break;	/* do nothing - handled in following case */

		case UA_LINEAR_DIMS_REL:

			/* modify dimension origin */

			um_vcplvc(eptr->dim_origin, offset, eptr->dim_origin);

			/* modify arc blocks */

			if(eptr->arc_blk_use > 0 )
				{
				for(i=0; i<eptr->arc_blk_use; i++)
					{
					um_vcplvc(eptr->arc_blk[i].center_pt, offset,
										eptr->arc_blk[i].center_pt);
					}
				}

			/* modify line blocks */

		 	if (eptr->etype == UA_HATCHING_REL)
			 	{
				 key = eptr->asso_blk[eptr->asso_blk_use -1].key;
				 ua_translate_hatch_rec(key, offset);
			 	}

			if( eptr->line_blk_use > 0 )
				{
				for(i=0; i<eptr->line_blk_use; i++)
					{
					for(j=0; j<eptr->line_blk[i].num_pts; j++)
						{
						pt[0] = eptr->line_blk[i].line_seg[j][0];
						pt[1] = eptr->line_blk[i].line_seg[j][1];
						pt[2] = eptr->line_blk[i].line_seg[j][2];
						um_vcplvc(pt, offset, pt);
						eptr->line_blk[i].line_seg[j][0] = pt[0];
						eptr->line_blk[i].line_seg[j][1] = pt[1];
						eptr->line_blk[i].line_seg[j][2] = pt[2];
						}
					}
				}

			/* modify arrow blocks */

			if(eptr->arrow_blk_use > 0 )
				{
				for(i=0; i<eptr->arrow_blk_use; i++)
					{
					um_vcplvc(eptr->arrow_blk[i].location, offset,
									eptr->arrow_blk[i].location);
					}
				}

			/* modify asso blocks */

			if(eptr->etype == UA_PL_DIM)
				{
				for(i=0; i<eptr->asso_blk_use; i++)
					{
					note.key = eptr->asso_blk[i].key;
					if(ua_get_text(&note, sizeof(struct UA_txt_rec)) == UU_SUCCESS)
						{
						ua_tr_text(&note, offset);
						}
					}
				}
			else
				{
				if(eptr->asso_blk_use > 0 )
					{
					for(i=0; i<eptr->asso_blk_use; i++)
						{
						um_vcplvc(eptr->asso_blk[i].location, offset,
										eptr->asso_blk[i].location);
						}
					}
				}


			/* update unibase record */

			key = eptr->key;
			ua_update_entity(key, eptr);
			break;
	}	/* end of switch */
	uu_dexit;
	return(status);
}


/*********************************************************************
**    E_FUNCTION     : ua_rotate_drafting(eptr, pt, dir, angle, rotmat)
**      Rotate a drafting entity
**    PARAMETERS   
**       INPUT  : 
**          eptr							drafting entity rocord
**          pt								point on rotation axis
**          dir							direction vector
**          angle							rotation angle
**          rotmat						rotation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_rotate_drafting(eptr, pt, dir, angle, rotmat)
	struct UA_generic_draft  *eptr;
	UM_coord			pt;
	UM_vector		dir;
	UM_angle			angle;
	UM_transf		rotmat;

	{
	int i, j, status, key;
	UU_REAL linept[3];
	struct   UA_txt_rec			note;			/* text record */

	uu_denter(UU_MTRC,(us,"ua_rotate_drafting()"));
	status = UU_SUCCESS;
	switch (eptr->rel_num)
		{
		case UA_HATCHING_REL:
			uu_denter2(UU_STRC,(us,"uc_rotate called for xhatch"));
			uu_dexit;	
			break;	/* do nothing - handled in following case */

		case UA_LINEAR_DIMS_REL:

			/* modify dimension origin */

			um_cctmtf(eptr->dim_origin, rotmat, eptr->dim_origin);

			/* modify text plane */

			um_vctmtf(eptr->cpln.xaxis, rotmat, eptr->cpln.xaxis);
			um_unitvc(eptr->cpln.xaxis,eptr->cpln.xaxis);
			um_vctmtf(eptr->cpln.yaxis, rotmat, eptr->cpln.yaxis);
			um_unitvc(eptr->cpln.yaxis,eptr->cpln.yaxis);
			um_vctmtf(eptr->cpln.zaxis, rotmat, eptr->cpln.zaxis);
			um_unitvc(eptr->cpln.zaxis,eptr->cpln.zaxis);

			/* modify text blocks */

			if(eptr->txt_blk_use > 0 )
				{
				for(i=0; i<eptr->txt_blk_use; i++)
					{
					um_vctmtf(eptr->txt_blk[i].origin, rotmat,
								eptr->txt_blk[i].origin);
					}
				}

			/* modify arc blocks */

			if(eptr->arc_blk_use > 0 )
				{
				for(i=0; i<eptr->arc_blk_use; i++)
					{
					um_cctmtf(eptr->arc_blk[i].center_pt, rotmat,
								eptr->arc_blk[i].center_pt);
					}
				}

			/* modify line blocks */

		 	if (eptr->etype == UA_HATCHING_REL)
		 		{
			 key = eptr->asso_blk[eptr->asso_blk_use -1].key;
			 ua_rotate_hatch_rec(key, pt, dir, angle, rotmat);
		 		}

			if( eptr->line_blk_use > 0 )
				{
				for(i=0; i<eptr->line_blk_use; i++)
					{
					for(j=0; j<eptr->line_blk[i].num_pts; j++)
						{
						linept[0] = eptr->line_blk[i].line_seg[j][0];
						linept[1] = eptr->line_blk[i].line_seg[j][1];
						linept[2] = eptr->line_blk[i].line_seg[j][2];
						um_cctmtf(linept, rotmat, linept);
						eptr->line_blk[i].line_seg[j][0] = linept[0];
						eptr->line_blk[i].line_seg[j][1] = linept[1];
						eptr->line_blk[i].line_seg[j][2] = linept[2];
						}
					}
				}

			/* modify arrow blocks */

			if(eptr->arrow_blk_use > 0 )
				{
				for(i=0; i<eptr->arrow_blk_use; i++)
					{
					um_cctmtf(eptr->arrow_blk[i].location, rotmat, 
								eptr->arrow_blk[i].location);
					}
				}

			/* modify asso blocks */

			if(eptr->etype == UA_PL_DIM)
				{
				for(i=0; i<eptr->asso_blk_use; i++)
					{
					note.key = eptr->asso_blk[i].key;
					if(ua_get_text(&note, sizeof(struct UA_txt_rec)) == UU_SUCCESS)
						{
						ua_tf_text(&note, rotmat, UU_TRUE);
						}
					}
				}
			else
				{
				if(eptr->asso_blk_use > 0 )
					{
					for(i=0; i<eptr->asso_blk_use; i++)
						{
						um_cctmtf(eptr->asso_blk[i].location, rotmat, 
									eptr->asso_blk[i].location);
						}
					}
				}


			/* update unibase record */

			key = eptr->key;
			ua_update_entity(key, eptr);
			break;
		}	/* end of switch */
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    I_FUNCTION: ua_query(key, list)
**      Build a character array of the data from unibase
**      given an entity key, data array and the extants of
**      of that array.
**
**      This function must be modified much the same as a
**      dispatcher to take into account new entities in Unibase.
**
**      If this function cannot extract the contents for the
**      given key, the function returns the forowsUsedowing strings
**      in the first 3 rows:
**
**          Entity type: ???
**           key . . . . . . . . . . . . (input key)
**           rel_num . . . . . . . . . . (entity relation number)
**
**       Additionally the function returns a status of UU_FAILURE
**       so the calling application has the option of operating
**       on the contents of the data area or not.
**    PARAMETERS
**       INPUT  :
**          key               Unibase key.
**				list					data list.
**       OUTPUT :
**    RETURNS      : UU_SUCCESS    if data has been extracted.
**                   UU_FAILURE    if rows < 30, or if maxCols < 70.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_query(key, list)
   UU_KEY_ID  key;
	UU_LIST	  *list;
{
	int status = UU_SUCCESS; 
	struct UA_generic_draft 	e;
	struct UM_attrdata_rec  attr;

	uu_denter(UU_STRC, (us,"ua_query(key=%d,list=%x)", key, list));

	e.key = key;
	if (uc_retrieve_data(&e, sizeof(struct UA_generic_draft)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 

	/*
	sprintf(buf, "sub_type: %s", name);
	uj_putstr(list, buf);
	*/

 
	goto done;
failed: status = UU_FAILURE;
done:;
   uu_dexitstatus("ua_query", status);
   return(status);
}

/*********************************************************************
**    E_FUNCTION: ua_text_query(key, list)
**      Builds a character array from the canonical data of a text entity.
**
**    PARAMETERS
**       INPUT  :
**          key               Unibase key.
**				list					data list.
**       OUTPUT :
**    RETURNS      : UU_SUCCESS    if data has been extracted.
**                   UU_FAILURE    if rows < 30, or if maxCols < 70.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_text_query(key, list)
UU_KEY_ID  key;
UU_LIST *list;
{
	int status = UU_SUCCESS,nc,ist,i; 
	UX_pathname buf,fname;
	struct UA_txt_rec e;
	struct UA_txtattr_rec attr;

/*
.....Get the text and attribute records
*/
	e.key = key;
	if (ua_get_text1(&e) != UU_SUCCESS) goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
/*
.....List the text specific attributes
*/
	uj_putstr(list," ");
	uj_putstr(list,"TEXT:");
	strcpy(fname,e.tchar);
	ist = 0;
	for (i=0;i<strlen(fname);i++)
	{
		if (fname[i] == '\n')
		{
			nc = i - ist;
			if (nc == 0) buf[0] = '\0';
			else
			{
				strncpy(buf,&fname[ist],nc);
				buf[nc] = '\0';
			}
			uj_putstr(list,buf);
			ist = i + 1;
		}
	}
	uj_putstr(list," ");
	if (attr.prec == UG_STROKE)
	{
		ua_get_txt_fontname(attr.font,fname);
		sprintf(buf,"Font: %s",fname);
	}
	else
		sprintf(buf,"Font: String Text");
	uj_putstr(list,buf);
	sprintf(buf,"Location: %10.5f,%10.5f,%10.5f",e.position[0],e.position[1],
		e.position[2]);
	uj_putstr(list,buf);
	sprintf(buf,"Character Height: %lf",attr.height);
	uj_putstr(list,buf);
	switch(attr.path)
	{
	case 0:
		sprintf(buf,"Text Path: Right");
		break;
	case 1:
		sprintf(buf,"Text Path: Left");
		break;
	case 2:
		sprintf(buf,"Text Path: Up");
		break;
	case 3:
		sprintf(buf,"Text Path: Down");
		break;
	}
	uj_putstr(list,buf);
	switch (attr.entity_site)
	{
	case UA_TOP_LEFT:
		sprintf(buf,"Text Site: Top Left");
		break;
	case UA_MIDDLE_LEFT:
		sprintf(buf,"Text Site: Middle Left");
		break;
	case UA_BOTTOM_LEFT:
		sprintf(buf,"Text Site: Bottom Left");
		break;
	case UA_TOP_CENTER:
		sprintf(buf,"Text Site: Top Center");
		break;
	case UA_MIDDLE_CENTER:
		sprintf(buf,"Text Site: Middle Center");
		break;
	case UA_BOTTOM_CENTER:
		sprintf(buf,"Text Site: Bottom Center");
		break;
	case UA_TOP_RIGHT:
		sprintf(buf,"Text Site: Top Right");
		break;
	case UA_MIDDLE_RIGHT:
		sprintf(buf,"Text Site: Middle Right");
		break;
	case UA_BOTTOM_RIGHT:
		sprintf(buf,"Text Site: Bottom Right");
		break;
	}
	uj_putstr(list,buf);
	sprintf(buf,"Character Slant: %lf",attr.slant);
	uj_putstr(list,buf);
	sprintf(buf,"Character Expansion: %lf",attr.expn);
	uj_putstr(list,buf);
	sprintf(buf,"Character Spacing: %lf",attr.spacing);
	uj_putstr(list,buf);
	sprintf(buf,"Line Spacing: %lf",attr.line_spacing);
	uj_putstr(list,buf);
	switch(attr.align_ver)
	{
	case 0:
		sprintf(buf,"Vertical Alignment: Normal");
		break;
	case 1:
		sprintf(buf,"Vertical Alignment: Top");
		break;
	case 2:
		sprintf(buf,"Vertical Alignment: Half");
		break;
	case 3:
		sprintf(buf,"Vertical Alignment: Base");
		break;
	case 4:
		sprintf(buf,"Vertical Alignment: Bottom");
		break;
	}
	uj_putstr(list,buf);
	switch(attr.align_hor)
	{
	case 0:
		sprintf(buf,"Horizontal Alignment: Normal");
		break;
	case 1:
		sprintf(buf,"Horizontal Alignment: Left");
		break;
	case 2:
		sprintf(buf,"Horizontal Alignment: Center");
		break;
	case 3:
		sprintf(buf,"Horizontal Alignment: Right");
		break;
	}
	uj_putstr(list,buf);
	if (e.arckey != 0) 
	{
		sprintf(buf,"Text On a Curve");
		uj_putstr(list,buf);
	}
	if (e.no_displst != 0)
	{
		sprintf(buf,"Text Projected to a Surface");
		uj_putstr(list,buf);
	}
	goto done;
failed: status = UU_FAILURE;
done:;
   return(status);
}

/*********************************************************************
**    E_FUNCTION: ua_scale_drafting(entr, scalpt, scalmat, scale)
**         scale a drafting entity
**    PARAMETERS
**       INPUT  :
**          entr              drafting record
**          scalpt            scale point
**          scalmat           scale matrix
**          scale           	scale value
**       OUTPUT :
**          entr              scaled drafting entity
**    RETURNS      : none 
**                  
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_scale_drafting(entr, scalpt, scalmat, scale)

	struct UA_generic_draft	   *entr;
	UM_coord         				scalpt;
	UM_transf       			  scalmat;
	UM_length      			  scale;
	{
	int i;
	struct   UA_txt_rec			note;			/* text record */

	uu_denter(UU_STRC, (us,"ua_scale"));

	ua_transform_drafting(entr, scalmat, UU_FALSE);

	/* check for a PARTS LIST TABLE */

	if(entr->etype == UA_PL_DIM)
		{
		for(i=0; i<entr->asso_blk_use; i++)
			{
			note.key = entr->asso_blk[i].key;
			if(ua_get_text(&note, sizeof(struct UA_txt_rec)) == UU_SUCCESS)
				{
				ua_scal_text(&note, scalpt,  scalmat, scale);
				}
			}
		}

	ua_update_entity(entr->key, entr);

done:;
   uu_dexit;
   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION: ua_mirror_drafting(entr, scalpt, scalmat, scale)
**         mirror a drafting entity
**    PARAMETERS
**       INPUT  :
**          entr              drafting record
**          mirrpt            mirror point
**          mirrnorm          mirror plane normal
**          mirrmat           mirror transformation
**       OUTPUT :
**          entr              mirrored drafting entity
**    RETURNS      : none 
**                  
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_mirror_drafting(entr, mirrpt, mirrnorm, mirrmat)

	struct UA_generic_draft	   *entr;
	UM_coord mirrpt;
	UM_vector mirrnorm;
	UM_transf mirrmat;
	{
	UM_vector z_def;
	UU_REAL   zdot;
	struct   UA_txt_rec			note;			/* text record */
	int i;

	uu_denter(UU_STRC, (us,"ua_mirror"));

	ua_transform_drafting(entr, mirrmat, UU_FALSE);

	um_cross(entr->cpln.xaxis, entr->cpln.yaxis, z_def);
	zdot = um_dot(z_def, entr->cpln.zaxis);
	if(zdot < 0.0 )
		{
		um_vctmsc(entr->cpln.zaxis, (UU_REAL) -1.0, entr->cpln.zaxis);
		}

	/* check for a PARTS LIST TABLE */

	if(entr->etype == UA_PL_DIM)
		{
		for(i=0; i<entr->asso_blk_use; i++)
			{
			note.key = entr->asso_blk[i].key;
			if(ua_get_text(&note, sizeof(struct UA_txt_rec)) == UU_SUCCESS)
				{
				ua_mirror_text(&note, mirrpt, mirrnorm, mirrmat);
				}
			}
		}

	ua_update_entity(entr->key, entr);
done:;
   uu_dexit;
   return(UU_SUCCESS);
}
