/*********************************************************************
**    NAME         :  adbms.c
**       CONTAINS:
**       Temp interface from DRAFTING to unibase.
**				 ua_update_entity (key, entity)
**				 ua_get_entity (entity, size)
**				 ua_uni_to_sal (entity1, entity2)
**				 ua_sal_to_uni (entity1, entity2)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       adbms.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:32
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mattr.h"
#include "adraft.h"
#include "adrf.h"

/*********************************************************************
**    E_FUNCTION :  ua_update_entity (key, entity)
**       Updates the entity given in the generic entity format in unibase.
**    PARAMETERS   
**       INPUT  : 
**          entity - generic entity to save
**       OUTPUT :  
**          key   - entity key
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_update_entity(key, entity)
	unsigned long		key;	/* unibase entity key	*/
	struct UA_generic_draft	*entity;	/*	entity to save			*/

{
	struct UA_draft_rec	e;
	struct UM_transf_rec	transfpacket;
	int status;

	uu_denter(UU_MTRC,(us,"ua_update_entity(%d,%x)",key,entity));
	entity->key = key;
	entity->rel_num = UA_LINEAR_DIMS_REL;

	ur_setup_data(UA_LINEAR_DIMS_REL,&e,sizeof(struct UA_draft_rec));
	ua_sal_to_uni(entity,&e);
	status = ur_update_data(&e);
	if(status == 0)
		{
		transfpacket.key = e.key;
		transfpacket.rel_num = UM_TRANSFORM_REL;
		um_tftotf(UM_idmat, transfpacket.tfmat);
		status = ur_update_transf(&transfpacket);
		}
	uu_dexit;
	return(status);

}

/*********************************************************************
**    E_FUNCTION :  ua_get_entity (entity, size)
**       get an entity from unibase
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 0 successful ; -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_get_entity(entity, size)
	struct UA_generic_draft	*entity;	/*	entity to get			*/
	int	 size;

{
	struct	UA_draft_rec	e;
	int status;

	uu_denter(UU_MTRC,(us,"ua_get_entity(%x %d)",entity, size));
	e.key = entity->key;

	status = ur_retrieve_data(&e,sizeof(struct UA_draft_rec));

	if(status != -1)
		{
		ua_uni_to_sal(&e,entity);
		}
	uu_dexit;
	return(status);

}

/*********************************************************************
**    E_FUNCTION :  ua_sal_to_uni (entity1, entity2)
**       Convert from SAL record structure to unibase record structure
**    PARAMETERS   
**       INPUT  : 
**          entity1 - SAL record
**       OUTPUT :  
**          entity2   - unibase record structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_sal_to_uni(e1, e2)
	struct UA_generic_draft	*e1;	/*	entity to save			*/
	struct UA_draft_rec	*e2;	/*	entity to save			*/

	{

	int i,j;
	uu_denter(UU_STRC,(us,"ua_sal_to_uni(%x,%x)",e1,e2));

	e2->key = e1->key;	/* key */
	e2->rel_num = e1->rel_num;	/* rel_num */
	e2->draf_ati[0 ] = e1->etype;	/* etype */
	e2->draf_ati[1 ] = e1->subtype;	/* subtype */
	e2->draf_ati[2 ] = e1->draft_stand;	/* draft_stand */
	e2->draf_ati[3 ] = e1->dims_display;	/* dims_display */
	e2->draf_ati[4 ] = e1->txt_place;	/* txt_place */
	e2->draf_ati[5 ] = e1->entity_site;	/* entity_site */
	e2->draf_ati[6 ] = e1->txt_entry;	/* txt_entry */
	e2->draf_ati[7 ] = e1->appn_text;	/* appn_text */
	e2->draf_ati[8 ] = e1->txt_orent;	/* txt_orent */
	e2->draf_ati[9 ] = e1->txt_just;	/* txt_just */
	e2->draf_ati[10] = e1->stack_grid;	/* stack_grid */
	e2->draf_ati[11] = e1->linear_units;	/* linear_units */
	e2->draf_ati[12] = e1->fract_units;	/* fract_units */
	e2->draf_ati[13] = e1->ang_units;	/* ang_units */
	e2->draf_ati[14] = e1->units_sym;	/* units_sym */
	e2->draf_ati[15] = e1->dim_type;	/* dim_type */
	e2->draf_ati[16] = e1->dim_places;	/* dim_places */
	e2->draf_ati[17] = e1->dim_zero_sup;	/* dim_zero_sup */
	e2->draf_ati[18] = e1->dim_roundoff;	/* dim_roundoff */
	e2->draf_ati[19] = e1->tol_places;	/* tol_places */
	e2->draf_ati[20] = e1->tol_roundoff;	/* tol_roundoff */
	e2->draf_ati[21] = e1->tol_method;	/* tol_method */
	e2->draf_ati[22] = e1->tol_site;	/* tol_site */
	e2->draf_ati[23] = e1->tol_zero_sup;	/* tol_zero_sup */
	e2->draf_ati[24] = e1->dual_format;	/* dual_format */
	e2->draf_ati[25] = e1->dual_l_units;	/* dual_l_units */
	e2->draf_ati[26] = e1->dual_f_units;	/* dual_f_units */
	e2->draf_ati[27] = e1->d_units_sym;	/* d_units_sym */
	e2->draf_ati[28] = e1->dual_place;	/* dual_place */
	e2->draf_ati[29] = e1->du_tol_pl;	/* du_tol_pl */
	e2->draf_ati[30] = e1->dual_a_units;	/* dual_a_units */
	e2->draf_ati[31] = e1->d_tol_z_sup;	/* d_tol_z_sup */
	e2->draf_ati[32] = e1->d_dim_z_sup;	/* d_dim_z_sup */
	e2->draf_ati[33] = e1->d_dim_roundoff;	/* d_dim_roundoff */
	e2->draf_ati[34] = e1->d_tol_roff_meth;	/* d_tol_roff_meth */
	e2->draf_ati[35] = e1->dia_place;	/* dia_place */
	e2->draf_ati[36] = e1->diam_symbol;	/* diam_symbol */
	e2->draf_ati[37] = e1->rad_place;	/* rad_place */
	e2->draf_ati[38] = e1->rad_symb;	/* rad_symb */
	e2->draf_ati[39] = e1->ext_line_sup;	/* ext_line_sup */
	e2->draf_ati[40] = e1->lead_orient;	/* lead_orient */
	e2->draf_ati[41] = e1->leader_loc;	/* leader_loc */
	e2->draf_ati[42] = e1->arrow_place;	/* arrow_place */
	e2->draf_ati[43] = e1->xh_pattern;	/* xh_pattern */
	e2->draf_atr[0 ] = e1->txt_gap;	/* txt_gap */
	e2->draf_atr[1 ] = e1->grid_dist;	/* grid_dist */
	e2->draf_atr[2 ] = e1->char_slant;	/* char_slant */
	e2->draf_atr[3 ] = e1->char_size;	/* char_size */
	e2->draf_atr[4 ] = e1->sub_sup_ratio;	/* sub_sup_ratio */
	e2->draf_atr[5 ] = e1->dim_rnd_fact;	/* dim_rnd_fact */
	e2->draf_atr[6 ] = e1->d_dim_rnd_fact;	/* d_dim_rnd_fact */
	e2->draf_atr[7 ] = e1->tol_rnd_fact;	/* tol_rnd_fact */
	e2->draf_atr[8 ] = e1->d_tol_rnd_fact;	/* d_tol_rnd_fact */
	e2->draf_atr[9 ] = e1->upper_tol;	/* upper_tol */
	e2->draf_atr[10] = e1->lower_tol;	/* lower_tol */
	e2->draf_atr[11] = e1->d_lin_up_tol;	/* d_lin_up_tol */
	e2->draf_atr[12] = e1->d_lin_lo_tol;	/* d_lin_lo_tol */
	e2->draf_atr[13] = e1->gap_to_geo;	/* gap_to_geo */
	e2->draf_atr[14] = e1->ext_past_line;	/* ext_past_line */
	e2->draf_atr[15] = e1->oblique_angle;	/* oblique_angle */
	e2->draf_atr[16] = e1->stub_length;	/* stub_length */
	e2->draf_atr[17] = e1->arrow_size;	/* arrow_size */
	e2->draf_atr[18] = e1->dim_value;	/* dim_value */
	e2->draf_atr[19] = e1->dim2_value;	/* dim2_value */
	e2->draf_atr[20] = e1->dim_origin[0];	/* dim_origin */
	e2->draf_atr[21] = e1->dim_origin[1];	/* dim_origin */
	e2->draf_atr[22] = e1->dim_origin[2];	/* dim_origin */
	e2->draf_atr[23] = e1->xh_angle;	/* xh_angle */
	e2->draf_atr[24] = e1->xh_spacing;	/* xh_spacing */

	for(i=0;i<3;i++)
		{
		e2->cpln[i] = e1->cpln.cpln_origin[i];
		e2->cpln[i+3] = e1->cpln.xaxis[i];
		e2->cpln[i+6] = e1->cpln.yaxis[i];
		e2->cpln[i+9] = e1->cpln.zaxis[i];
		}

	e2->no_txtblk = e1->txt_blk_use;
	for(i=0;i<e2->no_txtblk;i++)
		{
		e2->txtblk[i].subtype = (int) e1->txt_blk[i].subtype;
		e2->txtblk[i].txt_font = e1->txt_blk[i].text.line_font;
		e2->txtblk[i].txt_dens = e1->txt_blk[i].text.line_density;
		e2->txtblk[i].color = e1->txt_blk[i].text.color;
		e2->txtblk[i].char_cnt = e1->txt_blk[i].char_cnt;
		e2->txtblk[i].txt_just = e1->txt_blk[i].txt_just;
		for(j=0;j<17;j++)
			{
			e2->txtblk[i].fontname[j] = e1->txt_blk[i].fontname[j];
			}
		for(j=0;j<1025;j++)
			{
			e2->txtblk[i].tstring[j] = e1->txt_blk[i].tstring[j];
			}
		for(j=0;j<3;j++)
			{
			e2->txtblk[i].origin[j] = e1->txt_blk[i].origin[j];
			}
		e2->txtblk[i].dx = e1->txt_blk[i].dx;
		e2->txtblk[i].dy = e1->txt_blk[i].dy;
		e2->txtblk[i].slant = e1->txt_blk[i].slant;
		e2->txtblk[i].tangle = e1->txt_blk[i].tangle;
		e2->txtblk[i].txt_size = e1->txt_blk[i].txt_size;
		e2->txtblk[i].sub_sup = e1->txt_blk[i].sub_super;
		e2->txtblk[i].char_exp = e1->txt_blk[i].char_expansion;
		e2->txtblk[i].char_spa = e1->txt_blk[i].char_space;
		e2->txtblk[i].line_spa = e1->txt_blk[i].line_spacing;
		}

	e2->no_arcblk = e1->arc_blk_use;
	for(i=0;i<e2->no_arcblk;i++)
		{
		e2->arcblk[i].subtype = (int) e1->arc_blk[i].subtype;
		e2->arcblk[i].arc_font = e1->arc_blk[i].arc.line_font;
		e2->arcblk[i].arc_dens = e1->arc_blk[i].arc.line_density;
		e2->arcblk[i].color = e1->arc_blk[i].arc.color;
		e2->arcblk[i].num_pts = e1->arc_blk[i].num_pts;
		for(j=0;j<3;j++)
			{
			e2->arcblk[i].cent_pt[j] = e1->arc_blk[i].center_pt[j];
			}
		e2->arcblk[i].radius = e1->arc_blk[i].radius;
		for(j=0;j<50;j++)
			{
			e2->arcblk[i].angles[j] = e1->arc_blk[i].angles[j];
			}
		}

	e2->no_lineblk = e1->line_blk_use;
	for(i=0;i<e2->no_lineblk;i++)
		{
		e2->lineblk[i].subtype = (int) e1->line_blk[i].subtype;
		e2->lineblk[i].line_fon = e1->line_blk[i].line.line_font;
		e2->lineblk[i].line_den = e1->line_blk[i].line.line_density;
		e2->lineblk[i].color = e1->line_blk[i].line.color;
		e2->lineblk[i].num_pts = e1->line_blk[i].num_pts;
		for(j=0;j<50;j++)
			{
			e2->lineblk[i].line_seg[j][0] = e1->line_blk[i].line_seg[j][0];
			e2->lineblk[i].line_seg[j][1] = e1->line_blk[i].line_seg[j][1];
			e2->lineblk[i].line_seg[j][2] = e1->line_blk[i].line_seg[j][2];
			}
		}

	e2->no_arrowblk = e1->arrow_blk_use;
	for(i=0;i<e2->no_arrowblk;i++)
		{
		e2->arrowblk[i].arr_type = e1->arrow_blk[i].arrow_type;
		e2->arrowblk[i].arr_font = e1->arrow_blk[i].arrow.line_font;
		e2->arrowblk[i].arr_dens = e1->arrow_blk[i].arrow.line_density;
		e2->arrowblk[i].color = e1->arrow_blk[i].arrow.color;
		for(j=0;j<3;j++)
			{
			e2->arrowblk[i].location[j] = e1->arrow_blk[i].location[j];
			}
		e2->arrowblk[i].aangle = e1->arrow_blk[i].aangle;
		e2->arrowblk[i].asize = e1->arrow_blk[i].size;
		}

	e2->no_assoblk = e1->asso_blk_use;
	for(i=0;i<e2->no_assoblk;i++)
		{
		e2->assoblk[i].asso_typ = e1->asso_blk[i].asso_type;
		e2->assoblk[i].modifier = e1->asso_blk[i].modifier;
		e2->assoblk[i].asso_key = e1->asso_blk[i].key;
		for(j=0;j<3;j++)
			{
			e2->assoblk[i].location[j] = e1->asso_blk[i].location[j];
			}
		}
	uu_dexit;
	}

/*********************************************************************
**       E_FUNCTION : ua_uni_to_sal (entity1, entity2)
**       Convert from UNIBASE to SAL record format.
**    PARAMETERS   
**       INPUT  : 
**          entity1 - UNIBASE record
**       OUTPUT :  
**          entity2   - SAL record structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_uni_to_sal(e2, e1)
	struct UA_draft_rec	*e2;	/*	 UNIBASE record */
	struct UA_generic_draft	*e1;	/*	SAL record */

	{

	int i,j;

	e1->key = e2->key;	/* key */
	e1->rel_num = e2->rel_num;	/* rel_num */
	e1->etype =  e2->draf_ati[0 ] ;
	e1->subtype = e2->draf_ati[1 ] ;
	e1->draft_stand = e2->draf_ati[2 ];
	e1->dims_display =  e2->draf_ati[3 ];
	e1->txt_place =  e2->draf_ati[4 ];
	e1->entity_site =  e2->draf_ati[5 ];
	e1->txt_entry =  e2->draf_ati[6 ];
	e1->appn_text = e2->draf_ati[7 ];
	e1->txt_orent = e2->draf_ati[8 ];
	e1->txt_just = e2->draf_ati[9 ];
	e1->stack_grid = e2->draf_ati[10];
	e1->linear_units = e2->draf_ati[11];
	e1->fract_units = e2->draf_ati[12] ;
	e1->ang_units = e2->draf_ati[13];
	e1->units_sym = e2->draf_ati[14];
	e1->dim_type = e2->draf_ati[15];
	e1->dim_places = e2->draf_ati[16];
	e1->dim_zero_sup = e2->draf_ati[17];
	e1->dim_roundoff = e2->draf_ati[18];
	e1->tol_places = e2->draf_ati[19];
	e1->tol_roundoff = e2->draf_ati[20];
	e1->tol_method = e2->draf_ati[21];
	e1->tol_site = e2->draf_ati[22];
	e1->tol_zero_sup = e2->draf_ati[23];
	e1->dual_format = e2->draf_ati[24];
	e1->dual_l_units =  e2->draf_ati[25];
	e1->dual_f_units = e2->draf_ati[26];
	e1->d_units_sym = e2->draf_ati[27];
	e1->dual_place = e2->draf_ati[28];
	e1->du_tol_pl = e2->draf_ati[29];
	e1->dual_a_units = e2->draf_ati[30];
	e1->d_tol_z_sup = e2->draf_ati[31];
	e1->d_dim_z_sup = e2->draf_ati[32];
	e1->d_dim_roundoff = e2->draf_ati[33];
	e1->d_tol_roff_meth = e2->draf_ati[34];
	e1->dia_place = e2->draf_ati[35];
	e1->diam_symbol = e2->draf_ati[36];
	e1->rad_place = e2->draf_ati[37];
	e1->rad_symb = e2->draf_ati[38];
	e1->ext_line_sup = e2->draf_ati[39];
	e1->lead_orient = e2->draf_ati[40];
	e1->leader_loc = e2->draf_ati[41];
	e1->arrow_place = e2->draf_ati[42];
	e1->xh_pattern = e2->draf_ati[43];
	e1->txt_gap = e2->draf_atr[0 ];
	e1->grid_dist = e2->draf_atr[1 ];
	e1->char_slant = e2->draf_atr[2 ];
	e1->char_size = e2->draf_atr[3 ];
	e1->sub_sup_ratio = e2->draf_atr[4 ];
	e1->dim_rnd_fact = e2->draf_atr[5 ];
	e1->d_dim_rnd_fact = e2->draf_atr[6 ];
	e1->tol_rnd_fact = e2->draf_atr[7 ];
	e1->d_tol_rnd_fact = e2->draf_atr[8 ];
	e1->upper_tol = e2->draf_atr[9 ];
	e1->lower_tol = e2->draf_atr[10];
	e1->d_lin_up_tol = e2->draf_atr[11];
	e1->d_lin_lo_tol = e2->draf_atr[12];
	e1->gap_to_geo = e2->draf_atr[13];
	e1->ext_past_line = e2->draf_atr[14];
	e1->oblique_angle = e2->draf_atr[15];
	e1->stub_length = e2->draf_atr[16];
	e1->arrow_size = e2->draf_atr[17];
	e1->dim_value = e2->draf_atr[18];
	e1->dim2_value = e2->draf_atr[19];
	e1->dim_origin[0] = e2->draf_atr[20];
	e1->dim_origin[1] = e2->draf_atr[21];
	e1->dim_origin[2] = e2->draf_atr[22];
	e1->xh_angle = e2->draf_atr[23];
	e1->xh_spacing = e2->draf_atr[24];

	for(i=0;i<3;i++)
		{
		e1->cpln.cpln_origin[i] = e2->cpln[i] ;
		e1->cpln.xaxis[i] = e2->cpln[i+3];
		e1->cpln.yaxis[i] = e2->cpln[i+6];
		e1->cpln.zaxis[i] = e2->cpln[i+9];
		}

	e1->txt_blk_use = e2->no_txtblk;
	for(i=0;i<e2->no_txtblk;i++)
		{
		e1->txt_blk[i].subtype = (enum UA_text_blocks) e2->txtblk[i].subtype;
		e1->txt_blk[i].text.line_font = e2->txtblk[i].txt_font;
		e1->txt_blk[i].text.line_density = e2->txtblk[i].txt_dens;
		e1->txt_blk[i].text.color = e2->txtblk[i].color;
		e1->txt_blk[i].char_cnt = e2->txtblk[i].char_cnt;
		e1->txt_blk[i].txt_just = e2->txtblk[i].txt_just;
		for(j=0;j<17;j++)
			{
			e1->txt_blk[i].fontname[j] = e2->txtblk[i].fontname[j];
			}
		for(j=0;j<1025;j++)
			{
			e1->txt_blk[i].tstring[j] = e2->txtblk[i].tstring[j];
			}
		for(j=0;j<3;j++)
			{
			e1->txt_blk[i].origin[j] = e2->txtblk[i].origin[j];
			}
		e1->txt_blk[i].dx = e2->txtblk[i].dx;
		e1->txt_blk[i].dy = e2->txtblk[i].dy;
		e1->txt_blk[i].slant = e2->txtblk[i].slant;
		e1->txt_blk[i].tangle = e2->txtblk[i].tangle;
		e1->txt_blk[i].txt_size = e2->txtblk[i].txt_size;
		e1->txt_blk[i].sub_super = e2->txtblk[i].sub_sup;
		e1->txt_blk[i].char_expansion = e2->txtblk[i].char_exp;
		e1->txt_blk[i].char_space = e2->txtblk[i].char_spa;
		e1->txt_blk[i].line_spacing = e2->txtblk[i].line_spa;
		}

	e1->arc_blk_use = e2->no_arcblk;
	for(i=0;i<e2->no_arcblk;i++)
		{
		e1->arc_blk[i].subtype = (enum UA_arc_blocks) e2->arcblk[i].subtype;
		e1->arc_blk[i].arc.line_font = e2->arcblk[i].arc_font;
		e1->arc_blk[i].arc.line_density = e2->arcblk[i].arc_dens;
		e1->arc_blk[i].arc.color = e2->arcblk[i].color;
		e1->arc_blk[i].num_pts = e2->arcblk[i].num_pts;
		for(j=0;j<3;j++)
			{
			e1->arc_blk[i].center_pt[j] = e2->arcblk[i].cent_pt[j];
			}
		e1->arc_blk[i].radius = e2->arcblk[i].radius;
		for(j=0;j<50;j++)
			{
			e1->arc_blk[i].angles[j] = e2->arcblk[i].angles[j];
			}
		}

	e1->line_blk_use = e2->no_lineblk;
	for(i=0;i<e2->no_lineblk;i++)
		{
		e1->line_blk[i].subtype = (enum UA_line_blocks) e2->lineblk[i].subtype;
		e1->line_blk[i].line.line_font = e2->lineblk[i].line_fon;
		e1->line_blk[i].line.line_density = e2->lineblk[i].line_den;
		e1->line_blk[i].line.color = e2->lineblk[i].color;
		e1->line_blk[i].num_pts = e2->lineblk[i].num_pts;
		for(j=0;j<50;j++)
			{
			e1->line_blk[i].line_seg[j][0] = e2->lineblk[i].line_seg[j][0];
			e1->line_blk[i].line_seg[j][1] = e2->lineblk[i].line_seg[j][1];
			e1->line_blk[i].line_seg[j][2] = e2->lineblk[i].line_seg[j][2];
			}
		}

	e1->arrow_blk_use = e2->no_arrowblk;
	for(i=0;i<e2->no_arrowblk;i++)
		{
		e1->arrow_blk[i].arrow_type = e2->arrowblk[i].arr_type;
		e1->arrow_blk[i].arrow.line_font = e2->arrowblk[i].arr_font;
		e1->arrow_blk[i].arrow.line_density = e2->arrowblk[i].arr_dens;
		e1->arrow_blk[i].arrow.color = e2->arrowblk[i].color;
		for(j=0;j<3;j++)
			{
			e1->arrow_blk[i].location[j] = e2->arrowblk[i].location[j];
			}
		e1->arrow_blk[i].aangle = e2->arrowblk[i].aangle;
		e1->arrow_blk[i].size = e2->arrowblk[i].asize;
		}

	e1->asso_blk_use = e2->no_assoblk;
	for(i=0;i<e2->no_assoblk;i++)
		{
		e1->asso_blk[i].asso_type = e2->assoblk[i].asso_typ;
		e1->asso_blk[i].modifier = e2->assoblk[i].modifier;
		e1->asso_blk[i].key = e2->assoblk[i].asso_key;
		for(j=0;j<3;j++)
			{
			e1->asso_blk[i].location[j] = e2->assoblk[i].location[j];
			}
		}
	}

