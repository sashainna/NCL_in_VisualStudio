/*********************************************************************
**    NAME         : amoddim.c
**       CONTAINS:
**       ua_mod_attributes
**       ua_copy_text_attr
**       ua_check_txt
**       ua_update_txt_attr
**       ua_save_dim_attr
**       ua_save_global_attr
**       ua_restore_global_attr
**       ua_get_new_attributes
**       ua_check_new_attributes
**       ua_update_dim_attributes
**    	ua_is_regen_required
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       amoddim.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:36
*********************************************************************/

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"
#include "uhep.h"
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
#include "atext.h"

extern int UD_dimensions[UD_NMENTWD];
extern int UD_text[UD_NMENTWD];
extern int UD_editable[UD_NMENTWD];
extern UU_REAL UA_gap, UA_dot, UA_dash_min, UA_dash_max;
static int save_int[21], save_int_global[21];
static UU_REAL save_real[10], save_real_global[10];
static char save_char[5][20], save_char_global[5][20];
static	ATXT_FRM    save_note_attr;
static UU_LOGICAL field[40];
static UU_REAL gap_save, dot_save, dash_min_save, dash_max_save;

void ua_copy_text_attr(),ua_check_txt(),ua_update_txt_attr(),ua_save_dim_attr();
void ua_save_global_attr(),ua_restore_global_attr(),ua_get_new_attributes();
void ua_update_dim_attr();

/*********************************************************************
**    E_FUNCTION     : ua_mod_attributes(mod_type)
**		User select routine for dimension attribute modification
**    PARAMETERS   
**       INPUT  : 
**          type						type of modification selected
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_mod_attributes(mod_type)
	int mod_type;
	{
	int i, num1, relation, d_status;
	static int type;
	UU_KEY_ID curr_key;
	UU_LOGICAL status, first;
	UM_coord location, xaxis, yaxis, zaxis, cpln_org, del_vec, tmp_vec;
	struct UA_PLOCREC	plocrec;
	struct UA_PICKENT	pickent;
	struct UA_generic_draft	edrf;
	struct UA_txt_rec  note;
	struct UA_txtattr_rec    note_attr;
	UU_LOGICAL ua_gnxt();
	UU_LOGICAL ua_check_new_attributes();
	UU_LOGICAL ua_is_regen_required();
	UU_KEY_ID um_get_pickkey();
	int ud_world_coord();

	uu_denter(UU_STRC,(us,"ua_mod_attributes( %d )", mod_type));

	/* initilize local arrays */
	for(i=0;i<21;i++)
		{
		save_int[i] = 0;
		save_int_global[i] = 0;
		}
	for(i=0;i<10;i++)
		{
		save_real[i] = 0.0;
		save_real_global[i] = 0.0;
		}
	for(i=0;i<40;i++) field[i] = UU_FALSE;

	/* switch on edit type selected by the user */
	type = mod_type;
	switch(type)
		{
		case 1: 									/* edit text strings */

			/* get entities */
			ud_lgeo(UU_TRUE, UD_text);
			ua_select( 135, &num1);
			if( num1 == 0) goto fexit;
		
			/* traverse list */
			first = UU_TRUE;
			for(i=0; i<num1; i++)
				{
				status = ua_gnxt(&curr_key);
				if(status)
					{
					note.key = curr_key;
					if(ua_get_text1(&note) == UU_SUCCESS)
						{
						note_attr.key = note.key;
						if(ur_retrieve_attr(&note_attr) == UU_SUCCESS)
							{
							if(first == UU_TRUE)
								{
								/* get attribute changes */
								first = UU_FALSE;
								ua_copy_text_attr(&save_note_attr, &UA_txtattr, &note,
																	&note_attr);
								ua_get_txt_attr(&save_note_attr, "atxtinfo.frm");
								ua_check_txt(&save_note_attr, &UA_txtattr, &note,
																	&note_attr);
								}
							ua_update_txt_attr(&note, &note_attr, &save_note_attr);
							ur_update_data_fixed(&note,UM_DEFAULT_TF);
							ur_update_attr(&note_attr);
							uc_display(&note);
							}
						}
					}
					else break;
				}
			break;
		case 9:									/* change entity origin */

			/* get entity from the user */
loop:
			ud_lgeo(UU_TRUE, UD_editable);
			d_status = ud_pick_loc(13, 137, &plocrec, 1, &num1, UU_FALSE);
			if(d_status == UA_REJECT || num1 < 1) goto fexit;
			d_status = um_d_pickresolve(&plocrec.ppick, 1, &pickent);
			curr_key = um_get_pickkey(&pickent, 1);
			edrf.key = curr_key;

			/* check data */
			d_status = um_retrieve_data_relnum(edrf.key, &relation);
			if(  relation != UA_LINEAR_DIM  && relation != UA_TEXT_REL )
				{
				uu_uerror0(UA_DRAFTING,22);
				goto fexit;
				}
			if( relation == UA_LINEAR_DIM)
				{
				edrf.key = curr_key;
				uc_retrieve_data(&edrf, sizeof(struct UA_generic_draft	)) ;
				if(edrf.etype == UA_CROSSHATCH ||
					edrf.etype == UA_CENTERLINE ||
					edrf.etype == UA_SECT_ARROW)
					{
					uu_uerror0(UA_DRAFTING,40);
					goto fexit;
					}
				if( ua_asso_check(&edrf) == UU_FAILURE)
					{
					uu_uerror0(UA_DRAFTING,43);
					goto fexit;
					}
				}

			/* get new origin from the user */
			d_status = ud_world_coord(13, 30, location, 1, &num1, UU_FALSE);
			if( d_status==0 || d_status==2 ) goto fexit;
			if(  num1==0 )
				{
				d_status = 0;
				goto fexit;
				}

			/* update data */
			if( relation == UA_LINEAR_DIM)
				{
				ua_getcpln(&edrf, cpln_org, xaxis, yaxis, zaxis);
				um_nptpln(location, cpln_org, zaxis, tmp_vec);

				if(edrf.etype == UA_FANDP_TOL)
					{
					um_vctovc(edrf.dim_origin, del_vec);
					um_vctovc(tmp_vec, edrf.dim_origin);
					ua_update_fptol(del_vec, &edrf);
					ua_regen_drafting(&edrf);
					}
				else
					{
					/* update origin position */
					um_vcmnvc(tmp_vec, edrf.dim_origin, del_vec);
					um_vctovc(tmp_vec, edrf.dim_origin);
	
					/* update text block positions */
					if(edrf.txt_blk_use > 0)
						{
						for(i=0;i<edrf.txt_blk_use;i++)
							{
							um_vcplvc(edrf.txt_blk[i].origin,
										del_vec, edrf.txt_blk[i].origin);
							}
						}
	
					/* update arrow block positions */
					if(edrf.arrow_blk_use > 0)
						{
						for(i=0;i<edrf.arrow_blk_use;i++)
							{
							um_vcplvc(edrf.arrow_blk[i].location,
										del_vec, edrf.arrow_blk[i].location);
							}
						}
					
					/* call general regeneration routine */
					ua_regen_drafting(&edrf);
					}
				}
			else
				{
				/* note entity */
				note.key = curr_key;
				uc_retrieve_data(&note, sizeof(struct UA_txt_rec	)) ;
				um_getcpln(cpln_org, xaxis, yaxis, zaxis);

				/* update position */
				um_nptpln(location, cpln_org, zaxis, note.position);
				d_status = ur_update_data_fixed(&note);
				uc_display(&note);
				}
			goto loop;
			break;

		case 10:										/* modify crosshatching attr */
			/* get entity from the user */
loop1:
			ud_lgeo(UU_TRUE, UD_editable);
			d_status = ud_pick_loc(13, 141, &plocrec, 1, &num1, UU_FALSE);
			if(d_status == UA_REJECT || num1 < 1) goto fexit;
			d_status = um_d_pickresolve(&plocrec.ppick, 1, &pickent);
			curr_key = um_get_pickkey(&pickent, 1);
			edrf.key = curr_key;

			/* check data */
			d_status = um_retrieve_data_relnum(edrf.key, &relation);
			if(  relation != UA_LINEAR_DIM )
				{
				uu_uerror0(UA_DRAFTING,22);
				goto fexit;
				}
			edrf.key = curr_key;
			uc_retrieve_data(&edrf, sizeof(struct UA_generic_draft	)) ;
			if(edrf.etype != UA_CROSSHATCH)
				{
				uu_uerror0(UA_DRAFTING,22);
				goto fexit;
				}
			if( ua_asso_check(&edrf) == UU_FAILURE)
				{
				uu_uerror0(UA_DRAFTING,43);
				goto fexit;
				}
			ua_mod_xh_attr(&edrf);
			goto loop1;
			break;

		case 11:						/* modify centerline pattern attr */

			ud_lgeo(UU_TRUE, UD_dimensions);
			ua_select( 157, &num1);
			if( num1 == 0) goto fexit;

			/* save default attributes */
			gap_save = UA_gap;
			dot_save = UA_dot;
			dash_min_save = UA_dash_min;
			dash_max_save = UA_dash_max;

			/* update centerline attributes */
			ua_set_centerline_attr();

			/* traverse entity list */
			for(i=0; i<num1; i++)
				{
				status = ua_gnxt(&curr_key);
				if(status)
					{
					if(um_retrieve_data_relnum(curr_key, &relation) == UU_SUCCESS)
						{
						if(relation == UA_LINEAR_DIM)
							{
							edrf.key = curr_key;
							uc_retrieve_data(&edrf,sizeof(struct UA_generic_draft	)) ;
							if(ua_asso_check(&edrf) == UU_SUCCESS)
								{
								/* regenerate centerline entity */
								ua_center_regen(&edrf);
								ua_regen_update_record(&edrf);
								}
							}
						}
					}
				}

			/* restore appropriate attributes */
			UA_gap = gap_save;
			UA_dot = dot_save;
			UA_dash_min = dash_min_save;
			UA_dash_max = dash_max_save;
			break;

		default:										/* modify dimension attributes */
			ud_lgeo(UU_TRUE, UD_dimensions);
			ua_select( 136, &num1);
			if( num1 == 0) goto fexit;

			/* save global attributes */
			ua_save_global_attr(type);

			/* traverse entity list */
			first = UU_TRUE;
			for(i=0; i<num1; i++)
				{
				status = ua_gnxt(&curr_key);
				if(status)
					{
					if(um_retrieve_data_relnum(curr_key, &relation) == UU_SUCCESS)
						{
						if(relation == UA_LINEAR_DIM)
							{
							edrf.key = curr_key;
							uc_retrieve_data(&edrf,sizeof(struct UA_generic_draft	)) ;
							if(ua_asso_check(&edrf) == UU_SUCCESS)
								{
								if(first == UU_TRUE)
									{
									/* save appropriate attributes */
									first = UU_FALSE;
									ua_save_dim_attr(type, &edrf);
								
									/* get attribute modifications from user */
									ua_get_new_attributes(type);
									if(!ua_check_new_attributes(type)) goto label1;
									}
			
								/* update appropriate attributes */
								ua_update_dim_attr(&edrf, type);
			
								/* check if regen or redisplay required */
								if(ua_is_regen_required(type))
									ua_regen_drafting(&edrf);
								else
									ua_regen_update_record(&edrf);
								}
							}
						}
					}
/*
.....Out of enitities
*/
					else break;
				}
label1:
			/* restore appropriate attributes */
			ua_restore_global_attr(type);
			break;
		}

fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_copy_text_attr(out, in, note, note_attr)
**		Copy entity text attribute bundle to a local array along with
**    some Global parameters.
**    PARAMETERS   
**       INPUT  : 
**          out						local text forms structure
**          in							global text forms structure
**          note						note entity structure
**          note_attr				note entity attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_copy_text_attr(out, in, note, note_attr)
	ATXT_FRM *in, *out;
	struct UA_txt_rec *note;
	struct UA_txtattr_rec *note_attr;
	{
	UU_REAL scale;

	uu_denter(UU_STRC, (us, "ua_copy_text_attr"));

	um_get_drwscale(&scale);

	out->color = note_attr->color; 
	out->prec = note_attr->prec;
	out->entity_site = note_attr->entity_site;
	out->path = note_attr->path; 
	out->align_hor = note_attr->align_hor; 
	out->align_ver = note_attr->align_ver; 
	out->txt_dens = note_attr->txt_dens; 
	out->height = note_attr->height*scale; 
	out->tangle = note->tangle; 
/*
.....Get first fontname match
.....instead of using default fontname
*/
/*	strcpy(out->fontname, in->fontname);*/
	ua_get_txt_fontname(note_attr->font,out->fontname);
	out->slant = note_attr->slant; 
	out->expn = note_attr->expn; 
	out->spacing = note_attr->spacing; 
	out->sub_sup = note_attr->sub_sup; 
	out->line_spacing = note_attr->line_spacing; 

fexit:
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_check_text(out, in, note, note_attr)
**		Determine if any field were changed by the user
**    PARAMETERS   
**       INPUT  : 
**          out						local text forms structure(contains users
**																				responses)
**          in							global text forms structure
**          note						note entity structure
**          note_attr				note entity attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_check_txt(out, in, note, note_attr)
	ATXT_FRM *in, *out;
	struct UA_txt_rec *note;
	struct UA_txtattr_rec *note_attr;
	{
	UU_REAL scale;

	uu_denter(UU_STRC, (us, "ua_check_text"));

	um_get_drwscale(&scale);

	if(out->color != note_attr->color) field[0] = UU_TRUE; 
	if(out->prec != note_attr->prec) field[2] = UU_TRUE;
	if(out->entity_site != note_attr->entity_site) field[12] = UU_TRUE;
	if(out->path != note_attr->path) field[6] = UU_TRUE; 
	if(out->align_hor != note_attr->align_hor) field[7] = UU_TRUE; 
	if(out->align_ver != note_attr->align_ver) field[8] = UU_TRUE; 
	if(out->txt_dens != note_attr->txt_dens) field[9] = UU_TRUE; 
	if(out->height != note_attr->height*scale) field[4] = UU_TRUE; 
	if(out->tangle != note->tangle) field[14] = UU_TRUE; 
	if(strcmp(out->fontname, in->fontname) != 0) field[1] = UU_TRUE;
	if(out->slant != note_attr->slant) field[13] = UU_TRUE; 
	if(out->expn != note_attr->expn) field[3] = UU_TRUE; 
	if(out->spacing != note_attr->spacing) field[5] = UU_TRUE; 
	if(out->sub_sup != note_attr->sub_sup) field[10] = UU_TRUE; 
	if(out->line_spacing != note_attr->line_spacing) field[11] = UU_TRUE; 

fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_update_txt_attr(note, out, in)
**		Update current entity attribute bundle from local copy of bundle
**    PARAMETERS   
**       INPUT  : 
**          note							note entity
**				out							new attribute bundle
**				in                      new user defined attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_update_txt_attr(note, out, in)
	struct UA_txt_rec *note;
	ATXT_FRM *in;
	struct UA_txtattr_rec *out;
	{
	UM_transf   rotmat;
	UU_REAL     scale;

	uu_denter(UU_STRC, (us, "ua_update_txt_attr"));

	um_get_drwscale(&scale);

	if( field[0]  )	out->color = in->color;
	if( field[1]  )	out->font = ua_get_txt_fontnum(in->fontname); 
	if( field[2]  )	out->prec = in->prec;
	if( field[3]  )	out->expn = in->expn;
	if( field[4]  )	out->height = in->height/scale;
	if( field[5]  )	out->spacing = in->spacing;
	if( field[6]  )	out->path = in->path;
	if( field[7]  )	out->align_hor = in->align_hor; 
	if( field[8]  )	out->align_ver = in->align_ver;
	if( field[9]  )	out->txt_dens = in->txt_dens;
	if( field[10]  )	out->sub_sup = in->sub_sup;
	if( field[11]  )	out->line_spacing = in->line_spacing;
	if( field[12]  )	out->entity_site = in->entity_site;
	if( field[13]  )	out->slant = in->slant;
	if( field[14]  )
		{
		if(note->tangle != 0.0)
			{
			um_rottf(out->plane,-note->tangle,rotmat);
			um_cctmtf(out->up,rotmat,out->up);
			}
		note->tangle = in->tangle;
		if (in->tangle != 0.0)		/* up must be rotated */
		  {
			um_rottf(out->plane,in->tangle,rotmat);
			um_cctmtf(out->up,rotmat,out->up);
		  }
		}

fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_save_dim_attr(type, adrf)
**		Save appropriate entity drafting attributes
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_save_dim_attr(type, edrf)
	int type;
	struct UA_generic_draft *edrf;
	{
	int i, j;
	UU_REAL scale;

	uu_denter(UU_STRC, (us, "ua_save_dim_attr type = %d", type));

	um_get_drwscale(&scale);

	switch(type)
		{
		case 2:        /* save dimension text */
			save_int[0] = UA_txt_place = edrf->txt_place;
			save_int[1] = UA_entity_site = edrf->entity_site;
			save_int[2] = UA_txt_entry = edrf->txt_entry;
			save_int[3] = UA_app_text;
			save_int[4] = UA_txt_orient = edrf->txt_orent;
			save_int[5] = UA_txt_just = edrf->txt_just;
			save_int[6] = UA_char_dens = edrf->txt_blk[0].text.line_density;
			save_int[7] = UA_char_color = edrf->txt_blk[0].text.color;
			save_int[8] = UA_txt_fontnum = edrf->txt_blk[0].text.line_font;
			save_real[0] = UA_char_size = edrf->char_size*scale;
			save_real[1] = UA_text_ang = edrf->txt_blk[0].tangle;
			save_real[2] = UA_grid_dist = edrf->grid_dist*scale;
			save_real[3] = UA_char_expansion = edrf->txt_blk[0].char_expansion;
			save_real[4] = UA_char_space = edrf->txt_blk[0].char_space;
			save_real[5] = UA_sub_sup_ratio = edrf->sub_sup_ratio;
			save_real[6] = UA_line_spacing = edrf->txt_blk[0].line_spacing;
			strcpy(save_char[0], edrf->txt_blk[0].fontname);
			strcpy(UA_txt_fontname, edrf->txt_blk[0].fontname);
			break;
		case 3:			/* save dimension attributes */
			save_int[0] = UA_linear_units = edrf->linear_units;
			save_int[1] =UA_ang_units = edrf->ang_units;
			save_int[2] =UA_dim_zero_sup = edrf->dim_zero_sup;
			save_int[3] =UA_dim_roundoff = edrf->dim_roundoff;
			save_int[4] =UA_fraction_size = edrf->fract_units;
			save_int[5] =UA_diam_symbol = edrf->diam_symbol;
			save_int[6] =UA_dia_place 	 = edrf->dia_place;
			save_int[7] = UA_d_lin_units = edrf->dual_l_units;
			save_int[8] =UA_d_ang_units  = edrf->dual_a_units;
			save_int[9] =UA_d_dim_z_sup  = edrf->d_dim_z_sup;
			save_int[10] =UA_d_dim_roundoff = edrf->d_dim_roundoff;
			save_int[11] =UA_d_frac_size = edrf->dual_f_units;
			save_int[12] =UA_radius_sym  = edrf->rad_symb;
			save_int[13] =UA_radius_place = edrf->rad_place;
			save_int[14] =UA_dual_format  = edrf->dual_format;
			save_int[15] =UA_dim_type		 = edrf->dim_type;
			save_int[16] =UA_dec_places 	 = edrf->dim_places;
			save_int[17] = UA_d_dec_places  = edrf->dual_place;
			save_real[0] = UA_dim_rnd_fact  = edrf->dim_rnd_fact;
			save_real[1] = UA_d_dim_rnd_fact = edrf->d_dim_rnd_fact;
			strcpy(save_char[0],UA_usr_rad_sym);
			strcpy(save_char[1],UA_d_dec_symbol);
			strcpy(save_char[2],UA_dec_symbol);
			strcpy(save_char[3],UA_usr_dia_sym);
			break;
		case 4:			/* save arrowhead attributes */
			save_int[0] = UA_arrow_symbol = edrf->arrow_blk[0].arrow_type;
			save_int[1] = UA_arrow_color = edrf->arrow_blk[0].arrow.color;
			save_int[2] = UA_arrow_place = edrf->arrow_place;
			save_real[0] = UA_arrow_dens = edrf->arrow_blk[0].arrow.line_density;
			save_real[1] = UA_arrow_size = edrf->arrow_size*scale;
			break;
		case 5:			/* save dimension line attr */
			i = -1;
			for(j=0;j<5;j++)
				{
				if(edrf->line_blk[j].subtype == dim_line)
					{
					save_int[0] = UA_dim_line_color = edrf->line_blk[j].line.color;
					save_real[0] = UA_dim_line_dens = edrf->line_blk[j].
																			line.line_density;
					i = j;
					break;
					}
				}
			if(i < 0)
				{
				for(j=0;j<5;j++)
					{
					if(edrf->arc_blk[j].subtype == dim_arc)
						{
						i = j;
						save_int[0] = UA_dim_line_color = edrf->arc_blk[j].
																					arc.color;
						save_real[0] = UA_dim_line_dens = edrf->arc_blk[j].
																			arc.line_density;
						break;
						}
					}
				}
			if(i < 0)
				{
				save_int[0] = UA_dim_line_color;
				save_real[0] = UA_dim_line_dens;
				}
			save_real[1] = UA_gap_dim_text = edrf->txt_gap*scale;
			break;
		case 6:			/* save extension line attr */
			i = -1;
			for(j=0;j<5;j++)
				{
				if(edrf->line_blk[j].subtype == ext_line)
					{
					save_int[0] = UA_ext_line_color = edrf->line_blk[j].line.color;
					save_real[0] = UA_ext_line_dens = edrf->line_blk[j].
																			line.line_density;
					i = j;
					break;
					}
				}
			if(i < 0)
				{
				for(j=0;j<5;j++)
					{
					if(edrf->arc_blk[j].subtype == ext_arc)
						{
						i = j;
						save_int[0] = UA_ext_line_color = edrf->arc_blk[j].
																					arc.color;
						save_real[0] = UA_ext_line_dens = edrf->arc_blk[j].
																			arc.line_density;
						break;
						}
					}
				}
			if(i < 0)
				{
				save_int[0] = UA_ext_line_color;
				save_real[0] = UA_ext_line_dens;
				}
			save_int[1] = UA_ext_line_sup	 = edrf->ext_line_sup;
			save_real[1] = UA_gap_geom_line = edrf->gap_to_geo*scale;
			save_real[2] = UA_ext_past_line = edrf->ext_past_line*scale;
			break;
		case 7: 			/* save leader attr */
			save_int[0] = UA_ldr_orient = edrf->lead_orient;
			save_int[1] = UA_ldr_location = edrf->leader_loc;
			save_real[0] = UA_ldr_stub_len = edrf->stub_length*scale;
			break;
		case 8: 			/* save tolerance attr */
			save_int[0] = UA_tol_dec_places = edrf->tol_places;
			save_int[1] = UA_d_tol_dec_plac = edrf->du_tol_pl;
			save_int[2] = UA_tol_method = edrf->tol_method; 
			save_int[3] = UA_tol_site = edrf->tol_site; 
			save_int[4] = UA_tol_zero_sup = edrf->tol_zero_sup; 
			save_int[5] = UA_d_tol_z_sup = edrf->d_tol_z_sup; 
			save_int[6] = UA_tol_roff_meth = edrf->tol_roundoff; 
			save_int[7] = UA_d_tol_roff_meth = edrf->d_tol_roff_meth; 
			if(edrf->etype == UA_ANGULAR_DIM)
				{
				save_real[0] = UA_lin_up_tol_val;
				save_real[1] = UA_lin_lo_tol_val;
				save_real[4] = UA_ang_up_tol_val = edrf->upper_tol;
				save_real[5] = UA_ang_lo_tol_val = edrf->lower_tol;
				}
			else
				{
				save_real[0] = UA_lin_up_tol_val = edrf->upper_tol;
				save_real[1] = UA_lin_lo_tol_val = edrf->lower_tol;
				save_real[4] = UA_ang_up_tol_val;
				save_real[5] = UA_ang_lo_tol_val;
				}
			save_real[2] = UA_d_lin_up_tol  = edrf->d_lin_up_tol;
			save_real[3] = UA_d_lin_lo_tol = edrf->d_lin_lo_tol;
			save_real[6] = UA_tol_rnd_fact = edrf->tol_rnd_fact;
			save_real[7] = UA_d_tol_rnd_fact = edrf->d_tol_rnd_fact;
			break;
		default:
			break;
		}

fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_save_global_attr(type)
**		Save appropriate Global drafting attributes
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_save_global_attr(type)
	int type;
	{
	uu_denter(UU_STRC, (us, "ua_save_global_attr type = %d", type));

	switch(type)
		{
		case 2:        /* save dimension text */
			save_int_global[0] = UA_txt_place;
			save_int_global[1] = UA_entity_site;
			save_int_global[2] = UA_txt_entry;
			save_int_global[3] = UA_app_text;
			save_int_global[4] = UA_txt_orient;
			save_int_global[5] = UA_txt_just;
			save_int_global[6] = UA_char_dens;
			save_int_global[7] = UA_char_color;
			save_int_global[8] = UA_txt_fontnum;
			save_real_global[0] = UA_char_size;
			save_real_global[1] = UA_text_ang;
			save_real_global[2] = UA_grid_dist;
			save_real_global[3] = UA_char_expansion;
			save_real_global[4] = UA_char_space;
			save_real_global[5] = UA_sub_sup_ratio;
			save_real_global[6] = UA_line_spacing;
			strcpy(save_char_global[0], UA_txt_fontname);
			break;
		case 3:			/* save dimension attributes */
			save_int_global[0] = UA_linear_units;
			save_int_global[1] =UA_ang_units;
			save_int_global[2] =UA_dim_zero_sup;
			save_int_global[3] =UA_dim_roundoff;
			save_int_global[4] =UA_fraction_size;
			save_int_global[5] =UA_diam_symbol;
			save_int_global[6] =UA_dia_place 	;
			save_int_global[7] = UA_d_lin_units;
			save_int_global[8] =UA_d_ang_units ;
			save_int_global[9] =UA_d_dim_z_sup ;
			save_int_global[10] =UA_d_dim_roundoff;
			save_int_global[11] =UA_d_frac_size;
			save_int_global[12] =UA_radius_sym ;
			save_int_global[13] =UA_radius_place;
			save_int_global[14] =UA_dual_format ;
			save_int_global[15] =UA_dim_type		;
			save_int_global[16] =UA_dec_places 	;
			save_int_global[17] = UA_d_dec_places ;
			save_real_global[0] = UA_dim_rnd_fact ;
			save_real_global[1] = UA_d_dim_rnd_fact;
			strcpy(save_char_global[0],UA_usr_rad_sym);
			strcpy(save_char_global[1],UA_d_dec_symbol);
			strcpy(save_char_global[2],UA_dec_symbol);
			strcpy(save_char_global[3],UA_usr_dia_sym);
			break;
		case 4:			/* save arrowhead attributes */
			save_int_global[0] = UA_arrow_symbol;
			save_int_global[1] = UA_arrow_color;
			save_int_global[2] = UA_arrow_place;
			save_real_global[0] = UA_arrow_dens;
			save_real_global[1] = UA_arrow_size;
			break;
		case 5:			/* save dimension line attr */
			save_int_global[0] = UA_dim_line_color;
			save_real_global[0] = UA_dim_line_dens;
			save_real_global[1] = UA_gap_dim_text;
			break;
		case 6:			/* save extension line attr */
			save_int_global[0] = UA_ext_line_color;
			save_int_global[1] = UA_ext_line_sup	;
			save_real_global[0] = UA_ext_line_dens;
			save_real_global[1] = UA_gap_geom_line;
			save_real_global[2] = UA_ext_past_line;
			break;
		case 7: 			/* save leader attr */
			save_int_global[0] = UA_ldr_orient;
			save_int_global[1] = UA_ldr_location;
			save_real_global[0] = UA_ldr_stub_len;
			break;
		case 8: 			/* save tolerance attr */
			save_int_global[0] = UA_tol_dec_places;
			save_int_global[1] = UA_d_tol_dec_plac;
			save_int_global[2] = UA_tol_method; 
			save_int_global[3] = UA_tol_site; 
			save_int_global[4] = UA_tol_zero_sup; 
			save_int_global[5] = UA_d_tol_z_sup; 
			save_int_global[6] = UA_tol_roff_meth; 
			save_int_global[7] = UA_d_tol_roff_meth; 
			save_real_global[0] = UA_lin_up_tol_val;
			save_real_global[1] = UA_lin_lo_tol_val;
			save_real_global[2] = UA_d_lin_up_tol ;
			save_real_global[3] = UA_d_lin_lo_tol;
			save_real_global[4] = UA_ang_up_tol_val;
			save_real_global[5] = UA_ang_lo_tol_val;
			save_real_global[6] = UA_tol_rnd_fact;
			save_real_global[7] = UA_d_tol_rnd_fact;
			break;
		default:
			break;
		}

fexit:
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_restore_global_attr(type)
**		Restore appropriate Global drafting attributes
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_restore_global_attr(type)
	int type;
	{

	uu_denter(UU_STRC, (us, "ua_restore_dim_attr( %d )", type));

	switch(type)
		{
		case 2:        /* restore dimension attributes */
			UA_txt_place = save_int_global[0] ;
			UA_entity_site = save_int_global[1] ;
			UA_txt_entry = save_int_global[2] ;
			UA_app_text = save_int_global[3] ;
			UA_txt_orient = save_int_global[4] ;
			UA_txt_just = save_int_global[5] ;
			UA_char_dens = save_int_global[6] ;
			UA_char_color = save_int_global[7] ;
			UA_txt_fontnum = save_int_global[8] ;
			UA_char_size = save_real_global[0];
			UA_text_ang = save_real_global[1];
			UA_grid_dist = save_real_global[2];
			UA_char_expansion = save_real_global[3];
			UA_char_space = save_real_global[4];
			UA_sub_sup_ratio = save_real_global[5];
			UA_line_spacing = save_real_global[6];
			strcpy( UA_txt_fontname, save_char_global[0]);
			break;
		case 3:			/* restore dimension attributes */
			UA_linear_units = save_int_global[0]  ;
			UA_ang_units = save_int_global[1] ;
			UA_dim_zero_sup = save_int_global[2] ;
			UA_dim_roundoff = save_int_global[3] ;
			UA_fraction_size = save_int_global[4] ;
			UA_diam_symbol = save_int_global[5] ;
			UA_dia_place  = save_int_global[6] 	;
			 UA_d_lin_units = save_int_global[7] ;
			UA_d_ang_units = save_int_global[8]  ;
			UA_d_dim_z_sup = save_int_global[9]  ;
			UA_d_dim_roundoff = save_int_global[10] ;
			UA_d_frac_size = save_int_global[11] ;
			UA_radius_sym = save_int_global[12]  ;
			UA_radius_place = save_int_global[13] ;
			UA_dual_format = save_int_global[14];
			UA_dim_type	 = save_int_global[15];
			UA_dec_places  = save_int_global[16];
			 UA_d_dec_places  = save_int_global[17];
			UA_dim_rnd_fact = save_real_global[0];
			UA_d_dim_rnd_fact = save_real_global[1];
			strcpy(UA_usr_rad_sym, save_char_global[0]);
			strcpy(UA_d_dec_symbol, save_char_global[1]);
			strcpy(UA_dec_symbol, save_char_global[2]);
			strcpy(UA_usr_dia_sym, save_char_global[3]);
			break;
		case 4:			/* restore arrowhead attributes */
			UA_arrow_symbol = save_int_global[0] ;
			UA_arrow_color = save_int_global[1] ;
			UA_arrow_place = save_int_global[2] ;
			UA_arrow_dens = save_real_global[0] ;
			UA_arrow_size = save_real_global[1] ;
			break;
		case 5:			/* restore dimension line attr */
			UA_dim_line_color = save_int_global[0] ;
			UA_dim_line_dens = save_real_global[0] ;
			UA_gap_dim_text = save_real_global[1] ;
			break;
		case 6:			/* restore extension line attr */
			UA_ext_line_color = save_int_global[0]  ;
			UA_ext_line_sup = save_int_global[1]  	;
			UA_ext_line_dens = save_real_global[0]  ;
			UA_gap_geom_line = save_real_global[1]  ;
			UA_ext_past_line = save_real_global[2]  ;
			break;
		case 7: 			/* restore leader attr */
			UA_ldr_orient = save_int_global[0] ;
			UA_ldr_location = save_int_global[1] ;
			UA_ldr_stub_len = save_real_global[0] ;
			break;
		case 8: 			/* restore tolerance */
			UA_tol_dec_places = save_int_global[0]  ;
			UA_d_tol_dec_plac = save_int_global[1]  ;
			UA_tol_method = save_int_global[2]  ; 
			UA_tol_site = save_int_global[3]  ; 
			UA_tol_zero_sup = save_int_global[4]  ; 
			UA_d_tol_z_sup = save_int_global[5]  ; 
			UA_tol_roff_meth = save_int_global[6]  ; 
			UA_d_tol_roff_meth = save_int_global[7]  ; 
			UA_lin_up_tol_val = save_real_global[0]  ;
			UA_lin_lo_tol_val = save_real_global[1]  ;
			UA_d_lin_up_tol = save_real_global[2]   ;
			UA_d_lin_lo_tol = save_real_global[3]  ;
			UA_ang_up_tol_val = save_real_global[4]  ;
			UA_ang_lo_tol_val = save_real_global[5]  ;
			UA_tol_rnd_fact = save_real_global[6]  ;
			UA_d_tol_rnd_fact = save_real_global[7]  ;
			break;
		default:
			break;
		}

fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_get_new_attributes(type)
**		Get users new attributes
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_get_new_attributes(type)
	int type;
	{

	uu_denter(UU_STRC, (us, "ua_restore_dim_attr( %d )", type));

	switch(type)
		{
		case 2:        /* modify dimension text */
			ua_set_txt_attr("atxtattr.frm");
			break;
		case 3:			/* dimension attributes */
			ua_set_dim_attr("adimattr.frm");
			break;
		case 4:			/* arrowhead attributes */
			ua_set_aro_attr("aaroattr.frm");
			break;
		case 5:			/* dimension line attr */
			ua_set_dl_attr("adlattr.frm");
			break;
		case 6:			/* extension line attr */
			ua_set_ext_attr("aextattr.frm");
			break;
		case 7: 			/* leader */
			ua_set_ll_attr("allattr.frm");
			break;
		case 8: 			/* mod tolerance */
			ua_set_tol_attr("atolattr.frm");
			break;
		default:
			break;
		}
fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_check_new_attributes(type)
**		Check if form fields changed by the user.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ua_check_new_attributes(type)
	int type;
	{
	int i, num;
	UU_LOGICAL status;

	uu_denter(UU_STRC, (us, "ua_check_new_attributes type = %d", type));

	switch(type)
		{
		case 2:        /* check dimension text */
			if(save_int[0] != UA_txt_place) field[0] = UU_TRUE;
			if(save_int[1] != UA_entity_site) field[1] = UU_TRUE;
			if(save_int[2] != UA_txt_entry) field[2] = UU_TRUE;
			if(save_int[3] != UA_app_text) field[3] = UU_TRUE;
			if(save_int[4] != UA_txt_orient) field[4] = UU_TRUE;
			if(save_int[5] != UA_txt_just) field[5] = UU_TRUE;
			if(save_int[6] != UA_char_dens) field[6] = UU_TRUE;
			if(save_int[7] != UA_char_color) field[7] = UU_TRUE;
			if(save_real[0] != UA_char_size) field[8] = UU_TRUE;
			if(save_real[1] != UA_text_ang) field[9] = UU_TRUE;
			if(save_real[2] != UA_grid_dist) field[10] = UU_TRUE;
			if(save_real[3] != UA_char_expansion) field[11] = UU_TRUE;
			if(save_real[4] != UA_char_space) field[12] = UU_TRUE;
			if(save_real[5] != UA_sub_sup_ratio) field[13] = UU_TRUE;
			if(save_real[6] != UA_line_spacing) field[14] = UU_TRUE;
			if(strcmp(save_char[0], UA_txt_fontname) != 0) field[15] = UU_TRUE;
			num = 16;
			break;
		case 3:			/* check dimension attributes */
			if(save_int[0] !=  UA_linear_units) field[0] = UU_TRUE;
			if(save_int[1] != UA_ang_units) field[1] = UU_TRUE;
			if(save_int[2] != UA_dim_zero_sup) field[2] = UU_TRUE;
			if(save_int[3] != UA_dim_roundoff) field[3] = UU_TRUE;
			if(save_int[4] != UA_fraction_size) field[4] = UU_TRUE;
			if(save_int[5] != UA_diam_symbol) field[5] = UU_TRUE;
			if(save_int[6] != UA_dia_place 	) field[6] = UU_TRUE;
			if(save_int[7] !=  UA_d_lin_units) field[7] = UU_TRUE;
			if(save_int[8] != UA_d_ang_units ) field[8] = UU_TRUE;
			if(save_int[9] != UA_d_dim_z_sup ) field[9] = UU_TRUE;
			if(save_int[10] != UA_d_dim_roundoff) field[10] = UU_TRUE;
			if(save_int[11] != UA_d_frac_size) field[11] = UU_TRUE;
			if(save_int[12] != UA_radius_sym ) field[12] = UU_TRUE;
			if(save_int[13] != UA_radius_place) field[13] = UU_TRUE;
			if(save_int[14] != UA_dual_format ) field[14] = UU_TRUE;
			if(save_int[15] != UA_dim_type		) field[15] = UU_TRUE;
			if(save_int[16] != UA_dec_places 	) field[16] = UU_TRUE;
			if(save_int[17] !=  UA_d_dec_places ) field[17] = UU_TRUE;
			if(save_real[0] !=  UA_dim_rnd_fact ) field[18] = UU_TRUE;
			if(save_real[1] !=  UA_d_dim_rnd_fact) field[19] = UU_TRUE;
			if(strcmp(save_char[0],UA_usr_rad_sym) != 0) field[20] = UU_TRUE;
			if(strcmp(save_char[1],UA_d_dec_symbol) != 0) field[21] = UU_TRUE;
			if(strcmp(save_char[2],UA_dec_symbol) != 0) field[22] = UU_TRUE;
			if(strcmp(save_char[3],UA_usr_dia_sym) != 0) field[23] = UU_TRUE;
			num = 24;
			break;
		case 4:			/* check arrowhead attributes */
			if(save_int[0] != UA_arrow_symbol) field[0] = UU_TRUE;
			if(save_int[1] != UA_arrow_color)  field[1] = UU_TRUE;
			if(save_int[2] != UA_arrow_place)  field[2] = UU_TRUE;
			if(save_real[0] != UA_arrow_dens)  field[3] = UU_TRUE;
			if(save_real[1] != UA_arrow_size)  field[4] = UU_TRUE;
			num = 5;
			break;
		case 5:			/* check dimension line attr */
			if(save_int[0] != UA_dim_line_color) field[0] = UU_TRUE;
			if(save_real[0] != UA_dim_line_dens) field[1] = UU_TRUE;
			if(save_real[1] != UA_gap_dim_text) field[2] = UU_TRUE;
			num = 3;
			break;
		case 6:			/* check extension line attr */
			if(save_int[0] != UA_ext_line_color) field[0] = UU_TRUE;
			if(save_int[1] != UA_ext_line_sup	) field[1] = UU_TRUE;
			if(save_real[0] != UA_ext_line_dens) field[2] = UU_TRUE;
			if(save_real[1] != UA_gap_geom_line) field[3] = UU_TRUE;
			if(save_real[2] != UA_ext_past_line) field[4] = UU_TRUE;
			num = 5;
			break;
		case 7: 			/* check leader attr */
			if(save_int[0] != UA_ldr_orient) field[0] = UU_TRUE;
			if(save_int[1] != UA_ldr_location) field[1] = UU_TRUE;
			if(save_real[0] != UA_ldr_stub_len) field[2] = UU_TRUE;
			num = 3;
			break;
		case 8: 			/* check tolerance attr */
			if(save_int[0] != UA_tol_dec_places) field[0] = UU_TRUE;
			if(save_int[1] != UA_d_tol_dec_plac) field[1] = UU_TRUE;
			if(save_int[2] != UA_tol_method) field[2] = UU_TRUE; 
			if(save_int[3] != UA_tol_site) field[3] = UU_TRUE; 
			if(save_int[4] != UA_tol_zero_sup) field[4] = UU_TRUE; 
			if(save_int[5] != UA_d_tol_z_sup) field[5] = UU_TRUE; 
			if(save_int[6] != UA_tol_roff_meth) field[6] = UU_TRUE; 
			if(save_int[7] != UA_d_tol_roff_meth) field[7] = UU_TRUE; 
			if(save_real[0] != UA_lin_up_tol_val) field[8] = UU_TRUE;
			if(save_real[1] != UA_lin_lo_tol_val) field[9] = UU_TRUE;
			if(save_real[2] != UA_d_lin_up_tol ) field[10] = UU_TRUE;
			if(save_real[3] != UA_d_lin_lo_tol) field[11] = UU_TRUE;
			if(save_real[4] != UA_ang_up_tol_val) field[12] = UU_TRUE;
			if(save_real[5] != UA_ang_lo_tol_val) field[13] = UU_TRUE;
			if(save_real[6] != UA_tol_rnd_fact) field[14] = UU_TRUE;
			if(save_real[7] != UA_d_tol_rnd_fact) field[15] = UU_TRUE;
			num = 16;
			break;
		default:
			status = UU_FALSE;
			goto fexit;
		}

	/* check if any fields changed */
	status = UU_FALSE;
	for(i=0;i<num;i++)
		{
		if(field[i])
			{
			status = UU_TRUE;
			goto fexit;
			}
		}

fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_update_dim_attr(edrf, type)
**		Update appropriate entity attributes
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_update_dim_attr(edrf, type)
	struct UA_generic_draft *edrf;
	int type;
	{
	int i;
	UU_REAL scale;

	uu_denter(UU_STRC, (us, "ua_update_dim_attr"));

	um_get_drwscale(&scale);

	switch(type)
		{
		case 2:        /* update text attributes */
			if(field[0] ) edrf->txt_place = UA_txt_place;
			if(field[1] ) edrf->entity_site = UA_entity_site;
			if(field[2] ) edrf->txt_entry = UA_txt_entry;
			if(field[4] ) edrf->txt_orent = UA_txt_orient;
			if(field[5] ) edrf->txt_just = UA_txt_just;
			if(field[8] ) edrf->char_size = (UA_char_size/scale);
			if(field[10]) edrf->grid_dist = (UA_grid_dist/scale);
			if(field[13] ) edrf->sub_sup_ratio = UA_sub_sup_ratio;
			for(i=0;i<10;i++)
				{
				if(field[15])
					{
					strcpy(edrf->txt_blk[i].fontname, UA_txt_fontname);
					edrf->txt_blk[i].text.line_font = UA_txt_fontnum;
					}
				if(field[6] ) edrf->txt_blk[i].text.line_density = UA_char_dens;
				if(field[7] ) edrf->txt_blk[i].text.color = UA_char_color;
				if(field[5] ) edrf->txt_blk[i].txt_just = UA_txt_just;
				if(field[9] ) edrf->txt_blk[i].tangle = UA_text_ang;
				if(field[8] ) edrf->txt_blk[i].txt_size = (UA_char_size/scale);
				if(field[13] ) edrf->txt_blk[i].sub_super = UA_sub_sup_ratio;
				if(field[11] ) edrf->txt_blk[i].char_expansion = UA_char_expansion;
				if(field[12] ) edrf->txt_blk[i].char_space = UA_char_space;
				if(field[14] ) edrf->txt_blk[i].line_spacing = UA_line_spacing;
				}
			break;
		case 3:			/* dimension attributes */
			if(field[0] ) edrf->linear_units = UA_linear_units;
			if(field[1] ) edrf->ang_units = UA_ang_units;
			if(field[2] ) edrf->dim_zero_sup = UA_dim_zero_sup;
			if(field[3] ) edrf->dim_roundoff = UA_dim_roundoff;
			if(field[4] ) edrf->fract_units = UA_fraction_size;
			if(field[5] ) edrf->diam_symbol = UA_diam_symbol;
			if(field[6] ) edrf->dia_place = UA_dia_place;
			if(field[7] ) edrf->dual_l_units = UA_d_lin_units;
			if(field[8] ) edrf->dual_a_units = UA_d_ang_units;
			if(field[9] ) edrf->d_dim_z_sup = UA_d_dim_z_sup;
			if(field[10] ) edrf->d_dim_roundoff = UA_d_dim_roundoff;
			if(field[11] ) edrf->dual_f_units = UA_d_frac_size;
			if(field[12] ) edrf->rad_symb = UA_radius_sym;
			if(field[13] ) edrf->rad_place = UA_radius_place;
			if(field[14] ) edrf->dual_format = UA_dual_format;
			if(field[15] ) edrf->dim_type = UA_dim_type	;
			if(field[16] ) edrf->dim_places = UA_dec_places;
			if(field[17] ) edrf->dual_place = UA_d_dec_places;
			if(field[18] ) edrf->dim_rnd_fact = UA_dim_rnd_fact;
			if(field[19] ) edrf->d_dim_rnd_fact = UA_d_dim_rnd_fact;
			break;
		case 4:			/* update arrowhead attributes */
			if(field[2] ) edrf->arrow_place = UA_arrow_place;
			if(field[4] ) edrf->arrow_size = UA_arrow_size/scale;
			for(i=0;i<10;i++)
				{
				if(field[0] ) edrf->arrow_blk[i].arrow_type = UA_arrow_symbol;
				if(field[1] ) edrf->arrow_blk[i].arrow.color = UA_arrow_color;
				if(field[3] ) edrf->arrow_blk[i].arrow.line_density = UA_arrow_dens;
				if(field[4] ) edrf->arrow_blk[i].size = UA_arrow_size/scale;
				}
			break;
		case 5:			/* update dimension line attr */
			if(field[2] ) edrf->txt_gap = UA_gap_dim_text/scale;
			for(i=0;i<5;i++)
				{
				if(edrf->line_blk[i].subtype == dim_line)
					{
					if(field[1] )
								edrf->line_blk[i].line.line_density = UA_dim_line_dens;
					if(field[0] )
								edrf->line_blk[i].line.color = UA_dim_line_color;
					}
				}
			for(i=0;i<5;i++)
				{
				if(edrf->arc_blk[i].subtype == dim_arc)
					{
					if(field[1] )
							edrf->arc_blk[i].arc.line_density = UA_dim_line_dens;
					if(field[0] )
							edrf->arc_blk[i].arc.color = UA_dim_line_color;
					}
				}
			break;
		case 6:			/* update extension line attr */
			if(field[1] ) edrf->ext_line_sup = UA_ext_line_sup;
			if(field[3] ) edrf->gap_to_geo = UA_gap_geom_line/scale;
			if(field[4] ) edrf->ext_past_line = UA_ext_past_line/scale;
			for(i=0;i<5;i++)
				{
				if(edrf->line_blk[i].subtype == ext_line)
					{
					if(field[2] )
								edrf->line_blk[i].line.line_density = UA_ext_line_dens;
					if(field[0] ) 
								edrf->line_blk[i].line.color = UA_ext_line_color;
					}
				}
			for(i=0;i<5;i++)
				{
				if(edrf->arc_blk[i].subtype == ext_arc)
					{
					if(field[2] )
								edrf->arc_blk[i].arc.line_density = UA_ext_line_dens;
					if(field[0] )
								edrf->arc_blk[i].arc.color = UA_ext_line_color;
					}
				}
			break;
		case 7: 			/* update leader attr */
			if(field[0] ) edrf->lead_orient = UA_ldr_orient;
			if(field[1] ) edrf->leader_loc = UA_ldr_location;
			if(field[2] ) edrf->stub_length = UA_ldr_stub_len/scale;
			break;
		case 8: 			/* update tolerance */
			if(field[2] ) edrf->tol_method = UA_tol_method;
			if(field[3] ) edrf->tol_site = UA_tol_site;
			if(field[0] ) edrf->tol_places = UA_tol_dec_places;
			if(field[1] ) edrf->du_tol_pl = UA_d_tol_dec_plac;
			if(field[4] ) edrf->tol_zero_sup = UA_tol_zero_sup;
			if(field[5] ) edrf->d_tol_z_sup = UA_d_tol_z_sup;
			if(field[6] ) edrf->tol_roundoff = UA_tol_roff_meth;
			if(field[7] ) edrf->d_tol_roff_meth = UA_d_tol_roff_meth;
			if(field[14] ) edrf->tol_rnd_fact = UA_tol_rnd_fact;
			if(field[15] ) edrf->d_tol_rnd_fact = UA_d_tol_rnd_fact;
			if(field[8] ) edrf->upper_tol = UA_lin_up_tol_val;
			if(field[10] ) edrf->d_lin_up_tol = UA_d_lin_up_tol;
			if(field[9] ) edrf->lower_tol = UA_lin_lo_tol_val;
			if(field[11] ) edrf->d_lin_lo_tol = UA_d_lin_lo_tol;
			if(edrf->etype == UA_ANGULAR_DIM)
				{
				if(field[12] ) edrf->upper_tol = UA_ang_up_tol_val;
				if(field[13] ) edrf->lower_tol = UA_ang_lo_tol_val;
				}
			break;
		default:
			break;
		}
fexit:
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_is_regen_required(type)
**		Check if regen required or did user only change display attribute
**	   like color or linestyle.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ua_is_regen_required(type)
	int type;
	{
	UU_LOGICAL status;

	uu_denter(UU_STRC, (us, "ua_is_regen_required type = %d", type));

	switch(type)
		{
		case 2:        /* check dimension text */
			if(field[0] == UU_TRUE || field[1] == UU_TRUE 
									  	  || field[2] == UU_TRUE 
										  || field[3] == UU_TRUE 
										  || field[4] == UU_TRUE 
										  || field[5] == UU_TRUE 
										  || field[8] == UU_TRUE 
										  || field[9] == UU_TRUE 
										  || field[10] == UU_TRUE 
										  || field[11] == UU_TRUE 
										  || field[12] == UU_TRUE 
										  || field[13] == UU_TRUE 
										  || field[14] == UU_TRUE )
				status = UU_TRUE;
			else
				status = UU_FALSE;
			break;
		case 3:			/* check dimension attributes */
			status = UU_TRUE;
			break;
		case 4:			/* check arrowhead attributes */
			if(field[2] == UU_TRUE || field[4] == UU_TRUE)
				status = UU_TRUE;
			else
				status = UU_FALSE;
			break;
		case 5:			/* check dimension line attr */
			if(field[2] == UU_TRUE)
				status = UU_TRUE;
			else
				status = UU_FALSE;
			break;
		case 6:			/* check extension line attr */
			if(field[1] == UU_TRUE || field[3] == UU_TRUE || field[4] == UU_TRUE)
				status = UU_TRUE;
			else
				status = UU_FALSE;
			break;
		case 7: 			/* check leader attr */
			status = UU_TRUE;
			break;
		case 8: 			/* check tolerance attr */
			status = UU_TRUE;
			break;
		default:
			status = UU_FALSE;
			goto fexit;
		}

fexit:
	uu_dexit;
	return(status);
	}
