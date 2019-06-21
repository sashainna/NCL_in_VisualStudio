/*********************************************************************
**    NAME         : ainitent.c
**       CONTAINS:
**				ua_initent_tol
**				ua_initent_apptext
**				ua_initent_txt_blk
**				ua_initent_arc_blk
**				ua_initent_line_blk
**				ua_initent_arrow_blk
**				ua_initent_asso_blk
**   	     ua_init_entity	(uses above routines )
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       ainitent.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:35
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) ainitent.c 3.3 3/23/88 16:56:11 single"};
#else
static char uu_sccsident[]={"@(#) ainitent.c 3.3 3/23/88 16:56:11 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"

/*********************************************************************
**    E_FUNCTION     : ua_initent_tol(entity)
**			Initialize the tolerance specific fields of the entity
**			from global drafting variables.
**    PARAMETERS   
**       INPUT  : 
**				entity:			Generic draft record
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_initent_tol(entity)
struct UA_generic_draft	(*entity);
	{
	uu_denter(UU_STRC,(us,"ua_initent_tol(entity=%s)", "..."));

	(*entity).tol_method = UA_tol_method;
	(*entity).tol_site = UA_tol_site;
	(*entity).tol_places = UA_tol_dec_places;
	(*entity).du_tol_pl = UA_d_tol_dec_plac;
	(*entity).tol_zero_sup = UA_tol_zero_sup;
	(*entity).d_tol_z_sup = UA_d_tol_z_sup;
	(*entity).tol_roundoff = UA_tol_roff_meth;
	(*entity).d_tol_roff_meth = UA_d_tol_roff_meth;
	(*entity).tol_rnd_fact = UA_tol_rnd_fact;
	(*entity).d_tol_rnd_fact = UA_d_tol_rnd_fact;
	(*entity).upper_tol = UA_lin_up_tol_val;
	(*entity).d_lin_up_tol = UA_d_lin_up_tol;
	(*entity).lower_tol = UA_lin_lo_tol_val;
	(*entity).d_lin_lo_tol = UA_d_lin_lo_tol;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_init_entity(etype, esubtype, entity)
**       Initialize a drafting entity generic entity structure
**
**    PARAMETERS   
**       INPUT  : 
**          etype	-	Entity types
**				esubtype	-	Entity subtype
**				entity	-	Generic entity record to init
**       OUTPUT :  
**          entity	-	Initialized generic entity record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_init_entity(etype, esubtype, entity)
int		etype;
int		esubtype;
struct UA_generic_draft	(*entity);
	{
	int		i;
	UU_REAL	scal_factor;

	uu_denter(UU_STRC,(us,"ua_init_entity(etype=%d, esubtype=%d,\
	entity=%s)", etype, esubtype, "..."));

	(*entity).etype = etype;
	(*entity).subtype = esubtype;
	um_get_drwscale(&(scal_factor));
	ua_setcpln(&((*entity)));
	ua_initent_tol(&((*entity)));
	ua_initent_apptext(&((*entity)));
	(*entity).txt_place = UA_txt_place;
	(*entity).entity_site = UA_entity_site;
	(*entity).txt_entry = UA_txt_entry;
	(*entity).txt_orent = UA_txt_orient;
	(*entity).txt_just = UA_txt_just;
	(*entity).stack_grid = UA_stack_grid;
	(*entity).units_sym = UA_units_sym;
	(*entity).d_units_sym = UA_d_units_sym;
	(*entity).dim_places = UA_dec_places;
	(*entity).dual_place = UA_d_dec_places;
	(*entity).dia_place = UA_dia_place;
	(*entity).rad_place = UA_radius_place;
	(*entity).diam_symbol = UA_diam_symbol;
	(*entity).rad_symb = UA_radius_sym;
	(*entity).linear_units = UA_linear_units;
	(*entity).dual_l_units = UA_d_lin_units;
	(*entity).ang_units = UA_ang_units;
	(*entity).dual_a_units = UA_d_ang_units;
	(*entity).fract_units = UA_fraction_size;
	(*entity).dual_f_units = UA_d_frac_size;
	(*entity).dual_format = UA_dual_format;
	(*entity).dim_type = UA_dim_type;
	(*entity).dim_zero_sup = UA_dim_zero_sup;
	(*entity).d_dim_z_sup = UA_d_dim_z_sup;
	(*entity).dim_roundoff = UA_dim_roundoff;
	(*entity).ext_line_sup = UA_ext_line_sup;
	(*entity).lead_orient = UA_ldr_orient;
	(*entity).leader_loc = UA_ldr_location;
	(*entity).arrow_place = UA_arrow_place;
	(*entity).draft_stand = UA_drafting_std;
	(*entity).dims_display = UA_dims_disp;
	(*entity).grid_dist = ( UA_grid_dist/scal_factor );
	(*entity).char_slant = UA_char_slant;
	(*entity).char_size = ( UA_char_size/scal_factor );
	(*entity).sub_sup_ratio = UA_sub_sup_ratio;
	(*entity).dim_rnd_fact = UA_dim_rnd_fact;
	(*entity).gap_to_geo = ( UA_gap_geom_line/scal_factor );
	(*entity).ext_past_line = ( UA_ext_past_line/scal_factor );
	(*entity).oblique_angle = UA_oblique_angle;
	(*entity).txt_gap = ( UA_gap_dim_text/scal_factor );
	(*entity).stub_length = ( UA_ldr_stub_len/scal_factor );
	(*entity).arrow_size = ( UA_arrow_size/scal_factor );
	(*entity).d_dim_roundoff = UA_d_dim_roundoff;
	(*entity).d_dim_rnd_fact = UA_d_dim_rnd_fact;
	(*entity).dim_value = 0.000000e+000;
	(*entity).dim2_value = 0.000000e+000;
	(*entity).dim_origin[0] = 0.000000e+000;
	(*entity).dim_origin[1] = 0.000000e+000;
	(*entity).dim_origin[2] = 0.000000e+000;
	(*entity).xh_pattern = 0;
	(*entity).xh_angle = 0.000000e+000;
	(*entity).xh_spacing = 0.000000e+000;
	(*entity).txt_blk_use = 0;
	(*entity).arc_blk_use = 0;
	(*entity).line_blk_use = 0;
	(*entity).arrow_blk_use = 0;
	(*entity).asso_blk_use = 0;
	ua_initent_txt_blk(&((*entity)),1,0);
	ua_initent_arc_blk(&((*entity)),1,0);
	ua_initent_line_blk(&((*entity)),1,0);
	ua_initent_arrow_blk(&((*entity)),1,0);
	ua_initent_asso_blk(&((*entity)),1,0);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_initent_apptext(entity)
**			Initialize the appended text specific fields of the entity.
**    PARAMETERS   
**       INPUT  : 
**				entity:			Generic draft record
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_initent_apptext(entity)
struct UA_generic_draft	(*entity);
	{
	uu_denter(UU_STRC,(us,"ua_initent_apptext(entity=%s)", "..."));
	(*entity).appn_text = UA_app_text;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_initent_txt_blk(entity, istart, iuse)
**			Initialize entity text blocks starting with istart element.
**			Initialize from other iuse element unless = 0: init
**			from drafting modal variables.
**    PARAMETERS   
**       INPUT  : 
**				entity:			Generic draft record
**				istart:			block to start initializing
**				iuse:				=0 initialize from drafting modal vars
**									>0 initialize from this block
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_initent_txt_blk(entity, istart, iuse)
struct UA_generic_draft	(*entity);
int		istart;
int		iuse;
	{
	int		i;
	UU_REAL	scal_factor;

	uu_denter(UU_STRC,(us,"ua_initent_txt_blk(entity=%s,\
	istart=%d, iuse=%d)", "...", istart, iuse));

	um_get_drwscale(&(scal_factor));
	for(i=(istart-1);i<10;i++)
		{
		(*entity).txt_blk[i].subtype = main_txt1;
		if( ( iuse<1 ) )
			{
			(*entity).txt_blk[i].text.line_font = UA_txt_fontnum;
			(*entity).txt_blk[i].text.line_density = UA_char_dens;
			(*entity).txt_blk[i].text.color = UA_char_color;
			(*entity).txt_blk[i].txt_just = UA_txt_just;
			strcpy((*entity).txt_blk[i].fontname,UA_txt_fontname);
			(*entity).txt_blk[i].tangle = UA_text_ang;
			(*entity).txt_blk[i].slant = UA_char_slant;
			(*entity).txt_blk[i].txt_size = ( UA_char_size/scal_factor);
			(*entity).txt_blk[i].sub_super = UA_sub_sup_ratio;
			(*entity).txt_blk[i].char_expansion = UA_char_expansion;
			(*entity).txt_blk[i].char_space = UA_char_space;
			(*entity).txt_blk[i].line_spacing = UA_line_spacing;
			}
		else
			{
			(*entity).txt_blk[i].text.line_font = (*entity).txt_blk[
			    iuse-1].text.line_font;
			(*entity).txt_blk[i].text.line_density = (*entity).txt_blk
			    [iuse-1].text.line_density;
			(*entity).txt_blk[i].text.color = (*entity).txt_blk[iuse
			    -1].text.color;
			(*entity).txt_blk[i].txt_just = (*entity).txt_blk[iuse-1].
			    txt_just;
			strcpy((*entity).txt_blk[i].fontname,(*entity).txt_blk[
			    iuse-1].fontname);
			(*entity).txt_blk[i].slant = (*entity).txt_blk[iuse-1].
			    slant;
			(*entity).txt_blk[i].tangle = (*entity).txt_blk[iuse-1].
			    tangle;
			(*entity).txt_blk[i].txt_size = (*entity).txt_blk[iuse-1].
			    txt_size;
			(*entity).txt_blk[i].sub_super = (*entity).txt_blk[iuse-1]
			    .sub_super;
			(*entity).txt_blk[i].char_expansion = (*entity).txt_blk[
			    iuse-1].char_expansion;
			(*entity).txt_blk[i].char_space = (*entity).txt_blk[iuse
			    -1].char_space;
			(*entity).txt_blk[i].line_spacing = (*entity).txt_blk[iuse
			    -1].line_spacing;
			}
		(*entity).txt_blk[i].char_cnt = 0;
		strcpy((*entity).txt_blk[i].tstring,"");
		(*entity).txt_blk[i].origin[0] = 0.000000e+000;
		(*entity).txt_blk[i].origin[1] = 0.000000e+000;
		(*entity).txt_blk[i].origin[2] = 0.000000e+000;
		(*entity).txt_blk[i].dx = 0.000000e+000;
		(*entity).txt_blk[i].dy = 0.000000e+000;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_initent_arrow_blk(entity, istart, iuse)
**			Initialize entity arrow blocks starting with istart element.
**			Initialize from other iuse element unless = 0: init
**			from drafting modal variables.
**    PARAMETERS   
**       INPUT  : 
**				entity:			Generic draft record
**				istart:			block to start initializing
**				iuse:				=0 initialize from drafting modal vars
**									>0 initialize from this block
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_initent_arrow_blk(entity, istart, iuse)
struct UA_generic_draft	(*entity);
int		istart;
int		iuse;
	{
	int		i;
	UU_REAL	scal_factor;

	uu_denter(UU_STRC,(us,"ua_initent_arrow_blk(entity=%s,\
	istart=%d, iuse=%d)", "...", istart, iuse));

	um_get_drwscale(&(scal_factor));
	for(i=(istart-1);i<10;i++)
		{
		if( ( iuse<1 ) )
			{
			(*entity).arrow_blk[i].arrow_type = UA_arrow_symbol;
			(*entity).arrow_blk[i].arrow.line_density = UA_arrow_dens;
			(*entity).arrow_blk[i].arrow.color = UA_arrow_color;
			(*entity).arrow_blk[i].size = ( UA_arrow_size/scal_factor);
			}
		else
			{
			(*entity).arrow_blk[i].arrow_type = (*entity).arrow_blk[
			    iuse-1].arrow_type;
			(*entity).arrow_blk[i].arrow.line_density = (*entity).
			    arrow_blk[iuse-1].arrow.line_density;
			(*entity).arrow_blk[i].arrow.color = (*entity).arrow_blk[
			    iuse-1].arrow.color;
			(*entity).arrow_blk[i].size = (*entity).arrow_blk[iuse-1].
			    size;
			}
		(*entity).arrow_blk[i].arrow.line_font = 0;
		(*entity).arrow_blk[i].location[0] = 0.000000e+000;
		(*entity).arrow_blk[i].location[1] = 0.000000e+000;
		(*entity).arrow_blk[i].location[2] = 0.000000e+000;
		(*entity).arrow_blk[i].aangle = 0.000000e+000;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_initent_line_blk(entity, istart, iuse)
**			Initialize entity line blocks starting with istart element.
**			Initialize from other iuse element unless = 0: init
**			from drafting modal variables.
**    PARAMETERS   
**       INPUT  : 
**				entity:			Generic draft record
**				istart:			block to start initializing
**				iuse:				=0 initialize from drafting modal vars
**									>0 initialize from this block
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_initent_line_blk(entity, istart, iuse)
struct UA_generic_draft	(*entity);
int		istart;
int		iuse;
	{
	int		i;
	int		j;

	uu_denter(UU_STRC,(us,"ua_initent_line_blk(entity=%s,\
	istart=%d,\ iuse=%d)", "...", istart, iuse));

	for(i=(istart-1);i<5;i++)
		{
		if( ( iuse<1 ) )
			{
			(*entity).line_blk[i].line.line_font = UA_ext_line_font;
			(*entity).line_blk[i].line.line_density = UA_ext_line_dens
			    ;
			(*entity).line_blk[i].line.color = UA_ext_line_color;
			}
		else
			{
			(*entity).line_blk[i].line.line_font = (*entity).line_blk[
			    iuse-1].line.line_font;
			(*entity).line_blk[i].line.line_density = (*entity).
			    line_blk[iuse-1].line.line_density;
			(*entity).line_blk[i].line.color = (*entity).line_blk[iuse
			    -1].line.color;
			}
		(*entity).line_blk[i].subtype = ext_line;
		(*entity).line_blk[i].num_pts = 0;
		for(j=0;j<50;j++)
			{
			(*entity).line_blk[i].line_seg[j][0] = 0.000000e+000;
			(*entity).line_blk[i].line_seg[j][1] = 0.000000e+000;
			(*entity).line_blk[i].line_seg[j][2] = 0.000000e+000;
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_initent_asso_blk(entity, istart, iuse)
**			Initialize entity assoc blocks starting with istart element.
**			Initialize from other iuse element unless = 0: init
**			from drafting modal variables.
**    PARAMETERS   
**       INPUT  : 
**				entity:			Generic draft record
**				istart:			block to start initializing
**				iuse:				=0 initialize from drafting modal vars
**									>0 initialize from this block
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_initent_asso_blk(entity, istart, iuse)
struct UA_generic_draft	(*entity);
int		istart;
int		iuse;
	{
	int		i;

	uu_denter(UU_STRC,(us,"ua_initent_asso_blk(entity=%s,\
	istart=%d, iuse=%d)", "...", istart, iuse));

	i = istart;
	for(i=(istart-1);i<50;i++)
		{
		(*entity).asso_blk[i].asso_type = 0;
		(*entity).asso_blk[i].modifier = 0;
		(*entity).asso_blk[i].key = 0;
		(*entity).asso_blk[i].location[0] = 0.000000e+000;
		(*entity).asso_blk[i].location[1] = 0.000000e+000;
		(*entity).asso_blk[i].location[2] = 0.000000e+000;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_initent_arc_blk(entity, istart, iuse)
**			Initialize entity arc blocks starting with istart element.
**			Initialize from other iuse element unless = 0: init
**			from drafting modal variables.
**    PARAMETERS   
**       INPUT  : 
**				entity:			Generic draft record
**				istart:			block to start initializing
**				iuse:				=0 initialize from drafting modal vars
**									>0 initialize from this block
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_initent_arc_blk(entity, istart, iuse)
struct UA_generic_draft	(*entity);
int		istart;
int		iuse;
	{
	int		i;
	int		j;

	uu_denter(UU_STRC,(us,"ua_initent_arc_blk(entity=%s,\
	istart=%d, iuse=%d)", "...", istart, iuse));

	for(i=(istart-1);i<5;i++)
		{
		if( ( iuse<1 ) )
			{
			(*entity).arc_blk[i].arc.line_font = UA_ext_line_font;
			(*entity).arc_blk[i].arc.line_density = UA_ext_line_dens;
			(*entity).arc_blk[i].arc.color = UA_ext_line_color;
			}
		else
			{
			(*entity).arc_blk[i].arc.line_font = (*entity).arc_blk[
			    iuse-1].arc.line_font;
			(*entity).arc_blk[i].arc.line_density = (*entity).arc_blk[
			    iuse-1].arc.line_density;
			(*entity).arc_blk[i].arc.color = (*entity).arc_blk[iuse-1]
			    .arc.color;
			}
		(*entity).arc_blk[i].subtype = ext_arc;
		(*entity).arc_blk[i].num_pts = 0;
		(*entity).arc_blk[i].center_pt[0] = 0.000000e+000;
		(*entity).arc_blk[i].center_pt[1] = 0.000000e+000;
		(*entity).arc_blk[i].center_pt[2] = 0.000000e+000;
		(*entity).arc_blk[i].radius = 0.000000e+000;
		j = 1;
		for(j=0;j<50;j++)
			{
			(*entity).arc_blk[i].angles[j] = 0.000000e+000;
			}
		}
	uu_dexit;
	}
