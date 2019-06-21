/*********************************************************************
**    NAME         :  adrfcom.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       adrfcom.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:10
*********************************************************************/

#ifndef ADRFCOMH


/* C-code from SAL source file "adrfcom.sh" */
#include "usysdef.h"
#include "go.h"
#include "adraft.h"

#ifdef UA_COM
#define EXT
#else
#define EXT extern
#endif

EXT UU_REAL	UA_char_slant;
EXT char		UA_dec_symbol[2];
EXT int		UA_tol_dec_places;
EXT UU_REAL	UA_char_expansion;
EXT UU_REAL	UA_d_lin_lo_tol;
EXT int		UA_arrow_color;
EXT int		UA_ext_line_sup;
EXT int		UA_dim_zero_sup;
EXT int		UA_d_ang_units;
EXT int		UA_txt_just;
EXT UU_REAL	UA_char_dens;
EXT int		UA_entity_site;
EXT UU_REAL	UA_oblique_angle;
EXT int		UA_d_tol_dec_plac;
EXT UU_REAL	UA_d_lin_up_tol;
EXT UU_REAL	UA_d_dim_rnd_fact;
EXT int		UA_tol_method;
EXT int		UA_d_dim_roundoff;
EXT int		UA_dim_line_color;
EXT int		UA_ldr_orient;
EXT int		UA_d_lin_units;
EXT UU_REAL	UA_arrow_dens;
EXT int		UA_d_tol_roff_meth;
EXT char		UA_usr_dia_sym[13];
EXT int		UA_tol_zero_sup;
EXT UU_REAL	UA_gap_geom_line;
EXT UU_REAL	UA_char_size;
EXT UU_REAL	UA_grid_dist;
EXT UU_REAL	UA_dim_line_dens;
EXT int		UA_radius_sym;
EXT int		UA_txt_fontnum;
EXT int		UA_dims_disp;
EXT int		UA_d_dec_places;
EXT char		UA_usr_rad_sym[13];
EXT UU_REAL	UA_d_tol_rnd_fact;
EXT UU_REAL	UA_arrow_size;
EXT int		UA_color_rel;
EXT int		UA_fraction_size;
EXT UU_REAL	UA_line_spacing;
EXT int		UA_ext_line_color;
EXT int		UA_radius_place;
EXT int		UA_dim_mode;
EXT int		UA_d_dim_z_sup;
EXT int		UA_d_units_sym;
EXT int		UA_txt_place;
EXT enum UALORIENT	UA_label_orient;
EXT int		UA_dim_line_font;
EXT int		UA_ang_units;
EXT int		UA_txt_orient;
EXT UU_REAL	UA_ang_lo_tol_val;
EXT int		UA_drafting_std;
EXT UU_REAL	UA_dim_rnd_fact;
EXT int		UA_ldr_location;
EXT UU_REAL	UA_ext_line_dens;
EXT int		UA_dim_roundoff;
EXT UU_REAL	UA_ang_up_tol_val;
EXT int		UA_d_tol_z_sup;
EXT UU_REAL	UA_lin_lo_tol_val;
EXT int		UA_tol_roff_meth;
EXT char		UA_d_dec_symbol[2];
EXT int		UA_lin_arr_char;
EXT Gtxprec	UA_txt_precision;
EXT UU_REAL	UA_text_ang;
EXT int		UA_ext_line_font;
EXT UU_REAL	UA_ext_past_line;
EXT int		UA_dim_type;
EXT UU_REAL	UA_lin_up_tol_val;
EXT int		UA_linear_units;
EXT int		UA_dec_places;
EXT UU_REAL	UA_tol_rnd_fact;
EXT int		UA_tol_site;
EXT UU_REAL	UA_char_space;
EXT int		UA_dual_format;
EXT int		UA_arrow_place;
EXT int		UA_app_text;
EXT UU_REAL	UA_gap_dim_text;
EXT int		UA_diam_symbol;
EXT int		UA_units_sym;
EXT char		UA_txt_fontname[17];
EXT UU_REAL	UA_sub_sup_ratio;
EXT int		UA_txt_entry;
EXT UU_REAL	UA_ldr_stub_len;
EXT int		UA_d_frac_size;
EXT int		UA_arrow_symbol;
EXT int		UA_dia_place;
EXT int		UA_text_box_ovrd;
EXT int		UA_stack_grid;
EXT int		UA_char_color;

EXT int		UA_char_color_note;
EXT UU_REAL	UA_char_slant_note;
EXT UU_REAL	UA_char_expansion_note;
EXT UU_REAL	UA_char_dens_note;
EXT int		UA_entity_site_note;
EXT UU_REAL	UA_line_spacing_note;
EXT Gtxprec	UA_txt_precision_note;
EXT UU_REAL	UA_text_ang_note;
EXT UU_REAL	UA_char_space_note;
EXT UU_REAL	UA_txt_fontnum_note;
EXT char		UA_txt_fontname_note[17];
EXT UU_REAL	UA_sub_sup_ratio_note;
EXT UU_REAL	UA_char_size_note;

#undef EXT

#define ADRFCOMH
#endif
