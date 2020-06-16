
/*********************************************************************
**    NAME         : tigdrf1.c
**       CONTAINS:
** 		      ua_init_drafting
**   			  ua_init_standard(pathname, save_flag)
**					ua_get_linear_units
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigdrf1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:45
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) adrfinit.c 3.3 2/8/88 13:55:57 single"};
#else
static char uu_sccsident[]={"@(#) adrfinit.c 3.3 2/8/88 13:55:57 double"};
#endif

#define UA_COM 1
#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "go3.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#undef 	UA_COM

int /* BIT: */ UA_dump_choice_set;
int UA_write4;			/* write(4) direction */
										/* 0= 	default (trace file) */
										/* 1= write to standard out */

/*MILLS: following four declarations caused warnings on VMS */
/*       these variables are defined in adrfcom.h */
/*char UA_dec_symbol[2];		/* decimal symbol				*/
/*char UA_d_dec_symbol[2];	/* dual decimal symbol		*/
/*char UA_usr_dia_sym[13];	/* user diameter symbol		*/
/*char UA_usr_rad_sym[13];	/* user radius symbol		*/
char UA_SAVED_FILE_NAME[41]; /* user 	default std file name */

static char		PATHSYM[1025] = "DRAFTSTD";
/*********************************************************************
**    E_FUNCTION     : ua_init_drafting()
**       Init the UA_DRAFTING system common data areas
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_init_drafting()
	{
	char		fontenv[101];
	char		fontnnn[101];
	char		write4[101];
	char		pathname[81];
	int		retcode;
	int		fontnum;
	int		mode;

	uu_denter(UU_STRC,(us," ua_init_drafting()"));

	UA_dump_choice_set = 0x0;
	UA_write4 = 0;
	UA_txt_orient = 0;
	UA_linear_units = 0;
	UA_ang_units = 0;
	UA_units_sym = 0;
	UA_diam_symbol = 2;
	UA_dia_place = 2;
	UA_radius_sym = 1;
	UA_radius_place = 2;
	UA_d_lin_units = 4;
	UA_d_ang_units = 3;
	UA_d_units_sym = 0;
	UA_d_dim_z_sup = 0;
	UA_dual_format = 0;
	UA_dim_type = 0;
	UA_dim_zero_sup = 0;
	UA_dim_roundoff = 3;
	UA_d_dim_roundoff = 3;
	UA_tol_method = 0;
	UA_tol_site = 2;
	UA_tol_zero_sup = 0;
	UA_d_tol_z_sup = 0;
	UA_tol_roff_meth = 3;
	UA_d_tol_roff_meth = 3;
	UA_ldr_orient = 0;
	UA_ldr_location = 1;
	UA_arrow_symbol = 1;
	UA_drafting_std = 0;
	strcpy(UA_dec_symbol,".");
	strcpy(UA_d_dec_symbol,",");
	UA_grid_dist = 3.000000e-001;
	UA_char_size = 1.400000e-001;
	UA_dim_rnd_fact = 1.000000e-003;
	UA_d_dim_rnd_fact = 1.000000e-001;
	UA_lin_up_tol_val = 5.000000e-003;
	UA_lin_lo_tol_val = 5.000000e-003;
	UA_d_lin_up_tol = 1.000000e-001;
	UA_d_lin_lo_tol = 1.000000e-001;
	UA_ang_up_tol_val = 1.000000e-002;
	UA_ang_lo_tol_val = 1.000000e-002;
	UA_tol_rnd_fact = 1.000000e-003;
	UA_d_tol_rnd_fact = 1.000000e-001;
	UA_gap_dim_text = 3.130000e-002;
	UA_ldr_stub_len = 1.250000e-001;
	UA_arrow_size = 1.560000e-001;
	UA_gap_geom_line = 6.250000e-002;
	UA_ext_past_line = 1.250000e-001;
	UA_txt_place = 0;
	UA_entity_site = 0;
	UA_txt_entry = 0;
	UA_app_text = 4;
	UA_txt_just = 0;
	UA_stack_grid = 0;
	UA_char_color = 6;
	UA_char_dens = 1.000000e+000;
	UA_dec_places = 3;
	UA_fraction_size = 3;
	UA_d_dec_places = 3;
	UA_d_frac_size = 3;
	UA_tol_dec_places = 3;
	UA_d_tol_dec_plac = 3;
	UA_dim_line_font = 1;
	UA_dim_line_color = 6;
	UA_dim_line_dens = 1.000000e+000;
	UA_arrow_color = 6;
	UA_arrow_dens = 1.000000e+000;
	UA_arrow_place = 0;
	UA_lin_arr_char = 0;
	UA_dims_disp = 0;
	UA_text_box_ovrd = 0;
	UA_dim_mode = 0;
	UA_color_rel = 0;
	UA_ext_line_font = 1;
	UA_ext_line_color = 6;
	UA_ext_line_dens = 1.000000e+000;
	UA_ext_line_sup = 0;
	UA_text_ang = 0.000000e+000;
	UA_char_expansion = 1.000000e+000;
	UA_char_space = 2.000000e-001;
	UA_char_slant = 0.000000e+000;
	UA_sub_sup_ratio = 7.500000e-001;
	UA_line_spacing = 2.000000e-001;
	UA_oblique_angle = 0.000000e+000;
	strcpy(UA_txt_fontname,"drafting");
	UA_txt_fontnum = 6;
	strcpy(UA_usr_dia_sym,"D");
	strcpy(UA_usr_rad_sym,"R");
	UA_txt_precision = UG_STROKE;
	UA_label_orient = LO_HORIZ;
	uu_dexit;
	}
