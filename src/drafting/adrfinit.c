
/*********************************************************************
**    NAME         : adrfinit.c
**       CONTAINS:
** 		      ua_init_drafting
**   			  ua_init_standard(pathname, save_flag)
**					ua_get_linear_units
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**			adrfinit.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:05:34
*********************************************************************/

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
#include "xenv1.h"
#undef 	UA_COM

int /* BIT: */ UA_dump_choice_set;
int UA_write4;			/* write(4) direction */
										/* 0= 	default (trace file) */
										/* 1= write to standard out */

/*char UA_dec_symbol[2];		 decimal symbol         */
/*char UA_d_dec_symbol[2];	 dual decimal symbol		*/
/*char UA_usr_dia_sym[13];	 user diameter symbol		*/
/*char UA_usr_rad_sym[13];	 user radius symbol		*/
char UA_SAVED_FILE_NAME[41];  /* user default std file name     */

extern int UA_local_dir;
static char		PATHSYM[UX_MAX_PATH_LEN] = "DRAFTSTD";
static char    DIRSYM[UX_MAX_PATH_LEN] = "DRAFTSTDIR";
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
	char		pathname[UX_MAX_PATH_LEN], filename[UX_MAX_PATH_LEN];
	int		retcode;
	int		fontnum;

	uu_denter(UU_STRC,(us,"SAL ua_init_drafting()"));

	us_init_autility();
	us_init_adefault();
	us_init_axhatch();
	ua_xhatch_init();
	ua_initpp();
	UA_dump_choice_set = 0x0;
	uu_sgetenv("DRFWRITE4",write4);
	if( (strcmp( write4, "default" ) == 0) )
		{
		UA_write4 = 0;
		}
	else if( (strcmp( write4, "stdout" ) == 0) )
		{
		UA_write4 = 1;
		}
	else if( (strcmp( write4, "stderr" ) == 0) )
		{
		UA_write4 = 2;
		}
	else
		{
		UA_write4 = 0;
		}
	uu_sgetenv(PATHSYM,filename);
	sprintf(pathname,"%s%s",DIRSYM,filename);
	ua_init_standard(pathname,UU_TRUE);
	strcpy(fontenv,"FONT_");
	strcat(fontenv,UA_txt_fontname);
	uu_sgetenv(fontenv,fontnnn);
	if( ( strcmp( fontnnn, "" ) == 0 ) )
		{
		UA_txt_fontnum = 1;
		UA_txt_fontnum_note = 1;
		}
	else
		{
		retcode = sscanf(fontnnn,"FONT%d",&(fontnum));
		if( ( retcode==1 ) )
			{
			UA_txt_fontnum = fontnum;
			UA_txt_fontnum_note = fontnum;
			}
		else
			{
			UA_txt_fontnum = 1;
			UA_txt_fontnum_note = 1;
			}
		}
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : ua_init_standard(pathname, save_flag)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_init_standard(pathname, save_flag)
char		pathname[1];
UU_LOGICAL	save_flag;
	{
	int		options;
	int		status;
	int		mode;
	UU_LOGICAL ua_load_standard();
	int nc;
	UX_pathname fname,qname;
	char sympath[1025];

	uu_denter(UU_STRC,(us,"SAL ua_init_standard(pathname=%s, save_flag=%s)",
			pathname, save_flag?"TRUE":"FALSE"));

/*
.....Get full name of file by
.....expanding environmental variables
.....Bobby  -  3/29/94
*/
	ul_get_fname(pathname,qname);
	fname[0] = UX_QUOTE_CHAR;
	strcpy(&fname[1],qname);
	nc = strlen(fname);
	fname[nc] = UX_QUOTE_CHAR; fname[nc+1] = '\0';

	mode = 0;
	options = 1;
	strcpy(UA_SAVED_FILE_NAME," ");
	status = ux_access1(fname,&(mode),options);
	UA_txt_precision = UG_STROKE;
	UA_txt_precision_note = UG_STROKE;
	UA_label_orient = LO_HORIZ;
	if( ( ( ( status==0 )&&( strcmp( fname, "" ) != 0 ) )&&( mode==0 ) ) )
		{
		if(!ua_load_standard(fname))
			{
			ua_set_default_values(fname);
			ua_init_text();
			ua_save_standard(fname);
			}
		}
	else
		{
		if(UA_local_dir ==1)
			{
				UA_local_dir =0;
				sprintf(sympath,"%s/%s",DIRSYM,pathname);
				ua_swap_standard(sympath,UU_FALSE);
			}
			else
			{
				uu_uerror1(13,1,fname);
				if( save_flag )
				{
					ua_set_default_values(fname);
					ua_init_text();
					ua_save_standard(fname);
				}		
			}
		}
	um_update_drwunits();
	ua_init_text();
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : ua_set_default_values(pathname)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_default_values(pathname)
char		pathname[1];
	{

	uu_denter(UU_STRC,(us,"ua_set_default_values"));

	if( ( strcmp( pathname, "ANSISTD.DAT" ) == 0 ) )
		{
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
		UA_dec_places = 3;
		UA_d_dec_places = 3;
		UA_tol_dec_places = 3;
		UA_d_tol_dec_plac = 3;
		strcpy(UA_dec_symbol,".");
		strcpy(UA_d_dec_symbol,",");
		UA_grid_dist = 3.000000e-001;
		UA_char_size= 1.400000e-001;
		UA_char_size_note = 1.400000e-001;
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
		}
	else if( ( strcmp( pathname, "DINSTD.DAT" ) == 0 ) )
		{
		UA_txt_orient = 2;
		UA_linear_units = 3;
		UA_ang_units = 0;
		UA_units_sym = 0;
		UA_diam_symbol = 2;
		UA_dia_place = 2;
		UA_radius_sym = 1;
		UA_radius_place = 2;
		UA_d_lin_units = 0;
		UA_d_ang_units = 0;
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
		UA_ldr_orient = 2;
		UA_ldr_location = 1;
		UA_arrow_symbol = 5;
		UA_drafting_std = 2;
		UA_dec_places = 0;
		UA_d_dec_places = 0;
		UA_tol_dec_places = 2;
		UA_d_tol_dec_plac = 2;
		strcpy(UA_dec_symbol,",");
		strcpy(UA_d_dec_symbol,".");
		UA_grid_dist = 7.500000e+000;
		UA_char_size = 3.175000e+000;
		UA_char_size_note = 3.175000e+000;
		UA_dim_rnd_fact = 3.000000e-002;
		UA_d_dim_rnd_fact = 3.000000e+000;
		UA_lin_up_tol_val = 7.500000e-002;
		UA_lin_lo_tol_val = 7.500000e-002;
		UA_d_lin_up_tol = 3.000000e+000;
		UA_d_lin_lo_tol = 3.000000e+000;
		UA_ang_up_tol_val = 3.000000e-001;
		UA_ang_lo_tol_val = 3.000000e-001;
		UA_tol_rnd_fact = 3.000000e-002;
		UA_d_tol_rnd_fact = 3.000000e+000;
		UA_gap_dim_text = 8.000000e-001;
		UA_ldr_stub_len = 3.175000e+000;
		UA_arrow_size = 4.000000e+000;
		UA_gap_geom_line = 1.587500e+000;
		UA_ext_past_line = 3.175000e+000;
		}
	else if( ( strcmp( pathname, "ISOSTD.DAT" ) == 0 ) )
		{
		UA_txt_orient = 2;
		UA_linear_units = 3;
		UA_ang_units = 2;
		UA_units_sym = 0;
		UA_diam_symbol = 2;
		UA_dia_place = 2;
		UA_radius_sym = 1;
		UA_radius_place = 2;
		UA_d_lin_units = 0;
		UA_d_ang_units = 0;
		UA_d_units_sym = 0;
		UA_d_dim_z_sup = 0;
		UA_dual_format = 0;
		UA_dim_type = 0;
		UA_dim_zero_sup = 0;
		UA_dim_roundoff = 3;
		UA_d_dim_roundoff = 3;
		UA_tol_method = 0;
		UA_tol_site = 4;
		UA_tol_zero_sup = 0;
		UA_d_tol_z_sup = 0;
		UA_tol_roff_meth = 3;
		UA_d_tol_roff_meth = 3;
		UA_ldr_orient = 0;
		UA_ldr_location = 1;
		UA_arrow_symbol = 1;
		UA_drafting_std = 1;
		UA_dec_places = 0;
		UA_d_dec_places = 0;
		UA_tol_dec_places = 2;
		UA_d_tol_dec_plac = 2;
		strcpy(UA_dec_symbol,",");
		strcpy(UA_d_dec_symbol,".");
		UA_grid_dist = 7.500000e+000;
		UA_char_size = 3.175000e+000;
		UA_char_size_note = 3.175000e+000;
		UA_dim_rnd_fact = 3.000000e-002;
		UA_d_dim_rnd_fact = 3.000000e+000;
		UA_lin_up_tol_val = 7.500000e-002;
		UA_lin_lo_tol_val = 7.500000e-002;
		UA_d_lin_up_tol = 3.000000e+000;
		UA_d_lin_lo_tol = 3.000000e+000;
		UA_ang_up_tol_val = 3.000000e-001;
		UA_ang_lo_tol_val = 3.000000e-001;
		UA_tol_rnd_fact = 3.000000e-002;
		UA_d_tol_rnd_fact = 3.000000e+000;
		UA_gap_dim_text = 8.000000e-001;
		UA_ldr_stub_len = 3.175000e+000;
		UA_arrow_size = 4.000000e+000;
		UA_gap_geom_line = 1.587500e+000;
		UA_ext_past_line = 3.175000e+000;
		}
	else if( ( strcmp( pathname, "BSISTD.DAT" ) == 0 ) )
		{
		UA_txt_orient = 2;
		UA_linear_units = 3;
		UA_ang_units = 0;
		UA_units_sym = 0;
		UA_diam_symbol = 2;
		UA_dia_place = 2;
		UA_radius_sym = 1;
		UA_radius_place = 2;
		UA_d_lin_units = 0;
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
		UA_arrow_symbol = 5;
		UA_drafting_std = 3;
		UA_dec_places = 0;
		UA_d_dec_places = 0;
		UA_tol_dec_places = 2;
		UA_d_tol_dec_plac = 2;
		strcpy(UA_dec_symbol,",");
		strcpy(UA_d_dec_symbol,".");
		UA_grid_dist = 7.500000e+000;
		UA_char_size = 3.175000e+000;
		UA_char_size_note = 3.175000e+000;
		UA_dim_rnd_fact = 3.000000e-002;
		UA_d_dim_rnd_fact = 3.000000e+000;
		UA_lin_up_tol_val = 7.500000e-002;
		UA_lin_lo_tol_val = 7.500000e-002;
		UA_d_lin_up_tol = 3.000000e+000;
		UA_d_lin_lo_tol = 3.000000e+000;
		UA_ang_up_tol_val = 3.000000e-001;
		UA_ang_lo_tol_val = 3.000000e-001;
		UA_tol_rnd_fact = 3.000000e-002;
		UA_d_tol_rnd_fact = 3.000000e+000;
		UA_gap_dim_text = 8.000000e-001;
		UA_ldr_stub_len = 3.175000e+000;
		UA_arrow_size = 4.000000e+000;
		UA_gap_geom_line = 1.587500e+000;
		UA_ext_past_line = 3.175000e+000;
		}
	else
		{
		UA_txt_orient = 0;
		UA_linear_units = 0;
		UA_ang_units = 0;
		UA_units_sym = 0;
		UA_diam_symbol = 2;
		UA_dia_place = 2;
		UA_radius_sym = 1;
		UA_radius_place = 3;
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
		UA_dec_places = 3;
		UA_d_dec_places = 3;
		UA_tol_dec_places = 3;
		UA_d_tol_dec_plac = 3;
		strcpy(UA_dec_symbol,".");
		strcpy(UA_d_dec_symbol,",");
		UA_grid_dist = 3.000000e-001;
		UA_char_size = 1.250000e-001;
		UA_char_size_note = 1.250000e-001;
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
		}
	UA_txt_place = 0;
	UA_entity_site = 0;
	UA_entity_site_note = 0;
	UA_txt_entry = 0;
	UA_app_text = 4;
	UA_txt_just = 0;
	UA_stack_grid = 0;
	UA_char_color = 6;
	UA_char_color_note = 6;
	UA_char_dens = 1.000000e+000;
	UA_char_dens_note = 1.000000e+000;
	UA_fraction_size = 3;
	UA_d_frac_size = 3;
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
	UA_text_ang_note = 0.000000e+000;
	UA_char_expansion = 1.000000e+000;
	UA_char_expansion_note = 1.000000e+000;
	UA_char_space = 2.000000e-001;
	UA_char_space_note = 2.000000e-001;
	UA_char_slant = 0.000000e+000;
	UA_char_slant_note = 0.000000e+000;
	UA_sub_sup_ratio = 7.500000e-001;
	UA_sub_sup_ratio_note = 7.500000e-001;
	UA_line_spacing = 2.000000e-001;
	UA_line_spacing_note = 2.000000e-001;
	UA_oblique_angle = 0.000000e+000;
	strcpy(UA_txt_fontname,"drafting");
	strcpy(UA_txt_fontname_note,"drafting");
	strcpy(UA_usr_dia_sym,"D");
	strcpy(UA_usr_rad_sym,"R");
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : ua_get_linear_units(drw_units)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_get_linear_units(drw_units)
int		(*drw_units);
	{

	uu_denter(UU_STRC,(us,"SAL ua_get_linear_units(drw_units=%d)", *drw_units));

	(*drw_units) = UA_linear_units;
	uu_dexit;
	return 0;
	}
