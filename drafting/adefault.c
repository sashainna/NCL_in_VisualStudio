/*********************************************************************
**    NAME         : adefault.c
**       CONTAINS:
**       ua_save_standard
**       ua_load_standard
**			ua_save_new_standard
**			ua_load_new_standard
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       adefault.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:32
*********************************************************************/


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "xenv1.h"
#include "uhep.h"

static char		UA_VERSION[1025] = "DRAFTSTD.DAT VERSION 3.3";

extern char UA_SAVED_FILE_NAME[41];

struct UA_defaults 	{
			char file_version[41];	/* file id and version no.	*/
			int	txt_place;		/*	Text placement method	*/
			int	entity_site;	/*	entity site code			*/
			int	txt_entry;		/*	text entry method			*/
			int	app_text;		/*	appended text location	*/
			int	txt_orient;		/*	text orentation			*/
			int	txt_just;		/*	text justification		*/
			int	stack_grid;		/*	stacking grid in use		*/
			int char_color;		/* character color			*/
			int dec_places;		/* decimal places				*/
			int linear_units;	/* linear units				*/
			int ang_units;		/* angular units				*/
			int fraction_size;	/* fraction size				*/
			int	units_sym;		/*	dimension units symbol	*/
			int diam_symbol;	/* diameter symbol			*/
			int dia_place;		/* diameter place				*/
			int radius_sym;		/* radius symbol				*/
			int radius_place;	/* radius place				*/
			int d_dec_places;	/* dual dim decimal places	*/
			int d_lin_units;	/* dual dim linear units	*/
			int d_ang_units;	/* dual dim angular units	*/
			int d_frac_size;	/* dual dim fractional size*/
			int	d_units_sym;	/* dual units symbol			*/
			int	d_dim_z_sup;	/*	dual dim zero suppres	*/
			int	dual_format;	/*	dual dim format			*/
			int	dim_type;		/*	dimension types				*/
			int	dim_zero_sup;	/*	dim zero suppression		*/
			int dim_roundoff;	/* dimension roundoff		*/
			int tol_dec_places;/* tolerance decimal places*/
			int d_tol_dec_plac;/* dual tol decimal places	*/
			int tol_method;		/* tolerancing method		*/ 
			int tol_site;		/* tolerancing site			*/
			int	tol_zero_sup;	/*	tol zero suppresion		*/
			int	d_tol_z_sup;	/*	dual tol zero supres		*/
			int	tol_roff_meth;	/*	tolerance roundoff meth	*/
			int dim_line_font;	/* dimension line font		*/
			int dim_line_color;/* dimension line color		*/
			int	ldr_orient;		/*	leader line orientation	*/
			int	ldr_location;	/*	leader location			*/
			int arrow_symbol;	/* arrow symbol				*/
			int arrow_color;	/* arrow color					*/
			int arrow_place;	/* arrow placement			*/
			int drafting_std;	/* drafting standard			*/
			int lin_arr_char;	/* line/arrow/string relation*/
			int	dims_disp;		/*	dimension display			*/
			int text_box_ovrd;	/* text box override			*/
			int dim_mode;		/* dimensioning mode			*/
			int color_rel;		/* color relationship		*/
			int ext_line_font;	/*	extension line font		*/
			int ext_line_color;/* extension line color		*/
			int ext_line_sup;	/* extension line suppress	*/
			int d_dim_roundoff;/* dual dim roundoff meth	*/
			int d_tol_roff_meth;/* dual tol roundoff meth	*/
			UU_REAL		text_ang;		/* note/ua_label text angle	*/
			UU_REAL		grid_dist;		/*	grid distance				*/
			UU_REAL		char_size;		/*	character size				*/
			UU_REAL		char_expansion;/* character exp. ratio		*/
			UU_REAL		char_space;		/* character space ratio	*/
			UU_REAL		char_slant;		/*	character slant			*/
			UU_REAL		sub_sup_ratio;	/*	sub-superscript ratio	*/
			UU_REAL		line_spacing;	/* line spacing ratio		*/
			UU_REAL		dim_rnd_fact;	/*	dimension rounding fact	*/
			UU_REAL		lin_up_tol_val;/* linear upper tol value	*/
			UU_REAL		lin_lo_tol_val;/* linear lower tol value	*/
			UU_REAL		ang_up_tol_val;/*	angular upper tol value	*/
			UU_REAL		ang_lo_tol_val;/* angular lower tol value	*/
			UU_REAL		tol_rnd_fact;	/*	tolerance rounding fact	*/
			UU_REAL 	gap_dim_text;	/* gap to dimension text	*/
			UU_REAL		ldr_stub_len;	/*	leader stub length		*/
			UU_REAL		arrow_size;		/*	arrowhead size				*/
			UU_REAL		gap_geom_line;	/*	gap from geom to line	*/
			UU_REAL		ext_past_line;	/*	extension past dim line	*/
			UU_REAL		oblique_angle;	/*	oblique ext line angle	*/
			UU_REAL		char_dens;		/* character density			*/
			UU_REAL		dim_line_dens;	/* dimension line density	*/
			UU_REAL		arrow_dens;		/* arrow density				*/
			UU_REAL		ext_line_dens;	/* extension line density	*/
			UU_REAL		d_dim_rnd_fact;/* dual dim rounding fact	*/
			UU_REAL		d_tol_rnd_fact;/* dual tol rounding fact	*/
			UU_REAL		d_lin_up_tol;	/* dual linear upper tol	*/
			UU_REAL		d_lin_lo_tol;	/* dual linear lower tol	*/
			char txt_fontname[17];	/*	text font name				*/
			char dec_symbol[2];		/* decimal symbol				*/
			char d_dec_symbol[2];	/* dual decimal symbol		*/
			char usr_dia_sym[13];	/* user diameter symbol		*/
			char usr_rad_sym[13];	/* user radius symbol		*/
			 
/* new note text attributes */

			int			char_color_note;
			UU_REAL		char_slant_note;
			UU_REAL		char_expansion_note;
			UU_REAL		char_dens_note;
			int			entity_site_note;
			UU_REAL		line_spacing_note;
			Gtxprec		txt_precision_note;
			UU_REAL		text_ang_note;
			UU_REAL		char_space_note;
			char			txt_fontname_note[17];
			UU_REAL		sub_sup_ratio_note;
			UU_REAL		char_size_note;	
			}	;

/*********************************************************************
**    E_FUNCTION     : us_init_adefault()
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
void us_init_adefault()
	{
	}

/*********************************************************************
**    E_FUNCTION     : ua_load_standard(pathname)
**       Loads attribute settings from the DRAFTSTD.DAT file 
**       replacing the current values.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL ua_load_standard(pathname)
char		pathname[1];
	{
	struct UA_defaults	draftstd;
	char		interp[7];
	char		form[7];
	char		type[7];
	int		options, number, lu, error;
	UU_LOGICAL status;

	uu_denter(UU_STRC,(us,"ua_load_standard(pathname=%s)",pathname));

	status = UU_TRUE;
	strcpy(type,"r+");
	strcpy(form,"STREAM");
	strcpy(interp,"BINARY");
	options = 1;
	error = ux_open_to_data(pathname,type,form,interp,&(lu), options);
	if( ( error==0 ) )
		{
		number = 1;
		error = ux_read(lu,&(draftstd),sizeof( draftstd ) ,&(number),options);
		if( ( strcmp( UA_VERSION, draftstd.file_version ) != 0 ) )
			{
			uu_uerror1(UA_DRAFTING,4,pathname);
			error = ux_delete(pathname,1);
			status = UU_FALSE;
			}
		else
			{
			UA_txt_place = draftstd.txt_place;
			UA_entity_site = draftstd.entity_site;
			UA_txt_entry = draftstd.txt_entry;
			UA_app_text = draftstd.app_text;
			UA_txt_orient = draftstd.txt_orient;
			UA_txt_just = draftstd.txt_just;
			UA_stack_grid = draftstd.stack_grid;
			UA_char_color = draftstd.char_color;
			UA_char_dens = draftstd.char_dens;
			UA_dec_places = draftstd.dec_places;
			UA_linear_units = draftstd.linear_units;
			UA_ang_units = draftstd.ang_units;
			UA_fraction_size = draftstd.fraction_size;
			UA_units_sym = draftstd.units_sym;
			UA_diam_symbol = draftstd.diam_symbol;
			UA_dia_place = draftstd.dia_place;
			UA_radius_sym = draftstd.radius_sym;
			UA_radius_place = draftstd.radius_place;
			UA_d_dec_places = draftstd.d_dec_places;
			UA_d_lin_units = draftstd.d_lin_units;
			UA_d_ang_units = draftstd.d_ang_units;
			UA_d_frac_size = draftstd.d_frac_size;
			UA_d_units_sym = draftstd.d_units_sym;
			UA_d_dim_z_sup = draftstd.d_dim_z_sup;
			UA_dual_format = draftstd.dual_format;
			UA_dim_type = draftstd.dim_type;
			UA_dim_zero_sup = draftstd.dim_zero_sup;
			UA_dim_roundoff = draftstd.dim_roundoff;
			UA_tol_dec_places = draftstd.tol_dec_places;
			UA_d_tol_dec_plac = draftstd.d_tol_dec_plac;
			UA_tol_method = draftstd.tol_method;
			UA_tol_site = draftstd.tol_site;
			UA_tol_zero_sup = draftstd.tol_zero_sup;
			UA_d_tol_z_sup = draftstd.d_tol_z_sup;
			UA_tol_roff_meth = draftstd.tol_roff_meth;
			UA_dim_line_font = draftstd.dim_line_font;
			UA_dim_line_color = draftstd.dim_line_color;
			UA_dim_line_dens = draftstd.dim_line_dens;
			UA_ldr_orient = draftstd.ldr_orient;
			UA_ldr_location = draftstd.ldr_location;
			UA_arrow_symbol = draftstd.arrow_symbol;
			UA_arrow_color = draftstd.arrow_color;
			UA_arrow_dens = draftstd.arrow_dens;
			UA_arrow_place = draftstd.arrow_place;
			UA_drafting_std = draftstd.drafting_std;
			UA_lin_arr_char = draftstd.lin_arr_char;
			UA_dims_disp = draftstd.dims_disp;
			UA_text_box_ovrd = draftstd.text_box_ovrd;
			UA_dim_mode = draftstd.dim_mode;
			UA_color_rel = draftstd.color_rel;
			UA_ext_line_font = draftstd.ext_line_font;
			UA_ext_line_color = draftstd.ext_line_color;
			UA_ext_line_dens = draftstd.ext_line_dens;
			UA_ext_line_sup = draftstd.ext_line_sup;
			UA_text_ang = draftstd.text_ang;
			UA_grid_dist = draftstd.grid_dist;
			UA_char_size = draftstd.char_size;
			UA_char_expansion = draftstd.char_expansion;
			UA_char_space = draftstd.char_space;
			UA_char_slant = draftstd.char_slant;
			UA_sub_sup_ratio = draftstd.sub_sup_ratio;
			UA_line_spacing = draftstd.line_spacing;
			UA_dim_rnd_fact = draftstd.dim_rnd_fact;
			UA_lin_up_tol_val = draftstd.lin_up_tol_val;
			UA_lin_lo_tol_val = draftstd.lin_lo_tol_val;
			UA_ang_up_tol_val = draftstd.ang_up_tol_val;
			UA_ang_lo_tol_val = draftstd.ang_lo_tol_val;
			UA_tol_rnd_fact = draftstd.tol_rnd_fact;
			UA_gap_dim_text = draftstd.gap_dim_text;
			UA_ldr_stub_len = draftstd.ldr_stub_len;
			UA_arrow_size = draftstd.arrow_size;
			UA_gap_geom_line = draftstd.gap_geom_line;
			UA_ext_past_line = draftstd.ext_past_line;
			UA_oblique_angle = draftstd.oblique_angle;
			strcpy(UA_txt_fontname,draftstd.txt_fontname);
			strcpy(UA_dec_symbol,draftstd.dec_symbol);
			strcpy(UA_d_dec_symbol,draftstd.d_dec_symbol);
			strcpy(UA_usr_dia_sym,draftstd.usr_dia_sym);
			strcpy(UA_usr_rad_sym,draftstd.usr_rad_sym);
			UA_d_dim_roundoff = draftstd.d_dim_roundoff;
			UA_d_tol_roff_meth = draftstd.d_tol_roff_meth;
			UA_d_dim_rnd_fact = draftstd.d_dim_rnd_fact;
			UA_d_tol_rnd_fact = draftstd.d_tol_rnd_fact;
			UA_d_lin_up_tol = draftstd.d_lin_up_tol;
			UA_d_lin_lo_tol = draftstd.d_lin_lo_tol;
			UA_char_color_note = draftstd.char_color_note;
			UA_char_slant_note = draftstd.char_slant_note;
			UA_char_expansion_note = draftstd.char_expansion_note;
			UA_char_dens_note = draftstd.char_dens_note ;
			UA_entity_site_note = draftstd.entity_site_note ;
			UA_line_spacing_note = draftstd.line_spacing_note ;
			UA_txt_precision_note = draftstd.txt_precision_note ;
			UA_text_ang_note = draftstd.text_ang_note;
			UA_char_space_note = draftstd.char_space_note;
			strcpy(UA_txt_fontname_note,draftstd.txt_fontname_note);
			UA_sub_sup_ratio_note = draftstd.sub_sup_ratio_note;
			UA_char_size_note = draftstd.char_size_note;
			options = 1;
			ux_close(lu,options);
			}
		}
	else
		{
		if( ( strcmp( pathname, "" ) == 0 ) )
			{
			uu_uerror0(UA_DRAFTING,18);
			}
		else
			{
			uu_uerror1(UA_DRAFTING,1,pathname);
			}
		}
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_save_standard(pathname)
**       Saves the current attributes settings into the DRAFTSTD.DAT
**       file.  Replaces the values in that file if it is present;
**       creates the file if it does not exist.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_save_standard(pathname)
char		pathname[1];
	{
	struct UA_defaults	draftstd;
	char		form[11], type[11], dataptr[11], interp[11], msg[256];
	int		fenv, mode1, options, number, lu, status, error;
	unsigned	mode;

	uu_denter(UU_STRC,(us,"ua_save_standard(pathname=%s)",pathname));

	mode1 = 0;
	options = 1;
	status = ux_access1(pathname,&(mode1),options);
	if( ( ( status==0 )&&( mode1==0 ) ) )
		{
		error = ux_delete(pathname,1);
		}
	strcpy(type,"w+");
	strcpy(form,"STREAM");
	fenv = 0;
	mode = 0x1a4;
	strcpy(interp,"BINARY");
	strcpy(dataptr,"UX_NOEXTRA");
	options = 1;
	error = ux_create_file(pathname,mode,fenv,form,interp,dataptr,&(lu),options);
	if( ( error==0 ) )
		{
		strcpy(draftstd.file_version,"DRAFTSTD.DAT VERSION 3.3");
		draftstd.txt_place = UA_txt_place;
		draftstd.entity_site = UA_entity_site;
		draftstd.txt_entry = UA_txt_entry;
		draftstd.app_text = UA_app_text;
		draftstd.txt_orient = UA_txt_orient;
		draftstd.txt_just = UA_txt_just;
		draftstd.stack_grid = UA_stack_grid;
		draftstd.char_color = UA_char_color;
		draftstd.char_dens = UA_char_dens;
		draftstd.dec_places = UA_dec_places;
		draftstd.linear_units = UA_linear_units;
		draftstd.ang_units = UA_ang_units;
		draftstd.fraction_size = UA_fraction_size;
		draftstd.units_sym = UA_units_sym;
		draftstd.diam_symbol = UA_diam_symbol;
		draftstd.dia_place = UA_dia_place;
		draftstd.radius_sym = UA_radius_sym;
		draftstd.radius_place = UA_radius_place;
		draftstd.d_dec_places = UA_d_dec_places;
		draftstd.d_lin_units = UA_d_lin_units;
		draftstd.d_ang_units = UA_d_ang_units;
		draftstd.d_frac_size = UA_d_frac_size;
		draftstd.d_units_sym = UA_d_units_sym;
		draftstd.d_dim_z_sup = UA_d_dim_z_sup;
		draftstd.dual_format = UA_dual_format;
		draftstd.dim_type = UA_dim_type;
		draftstd.dim_zero_sup = UA_dim_zero_sup;
		draftstd.dim_roundoff = UA_dim_roundoff;
		draftstd.tol_dec_places = UA_tol_dec_places;
		draftstd.d_tol_dec_plac = UA_d_tol_dec_plac;
		draftstd.tol_method = UA_tol_method;
		draftstd.tol_site = UA_tol_site;
		draftstd.tol_zero_sup = UA_tol_zero_sup;
		draftstd.d_tol_z_sup = UA_d_tol_z_sup;
		draftstd.tol_roff_meth = UA_tol_roff_meth;
		draftstd.dim_line_font = UA_dim_line_font;
		draftstd.dim_line_color = UA_dim_line_color;
		draftstd.dim_line_dens = UA_dim_line_dens;
		draftstd.ldr_orient = UA_ldr_orient;
		draftstd.ldr_location = UA_ldr_location;
		draftstd.arrow_symbol = UA_arrow_symbol;
		draftstd.arrow_color = UA_arrow_color;
		draftstd.arrow_dens = UA_arrow_dens;
		draftstd.arrow_place = UA_arrow_place;
		draftstd.drafting_std = UA_drafting_std;
		draftstd.lin_arr_char = UA_lin_arr_char;
		draftstd.dims_disp = UA_dims_disp;
		draftstd.text_box_ovrd = UA_text_box_ovrd;
		draftstd.dim_mode = UA_dim_mode;
		draftstd.color_rel = UA_color_rel;
		draftstd.ext_line_font = UA_ext_line_font;
		draftstd.ext_line_color = UA_ext_line_color;
		draftstd.ext_line_dens = UA_ext_line_dens;
		draftstd.ext_line_sup = UA_ext_line_sup;
		draftstd.text_ang = UA_text_ang;
		draftstd.grid_dist = UA_grid_dist;
		draftstd.char_size = UA_char_size;
		draftstd.char_expansion = UA_char_expansion;
		draftstd.char_space = UA_char_space;
		draftstd.char_slant = UA_char_slant;
		draftstd.sub_sup_ratio = UA_sub_sup_ratio;
		draftstd.line_spacing = UA_line_spacing;
		draftstd.dim_rnd_fact = UA_dim_rnd_fact;
		draftstd.lin_up_tol_val = UA_lin_up_tol_val;
		draftstd.lin_lo_tol_val = UA_lin_lo_tol_val;
		draftstd.ang_up_tol_val = UA_ang_up_tol_val;
		draftstd.ang_lo_tol_val = UA_ang_lo_tol_val;
		draftstd.tol_rnd_fact = UA_tol_rnd_fact;
		draftstd.gap_dim_text = UA_gap_dim_text;
		draftstd.ldr_stub_len = UA_ldr_stub_len;
		draftstd.arrow_size = UA_arrow_size;
		draftstd.gap_geom_line = UA_gap_geom_line;
		draftstd.ext_past_line = UA_ext_past_line;
		draftstd.oblique_angle = UA_oblique_angle;
		strcpy(draftstd.txt_fontname,UA_txt_fontname);
		strcpy(draftstd.dec_symbol,UA_dec_symbol);
		strcpy(draftstd.d_dec_symbol,UA_d_dec_symbol);
		strcpy(draftstd.usr_dia_sym,UA_usr_dia_sym);
		strcpy(draftstd.usr_rad_sym,UA_usr_rad_sym);
		draftstd.d_dim_roundoff = UA_d_dim_roundoff;
		draftstd.d_tol_roff_meth = UA_d_tol_roff_meth;
		draftstd.d_dim_rnd_fact = UA_d_dim_rnd_fact;
		draftstd.d_tol_rnd_fact = UA_d_tol_rnd_fact;
		draftstd.d_lin_up_tol = UA_d_lin_up_tol;
		draftstd.d_lin_lo_tol = UA_d_lin_lo_tol;

/* note text attributes */
		ua_save_text();
		draftstd.char_color_note = UA_char_color_note;
		draftstd.char_slant_note = UA_char_slant_note;
		draftstd.char_expansion_note = UA_char_expansion_note;
		draftstd.char_dens_note = UA_char_dens_note ;
		draftstd.entity_site_note = UA_entity_site_note;
		draftstd.line_spacing_note = UA_line_spacing_note;
		draftstd.txt_precision_note = UA_txt_precision_note;
		draftstd.text_ang_note = UA_text_ang_note;
		draftstd.char_space_note = UA_char_space_note;
		strcpy(draftstd.txt_fontname_note, UA_txt_fontname_note);
		draftstd.sub_sup_ratio_note = UA_sub_sup_ratio_note;
		draftstd.char_size_note = UA_char_size_note;
		number = 1;
		options = 1;
		error = ux_write(lu,&(draftstd),sizeof(draftstd), &(number),options);
/*		uu_uerror1(UA_DRAFTING,3,pathname); */
		if (error==0)
		{
			sprintf(msg, "Drafting standard is saved in %s", pathname);
			ud_printmsg(msg);
		}
		else
		{
			sprintf(msg, "Drafting standard has a problem writing in %s", pathname);
			ud_winerror(msg);
		}
		ux_close(lu,options);
		}
	else
		{
		if( ( strcmp( pathname, "" ) == 0 ) )
			{
			uu_uerror0(UA_DRAFTING,18);
			}
		else
			{
			uu_uerror1(UA_DRAFTING,1,pathname);
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_load_new_standard()
**			Load the current drafting standard settings from a user
**			defined file
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_load_new_standard()
	{
	char		pathname[41];
	int		char_cnt;
	int		max_siz;
	int		prompt;
	int		status;

	uu_denter(UU_STRC,(us,"ua_load_new_standard()"));
	max_siz = 40;
	prompt = 112;
	status = ud_string(13,prompt,pathname,max_siz,&(char_cnt), UU_FALSE);
	if( ( char_cnt>0 ) )
		{
		ua_init_standard(pathname,UU_TRUE);
		ua_init_text();
		um_update_drwunits();
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_save_new_standard()
**			Save the current drafting standard settings to a user
**			defined file
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_save_new_standard()
	{
	char		form[7];
	char		type[7];
	char		interp[7];
	char		pathname[41];
	int		char_cnt;
	int		options;
	int		lu;
	int		max_siz;
	int		prompt;
	int		status;
	int		error;
	UU_LOGICAL	ans;
 UX_pathname bname;

	uu_denter(UU_STRC,(us,"ua_save_new_standard()"));

	max_siz = 40;
	prompt = 113;
	strcpy(pathname,UA_SAVED_FILE_NAME);
	status = ud_string(UA_DRAFTING,prompt,pathname,max_siz,&(char_cnt), 
								UU_TRUE);
	while (ux_get_base_fname(pathname, bname, (UX_NPRTERRS | UX_NCHK)) !=
		       UU_SUCCESS)
		{
		uu_uerror0(UX_UDOS,24);
		status = ud_string(UA_DRAFTING,prompt,pathname,max_siz,&(char_cnt), 											UU_TRUE);
		}
	if( ( char_cnt>0 ) )
		{
		strcpy(type,"r+");
		strcpy(form,"STREAM");
		strcpy(interp,"BINARY");
		options = 1;
		strcpy(UA_SAVED_FILE_NAME,pathname);
		error = ux_open_to_data(pathname,type,form,interp,&(lu), options);
		if( ( error==0 ) )
			{
			ans = ud_yesno(0, 
			"File exists. Do you want to overwrite?", "File exists?");
			if( ans )
				{
				options = 1;
				ux_close(lu,options);
				UA_drafting_std = 5;
				ua_save_standard(pathname);
				}
			}
		else
			{
			UA_drafting_std = 5;
			ua_save_standard(pathname);
			}
		}
	uu_dexit;
	}
