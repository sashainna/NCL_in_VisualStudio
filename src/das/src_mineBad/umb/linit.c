/*********************************************************************
**	FILENAME: linit.c
**	CONTAINS:		ul_init
**                ulf_set_line_length
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       linit.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 11:57:51
*********************************************************************/

#include "driver.h"
#define LPGM
#include "lcom.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"
#undef LPGM
#include "usysdef.h"
#include "xenv1.h"
#include	"gtbl.h"
#include "gdidd.h"
#include "nclfc.h"
#include "nclstack.h"
#include "dselect.h"
#include "nclmodals.h"
#include "mdattr.h"

extern UU_REAL NCL_pick_aper;
extern UU_LOGICAL NCL_pick_verify;
extern int uw_glhicolor,uw_glvrfcolor;
extern int UZ_key_pickloc;
/*********************************************************************
**	 E_FUNCTION : ul_init_basic()
**			This function initializes the umbrella common area for general data
**				can used for both NCLIPV app and NCL
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_init_basic()
{
	UM_int2 idx,ival;
	char *str,*ux_getenv(),*index();
	UX_pathname fullname;
	UU_REAL rval[2];
	int inum;
/*
.....Part Program Suffix
*/

	str = ux_getenv ("UL_PROGRAM_SUFFIX",UX_NPRTERRS);
	if (str != 0)
	{
		strcpy (UL_program_suffix,str);
		str = index (UL_program_suffix,',');
		if (str != 0) *str = '\0';
	}
	else UL_program_suffix[0] = '\0';
/*
.....Home Directory
*/
	str = ux_getenv ("HOMEDIR",UX_NPRTERRS);
	if (str != 0)
	{
		if (ul_get_full_dir(str,fullname) == UU_SUCCESS)
			ux_modenv ("replace","HOMEDIR",fullname,UX_NPRTERRS);
	}
	UL_list_part = 0;
/*
.....CAD/CAM Terminal
*/
	UL_cam = 1;
	UL_cad = 0;
	UL_ipv = 0;
/*
.....Display File Modals
*/
	UL_display_wrap = 0;	/* Truncate record, do not wrap */
	UL_display_more = 0;	/* Display entire file without pausing */
/*
.....NCL Que Modals
*/
	UL_ncq_cl = 1;		/* Create CL file */
	UL_ncq_as = 0;		/* Do not create AS file */
	UL_ncq_pr = 1;		/* Create full PR file */
	UL_ncq_pp = 0;		/* Do not save PP file */
	UL_ncq_nl = 60;		/* 60 lines per page */
	UL_ncq_pri = 7;		/* Priority H */
	UL_ncq_post = 0;	/* Do not run post-processor */
/*
.....Plot Modals
*/
	UL_hpp_type = 1;	/* PL:7470 */
	UL_ccp_type = 3;	/* PL:1043 */
	UL_tkp_type = 3;	/* PL:4109 */
	UL_plot_save = 0;	/* Save work file */
	UL_plot_txx = 1;	/* Use saved work file */
	UL_plot_abs = 1;	/* Absolute mode */
	UL_hpp_bypass = 0;	/* Bypass mode off */
	UL_plot_fpp = 0;	/* Leading zero suppression */
	UL_plot_dleft = 3;	/* 3.4 floating point format */
	UL_plot_dright = 4;
	UL_plot_units = 0;	/* Units = inches */
/*
.....Punch Modals
*/
	UL_pch_greco = 0;	/* Punch is not a GRECO box */
	UL_pch_list = 0;	/* Do not display tape file */
	UL_pch_reclen = 72;	/* 72 columns in tape file */
	UL_pch_off = 0;		/* Punch off code */
	UL_pch_on = 0;		/* Punch on code */
	strcpy (UL_pch_tfile,"ASC");	/* NC14:NCTAPE.ASC is translation file */
	UL_pch_trans = 0;	/* No user specified translation codes */
	UL_pch_ldr = 42;	/* 42 inches of trailing leader */
	UL_pch_partno = 1;	/* Punch manreadable PARTNO */
	UL_pch_speed = 75;	/* Punch speed is 75 cps */
	UL_pch_strt[0] = '\0';	/* Start xmit at beginning of file */
	UL_pch_com[0] = '\0';	/* No comment character */
/*
.....DNC Modals
*/
	UL_dnc_mode = 0;	/* DNC mode.  Def: /SE */
	UL_dnc_port[0] = '\0';	/* DNC port.  Def: DNC1 */
	UL_dnc_send_pr[0] = '\0';	/* Send prompt string */
	UL_dnc_rcv_pr[0] = '\0';	/* Receive prompt string */
	UL_dnc_echo_pr[0] = '\0';	/* Machine echo prompt string */
	UL_dnc_strt[0] = '\0';	/* Start xmit at beginning of file */
	UL_dnc_xon = 1;		/* Use XON/XOFF protocol */
	UL_dnc_list = 0;	/* Do not display tape file */
	UL_dnc_reclen = 72;	/* 72 columns in tape file */
	UL_dnc_par = 1;		/* Send prompt strings with even parity */
	UL_dnc_chw = 0;		/* No wait after sending character */
	UL_dnc_crw = 0;		/* No wait after sending record */
	UL_dnc_eof[0] = '\0';	/* No End-of-File string */
	strcpy (UL_dnc_eot,"M02");	/* End-of-Tape string is M02 */
	strcpy (UL_dnc_tfile,"ASC");	/* NC14:NCTAPE.ASC is translation file */
/*
.....NCLCAM Parameters
*/
	UL_load_pp = 1;		/* Load part program */
	UL_create_cl = 0;	/* Create CL file */
	UL_create_as = 0;	/* Do not create APT source file */
	UL_save_pp = 0;		/* Save part program */
	UL_run_ncl = 0;		/* Do not start processing NCL */
/*
.....IPV Parameters
.....Bobby  -  2/6/92
*/
	UL_ipv_npts = 20;
	UL_del_ipv = 1;
	UL_del_wip = 1;
	UL_stk_file[0] = '\0';
/*
.....INTERFACE Parameters
*/
	UL_nis_restart = 1;
	UL_cam_restart = 1;
	UL_cad_restart = 1;
/*
.....MACHIN card stack
*/
	UL_nposts = 0;
	UL_pworks_nmach = 0;
	strcpy(UL_pworks_mdf,"0");
/*
.....Scrolling window size
*/
	UL_winrow = 20;
	UL_wincol = 100;
/*
.....Unibase Modals
*/
	UR_save_display = 1;
	UR_save_tessel = 1;
/*
.....Source Command Modals, added by JLS 2/14/00
*/
	UL_major_case = 0;
	UL_label_case = 0;
	UL_vocab_case = 0;
	UL_accuracy = 0;
	UL_alignment = 0;
	UL_indent_all = 0;
	UL_indent_sep = 0;
/*
.....Command line modals
*/
	UL_line_len = 72;
	UL_comment_column = 73;
	idx = 106; ival = UL_line_len; setifl (&idx, &ival);
	idx = 387; ival = UL_comment_column; setifl (&idx, &ival);
/*
.....Cursor modals
*/
	UW_auto_cursor = 1;
	UW_text_cursor = 1;
	UW_text_select = 0;
	UW_frmtext_select = 0;
	UW_label_size[0] = 12;
	UW_label_size[1] = 5;
	UW_label_clr = 1;
	UW_keypad = 1;
	UW_browse_dir = 0;
	UW_stat_mode = 0;
	NCL_macro_outflag = 1;
	NCL_macro_remval = 0;
	NCL_macro_outdefault = 0;
	NCL_macro_modal = 0;
	strcpy (UW_com_font, "COURIER");
	UW_com_size = 8;
	strcpy (UW_prmpt_font, "MS Sans Serif");
	UW_prmpt_size = 8;
	strcpy (UW_error_font, "MS Sans Serif");
	UW_error_size = 8;
	strcpy (UW_statbar_font, "Segoe UI");
	UW_statbar_size = 10;
	UW_menu_fmt = 2;
	UW_icon_size = 1;
	strcpy (UW_form_font, "COURIER");
	UW_form_fontsize = 8;
	UW_form_helpsize = 8;
	strcpy (UW_status_font, "COURIER");
	UW_status_fontsize = 8;

	UW_picture_pos = 0;
/*
.....added for label enhancements
*/
	UW_label_on = 2;
	UW_bkg_on = 1;
	UW_bkg_clr= 0;
	UW_ldr_on = 1;
	UW_ldr_clr = 1;
	UW_ldr_arrow = 1;
	UW_out_cmd = 0;
	UW_overlap_dis =6;

	strcpy(UW_pocket_title, "Pocket Window");
	UW_pocket_pos[0] = 100;
	UW_pocket_pos[1] = 100;
	UW_pocket_size[0] = 200;
	UW_pocket_size[1] = 200;

	UW_pic_size[0] = 400;
	UW_pic_size[1] = 400;
	str = ux_getenv ("UU_FORM_PICTURE",UX_NPRTERRS);
	if (str != 0)
	{
		if (ul_to_reals(rval, &inum, 2, str) == UU_SUCCESS)
		{
			if (inum==1)
				UW_pic_size[0] = rval[0];
			else if (inum == 2) 
			{
				UW_pic_size[0] = rval[0];
				UW_pic_size[1] = rval[1];
			}
		}
	}
/*
.....Measurement form modals.
*/
	UL_measurement_disttol    = 0.0;
	UL_measurement_radtol     = 0.0;
	UL_measurement_range      = 0.0;
	UL_measurement_minsfgrid  = 0;
	UL_measurement_sfgridonly = 0;
	UL_measurement_color1     = 11;
	UL_measurement_color2     = 12;
/*
.....chain modals form.
*/
	UD_chain_mod.conditional = 0;
	UD_chain_mod.toler = 0.01;
	UD_chain_mod.planar = 0;
	strcpy(UD_chain_mod.plane,"0,0,1,1.0");   
	UD_chain_mod.lines = 1;
	UD_chain_mod.circles = 1;
	UD_chain_mod.composites = 1;
	UD_chain_mod.splines = 1;
	UD_chain_mod.surftoler = 0.005;
	UD_chain_mod.vectors = 4;
	UD_chain_mod.direction = 0;
	UD_chain_mod.mult_surf = 0;
/*
.....Initialize geometry color modals
*/
	UL_color_mod.point = UM_DARKRED;
	UL_color_mod.line = UM_DARKGREEN;
	UL_color_mod.circle = UM_BLACK;
	UL_color_mod.plane = UM_DARKGREEN;
	UL_color_mod.vector = UM_DARKRED;
	UL_color_mod.pntvec = UM_DARKRED;
	UL_color_mod.sspline = UM_BLUE;
	UL_color_mod.comp = UM_YELLOW;
	UL_color_mod.spline = UM_CYAN;
	UL_color_mod.nclcv = UM_CYAN;
	UL_color_mod.nclsf = UM_DARKBLUE;
	UL_color_mod.nurbs = UM_DARKBLUE;
	UL_color_mod.trimsf = UM_YELLOW;
	UL_color_mod.rvsf = UM_DARKBLUE;
	UL_color_mod.netsf = UM_DARKBLUE;
	UL_color_mod.matrix = UM_DARKBLUE;
	UL_color_mod.patern = UM_YELLOW;
	UL_color_mod.shape = UM_DARKBLUE;
	UL_color_mod.solid = UM_LIGHTGREEN;
	UL_color_mod.maxis = UM_YELLOW;
	UL_color_mod.waxis = UM_CYAN;

	UW_disp_buf = 0;
	UW_erase_method = 0;
	UW_cutter_method = 0;
	uw_glhicolor = 13;
	uw_glvrfcolor = 13;
	NCL_pick_verify = 0;
/*
.....Motion playback modals
*/
	UN_mot_stack_size = 5;

	LW_drawable = 0;
	LW_viewport = 0;

	UZ_key_pickloc = 0;
	UW_live_mouse = 1;
	UW_Store_color = 1;
	UW_Store_forms = 1;
	return(UU_SUCCESS);
}

/*********************************************************************
**	 E_FUNCTION : ul_init()
**			This function initializes the umbrella common
**			area
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_init()
{
	ul_init_basic();
/*
.....Load user defined default modals
*/
	ul_load_nis_mod();
/*
.....Initialize motion playback stack
*/
	ncl_mot_stack_init();
/*
.....Load user defined keys
*/
	uz_load_keys();
	uz_load_mousedef(NULL, 1);
	ud_load_accel();
/*
.....Define background colors
*/
	uv_set_background();
/*
....Initialize construction and modelling axis
*/
	um_cplinit();
	um_modaxisinit();

	return(UU_SUCCESS);
}

/*********************************************************************
**	 E_FUNCTION : ulf_set_line_length()
**			This function sets the command line length and comment start
**       column.  It is callable by Fortran.
**	 PARAMETERS	
**		 INPUT  :
**        klen    = New command line length.
**        kcom    = New comment start column.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
void ulf_set_line_length(klen,kcom)
UM_int2 *klen,*kcom;
{
	UL_line_len = *klen;
	UL_comment_column = *kcom;
}

