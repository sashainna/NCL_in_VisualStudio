/*********************************************************************
**    NAME         :  lcom.h
**     MODULE NAME AND RELEASE LEVEL 
**       lcom.h , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 09:47:39
*********************************************************************/
#ifndef LCOM

#include "dasnog.h"
#include "nclmplay.h"
#include "xenv1.h"
#ifdef LPGM
#define EXT
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif
/*
.....Color Modal structure
*/
typedef struct
{
	int  point;
	int  line;
	int  circle;
	int  plane;
	int  vector;
	int  pntvec;
	int  nclcv;
	int  spline;
	int  sspline;
	int  comp;
	int  nclsf;
	int  nurbs;
	int  trimsf;
	int  rvsf;
	int  netsf;
	int  matrix;
	int  patern;
	int  shape;
	int  solid;
	int  maxis;
	int  waxis;
} UL_color_mod_struc;

EXT char UL_part[UX_MAX_PATH_LEN], UL_program[UX_MAX_FILE_LEN];
EXT char UL_program_suffix[UX_SUFFIX_LEN];
EXT int UL_list_part,UL_nposts;
EXT int UL_pworks_machs[10],UL_pworks_nmach;
EXT char UL_posts[5][42],UL_pworks_mdf[42];
EXT int UL_cam, UL_cad, UL_ctrlc, UL_lathe, UL_3axis, UL_ipv;
EXT int UL_display_wrap, UL_display_more;
EXT int UL_ncq_cl, UL_ncq_as, UL_ncq_pr, UL_ncq_pp, UL_ncq_nl, UL_ncq_pri;
EXT int UL_ncq_post;
EXT int UL_ccp_type, UL_hpp_type, UL_tkp_type, UL_plot_save, UL_plot_txx;
EXT int UL_plot_abs, UL_hpp_bypass, UL_plot_fpp, UL_plot_dleft;
EXT int UL_plot_dright, UL_plot_units;
EXT int UL_pch_greco, UL_pch_list, UL_pch_reclen, UL_pch_off, UL_pch_on;
EXT int UL_pch_trans, UL_pch_ldr, UL_pch_partno, UL_pch_speed;
EXT char UL_pch_tfile[4], UL_pch_strt[21], UL_pch_com[2];
EXT int UL_dnc_mode, UL_dnc_xon, UL_dnc_list, UL_dnc_reclen, UL_dnc_par;
EXT int UL_dnc_chw, UL_dnc_crw;
EXT char UL_dnc_port[21], UL_dnc_send_pr[21], UL_dnc_rcv_pr[21];
EXT char UL_dnc_echo_pr[21], UL_dnc_strt[21], UL_dnc_eof[21], UL_dnc_eot[21];
EXT char UL_dnc_tfile[4];
EXT int UL_load_pp, UL_create_cl, UL_create_as, UL_save_pp, UL_run_ncl;
EXT int UL_del_ipv, UL_del_wip, UL_ipv_npts;
EXT char UL_stk_file[UX_MAX_PATH_LEN];
EXT int UL_major_case, UL_vocab_case,UL_label_case,UL_accuracy,UL_format_line;
EXT int UL_alignment, UL_indent_all, UL_indent_sep, UL_line_len;
EXT int UL_comment_column;

/* flags to determine if a new part has been restarted */
EXT int UL_cam_restart, UL_cad_restart, UL_nis_restart;

EXT int UL_winrow,UL_wincol;
/*
.....Unibase save options
*/
EXT int UR_save_display,UR_save_tessel;
/*
.....added for interface
.....Yurong 4/3/00
*/
EXT int UW_auto_cursor, UW_text_cursor, UW_text_select, UW_frmtext_select, 
		UW_keypad,UW_browse_dir, UW_stat_mode;
EXT int UW_icon_size, UW_menu_fmt;
EXT int UW_label_size[2], UW_label_clr, UW_com_size,
		UW_error_size, UW_prmpt_size, UW_statbar_size;
EXT char UW_form_font[20], UW_com_font[20], UW_error_font[20], 
		UW_prmpt_font[20], UW_status_font[20], UW_statbar_font[20];
EXT int UW_form_fontsize, UW_form_helpsize, UW_status_fontsize, UW_picture_pos;
EXT int UW_live_mouse;
EXT int UW_Store_color, UW_Store_forms;
/*
.....added for label enhancements
*/
EXT int UW_label_on;
EXT int UW_bkg_on;
EXT int UW_bkg_clr;
EXT int UW_ldr_on;
EXT int UW_ldr_clr;
EXT int UW_ldr_arrow;
EXT int UW_out_cmd;
EXT int UW_overlap_dis;
/*
.....added for Pocket Window
.....Yurong 12/10/01
*/
EXT int UW_pocket_pos[2], UW_pocket_size[2];
EXT char UW_pocket_title[256];
EXT int UW_pic_size[2];

EXT int UW_disp_buf,UW_erase_method,UW_cutter_method;
EXT UU_REAL UL_measurement_disttol, UL_measurement_radtol, UL_measurement_range;
EXT int UL_measurement_minsfgrid, UL_measurement_sfgridonly;
EXT int UL_measurement_color1, UL_measurement_color2;

EXT UL_color_mod_struc UL_color_mod;
#define LCOM

#define ul_icon_exit(iarg) { \
	   ud_muexit(); }

#define ul_menu_reset() 								\
			{ 											\
			UD_dastkfl = UU_FALSE; 						\
			ud_jump(UD_markptr, UU_FALSE); 				\
			}

#define ul_restart_on() { UL_cam_restart = UL_cad_restart = UL_nis_restart = 1;}

#undef EXT
#endif
/* #define NCLIPGM */

