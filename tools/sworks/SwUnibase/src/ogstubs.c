/*********************************************************************
**    NAME         :  ogstubs.c
**       CONTAINS:
**          Stub routines for the OpenNCL Geometry Library.
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ogstubs.c , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 13:05:06
*********************************************************************/
#define UA_COM 1
#include <setjmp.h>
#include "mdcoord.h"
#include "mdrel.h"
#include "nccs.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclver.h"
#include "mdattr.h"
#include "adrfcom.h"
#include "nclxunib.h"
#define WSGL_FIRST
#include "wsgl.h"
#undef UA_COM
#undef WSGL_FIRST
#define TIGMAIN
#include "tigglobal.h"
#undef TIGMAIN

#define MAX_VIEWS 100

/*
.....Added for NCL V9.6 release
*/
UU_KEY_ID fml_bskey;

extern int NCLX_internal_geom;
NCLX_sw_options gl_sw_opt;

int UG_def_line_wt = 0;
int no_of_views = 0;
int view_keys[MAX_VIEWS+2][2];
int LW_nclipv = 0;
int MSLite = 0;
int NCL_multi_ent;
int NCL_subprocess=0;
int NAUTIGES;
int NAUTCAM;
int NCL_fmill_past = 0;
int NCL_accuracy = 4;
int NCLX_internal_geom = 0;

int UA_txtattr;
int UD_chctable;
int UD_enablejmp;
int UD_host;
int UD_markptr;
int UD_markstk;
int UL_wincol;
int UL_winrow;
int UM_material_reset = 0;
int UR_save_display = 0;
int UR_save_tessel = 0;
int UU_stklen;
int UY_clstart;
int UY_nclxdebug;

jmp_buf UU_jmpb;
double toler=.001;
double _HUGE=.3402823466e+39;

int um_is_triangle_CCW() {return(UU_TRUE);}

void ua_init_txtrec() {};
void ud_yesno() {}
void norm_to_cvonsf1() {}
/*void um_nearest_to_ploc() {}*/
void uig_create_geom() {} /*temporary stub*/
void wintst(){}
void uerror() {}
void gssegvis() {}
void gssegdet() {}
void getist() {}
void getityp() {}
void mcswcs() {}
void setist() {}
void setityp() {}
void strwf2() {}
void ud_get_filename() {}
void uv_delsegs() {}
void iges_disply_as_percent() {}
void ncl_init_attr_rec() {}
void ncl_get_surf_attr() {}
void ncl_init_surf_attr() {}
void ncl_del_maclist() {}
void ncl_maclist_init() {}
void ncl_maclist_done() {}
void ncl_del_first_called_macro(){}
void ncl_edges_to_tess(){}
void ncl_get_next_called_macro() {}
void ncl_label_type(){}
void ncl_nclpl_to_umplane(){}
void ncl_print_ln6() {}
void ncl_srcctl_put() {}
void vx_mdl_inq_index() {}
void ncl_vx_create() {}
void ncl_vx_set_attr() {}
void ncl_vx_set_blank() {}
void uv_disp_entity() {}
void ncl_get_macptr() {}
void ncl_set_macptr() {}
void uig_trans_comp() {}
void uv_getvnm() {}
void uv_update_secondview() {} 
void ul_chkkey_common() {}
void ul_unibase_mtrl() {}
void uc_create_data() {}
void uc_display() {}
void uc_init_evcrvout() {}
void uc_transform() {}
void ud_ddas() {}
void ud_das() {}
void ud_form() {}
void ud_jmpmark() {}
void ud_markover() {}
void ud_printmsg() {}
void ud_prmerr() {}
void ud_strcomp() {}
void ud_wrerr() {}
void ul_close_window() {}
void ul_ipv_update_colors() {}
void ul_load_clrfile() {}
void ul_open_window() {}
void ul_save_clrmod() {}
void ul_unibase_clr() {}
void ul_win_out() {}

void um_ev4_conic() {}
void um_get_tess_toler() {}
/* void um_ilncir() {}*/
void um_rbcrv_frmnclcrv() {}
void um_ret_romgeom() {}
void um_setunits() {}
/*int um_transform_evcrvout() {return(0);}*/

void uu_uerror0() {}
void uu_uerror1() {}
void uu_uerror2() {}
void uu_ugerror1() {}
char *uu_uprompt0() {}
void uu_toolmalloc_init() {}
void uu_toolmalloc_term() {}
void uv_blanksegs() {}
void uwx_get_flist() {}
void uz_user_button() {}
void uz_user_key() {}
void uz_user_fkey() {}

void imouse3() {}
void ipuck4() {}
void ncl_ev_curve() {}
void ncl_evsrf_tf() {}
void ncl_ev7_nclcrv() {}
int ncl_fmill_past() {return(0);}
void ncl_geom_type() {}
void ncl_get1_tpar1() {}
void ncl_init_pick() {}
int ncl_itsa_fml_base() {return(0);}
void ncl_label_wf() {}
void ncl_maxim_crv1() {}
void ncl_put1_tpar1() {}
int ncl_setver() {return(0);}
void ncl_store_wf1() {}
int ncl_transform_evsfout() {return(0);}
void wf_geom_type() {}

void uig_error() {}
void uig_ev13_uvcvonsf() {}

void NclxDbgMemalloc() {}
void NclxDbgMemerr() {}
void NclxDbgMemtotal() {}
int NclxMdlFindGeo() {return(0);}

void clopen() {}
void getifl(idx,ival)
short *idx;
short *ival;
{
	*ival = 0;
}
void gettol(tol)
double *tol;
{
	*tol = toler;
}
void setifl() {}
void setsc() {}
void setstp() {}
void nclxini() {}
void um_proj_pt_on_plane() {}

void ncl_format_label(label,isub,str)
char *label,*str;
int isub;
{
	if (isub == 0)
		sprintf(str, "%s", label);
	else
		sprintf(str, "%s(%d)", label, isub);
}
/*
.....NCL V10.1
*/
int UB_snap_node_color;
int UB_snap_node_marker_type;
int UB_txtattr;
int UW_Store_color;
void um_c5_trimpart() {}
void ncl_ubput_ent1() {}
void ua_create_entity() {}
void ubi_retrieve_inst_data_in_msym() {}
void ncl_ubkey_search() {}
void ncl_ubkey_delete() {}
void ub_get_symmaster_by_name() {}
void ub_create_instance_tuple() {}
void ub_create_symbol_tuple() {}
void um_cctou_compcrv() {}
void ncl_ubcheck_symnam() {}
void ub_display_sym() {}
void ua_retrieve_data() {}
void ncl_ubput_keys() {}
void ncl_ubchg_symnam() {}
void ncl_sftopln() {}
void ua_display_drafting() {}
