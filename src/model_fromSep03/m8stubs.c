/*********************************************************************
**    NAME         :  m8stubs.c
**       CONTAINS: Stubs routines for remaining calls to AG library.
**                 Many routines are not necessary, but this is just
**                 previously used ag.c file
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**        m8stubs.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:08:11
*********************************************************************/

#include "usfltdef.h"
#include "ag_constant.h"
#include "ag_globinit.h"

int 	ag_pr_offsegs () {return(UU_FAILURE);}
#if UU_COMP != UU_VAXVMS
int 	ag_face_ob_new_coBf () {return(UU_FAILURE);}
int 	ag_face_ib_new_coBf () {return(UU_FAILURE);}
int		ag_sh_srf_to_Bez () {return(UU_FAILURE);}
#endif
int 	ag_al_bs () {return(UU_FAILURE);}
int 	ag_bld_bs () {return(UU_FAILURE);}
int 	ag_bld_ccxh () {return(UU_FAILURE);}
int 	ag_bld_cfxh () {return(UU_FAILURE);}
int 	ag_bld_cpt () {return(UU_FAILURE);}
int 	ag_bld_cp_l () {return(UU_FAILURE);}
int 	ag_bld_crv () {return(UU_FAILURE);}
int 	ag_bld_csxh () {return(UU_FAILURE);}
int 	ag_bld_lcrv () {return(UU_FAILURE);}
int 	ag_bld_offsetd () {return(UU_FAILURE);}
int 	ag_bs_crv () {return(UU_FAILURE);}
int 	ag_bs_flt_crv_it () {return(UU_FAILURE);}
int 	ag_bs_line_2pt () {return(UU_FAILURE);}
int 	ag_bs_srf_u () {return(UU_FAILURE);}
int 	ag_bs_srf_v () {return(UU_FAILURE);}
int 	ag_cls_pt_crv () {return(UU_FAILURE);}
int 	ag_cntd_area_srf () {return(UU_FAILURE);}
int 	ag_cntd_srf_vol () {return(UU_FAILURE);}
int 	ag_crvp_fs_crv () {return(UU_FAILURE);}
int 	ag_crv_app_bs () {return(UU_FAILURE);}
int 	ag_crv_bs () {return(UU_FAILURE);}
int 	ag_crv_bs_merge () {return(UU_FAILURE);}
int 	ag_crv_carc_ang_3d () {return(UU_FAILURE);}
int 	ag_crv_combine () {return(UU_FAILURE);}
int 	ag_crv_copy () {return(UU_FAILURE);}
int 	ag_crv_cub_fit_pts () {return(UU_FAILURE);}
int 	ag_crv_cub_intp () {return(UU_FAILURE);}
int 	ag_crv_div () {return(UU_FAILURE);}
int 	ag_crv_ellp () {return(UU_FAILURE);}
int 	ag_crv_hyper () {return(UU_FAILURE);}
int 	ag_crv_line_2pt () {return(UU_FAILURE);}
int 	ag_crv_offset () {return(UU_FAILURE);}
int 	ag_crv_parab () {return(UU_FAILURE);}
int 	ag_crv_rev_dir () {return(UU_FAILURE);}
int 	ag_crv_re_par () {return(UU_FAILURE);}
int 	ag_crv_uface () {return(UU_FAILURE);}
int 	ag_crv_vface () {return(UU_FAILURE);}
int 	ag_dal_bs () {return(UU_FAILURE);}
int 	ag_db_bs () {return(UU_FAILURE);}
int 	ag_db_ccxh () {return(UU_FAILURE);}
int 	ag_db_cfxh () {return(UU_FAILURE);}
int 	ag_db_cp_l () {return(UU_FAILURE);}
int 	ag_db_crv () {return(UU_FAILURE);}
int 	ag_db_crvs_l () {return(UU_FAILURE);}
int 	ag_db_csxh () {return(UU_FAILURE);}
int 	ag_db_lcrv () {return(UU_FAILURE);}
int 	ag_db_offsetd () {return(UU_FAILURE);}
int 	ag_db_sh () {return(UU_FAILURE);}
int 	ag_db_srf () {return(UU_FAILURE);}
int 	ag_drw_srf_u () {return(UU_FAILURE);}
int 	ag_drw_srf_v () {return(UU_FAILURE);}
int 	ag_dr_bs_ab () {return(UU_FAILURE);}
int 	ag_dr_crv () {return(UU_FAILURE);}
int 	ag_dr_sh () {return(UU_FAILURE);}
int 	ag_eval_bs () {return(UU_FAILURE);}
int 	ag_eval_crv () {return(UU_FAILURE);}
int 	ag_eval_srf () {return(UU_FAILURE);}
int 	ag_eval_srf_norm () {return(UU_FAILURE);}
int 	ag_face_ib_new_cobf () {return(UU_FAILURE);}
int 	ag_face_ob_new_cobf () {return(UU_FAILURE);}
int 	ag_fin_sh () {return(UU_FAILURE);}
int 	ag_fout_sh () {return(UU_FAILURE);}
int 	ag_fr_crv () {return(UU_FAILURE);}
int 	ag_fr_sh () {return(UU_FAILURE);}
int 	ag_fr_srf () {return(UU_FAILURE);}
int 	ag_fw_crv () {return(UU_FAILURE);}
int 	ag_fw_sh () {return(UU_FAILURE);}
int 	ag_fw_srf () {return(UU_FAILURE);}
int 	ag_init () {return(UU_FAILURE);}
int 	ag_len_crv () {return(UU_FAILURE);}
int 	ag_pro_conic () {return(UU_FAILURE);}
int 	ag_pro_srf () {return(UU_FAILURE);}
int 	ag_pr_ccxh () {return(UU_FAILURE);}
int 	ag_pr_cfxh () {return(UU_FAILURE);}
int 	ag_pr_csxh () {return(UU_FAILURE);}
int 	ag_pt_tan_crv_it () {return(UU_FAILURE);}
int 	ag_q_srf_clsd_u () {return(UU_FAILURE);}
int 	ag_q_srf_clsd_v () {return(UU_FAILURE);}
int 	ag_q_srf_mek_u () {return(UU_FAILURE);}
int 	ag_q_srf_mek_v () {return(UU_FAILURE);}
int 	ag_set_bsbox () {return(UU_FAILURE);}
int 	ag_set_cbox () {return(UU_FAILURE);}
int 	ag_set_cnode_1 () {return(UU_FAILURE);}
int 	ag_set_cnode_2 () {return(UU_FAILURE);}
int 	ag_set_cp2 () {return(UU_FAILURE);}
int 	ag_set_sp1 () {return(UU_FAILURE);}
int 	ag_sh_flipnrm () {return(UU_FAILURE);}
int 	ag_sh_srf_to_bez () {return(UU_FAILURE);}
int 	ag_sh_sum () {return(UU_FAILURE);}
int 	ag_srf_4crv () {return(UU_FAILURE);}
int 	ag_srf_bcub_fit_bs () {return(UU_FAILURE);}
int 	ag_srf_cone () {return(UU_FAILURE);}
int 	ag_srf_cone_seg () {return(UU_FAILURE);}
int 	ag_srf_copy () {return(UU_FAILURE);}
int 	ag_srf_cub_lin_fit_bs () {return(UU_FAILURE);}
int 	ag_srf_cyl () {return(UU_FAILURE);}
int 	ag_srf_cyl_seg () {return(UU_FAILURE);}
int 	ag_srf_plane () {return(UU_FAILURE);}
int 	ag_srf_rev () {return(UU_FAILURE);}
int 	ag_srf_rev_seg () {return(UU_FAILURE);}
int 	ag_srf_sphere () {return(UU_FAILURE);}
int 	ag_srf_sum () {return(UU_FAILURE);}
int 	ag_srf_swp_rail () {return(UU_FAILURE);}
int 	ag_srf_torus () {return(UU_FAILURE);}
int 	ag_tan_crv2_it () {return(UU_FAILURE);}
int 	ag_tr_crv () {return(UU_FAILURE);}
int 	ag_tr_sh () {return(UU_FAILURE);}
int 	ag_tr_srf () {return(UU_FAILURE);}
int 	ag_x_crv_crv () {return(UU_FAILURE);}
int 	ag_x_crv_crv_it () {return(UU_FAILURE);}
int 	ag_x_crv_face () {return(UU_FAILURE);}
int 	ag_x_crv_self () {return(UU_FAILURE);}
int 	ag_x_crv_srf () {return(UU_FAILURE);}
int 	ag_x_ipl_srf () {return(UU_FAILURE);}
int 	ag_x_srf_srf () {return(UU_FAILURE);}
int 	um_ret_romgeom () {return(UU_FAILURE);}
int 	um_save_appgeo () {return(UU_FAILURE);}

/****************************
c... Added for RS6000 . Paul
c... 11/21/91
*****************************/

#ifdef UU_RS6000
int     iconstr() {}
int     iuniview() {}
int     ux_udos_uni() {}

int     foreground() {}

int     gflush() {}

int     passthrough() {}

int     feedback() {}

int     endfeedback() {}

int     itim() {}

int     gencev() {}
int     parevl() {}
int     usfevl() {}
#endif


