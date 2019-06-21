/*********************************************************************
**    NAME         :  nclfc.h
**       CONTAINS: all FORTRAN/C interface routines
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nclfc.h , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:34
*********************************************************************/

#ifndef NCL_FCDEF

#include "usysdef.h"

#if UU_COMP == UU_SUN
#define main MAIN_
#endif
#if (UU_COMP == UU_IRIS4D) || (UU_COMP == UU_DECUNIX)
#ifndef UU_RS6000
#define main MAIN__
#endif
#endif

#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D) || (UU_COMP == UU_DECUNIX)
/*
...IBM
*/
#ifndef UU_RS6000

/***************************************************************************
       C ROUTINES CALLED BY FORTRAN TOOLS (MCV, NMG, QCV)
***************************************************************************/
#define iniaut iniaut_
/***************************************************************************
       C ROUTINES CALLED BY FORTRAN AND C
***************************************************************************/
#define ferrmsg ferrmsg_
#define clswin clswin_
#define drwlab drwlab_
#define gdraw gdraw_
#define glina3 glina3_
/*jingrong 02/26/99 changed for moving evaluator timer to neeval.c */
#define gtimx gtimx_
#define isitwf isitwf_
#define motini motini_
#define motapn motapn_
#define opnwin opnwin_
#define ptgeo ptgeo_
#define nclwsz nclwsz_
#define nclsig nclsig_
#define ncl_conv_sfseg_reset ncl_conv_sfseg_reset_ 
#define ncl_sf_prim_analyz ncl_sf_prim_analyz_
#define ncl_get_sf_primtyp ncl_get_sf_primtyp_
#define ncl_get_sf_primdat ncl_get_sf_primdat_
#define ncl_put_sf_primdat ncl_put_sf_primdat_
#define rld_primdat rld_primdat_
#define rscsym rscsym_
#define rsmply rsmply_
#define asvoc asvoc_
#define savecmd savecmd_
#define resetcmd resetcmd_
#define getsrcc getsrcc_
#define nclu_cmd_run_from_to nclu_cmd_run_from_to_
/***************************************************************************
       C ROUTINES CALLED BY FORTRAN 
***************************************************************************/
/*
.....Added for NCL501+ mode by Paul
.....02/12/92
*/
#define pass_to_lplus pass_to_lplus_
#define uw_newline_flush uw_newline_flush_
#define nclu_clean_ln nclu_clean_ln_
/*
.....Added for NCL-VT mode by Paul
.....12/04/91
*/
#define termsw termsw_
#define dmpbuf dmpbuf_
#define dmpbuf_w2 dmpbuf_w2_
#define restscr restscr_
#define crslft crslft_
#define rstcur rstcur_
#define clreol clreol_
#define savescr savescr_
#define trmrst trmrst_
#define getc1 getc1_
#define plot_w2 plot_w2_
#define plot plot_
#define hilit hilit_
#define scroll1 scroll1_
#define savcur savcur_
#define wflush wflush_
#define clreos clreos_
#define clrscr clrscr_
#define crsrgt crsrgt_
#define runvt  runvt_
#define restscr restscr_
#define trmatt trmatt_
#define trmrst trmrst_
#define clrscr clrscr_
#define savescr savescr_
/*
.....Added for NCL501+ mode by Paul
.....01/29/92
*/
#define runplus runplus_
/*
.....Added for pl/fit,cv,scalar
*/
#define plcvsf plcvsf_
#define gtclst gtclst_
#define agcvgn agcvgn_
#define ptrmsf ptrmsf_
#define lblini lblini_
#define trmrdf trmrdf_
#define trmpln trmpln_
#define trmbse trmbse_
#define trmrev trmrev_
#define trmext trmext_
#define extprm extprm_
#define plnsrf plnsrf_
#define trmrmv trmrmv_
#define rmvput rmvput_
#define rmvfre rmvfre_

#define dlbncv dlbncv_
#define igtmsf igtmsf_

#define upstat upstat_
#define dspnwf dspnwf_
#define isnswf isnswf_
#define getver getver_
#define ckintr ckintr_
#define namgen namgen_
#define motbgn motbgn_
#define motend motend_
#define motisn motisn_
#define mottln mottln_
#define motspn motspn_
#define motcln motcln_
#define motccm motccm_
#define motdel motdel_
#define moters moters_
#define filers filers_
#define filmrk filmrk_
#define namset namset_

#define dspent dspent_
#define dsprdw dsprdw_
#define drwpt drwpt_
#define drwpn drwpn_
#define getlin getlin_
#define swinpt swinpt_
#define puterm puterm_

#define gtgeo gtgeo_
#define gtcir gtcir_
#define gtcvhd gtcvhd_
#define gtcvsg gtcvsg_
#define gtspan gtspan_
#define gtspa1 gtspa1_
#define gtspat gtspat_
#define gtppat gtppat_
#define gtmpat gtmpat_
#define gtqpat gtqpat_
#define gtpnpt gtpnpt_
#define gtpnnp gtpnnp_
#define gtshap gtshap_

#define ptspan ptspan_
#define ptmhed ptmhed_
#define ptqhed ptqhed_
#define ptmpat ptmpat_
#define ptqpat ptqpat_
#define ptntsf ptntsf_
#define ptpnpt ptpnpt_
#define ptpnhd ptpnhd_
#define ptshap ptshap_

#define dlgeom dlgeom_
#define blkgeo blkgeo_
#define saveu  saveu_
#define loadu  loadu_
#define upattr upattr_
#define delast delast_

#define gmova3 gmova3_

#define inibuf inibuf_

#define setwp setwp_

#define nclpmt nclpmt_
#define nclpmt_255 nclpmt_255_
#define edtlin edtlin_

#define gtenv gtenv_
#define setmm setmm_
#define setin setin_
#define cmdmod cmdmod_
#define winopn winopn_
#define wintst wintst_
#define ptmsgu ptmsgu_
#define ptshd ptshd_
#define convrt convrt_
#define ssfcre ssfcre_
#define ssfupd ssfupd_
#define nevdsp nevdsp_
#define sftype sftype_
#define cvtype1 cvtype1_
#define evstup evstup_
#define uevcrv uevcrv_
#define uevsrf uevsrf_
#define wfstrg wfstrg_
#define postit postit_
#define lsavpp lsavpp_

/*
.....Paul. 04/02/92
*/
#define lsavsf lsavsf_

#define autcam autcam_
#define ldefpp ldefpp_
#define uledit uledit_
#define spwnit spwnit_
#define lpstnm lpstnm_
#define blankg blankg_
#define getfnm getfnm_
#define ulf_get_full_dir ulf_get_full_dir_
#define ulf_get_base_fname ulf_get_base_fname_
#define ulf_set_line_length ulf_set_line_length_
#define flopnv flopnv_
#define projpt projpt_
#define iscmpc iscmpc_
#define gtccnm gtccnm_
#define gtcent gtcent_
#define gtcccv gtcccv_
#define gtdpt gtdpt_
#define gtdesc gtdesc_
#define isitev isitev_
#define clnent clnent_
#define ofevsf ofevsf_
#define wfcvln wfcvln_
#define ulrenm ulrenm_
#define upsfuv upsfuv_
#define upmsuv upmsuv_
#define cmpdef cmpdef_
#define gtncsg gtncsg_
#define ptclsd ptclsd_
#define gtclsd gtclsd_
#define ofwfsf ofwfsf_
#define clnwf  clnwf_
#define gettol gettol_
#define getsct getsct_
#define getsuv getsuv_
#define gettool gettool_
#define bspdef bspdef_
#define bsfdef bsfdef_
#define sdedef sdedef_
#define revdef revdef_
#define revwf  revwf_
#define revsf  revsf_
#define ofswf  ofswf_
#define ofswf1  ofswf1_
#define stmdmx stmdmx_
#define stwpmx stwpmx_
#define clopen clopen_
#define clclos clclos_
#define clgetp clgetp_
#define clread clread_
#define isn2i4 isn2i4_
#define isn4i2 isn4i2_
#define clstor clstor_
#define cldel cldel_
#define clprev clprev_
#define clcool clcool_
#define clcutc clcutc_
#define clldtl clldtl_
#define clrev clrev_
#define inirvl inirvl_
#define finrvl finrvl_
#define pshrvl pshrvl_
#define poprvl poprvl_
#define setnln setnln_
#define pltmot pltmot_
#define moters moters_
#define seqsto seqsto_
#define seqend seqend_
#define gclinf gclinf_
#define ldcsym ldcsym_
#define nlower nlower_
#define nupper nupper_
#define umf_set_ent_attr umf_set_ent_attr_
#define umf_set_def_attr umf_set_def_attr_
#define umf_get_material_number umf_get_material_number_
/*
.....added by Yurong
.....4/15/99
*/
#define umu_set_def_shade umu_set_def_shade_
#define umu_set_def_lucency umu_set_def_lucency_
/*
.....added for shape
.....Yurong 5/14/99
*/
#define ncl_getshp_f2d3d ncl_getshp_f2d3d_
#define uvu_view_chk uvu_view_chk_
#define uvu_select_screen_format1 uvu_select_screen_format1_
#define uv_change_vport_attr      uv_change_vport_attr_
#define nclu_modify_view nclu_modify_view_
#define uv_repaint uv_repaint_
#define uv_fit uv_fit_
#define uv_redisp_label0 uv_redisp_label0_
#define ncl_tool_attr1 ncl_tool_attr1_
/*
....Added by Paul for *SET statement
*/
#define nclu_toggle_labels1 nclu_toggle_labels1_
#define nclu_alter_label1 nclu_alter_label1_
#define pklini pklini_
#define pklfin pklfin_
#define pklnew pklnew_
#define pklwrt pklwrt_
#define pklsto pklsto_
#define pkldel pkldel_
#define pklild pklild_
#define pkllod pkllod_
#define pklspt pklspt_
#define pklnpt pklnpt_
#define pklept pklept_
#define ang2dp ang2dp_
#define ptpnum ptpnum_
#define ptgetn ptgetn_
#define ptgetp ptgetp_
#define gtsfdp gtsfdp_
#define upsurf upsurf_
#define upsrst upsrst_
#define nclu_modify_view nclu_modify_view_
#define ncl_tool_attr1 ncl_tool_attr1_
#define resmem resmem_
#define tbncrv tbncrv_
#define intrim intrim_
#define ncl_toggle_labels1 ncl_toggle_labels1_
#define nclu_alter_label1 nclu_alter_label1_
#define ptclos ptclos_
#define pklini pklini_
#define pklild pklild_
#define pkllod pkllod_
#define ptopen ptopen_
#define pklsto pklsto_
#define ptput1 ptput1_
#define pklnpt pklnpt_
#define pklept pklept_
#define nclpmt_255 nclpmt_255_
#define pklnew pklnew_
#define pklspt pklspt_
#define pklwrt pklwrt_
#define pkldel pkldel_
#define ptsfdp ptsfdp_
#define pttess pttess_
#define initss initss_
#define rstvus rstvus_
#define ptrld  ptrld_
#define gtrld  gtrld_ 
#define dtinit dtinit_
#define dtstor dtstor_
#define dtdele dtdele_
#define dtgetv dtgetv_
#define sclvoc sclvoc_
#define cptint cptint_
#define cptnxt cptnxt_
#define cptend cptend_
#define cvbox  cvbox_
#define cvxbox cvxbox_
#define ptinbx ptinbx_
#define cilnth cilnth_
#define lnlnth lnlnth_
#define vxjump vxjump_
#define vxload vxload_
#define vxchk  vxchk_
#define vxstor vxstor_
#define vxdel  vxdel_
#define vxdlk  vxdlk_
#define vxlini vxlini_
#define vxlfin vxlfin_
#define gtsize gtsize_
#define vxlfst vxlfst_
#define vxlnxt vxlnxt_
#define ncl_vx_plotm ncl_vx_plotm_
#define vxldl1 vxldl1_
#define vxnupd vxnupd_
#define vxtest vxtest_
#define vxstnw vxstnw_
#define mclstr mclstr_
#define mclini mclini_
#define mclcll mclcll_
#define mcldel mcldel_
#define mclfnd mclfnd_
#define mclsav mclsav_
#define mclchk mclchk_
#define mclrst mclrst_
#define mclprv mclprv_
#define mclspt mclspt_
#define mclrpt mclrpt_
#define vx_gettrm  vx_gettrm_
#define vx_putapt  vx_putapt_
#define vx_includ  vx_includ_
#define vx_error  vx_error_
#define vx_ersw3  vx_ersw3_
#define vx_putw2  vx_putw2_
#define vx_putmsg  vx_putmsg_
#define tbyini tbyini_
#define vxnam  vxnam_
#define stdfwf stdfwf_
#define ppreop ppreop_
#define runvx  runvx_
#define mclpst mclpst_
#define getend getend_
#define pttrim pttrim_
/*jingrong 02/26/99 changed for moving evaluator timer to neeval.c
#define gtimx gtimx_
*/
#define shapcv shapcv_
#define num2char num2char_
#define subchk subchk_

#define ncl_psmult_init ncl_psmult_init_
#define ncl_psmult_free ncl_psmult_free_
#define ncl_psmult_load ncl_psmult_load_
#define ncl_load_curve ncl_load_curve_
#define ncl_psmult_save ncl_psmult_save_
#define ncl_psmult_chk  ncl_psmult_chk_
#define ncl_psmult_insf  ncl_psmult_insf_
#define ncl_psmult_isinit ncl_psmult_isinit_
#define ncl_psmult_sfinit ncl_psmult_sfinit_
#define ncl_psmult_select ncl_psmult_select_
#define ncl_psmult_use ncl_psmult_use_
#define ncl_psmult_nsf ncl_psmult_nsf_
#define ncl_psmult_init_s ncl_psmult_init_s_
#define ncl_psmult_store_s ncl_psmult_store_s_
#define ncl_psmult_gougck ncl_psmult_gougck_
#define ncl_psmult_setad2 ncl_psmult_setad2_
#define ncl_displst_delete ncl_displst_delete_
#define nclu_sh_circle nclu_sh_circle_
#define nclu_sh_line nclu_sh_line_
#define ncl_update_motion_list ncl_update_motion_list_
#define ncl_set_mot_choice ncl_set_mot_choice_
#define ncl_set_geo_color ncl_set_geo_color_
#define clstor_insert clstor_insert_

#define cctmtf cctmtf_
#define ncl_dscs_swap ncl_dscs_swap_
#define ncl_dscs_restore ncl_dscs_restore_
#define nclx_rmill_swap nclx_rmill_swap_

#define ncl_2sf_tanto ncl_2sf_tanto_
#define ncl_2cv_tanto ncl_2cv_tanto_
#define ncl_reproj_on_trimsf ncl_reproj_on_trimsf_
#define ncl_reproj_on_lnci ncl_reproj_on_lnci_
#define ncl_reproj_on_lnci8 ncl_reproj_on_lnci8_
#define uystup uystup_
#define uystcs uystcs_
#define uycspl uycspl_
#define gfadbgdata gfadbgdata_
#define gfadbglbl gfadbglbl_
#define gfadbgline gfadbgline_
#define yfgoauto yfgoauto_
#define ncl_filpts ncl_filpts_
#define ncl_filcvpt ncl_filcvpt_
#define ncl_getlab ncl_getlab_
#define ub_simple_symbol ub_simple_symbol_
#define ub_simple_place ub_simple_place_
#define ub_symbol_name ub_symbol_name_
#define ubf_instance_namgen ubf_instance_namgen_
#define ubf_instance_label ubf_instance_label_
#define ncl_ublist_init ncl_ublist_init_
#define ncl_ublist_free ncl_ublist_free_
#define ncl_clfind_isn ncl_clfind_isn_

#define wrtstat wrtstat_

#define scropn scropn_
#define scrfre scrfre_
#define scrput scrput_
#define scrget scrget_
#define ncl_fmgetuv ncl_fmgetuv_
#define ncl_fmfin ncl_fmfin_
#define ncl_fmcreate ncl_fmcreate_

#define ncl_fm_ptstor ncl_fm_ptstor_
#define ncl_fm_ptget ncl_fm_ptget_
#define ncl_fm_chkpts ncl_fm_chkpts_
#define ncl_fm_chkpts1 ncl_fm_chkpts1_
#define ncl_fm_ptlst_create ncl_fm_ptlst_create_
#define ncl_fmptlst_copy ncl_fmptlst_copy_
#define ncl_fmptlst_create ncl_fmptlst_create_
#define ncl_fmptlst_free ncl_fmptlst_free_
#define ncl_inter_pln ncl_inter_pln_
#define fmilcs fmilcs_
#define fmevsf fmevsf_
#define fmprj1 fmprj1_
#define fmdelp fmdelp_
#define sfini1 sfini1_

#define ncl_sm_get_npts ncl_sm_get_npts_
#define ncl_sm_get_pt ncl_sm_get_pt_
#define ncl_smfin ncl_smfin_

#define ntsf1 ntsf1_
#define ntsf2 ntsf2_

#define evsf_ext_set evsf_ext_set_
#define evsf_ext_rst evsf_ext_rst_

#define ulf_verify_box ulf_verify_box_
#define ulf_verify_chips ulf_verify_chips_
#define ulf_verify_cone ulf_verify_cone_
#define ulf_verify_copy ulf_verify_copy_
#define ulf_verify_cyl ulf_verify_cyl_
#define ulf_verify_load ulf_verify_load_
#define ulf_verify_modify ulf_verify_modify_
#define ulf_verify_move ulf_verify_move_
#define ulf_verify_remove ulf_verify_remove_
#define ulf_verify_sphere ulf_verify_sphere_
#define ulf_verify_stl ulf_verify_stl_
#define ulf_verify_torus ulf_verify_torus_
#define ulf_ipv_set_rapid ulf_ipv_set_rapid_

#define ncl_gttext ncl_gttext_
#define ncl_pttext ncl_pttext_
#define ncl_lntext ncl_lntext_
#define ncl_fmtold ncl_fmtold_
#define ncl_fmtstr ncl_fmtstr_

#define ncl_zroptr ncl_zroptr_
#define ncl_tstptr ncl_tstptr_
#define ncl_setptr ncl_setptr_
#define ncl_eqlptr ncl_eqlptr_

#define setdef	setdef_

#define ud_fyesno	ud_fyesno_
#define uv_get_vpnum	uv_get_vpnum_
#define uv_screen_chk uv_screen_chk_
#define uv_upd_scdv uv_upd_scdv_
#define ncl_get_curmac ncl_get_curmac_
#define nclf_set_curmac nclf_set_curmac_
#define ncl_geom_distf	ncl_geom_distf_
#define rmlab_prefix	rmlab_prefix_
#define ncl_load_toolf ncl_load_toolf_
#define ncl_sel_toolf ncl_sel_toolf_
#define ncl_cutool	ncl_cutool_
#define setvxidx setvxidx_
#define getvxinx getvxinx_
#define nclf_save_recent_file nclf_save_recent_file_
#define ud_clip_region ud_clip_region_
#define ud_clip_seg ud_clip_seg_
#define ud_close_clipseg ud_close_clipseg_
#define ud_init_select_buffer ud_init_select_buffer_
#define saveclip saveclip_

/***************************************************************************
      FORTRAN ROUTINES CALLED BY C AND FORTRAN
***************************************************************************/
#define isquit	isquit_
#define gettdata gettdata_
#define gettparms gettparms_
#define gettcomds gettcomds_
#define getloadtl	getloadtl_
#define ftim   ftim_
#define ncdate ncdate_
#define rsttrm rsttrm_
#define ptdfnm ptdfnm_
#define statln statln_
#define nclini nclini_
#define ranstr ranstr_
#define getclf getclf_
#define nclfin nclfin_
#define savepp savepp_
#define loadpp loadpp_
#define clinit clinit_
#define clmode clmode_
#define cutget cutget_
#define obcutr obcutr_
#define gdscut gdscut_
#define rpset rpset_
#define barseg barseg_
#define aspwrd aspwrd_
#define asclnm asclnm_
#define asgoto asgoto_
#define ascutr ascutr_
#define ascudi ascudi_
#define asmult asmult_
#define conent conent_
#define gtfedp gtfedp_
#define ptinsf ptinsf_
#define ncvevl1 ncvevl1_
#define ncl_get_tpar ncl_get_tpar_
#define ncl_ncrv_len ncl_ncrv_len_
#define ncl_maxim_crv ncl_maxim_crv_
#define ncl_put_tpar ncl_put_tpar_
#define ncl_put1_tpar ncl_put1_tpar_
#define ncl_get1_tpar ncl_get1_tpar_
#define ncl_2sfbnry ncl_2sfbnry_
#define ncl_2sfbnry_disp ncl_2sfbnry_disp_
#define ncl_2sfbnry_free ncl_2sfbnry_free_
#define ncl_psmult_free_bndry ncl_psmult_free_bndry_ 
#define ncl_bndry_cv_free ncl_bndry_cv_free_
#define ncl_ttcssf ncl_ttcssf_
#define cbdist cbdist_
#define ncl_proj_csds_bndr ncl_proj_csds_bndr_

#define ncl_tool_ps_rel ncl_tool_ps_rel_
#define ncl_create_tool ncl_create_tool_
#define ncl_update_tool ncl_update_tool_
#define ncl_update_look ncl_update_look_
#define ncl_toolstruct_size ncl_toolstruct_size_
#define ncl_create_tcyl ncl_create_tcyl_

#define ncl_cvonsf_func ncl_cvonsf_func_
#define ncl_cvonsf_free ncl_cvonsf_free_
#define pttrim pttrim_
#define ncl_cvonsf_disp ncl_cvonsf_disp_
#define ncl_2sfbnry_disp ncl_2sfbnry_disp_
#define tbdini tbdini_
#define tbdout tbdout_
#define ptinsf1 ptinsf1_
#define gtmmuv  gtmmuv_
#define ncl_cvonsf_init ncl_cvonsf_init_
#define ncl_cvonsf_init_mult ncl_cvonsf_init_mult_
#define ncl_cvonsf_free ncl_cvonsf_free_
#define norm_to_cvonsf norm_to_cvonsf_
#define norm_to_sfatcv norm_to_sfatcv_
#define ncl_norm_to_sfatcv ncl_norm_to_sfatcv_
#define ncl_ttcssf ncl_ttcssf_
#define bndprj bndprj_
#define ncl_2sfbnry ncl_2sfbnry_
#define ncl_2sfbnry_free ncl_2sfbnry_free_
#define ncl_crvpt_atdis ncl_crvpt_atdis_

#define lrfind lrfind_
#define crvevl crvevl_
#define namgen_s namgen_s_
#define fndpwd fndpwd_
#define cmplen cmplen_
#define ncl_getmac_parms0	ncl_getmac_parms0_
#define ncl_putmac_parms0	ncl_putmac_parms0_
#define ncl_getmac_parms	ncl_getmac_parms_
#define ncl_putmac_parms	ncl_putmac_parms_
#define ncl_getmac_plabel	ncl_getmac_plabel_
#define ncl_getmc_hd	ncl_getmc_hd_
#define ncl_storehd	ncl_storehd_
#define dstptv dstptv_
#define cutget_flag cutget_flag_
#define cutset_flag cutset_flag_
#define ncl_savscalar	ncl_savscalar_
#define ncl_getscalar	ncl_getscalar_
#define parse_expr		parse_expr_
#define isvocwd			isvocwd_
/***************************************************************************
      NCL FORTRAN ROUTINES CALLED BY C 
***************************************************************************/
/*
.....Added ncvclf
.....Bobby  -  1/29/92
*/
#define nclxpw nclxpw_
#define ncvclf ncvclf_
#define stunlb stunlb_
#define crvcls crvcls_
#define ncvevl ncvevl_
#define cvlbl cvlbl_
#define sflbl sflbl_
#define pwdaut pwdaut_  /* Bob Jr. auth code, called from nclc/nauth.c, etc */
#define pwdall pwdall_  /* Bob Jr. auth code, called from nclc/nauth.c, etc */
#define pwddea pwddea_  /* Bob Jr. auth code, called from nclc/nauth.c, etc */

#define ppini ppini_
#define rstint rstint_
#define cveval cveval_
#define sfeval sfeval_
#define drwent drwent_
#define randel randel_
#define randlk randlk_
#define ranini ranini_
#define curnam curnam_
#define nclsys nclsys_
#define getas getas_
#define gpgmnm gpgmnm_
#define gtrtnm gtrtnm_
#define tlpara tlpara_
#define savlab savlab_
#define reslab reslab_
#define resldr resldr_
#define savldr savldr_
#define setldt setldt_
#define setldf setldf_
#define dstptv dstptv_
#define setlbt setlbt_
#define setlbf setlbf_
#define cvepts cvepts_
#define gtpsrc gtpsrc_
#define unitcv unitcv_
#define getifl getifl_
#define getjfl getjfl_
#define getlfl getlfl_
#define getsc  getsc_
#define getnln getnln_
#define uparrw uparrw_
#define ptpsrc ptpsrc_
#define dnarrw dnarrw_
#define upiter upiter_
#define ptppnm ptppnm_
#define getpp getpp_
#define setifl setifl_
#define setjfl setjfl_
#define setlfl setlfl_
#define labwf labwf_
#define strwf1 strwf1_
#define strwf2 strwf2_
#define lblchk lblchk_
#define pleval pleval_
#define setsc setsc_
#define setscv setscv_
#define rpchk rpchk_
#define fdset fdset_
#define fdchk fdchk_
#define setstp setstp_
#define setins setins_
#define gtview gtview_
#define gtmx gtmx_
#define pokpar pokpar_
#define pokfrm pokfrm_
#define watfrm watfrm_
#define labcam labcam_
#define chklab chklab_
#define uwarn uwarn_
#define uerror uerror_
#define uerror1 uerror1_
#define uerror2 uerror2_
#define chkvoc chkvoc_
#define evrbsp evrbsp_
#define evrbsf evrbsf_
#define srfevl srfevl_
#define srfcls srfcls_
#define wcsmcs wcsmcs_
#define mcswcs mcswcs_
#define mcsmx  mcsmx_
#define stmdax stmdax_
#define stwpax stwpax_
#define getkey getkey_
#define getkeyf getkeyf_
#define getkey2 getkey2_
#define gttbse gttbse_
#define trmext trmext_
#define lnxbdy lnxbdy_
#define voccad voccad_
#define clload clload_
#define setclf setclf_
#define setas setas_
#define gtrafl gtrafl_
#define strafl strafl_
#define gettra gettra_
#define gcutlb gcutlb_
#define clsave clsave_
#define aptsrc aptsrc_
#define clfed clfed_
#define clturr clturr_
#define findtl findtl_
#define gtnxtl gtnxtl_
#define fntool fntool_
#define intool intool_
#define clpwd clpwd_
#define clspwd clspwd_
#define clcyc clcyc_
#define clcycl clcycl_
#define clspn clspn_
#define clret clret_
#define clmach clmach_
#define cycman cycman_
#define cylman cylman_
#define cycmn1 cycmn1_
#define setflg setflg_
#define frstpt frstpt_
#define sfplio sfplio_
#define flatio flatio_
#define watrln watrln_
#define wgtfinis wgtfinis_
#define wlevseq wlevseq_
#define wpock wpock_
#define wpock1 wpock1_
#define wpock2 wpock2_
#define wgtnpt wgtnpt_
#define wgtpt wgtpt_
#define gtnpt gtnpt_
#define gtpln gtpln_
#define gtpts gtpts_
#define stpkys stpkys_
#define stpky1 stpky1_
#define gtpknxt gtpknxt_
#define gtpkys gtpkys_
#define frpkys frpkys_
#define pts2pl pts2pl_
#define lcrect lcrect_
#define addsky addsky_
#define delsky delsky_
#define gtsky gtsky_
#define addskx addskx_
#define delskx delskx_
#define gtskx gtskx_
#define addskw addskw_
#define delskw delskw_
#define gtskm gtskm_
#define gtskw gtskw_
#define addskj addskj_
#define getskj getskj_
#define cvoutj cvoutj_
#define addskg addskg_
#define addskf addskf_
#define addskfc addskfc_
#define delskfc delskfc_
#define copyskftm copyskftm_
#define andskm_skfc andskm_skfc_
#define copyskft copyskft_
#define gtskg gtskg_
#define gtskfc gtskfc_
#define delskf delskf_
#define delskg delskg_
#define chgskg chgskg_
#define addskm addskm_
#define delskm delskm_
#define initpokpols initpokpols_
#define freepokpols freepokpols_
#define rstpokpol rstpokpol_
#define addpokpol addpokpol_
#define subpokpols subpokpols_
#define getpolb getpolb_
/*
....Added by PAul for DYNAMIC MACRO
*/
#define entnam entnam_
#define macpar macpar_
#define gtmsmm gtmsmm_
#define gtmsfl gtmsfl_
#define stmsmm stmsmm_
#define lodmac lodmac_
/*
...vp 26-May-94 for secondary unibase
*/
#define isitag isitag_
#define ur_cl04 ur_cl04_
#define ur_op04 ur_op04_
#define unbuni unbuni_
#define gtref gtref_
#define gtmod gtmod_
#define inches inches_
#define millim millim_
#define ubact ubact_
#define f77_resu f77_resu_
#define f77_fdst f77_fdst_
#define ub2lod ub2lod_
#define ur_st02c ur_st02c_
#define ubput ubput_
#define ubfind ubfind_

#define getist getist_
#define getityp getityp_
#define setist setist_
#define setityp setityp_

#define putinw putinw_
#define putinf putinf_
#define predef predef_
#define prefil prefil_
#define modtoc modtoc_
#define cibox  cibox_
#define upmatr upmatr_
#define dstmat dstmat_
#define dsxlab dsxlab_
#define gtmxds gtmxds_
#define nclf_invmx nclf_invmx_
#define mxcolc mxcolc_
#define presfe presfe_
#define ppword ppword_
#define pwddel pwddel_
#define nshent nshent_
#define getind getind_
#define setind setind_
#define seticsf seticsf_

#define to_unibase to_unibase_
#define from_unibase from_unibase_
#define to_unbs to_unbs_
#define fr_unbs fr_unbs_


#define ncl_gtssnum ncl_gtssnum_
#define ncl_create_ssplin ncl_create_ssplin_
#define ncl_cvonsf_get_bskey ncl_cvonsf_get_bskey_
#define ncl_put_uv ncl_put_uv_
#define ncl_push_uv ncl_push_uv_
#define ncl_insert_uv ncl_insert_uv_
#define ncl_put_asn ncl_put_asn_
#define ncl_push_asn ncl_push_asn_
#define ncl_get_ent ncl_get_ent_
#define ncl_get_asn ncl_get_asn_
#define ncl_put_key ncl_put_key_
#define ncl_weed_uv ncl_weed_uv_
#define ncl_io_weed ncl_io_weed_
#define ncl_free_circ ncl_free_circ_
#define ncl_free_uv ncl_free_uv_
#define ncl_nul_uv ncl_nul_uv_
#define ncl_getn_uv ncl_getn_uv_
#define ncl_revers_uv ncl_revers_uv_
#define ncl_intof_def ncl_intof_def_
#define ncl_cvonsf_wrap ncl_cvonsf_wrap_
#define ncl_cvonsf_proj ncl_cvonsf_proj_
#define ncl_check_key ncl_check_key_
#define nclfini nclfini_
#define nclxostr nclxostr_
#define nclf_save_session_file nclf_save_session_file_
#define nclf_save_session_common nclf_save_session_common_
#define nclf_load_session_file nclf_load_session_file_
#define nclf_load_session_common nclf_load_session_common_
#define nclf_flush_buffer nclf_flush_buffer_
#define ncl_profile_circle ncl_profile_circle_
#define ncl_profile_curve ncl_profile_curve_
#define ncl_profile_entry ncl_profile_entry_
#define ncl_profile_start ncl_profile_start_
#define ncl_profile_offset ncl_profile_offset_
#define ncl_profile_project ncl_profile_project_
#define ncl_profile_weed ncl_profile_weed_
#define ncl_profile_ps ncl_profile_ps_
#define ncl_profile_pt ncl_profile_pt_
#define ncl_profile_free ncl_profile_free_
#define prfps prfps_
#define nclf_load_cutprof nclf_load_cutprof_
#define nclf_is_revsf nclf_is_revsf_
#define nclf_cutter_get_bounds nclf_cutter_get_bounds_
#define nclf_cutter_get_cutsym nclf_cutter_get_cutsym_
#define nclf_cutter_set nclf_cutter_set_
#define nclf_get_tool_symlib nclf_get_tool_symlib_

#define wcsact wcsact_
/*
... aak 06-feb-1998: math. library routines
*/
#define um_vctovc_2d um_vctovc_2d_
#define um_middlept_2d um_middlept_2d_ 
#define um_vcplvc_2d um_vcplvc_2d_ 
#define um_vcmnvc_2d um_vcmnvc_2d_
#define um_vctmsc_2d um_vctmsc_2d_ 
#define um_mag_2d um_mag_2d_ 
#define um_dot_2d um_dot_2d_
#define um_unitvc_2d um_unitvc_2d_
#define um_xytovc_2d um_xytovc_2d_
#define um_dist_2d um_dist_2d_
#define um_to_edge um_to_edge_ 
#define umf_drw_cpl_axis umf_drw_cpl_axis_
#define umf_drw_cpl_axis_off umf_drw_cpl_axis_off_
#define umf_drw_mod_axis umf_drw_mod_axis_
#define umf_drw_mod_axis_off umf_drw_mod_axis_off_
#define umf_get_attrib umf_get_attrib_
#define labset1 labset1_
#define umf_get_attrib umf_get_attrib_
#define umf_get_material_number umf_get_material_number_
#define ncl_cvonsf1 ncl_cvonsf1_
#define ncl_rename_geom ncl_rename_geom_
#define ncl_ssonsf1 ncl_ssonsf1_
#define puterr puterr_
#define ncl_pt_intof_cv ncl_pt_intof_cv_
#define ncl_pt_on_cv ncl_pt_on_cv_
#define ncl_pt_on_sf ncl_pt_on_sf_
#define ncl_pt_inside_trimsf ncl_pt_inside_trimsf_
#define nclf_pt_project_sf nclf_pt_project_sf_
#define nclf_pn_project_sf nclf_pn_project_sf_
#define sfpt sfpt_
#define sfpt1 sfpt1_
#define sfpt2 sfpt2_
#define cvpv2 cvpv2_
#define ptdesc ptdesc_
#define ptdsc3 ptdsc3_
#define getdef getdef_
#define getcln getcln_
#define ncevolve ncevolve_
#define ncevof ncevof_
#define ncevolve2d ncevolve2d_
#define ncl_cvsfsd ncl_cvsfsd_
#define cvsfs1 cvsfs1_
#define ncl_bsp_intof ncl_bsp_intof_
#define ncl_replace_uv ncl_replace_uv_
#define ncvofs ncvofs_
#define ncvsp ncvsp_
#define ncofcv ncofcv_
#define ncwrap ncwrap_
#define ncproj ncproj_
#define ncontr ncontr_
#define nsfsio nsfsio_
#define sfscre sfscre_
#define sfsini sfsini_
#define sfsfre sfsfre_
#define offwf1 offwf1_
#define nsfsi1 nsfsi1_
#define nsfsi2 nsfsi2_
#define upsubs upsubs_
#define stsavi stsavi_
#define parslb parslb_
#define ncprojaa ncprojaa_
#define ncl_trim_cv ncl_trim_cv_
#define getapn getapn_
#define pfillet pfillet_
#define yfilet yfilet_
#define ncl_set_temp_clfile ncl_set_temp_clfile_
#define ncl_reset_temp_clfile ncl_reset_temp_clfile_
#define ncl_reset_select ncl_reset_select_
#define extrdf extrdf_
#define llini llini_
#define lldel lldel_
#define llnul llnul_
#define llpush llpush_
#define llsort llsort_
#define llpop llpop_
#define lisini lisini_
#define lisadd lisadd_
#define lisjct lisjct_
#define lisdel lisdel_
#define lisrst lisrst_
#define lisget lisget_
#define mclsetprv mclsetprv_
#define rrini rrini_
#define rrdel rrdel_
#define rrput rrput_
#define rrscal rrscal_
#define rrget rrget_
#define rrplce rrplce_
#define rrflip rrflip_
#define shfile shfile_
#define nuldsp nuldsp_
#define gtfilrad gtfilrad_
#define moinfo moinfo_
#define moinfs moinfs_
#define moinfr moinfr_
#define fdisp_stat      fdisp_stat_
#define pmdsav pmdsav_
#define pmdrst pmdrst_
#define poksav poksav_
#define pokrst pokrst_
#define conpt conpt_
#define conlen conlen_
#define conref conref_
#define ncl_fixif ncl_fixif_
#define ncl_ifreset ncl_ifreset_
#define ncl_ifstor ncl_ifstor_
#define ncl_ifpush ncl_ifpush_
#define ncl_ifgtend ncl_ifgtend_
#define ncl_ifgtln ncl_ifgtln_
#define ncl_ifinit ncl_ifinit_
#define ncl_iflabchk ncl_iflabchk_
#define ncl_ifunmatched ncl_ifunmatched_
#define dtgtnw dtgtnw_
#define wgtci wgtci_
#define ud_store_batchmsg ud_store_batchmsg_
#define ud_send_nclinfo ud_send_nclinfo_
#define getbatchinfo getbatchinfo_
#define ul_fshort_filename ul_fshort_filename_
#define ul_write_logfile ul_write_logfile_
#define uv_viewnum_key uv_viewnum_key_
#define ul_format_data11	ul_format_data11_

#define  nclf_readhead	nclf_readhead_
#define  nclf_readtl_info	nclf_readtl_info_
#define  nclf_rdlib_disp	nclf_rdlib_disp_
#define  intool2	intool2_
#define  toolparms	toolparms_
#define gtllib gtllib_
#define stllib stllib_
#define setncsfl5	setncsfl5_

#define ncl_pntvec_revsf ncl_pntvec_revsf_
#define ncl_sson_revsf ncl_sson_revsf_
#define ncl_cvon_revsf ncl_cvon_revsf_
#define updatepp updatepp_
#define nclf_get_scalar_value nclf_get_scalar_value_
#define chkwstr   chkwstr_
#define nclf_getlabel nclf_getlabel_

#define nclf_define_solid nclf_define_solid_
#define nclf_load_solid nclf_load_solid_
#define nclf_save_solid nclf_save_solid_
#define nclf_get_solid nclf_get_solid_
#define boxcnv boxcnv_

#define nclf_revnorm nclf_revnorm_
#define nclf_swapuv nclf_swapuv_
#define nclf_cv_project_sf nclf_cv_project_sf_
#define nclf_format_label nclf_format_label_
#define nclf_get_mcpam_value nclf_get_mcpam_value_
#define nclf_save_stl nclf_save_stl_
#define ncl_profile_anote ncl_profile_anote_
#define ud_chk_comm ud_chk_comm_
#define uaf_notes uaf_notes_
#define uaf_check_font uaf_check_font_
#define uaf_get_notes uaf_get_notes_
#define uaf_get_note_type uaf_get_note_type_
#define uaf_get_txt_defstyle uaf_get_txt_defstyle_
#define uaf_obtain_notes uaf_obtain_notes_
#define uaf_set_txt_attr uaf_set_txt_attr_
#define ubf_explode_syminstance ubf_explode_syminstance_
#define ub_symbol_set_vis ub_symbol_set_vis_
#define ub_symbol_delete ub_symbol_delete_
#define ub_symbol_rename ub_symbol_rename_
#define ulf_spawn ulf_spawn_
#define uz_acttime uz_acttime_
#define ub_get_instance_master ub_get_instance_master_
#define symfst symfst_
#define symnxt symnxt_

#define nclf_delsrc nclf_delsrc_
#define nclf_getsrc nclf_getsrc_
#define nclf_putsrc nclf_putsrc_
#define nclf_srceof nclf_srceof_
#define nclf_close_src nclf_close_src_
#define nclf_load_src nclf_load_src_
#define nclf_save_src nclf_save_src_
#define nclf_getw2 nclf_getw2_
#define nclf_putw2 nclf_putw2_
#define nclf_storew2 nclf_storew2_
#define nclf_save_source nclf_save_source_
#define nclf_load_source nclf_load_source_
#define nclf_putmerge nclf_putmerge_
#define nclf_merge_source nclf_merge_source_
#define nclf_src_rec_to_line nclf_src_rec_to_line_
#define nclf_src_line_to_rec nclf_src_line_to_rec_
#define nclf_src_line_to_rec_next nclf_src_line_to_rec_next_
#define nclf_src_next_rec nclf_src_next_rec_
#define nclf_getmac_callin nclf_getmac_callin_
#define getxln getxln_
#define getxln_count getxln_count_
#define nclf_trimsrf_ckuv nclf_trimsrf_ckuv_
#define nclf_transf nclf_transf_

#endif
#endif /*  UU_COMP  */

#define NCL_FCDEF
#endif
