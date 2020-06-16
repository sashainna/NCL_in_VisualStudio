/*********************************************************************
**    NAME         :  ipvstubs.c
**
**       CONTAINS:
**		dummy defines and functions
**
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ipvstubs.c , 25.6
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 16:25:32
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#define DPGM 1
#include "dmotif.h"
#include "usysg.h"
#include "dpipe.h"
#undef DPGM
#define UM_MPGM 1
#include "mdrwsize.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mpocket.h"
#include "m2dattr.h"
#include "mattr.h"
#undef UM_MPGM

#define VPGM
#include "view1.h"
#undef VPGM

#define NCLVERSION
#include "nclver.h"		/* Defines's version number */

#include "ws.h"
#include "wsgl.h"

int NCL_cmdmod = 0;
int NCL_pik_hierarchy = 1;

#define NCL_ICONS_FOR_INTER 1
#include "nclicons.h"
#undef NCL_ICONS_FOR_INTER
#include "nclmplay.h"
#include "gtblws.h"
#include "mdattr.h"
#include "udfrmdef.h"
#include "mdeval.h"
#include "calcom.h"
#define NCL_MPGM
#include "nkeywd.h"
#include "nclmodals.h"
#undef NCL_MPGM

#define MENUPGM 1
#include	"dmenucom.h"
#undef MENUPGM

#include "lipv.h"
#include "uhep.h"

#include "mcrv.h"

static FILE *Sfp=0;
static int UY_nclxdebug=0;
UU_LOGICAL UR_restore_units;

#define UD_SUBSYSTLEN 34
int UD_subsyslen = UD_SUBSYSTLEN;
UD_DSUBSYS UD_subsys[UD_SUBSYSTLEN];
UU_LOGICAL NCL_init_limit = UU_FALSE;
UU_LOGICAL     dflag;
int UD_num_safs = 1;
UD_SAF UD_saf_table[1];
char UD_prompt[1224];

#define UQ_TBSIZE   100
UQ_qstb	UQ_symtab[UQ_TBSIZE];
int UQ_tbindex;

UU_LOGICAL NCL_merge_overwrite;

int NCL_subprocess = 0;
int UW_signon = 0;
UU_LOGICAL UM_plotting = UU_TRUE;
UU_REAL	UM_plotprec = 0;
int UU_application = UU_NCLIPV;
int NCLX_internal_geom = 0;
int NCL_mot_seg;
int NCL_clipf;

double toler = UM_FUZZ;
UV_view UV_dynview, dyn_view;
int UZ_nclipv_view;
int UM_material_reset;
UN_cutdef_struc cutdef[UV_NVPORTS];

int UN_override_geo_mask;
int UL_clswin_flag = 0;
int NCL_macro_outflag, NCL_macro_remval, NCL_macro_outdefault, NCL_macro_modal;

char *UM_pocket_hwnd = 0;

int NAUTLTH = 1;
int NAUTSTEP = 0;
int NCL_edit_mode,NCL_cmd_window_mode,NCL_com_mode,NCL_mark_method;
int UN_motion_color=1, UN_rapid_color=3;
int UN_motion_line=UM_SOLID_LINE, UN_rapid_line=UM_DASHED_LINE;
int UN_motion_pen=1, UN_rapid_pen=1;
double UN_motion_width = 1.0;

char NCL_init_fstr[20];

int UW_menu_fmt,UW_icon_size,PKx,PKy, UW_light_reset,UW_pocket_mode;
#define NALPHAKEYS 18
static char *alpha[NALPHAKEYS]={"0","1","2","3","4","5","6","7","8","9",
	".",",","+","-","/","*","<-","ENTER"};

int UU_application;
int NCLHOST,UD_host,NCL_pick_aper,UD_duimsdeflt/*,UD_enablejmp*/;
int	UD_curlayout,UD_errorsegflag/*,UD_markptr,UD_markstk*/;
int NCL_mot_seg = -1;
int NAUTCAM=0, NAUTIGES=0;
int NCLX_external_unibase=0;

int UW_modelview_depth = 32, UW_project_depth = 2;
int UD_pickmode,UD_chain_mod,UN_mot_stack_size,NCL_pick_verify;
#define MAXLIGHT 15
int UM_light_keys[MAXLIGHT];

int UJ_plotting, UD_opengl;
int UR_restore_mtrl;
int UR_restore_lights;
UU_LOGICAL UN_motmodal_cmd=UU_TRUE;
struct UM_mtrlmdl_rec UM_mtrlmdl;
UU_REAL UV_dynvec[3], LW_dyncp[3], UV_dyncp[3];

int NCL_animation = 0;
int NCL_preview_mot = 0;
UN_motseg *mlist_ptr = (UN_motseg *)-1;
UN_motseg *mlist_first_ptr = (UN_motseg *)-1;
UN_motseg *mlist_first_ptr2 = (UN_motseg *)-1;
/*
......for now
*/
int NAUTMACH = 0;
int NAUTIPV = 0;

int UD_textpre_curpos, UD_string_add;

int UR_restore_clr;

UX_libdata_bag UB_libdata_rec = { UU_TRUE, "local", "******",
		"UB_LOC_M_SYMDIR", "UB_SYS_M_SYMDIR", "symbol", "UB_SYM_EXTEN",
		"UB_SYM_AREA_SUFFIX", "UB_SYM_SUFFIX" };


/*
.....Dummy routines
*/
void savepp(){}
void ncl_open_file(){}
void saveu(){}

/*
.....not used function called from d4actfm.c
*/
void ud_trmoff() 
{
}

/*
.....no status display for now
*/
ud_pstat(){}
/*
.....no status for now
*/				
uz_status() {}
umu_inactgrid() {}
umu_view_model() {}
um_reset_pocket_graphics() {}
uw_ntresize_graphics() {}
um_modaxisinit() {}
um_cplinit() {}
cutseg_flag() {}
ncl_label_cmp() {}
znu_init_machinetype() {}



/*********************************************************************
**    E_FUNCTION     : ncl_wcstomcs(option, wcs, mcs)
**         Convert a vector or cartesian  coordinate specified in the
**         current construction  coordinate system to a vector or
**         cartestian  coordinate specified relative to the modeling
**         coordinate system.
**    PARAMETERS   
**       INPUT  : 
**            option      0 => cartesian  coordinate;
**                        1 => vector
**            wcs         input (construction  coordinate system)
**       OUTPUT :  
**            mcs         output (modeling coordinate system)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_wcstomcs(option, wcs, mcs)
   int option;
   UM_coord  wcs;
   UM_coord  mcs;

   {
   UM_real8 buf[3];
   UM_int2 i2opt;

   buf[0] = wcs[0];
   buf[1] = wcs[1];
   buf[2] = wcs[2];
   i2opt = option;
   wcsmcs (&i2opt, buf);
   mcs[0] = buf[0];
   mcs[1] = buf[1];
   mcs[2] = buf[2];
   }





/*NCL only*/
ncl_display_motion() 
{
}
ncl_mot_stack_cldel()
{
}
uv_delete_hidden(vport)
UV_vport	*vport;
{
}
uj_noop() {}

ud_lpsh() {}
ud_lpop() {}
udi_mu_reset() {}
ncl_display_ldr() {}
nclc_save_recent_file(){}
/*
.....no status window 
*/
ncl_set_statact(){}
ncl_get_sclar_frm() {return -1;}

uw_glresize_graphics(){}
um_minimize_pocket_window(){}

ud_is_playback() 
{
	return 0;
}
char *um_get_ncl_win()
{
	return NULL;
}

ncl_retrieve_data_fixed()
{
}
ncl_post_load_color(){}
ncl_update_color(){}
ud_hakt(){}
ncl_get_tess_triangles(){}
uc_srf_tessellate(){}
ncl_set_boundary_toler()
{
}
ncl_get_boundary_toler()
{
}
ncl_set_tess_parms()
{
}
ncl_get_tess_parms()
{
}
ncl_evolve_shape()
{
}
um_get_attrib()
{
}
um_get_pickkey(){}
ud_gnxt(){}
um_dl_pldas(){}
getrel(){}

uv_delsegs(){}
um_feareset(){}
um_rm31_romini(){}
uj_help(){}
/*record/playback?*/
ud_rprd_ipvloc(){}
ud_rpwr_ipvloc(){}

uv_dispobjs2(){}
uv_delvp(){}
uv_dispvp(){}
um_actgrid(){}
uj_miplotting()
{
	return 0;
}
um_is_pocket_graphics()
{
	return 0;
}
uv_clear(){}
uv_dispobjs(){}
ncl_set_mark(){}
scrini(){}
ncl_cvonsf_free() {}
pklini(){}
ud_lgeo()
{
}
ncl_cmd_mode(){}
setnl(){}
pick_select_verify(){} 
ncl_on_verify_segno(){} 
strafl (){}
ascudi(){}
ncl_plane_to_nclpln()
{
}
ul_edit(){}
ncl_redraw_geo(){}
cutget_flag(){}
cutset_flag(){}
ud_brower_rprd(){}
ud_brower_endrprd(){}
um_get_disp_attr(){}

gettra(){}
gdscut(){}

uc_init_evcrvout (eptr, evoutptr)
	struct UM_crvdatabag *eptr;
	struct UM_evcrvout *evoutptr;
{
	evoutptr->firsteval = UU_TRUE;
	return UU_SUCCESS;
}
ud_get_pickstr(){}
uz_user_dascalls() {}
uz_daskey1() {}
ncl_cutseg_front() {}
uw_gldesel_vpik() {}
uw_glprintString() {}
uw_glviewsg() {}
ncl_reset_cutseg() {}
uw_glrasput() {}
uw_glupdate_front() {}
uw_glclearws() {}
uw_glmark_dirty_rect(){}
um_is_light_init(){}
um_set_pocket_graphics(){}
getinp(){}
setinp(){}

UU_LOGICAL ncl_own_geometry ()
{
	return (NCLX_internal_geom != 1);
}
ncl_evolve_rbsp()
{
}
um_rbcrv_frmnclcrv()
{
}
um_allocate_curve()
{
}
um_curve_size()
{
}
ncl_evolve_line()
{
}
ncl_evolve_uvcv()
{
}
ncl_evolve_curve_gen()
{
}
ncl_evolve_composite_curve()
{
}
ncl_revers1_list()
{
}
ncl_maxim_crv1()
{
}
ncl_get1_tpar1()
{
}
ncl_put1_tpar1()
{
}
ncl_cp_struct_uvcv_rbcv1()
{
}
ncl_get_entity_color()
{
}
vxchk(){}
ncl_set_macptr(){}
ncl_get_macptr(){}

ncl_tessellate_polygon()
{
}
setnln(){}
getnln(){}
ub_get_symmaster_by_name(symptr, foundptr, fcase,flag)
struct UB_symbol_rec *symptr;
UU_LOGICAL *foundptr;
int fcase,flag;
{
	*foundptr = UU_FALSE;
	return -1;
}
ncl_parse_label()
{
}
flopnv(ival, jval)
UM_int2 *ival;
UM_int2 *jval;
{
	*jval=*ival;
}
/*NCL only*/
ncl_delete_cutseg(){}
ncl_mot_stack_init(){}
mottln(){}
motccm(){}
motcln(){}
motspn(){}
ncl_closeobj(){}
ncl_draw_part(){}
ncl_makeobj(){}
ncl_redrawws()
{
}
pltmot(){}
ncl_get_cutgeo(){} /*NCL only*/
ncl_draw_cutsym()
{
}
ncl_get_cutsym()
{
}
ncl_move_cutsym()
{
}
/*......need unibase */
int ubu_load_symmaster()
{
	return -1;
}
char **ub_get_symmaster_name(number)
int *number;
{
	*number = 0;
	return NULL;
}
/*
.....used by clfile function which not used by NCLIPV */
ncl_zroptr(){}
aptcl(){}
getclf(){}
aptsrc(){}
ncl_setptr()
{
}
ncdate()
{
}
nctime()
{
}
ncl_tstptr()
{
}
ncl_eqlptr()
{
}
clput()
{
}
ncl_step_motion()
{
}
/* called by nclf_cutter_get_bounds() which called from cutget.f, not used by NCLIPV */
ncl_cutsym_box() 
{
} 
/* called by nclf_cutter_get_bounds() which called from cutget.f, not used by NCLIPV */
ncl_cutter_box() 
{
} /* called by nclf_cutter_get_bounds() which called from cutget.f, not used by NCLIPV */
ncl_cutter_box_add() {} /* called by nclf_cutter_get_bounds() which called from cutget.f, not used by NCLIPV */
ncl_cutter_box_init() {} /* called by nclf_cutter_get_bounds() which called from cutget.f, not used by NCLIPV */
um_plane1(){} /* used in ncl_display_blade_cutter */
ubi_load_file() /* used in ncl_load_cutter_symbol */
{
	return -1;
}
asmult(){} /* no show source */
asclnm(){} /* no show source */
asgoto(){} /* no show source */
ascutr(){} /* no show source */
aspwrd(){}
/*
.....source for NCL only 
*/
/*
int ncl_getsrc_rec(nrec,nline,sbuf,knc)
int nrec,*nline;
char *sbuf;
int *knc;
{
	*knc = 0;
	sbuf[0] = '\0';
	return -1;
}
*/
gtrafl(flag)
UM_int4 *flag;
{
	*flag = 0;
}
ncl_load_stl()
{
}
/*nclipvvw.c*/
/*
......we can't get the geo from NCL yet. So just ignore this fuction
*/
gtmx(){}
/*
......always not set ifl(73) and ifl(72) (default to 0)
......TRACUT
......REFSYS
*/
gtview(rsorig, rszvec, rsyvec, option, status)
UM_int2 *option, *status;
UM_real8 rsorig[3];
UM_real8 rszvec[3];
UM_real8 rsyvec[3];
{
	*status = 1;
}
um_ploctocc(){}
uvu_change_view(){}
//uv_locwin(){}
/*
......not used any geom from NCL database
*/
void getkey(f77label, nclkey)
UM_f77_str_ptr f77label;
UM_int4 *nclkey;
{
	*nclkey = 0;
}
/*
......not use NCL label
*/
ncl_format_label(){}
uw_glset_dirty_flag(){}
um_update_geom(){}

ncl_setver(){}
nclipv_terminate(){}
clswin(){}
_ud_rpwrcom(){}
uw_setpkt_cursor(){}
uw_setpkt_cursor_val (){}
uw_ntupd_cinput(){}

ud_setpick_type(){}
um_load_pocket_drawing (){}
ud_rdform(){}
ud_rpwrform (){}
/*
.....unibase files
*/
um_snapsave_romulus(){}
um_load_appgeo(){}
um_ld_romulus(){}
um_pre_load(){}
um_post_load(){}
ncl_tst_un_label(){}
ncl_fix_attr_label(){}
um_retrieve_data_fixed()
{
}
um_pre_save(){}
um_post_save(){}
um_sv_romulus(){}
ul_close_window(){}
ul_win_out(){}
ul_open_window(){}
ud_rpwrcom(){}
opnwin(){}
ud_getpick_type(){}
wf_geom_type(){}
ncl_geom_type(){}
ncl_get_surf_attr(){}
um_reset_labels(){}
umi_init_rel_label(){}
um_save_labels(){}
uv_set_immediate_mode(){}
uv_set_defered_mode(){}
um_post_restore(){}
um_restore_romulus(){}
um_post_load_appgeo(){}

ud_rpwrmenu(){}
ncvclf(){}
um_get_all_geom(){}
ud_unlimit(){}
nclu_add_list(){}
ncl_get_geo_color(){}
ncl_get_label(){}
nclu_repaint(){}
nclu_push_sfkey(){}
gettol(){}
ncl_retrieve_data()
{
}
uz_actsequnc(){}
ncl_update_geo_color(){}
ncl_err_mode(){}
nclsys(){}
setins(){}
insmode_val(){}
ncl_reset_cmdline(){}
um_pscroll(){}
ud_string_def(){}
putinf(){}
putinw(){}
uw_gllight_define(){}
uw_glinit_shading(){}
uw_gllight_pos(){}

ncl_solid_count_components() {}
uc_transform() {}
ncl_geo_box() {}
gtsky() {}

uc_init_txt3()
{
	return 0;
}
uc_init_txt2()
{
	return 0;
}
uc_init_txt_crv()
{
	return 0;
}
uc_init_txt_geo()
{
	return 0;
}
uc_init_txt_uni()
{
	return 0;
}
uc_init_drf62()
{
	return 0;
}
uc_init_drf44()
{
	return 0;
}
uc_init_drf42()
{
	return 0;
}
uc_init_drf40()
{
	return 0;
}
uc_init_drf31()
{
	return 0;
}
uc_init_drf20()
{
	return 0;
}
uc_init_drf10()
{
	return 0;
}
uc_init_drf8()
{
	return 0;
}
uc_init_drf7()
{
	return 0;
}
uc_init_drf5()
{
	return 0;
}
uc_init_drf4()
{
	return 0;
}
uc_init_drf3()
{
	return 0;
}
uc_init_drf2()
{
	return 0;
}
uc_init_drf1()
{
	return 0;
}
uc_init_drf_app()
{
	return 0;
}
uc_init_drf_sym()
{
	return 0;
}
uc_init_drf_rel()
{
	return 0;
}
uc_init_drf_sol()
{
	return 0;
}
uc_init_drf_sur()
{
	return 0;
}
uc_init_drf_crv()
{
	return 0;
}
uc_init_drf_geo()
{
	return 0;
}
uc_init_drf_uni()
{
	return 0;
}
ncl_init_textvar()
{
	return 0;
}
ncl_init_datast()
{
	return 0;
}
ncl_init_revsurf()
{
	return 0;
}
ncl_init_trimsf()
{
	return 0;
}
ncl_init_pntvec()
{
	return 0;
}
ncl_init_evalsf()
{
	return 0;
}
ncl_init_evalcv()
{
	return 0;
}
ncl_init_labloc()
{
	return 0;
}
ncl_init_scalar()
{
	return 0;
}
ncl_init_netsf()
{
	return 0;
}
ncl_init_patern()
{
	return 0;
}
ncl_init_plane()
{
	return 0;
}
ncl_init_circle()
{
	return 0;
}
ncl_init_line()
{
	return 0;
}
ncl_init_point()
{
	return 0;
}
ncl_init_shape()
{
	return 0;
}
ncl_init_quiltsf()
{
	return 0;
}
ncl_init_meshsf()
{
	return 0;
}
ncl_init_panel()
{
	return 0;
}
ncl_init_surf()
{
	return 0;
}
ncl_init_curve()
{
	return 0;
}
ncl_init_matrix()
{
	return 0;
}
ncl_init_vector()
{
	return 0;
}
ncl_init_attr()
{
	return 0;
}
uc_init_txtatt()
{
	return 0;
}
uc_init_text()
{
	return 0;
}
uc_init_connector()
{
	return 0;
}
uc_init_attsym()
{
	return 0;
}
uc_init_inssym()
{
	return 0;
}
uc_init_massym()
{
	return 0;
}
uc_init_xhatch()
{
	return 0;
}
uc_init_lindim()
{
	return 0;
}
uc_init_group()
{
	return 0;
}
uc_init_polyline()
{
	return 0;
}
uc_init_polygon()
{
	return 0;
}
uc_init_modatt()
{
	return 0;
}
uc_init_tran()
{
	return 0;
}
ncl_init_solid()
{
	return 0;
}
ncl_init_surfattr()
{
	return 0;
}
uc_init_cvsf()
{
	return 0;
}
ncl_init_rbsf()
{
	return 0;
}
uc_init_agsrf()
{
	return 0;
}
uc_init_agcrv()
{
	return 0;
}
uc_init_rspl()
{
	return 0;
}
uc_init_compcrv()
{
	return 0;
}
uc_init_conic()
{
	return 0;
}
uc_init_circle()
{
	return 0;
}
uc_init_line()
{
	return 0;
}
uc_init_point()
{
	return 0;
}
uc_init_application_class()
{
	return 0;
}
uc_init_attribute_class()
{
	return 0;
}
uc_init_symbol_class()
{
	return 0;
}
uc_init_related_class()
{
	return 0;
}
uc_init_anno_class()
{
	return 0;
}
uc_init_surface_class()
{
	return 0;
}
uc_init_curve_class()
{
	return 0;
}
uc_init_geom_class()
{
	return 0;
}
um_init_rel(rel_num, rel_name, expansion_factor, fixed_size,
				nbr_varlists, atom_size, list_size)
	int rel_num;
	char *rel_name;
	int expansion_factor;	
	int fixed_size;
	int nbr_varlists;
	int atom_size[];
	int list_size[];
{
	int status;

	status = UU_SUCCESS;
	if ( ur_init_rel(rel_num, rel_name, expansion_factor, fixed_size, 
				nbr_varlists, atom_size, list_size) != 0)
	{
		uu_uerror1(UM_MODEL, 168, rel_num);
		status = UU_FAILURE;
	}
	return(status);
}

um_init_transf()
{
	return 0;
}
um_init_attr()
{
	return 0;
}
ub_init_msym_rel()
{
	return 0;
}
ub_init_instance_rel()
{
	return 0;
}
ub_con_init_relations()
{
	return 0;
}
um_query(){}
um_cannot_undelete(){}
um_delete_all(){}
um_retrieve_data_relnum()
{
}
ncl_draw_unilabel(){}
um_d_pickresolve(){}
uc_delete(){}
um_p43_coordsys(){}
uv_disp_entity(){}
um_drw43_coordsys(){}
um_dl38_light(){}
um_create_geom(){}
um_tf38_tranflight(){}
um_mirror_using_matrix(){}
um_scale_using_matrix(){}
um_rotate_using_matrix(){}
um_translate_using_matrix(){}
um_p38_light(){}
um_drw38_light(){}
um_light_ploc_to_vector(){}
um_light_ploc_to_coord(){}
um_dl46_deldrawing(){}
um_p46_drawing(){}
um_drw46_drawing(){}

um_drwaxis(origin,xaxis,yaxis,zaxis,length)
	UM_coord origin;
	UM_vector xaxis;
	UM_vector yaxis;
	UM_vector zaxis;
	UM_length length;
{
	return ul_drw_vaxis_ipv(origin,xaxis,yaxis,zaxis,length);
}
uv_print_view(){}
uv_print_viewport(){}
uv_print_screen(){}
void ncl_mcstf(iflg, mcstf)
   int *iflg;
   UM_transf mcstf;
{
   *iflg = 0;
}
um_set_active_layer(){}
ud_resuwin(){}
ud_suspwin(){}
ud_rpwr(){}
ud_rprd()
{
}
ud_playinit(){}
uj_tutorial(){}
ux_write_logfile(){}
ud_outevt(){return 0;}
uq_calc2(){return 0;}
um_feasub(){return 0;}
ud_filter_entity(){}
ud_iconnum()
{
}
ud_reject()
{
}
ncl_srcctl_put() {}

UU_LOGICAL ud_motion_pick_type()
{
	return(UU_FALSE);
}

ud_update_win_title()
{
	nclipv_update_win_title();
}
uz_repaint()
{
	nclipv_repaint();
}

void getver(ver)
UM_real8 *ver;
{
	*ver = NCL_version;
	return;
}
nclu_update_pre_view(view)
UV_view view;
{
	ipv_update_pre_view(view);
}
uz_dyn_mouse() 
{
	nclipv_dynmouse();
}
void uw_glset_context(which,force)
UM_pkwin_type which;
UU_LOGICAL force;
{
	uw_ntset_context(which,force);
}
uz_extrema_zoom()
{
	nclipv_vpzoomextrema();
}

um_pocket_window(){ return 0;}
um_close_pocket_window(){ return 0;}
char *um_get_pocket_window(type)
UM_pkwin_type type;
{
	char *win;
	uw_ntget_mainwin(&win);
	return(win);
}
/*
......call by function for NCLIPV Measurement form
......seem just ignore and return no source
*/
void gtpsrc(cline, cnchar, cstr)
int *cline;
short *cnchar;
char *cstr;
{
	*cnchar = 0;
}
/*
......not yet for create solid
*/
ncl_ipv_contour_stock(){}
ncl_solid_to_stock() 
{
	return -1;
}
ncl_solid_wcstomcs(){}
uw_ntupd_statusfont(){}
um_free_boundary(){}
um_centroid(){}
ncl_get_boundary(){}
ud_isassist_seg(){}

void mcsmx(iflg,buf)
UM_int2 iflg;
UU_REAL buf[];
{
	int i;
	iflg = 0;
	for (i=0;i<12;i++) buf[i] = 0.;
	buf[0] = buf[5] = buf[10] = 1.;
}

void nclf_transf() {}
void nclf_invmx() {}
void ncl_update_colors(flag)
int flag;
{
	int i;

	if (flag)
		ncl_save_clrmod();
	for (i=0; i<64;i++)
	{
		UM_pkcolors[i].red = uw_color_table[i][0];
		UM_pkcolors[i].green = uw_color_table[i][1];
		UM_pkcolors[i].blue = uw_color_table[i][2];
	}
	ul_ipv_update_colors();
}

void ncl_srcctl_get() {}


void ncl_srcctl_get_count(key,nent)
UU_KEY_ID key;
int *nent;
{
	*nent = 0;
}

void nclu_load_matrix_list(dlist,sdef)
UD_LIST *dlist;
char *sdef;
{
	dlist->item = (char **)uu_malloc(sizeof(char *));
	dlist->answer = (char *)uu_malloc(sizeof(char)*NCL_MAX_LABEL_AND_SUBSCRIPT);
	dlist->item[0] = (char *)uu_malloc(sizeof(char)*NCL_MAX_LABEL_AND_SUBSCRIPT);
	dlist->num_item = 1;
	strcpy(dlist->answer," ");
	strcpy(dlist->item[0]," ");
}

void NclxDbgInit()
{
	char *p;
	int i;
	if (p=getenv("UU_DEBUGL"))
	{
		i = 0;
		sscanf(p,"%d",&i);
		if (i != 0)
		{
			Sfp = fopen("lp.lis","w");
			UY_nclxdebug = 1;
		}
	}
}

void NclxDbgPstr (sbuf)
char *sbuf;
{
	int i;
	if (UY_nclxdebug > 0)
	{
		char tbuf[256];
		strcpy(tbuf,sbuf);
		strcat(tbuf,"\n");
		fputs(tbuf,Sfp);
	}
}
ncl_reset_pikgeom(){}

uw_glput_pikgeom2(){}
ud_get_pickseg()
{
	return -1;
}
ud_getn_pick_segs()
{
	return -1;
}
ud_ddas_nearpt()
{
	return 0;
}
uvu_pickvp(vport)
UV_vport *vport;
{
	int stat;
	stat = uv_getvpid(LW_vport.key,vport);
	return stat;
}
/*
uv_dynzoom(){}
uv_dynzrot(){}
uv_dynxyrot(){}
uv_dyntumble(){}
uv_dynvecrot(){}
uv_dynmouse(){}
uv_dynpan(){}
*/
void ud_restore_hilgt_seg() {}

//void uv_get_box_midz(){}
int ud_ifpopup() {return(UU_FALSE);}
void uw_glput_pikgeom(){}
void uw_ntget_cmdstr2(){}
void uw_ntreplc_cmdstr(){}
void uw_ntset_livemouse(){}
int ncl_cmd_key(char *linestr)
{
	return 0;
}
int uw_ntload_cmdstr() 
{
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : chkwstr(token, nc1, wstr, nc2, match)
**        check if a token is match with wildcard string
**    PARAMETERS
**       INPUT  :
**          token     - token string to be checked
**			nc1:     - length of the token
**          wstr     - wildcard string to be compared
**			nc2:     - length of the wildcard string
**       OUTPUT :
**          match     - 1: matched
**						0: not matched
**    RETURNS      : Zero
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void chkwstr(token, nc1, wstr, nc2, match)
char *token, *wstr;
int *nc1, *nc2, *match;
{
	char string1[65], string2[65];
	strncpy(string1, token, *nc1);
	string1[*nc1]='\0';
	strncpy(string2, wstr, *nc2);
	string2[*nc2]='\0';
	*match = 0;
	if ((*nc1==0) || (*nc2==0)) 
		return;
	if (strcmp(string2, "*")==0)
	{
		*match = 1;
		return;
	}
	*match = S_ncl_match(string2, string1);
	if (*match==-1)
		*match = 0;
}

/*********************************************************************
**    E_FUNCTION     : S_ncl_match (pattern, text)
**        check if a text is match with wildcard string
**    PARAMETERS
**       INPUT  :
**          text     - text string to be checked
**          pattern     - wildcard string to be compared
**       OUTPUT : none
**    RETURNS      : 
**          match     - 1: matched
**						0: not matched
**						-1: abort
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_ncl_match (pattern, text)
char *pattern, *text;
{
	for ( ; *pattern; pattern++, text++)
	{
		if (text[0]=='\0')
		{
			if (*pattern== '*'&& *++pattern == '\0')
				return 1;  /* matched */
			else
				return -1;
		}
		switch (*pattern)
		{
			case '*': 
				return S_match_after_star (pattern, text);
			default:
				if (*pattern != *text)
					return 0;
		}
    }
/* 
.....if end of text not reached then the pattern fails 
*/
	if (*text)
       return 0;
	else  
		return 1;
}

/*********************************************************************
**    E_FUNCTION     : S_match_after_star (pattern, text)
**        check if a text is match with wildcard string
**    PARAMETERS
**       INPUT  :
**          text     - text string to be checked
**          pattern     - wildcard string to be compared
**       OUTPUT : none
**    RETURNS      : 
**          match     - 1: matched
**						0: not matched
**						-1: abort
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_match_after_star (pattern, text)
char *pattern, *text;
{
	int match = 0;
	char nextp;

	while (*pattern=='*')
	{
		pattern++;
	}
	if (*pattern=='\0')
		return 1;

	nextp = *pattern;
	do
	{
		if (nextp == *text)
			match = S_ncl_match(pattern, text);
		if (*text==0)
			match = -1;
		text++;
	} while ( match != 1 && match != -1 );
	if (match==-1) match = 0;
	return match;
}
/*********************************************************************
**    E_FUNCTION         :  ncl_filter_str2(label, filter)
**       This function doing the same as ncl_filter_str but the filter is without "*"
**		and the 'filter' can be anywhere in the string
**
**    PARAMETERS   
**       INPUT  : label: string to be checked
**					filter: filter string
**       OUTPUT : 
**    RETURNS      : 0: not match, 1: match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_filter_str2(label, filter)
char *label, *filter;
{
	char tmpstr[1024], label_str[1024], filter_str[1024];
	int i, nc1, nc2, match;
/*
.....should not change the input string
*/
	strcpy(label_str, label);
	strcpy(filter_str, filter);
	nc1 = strlen (label_str);
	while ((nc1>1)&&(label_str[nc1-1]==' ')) nc1--;
	label_str[nc1] = '\0';
/*
.....remove '*' in the front and end, then add in 
.....(to make sure the phase can be anywhere in the string)
*/
	nc2 = strlen (filter_str);
	i=0;
	while ((nc2>1)&&(filter_str[nc2-1]==' ')) nc2--;
	if (nc2==0)
/*
.....all empty for filter, always return matched
*/
		return 1;

	while ((nc2>1)&&(filter_str[nc2-1]=='*')) nc2--;
	filter_str[nc2] = '\0';
	
	if (nc2>0)
	{
		strcpy(tmpstr, "*");
		strcat(tmpstr, filter_str);
		strcat(tmpstr, "*");
		strcpy(filter_str, tmpstr);
		nc2 = strlen (filter_str);
		chkwstr(label_str, &nc1, filter_str, &nc2, &match);
		return match;
	}
/*
......if nc2 = 0, if mean only have "*", so always match
*/
	return 1;
}
void ud_unhilite_sellist() {}
int ud_get_pick_selstr()
{
	return 1;
}
void scutlb (char *clib)
{}

ub_get_symmaster_name_path()
{}

ub_retrieve_sym()
{}

ncl_update_input()
{}

void nclf_set_ctr_hgt(chc, rnum)
UM_real8 *rnum;
UM_int2 *chc;
{}


