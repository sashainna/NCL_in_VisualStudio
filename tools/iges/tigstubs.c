/*********************************************************************
**    NAME         :  tigstubs.c
**       CONTAINS:
**             misc. stubs
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**			tigstubs.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			10/27/16 , 14:55:18
*********************************************************************/
#include <setjmp.h>
#include "class.h"
#define ATEXT
#include "atext.h"
#undef ATEXT
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "nclfc.h"
#include "nclver.h"
#include "nccs.h"
#include "ncolorddl.h"

#include "uims.h"
#include "view0.h"
#include "xenv1.h"
#include "mattrddl.h"
#include "tiges.h"
#include "mgeom.h"

int UW_clr_selected;
int UM_materal_reset;
int UM_material_reset;
int NCL_accuracy;
int LW_nclipv;
/*
....Added for NCL V9.7 release
*/
int NCL_subprocess;
void ua_init_txtrec() {}
void ul_chkkey_common() {}
/*
.....Added for NCL V9.6 release
*/
UU_LOGICAL ncl_setver(nver)
int nver;
{
   return (UU_FALSE);
}
UU_LOGICAL ncl_fmill_past()
{
   return (UU_FALSE);
}
void ncl_percnt_on_sf1() {}
void getkey() {}
UU_KEY_ID fml_bskey;
int ncl_geogn2() {return 0;}
int ncl_fix_evol() {return 0;}
/*
.....Added for NCL V9.5 release
*/
UU_REAL um_get_tess_toler() { return (0); }
int NCL_merge_overwrite;
int UD_wsptr;
int UD_gksws;
void strwf2() {}
void uv_update_secondview() {}
void ul_unibase_mtrl() {}
void uv_special_view() {}
char *uu_uprompt0() {return(UU_NULL);}
void ud_get_filename() {}
void uv_set_defered_mode() {}
void uv_set_immediate_mode() {}
/*void um_pre_load() {}*/
void um_ld_romulus() {}
/*void um_post_load() {}*/
/*void ncl_fix_attr_label() {}*/
void gdeactivatews() {}
void gclosews() {}
void gclosegks() {}
void um_feareset() {}
void uv_deactivsc() {}
void um_rm31_romini() {}

/* Temporary stubs inserted by Ian at Paul's request */
int UU_application;
int UL_lathe;
char UL_posts[31];
int UY_cs;
int UY_ps;
int UY_ds;
int UY_ncs;
int NCLX_internal_geom;
extern int NCLX_external_unibase;
int NCL_fmill_past=0;

/* End temporary stubs inserted by Ian at Paul's request */
/* vp 31-May-94; Stubs for unibase units support*/
/*
.....Added for NCL502 V8.400 release
.....Bobby  -  11/27/95
*/
/*UV_vport_info UV_act_vports[UG_MAXOPWS][UV_NVPORTS];*/
UD_UIMS UD_duimsdeflt;
UD_CURLAYOUT UD_curlayout;
int UMB_ICONS,UMBS_ICONS,CAM_ICONS,CAMS_ICONS,CADD_ICONS,CADDS_ICONS;
int SELECT_ICONS,ALTACT_ICONS,MOTION_ICONS,MODEL_ICONS;
int UD_enablejmp;
double toler;
extern UX_pathname UR_exnam[2];
int UR_save_display;
int UR_save_tessel;
int UW_light_reset;
int UW_Store_color = 0;

/*
.....Added stub for m2dba2.c
.....Himani
*/
int NCL_multi_ent;

void gcloseseg() {}
void gcreateseg() {}
void gdeleteseg() {}
void gfillarea3() {}
void gnseg() {}
void gpolyline3() {}
void gpolymarker3() {}
void gsfillcolor() {}
void gslinecolor() {}
void gslinetype() {}
void gslinewidth() {}
void gsmarkcolor() {}
void gsmarktype() {}
void gsnormtran() {}
void gspickid() {}
void gssegdet() {}
void gssegvis() {}
void ncl_display_motion() {}
void ncl_set_label_attr() {}
/*void ncl_store_wf2() {}*/
/*void ncl_vx_setunits() {}*/
void stunlb() {}
void ua_get_linear_units() {}
void uc_canbe_undeleted() {}
//void uc_copy() {}
void uc_display() {}
void uc_draw() {}
void uc_ploc_to_coord() {}
void uc_ploc_to_vector() {}
/*void uc_retrieve_attr() {}*/
void uc_span_entity() {}
void ud_ddas() {}
void ud_form() {}
void ud_get_plane() {}
void ud_ldas() {}
void uj_setpen() {}
void uj_miplotting() {}
void uv_chgsc() {}
void uv_clear() {}
void uv_delsegs() {}
void uv_disp_entity() {}
void uv_getobjs() {}
void uv_getvinfo() {}
void uv_getvofvp() {}
void uv_getvpid() {}
void uv_putsc() {}
void uv_blanksegs() {}
void uv_unblanksegs() {}
void uv_setdetectable() {}
UU_KEY_ID uv_scdefine() {return(0);}
void uv_set_vport() {}
void uv_vp_aspect_ratio() {}
void uvu_pickvp() {}
void uwx_get_flist() {}
void ncl_draw_unilabel() {}
void ncl_motion_extrema() {}
void ncl_get_tess_parms() {}
void ncl_set_tess_parms() {}
void ncl_update_geo_color() {}
void ncl_edges_to_tess() {}
void ncl_create_ssplin() {}
/*um_save_appgeo() {}*/
/*
.....no where define this function
*/
void uj_help() {}
void uw_mferror() {}
void uw_mfget_filename() {}
void uw_mfprmerr() {}
void uw_mfwrprm() {}
void uw_mfwrstat() {}



void ul_open_window () { }
void ul_close_window () { }
void ncl_ubput_ent () { }
/*ul_strip_blanks () { }*/
void ul_win_out () { }
void ud_das () { }
int UL_winrow,UL_wincol;
/* End of units stubs */

struct {
char z[80];
}UD_windev;
struct {
char z[80];
}UD_winrow;
struct {
char z[80];
}UD_wincol;
struct {
char z[80];
}UD_ksws;
struct {
char z[80];
}UD_markptr;

struct
{
	UU_LOGICAL stopflag;
	int debugsave;
	jmp_buf markenv;
}UD_markstk[300];

/*
struct {
char z[80];
}UM_cpln;
*/

void wintst() { }
void uerror() { }
void uu_uerror0() { }
void uu_uerror() {}
void uu_uerror1() { }
void uu_uerror2() { }
void uu_uerror3() { }
void ud_gevt() {  }
void ud_wrwin() {  }
void ud_hakt() {  }
void ud_crwin() {  }
void ud_kiwin() { }
void ud_wrerr() { }
void ud_jump() {  }
void ud_jmpmark() {  }
void ud_updatews() {}
void ug_gksstli() {  }
void ug_redrw() {  }
void uu_inithep() {  }
void uu_initapp() {  }
void umi_print_transformation() {  }
/* um_ret_romgeom () {return(UU_FAILURE);}   */
void um_pscroll() {  }
void um_p_ary() {  }
void uv_print_view() { }
uc_init_evcrvout(a,b) 
	char *a;
	char *b;
	{ 
	int status;
	status = um_init_evcrvout(a, b);
	return (status);
	}
uc_init_evsrfout(a,b) 
	char *a;
	char *b;
	{ 
	int status;
	status = um_init_evsrfout(a, b);
	return (status);
	}
uc_evcrv(a, b, c, d, e) 
	int a;
	UU_REAL b;
	struct UC_entitydatabag *c;
	UU_REAL *d;
	char *e;
	{
	int status;
	if (c->rel_num == UM_AGCRV_REL)
		status = um_agcrv_evaluate(a,b,c,d,e);
	else
		status = um_evcrv(a,b,c,d,e);
	return (status);
	}
uc_reverse_curve(crvptr)
	struct UC_entitydatabag *crvptr;
	{
	int status;
	switch (crvptr->rel_num)
	{
		case UM_LINE_REL:
			status = um_reverse_line(crvptr);
		break;
		case UM_CIRCLE_REL:
			status = um_reverse_circle(crvptr);
		break;
		case UM_CONIC_REL:
			status = um_reverse_conic(crvptr);
		break;
		case UM_RBSPLCRV_REL:
			status = ncl_rbsp_reverse(crvptr);
		break;
		case UM_AGCRV_REL:
/*			status = um_agcrv_reverse(crvptr);*/
		break;
		default: 
			status = UU_FAILURE;
	}
	return (status);
	}

int uc_cctou(crvptr,tfmat,pt,u,distp)
struct UC_entitydatabag *crvptr;
UM_transf tfmat;
UM_coord pt;
UU_REAL *u,*distp;
{
	int status;
	switch (crvptr->rel_num)
	{
	case UM_COMPCRV_REL:
		status = um_cctou_compcrv(crvptr,tfmat,pt,u,distp);
		break;
	default:
		status = um_cctou(crvptr,tfmat,pt,u,distp);
		break;
	}
	return(status);
}
	
int
um_agcrv_delete(key)
	UU_KEY_ID key;

	{
	struct UM_agcrv_rec crv;
	int status;

	crv.key = key;
	status = uc_retrieve_mtuple_data(&crv, sizeof(crv));
	if (status != UU_SUCCESS) goto done;

	status = umi_agcrv_delete(&crv);
	if (status != UU_SUCCESS) goto done;

	ur_delete_all(key);

done:
	return (status);
	}
#ifndef WNT
void ur1i_schedUpdate(priority,assocKey,causeKey,field,message,argc,argv)
	int			priority;
	int			assocKey;
	int			causeKey;
	int			field;
	int			message;
	int			argc;
	char			*argv[];
	{
	}
void ur1i_beginSchedUpdates() { }
void ur1i_commitSchedUpdates() { }
#endif
void uu_outputerr(){}
void ud_yesno(){}
void um_agshell_delete(){}
void um_agsrf_delete(){}
void um_set_feature_char_height(){}
void um_get_current_viewplane(){}
void um_feacoord(){}
void um_feavect(){}
void um_snapsave_romulus(){}
void uc_crv_to_unirbsc(){}
void ncl_def_color(){}
/* Temporary stubs inserted by Ian at Paul's request */
void ul_reset_control() { }

void uu_reset_usr_app (){}
void znu_init_machinetype (){}
void ur_save_part (){}
void ncl_init_batch (){}
void nclsig_ (){}
void getpp_ (){}

void ud_markover (){}
void runvt_ (){}
void ckintr (){}
void iuniplus (){}
void znu_init_units (){}
void uu_init_usr_app (){}
void runplus_ (){}
void uu_reset_mpe (){}
void iuni (){}
void uu_init_mpe (){}
void UL_3axis (){}
void ncl_reset_unicad (){}
void NclxMdlFindGeo() {}
/* void ncl_get_boundary_toler() {} */
void uu_ugerror0() {} /* ian 03may2000 */
void uu_ugerror1() {}
void ud_prmerr() {}
void ncl_retrieve_data() {}
void chk_crv() {}
void ncl_check_bnd() {}
void ncl_init_attr_rec() {}
void ncl_get1_tpar1() {}
void ncl_put1_tpar1() {}
void ncl_maxim_crv1() {}
void ncl_evsrf_tf() {}
void norm_to_cvonsf1() {}
void ncvevl() {}
void ncevolv() {}
void uwarn() {}

/* End temporary stubs inserted by Ian at Paul's request */
/* 
.....Added stubs for nevxmac.c
-Himani
*/
void ncl_get_macptr() {}
void ncl_set_macptr() {}
void ncl_del_maclist() {}
void ncl_maclist_init() {}
void ncl_maclist_done() {}
void ncl_del_first_called_macro(){}
void ncl_get_next_called_macro() {}

/*
..... Added stubs for nevxsup.
..... Himani
*/
void vx_mdl_inq_index() {}
void ncl_vx_create() {}
void ncl_vx_set_attr() {}
void ncl_vx_set_blank() {}

void ncl_get_asn_ptr() {}
/*void ncl_get_subscript() {}*/
void ncl_copy_geom() {}
void ncl_create_geom_drw() {}
void ncl_proj_label_to_drw() {}
void ncl_tst_un_label() {}
void ncl_go_along_surf_arclen() {}

void ncl_srcctl_put() {}
void ncl_sftopln() {}

void ub_display_sym() {}
void ua_display_drafting() {}
void ua_retrieve_data() {}
void ncl_ubput_keys() {}
void ncl_ubkey_search() {}
void ub_create_symbol_tuple() {}
void ub_create_instance_tuple() {}
void ub_get_symmaster_by_name() {}
void ubi_retrieve_inst_data_in_msym() {}

/*********************************************************************
**    E_FUNCTION     : int ncl_agcrv_conv (fp, agcrv, bsp)
**       Convert an AG curve to an NCL B-spline. If the AG curve contains
**       multiple splines, convert it to a composite curve containing
**       B-splines.
**    PARAMETERS   
**       INPUT  : 
**          fp         - Pointer to AG file.
**          agcrvp     - ptr to AG curve.
**       OUTPUT :  
**          bsp        - ptr to B_spline.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_agcrv_conv (fp, agcrvp, bsp)
FILE *fp;
struct UM_agcrv_rec *agcrvp;
struct UM_rbsplcrv_rec *bsp;

   {
   return (0);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_agsrf_conv (fp, agsrf, bsfp)
**       Convert an AG surface to an NCL B-spline surface.
**    PARAMETERS   
**       INPUT  : 
**          fp         - Pointer to AG file.
**          agsrfp     - ptr to AG surface.
**       OUTPUT :  
**          bsfp       - ptr to B_spline surface.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_agsrf_conv (fp, agsrfp, bsfp)
FILE *fp;
struct UM_agsrf_rec *agsrfp;
struct UM_rbsplsrf_rec *bsfp;

   {
   return (0);
   }

void ud_printmsg(msg)
char *msg;
{
#if UU_COMP!=UU_WIN2K
	printf(msg);
#else
	iges_wnt_msg(NULL, "Message", msg);
#endif
}
/*********************************************************************
**    E_FUNCTION     : int to_unibase(pt,pt0,type)
**       Stub for units, modsys & refsys conversion routine
**       not needed for iges.
**    PARAMETERS   
**       INPUT  : 
**          pt         - Input point/vector
**          type       - Not used
**       OUTPUT :  
**          pt0        - Output point/vector (unchanged)
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int to_unibase(pt,pt0,type)
UM_real8 pt[3],pt0[3];
UM_int2 type;
{
	um_vctovc(pt,pt0);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int from_unibase(pt,pt0,type)
**       Stub for units, modsys & refsys conversion routine
**       not needed for iges.
**    PARAMETERS   
**       INPUT  : 
**          pt         - Input point/vector
**          type       - Not used
**       OUTPUT :  
**          pt0        - Output point/vector (unchanged)
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int from_unibase(pt,pt0,type)
UM_real8 pt[3],pt0[3];
UM_int2 type;
{
	um_vctovc(pt,pt0);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : void uc_transform()
**       Stub for uc_transform, calls uig_transform().
**    PARAMETERS
**       INPUT  :
**          eptr  - Pointer to entity to transform.
**          tfmat - transformation matrix.
**          store - Store flag.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_transform(eptr, tfmat, store)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UU_LOGICAL store;
{
   return (uig_transform(eptr, tfmat, store));
}

/*********************************************************************
**    E_FUNCTION     : ncl_init_surf_attr(key,sfattr,type)
**       Stub for ncl_init_surf_attr, calls uig_init_surf_attr().
**    PARAMETERS
**       INPUT  :
**          key    - Key of surface to initialize attribute for.
**          type   - Should be set to NCLI_SURF (not used).
**       OUTPUT :
**          sfattr - Surface attribute record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_init_surf_attr(key,sfattr,type)
{
	return(uig_init_surf_attr(key,sfattr));
}

/********************************************************************
*********************************************************************/
UU_LOGICAL ncl_itsa_fml_base (key)
UU_KEY_ID key;
{
   return (UU_FALSE);
}
ncl_print_ln6()
{
}
mcswcs()
{}

/*********************************************************************
**    E_FUNCTION     : ncl_create_data(eptr)
**       Calls 'ur_creaste_data' directly.
**    PARAMETERS
**       INPUT  :
**          eptr  - Pointer to entity to create.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_data(eptr)
struct NCL_fixed_databag *eptr;
{
   return (ur_create_data(eptr));
}
/*************************************************************************
**       E_FUNCTION : ul_unibase_clr()
**          This function restores the color default modals
**       PARAMETERS
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void ul_unibase_clr()
{
	int i;
	for( i=0; i<UIG_MAXCOLOR; i++ )
	{
         uw_color_table[i][0] = uw_color_table_sav[i][0];
         uw_color_table[i][1] = uw_color_table_sav[i][1];
         uw_color_table[i][2] = uw_color_table_sav[i][2];
		 if (uw_color_name_sav[i][0] !='\0')
			strcpy(uw_color_name[i], uw_color_name_sav[i]);
		 else
			 uw_color_name[i][0] = '\0';
	}
}
/*********************************************************************
**    E_FUNCTION     : ncl_nclpl_to_umplane(pl,pln)
**        Convert NCL_nclpl_rec to UM_Plane .
**    PARAMETERS
**       INPUT  :
**          pl       - NCL_nclpl_rec
**       OUTPUT :
**          pln      - UM_Plane 
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_nclpl_to_umplane(pl,pln)
struct NCL_nclpl_rec *pl;
UM_plane *pln;
{
	um_vctovc(pl->pt,pln->p0);
	um_vctovc(pl->nvec,pln->n);
}
/*********************************************************************
**    E_FUNCTION     : ncl_init_color()
**       Initialize the color tuple
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_init_color()
{
	struct NCL_color_rec colors;
	int	i, entnum,status;
	int color_table[UIG_STDCOLOR][3] =
	{
		0, 0, 0,				/* Black */
		255,  255, 255, 	/* White */
		0, 0, 255,			/* Dodger Blue */
		255,  0,   0, 		/* Red */
		0,    255, 0,		/* Green */
		255,  0,   255, 	/* Magenta */
		255,  255, 0, 		/* Yellow */
		0,    255, 255,	/* Cyan */
		184,  134, 11, 	/* Dark Goldenrod */
		210,  180, 140,	/* Tan */
		173,  216, 230,	/* Light Blue */
		84,   255, 159,	/* SeaGreen1 */
		255,  165, 0,		/* Orange */
		255,  195, 203,	/* Pink */
		221,  160, 221, 	/* Plum */
		192,  192, 192,	/* Gray */
	};
	char color_name[UIG_STDCOLOR][96] =
	{
		"BLACK",
		"WHITE",
		"BLUE",
		"RED",
		"GREEN",
		"MAGNTA",
		"YELLOW",
		"CYAN",
		"BROWN",
		"LTTAN",
		"LTBLUE",
		"SEAGRN",
		"ORANGE",
		"PINK",
		"PURPLE",
		"GREY",
	};

	colors.rel_num = NCL_COLOR_REL;
	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(colors.rel_num, &entnum);
		if (status == 0)
		{
/*
......aready initialize, just return
*/
			return;
		}
	}
	colors.key = 0;
	for (i=0; i<UIG_STDCOLOR; i++)
	{
		strcpy(colors.color_name[i], color_name[i]);
		colors.color_value[i][0] = color_table[i][0];
		colors.color_value[i][1] = color_table[i][1];
		colors.color_value[i][2] = color_table[i][2];
	}
	for (i=UIG_STDCOLOR; i<UIG_MAXCOLOR; i++)
	{
		colors.color_name[i][0] = '\0';
		colors.color_value[i][0] = 192;
		colors.color_value[i][1] = 192;
		colors.color_value[i][2] = 192;
		uw_color_name[i][0] = '\0';
		uw_color_table[i][0] = colors.color_value[i][0];
		uw_color_table[i][1] = colors.color_value[i][1];
		uw_color_table[i][2] = colors.color_value[i][2];
	}
	ur_create_tuple(colors.rel_num, &entnum, &colors);
}

/*********************************************************************
**    E_FUNCTION     : ncl_post_load_color()
**       load color attributes into global color value
**
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					not used
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_post_load_color(loadoperation)
UU_LOGICAL loadoperation;
{
	int i, status, entnum;
	struct NCL_color_rec colors;
	if (NCL_infile_version < 9.850)
	{
		ncl_init_color();
	}
/*
.....get the color data and assign to global color vaue
*/
	entnum = 1;
	colors.rel_num = NCL_COLOR_REL;
	status = ur_get_next_tuple_index(colors.rel_num, &entnum);
	if (status == 0)
	{
		ur_retrieve_tuple(colors.rel_num, entnum, &colors);
		for (i=0; i<UIG_MAXCOLOR; i++)
		{
			if (colors.color_name[i][0]!='\0')
			{
				strcpy(uw_color_name[i], colors.color_name[i]);
/*
......SAVE ALL IN UPPER CASE
*/
				ul_to_upper(uw_color_name[i]);
			}
			else
				uw_color_name[i][0] = '\0';
			uw_color_table[i][0] = colors.color_value[i][0];
			uw_color_table[i][1] = colors.color_value[i][1];
			uw_color_table[i][2] = colors.color_value[i][2];
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_update_colors(flag)
**       updated the color class data in the unibase
**       
**    PARAMETERS   
**       INPUT  : 
**          flag: 1: save updated color into the ncl_color.mod file
**					0: not save
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_update_colors(flag)
int flag;
{
	int i, status, entnum;
	struct NCL_color_rec colors;

	status = 0;
	entnum = 1;
	colors.rel_num = NCL_COLOR_REL;

	status = ur_get_next_tuple_index(colors.rel_num, &entnum);
	if (status == 0)
	{
		ur_delete_tuple_abs(NCL_COLOR_REL, entnum);
		for (i=0; i<UIG_MAXCOLOR; i++)
		{
			if (uw_color_name[i][0]!='\0')
				strcpy(colors.color_name[i], uw_color_name[i]);
			else
				colors.color_name[i][0] = '\0';
			colors.color_value[i][0] = uw_color_table[i][0];
			colors.color_value[i][1] = uw_color_table[i][1];
			colors.color_value[i][2] = uw_color_table[i][2];
		}
		entnum = 1;
		ur_create_tuple(colors.rel_num, &entnum, &colors);
	}
}

uw_ntyes_or_no(int *parent, char* msg, char *title)
{
	int status = iges_wntyesno(parent, title, msg);
	if (status==0) return 1;
	return 0;
}

void ncl_ubchg_symnam() {}
void ncl_ubcheck_symnam() {}
void ncl_ubkey_delete() {}
void ncl_ubput_ent1() {}
uu_free_all() {}

UU_LOGICAL ncl_where=UU_FALSE;
UU_LOGICAL NCL_mot_seg=UU_FALSE;
int UL_color_mod=0;

void gdraw() {}
void glina3() {}
void ncl_disp_curve() {}
void ug_setredrwflag() {}
void ug_seghash() {}
void getsct() {}
void NclxDbgPstr() {}

void um_init_drawing_screen() {}
void um_view_model() {}

UU_LOGICAL um_check_connected() {return(UU_TRUE);}
void um_c5_connect_comp() {}
void um_c5_splitcompcrv() {}
void um_c5_isectcomp() {}
void um_c5_trimpart() {}
void um_c5_endpoints() {}
void um_is_curve_closed() {}
void ncl_c7_trimnclcrv() {}
void ncl_pocket_checkind() {}

void ud_free_tlist() {}
void ud_tlist_copy() {}
void ud_tlist_free_idata() {}
void ud_issame_idata() {}
void ud_tlist_copy_idata() {}
void ncl_filter_str2(){}


