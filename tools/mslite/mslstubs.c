/*********************************************************************
**    NAME         :  mslstubs.c
**
**       CONTAINS:
**		dummy defines and functions
**
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       mslstubs.c , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 15:30:24
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"

#define DPGM 1
#include "dmotif.h"
#include "usysg.h"
#undef DPGM

#define NCLPLAYBACK 1
#define UM_MPGM 1
#include "nclmplay.h"
#undef NCLPLAYBACK

#include "mdcoord.h"
#include "mdrwsize.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mpocket.h"
#include "m2dattr.h"
#undef UM_MPGM

#define VPGM
#include "view1.h"
#undef VPGM

#define NCLVERSION
#include "nclver.h"		/* Defines's version number */

#define SPMOUSE_MAIN
#include "spmouse.h"
#undef SPMOUSE_MAIN

#include "ws.h"
#include "wsgl.h"

#define NCL_ICONS_FOR_INTER 1
#include "nclicons.h"
#undef NCL_ICONS_FOR_INTER
#include "nclmplay.h"
#include "gtblws.h"
#include "mdattr.h"
#include "udfrmdef.h"
#include "mdeval.h"

int NCL_cmdmod = 0;
int UW_signon = 0;
int IPV_resize = 1;
UU_LOGICAL UM_plotting = UU_TRUE;
UU_REAL	UM_plotprec = 0;
int UU_application = UU_NCLIPV;
int NCLX_internal_geom = 0;
	
int NCL_accuracy;
double toler = UM_FUZZ;
UV_view UV_dynview, dyn_view;
int UZ_nclipv_view;
int UV_current_sview;
int UM_material_reset;
UN_cutdef_struc cutdef[UV_NVPORTS];

struct UM_attrmdl_rec
{
	UU_KEY_ID	key;
};
struct UM_attrmdl_rec UM_attrmdl;

int UL_clswin_flag = 0;
int NCL_macro_outflag, NCL_macro_remval, NCL_macro_outdefault, NCL_macro_modal;

int UM_swap_ipv = 0;
char *UM_pocket_hwnd = 0;

int NAUTLTH = 0, NAUTSTEP = 0;
int NCL_edit_mode,NCL_cmd_window_mode,NCL_com_mode,NCL_mark_method;
int UN_motion_color=1, UN_rapid_color=3;
int UN_motion_line=UM_SOLID_LINE, UN_rapid_line=UM_DASHED_LINE;
int UN_motion_pen=1, UN_rapid_pen=1;

char NCL_init_fstr[20];

int Space_mouse_draw = 0;
int UW_menu_fmt,UW_icon_size,PKx,PKy, UW_light_reset,UW_pocket_mode;
#define NALPHAKEYS 18
static char *alpha[NALPHAKEYS]={"0","1","2","3","4","5","6","7","8","9",
	".",",","+","-","/","*","<-","ENTER"};

int UU_application;
int NCLHOST,UD_host,NCL_pick_aper,UD_duimsdeflt/*,UD_enablejmp*/;
int	UD_curlayout,UD_errorsegflag/*,UD_markptr,UD_markstk*/;
int NCL_mot_seg;
int NAUTCAM=0, NAUTIGES=0;
int NCLX_external_unibase=0;

int UW_modelview_depth = 32, UW_project_depth = 2;
int UD_printipv = 0;
int UD_pickmode,UD_chain_mod,UN_mot_stack_size,NCL_pick_verify;
#define MAXLIGHT 15
int UM_light_keys[MAXLIGHT];

int UJ_plotting, UD_opengl;
int UR_restore_mtrl;
int UR_restore_lights;
UU_LOGICAL UN_motmodal_cmd=UU_TRUE;
struct UM_mtrlmdl_rec
{
	int	index;
	char	name[64][20];
	UU_REAL	ka[64];
	UU_REAL	kd[64];
	UU_REAL	ks[64];
	UU_REAL	ks_r[64];
	UU_REAL	ks_g[64];
	UU_REAL	ks_b[64];
	UU_REAL	spec_exp[64];
};

struct UM_mtrlmdl_rec UM_mtrlmdl;
int LW_dyncenter = 0;
UU_REAL UV_dynvec[3], LW_dyncp[3], UV_dyncp[3];
int UV_dynview_active=UU_FALSE;
int UV_dynstatln = 1;
int UV_dyndisply = 0;
int UV_dyncenter = 0;

int UD_textpre_curpos, UD_string_add;
char NCL_infile_date[20];

getifl(idx,ival)
short *idx,*ival;
{
/*
.....message disp
*/
	if (*idx==35) *ival = 0;
/*
....UNITS, INCH
*/
	if (*idx==264) *ival = 0;
}

setifl(idx,ival) 
short *idx,*ival;
{
/*
.....only set ifl = 86 (stop motion) = 1
*/
}
getjfl() {}
setjfl() {}
void gettol(tol)
UU_REAL *tol;
{
	*tol = .001;
	return;
}
/*
......call by function for NCLIPV Measurement form
......ignore now
*/
gtpsrc() 
{
}
ncl_wcstomcs() {}

/*not consider motion and cutter now */
ncl_mot_stack_init() 
{
}
/*not consider motion and cutter now*/
ncl_display_motion() 
{
}
/*not consider motion and cutter now*/
ncl_motion_extrema() 
{
}
/*
.....used by ug_findsgpk in digs.lib which not related to MSL
*/
uv_getvpid() 
{
}
/*
.....only call by NCLIPV Measurement form (stop motion)
......ignore now
*/
rsttrm() 
{
}
ud_is_playback() 
{
	return 0;
}
/*
......NCL functions not used
*/
/////////////////////////////
/*
.....only for NCL picking used in digs.lib
*/
pick_select_verify() 
{
}
/*
.....No pocket window for MSLITE. called from form
*/
um_close_pocket_window()
{}
um_load_pocket_drawing()
{}
/*
.....no unibase
*/
ur_skip_ent() {}
ur_save_part() {}
ur_update_blanked() {}
ur_update_attr() {}
ur_update_data() {}
ur_retrieve_blanked() {}
uc_retrieve_attr() {}
uc_retrieve_data() {}
ur_setup_data(){}
/*
....not use, called by um_tf2_tranfpln in file m3ecpln1.c
*/
um_update_geom(){}
udi_mu_reset() {}
uj_help(){}
ud_rpwrmenu() {}
umu_inactgrid() {}
umu_view_model() {}
um_reset_pocket_graphics() {}
uw_ntupdate_accel() {}
uw_ntresize_graphics() {}
uw_ntinit_view(){}
opnwin(){}
ud_rpwrcom(){}
ud_setpick_type()
{
}
ud_getpick_type()
{
}
/*
.....no playback
*/
ud_rpwrform() {}
ud_brower_endrprd(){}
ud_rprd_ipvloc(){}
ud_rpwr_ipvloc(){}
/*
......NCL function called but will not used in MSLITE
*/
void uw_glresize_graphics() {}
/*
.....not used function called from d4actfm.c
*/
void ud_trmoff() {}
void ud_rdform() {}
void uw_setpkt_cursor_val (){}
void uw_setpkt_cursor (){}

/*
......functions may implement later
*/
/////////////////////////////////////
ncl_get_playfile() 
{
}
ncl_process_ipvcutter() 
{
}
ncl_load_cutprof() 
{
}
ncl_load_cutter_symbol() 
{
}
//not consider motion and cutter now
ul_ipv_cutter() 
{
}
//not consider motion and cutter now
ncl_play_resetscan() 
{
}
//not consider motion and cutter now
ncl_play_initscan() 
{
}
ncl_label_cmp(){}
/*
.....in MSL project, it only used by ud_actuims in d6layrn.c
.....and ul_string_def (in input function now, may add later) in lmisc.c
*/
ud_ddas() 
{
}
/*
.....no mod file loaded now
*/
ul_ipv_load_mod()
{}
/*
......not use mode file ncl502.mod for now, may set MSL mode file later
*/
ul_load_nis_mod() 
{
}
/*
.....no monitor displayed
*/
ul_ipv_monitor_form() {}
/*
.....no status display
*/
ud_pstat(){}
/*
.....no status
*/				
uz_status()
{}
void clswin() {}
ncl_set_statact() {}
/*
.....not used. no input function now, only call by d1dopngl.c
*/
void imouse3() 
{
}
char *um_get_ncl_win()
{
	char *win;
	uw_ntget_mainwin(&win);
	return(win);
}
char *um_get_pocket_window(type)
UM_pkwin_type type;
{
/*
.....no pocket window
*/
	return NULL;
}
/*
......try not to use nclf.lib
......may need add/change
*/
void getsc (short *ifl, double *val)
{
	if (*ifl==175)
		*val = 0.005;
/*
.....used default sc(27) = .025 (MM) or .001 (INCH)
*/
	if (*ifl==27)
		*val = 0.001;
}
void ul_ipv_diag_form()
{
	msl_diag_form();
}
/*
......the version is always true in MSLITE
*/
UU_LOGICAL ncl_setver (ver)
int ver;
{
	return UU_TRUE;
}
void uw_ntset_context(which,force)
UM_pkwin_type which;
UU_LOGICAL force;
{
	msl_win_context();
}
void uw_glset_context(which,force)
UM_pkwin_type which;
UU_LOGICAL force;
{
	uw_ntset_context(which,force);
}
//not consider motion and cutter now
ncl_motion_playback(pmod,scan,bounds,clist,ncutr)
int pmod[],scan;
UU_REAL bounds[6];
UU_LIST *clist;
int *ncutr;
{
	int i;
/*
.....Initialize Scan Type 2 variables
*/
	if (scan == 2)
	{
		for (i=0;i<3;i++)
		{
			bounds[i] = 10000. ; bounds[i+3] = -10000.;
		}
	}
}
uc_init_evcrvout (eptr, evoutptr)
	struct UM_crvdatabag *eptr;
	struct UM_evcrvout *evoutptr;
{
	evoutptr->firsteval = UU_TRUE;
	return UU_SUCCESS;
}
ud_get_pickstr(){}
ncl_get_clfile_src(){}
uz_user_dascalls() {}
uz_daskey1() {}
uw_gllight_pos(){}
uv_updatevp () {}
um_set_screen_area() {}
uv_putv() {}
uv_getvid() {}
ncl_cutseg_front() {}
uw_gldesel_vpik() {}
uw_glprintString() {}
UV_dynsegs() {}
uw_glviewsg() {}
ncl_reset_cutseg() {}
uw_glrasput() {}
uw_glupdate_front() {}
ur_update_data_fixed() {}
uw_glclearws() {}
uw_glmark_dirty_rect(){}
um_is_light_init(){}
um_set_pocket_graphics(){}
mcswcs() {}
ncl_own_geometry(){}
ncl_evolve_rbsp(){}
um_rbcrv_frmnclcrv(){}
um_allocate_curve(){}
um_curve_size(){}
ncl_evolve_line(){}
ncl_evolve_uvcv(){}
ncl_evolve_curve_gen(){}
ncl_evolve_composite_curve(){}
ncl_itsa_compcrv(){}
ncl_revers1_list(){}
ncl_compcrv_getelm(){}
ncl_compcrv_getnents(){}
ncl_get_entity_color(){}
ur_update_color(){}
ur_retrieve_data(){}
vxchk(){}
/*ncl_parse_scalar_values(){} */
ur_retrieve_data_relnum(){}
ncl_set_macptr(){}
ncl_get_macptr(){}

ncl_tessellate_polygon(){}
ncl_tessel_polyline(){}
um_proj_pt_on_plane(){}
um_init_tess(){}
um_free_tess(){}
um_plane1(){}
ncl_draw_lathe_cutter(){}
ncl_display_lathe_cutter(){}
uc_retrieve_transf(){}
ncl_get_sclar_frm() {return(-1);}
ncl_load_stl(){}
NclxDbgPstr(){}
uqi_gsym(){return(UU_FALSE);}
ul_ipv_display_obstruct(){}
nclc_save_recent_file(){}
void ur_retrieve_transf() {}
/*
.....NCL command routines
*/
void ncl_add_token() {}
void ncl_init_cmdbuf() {}
void ncl_add_cmdbuf() {}
void ncl_call() {}
void ncl_set_cmdmode(){}

void um_modaxisinit(){}
void um_cplinit(){}
/*not consider motion and cutter*/
int LW_mot_stack_size;
UU_LOGICAL LW_mot_stack_active,LW_mot_stack_fixture;
ul_ipv_mot_stack_init() {}
ul_ipv_mot_stack_delete() {}
ul_ipv_mot_stack_push() {}
ul_ipv_mot_stack_step() {}
ul_ipv_mot_stack_del_sess(){}
void ul_ipv_reset_tool_props(){}
void ul_verify_draw_sweep() {}
void ul_verify_draw_revsf() {}
void ul_verify_draw_box() {}

void uc_transform() {}
void ncl_retrieve_data() {}
void ncl_geo_box() {}
void gtsky() {}

void uz_repaint(){}
void ud_lgeo(){}
void nclf_transf(){}
void nclf_invmx(){}
void mcsmx(){}
int NCL_clipf = 0;

void ncl_update_colors(){}
void ckintr(){}
void uv_get_box_midz() {}

void ul_get_ipvbox_midz(llf,urb,midpt)
UU_REAL midpt[],llf[],urb[];
{
   midpt[0] = (urb[0] + llf[0]) / 2.0;
   midpt[1] = (urb[1] + llf[1]) / 2.0;
   midpt[2] = 0.;
}
	
int UV_dynwheel = 0;
char UD_prompt[1224];

void setnln() {}
void nclf_getsrc() {}
void getnln() {}
void nclc_putsrc() {}
void nclc_delsrc() {}
void ncl_cmd_key(){}
void uw_ntmenu_desgn(){}
void uj_noop(){}

void uw_ntget_cmdstr2() {}
void uw_ntreplc_cmdstr() {}
void uw_ntload_cmdstr(){}
void ncl_reset_pikgeom(){}
void ncl_retrieve_data_fixed(){}
void asvoc(){}

UU_LOGICAL ncl_on_verify_segno()
{
	return(UU_FALSE);
}
/*********************************************************************
**    E_SUBROUTINE     : subroutine getver
**          C callable routine to get the NCL version number.
**       INPUT  : 
**          none
**       OUTPUT :  
**          ver    -  Current NCL version number.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
getver(ver)
UM_real8 *ver;
	{
	*ver = NCL_version;
	return;
	}

int insmode_val() {return(0);}
ncl_reset_cmdline() {}
void ud_unhilite_sellist() {}
int ud_get_pick_selstr()
{
	return 1;
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
**    E_FUNCTION     : chkwstr(token, nc1, wstr, nc2, match)
**        check if a token is match with wildcard string
**    PARAMETERS
**       INPUT  :
**          token - token string to be checked
**			   nc1:  - length of the token
**          wstr  - wildcard string to be compared
**	         nc2:  - length of the wildcard string
**       OUTPUT :
**          match - 1: matched
**						  0: not matched
**    RETURNS      : none
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
