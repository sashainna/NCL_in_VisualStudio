#include "usysdef.h"
/*********************************************************************
**  NAME:  tpdum.c
**			Contains dummy routines and global variable definitions
**			to resolve undefined symbols when linking CADPLOT.
**
**       CONTAINS:
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tpdum.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:13:19
*********************************************************************/

#if (UU_COMP != UU_WIN2K)
#include "nclfc.h"
#endif

int NCLHOST,UD_host,NCL_pick_aper,UD_duimsdeflt,UD_enablejmp;
int	UD_curlayout,UD_errorsegflag,UD_markptr,UD_markstk;
int UN_override_geo_attr,UN_override_geo_mask,UD_motif;
int NCL_mot_seg;
int NAUTCAM=0, NAUTIGES=0, NAUTSTEP=0;
int NCLX_external_unibase=0;
int UD_ksws;
int LW_nclipv=0;
int NCL_clipf;
int UD_selclip_npl;
struct {UU_REAL rplane[6][4]; int side[6]; int cross[6];} UD_selclip;
/*
.....added for native Window NT
.....Yurong 1/20/00
*/
#if (UU_COMP == UU_WIN2K)
int T_PLOT;					 
double line_scale;					 
int UU_debmask;
extern int WNT_Plot;
#endif 
ud_yesno(){}
pick_select_verify(){}
getifl(){}
ud_killmsg(){}
ud_halfrect(){}
uu_uerror2(){}
ud_markover(){}
uj_help(){}
ud_jmpmark(){}
ud_jump(){}
ud_wrerr(){}
ur_save_part(){}
uu_uerror1(){}
uu_uerror0(){}
unauth() {}
ul_reset_control() {}
ur_skip_ent() {}
ud_ddas() {}
ud_prmerr() {}
ncl_on_verify_segno() {}
/*
...
...Added by Paul for 8.201 release. 05/20/93
...
*/
ncl_display_motion() {}
ncl_motion_extrema() {}
mcsmx() {}
nclf_invmx() {}
nclf_transf() {}
getsc() {}

int UD_opengl;
int UV_act_screen;
uv_getvpid() {}
ud_printmsg(msg)
char *msg;
{
#if (UU_COMP != UU_WIN2K)
	printf(msg);
#else
	if (WNT_Plot == 1)
		utp_wnt_msg(0, "Message", msg);
	else
		printf(msg);
#endif
}
int uwx_get_flist() {}

