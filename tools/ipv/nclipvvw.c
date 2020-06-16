/*********************************************************************
**  FILENAME: nclipvw.c
**  CONTAINS:   
*				nclipv_autofact7_redrawvp
**				ul_ipv_view_segs
**				ipv_update_pre_view(view)
**				nclipv_view_from_axis()
**
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        nclipvvw.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**        02/08/16 , 09:17:33
*********************************************************************/
#include <stdio.h>
#include "zsysdep.h"
#include "xenv1.h"
#include "usysg.h"
#include "uims.h"
#include "gobas.h"
#include "gdidd.h"
#include "gobas.h"
#include "ginqatt.h"
#include "gtbl.h"
#include "lcom.h"
#include "mdattr.h"
#include "mpocket.h"
#include "nclfc.h"
#include "lipv.h"
#include "lipvmach.h"
#include "gtbl.h"
#include "wsgl.h"
#include "gloc3.h"
#include "ginq.h"
#include "uhep.h"
#include "gerrorst.h"

extern char *LW_window;
extern int UW_dynamic_funkey;
extern int (*UW_dynfunc_key)();

int IPV_failed = 0;

extern int PKx,PKy;
extern int LW_dyncenter, LW_dyn_zval;
extern UU_REAL LW_dyncp[3];
extern int UV_dynview_active;
extern int UV_Cur_Dyn;
void ul_ipv_view_segs();
void ul_ipv_flush();
void ipv_update_pre_view();
void um_set_screen_area();
void nclipv_real8_to_uureal();

UV_view UV_dynview;
extern int NCL_dyn_ldr;
extern int UZ_nclipv_view;
extern int UV_dyncenter;
/*
.....current view active, defalt to front
*/
extern UW_glmatrix ul_ipv_matrix;
struct UV_pre_view_rec
	{
	UU_REAL	pre_ref_pt[3];
	UU_REAL	pre_pln_norm[3];
	UU_REAL	pre_up_vect[3];
	UU_REAL	pre_eye_dist;
	UU_REAL	pre_aperture;
	UU_REAL	pre_front_clip;
	UU_REAL	pre_back_clip;
	UU_LOGICAL	pre_do_clip;
	};
static struct UV_pre_view_rec pre_view={0,0,0, 0,0,0, 0,0,0, 0,0,0,0,0};
extern Gloc3 UD_nxhair;
UV_screen UV_act_screen[UG_MAXOPWS];

extern int UM_swap_ipv;
UV_view NCLIPV_defvw;

/*********************************/
/******************************/
/******************************/
/******************************/
/******************************/

/***********************************************************************
**  E_FUNCTION:  nclipv_update_view()
**			Update the viewing by the chosen methode.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
nclipv_update_view(ncl_origin,ncl_zaxis,ncl_yaxis)
UM_real8 ncl_origin[3];
UM_real8 ncl_zaxis[3];
UM_real8 ncl_yaxis[3];
{
	static UM_vector norm;				/* normal vector */
	static UM_coord ref_pt;				/* reference point */
	static UM_vector up_vec;			/* up vector */

	UU_REAL magnification;		/* magnification factor */
	UU_REAL wxlen, wylen;
	UV_view view;
	int status;
	UU_REAL old_aperture;
	UU_LOGICAL drw_vp_info;


	uu_denter(UU_MTRC,(us,"nclipv_update_view()"));

	nclipv_real8_to_uureal(3, ncl_origin, ref_pt);
	nclipv_real8_to_uureal(3, ncl_zaxis, norm);
	nclipv_real8_to_uureal(3, ncl_yaxis, up_vec);

	uv_getvid(LW_vport.cur_view, &view);
	ipv_update_pre_view(view);

	old_aperture = view.cur_aperture;
	magnification= um_mag(norm);

	um_unitvc(norm, norm);
	um_unitvc(up_vec, up_vec);

	if (!um_vcparall(norm, up_vec))
	{
		um_cross(up_vec, norm, up_vec);
		um_cross(norm, up_vec, up_vec);
		um_unitvc(up_vec, up_vec);
		uv_update_pln_norm(&view, norm);
		uv_update_up_vect(&view, up_vec);
		uv_update_ref_pt(&view, ref_pt);
		
			/* set the new view boundries */
		wxlen = view.cur_aperture;
			/*wxlen = wxlen / magnification;*/
		uv_update_vaperture(&view, wxlen);
		view.modified = UU_TRUE;
		drw_vp_info = UU_TRUE;
		uv_delete_hidden(&LW_vport);
		nclipv_autofact7_redrawvp(&LW_vport, &view, drw_vp_info);
	}
	uu_dexit;
}
/***********************************************************************
**  E_FUNCTION:  ipv_update_pre_view()
**			Update the viewing by the chosen methode.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void ipv_update_pre_view(view)
UV_view view;
{
	UM_vector vup,vpn;

	um_unitvc(view.cur_pln_norm, vpn);
	um_vctovc(vpn, pre_view.pre_pln_norm);
	um_unitvc(view.cur_up_vect, vup);
	um_vctovc(vup, pre_view.pre_up_vect);
	um_vctovc(view.cur_ref_pt, pre_view.pre_ref_pt);
	pre_view.pre_back_clip = view.cur_back_clip;
	pre_view.pre_front_clip = view.cur_front_clip;
	pre_view.pre_do_clip = view.cur_do_clip;
	pre_view.pre_aperture = view.cur_aperture;
}
/***********************************************************************
**  E_FUNCTION:  nclipv_reset_prev()
**			Update the viewing by the chosen methode.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
nclipv_reset_prev()
{
	UV_view view;
	UM_vector vup,vpn;
	UU_REAL	tmp_ref_pt[3];
	UU_REAL	tmp_pln_norm[3];
	UU_REAL	tmp_up_vect[3];
	UU_REAL	tmp_aperture;
	UU_REAL	tmp_front_clip;
	UU_REAL	tmp_back_clip;
	UU_LOGICAL	tmp_do_clip;
	UU_LOGICAL drw_vp_info;

	if (pre_view.pre_aperture == 0) goto done;

	uv_getvid(LW_vport.cur_view, &view);

	um_unitvc(view.cur_pln_norm, vpn);
	um_vctovc(vpn, tmp_pln_norm);
	um_unitvc(view.cur_up_vect, vup);
	um_vctovc(vup, tmp_up_vect);
	um_vctovc(view.cur_ref_pt, tmp_ref_pt);
	tmp_back_clip = view.cur_back_clip;
	tmp_front_clip = view.cur_front_clip;
	tmp_do_clip = view.cur_do_clip;
	tmp_aperture = view.cur_aperture;

	um_unitvc(pre_view.pre_pln_norm, vpn);
	um_vctovc(vpn, view.cur_pln_norm);	
	um_unitvc(pre_view.pre_up_vect, vup);
	um_vctovc(vup, view.cur_up_vect);
	um_vctovc(pre_view.pre_ref_pt, view.cur_ref_pt);
	view.cur_aperture = pre_view.pre_aperture;
	view.cur_front_clip = pre_view.pre_front_clip;
	view.cur_back_clip = pre_view.pre_back_clip;
	view.cur_do_clip = pre_view.pre_do_clip;

	um_vctovc(tmp_pln_norm, pre_view.pre_pln_norm);
	um_vctovc(tmp_up_vect, pre_view.pre_up_vect);
	um_vctovc(tmp_ref_pt, pre_view.pre_ref_pt);
	pre_view.pre_back_clip = tmp_back_clip;
	pre_view.pre_front_clip = tmp_front_clip;
	pre_view.pre_do_clip = tmp_do_clip;
	pre_view.pre_aperture = tmp_aperture;

	view.modified = UU_TRUE;
	drw_vp_info = UU_TRUE;
	uv_delete_hidden(&LW_vport);
	nclipv_autofact7_redrawvp(&LW_vport, &view, drw_vp_info);
done:;
}
/**************************************************************************
**  E_FUNCTION:  nclipv_vpzoomextrema()
**			Using the bounding boxes of the current segments for the
**			lower left and upper right of the viewport vfit
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
nclipv_vpzoomextrema()
{
	UU_REAL midpt[3];
	UU_REAL del_x,del_y,del_z;
	UU_LOGICAL ok;
	UU_REAL aspect_ratio;					/* aspect ratio of current viewport */
	UU_REAL wylen;								/* y length of view port */
	UU_REAL aperture;							/* x length of view */
	UU_REAL urbvs[3], llfvs[3];
	UU_REAL urb[3], llf[3],scale;
	UU_LOGICAL um_cceqcc(), cent_modified;
	Gnrect *gr_area;
	Gwrect3 box;
	int curr_scrn;
	UV_view view;

	uu_denter(UU_MTRC,(us,"nclipv_vpzoomextrema()"));

	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	uv_getvid(LW_vport.cur_view, &view);
	ipv_update_pre_view(view);

	scale = 1.;
	ok = ul_ipv_view_extrema(&LW_vport,&box,&scale);

	del_x = fabs (box.llf.x - box.urb.x);
	del_y = fabs (box.llf.y - box.urb.y);
	del_z = fabs (box.llf.z - box.urb.z);

	if( ok )
	{
		gsnormtran(LW_vport.xform);
		uv_extrema_project(&box.llf,&box.urb,&view);
		uvu_vcstomcs(&view, 0, &box.urb, urb);
		uvu_vcstomcs(&view, 0, &box.llf, llf);
		midpt[0] = (urb[0] + llf[0]) / 2.0;
		midpt[1] = (urb[1] + llf[1]) / 2.0;
		midpt[2] = (urb[2] + llf[2]) / 2.0;
		if(um_cceqcc(midpt, view.cur_ref_pt))
		{
			cent_modified = UU_FALSE;
		}
		else
		{
			cent_modified = UU_TRUE;
		}
			/*  if not single point get the current aspect ratio */
		if( (del_x > .042) || (del_y > .042 ) )
		{
			aspect_ratio = ((LW_vport.urb[1] - LW_vport.llf[1]) *
							(gr_area->ur.y - gr_area->ll.y))
						 / ((LW_vport.urb[0] - LW_vport.llf[0]) *
						(gr_area->ur.x - gr_area->ll.x));
		/* get the new aspect ratio */
			um_vctovc(&box.llf, llfvs);
			um_vctovc(&box.urb, urbvs);

			aperture = fabs(urbvs[0] - llfvs[0]);

			wylen = fabs(urbvs[1] - llfvs[1])/aspect_ratio;
			if(wylen > aperture ) aperture = wylen;

			aperture = aperture * scale * 1.05; /* allow 2.5% border */
			view.modified = (fabs(aperture - view.cur_aperture) > UM_FUZZ);
			view.modified = UU_TRUE;
		}
		else
		{
			view.modified = UU_TRUE;
			aperture = 0.0;
		}
	
		if (view.modified || cent_modified) 
		{
			view.modified = UU_TRUE;
			um_vctovc(midpt, view.cur_ref_pt);
			if (aperture > UM_FUZZ)
				view.cur_aperture = aperture;
			uv_delete_hidden(&LW_vport);
			nclipv_autofact7_redrawvp(&LW_vport, &view, UU_TRUE);
		}
	}
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : int nclipv_autofact7_redrawvp(vport, view,
**										drw_vp_info)
**       Update the VIEW in UNIBASE and the viewport (VPORT) in
**			DIGS.
**    PARAMETERS   
**       INPUT  : 
**          vport							viewport entity
**          view							view entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclipv_autofact7_redrawvp(vport, view, drw_vp_info)
	UV_vport *vport;
	UV_view *view;
	UU_LOGICAL drw_vp_info;

{
	uu_denter(UU_MTRC,(us,"nclipv_autofact7_redrawvp(vpkey=%x,vkey=%x,drw_vp=%d)",
		vport->key, view->key, drw_vp_info));

	um_set_screen_area(UM_IPV_WINDOW);
	uv_putv(view);
	uv_setxform(vport);
	ul_ipv_view_same(vport->xform);
	uu_dexit;
	return (UU_SUCCESS);
}

/**************************************************************************
**  E_FUNCTION:  nclipv_pan()
**			pan the view
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void nclipv_pan()
{
	UV_view view;							/* view structure */
	int numint;
	UD_NDCLOCREC ndcloc;
	UU_REAL loc[3],norm[3];
	int button,event;

	uu_denter( UU_MTRC,(us,"nclipv_pan()"));

	event = ul_ipv_locate("Indicate lower left corner",2,loc,norm,&button);
	if (button == 1) numint = 1;
	else numint = 0;
	if (numint > 0)
	{
		uv_getvid(LW_vport.cur_view, &view);
		ipv_update_pre_view(view);
		uv_update_ref_pt(&view, loc);
		view.modified = UU_TRUE;
		uv_delete_hidden(&LW_vport);
		nclipv_autofact7_redrawvp(&LW_vport, &view, UU_FALSE);
	}
	nclipv_repaint();
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclipv_repaint()
**       Repaint NCLIPV view.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclipv_repaint()
{
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_repaint_viewport()"));

	uv_getvid(LW_vport.cur_view, &view);
	view.modified = UU_TRUE;
	uv_delete_hidden(&LW_vport);
	nclipv_autofact7_redrawvp(&LW_vport, &view, UU_TRUE);
	uu_dexit;
	return 0;
}
/*
.....not really implement since we are not use space mouse now
*/
/*
void uv_dynstep_disp()
{
	if (UV_dynview_active == UU_TRUE) return;
	if (UV_Cur_Dyn==2)
	{
		ul_ipv_view_same(LW_vport.xform);
	}
}
*/
/******************************/
/******************************/
/******************************/
/******************************/
/***********************************************************************
**  E_FUNCTION:  nclipv_tool_view()
**     Change the viewing parameters to the current tool.
**  PARAMETERS
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void nclipv_tool_view()
{
	UM_int2 option, status;
	UM_real8 ncl_origin[3];
	UM_real8 ncl_zaxis[3];
	UM_real8 ncl_yaxis[3];
	UM_real8 ncl_fwd[3];
	UU_REAL um_mag();

	um_vctovc(LW_tool_pos,ncl_origin);
	um_vctovc(&LW_tool_pos[3],ncl_zaxis);
	um_vcmnvc(LW_tool_pos,LW_last_tool_pos,ncl_fwd);
	um_unitvc(ncl_fwd,ncl_fwd);
	um_cross(ncl_zaxis, ncl_fwd, ncl_yaxis);
	if (um_mag(ncl_yaxis) < UM_FUZZ)
	{
		um_perpvc(ncl_zaxis, ncl_yaxis);
	}
	um_unitvc(ncl_yaxis, ncl_yaxis);
	nclipv_update_view(ncl_origin, ncl_zaxis, ncl_yaxis);
}
/*********************************************************************
**    E_FUNCTION     : nclipv_uureal_to_real8(n, inbuf, outbuf)
**       Move N UU_REALs to UM_real8
**    PARAMETERS   
**       INPUT  : 
**          n                 number of values
**          inbuf             array of UU_REAL values
**       OUTPUT :  
**          outbuf            array of UM_real8 values
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclipv_uureal_to_real8(n, inbuf, outbuf)
   int n;
   UU_REAL inbuf[];
   UM_real8 outbuf[];
{
   int i;
   for (i=0; i<n; i++) outbuf[i] = inbuf[i];
}

/*********************************************************************
**    E_FUNCTION     : nclipv_real8_to_uureal(n, inbuf, outbuf)
**       Move N UM_real8 to UU_REAL
**    PARAMETERS   
**       INPUT  : 
**          n                 number of values
**          inbuf             array of UM_real8 values
**       OUTPUT :  
**          outbuf            array of UU_REAL values
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclipv_real8_to_uureal(n, inbuf, outbuf)
   int n;
   UM_real8 inbuf[];
   UU_REAL outbuf[];
{
   int i;
   for (i=0; i<n; i++) outbuf[i] = inbuf[i];
}

/***********************************************************************
**  E_FUNCTION:  nclipv_get_mx_view()
**			Change the viewing parameters by a predefined matrix.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void nclipv_get_mx_view()
{
	UM_real8 ncl_origin[3];
	UM_real8 ncl_zaxis[3];
	UM_real8 ncl_yaxis[3];
	UU_LOGICAL drw_vp_info, loop_par;
	UU_REAL magnification;		/* magnification factor */
	int status,isav;
    char mx_name[16];

	uu_denter(UU_MTRC,(us,"nclu_get_mx_view()"));
/*
.....not support for now since it need NCL key info
*/
	return;
loop:;
	magnification = 1.;
	loop_par = UU_FALSE;
	status = nclipv_get_str(mx_name, 416);
	gtmx(mx_name,ncl_origin,ncl_zaxis,ncl_yaxis,&loop_par);
	if (loop_par) goto loop;

	nclipv_update_view(ncl_origin, ncl_zaxis, ncl_yaxis);

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : int nclipv_get_str(str, prompt)
**       Prompt the user to enter a text string. 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
nclipv_get_str(str, prompt)
char str[];
int prompt;

{
	int status;
	int ret_status;
	int numint;
	UD_STRREC strrec;

	uu_denter(UU_MTRC,(us,"ncl_get_str(str=%x, prompt=%d)",
		*str, prompt));

	str[0] = '\0';
	strrec.instring = str;
////////////////need to do
	status = ud_ldas(UD_DASSTRINGDEF, UA_NCL, prompt, &strrec, 80,
				&numint, UD_NODEFAULT);

	ret_status = 0;
	if (numint == 0) ret_status = 1;

	uu_dexit;
	return (ret_status);
}

/***********************************************************************
**  E_FUNCTION:  nclipv_refsys_view()
**			Change the viewing parameters by the current refsys 
**			matrix.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
nclipv_refsys_view()
{
	UM_int2 option, status;
	UM_real8 ncl_origin[3];
	UM_real8 ncl_zaxis[3];
	UM_real8 ncl_yaxis[3];
	UU_REAL magnification;		/* magnification factor */

	uu_denter(UU_MTRC,(us,"nclu_refsys_view()"));

	magnification = 1.;
	option = 1;
	gtview(ncl_origin, ncl_zaxis, ncl_yaxis, &option, &status);
	if (status == 0)
		nclipv_update_view(ncl_origin, ncl_zaxis, ncl_yaxis);
	else
	   uu_uerror0(UA_NCL, 16);	
	uu_dexit;
	}

/***********************************************************************
**  E_FUNCTION:  nclipv_tracut_view()
**			Change the viewing parameters by the current tracut 
**			matrix.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
nclipv_tracut_view()

	{
	UM_int2 option, status;
	UM_real8 ncl_origin[3];
	UM_real8 ncl_zaxis[3];
	UM_real8 ncl_yaxis[3];
	UU_REAL magnification;		/* magnification factor */

	uu_denter(UU_MTRC,(us,"nclu_tracut_view()"));

	magnification = 1.;
	option=2;
	gtview(ncl_origin, ncl_zaxis, ncl_yaxis, &option, &status);
	if (status == 0)
		nclipv_update_view(ncl_origin, ncl_zaxis, ncl_yaxis);
	else
	   uu_uerror0(UA_NCL, 15);	
	uu_dexit;
}

/**************************************************************************
**  E_FUNCTION:  nclipv_vpzoomout()
**			Prompt the user for a viewport and a magnification factor for the
**			view.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void nclipv_vpzoomout()
{
	UU_REAL magnification;							/* magnification factor */
	int numint;
	UV_view view;										/* view */
	UU_REAL aperture;									/* x length of view */
	int isav;

	uu_denter( UU_MTRC,(us,"uvu_zoomout()"));

	ud_ldas(UD_DASUNITLESS,/*enter magnification factor */
					UM_MODEL,85,&magnification, 1,&numint,UD_NODEFAULT);
	if (numint > 0)
	{
		if (magnification < UM_FUZZ)
			uu_uerror0(/*magnification factor must be greater than zero*/
				UM_MODEL,117);
		else
		{
			uv_getvid(LW_vport.cur_view, &view);
			ipv_update_pre_view(view);

			aperture = view.cur_aperture / magnification;
			uv_update_vaperture(&view, aperture);
			view.modified = UU_TRUE;
			uv_delete_hidden(&LW_vport);
			nclipv_autofact7_redrawvp(&LW_vport, &view, UU_TRUE);
		}
	}
	uu_dexit;
}

/**************************************************************************
**  E_FUNCTION:  nclipv_vpzoomin()
**			Window zoom view.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void nclipv_vpzoomin()
{
	UU_REAL llf[3];
	UU_REAL urb[3];
	UD_NDCLOCREC ndcll;
	UD_NDCLOCREC ndcur;
	UU_REAL midpt[3];
	UV_view view;
	int numint;
	int i;
	UU_REAL aspect_ratio;					/* aspect ratio of current viewport */
	UU_REAL wxlen;								/* x length of view port */
	UU_REAL wylen;								/* y length of view port */
	UU_REAL aperture;							/* x length of view */
	UD_RUBBER rubber;							/* rubber box control block */
	int curr_scrn;
	Gnrect *gr_area;
	int button,event;
	UM_vector norm;
	int ifl;
	UM_transf tf;

	uu_denter(UU_MTRC,(us,"uvu_vpzoomin()"));
/*
.....Get MODSYS matrix if NCLIPV is active
*/
	ncl_mcstf(&ifl,tf);

	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);
	RUBBER_BOX(&rubber);
	event = ul_ipv_locate("Indicate lower left corner",2,llf,norm,&button);
	if (button == 1)
	{
		numint = 1;
		if (ifl) um_cctmtf(llf,tf,llf);
	}
	else numint = 0;
	if (numint != 0)
	{
		RUBBER_ON(&rubber);
		event = ul_ipv_locate("Indicate upper right corner",5,urb,norm,&button);
		if (button == 1)
		{
			numint = 1;
			if (ifl) um_cctmtf(urb,tf,urb);
		}
		else numint = 0;
		if (numint != 0)
		{
			uv_getvid(LW_vport.cur_view, &view);
			ipv_update_pre_view(view);

			um_ploctocc(&ndcll, llf);
			um_ploctocc(&ndcur, urb);

			for (i = 0; i < 3; i++) midpt[i] = (urb[i] + llf[i]) / 2.;
				uv_update_ref_pt(&view, midpt);
			um_vctovc(midpt, view.cur_ref_pt);

				/* get the current aspect ratio */

			aspect_ratio = ((LW_vport.urb[1] - LW_vport.llf[1]) *
									(gr_area->ur.y - gr_area->ll.y))
								 / ((LW_vport.urb[0] - LW_vport.llf[0]) *
									(gr_area->ur.x - gr_area->ll.x));

				/* get the new aspect ratio */
			wxlen = fabs(urb[0] - llf[0]) ;
			wylen = fabs(urb[1] - llf[1]) ;
			if (aspect_ratio > 0.0) wylen /= aspect_ratio;
			aperture = (wxlen < wylen)? wylen: wxlen;
			aperture *= 1.1;

			uv_update_vaperture(&view, aperture);

			view.modified = UU_TRUE;
			uv_delete_hidden(&LW_vport);
			nclipv_autofact7_redrawvp(&LW_vport, &view, UU_TRUE);
		}
		RUBBER_OFF(&rubber);
	}
	uu_dexit;
}

/***********************************************************************
**  E_FUNCTION:  nclipv_change_view()
**			Allow the user to change the viewing parameters defined in
**			the change view form (reference point, normal, up vector).
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void nclipv_change_view()
{
	UV_view view;
	UU_REAL old_aperture;
	UU_LOGICAL drw_vp_info;

	uu_denter(UU_MTRC,(us,"uv_change_view()"));

	uv_getvid(LW_vport.cur_view, &view);
	ipv_update_pre_view(view);

	old_aperture = view.cur_aperture;
	if(uvu_change_view_form(&view) == UU_SUCCESS)
	{
		view.modified = UU_TRUE;
		drw_vp_info = UU_TRUE;
		uv_delete_hidden(&LW_vport);
		nclipv_autofact7_redrawvp(&LW_vport, &view, drw_vp_info);
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  nclipv_view_from_axis(axis)
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          axis   = 0 - POSX, 1 - POSY, 2 - POSZ, 3 - USER,
**				         4 - NEGX, 5 = NEGY, 5 = NEGZ, 6 - FLIP
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void nclipv_view_from_axis(axis)
int axis;
{
	UM_vector norm;
	UM_vector up;
	UV_view view;
	int numint;

	uv_getvid(LW_vport.cur_view, &view);
	ipv_update_pre_view(view);

	if (axis == 0)
	{
		um_vctovc(UM_xaxis, norm);
		um_vctovc(UM_zaxis, up);
	}
	else if (axis == 1)
	{
		um_vctovc(UM_zaxis, up);
		um_vctovc(UM_yaxis, norm);
	}
	else if (axis == 2)
	{
		um_vctovc(UM_yaxis, up);
		um_vctovc(UM_zaxis, norm);
	}
	else if (axis == 4)
	{
		um_vctmsc(UM_xaxis, (UU_REAL) -1.0, norm);
		um_vctovc(UM_zaxis, up);
	}
	else if (axis == 5)
	{
		um_vctmsc(UM_yaxis, (UU_REAL) -1.0, norm);
		um_vctovc(UM_zaxis, up);
	}
	else if (axis == 6)
	{
		um_vctmsc(UM_zaxis, (UU_REAL) -1.0, norm);
		um_vctovc(UM_yaxis, up);
	}
	else if (axis == 7)
	{
		um_vctmsc(view.cur_pln_norm, (UU_REAL) -1.0, norm);
		um_vctovc(view.cur_up_vect, up);
	}
	uv_update_up_vect(&view, up);
	uv_update_pln_norm(&view, norm);
	view.modified = UU_TRUE;
	uv_delete_hidden(&LW_vport);
	uv_autofact7_redrawvp(&LW_vport, &view, UU_FALSE);
	nclipv_repaint();
}
void um_set_screen_area(type)
UM_pkwin_type type;
{
	int ix, iy;
	uw_ntget_ipvsize(&ix,&iy);
	udm_resize_graphics(&UD_duimsdeflt,ix, iy,0);
}
/********************************************************************* 
**  E_FUNCTION:        uv_dynmouse(vport)
**      Change the view plane normal.
**  PARAMETERS   
**      INPUT:    
**				vport						viewport entity
**      OUTPUT:  
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void nclipv_dynmouse2(vport)
	UV_vport	*vport;

{
	Glocrec locrec ;
	Gdrect locwin;
	Gqloc *greqloc();
	static char panprompt[] = "Dynamic viewing in effect";
	Gerror rtn;
	UV_view	view;
	Gwpoint3 pt;
	Gwpoint3 *vup;
	UM_coord csav;
	Gwrect3 *window;
	FILE *fd;
	int status,nc;
	char inbuffer[500], tmpstr[200], *tok;
	float t1, t2, t3, t4, t5, t6, t7, t8, t9;
	Gwrect3 wswin;
/*
.....during dynamic rotaion attach the leader line to the left corner of the
.....label box so that the leader lines always appear attached to the labels
*/
	NCL_dyn_ldr =1;
	ncl_display_ldr(vport);
	gsnormtran(vport->xform);

	locrec.prompt = panprompt;				/* locator data record */
	vport = vport;
	if (UZ_nclipv_view == 1)
	{
		csav[0] = LW_dyncp[0];
		csav[1] = LW_dyncp[1];
		csav[2] = LW_dyncp[2];
	}
	gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO) ;
	uv_locwin(&locwin);
	rtn=ginitloc(UD_ksws, 1, &UD_nxhair, 46, &locwin, &locrec);
	if (rtn==NCL_NO_ERROR)
	{
		
		greqloc(UD_ksws, 1);
		NCL_dyn_ldr =0;

		/* set the view plane normal */
		givpn3(vport->xform, &pt);
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		ipv_update_pre_view(view);
/*
.....Set the view normal
*/
		view.cur_pln_norm[0] = pt.x;
		view.cur_pln_norm[1] = pt.y;
		view.cur_pln_norm[2] = pt.z;
/*
.....Set the view up vector
*/
		vup = gqvup3(vport->xform);
		view.cur_up_vect[0] = (*vup).x;
		view.cur_up_vect[1] = (*vup).y;
		view.cur_up_vect[2] = (*vup).z;
/*
.....Set the aperture
*/
		window = gqwindow3(vport->xform);
		view.cur_aperture = (window->urb.x - window->llf.x);
/*
.....Update the view reference point
*/
		if (UZ_nclipv_view == 1)
		{
			view.cur_ref_pt[0] = LW_dyncp[0];
			view.cur_ref_pt[1] = LW_dyncp[1];
			view.cur_ref_pt[2] = LW_dyncp[2];
		}
		uv_putv(&view);
		if (UZ_nclipv_view == 1)
		{
			um_set_screen_area(UM_IPV_WINDOW);
			uv_setxform(vport);
			ul_ipv_view_same(vport->xform);
		}
		gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO) ;
		if (UZ_nclipv_view == 1)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
	}
done:;
}

/********************************************************************* 
**  E_FUNCTION:        nclipv_dynmouse
**      Invoke dynamic mouse viewing on the terminal
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void nclipv_dynmouse()

{
	char name[40];
started:;
	uv_getvid(LW_vport.cur_view,&UV_dynview);
	if (UZ_nclipv_view)
	{
		if (LW_dyncenter == 0)
		{
				LW_dyncp[0] = UV_dynview.cur_ref_pt[0];
				LW_dyncp[1] = UV_dynview.cur_ref_pt[1];
				LW_dyncp[2] = UV_dynview.cur_ref_pt[2];
		}
		uv_delete_hidden(&LW_vport);
		nclipv_dynmouse2(&LW_vport);
	}
	if (UW_dynamic_funkey!=-1)
	{
		(*(UW_dynfunc_key)) (UW_dynamic_funkey, &name,1);
		UW_dynamic_funkey = -1;
		goto started;
	}
}

/*********************************************************************
**    E_FUNCTION     : int uv_autofact7_redrawvp(vport, view,
**										drw_vp_info)
**       Update the VIEW in UNIBASE and the viewport (VPORT) in
**			DIGS.
**    PARAMETERS   
**       INPUT  : 
**          vport							viewport entity
**          view							view entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uv_autofact7_redrawvp(vport, view, drw_vp_info)
	UV_vport *vport;
	UV_view *view;
	UU_LOGICAL drw_vp_info;

{

	uu_denter(UU_MTRC,(us,"uv_autofact7_redrawvp(vpkey=%x,vkey=%x,drw_vp=%d)",
		vport->key, view->key, drw_vp_info));
/*
.....Redraw IPV viewport
*/
	if (UZ_nclipv_view == 1)
	{
		um_set_screen_area(UM_IPV_WINDOW);
		uv_putv(view);
		uv_setxform(vport);
		ul_ipv_view_same(vport->xform);
	}
	uu_dexit;
	return (UU_SUCCESS);
}

void uv_nclipv_screen(view)
UU_KEY_ID view;
{
	UU_KEY_ID vport1 ;
	UU_KEY_ID screen1;
	UM_ndc lleft;
	UM_ndc uright;
	UV_screen screen;

	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, lleft);
	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 0.0, uright);

	screen1 = uv_scdefine("nclipv", &screen, 0);
/*
.....Define the nclipv viewport
*/
	vport1 = uv_vpdefine("nclipv", &LW_vport, 0);
	if  (vport1==-1)
		return;
	uv_setvpbox(&LW_vport, lleft, uright);
	uv_vtovp(&LW_vport, view);
	uv_putvp(&LW_vport);
	uv_vptosc(&screen, vport1);
}

uv_dd1init()
{
	UU_KEY_ID nclipv;
	UV_screen screen;
	UM_ndc lleft, vpurb;

	uv_nclipv_view(&nclipv);
/*
......save default view to be reset
*/
	uv_getvid(nclipv, &NCLIPV_defvw);
	uv_nclipv_screen(nclipv);
	uv_getscnm("nclipv", &screen);
	uv_activsc(&screen);
}

void uv_getdef_ipvvw(view)
UV_view *view;
{
	uv_update_ref_pt(view, NCLIPV_defvw.cur_ref_pt);
	uv_update_pln_norm(view, NCLIPV_defvw.cur_pln_norm);
	uv_update_up_vect(view, NCLIPV_defvw.cur_up_vect);
	uv_update_vaperture(view, NCLIPV_defvw.cur_aperture);
	uv_update_fbclip(view, NCLIPV_defvw.cur_front_clip, NCLIPV_defvw.cur_back_clip);
	uv_update_clip(view, NCLIPV_defvw.cur_do_clip);
}          

/**************************************************************************
**  E_FUNCTION:  uvu_restorev(option)
**      Restore a view corresponding to a currently displayed view port
**  PARAMETERS   
**      INPUT  :  option			option = 0 ->	restore a view corresponding
**																to a picked view port
**											option = 1 ->	restore all views corresponding
**																to the view ports on the 
**																current screen
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uvu_restorev(option)
int	option;
{
	UV_vport vport;						/* vport containing view to be restored */
	UV_view view;							/* view to be restored */
	int i, j;

	uu_denter(UU_MTRC,(us,"uvu_restorev(option = %d)",option));

	if (option == 0)
	{
		if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
			uv_getvid(vport.cur_view, &view);
			/* Save the view parameters before changing. kathy */
			ipv_update_pre_view(view);

			uv_update_ref_pt(&view, view.sav_ref_pt);
			uv_update_pln_norm(&view, view.sav_pln_norm);
			uv_update_up_vect(&view, view.sav_up_vect);
			uv_update_vaperture(&view, view.sav_aperture);
			uv_update_fbclip(&view, view.sav_front_clip, view.sav_back_clip);
			uv_update_clip(&view, view.sav_do_clip);
			uv_delete_hidden(&vport);
			uv_autofact7_redrawvp(&vport, &view, UU_TRUE);
		}
	}
	uu_dexit;
	return 0;
}
