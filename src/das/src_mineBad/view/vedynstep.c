#include "usysdef.h"

/********************************************************************* 
**  NAME:  vedynstep.c
**
**      Immediate mode dynamic viewing section.
**
**		CONTAINS:
**			uv_select_sview
**			uv_get_sview
**			uv_dynstep_view
**			uv_dynstep_pan
**			uv_dynstep_rot
**			uv_dynstep_zoom
**			uv_dynstep_redraw
**			uv_dynstep_disp
**			uv_get_auto_center
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       vedynstep.c , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**       05/01/17 , 13:16:06
**
*********************************************************************/
 
#include "udebug.h"
#include "driver.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "ginqxf.h"
#include "gsegac.h"
#include "gmat4.h"
#include "mdcoord.h"
#include "math.h"
#include "view.h"
#include "lipv.h"
#include "spmouse.h"
#include "lcom.h"
/*
#if UU_COMP == UU_IRIS4D
#include "wsi4d.h"
#endif
*/

int UV_dynview_active=UU_FALSE;
int UV_current_sview=1;

int Space_mouse_draw = 0;
#ifndef PI
#define PI (3.1415926454)
#endif
#define DTOR (PI/180.0)
#define RTOD (180.0/PI)
#define PANX 1
#define PANY 2
#define XROT 3
#define YROT 4
#define ZROT 5
#define ZOOM 6

void uv_dynstep_disp();
void uv_dynstep_redraw();
void uv_get_auto_center();
static void S_get_dyn_center();

extern int UV_dyndisply, UV_dyncenter, UV_dynstatln, UV_dyn_zval;
extern UU_REAL UV_dyncp[3],UV_dynvec[3],LW_dyncp[3];
extern UV_view UV_dynview;
extern int UZ_nclipv_view;
extern int LW_dyncenter, LW_dyn_zval;
extern int NCL_mouse_func;
/*********************************************************************
**    I_FUNCTION     :  uv_select_sview(dir)
**
**		Selects the active view to use for immediate dynamic viewing
**		routines, when multiple viewports are on the screen.
**
**    PARAMETERS   
**       INPUT  : 
**          dir    = -1 = Select the previous view.
**			         0  = Select the current view (used just to make
**			              sure that the current view is within the
**			              range of views currently on the screen).
**			         1  = Select the next view.
**       OUTPUT :  
**          Sets the UV_current_sview variable to the selected view.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_select_sview(dir)
int dir;
{
	int isav;
	UV_vport vport;
/*
.....if it is standalone NCLIPV, LW_display_prop.swap is always false
*/
	if (LW_nclipv == LW_STANDALONE)
		return;
/*
.....Toggle the active view
*/
	isav = UV_current_sview;
	UV_current_sview = UV_current_sview + dir;
	if (UV_current_sview > UV_act_screen[0].nvports) UV_current_sview = 1;
	if (UV_current_sview < 1) UV_current_sview = UV_act_screen[0].nvports;
/*
.....If we changed the active viewport
.....Then let's show it
*/
	if (UV_current_sview != isav && dir != 0)
	{
		uv_getvpid(UV_act_screen[0].vports[isav-1],&vport);
		uv_updatevp(&vport,UU_TRUE);
		uv_getvpid(UV_act_screen[0].vports[UV_current_sview-1],&vport);
		uv_drawvp(&vport);
	}
}

/*********************************************************************
**    I_FUNCTION     :  uv_get_sview(vport,view)
**
**		Returns the viewport and view structures of the currently active view.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**          vport   = The currently active viewport.
**          view    = The active view in 'vport'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_get_sview(vport,view)
UV_vport *vport;
UV_view *view;
{
/*
.....Get the active Viewport
.....and View
*/
	uv_getvid(UV_act_vports[0][UV_current_sview-1].view,view);
	uv_getvpid(UV_act_screen[0].vports[UV_current_sview-1],vport);
}

/*********************************************************************
**    I_FUNCTION     :  uv_dynstep_view(xf,vflag,nrot)
**
**		Dynamically update the requested normalization transformation,
**		and redraw the requested viewport.
**
**    PARAMETERS   
**       INPUT  : 
**       xf    Normalization xform to change.
**			vflag	Type of transformation to apply.  1 = Pan X,
**					2 = Pan Y, 3 = Rotate X, 4 = Rotate Y, 5 = Rotate Z,
**					6 = Zoom.
**			nrot	A positive value moves 'nrot' steps in the positive
**					direction.  A negative value moves in the minus
**					direction.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_dynstep_view(xf,vflag,nrot)
int xf,vflag,nrot;
{
	UU_LOGICAL ipv,dyncp;
	UM_coord cen;
	UV_view view;
	UV_vport vport;
	int stat,n,sav_dyncenter;
/*
.....Dynamic viewing only valid
.....for SGI GL and X-Windows
*/
	if (UV_dynview_active == UU_TRUE) goto done;
/*
.....Get active view
........Spacemouse for IPV
*/
/*	if (((UV_SM_NCL==0)&&(UV_Cur_Dyn==2))||(LW_nclipv == LW_STANDALONE)) */
	if (((UV_SM_NCL==0)&&(UV_Cur_Dyn==2))||(LW_nclipv == LW_STANDALONE)
		|| (UZ_nclipv_view == 1))
	{
		if (ul_ipv_view_active())
		{
			if (uv_getvnm("Nclipv",&view) != UU_SUCCESS) goto done;
			if (uv_getvpid(LW_vport.key, &vport) != UU_SUCCESS) goto done;
			n = LW_vport.xform;
			ipv = UU_TRUE;
/*
......need set the context etc. to NCLIPV window
*/
			um_set_pocket_graphics(UM_IPV_WINDOW);
		}
		else
			return;
	}
/*
........NCL view
*/
	else
	{
/*
......we will use "AUTO rotation center for all STEP view
*/
/*
......changed to respect the dynamic center user selected
......so only if live mouse, we use auto center
*/
		sav_dyncenter = UV_dyncenter;
		if (NCL_mouse_func)
			UV_dyncenter = 2;
		if (NCL_mouse_func)
		{
/*
.....if mouse function, select the view mouse cursor is at
*/
			uv_get_vport_atcursor(&vport);
		}
		else
		{
			uv_getvpid(UV_act_screen[0].vports[xf-1],&vport);
		}
		gsnormtran(vport.xform);
		uv_getvid(vport.cur_view,&view);
		n = vport.xform;
		ipv = UU_FALSE;
	}
/*
.....Get the center of rotation
*/
	S_get_dyn_center(ipv,n, &view,cen,&dyncp);
/*
.....Call appropriate routine
*/
	stat = 0;
	switch (vflag)
	{
	case PANX:
	case PANY:
		stat = uv_dynstep_pan(n,vflag,nrot,&view); break;
	case XROT:
	case YROT:
	case ZROT:
		stat = uv_dynstep_rot(n,vflag,nrot,cen,dyncp,&view); break;
	case ZOOM:
		stat = uv_dynstep_zoom(n,nrot,cen,dyncp,&view); break;
	}
	if (stat!=0)
		goto done;
	uv_putv(&view);

	if (LW_nclipv != LW_STANDALONE)
		uv_updatevp(&vport, UU_TRUE);

	if ((UV_SM_NCL==0)&&(UV_Cur_Dyn==2)||(LW_nclipv == LW_STANDALONE))
		uv_setxform(&vport);

	if (Space_mouse_draw!=1)
	{
/*
.....Redraw the updated viewport
*/
		if (ipv)
			ul_ipv_view_same(vport.xform);
		else
			uv_dynstep_redraw(n);
/*
.....reset redraw flag, otherwise, it will redraw
.....in mainloop
.....Yurong 1/2/99
*/
		ug_setredrwflag(0);   
	}
done:;
	if (ipv==UU_FALSE)
		UV_dyncenter = sav_dyncenter;
}

/*********************************************************************
**    I_FUNCTION     :  uv_dynstep_pan(n,vflag,nrot,view)
**
**		Performs the PANX and PANY dynamic viewing functions.
**
**    PARAMETERS   
**       INPUT  : 
**          n		Normalization xform to change.
**			vflag	Type of transformation to apply.  1 = Pan X,
**					2 = Pan Y.
**			nrot	A positive value moves 'nrot' steps in the positive
**					direction.  A negative value moves in the minus
**					direction.
**			view	Active view.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uv_dynstep_pan(n,vflag,nrot,view)
int n,vflag,nrot;
UV_view *view;
{
	int inc;
	UM_coord xaxis;
	Gfloat sensitivity,delx;
	Gwrect3 wswin;

/*
.....Calculate sensitivity based on
.....type of dynamic viewing
*/
	if (UV_Cur_Dyn==2)
		sensitivity = UV_SM_pangain;
	else if (UV_Cur_Dyn==1)
		sensitivity = UV_MS_pangain;
	else
		sensitivity = UV_KB_pangain;

#ifdef UU_RS6000
	sensitivity = (ug_gksstli.vtran[n].window.urb.x -
		ug_gksstli.vtran[n].window.llf.x) * sensitivity * 2;
#else
	sensitivity = (ug_gksstli.vtran[n].window.urb.x -
		ug_gksstli.vtran[n].window.llf.x) * sensitivity;
#endif
/*
....Find the change in angle
*/
	delx = nrot * sensitivity;
#if UU_COMP==UU_WIN2K
	if (UV_Cur_Dyn == 2)
	{
		inc = UV_actsm_dial - 1;
 		if (UV_SM_data[inc] != 0)
			delx = abs(UV_SM_data[inc])*UV_SM_sensi*UV_SM_pangain*nrot *
				(ug_gksstli.vtran[n].window.urb.x-ug_gksstli.vtran[n].window.llf.x);
		else
			return -1;
		if ((vflag==2)&&((UV_actsm_dial==1)||(UV_actsm_dial==3)))
			delx = -delx;
		if ((vflag==1)&&(UV_actsm_dial==4))
			delx = -delx;
	}
#endif

	switch (vflag)
	{
/*
.....Pan in X by
.....Modifing window location
*/
	case PANX:
		wswin.llf.x = ug_gksstli.vtran[n].window.llf.x - delx;
		wswin.llf.y = ug_gksstli.vtran[n].window.llf.y;
		wswin.llf.z = ug_gksstli.vtran[n].window.llf.z;
		wswin.urb.x = ug_gksstli.vtran[n].window.urb.x - delx;
		wswin.urb.y = ug_gksstli.vtran[n].window.urb.y;
		wswin.urb.z = ug_gksstli.vtran[n].window.urb.z;
		um_cross(view->cur_up_vect,view->cur_pln_norm,xaxis);
		um_translate_point(view->cur_ref_pt,-delx,xaxis,view->cur_ref_pt);
		break;
/*
.....Pan in X by
.....Modifing window location
*/
	case PANY:
		wswin.llf.x = ug_gksstli.vtran[n].window.llf.x;
		wswin.llf.y = ug_gksstli.vtran[n].window.llf.y - delx;
		wswin.llf.z = ug_gksstli.vtran[n].window.llf.z;
		wswin.urb.x = ug_gksstli.vtran[n].window.urb.x;
		wswin.urb.y = ug_gksstli.vtran[n].window.urb.y - delx;
		wswin.urb.z = ug_gksstli.vtran[n].window.urb.z;
		um_translate_point(view->cur_ref_pt,-delx,view->cur_up_vect,
			view->cur_ref_pt);
		break;
	}
	if ((UV_Cur_Dyn != 2)&&(LW_nclipv!=LW_STANDALONE)) gswindow3(n,&wswin);
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  uv_dynstep_rot(n,vflag,nrot,cen,dyncp,view)
**
**		Performs the XROT, YROT, and ZROT dynamic viewing functions.
**
**    PARAMETERS   
**       INPUT  : 
**          n      Normalization xform to change.
**          vflag  Type of transformation to apply.  3 = Rotate X,
**                 4 = Rotate Y, 5 = Rotate Z.
**          nrot   A positive value moves 'nrot' steps in the positive
**                 direction.  A negative value moves in the minus
**                 direction.
**          cen    Dynamic center point.
**          dyncp  - UU_TRUE if viewing is around user defined location.
**                   UU_FALSE if viewing is around vport center.
**          view   Active view.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uv_dynstep_rot(n,vflag,nrot,cen,dyncp,view)
int n,vflag,nrot;
UM_coord cen;
UV_view *view;
{
	int i,inc;
	UM_vector vpn,vup,vrt;
	Gfloat vpcen[3];
	Gfloat sensitivity,delx,phix;
	UM_transf tm,tm1,tm2;
/*
.....Get center of viewport in WC
*/
	vpcen[0] = view->cur_ref_pt[0];
	vpcen[1] = view->cur_ref_pt[1];
	vpcen[2] = view->cur_ref_pt[2];
/*
.....Calculate sensitivity based on
.....type of dynamic viewing
*/
	if (UV_Cur_Dyn==2)
		sensitivity = UV_SM_rotgain;
	else if (UV_Cur_Dyn==1)
		sensitivity = UV_MS_rotgain;
	else
		sensitivity = UV_KB_rotgain;

#ifdef UU_RS6000
	sensitivity = sensitivity*2;
#endif
/*
.....Save vport NORMAL (Z), UP (Y), & RIGHT (X) vectors
*/
	um_vctovc(view->cur_up_vect,vup);
	um_vctovc(view->cur_pln_norm,vpn);
	um_cross(vup,vpn,vrt);
/*
.......Find the change in angle
*/
	delx = nrot * sensitivity;
	phix = delx * DTOR;
#if UU_COMP==UU_WIN2K
	if (UV_Cur_Dyn == 2)
	{
		inc = UV_actsm_dial - 1;
 		if (UV_SM_data[inc] != 0)
			phix = abs(UV_SM_data[inc])*UV_SM_sensi*UV_SM_rotgain*nrot;
		else
			return -1;
		if ((UV_actsm_dial==3) && ((vflag==3)||(vflag==4)||(vflag==5)))
			phix = -phix;
		if (((UV_actsm_dial==6)||(UV_actsm_dial==1)) && (vflag==4))
			phix = -phix;
	}
#endif
/*
........Calculate new Xform matrix
........To do this, rotate the current
........Xform matrix about a point and vector &
........Replace View Up, View Normal, and View Right vectors
........Create matrix from current VUP, VPN, & VRT vectors
*/
	for (i=0;i<3;i++)
	{
		tm[0][i] = vrt[i];
		tm[1][i] = vup[i];
		tm[2][i] = vpn[i];
		tm[3][i] = 0;
	}
	switch (vflag)
	{
	case XROT:
#if UU_COMP==UU_WIN2K
		if (UV_Cur_Dyn!=2)
			phix = phix * -1.;
#else
		phix = phix * -1.;
#endif
		um_rotlntf(cen,vup,phix,tm1);
		um_tftmtf(tm,tm1,tm);
		break;
	case YROT:
		um_rotlntf(cen,vrt,phix,tm1);
		um_tftmtf(tm,tm1,tm);
		break;
	case ZROT:
		um_rotlntf(cen,vpn,phix,tm1);
		um_tftmtf(tm,tm1,tm);
		break;
	}
/*
.....If rotating about anything other
.....than viewport center, then store
.....new viewport center
*/
	if (dyncp)
	{
		switch (vflag)
		{
		case XROT:
			um_rotatept(vpcen,vup,cen,phix,1,tm2);
			break;
		case YROT:
			um_rotatept(vpcen,vrt,cen,phix,1,tm2);
			break;
		case ZROT:
			um_rotatept(vpcen,vpn,cen,phix,1,tm2);
			break;
		}
		view->cur_ref_pt[0] = vpcen[0];
		view->cur_ref_pt[1] = vpcen[1];
		view->cur_ref_pt[2] = vpcen[2];
	}
/*
.....Store new VUP and VPN vectors
*/
	um_vctovc(&tm[1][0],view->cur_up_vect);
	um_vctovc(&tm[2][0],view->cur_pln_norm);
	if (UV_Cur_Dyn != 2)
	{
		gsvpn3(n,view->cur_pln_norm);
		gsvup3(n,view->cur_up_vect);
	}
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  uv_dynstep_zoom(n,nrot,dyncp,view)
**
**		Performs the Zoom dynamic viewing functions.
**
**    PARAMETERS   
**       INPUT  : 
**          n      Normalization xform to change.
**          vflag  Type of transformation to apply.  3 = Rotate X,
**                 4 = Rotate Y, 5 = Rotate Z.
**          nrot   A positive value moves 'nrot' steps in the positive
**                 direction.  A negative value moves in the minus
**                 direction.
**          cen    Dynamic center point.
**          dyncp  - UU_TRUE if viewing is around user defined location.
**                   UU_FALSE if viewing is around vport center.
**          vport  Active viewport.
**          view   Active view.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uv_dynstep_zoom(n,nrot,cen,dyncp,view)
int n,nrot;
UM_coord cen;
UV_view *view;
UU_LOGICAL dyncp;
{
	int inc;
	UM_coord xaxis,yaxis,zaxis;
	Gnpoint3 np1;
	Gfloat cvec[2],wcen[2],xofs,yofs;
	Gfloat sensitivity,delx;
	Gfloat vx,vy,vz,vcx,vcy,vcz,wx,wy,wz,wcx,wcy,wcz;
	Gfloat factor,sx,sy,sz,sxinit,syinit,szinit;
	Gwrect3 wswin,*pwin;
	Gnrect3 *pvp;
	UM_vector delta;
/*
.....Calculate viewport extents
*/
	pvp = gqvport3(n);
	vx = pvp->urb.x - pvp->llf.x;
	vy = pvp->urb.y - pvp->llf.y;
	vz = pvp->urb.z - pvp->llf.z;
	vcx = pvp->llf.x + vx/2.0;
	vcy = pvp->llf.y + vy/2.0;
	vcz = pvp->llf.z + vz/2.0;
/*
.....Calculate window extents
*/
	pwin = gqwindow3(n);
	wx = pwin->urb.x - pwin->llf.x;
	wy = pwin->urb.y - pwin->llf.y;
	wz = pwin->urb.z - pwin->llf.z;
	wcx = pwin->llf.x + wx/2.0;
	wcy = pwin->llf.y + wy/2.0;
	wcz = pwin->llf.z + wz/2.0;
/*
.....Calculate the window to viewport scale factors
*/
	sxinit = vx / wx; syinit = vy / wy; szinit = vz / wz;
/*
.....Calculate zoom point in window coordinates &
.....Vector direction from center of rotation to
.....center of viewport
*/
	UG_XFORM(cen[0],cen[1],cen[2],&np1,ug_cxform[n]);
	sx = np1.x - vcx;
	sy = np1.y - vcy;
	wcen[0] = sx / sxinit;
	wcen[1] = sy / syinit;
	cvec[0] = wcen[0] - wcx;
	cvec[1] = wcen[1] - wcy;
/*
.......Find the change in angle
*/
	if (UV_Cur_Dyn==2)
		sensitivity = UV_SM_zoomgain;
	else if (UV_Cur_Dyn==1)
		sensitivity = UV_MS_zoomgain;
	else
		sensitivity = UV_KB_zoomgain;

#ifdef UU_RS6000
	sensitivity = sensitivity*2;
#endif

	delx = nrot * sensitivity;

#if UU_COMP==UU_WIN2K
	if (UV_Cur_Dyn == 2)
	{
		inc = UV_actsm_dial - 1;
 		if (UV_SM_data[inc] != 0)
			delx = abs(UV_SM_data[inc])*UV_SM_sensi*UV_SM_zoomgain*nrot;
		else
			return -1;
		if ((UV_actsm_dial==4)||(UV_actsm_dial==4))
			delx = -delx;
	}
#endif
/*
.....Zoom current view
*/
	factor = pow(2.0,(delx));
	sx = sxinit * factor;
	sy = syinit * factor;
	sz = szinit * factor;
	wx = (vx / sx) / 2.0;
	wy = (vy / sy) / 2.0;
	wz = (vz / sz) / 2.0;

	xofs = wcen[0] - (cvec[0]/factor);
	yofs = wcen[1] - (cvec[1]/factor);
/*
.....make a limit for sx, sy in order to avoid to sx=0.0000 them matrix will be all 0s
*/
	if ((sx<0.0001)&&(factor<1.0000000)
		|| (sy<0.0001)&&(factor<1.0000000))
		return 0;
	wswin.llf.x = xofs - wx; 
	wswin.urb.x = xofs + wx;
	wswin.llf.y = yofs - wy; 
	wswin.urb.y = yofs + wy;
	wswin.llf.z = pwin->llf.z;
	wswin.urb.z = pwin->urb.z;
	gswindow3(n,&wswin);
/*
.....If zooming about anything other
.....than viewport center, then store
.....new viewport center
*/
	if (dyncp)
	{
		delta[0] = (wswin.urb.x + wswin.llf.x) / 2.0;
		delta[1] = (wswin.urb.y + wswin.llf.y) / 2.0;
		delta[2] = (wswin.urb.z + wswin.llf.z) / 2.0;

		um_cross(view->cur_up_vect, view->cur_pln_norm, xaxis);
		um_vctmsc(xaxis, delta[0], xaxis);
		um_vctmsc(view->cur_up_vect, delta[1], yaxis);
		um_vctmsc(view->cur_pln_norm, delta[2], zaxis);

		um_vcplvc(view->cur_ref_pt, xaxis, view->cur_ref_pt);
		um_vcplvc(view->cur_ref_pt, yaxis, view->cur_ref_pt);
		um_vcplvc(view->cur_ref_pt, zaxis, view->cur_ref_pt);
	}
/*
.....Modify the current view
*/
	view->cur_aperture = (wswin.urb.x - wswin.llf.x);
	return 0;
}
/*********************************************************************
**    I_FUNCTION     :  uv_dynstep_redraw(n)
**
**		Redraws the viewport after an immediate mode dynamic viewing
**		function.
**
**    PARAMETERS   
**       INPUT  : 
**          n		Viewport to redraw.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_dynstep_redraw(n)
int n;
{
	int isav;
/*
.....X-Window redraw
*/
	isav = ug_getsurf();
	ug_graphsurf();
	ug_clearvp(n);
/*
.....Redraw the viewport
*/
/*	ncl_redraw_geo(n,0,1,0); */
	ncl_redraw_geo(n,0,1,0, UU_TRUE);
/*
.....Reset the graphics surface
*/

	ug_setsurf(isav);
}

/*********************************************************************
**    I_FUNCTION     :  uv_dynstep_redraw(n)
**
**		Redraws the viewport after an immediate mode dynamic viewing
**		function.
**
**    PARAMETERS   
**       INPUT  : 
**          n		Viewport to redraw.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_dynstep_disp()
{
	UV_vport vport;
/*
.....Dynamic viewing only valid
.....for SGI GL and X-Windows
*/
	if (UV_dynview_active == UU_TRUE) return;
/*
.....Get active view
*/
	if ((UV_SM_NCL==0)&&(UV_Cur_Dyn==2))
/*
......spacemouse for IPV
*/
	{
		if (ul_ipv_view_active())
		{
			if (uv_getvpid(LW_vport.key, &vport) != UU_SUCCESS)
				return;
		}
		else
			return;
	}
	if ((UV_SM_NCL==0)&&(UV_Cur_Dyn==2))
	{
		ul_ipv_view_same(vport.xform);
	}
	else
	{
		uv_getvpid(UV_act_screen[0].vports[UV_current_sview-1],&vport);
		uv_setxform(&vport);
		uv_dynstep_redraw(UV_current_sview);
	}
	ug_setredrwflag(0);   
}

/*********************************************************************
**    E_FUNCTION     :  uv_get_auto_center(ipv,n,cen,dyncp)
**
**		get the current mouse position as center position for zoom
**
**    PARAMETERS   
**       INPUT  : 
**          ipv    - UU_TRUE if NCLIPV viewing is active.
**          n   - trans number.
**       OUTPUT :  
**          cen    - Center point to use for dynamic viewing.
**          dyncp  - UU_TRUE if viewing is around user defined location.
**                   UU_FALSE if viewing is around vport center.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_get_auto_center(ipv,n,cen,dyncp)
UU_LOGICAL ipv,*dyncp;
int n;
UM_coord cen;
{
	int rast[2],savcen;
	Gfloat ndc[2];
	UU_REAL nx,ny,nz;
	UU_REAL midpt[3], llf[3], urb[3];
	UM_coord savcp;
	LtDoublePoint orig;
	LtDoubleVector norm;
	UU_REAL cord[3];
	LW_rv_picked_entity rvent;
	LtPickedEntityList entlist;
	LtDouble dis;
	LtEntityType type;
	LtEntity entity;
	LtPickedEntity entpick;
	int status;
	LtDoublePoint wc;
/*
.....Set active viewport
*/
	ug_sntran(n);
/*
.....Use viewport center
*/
	if ((UV_dyncenter == 0)&&(ipv==0)&&(NCL_mouse_func==0))
	{
		*dyncp = UU_FALSE;
		givref3(n,cord);
		if (UV_dyn_zval==2)
/*
.....off, use reference point, include Z value
*/
		{
			cen[0] = cord[0];
			cen[1] = cord[0];
			cen[2] = cord[0];
			return;
		}
		gwndc3(&midpt[0], &midpt[1], &midpt[2], cord[0], cord[1], cord[2]);
		uw_glndctodev(midpt,rast);
	}
	else if ((LW_dyncenter == 0)&&(ipv==1)&&(NCL_mouse_func==0))
	{
		*dyncp = UU_FALSE;
		givref3(n,cord);
		if (LW_dyn_zval==2)
/*
.....off, use reference point, include Z value
*/
		{
			cen[0] = cord[0];
			cen[1] = cord[0];
			cen[2] = cord[0];
			return;
		}
		wc[0] = cord[0];
		wc[1] = cord[1];
		wc[2] = cord[2];
		LiDrawableProjectWorldToImage(LW_drawable,LW_viewport, wc, rast);
	}
	else
	{
		*dyncp = UU_TRUE;
		uw_get_curdc_xy(ipv, &rast[0], &rast[1]);
		if ((LW_dyn_zval==2)&&(ipv)||(UV_dyn_zval==2)&&(ipv==0))
/*
.....Off, use reference center Z value
*/
		{
			uw_gldevtondc(rast, ndc);
			givref3(n,cord);
			gwndc3(&nx, &ny, &nz, cord[0], cord[1], cord[2]);
			nx = ndc[0];
			ny = ndc[1];
			gndcw3(&nx, &ny, &nz, nx, ny, nz);
			cen[0] = nx;
			cen[1] = ny;
			cen[2] = nz;
			return;
		}
	}
	if (ipv==0)
	{
		if (UV_dyn_zval==1)
/*
.....Calc, Calculate Z value, don't use pick
*/
		{
			uw_gldevtondc(rast, ndc);
			llf[0] = ug_gksstli.vtran[n].vport.llf.x;
			llf[1] = ug_gksstli.vtran[n].vport.llf.y;
			llf[2] = ug_gksstli.vtran[n].vport.llf.z;
			urb[0] = ug_gksstli.vtran[n].vport.urb.x;
			urb[1] = ug_gksstli.vtran[n].vport.urb.y;
			urb[2] = ug_gksstli.vtran[n].vport.urb.z;
			uv_get_box_midz(llf, urb, midpt);
			nx = ndc[0];
			ny = ndc[1];
			nz = midpt[2];
			gndcw3(&nx, &ny, &nz, nx, ny, nz);
			cen[0] = nx;
			cen[1] = ny;
			cen[2] = nz;
			return;
		}
/*
.....else use pick first, if no segment pick used Z-value
*/
/*
.....If geometry item is under cursor
.....then use this point for the dynamic viewing center
*/
		uw_glput_pikgeom(rast[0],rast[1]);
		if (ud_getn_pick_segs() != 0)
		{
			um_vctovc(UV_dyncp,savcp);
			savcen = UV_dyncenter;
			uvu_dyncenter();
			nx = UV_dyncp[0]; ny = UV_dyncp[1]; nz = UV_dyncp[2];
			um_vctovc(savcp,UV_dyncp);
			UV_dyncenter = savcen;		
		}
/*
.....get mid Z-level point
*/
		else
		{
			uw_gldevtondc(rast, ndc);
			llf[0] = ug_gksstli.vtran[n].vport.llf.x;
			llf[1] = ug_gksstli.vtran[n].vport.llf.y;
			llf[2] = ug_gksstli.vtran[n].vport.llf.z;
			urb[0] = ug_gksstli.vtran[n].vport.urb.x;
			urb[1] = ug_gksstli.vtran[n].vport.urb.y;
			urb[2] = ug_gksstli.vtran[n].vport.urb.z;
			uv_get_box_midz(llf, urb, midpt);
			nx = ndc[0];
			ny = ndc[1];
			nz = midpt[2];
			gndcw3(&nx, &ny, &nz, nx, ny, nz);
		}
	}
	else
	{
		type = LI_ENTITY_TYPE_FACE;
		if (LW_dyn_zval==1)
		{
			uw_gldevtondc(rast, ndc);
/*
.....Calc, Calculate Z value, don't use pick
*/
			llf[0] = ug_gksstli.vtran[n].vport.llf.x;
			llf[1] = ug_gksstli.vtran[n].vport.llf.y;
			llf[2] = ug_gksstli.vtran[n].vport.llf.z;
			urb[0] = ug_gksstli.vtran[n].vport.urb.x;
			urb[1] = ug_gksstli.vtran[n].vport.urb.y;
			urb[2] = ug_gksstli.vtran[n].vport.urb.z;
			ul_get_ipvbox_midz(llf, urb, midpt);
			nx = ndc[0];
			ny = urb[1]-ndc[1];
			nz = midpt[2];
			gndcw3(&nx, &ny, &nz, nx, ny, nz);
			cen[0] = nx;
			cen[1] = ny;
			cen[2] = nz;
			return;
		}
/*
.....else use pick first, if no segment pick used Z-value
*/
		uw_gldevtondc(rast, ndc);
		LiDrawableProjectImageToWorld(LW_drawable,LW_viewport, rast, orig, norm);
		if (LW_mach_mode == LW_VISICUT)
		{
			entlist = 0;
			entlist = LiViSolidPick(LW_viewport,orig,norm,LI_MW_COORD_SYSTEM_VIEW,
				type,.001,2);
			if (entlist == 0) rvent.picked = UU_FALSE;
			else rvent.picked = UU_TRUE;
				
			entpick = LiViPickedEntityListGetFirst(entlist);
			if (entpick == 0) goto pick_failed;		
			status = LiViPickedEntityExpand(entpick,&entity,&type,&dis);
			if (status != 0) goto pick_failed;
		}
		else
		{
			ul_ipv_pick_next(orig,norm,&rvent);
			if (!rvent.picked) goto pick_failed;
			dis = rvent.dis;
		}
		cen[0] = orig[0] + norm[0]*dis;
		cen[1] = orig[1] + norm[1]*dis;
		cen[2] = orig[2] + norm[2]*dis;
		ncl_mcstowcs(0,cen,cen);
		return;
pick_failed:;
		llf[0] = ug_gksstli.vtran[n].vport.llf.x;
		llf[1] = ug_gksstli.vtran[n].vport.llf.y;
		llf[2] = ug_gksstli.vtran[n].vport.llf.z;
		urb[0] = ug_gksstli.vtran[n].vport.urb.x;
		urb[1] = ug_gksstli.vtran[n].vport.urb.y;
		urb[2] = ug_gksstli.vtran[n].vport.urb.z;
		ul_get_ipvbox_midz(llf, urb, midpt);
		nx = ndc[0];
		ny = urb[1]-ndc[1];
		nz = midpt[2];
		gndcw3(&nx, &ny, &nz, nx, ny, nz);
	}
	cen[0] = nx;
	cen[1] = ny;
	cen[2] = nz;
}

/*********************************************************************
**    I_FUNCTION     :  uv_get_dyn_center(ipv,n,view,cen)
**
**		Calculates the center point to use for dynamic rotations and
**		zooming.
**
**    PARAMETERS   
**       INPUT  : 
**          ipv    - UU_TRUE if NCLIPV viewing is active.
**          n   - trans number.
**          view   - Active view.
**       OUTPUT :  
**          cen    - Center point to use for dynamic viewing.
**          dyncp  - UU_TRUE if viewing is around user defined location.
**                   UU_FALSE if viewing is around vport center.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_get_dyn_center(ipv,n, view,cen,dyncp)
UU_LOGICAL ipv,*dyncp;
UV_view *view;
UM_coord cen;
int n;
{
/*
.....Use viewport center
*/
	*dyncp = UU_TRUE;
	if (NCL_mouse_func)
	{
		uv_get_auto_center(ipv, n, cen, dyncp);
		return;
	}
	if ( ((!ipv) &&(UV_dyncenter==0)||(ipv) &&(LW_dyncenter==0))
		&&(UV_dyn_zval==2))
	{
		*dyncp = UU_FALSE;
		um_vctovc(view->cur_ref_pt,cen);
	}
/*
.....Use IPV dynamic center point
*/
	else if (ipv&&(LW_dyncenter==1))
	{
		um_vctovc(LW_dyncp,cen);
	}
/*
.....Use NCL dynamic center point
*/
	else if (UV_dyncenter == 1)
	{
		um_vctovc(UV_dyncp,cen);
	}
	else if ((UV_dyncenter == 2)||(LW_dyncenter==2))
	{
		uv_get_auto_center(ipv, n, cen, dyncp);
	}
	else
	{
		uv_get_auto_center(ipv, n, cen, dyncp);
	}
}

