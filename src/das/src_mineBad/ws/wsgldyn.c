#include "usysdef.h"
#ifdef UU_OPENGL

/********************************************************************* 
**  NAME:  wsgldyn.c
**
**      GKS workstation: Dynamic viewing section
**
**		CONTAINS:
**
**			uw_glreqloc
**			uw_gldynview
**			uw_gldynaxis
**			uw_gldyndraw
**			uw_gldynlight
**
**    MODULE NAME AND RELEASE LEVEL
**       wsgldyn.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       02/08/16 , 09:19:57
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:cursorfont.h>
#else
#if UU_COMP != UU_WIN2K
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#endif
#endif
 
#include "udebug.h"					/* GKS/PHIGS pkg defs */
#include "driver.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "go1.h"
#include "gsegac.h"
#include "gmat4.h"
#include "mdcoord.h"
#include "lipv.h"
#if UU_COMP!=UU_WIN2K
#include "wsxw.h"					/* Driver include file */
#endif
#include "ginq.h"
#include "math.h"
#include "view.h"
#include "wsgl.h"
#include "spmouse.h"
#include "mdattr.h"
#include "mrender.h"
#include "mattrddl.h"
#include "mrenddl.h"
#include "mdrel.h"
#include "wsglfun.h"

#if UU_COMP==UU_WIN2K
#include "wsntglfunc.h"
int formActive;
int pan_cursor,rotate_cursor,zoom_cursor;
#else
extern Cursor pan_cursor,rotate_cursor,zoom_cursor;
extern int formActive;
#endif

int UW_dynamic_funkey = -1;
int (*UW_dynfunc_key)();

extern int MSLite;
extern int UV_dyndisply, UV_dyncenter, UV_dynstatln;
extern UU_REAL UV_dyncp[3],UV_dynvec[3],LW_dyncp[3];
extern UV_view UV_dynview, MSL_view[20];
extern int NCL_mot_seg;
extern int UV_dynview_active;
extern int UV_dynsegs;
extern UU_LOGICAL LW_active;
extern int UZ_nclipv_view;
extern int UM_light_keys[];
extern UG_wdt glwdt;
extern UU_REAL UV_dyncp2[3], LW_dyncp2[3];

#define PAN 40
#define XYROT 41
#define ZROT 42
#define ZOOM 43
#define TUMBLE 44
#define VECROT 45
#define MOUSE 46
#define STEPZOOM 47
#define NONE 0
#define XROT 101
#define YROT 102
#define NCLIPV_VIEW 19

#define glndctodevx(ndc,rast)  rast = uw_gl.wsxform.sf*ndc + uw_gl.wsxform.dx
#define glndctodevy(ndc,rast)  rast = uw_gl.wsxform.sf*ndc + uw_gl.wsxform.dy

extern int dyn_view;
extern int UV_current_sview;

void uw_gldyndraw();
void uw_gldynview();
void uw_gldynaxis();
extern int NCL_pick_verify, UD_picking_active;
extern int LW_dyncenter;
static GLuint view_obj = 0;
static int current_dyn_view = -1;
static Gnrect Svprect;
int NCL_mouse_func = 0;
/*********************************************************************
**    I_FUNCTION     :  uw_glreqloc(prms,reply)
**
**		Intercept the ug_dreqloc function call and check for prompt
**		and echo types which require X-Windows interactive control.
**		These are currently the dynamic viewing functions.
**
**    PARAMETERS   
**       INPUT  : 
**			prms = Prompt/echo device.
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glreqloc(prms,reply)
UG_reqdev *prms;
UG_reqloc *reply;
{
	int pet,xform;
	Glocst *locpt;
/*
.....Determine the current pet number
*/
	locpt = &(*ug_gksstli.wsopen[prms->id].inptr).locdata[(prms->devno)-1];
	pet = locpt->pet;
/*
.....Dynamic rotation
*/
	if (pet>39)
	{
		UV_dynview_active = UU_TRUE;
		if (MSLite==0)
		{
			if ((UZ_nclipv_view == 1)|| (LW_nclipv==LW_STANDALONE))
				xform = LW_vport.xform;
			else
				xform = ug_gksstli.curvwindex;
		}
		else
			xform = LW_vport.cur_view;
		uw_gldynview(xform,locpt);
		UV_dynview_active = UU_FALSE;
	}
/*
.....Some other pet
*/
	else if (MSLite==0)
	{
		ug_dreqloc(prms,reply);
	}
}

/*********************************************************************
**    I_FUNCTION     :  uw_gldynview(n,locpt)
**
**		Dynamically update the requested normalization transformation,
**		and redraw the requested viewport.
**
**    PARAMETERS   
**       INPUT  : 
**          n		Normalization xform to change.
**			pet		Prompt and echo type showing what to change.	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gldynview(n,locpt)
int n;
Glocst *locpt;
{
#if UU_COMP == UU_WIN2K
	int x,y;
#endif
	int xy[2],llax[2],urax[2],ll[2],ur[2],lstx,lsty,k,pushed,pan_flag;
	int i,orgx,orgy,pet,startx,starty,mb[5], verify_save, old_pet;
	int irtn,bas[2],zorgx,zorgy,dyncenter,nopick;
	UU_REAL vpn[3],vup[3],vrt[3],pt[3];
	char str[80];
	Gfloat sensitivity,tsens,zsens,psens,rsens;
	Gfloat axlen,delx,dely,phix,phiy,rotx,roty;
	Gfloat cen[3],vpcen[3],cvec[2],wcen[2],xofs,yofs, vpcen_dc[3];
	Gfloat vx,vy,vz,vcx,vcy,vcz,wx,wy,wz,wcx,wcy,wcz;
	Gfloat factor,sx,sy,sz,sxinit,syinit,szinit;
	Gwrect3 wswin;
	Gnpoint3 np1;
	UM_transf tm,tm1,tm2,tm3;
	int center_reset, out_loop, need_break;
#if UU_COMP!=UU_WIN2K
	Cursor *curs;
#else
	int *curs;
#endif
	GLint vpt[4];
	int clip[4], sav_clip[4];
	UM_vector delta;
	UM_coord xaxis,yaxis,zaxis;
	UV_vport vport;
	char name[40];
	int uz_user_fkey(), uz_user_key(), uz_mouse_key();
	Gwrect3 *window;
	Gwpoint3 pt3;
	Gwpoint3 *vup3;
	int wid, hgt, cen_rast[2];
	UU_LOGICAL dyncp;
	UU_REAL nx,ny,nz;
	UU_REAL midpt[3], llf[3], urb[3],cord[3];
	int start_fun;
/*
.....Initialize routine
*/
	start_fun = NCL_mouse_func;
	UV_Cur_Dyn = 1;
	pushed = 0;
	current_dyn_view = n;
	old_pet = -1;

	nopick = UU_FALSE;
	if (UZ_nclipv_view == 1 || LW_nclipv == LW_STANDALONE || MSLite)
	{
		dyncenter = LW_dyncenter;
		if (LW_mach_mode != LW_VISICUT)
		{
			dyncenter = 0;
			nopick = UU_TRUE;
		}
	}
	else
		dyncenter = UV_dyncenter;
/*
......if the center is user defined, don't call auto-center to get center point
*/
	if (!(locpt->pet == MOUSE) && !(dyncenter == 1 && NCL_mouse_func == 0))
	{
/*
......only in the case of locpt->pet == MOUSE, the dynamic view function
......will recal-center point when press mouse button. But for othe dynamic func,
......the auto-canter have to calc here
*/
		uv_get_auto_center(UZ_nclipv_view, n, cen, &dyncp);
		uw_get_curdc_xy(UZ_nclipv_view, &cen_rast[0], &cen_rast[1]);
		uw_ntcurdc_toscrn(UZ_nclipv_view, cen_rast[0], cen_rast[1], &cen_rast[0], &cen_rast[1]);
	}
	else
		uw_ntget_ctrpos(&cen_rast[0], &cen_rast[1], UZ_nclipv_view);
/*
.....disable NCL verify temperally because the dynamic mode highlight showing
.....also it will cause problem when done with dynamic rotation then reject without move mouse
*/
	verify_save = 0;
	if ((MSLite==0)&&(UD_picking_active) && (NCL_pick_verify))
	{
		uw_gldesel_vpik();
		NCL_pick_verify = 0;
		verify_save = 1;
	}
/*
.....Get type of dynamic rotation to perform
*/
 	pet = locpt->pet;
/*
.....Set moust position to center of screen
*/
#if UU_COMP!=UU_WIN2K
	XWarpPointer(uw_xw.disp,None,uw_xw.wd_id,0,0,0,0,
		uw_gl.xpixels/2,uw_gl.ypixels/2);
/*
.....Save current mouse position
*/
/*
	uw_mfevent(&k,0,xy,0);
*/
/*
.....don't need call uw_mfevent to get position, 
.....it just sets at middle of
screen.
*/
	xy[0] = uw_gl.xpixels/2;
	xy[1] = uw_gl.ypixels/2;
#else
	if (!(dyncenter == 2 || NCL_mouse_func))
	{
		uw_ntget_ctrpos(&x, &y, UZ_nclipv_view);
	}
	else
	{
		x = cen_rast[0]; 
		y = cen_rast[1];
	}
	uw_ntset_curpos(x, y);
/*
.....Save current mouse position
*/
	irtn = uw_ntevent(&k,1,xy,1);
	if (locpt->pet != MOUSE)
	{
		if ((irtn != 6) || ((NCL_mouse_func==0)&&(start_fun==0)))
		{
			if (UZ_nclipv_view == 1)
				uw_ntset_ipv_capture();
			else
				uw_ntset_gwcapture();
		}
		else
/*
......key release, get out of the dynamic viewing
*/
		{
			goto loopnd;
		}
	}
	uw_ntget_gwsize(&wid, &hgt);
	if ((MSLite)||(LW_nclipv==LW_STANDALONE))
		xy[1] = hgt - xy[1];
#endif
/*
.....Calculate Pixel size of viewport
*/
	uw_glndctodev(&ug_gksstli.vtran[n].vport.llf, ll);
	uw_glndctodev(&ug_gksstli.vtran[n].vport.urb, ur);
/*
.....Store center of rotation
*/
	if (dyncenter == 1 && NCL_mouse_func == 0)
	{
		if (UZ_nclipv_view == 1 || MSLite || LW_nclipv==LW_STANDALONE)
		{
			cen[0] = LW_dyncp[0];
			cen[1] = LW_dyncp[1];
			cen[2] = LW_dyncp[2];
		}
		else
		{
			cen[0] = UV_dyncp[0];
			cen[1] = UV_dyncp[1];
			cen[2] = UV_dyncp[2];
		}
	}
/*
.....Get center of viewport in WC
*/
	vpcen[0] = UV_dynview.cur_ref_pt[0];
	vpcen[1] = UV_dynview.cur_ref_pt[1];
	vpcen[2] = UV_dynview.cur_ref_pt[2];
/*
......save center of vport in DC
*/
	gwndc3(&vpcen_dc[0], &vpcen_dc[1], &vpcen_dc[2], vpcen[0], vpcen[1], vpcen[2]);	
/*
.....Calculate viewport extents
*/
	vx = ug_gksstli.vtran[n].vport.urb.x - ug_gksstli.vtran[n].vport.llf.x;
	vy = ug_gksstli.vtran[n].vport.urb.y - ug_gksstli.vtran[n].vport.llf.y;
	vz = ug_gksstli.vtran[n].vport.urb.z - ug_gksstli.vtran[n].vport.llf.z;
	vcx = ug_gksstli.vtran[n].vport.llf.x + vx/2.0;
	vcy = ug_gksstli.vtran[n].vport.llf.y + vy/2.0;
	vcz = ug_gksstli.vtran[n].vport.llf.z + vz/2.0;
/*
.....Set viewport update area
*/
	Svprect.ll.x = ug_gksstli.vtran[n].vport.llf.x;
	Svprect.ll.y = ug_gksstli.vtran[n].vport.llf.y;
	Svprect.ur.x = ug_gksstli.vtran[n].vport.urb.x;
	Svprect.ur.y = ug_gksstli.vtran[n].vport.urb.y;
/*
.....Calculate window extents
*/
	wx = ug_gksstli.vtran[n].window.urb.x - ug_gksstli.vtran[n].window.llf.x;
	wy = ug_gksstli.vtran[n].window.urb.y - ug_gksstli.vtran[n].window.llf.y;
	wz = ug_gksstli.vtran[n].window.urb.z - ug_gksstli.vtran[n].window.llf.z;
	wcx = ug_gksstli.vtran[n].window.llf.x + wx/2.0;
	wcy = ug_gksstli.vtran[n].window.llf.y + wy/2.0;
	wcz = ug_gksstli.vtran[n].window.llf.z + wz/2.0;
/*
.....Calculate the window to viewport scale factors
*/
	sxinit = vx / wx; syinit = vy / wy; szinit = vz / wz;
	factor = 1.;
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
.....Calculate length of drawn axis
*/
	axlen = (ug_gksstli.vtran[n].window.urb.y -
		ug_gksstli.vtran[n].window.llf.y) / 5.;
/*
.....Calculate sensitivity based on
.....type of dynamic viewing
*/
	psens = (ug_gksstli.vtran[n].window.urb.x -
		ug_gksstli.vtran[n].window.llf.x) * 0.0013;
	rsens = .4;
	tsens = .05;
	zsens = .005;
	if (pet == TUMBLE)
		sensitivity = tsens;
	else if (pet == PAN)
		sensitivity = psens;
	else if (pet == ZOOM)
		sensitivity = zsens;
	else
		sensitivity = rsens;
/*
.....Save vport NORMAL (Z), UP (Y), & RIGHT (X) vectors
*/
	vpn[0] = ug_gksstli.vtran[n].vpnorm.x;
	vpn[1] = ug_gksstli.vtran[n].vpnorm.y;
	vpn[2] = ug_gksstli.vtran[n].vpnorm.z;
	vup[0] = ug_gksstli.vtran[n].vup.x;
	vup[1] = ug_gksstli.vtran[n].vup.y;
	vup[2] = ug_gksstli.vtran[n].vup.z;
	um_cross(vup,vpn,vrt);
/*
.....Save current mouse position
*/
	orgx = xy[0]; orgy = xy[1];
	zorgx = xy[0]; zorgy = xy[1];
	startx = xy[0]; starty = xy[1];
	bas[0] = 0; bas[1] = 0;
/*
.....Force axis draw on first entry &
.....Disallow axis erasure
*/
	lstx = 10000;
	llax[0] = 10000;
	rotx = 0; roty = 0;
/*
.....Create double buffer
*/
/*
.....Clip graphics to this viewport
*/
	vpt[0] = ll[0] ; vpt[1] = ll[1] ; vpt[2] = ur[0] - ll[0];
	vpt[3] = ur[1] - ll[1];
	if ((MSLite==0)&&(LW_nclipv==LW_INPROCESS))
	{
		if (UV_dyndisply != 1 && UZ_nclipv_view != 1)
			uw_gldrawbuffer(UG_BACK_BUFFER);
		if (UV_dyndisply == 0 || UV_dyndisply == 2)
		{
			if (UZ_nclipv_view != 1)
			{
				view_obj = glGenLists_d(1);
				uw_glload_matrix(n,UU_FALSE,UU_NULL);
				uv_getvpid(UV_act_screen[0].vports[n-1],&vport);
	/*
	.....added glScissor test
	.....Yurong 11/11/98
	*/
				uw_glget_scissor(sav_clip);
				clip[0] = ll[0];
				clip[1] = ll[1];
				clip[2] = vpt[2];
				clip[3] = vpt[3];
				uw_glset_scissor(clip);
				if (ncl_cutseg_front(n-1))
				{
					glNewList_d(view_obj, GL_COMPILE_AND_EXECUTE);
					uw_gldyndraw(n,ll,ur,0);
					glEndList_d();
					uw_glswapbuffer(&Svprect);
				}
				else
				{
					glNewList_d(view_obj, GL_COMPILE);
					uw_gldyndraw(n,ll,ur,0);
					glEndList_d();
				}
			}
		}
	}
/*
.....Define the correct cursor
*/
	switch (pet)
	{
	case 40: curs = &pan_cursor; break;
	case 43: curs = &zoom_cursor; break;
	default: curs = &rotate_cursor; break;
	}
/*
.....Disable menus during dynamic rotation
*/
	if (locpt->pet == MOUSE) formActive = UU_TRUE;
/*
.....Loop until input from the user
*/
	mb[0] = 0; mb[1] = 0; mb[2] = 0; mb[3] = 0; mb[4] = 0;
	center_reset = 0;
	while (UU_TRUE)
	{
/*
........Get Mouse movement
*/
loopst:;
		need_break = out_loop = 0;
#if UU_COMP!=UU_WIN2K
		irtn = uw_mfevent(&k,0,xy,0);
		uw_mfsetcursor(locpt->pet);
/*
........If standard view functions and
........user entered a key, then exit
*/
		if ((locpt->pet != MOUSE && irtn != 0 && irtn != 6) 
			 || ( (locpt->pet == MOUSE) && ((irtn == 1) || (irtn == 2)) ))
		{
/*
.....check if the it is KEY_FUNCTION entered
*/
/*
.....if vecrot, don't allow change center
*/
			if (pet == VECROT) break;
			if ((irtn != 2)&&(irtn != 1))	break;
			else
			{	
				uz_getkey_funnam(irtn, k, name);
				if (strcmp(name, "KEY_FUNCTION")==0)
				{
					if (irtn == 1)
						UW_dynfunc_key = uz_user_key;
					else
						UW_dynfunc_key = uz_user_fkey;
					UW_dynamic_funkey = k;
					break;
				}
				else
					break;
			}
		}
		if (locpt->pet != MOUSE)
			XGrabPointer(uw_xw.disp,uw_xw.wd_id,False,ButtonPressMask,
				GrabModeAsync,GrabModeAsync,None,*curs,CurrentTime);
		else
		{
#else
		irtn = uw_ntevent(&k,1,xy,1);
		uw_ntget_gwsize(&wid, &hgt);
		if ((MSLite)||(LW_nclipv==LW_STANDALONE)) xy[1] = hgt - xy[1];
		uw_ntsetcursor(locpt->pet);
		if (irtn == 6) 
			irtn = 6;
/*
........If standard view functions and
........user entered a key, then exit
*/
		if ((locpt->pet != MOUSE && irtn != 0 && irtn != 6) 
			 || ( (locpt->pet == MOUSE) && ((irtn == 1) || (irtn == 2)) ))
		{
/*
.....check if the it is KEY_FUNCTION entered
*/
/*
.....if vecrot, don't allow change center
*/
			if (pet == VECROT) break;
			if ((irtn != 2)&&(irtn != 1)&&(irtn != 3))	
				break;
/*
......if it is not mouse function, we allow the mouse state control
......the dynamic view
*/
			else if (irtn == 3) 
			{
				UW_dynfunc_key = uz_mouse_key;
				UW_dynamic_funkey = k;
				break;
			}
			else
			{	
				uz_getkey_funnam(irtn, k, name);
				if (strcmp(name, "KEY_FUNCTION")==0)
				{
					if (irtn == 1)
						UW_dynfunc_key = uz_user_key;
					else
						UW_dynfunc_key = uz_user_fkey;
					UW_dynamic_funkey = k;
					break;
				}
				else
					break;
			}
		}
/*
.....execute key function
*/
		if (locpt->pet != MOUSE)
		{
			if ((irtn != 6) || ((NCL_mouse_func==0)&&(start_fun==0)))
			{
				if (UZ_nclipv_view == 1)
					uw_ntset_ipv_capture();
				else
					uw_ntset_gwcapture();
			}
			else
/*
......key release, get out of the dynamic viewing
*/
			{
/*
.....if it is key function, draw the last event then break
*/
				if (start_fun)
				{
					need_break = 1;
				}
				else
					break;
			}
		}
		else
		{
			if (UZ_nclipv_view == 1)
				uw_ntset_ipv_capture();
			else
				uw_ntset_gwcapture();
#endif
/*
........Mouse controlled dynamic viewing
........Check for button press/release
*/
			switch (irtn)
			{
			case 1:
				goto loopnd;
			case 3:
				mb[k-1] = 1;
/*
.......AUTO center use the current point as rot center
*/
				center_reset = 0;
				if ((mb[0] == 1)&&(mb[1] ==0)&&(mb[2]==0)&&(mb[3]==0))
					pan_flag = 1;
				else
					pan_flag = 0;
/*
......if the center is user defined, don't call auto-center to get center point
*/
				if (old_pet==-1 && !(dyncenter==1 && NCL_mouse_func==0) && !nopick)
				{
/*
......cal center the first time
*/
					uv_get_auto_center(UZ_nclipv_view, n, cen, &dyncp); 
					uw_get_curdc_xy(UZ_nclipv_view, &cen_rast[0], &cen_rast[1]);
					uw_ntcurdc_toscrn(UZ_nclipv_view, cen_rast[0], cen_rast[1], &cen_rast[0], &cen_rast[1]);
				}
/*
......we don't need recal-center point if it is the operation is pan
*/
				else if (pan_flag==1)
					goto loopst;
/*
......we need recal-center point if it is auto-center and the operation is not pan
......pan operation don't need recal-center
*/
				else if (dyncenter == 2 && locpt->pet == MOUSE && pan_flag == 0)
				{
					uv_get_auto_center(UZ_nclipv_view, n, cen, &dyncp); 
					uw_get_curdc_xy(UZ_nclipv_view, &cen_rast[0], &cen_rast[1]);
					uw_ntcurdc_toscrn(UZ_nclipv_view, cen_rast[0], cen_rast[1], &cen_rast[0], &cen_rast[1]);
				}
/*
......if viewport center, we don't need recal-center unless the previous
......operation is pan or first time here
*/
				else if (dyncenter == 0 && locpt->pet == MOUSE &&
					(old_pet == PAN || old_pet == -1))
				{
/*
......when MSLite, it will not recal-center, the way
......it handle the vport is different
......it already reset when Paned, so we don't do any thing here
......
......this previos operation is paning, the value center
......is still the original center, only
......Z could be changed
*/
					if ((old_pet==PAN)&&(MSLite==0) || nopick)
					{
						llf[0] = ug_gksstli.vtran[n].vport.llf.x;
						llf[1] = ug_gksstli.vtran[n].vport.llf.y;
						llf[2] = ug_gksstli.vtran[n].vport.llf.z;
						urb[0] = ug_gksstli.vtran[n].vport.urb.x;
						urb[1] = ug_gksstli.vtran[n].vport.urb.y;
						urb[2] = ug_gksstli.vtran[n].vport.urb.z;
						if (UZ_nclipv_view==0)
						{
							uv_get_box_midz(llf, urb, midpt);
						}
						else
						{
							ul_get_ipvbox_midz(llf, urb, midpt);
						}
						nx = vpcen_dc[0];
						ny = vpcen_dc[1];
						nz = midpt[2];
						gndcw3(&nx, &ny, &nz, nx, ny, nz);
						cen[0] = nx;
						cen[1] = ny;
						cen[2] = nz;
					}
					else if (MSLite==0 && !nopick)
					{
						uv_get_auto_center(UZ_nclipv_view, n, cen, &dyncp); 
						uw_get_curdc_xy(UZ_nclipv_view, &cen_rast[0], &cen_rast[1]);
						uw_ntcurdc_toscrn(UZ_nclipv_view, cen_rast[0], cen_rast[1], &cen_rast[0], &cen_rast[1]);
					}
				}
				center_reset = 1;
				goto loopst;
			case 6:
				mb[k-1] = 0;
				center_reset = 0;
/*
.....Reset to window and view when auto-center
.....since the auto-center is calcaulated using
.....the current view/window
*/
				if (UV_dyncenter==2)
				{
					if ((UZ_nclipv_view != 1)&&(MSLite==0)&&(LW_nclipv!=LW_STANDALONE))
					{
						uv_getvpid(UV_act_screen[0].vports[n-1],&vport);
					}
					else
					{
						uv_getvpid(LW_vport.key, &vport);
					}
					givpn3(vport.xform, &pt3);
					uv_getvid(vport.cur_view, &UV_dynview);
					UV_dynview.cur_pln_norm[0] = pt3.x;
					UV_dynview.cur_pln_norm[1] = pt3.y;
					UV_dynview.cur_pln_norm[2] = pt3.z;
					vup3 = gqvup3(vport.xform);

					if (MSLite==0)
						vup3 = gqvup3(vport.xform);
					else
						vup3 = gqvup3(n);
					UV_dynview.cur_up_vect[0] = (*vup3).x;
					UV_dynview.cur_up_vect[1] = (*vup3).y;
					UV_dynview.cur_up_vect[2] = (*vup3).z;

					if (MSLite==0)
						window = gqwindow3(vport.xform);
					else
						window = gqwindow3(n);

					UV_dynview.cur_aperture = (window->urb.x - window->llf.x);
					UV_dynview.cur_ref_pt[0] = vpcen[0];
					UV_dynview.cur_ref_pt[1] = vpcen[1];
					UV_dynview.cur_ref_pt[2] = vpcen[2];

					if (MSLite)
					{
						uu_move_byte((char*)&UV_dynview, (char*)&MSL_view[LW_vport.cur_view], 
									sizeof(UV_view));
						uv_setxform();
						ul_ipv_view_same(n);
					}
					else
					{
						uv_putv(&UV_dynview);
						if (UZ_nclipv_view == 1)
						{
							um_set_screen_area(UM_IPV_WINDOW);
							uv_setxform(&vport);
							ul_ipv_view_same(vport.xform);
						}
						else 
							uv_updatevp(&vport, UU_FALSE);
					}
				}
				goto loopst;
			}
			sensitivity = rsens;
			if (mb[0] == 1)
			{
/*
........Button 1, Button 2, Button 3
........End of dynamic viewing
*/
				if (mb[1] == 1 && mb[2] == 1) goto loopnd;
/*
........Button 1, Button 2
........X-Rotation
*/
				else if (mb[1] == 1) pet = XROT;
/*
........Button 1, Button 3
........Y-Rotation
*/
				else if (mb[2] == 1) pet = YROT;
				else if (mb[3] == 1)
				{
					if (locpt->pet == MOUSE)
					{
						pet = STEPZOOM;
						sensitivity = UV_KB_zoomgain;
					}
				}
				else if (mb[4] == 1)
				{
					if (locpt->pet == MOUSE)
					{
						pet = STEPZOOM;
						sensitivity = -UV_KB_zoomgain;
					}
				}
/*
........Button 1
........Pan
*/
				else
				{
					pet = PAN;
					psens = (ug_gksstli.vtran[n].window.urb.x -
					ug_gksstli.vtran[n].window.llf.x) * 0.0009;
					sensitivity = psens;
				}
			}
			else if (mb[1] == 1)
			{
/*
........Button 2, Button 3
........Z-Rotation
*/
				if (mb[2] == 1) pet = ZROT;
				else if (mb[3] == 1)
				{
					if (locpt->pet == MOUSE)
					{
						pet = STEPZOOM;
						sensitivity = UV_KB_zoomgain;
					}
				}
				else if (mb[4] == 1)
				{
					if (locpt->pet == MOUSE)
					{
						pet = STEPZOOM;
						sensitivity = -UV_KB_zoomgain;
					}
				}
/*
........Button 2
........Zoom
*/
				else
				{
					pet = ZOOM;
					sensitivity = zsens;
				}
			}
/*
........Button 3
........XY-Rotation
*/
			else if (mb[2] == 1) pet = XYROT;
			else if (mb[3] == 1)
			{
				if (locpt->pet == MOUSE)
				{
					pet = STEPZOOM;
					sensitivity = UV_KB_zoomgain;
				}
			}
			else if (mb[4] == 1)
			{
				if (locpt->pet == MOUSE)
				{
					pet = STEPZOOM;
					sensitivity = -UV_KB_zoomgain;
				}
			}
/*
........No Button
........Don't do anything
*/
			else pet = NONE;
		}
/*
.......Don't do anything if the mouse has not moved
*/
		if ((xy[0] != lstx || xy[1] != lsty)||(pet==STEPZOOM))
		{
			if (pet != TUMBLE)
			{
				if (locpt->pet == MOUSE)
				{
					lstx = xy[0];
					lsty = xy[1];
				}
				else
				{
					lstx = startx;
					lsty = starty;
				}
			}
/*
.......Find the mouse movement
.......Mouse dynamic Zoom is special
*/
			if (locpt->pet == MOUSE && pet == ZOOM)
			{
				delx = (xy[1]-zorgy) * sensitivity;
				dely = (zorgx-xy[0]) * sensitivity;
			}
			else if (pet==STEPZOOM)
			{
				delx = sensitivity;
				dely = sensitivity;
			}
/*
.......Find the mouse movement
.......Other than Mouse zoom
*/
			else
			{
				delx = (xy[1]-orgy) * sensitivity;
				dely = (orgx-xy[0]) * sensitivity;
			}
/*
........Special values when using Mouse dynamic viewing
*/
			if (locpt->pet == MOUSE)
			{
				orgx = xy[0];
				orgy = xy[1];
				if (pet == ZOOM)
				{
					bas[0] = startx - xy[0];
					bas[1] = starty - xy[1];
					zorgx = xy[0] + (startx - xy[0]);
					zorgy = xy[1] + (starty - xy[1]);
				}
				else
				{
					zorgx = xy[0] + bas[0];
					zorgy = xy[1] + bas[1];
					startx = xy[0] + bas[0];
					starty = xy[1] + bas[1];
				}
			}
/*
........Reposition the mouse to
........center of screen if not Mouse viewing
*/
			else if (pet != TUMBLE)
			{
#if UU_COMP!=UU_WIN2K
				XWarpPointer(uw_xw.disp,None,uw_xw.wd_id,0,0,0,0,
				uw_gl.xpixels/2,uw_gl.ypixels/2);
#else
				if (!(dyncenter == 2 || NCL_mouse_func))
				{
					uw_ntget_ctrpos(&x, &y, UZ_nclipv_view);
				}
				else
				{
					x = cen_rast[0]; 
					y = cen_rast[1];
				}
				uw_ntset_curpos(x, y);
#endif
				if (pet == ZOOM)
				{
					orgx = orgx + (startx - xy[0]);
					orgy = orgy + (starty - xy[1]);
				}
			}
/*
........Calculate new rotations (movement)
*/
			phix = delx * DTOR;
			phiy = dely * DTOR;
			if (pet == XROT) phix = 0;
			if (pet == YROT) phiy = 0;
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
/*
........Erase the Viewing Axes
*/
			if ((MSLite==0)&&(llax[0] != 10000 && UV_dyndisply == 1 &&
				pet != NONE && UZ_nclipv_view != 1))
			{
				uw_gldynaxis(n, cen,axlen,ug_gksstli.vtran[n],llax,urax,ll,ur);
			}
/*
...........Rotate this matrix about X, then Y
*/
			switch (pet)
			{
			case XROT:
			case YROT:
			case XYROT:
			case TUMBLE:
				um_rotlntf(cen,vrt,phix,tm1);
				um_rotlntf(cen,vup,phiy,tm2);
				um_tftmtf(tm,tm1,tm3);
				um_tftmtf(tm3,tm2,tm);
				old_pet = pet;
				break;
			case ZROT:
				um_rotlntf(cen,vpn,phiy-phix,tm1);
				um_tftmtf(tm,tm1,tm);
				old_pet = pet;
				break;
			case VECROT:
				um_rotlntf(cen,UV_dynvec,phiy-phix,tm1);
				um_tftmtf(tm,tm1,tm);
				old_pet = pet;
				break;
			case PAN:
				wswin.llf.x = ug_gksstli.vtran[n].window.llf.x + dely;
				wswin.llf.y = ug_gksstli.vtran[n].window.llf.y - delx;
				wswin.llf.z = ug_gksstli.vtran[n].window.llf.z;
				wswin.urb.x = ug_gksstli.vtran[n].window.urb.x + dely;
				wswin.urb.y = ug_gksstli.vtran[n].window.urb.y - delx;
				wswin.urb.z = ug_gksstli.vtran[n].window.urb.z;
				gswindow3(n,&wswin);
				rotx = rotx - dely;
				roty = roty + delx;
/*
........Calculate new center vector
........for Zoom when Mouse viewing
*/
				cvec[0] = cvec[0] - (dely*factor);
				cvec[1] = cvec[1] + (delx*factor);
/*
........Calculate new viewport center
*/
				delta[0] = (ug_gksstli.vtran[n].window.urb.x +
						ug_gksstli.vtran[n].window.llf.x) / 2.0;
				delta[1] = (ug_gksstli.vtran[n].window.urb.y +
						ug_gksstli.vtran[n].window.llf.y) / 2.0;
				delta[2] = (ug_gksstli.vtran[n].window.urb.z +
						ug_gksstli.vtran[n].window.llf.z) / 2.0;
				um_cross(vup, vpn, xaxis);
				um_vctmsc(xaxis, delta[0], xaxis);
				um_vctmsc(vup, delta[1], yaxis);
				um_vctmsc(vpn, delta[2], zaxis);
				vpcen[0] = UV_dynview.cur_ref_pt[0];
				vpcen[1] = UV_dynview.cur_ref_pt[1];
				vpcen[2] = UV_dynview.cur_ref_pt[2];
				um_vcplvc(vpcen, xaxis, vpcen);
				um_vcplvc(vpcen, yaxis, vpcen);
				um_vcplvc(vpcen, zaxis, vpcen);
				if (dyncenter == 0)
				{
					cen[0] = vpcen[0];
					cen[1] = vpcen[1];
					cen[2] = vpcen[2];
				}
				old_pet = pet;
				break;
			case ZOOM:
				if (old_pet==PAN && locpt->pet == MOUSE && dyncenter == 0)
				{
					if (dyncenter == 0 && MSLite)
					{
						delta[0] = (ug_gksstli.vtran[n].window.urb.x +
									ug_gksstli.vtran[n].window.llf.x) / 2.0;
						delta[1] = (ug_gksstli.vtran[n].window.urb.y +
									ug_gksstli.vtran[n].window.llf.y) / 2.0;
						delta[2] = (ug_gksstli.vtran[n].window.urb.z +
									ug_gksstli.vtran[n].window.llf.z) / 2.0;
						um_cross(vup, vpn, xaxis);
						um_vctmsc(xaxis, delta[0], xaxis);
						um_vctmsc(vup, delta[1], yaxis);
						um_vctmsc(vpn, delta[2], zaxis);
						vpcen[0] = ug_gksstli.vtran[n].vrefpt.x;
						vpcen[1] = ug_gksstli.vtran[n].vrefpt.y;
						vpcen[2] = ug_gksstli.vtran[n].vrefpt.z; 
						um_vcplvc(vpcen, xaxis, vpcen);
						um_vcplvc(vpcen, yaxis, vpcen);
						um_vcplvc(vpcen, zaxis, vpcen);
						givpn3(n, &pt3);
					}
					else if (MSLite==0)
					{
						if (UZ_nclipv_view == 1)
							uv_getvpid(LW_vport.key, &vport);
						else
							uv_getvpid(UV_act_screen[0].vports[n-1],&vport);
						givpn3(vport.xform, &pt3);
						uv_getvid(vport.cur_view, &UV_dynview);
					}
					old_pet = pet;
					UV_dynview.cur_pln_norm[0] = pt3.x;
					UV_dynview.cur_pln_norm[1] = pt3.y;
					UV_dynview.cur_pln_norm[2] = pt3.z;
					if (MSLite==0)
						vup3 = gqvup3(vport.xform);
					else
						vup3 = gqvup3(n);
					UV_dynview.cur_up_vect[0] = (*vup3).x;
					UV_dynview.cur_up_vect[1] = (*vup3).y;
					UV_dynview.cur_up_vect[2] = (*vup3).z;

					if (MSLite==0)
						window = gqwindow3(vport.xform);
					else
						window = gqwindow3(n);
					UV_dynview.cur_aperture = (window->urb.x - window->llf.x);

					UV_dynview.cur_ref_pt[0] = vpcen[0];
					UV_dynview.cur_ref_pt[1] = vpcen[1];
					UV_dynview.cur_ref_pt[2] = vpcen[2];
					if (MSLite)
					{
						uu_move_byte((char*)&UV_dynview, (char*)&MSL_view[LW_vport.cur_view], 
									sizeof(UV_view));
						uv_setxform();
						ul_ipv_view_same(n);
					}
					else
					{
						uv_putv(&UV_dynview);
						if (UZ_nclipv_view == 1)
						{
							um_set_screen_area(UM_IPV_WINDOW);
							uv_setxform(&vport);
							ul_ipv_view_same(vport.xform);
						}
						else uv_updatevp(&vport, UU_FALSE);
					}
					if (dyncenter == 0)
					{
						if (UZ_nclipv_view || MSLite || LW_nclipv==LW_STANDALONE)
						{
							LW_dyncp[0] = UV_dynview.cur_ref_pt[0];
							LW_dyncp[1] = UV_dynview.cur_ref_pt[1];
							LW_dyncp[2] = UV_dynview.cur_ref_pt[2]; 
						}
						else
						{
							UV_dyncp[0] = UV_dynview.cur_ref_pt[0];
							UV_dyncp[1] = UV_dynview.cur_ref_pt[1];
							UV_dyncp[2] = UV_dynview.cur_ref_pt[2]; 
						}
					}
					if (UZ_nclipv_view == 1 || MSLite || LW_nclipv==LW_STANDALONE)
					{
						cen[0] = LW_dyncp[0];
						cen[1] = LW_dyncp[1];
						cen[2] = LW_dyncp[2];
					}
					else
					{
						cen[0] = UV_dyncp[0];
						cen[1] = UV_dyncp[1];
						cen[2] = UV_dyncp[2];
					}
					
					vpcen[0] = UV_dynview.cur_ref_pt[0];
					vpcen[1] = UV_dynview.cur_ref_pt[1];
					vpcen[2] = UV_dynview.cur_ref_pt[2];

					vx = ug_gksstli.vtran[n].vport.urb.x - ug_gksstli.vtran[n].vport.llf.x;
					vy = ug_gksstli.vtran[n].vport.urb.y - ug_gksstli.vtran[n].vport.llf.y;
					vz = ug_gksstli.vtran[n].vport.urb.z - ug_gksstli.vtran[n].vport.llf.z;
					vcx = ug_gksstli.vtran[n].vport.llf.x + vx/2.0;
					vcy = ug_gksstli.vtran[n].vport.llf.y + vy/2.0;
					vcz = ug_gksstli.vtran[n].vport.llf.z + vz/2.0;

					wx = ug_gksstli.vtran[n].window.urb.x - ug_gksstli.vtran[n].window.llf.x;
					wy = ug_gksstli.vtran[n].window.urb.y - ug_gksstli.vtran[n].window.llf.y;
					wz = ug_gksstli.vtran[n].window.urb.z - ug_gksstli.vtran[n].window.llf.z;
					wcx = ug_gksstli.vtran[n].window.llf.x + wx/2.0;
					wcy = ug_gksstli.vtran[n].window.llf.y + wy/2.0;
					wcz = ug_gksstli.vtran[n].window.llf.z + wz/2.0;

					sxinit = vx / wx; syinit = vy / wy; szinit = vz / wz;
					UG_XFORM(cen[0],cen[1],cen[2],&np1,ug_cxform[n]);
					sx = np1.x - vcx;
					sy = np1.y - vcy;
					wcen[0] = sx / sxinit;
					wcen[1] = sy / syinit;
					cvec[0] = wcen[0] - wcx;
					cvec[1] = wcen[1] - wcy;

					vpn[0] = ug_gksstli.vtran[n].vpnorm.x;
					vpn[1] = ug_gksstli.vtran[n].vpnorm.y;
					vpn[2] = ug_gksstli.vtran[n].vpnorm.z;
					vup[0] = ug_gksstli.vtran[n].vup.x;
					vup[1] = ug_gksstli.vtran[n].vup.y;
					vup[2] = ug_gksstli.vtran[n].vup.z;
					um_cross(vup,vpn,vrt);
					
					factor = 1;
					if ((MSLite==0)&&UV_dyndisply != 1 && UZ_nclipv_view != 1)
						uw_gldrawbuffer(UG_BACK_BUFFER);
#if UU_COMP!=UU_WIN2K
					XWarpPointer(uw_xw.disp,None,uw_xw.wd_id,0,0,0,0,
						uw_gl.xpixels/2,uw_gl.ypixels/2);
					xy[0] = uw_gl.xpixels/2;
					xy[1] = uw_gl.ypixels/2;
#else
					if (!(dyncenter == 2 || NCL_mouse_func))
					{
						uw_ntget_ctrpos(&x, &y, UZ_nclipv_view);
					}
					else
					{
						x = cen_rast[0]; 
						y = cen_rast[1];
					}
					uw_ntset_curpos(x, y);
					irtn = uw_ntevent(&k,1,xy,1);
					if (locpt->pet != MOUSE)
					{
						if ((irtn != 6) || ((NCL_mouse_func==0)&&(start_fun==0)))
							out_loop = 0;
						else
							out_loop = 1;
					}
					uw_ntget_gwsize(&wid, &hgt);
					if ((MSLite)||(LW_nclipv==LW_STANDALONE)) xy[1] = hgt - xy[1];
#endif
					lstx = 10000;
					lsty = xy[1];
					orgx = xy[0]; orgy = xy[1];
					zorgx = xy[0]; zorgy = xy[1];
					startx = xy[0]; starty = xy[1];
				}
				else
					factor = pow(2.0,-(delx));

				if (dyncenter != 1 && center_reset)
				{
					if ((UZ_nclipv_view != 1)&&(MSLite==0)&&(LW_nclipv!=LW_STANDALONE))
					{
						uv_getvpid(UV_act_screen[0].vports[n-1],&vport);
					}
					else
					{
						uv_getvpid(LW_vport.key, &vport);
					}
					givpn3(vport.xform, &pt3);
					uv_getvid(vport.cur_view, &UV_dynview);
					UV_dynview.cur_pln_norm[0] = pt3.x;
					UV_dynview.cur_pln_norm[1] = pt3.y;
					UV_dynview.cur_pln_norm[2] = pt3.z;
					vup3 = gqvup3(vport.xform);
					UV_dynview.cur_up_vect[0] = (*vup3).x;
					UV_dynview.cur_up_vect[1] = (*vup3).y;
					UV_dynview.cur_up_vect[2] = (*vup3).z;
					window = gqwindow3(vport.xform);
					UV_dynview.cur_aperture = (window->urb.x - window->llf.x);
					UV_dynview.cur_ref_pt[0] = vpcen[0];
					UV_dynview.cur_ref_pt[1] = vpcen[1];
					UV_dynview.cur_ref_pt[2] = vpcen[2];
					uv_putv(&UV_dynview);
					uv_updatevp(&vport, UU_FALSE);
/*
.....Calculate viewport extents
*/
					vx = ug_gksstli.vtran[n].vport.urb.x - ug_gksstli.vtran[n].vport.llf.x;
					vy = ug_gksstli.vtran[n].vport.urb.y - ug_gksstli.vtran[n].vport.llf.y;
					vz = ug_gksstli.vtran[n].vport.urb.z - ug_gksstli.vtran[n].vport.llf.z;
					vcx = ug_gksstli.vtran[n].vport.llf.x + vx/2.0;
					vcy = ug_gksstli.vtran[n].vport.llf.y + vy/2.0;
					vcz = ug_gksstli.vtran[n].vport.llf.z + vz/2.0;
/*
.....Calculate window extents
*/
					wx = ug_gksstli.vtran[n].window.urb.x - ug_gksstli.vtran[n].window.llf.x;
					wy = ug_gksstli.vtran[n].window.urb.y - ug_gksstli.vtran[n].window.llf.y;
					wz = ug_gksstli.vtran[n].window.urb.z - ug_gksstli.vtran[n].window.llf.z;
					wcx = ug_gksstli.vtran[n].window.llf.x + wx/2.0;
					wcy = ug_gksstli.vtran[n].window.llf.y + wy/2.0;
					wcz = ug_gksstli.vtran[n].window.llf.z + wz/2.0;
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
					center_reset = 0;
					lstx = 10000;
					lsty = xy[1];
					orgx = xy[0]; orgy = xy[1];
					zorgx = xy[0]; zorgy = xy[1];
					startx = xy[0]; starty = xy[1];
				}
				sx = sxinit * factor;
				sy = syinit * factor;
				sz = szinit * factor;
				wx = 0.5*vx/sx;
				wy = 0.5*vy/sy;
				wz = 0.5*vz/sz;
				xofs = wcen[0] - (cvec[0]/factor);
				yofs = wcen[1] - (cvec[1]/factor);
				wswin.llf.x = xofs - wx; 
				wswin.urb.x = xofs + wx;
				wswin.llf.y = yofs - wy; 
				wswin.urb.y = yofs + wy;
				wswin.llf.z = ug_gksstli.vtran[n].window.llf.z;
				wswin.urb.z = ug_gksstli.vtran[n].window.urb.z;
				gswindow3(n,&wswin);
/*
........Calculate new viewport center
*/
				delta[0] = (ug_gksstli.vtran[n].window.urb.x +
						ug_gksstli.vtran[n].window.llf.x) / 2.0;
				delta[1] = (ug_gksstli.vtran[n].window.urb.y +
						ug_gksstli.vtran[n].window.llf.y) / 2.0;
				delta[2] = (ug_gksstli.vtran[n].window.urb.z +
						ug_gksstli.vtran[n].window.llf.z) / 2.0;
				um_cross(vup, vpn, xaxis);
				um_vctmsc(xaxis, delta[0], xaxis);
				um_vctmsc(vup, delta[1], yaxis);
				um_vctmsc(vpn, delta[2], zaxis);
				vpcen[0] = ug_gksstli.vtran[n].vrefpt.x;
				vpcen[1] = ug_gksstli.vtran[n].vrefpt.y;
				vpcen[2] = ug_gksstli.vtran[n].vrefpt.z; 
				
				um_vcplvc(vpcen, xaxis, vpcen);
				um_vcplvc(vpcen, yaxis, vpcen);
				um_vcplvc(vpcen, zaxis, vpcen); 
				old_pet = pet;
				break;
			case STEPZOOM:
				if ((UZ_nclipv_view != 1)&&(MSLite==0)&&(LW_nclipv!=LW_STANDALONE))
				{
					uv_getvpid(UV_act_screen[0].vports[n-1],&vport);
				}
				else
				{
					uv_getvpid(LW_vport.key, &vport);
				}
				givpn3(vport.xform, &pt3);
				uv_getvid(vport.cur_view, &UV_dynview);
				UV_dynview.cur_pln_norm[0] = pt3.x;
				UV_dynview.cur_pln_norm[1] = pt3.y;
				UV_dynview.cur_pln_norm[2] = pt3.z;
				vup3 = gqvup3(vport.xform);
				UV_dynview.cur_up_vect[0] = (*vup3).x;
				UV_dynview.cur_up_vect[1] = (*vup3).y;
				UV_dynview.cur_up_vect[2] = (*vup3).z;
				window = gqwindow3(vport.xform);
				UV_dynview.cur_aperture = (window->urb.x - window->llf.x);
				UV_dynview.cur_ref_pt[0] = vpcen[0];
				UV_dynview.cur_ref_pt[1] = vpcen[1];
				UV_dynview.cur_ref_pt[2] = vpcen[2];
				uv_putv(&UV_dynview);
				uv_updatevp(&vport, UU_FALSE);
/*
.....Calculate viewport extents
*/
				vx = ug_gksstli.vtran[n].vport.urb.x - ug_gksstli.vtran[n].vport.llf.x;
				vy = ug_gksstli.vtran[n].vport.urb.y - ug_gksstli.vtran[n].vport.llf.y;
				vz = ug_gksstli.vtran[n].vport.urb.z - ug_gksstli.vtran[n].vport.llf.z;
				vcx = ug_gksstli.vtran[n].vport.llf.x + vx/2.0;
				vcy = ug_gksstli.vtran[n].vport.llf.y + vy/2.0;
				vcz = ug_gksstli.vtran[n].vport.llf.z + vz/2.0;
/*
.....Calculate window extents
*/
				wx = ug_gksstli.vtran[n].window.urb.x - ug_gksstli.vtran[n].window.llf.x;
				wy = ug_gksstli.vtran[n].window.urb.y - ug_gksstli.vtran[n].window.llf.y;
				wz = ug_gksstli.vtran[n].window.urb.z - ug_gksstli.vtran[n].window.llf.z;
				wcx = ug_gksstli.vtran[n].window.llf.x + wx/2.0;
				wcy = ug_gksstli.vtran[n].window.llf.y + wy/2.0;
				wcz = ug_gksstli.vtran[n].window.llf.z + wz/2.0;
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

				factor = pow(2.0,-(delx));
				sx = sxinit * factor;
				sy = syinit * factor;
				sz = szinit * factor;
				wx = 0.5*vx/sx;
				wy = 0.5*vy/sy;
				wz = 0.5*vz/sz;
				xofs = wcen[0] - (cvec[0]/factor);
				yofs = wcen[1] - (cvec[1]/factor);
				wswin.llf.x = xofs - wx; 
				wswin.urb.x = xofs + wx;
				wswin.llf.y = yofs - wy; 
				wswin.urb.y = yofs + wy;
				wswin.llf.z = ug_gksstli.vtran[n].window.llf.z;
				wswin.urb.z = ug_gksstli.vtran[n].window.urb.z;
				gswindow3(n,&wswin);
/*
........Calculate new viewport center
*/
				delta[0] = (ug_gksstli.vtran[n].window.urb.x +
						ug_gksstli.vtran[n].window.llf.x) / 2.0;
				delta[1] = (ug_gksstli.vtran[n].window.urb.y +
						ug_gksstli.vtran[n].window.llf.y) / 2.0;
				delta[2] = (ug_gksstli.vtran[n].window.urb.z +
						ug_gksstli.vtran[n].window.llf.z) / 2.0;
				um_cross(vup, vpn, xaxis);
				um_vctmsc(xaxis, delta[0], xaxis);
				um_vctmsc(vup, delta[1], yaxis);
				um_vctmsc(vpn, delta[2], zaxis);		
				vpcen[0] = ug_gksstli.vtran[n].vrefpt.x;
				vpcen[1] = ug_gksstli.vtran[n].vrefpt.y;
				vpcen[2] = ug_gksstli.vtran[n].vrefpt.z; 				
				um_vcplvc(vpcen, xaxis, vpcen);
				um_vcplvc(vpcen, yaxis, vpcen);
				um_vcplvc(vpcen, zaxis, vpcen); 
				mb[3] = mb[4] = 0;
				old_pet = pet;
				break;
			}
/*
...........If rotating about anything other
...........than viewport center, then store
...........new viewport center
...........Not valid when PANning or ZOOMing
*/			
			if (pet != PAN && pet != NONE)
			{
				pt[0] = ug_gksstli.vtran[n].vrefpt.x;
				pt[1] = ug_gksstli.vtran[n].vrefpt.y;
				pt[2] = ug_gksstli.vtran[n].vrefpt.z;
				switch (pet)
				{
				case XROT:
				case YROT:
				case XYROT:
				case TUMBLE:
					um_rotatept(vpcen,vrt,cen,phix,1,tm2);
					um_rotatept(vpcen,vup,cen,phiy,1,tm2);
					um_rotatept(pt,vrt,cen,phix,1,tm2);
					um_rotatept(pt,vup,cen,phiy,1,tm2);
					break;
				case ZROT:
					um_rotatept(vpcen,&tm[2][0],cen,phiy-phix,1,tm2);
					um_rotatept(pt,&tm[2][0],cen,phiy-phix,1,tm2);
					break;
				case VECROT:
					um_rotatept(vpcen,UV_dynvec,cen,phiy-phix,1,tm2);
					um_rotatept(pt,UV_dynvec,cen,phiy-phix,1,tm2);
					break;
				}
				gsvref3(n,pt);
/*
...........Store new VUP, VPN, and VRT vectors
*/
				for (i=0;i<3;i++)
				{
					vrt[i] = tm[0][i];
					vup[i] = tm[1][i];
					vpn[i] = tm[2][i];
				}
 				gsvpn3(n, vpn);
 				gsvup3(n, vup);
			}
/*
........Find current rotations
*/
		 	if (pet == XYROT || pet == XROT || pet == YROT)
			{
				uw_glcvpn(vpn,&rotx,&roty);
				rotx *= -(RTOD);
				roty *= RTOD;
			}
			else if (pet == ZROT || pet == VECROT)
			{
				rotx = rotx + (delx - dely);
			}
/*
........Redraw the part
*/
			if ((UZ_nclipv_view != 1)&&(MSLite==0)) 
				uw_glload_matrix(n,UU_FALSE,UU_NULL);
			if ((UV_dyndisply == 0 || UV_dyndisply == 2) && pet != NONE)
			{
				if ((UZ_nclipv_view == 1)||(MSLite)|| (LW_nclipv==LW_STANDALONE))
				{
					ul_ipv_view_same(n);
				}
				else
				{
					glCallList_d(view_obj);   
					uw_gllighting_reset();
					if (LW_active && UZ_nclipv_view == 2 && n == UV_current_sview)
					{
						ul_ipv_view_same(n);
					}
				}
			}
/*
........Redraw the axis
*/
			if ((UZ_nclipv_view != 1)&&(MSLite==0)) 
			{
				if ((UV_dyndisply == 1 || UV_dyndisply == 2) && pet != NONE)
				{
					uw_gldynaxis(n, cen,axlen,ug_gksstli.vtran[n],llax,urax,ll,ur);
				} 
			}
/*
.......Echo the rotations to the error line
*/
			if (UV_dynstatln == 1 && locpt->pet != MOUSE)
			{
				switch (pet)
				{
					case XROT:
					case YROT:
					case XYROT:
						sprintf(str,"X Rotation: %7.2f,  Y Rotation: %7.2f", 
							rotx, roty);
						break;
					case ZROT:
						sprintf(str,"Z Rotation: %7.2f",rotx);
						break;
					case TUMBLE:
						sprintf(str,"X Speed: %7.2f,  Y Speed: %7.2f",
							delx,-(dely));
						break;
					case VECROT:
						sprintf(str,"Vector Rotation: %7.2f",rotx);
						break;
					case PAN:
						sprintf(str,"Window Center: X=%7.2f  Y=%7.2f",rotx,roty);
						break;
					case ZOOM:
						sprintf(str,"Scale: %7.2f",factor);
						break;
				}
/*
........Display status line
*/
				ud_prmerr(str);
			}
			if ((UZ_nclipv_view != 1)&&(MSLite==0) )
				uw_glswapbuffer(&Svprect);
		}
		if (out_loop==1)
			break;
		if (need_break==1)
			break;
	}
loopnd:;
/*
.....Restore blank cursor
*/
#if UU_COMP!=UU_WIN2K
	XUngrabPointer(uw_xw.disp,CurrentTime);
	uw_mfsetcursor(21);
#else
	uw_ntrelease_capture();
	uw_ntsetcursor(21);
#endif
/*
.....Save view center point
*/
/*
......Recalculate new viewport center here because we could be many activities 
......during mouse view 
*/			
	delta[0] = (ug_gksstli.vtran[n].window.urb.x +
				ug_gksstli.vtran[n].window.llf.x) / 2.0;
	delta[1] = (ug_gksstli.vtran[n].window.urb.y +
				ug_gksstli.vtran[n].window.llf.y) / 2.0;
	delta[2] = (ug_gksstli.vtran[n].window.urb.z +
				ug_gksstli.vtran[n].window.llf.z) / 2.0;

	um_cross(vup, vpn, xaxis);
	um_vctmsc(xaxis, delta[0], xaxis);
	um_vctmsc(vup, delta[1], yaxis);
	um_vctmsc(vpn, delta[2], zaxis);
	vpcen[0] = ug_gksstli.vtran[n].vrefpt.x;
	vpcen[1] = ug_gksstli.vtran[n].vrefpt.y;
	vpcen[2] = ug_gksstli.vtran[n].vrefpt.z; 
	um_vcplvc(vpcen, xaxis, vpcen);
	um_vcplvc(vpcen, yaxis, vpcen);
	um_vcplvc(vpcen, zaxis, vpcen);
	if ((UZ_nclipv_view == 1)||(MSLite)||(LW_nclipv==LW_STANDALONE))
	{
		LW_dyncp[0] = vpcen[0];
		LW_dyncp[1] = vpcen[1];
		LW_dyncp[2] = vpcen[2];
	}
	else
	{
		UV_dyncp[0] = vpcen[0];
		UV_dyncp[1] = vpcen[1];
		UV_dyncp[2] = vpcen[2];
	}
/*
.....Clear the prompt area
*/
	ud_prmerr(" ");
	if (verify_save)
		NCL_pick_verify = 1;

/*
.....Restore menu availability
*/
	formActive = UU_FALSE;
/*
.....added scissor test
.....Yurong 11/11/98
*/
	if ((UV_dyndisply == 0 || UV_dyndisply == 2) && UZ_nclipv_view != 1 && MSLite==0)
		uw_glset_scissor(sav_clip);
	if (view_obj!=0)
		glDeleteLists_d(view_obj, 1);
	view_obj = 0;
	UV_Cur_Dyn = 0;
	NCL_mouse_func = 0;
}

/*********************************************************************
**    I_FUNCTION     : uw_gldynaxis(cen,axlen,tran,ll,ur,vll,vur,w,h)
**
**		Displays the viewport axis at the center of rotation for
**		dynamic viewing.  The positive axes will be a solid line and
**		contain the axis letter (XYZ).  The negative axes will be a
**		dotted line.  The axes pointing out of the screen towards the
**		user will be White.  The axes pointing into the screen will
**		be red.
**
**    PARAMETERS   
**       INPUT  : 
**			cen   = Center of rotation.
**			axlen = Length of drawn axes.
**			tran  = Active transformation.
**			ll    = Lower left of axis extents in DC.
**			ur    = Upper right of axis extents in DC.
**			vll   = Lower left of viewport in DC.
**			vur   = Upper right of viewport in DC.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gldynaxis(n, cen,axlen,tran,ll,ur,vll,vur)
Gfloat cen[],axlen;
int n, ll[],ur[],vll[],vur[];
UG_vtran3 tran;
{
	int i,indx, ill[2],iur[2],savemask,pushed;
	Gfloat wvec[3],lx[3],fll[3],fur[3],axl[3];
	Gfloat wid=1.;
	Gnpoint3 np1,np2;
	char *axstr[3] = {"X","Y","Z"};

	savemask = uw_glget_depthmask();
	uw_gldepth_mask(0);
/*
.....Store window normal vector
*/
	wvec[0] = tran.vpnorm.x;
	wvec[1] = tran.vpnorm.y;
	wvec[2] = tran.vpnorm.z;
	ncl_mcstowcs(1,wvec,wvec);
/*
.....Setup extents of axis box
*/
	ll[0] =  10000; ll[1] =  10000;
	ur[0] = -10000; ur[1] = -10000;
/*
.....Loop to draw each axis
*/
	pushed = uw_gllighting(UU_FALSE);
	for (i=0;i<3;i++)
	{
/*
........Draw the Positive axis
*/
		lx[0] = cen[0];
		lx[1] = cen[1];
		lx[2] = cen[2];
		if (wvec[i] < .9999 && wvec[i] > -.9999)
		{
			axl[0] = 0; axl[1] = 0; axl[2] = 0; axl[i] = axlen;
			ncl_mcstowcs(1,axl,axl);
			lx[0] = lx[0] + axl[0];
			lx[1] = lx[1] + axl[1];
			lx[2] = lx[2] + axl[2];
		}
		indx  = 1; /*BASE_COLOR[1].index;*/   /*WHITE*/
		if (wvec[i] < 0.) indx = 3; /*BASE_COLOR[3].index;*/  /*Red*/
		uw_glcolor(indx);
		uw_gllinetype(1, 0);
		uw_gllinewidth(wid, 0);	
		glBegin_d(GL_LINES);
			glVertex3f_d(cen[0],cen[1],cen[2]);
			glVertex3f_d(lx[0],lx[1],lx[2]);
		glEnd_d();
		glRasterPos3f_d(lx[0],lx[1],lx[2]);
		uw_glprintString(axstr[i],uw_gltext_base);
/*
...........Modify the axis box extents
*/
		UG_XFORM(cen[0],cen[1],cen[2],&np1,ug_cxform[n]);
		UG_XFORM(lx[0],lx[1],lx[2],&np2,ug_cxform[n]);
		fll[0] = np1.x; fll[1] = np1.y;
		fur[0] = np2.x; fur[1] = np2.y;
		uw_glndctodev(fll,ill);
		uw_glndctodev(fur,iur);
		if (ill[0] < ll[0]) ll[0] = ill[0];
		if (ill[0] > ur[0]) ur[0] = ill[0];
		if (ill[1] < ll[1]) ll[1] = ill[1];
		if (ill[1] > ur[1]) ur[1] = ill[1];
		if (iur[0] < ll[0]) ll[0] = iur[0];
		if (iur[0] > ur[0]) ur[0] = iur[0];
		if (iur[1] < ll[1]) ll[1] = iur[1];
		if (iur[1] > ur[1]) ur[1] = iur[1];
/*
........Draw the Negative axis
*/
		lx[0] = cen[0];
		lx[1] = cen[1];
		lx[2] = cen[2];
		if (wvec[i] < .9999 && wvec[i] > -.9999)
		{
			lx[0] = lx[0] - axl[0];
			lx[1] = lx[1] - axl[1];
			lx[2] = lx[2] - axl[2];
		}
		indx  = 1; /*BASE_COLOR[1].index;*/
		if (wvec[i] > 0.) indx = 3; /*BASE_COLOR[3].index;*/
		uw_glcolor(indx);
		uw_gllinetype(5, 0);
		uw_gllinewidth(wid, 0);	
		glBegin_d(GL_LINES);
			glVertex3f_d(cen[0],cen[1],cen[2]);
			glVertex3f_d(lx[0],lx[1],lx[2]);
		glEnd_d();
/*
...........Modify the axis box extents
*/
		UG_XFORM(lx[0],lx[1],lx[2],&np2,ug_cxform[n]);
		fur[0] = np2.x; fur[1] = np2.y;
		uw_glndctodev(fur,iur);
		if (iur[0] < ll[0]) ll[0] = iur[0];
		if (iur[0] > ur[0]) ur[0] = iur[0];
		if (iur[1] < ll[1]) ll[1] = iur[1];
		if (iur[1] > ur[1]) ur[1] = iur[1];
	}
	uw_gllighting(pushed);
	uw_gldepth_mask(savemask);
	glDisable_d(GL_LOGIC_OP,0,0);
/*
.....reset line type to 1
*/
	uw_gllinetype(1, 0);	
	uw_gllinewidth(1.0, 0);
}

/*********************************************************************
**    I_FUNCTION     : uw_gldyndraw(n,ll,ur,erase)
**
**		Redraws the current viewport when during dynamic viewing.
**		Supports double buffering.
**
**    PARAMETERS   
**       INPUT  : 
**			n     = Active transformation.
**			ll    = Lower left of viewport in DC.
**			ur    = Upper right of viewport in DC.
**			erase = Erase segment flag.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gldyndraw(n,ll,ur,erase)
int n,ll[2],ur[2],erase;
{
	unsigned mask;
	UG_segstli *p;
	int i;
	UV_vport vport;
/*
.....Create bit mask for XFORM of segment header
*/
	mask = (1 << n) | UG_SEGINNTRAN;
/*
.....Clear screen
*/
	uw_gllighting_reset();
	uw_glrasput(ll,ur,NULL,ug_segerasecolor);  
/*
.....Initialize GKS segment traverser
*/
	ug_seginitscan();
	uv_getvpid(UV_act_screen[0].vports[n-1],&vport);
	i = 0;
	while ((p=ug_segscan()) != NULL)
	{
/*
........Display motion
*/
		if (p->segid == NCL_mot_seg && p->segatts.gvis == UG_VISIBLE)
		{
			ncl_reset_cutseg(n-1);  
			dyn_view = 1;
			ncl_display_motion(n,0,0,1,UU_TRUE,UU_FALSE,UU_NULL);
			dyn_view = 0;
			i++;
		}
/*
........Display digs graphics segment
*/
		
		else if (p->segatts.gvis == UG_VISIBLE && (p->xforms & mask) &&
			p->segid != vport.bord_seg)
		{
			uw_glviewsg(p->segid);
			i++;
		}
/*
.....UV_dynsegs means visible segments
.....Yurong 2/15/99
*/
/*
		i++;
*/
		if (i > UV_dynsegs) break;
	}
}

/*********************************************************************
**    E_FUNCTION     : uw_gldynlight(which)
**
**		Dynamically changes a light's position.
**
**    PARAMETERS   
**       INPUT  : 
**			   which  = Light number to change position of.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gldynlight(which)
int which;
{
	UG_segstli *p;
	Gnrect3 rect3;
	int xy[2],irtn,k;
	int ipos[2];
#if UU_COMP == UU_WIN2K
	int ispos[2];
#endif
	Gfloat npos[2],savpos[3];
	UM_coord inppos;
	Gfloat zmax, zmin;
	GLuint lgtview_obj = 0;
	struct UM_light_rec e;
/*
.....Get active light
*/
	e.key = UM_light_keys[which];
	uc_retrieve_data(&e,sizeof(struct UM_light_rec));
/*
.....Set up light's position
*/
	npos[0] = savpos[0] = e.position[0];
	npos[1] = savpos[1] = e.position[1];
	savpos[2] = e.position[2];
	uw_glndctodev(npos,ipos);
/*
.....Mover cursor to light position
*/
#if UU_COMP!=UU_WIN2K
	uw_mfsetcursor(2);
	XWarpPointer(uw_xw.disp,None,uw_xw.wd_id,0,0,0,0,
		ipos[0],ipos[1]);
#else
	uw_ntsetcursor(2);
	uw_ntcurdc_toscrn(UZ_nclipv_view, ipos[0], ipos[1], &ispos[0], &ispos[1]);
	uw_ntset_curpos(ispos[0],ispos[1]);
#endif
/*
.....Initialize light's Z-position
*/
	zmin = 100000;
	zmax = -100000;
/*
.....Create list of segments
*/
	lgtview_obj = glGenLists_d(1);
	glNewList_d(lgtview_obj, GL_COMPILE);
	uw_glclearws();
	ug_seginitscan();                                 
	while( (p=ug_segscan()) != NULL  ) 
	{
		if(p->segatts.gvis == UG_VISIBLE)
		{
			if (p->wcboxok==1)
			{
				ug_segndcbox3(p->segid,&rect3);
				if (rect3.urb.z>zmax)
					zmax = rect3.urb.z;
				if (rect3.llf.z<zmin)
					zmin = rect3.llf.z;
			}
			uw_glviewsg(p->segid);
		}
/*
........Draw segment hilite symbol?
*/
		if( p->segatts.ghilit==UG_HIGHLIGHTED )
			uw_glviewsg(UG_MAXSEGNO+p->segid+1);
	}
	ncl_display_motion(-1,0,0,0,UU_TRUE,UU_FALSE,UU_NULL);
	glEndList_d();
/*
.....Draw in back buffer
*/
	uw_gldrawbuffer(UG_BACK_BUFFER);
/*
.....Set Z-position
*/
	if (zmax+0.5>1.0) zmax = 1.0;
	else
		zmax += 0.5;
	inppos[2] = zmax; 
/*
.....Loop for mouse movement
*/	
	while (UU_TRUE)
	{
#if UU_COMP!=UU_WIN2K
		irtn = uw_mfevent(&k,0,xy,0);
#else
		irtn = uw_ntevent(&k,1,xy,1);
		if (UZ_nclipv_view == 1)
			uw_ntset_ipv_capture();
		else
			uw_ntset_gwcapture();
#endif
		if (irtn!=0)
		{
			if (k==3)
				break;
			if ((xy[0]==ipos[0])&&(xy[1]==ipos[1]))
				break;
		}
		else if ((xy[0]==ipos[0])&&(xy[1]==ipos[1]))
			continue;
		ipos[0] = xy[0];
		inppos[0] = (UU_REAL)ipos[0];
		ipos[1] = xy[1];
		inppos[1] = (UU_REAL)ipos[1];

		ipos[0] = (int)inppos[0];
		ipos[1] = (int)inppos[1];
		uw_gldevtondc(ipos,inppos);
/*
.....Define light's position
*/
		e.position[0] = inppos[0];
		e.position[1] = inppos[1];
		e.position[2] = inppos[2];
		ur_update_data_fixed(&e);
/*
.....Position lights
*/
		uw_gllight_pos(UU_TRUE);
		glCallList_d(lgtview_obj);
		uw_glupdate_front(UG_PERFORM);
		if (irtn!=0) break;
	}
/*
.....Restore initial light position
.....On Reject Op
*/
	if (k == 3)
	{
		e.position[0] = savpos[0];
		e.position[1] = savpos[1];
		e.position[2] = savpos[2];
		ur_update_data_fixed(&e);
		uw_gllight_pos(UU_TRUE);
		glCallList_d(lgtview_obj);
		uw_glupdate_front(UG_PERFORM);
	}
/*
.....Delete viewing list
*/
	if (lgtview_obj!=0) glDeleteLists_d(lgtview_obj, 1);
/*
.....Release mouse & set cursor
*/
#if UU_COMP!=UU_WIN2K
	uw_mfsetcursor(21);
#else
	uw_ntrelease_capture();
	uw_ntsetcursor(21);
#endif
}
/*********************************************************************
**    I_FUNCTION     : uw_glredraw_dynvw()
**
**		Redraw the dynamic view graphics by calling the dynamic viewing openGL list
**
**    PARAMETERS   
**       INPUT  : 
**			none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glredraw_dynvw()
{
	int llax[2], ll[2], ur[2];
	Gfloat axlen;
	Gfloat cen[3];

	if (current_dyn_view==-1)
		return;
/*
.....Store center of rotation
*/
	cen[0] = LW_dyncp[0];
	cen[1] = LW_dyncp[1];
	cen[2] = LW_dyncp[2];
	llax[0] = 10000;
	axlen = (ug_gksstli.vtran[current_dyn_view].window.urb.y -
		ug_gksstli.vtran[current_dyn_view].window.llf.y) / 5.;
/*
.....Calculate Pixel size of viewport
*/
	uw_glndctodev(&ug_gksstli.vtran[current_dyn_view].vport.llf, ll);
	uw_glndctodev(&ug_gksstli.vtran[current_dyn_view].vport.urb, ur);
/*
........Redraw the part
*/
	ul_ipv_view_same(current_dyn_view);
}

#endif
