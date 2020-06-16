/*********************************************************************
**  FILENAME: mslipview.c
**  CONTAINS:   
**				msl_resize_window()
**              msl_reset_view()
**				ul_ipv_view_segs    ....Those 5 functions use same name as NCL, 
**				uv_setxform()       ....but they are diff from NCL to fit 
**              ul_ipv_set_context()....MSLITE
**				ul_ipv_view_same()
**              ul_ipv_view_extrema()
**				ul_ipv_open_window() .... point to use MSLITE function
**				uz_dyn_mouse() .... point to use MSLITE function
**				ms_create_view()
**				ms_create_vport()
**				msl_aspect_ratio()
**				msl_calcvp()
**				msl_update_pre_view(view)
**				msl_reset_prev()
**				msl_vfit()
**				msl_window_view()
**				msl_view_from_axis()
**				msl_dynmouse
**				msl_dynamic
**
**    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        mslipvview.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:13:00
*********************************************************************/
#include <stdio.h>
#include "zsysdep.h"
#include "xenv1.h"
#include "usysg.h"
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
#include "view1.h"
extern char *LW_window;

extern int PKx,PKy;
extern int LW_dyncenter;
extern UU_REAL LW_dyncp[3];
extern int UV_dynview_active;
extern int UV_Cur_Dyn;

void ul_ipv_view_same();
void ul_ipv_view_segs();
void ul_ipv_flush();
void uv_setxform();
void msl_update_pre_view();
void msl_aspect_ratio();
void msl_calcvp();
/*
.....MSL_view[0] for original view
.....MSL_View[1...n] for different view defined
*/
UV_view MSL_view[20];
UV_view UV_dynview;
/*
.....current view active, defalt to front
*/
int MS_current_view = 0;
Gwrect3	MSL_window;
Gnrect MSL_area;
extern UW_glmatrix ul_ipv_matrix;
UU_REAL UM_model_size = 11.0;		/* 11.0 inches */
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
extern int UV_dyncenter;
/*********************************************************************
**	 E_FUNCTION: void msl_reset_view(xform)
**		This function saves a copy of the current NCLIPV view and then
**		changes this view to match the requested viewport.
**	 PARAMETERS	
**		 INPUT  : xform   = Specifies which NCL transform to copy.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void msl_reset_view(xf)
int xf;
{
	int n;
/*
.....Save the current view
*/
	n = LW_vport.cur_view;
	if (n!=0)
/*
.....copy default view to n
*/
	{
		MSL_view[0].key = MSL_view[n].key;
		MSL_view[0].rel_num = MSL_view[n].rel_num;
		strcpy(MSL_view[0].name,MSL_view[n].name);

		MSL_view[0].vtype = MSL_view[n].vtype;
		MSL_view[0].can_save = MSL_view[n].can_save;
		MSL_view[0].modified = MSL_view[n].modified;

		MSL_view[0].projection = MSL_view[n].projection;
		MSL_view[0].sav_eye_dist = MSL_view[n].sav_eye_dist;
		MSL_view[0].cur_eye_dist = MSL_view[n].cur_eye_dist;
		MSL_view[0].sav_ref_pt[0] = MSL_view[n].sav_ref_pt[0]; 
		MSL_view[0].sav_ref_pt[1] = MSL_view[n].sav_ref_pt[1]; 
		MSL_view[0].sav_ref_pt[2] = MSL_view[n].sav_ref_pt[2]; 
		MSL_view[0].cur_ref_pt[0] = MSL_view[n].cur_ref_pt[0]; 
		MSL_view[0].cur_ref_pt[1] = MSL_view[n].cur_ref_pt[1]; 
		MSL_view[0].cur_ref_pt[2] = MSL_view[n].cur_ref_pt[2]; 
		MSL_view[0].sav_up_vect[0] = MSL_view[n].sav_up_vect[0]; 
		MSL_view[0].sav_up_vect[1] = MSL_view[n].sav_up_vect[1]; 
		MSL_view[0].sav_up_vect[2] = MSL_view[n].sav_up_vect[2]; 
		MSL_view[0].cur_up_vect[0] = MSL_view[n].cur_up_vect[0]; 
		MSL_view[0].cur_up_vect[1] = MSL_view[n].cur_up_vect[1]; 
		MSL_view[0].cur_up_vect[2] = MSL_view[n].cur_up_vect[2]; 
		MSL_view[0].sav_pln_norm[0] = MSL_view[n].sav_pln_norm[0]; 
		MSL_view[0].sav_pln_norm[1] = MSL_view[n].sav_pln_norm[1]; 
		MSL_view[0].sav_pln_norm[2] = MSL_view[n].sav_pln_norm[2]; 
		MSL_view[0].cur_pln_norm[0] = MSL_view[n].cur_pln_norm[0]; 
		MSL_view[0].cur_pln_norm[1] = MSL_view[n].cur_pln_norm[1]; 
		MSL_view[0].cur_pln_norm[2] = MSL_view[n].cur_pln_norm[2]; 

		MSL_view[0].sav_aperture = MSL_view[n].sav_aperture;
		MSL_view[0].cur_aperture = MSL_view[n].cur_aperture;
		MSL_view[0].sav_front_clip = MSL_view[n].sav_front_clip;
		MSL_view[0].sav_back_clip = MSL_view[n].sav_back_clip;
		MSL_view[0].cur_front_clip = MSL_view[n].cur_front_clip;
		MSL_view[0].cur_back_clip = MSL_view[n].cur_back_clip;
		MSL_view[0].sav_do_clip = MSL_view[n].sav_do_clip;
		MSL_view[0].cur_do_clip = MSL_view[n].cur_do_clip;
	}
	msl_update_pre_view(MSL_view[n]);
/*
.....Change the NCLIPV view
*/
	LW_vport.cur_view = xf;
	MS_current_view = xf;
	uv_setxform();
	msl_vfit(0);
}
/*********************************************************************
**	 E_FUNCTION: void ul_ipv_view_same(xf)
**		This function have little changes from NCL ul_ipv_view_same function.
**			implement for MSLITE (it have to use same name as NCL because
**			we will use many NCLIPV fucntion to display MSLITE
**	 PARAMETERS	
**		 INPUT  : xf   = view number.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_view_same(xf)
int xf;
{
	LtMatrix mx;
	int i,j,x,y;
	UU_REAL a,xa,ya;

	if (xf!=LW_vport.cur_view)
	{
		LW_vport.cur_view = xf;
		MS_current_view = xf;
		uv_setxform();
	}
	uw_glload_matrix(xf, UU_FALSE,UU_NULL);
/*
.....Define X to Y screen ratio
*/
	x = PKx;
	y = PKy;
	if (x > y)
	{
		a = (double)x / (double)y;
		xa = (a - 1.) * .5;
		ya = 0.;
	}
	else
	{
		a = (double)y / (double)x;
		xa = 0.;
		ya = (a - 1.) * .5;
	}
	for (i=0;i<4;i++)
	{
		for (j=0;j<4;j++)
		{
			mx[i][j] = ul_ipv_matrix.matrix[4*i+j];
		}
	}
/*
.....Adjust scale factors for X-Y Ratio
*/
	for(i=0;i<3;i++)
		for (j=0;j<3;j++)mx[i][j] = mx[i][j]*a;
/*
.....NCLIPV matrix is YZ-rotated
.....compared to NCL Matrix
*/
	mx[0][1] = mx[0][1] * -1.;
	mx[1][1] = mx[1][1] * -1.;
	mx[2][1] = mx[2][1] * -1.;
	mx[0][2] = mx[0][2] * -1.;
	mx[1][2] = mx[1][2] * -1.;
	mx[2][2] = mx[2][2] * -1.;
/*
.....Adjust center point for XY ratio
*/
	mx[3][0] = (mx[3][0]*a) - xa;
	mx[3][1] = (1. - (mx[3][1]*a-ya));
/*
.....Define the MachineWorks matrix
*/
//	LiViewSetMatrices(LI_VIEW_CURRENT,&mx,0);
	LiViewSetMatrices(LW_view,&mx,0);
	if (LW_display == LW_PASSIVE)
//		LiViewSetResolution(LI_VIEW_CURRENT, PKx, PKy, 1.0);
		LiViewSetResolution(LW_view, PKx, PKy, 1.0);
/*
.....Enable auto clipping
*/
//	LiMWViewportAutoClipping(LI_VIEW_CURRENT);
/*
.....Flush the graphics
*/
	ul_ipv_flush();
}
/**************************************************************************
**  E_FUNCTION:  uv_update_up_vect(view, up_vect)
**      Update the up vector for this view in the current portion 
**			of the view record
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						up_vect	: view up vector to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_up_vect(view, up_vect)
	UV_view		*view;
	UM_vector	up_vect;

	{
	UM_vector vup;

	uu_denter(UU_MTRC,(us,"uv_update_up_vect(key=%x)",view->key));
	um_unitvc(up_vect, vup);
	um_vctovc(vup, view->cur_up_vect);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_update_pln_norm(view, pln_norm)
**      Update the view plane normal for this view in the current
**			portion of the view record
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						pln_norm	: view plane normal to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_pln_norm(view, pln_norm)
	UV_view		*view;
	UM_vector	pln_norm;

	{
	UM_vector vpn;

	uu_denter(UU_MTRC,(us,"uv_update_pln_norm(key=%x)",view->key));
	um_unitvc(pln_norm, vpn);
	um_vctovc(vpn, view->cur_pln_norm);
	uu_dexit;
	}
/**************************************************************************
**  E_FUNCTION:  uv_update_vaperture(view, aperture)
**      Update the aperture for this view in the current portion of the
**			view record
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						aperture	: length of window in "x" axis
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_vaperture(view, aperture)
	UV_view  *view;
	UU_REAL	aperture;
	{

	uu_denter(UU_MTRC,(us,"uv_update_vaperture(key=%x)",view->key));
	view->cur_aperture = aperture;
	uu_dexit;
	}
/**************************************************************************
**  E_FUNCTION:  uv_update_ref_pt(view, ref_pt)
**      Update the reference point for this view in the current portion
**			of the view record
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						ref_pt: view reference point to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_ref_pt(view, ref_pt)
	UV_view		*view;
	UM_coord		ref_pt;

	{
	uu_denter(UU_MTRC,(us,"uv_update_ref_pt(key=%x)",view->key));
	um_vctovc(ref_pt, view->cur_ref_pt);
	uu_dexit;
	}

/*********************************************************************
**	 I_FUNCTION: S_add_box(bounds,box,ifl,tf)
**		Updates a bounding box with the bounding box of an IPV solid.
**		Optionally applies a matrix to the solid bounding box.
**	 PARAMETERS	
**		 INPUT  :
**        bounds  = Bounding box of IPV solid.
**        box     = Bounding box to be updated.
**        ifl     = UU_TRUE - apply matrix to solid bounding box.
**        tf      = Matrix to apply to bounding box.
**		 OUTPUT :
**        box     = Updated bounding box.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
static void S_add_box(bounds,box,ifl,tf)
LtDoubleBounds bounds;
Gwrect3 *box;
UU_LOGICAL ifl;
UM_transf tf;
{
	UM_coord pt;
/*
.....Update box per input point
*/
	pt[0] = bounds[LI_MINX];
	pt[1] = bounds[LI_MINY];
	pt[2] = bounds[LI_MINZ];
	if (ifl) um_cctmtf(pt,tf,pt);
	if (pt[0] < box->llf.x) box->llf.x = pt[0];
	if (pt[1] < box->llf.y) box->llf.y = pt[1];
	if (pt[2] < box->llf.z) box->llf.z = pt[2];
	pt[0] = bounds[LI_MAXX];
	pt[1] = bounds[LI_MAXY];
	pt[2] = bounds[LI_MAXZ];
	if (ifl) um_cctmtf(pt,tf,pt);
	if (pt[0] > box->urb.x) box->urb.x = pt[0];
	if (pt[1] > box->urb.y) box->urb.y = pt[1];
	if (pt[2] > box->urb.z) box->urb.z = pt[2];
}
/*********************************************************************
**	 E_FUNCTION:int ul_ipv_set_context()
**		Set the graphic context to the MSLITE window.
**		Little changed from NCL function
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_set_context()
{
	if (LW_active)
	{;
//		LiOGLDrvWindowSetCurrent((HWND)LW_window);
	}
	else
		msl_win_context();
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_view_segs()
**		Displays all NCL generated segments in the NCLIPV window.
**	 PARAMETERS	
**		 INPUT  :
**        flush   = UU_TRUE = Flush out any defered graphics.
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_view_segs()
{
	int nr1, fr1;
	nr1=-1000; fr1=1000;
	if (LW_active)
	{
		LiViewSetClipping(LW_view,nr1,fr1);
//		LiMoViewSet(UU_NULL);
	}
	msl_win_context();
	glClearColor_d((GLclampf)0,(GLclampf)0,(GLclampf)0,(GLclampf)0);
	glClearDepth_d((GLclampd)1.0);
	glClear_d(GL_DEPTH_BUFFER_BIT);
	glClear_d(GL_COLOR_BUFFER_BIT);
	glFlush_d();
	return;
}
/*********************************************************************
**	 E_FUNCTION:int msl_resize_window()
**		resize MSLITE window.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void msl_resize_window()
{
	if (!LW_active)	
	{
		ul_ipv_view_segs();
		return;
	}
	msl_setsize(PKx, PKy);
	uv_setxform();
	LiViewSetResolution(LW_view, PKx, PKy, 1.0);
	ul_ipv_set_background();
	ul_ipv_view_same(MS_current_view);
}
void ipv_resize_window()
{
	msl_resize_window();
}
/**************************************************************************
**  I_FUNCTION:  uv_setxform()
**      Use DIGS calls to set up normalization transformation.
**			Map the UU_REAL viewing data to GKS data types
**  PARAMETERS   
**      INPUT  :  vport	: viewport entity to use to set xform
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setxform()
{
	Gnrect3	viewport;
	int		xform;
	Gwpoint3	pt;
	Gfloat phix, phiy;
	GLdouble  x, y;
	UU_REAL aspect_ratio;

	GLdouble phiz;					/* Up vector rotation value */
	Gfloat vup[3], vup2[3];	/* Temporary view up vectors */
	Gtran a;						/* Temporary rotation matrix */
	Gfloat ug_atan2();
	float s[3], t[3];
	int xf = LW_vport.cur_view;

	pt.x = MSL_view[xf].cur_ref_pt[0];
	pt.y = MSL_view[xf].cur_ref_pt[1];
	pt.z = MSL_view[xf].cur_ref_pt[2];
	gsvref3(xf, &pt);
	ul_ipv_matrix.trn[0] = -pt.x;
	ul_ipv_matrix.trn[1] = -pt.y;
	ul_ipv_matrix.trn[2] = -pt.z;

	pt.x = MSL_view[xf].cur_pln_norm[0];
	pt.y = MSL_view[xf].cur_pln_norm[1];
	pt.z = MSL_view[xf].cur_pln_norm[2];
	gsvpn3(xf, &pt);
	uw_glcvpn(&pt, &phix, &phiy);
	x = phix * RTOD  ;
	y = phiy * RTOD  ;
	ul_ipv_matrix.vpn[0] = x;
	ul_ipv_matrix.vpn[1] = y;
	vup[0] = MSL_view[xf].cur_up_vect[0];
	vup[1] = MSL_view[xf].cur_up_vect[1];
	vup[2] = MSL_view[xf].cur_up_vect[2];
	gsvup3(xf, &vup);
	ug_ident(a);
	ug_vectoz(a, &(MSL_view[xf].cur_pln_norm));
	ug_vecxform(vup2, vup, a);
	phiz =  RTOD * ug_atan2(vup2[0], vup2[1]);	
	ul_ipv_matrix.vup = phiz;

	msl_aspect_ratio(&LW_vport, &aspect_ratio);
	MSL_window.llf.x = -(MSL_view[xf].cur_aperture/2.);
	MSL_window.llf.y = -(MSL_view[xf].cur_aperture*aspect_ratio/2.);
	MSL_window.llf.z = MSL_view[xf].cur_front_clip;
	MSL_window.urb.x = (MSL_view[xf].cur_aperture/2.);
	MSL_window.urb.y = (MSL_view[xf].cur_aperture*aspect_ratio/2.);
	MSL_window.urb.z = MSL_view[xf].cur_back_clip;
	gswindow3(xf, &MSL_window);
  	zbytecp(ug_gksstli.vtran[xf].window, MSL_window);
	uw_glw2v( &(MSL_window) , &(LW_vport) , s , t );
	ul_ipv_matrix.w2v[0] = t[0];
	ul_ipv_matrix.w2v[1] = t[1];
	ul_ipv_matrix.w2v[2] = t[2];
	ul_ipv_matrix.s1[0] = s[0];
	ul_ipv_matrix.s1[1] = s[1];
	ul_ipv_matrix.s1[2] = s[2];

	msl_calcvp(&LW_vport, &viewport);
		
	gsview3(xf, &viewport);
	zbytecp(ug_gksstli.vtran[xf].vport, viewport);

	uw_glw2v( &(MSL_window) , &viewport , s , t );
	ul_ipv_matrix.w2v[0] = t[0];
	ul_ipv_matrix.w2v[1] = t[1];
	ul_ipv_matrix.w2v[2] = t[2];
	ul_ipv_matrix.s1[0] = s[0];
	ul_ipv_matrix.s1[1] = s[1];
	ul_ipv_matrix.s1[2] = s[2];
}
/*********************************************************************
**    E_FUNCTION     : ms_create_view()
**       Create MSL View.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ms_create_view()
{
	UM_vector vup;
	UM_vector vpn;
/*
......"Front"
*/
	MSL_view[0].key = 0;
	MSL_view[0].rel_num = 0;
	strcpy(MSL_view[0].name,"MS_FRONT");

	MSL_view[0].vtype = UV_SYSDEF_VIEW;
	MSL_view[0].can_save = 0;
	MSL_view[0].modified = UU_FALSE;

	MSL_view[0].projection = UV_PARALLEL;
	MSL_view[0].sav_eye_dist = 0.0;
	MSL_view[0].cur_eye_dist = 0.0;

	um_vctovc(UM_zerovec, MSL_view[0].sav_ref_pt);
	um_vctovc(UM_zerovec, MSL_view[0].cur_ref_pt);
	um_unitvc(UM_zaxis, vup);
	um_vctovc(vup, MSL_view[0].sav_up_vect);
	um_vctovc(vup, MSL_view[0].cur_up_vect);

	um_vctmsc(UM_yaxis, (UU_REAL) -1.0, vpn);
	um_vctovc(vpn, MSL_view[0].sav_pln_norm);
	um_vctovc(vpn, MSL_view[0].cur_pln_norm);

	MSL_view[0].sav_aperture = UM_model_size;
	MSL_view[0].cur_aperture = UM_model_size;
	MSL_view[0].sav_front_clip = 10000.0;
	MSL_view[0].sav_back_clip = -10000.0;
	MSL_view[0].cur_front_clip = 10000.0;
	MSL_view[0].cur_back_clip = -10000.0;
	MSL_view[0].sav_do_clip = UV_NOCLIP;
	MSL_view[0].cur_do_clip = UV_NOCLIP;
}

/*********************************************************************
**    E_FUNCTION     : ms_create_vport()
**       Create MSL viewport.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ms_create_vport()
{
	int i;

	LW_vport.llf[0] = 1.0;
	LW_vport.llf[1] = 1.0;
	LW_vport.llf[2] = 1.0;

	LW_vport.urb[0] = 1.0;
	LW_vport.urb[1] = 1.0;
	LW_vport.urb[2] = 1.0;

	LW_vport.cur_view = 0;

	LW_vport.disp_prio = 0;
	LW_vport.input_prio = 0;

	LW_vport.disp_all = UU_TRUE;

	LW_vport.bord_seg = -1;
	LW_vport.aperture_on = 0;
	LW_vport.v_axis_on = 0;
	LW_vport.name_on = 0;
	LW_vport.bord_on = UU_TRUE;
	LW_vport.nverts = 0;
	for (i=0; i<60; i++) LW_vport.vertices[i] = 0.0;
	LW_vport.grid_seg = -1;
	LW_vport.grid_on = UU_FALSE;
	LW_vport.motion = UU_TRUE;

	LW_vport.rel_num = 0;
	LW_vport.xform = -1;
	LW_vport.llf[0] = 0.0; LW_vport.llf[1] = 0.0; LW_vport.llf[2] = 0.0;
	LW_vport.urb[0] = 1.0; LW_vport.urb[1] = 1.0; LW_vport.urb[2] = 0.0;
}

/*********************************************************************
**    E_FUNCTION     : msl_aspect_ratio(vport, aspect_ratio)
**       Calculate the ASPECT_RATIO of the specified viewport VPORT.
**    PARAMETERS   
**       INPUT  : 
**          vport					viewport entity
**       OUTPUT :  
**          aspect_ratio		aspect ratio of the viewport
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void msl_aspect_ratio(vport, aspect_ratio)
UV_vport *vport;
UU_REAL *aspect_ratio;
{
	*aspect_ratio = ((vport->urb[1] - vport->llf[1]) *
						(MSL_area.ur.y - MSL_area.ll.y))/
						((vport->urb[0] - vport->llf[0]) *
						(MSL_area.ur.x - MSL_area.ll.x));
}

/**************************************************************************
**  E_FUNCTION:  msl_calcvp(vport)
**      Calculate the viewport size for the specified viewport.
**  PARAMETERS   
**      INPUT  :  vport	:	vport entity to be updated
**      OUTPUT :  viewport->llf	:	lower left front of viewport.
**					viewport->urb	:	upper right front of viewport.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void msl_calcvp(vport, viewport)
UV_vport		*vport;
Gnrect3	*viewport;
{
	UM_coord	llf;
	UM_vector urb;
	UM_ndc	lleft;
	UM_ndc	uright;
	Gint ll[2], ur[2];
	UU_REAL aspect_ratio;
	UU_REAL  zwidth;
	int xf = vport->cur_view;

	lleft[0] = vport->llf[0] * (MSL_area.ur.x - MSL_area.ll.x) + MSL_area.ll.x;
	lleft[1] = vport->llf[1] * (MSL_area.ur.y - MSL_area.ll.y) + MSL_area.ll.y;
	uright[0] = vport->urb[0] * (MSL_area.ur.x - MSL_area.ll.x) + MSL_area.ll.x;
	uright[1] = vport->urb[1] * (MSL_area.ur.y - MSL_area.ll.y) + MSL_area.ll.y;

	(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DNDCDEV]) (lleft,ll);
	(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DNDCDEV]) (uright,ur);

	ll[0]++;
	ll[1]++;
	ur[0]--;
	ur[1]--;

	(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DDEVNDC]) (ll,lleft);
	(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DDEVNDC]) (ur,uright);

	msl_aspect_ratio(&vport, &aspect_ratio);
	llf[0] = -(MSL_view[xf].cur_aperture/2.);
	llf[1] = -(MSL_view[xf].cur_aperture*aspect_ratio/2.);
	llf[2] = MSL_view[xf].cur_front_clip;
	urb[0] = (MSL_view[xf].cur_aperture/2.);
	urb[1] = (MSL_view[xf].cur_aperture*aspect_ratio/2.);
	urb[2] = MSL_view[xf].cur_back_clip;

	if (urb[0] == llf[0]) zwidth = 0;
	else
		zwidth = (llf[2] - urb[2]) * (uright[0]-lleft[0]) / (urb[0] - llf[0]);

	lleft[2]  =  zwidth / 2.0;
	uright[2] = -zwidth / 2.0;

	viewport->llf.x = lleft[0];
	viewport->llf.y = lleft[1];
	viewport->llf.z = lleft[2];

	viewport->urb.x = uright[0];
	viewport->urb.y = uright[1];
	viewport->urb.z = uright[2];
}
/***********************************************************************
**  E_FUNCTION:  msl_update_pre_view()
**			Update the viewing by the chosen methode.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void msl_update_pre_view(view)
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
**  E_FUNCTION:  msl_reset_prev()
**			Update the viewing by the chosen methode.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
msl_reset_prev()
{
	int xf;
	UM_vector vup,vpn;
	UU_REAL	tmp_ref_pt[3];
	UU_REAL	tmp_pln_norm[3];
	UU_REAL	tmp_up_vect[3];
	UU_REAL	tmp_aperture;
	UU_REAL	tmp_front_clip;
	UU_REAL	tmp_back_clip;
	UU_LOGICAL	tmp_do_clip;

	if (pre_view.pre_aperture == 0) goto done;

	xf = LW_vport.cur_view;
	um_unitvc(MSL_view[xf].cur_pln_norm, vpn);
	um_vctovc(vpn, tmp_pln_norm);
	um_unitvc(MSL_view[xf].cur_up_vect, vup);
	um_vctovc(vup, tmp_up_vect);
	um_vctovc(MSL_view[xf].cur_ref_pt, tmp_ref_pt);
	tmp_back_clip = MSL_view[xf].cur_back_clip;
	tmp_front_clip = MSL_view[xf].cur_front_clip;
	tmp_do_clip = MSL_view[xf].cur_do_clip;
	tmp_aperture = MSL_view[xf].cur_aperture;

	um_unitvc(pre_view.pre_pln_norm, vpn);
	um_vctovc(vpn, MSL_view[xf].cur_pln_norm);
	um_unitvc(pre_view.pre_up_vect, vup);
	um_vctovc(vup, MSL_view[xf].cur_up_vect);
	um_vctovc(pre_view.pre_ref_pt, MSL_view[xf].cur_ref_pt);
	MSL_view[xf].cur_aperture = pre_view.pre_aperture;
	MSL_view[xf].cur_front_clip = pre_view.pre_front_clip;
	MSL_view[xf].cur_back_clip = pre_view.pre_back_clip;
	MSL_view[xf].cur_do_clip = pre_view.pre_do_clip;

	um_vctovc(tmp_pln_norm, pre_view.pre_pln_norm);
	um_vctovc(tmp_up_vect, pre_view.pre_up_vect);
	um_vctovc(tmp_ref_pt, pre_view.pre_ref_pt);
	pre_view.pre_back_clip = tmp_back_clip;
	pre_view.pre_front_clip = tmp_front_clip;
	pre_view.pre_do_clip = tmp_do_clip;
	pre_view.pre_aperture = tmp_aperture;

	MSL_view[xf].modified = UU_TRUE;

	uv_setxform();
	ul_ipv_matrix.changed = 1;
	ul_ipv_view_same(xf);
done:;
}
/**************************************************************************
**  E_FUNCTION:  msl_vfit()
**			Using the bounding boxes of the current segments for the
**			lower left and upper right of the viewport
**  PARAMETERS   
**      INPUT  :  flag: 0: don't update pre_view 
**						1: update pre_view 
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
msl_vfit(flag)
int flag;
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
	int n;
	Gwrect3 box;

	uu_denter(UU_MTRC,(us,"uvu_vpzoomextrema()"));
		
	n = LW_vport.cur_view;
	if (flag)
		msl_update_pre_view(MSL_view[n]);

	scale = 1.;
	ok = ul_ipv_view_extrema(&LW_vport,&box,&scale);

	del_x = fabs (box.llf.x - box.urb.x);
	del_y = fabs (box.llf.y - box.urb.y);
	del_z = fabs (box.llf.z - box.urb.z);

	if( ok )
	{
		uv_extrema_project(&box.llf,&box.urb,&MSL_view[n]);
		uvu_vcstomcs(&MSL_view[n], 0, &box.urb, urb);
		uvu_vcstomcs(&MSL_view[n], 0, &box.llf, llf);
		midpt[0] = (urb[0] + llf[0]) / 2.0;
		midpt[1] = (urb[1] + llf[1]) / 2.0;
		midpt[2] = (urb[2] + llf[2]) / 2.0;
		if(um_cceqcc(midpt, MSL_view[n].cur_ref_pt))
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
								(MSL_area.ur.y - MSL_area.ll.y))
							 / ((LW_vport.urb[0] - LW_vport.llf[0]) *
								(MSL_area.ur.x - MSL_area.ll.x));
		/* get the new aspect ratio */
			um_vctovc(&box.llf, llfvs);
			um_vctovc(&box.urb, urbvs);

			aperture = fabs(urbvs[0] - llfvs[0]);

			wylen = fabs(urbvs[1] - llfvs[1])/aspect_ratio;
			if(wylen > aperture ) aperture = wylen;

			aperture = aperture * scale * 1.05; /* allow 2.5% border */
			MSL_view[n].modified = (fabs(aperture - MSL_view[n].cur_aperture) > UM_FUZZ);
			MSL_view[n].modified = UU_TRUE;
		}
		else
		{
			MSL_view[n].modified = UU_TRUE;
			aperture = 0.0;
		}
	
		if (MSL_view[n].modified || cent_modified) 
		{
			MSL_view[n].modified = UU_TRUE;
			um_vctovc(midpt, MSL_view[n].cur_ref_pt);
			if (aperture > UM_FUZZ)
				MSL_view[n].cur_aperture = aperture;
			uv_setxform();
			ul_ipv_matrix.changed = 1;
			ul_ipv_view_same(n);
		}
	}
	return 0;
}
/**************************************************************************
**  E_FUNCTION:  msl_window_view()
**			Prompt the user for a viewport and the lower left and upper right
**			corners of a new view.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void msl_window_view()
{
	UU_REAL llf[3];
	UU_REAL urb[3];
	UU_REAL midpt[3];
	int numint;
	int i;
	UU_REAL aspect_ratio;					/* aspect ratio of current viewport */
	UU_REAL wxlen;								/* x length of view port */
	UU_REAL wylen;								/* y length of view port */
	UU_REAL aperture;							/* x length of view */
	UD_RUBBER rubber;							/* rubber box control block */
	int button,event;
	UM_vector norm;
	int n, ifl;
	UM_transf tf;

	n = LW_vport.cur_view;
	ifl = 0;

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
			msl_update_pre_view(MSL_view[n]);

			for (i = 0; i < 3; i++) midpt[i] = (urb[i] + llf[i]) / 2.;
			um_vctovc(midpt, MSL_view[n].cur_ref_pt);

			/* get the current aspect ratio */

			aspect_ratio = ((LW_vport.urb[1] - LW_vport.llf[1]) *
									(MSL_area.ur.y - MSL_area.ll.y))
								 / ((LW_vport.urb[0] - LW_vport.llf[0]) *
									(MSL_area.ur.x - MSL_area.ll.x));

			wxlen = fabs(urb[0] - llf[0]) ;
			wylen = fabs(urb[1] - llf[1]) ;
			if (aspect_ratio > 0.0) wylen /= aspect_ratio;
			aperture = (wxlen < wylen)? wylen: wxlen;
			aperture *= 1.1;
			MSL_view[n].cur_aperture = aperture;
			MSL_view[n].modified = UU_TRUE;
			uv_setxform();
			ul_ipv_matrix.changed = 1;
			ul_ipv_view_same(n);
		}
		RUBBER_OFF(&rubber);
	}
}
/********************************************************************* 
**  E_FUNCTION:        msl_dynamic
**      Invoke dynamic mouse viewing on the terminal
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void msl_dynamic()
{	
	void msl_dynmouse();

	uu_move_byte((char*)&MSL_view[LW_vport.cur_view], (char*)&UV_dynview,
									sizeof(UV_view));
	if (LW_dyncenter == 0)
	{
		LW_dyncp[0] = UV_dynview.cur_ref_pt[0];
		LW_dyncp[1] = UV_dynview.cur_ref_pt[1];
		LW_dyncp[2] = UV_dynview.cur_ref_pt[2];
	}
	msl_dynmouse(&LW_vport);
}

/********************************************************************* 
**  E_FUNCTION:        msl_dynmouse(vport)
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
void msl_dynmouse(vport)
	UV_vport	*vport;
{
	Glocrec locrec ;
	Gdrect locwin;
	Gqloc *greqloc();
	static char panprompt[] = "Dynamic viewing in effect";
	Gerror rtn;
	Gwpoint3 pt;
	Gwpoint3 *vup;
	UM_coord csav;
	Gwrect3 *window;
	int pick_save;
/*
.....during dynamic rotaion attach the leader line to the left corner of the
.....label box so that the leader lines always appear attached to the labels
*/
	pick_save = 0;
	locrec.prompt = panprompt;
	csav[0] = LW_dyncp[0];
	csav[1] = LW_dyncp[1];
	csav[2] = LW_dyncp[2];

	gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO) ;
	rtn=ginitloc(UD_ksws, 1, &UD_nxhair, 46, &locwin, &locrec);
	if (rtn==0)
	{
		greqloc(UD_ksws, 1);
		givpn3(LW_vport.cur_view, &pt);
		msl_update_pre_view(MSL_view[LW_vport.cur_view]);
/*
.....Set the view normal
*/
		MSL_view[LW_vport.cur_view].cur_pln_norm[0] = pt.x;
		MSL_view[LW_vport.cur_view].cur_pln_norm[1] = pt.y;
		MSL_view[LW_vport.cur_view].cur_pln_norm[2] = pt.z;
/*
.....Set the view up vector
*/
		vup = gqvup3(LW_vport.cur_view);
		MSL_view[LW_vport.cur_view].cur_up_vect[0] = (*vup).x;
		MSL_view[LW_vport.cur_view].cur_up_vect[1] = (*vup).y;
		MSL_view[LW_vport.cur_view].cur_up_vect[2] = (*vup).z;
/*
.....Set the aperture
*/
		window = gqwindow3(LW_vport.cur_view);
		MSL_view[LW_vport.cur_view].cur_aperture = (window->urb.x - window->llf.x);
/*
.....Update the view reference point
*/
		MSL_view[LW_vport.cur_view].cur_ref_pt[0] = LW_dyncp[0];
		MSL_view[LW_vport.cur_view].cur_ref_pt[1] = LW_dyncp[1];
		MSL_view[LW_vport.cur_view].cur_ref_pt[2] = LW_dyncp[2];
		uv_setxform();
		ul_ipv_view_same(LW_vport.cur_view);

		gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO) ;
		LW_dyncp[0] = csav[0];
		LW_dyncp[1] = csav[1];
		LW_dyncp[2] = csav[2];
	}
done:;
}
/*********************************************************************
**    E_FUNCTION :  msl_view_from_axis()
**       view axis
**    PARAMETERS   
**       INPUT  : 
**          axis   = 0 - POSX, 1 - POSY, 2 - POSZ, 3 - USER,/* not implement 
**				         4 - NEGX, 5 = NEGY, 6 = NEGZ, 7 - Isometric
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void msl_view_from_axis(option)
int option;
{
	UM_vector norm;
	UM_vector up;
	int status;
	int xf;

	uu_denter(UU_MTRC,(us,"uvu_vwpnorm(option = %d)",option));

	status = 0;
	
	xf = LW_vport.cur_view;
	msl_update_pre_view(MSL_view[xf]);

	if (option == 0)
	{
		um_vctovc(UM_xaxis, norm);
		um_vctovc(UM_zaxis, up);
	}
	else if (option == 1)
	{
		um_vctovc(UM_zaxis, up);
		um_vctovc(UM_yaxis, norm);
	}
	else if (option == 2)
	{
		um_vctovc(UM_yaxis, up);
		um_vctovc(UM_zaxis, norm);
	}
	else if (option == 4)
	{
		um_vctmsc(UM_xaxis, (UU_REAL) -1.0, norm);
		um_vctovc(UM_zaxis, up);
	}
	else if (option == 5)
	{
		um_vctmsc(UM_yaxis, (UU_REAL) -1.0, norm);
		um_vctovc(UM_zaxis, up);
	}
	else if (option == 6)
	{
		um_vctmsc(UM_zaxis, (UU_REAL) -1.0, norm);
		um_vctovc(UM_yaxis, up);
	}
	else if (option == 7)
	{
		um_xyztovc((UU_REAL)-1.0, (UU_REAL) -1.0, (UU_REAL) 1.0, norm);
		um_unitvc(norm, norm);
		um_nptpln(UM_zaxis, UM_zerovec, norm, up);
		um_unitvc(up, up);
	}
	uv_update_up_vect(&MSL_view[xf], up);
	uv_update_pln_norm(&MSL_view[xf], norm);
	MSL_view[xf].modified = UU_TRUE;
	uv_setxform();
	msl_vfit(0);
}
/*********************************************************************
**	 E_FUNCTION:int ul_ipv_view_extrema(xform,box)
**		This function calculates the bounding box for the MSLITE view.
**		little changed from NCL
**	 PARAMETERS	
**		 INPUT  : xform   = Specifies the transformation matrix to use.
**		 OUTPUT : box     = Bounding box.
**	 RETURNS: UU_TRUE if a box could be calculated.  UU_FALSE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
UU_LOGICAL ul_ipv_view_extrema(vport,box,scale)
UV_vport *vport;
Gwrect3 *box;
UU_REAL *scale;
{
	int stat,i,j;
	LW_stock_struc *sd;
	LtBoolean flag;
	LtDoubleBounds bounds;
	LtData data;
	LW_mach_solid_struc *spt;
	UU_LOGICAL ifl;
	UM_transf mcstf;
/*
.....Initialize routine
*/
	stat = UU_FALSE;
	box->llf.x = 10000.; box->urb.x = -10000.;
	box->llf.y = 10000.; box->urb.y = -10000.;
	box->llf.z = 10000.; box->urb.z = -10000.;
/*
.....Get active MODSYS matrix
*/
	ifl = 0;
/*
.....Bounding box is furnished by difference solid
*/
	if (LW_diff_solid_view)
	{
		stat = UU_TRUE;
		S_add_box(LW_diff_bounds,box,ifl,mcstf);
	}
		
/*
.....Loop through stocks and fixtures
.....and get bounding boxes
*/
	else
	{
		stat = UU_TRUE;
		flag = LI_SPRIM_PROP_BOUNDS;
		if (LW_session[LW_mach_mode] == 0) flag = LI_SOLID_PROP_MW_BOUNDS;
		if (LW_nstock[0]+LW_nstock[1] > 0)
		{
			for (j=0;j<2;j++)
			{
				sd = LW_stock_first[j];
				for (i=0;i<LW_nstock[j];i++)
				{
					if (sd->stock != 0)
					{
//						LiPrimitiveGetSolidProperty(sd->stock,flag,&data);
//						LiDataGetDoubleBounds(&data,bounds);
//						S_add_box(bounds,box,ifl,mcstf);
		
						if (LW_session[LW_mach_mode] == 0)
							LiPrimitiveGetSolidProperty(sd->prim,flag,&data);
						else
							LiSessionPrimGetProperty(sd->stock,flag,&data);
						LiDataGetDoubleBounds(&data,bounds);
						S_add_box(bounds,box,ifl,mcstf);
					}
					sd = (LW_stock_struc *)uu_lsnext(sd);
				}
			}
		}
/*
........Loop through machine solids
*/
		spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
		for (i=0;i<LW_mach_nsolid;i++)
		{
			if (spt[i].stock.id != 0 && spt[i].stock.stock != 0)
			{
//				LiPrimitiveGetSolidProperty(spt[i].stock.stock,flag,&data);
				if (LW_session[LW_mach_mode] == 0)
					LiPrimitiveGetSolidProperty(spt[i].stock.prim,flag,&data);
				else
					LiSessionPrimGetProperty(spt[i].stock.stock,flag,&data);
				LiDataGetDoubleBounds(&data,bounds);
				S_add_box(bounds,box,ifl,mcstf);
			}
		}
	}
	*scale = 1;
/*
.....End of routine
*/
	return(stat);
}
/*
.....not really implement since we are not use space mouse now
*/
void uv_dynstep_disp()
{
	if (UV_dynview_active == UU_TRUE) return;
	if (UV_Cur_Dyn==2)
	{
		ul_ipv_view_same(LW_vport.xform);
	}
}
/*********************************************************************
**	 E_FUNCTION:int ul_ipv_open_window()
**		Opens an interactive MSLITE window. 
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_open_window()
{
	return msl_open_window();
}

/*********************************************************************
**	 E_FUNCTION : uz_dyn_mouse
**			This function calls the appropriate routine to
**			use the mouse for dynamic viewing.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Changes the screen view to a known state.
**	 WARNINGS: none.
**       NOTES: none.
*********************************************************************/
uz_dyn_mouse()
{
	msl_dynamic();
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
/*
.....For mslite, we don't have auto-center support
*/
	cen[0] = LW_dyncp[0];
	cen[1] = LW_dyncp[1];
	cen[2] = LW_dyncp[2];
	*dyncp = UU_FALSE;
}
uw_get_bkcolor(red, green, blue)
int *red, *green, *blue;
{
	*red = UV_background.bgrgb[0]*255 + 0.5;
	*green = UV_background.bgrgb[1]*255 + 0.5;
	*blue = UV_background.bgrgb[2]*255 + 0.5;
}

