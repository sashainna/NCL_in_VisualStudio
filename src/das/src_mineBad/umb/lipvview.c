/*********************************************************************
**  FILENAME: lipview.c
**  CONTAINS:   
**              ul_ipv_set_context()
**              ul_ipv_reset_view()
**              ul_ipv_view_same()
**              ul_ipv_view_extrema()
**              ul_ipv_view_segs()
**				ul_get_ipvbox_midz
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvview.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       05/01/17 , 13:14:39
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
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
extern char *LW_window;

extern int PKx,PKy;
extern int UZ_nclipv_view;
extern int LW_dyncenter;
extern UU_REAL LW_dyncp[3];
extern int NCL_swap_changed, UM_swap_ipv;
extern int UV_dynview_active;

static UU_LOGICAL Sdefered_mode = UU_FALSE, Sdefered_buffer = UU_FALSE;

void ul_ipv_view_same();
void ul_ipv_view_segs();
static void S_add_box();

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_set_context()
**		Set the graphic context to the NCLIPV window.
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
	{
/*
#ifdef UU_IPV
#if UU_COMP==UU_WIN2K
		LiOGLDrvWindowSetCurrent((HWND)LW_window);
#else
		LiOGLDrvWindowSetCurrent((Window)LW_window);
#endif
#endif
*/
	}
}
/*********************************************************************
**	 E_FUNCTION: void ul_ipv_reset_view(xform)
**		This function saves a copy of the current NCLIPV view and then
**		changes this view to match the requested viewport.
**	 PARAMETERS	
**		 INPUT  : xform   = Specifies which NCL transform to copy.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_reset_view(xf)
int xf;
{
	UV_view view;
/*
.....Save the current view
*/
	uv_getvid(LW_vport.cur_view,&view);
	nclu_update_pre_view(view);
/*
.....Change the NCLIPV view
*/
	ul_ipv_view_same(xf);
}

/*********************************************************************
**	 E_FUNCTION: void ul_ipv_view_same(xform)
**		This function copies the active NCL viewing matrix into the
**		NCLIPV viewing matrix and redisplays the NCLIPV window.
**	 PARAMETERS	
**		 INPUT  : xform   = Specifies which NCL transform to copy.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_view_same(xf)
int xf;
{
	LtMatrix mx;
	int i,j,x,y,xform,ifl;
	Gtran gmx;
	UM_transf mcstf;
	UU_REAL a,xa,ya;
	UV_vport vport;
	UV_view view,view1;

	um_set_screen_area(UM_IPV_WINDOW);
	xform = xf;
	if (LW_nclipv==LW_STANDALONE)
		xform = LW_vport.xform;
	uv_getvid(LW_vport.cur_view, &view);
	if (xform!=LW_vport.xform)
	{
		uv_getvpid(UV_act_screen[0].vports[xform-1],&vport);
		uv_getvid(vport.cur_view,&view1);
		uv_vupdate(&view1,&view);
		uv_putv(&view);
		uv_setxform(&LW_vport);
		uv_putvp(&LW_vport);
		xform = LW_vport.xform;
	}
/*
.....reset to default view
*/
	if ((xf==-1)&&(LW_nclipv==LW_STANDALONE))
	{
		uv_getdef_ipvvw(&view1);
		uv_update_ref_pt(&view, view1.cur_ref_pt);
		uv_update_pln_norm(&view, view1.cur_pln_norm);
		uv_update_up_vect(&view, view1.cur_up_vect);
		uv_update_vaperture(&view, view1.cur_aperture);
		uv_update_fbclip(&view, view1.cur_front_clip, view1.cur_back_clip);
		uv_update_clip(&view, view1.cur_do_clip);
		uv_putv(&view);
		uv_setxform(&LW_vport);
		uv_putvp(&LW_vport);
		xform = LW_vport.xform;
	}
	uw_glload_matrix(xform,UU_FALSE,UU_NULL);
/*
.....Get the size of IPV window
*/
#if UU_COMP == UU_WIN2K
	if (UM_swap_ipv)
		uw_ntget_gwsize(&x,&y);
	else
		uw_ntget_ipvsize(&x,&y);
#else
#if UU_COMP == UU_IRIS4D
#ifndef UU_RS6000
	if (UM_swap_ipv)
		uw_mfget_gwsize(&x,&y);
	else
		uw_mfget_ipvsize(&x,&y);
#endif
#endif
#endif
/*
.....Define X to Y screen ratio
*/
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
/*
.....Adjust for MODSYS
*/
	ncl_mcstf(&ifl,mcstf);
	if (ifl == 1)
	{
		ncl_43mx_to_44mx(mcstf,gmx);
		ug_matmp(mx,gmx,ug_cxform[xform]);
	}
/*
.....Copy current view matrix
.....into NCLIPV matrix
*/
	else
	{
#ifdef UU_OPENGL
		for (i=0;i<4;i++)
		{
			for (j=0;j<4;j++)
			{
/*
.....use ug_cxform, the uw_gl_matrix might not initial in the begin
//				mx[i][j] = uw_gl_matrix[xform].matrix[4*i+j];
*/
				mx[i][j] = ug_cxform[xform][i][j];
			}
		}
#endif
	}
/*
.....Adjust scale factors for X-Y Ratio
*/
	for(i=0;i<3;i++)
		for (j=0;j<3;j++) mx[i][j] = mx[i][j]*a;
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
	LiViewSetMatrices(LW_view,&mx,0);
	if (LW_display == LW_PASSIVE)
		LiViewSetResolution(LW_view, PKx, PKy, 1.0);
/*
.....Enable auto clipping
*/
/*	LiMWViewportAutoClipping(LW_view);*/
/*
.....Flush the graphics
*/
	ul_ipv_flush();
/*
.....Display the NCL segments
*/
	ul_ipv_view_segs();
	if (UV_dynview_active!=UU_TRUE)
		um_reset_pocket_graphics(UM_IPV_WINDOW);
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_view_extrema(xform,box)
**		This function calculates the bounding box for the NCLIPV view.
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
	int stat,i,j,icnt;
	LW_stock_struc *sd,*stock;
	LtBoolean flag;
	LtDoubleBounds bounds;
	LtData data;
	int x1,y1,rasll[3],rasur[3];
	UU_REAL xs,ys;
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
	ncl_mcstf(&ifl,mcstf);
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
					icnt = 0;
					do
					{
						ul_ipv_get_next_stock(sd,&stock,&icnt,UU_FALSE);
						if (icnt == -2) break;
						if (stock->stock != 0)
						{
							if (LW_session[LW_mach_mode] == 0)
								LiPrimitiveGetSolidProperty(stock->prim,flag,&data);
							else
								LiSessionPrimGetProperty(stock->stock,flag,&data);
							LiDataGetDoubleBounds(&data,bounds);
							S_add_box(bounds,box,ifl,mcstf);
						}
					} while (icnt != -1);
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
				if (LW_session[LW_mach_mode] == 0)
					LiPrimitiveGetSolidProperty(spt[i].stock.prim,flag,&data);
				else
					LiSessionPrimGetProperty(spt[i].stock.stock,flag,&data);
				LiDataGetDoubleBounds(&data,bounds);
				S_add_box(bounds,box,ifl,mcstf);
			}
		}
	}
/*
.....Define scale factor from
.....viewport size to actual window size
*/
	(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DNDCDEV])(vport->llf,rasll,UD_ksws);
	(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DNDCDEV])(vport->urb,rasur,UD_ksws);
	xs = (UU_REAL)(rasur[0] - rasll[0]) / (UU_REAL)PKx;
	ys = (UU_REAL)(rasur[1] - rasll[1]) / (UU_REAL)PKy;
/*	if (xs > ys)
		*scale = xs / 1.95;
	else *scale = ys / 1.95;*/
	*scale = 1;
#if UU_COMP == UU_WIN2K
	uw_ntget_gwsize(&x1,&y1);
#else
#if UU_COMP == UU_IRIS4D
#ifndef UU_RS6000
	uw_glget_gwsize(&x1,&y1);
#endif
#endif
#endif
	xs = (UU_REAL)x1 / (UU_REAL)y1;
	if (xs < 1.) xs = 1. / xs;
	ys = (UU_REAL)PKx / (UU_REAL)PKy;
	if (ys < 1.) ys = 1. / ys;
	*scale = xs / ys;
/*	x = PKx;
	y = PKy;
		*scale = (UU_REAL)x / (UU_REAL)y;
		if (*scale < 1.) *scale = 1. / *scale;*/
/*
.....Adjust for MODSYS
*/
/*
	if (stat)
	{
		ncl_mcstf(&ifl,mcstf);
		if (ifl == 1)
		{
			um_cctmtf(&box->llf,mcstf,&box->llf);
			um_cctmtf(&box->urb,mcstf,&box->urb);
		}
	}
*/
/*
.....End of routine
*/
	return(stat);
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
	int x,y,i,j;
	Glntype line_style;
	int wx,wy;
	UU_REAL ratio,rat1,rat2,aper;
	UV_view view;
	LtDouble nr1;
	LtDouble fr1;
	UM_coord origin;
	UM_vector mxaxis,myaxis,mzaxis;
	LW_stock_struc *sd;
/*
.....Enable the pocket window context
*/
	if (!LW_active) goto done;
	uw_glset_context(UM_IPV_WINDOW,UU_TRUE);
	uw_glset_dirty_flag(UU_FALSE);
/*
.....Draw in the front buffer
*/
	uw_gldrawbuffer(UG_FRONT_BUFFER);

	x = y = 1;
#if UU_COMP == UU_WIN2K
	if (UM_swap_ipv)
		uw_ntget_gwsize(&x,&y);
	else
		uw_ntget_ipvsize(&x,&y);
#endif
	wx = PKx; wy = PKy;
	ratio = (UU_REAL)x / (UU_REAL)y;
/*
.....Load the view matrix
*/
	nr1=-1000; fr1=1000;
	LiViewSetClipping(LW_view,nr1,fr1);
/*	LiMoViewSet(UU_NULL);*/
/*
........View aperture
*/
	uv_getvid(LW_vport.cur_view, &view);
	rat1 = (UU_REAL)wx / (UU_REAL)wy;
	rat2 = rat1 / ratio;
	aper = view.cur_aperture * rat2;
/*
......delete NCLIPV view axis first before drawing except when dynamic drawing
*/
	um_delv_axis_ipv();
/*
.....Display the viewport axes
*/
	if (LW_display_prop.axes && !UV_dynview_active)
	{
/*
........Draw the axes
*/
		rat2 = -(aper * .05);
		uv_drawvaxis(&LW_vport,rat2,aper,rat1);
	}
/*
.....Display the stock axes. 
.....the stock axis segment is store in stock and created when create stock
.....but when first time stock created, we haven't have LW_drawble setup yet for NCL
.....so the segment returned will be -1. In that case, we should recreated the axis
.....segment here. Otherwise, it is just checked and do nothing
*/
	if (!LW_mach_simul)
	{
		for (j=0;j<2;j++)
		{
			sd = LW_stock_first[j];
			for (i=0;i<LW_nstock[j];i++)
			{
				if ((sd->axes)&&(sd->axes_color!=0)&&(sd->axis_seg==-1))
				{
					sd->axis_seg = ul_ipv_create_axis(sd);
				}
				sd = (LW_stock_struc *)uu_lsnext(sd);
			}
		}
	}
/*
.....Reset the NCL graphics
*/
	uw_glset_dirty_flag(UU_TRUE);
	uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
/*
.....End of routine
*/
done:;
	return;
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
**    E_FUNCTION :  uv_get_ipvbox_midz(llf, urb, midpt)
**       Calculates the NDC midpoint of the active viewport as defined
**       by its corner points.  The Z-level of the midpoint is calculated
**       by averaging the Z-depths of all geometry displayed in the
**       viewport.
**    PARAMETERS   
**       INPUT  :
**          llf    = Lower left of viewport.
**          urb    = Upper right of viewport.
**       OUTPUT :
**          midpt  = Middle point of viewport.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_get_ipvbox_midz(llf, urb, midpt)
UU_REAL midpt[3], llf[3], urb[3];
{
	int i,j,k,n,ifl;
	UU_LOGICAL first;
	UU_REAL nx, ny, nz, minz, maxz, midz,z;
	UM_coord pt1;
	Gnrect rect;
	UG_segstli *p;
	char sbuf[80];

	int stat;
	LW_stock_struc *sd,*sdtmp;
	LtBoolean flag;
	LtDoubleBounds bounds;
	LtData data;
	int x1,y1,rasll[3],rasur[3];
	UU_REAL xs,ys;
	LW_mach_solid_struc *spt;
	UU_LOGICAL trfl;
	UM_transf mcstf;
	static void S_ipv_ndcbox();
/*
.....Loop through the displayed segments
*/
	minz = maxz = 0;
	first = UU_TRUE;
	stat = UU_TRUE;
/*
.....Get active MODSYS matrix
*/
	ncl_mcstf(&trfl,mcstf);
	flag = LI_SPRIM_PROP_BOUNDS;
	if (LW_session[LW_mach_mode] == 0) flag = LI_SOLID_PROP_MW_BOUNDS;
	if (LW_nstock[0]+LW_nstock[1] > 0)
	{
		for (j=0;j<2;j++)
		{
			sd = LW_stock_first[j];
			for (i=0;i<LW_nstock[j];i++)
			{
				ifl = 0;
				do
				{
					ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
					if (ifl == -2) break;
					if (sdtmp->stock != 0)
					{
						if (LW_session[LW_mach_mode] == 0)
							LiPrimitiveGetSolidProperty(sdtmp->prim,flag,&data);
						else
							LiSessionPrimGetProperty(sdtmp->stock,flag,&data);
						LiDataGetDoubleBounds(&data,bounds);
						S_ipv_ndcbox(bounds,&rect);
						if (urb[0] >= rect.ll.x && llf[0] <= rect.ur.x &&
						 	urb[1] >= rect.ll.y && llf[1] <= rect.ur.y)
						{
/*
.........Calculate average NDC Z-level
.........for intersecting box
*/
							pt1[0] = bounds[LI_MINX];
							pt1[1] = bounds[LI_MINY];
							pt1[2] = bounds[LI_MINZ];
							z = 0.; n = 0;
							for (k=0;k<9;k++)
							{
								gwndc3(&nx,&ny,&nz,pt1[0],pt1[1],pt1[2]);
								if (urb[0] >= nx && llf[0] <= nx &&
								 	urb[1] >= ny && llf[1] <= ny)
								{
									z = z + nz;
									n++;
								}
								if (k == 0) pt1[0] = bounds[LI_MAXX];
								else if (k == 1) pt1[2] = bounds[LI_MAXZ];
								else if (k == 2) pt1[1] = bounds[LI_MAXY];
								else if (k == 3) pt1[2] = bounds[LI_MINZ];
								else if (k == 4) pt1[0] = bounds[LI_MINX];
								else if (k == 5) pt1[0] = bounds[LI_MAXX];
								else if (k == 6) pt1[1] = bounds[LI_MINY];
								else
								{
									pt1[0]=bounds[LI_MINX] + (bounds[LI_MAXX]-bounds[LI_MINX])/2.;
									pt1[1]=bounds[LI_MINY] + (bounds[LI_MAXY]-bounds[LI_MINY])/2.;
									pt1[2]=bounds[LI_MINZ] + (bounds[LI_MAXZ]-bounds[LI_MINZ])/2.;
								}
							}
							if (n == 0)
							{
								z = nz;
								n = 1;
							}
							z = z / n;
/*
.........Save this Z-level and compare min/max Z
*/
							if (first)
							{
								minz = z;
								maxz = z;
								first = UU_FALSE;
							}
							else
							{
								if (z < minz) minz = z;
								if (z > maxz) maxz = z;
							}
						}
					}
				} while (ifl != -1);
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
			if (LW_session[LW_mach_mode] == 0)
				LiPrimitiveGetSolidProperty(spt[i].stock.prim,flag,&data);
			else
				LiSessionPrimGetProperty(spt[i].stock.stock,flag,&data);
			LiDataGetDoubleBounds(&data,bounds);
			S_ipv_ndcbox(bounds,&rect);
			if (urb[0] >= rect.ll.x && llf[0] <= rect.ur.x &&
					 urb[1] >= rect.ll.y && llf[1] <= rect.ur.y)
			{
/*
.........Calculate average NDC Z-level
.........for intersecting box
*/
				pt1[0] = bounds[LI_MINX];
				pt1[1] = bounds[LI_MINY];
				pt1[2] = bounds[LI_MINZ];
				z = 0.; n = 0;
				for (k=0;k<9;k++)
				{
					gwndc3(&nx,&ny,&nz,pt1[0],pt1[1],pt1[2]);
					if (urb[0] >= nx && llf[0] <= nx &&
							 urb[1] >= ny && llf[1] <= ny)
					{
						z = z + nz;
						n++;
					}
					if (k == 0) pt1[0] = bounds[LI_MAXX];
					else if (k == 1) pt1[2] = bounds[LI_MAXZ];
					else if (k == 2) pt1[1] = bounds[LI_MAXY];
					else if (k == 3) pt1[2] = bounds[LI_MINZ];
					else if (k == 4) pt1[0] = bounds[LI_MINX];
					else if (k == 5) pt1[0] = bounds[LI_MAXX];
					else if (k == 6) pt1[1] = bounds[LI_MINY];
					else
					{
						pt1[0]=bounds[LI_MINX] + (bounds[LI_MAXX]-bounds[LI_MINX])/2.;
						pt1[1]=bounds[LI_MINY] + (bounds[LI_MAXY]-bounds[LI_MINY])/2.;
						pt1[2]=bounds[LI_MINZ] + (bounds[LI_MAXZ]-bounds[LI_MINZ])/2.;
					}
				}
				if (n == 0)
				{
					z = nz;
					n = 1;
				}
				z = z / n;
/*
.........Save this Z-level and compare min/max Z
*/
				if (first)
				{
					minz = z;
					maxz = z;
					first = UU_FALSE;
				}
				else
				{
					if (z < minz) minz = z;
					if (z > maxz) maxz = z;
				}
			}
		}
	}
/*
.....Calculate viewport midpoint
*/
	midpt[0] = (urb[0] + llf[0]) / 2.0;
	midpt[1] = (urb[1] + llf[1]) / 2.0;
	midpt[2] = (maxz + minz) / 2.0;
}

/*********************************************************************
**	 I_FUNCTION: S_ipv_ndcbox(bounds, rect)
**		convert the bounding box of an IPV solid into a NDC box
**		
**	 PARAMETERS	
**		 INPUT  :
**        bounds  = Bounding box of IPV solid.
**		 OUTPUT :
**        rect     = NDC box.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
static void S_ipv_ndcbox(bounds, rect)
LtDoubleBounds bounds;
Gnrect *rect;
{
	UM_coord pt;
	Gwrect3 wcbox;

	wcbox.llf.x = bounds[LI_MINX];
	wcbox.llf.y = bounds[LI_MINY];
	wcbox.llf.z = bounds[LI_MINZ];
	wcbox.urb.x = bounds[LI_MAXX];
	wcbox.urb.y = bounds[LI_MAXY];
	wcbox.urb.z = bounds[LI_MAXX];

	ug_ndcbox(&wcbox, rect, LW_vport.xform);
}
