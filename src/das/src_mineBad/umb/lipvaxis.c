/*********************************************************************
**    NAME         :  lipvaxis.c
**       CONTAINS:
**			ul_ipv_create_axis
**			ul_delaxis_ipv
**			ul_drw_vaxis_ipv
**
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**        lipvaxis.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:11:11
*********************************************************************/
#include "usysdef.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "lipv.h"

#define MAX_STOCKS	3000
static LtPrim Saxis[MAX_STOCKS];
static LtSessionPrim Sessaxis[MAX_STOCKS];
/*
.....only one view axis
*/
static LtPrim Svaxis[6];
static LtSessionPrim Sessvaxis[6];
extern int UV_dynview_active;
extern int PKx,PKy;
extern int UM_swap_ipv;

/*********************************************************************
**    I_FUNCTION: ul_init_axisseg()
**      init an axis segment value
**			
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ul_init_axisseg()
{
	int j;
	for (j=0; j<MAX_STOCKS; j++)
	{
		Saxis[j] = 0;
		Sessaxis[j] = 0;
	}
	for (j=0; j<6; j++)
	{
		Svaxis[j] = 0;
		Sessvaxis[j] = 0;
	}
}

/*********************************************************************
**    I_FUNCTION: ul_create_axis_segnum()
**      Create an axis segment number
**			
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_create_axis_segnum()
{
	int j,seg;

	seg = -1;
	for (j=0; j<MAX_STOCKS; j++)
	{
		if (Saxis[j]==0)
		{
			seg = j/6;
			break;
		}
	}
	return seg;
}
/*********************************************************************
**    I_FUNCTION: ul_drw_vaxis_ipv
**						(origin,xaxis,yaxis,zaxis,length)
**      Draw a  coordinate system defined by an origin and unit vectors
**			defining the X, Y, and Z axes. The length of the axis is
**			specified by a length. on NCLIPV window
**    PARAMETERS   
**       INPUT  : 
**				origin				origin of  coordinate system
**				xaxis					unit vector defining x axis
**				yaxis					unit vector defining yaxis
**				zaxis					unit vector defining zaxis
**				length				length to draw axis
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_drw_vaxis_ipv(origin,xaxis,yaxis,zaxis,length)
	UM_coord origin;
	UM_vector xaxis;
	UM_vector yaxis;
	UM_vector zaxis;
	UM_length length;
{
	Gwpoint3 gpt[2];
	UM_coord temp[3];	
	static char *label[] = {
		"X","Y","Z"};
	int i,  xy[2];								
	UU_REAL axis_length;
	UM_coord spt, ept;
	LtDoublePoint orig;
	LtDoubleVector norm;
	Gfloat vxy[2];

	if (LW_drawable == 0 || LW_viewport == 0 || LW_mach_mode != LW_VISICUT)
		return;
	vxy[0] = 0.0;
	vxy[1] = 0.0;
	(*(ug_gksstli.wsopen[0].connid)[UG_DNDCDEV]) 
				(vxy, xy, 0);
	LiDrawableProjectImageToWorld(LW_drawable,LW_viewport, xy, orig, norm);
	spt[0] = orig[0];
	spt[1] = orig[1];
	spt[2] = orig[2];
	vxy[0] = 0.075;
	vxy[1] = 0.0;
	(*(ug_gksstli.wsopen[0].connid)[UG_DNDCDEV]) 
				(vxy, xy, 0);
	LiDrawableProjectImageToWorld(LW_drawable,LW_viewport, xy, orig, norm);
	ept[0] = orig[0];
	ept[1] = orig[1];
	ept[2] = orig[2];

	um_vcmnvc(ept, spt, temp[0]);
	axis_length = fabs(length);
	if (length > 0.)
	{
		axis_length = length * um_mag(temp[0]);
		if (length != 1.0)			
			length = length * 12.0;	
	}
	/* draw axis and labels */
	gpt[0].x = origin[0];
	gpt[0].y = origin[1];
	gpt[0].z = origin[2];
	um_vctmsc(xaxis,axis_length,temp[0]);
	um_vcplvc(origin,temp[0],temp[0]);
	um_vctmsc(yaxis,axis_length,temp[1]);
	um_vcplvc(origin,temp[1],temp[1]);
	um_vctmsc(zaxis,axis_length,temp[2]);
	um_vcplvc(origin,temp[2],temp[2]);
/*
.....only have one view axis for NCLIPV, if it is already created, don't create again
*/
	for (i=0; i<3; i++)
	{
		gpt[1].x = temp[i][0];
		gpt[1].y = temp[i][1];
		gpt[1].z = temp[i][2];
		ul_ipv_polyline(&Svaxis[i], &Sessvaxis[i], &gpt[0], 2, 1, 1);
		ul_ipv_text(&Svaxis[i+3], &Sessvaxis[i+3], label[i], &gpt[1], 1, 0);
	}
	ul_ipv_flush();
}

/*********************************************************************
**    I_FUNCTION: ul_create_axis2(origin,xaxis,yaxis,zaxis,length, color, axis_seg)
**			Draw an axis defined by an origin and unit vectors
**			defining the X, Y, and Z axes. 
**    PARAMETERS   
**       INPUT  : 
**				origin				origin of  coordinate system
**				xaxis					unit vector defining x axis
**				yaxis					unit vector defining yaxis
**				zaxis					unit vector defining zaxis
**				length				length to draw axis
**				color:				color of axis
**				axis_seg:			axis segment of stock now: -1: create a new one
**										Not -1: already draw, just return
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_create_axis2(origin,xaxis,yaxis,zaxis,length, color, axis_seg)
	UM_coord origin;
	UM_vector xaxis;
	UM_vector yaxis;
	UM_vector zaxis;
	UM_length length;
	int color, axis_seg;
{
	Gwpoint3 gpt[2];
	UM_coord temp[3];	
	static char *label[] = {
		"X","Y","Z"};
	int i,  xy[2];								
	UU_REAL axis_length;
	UM_coord spt, ept;
	LtDoublePoint orig;
	LtDoubleVector norm;
	Gfloat vxy[2];
	int seg;

	if (axis_seg!=-1)
		return axis_seg;

	if (LW_drawable == 0 || LW_viewport == 0 || LW_mach_mode != LW_VISICUT)
		return -1;
	vxy[0] = 0.0;
	vxy[1] = 0.0;
	(*(ug_gksstli.wsopen[0].connid)[UG_DNDCDEV]) 
				(vxy, xy, 0);
	LiDrawableProjectImageToWorld(LW_drawable,LW_viewport, xy, orig, norm);
	spt[0] = orig[0];
	spt[1] = orig[1];
	spt[2] = orig[2];
	vxy[0] = 0.075;
	vxy[1] = 0.0;
	(*(ug_gksstli.wsopen[0].connid)[UG_DNDCDEV]) 
				(vxy, xy, 0);
	LiDrawableProjectImageToWorld(LW_drawable,LW_viewport, xy, orig, norm);
	ept[0] = orig[0];
	ept[1] = orig[1];
	ept[2] = orig[2];

	um_vcmnvc(ept, spt, temp[0]);
	axis_length = fabs(length);
	if (length > 0.)
	{
		axis_length = length * um_mag(temp[0]);
		if (length != 1.0)			
			length = length * 12.0;	
	}
	/* draw axis and labels */
	gpt[0].x = origin[0];
	gpt[0].y = origin[1];
	gpt[0].z = origin[2];
	um_vctmsc(xaxis,axis_length,temp[0]);
	um_vcplvc(origin,temp[0],temp[0]);
	um_vctmsc(yaxis,axis_length,temp[1]);
	um_vcplvc(origin,temp[1],temp[1]);
	um_vctmsc(zaxis,axis_length,temp[2]);
	um_vcplvc(origin,temp[2],temp[2]);
	seg = ul_create_axis_segnum();
	for (i=0; i<3; i++)
	{
		gpt[1].x = temp[i][0];
		gpt[1].y = temp[i][1];
		gpt[1].z = temp[i][2];
		ul_ipv_polyline(&Saxis[i+6*seg], &Sessaxis[i+6*seg],&gpt[0], 2, color, 1);
		ul_ipv_text(&Saxis[i+6*seg+3], &Sessaxis[i+6*seg+3], label[i], &gpt[1],
			1, 1);
	}
	return seg;
}
/*********************************************************************
**    I_FUNCTION: ul_delaxis_ipv(stock)
**      delete an axis segment of a stock
**			
**    PARAMETERS   
**       INPUT  : 
**          stock: stock which segment to be deleted
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_delaxis_ipv(stock)
LW_stock_struc *stock;
{
	int j,seg;
	LtData data;
	seg = stock->axis_seg;
	if (stock->axis_seg<0)
		return;
	for (j=0; j<6; j++)
	{
		if (Saxis[seg*6+j]!=0)
		{
			LiDataSetBoolean(&data,FALSE);
			LiMWViewportSetSessPrimProperty(LW_viewport,Sessaxis[seg*6+j],
				LI_VPSP_PROP_MW_VISIBLE,&data);
			LiViSolidSetEnabled(Sessaxis[seg*6+j],FALSE);
			LiSessionRemovePrim(Sessaxis[seg*6+j]);
			LiPrimitiveDestroy(Saxis[seg*6+j]);
			Saxis[seg*6+j] = 0;
			Sessaxis[seg*6+j] = 0;
		}
	}
	stock->axis_seg = -1;
}

/*********************************************************************
**    E_FUNCTION     : um_delv_axis_ipv
**      Delete nclipv view axis segment
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_delv_axis_ipv()
{
	int j;

	for (j=0; j<6; j++)
	{
		if (Svaxis[j]!=0)
		{
			LiSessionRemovePrim(Sessvaxis[j]);
			LiPrimitiveDestroy(Svaxis[j]);
			Svaxis[j] = 0;
			Sessvaxis[j] = 0;
		}
	}
	ul_ipv_flush();
}

/*********************************************************************
**    I_FUNCTION: ul_ipv_create_axis(sd)
**      create an axis segment of a stock
**			
**    PARAMETERS   
**       INPUT  : 
**          sd: stock which segment to be created
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ul_ipv_create_axis(sd)
LW_stock_struc *sd;
{
	int x,y;
	UM_coord origin;
	UM_vector mxaxis,myaxis,mzaxis;
	int color;
	int wx,wy;
	UU_REAL ratio,rat1,rat2,aper;
	UV_view view;
	
	color = sd->axes_color;
	
	x = y = 1;
#if UU_COMP == UU_WIN2K
	if (UM_swap_ipv)
		uw_ntget_gwsize(&x,&y);
	else
		uw_ntget_ipvsize(&x,&y);
#endif
	wx = PKx; wy = PKy;
	ratio = (UU_REAL)x / (UU_REAL)y;
	uv_getvid(LW_vport.cur_view, &view);
	rat1 = (UU_REAL)wx / (UU_REAL)wy;
	rat2 = rat1 / ratio;
	aper = view.cur_aperture * rat2;
	rat2 = -(aper * .075);

	origin[0] = origin[1] = origin[2] = 0.;
	mxaxis[0] = 1.; mxaxis[1] = 0.; mxaxis[2] = 0.;
	myaxis[0] = 0.; myaxis[1] = 1.; myaxis[2] = 0.;
	mzaxis[0] = 0.; mzaxis[1] = 0.; mzaxis[2] = 1.;
	um_cctmtf(origin,sd->matrix,origin);
	um_vctmtf(mxaxis,sd->matrix,mxaxis);
	um_vctmtf(myaxis,sd->matrix,myaxis);
	um_vctmtf(mzaxis,sd->matrix,mzaxis);
	sd->axis_seg = ul_create_axis2(origin,mxaxis,myaxis,mzaxis,rat2, sd->axes_color, sd->axis_seg);
}
