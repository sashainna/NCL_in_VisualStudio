/*********************************************************************
**    NAME         :  gdcoord.c -- simulatin routines for ws_xform.
**       CONTAINS:
**       ug_dwswind(prms) -- workstation window.
**			ug_dwsvport(prms) -- workstation viewport.
**			ug_dwindow(prms) -- window.
**			ug_dviewport(prms) -- viewport.
**			ug_dvrefpt(prms) -- set view ref pt.
**			ug_dvpn(prms) -- set view plane normal.
**			ug_dvup(prms) -- set view up.
**			ug_dmodxf(prms) -- set modelling xform.
**			ug_dndctodev(f,i) -- ndc to raster coordinate conversion.
**			ug_devtondc(i,f) -- raster to ndc coordinate conversion.
**			ug_getwsxform(wsid,scale,dx,dy) -- inquire current wsxform.
**			ug_getredrwflag()  -- return digs redraw flag
**			ug_getredrwrect(rect)  -- return digs redraw rectangle & flag
**			ug_getredrwrect2(rect)  -- return digs 2-D redraw rectangle & flag
**			ug_setredrwflag(flag)  -- set digs redraw flag
**			ug_setredrwrect(rect)  -- set digs redraw rectangle & flag
**			ug_setredrwrect2(rect)  -- set digs 2-D redraw rectangle & flag
**			ug_resetredrwrect()  -- reset digs redraw rectangle & flag
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
** 
**    MODULE NAME AND RELEASE LEVEL
**       gdcoord.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:18
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "gtbl.h"
#include "zsysdep.h"

static struct
{ 
	int flag;
	Gnrect3 ndcrect;
} Sredrw= {0, 0.,0.,1., 1.,1.,0.};

static struct
{
	Gfloat scale;
	Gfloat dx,dy;
} Swsxform[UG_MAXOPWS];

void ug_setredrwrect();

static void ug_dwsxform();

/********************* ug_dwswind **************************/
typedef struct { Gint op; Gws ws; Gnrect3 wswind;} Grec3;
void ug_dwswind(prms)              /* set workstation window. */
Grec3 *prms;
{
/*	char us[120];*/
	zbytecp((*ug_gksstli.wsopen[(*prms).ws].outptr).curxform.w,(*prms).wswind);			/* structure assignment */
	ug_dwsxform(&(*ug_gksstli.wsopen[(*prms).ws].outptr).curxform,
			&Swsxform[(*prms).ws],(*prms).ws);
	uu_denter2(UU_GITRC,(us,"ug_dwswind(%g %g,  %g %g)",(*prms).wswind.llf.x,
			(*prms).wswind.llf.y,(*prms).wswind.urb.x,(*prms).wswind.urb.y));
	uu_dexit;
}

/********************** ug_dwsvport *********************/
typedef struct {Gint op; Gws ws; Gdrect3 wsvport;} Gvpt;
void ug_dwsvport(prms)				/* set workstation viewport */
Gvpt *prms;
{
/*	char us[120];*/
	zbytecp((*ug_gksstli.wsopen[(*prms).ws].outptr).curxform.v,(*prms).wsvport);				/* structure assignment */
	ug_dwsxform(&(*ug_gksstli.wsopen[(*prms).ws].outptr).curxform,
			&Swsxform[(*prms).ws],(*prms).ws);	/* update Swsxform */
	uu_denter2(UU_GITRC,(us," ug_dwsvport(%g %g,  %g %g)",(*prms).wsvport.llf.x,
			(*prms).wsvport.llf.y,(*prms).wsvport.urb.x,(*prms).wsvport.urb.y));
	uu_dexit;
}

/********************** ug_dwsxform *********************/
typedef struct {Gfloat scale; Gfloat dx,dy;} Gxfm;
static void ug_dwsxform(wstran,wsxform,ws)	
										/* calculate wsxform =scale and dx,dy to 
											perform the workstation transformation
											from window to vport to raster */
Gwstran3 *wstran;					/* workstation window and viewport */
Gxfm *wsxform;
Gws ws;
{
/*	char us[160];*/
	Gfloat sx,sy,s1,s;
	int ll[2],ur[2];
	int devxmax;
	Gnrect3 *wp;
	Gdrect3 *vp;
	Gfloat wdx,wdy;
	devxmax=(*ug_gksstli.wsopen[ws].wdtptr).dspsize.raster.x;
	wp= &(*wstran).w;						/* get pointers to ws window, viewport */
	vp= &(*wstran).v;
	wdx=(*wp).urb.x-(*wp).llf.x;		/* ws window width */
	wdy=(*wp).urb.y-(*wp).llf.y;		/* ws window height */
	sx=((*vp).urb.x-(*vp).llf.x)/wdx;
	sy=((*vp).urb.y-(*vp).llf.y)/wdy;
	s1=(sx<sy) ? sx : sy;			/* use smaller of scale factors */
	s=s1*devxmax/(*ug_gksstli.wsopen[ws].wdtptr).dspsize.device.x;	
	/* s now scales window to pixels */
	(*wsxform).scale=s;	/* save composite scale factor */
	(*wsxform).dx=devxmax*(*vp).llf.x-s*(*wp).llf.x;
	(*wsxform).dy=devxmax*(*vp).llf.y-s*(*wp).llf.y;
	/* ll,ur = window in integer coords */
	ll[0]=(*wp).llf.x*devxmax; ll[1]=(*wp).llf.y*devxmax;
	ur[0]=(*wp).urb.x*devxmax; ur[1]=(*wp).urb.y*devxmax;
	uu_denter2(UU_GITRC,(us,
			"ug_dwsxform. wswind=%g %g,%g %g, scale=%g, dx,dy=%g %g",
			(*wp).llf.x,(*wp).llf.y,(*wp).urb.x,
			(*wp).urb.y,(*wsxform).scale,(*wsxform).dx,(*wsxform).dy));
	uu_dexit;
	/* ll,ur= adjusted viewport in integer coords */
	ll[0]=(*wp).llf.x*s+(*wsxform).dx; 
	ll[1]=(*wp).llf.y*s+(*wsxform).dy;
	ur[0]=(*wp).urb.x*s+(*wsxform).dx; 
	ur[1]=(*wp).urb.y*s+(*wsxform).dy;
	uu_denter2(UU_GITRC,(us,"ug_dwsxform. adjusted wsvport=%d %d %d %d",
		ll[0],ll[1],ur[0],ur[1]));
	uu_dexit;
}

/**************** ug_dwindow(prms) ********************/
typedef struct { Gint op; Gws id; Gint xf; Gwrect3 wn; } Gwrc3;
void ug_dwindow(prms)			/* UG_DWINDOW simulation routine */
										/* set ug_redrw flag and ndcrect. */
Gwrc3 *prms;
{
	int indx;
/*	char us[120];*/
	indx=(*prms).xf;
	uu_denter2(UU_GITRC,(us,"ug_dwindow(%d, %g %g %g, %g %g %g) flag=%d",
		indx,(*prms).wn.llf.x,(*prms).wn.llf.y,(*prms).wn.llf.z,
		(*prms).wn.urb.x,(*prms).wn.urb.y,(*prms).wn.urb.z,Sredrw.flag));
	ug_setredrwrect(&ug_gksstli.vtran[indx].vport);
	Sredrw.flag = 1;
	uu_dexit;
}

/**************** ug_dviewport(prms) *******************/
void ug_dviewport(prms)				/* UG_DVPORT simulation routine.
											Set Sredrw flag and ndcrect */
Gwrc3 *prms;
{
	int indx;
/*	char us[120];*/
	indx=(*prms).xf;
	uu_denter2(UU_GITRC,(us,"ug_dviewport(%d, %g %g %g, %g %g %g) flag=%d",
		indx,(*prms).wn.llf.x,(*prms).wn.llf.y,(*prms).wn.llf.z,
		(*prms).wn.urb.x,(*prms).wn.urb.y,(*prms).wn.urb.z,Sredrw.flag));
	ug_setredrwrect(&(*prms).wn);
	Sredrw.flag = 1;
	uu_dexit;
}

/******************* ug_dvrefpt(prms) ***********************/
typedef struct { Gint op; Gws id; Gint xf; Gwpoint3 pt; } Gwpt3;
void ug_dvrefpt(prms)						/* set view ref pt */
Gwpt3 *prms;
{
	int indx;
/*	char us[120];*/
	indx=(*prms).xf;
	uu_denter2(UU_GITRC,(us,"ug_dvrefpt(%d, %g %g %g) flag=%d",
		indx,(*prms).pt.x,(*prms).pt.y,(*prms).pt.z,Sredrw.flag));
	ug_setredrwrect(&ug_gksstli.vtran[indx].vport);
	Sredrw.flag = 1;
	uu_dexit;
}

/******************* ug_dvpn(prms) ***********************/
void ug_dvpn(prms)						/* set view up */
Gwpt3 *prms;
{
	int indx;
/*	char us[120];*/
	indx=(*prms).xf;
	uu_denter2(UU_GITRC,(us,"ug_dvpn(%d, %g %g %g) flag=%d",
		indx,(*prms).pt.x,(*prms).pt.y,(*prms).pt.z,Sredrw.flag));
	ug_setredrwrect(&ug_gksstli.vtran[indx].vport);
	Sredrw.flag = 1;
	uu_dexit;
}

/******************* ug_dvup(prms) ***********************/
typedef struct { Gint op; Gws id; Gfloat dxup,dyup,dzup; int xf;} Gvup;
void ug_dvup(prms)						/* set view up */
Gvup *prms;
{
	int indx;
/*	char us[120];*/
	indx=(*prms).xf;
	uu_denter2(UU_GITRC,(us,"ug_dvpn(%d, %g %g %g) flag=%d",
		indx,(*prms).dxup,(*prms).dyup,(*prms).dzup,Sredrw.flag));
	ug_setredrwrect(&ug_gksstli.vtran[indx].vport);
	Sredrw.flag = 1;
	uu_dexit;
}

/********************** ug_dmodxf(prms) *********************/
typedef struct { Gint op; Gws id; Gtran mat; Gmodtran typ; } Gtrn;
void ug_dmodxf(prms)					/* set modelling xform */
Gtrn *prms;
{
	int indx;
/*	char us[120];*/
	indx=ug_gksstli.curvwindex;
	uu_denter2(UU_GITRC,(us,"ug_dmodxf(indx=%d) flag=%d",
		indx,Sredrw.flag));
	ug_setredrwrect(&ug_gksstli.vtran[0].vport);
	/*Sredrw.flag=1;*/
	uu_dexit;
}

/********************** ug_dndctodev(f,i,ws) *************************/
void ug_dndctodev(f,i,ws)	/* convert (fx,fy) from ndc to (ix,iy) 
											raster coords */
int i[2];
Gfloat f[2];
Gws ws;							/* workstation id */
{
/*	char us[180];*/
	i[0] = Swsxform[ws].scale * f[0] + Swsxform[ws].dx + 0.5;
	i[1] = Swsxform[ws].scale * f[1] + Swsxform[ws].dy + 0.5;
	uu_denter2(UU_GITRC,(us,"ug_dndctodev(%g %g, %d %d)",
		f[0],f[1],i[0],i[1]));
	uu_dexit;
}

/*********************** ug_devtondc(rast,ndc,ws) ***********************/		
void ug_devtondc(rast,ndc,ws)		/* convert from raster to ndc coords */
int rast[2];
Gfloat ndc[2];
Gws ws;
{
/*	char us[180];*/
	ndc[0]=(rast[0]-Swsxform[ws].dx)/Swsxform[ws].scale;
	ndc[1]=(rast[1]-Swsxform[ws].dy)/Swsxform[ws].scale;
	uu_denter2(UU_GITRC,(us,"ug_dndctodev(%g %g, %d %d)",
		ndc[0],ndc[1],rast[0],rast[1]));
	uu_dexit;
}

/********************* ug_getwsxform(wsid,scale,dx,dy)**************/

int ug_getwsxform(wsid,scale,dx,dy)	/* get current wsxform */
Gws wsid;
Gfloat *scale,*dx,*dy;
{
	int irtn = 0;

	uu_denter(UU_GITRC,(us,"ug_getwsxform(wsid=%d)",wsid));

	if (wsid < 0 || wsid >= UG_MAXOPWS)
	{
		uu_dprint(UU_GITRC,(us,"ug_getwsxform: ERROR bad wsid %d", wsid));
		*scale = *dx = *dy = 0.0;
		irtn = -1;
	}
	else 
	{
		*scale = Swsxform[wsid].scale;
		*dx = Swsxform[wsid].dx;
		*dy = Swsxform[wsid].dy;
	}

	uu_dprint(UU_GITRC,(us,"%d=ug_getwsxform(wsid,scale=%g,dx=%g,dy=%g)",
		irtn,*scale,*dx,*dy));

	uu_dexit;
	return(irtn);   
}

/********************* ug_getredrwflag()**************/

int ug_getredrwflag()  /* return digs redraw flag */
{
	return(Sredrw.flag);
}

/********************* ug_getredrwrect(rect)**************/

int ug_getredrwrect(rect)  /* return digs redraw rectangle & flag */
Gnrect3 *rect;
{
	*rect = Sredrw.ndcrect;
	return(Sredrw.flag);
}

/********************* ug_getredrwrect2(rect)**************/

int ug_getredrwrect2(rect)  /* return digs 2-D redraw rectangle & flag */
Gnrect *rect;
{
	rect->ll.x = Sredrw.ndcrect.llf.x;
	rect->ll.y = Sredrw.ndcrect.llf.y;
	rect->ur.x = Sredrw.ndcrect.urb.x;
	rect->ur.y = Sredrw.ndcrect.urb.y;
	return(Sredrw.flag);
}

/********************* ug_setredrwflag(flag)**************/

void ug_setredrwflag(flag)  /* set digs redraw flag */
int flag;
{
	Sredrw.flag = flag;
}

/********************* ug_setredrwrect(rect)**************/

void  ug_setredrwrect(rect)  /* set digs redraw rectangle & flag */
Gnrect3 *rect;
{
	if (Sredrw.flag == 0)
		Sredrw.ndcrect = *rect;
	else
		ug_drectunion(&Sredrw.ndcrect,&Sredrw.ndcrect,rect);
}

/********************* ug_setredrwrect2(rect)**************/

void ug_setredrwrect2(rect)  /* set digs 2-D redraw rectangle & flag */
Gnrect *rect;
{
	Gnrect3 ndcrect;
	ndcrect.llf.x = rect->ll.x;
	ndcrect.llf.y = rect->ll.y;
	ndcrect.llf.z = Sredrw.ndcrect.llf.z;
	ndcrect.urb.x = rect->ur.x;
	ndcrect.urb.y = rect->ur.y;
	ndcrect.urb.z = Sredrw.ndcrect.urb.z;
	ug_setredrwrect(&ndcrect);
}

/********************* ug_resetredrwrect()**************/

void ug_resetredrwrect()  /* reset digs redraw rectangle & flag */
{
	Sredrw.ndcrect.llf.x = 0.;
	Sredrw.ndcrect.llf.y = 0.;
	Sredrw.ndcrect.llf.z = 1.;
	Sredrw.ndcrect.urb.x = 1.;
	Sredrw.ndcrect.urb.y = 1.;
	Sredrw.ndcrect.urb.z = 0.;
}
