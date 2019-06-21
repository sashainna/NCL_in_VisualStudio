
/*********************************************************************
**    NAME         :  wspssec.c
**    CONTAINS:
**       names of functions in file
**			uw_pswswind
**			uw_pswsvport
**			uw_pswsxform
**			uw_psndctodev
**			uw_psdevtondc
**			uw_psdrwop
**			uw_psraspnts
**			uw_psrasline
**			uw_psdmpline
**			uw_pscircle
**			uw_pspntop
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wspssec.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:14
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "wsps.h"
#include "tplot.h" 

extern Gsps uw_ps;	
static  WSPTINFO	jpbuf[MAXJP];
extern	int	uw_psind;
static  WSPTINFO  ipbuf[MAXJP]; 
extern	int	uw_psipt;
/*
.....changed by Yurong
.....declared in tplot.h
.....here use as extern
.....Yurong 8/26/97
*/
extern int wsplotdx, wsplotdy;
extern  int pts; 
/********************************************************************
**    I_FUNCTION :  uw_pswswind(prms)              
**       Set and/or resize the workstation window
**    PARAMETERS   
**       INPUT  : 
**          prms: work station info structure
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_pswswind(prms) 
struct { Gint op; Gws ws; Gnrect3 wswind;} *prms;
{
	void uw_pswsxform();
	Gfloat winheight,nominal;

	uu_denter(UU_GITRC,(us,"uw_pswswind"));

	winheight = prms->wswind.urb.y - prms->wswind.llf.y;
	nominal = winheight/ug_gksstli.wsopen[prms->ws].wdtptr->dspsize.raster.y;
	ug_gksstli.wsopen[prms->ws].wdtptr->outwdtpt->lnfac.nom = nominal;
	uu_dprint(UU_GITRC,(us,"new nominal line width = %g",nominal));

	zbytecp((*ug_gksstli.wsopen[(*prms).ws].outptr).curxform.w,(*prms).wswind);			/* structure assignment */
	uw_pswsxform(&(*ug_gksstli.wsopen[(*prms).ws].outptr).curxform,
			&uw_ps.wsxform,(*prms).ws);

	uu_dprint(UU_GITRC,(us,"uw_pswswind(%g %g,  %g %g)",(*prms).wswind.llf.x,
			(*prms).wswind.llf.y,(*prms).wswind.urb.x,(*prms).wswind.urb.y));
	uu_dexit;
}	/* uw_pswswind */


/*********************************************************************
**    I_FUNCTION :  int uw_pswsvport(prms)				
**       Sets workstation viewport.
**    PARAMETERS   
**       INPUT  : 
**          prms: work station info structure
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_pswsvport(prms)
struct {Gint op; Gws ws; Gdrect3 wsvport;} *prms;

{
	void uw_pswsxform();
	uu_denter(UU_GITRC,(us," uw_pswsvport(%g %g,  %g %g,%d,%d)",(*prms).wsvport.llf.x,
	(*prms).wsvport.llf.y,(*prms).wsvport.urb.x,(*prms).wsvport.urb.y,
	(*prms).ws,uw_ps.isrotate));
	zbytecp((*ug_gksstli.wsopen[(*prms).ws].outptr).curxform.v,(*prms).wsvport);				/* structure assignment */
	uw_pswsxform(&(*ug_gksstli.wsopen[(*prms).ws].outptr).curxform,
			&uw_ps.wsxform,(*prms).ws);	/* update wsxform */

	if (strcmp(TP_size,"AV") == 0) uw_ps.isrotate = UU_FALSE;
	else uw_ps.isrotate = UU_TRUE;

	uu_dprint(UU_GITRC,(us," uw_pswsvport(%g %g,  %g %g)",(*prms).wsvport.llf.x,
			(*prms).wsvport.llf.y,(*prms).wsvport.urb.x,(*prms).wsvport.urb.y));
	uu_dexit;
}	/* uw_pswsvport */




/*********************************************************************
**    I_FUNCTION :  int uw_pswsxform(wstran,wsxform,ws)	
**       Calculate the wsxform scale and dx,dy to perform the
**       workstation transformation from window to viewport to raster
**       and resize the actual window to the new size.
**    PARAMETERS   
**       INPUT  : 
**          wstran   =  Input transformation to apply to window.
**          ws       =  Active window for transformations.
**       OUTPUT :  
**          wsxform  =  Output transformation for window.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_pswsxform(wstran,wsxform,ws)	
Gwstran3 *wstran;					
struct {Gfloat scale; Gfloat dx,dy;} *wsxform;
Gws ws;
{
	char us[160];
	Gfloat sx,sy,sx2,sy2,s1,s2,s;
	int ll[2],ur[2];
	int devxmax;
	Gnrect3 *wp;
	Gdrect3 *vp;
	Gfloat y,waspect,wdx,wdy;
	UU_REAL	ratio;

	devxmax= (*ug_gksstli.wsopen[ws].wdtptr).dspsize.raster.x;
	wp= &(*wstran).w;	
	vp= &(*wstran).v;
	uu_denter2(UU_GITRC,(us,"uw_pswsxfm,wp=%g,%g,%g,%g,wv=%g,%g,%g,%g",
	(*wp).urb.x,(*wp).llf.x, (*wp).urb.y,(*wp).llf.y,
	(*vp).urb.x,(*vp).llf.x, (*vp).urb.y,(*vp).llf.y));
	uu_dexit;

	wdx=(*wp).urb.x-(*wp).llf.x;
	wdy=(*wp).urb.y-(*wp).llf.y;
	sx=((*vp).urb.x-(*vp).llf.x)/wdx;
	sy=((*vp).urb.y-(*vp).llf.y)/wdy;
	s1=(sx<sy) ? sx : sy;
	ratio = devxmax/(*ug_gksstli.wsopen[ws].wdtptr).dspsize.device.x;	
	s=s1*ratio;
	(*wsxform).scale=s;	
	(*wsxform).dx=ratio*(*vp).llf.x-s*(*wp).llf.x;
	(*wsxform).dy=ratio*(*vp).llf.y-s*(*wp).llf.y;

	ll[0]=(*wp).llf.x*devxmax; ll[1]=(*wp).llf.y*devxmax;
	ur[0]=(*wp).urb.x*devxmax; ur[1]=(*wp).urb.y*devxmax;
	uu_denter2(UU_GITRC,(us,
			"uw_pswsxform. wswind=%g %g,%g %g, scale=%g, dx,dy=%g %g",
			(*wp).llf.x,(*wp).llf.y,(*wp).urb.x,
			(*wp).urb.y,(*wsxform).scale,(*wsxform).dx,(*wsxform).dy));
	uu_dexit;
	ll[0]=(*wp).llf.x*s+(*wsxform).dx; 
	ll[1]=(*wp).llf.y*s+(*wsxform).dy;
	ur[0]=(*wp).urb.x*s+(*wsxform).dx; 
	ur[1]=(*wp).urb.y*s+(*wsxform).dy;
	uw_ps.vpy = ur[1];
	uu_denter2(UU_GITRC,(us,"uw_pswsxform. adjusted wsvport=%d %d %d %d",
		ll[0],ll[1],ur[0],ur[1]));
	uu_dexit;
}	/* uw_pswsxform */
		

/*********************************************************************
**    I_FUNCTION :  uw_psndctodev(f,i,ws)  ----------  UG_DNDCTODEV
**       Convert ndc coordinates to device(raster).
**    PARAMETERS   
**       INPUT  : 
**          f[2] = ndc coordinates.
**			Gws ws: workstation id
**       OUTPUT :  
**          i[2] = ndc[2] converted to raster.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_psndctodev(f,i,ws)	
int i[2];
Gfloat f[2];
Gws ws;		
{
	char us[180];
	i[0]=uw_ps.wsxform.scale*f[0]+uw_ps.wsxform.dx+wsplotdx+.5;
	i[1]=uw_ps.wsxform.scale*f[1]+uw_ps.wsxform.dy+wsplotdy+.5;
	uu_denter2(UU_GITRC,(us,"uw_psndctodev(%g %g, %d %d)",
		f[0],f[1],i[0],i[1]));
	uu_dexit;
}	/* uw_psmdctodev */

		

/*********************************************************************
**    I_FUNCTION :  uw_psdevtondc(rast,ndc,ws)		
**       Convert device coordinates to ndc.
**    PARAMETERS   
**       INPUT  : 
**          rast[2] = raster coordinates
**			Gws ws: workstation id
**       OUTPUT :  
**          ndc[2] = rast[2] converted to ndc.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_psdevtondc(rast,ndc,ws)	
int rast[2];
Gfloat ndc[2];
Gws ws;
{
	int y;
	char us[180];
	ndc[0]=(rast[0]-uw_ps.wsxform.dx)/uw_ps.wsxform.scale;
	ndc[1]=(rast[1]-uw_ps.wsxform.dy)/uw_ps.wsxform.scale;
	uu_denter2(UU_GITRC,(us,"uw_psdevtondc(%g %g, %d %d)",
		ndc[0],ndc[1],rast[0],rast[1]));
	uu_dexit;
}	/* uw_psdevtondc */

/*********************************************************************
**    I_FUNCTION :  uw_psdrwop(prms)					
**		draw line from prms[2],prms[3] to [4],[5], NDC coordinates. Use
**		the current line color and linestyle. 
**    PARAMETERS   
**       INPUT  : 
**          prms: store info about the line
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_psdrwop(prms)
UG_line *prms;

{
	int	xy1[3], xy2[3], color,wid,typ; 
	int	tranlx,tranly,tmpx, tmpy;
	char	num[20];
	char	us[120];
	void uw_psdmpline();

	uw_psndctodev(&(*prms).p1,xy1,uw_ps.wid);
	uw_psndctodev(&(*prms).p2,xy2,uw_ps.wid);
	if (uw_ps.isrotate)
	{
		tmpx = -xy1[1] + uw_ps.vpy + wsplotdy*2;
		tmpy = xy1[0];
		xy1[0] = tmpx;		xy1[1] = tmpy;
		tmpx = -xy2[1] + uw_ps.vpy + wsplotdy*2;
		tmpy = xy2[0];
		xy2[0] = tmpx;		xy2[1] = tmpy;
		uu_denter2(UU_GITRC,(us, "psdrwopa,xy1=%d,%d,xy2=%d,%d",
		xy1[0],xy1[1],xy2[0],xy2[1]));
		uu_dexit; 
	}
	xy1[0] = xy1[0] - DEVXHALF;
	xy1[1] = xy1[1] - DEVYHALF;
	xy2[0] = xy2[0] - DEVXHALF;
	xy2[1] = xy2[1] - DEVYHALF;
	color = ug_gksstli.curprats.lnbundl.color;
	if (color > 255)   color = 1;
	typ = ug_gksstli.curprats.lnbundl.type.typeno;
	if (typ < 1 || typ > 8) typ = 1;
	wid = (int)ug_gksstli.curprats.lnbundl.width*plotopts.linewt/psrate_x;
	if (wid < 0 ) wid  = 1/psrate_x;
	jpbuf[uw_psind].color = color;
	jpbuf[uw_psind].type = typ;
	jpbuf[uw_psind].width = wid;
	jpbuf[uw_psind].pt1[0] = xy1[0];
	jpbuf[uw_psind].pt1[1] = xy1[1];
	jpbuf[uw_psind].pt2[0] = xy2[0];
	jpbuf[uw_psind].pt2[1] = xy2[1];
	uw_psind++;

	if ((uw_psind+uw_psipt) == MAXJP)
	{
		uw_psdmpline();
		uw_psind = 0;
	}
}	/* uw_psdrwop */

/*********************************************************************
**    I_FUNCTION :  uw_psraspnts()
**       Plot the information in the buffer
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_psraspnts()

{
	int	a, i, j, first;
	char	num[30];
	static short Ltype = -1;    
	static int Lcolor = -1;	   
	uu_denter(UU_GITRC,(us, "psdmpline,uw_psind=%d", uw_psind));

	a = 0;
	i = 0;
	utp_ttputps(uw_ps.ttfd,"/Wx 0 def\n", 10);
	utp_ttputps(uw_ps.ttfd,"/Wy 0 def\n", 10); 
	for (j=a; j<uw_psind; j++)
	{	
		if (ipbuf[j].type==0) break;
		if (ipbuf[j].color!=Lcolor)	
		{	
			Lcolor= ipbuf[j].color;
			uw_pspen (ipbuf[j].color);
		}
		if (ipbuf[j].type!=Ltype)
		{	
			Ltype = ipbuf[j].type;
			sprintf (num,"(%c) stringwidth Wx Wy\n",ipbuf[j].type);
        	utp_ttputps(uw_ps.ttfd,num,strlen(num));
		}
		sprintf(num, "%d Wx sub %d ", ipbuf[j].pt1[0], ipbuf[j].pt1[1]);
		utp_ttputps(uw_ps.ttfd,num,strlen(num));
		utp_ttputps(uw_ps.ttfd,"moveto\n  ",7);  
		sprintf (num,"(%c) show\n",ipbuf[j].type);
		utp_ttputps(uw_ps.ttfd,num,strlen(num));
	}
	uu_dexit;
}	/* uw_psraspnts  */


/*********************************************************************
**    I_FUNCTION :  uw_psrasline(x1,y1,x2,y2)
**       Draw a raster line from x1,y1 to x2,y2
**    PARAMETERS   
**       INPUT  : 
**         x1,y1,x2,y2: dev cord of two points 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_psrasline(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
	void uw_psdmpline();
	int color,wid,typ;
	color = ug_gksstli.curprats.lnbundl.color;
	if (color > 255)   color = 1;
	typ = ug_gksstli.curprats.lnbundl.type.typeno;
	if (typ < 1 || typ > 8) typ = 1;
	wid = (int)ug_gksstli.curprats.lnbundl.width*plotopts.linewt/psrate_x;
	if (wid < 0 ) wid  = 1/psrate_x;
	jpbuf[uw_psind].color = color;
	jpbuf[uw_psind].type = typ;
	jpbuf[uw_psind].width = wid;
	jpbuf[uw_psind].pt1[0] = x1;
	jpbuf[uw_psind].pt1[1] = y1;
	jpbuf[uw_psind].pt2[0] = x2;
	jpbuf[uw_psind].pt2[1] = y2;
	uw_psind++;
	if ((uw_psind+uw_psipt) == MAXJP)
	{
		uw_psdmpline();
		uw_psind = 0;
	}
}

/*********************************************************************
**    I_FUNCTION :  uw_psdmpline()
**       Plot the information in the buffer
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_psdmpline()
{
	int	a, i, j, k, first;
	char	num[30];
	Gfloat	prept[3];
	Gfloat   firstpoint[3];	
	uu_denter(UU_GITRC,(us, "psdmpline,uw_psind=%d", uw_psind));
	
	a = 0;
	i = 0;
	k = 0;
	prept[0] = jpbuf[0].pt1[0];
	prept[1] = jpbuf[0].pt1[1];
	first = UU_TRUE;
	for (j=a; j<uw_psind; j++)
	{
		if (first)
		{
			first = UU_FALSE;
			firstpoint[0] = jpbuf[j].pt1[0];
			firstpoint[1] = jpbuf[j].pt1[1];
			utp_ttputps(uw_ps.ttfd,"newpath\n", 8);
			uw_pspen (jpbuf[j].color);
			uw_pslintype(jpbuf[j].type);
			uw_pslinwid(jpbuf[j].width);
			sprintf(num, "%d %d ", jpbuf[j].pt1[0], jpbuf[j].pt1[1]);
 			utp_ttputps(uw_ps.ttfd,num,strlen(num));
			utp_ttputps(uw_ps.ttfd,"moveto\n  ",7);	
		}
		if (a==j) 	a++;
		if ((prept[0]!=jpbuf[j].pt1[0])||(prept[1]!=jpbuf[j].pt1[1]))
		{  
			firstpoint[0] = jpbuf[j].pt1[0];
			firstpoint[1] = jpbuf[j].pt1[1];
			utp_ttputps(uw_ps.ttfd,"stroke\n", 7);
			utp_ttputps(uw_ps.ttfd,"newpath\n", 8);
			uw_pspen (jpbuf[j].color);
			uw_pslintype(jpbuf[j].type);
			uw_pslinwid(jpbuf[j].width);
			sprintf(num, "%d %d ", jpbuf[j].pt1[0], jpbuf[j].pt1[1]);
 			utp_ttputps(uw_ps.ttfd,num,strlen(num));
 			utp_ttputps(uw_ps.ttfd,"moveto\n",7);	
		}
		
		sprintf(num, "%d %d ", jpbuf[j].pt2[0], jpbuf[j].pt2[1]);
 		utp_ttputps(uw_ps.ttfd,num,strlen(num));
 		utp_ttputps(uw_ps.ttfd,"lineto\n",7);	
		prept[0] = jpbuf[j].pt2[0];		/* save the last point  */
		prept[1] = jpbuf[j].pt2[1];
		if ((prept[0]==firstpoint[0])&&(prept[1]==firstpoint[1]))
			utp_ttputps(uw_ps.ttfd,"closepath\n",10);
	}

	utp_ttputps(uw_ps.ttfd,"stroke\n", 7);
	uu_dexit;
}	/* uw_psdmpline  */

/*********************************************************************
**    I_FUNCTION :  uw_pscircle(x1, y1, r,fill)
**       Draw a circle
**    PARAMETERS   
**       INPUT  : 
**         x1,y1: circle center line
**			r: radias
**			file: file or not 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_pscircle(x1, y1, r,fill)
int x1, y1, r, fill;
{	static Lcolor = -1;
 	static Ltype = -1;
	static Lwid = -1;
	int wid, typ, color;
	char num[30];
	color = ug_gksstli.curprats.mkbundl.color;
	if (color>255)  
		color=1;
	typ = ug_gksstli.curprats.lnbundl.type.typeno;
	if (typ < 1 || typ > 8) 
		typ = 1;
	wid = (int)ug_gksstli.curprats.lnbundl.width*plotopts.linewt/psrate_x;
	if (wid < 0 ) wid  = 1/psrate_x;
	if (wid!=Lwid)
	{	
		Lwid = wid;
		uw_pslinwid(wid);
	}
	if (typ!=Ltype)
	{
		Ltype = typ;
		uw_pslintype(typ);
	}
	if (color!=Lcolor)
	{
		Lcolor = color;
		uw_pspen (color);
	}
	utp_ttputps(uw_ps.ttfd,"newpath\n", 8); 
	sprintf(num, "%d %d %d 0 360 arc\n", x1, y1, r);
	utp_ttputps(uw_ps.ttfd,num,strlen(num));
	utp_ttputps(uw_ps.ttfd,"closepath\n",10);
	if (fill!=0)
   	utp_ttputps(uw_ps.ttfd,"fill\n", 5);
	utp_ttputps(uw_ps.ttfd,"stroke\n",7);
}


/*********************************************************************
**    I_FUNCTION : uw_pspntop(prms) ---------> UG_DPNTOP
**			
**	draw a marker of type (*prms).type centered at point (*prms).p in 
**	NDC coordinates. Use the current marker color index. Use the
**	marker type specified in (*prms).type. The predefined marker types
**	are:
**		1 = a small dot.
**		2 = a plus sign.
**		3 = an asterisk (*).
**		4 = a zero (0).
**		5 = a capital X.
**      7 = a diamond.
**
**    PARAMETERS   
**       INPUT  : 
**          prms: marker info structure
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_pspntop(prms)
UG_marker *prms;	
{
	char *mtype = {".+*0X"};

	int	xy1[3], pnts ; 
	int	tmpx, tmpy;
	char	num[20];
	int     first, i;
	int  wid,typ,color;
	Gfloat pointsx[10];
	Gfloat pointsy[10];
	uu_denter(UU_GITRC,(us,"uw_pspntop()"));
	uu_dexit;

	first = UU_TRUE;
	if (first)
	{
		pts = UU_TRUE;
		first = UU_FALSE;
	}
	uw_psndctodev(&(*prms).p,xy1,uw_ps.wid);
	if (uw_ps.isrotate)
	{
		tmpx = -xy1[1] + uw_ps.vpy + wsplotdy*2;
		tmpy = xy1[0];
		xy1[0] = tmpx;		xy1[1] = tmpy;
	}
	xy1[0] = xy1[0] - DEVXHALF;
	xy1[1] = xy1[1] - DEVYHALF;
									/* save the information to the buffer */
	color = ug_gksstli.curprats.mkbundl.color;
	uu_denter2(UU_GITRC,(us, "pscolor,color=%d", color));
	if (color > 255)   color = 1;
	jpbuf[uw_psind].color = color;
	typ = ug_gksstli.curprats.lnbundl.type.typeno;
	if (typ < 1 || typ > 8) typ = 1;
	wid = (int)ug_gksstli.curprats.lnbundl.width*plotopts.linewt/psrate_x;
	if (wid < 0 ) wid  = 1/psrate_x;
	switch((*prms).type)
	{
 /*
.....Dot (tiny plus)
*/
	case 1:
		pointsx[0]=xy1[0]-1/psrate_x + 0.5; pointsy[0]=xy1[1];
		pointsx[1]=xy1[0]+1/psrate_x + 0.5; pointsy[1]=xy1[1];
		pointsx[2]=xy1[0]; pointsy[2]=xy1[1]-1/psrate_y + 0.5;
		pointsx[3]=xy1[0]; pointsy[3]=xy1[1]+1/psrate_y + 0.5;
		pnts = 4;  
		break;
/*
.....Plus
*/
	case 2:
		pointsx[0]=xy1[0]-4/psrate_x + 0.5; pointsy[0]=xy1[1];
		pointsx[1]=xy1[0]+4/psrate_x + 0.5; pointsy[1]=xy1[1];
		pointsx[2]=xy1[0];   pointsy[2]=xy1[1]+4/psrate_y + 0.5;
		pointsx[3]=xy1[0];   pointsy[3]=xy1[1]-4/psrate_y + 0.5;
		pnts = 4;
		break;
/*
.....Asterisk
*/
	case 3:
		pointsx[0]=xy1[0]-6/psrate_x + 0.5; pointsy[0]=xy1[1];
		pointsx[1]=xy1[0]+6/psrate_x + 0.5; pointsy[1]=xy1[1];
		pointsx[2]=xy1[0]-4/psrate_x + 0.5; pointsy[2]=xy1[1]+4/psrate_y + 0.5;
		pointsx[3]=xy1[0]+4/psrate_x + 0.5; pointsy[3]=xy1[1]-4/psrate_y + 0.5;
		pointsx[4]=xy1[0];   pointsy[4]=xy1[1]+6/psrate_y + 0.5;
		pointsx[5]=xy1[0];   pointsy[5]=xy1[1]-6/psrate_y + 0.5;
		pointsx[6]=xy1[0]+4/psrate_x + 0.5; pointsy[6]=xy1[1]+4/psrate_y + 0.5;
		pointsx[7]=xy1[0]-4/psrate_x + 0.5; pointsy[7]=xy1[1]-4/psrate_y + 0.5;
		pnts = 8;
		break;
/*
.....Circle
*/
   case 4:
    	uw_pscircle(xy1[0], xy1[1],3/psrate_x + 0.5, 0);
		break;
/*
.....Cross:X
*/
	case 5:
		pointsx[0]=xy1[0]-4/psrate_x + 0.5; pointsy[0]=xy1[1]+4/psrate_y + 0.5;
		pointsx[1]=xy1[0]+4/psrate_x + 0.5; pointsy[1]=xy1[1]-4/psrate_y + 0.5;
		pointsx[2]=xy1[0]+4/psrate_x + 0.5; pointsy[2]=xy1[1]+4/psrate_y + 0.5;
		pointsx[3]=xy1[0]-4/psrate_x + 0.5; pointsy[3]=xy1[1]-4/psrate_y + 0.5;
 		pnts = 4;
		break;
/*
.....Diamond
*/
	case 7:
		pointsx[0]=xy1[0];   pointsy[0]=xy1[1]+4/psrate_y + 0.5;
		pointsx[1]=xy1[0]+4/psrate_x + 0.5; pointsy[1]=xy1[1];
		pointsx[2]=xy1[0]+4/psrate_x + 0.5; pointsy[2]=xy1[1];
		pointsx[3]=xy1[0];   pointsy[3]=xy1[1]-4/psrate_y + 0.5;
		pointsx[4]=xy1[0];   pointsy[4]=xy1[1]-4/psrate_y + 0.5;
		pointsx[5]=xy1[0]-4/psrate_x + 0.5; pointsy[5]=xy1[1];
		pointsx[6]=xy1[0]-4/psrate_x + 0.5; pointsy[6]=xy1[1];
		pointsx[7]=xy1[0];   pointsy[7]=xy1[1]+4/psrate_y + 0.5;
		pnts = 8;	
      break;
	}

	if ((*prms).type!=4)
	{
		for( i = 0; i<pnts; i+=2)
		{
			jpbuf[uw_psind].color = color;
			jpbuf[uw_psind].type = typ;
			jpbuf[uw_psind].width = wid;
			jpbuf[uw_psind].pt1[0] = pointsx[i];
			jpbuf[uw_psind].pt1[1] = pointsy[i];
			jpbuf[uw_psind].pt2[0] = pointsx[i+1];
			jpbuf[uw_psind].pt2[1] = pointsy[i+1];
			uw_psind++;
		}
		if ((uw_psind+uw_psipt) == MAXJP)
		{
			uw_psdmpline();
			uw_psind = 0;
		}
	}
}



