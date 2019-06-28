
/********************************************************************* 
**  NAME:  ws104sec.c
**
**      	secondary functions section.
**
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws104sec.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:04
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "ws104.h"
#include "tplot.h"   /* added kathy */

extern Gs104 uw_104;			/* declare workstation local data */
WSPTINFO	jpbuf[MAXJP];		/* information from pass 1 */
/* int jpind = 0;							/* index to jpbuf */
extern int	uw_1043ind;		/* index to jpbuf */
								/* decimal delta component codes */
static int deltacmd[7][7] = 
			{ {17, 41, 45, 28, 44, 40, 16},
			  {49, 21, 57, 32, 56, 20, 48},
			  {53, 61, 25, 36, 24, 60, 52},
			  {29, 33, 37, 0,  38, 34, 30},
			  {55, 63, 27, 39, 26, 62, 54},
			  {51, 23, 59, 35, 58, 22, 50},
			  {19, 43, 47, 31, 46, 42, 18}
			};


/*********************************************************************
**    I_FUNCTION :  uw_104wswind(prms)              
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_104wswind(prms)              /* set workstation window. */
struct { Gint op; Gws ws; Gnrect3 wswind;} *prms;
{
	Gfloat winheight,nominal;

	uu_denter(UU_GITRC,(us,"uw_104wswind."));

	/* reset nominal line width */
	winheight = prms->wswind.urb.y - prms->wswind.llf.y;
	nominal = winheight/ug_gksstli.wsopen[prms->ws].wdtptr->dspsize.raster.y;
	ug_gksstli.wsopen[prms->ws].wdtptr->outwdtpt->lnfac.nom = nominal;
	uu_dprint(UU_GITRC,(us,"new nominal line width = %g",nominal));

	zbytecp((*ug_gksstli.wsopen[(*prms).ws].outptr).curxform.w,(*prms).wswind);			/* structure assignment */
	uw_104wsxform(&(*ug_gksstli.wsopen[(*prms).ws].outptr).curxform,
			&uw_104.wsxform,(*prms).ws);

	uu_dprint(UU_GITRC,(us,"uw_104wswind(%g %g,  %g %g)",(*prms).wswind.llf.x,
			(*prms).wswind.llf.y,(*prms).wswind.urb.x,(*prms).wswind.urb.y));
	uu_dexit;
}	/* uw_104wswind */



/*********************************************************************
**    I_FUNCTION :  int uw_104wsvport(prms)				
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_104wsvport(prms)				/* set workstation viewport */
struct {Gint op; Gws ws; Gdrect3 wsvport;} *prms;

{
	uu_denter(UU_GITRC,(us," uw_104wsvport(%g %g,  %g %g,%d,%d)",(*prms).wsvport.llf.x,
	(*prms).wsvport.llf.y,(*prms).wsvport.urb.x,(*prms).wsvport.urb.y,
	(*prms).ws,uw_104.isrotate));
	zbytecp((*ug_gksstli.wsopen[(*prms).ws].outptr).curxform.v,(*prms).wsvport);				/* structure assignment */
	uw_104wsxform(&(*ug_gksstli.wsopen[(*prms).ws].outptr).curxform,
			&uw_104.wsxform,(*prms).ws);	/* update wsxform */

/*
.....Check for "A VERT" paper size, instead of X-axis travel greater
.....than a hard coded number, to handle rotation
.....Bobby  -  8/26/91
*/
	if (strcmp(TP_size,"AV") == 0) uw_104.isrotate = UU_TRUE;
	else uw_104.isrotate = UU_FALSE;

/*--- since drawing has A-horz and A-vert, doesn't have to rotate here */
/* uncommented. kathy */
/*	if ((!uw_104.isrotate)&&(prms->wsvport.urb.x < 0.3))
/*		 uw_104.isrotate = UU_TRUE;
/*	else
/*		if ((uw_104.isrotate)&&(prms->wsvport.urb.x > 0.3))
/*		 	uw_104.isrotate = UU_FALSE;
/* end. kathy */

	uu_dprint(UU_GITRC,(us," uw_104wsvport(%g %g,  %g %g)",(*prms).wsvport.llf.x,
			(*prms).wsvport.llf.y,(*prms).wsvport.urb.x,(*prms).wsvport.urb.y));
	uu_dexit;
}	/* uw_104wsvport */




/*********************************************************************
**    I_FUNCTION :  int uw_104wsxform(wstran,wsxform,ws)	
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_104wsxform(wstran,wsxform,ws)	
										/* calculate wsxform =scale and dx,dy to 
											perform the workstation transformation
											from window to vport to raster */
Gwstran3 *wstran;					/* workstation window and viewport */
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

	devxmax=(*ug_gksstli.wsopen[ws].wdtptr).dspsize.raster.x;
	wp= &(*wstran).w;						/* get pointers to ws window, viewport */
	vp= &(*wstran).v;
	uu_denter2(UU_GITRC,(us,"uw_104wsxfm,wp=%g,%g,%g,%g,wv=%g,%g,%g,%g",
	(*wp).urb.x,(*wp).llf.x, (*wp).urb.y,(*wp).llf.y,
	(*vp).urb.x,(*vp).llf.x, (*vp).urb.y,(*vp).llf.y));
	uu_dexit;

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
			"uw_104wsxform. wswind=%g %g,%g %g, scale=%g, dx,dy=%g %g",
			(*wp).llf.x,(*wp).llf.y,(*wp).urb.x,
			(*wp).urb.y,(*wsxform).scale,(*wsxform).dx,(*wsxform).dy));
	uu_dexit;
	/* ll,ur= adjusted viewport in integer coords */
	ll[0]=(*wp).llf.x*s+(*wsxform).dx; 
	ll[1]=(*wp).llf.y*s+(*wsxform).dy;
	ur[0]=(*wp).urb.x*s+(*wsxform).dx; 
	ur[1]=(*wp).urb.y*s+(*wsxform).dy;
	uw_104.vpy = ur[1];
	uu_denter2(UU_GITRC,(us,"uw_104wsxform. adjusted wsvport=%d %d %d %d",
		ll[0],ll[1],ur[0],ur[1]));
	uu_dexit;
}	/* uw_104wsxform */
		

/*********************************************************************
**    I_FUNCTION :  uw_104ndctodev(f,i,ws)	
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_104ndctodev(f,i,ws)	/* convert (fx,fy) from ndc to (ix,iy) 
											raster coords */
int i[2];
Gfloat f[2];
Gws ws;							/* workstation id */
{
	char us[180];
	extern int wsplotdx, wsplotdy;  /* for placement of the origion. kathy */

	i[0]=uw_104.wsxform.scale*f[0]+uw_104.wsxform.dx+.5+wsplotdx;
	i[1]=uw_104.wsxform.scale*f[1]+uw_104.wsxform.dy+.5+wsplotdy;
	uu_denter2(UU_GITRC,(us,"uw_104ndctodev(%g %g, %d %d)",
		f[0],f[1],i[0],i[1]));
	uu_dexit;
}	/* uw_104ndctodev */

		

/*********************************************************************
**    I_FUNCTION :  uw_104devtondc(rast,ndc,ws)		
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_104devtondc(rast,ndc,ws)		/* convert from raster to ndc coords */
int rast[2];
Gfloat ndc[2];
Gws ws;
{
	int y;
	char us[180];
	ndc[0]=(rast[0]-uw_104.wsxform.dx)/uw_104.wsxform.scale;
	ndc[1]=(rast[1]-uw_104.wsxform.dy)/uw_104.wsxform.scale;
	uu_denter2(UU_GITRC,(us,"uw_104devtondc(%g %g, %d %d)",
		ndc[0],ndc[1],rast[0],rast[1]));
	uu_dexit;
}	/* uw_104devtondc */

/*------------------------- local declarations --------------------------*/

/* secondary entries (lower level, called by wsdev.c simulation routines) */

/*---------------------------------------------> UG_DDRWOP */
/*********************************************************************
**    I_FUNCTION :  uw_104drwop(prms)					
**		draw line from prms[2],prms[3] to [4],[5], NDC coordinates. Use
**		the current line color and linestyle. 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_104drwop(prms)					/* line (2d) */
UG_line *prms;

{
	int	xy1[3], xy2[3];

	/*draw line from prms->p1 to prms->p2, NDC coordinates */
	uu_denter(UU_GITRC,(us, "104drwop(%g,%g,%g,%g)",(*prms).p1.x,(*prms).p1.y,(*prms).p2.x,(*prms).p2.y)); 
	uu_dexit; 
	uw_104ndctodev(&(*prms).p1,xy1,uw_104.wid);
	uw_104ndctodev(&(*prms).p2,xy2,uw_104.wid);
	uwi_104drwop1(xy1,xy2);
}	/* uw_104drwop */


/*********************************************************************
**    I_FUNCTION :  uwi_104drwop1(xy1,xy2)
**       This routine is separated from uw_104drwop in order to be able
**			to be called by polylnras.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uwi_104drwop1(xy1,xy2)
int	xy1[], xy2[];

{
	int	color; 
	int	tranlx,tranly,tmpx, tmpy;
	char	num[20];

	uu_denter(UU_GITRC,(us, "104drwop1,xy1=%d,%d; xy2=%d,%d",
		xy1[0],xy1[1],xy2[0],xy2[1]));
	if (uw_104.isrotate)			/* for A size plot rotate 90 deg */
	  {
		tmpx = -xy1[1] + uw_104.vpy;
		tmpy = xy1[0];
		xy1[0] = tmpx;		xy1[1] = tmpy;
		tmpx = -xy2[1] + uw_104.vpy;
		tmpy = xy2[0];
		xy2[0] = tmpx;		xy2[1] = tmpy;
	uu_dprint(UU_GITRC,(us, "104drwopa,xy1=%d,%d,xy2=%d,%d",
		xy1[0],xy1[1],xy2[0],xy2[1]));
	  }
									/* save the information to the buffer */
	color = ug_gksstli.curprats.lnbundl.color;
	uu_dprint(UU_GITRC,(us, "104color,color=%d", color));
	if (color > 8)   color = 1;
	jpbuf[uw_1043ind].type = LINE104;
	jpbuf[uw_1043ind].color = color;
	jpbuf[uw_1043ind].lntype = uw_104.lntype;
	uu_dprint(UU_GITRC,(us, "104drwopa,lntype=%d", uw_104.lntype));
	jpbuf[uw_1043ind].pt1[0] = xy1[0];
	jpbuf[uw_1043ind].pt1[1] = xy1[1];
	jpbuf[uw_1043ind].pt2[0] = xy2[0];
	jpbuf[uw_1043ind].pt2[1] = xy2[1];
	uw_1043ind++;
	if (uw_1043ind >= MAXJP)
		{
		 uw_104dmpline();
		 uw_1043ind = 0;
		}
	uu_dexit;
}	/* uwi_104drwop1 */



/*********************************************************************
**    I_FUNCTION :  uw_104pntop(prms)					
**		plot a point 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_104pntop(prms)					
UG_marker *prms;                /* marker */

{
	int	 xy1[3];
	int	 color,len; 
	int	 tranlx,tranly,tmpx, tmpy;
	char	num[20];
	static UU_LOGICAL first=UU_TRUE;

	uu_denter(UU_GITRC,(us, "104pntop(%g,%g)",(*prms).p.x,(*prms).p.y)); 
	uu_dexit; 

	uw_104ndctodev(&(*prms).p,xy1,uw_104.wid);

	if (uw_104.isrotate)			/* for A size plot rotate 90 deg */
	  {
		tmpx = -xy1[1] + uw_104.vpy;
		tmpy = xy1[0];
		xy1[0] = tmpx;		xy1[1] = tmpy;
	  }


	xy1[0] = xy1[0];
	xy1[1] = xy1[1];
	/* save the information to the buffer */
	color = ug_gksstli.curprats.mkbundl.color;
	uu_dprint(UU_GITRC,(us, "104color,color=%d", color));

	if (color > 8)   color = 1;
	jpbuf[uw_1043ind].type = LINE104;
	jpbuf[uw_1043ind].color = color;
	jpbuf[uw_1043ind].lntype = uw_104.lntype;
	jpbuf[uw_1043ind].pt1[0] = xy1[0]-150;
	jpbuf[uw_1043ind].pt1[1] = xy1[1];
	jpbuf[uw_1043ind].pt2[0] = xy1[0]+150;
	jpbuf[uw_1043ind].pt2[1] = xy1[1];
	uw_1043ind++;
	jpbuf[uw_1043ind].type = LINE104;
	jpbuf[uw_1043ind].color = color;
	jpbuf[uw_1043ind].lntype = uw_104.lntype;
	jpbuf[uw_1043ind].pt1[0] = xy1[0];
	jpbuf[uw_1043ind].pt1[1] = xy1[1]-150;
	jpbuf[uw_1043ind].pt2[0] = xy1[0];
	jpbuf[uw_1043ind].pt2[1] = xy1[1]+150;
	uw_1043ind++;

	if (uw_1043ind >= MAXJP)
		{
		 uw_104dmpline();
		 uw_1043ind = 0;
		}
}	/* uw_104pntop */


/*
.....Added new uw_104drasline routine which
.....accepts the standard parameters used to
.....call UG_DRASLINE.  Renamed old routine to
.....uw_104dmpline
.....Bobby  -  8/26/91
*/

/*********************************************************************
**    I_FUNCTION :  uw_104drasline()
**       Plot the information in the buffer
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_104drasline(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
	int color;
	color = ug_gksstli.curprats.lnbundl.color;
	if (color > 8) color = 1;
	jpbuf[uw_1043ind].type = LINE104;
	jpbuf[uw_1043ind].color = color;
	jpbuf[uw_1043ind].lntype = uw_104.lntype;
    jpbuf[uw_1043ind].pt1[0] = x1;
    jpbuf[uw_1043ind].pt1[1] = y1;
    jpbuf[uw_1043ind].pt2[0] = x2;
    jpbuf[uw_1043ind].pt2[1] = y2;
    uw_1043ind++;
    if (uw_1043ind == MAXJP)
    {
        uw_104dmpline();
        uw_1043ind = 0;
    }
}

/*********************************************************************
**    I_FUNCTION :  uw_104dmpline()
**       Plot the information in the buffer
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_104dmpline()

{
	int	a, i, j, first;
	char	numch[30];
	char	penup[2];				/* pen up command */
	char	pendown[2];				/* pen down command */
/*	char	penselect[3];			/* pen select command */
	char	bypass[4];				/* dash bypass command */
	int	origin[2];
	short	lntype;					/* the current line type */
	static UU_LOGICAL	atbegin = UU_TRUE;	/* the begining of the program */

	uu_denter(UU_GITRC,(us, "104dmpline,uw_1043ind=%d", uw_1043ind));

	penup[0] = BIAS104 + 3;			/* pen up command */
	pendown[0] = BIAS104 + 2;		/* pen down command */
/*   penselect[0] = BIAS104 + 4;	/* pen select command */
	bypass[0] = BIAS104 + 11;     /* the dash bypass command */
	bypass[1] = BIAS104 + 8;      /* the dash bypass command */
	a = 0;
	if ((atbegin)&&(jpbuf[0].pt1[0]!=0)&&(jpbuf[0].pt1[1]!=0))
	  {
		origin[0] = 0;
		origin[1] = 0;
	  	uu_ttput(uw_104.ttfd,penup,1);			/* pen up */
		buflen++;
		uw_104mvpen(origin,jpbuf[0].pt1);
		prept[0] = jpbuf[0].pt1[0];
		prept[1] = jpbuf[0].pt1[1];
		lntype = jpbuf[0].lntype;
		uwi_104lntype(lntype);
		buflen = buflen + 2;
		atbegin = UU_FALSE;
		/*--- select line type 
		lntype = jpbuf[0].lntype;
		bypass[2] = lntype + BIAS104;
	   uu_ttput(uw_104.ttfd,bypass,3);			
		buflen = buflen + 3;
		---*/
	  }
	for (i=1; i<9; i++)
	 {
	  first = UU_TRUE;
	  for (j=a; j<uw_1043ind; j++)
		 {
		  if (jpbuf[j].color == i)
			 {
			  if (first)
				 {
				  first = UU_FALSE;
/*
.....Select pen color
.....Bobby  -  8/26/91
*/
					uw_104pen(i);
/*				  penselect[1] = BIAS104 + i;					/* pen number */
/*				  uu_ttput(uw_104.ttfd,penselect,2);			/* select pen color */
/*				  buflen = buflen + 2;*/
		 		  uu_ttflout(uw_104.ttfd);
				 }
			  if (a==j) 	a++;
			  if ((prept[0]!=jpbuf[j].pt1[0])||(prept[1]!=jpbuf[j].pt1[1]))
				 {
 			  	  uu_ttput(uw_104.ttfd,penup,1);			/* pen up */
				  buflen++;
				  uw_104mvpen(prept,jpbuf[j].pt1);
				 }
			  				/* change line type whenever necessary */
			  if (jpbuf[j].lntype != lntype)	 
				 {
				  lntype = jpbuf[0].lntype;
				  uwi_104lntype(lntype);
				  buflen = buflen + 2;
				  /*--
				  lntype = jpbuf[j].lntype;
				  bypass[2] = lntype + BIAS104;
 			  	  uu_ttput(uw_104.ttfd,bypass,3);		
				  buflen = buflen + 3;
				  ---*/
				 }
 			  uu_ttput(uw_104.ttfd,pendown,1);			/* pen down */
			  buflen++;
			  if (jpbuf[j].type == LINE104)
			  	  uw_104mvpen(jpbuf[j].pt1,jpbuf[j].pt2);
			  else
				  uw_104symbols(&jpbuf[j]);

			  prept[0] = jpbuf[j].pt2[0];		/* save the last point  */
			  prept[1] = jpbuf[j].pt2[1];
			 }
		 }		/* for */
	 }	/* for */

   uu_ttput(uw_104.ttfd,penup,1);			/* pen up and move */
	buflen++;
	uu_dexit;
}	/* uw_104dmpline  */



/*********************************************************************
**    I_FUNCTION :  uw_104mvpen(pt1,pt2)
**       move pen from point 1 to point 2
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_104mvpen(pt1,pt2)
int	pt1[], pt2[];

{
	int	ix, iy, dx, dy, maxs;
	int	ixhgh, irem1, ixmid, ixlow;
	int	iyhgh, iymid, iylow;
	int	times;				/* repeat times */
	int	xsign, ysign;		/* sign for x and y direction */
	int	radix2;
	int	xcomp, ycomp;		/* number of non zero byte for x and y */
	int	i, indx, indy;
	char	cmd[2], xnum[4], ynum[4]; 

	uu_denter(UU_GITRC,(us, "104mvpen,pt1=%d,%d; pt2=%d,%d",pt1[0],pt1[1],pt2[0],pt2[1]));
	dx = pt2[0] - pt1[0];
	dy = pt2[1] - pt1[1];
	if ((dx!=0)||(dy!=0))		/* not the same point */
	  {
		xsign = ysign = 1;
		if (dx < 0) 
		  {
			dx = -dx;
			xsign = -1;
		  }
		if (dy < 0)
		  {
			dy = -dy;
			ysign = -1;
		  }
		maxs = (dx > dy)? dx : dy;
		times = maxs / 32767 + 1;
		ix = dx / times;
		iy = dy / times;
		radix2 = RADIX104*RADIX104;
		ixhgh = ix / radix2;					  	/* x higher order byte */
		irem1 = ix - (radix2*ixhgh);
		ixmid = irem1 /RADIX104;				/* x middle order byte */
		ixlow = irem1 - (RADIX104*ixmid);	/* x lower order byte  */
		iyhgh = iy / radix2;						/* y higher order byte */
		irem1 = iy - (radix2*iyhgh);
		iymid = irem1 /RADIX104;				/* y middle order byte */
		iylow = irem1 - (RADIX104*iymid);	/* y lower order byte  */
		uw_104pack(&xcomp,xnum,ixhgh,ixmid,ixlow);
		uw_104pack(&ycomp,ynum,iyhgh,iymid,iylow);
		indx = xcomp * xsign + 3;
		indy = 3 - (ycomp * ysign);
		cmd[0] = deltacmd[indy][indx] + BIAS104;
		uu_dprint(UU_GITRC,(us,"cmd=%d", cmd[0])); 
		for (i=0; i<times; i++)
		  {
   		uu_ttput(uw_104.ttfd,cmd,1);			/* move command*/
   		uu_ttput(uw_104.ttfd,xnum,xcomp);	/* x componenet */
   		uu_ttput(uw_104.ttfd,ynum,ycomp);	/* x componenet */
			buflen = buflen + 1 + xcomp + ycomp;
			if (buflen > BUF104SIZE)
				uw_104buflout();
		  }
	  }
	uu_dexit;
}	/* uw_104mvpen */



/*********************************************************************
**    I_FUNCTION :  uw_packnum(ind,num,hgh,mid,low)
**       pack the high, middle, low order byte to a array, delete the 
**			leading zero.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_104pack(ind,num,hgh,mid,low)
int	*ind;
char	num[];
int	hgh, mid, low;

{
	uu_denter(UU_GITRC,(us, "104pack,hgh=%d,mid=%d,low=%d",hgh,mid,low));
	*ind = 3;
	if (hgh == 0)
	  {
		(*ind)--;
		if (mid == 0)
		  {
			(*ind)--;
			if (low == 0)
			  (*ind)--;
			else
			  num[0] = low + BIAS104;
		  }
		else
		  {
			num[0] = mid + BIAS104;
			num[1] = low + BIAS104;
		  }
	  }
	else
	  {
		num[0] = hgh + BIAS104;
		num[1] = mid + BIAS104;
		num[2] = low + BIAS104;
	  }
	uu_dprint(UU_GITRC,(us, "after 104pack,ind=%d,num=%c,%c,%c",*ind,num[0],num[1],num[2]));
	uu_dexit;
}	/* uw_104pack */



/*********************************************************************
**    I_FUNCTION :  uwi_104lntype(size,lntype)
**       Output the line type command for a certain type of dash line.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : The dash commands are hard coded here. To add new
**							type or to change the dash formate, reference the
**							test program.
*********************************************************************/

uwi_104lntype(lntype)
int	lntype;

{
	int	pt1[2], pt2[2];
	char	dash[3];

	uu_denter(UU_GITRC,(us, "104lntype,lntype=%d",lntype));
	pt1[0] = pt1[1] = 0;
	dash[0] = BIAS104 + 13;		/* dash line command */
	/* uu_ttput(fd2,"-\"51-&T", 7);	*/
	switch (lntype)
	{
	 case 1:		/* solid line */
		dash[1] = BIAS104 + 1;	/* solid line */
   	uu_ttput(uw_104.ttfd,dash,2);
		break;

	 case 2:    /* short dash -- -1/8", +1/32"  */
		dash[1] = BIAS104 + 2;	/* two segments	*/
   	uu_ttput(uw_104.ttfd,dash,2);
		pt2[0] = -0.125 * STEP104 + 0.5;
		pt2[1] = 0.03125 * STEP104 + 0.5;
	   uw_104mvpen(pt1,pt2);
		break;

	 case 3:    /* dotted -- -1/32", +1/32"    */
		dash[1] = BIAS104 + 2;	/* two segments	*/
   	uu_ttput(uw_104.ttfd,dash,2);
		pt2[0] = -0.03125 * STEP104 + 0.5;
		pt2[1] = 0.03125 * STEP104 + 0.5;
	   uw_104mvpen(pt1,pt2);
		break;

	 case 4:		/* long dash -- -1/4", +1/16"  */
		dash[1] = BIAS104 + 2;	/* two segments	*/
   	uu_ttput(uw_104.ttfd,dash,2);
		pt2[0] = -0.25 * STEP104 + 0.5;
		pt2[1] = 0.0625 * STEP104 + 0.5;
	   uw_104mvpen(pt1,pt2);
		break;

	 case 5:		/* dash-dot-dash -- -1/4",+1/16";-1/16",+1/16" */
		dash[1] = BIAS104 + 4;	/* two segments	*/
   	uu_ttput(uw_104.ttfd,dash,2);
		pt2[0] = -0.25 * STEP104 + 0.5;
		pt2[1] = 0.0625 * STEP104 + 0.5;
	   uw_104mvpen(pt1,pt2);
		pt2[0] = -0.0625 * STEP104 + 0.5;
		pt2[1] = 0.0625 * STEP104 + 0.5;
	   uw_104mvpen(pt1,pt2);
		break;

	 case 6:    /* centerline -- -3/4",+1/16"; -1/5",+1/16"    */
		dash[1] = BIAS104 + 4;	/* two segments	*/
   	uu_ttput(uw_104.ttfd,dash,2);
		pt2[0] = -0.75 * STEP104 + 0.5;
		pt2[1] = 0.0625 * STEP104 + 0.5;
	   uw_104mvpen(pt1,pt2);
		pt2[0] = -0.2 * STEP104 + 0.5;
		pt2[1] = 0.0625 * STEP104 + 0.5;
	   uw_104mvpen(pt1,pt2);
		break;

	 case 7:		/* phantom -- -3/4",+1/16"; -1/8",+1/16" ; -1/8",+1/16"  */
		dash[1] = BIAS104 + 6;	/* two segments	*/
   	uu_ttput(uw_104.ttfd,dash,2);
		pt2[0] = -0.75 * STEP104 + 0.5;
		pt2[1] = 0.0625 * STEP104 + 0.5;
	   uw_104mvpen(pt1,pt2);
		pt2[0] = -0.125 * STEP104 + 0.5;
		pt2[1] = 0.0625 * STEP104 + 0.5;
	   uw_104mvpen(pt1,pt2);
		pt2[0] = -0.125 * STEP104 + 0.5;
		pt2[1] = 0.0625 * STEP104 + 0.5;
	   uw_104mvpen(pt1,pt2);
		break;	

	 default:
		dash[1] = BIAS104 + 1;	/* solid line */
   	uu_ttput(uw_104.ttfd,dash,2);
		break;
	}
	uu_dexit;

}	/* uwi_104lntype */



/*********************************************************************
**    I_FUNCTION :  uw_104buflout()
**       Flush the output buffer to the plotter, set 'end-message' information
**			initialize the syn char and the buffer size
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_104buflout()

{
	uu_denter(UU_GITRC,(us, "104buflout,buflen=%d",buflen));
	uu_ttput(uw_104.ttfd, "\003", 1);			/* end-of-mesage character */
	uu_ttflout(uw_104.ttfd);
				/* timing delay	*/
	uu_ttput(uw_104.ttfd, "          ",10);
	uu_ttput(uw_104.ttfd, "          ",10);
	uu_ttflout(uw_104.ttfd);
	/*uu_ttput(uw_104.ttfd, "\0x02\0x20", 2);	/* sync character & bias char */
	uu_ttput(uw_104.ttfd, "\002 ", 2);	/* sync character & bias char */
	uu_ttflout(uw_104.ttfd);
	buflen = 2;
	uu_dexit;
}	/* uw_104buflout */



/*---------------------------------------------> UG_DCHHTNDC */
uw_104chhtndc(prms)				/* set character height NDC */
struct { Gint op; Gws id; float ndc_ht; } *prms;
{
	/*	(*prms).ndc_ht contains the char height between 0.0 and 1.0.
		Remember this and use it on all subsequent UG_DTEXT
		calls. If the device is able, use it on UG_DATEXT also. 
		Otherwise use "standard" height text on UG_DATEXT. */
	uu_denter(UU_GITRC,(us,"uw_104chhtndc()"));
	uu_dexit;
}

