
/*********************************************************************
**    NAME         :  wspsout.c
**    CONTAINS:
**       names of functions in file
**		uw_psflarearas
**		uw_psrastext
**		uw_pstext
**		uw_pspolyln
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wspsout.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:14
*********************************************************************/
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "ginqatt.h"
#include "wsps.h"


extern 	Gsps uw_ps;					/* declare workstation local data */
typedef struct {int id; Gws ws; int n; Gipoint *points;} flarearasprms;
/*
.....Already declared in wsps.h
.....Removed by Yurong
*/
/*extern double psrate_x, psrate_y; */
extern int wsplotdx,wsplotdy;

/*********************************************************************
**    I_FUNCTION : uw_pspolyln(prms,reply) -----> UG_DPOLYLN
**     
**		DESCRIPTION:
**			Draw the polyline specified in (*prms). 
**			The number of 2D world coordinates (untransformed) 
**			is contained in (*prms).points.
**
**    PARAMETERS   
**       INPUT  : 
**          prms: store info about the line
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_pspolyln(prms,reply)
struct {int id; Gws ws; int n; Gwpoint *points;} *prms;
int reply[];
{	
	ug_dpolyln3(prms);
}
/*********************************************************************
**    I_FUNCTION :  uw_pstext(prms,reply)				
**		display the text in (*prms).s at position (*prms).pos. Use the
**		current text attributes as found in ug_gksstli.curprats, defined in
**		gkstbl.h 
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_pstext(prms,reply)	
UG_dtext *prms;
int reply[];
{
	int xy[2],i,n;
	Gnpoint3 npos;
	void uw_psrastext();

	uu_denter(UU_GITRC,(us,"wsps gtext(%g %g %g %s)",
				(*prms).pos.x,(*prms).pos.y,(*prms).pos.z,(*prms).s));
	n = strlen((*prms).s);
	uu_denter2(UU_GITRC,(us,"PREC=%d",ug_gksstli.curprats.txbundl.fp.prec));
	uu_dexit;

	if (ug_gksstli.curprats.txbundl.fp.prec!=UG_STROKE) {
		if ((n<1)||(n>136)) 
		{
			uu_denter2(UU_GITRC,(us,"wsps gtext bad strlen=%d",n));
			uu_dexit;
		}
		if (n>0) 
		{							/* no-op for null string */
			for (i=0; i<n; i++) 
			{
				if (((*prms).s[i]<' ')||((*prms).s[i]>126)) {
					(*prms).s[i]=' '; 			/* change all ctrl chars to blanks*/
				}
			}
			uu_denter2(UU_GITRC,(us,"pstext curvwindex=%d",
					ug_gksstli.curvwindex));
			uu_dexit;
			ug_xform((*prms).pos.x,(*prms).pos.y,(*prms).pos.z,
						&npos,ug_cxform[ug_gksstli.curvwindex]);
			uu_denter2(UU_GITRC,(us,"wspstext after xform %g,%g,%g, curvwindex=%d",
				npos.x,npos.y,npos.z,ug_gksstli.curvwindex));
			uu_dexit;
			uw_psndctodev(&npos,xy,uw_ps.wid);
			uw_psrastext(xy,(*prms).s);
			if (uw_ps.fid>0)
				uu_ttflout(uw_ps.ttfd);
		}
	}
	else ug_hershy(prms,0);
	uu_dexit;
}	/* uw_pstext */




/*********************************************************************
**    I_FUNCTION :  uw_psrastext(xy,s)
**       Display graphics text at raster coordinates.
**    PARAMETERS   
**       INPUT  : 
**          xy[2]: position to draw text
**			s: string to be draw
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_psrastext(xy,p)
int xy[2]; char *p;

{
	int	len;
	int	tmpx, tmpy;
	char	num[20];
	double scale_x, scale_y;
	uu_denter(UU_GITRC,(us,"uw_psrastext(%d %d,%s)",xy[0],xy[1],p));

	uw_pspen (ug_gksstli.curprats.txbundl.color);
	if (uw_ps.isrotate)
	{
		tmpx = (int)(-xy[1] + uw_ps.vpy + wsplotdy*2);
		tmpy = xy[0];
		xy[0] = tmpx;		xy[1] = tmpy;
		uu_denter2(UU_GITRC,(us, "psrastext,xy=%d,%d",xy[0],xy1[1]));
		uu_dexit; 
	}
	xy[0] = xy[0] - DEVXHALF;
	xy[1] = xy[1] - DEVYHALF;
   	utp_ttputps(uw_ps.ttfd,"gsave\n ", 6);
	sprintf(num, "%d %d ", xy[0],xy[1]);
	utp_ttputps(uw_ps.ttfd,num,strlen(num));	
	utp_ttputps(uw_ps.ttfd,"moveto\n ",7);	
	scale_x = 1/psrate_x;
	scale_y = 1/psrate_y;
	sprintf(num, "%f %f scale\n", scale_x, scale_y);
	utp_ttputps(uw_ps.ttfd,num,strlen(num));
	if (uw_ps.isrotate)
		utp_ttputps(uw_ps.ttfd,"90 rotate\n ", 10);	
	len = strlen(p);
	utp_ttputps(uw_ps.ttfd,"(", 1);
	while (len-- > 0)
	{ 	
		utp_ttputps(uw_ps.ttfd,p++,1);
	}

	utp_ttputps(uw_ps.ttfd,") show\n ", 7);
	utp_ttputps(uw_ps.ttfd,"grestore\n ",9);
	if (uw_ps.fid>0)
		uu_ttflout(uw_ps.ttfd);
	uu_dexit;
}	/* uw_psrastext */

/*********************************************************************
**    I_FUNCTION     :  int uw_psflarearas(prms,reply)
**			UG_DFLAREARAS jump-table entry
**       Fill area (raster)
**    PARAMETERS   
**       INPUT  : 
**          prms: store info about the area
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_psflarearas(prms,reply)
flarearasprms *prms;
int reply[];
{
	int i,j;
	UG_line	fline;
	Gnpoint	startpt;

	uu_denter(UU_GITRC,(us,"uw_psflarearas(npts=%d,raspts)",(*prms).n));
	j = ((*prms).n < MAX_FILLAREA_PTS) ? (*prms).n : MAX_FILLAREA_PTS;
	uw_psdevtondc(&((*prms).points[0]),&(fline.p1));
	startpt = fline.p1;
	for ( i=1; i<j; i++ )
	{
		uw_psdevtondc(&((*prms).points[i]),&(fline.p2));
		uw_psdrwop(&fline);
		fline.p1 = fline.p2;
	}
	fline.p2 = startpt;
	uw_psdrwop(&fline);
	uu_dexit;
}	/* uw_psflarearas */
