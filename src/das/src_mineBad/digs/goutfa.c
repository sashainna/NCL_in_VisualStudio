/*********************************************************************
**    NAME         :  goutfa.c -- fill area functions.
**       CONTAINS:
**		Gerror gfillarea3(n,points)
**		ug_fla3(n,points) -- call workstation fill area 3d.
**		ug_dfla3(prms,reply) -- fillarea 3d simulation routine.
**		ug_d2fla3(prms,reply) -- fillarea 3d simulation routine.
**		ug_d2fla2(prms,reply) -- fillarea 2d simulation routine.
**		Gerror gfillarea(n,points)
**		ug_fla2(n,points) -- call workstation fill area 2d.
**		ug_flaras(n,points) -- draw raster fill area.
**    Gerror ug_closepolygon(n,points)
**    int ug_inside(pt,npoints,points)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       goutfa.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:22
*********************************************************************/
#define UG_TRUE 1
#define UG_FALSE 0
#include "zsysdep.h"
#include "umath.h"
#include <stdio.h>
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
/*#include "ginq.h"*/
/*#include "gvlib.h"*/
#include "gdidd.h"
/*#include "gdidd2.h"*/
#include "udebug.h"
#include "gsegac.h"
/*#include "gmat4.h"*/

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) goutfa.c 3.4 7/20/88 15:46:32 single"};
#else
static char uu_sccsident[]={"@(#) goutfa.c 3.4 7/20/88 15:46:32 double"};
#endif
Gerror ug_chkrect(),ug_chkrect3(),ug_chkpoints(),ug_chkpoints3();
extern int ug_viwseg;
extern Glntype ug_lntypes[7];


/********************************************************************* 
**  E_FUNCTION:  Gerror gfillarea3(n,points) -- 3D fill area.
**      3D Fill area is drawn. The outline is specified by points. The
**		  number of points is n. The appearance of the fill area is
**		  governed by the fill area attributes.
**  PARAMETERS   
**      INPUT:  Gint n -- number of points.
**					 Gwpoint3 points[] -- points specifying the outline of fill area.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gfillarea3(n,points)
/*$ INPUT */
Gint n;
Gwpoint3 points[];
{
	Gerror irtn,irtn2;
	int i;
	UG_segstli *sp;

	uu_denter(UU_GTRC,(us,
			"gfillarea3(%d,p[0..2]=%g %g %g, %g %g %g, %g %g %g)",
			n,points[0].x,points[0].y,points[0].z,points[1].x,points[1].y,
			points[1].z,points[2].x,points[2].y,points[2].z));

	irtn=NCL_NO_ERROR;

#ifdef UU_CHECK
	irtn=ug_chkad(points,"gfillarea3");
	irtn=ug_chkwsac("gfillarea3");
	if ((n<3)||(n>=(UG_lismaxsiz-1)/2)) {		/* bad number of points */
		ug_errorhand(ENPOINTS,"gfillarea3",&n); irtn=ENPOINTS;
	}
	if ((irtn2=ug_chkpoints3(n,points,"gfillarea3"))!=NCL_NO_ERROR)
		irtn=irtn2;
	if (irtn==NCL_NO_ERROR) {
#endif

		ug_fla3(n,points);
		if (ug_gksos.sysstate==UG_SGOP) {
			ug_nfa3(ug_segac(ug_gksstli.opnseg)->seglist,n,points);
			sp=ug_segac(ug_gksstli.opnseg);
			ug_updxforms(sp);					/* update xforms bits in seg header*/
		}

#ifdef UU_CHECK
	}
#endif

	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION   :  ug_fla3(n,points) -- call workstation fill area 3d.
**    PARAMETERS   
**       INPUT  : 
**				int n					number of points defining fill area
**				Gwpoint3 points[]	array of 3D ndc coords
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_fla3(n,points)
int n;
Gwpoint3 points[];
{
	struct {int id; Gws ws; int n; Gwpoint3 *points;} prms;
	int i;
	Gfloat dx,dy;
	Gnpoint3 *p;
	UG_segstli *sp;

	uu_denter(UU_GITRC,(us,"ug_fla3(n=%d)",n));
#ifdef UU_CHECK
	ug_chkad(points,"ug_fla3");
#endif

	/* call workstation */
	prms.id=UG_DFLAREA3; prms.n=n;
	prms.points=points;
	if (ug_find.find==UG_FALSE)
		ug_wkout(&prms,i=sizeof(prms)/sizeof(int));
	else { 			/* here see if fill area is found  */
		/* see if point ug_find.x,y is near a vertex or inside polygon */
		ug_closepolygon(n,points);
	}
	if ((ug_viwseg>=0)&&(ug_ndcseg>0)) {
		sp=ug_segac(ug_viwseg);
		if (ug_find.find==UG_FALSE) ug_boxexpfa3(sp,points,n);
	}
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION :  ug_dfla3(prms,reply) -- fillarea 3d simulation routine.
**						Clips fill area, convert to raster coordinates,
**						call workstation's UG_DFLAREARAS entry with clipped fillarea
**						if segmetn it is in is visible, or if in no segment.
**    PARAMETERS   
**       INPUT  : struct {int id; Gws ws; 
**							int n; Gwpoint3 *points;} *prms;
**       OUTPUT :  int reply[]; -- not used now.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_dfla3(prms,reply)
struct {int id; Gws ws; int n; Gwpoint3 *points;} *prms;
{
	uu_denter(UU_GITRC,(us,"ug_dfla3(%d,%d,%g %g %g)",prms->ws,prms->n,
		(*prms).points[0].x,(*prms).points[0].y,(*prms).points[0].z));
	if (ug_gksos.sysstate==UG_SGOP) {
		if (ug_segac(ug_gksstli.opnseg)->segatts.gvis==UG_VISIBLE)
			ug_flarea3(prms->ws,(*prms).n,(*prms).points);
	}
	else
		ug_flarea3(prms->ws,(*prms).n,(*prms).points);
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION :  ug_dfla2(prms,reply) -- fillarea 2d simulation routine.
**						Clips fill area, convert to raster coordinates,
**						call workstation's UG_DFLAREARAS entry with clipped fillarea
**						if segmetn it is in is visible, or if in no segment.
**    PARAMETERS   
**       INPUT  : struct {int id; Gws ws; 
**							int n; Gwpoint3 *points;} *prms;
**       OUTPUT :  int reply[]; -- not used now.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_dfla2(prms,reply)
struct {int id; Gws ws; int n; Gwpoint *points;} *prms;
{
	Gwpoint3 p3[20];
	int n,i;
	struct {int id; Gws ws; int n; Gwpoint3 *points;} prms3;
	uu_denter(UU_GITRC,(us,"ug_dfla2(%d,%d,%g %g)",prms->ws,prms->n,
		(*prms).points[0].x,(*prms).points[0].y));
	/* just copy points into a 3D array, supply 0. z, and call dfla3 */
	n=prms->n;
	if (n>20) {
		prms3.points=(Gwpoint3 *)uu_toolmalloc(n*sizeof(Gwpoint3));
	}
	else prms3.points=p3;
	for (i=0; i<n; i++) {
		prms3.points[i].x=(*prms).points[i].x;
		prms3.points[i].y=(*prms).points[i].y;
		prms3.points[i].z=0.;
	}
	prms3.n=prms->n;
	prms3.ws=prms->ws;
	prms3.id=UG_DFLAREA3;
	ug_dfla3(&prms3,reply);
	if (n>20) uu_toolfree(prms3.points);
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION :  ug_d2fla3(prms,reply) -- fillarea 3d simulation routine.
**						Clips fill area, convert to raster coordinates,
**						call workstation's UG_DFLAREARAS entry with clipped fillarea.
**						Just like ug_dfla3 except calls workstation even if segment 
**						it is in is invisible. For workstations that maintain their
**						own segment storage.
**    PARAMETERS   
**       INPUT  : struct {int id; Gws ws; 
**							int n; Gwpoint3 *points;} *prms;
**       OUTPUT :  int reply[]; -- not used now.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_d2fla3(prms,reply)
struct {int id; Gws ws; int n; Gwpoint3 *points;} *prms;
{
	uu_denter(UU_GITRC,(us,"ug_d2fla3(%d,%d,%g %g %g)",prms->ws,prms->n,
		(*prms).points[0].x,(*prms).points[0].y,(*prms).points[0].z));
		ug_flarea3(prms->ws,(*prms).n,(*prms).points);
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION :  ug_d2fla2(prms,reply) -- fillarea 2d simulation routine.
**						Clips fill area, convert to raster coordinates,
**						call workstation's UG_DFLAREARAS entry with clipped fillarea.
**						Just like ug_dfla2 except calls workstation even if segment 
**						it is in is invisible. For workstations that maintain their
**						own segment storage.
**    PARAMETERS   
**       INPUT  : struct {int id; Gws ws; 
**							int n; Gwpoint3 *points;} *prms;
**       OUTPUT :  int reply[]; -- not used now.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_d2fla2(prms,reply)
struct {int id; Gws ws; int n; Gwpoint *points;} *prms;
{
	int n,i;
	Gwpoint3 *points;
	Gwpoint3 p3[20];

	uu_denter(UU_GITRC,(us,"ug_d2fla2(%d,%d,%g %g)",prms->ws,prms->n,
		(*prms).points[0].x,(*prms).points[0].y));
	/* just copy the points into a 3D fillarea, supply z=0. and call ug_flarea3*/
	n=prms->n;
	if (n>20) {
		points=(Gwpoint3 *)uu_toolmalloc(n*sizeof(Gwpoint3));
	}
	else points=p3;
	for (i=0; i<n; i++) {
		points[i].x=(*prms).points[i].x;
		points[i].y=(*prms).points[i].y;
		points[i].z=0.;
	}
	n=prms->n;
	ug_flarea3(prms->ws,n,points);
	if (n>20) uu_toolfree(points);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_flarea3(ws,n,points) -- draw fill area. Clip,
**							convert to raster coords, call UG_DFLAREARAS
**							with clipped fill area.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_flarea3(ws,n,points)
Gws ws;
int n;						/* number points in fill area */
Gwpoint3 points[];
{
	int i;
	Gnpoint3 ndcpts[20];		/* NDC unclipped fillarea vertices */
	Gnpoint3 clippts[40];	/* clipped NDC fillarea vertices */
	Gipoint raspts[40];		/* clipped raster fillarea vertices */
	int nclip;					/* size of clippts */
	Gnpoint3 *p,*pclip;
	Gipoint *pras;
	Gnrect3 *cliprect;		/* rectangle to clip to */
	struct {int id; Gws ws; int n; Gipoint *points;} rasprms;

	uu_denter(UU_GITRC,(us,"ug_flarea3(%d,%d,%g,%g,%g)",ws,n,points[0].x,
		points[0].y,points[0].z));
	/* convert to NDC points */
	if (n>20) {
		p=(Gnpoint3 *)uu_toolmalloc(n * sizeof(Gnpoint3));
		pclip=(Gnpoint3 *)uu_toolmalloc(2*n*sizeof(Gnpoint3));
		pras=(Gipoint *)uu_toolmalloc(2*n*sizeof(Gipoint));
	}
	else {
		p=ndcpts;
		pclip=clippts;
		pras=raspts;
	}
	for (i=0; i<n; i++) {
		ug_xform(points[i].x,points[i].y,points[i].z,&p[i],
			ug_cxform[ug_gksstli.curvwindex]);	/* xform to NDC */
	}
	/* clip to current viewport or to ug_clip2rect */
	if (ug_clip2flag==0) 
		cliprect= &ug_gksstli.vtran[ug_gksstli.curvwindex].vport;
	else cliprect= &ug_clip2rect[ug_gksstli.curvwindex];
	ug_facliprect(n,p,NULL,&nclip,pclip,NULL,cliprect,1);
	/* change to raster coordinates */
	for (i=0; i<nclip; i++) 
		(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])(&pclip[i],&raspts[i],ws);
	/* send workstation a raster fill area if not finding */
	rasprms.id=UG_DFLAREARAS; rasprms.n=nclip;
	rasprms.points=raspts;
	if (ug_find.find==UG_FALSE) ug_wkout(&rasprms,i=sizeof(rasprms)/sizeof(int));
	if (n>20) { 
		uu_toolfree(p); uu_toolfree(pclip); uu_toolfree(pras);
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gfillarea(n,points) -- fill area 2D.
**      Fill area is drawn. The outline is specified by points. The
**		  number of points is n. The appearance of the fill area is
**		  governed by the fill area attributes.
**  PARAMETERS   
**      INPUT:  Gint n -- number of points.
**					 Gwpoint points[] -- points specifying the outline of fill area.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gfillarea(n,points)
/*$ INPUT */
Gint n;
Gwpoint points[];
{
	Gerror irtn,irtn2;
	int i;
	UG_segstli *sp;				/* segment header pointer */

	uu_denter(UU_GTRC,(us,
			"gfillarea(%d,points[0..2]=%g %g, %g %g, %g %g)",
			n,points[0].x,points[0].y,points[1].x,points[1].y,
			points[2].x,points[2].y));

#ifdef UU_CHECK
	irtn=ug_chkad(points,"gfillarea");
	irtn=ug_chkwsac("gfillarea");
	if ((n<1)||(n>=(UG_lismaxsiz-1)/2)) {		/* bad number of points */
		ug_errorhand(ENPOINTS,"gfillarea",&n); irtn=ENPOINTS;
	}
	if ((irtn2=ug_chkpoints(n,points,"gfillarea"))!=NCL_NO_ERROR) irtn=irtn2;
	if (irtn==NCL_NO_ERROR) {
#endif

		ug_fla2(n,points);			/* call workstation */
		if (ug_gksos.sysstate==UG_SGOP) {
			ug_nfa2(ug_segac(ug_gksstli.opnseg)->seglist,n,points);
			sp=ug_segac(ug_gksstli.opnseg);
			ug_updxforms(sp);					/* update xforms bits in seg header*/
		}

#ifdef UU_CHECK
	}
#endif

	uu_dexit;
	return(irtn);
}
/*********************************************************************
**    I_FUNCTION   :  Gerror ug_fla2(n,points) -- call workstation fill area 2d.
**    PARAMETERS   
**       INPUT  : 
**				int n					number of points defining fill area
**				Gwpoint points[]	array of 2D ndc coords
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror ug_fla2(n,points)
int n;
Gwpoint points[];
{
	Gerror irtn;
	struct {int id; Gws ws; int n; Gwpoint *points;} prms;
	int i;
	Gwpoint3 *points3;
	UG_segstli *sp;

	uu_denter(UU_GITRC,(us,"ug_fla2(%d, %g %g, %g %g, %g %g)",
		n, points[0].x, points[0].y, 
			points[1].x, points[1].y,
			points[2].x, points[2].y));
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	ug_chkad(points,"ug_fla2");
#endif

	/* call workstation */
	prms.id=UG_DFLAREA; prms.n=n;
	prms.points=points;
	if (ug_find.find==UG_FALSE) ug_wkout(&prms,i=sizeof(prms)/sizeof(int));
	else { 			/* here see if fill area is found  */
		/* see if point ug_find.x,y is near a vertex or inside polygon */
		points3=(Gwpoint3 *)uu_toolmalloc(n*sizeof(Gwpoint3));
		if (points3==NULL) {
			irtn=EMEMSPAC; goto rtn;
		}
		for (i=0; i<n; i++) {
			points3[i].x=points[i].x;
			points3[i].y=points[i].y;
			points3[i].z=0.;
		}
		ug_closepolygon(n,points3);
		uu_toolfree(points3);
	}
	if ((ug_viwseg>=0)&&(ug_ndcseg>0)) {
		sp=ug_segac(ug_viwseg);
		ug_boxexpfa2(sp,points,n);
	}
rtn:	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  ug_flaras(n,points) -- draw raster fill area.
**    PARAMETERS   
**       INPUT  : 
**				int n					number of points defining fill area
**				Gipoint points[]	array of 2D raster coords
**				Gws wsid				workstation id
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_flaras(n,points,wsid)
int n;
Gipoint points[];
Gws wsid;
{
	struct {int id; Gws ws; int n; Gipoint *points;} prms;
	int i;
	int fposn[2];					/* integer version of ug_find.x,y */
	int rasll[2],rasur[2];		/* corners of rectangular fill area */
	char us[180];

#ifdef UU_CHECK
	ug_chkad(points,"ug_flaras");
#endif

	uu_denter2(UU_GITRC,(us,"ug_flaras(n=%d, p[0,1,2]=%d %d, %d %d, %d %d ws=%d)",
		n,points[0].x,points[0].y,points[1].x,points[1].y,
		points[2].x,points[2].y,wsid));

	/* call workstation  if not finding */
	prms.id=UG_DFLAREARAS; prms.n=n;
	prms.points=points;
	if (ug_find.find==UG_FALSE) ug_wkout(&prms,i=sizeof(prms)/sizeof(int));
	else { 					/* finding, don't draw fill area */
		/* here see if ug_find.x,y is within fill area */
		fposn[0]=ug_find.x; 	fposn[1]=ug_find.y;
		if (n==4) {			/* 4 vertices. for now assume rectangular */
			rasll[0]=(points[0].x<points[2].x) ? points[0].x : points[2].x;
			rasll[1]=(points[0].y<points[2].y) ? points[0].y : points[2].y;
			rasur[0]=(points[0].x>points[2].x) ? points[0].x : points[2].x;
			rasur[1]=(points[0].y>points[2].y) ? points[0].y : points[2].y;
			if ((fposn[0]>=rasll[0])&&(fposn[1]>=rasll[1])
			&&(fposn[0]<=rasur[0])&&(fposn[1]<=rasur[1])) {
				/* found it. */
				ug_find.found=UG_TRUE;
				ug_find.dist=0.0;
			}
		}

		uu_denter2(UU_GITRC,(us,"ug_flaras find.x,y=%d %d, find.found=%d",
			fposn[0],fposn[1],ug_find.found));
		uu_dexit;

	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  Gerror ug_closepolygon(n,points)
**       See is gfind.x,y is within or close to polygon defined by points.
**    PARAMETERS   
**       INPUT  : 	int n -- number of vertices in polygon.
**							Gnpoint3[] -- array of n NDC vertices.
**       OUTPUT :  
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : sets gfind.found, gfind.dist, gfind.epsx,gfind.epsy.
**    WARNINGS     : none
*********************************************************************/
Gerror ug_closepolygon(n,points)
int n;										/* number of vertices */
Gnpoint3 points[];						/* array of NDC vertices */
{
	Gnpoint3 *p;
	int i;
	Gfloat dx,dy;
	Gerror irtn;
	uu_denter(UU_GITRC,(us,
		"ug_closepolygon(%d,p[0..2]=%g %g %g, %g %g %g, %g %g %g)", n,
	points[0].x,points[0].y,points[0].z,
	points[1].x,points[1].y,points[1].z,
	points[2].x,points[2].y,points[2].z));

	irtn=NCL_NO_ERROR;
	p=(Gnpoint3 *)uu_toolmalloc(n*sizeof(Gnpoint3));
	if (p!=NULL) irtn=EMEMSPAC;			/* out of memory */
	else {										/* not out of memory */				
		/* first see if near the 1st vertex, for small polygons */
		/* really should pick it here as a polyline */
		ug_xform(points[0].x,points[0].y,points[0].z,&p[0],
			ug_cxform[ug_gksstli.curvwindex]);	/* xform to NDC */
		dx=fabs(p[0].x-ug_find.x);
		dy=fabs(p[0].y-ug_find.y);
		if ((dx<ug_find.epsx)&&(dy<ug_find.epsy)) {	
			ug_find.found=UG_TRUE;
			ug_find.dist=(dx>dy)?dx:dy;	/* max(dx,dy) */
			ug_find.epsx=ug_find.dist;
			ug_find.epsy=ug_find.dist;
		}
		else {							/* wasn't near 1st vertex, see if inside */
			for (i=1; i<n; i++)
				ug_xform(points[i].x,points[i].y,points[i].z,&p[i],
			ug_cxform[ug_gksstli.curvwindex]);	/* xform to NDC */
			if (ug_inside(&ug_find.x,n,p)==1) {		/* point is inside */
				ug_find.found=UG_TRUE;
				ug_find.dist=0.;
				ug_find.epsx=0.;
				ug_find.epsy=0.;
			}
		}
		uu_toolfree(p);
	}							/* end not out of memory */	
	uu_dprint(UU_GITRC,(us,"%d=ug_closepolygon. returns found=%d, dist=%d",
		irtn,ug_find.found,ug_find.dist));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_inside(pt,npoints,points)
**    ug_inside determines if pt is inside the polygon formed by 
**	   points.
**
**    PARAMETERS   
**       INPUT  : 
**          Gnpoint3 *pt			Point to be tested.
**				int npoints			Number of verticies of polygon
**				Gnpoint3 points		Vertices of the polygon
**       OUTPUT :  
**    RETURNS      : 1 if point is inside polygon or on boundary
**							0 if point is outside of polygon
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define INSIDE 	1
#define OUTSIDE 	0
#define PI			(UU_REAL) 3.141592654
#define EPS			(UU_REAL) 7.0e-4

/* Local macros for legibility */
#define SAMEPT(A,B)			( (A.x==B.x && A.y==B.y) ? 1 : 0 )
#define MKVEC(A,B,C) 		C.x = A.x - B.x; C.y = A.y - B.y
#define CROSS2D(A,B) 		( A.x*B.y - A.y*B.x )
#define DOT2D(A,B)			( A.x*B.x + A.y*B.y )
#define SIGN(A)				( A>0.0 ? 1.0 : -1.0 )
#define ALMOSTEQUAL(A,B)	( fabs((A)-(B)) < EPS ? 1 : 0 )
#define MAGNITUDE(A)			( sqrt(A.x*A.x + A.y*A.y) )

int ug_inside(pt, npoints, points)
Gnpoint3 *pt;
Gint npoints;
Gnpoint3 points[];
{
	int i, i2;
	int irtn;
	Gnpoint v1, v2;
	Gnpoint lpt;
	Gfloat k;
	Gfloat sign;
	Gfloat da;
	Gfloat cosda;
	Gfloat angle;

	uu_denter(UU_GITRC,(us,"ug_inside(%g %g, n=%d,points)",
		(*pt).x,(*pt).y,npoints));
	angle = 0.0;
	lpt.x = pt->x;
	lpt.y = pt->y;
	
	for(i=0; i < npoints; ++i) {

		/* i2 is index of vertex following vertex i */
		i2 = ( (i == npoints-1) ? 0 : i+1 );

		/* If pt = a vertex return INSIDE */
		if( SAMEPT(lpt, points[i] )) {
			uu_dexit;
			return(INSIDE);
		}

		if( SAMEPT(lpt, points[i2])) {
			uu_dexit;
			return(INSIDE);
		}
		
		/* Find the next increment of angle */
		MKVEC(lpt, points[i], v1);
		MKVEC(lpt, points[i2], v2);

		sign = SIGN( CROSS2D(v1, v2) );
		
		cosda =  DOT2D(v1,v2) / (MAGNITUDE(v1) * MAGNITUDE(v2));
		if (cosda < -1.0)
			cosda = -1.0;
		else if (cosda > 1.0)
			cosda = 1.0;

		da = acos(cosda);

		/* If da == 180 degrees, point is on boundary */
		if( ALMOSTEQUAL(da, PI) ) {
			uu_dexit;
			return(INSIDE);
		}

		angle += da * sign;

	}

	/* If angle is 0, pt is outside of polygon */
	if( ALMOSTEQUAL(angle, (UU_REAL) 0.0) )
		irtn=OUTSIDE;
	else
		irtn=INSIDE;

	uu_dexit;
	return(irtn);
}

