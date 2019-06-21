/*********************************************************************
**    NAME         :  gout.c -- DIGS user callable output fctns.
**       CONTAINS:
**		Gerror gpolyline3(n,points) -- polyline 3D.
**		ug_polyln3(n,points)  
**		ug_dpolyln3(prms) -- polyline3 simulation routine.
**		Gerror gpolyline(n,points)
**		int ug_polyln(n,points)				
**		ug_dpolyln(prms) -- 2D polyline simulation routine.
**		int ug_polylnras(n,points) -- draw raster polyline.
**		ug_dpolylnras(prms) -- raster polyline sim.
**		Gerror gpolymarker3(n,points) -- polymarker 3D.
**		ug_polymk3(n,points)				
**		ug_dpolymk3(prms) -- 3D polymarker simulation routine.
**		Gerror gpolymarker(n,points)
**		ug_polymk(n,points)					
**		ug_dpolymk(prms) -- 2D polymarker simulation routine.
**		int ug_polymkras(n,points)				
**		ug_dpolymkras(prms) -- raster polymarker sim.
**		Gerror gtext(position,string)
**		ug_textras(posn,s,wsid) -- Draw text at raster posn.
**  	Gchar *ggdp3(n,points,function,data)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gout.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:22
*********************************************************************/

/* Each graphics command is a union. List processing package used
   to store them. A segment is a list of graphics commands. The dispaly
   file is a collection of segments.
*/
/* defining trace causes prints to ug_gksos.erfile on subroutine entry */
#define UG_TRUE 1
#define UG_FALSE 0
#define logical int
#include "zsysdep.h"
#include "umath.h"
#include <stdio.h>
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "ginq.h"
#include "gvlib.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "udebug.h"
#include "gsegac.h"
#include "gmat4.h"

Gerror ug_chkrect(),ug_chkrect3(),ug_chkpoints(),ug_chkpoints3();
extern int ug_viwseg;
extern Glntype ug_lntypes[7];

typedef struct {int id; Gws ws; int n; Gwpoint3 *points;} Gsp3;
typedef struct {int id; Gws ws; int n; Gwpoint *points;} Gspt;
typedef struct {int id; Gws ws; int n; Gipoint  *points;} Gipt;

void ug_polyln3(),ug_dpolyln3(),ug_dpolyln(),ug_dpolylnras(),ug_polymk3();
void ug_dpolymk3(),ug_polymk(),ug_dpolymk(),ug_text(),ug_polyln();

/********************************************************************* 
**  E_FUNCTION:  Gerror gpolyline3(n,points) -- polyline 3D.
**      Draw a line connecting the specified points.
**		  N is the number of points.
**  PARAMETERS   
**      INPUT:  Gint n -- number of points.
**					 Gwpoint3 points[] -- array of 3D world coordinate points.
**							Gwpoint3 is defined as follows:
**							typedef struct {
**									Gwc x;
**									Gwc y;
**									Gwc z;
**							} Gwpoint3;
**      OUTPUT: none
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**						  ENOTWSOP if the operating state is not WASC or SGOP.
**						  ENPOINTS if the number of points was invalid
**							 (less than 2 or greater than 99)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gpolyline3(n,points)			/* polyline 3D */
/*$ INPUT */
Gint n;
Gwpoint3 points[];
{
	Gerror irtn;
	UG_segstli *sp;
/*	char us[80];*/

	irtn=NCL_NO_ERROR;
	uu_denter2(UU_GTRC,(us,"gpolyline3(%d,points)",n));

#ifdef UU_CHECK
	Gerror irtn2;
	int siz;
	irtn=ug_chkad(points,"gpolyline3");
	irtn=ug_chkwsac("gpolyline3");
	if ((n<2)||(n>=(UG_lismaxsiz-1)/3)) {		/* bad no. points */
		ug_errorhand(ENPOINTS,"gpolyline3",&n); irtn=ENPOINTS;
	}
	if ((irtn2=ug_chkpoints3(n,points,"gpolyline3"))!=NCL_NO_ERROR)
		irtn=irtn2;
	if (irtn==NCL_NO_ERROR) {
#endif
		ug_polyln3(n,points);	
		if (ug_gksos.sysstate==UG_SGOP) {
			ug_npla3(ug_segac(ug_gksstli.opnseg)->seglist,n,points);
			sp=ug_segac(ug_gksstli.opnseg);
			ug_updxforms(sp);				/* update xforms bits in seg header*/
		}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  ug_polyln3(n,points)  -- draw polyline 3D.
**			Call workstation to draw polyline 3d.
**    PARAMETERS   
**       INPUT  : 
**          input
**				int n		number of coordinates in polyline
**				Gwpoint3 points[]		array of coordinates
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_polyln3(n,points)				/* call workstation to draw polyline 3d */
int n; 								/* number of coordinates in polyline */
Gwpoint3 points[];				/* array of coordinates */
{
	int siz;
	Gsp3 prms;
/*	char us[80];*/

#ifdef UU_CHECK
	ug_chkad(points,"ug_polyln3");
#endif

	uu_denter2(UU_GITRC,(us,"ug_polyln3(n=%d, points[0]=%g %g %g)",n,
				points[0].x,points[0].y,points[0].z));
	/* call workstation */
	prms.id=UG_DPOLYLN3; prms.n=n;
	prms.points=points;
	siz=sizeof(prms)/sizeof(int);
	if (ug_find.find==UG_FALSE)
		ug_wkout(&prms,siz);
	else
		ug_dpolyln3(&prms);
	uu_dexit;
}

/********************************************************************* 
**  S_FUNCTION:  ug_dpolyln3(prms) -- polyline3 simulation routine.
**      Workstation simulation routine to break up polylines into single lines
**			and transform world coordinates to NDC coordinates. Useful for
**			workstations that can't handle world coordinates. ug_dpolyln3 calls
**			the workstation's UG_DDRWOP entry n-1 times, once per line.
**  PARAMETERS   
**      INPUT:  struct {int id; Gws ws; 
**								int n; 				 number of points in polyline 
**								Gwpoint3 *points;  3D world coordinates of polyline
**					 } *prms;
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_dpolyln3(prms)
Gsp3 *prms;
{

#ifdef UU_CHECK
	ug_chkad(prms,"ug_dpolyln3");
#endif

	if (ug_gksos.sysstate==UG_SGOP) {
		if (ug_segac(ug_gksstli.opnseg)->segatts.gvis==UG_VISIBLE)
			ug_plna3((*prms).n,(*prms).points);
	}
	else
		ug_plna3((*prms).n,(*prms).points);
}

/********************************************************************* 
**  S_FUNCTION:  ug_d2polyln3(prms) -- polyline3 simulation routine.
**      Workstation simulation routine to break up polylines into single lines
**			and transform world coordinates to NDC coordinates. Useful for
**			workstations that can't handle world coordinates. ug_dpolyln3 calls
**			the workstation's UG_DDRWOP entry n-1 times, once per line.
**			Just like ud_dpolyln3 but always callc ws, even if invisible.
**			For workstations that maintain own segments.
**  PARAMETERS   
**      INPUT:  struct {int id; Gws ws; 
**								int n; 				 number of points in polyline 
**								Gwpoint3 *points;  3D world coordinates of polyline
**					 } *prms;
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_d2polyln3(prms)
Gsp3 *prms;
{

#ifdef UU_CHECK
	ug_chkad(prms,"ug_d2polyln3");
#endif

	ug_plna3((*prms).n,(*prms).points);
}
		
/*********************************************************************
**    S_FUNCTION :  Gerror ug_d3polyln3(prms) -- polyline3 sim routine,
**							calls polylineras.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror ug_d3polyln3(prms)
Gsp3 *prms;
{
	Gerror irtn;
	Gipoint *rasp;						/* raster polyline pointer */
	int n,i;
	int rasn;
	int doit;
	Gwpoint3 *points;
	Gnpoint3 np1,np2;					/* unclipped ndc points */
	Gnpoint3 pos1,pos2;				/* clipped ndc points */
	int onscr,chg2;
	Gipt prmsras;
	int reply[4];

	uu_denter(UU_GITRC,(us,"ug_d3polyln3(n=%d)",(*prms).n));
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	ug_chkad(prms,"ug_d3polyln3");
#endif

	if ((ug_gksstli.curprats.lnbundl.type.typeno==21)||
			(ug_find.find==1)) ug_dpolyln3(prms);
	else {							/* not user defined line type and not finding*/
		doit=0;
		if (ug_gksos.sysstate!=UG_SGOP) doit=1;
		else if (ug_segac(ug_gksstli.opnseg)->segatts.gvis==UG_VISIBLE) doit=1;
		if (doit==1) {
			n=(*prms).n;
			points=(*prms).points;
			/* allocate space for a raster polyline */
			if ((rasp=(Gipoint *)uu_toolmalloc(n*sizeof(Gipoint)))==NULL) {
				irtn=EMEMSPAC; goto rtn;
			}	
			ug_xform(points[0].x,points[0].y,points[0].z,&np1,
					ug_cxform[ug_gksstli.curvwindex]);
			rasn=0;
			for (i=1; i<n; i++) {
				ug_xform(points[i].x,points[i].y,points[i].z,&np2,
						ug_cxform[ug_gksstli.curvwindex]);
				ug_clipexp(&np1,&np2,&pos1,&pos2,&onscr,&chg2);
				if (onscr!=0) {			/* at least partially on scrn */
					/* add another raster point (or two) */
					if (rasn==0) {				/* pick up 1st point also */
						(*(ug_gksstli.wsopen[(*prms).ws].connid)[UG_DNDCDEV])
							(&pos1,&rasp[0],(*prms).ws);	/*convert to raster coords*/
						rasn=1;
					}
					(*(ug_gksstli.wsopen[(*prms).ws].connid)[UG_DNDCDEV])
						(&pos2,&rasp[rasn],(*prms).ws);	/*convert to raster coords*/
					rasn++;						/* bump number raster points so far */
				}
				if (chg2==1) {			/* pos2!=np2, flush the raster polyline buffer*/
					if (rasn>0) {
						prmsras.id=UG_DPOLYLNRAS;
						prmsras.ws=(*prms).ws;
						prmsras.n=rasn;
						prmsras.points=rasp;
						(*(ug_gksstli.wsopen[(*prms).ws].connid)[UG_DPOLYLNRAS])
							(&prmsras,reply);
					}
					rasn=0;
				}							/* end of flush the raster polyline buffer */
				np1=np2;					/* roll the NDC points */
			}								/* for i=1 to n */
			if (rasn>0) {				/* dump the polyline raster buffer */
				prmsras.id=UG_DPOLYLNRAS; prmsras.ws=(*prms).ws;
				prmsras.n=rasn;
				prmsras.points=rasp;
				(*(ug_gksstli.wsopen[(*prms).ws].connid)[UG_DPOLYLNRAS])
					(&prmsras,reply);
			}
			uu_toolfree(rasp);				/* free the raster point array storage */
		} 								/* end doit==1 */
	} 									/* end not finding and not user linestyle */
rtn:	uu_dexit;
	return(irtn);
}
	
/********************************************************************* 
**  E_FUNCTION:  Gerror gpolyline(n,points) -- 2D polyline.
**      2D polyline. Draw a line connecting  the n points.
**  PARAMETERS   
**      INPUT:  Gint n -- number of points.
**					 Gwpoint points[] -- array of points.
**      OUTPUT: none
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gpolyline(n,points)				/* polyline abs 2 */
/*$ INPUT */
Gint n;
Gwpoint points[];
{ 
	Gerror irtn;
	UG_segstli *sp;
/*	char us[150];*/

	irtn=NCL_NO_ERROR;

#ifdef UU_CHECK
	Gerror irtn2;
	ug_chkad(points,"gpolyline");
	irtn=ug_chkwsac("gpolyline");
	if ((n<2)||(n>=(UG_lismaxsiz-1)/2)) {		/* bad no. points */
		ug_errorhand(ENPOINTS,"gpolyline",&n); irtn=ENPOINTS;
	}

	if ((irtn2=ug_chkpoints(n,points,"gpolyline"))!=NCL_NO_ERROR)
		irtn=irtn2;
	uu_denter2(UU_GTRC,(us,"gpolyline(%d,%g %g, %g %g,..)",n,
		points[0].x,points[0].y,points[1].x,points[1].y));
	if (irtn==NCL_NO_ERROR) {		/* no errors, call workstation */
#endif
		ug_polyln(n,points);
		if (ug_gksos.sysstate==UG_SGOP) {
			ug_npla2(ug_segac(ug_gksstli.opnseg)->seglist,n,points);
			sp=ug_segac(ug_gksstli.opnseg);
			ug_updxforms(sp);					/* update xforms bits in seg header*/
		}

#ifdef UU_CHECK
	}
	uu_dexit;
#endif

	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_polyln(n,points) -- draw 2D polyline.
**		call workstation to draw 2d polyline.
**    PARAMETERS   
**       INPUT  : 
**          input
**				int n		number of points in polyline
**				Gwpoint points[]	array of 2D points
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_polyln(n,points)				/* call workstation to draw 2d polyline */
int n;
Gwpoint points[];
{
	int siz;
	Gspt prms;
	UG_segstli *sp;
/*	char us[140];*/

#ifdef UU_CHECK
	ug_chkad(points,"ug_polyln");
#endif

	uu_denter2(UU_GITRC,(us,"ug_polyln(n=%d, points[0]=%g %g)",
			n,points[0].x,points[0].y));

	prms.id=UG_DPOLYLN; prms.n=n;
	prms.points=points;
	siz=sizeof(prms)/sizeof(int);
	if (ug_find.find==UG_FALSE)
		ug_wkout(&prms,siz);
	else
		ug_dpolyln(&prms);
	if ((ug_viwseg>=0)&&(ug_ndcseg>0)) {
		sp=ug_segac(ug_viwseg);
		ug_boxexpln2(sp,points,n);
	}
	uu_dexit;
}

/********************************************************************* 
**  S_FUNCTION:  ug_dpolyln(prms) -- 2D polyline simulation routine.
**      Workstation simulation routine to break up polylines into single lines
**			and transform world coordinates to NDC coordinates. Useful for
**			workstations that can't handle world coordinates. ug_dpolyln calls
**			the workstation's UG_DDRWOP entry n-1 times, once per line.
**  PARAMETERS   
**      INPUT:  struct {int id; Gws ws; 
**								int n; 				 number of points in polyline
**								Gwpoint *points;  2D world coordinates of polyline
**					 } *prms;
**      OUTPUT: none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_dpolyln(prms)
Gspt *prms;
{

#ifdef UU_CHECK
	ug_chkad(prms,"ug_dpolyln");
#endif

	if (ug_gksos.sysstate==UG_SGOP) {
		if (ug_segac(ug_gksstli.opnseg)->segatts.gvis==UG_VISIBLE)
				ug_plna2((*prms).n,(*prms).points);	/* draw polyline*/
	}
	else
		ug_plna2((*prms).n,(*prms).points);			/* draw the polyline */
}
/********************************************************************* 
**  S_FUNCTION:  ug_d2polyln(prms) -- 2D polyline simulation routine.
**      Workstation simulation routine to break up polylines into single lines
**			and transform world coordinates to NDC coordinates. Useful for
**			workstations that can't handle world coordinates. ug_dpolyln calls
**			the workstation's UG_DDRWOP entry n-1 times, once per line.
**			Just like ug_dpolyln except always calls ws, even if invisible.
**			Used by workstations that maintain their own segments.
**  PARAMETERS   
**      INPUT:  struct {int id; Gws ws; 
**								int n; 				 number of points in polyline
**								Gwpoint *points;  2D world coordinates of polyline
**					 } *prms;
**      OUTPUT: none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_d2polyln(prms)
Gspt *prms;
{

#ifdef UU_CHECK
	ug_chkad(prms,"ug_dpolyln");
#endif
	ug_plna2((*prms).n,(*prms).points);			/* draw the polyline */
}

/*********************************************************************
**    I_FUNCTION     :  int ug_polylnras(n,points) -- draw raster polyline.
**    PARAMETERS   
**       INPUT  : 
**          input
**				int n			number of points in polyline
**				Gipoint *points	pointer to array of raster coord. pairs
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_polylnras(n,points)			/* draw raster polyline */
int n;
Gipoint *points;
{
	int siz;
	Gipt prms;
/*	char us[120];*/

#ifdef UU_CHECK
	ug_chkad(points,"ug_polylnras");
#endif

	uu_denter2(UU_GITRC,(us,"ug_polylnras(n=%d, points[0]=%d %d)",
			n,points[0].x,points[0].y));

	prms.id=UG_DPOLYLNRAS;
	prms.n=n;
	prms.points=points;
	siz=sizeof(prms)/sizeof(int);
	if (ug_find.find==UG_FALSE)
		ug_wkout(&prms,siz);
	else
		ug_dpolylnras(&prms);
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  ug_dpolylnras(prms) -- raster polyline sim.
**       Workstation simulation routine for raster polyline.
**    PARAMETERS   
**       INPUT  : 
**          input
**				struct {
**					int id;
**					Gws ws;
**					int n;
**					Gipoint  *points;
**				} *prms			structure contains the function id,
**									workstation id, number of points,
**									and a pointer to an array of raster coords
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dpolylnras(prms)
Gipt *prms;
{
	int i;
	Gws ws;
	Gdpoint3 pos1,pos2;					/* really are float raster points */
	Gfloat dist;
/*	char us[120];*/

#ifdef UU_CHECK
	ug_chkad(prms,"ug_dpolylnras");
#endif

	uu_denter2(UU_GITRC,(us,"ug_dpolylnras(ws=%d,n=%d, points[0]=%d %d",
			(*prms).ws,(*prms).n,(*prms).points[0].x,(*prms).points[0].y));

	ws=(*prms).ws;
	for (i=0; i<(*prms).n-1; i++)	{
		if (ug_find.find==UG_FALSE)
		/* call workstation's UG_DRASLINE entry to draw one line */
		(*(ug_gksstli.wsopen[ws].connid)[UG_DRASLINE])
				((*prms).points[i].x,(*prms).points[i].y,
				 (*prms).points[i+1].x,(*prms).points[i+1].y);
		else {											/* finding, don't draw */
			/* see if the line is within eps of ug_find.x,y */
			pos1.x=(*prms).points[i].x; pos1.y=(*prms).points[i].y; 
			pos1.z=0.0;
			pos2.x=(*prms).points[i+1].x; pos2.y=(*prms).points[i+1].y; 
			pos2.z=0.0;
			if (ug_closln(&ug_find.x,&pos2,&pos1,ug_find.epsx,
				 ug_find.epsy,&dist,ug_find.pikmode)==1) {	/* found it */
				 ug_find.found=UG_TRUE; ug_find.dist=dist;
				 ug_find.epsx=dist; ug_find.epsy=dist;
			}
		}
	}
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  ug_d2polylnras(prms) -- raster polyline sim.
**       Workstation simulation routine for raster polyline. Just like
**			ug_dpolylnras except simulates dashed lines and calls
**			UG_DRWOP for each dash.
**    PARAMETERS   
**       INPUT  : 
**          input
**				struct {
**					int id;
**					Gws ws;
**					int n;
**					Gipoint  *points;
**				} *prms			structure contains the function id,
**									workstation id, number of points,
**									and a pointer to an array of raster coords
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_d2polylnras(prms)
Gipt *prms;
{
	int i;
	Gws ws;
	Gfloat ctr;
	Gnpoint3 pos1,pos2;	
	int typeno;
/*	char us[120];*/

#ifdef UU_CHECK
	ug_chkad(prms,"ug_dpolylnras");
#endif

	uu_denter2(UU_GITRC,(us,"ug_d2polylnras(ws=%d,n=%d, points[0]=%d %d",
			(*prms).ws,(*prms).n,(*prms).points[0].x,(*prms).points[0].y));

	typeno=ug_gksstli.curprats.lnbundl.type.typeno;
	if ((ug_find.find==UG_TRUE)||(typeno<2)||(typeno>7)) 
		ug_dpolylnras(prms);
	else {						/* not finding, and 2<=linetype<=8 */
		ug_set_pattern(&ug_lntypes[typeno-2]);
		ug_reset_pattern();
		ws=(*prms).ws;
		(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])
			(&prms->points[0],&pos1,ws);		/*convert to NDC coords*/
		ctr=(ug_gksstli.vtran[ug_gksstli.curvwindex].vport.urb.z
			 +	ug_gksstli.vtran[ug_gksstli.curvwindex].vport.llf.z)/2.;
		for (i=1; i<prms->n; i++) { 
			(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])
				(&prms->points[i],&pos2,ws);		/*convert to NDC coords*/
			pos2.z=ctr;
			ug_lin4ndc(&pos1,&pos2);				/* draw ndc line */
			pos1=pos2;
		}													/* end for (i=1; i<n) */
	}							/* end not finding and 2<=linetype<=8 */
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gpolymarker3(n,points) -- polymarker 3D.
**  PARAMETERS   
**      INPUT:  Gint n -- number of markers.
**				    Gwpoint3 points[] -- coordinates of markers.
**      OUTPUT: none
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gpolymarker3(n,points)			/* polymarker 3d */
/*$ INPUT */
Gint n;										/* number of markers */
Gwpoint3 points[];
{
	Gerror irtn;
	UG_segstli *sp;
	uu_denter(UU_GTRC,(us,"gpolymarker3(%d,points)",n));

	irtn=NCL_NO_ERROR;

#ifdef UU_CHECK
	Gerror irtn2;
	irtn=ug_chkad(points,"gpolymarker3");
	irtn=ug_chkwsac("gpolymarker3");
	if ((n<1)||(n>=(UG_lismaxsiz-1)/3)) {		/*bad number of points */
		ug_errorhand(ENPOINTS,"gpolymarker3",&n); irtn=ENPOINTS;
	}
	if ((irtn2=ug_chkpoints3(n,points,"gpolymarker3"))!=NCL_NO_ERROR) irtn=irtn2;
	if (irtn==NCL_NO_ERROR) {			/* no errors, call workstation */
#endif
		ug_polymk3(n,points);
		if (ug_gksos.sysstate==UG_SGOP) {
			ug_npma3(ug_segac(ug_gksstli.opnseg)->seglist,n,points);
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
**    I_FUNCTION     :  ug_polymk3(n,points) -- draw 3D polymarker.
**		call workstation to draw polymarker 3d.
**    PARAMETERS   
**       INPUT  : 
**          input
**				int n				number of points
**				Gwpoint3 points[]	array of NDC points
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_polymk3(n,points)				/* call workstation to draw polymarker 3d */
int n;
Gwpoint3 points[];
{
	int i;
	Gsp3 prms;
/*	char us[150];*/

#ifdef UU_CHECK
	ug_chkad(points,"ug_polymk3");
#endif

	uu_denter2(UU_GITRC,(us,"ug_polymk3(n=%d, points[0]=%g %g %g)",n,
				points[0].x,points[0].y,points[0].z));

	prms.id=UG_DPOLYMK3; 
	prms.n=n; prms.points=points;
	if (ug_find.find==UG_FALSE)
		ug_wkout(&prms,i=sizeof(prms)/sizeof(int));
	else
		ug_dpolymk3(&prms);
	uu_dexit;
}

/********************************************************************* 
**  S_FUNCTION:  ug_dpolymk3(prms) -- 3D polymarker simulation routine.
**      Workstation simulation routine to break up polymarkers into single 
**			markers and transform world coordinates to NDC coordinates. Useful for
**			workstations that can't handle world coordinates. ug_dpolymk3 calls
**			the workstation's GPNTOP entry n times, once per marker.
**  PARAMETERS   
**      INPUT:  struct {int id; Gws ws; 
**						int n; 				 number of points in polyline 
**						Gwpoint3 *points;  3D world coordinates of polymarker
**					 } *prms;
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_dpolymk3(prms)
Gsp3 *prms;
{

#ifdef UU_CHECK
	ug_chkad(prms,"ug_dpolymk3");
#endif

	if (ug_gksos.sysstate==UG_SGOP) {
		/* added to fix the redisplay of points after a *reset/disply,pt is
	   	   issued on vms systems. kathy */
#if defined  UU_UNIX || defined GPX
		if (ug_segac(ug_gksstli.opnseg)->segatts.gvis==UG_VISIBLE)
#endif
			ug_pmka3((*prms).n,(*prms).points);
	}
	else
		ug_pmka3((*prms).n,(*prms).points);
}

/********************************************************************* 
**  S_FUNCTION:  ug_d2polymk3(prms) -- 3D polymarker simulation routine.
**      Workstation simulation routine to break up polymarkers into single 
**			markers and transform world coordinates to NDC coordinates. Useful for
**			workstations that can't handle world coordinates. ug_dpolymk3 calls
**			the workstation's GPNTOP entry n times, once per marker.
**			Just like ug_dpolymk3 except always calls ws, even if invisible.
**			For workstations that maintain their own segments.
**  PARAMETERS   
**      INPUT:  struct {int id; Gws ws; 
**						int n; 				 number of points in polyline 
**						Gwpoint3 *points;  3D world coordinates of polymarker
**					 } *prms;
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_d2polymk3(prms)
Gsp3 *prms;
{

#ifdef UU_CHECK
	ug_chkad(prms,"ug_d2polymk3");
#endif
	ug_pmka3((*prms).n,(*prms).points);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gpolymarker(n,points) -- 2D polymarker.
**  PARAMETERS   
**      INPUT:  Gint n -- number of markers.
**					 Gwpoint points[] -- array of n 2D  coordinates where
**							markers will be placed.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gpolymarker(n,points)			/* polymarker 2d */
/*$ INPUT */
Gint n;
Gwpoint points[];
{
	Gerror irtn;
	UG_segstli *sp;
	uu_denter(UU_GTRC,(us,"gpolymarker(%d,points)",n));

	irtn=NCL_NO_ERROR;

#ifdef UU_CHECK
	Gerror irtn2;
	irtn=ug_chkad(points,"gpolymarker");
	irtn=ug_chkwsac("gpolymarker");
	if ((n<1)||(n>=(UG_lismaxsiz-1)/2)) {		/* bad number of points */
		ug_errorhand(ENPOINTS,"gpolymarker3",&n); irtn=ENPOINTS;
	}
	if ((irtn2=ug_chkpoints(n,points,"gpolymarker"))!=NCL_NO_ERROR)
		irtn=irtn2;
	if (irtn==NCL_NO_ERROR) {					/* no errors, call workstation */
#endif

		ug_polymk(n,points);
		if (ug_gksos.sysstate==UG_SGOP) {
			ug_npma2(ug_segac(ug_gksstli.opnseg)->seglist,n,points);
			sp=ug_segac(ug_gksstli.opnseg);
			ug_updxforms(sp);						/* update xforms bits in seg header*/
		}
		if ((ug_viwseg>=0)&&(ug_ndcseg>0)){
			sp=ug_segac(ug_viwseg);
			ug_boxexpmk2(sp,points,n);
		}

#ifdef UU_CHECK
	}
#endif

	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  ug_polymk(n,points) -- draw 2D polymarker.
**		call workstation to draw 2d polymarker.
**    PARAMETERS   
**       INPUT  : 
**          input
**				int n				number of points to place markers at
**				Gwpoint points[]	array of 2D NDC points
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_polymk(n,points)				/* call workstation to draw 2d polymarker*/
int n;
Gwpoint points[];
{
	int i;
	Gspt prms;
	UG_segstli *sp;
/*	char us[150];*/

#ifdef UU_CHECK
	ug_chkad(points,"ug_polymk");
#endif

	uu_denter2(UU_GITRC,(us,"ug_polymk(n=%d, points[0]=%g %g)",n,
				points[0].x,points[0].y));

	prms.id=UG_DPOLYMK; prms.n=n;
	prms.points=points;
	if (ug_find.find==UG_FALSE)
		ug_wkout(&prms,i=sizeof(prms)/sizeof(int));
	else
		ug_dpolymk(&prms);
	if ((ug_viwseg>=0)&&(ug_ndcseg>0)) {
		sp=ug_segac(ug_viwseg);
		ug_boxexpmk2(sp,points,n);
	}
	uu_dexit;
}

/********************************************************************* 
**  S_FUNCTION:  ug_dpolymk(prms) -- 2D polymarker simulation routine.
**      Workstation simulation routine to break up polymarkers into single 
**			markers and transform world coordinates to NDC coordinates. Useful for
**			workstations that can't handle world coordinates. ug_dpolymk calls
**			the workstation's GPNTOP entry n times, once per marker.
**  PARAMETERS   
**      INPUT:  struct {int id; Gws ws; 
**						int n; 				 number of points in polyline 
**						Gwpoint *points;  2D world coordinates of polymarker
**					 } *prms;
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_dpolymk(prms)
Gspt *prms;
{

#ifdef UU_CHECK
	ug_chkad(prms,"ug_dpolymk");
#endif

	if (ug_gksos.sysstate==UG_SGOP) {
		if (ug_segac(ug_gksstli.opnseg)->segatts.gvis==UG_VISIBLE) 
			ug_pmka2((*prms).n,(*prms).points);
	}
	else
		ug_pmka2((*prms).n,(*prms).points);
}

/********************************************************************* 
**  S_FUNCTION:  ug_d2polymk(prms) -- 2D polymarker simulation routine.
**      Workstation simulation routine to break up polymarkers into single 
**			markers and transform world coordinates to NDC coordinates. Useful for
**			workstations that can't handle world coordinates. ug_dpolymk calls
**			the workstation's GPNTOP entry n times, once per marker.
**			Just like ug_dpolymk except always calls ws, even if segment
**			is invisible. Used by workstations that maintain own segments.
**  PARAMETERS   
**      INPUT:  struct {int id; Gws ws; 
**						int n; 				 number of points in polyline 
**						Gwpoint *points;  2D world coordinates of polymarker
**					 } *prms;
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_d2polymk(prms)
Gspt *prms;
{

#ifdef UU_CHECK
	ug_chkad(prms,"ug_dpolymk");
#endif
	ug_pmka2((*prms).n,(*prms).points);
}

/*********************************************************************
**    I_FUNCTION   :  int ug_polymkras(n,points) -- draw raster polymarker.
**    PARAMETERS   
**       INPUT  : 
**          input
**				int n					number of points to put markers at
**				Gipoint *points	pointer to array of 2D raster coords
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_polymkras(n,points)			/* draw raster polymarker */
int n;
Gipoint *points;
{
	int i;
	Gipt prms;
/*	char us[120];*/

#ifdef UU_CHECK
	ug_chkad(points,"ug_polymkras");
#endif

	uu_denter2(UU_GITRC,(us,"ug_polymkras(n=%d, points[0]=%d %d)",
			n,points[0].x,points[0].y));

	prms.id=UG_DPOLYMKRAS;
	prms.n=n;
	prms.points=points;
	ug_wkout(&prms,i=sizeof(prms)/sizeof(int));
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  ug_dpolymkras(prms) -- raster polymarker sim.
**       Break raster polymarker into single raster markers.
**    PARAMETERS   
**       INPUT  : 
**          input
**				struct {
**					int id;
**					Gws ws;
**					int n;
**					Gipoint  *points;
**				} *prms			pointer to structure containing function id,
**									the workstation id, the number of points to
**									mark, and a pointer to an array of raster coords
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dpolymkras(prms)
Gipt *prms;
{
	int i;
	Gws ws;
/*	char us[150];*/

#ifdef UU_CHECK
	ug_chkad(prms,"ug_dpolymkras");
#endif

	uu_denter2(UU_GITRC,(us,"ug_dpolymkras(ws=%d,n=%d, points[0]=%d %d",
			(*prms).ws,(*prms).n,(*prms).points[0].x,(*prms).points[0].y));

	ws=(*prms).ws;
	for (i=0; i<(*prms).n; i++)	{
		(*(ug_gksstli.wsopen[ws].connid)[UG_DMARKERRAS])
				((*prms).points[i].x,(*prms).points[i].y,
				&ug_gksstli.curprats.mkbundl);
				 
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gtext(position,string) -- text.
**      Text.  Draw string starting at position.
**  PARAMETERS   
**      INPUT:  Gwpoint *position -- starting position.
**					 Gchar *string -- character string to draw.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gtext(position,str)		/* string str at cp of length l */
/*$ INPUT */
Gchar str[];
Gwpoint3 *position;
{
	Gnrect3 nrect;				/* text rectangle */
	UG_segstli *sp;
	Gerror irtn;
	int len;
/*	char us[180];*/

	irtn=NCL_NO_ERROR;

#ifdef UU_CHECK
	irtn=ug_chkad(position,"gtext position");
	irtn=ug_chkad(str,"gtext str");
#endif

  uu_denter2(UU_GTRC,(us,"gtext(%g %g %g,%s)",
							(*position).x,(*position).y,(*position).z,str));

	len=strlen(str);
#ifdef UU_CHECK
	Gerror irtn2;
	irtn=NCL_NO_ERROR;
	if (len>150) {
		ug_errorhand(EBADMSG,"gtext"); irtn=EBADMSG;
	}
	if ((irtn2=ug_chkpoints3(1,position,"gtext"))!=NCL_NO_ERROR)
		irtn=irtn2;
	if (irtn==NCL_NO_ERROR) {
#endif
		ug_text(position,str);
		if (ug_gksos.sysstate==UG_SGOP) {
			ug_ntext(ug_segac(ug_gksstli.opnseg)->seglist,position,str);
			ug_txrect(position,str,&nrect);
			sp=ug_segac(ug_gksstli.opnseg);
			ug_updxforms(sp);					/* update xforms bits in seg header*/
		}

#ifdef UU_CHECK
	}
#endif

	rtn: uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_text(position,string) 
**				call workstation to draw graphics text.
**    PARAMETERS   
**       INPUT  :  Gwpoint3 *position; -- start posn of string.
**						 Gchar string[]; -- the text.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_text(position,string)
Gwpoint3 *position;
Gchar string[];
{
	Gnrect3 nrect;
	Gint len;
	int prmsiz;
	Gfloat dx,dy;
	Gfloat dxl,dxr,dyl,dyu;
/*	Gnrect3 resultbox;*/
	UG_dtext txprms;				/* parms for UG_DTEXT call to workstation */
	UG_segstli *sp;
	uu_denter(UU_GITRC,(us,"ug_text(%g %g %g, %s)",position->x,
		position->y,position->z,string));
	if (ug_find.find==UG_TRUE) {			/* finding, dont draw text */
		ug_txrect(position,string,&nrect);	/* get nrect=text NDC rectangle */
		dxl=nrect.llf.x-ug_find.x;
		dxr=ug_find.x-nrect.urb.x;
		dx=(dxl>dxr)?dxl:dxr;	/* dx=distance ug_find.x is from text rect, 
											negative if within rectangle */
		dyl=nrect.llf.y-ug_find.y;
		dyu=ug_find.y-nrect.urb.y;
		dy=(dyl>dyu)?dyl:dyu;	/* dy=distance ug_find.y is from text rect 
											or negative if within rectangle */
		switch(ug_find.pikmode) {
		case 0:							/* part or all in */
			/* see if ug_find posn is within epsx,epsy of text rectangle */
			if ((dx<ug_find.epsx)&&(dy<ug_find.epsy)) {
				ug_find.found=UG_TRUE;
				ug_find.dist=(dx>dy)?dx:dy;
				if (ug_find.dist<0.) ug_find.dist=0.;
			}
			uu_denter2(UU_GITRC,(us,
				"gtext. finding loc=%g %g, rect=%g %g, %g %g, found=%d",
				ug_find.x,ug_find.y,nrect.llf.x,nrect.llf.y,nrect.urb.x,
						nrect.urb.y,ug_find.found));
			uu_dexit;
			goto rtn;
		case 1:										/* all or part out */
			if ((nrect.llf.x>(ug_find.x+ug_find.epsx))||
				(nrect.llf.y>(ug_find.y+ug_find.epsy))||
				(nrect.urb.x<(ug_find.x-ug_find.epsx))||
				(nrect.urb.y<(ug_find.y-ug_find.epsy))) {
				ug_find.found=UG_TRUE;
				ug_find.dist=(dx>dy)?dx:dy;
				if (ug_find.dist<0.) ug_find.dist=0.;
			}
			break;
		case 2:									/* text is all within pick aperture */
			if ((nrect.llf.x>(ug_find.x-ug_find.epsx))&&
				 (nrect.llf.y>(ug_find.y-ug_find.epsy))&&
				 (nrect.urb.x<(ug_find.x+ug_find.epsx))&&
				 (nrect.urb.y<(ug_find.y+ug_find.epsy))) {
				ug_find.found=UG_TRUE;
				ug_find.dist=(dx>dy)?dx:dy;
				if (ug_find.dist<0.) ug_find.dist=0.;
			}
			break;
		case 3:										/* all out */
			if ((nrect.llf.x>(ug_find.x+ug_find.epsx))||
				 (nrect.llf.y>(ug_find.y+ug_find.epsy))||
				 (nrect.urb.x<(ug_find.x-ug_find.epsx))||
				 (nrect.urb.y<(ug_find.y-ug_find.epsy))) {
				ug_find.found=UG_TRUE;
				ug_find.dist=(dx>dy)?dx:dy;
				if (ug_find.dist<0.) ug_find.dist=0.;
			}
			break;
		}												/* end switch(ug_find.pikmode) */
	}								/* end of finding. */
	else {						/* not finding. draw text */ 
		/* see if any of the text rectangle is in viewport */
		/*ug_txrect(position,string,&nrect);	get nrect=text NDC rectangle */
		/*if (ug_drectintersect(&resultbox,&nrect,
				&ug_gksstli.vtran[ug_gksstli.curvwindex].vport)==0) */
		len = strlen(string);
		txprms.op=UG_DTEXT; txprms.slen=len;
		txprms.pos.x=(*position).x; txprms.pos.y=(*position).y;
			txprms.pos.z=(*position).z;
		strcpy(txprms.s,string);
/*
.....Just retrieving Hershey text strokes
.....Call ug_hershey directly
.....This is required for Batch mode
*/
		if (ug_text_iscaptured())
			ug_hershy(&txprms,0);
/*
.....Otherwise draw the text
*/
		else
			ug_wkout(&txprms,prmsiz=sizeof(txprms)/sizeof(int));
	}												/* end not finding, draw text */
/*
.....Segments with graphic text cannot
.....support a WC box
*/
	if ((ug_viwseg>=0)&&(ug_ndcseg>0))
	{
		sp=ug_segac(ug_viwseg);
		if (sp != UU_NULL) UG_RESET_WCBOX(sp);
/*		ug_txrect(position,string,&nrect);
		ug_ndcboxadd(sp,&nrect.llf,&nrect.urb);*/
	}
rtn:	
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_textras(posn,s,wsid) -- Draw text at raster posn.
**    PARAMETERS   
**       INPUT  : 
**				Gipoint  *posn	raster coords of begin character position
**				Gws wsid			workstaton for which this raster seg was created
**				char *s			character string to display
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_textras(posn,s,wsid)
Gipoint  *posn;
Gws wsid;			/* workstaton for which this raster seg was created*/
char *s;
{
	struct {int id; Gws ws; Gipoint p; char *str;} prms;
	int reply[4];
	UG_wdt *wdtpt;
	int ht,width;							/* size of text box */
	int textur[2];							/* upper right of text box */
	int textll[2];							/* lower left of text box */
	int pikll[2],pikur[2];				/* raster corners of pick aperture */
	int scalex,scaley;					/* device to raster scale factor */
	Gnpoint pikndc;
	int fposn[2];
	Gfloat dxl,dyl,dxr,dyr,dx,dy,dist;	/* use in calculation of distance */
/*	char us[150];*/

#ifdef UU_CHECK
	ug_chkad(posn,"ug_textras posn");
	ug_chkad(s,"ug_textras s");
#endif

	uu_denter2(UU_GITRC,(us,"ug_textras(%d %d,%s)",(*posn).x,(*posn).y,s));

	prms.id=UG_DRASTEXT;
	prms.p.x=(*posn).x; prms.p.y=(*posn).y;
	prms.str=s;
	if (ug_find.find==UG_FALSE)
		ug_wkout(&prms,reply,(sizeof(prms)+sizeof(int)-1)/sizeof(int));
	else {					/* finding, don't draw */
		wdtpt=ug_gksstli.wsopen[wsid].wdtptr;
		/* calc size of text box */
		ht=(*wdtpt).dspsize.raster.y/(*wdtpt).rowmax;
		width=strlen(s)*(*wdtpt).dspsize.raster.x/(*wdtpt).colmax;
		textur[0]=(*posn).x+width;
		textur[1]=(*posn).y+ht;
		textll[0]=(*posn).x;
		textll[1]=(*posn).y;
		/* calc raster size of pik aperture box */
		scalex=(*wdtpt).dspsize.raster.x/(*wdtpt).dspsize.device.x;
		scaley=(*wdtpt).dspsize.raster.y/(*wdtpt).dspsize.device.y;
		pikndc.x=ug_find.x-ug_find.epsx;
		pikndc.y=ug_find.y-ug_find.epsy;
		(*(ug_gksstli.wsopen[wsid].connid)[UG_DNDCDEV])
			(&pikndc,pikll,wsid);			/*convert lower left to raster coords*/
		pikndc.x=ug_find.x+ug_find.epsx;
		pikndc.y=ug_find.y+ug_find.epsy;
		(*(ug_gksstli.wsopen[wsid].connid)[UG_DNDCDEV])
			(&pikndc,pikur,wsid);			/*convert upper right to raster coords*/
		(*(ug_gksstli.wsopen[wsid].connid)[UG_DNDCDEV])
			(&ug_find.x,fposn,wsid);			/*convert center to raster coords*/
		uu_denter2(UU_GITRC,(us,
			"ug_textras. pikbox=%d %d %d %d,textbox=%d %d %d %d mode=%d",
			pikll[0],pikll[1],pikur[0],pikur[1],textll[0],textll[1],
			textur[0],textur[1],ug_find.pikmode));
		uu_dexit;
		switch(ug_find.pikmode) {
		case 0:							/* all or part in */
			/* see if pik box overlaps text rectangle */
			if ((pikur[0]>=textll[0])&&(pikur[1]>=textll[1])&&
				(pikll[0]<=textur[0])&&(pikll[1]<=textur[1])) 
				/* found it. */
				ug_find.found=UG_TRUE;
			break;
		case 1:							/* all or part out */
			if ((pikur[0]<textur[0])||(pikur[1]<textur[1])||
				 (pikll[0]>textll[0])||(pikll[1]>textll[1]))
				ug_find.found=UG_TRUE;
			break;
		case 2:							/* all in */
			if ((pikur[0]>=textur[0])&&(pikur[1]>=textur[0])&&
				 (pikll[0]<=textll[0])&&(pikll[1]<=textll[1]))
				 ug_find.found=UG_TRUE;
			break;
		case 3:							/* all out */
			if ((pikur[0]<textll[0])||(pikur[1]<textll[1])||
				 (pikll[0]>textur[0])||(pikll[1]>textur[1]))
				 ug_find.found=UG_TRUE;
			break;
		}									/* end switch(ug_find.pikmode) */
		if (ug_find.found==UG_TRUE) {
			/* calc dist from pick center (fposn) to text box */
			dxl=textll[0]-fposn[0]; dxl=dxl/scalex;
			dxr=fposn[0]-textur[0]; dxr=dxr/scalex;
			dx=(dxl>dxr) ? dxl : dxr;	/* dx=x dist to box, neg if in box */
			dyl=textll[1]-fposn[1]; dyl=dyl/scaley;
			dyr=fposn[1]-textur[1]; dyr=dyr/scaley;
			dy=(dyl>dyr) ? dyl : dyr;	/* dy= y dist to box, neg if in box */
			dist=(dx > dy) ? dx : dy;
			if (dist<0.) dist=0.;
			ug_find.dist=dist;
		}

		uu_denter2(UU_GITRC,(us,
			"ug_textras fposn=%d %d, found=%d dist=%g",
			fposn[0],fposn[1],ug_find.found,ug_find.dist));
		uu_dexit;

	}										/* end finding */
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gchar *ggdp3(n,points,function,data)
**      3D Generalized drawing primitive. Way to generate non-standard
**			primitives.
**  PARAMETERS   
**      INPUT:  Gint n -- number of points.
**					 Gwpoint3 *points -- world coordinate points.
**					 (*function)() -- pointer to GDP function.
**					 Gchar *data -- GDP data record
**
**  RETURNS      :  Dependent upon the GDP function.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ggdp3(n,points,function,data)
/*$ INPUT */
Gint n;					/* number of points */
Gwpoint3 *points;
Gint (*function)();
Gchar *data;
{
	Gerror irtn;

	uu_denter(UU_GTRC,(us,"ggdp3(%d,points,function,data)",n));

	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	Gerror irtn2;
	if (points!=NULL) ug_chkad(points,"ggdp3 points");
	if (data!=NULL) ug_chkad(data,"ggdp3 data");
	irtn=ug_chkwsac("ggdp3");
	if ((n<0)||(n>=(UG_lismaxsiz-1)/3)) {		/* bad number of points */
		ug_errorhand(ENPOINTS,"ggdp3",&n); irtn=ENPOINTS;
	}
	if (n>0) 
		if ((irtn2=ug_chkpoints3(n,points,"ggdp3"))!=NCL_NO_ERROR)  irtn=irtn2;
	if (irtn==NCL_NO_ERROR) {
#endif
	/* not implemented yet */
	/* here should xform points to ndc */
	(*function)(data);			/* call the gdp function */
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
}
