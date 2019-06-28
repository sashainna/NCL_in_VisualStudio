/*********************************************************************
**    NAME         :  gout2.c -- DIGS output functions (cont).
**       CONTAINS:
**		ug_pag()
**		ug_lina3(p1,p2)   -- draw or find a line between two 3D WC points.
**		ug_linndc(p1,p2)   -- draw line between 2 ndc points.
**		int ug_plna3(n,points)
**    int ug_plna2(n,points)
**		ug_d4polyln3(prms)
**		ug_d5polyln3(prms)
**		int ug_closln(p,s,e,epsx,epsy,rdist,mode)
**    Gfloat ug_ptlndist(p,s,e,&dx,&dy) - dist from pt to line.
**		int ug_pmka2(n,points)
**		ug_pmka3(n,points)
**		ug_mrka3(p1)
**		ug_updxforms(sp) -- update xforms in open segment.
**  	Gerror gcallseg(n)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gout2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:22
*********************************************************************/
#define UG_TRUE 1
#define UG_FALSE 0
#define MINREPS (UU_REAL) 2.0		/* Minimum number of times a line style pattern must 
							be repeated on a line before it is drawn as a solid line.
							If the polyline has 2 endpoints, pattern will be used as
							long as 1 rep of the patern will fit on the line. */
#define logical int
#include "zsysdep.h"
#include "umath.h"
#include <stdio.h>
#include "g.h"
#include "gvlib.h"
#include "gviw.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "udebug.h"
#include "gsegac.h"
#include "ginqatt.h"
#include "gmat4.h"

/*
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gout2.c 2.28 10/27/86 11:00:35 single"};
#else
static char uu_sccsident[]={"@(#) gout2.c 2.28 10/27/86 11:00:35 double"};
#endif
*/
Gerror ug_chkpoints3(),ug_chkpoints();
char *uu_toolmalloc();
#define INTSIZ(x) ((x+sizeof(int)-1)/sizeof(int))
#define INTSIZEOF(x) ((sizeof(x)+sizeof(int)-1)/sizeof(int))

void ug_lin5ndc(),ug_d5polyln3(),ug_mrka3(),ug_lin4ndc(),ug_d4polyln3();
void ug_p5lna3(),ug_p4lna3(),ug_linndc();

typedef struct {int id; Gws ws; Gseg n; Gtran xform;} Stcall;
typedef struct {int id; Gws ws; int n; Gwpoint3 *points;} Stpolyln3;
typedef struct {int id; Gws ws; int n; Gwpoint *points;} Stpolyln;
typedef struct {int id; Gws ws; int n; Gwpoint3 *points;} Stpolyln4;

extern struct {
	int mod;				/* 0=no REPLACE modelling xform in open seg yet */
	int ntran;			/* 0=no GSNORMTRAN in open seg yet */
}	ug_xfseg;	
/*
.....These were incorrect patterns for the various linestyles
.....JLS 8/25/99
Glntype ug_lntypes[7]={
		{2,4,.02,1,1,1,0},		
		{3,4,.02,1,0,0,0},	
		{4,3,.015,1,1,0},	
		{5,10,.05,1,0,1,0,1,1,1,1,1,0},	
		{6,6,.03,1,1,1,1,0,0},			
		{7,9,.04,1,1,1,1,1,0,1,1,0},
		{8,4,.02,1,1,0,0}};		
*/

Glntype ug_lntypes[7]={
		{2,4,.02,1,1,0,0},		/* small dashed line pattern */
		{3,4,.01,1,0,0,0},		/* dotted line pattern */
		{4,14,.02,1,0,0,0,1,1,1,1,1,1,1,0,0,0},	/* centerline */
		{5,12,.03,1,0,0,1,0,0,1,1,1,1,0,0},	/* 3 dots dash patn - phantom*/
		{6,7,.02,1,1,1,1,1,0,0},			/* longer dashes than 8 */
		{7,12,.03,1,1,1,1,1,1,0,0,1,1,0,0},	/* long short dash */
		{8,9,.02,1,1,1,0,0,0,0,0,0}};		/* midsize dashes */

/*********************************************************************
**    I_FUNCTION     :  ug_pag() --  new page action.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_pag()                 /* new page action */
{
	int prms[4];
	int prmsiz;
	uu_denter(UU_GITRC,(us,"ug_pag()"));
	prms[0]=UG_DPAGOP;
	if (ug_find.find==UG_FALSE) ug_wkout(prms,prmsiz=sizeof(prms)/sizeof(int));
	prms[2]=ug_gksstli.vtran[ug_gksstli.curvwindex].vport.llf.x;
	prms[3]=ug_gksstli.vtran[ug_gksstli.curvwindex].vport.llf.y;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_lina3(p1,p2)   -- draw line from p1 to p2.
**    PARAMETERS   
**       INPUT  : 
**				Gwpoint3 *p1,*p2	pointers to 3D WC 'from' and 'to' points
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_lina3(p1,p2)               /* draw line from p1 to p2 */
Gwpoint3 *p1,*p2;
{ 
	Gfloat dist;
  Gwpoint3 opos,npos;
  Gnpoint3 pos1,pos2;
  int onscr,chg2;

#ifdef UU_CHECK
	ug_chkad(p1,"ug_lina3 p1");
	ug_chkad(p2,"ug_lina3 p2");
	irtn=ug_chkpoints3(1,p1,"ug_lina3 p1");
	irtn=ug_chkpoints3(1,p2,"ug_lina3 p2");
#endif
	 uu_denter2(UU_GITRC,(us,"ug_lina3(%g %g %g,%g %g %g)",
  					(*p1).x,(*p1).y,(*p1).z,(*p2).x,(*p2).y,(*p2).z));
	ug_xform((*p1).x,(*p1).y,(*p1).z,&opos,ug_cxform[ug_gksstli.curvwindex]);
	ug_xform((*p2).x,(*p2).y,(*p2).z,&npos,ug_cxform[ug_gksstli.curvwindex]);
	ug_clipexp(&opos,&npos,&pos1,&pos2,&onscr,&chg2);
	if (onscr!=0) {
		if (ug_find.find==UG_FALSE) {
			ug_linndc(&pos1,&pos2);		/* draw the line */
		}
		else {                       /* dont draw, just look for x,y */
			/* see if the line from pos1 to pos2 is within eps of ug_find.x,y */
     		if (ug_closln(&ug_find.x,&pos2,&pos1,ug_find.epsx,ug_find.epsy,
				&dist,ug_find.pikmode)==1) {
				ug_find.found=UG_TRUE; ug_find.dist=dist;
			}
     	}
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_linndc(p1,p2)   -- draw line from p1 to p2.
**    PARAMETERS   
**       INPUT  : 
**				Gwpoint3 *p1,*p2	pointers to 3D NDC  'from' and 'to' points,
**							already clipped.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_linndc(p1,p2)   		/* draw line from p1 to p2 (NDC coords), don't clip */
Gnpoint3 *p1,*p2;
{ int i;
	UG_line prms;
  int prmsiz;
  Gnpoint3 linein[2];				/* for ug_lnstyle */
  Glntype *p;
  int nlineout;
  Gnpoint3 *lineout;

	uu_denter2(UU_GITRC,(us,"ug_linndc(%g %g %g,%g %g %g)",
  					(*p1).x,(*p1).y,(*p1).z,(*p2).x,(*p2).y,(*p2).z));
	prms.p1.x=(*p1).x; prms.p1.y=(*p1).y;
	prms.p2.x=(*p2).x; prms.p2.y=(*p2).y;
	prms.op=UG_DDRWOP;
	if (ug_gksstli.curprats.lnbundl.type.typeno!=21)
		ug_wkout(&prms,prmsiz=sizeof(prms)/sizeof(int));
	else {							/* parameterized linestyle */
		p= &ug_gksstli.curprats.lnbundl.type;
		linein[0].x=(*p1).x; linein[0].y=(*p1).y; linein[0].z=(*p1).z;
		linein[1].x=(*p2).x; linein[1].y=(*p2).y; linein[1].z=(*p2).z;
		ug_lnstyle(linein,&nlineout,&lineout);
		for (i=0; i<nlineout; i++) {	/* draw a line for each dash */
			prms.p1.x=lineout[2*i].x;
			prms.p1.y=lineout[2*i].y;
			prms.p2.x=lineout[2*i+1].x;
			prms.p2.y=lineout[2*i+1].y;
			ug_wkout(&prms,prmsiz=sizeof(prms)/sizeof(int));
		}
		uu_toolfree(lineout);
	}									/* end parameterized linestyle */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_plna3(n,points) -- draw polyline 3D.
**    PARAMETERS   
**       INPUT  : 
**				Gint n				number of points in polyline
**				Gwpoint3 points[]	array of 3D WC points
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_plna3(n,points)						/* polyline abs 3 */
Gint n; Gwpoint3 points[];
{
	int i;
	Gnpoint3 np1,np2,pos1,pos2;
	int onscr,chg2;
	Gfloat dist;
	Gfloat epsx,epsy;				/* finding aperture */

	uu_denter(UU_GITRC,(us,"ug_plna3(%d,points)",n));
#ifdef UU_CHECK
	ug_chkad(points,"ug_plna3");
#endif
	ug_xform(points[0].x,points[0].y,points[0].z,&np1,
			ug_cxform[ug_gksstli.curvwindex]);

	if (ug_gksstli.curprats.lnbundl.type.typeno==21)/* parameterized lnstyle */
		ug_reset_pattern();
	if (ug_find.find==UG_TRUE) {	
		epsx=ug_find.epsx;			/* save the finding aperture */
		epsy=ug_find.epsy;
	}
	for (i=1; i<n; i++) { 
		ug_xform(points[i].x,points[i].y,points[i].z,&np2,
				ug_cxform[ug_gksstli.curvwindex]);
		ug_clipexp(&np1,&np2,&pos1,&pos2,&onscr,&chg2);
		if (onscr!=0) {
			if (ug_find.find==UG_FALSE) {
				ug_linndc(&pos1,&pos2);			/* draw ndc line */
			}
			else {                       /* dont draw, just look for x,y */
				/* see if the line from pos1 to pos2 is within (without,
					part in/out) epsx,epsy of ug_find.x,y. Use  the same
					epsx,epsy for each line segment in this polyline. */
  	   		if (ug_closln(&ug_find.x,&pos2,&pos1,epsx,epsy,
						&dist,ug_find.pikmode)==1) {		/* found it */
					/* remember we found it */
					if ((ug_find.found==UG_FALSE)||(dist<ug_find.dist)) {
						/* 1st segment found, or closest so far */
						ug_find.dist=dist;
						ug_find.epsx=dist;
						ug_find.epsy=dist;
						ug_find.found=UG_TRUE; 
					}
					/* if we are looking for part in or partout, this line segment
						passing the test causes the whole polyline to pass */
					if ((ug_find.pikmode==0)||(ug_find.pikmode==1))  {
						break;			/* exit the for each line segment loop */
					}
				}
				else {					/*  didn't find it */
					/* if we are looking for all in or all out for this
						polyline, this line segment failing the test causes
						the whole polyline to fail */
					if ((ug_find.pikmode==2)||(ug_find.pikmode==3)) {
						ug_find.found=UG_FALSE;
						ug_find.epsx=epsx;		/* put back the original aperture*/
						ug_find.epsy=epsy;
						break;			/* exit the for each line segment loop */
					}
				}
  	   	}
		}												/* end if (onscr!=0) */
		np1=np2;
	}													/* end for (i=1; i<n) */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_plna2(n,points) -- draw polyline  2D.
**    PARAMETERS   
**       INPUT  : 
**          input
**				Gint n				number of points in polyline
**				Gwpoint points[]	array of 2D WC points
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_plna2(n,points)							/* polyline abs 2 */
Gint n; Gwpoint points[];				/* not user callable */
{
	int i;
	Gwpoint3 p1,p2;
#ifdef UU_CHECK
	ug_chkad(points,"ug_plna2");
#endif
	 uu_denter2(UU_GITRC,(us,"ug_plna2(%d,points)",n));
	p1.x=points[0].x; p1.y=points[0].y; p1.z=0.0;
	for (i=1; i<n; i++){
		p2.x=points[i].x; p2.y=points[i].y; p2.z=0.0;
		ug_lina3(&p1,&p2);		/* more efficient to xlate here, not call ug_lina3*/
		zbytecp(p1,p2);							/* structure assignment */
	}
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION :  ug_d4polyln3(prms) -- polyline3 sim. routine which
**							software simulates all linestyles.
**													and line widths
**    PARAMETERS   
**       INPUT  : 	
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_d4polyln3(prms)
Stpolyln4 *prms;
{
#ifdef UU_CHECK
	ug_chkad(prms,"ug_d4polyln3");
#endif
	uu_denter2(UU_GITRC,(us,"ug_d4polyln3(%d,%d, %g %g %g)",(*prms).ws,
			(*prms).n,(*prms).points[0].x,(*prms).points[0].y,
			(*prms).points[0].z));
		  
/*
.....ug_gksstli.curprats.lnbundl.type.typeno can go up to 8, changed it
.....to be greater than 8 instead of greater than 7 JLS 8/23/99
*/
	if ((ug_find.find==UG_TRUE)||(ug_gksstli.curprats.lnbundl.type.typeno<2)
			|| (ug_gksstli.curprats.lnbundl.type.typeno>8))
		{
		if(gqlinewidth() < 2.0)
			ug_d3polyln3(prms);
		else
			{
			/*	simulate wide lines	*/
      	Gnpoint3 *points;
			int i;
	 
      	points = (Gnpoint3 *)uu_malloc(prms->n * sizeof(Gnpoint3));
      	for(i=0;i<prms->n;i++)
         	UG_XFORM(prms->points[i].x,prms->points[i].y,prms->points[i].z,
            	&points[i], ug_cxform[ug_gksstli.curvwindex]);
      	ug_dlinewidth(prms->ws, ug_gksstli.curprats.lnbundl.width,
         	ug_gksstli.wsopen[prms->ws].wdtptr->outwdtpt->lnfac.nom,
         	prms->n, points);
      	uu_free(points);
			}
		}
	else {						/* not finding, and 2<=linetype<=8 */
		if (ug_gksos.sysstate==UG_SGOP) {
			if (ug_segac(ug_gksstli.opnseg)->segatts.gvis==UG_VISIBLE)
				ug_p4lna3((*prms).n,(*prms).points);
		}
		else
			ug_p4lna3((*prms).n,(*prms).points);
	}								/* end not finding */
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION :  ug_d4polyln(prms) -- polyline sim. routine which
**							software simulates all linestyles.
**													and line widths
**    PARAMETERS   
**       INPUT  : 	
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_d4polyln(prms)
Stpolyln *prms;
{
	int n,i;
	Stpolyln4 prms3;
	Gwpoint3 pts[20];
#ifdef UU_CHECK
	ug_chkad(prms,"ug_d4polyln");
#endif
	uu_denter2(UU_GITRC,(us,"ug_d4polyln(%d,%d, %g %g)",(*prms).ws,
			(*prms).n,(*prms).points[0].x,(*prms).points[0].y));
	/* just change points to 3d and call ug_d4polyln3 */
	n=prms->n;
	if (n>20) prms3.points=(Gwpoint3 *)uu_toolmalloc(n*sizeof(Gwpoint3));
	else prms3.points=pts;
	for (i=0; i<n; i++) {
		prms3.points[i].x=prms->points[i].x;
		prms3.points[i].y=prms->points[i].y;
		prms3.points[i].z=0.;
	}
	prms3.id=prms->id; prms3.ws=prms->ws;
	prms3.n=prms->n;
	ug_d4polyln3(&prms3);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_p4lna3(n,points) -- draw the n points, simulating
**													all linestyles (2<=linestyle<=8).
**													and line widths
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_p4lna3(n,points)
int n;
Gwpoint3 *points;
{
	Gnpoint3 np1,np2;
	Gnpoint3 pos1,pos2;
	int onscr,chg2;
	int type;					/* current line type */
	int i;
	Gfloat dx,dy,dz,len;
	int ws=0;
	Gfloat reps;

	type=ug_gksstli.curprats.lnbundl.type.typeno;
	uu_denter2(UU_GITRC,(us,"ug_p4lna3(%d, %g %g %g) linetype=%d",
		n,points[0].x,points[0].y,points[0].z,type));
	len = 0.0;
	ug_xform(points[0].x,points[0].y,points[0].z,&np1,
			ug_cxform[ug_gksstli.curvwindex]);
	for(i=1;i<n;i++)
		{
		ug_xform(points[i].x,points[i].y,points[i].z,&np2,
				ug_cxform[ug_gksstli.curvwindex]);
		dx = np2.x - np1.x; dy = np2.y - np1.y; dz = np2.z - np1.z;
		len += sqrt(dx*dx+dy*dy+dz*dz);
		np1=np2;
		}
	reps = len/ug_lntypes[type-2].patnlen;
	if(reps < MINREPS)

			/* If pattern is repeatable less than MINREPS times, ignore
				pattern and draw as solid line unless there are only 2 
				endpoints.  In that case draw solid if 1 rep of pattern 
				won't fit on line	*/

		{
		int savetype;

		struct {int id; Gws ws; int n; Gwpoint3 *points;} prms;
		prms.id = UG_DPOLYLN3;
		prms.ws = ws;
		prms.n = n;
		prms.points = points;

		if((n == 2) && (reps >= 1.0)) goto tst;
		savetype = ug_gksstli.curprats.lnbundl.type.typeno;
		ug_gksstli.curprats.lnbundl.type.typeno = 1;
		if(gqlinewidth() < 2.0)
			{
			ug_d3polyln3(&prms);
			}
		else
			{
			int i;

			/*	simulate wide lines	*/
	 
      	for(i=0;i<prms.n;i++)
         	UG_XFORM(points[i].x,points[i].y,points[i].z,
            	&points[i], ug_cxform[ug_gksstli.curvwindex]);
      	ug_dlinewidth(ws, ug_gksstli.curprats.lnbundl.width,
         	ug_gksstli.wsopen[ws].wdtptr->outwdtpt->lnfac.nom,
         	n, points);
      	uu_free(points);
			}
		ug_gksstli.curprats.lnbundl.type.typeno = savetype;
		uu_dexit; return;
		}
tst:
	if((reps-(int)reps) > .001)
		{
		/* Reset pattern so an integer number of patterns fits on polyline */
		Glntype style;
		for(i=0;i<ug_lntypes[type-2].npatn;i++)
			style.typepatn[i] = ug_lntypes[type-2].typepatn[i];
		style.npatn=i;
		style.typeno = ug_lntypes[type-2].typeno;
		style.patnlen = len/(int) (reps +.5);
		uu_dprint(UU_GITRC,(us,"reps = %g, len = %g, patnlen = %g",
			reps, len, style.patnlen));
		ug_set_pattern(&style);
		}
 else
		ug_set_pattern(&ug_lntypes[type-2]);
	ug_xform(points[0].x,points[0].y,points[0].z,&np1,
			ug_cxform[ug_gksstli.curvwindex]);
	for (i=1; i<n; i++) { 
		ug_xform(points[i].x,points[i].y,points[i].z,&np2,
				ug_cxform[ug_gksstli.curvwindex]);
		ug_clipexpln(&np1,&np2,&pos1,&pos2,&onscr,&chg2,
			ug_gksstli.wsopen[0].wdtptr->outwdtpt->lnfac.nom);
		if (onscr!=0) {
			ug_lin4ndc(&pos1,&pos2);			/* draw ndc line */
		}												/* end if (onscr!=0) */
		np1=np2;
	}													/* end for (i=1; i<n) */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_lin4ndc(p1,p2)   -- draw line from p1 to p2,
**								simulating all linestyles and line widths
**    PARAMETERS   
**       INPUT  : 
**				Gwpoint3 *p1,*p2	pointers to 3D NDC  'from' and 'to' points,
**							already clipped.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_lin4ndc(p1,p2)   		/* draw line from p1 to p2 (NDC coords), don't clip */
Gnpoint3 *p1,*p2;
{ int i;
	UG_line prms;
  int prmsiz;
  Gnpoint3 linein[2];				/* for ug_lnstyle */
  Glntype *p;
  int nlineout;
  Gnpoint3 *lineout;
  Gfloat width;
  Glntype ntyp, otyp;

	uu_denter2(UU_GITRC,(us,"ug_lin4ndc(%g %g %g,%g %g %g)",
					(*p1).x,(*p1).y,(*p1).z,(*p2).x,(*p2).y,(*p2).z)); 
	prms.p1.x=(*p1).x; prms.p1.y=(*p1).y; 
	prms.p2.x=(*p2).x; prms.p2.y=(*p2).y; 
	prms.op=UG_DDRWOP;
	p= &ug_gksstli.curprats.lnbundl.type;
	linein[0].x=(*p1).x; linein[0].y=(*p1).y; linein[0].z=(*p1).z;
	linein[1].x=(*p2).x; linein[1].y=(*p2).y; linein[1].z=(*p2).z;
	width = gqlinewidth();
	uu_dprint(UU_GITRC,(us,"ug_lin4ndc: width = %g",width));
	ug_lnstyle(linein,&nlineout,&lineout);
	zbytecp(otyp,ug_gksstli.curprats.lnbundl.type);
	ntyp.typeno = 1;
	gslinetype(&ntyp);
	for (i=0; i<nlineout; i++) {	/* draw a line for each dash */
		if(width < 2.0)
			{
			prms.p1.x=lineout[2*i].x;
			prms.p1.y=lineout[2*i].y;
			prms.p2.x=lineout[2*i+1].x;
			prms.p2.y=lineout[2*i+1].y;
			ug_wkout(&prms,prmsiz=sizeof(prms)/sizeof(int));
			}
		else
			{
			Gnpoint3 *points;

			points = (Gnpoint3*)uu_toolmalloc(2 * sizeof(Gnpoint3));
			points[0].x = lineout[2*i].x;
			points[0].y = lineout[2*i].y;
			points[0].z = 0.0;
			points[1].x = lineout[2*i+1].x;
			points[1].y = lineout[2*i+1].y;
			points[1].z = 0.0;
			ug_dlinewidth(0,width, 
				ug_gksstli.wsopen[0].wdtptr->outwdtpt->lnfac.nom, 2, points);
			uu_toolfree(points);
			}
	}
	uu_toolfree(lineout);
	gslinetype(&otyp);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_closln(p,s,e,epsx,epsy,rdist,mode)
**       Point P and epsx,epsy define a rectangle centered at P, with
**			sides length 2*epsx,2*epsy. Determine if line from s to e is 
**			part in, part out, all in, or all out of this rectangle.
**    PARAMETERS   
**       INPUT  : 
**				Gfloat p[2],		2D WC/NDC point
**						s[2],		2D WC/NDC beginning of line segment
**						e[2],		2D WC/NDC end of line segment
**						epsx,		delta-x
**						epsy,		dwlta-y
**				int   mode;  	0=part in, 1=part out, 2=all in 3=all out.
**       OUTPUT :  Gfloat *rdist -- if return 1, *rdist=max of horiz or 
*						vert dist from p to line.
**    RETURNS      : 1 if line is within(without, etc), else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_closln(p,s,e,epsx,epsy,rdist,mode)	/* is rect centered at p within 
													(without, etc) epsx,epsy distance 
													of the line segment from s to e */
Gfloat p[2],s[2],e[2],epsx,epsy,*rdist;
int mode;
{
	Gfloat ug_ptlndist();
	Gfloat dx,dy;
	Gfloat xmin,xmax,ymin,ymax;		/* box surrounding line seg */
	Gnrect prect;							/* rect centerd at p epsx,epsy half size*/
	int irtn;
	uu_denter(UU_GITRC,(us,"ug_closln(%g %g,%g %g,%g %g,%g,%g,%d)",p[0],p[1],
								s[0],s[1],e[0],e[1],epsx,epsy,mode)); 
	/* first calculate rectangle surrounding the line segment */
	xmin = ((s[0]<e[0])?s[0]:e[0]);	/* xmin=min(e[0],s[0]) */
	xmax = ((s[0]>e[0])?s[0]:e[0]);
	ymin = ((s[1]<e[1])?s[1]:e[1]);
	ymax = ((s[1]>e[1])?s[1]:e[1]);
	switch (mode) {
	case 0:									/* want at least part in  */
		/* if p-box outside of rectangle bounding the line.. */
		if ((p[0]<(xmin-epsx))||(p[0]>(xmax+epsx))||(p[1]<(ymin-epsy))
			||(p[1]>(ymax+epsy))) {
				irtn=0; break;
		}
		prect.ll.x=p[0]-epsx;
		prect.ll.y=p[1]-epsy;
		prect.ur.x=p[0]+epsx;
		prect.ur.y=p[1]+epsy;
		irtn=ug_lineinrect2(s,e,&prect);
		if (irtn==1) {						/* line goes within pbox */
			*rdist=ug_ptlndist(p,s,e,&dx,&dy);		/* dist from p to line seg */
		}
		break;							/* end of case 0 (at least part in) */
	case 1:								/* want at least part out */
		/* line is part out iff box surrounding line is part out of p-box */
		if ((xmin<(p[0]-epsx))||(ymin<(p[1]-epsy))||
			 		(xmax>(p[0]+epsx))||(ymax>(p[1]+epsy))) {
			irtn=1;						/* line part out */
			*rdist=ug_ptlndist(p,s,e,&dx,&dy);	/* calc dist from p to line seg */
		}
		else  irtn=0;
		break;
	case 2:								/* want all in */
		/* line is all in iff box surrounding  line is all in p-box */
		if ((xmin<(p[0]-epsx))||(ymin<(p[1]-epsy))||
			 		(xmax>(p[0]+epsx))||(ymax>(p[1]+epsy))) {
			irtn=0;			/* line is not all in */
		}
		else {				/* line is all in */
			irtn=1;
			*rdist=ug_ptlndist(p,s,e,&dx,&dy);	/* calc dist from p to line seg */
		}
		break;
	case 3:								/* want all out */
		prect.ll.x=p[0]-epsx;
		prect.ll.y=p[1]-epsy;
		prect.ur.x=p[0]+epsx;
		prect.ur.y=p[1]+epsy;
		if (ug_lineinrect2(s,e,&prect)==0) {		/* line is all out */
			irtn=1;
			*rdist=ug_ptlndist(p,s,e,&dx,&dy);	/* calc dist from p to line seg */
		}
		else irtn=0;
		break;
	}										/* end of switch(mode) */
	uu_denter2(UU_GITRC,(us,"%d=ug_closln(*rdist=%g).",irtn,*rdist));
	uu_dexit;
	uu_dexit;
	return(irtn);
}	

/*********************************************************************
**    I_FUNCTION :  Gfloat ug_ptlndist(p,s,e,&dx,&dy) - dist from pt to line.
**       Return max of horiz and vert dist from point p to line segment 
**			defined by  s and e.
**    PARAMETERS   
**       INPUT  :  Gfloat p[2]; -- point near the line segment.
**						 Gfloat s[2],e[2]; -- points defining the line segment.
**       OUTPUT :  float *dx,*dy -- horiz, vert distance from p to line seg.
**    RETURNS      : max(*dx,*dy)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gfloat ug_ptlndist(p,s,e,dx,dy)
Gfloat p[2],s[2],e[2];
Gfloat *dx,*dy;
{
	Gfloat q[2];
	Gfloat ems[2];					/* e-s */
	Gfloat pms[2];					/* p-s */
	Gfloat emq[2],smq[2];
	Gfloat magems;					/* magnitude(e-s)**2 */
	Gfloat dot;						/* (p-s)dot(e-s) */
	Gfloat dists,diste;
	Gfloat dist;					/* return value */
	Gfloat dx1,dy1;

	/* calculate q = point on infinite line
	passing thru s and e nearest to point p:
	q = s + (p-s)dot(e-s)*(e-s)/magnitude(e-s)  */
	ems[0]=e[0]-s[0]; ems[1]=e[1]-s[1];		/* (e-s) */
	pms[0]=p[0]-s[0]; pms[1]=p[1]-s[1];		/* (p-s) */
	magems = (ems[0]*ems[0]+ems[1]*ems[1]); /* magnitude(e-s)**2 */
	if (magems<1.0e-30) {					/* check for zero length line */
		/* line is zero length, just return dist between p and s */
		*dx=fabs(pms[0]); 
/*
.....replaced dx with dy.typo found in purify.
*/
		*dy=fabs(pms[1]);
		dist=(*dx > *dy) ? *dx : *dy;		/* dist=max(*dx,*dy) */
	}
	else {										/* line not zero length */
		dot = pms[0]*ems[0]+pms[1]*ems[1];	/*(p-s)dot(e-s) */
		q[0]=s[0]+dot*ems[0]/magems;
		q[1]=s[1]+dot*ems[1]/magems;
		*dx=p[0]-q[0]; *dy=p[1]-q[1];
		*dx=fabs(*dx); *dy=fabs(*dy);
		uu_denter2(UU_GITRC,(us,"ug_ptlndist. q=%g %g, dx,dy=%g %g",
			q[0],q[1],*dx,*dy));
		uu_dexit;
		/* see if q is on the line segment (i.e. between e and s) */
 		/* if (e-q)dot(s-q)>0, q is not in middle */
 		emq[0]=e[0]-q[0]; emq[1]=e[1]-q[1];
 		smq[0]=s[0]-q[0]; smq[1]=s[1]-q[1];
 		if ((emq[0]*smq[0]+emq[1]*smq[1])>0) {
			/*  gets here if point q is not between s and e. Therefore either s 
				or e is nearest point on line segment to point p. */
			dx1=s[0]-p[0]; dy1=s[1]-p[1];
			dx1=fabs(dx1); dy1=fabs(dy1);
			dists = dx1*dx1+dy1*dy1;
			*dx=e[0]-p[0]; *dy=e[1]-p[1];
			*dx=fabs(*dx); *dy=fabs(*dy);
			diste = (*dx)*(*dx)+(*dy)*(*dy);
			if (dists<diste) {					/* point s is closest */
				*dx=dx1; *dy=dy1;
			}
		}												/* end q not between s and e */
		dist=(*dx > *dy) ? *dx : *dy;			/* dist=max(*dx,*dy) */
	}													/* end line is not zero length */
	uu_denter2(UU_GITRC,(us,
		"%g=ug_ptlndist(p=%g %g, line=%g %g, %g %g, *dx,*dy=%g %g",
		dist,p[0],p[1],s[0],s[1],e[0],e[1],*dx,*dy));
	uu_dexit;
	return(dist);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_pmka2(n,points) -- draw polymarker 2D.
**    PARAMETERS   
**       INPUT  : 
**				Gint n				number of points in polyline
**				Gwpoint points[]	array of 2D WC/NDC points
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_pmka2(n,points)		/* polymarker 2d */
Gint n;
Gwpoint points[];
{
	int i;
	Gwpoint3 p;
#ifdef UU_CHECK
	ug_chkad(points,"ug_pmka2");
#endif
	 uu_denter2(UU_GITRC,(us,"ug_pmka2"));
	for (i=0; i<n; i++) {
		p.x=points[i].x; 
		p.y=points[i].y;
		p.z=0.;
		ug_mrka3(&p);
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_pmka3(n,points) -- polymarker 3D.
**       description
**    PARAMETERS   
**       INPUT  : 
**				Gint n				number of points to 'marker'
**				Gwpoint3 points[]	array of 3D WC/NDC coords to place markers
										at
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_pmka3(n,points)			/* polymarker 3d */
Gint n;
Gwpoint3 points[];
{
	Gint i;
#ifdef UU_CHECK
	ug_chkad(points,"ug_pmka3");
#endif
	 uu_denter2(UU_GITRC,(us,"ug_pmka3"));
	for (i=0; i<n; i++) {
		ug_mrka3(&points[i]);
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_mrka3(p1) -- marker at p1.
**    PARAMETERS   
**       INPUT  : 
**				Gwpoint3 *p1	3D WC/NDC coord. point to pl;ace marker at
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_mrka3(p1)                  /* marker at p1 */
										/* not user callable */
Gwpoint3 *p1;
{
  UG_marker prms;
  int prmsiz;
  Gfloat dx,dy;
  Gnpoint3 p;
  int wantin,in;

	 uu_denter(UU_GITRC,(us,"ug_mrka3(%g,%g,%g) type=%d",(*p1).x,(*p1).y,(*p1).z,
							ug_gksstli.curprats.mkbundl.type));
	ug_xform((*p1).x,(*p1).y,(*p1).z,&p,ug_cxform[ug_gksstli.curvwindex]);
	if (ug_clpntexp(&p)==UG_TRUE) {
		prms.p.x=p.x; prms.p.y=p.y;
		prms.type=ug_gksstli.curprats.mkbundl.type;
		prms.op=UG_DPNTOP; 
		if (ug_find.find==UG_FALSE) ug_wkout(&prms,prmsiz=sizeof(prms)/sizeof(int));
		else {
			/* see if marker is within (without) rect centered at ug_find.x,y */
			dx=fabs(ug_find.x-p.x);
			dy=fabs(ug_find.y-p.y);
			if ((ug_find.pikmode==0)||(ug_find.pikmode==2)) wantin=1;
			else wantin=0;
			if ((dx<ug_find.epsx)&&(dy<ug_find.epsy)) in=1;
			else in=0;
			uu_denter2(UU_GITRC,(us,
			"ug_mrka3. find.mode=%d,find.xy=%g %g, find.eps=%g %g",
					ug_find.pikmode,ug_find.x,ug_find.y,ug_find.epsx,ug_find.epsy));
			uu_dexit;		
			if (wantin==in) {						/* found it, either in or out*/
				ug_find.found=UG_TRUE;
				ug_find.dist=(dx>dy)?dx:dy;	/* max(dx,dy) */
				ug_find.epsx=ug_find.dist;
				ug_find.epsy=ug_find.dist;
				uu_denter2(UU_GITRC,(us,"ug_mrka3. found it. dist=%g",
					ug_find.dist));
				uu_dexit;
			}

		}
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_arrayprt(array,siz,n) -- print array n numbers/line.
**    PARAMETERS   
**       INPUT  : 	int array[];	 array to print.
**							int siz;			size of array.
**							int n;			number of numbers per line to print.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_arrayprt(array,siz,n)		/* print array n numbers per line */
int array[];			/* array to print */
int siz;					/* size of a */
int n;					/* number of numbers per line */
{
#if UU_DEBUG==1
	for (j=0; j<siz; j=j+n) {		/* for each line of print */
		str[0]=0;
		for (i=0; ((i+j)<siz) && (i<n); i++) {
			len=strlen(str);
			sprintf(&str[len],"%d ",array[i+j]);
		}
		uu_denter2(UU_GITRC,(us,"%s",str));
		uu_dexit;
	}
#endif
}

/*********************************************************************
**    I_FUNCTION :  ug_updxforms(sp) -- update xforms in open seg.
**    PARAMETERS   
**       INPUT  : 	UG_gksstli *sp -- pointer to seg header.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_updxforms(sp)							/* an output primitive has just
						been inserted into the open seg. Update xforms bits */
UG_segstli *sp;
{

	if (ug_xfseg.mod==0) 				/* no modelling cmd in seg so far */
		(*sp).xforms |= UG_SEGINMODXF;
	if (ug_xfseg.ntran==0)				/* no normtran cmd in seg so far */
		(*sp).xforms |= UG_SEGINNTRAN;
	else 										/* normtran cmd in seg */
		(*sp).xforms |= 1<<ug_gksstli.curvwindex;

	uu_denter2(UU_GITRC,(us,"ug_updxforms(segid=%d) xforms=%x",
		(*sp).segid,(*sp).xforms));
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  ug_beep() -- beep the bell.
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_beep()						/* beep the bell */
{
	int prms[3];

	prms[0]=UG_DBEEP;
	ug_wkout(prms,3);		/* call workstations to beep */
}

/*********************************************************************
**    S_FUNCTION     :  ug_call(prms) -- call segment.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_call(prms)							/* UG_DCALL simulation routine */
Stcall *prms;
{
	UG_segat at;									/* segment attributes */

	uu_denter(UU_GITRC,(us,"ug_call(ws=%d, seg=%d,xform)",
		(*prms).ws,(*prms).n));

	zbytecp(at,ug_defsegat);					/* default seg atts to at */
   ug_viewsgpk((*prms).n,&at,0);				/* view the segment */
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gcallseg(n)
**      Call segment n.
**  PARAMETERS   
**      INPUT: Gseg n -- number of segment to call (instantiate)
**      OUTPUT: none
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gcallseg(n)		/* call segment n */
/*$ INPUT */
Gseg n;
{
	Gerror irtn;
	int i;
	struct {int id; Gws ws; Gseg n; } prms;

	uu_denter(UU_GTRC,(us,"gcallseg(%d)",n));

	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	if ((n<0)||(n>UG_MAXSEGNO)) {
		ug_errorhand(EBADNAME,"gcallseg"); irtn=EBADNAME;
	}
	if (ug_gksos.sysstate!=UG_SGOP) {
		ug_errorhand(ENOTSGOP,"gcallseg",&n);
		irtn=ENOTSGOP;
	}
	if (irtn==NCL_NO_ERROR) {						/* no errors so far */
#endif
		/* call workstations */
		prms.id=UG_DCALL; prms.n=n;
		ug_wkout(&prms,i=sizeof(prms)/sizeof(int));
		/* generate a call in open segment */
		ug_ncall(ug_segac(ug_gksstli.opnseg)->seglist,n);	
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}
/*********************************************************************
**    S_FUNCTION :  ug_d5polyln3(prms) -- polyline3 sim. routine which
**							software simulates all linestyles.
**    PARAMETERS   
**       INPUT  : 	
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_d5polyln3(prms)
Stpolyln3 *prms;
{
#ifdef UU_CHECK
	ug_chkad(prms,"ug_d5polyln3");
#endif
	uu_denter2(UU_GITRC,(us,"ug_d5polyln3(%d,%d, %g %g %g)",(*prms).ws,
			(*prms).n,(*prms).points[0].x,(*prms).points[0].y,
			(*prms).points[0].z));
		  
/*
.....Changing 7 to 8,if left at 7 then the dash-space line
.....type is ploted as a solid line.  JLS 8/25/99
*/
	if ((ug_find.find==UG_TRUE)||(ug_gksstli.curprats.lnbundl.type.typeno<2)
				|| (ug_gksstli.curprats.lnbundl.type.typeno>8)) 
		ug_d3polyln3(prms);
	else {						/* not finding, and 2<=linetype<=8 */
		if (ug_gksos.sysstate==UG_SGOP) {
			if (ug_segac(ug_gksstli.opnseg)->segatts.gvis==UG_VISIBLE)
				ug_p5lna3((*prms).n,(*prms).points);
		}
		else
			ug_p5lna3((*prms).n,(*prms).points);
	}								/* end not finding */
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION :  ug_d5polyln(prms) -- polyline sim. routine which
**							software simulates all linestyles.
**    PARAMETERS   
**       INPUT  : 	
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_d5polyln(prms)
Stpolyln *prms;
{
	int n,i;
	Stpolyln3 prms3;
	Gwpoint3 pts[20];
#ifdef UU_CHECK
	ug_chkad(prms,"ug_d5polyln");
#endif
	uu_denter2(UU_GITRC,(us,"ug_d5polyln(%d,%d, %g %g)",(*prms).ws,
			(*prms).n,(*prms).points[0].x,(*prms).points[0].y));
	/* just change points to 3d and call ug_d5polyln3 */
	n=prms->n;
	if (n>20) prms3.points=(Gwpoint3 *)uu_toolmalloc(n*sizeof(Gwpoint3));
	else prms3.points=pts;
	for (i=0; i<n; i++) {
		prms3.points[i].x=prms->points[i].x;
		prms3.points[i].y=prms->points[i].y;
		prms3.points[i].z=0.;
	}
	prms3.id=prms->id; prms3.ws=prms->ws;
	prms3.n=prms->n;
	ug_d5polyln3(&prms3);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_p4lna3(n,points) -- draw the n points, simulating
**													all linestyles (2<=linestyle<=8).
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_p5lna3(n,points)
int n;
Gwpoint3 *points;
{
	Gnpoint3 np1,np2;
	Gnpoint3 pos1,pos2;
	int onscr,chg2;
	int type;					/* current line type */
	int i;

	type=ug_gksstli.curprats.lnbundl.type.typeno;
	uu_denter2(UU_GITRC,(us,"ug_p5lna3(%d, %g %g %g) linetype=%d",
		n,points[0].x,points[0].y,points[0].z,type));
	ug_xform(points[0].x,points[0].y,points[0].z,&np1,
			ug_cxform[ug_gksstli.curvwindex]);
	ug_set_pattern(&ug_lntypes[type-2]);
	ug_reset_pattern();

	for (i=1; i<n; i++) { 
		ug_xform(points[i].x,points[i].y,points[i].z,&np2,
				ug_cxform[ug_gksstli.curvwindex]);
		ug_clipexpln(&np1,&np2,&pos1,&pos2,&onscr,&chg2,
			ug_gksstli.wsopen[0].wdtptr->outwdtpt->lnfac.nom);
		if (onscr!=0) {
			ug_lin5ndc(&pos1,&pos2);			/* draw ndc line */
		}												/* end if (onscr!=0) */
		np1=np2;
	}													/* end for (i=1; i<n) */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_lin5ndc(p1,p2)   -- draw line from p1 to p2,
**								simulating all linestyles.
**    PARAMETERS   
**       INPUT  : 
**				Gwpoint3 *p1,*p2	pointers to 3D NDC  'from' and 'to' points,
**							already clipped.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_lin5ndc(p1,p2)   		/* draw line from p1 to p2 (NDC coords), don't clip */
Gnpoint3 *p1,*p2;
{ int i;
	UG_line prms;
  int prmsiz;
  Gnpoint3 linein[2];				/* for ug_lnstyle */
  Glntype *p;
  int nlineout;
  Gnpoint3 *lineout;
  Glntype ntyp, otyp;

	uu_denter2(UU_GITRC,(us,"ug_lin5ndc(%g %g %g,%g %g %g)",
					(*p1).x,(*p1).y,(*p1).z,(*p2).x,(*p2).y,(*p2).z)); 
	prms.p1.x=(*p1).x; prms.p1.y=(*p1).y; 
	prms.p2.x=(*p2).x; prms.p2.y=(*p2).y; 
	prms.op=UG_DDRWOP;
	p= &ug_gksstli.curprats.lnbundl.type;
	linein[0].x=(*p1).x; linein[0].y=(*p1).y; linein[0].z=(*p1).z;
	linein[1].x=(*p2).x; linein[1].y=(*p2).y; linein[1].z=(*p2).z;
	ug_lnstyle(linein,&nlineout,&lineout);
	zbytecp(otyp,ug_gksstli.curprats.lnbundl.type);
	ntyp.typeno = 1;
	gslinetype(&ntyp);
	for (i=0; i<nlineout; i++) {	/* draw a line for each dash */
		prms.p1.x=lineout[2*i].x;
		prms.p1.y=lineout[2*i].y;
		prms.p2.x=lineout[2*i+1].x;
		prms.p2.y=lineout[2*i+1].y;
		ug_wkout(&prms,prmsiz=sizeof(prms)/sizeof(int));
		}
	uu_toolfree(lineout);
	gslinetype(&otyp);
	uu_dexit;
}

