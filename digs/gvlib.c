/*********************************************************************
**    NAME         :  GKS archive routines .
**
**    CONTAINS:
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       gvlib.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:27
*********************************************************************/
#include "zsysdep.h"
#include <stdio.h>
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "ginq.h"
#include "gvlib.h"
#include "gviw.h"
#include "gsegop.h"
#include "umath.h"
#include "udebug.h"
#include "gconvert.h"

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gvlib.c 2.4 5/2/86 13:55:21 single"};
#else
static char uu_sccsident[]={"@(#) gvlib.c 2.4 5/2/86 13:55:21 double"};
#endif
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
#define x1 (Gfloat) cmd->pts[0].x
#define y1 (Gfloat) cmd->pts[0].y
#define x2 (Gfloat) cmd->pts[1].x
#define y2 (Gfloat) cmd->pts[1].y
#else
#define x1 cmd->pts[0].x
#define y1 cmd->pts[0].y
#define x2 cmd->pts[1].x
#define y2 cmd->pts[1].y
#endif
/*********************************************************************
**    NAME         :  gksvlib.c -- library of output primitives.
**       CONTAINS:
**		ug_viwlib(cmd)
**		grect(xs,ys,xe,ye) -- rectangle 2D
**		gcirc(xc,yc,radius) -- circle 2D 
**		garrow(xs,ys,xe,ye) -- draw a 2D arrow.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       gvlib.c , 15.2
**    DATE AND TIME OF LAST  MODIFICATION
**       6/27/0 , 15:23:13
*********************************************************************/

void grect();
void gcirc();
void garrow();

/*********************************************************************
**    N_FUNCTION     :  ug_viwlib(cmd)
**       Access routine for all library primitives.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_viwlib(cmd)
UG_plylna2op *cmd;
{
  int i,j;
  int opcode;
	uu_denter(UU_GITRC,(us,"ug_viwlib(cmd). opcode=%d",opcode));
  opcode=cmd->elttype;
  switch (opcode) {
  case GRECT2OP: grect(x1,y1,x2,y2); break;   /* rectangle */
  case GCIRCLE2OP:                     /* circle */
       gcirc(x1,y1,(Gfloat)cmd->pts[1].x);
       break;
  case GARROW2OP: garrow(x1,y1,x2,y2); break;
  default: /*fprintf(stderr,"ug_viwlib illegal opcode=%d\n",opcode);
 */      break;
  }                                    /* switch */
	uu_dexit;
}

/********************************************************************* 
**  N_FUNCTION:  grect(xs,ys,xe,ye) -- rectangle 2D
**      GKS library graphics procedure to draw a 2D rectangle.
**  PARAMETERS   
**      INPUT:  Gfloat xs,ys -- lower left corner of rectangle
**					 Gfloat xe,ye -- upper right corner of rectangle
**      OUTPUT: none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void grect(xs,ys,xe,ye)
/*$ INPUT */
Gfloat xs,ys,xe,ye;
{
	int i;
	Gwpoint3 p[5];
	uu_denter(UU_GTRC,(us,"grect(%g,%g,%g,%g)",xs,ys,xe,ye));
	for (i=0; i<5; i++) p[i].z=0.0;
	p[0].x=xs; p[0].y=ys; 
	p[1].x=xe; p[1].y=ys; p[2].x=xe; p[2].y=ye;
	p[3].x=xs; p[3].y=ye;
	p[4].x=xs; p[4].y=ys;
	for (i=1; i<5; i++) ug_lina3(&p[i-1],&p[i]);
	uu_dexit;
}

/********************************************************************* 
**  N_FUNCTION:  gcirc(xc,yc,radius) -- circle 2D 
**      GKS viewing library routine to draw a 2D circle.
**  PARAMETERS   
**      INPUT:  Gfloat xc,yc -- center of circle.
**					 Gfloat radius -- radius of circle.
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void gcirc(xc,yc,radius)
/*$ INPUT */
Gfloat xc,yc,radius;
{
  Gfloat x,y,theta,dt;
  Gwpoint3 p,pold;
  uu_denter(UU_GTRC,(us,"gcirc(%g,%g,%g)",xc,yc,radius));
  dt=6.2832/30.;
  p.z=0.0;
  pold.x=xc+radius; pold.y=yc; pold.z=0.0;
  for (theta=dt; theta<=6.2833; theta=theta+dt) {
    p.x=xc+radius*cos(theta); p.y=yc+radius*sin(theta);
    ug_lina3(&pold,&p);
	 zbytecp(pold,p);							/* structure assignment */
  }
  uu_dexit;
}

/********************************************************************* 
**  N_FUNCTION:  garrow(xs,ys,xe,ye) -- draw a 2D arrow.
**      GKS viewing library routine to draw a 2D arrow.
**  PARAMETERS   
**      INPUT:  Gfloat xs,ys -- tail of arrow.
**					 Gfloat xe,ye -- head of arrow.
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void garrow(xs,ys,xe,ye)
/*$ INPUT */
Gfloat xs,ys,xe,ye;
{
  Gfloat len;
  Gfloat dx,dy;
  Gfloat cosa,sina;
  Gfloat xr,yr,xc,yc;
  Gwpoint3 pold,p;
  uu_denter(UU_GTRC,(us,"garrow(%g,%g,%g,%g)",xs,ys,xe,ye));
  pold.x=xs; pold.y=ys; pold.z=0.0;
  p.x=xe; p.y=ye; p.z=0.0;
  ug_lina3(&pold,&p);  /* draw the line */
  zbytecp(pold,p);						/* structure assignment */
  dx=xe-xs; dy=ye-ys;
  len=sqrt(dx*dx+dy*dy);
  cosa=dx/len; sina=dy/len;  /* sin and cos of slope angle*/
  xc=.9*xe+.1*xs; yc=.9*ye+.1*ys; /* point on line at end of head*/
  xr=xc+.05*len*sina; yr=yc-.05*len*cosa;
  p.x=xr; p.y=yr;
  ug_lina3(&pold,&p);        /* draw one slanted side */
  zbytecp(pold,p);						/* structure assignment */
  p.x=xc-.05*len*sina; p.y=yc+.05*len*cosa;
	ug_lina3(&pold,&p);        /* draw back side */
	zbytecp(pold,p);						/* structure assignment */
	p.x=xe; p.y=ye;
  ug_lina3(&pold,&p);        /* draw other slanted side */
  uu_dexit;
}
