/*********************************************************************
**    NAME         :  d1rect.c -- generic u.i. run time support
**       CONTAINS:
**				ud_halfrect
**				ud_get5pts
**				ud_devrect
**				ud_rectequal
**				ud_wsxform
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d1rect.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:03
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "usysg.h"
#include "g.h"
#include "unserve.h"
#include "udebug.h"

#include "uims.h"

#ifdef UU_DEBUGON
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) d1rect.c 3.3 7/20/88 15:27:09 single"};
#else
static char uu_sccsident[]={"@(#) d1rect.c 3.3 7/20/88 15:27:09 double"};
#endif
#endif

#define ON 1
#define OFF 0

char *uu_getenv();

static char typstr[UD_NTYPES][10]={"UD_GRAF","UD_SPRMT","UD_LPRMT",
		"UD_ERR","UD_HLP","UD_CHC","UD_ICON","UD_SCROLL","UD_FORMS",
		"UD_MENU","UD_ICONM"};			/* for debug printout */

/*********************************************************************
**    E_FUNCTION :  ud_get5pts(rect,corners) -- gen corners of rect.
**       description
**    PARAMETERS   
**       INPUT  :  Gnrect *rect -- rectangle.
**       OUTPUT :  Gnpoint corners[5] -- 4 corners of the rectangle.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_get5pts(rect,corners)
Gnrect *rect;
Gnpoint corners[5];
{
	corners[0].x=(*rect).ll.x;
	corners[0].y=(*rect).ll.y;
	corners[2].x=(*rect).ur.x;
	corners[2].y=(*rect).ur.y;
	corners[1].x=corners[2].x;
	corners[1].y=corners[0].y;
	corners[3].x=corners[0].x;
	corners[3].y=corners[2].y;
	corners[4].x=corners[0].x;
	corners[4].y=corners[0].y;
}

/*********************************************************************
**    I_FUNCTION :  ud_halfrect(halfrect,rect) -- upper 3/4 of rectangle.
**       calculate halfrect = upper 3/4 of rect. Used to be upper 1/2.
**			Also returns left edge .01 to right of rect.
**    PARAMETERS   
**       INPUT  : 	Gnrect *rect -- rectangle.
**       OUTPUT :  	Gnrect *halfrect -- upper half of rect.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_halfrect(halfrect,rect)
Gnrect *halfrect,*rect;
{
	uu_denter(UU_DTRC,(us,"ud_halfrect(half,%g %g, %g %g)",
			(*rect).ll.x,(*rect).ll.y,(*rect).ur.x,(*rect).ur.y));
	(*halfrect).ll.x=(*rect).ll.x+.005;
	(*halfrect).ll.y=(3*(*rect).ll.y+(*rect).ur.y)/4.;
	(*halfrect).ur.x=(*rect).ur.x;
	(*halfrect).ur.y=(*rect).ur.y;
	uu_dprint(UU_DTRC,(us,"ud_halfrect returns(%g %g, %g %g)",
		(*halfrect).ll.x,(*halfrect).ll.y,(*halfrect).ur.x,(*halfrect).ur.y));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ud_devrect(posn,pos) -- convert posn to DC.
**    PARAMETERS   
**       INPUT  :  Gnrect *posn -- ndc rectangle.
**       OUTPUT :  Gnrect *pos -- DC rectangle.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_devrect(posn,pos)
Gnrect *posn;				/* ndc rectangle */
Gdrect *pos;				/* DC rect */
{
	int scrn;
	struct {Gfloat scale; Gfloat dx,dy;} wsxform;	/* ws xform */
	uu_denter(UU_DTRC,(us,"ud_devrect(%g %g, %g %g,pos)",
	(*posn).ll.x,(*posn).ll.y,(*posn).ur.x,(*posn).ur.y));
	scrn=UD_curlayout.curr_screen;
	ud_wsxform(&UD_duimsdeflt.screen[scrn].wswind,&wsxform);
	(*pos).ll.x=(*posn).ll.x*wsxform.scale+wsxform.dx;
	(*pos).ll.y=(*posn).ll.y*wsxform.scale+wsxform.dy;
	(*pos).ur.x=(*posn).ur.x*wsxform.scale+wsxform.dx;
	(*pos).ur.y=(*posn).ur.y*wsxform.scale+wsxform.dy;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ud_wsxform(wp,wsxform)
**       Calculate workstation transformation.
**    PARAMETERS   
**       INPUT  : 	Gnrect *wp -- workstation window.
**       OUTPUT :   struct {Gfloat scale; Gfloat dx,dy} *wsxform -- ws xform.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ud_wsxform(wp,wsxform)	
										/* calculate wsxform =scale and dx,dy to 
											perform the workstation transformation
											from window to vport */
Gnrect *wp;						/* workstation window */
struct {Gfloat scale; Gfloat dx,dy;} *wsxform;
{
	char us[180];
	Gdspsize  *siz;
	Gfloat sx,sy,s;
	int ll[2],ur[2];
	Gfloat wdx,wdy;
	wdx=(*wp).ur.x-(*wp).ll.x;		/* ws window width */
	wdy=(*wp).ur.y-(*wp).ll.y;		/* ws window height */
	/* use whole screen as ws viewport */
	siz=gqdisplaysize(UD_ksws);
	sx=(*siz).device.x/wdx;
	sy=(*siz).device.y/wdy;
	s=(sx<sy) ? sx : sy;			/* use smaller of scale factors */
	/* s now scales window to device coords */
	(*wsxform).scale=s;			/* save scale factor */
	(*wsxform).dx= -s*(*wp).ll.x;
	(*wsxform).dy= -s*(*wp).ll.y;
	uu_denter2(UU_DTRC,(us,"ud_wsxform(wind=%g %g, %g %g, s=%g, dx,dy=%g %g)",
		(*wp).ll.x,(*wp).ll.y,(*wp).ur.x,(*wp).ur.y,
		(*wsxform).scale, (*wsxform).dx,(*wsxform).dy));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  int ud_rectequal(r1,r2) -- compare 2 rectangles
**    PARAMETERS   
**       INPUT  :  Gnrect *r1,*r2 -- rectangles to compare
**       OUTPUT :    none
**    RETURNS      : 1 if r1==r2, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#define EPS (UU_REAL) .002
int ud_rectequal(r1,r2)
Gnrect *r1,*r2;
{
	int irtn;
	char us[180];

	if ((fabs((*r1).ll.x-(*r2).ll.x)<EPS)&&
			(fabs((*r1).ll.y-(*r2).ll.y)<EPS)&&
			(fabs((*r1).ur.x-(*r2).ur.x)<EPS)&&
			(fabs((*r1).ur.y-(*r2).ur.y)<EPS)) irtn=1;
	else irtn=0;
	uu_denter2(UU_DTRC,(us,"%d=ud_rectequal(%g %g %g %g, %g %g %g %g)",
		irtn,(*r1).ll.x,(*r1).ll.y,(*r1).ur.x,(*r1).ur.y,
		(*r2).ll.x,(*r2).ll.y,(*r2).ur.x,(*r2).ur.y));
	uu_dexit;
	return(irtn);
}
