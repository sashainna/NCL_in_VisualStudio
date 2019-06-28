/*********************************************************************
**    NAME         :  dgetcolr.c -- get color from user with a pallet.
**       CONTAINS:
**       ud_setpallet(posn,ncolors) -- make color pallet.
**			ud_getcolr(prmt) -- ask user to pick a color.
**			ud_colrvis(v) -- set color pallet visibility.
**			ud_setpallet2(n) -- Display n color pallet left of graphics area
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d2getcol.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:04
*********************************************************************/
#include "usysdef.h"
#include "gobas.h"
#include "go3.h"
#include "goatt.h"
#include "udebug.h"
#include "dasnog.h"
#include "uims.h"


extern UD_UIMS UD_duimsdeflt;				/* user interface */
extern UD_CURLAYOUT UD_curlayout;			/* current areas in use */

static int palletseg;			/* segment number of color pallet segment */
static int nopallet=1;			/* 1=haven't called ud_setpallet */
static Gnrect defposn={.8,.1,.9,.5};	/* default pallet posn */
static Gnrect actposn;			/* actual pallet position */
static int actcolors;			/* actual number of colors */
static Gsegvis vis;				/* whether or not palletseg is visible */

static char colorstr[16][8] = {
			"BKGRD",
			"WHITE",
			"BLACK",
			"DRKRED",
			"DRKGRN",
			"DRKBLU",
			"YELLOW",
			"CYAN",
			"MAGENTA",
			"RED",
			"GREEN",
			"BLUE",
			"ORANGE",
			"PINK",
			"LGHTGRN",
			"LGHTBLU"
	};
/*********************************************************************
**    E_FUNCTION :  int ud_setpallet(posn,ncolors) --  make color pallet.
**    PARAMETERS   
**       INPUT  : 	Gwrect *posn -- position (ndc). 
**							int ncolors -- number of colors in pallet.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_setpallet(posn,ncolors) 		/* get color from user */
Gwrect *posn;
int ncolors;								/* number of colors in pallet */
{
	Gfloat dy;
	Gfloat dyposn,dxposn;
	Gwpoint3 points[5];
	Gwpoint3 textloc;
	int i;
	Gtxalign align;

	uu_denter(UU_DTRC,(us,"ud_setpallet(%g %g %g %g,%d)",
		(*posn).ll.x,(*posn).ll.y,(*posn).ur.x,(*posn).ur.y,ncolors));
	/* save pallet position, ncolors */
	actposn.ll.x=(*posn).ll.x;
	actposn.ll.y=(*posn).ll.y;
	actposn.ur.x=(*posn).ur.x;
	actposn.ur.y=(*posn).ur.y;
	actcolors=ncolors;

	/* display vertical color pallet */
	dyposn=(*posn).ur.y-(*posn).ll.y;
	dxposn=(*posn).ur.x-(*posn).ll.x;
	dy=dyposn/ncolors;
	points[0].z=0.; points[1].z=0.; points[2].z=0.; points[3].z=0.;
	points[4].z=0.;
	points[0].x=(*posn).ll.x;
	points[1].x=(*posn).ur.x;
	points[2].x=points[1].x;
	points[3].x=points[0].x;
	points[4].x=points[0].x;
	if (nopallet!=1) gdeleteseg(palletseg);		/* delete old palletseg*/
	palletseg=gnseg();
	align.hor=UG_TH_CENTRE;
	align.ver=UG_TV_HALF;
	gcreateseg(palletseg);
	gsnormtran(0);
	gstextalign(&align);
	gscharheight((UU_REAL) .01);
	textloc.x = points[0].x + ((points[1].x-points[0].x)/2);
	textloc.z = 0.0;
	/* draw color pallet */
	for (i=0; i<ncolors; i++) {
		gspickid(i+1);
		points[0].y=(*posn).ll.y+i*dy;
		points[1].y=points[0].y;
		points[2].y=points[0].y+dy;
		points[3].y=points[2].y;
		points[4].y=points[0].y;
		gsfillcolor(i);
		gfillarea3(4,points);
		gslinecolor(1);			/* white borders */
		gpolyline3(5,points);
		gstextcolor(0);
		textloc.y = points[0].y + (points[2].y-points[0].y)/2.0;
		gtext(&textloc,colorstr[i]);
	}
	gcloseseg();
	nopallet=0;							/* remember we have a pallet */
	vis=UG_VISIBLE;						/* remember pallet is visible */
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  int ud_getcolr(prmt) -- get color from user.
**       Ask user to pick a color from pallet. Return its index.
**    PARAMETERS   
**       INPUT  : 	char *prmt -- user prompt.
**       OUTPUT : 	none
**    RETURNS      : color index (>=0).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ud_getcolr(prmt)
char *prmt;							/* user prompt */
{
	int numint;
	int colr,found;
	Gfloat dyposn;
	UD_NDCLOCREC rec;
	Gnpoint3 p;
	uu_denter(UU_DTRC,(us,"ud_getcolr(%s). palletseg=%d",prmt,palletseg));
	if(nopallet) ud_setpallet2(8);
	if (vis==UG_INVISIBLE) {
		gssegvis(palletseg,UG_VISIBLE);
		vis=UG_VISIBLE;
	}
	found=0;
	while (1) {
		ud_ddas(UD_DASNDC,prmt,&rec,1, &numint, UD_NODEFAULT);
		p.x = rec.cord[0]; p.y = rec.cord[1]; p.z = rec.cord[2];
		uu_denter2(UU_DTRC,(us,"ud_getcolr ddas x,y=%g %g",p.x,p.y));
		uu_dexit;
		/* see if within pallet area */
		if ((p.x>=actposn.ll.x)&&(p.x<=actposn.ur.x)&&
			(p.y>=actposn.ll.y)&&(p.y<=actposn.ur.y)) {	/* is within pallet */
				dyposn=(actposn).ur.y-(actposn).ll.y;
				colr=actcolors*(p.y-actposn.ll.y)/dyposn;
				if (colr>=actcolors) colr=actcolors-1;
				break;
			}
	}
	uu_denter2(UU_DTRC,(us,"ud_getcolr() returns %d",colr));
	uu_dexit;
	uu_dexit;
	return(colr);
}

/*********************************************************************
**    E_FUNCTION :  ud_colrvis(v) -- set visibility of palletseg.
**    PARAMETERS   
**       INPUT  : 	Gsegvis v -- UG_VISIBLE or UG_INVISIBLE.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ud_colrvis(v)						/* set visibility of palletseg */
Gsegvis v;
{
	uu_denter(UU_DTRC,(us,"ud_colrvis(%d), palletseg=%d",v,palletseg));
	if(nopallet != 1)
	  if (palletseg!= -1) {
		if (v!=vis) gssegvis(palletseg,v);
	}
	vis=v;								/* remember this visibility */
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ud_setpallet2() -- Displays pallet just to the
**						left of graphics area 0.
**    PARAMETERS   
**       INPUT  :  ncolors - Number of colors in pallet. Usually 8 if
**					choosing colors for icons, 16 if chosing graphics colors.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ud_setpallet2(ncolors)
int ncolors;
{
	Gwrect posn;
	int scrn;
	Gfloat delta;

	uu_denter(UU_DTRC,(us,"ud_setpallet(%d)",ncolors));
	scrn = UD_curlayout.curr_screen;
	posn.ll.x= UD_duimsdeflt.screen[scrn].areas[UD_GRAF][0].posn.ll.x-.1;
	posn.ll.y= UD_duimsdeflt.screen[scrn].areas[UD_GRAF][0].posn.ll.y;
	delta=(UD_duimsdeflt.screen[scrn].areas[UD_GRAF][0].posn.ur.y-posn.ll.y)/16;
	posn.ur.x= UD_duimsdeflt.screen[scrn].areas[UD_GRAF][0].posn.ll.x-.01;
	posn.ur.y= posn.ll.y + ncolors*delta;
	if((ncolors != actcolors) || ( posn.ll.x != actposn.ll.x) || 
				(posn.ur.y != actposn.ur.y))
		ud_setpallet(&posn,ncolors);
	uu_dexit;
}
