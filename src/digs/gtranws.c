
/*********************************************************************
**    NAME         :  gtran3.c -- DIGS workstation window routines.
**       CONTAINS:
**
**		Gerror gswswindow(ws,window) -- Set workstation window 2D.
**		Gerror gswswindow3(ws,window) -- Set workstation window 3D.
**		Gerror gswsview(ws,viewport) -- Set workstation viewport 2D.
**		Gerror gswsview3(ws,viewport) -- Set workstation viewport 3D.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gtranws.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:26
*********************************************************************/
#include "zsysdep.h"
#include <stdio.h>
#include <math.h>
#include "gtbl.h"
/* #include "g.h" */
#include "gerror.h"
#include "gdidd.h"
/*#include "gdidd2.h"*/
#include "gviw.h"
#include "gmat4.h"
#include "udebug.h"
#include "gsegop.h"
#include "gsegac.h"

#define UG_TRUE 1
#define UG_FALSE 0

/********************************************************************* 
**  E_FUNCTION:  Gerror gswswindow(ws,window) -- Set workstation window 2D.
**						Z dimension set to (0.,1.).
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**					 Gnrect *window -- new workstation 2D window.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gswswindow(ws,win)		/* set workstation window */
/*$ INPUT */
Gws *ws;								/* workstation */
Gnrect *win;						/* window, NDC rectangle */
{
	Gerror irtn;
	Gos st;
	struct {Gint op; Gws wsid; Gnrect3 window;} prms;
	int reply[4];

	uu_denter(UU_GTRC,(us,"gswswindow(ws,%3g,%3g,%3g,%3g)",
			(*win).ll.x,(*win).ll.y,(*win).ur.x,(*win).ur.y));

	st=ug_gksos.sysstate;
	if (((*win).ll.x<0.)||((*win).ll.y<0.)|| 
		((*win).ur.x>1.)||((*win).ur.y>1.)) {
   	ug_errorhand(EBDWINDW,"gswswindow",NULL); irtn=EBDWINDW; 
	}
#ifdef trace
	else if ((st!=UG_GKOP)&&(st!=UG_WSOP)&&(st!=UG_WSAC)&&(st!=UG_SGOP)) {
		ug_errorhand(ENOTGWWS,"gswswindow",&ug_gksos.sysstate);
		irtn=ENOTGWWS;
	}
	else 
#endif
  {                    /* parms ok */
		prms.window.llf.x=(*win).ll.x;
		prms.window.llf.y=(*win).ll.y;
		prms.window.llf.z=0.;
		prms.window.urb.x=(*win).ur.x;
		prms.window.urb.y=(*win).ur.y;
		prms.window.urb.z=1.;
  		prms.op=UG_DWSWIND;
		prms.wsid= *ws;
  		ug_wkcal(*ws,&prms,reply);
  		ug_ndcset=UG_TRUE;            /* remember ndc space has been set */
		irtn=NCL_NO_ERROR;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gswswindow3(ws,window) -- Set workstation window 3D.
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**					 Gnrect3 *window -- new workstation 3D window.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gswswindow3(ws,win)		/* set workstation window */
/*$ INPUT */
Gws *ws;								/* workstation */
Gnrect3 *win;						/* window, NDC rectangle */
{
	Gerror irtn;
	Gos st;

	uu_denter(UU_GTRC,(us,"gswswindow3(ws,%3g,%3g,%3g,%3g,%3g,%3g)",
					(*win).llf.x,(*win).llf.y,(*win).llf.z,(*win).urb.x,
					(*win).urb.y,(*win).urb.z));

	st=ug_gksos.sysstate;
	if (((*win).llf.x<0.)||((*win).llf.y<0.)||((*win).urb.z<0.)||
			((*win).urb.x>1.)||((*win).urb.y>1.)||((*win).llf.z>1.)) {
   	ug_errorhand(EBDWINDW,"gswswindow3",NULL); irtn=EBDWINDW; 
	}
#ifdef trace
	else if ((st!=UG_GKOP)&&(st!=UG_WSOP)&&(st!=UG_WSAC)&&(st!=UG_SGOP)) {
		ug_errorhand(ENOTGWWS,"gswswindow3",&ug_gksos.sysstate);
		irtn=ENOTGWWS;
	}
	else 
#endif
  {                    /* parms ok */
	ug_wswindow3(*ws,win);
  	ug_ndcset=UG_TRUE;            /* remember ndc space has been set */
	irtn=NCL_NO_ERROR;
  }                        /* parms ok */
  uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_wswindow3(ws,win) -- set workstation window 3D.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_wswindow3(ws,win)
Gws ws;
Gnrect3 *win;
{
	struct {Gint op; Gws wsid; Gnrect3 window;} prms;
	int reply[4];
	zbytecp(prms.window,(*win));		/* structure assignment */
  	prms.op=UG_DWSWIND;
	prms.wsid= ws;
  	ug_wkcal(ws,&prms,reply);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gswsview(ws,viewport) -- Set workstation viewport 2D.
**						Z dimension set to (0.,1.).
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**					 Gdrect *viewport -- new workstation 2D viewport.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gswsview(ws,viewport)	/* set workstation viewport */
/*$ INPUT */
Gws *ws;								/* workstation */
Gdrect *viewport;					/* viewport, DC rectangle */
{
	Gerror irtn;
	Gos st;

	uu_denter(UU_GTRC,(us,"gswsview(ws,%3g,%3g,%3g,%3g)",
			(*viewport).ll.x,(*viewport).ll.y,(*viewport).ur.x,(*viewport).ur.y));

	st=ug_gksos.sysstate;
	if (((*viewport).ll.x<0.)||((*viewport).ll.y<0.)|| 
		((*viewport).ur.x>1.)||((*viewport).ur.y>1.)) {
   	ug_errorhand(EBDWINDW,"gswsview",NULL); irtn=EBDWINDW; 
	}
#ifdef trace
	else if ((st!=UG_GKOP)&&(st!=UG_WSOP)&&(st!=UG_WSAC)&&(st!=UG_SGOP)) {
		ug_errorhand(ENOTGWWS,"gswsview",&ug_gksos.sysstate);
		irtn=ENOTGWWS;
	}
	else 
#endif
  {                    /* parms ok */
		ug_wsview(*ws,viewport);
  		ug_ndcset=UG_TRUE;            /* remember ndc space has been set */
		irtn=NCL_NO_ERROR;
	}
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_dwsview(ws,viewport)
**       set workstation viewport.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_wsview(ws,viewport)
Gws ws;
Gdrect *viewport;
{
	struct {Gint op; Gws wsid; Gdrect3 viewport;} prms;
	int reply[4];
	prms.viewport.llf.x=(*viewport).ll.x;
	prms.viewport.llf.y=(*viewport).ll.y;
	prms.viewport.llf.z=0.;
	prms.viewport.urb.x=(*viewport).ur.x;
	prms.viewport.urb.y=(*viewport).ur.y;
	prms.viewport.urb.z=1.;
  	prms.op=UG_DWSVPORT;
	prms.wsid= ws;
  	ug_wkcal(ws,&prms,reply);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gswsview3(ws,viewport) -- Set workstation viewport 3D.
**						Z dimension set to (0.,1.).
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**					 Gdrect *viewport -- new workstation 3D viewport.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gswsview3(ws,viewport)	/* set workstation viewport */
/*$ INPUT */
Gws *ws;								/* workstation */
Gdrect3 *viewport;				/* viewport, DC rectangle */
{
	Gerror irtn;
	Gos st;

	uu_denter(UU_GTRC,(us,"gswsview3(ws,%3g,%3g,%3g,%3g,,%3g,%3g)",
			(*viewport).llf.x,(*viewport).llf.y,(*viewport).llf.z,
			(*viewport).urb.x,(*viewport).urb.y,(*viewport).urb.z));

	st=ug_gksos.sysstate;
	if (((*viewport).llf.x<0.)||((*viewport).llf.y<0.)||((*viewport).llf.z<0.)) {
   	ug_errorhand(EBDWINDW,"gswsview3",NULL); irtn=EBDWINDW; 
	}
#ifdef trace
	else if ((st!=UG_GKOP)&&(st!=UG_WSOP)&&(st!=UG_WSAC)&&(st!=UG_SGOP)) {
		ug_errorhand(ENOTGWWS,"gswsview",&ug_gksos.sysstate);
		irtn=ENOTGWWS;
	}
	else 
#endif
  {                    /* parms ok */
		ug_wsview3(*ws,viewport);
  		ug_ndcset=UG_TRUE;            /* remember ndc space has been set */
		irtn=NCL_NO_ERROR;
	}
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_wsview3(ws,viewport) -- set ws viewport.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_wsview3(ws,viewport)
Gws ws;
Gdrect3 *viewport;
{
	struct {Gint op; Gws wsid; Gdrect3 viewport;} prms;
	int reply[4];
	prms.viewport.llf.x=(*viewport).llf.x;
	prms.viewport.llf.y=(*viewport).llf.y;
	prms.viewport.llf.z=(*viewport).llf.z;
	prms.viewport.urb.x=(*viewport).urb.x;
	prms.viewport.urb.y=(*viewport).urb.y;
	prms.viewport.urb.z=(*viewport).urb.z;
	prms.op=UG_DWSVPORT;
	prms.wsid= ws;
  	ug_wkcal(ws,&prms,reply);
}

