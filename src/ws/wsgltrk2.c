#include "usysdef.h"
#ifdef UU_OPENGL

/*********************************************************************
**    NAME         :  wsgltrk2.c
**      	GKS workstation pick, locate, track, etc. type routines.
**
**    CONTAINS:
**			uw_glpik
**			uw_picking_rpseg
**			uw_clear_depth
**
**    MODULE NAME AND RELEASE LEVEL 
**			wsgltrk2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:09
*********************************************************************/

#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:keysym.h>
#include <decw$include:cursorfont.h>
#else
#if UU_COMP != UU_WIN2K 
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>
#endif
#endif

#include "wsgl.h"
#include "udebug.h"
#include "zsysdep.h"
#include "dasnog.h"
#include "uims.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "ginqatti.h"
#include "gsegac.h"
#include "class.h"
/*
.....we do need this file
.....Yurong 8/26/97
*/
#include "nclicons.h"
#if UU_COMP != UU_WIN2K 
#include "wsxw.h"
#else
#include "wsntglfunc.h"
#endif
#include "mpocket.h"
#include "lcom.h"
#include "wsglfun.h"

#define PICKTRIGLEN 6
extern int ug_picktriglen;
extern struct { 
	int dev; 
	int min,max;
} ug_piktrig[PICKTRIGLEN];

void uw_clear_depth();

extern int UG_cursor_window;

/*********************************************************************
**    I_FUNCTION     : uw_glpik(wid,path,depth,xy,k,cursorno) 
**
**    PARAMETERS
**       INPUT  :
**          wid,seg,depth,xy,cursorno
**       OUTPUT :
**          xy,depth,k
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_glpik(wid,path,depth,xy,k,cursorno)
int path[];             /* pick path, seg pickid, seg pickid.... */
int *depth;             /* return length of path here */
int xy[2];              /* on call, initial xy position of pick.
                           on return x,y coordinates of pick */
int *k;                 /* key that ended picking */
int cursorno;           /* number of the cursor to use: */
Gws wid;
{
	int i, done;
	int aper[2];         /* Cursor aperture */

	UG_cursor_window = 1;
	done = uw_gltrk(wid,xy,k,cursorno,0,0,NULL,NULL); 
	for (i=0; i<ug_picktriglen; i++) 
	{
		if ((done==ug_piktrig[i].dev)&&(*k>=ug_piktrig[i].min)
			&&(*k<=ug_piktrig[i].max))
			break;
	}
	if (i<ug_picktriglen)
	{
		uw_glpushx();
		aper[0] = aper[1] = 16;      
		uw_glpik2(path, depth, xy, aper);   /* Pick on all segments */
		uw_glpopx();     
		uw_glreset_xform();
	}
	else
		*depth=0;
/*
.....This code causes Pick Verify mode
.....to get a fatal error when called
.....from a form
.....Bobby  -  10/11/99
*/
/*	if (*depth <= 0)
	{
		path[0]= -1;
		path[1]=0;
	}*/
	UG_cursor_window = 0;
	return(done);
}


/*********************************************************************
**    I_FUNCTION     : uw_picking_rpseg(loc,path,depth,eps,xform)
**			Picking rotine for Record/Playback
**    PARAMETERS
**       INPUT  :
**          loc: picking location
**				xform: viewport number
**       OUTPUT :
**				path: return picking array
**				depth: segments depth
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_picking_rpseg(loc,path,depth,eps,xform)
Gnpoint *loc;
int path[];
int *depth;
Gfloat eps;
Gint xform;	
{
	int aper[2];  
	int xy[2];
/*
.....change NDC to DEVC
*/
	Gfloat ndcp[2];
	ndcp[0] = loc->x;
	ndcp[1] = loc->y;
	uw_glndctodev(ndcp, xy);

	uw_glpushx();
	aper[0] = aper[1] = 16;      
	uw_glpik2(path, depth, xy, aper); 
	uw_glpopx();     
	uw_glreset_xform();
}

/*********************************************************************
**    E_FUNCTION     : uw_clear_depth(llx,lly,urx,ury)
**					Clear depth in llx,lly,urx,ury
**
**    PARAMETERS
**       INPUT  :
**          llx,lly,urx,ury
**       OUTPUT :
**         None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_clear_depth(llx,lly,urx,ury)
int llx,lly,urx,ury;
{
	int clip[4], clip_sav[4];
	uw_glget_scissor(clip_sav);
	clip[0] = llx;
	clip[1] = lly;
	clip[2] = urx;
	clip[3] = ury;
	uw_glset_scissor(clip);
	glClearDepth_d((GLclampd)1.0);
	glClear_d(GL_DEPTH_BUFFER_BIT);
	uw_glset_scissor(clip_sav);
}
#endif
