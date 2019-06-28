#include "usysdef.h"
#ifdef UU_OPENGL

/*********************************************************************
**  NAME:  wsglout2.c
**
**      GKS workstation: output functions section.
**
**    CONTAINS:
**          uw_glset_dirty_flag
**          uw_glmark_dirty_seg
**          uw_glmark_dirty_rect
**          uw_glmark_dirty_dpt
**          uw_glmark_dirty_pt2
**          uw_glmark_dirty_pt3
**          uw_glmark_dirty_screen
**          uw_glredrawws
**          uw_glclearws
**				uw_glclearvp
**          uw_glupd
**          uw_glupdrec
**          uw_glupdate_front
**          uw_glvp
**          uw_glset_scissor
**          uw_glget_scissor
**          uw_glgetbuffer
**          uw_gldrawbuffer
**          uw_glswapbuffer
**				uw_glredraw_all
**				uw_glresize_graphics
**				uw_glget_context
**				uw_glset_context
**				uw_glflush
**
**    MODULE NAME AND RELEASE LEVEL 
**			wsglout2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:07
**    
*********************************************************************/

#if UU_COMP != UU_WIN2K 
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <Xm/Xm.h>
#endif
#include <math.h>

#include "udebug.h"
#include "zsysdep.h"
#include "driver.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "gsegac.h"
#include "mdattr.h"
#if UU_COMP != UU_WIN2K 
#include "wsxw.h"
#include "wsmf.h"
#endif
#include "ginqatti.h"
#include "gmat4.h"
#include "go1.h"
#include "mpocket.h"
#include "view.h"
#include "uims.h"
#include "view1.h"
#include "wsgl.h"
#include "nccs.h"
#include "nclfc.h"
#include "lcom.h"
#include "wsglfun.h"
#include "nclmodals.h"
#if UU_COMP == UU_WIN2K 
#include "wsntglfunc.h"
#endif

extern UG_wdt glwdt;
extern int NCL_mot_seg;
extern int UW_signon;
extern int *UD_ksws;
extern int UV_dynview_active;
extern int UW_print_screen, UW_print_back;

static UU_LOGICAL Supdate_dirty=UU_TRUE;
static Gnrect Sdirty_box = {10000.,10000., -10000.,-10000.};

UM_pkwin_type uw_glget_context();
void uw_glmark_dirty_rect();
void uw_glmark_dirty_pt3();
void uw_glget_scissor();
void uw_glset_scissor();
void uw_glredraw_all();
void uw_gldrawbuffer();
void uw_glswapbuffer();
void uw_glresize_graphics();
void uw_glset_context();
void uw_glflush();
void uw_glupdrec();
void uw_glupdate_front();
void uw_glclear_rect();
static void S_draw_background();

static void S_draw_segment();

/*********************************************************************
**    E_FUNCTION     :  uw_glget_dirty_flag()
**       Returns the flag to determine if the dirty area should be updated.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : flag   = UU_TRUE = Update dirty area, UU_FALSE = Don't.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL uw_glget_dirty_flag(flag)
{
	return(Supdate_dirty);
}

/*********************************************************************
**    E_FUNCTION     :  uw_glset_dirty_flag(flag)
**       Sets the flag to determine if the dirty area should be updated.
**    PARAMETERS   
**       INPUT  : 
**          flag   = UU_TRUE = Update dirty area, UU_FALSE = Don't.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glset_dirty_flag(flag)
UU_LOGICAL flag;
{
	Supdate_dirty = flag;
}

/*********************************************************************
**    E_FUNCTION     :  uw_glmark_dirty_seg(seg)
**       Adjusts the back buffer dirty area for the NDC box of a segment.
**    PARAMETERS   
**       INPUT  : 
**          seg    = Segment to use to mark dirty area.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmark_dirty_seg(seg)
int seg;
{
	UG_segstli *segptr;
	Gnrect rect;
/*
.....Get the segment's NDC box
*/
	segptr = ug_segac(seg);
	if (segptr != UU_NULL)
	{
		if (segptr->wcboxok != 1) ug_segwcbox(seg);
		if (segptr->wcboxok == 1)
		{
			ug_segndcbox(segptr->segid,&rect);
/*
........Merge the box with the dirty area box
*/
			uw_glmark_dirty_rect(&rect);
		}
/*
.....Cannot calculate segment's NDC box
.....Mark whole screen as dirty
*/
		else
		{
			rect.ll.x = rect.ll.y = 0.;
			rect.ur.x = rect.ur.y = 1.;
			uw_glmark_dirty_rect(&rect);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  uw_glmark_dirty_rect(rect)
**       Adusts the back buffer dirty area for the input NDC box.
**    PARAMETERS   
**       INPUT  : 
**          rect   = NDC box to use to mark dirty area.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmark_dirty_rect(rect)
Gnrect *rect;
{
/*
.....Merge the box with the dirty area box
*/
	if (Supdate_dirty)
	{
		ug_adjustndcbox(&Sdirty_box,rect);
	}
}

/*********************************************************************
**    E_FUNCTION     :  uw_glmark_dirty_dpt(pt)
**       Adusts the back buffer dirty area for the input DC point.
**    PARAMETERS   
**       INPUT  : 
**          rect   = NDC box to use to mark dirty area.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmark_dirty_dpt(pt)
Gdpoint *pt;
{
	int xy[2];
	Gnrect rect;
/*
.....Convert DC to NDC
*/
	xy[0] = pt->x; xy[1] = pt->y;
	uw_gldevtondc(xy,(Gfloat *)&(rect.ll));
	rect.ur.x = rect.ll.x; rect.ur.y = rect.ll.y;
/*
.....Merge the point with the dirty area box
*/
	uw_glmark_dirty_rect(&rect);
}

/*********************************************************************
**    E_FUNCTION     :  uw_glmark_dirty_pt2(pt)
**       Adusts the back buffer dirty area for the input 2-D WC point.
**    PARAMETERS   
**       INPUT  : 
**          rect   = NDC box to use to mark dirty area.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmark_dirty_pt2(pt)
Gwpoint *pt;
{
	Gwpoint3 pt3;
/*
.....Convert 2-D point to 3-D point
*/
	pt3.x = pt->x; pt3.y = pt->y; pt3.z = 0.;
	uw_glmark_dirty_pt3(&pt3);
}

/*********************************************************************
**    E_FUNCTION     :  uw_glmark_dirty_pt3(pt)
**       Adusts the back buffer dirty area for the input WC point.
**    PARAMETERS   
**       INPUT  : 
**          rect   = NDC box to use to mark dirty area.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmark_dirty_pt3(pt)
Gwpoint3 *pt;
{
	UU_REAL z;
	Gnrect rect;
/*
.....Convert 3-D point to NDC
*/
	gwndc3(&rect.ll.x,&rect.ll.y,&z,pt->x,pt->y,pt->z);
	rect.ur.x = rect.ll.x; rect.ur.y = rect.ll.y;
/*
.....Merge the point with the dirty area box
*/
	uw_glmark_dirty_rect(&rect);
}

/*********************************************************************
**    E_FUNCTION     :  uw_glmark_dirty_screen()
**       Marks the entire graphics window as dirty.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmark_dirty_screen()
{
/*
.....Make dirty area entire screen
*/
	Sdirty_box.ll.x = Sdirty_box.ll.y = 0.;
	Sdirty_box.ur.x = uw_gl.xndc;
	Sdirty_box.ur.y = uw_gl.yndc;
}

/*********************************************************************
**    E_FUNCTION     :  uw_glredrawws()
**       Redraws all visible segments.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Redraws the screen.
**    WARNINGS     : Before, The redraw is double buffered for the first
**                   UU_DBSIZE segments.  This there are less
**                   than two hundred segments.  Larger numbers are
**                   redrawn in both buffers simultaneously for speed.
**                   Now, only redraw in double buffering if it is
*********************************************************************/
void uw_glredrawws()
{
	int j;
	Gnrect rect;
/*
.....Don't update workstation if Dynamic viewing is active
.....The graphics will be corrupted otherwise
*/
	if (UV_dynview_active) return;
/*
.....Set the appropriate graphics modes
*/
	uw_glset_context(UM_NCL_WINDOW,UU_TRUE);
	uw_gldrawbuffer(UG_BACK_BUFFER);
		
	for (j=0;j<UV_act_screen[0].nvports;j++)	
		ncl_reset_cutseg(j);
/*
.....we need draw everything in the back buffer without any cutters
.....because the draw cutter routine will use back buffer as a good source
.....to moving cutter (ncl_display_motion, but it actually there is wrong 
.....graphic in the backbuffer at this time, so we draw every without cutter
.....first, swap cutter and draw cutter last. The cutter may only display in the front
.....buffer, but that's OK.
*/
	uw_glredraw_all(0, 0);
/*
.....then draw the cutter
*/
	rect.ll.x = rect.ll.y = 0.;
	rect.ur.x = uw_gl.xndc; rect.ur.y = uw_gl.yndc;
	ncl_display_motion(-1,0,0,0,UU_TRUE,UU_TRUE,&rect);
/*
.....Reset graphics modes
*/
	ug_setredrwflag(0);
/*
.....Display back buffer
*/
	ud_updatews(UG_PERFORM);
}

/*********************************************************************
**    E_FUNCTION     :  uw_glclearws()
**       Clears workstation.
**    PARAMETERS   
**       INPUT  : 
**                                      none
**       OUTPUT :  
**                                      none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glclearws()
{
/*
.....clear all
*/
	S_draw_background();
	uw_glmark_dirty_screen();
}

/*********************************************************************
**    E_FUNCTION : uw_glclearvp(n)
**       Clears a single viewport.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glclearvp(n)
int n;
{
	int ll[2],ur[2];
/*
.....Calculate viewport extents
*/
    uw_glndctodev(&ug_gksstli.vtran[n].vport.llf,ll);
    uw_glndctodev(&ug_gksstli.vtran[n].vport.urb,ur);
/*
.....Clear the viewport
*/
	uw_glclear_rect(ll,ur);
}

/*********************************************************************
**    E_FUNCTION : uw_glclear_rect(ll,ur)
**       Clears a single viewport.
**    PARAMETERS
**       INPUT  :
**          ll    = Lower left of rectangle to clear in device coords.
**          ur    = Upper right of rectangle to clear in device coords.
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glclear_rect(ll,ur)
int ll[],ur[];
{
	int clip[4], sav_clip[4];
	Gnrect rect;
/*
.....Set clipping box
*/
	clip[0] = ll[0];
	clip[1] = ll[1];
	clip[2] = ur[0] - ll[0] + 1;
	clip[3] = ur[1] -ll[1] + 1;
	uw_glget_scissor(sav_clip);
	uw_glset_scissor(clip);
/*
.....Clear the viewport
*/
	S_draw_background();
	uw_gldevtondc(ll,(Gfloat *)&(rect.ll));
	uw_gldevtondc(ur,(Gfloat *)&(rect.ur));
	uw_glmark_dirty_rect(&rect);
/*
.....Reset scissor box
*/
	uw_glset_scissor(sav_clip);
}

/*********************************************************************
**    E_FUNCTION     :  uw_glupd(prms)
**       Update workstation and copy the dirty part of the back buffer
**       to the front buffer.
**    PARAMETERS   
**       INPUT  : 
**          prms[2]   - UG_PERFORM = Copy entire buffer.
**       OUTPUT :  
**                              none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glupd(prms)
int prms[];
{
	Gnrect segrect;
/*
.....Redraw graphics first
*/
	if (ug_getredrwflag() == 1)
	{
		if (prms[2] == (int)UG_PERFORM)
		{
			segrect.ll.x = segrect.ll.y = 0.;
			segrect.ur.x = uw_gl.xndc;
			segrect.ur.y = uw_gl.yndc;
		}
		else
			ug_getredrwrect2(&segrect);
		uw_glupdrec(&segrect);
	}
/*
.....Just update the front buffer
*/
	else
		uw_glupdate_front(prms[2]);
}

/*********************************************************************
**    E_FUNCTION     : uw_glupdrec(segrect) 
**       Redraws all segments on workstation within segrect
**       Actually redraw the viewport that include segrect
**    PARAMETERS  
**       INPUT  :
**          segrect   = Rectangle to redraw.
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS :
**          The front buffer will be updated after updating the graphics.
**    WARNINGS     : none
*********************************************************************/
void uw_glupdrec(segrect)
Gnrect* segrect;
{
	Gnrect rect,rect1,rect2;
	UM_pkwin_type cx;
	int stat;
/*
.....Don't update workstation if Dynamic viewing is active
.....The graphics will be corrupted otherwise
*/
	if (UV_dynview_active) return;
/*
.....If rectangle is not valid
.....Then don't do anything
*/
	if (segrect->ll.x > segrect->ur.x) return;
/*
.....Merge rectangle with redraw rectangle
*/
	rect1 = *segrect;
	if (ug_getredrwflag() == 1)
	{
		ug_getredrwrect2(&rect1);
		ug_adjustndcbox(&rect1,segrect);
	}
/*
.....Clip rectangle to screen
*/
	rect2.ll.x = rect2.ll.y = 0.;
	rect2.ur.x = uw_gl.xndc;
	rect2.ur.y = uw_gl.yndc;
	stat = ug_drectintersect2(&rect,&rect1,&rect2);
/*
.....Reset pocket graphics if active
*/
	if (stat == 0)
	{
		cx = uw_glget_context();
		uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
/*
.....Set redraw buffer
*/
		uw_gldrawbuffer(UG_BACK_BUFFER);       
/*
.....Redraw the graphic segments
*/
		uw_glredraw_all(1,&rect);
/*
.....Then draw the cutter
*/
		ncl_display_motion(-1,0,0,0,UU_TRUE,UU_TRUE,&rect);
/*
...Reset draw area and draw flags
*/
		ug_resetredrwrect();
		ug_setredrwflag(0);
/*
.....Update the workstation
*/
		uw_glupdate_front(UG_SUPPRESS);
/*
.....Reset the current graphics mode
*/
		uw_glset_context(cx,UU_FALSE);
	}
}

/*********************************************************************
**    E_FUNCTION     :  uw_glupdate_front(flag)
**       Update workstation by copying the dirty part of the back buffer
**       to the front buffer.
**    PARAMETERS   
**       INPUT  : 
**          flag      = UG_PERFORM  = Copy entire buffer.
**                      UG_SUPPRESS = Copy only affected rectangle.
**       OUTPUT :  
**           none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glupdate_front(flag)
int flag;
{
/*
.....Set dirty box to entire screen
*/
	if (UW_signon == 0) return;
	if (flag == (int)UG_PERFORM)
	{
		Sdirty_box.ll.x = 0.;
		Sdirty_box.ll.y = 0.;
		Sdirty_box.ur.x = uw_gl.xndc;
		Sdirty_box.ur.y = uw_gl.yndc;
	}
/*
.....Only update when dirty area has changed
*/
	if (Sdirty_box.ll.x <= Sdirty_box.ur.x)
	{
		uw_glswapbuffer(&Sdirty_box);
/*
.....Redraw cutter in front buffer
*/
		if (ncl_cutseg_front(0) && ncl_motion_displayed())
		{
			ncl_display_cutter_front();
		}
/*
.....Reset NDC box
*/
		Sdirty_box.ll.x = Sdirty_box.ll.y = 10000.;
		Sdirty_box.ur.x = Sdirty_box.ur.y = -10000.;
		uw_glflush();
	}
}
		
/*********************************************************************
**    E_FUNCTION     :  uw_glvp(n , segrect)
**      Check if segrect is within viewport n 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**               1: within viewport n
**               0: not in viewport n
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_glvp(n , segrect)
Gnrect* segrect;
int n;
{
	int rasll[2],rasur[2];
	if (UV_act_screen[0].nvports < n )
		return 0;
	uw_glndctodev(&(*segrect).ll,rasll);
	uw_glndctodev(&(*segrect).ur,rasur);
	if((rasll[0]>=uw_glscrn_vport[n].x)&&
				(rasll[0]<uw_glscrn_vport[n].x+uw_glscrn_vport[n].width )&&
				(rasll[1]>=uw_glscrn_vport[n].y)&&
				(rasll[1]<uw_glscrn_vport[n].y+uw_glscrn_vport[n].height))
		return 1;
	if((rasur[0]>=uw_glscrn_vport[n].x)&&
				(rasur[0]<uw_glscrn_vport[n].x+uw_glscrn_vport[n].width )&&
				(rasur[1]>=uw_glscrn_vport[n].y)&&
				(rasur[1]<uw_glscrn_vport[n].y+uw_glscrn_vport[n].height))
		return 1;
	if((rasll[0]>=uw_glscrn_vport[n].x)&&
				(rasll[0]<uw_glscrn_vport[n].x+uw_glscrn_vport[n].width )&&
				(rasur[1]>=uw_glscrn_vport[n].y)&&
				(rasur[1]<uw_glscrn_vport[n].y+uw_glscrn_vport[n].height))
		return 1;
	if((rasur[0]>=uw_glscrn_vport[n].x)&&
				(rasur[0]<uw_glscrn_vport[n].x+uw_glscrn_vport[n].width )&&
				(rasll[1]>=uw_glscrn_vport[n].y)&&
				(rasll[1]<uw_glscrn_vport[n].y+uw_glscrn_vport[n].height))
		return 1;

	return 0;
}               
/*********************************************************************
**    E_FUNCTION     : uw_glredraw_all(flag, segrect)
**       Redraws all segments on workstation within segrect
**       
**    PARAMETERS  
**       INPUT  : flag: 0: redraw all
**						1: redraw segment in area segrect
**				segrect: area to be redraw
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glredraw_all(flag, segrect)
int flag;
Gnrect *segrect;
{
	UG_segstli *p;
	int llras[2],urras[2];	
	int offll[2],offur[2];
	int clip[4], clip_sav[4],seg,i;
/*
.....Redraw within within segrect
*/
	if (flag)
	{
		(*(ug_gksstli.wsopen[uw_gl.wid].connid)[UG_DNDCDEV])
			(&(*segrect).ll,llras,uw_gl.wid);
		(*(ug_gksstli.wsopen[uw_gl.wid].connid)[UG_DNDCDEV])
			(&(*segrect).ur,urras,uw_gl.wid);
		offll[0] = 0; offll[1] = 0; offur[0] = 0; offur[1] = 0;

		llras[0] -= 1;
		llras[1] -= 1;
		urras[0] += 1;
		urras[1] += 1;

		uw_glget_scissor(clip_sav);
		clip[0] = llras[0];
		clip[1] = llras[1];
		clip[2] = urras[0] - llras[0] + 1;
		clip[3] = urras[1] - llras[1] + 1;
		uw_glset_scissor(clip);

		(*(ug_gksstli.wsopen[uw_gl.wid].connid)[UG_DRASPUT])
					(llras,urras,NULL,UG_CLEAR,offll,offur);
		uw_glmark_dirty_rect(segrect);
	}
/*
.....Redraw entire screen
*/
	else
	{
		uw_glclearws();
		uw_glmark_dirty_screen();
	}
/*
.....Loop through segments
........Assist segments must be displayed last
........Without using the depth buffer
........So that they are always displayed on the screen
*/
	ug_seginitscan();                                 
	while( (p=ug_segscan()) != NULL  ) 
	{
/*
........Display the segment
*/
		if (!ud_isassist_seg(p->segid))
			S_draw_segment(p,flag,segrect);
/*
........Display the segment
*/
	}
/*
.....Display the assist segments
*/
	for (i=0;i<ud_getn_assist_segs();i++)
	{
		seg = ud_get_assist_seg(i);
		if (seg == -1) break;
		p = ug_segac(seg);
		if (p != UU_NULL) S_draw_segment(p,flag,segrect);
	}
/*
.....Reset the clipping rectangle
*/
	if (flag)
		uw_glset_scissor(clip_sav);
}

/*********************************************************************
**    E_FUNCTION     : S_draw_segment(segptr, flag, segrect)
**       Displays a single segment within segrect.
**       
**    PARAMETERS  
**       INPUT  :
**          segptr = Pointer to segment.
**          flag:  = 0: redraw all
**                   1: redraw segment in area segrect
**				segrect: area to be redrawn
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_draw_segment(segptr, flag, segrect)
UG_segstli *segptr;
int flag;
Gnrect *segrect;
{
	Gnrect rect;
	int draw;
/*
.....Initialize routine
*/
	draw = 0;
	Supdate_dirty = UU_FALSE;
/*
.....Segment is visible
.....Determine if it is within redraw rectangle
*/
	if (segptr->segid != NCL_mot_seg && segptr->segatts.gvis == UG_VISIBLE)
	{
		if ((segptr->wcboxok==1)&&(flag)) 
		{
			ug_segndcbox(segptr->segid,&rect);
/*
........Segment is not within rectangle
*/
			if ((flag) && ( (rect.ll.x > segrect->ur.x + .005) ||
				(rect.ll.y > segrect->ur.y + 0.005) ||
				(rect.ur.x + 0.005<(*segrect).ll.x) ||
				(rect.ur.y + 0.005<(*segrect).ll.y)));
/*
........Segment is within rectangle
*/
			else 			
				draw=1;
		}
/*
........Segment box is not defined
*/
		else 
			draw=1;
/*
.....View the segment
*/
		if (draw)
		{
/*
........Highlighted segment
*/
/*			if (segptr->segatts.ghilit==UG_HIGHLIGHTED)
				uw_glviewsg(UG_MAXSEGNO+segptr->segid+1);*/
/*
........Normal segment
*/
/*			else*/
				uw_glviewsg(segptr->segid);
		}
	}
	Supdate_dirty = UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION : uw_glresize_graphics(wid,hgt,disp)
**      
**    DESCRIPTION:
**        This routine is called whenever the user resizes the graphics
**			 area.  It recalculates the viewing matrix scale, adjusting
**			 the graphics display to fit within the new window. 
**
**    PARAMETERS   
**       INPUT  : 
**          wid    = New graphics area width.
**          hgt    = New graphics area width.
**          disp   = UU_TRUE = Redisplay the graphics, UU_FALSE = Don't.
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glresize_graphics(wid,hgt,disp)
int wid,hgt;
UU_LOGICAL disp;
{
	int i,px[2],prms[3];
	UU_REAL ndc[2];
	UV_vport vport;
	UV_view view;
	UM_pkwin_type cx;
/*
.....Reset pocket graphics if active
*/
	cx = uw_glget_context();
	uw_glset_context(UM_NCL_WINDOW, UU_FALSE);
/*
.....Let DAS perform an internal resize
*/
	udm_resize_graphics(&UD_duimsdeflt,wid,hgt,0);
/*
.....Redisplay all views at the new scale
*/
	gswswindow(UD_ksws,&(UD_duimsdeflt.screen[0].wswind));
	ug_ndcboxdel(); 
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		view.modified = UU_TRUE;
		uv_delete_hidden(&vport);
		if (disp)
			uv_autofact7_redrawvp(&vport, &view, UU_TRUE); 
		else
			uv_autofact7_redrawvp(&vport, &view, UU_FALSE); 
	}
/*
.....Set screen NDC coordinates
*/
	px[0] = uw_gl.xpixels;
	px[1] = uw_gl.ypixels;
	uw_gldevtondc(px,ndc);
	uw_gl.xndc = ndc[0];
	uw_gl.yndc = ndc[1];
/*
.....Redraw all displayed
*/
	prms[2] = UG_PERFORM;
	uw_glupd(prms);
/*
.....Reset current graphics mode
*/
	uw_glset_context(cx, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : uw_glget_context()
**       Returns the active graphics context.
**       
**    PARAMETERS  
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      :
**         UM_DRAWING_WINDOW - Drawing Window is Active.
**         UM_IPV_WINDOW     - NCLIPV Window is Active.
**         UM_NCL_WINDOW     - NCL Window is Active.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UM_pkwin_type uw_glget_context()
{
#if UU_COMP == UU_WIN2K
	UM_pkwin_type uw_ntget_context();
	return(uw_ntget_context());
#else
	UM_pkwin_type uw_mfget_context();
	return(uw_mfget_context());
#endif
}

/*********************************************************************
**    E_FUNCTION     : uw_glset_context(which,force)
**       Sets the active graphics context for the requested window.
**       
**    PARAMETERS  
**       INPUT  :
**          which     = UM_DRAWING_WINDOW - Drawing window.
**                      UM_IPV_WINDOW - NCLIPV window.
**                      UM_NCL_WINDOW - NCL window.
**
**          force     = UU_TRUE = Set context even if it was last one set.
**       OUTPUT : none
**    RETURNS      :
**         UM_DRAWING_WINDOW - Drawing Window is Active.
**         UM_IPV_WINDOW     - NCLIPV Window is Active.
**         UM_NCL_WINDOW     - NCL Window is Active.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glset_context(which,force)
UM_pkwin_type which;
UU_LOGICAL force;
{
#if UU_COMP == UU_WIN2K
	uw_ntset_context(which,force);
#else
	uw_mfset_context(which,force);
#endif
}
/*********************************************************************
**    I_FUNCTION     :  S_draw_background()
**       Paints the background onto the screen.
**    PARAMETERS   
**       INPUT  : 
**                                      none
**       OUTPUT :  
**                                      none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_draw_background()
{
	int mode;
	GLint vp[4],vport[4];
	UM_vector rgb;
/*
.....Pocket window always uses black background
*/
	if (um_is_pocket_graphics())
	{
		mode = 0; rgb[0] = rgb[1] = rgb[2] = 0.;
	}
	else if ((UW_print_screen==1)&&(UW_print_back==0))
	{
		mode = 0; 
		rgb[0] = rgb[1] = rgb[2] = 255.;
	}
	else
	{
		mode = UV_background.shader;
		rgb[0] = UV_background.colors[0][0];
		rgb[1] = UV_background.colors[0][1];
		rgb[2] = UV_background.colors[0][2];
	}
/*
.....Clear the depth and stencil buffers
*/
	if (uw_glget_stencil())
		glClear_d(GL_STENCIL_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	else
		glClear_d(GL_DEPTH_BUFFER_BIT);
/*
.....Solid color
*/
	switch (mode)
	{
	case 0:
	default:
		glClearColor_d((GLclampf)rgb[0],rgb[1],rgb[2],(GLclampf)0);
		glClear_d(GL_COLOR_BUFFER_BIT);
		break;
/*
.....Graduated background
*/
	case 1:
	case 2:
		uw_glpushx();
		glMatrixMode_d(GL_MODELVIEW);
		glLoadIdentity_d();
		glMatrixMode_d(GL_PROJECTION);
		glLoadIdentity_d();
		glOrtho_d(-1.0, 1.0, -1.0, 1.0, 1.0, -1.0);
		uw_glget_vport(vport);
		vp[0] = vp[1] = 0;
		vp[2] = glwdt.dspsize.raster.x; vp[3] = glwdt.dspsize.raster.y;
		uw_glViewport(vp);
		uw_gllighting_reset();
		uw_gllighting(UU_FALSE);
		glBegin_d(GL_QUADS);
		{                    
			glColor3f_d((GLclampf)UV_background.colors[0][0],
				(GLclampf)UV_background.colors[0][1],
				(GLclampf)UV_background.colors[0][2]);
			glVertex3f_d( -1.0f, 1.0f, 1.0f);
			glColor3f_d((GLclampf)UV_background.colors[1][0],
				(GLclampf)UV_background.colors[1][1],
				(GLclampf)UV_background.colors[1][2]);
			glVertex3f_d( 1.0f, 1.0f, 1.0f);
			glColor3f_d((GLclampf)UV_background.colors[3][0],
				(GLclampf)UV_background.colors[3][1],
				(GLclampf)UV_background.colors[3][2]);
			glVertex3f_d( 1.0f, -1.0f, 1.0f);
			glColor3f_d((GLclampf)UV_background.colors[2][0],
				(GLclampf)UV_background.colors[2][1],
				(GLclampf)UV_background.colors[2][2]);
			glVertex3f_d( -1.0f, -1.0f, 1.0f);
		}
		glEnd_d();
		uw_glViewport(vport);
		uw_glpopx();
		break;
/*
.....JPEG Image
*/
	case 3:
		break;
	}
}
#endif
