/*********************************************************************
**  FILENAME: lipview.c
**  CONTAINS:   ul_ipv_view_active()
**				ul_ipv_view_inactive()
**              ul_ipv_flush()
**              ul_ipv_view_extrema()
**              ul_ipv_view_rect()
**              ul_ipv_dyncenter()
**              ul_ipv_set_defered()
**              ul_ipv_set_immediate()
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvview2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:18
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "xenv1.h"
#include "usysg.h"
#include "gobas.h"
#include "gdidd.h"
#include "gobas.h"
#include "ginqatt.h"
#include "gtbl.h"
#include "lcom.h"
#include "mdattr.h"
#include "mpocket.h"
#include "nclfc.h"
#include "lipv.h"
#include "lipvmach.h"
#include "gtbl.h"
#include "wsgl.h"
extern char *LW_window;

extern int PKx,PKy;
extern int UZ_nclipv_view;
extern int LW_dyncenter, UV_dyncenter;
extern UU_REAL LW_dyncp[3], UV_dyncp[3];
extern int NCL_swap_changed, UM_swap_ipv;
extern int UV_dynview_active;

static UU_LOGICAL Sdefered_mode = UU_FALSE, Sdefered_buffer = UU_FALSE;
extern int MSLite;
/*********************************************************************
**	 E_FUNCTION: UU_LOGICAL ul_ipv_view_active()
**		This function determines if NCLIPV is active and if it is,
**		sets the IPV Viewing flag.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: UU_TRUE if NCLIPV is active.
**	 SIDE EFFECTS: Sets the UZ_nclipv_view variable if NCLIPV is active.
**	 WARNINGS: none
*********************************************************************/
UU_LOGICAL ul_ipv_view_active()
{
	if (LW_active)
	{
		UZ_nclipv_view = 1;
		um_set_screen_area(UM_IPV_WINDOW);
		return(UU_TRUE);
	}
	else return(UU_FALSE);
}

/*********************************************************************
**	 E_FUNCTION: UU_LOGICAL ul_ipv_view_inactive()
**		This function resets the IPV Viewing flag.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_view_inactive()
{
	UZ_nclipv_view = 0;
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_flush(xform)
**		This function refreshes the NCLIPV window display.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_flush()
{
	if (LW_active)
	{
/*
.....Defered mode is in effect
*/
		if (Sdefered_mode)
			Sdefered_buffer = UU_TRUE;
		else
		{
			Sdefered_buffer = UU_FALSE;
			um_set_pocket_graphics(UM_IPV_WINDOW);
			if (LW_display == LW_ACTIVE) LiDrawableRender(LW_drawable,LW_viewport,
					LI_RENDER_MODE_MW_HYBRID);
			else if (LW_mach_mode == LW_RAPIDCUT)
				LiDrawableRender(LW_drawable,LW_viewport,
					LI_RENDER_MODE_MW_POLYGONS);
			else
			{
				if (LW_display_prop.mode == 0)
					LiDrawableRender(LW_drawable,LW_viewport,
						LI_RENDER_MODE_MW_HYBRID);
				else if (LW_display_prop.mode == 1)
					LiDrawableRender(LW_drawable,LW_viewport,
						LI_RENDER_MODE_MW_WIREFRAME);
				else if (LW_display_prop.mode == 2)
					LiDrawableRender(LW_drawable,LW_viewport,
						LI_RENDER_MODE_MW_HIDDEN_LINE);
			}
		}
	}
}
/*********************************************************************
**	 E_FUNCTION:int ul_ipv_view_rect(llx,lly,urx,ury)
**		This function marks the requested rectangle as "dirty" and calls
**    the MachineWorks routine to refresh this area.
**	 PARAMETERS	
**		 INPUT  : llx     = Lower left X of rectangle to redraw.
**		          lly     = Lower left Y of rectangle to redraw.
**		          urx     = Upper right X of rectangle to redraw.
**		          ury     = Upper right Y of rectangle to redraw.
**		 OUTPUT : none
**	 RETURNS: UU_TRUE if a box could be calculated.  UU_FALSE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_view_rect(llx,lly,urx,ury)
int llx,lly,urx,ury;
{
	LtInt32 llf[2],urb[3];
/*
.....Convert NCL rectangle coordinates
.....to NCLIPV coordinates
*/
	llf[0] = llx; llf[1] = PKy - lly;
	urb[0] = urx; urb[1] = PKy - ury;
/*
.....Mark area as dirty
*/
/*	if (LW_mach_mode == LW_VISICUT)
		LiViDirtyAreaSet(llf[0],urb[0],llf[1],urb[1]);*/
/*
.....Refresh screen
*/
	ul_ipv_flush();
	if (MSLite==0)
		uw_glset_context(UM_IPV_WINDOW,UU_FALSE);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_dyncenter()
**		Let the user change the dynamic viewing center.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_dyncenter()
{
	int button,event,status;
	LtDouble dis;
	UM_coord pt;
	UM_vector norm;
	LtPickedEntityList entlist;
	LtEntityType type;
	LtEntity entity;
	LtPickedEntity entpick;
	LW_rv_picked_entity rvent;
/*
.....Get dynamic viewing center
*/
again:;
	if (LW_active)
	{
		event = ul_ipv_pick_entity("Enter dynamic viewing center",
			LI_ENTITY_TYPE_FACE,&entlist,&rvent,pt,norm,&button);
		if (event == 3 && button == 1)
		{
			if (LW_mach_mode == LW_VISICUT)
			{
				entpick = LiViPickedEntityListGetFirst(entlist);
				if (entpick == 0) goto failed;
				status = LiViPickedEntityExpand(entpick,&entity,&type,&dis);
				if (status != 0) goto failed;
			}
			else
			{
				if (!rvent.picked) goto failed;
				dis = rvent.dis;
			}
			LW_dyncp[0] = pt[0] + norm[0]*dis;
			LW_dyncp[1] = pt[1] + norm[1]*dis;
			LW_dyncp[2] = pt[2] + norm[2]*dis;
			ncl_mcstowcs(0,LW_dyncp,LW_dyncp);
			LW_dyncenter = 1;
			if (LW_mach_mode == LW_VISICUT) LiViPickedEntityListDestroy(entlist);
			if (UV_dyncenter==0)
			{
				UV_dyncenter = 1;
				UV_dyncp[0] = LW_dyncp[0];
				UV_dyncp[1] = LW_dyncp[1];
				UV_dyncp[2] = LW_dyncp[2];
			}
		}
	}
	goto done;
/*
.....Failed to pick entity
*/
failed:;
	ud_wrerr("Attempted pick failed.");
	goto again;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_set_defered()
**		Sets the NCLIPV defered viewing mode.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_set_defered()
{
	Sdefered_mode = UU_TRUE;
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_set_immediate(flush)
**		Sets the NCLIPV immediate viewing mode.
**	 PARAMETERS	
**		 INPUT  :
**        flush   = UU_TRUE = Flush out any defered graphics.
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_set_immediate(flush)
UU_LOGICAL flush;
{
	Sdefered_mode = UU_FALSE;
	if (Sdefered_buffer && flush)
	{
		ul_ipv_flush();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
	}
}
