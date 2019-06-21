/*********************************************************************
**    NAME         :  vwindow
**       CONTAINS:
**			uv_get_window_bounds(vpid, llf, urb)
**			UU_LOGICAL uv_view_cpln_parall(view)
**			uv_vp_aspect_ratio(vport, aspect_ratio)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       vwindow.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:00
*********************************************************************/

#include "udebug.h"
#include "umath.h"
#include "usysdef.h"
#include "view.h"
#include "dasnog.h"
#include "uims.h"
#include "mdcpln.h"

/*********************************************************************
**    E_FUNCTION     : uv_get_window_bounds(vpid, llf, urb)
**       Get the window bounds for the view in the viewport identified
**			by its key (VPID).
**    PARAMETERS   
**       INPUT  : 
**          vpid				view port id
**       OUTPUT :  
**          llf				lower left front of view in vport vpid
**				urb				upper right back of view in vport vpid
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uv_get_window_bounds(vport, llf, urb)
	UV_vport *vport;
	UM_coord	llf;
	UM_coord	urb;

	{
	UV_view view;
	UU_REAL aspect_ratio;						/* aspect ratio of vport */

	uu_denter(UU_MTRC,(us,"uv_get_window_bounds(vport->key = %x)",vport->key));

	uv_vp_aspect_ratio(vport, &aspect_ratio);
	uv_getvid(vport->cur_view, &view);

	llf[0] = -(view.cur_aperture/2.);
	llf[1] = -(view.cur_aperture*aspect_ratio/2.);
	llf[2] = view.cur_front_clip;
	urb[0] = (view.cur_aperture/2.);
	urb[1] = (view.cur_aperture*aspect_ratio/2.);
	urb[2] = view.cur_back_clip;

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL uv_view_cpln_parall(view)
**       Determine if the construction plane and the view normal
**			vector are parallel.
**    PARAMETERS   
**       INPUT  : 
**          view						view record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
uv_view_cpln_parall(view)
	UV_view *view;

	{
	UU_LOGICAL parallel;

	uu_denter(UU_MTRC,(us,"uv_view_cpln_parall()"));

	parallel = um_vcparall(UM_cpln.zaxis, view->cur_pln_norm);
	uu_dexit;
	return(parallel);
	}
/*********************************************************************
**    E_FUNCTION     : uv_vp_aspect_ratio(vport, aspect_ratio)
**       Calculate the ASPECT_RATIO of the specified viewport VPORT.
**    PARAMETERS   
**       INPUT  : 
**          vport					viewport entity
**       OUTPUT :  
**          aspect_ratio		aspect ratio of the viewport
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uv_vp_aspect_ratio(vport, aspect_ratio)
	UV_vport *vport;
	UU_REAL *aspect_ratio;

	{
	int curr_scrn;
	Gnrect *gr_area;

	uu_denter(UU_MTRC,(us,"uv_vp_aspect_ratio(vpkey=%x)",vport->key));

	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);
	*aspect_ratio = ((vport->urb[1] - vport->llf[1]) *
						(gr_area->ur.y - gr_area->ll.y))/
						((vport->urb[0] - vport->llf[0]) *
						(gr_area->ur.x - gr_area->ll.x));
	uu_dexit;
	}
