/*********************************************************************
**
**    NAME         :  zsview.c
**
**       CONTAINS:
**				uvu_savev
**				uz_reset_view
**				uz_view_from_axis
**				uz_change_view
**				uzu_change_view
**				uz_snap_view
**				uz_swap_view
**				uz_repaint
**				uz_extrema_zoom
**				uz_window_zoom
**				uz_zoom
**				uz_scale_xy
**				uz_pan
**				uz_rotate_x_y
**				uz_tumble
**				uz_rotate_vec
**				uz_view_single
**				uz_view_sbys
**				uz_view_sp
**				uz_view_fv
**				uz_view_es
**				uz_view_of
**				uz_view_shaded
**				uz_save_view
**				uz_load_view
**				uz_dyn_zoom
**				uz_dyn_pan
**				uz_dyn_unzoom
**				uz_dyn_mouse
**				uz_dyn_modals
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zsview.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:39
**
*********************************************************************/

#include "view.h"
#include "dinput.h"
#include "uhep.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "dmark.h"
#include "xenv1.h"
#include "nclicons.h"

#define RESL if(UV_act_screen[0].nvports>1)ud_dtcord(UD_LOCATOR, 1, 1)
#define RESL1 ud_dtcord(UD_LOCATOR, 1, 1)
#define TWODERR(name) uu_uerror1(UU_SIGNON, 6, name)

extern UU_LOGICAL UA_draft;
UU_LOGICAL UV_chng_format;

extern int NCLHOST;	/* from interface/znuinit.c - determined by terminal type */

void uz_repaint();
extern int UW_dynamic_funkey;
extern int (*UW_dynfunc_key)();
/*********************************************************************
**    E_FUNCTION :  uz_reset_view()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_reset_view()
{
	RESL;
	uvu_restorev(0);
}

/*********************************************************************
**    E_FUNCTION :  uz_view_from_axis()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          axis   = 0 - POSX, 1 - POSY, 2 - POSZ, 3 - USER,
**				         4 - NEGX, 5 = NEGY, 5 = NEGZ, 6 - FLIP
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_view_from_axis(axis)
int axis;
{
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		uvu_vwpnorm(axis);
		uz_repaint(1);
	} 
	else 
		TWODERR("View from AXIS");
}

/*********************************************************************
**    E_FUNCTION :  uz_change_view()
**       KEYBOARD entry point into change view form
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_change_view()
{

	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		uvu_change_view();
	} 
	else 
		TWODERR("Change View");
}

/*********************************************************************
**    E_FUNCTION :  uz_tracut_view()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_tracut_view()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		nclu_tracut_view();
	} 
	else 
		TWODERR("Change View");
}

/*********************************************************************
**    E_FUNCTION :  uz_refsys_view()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_refsys_view()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		nclu_refsys_view();
	} 
	else 
		TWODERR("Change View");
}

/*********************************************************************
**    E_FUNCTION :  uz_matrix_view()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_matrix_view()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		nclu_get_mx_view();
	} 
	else 
		TWODERR("Change View");
}

/*********************************************************************
**    E_FUNCTION :  uz_tool_view()
**       NCL view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_tool_view()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		nclu_tool_view();
	} 
	else 
		TWODERR("Tool Axis View");
}

/*********************************************************************
**    E_FUNCTION :  uz_snap_view()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_snap_view()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		uvu_snap_view();
	} 
	else 
		TWODERR("Snap View");
}

/*********************************************************************
**    E_FUNCTION :  uz_swap_view()
**       Swap the visible/invisible views.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_swap_view()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		uvu_swap_view();
	} 
	else 
		TWODERR("Swap View");
}

/*********************************************************************
**    E_FUNCTION :  uz_repaint()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          flg - 1 for viewport repaint
**                0 for full screen repaint
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_repaint(flg)
int flg;
	{
	RESL;
	if ((flg) && (NCLHOST == 0))
		uvu_repaint_viewport();
	else
		um_repaint();
	}

/*********************************************************************
**    E_FUNCTION :  uz_extrema_zoom()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_extrema_zoom()
{
	RESL;
	uvu_vpzoomextrema();
}

/*********************************************************************
**    E_FUNCTION :  uz_window_zoom()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_window_zoom()
{
	RESL1;
	uvu_vpzoomin();
}

/*********************************************************************
**    E_FUNCTION :  uz_zoom()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_zoom()
{
	RESL;
	uvu_vpzoomout();
}

/*********************************************************************
**    E_FUNCTION :  uz_scale_xy()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_scale_xy()
{
	RESL;
	uvu_scale_xy();
}

/*********************************************************************
**    E_FUNCTION :  uz_pan()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_pan()
{
	RESL;
	uvu_vppan();
	uz_repaint(1);
}

/*********************************************************************
**    E_FUNCTION :  uz_rotate_x_y()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_rotate_x_y()
{
	char name[40];
started:;
	if(UM_2d3d_mode==UM_3D) 
		uj_dynxyrot(); 
	else 
		TWODERR("Rotate XY");
	if (UW_dynamic_funkey!=-1)
	{
		(*(UW_dynfunc_key)) (UW_dynamic_funkey, &name,1);
		UW_dynamic_funkey = -1;
		goto started;
	}

}

/*********************************************************************
**    E_FUNCTION :  uz_tumble()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_tumble()
{
	char name[40];
started:;
	if(UM_2d3d_mode==UM_3D) 
		uj_dyntumble(); 
	else 
		TWODERR("Tumble XY");
	if (UW_dynamic_funkey!=-1)
	{
		(*(UW_dynfunc_key)) (UW_dynamic_funkey, &name,1);
		UW_dynamic_funkey = -1;
		goto started;
	}
}

/*********************************************************************
**    E_FUNCTION :  uz_rotate_vec()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_rotate_vec()
{
	char name[40];
started:;
	if(UM_2d3d_mode==UM_3D) 
		uj_dynvecrot(); 
	else 
		TWODERR("Rotate Vector");
	if (UW_dynamic_funkey!=-1)
	{
		(*(UW_dynfunc_key)) (UW_dynamic_funkey, &name,1);
		UW_dynamic_funkey = -1;
		goto started;
	}
}

/*********************************************************************
**    E_FUNCTION :  uz_view_single()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_view_single()
{
	uvu_select_screen_format("single"); 
	if (UM_cpln.grid.snap) 
		uz_actgrid("Grid ON"); 
	else 
		uz_actgrid("Grid OFF"); 
	UV_chng_format = UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION :  uz_view_sbys()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_view_sbys()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		uvu_select_screen_format("vert. dual"); 
		if (UM_cpln.grid.snap) 
			uz_actgrid("Grid ON"); 
		else 
			uz_actgrid("Grid OFF"); 
		if(UA_draft)
			UV_chng_format = UU_TRUE;
	} 
	else 
		TWODERR("vert. dual");
}

/*********************************************************************
**    E_FUNCTION :  uz_view_sp()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_view_sp()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		uvu_select_screen_format("horiz. dual");
		if (UM_cpln.grid.snap) 
			uz_actgrid("Grid ON"); 
		else 
			uz_actgrid("Grid OFF"); 
		if(UA_draft) 
			UV_chng_format = UU_TRUE;
	} 
	else 
		TWODERR("horiz. dual");
}

/*********************************************************************
**    E_FUNCTION :  uz_view_fv()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_view_fv()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		uvu_select_screen_format("quad"); 
		if (UM_cpln.grid.snap) 
			uz_actgrid("Grid ON"); 
		else 
			uz_actgrid("Grid OFF"); 
		if(UA_draft) 
			UV_chng_format = UU_TRUE;
	} 
	else
		TWODERR("quad");
}

/*********************************************************************
**    E_FUNCTION :  uz_view_es()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_view_es()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		uvu_select_screen_format("six equal"); 
		if (UM_cpln.grid.snap) 
			uz_actgrid("Grid ON"); 
		else 
			uz_actgrid("Grid OFF"); 
		if(UA_draft) 
			UV_chng_format = UU_TRUE;
	} 
	else
		TWODERR("six equal");
}

/*********************************************************************
**    E_FUNCTION :  uz_view_of()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_view_of()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		uvu_select_screen_format("five and one"); 
		if (UM_cpln.grid.snap) 
			uz_actgrid("Grid ON"); 
		else 
			uz_actgrid("Grid OFF"); 
		if(UA_draft) 
			UV_chng_format = UU_TRUE;
	} 
	else
		TWODERR("five and one");
}

/*********************************************************************
**    E_FUNCTION :  uz_vport_shaded(flag)
**       Changes the view format.
**    PARAMETERS   
**       INPUT  : 
**          flag    = 1 = Wireframe
**                    2 = Shaded
**                    3 = Hidden Line
**                    4 = Shaded only
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_vport_shaded(flag)
int flag;
{
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		uvu_vpshaded(flag);
	} 
	else 
		TWODERR("Shaded View");
}

/*********************************************************************
**    E_FUNCTION :  uz_save_view()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_save_view()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		uvu_savev();
	}
	else
		TWODERR("Save View");
}

/*********************************************************************
**    E_FUNCTION :  uz_load_view()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_load_view(viewname)
char *viewname;
{
	/* Check for drawing managment. kathy */
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		uvu_loadv(viewname,UU_TRUE); 
		if (UM_cpln.grid.snap) 
			uz_actgrid("Grid ON"); 
		else 
			uz_actgrid("Grid OFF");
	}
	else
		TWODERR("Load View");

}

/*********************************************************************
**	 E_FUNCTION : uz_dyn_zoom
**			This function calls the appropriate routine to
**			perform hardware zoom.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Changes the screen view to an unknown state.
**	 WARNINGS: none.
**       NOTES: none.
*********************************************************************/
void uz_dyn_zoom()
{
	char name[40];
started:;
	uj_dynzoom();
	if (UW_dynamic_funkey!=-1)
	{
		(*(UW_dynfunc_key)) (UW_dynamic_funkey, &name,1);
		UW_dynamic_funkey = -1;
		goto started;
	}
	return;
}
/*********************************************************************
**	 E_FUNCTION : uz_dyn_pan
**			This function calls the appropriate routine to
**			perform hardware pan.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Changes the screen view to an unknown state.
**	 WARNINGS: none.
**       NOTES: none.
*********************************************************************/

void uz_dyn_pan()
{
	char name[40];
started:;
	uj_dynpan();
	if (UW_dynamic_funkey!=-1)
	{
		(*(UW_dynfunc_key)) (UW_dynamic_funkey, &name,1);
		UW_dynamic_funkey = -1;
		goto started;
	}
	return;
}

/*********************************************************************
**	 E_FUNCTION : uz_dyn_unzoom
**			This function calls the appropriate routine to
**			reset hardware pan & zoom.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Changes the screen view to a known state.
**	 WARNINGS: none.
**       NOTES: none.
*********************************************************************/

void uz_dyn_unzoom()
{
	uz_reset_view();
	return;
}

/*********************************************************************
**	 E_FUNCTION : uz_dyn_mouse
**			This function calls the appropriate routine to
**			use the mouse for dynamic viewing.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Changes the screen view to a known state.
**	 WARNINGS: none.
**       NOTES: none.
*********************************************************************/

void uz_dyn_mouse()
{
	char name[40];
started:;
	uj_dynmouse();
	if (UW_dynamic_funkey!=-1)
	{
		(*(UW_dynfunc_key)) (UW_dynamic_funkey, &name,1);
		UW_dynamic_funkey = -1;
		goto started;
	}
	return;
}

/*********************************************************************
**    E_FUNCTION :  uz_dyn_modals()
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_dyn_modals()
{
	if(UM_2d3d_mode==UM_3D) 
	{
		uvu_dynmodals();
	} 
	else 
	{
		uvu_dynmodals();
/*		TWODERR("Dynamic modals");*/
	}
}
/*********************************************************************
**    E_FUNCTION :  uz_view_screen(sname)
**       DDC view menu entry points
**    PARAMETERS   
**       INPUT  : 
**          sname: screen name to view
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_view_screen(sname)
char *sname;
{
	if(UM_2d3d_mode==UM_3D) 
	{
		uvu_select_screen_format(sname);
		if (UM_cpln.grid.snap) 
			uz_actgrid("Grid ON"); 
		else 
			uz_actgrid("Grid OFF"); 
		if(UA_draft) 
			UV_chng_format = UU_TRUE;
	} 
	else 
		TWODERR(sname);
}
