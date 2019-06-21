/*********************************************************************
**
**    NAME         :  zssurfac.c
**
**       CONTAINS:
**          NCLCADD Surface interface handling routines
**			uz_render_surf()
**			uz_render_modals()
**			uz_material_modals()
**			uz_lights()
**
**    COPYRIGHT 1990 (c) MILLS DATA SYSTEMS CO. Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zssurfac.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:38
**
*********************************************************************/
#include "usysdef.h"

extern int NCLHOST;	/* which graphics device */
/*********************************************************************
**
**    E_FUNCTION :  uz_render_surf()
**       Disable Surface rendering on VMS TEK terminals
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_render_surf()
	{
#if UU_COMP != UU_VAXVMS
	if (NCLHOST == 0)
/*
... aak:
		um_render_surf();
*/
		nclu_shade_surf ();
	else
#else
		ud_prmerr("Surface rendering not available on this terminal.");
#endif
	return;
	}

/*********************************************************************
**
**    E_FUNCTION :  uz_material_modals()
**       Disable Surface rendering on VMS TEK terminals
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_material_modals()
	{
#if UU_COMP != UU_VAXVMS
	if (NCLHOST == 0)
		um_material_modals();
	else
#else
		ud_prmerr("Surface rendering not available on this terminal.");
#endif
	return;
	}

/*********************************************************************
**
**    E_FUNCTION :  uz_lights()
**       Disable Surface rendering on VMS TEK terminals
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_lights()
{
#if UU_COMP != UU_VAXVMS
	if (NCLHOST == 0)
	{
		if (ncl_shading_mode()) 
		{
			ncl_shading_switch ();
			uz_repaint(1);
		}
	}
	else
#else
		ud_prmerr("Surface rendering not available on this terminal.");
#endif
	return;
}
