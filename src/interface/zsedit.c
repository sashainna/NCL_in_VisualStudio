/*********************************************************************
**
**    NAME         :  zsedit.c
**
**       CONTAINS:
**    				ncl_reset_select()
**    				uz_delete
**    				uz_del_all_disp()
**    				uz_invisible()
**    				uz_invis_all()
**    				uz_visible()
**    				uz_visible_all()
**					uz_editable()
**					uz_nonedit()
**    				uz_trim_extend()
**    				uz_mod_attribs()
**    				uz_mod_entity()
**    				uz_scale()
**    				uz_move_pt_pt()
**    				uz_move_vector()
**    				uz_move_drag()
**    				uz_move_rotate()
**    				uz_move_mirror()
**    				uz_copy_pt_pt()
**    				uz_copy_vector()
**    				uz_copy_drag()
**    				uz_copy_rotate()
**    				uz_copy_mirror()
**    				uz_copy_scale()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zsedit.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:37
**
*********************************************************************/

#include "usysdef.h"
#include "dinput.h"
#include "dselect.h"
#include "uhep.h"
#include "mdunits.h"
#include "dmark.h"
#include "mfort.h"
#include "nclfc.h"

#define RESL ud_dtcord(UD_LOCATOR, 1, 1)
#define RBUFF(call) if(UD_BUFTEST() == UU_FALSE) \
		{ncl_reset_select(0); call; ncl_reset_select(0);} \
		else uu_uerror0(UD_DASHEP, 77)

/*********************************************************************
**    I_FUNCTION :  ncl_reset_select(flag)
**			reset the system select buffer pointers 
**    PARAMETERS   
**       INPUT  : flag: if it is 1: force reset
**          
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ncl_reset_select(flag)
UM_int4 *flag;
{
	int forced = 0;
	if (flag!=UU_NULL)
	{
		if (*flag)
			forced = 1;
	}
	if ((UD_BUFTEST() == UU_FALSE) || (forced))
	{
		UD_Select_cnt = UD_Selrej_ptr = UD_Selrej_cnt = UD_Select_ptr = 0;
	}
}

/*********************************************************************
**    E_FUNCTION :  uz_delete
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_delete()
{
	RBUFF(ucu_delete());
}

/*********************************************************************
**    E_FUNCTION :  uz_del_all_disp()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_del_all_disp()
{
	RBUFF(ucu_delete_all_disp());
}

/*********************************************************************
**    E_FUNCTION :  uz_invisible()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_invisible()
{
	RESL;
/*
	if (UU_application == UU_NCLCAM && UM_2d3d_mode != UM_2D)
		RBUFF(nclu_erase(0));
	else
*/
		RBUFF(umu_sea_ent_blank(0));
		
}

/*********************************************************************
**    E_FUNCTION :  uz_invis_all()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_invis_all()
{
	if (ud_yesno(0, "All entities will be made invisible.  Continue?", "Question?"))
		{
		if (UU_application == UU_NCLCAM)
			RBUFF(nclu_erase(1));
		else
			RBUFF(umu_sea_ent_blank(1));
		}
}

/*********************************************************************
**    E_FUNCTION :  uz_visible()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_visible()
{
	RESL;
/*	if (UU_application == UU_NCLCAM)*/
/*		RBUFF(nclu_disply(0));*/
/*	else*/
		RBUFF(umu_sea_ent_blank(2));
}

/*********************************************************************
**    E_FUNCTION :  uz_visible_all()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_visible_all()
{
	if (UU_application == UU_NCLCAM)
		{
		RESL;
		RBUFF(nclu_disply(1));
		}
	else
		umu_sea_ent_blank(3);
}

/*********************************************************************
**    E_FUNCTION :  uz_swap_vis()
**       Swap visible/invisible entities.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_swap_vis()
{
	RBUFF(umu_swap_ent_blank());
}

/*********************************************************************
**    E_FUNCTION :  uz_editable()
**       DDC turn entities editable entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_editable()
{
	RESL;
	RBUFF(umu_sea_ent_edit());
}

/*********************************************************************
**    E_FUNCTION :  uz_nonedit()
**       DDC turn entities non-editable entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_nonedit()
{
	RESL;
	RBUFF(umu_sea_ent_nedit());
}

/*********************************************************************
**    E_FUNCTION :  uz_trim_extend()
**       DDC select function entry point
**       The option was added to indicate whether untrim or
**       trimming is to be done. JLS 3/2/99
**       Untrim, option = 2
**       Trim, option = 0
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_trim_extend(option)
int option;
{
	RESL;
	umu_trim1_curve(option);
}

/*********************************************************************
**    E_FUNCTION :  uz_mod_attribs()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_mod_attribs()
{
	RBUFF(umu_get_ent_attr());
}

/*********************************************************************
**    E_FUNCTION :  uz_mod_entity()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_mod_entity()
{
	RBUFF(umu_modify_entity_geometry());
}

/*********************************************************************
**    E_FUNCTION :  uz_scale()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_scale()
{
	RBUFF(ucu_scale());
}

/*********************************************************************
**    E_FUNCTION :  uz_move_pt_pt()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_move_pt_pt()
{
	RBUFF(ucu_translate(0));
}

/*********************************************************************
**    E_FUNCTION :  uz_move_vector()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_move_vector()
{
	RBUFF(ucu_translate(1));
}

/*********************************************************************
**    E_FUNCTION :  uz_move_drag()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_move_drag()
{
	RESL;ucu_drag_translate();
}

/*********************************************************************
**    E_FUNCTION :  uz_move_rotate()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_move_rotate()
{
	RBUFF(ucu_rotate(0));
}

/*********************************************************************
**    E_FUNCTION :  uz_move_mirror()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_move_mirror()
{
	RBUFF(ucu_mirror());
}

/*********************************************************************
**    E_FUNCTION :  uz_copy_pt_pt()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_copy_pt_pt()
{
	RBUFF(ucu_copy_translate(0));
}

/*********************************************************************
**    E_FUNCTION :  uz_copy_vector()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_copy_vector()
{
	RBUFF(ucu_copy_translate(1));
}

/*********************************************************************
**    E_FUNCTION :  uz_copy_drag()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_copy_drag()
{
	RESL;ucu_drag_copy_translate();
}

/*********************************************************************
**    E_FUNCTION :  uz_copy_rotate()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_copy_rotate()
{
	RBUFF(ucu_copy_rotate());
}

/*********************************************************************
**    E_FUNCTION :  uz_copy_mirror()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_copy_mirror()
{
	RBUFF(ucu_copy_mirror());
}

/*********************************************************************
**    E_FUNCTION :  uz_copy_scale()
**       DDC select function entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_copy_scale()
{
	RBUFF(ucu_copy_scale());
}
/*********************************************************************
**    E_FUNCTION :  uz_ubfn_invisible()
**       make ubfn geom invisible
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_ubfn_invisible()
{
	RESL;
	RBUFF(umu_ubfn_ent_blank(0));		
}

/*********************************************************************
**    E_FUNCTION :  uz_ubfn_invis_all()
**       make all ubfn geom invisible
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_ubfn_invis_all()
{
	if (ud_yesno(0, "All entities in the Secondary Unibase will be made invisible.  Continue?", "Question?"))
	{
		RBUFF(umu_ubfn_ent_blank(1));
	}
}

/*********************************************************************
**    E_FUNCTION :  uz_ubfn_visible()
**       make all ubfn geom visible
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_ubfn_visible()
{
	RESL;
	RBUFF(umu_ubfn_ent_blank(2));
}

/*********************************************************************
**    E_FUNCTION :  uz_ubfn_vis_all()
**       make all ubfn geom visible
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_ubfn_vis_all()
{
	umu_ubfn_ent_blank(3);
}
