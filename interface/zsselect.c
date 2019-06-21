/*********************************************************************
**
**    NAME         :  zsselect.c
**
**       CONTAINS:
**          NCL select subsystem support routines
**
**            uz_nclu_allentity_type()
**    			uz_single()
**    			uz_all_display()
**    			uz_all_points()
**    			uz_region_in()
**    			uz_region_out()
**    			uz_reg_in_x()
**    			uz_reg_out_x()
**    			uz_set_sel_fil()
**    			uz_filtered_sgl()
**    			uz_filtered_chn()
**    			uz_fltr_all_dsp()
**    			uz_freg_in()
**    			uz_freg_out()
**    			uz_freg_in_x()
**    			uz_freg_out_x()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       zsselect.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:38
**
*********************************************************************/

#include "udebug.h"
#include "usysdef.h"
#include "dselect.h"
#include "dasnog.h"
#include "dinput.h"
#include "mdunits.h"
#include "uhep.h"
#include "nclicons.h"

#define MSG "single select - pick entity"
#define RESL ud_dtcord(UD_LOCATOR, 1, 1)
#define TWODERR(name) uu_uerror1(UU_SIGNON, 6, name)

/*********************************************************************
**    E_FUNCTION :  uz_nclu_allentity_type()
**            Select geometry by type.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_nclu_allentity_type()
{
	RESL;
	nclu_allentity_type();
}

/*********************************************************************
**    E_FUNCTION :  uz_single()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_single()
{
	ud_single(MSG, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION :  uz_all_display()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_all_display()
{
	RESL;
	ud_alldisp(UU_FALSE, UU_FALSE, UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION :  uz_all_points()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_all_points()
{
	RESL;
	ud_allpoints();
}

/*********************************************************************
**    E_FUNCTION :  uz_region_in()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_region_in()
{
   RESL;
	ud_region(UD_SELECTIN, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION :  uz_region_out()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_region_out()
{
   RESL;
	ud_region(UD_SELECTOUT, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION :  uz_reg_in_x()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_reg_in_x()
{
	RESL;
	ud_region(UD_SELECTXIN, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION :  uz_reg_out_x()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_reg_out_x()
{
	RESL;
	ud_region(UD_SELECTXOUT, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION :  uz_set_sel_fil()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_set_sel_fil()
{ 
	if(UM_2d3d_mode==UM_3D) 
		ud_getfilt();
	else 
		TWODERR("Set Filter");
}

/*********************************************************************
**    E_FUNCTION :  uz_filtered_sgl()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_filtered_sgl()
{ 
	if(UM_2d3d_mode==UM_3D) 
		ud_single(MSG, UU_TRUE);
	else 
		TWODERR("Filtered Select");
}

/*********************************************************************
**    E_FUNCTION :  uz_filtered_chn()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_filtered_chn()
{ 
	if(UM_2d3d_mode==UM_3D) 
		ud_chain(UU_TRUE);
	else 
		TWODERR("Filtered Select");
}

/*********************************************************************
**    E_FUNCTION :  uz_fltr_all_dsp()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_fltr_all_dsp()
{ 
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		ud_alldisp(UU_TRUE, UU_TRUE, UU_TRUE);
	} 
	else 
		TWODERR("Filtered Select");
}

/*********************************************************************
**    E_FUNCTION :  uz_freg_in()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_freg_in()
{ 
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		ud_region(UD_SELECTIN, UU_TRUE);
	} 
	else 
		TWODERR("Filtered Select");
}

/*********************************************************************
**    E_FUNCTION :  uz_freg_out()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_freg_out()
{ 
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		ud_region(UD_SELECTOUT, UU_TRUE);
	} 
	else 
		TWODERR("Filtered Select");
}

/*********************************************************************
**    E_FUNCTION :  uz_freg_in_x()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_freg_in_x()
{ 
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		ud_region(UD_SELECTXIN, UU_TRUE);
	} 
	else 
		TWODERR("Filtered Select");
}

/*********************************************************************
**    E_FUNCTION :  uz_freg_out_x()
**       DDC select system entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_freg_out_x()
{ 
	if(UM_2d3d_mode==UM_3D) 
	{
		RESL;
		ud_region(UD_SELECTXOUT, UU_TRUE);
	} 
	else 
		TWODERR("Filtered Select");
}
