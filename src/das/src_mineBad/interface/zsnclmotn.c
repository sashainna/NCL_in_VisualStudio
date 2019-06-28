/*********************************************************************
**    NAME         :  zsnclmotn.c
**       CONTAINS: NCL MOTION interface routines
**
**    				uz_zmotexit()
**    				uz_zmotion()
**    				uz_zncl_cmd()
**    				uz_lathe_rough()
**    				uz_lathe_finish()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       zsnclmotn.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:38
*********************************************************************/
#include "nclicons.h"
#include "mdunits.h"
#include "uhep.h"
#include "dmark.h"

#define TWODERR(name) uu_uerror1(UU_SIGNON, 6, name)
/*********************************************************************
**    I_FUNCTION :  uz_zmotexit()
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* OBSOLETED BY ABILITY TO USE DONE KEY TO EXIT MODEL ICONS *
uz_zmotexit()
	{
	UD_ICMU_DN(MOTION_ICONS);
	ICON_UP = 0;

	return;
	}
*/
/*********************************************************************
**    I_FUNCTION :  uz_zmotion()
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uz_zmotion()
	{
	char us[100];

	if (UM_2d3d_mode == UM_3D)
		{
		if (ICON_UP == MODEL_ICONS)
			UD_ICMU_DN(MODEL_ICONS);
		UD_ICMU_UP(MOTION_ICONS);
		ICON_UP = MOTION_ICONS;

		ul_menu_reset();
		}
	else
		TWODERR("Motion");

	return;
	}
/*********************************************************************
**    I_FUNCTION :  uz_zncl_cmd()
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uz_zncl_cmd()
	{
	if (UM_2d3d_mode == UM_3D)
	{
#if UU_COMP == UU_WIN2K
		ud_prmerr(" ");
#endif
		ncl_cmd_mode();
	}
	else
		TWODERR("Command Mode");

	return;
	}

/*********************************************************************
**    I_FUNCTION :  uz_lathe_rough()
**			check lathe authorization for lathe rough
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uz_lathe_rough()
	{
	if(UL_lathe == 1) 
		nclu_lathe_rough(); 
	else 
		ud_wrerr ("Lathe option not authorized on this workstation");
	}

/*********************************************************************
**    I_FUNCTION :  uz_lathe_finish()
**			check lathe authorization for lathe finish
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uz_lathe_finish()
	{
	if(UL_lathe == 1) 
		nclu_lathe_finish(); 
	else 
		ud_wrerr ("Lathe option not authorized on this workstation");
	}

