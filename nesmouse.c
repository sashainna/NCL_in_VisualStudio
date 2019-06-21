/*********************************************************************
**
**    NAME         :  nesmouse.c
**
**       CONTAINS:
**				ncl_pan_toggle()
**				ncl_rotate_toggle()
**				ncl_domi_toggle()
**				ncl_gain_down()
**				ncl_gain_up()
**				ncl_gain_def()
**				ncl_toggle_spacemouse()
**
**    COPYRIGHT 2002 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesmouse.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:50
**			   
*********************************************************************/
#include "usysdef.h"
#define SPMOUSE_MAIN
#include "spmouse.h"
#undef SPMOUSE_MAIN

/*********************************************************************
**    E_FUNCTION : ncl_pan_toggle()
**		Turn on/off the translation 
**			
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pan_toggle()
{
	if (UV_SM_pan)
	{
		ud_prmerr("SpaceMouse dominate panning disabled.");
		UV_SM_pan = 0;
	}
	else
	{
		ud_prmerr("SpaceMouse dominate panning enabled.");
		UV_SM_pan = 1;
	}
}

/*********************************************************************
**    E_FUNCTION : ncl_rotate_toggle()
**		Turn on/off the rotation
**			
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_rotate_toggle()
{
	if (UV_SM_rotate)
	{
		UV_SM_rotate = 0;
		ud_prmerr("SpaceMouse dominate rotation disabled.");
	}
	else
	{
		UV_SM_rotate = 1;
		ud_prmerr("SpaceMouse dominate rotation enabled.");
	}
}

/*********************************************************************
**    E_FUNCTION : ncl_domi_toggle()
**		Turn on/off the dominate 
**			
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_domi_toggle()
{
	if (UV_SM_dom)
	{
		UV_SM_dom = 0;
		ud_prmerr("SpaceMouse dominate axis disabled.");
	}
	else
	{
		UV_SM_dom = 1;
		ud_prmerr("SpaceMouse dominate axis enabled.");
	}
}

/*********************************************************************
**    E_FUNCTION : ncl_gain_down()
**		Decrease the Sensitivity
**			
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_gain_down()
{
	char sbuf[80];
	UV_SM_sensi -= UV_SM_sensi*0.1;
	if (UV_SM_sensi<0.1) UV_SM_sensi = 0.1;
	sprintf(sbuf,"SpaceMouse sensitivity = %3.2f.",UV_SM_sensi);
	ud_prmerr(sbuf);
}


/*********************************************************************
**    E_FUNCTION : ncl_gain_up()
**		Increase the Sensitivity
**			
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_gain_up()
{
	char sbuf[80];
	UV_SM_sensi += UV_SM_sensi*0.1;
	if (UV_SM_sensi>2.0) UV_SM_sensi = 2.0;
	sprintf(sbuf,"SpaceMouse sensitivity = %3.2f.",UV_SM_sensi);
	ud_prmerr(sbuf);
}

/*********************************************************************
**    E_FUNCTION : ncl_gain_def()
**		Reset the Sensitivity to deffault value
**			
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_gain_def()
{
	char sbuf[80];
	UV_SM_sensi = 1.0;
	sprintf(sbuf,"SpaceMouse sensitivity = %3.2f.",UV_SM_sensi);
	ud_prmerr(sbuf);
}

/*********************************************************************
**    E_FUNCTION : ncl_toggle_spacemouse()
**		Toggle between using spacemouse for NCL and NCLIPV
**			
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_toggle_spacemouse()
{
	if (UV_SM_NCL)
	{
		UV_SM_NCL = 0;
		ud_prmerr("SpaceMouse enabled for NCLIPV.");
	}
	else
	{
		UV_SM_NCL = 1;
		ud_prmerr("SpaceMouse enabled for NCL.");
	}
}
