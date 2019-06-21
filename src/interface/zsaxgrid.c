/*********************************************************************
**
**    NAME         :  zsaxgrid.c
**
**       CONTAINS:
**    		uz_chg_wp_axis()
**    		uz_swap_wp_axis()
**    	  	uz_rot_wp_axis()
**    	  	uz_act_grid()
**    	  	uz_inact_grid()
**          uz_wp_to_plane()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zsaxgrid.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:37
**
*********************************************************************/

#include "uhep.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "nclfc.h"
#include "mfort.h"

#define TWODERR(name) uu_uerror1(UU_SIGNON, 6, name)
/*********************************************************************
**    E_FUNCTION :  uz_chg_wp_axis()
**       change working plane axis
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_chg_wp_axis()
{
/* added for refsys. kathy */
UM_int2 ivl1,ivl2;
char label[80];
int flag;

	if(UM_2d3d_mode==UM_3D)
	{
		umu_align_cpln(&flag, label);
		if (flag==0)
			return;
		if (UU_application == UU_NCLCAM)
			{
			ivl1=2;ivl2=2;
			setins(&ivl1,&ivl2); 
			nclu_refsys(1, flag+1, label);
			ivl1=1;ivl2=0; 
			setins(&ivl1,&ivl2);
			}
		}
	else 
		TWODERR("Change WP Axis");
}

/*********************************************************************
**    E_FUNCTION :  uz_swap_wp_axis()
**       swap working plane axis
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_swap_wp_axis()
{
/* added for refsys. kathy */
UM_int2 ivl1,ivl2;

	if(UM_2d3d_mode==UM_3D)
		{
		umu_snap_cpln();
		if (UU_application == UU_NCLCAM)
			{
			ivl1=2;ivl2=2;
			setins(&ivl1,&ivl2); 
			nclu_refsys(4,0,0);
			ivl1=1;ivl2=0; 
			setins(&ivl1,&ivl2);
			}
		} 
	else TWODERR("Swap WP Axis");
}

/*********************************************************************
**    E_FUNCTION :  uz_wp_to_plane()
**       swap working plane normal
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_wp_to_plane()
{
/* added for refsys. kathy */
UM_int2 ivl1,ivl2;

/*
	if (UU_application == UU_NCLCAM)
		uu_uerror1(UA_NCL, 1, "Snap WP to PLANE");
*/

	if(UM_2d3d_mode==UM_3D)
	{
		umu_snap_cpln_to_plane();
		if (UU_application == UU_NCLCAM)
		{
			ivl1=2;ivl2=2;
			setins(&ivl1,&ivl2); 
			nclu_refsys(4,0,0);
			ivl1=1;ivl2=0; 
			setins(&ivl1,&ivl2);
		}
	}
	else TWODERR("Snap WP to PLANE");
}

/*********************************************************************
**    E_FUNCTION :  uz_rot_wp_axis()
**       rot working plane axis
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_rot_wp_axis()
{
/* added for refsys. kathy */
UM_int2 ivl1,ivl2;

	if(UM_2d3d_mode==UM_3D)
		{
		umu_rot_cpln();
		if (UU_application == UU_NCLCAM)
			{
			ivl1=2;ivl2=2;
			setins(&ivl1,&ivl2); 
			nclu_refsys(4,0,0);
			ivl1=1;ivl2=0; 
			setins(&ivl1,&ivl2);
			}
		}
	else 
		TWODERR("Rotate WP Axis");
}

/*********************************************************************
**    E_FUNCTION :  uz_act_grid()
**       activate grid
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_act_grid()
{
	umu_gridfrm();
	if (UM_cpln.grid.snap)
		uz_actgrid("Grid ON");
	else
		uz_actgrid("Grid OFF");
}

/*********************************************************************
**    E_FUNCTION :  uz_inactgrid()
**       deactivate grid
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_inact_grid()
{
	umu_inactgrid();
	uz_actgrid("Grid OFF");
}

/*********************************************************************
**    E_FUNCTION :  uz_setcpln()
**       reset WP, align WP-VP, CHG WP ORIG
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_setcpln(cpln, refsys)
int cpln, refsys;
	{
	/* added for refsys. kathy */
	UM_int2 ivl1,ivl2;
	char orig_label[80];

	umu_setcpln(cpln, orig_label); 
	if (UU_application == UU_NCLCAM)
		{
		ivl1=2;ivl2=2;
		setins(&ivl1,&ivl2); 
		nclu_refsys(refsys, 1, orig_label); 
		ivl1=1;ivl2=0; 
		setins(&ivl1,&ivl2);
		}

	return;
	}
