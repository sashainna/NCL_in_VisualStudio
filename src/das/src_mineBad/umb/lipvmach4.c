/*********************************************************************
**    NAME         :  lipvmach4.c
**       CONTAINS:
**				ul_ipv_mach_disable()
**				ul_ipv_mach_enable()
**    COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvmach4.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       07/28/15 , 11:12:14
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "lipv.h"
#include "lipvmach.h"
#include "nclmplay.h"
#include "xenv1.h"
#include "xfsys1.h"

/*********************************************************************
**    E_FUNCTION     : ul_ipv_mach_disable()
**       Disables the machine from being used in simulation.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mach_disable()
{
	UU_LOGICAL lmod;
	ul_ipv_destroy_assembly();
	ul_ipv_place_stock(UU_NULL,UU_TRUE,&lmod);
	LW_mach_simul = UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_mach_enable()
**       Enables the machine for simulation.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mach_enable()
{
	UU_LOGICAL lmod;
	LW_mach_simul = UU_TRUE;
	ul_ipv_define_assembly();
	ul_ipv_create_assembly();
	ul_ipv_place_stock(UU_NULL,UU_FALSE,&lmod);
}
