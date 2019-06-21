/********************************************************************* 
**
**    NAME         :  zssymmgt.c
**       CONTAINS:
**				uz_place_symbol()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zssymmgt.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:38
**
*********************************************************************/

#include "uhep.h"
#include "mdunits.h"

#define TWODERR(name) uu_uerror1(UU_SIGNON, 6, name)

/*********************************************************************
**    E_FUNCTION :  uz_place_symbol()
**       entry to place symbol
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_place_symbol()
{
	if(UM_2d3d_mode==UM_3D)
		ubu_instance_sym();
	else 
		TWODERR("Place Symbol");
}
