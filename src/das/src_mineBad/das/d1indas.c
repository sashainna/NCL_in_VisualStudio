/*********************************************************************
**    NAME         :  d1indas.c
**       CONTAINS:
**       ud_init_das
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d1indas.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:03
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"

/*********************************************************************
**    I_FUNCTION     :  int ud_init_das()
**       Initialize DAS and GKS
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ud_init_das()
{
	uu_denter(UU_DTRC,(us, " entering ud_init_das"));

/*	-- init das and gks -- */

	ud_idev();
	ud_ogks();
	ud_idas();
	uu_dexit;
}
