/*********************************************************************
**    NAME         :  micolor
**       CONTAINS:
**       um_ini_dispattr
**       um_sda1_geomclr
**       um_sda2_featclr
**       um_sda3_fillclr
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2icolor.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:47
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mdattr.h"
#include "mattr.h"
#include "mdrel.h"
#include "mxxx.h"
#include "mdebug.h"
#include "dmark.h"
#include "ginqst.h"

/*********************************************************************
**    E_FUNCTION     : um_ini_dispattr()
**       Initialize all of the global default um_display attributes.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ini_dispattr()

		{
	uu_denter( UU_MTRC,(us,"um_ini_dispattr()"));
	ur_put_dispattr_view_in(0);
	ur_put_dispattr_featclr(UM_WHITE);
	ur_put_dispattr_consclr(UM_GREEN);
 	ur_put_dispattr_fillclr(-1);
	uu_dexit;
		}
