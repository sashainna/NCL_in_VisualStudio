/*********************************************************************
**    NAME         :  nusystem.c
**       CONTAINS: User interface routines for system routines.
**			nclu_system()
**			nclu_edt()
**     MODULE NAME AND RELEASE LEVEL 
**       nusystem.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:16
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "driver.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mdcoord.h"
#include "modef.h"

#include "nccs.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "nclcmd.h"
#include "nclmodals.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : nclu_system()
**       description
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_system()
	{
	NCL_cmdbuf cmdbuf;

	uu_denter(UU_MTRC,(us,"nclu_system()"));

	ncl_init_cmdbuf(&cmdbuf);

	ncl_add_token(&cmdbuf, NCL_system, NCL_nocomma);

	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_edt()
**       description
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_edt()
	{
	NCL_cmdbuf cmdbuf;

	uu_denter(UU_MTRC,(us,"nclu_edt()"));

	ncl_init_cmdbuf(&cmdbuf);

	ncl_add_token(&cmdbuf, NCL_edit, NCL_nocomma);

	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);

	uu_dexit;
	}
