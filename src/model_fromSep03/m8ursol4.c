
/*********************************************************************
**    NAME         :  m8ursol4.c
**       CONTAINS:
**			umu_gromgeom
**			umu_cmdin
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m8ursol4.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:11
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mromcom.h"
#include "mfcifdef.h"

/*********************************************************************
**    E_FUNCTION     : umu_gromgeom()
**      Test routine to retrieve ROMULUS curve geometry given an edge.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_gromgeom()
	{
	UM_PICKENT	pent;										/* pick path to picked entity */
	struct UM_crvdatabag e;								/* UNICAD entity */
	int numint;

	uu_denter( UU_MTRC,(us,"umu_gromgeom()"));
	um_dl_pdas(UD_DASPICK, /*pick an edge*/UM_MODEL, 152, &pent, 1, &numint, 2);
	e.key = um_get_pickkey(&pent, 2);
	if (numint != 0)
		{
		um_ret_romgeom(e.key, &e.rel_num, &e);
		uc_print(&e);
		}
	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION     : umu_cmdin()
**      Read a ROMULUS command and pass it to ROMULUS.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_cmdin()
	{
	int numint;
	char cmd[120];						/* ROMULUS commmand */

	uu_denter( UU_MTRC,(us,"umu_cmdin()"));
	while (UU_TRUE)
		{
		ud_ldas(UD_DASSTRING,  UM_MODEL, 50, cmd, 120, &numint, UD_NODEFAULT);
		if (numint <= 0) break;
		um_romulus(cmd);
		}
	uu_dexit;
	}
