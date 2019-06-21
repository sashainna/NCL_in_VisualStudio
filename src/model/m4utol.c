/*********************************************************************
**    NAME         :  m4utol.c
**       CONTAINS: user interface routines to set AG tolerances.
**			umu_ag_set_tolerance(option)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4utol.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:05
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "class.h"
#include "mdebug.h"

#include "ag_incl.h"
#include "ag_global.h"

/*********************************************************************
**    E_FUNCTION     : umu_ag_set_tolerance(option)
**			Let user pick tolerance to change from popup menu, get new
**			value, and then change it and dependent tolerances in AG.
**    PARAMETERS   
**       INPUT  : 
**				option				one of AG tolerance definitions
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_ag_set_tolerance(option)
	int option;

	{
	int  numint;
	int status;
	UU_REAL tol;

	uu_denter(UU_MTRC,(us,"umu_ag_set_tolerance(option=%d)", option));

	switch (option)
		{
		case 1:
			tol = AG_tol_dist;
			break;
		case 2:
			tol = AG_tol_ortho;
			break;
		case 3:
			tol = AG_tol_knot;
			break;
		case 4:
			tol = AG_tol_mach;
			break;
		case 5:
			tol = AG_tol_cfit;
			break;
		case 6:
			tol = AG_tol_sfit;
			break;
		case 7:
			break;
		default:
			goto done;
		}

	ud_ldas(UD_DASUNITLESS,/* enter new tolerance */ UM_APPGEO, 48,
		&tol, 1, &numint, UD_DEFAULT);
	if (numint < 1) goto done;

	switch (option)
		{
		case 1:
			AG_tol_dist = tol;
			AG_tol_dist2 = tol * tol;
			break;
		case 2:
			AG_tol_ortho = tol;
			break;
		case 3:
			AG_tol_knot = tol;
			break;
		case 4:
			AG_tol_mach = tol;
			break;
		case 5:
			AG_tol_cfit = tol;
			break;
		case 6:
			AG_tol_sfit = tol;
			break;
		case 7:
			break;
		default:
			goto done;
		}

done:;
	uu_dexit;
	}

