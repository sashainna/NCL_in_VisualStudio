/*********************************************************************
**    NAME         :  m3udaxis.c
**       CONTAINS:
**			umu_set_mod_axis(option)
**			umu_crv_modals(rel_num)
**			umu_srf_modals()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m3udaxis.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:58
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdrel.h"
#include "modef.h"
#include "mdattr.h"
#include "mattr.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "udforms.h"
#include "udfdata.h"

extern struct {
	UU_KEY_ID modaxiskey; 			/* UNIBASE key of model axis */
	UU_REAL length;					/* length of axis */
	int color;							/* color of axis */
	}  UM_Mod_axis;

/*********************************************************************
**    E_FUNCTION     : umu_set_mod_axis(option)
**      Change the length of the model axis.
**    PARAMETERS   
**       INPUT  : 
**          option				1 => set model axis length
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umu_set_mod_axis(option)
int option;
	{
	UU_REAL length;						/* length of model axis */
	int numint;								/* number of DAS values returned */

	uu_denter( UU_MTRC,(us,"umu_set_mod_axis()"));
	if (option == 1)
		{
		ud_ldas(UD_DASDISTANCE,/*axis length*/UM_MODEL,28,&length,
				 1,&numint,UD_NODEFAULT);
		if (numint > 0)  UM_Mod_axis.length = length;
		UM_Mod_axis.color =  UM_RED;
		}
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_crv_modals(rel_num)
**      Change the display modals for curves.
**    PARAMETERS   
**       INPUT  : 
**				rel_num						type of curve
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umu_crv_modals(rel_num)
int rel_num;
	{
	UU_REAL err;							/* error value */
	int numint;								/* number of DAS entries */

/*---------------------------------------------------------------
**  Start of Executable Code
**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"umu_crv_modals(%d)",rel_num));
	switch (rel_num)
		{
		case  UM_CIRCLE_REL:
			err =  UM_crvattr.relcirerr;
			ud_ldas(UD_DASUNITLESS,/*drawing error (% of radius)*/UM_MODEL,187,
				&err,1,&numint,UD_DEFAULT);
			if ((err < UM_FUZZ) || (err > 1.0))
				ud_wrerr("drawing error outside of legal limits");
			else
				 UM_crvattr.relcirerr = err;
			break;
		default:
			ud_wrerr("umu_crv_modals: illegal curve relation");
			break;
		}
	uu_dexit;

	return (0);
	}
