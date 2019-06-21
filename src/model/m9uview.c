
/*********************************************************************
**    NAME         :  m9uview
**       CONTAINS: user interface for switching between subsystems
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m9uview.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:14
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mxxx.h"
#include "view.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     : umu_view_drawing()
**       Prompt the user for a drawing name, and display the drawing
**			in a single viewport.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_view_drawing()

	{
	int status;
	int numint;
	struct UM_drawing_rec drawing;

	uu_denter(UU_MTRC,(us,"umu_view_drawing()"));

	ud_ldas( UD_DASSTRING, /* Enter drawing name */ UM_MODEL, 227,
			drawing.name, 16,        /* buffer and size */
			&numint, UD_NODEFAULT );
	if (numint > 0)
		{
		status = um_key_from_drawing_name(drawing.name, &drawing.key);
		if (status != 0 )
			{
			uu_uerror1(/* drawing does not exist */ UM_MODEL, 203, drawing.name);
			}
		else
			{
			um_get_all_geom(&drawing, sizeof(struct UM_drawing_rec));
			um_view_drawing(&drawing);
			}
		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_view_model()
**			Change into model viewing mode.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_view_model()

	{

	uu_denter(UU_MTRC,(us,"umu_view_model()"));
	um_view_model();
	uu_dexit;
	}


