/*********************************************************************
**    NAME         :  virel.c
**       CONTAINS: Initialize viewing relations.
**			uv_init_viewing_rel()
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**   MODULE NAME AND RELEASE LEVEL 
**       virel.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:59
**************************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]=	{"@(#) virel.c 2.4 2/18/87 16:06:47 single"	};
#else
static char uu_sccsident[]=	{"@(#) virel.c 2.4 2/18/87 16:06:47 double"	};
#endif
#include "usysdef.h"
#include "udebug.h"
#include "view.h"
#include "mdrel.h"

/**************************************************************************
**  I_FUNCTION:  int uv_init_viewing_rel()
**			Initialize viewing relations. These are defined to be NON MASTER
**			TUPLE relations.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int uv_init_viewing_rel()
	{
	int atom_size[1];					/* size of atom on variable length lists */
	int list_size[1];					/* initial size/expansion factor for lists */

	uu_denter(UU_MTRC,(us,"uv_init_viewing_rel()"));

	/* init view relation */
	atom_size[0] = 0;
	ur_init_data_rel(UV_VIEW_REL, "viewdef", 25,
				   sizeof(UV_view), 0, atom_size, list_size);

	/* init viewport relation */
	atom_size[0] = 0;
	ur_init_data_rel(UV_VPORT_REL, "vport", 25,
				   sizeof(UV_vport), 0, atom_size, list_size);

	/* init screen relation */
	atom_size[0] = 0;
	ur_init_data_rel(UV_SCREEN_REL, "screen", 10,
				   sizeof(UV_screen), 0, atom_size, list_size);

	uu_dexit;
	return(UU_SUCCESS);
	}
