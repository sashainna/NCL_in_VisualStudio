
/*********************************************************************
**    NAME         :  m2upjpln.c
**       CONTAINS: user interface routines for projecting entities
**							to a plane.
**			umu_project_to_plane()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2upjpln.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:50
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "dasnog.h"
#include "dselmask.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdcpln.h"
#include "mdpick.h"

/*********************************************************************
**    E_FUNCTION     : umu_project_to_plane()
**			Prompt the user to pick entities to project to a plane.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_project_to_plane()

	{
	int status;
	int numint;
	UM_PICKENT pent;
	struct UC_entitydatabag *e;
	struct UC_entitydatabag *newe;
	UD_PLANE_REC plane;
	UU_LOGICAL initialize;
	char *uu_malloc();

	uu_denter(UU_MTRC,(us,"umu_project_to_plane()"));

	/* get plane to project entities onto */
	um_vctovc(UM_cpln.origin, plane.org);
	um_vctovc(UM_cpln.zaxis, plane.normal_vector);
	ud_get_plane(/* define plane to project onto */UM_MODEL, 321,
		&plane, 1, &numint, UD_DEFAULT);
	/*if (numint < 1) goto done;*/
	um_unitvc(plane.normal_vector, plane.normal_vector);

	/* limit pickable entities to those which can be projected to a plane */
	ud_lgeo(UU_TRUE, UD_proj_to_plane);

	e = (struct UC_entitydatabag *)uu_malloc(sizeof(struct UC_entitydatabag));
	newe = (struct UC_entitydatabag *)uu_malloc(sizeof(struct UC_entitydatabag));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASSELECT, /*Pick entity to project onto plane.*/
				UM_MODEL, 318, UU_NULL, 0, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		initialize = UU_TRUE;
		while (ud_gnxt(initialize, UU_NULL, &e->key, 1) == UU_TRUE)
			{
			initialize = UU_FALSE;
			status = uc_retrieve_data(e, sizeof(struct UC_entitydatabag));			
			if (status == UU_SUCCESS)
				status = uc_project_to_plane(e, plane.org, plane.normal_vector,
														 newe);
			if (status == UU_SUCCESS)
				status = uc_create_data(newe, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			if (status == UU_SUCCESS)
				uc_display(newe);
			}
		}

done:;
	uu_free(e);
	uu_free(newe);
	uu_dexit;
	}

