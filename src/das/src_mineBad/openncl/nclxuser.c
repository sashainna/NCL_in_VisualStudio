/*********************************************************************
**    NAME         :  nclxuser.c
**       CONTAINS:
**				int NclxLaunchRoutine(name,narg,args)
**				int NclxGetRoutine(name)
**       names of functions in file
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nclxuser.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:10:58
*********************************************************************/

#include "nclx.h"
#include "nclxuser.h"

/*********************************************************************
**    E_FUNCTION     : int NclxLaunchRoutine(name,narg,args)
**       Buffer routine which controls the calling of user defined
**			OpenNCL routines.
**    PARAMETERS
**       INPUT  :
**          name              Name of routine to call.
**          narg              Number of arguments to pass to routine.
**          args              Argument list to pass to routine.
**       OUTPUT :
**          none.
**    RETURNS      :
**       NCLX_SUCCESS if no error; else NCLX_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxLaunchRoutine(name,narg,args)
int narg;
char *name,args[35][64];
{
	int stat,irtn;
	stat = NCLX_SUCCESS;
	irtn = NclxGetRoutine(name);
	if (irtn >= 0)
	{
/*
.....Place switch statement here
*/
		switch (irtn)
		{
		case 0:
			my_drive_motion(narg,args);
			break;
		case 1:
			my_pick_id(narg,args);
			break;
		case 2:
			my_surf_eval(narg,args);
			break;
		case 3:
			my_scrub(narg,args);
			break;
		case 4:
			my_rmill(narg,args);
			break;
		case 5:
			my_patern_rec(narg,args);
			break;
		case 6:
			my_polyline_rec(narg,args);
			break;
		case 7:
			my_apocket(narg,args);
			break;
		case 8:
			my_shape_rec(narg,args);
			break;
		case 9:
			my_lathe_rough(narg,args);
			break;
		case 10:
			my_lathe_finish(narg,args);
			break;
		case 11:
			my_gofwd_auto(narg,args);
			break;
		case 12:
			ncl_test_catia();
			break;
		}
	}
	else
	{
		stat = NCLX_FAILURE;
	}
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxGetRoutine(name)
**       Verify that the user supplied OpenNCL routine exists.
**    PARAMETERS
**       INPUT  :
**          name              Name of routine.
**       OUTPUT :
**          none.
**    RETURNS      :
**       -1 if routine is not found, otherwise the placement of the
**			routine in the NCLX_user_routines array.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxGetRoutine(name)
char *name;
{
	int i;
	for (i=0;i<NCLX_NUM_ROUTINE;i++)
	{
		if (strcmp(name,NCLX_user_routines[i]) == 0) break;
	}
	if (i >= NCLX_NUM_ROUTINE) i = -1;
	return(i);
}
