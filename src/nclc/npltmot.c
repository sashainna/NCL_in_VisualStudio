/*********************************************************************
**    NAME         :  npltmot.c
**       CONTAINS:
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       npltmot.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:02
*********************************************************************/
#include "usysdef.h"
#include "zsysdep.h"
#include "udebug.h"
#include <math.h>
#include "gsegop.h"
#include "gsegac.h"
#include "mdrel.h"
#include "class.h"
#include "mcrv.h"
#include "mdcoord.h"
#include "mdraw.h"
#include "mdattr.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mxxx.h"
#include "view.h"
#include "ulist.h"
#include "gviw.h"
#include "gtbl.h"


/*********************************************************************
**    E_FUNCTION     : int ncl_proj_polyline_to_plane(gpt, n, point, normal, neweptr)
**			Project a polyline curve onto a plane
**    PARAMETERS   
**       INPUT  : 
**				point						point defining plane
**				normal					normal defining plane
**       OUTPUT :  
**          neweptr					projected entity
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_proj_polyline_to_plane(gpt, n, point, normal, neweptr)
Gwpoint3 *gpt;
int n;
UM_coord point;
UM_vector normal;
struct UM_polyline_rec *neweptr;
{
	int i,j;
	UM_coord pt;

	ur_setup_app_data(UM_POLYLINE_REL, neweptr, sizeof(struct UM_polyline_rec));
	neweptr->no_pt = 0;
	for (i=0; i<n; i++)
	{
		um_nptpln(&(gpt[i]), point, normal, pt);
		ur_update_app_data_varlist(neweptr, 1, pt, neweptr->no_pt+1, 1);
	}
	return (UU_SUCCESS);
}

