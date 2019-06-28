
/*********************************************************************
**    NAME:  bimktype.c
**       CONTAINS:
**       	ubi_set_marker_type
**				ubi_reset_marker_type
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bimktype.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:03
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "gtbl.h"			/* for next include */
#include "ginqatt.h"		/* for gqmarktype */
#include "bsym.h"

static UU_LOGICAL old_marker_exists = UU_FALSE;
static int old_marker_type;
/*********************************************************************
**    E_FUNCTION :int ubi_set_marker_type(type) 
**       This function sets a marker type. 
**    PARAMETERS   
**       INPUT  : 
**          type		marker type index.
**       OUTPUT : 	none. 
**    RETURNS      : none
**    SIDE EFFECTS : Assigns UU_TRUE  to "marker_set".
**    WARNINGS     : none
*********************************************************************/
int ubi_set_marker_type(type)
	int type;
{
	int status;

	uu_denter(UU_BTRC, (us,"ubi_set_marker_type(type:%d,?)", type));
	status = UU_SUCCESS;	/* assume success */
	old_marker_type = gqmarktype();	/* get current marker type */
	gsmarktype(type);					/* set marker type to new type (circle) */
	old_marker_exists = UU_TRUE;
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION :int ubi_reset_marker_type() 
**       This function resets a marker type.
**    PARAMETERS   
**       INPUT  :	none. 
**       OUTPUT : none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_reset_marker_type()
{
	int status;

	uu_denter(UU_BTRC, (us,"ubi_reset_marker_type()"));
	status = UU_SUCCESS;	/* assume success */
	if (old_marker_exists)
	{
		gsmarktype(old_marker_type);	/* reset marker type to a circle */
		old_marker_exists = UU_FALSE;
	}
	uu_dexit;
	return(status);
}
