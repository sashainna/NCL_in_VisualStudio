/*********************************************************************
**    NAME         :  alindata.c
**       CONTAINS:
**			ua_get_linedata
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       alindata.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:35
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mcrv.h"
/*********************************************************************
**    I_FUNCTION :  ua_get_linedata(key,spt,ept)
**       Get information about line entity.
**    PARAMETERS   
**       INPUT  : 
**				key			unibase key of entity
**       OUTPUT :  
**				spt			start point or line in model coord
**				ept			end point of line in model coord
**    RETURNS      : UU_FAILURE if any routine called fails
**							UU_SUCCESS if all OK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_get_linedata(key,spt,ept)
UU_KEY_ID		key;					/* arc entity key */
UM_coord		spt,ept;				/* start end points */
{
	int			status;
	struct		UM_line_rec	e;
	UM_transf	tfmat;
	/*----------- begin function code -----------------------------*/
	uu_denter(UU_STRC,(us,"ua_get_linedata(key:%d)",key));
	e.key = key;
	status = um_get_all_geom(&e,sizeof(struct UM_line_rec));
	if (status!=UU_SUCCESS)
		return(status);
	um_vctovc(e.spt,spt);					/* line start point */
	um_vctovc(e.ept,ept);					/* line end point  */
	uu_dexit;
}
