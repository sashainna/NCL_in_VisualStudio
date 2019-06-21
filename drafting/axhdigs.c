
/*********************************************************************
**    NAME         :  axhdigs.c
**       CONTAINS:
**				SAL Interfaces to DIGS routines needed for cross hatching
**			ua_xh_entity_box()
**
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       axhdigs.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:43
*********************************************************************/
#include	"usysdef.h"
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) axhdigs.c 2.3 2/10/87 10:22:13 single"};
#else
static char uu_sccsident[]={"@(#) axhdigs.c 2.3 2/10/87 10:22:13 double"};
#endif

#include	"udebug.h"
#include	"mdebug.h"
#include	"umath.h"
#include "gobas.h"
#include "gsegac.h"

#define BIGCOORD (UU_REAL) 10.0e+20

/*********************************************************************
**    E_FUNCTION :  ua_xh_entity_box(key, box)
**			Get DIGS extrema box for given entity.
**    PARAMETERS   
**       INPUT  : 
**				int		key	-	geometry key
**       OUTPUT :  
**         	UU_REAL	box[2][3] -	answer
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_xh_entity_box(key, box)
	int		key;
	UU_REAL	box[2][3];
	{
	int i, ret_val, segid;
	Gwrect3  rect;
	UG_segstli  *p;

	uu_denter(UU_STRC,(us,"ua_xh_entity_box(key %d)", key));
	ret_val = -1;
	ur_retrieve_disp_segid(key, &segid);
	if(segid != NULL)
		{
		p = ug_segac(segid);
		if(p != NULL)
			{
			/* set returned box to undefined. */
			rect.llf.x=BIGCOORD;
			rect.llf.y=BIGCOORD;
			rect.llf.z= -BIGCOORD;
			rect.urb.x= -BIGCOORD;
			rect.urb.y= -BIGCOORD;
			rect.urb.z=BIGCOORD;
			i = ug_segextrema(p, &rect);
			box[0][0] = rect.llf.x;
			box[0][1] = rect.llf.y;
			box[0][2] = rect.llf.z;
			box[1][0] = rect.urb.x;
			box[1][1] = rect.urb.y;
			box[1][2] = rect.urb.z;
			ret_val = 0;
			}
		}
	uu_dexit;
	return (ret_val);
	}
