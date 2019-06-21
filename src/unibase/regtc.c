/*********************************************************************
**    NAME         :  regtc.c
**       CONTAINS:
**       ur_get_tuple_count()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       regtc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:31
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) regtc.c 3.1 9/22/87 17:36:50 single"};
#else
static char uu_sccsident[]={"@(#) regtc.c 3.1 9/22/87 17:36:50 double"};
#endif
#endif

#include "udebug.h"
#include "ribase.h"

/*********************************************************************
**    E_FUNCTION :  count = ur_get_tuple_count(rel)
**       returns a count of the number of active tuples for the
**			requested relation.
**    PARAMETERS   
**       INPUT  : 
**          rel		the relation in question
**       OUTPUT :  
**          none
**    RETURNS      : the count of active tuples in the relation
**							Note: zero will be returned from any uninitialized
**								or illegal relations.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_tuple_count(rel)
UR_REL_NUM	rel;
{
	if ((rel <= UR_MAX_REL) && (UR_rcb[rel].status >= 0))
	{
		return(UR_rcb[rel].active_tuple_cnt);
	}
	else
	{
		uu_denter(-1,(us,"ERROR:get tuple count illegal or inactive relation."));
		return(0);
	}
}

