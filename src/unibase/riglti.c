/*********************************************************************
**    NAME         :  riglti
**       CONTAINS:
**       ur_get_last_tuple_index
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riglti.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:44
*********************************************************************/

#include	"usysdef.h"
#include "rbase.h"
#include	"ribase.h"
#include	"udebug.h"

/*********************************************************************
**  I_FUNCTION  :  status = ur_get_last_tuple_index(rel_num,&tuple_indx)
**      get the index for the last active tuple within a relation
**    PARAMETERS   
**       INPUT  : 
**			rel_num, relation in which last index is desired
**       OUTPUT :  
**			tuple_indx, the last active index
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     :  entries are numbered 1 - N
*********************************************************************/

ur_get_last_tuple_index(rel_num,tuple_indx)
UR_REL_NUM 		rel_num;			/* rel to find last active entry in	*/
UR_TUPLE_INDX	*tuple_indx;	/* the last active entry */
{
	int	status				;	/* status, -1 if error, 0 otherwise		*/

	uu_denter(UU_RITRC,(us,"ur_get_last_tuple_index(%d)", rel_num));
	status = 0;
	if(rel_num <= UR_MAX_REL )
	{
		if(UR_rcb[rel_num].status >= 0)
		{
			*tuple_indx = UR_rcb[rel_num].last_active_index;
		}
		else
		{
			/* inactive relation */
			*tuple_indx = 0;
			status  = -1;
		}
	}
	else
	{
		/* illegal relation specified */
		*tuple_indx = 0;
		status = -1;
	}
	uu_dprint(UU_RITRC,(us," found last tuple for rel = %d at %d",
					rel_num,*tuple_indx));
	uu_dexit ;
	return(status)	;
}
