/*********************************************************************
**    NAME         :  ricfk.c
**       CONTAINS:
**       uri_clear_fixed_keys()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ricfk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:41
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) ricfk.c 3.2 3/3/88 10:02:07 single"};
#else
static char uu_sccsident[]={"@(#) ricfk.c 3.2 3/3/88 10:02:07 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "riddldef.h"
#include "rbase.h"
#include "ribase.h"

/*********************************************************************
**    I_FUNCTION :  uri_clear_fixed_keys(data)
**       function to clear(initialize) the keys in the fixed data
**			portion of the record passed as 'data'
**    PARAMETERS   
**       INPUT  : 
**          data	UR_data *	data packet to have keys cleared
**										rel_num field must be filled in
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : associative keys in the fixed data portion of
**							the record are initialized to zero (an illegal value)
**    WARNINGS     : none
*********************************************************************/

uri_clear_fixed_keys(data)
struct UR_data	*data;
{
	UU_KEY_ID	*key_ptr;
	int			i, j, k;
	int			cndx, rndx;
	int			rel_typ;
	int			num_attr;
	struct attr_def	*atdefs;	/* attribute definitions */

	uu_denter(UU_RITRC,(us,"uri_clear_fixed_keys(0x%x)", data));
	if (uri_assocp(data->rel_num))
	{
		num_attr = UR_rcb[data->rel_num].rel_def->rellen;
		atdefs = UR_rcb[data->rel_num].relattr;
		for (i = 1; i < num_attr; i++)
		{
			if ((atdefs[i].attr_type == KEY_ID)
				|| (atdefs[i].attr_type == REL_ID))
			{
				key_ptr = (UU_KEY_ID *)((char *)data + atdefs[i].attr_here);

				/* set array indexs, non array has 1 row, 1 col */
				rndx = atdefs[i].num_rows;
				cndx = atdefs[i].num_cols;
				for(j = 1; j <= cndx; j++)
				{
					for(k = 1; k <= rndx; k++)
					{
						*key_ptr++ = 0;	/* set key to zero and bump ptr to next */
					}
				}
			}
		}
	}
	uu_dexit;
}

