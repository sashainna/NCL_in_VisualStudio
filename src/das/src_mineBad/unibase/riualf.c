#define MODULEDBG 1
/*********************************************************************
**    NAME         :  riualf.c
**       CONTAINS:
**       uri_update_assoc_list_fixed()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riualf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:48
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "riddldef.h"
#include "rbase.h"
#include "ribase.h"
#include "rmtuple.h"

/*********************************************************************
** I_FUNCTION : uri_update_assoc_list_fixed(o_data_ptr, n_data_ptr)
**      update association back keys for new data
**    PARAMETERS   
**       INPUT  : 
**				&n_data_ptr, address of data packet to update with
**				n_data_ptr.key_id, must equal key of data to update
**				n_data_ptr.rel_num, must equal relation number of data
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     :	data can only be updated if it has been previous 
**							created
*********************************************************************/

uri_update_assoc_list_fixed(o_data_ptr, n_data_ptr)
struct UR_data	*o_data_ptr;	/* pointer to old data */
struct UR_data	*n_data_ptr;	/* pointer to new data */
{
	UU_KEY_ID		*old_key, *new_key;	/* ptrs to embedded keys in data */
	UR_REL_NUM		rel;					/* the relation to update */
	UR_TUPLE_INDX	tuple;				/* index of the tuple to update */
	struct attr_def	*atdefs;	/* attribute definitions */
	int				num_attr;			/* number of attributes parsed */
	int				rel_typ;
	int				i;
	int			j, k;
	int			cndx, rndx;

	uu_denter(UU_RITRC,(us,
			"uri_update_assoc_list_fixed(old data @0x%x, new data @0x%x)",
			o_data_ptr, n_data_ptr));

#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us,"old data:"));
	ur_dump_tuple(o_data_ptr->rel_num, o_data_ptr);
	uu_dprint(UU_RITRC,(us,"new data:"));
	ur_dump_tuple(n_data_ptr->rel_num, n_data_ptr);
#endif
	num_attr = UR_rcb[n_data_ptr->rel_num].rel_def->rellen;
	atdefs = UR_rcb[n_data_ptr->rel_num].relattr;

	/* for each embedded mtuple key if old data != new data remove */
	/* this association tuple from the back list of old key and add */
	/* it to the new data's key */
	for (i = 1; i < num_attr; i++)
	{
		/* if master key */
		if ((atdefs[i].attr_type == KEY_ID)
			|| (atdefs[i].attr_type == REL_ID))
		{
			new_key = (UU_KEY_ID *)((char *)n_data_ptr
								+ atdefs[i].attr_here);
			old_key = (UU_KEY_ID *)((char *)o_data_ptr
								+ atdefs[i].attr_here);
			/* set array indexs, non array has 1 row, 1 col */
			rndx = atdefs[i].num_rows;
			cndx = atdefs[i].num_cols;
			for(j = 1; j <= cndx; j++)
			{
				for(k = 1; k <= rndx; k++)
				{
					if (*new_key != *old_key)
					{
#if MODULEDBG != 0
						uu_dprint(UU_RITRC,(us,
								"old key 0x%x at 0x%x new key 0x%x at 0x%x",
								*old_key, old_key, *new_key, new_key));
#endif
						ur_k2rt(*old_key, &rel, &tuple);
						if (rel == UR_MTUPLE_REL && tuple)
						{
							/* delete from old_key's back list */
							uri_del_assoc_list(*old_key, n_data_ptr->key_id);
						}
						ur_k2rt(*new_key, &rel, &tuple);
						if (rel == UR_MTUPLE_REL && tuple)
						{
							/* add to new_key's back list */
							uri_add_assoc_list(*new_key, n_data_ptr->key_id);
						}
					}
					new_key++;
					old_key++;	/* bump ptrs to next key */
				}
			}
		}
	}
	uu_dexit;
	return(0);
}

