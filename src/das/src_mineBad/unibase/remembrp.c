/*********************************************************************
**    NAME         :  remembrp.c
**       CONTAINS:
**       ur_memberp()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       remembrp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:32
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) remembrp.c 3.2 3/8/88 09:44:17 single"};
#else
static char uu_sccsident[]={"@(#) remembrp.c 3.2 3/8/88 09:44:17 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"
#include "ribase.h"

/*********************************************************************
**    E_FUNCTION :  UU_LOGICAL ur_memberp(data_ptr, rel)
**		check if the data is a member of the relation given any leading
**		key is skipped
**    PARAMETERS   
**	   INPUT  : 
**		 data_ptr	UR_data*	 	pointer to a data packet
**		 rel			UR_REL_NUM	table to be checked
**	   OUTPUT :
**		data_ptr		UR_data*		the pointer is unchanged, but the key
**										field is filled in if a match is found
**										in order to report the match which was found.
**    RETURNS:		UU_TRUE if a match is found, UU_FALSE otherwise
**    SIDE EFFECTS:	none
**    WARNINGS:		expensive operation
*********************************************************************/

UU_LOGICAL ur_memberp(data_ptr, rel)
struct UR_data	*data_ptr;	/* pointer to data packet */
UR_REL_NUM		rel;			/* table to be checked */
{
	int				comp_size;	/* length to compare */
	int				differ;
	UU_LOGICAL		found;
	UR_TUPLE_INDX	tuple;
	struct UR_data	*ent_ptr;	/* pointer to relation entry */
	int				list;			/* varlist number */
	UR_lpacket		*lpack_ptr;	/* pointer to list packet within data packet */
	char				*atom_ptr;	/* pointer to varlist atoms */
	int				atom_cnt;	/* number of atoms in list */
	int				atom_size;	/* size of atoms in the list */

	uu_denter(UU_RTRC, (us, "ur_memberp(data=0x%x, rel=%d)", data_ptr, rel));
	comp_size = UR_rcb[rel].tuple_size - sizeof(UU_KEY_ID) - sizeof(UR_REL_NUM)
					- UR_rcb[rel].var_info_size;
	for (found = UU_FALSE, tuple = 1;
			!ur_get_next_tuple_index(rel, &tuple) && !found;
			tuple++)
	{
		ur_get_tuple_ptr(rel, tuple, &ent_ptr);
		differ = uu_comp_byte(ent_ptr->data, data_ptr->data, comp_size);
		if (!differ)
		{
			/* fixed data compares -- check varlists (if any) */
			for (found = UU_TRUE, list = 1;
					list <= UR_rcb[rel].n_varl && found; list++)
			{
				ur_get_varlist_ptr(rel, tuple, list, &atom_ptr, &atom_cnt);
				ur_get_list_packet_ptr(rel, list, data_ptr, &lpack_ptr);
				if (atom_cnt == lpack_ptr->atom_cnt)
				{
					ur_get_atom_size(rel, list, &atom_size);
					comp_size = atom_cnt * atom_size;
					differ = uu_comp_byte(atom_ptr, lpack_ptr->list_ptr, comp_size);
					if (differ)	/* failed varlist comparison */
					{
						found = UU_FALSE;
					}
				}
				else	/* failed varlist comparison */
				{
					found = UU_FALSE;
				}
			}
		}
	}
	if (found)
	{
		ur_rt2k(rel, tuple, &data_ptr->key_id);
	}
	uu_dexit;
	return(found);
}

