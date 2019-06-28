/*********************************************************************
**    NAME         :  risp02a.c
**       CONTAINS:
**       uri_sp02a
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       risp02a.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:47
*********************************************************************/

#include "udebug.h"
#include "usysdef.h"
#include "ribase.h"

/*********************************************************************
**    I_FUNCTION     :  uri_sp02a(&key_id)
**       convert an key_id from absolute to relative
**    PARAMETERS   
**       INPUT  : 
**				key_id, the address of the key_id to convert
**       OUTPUT :  
**				key_id, the converted key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uri_sp02a(key_id) 
UU_KEY_ID	*key_id;		/* the key_id to convert	*/
{
	extern UU_LOGICAL	UR_sav_all;		/* boolean, UU_TRUE if save all		*/

	UU_KEY_ID		temp_key;		/* a temporary key_id			*/
	UR_TUPLE_INDX	abs_tuple_indx;/* the absolute entry id		*/
	UR_REL_NUM		rel_num;			/* the relation id				*/
	int				i,j;				/* indexs							*/
	long				max_index;		/* maximum to index into the bitmap	*/
	long				active_cnt;		/* number of actibe tuples 			*/
	unsigned long	bmw;				/* bitmap word */
	UU_LOGICAL		bit;
	long				bit_map_disp;	/* displacement to which map, ALLOC or SAVE*/

	uu_denter(UU_RTRC,(us,"uri_sp02a(0x%x)", *key_id));
	temp_key = *key_id;
	if(temp_key != 0)
	{
		ur_k2rt(temp_key, &rel_num, &abs_tuple_indx);
		if(rel_num <= UR_MAX_REL)
		{
			if((UR_rcb[rel_num].status < 0) ||
				(abs_tuple_indx > UR_rcb[rel_num].last_active_index))
			{
				/* illegal key -- clear it and return */
				*key_id = 0;
				uu_dexit;
				return;
			}
		}
		else
		{
			/* illegal key -- clear it and return */
			*key_id = 0;
			uu_dexit;
			return;
		}
		/* no need to relocate if relation is fully packed, no holes, and */
		/* we are saving all */
		if(UR_rcb[rel_num].last_active_index!=UR_rcb[rel_num].active_tuple_cnt ||
				!UR_sav_all)
		{
			if(UR_sav_all)
			{
				bit_map_disp = UR_ALLOC_MAP*UR_rcb[rel_num].bmap_size;
			}
			else
			{
				bit_map_disp = UR_SVLD_MAP*UR_rcb[rel_num].bmap_size;
			}
			/* need to find out how many active tuples exist before this tuple */
			max_index = abs_tuple_indx / UU_BITPW;
			active_cnt = 0;
			for(i=0; i < max_index; i++)
			{
				if(bmw = UR_rcb[rel_num].bmap_ptr[bit_map_disp+i])
				{
					/* non-zero word - some active tuples */
					if(~bmw == 0)
					{
						/* all ones - all active tuples */
						active_cnt += UU_BITPW;
					}
					else	/* only part active */
					{
						for(j=1; j<=UU_BITPW; j++)
						{
							if(bmw & 1)
							{
								active_cnt++;
							}
							bmw >>= 1;
						}
					}	/* partially active bitmap word */
				}	/* non-zero word */
			}
			/* now look at the word in the bit map that the tuple index is in */
			bmw = UR_rcb[rel_num].bmap_ptr[bit_map_disp+max_index];
			j = abs_tuple_indx - (max_index * UU_BITPW);
			for(i=1; i <= j; i++)
			{
				if(bmw & 1)
				{
					active_cnt++;
				}
				bmw >>= 1;
			}
			ur_rt2k(rel_num,active_cnt,&temp_key);
			uu_dprint(UU_RITRC,(us,"relocated 0x%x to 0x%x",*key_id,temp_key));
			*key_id = temp_key;
		}
	}
	uu_dexit;
	return;
}
