/*********************************************************************
**    NAME         :  rilp02a.c
**       CONTAINS:
**       uri_lp02a()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rilp02a.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:44
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include	"udebug.h"

/*********************************************************************
**    I_FUNCTION     :  uri_lp02a(&key_id,&bit_map)
**		convert a relative key_id imbedded in data to absolute key,
**		a support routine for relp02
**    PARAMETERS   
**       INPUT  : 
**          key_id, the address of the relative key_id to convert
**			  bit_map, address of relative bitmap to use in the conversion
**       OUTPUT :  
**          key_id, the converted absolute key_id
**    RETURNS      : 0 if legal key, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

	uri_lp02a(key_id,bmap_ptr)
	UU_KEY_ID	*key_id		;	/* the key_id to convert			*/
	unsigned long	*bmap_ptr;	/* pointer to relative(loaded) bit map		*/
	{
		UU_KEY_ID	temp_ident;		 /* a temporary key_id					*/
		long			rel_tuple_indx; /* a relative entry id				*/
		long			abs_tuple_indx; /* the absolute entry id				*/
		long			inactive_cnt;
		long			rel_num;			 /* the relation id						*/
		int			i,j;				 /* index									*/
		UU_LOGICAL	bit;
		long			bit_cnt;			 /* num of inactive bits within a wrd */
		UU_LOGICAL	hit_word;		 /* true when we found word index	*/
		unsigned	long	temp;			 /* holds a bit map word				*/

		uu_denter(UU_RTRC,(us,"uri_lp02a(key:0x%x, bmap:0x%x)", *key_id,bmap_ptr));
		temp_ident = *key_id;
		if(temp_ident != 0)	/* attempt only for non-zero keys */
		{
			ur_k2rt(temp_ident,&rel_num,&rel_tuple_indx) ;
			if(rel_num > UR_MAX_REL || UR_rcb[rel_num].status < 0)
			{
				/* illegal key -- clear it and return */
				*key_id = 0;
				uu_dexit ;
				return(-1) ;
			}
			/* need to relocate by figuring out how many unused tuples */
			/* exist before the relative index.  */
			/* count the inactive tuples that exist in the bitmap */
			/* up to the word that the tuple index should exist in */
			inactive_cnt = 0;
			i = 0;
			hit_word = UU_FALSE;
			while(!hit_word)
			{
				if(i >= UR_rcb[rel_num].bmap_size || bmap_ptr[i] == 0)
				{
					/* if bit map word all 0's add size of word to count */
					if(inactive_cnt + UU_BITPW < rel_tuple_indx) 
					{
						inactive_cnt += UU_BITPW;
						i++;
					}
					else
						hit_word = UU_TRUE;
				} /* bit map word == 0 */
				else if(~bmap_ptr[i] == 0)
				{
					/* if bit map word is all 1's, then just skip this word */
					i++;
				}
				else	/* (not all 0's or 1's) */
				{
					/* find out how many inactive bits within the word and */
					/* whether it would put us at the rel_tuple_indx */
					temp = bmap_ptr[i];
					bit_cnt = 0;
					for(j=1; j <= UU_BITPW; j++)
					{
						bit = (temp & 1);
						temp >>= 1;
						if(!bit)
						{
							bit_cnt++;
						}
					}
					if(inactive_cnt + bit_cnt < rel_tuple_indx) 
					{
						inactive_cnt += bit_cnt;
						i++;
					}
					else
						hit_word = UU_TRUE;
				} /* bit map word != 0 */
			}	/* !hit_word	*/
			/* we now have the word that the index will be found in, count */
			/* the actual number of inactive tuples to correctly place it */
			abs_tuple_indx = i*UU_BITPW; /* number of tuples before word*/
			if(i < UR_rcb[rel_num].bmap_size)
			{
				temp = bmap_ptr[i] ;
			}
			else
			{
				temp = 0;	/* all unallocated past current end */
			}
			while(inactive_cnt < rel_tuple_indx)
			{
				bit = (temp & 1);
				temp >>= 1;
				abs_tuple_indx++;
				if(!bit)
					inactive_cnt++;
			} /* inactive_cnt <= rel_tuple_indx	*/
			ur_rt2k(rel_num,abs_tuple_indx,&temp_ident) ;
			uu_dprint(UU_RITRC,(us,"relocated key_id 0x%x to 0x%x",
							*key_id,temp_ident)) ;
			*key_id = temp_ident ;
		} /* temp_ident != 0 */
		uu_dexit ;
		return(0) ;
	}
