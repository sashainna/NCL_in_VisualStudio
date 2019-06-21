/*********************************************************************
**    NAME         :  rixk.c
**       CONTAINS:
**       ur_extract_keys()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rixk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:49
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "ribase.h"
#include "riddldef.h"
#include "rmtuple.h"

/*********************************************************************
**    E_FUNCTION :  ur_extract_keys(rel_key,keys,key_max,&num_keys)
**			extract application keys for a relation tuple, do NOT extract
**			required system keys
**    PARAMETERS   
**    INPUT  : 
**				key,		key of tuple to extract keys from
**				keys,		address of array to put the extracted keys in
**				key_max,	maximum number of keys that will fit in keys array
**				num_keys,address of where to put the actual number of 
**							extracted keys
**    OUTPUT :  
**          	keys,		contains the extracted keys
**				num_keys,the number of keys put in the keys array
**    RETURNS    :	the number of keys that could have been extracted,
**						if returned value does not equal num_keys, the 
**						the keys array is full and not all keys were
**						returned
**    SIDE EFFECTS : none
**    WARNINGS     :	no checking is done as to the validity of an 
**							extracted key, i.e. - 0 is returned as a valid key
*********************************************************************/

ur_extract_keys(rel_key,keys,key_max,num_keys)
UU_KEY_ID	*rel_key;	/* relation key to traverse with					*/
UU_KEY_ID	keys[];		/* address of array to return the keys in		*/
long	key_max;				/* max number of entries in rel_keys array	*/
long	*num_keys;			/* the number of keys actually extarcted		*/
{
	int	status;				/* return status						*/
	long	rel;					/* the relation						*/
	long	tindex;				/* tuple index into the relation	*/
	long	num_attr;			/* number of attributes in the relation	*/
	long	attr_indx;			/* index into attr definitions				*/
	long	attr_type;			/* type of attribute								*/
	char	*attr_ptr;			/* ptr to an attribute within a tuple		*/
	long	var_num_attr[9];	/* number of var attributes in the list	*/
	long	var_attr_type;		/* type of var attribute						*/
	long	row_indx,col_indx	;	/* row,column indexs							*/
	long	key_cnt;				/*  number of keys found						*/
	char	*key_ptr;			/* ptr into data tuple to a imbeded key	*/
	long	key_offset;	
	int	i,j,k,m,n;			/* indexes											*/
	long	*a_ptr;				/* a dummy pointer								*/
	long	lst_len;				/* length of a variable list					*/
	long	atom_cnt;			/* number of an atoms in a list				*/
	long	rel_type,var_rel_type;
	UU_LOGICAL	keys_present; /* true if a key or rel_id is present	*/

#define attr_max	64	
	struct attr_def	*attr_defs;				/* tuple definition */
	struct attr_def	(* var_attr_defs)[attr_max];	/* varlist definition */
	struct UR_data	data_buff;	/* holder of data	tuple or atom		*/

	char	*uu_toolmalloc() ;	/* Unicad malloc routine	*/

	uu_denter(UU_RTRC,(us,"ur_extract_keys for rel_key = 0x%x and max_keys = %d",
				rel_key,key_max));
	/* make sure data dictionary initialized */
	status = ur_chk_data_dict() ;
	if(status != 0)
	{
		uu_dexit ;
		return(-1) ;
	}
	/* get space to hold max possible varlist definitions(9) */
	var_attr_defs = (struct attr_def (*)[attr_max])
						uu_toolmalloc(9 * attr_max*sizeof(struct attr_def));
	if(var_attr_defs != 0)	/* if we got the space proceed	*/
	{
		status = 0 ;
		key_cnt	=	0	;
		*num_keys = 0	;

		ur_k2rt(rel_key,&rel,&tindex) ;
		if(rel < 0) 
		{
			status = -1 ;
		}
		else
		{
			num_attr = UR_rcb[rel].rel_def->rellen;
			attr_defs = UR_rcb[rel].relattr;
			uu_dprint(UU_RITRC,(us," relation %s of type %d has %d attributes",
					UR_rcb[rel].relname,rel_type,num_attr));
			for(i = 0; i < UR_rcb[rel].n_varl; i++)
			{
				j = num_attr-UR_rcb[rel].n_varl+i;
				if(attr_defs[j].attr_type == STRING)
				{
					var_num_attr[i] = 1;
					strcpy(var_attr_defs[i][0].attr_name,"string");
					var_attr_defs[i][0].attr_type = CHARACTER;
					var_attr_defs[i][0].num_rows = 1;
					var_attr_defs[i][0].num_cols = 1;
					var_attr_defs[i][0].attr_off = 4;
				}
				else
				{
					var_num_attr[i] = ur_data_dict(&var_attr_defs[i][0], attr_max,
						attr_defs[j].attr_name,&var_rel_type);
					uu_dprint(UU_RITRC,(us,"varlist %s of type %d has %d attributes",
						attr_defs[j].attr_name,var_rel_type,var_num_attr[i]));
				}
			}
			/* look for imbedded keys */
			keys_present = UU_FALSE;
			for(i = 1; i < num_attr; i++)
			{
				attr_type = attr_defs[i].attr_type;
				if(attr_type == KEY_ID || attr_type == REL_ID)
					keys_present = UU_TRUE;
			}
			if(keys_present)
			{
				status = ur_retrieve_tuple_and_var_info(rel,tindex,&data_buff);
				if(status != 0)
				{
					goto l_99;	/* go exit */
				}
				/* build a pointer to the application data, bypass the req'd */
				/* key_id and rel_num if it is not a master tuple */
				attr_ptr = (char	*) &data_buff;
				attr_indx = 0;
				if(rel != UR_MTUPLE_REL)
				{
					attr_ptr += sizeof(UU_KEY_ID) + sizeof(long);
					attr_indx = 1;
				}
				for( ;attr_indx <num_attr-UR_rcb[rel].n_varl; attr_indx++)
				{
					attr_type = attr_defs[attr_indx].attr_type;

					/* set up row column index's , single attr will be 1 X 1 */
					row_indx = attr_defs[attr_indx].num_rows;
					col_indx = attr_defs[attr_indx].num_cols;

					/* iterate through the data , extracting the keys */
					if(attr_type == KEY_ID || attr_type == REL_ID)
					{
						key_ptr = attr_ptr;

						/* set the key offset */
						key_offset = attr_defs[attr_indx].attr_off /
															(row_indx * col_indx);
						for(m = 1; m <= col_indx; m++)
						{
							for(n = 1; n <= row_indx; n++)
							{
								if(key_cnt <= key_max)
								{
									(*num_keys)++;
									keys[key_cnt]	=	*((UU_KEY_ID *) key_ptr);
								}
								key_cnt++;		/* bump key cnt even if no room left */
								uu_dprint(UU_RITRC,(us,"key_cnt = %d, key = 0x%x",
												key_cnt,keys[key_cnt-1]));
								key_ptr += key_offset;
							}	/* for each row */
						}	/* for each col */
					} /* if attr_type == KEY_ID || attr_type == REL_ID	*/
					attr_ptr += attr_defs[attr_indx].attr_off ;
				}	/* for attr */
			} /* if keys present */
			/* now look through the variable lists, if any and if necessary */
			for(i = 1; i <= UR_rcb[rel].n_varl; i++)
			{
				/* look for imbedded keys */
				keys_present = UU_FALSE ;
				for(j = 1; j < var_num_attr[i-1]; j++)
				{
					var_attr_type = var_attr_defs[i-1][j].attr_type;
					uu_dprint(UU_RITRC,(us,"var attribute type = %d",var_attr_type));
					if(var_attr_type == KEY_ID || var_attr_type == REL_ID)
						keys_present = UU_TRUE;
				}
				/* if a key in this list, go look for it */
				if(keys_present)
				{
					status = ur_get_varlist_ptr(rel,tindex,i,&a_ptr,&lst_len);
					if(status == 0 && lst_len > 0)
					{
						uu_dprint(UU_RITRC,(us,"extracting from join to %s",
								attr_defs[num_attr-UR_rcb[rel].n_varl+i-1].attr_name));
						atom_cnt = lst_len;
						for(j = 1; j <= atom_cnt; j++)
						{
							ur_retrieve_data_varlist(rel_key,i,&data_buff,j,1);
							attr_ptr = (char *) &data_buff;
							for(k = 1; k < var_num_attr[i-1]; k++)
							{
								var_attr_type = var_attr_defs[i-1][k].attr_type;
								row_indx = var_attr_defs[i-1][k].num_rows;
								col_indx = var_attr_defs[i-1][k].num_cols;
								if(var_attr_type == KEY_ID || var_attr_type == REL_ID)
								{
									key_ptr = attr_ptr;
									key_offset = var_attr_defs[i-1][k].attr_off /
										(row_indx*col_indx);
									for(m = 1; m <= col_indx; m++)
									{
										for(n = 1; n <= row_indx; n++)
										{
											if(key_cnt <= key_max)
											{
												(*num_keys)++;
												keys[key_cnt] = *((UU_KEY_ID *) key_ptr);
											}
											key_cnt++;	/* bump key cnt even if no room */
											uu_dprint(UU_RITRC,(us,"key_cnt=%d, key=0x%x",
													key_cnt,keys[key_cnt-1]));
											key_ptr += key_offset;
										} /*  for each row */
									} /* for each col */
								}
								attr_ptr += var_attr_defs[i-1][k].attr_off;
							}
						}
					}	/* if status == 0 && lst_len > 0	*/
				} /* if keys present	*/
			} /* for i < n_varl	*/
			status = key_cnt;
		} /* else rel > 0	*/

l_99:
		uu_toolfree(var_attr_defs);
	} /* var_attr_defs != 0 */
	else
	{
		status = -1;	/* could not malloc the necessary space */
	}
	uu_dexit;
	return(status);
}
