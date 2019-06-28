#define MODULEDBG 1
/*********************************************************************
**    NAME         :  ress.c
**       CONTAINS:
**       ur_save_all()
**       ur_svld_clear()
**       ur_save_set()
**			uri_save_set_keys()
**			ur_save_clr()
**			ur_mark_to_save()
**       ur_load_set()
**		  ur_svld_test()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ress.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:37
*********************************************************************/

#include  "usysdef.h"
#include  "udebug.h"
#include "riddldef.h"
#include  "ribase.h"
#include "rmtuple.h"

extern	UU_LOGICAL	UR_sav_all;			/* boolean, UU_TRUE if save all */
extern	UU_LOGICAL	UR_save_modals;	/* boolean, UU_TRUE if save modals */

/*********************************************************************
**    E_FUNCTION     :  ur_save_all() 
**       save all active tuples for all relations
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_save_all()
{
	int	status;	/* return status */

	uu_denter(UU_RTRC,(us,"ur_save_all()"));
	UR_sav_all = UU_TRUE;
	uu_dexit;
	return(0);
}

/*********************************************************************
**    E_FUNCTION     :  ur_svld_clear()
**       clear(reset) all save tuple bits
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_svld_clear()
{
	int	status;	/* return status */
	int	i,j;		/* indexs */

	uu_denter(UU_RTRC,(us,"ur_svld_clear()"));
	for(i = 0; i <= UR_MAX_REL; i++)
	{
		for(j = 0; j < UR_rcb[i].bmap_size; j++)
		{
			UR_rcb[i].bmap_ptr[UR_SVLD_MAP*UR_rcb[i].bmap_size+j] = 0;
		}
	}
	uu_dexit;
	return(0);
}

/*********************************************************************
**    E_FUNCTION     :  ur_save_set(key)
**       set save tuple bit for this key
**    PARAMETERS   
**       INPUT  : 
**          key,	can be either a master(primary) key, or an 
**						actual relation key
**       OUTPUT :  
**          output
**    RETURNS      :	0 if able to set, -1 if specified tuples was
**							inactive(not used)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_save_set(key)
UU_KEY_ID	key;	/* a master key, or relation key		*/
{
	int				status;	/* return status */
	UR_REL_NUM		rel;		/* what relation to set save bit */
	UR_TUPLE_INDX	tuple;	/* what tuple within relation to set save bit */
	UU_KEY_ID		rel_key;	/* a relation key */
	int				i,j;		/* indexs */

	uu_denter(UU_RTRC,(us,"ur_save_set(key:0x%x)",key));
	status = 0;
	ur_k2rt(key,&rel,&tuple);
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us," rel=%d, tuple=%d",rel,tuple));
#endif
	if(UR_rcb[rel].status == 0)
	{
		if(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_ALLOC_MAP]),tuple-1))
		{
#if MODULEDBG != 0
			uu_dprint(UU_RITRC,(us," ...allocated tuple..."));
#endif
			if (uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_SVLD_MAP *
												UR_rcb[rel].bmap_size]), tuple-1))
			{
				uu_dexit;
				return(0);		/* already marked -- just return OK */
			}
#if MODULEDBG != 0
			uu_dprint(UU_RITRC,(us," ...not already marked..."));
#endif
			UR_sav_all = UU_FALSE;
			uu_set_bit(&(UR_rcb[rel].bmap_ptr[UR_SVLD_MAP *
												UR_rcb[rel].bmap_size]),tuple-1);
#if MODULEDBG != 0
			uu_dprint(UU_RITRC,(us," ...marked it..."));
#endif
			status = ur_save_set_keys(key);
		} /* if uu_tst_bit true	*/
		else
		{
			uu_dprint(-1,(us,"ERROR:ur_save_set for inactive tuple %d", tuple));
			status = -1;	/* attempted to set for save an inactive tuple */
		}
	}
	else
	{
		uu_dprint(-1,(us,"ERROR:ur_save_set for inactive relation %d", rel));
		status = -1;	/* tried to set save for inactive relation */
	}
	uu_dexit;
	return(status);
}


/*********************************************************************
**    E_FUNCTION     :  ur_save_set_keys(key)
**       set save tuple bit for all keys in tuple for this key
**    PARAMETERS   
**       INPUT  : 
**          key,	can be either a master(primary) key, or an 
**						actual relation key
**       OUTPUT :  
**          output
**    RETURNS      :	0 if able to set, -1 if specified tuples was
**							inactive(not used)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_save_set_keys(rel_key)
UU_KEY_ID	*rel_key;	/* relation key to traverse with */
{
	int				status;				/* return status */
	UR_REL_NUM		rel;					/* the relation */
	UR_TUPLE_INDX	tuple;				/* tuple index into the relation	*/
	long				num_attr;			/* number of attributes in the relation */
	long				attr_indx;			/* index into attr definitions */
	long				attr_type;			/* type of attribute */
	char				*attr_ptr;			/* ptr to an attribute within a tuple */
	long				var_num_attr[9];	/* number of var attributes in the list */
	long				var_attr_type;		/* type of var attribute */
	int				row_indx,col_indx;	/* row,column indexs */
	UU_KEY_ID		*key_ptr;	/* ptr into data tuple to a imbeded key */
	long				key_offset;	
	int				i,j,k,m,n;			/* indexes */
	long				lst_len;				/* length of a variable list */
	long				atom_cnt;			/* number of an atoms in a list */
	long				rel_type,var_rel_type;
	UU_LOGICAL		keys_present; /* true if a key or rel_id is present */
#define attr_max	64	
	struct attr_def	*attr_defs;	/* tuple definition */
	struct attr_def	(* var_attr_defs)[attr_max];	/* varlist definition */
	struct UR_data	*data_buff;	/* pointer to data tuple or atom */
	char				*uu_toolmalloc();	/* Unicad malloc routine */
	int				atom_size;

	uu_denter(UU_RTRC,(us,"ur_save_set_keys(key=0x%x)", rel_key));

	/* make sure data dictionary initialized */
	status = ur_chk_data_dict();
	if(status != 0)
	{
		uu_dexit;
		return(-1);
	}
	/* get space to hold max possible varlist definitions(9) */
	var_attr_defs =
		(struct attr_def (*)[attr_max]) uu_toolmalloc(9 * attr_max*sizeof(struct attr_def));
	if(var_attr_defs != 0)	/* if we got the space proceed	*/
	{
		status = 0;
		ur_k2rt(rel_key, &rel, &tuple);
		if(rel < 0) 
		{
			status = -1;
		}
		else
		{
			num_attr = UR_rcb[rel].rel_def->rellen;
			attr_defs = UR_rcb[rel].relattr;
#if MODULEDBG != 0
			uu_dprint(UU_RITRC, (us, " relation %s of type %d has %d attributes",
					UR_rcb[rel].relname, rel_type, num_attr));
#endif
			for(i = 0; i < UR_rcb[rel].n_varl; i++)
			{
				j = num_attr-UR_rcb[rel].n_varl+i;
				if(attr_defs[j].attr_type == STRING)
				{
					var_num_attr[i] = 1;
					strcpy(var_attr_defs[i][0].attr_name, "string");
					var_attr_defs[i][0].attr_type = CHARACTER;
					var_attr_defs[i][0].num_rows = 1;
					var_attr_defs[i][0].num_cols = 1;
					var_attr_defs[i][0].attr_off = 4;
				}
				else
				{
					var_num_attr[i] = ur_data_dict(&var_attr_defs[i][0], attr_max,
						attr_defs[j].attr_name, &var_rel_type);
#if MODULEDBG != 0
					uu_dprint(UU_RITRC, (us, "varlist %s of type %d has %d attributes",
						attr_defs[j].attr_name, var_rel_type, var_num_attr[i]));
#endif
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
				status = ur_get_tuple_ptr(rel, tuple, &data_buff);
				if(status != 0)
				{
					goto l_99;	/* go exit */
				}
				/* build a pointer to the application data, bypass the req'd */
				/* key_id and rel_num if it is not a master tuple */
				attr_ptr = (char	*) data_buff;
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

					/* iterate through the data , save_seting the keys */
					if(attr_type == KEY_ID || attr_type == REL_ID)
					{
						key_ptr = (UU_KEY_ID *)attr_ptr;

						/* set the key offset */
						key_offset = attr_defs[attr_indx].attr_off /
															(row_indx * col_indx);
						for(m = 1; m <= col_indx; m++)
						{
							for(n = 1; n <= row_indx; n++)
							{
								if (*key_ptr)	/* initialized key? */
									ur_save_set(*key_ptr);
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
#if MODULEDBG != 0
					uu_dprint(UU_RITRC, (us, "var attribute type = %d", var_attr_type));
#endif
					if(var_attr_type == KEY_ID || var_attr_type == REL_ID)
						keys_present = UU_TRUE;
				}
				/* if a key in this list, go look for it */
				if(keys_present)
				{
					status = ur_get_varlist_ptr(rel, tuple, i, &data_buff,
								&lst_len);
					ur_get_atom_size(rel, i, &atom_size);
					if(status == 0 && lst_len > 0)
					{
#if MODULEDBG != 0
						uu_dprint(UU_RITRC, (us, "save_seting from join to %s",
								attr_defs[num_attr-UR_rcb[rel].n_varl+i-1].attr_name));
#endif
						atom_cnt = lst_len;
						for(j = 0; j < atom_cnt; j++)
						{
							attr_ptr = (char *) data_buff + j * atom_size;
							for(k = 1; k < var_num_attr[i-1]; k++)
							{
								var_attr_type = var_attr_defs[i-1][k].attr_type;
								row_indx = var_attr_defs[i-1][k].num_rows;
								col_indx = var_attr_defs[i-1][k].num_cols;
								if(var_attr_type == KEY_ID || var_attr_type == REL_ID)
								{
									key_ptr = (UU_KEY_ID *)attr_ptr;
									key_offset = var_attr_defs[i-1][k].attr_off /
										(row_indx*col_indx);
									for(m = 1; m <= col_indx; m++)
									{
										for(n = 1; n <= row_indx; n++)
										{
											if (*key_ptr)	/* initialized key? */
												ur_save_set(*key_ptr);
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

/*********************************************************************
**    E_FUNCTION     :  ur_save_clr(key)
**       clear save tuple bit for this key
**    PARAMETERS   
**       INPUT  : 
**          key,	can be either a master(primary) key, or an 
**						actual relation key
**       OUTPUT :  
**          output
**    RETURNS      :	0 if able to set, -1 if specified tuples was
**							inactive(not used)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_save_clr(key)
UU_KEY_ID	key;			/* a master key, or relation key		*/
{
	int				status;	/* return status */
	UR_REL_NUM		rel;		/* what relation to set save bit */
	UR_TUPLE_INDX	tuple;	/* what tuple within relation to set save bit */
	UU_KEY_ID		rel_key;	/* a relation key */
	int				i,j;		/* indexs */

	uu_denter(UU_RTRC,(us,"ur_save_clr(key:0x%x)",key));
	status = 0;
	ur_k2rt(key,&rel,&tuple);
	if(UR_rcb[rel].status == 0)
	{
		if(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_ALLOC_MAP]),tuple-1))
		{
			uu_clr_bit(
				&(UR_rcb[rel].bmap_ptr[UR_SVLD_MAP*UR_rcb[rel].bmap_size])
				,tuple-1);
			if (rel == UR_MTUPLE_REL)
			{
				/*if (tuple == 1378897)
				{
					sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
				}*/
				status = uri_retrieve_master_attr(rel, tuple, UR_DATA_INDX,
											&rel_key);
				if ((status==0) && rel_key)
				{
					ur_save_clr(rel_key);
				}
				status = uri_retrieve_master_attr(rel, tuple, UR_ATTR_INDX,
											&rel_key);
				if ((status==0) && rel_key)
				{
					ur_save_clr(rel_key);
				}
				status = uri_retrieve_master_attr(rel, tuple, UR_TRANSF_INDX,
											&rel_key);
				if ((status==0) && rel_key)
				{
					ur_save_clr(rel_key);
				}
			}
		} /* if uu_tst_bit true	*/
		else
		{
			uu_dprint(-1,(us,"ERROR:ur_save_clr for inactive tuple %d", tuple));
			status = -1;	/* attempted to clear save an inactive tuple */
		}
	}
	else
	{
		uu_dprint(-1,(us,"ERROR:ur_save_clr for inactive relation %d", rel));
		status = -1;	/* tried to clear save for inactive relation */
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  ur_mark_to_save(key)
**       set save tuple bit for this key and related attrib. and transf.
**    PARAMETERS   
**       INPUT  : 
**          key,	can be either a master(primary) key, or an 
**						actual relation key
**       OUTPUT :  
**          output
**    RETURNS      :	0 if able to set, -1 if specified tuples was
**							inactive(not used)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_mark_to_save(key)
UU_KEY_ID	key;			/* a master key, or relation key		*/
{
	int				status;	/* return status */
	UR_REL_NUM		rel;		/* what relation to set save bit */
	UR_TUPLE_INDX	tuple;	/* what tuple within relation to set save bit */
	UU_KEY_ID		rel_key;	/* a relation key */
	int				i,j;		/* indexs */

	uu_denter(UU_RTRC,(us,"ur_mark_to_save(key:0x%x)",key));
	status = 0;
	ur_k2rt(key,&rel,&tuple);
	if(UR_rcb[rel].status == 0)
	{
		if(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_ALLOC_MAP]),tuple-1))
		{
			uu_set_bit(
				&(UR_rcb[rel].bmap_ptr[UR_SVLD_MAP*UR_rcb[rel].bmap_size])
				,tuple-1);
			UR_sav_all = UU_FALSE;
			if (rel == UR_MTUPLE_REL)
			{
				if (tuple == 1378897)
				{
					sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
				}
				status = uri_retrieve_master_attr(rel, tuple, UR_DATA_INDX,
											&rel_key);
				if ((status==0) && rel_key)
				{
					ur_mark_to_save(rel_key);
				}
				status = uri_retrieve_master_attr(rel, tuple, UR_ATTR_INDX,
											&rel_key);
				if ((status==0) && rel_key)
				{
					ur_mark_to_save(rel_key);
				}
				status = uri_retrieve_master_attr(rel, tuple, UR_TRANSF_INDX,
											&rel_key);
				if ((status==0) && rel_key)
				{
					ur_mark_to_save(rel_key);
				}
			}
		} /* if uu_tst_bit true	*/
		else
		{
			uu_dprint(-1,(us,"ERROR:ur_mark_to_save for inactive tuple %d",
										tuple));
			status = -1;	/* attempted to clear save an inactive tuple */
		}
	}
	else
	{
		uu_dprint(-1,(us,"ERROR:ur_mark_to_save for inactive relation %d", rel));
		status = -1;	/* tried to clear save for inactive relation */
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  ur_load_set(key)
**       set newly loaded tuple bit for this key
**    PARAMETERS   
**       INPUT  : 
**          key,	can be either a master(primary) key, or an 
**						actual relation key
**       OUTPUT :  
**          output
**    RETURNS      :	0 if able to set, -1 if specified tuples was
**							inactive(not used)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_load_set(key)
UU_KEY_ID	key;		/* a master key, or relation key		*/
{
	int				status;	/* return status */
	UR_REL_NUM		rel;		/* what relation to set save bit */
	UR_TUPLE_INDX	tuple;	/* what tuple within relation to set save bit */
	UU_KEY_ID		rel_key;	/* a relation key */
	int				i;			/* an index */

	uu_denter(UU_RTRC,(us,"ur_load_set(key:0x%x)",key));
	status = 0;
	ur_k2rt(key,&rel,&tuple);
	if(UR_rcb[rel].status == 0)
	{
		if(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_ALLOC_MAP]),tuple-1))
		{
			uu_set_bit(&(UR_rcb[rel].bmap_ptr[UR_SVLD_MAP *
												UR_rcb[rel].bmap_size]),tuple-1);
		}
		else
		{
			uu_dprint(-1,(us,"ERROR:ur_load_set for inactive tuple %d", tuple));
			status = -1 ; /* attempted to set for load an inactive tuple	*/
		}
	}
	else
	{
		uu_dprint(-1,(us,"ERROR:ur_load_set for inactive relation %d", rel));
		status = -1 ;	/* tried to set save for inactive relation		*/
	}
	uu_dexit ;
	return(status) ;
}

/*********************************************************************
**    E_FUNCTION     :  ur_svld_test(key) 
**       test tuples to see if it is to be saved
**    PARAMETERS   
**       INPUT  : 
**          key = key of relation,tuple to be tested
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if tuple to be saved, UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_svld_test(key)
UU_KEY_ID	key;	/* key to use in testing				*/
{
	int				status;	/* return status */
	UR_REL_NUM		rel;		/* what relation to set save bit */
	UR_TUPLE_INDX	tuple;	/* what tuple within relation to set save bit */

	uu_denter(UU_RTRC,(us,"ur_svld_test(key:0x%x)",key));
	status = UU_FALSE;
	ur_k2rt(key,&rel,&tuple);
	/* Second check to correct archive drawing. kathy */
	if(UR_rcb[rel].status == 0 && UR_rcb[rel].bmap_ptr != NULL)
	{
		if(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_SVLD_MAP *
												UR_rcb[rel].bmap_size]),tuple-1))
		{
			status = UU_TRUE;
		}
	}
#if MODULEDBG != 0
 	uu_dprint(UU_RITRC,(us,"ur_svld_test, return %d",status));
#endif
	uu_dexit;
	return(status);
}


/*********************************************************************
**    E_FUNCTION :  ur_set_save_modals()
**       Remember that we are to save modals with the succeeding saves
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_set_save_modals()
{
	UR_save_modals = UU_TRUE;
   return (UU_SUCCESS);
}



/*********************************************************************
**    E_FUNCTION :  ur_clr_save_modals()
**       Remember that we are not to save modals with the succeeding saves
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_clr_save_modals()
{
	UR_save_modals = UU_FALSE;
}

