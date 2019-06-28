#define MODULEDBG 0
/*********************************************************************
**    NAME         :  regnsak.c
**       CONTAINS:
**       ur_get_next_spec_assoc_key()
**       ur_get_app_spec_assoc_list()
**			uri_field2list()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       regnsak.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:31
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) regnsak.c 3.4 4/27/88 08:27:53 single"};
#else
static char uu_sccsident[]={"@(#) regnsak.c 3.4 4/27/88 08:27:53 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"
#include "ribase.h"
#include "riddldef.h"
#include "riddle.h"
#include "rmtuple.h"

/*********************************************************************
** E_FUNCTION :  ur_get_next_spec_assoc_key(key, nxt_assoc_indx,
**										assoc_rel, field, assoc_key)
**		Get the next key from the requested association associated with
**		the given key.  Search starts with nxt_assoc_indx'th key, if any,
**		and continues until one satisfying field and rel num is found or
**		until all have been examined.  The key found is returned as
**		assoc_key or the function fails.
**    PARAMETERS   
**      INPUT  : 
**    key				UU_KEY_ID	master tuple key for which to retrieve
**											associations
**		nxt_assoc_indx	int*			start index into association list - 1 based -
**											3 'special' keys are automatically skipped
**		assoc_rel		UR_REL_NUM	the type of association to be found
**		field				int			the field to find given key in.  Fields are
**											numbered starting at one and counted as in
**											the UniDDL file(skip rel_num, joins are one
**											field.)  Zero means any field.
**       OUTPUT :  
**    nxt_assoc_indx	int* 			updated index to association returned 
**		assoc_key		UU_KEY_ID	returned association key
**    RETURNS:		0 if successful, -1 otherwise
**    SIDE EFFECTS:	none
**    WARNINGS:	none
*********************************************************************/

ur_get_next_spec_assoc_key(key, nxt_assoc_indx, assoc_rel, field, assoc_key)
UU_KEY_ID	key;					/* master tuple key */
int			*nxt_assoc_indx;	/* which associate to start with */
UR_REL_NUM	assoc_rel;			/* the type of associations to be found */
int			field;				/* the field number to find the given */
										/* key in.  Zero means any field. */
UU_KEY_ID	*assoc_key;			/* key of associated tuple */
{
#define	atmax	64
	struct UR_MTID_rec	*m_ptr;			/* mtuple buffer */
	struct UR_data			*a_ptr;			/* association buffer */
	UR_REL_NUM				rel;
	UR_TUPLE_INDX			tuple;
	UU_LOGICAL				found;
	int						status;
	int						num_attr;		/* number of fields */
	struct attr_def		*atdefs;	/* attribute definitions */
	int						rel_typ;
	UU_KEY_ID				rel_key;
	int						v_nattr;			/* number of attributes defined */
	struct attr_def		*v_adefs;/* attribute definitions - varlist */
	int						list;				/* the number of the varlist */
	struct UR_lpacket		*lpack_ptr;		/* pointer to a list packet */
	int						atom_size;		/* size of an atom within a list */
	int						j, k;
	int						n_atoms;

	uu_denter(UU_RTRC,(us,
			"ur_get_next_spec_assoc_key(key=0x%x, indx=%d, rel=%d, field=%d)",
			key, *nxt_assoc_indx, assoc_rel, field));
#if MODULEDBG != 0
	ur_dump_mtuple(key);
#endif
	status = -1;
	ur_k2rt(key, &rel, &tuple);
	ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (m_ptr != UU_NULL)
	{
		*assoc_key = 0;
		found = UU_FALSE;
		num_attr = UR_rcb[assoc_rel].rel_def->rellen;
		atdefs = UR_rcb[assoc_rel].relattr;

		/* validate the field number */
		if (field > num_attr)
		{
			uu_dprint(-1, (us, "ERROR:illegal field number."));
			return(-1);
		}
		else if (field < 0)
		{
			uu_dprint(-1, (us, "ERROR:illegal field number."));
			return(-1);
		}

		/* if field then check if key field else error? */

		while ((m_ptr->no_assocs > (*nxt_assoc_indx + 2)) && !found)
		{
			/* check for requested type of association */
			ur_k2rt(m_ptr->assocs[*nxt_assoc_indx + 2], &rel, &tuple);
			if ((rel == UR_MTUPLE_REL) && (assoc_rel != UR_MTUPLE_REL))
			{
				/*if (tuple == 1378897)
				{
					sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
				}*/
				uri_retrieve_master_attr(rel, tuple, UR_DATA_INDX, &rel_key);
				ur_k2rt(rel_key,&rel,&tuple);
			}
			if (rel == assoc_rel)
			{
				/* check if associatiativity stuff enabled */
				if (uri_notify_allowedp(rel,tuple))
				{
					if (field == 0)
					{
						/* anything matches */
						*assoc_key = m_ptr->assocs[*nxt_assoc_indx + 2];
						status = 0;
						found = UU_TRUE;
						uu_dprint(UU_RTRC,(us,
								"ur_get_next_spec_assoc_key found 0x%x", *assoc_key));
					}
					else	/* not "any" field */
					{
						/* get ptr to assoc tuple */
						ur_get_tuple_ptr(rel, tuple, &a_ptr);
						if (atdefs[field - 1].attr_type == KEY_ID)
						{
							/* check for field match */
							if (*(UU_KEY_ID *)((char *)a_ptr + atdefs[field - 1].attr_here)
								== key)
							{
								*assoc_key = m_ptr->assocs[*nxt_assoc_indx + 2];
								status = 0;
								found = UU_TRUE;
								uu_dprint(UU_RTRC,(us,
									"ur_get_next_spec_assoc_key found 0x%x", *assoc_key));
							}
							else
							{
								(*nxt_assoc_indx)++;
							}
						}
						else if (atdefs[field - 1].attr_type == JOIN)
						{
							/* do the same thing with each joined field */
							list = uri_field2list(field, atdefs);
							v_nattr =UR_rcb[assoc_rel].lparms[list-1].atom_def->rellen;
							v_adefs = UR_rcb[assoc_rel].lparms[list-1].atomattr;
							ur_get_list_packet_ptr(assoc_rel, list, a_ptr,
									&lpack_ptr);
							ur_get_atom_size(assoc_rel, list, &atom_size);
							for (j = 1; j < v_nattr; j++)
							{
								if ((v_adefs[j].attr_type == KEY_ID)
									|| (v_adefs[j].attr_type == REL_ID))
								{
									n_atoms = lpack_ptr->atom_cnt;
									for (k = 0; k < n_atoms; k++)
									{
										uu_dprint(UU_RITRC,(us,"checking key in atom %d",
												k));
										if ( *(UU_KEY_ID *)((char *)lpack_ptr->list_ptr
												+ v_adefs[j].attr_here + k * atom_size)
											== key)
										{
											*assoc_key =
													m_ptr->assocs[*nxt_assoc_indx + 2];
											status = 0;
											found = UU_TRUE;
											uu_dprint(UU_RTRC,(us,
												"ur_get_next_spec_assoc_key found 0x%x",
												*assoc_key));
										}
									}
								}
							}
							if (!found)
							{
								(*nxt_assoc_indx)++;
							}
						}
						else	/* the field is neither a key or a varlist */
						{
							uu_dprint(-1, (us, "ERROR:illegal field type."));
							return(-1);
						}
					}
				}
				else
				{
					uu_dprint(UU_RTRC,(us, "notification disallowed."));
					(*nxt_assoc_indx)++;
				}
			}
			else
			{
				uu_dprint(UU_RTRC,(us, "wrong relation."));
				(*nxt_assoc_indx)++;
			}
		}
	}
	uu_dexit;
	return(status);
}


/*********************************************************************
**	E_FUNCTION :  ur_get_app_spec_assoc_list(key, assoc_rel, field,
**													list_len, assoc_list)
**		Get the list of keys associated with the given key and satisfying
**		field and rel num.
**    PARAMETERS   
**       INPUT  : 
**			key			UU_KEY_ID	master tuple key for which to retrieve
**											associations
**			assoc_rel	UR_REL_NUM	the type of association to be found
**			field			int			the field to find given key in.  Zero means
**											any field.
**       OUTPUT :  
**			list_len		int*			number of keys returned in the list
**			assoc_list	UU_KEY_ID**	returned association list
**    RETURNS:		0 if successful, -1 otherwise
**    SIDE EFFECTS:	none
**    WARNINGS:	none
*********************************************************************/

ur_get_app_spec_assoc_list(key, assoc_rel, field, list_len, assoc_list)
UU_KEY_ID	key;				/* master tuple key */
UR_REL_NUM	assoc_rel;		/* the type of associations to be found */
int			field;			/* the field number to find the given */
									/* key in.  Zero means any field. */
int			*list_len;		/* length of the returned list */
UU_KEY_ID	**assoc_list;	/* pointer to list of keys returned */
{
	struct UR_MTID_rec	*m_ptr;
	UR_REL_NUM				rel;
	UR_TUPLE_INDX			tuple;
	int						status;
	int						nxt_indx;	/* key index to start with */
	UU_KEY_ID				*list_ptr;	/* ptr to where next key goes */

	uu_denter(UU_RTRC,(us,
			"ur_get_app_spec_assoc_list(key=0x%x, rel=%d, field=%d)",
			key, assoc_rel, field));
	status = 0;
	*list_len = 0;
	ur_k2rt(key, &rel, &tuple);
	ur_get_tuple_ptr(rel, tuple, &m_ptr);
	uu_dprint(UU_RTRC,(us,"#assocs=%d", m_ptr->no_assocs));

	/* allocate memory to hold the list */
	*assoc_list = (UU_KEY_ID *)uu_malloc(m_ptr->no_assocs * sizeof(UU_KEY_ID));
	uu_dprint(UU_RTRC,(us,"assoc list at 0x%x", *assoc_list));
	if (*assoc_list != 0)
	{
		nxt_indx = 1;
		list_ptr = *assoc_list;
		while (ur_get_next_spec_assoc_key(key, &nxt_indx, assoc_rel, field,
													list_ptr) == 0)
		{
			status = 0;
			nxt_indx++;
			(*list_len)++;
			list_ptr++;
		}
	}
	uu_dexit;
	return(status);
}


/*********************************************************************
**    I_FUNCTION :  list = uri_field2list(field, atdefs);
**       return which list a given field of a definition represents
**    PARAMETERS   
**       INPUT  : 
**          field	int	the field involved
**				atdefs		the definitions of the fields of the relation
**       OUTPUT :  
**          none
**    RETURNS      : the list number of the field
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uri_field2list(field, atdefs)
int					field;
struct attr_def	atdefs[];
{
	int	i;
	int	list;

	for (i = 0, list = 0; i < field; i++)
	{
		if (atdefs[i].attr_type == JOIN)
		{
			list++;
		}
	}
	return(list);
}

