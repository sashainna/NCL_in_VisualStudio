/*********************************************************************
**    NAME         :  riualv.c
**       CONTAINS:
**       uri_update_assoc_list_varlist()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riualv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:48
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) riualv.c 3.2 1/27/88 15:44:31 single"};
#else
static char uu_sccsident[]={"@(#) riualv.c 3.2 1/27/88 15:44:31 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "ribase.h"
#include "riddldef.h"
#include "rmtuple.h"

/*********************************************************************
**    I_FUNCTION :	uri_update_assoc_list_varlist(old_list, new_list,
**											atom_def, n_attr, back_key)
**       fix the associativity back links of mtuples whose keys appear
**			in the given lists
**    PARAMETERS   
**       INPUT  : 
** 			old_list		pointer to a list packet
**				new_list		pointer to a list packet
**				atom_def[]	attribute definitions
**				n_attr		number of attributes
**				atom_size	size of each list element
**				back_key		key for data
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : various mtuple assoc lists fixed
**    WARNINGS     : none
*********************************************************************/

uri_update_assoc_list_varlist(old_list, new_list, atom_def, n_attr,
					atom_size, back_key)
UR_lpacket 			*old_list;	/* pointer to a list packet */
UR_lpacket 			*new_list;	/* pointer to a list packet */
struct attr_def	atom_def[];	/* attribute definitions */
int					n_attr;		/* number of attributes */
int					atom_size;	/* size of each list element */
UU_KEY_ID			back_key;	/* key for data */
{
	int				i, j;
	int				n_atoms;		/* min atoms in the lists */
	UU_KEY_ID		new_key, old_key;
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;

	uu_denter(UU_RITRC, (us, "uri_update_assoc_list_varlist()"));
	for (j = 1; j < n_attr; j++)
	{
		if ((atom_def[j].attr_type == KEY_ID)
			|| (atom_def[j].attr_type == REL_ID))
		{
			n_atoms = (old_list->atom_cnt < new_list->atom_cnt)
							?old_list->atom_cnt
							:new_list->atom_cnt;
			for (i = 0; i < n_atoms; i++)
			{
				if ((new_key = *(UU_KEY_ID *)((char *)new_list->list_ptr
								+ atom_def[j].attr_here + i * atom_size))
					!= (old_key = *(UU_KEY_ID *)((char *)old_list->list_ptr
								+ atom_def[j].attr_here + i * atom_size)))
				{
					ur_k2rt(old_key, &rel, &tuple);
					if (rel == UR_MTUPLE_REL)
					{
						/* delete assoc list back_key */
						uri_del_assoc_list(old_key, back_key);
					}
					ur_k2rt(new_key, &rel, &tuple);
					if (rel == UR_MTUPLE_REL)
					{
						/* add assoc list back_key */
						uri_add_assoc_list(new_key, back_key);
					}
				}
			}
			if (old_list->atom_cnt > n_atoms)	/* remove extra old atoms */
			{
				for (i = n_atoms; i < old_list->atom_cnt; i++)
				{
					old_key = *(UU_KEY_ID *)((char *)old_list->list_ptr
								+ atom_def[j].attr_here + i * atom_size);
					ur_k2rt(old_key, &rel, &tuple);
					if (rel == UR_MTUPLE_REL)
					{
						/* delete assoc list back_key */
						uri_del_assoc_list(old_key, back_key);
					}
				}
			}
			if (new_list->atom_cnt > n_atoms)	/* add extra new atoms */
			{
				for (i = n_atoms; i < new_list->atom_cnt; i++)
				{
					new_key = *(UU_KEY_ID *)((char *)new_list->list_ptr
								+ atom_def[j].attr_here + i * atom_size);
					ur_k2rt(new_key, &rel, &tuple);
					if (rel == UR_MTUPLE_REL)
					{
						/* add assoc list back_key */
						uri_add_assoc_list(new_key, back_key);
					}
				}
			}
		}
	}
	uu_dexit;
}

