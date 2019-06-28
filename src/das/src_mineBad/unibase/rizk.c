/*********************************************************************
**    NAME         :  rizk.c
**       CONTAINS:
**       uri_zap_keys()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rizk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:50
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) rizk.c 3.2 3/3/88 10:03:09 single"};
#else
static char uu_sccsident[]={"@(#) rizk.c 3.2 3/3/88 10:03:09 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "riddldef.h"
#include "rbase.h"
#include "ribase.h"
#include "riddle.h"
#include "rmtuple.h"

/*********************************************************************
**    E_FUNCTION :  uri_zap_keys(m_ptr, target_key)
**       clear occurences of target_key to prevent dangling references
**			typically to a deleted entity
**    PARAMETERS   
**       INPUT  : 
**          m_ptr			pointer to a master tuple record
**				target_key	key of said master tuple
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uri_zap_keys(m_ptr, target_key)
struct UR_MTID_rec	*m_ptr;	/* a master tuple */
UU_KEY_ID				target_key;
{
#define	atmax	64
	int					priority;
	int					field;
	UU_KEY_ID			assoc_rel_key;
	UR_REL_NUM			assoc_rel;
	UR_TUPLE_INDX		assoc_indx;
	struct attr_def	*atdefs;	/* attribute definitions */
	int					rel_typ;
	int					num_attr;		/* number of fields */
	char					*assoc_ptr;		/* ptr to the assoc data */
	int					i;
	int					list;
	int					v_nattr;			/* number of attributes defined */
	struct attr_def	*v_adefs;	/* attribute definitions - varlist */
	struct UR_lpacket	*lpack_ptr;		/* pointer to a list packet */
	int					atom_size;		/* size of an atom within a list */
	int					j, k;				/* for indices */
	int					n_atoms;
	int					status;

	uu_denter(UU_RITRC,(us,"uri_zap_keys(target=0x%x, no_assocs=%d)",
						target_key, m_ptr->no_assocs));
	status = 0;
	for (i = UR_MAX_INDX + 1; i < (*m_ptr).no_assocs; i++)
	{
		ur_k2rt((*m_ptr).assocs[i], &assoc_rel, &assoc_indx);
		/* get definition of assoc */
		if (assoc_rel == UR_MTUPLE_REL)
		{
			/*if (assoc_indx == 1378897)
				{
					sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
				}*/
			uri_retrieve_master_attr(assoc_rel, assoc_indx, UR_DATA_INDX,
							&assoc_rel_key);
			ur_k2rt(assoc_rel_key, &assoc_rel, &assoc_indx);
		}
		status = ur_get_tuple_ptr(assoc_rel, assoc_indx, &assoc_ptr);
		if (status == 0)
		{
			num_attr = UR_rcb[assoc_rel].rel_def->rellen;
			atdefs = UR_rcb[assoc_rel].relattr;
			list = 1;

			/* for each field if field == target_key then: */
			for (field = 0; field < num_attr; field++)
			{
				if (atdefs[field].attr_type == KEY_ID)
				{
					if ( *(UU_KEY_ID *)(assoc_ptr + atdefs[field].attr_here)
							== target_key)
					{
						uu_dprint(UU_RITRC,(us,
								"zapping target key in field %d", field));
						*(UU_KEY_ID *)(assoc_ptr + atdefs[field].attr_here) = 0;
					}
				}
				else if (atdefs[field].attr_type == JOIN)
				{
					/* do the same thing with each joined field */
					v_nattr = UR_rcb[assoc_rel].lparms[list-1].atom_def->rellen;
					v_adefs = UR_rcb[assoc_rel].lparms[list-1].atomattr;
					ur_get_list_packet_ptr(assoc_rel, list, assoc_ptr,
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
								if ( *(UU_KEY_ID *)((char *)lpack_ptr->list_ptr
										+ v_adefs[j].attr_here + k * atom_size)
									== target_key)
								{
									uu_dprint(UU_RITRC,(us,
										"zapping target key in atom %d of list %d",k,
										list));
									*(UU_KEY_ID *)((char *)lpack_ptr->list_ptr
										+ v_adefs[j].attr_here + k * atom_size) = 0;
								}
							}
						}
					}
					list++;
				}
			}	/* for each field */
		}	/* got tuple ptr */
	}	/* for each association */
	uu_dexit;
}

