/*********************************************************************
**    NAME         :  rinotify.c
**       CONTAINS:
**       uri_notify()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rinotify.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:45
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) rinotify.c 3.3 3/3/88 10:02:33 single"};
#else
static char uu_sccsident[]={"@(#) rinotify.c 3.3 3/3/88 10:02:33 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "riddldef.h"
#include "rbase.h"
#include "ribase.h"
#include "riddle.h"
#include "rmtuple.h"
#include "r1emsgpk.h"

/*********************************************************************
**    E_FUNCTION :  uri_notify(m_ptr, cause_key, theMessage, messageCnt)
**       notifiy associatiates of m_ptr(cause_key) of change described by
**			theMessage, messageCnt
**    PARAMETERS   
**       INPUT  : 
**          m_ptr			pointer to a master tuple record
**				cause_key	key of said master tuple
**				theMessage, messageCnt - the change to be notified about
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uri_notify(m_ptr, cause_key, theMessage, messageCnt)
struct UR_MTID_rec	*m_ptr;	/* a master tuple */
UU_KEY_ID				cause_key;
UR_messagePacket		theMessage[];
int						messageCnt;
{
#ifdef UR_ASSOCIATIVE
	int					priority;
	int					field;
	UU_KEY_ID			assoc_rel_key;
	UU_KEY_ID			assoc_key;
	UU_KEY_ID			*as_key_ptr;	/* assoc list ptr */
	UR_REL_NUM			assoc_rel;
	UR_TUPLE_INDX		assoc_indx;
	struct attr_def	*atdefs;	/* attribute definitions */
	int					rel_typ;
	int					num_attr;		/* number of fields */
	char					*assoc_ptr;		/* ptr to the assoc data */
	int					i, j, k;			/* for indices */
	UU_LOGICAL			done;				/* boolean indicates if notification */
												/* already accomplished */
	UU_LOGICAL			begun;			/* flag if any updates found yet */
	int					list;
	int					v_nattr;			/* number of attributes defined */
	struct attr_def	*v_adefs;	/* attribute definitions - varlist */
	struct UR_lpacket	*lpack_ptr;		/* pointer to a list packet */
	int					atom_size;		/* size of an atom within a list */
	int					n_atoms;

	uu_denter(UU_RITRC,(us,"uri_notify(cause=0x%x, no_assocs=%d)",
						cause_key, m_ptr->no_assocs));
	begun = UU_FALSE;
	for (i = UR_MAX_INDX + 1; i < m_ptr->no_assocs; i++)
	{
		ur_k2rt(assoc_key = m_ptr->assocs[i], &assoc_rel, &assoc_indx);
		uu_dprint(UU_RITRC,(us,"notify allowed = %d",
			uu_tst_bit(
			&(UR_rcb[assoc_rel].bmap_ptr[UR_ASSOC_MAP*UR_rcb[assoc_rel].bmap_size])
						,assoc_indx-1)));

		/* see if already done */
		done = UU_FALSE;
		for (j = UR_MAX_INDX + 1; j < i; j++)
		{
			if (assoc_key == m_ptr->assocs[j])
			{
				done = UU_TRUE;
				break;
			}
		}

		if ((!done) && (uri_notify_allowedp(assoc_rel, assoc_indx)))
		{
			/* get definition of assoc */
			if (assoc_rel == UR_MTUPLE_REL)
			{
				uri_retrieve_master_attr(assoc_rel, assoc_indx, UR_DATA_INDX,
								&assoc_rel_key);
				ur_k2rt(assoc_rel_key, &assoc_rel, &assoc_indx);
			}
			priority = uri_get_priority(assoc_rel);
			ur_get_tuple_ptr(assoc_rel, assoc_indx, &assoc_ptr);
			num_attr = UR_rcb[assoc_rel].rel_def->rellen;
			atdefs = UR_rcb[assoc_rel].relattr;
			list = 1;

			/* for each field if field == cause_key then: */
			for (field = 0; field < num_attr; field++)
			{
				if (atdefs[field].attr_type == KEY_ID)
				{
					if ( *(UU_KEY_ID *)(assoc_ptr + atdefs[field].attr_here)
						== cause_key)
					{
						if (!begun)
						{
							ur1i_beginSchedUpdates();
							begun = UU_TRUE;
						}
						ur1i_schedUpdate(priority, (*m_ptr).assocs[i], cause_key,
									field, theMessage, messageCnt);
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
								uu_dprint(UU_RITRC,(us,"checking key in atom %d",k));
								if ( *(UU_KEY_ID *)((char *)lpack_ptr->list_ptr
										+ v_adefs[j].attr_here + k * atom_size)
									== cause_key)
								{
									if (!begun)
									{
										ur1i_beginSchedUpdates();
										begun = UU_TRUE;
									}
									ur1i_schedUpdate(priority, (*m_ptr).assocs[i],
											cause_key, field, theMessage, messageCnt);
								}
							}
						}
					}
					list++;
				}
			}
		}
		else
		{
			uu_dprint(UU_RITRC,(us,"notification disallowed or already done."));
		}
	}
	if (begun)
	{
		ur1i_commitSchedUpdates();
	}
	uu_dexit;
#endif
}

