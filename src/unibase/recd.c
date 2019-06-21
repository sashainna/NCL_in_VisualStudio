#define MODULEDBG 0
/*********************************************************************
**    NAME         :  recd.c
**       CONTAINS:
**			ur_create_data()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       recd.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       08/17/15 , 17:41:45
*********************************************************************/

#include "usysdef.h"
#include "riddle.h"
#include	"rmtuple.h"
#include "rbase.h"
#include	"ribase.h"
#include "riddldef.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION     : ur_create_data(&data_packet)
**      create application data, both fixed and variable
**    PARAMETERS   
**       INPUT  : 
**			&data_packet, 			address of Unibase data packet
**			data_packet.rel_num, set to relation number data is to be
**										created with
**			data_packet.buff, 	the data to use when creating
**       OUTPUT :  
**			data_packet.key_id, 	set to assigned key_id the
**										data was created with
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_create_data(data_packet)
struct UR_data	*data_packet;	/* address of Unibase data packet*/
{
	UR_TUPLE_INDX	mtuple_indx;			/* holds allocated master tuple index */
	UR_TUPLE_INDX	dtuple_indx;			/* holds allocated rel tuple index */
	UR_TUPLE_INDX	tuple;					/* holds misc tuple index */
	UU_KEY_ID		mkey;						/* key id for master tuple */
	UU_KEY_ID		dkey;						/* key id for data tuple */
	UU_KEY_ID		back_key;				/* key id for associates */
	UU_KEY_ID		new_key;					/* key id for associates */
	UU_KEY_ID		assocs[UR_MAX_INDX+1];	/* key id for data tuple */
	UR_REL_NUM		drel_num;				/* relation number for the data */
	UR_REL_NUM		rel;						/* misc. relation number */
	struct UR_MTID_rec	m_rec;			/* a master tuple */
	int				status;					/* status, -1 if error, 0 otherwise */
	int				i, j, k;					/* for indices */
	struct UR_lpacket	*lpack_ptr;			/* pointer to a list packet */
	int				atom_size;				/* size of an atom within a list */
	UU_LOGICAL		mtuple_reqd;			/* true if rel requires a master tuple */
	struct attr_def	*atdefs;			/* attribute definitions */
	struct attr_def	*v_adefs;			/* attribute definitions - varlist */
	int				num_attr;				/* number of attributes defined */
	int				v_nattr;					/* number of attributes defined */
	int				rel_typ;
	int				list;
	int				n_atoms;

	uu_denter(UU_RTRC,(us,"ur_create_data(&data 0x%x, data.rel_num %d)",
				data_packet,data_packet->rel_num));
	status = 0;
	data_packet->key_id = 0;	/* clear the key_id in the packet */
	mtuple_reqd = uu_tst_bit(&UR_rcb[data_packet->rel_num].rel_flags,
									UR_MTUPLE_REQD);
	if(mtuple_reqd)
	{
		/* create a master entry, and build the key_id */
		/* initialize the master tuple to be created for this data packet */
		m_rec.save1 = 0;			/* clear reserved field */
		m_rec.bit_tbl = 1;		/* initially editable */

		/* clear view key, and set display segment as undefined */
		m_rec.view_key = 0;
		m_rec.dsegid = -1;				/* set undefined 	*/
		m_rec.no_assocs = 0;
		status = ur_create_tuple(UR_MTUPLE_REL, &mtuple_indx, &m_rec);

		/* clear out hardwired association indices. */
		for (i = 0; i <= UR_MAX_INDX; i++)
		{
			assocs[i] = 0;
		}
		ur_update_varlist_info(UR_MTUPLE_REL, mtuple_indx, 1, 0, 0);
		ur_update_tuple_varlist(UR_MTUPLE_REL, mtuple_indx, 1, 1,
							UR_MAX_INDX + 1, assocs);
		ur_rt2k(UR_MTUPLE_REL, mtuple_indx, &mkey);
		ur_assoc_on(mkey);
#if MODULEDBG != 0
		ur_dump_mtuple(mkey);
#endif
		data_packet->key_id = mkey;	/* set the key_id in the packet	*/
	} /* if master tuple required	*/
	if(status == 0)
	{
		/* create a tuple in requested relation, and store the fixed data */
		drel_num = data_packet->rel_num;
		status = ur_create_tuple(drel_num, &dtuple_indx, data_packet);
		if(status == 0)	
		{
			/* update the varlists and associate the data with the master tuple */
			for (i=1; i<=UR_rcb[drel_num].n_varl; i++)
			{
				ur_update_varlist_info(drel_num, dtuple_indx, i, 0, 0);
				ur_get_list_packet_ptr(drel_num, i, data_packet, &lpack_ptr);
				if(lpack_ptr != 0 && 
					lpack_ptr->atom_cnt > 0 && lpack_ptr->list_ptr != 0)
				{
					ur_update_tuple_varlist(drel_num, dtuple_indx, i, 1,
							lpack_ptr->atom_cnt, lpack_ptr->list_ptr);
				}
			}
			ur_rt2k(drel_num, dtuple_indx, &dkey);
			ur_assoc_on(dkey);
			if(mtuple_reqd)
			{
				ur_update_mtuple_attr(mkey, UR_DATA_INDX, dkey);/* set in mtuple */
				back_key = mkey;
			}
			else
			{
				data_packet->key_id = dkey;	/* set key in packet */
				back_key = dkey;
			}
			/* if the relation is an association add back pointers */
			if (uri_assocp(drel_num))
			{
				/* get definition */
				num_attr = UR_rcb[drel_num].rel_def->rellen;
				atdefs = UR_rcb[drel_num].relattr;
				list = 1;

				/* for each key field */
				for (i = 1; (i < num_attr); i++)
				{
					/* if master key */
					if ((atdefs[i].attr_type == KEY_ID)
						|| (atdefs[i].attr_type == REL_ID))
					{
						ur_k2rt(*(UU_KEY_ID *)((char *)data_packet
								+ atdefs[i].attr_here), &rel, &tuple);
						if (rel == UR_MTUPLE_REL && tuple)
						{
							/* add assoc list back_key */
							uri_add_assoc_list(*(UU_KEY_ID *)((char *)data_packet
								+ atdefs[i].attr_here), back_key);
						}
					}
					else if (atdefs[i].attr_type == JOIN)
					{
						/* do the same thing with each joined field */
						v_nattr = UR_rcb[drel_num].lparms[list-1].atom_def->rellen;
						v_adefs = UR_rcb[drel_num].lparms[list-1].atomattr;
						ur_get_list_packet_ptr(drel_num, list, data_packet,
								&lpack_ptr);
						ur_get_atom_size(drel_num, list, &atom_size);
						for (j = 1; j < v_nattr; j++)
						{
							if ((v_adefs[j].attr_type == KEY_ID)
								|| (v_adefs[j].attr_type == REL_ID))
							{
								n_atoms = lpack_ptr->atom_cnt;
								for (k = 0; k < n_atoms; k++)
								{
#if MODULEDBG != 0
									uu_dprint(UU_RITRC,(us,"updating key in atom %d",k));
#endif
									new_key = *(UU_KEY_ID *)((char *)lpack_ptr->list_ptr
											+ v_adefs[j].attr_here + k * atom_size);
									{
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
						list++;
					}
				}
			}
		}
		else
		{
			/* couldn't create the data tuple, so delete the master */
			if(mtuple_reqd)
				ur_delete_tuple(UR_MTUPLE_REL,mtuple_indx);
		}
	}
	uu_dexit;
	return(status);
}
