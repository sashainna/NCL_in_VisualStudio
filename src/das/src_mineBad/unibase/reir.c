/*********************************************************************
**    NAME         :  reir.c
**       CONTAINS:
**       ur_init_rel()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reir.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:31
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include	"udebug.h"
#include "rireldef.h"
#include "riddldef.h"

int	UR_big_entry = 0;		/* largest relation entry inited in Unibase */
int	UR_big_atom = 0;		/* largest atom size inited in Unibase */

/*********************************************************************
** E_FUNCTION : ur_init_rel(rel_num,relname,n_ent,tuple_size,n_varl,
**									atom_sizes,list_sizes)
**      initialize a relation control block
**    PARAMETERS   
**       INPUT  : 
**		rel_num,		the relation number to be initialized, a number 0-255
**		relname,	a	0-8 character relation name, i.e. - "LINE"
**		n_ent,		the initial number of entries in the entry,
**						this will also be used as an expansion factor
**						if and when the relation is full
**		tuple_size,	the  size of each entry within the relation,
**						the fixed data size and any variable length list info
**		n_varl,		the number of possible variable length lists
**						associated with each entry
**		atom_sizes,	an integer array containing the atom sizes for 
**						each variable length list 
**		list_sizes,	an integer array containing the size in atoms 
**						to be used when setting up or expanding 
**						each variable length list 
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_init_rel(rel_num,relname,n_ent,tuple_size,n_varl,atom_sizes,list_sizes)
int	rel_num;			/* relation id number									*/
char 	*relname;		/* relation name											*/
int 	n_ent;			/* number of initial entries							*/
int 	tuple_size;		/* size of fixed data for the relation in bytes	*/
int	n_varl;			/* number of variable lists for this type rel	*/
int	atom_sizes[];	/* an array of byte counts, 1 for each varl		*/
int	list_sizes[];	/* an array containing the size to be used when	*/
							/* setting up or expanding each variable list	*/
{
	char			*uu_toolmalloc(); /* allocate storage function */
	UR_rcb_rec	*rcb; 				/* ptr to relation control block*/
	int			size;
	int			i, j;					/* index */
	int			status;				/* return status */
	char			*m_ptr;				/* pointer to allocated memory */
	int			page_size;			/* temp copy of page size in rcb */
	int			num_attr;		/* number of attributes found in rel definition */
	UU_LOGICAL	has_key;
	struct attr_def	*atdefs;		/* attribute definitions */
	struct attr_def	*v_adefs;	/* attribute definitions - varlist */
	int			v_nattr;				/* number of attributes parsed	*/
	int			rel_typ;
	int			list_no;


	uu_denter(UU_RTRC,(us,
					"ur_init_rel(rel=%d, name=%s, entries=%d, size=%d, lists=%d)",
					rel_num,relname,n_ent,tuple_size,n_varl));
	if(tuple_size > UR_big_entry)
	{
		UR_big_entry = tuple_size;
	}
	for(i = 0;i < n_varl; i++)
	{
		uu_dprint(UU_RTRC,(us," atom size %d, list size %d for list %d",
				atom_sizes[i],list_sizes[i],i+1));
		if(atom_sizes[i] > UR_big_atom)
		{
			UR_big_atom = atom_sizes[i];
		}
	}
	status = 0;

	/* stuff the relation id, and name in the relation control block */
	if(rel_num >= 0 && rel_num <= UR_MAX_REL)
	{
		rcb = &UR_rcb[rel_num];
		if(rcb->status < 0) /* if not previously initialized	*/
		{
			rcb->rel_num = rel_num;
			uu_set_bit(&UR_rcb[rel_num].rel_flags,UR_DATA_REL);
			uu_set_bit(&UR_rcb[rel_num].rel_flags,UR_MTUPLE_REQD);
			strncpy(rcb->relname,relname,UR_MAX_CHARS);
			rcb->relname[UR_MAX_CHARS+1] = '\000';

			/* see if the relation is an association */
			uu_dprint(UU_RTRC,(us," about to call sdict_refer."));
			num_attr = ur_sdict_refer(&(rcb->rel_def), &(rcb->relattr),
						rcb->relname);
			atdefs = rcb->relattr;
			has_key = UU_FALSE;

			/* save number of variable length records, */
			/* and allocate space for atom info if necessary */
			rcb->n_varl = n_varl;
			if(n_varl > 0)
			{
				size = n_varl * sizeof(UR_list_parms);
				m_ptr = uu_toolmalloc(size);	/* sizes and list sizes	*/
				if (m_ptr == UU_NULL)
				{
					uu_dprint(-1,(us,"ERROR:unable to allocate varlist info block"));
					uu_dexit;
					return(-1);
				}
				rcb->lparms = (struct UR_list_parms * ) m_ptr;
				i = num_attr - n_varl;
				for (list_no = 0; list_no < n_varl; list_no++, i++)
				{
					rcb->lparms[list_no].atom_size = atom_sizes[list_no];
					rcb->lparms[list_no].list_size = list_sizes[list_no];
					v_nattr = ur_sdict_refer(&(rcb->lparms[list_no].atom_def),
								&(rcb->lparms[list_no].atomattr), atdefs[i].attr_name);
				}
			}
		
			/* skip the first field in determining "association-ness" */
			list_no = 0;
			for (i = 1; (i < num_attr) && (has_key == UU_FALSE); i++)
			{
				if ((atdefs[i].attr_type == KEY_ID)
						|| (atdefs[i].attr_type == REL_ID))
				{
					has_key = UU_TRUE;
				}
				else if (atdefs[i].attr_type == JOIN)
				{
					/* do the same thing with each joined field */
					v_nattr = rcb->lparms[list_no].atom_def->rellen;
					v_adefs = rcb->lparms[list_no].atomattr;
					for (j = 1; (j < v_nattr) && (has_key == UU_FALSE); j++)
					{
						if ((v_adefs[j].attr_type == KEY_ID)
							|| (v_adefs[j].attr_type == REL_ID))
						{
							has_key = UU_TRUE;
						}
					}
					list_no++;
				}
			}
			if (has_key)
			{
				uu_set_bit(&UR_rcb[rel_num].rel_flags, UR_ASSOC_REL);
			}
			else
			{
				uu_clr_bit(&UR_rcb[rel_num].rel_flags, UR_ASSOC_REL);
			}
			rcb->status = 0;

			/* expansion size must be power of 2 not less than # bits/word */
			/* the actual number of entries is calculated by how many */
			/* entries will exist in the bit map */
			rcb->n_ent = 0;			/* relation starts empty */
			rcb->init_ent = ur_largest_pow_2(
							((n_ent + UU_BITPW - 1)/UU_BITPW) * UU_BITPW );
			while (rcb->init_ent * tuple_size > UR_MAX_PAGE_SIZE)
				rcb->init_ent /= 2;	/* lower power if too large */
			if (rcb->init_ent < UU_BITPW)		/* not less than # bits/word */
			{
				rcb->init_ent = UU_BITPW;
			}
			uu_dprint(UU_RITRC,(us,"-exp entries = %d",rcb->init_ent));
			page_size	= rcb->page_size		/* in entries */
							= ur_largest_pow_2(UR_MAX_PAGE_SIZE /
								(rcb->init_ent * tuple_size)) * rcb->init_ent;
			if (page_size == 0)
			{
				page_size = rcb->page_size = UU_BITPW;
			}
			uu_dprint(UU_RITRC,(us,"-page size = %d",page_size));
			rcb->seg_shift = 0;
			while(page_size > 1)		/* compute segno shift count */
			{
				rcb->seg_shift++;
				page_size >>= 1;
			}
			uu_dprint(UU_RITRC,(us,"-seg shift = %d",rcb->seg_shift));
/*
 *			calculate sizes
 *
 *			var_info_size = size in bytes needed to hold the information
 *							 for the variable length list associated with a
 *							 particular tuple, if any
 *
 *			tuple_size = total number of bytes in the fixed portion of a tuple
*/
			rcb->var_info_size = n_varl * (sizeof (struct UR_lpacket));
			rcb->tuple_size = tuple_size;
			rcb->last_accessed_index = 0;
			rcb->last_active_index = 0;
			rcb->active_tuple_cnt = 0;

			/* defer the allocation for the entries until first use */
			rcb->ent_ptr =  UU_NULL;

			/* defer the allocation for the bitmaps also */
			rcb->bmap_size = 0;
			rcb->bmap_ptr = UU_NULL;
		}
		else
		{
			status = -1; /* previously initialized relation */
			uu_dprint(-1,(us,"Error - Previously initialized rel %d",rel_num));
		}
	}
	else
	{
		status = -1;	/* illegal rel num */
		uu_dprint(-1,(us,"Error - Illegal relation number %d",rel_num));
	}
	uu_dexit;
	return(status);
}
