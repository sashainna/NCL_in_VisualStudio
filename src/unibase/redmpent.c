#define MODULEDBG 0
/*********************************************************************
**    NAME         :  redmpent.c
**       CONTAINS:
**       ur_dump_data()
**			ur_dump_tuple()
**			ur_dump_mtuple()
**			ur_dump_atoms()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       redmpent.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:28
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) redmpent.c 3.4 3/8/88 09:44:02 single"};
#else
static char uu_sccsident[]={"@(#) redmpent.c 3.4 3/8/88 09:44:02 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"
#include "ribase.h"
#include "riddldef.h"
#include "rmtuple.h"
#include "rerrdef.h"

/*********************************************************************
**    E_FUNCTION :  int ur_dump_data(key)
**       dump data associated with a given key
**    PARAMETERS   
**       INPUT  : 
**          key	UU_KEY_ID	key to dump
**       OUTPUT :  
**         	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_dump_data(key)
UU_KEY_ID	key;
{
#ifndef UU_DEBUGOFF 
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	struct UR_data	*buffer;
	UR_lpacket		*lpack;
	UU_KEY_ID		sub_key;
	int				status;
	int				list;

	uu_denter(UU_RTRC,(us,"ur_dump_data(key=0x%x)",key));
	ur_k2rt(key, &rel, &tuple);
	if (rel == UR_MTUPLE_REL)
	{
		uri_retrieve_master_attr(rel, tuple, UR_DATA_INDX, &sub_key);
		ur_dump_data(sub_key);
		uri_retrieve_master_attr(rel, tuple, UR_TRANSF_INDX, &sub_key);
		if (sub_key)
		{
			ur_dump_data(sub_key);
		}
		uri_retrieve_master_attr(rel, tuple, UR_ATTR_INDX, &sub_key);
      if (sub_key) 
      { 
         ur_dump_data(sub_key); 
      } 
	}
	else	/* not master key */
	{
		status = ur_get_tuple_ptr(rel, tuple, &buffer);
		if (status != 0)
			uu_dprint(-1,(us,"oops, can't get pointer"));
		ur_dump_tuple(rel, buffer);
		for (list = 1; list <= UR_rcb[rel].n_varl; list++)
		{
			status = ur_get_list_packet_ptr(rel, list, buffer, &lpack);
			if (lpack->atom_cnt)
			{
				uu_dprint(UU_RTRC, (us, "list %d:", list));
				ur_dump_atoms(lpack->list_ptr, lpack->atom_cnt, rel, list);
			}
		}
	}
	uu_dexit;
#endif
}

/*********************************************************************
**    E_FUNCTION :  int ur_dump_tuple(rel, bag)
**       dump the data in the bag formatting according to the data
**				dictionary definition for the relation given.
**    PARAMETERS   
**       INPUT  : 
**          rel	int		relation to dump
**				bag		UR_data*	data to be dumped
**       OUTPUT :  
**         	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_dump_tuple(rel, bag)
UR_REL_NUM		rel;
struct UR_data	*bag;
{
#ifndef UU_DEBUGOFF 
	int		status;			/* return status */
   struct attr_def   *atdefs; /* attribute definitions */
   int      num_attr;		/* number of attributes parsed */
   int      atndx;			/* index into atdefs */
   int      atype;			/* the data type of the attribute */
   struct attr_def   *v_adefs;/* attribute definitions - varlist */
   int      v_nattr;			/* number of attributes parsed   */
   int      v_andx;			/* index into atdefs */
   int      v_atype;			/* the data type of the attribute   */
	int		rel_typ;
	int		row, col;		/* row and col index for arrays */
	char		pstr[132];		/* print string */
	UU_LOGICAL	ctrl;			/* line feed control */
	char			*attr_ptr;	/* ptr to each attribute within tuple */
	char     	*data_ptr;	/* ptr to data within attribute (for arrays) */
	int      	data_offset; /* offset to next data item */
	int      	extra_offset; /* offset to next data item */
	int			cndx;			/* column index */

   uu_denter(UU_RITRC,(us,"ur_dump_tuple(rel=%d, 0x%x)", rel, bag));
   status = ur_chk_data_dict();
   if (status)
   {
      uu_dexit;    /* bailout - still no data dictionary */
		return(-1);
   } 
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us,"data dictionary inited."));
#endif

	/* get definition from data dictionary */
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us,"looking up %s", UR_rcb[rel].relname));
#endif
	num_attr = UR_rcb[rel].rel_def->rellen;
	atdefs = UR_rcb[rel].relattr;
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us," ... has %d attributes", num_attr));
#endif
	if(num_attr <= 0)
	{
		status = URM_RELNTFND; /* rel not in data dictionary error */
		uu_dexit;
		return(status);
	}
	/* for each field of the definition:*/
	ctrl = UU_FALSE;
	attr_ptr = (char *)bag;
	for (atndx=0; atndx<num_attr; atndx++)
	{
		data_ptr = attr_ptr;
#if MODULEDBG != 0
		uu_dprint(UU_RITRC,(us,"field %s data at 0x%x", atdefs[atndx].attr_name,
						data_ptr));
#endif
		cndx = atdefs[atndx].num_cols;
		if (atdefs[atndx].attr_type == CHARACTER)
		{
			data_offset = atdefs[atndx].attr_off / atdefs[atndx].num_rows;
			cndx = 1;			/* no names for each column */
		}
		else
		{
			data_offset = atdefs[atndx].attr_off / (atdefs[atndx].num_rows *
										cndx);
		}
#if MODULEDBG != 0
		uu_dprint(UU_RITRC,(us," %d rows , %d cols", atdefs[atndx].num_rows, cndx));
#endif
		for (row = 1; row <= atdefs[atndx].num_rows; row++)
		{
			for (col = 1; col <= cndx; col++)
			{
				/* generate the control string for printf to print the field */
				pstr[0] = '\0';		/* clear out previous contents */
				uri_prnt_ctrl_str(row, col, &atdefs[atndx], pstr, ctrl);

				/* dump the field */
				switch (atdefs[atndx].attr_type)
				{
				case KEY_ID:
				case REL_ID:
				case KEY_REF:
					uu_dprint(UU_RTRC,(us, pstr, *(UU_KEY_ID *)data_ptr));
					break;
				case REAL:
					uu_dprint(UU_RTRC,(us, pstr, *(UU_REAL *)data_ptr));
					break;
				case JOIN:
				case INT:
					uu_dprint(UU_RTRC,(us, pstr, *(int *)data_ptr));
					break;
				case FLOAT:
					uu_dprint(UU_RTRC,(us, pstr, *(float *)data_ptr));
					break;
				case DOUBLE:
					uu_dprint(UU_RTRC,(us, pstr, *(double *)data_ptr));
					break;
				case LOGICAL:
					uu_dprint(UU_RTRC,(us, pstr, *(UU_LOGICAL *)data_ptr));
					break;
				case CHARACTER:
					uu_dprint(UU_RTRC,(us, pstr, data_ptr));
					break;
				case STRING:
					uu_dprint(UU_RTRC,(us, "***string***%d", *(int *)data_ptr));
					break;
				}
				data_ptr = data_ptr + data_offset;
				if (atndx == 0 && atdefs[atndx].attr_type == KEY_ID)
				{
					uu_dprint(UU_RTRC,(us,"rel = %d", *(UR_REL_NUM *)data_ptr));
					extra_offset = sizeof(UR_REL_NUM);	/* magic rel num field */
				}
				else
				{
					extra_offset = 0;
				}
			}
		}
#if MODULEDBG != 0
		uu_dprint(UU_RITRC,(us,"offset = %d", atdefs[atndx].attr_off));
#endif
		attr_ptr += atdefs[atndx].attr_off + extra_offset;
	}
	uu_dexit;
#endif
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_dump_atoms(buffer, atom_cnt,
**														rel, list) 
**  dump the atoms in the buffer according to the definition given
**    PARAMETERS   
**       INPUT  : 
**				buffer	area to receive varlist data
**				atom_cnt	# of atoms to write
**				rel		relation number to dump varlist for
**				list		list number to dump
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful (all other error)
**    SIDE EFFECTS : write the indicated varlist
**    WARNINGS     : none
*********************************************************************/

ur_dump_atoms(buffer, atom_cnt, rel, list)
char			*buffer;
int			atom_cnt;
UR_REL_NUM	rel;
int			list;
{
#ifndef UU_DEBUGOFF 
	int			iostat;					/* holds status of i/o calls			*/
	int			status;					/* holds status of unibase calls		*/
	int			lrecl;					/* length of record write				*/
	long			tuple_indx;				/* an entry id							*/
	int			atom_size;				/* atom size for var length data 	*/
	char			*atom_ptr;				/* a pointer to a byte					*/
	struct UR_data	*ent_ptr;			/* a pointer to an entry				*/
	char			*b_ptr;					/* pointer to a byte 					*/
	int			i;
	int			j;
	struct UR_lpacket	*lp_ptr;			/* pointer to a variable list packet*/
	struct attr_def	*atdefs;
	int					num_attr;
	int			atndx;					/* index into atdefs */
	int			atype;					/* the data type of the attribute */
	char			*attr_ptr;
	char			*data_ptr;
	int			data_offset;
	int			row;
	int			col;
	int			rndx;						/* row index of attribute array	*/
	int			cndx;						/* col index of attribute array	*/
	int			rel_typ;
	int			lst_len;					/* length of a list in bytes			*/
	char			buf[80];					/* write buffer for conversion to char */
	char			pstr[80];				/* string to hold print control */
	int			length;
	UU_LOGICAL	ctrl;			/* line feed control */

	uu_denter(UU_RTRC,(us,"ur_dump_atoms(0x%x, %d, %d, %d)",
					buffer, atom_cnt, rel, list));
   status = ur_chk_data_dict();
   if (status)
   {
      uu_dexit;    /* bailout - still no data dictionary */
		return(-1);
   } 
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us,"data dictionary inited."));
#endif

	/* get definition from data dictionary */
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us,"looking up %s", UR_rcb[rel].relname));
#endif
	num_attr = UR_rcb[rel].rel_def->rellen;
	atdefs = UR_rcb[rel].relattr;
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us," ... has %d attributes", num_attr));
#endif
	if(num_attr <= 0)
	{
		status = URM_RELNTFND; /* rel not in data dictionary error */
		uu_dexit;
		return(status);
	}
	num_attr = UR_rcb[rel].lparms[list-1].atom_def->rellen;
	atdefs = UR_rcb[rel].lparms[list-1].atomattr;
	if (num_attr)
	{
#if MODULEDBG != 0
		uu_dprint(UU_RITRC,(us," list %d has %d attributes", list, num_attr));
#endif
		ctrl = UU_FALSE;
#if MODULEDBG != 0
		uu_dprint(UU_RITRC,(us," ... buffer ptr = 0x%x", buffer));
#endif
		attr_ptr = buffer;
		for (i = 0; i < atom_cnt; i++)
		{
#if MODULEDBG != 0
			uu_dprint(UU_RITRC,(us," ... printing atom %d", i));
#endif
			for (atndx=1; atndx<num_attr; atndx++)	/* start with field 1 for atom */
			{
				data_ptr = attr_ptr;
				cndx = atdefs[atndx].num_cols;
				if (atdefs[atndx].attr_type == CHARACTER)
				{
					data_offset = atdefs[atndx].attr_off / atdefs[atndx].num_rows;
					cndx = 1;         /* no names for each column */
				}
				else
				{
					data_offset = atdefs[atndx].attr_off / (atdefs[atndx].num_rows *
												cndx);
				}
				for (row = 1; row <= atdefs[atndx].num_rows; row++)
				{
					for (col = 1; col <= cndx; col++)
					{
						/* generate the control string for printf to print the field */
						pstr[0] = '\0';		/* clear out previous contents */
						uri_prnt_ctrl_str(row, col, &atdefs[atndx], pstr, ctrl);
#if MODULEDBG != 0
						uu_dprint(UU_RITRC,(us," ... printing field %d with string %s",
									atndx, pstr));
#endif

						/* dump the field */
            		switch (atdefs[atndx].attr_type)
            		{
            		case KEY_ID:
            		case REL_ID:
            		case KEY_REF:
#if MODULEDBG != 0
							uu_dprint(UU_RITRC,(us,
									" ... field is a key at 0x%x", data_ptr));
#endif
							uu_dprint(UU_RTRC,(us, pstr, *(UU_KEY_ID *)data_ptr));
               		break;
            		case REAL:
#if MODULEDBG != 0
							uu_dprint(UU_RITRC,(us,
									" ... field is a key at 0x%x", data_ptr));
#endif
							uu_dprint(UU_RTRC,(us, pstr, *(UU_REAL *)data_ptr));
               		break;
            		case JOIN:
            		case INT:
#if MODULEDBG != 0
							uu_dprint(UU_RITRC,(us,
									" ... field is int(or join count) at 0x%x", data_ptr));
#endif
							uu_dprint(UU_RTRC,(us, pstr, *(int *)data_ptr));
               		break;
            		case FLOAT:
#if MODULEDBG != 0
							uu_dprint(UU_RITRC,(us,
									" ... field is a float at 0x%x", data_ptr));
#endif
							uu_dprint(UU_RTRC,(us, pstr, *(float *)data_ptr));
               		break;
            		case DOUBLE:
#if MODULEDBG != 0
							uu_dprint(UU_RITRC,(us,
									" ... field is a double at 0x%x", data_ptr));
#endif
							uu_dprint(UU_RTRC,(us, pstr, *(double *)data_ptr));
               		break;
            		case LOGICAL:
#if MODULEDBG != 0
							uu_dprint(UU_RITRC,(us,
									" ... field is a logical at 0x%x", data_ptr));
#endif
							uu_dprint(UU_RTRC,(us, pstr, *(UU_LOGICAL *)data_ptr));
               		break;
            		case CHARACTER:
#if MODULEDBG != 0
							uu_dprint(UU_RITRC,(us,
									" ... field is character at 0x%x", data_ptr));
#endif
							uu_dprint(UU_RTRC,(us, pstr, data_ptr));
               		break;
            		}
						data_ptr += data_offset;
					} /* for each column */
				} /* for each row */
				attr_ptr += atdefs[atndx].attr_off;
			} /* for each field definition */
		}	/* for each atom */
	}
	uu_dprint(UU_RTRC,(us,"ur_dump_atoms exit status = %d",status));
	uu_dexit;
	return(status);
#endif
}


/*********************************************************************
**    E_FUNCTION :  int ur_dump_mtuple(key)
**       dump master tuple data with a given key
**    PARAMETERS   
**       INPUT  : 
**          key	UU_KEY_ID	key to dump
**       OUTPUT :  
**         	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_dump_mtuple(key)
UU_KEY_ID	key;
{
#ifndef UU_DEBUGOFF 
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	struct UR_data	*bufptr;
	UU_KEY_ID		sub_key;
	int				status;
	UR_lpacket		*lpack;
	int				list;

	uu_denter(UU_RTRC,(us,"ur_dump_mtuple(key=0x%x)",key));
	ur_k2rt(key, &rel, &tuple);
	if (rel == UR_MTUPLE_REL)
	{
		status = ur_get_tuple_ptr(rel, tuple, &bufptr);
		if (status != 0)
		{
			uu_dprint(-1,(us,"oops, can't retrieve"));
		}
		else
		{
			ur_dump_tuple(rel, bufptr);
			for (list = 1; list <= UR_rcb[rel].n_varl; list++)
			{
				status = ur_get_list_packet_ptr(rel, list, bufptr, &lpack);
				if (lpack->atom_cnt)
				{
					uu_dprint(UU_RTRC, (us, "list %d:", list));
					ur_dump_atoms(lpack->list_ptr, lpack->atom_cnt, rel, list);
				}
			}
		}
	}
	else	/* not master key */
	{
		uu_dprint(-1,(us,"Can't dump master tuple with a rel key."));
	}
	uu_dexit;
#endif
}

