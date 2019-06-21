/*********************************************************************
**    NAME         :  redmpmdl.c
**       CONTAINS:
**       ur_dump_modal()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       redmpmdl.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:28
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) redmpmdl.c 3.3 3/3/88 09:57:20 single"};
#else
static char uu_sccsident[]={"@(#) redmpmdl.c 3.3 3/3/88 09:57:20 double"};
#endif
#endif

#include "udebug.h"
#include "usysdef.h"
#include "rbase.h"
#include "ribase.h"
#include "rienvtab.h"
#include "riddldef.h"
#include "rerrdef.h"

/*********************************************************************
**    E_FUNCTION :  int ur_dump_modal(mname, prefname, nflag)
**       dump data associated with the named modal
**    PARAMETERS   
**       INPUT  : 
**          mname		char[]		name of modal variable to print
**				prefname char[]		prefix in modal table
**				nflag		UU_LOGICAL	dump new version instead if true
**       OUTPUT :  
**         	none
**    RETURNS      : none
**    SIDE EFFECTS : prints the modal specified in the trace file
**    WARNINGS     : none
*********************************************************************/

ur_dump_modal(mname, prefname, nflag)
char			mname[];		/* modal name */
char			prefname[];	/* prefix name */
UU_LOGICAL	nflag;		/* flag for print new version */
{
#ifndef UU_DEBUGOFF 
	extern struct UR_env_table_rec UR_environ_table[];
	struct UR_data	*bag;
	int		i;
	UU_LOGICAL	found;
	int		status;			/* return status */
#define  atmax 64
   struct attr_def   atdefs[atmax]; /* attribute definitions */
   int      num_attr;		/* number of attributes parsed */
   int      atndx;			/* index into atdefs */
   int      atype;			/* the data type of the attribute */
   struct attr_def   v_adefs[atmax];/* attribute definitions - varlist */
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

   uu_denter(UU_RTRC,(us,"ur_dump_modal(name=%s,prefix=%s,new=%d)", mname,
				prefname, nflag));
   status = ur_chk_data_dict();
   if (status)
   {
		uu_dprint(-1,(us,"ERROR, unable to load data dictionary."));
      uu_dexit;    /* bailout - still no data dictionary */
		return(-1);
   } 
	/* get definition from data dictionary */
   num_attr = ur_data_dict(atdefs, atmax, mname, &rel_typ);
	if(num_attr <= 0)
	{
		uu_dprint(-1,(us,"ERROR, modal %s not known.", mname));
		status = URM_RELNTFND; /* rel not in data dictionary error */
		uu_dexit;
		return(status);
	}
	/* find the modal in the environment table */
	found = UU_FALSE;
	strcpy(pstr, prefname);
	strcat(pstr, mname);		/* construct table form of name */
	for(i = 0; UR_environ_table[i].name[0] != '\0'; i++)
	{
		if(strcmp(pstr, UR_environ_table[i].name) == 0)
		{
			found = UU_TRUE;
			break;
		}
	}
	if (!found)
	{
		uu_dprint(-1,(us,"ERROR, modal %s not in modal table",mname));
		uu_dexit;
		return(-1);
	}
	bag = (struct UR_data *)UR_environ_table[i].adrs;	/* start address for the modal */
	if (nflag) bag = (struct UR_data *)UR_environ_table[i].new_adrs;

	/* for each field of the definition:*/
	ctrl = UU_FALSE;
	attr_ptr = (char *)bag;
	for (atndx=0; atndx<num_attr; atndx++)
	{
		data_ptr = attr_ptr;
		uu_dprint(UU_RITRC,(us,"field %s data at 0x%x", atdefs[atndx].attr_name,
						data_ptr));
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
		uu_dprint(UU_RITRC,(us," %d rows , %d cols", atdefs[atndx].num_rows, cndx));
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
				}
				data_ptr = data_ptr + data_offset;
				if (atndx == 0 && atdefs[atndx].attr_type == KEY_ID)
				{
					uu_dprint(UU_RTRC,(us,"rel_num = %d", *(UR_REL_NUM *)data_ptr));
					extra_offset = sizeof(UR_REL_NUM);	/* magic rel num field */
				}
				else
				{
					extra_offset = 0;
				}
			}
		}
		attr_ptr += atdefs[atndx].attr_off + extra_offset;
	}
	uu_dexit;
	return(status);
#endif
}
