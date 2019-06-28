/*********************************************************************
**    NAME         :  riwtatms.c
**       CONTAINS:
**			ur_wrt_txt_atoms()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riwtatms.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:49
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) riwtatms.c 3.2 11/2/87 15:19:55 single"};
#else
static char uu_sccsident[]={"@(#) riwtatms.c 3.2 11/2/87 15:19:55 double"};
#endif
#endif

#include "usysdef.h"
#include	"udebug.h"
#define	UR_BLKRCL	1024	/* block record length					*/
#include "umoveb.h"
#include "uhep.h"
#include "ribase.h"
#include "ritrnerr.h"
#include "riddldef.h"
#include "rerrdef.h"
#include "xenv1.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_wrt_txt_atoms(lu, buffer, atom_cnt,
**														atdefs, num_attr) 
**  write a Universal Part File varlist into Unibase
**    PARAMETERS   
**       INPUT  : 
**          lu			file to write to
**				buffer	area to receive varlist data
**				atom_cnt	# of atoms to write
**				atdefs	attribute(field) definitions
**				num_attr	# of fields
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful (all other error)
**    SIDE EFFECTS : write the indicated varlist
**    WARNINGS     : none
*********************************************************************/

ur_wrt_txt_atoms(lu, buffer, atom_cnt, atdefs, num_attr)
int					lu;
char					*buffer;
int					atom_cnt;
struct attr_def	atdefs[];
int					num_attr;
{
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
#define	atmax	64
	int			atndx;					/* index into atdefs */
	int			atype;					/* the data type of the attribute */
	char			*attr_ptr;
	char			*data_ptr,*dptr;
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

	uu_denter(UU_RTRC,(us,"ur_wrt_txt_atoms(%d, 0x%x, %d, 0x%x, %d)", lu,
					buffer, atom_cnt, atdefs, num_attr));
	status = 0;
	attr_ptr = buffer;
	for (i = 0; i < atom_cnt; i++)
	{
		for (atndx=1; atndx<num_attr; atndx++)	/* start with field 1 for atom */
		{
			data_ptr = attr_ptr;
			cndx = atdefs[atndx].num_cols;
			if (atdefs[atndx].attr_type == CHARACTER)
			{
				ur_mk_print_str(&atdefs[atndx], pstr); /* make print control */
				data_offset = atdefs[atndx].attr_off / atdefs[atndx].num_rows;
				cndx = 1;         /* no names for each column */
			}
			else
			{
				ur_mk_print_str(&atdefs[atndx], pstr);	/* make print control */
				data_offset = atdefs[atndx].attr_off / (atdefs[atndx].num_rows *
											cndx);
				strcat(pstr, "\n");
			}
			for (row = 1; row <= atdefs[atndx].num_rows; row++)
			{
				for (col = 1; col <= cndx; col++)
				{
            	switch (atdefs[atndx].attr_type)
            	{
            	case KEY_ID:
            	case REL_ID:
            	case KEY_REF:
               	sprintf(buf, pstr, *(UU_KEY_ID *)data_ptr);
						length = strlen(buf);
               	break;
            	case REAL:
               	sprintf(buf, pstr, *(UU_REAL *)data_ptr);
						if (buf[0] == 'N')	/* NaN? */
						{
							*(UU_REAL *)data_ptr = 0.0;
              			sprintf(buf, pstr, *(UU_REAL *)data_ptr);
						}
						length = strlen(buf);
               	break;
            	case JOIN:
            	case STRING:
						dptr = data_ptr + sizeof(char *);
               	sprintf(buf, pstr, *(int *)dptr);
						length = strlen(buf);
               	break;
            	case INT:
               	sprintf(buf, pstr, *(int *)data_ptr);
						length = strlen(buf);
               	break;
            	case FLOAT:
               	sprintf(buf, pstr, *(float *)data_ptr);
						if (buf[0] == 'N')	/* NaN? */
						{
							*(UU_REAL *)data_ptr = 0.0;
              			sprintf(buf, pstr, *(UU_REAL *)data_ptr);
						}
						length = strlen(buf);
               	break;
            	case DOUBLE:
               	sprintf(buf, pstr, *(double *)data_ptr);
						if (buf[0] == 'N')	/* NaN? */
						{
							*(UU_REAL *)data_ptr = 0.0;
              			sprintf(buf, pstr, *(UU_REAL *)data_ptr);
						}
						length = strlen(buf);
               	break;
            	case LOGICAL:
               	sprintf(buf, pstr, *(UU_LOGICAL *)data_ptr);
						length = strlen(buf);
               	break;
            	case CHARACTER:
               	sprintf(buf, pstr, *data_ptr);
               	for (j = 1; j < atdefs[atndx].num_cols; j++)
               	{
                  	length = 1;
                  	status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
                  	sprintf(buf, pstr, *(data_ptr + j));
               	}
						buf[1] = '\n';
						length = 2;
						uu_dprint(UU_RITRC,(us,"%d:'%c'",atdefs[atndx].num_cols,*buf));
               	break;
            	}
					status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
					data_ptr += data_offset;
				} /* for each column */
			} /* for each row */
			attr_ptr += atdefs[atndx].attr_off;
		} /* for each field definition */
	}	/* for each atom */
	uu_dprint(UU_RTRC,(us,"ur_wrt_txt_atoms exit status = %d",status));
	uu_dexit;
	return(status);
}

