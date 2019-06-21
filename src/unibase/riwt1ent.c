/*********************************************************************
**    NAME         :  riwt1atms.c
**       CONTAINS:
**			ur_wrt1_txt_ent()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riwt1ent.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:48
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) riwt1ent.c 3.2 11/2/87 15:19:35 single"};
#else
static char uu_sccsident[]={"@(#) riwt1ent.c 3.2 11/2/87 15:19:35 double"};
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
**    E_FUNCTION     :  status = ur_wrt1_txt_ent(lu, buffer, num_attr, atdefs) 
**  write a Universal Part File relation entry into Unibase
**    PARAMETERS   
**       INPUT  : 
**          lu			file to write to
**				buffer	area to receive relation entry data
**				num_attr	# of fields
**				atdefs	attribute(field) definitions
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful (all other error)
**    SIDE EFFECTS : write the indicated relation entry
**    WARNINGS     : none
*********************************************************************/

ur_wrt1_txt_ent(lu, buffer, num_attr, atdefs)
int					lu;
char					*buffer;
int					num_attr;
struct attr_def	atdefs[];
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
	struct UR_lpacket	*lp_ptr;			/* pointer to a variable list packet*/
#define	atmax	64
	int			atndx;					/* index into atdefs */
	int			atype;					/* the data type of the attribute */
	char			*attr_ptr;
	char			*data_ptr,*dptr;
	int			data_offset;
	int			extra_offset;
	int			row;
	int			col;
	int			rndx;						/* row index of attribute array	*/
	int			cndx;						/* col index of attribute array	*/
	int			rel_typ;
	int			lst_len;					/* length of a list in bytes			*/
	char			buf[80], *pbuf;		/* write buffer for conversion to char */
	char			pstr[80];				/* string to hold print control */
	int			length, nc;

	uu_denter(UU_RTRC,(us,"ur_wrt1_txt_ent(%d, 0x%x, %d, 0x%x)", lu,
					buffer, num_attr, atdefs));
	status = 0;
	attr_ptr = (char *)buffer;
	for (atndx=0; atndx<num_attr; atndx++)
	{
		data_ptr = attr_ptr;
		cndx = atdefs[atndx].num_cols;
		ur_mk_print_str(&atdefs[atndx], pstr);	/* make print control */
		uu_dprint(UU_RITRC,(us,"field %s print str= '%s'",atdefs[atndx].attr_name,
				pstr));
		if (atdefs[atndx].attr_type == CHARACTER)
		{
			data_offset = atdefs[atndx].attr_off / atdefs[atndx].num_rows;
			cndx = 1;         /* no names for each column */
			uu_dprint(UU_RITRC,(us,"%d char field:",atdefs[atndx].num_cols));
		}
		else
		{
			data_offset = atdefs[atndx].attr_off / (atdefs[atndx].num_rows *
										cndx);
			strcat(pstr, "\n");
		}
		for (row = 1; row <= atdefs[atndx].num_rows; row++)
		{
			for (col = 1; col <= cndx; col++)
			{
				pbuf = buf;
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
					ul_to_upper (buf);
					if (buf[0] == '-') pbuf++;
					if (pbuf[0] == 'N' || pbuf[0] == '?' || pbuf[0] == 'I')	/* NaN? */
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
					ul_to_upper (buf);
					if (buf[0] == '-') pbuf++;
					if (pbuf[0] == 'N' || pbuf[0] == '?' || pbuf[0] == 'I')	/* NaN? */
					{
						*(UU_REAL *)data_ptr = 0.0;
              		sprintf(buf, pstr, *(UU_REAL *)data_ptr);
					}
					length = strlen(buf);
              	break;
           	case DOUBLE:
              	sprintf(buf, pstr, *(double *)data_ptr);
					ul_to_upper (buf);
					if (buf[0] == '-') pbuf++;
					if (pbuf[0] == 'N' || pbuf[0] == '?' || pbuf[0] == 'I')	/* NaN? */
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
              	sprintf(buf, pstr, *(data_ptr));
/*
.....vp 2/18/98 trim strings to the last valid character
.....end terminate it with LF
*/
					nc = ul_cut_string (data_ptr,atdefs[atndx].num_cols);
					length = 1;
					for (i = 1; i < nc; i++)
					{
						uu_dprint(UU_RITRC,(us,"%d:'%c'",i,*buf));
						status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
              		sprintf(buf, pstr, *(data_ptr + i));
					}
					if (nc > 0) length = 2;
					buf[length-1] = '\n';
					uu_dprint(UU_RITRC,(us,"%d:'%c'",atdefs[atndx].num_cols,*buf));
              	break;
           	}
				status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
				data_ptr += data_offset;
				if (atndx == 0 && atdefs[atndx].attr_type == KEY_ID)
				{
					sprintf(buf, "%d\n", *(UR_REL_NUM *)data_ptr);
					length = strlen(buf);
					status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
					extra_offset = sizeof(UR_REL_NUM);  /* magic rel num field */
				}
				else
				{
					extra_offset = 0;
				}
			} /* for each column */
		} /* for each row */
		attr_ptr += atdefs[atndx].attr_off + extra_offset;
	} /* for each field definition */
	uu_dprint(UU_RTRC,(us,"ur_wrt1_txt_ent exit status = %d",status));
	uu_dexit;
	return(status);
}

