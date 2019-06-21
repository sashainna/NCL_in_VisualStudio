/*********************************************************************
**    NAME         :  riwtatms1.c
**       CONTAINS:
**			ur_wrt_txt_atoms1()
**  Support handling of NaN values:
**          te_sprintf_float()
**          te_sprintf_real()
**          te_sprintf_double()
**          te_trap_nan()
**    COPYRIGHT 1990 (c) JONATHAN Corp.  All Rights Reserved.
**    DATE AND TIME OF LAST  MODIFICATION
**        riwtatms1.c , 25.1
**    MODULE NAME AND RELEASE LEVEL 
**        04/29/15 , 15:11:49
*********************************************************************/

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
#include "adrf.h"

#if UU_COMP==UU_VAXVMS
/*
.....Flags setup to handle recover from SIGILL (reserve operand fault)
.....on VMS.
*/
#include signal

int t_flag;
float *t_float;
UU_REAL *t_real;
double *t_double;
#endif
/*********************************************************************
**    E_FUNCTION     :  status = ur_wrt_txt_atoms1(lu, buffer, atom_cnt,
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
**    NOTE:  Modified to overcome the longword byte alignment
**    problem encountered on the sun4 and sg4d for the
**    UA_txtblk_rec structure.  the data field origin, needs
**    to start on a longword boundary.  this will be corrected
**    by changing isysddl.ddl for DDC Release 2.7.  However
**    for release 2.6 to enable dcnv to work, this code was
**    modified to use tmp variables to pass the sprintf for
**    the UU_REAL fields in the txtblk_rec structure which are
**    positioned after the char array: tstring.  CML 3/90
*********************************************************************/

ur_wrt_txt_atoms1(lu, buffer, atom_cnt, atdefs, num_attr)
int					lu;
char					*buffer;
int					atom_cnt;
struct attr_def	atdefs[];
int					num_attr;
{
	int			iostat;					/* holds status of i/o calls			*/
	int			status;					/* holds status of unibase calls		*/
	int			lrecl;					/* length of record write				*/
	int         txtblk_flag;         /* flag to look for txtblk fields */
	long			tuple_indx;				/* an entry id							*/
	int			atom_size;				/* atom size for var length data 	*/
	char			*atom_ptr;				/* a pointer to a byte					*/
	struct UA_txtblk_rec *txtbag;
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
	int			nc, length, strdef;
	static int delflg = 0;

	uu_denter(UU_RTRC,(us,"ur_wrt_txt_atoms(%d, 0x%x, %d, 0x%x, %d)", lu,
					buffer, atom_cnt, atdefs, num_attr));
	status = 0;
	attr_ptr = buffer;
	txtblk_flag=0;
	strdef = strncmp(atdefs[1].attr_name,"string",6) == 0;
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
/*
.....................Handled the converting of UU_REAL values to a character
.....................string in a machine dependant way.  This is so that
.....................Reserve Operand Fault signals on VMS can be dealt with.
*/
					te_sprintf_real(buf, pstr, (UU_REAL *)data_ptr);
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
/*
.....................Handled the converting of float values to a character
.....................string in a machine dependant way.  This is so that
.....................Reserve Operand Fault signals on VMS can be dealt with.
*/
					te_sprintf_float(buf,pstr, (float *)data_ptr);
					length = strlen(buf);
					break;
            	case DOUBLE:
/*
.....................Handled the converting of double values to a character
.....................string in a machine dependant way.  This is so that
.....................Reserve Operand Fault signals on VMS can be dealt with.
*/
					te_sprintf_double(buf,pstr, (double *)data_ptr);
					length = strlen(buf);
					break;
            	case LOGICAL:
					sprintf(buf, pstr, *(UU_LOGICAL *)data_ptr);
					length = strlen(buf);
					break;
            	case CHARACTER:
					sprintf(buf, pstr, *data_ptr);
/*
.....vp 2/12/98 trim string to process older versions of unibase
.....(.ud files) where some junk could trail a valid part of string
.....4/24/98 but do not purge strings in txt record (there are valid nl
.....characters and can be also other invisible stuff.
*/
					if (strdef) nc = 1;
					else
					{
						length = atdefs[atndx].num_cols;
						nc = ul_cut_string (data_ptr,length);
						if (nc == 0) while (data_ptr[nc] == ' ' && nc < length) nc++;
					}
					length = 1;
					for (j = 1; j < nc; j++)
					{
						status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
						sprintf(buf, pstr, *(data_ptr + j));
					}
					if (nc > 0 && buf[length-1] !='\0') length = 2;
					buf[length-1] = '\n'; 
					uu_dprint(UU_RITRC,(us,"%d:'%c'",atdefs[atndx].num_cols,*buf));
					break;
            	}
					status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
					data_ptr += data_offset;
				} /* for each column */
			} /* for each row */
			attr_ptr += atdefs[atndx].attr_off;

			/** If this record is the UA_txtblk_rec 
			*** setup to process the UU_REAL fields positioned
			*** after the char arrays ***/
	
			if (strncmp(atdefs[atndx].attr_name,"fontname",8) == 0)
			{
				txtbag = (struct UA_txtblk_rec*)buffer;
				txtblk_flag = 1;
			}

			if (strncmp(atdefs[atndx].attr_name,"tstring",7) == 0)
			{
				if (txtblk_flag == 1)

				{
#if (UU_COMP==UU_IRIS4D || UU_COMP==UU_HPUX || UU_COMP==UU_WINNT || UU_COMP == UU_WIN2K)
/*
.....vp 1.22.97 removed not rs6000 since it is necessary also for
.....rs6000 to make correct offset in drafting text for origin field 
.....Bob put this ifndef and possibly never tested on rs6000
  #ifndef UU_RS6000
*/
					attr_ptr += 2;
					uu_dprint(UU_RTRC,(us,"Longword alignment necessary"));
/*#endif*/
#else
#if ((UU_COMP==UU_SUN && UU_SUNTYPE==UU_SUN_SUN4) || UU_COMP==UU_DECUNIX)
					attr_ptr += 2;
					uu_dprint(UU_RTRC,(us,"Longword alignment necessary"));
#else
					uu_dprint(UU_RTRC,(us,"No alignment necessary"));
#endif
#endif
				}
				else
					txtblk_flag = 0;
			}
			/*MILLS: to correct alignment for alt label locs */
			if ((strncmp(atdefs[atndx].attr_name,"del",3) == 0) &&
				(strlen(atdefs[atndx].attr_name) == 3))
				{
				delflg = 1;
				}
			if ((strncmp(atdefs[atndx].attr_name,"index",5) == 0) && delflg)
				{
				delflg = 0;
#if (UU_COMP==UU_IRIS4D || UU_COMP==UU_HPUX || UU_COMP==UU_WINNT || UU_COMP == UU_WIN2K)
/*
.....vp 1.22.97 removed not rs6000 since it is necessary also for
.....rs6000 to make correct offset in drafting text for origin field 
.....Bob put this ifndef and possibly never tested on rs6000
  #ifndef UU_RS6000
*/
					attr_ptr += 4;
					uu_dprint(UU_RTRC,(us,"Longword alignment necessary"));
/*#endif*/
#else
#if ((UU_COMP==UU_SUN && UU_SUNTYPE==UU_SUN_SUN4) || UU_COMP==UU_DECUNIX)
					attr_ptr += 4;
					uu_dprint(UU_RTRC,(us,"Longword alignment necessary"));
#else
					uu_dprint(UU_RTRC,(us,"No alignment necessary"));
#endif
#endif
				}
		} /* for each field definition */
	}	/* for each atom */
	uu_dprint(UU_RTRC,(us,"ur_wrt_txt_atoms exit status = %d",status));
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  te_sprintf_float(buf, pstr, fval)
**         Write fval into char buffer 'buf' using control string pstr.
**    PARAMETERS   
**       INPUT  : 
**          buf			buffer to write to
**			pstr	    print control string           
**			fval       float value to write to buffer
**       OUTPUT :  
**          none
**    RETURNS      : 
**				none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
te_sprintf_float(buf,pstr,fval)
char *buf, *pstr;
float *fval;
	{
	char *pbuf;
#if UU_COMP==UU_VAXVMS
/*
.....Set up variables in case of SIGILL on VMS (Nan values).
.....VMS systems generate a SIGILL signal which gets handled in the routine
.....te_trap_nan().
*/
	t_flag = FLOAT;
	t_float = fval;
#endif
	sprintf(buf, pstr, *fval);
	pbuf = buf;
	ul_to_upper (pbuf);

#if UU_COMP!=UU_VAXVMS
/*
....On UNIX machines Nan values are output as "NaN".  We correct this by
....setting the value to 0.0.  This handled negative NaN values too.
*/
	if (pbuf[0] == '-') pbuf++;
	if (pbuf[0] == 'N' || pbuf[0] == '?' || pbuf[0] == 'I')/* NaN? */
		{
		*(float *)fval = 0.0;
		sprintf(buf, pstr, *(float *)fval);
		}
#endif

	return;
	}
/*********************************************************************
**    E_FUNCTION     :  te_sprintf_real(buf, pstr, rval)
**         Write rval into char buffer 'buf' using control string pstr.
**    PARAMETERS   
**       INPUT  : 
**          buf			buffer to write to
**			pstr	    print control string           
**			rval        real value to write to buffer
**       OUTPUT :  
**          none
**    RETURNS      : 
**				none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
te_sprintf_real(buf,pstr,rval)
char *buf, *pstr;
UU_REAL *rval;
	{
	char *pbuf;
#if UU_COMP==UU_VAXVMS
/*
.....Set up variables in case of SIGILL on VMS (Nan values).
.....VMS systems generate a SIGILL signal which gets handled in the routine
.....te_trap_nan().
*/
	t_flag = REAL;
	t_real = rval;
#endif
	sprintf(buf, pstr, *rval);
	pbuf = buf;
	ul_to_upper (pbuf);

#if UU_COMP!=UU_VAXVMS
/*
....On UNIX machines Nan values are output as "NaN".  We correct this by
....setting the value to 0.0.  This handled negative NaN values too.
*/
	if (pbuf[0] == '-') pbuf++;
	if (pbuf[0] == 'N' || pbuf[0] == '?' || pbuf[0] == 'I')/* NaN? */
		{
		*rval = 0.0;
		sprintf(buf, pstr, *rval);
		}
#endif

	return;
	}
/*********************************************************************
**    E_FUNCTION     :  te_sprintf_double(buf, pstr, dval)
**         Write dval into char buffer 'buf' using control string pstr.
**    PARAMETERS   
**       INPUT  : 
**          buf			buffer to write to
**			pstr	    print control string           
**			dval        double value to write to buffer
**       OUTPUT :  
**          none
**    RETURNS      : 
**				none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
te_sprintf_double(buf,pstr,dval)
char *buf, *pstr;
UU_REAL *dval;
	{
	char *pbuf;
#if UU_COMP==UU_VAXVMS
/*
.....Set up variables in case of SIGILL on VMS (Nan values).
.....VMS systems generate a SIGILL signal which gets handled in the routine
.....te_trap_nan().
*/
	t_flag = DOUBLE;
	t_double = dval;
#endif
	sprintf(buf, pstr, *dval);
	pbuf = buf;
	ul_to_upper (pbuf);

#if UU_COMP!=UU_VAXVMS
/*
....On UNIX machines Nan values are output as "NaN".  We correct this by
....setting the value to 0.0.  This handled negative NaN values too.
*/
	if (pbuf[0] == '-') pbuf++;
	if (pbuf[0] == 'N' || pbuf[0] == '?' || pbuf[0] == 'I')/* NaN? */
		{
		*(double *)dval = 0.0;
		sprintf(buf, pstr, *(double *)dval);
		}
#endif

	return;
	}
#if UU_COMP==UU_VAXVMS
/*********************************************************************
**    E_FUNCTION     :  status = te_trap_nan()
**      Signal trapping routine for NaN values on VMS.
**      SIGILL - reserve operand fault.
**    PARAMETERS   
**       INPUT  : 
**			none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
te_trap_nan()
	{
	int te_trap_nan();

/*
.....Reset signal trapping in case we get another
*/
	signal(SIGILL, te_trap_nan);

/*
.....Set bad value to zero 
*/
	if (t_flag == FLOAT)
		*t_float = 0.0;
	else if (t_flag == REAL)
		*t_real = 0.0;
	else if (t_flag == DOUBLE)
		*t_double = 0.0;

	return;
	}
#endif
