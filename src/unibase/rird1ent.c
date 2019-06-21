/*********************************************************************
**    NAME         :  rird1ent.c
**       CONTAINS:
**			ur_rd1_txt_ent()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rird1ent.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:46
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) rird1ent.c 3.2 11/2/87 15:16:21 single"};
#else
static char uu_sccsident[]={"@(#) rird1ent.c 3.2 11/2/87 15:16:21 double"};
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
#include "xfsys0.h"
#include "xenv1.h"
#include "nclver.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_rd1_txt_ent(lu, buffer, num_attr, atdefs) 
**  read a Universal Part File relation entry into Unibase
**    PARAMETERS   
**       INPUT  : 
**          lu			file to read from
**				buffer	area to receive varlist data
**				num_attr	# of fields
**				atdefs	attribute(field) definitions
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful (all other error)
**    SIDE EFFECTS : read the indicated varlist
**    WARNINGS     : none
*********************************************************************/

ur_rd1_txt_ent(lu, buffer, num_attr, atdefs)
int					lu;
char					*buffer;
int					num_attr;
struct attr_def	atdefs[];
{
	int			iostat;			/* holds status of i/o calls			*/
	int			status;			/* holds status of unibase calls		*/
	int			lrecl;			/* length of record read				*/
	long			tuple_indx;		/* an entry id							*/
	int			atom_size;		/* atom size for var length data 	*/
	char			*atom_ptr;		/* a pointer to a byte					*/
	struct UR_data	*ent_ptr;	/* a pointer to an entry				*/
	char			*b_ptr;			/* pointer to a byte 					*/
	int			i;
	struct UR_lpacket	*lp_ptr;	/* pointer to a variable list packet*/
#define	atmax	64
	int			atndx;			/* index into atdefs */
	int			atype;			/* the data type of the attribute */
	char			*attr_ptr;
	char			*data_ptr,*dptr;
	int			data_offset;
	int			extra_offset;
	int			row;
	int			col;
	int			rndx;				/* row index of attribute array	*/
	int			cndx;				/* col index of attribute array	*/
	int			rel_typ;
	int			lst_len;			/* length of a list in bytes			*/
	char			*buf;				/* read buffer for conversion from char */
	char			sstr[80];		/* string to hold scan control */
	FILE			*fd;
	int			chadj;			/* character adjustment */
	char str[80];
	char *pstr;
	int nan, nc, flch,over;
	UU_LOGICAL first=UU_TRUE;

	uu_denter(UU_RTRC,(us,"ur_rd1_txt_ent(%d, 0x%x, %d, 0x%x)", lu,
					buffer, num_attr, atdefs));
	ux_get_os_filedesc(lu, &fd, UX_PRTERRS);
	status = 0;
	attr_ptr = (char *)buffer;
	for (atndx=0; atndx<num_attr; atndx++)
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


		flch = 0;
		for (row = 1; row <= atdefs[atndx].num_rows; row++)
		{
			for (col = 1; col <= cndx; col++)
			{
				nan = 0;
				if (atdefs[atndx].attr_type == CHARACTER)
					status = 1;
				else
				{
					ur_mk_scan_str(&atdefs[atndx], sstr);	/* make scan control */
					dptr = data_ptr;
					if (NCL_infile_version >= 9.950 &&
						(atdefs[atndx].attr_type == JOIN ||
						atdefs[atndx].attr_type == STRING))
							dptr = data_ptr + sizeof(char *);
					UX_FSCANF0((fd, sstr, dptr), status);
				}
				if (status == 0)
				{
					fscanf (fd, "%s", str);
					pstr = str;
					if (!strncmp(pstr,"#QNAN",5))
					{
						col--;
						continue;
					}
					if (str[0] == '-') pstr++;
					pstr[3] = '\0';
					ul_to_upper(pstr);
					if (!strcmp(pstr,"NAN") || !strcmp(pstr,"INF") || pstr[0] == '?' ||
							pstr[0] == '*')
					{
						nan = 1;
						status = 1;
					}
				}
				if (status == 1)
				{
					status = 0;
					uu_dprint(UU_RITRC,(us,"%s[%d][%d] =",atdefs[atndx].attr_name,
								row,col));
           		switch (atdefs[atndx].attr_type)
           		{
           		case KEY_ID:
           		case REL_ID:
           		case KEY_REF:
              		uu_dprint(UU_RITRC,(us,sstr, *(UU_KEY_ID *)data_ptr));
              		break;
           		case REAL:
              		uu_dprint(UU_RITRC,(us,sstr, *(UU_REAL *)data_ptr));
						if (nan == 1) *(UU_REAL *)data_ptr = 0.;
              		break;
           		case JOIN:
					case STRING:
           		case INT:
              		uu_dprint(UU_RITRC,(us,sstr, *(int *)data_ptr));
              		break;
           		case FLOAT:
              		uu_dprint(UU_RITRC,(us,sstr, *(float *)data_ptr));
						if (nan == 1) *(float *)data_ptr = 0.;
              		break;
           		case DOUBLE:
						/* fix format 'cause we can only read and write float */
						*(double *)data_ptr = *(float *)data_ptr;
              		uu_dprint(UU_RITRC,(us,sstr, *(double *)data_ptr));
						if (nan == 1) *(double *)data_ptr = 0.;
              		break;
           		case LOGICAL:
              		uu_dprint(UU_RITRC,(us,sstr, *(UU_LOGICAL *)data_ptr));
              		break;
           		case CHARACTER:
						i = getc(fd);
#if (UU_COMP == UU_WINNT) || (UU_COMP == UU_WIN2K)
						if (i == '\r' )
							i = getc(fd);
#endif
						if (i == '\n')
						{
							i = getc(fd);
#if (UU_COMP == UU_WINNT) || (UU_COMP == UU_WIN2K)
							if (i == '\r' )
								i = getc(fd);
#endif
							flch = 1;
						}
						if (i == EOF)
						{
								status = -1;
								goto exit;
						}
						*data_ptr = i;
						nc = atdefs[atndx].num_cols;
						if (NCL_infile_version < 8.419) 
						{
							for (chadj = 1; chadj < atdefs[atndx].num_cols; chadj++)
							{
								i = getc(fd);
								if (i == EOF)
								{
									status = -1;
									goto exit;
								}
								data_ptr[chadj] = i;
							}
						}
						else
						{
/*
.....vp 3/3/98 if EOL is the only character in the line
.....string is empty and we need backup one charater
*/
/*
.....Changed to use ungetc to backup one character because fsetpos() 
.....does not work for WinNT when reading ud file with no carriage returns.
.....IJD 03 Jun 2002
*/
							if (*data_ptr == '\n') 
							{
								if (flch == 1) ungetc('\n',fd);
								*data_ptr = '\0';
								break;
							}	
/*
.....vp 2/12/98 read only up to end of the line character
.....ignore junk characters from input (can exist in old unibase .ud files)					
							for (chadj = 1; chadj < atdefs[atndx].num_cols; chadj++) 
*/
							chadj = 1;
							do
							{
								i = getc(fd);
#if (UU_COMP == UU_WINNT) || (UU_COMP == UU_WIN2K)
								if (i == '\r' )
									i = getc(fd);
#endif
								if (i == EOF)
								{
									status = -1;
									goto exit;
								}
								data_ptr[chadj] = i;
							} while (chadj < nc && data_ptr[chadj++] != '\n');
						}
/*
.....Push newline into stream so multiple blank lines will work.
.....IJD 03 Jun 2002
*/
						ungetc('\n',fd);
						data_ptr[ul_cut_string(data_ptr,nc)]= '\0';

              		uu_dprint(UU_RITRC,(us,"%s", data_ptr));
              		break;
           		}
				}
				else
				{
					uu_dprint(-1,(us,"ERROR status %d from UX_FSCANF0",status));
					status = -1;
					goto exit;
				}
				data_ptr += data_offset;
				if (atndx == 0 && atdefs[atndx].attr_type == KEY_ID)
				{
					UX_FSCANF0((fd, "%d", data_ptr), status);
					if (status == 1)
					{
						status = 0;
						uu_dprint(UU_RITRC,(us,"rel_num = %d", *(int *)data_ptr));
					}
					else
					{
						uu_dprint(-1,(us,"ERROR status %d from UX_FSCANF0",status));
						status = -1;
						goto exit;
					}
					extra_offset = sizeof(UR_REL_NUM);  /* magic rel num field */
				}
				else
				{
					extra_offset = 0;
				}
			} /* for each column */
		} /* for each row */
/*
.....Older Unibases use a different Join structure
.....and must be adjusted to lie on the correct
.....boundaries when read in
.....Bobby - 9/26/12
*/
		if (NCL_infile_version < 9.950 &&
			(atdefs[atndx].attr_type == JOIN || atdefs[atndx].attr_type == STRING))
		{
			if (first)
			{
				over = atdefs[atndx].attr_here % sizeof(char *);
				if (over != 0)
				{
					attr_ptr -= over;
				}
				first = UU_FALSE;
			}
			attr_ptr += sizeof(struct UR_lpacket_99);
		}
/*
.....Use standard offset values
*/
		else
			attr_ptr += atdefs[atndx].attr_off + extra_offset;
	} /* for each field definition */
	status = 0;
exit:
	uu_dprint(UU_RTRC,(us,"ur_rd1_txt_ent exit status = %d",status));
	uu_dexit;
	return(status);
}
