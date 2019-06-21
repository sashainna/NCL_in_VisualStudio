/*********************************************************************
**    NAME         :  rirdatms.c
**       CONTAINS:
**			ur_rd_txt_atoms()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rirdatms.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:46
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
#include "xfsys0.h"
#include "xenv1.h"
#include "nclver.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_rd_txt_atoms(lu, buffer, atom_cnt,
**														atdefs, num_attr) 
**  read a Universal Part File varlist into Unibase
**    PARAMETERS   
**       INPUT  : 
**          lu			file to read from
**				buffer	area to receive varlist data
**				atom_cnt	# of atoms to read
**				atdefs	attribute(field) definitions
**				num_attr	# of fields
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful (all other error)
**    SIDE EFFECTS : read the indicated varlist
**    WARNINGS     : none
*********************************************************************/

ur_rd_txt_atoms(lu, buffer, atom_cnt, atdefs, num_attr)
int					lu;
char					*buffer;
int					atom_cnt;
struct attr_def	atdefs[];
int					num_attr;
{
	int			iostat;					/* holds status of i/o calls			*/
	int			status;					/* holds status of unibase calls		*/
	int			lrecl;					/* length of record read				*/
	long			tuple_indx;				/* an entry id							*/
	int         txtblk_flag;
	int			atom_size;				/* atom size for var length data 	*/
	char			*atom_ptr;				/* a pointer to a byte					*/
	struct UA_txtblk_rec *txtbag;
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
	char			sstr[80];				/* string to hold scan control */
	FILE			*fd;
	int         chadj;         /* character adjustment */
	UU_REAL     myreal;
	int nc, flch, nan;  /* vp 3/3/98 'nan' not used for now but some day.. see rird1ent.c */
	fpos_t pos;
	static int delflg = 0;
	int strdef;

	uu_denter(UU_RTRC,(us,"ur_rd_txt_atoms(%d, 0x%x, %d, 0x%x, %d)", lu,
					buffer, atom_cnt, atdefs, num_attr));
	ux_get_os_filedesc(lu, &fd, UX_PRTERRS);
	status = 0;
	attr_ptr = buffer;
	strdef = strncmp(atdefs[0].attr_name,"string",6) == 0;
	for (i = 0; i < atom_cnt; i++)
	{
		txtblk_flag=0;
		for (atndx=1; atndx<num_attr; atndx++)	/* start at field 1 for atoms */
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
					ur_mk_scan_str(&atdefs[atndx], sstr);	/* make scan control */
					uu_dprint(UU_RITRC,(us,"sstr = '%s'",sstr));
					if (strncmp(atdefs[atndx].attr_name,"fontname",8) ==0)
					{
						txtbag = (struct UA_txtblk_rec*)buffer;
						txtblk_flag = 1;
					}
					fgetpos (fd,&pos);
					dptr = data_ptr;
					if (NCL_infile_version >= 9.950 &&
						atdefs[atndx].attr_type == JOIN ||
						atdefs[atndx].attr_type == STRING)
							dptr = data_ptr + sizeof(char *);
					UX_FSCANF0((fd, sstr, dptr), status);
					if (status != 1)
					{
						uu_dprint(-1,(us,"ERROR status from UX_FSCANF0 %d", status));
						uu_dexit;
						return(status);
					}
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
                  break;
               case JOIN:
						data_ptr = attr_ptr;
               case INT:
                  uu_dprint(UU_RITRC,(us,sstr, *(int *)data_ptr));
                  break;
               case FLOAT:
                  uu_dprint(UU_RITRC,(us,sstr, *(float *)data_ptr));
                  break;
               case DOUBLE:
                  /* fix format 'cause we can only read and write float */
                  *(double *)data_ptr = *(float *)data_ptr;
                  uu_dprint(UU_RITRC,(us,sstr, *(double *)data_ptr));
                  break;
               case LOGICAL:
                  uu_dprint(UU_RITRC,(us,sstr, *(UU_LOGICAL *)data_ptr));
                  break;
               case CHARACTER:
#if (UU_COMP == UU_WINNT) || (UU_COMP == UU_WIN2K)
						if (*data_ptr == '\r')
							UX_FSCANF0((fd, sstr, data_ptr), status);
#endif
						if (*data_ptr == '\n')
						{
							UX_FSCANF0((fd, sstr, data_ptr), status);
#if (UU_COMP == UU_WINNT) || (UU_COMP == UU_WIN2K)
							if (*data_ptr == '\r')
								UX_FSCANF0((fd, sstr, data_ptr), status);
#endif
							flch = 1;
							if (status != 1)
							{
								uu_dprint(-1,(us,"ERROR status %d from UX_FSCANF0",
											status));
								status = -1;
								uu_dexit;
								return(status);
							}
						}
						nc = atdefs[atndx].num_cols;
/*
...vp 4/24/98 old unibase character strings are read in same way
...as before and string in txt record are is also not cleaned 
*/
						if (NCL_infile_version < 8.419 || strdef)
						{
							for (chadj = 1; chadj < atdefs[atndx].num_cols; chadj++)
							{
								uu_dprint(UU_RITRC,(us,"0x%x",(int)(*((char *)(data_ptr + chadj -1)))));
								UX_FSCANF0((fd, sstr, data_ptr + chadj), status);
								if (status != 1)
								{
									uu_dprint(-1,(us,
											"ERROR status from UX_FSCANF0 %d at character %d",
									status, chadj));
									uu_dexit;
									return(status);
								}
							}
						}
						else
						{
/*
.....vp 3/3/98 if EOL is the only character in the line
.....string is empty and we need backup one charater
*/
                  	if (*data_ptr == '\n')
                  	{
								if (flch == 1) fsetpos (fd,&pos);
                     	*data_ptr = '\0';
                     	break;
                  	} 
/*
.....vp 2/12/98 read only up to the end of line character 
.....ignore junk characters from input (can exist in old unibase .ud files)  
                  for (chadj = 1; chadj < atdefs[atndx].num_cols; chadj++)
*/

                  	chadj = 1;
							do
                  	{
								uu_dprint(UU_RITRC,(us,"0x%x",(int)(*((char *)(data_ptr + chadj -1)))));
								UX_FSCANF0((fd, sstr, data_ptr + chadj), status);
#if (UU_COMP == UU_WINNT) || (UU_COMP == UU_WIN2K)
								if (*data_ptr == '\r')
									UX_FSCANF0((fd, sstr, data_ptr + chadj), status);
#endif
								if (status != 1)
								{
									uu_dprint(-1,(us,
											"ERROR status from UX_FSCANF0 %d at character %d",
											status, chadj));
									uu_dexit;
									return(status);
								}
							} while (chadj < nc && data_ptr[chadj++] != '\n');
						}
/*
...vp 4/24/98 strings in txt record are not terminated by null
...because length of string is defined by atom_cnt
*/
						if (!strdef) data_ptr[ul_cut_string(data_ptr,nc)] = '\0';

						uu_dprint(UU_RITRC,(us,"0x%x",(int)(*((char *)(data_ptr + chadj -1)))));
                  uu_dprint(UU_RITRC,(us,"%s", data_ptr));
                  break;
               }
					data_ptr += data_offset;
				} /* for each column */
			} /* for each row */
			attr_ptr += atdefs[atndx].attr_off;
			if (strncmp(atdefs[atndx].attr_name,"tstring",7) ==0)
				if (txtblk_flag == 1)
				{
#if (UU_COMP==UU_IRIS4D || UU_COMP==UU_HPUX || UU_COMP==UU_WINNT || UU_COMP == UU_WIN2K)
/*
.....vp 1.22.97 removed not rs6000 since it is necessary also for
.....rs6000 to make correct offset in drafting text for origin filed
.....Bob put this ifndef and possibly never tested on rs6000
  #ifndef UU_RS6000
*/

					attr_ptr+=2;
					uu_dprint(UU_RTRC,(us,"Longword alignment necessary"));
/*#endif*/
#else
#if ((UU_COMP==UU_SUN && UU_SUNTYPE==UU_SUN_SUN4) || UU_COMP==UU_DECUNIX)
					attr_ptr+=2;
					uu_dprint(UU_RTRC,(us,"Longword alignment necessary"));
#else
					uu_dprint(UU_RTRC,(us,"No alignment necessary"));
#endif
#endif
				}
				else
					txtblk_flag = 0;

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
.....rs6000 to make correct offset in drafting text for origin filed
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
	status = 0;
	uu_dprint(UU_RTRC,(us,"ur_rd_txt_atoms exit status = %d",status));
	uu_dexit;
	return(status);
}
