/*********************************************************************
**    NAME         :  riread1d.c
**       CONTAINS:
**       ur_read1def()
**       ur_read1field()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riread1d.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:47
*********************************************************************/

#include <stdio.h>
#include "udebug.h"
#include "ritokdef.h"
#include "riddldef.h"
#include "reddlerr.h"
#include "rerrdef.h"
#include "xfsys0.h"

/*********************************************************************
**    I_FUNCTION     :  int ur_read1def(fd, attrbuf, buflimit, tblname, tbltyp)
**       read a single table def. from the data dictionary file.
**    PARAMETERS   
**       INPUT  : 
**          fd			int	file descriptor returned by ur_popen()
**          buflimit	int	max. number of fields (array size)
**       OUTPUT :  
**          attrbuf[] struct array containing the field definitions
**          tblname	char*	name of the table parsed.
**				tbltyp	int*	type of the table
**    RETURNS      : # of fields, 0 for EOF, negative for errors.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_read1def(fd, attrbuf, buflimit, tblname, tbltyp)
FILE					*fd;
struct attr_def	attrbuf[];
int					buflimit;
char					*tblname;
int					*tbltyp;
{
	int	ind;
	char	keywrd[20];				/* keyword read from dictionary file */
	int	rel_num, init_size, exp_size;	/* throw away for now */
	char	rel_typ;					/* character for relation type */
	int	status;

	uu_denter(UU_RITRC,(us,"ur_read1def(file%d, buf 0x%x)", fd, attrbuf));
	ind = 0;							/* starting index */
	UX_FSCANF0((fd, "%s", keywrd), status);
	if (status == EOF)	/* get a keyword from the file */
	{
		uu_dexit;
		return(0);					/* return end of file status */
	}
	uu_dprint(UU_RITRC,(us,"ur_read1def:keyword=%s", keywrd));
	if (strcmp(keywrd, "*relation") == 0)
	{
		/* get the rest of the relation line */
		UX_FSCANF0((fd, "%s%d %c%d%d", tblname, &rel_num, &rel_typ, &init_size,
						&exp_size), status);
		*tbltyp = (rel_typ == 'M') ? UR_MODAL : UR_TABLE;
		/* read field list lines until *end */
		do
		{
			if (ind >= buflimit) break;				/* exit loop if out of space */
			ur_read1field(fd, &attrbuf[ind]);
		} while (strcmp(attrbuf[ind++].attr_name, "*end") != 0);
		ind--;
		if (strcmp(attrbuf[ind].attr_name, "*end") != 0)
		{
			ind = -ind;
		}
	}
	else if (strcmp(keywrd, "*fldgrp") == 0)
	{
			uu_dprint(-1,(us,"Oops *fldgrp"));
	}
	else if (strcmp(keywrd, "*enum") == 0)
	{
			uu_dprint(-1,(us,"Oops *enum"));
	}
	else if (strcmp(keywrd, "*int_range") == 0)
	{
			uu_dprint(-1,(us,"Oops *int_range"));
	}
	else if (strcmp(keywrd, "*real_range") == 0)
	{
			uu_dprint(-1,(us,"Oops *real_range"));
	}
	else if (strcmp(keywrd, "*class") == 0)
	{
			uu_dprint(-1,(us,"Oops *class"));
	}
	else if (strcmp(keywrd, "*type") == 0)
	{
			uu_dprint(-1,(us,"Oops *type"));
	}
	else			/* error */
	{
			uu_dprint(-1,(us,"Data dictionary file is illegal or damaged."));
			ind = UR_BAD_DD;
	}
	uu_dexit;
	return(ind);			/* return # of fields */
} /* read 1 data dictionary entry */



/*********************************************************************
**    I_FUNCTION     :  int ur_read1field(fd, attrbuf)
**       read a single field def. from the data dictionary file.
**    PARAMETERS   
**       INPUT  : 
**          fd			int	file descriptor returned by ur_popen()
**       OUTPUT :  
**          attrbuf	struct containing the field definition
**    RETURNS      : 1 if OK, 0 for EOF, negative for errors.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_read1field(fd, attrbuf)
FILE					*fd;
struct attr_def	*attrbuf;
{
	char	fldtyp[20];										/* string for field type */
	int	status;

	uu_denter(UU_RITRC,(us,"ur_read1field(?, 0x%x)", attrbuf));
	UX_FSCANF0((fd, "%s", attrbuf->attr_name), status);	/* 1st get field name */
	uu_dprint(UU_RITRC,(us,"field '%s'", attrbuf->attr_name));
	if (strcmp(attrbuf->attr_name, "*end") == 0)
	{
		uu_dexit;
		return(1);											/* *end is valid field here   */
	}
	UX_FSCANF0((fd, "%s%d%d%d%d", fldtyp, & attrbuf->num_rows,
				&attrbuf->num_cols, &attrbuf->attr_size,
							&attrbuf->attr_off), status);
	uu_dprint(UU_RITRC,(us,"%s%d%d%d%d", fldtyp, attrbuf->num_rows,
			attrbuf->num_cols, attrbuf->attr_size,
			attrbuf->attr_off));
	attrbuf->attr_type = ur_totype(fldtyp);
	if (attrbuf->num_rows < 0)
	{
		attrbuf->num_rows = 1;
	}
	if (attrbuf->num_cols < 0)
	{
		attrbuf->num_cols = 1;
	}
	uu_dexit;
	return(0);
}
