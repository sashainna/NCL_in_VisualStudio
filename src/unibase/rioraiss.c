/*********************************************************************
**    NAME         :  rioraiss.c
**       CONTAINS:
**       uri_nsrt_sql_str()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rioraiss.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:45
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "riddldef.h"

/*********************************************************************
**    E_FUNCTION : uri_nsrt_sql_str(relnm, attr_defs, num_attr, targstr)
**       create sql cmd string for insert into oracle tables
**    PARAMETERS   
**       INPUT  : 
**				relnm			char*					the relation to insert into
**				attr_defs	struct attr_def*	attribute definitions from
**														the data dictionary
**				num_attr		int					number of defs in attr_defs
**       OUTPUT :  
**          targstr		char*		the insert string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uri_nsrt_sql_str(relnm, attr_defs, num_attr, targstr)
char					relnm[];			/* relation name */
struct attr_def	attr_defs[];	/* data dictionary definition */
int					num_attr;		/* definition length */
char					targstr[];		/* target string buffer */
{
#define MAX_CHAR_COL_WIDTH 240
	int	tndx;							/* target string index */
	int	andx;							/* attribute index */
	int	nndx;							/* name index */
	int	i, j;							/* row and column indices */
	char	tmpstr[10];
	int         col_width;		/* width of character columns */
	int         col_num;       /* column counter */
	UU_LOGICAL  number_cols;   /* should char columns be numbered? */

	uu_denter(UU_RTRC,(us,"uri_nsrt_sql_str"));

	/* build the SQL command for the relation given. */
	strcpy(targstr, "INSERT INTO ");
	strcat(targstr, relnm);
	strcat(targstr, " VALUES(:PART_ID");
	uu_dprint(UU_RITRC,(us,"%s",targstr));
	tndx = strlen(targstr);			/* set up target index */
	for(andx = 0; andx < num_attr; andx++)	/* each attribute */
	{
		if (attr_defs[andx].attr_type == JOIN)
			continue;				/* join fields don't go out that easy! */
		if (attr_defs[andx].attr_type != CHARACTER)
		{
			for(j = 1; j <= attr_defs[andx].num_rows; j++)
				for(i = 1; i <= attr_defs[andx].num_cols; i++)
				{
					targstr[tndx++] = ',';
					targstr[tndx++] = ':';
					/* move the attribute name into the string */
					targstr[tndx]='\0';
					strcat(targstr, attr_defs[andx].attr_name);
					tndx=strlen(targstr);
					uu_dprint(UU_RITRC,(us,",:%s",attr_defs[andx].attr_name));

					/* fix the name for arrays: */
					/* add the digits */
					if (attr_defs[andx].num_rows > 1)
					{
						sprintf(tmpstr, "_%d", j-1);
						for (nndx = 0; tmpstr[nndx] != '\0'; nndx++, tndx++)
						{
							targstr[tndx] = tmpstr[nndx];
						}
						uu_dprint(UU_RITRC,(us,"%s",tmpstr));
					}
					if (attr_defs[andx].num_cols > 1)
					{
						sprintf(tmpstr, "_%d", i-1);
						for (nndx = 0; tmpstr[nndx] != '\0'; nndx++, tndx++)
						{
							targstr[tndx] = tmpstr[nndx];
						}
						uu_dprint(UU_RITRC,(us,"%s",tmpstr));
					}
					if((attr_defs[andx].attr_type == KEY_ID) ||
						(attr_defs[andx].attr_type == REL_ID))
					{
						/* split keys into two fields */
						targstr[tndx++] = '_';
						targstr[tndx++] = 'r';
						targstr[tndx++] = 'e';
						targstr[tndx++] = 'l';
						targstr[tndx++] = ',';
						targstr[tndx++] = ':';
						uu_dprint(UU_RITRC,(us,"_rel,:"));
						/* move the attribute name into the string */
						targstr[tndx]='\0';
						strcat(targstr, attr_defs[andx].attr_name);
						tndx=strlen(targstr);
						uu_dprint(UU_RITRC,(us,"%s",attr_defs[andx].attr_name));

						/* fix the name for arrays: */
						/* add the digits */
						if (attr_defs[andx].num_rows > 1)
						{
							sprintf(tmpstr, "_%d", j-1);
							for (nndx = 0; tmpstr[nndx] != '\0'; nndx++, tndx++)
							{
								targstr[tndx] = tmpstr[nndx];
							}
							uu_dprint(UU_RITRC,(us,"%s",tmpstr));
						}
						if (attr_defs[andx].num_cols > 1)
						{
							sprintf(tmpstr, "_%d", i-1);
							for (nndx = 0; tmpstr[nndx] != '\0'; nndx++, tndx++)
							{
								targstr[tndx] = tmpstr[nndx];
							}
							uu_dprint(UU_RITRC,(us,"%s",tmpstr));
						}
						targstr[tndx++] = '_';
						targstr[tndx++] = 'k';
						targstr[tndx++] = 'e';
						targstr[tndx++] = 'y';
						uu_dprint(UU_RITRC,(us,"_key"));
					}
				}
		}
		else				/* type is character */
		{
			for (j = 1; j <= attr_defs[andx].num_rows; j++)
			{
			 	col_width = attr_defs[andx].num_cols;  /* array of char is 1 col */
				number_cols = (col_width > MAX_CHAR_COL_WIDTH);
				col_num = 0;
				while(col_width > 0)
				{
					targstr[tndx++] = ',';
					targstr[tndx++] = ':';
					/* move the attribute name into the string */
					targstr[tndx]='\0';
					strcat(targstr, attr_defs[andx].attr_name);
					tndx=strlen(targstr);
					uu_dprint(UU_RITRC,(us,",:%s",attr_defs[andx].attr_name));
					/* fix the name for arrays: */
					/* add the digits */
					if (attr_defs[andx].num_rows > 1)
					{
						sprintf(tmpstr, "_%d", j-1);
						for (nndx = 0; tmpstr[nndx] != '\0'; nndx++, tndx++)
						{
							targstr[tndx] = tmpstr[nndx];
						}
						uu_dprint(UU_RITRC,(us,"%s",tmpstr));
					}
					if(number_cols)
					{
						sprintf(tmpstr, "_%d", col_num);
						col_num++;
						for (nndx = 0; tmpstr[nndx] != '\0'; nndx++, tndx++)
						{
							targstr[tndx] = tmpstr[nndx];
						}
						uu_dprint(UU_RITRC,(us,"%s",tmpstr));
					}
					col_width -= MAX_CHAR_COL_WIDTH;
				}
			}
		}
	}
	targstr[tndx++] = ')';	/* end the value list */
	uu_dprint(UU_RITRC,(us,")"));
	targstr[tndx] = '\0';	/* end the string */
	uu_dprint(UU_RITRC,(us,"command string length = %d",tndx));
	uu_dexit;
}
