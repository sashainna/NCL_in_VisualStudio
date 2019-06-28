#define MODULEDBG 0
/*********************************************************************
**    NAME         :  ridmppcs.c
**       CONTAINS:
**       uri_prnt_ctrl_str()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ridmppcs.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:42
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "riddldef.h"

/*********************************************************************
** E_FUNCTION : uri_prnt_ctrl_str(row,col,attr_def,targstr,terminate)
**       create control string for printing relations
**    PARAMETERS   
**       INPUT  : 
**				attr_def	struct attr_def*	attribute definitions from
**														the data dictionary
**				terminate, true if line terminator to be added
**       OUTPUT :  
**          targstr		char*		the append target string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uri_prnt_ctrl_str(row,col,attr_def, targstr,cntrl_flg)
int			row,col;				/* row, column position */
struct attr_def	*attr_def;	/* data dictionary definition */
char			targstr[];			/* target string buffer */
UU_LOGICAL	cntrl_flg;			/* <0 if start, 0 normal, >0 end	*/
{
	static int	tndx;		/* target string index */
	int	andx;		/* attribute index */
	int	nndx;		/* name index */
	char	tmpstr[64];
	int	size	;	/* size returned */
	int	i		;	/* index				*/

#if MODULEDBG != 0
	uu_denter(UU_RITRC,(us,"uri_prnt_ctrl_str(row=%d, col=%d, flag=%d)", row,
						col, cntrl_flg));
#endif

	/* if initializing the string, reset index and clear part of the string, */
	/* otherwise find the end of the string and start from there */
	if(cntrl_flg < 0)
	{
		tndx = 0;
		for(i = 0; i < sizeof(tmpstr); i++)
			targstr[i] = 0 ;	/* flush the build string */
	}
	else
	{
		tndx = strlen(targstr) ;
	}
	size = 0 ;

	/* build the "control string" describing this field */
	if (attr_def->attr_type != CHARACTER)
	{
		/* move the attribute name into the string */
		for(nndx=0;attr_def->attr_name[nndx]!='\0'; nndx++, tndx++)
		{
			targstr[tndx] = attr_def->attr_name[nndx];
		}
		/* fix the name for arrays: */
		/* add the digits */
		if (attr_def->num_rows > 1)
		{
			sprintf(tmpstr, "_%d", row-1);
			for (nndx = 0; tmpstr[nndx] != '\0'; nndx++, tndx++)
			{
				targstr[tndx] = tmpstr[nndx];
			}
		}
		if (attr_def->num_cols > 1)
		{
			sprintf(tmpstr, "_%d", col-1);
			for (nndx = 0; tmpstr[nndx] != '\0'; nndx++, tndx++)
			{
				targstr[tndx] = tmpstr[nndx];
			}
		}
		targstr[tndx++] = '=';	/* supply '=' */

		/* add format string */
		switch (attr_def->attr_type)
		{
		case FLOAT:
			targstr[tndx++] = '%';
			targstr[tndx++] = 'g';
			size = 4;
			break;
		case DOUBLE:
			targstr[tndx++] = '%';
			targstr[tndx++] = 'g';
			size = 8;
			break;
		case REAL:
			targstr[tndx++] = '%';
			targstr[tndx++] = 'g';
			size = 4;
			break;
		case JOIN:
		case INT:
			targstr[tndx++] = '%';
			targstr[tndx++] = 'd';
			size = 3;
			break;
		case KEY_ID:
			targstr[tndx++] = '0';
			targstr[tndx++] = 'x';
			targstr[tndx++] = '%';
			targstr[tndx++] = 'x';
			size = 2;
			break;
		case LOGICAL:
			targstr[tndx++] = '%';
			targstr[tndx++] = 'd';
			size = 3;
			break;
		default:
			size = 0 ;
			break;
		}
	}  /* then */
	else				/* type is character */
	{
		/* move the attribute name into the string */
		for(nndx=0;attr_def->attr_name[nndx]!='\0'; nndx++, tndx++)
		{
			targstr[tndx] = attr_def->attr_name[nndx];
		}
		targstr[tndx++] = '=';	/* supply '=' */

		/* add format string */
		targstr[tndx++] = '%';
		targstr[tndx++] = 's';
		size = 1;
	} /* else */
	if(cntrl_flg > 0)
	{
		targstr[tndx++] = '\n';
		targstr[tndx] = '\0';   /* end the string */
		tndx = 0 ;
	}
	else
	{
		targstr[tndx] = '\0';   /* end the string */
	}
quit:
#if MODULEDBG != 0
	uu_dexit;
#endif
	return(size);
}
