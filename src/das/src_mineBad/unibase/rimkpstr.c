/*********************************************************************
**    NAME         :  rimkpstr.c
**       CONTAINS:
**       ur_mk_print_str()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rimkpstr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:45
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "riddldef.h"

/*********************************************************************
** E_FUNCTION : ur_mk_print_str(attr_def, targstr)
**       create control string for printing relations
**    PARAMETERS   
**       INPUT  : 
**				attr_def	struct attr_def*	attribute definitions from
**														the data dictionary
**       OUTPUT :  
**          targstr		char*		the append target string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_mk_print_str(attr_def, targstr)
struct attr_def	*attr_def;	/* data dictionary definition */
char					targstr[];	/* target string buffer */
{
	static int	tndx;		/* target string index */
	int	andx;		/* attribute index */
	int	nndx;		/* name index */
	char	tmpstr[64];
	int	size	;	/* size returned */
	int	i		;	/* index				*/

	uu_denter(UU_RITRC,(us,"ur_mk_print_str()"));
	tndx = 0;
	size = 0;

	/* build the "control string" describing this field */
	switch (attr_def->attr_type)
	{
	case FLOAT:
		targstr[tndx++] = '%';
		targstr[tndx++] = 'e';
		size = 4;
		break;
	case DOUBLE:
		targstr[tndx++] = '%';
		targstr[tndx++] = 'l';
		targstr[tndx++] = 'e';
		size = 8;
		break;
	case REAL:
		targstr[tndx++] = '%';
#if UU_COMP != UU_CIM
#ifndef UU_SINGLE 
		targstr[tndx++] = 'l';
#endif
#endif
		targstr[tndx++] = 'e';
		size = 4;
		break;
	case JOIN:
	case STRING:
	case INT:
		targstr[tndx++] = '%';
		targstr[tndx++] = 'd';
		size = 3;
		break;
	case KEY_ID:
	case REL_ID:
	case KEY_REF:
		targstr[tndx++] = '%';
		targstr[tndx++] = 'l';
		targstr[tndx++] = 'u';
		size = 2;
		break;
	case LOGICAL:
		targstr[tndx++] = '%';
		targstr[tndx++] = 'd';
		size = 3;
		break;
	case CHARACTER:
		targstr[tndx++] = '%';
		targstr[tndx++] = 'c';	/* no string -- characters */
		size = 1;
		break;
	default:
		size = 0 ;
		break;
	}
	targstr[tndx] = '\0';   /* end the string */
	uu_dexit;
	return(size);
}

/*********************************************************************
** E_FUNCTION : ur_mk_scan_str(attr_def, targstr)
**       create control string for scaning relations
**    PARAMETERS   
**       INPUT  : 
**				attr_def	struct attr_def*	attribute definitions from
**														the data dictionary
**       OUTPUT :  
**          targstr		char*		the append target string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_mk_scan_str(attr_def, targstr)
struct attr_def	*attr_def;	/* data dictionary definition */
char					targstr[];	/* target string buffer */
{
int status;
	status = ur_mk_print_str(attr_def, targstr);
/*MILLS: placed change here so that it affects reads and not writes - rah */
#if UU_COMP == UU_VAXVMS
	if (!strcmp("%lu", targstr))
		{
		targstr[1] = 'd';
		targstr[2] = '\0';
		}
#endif
#if UU_COMP == UU_CIM
	if ((!strcmp("%e", targstr)) && (attr_def->attr_type == REAL))
		{
		targstr[1] = 'l';
		targstr[2] = 'e';
		targstr[3] = '\0';
		status = 4;
		}
#endif
	return(status);
}
