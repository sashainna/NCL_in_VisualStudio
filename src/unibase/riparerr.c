#define MODULEDBG 0
/*********************************************************************
**    NAME         :  riparerr.c
**       CONTAINS:
**       pars_err()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       riparerr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:46
*********************************************************************/

#if MODULEDBG != 0
#include "udebug.h"
#endif
extern	int	UR_tline_num;			/* current line number */

/*********************************************************************
**    I_FUNCTION     :  int ur_pars_err(err_msg)
**       prints parser error mesages.
**    PARAMETERS   
**       INPUT  : 
**          err_msg	int	which message
**       OUTPUT :  
**          none
**    RETURNS      : doesn't return - performs error exit
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_pars_err(err_msg)
int	err_msg;
{
	char msg[256];

	switch(err_msg)
	{
	case -1:
		sprintf(msg, "Parse error: Line %d - definition must start with 'create'.\n",UR_tline_num);
		break;
	case -2:
		sprintf(msg, "Parse error: Line %d - unexpected end of input.\n",UR_tline_num);
		break;
	case -3:
		sprintf(msg, "Parse error: Line %d - expecting 'table'.\n",UR_tline_num);
		break;
	case -4:
		sprintf(msg, "Parse error: Line %d - expecting a name.\n",UR_tline_num);
		break;
	case -5:
		sprintf(msg, "Parse error: Line %d - expecting '('.\n",UR_tline_num);
		break;
	case -6:
		sprintf(msg, "Parse error: Line %d - expecting type.\n",UR_tline_num);
		break;
	case -7:
		sprintf(msg, "Parse error: Line %d - too many dimensions!\n",UR_tline_num);
		break;
	case -8:
		sprintf(msg, "Parse error: Line %d - expecting ']'.\n",UR_tline_num);
		break;
	case -9:
		sprintf(msg, "Parse error: Line %d - semicolon expected.\n",UR_tline_num);
		break;
	case -10:
		sprintf(msg, "Parse error: Line %d - file not open for parse.\n",UR_tline_num);
		break;
	case -11:
		sprintf(msg, "Parse error: Line %d - pushed back too many chars.\n",UR_tline_num);
		break;
	case -12:
		sprintf(msg, "Parse error: Line %d - too many parse files open.\n",UR_tline_num);
		break;
	case -13:
		sprintf(msg, "Parse error: Line %d - number expected (for array size?).\n",UR_tline_num);
		break;
	case -14:
		sprintf(msg, "Parse error: Line %d - comma or close parenthesis expected.\n",UR_tline_num);
		break;
	case -15:
		sprintf(msg, "Parse error: Line %d - ddltool's buffer overflow\n",UR_tline_num);
		break;
	case -16:
		sprintf(msg, "Parse error: Line %d - 'join' declarations should be at the end of table\n",UR_tline_num);
		break;
	case -17:
		sprintf(msg, "Parse error: Line %d - Unknown data type\n", UR_tline_num);
		break;
	default:
		sprintf(msg, "Parse error: Line %d - %d \n", err_msg,UR_tline_num);
		break;
	}
	ud_printmsg(msg);
	exit(1);
}
