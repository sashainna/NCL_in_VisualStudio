
/*********************************************************************
**    NAME         :  acconvt.c
**       CONTAINS:
**       C character conversion functions for sal.
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       acconvt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:31
*********************************************************************/

#include "usysdef.h"
#include <stdio.h>

/*********************************************************************
**    E_FUNCTION :  void ua_rtos (value, dplaces, c_value)
**       Convert a floating point number to a char string.
**    PARAMETERS   
**       INPUT  : 
**          value - value to convert.
**				dplaces - number of decimal places
**
**       OUTPUT :  
**          c_value - character version of value
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ua_rtos(value, dplaces, c_value)
UU_REAL value;
int	  dplaces;
char	  *c_value;

{
	char	*cptr;

	sprintf(c_value, "%-20.*f", dplaces, value);
	cptr = c_value + 19;
	while (*cptr == ' ') cptr--;
	*(cptr+1) = '\0';
}
