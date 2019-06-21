/*********************************************************************
**    NAME         :  asgetenv.c
**       CONTAINS:
**       uu_sgetenv
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       asgetenv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:39
*********************************************************************/

/*********************************************************************
**    E_FUNCTION :  uu_sgetenv(name,value)
**       SAL front end to call ux_getenv.
**    PARAMETERS   
**       INPUT  : 
**          char *name      -  Pointer to symbolic name
**       OUTPUT :  
**          char *value     -  Pointer to value of symbolic name
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#include "usysdef.h"
#include "ustrings.h"
#include "xenv1.h"
uu_sgetenv(name,value)
		char *name, *value;
{
 char    *ux_getenv();
 char    *cptr;
/*--------- begin function code ----------------------------------*/
	cptr = ux_getenv(name, UX_PRTERRS);
	if (cptr!=0)							/* found the env name */
		strcpy(value,cptr);				/* move it to user var */
	else
		strcpy(value,"");					/* set value to null string*/
}
