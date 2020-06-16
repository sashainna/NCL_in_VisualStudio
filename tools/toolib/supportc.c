 /************************************************************************
c
c   FILE NAME: supportc.c
c
c	 CONTAINS:
c		getenvc
c		getver
c		toolib_getabout_str
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        supportc.c , 26.3
c     DATE AND TIME OF LAST  MODIFICATION
c        09/25/18 , 10:39:20
c
c**********************************************************************
*/

#include "usysdef.h"
#include <stdlib.h>
#include <string.h>
#define NCLVERSION
#include "nclver.h"
#include "mfort.h"

#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D)
#ifndef UU_RS6000
#define getver getver_
#endif
#endif

/***********************************************************************
c
c   SUBROUTINE:  getenvc(char* key, char *value, int len)
c
c   FUNCTION:  This function get the enviornment value
c
c   INPUT:  key    environment key
c
c   OUTPUT: value: environment value
c			len  : environment value's length
c
c***********************************************************************
*/
void getenvc(key, value, len)
char *key,*value;
int *len;
{
	char key1[20], *value1, *tool_getenv();
	strncpy(key1, key, *len);
	key1[*len] = '\0'; 
	value1 = tool_getenv (key1);
	if (value1!=NULL)
	{
		strcpy(value, value1);
		*len = strlen(value1);
	}
	else
	{
		value[0]='\0';
		*len = 0;
	}
}

/***********************************************************************
c
c   SUBROUTINE:  getver(vers)
c
c   FUNCTION:  This function returns the NCL version number.
c
c   INPUT:  none
c
c   OUTPUT: vers   = NCL Version number.
c
c***********************************************************************
*/
void getver(vers)
UM_real8 *vers;
{
	*vers = NCL_version;
	return;
}
/***********************************************************************
c
c   FUNCTION: toolib_getabout_str(msg1, msg2)
c
c         Get about dialog text string
c   INPUT:  None
c
c   OUTPUT :   msg1: first line of about text
c              msg2: second line of about text
c   RETURN:    None
c
**********************************************************************/
void toolib_getabout_str(msg1, msg2)
char *msg1, *msg2;
{
	sprintf(msg1, "Toolib Version %7.2f", NCL_version);
	strcpy(msg2, "Copyright (C) Numerical Control Computer Sciences 1991-2018")
;
}
