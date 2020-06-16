/************************************************************************
c
c   FILE NAME: supportc.c
c
c	 CONTAINS:
c		iges_getabout_str
c
c     COPYRIGHT 2005 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        supportc.c , 26.3
c     DATE AND TIME OF LAST  MODIFICATION
c        09/25/18 , 10:29:42
c
c**********************************************************************
*/

#include "usysdef.h"
#include "ustdio.h"
#include "nclver.h"
#include "mfort.h"


#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D)
#ifndef UU_RS6000
#define getver getver_
#endif
#endif


/***********************************************************************
c
c   FUNCTION: iges_getabout_str(msg1, msg2)
c
c         Get about dialog text string
c   INPUT:  None
c
c   OUTPUT :   msg1: first line of about text
c              msg2: second line of about text
c   RETURN:    None
c
**********************************************************************/
void iges_getabout_str(msg1,msg2)
char *msg1,*msg2;
{	
    double ver;
	getver(&ver);
	sprintf(msg1, " NCL/IGES Version %7.2f", ver);
	strcpy(msg2, "Copyright (C) Numerical Control Computer Sciences 1991-2018");
}
/***********************************************************************
c
c   FUNCTION: stricmp(str1, str2)
c			compare string case no-sensitive
c			active seems as stricmp from WinNT
c
c   INPUT:  str1, str2: string to be compared
c
c   OUTPUT :
c			0:  the same.
c   RETURN: None.
c
**********************************************************************/
#if UU_COMP!=UU_WIN2K
int stricmp(str1, str2)
char *str1, *str2;
{
	char *temp1, *temp2;
	int ret, len1, len2;
	len1 = strlen(str1);
	len2 = strlen(str2);
	if ((len1==0) || (len2==0))
		return -1;
	temp1 = (char *) uu_malloc((len1+1)*sizeof(char));
	temp2 = (char *) uu_malloc((len2+1)*sizeof(char));
	strcpy(temp1, str1);
	strcpy(temp2, str2);
	ul_to_lower(temp1);
	ul_to_lower(temp2);
	ret =  strcmp(temp1, temp2);
	uu_free (temp1);
	uu_free (temp2);
	return ret;
}
#endif

