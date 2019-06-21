/*********************************************************************
**    NAME         :  uindex.c -- string index for VAX
**       CONTAINS:
**       char *index(str,ch)
**       char *rindex(str,ch)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sindex.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:08
*********************************************************************/
#include "usysdef.h"
#if UU_COMP==UU_VAXVMS
char *strchr();
char *strrchr();

char *index(str,ch)
char *str;
char *ch;
{
	char *p;
	p=strchr(str,ch);
	return(p);
}


char *rindex(str,ch)
char *str;
char *ch;
{
	char *p;
	p=strrchr(str,ch);
	return(p);
}
#endif
