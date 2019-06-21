
/*********************************************************************
**    NAME         :  tlangtool.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tlangtool.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:55
*********************************************************************/

#ifndef	UT_LANGTOOL

#include "usysdef.h"

typedef  union
	{
	 int	lint;		/* integer and char element */
	 UU_REAL lreal;  /* real element */
	}	ITSTYPE;

#define	UT_LANGTOOL
#endif 

