/*********************************************************************
**
**    NAME         :  dunits.c
**
**    CONTAINS:
**
**            units()
**
** This routine is here only because of a bug in Sun OS 4.0 which
** leaves the routine units() as undefined.  This module will (hopefully)
** become obsolete.
**
**    COPYRIGHT 1987 (c) MILLS DATA SYSTEMS CORP. All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       dunits.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:20
**
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#if UU_COMP == UU_SUN

int
units()
	{
	return;
	}

#endif UU_SUN
