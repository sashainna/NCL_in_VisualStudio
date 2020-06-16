/************************************************************************
*
*   FILE NAME: supportc.c
*
*	 CONTAINS:
*		step_getabout_str
*
*     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
*           All Rights Reserved
*      MODULE NAME AND RELEASE LEVEL
*        supportc.c , 26.3
*     DATE AND TIME OF LAST  MODIFICATION
*        09/25/18 , 10:37:46
*
***********************************************************************/

#include "usysdef.h"
#include "ustdio.h"
#include "nclver.h"


/***********************************************************************
c
c   FUNCTION: utp_getabout_str(msg1, msg2)
c
c         Get about dialog text string
c   INPUT:  None
c
c   OUTPUT :   msg1: first line of about text
c              msg2: second line of about text
c   RETURN:    None
c
**********************************************************************/
void utp_getabout_str(msg1,msg2)
char *msg1,*msg2;
{	
    double ver;
	getver(&ver);
	sprintf(msg1, " NCL/STEP Version %7.2f", ver);
	strcpy(msg2, "Copyright (C) Numerical Control Computer Sciences 2013-2018");
}
