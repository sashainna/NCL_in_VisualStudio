/*********************************************************************
**    NAME         :  uversion.c
**       CONTAINS:
**            prints version, date and time
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uversion.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:56
*********************************************************************/

/*
.....DECUNIX
*/
#define UU_VMAIN 1
#include "usysdef.h"
#include "uhep.h"
#include "dmenucom.h"
#include "uversion.h"
#undef UU_VMAIN

uversion()
{
uu_uerror1(MENUERROR,3,UU_vdatime);
}
