/*********************************************************************
**    NAME         :  redmpevt.c
**       CONTAINS:
**       ur_dump_environ_table()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       redmpevt.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:28
*********************************************************************/

#include "udebug.h"
#include "rienvtab.h"
#include "ribase.h"

/*********************************************************************
**    E_FUNCTION     :  ur_dump_environ_table()
**       dump the environment table(see rienvtab.[ch] to trace file
**			(debuggery procedure)
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : print the contents of UR_environ_table into trc file
**    WARNINGS     : none
*********************************************************************/

ur_dump_environ_table()
{
	extern struct UR_env_table_rec UR_environ_table[];
	int	i;

	uu_denter(UU_RTRC,(us,"ur_dump_environ_table():"));
	uu_dprint(UU_RTRC,(us,"  name       adrs      new adrs length"));
	for(i = 0; UR_environ_table[i].name[0] != '\0'; i++)
	{
		uu_dprint(UU_RTRC,(us,"%.12s 0x%x 0x%x %d",UR_environ_table[i].name,
				UR_environ_table[i].adrs,UR_environ_table[i].new_adrs,
				UR_environ_table[i].length));
	}
	uu_dexit;
}
