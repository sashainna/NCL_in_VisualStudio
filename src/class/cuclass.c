/*********************************************************************
**    NAME         :  cuclass.c
**       CONTAINS: class user interface routines
** 			void ucu_dump_class_table()
** 			void ucu_set_class_trace()
** 			void ucu_reset_class_trace()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     
**    MODULE NAME AND RELEASE LEVEL 
**       cuclass.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:00
**
*********************************************************************/

#include "usysdef.h"
#include "class.h"
#include "canbe.h"
#include "udebug.h"

	extern UC_TABLE_DESCRIPTOR UC_root_descriptor, UC_cot_descriptor;
	extern UC_TABLE_DESCRIPTOR *UC_3d_descriptor[];

	int UC_object_trace = UU_FALSE;		/* object level trace flag */

/*********************************************************************
**
**    E_FUNCTION:  void ucu_dump_class_table()
**			This function dumps out the class tables
**
**    PARAMETERS   
**       INPUT: 
**				none
**       OUTPUT:  
**				none
**
**    RETURNS: none
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

void ucu_dump_class_table()
{
	int save_mask;
	int i;

	save_mask = UU_debmask;
	UU_debmask = UU_MTRC;

/*	-- for each class descriptor table in the 3d model -- */

	for(i=0; i<UC_NUM_DOMAINS; i++)
	{
		if(UC_3d_descriptor[i] != UU_NULL)
			uc_print_class_table(UC_3d_descriptor[i]);
	}

	UU_debmask = save_mask;
}

/*********************************************************************
**
**    E_FUNCTION:  void ucu_set_class_trace()
**			This function turns on object level trace
**
**    PARAMETERS   
**       INPUT: 
**				none
**       OUTPUT:  
**				none
**
**    RETURNS: none
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

void ucu_set_class_trace()
{
	UC_object_trace = UU_TRUE;
}

/*********************************************************************
**
**    E_FUNCTION:  void ucu_reset_class_trace()
**			turns off object level trace
**
**    PARAMETERS   
**       INPUT: 
**				none
**       OUTPUT:  
**				none
**
**    RETURNS: none
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

void ucu_reset_class_trace()
{
	UC_object_trace = UU_FALSE;
}
