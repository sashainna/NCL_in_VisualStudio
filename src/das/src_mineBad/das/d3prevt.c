/*********************************************************************
**
**    NAME         :  d3prevt.c
**
**       CONTAINS:
**  			  ud_prevent
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d3prevt.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:06
**
*********************************************************************/

#include "usysdef.h"
#include "gtbl.h"
#include "udebug.h"
#include "dinput.h"
#include "g.h"
#include "dasg.h"

/**************************************************************************
**
**  I_FUNCTION         :  ud_prevent(name, event)
**
**      print out a modelling event for debug purposes
**
**  PARAMETERS   
**
**      INPUT  : 
**
**          event = event buffer
**				name  = name of calling routine
**
**      OUTPUT :  
**
**          none
**
**  RETURNS      :  none
**
**  SIDE EFFECTS :  none
**
**  WARNINGS     :  none
**
**************************************************************************/

ud_prevent(name, event)
char *name;
UD_DEVENT *event ;
{

    /* local  parameter declarations */

	int i, j;
	char us[200];			/* for debug */

/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
**  block comment
*/

	switch ((*event).evclass)
	{
		case UD_NONE:

			uu_dprint(UU_DTRC,(us, " from %s event = NONE ", name));

			break;

		case UD_LOCATOR:
	
			uu_dprint(UU_DTRC,(us, " from %s event = UD_LOCATOR, device = %d",
					name, (*event).evdev));

			uu_dprint(UU_DTRC, (us, " trans = %d, loc  = (%g, %g),", 
				(*event).indata.locdata.transform, 
				(*event).indata.locdata.position[0],
				(*event).indata.locdata.position[1]));

			uu_dprint(UU_DTRC,(us, " choice = %d",
				(*event).indata.locdata.choice));

			break;
	
		case UD_STROKE:
	
	uu_dprint(UU_DTRC, (us, " from %s event = STROKE, device = %d, number = %d", 
				name, (*event).evdev, (*event).indata.strokedata.n_points));

			break;
	
		case UD_VALUATOR:
	
			uu_dprint(UU_DTRC,(us, " from %s event = VALUATOR, device = %d", 
				name, (*event).evdev));

			uu_dprint(UU_DTRC,(us, " value = %g", (*event).indata.valdata));

			break;
	
		case UD_CHOICE:
	
			uu_dprint(UU_DTRC,(us, " from %s event = CHOICE, device = %d", 
				name, (*event).evdev));

			uu_dprint(UU_DTRC,(us, " choice = %d", (*event).indata.choicedata));

			break;
	
		case UD_PICK:
				
			uu_dprint(UU_DTRC,(us, " from %s event = PICK, device = %d", 
				name, (*event).evdev));

			uu_dprint(UU_DTRC,(us, " status = %d", 
				(*event).indata.pickdata.status)); 

			j = (*event).indata.pickdata.depth;
			uu_dprint(UU_DTRC,(us, " , depth = %d, id = ( ", j));

			if(j > 2)
				j = 2;
			for(i=0; i<j; i++)
			{
				uu_dprint(UU_DTRC,(us,"%d ", 
					(*event).indata.pickdata.pickpath[i]));
			}

			uu_dprint(UU_DTRC,(us, "),"));

			uu_dprint(UU_DTRC,(us, " trans = %d, loc  = (%g, %g),", 
				(*event).indata.pickdata.transform, 
				(*event).indata.pickdata.position[0],
				(*event).indata.pickdata.position[1]));

			uu_dprint(UU_DTRC,(us, " choice = %d", 
				(*event).indata.pickdata.choice));

			break;
	
		case UD_STRING:

			uu_dprint(UU_DTRC,(us, " from %s event = STRING, device = %d", 
				name, (*event).evdev));

			uu_dprint(UU_DTRC,(us, " string = %s", (*event).indata.stringdata));

			break;

		case UD_VECTOR:

			uu_dprint(UU_DTRC,(us, " from %s event = VECTOR, device = %d",
											name, (*event).evdev));

			uu_dprint(UU_DTRC,(us, " vector  = (%g, %g, %g)", 
				(*event).indata.vecdata[0], 
				(*event).indata.vecdata[1],
				(*event).indata.vecdata[2]));

			break;

		default:
			uu_dprint(UU_DTRC,(us, " from %s event = UNKNOWN, device = %d", 
				name, (*event).evdev));
	}
}
