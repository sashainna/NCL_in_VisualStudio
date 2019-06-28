/*********************************************************************
**    NAME: int bfrmcolor.c 
**       CONTAINS:
**				ubi_get_form_color
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bfrmcolr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:03
*********************************************************************/
#include "usysdef.h"
#include "uhep.h"     	/* for error system */
#include "udebug.h"
#include "mdattr.h"		/* for modelling colors */
#include "bsym.h"

#define TRACE UU_FALSE /* for debugging only */
/*********************************************************************
**    E_FUNCTION : int ubi_get_form_color(formcolornbr, outcolorptr)
**			This function determines the number of a color from the number
**			returned from a symbol form containing a color field where the
**			toggle fields for the colors are in the following sequence:
**				white, background, black, dark red, dark green, dark blue,
**				yellow, cyan, magenta, red, green, blue, orange, pink,
**				light green, light blue.
**    PARAMETERS   
**       INPUT  : 
**          formcolornbr		number of a color from a form.
**       OUTPUT :  
**				outcolorptr			pointer to the color number used internally.
**    RETURNS: UU_SUCCESS if no errors detected, otherwise UB_FAILURE 
**					is returned.
**    SIDE EFFECTS : none
**    WARNINGS: Assumes defaults are already specified.
*********************************************************************/
int ubi_get_form_color(formcolornbr, outcolorptr)
	int formcolornbr;
	int *outcolorptr;
{
	int status;

	uu_denter(UU_BTRC,(us,"ubi_get_form_color(in:%d,?)",formcolornbr));
	status = UU_SUCCESS; /* assume success */
	switch(formcolornbr)
	{
		case 0/* background */: 
			*outcolorptr = UM_BACKGROUND;
			break;
		case 1/* white */: 
			*outcolorptr = UM_WHITE;
			break;
		case 2/* black */: 
			*outcolorptr = UM_BLACK;
			break;
		case 3/* dark red */: 
			*outcolorptr = UM_DARKRED;
			break;
		case 4/* dark green */: 
			*outcolorptr = UM_DARKGREEN;
			break;
		case 5/* dark blue */: 
			*outcolorptr = UM_DARKBLUE;
			break;
		case 6/* yellow */: 
			*outcolorptr = UM_YELLOW;
			break;
		case 7/* cyan */: 
			*outcolorptr = UM_CYAN;
			break;
		case 8/* magenta */: 
			*outcolorptr = UM_MAGENTA;
			break;
		case 9/* red */: 
			*outcolorptr = UM_RED;
			break;
		case 10/* green */: 
			*outcolorptr = UM_GREEN;
			break;
		case 11/* blue */: 
			*outcolorptr = UM_BLUE;
			break;
		case 12/* orange */: 
			*outcolorptr = UM_ORANGE;
			break;
		case 13/* pink */: 
			*outcolorptr = UM_PINK;
			break;
		case 14/* light green */: 
			*outcolorptr = UM_LIGHTGREEN;
			break;
		case 15/* light blue */: 
			*outcolorptr = UM_LIGHTBLUE;
			break;
		default:
			uu_uerror2(UB_SYMBOL, 56, formcolornbr, "ubi_get_form_color");
			/* error is: Illegal color number of %d,  (%s). */
			goto failed;
	}
#if (TRACE)
	uu_dprint(UU_BTRC,(us,"color returned: %d", *outcolorptr));
#endif
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}
