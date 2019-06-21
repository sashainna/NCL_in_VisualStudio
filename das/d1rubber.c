/*********************************************************************
**
**    NAME         :  d1gkssup.c
**
**       CONTAINS:
**				ud_strub
**				ud_onrub
**				ud_endrub
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d1rubber.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:03
**
*********************************************************************/

#include "usysg.h"
#include "udebug.h"
#include "dinput.h"
#include "dasnog.h"
#include "mattr.h"
#include "goatt.h"
#include "view.h"

/*********************************************************************
**    E_FUNCTION     : ud_strub(rubinfo, pet)
**       start rubber band sequence
**    PARAMETERS   
**       INPUT  : 
**          rubinfo = rubber band control block
**          pet = type of ubber band (4 = r.b. line, 5 r.b. box)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_strub(rubinfo, pet)
UD_RUBBER *rubinfo;
int pet;
{
	UU_LOGICAL ud_qlocpet();
	 
	uu_denter(UU_DTRC,(us,"ud_strub()"));

/*	-- see if rubber banding is active -- */

/*	-- enable for all cases except rubber line with multiple views active -- */

	if(pet==4 && UV_act_screen[0].nvports>1)
		(*rubinfo).rubber = UU_FALSE;
	else
		(*rubinfo).rubber = ud_qlocpet(UD_locdev, pet);

	(*rubinfo).rubberon = UU_FALSE;
	(*rubinfo).rubberpet = pet;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : ud_onrub(rubinfo)
**       activate rubber band sequence
**    PARAMETERS   
**       INPUT  : 
**          rubinfo = rubber band control block
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_onrub(rubinfo)
UD_RUBBER *rubinfo;
{
	 
	Glntype linestyle;			/* line style structure */

	uu_denter(UU_DTRC,(us,"ud_onrub()"));

/*		-- see if time to turn on rubber. if input type is not locator,
			then don't. -- */

	if((*rubinfo).rubber == UU_TRUE)
	{
		uu_denter2(UU_DTRC,(us,"in ud_onrub, type = %d",(*rubinfo).type));
		uu_dexit;
		ud_qcord(&((*rubinfo).type), &((*rubinfo).dev), &((*rubinfo).pet));
		if((*rubinfo).type == UD_LOCATOR)
		{
/*			-- set the active locater PET to rubber band -- */

			ud_dtcord(UD_LOCATOR, (*rubinfo).dev, (*rubinfo).rubberpet);

/*			-- set the current line color style and  width active -- */

			linestyle.typeno = ur_get_attrmdl_line_style();
			linestyle.npatn = 0;
			gslinecolor(ur_get_attrmdl_color());
			gslinetype(&linestyle);
			gslinewidth(ur_get_attrmdl_line_width());

			(*rubinfo).rubberon = UU_TRUE;
		}
		else
			(*rubinfo).rubberon = UU_FALSE;
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : ud_endrub(rubinfo)
**       end rubber band sequence
**    PARAMETERS   
**       INPUT  : 
**          rubinfo = rubber band control block
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_endrub(rubinfo)
UD_RUBBER *rubinfo;
{
	int type;			/* interaction type */
	int dev;				/* device number */
	int pet;				/* prompt and echo type */
	 
	uu_denter(UU_DTRC,(us,"ud_endrub()"));

/*	-- restore locator state if r.b. on and current state is locator -- */

	if((*rubinfo).rubberon == UU_TRUE)
	{
		ud_qcord(&type, &dev, &pet);
		uu_denter2(UU_DTRC,(us,"in ud_endrub, type=%d", type));
		uu_dexit;
		if(type == UD_LOCATOR)
			ud_dtcord((*rubinfo).type, (*rubinfo).dev, (*rubinfo).pet);
	}
	uu_dexit;
}
