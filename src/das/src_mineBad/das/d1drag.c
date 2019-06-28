/*********************************************************************
**
**    NAME         :  d1gkssup.c
**
**       CONTAINS:
**				ud_stdrag
**				ud_ondrag
**				ud_enddrag
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d1drag.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:02
**
*********************************************************************/

#include "usysg.h"
#include "usysdef.h"
#include "udebug.h"
#include "dasg.h"
#include "dasnog.h"
#include "dinput.h"
#include "uims.h"
#include "view.h"

/*********************************************************************
**    E_FUNCTION     : ud_stdrag(rubinfo, pet)
**       start rubber band sequence
**    PARAMETERS   
**       INPUT  : 
**          draginfo = rubber band control block
**          pet = type of drag
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_stdrag(draginfo, segno, center, normtran)
UD_RUBBER *draginfo;
int segno;
Gwpoint3 *center;
int normtran;
{
	Glocrec locrec;
	Gdrect halfrect;
	Gdrect pos;
	UD_AREA *areapt;

	UU_LOGICAL ud_qlocpet();
	 
	uu_denter(UU_DTRC,(us,"ud_stdrag(), seg=%d, pnt=%g %g %g, tran=%d",
			segno, (*center).x ,(*center).y, (*center).z, normtran));

/*	-- disable with multiple views active -- */

	if(UV_act_screen[0].nvports>1)
		(*draginfo).rubber = UU_FALSE;
	else
	{

/*		-- see if drag is supported by this device -- */

		(*draginfo).rubber = ud_qlocpet(2, 21);
	}

	if((*draginfo).rubber == UU_TRUE)
	{

/*	-- enable for all cases. when multiple views active enable only
		if contruction plane is parallel to view plane -- */

		if(uv_cpln_parallel_vpln(normtran) == UU_SUCCESS)
		{

/*			-- set up the init structure for locator -- */

			locrec.prompt = UD_locpmt;
			locrec.seg = segno;

			gsnormtran(normtran);
			gwndc3(&locrec.attach.x, &locrec.attach.y, &locrec.attach.z,
					(*center).x, (*center).y, (*center).z);

			areapt = &(UD_duimsdeflt.screen[UD_curlayout.curr_screen].
					areas[UD_LPRMT])[UD_curlayout.curr_lex_prompt_area];
			ud_halfrect(&pos,&(*areapt).posn);
			ud_devrect(&pos,&halfrect);
			gslocmode(UD_ksws, 2, UG_REQUEST, UG_ECHO) ;
			ginitloc(UD_ksws, 2, &UD_nxhair, 21, &halfrect, &locrec);
			gslocmode(UD_ksws, 2, UG_EVENT, UG_ECHO) ;
		}
		else
			(*draginfo).rubber = UU_FALSE;
	}

/*	-- init the rubber control block, set the device number to 2 -- */

	(*draginfo).rubberon = UU_FALSE;
	(*draginfo).rubberpet = 2;

/*	-- save current locator info -- */

	ud_qcord(&((*draginfo).type), &((*draginfo).dev), &((*draginfo).pet));

	uu_dprint(UU_DTRC,(us,"leave ud_stdrag, drag=%d", (*draginfo).rubber));
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : ud_ondrag(draginfo)
**       activate rubber band sequence
**    PARAMETERS   
**       INPUT  : 
**          draginfo = rubber band control block
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_ondrag(draginfo)
UD_RUBBER *draginfo;
{
	 
	Glntype linestyle;			/* line style structure */
	int type;			/* interaction type */
	int dev;				/* device number */
	int pet;				/* prompt and echo type */

	uu_denter(UU_DTRC,(us,"ud_ondrag()"));

/*		-- see if time to turn on drag. if input type is not locator,
			then don't. -- */

	if((*draginfo).rubber == UU_TRUE)
	{
		ud_qcord(&type, &dev, &pet);
		uu_dprint(UU_DTRC,(us,"in ud_ondrag, type = %d", type));
		if(type == UD_LOCATOR)
		{
/*			-- set the active locater PET to drag -- */

			ud_dtcord(UD_LOCATOR, (*draginfo).rubberpet, 21);
			(*draginfo).rubberon = UU_TRUE;
		}
		else
			(*draginfo).rubberon = UU_FALSE;
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : ud_enddrag(draginfo)
**       end drag band sequence
**    PARAMETERS   
**       INPUT  : 
**          draginfo = rubber band control block
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_enddrag(draginfo)
UD_RUBBER *draginfo;
{
	int type;			/* interaction type */
	int dev;				/* device number */
	int pet;				/* prompt and echo type */
	 
	uu_denter(UU_DTRC,(us,"ud_enddrag()"));

/*	-- restore locator state if drag on and current state is locator -- */

	if((*draginfo).rubberon == UU_TRUE)
	{
		ud_qcord(&type, &dev, &pet);
		uu_dprint(UU_DTRC,(us,"in ud_enddrag, type=%d, save t,d,p=%d,%d,%d",
				type, (*draginfo).type, (*draginfo).dev, (*draginfo).pet));
		if(type == UD_LOCATOR)
			ud_dtcord((*draginfo).type, (*draginfo).dev, (*draginfo).pet);
	}
	uu_dexit;
}
