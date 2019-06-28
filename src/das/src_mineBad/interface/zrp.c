/*********************************************************************
**
**    NAME         :  zrp.c
**
**    CONTAINS:
**				uz_zrecon
**				uz_zplayback
**				uz_zrecoff
**				uz_zpause
**				uz_zresu
**				uz_zpanic
**
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       zrp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:37
**
*********************************************************************/

#include "usysdef.h"
#include "dinput.h"
#include "uhep.h"
#include "udebug.h"
#include "nclfc.h"	/*NCL: now call rstint() from uz_zplayback() */

UU_LOGICAL	ud_recon(), ud_playback(), ud_inevt();

/*********************************************************************
**
**    E_FUNCTION     :  int uz_zrecon()
**       Record on for DDC
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

uz_zrecon()
{
	UD_DEVENT event;					/* event buffer for write r/p */
	int status;							/* status return cell */

	uu_denter(UU_DTRC,(us,"in uz_zrecon"));

	if(UD_Rpstate[UD_Rpstate_ptr].flag == RPOFF)
	{

/*		-- start the record going -- */
		
		status = ud_recon();

		if(status == UU_TRUE)
			uz_actrp("RECORD ON");
		else
			uu_uerror0(UD_DASHEP, 62);
	}
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION     :  int uz_zplayback()
**       Playback for DD1
**
**    PARAMETERS   
**       INPUT  : 
**          parms: most case it is playback filename
**					some place called with command "\playb"
**       OUTPUT :  
**          NONE
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
/*
.....added parameter
*/
uz_zplayback(parms)
char *parms;
{
	int status;							/* status return cell */

	uu_denter(UU_DTRC,(us,"in uz_zplayback"));

	/*NCL: reset nclf flags for proper display, etc */
#if UU_COMP == UU_SUN
	rstint();
#endif

/*	-- start the playback going -- */
	
	if (strcmp(parms, "\\playb")==0)
		status = ud_playback("");
	else
  		status = ud_playback(parms);
	if(status == UU_TRUE)
		uz_actrp("PLAYBACK");

	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION     :  int uz_zrecoff()
**       Record Off for DD1
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uz_zrecoff()
{
	int status;							/* status return cell */

	uu_denter(UU_DTRC,(us,"in uz_zrecoff"));

	ud_recoff();
	if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		uz_actrp("RECORD ON");
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		uz_actrp("PLAYBACK");
	else
		uz_actrp("RECORD OFF");

	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION     :  int uz_zpause()
**       Record Off for DD1
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uz_zpause()
{
	int status;							/* status return cell */

	uu_denter(UU_DTRC,(us,"in uz_zpause"));

	if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
	{
		ud_susp();
		uz_actrp("RECORD PAUSE");
	}
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{
		ud_susp();
		uz_actrp("PLAY PAUSE");
	}
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION     :  int uz_zresu()
**       Record Off for DD1
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uz_zresu()
{
	int status;							/* status return cell */

	uu_denter(UU_DTRC,(us,"in uz_zresu"));

	if(UD_Rpstate[UD_Rpstate_ptr].flag == RECSUSP)
	{
		ud_resu();
		uz_actrp("RECORD ON");
	}
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYSUSP)
	{
		ud_resu();
		uz_actrp("PLAYBACK");
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  int uz_zpanic()
**       Panic Stop for DD1
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uz_zpanic()
{
	uu_denter(UU_DTRC,(us,"in uz_zpanic"));
	uz_actrp("RECORD OFF");
	ud_inevt("STRING device = 1 string = \\\\F");
	uu_dexit;
}
