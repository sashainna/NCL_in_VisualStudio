/*********************************************************************
**
**    NAME         :  jmisc.c
**
**       CONTAINS:
**    		uj_refpnt
**    		uj_help
**				uj_dynpan
**				uj_dynzoom
**				uj_dynxyrot
**				uj_dynzrot
**				uj_dyntumble
**				uj_dynvecrot
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       jmisc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:46
**
*********************************************************************/

#include "usysdef.h"
#include "ustdio.h"
#include "uhep.h"
#include "calcom.h"
#include "dasnog.h"
#include "dmenucom.h"
#include "dsubcom.h"
#include "mdcpln.h"
#include "dinput.h"
#include "view.h"
#include "udebug.h"

UU_LOGICAL	ud_qlocpet();
extern int UW_dynamic_funkey;
extern int (*UW_dynfunc_key)();

#define RESL if(UV_act_screen[0].nvports>1)ud_dtcord(UD_LOCATOR, 1, 1)

/*********************************************************************
**
**    E_FUNCTION :  uj_refpnt()
**			reset reference point
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

uj_refpnt()
{
	int numint;						/* number of interactions */
	int deflag;						/* default flag */
	UU_REAL coord[3];				/* coordinate input buffer */
	CRSLT stbptr;					/* symbol table entry pointer */

	uu_denter(UU_DTRC,(us,"uj_refpnt()"));

	if(uq_calc2("rp", &stbptr) == UU_TRUE)
	{
		deflag = UD_DEFAULT;
		UM_cc_exttoint(stbptr.val.cval, coord);
	}
	else
		deflag = UD_NODEFAULT;

/*	-- get new reference point (Note that just inputting the point will
		cause the reference point to change) -- */

	ud_ldas(UD_DASCART, UU_SIGNON, 1, coord, 1, &numint, deflag);

	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION :  uj_help()
**			access help system
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

uj_help()
{
	uu_denter(UU_DTRC,(us,"in uj_help()"));
	uu_dprint(UU_DTRC,(us,"in error=%d, %d; prompt=%d, %d", UD_errorsys,
					UD_errornum, UD_promptsys, UD_promptnum));

/*	-- error takes first precidence -- */

	if(UD_errorsys != 0)
	{
		ud_help("\\help,4");
		UD_errorsys = 0;
	}

/*	-- prompt takes next precidence -- */

	else if(UD_promptsys != 0)
	{
		ud_help("\\help,2");
		UD_promptsys = 0;
	}

/*	-- tutorial takes last precidence -- */

	else if(UD_tut_ptr != NULL) 
		uj_tutorial(UD_tut_ptr);

	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION :  uj_dynpan()
**			access dynamic pan
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

uj_dynpan()
{
	static char *myname = "Dynamic Pan";

	uu_denter(UU_DTRC,(us,"enter %s", myname));
	if(ud_qlocpet(1, 40)==UU_FALSE)
		uu_uerror1(UU_SIGNON, 5, myname);
/*	else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		uu_uerror1(UU_SIGNON, 10, myname);
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		uu_uerror1(UU_SIGNON, 10, myname);
*/	else 
	{
		RESL; 
		uvu_dynpan();
	}
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION :  uj_dynzoom()
**			access dynamic zoom
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

uj_dynzoom()
{
	static char *myname = "Dynamic Zoom";

	uu_denter(UU_DTRC,(us,"enter %s", myname));
	if(ud_qlocpet(1, 43)==UU_FALSE)
		uu_uerror1(UU_SIGNON, 5, myname);
/*	else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		uu_uerror1(UU_SIGNON, 10, myname);
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		uu_uerror1(UU_SIGNON, 10, myname);
*/	else 
	{
		RESL; 
		uvu_dynzoom();
	}
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION :  uj_dynxyrot()
**			access help system
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/
uj_dynxyrot()
{
	static char *myname = "Dynamic Rotate";

	uu_denter(UU_DTRC,(us,"enter %s", myname));
	if(ud_qlocpet(1, 42)==UU_FALSE)
		uu_uerror1(UU_SIGNON, 5, myname);
/*	else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		uu_uerror1(UU_SIGNON, 10, myname);
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		uu_uerror1(UU_SIGNON, 10, myname);
*/	else 
	{
		RESL; 
		uvu_dynxyrot();
	}
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION :  uj_dynzrot()
**			access help system
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

uj_dynzrot()
{
	char name[40];
	static char *myname = "Dynamic Rotate";

	uu_denter(UU_DTRC,(us,"enter %s", myname));
	if(ud_qlocpet(1, 41)==UU_FALSE)
		uu_uerror1(UU_SIGNON, 5, myname);
/*	else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		uu_uerror1(UU_SIGNON, 10, myname);
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		uu_uerror1(UU_SIGNON, 10, myname);
*/	else 
	{
started:;
		RESL; 
		uvu_dynzrot();
		if (UW_dynamic_funkey!=-1)
		{
			(*(UW_dynfunc_key)) (UW_dynamic_funkey, &name,1);
			UW_dynamic_funkey = -1;
			goto started;
		}
	}
	uu_dexit;
}


/*********************************************************************
**
**    E_FUNCTION :  uj_dyntumble()
**			access help system
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

uj_dyntumble()
{
	static char *myname = "Dynamic Tumble";

	uu_denter(UU_DTRC,(us,"enter %s", myname));
	if(ud_qlocpet(1, 44)==UU_FALSE)
		uu_uerror1(UU_SIGNON, 5, myname);
/*	else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		uu_uerror1(UU_SIGNON, 10, myname);
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		uu_uerror1(UU_SIGNON, 10, myname);
*/	else 
	{
		RESL; 
		uvu_dyntumble();
	}
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION :  uj_dynvecrot()
**			access help system
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/
uj_dynvecrot()
{
	static char *myname = "Dynamic Rotate";

	uu_denter(UU_DTRC,(us,"enter %s", myname));
	if(ud_qlocpet(1, 45)==UU_FALSE)
		uu_uerror1(UU_SIGNON, 5, myname);
/*	else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		uu_uerror1(UU_SIGNON, 10, myname);
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		uu_uerror1(UU_SIGNON, 10, myname);
*/	else 
	{
		RESL; 
		uvu_dynvecrot();
	}
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION :  uj_dynmouse()
**			access help system
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/
uj_dynmouse()
{
	static char *myname = "Dynamic Mouse Viewing";

	uu_denter(UU_DTRC,(us,"enter %s", myname));
	if(ud_qlocpet(1, 46)==UU_FALSE)
		uu_uerror1(UU_SIGNON, 5, myname);
/*	else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		uu_uerror1(UU_SIGNON, 10, myname);
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		uu_uerror1(UU_SIGNON, 10, myname);
*/	else 
	{
		RESL; 
		uvu_dynmouse();
	}
	uu_dexit;
}
