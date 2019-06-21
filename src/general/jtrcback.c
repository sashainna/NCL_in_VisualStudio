/*********************************************************************
**
**    NAME         :  jtrcback.c
**
**       CONTAINS:
**				uj_trcback
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       jtrcback.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:48
**
*********************************************************************/

#include "usysdef.h"
#include "ustdio.h"
#include "uims.h"
#include "dwindow.h"
#include "dasnog.h"
#include "ustdio.h"
#include "dmenucom.h"
#include "udebug.h"
#include "uhep.h"
#include "gcolors.h"         /* color definitions - ud_crwin */

/*********************************************************************
**
**    E_FUNCTION     : uj_trcback()
**       display subsystem traceback stack
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uj_trcback()
{
	int i;
	char trcbuf[50];
	char buf[30];
	int markval;
	int bckgrnd;									/* background color of ansi window */
	UD_WINDOW_REC wcb;							/* window control block */
	static Gdrect trcwin = {.2,.2, .7,.6}; /* traceback window bounds */

	uu_denter(UU_DTRC,(us,"entering uj_trcback, trcptr=%d", UD_sstrc_ptr));

/*	-- open a window -- */

	bckgrnd = dqwinback();
	WINDOW_INIT(&wcb, &trcwin, UG_C_WHITE, bckgrnd);
	WINDOW_ON(&wcb, &UD_winrow, &UD_wincol);

/*		-- output header -- */

	WINDOW_WRITE(&wcb, "   SUBSYSTEM STACK\n");
	WINDOW_WRITE(&wcb, "\n");

/*		-- put up the next subsystem traceback -- */

	for(i=0; i<UD_sstrc_ptr; i++)
	{
		sprintf(trcbuf, " %d - %s\n", i+1, UD_sstrc[i]);
		WINDOW_WRITE(&wcb, trcbuf);
	}

/*		-- output date and time -- */

	WINDOW_WRITE(&wcb, "\n");
	WINDOW_WRITE(&wcb, "          Date and Time\n");
	uu_udatime(buf);
	sprintf(trcbuf, "      %s", buf);
	WINDOW_WRITE(&wcb, trcbuf);

/*			-- enter any key to terminate -- */

	ud_hakt(UD_DASHEP, 17);
	WINDOW_OFF(&wcb);
	uu_dexit;
}
