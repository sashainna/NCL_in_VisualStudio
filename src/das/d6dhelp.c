/*********************************************************************
**
**    NAME         :  d6dhelp.c
**
**       CONTAINS:
**    		ud_help
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d6dhelp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:13
**
*********************************************************************/

#include "usysdef.h"
#include "dwindow.h"
#include "dasnog.h"
#include "uims.h"
#include "derror.h"
#include "udebug.h"
#include "uhep.h"
#include "gcolors.h"         /* color definitions - ud_crwin */

/*********************************************************************
**
**    E_FUNCTION         :  ud_help(cptr)
**       set up user interface for unicad system
**
**    PARAMETERS   
**       INPUT  : 
**          cptr = command line used to invoke
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_help(cptr)
char *cptr;							/* command line */
{
	char *uu_uerhep(), *uu_upmthep(), *uu_usysinfor();
	char *uu_aerhep(), *uu_apmthep(), *uu_asysinfor();
	int linectr;						/* line counter */
	UU_LOGICAL first;					/* first time flag */
	char *msg;							/* message return pointer */
	UU_LOGICAL status;				/* status return */
	char reply[2];						/* reply buffer to continue message */
	int markval;						/* mark return integer */
	int locsys;							/* local system number */
	int locnum;							/* local prompt or error number */
	int type;
	Gdrect helpwin;					/* help window bounds */
	int bckgrnd;						/* bckgrnd color of window */
	UD_WINDOW_REC wcb;				/* window control block */
	int args[2];

	uu_denter(UU_DTRC,(us,"enter ud_help, state=%d, subsys=%d, number=%d", 
							UD_promptstate, UD_promptsys, UD_promptnum));

	if(strlen(cptr) > 5)
	{
		type = cptr[6];
		type = type - 48;
		switch(type)
		{

/*		-- system level help -- */

		case 1:
			locsys = UD_syssys;
			locnum = UD_sysnum;
			break;

/*		-- prompt level help -- */

		case 2:
			locsys = UD_promptsys;
			locnum = UD_promptnum;
			break;

/*		-- DAS level help -- */

		case 3:
			locsys = UD_DASHEP;
			locnum = UD_dasnum;
			break;

/*		-- error level help -- */

		case 4:
			locsys = UD_errorsys;
			locnum = UD_errornum;
			break;
		}
	}
	else
	{
		type = 1;
		locsys = UQ_CALCERROR;
		locnum = 1;
	}

	if(locsys != 0)
	{
		status = UU_TRUE;
		first = UU_TRUE;
		strcpy(reply, " ");

/*	-- open a window -- */

/*		bckgrnd = dqwinback();
		WINDOW_INIT(&wcb, UD_HELPWIN, UG_C_WHITE, bckgrnd);

		WINDOW_ON(&wcb, &UD_winrow, &UD_wincol);*/
		args[1] = 1;
		UD_winrow = 20; UD_wincol = 80;
		ul_open_window(UD_winrow,UD_wincol,args);
		linectr = UD_winrow - 3;

/*		-- read the prompt file and put it up -- */

		while(status == UU_TRUE)
		{
			switch(type)
			{

/*		-- system level help -- */

			case 1:
				if(UD_promptstate == UD_UNISTATE)
					msg = uu_usysinfor(locsys, first, &status, UD_wincol);
				else
					msg = uu_asysinfor(locsys, first, &status, UD_wincol);
				break;

/*		-- prompt level help -- */

			case 2:
				if(UD_promptstate == UD_UNISTATE)
					msg = uu_upmthep(locsys, locnum, first, &status, UD_wincol);
				else
					msg = uu_apmthep(locsys, locnum, first, &status, UD_wincol);
				break;

/*		-- DAS level help -- */

			case 3:
				msg = uu_upmthep(UD_DASHEP, locnum, first, &status, UD_wincol);
				break;

/*		-- error level help -- */

			case 4:
				if(UD_errorstate == UD_UNISTATE)
					msg = uu_uerhep(locsys, locnum, first, &status, UD_wincol);
				else
					msg = uu_aerhep(locsys, locnum, first, &status, UD_wincol);
				break;

			}

			first = UU_FALSE;
			if(status == UU_TRUE)
			{
				linectr--;
				if(linectr == 0)
				{

/*			-- enter any key to continue -- */

					ud_hakt(UD_DASHEP, 16);
					linectr = UD_winrow - 3;
				}
/*				WINDOW_WRITE(&wcb, msg);*/
				ul_win_out(msg,0);
			}
		}

/*			-- enter any key to terminate -- */

		ud_hakt(UD_DASHEP, 17);
/*		WINDOW_OFF(&wcb);*/
		ul_close_window();
	}
	else
	{

/*		-- help is not in effect for this request -- */

	}
	uu_dexit;
}
