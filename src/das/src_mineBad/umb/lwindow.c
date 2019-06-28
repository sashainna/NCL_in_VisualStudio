/*********************************************************************
**	FILENAME: lwindow.c
**	CONTAINS:		ul_open_window
**				ul_close_window
**				ul_win_out
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lwindow.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:22
*********************************************************************/
#include "usysdef.h"
#include "lumb.h"
#include "dasnog.h"
#include "ddef.h"
#include "dinput.h"
#include "driver.h"
#include "dwindow.h"
#include "gcolors.h"
#include "ginqdsiz.h"
#include "gobas.h"
#include "gomisc.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "uhep.h"
#include "uims.h"
#include "umasks.h"
#include "usysg.h"
#include "nclmodals.h"
#include "nclfc.h"
#include "dmotif.h"

#define TRACE UU_TRUE

/*extern float NCL_char_height;*/

static int actwin=0;
static int Wlines=20;
int UL_clswin_flag = 0;
/*********************************************************************
**	 E_FUNCTION:int ul_open_window(line,col,args)
**		This function opens a window for output.
**	 PARAMETERS	
**		 INPUT  : line = # of lines in window
**			  col = # of columns in window
**			  args[1] =	0 = no border
**					1 = white border
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
**       NOTES: This routine was "borrowed" from "ub_open_window",
*********************************************************************/
int ul_open_window(line,cols,args)
	int line,cols,args[3];
{
	int status;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us, "ul_open_window(line,cols,args)"));
	status = UU_SUCCESS;	/* assume success */

/*
.....If running on X-Window workstation
.....then call X-Window dependant
.....windowing routine
*/
	status = ud_open_window(line,cols);
	if (status == UU_SUCCESS) actwin = 1;
	Wlines = line;

done:;
	uu_dexit;
	return(status);
}	
/**************************************************************************
**
**  E_FUNCTION:  ul_close_window
**      Close the scrolling window and reset command reject action.
**
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

ul_close_window()
{
	int stat = 1;
	if (actwin == 1)
	{
/*
.....If running on X-Window workstation
.....then call X-Window dependant
.....windowing routine
*/
#if UU_COMP==UU_WIN2K
		if ((UDM_run_layout.statwin_type==0) && (UL_clswin_flag==0))
			return 0;
#endif
		stat = ud_close_window();
		if (stat)
		{
			actwin = 0;
			clswin(); 
		}
	}
	return stat;
}

/**************************************************************************
**
**  E_FUNCTION:  ul_win_out
**      write a string from the directory routines to the output
**      device.  If the screen is full wait for the user to hit a key
**			to continue
**
**  PARAMETERS   
**      INPUT  :  buff	:	string to be written out
**		  start	:	if == 0 reset line counter
**      OUTPUT :  none
**  RETURNS      :  UU_SUCCESS normally, UU_FAILURE if user requested display
**			to be terminated.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**  NOTES:	 :  This routine was "borrowed" from ux_udos_out.
**************************************************************************/
ul_win_out(buff, start)
	char *buff;
	int start;
{
	char buff2[250],com[10];
	int status,numint,l;
	static int nlines = {0};	/* number of lines from current command	*/
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Increment the # of output lines
*/
	nlines = 1;
/*	nlines++;*/
/*
.....Full window
.....Prompt user to continue
*/
	if (nlines == Wlines)
	{
		ud_das (UD_DASSTRING,"Enter RETURN to continue or Q to quit",
			com,sizeof(com),&numint);
		if (numint > 0 && (com[0]=='q' || com[0]=='Q')) status = UU_FAILURE;
		nlines = 1;
	}
/*
.....If running on X-Window workstation
.....then call X-Window dependant
.....windowing routine
*/
	l = strlen(buff);
	strcpy(buff2,buff);
	if (buff2[l-1] == '\n') buff2[l-1] = '\0';
	ud_win_out(buff2);
/*
.....Write to file capture
*/
	strcat(buff2,"\n");
	ud_capture_window(buff2);
	return(status);
}
