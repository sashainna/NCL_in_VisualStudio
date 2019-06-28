/*********************************************************************
**
**    NAME         :  d3win.c
**
**       CONTAINS:
**    		  ud_crwin
**    		  ud_kiwin
**    		  ud_suspwin
**    		  ud_resuwin
**    		  ud_wrwin
**    		  ud_modwin
**				  ud_eraswin
**				  ud_initwin_rec
**				  ud_inquire_window
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d3win.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:07
**
*********************************************************************/

#include "usysdef.h"
#include "gistr.h"
#include "ustdio.h"
#include "dwindow.h"
#include "dasg.h"
#include "dasnog.h"
#include "dinput.h"
#include "gtbl.h"
#include "ginq.h"
#include "gerrorst.h"
#include "usysg.h"
#include "zsysdep.h"
#include "udebug.h"
#include "uhep.h"
#include "gcolors.h"
#include "xenv1.h"
#include "dcapture.h"

/* NCL - added to get the window overlaying the icons fixed.kathy */
/*
#include "nclicons.h"
#include "ws411x.h"
#include "dsubcom.h"
#include "diconm.h"
*/

static int windowbusy = UD_FREE;				/* window busy / free */
void ud_capture_window();

/*********************************************************************
**
**    E_FUNCTION: ud_crwin(wcb, rows, columns)
**			Create a scrolling text window.
**
**    PARAMETERS   
**       INPUT  : 
**          (*wcb).winbnd			Window NDC extents.
**				(*wcb).border_color   Color of window border.
**				(*wcb).bckgrnd_color  Background color of window.
**				(*wcb).resflag 			Reserve window flag
**									  UU_RESERVE  - Reserve this window for 
**													    exclusive use of the calling
**													    program and keeps other 
**													    windows from being displayed.
**									  UU_NORESERVE- Let this window be overwritten
**													    and contents damaged by other 
**													    windowing operations.
**				(*wcb).retflag 			Return if window reserved flag.
**									  UD_RETURN   - Allows calling program
**													    to handle the case of the 
**													    previously defined reserved
**													    window.
**									  UD_NORETURN - Let ud_crwin handle the 
**													    case of a reserved window.
**				
**       OUTPUT :  
**				rows						Actual number of rows in window.
**				columns					Actual number of columns in window.
**				(*wcb).busyflag		Window free/busy (only if retflag == UU_TRUE).
**												UU_FREE if free
**												UU_BUSY if busy
**												UU_WINDOWERR if error creating window
**
**    RETURNS      : string device number
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

int ud_crwin(wcb, rows, columns)
UD_WINDOW_REC *wcb;			/* window control block */
int *rows;						/* row return cell */
int *columns;					/* columns return cell */
{
	Glntype solidline;
	Gdrect window;						/* window extents in device coordinates */
	Gerror status, ginitstring();
	static char *nullstr = "";
	/* initialize record */
	static Gstringrec instring= {80, 0, "", 20, 40, 1, UG_C_WHITE, UG_C_BLACK };

	uu_denter(UU_DTRC,(us,"ud_crwin(): UD_windev=%d, ptr=%d", UD_windev,
							UD_winstate_ptr));

	if(windowbusy == UD_FREE)
	{
		if(UD_winstate_ptr >= UD_MAX_NUM_WIN)
		{
			uu_uerror0(UD_DASHEP, 100);
			goto winerr;
		}

/* 	-- save the current linetype and linecolor -- */

		UD_winstate_ptr++;
		UD_winstate[UD_winstate_ptr].state = UD_WINDOWON;
		zbytecp(UD_winstate[UD_winstate_ptr].oldlntype,*gqlinetype());
		UD_winstate[UD_winstate_ptr].oldlinecolr=gqlinecolor();

/*		-- set the character height, linetype, linecolor-- */

		gscharheight((UU_REAL) .013);
		solidline.typeno=1;				/* solid lines */
		gslinetype(&solidline);

/*		-- get the next string device number -- */

		UD_windev++;

/*		-- initialize the init record -- */

		instring.lins = 100;
		instring.cols = 100;
		instring.border_color = (*wcb).border_color;
		instring.bckgrnd_color = (*wcb).bckgrnd_color;
		instring.perm_flag = (*wcb).up_flag;	/* Take down during pick/loc */

/*		-- convert NDC to DC coordinates -- */

		ud_devrect(&(*wcb).winbnd, &window);

/*		-- put the device in request mode -- */

		if(UD_Eventm == UU_TRUE)
			gsstringmode(UD_ksws, UD_windev, UG_REQUEST, UG_ECHO);

/*		-- activate the window -- */

		status = ginitstring(UD_ksws, UD_windev, nullstr, 22, &window, &instring);
		if(status != NCL_NO_ERROR)
			goto winerr;

/*		-- set the row & column count in common the character count -- */

		UD_winrow = instring.lins;
		UD_wincol = instring.cols;

/*		-- bring it up by putting it in event mode -- */

		if(UD_Eventm == UU_TRUE)
			gsstringmode(UD_ksws, UD_windev, UG_EVENT, UG_ECHO);

/*		-- set up return parameters -- */

		*rows = UD_winrow;
		*columns = UD_wincol;
		if((*wcb).retflag == UD_RETURN)
			if((*wcb).busyflag != NULL)
				(*wcb).busyflag = UD_FREE;
		if((*wcb).resflag == UD_RESERVE)
			windowbusy = UD_BUSY;
		uu_dexit;
		return(UD_windev);
	}
	else
	{

/*		-- window is already in use -- */

		uu_uerror0(UD_DASHEP, 91);
		if((*wcb).retflag == UD_RETURN)
		{
			if((*wcb).busyflag != NULL)
				(*wcb).busyflag = UD_BUSY;
			uu_dexit;
			return(0);
		}
		else
			ud_jump(-1, UU_FALSE);
	}

/*	-- window creation error -- */

winerr:
	gslinetype(&UD_winstate[UD_winstate_ptr].oldlntype);
	gslinecolor(UD_winstate[UD_winstate_ptr].oldlinecolr);
	UD_winstate_ptr--;
	windowbusy = UD_winstate[UD_winstate_ptr].state;
	uu_uerror0(UD_DASHEP, 95);
	if((*wcb).retflag == UD_RETURN)
	{
		if((*wcb).busyflag != NULL)
			(*wcb).busyflag = UD_WINDOWERR;
		uu_dexit;
		return(UU_FALSE);
	}
	else
		ud_jump(-1, UU_FALSE);
	return(0);
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_kiwin()
**       kill window routine
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

void ud_kiwin()
{
	static Gstringrec instring= {80, 0, "", 20, 40}; /* initialize record */
	static Gdrect winbnd={0.,0.,1.,.6};					 /* window extents */

/*	-- bring it down by putting it in request mode and re-initializing
		it to some pet other than 22 (use pet 1) -- */

	uu_denter(UU_DTRC,(us,"ud_kiwin(): UD_windev = %d", UD_windev));

	if(UD_Eventm == UU_TRUE)
		gsstringmode(UD_ksws, UD_windev, UG_REQUEST, UG_ECHO);

	instring.lins = 100;
	instring.cols = 100;
	ginitstring(UD_ksws, UD_windev, "", 1, &winbnd, &instring);

	UD_windev--;

/* -- put back the linetype, linecolor, saved by ud_crwin -- */

	gslinetype(&(UD_winstate[UD_winstate_ptr].oldlntype));
	gslinecolor(UD_winstate[UD_winstate_ptr].oldlinecolr);
	UD_winstate[UD_winstate_ptr].state = UD_WINDOWOFF;
	UD_winstate_ptr--;

	windowbusy = UD_FREE;
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_suspwin()
**       suspend window routine
**
**    PARAMETERS   
**       INPUT  : 
**					none
**       OUTPUT :  
**					none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_suspwin()
{
	uu_denter(UU_DTRC,(us,"ud_suspwin(): dev=%d, ptr=%d", 
			UD_windev, UD_winstate_ptr));

/*	-- if window system has been initted -- */

	if(UD_winstate_ptr >= 0)
	{
		uu_dprint(UU_DTRC,(us,"ud_suspwin(): state=%d", 
			UD_winstate[UD_winstate_ptr].state));

/*		-- bring it down by putting it in request mode -- */

		if(UD_winstate[UD_winstate_ptr].state == UD_WINDOWON)
		{
			if(UD_Eventm == UU_TRUE)
				gsstringmode(UD_ksws, UD_windev, UG_REQUEST, UG_ECHO);
			UD_winstate[UD_winstate_ptr].state = UD_WINDOWSUSP;
			UD_winstate[UD_winstate_ptr].pet = UD_strech;
			UD_winstate[UD_winstate_ptr].dev = UD_strdev;
			UD_strech = 1;
			UD_strdev++;

/* 		-- save the current linetype and linecolor -- */

			zbytecp(UD_winstate[UD_winstate_ptr].oldlntype,*gqlinetype());
			UD_winstate[UD_winstate_ptr].oldlinecolr=gqlinecolor();
		}
	}
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_resuwin()
**       resume window routine
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

void ud_resuwin()
{
	uu_denter(UU_DTRC,(us,"ud_resuwin(): dev=%d, ptr=%d", 
			UD_windev, UD_winstate_ptr));

/*	-- if window system has been initted -- */

	if(UD_winstate_ptr >= 0)
	{
		uu_dprint(UU_DTRC,(us,"ud_resuwin(): state=%d", 
			UD_winstate[UD_winstate_ptr].state));

		if(UD_winstate[UD_winstate_ptr].state == UD_WINDOWSUSP)
		{

/*		-- bring it up by putting it in event mode -- */

			if(UD_Eventm == UU_TRUE)
				gsstringmode(UD_ksws, UD_windev, UG_EVENT, UG_ECHO);
			UD_winstate[UD_winstate_ptr].state = UD_WINDOWON;
			UD_strech = UD_winstate[UD_winstate_ptr].pet;
			UD_strdev = UD_winstate[UD_winstate_ptr].dev;

/* 		-- put back the linetype, linecolor, saved by ud_suspwin -- */

			gslinetype(&(UD_winstate[UD_winstate_ptr].oldlntype));
			gslinecolor(UD_winstate[UD_winstate_ptr].oldlinecolr);
		}
	}
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_wrwin(message)
**       write to a scrolling window
**
**    PARAMETERS   
**       INPUT  : 
**          message = message string
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_wrwin(message)
char *message;							/* message buffer */

{

/*	gputstring(UD_ksws, UD_windev, message);*/
	ul_win_out(message,0);
	ud_capture_window(message);
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_capture_window()
**       write scrolling window text to a file.
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

void ud_capture_window(message)
char *message;
{
	int  status;
	FILE *fd;								/* os file descriptor */
	if(UD_capture.state == UU_TRUE)
	{
		ux_get_os_filedesc(UD_capture.cplun, &fd, UX_PRTERRS);
		status = ux_fputs0(message, fd);
	}
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_modwin(window, inc)
**       modify a scrolling window location
**
**    PARAMETERS   
**       INPUT  : 
**          window = window buffer
**          increment = window increment
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_modwin(window, inc)
Gdrect *window;					/* window buffer */
UU_REAL inc;						/* increment value */
{
	(*window).ll.x = (*window).ll.x + inc;
	(*window).ll.y = (*window).ll.y - inc;
	(*window).ur.x = (*window).ur.x + inc;
	(*window).ur.y = (*window).ur.y - inc;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_eraswin()
**      erase window text 
**
**    PARAMETERS   
**       INPUT  : 
**					none
**       OUTPUT : 
**					none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_eraswin()
{
	static int pos[2] = {1,1};		/* new cursor position */

	uu_denter(UU_DTRC,(us,"ud_eraswin(): UD_windev = %d", UD_windev));

/* -- erase text from window -- */

	gansiescr(UD_ksws,UD_windev);

/* -- reset cursor position -- */

	gansispos(UD_ksws,UD_windev,pos);
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_initwin_rec(wcb, window, border_col, back_col)
**      				initialize window window control block 
**
**    PARAMETERS   
**       INPUT  : 
**          window			Window NDC extents.
**				border_col   	Color of window border.
**				back_col			Background color of window.
**       OUTPUT : 
**    		wcb = initialezed window control block
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_initwin_rec(wcb, window, border_col, back_col)
UD_WINDOW_REC *wcb;			/* window control block */
Gnrect *window;				/* window extents */
int border_col;          	/* color of window border */
int back_col;         		/* background color of window */
{
	(*wcb).winbnd.ll.x = (*window).ll.x;
	(*wcb).winbnd.ll.y = (*window).ll.y;
	(*wcb).winbnd.ur.x = (*window).ur.x;
	(*wcb).winbnd.ur.y = (*window).ur.y;

	(*wcb).border_color = border_col;
	(*wcb).bckgrnd_color = back_col;

	(*wcb). resflag = UD_NORESERVE;
	(*wcb). retflag = UD_NORETURN;
	(*wcb). busyflag = UD_FREE;

	(*wcb).up_flag = UU_FALSE;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_inquire_window()
**      inquire window state 
**
**    PARAMETERS   
**       INPUT  : 
**					none
**       OUTPUT : 
**					none 
**
**    RETURNS      : UU_FREE or UD_BUSY
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_inquire_window()
{
	uu_denter(UU_DTRC,(us,"ud_inquire_window()=%d", windowbusy));
	uu_dexit;
	return(windowbusy);
}
