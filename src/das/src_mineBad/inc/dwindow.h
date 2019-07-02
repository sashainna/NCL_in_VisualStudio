/*********************************************************************
**    NAME         :  dwindow.h
**       CONTAINS:
**       	Window creation macros
**
**				To do rubber band line the sequence is
**					WINDOW_INIT to initialize
**					WINDOW_ON to open the window
**					WINDOW_WRITE to write to window
**					WINDOW_OFF when through
**
**				The macros will take care of cleaning up in the event of
**				a command reject or jump-to-root action by the operator.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       dwindow.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:15
**
*********************************************************************/

#ifndef DWINDOW
#include "usysdef.h"
#include "dmark.h"
#include "go.h"

/*	-- Window Control Block -- */

	typedef struct
	{
		Gnrect winbnd;			/* window extents */
		int border_color;    /* color of window border */
		int bckgrnd_color;   /* background color of window */
		int up_flag;			/* if UU_TRUE, leave window up between inputs */
		int resflag;			/* reserve window flag */
		int retflag;			/* return if window reserved flag */
		int busyflag;			/* window free/busy (only if retflag == UD_RETURN) */
		int markval;			/* MARK value storage */
		int str_dev_num;		/* string device number */
	} UD_WINDOW_REC;

/*********************************************************************
**    E_FUNCTION :  WINDOW_INIT(wcb)
**       initialize window window control block
**
**    PARAMETERS   
**       INPUT  : 
**          window			Window NDC extents.
**				border_col   	Color of window border.
**				back_col			Background color of window.
**       OUTPUT : 
**    		wcb = initialized window control block
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define WINDOW_INIT(wcb, window, border_col, back_col) \
	ud_initwin_rec(wcb, window, border_col, back_col)

/*********************************************************************
**    E_FUNCTION :  WINDOW_OFF(wcb)
**       close window
**
**    PARAMETERS   
**       INPUT  : 
**          wcb = window control block
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define WINDOW_OFF(wcb) {ud_kiwin(wcb); UD_UNMARK((*wcb).markval); }

/*********************************************************************
**    E_FUNCTION :  WINDOW_ON(wcb)
**       activate window
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
*********************************************************************/

#define WINDOW_ON(wcb, rows, columns) { \
	(*wcb).str_dev_num = ud_crwin(wcb, rows, columns); \
	UD_MARK((*wcb).markval, UU_FALSE); \
	if((*wcb).markval != 0) \
		WINDOW_OFF(wcb); }

/*********************************************************************
**    E_FUNCTION :  WINDOW_WRITE(wcb, string)
**       write to a window
**
**    PARAMETERS   
**       INPUT  : 
**          wcb = window control block
**          string = string to write to a window
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define WINDOW_WRITE(wcb, string) ud_wrwin(string)

#define DWINDOW
#endif
