#include "usysdef.h"
#if UU_COMP != UU_WIN2K

/********************************************************************* 
**  NAME:  wsmfhlp.c
**
**       Help text window routine.
**
**  COPYRIGHT  2000  NCCS Inc. (c) All rights reserved.
**
**	 CONTAINS:
**
**		uw_open_hlpwin
**		uw_close_hlpwin
**		uw_set_helptxt
**
**    MODULE NAME AND RELEASE LEVEL 
**			wsmfhlp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:11
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:DialogS.h>
#include <decw$include:Text.h>
#include <decw$include:PanedW.h>
#include <decw$include:Protocols.h>
#include descrip
#include dvidef
#include iodef
#include dcdef
#include ttdef
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <Xm/DialogS.h>
#include <Xm/Text.h>
#include <Xm/PanedW.h>
#include <Xm/Protocols.h>
#endif

#include "wsxw.h"
#include "wsmf.h"
#include "udebug.h"
#include "dasnog.h"
#include "uims.h"
#include <stdio.h>

#define MAXLIN 100

typedef struct
{
   int vis; /* visible flag */
   Widget shell;  /* display window id */
   Widget text;
   Widget pane;
} UW_MFhlpwin;

static UW_MFhlpwin help_window =
{
	UU_FALSE,
	NULL,
	NULL,
	NULL,
};
extern UWS_MF uw_mf;

/**************************************************************************
**
**  E_FUNCTION:  help_quitCB()
**      Called when the user closes the help window manually.
**
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
static void help_quitCB()
{
	uw_close_hlpwin();
	help_window.shell = NULL;
}

/*********************************************************************
**	 E_FUNCTION:int uw_open_hlpwin(title)
**		This function opens a scrolling Help window for output.
**	 PARAMETERS	
**		 INPUT  :
**			 		title: window title 
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int uw_open_hlpwin(title)
char *title;
{
	int status,n;
	Atom watom;
	Arg args[20];
/*
.....Assume success
*/
	status = UU_SUCCESS;
	help_window.vis = UU_TRUE;
/*
.....Window already created
.....Just remap it
*/
	if (help_window.shell != NULL)
	{
		XtManageChild(help_window.text);
		XtManageChild(help_window.pane);
		XtPopup(help_window.shell,XtGrabNone);
		return(status);
	}
/*
....Open PopUp dialog window
*/
	n = 0;
	XtSetArg(args[n],XmNtitle, title); n++;
	XtSetArg(args[n],XmNallowResize,True); n++;
	help_window.shell = XtCreatePopupShell("helpWindow",
		xmDialogShellWidgetClass, uw_mf.graphic_app, args,n);
/*
.....Trap the CLOSE button
*/
	watom =  XmInternAtom(uw_xw.disp,"WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(help_window.shell, watom,
											( XtCallbackProc) help_quitCB,NULL);
/*
.....Create paned window to hold text window
*/
	help_window.pane = XtVaCreateWidget("pane",
		xmPanedWindowWidgetClass,help_window.shell,
		XmNsashWidth,1,XmNsashHeight,1,NULL);
/*
.....Place scrolling text widget in dialog
*/
	n = 0;
	XtSetArg(args[n], XmNrows, 24); n++;
	XtSetArg(args[n], XmNcolumns, 80); n++;
	XtSetArg(args[n], XmNresizeWidth, True); n++;
	XtSetArg(args[n], XmNresizeHeight, True); n++;
	XtSetArg(args[n], XmNscrollVertical, True); n++;
	XtSetArg(args[n], XmNscrollHorizontal, False); n++;
	XtSetArg(args[n], XmNeditable, False); n++;
	XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
	help_window.text = XmCreateScrolledText(help_window.pane,
		"statWindow",args,n);
/*
.....Blank out the text window
*/
	XmTextSetString(help_window.text,"");
/*
.....Manage the text window
*/
	XtManageChild(help_window.text);
	XtManageChild(help_window.pane);
	XtPopup(help_window.shell,XtGrabNone);
	goto done;
/*
.....Failure
*/
failed:;
	help_window.vis = UU_FALSE;
	status = UU_FAILURE;
done:;
	return(status);
}

/**************************************************************************
**
**  E_FUNCTION:  uw_close_hlpwin
**      Close the help window.
**
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uw_close_hlpwin()
{
/*
.....Window is visible
.....Destroy it
*/
	if(help_window.vis == UU_TRUE)
	{
		help_window.vis = UU_FALSE;
		XtUnmanageChild(help_window.text);
		XtUnmanageChild(help_window.pane);
		XtPopdown(help_window.shell);
	}
	return(UU_SUCCESS);
}

/**************************************************************************
**
**  E_FUNCTION:  uw_set_helptxt(helpstr)
**      Set help text in the help window window.
**			This will reset help text if there is help text in the text field
**
**  PARAMETERS   
**      INPUT  :  helpstr: help text to write
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uw_set_helptxt(helpstr)
char *helpstr;
#define tab 9
#define field 7
{
/*
.....Window is visible
*/
	if(help_window.vis == UU_TRUE)
	{
#if UU_COMP != UU_VAXVMS
#if UU_COMP != UU_IRIS4D
#if UU_COMP != UU_HPUX
		XmTextDisableRedisplay(help_window.text);
#endif
#endif
#endif /* VAXVMS */
		XmTextInsert(help_window.text,0,helpstr);
		XmTextShowPosition(help_window.text,0);
#if UU_COMP != UU_VAXVMS
#if UU_COMP != UU_IRIS4D
#if UU_COMP != UU_HPUX
		XmTextEnableRedisplay(help_window.text);
#endif
#endif
#endif
	}
	return(UU_SUCCESS);
}

#endif
