#include "usysdef.h"
#if UU_COMP != UU_VAXVMS || UU_OPSYS == UU_ALPHAVMS

/********************************************************************* 
**  NAME:  wsmfapp.c
**
**		CONTAINS:
**			uw_mfwindow()
**			uw_mfgetprmerr(msg)
**			uw_mfgetprm(msg)
**			uw_mfgetprlabel(msg)
**			uw_mferror(msg)
**			uw_mfprmerr(msg)
**			uw_mfwrprm(msg)
**			uw_mfwrplabel(msg)
**			uw_mfget_filename(title,filter,filename,nc)
**			uw_mfmd_filename(parent,title,filter,nfld,ifld,ftext,fdir,filename,nc)
**			uw_mfwait()
**			uw_mfget_prompt()
**			uw_mfreset_prompt()
**			uw_mfget_dirname(title,dir,nc)
**			uw_mfnew_session
**			uw_mfget_subncl()
**			uw_mfterm_sub()
**			uw_mfget_labelwid()
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsmfapp.c , 26.2
**    DATE AND TIME OF LAST MODIFICATION
**       04/12/18 , 10:31:43
**
*********************************************************************/
#if UU_COMP!=UU_WIN2K
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:cursorfont.h>
#include <decw$include:Xm.h>
#include <decw$include:PushB.h>
#include <decw$include:PanedW.h>
#include <decw$include:DrawingA.h> 
#include <decw$include:MwmUtil.h>
#include <decw$include:Text.h>
#include <decw$include:TextF.h>
#include <decw$include:Form.h>
#include <decw$include:Frame.h>
#include <decw$include:Label.h>
#include <decw$include:RowColumn.h>
#include <decw$include:ToggleB.h>
#include <decw$include:Separator.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/PanedW.h>
#include <Xm/DrawingA.h> 
#include <Xm/MwmUtil.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#ifdef UU_OPENGL
#include <GL/gl.h>
#include <GL/glx.h>
#include <GL/glu.h>
#endif
#endif
#include <stropts.h>
#include <stdio.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include "dinput.h"
#include "driver.h"
#include "ws.h"
#include "wsxw.h"
#include "wsmf.h"
#include "wsgl.h"
#include "dmark.h"
#include "dmotif.h"
#include "xenv1.h"
#include "zkeysym.h"
#include "xfsys1.h"

/*
.....Global variable definitions
*/
extern int NCQ_running;
extern int Ncq_ncl_id;
extern char NCL_keyinfo[9];
extern unsigned int NCL_subprocess;
static int NCQ_common_open = 0;
static int NCL_keycommon_open = 0;
static int NCL_common_key = -1;
extern int NCL_com_size;
static int NCQ_comm_pos = 0;
static char *NCQ_comptr = NULL;
static char *NCL_key_comptr = NULL;
static int NCQ_shmid = -1;
static int NCL_keyid = -1;
extern unsigned int NCL_subprocess;
static int NCL_sub_ncl = 0;
static pid_t ncl_sub_pid[50];
typedef struct
{
   int flag;
   char ppfile[256];
   int current, highest, lines;
   char macro[64];
   int warn, error;
} NCLInfo;

extern char NCL_keyinfo[9];

typedef struct
{
	int sel;
	char *file;
/*
.....added this varible to save formActive,
.....we use varible formActive for disable/enable
.....other application functions instead disable
.....all functions, cause we want use key functon
.....during browser displayed
.....Yurong 11/9/98
*/
	int save_formActive;
} FSELECT;
/*
.....added by Yurong
.....Yurong 8/27/97
*/
UWS_MFPROMPT uw_mfprompt;
UWS_MF uw_mf;
extern int XBorder[2],YBorder[2];
extern int Border_stat;

static int uw_mfanswer;
static int yes_or_no;
static int yes_no_cancel;
static Widget mine, radio[10];
static Widget prompt_txt[10];
static char *lastdir[10];
static int radio_state = 0;
extern int formActive;
static char prmtxt[200],errtxt[200],labtxt[200];
static char def_filter[200];
/*
.....added for get string prompt
.....Yurong 4/9/99
*/
typedef struct
{
   int sel;
   char *ans;
} UW_PROMPT;

/*
.....added for get multiple string prompt
.....Yurong 11/28/00
*/
typedef struct
{
   int sel;
	int n;
   char **ans;
} UW_NPROMPT;

Widget XmFileSelectionBoxGetChild();
static void uw_mfprompt_okCB(), uw_mfprompt_cancelCB();
static void uw_mfnprompt_okCB(), uw_mfnprompt_cancelCB();
extern void uw_mousemoveCB();
extern UU_REAL NCL_version;

void uw_close_keycom();
static XtIntervalId timer = -1;
static void S_mfremove_subproc();
static int S_if_process_running();
UX_pathname Scomexe;
/**********************************************************************
**    I_FUNCTION :  uw_GPS_resizeCB(widget,client_data, event)
**      Get size and position when graphic, status, and prompt area
**			 move or sized and save in the layout structure 
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**				client_data = Contains program information about moved
**				              window.
**                   1:   Graphic area
**							2:   Prompt area
**							3.   Status area
**				widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_GPS_resizeCB (widget,
	client_data,
	event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	int number;
	Position x, y;
	Dimension width, height;
	XConfigureEvent *cevent = (XConfigureEvent *)event;
	if(cevent->type != ConfigureNotify)
		return;
/*
...get size and position
*/
	XtVaGetValues(widget, XmNx, &x, XmNy, &y, NULL);
	XtVaGetValues(widget, XmNwidth, &width, XmNheight, &height, NULL); 
/*
...Save in the layout structure
*/
	number = (int)client_data;
		x = x - XBorder[0];
		y = y - YBorder[0];
		width = width  + 2*XBorder[0];
		height = height + 2*YBorder[0];
		
	if(number==1)
	{
		UDM_layout.graphic_pos[0] = x;
		UDM_layout.graphic_pos[1] = y;
		UDM_layout.graphic_size[0] = width;
		UDM_layout.graphic_size[1] = height;
	}
	else if(number==2)
	{
		UDM_layout.prompt_pos[0] = x;
		UDM_layout.prompt_pos[1] = y;
		UDM_layout.prompt_size[0] = width;
		UDM_layout.prompt_size[1] = height;
	}
	else if(number==3)
	{
		UDM_layout.status_pos[0] = x;
		UDM_layout.status_pos[1] = y;
		UDM_layout.status_size[0] = width;
		UDM_layout.status_size[1] = height;
	}

}

/**********************************************************************
**    I_FUNCTION :  exitCB(widget,client_data,call_data)
**       Dummy routine used in conjunction with uw_mfwait().  Used to
**			exit the routine.
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Ignored.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Exits the application.
**    WARNINGS     : none
*********************************************************************/
static void exitCB(widget,client_data,call_data)
Widget widget;
XtPointer client_data,call_data;
{
	exit(0);
}
/**********************************************************************
**    I_FUNCTION :  exitCB(widget,client_data,call_data)
**    	Adjust graphic window size   
**		
**    PARAMETERS   
**       INPUT  : 
**				size
**       OUTPUT :  
**          size
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added for adjust border
*/
void uw_mfadjust_border(size)
int size[2];
{
	int  temp[2];
	temp[0] = UDM_layout.graphic_size[0] - 2*XBorder[0];
	temp[1] = UDM_layout.graphic_size[1] - 2*YBorder[0];
	size[0] = temp[0];
	size[1] = temp[1];
}
/**********************************************************************
**    I_FUNCTION :  uw_mfwindow()
**       Creates the Graphics, Prompt, and Status areas (windows).
**			These windows are not managed here, but wait until after the
**			Signon Form is accepted.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfwindow()
{
	int ifl,n;
	Arg args[20];
	void exitCB(),uw_mfpromptCB(),uw_mfprchgCB();
	char geom[20];
	void uw_mfgraphicCB();
	Widget frame;
	int pos[2], size[2];
/*
.....Open the GRAPHICS window
*/
	n = 0;
/*
........Get rid of Motif window decorations and menu
*/
	ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
			MWM_DECOR_MINIMIZE | MWM_DECOR_TITLE | MWM_DECOR_RESIZEH;
	XtSetArg(args[n],XmNmwmDecorations,ifl); n++;

/*
.....added for adjust pos and size
.....For SGI, we need adjust position
.....12/15/97 Yurong
*/
	pos[0] = UDM_layout.graphic_pos[0];
	pos[1] = UDM_layout.graphic_pos[1];
	size[0] = UDM_layout.graphic_size[0] - 2*XBorder[0];
	size[1] = UDM_layout.graphic_size[1] - 2*YBorder[0];
	if (Border_stat==0)
	{
		pos[0] += XBorder[0];
		pos[1] += YBorder[0];
	}
/*
........Set the window location
*/
/*	sprintf(geom,"%dx%d+%d+%d",
		UDM_layout.graphic_size[0],
		UDM_layout.graphic_size[1],
		UDM_layout.graphic_pos[0],
		UDM_layout.graphic_pos[1]); */
	sprintf(geom,"%dx%d+%d+%d",
		size[0], size[1], pos[0], pos[1]);

	XtSetArg(args[n],XmNgeometry,geom); n++;
/*
........Create the Graphics area
*/

	XtSetArg(args[n],XmNmappedWhenManaged,False); n++;
	uw_mf.graphic_app = XtAppCreateShell("Graphic","NCL",
				topLevelShellWidgetClass,
				uw_xw.disp,
				args,n);
	n = 0;
	XtSetArg(args[n],XmNgeometry,geom); n++;
	XtSetArg(args[n],XmNbackground,BlackPixel(uw_xw.disp,uw_xw.screen_no)); n++;
/*
...Open openGL own graphic Widget
...allow double buffer
*/
	uw_glopenwin();
/*
...Add the eventhandler to save the size and position when they change
...Yurong
*/
	XtAddEventHandler(uw_mf.graphic_app, StructureNotifyMask, False,
							(XtEventHandler)uw_GPS_resizeCB, (XtPointer)1);
	XtAddEventHandler(uw_mf.graphic_app, PointerMotionMask, False,
							(XtEventHandler)uw_mousemoveCB, (XtPointer)1);
/*
........Manage the Graphics area
*/
	XtManageChild(uw_mf.graphic);
	XtRealizeWidget(uw_mf.graphic_app);
	uw_xw.wd_id = XtWindow(uw_mf.graphic);
/*
........Set graphics color map
*/
/*
...Initalize for OPENGL rendering and set up fond, marker ...
*/
	XtRealizeWidget(uw_mf.parent);
	uw_glglxinit(size);
/*
.....Open the PROMPT window
*/
	n = 0;
/*
........Get rid of Motif window decorations and menu
*/
	ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
			MWM_DECOR_MINIMIZE | MWM_DECOR_TITLE | MWM_DECOR_RESIZEH;
	XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
/*
.....added for adjust pos and size
.....For SGI, we need adjust position
.....12/15/97 Yurong
*/
	pos[0] = UDM_layout.prompt_pos[0];
	pos[1] = UDM_layout.prompt_pos[1];
	size[0] = UDM_layout.prompt_size[0] - 2*XBorder[0];
	size[1] = UDM_layout.prompt_size[1] - 2*YBorder[0];
	if (Border_stat==0)
	{
		pos[0] += XBorder[0];
		pos[1] += YBorder[0];
	}
/*
........Set the window location
*/
/*	sprintf(geom,"%dx%d+%d+%d",
		UDM_layout.prompt_size[0],
		UDM_layout.prompt_size[1],
		UDM_layout.prompt_pos[0],
		UDM_layout.prompt_pos[1]); */
	sprintf(geom,"%dx%d+%d+%d",
				size[0], size[1], pos[0], pos[1]);
	XtSetArg(args[n],XmNgeometry,geom); n++;
/*
........Create the Prompt area
*/
	XtSetArg(args[n],XmNmappedWhenManaged,False); n++;
	uw_mf.prompt_app = XtAppCreateShell("Prompt","NCL",
		topLevelShellWidgetClass,
		uw_xw.disp,
		args,n);
/*
........Create Form to contain prompt lines
*/
	n = 0;
	XtSetArg(args[n],XmNfractionBase,24); n++;
	uw_mf.prompt = XtCreateWidget("prompts",
		xmFormWidgetClass,uw_mf.prompt_app,args,n);
	if (uw_mf.prompt == NULL)
	{
		printf("Could not create Prompting Window.\n");
		exit();
	}
/*
...........Create Top Prompt Label
.....Decreased the number of lines the Top Prompt
.....Label and Error Label takes up by one and increased 
.....tee Prompt Line by two. This take care of problems 
.....with the bottom portion of characters not being able
.....to be seen.  For example: gjqp, the stems of the letters
.....are not visible and the , looks like a . JLS 10/15/99
*/

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 7); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 24); n++;
	XtSetArg(args[n],XmNshadowType,XmSHADOW_ETCHED_IN); n++;
	frame = XtCreateManagedWidget(" ",xmFrameWidgetClass,
		uw_mf.prompt,args,n);
	n = 0;
	XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
	uw_mfprompt.prLabel = XtCreateManagedWidget(" ",xmLabelWidgetClass,
		frame,args,n);
/*
...........Create Prompt Label
*/
	n = 0;
	XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 7); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 17); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 3); n++;
	uw_mfprompt.prPlabel = XtCreateManagedWidget(" ",
		xmLabelWidgetClass,
		uw_mf.prompt,args,n);
/*
...........Create Prompt Line
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 7); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 17); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 3); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 24); n++;
	uw_mfprompt.prompt = XtCreateManagedWidget(" ",xmTextFieldWidgetClass,
		uw_mf.prompt,args,n);
	XtAddCallback(uw_mfprompt.prompt,XmNactivateCallback,uw_mfpromptCB,
		(XtPointer) NULL);
/*
...........Create Error Label
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 17); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 24); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 24); n++;
	XtSetArg(args[n],XmNshadowType,XmSHADOW_ETCHED_IN); n++;
	frame = XtCreateManagedWidget(" ",xmFrameWidgetClass,
		uw_mf.prompt,args,n);
	n = 0;
	XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
	uw_mfprompt.erLabel = XtCreateManagedWidget(" ",xmLabelWidgetClass,
		frame,args,n);
/*
...Add the eventhandler to save the size and position when they change
...Yurong
*/
	XtAddEventHandler(uw_mf.prompt_app, StructureNotifyMask, False,
							(XtEventHandler)uw_GPS_resizeCB, (XtPointer)2);
/*
........Manage the prompt area
*/
	XtManageChild(uw_mf.prompt);
	XtRealizeWidget(uw_mf.prompt_app);
/*
.....Open the STATUS window
*/
	uw_mfstat();
/*
....End of routine
*/
	uw_mf_xflush_expose();
	return(UU_SUCCESS);
}

/**********************************************************************
**    I_FUNCTION :  uw_mferror(msg)
**       Creates and displays the error message dialog.
**    PARAMETERS   
**       INPUT  : 
**          msg     = Error text to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Disables user interaction until the error
**				         message is acknowledged.
**    WARNINGS     : none
*********************************************************************/
uw_mferror(msg)
char *msg;
{
	int n;
	Arg args[20];
	Widget dlg;
	XmString lstr;
/*
.....if playback, we don''t accept user input, so
.....display error box will cause NCL in dead loop
.....when we restart follow the record file. at this 
.....time, parent of error box (uw_mf.graphic_app) unmanaged
.....user can't accept "OK" in error box, and because error
.....box is XmDIALOG_FULL_APPLICATION_MODAL, so every event
.....will stop
.....Yurong added  8/27/98
*/
	if (UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)
	{
		uw_mfprmerr(msg);
		return(UU_SUCCESS);
	}
/*
.....Bring up Error Dialog
*/
	n = 0;
	lstr = XmStringCreateSimple(msg);
	XtSetArg(args[n],XmNmessageString,lstr); n++;
/*
........Disable user interaction until
........Error message is acknowleged
*/
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
	XtSetArg(args[n],XmNtitle,"ERROR"); n++;
	dlg = (Widget) XmCreateErrorDialog(uw_mf.graphic_app,"Error",args,n);
	XtUnmanageChild((Widget)XmMessageBoxGetChild(dlg,XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild((Widget)XmMessageBoxGetChild(dlg,XmDIALOG_HELP_BUTTON));
/*	but = (Widget) XmMessageBoxGetChild(dlg,XmDIALOG_OK_BUTTON);*/
	XmStringFree(lstr);
	XtAddCallback(dlg,XmNokCallback,(XtCallbackProc)XtDestroyWidget,(XtPointer) NULL);
	XtManageChild(dlg);
/*
....Warp cursor to OK Button
*/
/*	n = 0;*/
/*	XtSetArg(args[n],XmNx,&x); n++;*/
/*	XtSetArg(args[n],XmNy,&y); n++;*/
/*	XtSetArg(args[n],XmNwidth,&w); n++;*/
/*	XtSetArg(args[n],XmNheight,&h); n++;*/
/*	XtGetValues(but,args,n);*/
/*	x = x + w/2 + XBorder[0];*/
/*	y = y + h/2 + YBorder[1];*/
/*	XWarpPointer(uw_xw.disp,None,XtWindow(dlg),0,0,0,0,x,y);*/
		return(UU_SUCCESS);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfgetprmerr(msg)
**       Returns the text from the prompt label field.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          msg     = Error line text.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfgetprmerr(msg)
char *msg;
{
	strcpy(msg,errtxt);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfgetprm(msg)
**       Returns the text from the prompt label field.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          msg     = Prompt text.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfgetprm(msg)
char *msg;
{
	strcpy(msg,prmtxt);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfgetprlabel(msg)
**       Returns the text from the prompt label field.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          msg     = Prompt label text.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfgetprlabel(msg)
char *msg;
{
	strcpy(msg,labtxt);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfprmerr(msg)
**       Displays a message on the error line of the Prompt area.
**    PARAMETERS   
**       INPUT  : 
**          msg     = Text to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfprmerr(msg)
char *msg;
{
	XmString labstr;
/*
.....Display the message
*/
	if (strcmp(errtxt,msg) != 0)
	{
		labstr = XmStringCreateSimple(msg);
		XtVaSetValues(uw_mfprompt.erLabel,XmNlabelString,labstr,NULL);
		XmStringFree(labstr);
		strcpy(errtxt,msg);
	}
	return(UU_SUCCESS);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfwrprm(msg)
**       Displays a message on the prompt line of the Prompt area.
**    PARAMETERS   
**       INPUT  : 
**          msg     = Text to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfwrprm(msg)
char *msg; 
{
	XmString labstr;
/*
.....Display the message
*/
/*#if UU_COMP == UU_WINNT*/
/*	if (strlen(msg) > 46) msg[46] = 0;*/
/*#endif*/
	if (strcmp(prmtxt,msg) != 0)
	{
		labstr = XmStringCreateSimple(msg);
		XtVaSetValues(uw_mfprompt.prLabel,XmNlabelString,labstr,NULL);
		XmStringFree(labstr);
		strcpy(prmtxt,msg);
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_mfwrprlabel(msg)
**       Displays a message on the user input line of the Prompt area.
**    PARAMETERS   
**       INPUT  : 
**          msg     = Text to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfwrplabel(msg)
char *msg; 
{
	XmString labstr;
/*
.....Display the message
*/
	if (strcmp(labtxt,msg) != 0)
	{
		labstr = XmStringCreateSimple(msg);
		XtVaSetValues(uw_mfprompt.prPlabel,XmNlabelString,labstr,NULL);
		XmStringFree(labstr);
		strcpy(labtxt,msg);
	}
}
/**********************************************************************
**    I_FUNCTION :  FileOkCB(widget,client_data,call_data)
**			Processes the OK button in the File Selection dialog.  Returns
**			the filename selected by the user.
**    PARAMETERS   
**       INPUT  : 
**          widget      = File Selection dialog shell.
**				client_data = Pointer to structure which will contain the
**				              selected filename.
**				call_data   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
static void FileOkCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	char *filen;
	FSELECT *p=(FSELECT *)client_data;
/*
.....Get the directory path and filename entered
*/
	XmFileSelectionBoxCallbackStruct *cbs =
	(XmFileSelectionBoxCallbackStruct *)call_data;
	if (!XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&filen))
	{
		p->file[0] = '\0';
	}
	else
	{
		strcpy(p->file,filen);
		XtFree(filen);
	}
   p->sel = UU_TRUE;
/*
.....Destroy the dialog
*/
/*
......we must use XtUnmanageChild to make dialog pop down
......Yurong 5/5/98
*/
	XtUnmanageChild(widget);
	XtDestroyWidget(widget);
	uw_mfflush();
/*
.....added for record/playback
.....Yurong 11/9/98
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
	{
		ud_rpwrbrowser("DATA", p->file);
		ud_rpwrbrowser("DONE", NULL);
	}
/*
.....reset formActive varibale
.....Yurong 11/9/98
*/
	formActive = p->save_formActive;
}

/**********************************************************************
**    I_FUNCTION :  MdFileOkCB(widget,client_data,call_data)
**			Processes the OK button in the File Selection dialog.  Returns
**			the filename selected by the user.
**			This call back for Menu design File browser
**    PARAMETERS   
**       INPUT  : 
**          widget      = File Selection dialog shell.
**				client_data = Pointer to structure which will contain the
**				              selected filename.
**				call_data   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
/*
.....added for Menu design browser 
*/
static void MdFileOkCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	char *filen, *dir;
	FSELECT *p=(FSELECT *)client_data;
	int i;
/*
.....Get the directory path and filename entered
*/
	XmFileSelectionBoxCallbackStruct *cbs =
	(XmFileSelectionBoxCallbackStruct *)call_data;
	if (!XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&filen))
	{
		p->file[0] = '\0';
	}
	else
	{
		strcpy(p->file,filen);
		XtFree(filen);
	}
   p->sel = UU_TRUE;
/*
.....Remember directory toggle choice
*/
	for (i=0;i<10;i++)
	{
		if (XmToggleButtonGetState(radio[i]))
		{
			radio_state = i;
			break;
		}
	}
/*
.....added for remember last directory for FileTree
.....for menu desgn file browse
.....12/17/97 Yurong
*/
	if (!XmStringGetLtoR(cbs->dir, XmSTRING_DEFAULT_CHARSET, &dir))
	{
		lastdir[radio_state][0] = '\0';
	}
	else
	{
		strcpy(lastdir[radio_state], dir);
		XtFree(dir);
	}
/*
.....Destroy the dialog
*/
	XtUnmanageChild(widget);
	XtDestroyWidget(widget);
	uw_mfflush();
/*
.....added for record/playback
.....Yurong 10/10/2001
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
	{
		ud_rpwrbrowser("DATA", p->file);
		ud_rpwrbrowser("DONE", NULL);
	}

}

/**********************************************************************
**    I_FUNCTION :  FileCancelCB(widget,client_data,call_data)
**			Processes the CANCEL button in the File Selection dialog.
**    PARAMETERS   
**       INPUT  : 
**          widget      = File Selection dialog shell.
**				client_data = Pointer to structure which will contain the
**				              blanked out filename.
**				call_data   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
static void FileCancelCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
/*
.....Set the filename to NULL
*/
	FSELECT *p = (FSELECT *)client_data;
   p->sel = UU_TRUE;
	p->file[0] = '\0';
/*
.....Destroy the dialog
*/
	XtDestroyWidget(widget);
/*
.....added for record/playback
.....Yurong 11/9/98
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
	{
		ud_rpwrbrowser("DONE", NULL);
	}
/*
.....reset formActive varibale
.....Yurong 11/9/98
*/
	formActive = p->save_formActive;
}

/**********************************************************************
**    I_FUNCTION :  uw_mfwait()
**       Dummy routine used to wait for user input while testing.  Not
**			called in production version.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Contains application main loop.
**    WARNINGS     : none
*********************************************************************/
void uw_mfwait()
{
	int n;
	Arg args[20];
	Widget pane,push;
	void exitCB();

	n = 0;
	pane = XmCreatePanedWindow(uw_mf.parent,"pane",args,n);
	n = 0;
	push = XmCreatePushButton(pane,"quit",args,n);
	XtAddCallback(push,XmNactivateCallback,(XtCallbackProc) exitCB,
		(XtPointer) NULL);
	XtManageChild(push);
	XtManageChild(pane);
	XtRealizeWidget(uw_mf.parent);
	XtAppMainLoop(uw_mf.application);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfget_filename(title,filter,filename,nc, descript,
**		                                open_flag)
**       Opens a File Selection dialog and returns the user selected
**			filename.
**    PARAMETERS   
**       INPUT  : 
**          title     = Title of File Selection dialog.
**				filter    = Filename filter to use for list of available
**				            files.
**				descript  = File type description. Not used on UNIX.
**				open_flag = This is not used by UNIX, just match the parameters
**				            with Windows NT.
**       OUTPUT :  
**          filename  = Name of selected file.
**				nc        = Number of chars in 'filename'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
...added parent Widget by Yurong
*/
uw_mfget_filename(parent, title,filter,filename,nc, descript,open_flag)
Widget parent;
char *title,*filter,*filename, *descript;
int *nc,open_flag;
{
	int i, n, sav_cur;
	void do_search();
	char ufilter[200];
	Arg args[20];
#if UU_COMP == UU_SUN
	Widget list;
#endif
	XmString labstr;
	XEvent event;
	FSELECT Fselect;
	Widget pkwin, uw_mfget_pkwin();
	int status ;
/*
.....replace '|' with ','
.....we allow different sets of file filter in WIN2K
.....(compiled use '|'
.....but for UNIX, it will all in one set
.....Yurong
*/
	for (i=0; i<strlen(filter); i++)
	{
		if (filter[i]=='|')
			ufilter[i] = ',';
		else
			ufilter[i] = filter[i];
	}
	ufilter[i] = '\0';
/*
.....save formActive because we use the varible for both 
.....browser & form, but sometimes, we need display/close
.....browser when form displayed, when browser close, need
.....reset this varible 
.....Yurong 11/9/98
*/
	Fselect.save_formActive = formActive;
/*
.....added for record/playback
.....Yurong 11/9/98
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
		ud_rpwrbrowser("BEGIN", NULL);
/*
.....check if Playback is active
.....Yurong 11/9/98
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)
	{
		status = uw_mfbrowse_playb(filename);
		if (status==0)
		{
/*
.....don't display browser
*/
			if (filename[0] == '\0')
				*nc = 0;
			else
				*nc = strlen(filename);
			return(UU_SUCCESS);
		}
/*
.....else need display filename as default filename
.....so continue;
*/
	}
/*
.....Create file selection dialog
*/
	n = 0;
	labstr = XmStringCreateSimple(ufilter);
	XtSetArg(args[n],XmNdirMask,labstr); n++;
	XtSetArg(args[n],XmNfileSearchProc,do_search); n++;
/*
.....now use formActive to disable menu functions
.....Yurong changed 11/9/98
.....
.....Changed back, since you could cancel the form without
.....cancelling the Browser window, which would cause
.....NCL to hang
.....Bobby  -  1/20/00
*/
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
	if(parent==UU_NULL)
	{
		pkwin = (Widget) uw_mfget_pkwin();
		if (pkwin==NULL)
		{
			mine = (Widget) XmCreateFileSelectionDialog
					(uw_mf.graphic_app,"file_select",args,n);
		}
		else
		{
			mine = (Widget) XmCreateFileSelectionDialog
					(pkwin,"file_select",args,n);
		}
	}
	else
	{
		mine = (Widget) XmCreateFileSelectionDialog
			(parent,"file_select",args,n);
	}
	XtVaSetValues(XtParent(mine),XmNtitle,title,NULL);
	XmStringFree(labstr);
/*
.....let scroll bar always displayed
.....for fixed prolems on SunOS pluto 5.5.1
.....(there is no scroll bar show on the
.....file select list
.....Yurong 11/14/97
*/
#if UU_COMP == UU_SUN
	list = (Widget)XmFileSelectionBoxGetChild(mine, XmDIALOG_LIST);
	XtVaSetValues(list, XmNscrollBarDisplayPolicy , XmSTATIC,
				NULL);
#endif
/*
.....Get rid of the HELP button
*/
	XtUnmanageChild((Widget)XmFileSelectionBoxGetChild(mine,XmDIALOG_HELP_BUTTON));
/*
.....Add default filename
*/
	if (filename[0]!='\0')
	{
		labstr = XmStringCreateSimple(filename);
		XtVaSetValues(mine, XmNdirSpec, labstr, NULL);
		XmStringFree(labstr);
	}
/*
.....Add Callbacks
*/
	Fselect.file = filename;
	XtAddCallback(mine,XmNcancelCallback,FileCancelCB,&Fselect);
	XtAddCallback(mine,XmNokCallback,FileOkCB,&Fselect);
/*
.....Manage the File Selection Dialog
*/
	XtManageChild(mine);
/*
.....save cursor
*/
	sav_cur = uw_mfget_cursor();
	XWarpPointer(uw_xw.disp,None,XtWindow(mine),0,0,0,0,XBorder[0]+5,YBorder[1]+5);
/*
.....Loop until user selects a file
*/
	Fselect.sel = UU_FALSE;
	Fselect.sel = False;
	formActive = UU_TRUE;		
	do
	{
		uw_mfsetcursor(1);
		XtAppNextEvent(uw_mf.application,&event);
		XtDispatchEvent(&event);
	} while (!Fselect.sel);
/*
.....Return filename
*/
	*nc = strlen(Fselect.file);
/*
.....reset cursor to wait
.....Yurong 9/14/98
*/
	uw_mfsetcursor(sav_cur);
done:;
	return(UU_SUCCESS);
}
/**********************************************************************
**    I_FUNCTION :  uw_mfreset_dirCB(widget,client_data,call_data)
**			Processes the radio button in the File Selection dialog. 
**			Change the file selection set acording to choice.
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored
**				client_data = Pointer to radio button choice
**				call_data   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : redisplay the File Selection dialog 
**    WARNINGS     : none
*********************************************************************/
void uw_mfreset_dirCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	UX_pathname old_dir,deffile,direc,fullname;
	XmString labstr;
	int stat,mode, file_status;
	char *text;
	char* choice = (char*) client_data;

	if (!XmToggleButtonGetState(widget)) return;
	ul_get_full_dir(choice,direc);
	mode = 0;
	stat = ux_file_inquire(UU_NULL,direc,UU_NULL,UU_NULL,UU_NULL,&mode,
		&file_status,direc,UX_NQUOTES|UX_NPRTERRS);
	ul_remove_quotes (direc);
	if (!(stat==UU_SUCCESS && mode == (mode|UX_EXISTS|UX_FAREA)))
		ul_get_full_dir ("HOMEDIR", direc);
/*
.....get filename from file selection box
.....in order to set default filename later
.....2/9/1998 Yurong
*/
	text = (char*)XmTextGetString(XmFileSelectionBoxGetChild(mine, XmDIALOG_TEXT));
	mode = 0;
	stat = ux_file_inquire(UU_NULL,text,UU_NULL,UU_NULL,UU_NULL,&mode,
		&file_status,text,UX_NQUOTES|UX_NPRTERRS);
	ul_remove_quotes (text);
/*
.....if the whole string is a directory
*/
	if (stat==UU_SUCCESS && mode == (mode|UX_EXISTS|UX_FAREA))
		deffile[0] = '\0';
/*
.....if the whole string is not a directory
.....it is a full path file name
*/
	else
		ul_break_fname(text, old_dir,deffile);
	XtFree(text);
/*
.....call do search to redisplay files  in different directory
*/
	ux_cat_paths(direc,def_filter,fullname,UX_NPRTERRS|UX_NCHK|UX_NQUOTES);
	ul_remove_quotes(direc);
	labstr = XmStringCreateSimple(fullname);
	XmFileSelectionDoSearch(mine, labstr); 
	XmStringFree(labstr);
/*
.....added for default filename
*/
	ux_cat_paths(direc, deffile, fullname,UX_NPRTERRS|UX_NCHK|UX_NQUOTES);
   labstr = XmStringCreateSimple(fullname);
   XtVaSetValues(mine, XmNdirSpec, labstr, NULL);
   XmStringFree(labstr);
}
		
/**********************************************************************
**    I_FUNCTION :  uw_mfmd_filename(parent,title,filter,nfld,ftext,fdir,
**		                               filename,nc,descrip)
**       Opens a File Selection dialog and returns the user selected
**			filename. This function is similar to uw_mfget_filename
**			but add radio box for select directory
**    PARAMETERS   
**       INPUT  : 
**          title     = Title of File Selection dialog.
**				filter    = Filename filter to use for list of available
**				            files.
**				nfld      = Number of directory toggle fields.
**				ifld      = Default directory toggle field.
**				ftext     = Text for directory toggle fields.
**				fdir      = Actual directories for toggle fields.
**				filename  = Default filename.
**			descript = File type description. Not used on UNIX.
**       OUTPUT :  
**          filename  = Name of selected file.
**				nc        = Number of chars in 'filename'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfmd_filename(parent,title,filter,nfld,ifld,ftext,fdir,filename,nc,descrip)
Widget parent;
char *title,*filter,*filename,*descrip;
char **ftext,**fdir;
int *nc,nfld,*ifld;
{
	int n, sav_cur,i;
	char ufilter[200];
	void do_search();
	Arg args[20];
	Widget board;
#if UU_COMP == UU_SUN
	Widget list;
#endif
	XmString defname;
	XEvent event;
	FSELECT Fselect;
	Widget pkwin, uw_mfget_pkwin();
/*
.....replace '|' with ','
.....we allow different sets of file filter in WIN2K
.....(compiled use '|'
.....but for UNIX, it will all in one set
.....Yurong
*/
	for (i=0; i<strlen(filter); i++)
	{
		if (filter[i]=='|')
			ufilter[i] = ',';
		else
			ufilter[i] = filter[i];
	}
	ufilter[i] = '\0';
/*
.....Create file selection dialog
*/
	n = 0;
	XtSetArg(args[n],XmNfileSearchProc,do_search); n++;
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
	strcpy(def_filter,ufilter);
/*
.....set default directory to System Menu
*/
	if(parent==UU_NULL)
	{
		pkwin = (Widget) uw_mfget_pkwin();
		if (pkwin==NULL)
		{
			mine = (Widget) XmCreateFileSelectionDialog
					(uw_mf.graphic_app,"file_select",args,n);
		}
		else
		{
			mine = (Widget) XmCreateFileSelectionDialog
					(pkwin,"file_select",args,n);
		}
	}
	else
	{
		mine = (Widget) XmCreateFileSelectionDialog
			(parent,"file_select",args,n);
	}
	XtVaSetValues(XtParent(mine),XmNtitle,title, NULL);
/*
.....let scroll bar always displayed
.....for fixed prolems on SunOS pluto 5.5.1
.....(there is no scroll bar show on the
.....file select list
.....Yurong 11/14/97
*/
#if UU_COMP == UU_SUN
	list = (Widget)XmFileSelectionBoxGetChild(mine, XmDIALOG_LIST);
	XtVaSetValues(list, XmNscrollBarDisplayPolicy , XmSTATIC,
				NULL);
#endif
	
/*
.....Get rid of the HELP button
*/
	XtUnmanageChild((Widget)XmFileSelectionBoxGetChild(mine,XmDIALOG_HELP_BUTTON));
/*
.....Add Callbacks
*/
	Fselect.file = filename;
	Fselect.save_formActive = formActive;
	XtAddCallback(mine,XmNcancelCallback,FileCancelCB,&Fselect);
	XtAddCallback(mine,XmNokCallback,MdFileOkCB,&Fselect);
/*
.....Create Directory radio box
*/
	board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass, mine,
								XmNorientation,XmHORIZONTAL,
								XmNradioBehavior, True,
								XmNradioAlwaysOne, True, NULL);
	for (i=0;i<nfld;i++)
	{
		radio[i] = XtCreateManagedWidget(ftext[i], xmToggleButtonWidgetClass,
			board,NULL,0);
		XtAddCallback(radio[i], XmNvalueChangedCallback, uw_mfreset_dirCB, fdir[i]);
		lastdir[i] = fdir[i];
	}

	if (*ifld > nfld-1) *ifld = nfld-1;
	radio_state = *ifld;
	XmToggleButtonSetState(radio[radio_state],True, TRUE); 
/*
......change for added default filename
.....Yurong 2/4/98
*/
	if (filename[0]!='\0')
	{
		defname = XmStringCreateSimple(filename);
		XtVaSetValues(mine, XmNdirSpec, defname, NULL);
		XmStringFree(defname);
	}
/*
.....Manage the File Selection Dialog
*/
	XtManageChild(board);
	XtManageChild(mine);
/*
.....save cursor
*/
	sav_cur = uw_mfget_cursor();
	XWarpPointer(uw_xw.disp,None,XtWindow(mine),0,0,0,0,XBorder[0]+5,YBorder[1]+5);
/*
.....Loop until user selects a file
*/
	Fselect.sel = UU_FALSE;
	do
	{
		uw_mfsetcursor(1);
		XtAppNextEvent(uw_mf.application,&event);
		XtDispatchEvent(&event);
	} while (!Fselect.sel);
/*
.....Return filename
*/
	*nc = strlen(Fselect.file);
	*ifld = radio_state;
/*
.....reset cursor 
.....Yurong 9/14/98
*/
	uw_mfsetcursor(sav_cur);
done:;
	return(UU_SUCCESS);
}

/**********************************************************************
**    I_FUNCTION : yes_or_noCB()
**       action for yes or no choice
**    PARAMETERS
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Motif callback structure.  Contains
**                        answer selected by the user.
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....function added by Yurong
.....8/20/97
*/
static void yes_or_noCB(w, client_data, call_data)
Widget w;
XtPointer client_data,call_data;
{
	XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)call_data;
	switch(cbs->reason)
	{
		case XmCR_OK:
			yes_or_no = 1;
			XtPopdown(XtParent(w));
			break;
		case XmCR_CANCEL:
			yes_or_no = 0;
			XtPopdown(XtParent(w));
			break;
	}
	uw_mfanswer = 1;
}

/**********************************************************************
**    I_FUNCTION : uw_mfyes_or_no(fname)
**			popup a dialog box for yes or no choice
**    PARAMETERS   
**       INPUT  : fname: filename that need be insured
**       OUTPUT :  
**         			none 
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
/*
.....function added by Yurong
.....8/20/97
*/
uw_mfyes_or_no(parent, msg, title)
Widget parent;
char *msg, *title;
{
	Widget dialog;
	XEvent x_event;
	Arg args[5];
	XmString m;
	int n  = 0;
	XmString yes = XmStringCreateSimple("Yes");
	XmString no = XmStringCreateSimple("No");
/*
.....default answer to "No"
*/
	yes_or_no = 0;
/*
...create a yesno dialog
*/
	m = XmStringCreateSimple(msg);
	XtSetArg(args[n], XmNautoUnmanage, False); n++;
	XtSetArg(args[n], XmNmessageString, m); n++;
	XtSetArg(args[n], XmNokLabelString, yes); n++;
	XtSetArg(args[n], XmNcancelLabelString, no); n++;
	if(parent==UU_NULL)
	{
		dialog = (Widget) XmCreateQuestionDialog(uw_mf.graphic_app, 
											title, args, n);
	}
	else
	{
		dialog = (Widget) XmCreateQuestionDialog(parent, 
											title, args, n);
	}
	XtVaSetValues(XtParent(dialog),XmNtitle,title,NULL);
/*
...callback
*/
	XtAddCallback(dialog, XmNokCallback, yes_or_noCB, (XtPointer)NULL);
	XtAddCallback(dialog, XmNcancelCallback, yes_or_noCB, (XtPointer)NULL);
	XmStringFree(m);
	XmStringFree(no);
	XmStringFree(yes);
/*
...no help used
*/
	XtUnmanageChild((Widget)XmMessageBoxGetChild((Widget)dialog,
									XmDIALOG_HELP_BUTTON)); 
	XtManageChild(dialog);
	XtPopup(XtParent(dialog), XtGrabNone);
	uw_mfanswer = 0;
	while (!uw_mfanswer)
	{
		XtAppNextEvent(uw_mf.application,&x_event);
		XtDispatchEvent(&x_event);
	}
	return yes_or_no;
}

/**********************************************************************
**    I_FUNCTION : uw_mfbrowse_playb(filename);
**       Read the record file to get browser's data
**    PARAMETERS
**       INPUT  :
**          filename:
**       OUTPUT :
**          filename
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_mfbrowse_playb(filename)
char *filename;
{
	int status;
	status = ud_rdbrowser(filename);
	return status;
}

/**********************************************************************
**    I_FUNCTION : uw_mfget_prompt(parent, title, msg, ln, cols, ans_str)
**			popup a prompt dialog box 
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent widget
**				title:  prompt box title
**				msg:    prompt str.
**				str_ans: user input in text field.
**       OUTPUT :  
**				str_ans: user input in text field.
**    RETURNS      : 1: if accepted
**							-1: if canceled
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
uw_mfget_prompt(parent, title, msg, ans_str)
Widget parent;
char *msg, *title, *ans_str;
{
	XEvent event;
	XmString t, t1;
	Arg args[5];
	Widget dialog;
	UW_PROMPT prompt;
	int n ;
	t = XmStringCreateSimple(msg);
	t1 = XmStringCreateSimple(ans_str);
/*
...Create the dialog
*/
	n = 0;
	XtSetArg(args[n], XmNselectionLabelString, t); n++;
	XtSetArg(args[n], XmNtextString, t1); n++; 
	dialog = (Widget)XmCreatePromptDialog(parent, "Label", args, n);
	XtVaSetValues(XtParent(dialog),XmNtitle, title, NULL);
	XmStringFree(t);
	XmStringFree(t1);

	prompt.ans = ans_str;
	prompt.sel = 0;
	XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)uw_mfprompt_okCB, &prompt);
	XtAddCallback(dialog, XmNcancelCallback, (XtCallbackProc)uw_mfprompt_cancelCB, &prompt);
/*
...no help used
*/
	XtUnmanageChild((Widget)XmSelectionBoxGetChild((Widget)dialog,XmDIALOG_HELP_BUTTON)); 
	XtManageChild(dialog);
	XtPopup(XtParent(dialog), XtGrabNone);
	
	do
	{
		XtAppNextEvent(uw_mf.application,&event);
		XtDispatchEvent(&event);
	} while (prompt.sel==0);

	return prompt.sel;
}

/**********************************************************************
**    I_FUNCTION : uw_mfprompt_okCB(widget, client_data, call_data) 
**       Processes the OK button in the prompt dialog.
**       return the string input by the user.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
static void uw_mfprompt_okCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	char *label;
	UW_PROMPT *p = (UW_PROMPT *)client_data;
	XmSelectionBoxCallbackStruct *cbs =
		(XmSelectionBoxCallbackStruct *)call_data;

	if (XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&label))
	{
		if(label[0]!='\0')
			strcpy(p->ans, label);
		else
			p->ans[0] = '\0';
	}
	p->sel = 1;
	XtDestroyWidget(widget);
}	

/**********************************************************************
**    I_FUNCTION : uw_mfprompt_cancelCB(widget, client_data, call_data)
**       Processes the CANCEL button in the prompt dialog.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
void uw_mfprompt_cancelCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	UW_PROMPT *p = (UW_PROMPT *)client_data;
	p->sel = -1;
	XtDestroyWidget(widget);
}

/**********************************************************************
**    I_FUNCTION : uw_mfget_nprompt(parent, title, num, msg, ans_str)
**			popup a multiple text prompt dialog box 
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent widget
**				title:  prompt box title
**				num:      number of prompt (no more than 10)
**				msg:    prompt strings.
**				str_ans: user inputs in text field.
**       OUTPUT :  
**				str_ans: user inputs in text field.
**    RETURNS      : 1: if accepted
**							-1: if canceled
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
uw_mfget_nprompt(parent, title, num, msg, ans_str)
Widget parent;
int num;
char **msg, *title, **ans_str;
{
	XEvent event;
	int i;
	Arg args[20];
	Widget dialog, wok, wcancel;
	char buf[20];
	UW_NPROMPT prompt;
	int n ;

	if (num>10)
		num = 10;
/*
...Create the dialog
*/
	n = 0;
	XtSetArg(args[n], XmNdialogStyle, XmDIALOG_APPLICATION_MODAL); n++;
	
	dialog = (Widget) XmCreateFormDialog(
					parent, "FormDialog", args,n);
	XtVaSetValues(dialog, XtVaTypedArg, XmNdialogTitle, XmRString,
						title, strlen(title) + 1, NULL);

	for (i=0; i<num; i++)
	{
		n = 0;
		XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNtopPosition, ((double)2*i/(2*num+1.1))*100); n++;
		XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNbottomPosition, ((double)(2*i+1)/(2*num+1.1))*100); n++;
		XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
		XtCreateManagedWidget(msg[i], xmLabelWidgetClass,
									dialog, args,n);
		n = 0;
		XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNtopPosition, ((double)(2*i+1)/(2*num+1.1))*100); n++;
		XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNleftPosition, 5); n++;
		sprintf(buf, "prompt_text%d", i+1);
		prompt_txt[i] = XtCreateManagedWidget(buf, xmTextWidgetClass,
								dialog, args,n);
		XmTextSetString(prompt_txt[i], ans_str[i]);
	}
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, ((double)2*num/(2*num+1.1))*100); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	XtCreateManagedWidget("sep", xmSeparatorWidgetClass,
					dialog, args,n);
	
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, ((double)2*num/(2*num+1.1))*100 +4); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 97); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 10);  n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNrightPosition, 45);  n++;
	wok = XtCreateManagedWidget("Ok", xmPushButtonWidgetClass,
					dialog, args,n);
	
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, ((double)2*num/(2*num+1.1))*100 +4); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 97); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 55);  n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNrightPosition, 90);  n++;
	wcancel = XtCreateManagedWidget("Cancel", xmPushButtonWidgetClass,
					dialog, args,n);

	prompt.ans = ans_str;
	prompt.sel = 0;
	prompt.n = num;

	XtAddCallback(wok,
					XmNactivateCallback, uw_mfnprompt_okCB, &prompt);
	XtAddCallback(wcancel,
					XmNactivateCallback, uw_mfnprompt_cancelCB, &prompt);
					
	XtManageChild(dialog);
	
	do
	{
		XtAppNextEvent(uw_mf.application,&event);
		XtDispatchEvent(&event);
	} while (prompt.sel==0);

	return prompt.sel;
}


/**********************************************************************
**    I_FUNCTION : uw_mfnprompt_okCB(widget, client_data, call_data) 
**       Processes the OK button in the multiple prompt dialog.
**       return the string input by the user.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the multiple prompt dialog shell.
**    WARNINGS     : none
*********************************************************************/
static void uw_mfnprompt_okCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int i;
	char *label;
	UW_NPROMPT *p = (UW_NPROMPT *)client_data;
	
	for (i=0; i<p->n; i++)
	{
		label = XmTextGetString(prompt_txt[i]);
		if (label)
		{
			strcpy(p->ans[i], label);	
			XtFree(label); 
		}
		else
			p->ans[i][0] = '\0';
	}
	p->sel = 1;
	XtDestroyWidget(widget);
}	

/**********************************************************************
**    I_FUNCTION : uw_mfnprompt_cancelCB(widget, client_data, call_data)
**       Processes the CANCEL button in the multiple prompt dialog.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the multiple prompt dialog shell.
**    WARNINGS     : none
*********************************************************************/
void uw_mfnprompt_cancelCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	UW_NPROMPT *p = (UW_NPROMPT *)client_data;
	p->sel = -1;
	XtDestroyWidget(widget);
}

/**********************************************************************
**    I_FUNCTION : uw_mfreset_prompt()
**       Disables the command line text field and erases all prompt
**			lines.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfreset_prompt()
{
	char *nstr="";
	XmTextSetString(uw_mfprompt.prompt,nstr);
	XtSetSensitive(uw_mfprompt.prompt,False);
	uw_mfwrplabel(nstr);
	uw_mfwrprm(nstr);
	uw_mfprmerr(nstr);
}

/**********************************************************************
**    I_FUNCTION :  do_dir_search (widget,sdata)
**       Creates the FileName list for the File Selection Widget using
**			the specialized file mask (file1,file2,...,filen).
**    PARAMETERS   
**       INPUT  : 
**          widget = File selection widget.
**				sdata  = Motif callback structure.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void do_dirsearch(widget, sdata)
Widget widget;
XtPointer sdata;
{
	int nfile,i;
	char *dir,*pattern;
	UX_pathname direc,file,fname;
	char *list,*listhead;
	XmString names[1024];
	XmFileSelectionBoxCallbackStruct *cbs = 
		(XmFileSelectionBoxCallbackStruct *)sdata;
/*
.....Make sure something is in the directory & filename
*/
	if (!XmStringGetLtoR(cbs->dir,XmSTRING_DEFAULT_CHARSET,&dir))
		return;
	strcpy(direc,dir);

	XtFree(dir);
	if (!XmStringGetLtoR(cbs->pattern,XmSTRING_DEFAULT_CHARSET,&pattern))
		return;
	strcpy(file,pattern);
	XtFree(pattern);
/*
.....Get list of filenames
*/
	ul_get_flist(direc,file,&list,0,(UX_NPRTERRS|UX_NCHK));
	listhead = list;
/*
.....No files in list
*/
	nfile = 0;
	if (list == NULL)
	{
		XtVaSetValues(widget,
			XmNfileListItems, NULL,
			XmNfileListItemCount, nfile,
			XmNlistUpdated, True,
			NULL);
	}
/*
.....Store files in File Selection List
*/
	else
	{
		while (ux_nxt_file(&list,fname,UX_NPRTERRS) == UU_SUCCESS)
		{
#if UU_COMP == UU_VAXVMS
			strcpy(file,fname);
			nc = strlen(file); 
			ul_strip_blanks(file,&nc);
#else
			strcpy(file,direc); 
			strcat(file,fname);
#endif
			if (ux_is_farea(file, UX_CHK)==UU_SUCCESS)
				names[nfile++] = XmStringCreateSimple(file);
		}
		XtVaSetValues(widget,
			XmNfileListItems, names,
			XmNfileListItemCount, nfile,
			XmNdirSpec, cbs->dir,
		XmNlistUpdated, True,
			NULL);
		if (listhead != NULL) uu_lsdel(listhead);
		for (i=0;i<nfile;i++) XmStringFree(names[i]);
	}
}
/**********************************************************************
**    I_FUNCTION :  uw_mfget_dirname(title,dir,nc)
**       Opens a File Selection dialog and returns the user selected
**			filename.
**    PARAMETERS   
**       INPUT  : 
**				       
**       OUTPUT :  
**          filename  = Name of selected file.
**				nc        = Number of chars in 'filename'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfget_dirname(parent, title, filename,nc)
Widget parent;
char *title,*filename;
int *nc;
{
	int n, sav_cur;
	Arg args[20];
#if UU_COMP == UU_SUN
	Widget list;
#endif
/*	XmString labstr;*/
	XEvent event;
	FSELECT Fselect;
	Widget pkwin, uw_mfget_pkwin();
	int status ;
/*
.....save formActive because we use the varible for both 
.....browser & form, but sometimes, we need display/close
.....browser when form displayed, when browser close, need
.....reset this varible 
.....Yurong 11/9/98
*/
	Fselect.save_formActive = formActive;
/*
.....added for record/playback
.....Yurong 11/9/98
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
		ud_rpwrbrowser("BEGIN", NULL);
/*
.....check if Playback is active
.....Yurong 11/9/98
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)
	{
		status = uw_mfbrowse_playb(filename);
		if (status==0)
		{
/*
.....don't display browser
*/
			if (filename[0] == '\0')
				*nc = 0;
			else
				*nc = strlen(filename);
			return(UU_SUCCESS);
		}
/*
.....else need display filename as default filename
.....so continue;
*/
	}
/*
.....Create file selection dialog
*/
	n = 0;
	XtSetArg(args[n],XmNfileTypeMask, XmFILE_DIRECTORY); n++;
	XtSetArg(args[n],XmNfileSearchProc,do_dirsearch); n++;
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
	if(parent==UU_NULL)
	{
		pkwin = (Widget) uw_mfget_pkwin();
		if (pkwin==NULL)
		{
			mine = (Widget) XmCreateFileSelectionDialog
					(uw_mf.graphic_app,"file_select",args,n);
		}
		else
		{
			mine = (Widget) XmCreateFileSelectionDialog
					(pkwin,"file_select",args,n);
		}
	}
	else
	{
		mine = (Widget) XmCreateFileSelectionDialog
			(parent,"file_select",args,n);
	}
	XtVaSetValues(XtParent(mine),XmNtitle,title,NULL);
/*
.....let scroll bar always displayed
.....for fixed prolems on SunOS pluto 5.5.1
.....(there is no scroll bar show on the
.....file select list
.....Yurong 11/14/97
*/
#if UU_COMP == UU_SUN
	list = (Widget)XmFileSelectionBoxGetChild(mine, XmDIALOG_LIST);
	XtVaSetValues(list, XmNscrollBarDisplayPolicy , XmSTATIC,
				NULL);
#endif
/*
.....Get rid of the HELP button
*/
	XtUnmanageChild((Widget)XmFileSelectionBoxGetChild(mine,XmDIALOG_HELP_BUTTON));
/*
.....Add default filename
*/
/*	if (filename[0]!='\0')
	{
		labstr = XmStringCreateSimple(filename);
		XtVaSetValues(mine, XmNdirSpec, labstr, NULL);
		XmStringFree(labstr);
	}
*/
/*
.....Add Callbacks
*/
	Fselect.file = filename;
	XtAddCallback(mine,XmNcancelCallback,FileCancelCB,&Fselect);
	XtAddCallback(mine,XmNokCallback,FileOkCB,&Fselect);
/*
.....Manage the File Selection Dialog
*/
	XtManageChild(mine);
/*
.....save cursor
*/
	sav_cur = uw_mfget_cursor();
	XWarpPointer(uw_xw.disp,None,XtWindow(mine),0,0,0,0,XBorder[0]+5,YBorder[1]+5);
/*
.....Loop until user selects a file
*/
	Fselect.sel = UU_FALSE;
	Fselect.sel = False;
	formActive = UU_TRUE;		
	do
	{
		uw_mfsetcursor(1);
		XtAppNextEvent(uw_mf.application,&event);
		XtDispatchEvent(&event);
	} while (!Fselect.sel);
/*
.....Return filename
*/
	*nc = strlen(Fselect.file);
/*
.....reset cursor to wait
.....Yurong 9/14/98
*/
	uw_mfsetcursor(sav_cur);
done:;
	return(UU_SUCCESS);
}
/**********************************************************************
**    I_FUNCTION :  uw_mfget_gwsize(int *wid, int *hgt);
**       Get the NCL Graphic window size
**    PARAMETERS   
**       INPUT  : 
**          None
**       OUTPUT :  
**          wid, hgt: size of the graphic window
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfget_gwsize(wid, hgt)
int *wid, *hgt;
{
	Dimension width, height;
	XtVaGetValues(uw_mf.graphic,XmNwidth, &width, XmNheight, &height, NULL); 
	*wid = width;
	*hgt = height;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfchk_comm
**       check if the current locate common area enough to run
**    PARAMETERS   
**       INPUT  : 
**          None
**       OUTPUT :  
**          None
**    RETURNS      : 0: OK; -1: not OK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_mfchk_comm()
{
	if (NCQ_common_open==0)
		return 0;
	if (NCQ_comm_pos+3*sizeof (int) + 21 * sizeof (char)>NCL_com_size)
		return -1;
	else
		return 0;
}
/***********************************************************************
**
**   SUBROUTINE: uw_write_commsg(int *jobno, int *err, int *warn, char *ldate, char *ltim)
**
**   FUNCTION:  This routine write NCL pp file execution
**						information into common memory area
**
**   INPUT:  jobno: job number executable now
**				 err: error executable the job
**					warn: warnings executable the job
**			ldate, ltim: date and time	
**				 
**   OUTPUT:  none
**
***********************************************************************/
int uw_write_commsg(jobno, err, warn, ldat, ltim)
int jobno, err, warn;
char *ldat, *ltim;
{
	char *temp;
	int i;
	if (NCQ_common_open==0)
		return 0;
/*
......the first word is for NCL process ID,
......the second part is NCLInfo for NCL running data,
......read from after NCLInfo
*/
	if (NCQ_comm_pos==0)
		NCQ_comm_pos = sizeof (int) + sizeof (NCLInfo);
	if (NCQ_comm_pos+3*sizeof (int) + 21 * sizeof (char)>NCL_com_size)
		return -1;
	temp = (char *)&jobno;
	for (i=0; i<sizeof (int); i++)
		NCQ_comptr[NCQ_comm_pos+i] = temp[i];
	NCQ_comm_pos = NCQ_comm_pos + sizeof(int);

	temp = (char *)&err;
	for (i=0; i<sizeof (int); i++)
		NCQ_comptr[NCQ_comm_pos+i] = temp[i];
	NCQ_comm_pos = NCQ_comm_pos + sizeof(int);

	temp = (char *)&warn;
	for (i=0; i<sizeof (int); i++)
		NCQ_comptr[NCQ_comm_pos+i] = temp[i];
	NCQ_comm_pos = NCQ_comm_pos + sizeof(int);

	temp = (char *)ldat;
	for (i=0; i<12; i++)
		NCQ_comptr[NCQ_comm_pos+i] = temp[i];
	NCQ_comm_pos = NCQ_comm_pos + 12;

	temp = (char *)ltim;
	for (i=0; i<9; i++)
		NCQ_comptr[NCQ_comm_pos+i] = temp[i];
	NCQ_comm_pos = NCQ_comm_pos + 9;
}

/***********************************************************************
**
**   SUBROUTINE: uw_write_comblk(NCLInfo *info)
**
**   FUNCTION:  This routine write NCL information into common memory area
**
**   INPUT:  info:         NCLInfo structure contain NCL information
**
**   OUTPUT: none
**
***********************************************************************/
void uw_write_comblk(info)
NCLInfo *info;
{
	int i;
	int pos, isize;
	char *temp;
/*
	char *temp = (char *)info;
*/
	if (NCQ_common_open)
	{
		isize = sizeof (int);
/*
......the first word is for NCL process ID, read from after first word
*/
		pos = isize;
		temp = (char *)&(info->flag);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + isize;

		temp = (char *)&(info->ppfile);
		for (i=0; i<256; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + 256;

		temp = (char *)&(info->current);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + isize;

		temp = (char *)&(info->highest);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + isize;

		temp = (char *)&(info->lines);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + isize;

		temp = (char *)&(info->macro);
		for (i=0; i<8; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + 8;

		temp = (char *)&(info->warn);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + isize;

		temp = (char *)&(info->error);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
	}
}				

/*********************************************************************
**    E_FUNCTION     : uw_write_comint(int *data, int pos)
**       write a integer data into common memory area at 'pos'
**    PARAMETERS
**       INPUT  :
**             data: integer to be wrote
**				pos: position to put data 
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_write_comint(data,pos)
int *data,pos;
{
	int i;
	char *temp = (char *)data;
	if (NCQ_common_open==0)
		return;
	for (i=pos; i<sizeof(int); i++)
		NCQ_comptr[i] = temp[i];
}

/*********************************************************************
**    E_FUNCTION     : uw_mfsend_NCLinfo()
**       Send a update NCL info to NCQ
**    PARAMETERS
**       INPUT  :
**             none 
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfsend_NCLinfo()
{
	NCLInfo info;
	if (Ncq_ncl_id<=0)
		return;
	if (NCQ_running)
	{
		ud_getbat_info(&info);
		info.flag = 3;
		uw_write_comblk(&info);
	}
}

/*********************************************************************
**    E_FUNCTION     : uw_mfstore_batchmsg(jobno, errno, warno)
**       Store the NCL batch message into common memory area
**    PARAMETERS
**       INPUT  :
**              jobno: batch file number
**          errno: error number
**          warno: warning number
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_mfstore_batchmsg(jobno, err, warn, ldat, ltim)
int jobno, err, warn;
char *ldat, *ltim;
{
	if (Ncq_ncl_id<=0)
		return;
	if (NCQ_running)
	{
		return uw_write_commsg(jobno, err, warn, ldat, ltim);
	}
}
/*********************************************************************
**    E_FUNCTION     : uw_mfupdate_ncq(flag)
**       Send NCL updating message to NCQ
**    PARAMETERS
**       INPUT  :
**          flag: 0: NCL exit message
**                1: NCL update message
**                2: Send NCL process ID message
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfupdate_ncq(flag)
int flag;
{
	NCLInfo info;
	int nclid;

	if (Ncq_ncl_id<=0)
		return;
	
	if (NCQ_running)
	{
/*
.....flag=1 NCL update flag
*/
		info.flag = 1;
		if (flag == 0)
		{
			strcpy(info.ppfile, "NCL EXIT");
			uw_write_comblk(&info);
		}
		else if (flag==1)
		{
			strcpy(info.ppfile,"NCL UPDATE");
			uw_write_comblk(&info);
		}
		else if (flag==2)
		{
			nclid = getpid();
			uw_write_comint(&nclid, 0);
		}
		else
			return;
	}
}

/*********************************************************************
**    E_FUNCTION     : uw_get_quenum()
**       get ncl que pp file number
**    PARAMETERS
**       INPUT  :
**             none
**       OUTPUT :
**          none
**    RETURNS      :
**          pp file number in the queue
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_get_quenum()
{
	FILE *fd;
	char buffer[256];
	int num = 0;
/*
......open ncl.que and see how many pp file is there
*/
	fd = fopen("ncl.que", "r");
	if (fd==NULL)
		return 0;
	while (fgets (buffer, 256, fd)!=NULL)
	{
		num++;
	}
	return num;
}
/***********************************************************************
**
**   SUBROUTINE: uw_chk_common()
**
**   FUNCTION:  check if we have the common memory area (Ncq_ncl_id as key)
**				if yes, attached to global buffer
**
**   INPUT:  none
**
**   OUTPUT: none
**
***********************************************************************/
uw_chk_common()
{
	int ncqnum;
	int *addr,shmflg;
	if (Ncq_ncl_id==-1)
		return 0;
/*
......Try to map to global section
*/
	shmflg = 438;  /* Allow everybody Read/Write access */
/*
......one word for process ID of NCL (must write from NCL)
......one NCLInfo struct for ncl running data
......6 word for 1 pp file (start and end both have 3 word)
*/
	ncqnum = uw_get_quenum();
	NCL_com_size = sizeof (int) + sizeof (NCLInfo) + 
					2 * ncqnum * (3*sizeof (int) + (12+9)*sizeof (char));
	NCQ_shmid = shmget(Ncq_ncl_id,NCL_com_size,shmflg);
/*
.....No such global section
*/
	if (NCQ_shmid < 0)
	{
		NCQ_common_open = 0;
		return 0;
	}
/*
........Attach to global section
*/
	addr = 0;
	shmflg = 0;
	NCQ_comptr = (char *)shmat(NCQ_shmid,addr,shmflg);
	if ((int)NCQ_comptr == -1)
		NCQ_common_open = 0;
	else
		NCQ_common_open = 1;
	return 1;
}
/***********************************************************************
**
**   SUBROUTINE: uw_chkkey_common()
**
**   FUNCTION:  check if we have the common memory area for NCL key
**				if yes, attached to global buffer and return key stored
**
**   INPUT:  none
**
**   OUTPUT: keys
**
***********************************************************************/
uw_chkkey_common(keys)
char *keys;
{
	int i, *addr,shmflg, size;
	if (NCL_subprocess==0)
		return 0;
/*
......Try to map to global section
*/
	shmflg = 438;  /* Allow everybody Read/Write access */
	size = sizeof (NCL_keyinfo);
	NCL_keyid = shmget(NCL_subprocess, size, shmflg);
/*
.....No such global section
*/
	if (NCL_keyid < 0)
	{
		NCL_keycommon_open = 0;
		strcpy(keys, "00000000");
		strcpy(NCL_keyinfo, "00000000");
		return 0;
	}
/*
........Attach to global section
*/
	addr = 0;
	shmflg = 0;
	NCL_key_comptr = (char *)shmat(NCL_keyid, addr,shmflg);
	if ((int)NCL_key_comptr == -1)
		NCL_keycommon_open = 0;
	else
		NCL_keycommon_open = 1;
	if (NCL_keycommon_open==1)
	{
		for (i=0; i<9; i++)
		{
			keys[i] = NCL_key_comptr[i];
		}
	}
	else
		strcpy(keys, "00000000");
	strcpy(NCL_keyinfo, keys);
	return 1;
}
/***********************************************************************
**
**   SUBROUTINE: uw_open_keycom()
**
**   FUNCTION:  This routine creates the common memory area for keys
**          and write the NCL_keyinfo into this area
**
**   INPUT:  none
**
**   OUTPUT: none
**
***********************************************************************/
int uw_open_keycom()
{
	int i, *addr,shmflg, size;
/*
.....Try to map to global section
*/
	shmflg = 438;
	size = sizeof (NCL_keyinfo);
	NCL_common_key = getpid();
	NCL_keyid = shmget(NCL_common_key, size, shmflg);
/*
.....No such global section
.....Let's create it
*/
	if (NCL_keyid>0)
	{
/*
.....already open, it shouldn't be, anyway, close it and open again
*/
		uw_close_keycom();
		NCL_keyid = shmget(NCL_common_key, size, shmflg);
		if (NCL_keyid>0)
			goto failed;
	}
	if (NCL_keyid<0)
	{
		shmflg = IPC_CREAT | shmflg;
		NCL_keyid = shmget(NCL_common_key, size, shmflg);
		if (NCL_keyid<0) goto failed;
/*
........Attach to global section
*/
		addr = 0;
		shmflg = 0;
		NCL_key_comptr = (char *)shmat(NCL_keyid, addr,shmflg);
		if ((int)NCL_key_comptr == -1) goto failed;
		for (i=0; i<9; i++)
		{
			NCL_key_comptr[i] = NCL_keyinfo[i];
		}
		NCL_keycommon_open = 1;
		goto done;
	}
/*
.....Error trying to allocate memory
*/
failed:;
   printf("*FATAL* Could not malloc common memory for NCL keys\n");
/*
.....End of routine
*/
done:;
   return;
}
/***********************************************************************
**
**   SUBROUTINE: uw_close_keycom()
**
**   FUNCTION:  This routine close the common memory area
**
**   INPUT:  none
**
**   OUTPUT: none
**
***********************************************************************/
void uw_close_keycom()
{
   if (NCL_keycommon_open==0)
		return;
	shmdt(NCL_key_comptr);
	shmctl (NCL_keyid, IPC_RMID,  NULL);
	NCL_keycommon_open = 0;
}

/**********************************************************************
**    I_FUNCTION : yes_no_cancelCB()
**       action for yes/no/cancel choice
**    PARAMETERS
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Motif callback structure.  Contains
**                        answer selected by the user.
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void yes_no_cancelCB(w, client_data, call_data)
Widget w;
XtPointer client_data,call_data;
{
	XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)call_data;
	switch(cbs->reason)
	{
		case XmCR_OK:
			yes_no_cancel = 1;
			XtPopdown(XtParent(w));
			break;
		case XmCR_CANCEL:
			yes_no_cancel = 0;
			XtPopdown(XtParent(w));
			break;
		case XmCR_HELP:
			yes_no_cancel = -1;
			XtPopdown(XtParent(w));
			break;
	}
	uw_mfanswer = 1;
}

/**********************************************************************
**    I_FUNCTION : uw_mfyes_no_cancel(parent, msg, title)
**			popup a dialog box for yes or no choice
**    PARAMETERS   
**       INPUT  : parent: parent window
**						msg: prompt message
**						title: window title
**       OUTPUT :  
**         			none 
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
uw_mfyes_no_cancel(parent, msg, title)
Widget parent;
char *msg, *title;
{
	Widget dialog;
	XEvent x_event;
	Arg args[5];
	XmString m;
	int n  = 0;
	XmString yes = XmStringCreateSimple("Yes");
	XmString no = XmStringCreateSimple("No");
	XmString cancel = XmStringCreateSimple("Cancel");
/*
.....default answer to "No"
*/
	yes_or_no = 0;
/*
...create a yesno dialog
*/
	m = XmStringCreateSimple(msg);
	XtSetArg(args[n], XmNautoUnmanage, False); n++;
	XtSetArg(args[n], XmNmessageString, m); n++;
	XtSetArg(args[n], XmNokLabelString, yes); n++;
	XtSetArg(args[n], XmNcancelLabelString, no); n++;
	XtSetArg(args[n], XmNhelpLabelString, cancel); n++;
	if(parent==UU_NULL)
	{
		dialog = (Widget) XmCreateQuestionDialog(uw_mf.graphic_app, 
											title, args, n);
	}
	else
	{
		dialog = (Widget) XmCreateQuestionDialog(parent, 
											title, args, n);
	}
	XtVaSetValues(XtParent(dialog),XmNtitle,title,NULL);
/*
...callback
*/
	XtAddCallback(dialog, XmNokCallback, yes_no_cancelCB, (XtPointer)NULL);
	XtAddCallback(dialog, XmNcancelCallback, yes_no_cancelCB, (XtPointer)NULL);
	XtAddCallback(dialog, XmNhelpCallback, yes_no_cancelCB, (XtPointer)NULL);
	XmStringFree(m);
	XmStringFree(no);
	XmStringFree(yes);
	XmStringFree(cancel);
	XtManageChild(dialog);
	XtPopup(XtParent(dialog), XtGrabNone);
	uw_mfanswer = 0;
	while (!uw_mfanswer)
	{
		XtAppNextEvent(uw_mf.application,&x_event);
		XtDispatchEvent(&x_event);
	}
	return yes_no_cancel;
}
/*********************************************************************
**    E_FUNCTION     : uw_mfupd_input(text)
**       Update the active input prompt (it could be
**    the input of command prompt or active form field if any of them is
active).
**
**    PARAMETERS
**       INPUT  : text: text to be updated
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfupd_input(text)
char *text;
{
	int stat;
	void uw_mfpost_textmsg();
/*
......update form input
*/
	stat = uw_formupd_input(text);
	if (stat==0)
		return;
/*
......update command line
*/
	uw_mfpost_textmsg(text);
}
/*********************************************************************
**    E_FUNCTION     : uw_mfpost_textmsg(text)
**       post the message for "Update the input of command prompt"
**			the text is limit to 19 chars 
**			since this function is used for update scalar label (which
**			is only limit to 6 chars, so it is enough
**
**    PARAMETERS
**       INPUT  : text: text to be updated
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfpost_textmsg(text)
char *text;
{
	XClientMessageEvent mevent;
	mevent.type = ClientMessage;
	mevent.display = uw_xw.disp;
	mevent.window = uw_xw.wd_id;
	mevent.message_type = NULL;
	mevent.format = 8;
	strncpy(mevent.data.b, text, 19);
	mevent.data.b[19] = '\0';
	XSendEvent(uw_xw.disp,uw_xw.wd_id,True,0,(XEvent *)&mevent);
/*	XPutBackEvent(uw_xw.disp, (XEvent *)&mevent); */
}

/*********************************************************************
**    E_FUNCTION     : uw_mfupd_cinput(text)
**       Update the input of command prompt
**
**    PARAMETERS
**       INPUT  : text: text to be updated
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfupd_cinput(text)
char *text;
{
	Boolean stat, sens;
	XmTextPosition left, right, ipos, start, end;

	sens = XtIsSensitive(uw_mfprompt.prompt);
	if (!sens)
		return;
	ipos = XmTextGetInsertionPosition(uw_mfprompt.prompt);
	stat = XmTextGetSelectionPosition(uw_mfprompt.prompt,
					&left, &right);
	if (stat)
	{
		start = left;
		end = right;
	}
	else
	{
		start = end = ipos;
	}
	XmTextReplace(uw_mfprompt.prompt, start, end, text);
}

/**********************************************************************
**    I_FUNCTION :  timeoutCB(XtPointer closure, XtIntervalId* id)
**       Callback for timeout function
**
**    PARAMETERS
**       INPUT  :
**          closure: user date, not used here
**          id: timer ID
**       OUTPUT :
**          none
**    RETURNS     
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
XtTimerCallbackProc timeoutCB(closure, id)
XtPointer closure;
XtIntervalId* id;
{
	char msg[256];
	int i, stat, ret;
	char keys[9];
   static int poll_time = 0;
   if (*id!=timer)
   {
/*
.....it must be the old time out left, ignore them
.....if not, it could abord the new process
*/
      return;
   }
/*
......if this NCL is a sub-process, we need check if the main program is
......running. if it's not, we need close and exit
*/
	if (NCL_subprocess!=0)
	{
/*
.....check the common memory to get NCL info
*/
		ret = uw_chkkey_common(keys);
/*
.....if there is no common memory now, it's mean main NCL is gone,
.....this NCL process have to forced to close
*/
		if (ret==0)
		{
/*
.....reset NCL_keyinfo to let function know it's forced out
*/
         strcpy(NCL_keyinfo, "00000000");
/*
......exit this NCL process
*/
			udm_signoff(UU_FALSE);
			return;
		}
		else
			strcpy(NCL_keyinfo, keys);
/*
......if there is a common memory, check the main processid still running,
......if not, remove this common area and this NCL process have to forced to
......close
*/
		if (S_if_process_running((pid_t)NCL_subprocess)==0)
		{
/*
.....reset NCL_keyinfo to let function know it's forced out
*/
         strcpy(NCL_keyinfo, "00000000");
/*
......exit this NCL process
*/
			udm_signoff(UU_FALSE);
			return;
		}
	}
/*
......this is a main NCL application, check if some sub-NCL closed and
......updated the NCL sub process array
*/
	else
	{
		for (i=0; i<NCL_sub_ncl;i++)
		{
			if (S_if_process_running(ncl_sub_pid[i])==0)
			{
				S_mfremove_subproc(i);
				i--;
			}
		}
	}
}		

/***********************************************************************
**
**   FUNCTION: uw_mfnew_session
**
**    This function start a sub-NCL process
**  
**  INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void uw_mfnew_session()
{
	char * com;
	int stat, mid, ver_int;
	char buf[300], cmdparm[120], sbuf[20], msg[400];
	UX_pathname dir, compath;
	pid_t pid;
	static int first = 1;
	if (NCL_subprocess>0)
	{
		uw_mferror("NCL sub-process cannot begin another NCL session.");
		return;
	}
	if (NCL_sub_ncl>=50)
	{
/*
......maxinum 50 sub-ncl process running
*/
		uw_mferror("We only allow 50 NCL sub-processes to run.\r\nClose some NCL sub-processes in order to run another one.");
		return;
	}
/*
	ver_int = NCL_version*10 + 0.5;
*/
    ver_int = NCL_version + 0.5;
	sprintf(sbuf,"NCLEXE%d",ver_int);
	com = (char*)getenv(sbuf);
	mid = getpid();
	if (com==NULL)
	{
		sprintf(msg, "%s is not defined.", sbuf);
		uw_mferror(msg);
		return;
	}
	sprintf(cmdparm, "-n=%u", mid);
	strcpy(compath, com);
	ul_break_fname(compath, dir, Scomexe);
/*
......use fork to create a child process and use exec to call
......the sub ncl process
*/
	pid = fork();
	if (pid<0)
	{
		uw_mferror("Error trying to create a sub-process of NCL");
		return;
	}
	switch (pid)
	{
		case 0:  /* Child process */
			sprintf(cmdparm, "-n=%u", mid);
/*
......it won't return unless error
*/
			execlp(compath,Scomexe,cmdparm,NULL);	
			uw_mferror("Error running NCL sub-process\n");
			break;
		default: /* Parent process */
/*
......save the child process ID
*/
			ncl_sub_pid[NCL_sub_ncl] = pid;
			NCL_sub_ncl++;
	}
	if (first)
	{
		first = 0;
		timer = XtAppAddTimeOut(uw_mf.application, 1000,
			(XtTimerCallbackProc)timeoutCB, NULL);
	}
}
/***********************************************************************
**
**   FUNCTION: uw_mfterm_sub()
**
**    This function kill all sub-NCL process
**  
**  INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
uw_mfterm_sub()
{
	int i;
	for (i=0; i<NCL_sub_ncl; i++)
		kill(ncl_sub_pid[i], 9);
}		
/***********************************************************************
**
**   FUNCTION: S_mfremove_subproc(sub)
**
**    This function remove a sub-NCL proces from the process array
**  
**  INPUT:  sub: index of the sub-process
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
static void S_mfremove_subproc(sub)
int sub;
{
	int i;
	for (i=sub; i<NCL_sub_ncl-1;i++)
	{
		ncl_sub_pid[i] = ncl_sub_pid[i+1];
	}
	if (sub<=NCL_sub_ncl-1)
		NCL_sub_ncl--;	
}
/***********************************************************************
**
**   FUNCTION: S_if_process_running
**
**    This function check if a NCL process is still running
**  
**  INPUT:  pid: process id 
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
static int S_if_process_running(pid)
pid_t pid;
{
	char cmd[256], *tok;
	char buf[BUFSIZ], str[BUFSIZ];
	FILE *ptr;
	int status;

	sprintf(cmd, "/usr/bin/ps -f -p %d", pid);
	if ((ptr = popen(cmd, "r")) != NULL)
	{
		while (fgets(buf, BUFSIZ, ptr) != NULL)
		{
/*
......check if the command path is the same
*/
			if (strstr(buf, Scomexe)!=0)
			{
				pclose(ptr);
				return 1;
			}
		}
		pclose(ptr);
	}
	return 0;
}

/***********************************************************************
**
**   FUNCTION: uw_mfget_subncl()
**
**    This function check if there are sub-process runnning
**		return the total number of sub-process running
**  
**  INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    the total number of sub-process running
**
**********************************************************************/
int uw_mfget_subncl()
{
	int i;
	for (i=0; i<NCL_sub_ncl; i++)
	{
/*
.....check process still running
*/
		if (S_if_process_running(ncl_sub_pid[i])==0)
		{
			S_mfremove_subproc(i);
			i--;
		}	
	}
	return NCL_sub_ncl;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfget_labelwid(str, wid)
**       get graphic label wid in pixels
**    PARAMETERS
**       INPUT  :
**          str: string label
**       OUTPUT :
**          wid: width of the string in pixel
**
**    RETURNS      -1: not success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfget_labelwid(str, wid)
char *str;
int *wid;
{
	*wid = uw_glfont.chrpwid * (strlen(str))+2;
}
#endif
#endif
