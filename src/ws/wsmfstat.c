#include "usysdef.h"
#if UU_COMP != UU_WIN2K

/********************************************************************* 
**  NAME:  wsmfstat.c
**
**		CONTAINS:
**			quitCB(widget, clientData, callData)
**			uw_mfstat()
**			uw_mfwrstat(field,msg)
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wsmfstat.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:13
**
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:cursorfont.h>
#include <decw$include:Xm.h>
#include <decw$include:PushB.h>
#include <decw$include:RowColumn.h>
#include <decw$include:DialogS.h>
#include <decw$include:PanedW.h>
#include <decw$include:Form.h>
#include <decw$include:Frame.h>
#include <decw$include:MwmUtil.h>
#include <decw$include:Protocols.h>
#include <decw$include:Label.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/MwmUtil.h>
#include <Xm/Protocols.h>
#include <Xm/Label.h>
#endif

#include "dmark.h"
#include "gtbl.h"
#include "zkeysym.h"
#include "ws.h"
#include "wsxw.h"
#include "wsmf.h"
#include "dmotif.h"

/*
......use extern 
......12/10/97 Yurong
*/
/*
#define XBorder 11
#define YBorder 34
*/
extern int XBorder[2], YBorder[2];
extern int Border_stat;
#define NROW 4
#define NCOL 2
#define FBASE 3

/*
.....Global variable definitions
*/
extern UWS_MF uw_mf;
static UWS_MFSTATUS uw_mfstatus[5];

/**********************************************************************
**    I_FUNCTION :  quitCB(widget, clientData, callData)
**       Called if the user takes down the Status Area.  Notifies the
**			application that it is gone.
**    PARAMETERS   
**       INPUT  : 
**          widget     = Ignored.
**          clientData = Ignored.
**          callData   = Ignored.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void quitCB(widget, clientData, callData)
Widget widget;
XtPointer clientData,callData;
{
	uw_mf.status_app = NULL;
}

/**********************************************************************
**    I_FUNCTION :  uw_mfstat()
**       Creates the Status Area shell.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfstat()
{
	int ifl,n,i,j,k;
	Arg args[20];
	void exitCB();
	Widget winid,frame;
	char geom[20],buf[20];
	Atom watom;
	XmString label;
	void uw_GPS_resizeCB();
	int pos[2], size[2];
/*
.....Open the Status area shell
*/
	n = 0;
/*
........Does not contain Window Title & Resize borders
*/
	ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
			MWM_DECOR_MINIMIZE | MWM_DECOR_TITLE | MWM_DECOR_RESIZEH;
	XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
/*
.....added for adjust pos and size
.....For SGI, we need adjust position
.....12/15/97 Yurong
*/
	pos[0] = UDM_layout.status_pos[0];
	pos[1] = UDM_layout.status_pos[1];
	size[0] = UDM_layout.status_size[0] - 2*XBorder[0];
	size[1] = UDM_layout.status_size[1] - 2*YBorder[0];
	if (Border_stat==0)
	{
		pos[0] += XBorder[0];
		pos[1] += YBorder[0];
	}
/*
.....Position & Size area
*/
/*	sprintf(geom,"%dx%d+%d+%d",
		UDM_layout.status_size[0],
		UDM_layout.status_size[1],
		UDM_layout.status_pos[0],
		UDM_layout.status_pos[1]); */
	sprintf(geom,"%dx%d+%d+%d",
      size[0], size[1], pos[0], pos[1]);
	XtSetArg(args[n],XmNgeometry,geom); n++;
/*
.....Create Window for Menu
*/
	XtSetArg(args[n],XmNmappedWhenManaged,False); n++;
	uw_mf.status_app = XtAppCreateShell("Status","NCL",
		topLevelShellWidgetClass,uw_xw.disp,
		args,n);
	if (uw_mf.status_app == NULL) goto failed;
/*
.....Trap the CLOSE button
*/
	watom = XmInternAtom(uw_xw.disp,"WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(uw_mf.status_app,watom,quitCB,NULL);
/*
.....Create a Form widget
......to hold status entries
*/
	n = 0;
	XtSetArg(args[n],XmNfractionBase,NROW*NCOL); n++;
	winid = XtCreateWidget("Status",
		xmFormWidgetClass,uw_mf.status_app,args,n);
	if (winid == NULL) goto failed;
/*
.....Put up menu entries
*/
	k = 0;
	for (i=0;i<NROW;i++)
	{
/*
........Add columns to each row
*/
		for (j=0;j<NCOL;j++)
		{
			n = 0;
			XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNtopPosition,i*NCOL); n++;
			XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNbottomPosition,(i+1)*NCOL); n++;
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNleftPosition,j*NROW); n++;
			XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNrightPosition,(j+1)*NROW); n++;
			frame = XtCreateManagedWidget(" ",xmFrameWidgetClass,
				winid,args,n);
			n = 0;
			XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
			label = XmStringCreateSimple(" ");
			XtSetArg(args[n],XmNlabelString,label); n++;
			sprintf(buf,"Stat_%d",k);
			uw_mfstatus[0].area[k] = XtCreateManagedWidget(buf,
					xmLabelWidgetClass,frame,args,n);
			XmStringFree(label);
			k++;
		}
	}
/*
...add event handler for save the size and position change
...Yurong
*/
	XtAddEventHandler(uw_mf.status_app, StructureNotifyMask, False,
							(XtEventHandler)uw_GPS_resizeCB, (XtPointer)3);
/*
.....Manage Status area
*/
	XtManageChild(winid);
	XtRealizeWidget(uw_mf.status_app);
	goto done;
/*
.....Error trying to create menu
*/
failed:;
	sprintf(buf,"Could not create Status area.");
	ud_wrerr(buf);
/*
.....End of routine
*/
done:;
	return;
}

/**********************************************************************
**    I_FUNCTION :  uw_mfwrstat(field,msg)
**       Outputs a message to a field in the Status Area.
**    PARAMETERS   
**       INPUT  : 
**          field   = Field number to write to.
**				msg     = Message to output.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfwrstat(field,msg)
int field;
char *msg;
{
	XmString label;
	if (uw_mf.status_app != NULL)
	{
		label = XmStringCreateSimple(msg);
		XtVaSetValues(uw_mfstatus[0].area[field-FBASE],
			XmNlabelString,label,
			NULL);
		XmStringFree(label);
	}
}
#endif
