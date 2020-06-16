#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**    NAME         :  tigmfmatchattr.c
**       CONTAINS:
**             uig_attributes(OptionFrm, client_data, call_data)
**             uig_levelcolorCB(widget, client_data, call_data)
**             uig_matchattCB(widget, client_data, call_data)
**             uig_matchatt_quitCB(widget, client_data, call_data)
**    COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigmfmatchattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:49
*********************************************************************/
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/MwmUtil.h>
#include <Xm/Protocols.h>
#include <Xm/SashP.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Frame.h>
#include <X11/cursorfont.h>

#include "mdrel.h"
#include "tiges.h"
#include "usignal.h"
#include "ustdio.h"
#include "ulist.h"
#include "xenv1.h"
#include "tigmf.h"
#include "nclver.h"
#include "xfsys1.h"

static void uig_matchattCB();
static void uig_matchatt_quitCB();

extern Cursor normal_cursor;
extern UIG_match_color_array[7];
extern UIG_match_layer_array[7];

static int formEof;
Widget UIG_level_text[7];

typedef struct
{
   int type,fld,chc;
} Pstruct;

static Pstruct LpStruct[7][16];

static int UIG_matchlevcolor[7] = {0,0,0,0,0,0,0};

static ActionAreaItem actionList[] =
{
	{"OK", uig_matchattCB, (XtPointer)1},
	{"CANCEL", uig_matchatt_quitCB, (XtPointer)2}
};

/*********************************************************************
**    I_FUNCTION     :  uig_levelcolorCB(widget, client_data, call_data)
**       Callback for matching color menu.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void uig_levelcolorCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	Pstruct *ps=(Pstruct *)client_data;

	UIG_matchlevcolor[ps->fld] = ps->chc;

	return;
}

/*********************************************************************
**    I_FUNCTION     : uig_attributes(OptionFrm, client_data, call_data)
**			Make a Motif form that allows the user to set the color and
**       layer attributes for each level of matching.
**    PARAMETERS
**       INPUT  :
**				OptionFrm	=	ignored
**				client_data	=	ignored
**				call_data	=	ignored
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_attributes(OptionFrm, client_data, call_data)
Widget OptionFrm;
XtPointer client_data;
XtPointer call_data;
{
	int i, j, n, cols, rows, num_levels, ifl;
	Arg args[20];
	char buf[256];
	static char *level_strs [] = {"Exact Match", "Level 1", "Level 2",
		"Level 3", "Level 4", "Unmatched","Unmatched Secondary"}; 
	static char *color_list [] = {"Default", "Black", "White", "Blue",
		"Red", "Green", "Magenta", "Yellow", "Cyan", "Brown", "Tan",
		"Lt Blue", "Sea Green", "Orange", "Pink", "Purple", "Grey"};
	Widget winid, pane, att_form, control_form, action_form;
	Widget accept_but, cancel_but;
	Widget level_frame[7], level_form[7], level_label[7];
	Widget level_color[7], level_menu[7];
	Widget color_choice[16];
	XmString lstr;
	XEvent x_event;
	void uig_levelcolorCB();
/*
......create another form
*/
	n = 0;
	ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
				MWM_DECOR_MINIMIZE;
	XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
	ifl = MWM_FUNC_ALL | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE |
				MWM_FUNC_CLOSE;
	XtSetArg(args[n],XmNmwmFunctions,ifl); n++;

	att_form = XtCreatePopupShell("iges_attributes",xmDialogShellWidgetClass,
						TOOL_Parent, args,n);
	if (att_form == NULL) return;
	XtVaSetValues(att_form, XmNtitle,
		 "Layer and Color for Matched Entities", NULL);
/*
.....Create a paned window to hold the form &
.....Action Area
*/
	pane = XtVaCreateWidget("iges_att_form",xmPanedWindowWidgetClass, att_form,
				XmNsashWidth,1,
				XmNsashHeight,1,
				NULL);
/*
..... Start of Match Attributes form.
*/
	cols = 100;
	rows = 250;
	num_levels = 7;
/*
.....Create a Form widget 
*/
	n = 0;
	XtSetArg(args[n],XmNfractionBase,rows*cols); n++;
	winid = XtCreateWidget("attributes", xmFormWidgetClass,pane,args,n);
	if (winid == NULL) return;

	for (i = 0; i < num_levels; i++)
	{
/*
.....create a frame for each match level
*/
		n = 0;
		if (i>0)
		{
			XtSetArg(args[n],XmNtopAttachment, XmATTACH_WIDGET); n++;
			XtSetArg(args[n],XmNtopWidget, level_frame[i-1]); n++;
		}
		level_frame[i] = XtCreateManagedWidget("Frame", xmFrameWidgetClass,
		     winid, args,n);
		n = 0;
		level_form[i] = XtCreateManagedWidget("level_form", xmFormWidgetClass,
							level_frame[i], args,n);

		XtVaCreateManagedWidget(level_strs[i],
			xmLabelGadgetClass, level_frame[i],
			XmNchildType, XmFRAME_TITLE_CHILD,
			XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
			NULL);
/*
.....Layer label
*/
		n = 0;
		level_label[i] = XtCreateManagedWidget("Layer:  ", xmLabelWidgetClass,
					level_form[i], args,n);
/*
......Text field for layer number
*/
		n = 0;
		XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
		XtSetArg(args[n],XmNleftWidget, level_label[i]); n++;
		XtSetArg(args[n],XmNcolumns, 8); n++;

		UIG_level_text[i] = XtCreateManagedWidget("Text", xmTextWidgetClass,
		    level_form[i], args, n);

		if (UIG_match_layer_array[i] >= 0)
			sprintf(buf,"%d",UIG_match_layer_array[i]);
		else
			buf[0] = '\0';
		XmTextSetString(UIG_level_text[i],buf);

/*
.....Create color pull down list
*/
		UIG_matchlevcolor[i] = UIG_match_color_array[i] + 1;
		n = 0;
		XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
		XtSetArg(args[n],XmNleftWidget, UIG_level_text[i]); n++;
		level_color[i] = (Widget)XmCreatePulldownMenu(level_form[i], "Colors", NULL,0);

		lstr = XmStringCreateSimple("  Color:  ");
		XtSetArg(args[n], XmNsubMenuId, level_color[i]); n++;
		XtSetArg(args[n], XmNlabelString, lstr); n++;
		XtSetArg(args[n], XmNmarginHeight, 0); n++;
		XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
		level_menu[i] = (Widget)XmCreateOptionMenu(level_form[i],
		"Color", args, n);
		XmStringFree(lstr);

		for (j=0;j<16;j++)
		{
			color_choice[j] = XtVaCreateManagedWidget(color_list[j],
							xmPushButtonGadgetClass,  level_color[i], NULL);
			LpStruct[i][j].fld = i;
			LpStruct[i][j].chc = j;
			LpStruct[i][j].type = 1;
			XtAddCallback(color_choice[j], XmNactivateCallback,
			   (XtCallbackProc)uig_levelcolorCB, (XtPointer)&LpStruct[i][j]); 
		}
/*
...........Set the default choice
*/
		j = UIG_matchlevcolor[i];
		XtVaSetValues(level_color[i] , XmNmenuHistory,
			color_choice[j], NULL);
		XtManageChild(level_menu[i]);

	}
/*
.....Create the Action Area
*/
	n = XtNumber(actionList);
	for (i=0;i<n;i++) actionList[i].data = (XtPointer)att_form	;
	iges_mfcreate_action(pane,actionList,n);
/*
.....Manage the Form
*/
	XtManageChild(winid);
	XtManageChild(pane);
	XtManageChild(att_form);
	while (!formEof)
	{
		XDefineCursor(XtDisplay(TOOL_Parent), XtWindow(TOOL_Parent), normal_cursor);
		XFlush(XtDisplay(TOOL_Parent));
		XtAppNextEvent(TOOL_App, &x_event);
		XtDispatchEvent(&x_event);
	}
/*
..... End of Match Attributes form.
*/            
}
/*********************************************************************
**    I_FUNCTION : uig_matchattCB(widget, client_data, call_data)
**       Callback for "OK" button on the matching attributes window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void uig_matchattCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int i, layer;
	char *txtp;

	formEof = True;
	for (i=0;i<7;i++)
	{
		UIG_match_color_array[i] = UIG_matchlevcolor[i] - 1;
		txtp = XmTextGetString(UIG_level_text[i]);
		layer = -1;
		if (strlen(txtp))
		{
			sscanf(txtp,"%d",&layer);
			if (layer > 9999) layer = 9999;
		}
		UIG_match_layer_array[i] = layer;
	}
	XtUnmanageChild((Widget)client_data); 
	XtDestroyWidget((Widget)client_data); 
	XmUpdateDisplay((Widget)client_data);
}
/*********************************************************************
**    I_FUNCTION  : uig_matchatt_quitCB(widget, client_data, call_data)
**       Callback for "Cancel" button on the options window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void uig_matchatt_quitCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	formEof = True;
	XtUnmanageChild((Widget)client_data); 
	XtDestroyWidget((Widget)client_data);
	XmUpdateDisplay((Widget)client_data);
}
#endif
