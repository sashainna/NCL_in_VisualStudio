#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**  NAME:  tmfplot.c
**			Motif function to CADPLOT
**
**       CONTAINS:
**       names of functions in file
**				utp_winplot
**          utp_ermsg
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tmfplot.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:19
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:cursorfont.h>
#include <decw$include:Xm.h>
#include <decw$include:PushB.h>
#include <decw$include:PushBG.h>
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
#include <decw$include:Protocols.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeBG.h>
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
#include <Xm/FileSB.h>
#include <Xm/SelectioB.h>
#include <Xm/DialogS.h>
#include <Xm/Protocols.h>
#include <sys/stat.h>
#include <stdio.h>
#endif

#include    "ustdio.h"
#include    "usysdef.h"
#include    "zsysdep.h"
#include "xenv1.h"
#include "gobas.h"
#include "tplot.h"
#include "stdlib.h"

typedef struct
{
   char *label;
   void (*callback)();
   XtPointer data;
} ActionAreaItem;

typedef struct
{
   int type,fld,chc;
} Pstruct;
Pstruct *pStruct[40];
typedef struct
{
   int sel;
   char *file;
} FSELECT;

XtAppContext Plot_App;
Widget Plot_Parent;
Widget Main_form;
Widget mform_item[25], mform_choice[16];
Widget plot_stat_win;
Cursor normal_cursor, wait_cursor;

static Widget opt_frame, form_in, actionBut[10], mine;
static int Nps=0;
static int Plot_Port=0;
static int Plot_ctl=0;

void utp_mfrun(), utp_appexit();
static ActionAreaItem actionList[] =
{
	{"RUN", utp_mfrun, (XtPointer)1},
	{"EXIT", utp_appexit, (XtPointer)2}
};

extern UX_pathname pfnm;

/*********************************************************************
**    I_FUNCTION     : utp_mfcontrol() 
**          get P1P2_control
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          filename
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
utp_mfcontrol()
{
	return Plot_ctl;
}

/*********************************************************************
**    I_FUNCTION     : utp_mfrun(widget, client_data, call_data)
**       Callback for "RUN" button on the main window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_mfrun(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	char *text;
	char *indx;
	text = XmTextGetString(mform_item[1]);
/*
.....get Parameters from main window
*/
	if(text[0]!='\0')
	{
		strcpy(pfnm, text);
		indx = strstr(pfnm, ".pl");
		if (indx!=NULL)
			*indx = '\0';
	}
	else
	{
		utp_ermsg("No input plot file specified.",NULL);
		XtFree(text);
		return;
	}
	XtFree(text);
	text = XmTextGetString(mform_item[8]);
	if(text[0]!='\0')
	{
		plotopts.linewt = atof (text);
	}
	if (plotopts.linewt<=0)
	{
		utp_ermsg("Invalid line width.",NULL);
		XtFree(text);
		return;
	}
	XtFree(text);
	if (Plot_Port==0)
	{
		plotopts.print = 0;
		text = XmTextGetString(mform_item[14]);
		if(text[0]!='\0')
			strcpy(plotopts.diskfnm, text);
		else
		{
			utp_ermsg("No output disk file specified.",NULL);
			XtFree(text);
			return;
		}
		XtFree(text);
	}
	else if (Plot_Port == 1)
	{
		plotopts.print = 0;
		text = XmTextGetString(mform_item[17]);
		if(text[0]!='\0')
			strcpy(plotopts.port, text);
		else
		{
			utp_ermsg("No port specified.",NULL);
			XtFree(text);
			return;
		}
		XtFree(text);
	}
	else
	{
		plotopts.print = 1;
		text = XmTextGetString(mform_item[22]);
		strcpy(plotopts.printque, text);
		XtFree(text);
	}

	utp_interact();
}

/*********************************************************************
**    I_FUNCTION     :utp_appexit(widget, client_data, call_data)
**       Callback for "Exit" button on the main window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_appexit(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int i;
	for (i=0;i<Nps;i++) free(pStruct[i]);
	exit(0);
}

/*********************************************************************
**    I_FUNCTION     :  plot_mbrowseCB(widget, client_data, call_data)
**       Callback for browse button on the main window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void plot_mbrowseCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	UX_pathname filename;
	int nc;
	int flag = (int)client_data;
	if (flag==1)
	{
/*
.....get plot file name
*/
		plot_mf_filename(NULL, "Input Plot File", "*.pl1", filename, &nc);
		if (nc!=0)
		{
			XmTextSetString(mform_item[1], filename);
			XmTextSetInsertionPosition(mform_item[1], nc);
		}
	}
	else if (flag==2)
	{
/*
.....get disk file
*/
		plot_mf_filename(NULL, "Output Disk File", "*.*", filename, &nc);
		if (nc!=0)
		{
			XmTextSetString(mform_item[14], filename);
			XmTextSetInsertionPosition(mform_item[14], nc);
		}
	}
}

/*********************************************************************
**    I_FUNCTION     :  Plot_OptCB(widget, client_data, call_data)
**       Callback for choice button on the main window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void Plot_OptCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	Pstruct *ps=(Pstruct *)client_data;
	if (ps->fld==4)
/*
.....Plot type options
*/
	{
		if (ps->chc==0)
		{
			strcpy(plotopts.type, "7475");
			if (Plot_Port==1)
			{
				XtSetSensitive(mform_item[19], True);
				XtSetSensitive(mform_item[20], True);
			}
			XtSetSensitive(mform_item[9], True);
			XtSetSensitive(mform_item[10], True);
		}
		else if (ps->chc==1)
		{
			strcpy(plotopts.type, "7580");
			if (Plot_Port==1)
			{
				XtSetSensitive(mform_item[19], True);
				XtSetSensitive(mform_item[20], True);
			}
			XtSetSensitive(mform_item[9], True);
			XtSetSensitive(mform_item[10], True);
		}
		else if (ps->chc==2)
		{
			strcpy(plotopts.type, "ps");
			if (Plot_Port==1)
			{
				XtSetSensitive(mform_item[19], False);
				XtSetSensitive(mform_item[20], False);
			}
			XtSetSensitive(mform_item[9], False);
			XtSetSensitive(mform_item[10], False);
		}
		else if (ps->chc==3)
		{
			strcpy(plotopts.type, "1043");
			if (Plot_Port==1)
			{
				XtSetSensitive(mform_item[19], False);
				XtSetSensitive(mform_item[20], False);
			}
			XtSetSensitive(mform_item[9], False);
			XtSetSensitive(mform_item[10], False);
		}
	}
	else if (ps->fld==6)
/*
.....Paper size options
*/
	{
		plotopts.size = ps->chc;
	}
	else if (ps->fld==10)
/*
.....Bypass mode options
*/
	{
		plotopts.bypass = ps->chc;
	}
	else if (ps->fld==12)
	{
/*
.....Port options
*/
		Plot_Port = ps->chc;
/*
........Disk file
*/
		if (Plot_Port==0)
		{
			XtSetSensitive(mform_item[13], True);
			XtSetSensitive(mform_item[14], True);
			XtSetSensitive(mform_item[15], True);
			XtSetSensitive(mform_item[16], False);
			XtSetSensitive(mform_item[17], False);
			XtSetSensitive(mform_item[19], False);
			XtSetSensitive(mform_item[20], False);
			XtSetSensitive(mform_item[21], False);
			XtSetSensitive(mform_item[22], False);
		}
/*
........Hardware port
*/
		else if (Plot_Port == 1)
		{
			XtSetSensitive(mform_item[13], False);
			XtSetSensitive(mform_item[14], False);
			XtSetSensitive(mform_item[15], False);
			XtSetSensitive(mform_item[16], True);
			XtSetSensitive(mform_item[17], True);
			XtSetSensitive(mform_item[21], False);
			XtSetSensitive(mform_item[22], False);
			if ((strcmp(plotopts.type, "7475")==0)||
					 (strcmp(plotopts.type, "7580")==0))
			{
				XtSetSensitive(mform_item[19], True);
				XtSetSensitive(mform_item[20], True);
			}
		}
/*
........Print que
*/
		else
		{
			XtSetSensitive(mform_item[13], False);
			XtSetSensitive(mform_item[14], False);
			XtSetSensitive(mform_item[15], False);
			XtSetSensitive(mform_item[16], False);
			XtSetSensitive(mform_item[17], False);
			XtSetSensitive(mform_item[19], False);
			XtSetSensitive(mform_item[20], False);
			XtSetSensitive(mform_item[21], True);
			XtSetSensitive(mform_item[22], True);
		}
	}
	else if (ps->fld==20)
/*
.....P1P2 control option
*/
	{
		Plot_ctl = ps->chc;
	}
}

/*********************************************************************
**    I_FUNCTION : utp_winplot 
**       window function to CADPLOT
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
utp_winplot()
{
	Arg args[20];
	XmString lstr;
	XEvent event;
	int rows1;
	char *p, swid[20];
	Widget opt_frame, pane, action_area, utp_mfcreate_action();
	int n,i,j,rows,cols,status,ifl, nc;
	Atom watom;
/*
.....Initialize MOTIF
*/
	i = 0;
	n = 0;
	XtSetArg(args[n],XmNmappedWhenManaged,False); n++;
	XtSetArg(args[n],XmNwidth, 700); n++;
	XtSetArg(args[n],XmNheight, 650); n++;
	XtSetArg(args[n],XmNtitle, "CADPLOT"), n++;
	Plot_Parent = XtAppInitialize (&Plot_App, "PLOT", NULL,0,&i,NULL,NULL,
						args,n);
	watom = XmInternAtom(XtDisplay(Plot_Parent), "WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(Plot_Parent, watom, utp_appexit,
						NULL);
	pane = XtVaCreateWidget("plot_main",xmPanedWindowWidgetClass, Plot_Parent,
				XmNsashWidth,1,
				XmNsashHeight,1,
				NULL);
/*
.....Create a Form widget
*/
	n = 0;
	rows = 15;
	cols = 26;
	XtSetArg(args[n],XmNfractionBase,rows*cols); n++;
	Main_form = XtCreateWidget("main_form", xmFormWidgetClass,pane,args,n);
	if (Main_form == NULL) return;
	
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 9*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	
	opt_frame = XtCreateManagedWidget("top_frame", xmFrameWidgetClass,Main_form,
						args,n);
	n = 0;
	rows1 = 10;
	XtSetArg(args[n],XmNfractionBase,rows1*cols); n++;
	form_in =  XtCreateManagedWidget("form_in", xmFormWidgetClass,opt_frame,
					args,n);

/*
.....create first rows
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;

	mform_item[0] = XtCreateManagedWidget("Input Plot File:",
					xmLabelWidgetClass,form_in,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[0]); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 25); n++;

	mform_item[1] = XtCreateManagedWidget("Text",
			xmTextWidgetClass,form_in,args,n);
	XmTextSetString(mform_item[1], pfnm);
	nc = strlen(pfnm);
	XmTextSetInsertionPosition(mform_item[1], nc);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[1]); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	mform_item[2] = XmCreatePushButton(form_in, "Browse", args, n);
	XtAddCallback(mform_item[2], XmNactivateCallback, 
					(XtCallbackProc)plot_mbrowseCB, (XtPointer)1);
	XtManageChild(mform_item[2]);
/*
.....create second rows
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;
	mform_item[3] = (Widget)XmCreatePulldownMenu(form_in, "Plot Type",NULL,0);
/*
...........Create an Option button
*/

	lstr = XmStringCreateSimple("Plotter Type:");
	XtSetArg(args[n], XmNsubMenuId, mform_item[3]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	mform_item[4] = (Widget)XmCreateOptionMenu(form_in,
						"", args, n);
	XmStringFree(lstr);
/*
...........Create the options
*/
	mform_choice[0] = XtVaCreateManagedWidget("HP 7475", 
								xmPushButtonGadgetClass, 
								mform_item[3], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 4;
	pStruct[Nps]->chc = 0;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[0], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);
	mform_choice[1] = XtVaCreateManagedWidget("HP 7580 (DesignJet)", 
								xmPushButtonGadgetClass, 
								mform_item[3], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 4;
	pStruct[Nps]->chc = 1;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[1], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[2] = XtVaCreateManagedWidget("PostScript", 
								xmPushButtonGadgetClass, 
								mform_item[3], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 4;
	pStruct[Nps]->chc = 2;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[2], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[3] = XtVaCreateManagedWidget("Calcomp 1043", 
								xmPushButtonGadgetClass, 
								mform_item[3], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 4;
	pStruct[Nps]->chc = 3;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[3], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);
/*
...........Set the default choice
*/
	if (strcmp(plotopts.type, "7475")==0)
		XtVaSetValues(mform_item[3], XmNmenuHistory,
							mform_choice[0], NULL);
	else if (strcmp(plotopts.type, "7580")==0)
		XtVaSetValues(mform_item[3], XmNmenuHistory,
							mform_choice[1], NULL);
	else if (strcmp(plotopts.type, "ps")==0)
		XtVaSetValues(mform_item[3], XmNmenuHistory,
							mform_choice[2], NULL);
	else if (strcmp(plotopts.type, "1043")==0)
		XtVaSetValues(mform_item[3], XmNmenuHistory,
							mform_choice[3], NULL);
	XtManageChild(mform_item[4]);
	
/*
.....create third rows
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;
	mform_item[5] = (Widget)XmCreatePulldownMenu(form_in, "Plot Size",NULL,0);
/*
...........Create an Option button
*/

	lstr = XmStringCreateSimple("Plot Size:");
	XtSetArg(args[n], XmNsubMenuId, mform_item[5]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	mform_item[6] = (Widget)XmCreateOptionMenu(form_in,
						"", args, n);
	XmStringFree(lstr);
/*
...........Create the options
*/
	mform_choice[0] = XtVaCreateManagedWidget("AH", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 0;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[0], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[1] = XtVaCreateManagedWidget("AV", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 1;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[1], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[2] = XtVaCreateManagedWidget("B", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 2;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[2], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[3] = XtVaCreateManagedWidget("C", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 3;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[3], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[4] = XtVaCreateManagedWidget("D", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 4;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[4], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[5] = XtVaCreateManagedWidget("E", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 5;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[5], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[6] = XtVaCreateManagedWidget("F", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 6;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[6], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[7] = XtVaCreateManagedWidget("A0", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 7;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[7], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[8] = XtVaCreateManagedWidget("A1", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 8;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[8], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[9] = XtVaCreateManagedWidget("A2", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 9;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[9], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[10] = XtVaCreateManagedWidget("A3", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 10;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[10], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[11] = XtVaCreateManagedWidget("A4", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 11;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[11], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);


	mform_choice[12] = XtVaCreateManagedWidget("USER1", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 12;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[12], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[13] = XtVaCreateManagedWidget("USER2", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 13;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[13], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[14] = XtVaCreateManagedWidget("USER3", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 14;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[14], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[15] = XtVaCreateManagedWidget("USER4", 
								xmPushButtonGadgetClass, 
								mform_item[5], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 6;
	pStruct[Nps]->chc = 15;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[15], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);
/*
.....set default choice
*/
	if (plotopts.size<0) plotopts.size = 0;
	XtVaSetValues(mform_item[5], XmNmenuHistory,
							mform_choice[plotopts.size], NULL);
	XtManageChild(mform_item[6]);
/*
.....create forth line
*/	
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 3*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 4*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;

	mform_item[7] = XtCreateManagedWidget("Line Width:",
					xmLabelWidgetClass,form_in,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 3*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 4*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[7]); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 10); n++;

	mform_item[8] = XtCreateManagedWidget("Text",
			xmTextWidgetClass,form_in,args,n);
	sprintf(swid, "%f",plotopts.linewt);
	XmTextSetString(mform_item[8], swid);

/*
.....create fifth rows
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 4*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 5*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;
	mform_item[9] = (Widget)XmCreatePulldownMenu(form_in, "Bypass mode",NULL,0);
/*
...........Create an Option button
*/

	lstr = XmStringCreateSimple("Bypass Mode:");
	XtSetArg(args[n], XmNsubMenuId, mform_item[9]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	mform_item[10] = (Widget)XmCreateOptionMenu(form_in,
						"", args, n);
	XmStringFree(lstr);
/*
...........Create the options
*/
	mform_choice[0] = XtVaCreateManagedWidget("Off", 
								xmPushButtonGadgetClass, 
								mform_item[9], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 10;
	pStruct[Nps]->chc = 0;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[0], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[1] = XtVaCreateManagedWidget("On", 
								xmPushButtonGadgetClass, 
								mform_item[9], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 10;
	pStruct[Nps]->chc = 1;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[1], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);
/*
.....set default choice
*/
	XtVaSetValues(mform_item[9], XmNmenuHistory,
							mform_choice[plotopts.bypass], NULL);
	XtManageChild(mform_item[10]);
/*
.....create sixth rows
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 5*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 6*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;
	mform_item[11] = (Widget)XmCreatePulldownMenu(form_in, "Plot Output",NULL,0);
/*
...........Create an Option button
*/

	lstr = XmStringCreateSimple("Plot Output:");
	XtSetArg(args[n], XmNsubMenuId, mform_item[11]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	mform_item[12] = (Widget)XmCreateOptionMenu(form_in,
						"", args, n);
	XmStringFree(lstr);
/*
...........Create the options
*/
	mform_choice[0] = XtVaCreateManagedWidget("Disk File", 
								xmPushButtonGadgetClass, 
								mform_item[11], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 12;
	pStruct[Nps]->chc = 0;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[0], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[1] = XtVaCreateManagedWidget("Plotter", 
								xmPushButtonGadgetClass, 
								mform_item[11], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 12;
	pStruct[Nps]->chc = 1;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[1], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

/*
...........Add the callback routine
*/
	mform_choice[2] = XtVaCreateManagedWidget("Print Que", 
								xmPushButtonGadgetClass, 
								mform_item[11], NULL);
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 12;
	pStruct[Nps]->chc = 2;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[2], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);
/*
.....set default choice
*/
	if (plotopts.print == 1)
	{
		XtVaSetValues(mform_item[11], XmNmenuHistory,
									mform_choice[2], NULL);
		Plot_Port = 2;
	}
	else if (((plotopts.diskfnm[0] == '\0')&&(plotopts.port[0] == '\0')) ||
			(plotopts.diskfnm[0] != '\0'))
	{
		XtVaSetValues(mform_item[11], XmNmenuHistory,
									mform_choice[0], NULL);
		Plot_Port = 0;
	}
	else if (plotopts.port[0] != '\0')
	{
		XtVaSetValues(mform_item[11], XmNmenuHistory,
									mform_choice[1], NULL);
		Plot_Port = 1;
	}
	XtManageChild(mform_item[12]);

/*
.....create seventh rows
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 6*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 7*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;

	mform_item[13] = XtCreateManagedWidget("Output Disk File:",
					xmLabelWidgetClass,form_in,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 6*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 7*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[13]); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 25); n++;

	mform_item[14] = XtCreateManagedWidget("Text",
			xmTextWidgetClass,form_in,args,n);
	XmTextSetString(mform_item[14], plotopts.diskfnm);
	nc = strlen(plotopts.diskfnm);
	XmTextSetInsertionPosition(mform_item[14], nc);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 6*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 7*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[14]); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	mform_item[15] = XmCreatePushButton(form_in, "Browse", args, n);
	XtAddCallback(mform_item[15], XmNactivateCallback, 
					(XtCallbackProc)plot_mbrowseCB, (XtPointer)2);
	XtManageChild(mform_item[15]);
/*
.....create eightth rows
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 7*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 8*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;

	mform_item[16] = XtCreateManagedWidget("Port:",
					xmLabelWidgetClass,form_in,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 7*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 8*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[16]); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;

	mform_item[17] = XtCreateManagedWidget("Text",
			xmTextWidgetClass,form_in,args,n);
	XmTextSetString(mform_item[17], plotopts.port);
	nc = strlen(plotopts.port);
	XmTextSetInsertionPosition(mform_item[17], nc);
/*
.....Create ninth row
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 8*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 9*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;

	mform_item[21] = XtCreateManagedWidget("Print Que:",
					xmLabelWidgetClass,form_in,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 8*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 9*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[21]); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;

	mform_item[22] = XtCreateManagedWidget("Text",
			xmTextWidgetClass,form_in,args,n);
	XmTextSetString(mform_item[22], plotopts.printque);
	nc = strlen(plotopts.printque);
	XmTextSetInsertionPosition(mform_item[22], nc);
/*
.....Create tenth row
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 9*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 10*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;
	mform_item[19] = (Widget)XmCreatePulldownMenu(form_in, "Control",NULL,0);
/*
...........Create an Option button
*/

	lstr = XmStringCreateSimple("Is Plot Size Controlled by P1 and P2?");
	XtSetArg(args[n], XmNsubMenuId, mform_item[19]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	mform_item[20] = (Widget)XmCreateOptionMenu(form_in,
						"", args, n);
	XmStringFree(lstr);
/*
...........Create the options
*/
	mform_choice[0] = XtVaCreateManagedWidget("No", 
								xmPushButtonGadgetClass, 
								mform_item[19], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 20;
	pStruct[Nps]->chc = 0;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[0], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);

	mform_choice[1] = XtVaCreateManagedWidget("Yes", 
								xmPushButtonGadgetClass, 
								mform_item[19], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 20;
	pStruct[Nps]->chc = 1;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[1], XmNactivateCallback,
				(XtCallbackProc)Plot_OptCB, (XtPointer)pStruct[Nps++]);
/*
.....set default choice
*/
	XtVaSetValues(mform_item[19], XmNmenuHistory,
									mform_choice[0], NULL);
	XtManageChild(mform_item[20]);
	if (Plot_Port==0)
	{
		XtSetSensitive(mform_item[13], True);
		XtSetSensitive(mform_item[14], True);
		XtSetSensitive(mform_item[15], True);
		XtSetSensitive(mform_item[16], False);
		XtSetSensitive(mform_item[17], False);
		XtSetSensitive(mform_item[19], False);
		XtSetSensitive(mform_item[20], False);
		XtSetSensitive(mform_item[21], False);
		XtSetSensitive(mform_item[22], False);
	}
	else if (Plot_Port == 1)
	{
		XtSetSensitive(mform_item[13], False);
		XtSetSensitive(mform_item[14], False);
		XtSetSensitive(mform_item[15], False);
		XtSetSensitive(mform_item[16], True);
		XtSetSensitive(mform_item[17], True);
		if ((strcmp(plotopts.type, "7475")==0)||
					(strcmp(plotopts.type, "7580")==0))
		{
			XtSetSensitive(mform_item[19], True);
			XtSetSensitive(mform_item[20], True);
		}
		XtSetSensitive(mform_item[21], False);
		XtSetSensitive(mform_item[22], False);
	}
	else
	{
		XtSetSensitive(mform_item[13], False);
		XtSetSensitive(mform_item[14], False);
		XtSetSensitive(mform_item[15], False);
		XtSetSensitive(mform_item[16], False);
		XtSetSensitive(mform_item[17], False);
		XtSetSensitive(mform_item[19], False);
		XtSetSensitive(mform_item[20], False);
		XtSetSensitive(mform_item[21], True);
		XtSetSensitive(mform_item[22], True);
	}
/*
....."status" label
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 9*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 10*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows); n++;

	mform_item[18] = XtCreateManagedWidget("Status",
				xmLabelWidgetClass,Main_form,args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNtopWidget, mform_item[18]); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNscrollVertical, True); n++;
	XtSetArg(args[n], XmNeditable, False); n++;
	XtSetArg(args[n],XmNeditMode, XmMULTI_LINE_EDIT); n++;
	plot_stat_win = XmCreateScrolledText(Main_form,
						"stat_text" ,args,n);
	XmTextSetString(plot_stat_win, "");
	XtManageChild(plot_stat_win);
	
/*
.....Create the Action Area
*/
	n = XtNumber(actionList);
	for (i=0;i<n;i++) actionList[i].data = (XtPointer)Plot_Parent;
	action_area = utp_mfcreate_action(pane,actionList,n);
	XtManageChild(Main_form);
	XtManageChild(pane);
	XtRealizeWidget(Plot_Parent);
	XtMapWidget(Plot_Parent);
	normal_cursor = XCreateFontCursor(XtDisplay(Plot_Parent),
							XC_top_left_arrow);
	wait_cursor = XCreateFontCursor(XtDisplay(Plot_Parent),XC_watch);
	for(;;)
	{
		XDefineCursor(XtDisplay(Plot_Parent),XtWindow(Plot_Parent),
				normal_cursor);
		XFlush(XtDisplay(Plot_Parent));
		XtAppNextEvent(Plot_App,&event);
		XDefineCursor(XtDisplay(Plot_Parent),XtWindow(Plot_Parent),
				wait_cursor);
		XFlush(XtDisplay(Plot_Parent));
		XtDispatchEvent(&event);
	}
}
		


/*********************************************************************
**    I_FUNCTION :  utp_ermsg(str1,str2)
**       Handle the error message.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
utp_ermsg(str1,str2)
char	*str1;
char	*str2;
{
	char string[1000];
	static int win_pos = 0;
	if (Motif_Plot!=1)
		printf(str1,str2);
	else
	{
		sprintf(string, str1,str2);
		utp_mfmsg_box(NULL, "Error!", string);
	}
}	/* utp_ermsg */


/*********************************************************************
**    I_FUNCTION     : utp_strout(str) 
**          Output a string to main window's status area
**    PARAMETERS
**       INPUT  :
**          string                  string to output
**       OUTPUT :
**						nine
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
utp_strout(str)
char	*str;
{
	static int win_pos = 0;
	if (Motif_Plot!=1)
		printf(str);
	else
	{
		XmTextInsert(plot_stat_win, win_pos, str);
		win_pos = win_pos + strlen(str);
		XmTextShowPosition(plot_stat_win, win_pos);
		XFlush(XtDisplay(plot_stat_win));
	}
}	
	

/**********************************************************************
**    I_FUNCTION :  utp_mfcreate_action(parent,actions,num_actions)
**       Creates the action area for forms, 
**    PARAMETERS   
**       INPUT  : 
**          parent      = Form widget to create Action Area for.
**				actions     = List of Action Area buttons and procedures.
**				num_actions = Number of 'actions' for this area.
**       OUTPUT :  
**          output
**    RETURNS      : Action Area Widget.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Widget utp_mfcreate_action(parent,actions,num_actions)
Widget parent;
ActionAreaItem *actions;
int num_actions;
{
#define TIGHTNESS 40
	Widget action_area,widget;
	int i;
	Dimension h,height;
/*
.....Create Action Area Form
*/
	action_area = XtVaCreateWidget("action_area",
		xmFormWidgetClass,parent,
		XmNfractionBase,num_actions*TIGHTNESS-1,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
/*
.....Create each of the buttons
*/
	for (i=0; i<num_actions; i++)
	{
		actionBut[i] = XtVaCreateManagedWidget(actions[i].label,
			xmPushButtonWidgetClass, action_area,
			XmNleftAttachment, i? XmATTACH_POSITION : XmATTACH_FORM,
			XmNleftPosition, TIGHTNESS*i,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNrightAttachment, i!=num_actions ? XmATTACH_POSITION : XmATTACH_FORM,
			XmNrightPosition, TIGHTNESS * i + (TIGHTNESS - 1),
			XmNshowAsDefault, i==0,
			XmNdefaultButtonShadowThickness, 1,
			NULL);
/*
.....Add the button callback
*/
		if (actions[i].callback)
			XtAddCallback(actionBut[i],XmNactivateCallback,
				actions[i].callback,actions[i].data);
/*
.....Set the default button and disable
.....resize of this pane
*/
		if (i == 0)
		{
			XtVaGetValues(action_area, XmNmarginHeight, &h, NULL);
			XtVaGetValues(actionBut[i], XmNheight, &height, NULL);
			height += 2 * h;
			XtVaSetValues(action_area,
				XmNdefaultButton, actionBut[i],
				XmNpaneMaximum, height,
				XmNpaneMinimum, height,
				NULL);
		}
	}
/*
.....Manage the Action Area
*/
	XtManageChild(action_area);
	return(action_area);
}

/**********************************************************************
**    I_FUNCTION :  FileOkCB(widget,client_data,call_data)
**       Processes the OK button in the File Selection dialog.  Returns
**       the filename selected by the user.
**    PARAMETERS
**       INPUT  :
**          widget      = File Selection dialog shell.
**          client_data = Pointer to structure which will contain the
**                        selected filename.
**          call_data   = Motif callback structure.
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
   char *filen, *dir;
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
   p->sel = 1;
/*
.....Destroy the dialog
*/
   XtUnmanageChild(widget);
/*   XtDestroyWidget(widget); */
}

/**********************************************************************
**    I_FUNCTION :  FileCancelCB(widget,client_data,call_data)
**       Processes the CANCEL button in the File Selection dialog.
**    PARAMETERS
**       INPUT  :
**          widget      = File Selection dialog shell.
**          client_data = Pointer to structure which will contain the
**                        blanked out filename.
**          call_data   = Motif callback structure.
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
   p->sel = 1;
   p->file[0] = '\0';
/*
.....Destroy the dialog
*/
   XtUnmanageChild(widget);
/*   XtDestroyWidget(widget); */
}

/**********************************************************************
**    I_FUNCTION :  plot_mf_filename(title,filter,filename,nc)
**       Opens a File Selection dialog and returns the user selected
**       filename.
**    PARAMETERS
**       INPUT  :
**          title     = Title of File Selection dialog.
**          filter    = Filename filter to use for list of available
**                      files.
**       OUTPUT :
**          filename  = Name of selected file.
**          nc        = Number of chars in 'filename'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
plot_mf_filename(parent, title,filter,filename,nc)
Widget parent;
char *title,*filter,*filename;
int *nc;
{
   int i, n,jmpflag;
   Arg args[20];
   Widget list;
   XmString labstr;
   XEvent event;
   FSELECT Fselect;
/*
.....Create file selection dialog
*/
   n = 0;
   labstr = XmStringCreateSimple(filter);
   XtSetArg(args[n],XmNdirMask,labstr); n++; 
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
   if(parent==NULL)
   {
      mine = (Widget) XmCreateFileSelectionDialog
         (Plot_Parent,"file_select",args,n);
   }
   else
	{
      mine = (Widget) XmCreateFileSelectionDialog
         (parent,"file_select",args,n);
   }
   XtVaSetValues(XtParent(mine),XmNtitle,title,NULL);
   XmStringFree(labstr);
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
   XtAddCallback(mine,XmNcancelCallback,FileCancelCB,&Fselect);
   XtAddCallback(mine,XmNokCallback,FileOkCB,&Fselect);
/*
.....Manage the File Selection Dialog
*/
   XtManageChild(mine);

/*
.....Loop until user selects a file
*/
   Fselect.sel = 0;
   do
   {
      XtAppNextEvent(Plot_App,&event);
      XtDispatchEvent(&event);
   } while (Fselect.sel==0);
/*
.....Return filename
*/
   *nc = strlen(Fselect.file);
done:;
	return;
}
/**********************************************************************
**    I_FUNCTION :  utp_mfmsg_box(parent, title, msg)
**       Creates and displays the message dialog.
**    PARAMETERS   
**       INPUT  : 
**          msg     = text to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Disables user interaction until the error
**				         message is acknowledged.
**    WARNINGS     : none
*********************************************************************/
utp_mfmsg_box(parent, title, msg)
Widget parent;
char *msg, *title;
{
	int n;
	Arg args[20];
	Widget dlg,but;
	XmString lstr;
	Position x,y,h,w;
/*
.....Bring up Message Dialog
*/
	n = 0;
	lstr = XmStringCreateLtoR(msg, XmSTRING_DEFAULT_CHARSET);
	XtSetArg(args[n],XmNmessageString,lstr); n++;
/*
........Disable user interaction until
........Error message is acknowleged
*/
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
	XtSetArg(args[n],XmNtitle,title); n++;
	if(parent==NULL)
		dlg = (Widget) XmCreateErrorDialog(Plot_Parent, "MSG",args,n);
	else
		dlg = (Widget) XmCreateErrorDialog(parent,"MSG",args,n);
	XtUnmanageChild((Widget)XmMessageBoxGetChild(dlg,XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild((Widget)XmMessageBoxGetChild(dlg,XmDIALOG_HELP_BUTTON));
	but = (Widget) XmMessageBoxGetChild(dlg,XmDIALOG_OK_BUTTON);
	XmStringFree(lstr);
	XtAddCallback(dlg,XmNokCallback,(XtPointer)XtDestroyWidget,(XtPointer) NULL);
	XtManageChild(dlg);
/*
....Warp cursor to OK Button
*/
	n = 0;
	XtSetArg(args[n],XmNx,&x); n++;
	XtSetArg(args[n],XmNy,&y); n++;
	XtSetArg(args[n],XmNwidth,&w); n++;
	XtSetArg(args[n],XmNheight,&h); n++;
	XtGetValues(but,args,n);
	x = x + w/2 ;
	y = y + h/2 ;
	XWarpPointer(XtDisplay(Plot_Parent),None,XtWindow(dlg),0,0,0,0,x,y);
}
#endif
