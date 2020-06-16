#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**  NAME:  uigmfapp.c
**
**    CONTAINS:
**				uig_mfstr_out(string,display)
**				static void FileOkCB
**				static void FileCancelCB
**				void do_search
**				uig_mf_filename
**				uig_mfmsg_box
**				static void yes_or_noCB
**				uig_mfyesno(parent, title, msg)
**				uig_mfprompt(parent, title, msg, ln, cols, ans_str)
**				void uig_mfdlg_okCB
**				uig_mflist_ans(parent, title, msg, list, ans, nc, flag)
**				void uig_mflist_okCB
**				void uig_mflist_cancelCB 
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       tigmfapp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:49
**
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:cursorfont.h>
#include <decw$include:keysym.h>
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
#include <decw$include:DialogS.h>
#include <decw$include:Protocols.h>
#include <decw$include:SelectioB.h>
#include <decw$include:FileSB.h>
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
#include <Xm/FileSB.h>
#include <Xm/SelectioB.h>
#include <Xm/DialogS.h>
#include <Xm/Protocols.h>
#include <sys/stat.h>
#include <stdio.h>
#endif

#include "tiges.h"
#include "tigmf.h"
#include "xenv1.h"

static int mfanswer;
static int yes_or_no;

typedef struct
{
   int sel;
   char *file;
} FSELECT;

typedef struct
{
   int sel;
   int *ans;
	int sel_num;
} LSELECT;

void uig_mfdlg_okCB(), uig_mfdlg_cancelCB();
void uig_mflist_okCB(), uig_mflist_cancelCB();


static Widget mine;
static Widget shell_prompt, text_prompt;
static Widget shell_list, sel_list;

/*********************************************************************
**    I_FUNCTION     :  uig_mfstr_out(string,display)
**          Output a string to main window's status area
**    PARAMETERS
**       INPUT  :
**          string                  string to output
**          display                 logical to set whether message is
**                                  displayed or just added to screen def.
**       OUTPUT :
**          option                  users selection
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_mfstr_out(string,display)
char string[];
int display;
{
	static int win_pos = 0;
	if (Iges_batch)
		return;

	XmTextInsert(tool_stat_win, win_pos, string);
	win_pos = win_pos + strlen(string);
	XmTextShowPosition(tool_stat_win, win_pos);
	XFlush(XtDisplay(tool_stat_win));
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
   XtDestroyWidget(widget);
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
   XtDestroyWidget(widget);
}

/**********************************************************************
**    I_FUNCTION :  do_search (widget,sdata)
**       Creates the FileName list for the File Selection Widget using
**       the specialized file mask (file1,file2,...,filen).
**    PARAMETERS
**       INPUT  :
**          widget = File selection widget.
**          sdata  = Motif callback structure.
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void do_search(widget, sdata)
Widget widget;
XtPointer sdata;
{
	XmString names[1024];
	char *list,*listhead;
	UX_pathname fname,direc,file;
	int nfile,i,nc;
	char *dir,*pattern;
   XmFileSelectionBoxCallbackStruct *cbs =
      (XmFileSelectionBoxCallbackStruct *)sdata;
	if (!XmStringGetLtoR(cbs->dir,XmSTRING_DEFAULT_CHARSET,&dir))
		return;
	strcpy(direc,dir);
	XtFree(dir);
	if (!XmStringGetLtoR(cbs->pattern,XmSTRING_DEFAULT_CHARSET,&pattern))
		return;
	strcpy(file,pattern);

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
			nc = strlen(file); ul_strip_blanks(file,&nc);
#else
			strcpy(file,direc); strcat(file,fname);
#endif
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
**    I_FUNCTION :  uig_mf_filename(title,filter,filename,nc)
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
uig_mf_filename(parent, title,filter,filename,nc)
Widget parent;
char *title,*filter,*filename;
int *nc;
{
   int i, n,jmpflag;
   void do_search();
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
   XtSetArg(args[n],XmNfileSearchProc,do_search); n++; 
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
   if(parent==NULL)
   {
      mine = (Widget) XmCreateFileSelectionDialog
         (TOOL_Parent,"file_select",args,n);
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
      XtAppNextEvent(TOOL_App,&event);
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
**    I_FUNCTION :  uig_mfmsg_box(parent, title, msg)
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
uig_mfmsg_box(parent, title, msg)
Widget parent;
char *msg, *title;
{
	int n;
	Arg args[20];
	Widget dlg,but;
	XmString lstr;
	Position x,y,h,w;
	if (Iges_batch)
		return;
/*
.....Bring up Message Dialog
*/
	n = 0;
#ifdef UU_RS6000
   lstr = XmStringCreateSimple(msg);
#else
	lstr = XmStringCreateLtoR(msg, XmFONTLIST_DEFAULT_TAG);
#endif
	XtSetArg(args[n],XmNmessageString,lstr); n++;
/*
........Disable user interaction until
........Error message is acknowleged
*/
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
	XtSetArg(args[n],XmNtitle,title); n++;
	if(parent==NULL)
		dlg = (Widget) XmCreateErrorDialog(TOOL_Parent, "MSG",args,n);
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
	XWarpPointer(XtDisplay(TOOL_Parent),None,XtWindow(dlg),0,0,0,0,x,y);
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
static void yes_or_noCB(w, client_data, call_data)
Widget w;
XtPointer client_data,call_data;
{
	XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)call_data;
	switch(cbs->reason)
	{
		case XmCR_OK:
			yes_or_no = 0;
			XtPopdown(XtParent(w));
			break;
		case XmCR_CANCEL:
			yes_or_no = 1;
			XtPopdown(XtParent(w));
			break;
	}
	mfanswer = 1;
}

/**********************************************************************
**    I_FUNCTION : uig_mfyesno(widget, title, msg)
**			popup a dialog box for yes or no choice
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**         			none 
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
uig_mfyesno(parent, title, msg)
Widget parent;
char *msg, *title;
{
	Widget dialog, no_button;
	XEvent x_event;
	Arg args[5];
	XmString m;
	XmString yes, no;
	int n  = 0;

	if (Iges_batch)
	{
/*
.....always return YES
*/
		return (0);
	}
	yes = XmStringCreateSimple("Yes");
	no = XmStringCreateSimple("No");
/*
.....default answer to "No"
*/
	yes_or_no = 1;
/*
...create a yesno dialog
*/
#ifdef UU_RS6000
   m = XmStringCreateSimple(msg);
#else
	m = XmStringCreateLtoR(msg, XmFONTLIST_DEFAULT_TAG);
#endif
	XtSetArg(args[n], XmNautoUnmanage, False); n++;
	XtSetArg(args[n], XmNmessageString, m); n++;
	XtSetArg(args[n], XmNokLabelString, yes); n++;
	XtSetArg(args[n], XmNcancelLabelString, no); n++;
	if(parent==UU_NULL)
	{
		dialog = (Widget) XmCreateQuestionDialog(TOOL_Parent, 
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
	mfanswer = 0;
	while (!mfanswer)
	{
		XtAppNextEvent(TOOL_App, &x_event);
		XtDispatchEvent(&x_event);
	}
	return yes_or_no;
}



/**********************************************************************
**    I_FUNCTION : uig_mfprompt(parent, title, msg, ln, cols, ans_str)
**			popup a prompt dialog box (it can be mutiple lines
**			 in text field)
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent widget
**				title:  prompt box title
**				msg:    prompt str.
**				ln: how many lines in text fields.
**				str_ans: user input in text field.
**       OUTPUT :  
**         			none 
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
uig_mfprompt(parent, title, msg, ln, cols, ans_str)
Widget parent;
char *msg, *title, *ans_str;
int ln, cols;
{
#define TIGHTNESS 40

	Widget pane_prompt, form_prompt, action_area, label_prompt ;
	int n, ifl;
	Arg args[20];
   FSELECT Fselect;
	Widget okbut, cancelbut;
	XEvent x_event;

	if (Iges_batch)
	{
		strcpy(ans_str, "Batch: should not come here");
		return (0);
	}
	n = 0;
	ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
            MWM_DECOR_MINIMIZE;
   XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
	ifl = MWM_FUNC_ALL | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE |
            MWM_FUNC_CLOSE;
   XtSetArg(args[n],XmNmwmFunctions,ifl); n++;


	XtSetArg(args[n],XmNtitle,title); n++;
	shell_prompt = XtCreatePopupShell("prompt window",
						xmDialogShellWidgetClass, TOOL_Parent, args,n);
	pane_prompt = XtVaCreateWidget("pane", xmPanedWindowWidgetClass,
						shell_prompt,  XmNsashWidth,1,XmNsashHeight,1,NULL);
/*
.....Create a Form widget
*/
      n = 0;
	XtSetArg(args[n],XmNfractionBase,(ln+2)*cols); n++;
	form_prompt = XtCreateWidget("form", 
				xmFormWidgetClass,pane_prompt,args,n);
	
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
	label_prompt = XtCreateManagedWidget(msg, xmLabelWidgetClass,
							form_prompt, args,n);

/*
.....Place scrolling text widget in dialog
*/
	n = 0;
	XtSetArg(args[n], XmNrows, ln); n++;
	XtSetArg(args[n], XmNcolumns, cols); n++;
	XtSetArg(args[n], XmNresizeWidth, True); n++;
	XtSetArg(args[n], XmNresizeHeight, True); n++;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNtopWidget, label_prompt); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
	if (ln>1)
	{
		XtSetArg(args[n], XmNscrollVertical, True); n++;
		XtSetArg(args[n], XmNscrollHorizontal, False); n++;
		XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
	}
	XtSetArg(args[n], XmNeditable, True); n++;
	if (ln>1)
		text_prompt = XmCreateScrolledText(form_prompt, "prompt dialog",
						args,n);
	else
		text_prompt = XmCreateText(form_prompt, "prompt dialog",
						args,n);
	XmTextSetString(text_prompt,"");
	XtManageChild(text_prompt);

	action_area = XtVaCreateWidget("action_area",
						xmFormWidgetClass,pane_prompt,
						XmNfractionBase, 5*TIGHTNESS-1,
						XmNleftOffset,30,
						XmNrightOffset,30,
						NULL);
	okbut = XtVaCreateManagedWidget("OK", 
					xmPushButtonWidgetClass, action_area,
					XmNleftAttachment, XmATTACH_POSITION,
					XmNleftPosition, TIGHTNESS,
					XmNtopAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNrightAttachment,  XmATTACH_POSITION,
					XmNrightPosition, TIGHTNESS * 1 + (TIGHTNESS - 1),
					XmNdefaultButtonShadowThickness, 1,
					NULL);
	XtAddCallback(okbut, XmNactivateCallback,
					uig_mfdlg_okCB, (XtPointer)&Fselect);	
	
	cancelbut = XtVaCreateManagedWidget("CANCEL", 
					xmPushButtonWidgetClass, action_area,
					XmNleftAttachment, XmATTACH_POSITION,
					XmNleftPosition, TIGHTNESS*3,
					XmNtopAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNrightAttachment,  XmATTACH_POSITION,
					XmNrightPosition, TIGHTNESS * 3 + (TIGHTNESS - 1),
					XmNdefaultButtonShadowThickness, 1,
					NULL);
	XtAddCallback(cancelbut, XmNactivateCallback,
					uig_mfdlg_cancelCB, (XtPointer)&Fselect);

	XtManageChild(form_prompt);
	XtManageChild(action_area);
	XtManageChild(pane_prompt);
	XtManageChild(shell_prompt);
   Fselect.file = ans_str;
   Fselect.sel = 0;
	while (!Fselect.sel)
	{
		XtAppNextEvent(TOOL_App, &x_event);
		XtDispatchEvent(&x_event);
	}
}


/**********************************************************************
**    I_FUNCTION : uig_mfdlg_okCB(widget, client_data, call_data) 
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
void uig_mfdlg_okCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
   char *text;
   FSELECT *p=(FSELECT *)client_data;
	text = XmTextGetString(text_prompt);
	if(text[0]!='\0')
		strcpy(p->file, text);
	else
		p->file[0] = '\0';

   p->sel = 1;
	XtUnmanageChild(shell_prompt);
	XtDestroyWidget(shell_prompt);
	XmUpdateDisplay(shell_prompt);
}

/**********************************************************************
**    I_FUNCTION : uig_mfdlg_cancelCB(widget, client_data, call_data)
**       Processes the CANCEL button in the prompt dialog.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
void uig_mfdlg_cancelCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
   FSELECT *p=(FSELECT *)client_data;
   p->sel = 1;
	XtUnmanageChild(shell_prompt);
	XtDestroyWidget(shell_prompt);
	XmUpdateDisplay(shell_prompt);
}
/**********************************************************************
**    I_FUNCTION : uig_mflist_ans(parent, title, msg, list, ans, nc)
**			popup a prompt dialog box (it can be mutiple lines
**			 in text field)
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent widget
**				title:  prompt box title
**				msg:    prompt str.
**				ln: how many lines in text fields.
**				str_ans: user input in text field.
**       OUTPUT :  
**         			none 
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
uig_mflist_ans(parent, title, msg, list, ans, nc, flag)
Widget parent;
char *msg, *title, **list;
int *ans, *nc;
int flag;
{
#define TIGHTNESS 40

	Widget pane_list, form_list, action_area, label_list, form_lin,
frame_list ;
   XmString xmstring;
	char line[80], msg1[80], *str, msg2[400];
	int i, n, ifl, cols, len , rows1;
	Arg args[20];
   LSELECT Lselect;
	Widget okbut, cancelbut;
	XEvent x_event;
/*
.....if batch, get no selection as answer
*/
	if (Iges_batch)
	{
		*nc = 0;
		return 0;
	}
	n = 0;
	ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
            MWM_DECOR_MINIMIZE;
   XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
	ifl = MWM_FUNC_ALL | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE |
            MWM_FUNC_CLOSE;
   XtSetArg(args[n],XmNmwmFunctions,ifl); n++;


	XtSetArg(args[n],XmNtitle,title); n++;
	shell_list = XtCreatePopupShell("prompt window",
						xmDialogShellWidgetClass, TOOL_Parent, args,n);
	pane_list = XtVaCreateWidget("pane", xmPanedWindowWidgetClass,
						shell_list,  XmNsashWidth,1,XmNsashHeight,1,NULL);
/*
.....Create a Form widget
*/
/*
.....calculate lines of label
*/
	i = 0;
	len = 0;
	strcpy(msg2, msg);
	while(1)
	{
		str = msg1;
		if (i==0)
			str = strtok(msg2, "\n\0");
		else
			str = strtok(NULL, "\n\0");
		if (str==NULL) break;
		if (len<strlen (str))
			len = strlen (str);
		i++;
	}
   n = 0;
	cols = len;
	XtSetArg(args[n],XmNfractionBase,(i+8)*len); n++; 
	form_list = XtCreateWidget("form", 
				xmFormWidgetClass,pane_list,args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, (i+1)*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	frame_list = XtCreateManagedWidget("frame", xmFrameWidgetClass, 
							form_list,args,n);
	rows1 = i;
	n = 0;
	XtSetArg(args[n],XmNfractionBase,rows1*cols); n++;
	form_lin = XtCreateManagedWidget("form_in", xmFormWidgetClass, frame_list,
							args,n);
	i = 0;
	while(1)
	{
		str = msg1;
		if (i==0)
			str = strtok(msg, "\n\0");
		else
			str = strtok(NULL, "\n\0");
		if (str==NULL) break;
		i++;
		if (msg1[0]=='\0') continue;
		n = 0;
		XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNtopPosition, (i-1)*cols); n++;
		XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNbottomPosition, i*cols); n++;
		XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
		label_list = XtCreateManagedWidget(str, xmLabelWidgetClass,
							form_lin, args,n);
	}

/*
.....Place scrolling list widget in dialog
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,  (rows1+2)*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
	if (flag==1)
	{
		XtSetArg(args[n], XmNselectionPolicy, XmSINGLE_SELECT); n++;
	}
	sel_list = (Widget)XmCreateScrolledList(form_list, "sel_list", args, n);
	for(i = 0; i<*nc; i++)
	{
		strcpy(line, list[i]);
		xmstring = XmStringCreateSimple(line);
		XmListAddItem(sel_list, xmstring, 0);
		XmStringFree(xmstring);
	}
	XmListSelectPos(sel_list, 1, True);
	XtManageChild(sel_list);

	action_area = XtVaCreateWidget("action_area",
						xmFormWidgetClass,pane_list,
						XmNfractionBase, 5*TIGHTNESS-1,
						XmNleftOffset,30,
						XmNrightOffset,30,
						NULL);
	okbut = XtVaCreateManagedWidget("OK", 
					xmPushButtonWidgetClass, action_area,
					XmNleftAttachment, XmATTACH_POSITION,
					XmNleftPosition, TIGHTNESS,
					XmNtopAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNrightAttachment,  XmATTACH_POSITION,
					XmNrightPosition, TIGHTNESS * 1 + (TIGHTNESS - 1),
					XmNdefaultButtonShadowThickness, 1,
					NULL);
	XtAddCallback(okbut, XmNactivateCallback,
					uig_mflist_okCB, (XtPointer)&Lselect);	
	
	cancelbut = XtVaCreateManagedWidget("CANCEL", 
					xmPushButtonWidgetClass, action_area,
					XmNleftAttachment, XmATTACH_POSITION,
					XmNleftPosition, TIGHTNESS*3,
					XmNtopAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNrightAttachment,  XmATTACH_POSITION,
					XmNrightPosition, TIGHTNESS * 3 + (TIGHTNESS - 1),
					XmNdefaultButtonShadowThickness, 1,
					NULL);
	XtAddCallback(cancelbut, XmNactivateCallback,
					uig_mflist_cancelCB, (XtPointer)&Lselect);

	XtManageChild(form_list);
	XtManageChild(action_area);
	XtManageChild(pane_list);
	XtManageChild(shell_list);
   Lselect.ans = ans;
	Lselect.sel_num = 0;
   Lselect.sel = 0;
	while (!Lselect.sel)
	{
		XtAppNextEvent(TOOL_App, &x_event);
		XtDispatchEvent(&x_event);
	}
	*nc = Lselect.sel_num;
	for (i=0; i<*nc; i++)
		ans[i] = Lselect.ans[i];
}


/**********************************************************************
**    I_FUNCTION : uig_mflist_okCB(widget, client_data, call_data) 
**       Processes the OK button in the List Selection dialog.  Returns
**       the positon selected by the user.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
void uig_mflist_okCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
   LSELECT *p=(LSELECT *)client_data;
	XmListGetSelectedPos(sel_list, &(p->ans), &(p->sel_num));
   p->sel = 1;
	XtUnmanageChild(shell_list);
	XtDestroyWidget(shell_list);
	XmUpdateDisplay(shell_list);
}
/**********************************************************************
**    I_FUNCTION : uig_mflist_cancelCB(widget, client_data, call_data) 
**       Processes the cancel button in the List Selection dialog. 
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
void uig_mflist_cancelCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
   LSELECT *p=(LSELECT *)client_data;
   p->sel = 1;
	p->sel_num = 0;
	XtUnmanageChild(shell_list);
	XtDestroyWidget(shell_list);
	XmUpdateDisplay(shell_list);
}
#endif
