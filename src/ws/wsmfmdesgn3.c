#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**      FILENAME: wsmenudesgn3.c
**      CONTAINS: uw_mfmd_cancelCB
**             uw_mfCreatePushbutton
**             uw_mfpmenu_menu
**             uw_mfpmenu_desc
**             uw_mfreadmenu
**             uw_mfmd_loadfileCB
**             uw_mfmessage
**             yes_or_noCB
**             uw_mfanswer
**             uw_mfget_savemenu
**             uw_mfcheck_and_save
**             uw_mfcheck_and_load
**             uw_mfsavemenu
**             uw_mfmd_acceptCB
**             uw_mfmd_savefileCB
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wsmfmdesgn3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:12
*********************************************************************/

#ifdef VMS
#include <decw$include:Xm.h>
#include <decw$include:Label.h>
#include <decw$include:PushB.h>
#include <decw$include:RowColumn.h>
#include <decw$include:MessageB.h>
#include <decw$include:SelectioB.h>
#include <decw$include:Frame.h>
#include <decw$include:ToggleB.h>
#else
#include <Xm/Xm.h>
#include<Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/PanedW.h>
#include <Xm/MwmUtil.h>
#include <Xm/DrawingA.h>
#include <Xm/BulletinB.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowB.h>
#include <Xm/CascadeBG.h>
#include <Xm/LabelG.h>
#include <Xm/Protocols.h>
#include <Xm/DialogS.h>
#endif
#include "ustdio.h"
#include "xfsys1.h"
#include "uhep.h"
#include "xenv1.h"
#include "wsmf.h"
#include "dmotif.h"
#include "usysdef.h"
#include "wsxw.h"
#include "wsmfmdesgn.h"
/*
.....added by Yurong
.....8/27/97
*/
extern UWS_MF uw_mf;
extern UWS_MFLAYOUT uw_mflayout;
extern int XBorder, YBorder;
extern int UDM_menu_mapped[UDM_MAX_MENU];
extern char *uu_malloc();
/*
.....variable for chech if need close
.....menu design window in main loop
.....Yurong 4/22/99
*/
int UW_close_mdsgn = 0;
/*
.....added flag just for time reason
.....we must finish one loading before
.....we loading another menu or close
.....menu desgn window
.....Yurong 4/20/99
*/
static int UW_smenu_loaded = 1;

void uw_mfmessage();
void uw_mfget_savemenu();

/**********************************************************************
**    I_FUNCTION : uw_mfmd_cancelCB(widget, client_data, call_data)
**                      action for cancel button.terminate the menu design selection
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**                              client_data = Ignored.
**                              call_data   = Ignored
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfmd_cancelCB(widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;

{
   int i;
/*
.....wait until menu load finish
.....before we free memory (the loading
.....will alloc memory)
*/
	while (UW_smenu_loaded==0) ;
/*
...Destroy the design windows
*/
/*
.....if we close & Destroy menu design window here
.....on same system (Maybe most because time matter)
.....the active menu we just changed and put back will
.....no longer active. Don't destroy menu design window
.....here, only set a UW_close_mdsgn flag to let
.....uw_mfmainloop Destroy them
.....This is for fix problem on IRIX64 octane 6.4
.....Yurong 4/22/99
*/
/*
	XtUnrealizeWidget(menu_win);
	XtUnrealizeWidget(desgn_win);
	XtUnrealizeWidget(funlist_win);

	XtDestroyWidget(menu_win);
	XtDestroyWidget(desgn_win);
	XtDestroyWidget(funlist_win);
	menu_win = NULL;
	desgn_win = NULL;
	funlist_win = NULL;
*/
/*
...Reset takedown active menu
*/
	if (takedown != -1)
	{
		UDM_menu[takedown].pos[0] = UDM_layout.menu_pos[takedown][0] ;
		UDM_menu[takedown].pos[1] = UDM_layout.menu_pos[takedown][1];
		UDM_menu[takedown].size[0] = UDM_layout.menu_size[takedown][0];
		UDM_menu[takedown].size[1] = UDM_layout.menu_size[takedown][1];
		UDM_menu_design = 1;
		uw_mfmenu(takedown, 1);
		UDM_menu_design = 0;

	}
	mfirst = 1;
	takedown = -1;
	menu_changed = 0;
	menu_first = 1;
	UDM_menu_design = 0;
/*
.....free toggle memery
*/
	for (i=0; i<UDM_MAX_MENU; i++)
	{
		if (UW_menu_item[i].toggle!=NULL)
		{
			uu_free(UW_menu_item[i].toggle);
			UW_menu_item[i].toggle = 0;
		}
	}

	UW_close_mdsgn = 1;
	UW_smenu_loaded = 1;
}

/*********************************************************************
**       I_FUNCTION : uw_mfinit_toggle(ctyp,cmsg) 
**                      This function allocates memory for the menu toogle
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uw_mfinit_toggle(ctyp,cmsg)
char *ctyp,*cmsg;
{
	int status;
	status = UU_SUCCESS;
	strcpy(UW_menu_item[menu_count].toggle_def,ctyp);
	UW_menu_item[menu_count].toggle = (UDM_menu_toggle_struc *)malloc(
								sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
	if (UW_menu_item[menu_count].toggle == NULL) goto failed;
	UW_menu_item[menu_count].toggle_num = 0;
	goto done;
failed:;
	status = UU_FAILURE;
done:
	return(status);
}

/*********************************************************************
**       I_FUNCTION : uw_mfpmenu_toggle(ctyp,cmsg) 
**                      This function parses a menu file record.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uw_mfpmenu_toggle (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int tog_num,nc;
	char *p;        
	char buf[80];
	tog_num = UW_menu_item[menu_count].toggle_num;
	strcpy(UW_menu_item[menu_count].toggle[tog_num].label,ctyp);
/*
.....added function parameter
*/
	p = (char *)index(cmsg,',');
	if (p != 0 && p != cmsg)
	{
		*p = '\0';
		++p;
		while ((*p==' ') || (*p=='\t')) p++;
		strcpy(buf, p);
		ul_remove_quotes(buf);
		nc = strlen(buf);
		UW_menu_item[menu_count].toggle[tog_num].params = uu_malloc((nc+1)*sizeof(char));
		strcpy(UW_menu_item[menu_count].toggle[tog_num].params,buf);
	}
	else
		UW_menu_item[menu_count].toggle[tog_num].params = NULL;
	strcpy(UW_menu_item[menu_count].toggle[tog_num].func,cmsg);

	UW_menu_item[menu_count].toggle_num++;
/*
.....added for color
*/
	UW_menu_item[menu_count].bgcolor[0] = '\0';
	UW_menu_item[menu_count].color[0] = -1;
	UW_menu_item[menu_count].color[1] = -1;
	UW_menu_item[menu_count].color[2] = -1;
	return 0;
}

void uw_mfpmenu_color(ctyp, cmsg)
char *ctyp,*cmsg;
{
	strcpy(UW_menu_item[menu_count-1].bgcolor, cmsg);
}

/*********************************************************************
**       I_FUNCTION : uw_mfpmenu_menu(ctyp,cmsg) 
**                      This function parses a menu file record.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void uw_mfpmenu_menu (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char *p, buf[80];
	int nc;
	strcpy(UW_menu_item[menu_count].name,ctyp);
	UW_menu_item[menu_count].params = NULL;
	p = (char *)index(cmsg,',');
	if (p != 0 && p != cmsg)
	{
		*p = '\0';
		++p;
		while ((*p==' ') || (*p=='\t')) p++;
		strcpy(buf, p);
		ul_remove_quotes(buf);
		nc = strlen (buf);
		UW_menu_item[menu_count].params = uu_malloc((nc+1)*sizeof(char));
		strcpy(UW_menu_item[menu_count].params,buf);
	}

	strcpy(UW_menu_item[menu_count].file,cmsg);
	UW_menu_item[menu_count].bgcolor[0] = '\0';
	UW_menu_item[menu_count].color[0] = -1;
	UW_menu_item[menu_count].color[1] = -1;
	UW_menu_item[menu_count].color[2] = -1;
	menu_count++;
}

void uw_mfdsn_minc()
{
	menu_count++;
}

/*********************************************************************
**       I_FUNCTION : uw_mfpmenu_desc(ctyp,cmsg) follow udm_pmenu_desc
**                      This function parses a menu file record.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uw_mfpmenu_desc (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,inc,status,inum;
	int maxsub=6;
	char num[200];
	static char csub[6][20] = {"NAME","POSITION","ROWS","COLS","TYPE",
		"SIZE"};
	static char ltype[3][10] = {"ICON","MENU","POPUP"};
	UU_REAL rval[2];
/*
.....Initialize routine
*/
	inc = UDM_menu_count;
	status = UU_SUCCESS;
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		uw_mfmessage(menu_form, "There are bad parameter in Menu file!");
		return UU_FAILURE;
	}
	switch(i)
	{
/*
.....NAME
*/
	case 0:
		strcpy(UMM_menu.name,cmsg);
		XmTextSetString(title_area, cmsg);
		break;
/*
.....POSITION
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) 
			goto bad_parm;
		if(UMM_menu.pos[0]==-1)
		{
			UMM_menu.pos[0] = rval[0] * uw_xw.dev_xmax;
			UMM_menu.pos[1] = rval[1] * uw_xw.dev_ymax;
		}
		sprintf(num, "%d", UMM_menu.pos[0]);
		XmTextSetString(pos0, num);
		sprintf(num, "%d", UMM_menu.pos[1]);
		XmTextSetString(pos1, num);
		break;
/*
.....ROWS
*/
	case 2:
		if ((ul_to_number(cmsg,&inum) != UU_SUCCESS) ||
			inum < 1) goto bad_parm;
		UMM_menu.rows = inum;
		sprintf(num, "%d", UMM_menu.rows);
		XmTextSetString(row_area, num);
		break;
/*
.....COLUMNS
*/
	case 3:
		if ((ul_to_number(cmsg,&inum) != UU_SUCCESS) ||
			inum < 1) goto bad_parm;
		UMM_menu.cols = inum;
		sprintf(num, "%d", UMM_menu.cols);
		XmTextSetString(col_area, num);
		break;
/*
.....TYPE
*/
	case 4:
		if (ul_modal_toggle(cmsg,ltype,3,&UMM_menu.type) != UU_SUCCESS)
			goto bad_parm;
/*
.....change following code because menu_desgn form changed
.....Yurong 12/17/97
*/
/*
.....set old menu type to false
*/
		if (uw_oldmenu_type==1)
			XmToggleButtonSetState(titled_opt, False, False);
		else if (uw_oldmenu_type==2)
			XmToggleButtonSetState(icon_opt, False, False);
		else
			XmToggleButtonSetState(popup_opt, False, False);
/*
.....set new value
*/
		if(UMM_menu.type==1)
			XmToggleButtonSetState(titled_opt, True, False);
/*                      XtVaSetValues(option, XmNmenuHistory, titled_opt, NULL); */
		else if(UMM_menu.type==0)
			XmToggleButtonSetState(icon_opt, True, False);
/*                      XtVaSetValues(option, XmNmenuHistory, icon_opt, NULL); */
		else
			XmToggleButtonSetState(popup_opt, True, False);
/*                      XtVaSetValues(option, XmNmenuHistory, popup_opt, NULL); */
		break;
/*
.....SIZE
*/
	case 5:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		if(UMM_menu.size[0] == -1)
		{
			UMM_menu.size[0] = rval[0] * uw_xw.dev_xmax;
			UMM_menu.size[1] = rval[1] * uw_xw.dev_ymax;
		}
		sprintf(num, "%d", UMM_menu.size[0]);
		XmTextSetString(size0, num);
		sprintf(num, "%d", UMM_menu.size[1]);
		XmTextSetString(size1, num);
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	uw_mfmessage(menu_form, "There are bad data in the Menu file!");
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/**********************************************************************
**    I_FUNCTION : uw_mfreadmenu()
**              read menu file  
**    PARAMETERS   
**       INPUT  : 
**                      none 
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_mfreadmenu()
{
	UX_pathname file1;
	char *file;
	int i,j, status, stat;
	stat = UU_SUCCESS;
	file = file1;
/*
.....Now, We include path in the file when they are saved in UDM_menu[i].file
.....so we need whole filename here. removed following code
.....12/15/97 Yurong
*/
	strcpy(file, UMM_menu.file);
/*
...Search for the requested menu
*/
	for (i=0;i<UDM_menu_count;i++)
	{
/*
...If it's up, let's take it down
*/
		if (strcmp(UDM_menu[i].file,file) == 0 &&
		    uw_mflayout.menu_app[i] != NULL)
		{
			XtUnmapWidget(uw_mflayout.menu_app[i]);
			XtUnrealizeWidget(uw_mflayout.menu_app[i]);
			XtDestroyWidget(uw_mflayout.menu_app[i]);
			uw_mflayout.menu_app[i] = NULL;
			strcpy(takemenu, file);
			takedown = i;
			UDM_menu_mapped[i] = 0;
			break;
		}
	}
/*
.....free toggle memer from before if have 
*/
	for (i=0; i<UDM_MAX_MENU; i++)
	{
		if (UW_menu_item[i].toggle!=NULL)
		{
			uu_free(UW_menu_item[i].toggle);
			UW_menu_item[i].toggle = 0;
		}
	}
/*
...initialize menu item
*/
	menu_count = 0;
	for(j = 0; j< UDM_MAX_MENU; j++)
	{
		strcpy(UW_menu_item[j].name," ");
		strcpy(UW_menu_item[j].file," ");
		UW_menu_item[j].params = NULL;
		UW_menu_item[j].toggle_num = 0;
		UW_menu_item[j].toggle = 0;
	}
/*
...remember the active menu position and size
*/
	if(takedown!= -1)
	{
		UMM_menu.pos[0] = UDM_layout.menu_pos[takedown][0];
		UMM_menu.pos[1] = UDM_layout.menu_pos[takedown][1];
		UMM_menu.size[0] = UDM_layout.menu_size[takedown][0];
		UMM_menu.size[1] = UDM_layout.menu_size[takedown][1];
		UDM_menu_design = 1;
		status = udm_read_menu(UMM_menu.file, UMM_menu.pos,
								UMM_menu.size, 0, 1, -1);
		UDM_menu_design = 0;
	}
	else
	{
/*
...Need reset the size and position from menu file
*/
		UMM_menu.pos[0] = -1;
		UMM_menu.pos[1] = -1;
		UMM_menu.size[0] = -1;
		UMM_menu.size[1] = -1;
		UDM_menu_design = 1;
		status = udm_read_menu(UMM_menu.file, UMM_menu.pos, UMM_menu.size,0, 1, -1);
		UDM_menu_design = 0;
	}
	uw_oldmenu_type = UMM_menu.type;
done:;
	menu_changed = 0;
	menu_first = 1;
/*
...reset menu
*/
	if(status == UU_SUCCESS)
		uw_mfresetmenu();
	return status;
}
			

/**********************************************************************
**    I_FUNCTION : uw_mfmessage(widget,msg)
**              Display a message       
**    PARAMETERS   
**       INPUT  : 
**          widget      = parent of message dialog 
**          msg:        = message displayed
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfmessage(widget,msg)
Widget widget;
char *msg;
{
	Widget dialog;
	XmString t;
	Arg args[5];
	int n ;
	t = XmStringCreateSimple(msg);
	if(widget==(Widget)NULL)
		widget = menu_form;
/*
...Create the dialog
*/
	n = 0;
	XtSetArg(args[n], XmNmessageString, t); n++;
	dialog = (Widget)XmCreateMessageDialog(widget,"message",  args, n);
	XtVaSetValues(XtParent(dialog),XmNtitle,"Errors",NULL);
	XmStringFree(t);
/*
...no help used
*/
	XtUnmanageChild((Widget)XmMessageBoxGetChild((Widget)dialog,XmDIALOG_HELP_BUTTON)); 
/*
...no cancel used
*/
	XtUnmanageChild((Widget)XmMessageBoxGetChild((Widget)dialog,XmDIALOG_CANCEL_BUTTON)); 
	XtManageChild(dialog);
}       

/**********************************************************************
**    I_FUNCTION : yes_or_noCB()
**                      action for yes or no choice
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**                              client_data = Ignored.
**                              call_data   = Motif callback structure.  Contains 
**                                            answer selected by the user.
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void yes_or_noCB(w, client_data, call_data)
Widget w;
XtPointer client_data,call_data;
{
	int status;
	UX_pathname filename;
	XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)call_data;
	switch(cbs->reason)
	{
		case XmCR_OK:
/*
...call from "if overwrite the exit file?"
*/
			if(strcmp((char*)client_data,"SAVEFILE")==0)
			{
				status = ux_delete(save_filename, UX_PRTERRS);
				if (status == 0)
					status = uw_mfsavemenu(save_filename);              
				if (status != 0) 
				{
					XtPopdown(XtParent(w));
					break;
				}
				menu_changed  = 0;
				menu_first = 1;
/*
...if it from accept button
...we will get out of design windows
*/
/*temp
				if(menu_accept == 1)
				{
					XtUnrealizeWidget(menu_win);
					XtUnrealizeWidget(desgn_win);
					XtUnrealizeWidget(funlist_win);

					XtDestroyWidget(menu_win);
					XtDestroyWidget(desgn_win);
					XtDestroyWidget(funlist_win);
					menu_win = NULL;
					desgn_win = NULL;
					funlist_win = NULL;
					mfirst = 1;
					menu_accept = 0;
					takedown = -1;
				}
				else
				{
*/
					strcpy(UMM_menu.file, (char*)load_filename);
					takedown = -1;
					uw_mfcheck_and_load();
/*                              }  */
				XtPopdown(XtParent(w));
			}
			if(strcmp((char*)client_data,"LOAD_SAVE")!=0)
				break;
			uw_mfget_savemenu(w, filename);
			break;
		case XmCR_CANCEL:
/*
...This is for answer no
*/
			XtPopdown(XtParent(w));
			if(strcmp((char*)client_data, "LOAD_SAVE")!=0)
				break;
/*
...if currect sample file is taken from active menu, put it back
*/
			if (takedown != -1)
			{
				UDM_menu[takedown].pos[0] = UDM_layout.menu_pos[takedown][0];
				UDM_menu[takedown].pos[1] = UDM_layout.menu_pos[takedown][1];
				UDM_menu[takedown].size[0] = UDM_layout.menu_size[takedown][0];
				UDM_menu[takedown].size[1] = UDM_layout.menu_size[takedown][1];
				UDM_menu_design = 1;
				uw_mfmenu(takedown, 1);
				UDM_menu_design = 0;
			}
			takedown = -1;
			
			strcpy(UMM_menu.file, (char*)load_filename);
			uw_mfcheck_and_load();
			break;
		case XmCR_HELP:
/*
...This is for answer cancel
*/
			XtPopdown(XtParent(w));
			break;
	}
}

/**********************************************************************
**    I_FUNCTION : uw_mfanswer(title, msg, use)
**                      create a question dialog.
**    PARAMETERS   
**       INPUT  : 
**              title: title of dialog
**              msg:   message displayed in dialog
**              use:   from where it call
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void uw_mfanswer(title, msg, use)
char *title, *msg, *use;
{
	Widget dialog;
	Arg args[5];
	int n  = 0;
/*
...create a yesno dialog
*/
	XmString m = XmStringCreateSimple(msg);
	XmString yes = XmStringCreateSimple("Yes");
	XmString no = XmStringCreateSimple("No");
	XmString cancel = XmStringCreateSimple("Cancel");
	XtSetArg(args[n], XmNautoUnmanage, False); n++;
	XtSetArg(args[n], XmNmessageString, m); n++;
	XtSetArg(args[n], XmNokLabelString, yes); n++;
	XtSetArg(args[n], XmNcancelLabelString, no); n++;
	if(strcmp(use,"LOAD_SAVE")==0)
/*
...if use for load_save, change help to cancel
...otherwise, no help used
*/
	{
		XtSetArg(args[n], XmNhelpLabelString, cancel); n++;
	}
	dialog = (Widget)XmCreateQuestionDialog(menu_form, title, args, n);
	XtVaSetValues(XtParent(dialog),XmNtitle,title,NULL);
/*
...callback
*/
	XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)yes_or_noCB, use);
	XtAddCallback(dialog, XmNcancelCallback, yes_or_noCB,use);
	if(strcmp(use,"LOAD_SAVE")==0)
		XtAddCallback(dialog, XmNhelpCallback, yes_or_noCB,use);
	XmStringFree(m);
	XmStringFree(no);
	XmStringFree(yes);
/*
...if use for load_save, change help to cancel
...otherwise, no help used
*/
	if(strcmp(use,"LOAD_SAVE")!=0)
	{
		XtUnmanageChild((Widget)XmMessageBoxGetChild((Widget)dialog,XmDIALOG_HELP_BUTTON)); 
	}
	else
		XmStringFree(cancel);
	XtManageChild(dialog);
	XtPopup(XtParent(dialog), XtGrabNone);
}

/**********************************************************************
**    I_FUNCTION : uw_mfmd_savefileCB(widget, client_data, call_data)
**                      callback for load button
**       check file and read in
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**                              client_data = Ignored.
**                              call_data   = Ignored
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfmd_savefileCB(widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;

{
	int len, answer, status, i, k;
	UX_pathname filename,dir2;
	FILE *fptr;
	static int browse_open = 0;

	static int nfld=3,ifld=0;
	static char *ftext[3]={"System","Local","File Tree"};
	static char ldir[3][UX_MAX_PATH_LEN]; /*={"NCL_MENU","NCL_USER_MENU","."}; */
	static char *fdir[3]={ldir[0],ldir[1],ldir[2]};

	if (browse_open==1)
		return;
	browse_open = 1;
/*
.....add for default filename
.....Yurong 2/4/97
*/
	strcpy(filename, UMM_menu.file);
	strcpy(ldir[0],"NCL_MENU");
	dir2[0] = '\0';
	status = ul_open_mod_file("UU_USER_SETTINGS", "menu", UU_NULL, UU_NULL, 
		dir2, 1, UU_NULL);
	if (status==UU_SUCCESS)
		strcpy(ldir[1], dir2);
	else
		ldir[1][0] = '\0';

	strcpy(ldir[2], ".");

	uw_mfmd_filename(widget, "Menu_select", "*.menu", nfld, &ifld,
		ftext, fdir, filename, &len, NULL);
	if (len!=0)
	{
/*
.....open for read to check if file exist
*/
		status = ul_open_mod_file("UU_USER_SETTINGS", "menu", "NCL_MENU", UU_NULL,
			filename, 0, UU_NULL);
		if (status==0)
		{
/*
...file exists, overwrite (y/n)?
*/
			answer = uw_mfyes_or_no(widget, "File exists, Overwrite?", "Save Menu");
			if (answer==0)
			{
				browse_open = 0;
				return;
			}
			status = ux_delete(filename, UX_PRTERRS);
		}
		else
/*
......file not exist, open for writing (will create the file)
*/
		{
			status = ul_open_mod_file("UU_USER_SETTINGS", "menu", "NCL_MENU", UU_NULL,
				filename, 3, &fptr);
			if (status!=UU_SUCCESS) goto done;
			ux_fclose0 (fptr);
		}
		status = uw_mfmd_writemenu(filename);
		if (status==0) 
		{
/*
.....check if this menu has already loaded
.....if load it save it in loaded menu structure
*/
			for (i=0;i<UDM_menu_count;i++)
			{
				if (strcmp(UDM_menu[i].file,filename) == 0)
				{
					strcpy(UDM_menu[i].name, UMM_menu.name);
					UDM_menu[i].type = UMM_menu.type;
					UDM_menu[i].cols = UMM_menu.cols;
					UDM_menu[i].rows = UMM_menu.rows;
					UDM_menu[i].pos[0] = UMM_menu.pos[0];
					UDM_menu[i].pos[1] = UMM_menu.pos[1];
					UDM_menu[i].size[0] = UMM_menu.size[0];
					UDM_menu[i].size[1] = UMM_menu.size[1];
					UDM_layout.menu_pos[i][0] = UDM_menu[i].pos[0];
					UDM_layout.menu_pos[i][1] = UDM_menu[i].pos[1];
					UDM_layout.menu_size[i][0] = UDM_menu[i].size[0];
					UDM_layout.menu_size[i][1] = UDM_menu[i].size[1];
					for(k = 0; k < UMM_menu.cols*UMM_menu.rows; k++)
					{
						strcpy(UDM_menu[i].menus[k].name, UW_menu_item[k].name);
						strcpy(UDM_menu[i].menus[k].file, UW_menu_item[k].file);
					}
					break;
				}
			}
/*
.....check if this menu is active before, if it is,
.....change it and load back
*/
			if (i == takedown)
			{
				UDM_menu_design = 1;
				uw_mfmenu(i, 1);
				UDM_menu_design = 0;
				takedown = -1;
			}
/*
.....check if this menu is active now, if it is
.....destroy the old one load the new one
*/
			else if (uw_mflayout.menu_app[i]!=NULL)
			{
				XtUnmapWidget(uw_mflayout.menu_app[i]);
				XtUnrealizeWidget(uw_mflayout.menu_app[i]);
				XtDestroyWidget(uw_mflayout.menu_app[i]);
				uw_mflayout.menu_app[i] = NULL;
				UDM_menu_design = 1;
				uw_mfmenu(i, 1);
				UDM_menu_design = 0;
			}
			menu_changed = 0;
			menu_first = 1;
		}
		else goto failed;
	}
done:;
	browse_open = 0;
	return;
failed:;
	uw_mfmessage(desgn_win,"Cannot create output menu file.");
	browse_open = 0;
	return;
}
	
/**********************************************************************
**    I_FUNCTION : uw_mfmd_loadfileCB(widget, client_data, call_data)
**                      callback for load button
**       check file and read in
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**                              client_data = Ignored.
**                              call_data   = Ignored
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfmd_loadfileCB(widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;

{
	Widget text_widget;
	char *text, msg[UX_MAX_PATH_LEN+40];
	text_widget = (Widget) file_area;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text[0]!='\0')
		{
/*
...asked if user want save currect menu file before load another file
*/      
			if(menu_changed==1)
			{
				strcpy((char*)load_filename,text); 
				sprintf(msg, "Do you want to save the current menu?"); 
				uw_mfanswer("Save and Load", msg, "LOAD_SAVE");
			}
			else
			{
/*
...We need load edited active menu back
*/
				if(takedown!=-1)
				{
					UDM_menu[takedown].pos[0] = UDM_layout.menu_pos[takedown][0];
					UDM_menu[takedown].pos[1] = UDM_layout.menu_pos[takedown][1];
					UDM_menu[takedown].size[0] = UDM_layout.menu_size[takedown][0];
					UDM_menu[takedown].size[1] = UDM_layout.menu_size[takedown][1];
					UDM_menu_design = 1;
					uw_mfmenu(takedown, 1);
					UDM_menu_design = 0;
				}
				takedown = -1;
				strcpy(UMM_menu.file, text);
				uw_mfcheck_and_load();
			}
		}
	}
}       

/**********************************************************************
**    I_FUNCTION : uw_mfmd_writemenu(filename)
**    write all the design menu info to menu file 
**    PARAMETERS   
**       INPUT  : 
**                                      filename
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfmd_writemenu(filename)
char *filename;
{
	int i, j, status;
	char buffer[UX_MAX_PATH_LEN+40];
	float number1, number2;
	int menu_file;
	UX_pathname file1,ptr1;
	char *ptr,*file;
	FILE *fd;                                               /* os dependent file descriptor */
	ptr = ptr1;
	file = file1;
	status = ux_create_file(filename, 0666, UU_NULL, "STREAM", "ASCII",
		"UX_NOHEADER", &menu_file, UX_PRTERRS);

	if (status == 0)
	{
/*
...put menu data into menu file
*/
		ux_get_os_filedesc(menu_file, &fd, UX_PRTERRS);
		ux_fputs0("#DESCRIPTOR#\n", fd);
		sprintf(buffer, "/NAME/ %s\n", UMM_menu.name);
		ux_fputs0(buffer, fd);
/*
.....changed for border
.....12/9/97 yurong
*/
		number1 = (float)UMM_menu.pos[0]/(float)uw_xw.dev_xmax;
		number2 = (float)UMM_menu.pos[1]/(float)uw_xw.dev_ymax;
/*
.....Don't allow menu position outside the screen
.....if pos[0], or pos[1] < 0
.....set to 0
.....12/11/97 Yurong
*/
		if (number1<0) number1 = 0;
		if (number2<0) number2 = 0;
		sprintf(buffer, "/POSITION/ %f,%f\n", number1, number2);
		ux_fputs0(buffer, fd);
		number1 = UMM_menu.size[0] ;
		number2 = UMM_menu.size[1] ;
		number1 = number1/(float)uw_xw.dev_xmax;
		number2 = number2/(float)uw_xw.dev_ymax;
		sprintf(buffer, "/SIZE/ %f,%f\n", number1, number2);
		ux_fputs0(buffer, fd);
		sprintf(buffer, "/ROWS/ %d\n", UMM_menu.rows);
		ux_fputs0(buffer, fd);
		sprintf(buffer, "/COLS/ %d\n", UMM_menu.cols);
		ux_fputs0(buffer, fd);
		if(UMM_menu.type == UDM_MTYPE_MENU)
			sprintf(buffer, "/TYPE/ MENU\n");
		else if(UMM_menu.type == UDM_MTYPE_ICON)
			sprintf(buffer, "/TYPE/ ICON\n");
		else
			sprintf(buffer, "/TYPE/ POPUP\n");
		ux_fputs0(buffer, fd);
		ux_fputs0("\n#MENUS#\n", fd);
		for(i = 0; i < UMM_menu.rows * UMM_menu.cols; i++)
		{
			if (UW_menu_item[i].name[0]=='\0') strcpy(UW_menu_item[i].name," ");
			if ((UW_menu_item[i].toggle!=NULL)&&(UW_menu_item[i].toggle_num!=0))
			{
				ux_fputs0("#CHOICE#\n", fd);
				sprintf(buffer, "/%s/   \n",UW_menu_item[i].toggle_def);
				ux_fputs0(buffer, fd);
				for (j=0; j<UW_menu_item[i].toggle_num; j++)
				{
					if ((UW_menu_item[i].toggle[j].params==NULL)||
						(UW_menu_item[i].toggle[j].params[0]=='\0'))
					{
						sprintf(buffer, "/%s/ %s\n",UW_menu_item[i].toggle[j].label,
													UW_menu_item[i].toggle[j].func);
					}
					else
					{
						sprintf(buffer, "/%s/ %s, \"%s\"\n", 
										UW_menu_item[i].toggle[j].label,
										UW_menu_item[i].toggle[j].func,
										UW_menu_item[i].toggle[j].params);
					}
					ux_fputs0(buffer, fd);
				}
				ux_fputs0("#MENUS#\n", fd);
			}               
			else
			{
				if ((UW_menu_item[i].params==NULL)||
					(UW_menu_item[i].params[0]=='\0'))
				{
					sprintf(buffer, "/%s/ %s\n",UW_menu_item[i].name,
													UW_menu_item[i].file);
				}
				else
				{
					sprintf(buffer, "/%s/ %s, \"%s\"\n",UW_menu_item[i].name,
									UW_menu_item[i].file, UW_menu_item[i].params);
				}
				ux_fputs0(buffer, fd);
			}
			if (UW_menu_item[i].bgcolor[0] != '\0')
			{
				sprintf(buffer, "#COLOR#  %s\n", UW_menu_item[i].bgcolor);
				ux_fputs0(buffer, fd);
			}
		}
		ux_close(menu_file, UX_PRTERRS);
		menu_changed = 0;
	}
	else 
	{
/*
...cannot open file
*/
		uw_mfmessage(desgn_win,"Cannot create output menu file.");
	}
	return status;
}

/**********************************************************************
**    I_FUNCTION : uw_mfsavemenu(filename)
**    save all the design menu info to menu file and take active menu back
**    PARAMETERS   
**       INPUT  : 
**                                      filename
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfsavemenu(filename)
char *filename;
{
	int i, m, status, k, maxrc, nc;
	UX_pathname file1,ptr1;
	char *ptr,*file;
	ptr = ptr1;
	file = file1;
/*
....write menu to file
*/
/*
.....add checking Yurong 12/12/97
*/
	status = uw_mfmd_writemenu(filename);
	if (status != 0) return status;
	menu_first = 1;
/*
...We need load edited active menu back
*/
	if(takedown!=-1)
	{  
		if ((strcmp(UMM_menu.file, filename)==0)&&(status==0))
		{
			maxrc = UDM_menu[takedown].rows * UDM_menu[takedown].cols;
			strcpy(UDM_menu[takedown].name, UMM_menu.name);
			UDM_menu[takedown].type = UMM_menu.type;
			UDM_menu[takedown].cols = UMM_menu.cols;
			UDM_menu[takedown].rows = UMM_menu.rows;
			UDM_menu[takedown].pos[0] = UMM_menu.pos[0];
			if (UMM_menu.type == UDM_MTYPE_MENU)
				UDM_menu[takedown].pos[1] = UMM_menu.pos[1];
			else
				UDM_menu[takedown].pos[1] = UMM_menu.pos[1];
/*
.....Don't allow menu position outside the screen
.....if they are set to 0
.....12/11/97 Yurong
*/
			if (UDM_menu[takedown].pos[0] < 0)
				UDM_menu[takedown].pos[0] = 0;
			if (UDM_menu[takedown].pos[1] < 0)
				UDM_menu[takedown].pos[1] = 0;
			
			UDM_menu[takedown].size[0] = UMM_menu.size[0];
			UDM_menu[takedown].size[1] = UMM_menu.size[1];
			UDM_layout.menu_pos[takedown][0] = UDM_menu[takedown].pos[0];
			UDM_layout.menu_pos[takedown][1] = UDM_menu[takedown].pos[1];
			UDM_layout.menu_size[takedown][0] = UDM_menu[takedown].size[0];
			UDM_layout.menu_size[takedown][1] = UDM_menu[takedown].size[1];
			for(k = 0; k < UMM_menu.cols*UMM_menu.rows; k++)
			{
				strcpy(UDM_menu[takedown].menus[k].name, UW_menu_item[k].name);
				strcpy(UDM_menu[takedown].menus[k].file, UW_menu_item[k].file);
				if (UW_menu_item[k].bgcolor[0]!='\0')
				{
					strcpy(UDM_menu[takedown].menus[k].bgcolor,
												UW_menu_item[k].bgcolor);
				}
				else
					UDM_menu[takedown].menus[k].bgcolor[0] = '\0';
				UDM_menu[takedown].menus[k].color[0] = -1;
				UDM_menu[takedown].menus[k].color[1] = -1;
				UDM_menu[takedown].menus[k].color[2] = -1;

				if (UW_menu_type[k]==UW_TOGGLE_MENU)
				{
					strcpy(UDM_menu[takedown].menus[k].toggle_def,
												UW_menu_item[k].toggle_def);
					UDM_menu[takedown].menus[k].toggle_num =
												UW_menu_item[k].toggle_num;
					if (UDM_menu[takedown].menus[k].toggle == NULL)
					{
						UDM_menu[takedown].menus[k].toggle = 
										(UDM_menu_toggle_struc *)malloc(
										sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
					}
					for (m=0; m<UW_menu_item[k].toggle_num; m++)
					{
						strcpy(UDM_menu[takedown].menus[k].toggle[m].label,
											UW_menu_item[k].toggle[m].label);
						strcpy(UDM_menu[takedown].menus[k].toggle[m].func,
											UW_menu_item[k].toggle[m].func);
						if (UDM_menu[takedown].menus[k].toggle[m].params!=NULL)
							uu_free(UDM_menu[takedown].menus[k].toggle[m].params);
						if (UW_menu_item[k].toggle[m].params==NULL)
							UDM_menu[takedown].menus[k].toggle[m].params = NULL;
						else
						{
							nc  = strlen (UW_menu_item[k].toggle[m].params);
							UDM_menu[takedown].menus[k].toggle[m].params = 
										uu_malloc((nc+1)*sizeof(char));
							strcpy(UDM_menu[takedown].menus[k].toggle[m].params,
											UW_menu_item[k].toggle[m].params);
						}
					}
				}
				else
				{
/*
......free UDM_menu's toggle memory
*/
					if (k < maxrc && UDM_menu[takedown].menus[k].toggle != NULL)
					{
						uu_free(UDM_menu[takedown].menus[k].toggle);
						UDM_menu[takedown].menus[k].toggle = 0;
					}
					UDM_menu[takedown].menus[k].toggle_num = 0;
				}
			}
		}
		UDM_menu_design = 1;
		uw_mfmenu(takedown, 1);
		UDM_menu_design = 0;
	}
/*
.....added for changed menu that already loaded.
.....reset this menu again. If not rerset it,
.....when load it later, it will load
.....the menus that are not changed. Set up menu
.....structure as needed
.....Yurong 9/10/97
*/
	else
	{
		for (i=0;i<UDM_menu_count;i++)
		{
			strcpy(file, filename);
			if (strcmp(UDM_menu[i].file,file) == 0)
			{
				strcpy(UDM_menu[i].name, UMM_menu.name);
				UDM_menu[i].type = UMM_menu.type;
				UDM_menu[i].cols = UMM_menu.cols;
				UDM_menu[i].rows = UMM_menu.rows;
				UDM_menu[i].pos[0] = UMM_menu.pos[0];
				UDM_menu[i].pos[1] = UMM_menu.pos[1];
				UDM_menu[i].size[0] = UMM_menu.size[0];
				UDM_menu[i].size[1] = UMM_menu.size[1];
				UDM_layout.menu_pos[i][0] = UDM_menu[i].pos[0];
				UDM_layout.menu_pos[i][1] = UDM_menu[i].pos[1];
				UDM_layout.menu_size[i][0] = UDM_menu[i].size[0];
				UDM_layout.menu_size[i][1] = UDM_menu[i].size[1];
				for(k = 0; k < UMM_menu.cols*UMM_menu.rows; k++)
				{
					strcpy(UDM_menu[i].menus[k].name, UW_menu_item[k].name);
					strcpy(UDM_menu[i].menus[k].file, UW_menu_item[k].file);
					if (UDM_menu[i].menus[k].params!=NULL)
						uu_free(UDM_menu[i].menus[k].params);
					if (UW_menu_item[k].params==NULL)
						UDM_menu[i].menus[k].params = NULL;
					else
					{
						nc  = strlen (UW_menu_item[k].params);
						UDM_menu[i].menus[k].params = 
										uu_malloc((nc+1)*sizeof(char));
						strcpy(UDM_menu[i].menus[k].params,
											UW_menu_item[k].params);
					}
					if (UW_menu_item[k].bgcolor[0]!='\0')
					{
						strcpy(UDM_menu[i].menus[k].bgcolor,
													UW_menu_item[k].bgcolor);
					}
					else
						UDM_menu[i].menus[k].bgcolor[0] = '\0';
					UDM_menu[i].menus[k].color[0] = -1;
					UDM_menu[i].menus[k].color[1] = -1;
					UDM_menu[i].menus[k].color[2] = -1;
					if (UW_menu_type[k]==UW_TOGGLE_MENU)
					{
						strcpy(UDM_menu[i].menus[k].toggle_def,
													UW_menu_item[k].toggle_def);
						UDM_menu[i].menus[k].toggle_num =
													UW_menu_item[k].toggle_num;
						if (UDM_menu[i].menus[k].toggle != NULL)
						{
							UDM_menu[i].menus[k].toggle = 
										(UDM_menu_toggle_struc *)malloc(
										sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
						}
						for (m=0; m<UW_menu_item[k].toggle_num; m++)
						{
							strcpy(UDM_menu[i].menus[k].toggle[m].label,
											UW_menu_item[k].toggle[m].label);
							strcpy(UDM_menu[i].menus[k].toggle[m].func,
											UW_menu_item[k].toggle[m].func);
							if (UDM_menu[i].menus[k].toggle[m].params!=NULL)
								uu_free(UDM_menu[i].menus[k].toggle[m].params);
							if (UW_menu_item[k].toggle[m].params==NULL)
								UDM_menu[i].menus[k].toggle[m].params = NULL;
							else
							{
								nc  = strlen (UW_menu_item[k].toggle[m].params);
								UDM_menu[i].menus[k].toggle[m].params = 
											uu_malloc((nc+1)*sizeof(char));
								strcpy(UDM_menu[i].menus[k].toggle[m].params,
												UW_menu_item[k].toggle[m].params);
							}
						}
					}
				}
				break;
			}
		}
	}
	return status;
}

/**********************************************************************
**    I_FUNCTION : uw_mfmd_acceptCB(widget, client_data, call_data)
**              callback function for accept  button
**    check menu file and save all the design menu info to menu file
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**                              client_data = Ignored.
**                              call_data   = Ignored
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfmd_acceptCB(widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;

{
	Widget text_widget;
	char *text;
	int i;
	text_widget = (Widget) file_area;
/*
.....wait until menu load finish
.....before we free memory (the loading
.....will alloc memory)
*/
	while (UW_smenu_loaded==0) ;

	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text[0]!='\0')
		{
/*                      strcpy(UMM_menu.file,text);  */
/* 
...Check for file existence 
*/
			menu_accept = 1;
			uw_mfcheck_and_save(text);
		}
		else
		{
			uw_mfmessage(widget,"You need to specify a filename.");
		}
		XtFree(text);
	}
	UDM_menu_design = 0;
/*
.....free toggle memery
*/
	for (i=0; i<UDM_MAX_MENU; i++)
	{
		if (UW_menu_item[i].toggle!=NULL)
		{
			uu_free(UW_menu_item[i].toggle);
			UW_menu_item[i].toggle = 0;
		}
	}
	UW_smenu_loaded = 1;
}

/**********************************************************************
**    I_FUNCTION : read_filenameCB(widget, client_data, call_data)
**              Read the user input and save the filename
**    PARAMETERS   
**       INPUT  : 
**          widget      = Prompt box
**                              client_data = save_filename
**                              call_data   = structure that include user input string
**             none
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void read_filenameCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	char *label;
	UX_pathname filename;
	XmSelectionBoxCallbackStruct *cbs =
		(XmSelectionBoxCallbackStruct *)call_data;
	if (XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&label))
	{
/*
...save the filename and save currect menu file, load the new file
*/
		strcpy(filename, label);
		uw_mfcheck_and_save(filename);  
	}
	XtDestroyWidget(widget);
	XtPopdown(XtParent((Widget)client_data));
}       


/**********************************************************************
**    I_FUNCTION : uw_mfget_savemenu (widget, filename)
**     popup a dialog asking for save menu file name
**    PARAMETERS   
**       INPUT  : 
**          widget      = parent widget for popup dialog
**       OUTPUT :  
**              filename:   = user input filename        
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfget_savemenu (widget, filename)
Widget widget;
char * filename;
{
	Widget dialog;
	char num[200];
	XmString t, t1;
	Arg args[5];
	int n ;
	sprintf(num, "Enter the menu file name:"); 
	t = XmStringCreateSimple(num);
	sprintf(num, "%s", UMM_menu.file);
	t1 = XmStringCreateSimple(num);
/*
...Create the dialog
*/
	n = 0;
	XtSetArg(args[n], XmNselectionLabelString, t); n++;
	XtSetArg(args[n], XmNtextString, t1); n++; 
	dialog = (Widget)XmCreatePromptDialog(widget, "Label", args, n);
	XtVaSetValues(XtParent(dialog),XmNtitle,"Input file name",NULL);
	XmStringFree(t);
	XmStringFree(t1);

	XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)read_filenameCB, 
							(XtPointer)widget);
/*
...When user select cancel, just destroy the dialog
*/
	XtAddCallback(dialog, XmNcancelCallback, (XtCallbackProc)XtDestroyWidget, NULL);
/*
...no help used
*/
	XtUnmanageChild((Widget)XmSelectionBoxGetChild((Widget)dialog,XmDIALOG_HELP_BUTTON)); 
	XtManageChild(dialog);
	XtPopup(XtParent(dialog), XtGrabNone);
}


/**********************************************************************
**    I_FUNCTION :uw_mfcheck_and_save(filename) 
**    check menu file and save the design menu info to menu file
**    PARAMETERS   
**       INPUT  : 
**                                              filename
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfcheck_and_save(filename)
char*filename;
{
	int status, answer;
	UX_pathname fullname;
	FILE *fptr;
/*
.....open for read to check if file exist
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "menu", "NCL_MENU", UU_NULL,
			filename, 0, UU_NULL);
	if (status==0)
	{
/*
...file exists, overwrite (y/n)?
*/
		answer = uw_mfyes_or_no(menu_form, "File exists, Overwrite?", "Save Menu");
		if (answer==0)
			return -1;
		status = ux_delete(fullname);
		if (status != 0) return status;
	}
	else
/*
......file not exist, open for writing (will create the file)
*/
	{
		status = ul_open_mod_file("UU_USER_SETTINGS", "menu", "NCL_MENU", UU_NULL,
			filename, 3, &fptr);
		if (status!=UU_SUCCESS) goto done;
		ux_fclose0 (fptr);
	}
/*
.....add checking
.....12/12/97 Yurong
*/
	status = uw_mfsavemenu(filename);
	if (status != 0) return status;
	menu_changed = 0;
	menu_first = 1;
	if(menu_accept == 1)
	{
		mfirst = 1;
		menu_accept = 0;
		takedown = -1;
		UW_close_mdsgn = 1;
	}
	else    
	{
		takedown = -1;
		strcpy(UMM_menu.file, (char*)load_filename);
		uw_mfcheck_and_load();
	}
done:;
	return 0;
}
/**********************************************************************
**    I_FUNCTION :uw_mfcheck_and_load() 
**    check menu file and load a  menu file
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**                              client_data = Ignored.
**                              call_data   = Ignored
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfcheck_and_load()
{
	int mode, status;
	UX_pathname fullname;
	char msg[UX_MAX_PATH_LEN+40];
/*
.....wait until menu load before
.....this one finish
*/
	while (UW_smenu_loaded==0) ;
		
	mode = 0;       
	menu_changed = 0;
	menu_first = 1;
	strcpy(fullname, UMM_menu.file);
	status = ul_open_mod_file("UU_USER_SETTINGS", "menu", "NCL_MENU", UU_NULL,
		fullname, 0, UU_NULL);
	if (status!=0)
	{       
		sprintf(msg, "The file %s does not exist!", UMM_menu.file);
		uw_mfmessage(menu_form,msg);
		return UU_FAILURE;
	}
	strcpy(UMM_menu.file, fullname);                        
	status = uw_mfreadmenu();
	UW_smenu_loaded = 1;
	return status;
}
/**********************************************************************
**    I_FUNCTION : uw_mfCreatePushbutton(parent, name, callback, client_data)
**              Create a push button    
**    PARAMETERS   
**       INPUT  : 
**          parent :      parent widget
**          name:         name of push button
**          callback:     callback function for push button
**                              client_data = client_data needed for push botton call back.
**       OUTPUT :  
**                      none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

Widget uw_mfCreatePushbutton(parent, name, callback, client_data)
Widget parent;
char *name;
XtCallbackProc callback;
XtPointer client_data;
{
	Widget push;
	Arg args[20];
	Cardinal n;
	n = 0;
	push = XmCreatePushButton(parent, name, args, n);
	XtAddCallback(push, XmNactivateCallback, callback, client_data);
	XtManageChild(push);
	return push;
}

#endif
