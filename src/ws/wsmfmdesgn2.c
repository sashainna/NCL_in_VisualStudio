#include "usysdef.h"
#if UU_COMP != UU_VAXVMS || UU_OPSYS == UU_ALPHAVMS
#if UU_COMP!=UU_WIN2K

/*********************************************************************
**	FILENAME: wsmenudesgn2.c
**	CONTAINS:	
**					asked_meCB
**					searchCB
**					fun_selectCB
**					disp_statusCB
**					configureCB
**					uw_mfmenulayout
**					uw_mfresetmenu
**					uw_mfsetmenu
**					uw_mffunselect
**					uw_mfremove_desgn_menu
.....added for toggle inside menu
..... 4/8/99
**					toggle_askedCB
**					disp_status2CB
**					toggle_insertCB
**					toggle_deleteCB
**					toggleCB
**					buttonCB
**					toggle_disp_statusCB
**					toggle_disp_choiceCB
**					popup_toggle
**					
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsmfmdesgn2.c , 25.1
**    DATE AND TIME
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
#include <decw$include:DialogS.h>
#include <decw$include:PanedW.h>
#include <decw$include:LabelG.h>
#include <decw$include:PushBG.h>
#include <decw$include:Protocols.h>
#include <decw$include:Form.h>
#include <decw$include:Text.h>
#include <decw$include:MwmUtil.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include<Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
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
#include <X11/StringDefs.h>
#endif
#include "xfsys1.h"
#include "uhep.h"
#include "xenv1.h"
#include "wsmf.h"
#include "dmotif.h"
#include "usysdef.h"
#include "wsxw.h"
#include "wsmfmdesgn.h"
#include "zkeysym.h"
/*
.....added by Yurong
.....8/17/97
*/
extern UWS_MF uw_mf;
extern int XBorder[2], YBorder[2];
extern int Border_stat;
extern char *uu_malloc();
/*
...select function by user
*/
static void uw_mfstat_okCB();
static char select_func[200];
static char menu_list_item[100];
static int delete_bt = -1;
static int insert_bt = -1;

void uw_mfresetmenu();
void uw_mfmenu_insert();
void uw_mfmenu_delete();
void uw_mfsetmenu();
void uw_mffunselect();

/*
.....Check if desgn menu already mapped
.....only if mapped (reset_flag=1), we can
.....continue reset menu (there are time delay for Motif)
.....if we have not get the right pos, size, and reset the
.....the menu, because adjust border, the size will get smaller
.....and smaller
.....Yurong 12/19/97
*/
static int reset_flag;
typedef struct
{
   char *label;
   void (*callback)();
   XtPointer data;
} ActionAreaItem;

static ActionAreaItem actionList[] =
{
	{"OK", uw_mfstat_okCB,  NULL}
};

static void uw_mfstat_okCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	Widget wgt = (Widget) client_data;
	XtDestroyWidget(wgt);
	wgt = 0;
}
	
CvtColor(widget, color_name, color)
Widget widget;
char *color_name;
Pixel *color;
{
	XrmValue from, to;
	Pixel return_color;
	int status;
	
	from.addr = color_name;
	from.size = strlen(from.addr) + 1;
	to.addr = (XtPointer) &return_color;
	to.size = sizeof(Pixel);

	status = XtConvertAndStore(widget, XtRString, &from,
						XtRPixel, &to);
	*color = return_color;
	return status;

}
	

/**********************************************************************
**    I_FUNCTION : toggle_askedCB (widget, client_data, call_data)
**     callback function for puchbutton on the sample menu's toggle button
**     Get toggle button label
**    PARAMETERS   
**       INPUT  : 
**          widget      = toggle button
**				client_data = Ignored.
**				call_data   = Ignored
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void toggle_askedCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	char *num[2];
	char *prompt[2];
	int pmenu, pchc, stat,nc;
	UDM_menu_toggle_struc *cbdata = (UDM_menu_toggle_struc *)client_data;

	pmenu =  cbdata->pos[1];
	pchc = cbdata->pos[2];

	num[0] = (char*) malloc(200*sizeof(char));
	num[1] = (char*) malloc(200*sizeof(char));
	prompt[0] = (char*) malloc(80*sizeof(char));
	prompt[1] = (char*) malloc(80*sizeof(char));

	sprintf(num[0], "Enter the toggle label for %s function:",select_func); 
	sprintf(num[1], "Enter the toggle function paramter for %s function:",
							select_func); 
	strcpy(prompt[0], UW_menu_item[pmenu].toggle[pchc].label);
	if (UW_menu_item[pmenu].toggle[pchc].params!=NULL)
		strcpy(prompt[1], UW_menu_item[pmenu].toggle[pchc].params);
	else
		prompt[1][0] = '\0';
	if (strcmp(prompt[0], " ")==0)
		prompt[0][0] = '\0';
	stat = uw_mfget_nprompt(menu_win, "Get Toggle Label", 2, num, prompt);
	if (stat<0) return;
	menu_changed = 1;
	if (prompt[0][0] == '\0')
		strcpy(prompt[0], " ");
	ul_to_upper(prompt[0]);
	strcpy(UW_menu_item[pmenu].toggle_def, prompt[0]);
	strcpy(UW_menu_item[pmenu].toggle[pchc].label, prompt[0]);
	nc = strlen(prompt[1]);
	if (nc>0)
	{
		UW_menu_item[pmenu].toggle[pchc].params = uu_malloc((nc+1)*sizeof(char));
		strcpy(UW_menu_item[pmenu].toggle[pchc].params, prompt[1]);
	}
	else
		UW_menu_item[pmenu].toggle[pchc].params = UU_NULL;
	
	strcpy(UW_menu_item[pmenu].toggle[pchc].func, select_func);
	free (num[0]);
	free (num[1]);
	free (prompt[0]);
	free (prompt[1]);
	uw_mfresetmenu();
}

/**********************************************************************
**    I_FUNCTION : asked_meCB (widget, client_data, call_data)
**     callback function for puchbutton on the sample menu
**     popup a dialog asking for menu label
**    PARAMETERS   
**       INPUT  : 
**          widget      = parent widget for popup dialog
**				client_data = Ignored.
**				call_data   = Ignored
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void asked_meCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	char *num[2];
	char *prompt[2];
	int pmenu, stat,nc;
	pmenu =  (int)client_data;

	num[0] = (char*) malloc(200*sizeof(char)); 
	num[1] = (char*) malloc(200*sizeof(char)); 
	prompt[0] = (char*) malloc(80*sizeof(char));
	prompt[1] = (char*) malloc(80*sizeof(char));
	sprintf(num[0], "Enter the menu label for %s function:",select_func); 
	sprintf(num[1], "Enter the parameter for %s function:",select_func); 
	strcpy(prompt[0], UW_menu_item[pmenu].name);
	if (UW_menu_item[pmenu].params==NULL)
		prompt[1][0] = '\0';
	else
		strcpy(prompt[1], UW_menu_item[pmenu].params);
/*
.....check if UW_menu_item[pmenu].name = " ",
.....which is initial value, we don't user it
.....as default value
.....( we use " " as initial value instead use ""
.....because read menu function don't allow 
.....syntax like // KEY_NOOP
.....it must / / KEY_NOOP, the name alway at least one space
.....Yurong 4/21/99
*/
	if (strcmp(prompt[0], " ")==0)
		prompt[0][0] = '\0';
	stat = uw_mfget_nprompt(menu_win, "Get Menu Label and parameter", 
										2, num, prompt);
	if (stat<0) return;
	if (prompt[0][0] == '\0')
		strcpy(prompt[0], " ");
	menu_changed = 1;
	ul_to_upper(prompt[0]);
	strcpy(UW_menu_item[pmenu].name, prompt[0]);
	nc = strlen(prompt[1]);
	if (nc>0)
	{
		UW_menu_item[pmenu].params = uu_malloc((nc+1)*sizeof(char));
		strcpy(UW_menu_item[pmenu].params, prompt[1]);
	}
	else
		UW_menu_item[pmenu].params = UU_NULL;
	strcpy(UW_menu_item[pmenu].file, select_func);
	free (num[0]);
	free (num[1]);
	free (prompt[0]);
	free (prompt[1]);
	uw_mfresetmenu();
}

/**********************************************************************
**    I_FUNCTION :  searchCB(widget,clientData,callData)
**       Callback function for search button 
**       Search the given string from the function list
**    PARAMETERS   
**       INPUT  : 
**          widget     = Ignored
**				clientData = Ignored
**				callData   = Motif callback structure, contain the search
**                       string
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void searchCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int i,j;
	char msg[80];
	char disp[80], ltext[80];
/*
...for store the last searched string number
*/
	static int ilast = -1;
	static int jlast = -1;
/*
...for store the last searched string 
*/
	static char last_str[80] = "";
	text_widget = (Widget) client_data;
	if (text_widget!=(Widget)NULL)
	{
/*
...get string from text field
*/
		text = XmTextGetString(text_widget);
		if (text[0]!='\0')
		{
/*
...reset last searched string number
*/
			strcpy(ltext, text);
			ul_to_upper(text);
			if(strcmp(last_str, text)!=0)
			{
				ilast = -1;
				jlast = -1;
			}
/*
...compare from last searched string
*/
			for(i=ilast+1, j=jlast+1; ; i++,j++)
			{
				if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
				if (strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) 
				{
					j--;
					continue;
				}
/*
...change to upper case to compare
*/
				strcpy(disp, UZ_keyfuncs[i].descrip);
				ul_to_upper(disp);
				if((strstr(UZ_keyfuncs[i].name, text)!=NULL)	||
						(strstr(disp, text)!=NULL))
				{
					ilast = i;
					jlast = j;
					strcpy(last_str, text);
					XmListSelectPos(func_select, j+2, True);
					XmListSetPos(func_select,j+2);
					return;
				}
			}
			if(strcmp(last_str, text)==0)
			{
				for(i=0,j=0; i<ilast+1; i++,j++)
				{
					if (strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) 
					{
						j--;
						continue;
					}
/*
...change to upper case to compare
*/
					strcpy(disp, UZ_keyfuncs[i].descrip);
					ul_to_upper(disp);
					if((strstr(UZ_keyfuncs[i].name, text)!=NULL)	||
						(strstr(disp, text)!=NULL))
					{
						ilast = i;
						jlast = j;
						strcpy(last_str, text);
						XmListSelectPos(func_select, j+2, True);
						XmListSetPos(func_select,j+2);
						return;
					}

				}
			}
			else
/*
...Not find it
*/
			sprintf(msg, "Sorry, can not find string %s", ltext);
			uw_mfmessage(widget,msg);
		}
	}
}
	
/**********************************************************************
**    I_FUNCTION :  fun_selectCB(widget,clientData,callData)
**       Callback function for function select list
**       Store the select function
**    PARAMETERS   
**       INPUT  : 
**          widget     = fileselect list.
**				clientData = Ignored
**				callData   = Motif callback structure, contain the select
**                       item.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void fun_selectCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	int len;
	UX_pathname old_fname,ptr0,ptr1,dir, dir2, fullname;
	char num[256];
	XmString delstr, addstr;
	XmListCallbackStruct *ptr;
	char *ptr2, *ptr3;
	char *string;
	int mode, status, found;
	char *pathlistptr = 0;	
	static UX_pathname filename = "\0";
	static int	browse_open = 0;

	static int nfld=3,ifld=0;
	static char *ftext[3]={"System","Local","File Tree"};
	static char ldir[3][UX_MAX_PATH_LEN];/*={"NCL_MENU","NCL_USER_MENU","."};*/
	static char *fdir[3]={ldir[0],ldir[1],ldir[2]};

	ptr = (XmListCallbackStruct*) call_data;
	XmStringGetLtoR(ptr->item, XmSTRING_DEFAULT_CHARSET,&string);
	ptr3 = ptr0;
	ptr3 = strtok(string, " ");
	strcpy(select_func, ptr3);
/*
...if select MENUS
...display a fileselect dialog, let user select menu files
*/
	if(strcmp(select_func,"MENUS")==0)
	{
/*
......changed for adding default filename
......Yurong 2/5/1998
*/
/*
.....added variable check for avoiding open two browser
.....Yurong 2/6/1998
*/
		if (browse_open == 1)
			return;
		browse_open = 1;
		strcpy(old_fname, filename);
		
		strcpy(ldir[0],"NCL_MENU");
/*		strcpy(ldir[1],"NCL_USER_MENU"); */
/*
.....default the saving directory to UU_USER_SETTING directory, if this
.....directory is not there, create one
*/
		ux_get_syspath("UU_USER_SETTINGS", &pathlistptr, dir, 
				&found, UX_NPRTERRS|UX_NCHK|UX_NQUOTES);
		uu_lsdel(pathlistptr);
		ul_build_full_dir(dir,"menu", dir2);
/*
.....check if the path exist first, if not create one.
*/
		mode = 0;
		status = ux_mk_chk_syspath(0, dir2, 0, 0,
					0, &mode, fullname,UX_NPRTERRS|UX_NQUOTES);
		if (status != UU_SUCCESS || mode&UX_NEXISTS)
		{
			status = ux_mk_pthdir(dir2);
			if (status == UU_SUCCESS)
			{
				strcpy(dir2, ".");
			}
		}
		strcpy(ldir[1], dir2);
		strcpy(ldir[2], ".");

		uw_mfmd_filename(widget, "Menu_select", "*.menu", nfld, &ifld,
			ftext, fdir, filename, &len, NULL);
		browse_open = 0;
		if (filename[0]=='\0')
			strcpy(filename, old_fname);
		ptr2 = ptr1;
/*		if((ptr2 = strrchr(filename,'/'))!=NULL)*/
/*			strcpy(select_func,ptr2+1) ;*/
/*		else*/
/*			strcpy(select_func, filename);*/
		ul_break_fname(filename,dir,select_func);
		delstr =	XmStringCreateSimple(menu_list_item);
		XmListDeleteItem(widget, delstr);
		sprintf(num, "MENUS                         %s", select_func);
		strcpy(menu_list_item, num);
		addstr =	XmStringCreateSimple(num);
		XmListAddItem(widget, addstr, 1);
		XmListSelectPos(widget, 1, False);
		
	}
}

/**********************************************************************
**    I_FUNCTION :  map_dialogCB(widget,client_data, call_data)
**      Set the status dialog position in designing menu position
**    PARAMETERS   
**       INPUT  : 
**				call_data   = Motif callback structure.
**				client_data = Ignored
**				widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void map_dialogCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data, call_data;
{
	int xy[2], root_x,root_y ;
	unsigned int keys_buttons;
	Window root,child;
	XQueryPointer(uw_xw.disp,uw_xw.wd_id,&root,&child,&root_x,&root_y,
		&xy[0],&xy[1],&keys_buttons);
	XtVaSetValues(widget, XmNx, xy[0], XmNy, xy[1], NULL);
}
/**********************************************************************
**    I_FUNCTION :  disp_status2CB(widget,client_data, call_data)
**      Display the label and function that select menu comtained 
**    PARAMETERS   
**       INPUT  : 
**				call_data   = Motif callback structure.
**				client_data = Contains program information about pushed
**				              button.
**				widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void disp_status2CB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data, call_data;
{
	Widget stat_box, form, pane, action_area;
	int i, j, number;
	XmString str1, str2, str3, lstr, fstr, dstr, pstr;
	int n;
	char first[80], second[80], third[400], name[400], label[40], 
			func[40], disp[400], parms[40];
	number = (int) client_data;
	sprintf(first, "This menu item is a toggle button with %d toggle choices",
							UW_menu_item[number].toggle_num);
	sprintf(second, "The default toggle choice label: %s", 
							UW_menu_item[number].toggle_def);

	str1 = XmStringCreateSimple(first);
	str2 = XmStringCreateSimple(second);
	stat_box = XtVaCreatePopupShell("status_disp",xmDialogShellWidgetClass,
					XtParent(widget),
					XmNdefaultPosition, False,
					XmNtitle,"Status Display",
					XmNdeleteResponse, XmDESTROY, NULL);
	XtAddCallback(stat_box, XmNpopupCallback, (XtCallbackProc)map_dialogCB, 
						(XtPointer)NULL);
	n = 0;
	pane = XtVaCreateWidget("pane",xmPanedWindowWidgetClass, stat_box,
				XmNsashWidth,1,
				XmNsashHeight,1,
				NULL);
	form = XtVaCreateWidget("contral_area", xmRowColumnWidgetClass,pane, NULL);
	XtVaCreateManagedWidget("first",xmLabelGadgetClass,form ,
					XmNlabelString, str1, NULL );
	XtVaCreateManagedWidget("second", xmLabelGadgetClass,form ,
					XmNlabelString, str2, NULL );
	
	if (UW_menu_item[number].toggle_num>0)
	{
		sprintf(third, "The toggle choices label, fuction&discription as following:");
		str3 = XmStringCreateSimple(third);
		XtVaCreateManagedWidget("third", xmLabelGadgetClass,form , 
					XmNlabelString, str3, NULL );
	}
	str3 = XmStringCreateSimple("         ");
	XtVaCreateManagedWidget("third", xmLabelGadgetClass,form ,
					XmNlabelString, str3, NULL );

	for (j=0; j<UW_menu_item[number].toggle_num; j++)
	{
		disp[0] = '\0';
		for(i=0; ; i++)
		{
			if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
			if (strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) continue;
/*
...change to upper case to compare
*/
			strcpy(name, UZ_keyfuncs[i].name);
			ul_to_upper(name);
			if (strstr(name, UW_menu_item[number].toggle[j].func)!=NULL)
			{
				sprintf(disp, "    Description: %s", UZ_keyfuncs[i].descrip);
				break;
			}
		}
		if (disp[0] == '\0')
			strcpy(disp, "    Description: No description available");

		sprintf(label, "    Toggle label: %s",
					 UW_menu_item[number].toggle[j].label);
		sprintf(func, "    Function:    %s", 
						UW_menu_item[number].toggle[j].func);
		if (UW_menu_item[number].toggle[j].params==NULL)
			strcpy(parms, "    Parameter:   ");
		else
			sprintf(parms, "    Parameter:   %s", 
						UW_menu_item[number].toggle[j].params);
		lstr = XmStringCreateSimple(label);
		fstr = XmStringCreateSimple(func);
		dstr = XmStringCreateSimple(disp);
		pstr = XmStringCreateSimple(parms);

		XtVaCreateManagedWidget("first",xmLabelGadgetClass,form ,
					XmNlabelString, lstr, NULL );
		XtVaCreateManagedWidget("second", xmLabelGadgetClass,form ,
					XmNlabelString, fstr, NULL );
		XtVaCreateManagedWidget("third", xmLabelGadgetClass,form , 
					XmNlabelString, pstr, NULL );
		XtVaCreateManagedWidget("fourth", xmLabelGadgetClass,form , 
					XmNlabelString, dstr, NULL );
	}
	XtManageChild(form);
	n = XtNumber(actionList);
	actionList[0].data = (XtPointer)stat_box;
	action_area = (Widget)uw_mfcreate_action(pane,actionList,n);

	XtManageChild(pane);
	XtPopup(stat_box, XtGrabNone); 
	XmStringFree(lstr);
	XmStringFree(fstr);
	XmStringFree(dstr);
	XmStringFree(str1);
	XmStringFree(str2);
	XmStringFree(str3);
}

/**********************************************************************
**    I_FUNCTION :  disp_statusCB(widget,client_data, call_data)
**      Display the label and function that select menu comtained 
**    PARAMETERS   
**       INPUT  : 
**				call_data   = Motif callback structure.
**				client_data = Contains program information about pushed
**				              button.
**				widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void disp_statusCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data, call_data;
{
	Widget stat_box, form, pane, action_area;
	int i, number;
	XmString str1, str2, str3, str4;
	int n;
	char first[80], second[80], third[80], fourth[400], name[400];
	number = (int) client_data;
	sprintf(first, "Button label: %s", UW_menu_item[number].name);
	sprintf(second, "Function:    %s", UW_menu_item[number].file);
	if ((UW_menu_item[number].params==NULL)||
		(UW_menu_item[number].params[0]=='\0'))
		strcpy(third, "Function parameter: No function parameter");
	else
		sprintf(third, "Function parameter: %s", UW_menu_item[number].params);
	fourth[0] = '\0';
	for(i=0; ; i++)
	{
		if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
		if (strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) continue;
/*
...change to upper case to compare
*/
		strcpy(name, UZ_keyfuncs[i].name);
		ul_to_upper(name);
		if (strstr(name, UW_menu_item[number].file)!=NULL)
		{
			sprintf(fourth, "Description: %s", UZ_keyfuncs[i].descrip);
			break;
		}
	}
	if (fourth[0] == '\0')
		strcpy(fourth, "Description: No description available");

	str1 = XmStringCreateSimple(first);
	str2 = XmStringCreateSimple(second);
	str3 = XmStringCreateSimple(third);
	str4 = XmStringCreateSimple(fourth);
	stat_box = XtVaCreatePopupShell("status_disp",xmDialogShellWidgetClass,
					XtParent(widget),
					XmNdefaultPosition, False,
					XmNtitle,"Status Display",
					XmNdeleteResponse, XmDESTROY, NULL);
	XtAddCallback(stat_box, XmNpopupCallback, (XtCallbackProc)map_dialogCB, 
						(XtPointer)NULL);
	n = 0;
	pane = XtVaCreateWidget("pane",xmPanedWindowWidgetClass, stat_box,
				XmNsashWidth,1,
				XmNsashHeight,1,
				NULL);
	form = XtVaCreateWidget("contral_area", xmRowColumnWidgetClass,pane, NULL);

	XtVaCreateManagedWidget("first",xmLabelGadgetClass,form ,
					XmNlabelString, str1, NULL );
	XtVaCreateManagedWidget("second", xmLabelGadgetClass,form ,
					XmNlabelString, str2, NULL );
	XtVaCreateManagedWidget("third", xmLabelGadgetClass,form , 
					XmNlabelString, str3, NULL );
	XtVaCreateManagedWidget("fourth", xmLabelGadgetClass,form , 
					XmNlabelString, str4, NULL );
	XtManageChild(form);
	n = XtNumber(actionList);
	actionList[0].data = (XtPointer)stat_box;
	action_area = (Widget)uw_mfcreate_action(pane,actionList,n);

	XtManageChild(pane);
	XtPopup(stat_box, XtGrabNone); 
	XmStringFree(str1);
	XmStringFree(str2);
	XmStringFree(str3);
	XmStringFree(str4);
	
}

/**********************************************************************
**    I_FUNCTION : toggle_insertCB (widget, client_data, call_data)
**       callback function for insert menu, insert one toggle button
**       into designing menu
**    PARAMETERS  
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Ignore
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void toggle_insertCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	int i,nc;
	char *num[2];
	char *prompt[2];
	XmPushButtonCallbackStruct *cbs;
	XEvent *bevent ;
	int pmenu, pchc, stat;
	UDM_menu_toggle_struc *cbdata = (UDM_menu_toggle_struc *)client_data;
	cbs= (XmPushButtonCallbackStruct *)call_data;
	bevent = (XEvent *)cbs->event;
	if (bevent->type!=ButtonRelease) return; 
	pmenu =  cbdata->pos[1];
	pchc = cbdata->pos[2];
	num[0] = (char*) malloc(200*sizeof(char));
	num[1] = (char*) malloc(200*sizeof(char));
	prompt[0] = (char*) malloc(80*sizeof(char));
	prompt[1] = (char*) malloc(80*sizeof(char));

	sprintf(num[0], "Enter the toggle label for %s function:",select_func); 
	sprintf(num[1], "Enter the toggle function paramter for %s function:",
								select_func); 
	prompt[0][0] = '\0';
	prompt[1][0] = '\0';
	stat = uw_mfget_nprompt(menu_win, "Insert Toggle", 2, num, prompt);
/*
.....canceled
*/
	if (stat<0) return;
	if (prompt[0][0] == '\0')
		strcpy(prompt[0], " ");
	menu_changed = 1;
	UW_menu_item[pmenu].toggle_num++;
	ul_to_upper(prompt[0]);
	strcpy(UW_menu_item[pmenu].toggle_def, prompt[0]);
/*
.....move the toggle buttons after insert button one back
*/
	for ( i = UW_menu_item[pmenu].toggle_num-1; i>pchc; i--)
	{
		strcpy(UW_menu_item[pmenu].toggle[i].label, UW_menu_item[pmenu].toggle[i-1].label);
		strcpy(UW_menu_item[pmenu].toggle[i].func, UW_menu_item[pmenu].toggle[i-1].func);
		if (UW_menu_item[pmenu].toggle[i].params!=NULL)
			uu_free(UW_menu_item[pmenu].toggle[i].params);
		if (UW_menu_item[pmenu].toggle[i-1].params!=UU_NULL)
		{
			nc = strlen (UW_menu_item[pmenu].toggle[i-1].params);
			UW_menu_item[pmenu].toggle[i].params = uu_malloc((nc+1)*sizeof(char));
			strcpy(UW_menu_item[pmenu].toggle[i].params, UW_menu_item[pmenu].toggle[i-1].params);
		}
		else
			UW_menu_item[pmenu].toggle[i].params = UU_NULL;
	}
	strcpy(UW_menu_item[pmenu].toggle[pchc].label, prompt[0]);
	nc = strlen(prompt[1]);
	if (nc>0)
	{
		UW_menu_item[pmenu].toggle[pchc].params = uu_malloc((nc+1)*sizeof(char));
		strcpy(UW_menu_item[pmenu].toggle[pchc].params, prompt[1]);
	}
	else
		UW_menu_item[pmenu].toggle[pchc].params = UU_NULL;
	strcpy(UW_menu_item[pmenu].toggle[pchc].func, select_func);
	free (num[0]);
	free (num[1]);
	free (prompt[0]);
	free (prompt[1]);
	uw_mfresetmenu();
	menu_changed = 1;
}
	
/**********************************************************************
**    I_FUNCTION : toggle_deleteCB (widget, client_data, call_data)
**       callback function for delete menu, delete one button
**       from designing menu
**    PARAMETERS  
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Ignore
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void toggle_deleteCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	XmPushButtonCallbackStruct *cbs;
	XEvent *bevent ;
	int i, pmenu, pchc,nc;
	UDM_menu_toggle_struc *cbdata = (UDM_menu_toggle_struc *)client_data;
	cbs= (XmPushButtonCallbackStruct *)call_data;
	bevent = (XEvent *)cbs->event;
	if (bevent->type!=ButtonRelease) return; 
	pmenu =  cbdata->pos[1];
	pchc = cbdata->pos[2];
/*
.....move the menu buttons after delete button one forward
*/
	if (UW_menu_item[pmenu].toggle_num > 1)
	{
		UW_menu_item[pmenu].toggle_num--;
		for ( i = pchc; i<UW_menu_item[pmenu].toggle_num ; i++)
		{
			strcpy(UW_menu_item[pmenu].toggle[i].label,
				UW_menu_item[pmenu].toggle[i+1].label);
			strcpy(UW_menu_item[pmenu].toggle[i].func,
				UW_menu_item[pmenu].toggle[i+1].func);
			if (UW_menu_item[pmenu].toggle[i].params!=NULL)
				uu_free(UW_menu_item[pmenu].toggle[i].params);
			if (UW_menu_item[pmenu].toggle[i+1].params!=UU_NULL)
			{
				nc = strlen(UW_menu_item[pmenu].toggle[i+1].params);
				UW_menu_item[pmenu].toggle[i].params = uu_malloc((nc+1)*sizeof(char));
				strcpy(UW_menu_item[pmenu].toggle[i].params,
						UW_menu_item[pmenu].toggle[i+1].params);
			}
			else
				UW_menu_item[pmenu].toggle[i].params = NULL;
		}
		strcpy(UW_menu_item[pmenu].toggle_def,
			UW_menu_item[pmenu].toggle[pchc].label);
		uw_mfresetmenu();
		menu_changed = 1;
	}
}

/**********************************************************************
**    I_FUNCTION : insertCB (widget, client_data, call_data)
**       callback function for insert menu, insert one button
**       into designing menu
**    PARAMETERS  
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Ignore
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void insertCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	XmPushButtonCallbackStruct *cbs;
	XEvent *bevent ;
	cbs= (XmPushButtonCallbackStruct *)call_data;
	bevent = (XEvent *)cbs->event;
	if (bevent->type!=ButtonRelease) return; 
	insert_bt = (int) client_data;
	menu_changed = 1;
	uw_mfresetmenu();
	insert_bt = -1;
}
	

/**********************************************************************
**    I_FUNCTION : deleteCB (widget, client_data, call_data)
**       callback function for delete menu, delete one button
**       from designing menu
**    PARAMETERS  
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Ignore
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void deleteCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	XmPushButtonCallbackStruct *cbs;
	XEvent *bevent ;
	cbs= (XmPushButtonCallbackStruct *)call_data;
	bevent = (XEvent *)cbs->event;
	if (bevent->type!=ButtonRelease) return; 
	delete_bt = (int) client_data;
/*
.....free toggle space
*/
	if (UW_menu_type[delete_bt] == UW_TOGGLE_MENU)
	{
		if (UW_menu_item[delete_bt].toggle != NULL )
		{
			uu_free(UW_menu_item[delete_bt].toggle);
			UW_menu_item[delete_bt].toggle = 0;
		}
		UW_menu_type[delete_bt] = UW_BUTTON_MENU;
		UW_menu_item[delete_bt].toggle_num = 0;
	}
	menu_changed = 1;
	uw_mfresetmenu();
	delete_bt = -1;
}

/**********************************************************************
**    I_FUNCTION : toggleCB (widget, client_data, call_data)
**       callback function for toggle menu, set this menu style to toggle
**       from designing menu
**    PARAMETERS  
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Ignore
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void toggleCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	XmPushButtonCallbackStruct *cbs;
	XEvent *bevent ;
	int i, bt;
	cbs= (XmPushButtonCallbackStruct *)call_data;
	bevent = (XEvent *)cbs->event;
	if (bevent->type!=ButtonRelease) return; 
	bt = (int) client_data;
	if (UW_menu_type[bt] == UW_TOGGLE_MENU)
		return;
	UW_menu_type[bt] = UW_TOGGLE_MENU;
	UW_menu_item[bt].toggle = (UDM_menu_toggle_struc *)malloc(
										sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
	if (UW_menu_item[bt].toggle == NULL)
		uw_mfmessage((Widget)NULL, "Can not malloc space for toggle\n");
	for (i=0; i<UDM_MAX_TOGGLE; i++)
	{
		strcpy(UW_menu_item[bt].toggle[i].label, " ");
		strcpy(UW_menu_item[bt].toggle[i].func, " ");
		UW_menu_item[bt].toggle[i].params = UU_NULL;
	}
	UW_menu_item[bt].toggle_num = 1;
	strcpy(UW_menu_item[bt].toggle_def, " ");
	menu_changed = 1;
	uw_mfresetmenu();
}

/**********************************************************************
**    I_FUNCTION : colorCB (widget, client_data, call_data)
**       callback function for color menu, set this menu item's color
**      
**    PARAMETERS  
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Ignore
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void colorCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	XmPushButtonCallbackStruct *cbs;
	XEvent *bevent ;
	int stat, bt;
	char num[150], newcolor[40];
	Pixel color;
	cbs= (XmPushButtonCallbackStruct *)call_data;
	bevent = (XEvent *)cbs->event;
	if (bevent->type!=ButtonRelease) return; 
	bt = (int) client_data;
	sprintf(num, "Enter the Color for the selected menu item:");
	if (UW_menu_item[bt].bgcolor[0] != '\0')
		strcpy(newcolor, UW_menu_item[bt].bgcolor);
	else
		newcolor[0] = '\0';
	stat = uw_mfget_prompt(menu_win, "Get Menu Color", num, newcolor);
	if (stat<0) return;
	if (newcolor[0] != '\0')
	{
		stat = CvtColor(widget, newcolor, &color);
		if (stat==0) 
		{
			sprintf(num, "Color name \"%s\" is not defined, This menu item color is not changed", newcolor);
			uw_mfmessage(menu_win, num);
			return;
		}
		strcpy(UW_menu_item[bt].bgcolor, newcolor);
	}
	else
		UW_menu_item[bt].bgcolor[0] = '\0';
	UW_menu_item[bt].color[0] = -1;
	UW_menu_item[bt].color[1] = -1;
	UW_menu_item[bt].color[2] = -1;
	menu_changed = 1;
	uw_mfresetmenu();
}
	
	

/**********************************************************************
**    I_FUNCTION : bottonCB (widget, client_data, call_data)
**       callback function for toggle menu, set this menu style to toggle
**       from designing menu
**    PARAMETERS  
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Ignore
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void buttonCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	XmPushButtonCallbackStruct *cbs;
	XEvent *bevent ;
	int bt;
	cbs= (XmPushButtonCallbackStruct *)call_data;
	bevent = (XEvent *)cbs->event;
	if (bevent->type!=ButtonRelease) return; 
	bt = (int) client_data;
	if (UW_menu_type[bt] == UW_BUTTON_MENU)
		return;
/*
.....free toggle space
*/
	if (UW_menu_item[bt].toggle != NULL )
	{
		uu_free(UW_menu_item[bt].toggle);
		UW_menu_item[bt].toggle = 0;
	}
	UW_menu_item[bt].toggle_num = 0;
	UW_menu_type[bt] = UW_BUTTON_MENU;
	menu_changed = 1;
	uw_mfresetmenu();
}

/**********************************************************************
**    I_FUNCTION :  toggle_disp_statusCB(widget,client_data, event)
**      Display the label and function that select menu comtained 
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**				client_data = Contains program information about pushed
**				              button.
**				widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void toggle_disp_statusCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data, call_data;
{
	Widget stat_box, form, pane, action_area;
	XmString str1, str2, str3, str4;
	int i, n;
	char first[40], second[40], third[80], fourth[200], name[100];
	UDM_menu_toggle_struc *cbs = (UDM_menu_toggle_struc *)client_data;
	sprintf(first, "Toggle label: %s", cbs->label); 
	sprintf(second, "Function:    %s", cbs->func); 
	if (cbs->params==NULL)
		strcpy(third, "Paramter:");
	else
		sprintf(third, "Paramter:    %s", cbs->params); 
	fourth[0] = '\0';
	for(i=0; ; i++)
	{
		if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
		if (strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) continue;
/*
...change to upper case to compare
*/
      strcpy(name, UZ_keyfuncs[i].name);
      ul_to_upper(name);
      if (strcmp(name, cbs->func)==0)
      {
         sprintf(fourth, "%s", UZ_keyfuncs[i].descrip);
         break;
      }
   }
   if (fourth[0] == '\0')
		strcpy(fourth, "Description: No description available");

	str1 = XmStringCreateSimple(first);
	str2 = XmStringCreateSimple(second);
	str3 = XmStringCreateSimple(third);
	str4 = XmStringCreateSimple(fourth);
	stat_box = XtVaCreatePopupShell("status_disp",xmDialogShellWidgetClass,
					XtParent(widget),
					XmNdefaultPosition, False,
					XmNtitle,"Status Display",
					XmNdeleteResponse, XmDESTROY, NULL);
	XtAddCallback(stat_box, XmNpopupCallback, (XtCallbackProc)map_dialogCB, 
						(XtPointer)NULL);
	n = 0;
	pane = XtVaCreateWidget("pane",xmPanedWindowWidgetClass, stat_box,
				XmNsashWidth,1,
				XmNsashHeight,1,
				NULL);
	form = XtVaCreateWidget("contral_area", xmRowColumnWidgetClass,pane, NULL);

	XtVaCreateManagedWidget("first",xmLabelGadgetClass,form ,
					XmNlabelString, str1, NULL );
	XtVaCreateManagedWidget("second", xmLabelGadgetClass,form ,
					XmNlabelString, str2, NULL );
	XtVaCreateManagedWidget("third", xmLabelGadgetClass,form , 
					XmNlabelString, str3, NULL );
	XtVaCreateManagedWidget("fourth", xmLabelGadgetClass,form , 
					XmNlabelString, str4, NULL );
	XtManageChild(form);
	n = XtNumber(actionList);
	actionList[0].data = (XtPointer)stat_box;
	action_area = (Widget)uw_mfcreate_action(pane,actionList,n);

	XtManageChild(pane);
	XtPopup(stat_box, XtGrabExclusive); 
	XmStringFree(str1);
	XmStringFree(str2);
	XmStringFree(str3);
	
/*
	while(stat_box)
	{
		XtAppNextEvent(uw_mf.application,&revent);
		XtDispatchEvent(&revent);
	}
*/
}

/**********************************************************************
**    I_FUNCTION :  toggle_disp_choiceCB(widget,client_data, event)
**      Display a popup menu of choice 
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**				client_data = Contains program information about pushed
**				              button.
**				widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void toggle_disp_choiceCB (widget,
	client_data,
	event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	Widget popup, insert_opt, delete_opt, stat_opt;
	int n;
	Arg args[20];
	XButtonPressedEvent *bevent = (XButtonPressedEvent *) event;
	if(bevent->button != 3)
		return;
	n = 0;
	popup = XmCreatePopupMenu(widget, "popup", args, n);
	insert_opt = XtVaCreateManagedWidget("Insert", xmPushButtonGadgetClass,
						 popup, NULL);
	delete_opt = XtVaCreateManagedWidget("Delete", xmPushButtonGadgetClass,
						popup, NULL);
	stat_opt = XtVaCreateManagedWidget("Status", xmPushButtonGadgetClass,
						 popup, NULL);
	XtAddCallback(stat_opt, XmNactivateCallback, 
						(XtCallbackProc)toggle_disp_statusCB,
						client_data);
	XtAddCallback(insert_opt, XmNactivateCallback, 
						(XtCallbackProc)toggle_insertCB, 
						client_data);
	XtAddCallback(delete_opt, XmNactivateCallback, 
						(XtCallbackProc)toggle_deleteCB, 
						client_data);
	XmMenuPosition(popup, bevent);
	XtManageChild(popup);
}

/**********************************************************************
**    I_FUNCTION :  disp_choiceCB(widget,client_data, event)
**      Display a popup menu of choice 
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**				client_data = Contains program information about pushed
**				              button.
**				widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void disp_choiceCB (widget,
	client_data,
	event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	Widget popup, insert_opt, delete_opt, stat_opt, toggle_opt, 
				button_opt, color_opt;
	int number;
	int n;
	Arg args[20];
	XButtonPressedEvent *bevent = (XButtonPressedEvent *) event;
	if(bevent->button != 3)
		return;
	number = (int) client_data;
	n = 0;
	popup = XmCreatePopupMenu(widget, "popup", args, n);
	insert_opt = XtVaCreateManagedWidget("Insert", xmPushButtonGadgetClass,
						 popup, NULL);
	delete_opt = XtVaCreateManagedWidget("Delete", xmPushButtonGadgetClass,
						popup, NULL);
	toggle_opt = XtVaCreateManagedWidget("Toggle", xmPushButtonGadgetClass,
						popup, NULL);
	button_opt = XtVaCreateManagedWidget("Button", xmPushButtonGadgetClass,
						popup, NULL);
	stat_opt = XtVaCreateManagedWidget("Status", xmPushButtonGadgetClass,
						 popup, NULL);
	color_opt = XtVaCreateManagedWidget("Color", xmPushButtonGadgetClass,
						popup, NULL);
	if (UW_menu_type[number] == UW_TOGGLE_MENU)
		XtAddCallback(stat_opt, XmNactivateCallback,
			(XtCallbackProc)disp_status2CB, (XtPointer) number);
	else
		XtAddCallback(stat_opt, XmNactivateCallback, 
			(XtCallbackProc)disp_statusCB, (XtPointer) number);

	XtAddCallback(insert_opt, XmNactivateCallback, (XtCallbackProc)insertCB, 
						(XtPointer)number);
	XtAddCallback(delete_opt, XmNactivateCallback, (XtCallbackProc)deleteCB, 
						(XtPointer)number);
	XtAddCallback(toggle_opt, XmNactivateCallback, (XtCallbackProc)toggleCB, 
						(XtPointer)number);
	XtAddCallback(button_opt, XmNactivateCallback, (XtCallbackProc)buttonCB, 
						(XtPointer)number);
	XtAddCallback(color_opt, XmNactivateCallback, (XtCallbackProc)colorCB,
						(XtPointer)number);

	XmMenuPosition(popup, bevent);
	XtManageChild(popup);
	
}


/**********************************************************************
**    I_FUNCTION :  configureCB(widget,client_data, event)
**      Get the Configuration info and Reset related menu edit fields 
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**				client_data = Ignore
**				widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void configureCB (widget,
	client_data,
	event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	Position x, y;
	Dimension width, height;
	char num[30];
	XConfigureEvent *cevent = (XConfigureEvent *) event;
/*
.....For SGI, when reset menu, it does not have event type
.....ConfigureNotify but MapNotify
.....so added it
.....12/12/97 Yurong
*/
/*	if(cevent->type !=ConfigureNotify) */
	if ((cevent->type !=ConfigureNotify)&&(cevent->type !=MapNotify)) 
		return;
	if ((menu_first!=1)&&(cevent->type ==ConfigureNotify))
		menu_changed = 1;

/*
...get the position and height
*/
	XtVaGetValues(widget, XmNx, &x, XmNy, &y, NULL);
	XtVaGetValues(widget, XmNwidth, &width, XmNheight, &height, NULL);

/*
...save the position and height
...and reset into the design form
*/
	UMM_menu.size[1] = height;
	UMM_menu.size[0] = width;
	UMM_menu.pos[1] = y;
	UMM_menu.pos[0] = x;
/*
.....adjust size outside border
.....12/15/97 Yurong
*/
	if (UMM_menu.type!=UDM_MTYPE_MENU)
	{
		UMM_menu.size[0] = UMM_menu.size[0] + 2*XBorder[0];
		UMM_menu.size[1] = UMM_menu.size[1] + 2*YBorder[0];
	}
	else
	{
		UMM_menu.size[0] = UMM_menu.size[0] + 2*XBorder[1];
		UMM_menu.size[1] = UMM_menu.size[1] + XBorder[1] + YBorder[1];
	}
/*
.....adjust position on top of window (include border)
.....12/15/97 Yurong
*/
	if (UMM_menu.type!=UDM_MTYPE_MENU)
	{
		UMM_menu.pos[0] = UMM_menu.pos[0] - XBorder[0];
		UMM_menu.pos[1] = UMM_menu.pos[1] - YBorder[0];
	}
	else
	{
		UMM_menu.pos[0] = UMM_menu.pos[0] - XBorder[1];
		UMM_menu.pos[1] = UMM_menu.pos[1] - YBorder[1];
	}
	sprintf(num, "%d", UMM_menu.size[0]);
	XmTextSetString(size0, num);
	sprintf(num, "%d", UMM_menu.size[1]);
	XmTextSetString(size1, num);
	sprintf(num, "%d", UMM_menu.pos[0]);
	XmTextSetString(pos0, num);
	sprintf(num, "%d", UMM_menu.pos[1]);
	XmTextSetString(pos1, num);
	reset_flag = 1;
	menu_first = 0;

}

/**********************************************************************
**    I_FUNCTION : uw_mfmenulayout()
**		main function to design a menu	
**    PARAMETERS   
**       INPUT  : 
**             none
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfmenulayout()
{
	int i;
	if(desgn_win!=NULL)
		uw_mfmessage(menu_win, "Menu Design is already active!");
	else
	{
		menu_count = 0;
		menu_changed = 0;
		menu_first = 1;
		takedown = -1;
		menu_accept = 0;
		mfirst = 1;
		UMM_menu.type = UDM_MTYPE_MENU;
		uw_oldmenu_type = UDM_MTYPE_MENU;
		strcpy(UMM_menu.name, "SampleMenu");
		UMM_menu.file[0] = '\0';
	   	UMM_menu.pos[0] = 500;
  	 	UMM_menu.pos[1] = 200;
		UMM_menu.rows = 3;
		UMM_menu.cols = 10;
		UMM_menu.size[0] = 600;
		UMM_menu.size[1] = 200;
		for(i = 0; i< UDM_MAX_MENU; i++)
		{
			strcpy(UW_menu_item[i].name, " ");
			strcpy(UW_menu_item[i].file, " ");
			UW_menu_item[i].params = NULL;
			UW_menu_item[i].toggle = NULL;
			UW_menu_item[i].toggle_num = 0;
			UW_menu_item[i].bgcolor[0] = '\0';
			UW_menu_item[i].color[0] = -1;
			UW_menu_item[i].color[1] = -1;
			UW_menu_item[i].color[2] = -1;
			UW_menu_type[i] = UW_BUTTON_MENU;
		}
		uw_mfsetmenu();
		uw_mfmenuform();
		uw_mffunselect();
	}
}

/**********************************************************************
**    I_FUNCTION : uw_mfresetmenu()
**		Reset the sample menu	
**    PARAMETERS   
**       INPUT  : 
**             none
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfresetmenu()
{
	if (insert_bt!=-1)
	{
		uw_mfmenu_insert();
	}
	if (delete_bt!=-1)
	{
		uw_mfmenu_delete();
	}
/*
.....added a glag check because time problem
*/
	if (reset_flag==1)
	{
/*
...Destroy the menu window
*/
		XtUnrealizeWidget(menu_win);
/*
.....added a glag check because time problem
*/
		uw_mfsetmenu();
	}
}


/**********************************************************************
**    I_FUNCTION : uw_mfmenu_insert()
**			Rearrange the menu number and order to insert a empty design 
**			button
**    PARAMETERS   
**       INPUT  : 
**             none
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....Yurong added 9/11/97
*/
void uw_mfmenu_insert()
{
	int i, j, last;
/*
.....move the menu buttons after insert button one back
*/
	last = UMM_menu.cols * UMM_menu.rows - 1;
	for ( i = last; i>insert_bt; i--)
	{
		strcpy(UW_menu_item[i].name, UW_menu_item[i-1].name);
		strcpy(UW_menu_item[i].file, UW_menu_item[i-1].file);
		if (UW_menu_item[i-1].params!=NULL)
			strcpy(UW_menu_item[i].params, UW_menu_item[i-1].params);
		else
			UW_menu_item[i].params = NULL;
/*
.....added for toggle button
*/
		UW_menu_type[i] = UW_menu_type[i-1];
		strcpy(UW_menu_item[i].toggle_def, UW_menu_item[i-1].toggle_def);
		UW_menu_item[i].toggle_num = UW_menu_item[i-1].toggle_num;
		if (UW_menu_type[i]==UW_TOGGLE_MENU)
		{
			if (UW_menu_item[i].toggle==NULL)
			{
				UW_menu_item[i].toggle = (UDM_menu_toggle_struc *)malloc(
								sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
			}
			for (j=0; j<UW_menu_item[i].toggle_num; j++)
			{
				strcpy(UW_menu_item[i].toggle[j].label, 
										UW_menu_item[i-1].toggle[j].label);
				strcpy(UW_menu_item[i].toggle[j].func,
										UW_menu_item[i-1].toggle[j].func);
				strcpy(UW_menu_item[i].toggle[j].params,
										UW_menu_item[i-1].toggle[j].params);
			}
			if (UW_menu_item[i-1].toggle!=NULL)
			{
				uu_free(UW_menu_item[i-1].toggle);
				UW_menu_item[i-1].toggle = 0;
			}
		}
	}
	strcpy(UW_menu_item[insert_bt].name," ");
	strcpy(UW_menu_item[insert_bt].file,"KEY_NOOP");
	UW_menu_item[insert_bt].params[0] = '\0';
	UW_menu_item[insert_bt].toggle_num = 0;
	UW_menu_item[insert_bt].toggle = 0;
	UW_menu_type[insert_bt] = UW_BUTTON_MENU;
	menu_changed = 1;
}


/**********************************************************************
**    I_FUNCTION : uw_mfmenu_delete()
**			Rearrange the menu number and order to delete a design menu
**			button
**    PARAMETERS   
**       INPUT  : 
**             none
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....Yurong added 9/11/97
*/
void uw_mfmenu_delete()
{
	int i, j;
	int last;
/*
.....move the menu buttons after delete button one forward
*/
	last = UMM_menu.cols * UMM_menu.rows - 1;
	for ( i = delete_bt; i<last ; i++)
	{
		strcpy(UW_menu_item[i].name, UW_menu_item[i+1].name);
		strcpy(UW_menu_item[i].file, UW_menu_item[i+1].file);
		strcpy(UW_menu_item[i].params, UW_menu_item[i+1].params);
/*
.....added for toggle button
*/
		UW_menu_type[i] = UW_menu_type[i+1];
		strcpy(UW_menu_item[i].toggle_def, UW_menu_item[i+1].toggle_def);
		UW_menu_item[i].toggle_num = UW_menu_item[i+1].toggle_num;
		if (UW_menu_type[i]==UW_TOGGLE_MENU)
		{
/*
.....clear old toggle
*/
			if (UW_menu_item[i].toggle!=NULL)
			{
				uu_free(UW_menu_item[i].toggle);
				UW_menu_item[i].toggle = 0;
			}
/*
.....malloc new toggle
*/
			UW_menu_item[i].toggle = (UDM_menu_toggle_struc *)malloc(
							sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
			
			for (j=0; j<UW_menu_item[i].toggle_num; j++)
			{
				strcpy(UW_menu_item[i].toggle[j].label, 
										UW_menu_item[i+1].toggle[j].label);
				strcpy(UW_menu_item[i].toggle[j].func,
										UW_menu_item[i+1].toggle[j].func);
				strcpy(UW_menu_item[i].toggle[j].params,
										UW_menu_item[i+1].toggle[j].params);
			}
			if (UW_menu_item[i+1].toggle!=NULL)
			{
				uu_free(UW_menu_item[i+1].toggle);
				UW_menu_item[i+1].toggle=0;
			}
		}
	}
	strcpy(UW_menu_item[last].name," ");
	strcpy(UW_menu_item[last].file,"KEY_NOOP");
	UW_menu_item[last].params[0] = '\0';
	UW_menu_item[last].toggle = 0;
	UW_menu_type[last] = UW_BUTTON_MENU;
	UW_menu_item[last].toggle_num = 0;
	strcpy(UW_menu_item[last].toggle_def, " ");
	menu_changed = 1;
}

/**********************************************************************
**    I_FUNCTION :  popup_toggle(button,client_data,call_data)
**       Menu selection callback routine.  Performs user defined function
**			for pushed menu button.
**    PARAMETERS   
**       INPUT  : 
**          button      = Button widget pushed.
**				client_data = Contains program information about pushed
**				              button.
**				call_data   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void popup_toggle (button, client_data, call_data)
Widget button;
XtPointer client_data,call_data;
{
	Position x, y;
	Dimension width, height;
	XEvent event; 
	XmPushButtonCallbackStruct *cbs1 = (XmPushButtonCallbackStruct *) call_data;
	XEvent  *bevent = cbs1->event;
	Widget cbs = (Widget)client_data;

	XtPopup(cbs, XtGrabNone); 
	XtVaGetValues(cbs, XmNwidth, &width, XmNheight, &height, NULL);
/*
.....at least = minWidth
*/
	if (width<50) width = 50;
	x = (bevent->xbutton).x_root-width;
	y = (bevent->xbutton).y_root;
	XtMoveWidget(cbs, x, y);
	while (1)
	{
		uw_mfsetcursor(1);
		XtAppNextEvent(uw_mf.application,&event);
		if (event.type == ButtonPress)
/*
.....if button pressed inside choice menu
.....XtDispatchEvent to let event handler or
.....callback handle it
.....otherwise destroy the choice menu
.....and Dispatch this event
.....Yurong 4/2/99
*/
		{
			if ((event.xbutton.x_root>x)&& 
					(event.xbutton.x_root<x+width)&&
					(event.xbutton.y_root>y)&&
					(event.xbutton.y_root<y+height))
			{
				XtDispatchEvent(&event);
			}
			else
			{
				XtPopdown(cbs);
				XtDispatchEvent(&event);
				break;
			}
		}
		else
			XtDispatchEvent(&event);
	}
}

/**********************************************************************
**    I_FUNCTION : uw_mfsetmenu()
**			Display a sample menu to edit
**    PARAMETERS   
**       INPUT  : 
**             none
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfsetmenu()
{
	Arg args[20];
	Cardinal n;
	Atom watom;
	int i, j, k, m, menu_number;
	int ifl;
	int menu_pos[2];
	Widget but,toggle_menu, opt_menu;
	Widget choice[UDM_MAX_TOGGLE];
	char geom[20],buf[120], geom1[20];
	char newlabel[80];
	char bgcolor[40];
	Pixel ibgcolor;
	extern void uw_mfmd_cancelCB();
	extern void uw_mfenter_handler(), uw_mfleave_handler();
	n = 0;
	reset_flag = 0;
	XtSetArg(args[n],XmNkeyboardFocusPolicy,  XmPOINTER); n++;
/*
........Icon and Pop-up menus
........Do not contain Window Title & Resize borders
*/
	if (UMM_menu.type != UDM_MTYPE_MENU)
	{
		ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
				MWM_DECOR_MINIMIZE | MWM_DECOR_TITLE | MWM_DECOR_RESIZEH;
		XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
	}
/*
...Position and Size menu
*/
	if (Border_stat==1)
	{
		menu_pos[0] = UMM_menu.pos[0];
		menu_pos[1] = UMM_menu.pos[1];
	}
	else
	{
		if (UMM_menu.type == UDM_MTYPE_MENU)
		{
			menu_pos[0] = UMM_menu.pos[0] + XBorder[1];
			menu_pos[1] = UMM_menu.pos[1] + YBorder[1];
		}
		else
		{
			menu_pos[0] = UMM_menu.pos[0] + XBorder[0];
			menu_pos[1] = UMM_menu.pos[1] + YBorder[0];
		}
	}
		
	if (UMM_menu.type!=UDM_MTYPE_MENU)
	{
		UMM_menu.size[0] = UMM_menu.size[0] - 2*XBorder[0];
		UMM_menu.size[1] = UMM_menu.size[1] - 2*YBorder[0];
	}
	else
	{
		UMM_menu.size[0] = UMM_menu.size[0] - 2*XBorder[1];
		UMM_menu.size[1] = UMM_menu.size[1] - XBorder[1] - YBorder[1];
	}
	sprintf(geom,"%dx%d+%d+%d", UMM_menu.size[0], UMM_menu.size[1],
			menu_pos[0], menu_pos[1]);
	XtSetArg(args[n],XmNgeometry,geom); n++;
	XtSetArg(args[n], XmNmappedWhenManaged, False); n++;
	menu_win = XtAppCreateShell("SAMPLE_MENU",NULL, topLevelShellWidgetClass,
				XtDisplay(uw_mf.parent), args, n);

	if (menu_win == NULL) goto failed;
/*
.....Trap the CLOSE button
*/
	watom = XmInternAtom(uw_xw.disp,"WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(menu_win,watom,uw_mfmd_cancelCB,
			(XtPointer)NULL);
/*
.....Set the window title
*/
	XtVaSetValues(menu_win, XmNtitle, UMM_menu.name, NULL);
/*
.....Create a Form widget
......to hold menu entries
*/
	n = 0;
	XtSetArg(args[n], XmNfractionBase, UMM_menu.cols*UMM_menu.rows);
	n++;
	menu_layout = XtCreateWidget("Menu_mang",xmFormWidgetClass,
						menu_win, args, n);
	if (menu_layout == NULL) goto failed;
/*
.....Put up menu entries
*/
	for (i=0;i<UMM_menu.rows;i++)
	{
/*
........Add columns to each row
*/
		for (j=0;j<UMM_menu.cols;j++)
		{
			n = 0;
			XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNtopPosition,i*UMM_menu.cols); n++;
			XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNbottomPosition,(i+1)*UMM_menu.cols); n++;
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNleftPosition,j*UMM_menu.rows); n++;
			XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNrightPosition,(j+1)*UMM_menu.rows); n++;
			XtSetArg(args[n],XmNhighlightOnEnter, True); n++;
			XtSetArg(args[n],XmNhighlightPixmap, XmUNSPECIFIED_PIXMAP); n++;
			menu_number = i*UMM_menu.cols + j;
/*
.....added choice button
*/
			if (UW_menu_item[menu_number].toggle!=NULL)
			{
				UW_menu_type[menu_number] = UW_TOGGLE_MENU;
/*
.....first create default button
*/
				but = XtCreateManagedWidget(UW_menu_item[menu_number].toggle_def,
						xmPushButtonWidgetClass,menu_layout,args,n);
				XtAddEventHandler(but, ButtonPressMask, False,
										(XtEventHandler)disp_choiceCB,
										(XtPointer)menu_number);
				XtAddEventHandler(but, EnterWindowMask, False,
						(XtEventHandler)uw_mfenter_handler,
						&(UW_menu_item[menu_number]));
				XtAddEventHandler(but, LeaveWindowMask, False,
						(XtEventHandler)uw_mfleave_handler,
						&(UW_menu_item[menu_number]));
/*
...........Create the shell window
...........To hold the choices
*/
				n = 0;
				XtSetArg(args[n],XmNmappedWhenManaged,False); n++;
				XtSetArg(args[n],XmNkeyboardFocusPolicy,  XmPOINTER); n++;
				ifl = 0;
				XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
				XtSetArg(args[n],XmNborderWidth, 0); n++;
				sprintf(geom1,"30x10+10+10");
				XtSetArg(args[n],XmNgeometry,geom1); n++;
				sprintf(buf,"choice_%d",k);
				XtSetArg(args[n],XmNkeyboardFocusPolicy,  XmPOINTER); n++;
				XtSetArg(args[n],XmNminHeight, 30); n++;  
				XtSetArg(args[n],XmNminWidth, 50); n++;  
				XtSetArg(args[n],XmNisAligned, True); n++;
				XtSetArg(args[n],XmNentryAlignment, XmALIGNMENT_CENTER); n++;
				toggle_menu = (Widget)XtCreatePopupShell("SubMenu",
										xmDialogShellWidgetClass, but,
										args,n);
				if (toggle_menu == NULL)
					goto failed;
/*
.....Create a RowColumnWidget
......to hold menu entries
*/
				n=0;
				XtSetArg(args[n],XmNspacing, 0); n++;
				XtSetArg(args[n],XmNborderWidth, 0); n++;
				XtSetArg(args[n],XmNminHeight, 30); n++;  
				XtSetArg(args[n],XmNminWidth, 50); n++;  
				XtSetArg(args[n],XmNisAligned, True); n++;
				XtSetArg(args[n],XmNentryAlignment, XmALIGNMENT_CENTER); n++;
				opt_menu = (Widget)XtCreateWidget("choice",
									xmRowColumnWidgetClass, toggle_menu, args,n);
				if (opt_menu == NULL) goto failed;
				for (m=0; m<UW_menu_item[menu_number].toggle_num; m++)
				{
					strcpy(newlabel,
							UW_menu_item[menu_number].toggle[m].label);
/*
					len = strlen(newlabel);
					if (len<6)
					{
						for (l=len; l<6; l++)
							newlabel[l] = ' ';
						newlabel[6] = '\0';
					}
*/
					choice[m] = XtVaCreateManagedWidget( newlabel,
						xmPushButtonWidgetClass, opt_menu, 
						XmNisAligned, True,
						XmNalignment, XmALIGNMENT_CENTER,
						XmNhighlightOnEnter, True,
						XmNfillOnArm, True,
						XmNhighlightThickness, 2,
						XmNshadowThickness, 2,
						NULL);
/*
					choice[m] = XtVaCreateManagedWidget( newlabel,
						xmPushButtonGadgetClass, opt_menu, NULL);
*/

					UW_menu_item[menu_number].toggle[m].pos[0] = -1;
					UW_menu_item[menu_number].toggle[m].pos[1] = menu_number;
					UW_menu_item[menu_number].toggle[m].pos[2] = m;
					XtAddCallback(choice[m],XmNactivateCallback,toggle_askedCB,
						&(UW_menu_item[menu_number].toggle[m]));
					XtAddEventHandler(choice[m], ButtonPressMask, False,
										(XtEventHandler)toggle_disp_choiceCB,
										&(UW_menu_item[menu_number].toggle[m]));
					XtAddEventHandler(choice[m], EnterWindowMask, False,
										(XtEventHandler)uw_mfenter_handler,
										&(UW_menu_item[menu_number]));
					XtAddEventHandler(choice[m], LeaveWindowMask, False,
										(XtEventHandler)uw_mfleave_handler,
										&(UW_menu_item[menu_number]));
					if (UW_menu_item[menu_number].bgcolor[0]!='\0')
					{
						strcpy(bgcolor, UW_menu_item[menu_number].bgcolor);
						XtVaSetValues(choice[m], XtVaTypedArg, XmNbackground,
									XmRString, bgcolor, strlen(bgcolor)+1, NULL);
						XtVaGetValues(choice[m], XmNbackground, &ibgcolor, NULL);
/*
.....removed because only MOTIF 1.2 support it,
.....we call uw_mfChangeColor(widget, color)
.....It use MOTIF 1.1
.....Yurong 5/6/99
*/
/*
						XmChangeColor(choice[m], ibgcolor);
*/
						uw_mfChangeColor(choice[m], ibgcolor);
					}
				}
				XtManageChild(opt_menu);
				XtRealizeWidget(toggle_menu);
				XtAddCallback(but, XmNactivateCallback,popup_toggle,
					toggle_menu);
				if (UW_menu_item[menu_number].bgcolor[0]!='\0')
				{
					strcpy(bgcolor, UW_menu_item[menu_number].bgcolor);
					XtVaSetValues(but, XtVaTypedArg, XmNbackground,
								XmRString, bgcolor, strlen(bgcolor)+1, NULL);
					XtVaGetValues(but, XmNbackground, &ibgcolor, NULL);
/*
.....removed because only MOTIF 1.2 support it,
.....we call uw_mfChangeColor(widget, color)
.....It use MOTIF 1.1
.....Yurong 5/6/99
*/
/*
					XmChangeColor(but, ibgcolor);
*/
					uw_mfChangeColor(but, ibgcolor);
				}
			}
			else
			{

				but = XtCreateManagedWidget(UW_menu_item[menu_number].name,
						xmPushButtonWidgetClass,menu_layout,args,n);
				XtAddCallback(but,XmNactivateCallback, (XtCallbackProc)asked_meCB,
								(XtPointer) menu_number);
				XtAddEventHandler(but,ButtonPressMask, False, 
										(XtEventHandler)disp_choiceCB,
										(XtPointer) menu_number);
				XtAddEventHandler(but, EnterWindowMask, False,
						(XtEventHandler)uw_mfenter_handler,
						&(UW_menu_item[menu_number]));
				XtAddEventHandler(but, LeaveWindowMask, False,
						(XtEventHandler)uw_mfleave_handler,
						&(UW_menu_item[menu_number]));
				if (UW_menu_item[menu_number].bgcolor[0]!='\0')
				{
					strcpy(bgcolor, UW_menu_item[menu_number].bgcolor);
					XtVaSetValues(but, XtVaTypedArg, XmNbackground,
								XmRString, bgcolor, strlen(bgcolor)+1, NULL);
					XtVaGetValues(but, XmNbackground, &ibgcolor, NULL);
/*
.....removed because only MOTIF 1.2 support it,
.....we call uw_mfChangeColor(widget, color)
.....It use MOTIF 1.1
.....Yurong 5/6/99
*/
/*
					XmChangeColor(but, ibgcolor);
*/
					uw_mfChangeColor(but, ibgcolor);
				}
			}
		}
	}
	XtManageChild(menu_layout);
	XtAddEventHandler(menu_win, StructureNotifyMask, False, 
							(XtEventHandler)configureCB, (XtPointer)NULL);

	XtRealizeWidget(menu_win);
	XtMapWidget(menu_win);
	mfirst = 0;
	uw_oldmenu_type = UMM_menu.type;
	goto done;
/*
.....Error trying to create menu
*/
failed:;
	sprintf (buf, "Could not create Menu: %s\n", UMM_menu.name);
	if (menu_form!=NULL)
		uw_mfmessage(menu_form, buf);
	else
		uw_mferror(buf);
	
/*
.....End of routine
*/
done:;
	return;
}

/**********************************************************************
**    I_FUNCTION : uw_mffunselect()
**		Display a window for select function	
**    PARAMETERS   
**       INPUT  : 
**            none
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mffunselect()
{
	Arg args[20];
	Cardinal n;
	int i, j, lenth;
	char line[80];
	Atom watom;
	int is1[2],ip1[2];
	Widget but,but1,search,func_form;
	char geom[20],buf[120];
	XmString xmstring;
	extern void uw_mfmd_cancelCB();

	n = 0;
	is1[0]=600;
	is1[1]=300;
	ip1[0]=50;
	ip1[1]=650;
	sprintf(geom,"%dx%d+%d+%d",is1[0],is1[1],ip1[0],ip1[1]);
   XtSetArg(args[n],XmNgeometry,geom); n++;
/*
...Create a window to hold function list and search button
*/
	funlist_win = XtAppCreateShell("FUNCTION_SELECT",NULL, topLevelShellWidgetClass,
				XtDisplay(uw_mf.parent), args, n);
	if (funlist_win == NULL) goto failed;
/*
.....Trap the CLOSE button
*/
	watom = XmInternAtom(uw_xw.disp,"WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(funlist_win,watom,uw_mfmd_cancelCB,
			(XtPointer)NULL);
/*
...Create a form to hold function list and search button
*/
	func_form = XtCreateWidget("func_form",xmFormWidgetClass,
						funlist_win, args, n);
	if (func_form == NULL) goto failed;
	
/*
...Create a function list to hold all functions
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,80); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;

	XtSetArg(args[n], XmNselectionPolicy, XmSINGLE_SELECT); n++;
	XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
	func_select = (Widget)XmCreateScrolledList(func_form,"funclist",args, n);
	strcpy(line, "MENUS");
	lenth = strlen(line);
/*
...first row for select menu
*/
	for(j = lenth;  j< 30; j++)
	{
		line[j] = ' ';
	}
	line[j] = '\0';
	strcat(line, "Select here to see menu files");
	strcpy(menu_list_item, line);
	xmstring = XmStringCreateSimple(line);
	XmListAddItem(func_select, xmstring, 0);
	XmStringFree(xmstring);
/*
...Input all functions and descriptions
*/
	for(i = 0; ; i++)
	{
		if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
		if (strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) continue;
		strcpy(line, UZ_keyfuncs[i].name);
		lenth = strlen(UZ_keyfuncs[i].name);
		for(j = lenth;  j< 30; j++)
		{
			line[j] = ' ';
		}
		line[j] = '\0';
		strcat(line, UZ_keyfuncs[i].descrip);
		xmstring = XmStringCreateSimple(line);
		XmListAddItem(func_select, xmstring, 0);
		XmStringFree(xmstring);
	}
	XtAddCallback(func_select, XmNsingleSelectionCallback,
                     (XtCallbackProc)fun_selectCB , NULL);
	XmListSelectPos(func_select, 2, True);
	XtManageChild(func_select);
/*
...Create a text field and button for search string
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,85); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,95); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;

	but1 = XtCreateManagedWidget("Search string:",
					xmLabelWidgetClass,func_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,85); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,95); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, but1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,80); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;

	but = XtCreateManagedWidget("Text10.dsgn",
			xmTextWidgetClass,func_form,args,n);
	XtAddCallback(but,XmNactivateCallback, (XtCallbackProc)searchCB,
							 (XtPointer)but);
	n  = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,85); n++; 
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,95); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 80); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,95); n++; 
	search = XmCreatePushButton(func_form, "Search", args, n);
	XtAddCallback(search, XmNactivateCallback, (XtCallbackProc)searchCB,
							 (XtPointer)but);
	XtManageChild(search);
	XtManageChild(func_form);
	
	XtRealizeWidget(funlist_win);
	XtMapWidget(funlist_win);
	goto done;
/*
.....Error trying to create menu
*/
failed:;
	ud_wrerr(buf);
/*
.....End of routine
*/
done:;
	return;
}	

/**********************************************************************
**    I_FUNCTION : uw_mfremove_desgn_menu()
**			remove the all menu design window
**    PARAMETERS   
**       INPUT  : 
**            none
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfremove_desgn_menu()
{
	if((menu_win==NULL)||(desgn_win==NULL)||(funlist_win==NULL))
      return 1;
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
	takedown = -1;
	return 0;
}	
/**********************************************************************
**    I_FUNCTION :uw_mfismd() 
**			Check if it is in menu design phase
**    PARAMETERS   
**       INPUT  : 
**            none
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfismd()
{
	if (desgn_win==NULL) 
		return 0;
	else
		return 1;
}

/**********************************************************************
**    I_FUNCTION :uw_mfraise_menud()
**		raise menu design widows	
**    PARAMETERS   
**       INPUT  : 
**            none
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfraise_menud()
{
	if((menu_win==NULL)||(desgn_win==NULL)||(funlist_win==NULL))
      return 1;
	else
	{
		XRaiseWindow(XtDisplay(menu_win), XtWindow(menu_win));
		XRaiseWindow(XtDisplay(desgn_win), XtWindow(desgn_win));
		XRaiseWindow(XtDisplay(funlist_win), XtWindow(funlist_win));
	}
	return 0;
}
#endif
#endif
