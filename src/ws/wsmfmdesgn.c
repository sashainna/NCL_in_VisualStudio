#include "usysdef.h"
#if UU_COMP != UU_VAXVMS || UU_OPSYS == UU_ALPHAVMS
#if UU_COMP!=UU_WIN2K

/*********************************************************************
**	FILENAME: wsmenudesgn.c
**	CONTAINS:	
**					uw_mfmenuform
**					change_rowupCB
**					change_rowdownCB
**					change_colsupCB
**					change_colsdownCB
**					change_widthupCB
**					change_widthdownCB
**					change_htupCB
**					change_htdownCB
**					change_xupCB
**					change_xdownCB
**					change_yupCB
**					change_ydownCB
**					save_rowCB
**					save_colsCB
**					save_titleCB
**					save_widthCB
**					save_heightCB
**					save_posxCB
**					save_posyCB
**					save_typeCB
**					browseCB
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wsmfmdesgn.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:11
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
#include <decw$include:PanedW.h>
#include <decw$include:Form.h>
#include <decw$include:Text.h>
#include <decw$include:ArrowB.h>
#include <decw$include:Protocols.h>
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
#include <Xm/ToggleB.h>
#endif
#include "xfsys1.h"
#include "uhep.h"
#include "xenv1.h"
#include "wsmf.h"
#include "dmotif.h"
#include "usysdef.h"
#include "wsxw.h"
#define WS_MENUDEGN
#include "wsmfmdesgn.h"
#undef WS_MENUDEGN
/*
.....Removed following variables
.....all define in wsmfmdesgn.h
.....Yurong 8/26/97
*/
/*extern Widget menu_form;
extern Widget desgn_win; */
/*
...Text Widget for input Menu size and Position
*/
/*extern Widget row_area, col_area, size0, size1, pos0, pos1, 
					title_area, file_area ,titled_opt, icon_opt, popup_opt, option; 
*/
/*
...save menu structure
*/
/*
extern UDM_MENU UMM_menu;
extern int menu_changed;
extern int mfirst ; */
/*
.....added by Yurong
.....8/27/97
*/
extern UWS_MF uw_mf;
/*
...check for if push Accept button 
*/
extern void uw_mfmd_acceptCB(), uw_mfmd_loadfileCB(), uw_mfmd_cancelCB(),
				uw_mfmd_savefileCB();
typedef struct
{
	char *label;
	void (*callback)();
	XtPointer data;
} ActionAreaItem;

static ActionAreaItem ZactionList[] =
{
	{"ACCEPT", uw_mfmd_acceptCB, NULL},
	{"LOAD", uw_mfmd_loadfileCB, NULL},
	{"SAVE AS", uw_mfmd_savefileCB, NULL},
	{"CANCEL", uw_mfmd_cancelCB,NULL}
};

/**********************************************************************
**    I_FUNCTION :change_rowupCB (widget, client_data, call_data)
**       callback function for up arrow right to row number text field 
**       in menu design form. Increase the row number  by 1 and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void change_rowupCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int rows;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			rows = atoi(text);
			rows++;
			UMM_menu.rows = rows;
			sprintf(num, "%d", UMM_menu.rows);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :change_rowdownCB (widget, client_data, call_data)
**       callback function for  down arrow right to row number text field 
**       in menu design form. Decrease the row number  by 1 and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_rowdownCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int rows;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			rows = atoi(text);
			if(rows>1)
				rows--;
			UMM_menu.rows = rows;
			sprintf(num, "%d", UMM_menu.rows);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :change_colsupCB (widget, client_data, call_data)
**       callback function for up arrow right to column number text field 
**       in menu design form. Increase the column number  by 1 and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_colsupCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int cols;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			cols = atoi(text);
			cols++;
			UMM_menu.cols = cols;
			sprintf(num, "%d", UMM_menu.cols);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :change_colsdownCB (widget, client_data, call_data)
**       callback function for  down arrow right to column number text field 
**       in menu design form. Decrease the column number  by 1 and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_colsdownCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int cols;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			cols = atoi(text);
			if(cols>1)
				cols--;
			UMM_menu.cols = cols;
			sprintf(num, "%d", UMM_menu.cols);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :change_widthupCB (widget, client_data, call_data)
**       callback function for up arrow right to width number text field 
**       in menu design form. Increase the width  by 10 pixel and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_widthupCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int width;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			width = atoi(text);
			width += 10;
			UMM_menu.size[0] = width;
			sprintf(num, "%d", UMM_menu.size[0]);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}


/**********************************************************************
**    I_FUNCTION :change_widthdownCB (widget, client_data, call_data)
**       callback function for  down arrow right to width  number text field 
**       in menu design form. Decrease the width  by 10 pixel and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_widthdownCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int width;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			width = atoi(text);
			if(width>=10)
				width -= 10;
			UMM_menu.size[0] = width;
			sprintf(num, "%d", UMM_menu.size[0]);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :change_htupCB (widget, client_data, call_data)
**       callback function for up arrow right to height number text field 
**       in menu design form. Increase the height  by 10 pixel and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_htupCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int height;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			height = atoi(text);
			height += 10;
			UMM_menu.size[1] = height;
			sprintf(num, "%d", UMM_menu.size[1]);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :change_htdownCB (widget, client_data, call_data)
**       callback function for  down arrow right to height number text field 
**       in menu design form. Decrease the height  by 10 pixel and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_htdownCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int height;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			height = atoi(text);
			if(height>=10)
				height -= 10;
			UMM_menu.size[1] = height;
			sprintf(num, "%d", UMM_menu.size[1]);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :change_xupCB (widget, client_data, call_data)
**       callback function for up arrow right to pos_x number text field 
**       in menu design form. Increase the pos_x by 10 pixel and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_xupCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int pos_x;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			 pos_x = atoi(text);
			 pos_x += 10;
			UMM_menu.pos[0] = pos_x;
			sprintf(num, "%d", UMM_menu.pos[0]);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :change_xdownCB (widget, client_data, call_data)
**       callback function for down arrow right to pos_x number text field 
**       in menu design form. Decrease the pos_x by 10 pixel and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_xdownCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int pos_x;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			pos_x = atoi(text);
			pos_x -= 10;
			UMM_menu.pos[0] = pos_x;
			sprintf(num, "%d", UMM_menu.pos[0]);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :change_yupCB (widget, client_data, call_data)
**       callback function for up arrow right to pos_y number text field 
**       in menu design form. Increase the pos_y by 10 pixel and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_yupCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int pos_y;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			 pos_y = atoi(text);
			 pos_y += 10;
			UMM_menu.pos[1] = pos_y;
			sprintf(num, "%d", UMM_menu.pos[1]);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :change_ydownCB (widget, client_data, call_data)
**       callback function for down arrow right to pos_y number text field 
**       in menu design form. Decrease the pos_y by 10 pixel and reset
**       sample menu
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text widget
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void change_ydownCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	int pos_y;
	char num[30];
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			pos_y = atoi(text);
			if(pos_y>=10)
				pos_y -= 10;
			UMM_menu.pos[1] = pos_y;
			sprintf(num, "%d",UMM_menu.pos[1]);
			XmTextSetString(client_data, num);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :save_rowCB (widget, client_data, call_data)
**       callback function for Row number text field in menu design form
**       save the select menu row number
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text field
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void save_rowCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			UMM_menu.rows = atoi(text);
			if(UMM_menu.rows!=0)
				uw_mfresetmenu();
		}
		XtFree(text);
	}
}


/**********************************************************************
**    I_FUNCTION :save_colsCB (widget, client_data, call_data)
**       callback function for column text field in menu design form
**       save the select menu column number
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text field
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void save_colsCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			UMM_menu.cols = atoi(text);
			if(UMM_menu.cols!=0)
				uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :save_titleCB (widget, client_data, call_data)
**       callback function for Title text field in menu design form
**       save the select menu name
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text field
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void save_titleCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			strcpy(UMM_menu.name,text); 
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}


/**********************************************************************
**    I_FUNCTION :save_widthCB (widget, client_data, call_data)
**       callback function for width text field in menu design form
**       save the select menu size[1]
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text field
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void save_widthCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			UMM_menu.size[0] = atoi(text);
			if(UMM_menu.size[0]!=0)
				uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :save_heightCB (widget, client_data, call_data)
**       callback function for height text field in menu design form
**       save the select menu size[0]
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text field
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void save_heightCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			UMM_menu.size[1] = atoi(text);
			if(UMM_menu.size[1]!=0)
				uw_mfresetmenu();
		}
		XtFree(text);
	}
}

/**********************************************************************
**    I_FUNCTION :save_posxCB (widget, client_data, call_data)
**       callback function for position_x text field in menu design form
**       save the select menu pos[0]
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text field
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void save_posxCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			UMM_menu.pos[0] = atoi(text);
			if(UMM_menu.pos[0]>=0)
				uw_mfresetmenu();
		}
		XtFree(text);
	}
}


/**********************************************************************
**    I_FUNCTION :save_posyCB (widget, client_data, call_data)
**       callback function for position_y text field in menu design form
**       save the select menu pos[1]
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = text field.
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void save_posyCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	Widget text_widget;
	char *text;
	text_widget = (Widget) client_data;
	menu_changed = 1;
	if(text_widget!=(Widget)NULL)
	{
		text = XmTextGetString(text_widget);
		if(text!=NULL)
		{
			UMM_menu.pos[1] = atoi(text);
			uw_mfresetmenu();
		}
		XtFree(text);
	}
}


/**********************************************************************
**    I_FUNCTION :save_typeCB (widget, client_data, call_data)
**       callback function for option menu in menu design form
**       save the select menu type
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = select label.
**				call_data   = Ignore
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void save_typeCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	menu_changed = 1;
	uw_oldmenu_type = UMM_menu.type;
	if (XmToggleButtonGetState(widget))
	{
		if(strcmp((char*)client_data, "Titled_Menu")==0)
		{
			if (uw_oldmenu_type == UDM_MTYPE_MENU)
				return;
			UMM_menu.type = UDM_MTYPE_MENU;
			uw_mfresetmenu();
		}
		else if(strcmp((char*)client_data, "Icon_Menu")==0)
		{
			UMM_menu.type = UDM_MTYPE_ICON;
			if (uw_oldmenu_type != UDM_MTYPE_MENU)
				return;
			uw_mfresetmenu();
		}
		else if(strcmp((char*)client_data, "Popup_Menu")==0)
		{
			UMM_menu.type = UDM_MTYPE_POPUP;
			if (uw_oldmenu_type != UDM_MTYPE_MENU)
				return;
			uw_mfresetmenu();
		}
	}
}


/**********************************************************************
**    I_FUNCTION :  browseCB(widget,clientData,callData)
**       Callback function for browse button 
**       display a file select dialog 
**    PARAMETERS   
**       INPUT  : 
**          widget     = browse button
**				clientData = Ignored
**				callData   = Ignored
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void browseCB (widget,
	client_data,
	call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	int len;
	UX_pathname old_fname, dir, dir2, fullname;
	int mode, status, found;
	char *pathlistptr = 0;
	static int browse_open = 0;

	static int nfld=3,ifld=0;
	static char *ftext[3]={"System","Local","File Tree"};
/*
.....we don't use NCL_USER_MENU any more, but use "UU_USER_SETTTING/menu"
*/
	static char ldir[3][UX_MAX_PATH_LEN]; /*={"NCL_MENU","NCL_USER_MENU","."}; */
	static char *fdir[3]={ldir[0],ldir[1],ldir[2]};

/*
.....changed for adding default filename
.....Yurong 2/4/97
*/
	static UX_pathname filename = "\0";
	if (browse_open == 1)
		return;
	browse_open = 1;
	strcpy(old_fname, filename);
	strcpy(ldir[0],"NCL_MENU");
/*	strcpy(ldir[1],"NCL_USER_MENU"); */
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
	if (len!=0)
		XmTextSetString(file_area, filename);
	else
		strcpy(filename, old_fname);
	browse_open = 0;
}


/**********************************************************************
**    I_FUNCTION : uw_mfmenuform()
**		Display a menu design form	
**    PARAMETERS   
**       INPUT  : 
**            none
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfmenuform()
{
	Arg args[20];
	Cardinal n;
	Atom watom;
	char num[80];
	int is2[2], ip2[2];
	Widget but1,row,arrow1, arrow2, browse;
	Widget  pane, action_area, uw_mfcreate_action();
	char geom[20],buf[120];
/*
...Create a pane to hold the menu design form and design contral button
*/		
	n = 0;
	is2[0]=450;
	is2[1]=550;
	ip2[0]=50;
	ip2[1]=50;
	sprintf(geom,"%dx%d+%d+%d",is2[0],is2[1],ip2[0],ip2[1]);
   XtSetArg(args[n],XmNgeometry,geom); n++;
	desgn_win = XtAppCreateShell("MENU_DESIGN",NULL, topLevelShellWidgetClass,
				XtDisplay(uw_mf.parent), args, n);
	if (desgn_win == NULL) goto failed;
/*
.....Trap the CLOSE button
*/
	watom = XmInternAtom(uw_xw.disp,"WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(desgn_win, watom,uw_mfmd_cancelCB,
			(XtPointer)NULL);
/*
...Create a pane to hold the menu design form and design contral button
*/
	pane = XtVaCreateWidget("menu_desgn", xmPanedWindowWidgetClass, desgn_win,
									XmNsashWidth, 1,
									XmNsashHeight, 1,NULL);
	n = 0;
/*
...Create a form to hold the menu design field
*/
	menu_form = XtCreateWidget("Menu_form",xmFormWidgetClass,
						pane, args, n);
	if (menu_form == NULL) goto failed;
	
/*
...Create first row in menu design form. it is for menu rows
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,5); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,15); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,2); n++;

	but1 = XtCreateManagedWidget("Number of rows:",
					xmLabelWidgetClass,menu_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 5); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,15); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, but1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,45); n++; 
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;

	row_area = XtCreateManagedWidget("Text1.dsgn",
					xmTextWidgetClass,menu_form,args,n);
	sprintf(num, "%d", UMM_menu.rows);
	XmTextSetString(row_area, num);
	XtAddCallback(row_area,XmNactivateCallback, (XtCallbackProc)save_rowCB,
						(XtPointer) row_area);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,5); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,10); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 45); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 50); n++; 
	XtSetArg(args[n],XmNarrowDirection, XmARROW_UP); n++;
	arrow1 = XtCreateManagedWidget("arrow1",
					xmArrowButtonWidgetClass,menu_form,args,n);
	
	XtAddCallback(arrow1,XmNactivateCallback, (XtCallbackProc)change_rowupCB,
						(XtPointer) row_area); 
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 10); n++; 
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,15); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 45); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 50); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_DOWN); n++;
	arrow2 = XtCreateManagedWidget("arrow2",
					xmArrowButtonWidgetClass,menu_form,args,n);
	XtAddCallback(arrow2,XmNactivateCallback, (XtCallbackProc)change_rowdownCB,
						(XtPointer) row_area);
/*
...Create second row in menu design form. it is for menu cols
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,15); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,25); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,2); n++; 

	but1 = XtCreateManagedWidget("Number of columns:",
				xmLabelWidgetClass,menu_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,15); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,25); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, but1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,55); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;

	col_area = XtCreateManagedWidget("Text2.dsgn",
				xmTextWidgetClass,menu_form,args,n);
	sprintf(num, "%d", UMM_menu.cols);
	XmTextSetString(col_area, num);
	XtAddCallback(col_area,XmNactivateCallback, (XtCallbackProc)save_colsCB,
						(XtPointer) col_area);  

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,15); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,20); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 55); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 60); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_UP); n++;
	arrow1 = XtCreateManagedWidget("arrow1",
					xmArrowButtonWidgetClass,menu_form,args,n);
	
	XtAddCallback(arrow1,XmNactivateCallback, (XtCallbackProc)change_colsupCB,
						(XtPointer) col_area);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,20 ); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,25); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 55); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 60); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_DOWN); n++;
	arrow2 = XtCreateManagedWidget("arrow2",
					xmArrowButtonWidgetClass,menu_form,args,n);
	XtAddCallback(arrow2,XmNactivateCallback, (XtCallbackProc)change_colsdownCB,
						(XtPointer) col_area); 
/*
...Create third row in menu design form. it is for menu title
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,25); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,33); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,2); n++;

	but1 = XtCreateManagedWidget("Title:",
					xmLabelWidgetClass,menu_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,25); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,33); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, but1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,90); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;

	title_area = XtCreateManagedWidget("Text3.dsgn",
			xmTextWidgetClass,menu_form,args,n);
	XmTextSetString(title_area, UMM_menu.name);
	XtAddCallback(title_area,XmNactivateCallback, (XtCallbackProc)save_titleCB,
						(XtPointer) title_area);
/*
...Create forth row in menu design form. it is for menu width
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,35); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,45); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,2); n++; 

	but1 = XtCreateManagedWidget("Width:",
				xmLabelWidgetClass,menu_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,35); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,45); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, but1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,50); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;

	size0 = XtCreateManagedWidget("Text4.dsgn",
					xmTextWidgetClass,menu_form,args,n);
	sprintf(num, "%d", UMM_menu.size[0]);
	XmTextSetString(size0, num);
	XtAddCallback(size0,XmNactivateCallback, (XtCallbackProc)save_widthCB,
						(XtPointer) size0);  
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,35); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,40); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 50); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 55); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_UP); n++;
	arrow1 = XtCreateManagedWidget("arrow1",
					xmArrowButtonWidgetClass,menu_form,args,n);
	
	XtAddCallback(arrow1,XmNactivateCallback, (XtCallbackProc)change_widthupCB,
						(XtPointer) size0);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,40 ); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,45); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 50); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 55); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_DOWN); n++;
	arrow2 = XtCreateManagedWidget("arrow2",
					xmArrowButtonWidgetClass,menu_form,args,n);
	XtAddCallback(arrow2,XmNactivateCallback, (XtCallbackProc)change_widthdownCB,
						(XtPointer) size0); 
/*
...Create fifth row in menu design form. it is for menu height
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,45); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,55); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,2); n++; 

	but1 = XtCreateManagedWidget("Height:",
					xmLabelWidgetClass,menu_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,45); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,55); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, but1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,50); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;

	size1 = XtCreateManagedWidget("Text5.dsgn",
				xmTextWidgetClass,menu_form,args,n);
	sprintf(num, "%d", UMM_menu.size[1]);
	XmTextSetString(size1, num);
	XtAddCallback(size1,XmNactivateCallback, (XtCallbackProc)save_heightCB,
					(XtPointer) size1);  

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,45); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,50); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 50); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 55); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_UP); n++;
	arrow1 = XtCreateManagedWidget("arrow1",
					xmArrowButtonWidgetClass,menu_form,args,n);
	
	XtAddCallback(arrow1,XmNactivateCallback, (XtCallbackProc)change_htupCB,
						(XtPointer) size1);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,50 ); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,55); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 50); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 55); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_DOWN); n++;
	arrow2 = XtCreateManagedWidget("arrow2",
					xmArrowButtonWidgetClass,menu_form,args,n);
	XtAddCallback(arrow2,XmNactivateCallback, (XtCallbackProc)change_htdownCB,
						(XtPointer) size1); 
/*
...Create sixth row in menu design form. it is for menu position in X
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,55); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,65); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,2); n++; 

	but1 = XtCreateManagedWidget("Position_X:",
				xmLabelWidgetClass,menu_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,55); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,65); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, but1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,55); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;

	pos0 = XtCreateManagedWidget("Text6.dsgn",
			xmTextWidgetClass,menu_form,args,n);
	sprintf(num, "%d", UMM_menu.pos[0]);
	XmTextSetString(pos0, num);
	XtAddCallback(pos0,XmNactivateCallback, (XtCallbackProc)save_posxCB,
					(XtPointer) pos0);  
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,55); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,60); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 55); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 60); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_UP); n++;
	arrow1 = XtCreateManagedWidget("arrow1",
					xmArrowButtonWidgetClass,menu_form,args,n);
	
	XtAddCallback(arrow1,XmNactivateCallback, (XtCallbackProc)change_xupCB,
						(XtPointer) pos0);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,60 ); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,65); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 55); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 60); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_DOWN); n++;
	arrow2 = XtCreateManagedWidget("arrow2",
					xmArrowButtonWidgetClass,menu_form,args,n);
	XtAddCallback(arrow2,XmNactivateCallback, (XtCallbackProc)change_xdownCB,
						(XtPointer) pos0); 
/*
...Create seventh row in menu design form. it is for menu position in Y
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,65); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,75); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,2); n++; 

	but1 = XtCreateManagedWidget("Position_Y:",
			xmLabelWidgetClass,menu_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,65); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,75); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, but1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,55); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;

	pos1 = XtCreateManagedWidget("Text7.dsgn",
					xmTextWidgetClass,menu_form,args,n);
	sprintf(num, "%d", UMM_menu.pos[1]);
	XmTextSetString(pos1, num);
	XtAddCallback(pos1,XmNactivateCallback, (XtCallbackProc)save_posyCB,
							(XtPointer) pos1);  

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,65); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,70); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 55); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 60); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_UP); n++;
	arrow1 = XtCreateManagedWidget("arrow1",
					xmArrowButtonWidgetClass,menu_form,args,n);
	
	XtAddCallback(arrow1,XmNactivateCallback, (XtCallbackProc)change_yupCB,
						(XtPointer) pos1);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,70 ); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,75); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 55); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 60); n++;
	XtSetArg(args[n],XmNarrowDirection, XmARROW_DOWN); n++;
	arrow2 = XtCreateManagedWidget("arrow2",
					xmArrowButtonWidgetClass,menu_form,args,n);
	XtAddCallback(arrow2,XmNactivateCallback, (XtCallbackProc)change_ydownCB,
						(XtPointer) pos1); 
/*
...Create eightth row in menu design form. it is for menu type choice
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,76); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,81); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2); n++; 
	but1 = XtCreateManagedWidget("Type:",
			xmLabelWidgetClass,menu_form,args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,81); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,89); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 10); n++; 
	XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
/*
.....change to radio box
*/
	XtSetArg(args[n], XmNradioBehavior, True); n++;
	XtSetArg(args[n], XmNradioAlwaysOne, True); n++;
	row = XtCreateManagedWidget("rows",
					xmRowColumnWidgetClass,menu_form ,args,n);
	
/*
	n = 0;
	opt_menu = XmCreatePulldownMenu(row, "opt_menu", args, n);
	n = 0;
	XtSetArg(args[n], XmNsubMenuId, opt_menu); n++;
	option = XmCreateOptionMenu(row, "option", args, n);
	titled_opt = (Widget)uw_mfCreatePushbutton(opt_menu, "Titled_Menu",
							(XtCallbackProc)save_typeCB, (XtPointer)"Titled_Menu");
	popup_opt = (Widget)uw_mfCreatePushbutton(opt_menu, "Popup_Menu",
							(XtCallbackProc)save_typeCB, (XtPointer)"Popup_Menu");
	icon_opt = (Widget)uw_mfCreatePushbutton(opt_menu, "Icon_Menu",
							(XtCallbackProc)save_typeCB, (XtPointer)"Icon_Menu");
	XtVaSetValues(option, XmNmenuHistory, titled_opt, NULL);
	XtManageChild(option);
*/
	titled_opt = (Widget)XtCreateManagedWidget("Titled_Menu",
						xmToggleButtonWidgetClass, row, NULL,0);
	popup_opt = (Widget)XtCreateManagedWidget("Popup_Menu",
						xmToggleButtonWidgetClass, row, NULL,0);
	icon_opt = (Widget)XtCreateManagedWidget("Icon_Menu",
						xmToggleButtonWidgetClass, row, NULL,0);
	XtAddCallback(titled_opt, XmNvalueChangedCallback, save_typeCB,
									(XtPointer)"Titled_Menu");
	XtAddCallback(popup_opt, XmNvalueChangedCallback, save_typeCB,
									(XtPointer)"Popup_Menu");
	XtAddCallback(icon_opt, XmNvalueChangedCallback, save_typeCB,
									(XtPointer)"Icon_Menu");
	XmToggleButtonSetState(titled_opt, True, False);

/*
...Create nineth row in menu design form. it is for menu file input
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,90); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,98); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,2); n++;

	but1 = XtCreateManagedWidget("Menu File Name:",
					xmLabelWidgetClass,menu_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,90); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,98); n++; 
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, but1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,80); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;

	file_area = XtCreateManagedWidget("Text8.dsgn",
			xmTextWidgetClass,menu_form,args,n);
	XmTextSetString(file_area, UMM_menu.file);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,90 ); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,98); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 80); n++; 
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightPosition, 98); n++; 
	browse = XmCreatePushButton(menu_form, "Browse...", args, n);
	XtAddCallback(browse, XmNactivateCallback, (XtCallbackProc)browseCB,
							 (XtPointer)file_area);
	XtManageChild(browse);
	n = XtNumber(ZactionList);
	action_area = uw_mfcreate_action(pane, ZactionList, n);
	XtManageChild(menu_form);
	XtManageChild(pane);
	XtRealizeWidget(desgn_win);
	XtMapWidget(desgn_win);
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

#endif
#endif
