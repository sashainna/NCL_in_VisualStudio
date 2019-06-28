#include "usysdef.h"
#if UU_COMP != UU_WIN2K

/********************************************************************* 
**  NAME:  wsmfmenu.c
**
**              CONTAINS:
**                      menuFocus (button, client_data, call_data)
**                      menuSelect (button, client_data, call_data)
**                      quitCB(widget, clientData, callData)
**                      unmapCB(widget, clientData, callData)
**                      mapCB(widget, clientData, callData)
**                      uw_mfdown_menu(fname)
**                      uw_mfmenu(kinc,kdis)
**                      uw_mfdisplay_menu(kinc)
**                      disp_status (widget,client_data, event)
**                      get_size_posCB(widget,client_data, event)
**                      uw_mfpopup_menu(wid,devno)
**                      uw_mfpopup_choice(menu,choice,xy,k,devno)
**                      uw_mfmenu_reset(kmenu,kgraph,kicon)
**                      uw_mfdnprompt(ws,num)
**                      uw_mfstr_prompt(ws,prompt,loc,num)
**						uw_mfdown_menunum
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsmfmenu.c , 25.1
**    DATE AND TIME
**       04/29/15 , 15:12:12
**
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:cursorfont.h>
#include <decw$include:keysym.h>
#include <decw$include:Xm.h>
#include <decw$include:PushB.h>
#include <decw$include:PushBG.h>
#include <decw$include:ToggleB.h>
#include <decw$include:RowColumn.h>
#include <decw$include:DialogS.h>
#include <decw$include:PanedW.h>
#include <decw$include:Form.h>
#include <decw$include:MwmUtil.h>
#include <decw$include:Protocols.h>
#include <decw$include:Label.h>
#include <decw$include:LabelG.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/MwmUtil.h>
#include <Xm/Protocols.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/MenuShell.h>
#endif

#include<string.h>
#include "dasnog.h"
#include "dinput.h"
#include "dmark.h"
#include "dmotif.h"
#include "gi1.h"
#include "gtbl.h"
#include "zkeysym.h"
#include "ws.h"
#include "wsxw.h"
#include "wsmf.h"
#include "wsgl.h"
#include "driver.h"
#include "xenv1.h"
#include "lcom.h"
#include "dselect.h"
/*
.....Global variable definitions
*/
extern UWS_MF uw_mf; 
extern int XBorder[2],YBorder[2];
extern int Border_stat;
UWS_MFLAYOUT uw_mflayout;
int UDM_menu_mapped[UDM_MAX_MENU]; 
extern int formActive;
extern Widget formInpWin;
static int PopupChoice[20];
static int NPopup = -1;
static Widget *PopupWindow[20];
/*
.....added by Yurong to see if NCL iconfied
*/
static int App_icon = 0;
extern struct
{
	Giclass curdevclas;     /* currently being used device class */
	Gint curdevno;       /* currently being used device number */
	Giclass curreqclas;     /* currently requested device class, or -1 */
	Gint curreqno;       /* currently requested device number */
} ug_wsdev;

void uw_mf_menuselect();
void uw_mfdisp_status();
void uw_mfdisplay_menu();
void uw_mfmenu_reset();

/**********************************************************************
**    I_FUNCTION : uw_mfChangeColor(widget, color) 
**      Routine to simulate XmChangeColor (using MOTIF 1.1) 
**                      
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfChangeColor(widget, color)
Widget widget;
Pixel color;
{
   Colormap cmap;
   Pixel bg, top_shadow,bottom_shadow,select_color,fg;

   XtVaGetValues(widget, XmNbackground, &bg, XmNcolormap, &cmap, NULL);
   XmGetColors(XtScreen(widget), cmap, bg, &fg, &top_shadow,
						&bottom_shadow, &select_color);
   XtVaSetValues(widget, XmNtopShadowColor, top_shadow,
			XmNbottomShadowColor, bottom_shadow,
			XmNarmColor, select_color,
			XmNborderColor, fg, NULL);
}

/**********************************************************************
**    I_FUNCTION :  menuFocus(button,client_data,call_data)
**       Dummy routine.  Tried to use to highlight button that cursor is
**                      on.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void menuFocus (button, client_data, call_data)
Widget button;
XtPointer client_data,call_data;
{
	XmProcessTraversal(button,XmTRAVERSE_CURRENT);
}

/**********************************************************************
**    I_FUNCTION :  toggleSelect(button,client_data,call_data)
**       Menu itoggle selection callback routine.  Performs user defined 
**                      function        for pushed menu button.
**    PARAMETERS   
**       INPUT  : 
**          button      = Button widget pushed.
**                              client_data = Contains program information about pushed
**                                            button.
**                              call_data   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void toggleSelect (button, client_data, call_data)
Widget button;
XtPointer client_data,call_data;
{
	int type,sub,irtn,ifl, ptype_saved, save_ptype;
	XmString setstring;
	int size[2], pos[2];
	short app;
	char func[80],buf[80], func1[80], name[80];
	char args[35][64];
	UDM_menu_toggle_struc *cbs = (UDM_menu_toggle_struc *)client_data;
	UZ_keytable ktab;
	char *index;
	XClientMessageEvent mevent;
	XKeyPressedEvent kevent;
#define NALPHAKEYS 18
	static unsigned int Keycode[NALPHAKEYS] = {XK_0, XK_1, XK_2, XK_3, XK_4,
		XK_5, XK_6, XK_7, XK_8, XK_9, XK_period, XK_comma, XK_plus, XK_minus,
		XK_slash, XK_asterisk, XK_BackSpace, XK_Return};
/*
.....Destroy Choice Menu
*/
	XtPopdown(XtParent(XtParent(button)));
	setstring = XmStringCreateSimple(cbs->label);
	XtVaSetValues(XtParent(XtParent(XtParent(button))), XmNlabelString,
			setstring, NULL);
	ptype_saved = 0;
/*
.....This menu performs an NCL function
*/
	strcpy(func,cbs->func);
	strcpy(func1,cbs->func);
	strcpy(name,cbs->label);
/*
.....set cursor
*/
	uw_mfsetcursor(21);
	if (uz_which_keydef(func,&type,&sub,&app) == UU_SUCCESS)
	{
		ktab.type = type;
		ktab.sub = sub;
		ktab.flag = app;
		ktab.params = cbs->params;
		index = buf;
		ifl = 1;
		if (formActive) ifl = 0;
/*
...added for record menu
...Yurong
*/
		if((!formActive)&&(type!=DASKEY))
			ud_rpwrmenu(func1, cbs->params, name);

		if (!((strcmp(func, "KEY_TEXT")==0) ||
				(strcmp(func, "KEY_LOCATE")==0) ||
				(strcmp(func, "KEY_PICK")==0) ||
				((type == NCLKEY) && (sub>=112) && (sub<=131))))
		{
			ptype_saved = 1;
			save_ptype = ud_getpick_type();
			ud_setpick_type(UD_PICK_NORMAL);
		}
		irtn = uz_user_keydef(ktab,&index,ifl);
/*
........DAS Function
........Propagate DAS key to graphics window
*/
/*
...only when in pick mode, the event can send to window
...so added for check
...yurong
*/
		if (irtn == 1 && !formActive)
		{
			if (UD_pickmode==1)
			{
				mevent.type = ClientMessage;
				mevent.display = uw_xw.disp;
				mevent.window = uw_xw.wd_id;
				mevent.message_type = NULL;
				mevent.format = 16;
				mevent.data.s[0] = 1111;
				mevent.data.s[1] = 10000 + sub;
				XSendEvent(uw_xw.disp,uw_xw.wd_id,True,0,(XEvent *)&mevent);
			}
			else
/*
...if not send event call daskey func to execute
*/
			{
				ud_rpwrmenu(func1, cbs->params, name);
				uz_daskey1(sub, cbs->params);
			}
		}
/*
........Propagate ALPHA key to graphics window
*/
		else if (irtn == 3 && (!formActive || formInpWin != NULL))
		{
			kevent.type = KeyPress;
			kevent.display = uw_xw.disp;
			kevent.window = uw_xw.wd_id;
			if (formActive) kevent.window = XtWindow(formInpWin);
			kevent.state = 0;
			if (formActive && Keycode[sub] == XK_Return)
				kevent.keycode = XKeysymToKeycode(uw_xw.disp,XK_Tab);
			else
				kevent.keycode = XKeysymToKeycode(uw_xw.disp,Keycode[sub]);
			kevent.state = 0;
			if (Keycode[sub] == XK_plus || Keycode[sub] == XK_asterisk)
				kevent.state = ShiftMask;
			XSendEvent(uw_xw.disp,kevent.window,True,0,(XEvent *)&kevent);
		}
	}
/*
.....Load a new menu
*/
	else if (!formActive)
	{
/*
.....USERDEF
*/
		irtn = NclxLaunchRoutine(func,0,args);
		if (irtn != UU_SUCCESS)
		{
			ud_rpwrmenu(cbs->func, "", cbs->label);
			pos[0] = -1;
			pos[1] = -1;
			size[0] = -1;
			size[1] = -1;
			if (udm_read_menu(func,pos,size, 1, 1, -1) != UU_SUCCESS)
			{
				sprintf(buf,"Could not load menu: %s",func);
				uw_mferror(buf);
			}
		}
	}
/*
.....Sometimes, after this function, it does not
.....go back to begining of for loop in mainloop
.....it still in XtAppNextEvent. So flush here
.....For motif, it flush in setcursor.
.....added by Yurong 9/15/97
*/
	ud_updatews(UG_SUPPRESS);
	if (ptype_saved)
	{
		ud_setpick_type(save_ptype);
	}
}
/**********************************************************************
**    I_FUNCTION :  toggle_menuSelect(button,client_data,call_data)
**       Menu selection callback routine.  Performs user defined function
**                      for pushed menu button.
**    PARAMETERS   
**       INPUT  : 
**          button      = Button widget pushed.
**                              client_data = Contains program information about pushed
**                                            button.
**                              call_data   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void toggle_menuSelect (button, client_data, call_data)
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
**    I_FUNCTION :  menuSelect(button,client_data,call_data)
**       Menu selection callback routine.  Performs user defined function
**                      for pushed menu button.
**    PARAMETERS   
**       INPUT  : 
**          button      = Button widget pushed.
**                              client_data = Contains program information about pushed
**                                            button.
**                              call_data   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void menuSelect (button, client_data, call_data)
Widget button;
XtPointer client_data,call_data;
{
	UDM_menu_struc *cbs = (UDM_menu_struc *)client_data;
	uw_mf_menuselect(button,cbs);
}

/**********************************************************************
**    I_FUNCTION :  uw_mf_menuselect(cbs)
**       Performs user defined function
**                      for pushed menu button.
**    PARAMETERS   
**       INPUT  : 
**                              cbs = menu structure 
**                                            
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mf_menuselect(button,cbs)
Widget button;
UDM_menu_struc *cbs;
{
	int type,sub,irtn,ifl, ptype_saved, save_ptype;
	short app;
	char num[80];
	char func[80],buf[256], func1[80], name[80];
	char args[35][64];
	UZ_keytable ktab;
	char *index;
	XClientMessageEvent mevent;
	XKeyPressedEvent kevent;
#define NALPHAKEYS 18
	static unsigned int Keycode[NALPHAKEYS] = {XK_0, XK_1, XK_2, XK_3, XK_4,
		XK_5, XK_6, XK_7, XK_8, XK_9, XK_period, XK_comma, XK_plus, XK_minus,
		XK_slash, XK_asterisk, XK_BackSpace, XK_Return};

	index = buf;
	ptype_saved = 0;
/*
.....Destroy PopUp Menu
*/
	if (UDM_menu[cbs->kinc].type == UDM_MTYPE_POPUP ||
		UDM_menu[cbs->kinc].type == UDM_MTYPE_INTERNAL )
	{
		XtUnrealizeWidget(uw_mflayout.menu_app[cbs->kinc]);
		uw_mflayout.menu_app[cbs->kinc] = NULL;
	}
/*
.....NCL Popup Menu
.....Return choice number
*/

if (UDM_menu[cbs->kinc].type == UDM_MTYPE_INTERNAL)
	{
		type = 0;
		ul_to_number(cbs->file,&type);
		PopupChoice[NPopup] = type;
		sprintf(num, "Choice %d %s", type, cbs->name);
		ud_rpwrcom(num);
		return;
	}
/*
.....This menu performs an NCL function
*/
	strcpy(func,cbs->file);
	strcpy(func1,cbs->file);
	strcpy(name,cbs->name);
/*
.....set cursor
*/
	uw_mfsetcursor(21);
	if (uz_which_keydef(func,&type,&sub,&app) == UU_SUCCESS)
	{
		ktab.type = type;
		ktab.sub = sub;
		ktab.flag = app;
		ktab.params = cbs->params;
		index = buf;
		ifl = 1;
		if (formActive) ifl = 0;
/*
...added for record menu
...Yurong
*/
		if((!formActive)&&(type!=DASKEY))
/*
.....added function paramters
.....Yurong 11/29/00
*/
		ud_rpwrmenu(func1, cbs->params, name);
		if (!((strcmp(func, "KEY_TEXT")==0) ||
				(strcmp(func, "KEY_LOCATE")==0) ||
				(strcmp(func, "KEY_PICK")==0) ||
				((type == NCLKEY) && (sub>=112) && (sub<=131))))
		{
			ptype_saved = 1;
			save_ptype = ud_getpick_type();
			ud_setpick_type(UD_PICK_NORMAL);
		}
		irtn = uz_user_keydef(ktab,&index,ifl);
/*
........DAS Function
........Propagate DAS key to graphics window
*/
/*
...only when in pick mode, the event can send to window
...so added for check
...yurong
*/
		if (irtn == 1 && !formActive)
		{
			if (UD_pickmode==1)
			{
				mevent.type = ClientMessage;
				mevent.display = uw_xw.disp;
				mevent.window = uw_xw.wd_id;
				mevent.message_type = NULL;
				mevent.format = 16;
				mevent.data.s[0] = 1111;
				mevent.data.s[1] = 10000 + sub;
				XSendEvent(uw_xw.disp,uw_xw.wd_id,True,0,(XEvent *)&mevent);
			}
			else
/*
...if not send event call daskey func to execute
*/
			{
				ud_rpwrmenu(func1, cbs->params, name);
				uz_daskey1(sub, cbs->params);
			}
		}
/*
........Propagate ALPHA key to graphics window
*/
		else if (irtn == 3 && (!formActive || formInpWin != NULL))
		{
			kevent.type = KeyPress;
			kevent.display = uw_xw.disp;
			kevent.window = uw_xw.wd_id;
			if (formActive) kevent.window = XtWindow(formInpWin);
			kevent.state = 0;
			if (formActive && Keycode[sub] == XK_Return)
				kevent.keycode = XKeysymToKeycode(uw_xw.disp,XK_Tab);
			else
				kevent.keycode = XKeysymToKeycode(uw_xw.disp,Keycode[sub]);
			kevent.state = 0;
			if (Keycode[sub] == XK_plus || Keycode[sub] == XK_asterisk)
				kevent.state = ShiftMask;
			XSendEvent(uw_xw.disp,kevent.window,True,0,(XEvent *)&kevent);
		}
		else if (irtn == 5)
		{
			if (index[0]!='\0')
				uw_mfupd_cinput(index);
		}
	}
/*
.....Load a new menu
*/
	else if (!formActive)
	{
/*
.....USERDEF
*/
		irtn = NclxLaunchRoutine(func,0,args);
		if (irtn != UU_SUCCESS)
		{
			ud_rpwrmenu(cbs->file, "", cbs->name);
			if (udm_read_menu(func,cbs->pos,cbs->size, 1,1, -1) != UU_SUCCESS)
			{
				sprintf(buf,"Could not load menu: %s",func);
				uw_mferror(buf);
			}
		}
	}
/*
.....Sometimes, after this function, it does not
.....go back to begining of for loop in mainloop
.....it still in XtAppNextEvent. So flush here
.....For motif, it flush in setcursor.
.....added by Yurong 9/15/97
*/
	ud_updatews(UG_SUPPRESS);
	if (ptype_saved)
	{
		ud_setpick_type(save_ptype);
	}
}


/**********************************************************************
**    I_FUNCTION :  quitCB(widget,clientData,callData)
**       Saves the position of a menu prior to being taken down so that
**                      it will come up in the same position.
**    PARAMETERS   
**       INPUT  : 
**          widget     = Ignored.
**                              clientData = Menu number being taken down.
**                              callData   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void quitCB(widget, clientData, callData)
Widget widget;
XtPointer clientData,callData;
{
	Position x,y,w,h;
	int kinc=(int)clientData;
/*
.....Save the menu position
*/
	XtVaGetValues(uw_mflayout.menu_app[kinc],
		XmNx,&x,
		XmNy,&y,
		XmNwidth,&w,
		XmNheight,&h,
		NULL);
/*
......icon and popup menu has same YBorder=XBorder
*/      
	if (UDM_menu[kinc].type==UDM_MTYPE_MENU)
	{
		UDM_menu[kinc].pos[0] = x - XBorder[1];
		UDM_menu[kinc].pos[1] = y - YBorder[1];
		UDM_menu[kinc].size[0] = w + 2*XBorder[1];
		UDM_menu[kinc].size[1] = h + XBorder[1] + YBorder[1];
	}
	else
	{
		UDM_menu[kinc].pos[0] = x - XBorder[0];
		UDM_menu[kinc].pos[1] = y - XBorder[0];
		UDM_menu[kinc].size[0] = w + 2*XBorder[0];
		UDM_menu[kinc].size[1] = h + 2*YBorder[0];
	}
/*
.....Let the application know this menu is gone
*/
	uw_mflayout.menu_app[kinc] = NULL;
}

/**********************************************************************
**    I_FUNCTION :  unmapCB(widget,clientData,callData)
**       Iconifys all active menus when the top level menu is iconified.
**    PARAMETERS   
**       INPUT  : 
**          widget     = Ignored.
**                              clientData = Menu number being taken down.
**                              callData   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void unmapCB(widget, clientData, event)
Widget widget;
XtPointer clientData;
XEvent *event;
{
	int i;
	XUnmapEvent *uev=(XUnmapEvent *)event;
/*
...remember unmapped menu
*/
	i = (int)clientData;
	if (uev->type == UnmapNotify)
		UDM_menu_mapped[i] = 0;
/*
........Iconify all active menus
*/
/*      if (uev->type == UnmapNotify)
		uw_mfmenu_reset(UU_FALSE,UU_TRUE,UU_TRUE);*/
}

/**********************************************************************
**    I_FUNCTION :  mapCB(widget,clientData,callData)
**       Restores all active menus when the top level menu is restored.
**    PARAMETERS   
**       INPUT  : 
**          widget     = Ignored.
**                              clientData = Menu number being taken down.
**                              callData   = Motif callback structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void mapCB(widget, clientData, event)
Widget widget;
XtPointer clientData;
XEvent *event;
{
	int i;
	XMapEvent *uev=(XMapEvent *)event;
/*
...remember mapped menu
*/
	i = (int)clientData;
	if (uev->type == MapNotify)
	{
		UDM_menu_mapped[i] = 1;
	}
/*
........Restore all active menus
...... if main menu ( menu number = 1) maped
...... yurong
*/
/*
.....but not when menu design phase
.....changed 12/12/97 Yurong
*/
/*      if ((uev->type == MapNotify)&&(i==0)) */
/*
.....we didn't Restore all active menus when menu design phase
.....because we don't want reset all menus after we finished main menu 
.....design. But we didn't consider "MIN_ALL" or "MIN_MENU", we do need
.....reset all menus when map main menu even in menu design phase
.....change again by Yurong 8/24/98
*/
	if ((uev->type == MapNotify)&&(i==0))
	{
/*              if (uw_mfismd()==0)                */
		if (App_icon==1) 
			uw_mfmenu_reset(UU_TRUE,UU_TRUE,UU_FALSE); 
/*
.....if in menu design phase, raise menu design windows
.....Yurong 8/24/98
*/
		if (uw_mfismd()!=0)
			uw_mfraise_menud();
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_mfdown_menu(fname)
**       Takes down a menu based on its file name.  This routine is
**                      usually called for the fixed name menus, such as 'SELECT.menu'.
**    PARAMETERS   
**       INPUT  : 
**          fname   = File name of menu to take down.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfdown_menu(fname)
char *fname;
{
	int i,args[3];
	char buf[256];
/*
.....Search for the requested menu
*/
	for (i=0;i<UDM_menu_count;i++)
	{
/*
........Allow for directory path on menu file name
........Bobby  -  1/7/98
*/
		args[0] = 1; args[1] = 0;
		ul_get_base_fname(UDM_menu[i].file,buf,args,UX_NCHK|UX_NQUOTES|UX_NPRTERRS);
/*
........Found the menu
........If it's up, let's take it down
*/
		if ((strcmp(UDM_menu[i].file,fname) == 0 || strcmp(buf,fname) == 0) &&
			uw_mflayout.menu_app[i] != NULL)
		{
			XtUnrealizeWidget(uw_mflayout.menu_app[i]);
			uw_mflayout.menu_app[i] = NULL;
			break;
		}
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_mfdown_menunum(menu_num, drag_num)
**       Takes down a menu based on menu index number.
**			if it is a Pupup menu, destroy it
**    PARAMETERS
**       INPUT  :
**          menu_num   = menu index number.
**       OUTPUT : not for UNIX. always return -1
**          drag_num: if the menu is popup menu and have the drag menu
**                it  return the drag menu number
**    RETURNS      : 1: the menu is displayed and be taken down
**							0: the menu is not display at all and doing nothing
here
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_mfdown_menunum(menu_num, drag_num)
int menu_num, *drag_num;
{
	*drag_num = -1;
	if (uw_mflayout.menu_app[menu_num] != NULL)
	{
		XtUnrealizeWidget(uw_mflayout.menu_app[menu_num]);
		uw_mflayout.menu_app[menu_num] = NULL;
		return 1;
	}
	return 0;
}
/**********************************************************************
**    I_FUNCTION :  disp_status(widget,client_data, event)
**      Display the label and function that select menu comtained 
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**                              client_data = Contains program information about pushed
**                                            button.
**                              widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void disp_status (widget,
	client_data,
	event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	UDM_menu_struc *cbs ;
	XButtonPressedEvent *bevent = (XButtonPressedEvent *) event;
	if(bevent->button != 3)
		return;
	cbs = (UDM_menu_struc *)client_data;
	uw_mfdisp_status(cbs->file, cbs->descrip, bevent);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfdisp_status(name, file, event)
**      Display the label and function that select menu comtained 
**    PARAMETERS   
**       INPUT  : 
**                              name :        Menu item label
**                              file:         Menu function
**          bevent:      Xevent.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfdisp_status(file, descrip, bevent)
char *file, *descrip;
XButtonPressedEvent  *bevent;
{

	Widget popup;
	XmString third_str;
	int i, n;
	Arg args[20];
	char second[40], third[200], name[100];


	sprintf(second, "Function:    %s", file); 
	third[0] = '\0';
	if (descrip[0]!='\0')
		strcpy(third, descrip);
	else
	{
/*
......changes made because we have removed wsmfpfunc.h
*/
		for(i=0; ; i++)
		{
			if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
			if (strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) continue;
/*
...change to upper case to compare
*/
			strcpy(name, UZ_keyfuncs[i].name);
			ul_to_upper(name);
			if (strcmp(name, file)==0)
			{
				sprintf(third, "%s", UZ_keyfuncs[i].descrip);
				break;
			}
		}
		if (third[0] == '\0')
			strcpy(third, file);
	}
	n = 0;
	popup = XmCreatePopupMenu(uw_mf.parent, "status_info", args, n);
	third_str = XmStringCreateSimple(third);
	(void)XtVaCreateManagedWidget("status_info", xmLabelGadgetClass, popup, 
					XmNalignment, XmALIGNMENT_BEGINNING,
					XmNlabelString, third_str, NULL);
	XmMenuPosition(popup, bevent);
	XtManageChild(popup);
	XmStringFree(third_str);
}

/**********************************************************************
**    I_FUNCTION :  get_size_posCB(widget,client_data, event)
**      Get size and position when menu move or sized and save in the 
**      layout structure 
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**                              client_data = Contains program information about pushed
**                                            button.
**                              widget:    widget that event happened.
*       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void get_size_posCB (widget,
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
/*
.....For SGI, when reset menu, it does not have event type
.....ConfigureNotify but MapNotify
.....so added it
.....12/16/97 Yurong
*/
/* if(cevent->type !=ConfigureNotify) */
   if ((cevent->type !=ConfigureNotify)&&(cevent->type !=MapNotify))
		return;
	number = (int)client_data;
/*
...get size and position
*/
	XtVaGetValues(widget, XmNx, &x, XmNy, &y, NULL);
	XtVaGetValues(widget, XmNwidth, &width, XmNheight, &height, NULL); 
/*
...Save in the layout structure
*/
	if (UDM_menu[number].type==UDM_MTYPE_MENU)
	{
		UDM_layout.menu_pos[number][0] = x - XBorder[1];
		UDM_layout.menu_pos[number][1] = y - YBorder[1];
		UDM_layout.menu_size[number][0] = width + 2*XBorder[1];
		UDM_layout.menu_size[number][1] = height + XBorder[1] + YBorder[1];
	}
	else
	{
		UDM_layout.menu_pos[number][0] = x - XBorder[0];
		UDM_layout.menu_pos[number][1] = y - YBorder[0];
		UDM_layout.menu_size[number][0] = width + 2*XBorder[0];
		UDM_layout.menu_size[number][1] = height + 2*YBorder[0];
	}
	UDM_menu[number].pos[0] = UDM_layout.menu_pos[number][0];
	UDM_menu[number].pos[1] = UDM_layout.menu_pos[number][1];
	UDM_menu[number].size[0] = UDM_layout.menu_size[number][0];
	UDM_menu[number].size[1] = UDM_layout.menu_size[number][1];
}

/**********************************************************************
**    I_FUNCTION :  uw_mfenter_handler(widget,client_data, event)
**      Event handler for pointer enter the menu
**      
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**                              client_data = Contains program information about 
**                                            which type of menu we are get from
**                                                              1: Normal Menu
**                                                              2: Icon Menu
**                                                              3: Popup Menu
**                                                              4: Internal Menu
**                                                              5: Design Menu
**                              widget:    widget that event happened.
*       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfenter_handler (widget,
	client_data,
	event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	Pixel color;
	UDM_menu_struc *cbs = (UDM_menu_struc *)client_data;
	if(event->type != EnterNotify) 
		return;

	if ((cbs->color[0]==-1)||(cbs->color[1]==-1)||(cbs->color[2]==-1))
	{
		XtVaGetValues(widget, XmNbackground, &color, NULL);
		cbs->color[0] = color;
		XtVaGetValues(widget, XmNarmColor, &color, NULL);
		cbs->color[2] = color;
		XtVaGetValues(widget, XmNforeground, &color, NULL);
		cbs->color[1] = color;
	}
	color = cbs->color[2];
	XtVaSetValues(widget, XmNbackground, color, NULL);
/*
.....removed because only MOTIF 1.2 support it,
.....we call uw_mfChangeColor(widget, color)
.....It use MOTIF 1.1 
.....Yurong 5/6/99
*/
/*
	XmChangeColor(widget, color);
*/
	uw_mfChangeColor(widget, color);
	return;
}
	
/**********************************************************************
**    I_FUNCTION :  uw_mfleave_handler(widget,client_data, event)
**      Event handler for pointer enter the menu
**      
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**                              client_data = Contains program information about 
**                                            which type of menu we are get from
**                              widget:    widget that event happened.
*       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfleave_handler (widget,
	client_data,
	event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	UDM_menu_struc *cbs = (UDM_menu_struc *)client_data;
	if(event->type != LeaveNotify) 
		return;
	if (cbs->color[0]!=-1)
	{
		XtVaSetValues(widget, XmNbackground, cbs->color[0], NULL);
/*
.....removed because only MOTIF 1.2 support it,
.....we call uw_mfChangeColor(widget, color)
.....It use MOTIF 1.1 
.....Yurong 5/6/99
*/
/*
		XmChangeColor(widget, cbs->color[0]);
*/
		uw_mfChangeColor(widget, cbs->color[0]);
/*
.....XmChangeColor will change foreground color as Motif default select
.....we need reset foreground color
*/
		XtVaSetValues(widget, XmNforeground, cbs->color[1], NULL);
		return;
	}
}


/**********************************************************************
**    I_FUNCTION :  choice_evnthandler(widget,client_data, event)
**      Display the label and function that select menu comtained 
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**                              client_data = Contains program information about pushed
**                                            button.
**                              widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void choice_evnthandler (widget,
	client_data,
	event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	XEvent revent;
	UDM_menu_toggle_struc *cbs = (UDM_menu_toggle_struc *)client_data;
	XButtonPressedEvent *bevent = (XButtonPressedEvent *) event;
	if(bevent->button != 3)
		return;
	if(bevent->button == 3)
	{
		uw_mfdisp_status(cbs->func, cbs->descrip, bevent);;
		while(1)
		{
			XtAppNextEvent(uw_mf.application,&revent);
			if (revent.type == ButtonRelease)
			{
				XtDispatchEvent(&revent);
				break;
			}
			else
				XtDispatchEvent(&revent);
		}
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_menu(kinc,kdis)
**       Activates a menu.
**    PARAMETERS   
**       INPUT  : 
*n*          kinc    = Menu number to activate.
**                              kdis    = 0 = Define the menu only, but do not manage it.
**                                        1 = Manage (display) the menu.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfmenu(kinc,kdis)
int kinc,kdis;
{
	int ifl,n,i,j,k,px,py, m;
	unsigned int state;
	Arg args[20];
	void exitCB();
	int flag;
	Widget winid,but,toggle_menu, opt_menu;
	Window root, child;
	char geom[20],buf[120],nam[30];
	char geom1[20];
	char bgcolor[40];
	Atom watom;
	Widget choice[UDM_MAX_TOGGLE];
	Pixel ibgcolor;

	if (kdis)
	{
/*		if (UDM_menu[kinc].key_loaded==0)*/
		{
			UDM_menu[kinc].key_loaded = 1;
			if (UDM_menu[kinc].keyfile[0]!='\0')
			{
				uz_load_keys2(UDM_menu[kinc].keyfile, 0);
			}
		}
	}

	if (UDM_menu[kinc].type == UDM_MTYPE_MENU)
		flag = 1;
	else if (UDM_menu[kinc].type == UDM_MTYPE_ICON)
		flag = 2;
	else if (UDM_menu[kinc].type == UDM_MTYPE_POPUP)
		flag = 3;
	else if (UDM_menu[kinc].type == UDM_MTYPE_INTERNAL)
		flag = 4;
	else
		flag = 1;
/*
.....Menu is already up
.....Just display it
*/
	if (uw_mflayout.menu_app[kinc] != NULL)
	{
		if (kdis == 1)
		{
			XtMapWidget(uw_mflayout.menu_app[kinc]);
			XRaiseWindow(XtDisplay(uw_mflayout.menu_app[kinc]),
				XtWindow(uw_mflayout.menu_app[kinc]));
/*
.....added user changable auto-movement of cursor flag
.....Yurong 4/3/00
*/
			if (UW_auto_cursor)
			{
				XWarpPointer(uw_xw.disp,None,XtWindow(uw_mflayout.menu_app[kinc]),
					0,0,0,0,XBorder[1]+5,YBorder[1]+5);
			}
		}
		goto done;
	}
/*
.....Open the Menu shell
*/
	n = 0;
	XtSetArg(args[n],XmNkeyboardFocusPolicy,  XmPOINTER); n++;
/*
........Icon and Pop-up menus
........Do not contain Window Title & Resize borders
*/
	if (UDM_menu[kinc].type != UDM_MTYPE_MENU)
	{
		ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
				MWM_DECOR_MINIMIZE | MWM_DECOR_TITLE | MWM_DECOR_RESIZEH;
		XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
	}
	if (UDM_menu[kinc].type != UDM_MTYPE_MENU)
	{
		UDM_menu[kinc].size[0] = UDM_menu[kinc].size[0] -  2*XBorder[0];
		UDM_menu[kinc].size[1] = UDM_menu[kinc].size[1] - 2*YBorder[0];
	}
	else
	{
		UDM_menu[kinc].size[0] = UDM_menu[kinc].size[0] - 2*XBorder[1];
		UDM_menu[kinc].size[1] = UDM_menu[kinc].size[1] - XBorder[1] - YBorder[1];
	}

/*
.....added for adjust border
.....make seting position of window to the top of window
.....but not inside border for SGI, because it alway set(get)
.....position of the window to exclude the border.
.....For other platform, such as SUN, HP, it set position to
.....the top of the window(so we don't need adjust here), but
.....get position exclude the border, so, when we save menufile
.....or layout file, we need adject both
.....12/12/97 Yurong
*/
	if (Border_stat==0)
	{
		if (UDM_menu[kinc].type != UDM_MTYPE_MENU)
		{
			UDM_menu[kinc].pos[0] += XBorder[0];
			UDM_menu[kinc].pos[1] += YBorder[0];
		}
		else
		{
			UDM_menu[kinc].pos[0] += XBorder[1];
			UDM_menu[kinc].pos[1] += YBorder[1];
		}
	}
/*
.....Pop-up menus appear at the cursor location
*/
	if (UDM_menu[kinc].type == UDM_MTYPE_POPUP ||
		UDM_menu[kinc].type == UDM_MTYPE_INTERNAL)
	{
		XQueryPointer(uw_xw.disp,uw_xw.wd_id,&root,&child,
			&UDM_menu[kinc].pos[0],&UDM_menu[kinc].pos[1],
			&px,&py,&state);
	}
/*
.....Name menu
*/
	if (kinc == 0)
	{
		strcpy(nam,"NCL");
	}
	else
	{
		if (UDM_menu[kinc].type == UDM_MTYPE_ICON) strcpy(nam,"IconMenu");
		else if (UDM_menu[kinc].type == UDM_MTYPE_POPUP) strcpy(nam,"PopupMenu");
		else if (UDM_menu[kinc].type == UDM_MTYPE_INTERNAL) strcpy(nam,"InternalMenu");
		else strcpy(nam,"Menu");
	}  
/*
.....Position & Size menu
*/
	if (UDM_menu[kinc].size[0] < 0)
		sprintf(geom,"+%d+%d",UDM_menu[kinc].pos[0],UDM_menu[kinc].pos[1]);
	else
		sprintf(geom,"%dx%d+%d+%d",UDM_menu[kinc].size[0],UDM_menu[kinc].size[1],
			UDM_menu[kinc].pos[0],UDM_menu[kinc].pos[1]);
	XtSetArg(args[n],XmNgeometry,geom); n++; 
/*
.....Create Window for Menu
*/
	sprintf(buf,"UDM_menu_%d",kinc);
	XtSetArg(args[n],XmNmappedWhenManaged,False); n++;
	XtSetArg(args[n],XmNhighlightOnEnter, True); n++;
	XtSetArg(args[n],XmNhighlightPixmap, XmUNSPECIFIED_PIXMAP); n++;
	uw_mflayout.menu_app[kinc] = XtAppCreateShell(nam,buf,
		topLevelShellWidgetClass,uw_xw.disp,
		args,n);
	if (uw_mflayout.menu_app[kinc] == NULL) goto failed;
/*
.....Trap the CLOSE button
*/
	watom = XmInternAtom(uw_xw.disp,"WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(uw_mflayout.menu_app[kinc],watom,quitCB,
			(XtPointer)kinc);
/*
...Add callback to all menus to remember if it is mapped
...Yurong
*/
	XtAddEventHandler(uw_mflayout.menu_app[kinc],StructureNotifyMask,False,
		(XtEventHandler)unmapCB,(XtPointer)kinc);
	XtAddEventHandler(uw_mflayout.menu_app[kinc],StructureNotifyMask,False,
		(XtEventHandler)mapCB,(XtPointer)kinc);
/*
.....Set the window title
*/
	XtVaSetValues(uw_mflayout.menu_app[kinc],XmNtitle,UDM_menu[kinc].name,NULL);
/*
.....Create a Form widget
......to hold menu entries
*/
	n = 0;
	XtSetArg(args[n],XmNfractionBase,UDM_menu[kinc].cols*UDM_menu[kinc].rows);
	n++;
	XtSetArg(args[n],XmNhighlightOnEnter, True); n++;
	XtSetArg(args[n],XmNhighlightPixmap, XmUNSPECIFIED_PIXMAP); n++;
	winid = XtCreateWidget(UDM_menu[kinc].name,
		xmFormWidgetClass,uw_mflayout.menu_app[kinc],args,n);
	if (winid == NULL) goto failed;
/*
.....Put up menu entries
*/
	k = 0;
	for (i=0;i<UDM_menu[kinc].rows;i++)
	{
/*
........Add columns to each row
*/
		for (j=0;j<UDM_menu[kinc].cols;j++)
		{
			n = 0;
			XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNtopPosition,i*UDM_menu[kinc].cols); n++;
			XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNbottomPosition,(i+1)*UDM_menu[kinc].cols); n++;
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNleftPosition,j*UDM_menu[kinc].rows); n++;
			XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNrightPosition,(j+1)*UDM_menu[kinc].rows); n++;
			XtSetArg(args[n],XmNhighlightOnEnter, True); n++;
			XtSetArg(args[n],XmNhighlightPixmap, XmUNSPECIFIED_PIXMAP); n++;
			if (i == 0 && j == 0 && UDM_menu[kinc].type == UDM_MTYPE_INTERNAL)
			{
				but = XtCreateManagedWidget(UDM_menu[kinc].menus[k].name,
					xmLabelWidgetClass,winid,args,n);
			}
			else
			{
/*
.....added choice button
*/
				if ((UDM_menu[kinc].menus[k].toggle!=NULL)&&
						(UDM_menu[kinc].menus[k].toggle_num!=0))
				{
/*
.....first create default button
*/
					but = XtCreateManagedWidget(
							UDM_menu[kinc].menus[k].toggle_def,
							xmPushButtonWidgetClass,winid,args,n);
					XtAddEventHandler(but, ButtonPressMask, False,
											(XtEventHandler)disp_status,
											&(UDM_menu[kinc].menus[k]));
					XtAddEventHandler(but, EnterWindowMask, False,
											(XtEventHandler)uw_mfenter_handler,
											&(UDM_menu[kinc].menus[k]));
					XtAddEventHandler(but, LeaveWindowMask, False,
											(XtEventHandler)uw_mfleave_handler,
											&(UDM_menu[kinc].menus[k]));
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
					strcpy(nam,"SubMenu");
					sprintf(geom1,"100x100+10+10");
					XtSetArg(args[n],XmNgeometry,geom1); n++;
					sprintf(buf,"choice_%d",k);
					toggle_menu = (Widget)XtCreatePopupShell(nam,
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
					opt_menu = (Widget)XtCreateWidget("choice",
								xmRowColumnWidgetClass, toggle_menu, args,n);
					if (opt_menu == NULL) goto failed;
/*
........Copy the first toggle definition
........into the main body, for status display
*/
					strcpy(UDM_menu[kinc].menus[k].name,
							UDM_menu[kinc].menus[k].toggle[0].label);
					strcpy(UDM_menu[kinc].menus[k].file,
							UDM_menu[kinc].menus[k].toggle[0].func);
/*
...........Create the options
*/
					for (m=0; m<UDM_menu[kinc].menus[k].toggle_num; m++)
					{
						choice[m] = XtVaCreateManagedWidget(
							UDM_menu[kinc].menus[k].toggle[m].label,
							xmPushButtonWidgetClass, opt_menu, 
							XmNborderWidth, 0,
							XmNshadowType, XmSHADOW_IN,
							XmNhighlightPixmap, XmUNSPECIFIED_PIXMAP,
							XmNhighlightOnEnter, True,
							NULL);
						XtAddCallback(choice[m],XmNactivateCallback,toggleSelect,
							&(UDM_menu[kinc].menus[k].toggle[m]));
						XtAddEventHandler(choice[m], ButtonPressMask, False,
											(XtEventHandler)choice_evnthandler,
											&(UDM_menu[kinc].menus[k].toggle[m]));
						XtAddEventHandler(choice[m], EnterWindowMask, False,
											(XtEventHandler)uw_mfenter_handler,
											&(UDM_menu[kinc].menus[k]));
						XtAddEventHandler(choice[m], LeaveWindowMask, False,
											(XtEventHandler)uw_mfleave_handler,
											&(UDM_menu[kinc].menus[k]));
						if (UDM_menu[kinc].menus[k].bgcolor[0]!='\0')
						{
							strcpy(bgcolor, UDM_menu[kinc].menus[k].bgcolor);
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
					XtAddCallback(but, XmNactivateCallback,toggle_menuSelect,
						toggle_menu);
					if (UDM_menu[kinc].menus[k].bgcolor[0]!='\0')
					{
						strcpy(bgcolor, UDM_menu[kinc].menus[k].bgcolor);
						XtVaSetValues(but, XtVaTypedArg, XmNbackground, XmRString, 
											bgcolor, strlen(bgcolor)+1, NULL); 
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
					but = XtCreateManagedWidget(UDM_menu[kinc].menus[k].name,
							xmPushButtonWidgetClass,winid,args,n);
					XtAddCallback(but,XmNactivateCallback,menuSelect,
							&(UDM_menu[kinc].menus[k]));
					XtAddEventHandler(but, ButtonPressMask, False,
											(XtEventHandler)disp_status,
											&(UDM_menu[kinc].menus[k]));
					XtAddEventHandler(but, EnterWindowMask, False,
											(XtEventHandler)uw_mfenter_handler,
											&(UDM_menu[kinc].menus[k]));
					XtAddEventHandler(but, LeaveWindowMask, False,
											(XtEventHandler)uw_mfleave_handler,
											&(UDM_menu[kinc].menus[k]));
					if (UDM_menu[kinc].menus[k].bgcolor[0]!='\0')
					{
						strcpy(bgcolor, UDM_menu[kinc].menus[k].bgcolor);
						XtVaSetValues(but, XtVaTypedArg, XmNbackground, XmRString, 
											bgcolor, strlen(bgcolor)+1, NULL); 
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
			k++;
		}
	}
/*
...add event handler for save the size and position change
...Yurong
*/
	XtAddEventHandler(uw_mflayout.menu_app[kinc],StructureNotifyMask,False,
								(XtEventHandler)get_size_posCB, (XtPointer)kinc); 
/*
.....Manage menu
*/
	XtManageChild(winid);
	XtRealizeWidget(uw_mflayout.menu_app[kinc]);
	if (kdis == 1)
	{
		uw_mfdisplay_menu(kinc);
/*
.....added user changable auto-movement of cursor flag
.....Yurong 4/3/00
*/
		if (UW_auto_cursor)
		{
			XWarpPointer(uw_xw.disp,None,XtWindow(uw_mflayout.menu_app[kinc]),
				0,0,0,0,XBorder[1]+5,YBorder[1]+5);
		}
	}
	goto done;
/*
.....Error trying to create menu
*/
failed:;
	sprintf(buf,"Could not create Menu: %s\n",UDM_menu[kinc].name);
	ud_wrerr(buf);
/*
.....End of routine
*/
done:;
	return;
}

/**********************************************************************
**    I_FUNCTION :  uw_displaymenu(kinc)
**       Manages (displays) a menu.
**    PARAMETERS   
**       INPUT  : 
**          kinc    = Menu number to manage.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfdisplay_menu(kinc)
int kinc;
{
	XtMapWidget(uw_mflayout.menu_app[kinc]);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfpopup_menu(wid,devno)
**       Defines and activates a DAS internal POPUP menu.
**    PARAMETERS   
**       INPUT  : 
**          wid     = DAS window id.
**          devno   = DAS device number.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
...Use this function replace
...uw_xwmenu. We need have same parms
...in order to not have problem
...with parms
...Yurong
*/
void uw_mfpopup_menu(wid,devno, prompt)
Gws wid;
int devno;
Gchar *prompt;
{
	int i,n,pos[2],size[2],kinc,stat;
	char **p;
/*
.....change len from 20 to 200
.....Yurong 9/15/97
*/
	char msg[200];
	Gchoicest *choicept;
/*
.....Get the parameters for this menu
*/
	choicept = &((*ug_gksstli.wsopen[wid].inptr).choicedata[devno-1]);
	n = choicept->record.number;
	p = choicept->record.strings;
	
/*
.....Initialize menu structure
*/
	pos[0] = -1; pos[1] = -1;
	size[0] = -1; size[1] = -1;
	stat = udm_init_menu_desc(*p,pos, size,&kinc);
	if (kinc >= UDM_menu_count)
	{
		UDM_menu[kinc].rows = n + 1;
		UDM_menu[kinc].cols = 1;
		UDM_menu[kinc].type = UDM_MTYPE_INTERNAL;
		UDM_menu[kinc].num = UDM_menu[kinc].rows;
/*
.....Initialize menu entries
*/
		stat = udm_init_menu();
		for (i=0; i<=n; i++,p++)
		{
			strcpy(UDM_menu[kinc].menus[i].name,*p);
			sprintf(UDM_menu[kinc].menus[i].file,"%d",i);
			UDM_menu[kinc].menus[i].kinc = kinc;
/*
.....use default color
.....Yurong 5/10/99
*/
			UDM_menu[kinc].menus[i].bgcolor[0] = '\0';
			UDM_menu[kinc].menus[i].color[0] = -1;
			UDM_menu[kinc].menus[i].color[1] = -1;
			UDM_menu[kinc].menus[i].color[2] = -1;
		}
		UDM_menu_count++;
	}
/*
...record comment for get in Popup menu
...Yurong
*/
	sprintf(msg,"Popup Menu %s", UDM_menu[kinc].menus[0].name);
	ud_rpwrcom(msg);
/*
.....Take down any current popup menus
*/
	if (NPopup >= 0)
		XtUnmapWidget(*(PopupWindow[NPopup]));
/*
.....Display menu
*/
	uw_mfmenu(kinc,1);
	NPopup++;
	PopupWindow[NPopup] = &(uw_mflayout.menu_app[kinc]);
	PopupChoice[NPopup] = -1;
	return;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfpopup_choice(menu,choice,xy,k,devno)
**       Loops while awaiting the user to select a POPUP menu choice.
**    PARAMETERS   
**       INPUT  : 
**          devno   = DAS menu (device) number.
**       OUTPUT :  
**          menu     = Returns the menu (device) number input from
**                                         'devno'.
**                              choice   = Menu item selected.
**                              xy       = Cursor position (always returns 0,0).
**                              k        = Mouse button used to select menu item.
**    RETURNS      : none
**    SIDE EFFECTS : Remains in event loop until user makes a selection.
**    WARNINGS     : none
*********************************************************************/
int uw_mfpopup_choice(menu,choice,xy,k,devno)
int *menu,*choice,xy[2],*k,devno;
{
	int markval,irtn;
	XEvent event;
/*
.....Loop until a menu selection
*/
	UD_MARK(markval,UU_FALSE);
	*k = 1;
	if (markval == 0)
	{
		while (NPopup >= 0 && PopupChoice[NPopup] == -1)
		{
			uw_mfsetcursor(1);
			XtAppNextEvent(uw_mf.application,&event);
			if (event.type == ButtonPress &&
				(event.xbutton.button == Button3 ||
				event.xbutton.button == Button2))
			{
				if (XtIsRealized(*(PopupWindow[NPopup])))
					XtUnrealizeWidget(*(PopupWindow[NPopup]));
				*PopupWindow[NPopup] = NULL;
				PopupChoice[NPopup] = 0;
				*k = 2;
				if (event.xbutton.button == Button3) *k = 3;
				break;
			}
			else
				XtDispatchEvent(&event);
/*                              if (PopupChoice[NPopup] == 0 && event.type == ButtonPress) *k = 2;*/
		}
/*
.....User hit Go To Root function
.....So the menus are already gone
.....Treat like Reject Op
.....Bobby  -  2/22/99
*/
		if (NPopup < 0)
		{
			*k = 3;
			*choice = 0;
		}
		else *choice = PopupChoice[NPopup];
		irtn = 3;
	}
	else
	{
		*PopupWindow[NPopup] = NULL;
		irtn = 3;
	}
	xy[0] = 0; xy[1] = 0;
	if (NPopup >= 0)
	{
		PopupChoice[NPopup] = -1;
		NPopup--;
	}
/*
.....Bring up any stacked popup menus
*/
	if (NPopup >= 0) XtMapWidget(*(PopupWindow[NPopup]));
	*menu = devno;
	UD_UNMARK(markval);
/*
.....Make sure current menu is set to the active menu
*/
	ug_wsdev.curdevno = devno;
	ug_wsdev.curreqno = devno;
	return(irtn);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfmenu_reset(kmenu,kgraph,kicon)
**       Performs one or more of the following depending on the input
**                      flags.
**
**                      1.  Takes down all non-layout defined menus (always).
**                      2.  Takes down/Displays all layout defined menus, prompt area,
**                          and status area (kmenu).
**                      3.  Takes down/Displays graphics area (kgraph).
**                      4.  Iconifies first menu in layout file (kicon).
**
**                      Typical calls are as follows.
**
**                      GO TO ROOT    - uw_mfmenu_reset(UU_TRUE,UU_TRUE,UU_FALSE)
**                      After Signon  - uw_mfmenu_reset(UU_TRUE,UU_TRUE,UU_FALSE)
**                      Go to Signon  - uw_mfmenu_reset(UU_FALSE,UU_FALSE,UU_FALSE)
**                      Minimize Menu - uw_mfmenu_reset(UU_FALSE,UU_TRUE,UU_TRUE)
**                      Minimize All  - uw_mfmenu_reset(UU_FALSE,UU_FALSE,UU_TRUE)
**
**    PARAMETERS   
**       INPUT  : 
**                              kmenu    = True = Display layout menus.  False = Take down.
**                              kgraph   = True = Display graphics area.  False = Take down.
**                              kicon    = True = Iconify first layout menu.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfmenu_reset(kmenu,kgraph,kicon)
UU_LOGICAL kmenu,kgraph,kicon;
{
	int i,y1;
	UU_LOGICAL ic;
	App_icon = kicon;
/*
.....Take down all user menus
.....in all cases
*/
	for (i=UDM_layout.nmenu;i<UDM_menu_count;i++)
	{
		if (uw_mflayout.menu_app[i] != NULL)
			XtUnrealizeWidget(uw_mflayout.menu_app[i]);
		uw_mflayout.menu_app[i] = NULL;
	}
/*
.....Display all original layout menus and
.....Status and Prompt areas
*/
	if (kmenu)
	{
		for (i=0;i<UDM_layout.nmenu;i++)
		{
			if (uw_mflayout.menu_app[i] != NULL)
				XRaiseWindow(XtDisplay(uw_mflayout.menu_app[i]),
					XtWindow(uw_mflayout.menu_app[i]));
			else
				udm_read_menu(UDM_layout.menu_name[i],UDM_layout.menu_pos[i],
									UDM_layout.menu_size[i], 0, 1, -1);
			if (NPopup >= 0) NPopup--;
		}
		XtMapWidget(uw_mf.prompt_app);
		XtMapWidget(uw_mf.status_app);
		y1 = UDM_layout.graphic_pos[1]+UDM_layout.graphic_size[1];
		for (i=0;i<UDM_layout.nmenu;i++)
			if (UDM_layout.menu_pos[i][1] > y1)
				XtMapWidget(uw_mflayout.menu_app[i]);
	}
/*
.....Take down all original layout menus and
.....Status and Prompt areas
*/
	else
	{
		ic = UU_TRUE;
		if (kicon) ic = UU_FALSE;
		for (i=0;i<UDM_layout.nmenu;i++)
		{
			if (uw_mflayout.menu_app[i] != NULL)
			{
				if (ic) XtUnmapWidget(uw_mflayout.menu_app[i]);
				else
				{
					XIconifyWindow(uw_xw.disp,XtWindow(uw_mflayout.menu_app[i]),
						uw_xw.screen_no);
					ic = UU_TRUE;
				}
			}
			else
			{
				udm_read_menu(UDM_layout.menu_name[i],UDM_layout.menu_pos[i],
									UDM_layout.menu_size[i], 0, 1, -1);
			}
		}
		XtUnmapWidget(uw_mf.status_app);
		XtUnmapWidget(uw_mf.prompt_app);
	}
/*
.....Display Graphics area
*/
	if (kgraph)
	{
		XtMapWidget(uw_mf.graphic_app);
	}
/*
.....Take down Graphics area
*/
	else
	{
		XtUnmapWidget(uw_mf.graphic_app);
	}
/*
.....Display rest of menus
.....(Placed here because banners show on OpenView)
*/
	if (kmenu)
	{
		for (i=0;i<UDM_layout.nmenu;i++)
			if (UDM_layout.menu_pos[i][1] <= y1)
			{
				XtMapWidget(uw_mflayout.menu_app[i]);
			}
	}
/*
.....Flush I/O
*/
	uw_mfflush();
}
/*********************************************************************
**    I_FUNCTION     :  uw_mfdnprompt(ws,num)
**       Take down prompt string.
**    PARAMETERS
**       INPUT  :
**          ws     = Workstation id.
**          num    = Prompt number to take down.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfdnprompt(ws,num)
Gws  ws;
Gint num;
{
	uw_mfwrplabel(" ");
   return;
}


/*********************************************************************
**    I_FUNCTION     :  uw_mfstr_prompt(ws,prompt,loc,num)
**       Pop up prompt string at loc.
**    PARAMETERS
**       INPUT  :
**          ws     = Workstation id.
**          prompt = Prompt text
**          loc    = XY location of prompt in pixels
**          num    = Prompt id number.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfstr_prompt(ws,prompt,loc,num)
Gws     ws;
Gipoint *loc;
char    *prompt;
Gint     num;
{
	uw_mfwrplabel(prompt);
	return;
}

/*********************************************************************
**    I_FUNCTION     :  uw_mfchoice(ws,devno,xy,k,choice,menu)
**       
**      This routine controls the input when a menu is active and we
**      are in choice mode.  It returns the device number and choice
**      number of the menu selected, plus the input event.
**
**    PARAMETERS   
**      INPUT:
**         ws     = Workstation id 
**         devno  = choice device number
**      OUTPUT:
**         k      = ending key or choice value
**         xy     = loc position in dev coords
**         choice = menu choice number, or zero 
**         menu   = the menu number, or zero
**
**    RETURNS      :
**      0 = A keypad-2 key was hit, k contains the key number. 
**      1 = An ASCII kbd key was hit, k contains ascii value.
**      2 = A keypad-1 key was hit, k contains key number. 
**      3 = A tablet puck button was used, k contains number. 
**      NOTE: 4,5 below not possible on skel.
**      4 = The expected device type was used, k contains choice number
**      5 = An inappropriate device was used, k contains device type.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_mfchoice(ws,devno,xy,k,choice,menu)      
Gws ws;
int devno,*k,xy[2],*choice,*menu;
{
	int irtn,pet,select;
	Gchoicest *choicept;
	Gfloat dxy[2];
	float rxy[2];
/*
.....Get the active device and pet number
*/
	devno = ug_dmenun( uw_gl.wid, xy );
	choicept = &(*ug_gksstli.wsopen[uw_gl.wid].inptr).choicedata[devno-1];
	pet = (*choicept).pet;
/*
.....Main loop which waits for an event
.....When a text menu is visible
*/
	if (pet == 24)
	{
		select = uw_mfpopup_choice(menu,choice,xy,k,devno);
	}
/*
.....No text menu is visible
.....Just get the next event
*/
	else
	{
		select = uw_mfget_event(k,&rxy[0],&rxy[1]);
		dxy[0] = rxy[0]; dxy[1] = rxy[1];
		uw_glndctodev(dxy,xy);
		*menu = 0;
		*choice = 0;
	}
	irtn = select;

	return(irtn);
}
#endif
