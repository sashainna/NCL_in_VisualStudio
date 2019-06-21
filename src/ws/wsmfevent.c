#include "usysdef.h"
#if UU_COMP != UU_VAXVMS || UU_OPSYS == UU_ALPHAVMS
#if UU_COMP!=UU_WIN2K

/********************************************************************* 
**  NAME:  wsmfevent.c
**
**		CONTAINS:
**			uw_mfget_event(event,xpos,ypos)
**			uw_mfevent(event,xy)
**			uw_mfgraphicCB(widget, client_data, call_data)
**			uw_mfprocess_event(x_event,event,xpos,ypos)
**			uw_mfresize_graphics()
**			uw_mfkbd(ws,row,col,dev,msg,msglen,k)
**			uw_mfpromptCB(widget, client_data, call_data)
**			uw_mftext_event(x_event,event)
**			uw_mfmouse_event(x_event)
**			uw_check_event()
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       wsmfevent.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:10
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
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PanedW.h>
#include <Xm/DrawingA.h>
#include <Xm/MwmUtil.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <X11/Intrinsic.h>
#endif

/*
.....added for OpenGL Widget
.....Yurong
*/
#ifdef UU_OPENGL
#include <X11/GLw/GLwMDrawA.h>
#endif
#include "driver.h"
#include "dinput.h"
#include "gtblvar6.h"
#include "nclfc.h"
#include "ws.h"
#include "wsxw.h"
#include "wsmf.h"
#include "wsgl.h"
#include "dmotif.h"
#include "uims.h"
#include "view.h"
#include "zkeysym.h"
#include "dasnog.h"
#include "lcom.h"
#include "dselect.h"

/*
.....Global variable definitions
*/
int UW_XWcursor=1;
extern int *UD_ksws;
extern int UV_dynview_active;
extern char *UM_pocket_hwnd;
extern int UZ_nclipv_view;
/*
.....added by Yurong
.....8/27/97
*/
extern UWS_MF uw_mf;
extern UWS_MFPROMPT uw_mfprompt;

static int expose_event = 0;
char *kbdStr;
int kbdKey,kbdEof;
int glresize = 0;
extern Cursor pick_cursor,menu_cursor,loc_cursor,strok_cursor,blank_cursor,
   pan_cursor,rotate_cursor,zoom_cursor,text_cursor,mouse_cursor;
extern int moving_part;
/*
.....added for button&dial
.....Yurong 1/15/98
*/
#ifdef UU_OPENGL
#if UU_COMP == UU_IRIS4D || UU_COMP == UU_SUN
extern int but_dialMotionNotify, but_dialPressNotify, but_dialReleaseNotify;
#endif
#endif
static int curr_cursor = 99;
static Widget tip_popup = NULL;

void uw_mfresize_graphics();
void uw_mfresize_graphics2();
void uw_mf_xflush_expose();
void uw_mfsetcursor();
void uw_mfbcomlin();
void uw_mfecomlin();
void uw_mfshow_tooltip();
void uw_mfclose_tooltip();
void uw_mfdelete_tooltip();

int WS_update_tip = 0;
extern int NCL_pick_verify, UD_picking_active;
static int m_x, m_y;
static char uw_tiptext[80];
Widget tip_label;
static int tooltip_loop = 0;
static int tooltip_unmap = 0;
extern int UW_rubber_active;
extern int UD_string_add;
extern int UD_textpre_curpos;
#define HILITE_MARK 0
#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3
extern int NCL_mark_method;
static int Skbd_open = 0;

extern int UW_keypad;

/*********************************************************************
**    E_FUNCTION : uw_mousemoveCB (widget,client_data,event)
**      
**    DESCRIPTION:
**        Mouse moving callback for graphic window
**
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = Ignored.
**				event			= Ignored.
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mousemoveCB (widget,
	client_data,
	event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	if (NCL_mark_method != DYNAMIC_MARK)
		return;
	if (!((UD_picking_active) && (NCL_pick_verify)))
	{
		uw_mfdelete_tooltip();
		WS_update_tip = 0;
		m_x = -1; m_y=-1;
		tooltip_loop = 0;
	}
	return;
}
/*********************************************************************
**    E_FUNCTION : tooltip_display(x,y)
**      
**    DESCRIPTION:
**        Display tooltip
**
**    PARAMETERS   
**       INPUT  : 
**      			x,y: mouse position    
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void tooltip_display(x,y)
int x,y;
{
	if (NCL_mark_method != DYNAMIC_MARK)
		return;
	if ((UD_picking_active) && (NCL_pick_verify))
	{
		if ((uw_tiptext[0]!='\0')&& (abs(x-m_x)<2) && (abs(y-m_y)<2))
		{
			tooltip_loop = 0;
			uw_mfshow_tooltip(m_x, m_y, uw_tiptext);
		}
	}
}
/*********************************************************************
**    E_FUNCTION : uw_mousemove (x,y)
**      
**    DESCRIPTION:
**        Function to handle mouse move
**
**    PARAMETERS   
**       INPUT  : 
**      			x,y: mouse position    
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mousemove (x,y)
int x, y;
{
	int stat;
	int xy[2];

	if (NCL_mark_method != DYNAMIC_MARK)
		return;
#ifdef UU_OPENGL
	if ((UW_rubber_active) || (!((UD_picking_active) && (NCL_pick_verify))))
	{
		m_x = -1; 
		m_y = -1;
		WS_update_tip = 0;
		tooltip_loop = 0;
		return;
	}
	if ((abs(x-m_x)<2) && (abs(y-m_y)<2)&& (WS_update_tip==0))
	{
		return;
	}
	else
	{
		uw_mfclose_tooltip();
	}
	tooltip_loop = 0;
	uw_tiptext[0] = '\0';
	xy[0] = x;
	xy[1] = YTRANSPOSE(y);
	m_x = x;
	m_y = y;
	stat = uw_glget_piksurf(xy[0], xy[1], uw_tiptext,1);
	if (stat==-1)
		uw_tiptext[0] = '\0';
	WS_update_tip = 0;
#endif
}

/*********************************************************************
**    E_FUNCTION : uw_mfget_event(event,xpos,ypos)
**      
**    DESCRIPTION:
**        Get the next event in the input queue.  This routine waits
**        until an event is received before returning.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          event      =  Key or mouse button entered
**          xpos,ypos  =  Position of mouse when event received
**
**    RETURNS      :
**          1  =  Normal character key.
**          2  =  Keypad key.
**          3  =  Mouse button.
**				4  =  Buttons and Dials
**          5  =  Arrow key.
**				6  =  Button release.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfget_event(event,xpos,ypos)
int *event;
float *xpos,*ypos;
{       
	int irtn,cursno,isav,stat;
	XEvent x_event;
	Window win;
	char lstr[200],pstr[200],estr[200];
	Gfloat uw_gldevxtondc();
/*
.....No key entered yet
*/
reloop:;
	irtn = 0;
/*
.....Save prompt area labels
.....just in case they are overwritten
.....by another call during input
.....Bobby  -  11/06/98
*/
	uw_mfgetprm(pstr); uw_mfgetprlabel(lstr); uw_mfgetprmerr(estr);
/*
.....Loop until we get a valid event
*/
	cursno = UW_XWcursor;
	while (!irtn)
	{
/*
.....Update the graphics
*/
		ud_updatews(UG_SUPPRESS);
/*
.....Dynamic viewing active
.....Get any Mouse Button events prior
.....to mouse movement events
*/
		stat = UU_FALSE;
		if (UV_dynview_active) stat = uw_mfbutton_event(&x_event);

		if (!stat) XtAppNextEvent(uw_mf.application,&x_event);
/*
.....Pocket window input is active
*/
#ifdef UU_IPV
		if (UM_pocket_hwnd != 0)
		{
			bevent = (XButtonPressedEvent *)&x_event;
			if (x_event.xany.window == (Window)UM_pocket_hwnd &&
				x_event.type == ButtonPress)
			{
					xd = bevent->x;
					yd = bevent->y;
					*xpos = uw_gldevxtondc(xd);
					*ypos = uw_gldevxtondc(yd);
					irtn = 3;
					*event = 1;
					if (bevent->button == Button2) *event = 2;
					if (bevent->button == Button3) *event = 3;
					return(irtn);
			}
			if (x_event.xany.window == uw_xw.wd_id &&
				x_event.type == ButtonPress && (bevent->button == Button2 ||
				bevent->button == Button3))
			{
					*xpos = 0;
					*ypos = 0;
					irtn = 3;
					if (bevent->button == Button2) *event = 2;
					else *event = 3;
					return(irtn);
			}
			else
			{
				hwnd = UM_pocket_hwnd;
				UM_pocket_hwnd = 0;
				isav = expose_event;
				XtDispatchEvent(&x_event);
				expose_event = isav;
				irtn = expose_event;
				*xpos = 0;
				*ypos = 0;
				UM_pocket_hwnd = hwnd;
			}
			return(irtn);
		}
#endif
/*
.....Normal input
*/

		if (UV_dynview_active && UZ_nclipv_view == 1)
			uw_mfget_pockwin(&win);
		else
			win = 0;
		if (x_event.xany.window != uw_xw.wd_id && x_event.xany.window != win)
		{
			isav = expose_event;
			XtDispatchEvent(&x_event);
			expose_event = isav;
			irtn = expose_event;
		}
		else
		{
			isav = expose_event;
			irtn = uw_mfprocess_event(&x_event,event,xpos,ypos);
			expose_event = isav;
		}
/*
.....Restore prompt & error text strings
*/
		uw_mfwrplabel(lstr);
		uw_mfwrprm(pstr);
		uw_mfprmerr(estr);
	} /* end while */
/*
.....Return event code
*/
	return(irtn);

}

/*********************************************************************
**    E_FUNCTION : uw_mfevent(event,xy)
**      
**    DESCRIPTION:
**        Get the next event in the input queue.  This routine does not
**        wait until an event is received before returning.  It will
**        return with the current mouse position.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          event      =  Key or mouse button entered.  Set to 0 when
**                        an event is not available.
**          xpos,ypos  =  Position of mouse
**
**    RETURNS      :
**          1  =  Normal character key.
**          2  =  Keypad key.
**          3  =  Mouse button.
**				4  =  Buttons and Dials.
**          5  =  Arrow key.
**				6  =  Button Release.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfevent(event,xy)
int *event,xy[2];
{
	int irtn,root_x,root_y,x,y;
	unsigned int keys_buttons;
	float rxy[2];
	Gfloat dxy[2];
	Window root,child;
	XMotionEvent *mtevent;
	XEvent x_event;
	Window win1, win2;
/*
.....Have uw_xw_get_event
.....return on Expose Events
*/
	expose_event = -1;
/*
.....No event on the queue
*/
	if (XPending(uw_xw.disp) == 0)
	{
/*
.....Get the current mouse position
*/
		XQueryPointer(uw_xw.disp,uw_xw.wd_id,&root,&child,&root_x,&root_y,
			&xy[0],&xy[1],&keys_buttons);
		x = xy[0];
		y = xy[1];
		xy[1] = YTRANSPOSE(xy[1]);
		irtn = 0;
		if ((UD_picking_active) && (NCL_pick_verify)
					&& (NCL_mark_method == DYNAMIC_MARK))
		{
			tooltip_loop++;
			if (tooltip_loop==1000)
			{
				tooltip_loop = 0;
				tooltip_display(x, y);
			}
		}
		else
			tooltip_loop = 0;
	}
/*
.....Return the event on the input queue
*/
	else
	{
		if (XtAppPeekEvent(uw_mf.application, &x_event))
		{
			if (x_event.type==MotionNotify)
			{
				win1 = XtWindow(uw_mf.graphic);
				win2 = XtWindow(uw_mf.graphic_app);
				if ((x_event.xany.window==win1) ||
							(x_event.xany.window==win2))
				{
					mtevent = (XMotionEvent *)&x_event;
					XQueryPointer(uw_xw.disp,uw_xw.wd_id,&root,&child,&root_x,
								&root_y, &xy[0],&xy[1],&keys_buttons);
/*
.....only if the corrent points is the same as event point,
.....we call uw_mousemove because all move event before the
.....current point will be sent here (if all called, it will
....cost a lot of time 
*/
					if ((mtevent->x==xy[0]) && (mtevent->y==xy[1]))
					{
						uw_mousemove(mtevent->x, mtevent->y);
					}
					else
					{
						if ((UD_picking_active) && (NCL_pick_verify)
								&& (NCL_mark_method == DYNAMIC_MARK))
						{
							uw_mfclose_tooltip();
							tooltip_loop = 0;
						}
					}
				}
			}
			irtn = uw_mfget_event(event,&rxy[0],&rxy[1]);
		}
		if (irtn == expose_event)
		{
			XQueryPointer(uw_xw.disp,uw_xw.wd_id,&root,&child,&root_x,&root_y,
				&xy[0],&xy[1],&keys_buttons);
			xy[1] = YTRANSPOSE(xy[1]);
			irtn = 0;
		}
		else
		{
			dxy[0] = rxy[0];
			dxy[1] = rxy[1];
/*
...added openGL
...Yurong
*/
			uw_glndctodev(dxy,xy);
		}
	}
/*
.....Reset expose event flag
*/
	expose_event = 0;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION : uw_mfgraphicCB(widget,client_data,call_data)
**      
**    DESCRIPTION:
**        Get the next event in the input queue.  This routine waits
**        until an event is received before returning.
**
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = Ignored.
**				call_data   = Motif callback structure containing event.
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfgraphicCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int irtn,event,ir;
	float xpos,ypos;
	char *index, buf[20], num[20];
	XmDrawingAreaCallbackStruct *cbs;
/*
.....added for OpenGL Widget
.....Yurong 7/22/97
*/
#ifdef UU_OPENGL
	GLwDrawingAreaCallbackStruct *gbs;
#endif
	XEvent *x_event;
	int reason;
	int input, resize, expose;
	int jmpflag, save_ptype,ptype_saved;
	int * save_inptr;
	int mark = 0;
/*
...Set clock cursor
...Yurong 7/28/97
*/
/*
.....only set wait cursor when we do have a function to execute
	uw_mfsetcursor(21); 
*/
/*
.....add for OpenGL Widget
.....Yurong 7/22/97
*/
	gbs= (GLwDrawingAreaCallbackStruct *)call_data;
	cbs= (XmDrawingAreaCallbackStruct *)call_data;
	x_event = gbs->event;
	reason = gbs->reason;
	input = GLwCR_INPUT;
	resize = GLwCR_RESIZE;
	expose = GLwCR_EXPOSE;
/*
.....Input key
*/
	index = buf;
	ptype_saved = 0;
	if (reason == input || reason == expose)
	{
		irtn = uw_mfprocess_event(x_event,&event,&xpos,&ypos);
		if ((irtn == 1) || (irtn == 2))
		{
			mark = 1;
			ug_save_event();
			ug_save_input(&save_inptr);
			if (uw_mfis_inputfunc(event)==0)
			{
				ptype_saved = 1;
				save_ptype = ud_getpick_type();
				ud_setpick_type(UD_PICK_NORMAL);
			}
			UD_MARK(jmpflag,UU_TRUE);
			if (jmpflag == 0)
			{
				if (irtn == 1)
				{
/*
... added for record keyboard event
... Yurong
*/
					if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
					{
						sprintf(num,"%d", event);
						ud_rpwrmenu(num, "", "KEYBOARD");
					}
					uw_mfsetcursor(21); 
					ir = uz_user_key(event,&index,1);
				}
				else if (irtn == 2)
				{
					uw_mfsetcursor(21); 
					ir = uz_user_fkey(event,&index,1);
				}
			}
		}
		else
		{
			ir = 0;
		}
/*
........GO TO ROOT
........Restore original menu layout
*/
		if (ir == 1)
		{
			uw_mfsetcursor(21); 
			if ( strcmp(index,"\\\\0") == 0)
				uw_mfmenu_reset(UU_TRUE,UU_TRUE,UU_FALSE);
			else
/* 
... added for execute function keys
... Yurong
*/
			{
				uz_user_dascalls(index);
			}
		}
		if (mark==1)
		{
			ug_reset_event();
			ug_reset_input(save_inptr);
			if (ptype_saved)
				ud_setpick_type(save_ptype);
			UD_UNMARK(jmpflag);
		}
	}
/*
.....Resize event
*/
	else if (reason == resize)
	{
		uw_mfsetcursor(21); 
		uw_mfresize_graphics();
	}
}

/*********************************************************************
**    E_FUNCTION : uw_mfprocess_event(x_event,event,xpos,ypos)
**      
**    DESCRIPTION:
**        Parses a single X event.
**
**    PARAMETERS   
**       INPUT  : 
**          x_event    =  X event to process.
**       OUTPUT :  
**          event      =  Key or mouse button entered
**          xpos,ypos  =  Position of mouse when event received
**
**    RETURNS      :
**          1  =  Normal character key.
**          2  =  Keypad key.
**          3  =  Mouse button.
**          5  =  Arrow key.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_mfprocess_event(x_event,event,xpos,ypos)
int *event;
XEvent *x_event;
float *xpos,*ypos;
{
	int irtn,rebound,xd,yd,ll[2],ur[2],ibuf,ir;
	char buffer[100];
	Gnrect rect;
	Gfloat nll[2],nur[2];
	KeySym keysym;
	XKeyPressedEvent *kevent;
	XButtonPressedEvent *bevent;
	XExposeEvent *eevent;
	XClientMessageEvent *mevent;
	char *index;
#ifdef UU_OPENGL
	Gfloat uw_gldevxtondc();
	Gfloat uw_gldevytondc();
#endif
	irtn = expose_event;
	switch(x_event->type)
	{
/*
.......Expose event
.......Redraw the exposed rectangle
*/
	case Expose:
		uw_mfsetcursor(21); 
		eevent = (XExposeEvent *)x_event;
		ll[0] = eevent->x;
		ll[1] = YTRANSPOSE((eevent->y + eevent->height));
		ur[0] = eevent->x + eevent->width;
		ur[1] = YTRANSPOSE(eevent->y);
		uw_gldevtondc(ll,nll);
		uw_gldevtondc(ur,nur);


		rect.ll.x = nll[0];
		rect.ll.y = nll[1];
		rect.ur.x = nur[0];
		rect.ur.y = nur[1];
/*
.....only redraw in the front buffer, no need for back buffer
*/
		uw_glredrawsegrect(uw_gl.wid,&rect,-1);
		uw_gldraw_pickfm();
		*xpos = 0;
		*ypos = 0;
		irtn = expose_event;
#if UU_COMP == UU_SUN
/*		XSetInputFocus(uw_xw.disp,uw_xw.wd_id,RevertToNone,CurrentTime); */
#endif
		break;
/*
.......Resize event
.......Recalculate the Graphics area
*/
	case ConfigureNotify:
		uw_mfsetcursor(21); 
		uw_mfresize_graphics();
		break;
/*
.......Keyboard Focus
*/
	case FocusIn:
#if UU_COMP == UU_SUN
/*		XSetInputFocus(uw_xw.disp,uw_xw.wd_id,RevertToNone,CurrentTime); */
#endif
		*xpos = 0;
		*ypos = 0;
		irtn = expose_event;
		break;
	case FocusOut:
		*xpos = 0;
		*ypos = 0;
		irtn = expose_event;
		break;
/*
.......Keyboard key
*/
	case KeyPress:
		kevent = (XKeyPressedEvent *)x_event;
		xd = kevent->x;
		yd = YTRANSPOSE(kevent->y);
/*
...added openGL
...Yurong
*/
		*xpos = uw_gldevxtondc(xd);
		*ypos = uw_gldevytondc(yd);

		rebound = XLookupString(kevent, buffer, sizeof(buffer),
			&keysym, NULL);
/*
......use keysym not buffer[0]
......it will have error, for example, when ^2, the buffer[0] = 0
......Yurong
		ibuf = buffer[0];
*/
		ibuf = keysym;
/*	printf("keysym = %x   buf = %x   rebound = %d\n",keysym,buffer,rebound);*/
/*
.........Arrow key
*/
		if ((keysym == XK_Left ) || (keysym == XK_Up) ||
			(keysym == XK_Right) || (keysym == XK_Down))
		{
			irtn = 5;
			switch(keysym)
			{
			case XK_Left:
				*event = 4;
				break;	
			case XK_Up:
				*event = 1;
				break;
			case XK_Right:
				*event = 3;
				break;
			case XK_Down:
				*event = 2;
				break;
			}
		}
/*
.........Pause/Scroll Lock
*/
		else if (keysym == XK_Pause || keysym == XK_Scroll_Lock)
		{
			irtn = 2;
			*event = keysym - XK_Pause + 1;
		}
		else if (keysym == XK_Multi_key)
		{
			irtn = 2;
			*event = keysym - XK_Multi_key + 3;
		}
/*
.........Cursor function keys
*/
		else if (keysym == XK_Home)
		{
			irtn = 2;
			*event = keysym - XK_Home + 4;
		}
		else if (keysym >= XK_Prior && keysym <= XK_End)
		{
			irtn = 2;
			*event = keysym - XK_Prior + 5;
		}
/*
.........Miscellaneous function keys
*/
		else if (keysym >= XK_Select && keysym <= XK_Insert)
		{
			irtn = 2;
			*event = keysym - XK_Select + 8;
		}
		else if (keysym == XK_Undo)
		{
			irtn = 2;
			*event = keysym - XK_Undo + 12;
		}
		else if (keysym >= XK_Find && keysym <= XK_Break)
		{
			irtn = 2;
			*event = keysym - XK_Find + 13;
		}
		else if (keysym == XK_Num_Lock)
		{
			irtn = 2;
			*event = keysym - XK_Num_Lock + 17;
		}
/*
.........Keypad function keys
*/
		else if (keysym == XK_KP_Space)
		{
			irtn = 2;
			*event = keysym - XK_KP_Space + 18;
		}
		else if (keysym == XK_KP_Tab)
		{
			irtn = 2;
			*event = keysym - XK_KP_Tab + 19;
		}
		else if (keysym == XK_KP_Enter)
		{
			irtn = 2;
			*event = keysym - XK_KP_Enter + 20;
		}
		else if (keysym >= XK_KP_F1 && keysym <= XK_KP_F4)
		{
			irtn = 2;
			*event = keysym - XK_KP_F1 + 21;
		}
		else if (keysym == XK_KP_Equal)
		{
			irtn = 2;
			*event = keysym - XK_KP_Equal + 25;
		}
		else if (keysym >= XK_KP_Multiply && keysym <= XK_KP_9)
		{
			irtn = 2;
			*event = keysym - XK_KP_Multiply + 26;
		}
/*
.........Standard function keys (F1-F35)
*/
		else if (keysym >= XK_F1 && keysym <= XK_F35)
		{
			irtn = 2;
			*event = keysym - XK_F1 + 42;
		}
/*
.........Alpha-numeric keys
*/
/*
......when ^2 ibuf = 0
*/
/*		else if (keysym > 0 && ibuf <= NKEYSYMS && ibuf > 0)
*/
		else if (keysym > 0 && ibuf <= NKEYSYMS && ibuf >= 0)
		{
			if (rebound > 0)
			{
				irtn = 1;
/*
......use keysym not buffer[0]
......it will have error, for example, when ^2, the buffer[0] = 0
......Yurong
				*event = buffer[0];
*/
				*event = keysym;
			}
		}
/*
......if ibuf > NKEYSYMS, check buffer[0]
......Yurong 2/21/01
*/
		else if ((ibuf > NKEYSYMS)&&(buffer[0]>0)&&(buffer[0]<NKEYSYMS))
		{
			*event = buffer[0];
			irtn = 1;
		}
/*
.......Controlled Normal key
*/
		ir = 0;
		if (irtn == 1 && kevent->state & ControlMask)
		{
/*
......Added one more statement because we
......use keysym not buffer[0]
*/
			if (*event >= 97 && *event <= 122) *event = *event - 96;
			if (*event == 0) *event = 64;
			if (*event >= 32) *event = *event + NKEYSYMS;
			ir = uz_user_key(*event,&index,0);
		}
/*
.......Controlled Function key
*/
		if (irtn == 2)
		{
			if (kevent->state & ControlMask)
				*event = *event + NFKEYSYMS * 2;
/*
.......Shifted Function key
*/
			else if (kevent->state & ShiftMask)
				*event = *event + NFKEYSYMS;
			ir = uz_user_fkey(*event,&index,0);
		}
/*
.....Immediate function key
.....Go get another key
*/
		if (ir == -1)
		{
			irtn = expose_event;
		}
/*
.......CLEAR FIELD converts to ^U
*/
		if (ir == 1)
		{
			if (strcmp(index,"\\\\I") == 0)
			{
				irtn = 1;
				*event = 21;
			}
/*
.....RIGHT ARROW converts to ->
*/
			else if (strcmp(index,"\\\\L") == 0)
			{
				irtn = 5;
				*event = 3;
			}
/*
.....LEFT ARROW converts to <-
*/
			else if (strcmp(index,"\\\\M") == 0)
			{
				irtn = 5;
				*event = 4;
			}
/*
.....Bline converts to ^A
*/
			else if (strcmp(index,"\\blin") == 0)
			{
				irtn = 1;
				*event = 1;
			}
/*
.....Eline converts to ^E
*/
			else if (strcmp(index,"\\elin") == 0)
			{
				irtn = 1;
				*event = 5;
			}
		}
		break;
/*
.......Mouse button
*/
	case ButtonRelease:
	case ButtonPress:
		bevent = (XButtonPressedEvent *)x_event;
		xd = bevent->x;
		yd = YTRANSPOSE(bevent->y);
/*
...added openGL
...Yurong
*/
		*xpos = uw_gldevxtondc(xd);
		*ypos = uw_gldevytondc(yd);
		irtn = 3;
		if (x_event->type == ButtonRelease) irtn = 6;
		switch(bevent->button)
		{
		case Button1:
			*event = 1;
			break;
		case Button2:
			*event = 2;
		  	break;
		case Button3:
			*event = 3;
		}
		break;
/*
.......Client messages (DAS function)
.......Sent from Motif Menu Callback
*/
	case ClientMessage:
		*xpos = 0;
		*ypos = 0;
		mevent = (XClientMessageEvent *)x_event;
		irtn = expose_event;
		if (mevent->format==16)
		{
			if (mevent->data.s[0] == 1111)
			{
				*event = mevent->data.s[1];
				irtn = 2;
			}
		}
		else if (mevent->format==8)
		{
			uw_mfupd_cinput(mevent->data.b);
		}
		break;
/*
.......Unrecognized event
*/
	default:
/*
......added for button and dail
......Yurong 1/15/97
*/
#if UU_COMP == UU_IRIS4D 
#ifndef UU_RS6000
		if ((x_event->type==but_dialMotionNotify)||
				(x_event->type==but_dialPressNotify)||
				(x_event->type==but_dialReleaseNotify))
		{
			uw_mfsetcursor(21); 
			ir = uw_mfbut_dail(x_event, event);
			irtn = 4;
/*
.....Immediate function key
.....Go get another key
*/
			if (ir == -1)
			{
				irtn = expose_event;
			}
		}
		else
#endif
#endif
			irtn = expose_event;

/*		printf("Illegal event - type %d\n", x_event->type);*/
		*xpos = 0;
		*ypos = 0;
	}
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION : uw_mfresize_graphics()
**      
**    DESCRIPTION:
**        This routine is called whenever the user resizes the graphics
**			 area.  It recalculates the viewing matrix scale, adjusting
**			 the graphics display to fit within the new window. 
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfresize_graphics()
{
	Dimension wid,hgt;
/*
.....Get the new window size
*/
	XtVaGetValues(uw_mf.graphic,XmNwidth,&wid,XmNheight,&hgt,NULL);
	uw_mfresize_graphics2(wid, hgt);
}

/*********************************************************************
**    E_FUNCTION : uw_mfresize_graphics2(int wid, int hgt)
**      
**    DESCRIPTION:
**			 It recalculates the viewing matrix scale, adjusting
**			 the graphics display to fit within the new window. 
**
**    PARAMETERS   
**       INPUT  : 
**          wid, hgt: size
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfresize_graphics2(wid, hgt)
int wid, hgt;
{
	int i;
	UV_vport vport;
	UV_view view;
	glresize = 1;
/*
.....Let DAS perform an internal resize
*/
	udm_resize_graphics(&UD_duimsdeflt,wid,hgt,0);
/*	ud_setuims(&UD_duimsdeflt,&UD_curlayout,0);*/
/*
.....Redisplay all views at the new scale
*/
	gswswindow(UD_ksws,&(UD_duimsdeflt.screen[0].wswind));
	ug_ndcboxdel(); 
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		view.modified = UU_TRUE;
		uv_delete_hidden(&vport);
		uv_autofact7_redrawvp(&vport, &view, UU_TRUE); 
	}
	uw_mf_xflush_expose();
	glresize = 0;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfkbd(ws,row,col,dev,msg,msglen,k)
**       Managing routine for the "By Text" prompt area.  Gets the user's
**			input from the text input area.
**    PARAMETERS   
**       INPUT  : 
**				ws      = Ignored.
**          msg     = Text message to display in Prompt area.
**				msglen  = Length of 'msg'.
**				row     = Ignored.
**				col     = Ignored.
**				dev     = Ignored.
**       OUTPUT :  
**          k       = Key that terminated input.  Text entered is
**				          stored in 'kbdStr'.
**    RETURNS      : none
**    SIDE EFFECTS : Redirects keyboard input from all other
**		               windows into the text field.
**    WARNINGS     : none
*********************************************************************/
int uw_mfkbd(ws,row,col,dev,msg,msglen,k)
char *msg;
int msglen,*k;
int row,col,dev;
Gws ws;
{
	XEvent x_event;
	int ir;
	char *index,*nstr="";
	char lstr[200],pstr[200],estr[200], fname[80];
	char *savstr, buf[256];
	Gnrect ndcrect2;
	int sav_mode, save_ptype;
	int ist;
	int markval;
	static XmTextScanType sarray1[] =
		{XmSELECT_POSITION, XmSELECT_WORD, XmSELECT_LINE, XmSELECT_ALL};
	static XmTextScanType sarray2[] =
		{XmSELECT_ALL, XmSELECT_POSITION, XmSELECT_WORD, XmSELECT_LINE};
/*
.....Save currurent event in order to
.....execute event that interupt by select menu
.....after execute menu function
.....Yurong 9/9/98
*/
	ug_save_event();
	sav_mode = UD_pickmode ;
	save_ptype = ud_getpick_type();
	savstr = XmTextGetString(uw_mfprompt.prompt);

	Skbd_open = 1;
	index = buf;

	UD_MARK(markval, UU_FALSE);
	if (markval != 0)
		goto done;
/*
.....Enable keyboard input &
.....Display default line &
.....Grab Keyboard
*/
   if (UW_text_select)
   {
      XtVaSetValues(uw_mfprompt.prompt,
			XmNselectionArray, sarray2,
      	XmNselectionArrayCount, 4, NULL);
   }
	else
   {
      XtVaSetValues(uw_mfprompt.prompt,
			XmNselectionArray, sarray1,
      	XmNselectionArrayCount, 4, NULL);
   }
	XtSetSensitive(uw_mfprompt.prompt,True);
	XmTextSetString(uw_mfprompt.prompt,msg);
/*
.....added flag Yurong 4/3/00
*/
	if (UD_string_add)
	{
		XmTextSetInsertionPosition(uw_mfprompt.prompt,UD_textpre_curpos);
	}
	else
	{
		if (UW_text_cursor)
			XmTextSetInsertionPosition(uw_mfprompt.prompt,strlen(msg));
		else
			XmTextSetInsertionPosition(uw_mfprompt.prompt,0);
	}
/*
.....Make the Command Line's Text Field
.....Active at all times
.....Previously, using the up and down arrows or
.....Return key would unhilite the text field
.....Bobby  -  1/12/98
*/
	XmProcessTraversal(uw_mfprompt.prompt,XmTRAVERSE_CURRENT);
/*
.....Save prompt area labels
.....just in case they are overwritten
.....by another call during text input
.....Bobby  -  9/25/97
*/
	uw_mfgetprm(pstr); uw_mfgetprlabel(lstr); uw_mfgetprmerr(estr);
/*
.....Loop until End-of-Input
.....Which is set in the Text Field Activation Callback
*/
	kbdEof = 0;
/*
......should set this at beginning just after save the event and command
......string
*/
/*
	UD_MARK(markval, UU_FALSE);
	if (markval != 0)
		goto done;
*/
	while (!kbdEof)
	{
		UD_textpre_curpos = XmTextGetInsertionPosition(uw_mfprompt.prompt);
		uw_mfsetcursor(22);
		XtAppNextEvent(uw_mf.application,&x_event);
/*
........Get input event
*/
		kbdEof = uw_mftext_event(&x_event,&kbdKey);
/*
........Test for immediate mode Function Key
*/
		ir = 0;
		if (kbdEof == 1)
/*
.....ctrl normal key
*/
		{
			uw_mfsetcursor(21);
			if (uw_mfis_inputfunc(kbdKey)==0)
				ud_setpick_type(UD_PICK_NORMAL);
			ir = uz_user_key(kbdKey,&index,1);
			if ((kbdKey!=013) || (kbdKey!=015))
				kbdEof = 0;
		}
		if (kbdEof == 2) 
		{
/*
.....set clock cursor
.....Yurong 7/28/97
*/
			uw_mfsetcursor(21); 
			uz_user_fkey_name(kbdKey, fname);
			if ((strcmp(fname, "NCL_CALCULATOR")==0) ||
					(strcmp(fname, "CAM_SCALARS")==0))
			{
				ir = uz_user_fkey(kbdKey,&index,1);
			}
			else
			{
				ir = uz_user_fkey(kbdKey,&index,0);
			}
		}
/*
......added button &dial
......Yurong 1/16/98
*/
		if (kbdEof == 4)
		{
			uw_mfsetcursor(21);
			ir = uz_user_button(kbdKey,&index,0);
		}
		
		if (ir == -1) kbdEof = 0;
/*
........We still support Clear field
*/
		else if (ir == 1 && strcmp(index,"\\\\I") == 0)
		{
			XmTextSetString(uw_mfprompt.prompt,nstr);
			kbdEof = 0;
		}
		else if (ir == 1 && strcmp(index,"\\blin") == 0)
		{
			uw_mfbcomlin();
			kbdEof = 0;
		}
		else if (ir == 1 && strcmp(index,"\\elin") == 0)
		{
			uw_mfecomlin();
			kbdEof = 0;
		}
		else if (ir == 5)
		{
			if (index[0]!='\0')
				uw_mfupd_cinput(index);
			kbdEof = 0;
		}
		else if (kbdEof==0) XtDispatchEvent(&x_event);
/*
........In case this is reentrant code
........Set the prompt area sensitivity
*/
		XtSetSensitive(uw_mfprompt.prompt,True);
		ud_updatews(UG_SUPPRESS);
/*
.....added 11/2/97
*/
      if (ug_getredrwflag()==1)
      {
/*
...........Make a 2D copy of ug_redrw.ndcrect in ndcrect2
*/
			ug_getredrwrect2(&ndcrect2);
			uw_glredrawsegrect(uw_gl.wid,&ndcrect2,-1);
/*
...........Reset ndcrect to null rectangle
*/
			ug_resetredrwrect();
         ug_setredrwflag(0);           /* remember don't have to redraw */
      }

/*
.....Restore prompt & error text strings
*/
		uw_mfwrplabel(lstr);
		uw_mfwrprm(pstr);
		uw_mfprmerr(estr);
/*
.....Reset currurent event in order to
.....execute event that interupt by select menu
.....Yurong 9/9/98
*/
		ug_reset_event();
		UD_pickmode = sav_mode;
		ud_setpick_type(save_ptype);
	}
done:;
/*
.....Disable text input
*/
	kbdStr = XmTextGetString(uw_mfprompt.prompt);
	XtSetSensitive(uw_mfprompt.prompt,False);
/*
.....Reset default string
.....(reentrant code)
*/
	XmTextSetString(uw_mfprompt.prompt,savstr);
	XtFree(savstr);
/*
.....Return key that ended input
*/
	*k = kbdKey;
	strcpy(msg,kbdStr);
	XtFree(kbdStr);
/*
.....set clock cursor
.....Yurong 7/28/97
*/
	uw_mfsetcursor(21); 
/*
.....Reset currurent event in order to
.....execute event that interupt by select menu
.....Yurong 9/9/98
*/
	ug_reset_event();
	UD_pickmode = sav_mode;
/*
.....Reset kbd*** just in case this routine
.....is called and it is already active
.....(reentrant code)
.....Bobby  -  5/12/99
*/
	ist = kbdEof;
	kbdKey = 0;
	kbdEof = 0;
	Skbd_open = 0;
	UD_UNMARK(markval);
	return(ist);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfpromptCB(widget,client_data,call_data)
**       "By Text" activation callback routine.  Called when the user
**			enters the RETURN or ENTER key.
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Ignored.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfpromptCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	kbdKey = '\015';
	kbdEof = 1;
}

/*********************************************************************
**    E_FUNCTION : uw_mftext_event(x_event,event)
**      
**    DESCRIPTION:
**        Parses the X event while in text input mode.  Any keyboard
**			 or mouse input in any other window is redirected to the
**			 text window.
**
**    PARAMETERS   
**       INPUT  : 
**          x_event    =  X event obtained from server.
**       OUTPUT :  
**          event      =  Key or mouse button entered.
**
**    RETURNS      :
**				0  =  Unrecognized event.
**          1  =  Normal character key.
**          2  =  Keypad key.
**          3  =  Mouse button.
**          5  =  Arrow key.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_mftext_event(x_event,event)
int *event;
XEvent *x_event;
{
	int irtn,rebound,ibuf, ir;
	char buffer[100];
	KeySym keysym;
	XKeyPressedEvent *kevent;
	XButtonPressedEvent *bevent;
	XClientMessageEvent *mevent;
	irtn = 0;
	switch(x_event->type)
	{
/*
.......Keyboard key
*/
	case KeyPress:
		x_event->xany.window = XtWindow(uw_mfprompt.prompt);
		kevent = (XKeyPressedEvent *)x_event;
		rebound = XLookupString(kevent, buffer, sizeof(buffer),
			&keysym, NULL);
/*
......use keysym not buffer[0]
......it will have error, for example, when ^2, the buffer[0] = 0
......Yurong
		ibuf = buffer[0];
*/
		ibuf = keysym;
/*
.........Arrow key
*/
		if ((keysym == XK_Left ) || (keysym == XK_Up) ||
			(keysym == XK_Right) || (keysym == XK_Down))
		{
			irtn = 5;
			switch(keysym)
			{
			case XK_Left:
			case XK_Right:
				irtn = 0;
				break;	
			case XK_Up:
				*event = 128;
				break;
			case XK_Down:
				*event = 129;
				break;
			}
		}
/*
.........Pause/Scroll Lock
*/
		else if (keysym == XK_Pause || keysym == XK_Scroll_Lock)
		{
			irtn = 2;
			*event = keysym - XK_Pause + 1;
		}
		else if (keysym == XK_Multi_key)
		{
			irtn = 2;
			*event = keysym - XK_Multi_key + 3;
		}
/*
.........Cursor function keys
*/
		else if (keysym == XK_Home)
		{
			irtn = 2;
			*event = keysym - XK_Home + 4;
		}
		else if (keysym >= XK_Prior && keysym <= XK_End)
		{
			irtn = 2;
			*event = keysym - XK_Prior + 5;
		}
/*
.........Miscellaneous function keys
*/
		else if (keysym >= XK_Select && keysym <= XK_Insert)
		{
			irtn = 2;
			*event = keysym - XK_Select + 8;
		}
		else if (keysym == XK_Undo)
		{
			irtn = 2;
			*event = keysym - XK_Undo + 12;
		}
		else if (keysym >= XK_Find && keysym <= XK_Break)
		{
			irtn = 2;
			*event = keysym - XK_Find + 13;
		}
		else if (keysym == XK_Num_Lock)
		{
			irtn = 2;
			*event = keysym - XK_Num_Lock + 17;
		}
/*
.........Keypad function keys
*/
		else if (keysym == XK_KP_Space)
		{
			irtn = 2;
			*event = keysym - XK_KP_Space + 18;
		}
		else if (keysym == XK_KP_Tab)
		{
			irtn = 2;
			*event = keysym - XK_KP_Tab + 19;
		}
		else if (keysym == XK_KP_Enter)
		{
			irtn = 2;
			*event = keysym - XK_KP_Enter + 20;
		}
		else if (keysym >= XK_KP_F1 && keysym <= XK_KP_F4)
		{
			irtn = 2;
			*event = keysym - XK_KP_F1 + 21;
		}
		else if (keysym == XK_KP_Equal)
		{
			irtn = 2;
			*event = keysym - XK_KP_Equal + 25;
		}
		else if (keysym >= XK_KP_Multiply && keysym <= XK_KP_9)
		{
			irtn = 2;
			*event = keysym - XK_KP_Multiply + 26;
		}
/*
.........Standard function keys (F1-F35)
*/
		else if (keysym >= XK_F1 && keysym <= XK_F35)
		{
			irtn = 2;
			*event = keysym - XK_F1 + 42;
		}
/*
.........Alpha-numeric keys
*/
		else if (keysym > 0 && ibuf <= NKEYSYMS)
		{
			if (rebound > 0)
			{
				irtn = 1;
/*
......use keysym not buffer[0]
......it will have error, for example, when ^2, the buffer[0] = 0
......Yurong
				*event = buffer[0];
*/
				*event = keysym;
			}
		}
/*
.......Controlled Normal key
*/
		if (irtn == 1 && kevent->state & ControlMask)
		{
/*
......Added one more statement because we
......use keysym not buffer[0]
*/
			if (*event >= 97 && *event <= 122) *event = *event - 96;
			if (*event == 0) *event = 64;
			if (*event >= 32) *event = *event + NKEYSYMS;
		}
		else if (irtn == 1)
		{
			irtn = 0;
		}
/*
.....0,1..9 keypad, could be function or just numeric key
*/
		else if ((UW_keypad==0) && (irtn == 2)
				&& (*event>=26) && (*event<=41))
			irtn = 0;
/*
.......Controlled Function key
*/
		if (irtn == 2)
		{
			if (kevent->state & ControlMask)
				*event = *event + NFKEYSYMS * 2;
/*
.......Shifted Function key
*/
			else if (kevent->state & ShiftMask)
				*event = *event + NFKEYSYMS;
		}
		break;
/*
.......Mouse button
*/
	case ButtonPress:
		bevent = (XButtonPressedEvent *)x_event;
		irtn = 3;
		switch(bevent->button)
		{
		case Button1:
			irtn = 0;
			break;
		case Button2:
			if (x_event->xany.window == XtWindow(uw_mfprompt.prompt)) irtn = 0;
			else *event = 2;
		  	break;
		case Button3:
			*event = 3;
		}
		break;
/*
.......Client messages (DAS function)
.......Sent from Motif Menu Callback
*/
   case ClientMessage:
		irtn = 0;
		mevent = (XClientMessageEvent *)x_event;
		if (mevent->format==16)
		{
			if (mevent->data.s[0] == 1111)
			{
				*event = mevent->data.s[1];
				irtn = 2;
			}
		}
		else if (mevent->format==8)
		{
			uw_mfupd_cinput(mevent->data.b);
		}
		break;
/*
.......Unrecognized event
*/
	default:
/*
......added for button and dail
......Yurong 1/15/97
*/
#if UU_COMP == UU_IRIS4D 
#ifndef UU_RS6000
		if ((x_event->type==but_dialMotionNotify)||
				(x_event->type==but_dialPressNotify)||
				(x_event->type==but_dialReleaseNotify))
		{
			ir = uw_mfbut_dail(x_event, event);
			irtn = 4;
/*
.....Immediate function key
.....Go get another key
*/
			if (ir == -1)
			{
				irtn = expose_event;
			}
		}
		else
#endif
#endif
			irtn = 0;
		
	}
	return(irtn);
}
/*********************************************************************
**    E_FUNCTION : uw_mf_xflush_expose()
**      
**    DESCRIPTION:
**        Gets rid of any impeding expose events on the input queue.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mf_xflush_expose()
{       
	int i;
	XEvent x_event;
/*
.....Flush all expose events
*/
	i = XPending(uw_xw.disp);
	if (i > 0)
	{
		while(XCheckTypedEvent(uw_xw.disp,Expose,&x_event))
		{
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION : uw_mfbutton_event()
**      
**    DESCRIPTION:
**        Finds any Button events (Press/Release) on the X event que
**        and returns the first one if found.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          event			X-event which holds Button Press/Release event.
**
**    RETURNS      : UU_TRUE if button event is on input queue.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfbutton_event(event)
XEvent *event;
{       
	int i;
/*
.....Look ahead for Mouse button events
*/
	i = XPending(uw_xw.disp);
	if (i > 0)
	{
		if (XCheckTypedEvent(uw_xw.disp,ButtonPress,event))
			return(UU_TRUE);
		if (XCheckTypedEvent(uw_xw.disp,ButtonRelease,event))
			return(UU_TRUE);
	}
	return(UU_FALSE);
}

/*********************************************************************
**    I_FUNCTION     :  uw_xwbeep() --------->   UG_DBEEP
**
**       Sound a bell.
**
**    PARAMETERS
**       INPUT  :
**    prms
**       OUTPUT :
**             none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfbeep()
{
	XBell (uw_xw.disp, 0);
}

/*********************************************************************
**    I_FUNCTION : uw_mfget_cursor()
**
**			get the current X-Windows cursor 
**			
**    PARAMETERS   
**       INPUT  : 
**			           none
**       OUTPUT :  
**          cursor number
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfget_cursor()
{
	return curr_cursor;
}


/*********************************************************************
**    I_FUNCTION : uw_mfsetcursor(cursorno)
**
**	Set the X-Windows cursor as requested.
**			
**    PARAMETERS   
**       INPUT  : 
**          cursorno = 1            - Pick cursor.
**                     2            - Menu cursor.
**                     3,6,7        - Stroke cursor.
**                     4,5          - Locator cursor.
**                     21           - Blank cursor.
**			           40           - Pan cursor.
**			           41,42,44,45  - Rotate cursor.
**			           43           - Zoom cursor.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfsetcursor(cursorno)
int cursorno;
{
/*
.....move this variable to file level
.....because we want to now curr_cursor
.....in other routine.
.....Yurong 9/14/98
*/
/*
	static int curr_cursor = 99;
	Cursor cursor;
*/
	Window win;
/*
.....Get NCLIPV window handle
.....if picking is in NCLIPV window
*/
	uw_mfget_pockwin(&win);
/*
.....Change the cursor only when different
.....than current cursor
*/
	if (cursorno != 21) UW_XWcursor = cursorno;
	if (cursorno != curr_cursor )
	{
/*
.......Define the cursor type
*/
		switch (cursorno)
		{
/*
.........Pick cursor
*/
		case 0:
			if (UM_pocket_hwnd != UU_NULL) 
				XDefineCursor(uw_xw.disp, win, pick_cursor);
			else
				XDefineCursor(uw_xw.disp, uw_xw.wd_id, pick_cursor);
			break;
/*
.........Menu cursor
*/
		case 1:
			if (win != 0) 
				XDefineCursor(uw_xw.disp, win, menu_cursor);
			XDefineCursor(uw_xw.disp, uw_xw.wd_id, menu_cursor);
			break;
/*
.........Locator cursor
*/
		case 2:
		case 4:
		case 5:
			if (UM_pocket_hwnd != UU_NULL) 
				XDefineCursor(uw_xw.disp, win, loc_cursor);
			else
				XDefineCursor(uw_xw.disp, uw_xw.wd_id, loc_cursor);
			break;
/*
.........Stroke cursor
*/
		case 3:
		case 6:
		case 7:
			XDefineCursor(uw_xw.disp, uw_xw.wd_id, strok_cursor);
			break;
/*
.........Text cursor
*/
		case 22:
			XDefineCursor(uw_xw.disp, uw_xw.wd_id, text_cursor);
			break;
/*
.........PAN cursor
*/
		case 40:
			XDefineCursor(uw_xw.disp, uw_xw.wd_id, pan_cursor);
			break;
/*
.........ROTATE cursor
*/
		case 41:
		case 42:
		case 44:
		case 45:
			XDefineCursor(uw_xw.disp, uw_xw.wd_id, rotate_cursor);
			break;
/*
.........ZOOM cursor
*/
		case 43:
			XDefineCursor(uw_xw.disp, uw_xw.wd_id, zoom_cursor);
			break;
/*
.........Mouse cursor
*/
		case 46:
			if (UZ_nclipv_view == 1) 
				XDefineCursor(uw_xw.disp, win, mouse_cursor);
			XDefineCursor(uw_xw.disp, uw_xw.wd_id, mouse_cursor);
			break;
/*
.........Blank cursor
*/
		case 21:
		default:
			if (UM_pocket_hwnd != UU_NULL) 
				XDefineCursor(uw_xw.disp, win, blank_cursor);
			XDefineCursor(uw_xw.disp, uw_xw.wd_id, blank_cursor);
			uw_mfflush();
			break;
		}  /* end switch */
/*
.......Save the current cursor
*/
		curr_cursor = cursorno;
	}
}


void uw_mfbcomlin()
{
	XmTextSetInsertionPosition(uw_mfprompt.prompt, 0);
}

void uw_mfecomlin()
{
	char *text;
	int len;
	text = XmTextGetString(uw_mfprompt.prompt);
	if (text==NULL)
		return;
	len = strlen(text);
	XmTextSetInsertionPosition(uw_mfprompt.prompt, len);
	XtFree(text);
}

/*********************************************************************
**    E_FUNCTION : uw_mf_xflush_win()
**      
**    DESCRIPTION:
**        execute any impeding expose events on the input queue.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mf_xflush_win()
{       
	int i;
	XEvent x_event;
/*
.....Flush all expose and events
*/
	i = XPending(uw_xw.disp);
	if (i > 0)
	{
/*
		while(XCheckTypedEvent(uw_xw.disp,Expose,&x_event)||
				XCheckTypedEvent(uw_xw.disp,ConfigureNotify,&x_event))
*/
		while(XCheckTypedEvent(uw_xw.disp,Expose,&x_event))
		{
			XtDispatchEvent(&x_event);
		}
	}
	return;
}

void uw_mfshow_tooltip(x, y, tiptext)
int x, y;
char *tiptext;
{
	int n, ifl;
	Arg args[20];
	XmString str;
	char geom[20];
	int wid, hgt;
	static char tool_text[80] = "";
	static int tool_x=-1, tool_y=-1;

	x += UDM_layout.graphic_pos[0] + 10;
	y += UDM_layout.graphic_pos[1];

	if ((x<=0) && (y<=0)) return;
	if (strlen(tiptext)==0) return;

	if ((x==tool_x)&&(y==tool_y)&&(strcmp(tiptext, tool_text)==0))
		return;
	strcpy(tool_text, tiptext);
	tool_x = x;
	tool_y = y;
	wid = strlen(tiptext) * 8 + 8;
	hgt = 20;
	if (tip_popup!=NULL)
	{
		str = XmStringCreateSimple(tiptext);
		XtVaSetValues(tip_label, XmNlabelString, str, NULL);
		sprintf(geom,"%dx%d+%d+%d",wid, hgt, x,y);
		XtVaSetValues(tip_popup, XmNgeometry, geom,NULL);
		XtRealizeWidget(tip_popup);
		XtMapWidget(tip_popup);
		XRaiseWindow(XtDisplay(tip_popup),XtWindow(tip_popup));
		XmStringFree(str);
		tooltip_unmap = 0;
		return;
	}
	n = 0;
	ifl = MWM_DECOR_BORDER | MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE 
				| MWM_DECOR_MENU | MWM_DECOR_MINIMIZE 
				| MWM_DECOR_TITLE | MWM_DECOR_RESIZEH;
	XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
	sprintf(geom,"%dx%d+%d+%d",wid, hgt, x,y);
	XtSetArg(args[n],XmNgeometry,geom); n++;
	XtSetArg(args[n],XmNmappedWhenManaged,False); n++;
	XtSetArg(args[n],XmNallowResize,True); n++;
	tip_popup = XtAppCreateShell("Tooltip", "tooltip",
						topLevelShellWidgetClass,uw_xw.disp,
						args,n);
	if (tip_popup==NULL)
		return;
	n = 0;
	str = XmStringCreateSimple(tiptext);
	XtSetArg(args[n], XmNlabelString, str); n++;
	tip_label = XtCreateManagedWidget("Tooltip", xmLabelWidgetClass, tip_popup,
				args,n);
	XtRealizeWidget(tip_popup);
	XtMapWidget(tip_popup);
	XmStringFree(str);
	tooltip_unmap = 0;
}

void uw_mfclose_tooltip()
{
	if (tip_popup!=NULL)
	{
		if (tooltip_unmap==0)
		{
			tooltip_unmap = 1;
			XtUnmapWidget(tip_popup);
			XtUnrealizeWidget(tip_popup);
		}
	}
}

void uw_mfdelete_tooltip()
{
	uw_mfclose_tooltip();
	if (tip_popup!=NULL)
	{
		XtDestroyWidget(tip_popup);
	}
	tip_popup = NULL;
}

/**********************************************************************
**    I_FUNCTION :  uw_mfpost_msg(int msg)
**      Post a message to the graphic window
**
**    PARAMETERS   
**       INPUT  : 
**          msg: 1: left button down message
**						2: Mouse move message
**			
**       OUTPUT :  
**          pt: graphic window client position
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfpost_msg(msg)
int msg;
{
	int stat, xy[2];
	XButtonPressedEvent bevent;
	XMotionEvent mevent;
	int root_x,root_y;
	unsigned int keys_buttons;
	Window root,child;

	XQueryPointer(uw_xw.disp,uw_xw.wd_id,&root,&child,&root_x,&root_y,
				&xy[0],&xy[1],&keys_buttons);
	if (msg == 1)
	{
		bevent.type = ButtonPress;
		bevent.display = uw_xw.disp;
		bevent.window = uw_xw.wd_id;
		bevent.button = 1;
		bevent.x = xy[0];
		bevent.y = xy[1];
		stat = XSendEvent(uw_xw.disp,uw_xw.wd_id,True,0,(XEvent *)&bevent);
	}
	else if (msg==2)
	{
		mevent.type = MotionNotify;
		mevent.display = uw_xw.disp;
		mevent.window = XtWindow(uw_mf.graphic_app);
		mevent.x = xy[0];
		mevent.y = xy[1];
		WS_update_tip = 1;
		stat = XSendEvent(uw_xw.disp,uw_xw.wd_id,True,0,(XEvent *)&mevent);
	}
	return 0;
}
int uw_mfis_kbd_open()
{
	return Skbd_open;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfis_inputfunc(int num)
**       check if a function event number correspond to a 'by text', 'by location'
**			and 'by pick' function or selecting event input function
**
**    PARAMETERS   
**       INPUT  : 
**          num: event number
**			
**       OUTPUT :  
**          None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_mfis_inputfunc(event)
int event;
{
	UZ_keytable ktab;
	char *func;
	if (event<10000)
	{
		if (UU_application == UU_NCLNIS || UU_application == UU_NCLSIGNON)
			ktab = UZ_nisfkeytab[event-1];
		else if (UU_application == UU_NCLCAM) ktab = UZ_camfkeytab[event-1];
		else if (UU_application == UU_NCLCADD) ktab = UZ_cadfkeytab[event-1];
		else if (UU_application == UU_NCLIPV) ktab = UZ_ipvfkeytab[event-1];
		if ((ktab.type==NCLKEY) && (ktab.sub>=112) && (ktab.sub<=131))
			return 1;
		func = ktab.name;
	}
	else
	{
		func = UZ_daskey[event-10000];
	}
	if ((strcmp(func, "KEY_TEXT")==0) ||
		(strcmp(func, "KEY_LOCATE")==0) ||
		(strcmp(func, "KEY_PICK")==0))
		return 1;
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : ud_chkwin_event();
**			Check if there is a window event in the mesage queue
**    PARAMETERS
**       INPUT  :none
**
**       OUTPUT :none
**    RETURNS      : 0: no event in the queue
**					1: yes, there is a message in the queue
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_check_event()
{
	if (XPending(uw_xw.disp) == 0)		
		return 0;
	return 1;
}

#endif
#endif
