#include "usysdef.h"
#if UU_COMP != UU_WIN2K

/********************************************************************* 
**  NAME:  wsmfapp.c
**
**              CONTAINS:
**                      uw_mfpocket_window
**                      uw_mfclose_pocket
**                      uw_mfpocket_reset
**                      uw_mfset_context
**                      uw_mfget_context
**                      uw_mfget_pockwin
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       wsmfpocket.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:12
**
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xm.h>
#include <decw$include:DialogS.h> 
#include <decw$include:DrawingA.h> 
#else
#include <X11/Xlib.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h> 
#include <Xm/DialogS.h> 
#include <Xm/MwmUtil.h> 
#endif
#include "driver.h"
#include "mpocket.h"
#include "ws.h"
#include "wsxw.h"
#include "wsmf.h"
#ifdef UU_OPENGL
#include "wsgl.h"
#include <X11/GLw/GLwMDrawA.h>
extern GLXContext glx_context;
extern XVisualInfo uw_glvinf;
#endif
#include "dmotif.h"
#include "mxxx.h"
#include "uims.h"
#include "view.h"
/*
.....Global variable definitions
*/
extern UWS_MF uw_mf;
extern int XBorder[2],YBorder[2];
extern int Border_stat;
extern int *UD_ksws;
/*
.....Static variables
*/
static Widget pocket_app=UU_NULL;
static Window pocket_window;
/*
......use global value UW_pocket_size which will be initialize in
......initial routine for NCL
......Yurong 8/30/05
*/
/*
static int pocket_size[2]={0,0},pocket_pos[2]={0,0};
*/
extern int UW_pocket_size[2],UW_pocket_pos[2];
static struct UM_drawing_rec pocket_drawing;
static UM_pkwin_type UM_current_type = UM_DRAWING_WINDOW;
static UM_pkwin_type LastContext = UM_DRAWING_WINDOW;
static Window nclipv_window=0;
Widget nclipv_graphic = 0;

#ifdef UU_IPV
#include "lipv.h"
#include "li/lidrvxlb.h"
static Widget nclipv_app=UU_NULL;
extern char LW_ipv_title[256];
extern int LW_ipv_pos[2],LW_ipv_size[2];
static GLXContext nclipv_context = UU_NULL;
extern int PKx,PKy;
extern int LW_active;
#endif

int UW_pocket_mode=UU_FALSE;
/**********************************************************************
**    I_FUNCTION :  pocket_resizeCB(widget,client_data, event)
**      Get size and position when graphic, status, and prompt area
**                       move or sized and save in the layout structure 
**    PARAMETERS   
**       INPUT  : 
**          event:      Xevent.
**                              client_data = Contains program information about moved
**                                            window.
**                   1:   Graphic area
**                                                      2:   Prompt area
**                                                      3.   Status area
**                              widget:    widget that event happened.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void pocket_resizeCB (widget,
	client_data,
	event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	Position x, y;
	Dimension width, height;
	char cur_screen[20];
	XConfigureEvent *cevent = (XConfigureEvent *)event;
	UM_pkwin_type type = (UM_pkwin_type)client_data;
	Window pocket_window_save;
/*
.....Drawing Window
*/
	if (type == UM_DRAWING_WINDOW)
	{
/*
.....Save the current screen
*/
		uv_current_screen_name(cur_screen);
/*
...get size and position
*/
		if (cevent->type == ConfigureNotify)
		{
			XtVaGetValues(widget, XmNx, &x, XmNy, &y, NULL);
			XtVaGetValues(widget, XmNwidth, &width, XmNheight, &height, NULL); 
			if (Border_stat == 1)
			{
				UW_pocket_pos[0] = x - XBorder[1];
				UW_pocket_pos[1] = y - YBorder[1];
			}
			else
			{
				UW_pocket_pos[0] = x;
				UW_pocket_pos[1] = y;
			}
			UW_pocket_size[0] = width;
			UW_pocket_size[1] = height;
		}
		else
		{
			width = UW_pocket_size[0];
			height = UW_pocket_size[1];
		}
/*
.....Set new screen size to Pocket Window
*/
		udm_resize_graphics(&UD_duimsdeflt,width,height,0);
		gswswindow(UD_ksws,&(UD_duimsdeflt.screen[0].wswind));
		pocket_window_save = uw_xw.wd_id;
		uw_xw.wd_id = pocket_window;
		glXMakeCurrent( uw_xw.disp , uw_xw.wd_id, glx_context );
/*
.....Display the drawing
*/
		um_set_pocket_graphics(UM_DRAWING_WINDOW);
		gclearws(UD_ksws,1);
		um_view_pocket_drawing(&pocket_drawing);
/*
.....Reset the graphic window size
*/
		uw_xw.wd_id = pocket_window_save;
		glXMakeCurrent( uw_xw.disp , uw_xw.wd_id, glx_context );
		XtVaGetValues(uw_mf.graphic,XmNwidth,&width,XmNheight,&height,NULL);
		udm_resize_graphics(&UD_duimsdeflt,width,height,0);
		gswswindow(UD_ksws,&(UD_duimsdeflt.screen[0].wswind));
		uv_chgsc(cur_screen);
		um_reset_pocket_graphics(UM_DRAWING_WINDOW);
	}
/*
.....NCLIPV Window
*/
#ifdef UU_IPV
	else if (type == UM_IPV_WINDOW)
	{
/*
........Get size and position
*/
		if (cevent->type == ConfigureNotify)
		{
			XtVaGetValues(widget, XmNx, &x, XmNy, &y, NULL);
			XtVaGetValues(widget, XmNwidth, &width, XmNheight, &height, NULL); 
			if (Border_stat == 1)
			{
				LW_ipv_pos[0] = x - XBorder[1];
				LW_ipv_pos[1] = y - YBorder[1];
			}
			else
			{
				LW_ipv_pos[0] = x;
				LW_ipv_pos[1] = y;
			}
			LW_ipv_size[0] = width;
			LW_ipv_size[1] = height;
			XResizeWindow(uw_xw.disp,nclipv_window,width,height);
		}
		else
		{
			width = LW_ipv_size[0];
			height = LW_ipv_size[1];
		}
		PKx = width;
		PKy = height;
/*
.....Let IPV know the window resized
*/
		if (LW_active == 1)
		{
			ipv_resize_window();
		}
	}
#endif
}

/**********************************************************************
**    I_FUNCTION :  uw_mfpocket_window(drawing,type)
**       Creates a Pocket Graphics Window.
**    PARAMETERS   
**       INPUT  : 
**          drawing     Drawing to display in pocket window.
**          type        UM_DRAWING_WINDOW or UM_IPV_WINDOW.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_mfpocket_window(title, draw_rec,type)
char *title;
int *draw_rec;
UM_pkwin_type type;
{
	int n;
	Arg args[20];
	char geom[20];
	int retstat;
	Widget pocket_graphic;
	char *temp, pic_file[256];
	struct UM_drawing_rec *drawing;
	if (type==UM_DRAWING_WINDOW)
		drawing = (struct UM_drawing_rec *)draw_rec;
	else
	{
		temp = (char *)draw_rec;
		strcpy(pic_file, temp);
	}
/*
.....Initalize routine
*/
	n = 0;
	UM_current_type = type;
	if (type == UM_DRAWING_WINDOW)
		uu_move_byte(drawing,&pocket_drawing,sizeof(struct UM_drawing_rec));
/*
.....Take down previous pocket window
*/
	uw_mfclose_pocket(type);
/*
.....Set the window location & size
*/
	if (type == UM_DRAWING_WINDOW)
	{
		if (UW_pocket_pos[0] != 0)
		{
			sprintf(geom,"%dx%d+%d+%d",
				UW_pocket_size[0], UW_pocket_size[1], UW_pocket_pos[0], UW_pocket_pos[1]);
			XtSetArg(args[n],XmNgeometry,geom); n++;
		}
/*
.....Create the Graphics area
*/
		XtSetArg(args[n],XmNtitle,drawing->name); n++;
		pocket_app = XtCreatePopupShell(/*drawing->name*/"pocket_view",
			xmDialogShellWidgetClass,
			uw_mf.graphic_app,
			args,n);
		n = 0;
		XtSetArg(args[n],XmNbackground,BlackPixel(uw_xw.disp,uw_xw.screen_no)); n++;
		if (UW_pocket_size[0] != 0)
		{
			XtSetArg(args[n],XmNwidth,UW_pocket_size[0]); n++;
			XtSetArg(args[n],XmNheight,UW_pocket_size[1]); n++;
		}
/*
.....Open openGL own graphic Widget
*/
/*
.....Make sure pocket window uses same visual
.....as main graphics window
*/
		XtSetArg(args[n],GLwNvisualInfo,&uw_glvinf); n++;
		pocket_graphic = XtCreateWidget("pocket_graphics",
			glwMDrawingAreaWidgetClass,pocket_app,args,n);
		if (pocket_graphic == NULL) goto failed;
/*
.....Add the Graphics callbacks
*/
		XtAddCallback(pocket_graphic,XmNexposeCallback,
			(XtCallbackProc)pocket_resizeCB,UM_DRAWING_WINDOW);
/*
.....Add the eventhandler to save the
.....size and position when they change
*/
		XtAddEventHandler(pocket_app, StructureNotifyMask, False,
			(XtEventHandler)pocket_resizeCB, UM_DRAWING_WINDOW);
/*
.....Manage the Pocket Graphics area
*/
		XtManageChild(pocket_graphic);
		XtRealizeWidget(pocket_app);
		pocket_window = XtWindow(pocket_graphic);
/*
.....Set new screen size to Pocket Window
*/
		udm_resize_graphics(&UD_duimsdeflt,UW_pocket_size[0],UW_pocket_size[1],0);
		gswswindow(UD_ksws,&(UD_duimsdeflt.screen[0].wswind));
/*
.....Make the pocket window active
*/
		UW_pocket_mode = UU_TRUE;
	}
/*
.....Open NCLIPV Window
*/
#ifdef UU_IPV
	else
	{
		static XVisualInfo visual;
/*
.....Let lightworks know the display we're on
*/
/*		LiDataSetGenericPtr(&data,(LtGenericPtr)uw_xw.disp);
		LiControlSet(LI_CONTROL_DRVXL_DISPLAY_PTR,&data);*/
		if (nclipv_context == UU_NULL)
		{
			retstat = LiOGLDrvDisplaySet(uw_xw.disp);
			if (retstat != 0) goto failed;
		}
		if (LW_ipv_pos[0] != 0)
		{
			sprintf(geom,"%dx%d+%d+%d",
				LW_ipv_size[0], LW_ipv_size[1], LW_ipv_pos[0], LW_ipv_pos[1]);
			XtSetArg(args[n],XmNgeometry,geom); n++;
		}
/*
.....Create the Graphics area
*/
		XtSetArg(args[n],XmNtitle,LW_ipv_title); n++;
		nclipv_app = XtCreatePopupShell(/*drawing->name*/"nclipv",
			xmDialogShellWidgetClass,
			uw_mf.graphic_app,
			args,n);
		if (nclipv_app == UU_NULL) goto failed;
		n = 0;
		XtSetArg(args[n],XmNbackground,BlackPixel(uw_xw.disp,uw_xw.screen_no)); n++;
		if (LW_ipv_size[0] != 0)
		{
			XtSetArg(args[n],XmNwidth,LW_ipv_size[0]); n++;
			XtSetArg(args[n],XmNheight,LW_ipv_size[1]); n++;
		}
/*
.....Create the NCLIPV visual
*/
		if (nclipv_context == UU_NULL)
		{
			if (LiOGLDrvGetXVisualInfo(&visual) != LI_STATUS_OK) goto failed;
			nclipv_context = glXCreateContext(uw_xw.disp,&visual,NULL,GL_TRUE);
			if (nclipv_context == UU_NULL) goto failed;
		}
/*
.....Open openGL own graphic Widget
*/
		XtSetArg(args[n],GLwNvisualInfo,&visual); n++;
		nclipv_graphic = XtCreateWidget("nclipv_graphics",
			glwMDrawingAreaWidgetClass,nclipv_app,args,n);
		if (nclipv_graphic == NULL) goto failed;
/*
.....Add the Graphics callbacks
*/
		XtAddCallback(nclipv_graphic,XmNexposeCallback,
			(XtCallbackProc)pocket_resizeCB,(XtPointer)UM_IPV_WINDOW);
/*
.....Add the eventhandler to save the
.....size and position when they change
*/
		XtAddEventHandler(nclipv_app, StructureNotifyMask, False,
			(XtEventHandler)pocket_resizeCB, (XtPointer)UM_IPV_WINDOW);
/*
.....Manage the Pocket Graphics area
*/
		XtManageChild(nclipv_graphic);
		XtRealizeWidget(nclipv_app);
		nclipv_window = XtWindow(nclipv_graphic);
/*		nclipv_window = LiOGLDrvChildWindowCreate(input_window,0,0,0,0);*/
		if (nclipv_window == 0) goto failed;
	}
#endif
/*
.....Return success
*/
	retstat = UU_SUCCESS;
	goto done;
/*
.....Could not open Pocket Window
*/
failed:;
	retstat = UU_FAILURE;
/*
....End of routine
*/
done:;
	uw_mf_xflush_expose();
	return(retstat);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfclose_pocket(type)
**       Closes a Pocket Graphics Window.
**    PARAMETERS   
**       INPUT  : 
**          type        UM_DRAWING_WINDOW or UM_IPV_WINDOW.
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_mfclose_pocket(type)
UM_pkwin_type type;
{
	int status,i;
	UV_vport vport;
/*
.....Close the drawing pocket window
*/
	if (type == UM_DRAWING_WINDOW)
	{
		if (pocket_app != UU_NULL && XtIsRealized(pocket_app))
		{
			XtUnrealizeWidget(pocket_app);
			XtDestroyWidget(pocket_app);
		}
/*
.....Do not redraw main graphics
*/
		ug_setredrwflag(0);
/*
.....Redraw all viewports (not contents though)
.....Because the segments were altered when the
.....screen changed
*/
		if (UW_pocket_mode)
		{
			for (i=0;i<UV_act_screen[0].nvports; i++)
			{
				status = uv_getvpid(UV_act_screen[0].vports[i],&vport);
				uv_drawvp(&vport);
			}
		}
		UW_pocket_mode = UU_FALSE;
		pocket_app = UU_NULL;
	}
/*
.....Close the NCLIPV Window
*/
#ifdef UU_IPV
	else
	{
/*		LiOGLDrvChildWindowDestroy(nclipv_window);*/
		if (nclipv_app != UU_NULL && XtIsRealized(nclipv_app))
		{
			XtUnrealizeWidget(nclipv_app);
			XtDestroyWidget(nclipv_app);
		}
		nclipv_window = UU_NULL;
		nclipv_app = UU_NULL;
	}
#endif
	status = UU_SUCCESS;
	return(status);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfset_context(type)
**       Sets the correct graphics context for the requested window.
**    PARAMETERS   
**       INPUT  : 
**          which     = UM_DRAWING_WINDOW - Ignored for now.
**                      UM_IPV_WINDOW - NCLIPV window.
**                      UM_NCL_WINDOW - NCL main window.
**
**          force     = UU_TRUE - Set context even if it was last one set.
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfset_context(which,force)
UM_pkwin_type which;
UU_LOGICAL force;
{
/*
.....Only change context if different
*/
	if (which != LastContext || force)
	{
/*
.....NCL window
*/
		if (which == UM_NCL_WINDOW)
		{
		if ((uw_xw.wd_id!=0))
			glXMakeCurrent(uw_xw.disp,uw_xw.wd_id,glx_context);
		}
		else if (which == UM_IPV_WINDOW)
		{
			ul_ipv_set_context();
		}
		else if (which == UM_DRAWING_WINDOW)
		{
/*
.....why call IPV context setup?
.....remove and set the context to pocket window
.....Yurong 8/30/05             
*/
			if ((pocket_window!=0))
				glXMakeCurrent(XtDisplay(pocket_app),pocket_window, glx_context);
		}
	}
/*
.....Save this context setting
*/
	LastContext = which;
}

/**********************************************************************
**    I_FUNCTION :  uw_mfget_context(type)
**       Sets the correct graphics context for the requested window.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      :
**          UM_DRAWING_WINDOW - Ignored for now.
**          UM_IPV_WINDOW - NCLIPV window.
**          UM_NCL_WINDOW - NCL main window.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UM_pkwin_type uw_mfget_context()
{
/*
.....Return the last Graphics Context
*/
	return(LastContext);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfget_pockwin(win)
**       Returns the window ID of the NCLIPV pocket window.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          win       = Window ID of NCLIPV pocket window.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfget_pockwin(win)
char **win;
{
		*win = (char *)nclipv_window;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfget_mainwin(win)
**       Returns the window ID of the NCL main window.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          win       = Window ID of NCL main window.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfget_mainwin(win)
char **win;
{
	*win = (char *)uw_xw.wd_id;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfget_ipvsize(int *wid, int *hgt)
**       Get the NCLIPV window size
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          wid, hgt: size of the NCLIPV window
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfget_ipvsize(wid, hgt)
int *wid, *hgt;
{
	Dimension width, height;
	if (nclipv_window!=NULL)
	{
		XtVaGetValues(nclipv_graphic,XmNwidth,&width,XmNheight,&height,NULL);
		*wid = width;
		*hgt = height;
	}
	else
	{
		*wid = *hgt = 0;
	}
}
Widget uw_mfget_pkwin()
{
	return pocket_app;
}

#endif
