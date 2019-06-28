
#include "usysdef.h"
#if UU_COMP != UU_WIN2K

/********************************************************************* 
**  NAME:  wsglwin.c
**
**		CONTAINS:
**			uw_glopenwin()
**			uw_glresizeCB(widget,client_data,call_data)
**
**    MODULE NAME AND RELEASE LEVEL 
**       wsglwin.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:09
**
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:cursorfont.h>
#include <decw$include:Xm.h>
#include <decw$include:PushB.h>
#include <decw$include:PanedW.h>
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
#include <Xm/MwmUtil.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <GL/gl.h>
#include <GL/glx.h>
#include <GL/glu.h>
#endif
#include <X11/GLw/GLwMDrawA.h>
#include <X11/extensions/XInput.h>
#include <X11/extensions/XIproto.h>

#include "driver.h"
#include "ws.h"
#include "wsgl.h"
#include "wsxw.h"
#include "wsmf.h"
#include "dmark.h"
#include "dmotif.h"
#include "zkeysym.h"

#define DIAL_WARP 1
/*
.....variable for button and dial
*/
int but_dialMotionNotify, but_dialPressNotify, but_dialPressGrabNotify,
			but_dialReleaseNotify;
/*
.....Global variable definitions
*/
extern UWS_MF uw_mf;
extern XVisualInfo uw_glvinf;
extern int XBorder[2], YBorder[2], Border_stat;
static int last_dial[8] = {0,0,0,0,0,0,0,0};
static int last_bdevt;
static int dial_ret; /* value for return of uz_user_button */
/**********************************************************************
**    I_FUNCTION :  uw_glresizeCB(widget,client_data,call_data)
**       resize graphics.
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

static void uw_glresizeCB(widget,client_data,call_data)
Widget widget;
XtPointer client_data,call_data;
{
	uw_mfresize_graphics();
}
/*********************************************************************
**    I_FUNCTION :  uw_glopenwin() ----------  UG_DOPENWS
**       open graphic widget
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_glopenwin()
{
	Arg args[20];
	int n;
	int pos[2], size[2];
	char geom[20];
	void uw_mfgraphicCB();
	void uw_glresizeCB();
	n = 0;
/*
.....added for adjust pos and size
.....For SGI, we need adjust position
.....12/15/97 Yurong
*/
	pos[0] = UDM_layout.graphic_pos[0];
	pos[1] = UDM_layout.graphic_pos[1];
	size[0] = UDM_layout.graphic_size[0] - 2*XBorder[0];
	size[1] = UDM_layout.graphic_size[1] - 2*YBorder[0];
	if (Border_stat==0)
	{
		pos[0] += XBorder[0];
		pos[1] += YBorder[0];
	}
	sprintf(geom,"%dx%d+%d+%d",
		size[0], size[1], pos[0], pos[1]);
/*
	sprintf(geom,"%dx%d+%d+%d",
   	  UDM_layout.graphic_size[0],
      	UDM_layout.graphic_size[1],
      	UDM_layout.graphic_pos[0],
      	UDM_layout.graphic_pos[1]);
*/
   XtSetArg(args[n],XmNgeometry,geom); n++;
	XtSetArg(args[n], GLwNvisualInfo, &uw_glvinf); n++;	
   XtSetArg(args[n],XmNbackground,BlackPixel(uw_xw.disp,uw_xw.screen_no));
   n++; 
	
	uw_mf.graphic = XtCreateWidget("graphics",
			glwMDrawingAreaWidgetClass,uw_mf.graphic_app, args,n);
	if (uw_mf.graphic == NULL)
	{
		printf("Could not create Graphics Window.\n");
		exit();
	}
	
	XtAddCallback(uw_mf.graphic,XmNresizeCallback,uw_mfgraphicCB,NULL);
	XtAddCallback(uw_mf.graphic,XmNexposeCallback,uw_mfgraphicCB,NULL);
	XtAddCallback(uw_mf.graphic,XmNinputCallback,uw_mfgraphicCB,NULL);

/*
.....add dials&buttons
.....Yurong 12/1/97
*/
#if UU_COMP == UU_IRIS4D 
#ifndef UU_RS6000
	uw_glinit_dialbutn();
#endif
#endif
}

/*********************************************************************
**    I_FUNCTION :  uw_glget_gwsize(x,y)
**       Return the graphics window size in pixels.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          x     = Size of graphics window in X.
**          y     = Size of graphics window in Y.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_glget_gwsize(x,y)
int *x,*y;
{
	*x = uw_gl.xpixels;
	*y = uw_gl.ypixels;
}

#if UU_COMP == UU_IRIS4D 
#ifndef UU_RS6000
int uw_mfbut_dail(x_event, event_num)
XEvent *x_event;
int *event_num;
{
	Boolean uw_gldispXinput();
	int ir;
	uw_gldispXinput(x_event);
	ir = dial_ret; 
	*event_num = last_bdevt;
	return ir;
}

Boolean uw_gldispXinput(XEvent *event)
{
	Boolean status;
	Widget w;
	int button, dial, et_num;
	int data;
	char* index;
	XAnyEvent *xany = (XAnyEvent *)event;
/*
.....reset dial_ret
*/
	dial_ret = 0;
	if (but_dialMotionNotify&&event->type==but_dialMotionNotify)
	{
		XDeviceMotionEvent *devmot = (XDeviceMotionEvent*)event;
		dial = devmot->first_axis;
		data = devmot->axis_data[0]; 
		if (last_dial[dial]-data>= DIAL_WARP)
		{
			et_num = 33 + (dial*2);
			dial_ret = uz_user_button(et_num,&index,0);
		}
		else if (data-last_dial[dial] >= DIAL_WARP)
		{
			et_num = 33 + (dial*2)+1;
			dial_ret = uz_user_button(et_num,&index,1);
		}
		last_dial[dial] = data;	
		last_bdevt = et_num;
	}
	else if (but_dialPressNotify&&event->type==but_dialPressNotify)
	{
		XDeviceButtonEvent *devbtn = (XDeviceButtonEvent*)event;
		button = devbtn->button;
/*
.....call button function here
*/
		dial_ret = uz_user_button(button,&index,1);
		last_bdevt = button;
	}
	return 1;
}

static void selectXInputEvents(Widget w, int *event_types,
	XtPointer *select_data, int count, XtPointer client_data)
{
	XEventClass *xcp = (XEventClass*) select_data;
	XSelectExtensionEvent(XtDisplay(w), XtWindow(w), xcp, count);
}

uw_glinit_dialbutn()
{
	XDevice *dialDevice;
	XExtensionVersion *version;
	XDeviceInfoPtr deviceInfo, device;
	XAnyClassPtr any;
	XButtonInfoPtr b;
	XValuatorInfoPtr v;
	XAxisInfoPtr a;
	int numdev, numbut, numdial;
	int i,j,k;
	int eventBase, opcode, errorBase;
	int *dialsResolution;
	Bool exists;
	void uw_mfgraphicCB(), selectXInputEvents();
	Boolean uw_gldispXinput();
	XEventClass but_dialMotionClass, but_dialPressClass, but_dialPressGrabClass,
					but_dialReleaseClass;

	exists = XQueryExtension(uw_xw.disp, "XInputExtension", &opcode,
					 &eventBase, &errorBase);
	if (!exists) goto noDevice;
	version = XGetExtensionVersion(uw_xw.disp, "XInputExtension");
	if (version!=NULL&&((int)version)!=NoSuchExtension)
	{
		XFree(version);
		deviceInfo = XListInputDevices(uw_xw.disp, &numdev);
		if (deviceInfo)
		{
			for (i=0; i<numdev; i++)
			{
				device = &deviceInfo[i];
				any = (XAnyClassPtr) device->inputclassinfo;
				if (!strcmp(device->name, "dial+buttons"))
				{
					v = NULL;
					b = NULL;
					for (j=0; j<device->num_classes; j++)
					{
						switch (any->class)
						{
							case ButtonClass:
								b = (XButtonInfoPtr) any;
								numbut = b->num_buttons;
								break;
							case ValuatorClass:
/*
.....we don't use value, but just leave there in case we want it
*/
								v = (XValuatorInfoPtr) any;
								numdial = v->num_axes;
								dialsResolution = (int*) malloc(sizeof(int)*numdial);
								a = (XAxisInfoPtr) ((char*)v+sizeof(XValuatorInfo));
								for (k=0; k<numdial; k++, a++)
								{
									dialsResolution[k] = a->resolution;
								}
						}
						any = (XAnyClassPtr) ((char*)any + any->length);
					}
					dialDevice = XOpenDevice(uw_xw.disp, device->id);
					if (dialDevice)
					{
						DeviceMotionNotify(dialDevice, but_dialMotionNotify,
														but_dialMotionClass);
						DeviceButtonPress(dialDevice, but_dialPressNotify,	
														but_dialPressClass);
						DeviceButtonPressGrab(dialDevice, but_dialPressGrabNotify,	
														but_dialPressGrabClass);
						DeviceButtonRelease(dialDevice, but_dialReleaseNotify,	
														but_dialReleaseClass);
						XtRegisterExtensionSelector(uw_xw.disp, eventBase,
							eventBase+IEVENTS-1, selectXInputEvents, NULL);
						XtSetEventDispatcher(uw_xw.disp, but_dialPressNotify,
														uw_gldispXinput);
						XtSetEventDispatcher(uw_xw.disp, but_dialPressGrabNotify,
														uw_gldispXinput);
						XtSetEventDispatcher(uw_xw.disp, but_dialReleaseNotify,
														uw_gldispXinput);
						XtSetEventDispatcher(uw_xw.disp, but_dialMotionNotify,
														uw_gldispXinput);
						XtInsertEventTypeHandler(uw_mf.graphic, but_dialMotionNotify,
							(XtPointer)but_dialMotionClass,
							uw_mfgraphicCB, NULL, XtListTail);
						XtInsertEventTypeHandler(uw_mf.graphic, but_dialPressNotify,
							(XtPointer)but_dialPressClass,
							uw_mfgraphicCB, NULL, XtListTail);
						XtInsertEventTypeHandler(uw_mf.graphic, 
							but_dialPressGrabNotify,(XtPointer)but_dialPressGrabClass,
							uw_mfgraphicCB, NULL, XtListTail);
						XtInsertEventTypeHandler(uw_mf.graphic, but_dialReleaseNotify,
							(XtPointer)but_dialReleaseClass,
							uw_mfgraphicCB, NULL, XtListTail);
						XFreeDeviceList(deviceInfo);
						return;
					}
				}
			}
		}
	}
noDevice: ;
}		
						 				
#endif
#endif
#endif
