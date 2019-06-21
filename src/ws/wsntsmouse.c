#include "usysdef.h"
#if UU_COMP == UU_WIN2K

/*********************************************************************
**    NAME         :  wsntsmouse.c
**       CONTAINS: SpaceMouse function on Windows NT 
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsntsmouse.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:14
*********************************************************************/
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <windows.h>
#include <windowsx.h>
#include <winnt.h>
#include <winuser.h>

#include "wsntsmouse.h"
#include "spmouse.h"

static MagellanControl MagellanControlInfoDLL;

extern uw_ntflush_spmouse();

/*********************************************************************
**	 I_FUNCTION : MagellanSetWindow
**		This function set the window handler into SpaceMouse driver
**		as the current application window.
**		This function is copying Magellan Driver routine
**	 PARAMETERS	
**		 INPUT  :
**					MagellanHandle: SpaceMouse handler
**					
**					ApplicationWindow: currect application window handler
**		 OUTPUT :
**	 RETURNS: 0 = failed
**	          
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int WINAPI MagellanSetWindow( HMAGELLAN MagellanHandle, HWND ApplicationWindow )
{
	if ( MagellanHandle == NULL )
		return FALSE;
	else
	{
		MagellanHandle->MagellanWindow =	ApplicationWindow;
		return MagellanInfoWindow( MagellanHandle, (WPARAM) MagellanSetWindowCommand, (LPARAM) ApplicationWindow );
	};
}

/*********************************************************************
**	 I_FUNCTION : MagellanInfoWindow
**		This function coomands the SpaceMouse driver to execute several different functions.
**		This function is copying Magellan Driver routine
**	 PARAMETERS	
**		 INPUT  :
**					MagellanHandle: SpaceMouse handler
**					MagellanInfoType: command type
**					MagellanInfo: command info
**		 OUTPUT :
**	 RETURNS: 0 = failed
**	          
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int WINAPI MagellanInfoWindow( HMAGELLAN MagellanHandle, WPARAM MagellanInfoType, LPARAM MagellanInfo )
{
	HWND MagellanDriverWindow;
	int mode = 0x01  + 0x02 + 0x04;

	if ( MagellanHandle == NULL )
		return FALSE;
	else
	{
		MagellanDriverWindow = FindWindow( MagellanClassName, NULL );
		if ( MagellanDriverWindow == NULL )
			return FALSE;
		PostMessage( MagellanDriverWindow, MagellanHandle->MagellanCommandEvent, 
			(WPARAM) MagellanInfoType, (LPARAM) MagellanInfo );    
		return TRUE;
	};
	return FALSE;
}

/*********************************************************************
**	 I_FUNCTION : MagellanInit
**		This function initialize spacemouse driver
**		This function is copying Magellan Driver routine
**	 PARAMETERS	
**		 INPUT  :
**					MagellanHandle: SpaceMouse handler
**					MagellanInfoType: command type
**					MagellanInfo: command info
**		 OUTPUT :
**	 RETURNS: 0 = failed
**	          
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
HMAGELLAN WINAPI MagellanInit( HWND ApplicationWindow )
{
	memset( &MagellanControlInfoDLL, 0x00, sizeof( MagellanControlInfoDLL ) );

	MagellanControlInfoDLL.MagellanDriverWindow = FindWindow( MagellanClassName, NULL );
	if ( MagellanControlInfoDLL.MagellanDriverWindow == NULL )
		return NULL; 

	MagellanControlInfoDLL.MagellanWindow = ApplicationWindow;
 
	MagellanControlInfoDLL.MagellanMotionEvent = RegisterWindowMessage("MotionEvent");
	MagellanControlInfoDLL.MagellanButtonPressEvent = RegisterWindowMessage("ButtonPressEvent");
	MagellanControlInfoDLL.MagellanButtonReleaseEvent = RegisterWindowMessage("ButtonReleaseEvent");
	MagellanControlInfoDLL.MagellanCommandEvent = RegisterWindowMessage("CommandEvent");
	MagellanControlInfoDLL.MagellanEvent.MagellanPeriod = 1;
	if ( (MagellanControlInfoDLL.MagellanMotionEvent==0) || 
		(MagellanControlInfoDLL.MagellanButtonPressEvent==0) ||
		(MagellanControlInfoDLL.MagellanButtonReleaseEvent==0) ||
		(MagellanControlInfoDLL.MagellanCommandEvent==0) )
		return NULL;	

	if ( MagellanSetWindow( &MagellanControlInfoDLL, ApplicationWindow ) )
		return &MagellanControlInfoDLL;
	else
		return NULL;
}

/*********************************************************************
**	 I_FUNCTION : MagellanClose
**		This function close the interface to spacemouse driver
**		This function is copying Magellan Driver routine
**	 PARAMETERS	
**		 INPUT  :
**					MagellanHandle: SpaceMouse handler
**		 OUTPUT :
**	 RETURNS: 0 = failed
**	          
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int WINAPI MagellanClose( HMAGELLAN MagellanHandle )
{
	if ( MagellanHandle == NULL )
		return FALSE;
 
	MagellanSetWindow( MagellanHandle, MagellanHandle->MagellanDriverWindow );
	memset( MagellanHandle, 0x00, sizeof( *MagellanHandle ) );
	return TRUE;
}

/*********************************************************************
**	 I_FUNCTION : MagellanTranslateEvent
**		This function decode incoming event message from Spacemouse
**		for NCL
**	 PARAMETERS	
**		 INPUT  :
**					MagellanHandle: SpaceMouse handler
**					Message: incoming message
**		 OUTPUT :
**					MagellanEvent: event structure
**					event: event number for NCL
**	 RETURNS: 0 = not spacemouse event
**	          
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int WINAPI MagellanTranslateEvent( HMAGELLAN MagellanHandle, LPMSG Message,
		 		  				   MagellanIntegerEvent *MagellanEvent, int *event )
{			    
	MagellanCompress MagellanData;
	int offset_x, offset_y, offset_z, offset_a, offset_b, offset_c; 
	static int last_event = 33;
	static max_x=0, max_y = 0, max_z = 0,max_a=0, max_b = 0, max_c = 0;

	if ( MagellanHandle == NULL )
		return FALSE;

	if ( Message->message == MagellanHandle->MagellanMotionEvent )
	{
		MagellanEvent->MagellanWindow = Message->hwnd;

		MagellanData.MagellanDWord = Message->wParam;
		MagellanEvent->MagellanData[ MagellanX ] = MagellanData.MagellanValues.ValueAX;
		MagellanEvent->MagellanData[ MagellanY ] = MagellanData.MagellanValues.ValueBY;
		MagellanEvent->MagellanData[ MagellanZ ] = MagellanData.MagellanValues.ValueCZ;

		MagellanData.MagellanDWord = Message->lParam;
		MagellanEvent->MagellanData[ MagellanA ] = -MagellanData.MagellanValues.ValueAX;
		MagellanEvent->MagellanData[ MagellanB ] = -MagellanData.MagellanValues.ValueBY;
		MagellanEvent->MagellanData[ MagellanC ] = -MagellanData.MagellanValues.ValueCZ;

		if (UV_SM_dom==1)
/*
......only save one direction data (largest offset)
*/
		{
			offset_x = MagellanEvent->MagellanData[ MagellanX ];
			offset_y = MagellanEvent->MagellanData[ MagellanY ];
			offset_z = MagellanEvent->MagellanData[ MagellanZ ];
			offset_a = MagellanEvent->MagellanData[ MagellanA ];
			offset_b = MagellanEvent->MagellanData[ MagellanB ];
			offset_c = MagellanEvent->MagellanData[ MagellanC ];

			if (UV_SM_pan==0)
/*
......disable pan, so ignore x,y,z offset
*/
			{
				offset_x = offset_y = offset_z = 0;
			}
			if (UV_SM_rotate==0)
/*
......disable rotate, so ignore a,b,c offset
*/
			{
				offset_a = offset_b = offset_c = 0;
			}

/*
......no offset on all direction, using the last event number
*/
			if ((offset_a==0)&&(offset_b==0)&&(offset_c==0)
				&&(offset_x==0)&&(offset_y==0)&&(offset_z==0))
			{
				return FALSE;
			}
			if ( (abs(offset_x)>=abs(offset_y)) && (abs(offset_x)>=abs(offset_z)) && 
				(abs(offset_x)>=abs(offset_a)) && (abs(offset_x)>=abs(offset_b))
				&& (abs(offset_x)>=abs(offset_c)) )
			{
/*
......dial_1_left NCL_XPAN_LEFT
*/
				if (offset_x < 0) 
					*event = 33;
/*
......dial_1_right NCL_XPAN_RIGHT
*/
				else
					*event = 34;

				UV_actsm_dial = 1;
			}
			else if ( (abs(offset_y)>=abs(offset_x)) && (abs(offset_y)>=abs(offset_z)) && 
				(abs(offset_y)>=abs(offset_a)) && (abs(offset_y)>=abs(offset_b))
				&& (abs(offset_y)>=abs(offset_c)) )
			{
/*
.....dial_2_left NCL_ZOOM_DOWN
*/
				if (offset_y < 0) 
					*event = 37;
/*
.....dial_2_right NCL_ZOOM_UP
*/
				else
					*event = 38;
				UV_actsm_dial = 2;
			}
			else if ( (abs(offset_z)>=abs(offset_x)) && (abs(offset_z)>=abs(offset_y)) && 
				(abs(offset_z)>=abs(offset_a)) && (abs(offset_z)>=abs(offset_b))
				&& (abs(offset_z)>=abs(offset_c)) )
			{
/*
.....dial_3_left
*/
				if (offset_z < 0) 
					*event = 35;
/*
.....dial_3_right
*/
				else
					*event = 36;
				UV_actsm_dial = 3;
			}
			else if ( (abs(offset_a)>=abs(offset_x)) && (abs(offset_a)>=abs(offset_y)) && 
				(abs(offset_a)>=abs(offset_z)) && (abs(offset_a)>=abs(offset_b))
				&& (abs(offset_a)>=abs(offset_c)) )
			{
/*
.....dial_4_left
*/
				if (offset_a < 0) 
					*event = 41;
/*
.....dial_4_right
*/
				else
					*event = 42;
				UV_actsm_dial = 4;
	
			}
			else if ( (abs(offset_b)>=abs(offset_x)) && (abs(offset_b)>=abs(offset_y)) && 
				(abs(offset_b)>=abs(offset_z)) && (abs(offset_b)>=abs(offset_a))
				&& (abs(offset_b)>=abs(offset_c)) )
			{
/*
.....dial_5_left
*/
				if (offset_b < 0) 
					*event = 39;
/*
.....dial_5_right
*/
				else
					*event = 40;

				UV_actsm_dial = 5;
			}
			else if ( (abs(offset_c)>=abs(offset_x)) && (abs(offset_c)>=abs(offset_y)) && 
				(abs(offset_c)>=abs(offset_z)) && (abs(offset_c)>=abs(offset_a))
				&& (abs(offset_c)>=abs(offset_b)) )
			{
/*
.....dial_6_left NCL_ZROT_LEFT
*/
				if (offset_c < 0) 
					*event = 43;
/*
.....dial_6_right NCL_ZROT_RIGHT
*/
				else
					*event = 44;
				UV_actsm_dial = 6;
			}
			last_event = *event;
		}
		else
		{
/*
.....all motion function need execute
*/
			*event = 45;
		}
		MagellanEvent->MagellanPeriod = 1;
		MagellanEvent->MagellanType = MotionEvent;
		uw_ntflush_spmouse();
		return MotionEvent;
	};

	if ( Message->message == MagellanHandle->MagellanButtonPressEvent )
	{
		MagellanEvent->MagellanWindow = Message->hwnd;
		MagellanEvent->MagellanButton = (int) Message->wParam;
		MagellanEvent->MagellanType = ButtonPressEvent;
		*event = MagellanEvent->MagellanButton;
		return ButtonPressEvent;
	};

	if ( Message->message == MagellanHandle->MagellanButtonReleaseEvent )
	{
		MagellanEvent->MagellanWindow = Message->hwnd;
		MagellanEvent->MagellanButton = (int) Message->wParam;
		MagellanEvent->MagellanType = ButtonReleaseEvent;
		return ButtonReleaseEvent;
	};

	return FALSE;
}
#endif
