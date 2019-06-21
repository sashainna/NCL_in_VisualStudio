/************************************************************************
c
c   FILE NAME: DialogItem.cpp
c
c	 CONTAINS: 
c	 all CDialogItem class override functions and 
c			Implementation functions
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        DialogItem.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:01
c
c**********************************************************************
*/

#include "Pwstdafx.h"
#include "Mpost.h"
#include "DialogTemplate.h"
#include "mpostres.h"
#include "NpwHeaders.h"

/***********************************************************************
c
c   SUBROUTINE:  InitBox(int x1, int y1, int x2, int y2)
c
c   FUNCTION:  This function initial CDialogItem data for groupbox
c
c   INPUT:  int x1, int y1, int x2, int y2: coordinate of 
c							groupbox in pixel
c		
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogItem::InitBox(int x1, int y1, int x2, int y2)
{
	m_controltype = 0x0082;
	m_dlgItemTemplate.x = x1;
	m_dlgItemTemplate.y = y1;
	m_dlgItemTemplate.cx = x2 - x1;
	m_dlgItemTemplate.cy = y2 - y1;
	m_dlgItemTemplate.dwExtendedStyle = 0;
	m_dlgItemTemplate.id = IDC_GROUP_BOX;
	m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE |SS_WHITEFRAME;
}

/***********************************************************************
c
c   SUBROUTINE:  Initpanel(LPCTSTR Buttonname, UINT ButtonId,  int x1, 
c												int y1, int x2, int y2)
c
c   FUNCTION:  This function initial CDialogItem data for push buttons
c
c   INPUT:  int x1, int y1, int x2, int y2: coordinate of button
c			Buttonname: push button label
c			ButtonId: button callback id		
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogItem::Initpanel(LPCTSTR Buttonname, UINT ButtonId,  int x1, 
int y1, int x2, int y2)
{
	m_controltype = 0x0080;
	m_dlgItemTemplate.x = x1;
	m_dlgItemTemplate.y = y1;
	m_dlgItemTemplate.cx = x2 - x1;
	m_dlgItemTemplate.cy = y2 - y1;
	m_dlgItemTemplate.dwExtendedStyle = 0;
	m_dlgItemTemplate.id = ButtonId;
	m_dlgItemTemplate.style = WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON ;
	m_strCaption = (Buttonname != NULL)? Buttonname : _T("");
}

void CDialogItem::Inittemp(LPCTSTR Buttonname, int Buttype, 
				UINT call_id, int x, int y, int wid, int hgt)
{

	switch (Buttype)
	{
/*
......push button
*/
		case 1:
			m_controltype = 0x0080;
			break;
/*
......choice, use Combo box
*/
		case 2:
			m_controltype = 0x0085;
			break;
/*
......Edit Text
*/
		case 3:
			m_controltype = 0x0081;
			break;
/*
......Label
*/
		case 4:
			m_controltype = 0x0082;
			break;
/*
.....List Box
*/
		case 5:
			m_controltype = 0x0083;
			break;
		case 6:
/*
......Scroll bar 
*/
			m_controltype = 0x0084;
/*
......check button
*/
		case 7:
			m_controltype = 0x0080;
			break;
/*
......combol list
*/
		case 8:
		case 9:
			m_controltype = 0x0085;
			break;
		case 10:
			m_controltype = 0x0082;
			break;
/*
......Read only Edit Text
*/
		case 11:
			m_controltype = 0x0081;
			break;
		case 12:
/*
......group box
*/
			m_controltype = 0x0080;
			break;
	}

	m_dlgItemTemplate.x = x;
	m_dlgItemTemplate.y = y;
	m_dlgItemTemplate.cx = wid;
	m_dlgItemTemplate.cy = hgt;
	m_dlgItemTemplate.dwExtendedStyle = 0;
	m_dlgItemTemplate.id = call_id;
	switch(m_controltype)
	{
/*
.....Button
*/
	case 0x0080:
		if (Buttype==7)
			m_dlgItemTemplate.style = 
								WS_VISIBLE | WS_CHILD | BS_LEFT | BS_AUTOCHECKBOX | WS_TABSTOP | BS_NOTIFY ;
		else if (Buttype==12)
			m_dlgItemTemplate.style = 
								WS_VISIBLE | WS_CHILD | WS_GROUP | BS_GROUPBOX;
		else
			m_dlgItemTemplate.style = 
								WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT | WS_TABSTOP | BS_NOTIFY ;
		break;
/*
.....Edit control
*/
	case 0x0081:
		if (Buttype==11)
/*
.....read only, multiple line
*/
		{
			m_dlgItemTemplate.style = 
						WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | ES_AUTOVSCROLL | ES_LEFT | WS_BORDER | ES_MULTILINE |ES_WANTRETURN | ES_READONLY;
		}
		else
			m_dlgItemTemplate.style = 
						WS_CHILD | WS_VISIBLE | ES_AUTOHSCROLL | ES_LEFT | WS_BORDER | WS_TABSTOP;
		break;
/*
.....Static Text
*/
	case 0x0082:
		if (Buttype!=10)
			m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | SS_LEFT;
		else
			m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | SS_GRAYFRAME | SS_SUNKEN;
		break;
/*
.....List Box
*/
	case 0x0083:
		m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_BORDER | LBS_NOTIFY | WS_VSCROLL | WS_TABSTOP;
		break;
/*
.....combo box
*/
	case 0x0085:
		if (Buttype==2)
			m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL | WS_TABSTOP;
		else if (Buttype==8)
			m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | CBS_SIMPLE | CBS_AUTOHSCROLL | WS_VSCROLL | WS_TABSTOP;
		else
			m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_VSCROLL | WS_TABSTOP;
		break;
	default:
		ASSERT(FALSE);  
	}
	m_strCaption = (Buttonname != NULL)? Buttonname : _T("");
	if ((m_controltype==0x0080)&&(Buttonname != NULL))
/*
.....if it is button, replace '&' with '&&' because
.....if only '&', Window will though it set 'Mnemonic key'
*/
	{
		m_strCaption.Replace("&", "&&");
	}
}


/***********************************************************************
c
c   SUBROUTINE:  Inittemp(LPCTSTR Buttonname, UINT ButtonId, int Buttype, 
	UINT call_id, int rows, int len)
c
c   FUNCTION:  This function initial CDialogItem data for controls from 
c				CDialodTemplate
c
c   INPUT:  Buttype: button type
c			call_id: button callback id
c			rows , len: rows and cols of button
c			Buttonname: push button label
c			ButtonId: button id		
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogItem::Inittemp(LPCTSTR Buttonname, UINT ButtonId, int Buttype, 
UINT call_id, int rows, int len)
{
	// first fill in the type, location and size of the control
/*
.....need change Buttype to MFC controltype
.....Yurong 3/24/98
*/
	switch (Buttype)
	{
/*
......push button
*/
		case 1:
			m_controltype = 0x0080;
			break;
/*
......choice, use Combo box
*/
		case 2:
			m_controltype = 0x0085;
			break;
/*
......Edit Text
*/
		case 3:
			m_controltype = 0x0081;
			break;
/*
......Label
*/
		case 4:
			m_controltype = 0x0082;
			break;
/*
.....Dynamic Choice
*/
		case 5:
			m_controltype = 0x0085;
	}

//	id = ButtonId;
	m_dlgItemTemplate.x = 3*CHAR_WID;
	m_dlgItemTemplate.y = CHAR_HT*(rows+1);
	m_dlgItemTemplate.cx = len*CHAR_WID;
	m_dlgItemTemplate.cy = CHAR_HT;
	m_dlgItemTemplate.dwExtendedStyle = 0;
	m_dlgItemTemplate.id = call_id;

	switch(m_controltype)
	{
/*
.....Button
*/
	case 0x0080:
		m_dlgItemTemplate.style = 
								WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT;
		break;
/*
.....Edit control
*/
	case 0x0081:
		m_dlgItemTemplate.style = 
						WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_MULTILINE | ES_LEFT;
		break;
/*
.....Static Text
*/
	case 0x0082:
		m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | SS_LEFT;
		break;
/*
.....List Box
*/
	case 0x0083:
		m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE;
		break;
/*
.....combo box
*/
	case 0x0085:
		m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST;
		break;

	default:
		ASSERT(FALSE);  
	}

	m_strCaption = (Buttonname != NULL)? Buttonname : _T("");
}

/***********************************************************************
c
c   SUBROUTINE:  setctl(int type)
c
c   FUNCTION:  This function set CDialogItem data m_controltype
c
c   INPUT:  type: control type to be set
c		
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogItem::setctl(int type)
{
	switch (type)
	{
/*
......push button
*/
		case 0:
			m_controltype = 0x0080;
			break;
		case 1:
			m_controltype = 0x0081;
			break;
		case 2:
			m_controltype = 0x0082;
			break;
		case 3:
			m_controltype = 0x0083;
			break;
		case 4:
			m_controltype = 0x0084;
			break;
		case 5:
			m_controltype = 0x0085;
			break;

	}	  
}

/***********************************************************************
c
c   SUBROUTINE:  setcaption(LPCTSTR Buttonname)
c
c   FUNCTION:  This function set CDialogItem data m_strCaption
c
c   INPUT:  Buttonname: control label to be set
c		
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogItem::setcaption(LPCTSTR Buttonname)
{
	m_strCaption = (Buttonname != NULL)? Buttonname : _T("");
}

/***********************************************************************
c
c   SUBROUTINE:  setcaption(LPCTSTR Buttonname)
c
c   FUNCTION:  This function set CDialogItem data m_strCaption
c
c   INPUT:  Buttonname: control label to be set
c		
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogItem::setId(int num)
{
	id = num;
}
/***********************************************************************
c
c   SUBROUTINE:  settemp(int cols, int rows, int height, int width,
c				 int call_id, DWORD style)
c
c   FUNCTION:  This function set CDialogItem data m_dlgItemTemplate
c
c   INPUT:  
c		
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogItem::settemp(int cols, int rows, int height, int width,
 int call_id, DWORD style)
{
	double cwid;
	cwid = CHAR_WID;
	m_dlgItemTemplate.x = (short)(cols) * cwid;
	m_dlgItemTemplate.y = (CHAR_HT)*(rows);
	m_dlgItemTemplate.cx = (short)width*cwid;
	m_dlgItemTemplate.cy = (CHAR_HT)*height;
	m_dlgItemTemplate.dwExtendedStyle = 0;
	m_dlgItemTemplate.id = call_id;
	m_dlgItemTemplate.style = style;
}


/***********************************************************************
c
c   SUBROUTINE:  settemp(int cols, int rows, int height, int width,
c				 int call_id, DWORD style)
c
c   FUNCTION:  This function set CDialogItem data m_dlgItemTemplate
c
c   INPUT:  
c		
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogItem::settemp2(int x, int y, int height, int width,
 int call_id, DWORD style)
{
	m_dlgItemTemplate.x = x;
	m_dlgItemTemplate.y = y;
	m_dlgItemTemplate.cx = (short)width;
	m_dlgItemTemplate.cy = height;
	m_dlgItemTemplate.dwExtendedStyle = 0;
	m_dlgItemTemplate.id = call_id;
	m_dlgItemTemplate.style = style;
}


