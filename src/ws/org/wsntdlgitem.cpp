/************************************************************************
**
**   FILE NAME: wsntdlgitem.cpp
**
**	 CONTAINS: 
**	 all CDialogItem class override functions and 
**			Implementation functions
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdlgitem.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 17:49:04
**
c**********************************************************************
*/

#include "wsntstdafx.h"
#include "wsntdlgitem.h"
#include "wsntres.h"

/***********************************************************************
c
c   SUBROUTINE:  Inittemp(LPCTSTR Buttonname, UINT ButtonId, int Buttype, 
	UINT call_id, int x, int y, int wid, int hgt)
c
c   FUNCTION:  This function initial CDialogItem data for controls 
c			
c
c   INPUT:   x, y: dialog item position
c			hgt, wid: dialog item size
c			Buttype: dialog item type
c			call_id: dialog item callback id
c			Buttonname: dialog item label
c			ButtonId: dialog item id		
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogItem::Inittemp(LPCTSTR Buttonname, int Buttype, 
				UINT call_id, int x, int y, int wid, int hgt)
{
	const char* cstr = (LPCTSTR)Buttonname;
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
......Edit Text (single line)
*/
		case 3:
/*
......Edit Text (multi-lines)
*/
		case 16:
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
		case 13:
/*
......process bar, the code is set by myself, all other code
......is predefined system class code, but don't know the
......predefined system class code for process bar, so I use
......'class name' instead of ordinal value code for process bar
......this code define for remind me later to use class name 
......instead of code, don't using the code predefined
*/
			m_controltype = 0x1080;
			break;
		case 14:
/*
.....fro read only text with scroll bar
*/
			m_controltype = 0x0081;
			break;
/*
......choice, use Combo box
*/
		case 15:
			m_controltype = 0x0085;
			break;
		case 17:
/*
......frame box
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
		else if (Buttype==17)
			m_dlgItemTemplate.style = 
								WS_VISIBLE | WS_CHILD | WS_GROUP | BS_GROUPBOX;
		else
			m_dlgItemTemplate.style = 
								WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_NOTIFY ;
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
						WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | ES_AUTOVSCROLL | ES_LEFT | WS_BORDER | ES_MULTILINE |ES_WANTRETURN | ES_READONLY | DS_SETFONT;
		}
		else if (Buttype==14)
		{
			m_dlgItemTemplate.style = 
						WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | WS_HSCROLL | WS_VSCROLL | ES_LEFT | WS_BORDER | ES_MULTILINE |ES_WANTRETURN | ES_READONLY | DS_SETFONT;
		}
/*
.....editable, multiple line
*/
		else if (Buttype==16)
		{
			m_dlgItemTemplate.style = 
						WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | ES_AUTOVSCROLL | ES_LEFT | WS_BORDER | ES_MULTILINE |ES_WANTRETURN | DS_SETFONT;
		}
		else
			m_dlgItemTemplate.style = 
						WS_CHILD | WS_VISIBLE | ES_AUTOHSCROLL | ES_LEFT | WS_BORDER | WS_TABSTOP | DS_SETFONT;
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
		m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_BORDER | LBS_NOTIFY | WS_VSCROLL 
			| LBS_USETABSTOPS | WS_HSCROLL ;
		break;
/*
.....combo box
*/
	case 0x0085:
		if (Buttype==2)
			m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL | WS_TABSTOP;
		else if (Buttype==8)
			m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | CBS_SIMPLE | WS_VSCROLL | CBS_AUTOHSCROLL
									| WS_TABSTOP | WS_HSCROLL;
		else if (Buttype==15)
			m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL | WS_TABSTOP | CBS_AUTOHSCROLL | WS_HSCROLL;
		else
			m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | CBS_DROPDOWN | WS_VSCROLL | CBS_AUTOHSCROLL
									| WS_TABSTOP | WS_HSCROLL | CBS_NOINTEGRALHEIGHT;
		break;
	case 0x1080:
		m_dlgItemTemplate.style = WS_CHILD | WS_VISIBLE | WS_BORDER | PBS_SMOOTH;
		break;
	default:
		ASSERT(FALSE);  
	}
	m_strCaption = (Buttonname != NULL)? Buttonname : _T("");
	if ((Buttype==7)&&(cstr[0]=='\0'))
		m_strCaption = _T(" ");
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
c   SUBROUTINE:  settemp(int cols, int rows, int height, int width,
c				 int call_id, DWORD style)
c
c   FUNCTION:  This function set CDialogItem data m_dlgItemTemplate
c
c   INPUT:  x, y: dialog item position
c			height, width: dialog item size
c			call_id: dialog item ID
c			style: dialog item style
c		
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogItem::settemp(int x, int y, int height, int width,
 int call_id, DWORD style)
{
	m_dlgItemTemplate.x = x;
	m_dlgItemTemplate.y = y;
	m_dlgItemTemplate.cx = width;
	m_dlgItemTemplate.cy = height;
	m_dlgItemTemplate.dwExtendedStyle = 0;
	m_dlgItemTemplate.id = call_id;
	m_dlgItemTemplate.style = style;
}


