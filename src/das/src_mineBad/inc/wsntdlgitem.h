/************************************************************************
**
**   FILE NAME: wsntdlgitem.h
**
**	 Description - Functions and struct declarations for
**		CDialogItem class 
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdlgitem.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:16
**
************************************************************************
*/
#ifndef DIALOGITEM_H
#define DIALOGITEM_H

#define CHAR_HT							 12
#define CHAR_WID							 4

class CDialogItem
{
public:

	DLGITEMTEMPLATE  m_dlgItemTemplate;

	WORD 	m_controltype;
	CString		m_strCaption;
	WORD id;

	CDialogItem() {}; 
	void Inittemp(LPCTSTR Buttonname, int Buttype, 
				UINT call_id, int x, int y, int wid, int hgt);
	void CDialogItem::setctl(int type);
	void CDialogItem::setcaption(LPCTSTR Buttonname);
	void CDialogItem::settemp(int cols, int rows, int height, 
			int width, int call_id, DWORD style);
};
#endif
