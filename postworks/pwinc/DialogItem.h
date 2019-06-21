/************************************************************************
c
c   FILE NAME: DialogItem.h
c
c	 Description - Functions and struct declarations for
c		CDialogItem.h class 
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        DialogItem.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        DialogItem.h , 24.1
c
c**********************************************************************
*/
#ifndef DIALOGITEM_H
#define DIALOGITEM_H

#include "NpwHeaders.h"

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

	void Inittemp(LPCTSTR pszCaption = NULL, UINT nID = 0, int cType = 0, 
		UINT call_id=0, int rows = 0, int len = 0);
	void InitBox(int x1=0, int y1=0, int x2=0, int y2=0);
	void Initpanel(LPCTSTR pszCaption = NULL, UINT nID = 0, int x1=0, int y1=0,
			 int x2=0, int y2=0);
	void CDialogItem::setctl(int type);
	void CDialogItem::setcaption(LPCTSTR Buttonname);
	void CDialogItem::setId(int num);
	void CDialogItem::settemp(int cols, int rows, int height, 
			int width, int call_id, DWORD style);
	void CDialogItem::settemp2(int x, int y, int height, int width,
		int call_id, DWORD style);
};
#endif
