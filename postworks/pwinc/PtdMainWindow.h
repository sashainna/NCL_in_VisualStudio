/*********************************************************************
**  NAME:  PtdMainWindow.h
**  Description:
**          all function defined for class PtdMainWindow
**				which support a toplevel window
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdMainWindow.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:31
*********************************************************************/
#ifndef PTDMAINWINDOWH
#define PTDMAINWINDOWH

#include "PtdBase.h"

class PtdView;

class PtdMainWindow : public PtdBase 
{
	protected:
    
		Widget	m_wMain;        
		Widget	m_wWorkArea;    
		Cursor	m_BusyCursor;
		Cursor	m_NormalCursor;
		int m_type;
    
	public:
    
/*
.....0, main window
.....1, normal child window
.....2, Backup child window
.....3, Text read only window
*/
		PtdMainWindow(char *, int type=0);
		virtual ~PtdMainWindow();
		virtual void Initialize();
		virtual void ManageInitialize();
		virtual void Manage();
		virtual void UnManage();
		virtual void Iconize();
	
		void SetBusyCursor();
		void SetNormalCursor();
	
		virtual Widget MainWidget() { return m_wMain;}
		virtual Widget WorkArea() { return m_wWorkArea;}
		void SetWorkArea(Widget workArea) 
			{ m_wWorkArea = workArea;}
};
#endif
