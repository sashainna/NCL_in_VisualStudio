/*********************************************************************
**  NAME:  PtdProcessDlg.h
**  Description:
**				all member function and variable for class PtdProcessDlg
**				A class for Process dialogs
**    CONTAINS:
**
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**      PtdProcessDlg.h , 24.1 
**    DATE AND TIME OF LAST  MODIFICATION
**      09/11/13 , 12:58:32 
*********************************************************************/
#ifndef PTDPROCESSDLGH
#define PTDPROCESSDLGH

#include "PtdBase.h"
#include "PtdMainWindow.h"
                           
class PtdProcessDlg : public PtdBase
{
	protected:
		int m_select, m_first;
		char m_label[256];
		int m_percent, m_lastpos;
		Widget m_labelwin;
		Widget m_process_area;
		Widget m_wok;
		Widget m_parent;
		GC m_draw_gc;
		
	public:
    
		PtdProcessDlg(Widget, char *);
		~PtdProcessDlg();
    
		int Post(char *, 
			 void *clientData      = NULL);
		static void CancelCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void drawCB(Widget widget, XtPointer client_data, XtPointer call_data);
		void Display_as_percent(int num);
};
#endif
