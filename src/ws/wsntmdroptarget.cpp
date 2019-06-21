/********************************************************************* 
**  NAME:  wsntmdroptarget.cpp
**
**			implementation of CNCLDropTarget class functions
**	CONTAINS: CNCLDropTarget  class functions
**			all functions declared in wsntdroptarget.h
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**     wsntmdroptarget.cpp , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**     04/29/15 , 15:12:28
*********************************************************************/
#include "wsntstdafx.h"
#include "wsntres.h"
#include "wsntdroptarget.h"

#include "wsntdoc.h"
#include "wsntframe.h"
#include "wsntview.h"
#include "wsnttmenu.h"
#include "wsntpmenu.h"
#include "wsncldockbar.h"
#include "wsntStatusBar.h"
#include "wsntstatbtn.h"
int UW_drag_obj = -1;

/***********************************************************************
**
**   FUNCTION: CNCLDropTarget()
**
**              Constructor of class CNCLDropTarget
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLMnDropTarget::CNCLMnDropTarget()
{
}

/***********************************************************************
**
**   FUNCTION: GotDrag()
**
**      Dragging in the control. Check the runtime class to see if it
**		is one of the class we want to drag
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
DWORD CNCLMnDropTarget::GotDrag()
{
	if(m_DropTargetWnd)
	{
		CPoint iPoint = m_DropPoint;
		CWnd *iPossibleWnd = CWnd::WindowFromPoint(iPoint);
		if(iPossibleWnd==NULL)
			return DROPEFFECT_NONE;	

		if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CToolmenu)))
		{
			int type = ((CToolmenu*)iPossibleWnd)->GetBarType();
			if (type==0)
			{
				if (UW_drag_obj!=1)
					return DROPEFFECT_MOVE;   
				else
					return DROPEFFECT_NONE;	
			}
			else
			{
				if (UW_drag_obj==1)
					return DROPEFFECT_MOVE;   
				else
					return DROPEFFECT_NONE;	
			}
		}
		else if (iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLStatusBar)))
		{
			return DROPEFFECT_MOVE;   
//			return DROPEFFECT_NONE;	
		}
		else if (iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLStatButton)))
		{
/*			if (UW_drag_obj==1)
				return DROPEFFECT_MOVE;   
			else
				return DROPEFFECT_NONE;	
*/
			return DROPEFFECT_MOVE;   
/*			int type = ((CNCLStatButton*)iPossibleWnd)->GetType();
			if (type==0)
			{
				if (UW_drag_obj==1)
					return DROPEFFECT_MOVE;   
				else
					return DROPEFFECT_NONE;	
			}
			else
			{
				if (UW_drag_obj==1)
					return DROPEFFECT_MOVE;   
				else
					return DROPEFFECT_NONE;	
			}
*/
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLMenu)))
		{
			if (UW_drag_obj!=1)
				return DROPEFFECT_MOVE;   
			else
				return DROPEFFECT_NONE;	
//			return DROPEFFECT_LINK;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLView)))
		{
//			if (UW_drag_obj!=1)
				return DROPEFFECT_LINK;		
//			else
//				return DROPEFFECT_NONE;	
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CMainFrame)))
		{
//			return DROPEFFECT_LINK;		
			return DROPEFFECT_NONE;	
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLTableList)))
		{
			if(m_DropTargetWnd->IsKindOf(RUNTIME_CLASS(CNCLTableList)))
				return DROPEFFECT_LINK;		
			else
				return DROPEFFECT_NONE; 
		}
	}
	return DROPEFFECT_NONE;   
}

/***********************************************************************
**
**   FUNCTION: GotLeave()
**
**        Called if we have a drop text leave here.
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
void CNCLMnDropTarget::GotLeave()
{
}

/***********************************************************************
**
**   FUNCTION: GotEnter()
**
**        Called if we have a drop text enter here.
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
DWORD CNCLMnDropTarget::GotEnter()
{
//	return DROPEFFECT_LINK;   
	return DROPEFFECT_NONE;
}

/***********************************************************************
**
**   FUNCTION: GotDrop()
**
**        Called if we have a drop text drop here.
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
void CNCLMnDropTarget::GotDrop()
{
	char input_text[100];
	if(m_Data)
	{
		char *iText = (char *)m_Data;
		strcpy(input_text, iText);
		if((iText) && (m_DropTargetWnd))
		{
			CPoint pt = m_DropPoint;

			CWnd *iPossibleWnd = CWnd::WindowFromPoint(pt);
			if(NULL == iPossibleWnd)
				return;	

			if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CToolmenu)))
			{
				((CToolmenu*)iPossibleWnd)->UpdateMenuBar(pt, input_text);
			}
			else if (iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLStatusBar)))
			{
				((CNCLStatusBar*)iPossibleWnd)->UpdateStatusBar(pt, input_text);
			}
			else if (iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLStatButton)))
			{
				((CNCLStatButton*)iPossibleWnd)->UpdateStatusBar(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLView)))
			{
				((CNCLView*)iPossibleWnd)->AddFloatMenu(pt, input_text);
			}
//			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CMainFrame)))
//			{
//				((CMainFrame*)iPossibleWnd)->UpdatePOPMenu(pt, input_text);
//			}
//			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDockBar)))
//			{
//				((CNCLDockBar*)iPossibleWnd)->AddMenuBar(pt, input_text);
//			}
		}
	}
}
