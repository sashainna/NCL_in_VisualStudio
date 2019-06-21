/********************************************************************* 
**  NAME:  wsntdroptarget2.cpp
**
**			implementation of CNCLDropTarget class functions
**	CONTAINS: CNCLDropTarget2  class functions
**			all functions declared in wsntdroptarget2.h
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**     wsntmdroptarget2.cpp , 25.4
**  DATE AND TIME OF LAST  MODIFICATION
**     08/18/15 , 08:53:49
*********************************************************************/
#include "wsntstdafx.h"
#include "wsntres.h"
#include "wsntdroptarget.h"
#include "wsntDDListBox.h"
#include "wsntDDCombo.h"
#include "wsntDDButton.h"
#include "wsntDDform.h"
#include "wsntDDpic.h"
#include "wsntDDclrbtn.h"
#include "wsntDDEdit.h"
#include "wsntDDlstctl.h"
#include "wsntDDlstctl2.h"
#include "wsntDDprocess.h"
#include "wsntDDGroup.h"
#include "wsntIconListBox.h"
#include "wsntDDstatic.h"
#include "wsntDDstatic.h"
#include "wsntDDSlider.h"
#include "wsntsecbtn.h"
#include "wsntfrmview.h"
int UW_formadd_item = 0;
int UW_form_hotspot_item = 0;
int UW_form_no_picitem = 0;
static int test_count = 0;
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
CNCLMnDropTarget2::CNCLMnDropTarget2()
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
DWORD CNCLMnDropTarget2::GotDrag()
{
	if(m_DropTargetWnd)
	{
		CPoint iPoint = m_DropPoint;
		CWnd *iPossibleWnd = CWnd::WindowFromPoint(iPoint);
		if(iPossibleWnd==NULL)
			return DROPEFFECT_NONE;	

		if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDform)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_COPY;   
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDListBox)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDCombo)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDPicWin)))
		{
			if (UW_form_no_picitem)
				return DROPEFFECT_NONE;
			if (UW_formadd_item==1)
				return DROPEFFECT_NONE;		
			else
				return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDGroup)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDProcess)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDListCtrl)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDListCtrl2)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDEdit)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDColorButton)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CIconListBox)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDSlider)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			return DROPEFFECT_MOVE;		
		}
		else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLSecButton)))
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			if (UW_formadd_item==1)
				return DROPEFFECT_NONE;		
			else
				return DROPEFFECT_MOVE;		
		}
		else
		{
			if (UW_form_hotspot_item)
				return DROPEFFECT_NONE;	
			CWnd *temp = iPossibleWnd->GetParent();
			if ((temp!=NULL)&&(temp->IsKindOf(RUNTIME_CLASS(CNCLDDCombo))))
			{
				return DROPEFFECT_MOVE;
			}
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
void CNCLMnDropTarget2::GotLeave()
{
	if (m_DropTargetWnd)
	{
		if(m_DropTargetWnd->IsKindOf(RUNTIME_CLASS(CNCLSecButton)))
		{
			((CNCLSecButton*)m_DropTargetWnd)->ShowDropBox(0);
		}
	}
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
**New CHECKBOX
**********************************************************************/
DWORD CNCLMnDropTarget2::GotEnter()
{
	if (m_DropTargetWnd)
	{
		if(m_DropTargetWnd->IsKindOf(RUNTIME_CLASS(CNCLSecButton)))
		{
			if (UW_formadd_item==0)
				((CNCLSecButton*)m_DropTargetWnd)->ShowDropBox(1);
		}
	}
	return DROPEFFECT_LINK;   
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
void CNCLMnDropTarget2::GotDrop()
{
	int i;
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

			if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDform)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDform*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDButton*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDListBox)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDListBox*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDCombo)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDCombo*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDColorButton)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDColorButton*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDEdit)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDEdit*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDListCtrl)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDListCtrl*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDListCtrl2)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDListCtrl2*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDPicWin)))
			{
				((CNCLDDPicWin*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDProcess)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDProcess*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDStatic*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDGroup)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDGroup*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CIconListBox)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CIconListBox*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLDDSlider)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLDDSlider*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
			}
			else if(iPossibleWnd->IsKindOf(RUNTIME_CLASS(CNCLSecButton)))
			{
				if (UW_form_hotspot_item)
					return;	
				((CNCLSecButton*)iPossibleWnd)->OnDragDropCallback(pt, input_text);
				((CNCLSecButton*)iPossibleWnd)->ShowDropBox(0);
			}
			else
			{
				CWnd *temp = iPossibleWnd->GetParent();
				if(temp->IsKindOf(RUNTIME_CLASS(CNCLDDCombo)))
				{
					((CNCLDDCombo*)temp)->OnDragDropCallback(pt, input_text);
				}
			}
		}
	}
}
