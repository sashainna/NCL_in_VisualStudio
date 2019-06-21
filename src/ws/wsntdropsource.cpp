/********************************************************************* 
**  NAME:  wsntdropsource.cpp
**
**			implementation of CNCLDropSource class functions
**	CONTAINS: CNCLDropSource  class functions
**			all functions declared in wsntdropsource.h
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**     wsntdropsource.cpp , 25.3
**  DATE AND TIME OF LAST  MODIFICATION
**     08/18/15 , 08:45:56
*********************************************************************/
#include "wsntstdafx.h"
#include <afxole.h>         // MFC OLE classes
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxdisp.h >       // MFC OLE automation classes
#include "wsntres.h"
#include "wsntdropsource.h"
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
#include "wsntDDstatic.h"
#include "wsntDDSlider.h"

/***********************************************************************
**
**   FUNCTION: CNCLDropSource()
**
**              Constructor of class CNCLDropSource
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDropSource::CNCLDropSource()
{
	m_DragImage =NULL;
}

void CNCLDropSource::SetImageList(CImageList *imagelist)
{
	m_DragImage = imagelist;
}

void CNCLDropSource::SetAttWindow(CWnd *wnd, CPoint point, CRect rect)
{
	m_parent = wnd;
	m_point = point;
	m_rect = rect;
}

CNCLDropSource::~CNCLDropSource()
{
}

/***********************************************************************
c
c   SUBROUTINE:  AdjustGrid(POINT &ptCursor)
c
c   FUNCTION:  adjust grid point
c
c   INPUT:  ptCursor: pt to be adjust
c			
c   OUTPUT: none
c	RETURN: none
c			
c
c***********************************************************************
*/
void CNCLDropSource::AdjustGrid(POINT &ptCursor)
{
	POINT temp;
	int delx = ptCursor.x - m_point.x;
	int dely = ptCursor.y - m_point.y;

	int x, y;
	x = m_rect.left + delx;
	y = m_rect.top + dely;

	if (x%10<=5)
	{
		delx = -x%10;
	}
	else
	{
		delx = 10 - x%10;
	}
	if (y%10<=5)
	{
		dely = -y%10;
	}
	else
	{
		dely = 10 - y%10;
	}
	ptCursor.x += delx;
	ptCursor.y += dely;
}

/***********************************************************************
c
c   SUBROUTINE:  OnBeginDrag(CWnd* pWnd)
c
c   FUNCTION:  Called when beginning drag operation
c
c   INPUT:  pWnd: Window which drag
c			
c   OUTPUT: none
c	RETURN: 
c			
c
c***********************************************************************
*/
BOOL CNCLDropSource::OnBeginDrag(CWnd* pWnd)
{
	BOOL ret = COleDropSource::OnBeginDrag(pWnd);
	if (m_DragImage==NULL)
		return ret;
	if (ret)
	{
		POINT ptCursor;
		::GetCursorPos(&ptCursor);
		if (m_DragImage->BeginDrag(0, m_dragpt))
		{
			m_DragImage->DragEnter(NULL, (CPoint)ptCursor);
			m_ptCursor.x = ptCursor.x;
			m_ptCursor.y = ptCursor.y;
		}
	}
	return ret;
}
/***********************************************************************
c
c   SUBROUTINE:  GiveFeedback(DROPEFFECT dropEffect)
c
c   FUNCTION:  Enables a source application to give visual feedback to the end user 
c			during a drag-and-drop operation by providing the DoDragDrop function with 
c			an enumeration value specifying the visual effect.
c
c   INPUT:  dropEffect: The DROPEFFECT value returned by the most recent call to 
c				IDropTarget::DragEnter, IDropTarget::DragOver, or IDropTarget::DragLeave. 
c			
c   OUTPUT: none
c	RETURN: S_OK: successful compNew CHECKBOXletion
c			
c
c***********************************************************************
*/
SCODE CNCLDropSource::GiveFeedback(DROPEFFECT dropEffect)
{
	if (m_DragImage==NULL)
		return COleDropSource::GiveFeedback(dropEffect);

	POINT ptCursor;
	::GetCursorPos(&ptCursor);
	AdjustGrid(ptCursor);
	if ((ptCursor.x!=m_ptCursor.x)||(ptCursor.y!=m_ptCursor.y))
	{
		m_DragImage->DragMove((CPoint)ptCursor);
		m_DragImage->DragEnter(NULL, (CPoint)ptCursor);
		m_ptCursor.x = ptCursor.x;
		m_ptCursor.y = ptCursor.y;
		if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDListBox)))
		{
			((CNCLDDListBox*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
		{
			((CNCLDDButton*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDCombo)))
		{
			((CNCLDDCombo*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
		{
			((CNCLDDStatic*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDPicWin)))
		{
			((CNCLDDPicWin*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDGroup)))
		{
			((CNCLDDGroup*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDProcess)))
		{
			((CNCLDDProcess*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDListCtrl)))
		{
			((CNCLDDListCtrl*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDListCtrl2)))
		{
			((CNCLDDListCtrl2*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDEdit)))
		{
			((CNCLDDEdit*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDColorButton)))
		{
			((CNCLDDColorButton*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
		else if(m_parent->IsKindOf(RUNTIME_CLASS(CNCLDDSlider)))
		{
			((CNCLDDSlider*)m_parent)->DrawGuides(m_point, m_ptCursor);
		}
	}
	return COleDropSource::GiveFeedback(dropEffect);
//	return S_OK;
}
/***********************************************************************
c
c   SUBROUTINE:  QueryContinueDrag(BOOL bEscapePreessed, DWORD dwkeyState)
c
c   FUNCTION:  called while in Drag&drop event to check the event
c			Determines whether a drag-and-drop operation should be continued, 
c			canceled, or completed. The OLE DoDragDrop function calls this method 
c			during a drag-and-drop operation
c
c   INPUT:  bEscapePreessed: Indicates whether the Esc key has been pressed since 
c					the previous call to QueryContinueDrag or to DoDragDrop if this is 
c					the first call to QueryContinueDrag. 
c					A TRUE value indicates the end user has pressed the escape key; 
c					a FALSE value indicates it has not been pressed.
c			dwkeyState: The current state of the keyboard modifier keys on the keyboard. 
c					Possible values can be a combination of any of the flags MK_CONTROL, 
c					MK_SHIFT, MK_ALT, MK_BUTTON, MK_LBUTTON, MK_MBUTTON, and MK_RBUTTON.
c			
c   OUTPUT: none
c
c***********************************************************************
*/
SCODE CNCLDropSource::QueryContinueDrag(BOOL bEscapePreessed, DWORD dwkeyState)
{
	SCODE ret = COleDropSource::QueryContinueDrag(bEscapePreessed, dwkeyState);
	if (m_DragImage==NULL)
		return ret;

	if ((S_OK)!=ret)
	{
		m_DragImage->DragLeave(NULL);
		m_DragImage->EndDrag();
	}
	return ret;
}
