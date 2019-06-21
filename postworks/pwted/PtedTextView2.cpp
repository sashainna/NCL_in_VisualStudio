/************************************************************************
c
c   FILE NAME: PtedTextView2.cpp
c
c   CONTAINS:
c		CPtedTextView::MoveLeft(BOOL bSelect)
c		CPtedTextView::MoveRight(BOOL bSelect)
c		CPtedTextView::MoveWordLeft(BOOL bSelect)
c		CPtedTextView::MoveWordRight(BOOL bSelect)
c		CPtedTextView::MoveUp(BOOL bSelect)
c		CPtedTextView::MoveDown(BOOL bSelect)
c		CPtedTextView::MoveHome(BOOL bSelect)
c		CPtedTextView::MoveEnd(BOOL bSelect)
c		CPtedTextView::MovePgUp(BOOL bSelect)
c		CPtedTextView::MovePgDn(BOOL bSelect)
c		CPtedTextView::MoveCtrlHome(BOOL bSelect)
c		CPtedTextView::MoveCtrlEnd(BOOL bSelect)
c		CPtedTextView::ScrollUp()
c		CPtedTextView::ScrollDown()
c		CPtedTextView::ScrollLeft()
c		CPtedTextView::ScrollRight()
c		CPtedTextView::WordToRight(CPoint pt)
c		CPtedTextView::WordToLeft(CPoint pt)
c		CPtedTextView::SelectAll()
c		CPtedTextView::OnLButtonDown(UINT nFlags, CPoint point) 
c		CPtedTextView::OnMouseMove(UINT nFlags, CPoint point) 
c		CPtedTextView::OnLButtonUp(UINT nFlags, CPoint point) 
c		CPtedTextView::OnTimer(UINT nIDEvent) 
c		CPtedTextView::OnLButtonDblClk(UINT nFlags, CPoint point) 
c		CPtedTextView::OnEditSelectAll() 
c		CPtedTextView::OnRButtonDown(UINT nFlags, CPoint point) 
c		CPtedTextView::IsSelection()
c		CPtedTextView::Copy()
c		CPtedTextView::PutToClipboard(LPCTSTR pszText)
c		CPtedTextView::GetFromClipboard(CString &text)
c		CPtedTextView::PrepareDragData()
c	
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c	MODULE NAME AND RELEASE LEVEL
c       PtedTextView2.cpp , 24.1
c	DATE AND TIME OF LAST  MODIFICATION
c       09/11/13 , 12:59:28
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include "PtedEditCmd.h"
#include "PtedTextView.h"
#include "PtedTextBuffer.h"
#include <malloc.h>

#ifndef __AFXPRIV_H__
#include <afxpriv.h>
#endif
#include <afxole.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define	PTED_TIMER_DRAGSEL	1001


/////////////////////////////////////////////////////////////////////////////
// CPtedTextView

/***********************************************************************
**
**   FUNCTION: MoveLeft(BOOL bSelect)
**
**       called when VK_LEFT is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MoveLeft(BOOL bSelect)
{
	PrepareSelBounds();
	if (m_ptDrawSelStart != m_ptDrawSelEnd && ! bSelect)
	{
		m_ptCursorPos = m_ptDrawSelStart;
	}
	else
	{
		if (m_ptCursorPos.x == 0)
		{
			if (m_ptCursorPos.y > 0)
			{
				m_ptCursorPos.y --;
				m_ptCursorPos.x = GetLineLength(m_ptCursorPos.y);
			}
		}
		else
			m_ptCursorPos.x --;
	}
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MoveRight(BOOL bSelect)
**
**       called when VK_RIGHT is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MoveRight(BOOL bSelect)
{
	PrepareSelBounds();
	if (m_ptDrawSelStart != m_ptDrawSelEnd && ! bSelect)
	{
		m_ptCursorPos = m_ptDrawSelEnd;
	}
	else
	{
		if (m_ptCursorPos.x == GetLineLength(m_ptCursorPos.y))
		{
			if (m_ptCursorPos.y < GetLineCount() - 1)
			{
				m_ptCursorPos.y ++;
				m_ptCursorPos.x = 0;
			}
		}
		else
			m_ptCursorPos.x ++;
	}
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MoveWordLeft(BOOL bSelect)
**
**       called when VK_LEFT with control is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MoveWordLeft(BOOL bSelect)
{
	PrepareSelBounds();
	if (m_ptDrawSelStart != m_ptDrawSelEnd && ! bSelect)
	{
		MoveLeft(bSelect);
		return;
	}

	if (m_ptCursorPos.x == 0)
	{
		if (m_ptCursorPos.y == 0)
			return;
		m_ptCursorPos.y --;
		m_ptCursorPos.x = GetLineLength(m_ptCursorPos.y);
	}

	LPCTSTR pszChars = GetLineChars(m_ptCursorPos.y);
	int nPos = m_ptCursorPos.x;
	while (nPos > 0 && isspace(pszChars[nPos - 1]))
		nPos --;

	if (nPos > 0)
	{
		nPos --;
		if (isalnum(pszChars[nPos]) || pszChars[nPos] == _T('_'))
		{
			while (nPos > 0 && (isalnum(pszChars[nPos - 1]) || pszChars[nPos - 1] == _T('_')))
				nPos --;
		}
		else
		{
			while (nPos > 0 && ! isalnum(pszChars[nPos - 1])
						&& pszChars[nPos - 1] != _T('_') && ! isspace(pszChars[nPos - 1]))
				nPos --;
		}
	}

	m_ptCursorPos.x = nPos;
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MoveWordLeft(BOOL bSelect)
**
**       called when VK_RIGHT with control is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MoveWordRight(BOOL bSelect)
{
	PrepareSelBounds();
	if (m_ptDrawSelStart != m_ptDrawSelEnd && ! bSelect)
	{
		MoveRight(bSelect);
		return;
	}

	if (m_ptCursorPos.x == GetLineLength(m_ptCursorPos.y))
	{
		if (m_ptCursorPos.y == GetLineCount() - 1)
			return;
		m_ptCursorPos.y ++;
		m_ptCursorPos.x = 0;
	}

	int nLength = GetLineLength(m_ptCursorPos.y);
	if (m_ptCursorPos.x == nLength)
	{
		MoveRight(bSelect);
		return;
	}

	LPCTSTR pszChars = GetLineChars(m_ptCursorPos.y);
	int nPos = m_ptCursorPos.x;
	if (isalnum(pszChars[nPos]) || pszChars[nPos] == _T('_'))
	{
		while (nPos < nLength && isalnum(pszChars[nPos]) || pszChars[nPos] == _T('_'))
			nPos ++;
	}
	else
	{
		while (nPos < nLength && ! isalnum(pszChars[nPos])
						&& pszChars[nPos] != _T('_') && ! isspace(pszChars[nPos]))
			nPos ++;
	}

	while (nPos < nLength && isspace(pszChars[nPos]))
		nPos ++;

	m_ptCursorPos.x = nPos;
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MoveUp(BOOL bSelect)
**
**       called when VK_UP is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MoveUp(BOOL bSelect)
{
	PrepareSelBounds();
	if (m_ptDrawSelStart != m_ptDrawSelEnd && ! bSelect)
		m_ptCursorPos = m_ptDrawSelStart;

	if (m_ptCursorPos.y > 0)
	{
		if (m_nIdealCharPos == -1)
			m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
		m_ptCursorPos.y --;
		m_ptCursorPos.x = ApproxActualOffset(m_ptCursorPos.y, m_nIdealCharPos);
		if (m_ptCursorPos.x > GetLineLength(m_ptCursorPos.y))
			m_ptCursorPos.x = GetLineLength(m_ptCursorPos.y);
	}
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MoveDown(BOOL bSelect)
**
**       called when VK_DOWN is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MoveDown(BOOL bSelect)
{
	PrepareSelBounds();
	if (m_ptDrawSelStart != m_ptDrawSelEnd && ! bSelect)
		m_ptCursorPos = m_ptDrawSelEnd;

	if (m_ptCursorPos.y < GetLineCount() - 1)
	{
		if (m_nIdealCharPos == -1)
			m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
		m_ptCursorPos.y ++;
		m_ptCursorPos.x = ApproxActualOffset(m_ptCursorPos.y, m_nIdealCharPos);
		if (m_ptCursorPos.x > GetLineLength(m_ptCursorPos.y))
			m_ptCursorPos.x = GetLineLength(m_ptCursorPos.y);
	}
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MoveHome(BOOL bSelect)
**
**       called when VK_HOME is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MoveHome(BOOL bSelect)
{
	int nLength = GetLineLength(m_ptCursorPos.y);
	LPCTSTR pszChars = GetLineChars(m_ptCursorPos.y);
	int nHomePos = 0;
	while (nHomePos < nLength && isspace(pszChars[nHomePos]))
		nHomePos ++;
	if (nHomePos == nLength || m_ptCursorPos.x == nHomePos)
		m_ptCursorPos.x = 0;
	else
		m_ptCursorPos.x = nHomePos;
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MoveEnd(BOOL bSelect)
**
**       called when VK_END with control is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MoveEnd(BOOL bSelect)
{
	m_ptCursorPos.x = GetLineLength(m_ptCursorPos.y);
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MovePgUp(BOOL bSelect)
**
**       called when VK_PRIOR is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MovePgUp(BOOL bSelect)
{
	int nNewTopLine = m_nTopLine - GetScreenLines() + 1;
	if (nNewTopLine < 0)
		nNewTopLine = 0;
	if (m_nTopLine != nNewTopLine)
	{
		ScrollToLine(nNewTopLine);
	}

	m_ptCursorPos.y -= GetScreenLines() - 1;
	if (m_ptCursorPos.y < 0)
		m_ptCursorPos.y = 0;
	if (m_ptCursorPos.x > GetLineLength(m_ptCursorPos.y))
		m_ptCursorPos.x = GetLineLength(m_ptCursorPos.y);
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MovePgUp(BOOL bSelect)
**
**       called when VK_NEXT is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MovePgDn(BOOL bSelect)
{
	int nNewTopLine = m_nTopLine + GetScreenLines() - 1;
	if (nNewTopLine >= GetLineCount())
		nNewTopLine = GetLineCount() - 1;
	if (m_nTopLine != nNewTopLine)
	{
		ScrollToLine(nNewTopLine);
	}

	m_ptCursorPos.y += GetScreenLines() - 1;
	if (m_ptCursorPos.y >= GetLineCount())
		m_ptCursorPos.y = GetLineCount() - 1;
	if (m_ptCursorPos.x > GetLineLength(m_ptCursorPos.y))
		m_ptCursorPos.x = GetLineLength(m_ptCursorPos.y);
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MoveCtrlHome(BOOL bSelect)
**
**       called when VK_HOME with control key is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MoveCtrlHome(BOOL bSelect)
{
	m_ptCursorPos.x = 0;
	m_ptCursorPos.y = 0;
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: MoveCtrlEnd(BOOL bSelect)
**
**       called when VK_END with control key is executed
**
**   INPUT: bSelect: if true: shift key
**						false: normal left key
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::MoveCtrlEnd(BOOL bSelect)
{
	m_ptCursorPos.y = GetLineCount() - 1;
	m_ptCursorPos.x = GetLineLength(m_ptCursorPos.y);
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	EnsureVisible(m_ptCursorPos);
	UpdateCaret();
	if (! bSelect)
		m_ptAnchor = m_ptCursorPos;
	SetSelection(m_ptAnchor, m_ptCursorPos);
	FlushUndoGroup();
}

/***********************************************************************
**
**   FUNCTION: ScrollUp()
**
**       scroll the line up
**
**   INPUT: none
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::ScrollUp()
{
	if (m_nTopLine > 0)
	{
		ScrollToLine(m_nTopLine - 1);
	}
}

/***********************************************************************
**
**   FUNCTION: ScrollDown()
**
**       scroll the line Down
**
**   INPUT: none
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::ScrollDown()
{
	if (m_nTopLine < GetLineCount() - 1)
	{
		ScrollToLine(m_nTopLine + 1);
	}
}

/***********************************************************************
**
**   FUNCTION: ScrollLeft()
**
**       scroll Left
**
**   INPUT: none
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::ScrollLeft()
{
	if (m_nOffsetChar > 0)
	{
		ScrollToChar(m_nOffsetChar - 1);
		UpdateCaret();
		FlushUndoGroup();
	}
}

/***********************************************************************
**
**   FUNCTION: ScrollRight()
**
**       scroll Right
**
**   INPUT: none
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::ScrollRight()
{
	if (m_nOffsetChar < GetMaxLineLength() - 1)
	{
		ScrollToChar(m_nOffsetChar + 1);
		UpdateCaret();
		FlushUndoGroup();
	}
}

/***********************************************************************
**
**   FUNCTION: WordToRight(CPoint pt)
**
**       calculate the word right position
**
**   INPUT: pt: current position
**
**   OUTPUT : pt: position of the right of the word
**   RETURN:  pt: position of the right of the word
**
**********************************************************************/
CPoint CPtedTextView::WordToRight(CPoint pt)
{
	ASSERT_VALIDTEXTPOS(pt);
	int nLength = GetLineLength(pt.y);
	LPCTSTR pszChars = GetLineChars(pt.y);
	while (pt.x < nLength)
	{
		if (! isalnum(pszChars[pt.x]) && pszChars[pt.x] != _T('_'))
			break;
		pt.x ++;
	}
	ASSERT_VALIDTEXTPOS(pt);
	return pt;
}

/***********************************************************************
**
**   FUNCTION: WordToLeft(CPoint pt)
**
**       calculate the word left position
**
**   INPUT: pt: current position
**
**   OUTPUT : pt: position of the left of the word
**   RETURN:  pt: position of the left of the word
**
**********************************************************************/
CPoint CPtedTextView::WordToLeft(CPoint pt)
{
	ASSERT_VALIDTEXTPOS(pt);
	LPCTSTR pszChars = GetLineChars(pt.y);
	while (pt.x > 0)
	{
		if (! isalnum(pszChars[pt.x - 1]) && pszChars[pt.x - 1] != _T('_'))
			break;
		pt.x --;
	}
	ASSERT_VALIDTEXTPOS(pt);
	return pt;
}

/***********************************************************************
**
**   FUNCTION: SelectAll()
**
**       select all the text
**
**   INPUT: none
**
**   OUTPUT : none
**   RETURN:  none
**
**********************************************************************/
void CPtedTextView::SelectAll()
{
	int nLineCount = GetLineCount();
	m_ptCursorPos.x = GetLineLength(nLineCount - 1);
	m_ptCursorPos.y = nLineCount - 1;
	SetSelection(CPoint(0, 0), m_ptCursorPos);
	UpdateCaret();
	FlushUndoGroup();
}

/***********************************************************************
c
c   FUNCTION: OnLButtonDown(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user presses the left mouse button.
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPtedTextView::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CView::OnLButtonDown(nFlags, point);
	CPoint save_cur_pos = m_ptCursorPos;

	SetFocus();
	int linkline;
/*
.....treat 'find all' window differently
*/
	if (((CPtedWindow*)m_parent)->m_wtype==4)
	{
		AdjustTextPoint(point);
		m_ptCursorPos = ClientToText(point);
		linkline = m_pTextBuffer->GetLinkLine(m_ptCursorPos.y);
		if (linkline!=-1)
			((CPtedWindow*)m_parent)->SelParentLine(linkline);
		HighLightSelLine(m_ptCursorPos.y);
		return;
	}

	BOOL bShift = GetKeyState(VK_SHIFT) & 0x8000;
	BOOL bControl = GetKeyState(VK_CONTROL) & 0x8000;

	if (point.x < GetMarginWidth())
	{
		AdjustTextPoint(point);
		if (bControl)
		{
			SelectAll();
		}
		else
		{
			m_ptCursorPos = ClientToText(point);
			m_ptCursorPos.x = 0;			
			if (! bShift)
				m_ptAnchor = m_ptCursorPos;

			CPoint ptStart, ptEnd;
			ptStart = m_ptAnchor;
			if (ptStart.y == GetLineCount() - 1)
				ptStart.x = GetLineLength(ptStart.y);
			else
			{
				ptStart.y ++;
				ptStart.x = 0;
			}

			ptEnd = m_ptCursorPos;
			ptEnd.x = 0;

			m_ptCursorPos = ptEnd;
			UpdateCaret();
			EnsureVisible(m_ptCursorPos);
			SetSelection(ptStart, ptEnd);

			SetCapture();
			m_nDragSelTimer = SetTimer(PTED_TIMER_DRAGSEL, 100, NULL);
			ASSERT(m_nDragSelTimer != 0);
			m_bWordSelection = FALSE;
			m_bLineSelection = TRUE;
			m_bDragSelection = TRUE;
		}
	}
	else
	{
		CPoint ptText = ClientToText(point);
		PrepareSelBounds();
/*
.....not using drag and drop now
*/
//		if ((IsInsideSelBlock(ptText)) &&
//				(!m_bDisableDragAndDrop))
//		{
//			m_bPreparingToDrag = TRUE;
//		}
//		else
		{
			AdjustTextPoint(point);
			m_ptCursorPos = ClientToText(point);
			if (! bShift)
				m_ptAnchor = m_ptCursorPos;

			CPoint ptStart, ptEnd;
			if (bControl)
			{
				if (m_ptCursorPos.y < m_ptAnchor.y ||
					m_ptCursorPos.y == m_ptAnchor.y && m_ptCursorPos.x < m_ptAnchor.x)
				{
					ptStart = WordToLeft(m_ptCursorPos);
					ptEnd = WordToRight(m_ptAnchor);
				}
				else
				{
					ptStart = WordToLeft(m_ptAnchor);
					ptEnd = WordToRight(m_ptCursorPos);
				}
			}
			else
			{
				ptStart = m_ptAnchor;
				ptEnd = m_ptCursorPos;
			}

			m_ptCursorPos = ptEnd;
			UpdateCaret();
			EnsureVisible(m_ptCursorPos);
			SetSelection(ptStart, ptEnd);

			SetCapture();
			m_nDragSelTimer = SetTimer(PTED_TIMER_DRAGSEL, 100, NULL);
			ASSERT(m_nDragSelTimer != 0);
			m_bWordSelection = bControl;
			m_bLineSelection = FALSE;
			m_bDragSelection = TRUE;
		}
	}
	if (save_cur_pos!=m_ptCursorPos)
		FlushUndoGroup();
	ASSERT_VALIDTEXTPOS(m_ptCursorPos);
}

/***********************************************************************
c
c   FUNCTION: OnMouseMove(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the mouse cursor moves
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPtedTextView::OnMouseMove(UINT nFlags, CPoint point) 
{
	CView::OnMouseMove(nFlags, point);

	if (m_bDragSelection)
	{
		BOOL bOnMargin = point.x < GetMarginWidth();

		AdjustTextPoint(point);
		CPoint ptNewCursorPos = ClientToText(point);

		CPoint ptStart, ptEnd;
		if (m_bLineSelection)
		{
			if (bOnMargin)
			{
				if (ptNewCursorPos.y < m_ptAnchor.y ||
					ptNewCursorPos.y == m_ptAnchor.y && ptNewCursorPos.x < m_ptAnchor.x)
				{
					ptEnd = m_ptAnchor;
					if (ptEnd.y == GetLineCount() - 1)
					{
						ptEnd.x = GetLineLength(ptEnd.y);
					}
					else
					{
						ptEnd.y ++;
						ptEnd.x = 0;
					}
					ptNewCursorPos.x = 0;
					m_ptCursorPos = ptNewCursorPos;
				}
				else
				{
					ptEnd = m_ptAnchor;
					ptEnd.x = 0;
					m_ptCursorPos = ptNewCursorPos;
					if (ptNewCursorPos.y == GetLineCount() - 1)
					{
						ptNewCursorPos.x = GetLineLength(ptNewCursorPos.y);
					}
					else
					{
						ptNewCursorPos.y ++;
						ptNewCursorPos.x = 0;
					}
					m_ptCursorPos.x = 0;
				}
				UpdateCaret();
				SetSelection(ptNewCursorPos, ptEnd);
				return;
			}

			::SetCursor(::LoadCursor(NULL, MAKEINTRESOURCE(IDC_IBEAM)));
			m_bLineSelection = m_bWordSelection = FALSE;
		}

		if (m_bWordSelection)
		{
			if (ptNewCursorPos.y < m_ptAnchor.y ||
				ptNewCursorPos.y == m_ptAnchor.y && ptNewCursorPos.x < m_ptAnchor.x)
			{
				ptStart = WordToLeft(ptNewCursorPos);
				ptEnd = WordToRight(m_ptAnchor);
			}
			else
			{
				ptStart = WordToLeft(m_ptAnchor);
				ptEnd = WordToRight(ptNewCursorPos);
			}
		}
		else
		{
			ptStart = m_ptAnchor;
			ptEnd = ptNewCursorPos;
		}

		m_ptCursorPos = ptEnd;
		UpdateCaret();
		SetSelection(ptStart, ptEnd);
	}

/*	if (m_bPreparingToDrag)
	{
		m_bPreparingToDrag = FALSE;
		HGLOBAL hData = PrepareDragData();
		if (hData != NULL)
		{
			if (m_pTextBuffer != NULL)
				m_pTextBuffer->BeginUndoGroup();

			COleDataSource ds;
			ds.CacheGlobalData(CF_TEXT, hData);
			m_bDraggingText = TRUE;
			DROPEFFECT de = ds.DoDragDrop(GetDropEffect());
			if (de != DROPEFFECT_NONE)
				OnDropSource(de);
			m_bDraggingText = FALSE;

			if (m_pTextBuffer != NULL)
				m_pTextBuffer->FlushUndoGroup(this);
		}
	}
*/
	ASSERT_VALIDTEXTPOS(m_ptCursorPos);
}

/***********************************************************************
c
c   FUNCTION: OnLButtonUp(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when left button down
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPtedTextView::OnLButtonUp(UINT nFlags, CPoint point) 
{
	CView::OnLButtonUp(nFlags, point);
/*
.....treat 'find all' window differently
*/
	if (((CPtedWindow*)m_parent)->m_wtype==4)
	{
		return;
	}

	CPoint save_cur_pos = m_ptCursorPos;

	if (m_bDragSelection)
	{
		AdjustTextPoint(point);
		CPoint ptNewCursorPos = ClientToText(point);

		CPoint ptStart, ptEnd;
		if (m_bLineSelection)
		{
			CPoint ptEnd;
			if (ptNewCursorPos.y < m_ptAnchor.y ||
				ptNewCursorPos.y == m_ptAnchor.y && ptNewCursorPos.x < m_ptAnchor.x)
			{
				ptEnd = m_ptAnchor;
				if (ptEnd.y == GetLineCount() - 1)
				{
					ptEnd.x = GetLineLength(ptEnd.y);
				}
				else
				{
					ptEnd.y ++;
					ptEnd.x = 0;
				}
				ptNewCursorPos.x = 0;
				m_ptCursorPos = ptNewCursorPos;
			}
			else
			{
				ptEnd = m_ptAnchor;
				ptEnd.x = 0;
				if (ptNewCursorPos.y == GetLineCount() - 1)
				{
					ptNewCursorPos.x = GetLineLength(ptNewCursorPos.y);
				}
				else
				{
					ptNewCursorPos.y ++;
					ptNewCursorPos.x = 0;
				}
				m_ptCursorPos = ptNewCursorPos;
			}
			EnsureVisible(m_ptCursorPos);
			UpdateCaret();
			SetSelection(ptNewCursorPos, ptEnd);
		}
		else
		{
			if (m_bWordSelection)
			{
				if (ptNewCursorPos.y < m_ptAnchor.y ||
					ptNewCursorPos.y == m_ptAnchor.y && ptNewCursorPos.x < m_ptAnchor.x)
				{
					ptStart = WordToLeft(ptNewCursorPos);
					ptEnd = WordToRight(m_ptAnchor);
				}
				else
				{
					ptStart = WordToLeft(m_ptAnchor);
					ptEnd = WordToRight(ptNewCursorPos);
				}
			}
			else
			{
				ptStart = m_ptAnchor;
				ptEnd = m_ptCursorPos;
			}

			m_ptCursorPos = ptEnd;
			EnsureVisible(m_ptCursorPos);
			UpdateCaret();
			SetSelection(ptStart, ptEnd);
		}

		ReleaseCapture();
		KillTimer(m_nDragSelTimer);
		m_bDragSelection = FALSE;
	}

/*	if (m_bPreparingToDrag)
	{
		m_bPreparingToDrag = FALSE;

		AdjustTextPoint(point);
		m_ptCursorPos = ClientToText(point);
		EnsureVisible(m_ptCursorPos);
		UpdateCaret();
		SetSelection(m_ptCursorPos, m_ptCursorPos);
	}
*/
	if (save_cur_pos!=m_ptCursorPos)
		FlushUndoGroup();
	ASSERT_VALIDTEXTPOS(m_ptCursorPos);
}

void CPtedTextView::OnTimer(UINT_PTR nIDEvent) 
{
	CView::OnTimer(nIDEvent);
	if (nIDEvent == PTED_TIMER_DRAGSEL)
	{
		ASSERT(m_bDragSelection);
		CPoint pt;
		::GetCursorPos(&pt);
		ScreenToClient(&pt);
		CRect rcClient;
		GetClientRect(&rcClient);

		BOOL bChanged = FALSE;

		int nNewTopLine = m_nTopLine;
		int nLineCount = GetLineCount();
		if (pt.y < rcClient.top)
		{
			nNewTopLine --;
			if (pt.y < rcClient.top - GetLineHeight())
				nNewTopLine -= 2;
		}
		else
		if (pt.y >= rcClient.bottom)
		{
			nNewTopLine ++;
			if (pt.y >= rcClient.bottom + GetLineHeight())
				nNewTopLine += 2;
		}

		if (nNewTopLine < 0)
			nNewTopLine = 0;
		if (nNewTopLine >= nLineCount)
			nNewTopLine = nLineCount - 1;

		if (m_nTopLine != nNewTopLine)
		{
			ScrollToLine(nNewTopLine);
			bChanged = TRUE;
		}

		int nNewOffsetChar = m_nOffsetChar;
		int nMaxLineLength = GetMaxLineLength();
		if (pt.x < rcClient.left)
			nNewOffsetChar --;
		else
		if (pt.x >= rcClient.right)
			nNewOffsetChar ++;

		if (nNewOffsetChar >= nMaxLineLength)
			nNewOffsetChar = nMaxLineLength - 1;
		if (nNewOffsetChar < 0)
			nNewOffsetChar = 0;

		if (m_nOffsetChar != nNewOffsetChar)
		{
			ScrollToChar(nNewOffsetChar);
			UpdateCaret();
			bChanged = TRUE;
		}

		if (bChanged)
		{
			AdjustTextPoint(pt);
			CPoint ptNewCursorPos = ClientToText(pt);
			if (ptNewCursorPos != m_ptCursorPos)
			{
				m_ptCursorPos = ptNewCursorPos;
				UpdateCaret();
				FlushUndoGroup();
			}
			SetSelection(m_ptAnchor, m_ptCursorPos);
		}
	}
}

/***********************************************************************
c
c   FUNCTION: OnLButtonDblClk(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when left button double click
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPtedTextView::OnLButtonDblClk(UINT nFlags, CPoint point) 
{
	CView::OnLButtonDblClk(nFlags, point);
	int linkline;
/*
.....treat 'find all' window differently
*/
	if (((CPtedWindow*)m_parent)->m_wtype==4)
	{
		AdjustTextPoint(point);
		m_ptCursorPos = ClientToText(point);
		linkline = m_pTextBuffer->GetLinkLine(m_ptCursorPos.y);
		if (linkline!=-1)
			((CPtedWindow*)m_parent)->SelParentLine(linkline);
		HighLightSelLine(m_ptCursorPos.y);
		return;
	}

	if (! m_bDragSelection)
	{
		AdjustTextPoint(point);

		m_ptCursorPos = ClientToText(point);
		m_ptAnchor = m_ptCursorPos;

		CPoint ptStart, ptEnd;
		if (m_ptCursorPos.y < m_ptAnchor.y ||
			m_ptCursorPos.y == m_ptAnchor.y && m_ptCursorPos.x < m_ptAnchor.x)
		{
			ptStart = WordToLeft(m_ptCursorPos);
			ptEnd = WordToRight(m_ptAnchor);
		}
		else
		{
			ptStart = WordToLeft(m_ptAnchor);
			ptEnd = WordToRight(m_ptCursorPos);
		}

		m_ptCursorPos = ptEnd;
		UpdateCaret();
		EnsureVisible(m_ptCursorPos);
		SetSelection(ptStart, ptEnd);

		SetCapture();
		m_nDragSelTimer = SetTimer(PTED_TIMER_DRAGSEL, 100, NULL);
		ASSERT(m_nDragSelTimer != 0);
		m_bWordSelection = TRUE;
		m_bLineSelection = FALSE;
		m_bDragSelection = TRUE;
	}
	FlushUndoGroup();
}

void CPtedTextView::OnEditSelectAll() 
{
	SelectAll();
}

/***********************************************************************
c
c   FUNCTION: OnRButtonDown(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when right button down
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPtedTextView::OnRButtonDown(UINT nFlags, CPoint point) 
{
	CPoint pt = point;
	AdjustTextPoint(pt);
	pt = ClientToText(pt);
	if (! IsInsideSelBlock(pt))
	{
		m_ptAnchor = m_ptCursorPos = pt;
		SetSelection(m_ptCursorPos, m_ptCursorPos);
		EnsureVisible(m_ptCursorPos);
		UpdateCaret();
	}
	CMenu* pmenu = m_parent->GetMenu();
	CMenu* emenu = pmenu->GetSubMenu(1);
	UINT fid, mstate;
	CString menuString;
	int i, menunum;
	if (m_textpopup==NULL)
	{
		m_textpopup = new CMenu();
		m_textpopup->CreatePopupMenu();
		if (((CPtedWindow*)m_parent)->GetUndo_Menu()) menunum = 11;
		else menunum = 8;
		for (i=0; i<menunum; i++)
		{
			fid = emenu->GetMenuItemID(i);
			mstate = emenu->GetMenuState(i, MF_BYPOSITION);
			emenu->GetMenuString(i, menuString, MF_BYPOSITION);
			if (menuString!="")
				m_textpopup->AppendMenu(MF_STRING|mstate|MF_POPUP, 
									fid, menuString);
			else
				m_textpopup->AppendMenu(MF_SEPARATOR, 
									fid, menuString);
		}
	}
	::GetCursorPos(&pt);
	m_textpopup->TrackPopupMenu(TPM_LEFTALIGN,
		pt.x, pt.y, m_parent, NULL);
	delete m_textpopup;
	m_textpopup = NULL;

	CView::OnRButtonDown(nFlags, point);
}

/***********************************************************************
**
**   FUNCTION: IsSelection()
**
**       See if there is a selection
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    true if selected
**
**********************************************************************/
BOOL CPtedTextView::IsSelection()
{
	return m_ptSelStart != m_ptSelEnd;
}

/***********************************************************************
**
**   FUNCTION: Copy()
**
**       copy the select text
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::Copy()
{
	if (m_ptSelStart == m_ptSelEnd)
		return;

	PrepareSelBounds();
	CString text;
	GetText(m_ptDrawSelStart, m_ptDrawSelEnd, text);
	PutToClipboard(text);
}

/***********************************************************************
**
**   FUNCTION: PutToClipboard(LPCTSTR pszText)
**
**       Put the text To Clipboard
**
**   INPUT:  pszText: text to be put in clipboard
**
**   OUTPUT :   none
**   RETURN:   none
**
**********************************************************************/
BOOL CPtedTextView::PutToClipboard(LPCTSTR pszText)
{
	if (pszText == NULL || lstrlen(pszText) == 0)
		return FALSE;

	CWaitCursor wc;
	BOOL bOK = FALSE;
	if (OpenClipboard())
	{
		EmptyClipboard();
		HGLOBAL hData = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, lstrlen(pszText) + 1);
		if (hData != NULL)
		{
			LPSTR pszData = (LPSTR) ::GlobalLock(hData);
			USES_CONVERSION;
			strcpy(pszData, T2A((LPTSTR) pszText));
			GlobalUnlock(hData);
			bOK = SetClipboardData(CF_TEXT, hData) != NULL;
		}
		CloseClipboard();
	}
	return bOK;
}

/***********************************************************************
**
**   FUNCTION: GetFromClipboard(LPCTSTR pszText)
**
**       Get the text from Clipboard
**
**   INPUT:  pszText: text to get from clipboard
**
**   OUTPUT :   none
**   RETURN:   none
**
**********************************************************************/
BOOL CPtedTextView::GetFromClipboard(CString &text)
{
	BOOL bSuccess = FALSE;
	if (OpenClipboard())
	{
		HGLOBAL hData = GetClipboardData(CF_TEXT);
		if (hData != NULL)
		{
			LPSTR pszData = (LPSTR) GlobalLock(hData);
			if (pszData != NULL)
			{
				text = pszData;
				GlobalUnlock(hData);
				bSuccess = TRUE;
			}
		}
		CloseClipboard();
	}
	return bSuccess;
}

/***********************************************************************
**
**   FUNCTION: PrepareDragData()
**
**       
**
**   INPUT:  
**
**   OUTPUT :   none
**   RETURN:   none
**
**********************************************************************/
HGLOBAL CPtedTextView::PrepareDragData()
{
	PrepareSelBounds();
	if (m_ptDrawSelStart == m_ptDrawSelEnd)
		return NULL;

	CString text;
	GetText(m_ptDrawSelStart, m_ptDrawSelEnd, text);
	HGLOBAL hData = ::GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, lstrlen(text) + 1);
	if (hData == NULL)
		return NULL;

	LPSTR pszData = (LPSTR) ::GlobalLock(hData);
	USES_CONVERSION;
	strcpy(pszData, T2A(text.GetBuffer(0)));
	text.ReleaseBuffer();
	::GlobalUnlock(hData);

	m_ptDraggedTextBegin = m_ptDrawSelStart;
	m_ptDraggedTextEnd = m_ptDrawSelEnd;
	return hData;
}
