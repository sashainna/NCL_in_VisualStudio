/************************************************************************
c
c   FILE NAME: PtedTextView4.cpp
c
c   CONTAINS:
c		CPtedTextView::Paste()
c		CPtedTextView::Cut()
c		CPtedTextView::OnEditDelete() 
c								BOOL bWrapSearch, CPoint *pptFoundPos)
c		CPtedTextView::FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPosition, 
c									   CPoint &ptBlockBegin, CPoint &ptBlockEnd,
c										DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos)
c		CPtedTextView::OnEditInsertLine() 
c		CPtedTextView::OnEditDeleteLine() 
c		CPtedTextView::OnEditDeleteBack() 
c		CPtedTextView::OnEditTab() 
c		CPtedTextView::OnEditUnTab() 
c		CPtedTextView::OnEditSwitchOvrmode() 
c		CPtedTextView::ReplaceSelection(LPCTSTR pszNewText)
c		CPtedTextView::OnEditUndo() 
c		CPtedTextView::OnEditRedo() 
c		CPtedTextView::SetDisableBSAtSOL(BOOL bDisableBSAtSOL)
c		CPtedTextView::OnEditOperation(int nAction, LPCTSTR pszText)
c		CPtedTextView::OnEditReverse() 
c		CPtedTextView::GetUndoDescription(CString &desc, POSITION pos)
c		CPtedTextView::GetRedoDescription(CString &desc, POSITION pos)
c		CPtedTextView::IsModified() const
c		CPtedTextView::GetSelected_FindText(CString& strResult)
c		CPtedTextView::FindTextInRange(LPCTSTR lpszFind, CPoint &ptStart, CPoint &ptEnd,
c                                BOOL bNext, BOOL bCase))
c		CPtedTextView::FindStrAddrInRange(LPCTSTR lpszFind, char *addr, CPoint &ptStart, CPoint &ptEnd,
c		CPtedTextView::Get_Range_Pos(PtedRangeStruct* cRange, 
c		CPoint &ptStart, CPoint & ptEnd)
c		CPtedTextView::FindStrAddrInBlock(LPCTSTR findstr, char*FindAddr,  CPoint &ptCurrentPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, int *flen, double *fval, CPoint *pptFoundPos)
c		CPtedTextView::FindAll(CString strFind, DWORD dwFlags, int letter, CPoint &ptStart, CPoint &ptEnd, CPtedTextView *textview)
c		CPtedTextView::FindStrAdds(LPCTSTR lpszFind, char *adds, CPoint &ptStart, CPoint &ptEnd, BOOL bNext, BOOL bCase,
c						int *fadd, double* fval)
c		CPtedTextView::ReplaceAll(LPCTSTR lpszFind, LPCTSTR lpszReplace, BOOL bCase,
c			int fletter, CPoint &ptStart, CPoint &ptEnd, int vflag)
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c	MODULE NAME AND RELEASE LEVEL
c       PtedTextView5.cpp , 24.1
c	DATE AND TIME OF LAST  MODIFICATION
c       09/11/13 , 12:59:29
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include <malloc.h>
#include "PtedEditCmd.h"
#include "PtedTextView.h"
#include "PtedTextBuffer.h"
#include "PtedWindow.h"
#include "PtdFunc.h"
#include <afxpriv.h>
//#include <afximpl.h>

extern "C" int Ptd_FindStringInTextLine(char* pszFindWhere, char* pszFindWhat, int bWholeWord);
extern "C" int Ptd_FindAddrInTextLine(char* pszFindWhere, char* pszFindAddr, int* flen, double *fval,int *fpos);
extern "C" int Ptd_FindAddrInTextLine2(char* pszFindWhere, int kreg, double gval, int regonly, int* flen, double *fval,int *fpos);
extern "C" void Pted_disply_ProcessWindow(char *title, CPtedWindow *parent);
extern "C" void Pted_Display_as_percent(int num, CPtedWindow* parent);
extern "C" void Pted_Close_ProcessWindow(CPtedWindow *parent);

/***********************************************************************
c
c   SUBROUTINE:  Paste
c
c   FUNCTION:  This function paste the text from Clipboard 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::Paste()
{
	if (! QueryEditable())
		return;
	if (m_pTextBuffer == NULL)
		return;
	m_pTextBuffer->BeginUndoGroup();

	DeleteCurrentSelection();

	CString text;
	if (GetFromClipboard(text))
	{
		CPoint ptCursorPos = GetCursorPos();
		ASSERT_VALIDTEXTPOS(ptCursorPos);
		int x, y;
		m_pTextBuffer->InsertText(ptCursorPos.y, ptCursorPos.x, text, y, x, PTED_PASTE); 
		ptCursorPos.x = x;
		ptCursorPos.y = y;
		ASSERT_VALIDTEXTPOS(ptCursorPos);
		SetAnchor(ptCursorPos);
		SetSelection(ptCursorPos, ptCursorPos);
		SetCursorPos(ptCursorPos);
		EnsureVisible(ptCursorPos);
	}
	m_pTextBuffer->FlushUndoGroup(this);
}

/***********************************************************************
c
c   SUBROUTINE:  Cut
c
c   FUNCTION:  This function Cut the text and save into Clipboard 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::Cut()
{
	if (! QueryEditable())
		return;
	if (m_pTextBuffer == NULL)
		return;
	if (! IsSelection())
		return;

	CPoint ptSelStart, ptSelEnd;
	GetSelection(ptSelStart, ptSelEnd);
	CString text;
	GetText(ptSelStart, ptSelEnd, text);
	PutToClipboard(text);

	CPoint ptCursorPos = ptSelStart;
	ASSERT_VALIDTEXTPOS(ptCursorPos);
	SetAnchor(ptCursorPos);
	SetSelection(ptCursorPos, ptCursorPos);
	SetCursorPos(ptCursorPos);
	EnsureVisible(ptCursorPos);

	m_pTextBuffer->DeleteText(ptSelStart.y, ptSelStart.x, ptSelEnd.y, ptSelEnd.x, PTED_CUT); 
}

/***********************************************************************
c
c   SUBROUTINE:  OnEditDelete
c
c   FUNCTION:  This function delete the currect select text 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditDelete() 
{
	if (! QueryEditable() || m_pTextBuffer == NULL)
		return;

	CPoint ptSelStart, ptSelEnd;
	GetSelection(ptSelStart, ptSelEnd);
	if (ptSelStart == ptSelEnd)
	{
		if (ptSelEnd.x == GetLineLength(ptSelEnd.y))
		{
			if (ptSelEnd.y == GetLineCount() - 1)
				return;
			ptSelEnd.y ++;
			ptSelEnd.x = 0;
		}
		else
			ptSelEnd.x ++;
	}

	CPoint ptCursorPos = ptSelStart;
	ASSERT_VALIDTEXTPOS(ptCursorPos);
	SetAnchor(ptCursorPos);
	SetSelection(ptCursorPos, ptCursorPos);
	SetCursorPos(ptCursorPos);
	EnsureVisible(ptCursorPos);

	m_pTextBuffer->DeleteText(ptSelStart.y, ptSelStart.x, ptSelEnd.y, ptSelEnd.x, PTED_DELETE); 
	((CPtedWindow*)m_parent)->Update_undo_redomenu(1, 0);
	RedrawWindow();

}

/***********************************************************************
c
c   SUBROUTINE:  FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPosition,
c					CPoint &ptBlockBegin, CPoint &ptBlockEnd, 
c					DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos)
c
c   FUNCTION:  This function find the text in specified range
c
c   INPUT:  pszText: text string to find
c			ptStartPosition: start searching position
c           ptBlockBegin, ptBlockEnd:    Range to search
c			dwFlags: search flag
c           bWrapSearch:    if search wrapped
c   OUTPUT: pptFoundPos: found position
c   RETURN: true if found
c
c***********************************************************************
*/
BOOL CPtedTextView::FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPosition, 
									   CPoint &ptBlockBegin, CPoint &ptBlockEnd,
										DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos)
{
	CPoint ptCurrentPos = ptStartPosition;

	ASSERT(pszText != NULL && lstrlen(pszText) > 0);
	ASSERT_VALIDTEXTPOS(ptCurrentPos);
	ASSERT_VALIDTEXTPOS(ptBlockBegin);
	ASSERT_VALIDTEXTPOS(ptBlockEnd);
	ASSERT(ptBlockBegin.y < ptBlockEnd.y || ptBlockBegin.y == ptBlockEnd.y && 
		ptBlockBegin.x <= ptBlockEnd.x);
	if (ptBlockBegin == ptBlockEnd)
		return FALSE;

	return m_pTextBuffer->FindTextInBlock(pszText, ptStartPosition, ptBlockBegin, ptBlockEnd,
									dwFlags, bWrapSearch, pptFoundPos);
}
/***********************************************************************
c
c   SUBROUTINE: OnEditInsertLine  
c
c   FUNCTION:  Insert a empty line in front of the current line 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditInsertLine() 
{
	if (! QueryEditable())
		return;
	if (m_pTextBuffer == NULL)
		return;

	m_pTextBuffer->BeginUndoGroup();
	DeleteCurrentSelection();

	const static TCHAR pszText[3] = _T("\r\n");

	CPoint ptCursorPos = GetCursorPos();
	int line_len = -1;
	if (ptCursorPos.y>0)
	{
		ptCursorPos.y--;
	}
	else
		line_len = 0;

	ASSERT_VALIDTEXTPOS(ptCursorPos);
	if (line_len==-1)
		line_len = GetLineLength(ptCursorPos.y);

	int x, y;
	m_pTextBuffer->InsertText(ptCursorPos.y, line_len, pszText, y, x, PTED_INSERT);
	ptCursorPos.x = x;
	ptCursorPos.y = y;
	ASSERT_VALIDTEXTPOS(ptCursorPos);
	SetAnchor(ptCursorPos);
	SetSelection(ptCursorPos, ptCursorPos);
	SetCursorPos(ptCursorPos);
	EnsureVisible(ptCursorPos);
	m_pTextBuffer->FlushUndoGroup(this);
	((CPtedWindow*)m_parent)->Update_undo_redomenu(1,0);
	RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE: OnEditDeleteLine  
c
c   FUNCTION:  delete current text line
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditDeleteLine() 
{
	if (! QueryEditable() || m_pTextBuffer == NULL)
		return;
	CPoint ptCursorPos = GetCursorPos();
	CPoint ptCurrentCursorPos = ptCursorPos;
	int line_len = GetLineLength(ptCursorPos.y);

	if ((ptCursorPos.x == 0) && (ptCursorPos.y == 0))
		return;
	ptCursorPos.y --;
	if (ptCursorPos.y<0)
	{
		ptCursorPos.y = 0;
		ptCursorPos.x = 0;
		m_pTextBuffer->DeleteText(0, 0, 0, line_len, PTED_DELETE); 
	}
	else
	{
		ptCursorPos.x = GetLineLength(ptCursorPos.y);
		m_pTextBuffer->DeleteText(ptCursorPos.y, ptCursorPos.x, ptCurrentCursorPos.y, line_len, PTED_DELETE); 
	}
	SetAnchor(ptCursorPos);
	SetSelection(ptCursorPos, ptCursorPos);
	SetCursorPos(ptCursorPos);
	EnsureVisible(ptCursorPos);
	((CPtedWindow*)m_parent)->Update_undo_redomenu(1,0);
	RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE: OnEditDeleteBack  
c
c   FUNCTION:  delete selection or back delete one character
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditDeleteBack() 
{
	if (IsSelection())
	{
		OnEditDelete();
		return;
	}

	if (! QueryEditable() || m_pTextBuffer == NULL)
		return;

	CPoint ptCursorPos = GetCursorPos();
	CPoint ptCurrentCursorPos = ptCursorPos;
	bool	bDeleted = false;

	if( !( ptCursorPos.x ) )	
	{
		if( !m_bDisableBSAtSOL )
		{
			if( ptCursorPos.y > 0 )
			{
				ptCursorPos.y--;
				ptCursorPos.x = GetLineLength(
						ptCursorPos.y );
				bDeleted = true;
			}
		}
	}
	else
	{
		ptCursorPos.x--;
		bDeleted = true;
	}
	ASSERT_VALIDTEXTPOS(ptCursorPos);
	SetAnchor(ptCursorPos);
	SetSelection(ptCursorPos, ptCursorPos);
	SetCursorPos(ptCursorPos);
	EnsureVisible(ptCursorPos);

	if (bDeleted)
	{
		m_pTextBuffer->DeleteText(ptCursorPos.y, ptCursorPos.x, ptCurrentCursorPos.y, ptCurrentCursorPos.x, PTED_DELETE); 
	}
	((CPtedWindow*)m_parent)->Update_undo_redomenu(1, 0);
	RedrawWindow();
	return;
}

/***********************************************************************
c
c   SUBROUTINE: OnEditTab  
c
c   FUNCTION:  Called when type 'Tab'
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditTab() 
{
	if (! QueryEditable() || m_pTextBuffer == NULL)
		return;

	BOOL bTabify = FALSE;
	CPoint ptSelStart, ptSelEnd;
	if (IsSelection())
	{
		GetSelection(ptSelStart, ptSelEnd);
		bTabify = ptSelStart.y != ptSelEnd.y;
	}

	if (bTabify)
	{
		m_pTextBuffer->BeginUndoGroup();

		int nStartLine = ptSelStart.y;
		int nEndLine = ptSelEnd.y;
		ptSelStart.x = 0;
		if (ptSelEnd.x > 0)
		{
			if (ptSelEnd.y == GetLineCount() - 1)
			{
				ptSelEnd.x = GetLineLength(ptSelEnd.y);
			}
			else
			{
				ptSelEnd.x = 0;
				ptSelEnd.y ++;
			}
		}
		else
			nEndLine --;
		SetSelection(ptSelStart, ptSelEnd);
		SetCursorPos(ptSelEnd);
		EnsureVisible(ptSelEnd);

		m_bHorzScrollBarLocked = TRUE;
		static const TCHAR pszText[] = _T("\t");
		for (int L = nStartLine; L <= nEndLine; L ++)
		{
			int x, y;
			m_pTextBuffer->InsertText(L, 0, pszText, y, x, PTED_INDENT); 
		}
		m_bHorzScrollBarLocked = FALSE;
		RecalcHorzScrollBar();

		m_pTextBuffer->FlushUndoGroup(this);
		RedrawWindow();
		((CPtedWindow*)m_parent)->Update_undo_redomenu(1, 0);
		return;
	}

	if (m_bOvrMode)
	{
		CPoint ptCursorPos = GetCursorPos();
		ASSERT_VALIDTEXTPOS(ptCursorPos);

		int nLineLength = GetLineLength(ptCursorPos.y);
		LPCTSTR pszLineChars = GetLineChars(ptCursorPos.y);
		if (ptCursorPos.x < nLineLength)
		{
			int nTabSize = GetTabSize();
			int nChars = nTabSize - CalculateActualOffset(ptCursorPos.y, ptCursorPos.x) % nTabSize;
			ASSERT(nChars > 0 && nChars <= nTabSize);

			while (nChars > 0)
			{
				if (ptCursorPos.x == nLineLength)
					break;
				if (pszLineChars[ptCursorPos.x] == _T('\t'))
				{
					ptCursorPos.x ++;
					break;
				}
				ptCursorPos.x ++;
				nChars --;
			}
			ASSERT(ptCursorPos.x <= nLineLength);
			ASSERT_VALIDTEXTPOS(ptCursorPos);

			SetSelection(ptCursorPos, ptCursorPos);
			SetAnchor(ptCursorPos);
			SetCursorPos(ptCursorPos);
			EnsureVisible(ptCursorPos);
			RedrawWindow();
			((CPtedWindow*)m_parent)->Update_undo_redomenu(1, 0);
			return;
		}
	}

	m_pTextBuffer->BeginUndoGroup();

	DeleteCurrentSelection();

	CPoint ptCursorPos = GetCursorPos();
	ASSERT_VALIDTEXTPOS(ptCursorPos);

	static const TCHAR pszText[] = _T("\t");
	int x, y;
	m_pTextBuffer->InsertText(ptCursorPos.y, ptCursorPos.x, pszText, y, x, PTED_TYPING);
	ptCursorPos.x = x;
	ptCursorPos.y = y;
	ASSERT_VALIDTEXTPOS(ptCursorPos);
	SetSelection(ptCursorPos, ptCursorPos);
	SetAnchor(ptCursorPos);
	SetCursorPos(ptCursorPos);
	EnsureVisible(ptCursorPos);

	m_pTextBuffer->FlushUndoGroup(this);
	RedrawWindow();
	((CPtedWindow*)m_parent)->Update_undo_redomenu(1, 0);
}
/***********************************************************************
c
c   SUBROUTINE: OnEditTab  
c
c   FUNCTION:  Called when type 'Tab' with shift
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditUntab() 
{
	if (! QueryEditable() || m_pTextBuffer == NULL)
		return;

	BOOL bTabify = FALSE;
	CPoint ptSelStart, ptSelEnd;
	if (IsSelection())
	{
		GetSelection(ptSelStart, ptSelEnd);
		bTabify = ptSelStart.y != ptSelEnd.y;
	}

	if (bTabify)
	{
		m_pTextBuffer->BeginUndoGroup();

		CPoint ptSelStart, ptSelEnd;
		GetSelection(ptSelStart, ptSelEnd);
		int nStartLine = ptSelStart.y;
		int nEndLine = ptSelEnd.y;
		ptSelStart.x = 0;
		if (ptSelEnd.x > 0)
		{
			if (ptSelEnd.y == GetLineCount() - 1)
			{
				ptSelEnd.x = GetLineLength(ptSelEnd.y);
			}
			else
			{
				ptSelEnd.x = 0;
				ptSelEnd.y ++;
			}
		}
		else
			nEndLine --;
		SetSelection(ptSelStart, ptSelEnd);
		SetCursorPos(ptSelEnd);
		EnsureVisible(ptSelEnd);

		m_bHorzScrollBarLocked = TRUE;
		for (int L = nStartLine; L <= nEndLine; L ++)
		{
			int nLength = GetLineLength(L);
			if (nLength > 0)
			{
				LPCTSTR pszChars = GetLineChars(L);
				int nPos = 0, nOffset = 0;
				while (nPos < nLength)
				{
					if (pszChars[nPos] == _T(' '))
					{
						nPos ++;
						if (++ nOffset >= GetTabSize())
							break;
					}
					else
					{
						if (pszChars[nPos] == _T('\t'))
							nPos ++;
						break;
					}
				}

				if (nPos > 0)
					m_pTextBuffer->DeleteText(L, 0, L, nPos, PTED_INDENT); 
			}
		}
		m_bHorzScrollBarLocked = FALSE;
		RecalcHorzScrollBar();

		m_pTextBuffer->FlushUndoGroup(this);
	}
	else
	{
		CPoint ptCursorPos = GetCursorPos();
		ASSERT_VALIDTEXTPOS(ptCursorPos);
		if (ptCursorPos.x > 0)
		{
			int nTabSize = GetTabSize();
			int nOffset = CalculateActualOffset(ptCursorPos.y, ptCursorPos.x);
			int nNewOffset = nOffset / nTabSize * nTabSize;
			if (nOffset == nNewOffset && nNewOffset > 0)
				nNewOffset -= nTabSize;
			ASSERT(nNewOffset >= 0);

			LPCTSTR pszChars = GetLineChars(ptCursorPos.y);
			int nCurrentOffset = 0;
			int I = 0;
			while (nCurrentOffset < nNewOffset)
			{
				if (pszChars[I] == _T('\t'))
					nCurrentOffset = nCurrentOffset / nTabSize * nTabSize + nTabSize;
				else
					nCurrentOffset ++;
				I ++;
			}

			ASSERT(nCurrentOffset == nNewOffset);

			ptCursorPos.x = I;
			ASSERT_VALIDTEXTPOS(ptCursorPos);
			SetSelection(ptCursorPos, ptCursorPos);
			SetAnchor(ptCursorPos);
			SetCursorPos(ptCursorPos);
			EnsureVisible(ptCursorPos);
		}
	}
	((CPtedWindow*)m_parent)->Update_undo_redomenu(1, 0);
	RedrawWindow();
}


/***********************************************************************
c
c   SUBROUTINE: OnEditSwitchOvrmode  
c
c   FUNCTION:  switched 'overwrite' mode
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditSwitchOvrmode() 
{
	m_bOvrMode = ! m_bOvrMode;
}

/***********************************************************************
c
c   SUBROUTINE: ReplaceSelection(LPCTSTR pszNewText)  
c
c   FUNCTION:  replace the current selection with new text
c
c   INPUT:  pszNewText: new text to be used to replace selection
c			
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CPtedTextView::ReplaceSelection(LPCTSTR pszNewText)
{
	ASSERT(pszNewText != NULL);
	if (! IsSelection())
		return FALSE;

	DeleteCurrentSelection();

	CPoint ptCursorPos = GetCursorPos();
	ASSERT_VALIDTEXTPOS(ptCursorPos);
	int x, y;
	m_pTextBuffer->InsertText(ptCursorPos.y, ptCursorPos.x, pszNewText, y, x, PTED_REPLACE);
	CPoint ptEndOfBlock = CPoint(x, y);
	ASSERT_VALIDTEXTPOS(ptCursorPos);
	ASSERT_VALIDTEXTPOS(ptEndOfBlock);
	SetAnchor(ptEndOfBlock);
	SetSelection(ptCursorPos, ptEndOfBlock);
	SetCursorPos(ptEndOfBlock);
	EnsureVisible(ptEndOfBlock);
	return TRUE;
}
/***********************************************************************
c
c   SUBROUTINE: OnEditUndo()  
c
c   FUNCTION:  Undo the last command
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditUndo() 
{
	if (m_pTextBuffer != NULL && m_pTextBuffer->CanUndo())
	{
		CPoint ptCursorPos;
		if (m_pTextBuffer->Undo(ptCursorPos))
		{
			ASSERT_VALIDTEXTPOS(ptCursorPos);
			SetAnchor(ptCursorPos);
			SetSelection(ptCursorPos, ptCursorPos);
			SetCursorPos(ptCursorPos);
			EnsureVisible(ptCursorPos);
		}
	}
	((CPtedWindow*)m_parent)->Update_undo_redomenu(1, 1);
	RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE: SetDisableBSAtSOL(BOOL bDisableBSAtSOL)
c
c   FUNCTION:  Diable the back space at the start of the line
c
c   INPUT:  bDisableBSAtSOL: flag to be set
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::SetDisableBSAtSOL(BOOL bDisableBSAtSOL)
{
	m_bDisableBSAtSOL = bDisableBSAtSOL;
}

/***********************************************************************
c
c   SUBROUTINE: OnEditRedo
c
c   FUNCTION:  Redo the last command
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditRedo() 
{
	if (m_pTextBuffer != NULL && m_pTextBuffer->CanRedo())
	{
		CPoint ptCursorPos;
		if (m_pTextBuffer->Redo(ptCursorPos))
		{
			ASSERT_VALIDTEXTPOS(ptCursorPos);
			SetAnchor(ptCursorPos);
			SetSelection(ptCursorPos, ptCursorPos);
			SetCursorPos(ptCursorPos);
			EnsureVisible(ptCursorPos);
		}
	}
	((CPtedWindow*)m_parent)->Update_undo_redomenu(1, 1);
	RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE: OnEditOperation(int nAction, LPCTSTR pszText)
c
c   FUNCTION:  operation when m_bAutoIndent is on
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditOperation(int nAction, LPCTSTR pszText)
{
	if (m_bAutoIndent)
	{
		if (nAction == PTED_TYPING && _tcscmp(pszText, _T("\r\n")) == 0 && ! m_bOvrMode)
		{
			CPoint ptCursorPos = GetCursorPos();
			ASSERT(ptCursorPos.y > 0);

			int nLength = m_pTextBuffer->GetLineLength(ptCursorPos.y - 1);
			LPCTSTR pszLineChars = m_pTextBuffer->GetLineChars(ptCursorPos.y - 1);
			int nPos = 0;
			while (nPos < nLength && isspace(pszLineChars[nPos]))
				nPos ++;

			if (nPos > 0)
			{
				TCHAR *pszInsertStr = (TCHAR *) _alloca(sizeof(TCHAR) * (nLength + 1));
				_tcsncpy(pszInsertStr, pszLineChars, nPos);
				pszInsertStr[nPos] = 0;

				int x, y;
				m_pTextBuffer->InsertText(ptCursorPos.y, ptCursorPos.x,
															pszInsertStr, y, x, PTED_AUTOINDENT);
				CPoint pt(x, y);
				SetCursorPos(pt);
				SetSelection(pt, pt);
				SetAnchor(pt);
				EnsureVisible(pt);
			}
		}
	}
}


/***********************************************************************
c
c   SUBROUTINE: OnEditReverse
c
c   FUNCTION:  Reverse the current text
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnEditReverse() 
{
	int i;
	DWORD flags = TED_RANGE_BEGIN1 | TED_RANGE_BEGIN6 | TED_RANGE_BEGIN7
				| TED_RANGE_BEGIN2 | TED_RANGE_BEGIN3 | TED_RANGE_END5
				| TED_RANGE_BEGIN4 | TED_RANGE_BEGIN5 | TED_RANGE_END1
				| TED_RANGE_END2 | TED_RANGE_END3 | TED_RANGE_END4;


	PtedRangeStruct cRange;
		
	if (IsSelection())
		cRange.begin = 2;
	else
		cRange.begin = 1;

	cRange.end = 1;
	cRange.baddress[0] = '\0';
	cRange.bstring[0] = '\0';
	cRange.enumber[0] = '\0';
	cRange.eaddress[0] = '\0';
	cRange.estring[0] = '\0';

	PtedRangeBox *RangeBox = new PtedRangeBox(this, flags, &cRange);
	if (RangeBox->DoModal()==IDCANCEL)
	{
		delete RangeBox;
		return;
	}

	if (RangeBox->m_begin1==1)
		cRange.begin = 1;
	if (RangeBox->m_begin2==1)
		cRange.begin = 2;
	if (RangeBox->m_begin3==1)
		cRange.begin = 3;
	if (RangeBox->m_begin4==1)
		cRange.begin = 4;
	if (RangeBox->m_begin5==1)
		cRange.begin = 5;
	if (RangeBox->m_begin6==1)
		cRange.begin = 6;
	if (RangeBox->m_begin7==1)
		cRange.begin = 7;
	if (RangeBox->m_end1==1)
		cRange.end = 1;
	if (RangeBox->m_end2==1)
		cRange.end = 2;
	if (RangeBox->m_end3==1)
		cRange.end = 3;
	if (RangeBox->m_end4==1)
		cRange.end = 4;
	if (RangeBox->m_end5==1)
		cRange.end = 5;
	
	if (RangeBox->m_estring!="")
	{
		for (i=0; i<(RangeBox->m_estring).GetLength(); i++)
			cRange.estring[i] = RangeBox->m_estring[i];
		cRange.estring[i] = '\0';
	}
	if (RangeBox->m_bstring!="")
	{
		for (i=0; i<(RangeBox->m_bstring).GetLength(); i++)
			cRange.bstring[i] = RangeBox->m_bstring[i];
		cRange.bstring[i] = '\0';
	}
	if (RangeBox->m_enumber!="")
	{
		for (i=0; i<(RangeBox->m_enumber).GetLength(); i++)
			cRange.enumber[i] = RangeBox->m_enumber[i];
		cRange.enumber[i]= '\0';	
	}
	if (RangeBox->m_eaddress!="")
	{
		for (i=0; i<(RangeBox->m_eaddress).GetLength(); i++)
			cRange.eaddress[i] = RangeBox->m_eaddress[i];
		cRange.eaddress[i] = '\0';
	}
	if (RangeBox->m_baddress!="")
	{
		for (i=0; i<(RangeBox->m_baddress).GetLength(); i++)
			cRange.baddress[i] = RangeBox->m_baddress[i];
		cRange.baddress[i] = '\0';
	}
	cRange.bline = RangeBox->m_bline;
	cRange.eline = RangeBox->m_eline;
/*
.....set wait cursor
*/
	CWaitCursor wait;
	CPoint ptStart, ptEnd;
	Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
		m_pTextBuffer->ReverseRange(ptStart, ptEnd);
	delete RangeBox;
	RedrawWindow();
	if (ptStart!=ptEnd)
		((CPtedWindow*)m_parent)->Update_undo_redomenu(1, 0);
}
/***********************************************************************
c
c   SUBROUTINE: GetUndoDescription
c
c   FUNCTION:  Get the undo command description
c
c   INPUT:  pos: command index
c			
c   OUTPUT: desc: undo command description
c	RETURN: undo position
c
c***********************************************************************
*/
POSITION CPtedTextView::GetUndoDescription(CString &desc, POSITION pos /*= NULL*/)
{
	BOOL bCanUndo = m_pTextBuffer != NULL && m_pTextBuffer->CanUndo();
	if (bCanUndo)
		return m_pTextBuffer->GetUndoDescription(desc, pos);
	else
		return (POSITION)(-1);
}

/***********************************************************************
c
c   SUBROUTINE: GetRedoDescription
c
c   FUNCTION:  Get the redo command description
c
c   INPUT:  pos: command index
c			
c   OUTPUT: desc: redo command description
c	RETURN: redo position
c
c***********************************************************************
*/
POSITION CPtedTextView::GetRedoDescription(CString &desc, POSITION pos /*= NULL*/)
{
	BOOL bCanRedo = m_pTextBuffer != NULL && m_pTextBuffer->CanRedo();
	if (bCanRedo)		
		return m_pTextBuffer->GetRedoDescription(desc, pos);
	else
		return (POSITION)(-1);
}

/***********************************************************************
c
c   SUBROUTINE: IsModified()
c
c   FUNCTION:  if the text view is modified
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CPtedTextView::IsModified() const
{
	return m_pTextBuffer->IsModified();
}
/***********************************************************************
c
c   SUBROUTINE: GetSelected_FindText(CString& strResult)
c				only for one line selection
c
c   FUNCTION:  Get the select text
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::GetSelected_FindText(CString& strResult)
{
	strResult = "";
	if (IsSelection())
	{
		CPoint ptSelStart, ptSelEnd;
		GetSelection(ptSelStart, ptSelEnd);		
		if (ptSelStart.y == ptSelEnd.y)
		{
			LPCTSTR pszChars = GetLineChars(ptSelStart.y);
			int nChars = ptSelEnd.x - ptSelStart.x;
			lstrcpyn(strResult.GetBuffer(nChars + 1), pszChars + ptSelStart.x, nChars + 1);
			strResult.ReleaseBuffer();
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  FindTextInRange(LPCTSTR lpszFind,
c					CPoint &ptStart, CPoint &ptEnd, 
c					BOOL bNext, BOOL bCase)
c
c   FUNCTION:  This function find the text in specified range and select it
c
c   INPUT:  lpszFind: text string to find
c           ptStart, ptEnd:    Range to search
c			bNext: search direction
c           bCase:    if search match the case
c   OUTPUT: none
c   RETURN: true if found
c
c***********************************************************************
*/
BOOL CPtedTextView::FindTextInRange(LPCTSTR lpszFind, CPoint &ptStart, CPoint &ptEnd,
                                BOOL bNext, BOOL bCase)
{
	ASSERT_VALID(this);
	if ((lpszFind == NULL) || (*lpszFind == '\0'))
		return 0;

	CWaitCursor wait;
	CPoint ptCurrent, ptFoundPos;
	ptCurrent = m_ptCursorPos;

//	DWORD dwFlags = FIND_WHOLE_WORD;
	DWORD dwFlags = 0;
	if (bNext==0) 
		dwFlags = FIND_DIRECTION_UP | dwFlags;
	if (bCase) 
		dwFlags = FIND_MATCH_CASE | dwFlags;
	int stat = FindTextInBlock(lpszFind, ptCurrent, ptStart, ptEnd,
			dwFlags, 1, &ptFoundPos);
	if (stat)
		HighlightText(ptFoundPos, lstrlen(lpszFind));
	return stat;
}
/***********************************************************************
c
c   SUBROUTINE:  FindStrAddrInRange(LPCTSTR lpszFind, char *addr,
c					CPoint &ptStart, CPoint &ptEnd, 
c					BOOL bNext, BOOL bCase,int *flen, double *fval)
c
c   FUNCTION:  This function find the text or adrress whichever first
c				in specified range and select it
c
c   INPUT:  lpszFind: text string to find
c			addr: address to find
c           ptStart, ptEnd:    Range to search
c			bNext: search direction
c           bCase:    if search match the case
c   OUTPUT: flen: find letter address length
c			fval: find letter address value
c   RETURN: true if found
c
c***********************************************************************
*/
BOOL CPtedTextView::FindStrAddrInRange(LPCTSTR lpszFind, char *addr, CPoint &ptStart, CPoint &ptEnd,
                                BOOL bNext, BOOL bCase, int *flen, double *fval)
{
	ASSERT_VALID(this);

	*flen = 0;
	CWaitCursor wait;
	CPoint ptCurrent, ptFoundPos;
	ptCurrent = m_ptCursorPos;

	DWORD dwFlags = 0;
	if (bNext==0) 
		dwFlags = FIND_DIRECTION_UP | dwFlags;
	if (bCase) 
		dwFlags = FIND_MATCH_CASE | dwFlags;

	int stat = FindStrAddrInBlock((char *)lpszFind, addr, ptCurrent, ptStart, ptEnd, dwFlags, flen, fval, &ptFoundPos);
	if (stat)
	{
		if (*flen>0)
			HighlightText(ptFoundPos, *flen);
		else
			HighlightText(ptFoundPos, lstrlen(lpszFind));
	}
	return stat;
}

/***********************************************************************
c
c   SUBROUTINE:  Get_Range_Pos(PtedRangeStruct* cRange, 
c					CPoint &ptStart, CPoint & ptEnd)
c
c   FUNCTION:  This function Get Range start and end position
c			
c
c   INPUT:  
c			PtedRangeStruct* cRange:  
c
c   OUTPUT: CPoint ptStart: rang start position (line, char)
c			CPoint ptEnd: range end position (line, char)
c			blin: range start line
c			elin: range end line
c			rflag:	1: return position include '\r' chars
c					0: return position not include '\r' chars
c
c***********************************************************************
*/

void CPtedTextView::Get_Range_Pos(PtedRangeStruct* cRange, 
		CPoint &ptStart, CPoint & ptEnd)
{
	int stat, flen;
	CPoint ptFindPos;
	double fval;

	ptStart.x = 0;
	ptStart.y = 0;
	ptEnd.y = GetLineCount() - 1;
	ptEnd.x = GetLineLength(ptEnd.y);
	
	if (cRange->begin==1)
	{
		return;
	}
	else if (cRange->begin==2)
	{
		if (IsSelection())
		{
			GetSelection(ptStart, ptEnd);
		}
		else
		{
			ptEnd.y = ptStart.y;
			ptEnd.x = ptStart.x;
		}
		return;
	}
	else if (cRange->begin==3)
	{
		ptStart = GetCursorPos();
	}
	else if (cRange->begin==4)
	{
		ptStart.x = 0;
		ptStart.y = 0;
	}
	else if (cRange->begin==5)
	{
		char *out;
		out = new char[200];
		int ftype;
		Ptd_GetFindStr(cRange->baddress, &out, 1, &ftype);
		DWORD dwFlags = 0;
		stat = FindStrAddrInBlock(NULL, out, ptStart, ptStart, ptEnd, dwFlags, &flen, &fval, &ptFindPos);
		if (stat)
			ptStart = ptFindPos;
		delete out;
	}
	else if (cRange->begin==6)
	{
		DWORD dwFlags = 0;
		stat = FindTextInBlock(cRange->bstring, ptStart, ptStart, ptEnd,
									dwFlags, 0, &ptFindPos);
		if (stat)
			ptStart = ptFindPos;
	}
	else if (cRange->begin==7)
	{
		ptStart.y = m_pTextBuffer->GetLineWithFlag(PTED_BOOKMARK(cRange->bline));
	}

	if (cRange->end==1)
	{
		ptEnd.y = ptStart.y + atoi(cRange->enumber) - 1;
		if (ptEnd.y>GetLineCount()-1) ptEnd.y = GetLineCount()-1;
		if (ptEnd.y<ptStart.y) ptEnd.y = ptStart.y;
		ptEnd.x = GetLineLength(ptEnd.y);
	}
	else if (cRange->end==2)
	{
		ptEnd.y = GetLineCount()-1;
		ptEnd.x = GetLineLength(ptEnd.y);
	}
	else if (cRange->end==3)
	{
		char *out;
		out = new char[200];
		int ftype;
		Ptd_GetFindStr(cRange->eaddress, &out, 1, &ftype);
		DWORD dwFlags = 0;
		stat = FindStrAddrInBlock(NULL, out, ptStart, ptStart, ptEnd, dwFlags, &flen, &fval, &ptFindPos);
		if (stat)
			ptStart = ptFindPos;
		delete out;
	}
	else if (cRange->end==4)
	{
		DWORD dwFlags = 0;
		stat = FindTextInBlock(cRange->bstring, ptStart, ptStart, ptEnd,
									dwFlags, 0, &ptFindPos);
		if (stat)
			ptStart = ptFindPos;
	}
	else if (cRange->end==5)
	{
		ptEnd.y = m_pTextBuffer->GetLineWithFlag(PTED_BOOKMARK(cRange->eline));
	}
}
/*
.....this ptBlockBegin is already before ptBlockEnd
.....if fnext = 0 (direction up), then assign current point to ptBlockEnd
......												(0,0) to ptBlockBegin
.....if fnext = 1 (direction down), then assign current point to ptBlockBegin
......												(endline,endchar) to ptBlockEnd
......when call this function
*/

/***********************************************************************
c
c   SUBROUTINE:  FindStrAddrInBlock(LPCTSTR findstr, char *FindAddr, CPoint &ptCurrentPos, 
c					CPoint &ptBlockBegin, CPoint &ptBlockEnd, 
c					DWORD dwFlags,int *flen, double *fval,CPoint *pptFoundPos)
c
c   FUNCTION:  This function find the text or adrress whichever first
c				in specified range
c
c   INPUT:  findstr: text string to find
c			FindAddr: address to find
c			ptCurrentPos: currect position to start searching
c           ptBlockBegin, ptBlockEnd:    Range to search
c			dwFlags: search flag
c   OUTPUT: flen: find letter address length
c			fval: find letter address value
c			pptFoundPos: found position
c   RETURN: true if found
c
c***********************************************************************
*/
BOOL CPtedTextView::FindStrAddrInBlock(LPCTSTR findstr, char*FindAddr,  CPoint &ptCurrentPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, int *flen, double *fval, CPoint *pptFoundPos)
{
	return m_pTextBuffer->FindStrAddrInBlock(findstr, FindAddr,  ptCurrentPos, ptBlockBegin, ptBlockEnd, dwFlags, flen, fval, pptFoundPos);
}

/***********************************************************************
c
c   SUBROUTINE:  FindAddrInBlock(int kreg, double gval, int regonly, CPoint &ptCurrentPos, 
c					CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, int *flen, 
c					double *fval, CPoint *pptFoundPos)
c
c   FUNCTION:  This function find the letter adrress 
c
c   INPUT:  kreg: letter address register to be found
c			gval: letter address value to be found
c			regonly: register only flag (ignore the value if regonly=1)
c			ptCurrentPos: currect position to start searching
c           ptBlockBegin, ptBlockEnd:    Range to search
c			dwFlags: search flag
c   OUTPUT: flen: find letter address length
c			fval: find letter address value
c			pptFoundPos: found position
c   RETURN: true if found
c
c***********************************************************************
*/
BOOL CPtedTextView::FindAddrInBlock(int kreg, double gval, int regonly, CPoint &ptCurrentPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, int *flen, double *fval, CPoint *pptFoundPos)
{
	return m_pTextBuffer->FindAddrInBlock(kreg, gval, regonly,  ptCurrentPos, ptBlockBegin, ptBlockEnd, dwFlags, flen, fval, pptFoundPos);
}

/***********************************************************************
c
c   SUBROUTINE:  FindAll(CString strFind, DWORD dwFlags, int letter, CPoint &ptStart, 
c					CPoint &ptEnd, CPtedTextView *textview)
c
c   FUNCTION:  This function find all the text or the letter adrress in 
c				certain range and put into a text buffer 
c
c   INPUT:  strFind: string/letter address to be found
c			dwFlags: search flag
c           ptStart, ptEnd:    Range to search
c			letter: if match letter address
c   OUTPUT: textview: update the view's textbuffer with all the founding
c   RETURN: true if found
c
c***********************************************************************
*/
BOOL CPtedTextView::FindAll(CString strFind, DWORD dwFlags, int letter, CPoint &ptStart, CPoint &ptEnd, CPtedTextView *textview)
{
	int nc, bCase, stat, flen,line_num, nPos;
	char *temp;
	double fval;
	char line_str[256], find_str[256];
	int newline, newchar;
	CPoint ptCurrentPos, ptBlockBegin, ptBlockEnd;
	ptCurrentPos.x = ptBlockBegin.x = ptStart.x;
	ptCurrentPos.y = ptBlockBegin.y = ptStart.y;
	ptBlockEnd.y = ptEnd.y;
	ptBlockEnd.x = ptEnd.x;
	line_num = 0;

	if (strFind.IsEmpty())
		return 0;
	ASSERT_VALIDTEXTPOS(ptCurrentPos);
	ASSERT_VALIDTEXTPOS(ptBlockBegin);
	ASSERT_VALIDTEXTPOS(ptBlockEnd);
	ASSERT(ptBlockBegin.y < ptBlockEnd.y || ptBlockBegin.y == ptBlockEnd.y && 
		ptBlockBegin.x <= ptBlockEnd.x);

	if (ptCurrentPos.y < ptBlockBegin.y || ptCurrentPos.y == ptBlockBegin.y && 
		ptCurrentPos.x < ptBlockBegin.x)
		ptCurrentPos = ptBlockBegin;

	CString what = strFind;
	if ((dwFlags & FIND_MATCH_CASE) == 0)
		what.MakeUpper();
	nc = what.GetLength();
	temp = what.GetBuffer(nc);
	strcpy(find_str, temp);

	char *addr;
	addr = new char[200];
	if ((dwFlags & FIND_MATCH_CASE) == 0)
		bCase = 0;
	else
		bCase = 1;
	int fType;

	Ptd_GetFindStr(find_str, &addr, bCase, &fType);
	int kerr, kreg, regonly;
	double gval;
	nc = strlen(addr);
	ptd_defmtcod(&kreg, &gval, addr, &nc, &bCase, &kerr);
	regonly = 0;
	if (kerr == 2)
		regonly = 1;
	else if (kerr==-1)
		letter = 0;

	Pted_disply_ProcessWindow("Pted Find All...", (CPtedWindow *)m_parent);
	Pted_Display_as_percent(1, (CPtedWindow *)m_parent);
	int perc;
	for (;;)
	{
		while (ptCurrentPos.y <= ptBlockEnd.y)
		{
			int nLineLength = GetLineLength(ptCurrentPos.y);
			nLineLength -= ptCurrentPos.x;
			if (nLineLength <= 0)
			{
				ptCurrentPos.x = 0;
				ptCurrentPos.y ++;
				continue;
			}

			LPCTSTR pszChars = GetLineChars(ptCurrentPos.y);
			pszChars += ptCurrentPos.x;

			CString line;
			lstrcpyn(line.GetBuffer(nLineLength + 1), pszChars, nLineLength + 1);
			line.ReleaseBuffer();
			if ((dwFlags & FIND_MATCH_CASE) == 0)
				line.MakeUpper();

			nc = line.GetLength();
			temp = line.GetBuffer(nc);
			strcpy(line_str, temp);
			if (letter==0)
			{
/*
......always partial find
*/
				nPos = ::Ptd_FindStringInTextLine(line_str, find_str, 0);
				if (nPos >= 0)
				{
/*
.....find the line, put this line into the new text buffer
*/
					if (line_num==0)
					{
						textview->Get_TextBuffer()->AppendLine(line_num, A2T(line_str), -1, newline, newchar);
						textview->Get_TextBuffer()->SetLinkLine(line_num, ptCurrentPos.y);
					}
					else
						textview->Get_TextBuffer()->InsertLine(A2T(line_str), -1, -1, 0, ptCurrentPos.y);
//					textview->Get_TextBuffer()->InsertLine(A2T(line_str));
					line_num++;
					if (ptCurrentPos.y == ptBlockEnd.y)
						break;
				}
			}
			else
			{
				if (fType!=1)
				{
					stat = Ptd_FindAddrInTextLine2(line_str, kreg, gval, regonly, &flen, &fval,&nPos);
					if (nPos >= 0)
					{
/*
.....find the line, put this line into the new text buffer
*/	
//						textview->Get_TextBuffer()->InsertLine(A2T(line_str));
						if (line_num==0)
						{
							textview->Get_TextBuffer()->AppendLine(line_num, A2T(line_str), -1, newline, newchar);
							textview->Get_TextBuffer()->SetLinkLine(line_num, ptCurrentPos.y);
						}
						else
							textview->Get_TextBuffer()->InsertLine(A2T(line_str), -1, -1, 0, ptCurrentPos.y);
						line_num++;
						if (ptCurrentPos.y == ptBlockEnd.y)
							break;
					}
/*					else
					{
						nPos = ::Ptd_FindStringInTextLine(line_str, find_str, (dwFlags & FIND_WHOLE_WORD) != 0);
						if (nPos >= 0)
						{
							textview->Get_TextBuffer()->InsertLine(A2T(line_str));
							line_num++;
							if (ptCurrentPos.y == ptBlockEnd.y)
								break;
						}
					}
*/				}
			}
			ptCurrentPos.x = 0;
			ptCurrentPos.y ++;
			perc = 100*(ptCurrentPos.y - ptStart.y)/(ptEnd.y - ptStart.y);
			Pted_Display_as_percent(perc, (CPtedWindow *)m_parent);
		}
		Pted_Close_ProcessWindow((CPtedWindow *)m_parent);
		textview->Get_TextBuffer()->SetModified(0);
		textview->Get_TextBuffer()->UpdateViews(NULL, UPDATE_RESET);

		if (line_num==0)
			return FALSE;
		else
			return TRUE;
	}
	Pted_Close_ProcessWindow((CPtedWindow *)m_parent);
	textview->Get_TextBuffer()->SetModified(0);
	if (line_num==0)
		return FALSE;
}
/***********************************************************************
c
c   SUBROUTINE:  FindStrAdds(LPCTSTR lpszFind, char *adds, CPoint &ptStart, CPoint &ptEnd, 
c						BOOL bNext, BOOL bCase, int *fadd, double *fval)
c
c   FUNCTION:  This function find the text or letter address in 
c				specified range whichever come first 
c				and select these find
c
c   INPUT:  lpszFind: text string to find
c			adds:     Letter address to find
c           ptStart, ptEnd:    Range to search
c			bNext:	  find direction
c			bCase:    case sensitivity
c   OUTPUT: fadd: if letter address found
c			fval: find letter address value
c	RETURN: None
c
c***********************************************************************
*/
BOOL CPtedTextView::FindStrAdds(LPCTSTR lpszFind, char *adds, CPoint &ptStart, CPoint &ptEnd, BOOL bNext, BOOL bCase,
						int *fadd, double* fval)
{
	int len, stat;
	*fadd = 0;
	stat = FindStrAddrInRange(lpszFind, adds, ptStart, ptEnd, bNext, bCase, &len, fval);
	if (stat)
	{
		if (len>0)
			*fadd = 1;
	}
	return stat;
}

/***********************************************************************
c
c   SUBROUTINE:  ReplaceAll(LPCTSTR lpszFind, BOOL bCase,
c			LPCTSTR lpszReplace, int fletter, CPoint &ptStart, CPoint &ptEnd)
c				
c   FUNCTION:  Callback function for "Replace All" on findreplace dialog
c
c   INPUT:  fletter: match register
c           ptStart, ptEnd:    Range to search
c			lpszFind: find string
c			lpszReplace: replace string
c			bCase:  case sensitivity
c			vflag: verify flag
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void CPtedTextView::ReplaceAll(LPCTSTR lpszFind, LPCTSTR lpszReplace, BOOL bCase,
			int fletter, CPoint &ptStart, CPoint &ptEnd, int vflag)
{

	CPoint ptFindPos, ptCurrent, rStartpos, rEndpos,ptNewPos;
	double fval, gval, gval2;
	int flen, perc, regonly, kreg, kreg2, kerr,nc;
	char m_replacestr[256];
	char m_findstr[256];

	CPoint old_ptStart = ptStart;
	CPoint old_ptEnd = ptEnd;

	ptCurrent.x = 0;
	ptCurrent.y = 0;
	strcpy(m_replacestr, lpszReplace);
	strcpy(m_findstr, lpszFind);

	Pted_disply_ProcessWindow("Pted Replace All...", (CPtedWindow *)m_parent);
	Pted_Display_as_percent(1, (CPtedWindow *)m_parent);


	CString sTextToDelete;
	int saved = 0;
	if (m_pTextBuffer->GetUndoFlag()==0)
	{
/*
.....if undo active, then we save the old text
*/
		m_pTextBuffer->GetText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x, sTextToDelete);
		m_pTextBuffer->BeginNoUndo();
		saved = 1;
	}

	m_no_update = 1;
	DWORD dwFlags = 0;
	if (bCase) 
		dwFlags = FIND_MATCH_CASE | dwFlags;

repeat1:
	if (fletter==0)
	{
		if (!FindTextInBlock(lpszFind, ptCurrent, ptStart, ptEnd, dwFlags, 0, 
					&ptFindPos))
		{
			goto done;
		}
		else
		{
			rStartpos.x = ptFindPos.x;
			rStartpos.y = ptFindPos.y;
			rEndpos.x = ptFindPos.x + strlen(lpszFind);
			rEndpos.y = ptFindPos.y;
			m_pTextBuffer->TextReplace(rStartpos, rEndpos, m_replacestr, ptNewPos);
			ptCurrent.x = ptNewPos.x;
			ptCurrent.y = ptNewPos.y;
			if ((ptNewPos.y-rEndpos.y)>0)
				ptEnd.y = ptEnd.y + (ptNewPos.y-rEndpos.y);
		}
		perc = 100*(ptCurrent.y - ptStart.y)/(ptEnd.y - ptStart.y);
		Pted_Display_as_percent(perc, (CPtedWindow *)m_parent);
		goto repeat1;
	}
/*
.....Letter address
*/
	char *in1, *out1;
	char *in2, *out2;
	int len1, len2, ftype, rtype;
	len1 = strlen(m_findstr);
	len2 = strlen(m_replacestr);
	in1 = new char[200];
	in2 = new char[200];
	strcpy(in1, m_findstr);
	strcpy(in2, m_replacestr);
	out1 = new char[200];
	out2 = new char[200];
	Ptd_GetFindStr(in1, &out1, bCase, &ftype);
	Ptd_GetFindStr(in2, &out2, bCase, &rtype);

	nc = strlen(out1);
	ptd_defmtcod(&kreg, &gval, out1, &nc, &bCase, &kerr);
	if (kerr == 2)
		regonly = 1;
	else if (kerr==-1)
		ftype = 1;
	if (rtype==2)
	{
		nc = strlen(out2);
		ptd_defmtcod(&kreg2, &gval2, out2, &nc, &bCase, &kerr);
	}

repeat2:
	if (ftype==1)
	{
		if (!FindTextInBlock(m_findstr, ptCurrent, ptStart, ptEnd, dwFlags, 0, 
					&ptFindPos))
		{
			goto done;
		}
		else
		{
			rStartpos.x = ptFindPos.x;
			rStartpos.y = ptFindPos.y;
			rEndpos.x = ptFindPos.x + strlen(m_findstr);
			rEndpos.y = ptFindPos.y;
			m_pTextBuffer->TextReplace(rStartpos, rEndpos, m_replacestr, ptNewPos);
			ptCurrent.x = ptNewPos.x;
			ptCurrent.y = ptNewPos.y;
			if ((ptNewPos.y-rEndpos.y)>0)
				ptEnd.y = ptEnd.y + (ptNewPos.y-rEndpos.y);
		}
		perc = 100*(ptCurrent.y - ptStart.y)/(ptEnd.y - ptStart.y);
		Pted_Display_as_percent(perc, (CPtedWindow *)m_parent);
	}
	else
	{
/*
.....only search for the letter address if match register
*/
		if (!FindAddrInBlock(kreg, gval, regonly, ptCurrent, ptStart, ptEnd, dwFlags, 
					&flen, &fval, &ptFindPos))
		{
			goto done;
		}
/*
.....if we find address, then we replace with address if it is address,
.....otherwise
.....replace with text string
.....12/17/99
*/
		else if ((flen>0)&&(rtype==2))
		{
/*
.....we need comvert the letter address first
*/
			ptd_fmticod(&kreg2, &fval, out2, &nc);
			rStartpos.x = ptFindPos.x;
			rStartpos.y = ptFindPos.y;
			rEndpos.x = ptFindPos.x + flen;
			rEndpos.y = ptFindPos.y;
			m_pTextBuffer->TextReplace(rStartpos, rEndpos, out2, ptNewPos);
			ptCurrent.x = ptNewPos.x;
			ptCurrent.y = ptNewPos.y;
		}
		else	
		{
			rStartpos.x = ptFindPos.x;
			rStartpos.y = ptFindPos.y;
			rEndpos.x = ptFindPos.x + strlen(m_findstr);
			rEndpos.y = ptFindPos.y;
			m_pTextBuffer->TextReplace(rStartpos, rEndpos, m_replacestr, ptNewPos);
			ptCurrent.x = ptNewPos.x;
			ptCurrent.y = ptNewPos.y;
		}
		if ((ptNewPos.y-rEndpos.y)>0)
			ptEnd.y = ptEnd.y + (ptNewPos.y-rEndpos.y);
		perc = 100*(ptCurrent.y - ptStart.y)/(ptEnd.y - ptStart.y);
		Pted_Display_as_percent(perc, (CPtedWindow *)m_parent);
	}
	goto repeat2;
done:
	if (ptCurrent.x == ptEnd.y)
	{
		ptEnd.x = ptNewPos.x;
	}
	if (saved)
	{
		m_pTextBuffer->EndNoUndo();
		BeginUndoGroup();
		m_pTextBuffer->AddUndoRecord(FALSE, old_ptStart, old_ptEnd, sTextToDelete, NULL, PTED_REPLACE);
		CString pszText;
		m_pTextBuffer->GetText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x, pszText);
		m_pTextBuffer->AddUndoRecord(TRUE, ptStart, ptEnd, pszText, NULL, PTED_REPLACE);
		FlushUndoGroup();
	}
	m_no_update = 0;

	UpdateView(NULL, UPDATE_HORZRANGE | UPDATE_VERTRANGE);

	Pted_Close_ProcessWindow((CPtedWindow *)m_parent);
	RedrawWindow();
	return;
}



/* 
.....The following print function is mostly copied from MFC code ViewPrint.cpp 
.....but made some change because the CView class we use here
.....is child of CDialog instead of CFrameWnd, so directly use CView print function
.....will cause errors.
*/
/////////////////////////////////////////////////////////////////////////////
// Printing Dialog

class CPrintingDialog : public CDialog
{
public:
	//{{AFX_DATA(CPrintingDialog)
	enum { IDD = AFX_IDD_PRINTDLG };
	//}}AFX_DATA
	CPrintingDialog::CPrintingDialog(CWnd* pParent)
		{
			Create(CPrintingDialog::IDD, pParent);      // modeless !
			_afxWinState->m_bUserAbort = FALSE;
		}
	virtual ~CPrintingDialog() { }

	virtual BOOL OnInitDialog();
	virtual void OnCancel();
};

BOOL CALLBACK _AfxAbortProc(HDC, int)
{
	_AFX_WIN_STATE* pWinState = _afxWinState;
	MSG msg;
	while (!pWinState->m_bUserAbort &&
		::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE))
	{
		if (!AfxGetThread()->PumpMessage())
			return FALSE;   // terminate if WM_QUIT received
	}
	return !pWinState->m_bUserAbort;
}

BOOL CPrintingDialog::OnInitDialog()
{
	SetWindowText(AfxGetAppName());
	CenterWindow();
	return CDialog::OnInitDialog();
}

void CPrintingDialog::OnCancel()
{
	_afxWinState->m_bUserAbort = TRUE;  // flag that user aborted print
	CDialog::OnCancel();
}



/***********************************************************************
c
c   SUBROUTINE: OnFilePrint()  
c
c   FUNCTION:  Print the file
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnFilePrint()
{
	CPrintInfo printInfo;
	ASSERT(printInfo.m_pPD != NULL);    // must be set

	if (LOWORD(GetCurrentMessage()->wParam) == ID_FILE_PRINT_DIRECT)
	{
		CCommandLineInfo* pCmdInfo = AfxGetApp()->m_pCmdInfo;

		if (pCmdInfo != NULL)
		{
			if (pCmdInfo->m_nShellCommand == CCommandLineInfo::FilePrintTo)
			{
				printInfo.m_pPD->m_pd.hDC = ::CreateDC(pCmdInfo->m_strDriverName,
					pCmdInfo->m_strPrinterName, pCmdInfo->m_strPortName, NULL);
				if (printInfo.m_pPD->m_pd.hDC == NULL)
				{
					AfxMessageBox(AFX_IDP_FAILED_TO_START_PRINT);
					return;
				}
			}
		}

		printInfo.m_bDirect = TRUE;
	}

/*	if (IsSelection())
		((CPrintDialog *)(printInfo.m_pPD))->m_pd.Flags = PD_SELECTION;
	else
		((CPrintDialog *)(printInfo.m_pPD))->m_pd.Flags = PD_ALLPAGES;
*/	if (OnPreparePrinting(&printInfo))
	{
		ASSERT(printInfo.m_pPD->m_pd.hDC != NULL);

		CString strOutput;
		if (printInfo.m_pPD->m_pd.Flags & PD_PRINTTOFILE && !printInfo.m_bDocObject)
		{
			CString strDef(MAKEINTRESOURCE(AFX_IDS_PRINTDEFAULTEXT));
			CString strPrintDef(MAKEINTRESOURCE(AFX_IDS_PRINTDEFAULT));
			CString strFilter(MAKEINTRESOURCE(AFX_IDS_PRINTFILTER));
			CString strCaption(MAKEINTRESOURCE(AFX_IDS_PRINTCAPTION));
			CFileDialog dlg(FALSE, strDef, strPrintDef,
				OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT, strFilter);
			dlg.m_ofn.lpstrTitle = strCaption;

			if (dlg.DoModal() != IDOK)
				return;

			strOutput = dlg.GetPathName();
		}

		CString strTitle;
		m_parent->GetWindowText(strTitle);
		DOCINFO docInfo;
		memset(&docInfo, 0, sizeof(DOCINFO));
		docInfo.cbSize = sizeof(DOCINFO);
		docInfo.lpszDocName = strTitle;
		CString strPortName;
		int nFormatID;
		if (strOutput.IsEmpty())
		{
			docInfo.lpszOutput = NULL;
			strPortName = printInfo.m_pPD->GetPortName();
			nFormatID = AFX_IDS_PRINTONPORT;
		}
		else
		{
			docInfo.lpszOutput = strOutput;
			LPTSTR lpszTitle;
			lpszTitle = strPortName.GetBuffer(_MAX_PATH);
			if (lpszTitle==NULL)
				return;
			if (::GetFileTitle(strOutput, lpszTitle, (WORD)_MAX_PATH) != 0)
			{
				return;
			}
			nFormatID = AFX_IDS_PRINTTOFILE;
		}

		CDC dcPrint;
		if (!printInfo.m_bDocObject)
		{
			dcPrint.Attach(printInfo.m_pPD->m_pd.hDC);  // attach printer dc
			dcPrint.m_bPrinting = TRUE;
		}
		OnBeginPrinting(&dcPrint, &printInfo);

		if (!printInfo.m_bDocObject)
			dcPrint.SetAbortProc(_AfxAbortProc);

		AfxGetMainWnd()->EnableWindow(FALSE);
		CPrintingDialog dlgPrintStatus(this);

		CString strTemp;
		dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_DOCNAME, strTitle);
		dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_PRINTERNAME,
			printInfo.m_pPD->GetDeviceName());
		AfxFormatString1(strTemp, nFormatID, strPortName);
		dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_PORTNAME, strTemp);
		dlgPrintStatus.ShowWindow(SW_SHOW);
		dlgPrintStatus.UpdateWindow();

		if (!printInfo.m_bDocObject && dcPrint.StartDoc(&docInfo) == SP_ERROR)
		{
			AfxGetMainWnd()->EnableWindow(TRUE);

			OnEndPrinting(&dcPrint, &printInfo);
			dlgPrintStatus.DestroyWindow();
			dcPrint.Detach();
			AfxMessageBox(AFX_IDP_FAILED_TO_START_PRINT);
			return;
		}

		UINT nEndPage = printInfo.GetToPage();
		UINT nStartPage = printInfo.GetFromPage();
/*		if ((printInfo.m_pPD)->m_pd.Flags == PD_SELECTION)
		{
			nStartPage = 1;
			nEndPage = 0xffff;
		}
*/
		if (nEndPage < printInfo.GetMinPage())
			nEndPage = printInfo.GetMinPage();
		if (nEndPage > printInfo.GetMaxPage())
			nEndPage = printInfo.GetMaxPage();

		if (nStartPage < printInfo.GetMinPage())
			nStartPage = printInfo.GetMinPage();
		if (nStartPage > printInfo.GetMaxPage())
			nStartPage = printInfo.GetMaxPage();

		int nStep = (nEndPage >= nStartPage) ? 1 : -1;
		nEndPage = (nEndPage == 0xffff) ? 0xffff : nEndPage + nStep;

		VERIFY(strTemp.LoadString(AFX_IDS_PRINTPAGENUM));

		BOOL bError = FALSE;
		if (printInfo.m_bDocObject)
		{
			OnPrepareDC(&dcPrint, &printInfo);
			OnPrint(&dcPrint, &printInfo);
		}
		else
		{
			for (printInfo.m_nCurPage = nStartPage;
				printInfo.m_nCurPage != nEndPage; printInfo.m_nCurPage += nStep)
			{
				OnPrepareDC(&dcPrint, &printInfo);

				if (!printInfo.m_bContinuePrinting)
					break;

				TCHAR szBuf[80];
				wsprintf(szBuf, strTemp, printInfo.m_nCurPage);
				dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_PAGENUM, szBuf);

				printInfo.m_rectDraw.SetRect(0, 0,
					dcPrint.GetDeviceCaps(HORZRES),
					dcPrint.GetDeviceCaps(VERTRES));
				dcPrint.DPtoLP(&printInfo.m_rectDraw);

				if (dcPrint.StartPage() < 0)
				{
					bError = TRUE;
					break;
				}

				OnPrepareDC(&dcPrint, &printInfo);

				ASSERT(printInfo.m_bContinuePrinting);

				OnPrint(&dcPrint, &printInfo);
				if (dcPrint.EndPage() < 0 || !_AfxAbortProc(dcPrint.m_hDC, 0))
				{
					bError = TRUE;
					break;
				}
				if ((nStartPage + m_nPrintPages - 1) < (printInfo.m_nCurPage + nStep))
					break;
			}
		}

		if (!printInfo.m_bDocObject)
		{
			if (!bError)
				dcPrint.EndDoc();
			else
				dcPrint.AbortDoc();
		}

		AfxGetMainWnd()->EnableWindow();

		OnEndPrinting(&dcPrint, &printInfo);
		dlgPrintStatus.DestroyWindow();

		dcPrint.Detach();
	}
}
