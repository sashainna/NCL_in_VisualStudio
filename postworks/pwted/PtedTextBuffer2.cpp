/************************************************************************
c
c   FILE NAME: PtedTextBuffer2.cpp
c
c   CONTAINS:
c		CPtedTextBuffer::InternalInsertBuffer(int nLine, int nPos, CPtedTextBuffer *tBuffer, int &nEndLine, int &nEndChar)
c		CPtedTextBuffer::InternalDeleteText(int nStartLine, int nStartChar, int nEndLine, int nEndChar)
c		CPtedTextBuffer::InternalInsertText(int nLine, int nPos, LPCTSTR pszText, int &nEndLine, int &nEndChar)
c		CPtedTextBuffer::CanUndo()
c		CPtedTextBuffer::CanRedo()
c		CPtedTextBuffer::GetUndoDescription(CString &desc, POSITION pos)
c		CPtedTextBuffer::GetRedoDescription(CString &desc, POSITION pos)
c		CPtedTextBuffer::Undo(CPoint &ptCursorPos)
c		CPtedTextBuffer::Redo(CPoint &ptCursorPos)
c		CPtedTextBuffer::AddUndoRecord(BOOL bInsert, const CPoint &ptStartPos, const CPoint &ptEndPos, LPCTSTR pszText, CPtedTextBuffer *buffer, int nActionType)
c		CPtedTextBuffer::InsertText(int nLine, int nPos, LPCTSTR pszText, 
c		CPtedTextBuffer::DeleteText(int nStartLine, int nStartChar, 
c									int nEndLine, int nEndChar, int nAction)
c		CPtedTextBuffer::GetActionDescription(int nAction, CString &desc)
c		CPtedTextBuffer::SetModified(BOOL bModified)
c		CPtedTextBuffer::BeginUndoGroup(BOOL bMergeWithPrevious)
c		CPtedTextBuffer::FlushUndoGroup(CPtedTextView *pSource)
c		CPtedTextBuffer::FindNextBookmarkLine(int nCurrentLine)
c		CPtedTextBuffer::FindPrevBookmarkLine(int nCurrentLine)
c		CPtedTextBuffer::ReverseRange(CPoint &ptStart, CPoint &ptEnd)
c		CPtedTextBuffer::GetUndoActive()
c		CPtedTextBuffer::Includebuffer(CPtedTextBuffer *include_buf, CPoint &currentPt, CPoint &ptStart, 
c									CPoint & ptEnd, int nAction, int flag)
c		CPtedTextBuffer::FindLineWithFlag(DWORD dwFlag)
c		CPtedTextBuffer::GetLineWithFlag(DWORD dwFlag)
c		CPtedTextBuffer::SetLineFlag(int nLine, DWORD dwFlag, BOOL bSet, BOOL bRemoveFromPreviousLine)
c		CPtedTextBuffer::AttachView(CPtedTextView *pView)
c		CPtedTextBuffer::UpdateViews(CUpdateContext *pContext, DWORD dwUpdateFlags, int nLineIndex) 
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c	MODULE NAME AND RELEASE LEVEL
c       PtedTextBuffer2.cpp , 24.1
c	DATE AND TIME OF LAST  MODIFICATION
c       09/11/13 , 12:59:28
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include <malloc.h>
#include "PtedTextBuffer.h"
#include "PtedTextView.h"
#include <sys/stat.h>
#include <sys/types.h>
#include "PtdFunc.h"
#include "PtdGlobal.h"
#include <time.h>

#ifndef __AFXPRIV_H__
#include <afxpriv.h>
#endif

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#ifdef _DEBUG
#define _ADVANCED_BUGCHECK	1
#endif
/*
.....we still need to do something for string more than LINE_MAX_CHARS when merge 2 string into one line 
*/
/***********************************************************************
c
c   SUBROUTINE:  InternalInsertBuffer(int nLine, int nPos, CPtedTextBuffer *tBuffer, int &nEndLine, int &nEndChar)
c
c   FUNCTION:  internal (don't consider undo/redo) Insert  a text buffer into current text buffer
c
c   INPUT:  nLine, nPos: position to insert
c			tBuffer: text buffer to insert
c			
c   OUTPUT: nEndLine, nEndChar: ending position after insert
c	RETURN: true if insert ok
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::InternalInsertBuffer(int nLine, int nPos, CPtedTextBuffer *tBuffer, int &nEndLine, int &nEndChar)
{
	int i,j;

	int ins_lines = tBuffer->GetLineCount();
	if (ins_lines==0)
		return 1;
	int endchar, endline;

	endline = ins_lines-1;
	endchar = tBuffer->GetLineLength(ins_lines-1);

	m_aLines.SetSize(m_current_size + ins_lines + 10, 5000);
	m_max_size = m_current_size + ins_lines + 10;
	CArray <SLineInfo, SLineInfo&> m_newLines;
	CArray <SLineInfo, SLineInfo&> m_insLines;
	SLineInfo temp;
	char *tmpstr;

	m_insLines.SetSize(ins_lines + 2, 5000);
	m_insLines.Copy(tBuffer->m_aLines);
	m_newLines.SetSize(m_current_size + ins_lines + 10, 5000);

	int nLength;
	char line_str[LINE_MAX_CHARS];

	for (i=0, j=0; i<nLine; i++,j++)
	{
		temp = m_aLines.GetAt(i);
		m_newLines[j] = temp;
	}
	if (nPos!=0)
	{
		tmpstr = T2A(m_aLines[nLine].m_pcLine);
		strncpy(line_str, &(tmpstr[0]), nPos);
		tmpstr = T2A(m_insLines[0].m_pcLine);
		nLength = m_insLines[0].m_nLength;
		strncpy(&(line_str[nPos]), &(tmpstr[0]), nLength);
		nLength = nLength+nPos;
		line_str[nLength] = '\0';
		temp.m_nLength = nLength;
		temp.m_linkline = -1;
		temp.m_spaces = nLength*1.5;
		if (temp.m_spaces>LINE_MAX_CHARS) 
			temp.m_spaces = LINE_MAX_CHARS;
		temp.m_nMax = LINE_MAX_CHARS;
		temp.m_pcLine = new TCHAR[temp.m_spaces];
		if (temp.m_nLength > 0)
			memcpy(temp.m_pcLine, A2T(line_str), sizeof(TCHAR) * temp.m_nLength);
		temp.m_dwFlags = 0;
		m_newLines[j] = temp;
		j++;
	}
	else
	{
		tmpstr = T2A(m_insLines[0].m_pcLine);
		nLength = m_insLines[0].m_nLength;
		strncpy(line_str, tmpstr, nLength);
		line_str[nLength] = '\0';
		temp.m_nLength = nLength;
		temp.m_linkline = -1;
		temp.m_nMax = LINE_MAX_CHARS;
		temp.m_spaces = nLength*1.5;
		if (temp.m_spaces>LINE_MAX_CHARS) 
			temp.m_spaces = LINE_MAX_CHARS;
		temp.m_pcLine = new TCHAR[temp.m_spaces];
		if (temp.m_nLength > 0)
			memcpy(temp.m_pcLine, A2T(line_str), sizeof(TCHAR) * temp.m_nLength);
		temp.m_dwFlags = 0;
		m_newLines[j] = temp;
		j++;
	}
	for (i=1; i<ins_lines-1; i++, j++)
	{
		tmpstr = T2A(m_insLines[i].m_pcLine);
		nLength = m_insLines[i].m_nLength;
		strncpy(line_str, tmpstr, nLength);
		line_str[nLength] = '\0';
		temp.m_nLength = nLength;
		temp.m_linkline = -1;
		temp.m_nMax = LINE_MAX_CHARS;
		temp.m_spaces = nLength*1.5;
		if (temp.m_spaces>LINE_MAX_CHARS) 
			temp.m_spaces = LINE_MAX_CHARS;
		temp.m_pcLine = new TCHAR[temp.m_spaces];
		if (temp.m_nLength > 0)
			memcpy(temp.m_pcLine, A2T(line_str), sizeof(TCHAR) * temp.m_nLength);
		temp.m_dwFlags = 0;
		m_newLines[j] = temp;
	}
	nEndLine = j;
	if (endchar!=0)
	{
		tmpstr = T2A(m_insLines[endline].m_pcLine);
		strncpy(line_str, &(tmpstr[0]), endchar);
		tmpstr = T2A(m_aLines[nLine].m_pcLine);
		nLength = m_aLines[nLine].m_nLength - nPos;
		strncpy(&(line_str[endchar]), &(tmpstr[nPos]), nLength);
		nLength = nLength+endchar;
		line_str[nLength] = '\0';
		temp.m_nLength = nLength;
		temp.m_linkline = -1;
		temp.m_nMax = LINE_MAX_CHARS;
		temp.m_spaces = nLength*1.5;
		if (temp.m_spaces>LINE_MAX_CHARS) 
			temp.m_spaces = LINE_MAX_CHARS;
		temp.m_pcLine = new TCHAR[temp.m_spaces];
		if (temp.m_nLength > 0)
			memcpy(temp.m_pcLine, A2T(line_str), sizeof(TCHAR) * temp.m_nLength);
		temp.m_dwFlags = 0;
		m_newLines[j] = temp;
		j++;
	}
	else
	{
		temp = m_aLines[nLine];
		m_newLines[j] = temp;
		j++;
	}
	nEndChar = endchar;
	for (i=nLine+1; i<m_current_size; i++,j++)
	{
		temp = m_aLines.GetAt(i);
		m_newLines[j] = temp;
	}
	m_aLines.SetSize(m_current_size + ins_lines + 10, 5000);
	m_aLines.Copy(m_newLines);
	m_current_size = j;
	
	CInsertContext context;
	context.m_ptStart.x = nPos;
	context.m_ptStart.y = nLine;
				
	context.m_ptEnd.x = nEndChar;
	context.m_ptEnd.y = nEndLine;

	UpdateViews(&context, UPDATE_HORZRANGE | UPDATE_VERTRANGE, nLine);

	if (! m_bModified)
		SetModified(TRUE);
	return TRUE;
}

/***********************************************************************
c
c   SUBROUTINE:  InternalDeleteText(int nStartLine, int nStartChar, int nEndLine, int nEndChar, int update)
c
c   FUNCTION:  internal (don't consider undo/redo) delete  a text string into current text buffer
c
c   INPUT:  nStartLine, nStartChar, nEndLine, nEndChar: range to be deleted
c			update: if update the view
c			
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::InternalDeleteText(int nStartLine, int nStartChar, int nEndLine, int nEndChar, int update)
{
	ASSERT(m_bInit);	
	ASSERT(nStartLine >= 0 && nStartLine < m_current_size);
int len = m_aLines[nStartLine].m_nLength;
	ASSERT(nStartChar >= 0 && nStartChar <= m_aLines[nStartLine].m_nLength);
	ASSERT(nEndLine >= 0 && nEndLine < m_current_size);
	ASSERT(nEndChar >= 0 && nEndChar <= m_aLines[nEndLine].m_nLength);
	ASSERT(nStartLine < nEndLine || nStartLine == nEndLine && nStartChar < nEndChar);
	if (m_bReadOnly)
		return FALSE;

	CDeleteContext context;
	context.m_ptStart.y = nStartLine;
	context.m_ptStart.x = nStartChar;
	context.m_ptEnd.y = nEndLine;
	context.m_ptEnd.x = nEndChar;
	DWORD flag;
	int newline = 0;
	if (nStartLine == nEndLine)
	{
		SLineInfo &li = m_aLines[nStartLine];
		if (nEndChar < li.m_nLength)
		{
			memcpy(li.m_pcLine + nStartChar, li.m_pcLine + nEndChar,
					sizeof(TCHAR) * (li.m_nLength - nEndChar));
		}
		li.m_nLength -= (nEndChar - nStartChar);
		if (m_current_size>nStartLine+1)
		{
			flag = GetLineFlags(nStartLine+1);
			if (flag & PTED_APPEND_LINE)
			{
				InternalDeleteText(nStartLine, li.m_nLength, nStartLine+1, 0, 0);
				newline = 1;
			}
		}
	}
	else
	{
		int nRestCount = m_aLines[nEndLine].m_nLength - nEndChar;
		LPTSTR pszRestChars = NULL;
		if (nRestCount > 0)
		{
			pszRestChars = new TCHAR[nRestCount];
			memcpy(pszRestChars, m_aLines[nEndLine].m_pcLine + nEndChar, nRestCount * sizeof(TCHAR));
		}

		int nDelCount = nEndLine - nStartLine;
		for (int L = nStartLine + 1; L <= nEndLine; L ++)
			delete m_aLines[L].m_pcLine;
		m_aLines.RemoveAt(nStartLine + 1, nDelCount);
		m_current_size = m_current_size - nDelCount;
		m_max_size = m_current_size;

		m_aLines[nStartLine].m_nLength = nStartChar;
		if (nRestCount > 0)
		{
			int newline,newchar;
			AppendLine(nStartLine, pszRestChars, nRestCount, newline, newchar);
			delete pszRestChars;
		}
		else if (m_current_size>nEndLine+1)
		{
			flag = GetLineFlags(nEndLine+1);
			if (flag & PTED_APPEND_LINE)
			{
				InternalDeleteText(nEndLine, 0, nEndLine+1, 0, 0);
			}
		}
		newline = 1;
	}
//	if (update)
	{
		if (newline)
			UpdateViews(&context, UPDATE_HORZRANGE | UPDATE_VERTRANGE, nStartLine);
		else
			UpdateViews(&context, UPDATE_SINGLELINE | UPDATE_HORZRANGE, nStartLine);
	}
	if (! m_bModified)
		SetModified(TRUE);
	return TRUE;
}
/***********************************************************************
c
c   SUBROUTINE:  InternalInsertText(int nLine, int nPos, LPCTSTR pszText, int &nEndLine, int &nEndChar)
c
c   FUNCTION:  internal (don't consider undo/redo) Insert  a text string into current text buffer
c
c   INPUT:  nLine, nPos: position to insert
c			pszText: text to insert
c			update: if need update the view
c			
c   OUTPUT: nEndLine, nEndChar: ending position after insert
c	RETURN: true if insert ok
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::InternalInsertText(int nLine, int nPos, LPCTSTR pszText, int &nEndLine, int &nEndChar, int update)
{
	return InternalInsertText(nLine, nPos, pszText, -1, nEndLine, nEndChar, update);
}

/***********************************************************************
c
c   SUBROUTINE:  InternalInsertText(int nLine, int nPos, LPCTSTR pszText, int &nEndLine, int &nEndChar)
c
c   FUNCTION:  internal (don't consider undo/redo) Insert  a text string into current text buffer
c
c   INPUT:  nLine, nPos: position to insert
c			pszText: text to insert
c			ins_len: insert text length
c			update: if need update the view
c			
c   OUTPUT: nEndLine, nEndChar: ending position after insert
c	RETURN: true if insert ok
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::InternalInsertText(int nLine, int nPos, LPCTSTR pszText, int ins_len, 
										 int &nEndLine, int &nEndChar, int update)
{
	ASSERT(m_bInit);	
	ASSERT(nLine >= 0 && nLine < m_current_size);
	ASSERT(nPos >= 0 && nPos <= m_aLines[nLine].m_nLength);
	if (m_bReadOnly)
		return FALSE;
	DWORD flag = 0;

	int old_llen = 0;
	if (ins_len==-1)
		ins_len  = lstrlen (pszText);
	int lines_num = ins_len/25;
	lines_num = lines_num * 1.15;
	if (m_max_size<m_current_size+lines_num)
	{
		m_max_size = m_current_size+lines_num;
		SetSize(m_max_size);
	}

	CInsertContext context;
	context.m_ptStart.x = nPos;
	context.m_ptStart.y = nLine;

	int nRestCount = m_aLines[nLine].m_nLength - nPos;
	LPTSTR pszRestChars = NULL;
	if (nRestCount > 0)
	{
		pszRestChars = new TCHAR[nRestCount];
		memcpy(pszRestChars, m_aLines[nLine].m_pcLine + nPos, nRestCount * sizeof(TCHAR));
		m_aLines[nLine].m_nLength = nPos;
	}

	int nCurrentLine = nLine;
	BOOL bNewLines = FALSE;
	int newline,newchar,passlen;
	int nTextPos;
	passlen = 0;
	for (;;)
	{
		nTextPos = 0;
		if (nCurrentLine == nLine)
			old_llen = m_aLines[nCurrentLine].m_nLength;
		else
			old_llen = 0;
		while (pszText[nTextPos] != 0 && pszText[nTextPos] != _T('\r')
				&& (nTextPos + old_llen < LINE_MAX_CHARS - 1) && (passlen+nTextPos<ins_len))
			nTextPos ++;

		if (nCurrentLine == nLine)
		{
			AppendLine(nLine, pszText, nTextPos, newline,newchar);
		}
		else
		{
			InsertLine(pszText, nTextPos, nCurrentLine, flag);
			bNewLines = TRUE;
		}

		if ((pszText[nTextPos] == 0) || (passlen+nTextPos>=ins_len))
		{
			nEndLine = nCurrentLine;
			nEndChar = m_aLines[nCurrentLine].m_nLength;
			AppendLine(nCurrentLine, pszRestChars, nRestCount, newline,newchar);
			if (nCurrentLine!=newline)
				bNewLines = TRUE;
			break;
		}

		nCurrentLine ++;
/*
.....which mean we cut the line by LINE_MAX_CHARS
*/
		if (pszText[nTextPos] != 0 && pszText[nTextPos] != _T('\r'))
		{
/*			flag = PTED_APPEND_LINE;
			nLine++;
			if (m_current_size>nLine)
			{
				flag = GetLineFlags(nLine);
				if ((flag & PTED_APPEND_LINE) == 0)
				{
					InsertLine("", 0, nLine, PTED_APPEND_LINE);
					bNewLines = TRUE;
				}
			}
			else
			{
				InsertLine("", 0, nLine, PTED_APPEND_LINE);
				bNewLines = TRUE;
			}
*/			
			flag = PTED_APPEND_LINE;
			pszText += nTextPos;
			passlen += nTextPos;
			continue;
		}
		flag = 0;
		nTextPos ++;

		if (pszText[nTextPos] == _T('\n'))
		{
			nTextPos ++;
			flag = 0;
		}
		else
		{
			ASSERT(FALSE);			
		}

		pszText += nTextPos;
		passlen += nTextPos;
	}

	if (pszRestChars != NULL)
		delete pszRestChars;

	if (update)
	{
		context.m_ptEnd.x = nEndChar;
		context.m_ptEnd.y = nEndLine;

		if (bNewLines)
			UpdateViews(&context, UPDATE_HORZRANGE | UPDATE_VERTRANGE, nLine);
		else
			UpdateViews(&context, UPDATE_SINGLELINE | UPDATE_HORZRANGE, nEndLine);
	}
	if (! m_bModified)
		SetModified(TRUE);
	return TRUE;
}

BOOL CPtedTextBuffer::CanUndo()
{
	ASSERT(m_nUndoPosition >= 0 && m_nUndoPosition <= m_aUndoBuf.GetSize());
	return m_nUndoPosition > 0;
}

BOOL CPtedTextBuffer::CanRedo()
{
	ASSERT(m_nUndoPosition >= 0 && m_nUndoPosition <= m_aUndoBuf.GetSize());
	return m_nUndoPosition < m_aUndoBuf.GetSize();
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
POSITION CPtedTextBuffer::GetUndoDescription(CString &desc, POSITION pos /*= NULL*/)
{
	ASSERT(CanUndo());	
	int flag = m_aUndoBuf[0].m_dwFlags;
	ASSERT((m_aUndoBuf[0].m_dwFlags & UNDO_BEGINGROUP) != 0);

	int nPosition;
	if (pos == NULL)
	{
		nPosition = m_nUndoPosition;
	}
	else
	{
		nPosition = (int) pos;
		ASSERT(nPosition > 0 && nPosition < m_nUndoPosition);
		ASSERT((m_aUndoBuf[nPosition].m_dwFlags & UNDO_BEGINGROUP) != 0);
	}

	nPosition --;
	while ((m_aUndoBuf[nPosition].m_dwFlags & UNDO_BEGINGROUP) == 0)
		nPosition --;

	if (! GetActionDescription(m_aUndoBuf[nPosition].m_nAction, desc))
		desc.Empty();	

	return (POSITION) nPosition;
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
POSITION CPtedTextBuffer::GetRedoDescription(CString &desc, POSITION pos /*= NULL*/)
{
	ASSERT(CanRedo());		
	ASSERT((m_aUndoBuf[0].m_dwFlags & UNDO_BEGINGROUP) != 0);
	ASSERT((m_aUndoBuf[m_nUndoPosition].m_dwFlags & UNDO_BEGINGROUP) != 0);

	int nPosition;
	if (pos == NULL)
	{
		nPosition = m_nUndoPosition;
	}
	else
	{
		nPosition = (int) pos;
		ASSERT(nPosition > m_nUndoPosition);
		ASSERT((m_aUndoBuf[nPosition].m_dwFlags & UNDO_BEGINGROUP) != 0);
	}

	if (! GetActionDescription(m_aUndoBuf[nPosition].m_nAction, desc))
		desc.Empty();	

	nPosition ++;
	while (nPosition < m_aUndoBuf.GetSize() &&
		(m_aUndoBuf[nPosition].m_dwFlags & UNDO_BEGINGROUP) == 0)
		nPosition --;

	if (nPosition >= m_aUndoBuf.GetSize())
		return NULL;
	return (POSITION) nPosition;
}

/***********************************************************************
c
c   SUBROUTINE: Undo(CPoint &ptCursorPos)  
c
c   FUNCTION:  Undo the last command
c
c   INPUT:  ptCursorPos: current cursion positin
c			
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::Undo(CPoint &ptCursorPos)
{
	ASSERT(CanUndo());
	ASSERT((m_aUndoBuf[0].m_dwFlags & UNDO_BEGINGROUP) != 0);
	for (;;)
	{
		m_nUndoPosition --;
		SUndoRecord &ur = m_aUndoBuf[m_nUndoPosition];
		CPtedTextBuffer *tBuffer = ur.GetBuffer();
		if (ur.m_dwFlags & UNDO_INSERT)
		{
			VERIFY(InternalDeleteText(ur.m_ptStartPos.y, ur.m_ptStartPos.x, ur.m_ptEndPos.y, ur.m_ptEndPos.x));
			ptCursorPos = ur.m_ptStartPos;
		}
		else
		{
			int nEndLine, nEndChar;
			if (tBuffer!=NULL)
			{
				InternalInsertBuffer(ur.m_ptStartPos.y, ur.m_ptStartPos.x, tBuffer, nEndLine, nEndChar);
#ifdef _ADVANCED_BUGCHECK
				ASSERT(ur.m_ptEndPos.y == nEndLine);
				ASSERT(ur.m_ptEndPos.x == nEndChar);
#endif
				ptCursorPos = ur.m_ptEndPos;				
			}
			else
			{
				VERIFY(InternalInsertText(ur.m_ptStartPos.y, ur.m_ptStartPos.x, ur.GetText(), nEndLine, nEndChar));
#ifdef _ADVANCED_BUGCHECK
				ASSERT(ur.m_ptEndPos.y == nEndLine);
				ASSERT(ur.m_ptEndPos.x == nEndChar);
#endif
				ptCursorPos = ur.m_ptEndPos;
			}
		}
		if (ur.m_dwFlags & UNDO_BEGINGROUP)
			break;
	}
	if (m_bModified && m_nSyncPosition == m_nUndoPosition)
		SetModified(FALSE);
	if (! m_bModified && m_nSyncPosition != m_nUndoPosition)
		SetModified(TRUE);
	return TRUE;
}

/***********************************************************************
c
c   SUBROUTINE: Redo(CPoint &ptCursorPos)  
c
c   FUNCTION:  Redo the last command
c
c   INPUT:  ptCursorPos: current cursion positin
c			
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::Redo(CPoint &ptCursorPos)
{
	ASSERT(CanRedo());
	ASSERT((m_aUndoBuf[0].m_dwFlags & UNDO_BEGINGROUP) != 0);
	ASSERT((m_aUndoBuf[m_nUndoPosition].m_dwFlags & UNDO_BEGINGROUP) != 0);
	for (;;)
	{
		SUndoRecord &ur = m_aUndoBuf[m_nUndoPosition];
		CPtedTextBuffer *tBuffer = ur.GetBuffer();
		if (ur.m_dwFlags & UNDO_INSERT)
		{
			int nEndLine, nEndChar;
			if (tBuffer!=NULL)
			{
				InternalInsertBuffer(ur.m_ptStartPos.y, ur.m_ptStartPos.x, tBuffer, nEndLine, nEndChar);
#ifdef _ADVANCED_BUGCHECK
				ASSERT(ur.m_ptEndPos.y == nEndLine);
				ASSERT(ur.m_ptEndPos.x == nEndChar);
#endif
				ptCursorPos = ur.m_ptEndPos;				
			}
			else
			{
				VERIFY(InternalInsertText(ur.m_ptStartPos.y, ur.m_ptStartPos.x, ur.GetText(), nEndLine, nEndChar));
#ifdef _ADVANCED_BUGCHECK
				ASSERT(ur.m_ptEndPos.y == nEndLine);
				ASSERT(ur.m_ptEndPos.x == nEndChar);
#endif
			}
			ptCursorPos = ur.m_ptEndPos;
		}
		else
		{
			VERIFY(InternalDeleteText(ur.m_ptStartPos.y, ur.m_ptStartPos.x, ur.m_ptEndPos.y, ur.m_ptEndPos.x));
			ptCursorPos = ur.m_ptStartPos;
		}
		m_nUndoPosition ++;
		if (m_nUndoPosition == m_aUndoBuf.GetSize())
			break;
		if ((m_aUndoBuf[m_nUndoPosition].m_dwFlags & UNDO_BEGINGROUP) != 0)
			break;
	}
	if (m_bModified && m_nSyncPosition == m_nUndoPosition)
		SetModified(FALSE);
	if (! m_bModified && m_nSyncPosition != m_nUndoPosition)
		SetModified(TRUE);
	return TRUE;
}

/***********************************************************************
c
c   SUBROUTINE: AddUndoRecord(BOOL bInsert, const CPoint &ptStartPos, const CPoint &ptEndPos, LPCTSTR pszText, CPtedTextBuffer *buffer, int nActionType)
c
c   FUNCTION:  Get the redo command description
c
c   INPUT:  bInsert: if it's insert or delete
c			ptStartPos, ptEndPos: undo range
c			pszText: undo text string
c			buffer:  undo buffer
c			nActionType: undo action
c			
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextBuffer::AddUndoRecord(BOOL bInsert, const CPoint &ptStartPos, const CPoint &ptEndPos, LPCTSTR pszText, CPtedTextBuffer *buffer, int nActionType)
{
	int count;
	ASSERT(m_bUndoGroup);
	ASSERT(m_aUndoBuf.GetSize() == 0 || (m_aUndoBuf[0].m_dwFlags & UNDO_BEGINGROUP) != 0);

	if (m_no_undo)
		return;
	if (nActionType==PTED_NEW)
		return;
/*
.....one group maxinum 1024 action
*/
	if (m_group_num>=1023)
	{
		if (! m_bUndoBeginGroup)
			FlushUndoGroup(m_attview);
		BeginUndoGroup();
	}
	int nBufSize = m_aUndoBuf.GetSize();
	if (m_nUndoPosition < nBufSize)
	{
		for (int I = m_nUndoPosition; I < nBufSize; I++)
		{	
			m_aUndoBuf[I].FreeText();
			m_aUndoBuf[I].FreeBuffer();
		}
		m_aUndoBuf.SetSize(m_nUndoPosition);
	}
	ASSERT(m_aUndoBuf.GetSize() <= m_nUndoBufSize);
	nBufSize = m_aUndoBuf.GetSize();
	if ((nBufSize >= m_nUndoBufSize) || (m_group_count>= m_nUndoGroupSize))
	{
		int nIndex = 0;
		for (;;)
		{
			m_aUndoBuf[nIndex].FreeText();
			m_aUndoBuf[nIndex].FreeBuffer();
			nIndex ++;
			if (nIndex == nBufSize || (m_aUndoBuf[nIndex].m_dwFlags & UNDO_BEGINGROUP) != 0)
				break;
		}
		m_aUndoBuf.RemoveAt(0, nIndex);
		m_group_count--;
	}
	ASSERT(m_aUndoBuf.GetSize() < m_nUndoBufSize);
	
	SUndoRecord ur;
	ur.m_dwFlags = bInsert ? UNDO_INSERT : 0;
	ur.m_nAction = nActionType;
	if (m_bUndoBeginGroup)
	{
		ur.m_dwFlags |= UNDO_BEGINGROUP;
		m_bUndoBeginGroup = FALSE;
		m_group_num = 1;
		m_group_count++;
	}
	else
		m_group_num++;
	ur.m_ptStartPos = ptStartPos;
	ur.m_ptEndPos = ptEndPos;
	switch (nActionType)
	{
		case PTED_CONVERT:
		case PTED_RESEQUENCE:
		case PTED_FORMAT:
		case PTED_UNFORMAT:
		case PTED_REVERSE:
		case PTED_INCLUDE:
		case PTED_GET:
		case PTED_ADD:
		case PTED_MIR:
		case PTED_MULT:
		case PTED_ROTATE:
		case PTED_SCALE:
		case PTED_TRANS:
//			if (bInsert)
//				ur.SetBuffer(buffer);
//			else
//				ur.SetText(pszText);
//			break;
		case PTED_TYPING:
		case PTED_CUT:
		case PTED_PASTE:
		case PTED_REPLACE:
		case PTED_DELETE:
		case PTED_INSERT:
		default:
			if (buffer)
				ur.SetBuffer(buffer);
			else
				ur.SetText(pszText);
//			ur.SetText(pszText);
			break;
	}	
	m_aUndoBuf.Add(ur);
	m_nUndoPosition = m_aUndoBuf.GetSize();
	count = m_aUndoBuf.GetSize();
	ASSERT(m_aUndoBuf.GetSize() <= m_nUndoBufSize);
}

/***********************************************************************
c
c   SUBROUTINE:  InsertText(int nLine, int nPos, LPCTSTR pszText, int &nEndLine, int &nEndChar)
c
c   FUNCTION: Insert  a text string into current text buffer
c
c   INPUT:  nLine, nPos: position to insert
c			pszText: text to insert
c			nAction: insert text action (for undo/redo purpose)
c			
c   OUTPUT: nEndLine, nEndChar: ending position after insert
c	RETURN: true if insert ok
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::InsertText(int nLine, int nPos, LPCTSTR pszText, 
									int &nEndLine, int &nEndChar, int nAction)
{
	if (! InternalInsertText(nLine, nPos, pszText, nEndLine, nEndChar))
		return FALSE;
	if (m_no_undo)
		return TRUE;

	BOOL bGroupFlag = FALSE;
	if (! m_bUndoGroup)
	{
		BeginUndoGroup();
		bGroupFlag = TRUE;
	}
	AddUndoRecord(TRUE, CPoint(nPos, nLine), CPoint(nEndChar, nEndLine), pszText, NULL, nAction);
	if (bGroupFlag)
		FlushUndoGroup(m_attview);
	return TRUE;
}

/***********************************************************************
c
c   SUBROUTINE:  DeleteText(int nStartLine, int nStartChar, int nEndLine, 
c						int nEndChar, int nAction)
c
c   FUNCTION:  delete  a text string into current text buffer
c
c   INPUT:  nStartLine, nStartChar, nEndLine, nEndChar: range to be deleted
c			nAction: delete text action (for undo/redo purpose)
c			
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::DeleteText(int nStartLine, int nStartChar, 
									int nEndLine, int nEndChar, int nAction)
{
	CString sTextToDelete;
	if (m_no_undo==0)
	{
		GetText(nStartLine, nStartChar, nEndLine, nEndChar, sTextToDelete);
	}
	if (! InternalDeleteText(nStartLine, nStartChar, nEndLine, nEndChar))
		return FALSE;

	if (m_no_undo)
		return TRUE;
	BOOL bGroupFlag = FALSE;
	if (! m_bUndoGroup)
	{
		BeginUndoGroup();
		bGroupFlag = TRUE;
	}

	AddUndoRecord(FALSE, CPoint(nStartChar, nStartLine), CPoint(nEndChar, nEndLine), sTextToDelete, NULL, nAction);
	if (bGroupFlag)
		FlushUndoGroup(m_attview);
	return TRUE;
}

/***********************************************************************
c
c   SUBROUTINE:  GetActionDescription(int nAction, CString &desc)
c
c   FUNCTION:  Get the action decription
c
c   INPUT:  
c			nAction: action ID
c			
c   OUTPUT: desc: decription of the action
c	RETURN: none
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::GetActionDescription(int nAction, CString &desc)
{
	switch (nAction)
	{
	case PTED_TYPING:
		desc = "Typing";
		break;
	case PTED_CUT:
		desc = "Cut";
		break;
	case PTED_PASTE:
		desc = "Paste";
		break;
	case PTED_REPLACE:
		desc = "Replace";
		break;
	case PTED_DELETE:
		desc = "Delete";
		break;
	case PTED_INSERT:
		desc = "Insert";
		break;
	case PTED_CONVERT:
		desc = "Convert";
		break;
	case PTED_RESEQUENCE:
		desc = "Resequence";
		break;
	case PTED_FORMAT:
		desc = "Format";
		break;
	case PTED_UNFORMAT:
		desc = "Unformat";
		break;
	case PTED_REVERSE:
		desc = "Reverse";
		break;
	case PTED_INCLUDE:
		desc = "Include";
		break;
	case PTED_GET:
		desc = "Get";
		break;
	case PTED_ADD:
		desc = "Add";
		break;
	case PTED_MIR:
		desc = "Mir";
		break;
	case PTED_MULT:
		desc = "Mult";
		break;
	case PTED_ROTATE:
		desc = "Rotate";
		break;
	case PTED_SCALE:
		desc = "Scale";
		break;
	case PTED_TRANS:
		desc = "Trans";
		break;
	case PTED_NEW:
		desc = "New File";
		break;
	default: desc = " ";
	}
	return TRUE;
}

/***********************************************************************
c
c   FUNCTION: SetModified(BOOL bModified)
c
c			This function set the text buffer as modified.
c
c   INPUT:  bModified: modify flag to be set
c			
c   OUTPUT: none
c		
c
c***********************************************************************
*/
void CPtedTextBuffer::SetModified(BOOL bModified /*= TRUE*/)
{
	if (bModified!=m_bModified)
	{
		m_bModified = bModified;
		if (m_attview!=NULL)
			m_attview->SetModifiedFlag(bModified);
	}
}

/***********************************************************************
c
c   FUNCTION: BeginUndoGroup(BOOL bMergeWithPrevious)
c
c			Set the undo group begin
c
c   INPUT:  bMergeWithPrevious: if this undo group merge with previous group
c			
c   OUTPUT: none
c		
c
c***********************************************************************
*/
void CPtedTextBuffer::BeginUndoGroup(BOOL bMergeWithPrevious /*= FALSE*/)
{
	ASSERT(! m_bUndoGroup);
	m_bUndoGroup = TRUE;
	if (m_bMergeWithPrevious==0)
	{
		bMergeWithPrevious = m_bMergeWithPrevious;
		m_bMergeWithPrevious = 1;
	}
	m_bUndoBeginGroup = m_nUndoPosition == 0 || ! bMergeWithPrevious;
}

/***********************************************************************
c
c   FUNCTION: FlushUndoGroup(CPtedTextView *pSource)
c
c			Flush the UndoGroup
c
c   INPUT:  pSource: the view to flush
c			
c   OUTPUT: none
c		
c
c***********************************************************************
*/
void CPtedTextBuffer::FlushUndoGroup(CPtedTextView *pSource)
{
	if (m_bUndoGroup==0)
	{
		m_bMergeWithPrevious = 0;
		m_group_num = 0;
		return;
	}
	int first = 1;
	if ((pSource != NULL) && (first))
	{
		first = 0;
		int tt = m_aUndoBuf.GetSize();
		ASSERT(m_nUndoPosition == m_aUndoBuf.GetSize());
		if (m_nUndoPosition > 0)
		{
			m_bUndoBeginGroup = TRUE;
			pSource->OnEditOperation(m_aUndoBuf[m_nUndoPosition - 1].m_nAction, m_aUndoBuf[m_nUndoPosition - 1].GetText());
		}
		first = 1;
	}
	m_bUndoGroup = FALSE;
}

/***********************************************************************
c
c   SUBROUTINE:  FindNextBookmarkLine(int nCurrentLine)
c
c   FUNCTION:  Get the next bookmark line
c
c   INPUT:  
c			nCurrentLine: start line to find the next
c			
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
int CPtedTextBuffer::FindNextBookmarkLine(int nCurrentLine)
{
	BOOL bWrapIt = TRUE;
	DWORD dwFlags = GetLineFlags(nCurrentLine);
	if ((dwFlags & PTED_BOOKMARKS) != 0)
		nCurrentLine++;

	int nSize = m_aLines.GetSize();
	for (;;)
	{ 
		while (nCurrentLine < nSize)
		{
			if ((m_aLines[nCurrentLine].m_dwFlags & PTED_BOOKMARKS) != 0)
				return nCurrentLine;
			nCurrentLine++;
		}
		if (!bWrapIt)
			return -1;

		bWrapIt = FALSE;
		nCurrentLine = 0;
	}
	return -1;
}

/***********************************************************************
c
c   SUBROUTINE:  FindPrevBookmarkLine(int nCurrentLine)
c
c   FUNCTION:  Get the Previous bookmark line
c
c   INPUT:  
c			nCurrentLine: start line to find the next
c			
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
int CPtedTextBuffer::FindPrevBookmarkLine(int nCurrentLine)
{
	BOOL bWrapIt = TRUE;
	DWORD dwFlags = GetLineFlags(nCurrentLine);
	if ((dwFlags & PTED_BOOKMARKS) != 0)
		nCurrentLine--;

	int nSize = m_aLines.GetSize();
	for (;;)
	{ 
		while (nCurrentLine >= 0)
		{
			if ((m_aLines[nCurrentLine].m_dwFlags & PTED_BOOKMARKS) != 0)
				return nCurrentLine;
			nCurrentLine--;
		}
		if (!bWrapIt)
			return -1;

		bWrapIt = FALSE;
		nCurrentLine = nSize - 1;
	}
	return -1;
}
 
/***********************************************************************
c
c   SUBROUTINE:  ReverseRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function Reverse all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::ReverseRange(CPoint &ptStart, CPoint &ptEnd)
{
	int sline, eline,i,j;
	CPoint pt_inc1, pt_inc2;

	sline = 0;
	eline = m_current_size;
/*
.....we are not consider X value when we reverse the line, so
.....when last line X=0, ignore the last line
*/
	if (ptEnd.x==0) ptEnd.y = ptEnd.y - 1;
	if (ptEnd.y<0) ptEnd.y = 0; 
	CArray <SLineInfo, SLineInfo&> m_revLines;
	SLineInfo temp;
	m_revLines.SetSize(m_current_size, 5000);
	i=0; j=0;
	for (i=0, j=0; i<ptStart.y; i++,j++)
	{
		temp = m_aLines.GetAt(i);
		m_revLines[j] = temp;
	}
	for (i=ptEnd.y; i>=ptStart.y &&i>=0; i--,j++)
	{
		temp = m_aLines.GetAt(i);
		m_revLines[j] = temp;
	}
	for (i=ptEnd.y+1; i<m_current_size; i++,j++)
	{
		temp = m_aLines.GetAt(i);
		m_revLines[j] = temp;
	}
	if (m_no_undo==0)
	{
		int nEndLine, nEndChar;
		BeginUndoGroup();
		CString pszText, sTextToDelete;	
		nEndLine = ptEnd.y;
		nEndChar = m_aLines[nEndLine].m_nLength;
		GetText(ptStart.y, 0, nEndLine, nEndChar, sTextToDelete);
		AddUndoRecord(FALSE, CPoint(0, ptStart.y), CPoint(nEndChar, nEndLine), sTextToDelete, NULL, PTED_REVERSE);
		m_aLines.Copy(m_revLines);
		nEndChar = m_aLines[nEndLine].m_nLength;
		GetText(ptStart.y, 0, nEndLine, nEndChar, pszText);
		AddUndoRecord(TRUE, CPoint(0, ptStart.y), CPoint(nEndChar, nEndLine), pszText, NULL, PTED_REVERSE);
		FlushUndoGroup(m_attview);
	}
	m_aLines.Copy(m_revLines);
}

/***********************************************************************
c
c   SUBROUTINE:  GetUndoActive()
c
c   FUNCTION:  Get the Undo Action
c
c   INPUT:  none
c   OUTPUT: none
c	RETURN: action ID
c
c***********************************************************************
*/
int CPtedTextBuffer::GetUndoActive()
{
	if (CanUndo()==0)
		return -1;
	int nPosition;
	nPosition = m_nUndoPosition;
	nPosition --;
	if (nPosition<0) nPosition = 0;
	while ((m_aUndoBuf[nPosition].m_dwFlags & UNDO_BEGINGROUP) == 0)
		nPosition --;
	return m_aUndoBuf[nPosition].m_nAction;
}
/*currentPt: current cursor in current buffer
ptStart, ptEnd: start and ending points in include buffer
*/
/*
.....include the buffer into the next line of current position
*/
/***********************************************************************
c
c   SUBROUTINE:  Includebuffer(CPtedTextBuffer *include_buf, CPoint &currentPt, CPoint &ptStart, 
c					CPoint & ptEnd, int nAction)
c
c   FUNCTION:  This function include a text buffer into current view
c				in specified range at currentPt
c
c   INPUT:  currentPt: current cursor
c			ptStart, ptEnd: range
c			include_buf: buffer to be included
c			nAction: include action
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::Includebuffer(CPtedTextBuffer *include_buf, CPoint &currentPt, CPoint &ptStart, 
									CPoint & ptEnd, int nAction, int flag)
{
	int i,j;
	int inc_lines = ptEnd.y - ptStart.y + 1;
	if (inc_lines==0)
		return;
	
	m_aLines.SetSize(m_current_size + inc_lines + 10, 5000);
	m_max_size = m_current_size + inc_lines + 10;
	SLineInfo temp;
	char *tmpstr;
/*
......move inc_lines lines back to leave space for include lines
*/
	i=0; j=0;
	for (i=currentPt.y+1; i<m_current_size; i++)
	{
		temp = m_aLines.GetAt(i);
		m_aLines[i+inc_lines] = temp;
	}
	int nLength;
	char line_str[LINE_MAX_CHARS];
	for (i=ptStart.y, j=currentPt.y+1; i<=ptEnd.y; i++,j++)
	{
		tmpstr = T2A(include_buf->m_aLines[i].m_pcLine);
		nLength = include_buf->m_aLines[i].m_nLength;
		strncpy(line_str, tmpstr, nLength);
		line_str[nLength] = '\0';
		temp.m_nLength = nLength;
		temp.m_spaces = include_buf->m_aLines[i].m_spaces;
		temp.m_linkline = -1;
		temp.m_nMax = LINE_MAX_CHARS;
		temp.m_pcLine = new TCHAR[temp.m_spaces];
		if (temp.m_nLength > 0)
			memcpy(temp.m_pcLine, A2T(line_str), sizeof(TCHAR) * temp.m_nLength);
		temp.m_dwFlags = include_buf->m_aLines[i].m_dwFlags;
		m_aLines[j] = temp;
	}
	m_current_size = m_current_size + inc_lines;
	
	CInsertContext context;
	context.m_ptStart.y = currentPt.y;
	context.m_ptStart.x = m_aLines[currentPt.y].m_nLength;
			
	int nEndLine = currentPt.y+inc_lines;
	int nEndChar = m_aLines[nEndLine].m_nLength;
	
	context.m_ptEnd.x = nEndChar;
	context.m_ptEnd.y = nEndLine;

	UpdateViews(&context, UPDATE_HORZRANGE | UPDATE_VERTRANGE, currentPt.y);

	if ((flag)&&(m_no_undo==0))
	{
		BOOL bGroupFlag = FALSE;
		if (! m_bUndoGroup)
		{
			BeginUndoGroup();
			bGroupFlag = TRUE;
		}
		CString pszText;	
		include_buf->GetText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x, pszText);
		AddUndoRecord(TRUE, CPoint(context.m_ptStart.x, currentPt.y), CPoint(nEndChar, nEndLine), pszText, NULL, nAction);
		if (bGroupFlag)
			FlushUndoGroup(m_attview);
	}
	if (! m_bModified)
		SetModified(TRUE);
}
