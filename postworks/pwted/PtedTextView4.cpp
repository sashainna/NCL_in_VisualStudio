/************************************************************************
c
c   FILE NAME: PtedTextView4.cpp
c
c   CONTAINS:
c		CPtedTextView::GetParseCookie(int nLineIndex)
c		CPtedTextView::ParseLine(DWORD dwCookie, int nLineIndex, TEXTBLOCK *pBuf, int &nActualItems)
c		CPtedTextView::UpdateView(CUpdateContext *pContext,
c								  DWORD dwFlags, int nLineIndex)
c		CPtedTextView::GetCursorPos()
c		CPtedTextView::SetCursorPos(const CPoint &ptCursorPos)
c		CPtedTextView::SetSelectionMargin(BOOL bSelMargin)
c		CPtedTextView::GetFont(LOGFONT &lf)
c		CPtedTextView::OnToggleBookmark(UINT nCmdID)
c		CPtedTextView::OnGoBookmark(UINT nCmdID)
c		CPtedTextView::OnClearBookmarks()
c		CPtedTextView::ShowCursor()
c		CPtedTextView::HideCursor()
c		CPtedTextView::GetViewTabs()
c		CPtedTextView::SetViewTabs(BOOL bViewTabs)
c		CPtedTextView::GetSelectionMargin()
c		CPtedTextView::BadCommand(CPtedTextView *textview)
c		CPtedTextView::ConvertMathRange(char **adds, int num, CPoint &ptStart, CPoint &ptEnd, int mflag)
c		CPtedTextView::ConvertRange(CPoint &ptStart, CPoint &ptEnd)
c		CPtedTextView::ConvertBadBlockRange(CPoint &ptStart, CPoint &ptEnd, int ftype, CPtedTextView *textview)
c		CPtedTextView::ConvertFormatRange(CPoint &ptStart, CPoint &ptEnd)
c		CPtedTextView::SetUndoFlag(int flag)
c		CPtedTextView::ConvertUnFormatRange(CPoint &ptStart, CPoint &ptEnd)
c		CPtedTextView::ReseqRange(CPoint &ptStart, CPoint &ptEnd, int bseq, int seqinc,int seqn, int nonly)
c		CPtedTextView::NctoSimfile(char *file, CPtedTextView *textview)
c		CPtedTextView::ConvertNctoapt(CPtedTextView* textview)
c		CPtedTextView::InitNew()
c		CPtedTextView::Includebuffer(CPtedTextBuffer *include_buf, CPoint &ptStart, CPoint & ptEnd, int nAction)
c		CPtedTextView::OnNextBookmark()
c		CPtedTextView::OnPrevBookmark()
c		CPtedTextView::OnClearAllBookmarks()
c		CPtedTextView::GetMarginWidth()
c		CPtedTextView::SaveToFile(char *filename, int ftype, int *classpt, int class_type)
c		CPtedTextView::SaveSelectToFile(char *filename, int ftype, CPoint &ptStart, CPoint &ptEnd, int *classpt, int class_type)
c		CPtedTextView::LoadFromFile(char *filename, int ftype)
c		CPtedTextView::LoadFromBuffer(CPtedTextBuffer *TextBuffer)
c		CPtedTextView::LoadCutterFile(char *filename)
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c	MODULE NAME AND RELEASE LEVEL
c       PtedTextView4.cpp , 24.1
c	DATE AND TIME OF LAST  MODIFICATION
c       09/11/13 , 12:59:29
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include "PtedEditCmd.h"
#include "PtedTextView.h"
#include "PtedTextBuffer.h"
#include <malloc.h>

extern "C" void Pted_getparse_word(char *msgbblk, char *msgeblk, char *eob, char *opskip, 
						char *tab, char *rewind);
extern "C" int IsRegister(const char *str, int knc, int *regnum, int *nc);
extern "C" int IsMajorMinorWord(const char *str, int knc, int *nc, int *flag);
extern "C" int IsSymbols(char sym);

static BOOL IsNumber(LPCTSTR pszChars, int nLength)
{
	int plus, mins, dot, ee;
	int start = 1;
	plus = mins = dot = ee = 0;
	if (! isdigit(pszChars[0]))
	{
		if ((nLength>1) && ((pszChars[0] == '+') || (pszChars[0] == '-') || (pszChars[0] == '.')) 
			&& (isdigit(pszChars[1])))
		{
			start = 2;
			if (pszChars[0] == '+') plus++;
			if (pszChars[0] == '-') mins++;
			if (pszChars[0] == '.') dot++;
			goto doit;
		}
		return FALSE;
	}
doit:;
	for (int I = start; I < nLength; I++)
	{
		if (isdigit(pszChars[I]))
		{
			continue;
		}
		else
		{
			if ((pszChars[I] == '.') && (dot==0) && 
				( ((I+1<nLength) && (isdigit(pszChars[I+1]))) ||
			      ((I-1>=0)      && (isdigit(pszChars[I-1])))   ))
			{
				dot++;
				I++;
			}
			else if ((pszChars[I] == '+') && (plus==0) && (I+1 < nLength) && (isdigit(pszChars[I+1])))
			{
				plus++;
				I++;
			}
			else if ((pszChars[I] == '-') && (mins==0) && (I+1 < nLength) && (isdigit(pszChars[I+1])))
			{
				mins++;
				I++;
			}
			else if ((ee==0) && ((pszChars[I] == 'e') || (pszChars[I] == 'E')))
			{
				plus = mins = dot = 0;
				ee++;
			}
			else
				return FALSE;
		}
	}
	return TRUE;
}

#define DEFINE_BLOCK(pos, colorindex)	\
	ASSERT((pos) >= 0 && (pos) <= nLength);\
	if (pBuf != NULL)\
	{\
		if (nActualItems == 0 || pBuf[nActualItems - 1].m_nCharPos <= (pos)) {\
		pBuf[nActualItems].m_nCharPos = (pos);\
		pBuf[nActualItems].m_nColorIndex = (colorindex);\
		nActualItems ++;}\
	}

#define COOKIE_NORMALTEXT			0x0001
#define COOKIE_MSGTXT		0x0002
#define COOKIE_EOB			0x0004
#define COOKIE_OPSKIP		0x0008
#define COOKIE_TAB			0x0010
#define COOKIE_REWIND		0x0100
#define COOKIE_SYMBOLS		0x1000

DWORD CPtedTextView::GetParseCookie(int nLineIndex)
{
	int nLineCount = GetLineCount();
	if (m_pdwParseCookies == NULL)
	{
		m_nParseArraySize = nLineCount;
		m_pdwParseCookies = new DWORD[nLineCount];
		memset(m_pdwParseCookies, 0xff, nLineCount * sizeof(DWORD));
	}

	if (nLineIndex < 0)
		return 0;
	if (m_pdwParseCookies[nLineIndex] != (DWORD) -1)
		return m_pdwParseCookies[nLineIndex];

	int L = nLineIndex;
	while (L >= 0 && m_pdwParseCookies[L] == (DWORD) -1)
		L --;
	L ++;

	int nBlocks;
	while (L <= nLineIndex)
	{
		DWORD dwCookie = 0;
		if (L > 0)
			dwCookie = m_pdwParseCookies[L - 1];
		ASSERT(dwCookie != (DWORD) -1);
		m_pdwParseCookies[L] = ParseLine(dwCookie, L, NULL, nBlocks);
		ASSERT(m_pdwParseCookies[L] != (DWORD) -1);
		L ++;
	}
	return m_pdwParseCookies[nLineIndex];
}

DWORD CPtedTextView::ParseLine(DWORD dwCookie, int nLineIndex, TEXTBLOCK *pBuf, int &nActualItems)
{
	int I,flag;
	int regnum, nc, pos;
	int nLength = GetLineLength(nLineIndex);
	int ftype = ((CPtedWindow *)m_parent)->m_ftype;
	int wtype = ((CPtedWindow *)m_parent)->m_wtype;
	if (wtype==4)
	{
		LPCTSTR pszChars    = GetLineChars(nLineIndex);
		BOOL bFirstChar     = 0;
		BOOL bRedefineBlock = TRUE;
		BOOL bDecIndex  = FALSE;
		int nIdentBegin = -1;
		for (int I = 0; ; I++)
		{
			if (I >= nLength)
				break;
			if (bRedefineBlock)
			{
				int nPos = I;
				if (bDecIndex)
					nPos--;
				DEFINE_BLOCK(nPos, COLORINDEX_NORMALTEXT);
				bRedefineBlock = FALSE;
				bDecIndex      = FALSE;
			}
			if (((CPtedWindow*)m_parent)->IsFindString(&(pszChars[I]), nLength-I, &pos, &nc))
			{
				DEFINE_BLOCK(I+pos, COLORINDEX_FINDTEXT);
				I = I + pos + nc-1;
				bRedefineBlock = TRUE;
			}
			if (I==0)
				return dwCookie & COOKIE_NORMALTEXT;
		}
	}
	if (m_syntax_color==0)
		return dwCookie & COOKIE_NORMALTEXT;
	if ((nLength <= 0) || (ftype==1))
		return dwCookie & COOKIE_NORMALTEXT;
/*
.....before parsing, set the parse value we may use
*/
	char msgbblk[20], msgeblk[20], eob[20], opskip[20], tab[20], rewind[20];
	Pted_getparse_word(msgbblk, msgeblk, eob, opskip, tab, rewind);

	LPCTSTR pszChars    = GetLineChars(nLineIndex);
	BOOL bFirstChar     = 0;
	BOOL bRedefineBlock = TRUE;
	BOOL bDecIndex  = FALSE;
	int nIdentBegin = -1;
	int nIdentEnd = -1;
	for (I = 0; ; I++)
	{
loop:;
		if (I >= nLength)
			break;
		if (bRedefineBlock)
		{
			int nPos = I;
			if (bDecIndex)
				nPos--;
			if ((dwCookie & (COOKIE_MSGTXT)) && (ftype==2))
			{
/*
......only Control Data file using MSG
*/
				DEFINE_BLOCK(nPos, COLORINDEX_MSGTEXT);
			}
			else
			{
				DEFINE_BLOCK(nPos, COLORINDEX_NORMALTEXT);
			}
			bRedefineBlock = FALSE;
			bDecIndex      = FALSE;
		}
/*
......Check for Message block
*/
		if (ftype==2)
		{
			if (dwCookie & COOKIE_MSGTXT)
			{
				if (strncmp(&(pszChars[I]), msgeblk, strlen(msgeblk))==0)
				{
					DEFINE_BLOCK(I, COLORINDEX_MSG);
					I = I + strlen(msgeblk)-1;
					dwCookie &= ~COOKIE_MSGTXT;
					bRedefineBlock = TRUE;
				}
				continue;
			}
			else
			{
				if (strncmp(&(pszChars[I]), msgbblk, strlen(msgbblk))==0)
				{
					DEFINE_BLOCK(I, COLORINDEX_MSG);
					I = I + strlen(msgbblk)-1;
					DEFINE_BLOCK(I, COLORINDEX_MSGTEXT);
					dwCookie |= COOKIE_MSGTXT;				
					bRedefineBlock = FALSE;
				}
			}
			if (dwCookie & COOKIE_MSGTXT)
				continue;

/*
......Check for Symbol characters
*/
			if (IsSymbols(pszChars[I]))
			{
				DEFINE_BLOCK(I, COLORINDEX_SYMBOLS);
				bRedefineBlock = TRUE;
				continue;
			}
			else if ((strlen(eob)>0)&&(strncmp(&(pszChars[I]), eob, strlen(eob))==0))
			{
				DEFINE_BLOCK(I, COLORINDEX_EOB);
				I = I + strlen(eob)-1;
				bRedefineBlock = TRUE;
				continue;
			}
			else if ((strlen(opskip)>0)&&(strncmp(&(pszChars[I]), opskip, strlen(opskip))==0))
			{
				DEFINE_BLOCK(I, COLORINDEX_OPSKIP);
				I = I + strlen(opskip)-1;
				bRedefineBlock = TRUE;
				continue;
			}
			else if ((strlen(tab)>0)&&(strncmp(&(pszChars[I]), tab, strlen(tab))==0))
			{
				DEFINE_BLOCK(I, COLORINDEX_TAB);
				I = I + strlen(tab)-1;
				bRedefineBlock = TRUE;
				continue;
			}
			else if ((strlen(rewind)>0)&&(strncmp(&(pszChars[I]), rewind, strlen(rewind))==0))
			{
				DEFINE_BLOCK(I, COLORINDEX_REWIND);
				I = I + strlen(rewind)-1;
				bRedefineBlock = TRUE;
				continue;
			}
/*
.....register and value
*/
			if (IsRegister(&(pszChars[I]), nLength-I, &regnum, &nc))
			{
				DEFINE_BLOCK(I, (COLORINDEX_A1+regnum-1));
				I = I + nc;
				bRedefineBlock = TRUE;
				continue;
			}
		}
		else
		{
			if ( (isalnum(pszChars[I])) || (pszChars[I]=='.')||(pszChars[I]=='+')||(pszChars[I]=='-'))
			{
				if (nIdentBegin == -1)
					nIdentBegin = I;
			}
			else if ((nIdentBegin >=0)&&((IsSymbols(pszChars[I])) || (pszChars[I]==' ')))
			{
				nIdentEnd = I;
				I = I - 1;
			}
			if ((nIdentEnd<0) || (nIdentBegin<0))
			{
				if (IsSymbols(pszChars[I])) 
				{
					DEFINE_BLOCK(I, COLORINDEX_SYMBOLS);
					if ( (I+1<nLength) && 
						((isdigit(pszChars[I+1])) || (pszChars[I+1]=='.')))
						bRedefineBlock = FALSE;
					else
						bRedefineBlock = TRUE;
					continue;
				}
			}
			else
			{
				if (nIdentBegin >= 0)
				{
					if (IsNumber(&(pszChars[nIdentBegin]), nIdentEnd-nIdentBegin))
					{
						DEFINE_BLOCK(nIdentBegin, COLORINDEX_NUMBER);
					}
					else if ((ftype!=5)&&(IsMajorMinorWord(&(pszChars[nIdentBegin]), nIdentEnd-nIdentBegin, &nc, &flag)))
					{
						if (flag==1)
						{
							DEFINE_BLOCK(nIdentBegin, COLORINDEX_MAJOR);
						}
						else
						{
							DEFINE_BLOCK(nIdentBegin, COLORINDEX_MINOR);
						}
						I = I - (nIdentEnd-nIdentBegin-nc);
					}
					bRedefineBlock = TRUE;
					nIdentBegin = -1;			
					nIdentEnd = -1;			
				}
			}
		}
	}
	if ((ftype!=2)&&(nIdentBegin >= 0))
	{
		if (IsNumber(&(pszChars[nIdentBegin]), I-nIdentBegin))
		{
			DEFINE_BLOCK(nIdentBegin, COLORINDEX_NUMBER);
		}
		else if ((ftype!=5)&&(IsMajorMinorWord(&(pszChars[nIdentBegin]), I-nIdentBegin, &nc, &flag)))
		{
			if (flag==1)
			{
				DEFINE_BLOCK(nIdentBegin, COLORINDEX_MAJOR);
			}
			else
			{
				DEFINE_BLOCK(nIdentBegin, COLORINDEX_MINOR);
			}
			I = I - (nLength-nIdentBegin-nc);
			nIdentBegin = -1;			
			if (I < nLength)
			{
				bRedefineBlock = TRUE;
				nIdentBegin = -1;			
				nIdentEnd = -1;			
				goto loop;
			}
		}
	}
	return dwCookie;
}
/***********************************************************************
c
c   SUBROUTINE:  UpdateView(CUpdateContext *pContext,DWORD dwFlags, int nLineIndex )
c
c   FUNCTION:  This function update the view value
c
c   INPUT:  pContext: update context to be update
c			dwFlags:     update flag
c           nLineIndex: line index to be update
c   OUTPUT: none
c	RETURN: None
c
c***********************************************************************
*/
void CPtedTextView::UpdateView(CUpdateContext *pContext,
								  DWORD dwFlags, int nLineIndex /*= -1*/)
{
	if (m_no_update)
		return;
	if (dwFlags & UPDATE_RESET)
	{
		ResetView();
		RecalcVertScrollBar();
		RecalcHorzScrollBar();
		return;
	}

	int nLineCount = GetLineCount();
	ASSERT(nLineCount > 0);
	ASSERT(nLineIndex >= -1 && nLineIndex < nLineCount);
	if ((dwFlags & UPDATE_SINGLELINE) != 0)
	{
		ASSERT(nLineIndex != -1);
		if (m_pdwParseCookies != NULL)
		{
			ASSERT(m_nParseArraySize == nLineCount);
			memset(m_pdwParseCookies + nLineIndex, 0xff, sizeof(DWORD) * (m_nParseArraySize - nLineIndex));
		}
		if (m_pnActualLineLength != NULL)
		{
			ASSERT(m_nActualLengthArraySize == nLineCount);
			m_pnActualLineLength[nLineIndex] = -1;
		}
		InvalidateLines(nLineIndex, -1, TRUE);
	}
	else
	{
		if (nLineIndex == -1)
			nLineIndex = 0;		
		if (m_pdwParseCookies != NULL)
		{
			if (m_nParseArraySize != nLineCount)
			{
				DWORD *pdwNewArray = new DWORD[nLineCount];
				if (nLineIndex > 0)
					memcpy(pdwNewArray, m_pdwParseCookies, sizeof(DWORD) * nLineIndex);
				delete m_pdwParseCookies;
				m_nParseArraySize = nLineCount;
				m_pdwParseCookies = pdwNewArray;
			}
			memset(m_pdwParseCookies + nLineIndex, 0xff, sizeof(DWORD) * (m_nParseArraySize - nLineIndex));
		}
		if (m_pnActualLineLength != NULL)
		{
			if (m_nActualLengthArraySize != nLineCount)
			{
				int *pnNewArray = new int[nLineCount];
				if (nLineIndex > 0)
					memcpy(pnNewArray, m_pnActualLineLength, sizeof(int) * nLineIndex);
				delete m_pnActualLineLength;
				m_nActualLengthArraySize = nLineCount;
				m_pnActualLineLength = pnNewArray;
			}
			memset(m_pnActualLineLength + nLineIndex, 0xff, sizeof(DWORD) * (m_nActualLengthArraySize - nLineIndex));
		}
		InvalidateLines(nLineIndex, -1, TRUE);
	}

	if (pContext != NULL)
	{
		pContext->RecalcPoint(m_ptCursorPos);
		pContext->RecalcPoint(m_ptSelStart);
		pContext->RecalcPoint(m_ptSelEnd);
		pContext->RecalcPoint(m_ptAnchor);
		ASSERT_VALIDTEXTPOS(m_ptCursorPos);
		ASSERT_VALIDTEXTPOS(m_ptSelStart);
		ASSERT_VALIDTEXTPOS(m_ptSelEnd);
		ASSERT_VALIDTEXTPOS(m_ptAnchor);
//		if (m_bDraggingText)
//		{
//			pContext->RecalcPoint(m_ptDraggedTextBegin);
//			pContext->RecalcPoint(m_ptDraggedTextEnd);
//			ASSERT_VALIDTEXTPOS(m_ptDraggedTextBegin);
//			ASSERT_VALIDTEXTPOS(m_ptDraggedTextEnd);
//		}
		CPoint ptTopLine(0, m_nTopLine);
		pContext->RecalcPoint(ptTopLine);
		ASSERT_VALIDTEXTPOS(ptTopLine);
		m_nTopLine = ptTopLine.y;
		UpdateCaret();
	}

	if ((dwFlags & UPDATE_VERTRANGE) != 0)
	{
		if (! m_bVertScrollBarLocked)
			RecalcVertScrollBar();
	}

	if ((dwFlags & UPDATE_HORZRANGE) != 0)
	{
		m_nMaxLineLength = -1;
		if (! m_bHorzScrollBarLocked)
			RecalcHorzScrollBar();
	}
	if (m_bSelectionPushed && pContext != NULL)
	{
		pContext->RecalcPoint(m_ptSavedSelStart);
		pContext->RecalcPoint(m_ptSavedSelEnd);
		ASSERT_VALIDTEXTPOS(m_ptSavedSelStart);
		ASSERT_VALIDTEXTPOS(m_ptSavedSelEnd);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  GetCursorPos()
c
c   FUNCTION:  This function get current cursor position
c
c   INPUT:  none
c   OUTPUT: none
c	RETURN: current cursor position
c
c***********************************************************************
*/
CPoint CPtedTextView::GetCursorPos()
{
	return m_ptCursorPos;
}

/***********************************************************************
c
c   SUBROUTINE:  SetCursorPos(const CPoint &ptCursorPos)
c
c   FUNCTION:  This function set current cursor position
c
c   INPUT:  ptCursorPos: current cursor position
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::SetCursorPos(const CPoint &ptCursorPos)
{
	ASSERT_VALIDTEXTPOS(ptCursorPos);
	m_ptCursorPos = ptCursorPos;
	m_nIdealCharPos = CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x);
	UpdateCaret();
}

/***********************************************************************
c
c   SUBROUTINE:  SetSelectionMargin(BOOL bSelMargin)
c
c   FUNCTION:  This function senable/diable the margin
c
c   INPUT:  bSelMargin: margin flag
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::SetSelectionMargin(BOOL bSelMargin)
{
	if (m_bSelMargin != bSelMargin)
	{
		m_bSelMargin = bSelMargin;
		if (::IsWindow(m_hWnd))
		{
			m_nScreenChars = -1;
			Invalidate();
			RecalcHorzScrollBar();
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  GetFont(LOGFONT &lf)
c
c   FUNCTION:  This function Get font
c
c   INPUT:  none
c   OUTPUT: lf: font to return
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::GetFont(LOGFONT &lf)
{
	lf = m_lfBaseFont;
}

/***********************************************************************
c
c   SUBROUTINE: SetFont(const LOGFONT &lf)
c
c   FUNCTION:  This function set font
c
c   INPUT:  lf: font to set
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::SetFont(const LOGFONT &lf)
{
	m_lfBaseFont = lf;
	m_nScreenLines = -1;
	m_nScreenChars = -1;
	m_nCharWidth = -1;
	m_nLineHeight = -1;
	if (m_pCacheBitmap != NULL)
	{
		m_pCacheBitmap->DeleteObject();
		delete m_pCacheBitmap;
		m_pCacheBitmap = NULL;
	}
	for (int I = 0; I < 4; I ++)
	{
		if (m_apFonts[I] != NULL)
		{
			m_apFonts[I]->DeleteObject();
			delete m_apFonts[I];
			m_apFonts[I] = NULL;
		}
	}
	if (::IsWindow(m_hWnd))
	{
		RecalcVertScrollBar();
		RecalcHorzScrollBar();
		UpdateCaret();
		Invalidate();
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnToggleBookmark(UINT nCmdID)
c
c   FUNCTION:  Callback function for 'Book mark #"
c
c   INPUT:  nCmdID: command ID
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnToggleBookmark(UINT nCmdID)
{
	int nBookmarkID = nCmdID - ID_EDIT_TOGGLE_BOOKMARK0;
	ASSERT(nBookmarkID >= 0 && nBookmarkID <= 9);
	if (m_pTextBuffer != NULL)
	{
		DWORD dwFlags = GetLineFlags(m_ptCursorPos.y);
		DWORD dwMask = PTED_BOOKMARK(nBookmarkID);
		m_pTextBuffer->SetLineFlag(m_ptCursorPos.y, dwMask, (dwFlags & dwMask) == 0);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnGoBookmark(UINT nCmdID)
c
c   FUNCTION:  Callback function for 'Goto Book mark #"
c
c   INPUT:  nCmdID: command ID
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnGoBookmark(UINT nCmdID)
{
	int nBookmarkID = nCmdID - ID_EDIT_GO_BOOKMARK0;
	ASSERT(nBookmarkID >= 0 && nBookmarkID <= 9);
	if (m_pTextBuffer != NULL)
	{
		int nLine = m_pTextBuffer->GetLineWithFlag(PTED_BOOKMARK(nBookmarkID));
		if (nLine >= 0)
		{
			CPoint pt(0, nLine);
			ASSERT_VALIDTEXTPOS(pt);
			SetCursorPos(pt);
			SetSelection(pt, pt);
			SetAnchor(pt);
			EnsureVisible(pt);
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnClearBookmarks()
c
c   FUNCTION:  Callback function for 'Clear Bookmark"
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnClearBookmarks()
{
	if (m_pTextBuffer != NULL)
	{
		for (int nBookmarkID = 0; nBookmarkID <= 9; nBookmarkID++)
		{
			int nLine = m_pTextBuffer->GetLineWithFlag(PTED_BOOKMARK(nBookmarkID));
			if (nLine >= 0)
			{
				m_pTextBuffer->SetLineFlag(nLine, PTED_BOOKMARK(nBookmarkID), FALSE);
			}
		}
		
	}
}

/***********************************************************************
c
c   SUBROUTINE:  ShowCursor()
c
c   FUNCTION:  show the cursor
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::ShowCursor()
{
	m_bCursorHidden = FALSE;
	UpdateCaret();
}

/***********************************************************************
c
c   SUBROUTINE:  HideCursor()
c
c   FUNCTION:  hide the cursor
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::HideCursor()
{
	m_bCursorHidden = TRUE;
	UpdateCaret();
}

/***********************************************************************
c
c   SUBROUTINE:  GetSelectionMargin()
c
c   FUNCTION:  
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CPtedTextView::GetSelectionMargin()
{
	return m_bSelMargin;
}
/***********************************************************************
c
c   SUBROUTINE:  BadCommand()
c				
c   FUNCTION:  Check the current file as APT description file
c				and put bad statments into the view text buffer if have any.
c
c   INPUT:  None
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
int CPtedTextView::BadCommand(CPtedTextView *textview)
{
	int stat;
	textview->m_no_update = 1;
	stat = m_pTextBuffer->BadCommand(textview->Get_TextBuffer());
	textview->m_no_update = 0;
	textview->UpdateView(NULL, UPDATE_HORZRANGE | UPDATE_VERTRANGE);
	return stat;
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertMathRange(char **adds, int num, PtedRangeStruct cRange, 
c					int mflag)
c				
c   FUNCTION:  Do math convert function in specified range
c
c   INPUT:  adds: array of input
c			ptStart, ptEnd:  range
c			num: number of input
c			mflag: which math funtion.
c				1:	ADD
c				2:	Mirror
c				3:	Multiply
c				4:	Rotate
c				5:	Scale
c				6:	Translate
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void CPtedTextView::ConvertMathRange(char **adds, int num, CPoint &ptStart, CPoint &ptEnd, int mflag)
{
	m_pTextBuffer->ConvertMathRange(adds, num, ptStart, ptEnd, mflag);
	RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  ConvertRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function convert all text in current window
c				in specified range
c
c   INPUT:  ptStart, ptEnd: range
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::ConvertRange(CPoint &ptStart, CPoint &ptEnd)
{
	m_no_update = 1;
	m_pTextBuffer->ConvertRange(ptStart, ptEnd);
	m_no_update = 0;
	UpdateView(NULL, UPDATE_HORZRANGE | UPDATE_VERTRANGE);
	RedrawWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertBadBlockRange(CPoint &ptStart, CPoint &ptEnd, 
c					int ftype, CPtedTextView *textview)
c
c   FUNCTION:  This function check the bad block and put the bad block into 
c				the view buffer
c
c   INPUT:  ptStart, ptEnd: range
c			ftype: file type
c   OUTPUT: textview: with the bad block text buffer
c
c***********************************************************************
*/
int CPtedTextView::ConvertBadBlockRange(CPoint &ptStart, CPoint &ptEnd, int ftype, CPtedTextView *textview)
{
	textview->m_no_update = 1;
	int stat = m_pTextBuffer->ConvertBadBlockRange(ptStart, ptEnd, ftype, textview->Get_TextBuffer());
	textview->m_no_update = 0;
	textview->UpdateView(NULL, UPDATE_HORZRANGE | UPDATE_VERTRANGE);
	return stat;
}

/***********************************************************************
c
c   SUBROUTINE:  ConvertFormatRange(CPoint &ptStart, CPoint &ptEnd)
c
c   FUNCTION:  This function Format all text in current view
c				in specified range
c
c   INPUT:  ptStart, ptEnd: range
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::ConvertFormatRange(CPoint &ptStart, CPoint &ptEnd)
{
	m_no_update = 1;
	m_pTextBuffer->ConvertFormatRange(ptStart, ptEnd);
	m_no_update = 0;
	UpdateView(NULL, UPDATE_HORZRANGE | UPDATE_VERTRANGE);
	RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  SetUndoFlag(int flag)
c
c   FUNCTION:  This function set the undo flag
c				Called back for 'Active conversion undo'
c
c   INPUT:  flag
c   OUTPUT: none
c
c***********************************************************************
*/
int CPtedTextView::SetUndoFlag(int flag)
{
	return m_pTextBuffer->SetUndoFlag(flag);
}

/***********************************************************************
c
c   SUBROUTINE:  ConvertUnFormatRange(CPoint &ptStart, CPoint &ptEnd)
c
c   FUNCTION:  This function Unformat all text in current view
c				in specified range
c
c   INPUT:  ptStart, ptEnd: range
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::ConvertUnFormatRange(CPoint &ptStart, CPoint &ptEnd)
{
	m_no_update = 1;
	m_pTextBuffer->ConvertUnFormatRange(ptStart, ptEnd);
	m_no_update = 0;

	UpdateView(NULL, UPDATE_HORZRANGE | UPDATE_VERTRANGE);
	RedrawWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  ReseqRange(PtedRangeStruct sRange, int bseq, int seqinc,
c				int seqn, int nonly)
c
c   FUNCTION:  This function Reseqence all text in current window
c				in specified range
c
c   INPUT:  ptStart, ptEnd: range
c			bseq:  beginning sequence number
c			seqinc: sequence increment
c			seqn:	output sequence number every seqn
c			nonly:  sequence numbered block only?		
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::ReseqRange(CPoint &ptStart, CPoint &ptEnd, int bseq, int seqinc,int seqn, int nonly)
{
	m_no_update = 1;
	m_pTextBuffer->ReseqRange(ptStart, ptEnd, bseq, seqinc,seqn, nonly);
	m_no_update = 0;

	UpdateView(NULL, UPDATE_HORZRANGE | UPDATE_VERTRANGE);
	RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertNctosim(char *file, CPtedTextView *textview, CPtedWindow *parent) 
c
c   FUNCTION:  This function converts the current control data file
c              into a simultation file .
c
c   INPUT:  file: current NC filename
c			parent: parent window of new view to be generate with simulnation file
c			
c   OUTPUT: 
c			textview: new view to be generate with simulnation file
c
c***********************************************************************
*/
int CPtedTextView::NctoSimfile(char *file, CPtedTextView *textview, CPtedWindow *parent)
{
	return m_pTextBuffer->NctoSimfile(file, textview->Get_TextBuffer(), parent);
}


/***********************************************************************
c
c   SUBROUTINE:  ConvertNctoapt(CPtedTextView* textview, CPtedWindow *parent)
c
c   FUNCTION:  This function converts the current control data file
c              into a APT file .
c
c   INPUT:  
c			parent: parent window of new view to be generate with APT file
c			
c   OUTPUT: 
c			textview: new view to be generate with APT file
c
c***********************************************************************
*/
int CPtedTextView::ConvertNctoapt(CPtedTextView* textview, CPtedWindow *parent)
{
	return m_pTextBuffer->ConvertNctoapt( textview->Get_TextBuffer(), parent);
}


/***********************************************************************
c
c   SUBROUTINE:  InitNew()
c
c   FUNCTION:  This function initial view with new text buffer .
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::InitNew()
{
	m_pTextBuffer->InitNew();
}


/***********************************************************************
c
c   SUBROUTINE:  Includebuffer(CPtedTextBuffer *include_buf, CPoint &ptStart, 
c					CPoint & ptEnd, int nAction)
c
c   FUNCTION:  This function include a text buffer into current view
c				in specified range
c
c   INPUT:  ptStart, ptEnd: range
c			include_buf: buffer to be included
c			nAction: include action
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::Includebuffer(CPtedTextBuffer *include_buf, CPoint &ptStart, CPoint & ptEnd, int nAction)
{
	m_pTextBuffer->Includebuffer(include_buf, m_ptCursorPos, ptStart, ptEnd, nAction);
	RedrawWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  OnToggleBookmark()
c
c   FUNCTION:  Callback function for 'Bookmark"
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnToggleBookmark()
{
	if (m_pTextBuffer != NULL)
	{
		DWORD dwFlags = GetLineFlags(m_ptCursorPos.y);
		DWORD dwMask  = PTED_BOOKMARKS;
		m_pTextBuffer->SetLineFlag(m_ptCursorPos.y, dwMask, (dwFlags & dwMask) == 0, FALSE);
	}
	int nLine = m_pTextBuffer->GetLineWithFlag(PTED_BOOKMARKS);
	if (nLine >= 0)
		m_bBookmarkExist = TRUE;
	else
		m_bBookmarkExist = FALSE;
}

void CPtedTextView::SelectLine(int line)
{
	ASSERT(line >= 0);

	if (m_pTextBuffer != NULL)
	{
		DWORD dwMask  = PTED_FINDMARKS;
		SetCursorPos(CPoint(0,line));
		m_pTextBuffer->SetLineFlag(line, dwMask, 1, 1);
		EnsureVisible(CPoint(0,line));
	}
	int nLine = m_pTextBuffer->GetLineWithFlag(PTED_FINDMARKS);
	if (nLine >= 0)
		m_bFindmarkExist = TRUE;
	else
		m_bFindmarkExist = FALSE;
}

/***********************************************************************
c
c   SUBROUTINE:  OnNextBookmark()
c
c   FUNCTION:  Callback function for 'Goto Next Bookmark"
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnNextBookmark()
{
	if (m_pTextBuffer != NULL)
	{
		int nLine = m_pTextBuffer->FindNextBookmarkLine(m_ptCursorPos.y);
		if (nLine >= 0)
		{
			CPoint pt(0, nLine);
			ASSERT_VALIDTEXTPOS(pt);
			SetCursorPos(pt);
			SetSelection(pt, pt);
			SetAnchor(pt);
			EnsureVisible(pt);
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnPrevBookmark()
c
c   FUNCTION:  Callback function for 'Goto Prev Bookmark"
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnPrevBookmark()
{
	if (m_pTextBuffer != NULL)
	{
		int nLine = m_pTextBuffer->FindPrevBookmarkLine(m_ptCursorPos.y);
		if (nLine >= 0)
		{
			CPoint pt(0, nLine);
			ASSERT_VALIDTEXTPOS(pt);
			SetCursorPos(pt);
			SetSelection(pt, pt);
			SetAnchor(pt);
			EnsureVisible(pt);
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnClearAllBookmarks()
c
c   FUNCTION:  Callback function for 'Clear all Bookmark"
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::OnClearAllBookmarks()
{
	if (m_pTextBuffer != NULL)
	{
		int nLineCount = GetLineCount();
		for (int I = 0; I < nLineCount; I ++)
		{
			if (m_pTextBuffer->GetLineFlags(I) & PTED_BOOKMARKS)
				m_pTextBuffer->SetLineFlag(I, PTED_BOOKMARKS, FALSE);
		}
		m_bBookmarkExist = FALSE;
	}							 
}
int CPtedTextView::GetMarginWidth()
{
	return m_bSelMargin ? 20 : 1;
}

/***********************************************************************
c
c   SUBROUTINE:  SaveToFile(char *filename, int ftype, int *classpt, 
c					int class_type)
c
c   FUNCTION:  This function save the current edit text 
c              into a file fileName
c
c   INPUT:  fileName: save file
c			ftype: file type
c			class_type: batch or window
c			classpt: class pointer
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::SaveToFile(char *filename, int ftype, int *classpt, int class_type)
{
	m_pTextBuffer->SaveToFile(filename, ftype, classpt, class_type);
}

/***********************************************************************
c
c   SUBROUTINE:  SaveSelectToFile(char *filename, int ftype, 
c				CPoint &ptStart, CPoint &ptEnd, int *classpt, int class_type)
c
c   FUNCTION:  This function save the current edit text 
c              in certain range into a file fileName
c
c   INPUT:  fileName: save file
c			ftype: file type
c			class_type: batch or window
c			classpt: class pointer
c			ptStart, ptEnd: range to be save
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::SaveSelectToFile(char *filename, int ftype, CPoint &ptStart, CPoint &ptEnd, int *classpt, int class_type)
{
	m_pTextBuffer->SaveSelectToFile(filename, ftype, ptStart, ptEnd, classpt, class_type);
}

/***********************************************************************
c
c   SUBROUTINE:  LoadFromFile(char *filename, int ftype)
c
c   FUNCTION:  This function load the file into the current edit text 
c             
c
c   INPUT:  fileName: file to be load
c			ftype: file type to be load
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::LoadFromFile(char *filename, int ftype)
{
/*
.....close the text view first, then load again
*/
	OnCloseReset();

	int stat = m_pTextBuffer->LoadFromFile(filename, ftype);
	if (stat)
	{
		AttachToBuffer(m_pTextBuffer);
		RedrawWindow();
	}
}
/***********************************************************************
c
c   SUBROUTINE:  LoadFromBuffer(CPtedTextBuffer *TextBuffer)
c
c   FUNCTION:  This function load the a text buffer into the current edit text buffer
c             
c
c   INPUT:  TextBuffer: buufer to be load
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextView::LoadFromBuffer(CPtedTextBuffer *TextBuffer)
{
/*
.....close the text view first, then load again
*/
/*	OnCloseReset();
	m_pTextBuffer->InitNew();

	int size = TextBuffer->GetLineCount();
	m_pTextBuffer->SetSize(size);
	m_pTextBuffer->Copy(TextBuffer);
*/
	DetachFromBuffer();
	m_pTextBuffer = TextBuffer;
	AttachToBuffer(m_pTextBuffer);
//	SetModifiedFlag(1);
	m_pTextBuffer->SetModified(1);
	RedrawWindow();
}
/***********************************************************************
c
c   SUBROUTINE: LoadCutterFile(char *filename)
c
c   FUNCTION:  This functions loads a cutter file into current edit window 
c
c   INPUT:  filename: file to load
c
c   OUTPUT: none
c
c***********************************************************************
*/
int CPtedTextView::LoadCutterFile(char *filename)
{
	int err = 0;
	char msg[256];
	m_pTextBuffer->LoadCutterFile(filename, msg, &err);
	int stat = 1;
	if (err)
	{
		MessageBox(msg, "Loading Cutter File Error", MB_OK);
		stat = 0;
	}
	RedrawWindow();
	return stat;
}
