/************************************************************************
c
c   FILE NAME: PtedTextView.cpp
c
c   CONTAINS:
c		CPtedTextView::CPtedTextView()
c		CPtedTextView::OnDestroy() 
c		CPtedTextView::~CPtedTextView()
c		CPtedTextView::SetParent(CWnd *parent)
c		CPtedTextView::PreCreateWindow(CREATESTRUCT& cs)
c		CPtedTextView::GetSelection(CPoint &ptStart, CPoint &ptEnd)
c		CPtedTextView::GetLineActualLength(int nLineIndex)
c		CPtedTextView::ScrollToChar(int nNewOffsetChar, BOOL bNoSmoothScroll, BOOL bTrackScrollBar)
c		CPtedTextView::ScrollToLine(int nNewTopLine, BOOL bNoSmoothScroll, BOOL bTrackScrollBar)
c		CPtedTextView::ExpandChars(LPCTSTR pszChars, int nOffset, int nCount, CString &line)
c		CPtedTextView::DrawTextLineImpl(CDC *pdc, CPoint &ptOrigin, const CRect &rcClip,
c		CPtedTextView::DrawTextLine(CDC *pdc, CPoint &ptOrigin, const CRect &rcClip, int nColorIndex,
c		CPtedTextView::GetLineColors(int nLineIndex, COLORREF &crBkgnd,
c					COLORREF &crText, BOOL &bDrawWhitespace)
c		CPtedTextView::DrawSingleLine(CDC *pdc, const CRect &rc, int nLineIndex)
c		CPtedTextView::GetColor(int nColorIndex)
c		CPtedTextView::GetLineFlags(int nLineIndex)
c		CPtedTextView::DrawMargin(CDC *pdc, const CRect &rect, int nLineIndex)
c		CPtedTextView::IsInsideSelBlock(CPoint ptTextPos)
c		CPtedTextView::IsInsideSelection(const CPoint &ptTextPos)
c		CPtedTextView::PrepareSelBounds()
c		CPtedTextView::OnDraw(CDC *pdc)
c		CPtedTextView::ResetView()
c		CPtedTextView::UpdateCaret()
c		CPtedTextView::BeginUndoGroup(BOOL bMergeWithPrevious)
c		CPtedTextView::FlushUndoGroup()
c....not working yet for drag &drop
c		DROPEFFECT CEditDropTargetImpl::OnDragEnter(CWnd* pWnd, COleDataObject* pDataObject, DWORD dwKeyState, CPoint point)
c		CEditDropTargetImpl::OnDragLeave(CWnd* pWnd)
c		CEditDropTargetImpl::OnDragOver(CWnd* pWnd, COleDataObject* pDataObject, DWORD dwKeyState, CPoint point)
c		CEditDropTargetImpl::OnDrop(CWnd* pWnd, COleDataObject* pDataObject, DROPEFFECT dropEffect, CPoint point)
c		CEditDropTargetImpl::OnDragScroll(CWnd* pWnd, DWORD dwKeyState, CPoint point)
c		CPtedTextView::DoDragScroll(const CPoint &point)
c		CPtedTextView::DoDropText(COleDataObject *pDataObject, const CPoint &ptClient)
c		CPtedTextView::ShowDropIndicator(const CPoint &point)
c		CPtedTextView::HideDropIndicator()
c		CPtedTextView::GetDropEffect()
c		CPtedTextView::OnDropSource(DROPEFFECT de)
c		CPtedTextView::Disp_Msg(char *msg, int flag)
c		CPtedTextView::OnCloseReset()
c	
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c	MODULE NAME AND RELEASE LEVEL
c       PtedTextView.cpp , 24.1
c	DATE AND TIME OF LAST  MODIFICATION
c       09/11/13 , 12:59:28
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include <malloc.h>
#include "PtedEditCmd.h"
#include "PtedTextView.h"
#include "PtedTextBuffer.h"
#include "PtdFunc.h"
#include <time.h>

#include <afxpriv.h>
#include <afxole.h>
#include "PtedRangeBox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

// zero fill everything after the vtbl pointer
#define AFX_ZERO_INIT_OBJECT(base_class) \
memset(((base_class*)this)+1, 0, sizeof(*this) - sizeof(class base_class));

#define TAB_CHARACTER				_T('\xBB')
#define SPACE_CHARACTER				_T('\x95')

#define SMOOTH_SCROLL_FACTOR		6
//#define DRAG_BORDER_X		5
//#define DRAG_BORDER_Y		5
/*
class CEditDropTargetImpl : public COleDropTarget
{
private:
	CPtedTextView *m_pOwner;
public:
	CEditDropTargetImpl(CPtedTextView *pOwner) { m_pOwner = pOwner; };

	virtual DROPEFFECT OnDragEnter(CWnd* pWnd, COleDataObject* pDataObject, DWORD dwKeyState, CPoint point);
	virtual void OnDragLeave(CWnd* pWnd);
	virtual DROPEFFECT OnDragOver(CWnd* pWnd, COleDataObject* pDataObject, DWORD dwKeyState, CPoint point);
	virtual BOOL OnDrop(CWnd* pWnd, COleDataObject* pDataObject, DROPEFFECT dropEffect, CPoint point);
	virtual DROPEFFECT OnDragScroll(CWnd* pWnd, DWORD dwKeyState, CPoint point);
};
*/
extern COLORREF Ptd_editcolor[102] ;
////////////////////////////////////////////////////////////////////////////
// CPtedTextView

IMPLEMENT_DYNCREATE(CPtedTextView, CView)

HINSTANCE CPtedTextView::s_hResourceInst = NULL;

BEGIN_MESSAGE_MAP(CPtedTextView, CView)
	//{{AFX_MSG_MAP(CPtedTextView)
	ON_WM_DESTROY()
	ON_WM_ERASEBKGND()
	ON_WM_SIZE()
	ON_WM_VSCROLL()
	ON_WM_SETCURSOR()
	ON_WM_LBUTTONDOWN()
	ON_WM_MOUSEWHEEL()
	ON_WM_SETFOCUS()
	ON_WM_HSCROLL()
	ON_WM_LBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_WM_KEYDOWN ()
	ON_WM_TIMER()
	ON_WM_KILLFOCUS()
	ON_WM_LBUTTONDBLCLK()
	ON_COMMAND(ID_EDIT_DELETE_BACK, OnEditDeleteBack)
	ON_COMMAND(ID_EDIT_UNTAB, OnEditUntab)
	ON_COMMAND(ID_EDIT_TAB, OnEditTab)
	ON_COMMAND(ID_EDIT_SELECT_ALL, OnEditSelectAll)
	ON_WM_RBUTTONDOWN()
	ON_WM_SYSCOLORCHANGE()
	ON_WM_CREATE()
	//}}AFX_MSG_MAP
	ON_COMMAND(ID_EDIT_CHAR_LEFT, OnCharLeft)
	ON_COMMAND(ID_EDIT_EXT_CHAR_LEFT, OnExtCharLeft)
	ON_COMMAND(ID_EDIT_CHAR_RIGHT, OnCharRight)
	ON_COMMAND(ID_EDIT_EXT_CHAR_RIGHT, OnExtCharRight)
	ON_COMMAND(ID_EDIT_WORD_LEFT, OnWordLeft)
	ON_COMMAND(ID_EDIT_EXT_WORD_LEFT, OnExtWordLeft)
	ON_COMMAND(ID_EDIT_WORD_RIGHT, OnWordRight)
	ON_COMMAND(ID_EDIT_EXT_WORD_RIGHT, OnExtWordRight)
	ON_COMMAND(ID_EDIT_LINE_UP, OnLineUp)
	ON_COMMAND(ID_EDIT_EXT_LINE_UP, OnExtLineUp)
	ON_COMMAND(ID_EDIT_LINE_DOWN, OnLineDown)
	ON_COMMAND(ID_EDIT_EXT_LINE_DOWN, OnExtLineDown)
	ON_COMMAND(ID_EDIT_SCROLL_UP, ScrollUp)
	ON_COMMAND(ID_EDIT_SCROLL_DOWN, ScrollDown)
	ON_COMMAND(ID_EDIT_PAGE_UP, OnPageUp)
	ON_COMMAND(ID_EDIT_EXT_PAGE_UP, OnExtPageUp)
	ON_COMMAND(ID_EDIT_PAGE_DOWN, OnPageDown)
	ON_COMMAND(ID_EDIT_EXT_PAGE_DOWN, OnExtPageDown)
	ON_COMMAND(ID_EDIT_LINE_END, OnLineEnd)
	ON_COMMAND(ID_EDIT_EXT_LINE_END, OnExtLineEnd)
	ON_COMMAND(ID_EDIT_HOME, OnHome)
	ON_COMMAND(ID_EDIT_EXT_HOME, OnExtHome)
	ON_COMMAND(ID_EDIT_TEXT_BEGIN, OnTextBegin)
	ON_COMMAND(ID_EDIT_EXT_TEXT_BEGIN, OnExtTextBegin)
	ON_COMMAND(ID_EDIT_TEXT_END, OnTextEnd)
	ON_COMMAND(ID_EDIT_EXT_TEXT_END, OnExtTextEnd)
	ON_COMMAND_RANGE(ID_EDIT_TOGGLE_BOOKMARK0, ID_EDIT_TOGGLE_BOOKMARK9, OnToggleBookmark)
	ON_COMMAND_RANGE(ID_EDIT_GO_BOOKMARK0, ID_EDIT_GO_BOOKMARK9, OnGoBookmark)
	ON_COMMAND(ID_EDIT_CLEAR_BOOKMARKS, OnClearBookmarks)
	ON_COMMAND(ID_EDIT_TOGGLE_BOOKMARK,     OnToggleBookmark)
	ON_COMMAND(ID_EDIT_GOTO_NEXT_BOOKMARK,  OnNextBookmark)
	ON_COMMAND(ID_EDIT_GOTO_PREV_BOOKMARK,  OnPrevBookmark)
	ON_COMMAND(ID_EDIT_CLEAR_ALL_BOOKMARKS, OnClearAllBookmarks)
	ON_COMMAND(ID_EDIT_DELETE, OnEditDelete)
	ON_WM_CHAR()
	ON_COMMAND(ID_EDIT_UNTAB, OnEditUntab)
	ON_COMMAND(ID_EDIT_TAB, OnEditTab)
	ON_COMMAND(ID_EDIT_SWITCH_OVRMODE, OnEditSwitchOvrmode)
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_COMMAND(ID_EDIT_UNDO, OnEditUndo)
	ON_COMMAND(ID_EDIT_REDO, OnEditRedo)
END_MESSAGE_MAP()

#define EXPAND_PRIMITIVE(impl, func)	\
	void CPtedTextView::On##func() { impl(FALSE); }	\
	void CPtedTextView::OnExt##func() { impl(TRUE); }
	EXPAND_PRIMITIVE(MoveLeft, CharLeft)
	EXPAND_PRIMITIVE(MoveRight, CharRight)
	EXPAND_PRIMITIVE(MoveWordLeft, WordLeft)
	EXPAND_PRIMITIVE(MoveWordRight, WordRight)
	EXPAND_PRIMITIVE(MoveUp, LineUp)
	EXPAND_PRIMITIVE(MoveDown, LineDown)
	EXPAND_PRIMITIVE(MovePgUp, PageUp)
	EXPAND_PRIMITIVE(MovePgDn, PageDown)
	EXPAND_PRIMITIVE(MoveHome, Home)
	EXPAND_PRIMITIVE(MoveEnd, LineEnd)
	EXPAND_PRIMITIVE(MoveCtrlHome, TextBegin)
	EXPAND_PRIMITIVE(MoveCtrlEnd, TextEnd)
#undef EXPAND_PRIMITIVE



/////////////////////////////////////////////////////////////////////////////
// CPtedTextView construction/destruction


/***********************************************************************
**
**   FUNCTION: OnDestroy() 
**
**		OnDestroy is called after the CPtedTextView object 
**		is removed from the screen. 
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CPtedTextView::OnDestroy() 
{
	if (m_pDropTarget != NULL)
	{
		m_pDropTarget->Revoke();
		delete m_pDropTarget;
		m_pDropTarget = NULL;
	}

	DetachFromBuffer();
	m_hAccel = NULL;

	CView::OnDestroy();

	for (int I = 0; I < 4; I ++)
	{
		if (m_apFonts[I] != NULL)
		{
			m_apFonts[I]->DeleteObject();
			delete m_apFonts[I];
			m_apFonts[I] = NULL;
		}
	}
	if (m_pCacheBitmap != NULL)
	{
		delete m_pCacheBitmap;
		m_pCacheBitmap = NULL;
	}
	delete this;
}

CPtedTextView::CPtedTextView(int mark_flag)
{
	AFX_ZERO_INIT_OBJECT(CView);
	m_pdwParseCookies = NULL;
	m_bDisableBSAtSOL = FALSE;
	m_bSelMargin = mark_flag;
	m_syntax_color = 1;
	ResetView();
	m_bAutoIndent = TRUE;
	m_pTextBuffer = new CPtedTextBuffer();
	m_pTextBuffer->InitNew();
	m_no_update = 0;
	m_textpopup = NULL;
}

CPtedTextView::~CPtedTextView()
{
	ASSERT(m_hAccel == NULL);
	ASSERT(m_pCacheBitmap == NULL);
	ASSERT(m_pTextBuffer == NULL);	
	if (m_pdwParseCookies != NULL)
	{
		delete m_pdwParseCookies;
		m_pdwParseCookies = NULL;
	}
	if (m_pnActualLineLength != NULL)
	{
		delete m_pnActualLineLength;
		m_pnActualLineLength = NULL;
	}
	if (m_textpopup != NULL)
	{
		delete 	m_textpopup;
		m_textpopup = NULL;
	}
}

void CPtedTextView::SetParent(CWnd *parent)
{
	m_parent = parent;
}

/***********************************************************************
**
**   FUNCTION: PreCreateWindow(CREATESTRUCT& cs)
**
**       Called by the framework before the creation 
**		of the Windows window attached to this CWnd object.
**		Override this member function to modify the 
**		CREATESTRUCT structure before the window is created. 
**
**		Never call this function directly.
**
**
**   INPUT:  CREATESTRUCT& cs
**
**   OUTPUT :   CREATESTRUCT& cs
**   RETURN:    None
**
**********************************************************************/
BOOL CPtedTextView::PreCreateWindow(CREATESTRUCT& cs)
{
	CWnd *pParentWnd = CWnd::FromHandlePermanent(cs.hwndParent);
	if (pParentWnd == NULL || ! pParentWnd->IsKindOf(RUNTIME_CLASS(CSplitterWnd)))
	{
		cs.style |= (WS_HSCROLL | WS_VSCROLL);
	}
//	cs.lpszClass = AfxRegisterWndClass(CS_DBLCLKS);
	cs.lpszClass = AfxRegisterWndClass(CS_HREDRAW|CS_VREDRAW);
//	return CView::PreCreateWindow(cs);
	ASSERT(cs.style & WS_CHILD);
	int stat = CView::PreCreateWindow(cs);
	return stat;
}


/////////////////////////////////////////////////////////////////////////////
// CPtedTextView drawing

/***********************************************************************
**
**   FUNCTION: GetSelection(CPoint &ptStart, CPoint &ptEnd)
**		Get the current selection
**
**
**
**   INPUT: none
**
**   OUTPUT :   ptStart, ptEnd: selection
**   RETURN:    None
**
**********************************************************************/
void CPtedTextView::GetSelection(CPoint &ptStart, CPoint &ptEnd)
{
	PrepareSelBounds();
	ptStart = m_ptDrawSelStart;
	ptEnd = m_ptDrawSelEnd;
}

/***********************************************************************
**
**   FUNCTION: GetLineActualLength(int nLineIndex)
**		Get the line actual length
**
**
**   INPUT: nLineIndex: line index
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CPtedTextView::GetLineActualLength(int nLineIndex)
{
	int nLineCount = GetLineCount();
	ASSERT(nLineCount > 0);
	ASSERT(nLineIndex >= 0 && nLineIndex < nLineCount);
	if (m_pnActualLineLength == NULL)
	{
		m_pnActualLineLength = new int[nLineCount];
		memset(m_pnActualLineLength, 0xff, sizeof(int) * nLineCount);
		m_nActualLengthArraySize = nLineCount;
	}

	if (m_pnActualLineLength[nLineIndex] >= 0)
		return m_pnActualLineLength[nLineIndex];

	int nActualLength = 0;
	int nLength = GetLineLength(nLineIndex);
	if (nLength > 0)
	{
		LPCTSTR pszLine = GetLineChars(nLineIndex);
		LPTSTR pszChars = (LPTSTR) _alloca(sizeof(TCHAR) * (nLength + 1));
		memcpy(pszChars, pszLine, sizeof(TCHAR) * nLength);
		pszChars[nLength] = 0;
		LPTSTR pszCurrent = pszChars;

		int nTabSize = GetTabSize();
		for (;;)
		{
#ifdef _UNICODE
			LPTSTR psz = wcschr(pszCurrent, L'\t');
#else
			LPTSTR psz = strchr(pszCurrent, '\t');
#endif
			if (psz == NULL)
			{
				nActualLength += (pszChars + nLength - pszCurrent);
				break;
			}

			nActualLength += (psz - pszCurrent);
			nActualLength += (nTabSize - nActualLength % nTabSize);
			pszCurrent = psz + 1;
		}
	}

	m_pnActualLineLength[nLineIndex] = nActualLength;
	return nActualLength;
}

/***********************************************************************
**
**   FUNCTION: ScrollToChar(int nNewOffsetChar, BOOL bNoSmoothScroll 
**					BOOL bTrackScrollBar)
**		scroll to nNewOffsetChar characters (horizonal)
**
**
**   INPUT: nNewOffsetChar: characters to scroll
**			bNoSmoothScroll: not used
**			bTrackScrollBar: if recalculate scrollbar
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CPtedTextView::ScrollToChar(int nNewOffsetChar, BOOL bNoSmoothScroll /*= FALSE*/, BOOL bTrackScrollBar /*= TRUE*/)
{
	if (m_nOffsetChar != nNewOffsetChar)
	{
		int nScrollChars = m_nOffsetChar - nNewOffsetChar;
		m_nOffsetChar = nNewOffsetChar;
		CRect rcScroll;
		GetClientRect(&rcScroll);
		rcScroll.left += GetMarginWidth();
		ScrollWindow(nScrollChars * GetCharWidth(), 0, &rcScroll, &rcScroll);
		UpdateWindow();
		if (bTrackScrollBar)
			RecalcHorzScrollBar(TRUE);
	}
}

/***********************************************************************
**
**   FUNCTION: ScrollToLine(int nNewTopLine, BOOL bNoSmoothScroll 
**					BOOL bTrackScrollBar)
**		scroll to nNewTopLine line
**
**
**   INPUT: nNewTopLine: line to scroll to
**			bNoSmoothScroll: 
**			bTrackScrollBar: if recalculate scrollbar
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CPtedTextView::ScrollToLine(int nNewTopLine, BOOL bNoSmoothScroll /*= FALSE*/, BOOL bTrackScrollBar /*= TRUE*/)
{
	if (m_nTopLine != nNewTopLine)
	{
		if (bNoSmoothScroll || ! m_bSmoothScroll)
		{
			int nScrollLines = m_nTopLine - nNewTopLine;
			m_nTopLine = nNewTopLine;
			ScrollWindow(0, nScrollLines * GetLineHeight());
			UpdateWindow();
			if (bTrackScrollBar)
				RecalcVertScrollBar(TRUE);
		}
		else
		{
			int nLineHeight = GetLineHeight();
			if (m_nTopLine > nNewTopLine)
			{
				int nIncrement = (m_nTopLine - nNewTopLine) / SMOOTH_SCROLL_FACTOR + 1;
				while (m_nTopLine != nNewTopLine)
				{
					int nTopLine = m_nTopLine - nIncrement;
					if (nTopLine < nNewTopLine)
						nTopLine = nNewTopLine;
					int nScrollLines = nTopLine - m_nTopLine;
					m_nTopLine = nTopLine;
					ScrollWindow(0, - nLineHeight * nScrollLines);
					UpdateWindow();
					if (bTrackScrollBar)
						RecalcVertScrollBar(TRUE);
				}
			}
			else
			{
				int nIncrement = (nNewTopLine - m_nTopLine) / SMOOTH_SCROLL_FACTOR + 1;
				while (m_nTopLine != nNewTopLine)
				{
					int nTopLine = m_nTopLine + nIncrement;
					if (nTopLine > nNewTopLine)
						nTopLine = nNewTopLine;
					int nScrollLines = nTopLine - m_nTopLine;
					m_nTopLine = nTopLine;
					ScrollWindow(0, - nLineHeight * nScrollLines);
					UpdateWindow();
					if (bTrackScrollBar)
						RecalcVertScrollBar(TRUE);
				}
			}
		}
	}
}

/***********************************************************************
**
**   FUNCTION: ExpandChars(LPCTSTR pszChars, int nOffset, int nCount, CString &line)
**		expand the characters with spaces replace of tabs, for drawing routine
**
**
**   INPUT: pszChars: string to be expand
**			nOffset: 
**			nCount: 
**   OUTPUT :   line: expanded string
**   RETURN:    None
**
**********************************************************************/
void CPtedTextView::ExpandChars(LPCTSTR pszChars, int nOffset, int nCount, CString &line)
{
	int I;

	if (nCount <= 0)
	{
		line = _T("");
		return;
	}

	int nTabSize = GetTabSize();

	int nActualOffset = 0;
	for (I = 0; I < nOffset; I ++)
	{
		if (pszChars[I] == _T('\t'))
			nActualOffset += (nTabSize - nActualOffset % nTabSize);
		else
			nActualOffset ++;
	}

	pszChars += nOffset;
	int nLength = nCount;

	int nTabCount = 0;
	for (I = 0; I < nLength; I ++)
	{
		if (pszChars[I] == _T('\t'))
			nTabCount ++;
	}

	LPTSTR pszBuf = line.GetBuffer(nLength + nTabCount * (nTabSize - 1) + 1);
	int nCurPos = 0;
	if (nTabCount > 0 || m_bViewTabs)
	{
		for (I = 0; I < nLength; I ++)
		{
			if (pszChars[I] == _T('\t'))
			{
				int nSpaces = nTabSize - (nActualOffset + nCurPos) % nTabSize;
				if (m_bViewTabs)
				{
					pszBuf[nCurPos ++] = TAB_CHARACTER;
					nSpaces --;
				}
				while (nSpaces > 0)
				{
					pszBuf[nCurPos ++] = _T(' ');
					nSpaces --;
				}
			}
			else
			{
				if (pszChars[I] == _T(' ') && m_bViewTabs)
					pszBuf[nCurPos] = SPACE_CHARACTER;
				else
					pszBuf[nCurPos] = pszChars[I];
				nCurPos ++;
			}
		}
	}
	else
	{
		memcpy(pszBuf, pszChars, sizeof(TCHAR) * nLength);
		nCurPos = nLength;
	}
	pszBuf[nCurPos] = 0;
	line.ReleaseBuffer();
}

/***********************************************************************
**
**   FUNCTION: DrawTextLineImpl(CDC *pdc, CPoint &ptOrigin, const CRect &rcClip,
**					 LPCTSTR pszChars, int nOffset, int nCount)
**
**       Draw text line
**
**   INPUT: 
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::DrawTextLineImpl(CDC *pdc, CPoint &ptOrigin, const CRect &rcClip,
									 LPCTSTR pszChars, int nOffset, int nCount)
{
	ASSERT(nCount >= 0);
	if (nCount > 0)
	{
		CString line;
		ExpandChars(pszChars, nOffset, nCount, line);
		int nWidth = rcClip.right - ptOrigin.x;
		if (nWidth > 0)
		{
			int nCharWidth = GetCharWidth();
			int nCount = line.GetLength();
			int nCountFit = nWidth / nCharWidth + 1;
			if (nCount > nCountFit)
				nCount = nCountFit;
			VERIFY(pdc->ExtTextOut(ptOrigin.x, ptOrigin.y, ETO_CLIPPED, &rcClip, line, nCount, NULL));
		}
		ptOrigin.x += GetCharWidth() * line.GetLength();
	}
}

/***********************************************************************
**
**   FUNCTION: DrawTextLine(CDC *pdc, CPoint &ptOrigin, const CRect &rcClip, int nColorIndex,
**					 LPCTSTR pszChars, int nOffset, int nCount, CPoint ptTextPos)
**
**       Draw text line
**
**   INPUT: 
**
**   OUTPUT : 
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::DrawTextLine(CDC *pdc, CPoint &ptOrigin, const CRect &rcClip, int nColorIndex,
									 LPCTSTR pszChars, int nOffset, int nCount, CPoint ptTextPos)
{
	if (nCount > 0)
	{
		if (m_bFocused || m_bShowInactiveSelection)
		{
			int nSelBegin = 0, nSelEnd = 0;
			if (m_ptDrawSelStart.y > ptTextPos.y)
			{
				nSelBegin = nCount;
			}
			else
			if (m_ptDrawSelStart.y == ptTextPos.y)
			{
				nSelBegin = m_ptDrawSelStart.x - ptTextPos.x;
				if (nSelBegin < 0)
					nSelBegin = 0;
				if (nSelBegin > nCount)
					nSelBegin = nCount;
			}
			if (m_ptDrawSelEnd.y > ptTextPos.y)
			{
				nSelEnd = nCount;
			}
			else
			if (m_ptDrawSelEnd.y == ptTextPos.y)
			{
				nSelEnd = m_ptDrawSelEnd.x - ptTextPos.x;
				if (nSelEnd < 0)
					nSelEnd = 0;
				if (nSelEnd > nCount)
					nSelEnd = nCount;
			}

			ASSERT(nSelBegin >= 0 && nSelBegin <= nCount);
			ASSERT(nSelEnd >= 0 && nSelEnd <= nCount);
			ASSERT(nSelBegin <= nSelEnd);

			if (nSelBegin > 0)
			{
				DrawTextLineImpl(pdc, ptOrigin, rcClip, pszChars, nOffset, nSelBegin);
			}
			if (nSelBegin < nSelEnd)
			{
				COLORREF crOldBk = pdc->SetBkColor(GetColor(COLORINDEX_SELBKGND));
				COLORREF crOldText = pdc->SetTextColor(GetColor(COLORINDEX_SELTEXT));
				DrawTextLineImpl(pdc, ptOrigin, rcClip, pszChars, nOffset + nSelBegin, nSelEnd - nSelBegin);
				pdc->SetBkColor(crOldBk);
				pdc->SetTextColor(crOldText);
			}
			if (nSelEnd < nCount)
			{
				DrawTextLineImpl(pdc, ptOrigin, rcClip, pszChars, nOffset + nSelEnd, nCount - nSelEnd);
			}
		}
		else
		{
			DrawTextLineImpl(pdc, ptOrigin, rcClip, pszChars, nOffset, nCount);
		}
	}
}

/***********************************************************************
**
**   FUNCTION: GetLineColors(int nLineIndex, COLORREF &crBkgnd,
**					COLORREF &crText, BOOL &bDrawWhitespace)
**
**       Get Line Colors (text and background color)
**
**   INPUT:  bDrawWhitespace: 
**			nLineIndex: line index to draw
**
**   OUTPUT : crText, crBkgnd: text and background color
**   RETURN:   none
**
**********************************************************************/
void CPtedTextView::GetLineColors(int nLineIndex, COLORREF &crBkgnd,
					COLORREF &crText, BOOL &bDrawWhitespace)
{
	DWORD dwLineFlags = GetLineFlags(nLineIndex);
	bDrawWhitespace = TRUE;
	crBkgnd = CLR_NONE;
	crText = CLR_NONE;
	if ((dwLineFlags & PTED_SELMARKS) != 0)
	{
		crText = RGB(255, 255, 255);
		crBkgnd = RGB(0, 0, 255);
		bDrawWhitespace = TRUE;
	}
	else
		bDrawWhitespace = FALSE;
}


/***********************************************************************
**
**   FUNCTION: DrawSingleLine(CDC *pdc, const CRect &rc, int nLineIndex)
**
**       Draw Single Line
**
**   INPUT:  pdc: 
**			rc: 
**			nLineIndex: line index to draw
**
**   OUTPUT :   none
**   RETURN:    color
**
**********************************************************************/
void CPtedTextView::DrawSingleLine(CDC *pdc, const CRect &rc, int nLineIndex)
{
	ASSERT(nLineIndex >= -1 && nLineIndex < GetLineCount());

	if (nLineIndex == -1)
	{
		pdc->FillSolidRect(rc, GetColor(COLORINDEX_WHITESPACE));
		return;
	}

	BOOL bDrawWhitespace = FALSE;
	COLORREF crBkgnd, crText;
	GetLineColors(nLineIndex, crBkgnd, crText, bDrawWhitespace);
	if (crBkgnd == CLR_NONE)
		crBkgnd = GetColor(COLORINDEX_BKGND);

	int nLength = GetLineLength(nLineIndex);
	if (nLength == 0)
	{
		CRect rect = rc;
		if ((m_bFocused || m_bShowInactiveSelection) && IsInsideSelBlock(CPoint(0, nLineIndex)))
		{
			pdc->FillSolidRect(rect.left, rect.top, GetCharWidth(), rect.Height(), GetColor(COLORINDEX_SELBKGND));
			rect.left += GetCharWidth();
		}
		pdc->FillSolidRect(rect, bDrawWhitespace ? crBkgnd : GetColor(COLORINDEX_WHITESPACE));
		return;
	}

/*	
......Parse the line
*/
	LPCTSTR pszChars = GetLineChars(nLineIndex);
	DWORD dwCookie = GetParseCookie(nLineIndex-1);
	TEXTBLOCK *pBuf = (TEXTBLOCK *) _alloca(sizeof(TEXTBLOCK) * nLength * 3);
	int nBlocks = 0;
	m_pdwParseCookies[nLineIndex] = ParseLine(dwCookie, nLineIndex, pBuf, nBlocks);
	ASSERT(m_pdwParseCookies[nLineIndex] != (DWORD) -1);
/*
......Draw the line text
*/
	CPoint origin(rc.left - m_nOffsetChar * GetCharWidth(), rc.top);
	pdc->SetBkColor(crBkgnd);
	if (crText != CLR_NONE)
		pdc->SetTextColor(crText);

	BOOL bColorSet = FALSE;

	if (nBlocks > 0)
	{
		ASSERT(pBuf[0].m_nCharPos >= 0 && pBuf[0].m_nCharPos <= nLength);
		if (crText == CLR_NONE)
			pdc->SetTextColor(GetColor(COLORINDEX_NORMALTEXT));

		pdc->SelectObject(GetFont(GetItalic(COLORINDEX_NORMALTEXT), GetBold(COLORINDEX_NORMALTEXT)));
		DrawTextLine(pdc, origin, rc, COLORINDEX_NORMALTEXT, pszChars, 0, pBuf[0].m_nCharPos, CPoint(0, nLineIndex));
		for (int I = 0; I < nBlocks - 1; I ++)
		{
			ASSERT(pBuf[I].m_nCharPos >= 0 && pBuf[I].m_nCharPos <= nLength);
			if (crText == CLR_NONE)
				pdc->SetTextColor(GetColor(pBuf[I].m_nColorIndex));
			pdc->SelectObject(GetFont(GetItalic(pBuf[I].m_nColorIndex), GetBold(pBuf[I].m_nColorIndex)));
			DrawTextLine(pdc, origin, rc, pBuf[I].m_nColorIndex, pszChars,
							pBuf[I].m_nCharPos, pBuf[I + 1].m_nCharPos - pBuf[I].m_nCharPos,
							CPoint(pBuf[I].m_nCharPos, nLineIndex));
		}
		ASSERT(pBuf[nBlocks - 1].m_nCharPos >= 0 && pBuf[nBlocks - 1].m_nCharPos <= nLength);
		if (crText == CLR_NONE)
			pdc->SetTextColor(GetColor(pBuf[nBlocks - 1].m_nColorIndex));
		pdc->SelectObject(GetFont(GetItalic(pBuf[nBlocks - 1].m_nColorIndex),
							GetBold(pBuf[nBlocks - 1].m_nColorIndex)));
		DrawTextLine(pdc, origin, rc, pBuf[nBlocks - 1].m_nColorIndex, pszChars,
							pBuf[nBlocks - 1].m_nCharPos, nLength - pBuf[nBlocks - 1].m_nCharPos,
							CPoint(pBuf[nBlocks - 1].m_nCharPos, nLineIndex));
	}
	else
	{
		if (crText == CLR_NONE)
			pdc->SetTextColor(GetColor(COLORINDEX_NORMALTEXT));
		pdc->SelectObject(GetFont(GetItalic(COLORINDEX_NORMALTEXT), GetBold(COLORINDEX_NORMALTEXT)));
		DrawTextLine(pdc, origin, rc, COLORINDEX_NORMALTEXT, pszChars, 0, nLength, CPoint(0, nLineIndex));
	}

	CRect frect = rc;
	if (origin.x > frect.left)
		frect.left = origin.x;
	if (frect.right > frect.left)
	{
		if ((m_bFocused || m_bShowInactiveSelection) && IsInsideSelBlock(CPoint(nLength, nLineIndex)))
		{
			pdc->FillSolidRect(frect.left, frect.top, GetCharWidth(), frect.Height(),
												GetColor(COLORINDEX_SELBKGND));
			frect.left += GetCharWidth();
		}
		if (frect.right > frect.left)
			pdc->FillSolidRect(frect, bDrawWhitespace ? crBkgnd : GetColor(COLORINDEX_WHITESPACE));
	}
}

/***********************************************************************
**
**   FUNCTION: GetColor(int nColorIndex)
**
**       Get Color
**
**   INPUT:  nColorIndex: color index to get thecolor
**
**   OUTPUT :   none
**   RETURN:    color
**
**********************************************************************/
COLORREF CPtedTextView::GetColor(int nColorIndex)
{
	switch (nColorIndex)
	{
	case COLORINDEX_WHITESPACE:
	case COLORINDEX_BKGND:
		return ::GetSysColor(COLOR_WINDOW);
	case COLORINDEX_NORMALTEXT:
		return ::GetSysColor(COLOR_WINDOWTEXT);
	case COLORINDEX_SELMARGIN:
		return ::GetSysColor(COLOR_SCROLLBAR);
	case COLORINDEX_SELBKGND:
		return RGB(0, 0, 0);
	case COLORINDEX_SELTEXT:
		return RGB(255, 255, 255);
	case COLORINDEX_FINDTEXT:
		return RGB(255, 0, 0);

	case COLORINDEX_MSG:
		return Ptd_editcolor[0];
	case COLORINDEX_MSGTEXT:
		return Ptd_editcolor[1];
	case COLORINDEX_NUMBER:
		return Ptd_editcolor[2];
	case COLORINDEX_SYMBOLS:
		return Ptd_editcolor[3];
	case COLORINDEX_MAJOR:
		return Ptd_editcolor[4];
	case COLORINDEX_MINOR:
		return Ptd_editcolor[5];
	case COLORINDEX_EOB:
		return Ptd_editcolor[6];
	case COLORINDEX_OPSKIP:
		return Ptd_editcolor[7];
	case COLORINDEX_TAB:
		return Ptd_editcolor[8];
	case COLORINDEX_REWIND:
		return Ptd_editcolor[9];
	case COLORINDEX_A1:
		return Ptd_editcolor[10];
	case COLORINDEX_A2:
		return Ptd_editcolor[11];
	case COLORINDEX_A3:
		return Ptd_editcolor[12];
	case COLORINDEX_B1:
		return Ptd_editcolor[13];
	case COLORINDEX_B2:
		return Ptd_editcolor[14];
	case COLORINDEX_B3:
		return Ptd_editcolor[15];
	case COLORINDEX_C1:
		return Ptd_editcolor[16];
	case COLORINDEX_C2:
		return Ptd_editcolor[17];
	case COLORINDEX_C3:
		return Ptd_editcolor[18];
	case COLORINDEX_C4:
		return Ptd_editcolor[19];
	case COLORINDEX_C5:
		return Ptd_editcolor[20];
	case COLORINDEX_C6:
		return Ptd_editcolor[21];
	case COLORINDEX_D:
		return Ptd_editcolor[22];
	case COLORINDEX_E:
		return Ptd_editcolor[23];
	case COLORINDEX_F1:
		return Ptd_editcolor[24];
	case COLORINDEX_F2:
		return Ptd_editcolor[25];
	case COLORINDEX_F3:
		return Ptd_editcolor[26];
	case COLORINDEX_G0:
		return Ptd_editcolor[27];
	case COLORINDEX_G1:
		return Ptd_editcolor[28];
	case COLORINDEX_G2:
		return Ptd_editcolor[29];
	case COLORINDEX_G3:
		return Ptd_editcolor[30];
	case COLORINDEX_G4:
		return Ptd_editcolor[31];
	case COLORINDEX_G5:
		return Ptd_editcolor[32];
	case COLORINDEX_G6:
		return Ptd_editcolor[33];
	case COLORINDEX_G7:
		return Ptd_editcolor[34];
	case COLORINDEX_G8:
		return Ptd_editcolor[35];
	case COLORINDEX_G9:
		return Ptd_editcolor[36];
	case COLORINDEX_GA:
		return Ptd_editcolor[37];
	case COLORINDEX_H:
		return Ptd_editcolor[38];
	case COLORINDEX_I1:
		return Ptd_editcolor[39];
	case COLORINDEX_I2:
		return Ptd_editcolor[40];
	case COLORINDEX_J1:
		return Ptd_editcolor[41];
	case COLORINDEX_J2:
		return Ptd_editcolor[42];
	case COLORINDEX_K1:
		return Ptd_editcolor[43];
	case COLORINDEX_K2:
		return Ptd_editcolor[44];
	case COLORINDEX_L:
		return Ptd_editcolor[45];
	case COLORINDEX_M0:
		return Ptd_editcolor[46];
	case COLORINDEX_M1:
		return Ptd_editcolor[47];
	case COLORINDEX_M2:
		return Ptd_editcolor[48];
	case COLORINDEX_M3:
		return Ptd_editcolor[49];
	case COLORINDEX_M4:
		return Ptd_editcolor[50];
	case COLORINDEX_M5:
		return Ptd_editcolor[51];
	case COLORINDEX_M6:
		return Ptd_editcolor[52];
	case COLORINDEX_M7:
		return Ptd_editcolor[53];
	case COLORINDEX_M8:
		return Ptd_editcolor[54];
	case COLORINDEX_M9:
		return Ptd_editcolor[55];
	case COLORINDEX_MA:
		return Ptd_editcolor[56];
	case COLORINDEX_N:
		return Ptd_editcolor[57];
	case COLORINDEX_O:
		return Ptd_editcolor[58];
	case COLORINDEX_P:
		return Ptd_editcolor[59];
	case COLORINDEX_Q:
		return Ptd_editcolor[60];
	case COLORINDEX_R:
		return Ptd_editcolor[61];
	case COLORINDEX_S:
		return Ptd_editcolor[62];
	case COLORINDEX_T:
		return Ptd_editcolor[63];
	case COLORINDEX_U1:
		return Ptd_editcolor[64];
	case COLORINDEX_U2:
		return Ptd_editcolor[65];
	case COLORINDEX_V1:
		return Ptd_editcolor[66];
	case COLORINDEX_V2:
		return Ptd_editcolor[67];
	case COLORINDEX_W1:
		return Ptd_editcolor[68];
	case COLORINDEX_W2:
		return Ptd_editcolor[69];
	case COLORINDEX_X1:
		return Ptd_editcolor[70];
	case COLORINDEX_X2:
		return Ptd_editcolor[71];
	case COLORINDEX_Y1:
		return Ptd_editcolor[72];
	case COLORINDEX_Y2:
		return Ptd_editcolor[73];
	case COLORINDEX_Z1:
		return Ptd_editcolor[74];
	case COLORINDEX_Z2:
		return Ptd_editcolor[75];
	case COLORINDEX_AA:
		return Ptd_editcolor[76];
	case COLORINDEX_AB:
		return Ptd_editcolor[77];
	case COLORINDEX_AC:
		return Ptd_editcolor[78];
	case COLORINDEX_AD:
		return Ptd_editcolor[79];
	case COLORINDEX_AE:
		return Ptd_editcolor[80];
	case COLORINDEX_AF:
		return Ptd_editcolor[81];
	case COLORINDEX_AG:
		return Ptd_editcolor[82];
	case COLORINDEX_AH:
		return Ptd_editcolor[83];
	case COLORINDEX_AI:
		return Ptd_editcolor[84];
	case COLORINDEX_AJ:
		return Ptd_editcolor[85];
	case COLORINDEX_AK:
		return Ptd_editcolor[86];
	case COLORINDEX_AL:
		return Ptd_editcolor[87];
	case COLORINDEX_AM:
		return Ptd_editcolor[88];
	case COLORINDEX_AN:
		return Ptd_editcolor[89];
	case COLORINDEX_AO:
		return Ptd_editcolor[90];
	case COLORINDEX_AP:
		return Ptd_editcolor[91];
	case COLORINDEX_AQ:
		return Ptd_editcolor[92];
	case COLORINDEX_AR:
		return Ptd_editcolor[93];
	case COLORINDEX_AS:
		return Ptd_editcolor[94];
	case COLORINDEX_AT:
		return Ptd_editcolor[95];
	case COLORINDEX_AU:
		return Ptd_editcolor[96];
	case COLORINDEX_AV:
		return Ptd_editcolor[97];
	case COLORINDEX_AW:
		return Ptd_editcolor[98];
	case COLORINDEX_AX:
		return Ptd_editcolor[99];
	case COLORINDEX_AY:
		return Ptd_editcolor[100];
	case COLORINDEX_AZ:
		return Ptd_editcolor[101];
	}
/*
.....unknown
*/
	return RGB(0, 0, 0);
}


/***********************************************************************
**
**   FUNCTION: GetLineFlags(int nLineIndex)
**
**       Get Line Flags
**
**   INPUT:  nLineIndex: line index to get the flag
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
DWORD CPtedTextView::GetLineFlags(int nLineIndex)
{
	if (m_pTextBuffer == NULL)
		return 0;
	return m_pTextBuffer->GetLineFlags(nLineIndex);
}

/***********************************************************************
**
**   FUNCTION: DrawMargin(CDC *pdc, const CRect &rect, int nLineIndex)
**
**       draw the margin
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CPtedTextView::DrawMargin(CDC *pdc, const CRect &rect, int nLineIndex)
{
	if (! m_bSelMargin)
	{
		pdc->FillSolidRect(rect, GetColor(COLORINDEX_BKGND));
		return;
	}

	pdc->FillSolidRect(rect, GetColor(COLORINDEX_SELMARGIN));

	int nImageIndex = -1;
	if (nLineIndex >= 0)
	{
		DWORD dwLineFlags = GetLineFlags(nLineIndex);
		static const DWORD adwFlags[] =
		{
			PTED_BOOKMARK(1),
			PTED_BOOKMARK(2),
			PTED_BOOKMARK(3),
			PTED_BOOKMARK(4),
			PTED_BOOKMARK(5),
			PTED_BOOKMARK(6),
			PTED_BOOKMARK(7),
			PTED_BOOKMARK(8),
			PTED_BOOKMARK(9),
			PTED_BOOKMARK(0),
			PTED_BOOKMARKS,
			PTED_FINDMARKS
		};
		for (int I = 0; I < sizeof(adwFlags) / sizeof(adwFlags[0]); I ++)
		{
			if ((dwLineFlags & adwFlags[I]) != 0)
			{
				nImageIndex = I;
				break;
			}
		}
	}

	if (nImageIndex >= 0)
	{
		if (m_pIcons == NULL)
		{ 
			m_pIcons = new CImageList;
			VERIFY(m_pIcons->Create(IDR_MARGIN_ICONS, 16, 16, RGB(255, 255, 255)));
		}
		CPoint pt(rect.left + 2, rect.top + (rect.Height() - 16) / 2);
		VERIFY(m_pIcons->Draw(pdc, nImageIndex, pt, ILD_TRANSPARENT));
	}
}

/***********************************************************************
**
**   FUNCTION: IsInsideSelBlock(const CPoint &ptTextPos)
**
**       check if position is inside a selection
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    true/false
**
**********************************************************************/
BOOL CPtedTextView::IsInsideSelBlock(CPoint ptTextPos)
{
	ASSERT_VALIDTEXTPOS(ptTextPos);
	if (ptTextPos.y < m_ptDrawSelStart.y)
		return FALSE;
	if (ptTextPos.y > m_ptDrawSelEnd.y)
		return FALSE;
	if (ptTextPos.y < m_ptDrawSelEnd.y && ptTextPos.y > m_ptDrawSelStart.y)
		return TRUE;
	if (m_ptDrawSelStart.y < m_ptDrawSelEnd.y)
	{
		if (ptTextPos.y == m_ptDrawSelEnd.y)
			return ptTextPos.x < m_ptDrawSelEnd.x;
		ASSERT(ptTextPos.y == m_ptDrawSelStart.y);
		return ptTextPos.x >= m_ptDrawSelStart.x;
	}
	ASSERT(m_ptDrawSelStart.y == m_ptDrawSelEnd.y);
	return ptTextPos.x >= m_ptDrawSelStart.x && ptTextPos.x < m_ptDrawSelEnd.x;
}

/***********************************************************************
**
**   FUNCTION: IsInsideSelection(const CPoint &ptTextPos)
**
**       check if position is inside a selection
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    true/false
**
**********************************************************************/
BOOL CPtedTextView::IsInsideSelection(const CPoint &ptTextPos)
{
	PrepareSelBounds();
	return IsInsideSelBlock(ptTextPos);
}

/***********************************************************************
**
**   FUNCTION: PrepareSelBounds()
**
**       prepare the selection for draw
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CPtedTextView::PrepareSelBounds()
{
	if (m_ptSelStart.y < m_ptSelEnd.y ||
			(m_ptSelStart.y == m_ptSelEnd.y && m_ptSelStart.x < m_ptSelEnd.x))
	{
		m_ptDrawSelStart = m_ptSelStart;
		m_ptDrawSelEnd = m_ptSelEnd;
	}
	else
	{
		m_ptDrawSelStart = m_ptSelEnd;
		m_ptDrawSelEnd = m_ptSelStart;
	}
}

/***********************************************************************
**
**   FUNCTION: OnDraw(CDC* pDC)
**
**       The framework calls this function to perform 
**		 screen display, and it passes a different device 
**		 context in each case. There is no default implementation.
**
**   INPUT:  CDC* pDC: device context
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CPtedTextView::OnDraw(CDC *pdc)
{
	CRect rcClient;
	GetClientRect(rcClient);

	int nLineCount = GetLineCount();
	int nLineHeight = GetLineHeight();
	PrepareSelBounds();
	CDC cacheDC;
	VERIFY(cacheDC.CreateCompatibleDC(pdc));
	if (m_pCacheBitmap == NULL)
	{
		m_pCacheBitmap = new CBitmap;
		VERIFY(m_pCacheBitmap->CreateCompatibleBitmap(pdc, rcClient.Width(), nLineHeight));
	}

	CBitmap *pOldBitmap = cacheDC.SelectObject(m_pCacheBitmap);

	CRect rcLine;
	rcLine = rcClient;
	rcLine.bottom = rcLine.top + nLineHeight;
	CRect rcCacheMargin(0, 0, GetMarginWidth(), nLineHeight);
	CRect rcCacheLine(GetMarginWidth(), 0, rcLine.Width(), nLineHeight);

	int nCurrentLine = m_nTopLine;
	while (rcLine.top < rcClient.bottom)
	{
		if (nCurrentLine < nLineCount)
		{
			DrawMargin(&cacheDC, rcCacheMargin, nCurrentLine);
			DrawSingleLine(&cacheDC, rcCacheLine, nCurrentLine);
		}
		else
		{
			DrawMargin(&cacheDC, rcCacheMargin, -1);
			DrawSingleLine(&cacheDC, rcCacheLine, -1);
		}

		VERIFY(pdc->BitBlt(rcLine.left, rcLine.top, rcLine.Width(), rcLine.Height(), &cacheDC, 0, 0, SRCCOPY));

		nCurrentLine ++;
		rcLine.OffsetRect(0, nLineHeight);
	}

	cacheDC.SelectObject(pOldBitmap);
	cacheDC.DeleteDC();
}

/***********************************************************************
c
c   FUNCTION: ResetView
c
c			Reset View
c
c   INPUT:  none
c			
c   OUTPUT: none
c	RETURN: none		
c
c***********************************************************************
*/
void CPtedTextView::ResetView()
{
	m_bAutoIndent = TRUE;
	m_bOvrMode = FALSE;
	m_nTopLine = 0;
	m_nOffsetChar = 0;
	m_nLineHeight = -1;
	m_nCharWidth = -1;
	m_nTabSize = 4;
	m_nMaxLineLength = -1;
	m_nScreenLines = -1;
	m_nScreenChars = -1;
	m_nIdealCharPos = -1;
	m_ptAnchor.x = 0;
	m_ptAnchor.y = 0;
	if (m_pIcons != NULL)
	{
		delete m_pIcons;
		m_pIcons = NULL;
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
	if (m_pdwParseCookies != NULL)
	{
		delete m_pdwParseCookies;
		m_pdwParseCookies = NULL;
	}
	if (m_pnActualLineLength != NULL)
	{
		delete m_pnActualLineLength;
		m_pnActualLineLength = NULL;
	}
	m_nParseArraySize = 0;
	m_nActualLengthArraySize = 0;
	m_ptCursorPos.x = 0;
	m_ptCursorPos.y = 0;
	m_ptSelStart = m_ptSelEnd = m_ptCursorPos;
	m_bDragSelection = FALSE;
	m_bVertScrollBarLocked = FALSE;
	m_bHorzScrollBarLocked = FALSE;
	if (::IsWindow(m_hWnd))
		UpdateCaret();
	m_bShowInactiveSelection = FALSE;
	m_bPrintHeader = FALSE;
	m_bPrintFooter = FALSE;
	strcpy(m_bPrintHeaderText, "&f");;
	strcpy(m_bPrintFooterText, "&p");;
	m_bBookmarkExist  = FALSE;
	m_bFindmarkExist  = FALSE;
	FlushUndoGroup();
}

/***********************************************************************
c
c   FUNCTION: UpdateCaret
c
c			Update Caret
c
c   INPUT:  none
c			
c   OUTPUT: none
c	RETURN: none		
c
c***********************************************************************
*/
void CPtedTextView::UpdateCaret()
{
	ASSERT_VALIDTEXTPOS(m_ptCursorPos);
	if (m_bFocused && ! m_bCursorHidden &&
		CalculateActualOffset(m_ptCursorPos.y, m_ptCursorPos.x) >= m_nOffsetChar)
	{
		CreateSolidCaret(2, GetLineHeight());
		SetCaretPos(TextToClient(m_ptCursorPos));
		ShowCaret();
	}
	else
	{
		HideCaret();
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
void CPtedTextView::BeginUndoGroup(BOOL bMergeWithPrevious)
{
	if (m_pTextBuffer)
		m_pTextBuffer->BeginUndoGroup(bMergeWithPrevious);
}
/***********************************************************************
c
c   FUNCTION: FlushUndoGroup()
c
c			Flush the UndoGroup
c
c   INPUT:  none
c			
c   OUTPUT: none
c		
c
c***********************************************************************
*/
void CPtedTextView::FlushUndoGroup()
{
	if (m_pTextBuffer)
		m_pTextBuffer->FlushUndoGroup(this);
}

/*
DROPEFFECT CEditDropTargetImpl::OnDragEnter(CWnd* pWnd, COleDataObject* pDataObject, DWORD dwKeyState, CPoint point)
{
	if (! pDataObject->IsDataAvailable(CF_TEXT))
	{
		m_pOwner->HideDropIndicator();
		return DROPEFFECT_NONE;
	}
	m_pOwner->ShowDropIndicator(point);
	if (dwKeyState & MK_CONTROL)
		return DROPEFFECT_COPY;
	return DROPEFFECT_MOVE;
}

void CEditDropTargetImpl::OnDragLeave(CWnd* pWnd)
{
	m_pOwner->HideDropIndicator();
}

DROPEFFECT CEditDropTargetImpl::OnDragOver(CWnd* pWnd, COleDataObject* pDataObject, DWORD dwKeyState, CPoint point)
{
	bool bDataSupported = false;

	if ((!m_pOwner) ||							
			(!( m_pOwner->QueryEditable())) ||	
			(m_pOwner->GetDisableDragAndDrop()))
	{
		m_pOwner -> HideDropIndicator();			
		return DROPEFFECT_NONE;	
	}
	if (pDataObject->IsDataAvailable(CF_TEXT))
	{
		bDataSupported = true;
	}
	if (!bDataSupported)
	{
		m_pOwner->HideDropIndicator();
		return DROPEFFECT_NONE;
	}
	m_pOwner->ShowDropIndicator(point);
	if (dwKeyState & MK_CONTROL)
		return DROPEFFECT_COPY;
	return DROPEFFECT_MOVE;
}

BOOL CEditDropTargetImpl::OnDrop(CWnd* pWnd, COleDataObject* pDataObject, DROPEFFECT dropEffect, CPoint point)
{
	bool bDataSupported = false;

	m_pOwner->HideDropIndicator();
	if ((!m_pOwner) ||								
			(!( m_pOwner->QueryEditable())) ||
			(m_pOwner->GetDisableDragAndDrop()))
	{
		return DROPEFFECT_NONE;				
	}
	if (pDataObject->IsDataAvailable(CF_TEXT))			    
	{
		bDataSupported = true;								
	}
	if (!bDataSupported)									
	{
		return DROPEFFECT_NONE;							    
	}
	return (m_pOwner->DoDropText(pDataObject, point));	    
}

DROPEFFECT CEditDropTargetImpl::OnDragScroll(CWnd* pWnd, DWORD dwKeyState, CPoint point)
{
	ASSERT(m_pOwner == pWnd);
	m_pOwner->DoDragScroll(point);

	if (dwKeyState & MK_CONTROL)
		return DROPEFFECT_COPY;
	return DROPEFFECT_MOVE;
}

void CPtedTextView::DoDragScroll(const CPoint &point)
{
	CRect rcClientRect;
	GetClientRect(rcClientRect);
	if (point.y < rcClientRect.top + DRAG_BORDER_Y)
	{
		HideDropIndicator();
		ScrollUp();
		UpdateWindow();
		ShowDropIndicator(point);
		return;
	}
	if (point.y >= rcClientRect.bottom - DRAG_BORDER_Y)
	{
		HideDropIndicator();
		ScrollDown();
		UpdateWindow();
		ShowDropIndicator(point);
		return;
	}
	if (point.x < rcClientRect.left + GetMarginWidth() + DRAG_BORDER_X)
	{
		HideDropIndicator();
		ScrollLeft();
		UpdateWindow();
		ShowDropIndicator(point);
		return;
	}
	if (point.x >= rcClientRect.right - DRAG_BORDER_X)
	{
		HideDropIndicator();
		ScrollRight();
		UpdateWindow();
		ShowDropIndicator(point);
		return;
	}
}

BOOL CPtedTextView::DoDropText(COleDataObject *pDataObject, const CPoint &ptClient)
{
	HGLOBAL hData = pDataObject->GetGlobalData(CF_TEXT);
	if (hData == NULL)
		return FALSE;

	CPoint ptDropPos = ClientToText(ptClient);
	if (IsDraggingText() && IsInsideSelection(ptDropPos))
	{
		SetAnchor(ptDropPos);
		SetSelection(ptDropPos, ptDropPos);
		SetCursorPos(ptDropPos);
		EnsureVisible(ptDropPos);
		return FALSE;
	}

	LPSTR pszText = (LPSTR) ::GlobalLock(hData);
	if (pszText == NULL)
		return FALSE;

	int x, y;
	USES_CONVERSION;
	m_pTextBuffer->InsertText(ptDropPos.y, ptDropPos.x, A2T(pszText), y, x, PTED_DRAGDROP);
	CPoint ptCurPos(x, y);
	ASSERT_VALIDTEXTPOS(ptCurPos);
	SetAnchor(ptDropPos);
	SetSelection(ptDropPos, ptCurPos);
	SetCursorPos(ptCurPos);
	EnsureVisible(ptCurPos);

	::GlobalUnlock(hData);
	return TRUE;
}


void CPtedTextView::ShowDropIndicator(const CPoint &point)
{
	if (! m_bDropPosVisible)
	{
		HideCursor();
		m_ptSavedCaretPos = GetCursorPos();
		m_bDropPosVisible = TRUE;
		::CreateCaret(m_hWnd, (HBITMAP) 1, 2, GetLineHeight());
	}
	m_ptDropPos = ClientToText(point);
	if (m_ptDropPos.x >= m_nOffsetChar)
	{
		SetCaretPos(TextToClient(m_ptDropPos));
		ShowCaret();
	}
	else
	{
		HideCaret();
	}
}

void CPtedTextView::HideDropIndicator()
{
	if (m_bDropPosVisible)
	{
		SetCursorPos(m_ptSavedCaretPos);
		ShowCursor();
		m_bDropPosVisible = FALSE;
	}
}

DROPEFFECT CPtedTextView::GetDropEffect()
{
	return DROPEFFECT_COPY | DROPEFFECT_MOVE;
}

void CPtedTextView::OnDropSource(DROPEFFECT de)
{
	if (! IsDraggingText())
		return;

	ASSERT_VALIDTEXTPOS(m_ptDraggedTextBegin);
	ASSERT_VALIDTEXTPOS(m_ptDraggedTextEnd);

	if (de == DROPEFFECT_MOVE)
	{
		m_pTextBuffer->DeleteText(m_ptDraggedTextBegin.y, m_ptDraggedTextBegin.x, m_ptDraggedTextEnd.y, 
			m_ptDraggedTextEnd.x, PTED_DRAGDROP);
	}
}
*/
/***********************************************************************
c
c   SUBROUTINE:  Disp_Msg(char *msg, int flag==1)
c
c   FUNCTION:  This function is display a messge
c
c   INPUT:  msg: message string to display
c			flag: 1: error messge
c				  2: warning message
c				  3. info message
c
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedTextView::Disp_Msg(char *msg, int flag)
{
	((CPtedWindow*)m_parent)->Disp_Msg(msg, flag);
}
void CPtedTextView::OnCloseReset()
{
	if (m_pDropTarget != NULL)
	{
		m_pDropTarget->Revoke();
		delete m_pDropTarget;
		m_pDropTarget = NULL;
	}
	DetachFromBuffer();
	ResetView();
	m_bAutoIndent = TRUE;
	m_pTextBuffer = new CPtedTextBuffer();
}
	
/***********************************************************************
c
c   FUNCTION: SetModifiedFlag(BOOL bModified)
c
c			This function set the text view as modified.
c
c   INPUT:  bModified: modify flag to be set
c			
c   OUTPUT: none
c		
c
c***********************************************************************
*/
void CPtedTextView::SetModifiedFlag(BOOL bModified /*= TRUE*/)
{
	((CPtedWindow*)m_parent)->SetModifiedFlag(bModified);
}
