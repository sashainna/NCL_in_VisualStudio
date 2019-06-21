
/************************************************************************
c
c   FILE NAME: PtedTextView2.cpp
c
c   CONTAINS:
c		CPtedTextView::GetTabSize()
c		CPtedTextView::SetTabSize(int nTabSize)
c		CPtedTextView::GetFont(BOOL bItalic, BOOL bBold)
c		CPtedTextView::CalcLineCharDim()
c		CPtedTextView::GetLineHeight()
c		CPtedTextView::GetCharWidth()
c		CPtedTextView::GetMaxLineLength()
c		CPtedTextView::OnInitialUpdate()
c		CPtedTextView::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo) 
c		CPtedTextView::OnPreparePrinting(CPrintInfo* pInfo)
c		CPtedTextView::PrintLineHeight(CDC *pdc, int nLine)
c		CPtedTextView::GetPrintHeaderText(int nPageNum, CString &text)
c		CPtedTextView::GetPrintFooterText(int nPageNum, CString &text)
c		CPtedTextView::PrintHeader(CDC *pdc, int nPageNum)
c		CPtedTextView::PrintFooter(CDC *pdc, int nPageNum)
c		CPtedTextView::RecalcPageLayouts(CDC *pdc, CPrintInfo *pInfo)
c		CPtedTextView::OnBeginPrinting(CDC *pdc, CPrintInfo *pInfo)
c		CPtedTextView::OnEndPrinting(CDC *pdc, CPrintInfo *pInfo)
c		CPtedTextView::OnPrint(CDC* pdc, CPrintInfo* pInfo) 
c		CPtedTextView::GetLineCount() 
c		CPtedTextView::GetLineLength(int nLineIndex) 
c		CPtedTextView::GetLineChars(int nLineIndex)
c		CPtedTextView::AttachToBuffer(CPtedTextBuffer *pBuf)
c		CPtedTextView::DetachFromBuffer()
c		CPtedTextView::GetScreenLines()
c		CPtedTextView::GetItalic(int nColorIndex)
c		CPtedTextView::GetBold(int nColorIndex)
c		CPtedTextView::GetScreenChars()
c		CPtedTextView::OnEraseBkgnd(CDC *pdc) 
c		CPtedTextView::OnSize(UINT nType, int cx, int cy) 
c		CPtedTextView::RecalcVertScrollBar(BOOL bPositionOnly)
c		CPtedTextView::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
c		CPtedTextView::RecalcHorzScrollBar(BOOL bPositionOnly )
c		CPtedTextView::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
c		OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message) 
c		CPtedTextView::ClientToText(const CPoint &point)
c		CPtedTextView::TextToClient(const CPoint &point)
c		CPtedTextView::InvalidateLines(int nLine1, int nLine2, BOOL bInvalidateMargin)
c		CPtedTextView::SetSelection(const CPoint &ptStart, const CPoint &ptEnd)
c		CPtedTextView::AdjustTextPoint(CPoint &point)
c		CPtedTextView::OnSetFocus(CWnd* pOldWnd) 
c		CPtedTextView::CalculateActualOffset(int nLineIndex, int nCharIndex)
c		CPtedTextView::ApproxActualOffset(int nLineIndex, int nOffset)
c		CPtedTextView::EnsureVisible(CPoint pt)
c		CPtedTextView::SetBookMarkWindow(int flag)
c		CPtedTextView::OnKillFocus(CWnd* pNewWnd) 
c		CPtedTextView::OnSysColorChange() 
c		CPtedTextView::GetText(const CPoint &ptStart, const CPoint &ptEnd, CString &text)
c		CPtedTextView::GetTextLength(int nStartLine, int nStartChar, int nEndLine, int nEndChar)
c		CPtedTextView::GetResourceHandle()
c		CPtedTextView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
c		CPtedTextView::SetAnchor(const CPoint &ptNewAnchor)
c		CPtedTextView::PreTranslateMessage(MSG *pMsg) 
c		CPtedTextView::HighlightText(const CPoint &ptStartPos, int nLength)
//c		BOOL CPtedTextView::GetDisableDragAndDrop() const
//c		CPtedTextView::SetDisableDragAndDrop(BOOL bDDAD)
c		CPtedTextView::QueryEditable()
c		CPtedTextView::DeleteCurrentSelection()
c		CPtedTextView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags) 
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c	MODULE NAME AND RELEASE LEVEL
c       PtedTextView3.cpp , 24.2
c	DATE AND TIME OF LAST  MODIFICATION
c       10/29/13 , 16:05:36
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
#include "PtedSetupPageDialog.h"

#include <afxpriv.h>
#include <afxole.h>
#define DEFAULT_PRINT_MARGIN 1000

extern int FontSize;


/***********************************************************************
c
c   SUBROUTINE: GetTabSize()
c
c   FUNCTION:  This functions return tab size
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETURN: tab size
c
c***********************************************************************
*/
int CPtedTextView::GetTabSize()
{
	ASSERT(m_nTabSize >= 0 && m_nTabSize <= 64);
	return m_nTabSize;
}

/***********************************************************************
c
c   SUBROUTINE: SetTabSize(int nTabSize)
c
c   FUNCTION:  This functions set tab size
c
c   INPUT:  nTabSize: tab size
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::SetTabSize(int nTabSize)
{
	ASSERT(nTabSize >= 0 && nTabSize <= 64);
	if (m_nTabSize != nTabSize)
	{
		m_nTabSize = nTabSize;
		if (m_pnActualLineLength != NULL)
		{
			delete m_pnActualLineLength;
			m_pnActualLineLength = NULL;
		}
		m_nActualLengthArraySize = 0;
		m_nMaxLineLength = -1;
		RecalcHorzScrollBar();
		Invalidate();
		UpdateCaret();
	}
}

/***********************************************************************
c
c   SUBROUTINE: GetFont((BOOL bItalic, BOOL bBold)
c
c   FUNCTION:  This functions Get the font
c
c   INPUT:  bItalic: return the bItalic font
c			bBold: return the bBold font
c
c   OUTPUT: none
c	RETURN: view font
c
c***********************************************************************
*/
CFont *CPtedTextView::GetFont(BOOL bItalic /*= FALSE*/, BOOL bBold /*= FALSE*/)
{
	int nIndex = 0;
	if (bBold)
		nIndex |= 1;
	if (bItalic)
		nIndex |= 2;

	if (m_apFonts[nIndex] == NULL)
	{
		CDC *pdc = GetDC();
		m_apFonts[nIndex] = new CFont;
		m_lfBaseFont.lfPitchAndFamily = FIXED_PITCH | FF_MODERN;
		m_lfBaseFont.lfHeight = -MulDiv(FontSize, pdc->GetDeviceCaps(LOGPIXELSY),
			72);
		strcpy(m_lfBaseFont.lfFaceName,"Courier New");
		m_lfBaseFont.lfWeight = bBold ? FW_BOLD : FW_NORMAL;
		m_lfBaseFont.lfItalic = (BYTE) bItalic;
		if (! m_apFonts[nIndex]->CreateFontIndirect(&m_lfBaseFont))
		{
			delete m_apFonts[nIndex];
			m_apFonts[nIndex] = NULL;
			return CView::GetFont();
		}

	}
	return m_apFonts[nIndex];
}

/***********************************************************************
c
c   SUBROUTINE: CalcLineCharDim
c
c   FUNCTION:  This functions calculate the line height and char width
c				according to current font
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::CalcLineCharDim()
{
	CDC *pdc = GetDC();
	CFont *pOldFont = pdc->SelectObject(GetFont());
	CSize szCharExt = pdc->GetTextExtent(_T("X"));
	m_nLineHeight = szCharExt.cy;
	if (m_nLineHeight < 1)
		m_nLineHeight = 1;
	m_nCharWidth = szCharExt.cx;
	pdc->SelectObject(pOldFont);
	ReleaseDC(pdc);
}

/***********************************************************************
c
c   SUBROUTINE: GetLineHeight
c
c   FUNCTION:  return line height
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: line height
c
c***********************************************************************
*/
int CPtedTextView::GetLineHeight()
{
	if (m_nLineHeight == -1)
		CalcLineCharDim();
	return m_nLineHeight;
}

/***********************************************************************
c
c   SUBROUTINE: GetCharWidth
c
c   FUNCTION:  return character width
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: character width
c
c***********************************************************************
*/
int CPtedTextView::GetCharWidth()
{
	if (m_nCharWidth == -1)
		CalcLineCharDim();
	return m_nCharWidth;
}

/***********************************************************************
c
c   SUBROUTINE: GetMaxLineLength
c
c   FUNCTION:  Get Maxinum Line Length
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: Maxinum Line Length
c
c***********************************************************************
*/
int CPtedTextView::GetMaxLineLength()
{
	if (m_nMaxLineLength == -1)
	{
		m_nMaxLineLength = 0;
		int nLineCount = GetLineCount();
		for (int I = 0; I < nLineCount; I ++)
		{
			int nActualLength = GetLineActualLength(I);
			if (m_nMaxLineLength < nActualLength)
				m_nMaxLineLength = nActualLength;
		}
	}
	return m_nMaxLineLength;
}
/***********************************************************************
c
c   SUBROUTINE: OnInitialUpdate
c
c   FUNCTION:  Initial Update the view
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::OnInitialUpdate()
{
	CView::OnInitialUpdate();

	AttachToBuffer(NULL);
}


/////////////////////////////////////////////////////////////////////////////
// CPtedTextView printing

/***********************************************************************
c
c   SUBROUTINE: OnPrepareDC(CDC* pDC, CPrintInfo* pInfo)
c
c   FUNCTION:  the function examines the page information stored in the pInfo parameter. 
c				The function stops the print loop by setting the m_bContinuePrinting 
c				member of the structure to FALSE.
c
c   INPUT:  pdc   Points to the printer device context.
c			pInfo   Points to a CPrintInfo structure that describes the current print job
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo) 
{
	CView::OnPrepareDC(pDC, pInfo);

	if (pInfo != NULL)
	{
		pInfo->m_bContinuePrinting = TRUE;
		if (m_pnPages != NULL && (int) pInfo->m_nCurPage > m_nPrintPages)
			pInfo->m_bContinuePrinting = FALSE;
	}
}

/***********************************************************************
c
c   SUBROUTINE: OnPreparePrinting(CPrintInfo* pInfo)
c
c   FUNCTION:  We must override this function to enable printing and print preview
c
c   INPUT:  pInfo   Points to a CPrintInfo structure that describes the current print job.
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
BOOL CPtedTextView::OnPreparePrinting(CPrintInfo* pInfo)
{
	return DoPreparePrinting(pInfo);
}

/***********************************************************************
c
c   SUBROUTINE: PrintLineHeight(CDC *pdc, int nLine)
c
c   FUNCTION:  Get the print line's height
c
c   INPUT:  pdc   Points to the printer device context.
c			nLine: the line index
c
c   OUTPUT: none
c	RETURN: print line's height
c
c***********************************************************************
*/
int CPtedTextView::PrintLineHeight(CDC *pdc, int nLine)
{
	ASSERT(nLine >= 0 && nLine < GetLineCount());
	ASSERT(m_nPrintLineHeight > 0);
	int nLength = GetLineLength(nLine);
	if (nLength == 0)
		return m_nPrintLineHeight;

	CString line;
	LPCTSTR pszChars = GetLineChars(nLine);
	ExpandChars(pszChars, 0, nLength, line);
	CRect rcPrintArea = m_rcPrintArea;
	pdc->DrawText(line, &rcPrintArea, DT_LEFT | DT_NOPREFIX | DT_TOP | DT_WORDBREAK | DT_CALCRECT);
	return rcPrintArea.Height();
}

/***********************************************************************
c
c   SUBROUTINE: GetPrintHeaderText(int nPageNum, CString &text)
c
c   FUNCTION:  Get the print header text
c
c   INPUT:  nPageNum: index of current print page
c
c   OUTPUT: text:  print header text
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::GetPrintHeaderText(int nPageNum, CString &text, UINT &flag)
{
	ASSERT(m_bPrintHeader);

	int i, j;
	char tempstr[256], tempstr2[512];
	int file = 0, page = 0, total = 0;
	int len = strlen(m_bPrintHeaderText);
	flag = DT_CENTER;
	if (len==0)
	{
		text = "";
		return;
	}
	for (i=0, j=0; i<len; i++, j++)
	{
		if (m_bPrintHeaderText[i]!='&')
			tempstr[j] = m_bPrintHeaderText[i];
		else
		{
			if ((m_bPrintHeaderText[i+1] == 'C') ||
					(m_bPrintHeaderText[i+1] == 'c'))
			{
				flag = DT_CENTER;
				i = i + 1;
				j = j - 1;
			}
			else if ((m_bPrintHeaderText[i+1] == 'L') ||
					(m_bPrintHeaderText[i+1] == 'l'))
			{
				flag = DT_LEFT;
				i = i + 1;
				j = j - 1;
			}
			else if ((m_bPrintHeaderText[i+1] == 'R') ||
					(m_bPrintHeaderText[i+1] == 'r'))
			{
				flag = DT_RIGHT;
				i = i + 1;
				j = j - 1;
			}
			else if ((m_bPrintHeaderText[i+1] == 'F') ||
					(m_bPrintHeaderText[i+1] == 'f'))
			{
				if (file==0)
				{
					if (page!=0)
						file++;
					if (total!=0)
						file++;
					file++;
					tempstr[j] = '%';
					tempstr[j+1] = 's';
				}
				i = i + 1;
				j = j+1;
			}
			else if ((m_bPrintHeaderText[i+1] == 'P') ||
					(m_bPrintHeaderText[i+1] == 'p'))
			{
				if (page==0)
				{
					if (file!=0)
						page++;
					if (total!=0)
						page++;
					page++;
					tempstr[j] = '%';
					tempstr[j+1] = 'd';
				}
				i = i + 1;
				j = j+1;
			}
			else if ((m_bPrintHeaderText[i+1] == 'T') ||
					(m_bPrintHeaderText[i+1] == 't'))
			{
				if (total==0)
				{
					if (file!=0)
						total++;
					if (page!=0)
						total++;
					total++;
					tempstr[j] = '%';
					tempstr[j+1] = 'd';
				}
				i = i + 1;
				j = j+1;
			}
		}
	}
	tempstr[j] = '\0';
/*
	if (stricmp(m_bPrintHeaderText, "&f")==0)
		text = ((CPtedWindow*)m_parent)->m_file;
	else if (stricmp(m_bPrintHeaderText, "&p")==0)
		text.Format(_T("Page %d of %d"), nPageNum, m_nPrintPages);
	else
		text = m_bPrintHeaderText;
*/
	if (page==1)
	{
		if (file==0)
		{
			if (total==0)
				sprintf (tempstr2, tempstr, nPageNum);
			else
				sprintf (tempstr2, tempstr, nPageNum, m_nPrintPages);
		}
		else
		{
			if ((file==2) && (total==0))
			{
				sprintf (tempstr2, tempstr, nPageNum, ((CPtedWindow*)m_parent)->m_file);
			}
			else if ((file==2) && (total==3))
			{
				sprintf (tempstr2, tempstr, nPageNum, 
					((CPtedWindow*)m_parent)->m_file, m_nPrintPages);
			}
			else if ((file==3) && (total==2))
			{
				sprintf (tempstr2, tempstr, nPageNum, m_nPrintPages, ((CPtedWindow*)m_parent)->m_file);
			}
		}
	}
	else if (page==0)
	{
		if ((file==1) && (total==0))
			sprintf (tempstr2, tempstr, ((CPtedWindow*)m_parent)->m_file);
		else if ((file==1) && (total==2))
			sprintf (tempstr2, tempstr, ((CPtedWindow*)m_parent)->m_file, m_nPrintPages);
		else if ((file==2) && (total==1))
			sprintf (tempstr2, tempstr,  m_nPrintPages, ((CPtedWindow*)m_parent)->m_file);
		else if ((file==0) && (total==1))
			sprintf (tempstr2, tempstr,  m_nPrintPages);
	}
	else if (page==2)
	{
		if (file==0)
		{
			sprintf (tempstr2, tempstr, m_nPrintPages, nPageNum);
		}
		else if ((file==1) && (total==0))
			sprintf (tempstr2, tempstr, ((CPtedWindow*)m_parent)->m_file, 
						nPageNum);
		else if ((file==1) && (total==3))
			sprintf (tempstr2, tempstr, ((CPtedWindow*)m_parent)->m_file, 
						nPageNum, m_nPrintPages);
		else if ((file==3) && (total==1))
			sprintf (tempstr2, tempstr, m_nPrintPages, nPageNum, 
						((CPtedWindow*)m_parent)->m_file, nPageNum);
	}
	else if (page==3)
	{
		if ((file==1) && (total==2))
			sprintf (tempstr2, tempstr, ((CPtedWindow*)m_parent)->m_file, 
						m_nPrintPages, nPageNum);
		else if ((file==2) && (total==1))
			sprintf (tempstr2, tempstr, m_nPrintPages, 
						((CPtedWindow*)m_parent)->m_file, nPageNum);
	}
	else
		strcpy(tempstr2, tempstr);
	text = tempstr2;
}

/***********************************************************************
c
c   SUBROUTINE: GetPrintFooterText(int nPageNum, CString &text)
c
c   FUNCTION:  Get the print footer text
c
c   INPUT:  nPageNum: index of current print page
c
c   OUTPUT: text:  print footer text
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::GetPrintFooterText(int nPageNum, CString &text, UINT &flag)
{
	ASSERT(m_bPrintFooter);

	int i, j;
	char tempstr[256], tempstr2[512];
	int file = 0, page = 0, total = 0;
	int len = strlen(m_bPrintFooterText);
	flag = DT_CENTER;
	if (len==0)
	{
		text = "";
		return;
	}
	for (i=0, j=0; i<len; i++, j++)
	{
		if (m_bPrintFooterText[i]!='&')
			tempstr[j] = m_bPrintFooterText[i];
		else
		{
			if ((m_bPrintFooterText[i+1] == 'C') ||
					(m_bPrintFooterText[i+1] == 'c'))
			{
				flag = DT_CENTER;
				i = i + 1;
				j = j - 1;
			}
			else if ((m_bPrintFooterText[i+1] == 'L') ||
					(m_bPrintFooterText[i+1] == 'l'))
			{
				flag = DT_LEFT;
				i = i + 1;
				j = j - 1;
			}
			else if ((m_bPrintFooterText[i+1] == 'R') ||
					(m_bPrintFooterText[i+1] == 'r'))
			{
				flag = DT_RIGHT;
				i = i + 1;
				j = j - 1;
			}
			else if ((m_bPrintFooterText[i+1] == 'F') ||
					(m_bPrintFooterText[i+1] == 'f'))
			{
				if (file==0)
				{
					if (page!=0)
						file++;
					if (total!=0)
						file++;
					file++;
					tempstr[j] = '%';
					tempstr[j+1] = 's';
				}
				i = i + 1;
				j = j+1;
			}
			else if ((m_bPrintFooterText[i+1] == 'P') ||
					(m_bPrintFooterText[i+1] == 'p'))
			{
				if (page==0)
				{
					if (file!=0)
						page++;
					if (total!=0)
						page++;
					page++;
					tempstr[j] = '%';
					tempstr[j+1] = 'd';
				}
				i = i + 1;
				j = j+1;
			}
			else if ((m_bPrintFooterText[i+1] == 'T') ||
					(m_bPrintFooterText[i+1] == 't'))
			{
				if (total==0)
				{
					if (file!=0)
						total++;
					if (page!=0)
						total++;
					total++;
					tempstr[j] = '%';
					tempstr[j+1] = 'd';
				}
				i = i + 1;
				j = j+1;
			}
		}
	}
	tempstr[j] = '\0';
/*
	if (stricmp(m_bPrintFooterText, "&f")==0)
		text = ((CPtedWindow*)m_parent)->m_file;
	else if (stricmp(m_bPrintFooterText, "&p")==0)
		text.Format(_T("Page %d of %d"), nPageNum, m_nPrintPages);
	else
		text = m_bPrintFooterText;
*/
	if (page==1)
	{
		if (file==0)
		{
			if (total==0)
				sprintf (tempstr2, tempstr, nPageNum);
			else
				sprintf (tempstr2, tempstr, nPageNum, m_nPrintPages);
		}
		else
		{
			if ((file==2) && (total==0))
			{
				sprintf (tempstr2, tempstr, nPageNum, ((CPtedWindow*)m_parent)->m_file);
			}
			else if ((file==2) && (total==3))
			{
				sprintf (tempstr2, tempstr, nPageNum, 
					((CPtedWindow*)m_parent)->m_file, m_nPrintPages);
			}
			else if ((file==3) && (total==2))
			{
				sprintf (tempstr2, tempstr, nPageNum, m_nPrintPages, ((CPtedWindow*)m_parent)->m_file);
			}
		}
	}
	else if (page==0)
	{
		if ((file==1) && (total==0))
			sprintf (tempstr2, tempstr, ((CPtedWindow*)m_parent)->m_file);
		else if ((file==1) && (total==2))
			sprintf (tempstr2, tempstr, ((CPtedWindow*)m_parent)->m_file, m_nPrintPages);
		else if ((file==2) && (total==1))
			sprintf (tempstr2, tempstr,  m_nPrintPages, ((CPtedWindow*)m_parent)->m_file);
		else if ((file==0) && (total==1))
			sprintf (tempstr2, tempstr,  m_nPrintPages);
	}
	else if (page==2)
	{
		if (file==0)
		{
			sprintf (tempstr2, tempstr, m_nPrintPages, nPageNum);
		}
		else if ((file==1) && (total==0))
			sprintf (tempstr2, tempstr, ((CPtedWindow*)m_parent)->m_file, 
						nPageNum);
		else if ((file==1) && (total==3))
			sprintf (tempstr2, tempstr, ((CPtedWindow*)m_parent)->m_file, 
						nPageNum, m_nPrintPages);
		else if ((file==3) && (total==1))
			sprintf (tempstr2, tempstr, m_nPrintPages, nPageNum, 
						((CPtedWindow*)m_parent)->m_file, nPageNum);
	}
	else if (page==3)
	{
		if ((file==1) && (total==2))
			sprintf (tempstr2, tempstr, ((CPtedWindow*)m_parent)->m_file, 
						m_nPrintPages, nPageNum);
		else if ((file==2) && (total==1))
			sprintf (tempstr2, tempstr, m_nPrintPages, 
						((CPtedWindow*)m_parent)->m_file, nPageNum);
	}
	else
		strcpy(tempstr2, tempstr);
	text = tempstr2;
}

/***********************************************************************
c
c   SUBROUTINE: PrintHeader(CDC *pdc, int nPageNum)
c
c   FUNCTION:  Print the page header
c
c   INPUT:  pdc   Points to the printer device context.
c			nPageNum: index of current printting page
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::PrintHeader(CDC *pdc, int nPageNum)
{
	CRect rcHeader = m_rcPrintArea;
	rcHeader.bottom = rcHeader.top;
	rcHeader.top -= (m_nPrintLineHeight + m_nPrintLineHeight / 2);

	CString text;
	UINT flag;
	GetPrintHeaderText(nPageNum, text, flag);
	if (! text.IsEmpty())
	{
		flag = flag | DT_NOPREFIX | DT_TOP | DT_SINGLELINE;
//		pdc->DrawText(text, &rcHeader, DT_CENTER | DT_NOPREFIX | DT_TOP | DT_SINGLELINE);
		pdc->DrawText(text, &rcHeader, flag);
	}
}

/***********************************************************************
c
c   SUBROUTINE: PrintFooter(CDC *pdc, int nPageNum)
c
c   FUNCTION:  Print the page Footer
c
c   INPUT:  pdc   Points to the printer device context.
c			nPageNum: index of current printting page
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::PrintFooter(CDC *pdc, int nPageNum)
{
	CRect rcFooter = m_rcPrintArea;
	rcFooter.top = rcFooter.bottom;
	rcFooter.bottom += (m_nPrintLineHeight + m_nPrintLineHeight / 2);

	CString text;
	UINT flag;
	GetPrintFooterText(nPageNum, text, flag);
	if (! text.IsEmpty())
	{
		flag = flag | DT_NOPREFIX | DT_BOTTOM | DT_SINGLELINE;
//		pdc->DrawText(text, &rcFooter, DT_CENTER | DT_NOPREFIX | DT_BOTTOM | DT_SINGLELINE);
		pdc->DrawText(text, &rcFooter, flag);
	}
}

/***********************************************************************
c
c   SUBROUTINE: RecalcPageLayouts(CDC *pdc, CPrintInfo *pInfo)
c
c   FUNCTION:  Calculate the page layout
c
c   INPUT:  pdc   Points to the printer device context.
c			pInfo   Points to a CPrintInfo structure that describes the current print job
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::RecalcPageLayouts(CDC *pdc, CPrintInfo *pInfo)
{
	m_ptPageArea = pInfo->m_rectDraw;
	m_ptPageArea.NormalizeRect();

	m_nPrintLineHeight = pdc->GetTextExtent(_T("X")).cy;

	m_rcPrintArea = m_ptPageArea;
	CSize szTopLeft, szBottomRight;
	CWinApp *pApp = AfxGetApp();
	ASSERT(pApp != NULL);
	szTopLeft.cx = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_LEFT, DEFAULT_PRINT_MARGIN);
	szBottomRight.cx = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_RIGHT, DEFAULT_PRINT_MARGIN);
	szTopLeft.cy = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_TOP, DEFAULT_PRINT_MARGIN);
	szBottomRight.cy = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_BOTTOM, DEFAULT_PRINT_MARGIN);
	pdc->HIMETRICtoLP(&szTopLeft);
	pdc->HIMETRICtoLP(&szBottomRight);
	m_rcPrintArea.left += szTopLeft.cx;
	m_rcPrintArea.right -= szBottomRight.cx;
	m_rcPrintArea.top += szTopLeft.cy;
	m_rcPrintArea.bottom -= szBottomRight.cy;
	if (m_bPrintHeader)
		m_rcPrintArea.top += m_nPrintLineHeight + m_nPrintLineHeight / 2;
	if (m_bPrintFooter)
		m_rcPrintArea.bottom -= (m_nPrintLineHeight + m_nPrintLineHeight / 2);

	int nLimit = 32;
	m_nPrintPages = 1;
	m_pnPages = new int[nLimit];
	m_pnPages[0] = 0;

	int nLineCount = GetLineCount();
	int nLine = 1;
/*	if (pInfo->m_pPD->m_pd.Flags == PD_SELECTION)
	{
		nLine = m_ptSelStart.y + 1;
		nLineCount = m_ptSelEnd.y + 1;
	}
*/
	int y = m_rcPrintArea.top + PrintLineHeight(pdc, 0);
	while (nLine < nLineCount)
	{
		int nHeight = PrintLineHeight(pdc, nLine);
		if (y + nHeight <= m_rcPrintArea.bottom)
		{
			y += nHeight;
		}
		else
		{
			ASSERT(nLimit >= m_nPrintPages);
			if (nLimit <= m_nPrintPages)
			{
				nLimit += 32;
				int *pnNewPages = new int[nLimit];
				memcpy(pnNewPages, m_pnPages, sizeof(int) * m_nPrintPages);
				delete m_pnPages;
				m_pnPages = pnNewPages;
			}
			ASSERT(nLimit > m_nPrintPages);
			m_pnPages[m_nPrintPages ++] = nLine;
			y = m_rcPrintArea.top + nHeight;
		}
		nLine ++;
	}
}

/***********************************************************************
c
c   SUBROUTINE: OnBeginPrinting
c
c   FUNCTION:  Function called before print
c
c   INPUT:  pdc   Points to the printer device context.
c			pInfo   Points to a CPrintInfo structure that describes the current print job
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::OnBeginPrinting(CDC *pdc, CPrintInfo *pInfo)
{
	ASSERT(m_pnPages == NULL);
	ASSERT(m_pPrintFont == NULL);
	CFont *pDisplayFont = GetFont();

	LOGFONT lf;
	pDisplayFont->GetLogFont(&lf);

	CDC *pDisplayDC = GetDC();
	lf.lfHeight = MulDiv(lf.lfHeight, pdc->GetDeviceCaps(LOGPIXELSY), pDisplayDC->GetDeviceCaps(LOGPIXELSY) * 2);
	lf.lfWidth = MulDiv(lf.lfWidth, pdc->GetDeviceCaps(LOGPIXELSX), pDisplayDC->GetDeviceCaps(LOGPIXELSX) * 2);
	ReleaseDC(pDisplayDC);

	m_pPrintFont = new CFont;
	if (! m_pPrintFont->CreateFontIndirect(&lf))
	{
		delete m_pPrintFont;
		m_pPrintFont = NULL;
		return;
	}

	pdc->SelectObject(m_pPrintFont);
}

/***********************************************************************
c
c   SUBROUTINE: OnEndPrinting(CDC *pdc, CPrintInfo *pInfo)
c
c   FUNCTION:  Function called after printting
c
c   INPUT:  pdc   Points to the printer device context.
c			pInfo   Points to a CPrintInfo structure that describes the current print job
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::OnEndPrinting(CDC *pdc, CPrintInfo *pInfo)
{
	if (m_pPrintFont != NULL)
	{
		delete m_pPrintFont;
		m_pPrintFont = NULL;
	}
	if (m_pnPages != NULL)
	{
		delete m_pnPages;
		m_pnPages = NULL;
	}
	m_nPrintPages = 0;
	m_nPrintLineHeight = 0;
}

/***********************************************************************
c
c   SUBROUTINE: OnPrint(CDC* pdc, CPrintInfo* pInfo) 
c
c   FUNCTION: printing the edit view onto the printer
c
c   INPUT:  pdc   Points to the printer device context.
c			pInfo   Points to a CPrintInfo structure that describes the current print job
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::OnPrint(CDC* pdc, CPrintInfo* pInfo) 
{
	if (m_pnPages == NULL)
	{
		RecalcPageLayouts(pdc, pInfo);
		ASSERT(m_pnPages != NULL);
	}

	ASSERT(pInfo->m_nCurPage >= 1 && (int) pInfo->m_nCurPage <= m_nPrintPages);
	int nLine = m_pnPages[pInfo->m_nCurPage - 1];
	int nEndLine = GetLineCount();
	if ((int) pInfo->m_nCurPage < m_nPrintPages)
		nEndLine = m_pnPages[pInfo->m_nCurPage];
	TRACE(_T("Printing page %d of %d, lines %d - %d\n"), pInfo->m_nCurPage, m_nPrintPages,
						nLine, nEndLine - 1);

	if (m_bPrintHeader)
		PrintHeader(pdc, pInfo->m_nCurPage);
	if (m_bPrintFooter)
		PrintFooter(pdc, pInfo->m_nCurPage);

	int y = m_rcPrintArea.top;
	for (; nLine < nEndLine; nLine ++)
	{
		int nLineLength = GetLineLength(nLine);
		if (nLineLength == 0)
		{
			y += m_nPrintLineHeight;
			continue;
		}

		CRect rcPrintRect = m_rcPrintArea;
		rcPrintRect.top = y;
		LPCTSTR pszChars = GetLineChars(nLine);
		CString line;
		ExpandChars(pszChars, 0, nLineLength, line);
		y += pdc->DrawText(line, &rcPrintRect, DT_LEFT | DT_NOPREFIX | DT_TOP | DT_WORDBREAK);
	}
}

/***********************************************************************
c
c   SUBROUTINE: OnFilePageSetup() 
c
c   FUNCTION: Set up the print page
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::OnFilePageSetup()
{
	CWinApp *pApp = AfxGetApp();
	ASSERT(pApp != NULL);
/*
.....we don't use the standard CPageSetupDialog because we have different field
*/
/*	CPageSetupDialog dlg;
	dlg.m_psd.Flags &= ~PSD_INTHOUSANDTHSOFINCHES;
	dlg.m_psd.Flags |= PSD_INHUNDREDTHSOFMILLIMETERS | PSD_DISABLEORIENTATION | PSD_DISABLEPAPER;
	dlg.m_psd.rtMargin.left = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_LEFT, DEFAULT_PRINT_MARGIN);
	dlg.m_psd.rtMargin.right = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_RIGHT, DEFAULT_PRINT_MARGIN);
	dlg.m_psd.rtMargin.top = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_TOP, DEFAULT_PRINT_MARGIN);
	dlg.m_psd.rtMargin.bottom = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_BOTTOM, DEFAULT_PRINT_MARGIN);
*/
	int left, right, top, bottom;

	left = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_LEFT, DEFAULT_PRINT_MARGIN) /100;
	right = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_RIGHT, DEFAULT_PRINT_MARGIN)/100;
	top = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_TOP, DEFAULT_PRINT_MARGIN)/100;
	bottom = pApp->GetProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_BOTTOM, DEFAULT_PRINT_MARGIN)/100;
	
	PtedSetupPageDialog *dlg = new PtedSetupPageDialog(m_bPrintHeader, m_bPrintFooter, left, right,
						top, bottom, m_bPrintHeaderText, m_bPrintFooterText);
	if (dlg->DoModal() == IDOK)
	{
		m_bPrintHeader = dlg->m_header;
		m_bPrintFooter = dlg->m_footer;
		if (m_bPrintHeader)
			strcpy(m_bPrintHeaderText, dlg->m_htext);
		if (m_bPrintFooter)
			strcpy(m_bPrintFooterText, dlg->m_ftext);
		left = dlg->m_left*100;
		right = dlg->m_right*100;
		top = dlg->m_top*100;
		bottom = dlg->m_bottom*100;
		pApp->WriteProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_LEFT, left);
		pApp->WriteProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_RIGHT, right);
		pApp->WriteProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_TOP, top);
		pApp->WriteProfileInt(REG_PAGE_SUBKEY, REG_MARGIN_BOTTOM, bottom);
	}
}
/////////////////////////////////////////////////////////////////////////////
// CPtedTextView message handlers

/***********************************************************************
c
c   SUBROUTINE: GetLineCount
c
c   FUNCTION:  Get Line Count
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: Line Count
c
c***********************************************************************
*/
int CPtedTextView::GetLineCount()
{
	if (m_pTextBuffer == NULL)
		return 1;	
	int nLineCount = m_pTextBuffer->GetLineCount();
//	ASSERT(nLineCount > 0);
	return nLineCount;
}

/***********************************************************************
c
c   SUBROUTINE: GetLineLength
c
c   FUNCTION:  Get Line Length
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: Line Length
c
c***********************************************************************
*/
int CPtedTextView::GetLineLength(int nLineIndex)
{
	if (m_pTextBuffer == NULL)
		return 0;
	return m_pTextBuffer->GetLineLength(nLineIndex);
}

/***********************************************************************
c
c   SUBROUTINE: GetLineChars(int nLineIndex)
c
c   FUNCTION:  Get Line characters
c
c   INPUT:  nLineIndex: line index
c
c   OUTPUT: none
c	RETURN: line characters
c
c***********************************************************************
*/
LPCTSTR CPtedTextView::GetLineChars(int nLineIndex)
{
	if (m_pTextBuffer == NULL)
		return NULL;
	return m_pTextBuffer->GetLineChars(nLineIndex);
}

/***********************************************************************
c
c   SUBROUTINE: AttachToBuffer(CPtedTextBuffer *pBuf)
c
c   FUNCTION:  attached this view to the buffer
c
c   INPUT:  pBuf: buffer to attched to
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::AttachToBuffer(CPtedTextBuffer *pBuf /*= NULL*/)
{
	m_pTextBuffer->AttachView(this);
	ResetView();

	CScrollBar *pVertScrollBarCtrl = GetScrollBarCtrl(SB_VERT);
	if (pVertScrollBarCtrl != NULL)
		pVertScrollBarCtrl->EnableScrollBar(GetScreenLines() >= GetLineCount() ?
											ESB_DISABLE_BOTH : ESB_ENABLE_BOTH);
	CScrollBar *pHorzScrollBarCtrl = GetScrollBarCtrl(SB_HORZ);
	if (pHorzScrollBarCtrl != NULL)
		pHorzScrollBarCtrl->EnableScrollBar(GetScreenChars() >= GetMaxLineLength() ?
											ESB_DISABLE_BOTH : ESB_ENABLE_BOTH);

	RecalcVertScrollBar();
	RecalcHorzScrollBar();
}

/***********************************************************************
c
c   SUBROUTINE: DetachFromBuffer
c
c   FUNCTION:  dettached this view to the text buffer
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::DetachFromBuffer()
{
	if (m_pTextBuffer != NULL)
	{
		delete m_pTextBuffer;
		m_pTextBuffer = NULL;
		ResetView();
	}
}

/***********************************************************************
c
c   SUBROUTINE: GetScreenLines
c
c   FUNCTION:  Get Lines in screen
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETURN: Screen Lines
c
c***********************************************************************
*/
int CPtedTextView::GetScreenLines()
{
	if (m_nScreenLines == -1)
	{
		CRect rect;
		GetClientRect(&rect);
		m_nScreenLines = rect.Height() / GetLineHeight();
	}
	return m_nScreenLines;
}

/***********************************************************************
c
c   SUBROUTINE: GetItalic
c
c   FUNCTION:  don't use Italic
c
c   INPUT:  nColorIndex
c
c   OUTPUT: none
c	RETURN: false
c
c***********************************************************************
*/
BOOL CPtedTextView::GetItalic(int nColorIndex)
{
	return FALSE;
}

/***********************************************************************
c
c   SUBROUTINE: GetBold
c
c   FUNCTION:  don't use Bold
c
c   INPUT:  nColorIndex
c
c   OUTPUT: none
c	RETURN: false
c
c***********************************************************************
*/
BOOL CPtedTextView::GetBold(int nColorIndex)
{
	return FALSE;
}

/***********************************************************************
c
c   SUBROUTINE: GetScreenChars
c
c   FUNCTION:  Get Chars in a line at the Screen
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETURN: Screen Chars
c
c***********************************************************************
*/
int CPtedTextView::GetScreenChars()
{
	if (m_nScreenChars == -1)
	{
		CRect rect;
		GetClientRect(&rect);
		m_nScreenChars = (rect.Width() - GetMarginWidth()) / GetCharWidth();
	}
	return m_nScreenChars;
}
/***********************************************************************
c
c   SUBROUTINE: OnEraseBkgnd
c
c   FUNCTION:  called when erase background
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETURN: true
c
c***********************************************************************
*/
BOOL CPtedTextView::OnEraseBkgnd(CDC *pdc) 
{
	return TRUE;
}

/***********************************************************************
c
c   SUBROUTINE:  OnSize( UINT nType, int cx, int cy )
c
c   FUNCTION:  This function called after resize window
c
c   INPUT:  ntype: Specifies the type of resizing 
c			cx, cy: new width and height
c   OUTPUT: none
c
c***********************************************************************/
void CPtedTextView::OnSize(UINT nType, int cx, int cy) 
{
	CView::OnSize(nType, cx, cy);
	
	if (m_pCacheBitmap != NULL)
	{
		m_pCacheBitmap->DeleteObject();
		delete m_pCacheBitmap;
		m_pCacheBitmap = NULL;
	}
	m_nScreenLines = -1;
	m_nScreenChars = -1;
	RecalcVertScrollBar();
	RecalcHorzScrollBar();
}
/***********************************************************************
c
c   SUBROUTINE:  RecalcVertScrollBar(BOOL bPositionOnly)
c
c   FUNCTION:  This function recalculate the vertical scrollbar
c
c   INPUT:  bPositionOnly: only recalculate the csrollbar position only
c			
c   OUTPUT: none
c
c***********************************************************************/
void CPtedTextView::RecalcVertScrollBar(BOOL bPositionOnly /*= FALSE*/)
{
	SCROLLINFO si;
	si.cbSize = sizeof(si);
	if (bPositionOnly)
	{
		si.fMask = SIF_POS;
		si.nPos = m_nTopLine;
	}
	else
	{
		if (GetScreenLines() >= GetLineCount() && m_nTopLine > 0)
		{
			m_nTopLine = 0;
			Invalidate();
			UpdateCaret();
		}
//		si.fMask = SIF_DISABLENOSCROLL | SIF_PAGE | SIF_POS | SIF_RANGE;
		si.fMask = SIF_PAGE | SIF_POS | SIF_RANGE;
		si.nMin = 0;
		si.nMax = GetLineCount() - 1;
		si.nPage = GetScreenLines();
		si.nPos = m_nTopLine;
	}
	VERIFY(SetScrollInfo(SB_VERT, &si));
}

/***********************************************************************
c
c   SUBROUTINE:  OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
c
c   FUNCTION:  The framework calls this member function when the user 
c				clicks the window's vertical scroll bar
c
c   INPUT:  nSBCode: Specifies a scroll-bar code that indicates 
c						the user's scrolling request.
c			nPos: the current scroll-box position
c			pScrollBar: pointer to the scroll control
c			
c   OUTPUT: none
c
c***********************************************************************/
void CPtedTextView::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	CView::OnVScroll(nSBCode, nPos, pScrollBar);

	SCROLLINFO si;
	si.cbSize = sizeof(si);
	si.fMask = SIF_ALL;
	VERIFY(GetScrollInfo(SB_VERT, &si));

	int nPageLines = GetScreenLines();
	int nLineCount = GetLineCount();

	int nNewTopLine;
	BOOL bDisableSmooth = TRUE;
	switch (nSBCode)
	{
	case SB_TOP:
		nNewTopLine = 0;
		bDisableSmooth = FALSE;
		break;
	case SB_BOTTOM:
		nNewTopLine = nLineCount - nPageLines + 1;
		bDisableSmooth = FALSE;
		break;
	case SB_LINEUP:
		nNewTopLine = m_nTopLine - 1;
		break;
	case SB_LINEDOWN:
		nNewTopLine = m_nTopLine + 1;
		break;
	case SB_PAGEUP:
		nNewTopLine = m_nTopLine - si.nPage + 1;
		bDisableSmooth = FALSE;
		break;
	case SB_PAGEDOWN:
		nNewTopLine = m_nTopLine + si.nPage - 1;
		bDisableSmooth = FALSE;
		break;
	case SB_THUMBPOSITION:
	case SB_THUMBTRACK:
		nNewTopLine = si.nTrackPos;
		break;
	default:
		return;
	}

	if (nNewTopLine < 0)
		nNewTopLine = 0;
	if (nNewTopLine >= nLineCount)
		nNewTopLine = nLineCount - 1;
	ScrollToLine(nNewTopLine, bDisableSmooth);
}
/***********************************************************************
c
c   FUNCTION: OnMouseWheel(UINT nFlags, short zDelta, CPoint pt)
c
c       The framework calls this member function 
c			when the user scroll the mouse wheel button.
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
BOOL CPtedTextView::OnMouseWheel(UINT nFlags, short zDelta, CPoint pt)
{	
	int sav_cur;
	int status = CView::OnMouseWheel(nFlags, zDelta, pt);
		
	if (zDelta==0)
		return status;
	SCROLLINFO si;
	si.cbSize = sizeof(si);
	si.fMask = SIF_ALL;
	VERIFY(GetScrollInfo(SB_VERT, &si));

	int nPageLines = GetScreenLines();
	int nLineCount = GetLineCount();

	int nNewTopLine;
	BOOL bDisableSmooth = TRUE;

	if (zDelta>0)
/*
......wheel_up
*/
		nNewTopLine = m_nTopLine - 1;
	else
/*
......wheel_down
*/
		nNewTopLine = m_nTopLine + 1;

	if (nNewTopLine < 0)
		nNewTopLine = 0;
	if (nNewTopLine >= nLineCount)
		nNewTopLine = nLineCount - 1;
	ScrollToLine(nNewTopLine, bDisableSmooth);
	return status;
}

/***********************************************************************
c
c   SUBROUTINE:  RecalcHorzScrollBar(BOOL bPositionOnly)
c
c   FUNCTION:  This function recalculate the Horizon scrollbar
c
c   INPUT:  bPositionOnly: only recalculate the csrollbar position only
c			
c   OUTPUT: none
c
c***********************************************************************/
void CPtedTextView::RecalcHorzScrollBar(BOOL bPositionOnly )
{
	SCROLLINFO si;
	si.cbSize = sizeof(si);
	if (bPositionOnly)
	{
		si.fMask = SIF_POS;
		si.nPos = m_nOffsetChar;
	}
	else
	{
		if (GetScreenChars() >= GetMaxLineLength() && m_nOffsetChar > 0)
		{
			m_nOffsetChar = 0;
			Invalidate();
			UpdateCaret();
		}
//		si.fMask = SIF_DISABLENOSCROLL | SIF_PAGE | SIF_POS | SIF_RANGE;
		si.fMask = SIF_PAGE | SIF_POS | SIF_RANGE;
		si.nMin = 0;
		si.nMax = GetMaxLineLength() - 1;
		si.nPage = GetScreenChars();
		si.nPos = m_nOffsetChar;
	}
	VERIFY(SetScrollInfo(SB_HORZ, &si));
}

/***********************************************************************
c
c   SUBROUTINE:  OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
c
c   FUNCTION:  The framework calls this member function when the user 
c				clicks the window's horizon scroll bar
c
c   INPUT:  nSBCode: Specifies a scroll-bar code that indicates 
c						the user's scrolling request.
c			nPos: the current scroll-box position
c			pScrollBar: pointer to the scroll control
c			
c   OUTPUT: none
c
c***********************************************************************/
void CPtedTextView::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	CView::OnHScroll(nSBCode, nPos, pScrollBar);

	SCROLLINFO si;
	si.cbSize = sizeof(si);
	si.fMask = SIF_ALL;
	VERIFY(GetScrollInfo(SB_HORZ, &si));

	int nPageChars = GetScreenChars();
	int nMaxLineLength = GetMaxLineLength();

	int nNewOffset;
	switch (nSBCode)
	{
	case SB_LEFT:
		nNewOffset = 0;
		break;
	case SB_BOTTOM:
		nNewOffset = nMaxLineLength - nPageChars + 1;
		break;
	case SB_LINEUP:
		nNewOffset = m_nOffsetChar - 1;
		break;
	case SB_LINEDOWN:
		nNewOffset = m_nOffsetChar + 1;
		break;
	case SB_PAGEUP:
		nNewOffset = m_nOffsetChar - si.nPage + 1;
		break;
	case SB_PAGEDOWN:
		nNewOffset = m_nOffsetChar + si.nPage - 1;
		break;
	case SB_THUMBPOSITION:
	case SB_THUMBTRACK:
		nNewOffset = si.nTrackPos;
		break;
	default:
		return;
	}

	if (nNewOffset >= nMaxLineLength)
		nNewOffset = nMaxLineLength - 1;
	if (nNewOffset < 0)
		nNewOffset = 0;
	ScrollToChar(nNewOffset, TRUE);
	UpdateCaret();
}

/***********************************************************************
c
c   SUBROUTINE:  OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message)
c
c   FUNCTION:  TThe framework calls this member function if mouse input 
c				is not captured and the mouse causes cursor movement 
c				within the CWnd object.
c
c   INPUT:  pWnd   Specifies a pointer to the window that contains the cursor
c			nHitTest   Specifies the hit-test area code. The hit test 
c						determines the cursor's location
c			message: Specifies the mouse message number
c			
c   OUTPUT: none
c
c***********************************************************************/
BOOL CPtedTextView::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message) 
{
	if (nHitTest == HTCLIENT)
	{
		CPoint pt;
		::GetCursorPos(&pt);
		ScreenToClient(&pt);
		if (pt.x < GetMarginWidth())
		{
			::SetCursor(::LoadCursor(GetResourceHandle(), MAKEINTRESOURCE(IDR_MARGIN_CURSOR)));
		}
		else
		{
			CPoint ptText = ClientToText(pt);
			PrepareSelBounds();
//			if (IsInsideSelBlock(ptText))
//			{
//				if (!m_bDisableDragAndDrop)				
//					::SetCursor(::LoadCursor(NULL, MAKEINTRESOURCE(IDC_ARROW)));
//			}
//			else
				::SetCursor(::LoadCursor(NULL, MAKEINTRESOURCE(IDC_IBEAM)));
		}
		return TRUE;
	}
	return CView::OnSetCursor(pWnd, nHitTest, message);
}

/***********************************************************************
c
c   SUBROUTINE:  ClientToText(const CPoint &point)
c
c   FUNCTION:  The function translate the client position to text position.
c
c   INPUT:  point: client position 
c			
c   OUTPUT: none
c	RETURN: text position
c
c***********************************************************************/
CPoint CPtedTextView::ClientToText(const CPoint &point)
{
	int nLineCount = GetLineCount();

	CPoint pt;
	pt.y = m_nTopLine + point.y / GetLineHeight();
	if (pt.y >= nLineCount)
		pt.y = nLineCount - 1;
	if (pt.y < 0)
		pt.y = 0;

	int nLength = 0;
	LPCTSTR pszLine = NULL;
	if (pt.y >= 0 && pt.y < nLineCount)
	{
		nLength = GetLineLength(pt.y);
		pszLine = GetLineChars(pt.y);
	}

	int nPos = m_nOffsetChar + (point.x - GetMarginWidth()) / GetCharWidth();
	if (nPos < 0)
		nPos = 0;

	int nIndex = 0, nCurPos = 0;
	int nTabSize = GetTabSize();
	while (nIndex < nLength)
	{
		if (pszLine[nIndex] == _T('\t'))
			nCurPos += (nTabSize - nCurPos % nTabSize);
		else
			nCurPos ++;

		if (nCurPos > nPos)
			break;

		nIndex ++;
	}

	ASSERT(nIndex >= 0 && nIndex <= nLength);
	pt.x = nIndex;
	return pt;
}

#ifdef _DEBUG
void CPtedTextView::AssertValidTextPos(const CPoint &point)
{
	if (GetLineCount() > 0)
	{
		ASSERT(m_nTopLine >= 0 && m_nOffsetChar >= 0);
		int len = GetLineCount();
		ASSERT(point.y >= 0 && point.y < GetLineCount());
		len = GetLineLength(point.y);
//		ASSERT(point.x >= 0 && point.x <= GetLineLength(point.y));
	}
}
#endif

/***********************************************************************
c
c   SUBROUTINE:  TextToClient(const CPoint &point)
c
c   FUNCTION:  The function translate text position to the client position.
c
c   INPUT:  point: text position 
c			
c   OUTPUT: none
c	RETURN: client position
c
c***********************************************************************/
CPoint CPtedTextView::TextToClient(const CPoint &point)
{
	ASSERT_VALIDTEXTPOS(point);
	int nLength = GetLineLength(point.y);
	LPCTSTR pszLine = GetLineChars(point.y);

	CPoint pt;
	pt.y = (point.y - m_nTopLine) * GetLineHeight();
	pt.x = 0;
	int nTabSize = GetTabSize();
	for (int i = 0; i < point.x; i ++)
	{
		if (pszLine[i] == _T('\t'))
			pt.x += (nTabSize - pt.x % nTabSize);
		else
			pt.x ++;
	}

	pt.x = (pt.x - m_nOffsetChar) * GetCharWidth() + GetMarginWidth();
	return pt;
}

/***********************************************************************
c
c   SUBROUTINE:  InvalidateLines(int nLine1, int nLine2, BOOL bInvalidateMargin)
c
c   FUNCTION:  The function Invalidates from line1 to line2
c
c   INPUT:  nLine1, nLine2: range to be Invalidated  
c			bInvalidateMargin: if Invalidate Margin (always true now)
c			
c   OUTPUT: none
c	RETURN:none
c
c***********************************************************************/
void CPtedTextView::InvalidateLines(int nLine1, int nLine2, BOOL bInvalidateMargin /*= FALSE*/)
{
	bInvalidateMargin = TRUE;
	if (nLine2 == -1)
	{
		CRect rcInvalid;
		GetClientRect(&rcInvalid);
		if (! bInvalidateMargin)
			rcInvalid.left += GetMarginWidth();
		rcInvalid.top = (nLine1 - m_nTopLine) * GetLineHeight();
		InvalidateRect(&rcInvalid, FALSE);
	}
	else
	{
		if (nLine2 < nLine1)
		{
			int nTemp = nLine1;
			nLine1 = nLine2;
			nLine2 = nTemp;
		}
		CRect rcInvalid;
		GetClientRect(&rcInvalid);
		if (! bInvalidateMargin)
			rcInvalid.left += GetMarginWidth();
		rcInvalid.top = (nLine1 - m_nTopLine) * GetLineHeight();
		rcInvalid.bottom = (nLine2 - m_nTopLine + 1) * GetLineHeight();
		InvalidateRect(&rcInvalid, FALSE);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  SetSelection(const CPoint &ptStart, const CPoint &ptEnd)
c
c   FUNCTION:  The function Set Selection
c
c   INPUT:  ptStart, ptEnd: range to be Selected  
c			
c   OUTPUT: none
c	RETURN:none
c
c***********************************************************************/
void CPtedTextView::SetSelection(const CPoint &ptStart, const CPoint &ptEnd)
{
	ASSERT_VALIDTEXTPOS(ptStart);
	ASSERT_VALIDTEXTPOS(ptEnd);
	
	if ((ptStart==ptEnd) && (m_ptSelStart!=m_ptSelEnd))
	{
		((CPtedWindow*)m_parent)->SetCutCopyDel_menu(0);
	}
	else if ((ptStart!=ptEnd) && (m_ptSelStart==m_ptSelEnd))
		((CPtedWindow*)m_parent)->SetCutCopyDel_menu(1);

	if (m_ptSelStart == ptStart)
	{
		if (m_ptSelEnd != ptEnd)
			InvalidateLines(ptEnd.y, m_ptSelEnd.y);
	}
	else
	{
		InvalidateLines(ptStart.y, ptEnd.y);
		InvalidateLines(m_ptSelStart.y, m_ptSelEnd.y);
	}
	m_ptSelStart = ptStart;
	m_ptSelEnd = ptEnd;
}

/***********************************************************************
c
c   SUBROUTINE:  AdjustTextPoint(CPoint &point)
c
c   FUNCTION:  The function Set Selection
c
c   INPUT:  point: point to be adjusted  
c			
c   OUTPUT: point
c	RETURN:none
c
c***********************************************************************/
void CPtedTextView::AdjustTextPoint(CPoint &point)
{
	point.x += GetCharWidth() / 2;	
}

/***********************************************************************
c
c   SUBROUTINE:  OnSetFocus(CWnd* pOldWnd)
c
c   FUNCTION:  The framework calls this member function after gaining 
c				the input focus
c
c   INPUT:  pOldWnd   Contains the CWnd object that loses the input focus
c			
c   OUTPUT: none
c	RETURN:none
c
c***********************************************************************/
void CPtedTextView::OnSetFocus(CWnd* pOldWnd) 
{
	CView::OnSetFocus(pOldWnd);

	m_bFocused = TRUE;
	if (m_ptSelStart != m_ptSelEnd)
		InvalidateLines(m_ptSelStart.y, m_ptSelEnd.y);
	UpdateCaret();
}


/***********************************************************************
c
c   SUBROUTINE:  CalculateActualOffset(int nLineIndex, int nCharIndex)
c
c   FUNCTION:  The function calculate the actual offset
c
c   INPUT:  nLineIndex: line index
c			  nCharIndex: character index
c			
c   OUTPUT: none
c	RETURN: Offset
c
c***********************************************************************/
int CPtedTextView::CalculateActualOffset(int nLineIndex, int nCharIndex)
{
	int nLength = GetLineLength(nLineIndex);
	ASSERT(nCharIndex >= 0 && nCharIndex <= nLength);
	LPCTSTR pszChars = GetLineChars(nLineIndex);
	int nOffset = 0;
	int nTabSize = GetTabSize();
	for (int I = 0; I < nCharIndex; I ++)
	{
		if (pszChars[I] == _T('\t'))
			nOffset += (nTabSize - nOffset % nTabSize);
		else
			nOffset ++;
	}
	return nOffset;
}

/***********************************************************************
c
c   SUBROUTINE:  ApproxActualOffset(int nLineIndex, int nOffset)
c
c   FUNCTION:  The function approximate actual offset
c
c   INPUT:  nLineIndex: line index
c			  nOffset: character index
c			
c   OUTPUT: none
c	RETURN: approximate number
c
c***********************************************************************/
int CPtedTextView::ApproxActualOffset(int nLineIndex, int nOffset)
{
	if (nOffset == 0)
		return 0;

	int nLength = GetLineLength(nLineIndex);
	LPCTSTR pszChars = GetLineChars(nLineIndex);
	int nCurrentOffset = 0;
	int nTabSize = GetTabSize();
	for (int I = 0; I < nLength; I ++)
	{
		if (pszChars[I] == _T('\t'))
			nCurrentOffset += (nTabSize - nCurrentOffset % nTabSize);
		else
			nCurrentOffset ++;
		if (nCurrentOffset >= nOffset)
		{
			if (nOffset <= nCurrentOffset - nTabSize / 2)
				return I;
			return I + 1;
		}
	}
	return nLength;
}

/***********************************************************************
c
c   SUBROUTINE:  EnsureVisible(CPoint pt)
c
c   FUNCTION:  The function make point visible
c
c   INPUT:  pt: point
c			
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************/
void CPtedTextView::EnsureVisible(CPoint pt)
{
	int nLineCount = GetLineCount();
	int nNewTopLine = m_nTopLine;
	if (pt.y >= nNewTopLine + GetScreenLines())
	{
		nNewTopLine = pt.y - GetScreenLines() + 1;
	}
	if (pt.y < nNewTopLine)
	{
		nNewTopLine = pt.y;
	}

	if (nNewTopLine < 0)
		nNewTopLine = 0;
	if (nNewTopLine >= nLineCount)
		nNewTopLine = nLineCount - 1;

	if (m_nTopLine != nNewTopLine)
	{
		ScrollToLine(nNewTopLine);
	}

	int nActualPos = CalculateActualOffset(pt.y, pt.x);
	int nNewOffset = m_nOffsetChar;
	if (nActualPos > nNewOffset + GetScreenChars())
	{
		nNewOffset = nActualPos - GetScreenChars();
	}
	if (nActualPos < nNewOffset)
	{
		nNewOffset = nActualPos;
	}

	if (nNewOffset >= GetMaxLineLength())
		nNewOffset = GetMaxLineLength() - 1;
	if (nNewOffset < 0)
		nNewOffset = 0;

	if (m_nOffsetChar != nNewOffset)
	{
		ScrollToChar(nNewOffset);
		UpdateCaret();
	}
}
/***********************************************************************
c
c   SUBROUTINE: SetBookMarkWindow(int flag)
c
c   FUNCTION:  The function set the select margin display, then we can use marker
c
c   INPUT:  flag: 1: marker window enable
c			
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************/
void CPtedTextView::SetBookMarkWindow(int flag)
{
	m_bSelMargin = flag;
}
/***********************************************************************
c
c   SUBROUTINE: SetSyntaxClrWindow(int flag)
c
c   FUNCTION:  The function set the syntax color display
c
c   INPUT:  flag: 1: display with syntax color
c			
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************/
void CPtedTextView::SetSyntaxClrWindow(int flag)
{
	m_syntax_color = flag;
}

/***********************************************************************
c
c   SUBROUTINE: OnKillFocus(CWnd* pNewWnd)
c
c   FUNCTION:  this member function called immediately before losing the input focus
c
c   INPUT:  pNewWnd:pointer to the window that receives the input focus 
c			
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************/
void CPtedTextView::OnKillFocus(CWnd* pNewWnd) 
{
	CView::OnKillFocus(pNewWnd);

	m_bFocused = FALSE;
	UpdateCaret();
	if (m_ptSelStart != m_ptSelEnd)
		InvalidateLines(m_ptSelStart.y, m_ptSelEnd.y);
	if (m_bDragSelection)
	{
		ReleaseCapture();
		KillTimer(m_nDragSelTimer);
		m_bDragSelection = FALSE;
	}
}

/***********************************************************************
c
c   SUBROUTINE: OnSysColorChange
c
c   FUNCTION:  this member function called when change is made in the system color setting.
c
c   INPUT:  none
c			
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************/
void CPtedTextView::OnSysColorChange() 
{
	CView::OnSysColorChange();
	Invalidate();
}

/***********************************************************************
c
c   SUBROUTINE: GetText(const CPoint &ptStart, const CPoint &ptEnd, CString &text)
c
c   FUNCTION:  get the text from certain range
c
c   INPUT:  ptStart, ptEnd: range o get text
c			
c   OUTPUT: text: text to get
c	RETURN: none
c
c***********************************************************************/
void CPtedTextView::GetText(const CPoint &ptStart, const CPoint &ptEnd, CString &text)
{
	if (m_pTextBuffer != NULL)
		m_pTextBuffer->GetText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x, text);
	else
		text = _T("");
}
/***********************************************************************
c
c   SUBROUTINE: GetTextLength(int nStartLine, int nStartChar, int nEndLine, int nEndChar)
c
c   FUNCTION:  get the text from certain range
c
c   INPUT:  nStartLine, nStartChar, nEndLine,nEndChar: range o get text length
c			
c   OUTPUT: none
c	RETURN: text length
c
c***********************************************************************/
int CPtedTextView::GetTextLength(int nStartLine, int nStartChar, int nEndLine, int nEndChar)
{
	if (m_pTextBuffer != NULL)
		return m_pTextBuffer->GetTextLength(nStartLine, nStartChar, nEndLine, nEndChar);
	else
		return 0;
}


HINSTANCE CPtedTextView::GetResourceHandle()
{
	if (s_hResourceInst != NULL)
		return s_hResourceInst;
	return AfxGetResourceHandle();
}

/***********************************************************************
**
**   FUNCTION: OnCreate(LPCREATESTRUCT lpCreateStruct) 
**
**		Override this member function to perform any needed 
**		initialization of a derived class. 
**   
**		INPUT:  LPCREATESTRUCT lpCreateStruct: contains copies of 
**						the parameters used to create the window.
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CPtedTextView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	memset(&m_lfBaseFont, 0, sizeof(m_lfBaseFont));
	lstrcpy(m_lfBaseFont.lfFaceName, _T("FixedSys"));
	m_lfBaseFont.lfHeight = 0;
	m_lfBaseFont.lfWeight = FW_NORMAL;
	m_lfBaseFont.lfItalic = FALSE;
	m_lfBaseFont.lfCharSet = DEFAULT_CHARSET;
	m_lfBaseFont.lfOutPrecision = OUT_DEFAULT_PRECIS;
	m_lfBaseFont.lfClipPrecision = CLIP_DEFAULT_PRECIS;
	m_lfBaseFont.lfQuality = DEFAULT_QUALITY;
	m_lfBaseFont.lfPitchAndFamily = DEFAULT_PITCH;

	if (CView::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	ASSERT(m_hAccel == NULL);
	strcpy(m_menuname, "IDR_DEFAULT_TEXTACCEL");
	m_hAccel = ::LoadAccelerators(GetResourceHandle(), m_menuname);
	ASSERT(m_hAccel != NULL);
	ASSERT(m_pDropTarget == NULL);
/*	m_pDropTarget = new CEditDropTargetImpl(this);
	if (! m_pDropTarget->Register(this))
	{
		TRACE0("Warning: Unable to register drop target for CPtedTextView.\n");
		delete m_pDropTarget;
		m_pDropTarget = NULL;
	}
*/	m_pDropTarget = NULL;
	OnInitialUpdate();
	SetFocus();
	return 0;
}

void CPtedTextView::SetAnchor(const CPoint &ptNewAnchor)
{
	ASSERT_VALIDTEXTPOS(ptNewAnchor);
	m_ptAnchor = ptNewAnchor;
}

/***********************************************************************
c
c   FUNCTION: PreTranslateMessage(MSG* pMsg) 
c
c       translate window messages before they are dispatch
c
c   INPUT:  pMsg   Points to a MSG structure that contains the 
c					message to process.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CPtedTextView::PreTranslateMessage(MSG *pMsg) 
{
	if (pMsg->message >= WM_KEYFIRST && pMsg->message <= WM_KEYLAST)
	{
		if (m_hAccel != NULL)
		{
			if (::TranslateAccelerator(m_hWnd, m_hAccel, pMsg))
				return TRUE;
		}
	}
	return FALSE;
}



/***********************************************************************
c
c   FUNCTION: HighlightText(const CPoint &ptStartPos, int nLength)
c
c       Highlight Text in certain range
c
c   INPUT:  ptStartPos:   start point to a highlight
c			nLength: highlight length
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CPtedTextView::HighlightText(const CPoint &ptStartPos, int nLength)
{
	ASSERT_VALIDTEXTPOS(ptStartPos);
	m_ptCursorPos = ptStartPos;
	m_ptCursorPos.x += nLength;
	ASSERT_VALIDTEXTPOS(m_ptCursorPos);		
	m_ptAnchor = m_ptCursorPos;
	SetSelection(ptStartPos, m_ptCursorPos);
	UpdateCaret();
	EnsureVisible(m_ptCursorPos);
	return TRUE;
}

/***********************************************************************
c
c   FUNCTION: HighLightSelLine(int line)
c
c       Highlight the whole line
c
c   INPUT:  line:   line to be high lighted
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPtedTextView::HighLightSelLine(int line)
{
	if (m_pTextBuffer != NULL)
	{
		DWORD dwMask  = PTED_SELMARKS;
		m_pTextBuffer->SetLineFlag(line, dwMask, 1, 1);
	}
}
//BOOL CPtedTextView::GetDisableDragAndDrop() const
//{
//	return m_bDisableDragAndDrop;
//}

//void CPtedTextView::SetDisableDragAndDrop(BOOL bDDAD)
//{
//	m_bDisableDragAndDrop = bDDAD;
//}



/***********************************************************************
c
c   SUBROUTINE: QueryEditable
c
c   FUNCTION:  see if it's read only
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
BOOL CPtedTextView::QueryEditable()
{
	if (m_pTextBuffer == NULL)
		return FALSE;
	return ! m_pTextBuffer->GetReadOnly();
}

/***********************************************************************
c
c   SUBROUTINE: DeleteCurrentSelection
c
c   FUNCTION:  Delete Current Selection
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: true: delete selection
c
c***********************************************************************
*/
BOOL CPtedTextView::DeleteCurrentSelection()
{
	if (IsSelection())
	{
		CPoint ptSelStart, ptSelEnd;
		GetSelection(ptSelStart, ptSelEnd);

		CPoint ptCursorPos = ptSelStart;
		ASSERT_VALIDTEXTPOS(ptCursorPos);
		SetAnchor(ptCursorPos);
		SetSelection(ptCursorPos, ptCursorPos);
		SetCursorPos(ptCursorPos);
		EnsureVisible(ptCursorPos);

		m_pTextBuffer->DeleteText(ptSelStart.y, ptSelStart.x, ptSelEnd.y, ptSelEnd.x, PTED_DELETE_SEL);
		return TRUE;
	}
	return FALSE;
}

/***********************************************************************
c
c   SUBROUTINE: OnChar
c
c   FUNCTION:  called when typing a character
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	int action, flag = 0;
	CView::OnChar(nChar, nRepCnt, nFlags);

	if ((::GetAsyncKeyState(VK_LBUTTON) & 0x8000) != 0 ||
			(::GetAsyncKeyState(VK_RBUTTON) & 0x8000) != 0)
		return;

	BOOL bTranslated = FALSE;
	if (nChar == VK_RETURN)
	{
		if (m_bOvrMode)
		{
			CPoint ptCursorPos = GetCursorPos();
			ASSERT_VALIDTEXTPOS(ptCursorPos);
			if (ptCursorPos.y < GetLineCount() - 1)
			{
				ptCursorPos.x = 0;
				ptCursorPos.y++;

				ASSERT_VALIDTEXTPOS(ptCursorPos);
				SetSelection(ptCursorPos, ptCursorPos);
				SetAnchor(ptCursorPos);
				SetCursorPos(ptCursorPos);
				EnsureVisible(ptCursorPos);
				return;
			}
		}

		m_pTextBuffer->BeginUndoGroup();

		if (QueryEditable() && m_pTextBuffer != NULL)
		{
			DeleteCurrentSelection();

			CPoint ptCursorPos = GetCursorPos();
			ASSERT_VALIDTEXTPOS(ptCursorPos);
			const static TCHAR pszText[3] = _T("\r\n");

			int x, y;
			m_pTextBuffer->InsertText(ptCursorPos.y, ptCursorPos.x, pszText, y, x, PTED_TYPING);
			ptCursorPos.x = x;
			ptCursorPos.y = y;
			ASSERT_VALIDTEXTPOS(ptCursorPos);
			SetSelection(ptCursorPos, ptCursorPos);
			SetAnchor(ptCursorPos);
			SetCursorPos(ptCursorPos);
			EnsureVisible(ptCursorPos);
		}

		m_pTextBuffer->FlushUndoGroup(this);
		goto done;
	}
	if (nChar > 31)
	{
		if (QueryEditable() && m_pTextBuffer != NULL)
		{
			action = m_pTextBuffer->GetUndoActive();
			if ((action!=-1) && (action!=PTED_TYPING))
				flag = 0;
			else
				flag = (nChar != _T(' '));
//				flag = 1;
			m_pTextBuffer->BeginUndoGroup(flag);
			CPoint ptSelStart, ptSelEnd;
			GetSelection(ptSelStart, ptSelEnd);
			CPoint ptCursorPos;
			if (ptSelStart != ptSelEnd)
			{
				ptCursorPos = ptSelStart;
				DeleteCurrentSelection();
			}
			else
			{
				ptCursorPos = GetCursorPos();
				if (m_bOvrMode && ptCursorPos.x < GetLineLength(ptCursorPos.y))
					m_pTextBuffer->DeleteText(ptCursorPos.y, ptCursorPos.x, ptCursorPos.y, ptCursorPos.x + 1, PTED_TYPING); 
			}

			ASSERT_VALIDTEXTPOS(ptCursorPos);

			char pszText[2];
			pszText[0] = (char) nChar;
			pszText[1] = 0;

			int x, y;
			USES_CONVERSION;
			m_pTextBuffer->InsertText(ptCursorPos.y, ptCursorPos.x, A2T(pszText), y, x, PTED_TYPING); 
			ptCursorPos.x = x;
			ptCursorPos.y = y;
			ASSERT_VALIDTEXTPOS(ptCursorPos);
			SetSelection(ptCursorPos, ptCursorPos);
			SetAnchor(ptCursorPos);
			SetCursorPos(ptCursorPos);
			EnsureVisible(ptCursorPos);

			m_pTextBuffer->FlushUndoGroup(this);
		}
	}
done:;
	((CPtedWindow*)m_parent)->Update_undo_redomenu(1, 0);
	RedrawWindow();
}
