#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**		FILE NAME: wsnttblst.cpp
**
**	 Description - Functions and implementations for
**		CNCLTableList class (NCL table list)
**		This class right have drag and other feature than CNCLTableList
**		
**	 CONTAINS:
**		member function of CNCLTableList
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttblst.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:30
**********************************************************************
*/
#include "wsntstdafx.h"
#include "wsnttblst.h"
#include <afxole.h>         // MFC OLE classes
#include "afxpriv.h"
#include "wsntcfunc.h"
#include "wsntres.h"
#include "wsntmenudsndlg.h"
#include "wsntframe.h"
#include "wsntbitmap.h"
#include "wsntheaderctrl.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

UINT UW_LIST_REPAINT = WM_APP+100011;
UINT UW_SEL_TREE = WM_APP+100012;
extern "C" int uw_ntsave_cur_menudegn(int *);
extern CMainFrame *NCL_MainFrame;
extern "C" int UW_icon_size;
extern int UW_drag_obj;
/*
extern void TransparentBlt( HDC hdcDest, int nXDest, int nYDest, int nWidth, 
			int nHeight, HBITMAP hBitmap, int nXSrc, int nYSrc,
			COLORREF colorTransparent);
*/
////////////////////////////////////////////////////////////////////////////
// CNCLTableList

/***********************************************************************
c
c   SUBROUTINE:  CNCLTableList
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLTableList::CNCLTableList()
{
	m_selitem = -1;
	m_change = 0;

	m_StartPoint = CPoint(-1, -1);
	m_TargetDrop = NULL;
	m_TimerID = 0;

	m_row_height = 20;
	for (int i=0; i<5000; i++)
	{
		m_hBitmap[i] = NULL;
	}
	m_iconsize = 16;
	m_parent = NULL;
}

CNCLTableList::~CNCLTableList()
{
}

BEGIN_MESSAGE_MAP(CNCLTableList, CListCtrl)
	//{{AFX_MSG_MAP(CNCLTableList)
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_PAINT()
	ON_WM_NCCALCSIZE()
	ON_WM_LBUTTONDOWN()
	ON_WM_MOUSEMOVE()
	ON_WM_TIMER()
	ON_WM_DESTROY()
	ON_WM_DRAWITEM()

	ON_WM_CREATE()
	ON_NOTIFY_REFLECT(NM_CUSTOMDRAW, OnCustomDraw)
	ON_WM_HSCROLL()
	ON_WM_VSCROLL()
	ON_WM_KEYDOWN()
	ON_WM_MEASUREITEM_REFLECT()
	ON_WM_ERASEBKGND()
	ON_COMMAND(UW_LIST_REPAINT, OnAdjustStatic)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   FUNCTION: OnAdjustStatic()
c
c       adjust the size and position of the filter area
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    none
c
**********************************************************************/
void CNCLTableList::OnAdjustStatic()
{
	m_HeaderCtrl.OnAdjustStatic();
	RedrawWindow();
	UpdateWindow();
	((CNCLMenuDsnDlg*)m_parent)->RedrawWindow();
	((CNCLMenuDsnDlg*)m_parent)->UpdateWindow();
}
/***********************************************************************
c
c   FUNCTION: PreSubclassWindow()
c
c       assign our own class for the list header
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    none
c
**********************************************************************/
void CNCLTableList::PreSubclassWindow() 
{
	//use our custom CHeaderCtrl
	m_HeaderCtrl.SubclassWindow(GetHeaderCtrl()->m_hWnd);
	CListCtrl::PreSubclassWindow();
}
/***********************************************************************
c
c   FUNCTION: ResetRowSize()
c
c       after we assign the lsi row size, the function drawitem not called
c		until the position changed, so we pretend one
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    none
c
**********************************************************************/
void CNCLTableList::ResetRowSize()
{
	CRect rect;
	WINDOWPOS pos;

	GetWindowRect(&rect);
	pos.hwnd = m_hWnd;
	pos.cx = rect.Width();
	pos.cy = rect.Height();
	pos.flags = SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOOWNERZORDER | SWP_NOZORDER;
	SendMessage(WM_WINDOWPOSCHANGED,0, (LPARAM) &pos);
}

/***********************************************************************
**
**   FUNCTION: MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct)
**
**         Setm table list item size
**
**   INPUT:  lpMeasureItemStruct:   A pointer to a 
**				DRAWITEMSTRUCT structure that contains 
**				information about the type of drawing required.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLTableList::MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct )
{
	TEXTMETRIC tm;
	HDC hDC = ::GetDC(NULL);
	CFont *pFont = GetFont();
	HFONT hFontOld = (HFONT)SelectObject(hDC, pFont->GetSafeHandle());
	GetTextMetrics(hDC, &tm);
	lpMeasureItemStruct->itemHeight = tm.tmHeight + tm.tmExternalLeading + 1;
	if (lpMeasureItemStruct->itemHeight<20)
		lpMeasureItemStruct->itemHeight = 20;
	m_row_height = lpMeasureItemStruct->itemHeight;
	SelectObject(hDC, hFontOld);
	::ReleaseDC(NULL, hDC);
}

/////////////////////////////////////////////////////////////////////////////
// CNCLTableList message handlers

/***********************************************************************
c
c   FUNCTION: OnHeadItemEndTrack (NMHDR* pNMHDR, LRESULT* pResult) 
c
c       Callback function for end of adjusting header item
c
c   INPUT:  pNMHDR: data structure include header ajust information
c
c   OUTPUT : 
c			pResult:  
c   RETURN:    None
c
**********************************************************************/
void CNCLTableList::OnHeadItemEndTrack (NMHDR* pNMHDR, LRESULT* pResult)
{
	NMHEADER *pHdr = (HD_NOTIFY*)pNMHDR;
	CRect rcRect, rcRect1, rcRect2, rcRect3, rcRect4, rcRect5;
	int width;
	if (!((pHdr->pitem)&&(pHdr->pitem->mask&HDI_WIDTH)))
		return;
	width = pHdr->pitem->cxy;
	m_HeaderCtrl.OnHeadItemEndTrack (pNMHDR, pResult);
/*
.....the resize of window above "MoveWindow" does not work inside the ENDTRACK callback function
.....we have to post this message to the top top and redo it
*/
	PostMessage(WM_COMMAND, UW_LIST_REPAINT);
}


/***********************************************************************
c
c   FUNCTION: OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
c
c      Callback function for Horizetal scroll
c
c   INPUT:  nSBCode: scroll code of operation
c			nPos: scrolling position
c			pScrollBar: scrolling bar
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLTableList::OnHScroll(UINT iSBCode, UINT iPos, CScrollBar* pScrollBar) 
{
	if (GetFocus() != this)
	{
		SetFocus();
	}
	CListCtrl::OnHScroll(iSBCode, iPos, pScrollBar);
}
void CNCLTableList::OnPaint() 
{
	CListCtrl::OnPaint();
}

/***********************************************************************
c
c   FUNCTION: OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
c
c      Callback function for V scroll
c
c   INPUT:  nSBCode: scroll code of operation
c			nPos: scrolling position
c			pScrollBar: scrolling bar
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLTableList::OnVScroll(UINT iSBCode, UINT iPos, CScrollBar* pScrollBar) 
{
	if (GetFocus() != this)
	{
		SetFocus();
	}
	
	CListCtrl::OnVScroll(iSBCode, iPos, pScrollBar);	

	SCROLLINFO si1;
	si1.cbSize = sizeof(si1);
	GetScrollInfo(SB_VERT, &si1);	
/*
.....for sone reason, the last line is wrong (the CListCtrl::OnVScroll will have one extra with random item)
.....and need repaint from dialog level to fixed
*/
	if (si1.nPos+si1.nPage >=si1.nMax)
	{
		((CNCLMenuDsnDlg*)m_parent)->RedrawWindow();
		((CNCLMenuDsnDlg*)m_parent)->UpdateWindow();
	}
}

/**********************************************************************
**    I_FUNCTION :  OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags)
**       callback routine for key down
**    PARAMETERS   
**       INPUT  : 
**				nChar,      nRepCnt,    nFlags               
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLTableList::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	int top, per_page, total;
	CListCtrl::OnKeyDown(nChar, nRepCnt, nFlags);
	if ((nChar==VK_DOWN)||(nChar==VK_NEXT)||(nChar==VK_END))
	{
		top = GetTopIndex();
		per_page = GetCountPerPage();
		total = GetItemCount();
		if (top+per_page >=total)
		{
			((CNCLMenuDsnDlg*)m_parent)->RedrawWindow();
			((CNCLMenuDsnDlg*)m_parent)->UpdateWindow();
		}
	}
}
/***********************************************************************
**
**   FUNCTION: SetBitmap(HBITMAP hbmap, HBITMAP hchkbmap, int num, UINT id, int bnum)
**
**         Set bitmap (icon size) used by table list
**
**   INPUT:  hbmap: bitmap handle for list item
**			itemnum: item item index number
**			id: menu item id
**			bnum: bitmap number inside hbmap
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLTableList::SetBitmap(HBITMAP hbmap, int itemnum)
{
	m_hBitmap[itemnum] = hbmap;
	if ((UW_icon_size==0)||(UW_icon_size==1)||(UW_icon_size==2))
	{
		m_iconsize = 16;
	}
	else if ((UW_icon_size==3)||(UW_icon_size==4))
	{
		m_iconsize = 24;
	}
}

/***********************************************************************
**
**   FUNCTION: ReSetBitmap
**
**         Reset bitmap used by table list
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLTableList::ReSetBitmap()
{
	for (int i=0; i<5000; i++)
	{
		m_hBitmap[i] = NULL;
	}
}

/***********************************************************************
**
**   FUNCTION: DrawItem(LPDRAWITEMSTRUCT lpDIS)
**
**         Called by the framework when a visual aspect of 
**			an owner-drawn control list item changes
**
**   INPUT:  lpDIS:   A pointer to a 
**					DRAWITEMSTRUCT structure that contains 
**					information about the type of drawing required.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLTableList::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	CDC* pDC=CDC::FromHandle(lpDrawItemStruct->hDC);
	CRect Rect(lpDrawItemStruct->rcItem);
	int itemnum=lpDrawItemStruct->itemID;
	CImageList* pImageList=GetImageList(LVSIL_SMALL);
	bool HasFocus=(GetFocus()==this);

	LV_ITEM lvi;
	lvi.mask=LVIF_IMAGE | LVIF_STATE;
	lvi.iItem=itemnum;
	lvi.iSubItem=0;
	lvi.stateMask=-1;
	GetItem(&lvi);
	CRect VisibleRect;
	GetClientRect(&VisibleRect);
	VisibleRect.top=Rect.top;
	VisibleRect.bottom=Rect.bottom;

	if(lvi.state & LVIS_SELECTED) 
	{
		if((lvi.state & LVIS_DROPHILITED) || HasFocus) 
		{ 
			pDC->SetBkColor  (GetSysColor(COLOR_HIGHLIGHT));
			pDC->SetTextColor(GetSysColor(COLOR_HIGHLIGHTTEXT));
		}
		else
		{
			pDC->SetBkColor  (RGB(220,220,220));
			pDC->SetTextColor(GetSysColor(COLOR_BTNTEXT));
		}
	}
	else
	{
		pDC->SetBkColor  (GetSysColor(COLOR_WINDOW    ));
		pDC->SetTextColor(GetSysColor(COLOR_WINDOWTEXT));
	}

	LVCOLUMN lvc;
	lvc.mask=LVCF_FMT | LVCF_WIDTH;
	CRect CellRect(Rect);
	CellRect.right=CellRect.left; // Add the column width to it in the Column loop:
	for (int Col=0; GetColumn(Col, &lvc); ++Col) 
	{
		CellRect.left=CellRect.right; // Next cell
		CellRect.right+=lvc.cx; // Keep track of the right of the cell
		pDC->FillSolidRect(CellRect, pDC->GetBkColor());

		if ((CellRect.right<VisibleRect.left)) 
			continue;

		lvi.iSubItem=Col;
		GetItem(&lvi);

		if ((Col==0) && (CellRect.right>VisibleRect.left) 
			&& (lvi.state & LVIS_STATEIMAGEMASK) 
			&& GetImageList(LVSIL_STATE)) 
		{
			GetImageList(LVSIL_STATE)->Draw(pDC, ((lvi.state & LVIS_STATEIMAGEMASK)>>12)-1, CellRect.TopLeft(), ILD_TRANSPARENT);
		}

		POINT ptOrigin;
		ptOrigin.x = 0;
		ptOrigin.y = 0;
		CPoint pt;
/*
.....strangely, the first col image is always 0 even we setup as -1, but we don't want to draw
.....any image in the first column, so we check here
*/
		if ((pImageList && lvi.iImage!=-1)&&(lvi.iSubItem!=0))
		{
			CellRect.left += pDC->GetTextExtent(" ",1).cx<<1; 
			pt.x = CellRect.left;
			pt.y = (Rect.top + Rect.bottom)/2 - m_iconsize/2;
			
			pImageList->Draw(pDC, lvi.iImage, pt, ILD_TRANSPARENT);
/*			if (m_hBitmap[itemnum]!=NULL)
			{
				TransparentBlt(lpDrawItemStruct->hDC, pt.x, pt.y , m_iconsize, 
					m_iconsize, m_hBitmap[itemnum], ptOrigin.x, ptOrigin.y, 0xC0C0C0);
			}	
*/			CellRect.left+=16;
		}
		CString sText = GetItemText(itemnum, Col);
		if (sText.GetLength()==0) 
			continue;

		UINT nJustify=DT_LEFT;
		switch(lvc.fmt & LVCFMT_JUSTIFYMASK) 
		{
			case LVCFMT_CENTER: nJustify=DT_CENTER; break;
			case LVCFMT_RIGHT : nJustify=DT_RIGHT ; break;
		}
		pDC->DrawText(' '+sText+' ', -1, CellRect, nJustify | DT_SINGLELINE | DT_NOPREFIX | DT_NOCLIP | DT_VCENTER | DT_END_ELLIPSIS);
	}
	if((lvi.state & LVIS_FOCUSED) && HasFocus) pDC->DrawFocusRect(Rect); // Draw focus rectangle if item has focus
}
/***********************************************************************
c
c   FUNCTION: OnCustomDraw (NMHDR* pNMHDR, LRESULT* pResult)
c		Custom draw for List control
c
c   INPUT:  pNMHDR: list control data information
c
c   OUTPUT :   
c			pResult:  
c   RETURN:    None
c
**********************************************************************/
void CNCLTableList::OnCustomDraw(NMHDR* pNMHDR, LRESULT* pResult) 
{
	int item;
	LPNMLVCUSTOMDRAW lplvcd = (LPNMLVCUSTOMDRAW)pNMHDR;
	if (lplvcd->nmcd.dwDrawStage == CDDS_PREPAINT)
	{
	    *pResult = CDRF_NOTIFYITEMDRAW;
	}
    else if (lplvcd->nmcd.dwDrawStage == CDDS_ITEMPREPAINT)
	{
		item = lplvcd->nmcd.dwItemSpec;
		*pResult = CDRF_NOTIFYSUBITEMDRAW;
	}
    else if (lplvcd->nmcd.dwDrawStage == (CDDS_ITEMPREPAINT | CDDS_SUBITEM))
    {
		ASSERT(lplvcd->iSubItem >= 0);
		item = lplvcd->nmcd.dwItemSpec;
		lplvcd->clrText = ::GetSysColor(COLOR_WINDOWTEXT);
		lplvcd->clrTextBk = ::GetSysColor(COLOR_WINDOW);
		*pResult = CDRF_DODEFAULT;
	}
}
/*
......not used now
*/
void CNCLTableList::OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS FAR* lpncsp) 
{
	CListCtrl::OnNcCalcSize(bCalcValidRects, lpncsp);
}

int CNCLTableList::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	lpCreateStruct->style &= ~(LVS_EDITLABELS | LVS_LIST | LVS_NOSCROLL);
	lpCreateStruct->style |= (WS_CHILD | WS_VISIBLE | WS_HSCROLL | WS_VSCROLL | LVS_REPORT);
	if (CListCtrl::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	SetExtendedStyle(GetExtendedStyle());
	ASSERT(GetHeaderCtrl() != NULL);
	return 0;
}

void CNCLTableList::OnDestroy() 
{
	if(m_TargetDrop)
	{
		m_TargetDrop->Revoke();
		delete m_TargetDrop;
	}
	m_TargetDrop = NULL;
	CListCtrl::OnDestroy();	
}
/***********************************************************************
c
c   FUNCTION: InitDrag()
c		Init this control list as a drag and drop control. The h_Wnd MUST be valid now
c
c   INPUT:  none
c
c   OUTPUT :  none 
c   RETURN:    None
c
**********************************************************************/
void CNCLTableList::InitDrag()
{
	if (m_TargetDrop==NULL)
		m_TargetDrop = new CNCLMnDropTarget();
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
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
void CNCLTableList::OnLButtonDown(UINT nFlags, CPoint point) 
{
	LVHITTESTINFO hti;
	hti.pt = point;
	const int IDX = SubItemHitTest(&hti);

	if ((m_TargetDrop!=NULL)&&(IDX>=0))
	{
		m_StartPoint = 	point;
		if(m_TimerID)
		{
			KillTimer(1);
			m_TimerID = 0;
		}
		m_TimerID = SetTimer(1, 200, NULL);
	}
	CListCtrl::OnLButtonDown(nFlags, point); 
}

/***********************************************************************
c
c   FUNCTION: OnCtlClicked()
c		This function reset the starting point and timer for draging
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLTableList::OnCtlClicked()
{
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;	
	if(m_TimerID)
	{
		KillTimer(1);
		m_TimerID = 0;
	}
}
/***********************************************************************
c
c   FUNCTION: SetSelectPoint(CPoint point)
c		This function select the item under the point
c
c   INPUT:  point: mouse point
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLTableList::SetSelectPoint(CPoint point)
{
	CRect rcRect;
	int i;
	int item = -1;
	for (i=0; i<GetItemCount();i++)
	{
		ListView_GetSubItemRect(GetSafeHwnd(), i, 0, LVIR_BOUNDS, &rcRect);
		if (rcRect.PtInRect(point))
		{
			item = i;
			break;
		}
	}
	if (item!=-1)
	{
		((CNCLMenuDsnDlg*)m_parent)->UpdatedSel(item);
	}
}
/***********************************************************************
c
c   FUNCTION: OnMouseMove(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the mouse cursor moves on this control list
c			call the drag and drop here
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
void CNCLTableList::OnMouseMove(UINT nFlags, CPoint point) 
{
	if (m_TargetDrop!=NULL)
	{
		if(m_TimerID > 0)
		{
			int iX = m_StartPoint.x - point.x;
			int iY = m_StartPoint.y - point.y;
			if((iX*iX + iY*iY) > 100)
			{
/*
.....need check out which item is on at m_StartPoint and set the select
*/
				SetSelectPoint(m_StartPoint);
				m_StartPoint.x = -100;
				m_StartPoint.y = -100;	
				if(m_TimerID)
				{
					KillTimer(1);
					m_TimerID = 0;
				}
				COleDataSource*	pSource = new COleDataSource();
				if(pSource)
				{
					CSharedFile	sf(GMEM_MOVEABLE|GMEM_DDESHARE|GMEM_ZEROINIT);
					CString iText;
					int statflag;
					int status = uw_ntsave_cur_menudegn(&statflag);
					if (status==-1)
					{					
						CListCtrl::OnMouseMove(nFlags, point);
						delete pSource;
						CListCtrl::OnMouseMove(nFlags, point);
						return;
					}
					iText = "MenuDesign";

					if(iText.GetLength())
					{
						sf.Write(iText, iText.GetLength());
						HGLOBAL hMem = sf.Detach();
						if (!hMem) 
							return;
						pSource->CacheGlobalData(CF_TEXT, hMem);
						if (statflag)
							UW_drag_obj = 1;
						else
							UW_drag_obj = 0;
						pSource->DoDragDrop();
					}
					delete pSource;
					UW_drag_obj = -1;
				}
			}
		}
	}
	CListCtrl::OnMouseMove(nFlags, point);
}

/***********************************************************************
**
**   FUNCTION: OnTimer() 
**
**		this function will timely check if the mouse point is on the 
**		item, if not reset the time
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLTableList::OnTimer(UINT_PTR nIDEvent) 
{
	if(nIDEvent == 1)
	{
		POINT pt;
		::GetCursorPos(&pt);
		CRect iRect;
		GetWindowRect(iRect);
		if(!(iRect.PtInRect(pt)))
		{
			KillTimer(1);
			m_TimerID = 0;
		}
	}
	CListCtrl::OnTimer(nIDEvent);
}
/*
.....not really used
*/
BOOL CNCLTableList::OnEraseBkgnd(CDC* pDC) 
{
//	return FALSE;
		return TRUE;
}

//****************************************************************************************
#endif
