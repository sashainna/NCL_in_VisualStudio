/************************************************************************
**
**   FILE NAME: wsntIconListBox.cpp
**
**	 Description - Functions and implementations for
**		CIconListBox class
**
**	 CONTAINS: 
**		class functions of CIconListBox class
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntIconListBox.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 17:41:37
***********************************************************************
*/
#include "wsntstdafx.h"
#include <afxole.h>         // MFC OLE classes
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxdisp.h >       // MFC OLE automation classes
#include <afxpriv.h>

#include "wsntdropsource.h"
#include "wsntDDform.h"
#include "wsntIconListBox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern char UW_formdlg_font[];
extern HBITMAP WINAPI  CopyWindowToBitmap(LPRECT lpRect, CWnd *pWnd);
extern int UW_formadd_item;

IMPLEMENT_DYNAMIC(CIconListBox, CListBox)

/***********************************************************************
**   FUNCTION: CNCLDDButton
**		Constructor of class CNCLDDButton
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CIconListBox::CIconListBox()
	:	m_TargetDrop(NULL),
		m_TimerID(0)
{
	m_clrSel  = RGB(255,225,170);
	m_clrBg   = RGB(255,255,255);
	m_clrText = RGB(0,0,0);
	m_bEdge   = TRUE;

	m_StartPoint = CPoint(-1, -1);
	m_TargetDrop = new CNCLMnDropTarget2();
	m_DragImage = NULL;
	m_dragimg_create = 0;
	m_flag = 0;
	m_type = 0;
	m_itemno = -1;
	m_parent = NULL;
	m_DragAllImage = NULL;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLDDButton
**              Destructor of class CNCLDDButton, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CIconListBox::~CIconListBox()
{
	while( m_pItemList.GetCount() )
	{
		CExtItem* pItem = m_pItemList.GetHead();
		delete pItem;
		m_pItemList.RemoveHead();
	}
	if (m_DragImage!=NULL)
		delete m_DragImage;
	if (m_DragAllImage!=NULL)
		delete m_DragAllImage;
}


BEGIN_MESSAGE_MAP(CIconListBox, CListBox)
	//{{AFX_MSG_MAP(CIconListBox)
	ON_CONTROL_REFLECT(LBN_SELCHANGE, OnSelchange)
	ON_WM_LBUTTONUP()

	ON_WM_LBUTTONDOWN()
	ON_WM_RBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_WM_TIMER()
	ON_WM_DESTROY()
	ON_WM_SIZE()
	ON_WM_CREATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
**
**   FUNCTION: AddItem(LPCSTR lpszItemName, HICON hIcon)
**            Add an item into the listbox structure.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CIconListBox::AddItem(LPCSTR lpszItemName, HICON hIcon)
{
	CExtItem* pItem = new CExtItem(lpszItemName,hIcon);
	m_pItemList.AddTail(pItem);

	int nIndex = AddString("");
	SetItemDataPtr(nIndex,pItem);
}
void CIconListBox::AddItem(LPCSTR lpszItemName, HBITMAP hBitmap)
{
	CExtItem* pItem = new CExtItem(lpszItemName, hBitmap);
	m_pItemList.AddTail(pItem);

	int nIndex = AddString("");
	SetItemDataPtr(nIndex,pItem);
}

void CIconListBox::SetSelColor(COLORREF clr)
{
	m_clrSel = clr;
}
void CIconListBox::SetBgColor(COLORREF clr)
{
	m_clrBg = clr;
}
void CIconListBox::SetTextColor(COLORREF clr)
{
	m_clrText = clr;
}
void CIconListBox::EnableEdge(BOOL bEnable)
{
	m_bEdge = bEnable;
}
void CIconListBox::SetCurSel(int curSel)
{
	m_curSel = curSel;
	CListBox::SetCurSel(curSel);
	OnSelchange();
}
int CIconListBox::GetCurSel()
{	
	return m_curSel;
}
void CIconListBox::MeasureItem(LPMEASUREITEMSTRUCT lpMIS) 
{
	lpMIS->itemWidth   = 50;
	lpMIS->itemHeight  = 32;
}

/***********************************************************************
**
**   FUNCTION: DrawItem(LPDRAWITEMSTRUCT lpDIS)
**
**         Called when a visual aspect of 
**			an owner-drawn listbox changes
**
**   INPUT:  lpDIS:   A pointer to a 
**				DRAWITEMSTRUCT structure that contains 
**				information about the type of drawing required.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CIconListBox::DrawItem(LPDRAWITEMSTRUCT lpDIS) 
{
	CDC* pDC = CDC::FromHandle(lpDIS->hDC);
	HDC hMemDC = CreateCompatibleDC(lpDIS->hDC);
	HBITMAP hOldBitmap;

	CRect rectFull(lpDIS->rcItem);
	CRect rectIcon(rectFull.left,rectFull.top,rectFull.left+32,rectFull.top+32); 
	CRect rectText(rectIcon.right,rectFull.top,rectFull.right,rectFull.bottom);	
	
	
	CExtItem* pItem = static_cast<CExtItem *>(GetItemDataPtr(lpDIS->itemID));
	pDC->SetBkMode(TRANSPARENT);

	if ( (lpDIS->itemAction & ODA_DRAWENTIRE) ||
	     ((!(lpDIS->itemState & ODS_SELECTED) && (lpDIS->itemAction & ODA_SELECT)))
	   )
	{
		CRect rect(rectFull);

		pDC->FillSolidRect(rect,m_clrBg);
		if( m_bEdge )
			pDC->DrawEdge(rect,EDGE_SUNKEN,BF_BOTTOM);
/*
......use bitmap have more choice for picture
*/
		hOldBitmap = (HBITMAP)SelectObject(hMemDC, pItem->m_hBitmap);
		BitBlt(lpDIS->hDC, rectIcon.left+7,rectIcon.top+7,16, 16, hMemDC, 0, 0, SRCCOPY);	
		pDC->SetTextColor(RGB(0,0,0));			

		rect.CopyRect(rectText);
		rect.DeflateRect(2,2);
		rect.OffsetRect(2,6);
		if( pItem->m_szItemName != NULL )
			pDC->DrawText(pItem->m_szItemName,lstrlen(pItem->m_szItemName),
						  rect,DT_LEFT | DT_SINGLELINE);
	}

	
	if ((lpDIS->itemState & ODS_SELECTED) &&
		(lpDIS->itemAction & (ODA_SELECT | ODA_DRAWENTIRE)))
	{	
		CRect rect(rectFull);

		CPen  Pen(PS_SOLID,1,RGB(0,0,0));
		CPen* pOldPen = pDC->SelectObject(&Pen);
		pDC->Rectangle(rect);
		pDC->SelectObject(pOldPen);
				
		rect.DeflateRect(1,1,1,1);
		pDC->FillRect(rect,&CBrush(m_clrSel));
			
		if( m_bEdge )
			pDC->DrawEdge(rect,EDGE_SUNKEN,BF_BOTTOM);

		hOldBitmap = (HBITMAP)SelectObject(hMemDC, pItem->m_hBitmap);
		BitBlt(lpDIS->hDC, rectIcon.left+7,rectIcon.top+7,16, 16, hMemDC, 0, 0, SRCCOPY);	
		pDC->SetTextColor(m_clrText);			
		rect.CopyRect(rectText);
		rect.DeflateRect(2,2);
		rect.OffsetRect(2,6);					
		if( pItem->m_szItemName != NULL )
			pDC->DrawText(pItem->m_szItemName,lstrlen(pItem->m_szItemName),
						  rect,DT_LEFT | DT_SINGLELINE);
	}
	HBITMAP hBitmap = (HBITMAP)SelectObject(hMemDC, hOldBitmap);
	DeleteDC(hMemDC);
}
void CIconListBox::PreSubclassWindow() 
{
	m_curSel = -1;
}
/***********************************************************************
**
**   FUNCTION: OnSelchange
**
**         Called when list selection changed
**			
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CIconListBox::OnSelchange() 
{
	static int oldSel = m_curSel;

	if( oldSel != m_curSel )
	{	
		CRect rect;					
		GetItemRect(oldSel,rect);
		InvalidateRect(rect,TRUE);

		GetItemRect(m_curSel,rect);
		InvalidateRect(rect,TRUE);
		oldSel = m_curSel;
	}
}


/***********************************************************************
**
**   FUNCTION: OnCreate(LPCREATESTRUCT lpCreateStruct) 
**
**		Override this member function to perform any needed 
**		initialization of the class.
**   
**		INPUT:  LPCREATESTRUCT lpCreateStruct: contains copies of 
**						the parameters used to create the window.
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CIconListBox::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CListBox::OnCreate(lpCreateStruct) == -1)
		return -1;
	m_fieldFont.CreatePointFont(80, UW_formdlg_font);
	SetFont(&m_fieldFont);
}

/***********************************************************************
**
**   FUNCTION: OnDestroy() 
**
**		OnDestroy is called after the this object 
**		is removed from the screen. Free the memory
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CIconListBox::OnDestroy() 
{
	if(m_TargetDrop)
	{
		m_TargetDrop->Revoke();
		delete m_TargetDrop;
	}
	m_TargetDrop = NULL;
	CListBox::OnDestroy();	
}

/***********************************************************************
c
c   FUNCTION: InitDrag()
c		Init this control as a drag and drop control. The h_Wnd MUST be valid now
c
c   INPUT:  none
c
c   OUTPUT :  none 
c   RETURN:    None
c
**********************************************************************/
void CIconListBox::InitDrag()
{
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
}

/***********************************************************************
c
c   FUNCTION: OnLButtonDown(UINT nFlags, CPoint point) 
c
c			Callback function for left mouse button down
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
void CIconListBox::OnLButtonDown(UINT nFlags, CPoint point) 
{
	m_StartPoint = 	point;
	m_TimerID = SetTimer(1, 100, NULL);	
	CListBox::OnLButtonDown(nFlags, point);
}

/***********************************************************************
c
c   FUNCTION: OnLButtonUp(UINT nFlags, CPoint point)
c			Callback function for left mouse button up
c
c   INPUT:  nFlags: Indicates whether various virtual keys are down.
c					MK_CONTROL   Set if the CTRL key is down.
c					MK_SHIFT   Set if the SHIFT key is down.
c			point:  Specifies the x- and y-coordinate of the cursor.
c
c   OUTPUT :   None.
c   RETURN:    None.
c
**********************************************************************/
void CIconListBox::OnLButtonUp(UINT nFlags, CPoint point) 
{
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;
	if(m_TimerID)
	{
		KillTimer(m_TimerID);
		m_TimerID = 0;
	}	
	BOOL data;
	m_curSel = ItemFromPoint(point,data);	
	OnSelchange();
	CListBox::OnLButtonUp(nFlags, point);
}
/***********************************************************************
c
c   FUNCTION: OnRButtonUp(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user release the right mouse button.
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
void CIconListBox::OnRButtonUp(UINT nFlags, CPoint point) 
{
	CListBox::OnRButtonUp(nFlags, point);
}
void CIconListBox::OnSize(UINT nType, int cx, int cy) 
{
	CListBox::OnSize(nType, cx, cy);
}
void CIconListBox::GetText(int nIdex, CString &rString) const
{
	CExtItem* pItem = static_cast<CExtItem *>(GetItemDataPtr(nIdex));
	rString = pItem->m_szItemName;
}
int CIconListBox::GetText(int nIdex, LPTSTR lpszBuffer) const
{
	CExtItem* pItem = static_cast<CExtItem *>(GetItemDataPtr(nIdex));
	strcpy(lpszBuffer, pItem->m_szItemName);
	return strlen(pItem->m_szItemName);
}

/***********************************************************************
c
c   FUNCTION: OnMouseMove(UINT nFlags, CPoint point) 
c
c       Tthis member function will be called
c			when the mouse cursor moves on this control
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
void CIconListBox::OnMouseMove(UINT nFlags, CPoint point) 
{
	if (!(nFlags&MK_LBUTTON))
	{
/*
.....if not left button down, reset timer for drag
*/
		if(m_TimerID > 0)
		{
			KillTimer(m_TimerID);
			m_TimerID = 0;
		}
	}
	if(m_TimerID > 0)
	{
		int iX = m_StartPoint.x - point.x;
		int iY = m_StartPoint.y - point.y;
		if((iX*iX + iY*iY) > 9)
		{
			CNCLDropSource* pdropsource = new CNCLDropSource();
			COleDataSource*	pSource = new COleDataSource();
			if(pSource)
			{
				pdropsource->SetImageList(m_DragImage);
				pdropsource->SetDragPt(point);
				CSharedFile	sf(GMEM_MOVEABLE|GMEM_DDESHARE|GMEM_ZEROINIT);
				CString iText;
				char tempstr[80];
				CString itext2;
				int sel = GetCurSel();
				BOOL data;
				sel = ItemFromPoint(point,data);
				GetText(sel, iText);
				if (m_type==0)
				{
					iText = "New " + iText;
				}
				else
				{
					ClientToScreen(&point);
					sprintf(tempstr, "%d %d %d ", m_itemno, point.x, point.y);
					itext2 = tempstr;
					iText = "Move CNCLDDListBox " + itext2 + iText;
				}
				if(iText.GetLength())
				{
					sf.Write(iText, iText.GetLength());
					HGLOBAL hMem = sf.Detach();
					if (!hMem) 
						return;
					UW_formadd_item = 1;
					pSource->CacheGlobalData(CF_TEXT, hMem);
					pSource->DoDragDrop(DROPEFFECT_MOVE|DROPEFFECT_COPY, NULL, pdropsource);
				}
				delete pdropsource;
				delete pSource;
				UW_formadd_item = 0;
			}
		}
	}
	
	CListBox::OnMouseMove(nFlags, point);
	ClientToScreen(&point);
	if (m_parent!=NULL)
	{
		((CNCLDDform*)m_parent)->ScreenToClient(&point);
		((CNCLDDform*)m_parent)->HandleMouseMove(nFlags, point);
	}
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
void CIconListBox::OnTimer(UINT_PTR nIDEvent) 
{	
/*	
......check if mouse is still in rect
*/
	if(nIDEvent == 1)
	{
		POINT pt;
		::GetCursorPos(&pt);
		CRect iRect;
		GetWindowRect(iRect);
		if(!(iRect.PtInRect(pt)))
		{
			KillTimer(nIDEvent);
			m_TimerID = 0;
		}
	}
	CListBox::OnTimer(nIDEvent);
}
void CIconListBox::OnDragDropCallback(CPoint pt, char *input_text)
{
	return;
}

/***********************************************************************
c
c   FUNCTION: VKeyToItem(UINT nKey, UINT nIndex) 
c
c       callback function when we push Vkey on the list
c   INPUT:  nKey: key ID
c			nIndex: listbox item index
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CIconListBox::VKeyToItem(UINT nKey, UINT nIndex) 
{
	if ((nKey == VK_UP) && (nIndex > 0))
		SetCurSel(nIndex);
	
   else if ((nKey == VK_DOWN) && (nIndex < (UINT)GetCount()))
		SetCurSel(nIndex);
	return -1;
}

