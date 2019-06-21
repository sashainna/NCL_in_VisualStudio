#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**		FILE NAME: wsntbmpbtn.cpp
**
**	 Description - Functions and implementations for
**		CNCLBmpButton class
**
**	 CONTAINS:
**		member function of CNCLBmpButton
**
**    COPYRIGHT 2011 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntbmpbtn.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:19
**********************************************************************
*/

#include "stdafx.h"
#include "wsntbmpbtn.h"
#include "wsntres.h"
  
#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNAMIC(CNCLBmpButton, CButton)

BEGIN_MESSAGE_MAP(CNCLBmpButton, CButton)
	//{{AFX_MSG_MAP(CNCLBmpButton)
		ON_WM_MOUSEMOVE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  CNCLBmpButton
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLBmpButton::CNCLBmpButton() 
{  
#if (_MFC_VER < 0x0250)
  hwndOwner = NULL; 
#endif 
	m_current_cursor = LoadCursor(NULL, IDC_ARROW);
} 


/***********************************************************************
c
c   SUBROUTINE:  ~CNCLBmpButton
c
c   FUNCTION:  Destructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLBmpButton::~CNCLBmpButton()
{
} 

/***********************************************************************
**
**   FUNCTION: DrawItem(LPDRAWITEMSTRUCT lpDIS)
**
**         Called by the framework when a visual aspect of 
**			an owner-drawn button changes
**
**   INPUT:  lpDIS:   A pointer to a 
**				DRAWITEMSTRUCT structure that contains 
**				information about the type of drawing required.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLBmpButton::DrawItem(LPDRAWITEMSTRUCT lpDIS)
{
	int flag;
	CDC* pDC = CDC::FromHandle(lpDIS->hDC);

	UINT state = lpDIS->itemState; 
	CRect focusRect, btnRect;
	focusRect.CopyRect(&lpDIS->rcItem); 
	btnRect.CopyRect(&lpDIS->rcItem); 

	focusRect.left += 4;
    focusRect.right -= 4;
    focusRect.top += 4;
    focusRect.bottom -= 4;
      
    const int bufSize = 512;
    TCHAR buffer[bufSize];
    GetWindowText(buffer, bufSize);
	
    DrawFilledRect(pDC, btnRect, pDC->GetBkColor()); 
    DrawFrame(pDC, btnRect,2);
/*
......Now, depending upon the state, redraw the button (down image) if it is selected,
......place a focus rectangle on it, or redisplay the caption if it is disabled
*/
	if (state & ODS_DISABLED) flag = 0;
	else flag = 1;
	if (state & ODS_FOCUS) 
	{
		DrawFocusRect(lpDIS->hDC, (LPRECT)&focusRect);
		if (state & ODS_SELECTED)
		{ 
    		DrawFilledRect(pDC, btnRect, pDC->GetBkColor()); 
    		DrawFrame(pDC, btnRect, -1);
			DrawFocusRect(lpDIS->hDC, (LPRECT)&focusRect);
		}
	}
//	TransparentBlt(lpDIS->hDC, 0, 0, 16, 
//			16, m_hBitmap, 0, 0, 0xC0C0C0);
/*
......maybe just draw a arrow
*/
	if (m_type==0)
		DrawDownArrow(pDC, btnRect.Width(), btnRect.Height(), flag);
	else
		DrawUpArrow(pDC, btnRect.Width(), btnRect.Height(), flag);
} 


/***********************************************************************
**
**   FUNCTION: DrawFrame(CDC *DC, CRect R, int Inset)
**
**         Draw the button frame
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLBmpButton::DrawFrame(CDC *DC, CRect R, int Inset)
{ 
	COLORREF dark, light, tlColor, brColor;
	int i, m, width;
	width = (Inset < 0)? -Inset : Inset;
	
	for (i = 0; i < width; i += 1) {
		m = 255 / (i + 2);
		dark = PALETTERGB(m, m, m);
		m = 192 + (63 / (i + 1));
		light = PALETTERGB(m, m, m);
		  
	  	if ( width == 1 ) {
			light = RGB(255, 255, 255);
			dark = RGB(128, 128, 128);
		}
		
		if ( Inset < 0 ) {
			tlColor = RGB(0, 0, 128);;
			brColor = light;
		}
		else {
			tlColor = light;
			brColor = dark;
		}
		
		DrawLine(DC, R.left, R.top, R.right, R.top, tlColor);							// Across top
		DrawLine(DC, R.left, R.top, R.left, R.bottom, tlColor);							// Down left
	  
		if ( (Inset < 0) && (i == width - 1) && (width > 1) ) {
			DrawLine(DC, R.left + 1, R.bottom - 1, R.right, R.bottom - 1, RGB(1, 1, 1));// Across bottom
			DrawLine(DC, R.right - 1, R.top + 1, R.right - 1, R.bottom, RGB(1, 1, 1));	// Down right
		}
	  	else {
			DrawLine(DC, R.left + 1, R.bottom - 1, R.right, R.bottom - 1, brColor);		// Across bottom
			DrawLine(DC, R.right - 1, R.top + 1, R.right - 1, R.bottom, brColor);		// Down right
		}
	  	InflateRect(R, -1, -1);
	}
}
/***********************************************************************
**
**   FUNCTION: OnMouseMove(UINT nFlags, CPoint pt)
**
**       mouse move callback
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLBmpButton::OnMouseMove(UINT nFlags, CPoint point) 
{	
	CButton::OnMouseMove(nFlags, point);
	CRect rect;
	GetWindowRect(&rect);
	ScreenToClient(&rect);
	DrawHighLight(rect);
	::SetCursor(m_current_cursor);
}

/***********************************************************************
**
**   FUNCTION: DrawHighLight(CRect rect)
**
**       Draw highlight button
**
**   INPUT:  rect: area to draw
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLBmpButton::DrawHighLight(CRect rect)
{
	if (IsWindowEnabled()==0)
		return;
	CClientDC dc(this);

	CRect focusRect, btnRect;
	focusRect.left = focusRect.top = 0;
	focusRect.right = rect.Width();
	focusRect.bottom = rect.Height();
	btnRect.CopyRect(&focusRect); 

	focusRect.left += 4;
    focusRect.right -= 4;
    focusRect.top += 4;
    focusRect.bottom -= 4;
      
    const int bufSize = 512;
    TCHAR buffer[bufSize];
    GetWindowText(buffer, bufSize);
	
    DrawFilledRect(&dc, btnRect, dc.GetBkColor()); 
    DrawFrame(&dc, btnRect,-1);
/*
......maybe just draw a arrow
*/
	if (m_type==0)
		DrawDownArrow(&dc, btnRect.Width(), btnRect.Height(), 1);
	else
		DrawUpArrow(&dc, btnRect.Width(), btnRect.Height(), 1);
}

/***********************************************************************
**
**   FUNCTION: DrawNormal(CRect rect)
**
**       Draw normal button
**
**   INPUT:  rect: area to draw
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLBmpButton::DrawNormal(CRect rect)
{
	if (IsWindowEnabled()==0)
		return;
	CClientDC dc(this);

	CRect focusRect, btnRect;
	focusRect.left = focusRect.top = 0;
	focusRect.right = rect.Width();
	focusRect.bottom = rect.Height();
	btnRect.CopyRect(&focusRect); 

	focusRect.left += 4;
    focusRect.right -= 4;
    focusRect.top += 4;
    focusRect.bottom -= 4;
      
    const int bufSize = 512;
    TCHAR buffer[bufSize];
    GetWindowText(buffer, bufSize);
	
    DrawFilledRect(&dc, btnRect, dc.GetBkColor()); 
    DrawFrame(&dc, btnRect,2);
/*
......maybe just draw a arrow
*/
	if (m_type==0)
		DrawDownArrow(&dc, btnRect.Width(), btnRect.Height(), 1);
	else
		DrawUpArrow(&dc, btnRect.Width(), btnRect.Height(), 1);
}

/***********************************************************************
**
**   FUNCTION: :DrawFilledRect(CDC *DC, CRect R, COLORREF color)
**
**         Draw the button filledrect
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLBmpButton::DrawFilledRect(CDC *DC, CRect R, COLORREF color)
{ 
	CBrush B;
	B.CreateSolidBrush(color);
	DC->FillRect(R, &B);
}
 

/***********************************************************************
**
**   FUNCTION: :DrawLine(CDC *DC, CRect EndPoints, COLORREF color)
**
**         Draw the line in the button
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLBmpButton::DrawLine(CDC *DC, CRect EndPoints, COLORREF color)
{ 
	CPen newPen;
	newPen.CreatePen(PS_SOLID, 1, color);
	CPen *oldPen = DC->SelectObject(&newPen);
	DC->MoveTo(EndPoints.left, EndPoints.top);
	DC->LineTo(EndPoints.right, EndPoints.bottom);
	DC->SelectObject(oldPen);
    newPen.DeleteObject();
}

/***********************************************************************
**
**   FUNCTION: :DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color)
**
**         Draw the line in the button
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLBmpButton::DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color)
{ 
	CPen newPen;
	newPen.CreatePen(PS_SOLID, 1, color);
	CPen *oldPen = DC->SelectObject(&newPen);
	DC->MoveTo(left, top);
	DC->LineTo(right, bottom);
	DC->SelectObject(oldPen);
    newPen.DeleteObject();
}

/***********************************************************************
**
**   FUNCTION: :DrawDownArrow(CDC *DC, int left, int top, int flag)
**
**         Draw the line in the button
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLBmpButton::DrawDownArrow(CDC *DC, int wid, int hgt, int flag)
{ 
	CPen newPen;
	newPen.CreatePen(PS_SOLID, 1, RGB(0, 0, 0));
	CPen *oldPen = DC->SelectObject(&newPen);
	CBrush brush1(RGB(0, 0, 0));
	CBrush brush2(RGB(125, 125, 125));
	CBrush* pOldBrush;
	if (flag)
		pOldBrush = DC->SelectObject(&brush1);
	else
		pOldBrush = DC->SelectObject(&brush2);

	int left = (int)(wid/4.0);
	int top = (int)((hgt*4.0)/9.0);
	int bwid = (int)(wid/2.0);
	int bhgt = (int)(hgt/9.0);

	CPoint pts[3];
	pts[0].x = left;
	pts[0].y = top;

	pts[1].x = left + bwid;
	pts[1].y = top;

	pts[2].x = (long)(left + bwid/2.0);
	pts[2].y = top + bhgt;

	DC->Polygon(pts, 3);

	DC->SelectObject(oldPen);
	DC->SelectObject(pOldBrush);
    newPen.DeleteObject();
}
	
/***********************************************************************
**
**   FUNCTION: :DrawUpArrow(CDC *DC, int left, int top, int flag)
**
**         Draw the line in the button
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLBmpButton::DrawUpArrow(CDC *DC, int wid, int hgt, int flag)
{ 
	CPen newPen;
	newPen.CreatePen(PS_SOLID, 1, RGB(0, 0, 0));
	CPen *oldPen = DC->SelectObject(&newPen);
	CBrush brush1(RGB(0, 0, 0));
	CBrush brush2(RGB(125, 125, 125));
	CBrush* pOldBrush;
	if (flag)
		pOldBrush = DC->SelectObject(&brush1);
	else
		pOldBrush = DC->SelectObject(&brush2);

	int left = (int)(wid/4.0);
	int top = (int)((hgt*4.0)/9.0);
	int bwid = (int)(wid/2.0);
	int bhgt = (int)(hgt/9.0);

	CPoint pts[3];
	pts[0].x = (long)(left + bwid/2.0);
	pts[0].y = top;

	pts[1].x = left;
	pts[1].y = top + bhgt;

	pts[2].x = left + bwid;
	pts[2].y = top + bhgt;

	DC->Polygon(pts, 3);

	DC->SelectObject(oldPen);
	DC->SelectObject(pOldBrush);
    newPen.DeleteObject();
}


#endif






