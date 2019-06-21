#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**		FILE NAME: wsntclrbtn.cpp
**
**	 Description - Functions and implementations for
**		CNCLColorButton class (NCL color button)
**
**	 CONTAINS:
**		member function of CNCLColorButton
**
**    COPYRIGHT 2006 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntclrbtn.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:20
**********************************************************************
*/


#include "stdafx.h"
#include "wsntclrbtn.h"
  
#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNAMIC(CNCLColorButton, CButton)

/***********************************************************************
c
c   SUBROUTINE:  CNCLColorButton
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLColorButton::CNCLColorButton() 
{  
#if (_MFC_VER < 0x0250)
  hwndOwner = NULL; 
#endif 
} 


/***********************************************************************
c
c   SUBROUTINE:  ~CNCLColorButton
c
c   FUNCTION:  Destructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLColorButton::~CNCLColorButton()
{
} 

/***********************************************************************
c
c   SUBROUTINE:  set_color
c
c   FUNCTION:  Set button's color
c
c   INPUT:  bcolor: background color
c			fcolor: foreground color
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLColorButton::set_color(const COLORREF bcolor, const COLORREF fcolor)
{
	m_fg = fcolor;
	m_bg = bcolor; 
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
void CNCLColorButton::DrawItem(LPDRAWITEMSTRUCT lpDIS)
{
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
	
    DrawFilledRect(pDC, btnRect, GetBGColor()); 
    DrawFrame(pDC, btnRect,2);
  	DrawButtonText(pDC, btnRect, buffer, GetFGColor());
/*
......Now, depending upon the state, redraw the button (down image) if it is selected,
......place a focus rectangle on it, or redisplay the caption if it is disabled
*/
	if (state & ODS_FOCUS) {
		DrawFocusRect(lpDIS->hDC, (LPRECT)&focusRect);
		if (state & ODS_SELECTED){ 
    		DrawFilledRect(pDC, btnRect, GetBGColor()); 
    		DrawFrame(pDC, btnRect, -1);
  			DrawButtonText(pDC, btnRect, buffer, GetFGColor());
			DrawFocusRect(lpDIS->hDC, (LPRECT)&focusRect);
		}
	}
	else if (state & ODS_DISABLED) {
  		DrawButtonText(pDC, btnRect, buffer, RGB(125,125,125));
    }
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
void CNCLColorButton::DrawFrame(CDC *DC, CRect R, int Inset)
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
			tlColor = dark;
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
void CNCLColorButton::DrawFilledRect(CDC *DC, CRect R, COLORREF color)
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
void CNCLColorButton::DrawLine(CDC *DC, CRect EndPoints, COLORREF color)
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
void CNCLColorButton::DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color)
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
**   FUNCTION: :DrawButtonText(CDC *DC, CRect R, const char *Buf, COLORREF TextColor)
**
**         Draw the button text in the button
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLColorButton::DrawButtonText(CDC *DC, CRect R, const char *Buf, COLORREF TextColor)
{
    COLORREF prevColor = DC->SetTextColor(TextColor);
    DC->SetBkMode(TRANSPARENT);
	DC->DrawText(Buf, strlen(Buf), R, DT_CENTER|DT_VCENTER|DT_SINGLELINE);
	DC->SetTextColor(prevColor);
}


		
#endif






