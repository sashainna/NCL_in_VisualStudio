/************************************************************************
**		FILE NAME: wsntNCLbutton.cpp
**
**	 Description - Functions and implementations for
**		CNCLButton class (NCL button)
**		
**	 CONTAINS:
**		member function of CNCLButton
**
**    COPYRIGHT 2015 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntNCLButton.cpp , 26.3
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:39:39
**********************************************************************
*/
#include "wsntstdafx.h"
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxdisp.h >       // MFC OLE automation classes
#include <afxpriv.h>

#include "wsntNCLButton.h"
#include "wsntbitmap.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern char UW_formdlg_font[];
extern "C" int UW_form_fontsize;

extern void TransparentBlt( HDC hdcDest, int nXDest, int nYDest, int nWidth, 
			int nHeight, HBITMAP hBitmap, int nXSrc, int nYSrc,
			COLORREF colorTransparent);

/***********************************************************************
**
**   FUNCTION: Transparent_StretchBlt( HDC hdcDest, int nXDest, int nYDest, int nWidth, 
**				int nHeight, HBITMAP hBitmap, int nXSrc, int nYSrc,
**				COLORREF colorTransparent)
**
**             drawing bitmap transparently
**
**   INPUT:  hdcDest		- Handle to destination device context 
**			nXDest		- x-coordinate of destination rectangle's upper-left corner 
**			nYDest		- y-coordinate of destination rectangle's upper-left corner 
**			nWidth		- Width of destination rectangle 
**			nHeight		- height of destination rectangle 
**			hBitmap		- Handle of the source bitmap
**			nXSrc		- x-coordinate of source rectangle's upper-left corner 
**			nYSrc		- y-coordinate of source rectangle's upper-left corner 
**			colorTransparent	- The transparent color
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void Transparent_StretchBlt( HDC hdcDest, int nXDest, int nYDest, int nWidth, 
			int nHeight, HBITMAP hBitmap, int nXSrc, int nYSrc, int nCxSrc, int nCySrc,
			COLORREF colorTransparent)
{
	CDC dc, memDC, maskDC, tempDC;
	dc.Attach( hdcDest );
	maskDC.CreateCompatibleDC(&dc);
	CBitmap maskBitmap;
	
	CBitmap* pOldMemBmp = NULL;
	CBitmap* pOldMaskBmp = NULL;
	HBITMAP hOldTempBmp = NULL;
	
	memDC.CreateCompatibleDC(&dc);
	tempDC.CreateCompatibleDC(&dc);
	CBitmap bmpImage;
	bmpImage.CreateCompatibleBitmap( &dc, nWidth, nHeight );
	pOldMemBmp = memDC.SelectObject( &bmpImage );
	
	hOldTempBmp = (HBITMAP) ::SelectObject( tempDC.m_hDC, hBitmap );
	
	SetStretchBltMode(memDC, HALFTONE);
	int status = memDC.StretchBlt(0, 0, nWidth, nHeight,  &tempDC, nXSrc, nYSrc, nCxSrc, nCySrc, SRCCOPY );	
/*
......Create monochrome bitmap for the mask
*/
	maskBitmap.CreateBitmap( nWidth, nHeight, 1, 1, NULL );
	pOldMaskBmp = maskDC.SelectObject( &maskBitmap );
	memDC.SetBkColor( colorTransparent );
/*
......Create the mask from the memory DC
*/
	maskDC.BitBlt( 0, 0, nWidth, nHeight, &memDC, 
		0, 0, SRCCOPY );
/* 
.....Set the background in memDC to black. Using SRCPAINT with black 
.....and any other color results in the other color, thus making 
.....black the transparent color
*/
	memDC.SetBkColor(RGB(0,0,0));
	memDC.SetTextColor(RGB(255,255,255));
	memDC.BitBlt(0, 0, nWidth, nHeight, &maskDC, 0, 0, SRCAND);
/* 
.....Set the foreground to black. See comment above.
*/
	dc.SetBkColor(RGB(255,255,255));
	dc.SetTextColor(RGB(0,0,0));
	dc.BitBlt(nXDest, nYDest, nWidth, nHeight, &maskDC, 0, 0, SRCAND);
/*
.....Combine the foreground with the background
*/
	dc.BitBlt(nXDest, nYDest, nWidth, nHeight, &memDC, 
		0, 0, SRCPAINT);
	if (hOldTempBmp)
		::SelectObject( tempDC.m_hDC, hOldTempBmp);
	if (pOldMaskBmp)
		maskDC.SelectObject( pOldMaskBmp );
	if (pOldMemBmp)
		memDC.SelectObject( pOldMemBmp );
	
	dc.Detach();
}


IMPLEMENT_DYNAMIC(CNCLButton, CMFCButton)

/***********************************************************************
**   FUNCTION: CNCLButton
**		Constructor of class CNCLButton
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLButton::CNCLButton(int type)
{
	m_type = type;
	m_parent = NULL;
	m_fg = RGB(0,0,0);
	m_bg = GetSysColor(COLOR_BTNFACE); 
	m_active = 1;
	m_bmpfile[0] = '\0';
}

/***********************************************************************
**
**   FUNCTION: ~CNCLButton
**              Destructor of class CNCLButton, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLButton::~CNCLButton()
{
}

void CNCLButton::SetBitMapFile(char *filename)
{
	if (filename!=NULL)
	{
		strncpy(m_bmpfile, filename,255);
		m_bmpfile[255] = '\0';
	}
}

BEGIN_MESSAGE_MAP(CNCLButton, CMFCButton)
	//{{AFX_MSG_MAP(CNCLButton)
	ON_WM_DESTROY()
	ON_WM_CREATE()
	ON_WM_ERASEBKGND()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

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
int CNCLButton::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CMFCButton::OnCreate(lpCreateStruct) == -1)
		return -1;
	m_fieldFont.CreatePointFont(UW_form_fontsize*10, UW_formdlg_font);
	SetFont(&m_fieldFont);	
	m_active = 1;
}

void CNCLButton::set_color(const COLORREF bcolor, const COLORREF fcolor)
{
	if (m_fg != fcolor)
	{
		m_fg = fcolor;
		SetTextColor(fcolor);
	}
	if (m_bg != bcolor)
	{
		m_bg = bcolor; 
		if (m_type==1)
		{
			SetFaceColor(bcolor);
		}
	}
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
void CNCLButton::DrawItemOldWay(LPDRAWITEMSTRUCT lpDIS)
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
	if ((state & ODS_FOCUS) && (m_active!=-1))
	{
		DrawFocusRect(lpDIS->hDC, (LPRECT)&focusRect);
		if (state & ODS_SELECTED){ 
    		DrawFilledRect(pDC, btnRect, GetBGColor()); 
    		DrawFrame(pDC, btnRect, -1);
  			DrawButtonText(pDC, btnRect, buffer, GetFGColor());
			DrawFocusRect(lpDIS->hDC, (LPRECT)&focusRect);
		}
	}
	else if ((state & ODS_DISABLED) || (m_active==-1))
	{
  		DrawButtonText(pDC, btnRect, buffer, RGB(125,125,125));
    }
} 

void CNCLButton::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct) 
{
	BOOL stat;
	COLORREF def = GetSysColor(COLOR_BTNFACE);
	if ((m_type==1)&&(GetBGColor()!=def))
	{
		DrawItemOldWay(lpDrawItemStruct);
		return;
	}
	CDC* pDC = CDC::FromHandle(lpDrawItemStruct->hDC);
	UINT state = lpDrawItemStruct->itemState; 
	CRect focusRect, btnRect, btnRect2;
	focusRect.CopyRect(&lpDrawItemStruct->rcItem); 
	btnRect.CopyRect(&lpDrawItemStruct->rcItem); 
	pDC->SelectObject(m_fieldFont );

    const int bufSize = 512;
    TCHAR buffer[bufSize];
    GetWindowText(buffer, bufSize);

	DWORD dwStyle = GetStyle();
	if (m_type!=7)
	{
		stat = CMFCVisualManager::GetInstance()->DrawPushButton(pDC, btnRect, this, state);
		if (stat==FALSE)
		{
			DrawItemOldWay(lpDrawItemStruct);
			return;
		}
	  	DrawButtonText(pDC, btnRect, buffer, GetFGColor());
		HBITMAP hBmap = NULL;
		if ((m_type==25)||((m_type==-1)&&(m_bmpfile[0]!='\0')))
		{
			if (m_bmpfile[0]!='\0')
				hBmap = uw_get_bitmap(m_bmpfile, GetDC()->m_hDC, -3);
			if (hBmap!=NULL)
				DrawButtonIcon(pDC, btnRect, hBmap);
			else if (m_type==25)
			{
				hBmap = LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP13));
				if (hBmap!=NULL)
					DrawButtonIcon(pDC, btnRect, hBmap);	
			}
		}
		if ((state & ODS_DISABLED) || (m_active==-1))
		{
  			DrawButtonText(pDC, btnRect, buffer, RGB(125,125,125));
		}
	}
	else if (m_type==7)
	{
		CBrush pFillBrush;
		pFillBrush.CreateSolidBrush(m_bg);
		CBrush *old = pDC->SelectObject(&pFillBrush);
		pDC->FillRect(btnRect, &pFillBrush);
		pDC->SelectObject(old);

		BOOL select = state & ODS_SELECTED;
		BOOL hlt = false;
		BOOL enable = !((state & ODS_DISABLED) || (m_active==-1));
		UINT cstate = GetCheck();
		CSize sizeText = pDC->GetTextExtent("X",1);
		int hgt = sizeText.cy;
		btnRect2 = btnRect;
		btnRect2.right = btnRect2.left + hgt;
		stat = CMFCVisualManager::GetInstance()->DrawCheckBox(pDC, btnRect2, hlt, cstate, enable, select);
		if (stat==FALSE)
		{
			MessageBox("Visual Style Should be handle when create checkbox.", "Error", MB_OK);
			return;
		}
		btnRect.left += hgt + 4;
		COLORREF TextColor = GetFGColor();
		if ((state & ODS_DISABLED) || (m_active==-1))
			TextColor = RGB(125,125,125);
		COLORREF prevColor = pDC->SetTextColor(TextColor);
		pDC->SetBkMode(TRANSPARENT);
		pDC->DrawText(buffer, strlen(buffer), btnRect, DT_LEFT|DT_VCENTER|DT_SINGLELINE);
		pDC->SetTextColor(prevColor);
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
void CNCLButton::DrawFrame(CDC *DC, CRect R, int Inset)
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
void CNCLButton::DrawFilledRect(CDC *DC, CRect R, COLORREF color)
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
void CNCLButton::DrawLine(CDC *DC, CRect EndPoints, COLORREF color)
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
void CNCLButton::DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color)
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
void CNCLButton::DrawButtonText(CDC *DC, CRect R, const char *Buf, COLORREF TextColor)
{
    COLORREF prevColor = DC->SetTextColor(TextColor);
	DC->SelectObject(m_fieldFont );
    DC->SetBkMode(TRANSPARENT);
	DC->DrawText(Buf, strlen(Buf), R, DT_CENTER|DT_VCENTER|DT_SINGLELINE);
	DC->SetTextColor(prevColor);
}

/***********************************************************************
**
**   FUNCTION: :DrawButtonIcon(CDC *DC, CRect R, HBITMAP hBmap)
**
**         Draw the button icon in the button
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLButton::DrawButtonIcon(CDC *DC, CRect R, HBITMAP hBmap)
{
    DC->SetBkMode(TRANSPARENT);
	BITMAP bm;
	::GetObjectA(hBmap, sizeof (bm), &bm);
	int cx = bm.bmWidth;
	int cy = bm.bmHeight;
	int hgt = R.Height()-1;
	int wdt = hgt*((float)cx/(float)cy);
	int left = (R.Width()-wdt)/2.0;
	
//	TransparentBlt(DC->m_hDC, left, top, cx, 
//			cy, hBmap, 0, 0, 0xC0C0C0);
	Transparent_StretchBlt(DC->m_hDC, left, 0, R.Height()-1,
			R.Height()-1, hBmap, 0, 0, cx, cy, 0xC0C0C0);
}

BOOL CNCLButton::OnEraseBkgnd(CDC* pDC) 
{
//	return CButton::OnEraseBkgnd(pDC);

	if (m_type!=7)
		return CButton::OnEraseBkgnd(pDC);
/*
.....background set here not working
*/
	CRect rect;
	GetClientRect(&rect);

	CBrush pFillBrush;
	pFillBrush.CreateSolidBrush(m_bg);
	CBrush *old = pDC->SelectObject(&pFillBrush);
	pDC->FillRect(rect, &pFillBrush);
	pDC->SelectObject(old);
	return TRUE;
}

void CNCLButton::OnFillBackground(CDC* pDC, const CRect & rect)
{
//	return CMFCButton::OnFillBackground(pDC, rect);
	if (m_type!=7)
		return CMFCButton::OnFillBackground(pDC, rect);
	CBrush pFillBrush;
	pFillBrush.CreateSolidBrush(m_bg);
	CBrush *old = pDC->SelectObject(&pFillBrush);
	pDC->FillRect(rect, &pFillBrush);
	pDC->SelectObject(old);
}


