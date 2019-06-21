/************************************************************************
**
**   FILE NAME: wsntlumchooser.cpp
**
**	 Description - Functions implementation for
**		CNCLLumChooser class 
**	 CONTAINS: 
**		all functions declared in wsntlumchooser.h
**
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntlumchooser.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:27
**
************************************************************************
*/
#include "wsntstdafx.h"
#include "wsntlumchooser.h"

#ifdef _DEBUG
//#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNCLLumChooser

/***********************************************************************
c
c   SUBROUTINE:  CNCLLumChooser
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLLumChooser::CNCLLumChooser()
{
	m_bCapture		= FALSE;
	m_iColorAreaWidth = LUM_COLORBAR_WIDTH;
	InternalPaint	= &CNCLLumChooser::FirstTimePaint;

	WNDCLASS wc;

	::ZeroMemory(&wc, sizeof(wc));
	wc.hInstance	= ::AfxGetInstanceHandle();
	wc.lpfnWndProc	= ::DefWindowProc;
	wc.hCursor		= (HCURSOR)::LoadCursor(NULL, MAKEINTRESOURCE(IDC_ARROW));
	wc.lpszClassName= LUMCLASSNAME;

	::AfxRegisterClass(&wc);
}

CNCLLumChooser::~CNCLLumChooser()
{
}

/***********************************************************************
c
c   SUBROUTINE:  Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext)
c
c   FUNCTION:  This function Create a CNCLLumChooser window
c
c   INPUT:  dwStyle: window style
c			rect: window size
c			nID: wndow ID
c			pContext: 
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CNCLLumChooser::Create (DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext)
{
	ASSERT((rect.bottom - rect.top) > (PADDING+BORDER_WIDTH) * 2);
	return CWnd::Create(NULL, NULL, dwStyle, rect, pParentWnd, nID, pContext);
}

BEGIN_MESSAGE_MAP(CNCLLumChooser, CWnd)
	//{{AFX_MSG_MAP(CNCLLumChooser)
	ON_WM_PAINT()
	ON_WM_DESTROY()
	ON_WM_ERASEBKGND()
	ON_WM_LBUTTONUP()
	ON_WM_LBUTTONDOWN()
	ON_WM_MOUSEMOVE()
	ON_WM_KILLFOCUS()
	ON_WM_SIZE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  ResetBitmap
c
c   FUNCTION:  This function reset the bitmap of a CNCLLumChooser window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLLumChooser::ResetBitmap() 
{
	CRect rect;
	GetClientRect(&rect);
	m_size = Rect(rect.left, rect.top, rect.Width(), rect.Height());

	m_spOffScreen= auto_ptr<Bitmap>(new Bitmap(m_size.Width, m_size.Height, PixelFormat32bppARGB));
	m_spTriangle = auto_ptr<Bitmap>(new Bitmap(TRANGLEWIDTH, TRANGLEHEIGHT, PixelFormat32bppARGB));
	
	//Draw the triangle
	Graphics	*pg = new Graphics(m_spTriangle.get());
	SolidBrush	brush(Color(0,0,0,0));
	Point		triangle[3] = {Point(0,TRANGLEHEIGHT/2), Point(m_spTriangle->GetWidth(), 0), Point(m_spTriangle->GetWidth(), TRANGLEHEIGHT)};
	
	pg->FillRectangle(&brush, 0, 0, m_spTriangle->GetWidth(), m_spTriangle->GetHeight());
	brush.SetColor(Color(255, 0, 0, 0));
	pg->SetSmoothingMode(SmoothingModeAntiAlias);
	pg->FillPolygon(&brush, triangle, 3);
	delete pg;

	//Draw the edge around color area
	pg		= new Graphics(m_spOffScreen.get());
	HDC hdc = pg->GetHDC();
	::DrawEdge(hdc, CRect(0, PADDING, m_iColorAreaWidth+2, GetColorAreaHeight()+BORDER_WIDTH*2+PADDING), EDGE_ETCHED, BF_RECT);
	pg->ReleaseHDC(hdc);
	DrawColor();
	pg->DrawImage(m_spTriangle.get(), TrangleLeftLimit(), GetTrangleYPos());
}

/***********************************************************************
c
c   SUBROUTINE:  DrawColor
c
c   FUNCTION:  This function draw the colors of the CNCLLumChooser window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLLumChooser::DrawColor()
{
	BitmapData bitmapData;
	
	Rect rect(BORDER_WIDTH, 
				PADDING + BORDER_WIDTH, 
				m_iColorAreaWidth, 
				GetColorAreaHeight());
	
	m_spOffScreen->LockBits(
      &rect,
      ImageLockModeWrite,
	  PixelFormat32bppRGB,
	  &bitmapData);

	DWORD *pixel = (COLORREF*)bitmapData.Scan0;
	CHls hls = m_color;
	for(double y=0; y<rect.Height; y++) {
		hls.SetLuminance(1.0f - y/(double)rect.Height);
		*(pixel++) = hls.ToRGB();
		for(int x=0; x<rect.Width-1; x++) 
			*(pixel++) = *(pixel-1);
	}
	m_spOffScreen->UnlockBits(&bitmapData);
}

/***********************************************************************
c
c   SUBROUTINE:  NotifyColorChanged
c
c   FUNCTION:  handle color change
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLLumChooser::NotifyColorChanged()
{
	NMHDR mhdr;
	mhdr.code		= NM_COLORCHANGE;
	mhdr.hwndFrom	= GetSafeHwnd();
	mhdr.idFrom		= GetDlgCtrlID();
	
	GetParent()->SendMessage(WM_NOTIFY, (WPARAM)mhdr.idFrom, (LPARAM)&mhdr);
}

inline int CNCLLumChooser::TrangleLeftLimit()
{
	return m_iColorAreaWidth+2;
}

inline int CNCLLumChooser::TrangleRightLimit()
{
	return m_size.Width;
}
inline int CNCLLumChooser::GetTrangleYPos()
{
	int lineInColorArea = (int)((CHls::HLSMAX-m_color.GetLuminance())*GetColorAreaHeight())-1;
	return PADDING+BORDER_WIDTH + (lineInColorArea-TRANGLEHEIGHT/2);
}

inline int CNCLLumChooser::GetColorAreaHeight()
{
	return m_size.Height - (PADDING+BORDER_WIDTH) * 2;
}

/***********************************************************************
c
c   SUBROUTINE: ResetTrangleAndLum(CPoint &mouse)
c
c   FUNCTION:  reset both lum area and trangle area (arrow)
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
inline void CNCLLumChooser::ResetTrangleAndLum(CPoint &mouse)
{
	Graphics graphics(m_spOffScreen.get());

	ClearTrangle(&graphics);
	m_color.SetLuminance(CHls::HLSMAX - (double)(mouse.y - PADDING)/(double)GetColorAreaHeight());
	graphics.DrawImage(m_spTriangle.get(), TrangleLeftLimit(), GetTrangleYPos());
}

/***********************************************************************
c
c   SUBROUTINE: ClearTrangle(Graphics *pg)
c
c   FUNCTION: clear trangle area (arrow)
c
c   INPUT:  pg:
c   OUTPUT: none
c
c***********************************************************************
*/
inline void CNCLLumChooser::ClearTrangle(Graphics *pg)
{
	Color		co3Dface;
	co3Dface.SetFromCOLORREF(::GetSysColor(COLOR_3DFACE));
	SolidBrush	brush(co3Dface);
	pg->FillRectangle(&brush, TrangleLeftLimit(), GetTrangleYPos(), TRANGLEWIDTH, TRANGLEHEIGHT);
}

/***********************************************************************
c
c   SUBROUTINE:  FirstTimePaint
c
c   FUNCTION:  first time paint
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLLumChooser::FirstTimePaint()
{
	ResetBitmap();
	Paint();
	InternalPaint = &CNCLLumChooser::Paint;
//	NotifyColorChanged();
}

/***********************************************************************
c
c   SUBROUTINE:  Paint
c
c   FUNCTION:  Paint the CNCLHueSatChooser window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLLumChooser::Paint()
{
	CPaintDC dc(this);
	
	Graphics graphics(dc.GetSafeHdc());
	graphics.DrawImage(m_spOffScreen.get(), 0, 0);
}
/////////////////////////////////////////////////////////////////////////////
// CNCLLumChooser message handlers
void CNCLLumChooser::OnPaint() 
{
	(this->*InternalPaint)();
}

void CNCLLumChooser::OnDestroy() 
{
	CWnd::OnDestroy();
	
	m_spOffScreen	= auto_ptr<Bitmap>(NULL);
	m_spTriangle	= auto_ptr<Bitmap>(NULL);
}

BOOL CNCLLumChooser::OnEraseBkgnd(CDC* pDC) 
{
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: OnLButtonDown(UINT nFlags, CPoint pt)
**
**       Left mouse button down callback
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLLumChooser::OnLButtonDown(UINT nFlags, CPoint point) 
{
	SetFocus();
	m_bCapture = TRUE;
	
	CRect rect(TrangleLeftLimit(), PADDING, TrangleRightLimit(), m_size.Height - PADDING - BORDER_WIDTH);

	::ClientToScreen(GetSafeHwnd(), &rect.TopLeft());
	::ClientToScreen(GetSafeHwnd(), &rect.BottomRight());

	::ClipCursor(&rect);

	CPoint pt;
	::GetCursorPos(&pt);
	ScreenToClient(&pt);
	ResetTrangleAndLum(pt);
	Invalidate();

	NotifyColorChanged();
	CWnd::OnLButtonDown(nFlags, point);
}
/***********************************************************************
**
**   FUNCTION: OnLButtonUp(UINT nFlags, CPoint pt)
**
**       Left mouse button up callback
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLLumChooser::OnLButtonUp(UINT nFlags, CPoint point) 
{
	m_bCapture = FALSE;
	::ClipCursor(NULL);
	CWnd::OnLButtonUp(nFlags, point);
}
/***********************************************************************
**
**   FUNCTION: OnKillFocus(CWnd* pNewWnd) 
**
**       lose focus callback
**
**   INPUT:  pNewWnd: new focus window
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLLumChooser::OnKillFocus(CWnd* pNewWnd) 
{
	CWnd::OnKillFocus(pNewWnd);

	m_bCapture = FALSE;
	::ClipCursor(NULL);
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
void CNCLLumChooser::OnMouseMove(UINT nFlags, CPoint point) 
{
	if(m_bCapture) {
		ResetTrangleAndLum(point);
		NotifyColorChanged();
		Invalidate();
	}
	CWnd::OnMouseMove(nFlags, point);
}

/***********************************************************************
**
**   FUNCTION: OnSize(UINT nType, int cx, int cy)
**
**       size change callback
**
**   INPUT:  nType:
**				cx, cy:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLLumChooser::OnSize(UINT nType, int cx, int cy) 
{
	CWnd::OnSize(nType, cx, cy);
	ResetBitmap();
	Invalidate();
}
