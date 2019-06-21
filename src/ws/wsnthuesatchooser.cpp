/************************************************************************
**
**   FILE NAME: wsnthuesatchooser.cpp
**
**	 Description - Functions implementation for
**		CNCLHueSatChooser class 
**	 CONTAINS: 
**		all functions declared in wsnthuesatchooser.h
**
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnthuesatchooser.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:27
**
************************************************************************
*/
#include "wsntstdafx.h"
#include "wsnthuesatchooser.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNCLHueSatChooser

/***********************************************************************
c
c   SUBROUTINE:  CNCLHueSatChooser
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLHueSatChooser::CNCLHueSatChooser()
{
	m_bCapture		= FALSE;
	InternalPaint	= &CNCLHueSatChooser::FirstTimePaint;

	WNDCLASS wc;

	::ZeroMemory(&wc, sizeof(wc));
	wc.hInstance	= ::AfxGetInstanceHandle();
	wc.lpfnWndProc	= ::DefWindowProc;
	wc.hCursor		= (HCURSOR)::LoadCursor(NULL, MAKEINTRESOURCE(IDC_ARROW));
	wc.lpszClassName= HUESATCLASSNAME;
	::AfxRegisterClass(&wc);
}

CNCLHueSatChooser::~CNCLHueSatChooser()
{
}

/***********************************************************************
c
c   SUBROUTINE:  Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext)
c
c   FUNCTION:  This function Create a CNCLHueSatChooser window
c
c   INPUT:  dwStyle: window style
c			rect: window size
c			nID: wndow ID
c			pContext: 
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CNCLHueSatChooser::Create (DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext)
{
	ASSERT((rect.bottom - rect.top) > (PADDING+BORDER_WIDTH) * 2);
	return CWnd::Create(NULL, NULL, dwStyle, rect, pParentWnd, nID, pContext);
}

BEGIN_MESSAGE_MAP(CNCLHueSatChooser, CWnd)
	//{{AFX_MSG_MAP(CNCLHueSatChooser)
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_PAINT()
	ON_WM_ERASEBKGND()
	ON_WM_LBUTTONDOWN()
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONUP()
	ON_WM_KILLFOCUS()
	ON_WM_SIZE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  UpdateSize
c
c   FUNCTION:  This function update the size of a CNCLHueSatChooser window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLHueSatChooser::UpdateSize()
{
	CRect rect;
	GetClientRect(&rect);
	m_size = Rect(rect.left, rect.top, rect.Width(), rect.Height());

	m_colorArea = Rect (BORDER_WIDTH, 
		PADDING+BORDER_WIDTH, 
		m_size.Width-BORDER_WIDTH*2, 
		m_size.Height-(PADDING+BORDER_WIDTH)*2);
}

/***********************************************************************
c
c   SUBROUTINE:  ResetBitmap
c
c   FUNCTION:  This function reset the bitmap of a CNCLHueSatChooser window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLHueSatChooser::ResetBitmap()
{
	UpdateSize();	
	m_spOffScreen = auto_ptr<Bitmap>(new Bitmap(m_size.Width, m_size.Height, PixelFormat32bppARGB));
	m_spSaved	  = auto_ptr<Bitmap>(new Bitmap(5, 5, PixelFormat32bppARGB));
	
	DrawEdge();
	DrawColor();
}

/***********************************************************************
c
c   SUBROUTINE:  DrawEdge
c
c   FUNCTION:  This function draw the edge of the window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
inline void CNCLHueSatChooser::DrawEdge()
{
	Graphics graphics(m_spOffScreen.get());
	HDC hdc = graphics.GetHDC();
	::DrawEdge(hdc, 
		CRect(0, PADDING, m_size.Width, m_size.Height-PADDING), 
		EDGE_ETCHED, BF_RECT);
	graphics.ReleaseHDC(hdc);
}

/***********************************************************************
c
c   SUBROUTINE:  DrawColor
c
c   FUNCTION:  This function draw the colors of the CNCLHueSatChooser window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLHueSatChooser::DrawColor()
{
	BitmapData bitmapData;
	
	m_spOffScreen->LockBits(
      &m_colorArea,
      ImageLockModeWrite,
	  PixelFormat32bppRGB,
	  &bitmapData);

	DWORD *pixel = (COLORREF*)bitmapData.Scan0;
	CHls color(0.0f, 0.5f, 0.0f);
	for(double y=0; y<m_colorArea.Height; y++) {
		color.SetSaturation(1.0f - y/(double)m_colorArea.Height);
		for(double x=0; x<m_colorArea.Width; x++) {
			color.SetHue(x/(double)m_colorArea.Width);
			*pixel++ = color.ToRGB();
		}
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
void CNCLHueSatChooser::NotifyColorChanged()
{
	NMHDR mhdr;
	mhdr.code		= NM_COLORCHANGE;
	mhdr.hwndFrom	= GetSafeHwnd();
	mhdr.idFrom		= GetDlgCtrlID();
	
	GetParent()->SendMessage(WM_NOTIFY, (WPARAM)mhdr.idFrom, (LPARAM)&mhdr);
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
void CNCLHueSatChooser::FirstTimePaint()
{
	ResetBitmap();
	Paint();
	InternalPaint = &CNCLHueSatChooser::Paint;
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
void CNCLHueSatChooser::Paint()
{
	CPaintDC dc(this);
	
	Graphics g(dc.GetSafeHdc());
	g.DrawImage(m_spOffScreen.get(), 0, 0);	

	//Draw Selected color box	
	Pen		whitePen(Color::White, 1);
	Region	clip;
	g.GetClip(&clip);
	g.SetClip(m_colorArea, CombineModeReplace);
	g.DrawRectangle(&whitePen, 
		(int)(m_colorArea.X + (double)m_colorArea.Width * GetHue() - 2),
		(int)(m_colorArea.Y + (double)m_colorArea.Height * (1.0f-GetSat()) - 2),
		5, 5);
	g.SetClip(&clip, CombineModeReplace);
}

/////////////////////////////////////////////////////////////////////////////
// CNCLHueSatChooser message handlers

void CNCLHueSatChooser::OnDestroy() 
{
	CWnd::OnDestroy();
	m_spOffScreen	= auto_ptr<Bitmap>(NULL);
	m_spSaved		= auto_ptr<Bitmap>(NULL);
}

void CNCLHueSatChooser::OnPaint() 
{
	(this->*InternalPaint)();
}

BOOL CNCLHueSatChooser::OnEraseBkgnd(CDC* pDC) 
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
void CNCLHueSatChooser::OnLButtonDown(UINT nFlags, CPoint point) 
{
	m_bCapture = TRUE;

	SetFocus();

	CRect r(BORDER_WIDTH, 
		PADDING+BORDER_WIDTH, 
		m_size.Width - BORDER_WIDTH, 
		m_size.Height- PADDING - BORDER_WIDTH);
	
	::ClientToScreen(GetSafeHwnd(), &r.TopLeft());
	::ClientToScreen(GetSafeHwnd(), &r.BottomRight());

	ClipCursor(&r);

	::GetCursorPos(&point);
	ScreenToClient(&point);

	m_color.SetHue((double)(point.x-m_colorArea.X)/(double)m_colorArea.Width);
	m_color.SetSaturation(1.0f - (double)(point.y-m_colorArea.Y)/(double)m_colorArea.Height);

	NotifyColorChanged();

	Invalidate();

	CWnd::OnMButtonDown(nFlags, point);
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
void CNCLHueSatChooser::OnLButtonUp(UINT nFlags, CPoint point) 
{
	ClipCursor(NULL);
	m_bCapture = NULL;
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
void CNCLHueSatChooser::OnKillFocus(CWnd* pNewWnd) 
{
	CWnd::OnKillFocus(pNewWnd);
	ClipCursor(NULL);
	m_bCapture = NULL;
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
void CNCLHueSatChooser::OnMouseMove(UINT nFlags, CPoint point) 
{
	if(m_bCapture) {
		m_color.SetHue((double)(point.x-m_colorArea.X)/(double)(m_colorArea.Width-1));
		m_color.SetSaturation(1.0f - (double)(point.y-m_colorArea.Y)/(double)(m_colorArea.Height-1));

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
void CNCLHueSatChooser::OnSize(UINT nType, int cx, int cy)
{
	CWnd::OnSize(nType, cx, cy);
	ResetBitmap();
}
