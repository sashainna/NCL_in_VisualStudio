/********************************************************************* 
**  NAME:  wsnttxtwin.cpp
**
**			Native WinNT read only text scrolling window functions
**			implementation of CNCLTextWin class functions
**	CONTAINS: CNCLTextWin class functions
**			all functions declared in wsnttxtwin.h
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttxtwin.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			01/20/17 , 12:26:14
*********************************************************************/
#include "wsntstdafx.h"
#include "wsnttxtwin.h"
#include "wsntform.h"
#include "wsntformbar.h"
#include "wsntcfunc.h"
#include "lcom.h"

/***********************************************************************
**
**   FUNCTION: CNCLTextWin(CWnd* pParentWnd, int type)
**
**              Constructor of class CNCLTextWin
**
**   INPUT:  pParentWnd: parent window
**			ctype: 1: form help box
**					others: scrolling text window
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLTextWin::CNCLTextWin(CWnd* pParentWnd, int type, char *title) : CDialog(CNCLTextWin::IDD, pParentWnd)
{
	m_type = type;
	m_insertpos = 0;
	m_nrows = 24;
	m_ncols = 80;
	if (title!=NULL)
		m_title = title;
	else
		m_title = "";
	if (m_txtfont.m_hObject)
		VERIFY (m_txtfont.DeleteObject ());	
	VERIFY (m_txtfont.CreatePointFont (UW_form_helpsize*10, UW_form_font));
}

/***********************************************************************
**
**   FUNCTION: ~CNCLTextWin()
**
**              Destructor of class CNCLTextWin
**			
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**********************************************************************/
CNCLTextWin::~CNCLTextWin()
{
/*
......remove this from parent window
*/
	if (m_type == 1)
	{
		((CNCLForm*)m_pParentWnd)->m_helpbox = NULL;
	}
	else if (m_type == 0)
	{
		((CNCLFormBar*)m_pParentWnd)->m_helpbox = NULL;
	}
}

/***********************************************************************
**
**   FUNCTION: SetText(char *text)
**
**        Set the window text
**			
**   INPUT:  text: text to set
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLTextWin::SetText(char *text)
{
	int len = strlen(text);
	if (len>1000000)
		text[1000000] = '\0';
	SetDlgItemText(IDC_TEXTWINEDIT, text);
}

void CNCLTextWin::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNCLTextWin)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CNCLTextWin, CDialog)
	//{{AFX_MSG_MAP(CNCLTextWin)
		ON_WM_SIZE()
		ON_WM_CREATE()
		ON_WM_NCPAINT()
		ON_WM_NCLBUTTONDOWN() 
		ON_WM_LBUTTONUP()
		ON_WM_NCHITTEST()
		ON_WM_NCACTIVATE()
		ON_MESSAGE(WM_CTLCOLORSTATIC, OnCtrlColorEdit)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
/***********************************************************************
**   FUNCTION: OnCtrlColorEdit(WPARAM wParam, LPARAM lParam)
**			This function called when edit field color changed
**			this is working for editbox and CNCLDDCombo
**   INPUT:  WPARAM wParam: include HDC information of the control 
**			LPARAM lParam: Window handler of the control
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
LRESULT CNCLTextWin::OnCtrlColorEdit(WPARAM wParam, LPARAM lParam)
{
	COLORREF fcolor, bcolor;

	bcolor = RGB(255,255,210);
	SetBkColor((HDC)wParam, bcolor);
	fcolor = RGB(0,0,100);
	SetTextColor((HDC)wParam, fcolor);
	static HBRUSH bh = CreateSolidBrush(bcolor);
	return (LRESULT)bh;
}


/***********************************************************************
**
**   FUNCTION: InsertText(char *tmp)
**
**        Insert a text into current position
**			
**   INPUT:  tmp: text to insert
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLTextWin::InsertText(char *tmp)
{
	char *buf;
	CString wintxt, wintxt2;
	int len = strlen(tmp);
	CEdit *edt = (CEdit*) GetDlgItem(IDC_TEXTWINEDIT);
	edt->GetWindowText(wintxt); 
	buf = wintxt.GetBuffer(200*80);
	if (m_insertpos+len>80*200)
		wintxt2 = buf + len;
	else
		wintxt2 = buf; 
	edt->SetWindowText(wintxt2);
	edt->SetSel(m_insertpos, m_insertpos);
	edt->ReplaceSel(tmp);
	m_insertpos += len;
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
c***********************************************************************
*/
void CNCLTextWin::OnSize( UINT nType, int cx, int cy )
{
	CWnd::OnSize(nType, cx, cy );

	CRect windowRect;
	GetClientRect(windowRect);
	CWnd* pChildWnd = GetWindow(GW_CHILD);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(windowRect);
}
/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize window text
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CNCLTextWin::OnInitDialog()
{
	CDialog::OnInitDialog();
	if (m_type==2)
/*
.....scrolling window
*/
		((CEdit*)GetDlgItem(IDC_TEXTWINEDIT))->LimitText(200*80);
	else
/*
......help window
*/
		((CEdit*)GetDlgItem(IDC_TEXTWINEDIT))->LimitText(1000000);
	
	CWnd *txtwin = GetDlgItem(IDC_TEXTWINEDIT);
	if (txtwin!=NULL)
	{
		txtwin->SetFont(&m_txtfont);
	}
	int baseunitX;
	int baseunitY;
	uw_ntget_avrchr_size(UW_form_helpsize*10, "COURIER", &baseunitX, &baseunitY);
	int size[2];
	size[0] = baseunitX*m_ncols + 30;
	size[1] = baseunitY*m_nrows + 10;
	MoveWindow(100, 100, size[0],size[1]);
	if (m_title!="")
		SetWindowText(m_title);
	return TRUE;
}

/***********************************************************************
c
c   FUNCTION: OnNcPaint()
c			paint the no-client area
c
c   INPUT:  None.
c
c   OUTPUT :   None.
c   RETURN:    None.
c
**********************************************************************/
void CNCLTextWin::OnNcPaint() 
{
	Default();
/*
	CWindowDC dc(this);

	CRect rc;
	GetWindowRect(rc);

	rc.bottom = GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);

	CRect closeRect;
	closeRect.top = GetSystemMetrics(SM_CYFRAME) + 1;
	closeRect.bottom = GetSystemMetrics( SM_CYSIZE ) + 1;
	closeRect.left = rc.right - rc.left - closeRect.bottom;
	closeRect.right = rc.right - rc.left - closeRect.top;

	dc.DrawFrameControl(closeRect,
	DFC_CAPTION,
	DFCS_CAPTIONCLOSE );

	m_rcClose = closeRect;
	
	CRect minRect;
	minRect.top = closeRect.top;
	minRect.bottom = closeRect.bottom;
	minRect.right = rc.right - rc.left - closeRect.bottom;
	minRect.left = minRect.right - (closeRect.bottom - closeRect.top);

	dc.DrawFrameControl(minRect,
	DFC_CAPTION,
	DFCS_CAPTIONMIN );
	m_rcMin = minRect;
*/
}


/***********************************************************************
c
c   FUNCTION: HitTest( CPoint point )
c
c       Call this function to find out which area the user is click in
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of point need to be tested. 
c
c   OUTPUT :   None
c   RETURN:    One of the mouse hit-test enumerated values listed below. 
c				DHT_CLOSE	in the title-bar 'X' close box area
c				DHT_MIN		in the title-bar '-' minimize box area
c				DHT_CAPTION   In a title-bar area.
c
**********************************************************************/
DWORD CNCLTextWin::HitTest(CPoint pt)
{
	CRect rect=m_rcClose;
	CRect rect2=m_rcMin;

	if(rect.PtInRect(pt))
		return (DWORD) DHT_CLOSE;
	else if (rect2.PtInRect(pt))
		return (DWORD) DHT_MIN;
	else
		return (DWORD) DHT_CAPTION;
}

/***********************************************************************
c
c   FUNCTION: OnNcLButtonDown(UINT nHitTest, CPoint point)
c
c           The framework calls this member function when the user 
c			push the left mouse button while the cursor is within a nonclient area
c
c   INPUT:  nHitTest: Specifies the hit-test code. A hit test is a test that determines the location of the cursor
c			point: Specifies a CPoint object that contains the x and y screen coordinates of the cursor position
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLTextWin::OnNcLButtonDown(UINT nHitTest, CPoint point) 
{ 
///////// The code below is old version window can't display systembox right, we draw ourselves but not need now
	Default();
	return;
////////
	CPoint pt=point;
	ScreenToClient(&pt);
	pt.y += GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);
	pt.x += 5;

	DWORD hitTest = HitTest(pt);

	switch(hitTest)
	{
	case DHT_CLOSE:
			{
		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcClose,
			DFC_CAPTION,
			DFCS_CAPTIONCLOSE | DFCS_PUSHED);
			m_LastHit = hitTest;
			m_ButtonDown = hitTest;
			SetCapture();
			}
			break;
	case DHT_MIN:
			{
		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcMin,
			DFC_CAPTION,
			DFCS_CAPTIONMIN | DFCS_PUSHED);
			m_LastHit = hitTest;
			m_ButtonDown = hitTest;
			SetCapture();
			}
			break;
	default:
			Default(); 
			break;
	}
} 

/***********************************************************************
c
c   FUNCTION: OnLButtonUp(UINT nFlags, CPoint point)
c			Callback function for mouse button up
c
c   INPUT:  nFlags: Indicates whether various virtual keys are down.
c					MK_CONTROL   Set if the CTRL key is down.
c					MK_MBUTTON   Set if the middle mouse button is down.
c					MK_RBUTTON   Set if the right mouse button is down.
c					MK_SHIFT   Set if the SHIFT key is down.
c			point:  Specifies the x- and y-coordinate of the cursor.
c
c   OUTPUT :   None.
c   RETURN:    None.
c
**********************************************************************/
void CNCLTextWin::OnLButtonUp(UINT nFlags, CPoint point) 
{
	if(this != GetCapture())
		return;
///////// The code below is old version window can't display systembox right, we draw ourselves but not need now
	m_ButtonDown = 0;
	ReleaseCapture();
	Default();
	return;
///////
	CPoint pt=point;
	point.y += GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);
	point.x += 5;

	DWORD hitTest = HitTest(point);

	switch(m_ButtonDown) // Release the pressed button
	{
	case DHT_CLOSE:
			{
		    CWindowDC dc(this);

			DrawFrameControl(dc.m_hDC,
				m_rcClose,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE);
			}
			break;
	case DHT_MIN:
			{
		    CWindowDC dc(this);

			DrawFrameControl(dc.m_hDC,
				m_rcMin,
				DFC_CAPTION,
				DFCS_CAPTIONMIN);
			}
			break;
	default:
			break;
	}

	switch(hitTest)	
	{
	case DHT_CLOSE:
			SendMessage(WM_CLOSE, 0,0);
			if (m_type == 1)
			{
				((CNCLForm*)m_pParentWnd)->m_helpbox = NULL;
			}
			else if (m_type == 0)
			{
				((CNCLFormBar*)m_pParentWnd)->m_helpbox = NULL;
			}
			break;
	case DHT_MIN:
			ShowWindow(SW_MINIMIZE);
			break;
	default:
			break;
	}
	m_ButtonDown = 0;
	ReleaseCapture();
}

/***********************************************************************
c
c   FUNCTION: OnNcActivate(BOOL bActive)
c			called when active no-client area of the dialog
c
c   INPUT:  bActive: not used here.
c
c   OUTPUT :   None.
c   RETURN:    True
c
**********************************************************************/
BOOL CNCLTextWin::OnNcActivate(BOOL bActive) 
{
   OnNcPaint(); 
   return TRUE; 
}

/***********************************************************************
c
c   FUNCTION: OnNcHitTest( CPoint point )
c
c       The framework calls this member function 
c			for the CWnd object that contains the cursor 
c			every time the mouse is moved.
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always screen coordinates.
c
c   OUTPUT :   None
c   RETURN:    One of the mouse hit-test enumerated values listed below. 
c				DHT_CLOSE	in the title-bar 'X' close box area
c				DHT_MIN		in the title-bar '-' minimize box area
c				DHT_CAPTION   In a title-bar area.
c
**********************************************************************/
LRESULT CNCLTextWin::OnNcHitTest(CPoint point) 
{
///////// The code below is old version window can't display system box right, we draw ourselves but not need now
return CDialog::OnNcHitTest(point);
///////////
	if(this != GetCapture())
		return CDialog::OnNcHitTest(point); // The default handler

	CPoint pt=point;
	ScreenToClient(&pt);
	pt.y += GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);
	pt.x += 5;

	DWORD hitTest = HitTest(pt);

	if(hitTest == m_LastHit)
		return CDialog::OnNcHitTest(point);

	m_LastHit = hitTest;

	UINT pushed = 0;
	if(m_ButtonDown == hitTest)
		pushed = DFCS_PUSHED;

	CWindowDC dc(this);
	switch(hitTest)
	{
	case DHT_CLOSE:
			{
			DrawFrameControl(dc.m_hDC,
				m_rcClose,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE | pushed);
			}
			break;
	case DHT_MIN:
			{
			DrawFrameControl(dc.m_hDC,
				m_rcMin,
				DFC_CAPTION,
				DFCS_CAPTIONMIN | pushed);
			}
			break;
	default:
			DrawFrameControl(dc.m_hDC,
				m_rcClose,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE);
			break;
	}
	return hitTest;
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
int CNCLTextWin::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CDialog::OnCreate(lpCreateStruct) == -1)
		return -1;
/*
......not used, initial here for set a font style
*/
/*
	if (m_txtfont.m_hObject)
		VERIFY (m_txtfont.DeleteObject ());	

	LOGFONT lf;
	memset(&lf, 0, sizeof(LOGFONT));
//if we need set bold
//	lf.lfWeight = 700;
	strncpy_s(lf.lfFaceName, LF_FACESIZE, _T("MS Sans Serif"), 20);
	lf.lfHeight = 100;
	m_txtfont.CreatePointFontIndirect(&lf, GetDC());
	SetFont(&m_txtfont);
*/
}


