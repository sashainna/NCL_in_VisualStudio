/************************************************************************
**
**   FILE NAME: wsntDDpic.cpp
**
**	 Description - Functions implementation for
**		CNCLDDPicWin class it's a window display a picture file
**	 CONTAINS: 
**		all functions declared in wsntpicsel.h
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**		   wsntDDpic.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			05/04/15 , 11:44:57
************************************************************************
*/
#include "wsntstdafx.h"
#include <afxole.h>         // MFC OLE classes
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxdisp.h >       // MFC OLE automation classes
#include <afxpriv.h>
#include "wsntDDpic.h"
#include "wsntdropsource.h"
#include "wsntDDform.h"
#include "xenv1.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define BARLEN	4
#define HT_NONE		0
#define HT_INSIDE	1
#define HT_TOPLEFT	2
#define HT_LEFT		3
#define HT_BOTTOMLEFT	4
#define HT_BOTTOM	5
#define HT_BOTTOMRIGHT	6
#define HT_RIGHT	7
#define HT_TOPRIGHT	8
#define HT_TOP	9

extern "C" int ul_open_mod_file(char*, char*, char*, char*, char*, int, FILE**);
extern "C" char * uu_malloc(int);
extern "C" void uu_free(char*);
extern char UW_formdlg_font[];
extern HBITMAP WINAPI  CopyWindowToBitmap(LPRECT lpRect, CWnd *pWnd);
extern void CopyPropertyPage(CNCLFormProp *prop_dlg_from, CNCLFormProp *prop_dlg_to);
extern int UW_form_hotspot_item;

/////////////////////////////////////////////////////////////////////////////
// CNCLDDPicWin

/***********************************************************************
c
c   SUBROUTINE:  CNCLDDPicWin
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLDDPicWin::CNCLDDPicWin(char *name, char* filename, CWnd *parent)
	:	m_TargetDrop(NULL),
		m_TimerID(0)
{
	WNDCLASS wc;

	::ZeroMemory(&wc, sizeof(wc));
	wc.hInstance	= ::AfxGetInstanceHandle();
	wc.lpfnWndProc	= ::DefWindowProc;
	wc.hCursor		= (HCURSOR)::LoadCursor(NULL, MAKEINTRESOURCE(IDC_ARROW));
	wc.lpszClassName= "CNCLDDPicWin";
	::AfxRegisterClass(&wc);

	UX_pathname filen;
	strcpy(filen, filename);
	int status = ul_open_mod_file("UU_USER_SETTINGS", "forms", "UD_FORMDIR", (char*)UU_NULL, 
		filen, 0, (FILE**)UU_NULL);
	m_filename = filen;
	if (status==0)
		m_filename = filen;
	else
		m_filename = "";
	m_parent = parent;

	m_x = m_y = -1;
	m_mpoint.x = m_mpoint.y = -1;

	m_StartPoint = CPoint(-1, -1);
	m_TargetDrop = new CNCLMnDropTarget2();
	m_DragImage = new CImageList();
	m_dragimg_create = 0;
	m_itemno = -1;
	m_parent = NULL;
	m_prop_dlg = NULL;
	m_DragAllImage = NULL;
	strncpy(m_name, name, 39);
	m_name[39] = '\0';
	m_picture.Load(m_filename);

	m_area_num = 0;
	m_draw_areanum = -1;
	m_hotspot_rect.left = -1000;

	m_cursor[HT_NONE] = LoadCursor(NULL, IDC_ARROW);
	m_cursor[HT_INSIDE] = LoadCursor(NULL, IDC_SIZEALL);
	m_cursor[HT_TOPLEFT] = LoadCursor(NULL, IDC_SIZENWSE);
	m_cursor[HT_LEFT] = LoadCursor(NULL, IDC_SIZEWE);
	m_cursor[HT_BOTTOMLEFT] = LoadCursor(NULL, IDC_SIZENESW);
	m_cursor[HT_BOTTOM] = LoadCursor(NULL, IDC_SIZENS);
	m_cursor[HT_BOTTOMRIGHT] = LoadCursor(NULL, IDC_SIZENWSE);
	m_cursor[HT_RIGHT] = LoadCursor(NULL, IDC_SIZEWE);
	m_cursor[HT_TOPRIGHT] = LoadCursor(NULL, IDC_SIZENESW);
	m_cursor[HT_TOP] = LoadCursor(NULL, IDC_SIZENS);
	m_buttondown = 0;
	m_sizedir = 0;
	m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
	
	for (int i=0; i<8; i++)
	{
		m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
	}
	m_resize = 0;
	m_hotspot_rect.left = -1000;
	m_hittest = HT_NONE;
}

CNCLDDPicWin::~CNCLDDPicWin()
{
	if (m_prop_dlg!=NULL)
		delete m_prop_dlg;
	if (m_DragImage!=NULL)
		delete m_DragImage;
	if (m_DragAllImage!=NULL)
		delete m_DragAllImage;
}

/***********************************************************************
c
c   SUBROUTINE:  Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext)
c
c   FUNCTION:  This function Create a CNCLDDPicWin window
c
c   INPUT:  dwStyle: window style
c			rect: window size
c			nID: wndow ID
c			pContext: 
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CNCLDDPicWin::Create (DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext)
{
	ASSERT((rect.bottom - rect.top) > (PADDING+BORDER_WIDTH) * 2);
	BOOL ret = CWnd::Create(NULL, NULL, dwStyle, rect, pParentWnd, nID, pContext);
	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(2,-1,this);
	}
	m_prop_dlg->m_choices = m_filename;
	m_prop_dlg->m_label = m_name;
	m_picture.CreatePicBitmap(GetDC());
	return ret;
}
IMPLEMENT_DYNAMIC(CNCLDDPicWin, CWnd)

BEGIN_MESSAGE_MAP(CNCLDDPicWin, CWnd)
	//{{AFX_MSG_MAP(CNCLDDPicWin)
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_PAINT()
	ON_WM_ERASEBKGND()
	ON_WM_LBUTTONDOWN()
	ON_WM_RBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONUP()
	ON_WM_SIZE()
	ON_WM_MOVE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

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
void CNCLDDPicWin::InitDrag()
{
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
}

/***********************************************************************
c
c   SUBROUTINE:  Paint
c
c   FUNCTION:  Paint the CNCLDDPicWin window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLDDPicWin::Paint()
{	
	CRect rect;
	CBitmap picture;
	HDC memdc;
	int bmpwid, bmphgt;
	GetWindowRect(&rect);
	m_pDC = GetDC();
/*
.....we need paint all background first
*/
	COLORREF bkclr = m_pDC->GetBkColor();
	CBrush* pFillBrush = new CBrush(bkclr);
	CRect rect2 = rect;
	ScreenToClient(&rect2);
	m_pDC->FillRect(&rect2, pFillBrush);

	if (m_picture.m_picture==NULL)
	{
		picture.LoadBitmap(IDB_NCLPIC);
		m_Bmap = (HBITMAP)picture.GetSafeHandle();
		if (m_Bmap==NULL)
			return;
		GetClientRect(&rect);
		memdc=::CreateCompatibleDC(m_pDC->m_hDC);
		::SelectObject(memdc,m_Bmap);
		bmpwid = 48;
		bmphgt = 48;

		int right, bottom;
		float rat = (float)bmphgt/(float)bmpwid;
		float rat2 = (float)rect.bottom/(float)rect.right;

		if (rat<rat2)
		{
			right = rect.right;
			bottom = rect.right*rat;
		}
		else
		{
			bottom = rect.bottom;
			right = rect.bottom/rat;
		}
		SetStretchBltMode(m_pDC->m_hDC, HALFTONE);
		POINT pt;
		SetBrushOrgEx(m_pDC->m_hDC, 0, 0, &pt);
		int status;
		if (m_prop_dlg->m_active==-1)
			status = StretchBlt(m_pDC->m_hDC, 0,0, right, bottom,  memdc, 0, 0, bmpwid, bmphgt, SRCPAINT);
		else
			status = StretchBlt(m_pDC->m_hDC, 0,0, right, bottom,  memdc, 0, 0, bmpwid, bmphgt, SRCCOPY );	
		goto done;
	}
	m_picture.UpdateSizeOnDC(m_pDC);
	m_picture.Show(m_pDC, CPoint(0,0), CPoint(rect.Width(), rect.Height()), 0,0);
done:;
/*
.....draw the high light hotspot rect
*/
	if (m_hotspot_rect.left != -1000)
	{
		DrawHotspotRect(m_pDC, m_hotspot_rect);
	}
}
void CNCLDDPicWin::SetHotSpotRect(CRect rect)
{
	m_hotspot_rect = rect;
	Reset_selrec(rect);
/*
	int indx = m_prop_dlg->m_pic_act_area;
	m_prop_dlg->m_picarea[indx].xmin = rect.left;
	m_prop_dlg->m_picarea[indx].ymin = rect.top;
	m_prop_dlg->m_picarea[indx].xmax = rect.right;
	m_prop_dlg->m_picarea[indx].ymax = rect.bottom;
*/
}

void CNCLDDPicWin::ReseteHotSpotRect()
{
	m_hotspot_rect.left = -1000;
	m_selsize[0].left = -1000;
}
/***********************************************************************
**
**   FUNCTION: Handle_Hotspot_event(int evt, UINT nFlags, CPoint point)
**
**		this function will handle the event happened in CNCLDDFrame class
**		since the GroupBox itself can't accept any event.
**   
**	 INPUT:  evt: event ID
**			nFlags: event flag
**			point: cursor current point
**
**   OUTPUT :   none
**   RETURN:    return 0 if handled
**
**********************************************************************/
int CNCLDDPicWin::Handle_Hotspot_event(int evt, UINT nFlags, CPoint point)
{
	CRect rect, rect1, rect2, rect3, rect4;
	int ret;
	CNCLDDform *form = (CNCLDDform*) m_parent;

	if (m_hotspot_rect.left==-1000)
		return 1;
	if (evt==1)
		m_buttondown = 1;
	if (evt==2)
		m_buttondown = 0;
	rect = m_hotspot_rect;
	if ((rect.PtInRect(point))
		&& ((m_hittest == HT_NONE)||(m_hittest == HT_INSIDE)))
	{
		if (evt==1)
		{
/*
......button down
*/
			m_buttondown = 1;	
			m_sizedir = m_hittest;
/*
......draw the select box
*/
			Reset_selrec(rect);
			return 0;
		}
	}
//	if (m_hittest != HT_NONE)
	{
		if (evt==1)
		{
/*
......button down
*/
			m_buttondown = 1;	
			m_sizedir = m_hittest;
			m_sizerec = m_hotspot_rect;
		}
		if ((evt==2)&&(m_resize==1))
		{
/*
......resize the select picture area
*/	
			m_resize = 0;
			m_StartPoint.x = -100;
			m_StartPoint.y = -100;
			m_buttondown = 0;	
			if ((m_sizedir!=HT_NONE)&&(m_sizedir!=HT_INSIDE))
			{
				int sizing = 0;
				CRect old_rect = m_hotspot_rect;
				old_rect.left = old_rect.left - BARLEN - 2;
				old_rect.right = old_rect.right + BARLEN + 2;
				old_rect.top = old_rect.top - BARLEN - 2;
				old_rect.bottom = old_rect.bottom + BARLEN + 2;

				if ((m_sizedir!=HT_NONE)&&(m_sizedir!=HT_INSIDE))
					sizing = 1;

				m_hotspot_rect = m_sizerec;
				((CNCLDDform*)m_parent)->UpdatePropertyHSPTSize(m_sizerec, m_itemno);
				CRect rect = m_sizerec;
/*
.....left-top
*/
				m_selsize[0].left = rect.left - BARLEN;
				m_selsize[0].right = rect.left;
				m_selsize[0].top = rect.top - BARLEN;
				m_selsize[0].bottom = rect.top;
/*
.....left-middle
*/	
				m_selsize[1].left = rect.left - BARLEN;
				m_selsize[1].right = rect.left;
				m_selsize[1].top = rect.top+rect.Height()/2 - BARLEN;
				m_selsize[1].bottom = m_selsize[1].top + BARLEN;
/*
.....left-bottom
*/	
				m_selsize[2].left = rect.left - BARLEN;
				m_selsize[2].right = rect.left;
				m_selsize[2].top = rect.bottom;
				m_selsize[2].bottom = m_selsize[2].top + BARLEN;
/*
.....-middle-bottom
*/	
				m_selsize[3].left = rect.left + rect.Width()/2- BARLEN;
				m_selsize[3].right = m_selsize[3].left + BARLEN;
				m_selsize[3].top = rect.bottom;
				m_selsize[3].bottom = m_selsize[3].top + BARLEN;
/*
.....right-bottom
*/	
				m_selsize[4].left = rect.right;
				m_selsize[4].right = rect.right + BARLEN;
				m_selsize[4].top = rect.bottom;
				m_selsize[4].bottom = m_selsize[4].top + BARLEN;
/*
.....right-middle
*/	
				m_selsize[5].left = rect.right;
				m_selsize[5].right = rect.right + BARLEN;
				m_selsize[5].top = rect.top + rect.Height()/2 - BARLEN;
				m_selsize[5].bottom = m_selsize[5].top + BARLEN;
/*
.....right-top
*/	
				m_selsize[6].left = rect.right;
				m_selsize[6].right = rect.right + BARLEN;
				m_selsize[6].top = rect.top - BARLEN;
				m_selsize[6].bottom = m_selsize[6].top + BARLEN;
/*
.....middle-top
*/	
				m_selsize[7].left = rect.left  + rect.Width()/2- BARLEN;
				m_selsize[7].right = m_selsize[7].left + BARLEN;
				m_selsize[7].top = rect.top - BARLEN;
				m_selsize[7].bottom = rect.top;
	
				DrawSelect();
				RedrawWindow();
				UpdateWindow();
				((CNCLDDform*)m_parent)->UpdateDDFormView();
				if (sizing)
				{
					CRect upt_rect = m_sizerec;
//			ClientToScreen(&upt_rect);
					upt_rect.left = upt_rect.left - BARLEN - 2;
					upt_rect.right = upt_rect.right + BARLEN + 2;
					upt_rect.top = upt_rect.top - BARLEN - 2;
					upt_rect.bottom = upt_rect.bottom + BARLEN + 2;
//			updateWindow_draw(2, &upt_rect);
//			updateWindow_draw(2, &old_rect);		
					return 0;
				}
			}
		}
		if (evt==5)
		{
			if (m_sizerec.top==-1000)
				return 0;

			CRect rect = m_sizerec;
			if (m_buttondown==1)
			{
				if (m_sizedir==HT_LEFT)
				{
					rect.left = point.x;
				}
				if (m_sizedir==HT_TOPLEFT)
				{
					rect.left = point.x;
					rect.top = point.y;
				}
				if (m_sizedir==HT_BOTTOMLEFT)
				{
					rect.left = point.x;
					rect.bottom = point.y;
				}
				if (m_sizedir==HT_BOTTOM)
				{
					rect.bottom = point.y;
				}
				if (m_sizedir==HT_BOTTOMRIGHT)
				{
					rect.right = point.x;
					rect.bottom = point.y;
				}
				if (m_sizedir==HT_RIGHT)
				{
					rect.right = point.x;
				}
				if (m_sizedir==HT_TOPRIGHT)
				{
					rect.right = point.x;
					rect.top = point.y;
				}
				if (m_sizedir==HT_TOP)
				{
					rect.top = point.y;
				}
				if (m_sizerec!=rect)
				{
					CRect upt_rect = m_sizerec;
					m_resize = 1;
					m_sizerec = rect;
/*					ClientToScreen(&upt_rect);
					upt_rect.left = upt_rect.left - BARLEN - 2;
					upt_rect.right = upt_rect.right + BARLEN + 2;
					upt_rect.top = upt_rect.top - BARLEN - 2;
					upt_rect.bottom = upt_rect.bottom + BARLEN + 2;
					updateWindow_draw(2, &upt_rect);
					upt_rect = m_sizerec;
					ClientToScreen(&upt_rect);
					upt_rect.left = upt_rect.left - BARLEN - 2;
					upt_rect.right = upt_rect.right + BARLEN + 2;
					upt_rect.top = upt_rect.top - BARLEN - 2;
					upt_rect.bottom = upt_rect.bottom + BARLEN + 2;
					updateWindow_draw(2, &upt_rect);
*/
	RedrawWindow();
	UpdateWindow();
	((CNCLDDform*)m_parent)->UpdateDDFormView();
				}
			}
		}
		return 0;
	}
	return 1;
}
/***********************************************************************
**
**   FUNCTION: Reset_selrec(CRect rect)
**			reset selection dot position 
**		
**	 INPUT:  rect: new selection rect
**					
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDPicWin::Reset_selrec(CRect rect) 
{
	m_selsize[0].left = rect.left - BARLEN;
	m_selsize[0].right = rect.left;
	m_selsize[0].top = rect.top - BARLEN;
	m_selsize[0].bottom = rect.top;
	m_selsize[1].left = rect.left - BARLEN;
	m_selsize[1].right = rect.left;
	m_selsize[1].top = rect.top+rect.Height()/2 - BARLEN;
	m_selsize[1].bottom = m_selsize[1].top + BARLEN;
	m_selsize[2].left = rect.left - BARLEN;
	m_selsize[2].right = rect.left;
	m_selsize[2].top = rect.bottom;
	m_selsize[2].bottom = m_selsize[2].top + BARLEN;
	m_selsize[3].left = rect.left + rect.Width()/2- BARLEN;
	m_selsize[3].right = m_selsize[3].left + BARLEN;
	m_selsize[3].top = rect.bottom;
	m_selsize[3].bottom = m_selsize[3].top + BARLEN;
	m_selsize[4].left = rect.right;
	m_selsize[4].right = rect.right + BARLEN;
	m_selsize[4].top = rect.bottom;
	m_selsize[4].bottom = m_selsize[4].top + BARLEN;
	m_selsize[5].left = rect.right;
	m_selsize[5].right = rect.right + BARLEN;
	m_selsize[5].top = rect.top + rect.Height()/2 - BARLEN;
	m_selsize[5].bottom = m_selsize[5].top + BARLEN;
	m_selsize[6].left = rect.right;
	m_selsize[6].right = rect.right + BARLEN;
	m_selsize[6].top = rect.top - BARLEN;
	m_selsize[6].bottom = m_selsize[6].top + BARLEN;
	m_selsize[7].left = rect.left  + rect.Width()/2- BARLEN;
	m_selsize[7].right = m_selsize[7].left + BARLEN;
	m_selsize[7].top = rect.top - BARLEN;
	m_selsize[7].bottom = rect.top;
	DrawSelect();
}
/***********************************************************************
**
**   FUNCTION: DrawSelect()
**
**       drawing the cell as select
**
**   INPUT:  CDC* pDC: device context
**			
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDPicWin::DrawSelect()
{
	if (m_selsize[0].left==-1000)
		return;
	CClientDC dc(this);
	CBrush* pBrush;
	pBrush = new CBrush(RGB(0, 0, 100));

	CRect rect;
	dc.FillRect(&m_selsize[1], pBrush );
	dc.FillRect(&m_selsize[5], pBrush );		
	dc.FillRect(&m_selsize[0], pBrush );
	dc.FillRect(&m_selsize[2], pBrush );
	dc.FillRect(&m_selsize[3], pBrush );
	dc.FillRect(&m_selsize[4], pBrush );
	dc.FillRect(&m_selsize[6], pBrush );
	dc.FillRect(&m_selsize[7], pBrush );
	delete pBrush;

	if (m_resize==1)
	{
		CPen *aPen;
		aPen= new CPen(PS_DOT, 1, RGB(255,0,0));
/*
.....save old pen and select default pen for draw
*/
		CPen* oldPen = dc.SelectObject(aPen);
		dc.MoveTo(m_sizerec.left, m_sizerec.top);
		dc.LineTo(m_sizerec.left, m_sizerec.bottom);
		dc.LineTo(m_sizerec.right, m_sizerec.bottom);
		dc.LineTo(m_sizerec.right, m_sizerec.top);
		dc.LineTo(m_sizerec.left, m_sizerec.top);
		dc.SelectObject(oldPen);
		delete aPen;
	}
}

/***********************************************************************
**
**   FUNCTION: DrawSizeRect(CDC *dc, CRect &rect, int vis)
**		Draw Size Rect
**   INPUT:  dc:
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDPicWin::DrawHotspotRect(CDC *dc, CRect &rect)
{
	CPen *aPen;
	aPen= new CPen(PS_DOT, 1, RGB(0,0,200));
/*
.....save old pen and select default pen for draw
*/
	CPen* oldPen = dc->SelectObject(aPen);
	dc->MoveTo(rect.left, rect.top);
	dc->LineTo(rect.left, rect.bottom);
	dc->LineTo(rect.right, rect.bottom);
	dc->LineTo(rect.right, rect.top);
	dc->LineTo(rect.left, rect.top);
	dc->SelectObject(oldPen);
	delete aPen;
}

/***********************************************************************
**
**   FUNCTION: Reset_picture(char* filename)
**
**       Reset the picture using new image file
**
**   INPUT:  filename: new image file
**
**   OUTPUT :   None
**   RETURN:    none
**
**********************************************************************/
void CNCLDDPicWin::Reset_picture(char* filename)
{
	UX_pathname filen;
	strcpy(filen, filename);
	int status = ul_open_mod_file("UU_USER_SETTINGS", "forms", "UD_FORMDIR", (char*)UU_NULL, 
		filen, 0, (FILE**)UU_NULL);

	if (status==0)
		m_filename = filen;
	else
		m_filename = "";
	m_picture.Load(m_filename);
	m_picture.CreatePicBitmap(GetDC());
	Paint();
	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(2,-1,this);
	}
	m_prop_dlg->m_choices = m_filename;
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
void CNCLDDPicWin::OnDestroy() 
{
	if(m_TargetDrop)
	{
		m_TargetDrop->Revoke();
		delete m_TargetDrop;
	}
	m_TargetDrop = NULL;
	CWnd::OnDestroy();
}

/***********************************************************************
c
c   FUNCTION: OnPaint()
c
c         The framework calls this member function when Windows 
c			or an application makes a request to repaint a 
c			portion of an application's window. The WM_PAINT 
c			message is sent when the UpdateWindow or RedrawWindow 
c			member function is called.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDDPicWin::OnPaint() 
{
	CWnd::OnPaint();
	Paint();
	DrawSelect();
}

BOOL CNCLDDPicWin::OnEraseBkgnd(CDC* pDC) 
{
	return CWnd::OnEraseBkgnd(pDC);
//	return TRUE;
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
void CNCLDDPicWin::OnLButtonDown(UINT nFlags, CPoint point) 
{
	m_StartPoint = 	point;
	m_TimerID = SetTimer(1, 100, NULL);	

	int status = Handle_Hotspot_event(1, nFlags, point);
	if (status==0)
		return;

	CWnd::OnLButtonDown(nFlags, point);
	RECT rectClient;
	GetClientRect(&rectClient);
	int cx, cy;
	cx = rectClient.right - rectClient.left;
	cy =  rectClient.bottom -  rectClient.top;
/*
......draw the select box and select this item
*/
	int type = 2;
	int itype = -1;
	int flag;
	if (nFlags&MK_CONTROL)
		flag = 1;
	else if (nFlags&MK_SHIFT)
		flag = 2;
	else
		flag = 0;
	((CNCLDDform*)m_parent)->OnSelectItem(m_itemno, type, itype, flag);
	ClientToScreen(&point);
	((CNCLDDform*)m_parent)->ScreenToClient(&point);
	((CNCLDDform*)m_parent)->HandleButtonDown(nFlags, point);
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
void CNCLDDPicWin::OnLButtonUp(UINT nFlags, CPoint point) 
{
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;
	
	if(m_TimerID)
	{
		KillTimer(m_TimerID);
		m_TimerID = 0;
	}
	CWnd::OnLButtonUp(nFlags, point);
	int status = Handle_Hotspot_event(2, nFlags, point);
	if (status==0)
		return;
/*
......draw the select box and select this item
*/
	ClientToScreen(&point);
	((CNCLDDform*)m_parent)->ScreenToClient(&point);
	((CNCLDDform*)m_parent)->HandleButtonup(nFlags, point);
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
void CNCLDDPicWin::OnRButtonUp(UINT nFlags, CPoint point) 
{
	if (m_parent==NULL)
		return;
	CMenu pmenu;
	pmenu.CreatePopupMenu();
		
//	pmenu.AppendMenu (MF_ENABLED, ID_POPUP_EDIT_ITEM, _T("Properties"));
	pmenu.AppendMenu (MF_ENABLED, ID_POPUP_DELETE, _T("Delete"));
	pmenu.AppendMenu (MF_ENABLED, ID_POPUP_CANCEL, _T("Cancel"));

	POINT pt;
	GetCursorPos(&pt);
	UINT cmdid = pmenu.TrackPopupMenu(TPM_LEFTBUTTON|TPM_LEFTALIGN|TPM_RETURNCMD|TPM_RECURSE,
					pt.x, pt.y, this, NULL);
	int type, flag;
	int itype = -1;
	if (cmdid==ID_POPUP_EDIT_ITEM)
	{
		flag = 0;
		type = 2; 
		((CNCLDDform*)m_parent)->OnSelectItem(m_itemno, type, itype, flag);
	}
	if (cmdid==ID_POPUP_EDIT_ITEM)
/*
......display property form
*/
	{
		if (m_prop_dlg==NULL)
		{
			m_prop_dlg = new CNCLFormProp(2,-1,this);
		}
/*
......get size
*/
		RECT rectClient;
		GetWindowRect(&rectClient);	
		m_parent->ScreenToClient(&rectClient);
		m_prop_dlg->m_size[0] = rectClient.right - rectClient.left;
		m_prop_dlg->m_size[1] = rectClient.bottom - rectClient.top;
		m_prop_dlg->m_input_itemno = m_itemno;
/*
......just open the parent level property page
*/
		((CNCLDDform*)m_parent)->OpenPropertyPage(m_prop_dlg); 
	}
	else if (cmdid==ID_POPUP_DELETE)
/*
......delete this item
*/
	{
		type = 2;
		((CNCLDDform*)m_parent)->SetActionItem(3,m_itemno, type);
		((CNCLDDform*)m_parent)->PostMessage(WM_COMMAND, ID_DELETE_FRMITEM);
	}
done:;
	CWnd::OnRButtonUp(nFlags, point);
}
void CNCLDDPicWin::SetProperty(CNCLFormProp *prop_dlg)
{
	RECT rectClient;
	GetWindowRect(&rectClient);	
	m_parent->ScreenToClient(&rectClient);
/*
.....copy all prop_dlg date into the member value m_prop_dlg
*/
	CopyPropertyPage(prop_dlg, m_prop_dlg);	
	rectClient.left = m_prop_dlg->m_pos[0];
	rectClient.top = m_prop_dlg->m_pos[1];
	rectClient.right = rectClient.left + m_prop_dlg->m_size[0];
	rectClient.bottom = rectClient.top + m_prop_dlg->m_size[1];
	MoveWindow(&rectClient);
/*
......filename is saved in m_prop_dlg->m_choices string
*/
	UX_pathname fnam;
	int nc = m_prop_dlg->m_choices.GetLength();
	char *temp = m_prop_dlg->m_choices.GetBuffer(nc);
	strcpy_s(fnam, UX_MAX_PATH_LEN, temp);
	Reset_picture(fnam);
	UpdateWindow();
	((CNCLDDform*)m_parent)->UpdateDDFormView();
	nc = m_prop_dlg->m_label.GetLength();
	temp = m_prop_dlg->m_label.GetBuffer(nc);
	strncpy(m_name, temp,39);
	m_name[39] = '\0';
	m_itemno = m_prop_dlg->m_input_itemno;
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
void CNCLDDPicWin::OnMouseMove(UINT nFlags, CPoint point) 
{
	if ((m_buttondown==1)&&(m_resize==1)&&(m_sizedir!=HT_NONE)&&(m_sizedir!=HT_INSIDE))
	{
		SetCursor(m_cursor[m_sizedir]);
	}
	else
	{
		m_hittest = HT_NONE;
		if (m_selsize[1].PtInRect(point))
			m_hittest = HT_LEFT;
		if (m_selsize[5].PtInRect(point))
			m_hittest = HT_RIGHT;
		if (m_selsize[0].PtInRect(point))
			m_hittest = HT_TOPLEFT;
		if (m_selsize[2].PtInRect(point))
			m_hittest = HT_BOTTOMLEFT;
		if (m_selsize[3].PtInRect(point))
			m_hittest = HT_BOTTOM;
		if (m_selsize[4].PtInRect(point))
			m_hittest = HT_BOTTOMRIGHT;
		if (m_selsize[6].PtInRect(point))
			m_hittest = HT_TOPRIGHT;
		if (m_selsize[7].PtInRect(point))
			m_hittest = HT_TOP;
		if (m_hotspot_rect.PtInRect(point))
			m_hittest = HT_INSIDE;
		switch (m_hittest)
		{
			case HT_NONE:
			case HT_INSIDE:
			case HT_TOPLEFT:
			case HT_LEFT:
			case HT_BOTTOMLEFT:
			case HT_BOTTOM:
			case HT_BOTTOMRIGHT:
			case HT_RIGHT:
			case HT_TOPRIGHT:
			case HT_TOP:
				SetCursor(m_cursor[m_hittest]);
		}
	}
	int status = Handle_Hotspot_event(5, nFlags, point);
	if ((status==0)||(m_resize==1))
	{
		CWnd::OnMouseMove(nFlags, point);
		if (m_resize==1)
			return;
		if ((m_hittest!=HT_INSIDE)&&(m_hittest!=HT_NONE))
			return;
		UW_form_hotspot_item = 1;
	}

	if(m_TimerID > 0)
	{

		int iX = m_StartPoint.x - point.x;
		int iY = m_StartPoint.y - point.y;
		if((iX*iX + iY*iY) > 9)
		{
			KillTimer(m_TimerID);
			m_TimerID = 0;
			CNCLDropSource* pdropsource = new CNCLDropSource();
			COleDataSource*	pSource = new COleDataSource();
			if(pSource)
			{
/*
.....Get the drag image list
*/
				if (m_DragAllImage!=NULL)
					delete m_DragAllImage;
				RECT rectClient;
				int cx, cy;

				m_DragAllImage = new CImageList();		
				HBITMAP bitmap2 = ((CNCLDDform*)m_parent)->CreateAllToBitmap(&cx, &cy, m_itemno);
				CBitmap *defImage2, defcBmap2;
				defImage2 = defcBmap2.FromHandle(bitmap2);
				m_DragAllImage->Create(cx,cy, ILC_COLOR32|ILC_MASK, 0, 1000);
				m_DragAllImage->Add(defImage2, RGB(0,0,0));

				if (m_DragAllImage!=NULL)
					pdropsource->SetImageList(m_DragAllImage);
				else
					pdropsource->SetImageList(m_DragImage);
/*
......we need adjust the drag point since the drag image now is draw
......for the whole CNCLDDform, not just the button
.....adject point from client point of button to client to the CNCLDDform
*/
				if (UW_form_hotspot_item==0)
				{
					GetWindowRect(&rectClient);
					((CNCLDDform*)m_parent)->ScreenToClient(&rectClient);
					CPoint point2 = point;
					point2.x += rectClient.left;
					point2.y += rectClient.top;
					pdropsource->SetDragPt(point2);
				}
				else
				{
					CRect prect;
					GetWindowRect(&prect);
					ScreenToClient(&prect);
/*
......need adjust the drop point
*/
					int dx = m_hotspot_rect.left - prect.left;
					int dy = m_hotspot_rect.top - prect.top;

					rectClient = m_hotspot_rect;
					ClientToScreen(&rectClient);
					((CNCLDDform*)m_parent)->ScreenToClient(&rectClient);
					CPoint point2 = point;
					point2.x += rectClient.left - dx;
					point2.y += rectClient.top - dy;
					pdropsource->SetDragPt(point2);
				}
				CSharedFile	sf(GMEM_MOVEABLE|GMEM_DDESHARE|GMEM_ZEROINIT);
				CString iText = m_filename;

				char tempstr[80];
				CString itext2;

				ClientToScreen(&point);
				sprintf(tempstr, "%d %d %d ", m_itemno, point.x, point.y);
				itext2 = tempstr;
				iText = "Move CNCLDDPicWin " + itext2 + iText;
				pdropsource->SetAttWindow(this, point, rectClient);

				//	write name to clipboard
				sf.Write(iText, iText.GetLength());

				HGLOBAL hMem = sf.Detach();
				if (!hMem) 
					return;
				pSource->CacheGlobalData(CF_TEXT, hMem);

				//	Do drag and drop!
				pSource->DoDragDrop(DROPEFFECT_MOVE|DROPEFFECT_COPY, NULL, pdropsource);

				//	free source
				delete pdropsource;
				delete pSource;
			}
		}
	}
	UW_form_hotspot_item = 0;
	CWnd::OnMouseMove(nFlags, point);
	ClientToScreen(&point);
	((CNCLDDform*)m_parent)->ScreenToClient(&point);
	((CNCLDDform*)m_parent)->HandleMouseMove(nFlags, point);
}

/***********************************************************************
c
c   FUNCTION: DrawGuides(CPoint org_pt, POINT ptCursor)
c
c       This function draw Guides line
c
c   INPUT:  org_pt: orginal point
c			ptCursor: current cursor point
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDDPicWin::DrawGuides(CPoint org_pt, POINT ptCursor)
{
	int delta_x = ptCursor.x - org_pt.x;
	int delta_y = ptCursor.y - org_pt.y;
	CRect rect;
	GetWindowRect(&rect);
	rect.top += delta_y;
	rect.left += delta_x;
	rect.bottom += delta_y;
	rect.right += delta_x;
	((CNCLDDform*)m_parent)->DrawGuides(rect);
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
void CNCLDDPicWin::OnSize(UINT nType, int cx, int cy)
{
	CWnd::OnSize(nType, cx, cy);

	RECT rectClient;
	GetWindowRect(&rectClient);	
	m_parent->ScreenToClient(&rectClient);

	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(2,-1,this);
	}
/*
......set size
*/	
	m_prop_dlg->m_size[0] = rectClient.right - rectClient.left;
	m_prop_dlg->m_size[1] = rectClient.bottom - rectClient.top;
	m_prop_dlg->m_input_itemno = m_itemno;
/*
......set pos
*/	
	m_prop_dlg->m_pos[0] = rectClient.left;
	m_prop_dlg->m_pos[1] = rectClient.top;

	if (m_dragimg_create)
	{
		delete m_DragImage;
		m_DragImage = new CImageList();
		m_dragimg_create = 0;
/*
		HBITMAP bitmap = CopyWindowToBitmap(&rectClient, this);
		CBitmap *defImage, defcBmap;
		defImage = defcBmap.FromHandle(bitmap);
		m_DragImage->Create(cx,cy, ILC_COLOR32|ILC_MASK, 0, 1000);
		m_DragImage->Add(defImage, 0xC0C0C0);
*/
	}		
/*
.....updated the property page only if it is the select item
*/
	int type, sel;
	type = 2;
	sel = ((CNCLDDform*)m_parent)->IsSelItem(m_itemno, type, -1);
	if (sel)
		((CNCLDDform*)m_parent)->UpdatePropertySize(m_prop_dlg->m_size[0], m_prop_dlg->m_size[1]);
}
/***********************************************************************
**
**   FUNCTION: OnMove(int x, int y)
**
**		this member function will be called
**		after the window's posiiton has changed. 
**   
**	 INPUT:  
**			  x:   Specifies the new x of position.
**			  y:   Specifies the new y of position.
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDPicWin::OnMove(int x, int y)
{
	CWnd::OnMove(x, y);
	CRect rectClient;
	GetWindowRect(&rectClient);	
	m_parent->ScreenToClient(&rectClient);
	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(2,-1,this);
	}
/*
......set pos
*/	
	m_prop_dlg->m_pos[0] = rectClient.left;
	m_prop_dlg->m_pos[1] = rectClient.top;
/*
.....updated the property page only if it si the select item
*/
	int type, sel;
	type = 2;
	sel = ((CNCLDDform*)m_parent)->IsSelItem(m_itemno, type, -1);
	if (sel)
		((CNCLDDform*)m_parent)->UpdatePropertyPos(rectClient.left, rectClient.top);
}

/***********************************************************************
**
**   FUNCTION: OnDragDropCallback(CPoint pt, char *input_text)
**
**         Called by the the mouse drop point is on the control
**
**   INPUT:  pt: current cursor point (drop point)
**			input_text: the text string data contains draging window information
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDDPicWin::OnDragDropCallback(CPoint pt, char *input_text)
{
	char *tok, datastr[100];
	RECT rect;
	strcpy(datastr, input_text);

	tok = strtok(datastr, " \t\r\n");
	if (stricmp(tok, "Move")!=0)
	{
		return;
	}
	if (strcmp(tok, "Move")==0)
	{
		tok = strtok(NULL, " \t\r\n");
		int display = 0;
		if (stricmp(tok, "CNCLDDPicWin")==0)
		{
			if (UW_form_hotspot_item)
			{
				((CNCLDDform*)m_parent)->OnDragDropHotSpotCallback(pt, input_text);
				return;
			}
		}
	}
	((CNCLDDform*)m_parent)->OnDragDropCallback(pt, input_text);
}
/***********************************************************************
**
**   FUNCTION: :SetItemNo(int itemno)
**
**         Set this control's item number
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDDPicWin::SetItemNo(int itemno)
{
	m_itemno = itemno;
	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(2,-1,this);
	}
	m_prop_dlg->m_input_itemno = itemno;
}

void CNCLDDPicWin::Reset_picture_area()
{
	ReseteHotSpotRect();
	RedrawWindow();
	UpdateWindow();
	((CNCLDDform*)m_parent)->UpdateDDFormView();
}
