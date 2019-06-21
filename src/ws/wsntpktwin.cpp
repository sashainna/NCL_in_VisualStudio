/********************************************************************* 
**  NAME:  wsntpktwin.cpp
**
**			Native WinNT pocket window functions
**			implementation of CNCLPockWin class functions
**	CONTAINS: CNCLPockWin class functions
**			all functions declared in wsntpktwin.h
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntpktwin.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			04/05/18 , 14:59:32
*********************************************************************/

#include "usysdef.h"
#include "go1.h"
#include "mpocket.h"
#include "dmotif.h"
#include "wsntstdafx.h"
#include "wsntpktwin.h"
#include "wsgl.h"
#include "wsntglfunc.h"
#include "wsntcfunc.h"
#include "wsglfun.h"
#include "zkeysym.h"
#include "wsntframe.h"
#include "wsntdoc.h"
#include "wsntview.h"
#include "dselect.h"
#include "mpocket.h"

extern CMainFrame *NCL_MainFrame;
CClientDC *uw_curIPVhDC;
CWnd *NCL_CurrentPKT_View;
HWND UW_NTgraphicIPV;
char UW_cur_screen[20];
extern "C" int NCL_animation;

#define TIP_ACTIVE
extern "C" int UV_dynview_active, UV_dynwheel;
extern int UU_SM_capture;
extern CDialog *Pocket_Win[UM_MAX_POCKET];
extern "C" int PKx,PKy, GP_PKx, GP_PKy;
extern "C" int UM_swap_ipv;
extern "C" void ipv_resize_window();
extern "C" int LW_active;
extern "C" int NCL_swap_changed;
extern "C" int UW_rubber_active;
extern "C" int NCL_pick_verify, UD_picking_active;
extern "C" int uw_glget_piksurf(int x, int y, char *text,int flag);
extern "C" int WS_update_tip;
extern "C" void uw_gldraw_pickfm();
extern "C" int uw_gldrawbuffer(Gbuffer);

extern "C" int UW_pocket_mode;
extern "C" int UW_pocket_pos[2], UW_pocket_size[2];
extern "C" char UW_pocket_title[256];
extern "C" char LW_ipv_title[256];
extern "C" int LW_ipv_pos[2], LW_ipv_size[2];
extern "C" int uw_ntsave_prect(UM_pkwin_type type, int left, int top, int right, int bottom);
extern "C" void uw_ntset_context(UM_pkwin_type which, UU_LOGICAL force);
extern BOOL WINAPI uw_ntpaintDIB(HDC hDC, LPRECT lpDCRect, HDIB hDIB, 
					 LPRECT lpDIBRect, HPALETTE hPal);
extern BOOL WINAPI uw_saveDIB(HDIB hDib, HANDLE file);
extern HDIB WINAPI  uw_cywin_DIB(HWND hWnd, int size, int fit, int bcolor);
extern DWORD     WINAPI  DIBWidth (LPSTR lpDIB);
extern DWORD     WINAPI  DIBHeight (LPSTR lpDIB);

extern HDIB WINAPI uw_cypixels_DIB(unsigned char *pixels, int width, int height, int size, int paper_size, int fit, int bcolor);
extern short SetDefaultPrinterOrientation(short dmOrientation);

extern int UW_view_hit;
extern CWnd *NCL_Main_View;
extern "C" ACCEL *UZ_ncl_accel;
extern "C" int UZ_ncl_accelnum;
extern "C" int ug_save_input(int** save_inptr_pt);
extern "C" int ug_reset_input(int* save_inptr);
extern "C" int uw_ntgetcur_cursor();
#define HILITE_MARK 0
#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3

extern "C" int NCL_mouse_func;
extern "C" int UW_live_mouse;
extern  "C" int ud_ifpopup();
extern "C" int uz_mouse_functions(char*, int);
extern "C" int ul_ipv_view_active();

extern "C" int NCL_mark_method;
extern "C" int ud_setpick_type(int);
extern "C" int ud_getpick_type();
extern "C" int ud_updatews(int);
extern "C" void um_dwpocket_reset(int);
extern "C" void um_set_screen_area(UM_pkwin_type);
extern "C" UM_pkwin_type um_get_screen_area();
extern "C" int UZ_nclipv_view;
extern "C" void uw_ntget_ecurpos(int *start, int *end);
extern "C" char * uu_malloc(int);
extern "C" void uu_free(char*);
extern "C" void uw_glset_context(UM_pkwin_type which, UU_LOGICAL force);
extern "C" void uw_glset_dirty_flag(UU_LOGICAL flag);

extern "C" void  um_set_pocket_graphics(UM_pkwin_type type);
extern "C" void um_reset_pocket_graphics(int type);
extern "C" void ul_ipv_view_inactive();
/***********************************************************************
**
**   FUNCTION: CNCLPockWin(CWnd* pParentWnd, UM_pkwin_type)
**
**              Constructor of class CNCLPockWin
**
**   INPUT:  pParentWnd: parent window
**           type    = Type of window to open.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLPockWin::CNCLPockWin(CWnd* pParentWnd, UM_pkwin_type type) : CDialog(CNCLPockWin::IDD, pParentWnd)
{
	m_cPalette = NULL;
	m_Wtype = type;
	m_palDIB   = new CPalette;
	m_hDIB	   = NULL;
	m_current_cursor = LoadCursor(NULL, IDC_ARROW);
	m_tiptext = new char[80];
	m_tiptext[0] = '\0';
	m_x = m_y = -1;
	m_mpoint.x = m_mpoint.y = -1;
	m_timer = 0;
	m_pcenter = 0;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLPockWin
**
**              Destructor of class CNCLPockWin
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLPockWin::~CNCLPockWin()
{
	if (m_palDIB != NULL)
	{
		delete m_palDIB;
		m_palDIB = NULL;
	}
	if (m_tiptext!=NULL)
		delete m_tiptext;
	if( m_hDIB != NULL )
    {
        ::GlobalUnlock(m_hDIB);
	    ::GlobalFree(m_hDIB);
	    m_hDIB = NULL;
	}  
}

void CNCLPockWin::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNCLPockWin)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CNCLPockWin, CDialog)
	//{{AFX_MSG_MAP(CNCLPockWin)
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_SIZE()
	ON_WM_ERASEBKGND()
	ON_WM_NCHITTEST ()
	ON_WM_PAINT()
	ON_WM_NCPAINT()
	ON_WM_NCACTIVATE()
    ON_WM_NCLBUTTONDOWN() 
    ON_WM_NCLBUTTONUP() 
	ON_WM_MOUSEMOVE()
//
	ON_WM_LBUTTONDOWN()
	ON_WM_MBUTTONDOWN()
	ON_WM_RBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_MBUTTONUP()
	ON_WM_RBUTTONUP()
	ON_WM_MOUSEWHEEL()
//
	ON_WM_TIMER()
	ON_COMMAND_RANGE(WM_APP+UDM_MAX_MENU, WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM-1, OnAccelFunctions)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
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
c				HTBORDER   In the border of a window that does not have a sizing border.
c				HTBOTTOM   In the lower horizontal border of the window.
c				HTBOTTOMLEFT   In the lower-left corner of the window border.
c				HTBOTTOMRIGHT   In the lower-right corner of the window border.
c				HTCAPTION   In a title-bar area.
c				HTCLIENT   In a client area.
c				HTERROR   On the screen background or on a dividing line between windows (same as HTNOWHERE except that the DefWndProc Windows function produces a system beep to indicate an error).
c				HTGROWBOX   In a size box.
c				HTHSCROLL   In the horizontal scroll bar.
c				HTLEFT   In the left border of the window.
c				HTMAXBUTTON   In a Maximize button.
c				HTMENU   In a menu area.
c				HTMINBUTTON   In a Minimize button.
c				HTNOWHERE   On the screen background or on a dividing line between windows.
c				HTREDUCE   In a Minimize button.
c				HTRIGHT   In the right border of the window.
c				HTSIZE   In a size box (same as HTGROWBOX).
c				HTSYSMENU   In a Control menu or in a Close button in a child window.
c				HTTOP   In the upper horizontal border of the window.
c				HTTOPLEFT   In the upper-left corner of the window border.
c				HTTOPRIGHT   In the upper-right corner of the window border.
c				HTTRANSPARENT   In a window currently covered by another window.
c				HTVSCROLL   In the vertical scroll bar.
c				HTZOOM   In a Maximize button. 
c
**********************************************************************/
LRESULT CNCLPockWin::OnNcHitTest( CPoint point )
{	
	if (m_current_cursor!=NULL)
	{
		::SetCursor(m_current_cursor);
	}
	UW_view_hit = 0;
	CWnd *win = GetCapture();
	if ((this != GetCapture()) && (win!=NULL))
		return CDialog::OnNcHitTest(point); 

	CPoint pt=point;
	ScreenToClient(&pt);
	if (IsIconic()==0)
	{
		pt.y += GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);
	}
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
/*			{
			DrawFrameControl(dc.m_hDC,
				m_rcClose,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE | pushed);
			}
*/		break;
	case DHT_MIN:
/*		{
		DrawFrameControl(dc.m_hDC,
				m_rcMin,
				DFC_CAPTION,
				DFCS_CAPTIONMIN | pushed);
		}
*/		break;
	default:
/*		DrawFrameControl(dc.m_hDC,
			m_rcMin,
			DFC_CAPTION,
			DFCS_CAPTIONMIN);
		if (m_Wtype != UM_IPV_WINDOW)
			DrawFrameControl(dc.m_hDC,
				m_rcClose,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE);
*/		break;
	}
	return hitTest;
}

/*********************************************************************
**    I_FUNCTION : SetCursor(HCURSOR cursor)
**
**	         Set the windows cursor as requested.
**			
**    PARAMETERS   
**       INPUT  : 
**          cursor: cursor to be set
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLPockWin::SetCursor(HCURSOR cursor)
{
	m_current_cursor = cursor;

	if (UW_view_hit!=0)
		return;
	CPoint point;
	GetCursorPos(&point);
	ScreenToClient(&point);
/*
.....if the point is inside window and the cursor is different, set cursor
*/
	int cx, cy;
	if (UM_swap_ipv==1)
	{
		cx = GP_PKx; 
		cy = GP_PKy;
	}
	else
	{
		cx = PKx; 
		cy = PKy;
	}
	if ((m_current_cursor!=NULL)&&(point.x>0) && (point.y>0) 
				&& (point.x<cx) && (point.y<cy))
	{
		::SetCursor(m_current_cursor);
	}
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
int CNCLPockWin::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CDialog::OnCreate(lpCreateStruct) == -1)
		return -1;
	NCL_CurrentPKT_View = this;
	m_pDC = new CClientDC(this);
	uw_curIPVhDC = m_pDC;

    ASSERT(m_pDC != NULL);
/*
......initialize OpenGL
*/
	Init();		
	return 0;
}

/***********************************************************************
c
c   FUNCTION: GetWin()
c
c         Get the this graphic window's handler
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    graphic window's handler
c
**********************************************************************/
char * CNCLPockWin::GetWin()
{
	HWND hWndParent = this->GetSafeHwnd();
	return (char *)hWndParent;
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
void CNCLPockWin::OnPaint() 
{
	CDialog::OnPaint();

	UM_pkwin_type sarea;
	sarea = um_get_screen_area();	
/*
.....if dynamic view is on, we can't doing redraw because
.....first, not neccesary since the 'dynamic view' will redraw
.....second, when double buffering for dynamic view, the glFlush here
.....will flush the screen and make parts shaky.
.....Yurong
*/
	if (UV_dynview_active || IsIconic()) 
	{
		return;
	}
//test
	if (NCL_animation)
		return;/*
.....NCLIPV 
*/
	NCL_CurrentPKT_View = this;
	if (m_Wtype == UM_IPV_WINDOW)
	{
		if (UM_swap_ipv==0)
		{
			UZ_nclipv_view = 1;
			um_set_screen_area(UM_IPV_WINDOW);
			ipv_resize_window();
		}
		else
		{
//			if (current_glctx != 7)
			{
				uw_ntset_context(UM_NCL_WINDOW,1);
				current_glctx = 7;
			}
			um_set_screen_area(UM_NCL_WINDOW);
			UZ_nclipv_view = 0;
			uw_glresize_graphics(m_oldRect.right, m_oldRect.bottom, 1);
			uw_gldraw_pickfm();
			ud_updatews(UG_SUPPRESS);
		}
	}
	else if (m_Wtype == UM_NCL_WINDOW)
	{
/*
.....paint NCL in pocket window
*/
    	GetClientRect(&m_oldRect);
    	GetWindowRect(&m_oldSRect);
		NCL_CurrentPKT_View = this;

		if (current_glctx != 0)
		{
			uw_ntset_context(UM_NCL_WINDOW,UU_FALSE);
			current_glctx = 0;
		}
		um_set_screen_area(UM_NCL_WINDOW);
		UZ_nclipv_view = 0;
		uw_glresize_graphics(m_oldRect.right, m_oldRect.bottom, 1);
		uw_gldraw_pickfm();
		ud_updatews(UG_SUPPRESS);
	}
/*
.....Save the current screen
*/
	else
	{
		uv_current_screen_name(UW_cur_screen);
/*
.....paint draw in pocket window
*/
    	GetClientRect(&m_oldRect);
    	GetWindowRect(&m_oldSRect);
		NCL_CurrentPKT_View = this;

		if (current_glctx != 6)
		{
			uw_ntset_context(UM_NCL_WINDOW,UU_FALSE);
			current_glctx = 6;
		}

		uw_ntpaint_pocket(m_oldRect.right, m_oldRect.bottom);
/*
.....then disable pocket view to allow main graphic
.....window to draw
*/
		uw_ntset_maingw(UW_cur_screen);
	}
	OnNcPaint();
	um_set_screen_area(sarea);
	UZ_nclipv_view = 0;
}

/***********************************************************************
**
**   FUNCTION: OnDestroy() 
**
**		OnDestroy is called after the CNCLPockWin object 
**		is removed from the screen. 
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLPockWin::OnDestroy() 
{
    ::wglMakeCurrent(NULL,  NULL);

    if (m_hrc)
        ::wglDeleteContext(m_hrc);

    if (m_pDC)
        delete m_pDC;
	CDialog::OnDestroy();
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
void CNCLPockWin::OnSize( UINT nType, int cx, int cy )
{
	CWnd::OnSize(nType, cx, cy );
	if (IsIconic())
	{
		return;
	}
	NCL_CurrentPKT_View = this;
	if (UM_swap_ipv==0)
	{
		PKx = cx ; 
		PKy = cy;
	}
	else
	{
		GP_PKx = cx ; 
		GP_PKy = cy;
	}

	GetClientRect(&m_oldRect);
	GetWindowRect(&m_oldSRect);
/*
.....following statement to avoid mutiple_repaint when swith IPV/Graphic Window
.....Yurong 7/25/03
*/
	if (NCL_swap_changed)
	{
		return;
	}
	UM_pkwin_type sarea;
	sarea = um_get_screen_area();	

	if (m_Wtype == UM_IPV_WINDOW)
	{
		if (LW_active == 1)
		{
			if (UM_swap_ipv==0)
			{
				RECT rect;
				GetWindowRect(&rect);
				um_set_screen_area(UM_IPV_WINDOW);
				ipv_resize_window();
				uw_ntsave_prect(m_Wtype, rect.left, rect.top, rect.right, rect.bottom);
			}
			else
			{
		    	GetClientRect(&m_oldRect);
				GetWindowRect(&m_oldSRect);
//				if (current_glctx != 7)
				{
					uw_ntset_context(UM_NCL_WINDOW,1);
					current_glctx = 7;
				}
				um_set_screen_area(UM_NCL_WINDOW);
				uw_glresize_graphics(m_oldRect.right, m_oldRect.bottom, 1);
			}
		}
	}
	else if (m_Wtype == UM_NCL_WINDOW)
	{
/*
.....paint NCL in pocket window
*/
    	GetClientRect(&m_oldRect);
		GetWindowRect(&m_oldSRect);

		if (current_glctx != 0)
		{
			uw_ntset_context(UM_NCL_WINDOW,UU_FALSE);
			current_glctx = 0;
		}
		um_set_screen_area(UM_NCL_WINDOW);
		uw_glresize_graphics(m_oldRect.right, m_oldRect.bottom, 1);
	}
	else
	{
		if(cy > 0)
		{    
			if (current_glctx != 6)
			{
				wglMakeCurrent(m_pDC->GetSafeHdc(), m_hrc);
				current_glctx = 6;
			}
			OnPaint();
		}
	}
	um_set_screen_area(sarea);
}

/***********************************************************************
**
**   FUNCTION: OnEraseBkgnd(CDC* pDC) 
**			The framework calls this member function 
**			when the CNCLView object background needs 
**			erasing (for example, when resized). 
**		    It is called to prepare an invalidated 
**			region for painting.
**		    Because we don't don't window erase the 
**			graphic for us, we will erase it in openGL
**			function, so simply return;
**   
**	 INPUT:  CDC* pDC: device context
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLPockWin::OnEraseBkgnd(CDC* pDC) 
{
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: Init() 
**
**		initialize openGL
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLPockWin::Init()
{
	int size[2];

	if ((m_Wtype == UM_DRAWING_WINDOW) || ((m_Wtype == UM_IPV_WINDOW) &&
		(UM_swap_ipv)) )
	{
		uw_glinit_visual();

		m_hrc = wglCreateContext(m_pDC->GetSafeHdc());
		wglMakeCurrent(m_pDC->GetSafeHdc(), m_hrc);
		if (m_Wtype == UM_DRAWING_WINDOW)
			current_glctx = 6;
		else
			current_glctx = 7;
		if ((m_Wtype == UM_IPV_WINDOW) && (UM_swap_ipv))
		{
			GetWindowRect(&m_oldSRect);
		    GetClientRect(&m_oldRect);
			size[0] = m_oldRect.right;
			size[1] = m_oldRect.bottom;
			uw_glglxinit(size);
		}
		uw_ntcreate_font(1);
		GetWindowRect(&m_oldSRect);
		GetClientRect(&m_oldRect);
		uw_gldrawbuffer(UG_BACK_BUFFER);
		glClearDepth_d(1.0f);
		glEnable_d(GL_DEPTH_TEST);
	}
	else if ((m_Wtype == UM_IPV_WINDOW) && (!UM_swap_ipv))
	{
		UW_pocket_mode = UU_TRUE;
		uw_glinit_visual();
		m_hrc = wglCreateContext(m_pDC->GetSafeHdc());
		wglMakeCurrent(m_pDC->GetSafeHdc(), m_hrc);
		current_glctx = 6;
		glClearDepth_d(1.0f);
		glEnable_d(GL_DEPTH_TEST);
		UW_pocket_mode = UU_FALSE;
		uw_ntcreate_font(-1);
	}
	UW_NTgraphicIPV = m_hWnd;
}

/***********************************************************************
**
**   FUNCTION: inicolormap()
**
**		initial openGL index color map for pocket window
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int  CNCLPockWin::inicolormap()
{
	int         n;
	PIXELFORMATDESCRIPTOR pfd;

    if (!bSetupPixelFormat())
        return -1;
    n = ::GetPixelFormat(
		m_pDC->GetSafeHdc());
    ::DescribePixelFormat(m_pDC->GetSafeHdc(), n, sizeof(pfd), &pfd);
	return 0;
}

/***********************************************************************
**
**   FUNCTION: bSetupPixelFormat() 
**
**		Setup pixel format for openGL
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLPockWin::bSetupPixelFormat()
{
	static PIXELFORMATDESCRIPTOR pfdipv = 
	{
        sizeof(PIXELFORMATDESCRIPTOR),  // size of this pfd
        1,                              // version number
        PFD_DRAW_TO_WINDOW |            // support window
        PFD_SUPPORT_OPENGL |          // support OpenGL
        PFD_DOUBLEBUFFER,             // double buffered
        PFD_TYPE_RGBA,
        24,                             // 24-bit color depth
        0, 0, 0, 0, 0, 0,               // color bits ignored
        0,                              // no alpha buffer
        0,                              // shift bit ignored
        0,                              // no accumulation buffer
        0, 0, 0, 0,                     // accum bits ignored
        32,                             // 32-bit z-buffer
        8,                              // no stencil buffer
        0,                              // no auxiliary buffer
        PFD_MAIN_PLANE,                 // main layer
        0,                              // reserved
        0, 0, 0                         // layer masks ignont
    };
	static PIXELFORMATDESCRIPTOR pfdd = 
	{
        sizeof(PIXELFORMATDESCRIPTOR),  // size of this pfd
        1,                              // version number
        PFD_DRAW_TO_WINDOW |            // support window
        PFD_SUPPORT_OPENGL |          // support OpenGL
        PFD_DOUBLEBUFFER,             // double buffered
        PFD_TYPE_COLORINDEX,
        24,                             // 24-bit color depth
        0, 0, 0, 0, 0, 0,               // color bits ignored
        0,                              // no alpha buffer
        0,                              // shift bit ignored
        0,                              // no accumulation buffer
        0, 0, 0, 0,                     // accum bits ignored
        32,                             // 32-bit z-buffer
        8,                              // no stencil buffer
        0,                              // no auxiliary buffer
        PFD_MAIN_PLANE,                 // main layer
        0,                              // reserved
        0, 0, 0                         // layer masks ignont
    };

    int pixelformat;

	pfdd.cColorBits = 32;
	pfdd.iPixelType = PFD_TYPE_RGBA;
	pfdd.cStencilBits  = 8;
	pfdipv.cStencilBits  = 8;

	if (m_Wtype == UM_IPV_WINDOW)
	{
		if ( (pixelformat = ChoosePixelFormat(m_pDC->GetSafeHdc(), &pfdipv)) == 0 )
		{
			MessageBox("ChoosePixelFormat for NCLIPV failed");
			return FALSE;
		}

		if (SetPixelFormat(m_pDC->GetSafeHdc(), pixelformat, &pfdd) == FALSE)
		{
			MessageBox("SetPixelFormat for NCLIPV failed");
			return FALSE;
		}
	}
	else
	{
		if ( (pixelformat = ChoosePixelFormat(m_pDC->GetSafeHdc(), &pfdd)) == 0 )
		{
			MessageBox("ChoosePixelFormat double buffering failed");
			return FALSE;
		}

		if (SetPixelFormat(m_pDC->GetSafeHdc(), pixelformat, &pfdd) == FALSE)
		{
			MessageBox("SetPixelFormat double buffering failed");
			return FALSE;
		}
	}
    return TRUE;
}

/***********************************************************************
c
c   SUBROUTINE:  OnCancel
c   FUNCTION:  This function called when "Cancel" button pushed
c				it destroy and remove itself
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLPockWin::OnCancel()
{
	RECT rect;

	GetWindowRect(&rect);
	uw_ntsave_prect(m_Wtype, rect.left, rect.top, rect.right, rect.bottom);
/*
.....let the other routine know the Pocket
.....window is closed
*/
	Pocket_Win[m_Wtype] = NULL;
	NCL_CurrentPKT_View = NULL;
	CDialog::OnCancel();
	UW_pocket_mode = UU_FALSE;
}
/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize 
c				the pocket window
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CNCLPockWin::OnInitDialog()
{
	CDialog::OnInitDialog();
/*
.....set title
*/
	if (m_Wtype == UM_DRAWING_WINDOW)
	{
		SetWindowText(UW_pocket_title);
		MoveWindow(UW_pocket_pos[0],UW_pocket_pos[1], 
					UW_pocket_size[0],UW_pocket_size[1]);
	}
	else if (m_Wtype == UM_NCL_WINDOW)
	{
		SetWindowText("NCL Graphic Window");
		MoveWindow(LW_ipv_pos[0],LW_ipv_pos[1], 
					LW_ipv_size[0],LW_ipv_size[1]);
	}
	else
	{
		SetWindowText(LW_ipv_title);
		MoveWindow(LW_ipv_pos[0],LW_ipv_pos[1], 
					LW_ipv_size[0],LW_ipv_size[1]);
	}
#ifdef TIP_ACTIVE
	m_RectTips.Create(this);
#endif
/*
.....Create user key Accelerator
*/
	if (UZ_ncl_accelnum!=0)
		m_accel = CreateAcceleratorTable(UZ_ncl_accel, UZ_ncl_accelnum);

	if (m_Wtype != UM_DRAWING_WINDOW)
	{
		CMenu *menu = GetSystemMenu(FALSE);
		menu->EnableMenuItem(SC_CLOSE, MF_BYCOMMAND | MF_GRAYED);
	}
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  SetContext
c   FUNCTION:  This function set openGL context
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLPockWin::SetContext()
{
	wglMakeCurrent(m_pDC->GetSafeHdc(), m_hrc);
}
/***********************************************************************
c
c   FUNCTION: PrintIPVDIB()
c
c     Print DIB on a printer
c
c   INPUT:  printDC: printer device context
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLPockWin::PrintIPVDIB(CDC* printDC)
{
	if(m_hDIB == NULL || m_palDIB == NULL) return;

	int cxPage = printDC->GetDeviceCaps(HORZRES);
	int cyPage = printDC->GetDeviceCaps(VERTRES);
	int cxInch = printDC->GetDeviceCaps(LOGPIXELSX);
	int cyInch = printDC->GetDeviceCaps(LOGPIXELSY);
	
	char* pDIB = (char*)::GlobalLock(m_hDIB);
	int cxDIB  = (int)  ::DIBWidth(pDIB);
	int cyDIB  = (int)  ::DIBHeight(pDIB);
	if( cxDIB <= 0 || cyDIB <= 0 ) return;

	RECT rcDst, rcDIB;
	rcDIB.top    = 0;
	rcDIB.left	 = 0;
	rcDIB.right  = cxDIB;
	rcDIB.bottom = cyDIB;
	rcDst.left   = 0;
	rcDst.top    = 0;
	rcDst.right  = cxPage;
	rcDst.bottom = (int)(1.0 * cyDIB * cxPage * cyInch / cxDIB / cxInch);
	int shiftx, shifty;
	shiftx = 0;
	shifty = (cyPage - rcDst.bottom)/2;
/*
.....if printer is attached on CNCLView, so print to CNCLView device context
.....which pass in as parameter
.....not the pocket window context
*/
//	::uw_ntpaintDIB(pDC->m_hDC, &rcDst, (HDIB)m_hDIB, &rcDIB, (HPALETTE)m_palDIB->m_hObject);
	if (m_pcenter)
	{
		rcDst.left   = shiftx;
		rcDst.top    = shifty;
		rcDst.right  = cxPage + shiftx;
		rcDst.bottom = (int)(1.0 * cyDIB * cxPage * cyInch / cxDIB / cxInch) + shifty;
	}
	::uw_ntpaintDIB(printDC->m_hDC, &rcDst, (HDIB)m_hDIB, &rcDIB, (HPALETTE)m_palDIB->m_hObject);
}


/***********************************************************************
c
c   FUNCTION: SaveToBMP(char *filename)
c
c     Save current graphic view into a bitmap file
c
c   INPUT:  
c			filename: BITMAP filename
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLPockWin::SaveToBMP(char *filename)
{
	char msg[500];

	GetDIB();
	if( m_hDIB == NULL )
	{
		MessageBox("HDIB handle is NULL, save abort !!!");
		return;
	}
												   
	CFile file;
	CFileException fe;

	if (!file.Open(filename, CFile::modeCreate |
	  CFile::modeReadWrite | CFile::shareExclusive, &fe))
	{
		FreeDIB();
		sprintf_s(msg, sizeof(msg), "Cannot save the BMP file %s !!!", filename);
		MessageBox(msg);
		return;
	}

	BOOL bSuccess = FALSE;
	TRY
	{
		BeginWaitCursor();
		bSuccess = ::uw_saveDIB(m_hDIB, (HANDLE)file.m_hFile);
		file.Close();
	}
	CATCH (CException, eSave)
	{
		file.Abort(); // will not throw an exception
		EndWaitCursor();
		FreeDIB();
		sprintf_s(msg, sizeof(msg), "Cannot save the BMP file %s !!!", filename);
		MessageBox(msg);
		return;
	}
	END_CATCH
	EndWaitCursor();
	if (!bSuccess)  
	{
		FreeDIB();
		sprintf_s(msg, sizeof(msg), "Cannot save the BMP file %s !!!", filename);
		MessageBox(msg);
	}

	FreeDIB();
}

/***********************************************************************
c
c   FUNCTION: GetDIB()
c
c     Create member DIB from graphic window
c
c   INPUT:  
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLPockWin::GetDIB()
{
	if(m_hDIB != NULL)
	{
		::GlobalUnlock(m_hDIB);
		::GlobalFree(m_hDIB);
		m_hDIB = NULL;
	}
	RECT rect;
	GetClientRect(&rect);

	int width = rect.right - rect.left;
	int height = rect.bottom - rect.top;
	int size = width * height;
	unsigned char *pixels = NULL;

	uw_glset_context(UM_IPV_WINDOW,UU_TRUE);
	uw_glset_dirty_flag(UU_FALSE);
	glPixelStorei_d(GL_PACK_ALIGNMENT, 1);
	glReadBuffer_d(GL_BACK);
	pixels = (unsigned char *)uu_malloc(3*size*sizeof(unsigned char));
	glReadPixels_d(0,0, width, height, GL_RGB, GL_UNSIGNED_BYTE, pixels);

	m_hDIB = ::uw_cypixels_DIB(pixels, width, height, size, m_papersize, m_fit, m_bcolor);
	uu_free((char*)pixels);

	uw_glset_dirty_flag(UU_TRUE);
	uw_glset_context(UM_NCL_WINDOW, UU_TRUE);
	um_set_pocket_graphics(UM_NCL_WINDOW);
}

/***********************************************************************
c
c   FUNCTION: FreeDIB()
c
c     Free member DIB memory
c
c   INPUT:  
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLPockWin::FreeDIB()
{
	::GlobalUnlock(m_hDIB);
	::GlobalFree(m_hDIB);
	m_hDIB = NULL;
}

/***********************************************************************
c
c   FUNCTION: OnMouseMove(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the mouse cursor moves
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
void CNCLPockWin::OnMouseMove(UINT nFlags, CPoint point) 
{	
/*
.....if this pocket window is a graphic window, we need have verify mode for picking
*/
	if ((UM_swap_ipv==0)|| (NCL_mark_method != DYNAMIC_MARK))
	{
		CDialog::OnMouseMove(nFlags, point);
		return;
	}
/*
......when playback is on (geometry is moving)
......don't call picking verify function
*/
	if (NCL_animation)
	{
		CDialog::OnMouseMove(nFlags, point);
		return;
	}
	if (UW_rubber_active || (!((UD_picking_active) && (NCL_pick_verify))))
	{
		if (m_timer)
			KillTimer(1);
		m_x = -1; m_y=-1;
		m_mpoint.x = m_mpoint.y = -1;
		m_RectTips.Close();
		CDialog::OnMouseMove(nFlags, point);
		WS_update_tip = 0;
		return;
	}
/*
......if pick and verify active
*/
	if ((point.x==m_x) && (point.y==m_y) && (WS_update_tip==0))
	{
		if (m_timer==0)
		{
			SetTimer(1, 1000, NULL);
			m_timer = 1;
		}
		WS_update_tip = 0;
		CDialog::OnMouseMove(nFlags, point);
		return;
	}
	if (m_timer)
	{
		KillTimer(1);
		m_RectTips.Close();
	}
	SetTimer(1, 1000, NULL);
	m_timer = 1;
	m_mpoint.x = m_x = point.x; 
	m_mpoint.y = m_y = point.y;
#ifdef TIP_ACTIVE
	if ((UD_picking_active) && (NCL_pick_verify))
	{
		int stat = uw_glget_piksurf(point.x, m_oldRect.bottom - point.y, m_tiptext, 1);
		if (stat==-1)
			m_tiptext[0] = '\0';
	}
#endif
	CDialog::OnMouseMove(nFlags, point);
	WS_update_tip = 0;
}

/***********************************************************************
c
c   FUNCTION: OnTimer(UINT_PTR nIDEvent)
c
c       The timer callback function 
c
c   INPUT:  nIDEvent: Timer ID.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLPockWin::OnTimer(UINT_PTR nIDEvent) 
{	
	if ((UM_swap_ipv==0)|| (NCL_mark_method != DYNAMIC_MARK))
	{
		CDialog::OnTimer(nIDEvent);
		return;
	}

	if (!((UD_picking_active) && (NCL_pick_verify)))
	{
		if (m_timer)
		{
			KillTimer(1);
			m_RectTips.Close();
		}
		m_timer = 0;
		goto done;
	}
	if ((UD_picking_active) && (NCL_pick_verify))
	{
		if (m_tiptext[0]!='\0')
		{
			m_RectTips.SetText(m_tiptext);
			m_RectTips.Show(m_mpoint);
		}
	}
done:;
	CDialog::OnTimer(nIDEvent);

	// Eat spurious WM_TIMER messages
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_TIMER, WM_TIMER, PM_REMOVE));
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
BOOL CNCLPockWin::PreTranslateMessage(MSG* msg)
{
	HWND hWnd = (HWND)*this; 
	
	UINT message = msg->message;

	if (((UD_pickmode==1)||(UU_SM_capture))&&(message==ID_NCL_SMEVENT) && (UM_swap_ipv))
	{
		NCL_CurrentPKT_View->PostMessage(ID_NCL_SMEVENT, msg->wParam, msg->lParam);
		return TRUE;
	}

	if (UZ_ncl_accelnum==0)
		return CDialog::PreTranslateMessage( msg );

	if (TranslateAccelerator(hWnd, m_accel, msg))
	{
/*
.....if it is in input mode, post the message like UNIX does
.....the TranslateAccelerator will call OnAccelFunctions which will
.....doing nothing if UD_pickmode = 1. Change here just to consistence with UNIX
.....Yurong 3/15/02
*/
		if (UD_pickmode==1)
			NCL_CurrentPKT_View->PostMessage(WM_KEYDOWN, (WPARAM)msg->wParam, (LPARAM)(msg->lParam));
		return TRUE;
	}
	else
	{
		return CDialog::PreTranslateMessage( msg );
	}
}
/***********************************************************************
c
c   FUNCTION: NCLPreTranslateMessage(MSG* pMsg) 
c
c       translate window messages before they are dispatch
c		this function just call PreTranslateMessage which is
c		a pretect function, so that other class can't access 
c
c   INPUT:  pMsg   Points to a MSG structure that contains the 
c					message to process.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNCLPockWin::NCLPreTranslateMessage(MSG* pMsg)
{
	return PreTranslateMessage(pMsg);
}

/**********************************************************************
**    I_FUNCTION :  OnAccelFunctions(UINT id)
**		Callback function for all NCL Accelerator functions
**
**    PARAMETERS   
**       INPUT  : 
**          UINT id: Accelerator ID
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLPockWin::OnAccelFunctions(UINT id)
{
	CString sav_cmdstr;
/*
......use local value
*/
	int savcmd_enable, sav_cmdcur1, sav_cmdcur2;
	int * save_inptr;
	int jmpflag, sav_cur,sav_mode,save_ptype, line_num, upload;
/*
.....if it is in input mode, post the message like UNIX does in
.....function PreTranslateMessage to let uw_ntevent to handle it 
.....doing nothing if UD_pickmode = 1
.....Yurong 3/15/02
*/
	if (UD_pickmode==1)
		return;
	sav_mode = UD_pickmode;
	ug_save_event();
	ug_save_input(&save_inptr);
	save_ptype = ud_getpick_type();
	ud_setpick_type(UD_PICK_NORMAL);
	savcmd_enable = NCL_MainFrame->Get_CmdEnable();
	sav_cur = uw_ntgetcur_cursor();
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag == 0)
	{
		int num = id - (WM_APP+UDM_MAX_MENU);
/*
.....save command text before disable it 
*/
		if (savcmd_enable==1)
		{
			NCL_MainFrame->GetCommand_Str(sav_cmdstr, line_num, upload);
			uw_ntget_ecurpos(&sav_cmdcur1, &sav_cmdcur2);
			NCL_MainFrame->SetCommand_Str("");
			NCL_MainFrame->Enable_cmdbar(0);
			NCL_MainFrame->SetCommand_focus(0);
		}
		uw_ntsetcursor(21);
		uz_wnt_callfunc(num, 1);
	}
	if (savcmd_enable==1)
	{
		NCL_MainFrame->Enable_cmdbar(1);
		NCL_MainFrame->SetCommand_Str(sav_cmdstr, line_num, upload);
		NCL_MainFrame->SetCommand_focus(1);
		NCL_MainFrame->SetCommand_Insertpos(sav_cmdcur1, sav_cmdcur2);
	}
	uw_ntsetcursor(sav_cur);
	ud_updatews(UG_SUPPRESS);
	UD_pickmode = sav_mode;
	ud_setpick_type(save_ptype);
	ug_reset_event();
	ug_reset_input(save_inptr);

	UD_UNMARK(jmpflag);
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
DWORD CNCLPockWin::HitTest(CPoint pt)
{
	CRect rect, rect2;
	if (m_Wtype == UM_IPV_WINDOW)
		rect = m_rcMin;
	else
	{
		rect = m_rcMin;
		rect2 = m_rcClose;
	}
	if(rect.PtInRect(pt))
		return (DWORD) DHT_MIN;
	else if ((m_Wtype != UM_IPV_WINDOW) && (rect2.PtInRect(pt)))
		return (DWORD) DHT_CLOSE;
	else
		return (DWORD) DHT_CAPTION;
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
void CNCLPockWin::OnNcPaint() 
{
	Default();
/*
	CWindowDC dc(this);

	CRect rc;
	GetWindowRect(rc);

	rc.bottom = GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);

	CRect minRect;
	CRect closeRect;

	if (m_Wtype == UM_IPV_WINDOW)
	{
		minRect.top = GetSystemMetrics(SM_CYFRAME) + 1;
		minRect.bottom = GetSystemMetrics( SM_CYSIZE ) + 1;
		minRect.left = rc.right - rc.left - minRect.bottom;
		minRect.right = rc.right - rc.left - minRect.top;
		m_rcMin = minRect;
		dc.DrawFrameControl(minRect,
		DFC_CAPTION,
		DFCS_CAPTIONMIN );
	}
	else
	{
		closeRect.top = GetSystemMetrics(SM_CYFRAME) + 1;
		closeRect.bottom = GetSystemMetrics( SM_CYSIZE ) + 1;
		closeRect.left = rc.right - rc.left - closeRect.bottom;
		closeRect.right = rc.right - rc.left - closeRect.top;
		minRect.top = closeRect.top;
		minRect.bottom = closeRect.bottom;
		minRect.right = rc.right - rc.left - closeRect.bottom;
		minRect.left = minRect.right - (closeRect.bottom - closeRect.top);
		m_rcMin = minRect;
		m_rcClose = closeRect;
		dc.DrawFrameControl(minRect,
		DFC_CAPTION,
		DFCS_CAPTIONMIN );
		dc.DrawFrameControl(closeRect,
		DFC_CAPTION,
		DFCS_CAPTIONCLOSE);
	}
*/
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
BOOL CNCLPockWin::OnNcActivate(BOOL bActive) 
{
   // If you want different look when inactive change this
   OnNcPaint(); 
   return TRUE; 
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
void CNCLPockWin::OnLButtonUp(UINT nFlags, CPoint point) 
{
	CWnd *win = GetCapture();
	if ((this != GetCapture()) && (win!=NULL))
		goto done;
	
	CPoint pt=point;
	if (IsIconic()==0)
	{
		point.y += GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);
	}
	point.x += 5;

	DWORD hitTest = HitTest(point);

	switch(m_ButtonDown) 
	{
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
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());

	switch(hitTest)	
	{
/*
......close is only for drawing window
*/
	case DHT_CLOSE:
/*
.....release the key capture before close
*/
			ReleaseCapture();
			::EnableWindow(GetParent()->GetSafeHwnd(), TRUE);
			::SetActiveWindow(GetParent()->GetSafeHwnd());
			um_close_pocket_window(UM_DRAWING_WINDOW);
/*
.....return in this case because this object already being deleted
*/ 
			goto done;
	case DHT_MIN:
		if (IsIconic()==0)
		{
			((CNCLView*)NCL_Main_View)->reset_redraw();
			::EnableWindow(GetParent()->GetSafeHwnd(), TRUE);
			::SetActiveWindow(GetParent()->GetSafeHwnd());
			ShowWindow(SW_MINIMIZE);
			ReleaseCapture();
			::SetCapture(GetParent()->GetSafeHwnd());
		}
		else
			ShowWindow(SW_SHOWNORMAL);
		break;
	default:
			break;
	}
	m_ButtonDown = 0;
	ReleaseCapture();
////////////test
done:;
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
	NCL_mouse_func = 0;
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
void CNCLPockWin::OnNcLButtonDown(UINT nHitTest, CPoint point) 
{ 
	CPoint pt=point;
	ScreenToClient(&pt);
	if (IsIconic()==0)
	{
		pt.y += GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);
	}
	pt.x += 5;

	DWORD hitTest = HitTest(pt);

	switch(hitTest)
	{
	case DHT_CLOSE:
			{
/*
		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcClose,
			DFC_CAPTION,
			DFCS_CAPTIONCLOSE | DFCS_PUSHED);
			m_LastHit = hitTest;
			m_ButtonDown = hitTest;
*/
			SetCapture();
			}
			break;
	case DHT_MIN:
			{
/*		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcMin,
			DFC_CAPTION,
			DFCS_CAPTIONMIN | DFCS_PUSHED);
			m_LastHit = hitTest;
			m_ButtonDown = hitTest;
*/			SetCapture();
			}
			break;
	default:
			Default(); 
			break;
	}
} 
/***********************************************************************
c
c   FUNCTION: OnNcLButtonUp(UINT nHitTest, CPoint point)
c
c           The framework calls this member function when the user 
c			releases the left mouse button while the cursor is within a nonclient area
c
c   INPUT:  nHitTest: Specifies the hit-test code. A hit test is a test that determines the location of the cursor
c			point: Specifies a CPoint object that contains the x and y screen coordinates of the cursor position
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLPockWin::OnNcLButtonUp(UINT nHitTest, CPoint point) 
{ 
	CDialog::OnNcLButtonUp(nHitTest, point);
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
void CNCLPockWin::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CDialog::OnLButtonDown(nFlags, point);

//////////////////test
	int save_seg, sav_cur;
	int jmpflag;
	UD_MARK(jmpflag,UU_TRUE);
	if (UW_live_mouse==0)
		goto done;
	if (ud_ifpopup()) goto done;
/*
......call NCL define mouse function on CHOICE mode
......if it is input mode, the DAS event will handle the function
*/
	CPoint pt;
	GetCursorPos(&pt);
	ScreenToClient(&pt);
	if (UD_pickmode)
		goto done;
/******not included now
	if ((NCL_funkey==1)&& (NCL_pick_verify))
	{
		NCL_funkey = 0;
		SetTimer(1, 1000, NULL);
		m_timer = 1;
		m_mpoint.x = m_x = pt.x; 
		m_mpoint.y = m_y = pt.y;

		m_tiptext[0] = '\0';
		int stat = uw_glget_piksurf(pt.x, m_oldRect.bottom - pt.y, m_tiptext, 0);
		if (stat==-1)
			m_tiptext[0] = '\0';
		else
		{
			if (UD_select_key)
			{
				ud_post_msg(2);
				UD_select_key = 0;
				NCL_funkey = 1;
			}
		}
		WS_update_tip = 0;
		goto done;
	}
*/
/*
......check if any geometry is under this position and put into the picking stack
*/
/*
......we will get the select first if already have segment selected
*/
	sav_cur = uw_ntgetcur_cursor();
/*	save_seg = ud_getn_pick_segs();
	if (save_seg==0)
		uw_glput_pikgeom(point.x, m_oldRect.bottom - point.y);
	save_seg = ud_getn_pick_segs();
*/	
	NCL_mouse_func = 1;
	if (jmpflag == 0)
	{
		if (ul_ipv_view_active()) 
			uz_mouse_functions("mouse_left", 0);
	}
	else
		NCL_mouse_func = 0;
	if (UZ_nclipv_view == 1)
	{
		ul_ipv_view_inactive();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
	}
	NCL_mouse_func = 0;
/*
.....if pick stack is used, reset the stack,
.....otherwise, do nothing
*/
/*
	if (save_seg!=ud_getn_pick_segs())
		ncl_reset_pikgeom();
*/
	uw_ntsetcursor(sav_cur);
done:;
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE));
	UD_UNMARK(jmpflag);
}

/***********************************************************************
c
c   FUNCTION: OnMButtonDown(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user presses the middle mouse button.
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
void CNCLPockWin::OnMButtonDown(UINT nFlags, CPoint point) 
{	
	int save_seg, sav_cur;
	CDialog::OnMButtonDown(nFlags, point);
	int jmpflag;
	UD_MARK(jmpflag,UU_TRUE);
	if (UW_live_mouse==0)
		goto done;
	if (ud_ifpopup()) 
		goto done;
/*
......call NCL define mouse function on CHOICE mode
......if it is input mode, the DAS event will handle the function
*/
	if (UD_pickmode)
		goto done;
/*
......check if any geometry is under this position and put into the picking stack
*/
/*
......we will get the select first if already have segment selected
*/
	sav_cur = uw_ntgetcur_cursor();
/*	save_seg = ud_getn_pick_segs();
	if (save_seg==0)
		uw_glput_pikgeom(point.x, m_oldRect.bottom - point.y);
	save_seg = ud_getn_pick_segs();
*/
	NCL_mouse_func = 1;
	if (jmpflag == 0)
	{
		if (ul_ipv_view_active()) 
			uz_mouse_functions("mouse_middle", 0);
	}
	else
		NCL_mouse_func = 0;
	if (UZ_nclipv_view == 1)
	{
		ul_ipv_view_inactive();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
	}
	NCL_mouse_func = 0;
/*
.....if pick stack is used, reset the stack,
.....otherwise, do nothing
*/
/*
	if (save_seg!=ud_getn_pick_segs())
		ncl_reset_pikgeom();
*/
	uw_ntsetcursor(sav_cur);
done:;
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_MBUTTONDOWN, WM_MBUTTONDOWN, PM_REMOVE));
	UD_UNMARK(jmpflag);
}
/***********************************************************************
c
c   FUNCTION: OnRButtonDown(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user presses the right mouse button.
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
void CNCLPockWin::OnRButtonDown(UINT nFlags, CPoint point) 
{	
	int save_seg, sav_cur;
	CDialog::OnRButtonDown(nFlags, point);
	int jmpflag;
	UD_MARK(jmpflag,UU_TRUE);
	if (UW_live_mouse==0)
		goto done;
	if (ud_ifpopup()) goto done;
/*
......call NCL define mouse function on CHOICE mode
......if it is input mode, the DAS event will handle the function
*/
	if (UD_pickmode)
		goto done;
/*
......check if any geometry is under this position and put into the picking stack
*/
/*
......we will get the select first if already have segment selected
*/
	sav_cur = uw_ntgetcur_cursor();
/*	save_seg = ud_getn_pick_segs();
	if (save_seg==0)
		uw_glput_pikgeom(point.x, m_oldRect.bottom - point.y);
	save_seg = ud_getn_pick_segs();
*/
	NCL_mouse_func = 1;
	if (jmpflag == 0)
	{
		if (ul_ipv_view_active()) 
			uz_mouse_functions("mouse_right", 0);
	}
	else
		NCL_mouse_func = 0;
	if (UZ_nclipv_view == 1)
	{
		ul_ipv_view_inactive();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
	}
	NCL_mouse_func = 0;
/*
.....if pick stack is used, reset the stack,
.....otherwise, do nothing
*/
/*	if (save_seg!=ud_getn_pick_segs())
		ncl_reset_pikgeom();
*/
	uw_ntsetcursor(sav_cur);
done:;
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_RBUTTONDOWN, WM_RBUTTONDOWN, PM_REMOVE));
	UD_UNMARK(jmpflag);
}

/***********************************************************************
c
c   FUNCTION: OnMButtonUp(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user release the middle mouse button.
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
void CNCLPockWin::OnMButtonUp(UINT nFlags, CPoint point) 
{	
	CDialog::OnMButtonUp(nFlags, point);
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
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
void CNCLPockWin::OnRButtonUp(UINT nFlags, CPoint point) 
{	
	CDialog::OnRButtonUp(nFlags, point);
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
}
/***********************************************************************
c
c   FUNCTION: OnMouseWheel(UINT nFlags, short zDelta, CPoint pt)
c
c       The framework calls this member function 
c			when the user presses the right mouse button.
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
BOOL CNCLPockWin::OnMouseWheel(UINT nFlags, short zDelta, CPoint pt)
{	
	int sav_cur;
	int status = CDialog::OnMouseWheel(nFlags, zDelta, pt);
	if (UW_live_mouse==0)
		return status;
/*
......call NCL define mouse function on CHOICE mode
......if it is input mode, the DAS event will handle the function
*/
	if (UD_pickmode)
		return status;
	if ((UV_dynwheel==0)&&(UV_dynview_active))
		return status;
	NCL_mouse_func = 1;
	sav_cur = uw_ntgetcur_cursor();
	int jmpflag;
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag == 0)
	{
		if (ul_ipv_view_active()) 
		{
			if (zDelta>0)
				uz_mouse_functions("wheel_up", 0);
			else
				uz_mouse_functions("wheel_down", 0);
		}
	}
	else
		NCL_mouse_func = 0;
	if (UZ_nclipv_view == 1)
	{
		ul_ipv_view_inactive();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
	}
	NCL_mouse_func = 0;

	uw_ntsetcursor(sav_cur);
	MSG msg;
	while(::PeekMessage(&msg, NULL, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
	UD_UNMARK(jmpflag);
	return status;
}


extern "C" void uw_setpkt_cursor_val (CWnd* win, HCURSOR cur)
{
	((CNCLPockWin *)win)->m_current_cursor = cur;
}
extern "C" void uw_setpkt_cursor (CWnd* win, HCURSOR cur)
{
	((CNCLPockWin *)win)->SetCursor(cur);
}

