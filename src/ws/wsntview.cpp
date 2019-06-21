/********************************************************************* 
**  NAME:  wsntview.cpp
**
**			Native WinNT main graphic view functions
**			implementation of CNCLView class functions
**	CONTAINS: CNCLView  class functions
**			all functions declared in wsntview.h
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       wsntview.cpp , 25.5
**  DATE AND TIME OF LAST  MODIFICATION
**       04/06/18 , 12:12:00
*********************************************************************/

#include "wsntstdafx.h"
#include "wsntctl.h"
#include <GL/gl.h>
#include <GL/glu.h>

#include "wsntdoc.h"
#include "wsntview.h"
#include "wsntframe.h"
#include "wsgl.h"
#include "wsntglfunc.h"
#include "wsntcfunc.h"
#include "mpocket.h"
#include "wsglfun.h"
#include "wsnttooltip.h"
#include "wsglfun.h"
#include "winspool.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int UZ_nclipv_view;
extern "C" int UW_resize_window;
CClientDC *uw_curhDC;
CWnd *NCL_Current_View = NULL;
CWnd *NCL_Main_View = NULL;
extern CMainFrame *NCL_MainFrame;
extern int frm_done;
extern char UW_cur_screen[20];
extern int NT_cmap_changed;
extern "C" int UD_cmap_changed;
extern "C" int PKx,PKy;
extern "C" int UW_rubber_active;
extern "C" int NCL_animation;

extern "C" void ipv_resize_window();
extern "C" int uw_ntsave_prect(UM_pkwin_type type, int left, int top, int right, int bottom);
extern "C" void uw_ntset_context(UM_pkwin_type which, UU_LOGICAL force);
extern BOOL WINAPI uw_ntpaintDIB(HDC hDC, LPRECT lpDCRect, HDIB hDIB, 
					 LPRECT lpDIBRect, HPALETTE hPal);
extern BOOL WINAPI uw_saveDIB(HDIB hDib, HANDLE file);
extern HDIB WINAPI  uw_cywin_DIB(HWND hWnd, int size, int fit, int bcolor);
extern DWORD     WINAPI  DIBWidth (LPSTR lpDIB);
extern DWORD     WINAPI  DIBHeight (LPSTR lpDIB);

extern HDIB WINAPI uw_cypixels_DIB(unsigned char *pixels, int width, int height, int size, int paper_size, int fit, int bcolor);

int NCL_WNT_BUTDOWN = 0;
extern "C" int UV_dynview_active, UV_dynwheel;
extern int UW_not_paint;
extern "C" int uw_ntprintIPV(CDC* pDC);
extern "C" int uw_ntgetDIBIPV();
extern "C" int UD_printipv;
extern "C" int LW_active;
extern "C" int UM_swap_ipv;
extern "C" int UW_disp_buf;

#define TIP_ACTIVE
extern "C" int uu_delay(int itim);
extern "C" int NCL_pick_verify, UD_picking_active;
extern "C" int uw_glget_piksurf(int x, int y, char *text,int flag);
extern "C" int WS_update_tip;
extern "C" void uw_gldraw_pickfm();
extern "C" void uw_gldrawbuffer(Gbuffer);
extern "C" void uw_glupdrec(Gnrect *, UU_LOGICAL);
extern "C" void uw_glmark_dirty_rect(Gnrect *);
extern "C" void uv_set_background();

int UW_view_hit = -1;
#define HILITE_MARK 0
#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3
extern "C" int NCL_mark_method;
extern "C" void um_set_screen_area(UM_pkwin_type);
extern "C" UM_pkwin_type um_get_screen_area();
extern "C" int uz_mouse_functions(char*, int);
extern "C" int uw_glput_pikgeom(int, int);
extern "C" int ncl_reset_pikgeom();
extern "C" int ud_getn_pick_segs();
extern "C" int uw_ntgetcur_cursor();
extern "C" int NCL_mouse_func;
extern "C" int NCL_funkey;
extern "C" int UD_select_key;
extern "C" int ud_post_msg(int);
extern "C" int ud_reset_verify_list();
extern  "C" int ud_ifpopup();
extern "C" char * uu_malloc(int);
extern "C" void uu_free(char*);

extern "C" int UW_live_mouse;

//extern "C" void um_reset_pocket_graphics(int type);
//extern "C" void ul_ipv_view_inactive();
extern "C" void uw_setipv_winfocus();

static int S_pick_start = 0;
/***********************************************************************
c
c   FUNCTION: SetDefaultPrinterOrientation(short dmOrientation)
c
c              Set the default printer's printing orientation
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    the old printing orientation
c
**********************************************************************/
short SetDefaultPrinterOrientation(short dmOrientation)
{
	HANDLE hPrinter = NULL;
	DWORD dwNeeded = 0;
	PRINTER_INFO_2 *pi2 = NULL;
	DEVMODE *pDevMode = NULL;
	PRINTER_DEFAULTS pd;
	BOOL bFlag;
	LONG lFlag;
	LPTSTR pPrinterName = NULL;
	DWORD size;
	GetDefaultPrinter(NULL, &size);
	TCHAR* buffer = new TCHAR[size];
	if(GetDefaultPrinter(buffer, &size))
		pPrinterName = buffer;
	else
	{
		if(buffer != NULL)
			delete buffer;
		return 0;
	}
	ZeroMemory(&pd, sizeof(pd));
	pd.DesiredAccess = PRINTER_ALL_ACCESS;
	bFlag = OpenPrinter(pPrinterName, &hPrinter, &pd);
	if (!bFlag || (hPrinter == NULL))
	{
		if(buffer != NULL)
			delete buffer;
		return 0;
	}
	SetLastError(0);
	bFlag = GetPrinter(hPrinter, 2, 0, 0, &dwNeeded);
	if ((!bFlag) && (GetLastError() != 
			ERROR_INSUFFICIENT_BUFFER) || (dwNeeded == 0))
	{
		ClosePrinter(hPrinter);
		if(buffer != NULL)
			delete buffer;
		return 0;
	}
	pi2 = (PRINTER_INFO_2 *)GlobalAlloc(GPTR, dwNeeded);
	if (pi2 == NULL)
	{
		ClosePrinter(hPrinter);
		if(buffer != NULL)
			delete buffer;
		return 0;
	}
	bFlag = GetPrinter(hPrinter, 2, (LPBYTE)pi2, dwNeeded, &dwNeeded);
	if (!bFlag)
	{
		GlobalFree(pi2);
		ClosePrinter(hPrinter);
		if(buffer != NULL)
			delete buffer;
		return 0;
	}
	if (pi2->pDevMode == NULL)
	{
		dwNeeded = DocumentProperties(NULL, hPrinter, 
					pPrinterName,NULL, NULL, 0);
		if (dwNeeded <= 0)
		{
			GlobalFree(pi2);
			ClosePrinter(hPrinter);
			if(buffer != NULL)
				delete buffer;
			return 0;
		}
		pDevMode = (DEVMODE *)GlobalAlloc(GPTR, dwNeeded);
		if (pDevMode == NULL)
		{
			GlobalFree(pi2);
			ClosePrinter(hPrinter);
			if(buffer != NULL)
				delete buffer;
			return 0;
		}
		lFlag = DocumentProperties(NULL, hPrinter, 
					pPrinterName, pDevMode, NULL,DM_OUT_BUFFER);
		if (lFlag != IDOK || pDevMode == NULL)
		{
			GlobalFree(pDevMode);
			GlobalFree(pi2);
			ClosePrinter(hPrinter);
			if(buffer != NULL)
				delete buffer;
			return 0;
		}
		pi2->pDevMode = pDevMode;
	}
	if (!(pi2->pDevMode->dmFields & DM_ORIENTATION))
	{
		GlobalFree(pi2);
		ClosePrinter(hPrinter);
		if (pDevMode)
			GlobalFree(pDevMode);
		if(buffer != NULL)
			delete buffer;
		return 0;
	}
	pi2->pDevMode->dmFields = DM_ORIENTATION;
	int OriginalOrientation = pi2->pDevMode->dmOrientation;
	pi2->pDevMode->dmOrientation = dmOrientation;
	pi2->pSecurityDescriptor = NULL;
	lFlag = DocumentProperties(NULL, hPrinter, pPrinterName, 
			pi2->pDevMode, pi2->pDevMode, 
			DM_IN_BUFFER | DM_OUT_BUFFER);
	if (lFlag != IDOK)
	{
		GlobalFree(pi2);
		ClosePrinter(hPrinter);
		if (pDevMode)
			GlobalFree(pDevMode);
		if(buffer != NULL)
			delete buffer;
		return 0;
	}
	bFlag = SetPrinter(hPrinter, 2, (LPBYTE)pi2, 0);
	if (!bFlag)
	{
		GlobalFree(pi2);
		ClosePrinter(hPrinter);
		if (pDevMode)
			GlobalFree(pDevMode);
		if(buffer != NULL)
			delete buffer;
		return 0;
	}
	SendMessageTimeout(HWND_BROADCAST, WM_DEVMODECHANGE, 0L, 
			(LPARAM)(LPCSTR)pPrinterName, SMTO_NORMAL, 1000, NULL);
	if (pi2)
		GlobalFree(pi2);
	if (hPrinter)
		ClosePrinter(hPrinter);
	if (pDevMode)
		GlobalFree(pDevMode);
	if(buffer != NULL)
		delete buffer;
	return OriginalOrientation;
}

/////////////////////////////////////////////////////////////////////////////
// CNCLView

IMPLEMENT_DYNCREATE(CNCLView, CView)

BEGIN_MESSAGE_MAP(CNCLView, CView)
	//{{AFX_MSG_MAP(CNCLView)
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_SIZE()
	ON_WM_ERASEBKGND()
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONDOWN()
	ON_WM_MBUTTONDOWN()
	ON_WM_RBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_MBUTTONUP()
	ON_WM_RBUTTONUP()  
	ON_WM_MOUSEWHEEL()
	ON_WM_LBUTTONDBLCLK()
	ON_WM_TIMER()
	ON_COMMAND(ID_FILE_PRINT, OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, OnFilePrintPreview)
	ON_WM_NCHITTEST ()
	//}}AFX_MSG_MAP
	// Standard printing commands
END_MESSAGE_MAP()
/////////////////////////////////////////////////////////////////////////////
/***********************************************************************
c
c   FUNCTION: CNCLView()
c
c              Constructor of class CNCLView
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CNCLView::CNCLView()
{
	m_redraw = 1;
	m_cPalette = NULL;
	m_tiptext = new char[80];
	m_tiptext[0] = '\0';
	m_hDIB	   = NULL;
	m_palDIB   = new CPalette;
	m_papersize = 0;
	m_fit = 1;
	m_bcolor = 0;
	m_Wtype = UM_NCL_WINDOW;
	m_x = m_y = -1;
	m_mpoint.x = m_mpoint.y = -1;
	m_timer = 0;
	m_current_cursor = LoadCursor(NULL, IDC_ARROW);
	m_pcenter = 0;
}
		
/***********************************************************************
c
c   FUNCTION: ~CNCLView()
c
c              Destructor of class CNCLView
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CNCLView::~CNCLView()
{
	if (m_tiptext!=NULL)
		delete m_tiptext;
	
	if( m_hDIB != NULL )
    {
        ::GlobalUnlock(m_hDIB);
	    ::GlobalFree(m_hDIB);
	    m_hDIB = NULL;
	}  
	if (m_palDIB != NULL)
	{
		delete m_palDIB;
		m_palDIB = NULL;
	}
}

/***********************************************************************
**
**   FUNCTION: PreCreateWindow(CREATESTRUCT& cs)
**
**       Called by the framework before the creation 
**		of the Windows window attached to this CWnd object.
**		Override this member function to modify the 
**		CREATESTRUCT structure before the window is created. 
**
**		Never call this function directly.
**
**
**   INPUT:  CREATESTRUCT& cs
**
**   OUTPUT :   CREATESTRUCT& cs
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLView::PreCreateWindow(CREATESTRUCT& cs)
{
/*
......An OpenGL window must be created with the following flags and must not
......include CS_PARENTDC for the class style. 
*/
	CNCLApp *app = (CNCLApp *)AfxGetApp();
	cs.lpszClass = (const char*) (app->m_strMyClassName);

	cs.style |= WS_CLIPSIBLINGS | WS_CLIPCHILDREN;

	return CView::PreCreateWindow(cs);
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
void CNCLView::SetContext()
{
	wglMakeCurrent(m_pDC->GetSafeHdc(), m_hrc);
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
char * CNCLView::GetWin()
{
	HWND hWndParent = this->GetSafeHwnd();
	return (char *)hWndParent;
}

/////////////////////////////////////////////////////////////////////////////
// CNCLView drawing
/***********************************************************************
**
**   FUNCTION: OnDraw(CDC* pDC)
**
**       The framework calls this function to perform 
**		 screen display, and it passes a different device 
**		 context in each case. There is no default implementation.
**
**   INPUT:  CDC* pDC: device context
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLView::OnDraw(CDC* pDC)
{
	static int flushing = 0;
	CNCLDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	UM_pkwin_type sarea;
	sarea = um_get_screen_area();	

	if (pDC->IsPrinting()) 
	{
		if (UD_printipv)
		{
			uw_ntprintIPV(pDC);
		}
		else
		{
			PrintDIB(pDC);
			flushing = 0;
		}
		return;
	}
	if (UW_not_paint==1)
		return;
/*
.....if dynamic view is on, we can't doing redraw because
.....first, not neccesary since the 'dynamic view' will redraw
.....second, when double buffering for dynamic view, the glFlush here
.....will flush the screen and make parts shaky.
.....Yurong
*/
	if (UV_dynview_active) return;

	RECT prect;
	int stat = pDC->GetClipBox(&prect);

	if (m_Wtype == UM_IPV_WINDOW)
	{
		um_set_screen_area(UM_IPV_WINDOW);
		ipv_resize_window();
	}
	else if (m_Wtype == UM_NCL_WINDOW)
	{
		NCL_Current_View = this;
		if (UM_swap_ipv==0)
		{
			if (current_glctx != 0)
			{
				uw_ntset_context(UM_NCL_WINDOW,UU_FALSE);
				current_glctx = 0;
				uw_ntset_maingw(UW_cur_screen);
				m_redraw = 1;
			}
			if (m_redraw==0)
			{
				m_redraw = 1;
				return;
			}
/*
.....folloeing statement have to after reset m_redraw call
.....because we set m_redraw on OnSize function (to avoid drawing graphic twice or more)
.....and OnSize function will
.....call OnDraw but it's cliparea could be  empty
.....Yurong 2/5/03
*/
			if ((prect.left==0) && (prect.right==0) && (prect.top==0) && (prect.bottom==0) )
				return;
			um_set_screen_area(UM_NCL_WINDOW);

			Gnrect rect;
			Gfloat nll[2],nur[2];
			int ll[2],ur[2];

			ll[0] = prect.left;
			ll[1] = m_oldRect.bottom - prect.bottom;
			ur[0] = prect.right;
			ur[1] = m_oldRect.bottom - prect.top;

			uw_gldevtondc(ll,nll);
			uw_gldevtondc(ur,nur);

			rect.ll.x = nll[0];
			rect.ll.y = nll[1];
			rect.ur.x = nur[0];
			rect.ur.y = nur[1];
		
			uw_glupdrec(&rect,UU_TRUE);
			uw_gldraw_pickfm();
		}
		else if (LW_active == 1)
		{
			um_set_screen_area(UM_IPV_WINDOW);
			ipv_resize_window();
			RECT rect;
			GetWindowRect(&rect);
			uw_ntsave_prect(m_Wtype, rect.left, rect.top, rect.right, rect.bottom);
		}
	}
	um_set_screen_area(sarea);
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
int CNCLView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CView::OnCreate(lpCreateStruct) == -1)
		return -1;

	NCL_Main_View = NCL_Current_View = this;
	m_pDC = new CClientDC(this);
	uw_curhDC = m_pDC;
    ASSERT(m_pDC != NULL);
/*
......initialize OpenGL
*/
	Init();	
	return 0;
}
/***********************************************************************
**
**   FUNCTION: OnDestroy() 
**
**		OnDestroy is called after the CNCLView object 
**		is removed from the screen. Delete openGL
**		context here 
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLView::OnDestroy() 
{
    ::wglMakeCurrent(NULL,  NULL);

    if (m_hrc)
        ::wglDeleteContext(m_hrc);


    if (m_pDC)
        delete m_pDC;

	CView::OnDestroy();
}
/***********************************************************************
**
**   FUNCTION: OnSize(UINT nType, int cx, int cy) 
**
**		The framework calls this member function 
**		after the window's size has changed. 
**   
**	 INPUT:  nType:   Specifies the type of resizing 
**					requested. This parameter can 
**					be one of the following values:
**					SIZE_MAXIMIZED   Window has been maximized.
**					SIZE_MINIMIZED   Window has been minimized.
**					SIZE_RESTORED   Window has been resized, but neither 
**									SIZE_MINIMIZED nor SIZE_MAXIMIZED applies.
**					SIZE_MAXHIDE   Message is sent to all pop-up windows when some other window is maximized.
**					SIZE_MAXSHOW   Message is sent to all pop-up windows when some other window has been restored to its former size.
**			  cx:   Specifies the new width of the client area.
**			  cy:   Specifies the new height of the client area.
**
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLView::OnSize(UINT nType, int cx, int cy) 
{
	static int old_cx = -1;
	static int old_cy = -1;
	GLint dw_buf = -1;
	if (NCL_MainFrame==NULL) return;
	CView::OnSize(nType, cx, cy);
		
	UM_pkwin_type sarea;
	sarea = um_get_screen_area();	

	if ((cx<50) && (cy<50)) return;
	NCL_Current_View = this;
	if (UM_swap_ipv==1)
	{
		PKx = cx ; 
		PKy = cy;
	}

	if (m_Wtype == UM_NCL_WINDOW)
    {
		if (UM_swap_ipv==0)
		{
			if (cy > 0)
			{    
				if (current_glctx != 0)
				{
					uw_ntset_context(UM_NCL_WINDOW,UU_FALSE);
					current_glctx = 0;
				}
				m_oldRect.right = cx;
			    m_oldRect.bottom = cy;
				if (NCL_MainFrame!=NULL)
				{
/*
.....we don't call openGL resize function
.....util finished frame layout. 
*/
					if ((frm_done==1)&&(UD_cmap_changed==0))
					{
						int clip[4];
						clip[0] = 0;
						clip[1] = 0;
						clip[2] = cx;
						clip[3] = cy;
						uw_glset_scissor(clip);
/*
.....this function will call OnDraw function to redraw
.....window because of resize
.....but we don't want the graphic display now
*/
						if ((cx!=old_cx)||(cy!=old_cy)||UW_resize_window)
						{
							UZ_nclipv_view = 0;
							uw_glresize_graphics(cx, cy, 1);
							UW_resize_window = 0;
						}
						old_cx = cx;
						old_cy = cy;
					}
					else
					{
						old_cx = cx;
						old_cy = cy;
					}
				}
			}
			m_redraw = 0;
		}
		else if (LW_active == 1)
		{
			um_set_screen_area(UM_IPV_WINDOW);
			UZ_nclipv_view = 1;
			ipv_resize_window();
			RECT rect;
			GetWindowRect(&rect);
			uw_ntsave_prect(m_Wtype, rect.left, rect.top, rect.right, rect.bottom);
		}
	}
	else if (m_Wtype == UM_IPV_WINDOW)
	{
		if (LW_active == 1)
		{
			um_set_screen_area(UM_IPV_WINDOW);
			UZ_nclipv_view = 1;
			ipv_resize_window();
			RECT rect;
			GetWindowRect(&rect);
			uw_ntsave_prect(m_Wtype, rect.left, rect.top, rect.right, rect.bottom);
		}
	}
	um_set_screen_area(sarea);
	UZ_nclipv_view = 0;
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
void CNCLView::Init()
{
	if ((NCL_MainFrame==NULL)||(NT_cmap_changed==1))
		return;

	uw_glinit_visual();

	UW_NTgraphic = m_hWnd;
	UW_NTgraphicDC = m_pDC->GetSafeHdc();

	m_hrc = wglCreateContext(m_pDC->GetSafeHdc());
	uw_ntset_context(UM_NCL_WINDOW,UU_TRUE);
	current_glctx = 0;
/*
......not call here, called in CMainFrame::OnCreate after we call unicad_
......which set some global value like UW_label_size
......Yurong
	uw_ntcreate_font();
*/
	GetClientRect(&m_oldRect);
	uw_gldrawbuffer(UG_BOTH_BUFFER);
	glClearDepth_d(1.0f);
	glEnable_d(GL_DEPTH_TEST);

	glEnable_d(GL_BLEND);
	glDisable_d(GL_STENCIL_TEST);  

	int size[2];
    GetClientRect(&m_oldRect);
	size[0] = m_oldRect.right;
	size[1] = m_oldRect.bottom;
	uw_gldrawbuffer(UG_BACK_BUFFER);
	uw_glglxinit(size);
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
BOOL CNCLView::bSetupPixelFormat()
{
	static PIXELFORMATDESCRIPTOR pfdd = 
	{
		sizeof(PIXELFORMATDESCRIPTOR),  // size of this pfd
		1,                              // version number
		PFD_DRAW_TO_WINDOW |            // support window
		PFD_SUPPORT_OPENGL |            // support OpenGL
		PFD_SWAP_COPY |                 // Copy back buffer to front on SwapBuffer
		PFD_DOUBLEBUFFER,               // double buffered
		PFD_TYPE_RGBA,
		24,                              // 8-bit color depth
		0, 0, 0, 0, 0, 0,               // color bits ignored
		0,                              // no alpha buffer
		0,                              // shift bit ignored
		0,                              // no accumulation buffer
		0, 0, 0, 0,                     // accum bits ignored
		32,                             // 8-bit z-buffer
		8,                              // 1 bit stencil buffer
		0,                              // no auxiliary buffer
		PFD_MAIN_PLANE,                 // main layer
		0,                              // reserved
		0, 0, 0                         // layer masks ignont
	};
    int pixelformat;

	pfdd.iPixelType = PFD_TYPE_RGBA;
	pfdd.cStencilBits  = 2; 

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
	int n = ::GetPixelFormat(m_pDC->GetSafeHdc());
    ::DescribePixelFormat(m_pDC->GetSafeHdc(), n, sizeof(pfdd), &pfdd);
	if (!(pfdd.dwFlags & PFD_SWAP_COPY))
		UW_disp_buf = 1;

    return TRUE;
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
**			function, so simply return 0;
**   
**	 INPUT:  CDC* pDC: device context
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLView::OnEraseBkgnd(CDC* pDC) 
{
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// CNCLView printing, we are not using it now

BOOL CNCLView::OnPreparePrinting(CPrintInfo* pInfo)
{
	return DoPreparePrinting(pInfo);
}

void CNCLView::OnBeginPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add extra initialization before printing
}

void CNCLView::OnEndPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add cleanup after printing
}


/***********************************************************************
c
c   FUNCTION: OnInitialUpdate()
c
c       Called by the framework before the view is initially displayed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLView::OnInitialUpdate() 
{
	CView::OnInitialUpdate();

#ifdef TIP_ACTIVE
	m_RectTips.Create(this);
#endif
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
BOOL CNCLView::PreTranslateMessage(MSG* pMsg) 
{
#ifdef TIP_ACTIVE
/*
......Because modal dialogs will cause all CToolTipCtrls to get deactivated,
......the tool tip control needs to be manually reactivated.
*/
	if (pMsg->message == WM_MOUSEMOVE)
	{
//		if ((UD_picking_active) && (NCL_pick_verify))
//		{
//			m_RectTips.Activate(TRUE);

/*
......RelayEvent is required for CToolTipCtrl objects -
......it passes mouse messages on to the tool tip control
......so it can decide when to show the tool tip
*/
//			m_RectTips.RelayEvent(pMsg);
//		}
	}
#endif	

	return CView::PreTranslateMessage(pMsg);
}



/////////////////////////////////////////////////////////////////////////////
// CNCLView diagnostics

#ifdef _DEBUG
void CNCLView::AssertValid() const
{
	CView::AssertValid();
}

void CNCLView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CNCLDoc* CNCLView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CNCLDoc)));
	return (CNCLDoc*)m_pDocument;
}
#endif 

/////////////////////////////////////////////////////////////////////////////
// CNCLView message handlers

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
void CNCLView::OnMouseMove(UINT nFlags, CPoint point) 
{		
	CWnd* wnd = GetActiveWindow( );
	if (wnd==UU_NULL) 
	{
		CView::OnMouseMove(nFlags, point);
		goto done;
	}
/*
......when playback is on (geometry is moving)
......don't call picking verify function
*/
	if (NCL_animation)
	{
		CView::OnMouseMove(nFlags, point);
		goto done;
	}
start:;
/*
......if in dynamic view, disable the picking verify, it will use too much CPU
......and cause problem
*/
	if ((UD_pickmode==1) && UV_dynview_active)
	{
		CView::OnMouseMove(nFlags, point);
		goto done;
	}

	if (((UM_swap_ipv) || (NCL_mark_method != DYNAMIC_MARK)&&(UD_pickmode==1))
		|| ((UD_pickmode==0) && UV_dynview_active))
	{
		CView::OnMouseMove(nFlags, point);
		goto done;
	}
	if (UW_rubber_active || (!((UD_picking_active) && (NCL_pick_verify)))&&(UD_pickmode==1))
	{
		if (m_timer)
		{
			KillTimer(1);
			m_timer = 0;
		}
		m_x = -1; m_y=-1;
		m_mpoint.x = m_mpoint.y = -1;
		m_RectTips.Close();
		CView::OnMouseMove(nFlags, point);
		WS_update_tip = 0;
		goto done;
	}
/*
......if pick and verify active
*/
	if (((point.x==m_x) && (point.y==m_y) && (WS_update_tip==0))&&(NCL_funkey==0))
	{
		if (m_timer==0)
		{
			SetTimer(1, 1000, NULL);
			m_timer = 1;
		}
		WS_update_tip = 0;
		CView::OnMouseMove(nFlags, point);
		goto done;
	}
//test
if (S_pick_start)
	return;
S_pick_start = 1;
//end test
	if (!((point.x==m_x) && (point.y==m_y)))
/*
......if mouse moved, reset the verify list
*/
	{
		ud_reset_verify_list();
		ncl_reset_pikgeom();
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
	if (((UD_picking_active) && (NCL_pick_verify))||(UD_pickmode==0)&&(UW_live_mouse))
	{
		m_tiptext[0] = '\0';
		int stat = uw_glget_piksurf(point.x, m_oldRect.bottom - point.y, m_tiptext,1);
		if (stat==-1)
			m_tiptext[0] = '\0';
	}
#endif
//test
S_pick_start = 0;
	CView::OnMouseMove(nFlags, point);
	WS_update_tip = 0;
	NCL_funkey = 0;
/*
.....Eat spurious WM_MOUSEMOVE messages
.....remove the mousemove event (because if we handle too many event in short time
.....it may cause event to stall
*/
//not post message (handle the last mouse move event) for all case, 'done' should after
//those event handling
//done:;
	MSG msg;
	int i=0;
	while(::PeekMessage(&msg, m_hWnd, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE))
	{
		i++;
	}
	if (i==0)
		return;
/*
.....we need handle the last mouse move event at the current point
*/

	PostMessage(WM_MOUSEMOVE, msg.wParam, msg.lParam);
done:;
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
void CNCLView::OnTimer(UINT_PTR nIDEvent) 
{	
	if ((UM_swap_ipv)|| ((NCL_mark_method != DYNAMIC_MARK)&&(UD_pickmode==1)))
	{
		goto done;
	}

	if ((!((UD_picking_active) && (NCL_pick_verify)))&&(UD_pickmode==1))
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
			m_tiptext[0] = '\0';
		}
	}
	if (UD_pickmode==0)
	{
		if (m_tiptext[0]!='\0')
		{
			m_RectTips.SetText(m_tiptext);
			m_RectTips.Show(m_mpoint);
			m_tiptext[0] = '\0';
			KillTimer(1);
			SetTimer(1, 3000, NULL);
			m_timer = 1;
		}
		else if (m_timer)
		{
			KillTimer(1);
			m_RectTips.Close();
			m_timer = 0;
		}
	}
done:;
	CView::OnTimer(nIDEvent);

	// Eat spurious WM_TIMER messages
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_TIMER, WM_TIMER, PM_REMOVE));
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
void CNCLView::OnLButtonDown(UINT nFlags, CPoint point) 
{	
	int save_seg, sav_cur;
	CView::OnLButtonDown(nFlags, point);
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
//test
if (S_pick_start)
	return;
S_pick_start = 1;
//end test
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
/*
......check if any geometry is under this position and put into the picking stack
*/
/*
......we will get the select first if already have segment selected
*/
	sav_cur = uw_ntgetcur_cursor();
	save_seg = ud_getn_pick_segs();
	if (save_seg==0)
		uw_glput_pikgeom(point.x, m_oldRect.bottom - point.y);
	save_seg = ud_getn_pick_segs();
	
	NCL_mouse_func = 1;
	if (jmpflag == 0)
	{
		uz_mouse_functions("mouse_left", 0);
	}
	else
		NCL_mouse_func = 0;
/*
.....if pick stack is used, reset the stack,
.....otherwise, do nothing
*/
	if (save_seg!=ud_getn_pick_segs())
		ncl_reset_pikgeom();
	uw_ntsetcursor(sav_cur);
//test
	S_pick_start = 0;
done:;
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE));
	UD_UNMARK(jmpflag);
//test
	S_pick_start = 0;
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
void CNCLView::OnMButtonDown(UINT nFlags, CPoint point) 
{	
	int save_seg, sav_cur;
	CView::OnMButtonDown(nFlags, point);
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
	save_seg = ud_getn_pick_segs();
	if (save_seg==0)
		uw_glput_pikgeom(point.x, m_oldRect.bottom - point.y);
	save_seg = ud_getn_pick_segs();
	NCL_mouse_func = 1;
	if (jmpflag == 0)
	{
		uz_mouse_functions("mouse_middle", 0);
	}
	else
		NCL_mouse_func = 0;
/*
.....if pick stack is used, reset the stack,
.....otherwise, do nothing
*/
	if (save_seg!=ud_getn_pick_segs())
		ncl_reset_pikgeom();
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
void CNCLView::OnRButtonDown(UINT nFlags, CPoint point) 
{	
	int save_seg, sav_cur;
	CView::OnRButtonDown(nFlags, point);
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
	save_seg = ud_getn_pick_segs();
	if (save_seg==0)
		uw_glput_pikgeom(point.x, m_oldRect.bottom - point.y);
	save_seg = ud_getn_pick_segs();
	NCL_mouse_func = 1;
	if (jmpflag == 0)
	{
		uz_mouse_functions("mouse_right", 0);
	}
	else
		NCL_mouse_func = 0;
/*
.....if pick stack is used, reset the stack,
.....otherwise, do nothing
*/
	if (save_seg!=ud_getn_pick_segs())
		ncl_reset_pikgeom();
	uw_ntsetcursor(sav_cur);
done:;
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_RBUTTONDOWN, WM_RBUTTONDOWN, PM_REMOVE));
	UD_UNMARK(jmpflag);
}

/***********************************************************************
c
c   FUNCTION: OnLButtonUp(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user release the left mouse button.
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
void CNCLView::OnLButtonUp(UINT nFlags, CPoint point) 
{	
	CView::OnLButtonUp(nFlags, point);

	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
/*	if (UZ_nclipv_view == 1)
	{
//		ul_ipv_view_inactive();
//		um_reset_pocket_graphics(UM_IPV_WINDOW);
		UZ_nclipv_view = 0;
		if (NCL_mouse_func)
		{
			uw_setipv_winfocus();
		}
	}
*/
	NCL_mouse_func = 0;
//test
S_pick_start = 0;
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
void CNCLView::OnMButtonUp(UINT nFlags, CPoint point) 
{	
	CView::OnMButtonUp(nFlags, point);
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
/*	if (UZ_nclipv_view == 1)
	{
//		ul_ipv_view_inactive();
//		um_reset_pocket_graphics(UM_IPV_WINDOW);
		UZ_nclipv_view = 0;
		if (NCL_mouse_func)
		{
			uw_setipv_winfocus();
		}
	}
*/
	NCL_mouse_func = 0;
//test
S_pick_start = 0;
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
void CNCLView::OnRButtonUp(UINT nFlags, CPoint point) 
{	
	CView::OnRButtonUp(nFlags, point);
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
/*	if (UZ_nclipv_view == 1)
	{
//		ul_ipv_view_inactive();
//		um_reset_pocket_graphics(UM_IPV_WINDOW);
		UZ_nclipv_view = 0;
		if (NCL_mouse_func)
		{
			uw_setipv_winfocus();
		}
	}
*/
	NCL_mouse_func = 0;
//test
S_pick_start = 0;
}

/***********************************************************************
**
**   FUNCTION: inicolormap()
**
**		initial openGL index color map
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int  CNCLView::inicolormap()
{
	int      i,   n;
	PIXELFORMATDESCRIPTOR pfd;

    if (!bSetupPixelFormat())
        return -1;
    n = ::GetPixelFormat(
		m_pDC->GetSafeHdc());
    ::DescribePixelFormat(m_pDC->GetSafeHdc(), n, sizeof(pfd), &pfd);

/*
.....Store color indices for pocket window
*/
	for (i=0;i<64;i++)
	{
		UM_pkcolors[i].red = uw_color_table[i][0];
		UM_pkcolors[i].green = uw_color_table[i][1];
		UM_pkcolors[i].blue = uw_color_table[i][2];
	}
/*
.....Set the background colors
*/
	uv_set_background();
	return 0;
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
BOOL CNCLView::OnMouseWheel(UINT nFlags, short zDelta, CPoint pt)
{	
	int sav_cur;
	int status = CView::OnMouseWheel(nFlags, zDelta, pt);

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
//test
if (S_pick_start)
	return status;
S_pick_start = 1;
//end test
	NCL_mouse_func = 1;
	sav_cur = uw_ntgetcur_cursor();
	int jmpflag;
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag == 0)
	{
		if (zDelta>0)
			uz_mouse_functions("wheel_up", 0);
		else
			uz_mouse_functions("wheel_down", 0);
	}
	else
		NCL_mouse_func = 0;
	uw_ntsetcursor(sav_cur);

//test
S_pick_start = 0;
NCL_mouse_func = 0;

	MSG msg;
	while(::PeekMessage(&msg, NULL, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
	UD_UNMARK(jmpflag);

	return status;
}

/***********************************************************************
c
c   FUNCTION: PrintDIB(CDC* pDC)
c
c     Print DIB on a printer
c
c   INPUT:  
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLView::PrintDIB(CDC* pDC)
{
	if(m_hDIB == NULL || m_palDIB == NULL) return;

	int cxPage = pDC->GetDeviceCaps(HORZRES);
	int cyPage = pDC->GetDeviceCaps(VERTRES);
	int cxInch = pDC->GetDeviceCaps(LOGPIXELSX);
	int cyInch = pDC->GetDeviceCaps(LOGPIXELSY);
	
	char* pDIB = (char*)::GlobalLock(m_hDIB);
	int cxDIB  = (int)  ::DIBWidth(pDIB);
	int cyDIB  = (int)  ::DIBHeight(pDIB);
	if( cxDIB <= 0 || cyDIB <= 0 ) return;

	RECT rcDst, rcDIB;
	rcDIB.top    = 0;
	rcDIB.left	 = 0;
	rcDIB.right  = cxDIB;
	rcDIB.bottom = cyDIB;
	int shiftx, shifty;
	if (m_papersize==1)
	{
		rcDst.left   = 0;
		rcDst.top    = 0;
		rcDst.right  = cxPage;
		rcDst.bottom = (int)(1.0 * cyDIB * cxPage * cyInch / cxDIB / cxInch);
		shiftx = 0;
		shifty = (cyPage - rcDst.bottom)/2;
	}
	else
	{
		rcDst.left   = 0;
		rcDst.top    = 0;
		rcDst.right  = cxPage;
		rcDst.bottom = (int)(1.0 * cyDIB * cxPage * cyInch / cxDIB / cxInch);
		shiftx = 0;
		shifty = (cyPage - rcDst.bottom)/2;
	}
	if (m_pcenter)
	{
		rcDst.left   = shiftx;
		rcDst.top    = shifty;
		rcDst.right  = cxPage + shiftx;
		rcDst.bottom = (int)(1.0 * cyDIB * cxPage * cyInch / cxDIB / cxInch) + shifty;
	}
	::uw_ntpaintDIB(pDC->m_hDC, &rcDst, (HDIB)m_hDIB, &rcDIB, (HPALETTE)m_palDIB->m_hObject);
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
void CNCLView::GetDIB()
{
	if (m_hDIB != NULL)
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
	glPixelStorei_d(GL_PACK_ALIGNMENT, 1);
	glReadBuffer_d(GL_BACK);
	pixels = (unsigned char *)uu_malloc(3*size*sizeof(unsigned char));
	glReadPixels_d(0,0, width, height, GL_RGB, GL_UNSIGNED_BYTE, pixels);

	m_hDIB = ::uw_cypixels_DIB(pixels, width, height, size, m_papersize, m_fit, m_bcolor);
		
	uu_free((char*)pixels);
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
void CNCLView::FreeDIB()
{
	::GlobalUnlock(m_hDIB);
	::GlobalFree(m_hDIB);
	m_hDIB = NULL;
}

/***********************************************************************
c
c   FUNCTION: OnFilePrint()
c
c     Print current graphic view
c
c   INPUT:  
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLView::OnFilePrint() 
{
	int papersize;
	if (( (UD_printipv == 0) && (UM_swap_ipv==0) )
		|| ( (UD_printipv == 1) && (UM_swap_ipv==1) ))
	{
		GetDIB();
		papersize = m_papersize;
	}
	else
	{
		papersize = uw_ntgetDIBIPV();
	}
	short old_orien, dmOrientation;
	if (papersize!=1)
		dmOrientation = DMORIENT_LANDSCAPE;
	else
		dmOrientation = DMORIENT_PORTRAIT;
	old_orien = SetDefaultPrinterOrientation(dmOrientation);

	CView::OnFilePrint();
	SetDefaultPrinterOrientation(old_orien);
}

/***********************************************************************
c
c   FUNCTION: OnFilePrintPreview()
c
c     Preview current graphic view
c
c   INPUT:  
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLView::OnFilePrintPreview() 
{
	GetDIB();

	CView::OnFilePrintPreview();
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
void CNCLView::SaveToBMP(char *filename)
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
		sprintf_s(msg, sizeof(msg),"Cannot save the BMP file %s !!!", filename);
		MessageBox(msg);
		return;
	}
	END_CATCH
	EndWaitCursor();
	if (!bSuccess)  
	{
		FreeDIB();
		sprintf_s(msg, sizeof(msg),"Cannot save the BMP file %s !!!", filename);
		MessageBox(msg);
	}

	FreeDIB();
}

void CNCLView::reset_redraw()
{
	m_redraw = 1;
}

/*********************************************************************
**    I_FUNCTION : Set_win_type(UM_pkwin_type type)
**
**	         Set the windows type
**			
**    PARAMETERS   
**       INPUT  : 
**          type: type to be set
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLView::Set_win_type(UM_pkwin_type type)
{
	m_Wtype = type;
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
void CNCLView::SetCursor(HCURSOR cursor)
{
	m_current_cursor = cursor;
	if (UW_view_hit!=1)
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
		cx = PKx; 
		cy = PKy;
	}
	else
	{
		cx = m_oldRect.right; 
		cy = m_oldRect.bottom;
	}
	if ((m_current_cursor!=NULL)&&(point.x>0) && (point.y>0) 
				&& (point.x<cx) && (point.y<cy))
	{
		::SetCursor(m_current_cursor);
/*
.....Causes some graphics cards to
.....slow down immensely
.....No reason to call it every time the cursor changes
.....since it is a count and not just a setting
.....Removed this call from all source files
.....Bobby  -  3/22/05
*/
//		::ShowCursor(TRUE);
	}
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
LRESULT CNCLView::OnNcHitTest( CPoint point )
{
	if (m_current_cursor!=NULL)
	{
		::SetCursor(m_current_cursor);
		::ShowCursor(TRUE);
	}
	UW_view_hit = 1;
	return CView::OnNcHitTest(point);
}
extern "C" void uw_setview_cursor (HCURSOR cur, int flag)
{
	if (flag==0)
		((CNCLView*)NCL_Current_View)->m_current_cursor = cur;
	else
		((CNCLView*)NCL_Current_View)->SetCursor(cur);
}

extern "C" void uw_ntreset_redraw()
{
	((CNCLView*)NCL_Main_View)->reset_redraw();
}
/***********************************************************************
**
**   FUNCTION: AddFloatMenu(CPoint pt, char* input_text)
**       added a float menu
**
**   INPUT: pt: the position the float menu display
**			input_text: text include the menu info we need add
**
**   OUTPUT :None
**   RETURN: None   
**
**********************************************************************/
void CNCLView::AddFloatMenu(CPoint pt, char* input_text)
{
	int menunum1, itemnum1;
	char menudata[100];
	int menu_desgn = 0;
	strcpy_s(menudata, sizeof(menudata), input_text);
	if (strncmp(menudata, "CToolmenu2", 10)==0)
		sscanf_s (menudata, "CToolmenu2 %d, %d", &menunum1, &itemnum1);
	else if (strncmp(menudata, "CToolmenu", 9)==0)
		sscanf_s (menudata, "CToolmenu %d, %d", &menunum1, &itemnum1);
	else if (strncmp(menudata, "CNCLMenu", 8)==0)
		sscanf_s (menudata, "CNCLMenu %d, %d", &menunum1, &itemnum1);
	else if (strncmp(menudata, "CNCLStatButton2", 15)==0)
		sscanf_s (menudata, "CNCLStatButton2 %d, %d", &menunum1, &itemnum1);
	else if (strncmp(menudata, "CNCLStatButton", 14)==0)
		sscanf_s (menudata, "CNCLStatButton %d, %d", &menunum1, &itemnum1);
	else if (strcmp(menudata, "MenuDesign")==0)
	{
		menu_desgn = 1;
	}
	else
/*
.....should not come here
*/
		return;
/*
......somehow, we have problem execute debug following here (I think it's time issue), just send a message to frame level
......and return here first
*/
	NCL_MainFrame->m_addmenu = -1;
	NCL_MainFrame->m_additem = -1;
	NCL_MainFrame->m_menu_desgn = menu_desgn;
	if (menu_desgn)
	{
		NCL_MainFrame->m_remove_menu = -1;
	}
	else
	{
		NCL_MainFrame->m_remove_menu = menunum1;
		NCL_MainFrame->m_remove_item = itemnum1;
	}
	NCL_MainFrame->m_add_menupos[0] = -1;
	NCL_MainFrame->m_add_menupos[1] = -1;
	NCL_MainFrame->m_add_menupos[2] = -1;
	NCL_MainFrame->m_add_menupos[3] = pt.x;
	NCL_MainFrame->m_add_menupos[4] = pt.y;
	NCL_MainFrame->PostMessage(WM_COMMAND, UW_ADDNEW_MENUBAR);
	return;
}
void CNCLView::OnLButtonDblClk(UINT nFlags, CPoint point) 
{	
	int save_seg, sav_cur;
	CView::OnLButtonDown(nFlags, point);
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
//test
if (S_pick_start)
	return;
S_pick_start = 1;
//end test
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
/*
......check if any geometry is under this position and put into the picking stack
*/
/*
......we will get the select first if already have segment selected
*/
	sav_cur = uw_ntgetcur_cursor();
	save_seg = ud_getn_pick_segs();
	if (save_seg==0)
		uw_glput_pikgeom(point.x, m_oldRect.bottom - point.y);
	save_seg = ud_getn_pick_segs();
	
	NCL_mouse_func = 1;
	if (jmpflag == 0)
	{
		uz_mouse_functions("mouse_left", 0);
	}
	else
		NCL_mouse_func = 0;
/*
.....if pick stack is used, reset the stack,
.....otherwise, do nothing
*/
	if (save_seg!=ud_getn_pick_segs())
		ncl_reset_pikgeom();
	uw_ntsetcursor(sav_cur);
//test
	S_pick_start = 0;
done:;
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_LBUTTONDBLCLK, WM_LBUTTONDBLCLK, PM_REMOVE));
	while(::PeekMessage(&msg, m_hWnd, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE));
	UD_UNMARK(jmpflag);
//test
	S_pick_start = 0;
}

