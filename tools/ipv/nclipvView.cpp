/********************************************************************* 
**  NAME:  nclipvview.cpp
**
**			implementation of the CNclipvView class functions
**		CONTAINS: CNclipvView class functions
**			all functions declared in nclipvview.h
**			extern "C" void nclipv_win_context()
**			uw_setview_cursor ()
**
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclipvView.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       02/08/16 , 09:15:03
*********************************************************************/
#include "wsntstdafx.h"
#include "nclipv.h"
#include "wsntdoc.h"
#include "nclipvView.h"
#include "dmotif.h"
#include "zkeysym.h"
//#include "wsntframe.h"
#include "NclipvFrm.h"
#include "wsntglfunc.h"
#include "wsntcfunc.h"
#include "wsglfun.h"
#include "winspool.h"
#include "mpocket.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int PKx,PKy, GP_PKx, GP_PKy;
extern CMainFrame *NCL_MainFrame;
CClientDC *uw_curIPVhDC;
CWnd *NCL_CurrentPKT_View;
CWnd *NCL_Current_View;
CWnd *NCL_Main_View = NULL;
HWND UW_NTgraphicIPV, UW_NTgraphic;
extern "C" int NCL_mouse_func;
extern "C" int uw_gllight_on(int);
extern "C" int UW_pocket_mode;
extern "C" int UW_light_reset;
extern BOOL WINAPI uw_ntpaintDIB(HDC hDC, LPRECT lpDCRect, HDIB hDIB, 
					 LPRECT lpDIBRect, HPALETTE hPal);
extern HDIB WINAPI  uw_cywin_DIB(HWND hWnd, int size, int fit, int bcolor);
extern BOOL WINAPI uw_saveDIB(HDIB hDib, HANDLE file);
extern DWORD     WINAPI  DIBWidth (LPSTR lpDIB);
extern DWORD     WINAPI  DIBHeight (LPSTR lpDIB);
extern "C" char *LW_window;
extern "C" int UW_disp_buf;
int UW_view_hit = -1;
extern "C" UU_LOGICAL LW_active;
extern "C" int UZ_nclipv_view;
extern "C" int UV_dynview_active, UV_dynwheel;
extern "C" void um_set_screen_area(UM_pkwin_type);
extern "C" void ipv_resize_window();
extern "C" char * uu_malloc(int);
extern "C" void uu_free(char*);
extern "C" void uw_glset_context(UM_pkwin_type which, UU_LOGICAL force);
extern "C" void uw_glset_dirty_flag(UU_LOGICAL flag);
extern HDIB WINAPI uw_cypixels_DIB(unsigned char *pixels, int width, int height, int size, int paper_size, int fit, int bcolor);

extern "C" void uw_ntsetcursor(int cursorno);
extern "C" int uw_ntgetcur_cursor();
extern "C" void  um_set_pocket_graphics(UM_pkwin_type type);
extern "C" int uz_mouse_functions(char*, int);

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
// CNclipvView

IMPLEMENT_DYNCREATE(CNclipvView, CView)

BEGIN_MESSAGE_MAP(CNclipvView, CView)
	//{{AFX_MSG_MAP(CNclipvView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard printing commands
	ON_COMMAND(ID_FILE_PRINT, OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, OnFilePrintPreview)
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
	
	ON_WM_LBUTTONDOWN()
	ON_WM_MBUTTONDOWN()
	ON_WM_RBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_MBUTTONUP()
	ON_WM_RBUTTONUP()
	ON_WM_MOUSEWHEEL()
 
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNclipvView construction/destruction

/***********************************************************************
c
c   FUNCTION: CNclipvView()
c
c              Constructor of class CNclipvView
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CNclipvView::CNclipvView()
{
	m_cPalette = NULL;
	m_hDIB	   = NULL;
	m_palDIB   = new CPalette;
	m_current_cursor = LoadCursor(NULL, IDC_ARROW);
	m_papersize = 0;
	m_fit = 1;
	m_bcolor = 0;
	m_current_cursor = LoadCursor(NULL, IDC_ARROW);
	m_pcenter = 0;
}

/***********************************************************************
c
c   FUNCTION: ~CNclipvView()
c
c              Destructor of class CNclipvView
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CNclipvView::~CNclipvView()
{
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
BOOL CNclipvView::PreCreateWindow(CREATESTRUCT& cs)
{
/*
......An OpenGL window must be created with the following flags and must not
......include CS_PARENTDC for the class style. 
*/
	CNclipvApp *app = (CNclipvApp *)AfxGetApp();
	cs.lpszClass = (const char*) (app->m_strMyClassName);
	cs.style |= WS_CLIPSIBLINGS | WS_CLIPCHILDREN;

	return CView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
// CNclipvView drawing

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
void CNclipvView::OnDraw(CDC* pDC)
{
	static int flushing = 0;
	CNCLDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	if (pDC->IsPrinting()) 
	{
		PrintDIB(pDC);
		flushing = 0;
		return;
	}
	NCL_CurrentPKT_View = this;
	NCL_Current_View = this;
	if (UV_dynview_active) 
	{
		return;
	}
	UZ_nclipv_view = 1;
	um_set_screen_area(UM_IPV_WINDOW);
	if (LW_active)
		ipv_resize_window();
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
int CNclipvView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CView::OnCreate(lpCreateStruct) == -1)
		return -1;

	NCL_CurrentPKT_View = this;
	NCL_Main_View = NCL_Current_View = this;
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
**
**   FUNCTION: OnDestroy() 
**
**		OnDestroy is called after the CNclipvView object 
**		is removed from the screen. Delete openGL
**		context here 
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNclipvView::OnDestroy() 
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
void CNclipvView::OnSize(UINT nType, int cx, int cy) 
{
	static int old_cx = -1;
	static int old_cy = -1;
	if (NCL_MainFrame==NULL) return;

	CView::OnSize(nType, cx, cy);
		
	if (IsIconic())
	{
		return;
	}
	NCL_CurrentPKT_View = this;
	NCL_Current_View = this;

	PKx = cx ; 
	PKy = cy;
	m_oldRect.right = cx;
	m_oldRect.bottom = cy;

	RECT rect;
	GetWindowRect(&rect);
	um_set_screen_area(UM_IPV_WINDOW);
	UZ_nclipv_view = 1;
	if (UW_NTgraphicIPV&&LW_active)
		ipv_resize_window();
//	uw_ntsave_prect(m_Wtype, rect.left, rect.top, rect.right, rect.bottom);
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
void CNclipvView::Init()
{
	if (NCL_MainFrame==NULL)
		return;

	UW_pocket_mode = UU_TRUE;
	UW_light_reset = UU_TRUE;
/*
.....Setup the colormap, actually, setup the color setting
*/
	uw_gl.maxgcolors = 256;
	inicolormap();
	m_hrc = wglCreateContext(m_pDC->GetSafeHdc());
	wglMakeCurrent(m_pDC->GetSafeHdc(), m_hrc);
	uw_gllight_on(UU_TRUE);
	glDrawBuffer_d(GL_FRONT_AND_BACK);
	glClearDepth_d(1.0f);
	glEnable_d(GL_DEPTH_TEST);

	UW_NTgraphicIPV = m_hWnd;
	LW_window = (char*)(this->GetSafeHwnd());

	GetClientRect(&m_oldRect);
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
int  CNclipvView::inicolormap()
{
	int i,n;
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
BOOL CNclipvView::bSetupPixelFormat()
{
	static PIXELFORMATDESCRIPTOR pfdipv = 
	{
        sizeof(PIXELFORMATDESCRIPTOR),  // size of this pfd
        1,                              // version number
        PFD_DRAW_TO_WINDOW |            // support window
        PFD_SUPPORT_OPENGL |          // support OpenGL
 		PFD_SWAP_COPY |                 // Copy back buffer to front on SwapBuffer
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
    int pixelformat;

	pfdipv.iPixelType = PFD_TYPE_RGBA;
	pfdipv.cStencilBits  = 8;

	if ( (pixelformat = ChoosePixelFormat(m_pDC->GetSafeHdc(), &pfdipv)) == 0 )
	{
		MessageBox("ChoosePixelFormat for NCLIPV failed");
		return FALSE;
	}

	if (SetPixelFormat(m_pDC->GetSafeHdc(), pixelformat, &pfdipv) == FALSE)
	{
		MessageBox("SetPixelFormat for NCLIPV failed");
		return FALSE;
	}
	int n = ::GetPixelFormat(m_pDC->GetSafeHdc());
    ::DescribePixelFormat(m_pDC->GetSafeHdc(), n, sizeof(pfdipv), &pfdipv);
	if (!(pfdipv.dwFlags & PFD_SWAP_COPY))
		UW_disp_buf = 1;
    return TRUE;
}


/***********************************************************************
**
**   FUNCTION: OnEraseBkgnd(CDC* pDC) 
**			The framework calls this member function 
**			when the CNclipvView object background needs 
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
BOOL CNclipvView::OnEraseBkgnd(CDC* pDC) 
{
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CNclipvView printing

BOOL CNclipvView::OnPreparePrinting(CPrintInfo* pInfo)
{
	// default preparation
	return DoPreparePrinting(pInfo);
}

void CNclipvView::OnBeginPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add extra initialization before printing
}

void CNclipvView::OnEndPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add cleanup after printing
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
void CNclipvView::PrintDIB(CDC* pDC)
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
	rcDst.left   = 0;
	rcDst.top    = 0;
	rcDst.right  = cxPage;
	rcDst.bottom = (int)(1.0 * cyDIB * cxPage * cyInch / cxDIB / cxInch);
	int shiftx, shifty;
	shiftx = 0;
	shifty = (cyPage - rcDst.bottom)/2;
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
void CNclipvView::GetDIB()
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
	uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
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
void CNclipvView::FreeDIB()
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
void CNclipvView::OnFilePrint() 
{
	GetDIB();

	short old_orien, dmOrientation;
	if (m_papersize!=1)
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
void CNclipvView::OnFilePrintPreview() 
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
void CNclipvView::SaveToBMP(char *filename)
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
		sprintf(msg, "Cannot save the BMP file %s !!!", filename);
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
		sprintf(msg, "Cannot save the BMP file %s !!!", filename);
		MessageBox(msg);
		return;
	}
	END_CATCH
	EndWaitCursor();
	if (!bSuccess)  
	{
		FreeDIB();
		sprintf(msg, "Cannot save the BMP file %s !!!", filename);
		MessageBox(msg);
	}

	FreeDIB();
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
void CNclipvView::SetContext()
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
char * CNclipvView::GetWin()
{
	HWND hWndParent = this->GetSafeHwnd();
	return (char *)hWndParent;
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
void CNclipvView::SetCursor(HCURSOR cursor)
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
		
	cx = PKx; 
	cy = PKy;
	if ((m_current_cursor!=NULL)&&(point.x>0) && (point.y>0) 
				&& (point.x<cx) && (point.y<cy))
	{
		::SetCursor(m_current_cursor);
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
LRESULT  CNclipvView::OnNcHitTest( CPoint point )
{
	if (m_current_cursor!=NULL)
	{
		::SetCursor(m_current_cursor);
		::ShowCursor(TRUE);
	}
	UW_view_hit = 1;
	return CView::OnNcHitTest(point);
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
void CNclipvView::AddFloatMenu(CPoint pt, char* input_text)
{
	int menunum1, itemnum1;
	char menudata[100];
	int menu_desgn = 0;
	strcpy_s(menudata, sizeof(menudata), input_text);
	if (strncmp(menudata, "CToolmenu", 9)==0)
		sscanf_s (menudata, "CToolmenu %d, %d", &menunum1, &itemnum1);
	else if (strncmp(menudata, "CNCLMenu", 8)==0)
		sscanf_s (menudata, "CNCLMenu %d, %d", &menunum1, &itemnum1);
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

/////////////////////////////////////////////////////////////////////////////
// CNclipvView diagnostics

#ifdef _DEBUG
void CNclipvView::AssertValid() const
{
	CView::AssertValid();
}

void CNclipvView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CNCLDoc* CNclipvView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CNCLDoc)));
	return (CNCLDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CNclipvView message handlers

extern "C" void nclipv_win_context()
{
	CNclipvView *pkt_view;
	pkt_view = (CNclipvView *)NCL_CurrentPKT_View;
	pkt_view->SetContext();
}

extern "C" void uw_setview_cursor (HCURSOR cur, int flag)
{
	((CNclipvView *)NCL_Main_View)->SetCursor(cur);
}

extern "C" void uw_ntreset_redraw()
{
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
void CNclipvView::OnLButtonDown(UINT nFlags, CPoint point) 
{	
	int save_seg, sav_cur;
	CView::OnLButtonDown(nFlags, point);
	int jmpflag;
	UD_MARK(jmpflag,UU_TRUE);
/*
......call NCL define mouse function on CHOICE mode
......if it is input mode, the DAS event will handle the function
*/
	if (UD_pickmode)
		goto done;
	sav_cur = uw_ntgetcur_cursor();
	
	NCL_mouse_func = 1;
	if (jmpflag == 0)
	{
		uz_mouse_functions("mouse_left", 0);
	}
	else
		NCL_mouse_func = 0;
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
void CNclipvView::OnMButtonDown(UINT nFlags, CPoint point) 
{	
	int save_seg, sav_cur;
	CView::OnMButtonDown(nFlags, point);
	int jmpflag;
	UD_MARK(jmpflag,UU_TRUE);
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
	NCL_mouse_func = 1;
	if (jmpflag == 0)
	{
		uz_mouse_functions("mouse_middle", 0);
	}
	else
		NCL_mouse_func = 0;
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
void CNclipvView::OnRButtonDown(UINT nFlags, CPoint point) 
{	
	int sav_cur;
	CView::OnRButtonDown(nFlags, point);
	int jmpflag;
	UD_MARK(jmpflag,UU_TRUE);

	if (UD_pickmode)
		goto done;
	sav_cur = uw_ntgetcur_cursor();
	NCL_mouse_func = 1;
	if (jmpflag == 0)
	{
		uz_mouse_functions("mouse_right", 0);
	}
	else
		NCL_mouse_func = 0;
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
void CNclipvView::OnLButtonUp(UINT nFlags, CPoint point) 
{	
	CView::OnLButtonUp(nFlags, point);

	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
	NCL_mouse_func = 0;
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
void CNclipvView::OnMButtonUp(UINT nFlags, CPoint point) 
{	
	CView::OnMButtonUp(nFlags, point);
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
	NCL_mouse_func = 0;
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
void CNclipvView::OnRButtonUp(UINT nFlags, CPoint point) 
{	
	CView::OnRButtonUp(nFlags, point);
	MSG msg;
	while(::PeekMessage(&msg, m_hWnd, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
	NCL_mouse_func = 0;
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
BOOL CNclipvView::OnMouseWheel(UINT nFlags, short zDelta, CPoint pt)
{	
	int sav_cur;
	int status = CView::OnMouseWheel(nFlags, zDelta, pt);
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
		if (zDelta>0)
			uz_mouse_functions("wheel_up", 0);
		else
			uz_mouse_functions("wheel_down", 0);
	}
	else
		NCL_mouse_func = 0;
	uw_ntsetcursor(sav_cur);
	MSG msg;
	while(::PeekMessage(&msg, NULL, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE));
	UD_UNMARK(jmpflag);
	return status;
}
