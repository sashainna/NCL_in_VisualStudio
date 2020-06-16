/************************************************************************
c
c   FILE NAME: GLtestView.cpp
c
c	 CONTAINS: 
c		View class for draw openGL
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        GLtestView.cpp , 21.1
c     DATE AND TIME OF LAST  MODIFICATION
c        12/10/09 , 18:01:43
c
c**********************************************************************
*/
#include "glStdafx.h"
#include "GLtest.h"

#include "GLtestDoc.h"
#include "GLtestView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int load_exe_jf();
extern "C" int draw1();

IMPLEMENT_DYNCREATE(COGView, CView)

BEGIN_MESSAGE_MAP(COGView, CView)
	//{{AFX_MSG_MAP(COGView)
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_SIZE()
	ON_WM_ERASEBKGND()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

COGView::COGView()
{
}

COGView::~COGView(){}

BOOL COGView::PreCreateWindow(CREATESTRUCT& cs)
{
	cs.style |= WS_CLIPSIBLINGS | WS_CLIPCHILDREN;
	return CView::PreCreateWindow(cs);
}

void COGView:: OnDraw(CDC* pDC)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	DrawScene();
}

BOOL COGView::OnEraseBkgnd(CDC* pDC) 
{ 
	return TRUE; 
}

int COGView::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CView::OnCreate(lpCreateStruct) == -1)
		return -1;

	PIXELFORMATDESCRIPTOR pfd =
	{
		sizeof(PIXELFORMATDESCRIPTOR),
		1,		
		PFD_DRAW_TO_WINDOW |
		PFD_SUPPORT_OPENGL |
//		PFD_GENERIC_FORMAT |
		PFD_DOUBLEBUFFER,
		PFD_TYPE_RGBA,
		24, 		 		
		24,	0,		// for Red-component
		24,	0,		// for Green-component
		24,	0,		// for Blue-component
		24,	0,		// for Alpha-component
		0,			// Number of planes
					// of Accumulation buffer
		0,			// for Red-component
		0,			// for Green-component
		0,			// for Blue-component
		0,			// for Alpha-component
		24, 			// Depth of Z-buffer
		1,			// Depth of Stencil-buffer
		0,			// Depth of Auxiliary-buffer
		PFD_MAIN_PLANE,			// Now is ignored
		0,			// Number of planes
		0,			// Now is ignored
		0,			// Color of transparent mask
		0			// Now is ignored
	};

	m_hDC = ::GetDC(GetSafeHwnd());

	int iD = ChoosePixelFormat(m_hDC, &pfd);
	if ( !iD )
	{
		MessageBox("ChoosePixelFormat::Error");
		return -1;
	}

	if ( !SetPixelFormat (m_hDC, iD, &pfd) )
	{
		MessageBox("SetPixelFormat::Error");
		return -1;
	}
    
/*	int n = ::GetPixelFormat(
		m_pDC->GetSafeHdc());
    ::DescribePixelFormat(m_pDC->GetSafeHdc(), n, sizeof(pfd), &pfd);
*/
	int n = ::GetPixelFormat(m_hDC);
	::DescribePixelFormat(m_hDC, n, sizeof(pfd), &pfd);

	if ( !(m_hRC = wglCreateContext (m_hDC)))
	{
		MessageBox("wglCreateContext::Error");
		return -1;
	}

	if ( !wglMakeCurrent (m_hDC, m_hRC))
	{
		MessageBox("wglMakeCurrent::Error");
		return -1;
	}

//	DrawScene();
	return 0;
}

void COGView::DrawScene()
{

	load_exe_jf();
//	draw1();
}	

void COGView::OnSize(UINT nType, int cx, int cy) 
{
	CView::OnSize(nType, cx, cy);
	double dAspect = cx<=cy ? double(cy)/cx : double(cx)/cy;
}

void COGView::OnDestroy() 
{
    ::wglMakeCurrent(NULL,  NULL);

    if (m_hRC)
        ::wglDeleteContext(m_hRC);

	CView::OnDestroy();
}

/**********************************************************************
**    I_FUNCTION : test_get_filename(CWnd* parent, char *title, LPCTSTR filter, char* fnam, int *nc)
**			popup a file browser dialog box 
**			
**    PARAMETERS   
**       INPUT  : 
**				title:  file browser title
**				filter: file filter.
**				fname:  default file name
**				nc:     number of character of filename
**       OUTPUT :  
**				fname:  use enter file name
**				nc:     number of character of filename
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
extern "C" int test_get_filename(char *title, char * filter, char* fnam, int *nc)
{
	CString FileName;
	char  *temp;
	DWORD dwFlags = OFN_NOCHANGEDIR | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;

	fnam[0] = '\0';
	CFileDialog *filedlg = new CFileDialog(TRUE, NULL, fnam, dwFlags,
			filter, NULL);
	filedlg->m_ofn.lpstrTitle = title;
	if (filedlg->DoModal()==IDCANCEL)
	{
		*nc = 0;
		fnam[0] = '\0';
		goto done;;
	}
	FileName = filedlg->GetPathName();
	*nc = FileName.GetLength();
	temp = FileName.GetBuffer(*nc);
	strcpy(fnam, temp);
done:;
	delete filedlg;	
	return 0;
}

