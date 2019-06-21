/********************************************************************* 
**  NAME:  wsntgphwin.cpp
**
**			Native WinNT graphic/picture window functions
**			implementation of CNCLGrphicWin class functions
**	CONTAINS: CNCLGrphicWin class functions
**			all functions declared in wsntgphwin.h
**
**    COPYRIGHT 2005 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntgphwin.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:26
*********************************************************************/

#include "usysdef.h"
#include "mpocket.h"
#include "dmotif.h"
#include "wsntstdafx.h"
#include "wsntgraphic.h"
#include "wsntgphwin.h"
#include "wsntcfunc.h"
#include "wsntframe.h"
#include "wsntdoc.h"
#include "wsntview.h"

extern CWnd *NCL_CurrentPKT_View;
extern CMainFrame *NCL_MainFrame;
extern CWnd *NCL_Main_View;
extern CDialog *Pocket_Win[UM_MAX_POCKET];
extern "C" int UW_pic_size[2];
extern "C" int uw_ntsave_prect(UM_pkwin_type type, int left, int top, int right, int bottom);
/***********************************************************************
**
**   FUNCTION: CNCLGraphicWin(CWnd* pParentWnd, UM_pkwin_type)
**
**              Constructor of class CNCLGraphicWin
**
**   INPUT:  pParentWnd: parent window
**           type    = Type of window to open.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLGraphicWin::CNCLGraphicWin(CWnd* pParentWnd, char *filename, UM_pkwin_type type) : CDialog(CNCLGraphicWin::IDD, pParentWnd)
{
	m_Wtype = type;
	strcpy_s(m_file, sizeof(m_file), filename);
	m_picture.Load(m_file);

	CSize sizeTotal;
	sizeTotal.cx = m_picture.m_wid;
	sizeTotal.cy = m_picture.m_height;
	m_picwin = NULL;
	m_init = 0;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLGraphicWin
**
**              Destructor of class CNCLGraphicWin
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLGraphicWin::~CNCLGraphicWin()
{
}

void CNCLGraphicWin::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNCLGraphicWin)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CNCLGraphicWin, CDialog)
	//{{AFX_MSG_MAP(CNCLGraphicWin)
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_SIZE()
	ON_WM_PAINT()
	ON_WM_CLOSE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
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
int CNCLGraphicWin::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CDialog::OnCreate(lpCreateStruct) == -1)
		return -1;
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
char * CNCLGraphicWin::GetWin()
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
void CNCLGraphicWin::OnPaint() 
{
	CDialog::OnPaint();	

	if (IsIconic())
		return;

	CRect rect;
	m_picwin = GetDlgItem(IDC_PICTURE_BOX);
	m_picwin->GetWindowRect(&rect);
	m_pDC = m_picwin->GetDC();
	if (m_picture.m_picture==NULL)
	{
/*
......output text String
*/
		int cy = rect.Height();
		m_pDC->TextOut(10, cy/2, "Picture not available", 21);
		return;
	}
	m_picwin = GetDlgItem(IDC_PICTURE_BOX);
	m_pDC = m_picwin->GetDC();
	m_picture.UpdateSizeOnDC(m_pDC);

	m_picture.Show(m_pDC, CPoint(0,0), CPoint(rect.Width(), rect.Height()), 0,0);
}

/***********************************************************************
**
**   FUNCTION: OnDestroy() 
**
**		OnDestroy is called after the CNCLGraphicWin object 
**		is removed from the screen. 
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLGraphicWin::OnDestroy() 
{
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
void CNCLGraphicWin::OnSize( UINT nType, int cx, int cy )
{
	if ((m_picwin==NULL)||(IsIconic()))
	{
		CWnd::OnSize(nType, cx, cy );
		return;
	}
	int cx1 = cx-4;
	int cy1 = cy-4;
	int x,y;
	x=y=2;
	int right, bottom;
	float rat = (float)m_picture.m_height/(float)m_picture.m_wid;
	float rat2 = (float)cy1/(float)cx1;
	if (rat<rat2)
	{
		right = cx1;
		bottom = cx1*rat;
	}
	else
	{
		bottom = cy1;
		right = cy1/rat;
	}
	m_picwin->MoveWindow(x, y, right, bottom);
	UW_pic_size[0] = cx1;
	UW_pic_size[1] = cy1;
	CWnd::OnSize(nType, cx, cy );
	RedrawWindow();
}
/***********************************************************************
c
c   FUNCTION: OnClose()
c
c           callback for close graphic display window.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLGraphicWin::OnClose()
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
	CDialog::OnClose();
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
BOOL CNCLGraphicWin::OnInitDialog()
{
	CDialog::OnInitDialog();
/*
.....resize the picture box to same size as picture
*/
	m_picwin = GetDlgItem(IDC_PICTURE_BOX);
	int wid, height;
	wid = m_picture.m_wid;
	height = m_picture.m_height;
	CDC *pDC = m_picwin->GetDC();
	m_picture.CreatePicBitmap(pDC);

	int right, bottom;
	float rat = (float)height/(float)wid;
	float rat2 = (float)UW_pic_size[1]/(float)UW_pic_size[0];
	int x,y;
	x=y=2;
	if (rat<rat2)
	{
		right = UW_pic_size[0];
		bottom = UW_pic_size[0]*rat;
	}
	else
	{
		bottom = UW_pic_size[1];
		right = UW_pic_size[1]/rat;
	}
	m_picwin->MoveWindow(x, y, right+4, bottom+4);
/*
.....set title
*/
	char title[400];
	sprintf_s(title, sizeof(title),"Graphic file: %s", m_file);
	SetWindowText(title);
	m_init = 1;
	return 0;
}
