/************************************************************************
**
**   FILE NAME: wsntpicsel.cpp
**
**	 Description - Functions implementation for
**		CNCLPicSelWin class it's a window display a picture file
**	 CONTAINS: 
**		all functions declared in wsntpicsel.h
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntpicsel.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			01/20/17 , 12:24:03
************************************************************************
*/
#include "wsntstdafx.h"
#include "wsntpicsel.h"
#include "wsntfsview.h"
#include "wsntformbar.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int UW_picture_pos;
extern "C" int ul_open_mod_file(char*, char*, char*, char*, char*, int, FILE**);
extern "C" char * uu_malloc(int);
extern "C" void uu_free(char*);

/////////////////////////////////////////////////////////////////////////////
// CNCLPicSelWin

/***********************************************************************
c
c   SUBROUTINE:  CNCLPicSelWin
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLPicSelWin::CNCLPicSelWin(char *name, char* filename, CWnd *parent)
{
	WNDCLASS wc;

	::ZeroMemory(&wc, sizeof(wc));
	wc.hInstance	= ::AfxGetInstanceHandle();
	wc.lpfnWndProc	= ::DefWindowProc;
	wc.hCursor		= (HCURSOR)::LoadCursor(NULL, MAKEINTRESOURCE(IDC_ARROW));
	wc.lpszClassName= "NCLPicSelWnd";
	::AfxRegisterClass(&wc);

	m_area_num = 0;
	m_draw_areanum = -1;
	strcpy(m_name, name);

	UX_pathname filen;
	strcpy(filen, filename);
	int status = ul_open_mod_file("UU_USER_SETTINGS", "forms", "UD_FORMDIR", (char*)UU_NULL, 
		filen, 0, (FILE**)UU_NULL);
	m_filename = filen;
//	if (status==0)
		m_filename = filen;
//	else
//		m_filename = "";
	m_parent = parent;

	m_tiptext = new char[80];
	m_tiptext[0] = '\0';
	m_buttondown = 0;
	m_picture.Load(m_filename);
	m_MouseTrack = FALSE;
}

CNCLPicSelWin::~CNCLPicSelWin()
{
	for (int i=0; i<m_area_num;i++)
	{
		if (m_params[i]!=NULL)
			uu_free(m_params[i]);
		if (m_tooltext[i]!=NULL)
			uu_free(m_tooltext[i]);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext)
c
c   FUNCTION:  This function Create a CNCLPicSelWin window
c
c   INPUT:  dwStyle: window style
c			rect: window size
c			nID: wndow ID
c			pContext: 
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CNCLPicSelWin::Create (DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext)
{
	ASSERT((rect.bottom - rect.top) > (PADDING+BORDER_WIDTH) * 2);
	BOOL ret = CWnd::Create(NULL, NULL, dwStyle, rect, pParentWnd, nID, pContext);
	m_RectTips.Create(this);
	m_picture.CreatePicBitmap(GetDC());
	return ret;
}

BEGIN_MESSAGE_MAP(CNCLPicSelWin, CWnd)
	//{{AFX_MSG_MAP(CNCLPicSelWin)
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_PAINT()
	ON_WM_LBUTTONDOWN()
	ON_WM_MOUSEMOVE()
	ON_WM_MOUSELEAVE()
	ON_WM_LBUTTONUP()
	ON_WM_SIZE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
/***********************************************************************
c
c   FUNCTION: IsCursorOn()
c
c    check if the current mouse cursor is inside the window
c
c   INPUT:  point: mouse cursor position to be checked
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CNCLPicSelWin::IsCursorOn()
{
	CRect rect;
	GetWindowRect(&rect);
	POINT pt;
	GetCursorPos(&pt);
	if (rect.PtInRect(pt))
		return 1;
	else
		return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  Paint
c
c   FUNCTION:  Paint the CNCLPicSelWin window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLPicSelWin::Paint()
{
	CRect rect;
	GetWindowRect(&rect);
	m_pDC = GetDC();
	if (m_picture.m_picture==NULL)
	{
/*
......output text String
*/
		int cy = rect.Height();
		m_pDC->TextOut(10, 10, "Picture not available", 21);
		return;
	}
	m_picture.UpdateSizeOnDC(m_pDC);
	m_picture.Show(m_pDC, CPoint(0,0), CPoint(rect.Width(), rect.Height()), 0,0);
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
void CNCLPicSelWin::Reset_picture(char* filename)
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
}
/////////////////////////////////////////////////////////////////////////////
// CNCLPicSelWin message handlers

void CNCLPicSelWin::OnDestroy() 
{
	CWnd::OnDestroy();
}

void CNCLPicSelWin::OnPaint() 
{
	CWnd::OnPaint();
	Paint();
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
void CNCLPicSelWin::OnLButtonDown(UINT nFlags, CPoint point) 
{
	m_buttondown = 1;
	m_StartPoint = 	point;
	SetFocus();
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
void CNCLPicSelWin::OnLButtonUp(UINT nFlags, CPoint point) 
{
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;
	ClipCursor(NULL);
	CWnd::OnLButtonUp(nFlags, point);
/*
.....if buttondown is not happened here, don't do picture function
*/
	if (m_buttondown==0)
		return;
	m_buttondown = 0;

	char text[1024];
	int i = GetAreaNum(point);
	if (i<0)
		return;
	if (m_params[i]!=NULL)
		strcpy(text, m_params[i]);
	else
		text[0] = '\0';
	if (m_parent->IsKindOf(RUNTIME_CLASS(CNCLFormScrollView)))
		((CNCLFormScrollView*)m_parent)->OnPicAreaClick((WPARAM)text, (LPARAM)m_picID[i]);
	else if (m_parent->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
		((CNCLFormBar*)m_parent)->PicAreaClick(m_picID[i], text);
}

/***********************************************************************
**
**   FUNCTION: GetAreaNum(CPoint point)
**
**       return the picture area number which the 'point' is in
**
**   INPUT:  point: mouse cursor position to be checked
**
**   OUTPUT :   None
**   RETURN:    area number index
**
**********************************************************************/
int CNCLPicSelWin::GetAreaNum(CPoint point)
{
	CRect rect, area;
/*
.....do not use physical window rect but use the image rectage
*/
	GetClientRect(&rect);
	if ((m_picture.m_wid!=0)&&(m_picture.m_height!=0))
	{
		int right, bottom;
		float rat = (float)m_picture.m_height/(float)m_picture.m_wid;
		float rat2 = (float)rect.Height()/(float)rect.Width();
		if (rat<rat2)
		{
			right = rect.Width();
			bottom = rect.Width()*rat;
		}
		else
		{
			bottom = rect.Height();
			right = rect.Height()/rat;
		}
		rect.right = rect.left + right;
		rect.bottom = rect.top + bottom;
	}
	int area_num = -1;
	int temp_area = -1;
	for (int i=0; i<m_area_num;i++)
	{
		area.left = (m_area[i].left*rect.Width())/100.0;
		area.top = (m_area[i].top*rect.Height())/100.0;
		area.right = (m_area[i].right*rect.Width())/100.0;
		area.bottom = (m_area[i].bottom*rect.Height())/100.0;

		if (area.PtInRect(point))
		{
/*
......need check if the picked area is valid
......add this since sometime we have many form field
......put in the same position but only one active (visible)
*/
			if (((CNCLFormScrollView*)m_parent)->ifFormViewValidId(m_picID[i]))
			{
				area_num = i;
				break;
			}
			temp_area = i;
		}
	}
/*
......if all picture currespond fields all insivible
......return the last hidden field number
......add those since sometimes we use hiddle field
......to do some callback function
*/
	if (area_num==-1)
		area_num = temp_area;
	return area_num;
}
/***********************************************************************
**
**   FUNCTION: ifarea_notchg(CPoint point)
**
**       check if the 'point' is in the same area as before
**
**   INPUT:  point: mouse cursor position to be checked
**
**   OUTPUT :   None
**   RETURN:    1: same
**				0: not the same
**
**********************************************************************/
int CNCLPicSelWin::ifarea_notchg(CPoint point)
{
	int area_num = GetAreaNum(point);
	if (area_num==m_draw_areanum)
		return 1;
	else
		return 0;
}
void CNCLPicSelWin::OnMouseLeave() 
{
	m_tiptext[0] = '\0';
	m_draw_areanum = -1;
	m_RectTips.Close();
	m_MouseTrack = FALSE;
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
void CNCLPicSelWin::OnMouseMove(UINT nFlags, CPoint point) 
{
	int iX = m_StartPoint.x - point.x;
	int iY = m_StartPoint.y - point.y;
	if((iX*iX + iY*iY) > 9)
		m_buttondown = 0;

	if ((ifarea_notchg(point))&&(UW_picture_pos==0))
	{
		if (m_tiptext[0] != '\0')
		{
			m_RectTips.SetText(m_tiptext);
			m_RectTips.Show(point);
		}
		else
			m_RectTips.Close();
		goto done;
//		CWnd::OnMouseMove(nFlags, point);
//		return;
	}
	int i = GetAreaNum(point);
	if (UW_picture_pos==0)
	{
		if ((i>=0)&&(m_draw_areanum!=i))
		{
			if (m_tooltext[i]!=NULL)
				strcpy(m_tiptext, m_tooltext[i]);
			else
			{
				m_tiptext[0] = '\0';
			}
		}
		else if (i<0)
			m_tiptext[0] = '\0';
	}
	else
	{
		CRect rect;
//
/*
.....do not use physical window rect but use the image rectage
*/
		int height, wid;
		GetClientRect(&rect);
		if ((m_picture.m_wid!=0)&&(m_picture.m_height!=0))
		{
			int right, bottom;
			float rat = (float)m_picture.m_height/(float)m_picture.m_wid;
			float rat2 = (float)rect.Height()/(float)rect.Width();
			if (rat<rat2)
			{
				right = rect.Width();
				bottom = rect.Width()*rat;
			}
			else
			{
				bottom = rect.Height();
				right = rect.Height()/rat;
			}
			rect.right = rect.left + right;
			rect.bottom = rect.top + bottom;
		}
		int posx = (((float)point.x)/((float)rect.Width()))*100+.5;
		int posy = (((float)point.y)/((float)rect.Height()))*100+.5;
		sprintf(m_tiptext, "%d, %d", posx, posy);
	}
	m_draw_areanum = i;
				
	if (m_tiptext[0] != '\0')
	{
		m_RectTips.SetText(m_tiptext);
		m_RectTips.Show(point);
	}
	else
		m_RectTips.Close();
done:;
	if (m_MouseTrack==FALSE)
	{
		TRACKMOUSEEVENT tevent;
		tevent.cbSize = sizeof(TRACKMOUSEEVENT);
		tevent.dwFlags = TME_LEAVE;
		tevent.hwndTrack = this->m_hWnd;
		if (::_TrackMouseEvent(&tevent))
			m_MouseTrack = TRUE;
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
void CNCLPicSelWin::OnSize(UINT nType, int cx, int cy)
{
	CWnd::OnSize(nType, cx, cy);
}

/***********************************************************************
**
**   FUNCTION: set_picarea(UD_PICAREA picarea, UINT pid)
**
**       Set the pciture area
**
**   INPUT:  picarea: picture area structure
**			pid: picture ID
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPicSelWin::set_picarea(UD_PICAREA picarea, UINT pid)
{
	m_area[m_area_num].left = picarea.xmin;
	m_area[m_area_num].top = picarea.ymin;
	m_area[m_area_num].right = picarea.xmax;
	m_area[m_area_num].bottom = picarea.ymax;
	m_picID[m_area_num] = pid;
	int len;

	if (picarea.tooltext!=NULL)
	{
		len = strlen(picarea.tooltext);
	}
	else
		len = 0;
	if (len!=0)
	{
		m_tooltext[m_area_num] = (char *)uu_malloc((len+1)*sizeof(char));
		strcpy(m_tooltext[m_area_num], picarea.tooltext);
	}
	else
		m_tooltext[m_area_num] = NULL;

	if (picarea.params!=NULL)
	{
		len = strlen(picarea.params);
	}
	else
		len = 0;
	if (len!=0)
	{
		m_params[m_area_num] = (char *)uu_malloc((len+1)*sizeof(char));
		strcpy(m_params[m_area_num], picarea.params);
	}
	else
		m_params[m_area_num] = NULL;
	m_area_num++;
}

