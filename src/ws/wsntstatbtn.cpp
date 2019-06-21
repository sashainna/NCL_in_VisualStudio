#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**		FILE NAME: wsntstatbtn.cpp
**
**	 Description - Functions and implementations for
**		CNCLStatButton class
**
**	 CONTAINS:
**		member function of CNCLStatButton
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntstatbtn.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:30
**********************************************************************
*/
#include "wsntstdafx.h"
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxdisp.h >       // MFC OLE automation classes
#include <afxpriv.h>
#include "wsntstatbtn.h"
#include "wsntres.h"
#include "xenv1.h"
#include "wsntStatusBar.h"
#include "dmotif.h"
#include "wsntglfunc.h"
#include "wsntcfunc.h"
#include "zkeysym.h"
#include "wsntframe.h"
#include "wsntdropsource.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif
#define NBUTTONNUM	5000
#define NCHOICEBUTTON 1000

extern CMainFrame *NCL_MainFrame;
extern int UW_addmenu, UW_additem, UW_remove_menu, UW_remove_item;

extern "C" int UW_statbar_size;
extern "C" char UW_statbar_font[20];
extern "C" int ul_open_mod_file(char*, char*, char*, char*, char*, int, FILE**);
extern "C" char * uu_malloc(int);
extern "C" void uu_free(char*);
extern "C" int uz_ntcnvtnum_to_menu(int num, int *menunum);
extern "C" void uw_ntpopchoice(int mflag, int *choice);
extern "C" void ud_upt_UDMmenu(int upt_menu, int upt_item, int add_menu, int add_item, int flag);
extern "C" void ud_del_UDMmenu(int, int);
extern "C" int uw_ntmenu_desgn(int flag, UDM_menu_struc *menu_item);
extern "C" void ud_insert_UDMmenu(int menunum, int select, UDM_menu_struc *menu_item);

extern int UW_drag_obj;

IMPLEMENT_DYNAMIC(CNCLStatButton, CButton)

BEGIN_MESSAGE_MAP(CNCLStatButton, CButton)
	//{{AFX_MSG_MAP(CNCLStatButton)
	ON_WM_CREATE()
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONUP()
	ON_WM_LBUTTONDOWN()
	ON_WM_RBUTTONUP()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  CNCLStatButton
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLStatButton::CNCLStatButton() 
{  
#if (_MFC_VER < 0x0250)
  hwndOwner = NULL; 
#endif 
	m_current_cursor = LoadCursor(NULL, IDC_ARROW);
	m_focus = 0;
	m_parent = NULL;
	m_type = 0;
	m_fid = -1;
	m_picture.m_trans = 1;
	m_TimerID = 0;
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;	
} 
/***********************************************************************
c
c   SUBROUTINE:  ~CNCLStatButton
c
c   FUNCTION:  Destructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLStatButton::~CNCLStatButton()
{
} 
/***********************************************************************
c
c   SUBROUTINE:  SetBitmapFile(char *filename)
c
c   FUNCTION:  Set the bitmap of the button
c
c   INPUT:  filename: new bitmap file
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLStatButton::SetBitmapFile(char *filename)
{
	UX_pathname filen;
	strcpy(filen, filename);
	int status = ul_open_mod_file("UU_USER_SETTINGS", "bitmaps", "NCL_BITMAP", (char*)UU_NULL, 
		filen, 0, (FILE**)UU_NULL);

	if (status==0)
		m_filename = filen;
	else
		m_filename = "";
	m_picture.Load(m_filename);
}

void CNCLStatButton::SetText(CString text)
{
	m_text = text;
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
int CNCLStatButton::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CButton::OnCreate(lpCreateStruct) == -1)
		return -1;
	m_picture.CreatePicBitmap(GetDC());

//	m_txtfont.CreatePointFont (80, "MS Sans Serif");
	int stat;
	if (m_txtfont.m_hObject)
		VERIFY (m_txtfont.DeleteObject ());	
	stat = m_txtfont.CreatePointFont (UW_statbar_size*10, UW_statbar_font);
	if (stat==0)
		m_txtfont.CreatePointFont (UW_statbar_size*10, "COURIER");
	SetFont(&m_txtfont);
	m_ToolTips.Create(this);
}

/***********************************************************************
**
**   FUNCTION: SetButID(UINT fid)
**
**		Set button's ID and update the tooltip of the button
**   
**		INPUT:  fid: button ID
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::SetButID(UINT fid)
{
	m_fid = fid;
	int num, menunum;
	
	if ((fid >= WM_APP)&&(fid<WM_APP+UDM_MAX_MENU)) 
	{
		UX_pathname filename,dir,fname1,fname;
		num = fid-WM_APP;
		uz_ntcnvtnum_to_menu(num, &menunum);
		strcpy_s(filename, UDM_menu[menunum].file);
		ul_break_fname(filename,dir,fname);
		sprintf_s(fname1, "Load %s", fname);
		m_tiptext = fname1;
	}
	else if ((fid >= WM_APP+UDM_MAX_MENU)&&(fid<WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM)) 
	{
		int num, status;
		char descript[256];
		num = fid - (WM_APP+UDM_MAX_MENU);
		status = uz_ntget_dspt(num, descript, 1);
		if (status==-1)
			strcpy_s(descript, "No description");
		m_tiptext = descript;
	}
	else if ((fid >= WM_APP+UDM_MAX_MENU + UZ_MAX_KEYITEM)&&(fid<WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM)) 
	{
		int num, status;
		char descript[256];
		num = fid - (WM_APP+UDM_MAX_MENU+ UZ_MAX_KEYITEM);
		status = uz_ntget_dspt(num, descript, 2);
		if (status==-1)
			strcpy_s(descript, "No description");
		m_tiptext = descript;
	}
	else
	{
		TCHAR szFullText[256];
		AfxLoadString(fid, szFullText);
		AfxExtractSubString(m_tiptext, szFullText, 1, '\n');
	}
}

/***********************************************************************
**
**   FUNCTION: DrawItem(LPDRAWITEMSTRUCT lpDIS)
**
**         Called by the framework when a visual aspect of 
**			an owner-drawn button changes
**
**   INPUT:  lpDIS:   A pointer to a 
**				DRAWITEMSTRUCT structure that contains 
**				information about the type of drawing required.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::DrawItem(LPDRAWITEMSTRUCT lpDIS)
{
	CDC* pDC = CDC::FromHandle(lpDIS->hDC);
	CRect rc(lpDIS->rcItem);

	UINT state = lpDIS->itemState; 
	CRect focusRect, btnRect;
	focusRect.CopyRect(&lpDIS->rcItem); 
	btnRect.CopyRect(&lpDIS->rcItem); 

	focusRect.left += 4;
    focusRect.right -= 4;
    focusRect.top += 4;
    focusRect.bottom -= 4;
      	
	COLORREF cr;
	CRgn rgn;
	rgn.CreateRectRgn(rc.left, rc.top, rc.right, rc.bottom);
	if ((m_type==1)||(m_type==2))
	{
		cr = pDC->GetBkColor();
		CBrush brushFill(cr);
		cr = pDC->GetTextColor();
		pDC->SetTextColor(RGB(0,0,0));
		int nBkMode = pDC->SetBkMode(TRANSPARENT);
		pDC->SelectClipRgn(&rgn);
		pDC->FillRect(&rc, &brushFill);
		if (m_type==1)
			pDC->TextOut(rc.left+2, rc.top, m_text, m_text.GetLength());
		if (m_type==2)
/*
......center
*/
		{
			CSize szText;
			szText = pDC->GetTextExtent(m_text);
			int delx = rc.Width() - szText.cx;
			pDC->TextOut(rc.left+delx/2, rc.top, m_text, m_text.GetLength());
		}
	}
/*
.....seperator
*/
	if (m_type==3)
	{
		int left, top, right, bottom;
		left = right = (rc.left + rc.right)*.5;
		top = rc.top;
		bottom = rc.bottom;
		DrawLine(pDC, left, top, right, bottom, RGB(100,100,100));							// Across top
	}
	if (m_type==0)
	{
		DrawBitmap(pDC);
	}
} 


/***********************************************************************
**
**   FUNCTION: DrawFrame(CDC *DC, CRect R, int Inset)
**
**         Draw the button frame
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::DrawFrame(CDC *DC, CRect R, int Inset)
{ 
	COLORREF dark, light, tlColor, brColor;
	int i, m, width;
	width = (Inset < 0)? -Inset : Inset;
	if (Inset==0)
		width = 1;
	for (i = 0; i < width; i += 1) {
		m = 255 / (i + 2);
		dark = PALETTERGB(m, m, m);
		m = 192 + (63 / (i + 1));
		light = PALETTERGB(m, m, m);
		  
	  	if ( width == 1 ) {
			light = RGB(255, 255, 255);
			dark = RGB(128, 128, 128);
		}
		
		if ( Inset < 0 ) {
			tlColor = RGB(0, 0, 128);
			brColor = light;
		}
		else if ( Inset > 0 )
		{
			tlColor = light;
			brColor = dark;
		}
		else
		{
			tlColor = RGB(255,200,100);
			brColor = RGB(255,200,100);
		}
		
		DrawLine(DC, R.left, R.top, R.right, R.top, tlColor);							// Across top
		DrawLine(DC, R.left, R.top, R.left, R.bottom, tlColor);							// Down left
	  
		if ( (Inset < 0) && (i == width - 1) && (width > 1) ) {
			DrawLine(DC, R.left + 1, R.bottom - 1, R.right, R.bottom - 1, RGB(1, 1, 1));// Across bottom
			DrawLine(DC, R.right - 1, R.top + 1, R.right - 1, R.bottom, RGB(1, 1, 1));	// Down right
		}
	  	else 
		{
			DrawLine(DC, R.left + 1, R.bottom - 1, R.right, R.bottom - 1, brColor);		// Across bottom
			DrawLine(DC, R.right - 1, R.top + 1, R.right - 1, R.bottom, brColor);		// Down right
		}
	  	InflateRect(R, -1, -1);
	}
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
void CNCLStatButton::OnMouseMove(UINT nFlags, CPoint point) 
{	
	CButton::OnMouseMove(nFlags, point);
	if (m_parent!=NULL)
	{
		ClientToScreen(&point);
		((CNCLStatusBar*)m_parent)->OnItemMouseMove(nFlags, point);
	}
	::SetCursor(m_current_cursor);
	
	char menudata[100];
	if(m_TimerID > 0)
	{
		int iX = m_StartPoint.x - point.x;
		int iY = m_StartPoint.y - point.y;
		if((iX*iX + iY*iY) > 9)
		{
			m_StartPoint.x = -100;
			m_StartPoint.y = -100;
			KillTimer(m_TimerID);
			m_TimerID = 0;

			COleDataSource*	pSource = new COleDataSource();
			if(pSource)
			{
				CSharedFile	sf(GMEM_MOVEABLE|GMEM_DDESHARE|GMEM_ZEROINIT);
				CString iText;
				int mneunum = ((CNCLStatusBar*)m_parent)->get_menunum2(m_barnum, 1);
				if (m_type==1)
					sprintf_s(menudata, 100, "CNCLStatButton2 %d, %d", mneunum, m_itemnum);
				else
					sprintf_s(menudata, 100, "CNCLStatButton %d, %d", mneunum, m_itemnum);

				iText = menudata;

				//	write name to clipboard
				sf.Write(iText, iText.GetLength());

				HGLOBAL hMem = sf.Detach();
				if (!hMem) 
					return;
				pSource->CacheGlobalData(CF_TEXT, hMem);
				if (m_type==1)
					UW_drag_obj = 1;
				else
					UW_drag_obj = 0;
				//	Do drag and drop!
				pSource->DoDragDrop();

				//	free source
				delete pSource;
				UW_drag_obj = -1;
			}
		}
	}
}

/***********************************************************************
**
**   FUNCTION: DrawHighLight(CRect rect, int in_flag)
**
**       Draw highlight button
**
**   INPUT:  rect: area to draw
**			in_flag: draw in frame
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::DrawHighLight(CRect rect, int in_flag)
{
	if (IsWindowEnabled()==0)
		return;
/*
.....seperator
*/
	if (m_type==3)
		return;
	CClientDC dc(this);

	CRect focusRect, btnRect;
	focusRect.left = focusRect.top = 0;
	focusRect.right = rect.Width();
	focusRect.bottom = rect.Height();
	btnRect.CopyRect(&focusRect); 

	focusRect.left += 4;
    focusRect.right -= 4;
    focusRect.top += 4;
    focusRect.bottom -= 4;

	COLORREF cr;
	CRgn rgn;
	rgn.CreateRectRgn(rect.left, rect.top, rect.right, rect.bottom);
	if ((m_type==1)||(m_type==2))
	{
		cr = RGB(255, 220, 150);
		CBrush brushFill(cr);
		cr = dc.GetTextColor();
		dc.SetTextColor(RGB(0,0,0));
		int nBkMode = dc.SetBkMode(TRANSPARENT);
		dc.SelectObject(&m_txtfont );
		dc.SelectClipRgn(&rgn);
		dc.FillRect(&rect, &brushFill);
//		dc.TextOut(rect.left, rect.top, m_text, m_text.GetLength());
		if (m_type==1)
			dc.TextOut(rect.left+2, rect.top, m_text, m_text.GetLength());
		if (m_type==2)
/*
......center
*/
		{
			CSize szText;
			szText = dc.GetTextExtent(m_text);
			int delx = rect.Width() - szText.cx;
			dc.TextOut(rect.left+delx/2, rect.top, m_text, m_text.GetLength());
		}
		if (m_type==1)
			DrawFrame(&dc, btnRect,0);
		else if (m_type==2)
		{
			if (in_flag==0)
				DrawFrame(&dc, btnRect,1);
			else
				DrawFrame(&dc, btnRect,-1);
		}
	}
/*
.....seperator
*/
	if (m_type==3)
	{
		int left, top, right, bottom;
		left = right = (rect.left + rect.right)*.5;
		top = rect.top;
		bottom = rect.bottom;
		DrawLine(&dc, left, top, right, bottom, RGB(100,100,100));							// Across top
	}
	if (m_type==0)
	{
		cr = RGB(255, 220, 150);
		CBrush brushFill(cr);
		cr = dc.GetTextColor();
		dc.SetTextColor(RGB(0,0,0));
		int nBkMode = dc.SetBkMode(TRANSPARENT);
		dc.SelectObject(&m_txtfont );
		dc.SelectClipRgn(&rgn);
		dc.FillRect(&rect, &brushFill);

		DrawBitmap(&dc);
		if (in_flag==0)
			DrawFrame(&dc, btnRect,1);
		else
			DrawFrame(&dc, btnRect,-1);

	}
 }

/***********************************************************************
**
**   FUNCTION: DrawNormal(CRect rect)
**
**       Draw normal button
**
**   INPUT:  rect: area to draw
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::DrawNormal(CRect rect)
{
	if (IsWindowEnabled()==0)
		return;
	CClientDC dc(this);

	CRect focusRect, btnRect;
	focusRect.left = focusRect.top = 0;
	focusRect.right = rect.Width();
	focusRect.bottom = rect.Height();
	btnRect.CopyRect(&focusRect); 

	focusRect.left += 4;
    focusRect.right -= 4;
    focusRect.top += 4;
    focusRect.bottom -= 4;
      
    const int bufSize = 512;
    TCHAR buffer[bufSize];
    GetWindowText(buffer, bufSize);
	
	DrawFilledRect(&dc, btnRect, dc.GetBkColor()); 
	COLORREF cr;
	CRgn rgn;
	rgn.CreateRectRgn(rect.left, rect.top, rect.right, rect.bottom);
	if ((m_type==1)||(m_type==2))
	{
		cr = dc.GetBkColor();
		CBrush brushFill(cr);
		cr = dc.GetTextColor();
		dc.SetTextColor(RGB(0,0,0));
		int nBkMode = dc.SetBkMode(TRANSPARENT);
		dc.SelectClipRgn(&rgn);
		dc.SelectObject(&m_txtfont );
		dc.FillRect(&rect, &brushFill);
//		dc.TextOut(rect.left, rect.top, m_text, m_text.GetLength());
		if (m_type==1)
			dc.TextOut(rect.left+2, rect.top, m_text, m_text.GetLength());
		if (m_type==2)
/*
......center
*/
		{
			CSize szText;
			szText = dc.GetTextExtent(m_text);
			int delx = rect.Width() - szText.cx;
			dc.TextOut(rect.left+delx/2, rect.top, m_text, m_text.GetLength());
		}
	}
/*
.....seperator
*/
	if (m_type==3)
	{
		int left, top, right, bottom;
		left = right = (rect.left + rect.right)*.5;
		top = rect.top;
		bottom = rect.bottom;
		DrawLine(&dc, left, top, right, bottom, RGB(100,100,100));							// Across top
	}
	if (m_type==0)
	{
		DrawBitmap(&dc);
	}
}
void CNCLStatButton::DrawBitmap(CDC *DC)
{
	CRect rect;
	GetWindowRect(&rect);

	if (m_picture.m_picture==NULL)
	{
		return;
	}
	m_picture.UpdateSizeOnDC(DC);
	m_picture.Show(DC, CPoint(1,1), CPoint(rect.Width()-2, rect.Height()-2), 0,0);
}
/***********************************************************************
**
**   FUNCTION: :DrawFilledRect(CDC *DC, CRect R, COLORREF color)
**
**         Draw the button filledrect
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::DrawFilledRect(CDC *DC, CRect R, COLORREF color)
{ 
	CBrush B;
	B.CreateSolidBrush(color);
	DC->FillRect(R, &B);
}
/***********************************************************************
**
**   FUNCTION: :DrawLine(CDC *DC, CRect EndPoints, COLORREF color)
**
**         Draw the line in the button
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::DrawLine(CDC *DC, CRect EndPoints, COLORREF color)
{ 
	CPen newPen;
	newPen.CreatePen(PS_SOLID, 1, color);
	CPen *oldPen = DC->SelectObject(&newPen);
	DC->MoveTo(EndPoints.left, EndPoints.top);
	DC->LineTo(EndPoints.right, EndPoints.bottom);
	DC->SelectObject(oldPen);
    newPen.DeleteObject();
}

/***********************************************************************
**
**   FUNCTION: :DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color)
**
**         Draw the line in the button
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color)
{ 
	CPen newPen;
	newPen.CreatePen(PS_SOLID, 1, color);
	CPen *oldPen = DC->SelectObject(&newPen);
	DC->MoveTo(left, top);
	DC->LineTo(right, bottom);
	DC->SelectObject(oldPen);
    newPen.DeleteObject();
}

/***********************************************************************
**
**   FUNCTION: Reset_Bitmap(char* filename)
**
**       Reset the picture using new image file
**
**   INPUT:  filename: new image file
**
**   OUTPUT :   None
**   RETURN:    none
**
**********************************************************************/
void CNCLStatButton::Reset_Bitmap(char* filename)
{
	UX_pathname filen;
	strcpy(filen, filename);

	int status = ul_open_mod_file("UU_USER_SETTINGS", "bitmaps", "NCL_BITMAP", (char*)UU_NULL, 
		filen, 0, (FILE**)UU_NULL);

	if (status==0)
		m_filename = filen;
	else
		m_filename = "";
	m_picture.Load(m_filename);
	m_picture.CreatePicBitmap(GetDC());
	DrawBitmap(GetDC());
}
void CNCLStatButton::DrawNormal2()
{
	CRect rect;
	GetWindowRect(&rect);
	ScreenToClient(&rect);
/*
.....check if the mouse is inside the button
*/
	if (m_focus)
	{
		m_focus = 0;
		DrawNormal(rect);
		CloseToolTip();
	}
}

void CNCLStatButton::DrawHighLight2()
{
	CRect rect;
	GetWindowRect(&rect);
	ScreenToClient(&rect);
/*
.....check if the mouse is inside the button
*/
	if (m_focus==0)
	{
		m_focus = 1;
		DrawHighLight(rect, 0);
	}
}
/***********************************************************************
**
**   FUNCTION: OnTimer() 
**
**		this function will timely check if the mouse point is on the 
**		item, if not reset the time
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::OnTimer(UINT_PTR nIDEvent) 
{
	if (nIDEvent == 1)
	{
		POINT pt;
		::GetCursorPos(&pt);
		CRect iRect;
		GetWindowRect(iRect);
		if(!(iRect.PtInRect(pt)))
		{
			KillTimer(nIDEvent);
			m_TimerID = 0;
		}
	}
	CButton::OnTimer(nIDEvent);
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
void CNCLStatButton::OnLButtonUp(UINT nFlags, CPoint point) 
{
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;
	
	if(m_TimerID)
	{
		KillTimer(m_TimerID);
		m_TimerID = 0;
	}
	CloseToolTip();
	CButton::OnLButtonUp(nFlags, point);
	DrawNormal2();
}
/***********************************************************************
c
c   FUNCTION: OnLButtonDown(UINT nFlags, CPoint point)
c			Callback function for left mouse button down
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
void CNCLStatButton::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CButton::OnLButtonDown(nFlags, point);

	CRect rect;
	GetWindowRect(&rect);
	ScreenToClient(&rect);
	DrawHighLight(rect, 1);
//don't know why but this following call will cause the button function not called
//I guess it disable the Lbuttonup function message which should following this Lbuttondown
//so we put this function call inside Lbuttonup just before call CButton::OnLButtonUp which
//will call the ID function.
//	CloseToolTip();
	m_StartPoint = 	point;
	ClientToScreen(&point);
	((CNCLStatusBar*)m_parent)->ScreenToClient(&point);
	((CNCLStatusBar*)m_parent)->CheckPtOnItem(point);
	((CNCLStatusBar*)m_parent)->getbutnum(m_barnum, m_itemnum);
	m_TimerID = SetTimer(1, 100, NULL);	
}

/***********************************************************************
**
**   FUNCTION: UpdateLabel(CString newlabel)
**
**       Update the button label
**
**   INPUT:  newlabel: new label to update
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::UpdateLabel(CString newlabel)
{
	SetText(newlabel);
	RedrawWindow();
}

/***********************************************************************
**
**   FUNCTION: ShowToolTip(CPoint point)
**
**      show the tooltip window at point
**
**   INPUT:  point: location to show tooltip
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::ShowToolTip(CPoint point)
{
	m_ToolTips.SetText(m_tiptext);
/*
.....using the bottom y for position
*/
	CRect rect;
	GetClientRect(&rect);
	ScreenToClient(&point);
	point.y = rect.top - 15;
	point.x -= 15;
	m_ToolTips.Show(point);
}

/***********************************************************************
**
**   FUNCTION: CloseToolTip()
**
**      close the tooltip window
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::CloseToolTip()
{
	m_ToolTips.Close();
}
/***********************************************************************
**
**   FUNCTION: UpdateStatusBar(CPoint pt, char* input_text)
**
**       updated the statusbar while get the dragging object
**
**   INPUT: pt: the position the dragging object display
**			input_text: text include the button info we need add
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::UpdateStatusBar(CPoint pt, char* input_text)
{
	int barnum1, itemnum1, choice, barnum2, itemnum2;
	char menudata[100];

	barnum1 = itemnum1 = -1;
	int menu_but = 0;
	int menu_desgn = 0;
	strcpy_s(menudata, sizeof(menudata), input_text);
	if (strncmp(menudata, "CToolmenu2", 10)==0)
	{
		sscanf_s (menudata, "CToolmenu2 %d, %d", &barnum1, &itemnum1);
		menu_but = 1;
	}
	else if (strncmp(menudata, "CToolmenu", 9)==0)
	{
		sscanf_s (menudata, "CToolmenu %d, %d", &barnum1, &itemnum1);
		menu_but = 1;
	}
	else if (strncmp(menudata, "CNCLMenu", 8)==0)
	{
		sscanf_s (menudata, "CNCLMenu %d, %d", &barnum1, &itemnum1);
		menu_but = 1;
	}
	else if (strncmp(menudata, "CNCLStatusBar2", 14)==0)
	{
		sscanf_s (menudata, "CNCLStatusBar2 %d, %d", &barnum1, &itemnum1);
		menu_but = 0;
	}
	else if (strncmp(menudata, "CNCLStatusBar", 13)==0)
	{
		sscanf_s (menudata, "CNCLStatusBar %d, %d", &barnum1, &itemnum1);
		menu_but = 0;
	}
	else if (strncmp(menudata, "CNCLStatButton2", 15)==0)
	{
		sscanf_s (menudata, "CNCLStatButton2 %d, %d", &barnum1, &itemnum1);
		menu_but = 0;
	}
	else if (strncmp(menudata, "CNCLStatButton", 14)==0)
	{
		sscanf_s (menudata, "CNCLStatButton %d, %d", &barnum1, &itemnum1);
		menu_but = 0;
	}
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
......get the button position of the drop and insert this item into current
......menubar and update the menu bar
*/
/*
.....current status number m_barnum, item num
*/
	if (m_parent==NULL)
		return;
	CPoint point = pt;
	((CNCLStatusBar*)m_parent)->CheckClosestBut(this, point, barnum2, itemnum2);
	if (menu_desgn)
	{
		NCL_MainFrame->m_menu_desgn = menu_desgn;
		int menunum = ((CNCLStatusBar*)m_parent)->get_menunum2(barnum2, 1);
		ud_insert_UDMmenu(menunum, itemnum2, &((NCL_MainFrame->m_menudsgn_dlg)->m_menu_item));
		UW_addmenu = menunum;
		UW_additem = itemnum2;
		UW_remove_menu = -1;
		NCL_MainFrame->PostMessage(WM_COMMAND, UW_UPDATE_STATUSBAR);
		return;
	}
// the accept pane is always pane number, not menu number
//	barnum2 = ((CNCLStatusBar*)m_parent)->get_menunum2(barnum2, 1);
	
	if ((menu_but==0)&&(barnum2==barnum1))
		((CNCLStatusBar*)m_parent)->MovedStatusItem(barnum2, itemnum2, barnum1, itemnum1, 1, 1);
	else if ((menu_but==0)&&(barnum2==barnum1)&&(itemnum2==itemnum1))
		return;
	else
	{
//......open a POPUP menu with "Copy Here" (choice=2), "Move Here", "Cancel" (choice=0)
		uw_ntpopchoice(0, &choice);
		if ((choice==0)||(choice==-1))
			return;
		if (menu_but==0)
			((CNCLStatusBar*)m_parent)->MovedStatusItem(barnum2, itemnum2, barnum1, itemnum1, 1, choice);
		else
		{
			((CNCLStatusBar*)m_parent)->MovedStatusItem(barnum2, itemnum2, barnum1, itemnum1, 0, choice);
		}
	}
}
/***********************************************************************
**
**   FUNCTION: OnRButtonUp(UINT nFlags, CPoint pt)
**
**       callback for right mouse button up
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatButton::OnRButtonUp(UINT nFlags, CPoint pt) 
{
	UDM_menu_struc menu_item;
	int status, choice = 0;
	CButton::OnRButtonUp(nFlags, pt);

	ClientToScreen(&pt);
	((CNCLStatusBar*)m_parent)->ScreenToClient(&pt);
	((CNCLStatusBar*)m_parent)->CheckPtOnItem(pt);
	((CNCLStatusBar*)m_parent)->getbutnum(m_barnum, m_itemnum);

	int menunum = ((CNCLStatusBar*)m_parent)->get_menunum2(m_barnum, 1);
/*
......display popup menu " Insert Separator, Insert Item, Delete Item, Cancel"
*/
	uw_ntpopchoice(3, &choice);
	if (choice==0)
		return;
	status = 0;
	if (choice==1)
	{
/*
......delete item
*/
/*
......updated UDM_menu[menunum] with delete one item in menu = menunum1, item = m_itemnum
*/
			ud_del_UDMmenu(menunum, m_itemnum);
			UW_addmenu = -1;
			UW_remove_menu = menunum;
			UW_remove_item = m_itemnum;
	}
	else if (choice==2)
/*
.....edit select item
*/
	{
		status = uw_ntmenu_desgn(1, &(UDM_menu[menunum].menus[m_itemnum]));
		if (status!=-1)
		{
			UW_addmenu = menunum;
			UW_additem = m_itemnum;
			UW_remove_menu = menunum;
			UW_remove_item = m_itemnum;
		}
	}
	else if (choice==3)
/*
.....insert new item
*/
	{
		menu_item.toggle_num = 0;
		menu_item.toggle = 0;
		menu_item.chcfile[0] = '\0';
		menu_item.chcdef = 0;
		menu_item.name[0] = '\0';
		menu_item.file[0] = '\0';
		menu_item.statname[0] = '\0';
		menu_item.params = (char *)UU_NULL;
		menu_item.descrip[0] = '\0';
		menu_item.bgcolor[0] = '\0';
		menu_item.color[0] = -1;
		menu_item.color[1] = -1;
		menu_item.color[2] = -1;
		menu_item.pos[0] = -1;
		menu_item.pos[1] = -1;
		menu_item.size[0] = -1;
		menu_item.size[0] = -1;
		menu_item.bmpfile[0] = '\0';
		menu_item.bmpnum = -1;
		menu_item.separator = 0;
		status = uw_ntmenu_desgn(1, &menu_item);
		if (status!=-1)
		{
			ud_insert_UDMmenu(menunum, m_itemnum, &menu_item);
			UW_addmenu = menunum;
			UW_additem = m_itemnum;
			UW_remove_menu = -1;
		}
	}
	else if (choice==4)
	{
/*
.....insert separator
*/
		menu_item.toggle_num = 0;
		menu_item.toggle = 0;
		menu_item.chcfile[0] = '\0';
		menu_item.chcdef = 0;
		menu_item.name[0] = '\0';
		menu_item.file[0] = '\0';
		menu_item.statname[0] = '\0';
		menu_item.params = (char*)UU_NULL;
		menu_item.descrip[0] = '\0';
		menu_item.bgcolor[0] = '\0';
		menu_item.color[0] = -1;
		menu_item.color[1] = -1;
		menu_item.color[2] = -1;
		menu_item.pos[0] = -1;
		menu_item.pos[1] = -1;
		menu_item.size[0] = -1;
		menu_item.size[0] = -1;
		menu_item.bmpfile[0] = '\0';
		menu_item.bmpnum = -1;
		menu_item.separator = 1;
		ud_insert_UDMmenu(menunum, m_itemnum, &menu_item);
		UW_addmenu = menunum;
		UW_additem = m_itemnum;
		UW_remove_menu = -1;
	}
/*
.....send message to Frame level to updated the menu display
.....we use POstMessage because we have to destroy the menu then redisplay
.....but don't want to mess up the message loop
*/
	if (status!=-1)
		NCL_MainFrame->PostMessage(WM_COMMAND, UW_UPDATE_STATUSBAR);
}
#endif






