#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**		FILE NAME: wsntsec\btn.cpp
**
**	 Description - Functions and implementations for
**		CNCLSecButton class
**
**	 CONTAINS:
**		member function of CNCLSecButton
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntsecbtn.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			01/20/17 , 12:25:09
**********************************************************************
*/
#include "wsntstdafx.h"
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxdisp.h >       // MFC OLE automation classes
#include <afxpriv.h>
#include "wsntsecbtn.h"
#include "wsntres.h"
#include "xenv1.h"
#include "wsntform.h"
#include "wsntfrmview.h"
#include "dmotif.h"
#include "wsntglfunc.h"
#include "wsntcfunc.h"
#include "zkeysym.h"
#include "wsntframe.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif
#define NBUTTONNUM	5000
#define NCHOICEBUTTON 1000

extern CMainFrame *NCL_MainFrame;
extern int UW_addmenu, UW_additem, UW_remove_menu, UW_remove_item;
extern "C" float uw_form_scaley;
extern char UW_formdlg_font[];
extern "C" int UW_form_fontsize;

extern "C" int ul_open_mod_file(char*, char*, char*, char*, char*, int, FILE**);
extern "C" char * uu_malloc(int);
extern "C" void uu_free(char*);

IMPLEMENT_DYNAMIC(CNCLSecButton, CButton)

BEGIN_MESSAGE_MAP(CNCLSecButton, CButton)
	//{{AFX_MSG_MAP(CNCLSecButton)
	ON_WM_CREATE()
	ON_WM_MOUSEMOVE()
	ON_WM_RBUTTONUP()
	ON_WM_LBUTTONUP()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  CNCLSecButton
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLSecButton::CNCLSecButton() :	m_TargetDrop(NULL)
{  
#if (_MFC_VER < 0x0250)
  hwndOwner = NULL; 
#endif 
	m_current_cursor = LoadCursor(NULL, IDC_ARROW);
	m_focus = 0;
	m_disabled = 0;
	m_parent = NULL;
	m_type = 0;
	m_fid = -1;
	m_picture.m_trans = 1;
	m_TimerID = 0;
	m_TargetDrop = new CNCLMnDropTarget2();
	m_boxflag = 0;
	m_tbold = 0;
	m_itemnum = -1;
	m_page = -1;
} 
/***********************************************************************
c
c   SUBROUTINE:  ~CNCLSecButton
c
c   FUNCTION:  Destructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLSecButton::~CNCLSecButton()
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
void CNCLSecButton::SetBitmapFile(char *filename)
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

void CNCLSecButton::SetText(CString text)
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
int CNCLSecButton::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CButton::OnCreate(lpCreateStruct) == -1)
		return -1;
	m_picture.CreatePicBitmap(GetDC());

	int stat;
	if (m_txtfont.m_hObject)
		VERIFY (m_txtfont.DeleteObject ());	

	LOGFONT lf;
	memset(&lf, 0, sizeof(LOGFONT));
	lf.lfWeight = 400;
//	strncpy_s(lf.lfFaceName, LF_FACESIZE, _T("MS Sans Serif"), 20);
	strcpy(lf.lfFaceName, UW_formdlg_font);
	lf.lfHeight = UW_form_fontsize*10;
	m_txtfont.CreatePointFontIndirect(&lf, GetDC());

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
void CNCLSecButton::SetButID(UINT fid)
{
/*
......not used
.........
	m_fid = fid;

	char msg[80];	
	if ((fid >= IDC_FORMSECTION1)&&(fid<IDC_FORMSECTION20)) 
	{
		sprintf_s(msg, "section %d", fid-IDC_FORMSECTION1+1);
		m_tiptext = msg;
	}
	else
		m_tiptext = "";
*/
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
void CNCLSecButton::DrawItem(LPDRAWITEMSTRUCT lpDIS)
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
	if (m_type==3)
	{
/*
......seperator
*/
		int left, top, right, bottom;
		top = bottom = (rc.top + rc.bottom)*.5-1;
		left = rc.left+2;
		right = rc.right-2;
		DrawLine(pDC, left, top, right, bottom, RGB(150,150,150));
		return;
	}
    const int bufSize = 512;
    TCHAR buffer[bufSize];
    GetWindowText(buffer, bufSize);
	
	if (m_select!=1)
	    DrawFilledRect(pDC, btnRect, pDC->GetBkColor()); 
	else
	{
		DrawFilledRect(pDC, btnRect, RGB(255, 220, 150)); 
	    DrawFrame(pDC, btnRect,2);
	}
	if (m_boxflag==1)
	{
	    DrawFrame(pDC, btnRect,-1000);
	}	
  	DrawButtonText(pDC, btnRect, buffer, m_tcolor);
/*
......Now, depending upon the state, redraw the button (down image) if it is selected,
......place a focus rectangle on it, or redisplay the caption if it is disabled
*/
	if (state & ODS_FOCUS) 
	{
		DrawFocusRect(lpDIS->hDC, (LPRECT)&focusRect);
		m_focus = 1; 
		if (state & ODS_SELECTED)
		{ 
    		DrawFilledRect(pDC, btnRect, pDC->GetBkColor()); 
  			DrawButtonText(pDC, btnRect, buffer, m_tcolor);
			DrawFocusRect(lpDIS->hDC, (LPRECT)&focusRect);
		}
		m_disabled = 0;
	}
	else if (state & ODS_DISABLED)
	{
  		DrawButtonText(pDC, btnRect, buffer, RGB(125,125,125));
		m_disabled = 1;
		m_focus = 0; 
    }
	else
	{
		m_disabled = 0;
		m_focus = 0; 
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
void CNCLSecButton::DrawFrame(CDC *DC, CRect R, int Inset)
{ 
	COLORREF dark, light, tlColor, brColor;

	
	dark = PALETTERGB(200, 120, 0);
	light = PALETTERGB(200, 120, 0);
	tlColor = light;
	brColor = dark;	
	if (Inset==-1000)
/*
.....for draw drop-hilight
*/
	{
		tlColor =  PALETTERGB(0, 120, 255);
	}
	DrawLine(DC, R.left+2, R.top, R.right-2, R.top, tlColor);
	DrawLine(DC, R.left, R.top+2, R.left, R.bottom-3, tlColor);				
	DrawLine(DC, R.left+2, R.bottom-1, R.right-2, R.bottom-1, tlColor);
	DrawLine(DC, R.right-1, R.top + 2, R.right-1, R.bottom-2, tlColor);
	DrawLine(DC, R.left, R.top+2, R.left+2, R.top, tlColor);
	DrawLine(DC, R.left, R.bottom-3, R.left+2, R.bottom-1, tlColor);
	DrawLine(DC, R.right-3, R.bottom-1, R.right-1, R.bottom-3, tlColor);
	DrawLine(DC, R.right-1, R.top + 2, R.right-3, R.top, tlColor);
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
void CNCLSecButton::OnMouseMove(UINT nFlags, CPoint point) 
{	
	CButton::OnMouseMove(nFlags, point);

	if (m_parent!=NULL)
	{
		ClientToScreen(&point);
		if (m_parent->IsKindOf(RUNTIME_CLASS(CNCLFormScrollView)))
			((CNCLFormScrollView*)m_parent)->OnItemMouseMove(m_itemnum);
		else
			((CNCLFormView*)m_parent)->OnItemMouseMove(m_itemnum);
	}
	::SetCursor(m_current_cursor);	
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
void CNCLSecButton::DrawHighLight(CRect rect, int in_flag)
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

    const int bufSize = 512;
    TCHAR buffer[bufSize];
    GetWindowText(buffer, bufSize);
	
    DrawFilledRect(&dc, btnRect, RGB(255, 220, 150)); 
    DrawFrame(&dc, btnRect,1);
  	DrawButtonText(&dc, btnRect, buffer, m_tcolor);
	if (m_boxflag==1)
	{
	    DrawFrame(&dc, btnRect,-1000);
	}	
/*	if (m_focus) 
	{
		DrawFocusRect(dc.m_hDC, (LPRECT)&focusRect);
	}
	else if (m_disabled) 
	{
  		DrawButtonText(&dc, btnRect, buffer, RGB(125,125,125));
    }
*/
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
void CNCLSecButton::DrawNormal(CRect rect)
{
	if (IsWindowEnabled()==0)
		return;
	if (m_type==3)
		return;
	CClientDC dc(this);
	CDC* dc1 = m_parent->GetDC();
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
	if (m_select!=1)
	    DrawFilledRect(&dc, btnRect, GetSysColor(COLOR_3DFACE)); 
	else
	{
		DrawFilledRect(&dc, btnRect, RGB(255, 220, 150)); 
	    DrawFrame(&dc, btnRect,2);
	}
	if (m_boxflag==1)
	{
	    DrawFrame(&dc, btnRect,-1000);
	}	
  	DrawButtonText(&dc, btnRect, buffer, m_tcolor);
	if (m_focus) 
	{
		DrawFocusRect(dc.m_hDC, (LPRECT)&focusRect);
	}
	else if (m_disabled) 
	{
  		DrawButtonText(&dc, btnRect, buffer, RGB(125,125,125));
    }
}
/***********************************************************************
**
**   FUNCTION: DrawBorders(CDC* pDC, CRect& rect)
**
**       Drawing border (we don't want draw border for our menubar, so simply return)
**
**   INPUT:   pDC: toolbar device context 
**				rect: toolbar rectangle
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLSecButton::DrawBorders(CDC* pDC, CRect& rect)
{
	COLORREF clr=RGB(255,0,0);
	pDC->FillSolidRect(rect.right-1,0,rect.right,rect.bottom,clr); 
	pDC->FillSolidRect(0,rect.bottom-1,rect.right,rect.bottom,clr); 

	clr=GetSysColor(COLOR_3DHIGHLIGHT);
	pDC->FillSolidRect(0,0,rect.right,1,clr);
	pDC->FillSolidRect(0,0,1,rect.bottom,clr);
}

void CNCLSecButton::DrawBitmap(CDC *DC)
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
void CNCLSecButton::DrawFilledRect(CDC *DC, CRect R, COLORREF color)
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
void CNCLSecButton::DrawLine(CDC *DC, CRect EndPoints, COLORREF color)
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
void CNCLSecButton::DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color)
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
void CNCLSecButton::Reset_Bitmap(char* filename)
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
void CNCLSecButton::DrawNormal2()
{
	CRect rect;
	GetWindowRect(&rect);
	ScreenToClient(&rect);
/*
.....check if the mouse is inside the button
*/
//	if ((m_focus)||(m_select))
	{
		m_focus = 0;
		DrawNormal(rect);
		CloseToolTip();
	}
}

void CNCLSecButton::DrawHighLight2()
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
void CNCLSecButton::OnTimer(UINT_PTR nIDEvent) 
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
void CNCLSecButton::OnLButtonUp(UINT nFlags, CPoint point) 
{	
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
void CNCLSecButton::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CButton::OnLButtonDown(nFlags, point);
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
void CNCLSecButton::OnRButtonUp(UINT nFlags, CPoint point) 
{
	if (m_parent==NULL)
		return;
	if (m_parent->IsKindOf(RUNTIME_CLASS(CNCLFormView))==0)
		return;
	if (m_itemnum==-1)
		return;
	if (m_page==-1)
		((CNCLFormView*)m_parent)->OnSectionSelItem(m_itemnum);

	CMenu pmenu;
	pmenu.CreatePopupMenu();
		
	pmenu.AppendMenu (MF_ENABLED, ID_POPUP_INSERT_ITEM, _T("Insert Section"));
	if (m_itemnum==50)
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_DELETE, _T("Delete All Items"));
	else
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_DELETE, _T("Delete Section"));
	if (m_itemnum!=50)
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_EDIT_ITEM, _T("Properties"));
	pmenu.AppendMenu (MF_ENABLED, ID_POPUP_CANCEL, _T("Cancel"));

	POINT pt;
	GetCursorPos(&pt);
	UINT cmdid = pmenu.TrackPopupMenu(TPM_LEFTBUTTON|TPM_LEFTALIGN|TPM_RETURNCMD|TPM_RECURSE,
					pt.x, pt.y, this, NULL);
	int type, flag;
	if (cmdid==ID_POPUP_DELETE)
	{
		((CNCLFormView*)m_parent)->OnDeleteSec(m_itemnum);
	}
	if (cmdid==ID_POPUP_INSERT_ITEM)
	{
		((CNCLFormView*)m_parent)->OnInsertSec(m_itemnum);
	}
	if (cmdid==ID_POPUP_EDIT_ITEM)
	{
		((CNCLFormView*)m_parent)->OnChangeSecProperty(m_itemnum);
	}
done:;	
	CButton::OnRButtonUp(nFlags, point);
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
void CNCLSecButton::UpdateLabel(CString newlabel)
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
void CNCLSecButton::ShowToolTip(CPoint point)
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
void CNCLSecButton::CloseToolTip()
{
	m_ToolTips.Close();
}

/***********************************************************************
**
**   FUNCTION: :DrawButtonText(CDC *DC, CRect R, const char *Buf, COLORREF TextColor)
**
**         Draw the button text in the button
**
**   INPUT:  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLSecButton::DrawButtonText(CDC *DC, CRect R, const char *Buf, COLORREF TextColor)
{
    COLORREF prevColor = DC->SetTextColor(TextColor);
 	CFont* pOldFont = NULL;
	pOldFont = (CFont *)(DC->SelectObject(m_txtfont ));
 
	DC->SetBkMode(TRANSPARENT);
	DC->DrawText(Buf, strlen(Buf), R, DT_CENTER|DT_VCENTER|DT_SINGLELINE);
	DC->SetTextColor(prevColor);
}
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
void CNCLSecButton::InitDrag()
{
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
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
void CNCLSecButton::OnDestroy() 
{
	if(m_TargetDrop)
	{
		m_TargetDrop->Revoke();
		delete m_TargetDrop;
	}
	m_TargetDrop = NULL;
	CButton::OnDestroy();	
}

/***********************************************************************
**
**   FUNCTION: OnDragDropCallback(CPoint pt, char *input_text, int auto_flag)
**
**         Called by the the mouse drop point is on the window
**
**   INPUT:  pt: current cursor point (drop point)
**			input_text: the text string data contains draging window information
**			flag = 1: macro form auto item
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLSecButton::OnDragDropCallback(CPoint pt, char *input_text)
{
	if (m_page==-1)
		return;
	((CNCLFormView*)m_parent)->OnDragDropSection(m_page, input_text);
}
/*
.....show rectangle arround button to show drag enter
*/
void CNCLSecButton::ShowDropBox(int flag)
{
	m_boxflag = flag;
	RedrawWindow();
}

void CNCLSecButton::SetBoldFont(int bold)
{
	if (m_tbold==bold)
		return;
	if (m_txtfont.m_hObject)
		VERIFY (m_txtfont.DeleteObject ());

	LOGFONT lf;
	memset(&lf, 0, sizeof(LOGFONT));
	if (bold==0)
		lf.lfWeight = 400;
	else
		lf.lfWeight = 800;
	strcpy(lf.lfFaceName, UW_formdlg_font);
//	strncpy_s(lf.lfFaceName, LF_FACESIZE, _T("MS Sans Serif"), 20);
	lf.lfHeight = UW_form_fontsize*10;
	m_txtfont.CreatePointFontIndirect(&lf, GetDC());
	SetFont(&m_txtfont);
	m_tbold = bold;
}

#endif






