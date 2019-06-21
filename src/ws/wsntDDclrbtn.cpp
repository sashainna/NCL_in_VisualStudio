#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**		FILE NAME: wsntDDclrbtn.cpp
**
**	 Description - Functions and implementations for
**		CNCLDDColorButton class (NCL color button)
**		with Drag &Drop
**	 CONTAINS:
**		member function of CNCLDDColorButton
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDclrbtn.cpp , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**			06/17/15 , 14:14:35
**********************************************************************
*/

#include "wsntstdafx.h"
#include <afxole.h>         // MFC OLE classes
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxdisp.h >       // MFC OLE automation classes
#include <afxpriv.h>
#include "wsntdropsource.h"
#include "wsntddclrbtn.h"
#include "wsntDDform.h"
 
#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif
extern char UW_formdlg_font[];
extern HBITMAP WINAPI  CopyWindowToBitmap(LPRECT lpRect, CWnd *pWnd);
extern void CopyPropertyPage(CNCLFormProp *prop_dlg_from, CNCLFormProp *prop_dlg_to);

IMPLEMENT_DYNAMIC(CNCLDDColorButton, CButton)

/***********************************************************************
c
c   SUBROUTINE:  CNCLDDColorButton
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLDDColorButton::CNCLDDColorButton(int type) 
	:	m_TargetDrop(NULL),
		m_TimerID(0)
{  
#if (_MFC_VER < 0x0250)
  hwndOwner = NULL; 
#endif 
	m_StartPoint = CPoint(-1, -1);
	m_TargetDrop = new CNCLMnDropTarget2();
	m_DragImage = new CImageList();
	m_dragimg_create = 0;
	m_type = type;
	m_itemno = -1;
	m_parent = NULL;
	m_prop_dlg = NULL;
	m_DragAllImage = NULL;
}

/***********************************************************************
c
c   SUBROUTINE:  ~CNCLDDColorButton
c
c   FUNCTION:  Destructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLDDColorButton::~CNCLDDColorButton()
{
	if (m_prop_dlg!=NULL)
		delete m_prop_dlg;
	if (m_DragImage!=NULL)
		delete m_DragImage;
	if (m_DragAllImage!=NULL)
		delete m_DragAllImage;
} 

BEGIN_MESSAGE_MAP(CNCLDDColorButton, CButton)
	//{{AFX_MSG_MAP(CNCLDDColorButton)
	ON_WM_LBUTTONUP()
	ON_WM_LBUTTONDOWN()
	ON_WM_RBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_WM_DESTROY()
	ON_WM_TIMER()
	ON_WM_SIZE()
	ON_WM_MOVE()
	ON_WM_CREATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

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
int CNCLDDColorButton::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CButton::OnCreate(lpCreateStruct) == -1)
		return -1;
	m_fieldFont.CreatePointFont(80, UW_formdlg_font);
	SetFont(&m_fieldFont);
	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(4,m_type,this);
	}
/*
......set size
*/	
	RECT rect;
	GetWindowRect(&rect);	
	m_parent->ScreenToClient(&rect);
	GetWindowText(m_prop_dlg->m_label);
	m_prop_dlg->m_size[0] = rect.right - rect.left;
	m_prop_dlg->m_size[1] = rect.bottom - rect.top;
/*
......set pos
*/	
	m_prop_dlg->m_pos[0] = rect.left;
	m_prop_dlg->m_pos[1] = rect.top;
	m_prop_dlg->m_input_itemno = m_itemno;
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
void CNCLDDColorButton::OnDestroy() 
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
void CNCLDDColorButton::InitDrag()
{
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
}

/***********************************************************************
c
c   FUNCTION: OnLButtonDown(UINT nFlags, CPoint point) 
c
c			Callback function for left mouse button down
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
void CNCLDDColorButton::OnLButtonDown(UINT nFlags, CPoint point) 
{
	m_StartPoint = 	point;
	m_TimerID = SetTimer(1, 100, NULL);	
	CButton::OnLButtonDown(nFlags, point);

	RECT rectClient;
	GetClientRect(&rectClient);
	int cx, cy;
	cx = rectClient.right - rectClient.left;
	cy =  rectClient.bottom -  rectClient.top;
/*
......draw the select box and select this item
*/
	int type = 4;
	int flag;
	if (nFlags&MK_CONTROL)
		flag = 1;
	else if (nFlags&MK_SHIFT)
		flag = 2;
	else
		flag = 0;
	((CNCLDDform*)m_parent)->OnSelectItem(m_itemno, type, m_type, flag);
	ClientToScreen(&point);
	((CNCLDDform*)m_parent)->ScreenToClient(&point);
	((CNCLDDform*)m_parent)->HandleButtonDown(nFlags, point);
}

/***********************************************************************
c
c   FUNCTION: OnLButtonUp(UINT nFlags, CPoint point)
c			Callback function for left mouse button up
c
c   INPUT:  nFlags: Indicates whether various virtual keys are down.
c					MK_CONTROL   Set if the CTRL key is down.
c					MK_SHIFT   Set if the SHIFT key is down.
c			point:  Specifies the x- and y-coordinate of the cursor.
c
c   OUTPUT :   None.
c   RETURN:    None.
c
**********************************************************************/
void CNCLDDColorButton::OnLButtonUp(UINT nFlags, CPoint point) 
{
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;
	
	if(m_TimerID)
	{
		KillTimer(m_TimerID);
		m_TimerID = 0;
	}
	CButton::OnLButtonUp(nFlags, point);
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
void CNCLDDColorButton::OnRButtonUp(UINT nFlags, CPoint point) 
{
	if (m_parent==NULL)
		return;
	CMenu pmenu;
	pmenu.CreatePopupMenu();
		
	pmenu.AppendMenu (MF_ENABLED, ID_POPUP_DELETE, _T("Delete"));
	pmenu.AppendMenu (MF_ENABLED, ID_POPUP_CANCEL, _T("Cancel"));

	POINT pt;
	GetCursorPos(&pt);
	UINT cmdid = pmenu.TrackPopupMenu(TPM_LEFTBUTTON|TPM_LEFTALIGN|TPM_RETURNCMD|TPM_RECURSE,
					pt.x, pt.y, this, NULL);
	int type, flag;
	if (cmdid==ID_POPUP_EDIT_ITEM)
	{
		flag = 0;
		type = 4; 
		((CNCLDDform*)m_parent)->OnSelectItem(m_itemno, type, m_type, flag);
	}
	if (cmdid==ID_POPUP_EDIT_ITEM)
/*
......display property form
*/
	{
		if (m_prop_dlg==NULL)
		{
			m_prop_dlg = new CNCLFormProp(4,m_type,this);
		}
/*
......set label
*/
		GetWindowText(m_prop_dlg->m_label);
/*
......set size
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
		int itype = m_type;
		type = 4;
		((CNCLDDform*)m_parent)->SetActionItem(3,m_itemno, type, itype);
		((CNCLDDform*)m_parent)->PostMessage(WM_COMMAND, ID_DELETE_FRMITEM);
	}
done:;	
	CButton::OnRButtonUp(nFlags, point);
}
void CNCLDDColorButton::SetProperty(CNCLFormProp *prop_dlg)
{
	RECT rectClient;
	GetWindowRect(&rectClient);	
	m_parent->ScreenToClient(&rectClient);
/*
.....copy all prop_dlg date into the member value m_prop_dlg
*/
	CopyPropertyPage(prop_dlg, m_prop_dlg);
/*
.....always empty string for color button
*/
	rectClient.left = m_prop_dlg->m_pos[0];
	rectClient.top = m_prop_dlg->m_pos[1];
	rectClient.right = rectClient.left + m_prop_dlg->m_size[0];
	rectClient.bottom = rectClient.top + m_prop_dlg->m_size[1];
	MoveWindow(&rectClient);
	if (m_prop_dlg->m_input_itemno!=m_itemno)
		((CNCLDDform*)m_parent)->ReplaceItem(m_itemno, m_prop_dlg->m_input_itemno);
	m_itemno = m_prop_dlg->m_input_itemno;
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
void CNCLDDColorButton::OnTimer(UINT_PTR nIDEvent) 
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
void CNCLDDColorButton::OnSize(UINT nType, int cx, int cy) 
{
	CButton::OnSize(nType, cx, cy);

	RECT rectClient;
	GetWindowRect(&rectClient);	
	m_parent->ScreenToClient(&rectClient);
	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(4,m_type,this);
	}
/*
......set size
*/	
	m_prop_dlg->m_size[0] = rectClient.right - rectClient.left;
	m_prop_dlg->m_size[1] = rectClient.bottom - rectClient.top;
/*
......set pos
*/	
	m_prop_dlg->m_pos[0] = rectClient.left;
	m_prop_dlg->m_pos[1] = rectClient.top;
/*
.....updated the property page only if it is the select item
*/
	int type, sel;
	type = 4;
	sel = ((CNCLDDform*)m_parent)->IsSelItem(m_itemno, type, m_type);
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
void CNCLDDColorButton::OnMove(int x, int y)
{
	CButton::OnMove(x, y);
	CRect rectClient;
	GetWindowRect(&rectClient);	
	m_parent->ScreenToClient(&rectClient);
	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(4,m_type,this);
	}
/*
......set pos
*/	
	m_prop_dlg->m_pos[0] = rectClient.left;
	m_prop_dlg->m_pos[1] = rectClient.top;
/*
.....updated the property page only if it is the select item
*/
	int type, sel;
	type = 4;
	sel = ((CNCLDDform*)m_parent)->IsSelItem(m_itemno, type, m_type);
	if (sel)
		((CNCLDDform*)m_parent)->UpdatePropertyPos(rectClient.left, rectClient.top);
}

/***********************************************************************
c
c   FUNCTION: OnMouseMove(UINT nFlags, CPoint point) 
c
c       Tthis member function will be called
c			when the mouse cursor moves on this control
c			call the drag and drop here
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
void CNCLDDColorButton::OnMouseMove(UINT nFlags, CPoint point) 
{
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
				HBITMAP bitmap2 = ((CNCLDDform*)m_parent)->CreateAllToBitmap(&cx, &cy);
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
				GetWindowRect(&rectClient);
				((CNCLDDform*)m_parent)->ScreenToClient(&rectClient);
				CPoint point2 = point;
				point2.x += rectClient.left;
				point2.y += rectClient.top;
				pdropsource->SetDragPt(point2);
				CSharedFile	sf(GMEM_MOVEABLE|GMEM_DDESHARE|GMEM_ZEROINIT);
				CString iText;
				char tempstr[80];
				CString itext2;

				GetWindowText(iText);
				ClientToScreen(&point);
				sprintf(tempstr, "%d %d %d ", m_itemno, point.x, point.y);
				itext2 = tempstr;
				iText = "Move CNCLDDColorButton " + itext2 + iText;
				pdropsource->SetAttWindow(this, point, rectClient);

				sf.Write(iText, iText.GetLength());

				HGLOBAL hMem = sf.Detach();
				if (!hMem) 
					return;
				pSource->CacheGlobalData(CF_TEXT, hMem);
				pSource->DoDragDrop(DROPEFFECT_MOVE|DROPEFFECT_COPY, NULL, pdropsource);
				delete pdropsource;
				delete pSource;
			}
		}
	}
	CButton::OnMouseMove(nFlags, point);
	ClientToScreen(&point);
	((CNCLDDform*)m_parent)->ScreenToClient(&point);
	((CNCLDDform*)m_parent)->HandleMouseMove(nFlags, point);
}
/***********************************************************************
c
c   FUNCTION: SetActive(int active)
c
c       This function set the control as active (1: show as enabled, 0: show as disabled)
c
c   INPUT:  active: 1: show as enabled, 0: show as disabled
c						refer to /ACTIVE/ NO/DEFAULT/YES
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDDColorButton::SetActive(int active)
{
	m_prop_dlg->m_active = active;
	CRect rect;
	GetWindowRect(&rect);	
	((CNCLDDform*)m_parent)->updateWindow_draw(2,&rect) ;
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
void CNCLDDColorButton::DrawGuides(CPoint org_pt, POINT ptCursor)
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
c
c   SUBROUTINE:  set_color
c
c   FUNCTION:  Set button's color
c
c   INPUT:  bcolor: background color
c			fcolor: foreground color
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLDDColorButton::set_color(const COLORREF bcolor, const COLORREF fcolor)
{
	m_fg = fcolor;
	m_bg = bcolor; 
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
void CNCLDDColorButton::DrawItem(LPDRAWITEMSTRUCT lpDIS)
{
	CDC* pDC = CDC::FromHandle(lpDIS->hDC);

	UINT state = lpDIS->itemState; 
	CRect focusRect, btnRect;
	focusRect.CopyRect(&lpDIS->rcItem); 
	btnRect.CopyRect(&lpDIS->rcItem); 

	focusRect.left += 4;
    focusRect.right -= 4;
    focusRect.top += 4;
    focusRect.bottom -= 4;
      
    const int bufSize = 512;
    TCHAR buffer[bufSize];
    GetWindowText(buffer, bufSize);
	
    DrawFilledRect(pDC, btnRect, GetBGColor()); 
    DrawFrame(pDC, btnRect,2);
  	DrawButtonText(pDC, btnRect, buffer, GetFGColor());
/*
......Now, depending upon the state, redraw the button (down image) if it is selected,
......place a focus rectangle on it, or redisplay the caption if it is disabled
*/
	if ((state & ODS_FOCUS) && (m_prop_dlg->m_active!=-1))
	{
		DrawFocusRect(lpDIS->hDC, (LPRECT)&focusRect);
		if (state & ODS_SELECTED){ 
    		DrawFilledRect(pDC, btnRect, GetBGColor()); 
    		DrawFrame(pDC, btnRect, -1);
  			DrawButtonText(pDC, btnRect, buffer, GetFGColor());
			DrawFocusRect(lpDIS->hDC, (LPRECT)&focusRect);
		}
	}
	else if ((state & ODS_DISABLED) || (m_prop_dlg->m_active==-1))
	{
  		DrawButtonText(pDC, btnRect, buffer, RGB(125,125,125));
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
void CNCLDDColorButton::DrawFrame(CDC *DC, CRect R, int Inset)
{ 
	COLORREF dark, light, tlColor, brColor;
	int i, m, width;
	width = (Inset < 0)? -Inset : Inset;
	
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
			tlColor = dark;
			brColor = light;
		}
		else {
			tlColor = light;
			brColor = dark;
		}
		
		DrawLine(DC, R.left, R.top, R.right, R.top, tlColor);							// Across top
		DrawLine(DC, R.left, R.top, R.left, R.bottom, tlColor);							// Down left
	  
		if ( (Inset < 0) && (i == width - 1) && (width > 1) ) {
			DrawLine(DC, R.left + 1, R.bottom - 1, R.right, R.bottom - 1, RGB(1, 1, 1));// Across bottom
			DrawLine(DC, R.right - 1, R.top + 1, R.right - 1, R.bottom, RGB(1, 1, 1));	// Down right
		}
	  	else {
			DrawLine(DC, R.left + 1, R.bottom - 1, R.right, R.bottom - 1, brColor);		// Across bottom
			DrawLine(DC, R.right - 1, R.top + 1, R.right - 1, R.bottom, brColor);		// Down right
		}
	  	InflateRect(R, -1, -1);
	}
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
void CNCLDDColorButton::DrawFilledRect(CDC *DC, CRect R, COLORREF color)
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
void CNCLDDColorButton::DrawLine(CDC *DC, CRect EndPoints, COLORREF color)
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
void CNCLDDColorButton::DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color)
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
void CNCLDDColorButton::DrawButtonText(CDC *DC, CRect R, const char *Buf, COLORREF TextColor)
{
    COLORREF prevColor = DC->SetTextColor(TextColor);
    DC->SetBkMode(TRANSPARENT);
	DC->DrawText(Buf, strlen(Buf), R, DT_CENTER|DT_VCENTER|DT_SINGLELINE);
	DC->SetTextColor(prevColor);
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
void CNCLDDColorButton::OnDragDropCallback(CPoint pt, char *input_text)
{
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
void CNCLDDColorButton::SetItemNo(int itemno)
{
	m_itemno = itemno;
	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(4,m_type,this);
	}
	m_prop_dlg->m_input_itemno = itemno;
}
/***********************************************************************
**
**   FUNCTION: :set_prop_values(double font_scale, int active, char *color, char *pcolor)
**
**         Set some of this control's property values
**
**   INPUT: font_scale, 
**			active, 
**			color, pcolor
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDDColorButton::set_prop_values(double font_scale, int active, char *color, char *pcolor, char *limit, int input)
{
	m_prop_dlg->m_font = font_scale;
	m_prop_dlg->m_active = active;
	m_prop_dlg->m_color = color;
	m_prop_dlg->m_pcolor = pcolor;
	if (limit!=NULL)
		m_prop_dlg->m_limit = limit;
	else
		m_prop_dlg->m_limit = "";
	m_prop_dlg->m_input = input;
}

void CNCLDDColorButton::set_prop_HSPvalues(int n_picarea, UD_PICAREA *picarea)
{
	int k;
	if ((m_prop_dlg->m_picarea_no>0)&&(m_prop_dlg->m_picarea!=NULL))
	{
		for (k=0; k<m_prop_dlg->m_picarea_no; k++)
		{
			if (m_prop_dlg->m_picarea[k].params!=NULL)
				uu_free(m_prop_dlg->m_picarea[k].params);
			if (m_prop_dlg->m_picarea[k].tooltext!=NULL)
				uu_free(m_prop_dlg->m_picarea[k].tooltext);
		}
		uu_free((char*)m_prop_dlg->m_picarea);
		m_prop_dlg->m_picarea = NULL;
	}
	m_prop_dlg->m_picarea_no = n_picarea;
	if ((m_prop_dlg->m_picarea_no>0)&&(picarea!=NULL))
	{
		m_prop_dlg->m_picarea = (UD_PICAREA *) uu_malloc(m_prop_dlg->m_picarea_no*sizeof(UD_PICAREA));
		for (k=0; k<m_prop_dlg->m_picarea_no; k++)
		{
			if (picarea[k].params!=NULL)
			{
				m_prop_dlg->m_picarea[k].params = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_prop_dlg->m_picarea[k].params, picarea[k].params);
			}
			else
				m_prop_dlg->m_picarea[k].params = NULL;
			if (picarea[k].tooltext!=NULL)
			{
				m_prop_dlg->m_picarea[k].tooltext = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_prop_dlg->m_picarea[k].tooltext, picarea[k].tooltext);
			}
			else
				m_prop_dlg->m_picarea[k].tooltext = NULL;
			strcpy(m_prop_dlg->m_picarea[k].name, picarea[k].name);
			m_prop_dlg->m_picarea[k].xmin = picarea[k].xmin;
			m_prop_dlg->m_picarea[k].ymin = picarea[k].ymin;
			m_prop_dlg->m_picarea[k].xmax = picarea[k].xmax;
			m_prop_dlg->m_picarea[k].ymax = picarea[k].ymax;
		}
	}
	else
	{
		m_prop_dlg->m_picarea = NULL;
		m_prop_dlg->m_picarea_no = 0;
		m_prop_dlg->m_pic_act_area = 0;
	}
}

#endif






