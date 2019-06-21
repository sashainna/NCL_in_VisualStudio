/************************************************************************
**		FILE NAME: wsntDDbutton.cpp
**
**	 Description - Functions and implementations for
**		CNCLDDButton class (NCL button)
**		with Drag&Drop
**	 CONTAINS:
**		member function of CNCLDDButton
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDButton.cpp , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:31:33
**********************************************************************
*/
#include "wsntstdafx.h"
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxdisp.h >       // MFC OLE automation classes
#include <afxpriv.h>

#include "wsntDDButton.h"
#include "wsntdropsource.h"
#include "wsntDDform.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern char UW_formdlg_font[];
extern HBITMAP WINAPI  CopyWindowToBitmap(LPRECT lpRect, CWnd *pWnd);
extern void CopyPropertyPage(CNCLFormProp *prop_dlg_from, CNCLFormProp *prop_dlg_to);

extern int UW_form_no_picitem;
IMPLEMENT_DYNAMIC(CNCLDDButton, CNCLButton)

/***********************************************************************
**   FUNCTION: CNCLDDButton
**		Constructor of class CNCLDDButton
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDDButton::CNCLDDButton(int type)
	: CNCLButton(type),
	m_TargetDrop(NULL),
		m_TimerID(0)
{
	m_StartPoint = CPoint(-1, -1);
	m_TargetDrop = new CNCLMnDropTarget2();
	m_DragImage = new CImageList();
	m_DragAllImage = NULL;
	m_dragimg_create = 0;
	m_itemno = -1;
	m_prop_dlg = NULL;
	m_prompt = 0;
	m_macro_flag = 0;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLDDButton
**              Destructor of class CNCLDDButton, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDDButton::~CNCLDDButton()
{
	if (m_prop_dlg!=NULL)
		delete m_prop_dlg;
	if (m_DragImage!=NULL)
		delete m_DragImage;
	if (m_DragAllImage!=NULL)
		delete m_DragAllImage;
}

BEGIN_MESSAGE_MAP(CNCLDDButton, CMFCButton)
	//{{AFX_MSG_MAP(CNCLDDButton)
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
int CNCLDDButton::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CMFCButton::OnCreate(lpCreateStruct) == -1)
		return -1;
	m_fieldFont.CreatePointFont(80, UW_formdlg_font);
	SetFont(&m_fieldFont);	
	if (m_prop_dlg==NULL)
	{
		int type = 4;
		if (m_prompt)
			type = 3;
		if (m_macro_flag)
			type = 5;
		m_prop_dlg = new CNCLFormProp(type,m_type,this);
	}
/*
......set size
*/	
	RECT rect;
	GetWindowRect(&rect);	
	m_parent->ScreenToClient(&rect);
	if (m_type!=25)
		GetWindowText(m_prop_dlg->m_label);
/*
......set pos
*/	
	m_prop_dlg->m_pos[0] = rect.left;
	m_prop_dlg->m_pos[1] = rect.top;
	m_prop_dlg->m_size[0] = rect.right - rect.left;
	m_prop_dlg->m_size[1] = rect.bottom - rect.top;
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
void CNCLDDButton::OnDestroy() 
{
	if(m_TargetDrop)
	{
		m_TargetDrop->Revoke();
		delete m_TargetDrop;
	}
	m_TargetDrop = NULL;
	CMFCButton::OnDestroy();
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
void CNCLDDButton::InitDrag()
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
void CNCLDDButton::OnLButtonDown(UINT nFlags, CPoint point) 
{
	m_StartPoint = 	point;
	m_TimerID = SetTimer(1, 100, NULL);	
	CMFCButton::OnLButtonDown(nFlags, point);
	RECT rectClient;
	GetClientRect(&rectClient);
	int cx, cy;
	cx = rectClient.right - rectClient.left;
	cy =  rectClient.bottom -  rectClient.top;
/*
......draw the select box and select this item
*/
	int flag;
	if (nFlags&MK_CONTROL)
		flag = 1;
	else if (nFlags&MK_SHIFT)
		flag = 2;
	else
		flag = 0;
	int type;
	if (m_macro_flag)
	{
		flag = 0;
		type = 5;
//		((CNCLDDform*)m_parent)->OnSelectAutoItem(m_itemno, type, m_type, flag);
//		return;
	}
	else
	{
		type = 4;
		if (m_prompt)
			type = 3;
	}
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
void CNCLDDButton::OnLButtonUp(UINT nFlags, CPoint point) 
{
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;
	
	if(m_TimerID)
	{
		KillTimer(m_TimerID);
		m_TimerID = 0;
	}
	CMFCButton::OnLButtonUp(nFlags, point);
//	if (m_macro_flag)
//		return;
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
void CNCLDDButton::OnRButtonUp(UINT nFlags, CPoint point) 
{
	if (m_macro_flag)
		return CMFCButton::OnRButtonUp(nFlags, point);
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
		if (m_prompt)
			type = 3;
		((CNCLDDform*)m_parent)->OnSelectItem(m_itemno, type, m_type, flag);
	}
	if (cmdid==ID_POPUP_EDIT_ITEM)
/*
......display property form
*/
	{
		if (m_prop_dlg==NULL)
		{
			type = 4;
			if (m_prompt)
				type = 3;
			if (m_macro_flag)
				type = 5;
			m_prop_dlg = new CNCLFormProp(type,m_type,this);
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
		ClientToScreen(&point);
		((CNCLDDform*)m_parent)->ScreenToClient(&point);
		((CNCLDDform*)m_parent)->HandleButtonup(nFlags, point);
		return;
	}
	else if (cmdid==ID_POPUP_DELETE)
/*
......delete this item
*/
	{
		int itype = 1;
		type = 4;
		if (m_prompt)
			type = 3;
		((CNCLDDform*)m_parent)->SetActionItem(3,m_itemno, type, itype);
		((CNCLDDform*)m_parent)->PostMessage(WM_COMMAND, ID_DELETE_FRMITEM);
	}
done:;	
	CMFCButton::OnRButtonUp(nFlags, point);
}
void CNCLDDButton::SetProperty(CNCLFormProp *prop_dlg, int flag)
{
	UX_pathname vfile;
	if (flag==1)
	{
		m_prop_dlg->m_label = prop_dlg->m_label;
		m_prop_dlg->m_pcolor = prop_dlg->m_pcolor;
		m_prop_dlg->m_active = prop_dlg->m_active;
		goto text_change;
	}
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
	if (m_prop_dlg->m_input_itemno!=m_itemno)
		((CNCLDDform*)m_parent)->ReplaceItem(m_itemno, m_prop_dlg->m_input_itemno);
	m_itemno = m_prop_dlg->m_input_itemno;
/*
......adust font size and field color
*/
text_change:;
/*
.....if it is video button, no label need to be updated
*/
	if (m_type==25)
	{
		char *temp = (m_prop_dlg->m_label).GetBuffer();
		strcpy(vfile, temp);		
		SetBitMapFile(vfile);
	}
	else
	{
		SetWindowText(m_prop_dlg->m_label);
		int font_size = (int)(80 * m_prop_dlg->m_font);
		if (flag==0)
		{
			m_fieldFont.DeleteObject();
			m_fieldFont.CreatePointFont(font_size, UW_formdlg_font);
			SetFont(&m_fieldFont);
		}
	}
	CRect rect;
	GetWindowRect(&rect);	
	((CNCLDDform*)m_parent)->updateWindow_draw(2,&rect) ;
}
/***********************************************************************
**
**   FUNCTION: :set_prop_values(double font_scale, int input, int active, char *color, char *pcolor)
**
**         Set some of this control's property values
**
**   INPUT: font_scale, 
**			input, active, 
**			ccolor, pcolor
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDDButton::set_prop_values(double font_scale, int input, int active, char *color, char *pcolor)
{
	m_prop_dlg->m_input = input;
	m_prop_dlg->m_font = font_scale;
	m_prop_dlg->m_active = active;
	m_prop_dlg->m_color = color;
	m_prop_dlg->m_pcolor = pcolor;
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
void CNCLDDButton::OnTimer(UINT_PTR nIDEvent) 
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
	CMFCButton::OnTimer(nIDEvent);
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
void CNCLDDButton::OnSize(UINT nType, int cx, int cy) 
{
	CMFCButton::OnSize(nType, cx, cy);
	int type, sel;
	RECT rectClient;
	GetWindowRect(&rectClient);	
	m_parent->ScreenToClient(&rectClient);
	if (m_prop_dlg==NULL)
	{
		type = 4;
		if (m_prompt)
			type = 3;
		if (m_macro_flag)
			type = 5;
		m_prop_dlg = new CNCLFormProp(type,m_type,this);
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
	type = 4;
	if (m_prompt)
		type = 3;
	if (m_macro_flag)
		type = 5;
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
void CNCLDDButton::OnMove(int x, int y)
{
	CMFCButton::OnMove(x, y);
	CRect rectClient;
	GetWindowRect(&rectClient);	
	m_parent->ScreenToClient(&rectClient);
	if (m_prop_dlg==NULL)
	{
		int type = 4;
		if (m_prompt)
			type = 3;
		if (m_macro_flag)
			type = 5;
		m_prop_dlg = new CNCLFormProp(type,m_type,this);
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
	if (m_prompt)
		type = 3;
	if (m_macro_flag)
		type = 5;
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
void CNCLDDButton::OnMouseMove(UINT nFlags, CPoint point) 
{
	if(m_TimerID > 0)
	{
		int iX = m_StartPoint.x - point.x;
		int iY = m_StartPoint.y - point.y;
		if((iX*iX + iY*iY) > 9)
		{
			KillTimer(m_TimerID);
			m_TimerID = 0;

			if (m_itemno==-1)
				return;

			RECT rectClient;
			int cx, cy;
			CNCLDropSource* pdropsource = new CNCLDropSource();
			COleDataSource*	pSource = new COleDataSource();
			if(pSource)
			{
/*
.....Get the drag image list
*/
				if (m_DragAllImage!=NULL)
					delete m_DragAllImage;

				CBitmap *defImage, defcBmap;
				HBITMAP bitmap;
//				if (m_macro_flag==0)
//				{
					m_DragAllImage = new CImageList();		
					if (m_macro_flag==0)
						bitmap = ((CNCLDDform*)m_parent)->CreateAllToBitmap(&cx, &cy);
					else
						bitmap = ((CNCLDDform*)m_parent)->CreateAllToBitmap(&cx, &cy, m_itemno);
					defImage = defcBmap.FromHandle(bitmap);
					m_DragAllImage->Create(cx,cy, ILC_COLOR32|ILC_MASK, 0, 1000);
					m_DragAllImage->Add(defImage, RGB(0,0,0));

					if (m_DragAllImage!=NULL)
						pdropsource->SetImageList(m_DragAllImage);
//				}
//				else
//				{
//					GetClientRect(&rectClient);
//					cx = rectClient.right - rectClient.left;
//					cy =  rectClient.bottom -  rectClient.top;
//					m_DragImage = new CImageList();
//					bitmap = CopyWindowToBitmap(&rectClient, this);
//					defImage = defcBmap.FromHandle(bitmap);
//					m_DragImage->Create(cx,cy, ILC_COLOR32|ILC_MASK, 0, 1000);
//					m_DragImage->Add(defImage, 0xC0C0C0);
//					if (m_DragImage!=NULL)
//						pdropsource->SetImageList(m_DragImage);
//				}
/*
......we need adjust the drag point since the drag image now is draw
......for the whole CNCLDDform, not just the button
.....adject point from client point of button to client to the CNCLDDform
*/
				GetWindowRect(&rectClient);
				CRect testrect = rectClient;
				CWnd *parent_view = ((CNCLDDform*)m_parent)->m_parent;
				parent_view->ScreenToClient(&testrect);

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
				if (m_macro_flag==0)
					iText = "Move CNCLDDButton " + itext2 + iText;
				else
					iText = "Move CNCLDDButton2 " + itext2 + iText;
				pdropsource->SetAttWindow(this, point, testrect);

				sf.Write(iText, iText.GetLength());

				HGLOBAL hMem = sf.Detach();
				if (!hMem) 
					return;
							
				if ((m_prompt)||(m_macro_flag))
					UW_form_no_picitem = 1;
				pSource->CacheGlobalData(CF_TEXT, hMem);
				pSource->DoDragDrop(DROPEFFECT_MOVE|DROPEFFECT_COPY, NULL, pdropsource);
				delete pdropsource;
				delete pSource;
				UW_form_no_picitem = 0;
			}
		}
	}
	CMFCButton::OnMouseMove(nFlags, point);
//	if (m_macro_flag)
//		return;
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
void CNCLDDButton::SetActive(int active)
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
void CNCLDDButton::DrawGuides(CPoint org_pt, POINT ptCursor)
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
void CNCLDDButton::OnDragDropCallback(CPoint pt, char *input_text)
{
	((CNCLDDform*)m_parent)->OnDragDropCallback(pt, input_text, m_macro_flag);
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
void CNCLDDButton::SetItemNo(int itemno)
{
	m_itemno = itemno;
	if (m_prop_dlg==NULL)
	{
		int type = 4;
		if (m_prompt)
			type = 3;
		if (m_macro_flag)
			type = 5;
		m_prop_dlg = new CNCLFormProp(type,m_type,this);
	}
	m_prop_dlg->m_input_itemno = itemno;
}

void CNCLDDButton::set_prop_HSPvalues(int n_picarea, UD_PICAREA *picarea)
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

