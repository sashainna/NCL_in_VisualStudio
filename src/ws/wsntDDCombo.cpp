/************************************************************************
**		FILE NAME: wsntDDCombo.cpp
**
**	 Description - Functions and implementations for
**		CNCLDDCombo class with Drag&Drop
**	 CONTAINS:
**		member function of CNCLDDCombo
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDCombo.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			05/04/15 , 11:34:01
**********************************************************************
*/
#include "wsntstdafx.h"

#include <afxole.h>         // MFC OLE classes
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxdisp.h >       // MFC OLE automation classes
#include <afxpriv.h>

#include "wsntDDCombo.h"
#include "wsntdropsource.h"
#include "wsntDDform.h"
#define HT_UPDOWN	10

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern char UW_formdlg_font[];
extern HBITMAP WINAPI  CopyWindowToBitmap(LPRECT lpRect, CWnd *pWnd);
extern void CopyPropertyPage(CNCLFormProp *prop_dlg_from, CNCLFormProp *prop_dlg_to);

IMPLEMENT_DYNAMIC(CNCLDDCombo, CComboBox)	//	Want info for drag

/***********************************************************************
**   FUNCTION: CNCLDDCombo
**		Constructor of class CNCLDDCombo
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDDCombo::CNCLDDCombo(int type)
	:	m_TargetDrop(NULL),
		m_TimerID(0)
{
	m_StartPoint = CPoint(-1, -1);
	m_TargetDrop = new CNCLMnDropTarget2();
	m_DragImage = new CImageList();
	m_dragimg_create = 0;
	m_type = type;
	m_itemno = -1;
	m_parent = NULL;
	m_prop_dlg = NULL;
	m_DragAllImage = NULL;
	m_choice_num = 0;
	m_combchoice = NULL;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLDDCombo
**              Destructor of class CNCLDDCombo, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDDCombo::~CNCLDDCombo()
{
	if (m_prop_dlg!=NULL)
		delete m_prop_dlg;
	if (m_DragImage!=NULL)
		delete m_DragImage;
	if (m_DragAllImage!=NULL)
		delete m_DragAllImage;
	if (m_combchoice!=NULL)
	{
		for (int j=0; j<m_choice_num; j++)
		{
			if (m_combchoice[j]!=NULL)
				free (m_combchoice[j]);
		}
		free (m_combchoice);
	}
}

void CNCLDDCombo::adddata(int num, UD_DASIN *data)
{
	int len;
	if (num<=0)
		return;
			
	int j, cont = GetCount();
	for (j = 0; j<cont; j++)
	{
		DeleteString(0);
	}
	m_choice_num = num;
	m_combchoice = (char**)malloc((m_choice_num)*sizeof(char*));
	for (j=0; j<num; j++)
	{
		len = strlen(data[j].dstr);
		m_combchoice[j] = (char*)malloc((len+1)*sizeof(char));
		strcpy_s(m_combchoice[j], len+1, data[j].dstr);
		AddString(m_combchoice[j]);
	}
	SetCurSel(0);

	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(4, m_type,this);
	}
	m_prop_dlg->adddata(num, data);
}

BEGIN_MESSAGE_MAP(CNCLDDCombo, CComboBox)
	//{{AFX_MSG_MAP(CNCLDDCombo)
	ON_WM_LBUTTONUP()
	ON_WM_LBUTTONDOWN()
	ON_WM_RBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_WM_TIMER()
	ON_WM_DESTROY()
	ON_WM_SIZE()
	ON_WM_MOVE()
	ON_WM_CREATE()
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

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
void CNCLDDCombo::InitDrag()
{
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
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
int CNCLDDCombo::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CComboBox::OnCreate(lpCreateStruct) == -1)
		return -1;
	m_fieldFont.CreatePointFont(80, UW_formdlg_font);
	SetFont(&m_fieldFont);
	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(4, m_type,this);
	}
/*
......set size
*/	
	RECT rect;
	GetWindowRect(&rect);	
	m_parent->ScreenToClient(&rect);
	GetWindowText(m_prop_dlg->m_label);
/*
......set pos
*/	
	m_prop_dlg->m_pos[0] = rect.left;
	m_prop_dlg->m_pos[1] = rect.top;
		
	CRect listrect;
	GetDroppedControlRect(&listrect);
	m_prop_dlg->m_size[0] = listrect.right - listrect.left;
	m_prop_dlg->m_size[1] = listrect.bottom - listrect.top;
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
void CNCLDDCombo::OnDestroy() 
{
	if(m_TargetDrop)
	{
		m_TargetDrop->Revoke();
		delete m_TargetDrop;
	}
	m_TargetDrop = NULL;

	CComboBox::OnDestroy();
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
void CNCLDDCombo::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CComboBox::OnLButtonDown(nFlags, point);
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
void CNCLDDCombo::OnLButtonUp(UINT nFlags, CPoint point) 
{
	CComboBox::OnLButtonUp(nFlags, point);
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
void CNCLDDCombo::OnRButtonUp(UINT nFlags, CPoint point) 
{
	if (m_parent==NULL)
		return;
	CComboBox::OnRButtonUp(nFlags, point);
}
void CNCLDDCombo::SetProperty(CNCLFormProp *prop_dlg)
{
	RECT rectClient;
	GetWindowRect(&rectClient);	
	m_parent->ScreenToClient(&rectClient);
/*
.....copy all prop_dlg date into the member value m_prop_dlg
*/
	CopyPropertyPage(prop_dlg, m_prop_dlg);
// the prompt should set in label prompt, not the combobox here
//	SetWindowText(m_prop_dlg->m_label);
	rectClient.left = m_prop_dlg->m_pos[0];
	rectClient.top = m_prop_dlg->m_pos[1];
	rectClient.right = rectClient.left + m_prop_dlg->m_size[0];
	rectClient.bottom = rectClient.top + m_prop_dlg->m_size[1];
	MoveWindow(&rectClient);
/*
......adust font size and field color
*/
	int font_size = (int)(80 * m_prop_dlg->m_font);
	m_fieldFont.DeleteObject();
	m_fieldFont.CreatePointFont(font_size, UW_formdlg_font);
	SetFont(&m_fieldFont);
	if (m_prop_dlg->m_input_itemno!=m_itemno)
		((CNCLDDform*)m_parent)->ReplaceItem(m_itemno, m_prop_dlg->m_input_itemno);
	m_itemno = m_prop_dlg->m_input_itemno;
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
void CNCLDDCombo::OnSize(UINT nType, int cx, int cy) 
{
	CComboBox::OnSize(nType, cx, cy);

	RECT rectClient;
	GetWindowRect(&rectClient);
	((CNCLDDform*)m_parent)->ScreenToClient(&rectClient);

	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(4, m_type, this);
	}
/*
......set size
*/	
	CRect listrect;
	GetDroppedControlRect(&listrect);
	m_prop_dlg->m_size[0] = listrect.right - listrect.left;
	m_prop_dlg->m_size[1] = listrect.bottom - listrect.top;
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
void CNCLDDCombo::OnMove(int x, int y)
{
	CComboBox::OnMove(x, y);
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
.....updated the property page only if it si the select item
*/
	int type, sel;
	type = 4;
	sel = ((CNCLDDform*)m_parent)->IsSelItem(m_itemno, type, m_type);
	if (sel)
		((CNCLDDform*)m_parent)->UpdatePropertyPos(rectClient.left, rectClient.top);
}

// *****************************************************************
//	OnMouseMove
// *****************************************************************
void CNCLDDCombo::OnMouseMove(UINT nFlags, CPoint point) 
{
	CComboBox::OnMouseMove(nFlags, point);
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
void CNCLDDCombo::OnTimer(UINT_PTR nIDEvent) 
{
	if(nIDEvent == 1)
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
	CComboBox::OnTimer(nIDEvent);
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
void CNCLDDCombo::OnDragDropCallback(CPoint pt, char *input_text)
{
	((CNCLDDform*)m_parent)->OnDragDropCallback(pt, input_text);
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
BOOL CNCLDDCombo::PreTranslateMessage(MSG* pMsg) 
{
	int stat = 0;
	HWND hWnd = (HWND)*this; 

	if (hWnd==NULL)
		return 0;
	if ((pMsg->message == WM_LBUTTONDOWN)||(pMsg->message == WM_LBUTTONDBLCLK))
/*
......since the LBUTTONDOWN message will not called when it's simple combobox
......so we do the function here
*/
	{
		m_StartPoint = 	pMsg->pt;
		m_TimerID = SetTimer(1, 100, NULL);

		RECT rectClient;
		GetClientRect(&rectClient);
		int cx, cy;
		cx = rectClient.right - rectClient.left;
		cy =  rectClient.bottom -  rectClient.top;
/*
......draw the select box and select this item
*/
		int type = 4;
		CPoint point = pMsg->pt;
		if (m_parent!=NULL)
		{
			int flag;
			if (pMsg->wParam&MK_CONTROL)
				flag = 1;
			else if (pMsg->wParam&MK_SHIFT)
				flag = 2;
			else
				flag = 0;
			((CNCLDDform*)m_parent)->OnSelectItem(m_itemno, type, m_type, flag);
			((CNCLDDform*)m_parent)->ScreenToClient(&point);
			int dropdown;
			CPoint pt = pMsg->pt;
			ScreenToClient(&pt);
			CRect arrow_rect = rectClient;
			arrow_rect.left = arrow_rect.right - 20;
			if (arrow_rect.PtInRect(pt))
			{
				dropdown = 2;
				((CNCLDDform*)m_parent)->SetSizeDir(HT_UPDOWN);
			}
			else
				dropdown = 1;
			((CNCLDDform*)m_parent)->HandleButtonDown(pMsg->wParam, point, dropdown);
		}
		while(::PeekMessage(pMsg, m_hWnd, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE));
		return TRUE;
	}
	if (pMsg->message == WM_LBUTTONUP)
	{
		m_StartPoint.x = -100;
		m_StartPoint.y = -100;
/*
.....if the listbox is open
*/
		int flag;
		BOOL status = GetDroppedState();
		if (status)
			flag = 2;
		else
			flag = 1;
		if(m_TimerID)
		{
			KillTimer(m_TimerID);
			m_TimerID = 0;
		}
/*
......draw the select box and select this item
*/
		CPoint point = pMsg->pt;
		if (m_parent!=NULL)
		{
			((CNCLDDform*)m_parent)->ScreenToClient(&point);
			((CNCLDDform*)m_parent)->HandleButtonup(pMsg->wParam, point, flag);
		}
		return TRUE;
	}
	if (pMsg->message == WM_RBUTTONUP)
	{
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
				m_prop_dlg = new CNCLFormProp(4, m_type,this);
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
			m_prop_dlg->m_pos[0] = rectClient.left;
			m_prop_dlg->m_pos[1] = rectClient.top;

			CRect listrect;
			GetDroppedControlRect(&listrect);
			m_prop_dlg->m_size[0] = listrect.right - listrect.left;
			m_prop_dlg->m_size[1] = listrect.bottom - listrect.top;
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
			type = 4;
			int itype = m_type;
			((CNCLDDform*)m_parent)->SetActionItem(3,m_itemno, type, itype);
			((CNCLDDform*)m_parent)->PostMessage(WM_COMMAND, ID_DELETE_FRMITEM);
		}
		return TRUE;
	}
	if (pMsg->message == WM_MOUSEMOVE)
	{
		if (!(pMsg->wParam&MK_LBUTTON))
		{
/*
.....if not left button down, reset timer for drag
*/
			if(m_TimerID > 0)
			{
				KillTimer(m_TimerID);
				m_TimerID = 0;
			}
		}
		if(m_TimerID > 0)
		{
			CPoint point = pMsg->pt;
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
					ScreenToClient(&point);
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
					iText = "Move CNCLDDCombo " + itext2 + iText;
					pdropsource->SetAttWindow(this, point, rectClient);

					if(iText.GetLength())
					{
						sf.Write(iText, iText.GetLength());

						HGLOBAL hMem = sf.Detach();
						if (!hMem) 
							goto done;
						pSource->CacheGlobalData(CF_TEXT, hMem);
						pSource->DoDragDrop(DROPEFFECT_MOVE|DROPEFFECT_COPY, NULL, pdropsource);
					}
					delete pdropsource;
					delete pSource;
				}
			}
		}
		CPoint point = pMsg->pt;
		if (m_parent!=NULL)
		{
			((CNCLDDform*)m_parent)->ScreenToClient(&point);
			((CNCLDDform*)m_parent)->HandleMouseMove(pMsg->wParam, point);
		}
	}
done:;
	return CComboBox::PreTranslateMessage(pMsg);
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
void CNCLDDCombo::SetActive(int active)
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
void CNCLDDCombo::DrawGuides(CPoint org_pt, POINT ptCursor)
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
void CNCLDDCombo::SetItemNo(int itemno)
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
**			ccolor, pcolor
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDDCombo::set_prop_values(double font_scale, int active, char *color, char *pcolor)
{
	m_prop_dlg->m_font = font_scale;
	m_prop_dlg->m_active = active;
	m_prop_dlg->m_color = color;
	m_prop_dlg->m_pcolor = pcolor;
}
/***********************************************************************
**
**   FUNCTION: OnPaint()
**
**       Combo box paint function
**
**   INPUT:  None
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDDCombo::OnPaint() 
{
	CComboBox::OnPaint();
}

void CNCLDDCombo::set_prop_HSPvalues(int n_picarea, UD_PICAREA *picarea)
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
