/************************************************************************
**
**   FILE NAME: wsntformview.cpp
**
**	 Description - Functions and implementations for
**		CNCLFormView class
**
**	 CONTAINS: 
**		class functions of CNCLFormView class
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntformview.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			05/04/15 , 11:56:19
***********************************************************************
*/
#include "stdafx.h"
#include "wsntctl.h"
#include <gdiplus.h>
#include "wsntformdoc.h"
#include "wsntfrmview.h"
#include "wsntfrmMview.h"

#include "wsntddedit.h"
#include "wsntddbutton.h"
#include "wsntDDform.h"
#include "wsntcfunc.h"
#include "wsntsecbtn.h"
#include "wsntclrdlg.h"
#include "wsntsecprop.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
#define SEP_WID 3
extern CFrameWnd *UW_Dform_frame;
extern "C" float uw_form_scalex;
extern "C" float uw_form_scaley;
extern "C"  int uw_color_table[64][3];
extern "C"  char uw_color_name[64][96];
extern int uw_get_rgb (char *color_str, COLORREF &color) ;
//extern void uw_MapDialogRect(CRect &rect);
extern void uw_MapDialogRect(CRect &rect, CDialog *wnd);

/////////////////////////////////////////////////////////////////////////////
// CNCLFormView

IMPLEMENT_DYNCREATE(CNCLFormView, CFormView)

BEGIN_MESSAGE_MAP(CNCLFormView, CFormView)
	//{{AFX_MSG_MAP(CNCLFormView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	ON_WM_SIZE()
	ON_WM_DESTROY()
	ON_WM_MOUSEMOVE()
	ON_COMMAND_RANGE(IDC_FORMDESGN_SEC, IDC_FORMDESGN_SEC+50, OnSectionSel)
	ON_MESSAGE(ID_FORM_DELSEC_BUT, OnDeleteSecBut)
	//}}AFX_MSG_MAP
	// Standard printing commands
END_MESSAGE_MAP()

/***********************************************************************
c
c   FUNCTION: OnDeleteSecBut(WPARAM wParam, LPARAM lParam)
c
c          message callback for delete a select section button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
LRESULT CNCLFormView::OnDeleteSecBut(WPARAM wParam, LPARAM lParam)
{
	delete m_delete_secbut;
	return 0;
}

/***********************************************************************
c
c   FUNCTION: OnSectionSel(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormView::OnSectionSel(UINT id)
{
	int i, page;
	if (m_type!=0)
		return;

	if (id==IDC_FORMDESGN_SEC+50)
	{
		page = 50;
		goto next;
	}
	for (i=0; i<m_secno;i++)
	{
		if (m_sec_id[i]==id)
		{
			if (((CNCLSecButton*)m_sec_win[i])->m_type==3)
/*
......doing nothing if it is a seperator
*/
				return;
			page = ((CNCLSecButton*)m_sec_win[i])->m_page;
			break;
		}
	}
next:;
	OnSectionSelItem(page);
}
/*
......here page is not index of select button, but page number (not include seperator)
*/
void CNCLFormView::OnSectionSelItem(int page)
{
	if (m_type!=0)
		return;
	m_selsec = page;
	CPoint point, wpt;
	GetCursorPos(&point);
	ScreenToClient(&point);
/*
.....check if the mouse is inside the button
*/
	CRect rect;
	for(int i = 0; i < m_secno; i++ )
	{
		if (((CNCLSecButton*)m_sec_win[i])->m_type==3)
			continue;
		((CNCLSecButton*)m_sec_win[i])->GetWindowRect(rect);
		ScreenToClient(&rect);
		if (page==((CNCLSecButton*)m_sec_win[i])->m_page)
			((CNCLSecButton*)m_sec_win[i])->m_select = 1;
		else
			((CNCLSecButton*)m_sec_win[i])->m_select = 0;

		if(rect.PtInRect(point))
		{
			if (((CNCLSecButton*)m_sec_win[i])->m_focus==0)
			{
				((CNCLSecButton*)m_sec_win[i])->DrawHighLight2();
				((CNCLSecButton*)m_sec_win[i])->m_focus = 1;
			}
		}
		else
		{
			((CNCLSecButton*)m_sec_win[i])->DrawNormal2();
			((CNCLSecButton*)m_sec_win[i])->m_focus = 0;
			((CNCLSecButton*)m_sec_win[i])->CloseToolTip();
		}
	}
all_but:;
	((CNCLSecButton*)m_sec_win[50])->GetWindowRect(rect);
	ScreenToClient(&rect);
	if (page==50)
		((CNCLSecButton*)m_sec_win[50])->m_select = 1;
	else
		((CNCLSecButton*)m_sec_win[50])->m_select = 0;
	if(rect.PtInRect(point))
	{
		if (((CNCLSecButton*)m_sec_win[50])->m_focus==0)
		{
			((CNCLSecButton*)m_sec_win[50])->DrawHighLight2();
			((CNCLSecButton*)m_sec_win[50])->m_focus = 1;
		}
	}
	else
	{
		((CNCLSecButton*)m_sec_win[50])->DrawNormal2();
		((CNCLSecButton*)m_sec_win[50])->m_focus = 0;
		((CNCLSecButton*)m_sec_win[50])->CloseToolTip();
	}
	m_right_form->OnSecButton(page);
	RedrawWindow();
}

/***********************************************************************
**   FUNCTION: CNCLFormView
**		Constructor of class CNCLFormView
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormView::CNCLFormView() : CFormView(CNCLFormView::IDD)
{
	m_init = 0;
	m_type = 0;
	m_right_form = NULL;
	m_rect.top = m_rect.bottom = m_rect.left = m_rect.right;
	m_viewrect.top = m_viewrect.bottom = m_viewrect.left = m_viewrect.right;
	m_parent_dlg = NULL;
	m_formchanged = 0;
	m_view_reset = 0;
	m_prop_dlg = new CNCLFormProp(-1, -1, this);
	m_selsec = -1;
	for (int i=0; i<50; i++)
	{
		m_sec_win[i] = NULL;
		m_sec_color[i][0] = '\0';
		m_sec_name[i][0] = '\0';
		m_sec_id[i] = -1;
	}
	m_sec_win[50] = NULL;
	m_box1 = NULL;
	m_box2 = NULL;
	m_del_id = -1;
	m_sec_flag = 0;
	m_secno = 0;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLFormView
**              Destructor of class CNCLFormView, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormView::~CNCLFormView()
{
	if (m_right_form!=NULL)
	{
		delete m_right_form;
		m_right_form = NULL;
	}
	if (m_prop_dlg!=NULL)
	{
		delete m_prop_dlg;
		m_prop_dlg = NULL;
	}
	for (int i=0; i<m_secno; i++)
	{
		if (m_sec_win[i]!=NULL)
			delete m_sec_win[i];
	}
	if (m_sec_win[50]!=NULL)
		delete m_sec_win[50];
}

int CNCLFormView::get_valid_id()
{
	int id;
	if (m_del_id!=-1)
	{
		id = m_del_id;
		m_del_id = -1;
		return id;
	}
	else
		return IDC_FORMDESGN_SEC + m_secno;
}
/***********************************************************************
**
**   FUNCTION: LoadFormItem(UD_FSTRUCT *fstruct)
**			Create all form item according to the form structure
**		
**	 INPUT:  fstruct: form structure
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLFormView::LoadFormItem(UD_FSTRUCT *fstruct)
{
	int i;
	char name[40];
	for (i=0; i<50; i++)
	{
		if (m_sec_win[i]!=NULL)
			delete m_sec_win[i];
		m_sec_win[i] = NULL;
	}
	m_del_id = -1;
	if (m_sec_win[50]!=NULL)
		delete m_sec_win[50];
	if (m_box1!=NULL)
		delete m_box1;
	m_box1 = NULL;
	if (m_box2!=NULL)
		delete m_box2;
	m_box2 = NULL;
	if (fstruct->n_section_fields>0)
		m_sec_flag = 1;
	else
		m_sec_flag = 0;
	m_sec_win[50] = NULL;
	m_secno = 0;
	if (((*fstruct).dockable[0] == 0)&&((*fstruct).dockable[1]==0)
		&&((*fstruct).dockable[2] == 0)&&((*fstruct).dockable[3]==0))
		m_type = 0;
	else if (((*fstruct).dockable[0] == 1)&&((*fstruct).dockable[1]==1)
		&&((*fstruct).dockable[2] == 1)&&((*fstruct).dockable[3]==1))
		m_type = 1;
	else if (((*fstruct).dockable[0] == 1)&&((*fstruct).dockable[1]==0)
		&&((*fstruct).dockable[2] == 0)&&((*fstruct).dockable[3]==0))
		m_type = 2;
	else if (((*fstruct).dockable[0] == 0)&&((*fstruct).dockable[1]==1)
		&&((*fstruct).dockable[2] == 0)&&((*fstruct).dockable[3]==0))
		m_type = 3;
	else if (((*fstruct).dockable[0] == 0)&&((*fstruct).dockable[1]==0)
		&&((*fstruct).dockable[2] == 1)&&((*fstruct).dockable[3]==0))
		m_type = 4;
	else if (((*fstruct).dockable[0] == 0)&&((*fstruct).dockable[1]==0)
		&&((*fstruct).dockable[2] == 0)&&((*fstruct).dockable[3]==1))
		m_type = 5;

	if ((m_type!=0)&&(m_box1!=NULL))
		m_box1->ShowWindow(SW_HIDE);
	if ((m_type!=0)&&(m_box2!=NULL))
		m_box2->ShowWindow(SW_HIDE);

	if (fstruct->n_section_fields==0)	
		m_right_form->ModifyStyle(0, WS_BORDER, 0);
	else
		m_right_form->ModifyStyle(WS_BORDER, 0, 0);

	int chrwid, phgt;
	CRect vrect, rect, rect0, rect1, rect2;
	GetWindowRect(&vrect);
	ScreenToClient(&vrect);
	GetClientRect(&m_rect);

	int secmax, start_x = 0;
/*
......this buttion will be created when initial dialog form
*/
	int widlabel = 0;
	for (i=0; i<fstruct->n_section_fields; i++)
	{
		uw_ntget_strsize(fstruct->section_flds[i].name, 80, "MS Sans Serif", &chrwid, &phgt);
		if (widlabel<chrwid)
			widlabel = chrwid;
	}
	if (fstruct->n_section_fields>=2)
		secmax = fstruct->n_section_fields;
	else 
	{
		uw_ntget_strsize("Xx", 80, "MS Sans Serif", &chrwid, &phgt);
 		secmax = 0;
	}
/*
.....we need consider bold text, so adjust by widlabel*1.02
*/
	widlabel = widlabel*1.02;
	if (widlabel<50)
		widlabel = 50;

	CRect rect_b;
	if (m_box1!=NULL)
	{
		m_box1->GetWindowRect(&rect_b);
		ScreenToClient(&rect_b);
	}
	if ((m_sec_flag)&&(m_type==0)&&(m_box1==NULL))
/*
.....float
*/
	{
		start_x = widlabel + 10;
		rect0.left = 0;
		rect0.right = start_x-5;
		rect0.top = 0;
		rect0.bottom = (short)((*fstruct).ud_frmdf.ud_frc.ud_r*uw_form_scaley);
//		uw_MapDialogRect(rect0);
		uw_MapDialogRect(rect0, ((CDialog*)m_parent_dlg));

		GetClientRect(&m_rect);
		rect.top = m_rect.top-1;
		rect.left = m_rect.left-1;
		rect.bottom = m_rect.bottom+1;
		rect.right = m_rect.right+1;
/*
......leave 20 unit for active bar area height
*/
		if (m_parent_dlg!=NULL)
		{
			CRect temp2 (0,0, 20, 20);
//			uw_MapDialogRect(temp2);
			uw_MapDialogRect(temp2, ((CDialog*)m_parent_dlg));
			rect.top = m_rect.top;
			rect.bottom = m_rect.bottom - temp2.Height();//  - (2+8);
			rect.left = m_rect.left;
			rect.right = m_rect.right;
		}
		//
		rect1.bottom = rect.bottom;
		rect1.left = rect.left + 8;
		rect1.right = rect1.left + rect0.Width();
		rect1.top = rect.top;

		m_box1 = new CButton();
		m_box1->Create("", WS_VISIBLE | WS_CHILD | WS_GROUP | BS_GROUPBOX, 
				rect1, this, IDC_DLG_BOX1);
		rect_b = rect1;

		rect0.left = start_x;
		rect0.right = start_x + (short)((*fstruct).ud_frmdf.ud_frc.ud_c *uw_form_scalex);
		rect0.top = 0;
		rect0.bottom = (short)((*fstruct).ud_frmdf.ud_frc.ud_r*uw_form_scaley);
//		uw_MapDialogRect(rect0);
		uw_MapDialogRect(rect0, ((CDialog*)m_parent_dlg));

		rect2 = rect1;
		rect2.left = rect0.left + 8;
		rect2.right = rect0.right+2; 
		m_box2 = new CButton();
		m_box2->Create("", WS_VISIBLE | WS_CHILD | WS_GROUP | BS_GROUPBOX, 
				rect2, this, IDC_DLG_BOX2);
		rect2.top = rect2.top + 8;
		m_right_form->MoveWindow(&rect2,1);
	}
	if (m_sec_flag==0)
	{
		if (m_parent_dlg!=NULL)
		{
			CRect temp3 (0,0, 25, 25);
			if (m_type==0)
			{
//				uw_MapDialogRect(temp3);
				uw_MapDialogRect(temp3, ((CDialog*)m_parent_dlg));
				rect.top = m_rect.top + 6 + 8;
				rect.bottom = m_rect.bottom - temp3.Height() + 8;
				rect.left = m_rect.left + 10;
				rect.right = m_rect.right - 10;
			}
			else
			{
				rect.top = m_rect.top + 5;
				rect.bottom = m_rect.bottom - 5 - ((CNCLFdsnFrame*)UW_Dform_frame)->m_dely;
				rect.left = m_rect.left + 5;
				rect.right = m_rect.right - 5 - ((CNCLFdsnFrame*)UW_Dform_frame)->m_delx;
				((CNCLFdsnFrame*)UW_Dform_frame)->m_delx = 0;
				((CNCLFdsnFrame*)UW_Dform_frame)->m_dely = 0;
			}
			m_right_form->ModifyStyle(0, WS_BORDER, 0);
			m_right_form->MoveWindow(&rect,1);
		}
	}
	int indx, sep = 0, page=0;
	COLORREF ocolor = ::GetSysColor(COLOR_BTNTEXT);
	COLORREF color;
	if ((m_sec_flag)&&(m_type==0))
	{
		for (i=0; (i<fstruct->n_section_fields)||(i<secmax); i++)
		{
			if (fstruct->section_flds[i].type==0)
			{
				rect.left = rect_b.left + 2;
				rect.right = rect_b.right-4;
				rect.top = rect_b.top+ (phgt+2) + page * (phgt+15+2)+ sep*SEP_WID;
				rect.bottom = rect.top + (phgt+15);

				m_sec_win[m_secno] = new CNCLSecButton();
				((CNCLSecButton*)m_sec_win[m_secno])->SetParent(this);
				((CNCLSecButton*)m_sec_win[m_secno])->set_itemnum(m_secno);
				((CNCLSecButton*)m_sec_win[m_secno])->SetType(1);
				((CNCLSecButton*)m_sec_win[m_secno])->SetPageNum(page);
				if (i<fstruct->n_section_fields)
				{
					((CNCLSecButton*)m_sec_win[m_secno])->Create(fstruct->section_flds[i].name, 
						WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT | WS_TABSTOP | BS_OWNERDRAW, 
						rect, this, (UINT)(IDC_FORMDESGN_SEC + m_secno));
					strcpy(m_sec_name[m_secno], fstruct->section_flds[i].name);
					indx = uw_get_rgb (fstruct->section_flds[i].color, color);
					if (indx==-1)
						color = ocolor;
					strcpy(m_sec_color[m_secno], fstruct->section_flds[i].color);
					((CNCLSecButton*)m_sec_win[m_secno])->InitDrag();
				}
				else
				{
					sprintf(name, "Section %d", i+1);
					((CNCLSecButton*)m_sec_win[m_secno])->Create(name, 
						WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT | WS_TABSTOP | BS_OWNERDRAW, 
						rect, this, (UINT)(IDC_FORMDESGN_SEC + m_secno));
					strcpy(m_sec_name[m_secno], name);
					strcpy(m_sec_color[m_secno], "Default");
					color = ocolor;
					((CNCLSecButton*)m_sec_win[m_secno])->InitDrag();
				}
				((CNCLSecButton*)m_sec_win[m_secno])->SetTColor(color);
				m_sec_id[m_secno] = IDC_FORMDESGN_SEC + m_secno;
				page++;
			}
			else
			{
				rect.left = rect_b.left + 2;
				rect.right = rect_b.right-4;
				rect.top = rect_b.top+ (phgt+2) + page * (phgt+15+2)+ sep*SEP_WID;
				rect.bottom = rect.top + SEP_WID;
				m_sec_win[m_secno] = new CNCLSecButton();
				((CNCLSecButton*)m_sec_win[m_secno])->SetParent(this);
				((CNCLSecButton*)m_sec_win[m_secno])->set_itemnum(m_secno);
				((CNCLSecButton*)m_sec_win[m_secno])->SetType(3);
				((CNCLSecButton*)m_sec_win[m_secno])->SetPageNum(-1);
/*
.....we still need id to delete
*/
				m_sec_id[m_secno] = IDC_FORMDESGN_SEC + m_secno;
				((CNCLSecButton*)m_sec_win[m_secno])->Create(name, 
						WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT | WS_TABSTOP | BS_OWNERDRAW, 
						rect, this, (UINT)(-1));
				sep++;
			}
			((CNCLSecButton*)m_sec_win[m_secno])->ShowWindow(SW_SHOW);
			m_secno++;
		}
		rect.left = rect_b.left + 2;
		rect.right = rect_b.right-4;
		rect.top = rect_b.top+ (phgt+2) + page * (phgt+15+2) + sep*SEP_WID;
		rect.bottom = rect.top + (phgt+15);
		m_sec_win[50] = new CNCLSecButton();
		((CNCLSecButton*)m_sec_win[50])->SetParent(this);
		((CNCLSecButton*)m_sec_win[50])->set_itemnum(50);
		((CNCLSecButton*)m_sec_win[50])->Create("ALL", 
					WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT | WS_TABSTOP | BS_OWNERDRAW, 
					rect, this, (UINT)(IDC_FORMDESGN_SEC + 50));
		COLORREF tcolor = ::GetSysColor(COLOR_BTNTEXT);
		((CNCLSecButton*)m_sec_win[50])->SetTColor(tcolor);
		((CNCLSecButton*)m_sec_win[50])->ShowWindow(SW_SHOW);
		((CNCLSecButton*)m_sec_win[50])->InitDrag();
	}
	m_right_form->LoadFormItem(fstruct);
	m_right_form->SetsecNum(m_secno);
	if (m_secno>0)
	{
		OnSectionSelItem(0);
		if (UW_Dform_frame!=NULL)
		{
			CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
			pview->SetFormSecNo(m_secno);
		}
	}
}

void CNCLFormView::SetSecInfo(int indx, char *name, char *color)
{
}
void CNCLFormView::SetSectionAttr(int indx, CString label, CString color)
{
	int nc = label.GetLength();
	char *temp = label.GetBuffer(nc);
	strcpy(m_sec_name[indx], temp);
	char *temp1 = color.GetBuffer(nc);
	strcpy(m_sec_color[indx], temp1);
	OnChangeSecProperty(indx, 1);
}
	
void CNCLFormView::OnDeleteSec(int indx)
{
	if (m_secno<=1)
		return;
	int i;
/*
.....prompt ask first
*/
	int result = MessageBox("Delete this section will delete all items\r\nwithin this section, continue?",
				"Delete Section", MB_YESNO);
	int sep = 0;
	if (result==IDYES)
	{
		if (m_sec_win[indx]!=NULL)
		{
			if (((CNCLSecButton*)m_sec_win[indx])->GetType()==3)
				sep = 1;
		}
		m_right_form->DeleteSec(indx);
		if (indx==50)
		{
			RedrawWindow();
			return;
		}
		if (m_sec_win[indx]!=NULL)
		{
/*
.....can't delete this button now since it need handle the message event (in the message loop)
.....so post message to delete this button
*/
//			delete m_sec_win[indx];
////
			m_sec_win[indx]->ShowWindow(SW_HIDE);
			m_delete_secbut = m_sec_win[indx];
			m_sec_win[indx] = NULL;
			PostMessage(ID_FORM_DELSEC_BUT, (WPARAM)NULL, (LPARAM)NULL);
			m_del_id = m_sec_id[indx];
		}
		for (i=indx; i<m_secno-1; i++)
		{
			m_sec_win[i] = m_sec_win[i+1];
			m_sec_id[i] = m_sec_id[i+1];
			((CNCLSecButton*)m_sec_win[i])->set_itemnum(i);
			strcpy(m_sec_name[i], m_sec_name[i+1]);
			strcpy(m_sec_color[i], m_sec_name[i+1]);
			if (m_sec_win[i]!=NULL)
				m_right_form->ReplaceSecNo(i+1, i);
		}
		if (m_secno>0)
		{
			m_secno--;
			m_sec_win[m_secno] = NULL;
		}
		else
			return;
/*
......reposition section button
*/
		CRect rect, rect_b;
		int chrwid, phgt;
		uw_ntget_strsize("XX", 80, "MS Sans Serif", &chrwid, &phgt);
		if (m_box1!=NULL)
		{
			m_box1->GetWindowRect(&rect_b);
			ScreenToClient(&rect_b);
		}
		for (i=indx; i<m_secno; i++)
		{
			rect.left = rect_b.left + 2;
			rect.right = rect_b.right-4;
			rect.top = rect_b.top+ (phgt+2) + i * (phgt+15+2);
			rect.bottom = rect.top + (phgt+15);
			m_sec_win[i]->MoveWindow(&rect,1);
		}
/*
......reposition 'ALL' button
*/
		rect.left = rect_b.left + 2;
		rect.right = rect_b.right-4;
		rect.top = rect_b.top+ (phgt+2) + i * (phgt+15+2);
		rect.bottom = rect.top + (phgt+15);
		m_sec_win[50]->MoveWindow(&rect,1);
	}
	m_right_form->SetsecNum(m_secno);
	int page = 0;
	if (sep==0)
	{
		if (indx<m_secno)
		{
			while ((((CNCLSecButton*)m_sec_win[indx])->GetType()==3)
				&&(indx>0)) indx--;
			page = ((CNCLSecButton*)m_sec_win[indx])->GetPageNum();
			OnSectionSelItem(page);
		}
		else
		{
			OnSectionSelItem(0);
		}
	}
	CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
	pview->SetFormSecNo(m_secno);
}
void CNCLFormView::OnInsertSec(int itemnum)
{
	int i;

	CRect rect, rect_b;
	int chrwid, phgt;
	uw_ntget_strsize("XX", 80, "MS Sans Serif", &chrwid, &phgt);
	if (m_box1!=NULL)
	{
		m_box1->GetWindowRect(&rect_b);
		ScreenToClient(&rect_b);
	}
	m_secno++;
	if ((itemnum==50)||(itemnum==-1))
	{
		itemnum = m_secno - 1;
	}
	else
	{
		for (i=m_secno-1; i>itemnum; i--)
		{
			m_sec_win[i] = m_sec_win[i-1];
			m_sec_id[i] = m_sec_id[i-1];
			((CNCLSecButton*)m_sec_win[i])->set_itemnum(i);
			strcpy(m_sec_name[i], m_sec_name[i-1]);
			strcpy(m_sec_color[i], m_sec_name[i-1]);
			rect.left = rect_b.left + 2;
			rect.right = rect_b.right-4;
			rect.top = rect_b.top+ (phgt+2) + i * (phgt+15+2);
			rect.bottom = rect.top + (phgt+15);
			if (m_sec_win[i]!=NULL)
			{
				m_sec_win[i]->MoveWindow(&rect,1);
				m_right_form->ReplaceSecNo(i-1, i);
			}
		}
	}
	rect.left = rect_b.left + 2;
	rect.right = rect_b.right-4; //need check the page number and seperator
	rect.top = rect_b.top+ (phgt+2) + itemnum * (phgt+15+2);
	rect.bottom = rect.top + (phgt+15);

	m_sec_win[itemnum] = new CNCLSecButton();
	((CNCLSecButton*)m_sec_win[itemnum])->SetParent(this);
	((CNCLSecButton*)m_sec_win[itemnum])->set_itemnum(itemnum);
	char name[40];
	strcpy(name, "new section");
	strcpy(m_sec_name[itemnum], name);
	strcpy(m_sec_color[itemnum], "Default");
	m_sec_id[itemnum] = get_valid_id();
	((CNCLSecButton*)m_sec_win[itemnum])->Create(name, 
					WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT | WS_TABSTOP | BS_OWNERDRAW, 
					rect, this, (UINT)m_sec_id[itemnum]);
	COLORREF tcolor = ::GetSysColor(COLOR_BTNTEXT);
	((CNCLSecButton*)m_sec_win[itemnum])->SetTColor(tcolor);
	((CNCLSecButton*)m_sec_win[itemnum])->ShowWindow(SW_SHOW);
	((CNCLSecButton*)m_sec_win[itemnum])->InitDrag();
/*
......reposition ALL button
*/
	rect.left = rect_b.left + 2;
	rect.right = rect_b.right-4;
	rect.top = rect_b.top+ (phgt+2) + m_secno * (phgt+15+2);
	rect.bottom = rect.top + (phgt+15);
	m_sec_win[50]->MoveWindow(&rect,1);
	m_right_form->SetsecNum(m_secno);
	OnSectionSelItem(itemnum);
	CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
	pview->SetFormSecNo(m_secno);
	m_right_form->AddUndo_addSection(itemnum);
}
void CNCLFormView::OnChangeSecProperty(int itemnum, int flag)
{
	COLORREF color;
	CString label;
	char *temp;
	m_selsec = itemnum;
	int indx = uw_get_rgb (m_sec_color[m_selsec], color);
/*
......if flag!=0, use the current color and label to update the section
*/
	if (flag==0)
	{
		CNCLSecProp *dlg;
		dlg  = new CNCLSecProp(UW_Dform_frame, "Section Property");
		dlg->SetTextString(m_sec_name[m_selsec]);
		dlg->SetTextColor(indx);

		int nResponse = dlg->DoModal();
		if (nResponse == IDOK)
		{
			dlg->GetTextString(label);
			int nc = label.GetLength();
			temp = label.GetBuffer(nc);
			strcpy(m_sec_name[m_selsec], temp);
			dlg->GetTextColor(&indx);
		}
		else
			return;
	}
	else
		label = m_sec_name[m_selsec];
	if (indx==-1)
	{
		if (flag==0)
			strcpy(m_sec_color[m_selsec], "Default");
		color = ::GetSysColor(COLOR_BTNTEXT);
	}
	else
	{
		if (flag==0)
			strcpy(m_sec_color[m_selsec], uw_color_name[indx]);
		color = RGB(uw_color_table[indx][0], 
					uw_color_table[indx][1], 
					uw_color_table[indx][2]);
	}
	((CNCLSecButton*)m_sec_win[m_selsec])->SetTColor(color);
	m_sec_win[m_selsec]->SetWindowText(label);
/*
......we need extend the section button len and resize the form
......if the new label is longer than the section length we have
*/
	CFont font;
	CClientDC dc(this);
	font.CreatePointFont(80, "MS Sans Serif");
	CFont* savfont = dc.SelectObject(&font );		
	CSize sizeText = dc.GetTextExtent(label, strlen(label));
	dc.SelectObject(&savfont);

	CRect rect, brect, rect_b;
	m_sec_win[m_selsec]->GetWindowRect(&brect);
	if (sizeText.cx+2!=brect.Width()-4)
	{
		int i, phgt, dx = sizeText.cx-brect.Width()+6;
		phgt = brect.Height() - 15;
/*
......resize box1 size and button size
*/
		CRect frect;
		m_box1->GetWindowRect(&frect);
		ScreenToClient(&frect);
		int old_right = frect.right;
		frect.right += dx;
//min width 110 (50+5 units)
		if (frect.Width()<=110)
		{
			frect.right = frect.left + 110;
			dx = frect.right - old_right;
		}
/*
.....resize the section area and buttons
*/
		m_box1->MoveWindow(&frect,1);
		m_box1->ShowWindow(SW_SHOW);
		m_box1->RedrawWindow();
		rect_b = frect;
/*
.....move and size box2
*/
		m_box2->GetWindowRect(&frect);
		ScreenToClient(&frect);
		frect.left += dx;
		frect.right += dx;
		m_box2->MoveWindow(&frect,1);
		m_box2->ShowWindow(SW_SHOW);
		m_box2->RedrawWindow();

		for (i=0; i<m_secno; i++)
		{
			rect.left = rect_b.left + 2;
			rect.right = rect_b.right-4;
			rect.top = rect_b.top+ (phgt+2) + i * (phgt+15+2);
			rect.bottom = rect.top + (phgt+15);
			m_sec_win[i]->MoveWindow(rect);
			m_sec_win[i]->ShowWindow(SW_SHOW);
		}
		rect.left = rect_b.left + 2;
		rect.right = rect_b.right-4;
		rect.top = rect_b.top+ (phgt+2) + i * (phgt+15+2);
		rect.bottom = rect.top + (phgt+15);
		m_sec_win[50]->MoveWindow(rect);
		m_sec_win[50]->ShowWindow(SW_SHOW);
		((CNCLFdsnFrame*) (UW_Dform_frame))->Resize_form_frame(dx, 0);
	}
}

void CNCLFormView::OnChangeSecColor(int itemnum)
{
	GdiplusStartupInput gdiplusStartupInput;
	ULONG_PTR gdiplusToken;
	char defstr[64];

	GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);

	CNCLColorDlg dlg;
	dlg.SetColorIndex(0);
	defstr[0] = '\0';
	dlg.SetColorDefault(2, defstr);
	int nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
		int indx = dlg.m_current_color;
		COLORREF color;
		if (indx!=-1)
			color = RGB (uw_color_table[indx][0],
						uw_color_table[indx][1],
						uw_color_table[indx][2]);
		else
			color = ::GetSysColor(COLOR_BTNTEXT);
		strcpy(m_sec_color[m_selsec], uw_color_name[indx]);
		((CNCLSecButton*)m_sec_win[m_secno])->SetTColor(color);
		((CNCLSecButton*)m_sec_win[m_secno])->ShowWindow(SW_SHOW);
	}
	else if (nResponse == IDCANCEL)
	{
	}
	GdiplusShutdown(gdiplusToken);
}


/***********************************************************************
c
c   FUNCTION: PreTranslateMessage(MSG* pMsg) 
c
c       translate window messages before they are dispatch
c		we don't want to display Combo box dropdown box
c
c   INPUT:  pMsg   Points to a MSG structure that contains the 
c					message to process.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNCLFormView::PreTranslateMessage(MSG* pMsg)
{
	ASSERT(pMsg != NULL);
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);
	if (pMsg->message == CB_SHOWDROPDOWN)
		return TRUE;

	if (CView::PreTranslateMessage(pMsg))
		return TRUE;

	CWnd* pParentWnd = GetParent();
	while (pParentWnd != NULL)
	{
		if ((pParentWnd->IsWindowEnabled())&&(pParentWnd->IsFrameWnd()))
		{
			if ((CFrameWnd*)pParentWnd->PreTranslateMessage(pMsg))
				return TRUE;
		}
		pParentWnd = pParentWnd->GetParent();
	}

	if (::GetWindow(m_hWnd, GW_CHILD) == NULL)
		return FALSE;

	return PreTranslateInput(pMsg);
}

void CNCLFormView::UpdatePropertySize(int cx, int cy)
{
	((CNCLFormMView*)m_parent_dlg)->UpdatePropertySize(cx, cy);
}

void CNCLFormView::UpdatePropertyPos(int cx, int cy)
{
	((CNCLFormMView*)m_parent_dlg)->UpdatePropertyPos(cx, cy);
}

void CNCLFormView::UpdatePropertyHSPTSize(CRect sizerec, float prect[4])
{
	((CNCLFormMView*)m_parent_dlg)->UpdatePropertyHSPTSize(sizerec, prect);
}


/***********************************************************************
**
**   FUNCTION: OpenPropertyPage(CNCLFormProp *prop_dlg, int flag)
**
**		open current property page window and initial the value with input prop_dlg
**		structure
**	 INPUT:  prop_dlg: property page data structure
**			flag: when = 1, only reset label, font and active value
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLFormView::OpenPropertyPage(CNCLFormProp *prop_dlg, int flag)
{
	((CNCLFormMView*)m_parent_dlg)->OpenPropertyPage(prop_dlg, flag);
}

/***********************************************************************
c
c   SUBROUTINE:  OnUndo()
c   FUNCTION:  This function called when undo function is called
c				it will undo the last delete action
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormView::OnUndo()
{
	if (m_right_form!=NULL)
		m_right_form->UndoItem();
}

/***********************************************************************
c
c   SUBROUTINE:  OnRedo()
c   FUNCTION:  This function called when undo function is called
c				it will undo the last delete action
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormView::OnRedo()
{
	if (m_right_form!=NULL)
		m_right_form->RedoItem();
}

BOOL CNCLFormView::PreCreateWindow(CREATESTRUCT& cs)
{
	BOOL bPreCreated = CFormView::PreCreateWindow(cs);
	return bPreCreated;
}
/***********************************************************************
c
c   SUBROUTINE:  CreateForm()
c   FUNCTION:  Create the design form inside this view
c				
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormView::CreateForm()
{
	long test = GetDialogBaseUnits();
	int unitx = LOWORD(test);
	int unity = HIWORD(test);
//		cx = (cx_w*4)/baseunitX;
//		cy = (cy_w*8)/baseunitY;				

	CRect rect;
	GetClientRect(&m_rect);
	rect.top = m_rect.top-1;
	rect.left = m_rect.left-1;
	rect.bottom = m_rect.bottom+1;
	rect.right = m_rect.right+1;
/*
.....create a window using unit and show it 
/*
......leave 25 unit for active bar area height
*/
	if (m_parent_dlg!=NULL)
	{
		CRect temp2 (0,0, 20, 20);
//		((CDialog*)m_parent_dlg)->MapDialogRect(&temp2);
//		uw_MapDialogRect(temp2);
		uw_MapDialogRect(temp2, ((CDialog*)m_parent_dlg));
		rect.top = m_rect.top;
		rect.bottom = m_rect.bottom - temp2.Height();
		rect.left = m_rect.left;
		rect.right = m_rect.right;
	}
	m_right_form = new CNCLDDform(this);
	if ((m_type)||(m_sec_flag==0)) /*dockable */
		m_right_form->Create(NULL, NULL, WS_CHILD|WS_VISIBLE|WS_TABSTOP | WS_BORDER, rect, this, IDC_STATIC);
	else /*floating */
		m_right_form->Create(NULL, NULL, WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this, IDC_STATIC);

	m_right_form->ShowWindow(SW_SHOW);
	if ((m_type==0)&&(m_sec_flag))
		CreateSectionButton(rect);
	m_right_form->InitDrag();
	m_right_form->SetsecNum(m_secno);
	SizeForm(m_type);
	m_formchanged = 0;
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
void CNCLFormView::OnDestroy() 
{
	if(m_right_form)
	{
		delete m_right_form;
		m_right_form = NULL;
	}
	if (m_prop_dlg!=NULL)
	{
		delete m_prop_dlg;
		m_prop_dlg = NULL;
	}
	CView::OnDestroy();	
}

/***********************************************************************
c
c   SUBROUTINE:  SizeForm(int type)
c   FUNCTION:  Size the form
c				
c   INPUT:  type: form type 0: float form
c					1: dockable form
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormView::SizeForm(int type)
{
/*
.....if it is floating form
.....only size bigger for view, if smaller, don't resize
.....this size is only for view purpose
*/
	int move = 0;
	CRect rect, rect1, rect2, temprect;
	temprect = m_rect;
	GetClientRect(&rect);

	if (type==0)
	{
		move = 1;
		if (m_view_reset)
		{
			temprect = m_viewrect;
			m_view_reset = 0;
		}
		else
			temprect = rect;
		if (move)
		{
			m_rect = temprect;
		}
	}
	else
		m_rect = rect;
/*
......leave 20 unit for active bar area height
*/
	if (m_parent_dlg!=NULL)
	{
		CRect temp1 (0,0, 20, 20);
		CRect temp2 (0,0, 25, 25);
//		uw_MapDialogRect(temp1);
//		uw_MapDialogRect(temp2);
		uw_MapDialogRect(temp1, ((CDialog*)m_parent_dlg));
		uw_MapDialogRect(temp2, ((CDialog*)m_parent_dlg));
		if (type==0)
		{
			rect.top = m_rect.top;
			if (m_sec_flag==0)
				rect.bottom = m_rect.bottom - temp2.Height();
			else
				rect.bottom = m_rect.bottom - temp1.Height();
			rect.left = m_rect.left;
			rect.right = m_rect.right;
		}
		else
		{
			rect = m_rect;
		}
		if (m_right_form!=NULL)
		{
			int chrwid, phgt;
			uw_ntget_strsize("XX", 80, "MS Sans Serif", &chrwid, &phgt);
			if ((m_box1!=NULL)&&(m_type==0)&&(m_sec_flag==1))
			{
				m_box1->GetWindowRect(&rect1);
				ScreenToClient(&rect1);
				rect1.bottom = rect1.top + rect.Height();
				m_box1->MoveWindow(&rect1,1);	
				m_box2->GetWindowRect(&rect2);
				ScreenToClient(&rect2);
				rect2.bottom = rect2.top + rect.Height();
				m_box2->MoveWindow(&rect2,1);
				rect2.top = rect2.top + 8;
				m_right_form->MoveWindow(&rect2,1);
			}
			else if (m_type==0)
			{
				rect1.top = m_rect.top + 6 + 8;
				rect1.left = m_rect.left + 10;
				rect1.right = m_rect.right - 10;
				rect1.bottom = rect1.top + rect.Height();
				m_right_form->MoveWindow(&rect1,1);
			}
			else
			{
				rect1.top = m_rect.top + 5 ;
				rect1.bottom = m_rect.bottom - 5 - ((CNCLFdsnFrame*)UW_Dform_frame)->m_dely;
				rect1.left = m_rect.left + 5;
				rect1.right = m_rect.right - 5 - ((CNCLFdsnFrame*)UW_Dform_frame)->m_delx;
				((CNCLFdsnFrame*)UW_Dform_frame)->m_delx = 0;
				((CNCLFdsnFrame*)UW_Dform_frame)->m_dely = 0;
				m_right_form->MoveWindow(&rect1,1);
			}
		}
	}
	if (m_init==0)
	{
		SIZE sizeTotal;
		sizeTotal.cx = 100;
		sizeTotal.cy = 100;
		SetScrollSizes(MM_TEXT, sizeTotal);
		m_viewrect = m_rect;
	}
}
/***********************************************************************
c
c   FUNCTION: OnInitialUpdate()
c
c       Initial view value here
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormView::OnInitialUpdate()
{
	ASSERT_VALID(this);
	if (!UpdateData(FALSE))
		TRACE(traceAppMsg, 0, "UpdateData failed during formview initial update.\n");
	CRect rect;
	GetClientRect(&rect);
	CFormView::OnInitialUpdate();
	m_init = 1;
}

void CNCLFormView::OnSize(UINT nType, int cx, int cy) 
{
	CFormView::OnSize(nType, cx, cy);
	SizeForm(m_type);
	m_formchanged = 1;
}

BOOL CNCLFormView::OnEraseBkgnd(CDC* pDC) 
{
	return TRUE;
}


/***********************************************************************
**
**   FUNCTION: OnDraw(CDC* pDC)
**
**       this function to perform 
**		 drawing of the window
**
**   INPUT:  CDC* pDC: device context
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLFormView::OnDraw(CDC* pDC)
{
	int i, j, cx, cy, toggle = ( (CNCLFdsnFrame*) (UW_Dform_frame))->GetToggle();
	CRect vrect, rect;
	if ((toggle==1)||(toggle==3))
	{
		GetClientRect(&vrect);
		CClientDC dc(this);
		CPen newPen;
		newPen.CreatePen(PS_SOLID, 0, RGB(255,0,0));
		CPen *oldPen = dc.SelectObject(&newPen);
		cx = vrect.Width();
		cy = vrect.Height();
		for (i=0; i<=cx/10; i++)
		{
			for (j=0; j<=cy/10;j++)
			{
				rect.left = i*10;
				rect.right = rect.left;
				rect.top = j*10;
				rect.bottom = rect.top;

				dc.SetPixel(rect.left, rect.top, RGB(255,0,0));
			}
		}
		dc.SelectObject(oldPen);
		newPen.DeleteObject();
	}
}

int CNCLFormView::GetToggle()
{		
	return ( (CNCLFdsnFrame*) (UW_Dform_frame))->GetToggle();
}

void CNCLFormView::CreateSectionButton(CRect rect)
{
	char name[40];
	int chrwid, phgt;
	int i, start_x = 0;
/*
......this buttion will be created when initial dialog form
*/
	int widlabel = 0;
	widlabel = 50;

	CRect rect1, rect2, rect_b;
		
	start_x = widlabel + 10;
	CRect rect0(0, 0, start_x-5, 2);

//	uw_MapDialogRect(rect0);
	uw_MapDialogRect(rect0, ((CDialog*)m_parent_dlg));

	rect1.top = rect.top;
	rect1.left = rect.left + 8;
	rect1.right = rect1.left + rect0.Width();
	rect1.bottom = rect.bottom - 8;

	m_box1 = new CButton();
	m_box1->Create("", WS_VISIBLE | WS_CHILD | WS_GROUP | BS_GROUPBOX, 
				rect1, this, IDC_DLG_BOX1);		

	rect2 = rect;
	rect0.left = start_x;
	rect0.top = 0;
	rect0.right = rect.Width();
	rect0.bottom = rect.Height();
//	((CDialog*)m_parent_dlg)->MapDialogRect(&rect0);
//	uw_MapDialogRect(rect0);
	uw_MapDialogRect(rect0, ((CDialog*)m_parent_dlg));
	rect2.top = rect.top;
	rect2.left = rect0.left + rect.left + 8;
	rect2.right = rect2.left + rect.Width();
	rect2.bottom = rect.bottom;
	m_box2 = new CButton();
	m_box2->Create("", WS_VISIBLE | WS_CHILD | WS_GROUP | BS_GROUPBOX, 
				rect2, this, IDC_DLG_BOX2);
	rect2.top = rect2.top + 8;
	m_right_form->MoveWindow(&rect2,1);

	rect_b = rect1;
	uw_ntget_strsize("XX", 80, "MS Sans Serif", &chrwid, &phgt);
	m_secno = 0;
	COLORREF tcolor = ::GetSysColor(COLOR_BTNTEXT);
	for (i=0; i<1; i++)
	{
		rect.left = rect_b.left + 2;
		rect.right = rect_b.right-4;
		rect.top = rect_b.top+ (phgt+2) + i * (phgt+15+2);
		rect.bottom = rect.top + (phgt+15);

		m_sec_win[m_secno] = new CNCLSecButton();
		((CNCLSecButton*)m_sec_win[m_secno])->SetParent(this);
		((CNCLSecButton*)m_sec_win[m_secno])->set_itemnum(m_secno);
		((CNCLSecButton*)m_sec_win[m_secno])->SetType(1);
		((CNCLSecButton*)m_sec_win[m_secno])->SetPageNum(i);
		sprintf(name, "Section %d", i+1);
		((CNCLSecButton*)m_sec_win[m_secno])->Create(name, 
				WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT | WS_TABSTOP | BS_OWNERDRAW, 
				rect, this, (UINT)(IDC_FORMDESGN_SEC + m_secno));
		strcpy(m_sec_name[m_secno], name);
		m_sec_id[m_secno] = IDC_FORMDESGN_SEC + m_secno;
		((CNCLSecButton*)m_sec_win[m_secno])->SetTColor(tcolor);
		((CNCLSecButton*)m_sec_win[m_secno])->ShowWindow(SW_SHOW);
		((CNCLSecButton*)m_sec_win[m_secno])->InitDrag();
		m_secno++;
	}
/*
.....add button for all section
*/
	rect.left = rect_b.left + 2;
	rect.right = rect_b.right-4;
	rect.top = rect_b.top+ (phgt+2) + i * (phgt+15+2);
	rect.bottom = rect.top + (phgt+15);

	m_sec_win[50] = new CNCLSecButton();
	((CNCLSecButton*)m_sec_win[50])->SetParent(this);
	((CNCLSecButton*)m_sec_win[50])->set_itemnum(50);
	((CNCLSecButton*)m_sec_win[50])->SetType(1);
	((CNCLSecButton*)m_sec_win[50])->SetPageNum(-1);
	((CNCLSecButton*)m_sec_win[50])->Create("ALL", 
				WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT | WS_TABSTOP | BS_OWNERDRAW, 
				rect, this, (UINT)(IDC_FORMDESGN_SEC + 50));
	((CNCLSecButton*)m_sec_win[50])->SetTColor(tcolor);
	((CNCLSecButton*)m_sec_win[50])->ShowWindow(SW_SHOW);
	((CNCLSecButton*)m_sec_win[50])->InitDrag();
	m_right_form->SetsecNum(m_secno);
	if (m_secno>0)
	{
		OnSectionSelItem(0);
		if (UW_Dform_frame!=NULL)
		{
			CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
			pview->SetFormSecNo(m_secno);
		}
	}
}
void CNCLFormView::OnMouseMove(UINT nFlags, CPoint point) 
{		
	CFormView::OnMouseMove(nFlags, point);

	if (m_type!=0)
		return;
	CRect rect;
/*
......else erase the high light of current button
*/
	for(int i = 0; i < m_secno; i++ )
	{
		m_sec_win[i]->GetWindowRect(&rect);
		ScreenToClient(rect);
		if (rect.PtInRect(point))
			return;
		if (((CNCLSecButton*)m_sec_win[i])->m_focus==1)
		{
			((CNCLSecButton*)m_sec_win[i])->DrawNormal2();
			((CNCLSecButton*)m_sec_win[i])->m_focus = 0;
			((CNCLSecButton*)m_sec_win[i])->CloseToolTip();
		}
	}
	if (m_sec_win[50]==NULL)
		return;
	m_sec_win[50]->GetWindowRect(&rect);
	ScreenToClient(rect);
	if (rect.PtInRect(point))
		return;
	if (((CNCLSecButton*)m_sec_win[50])->m_focus==1)
	{
		((CNCLSecButton*)m_sec_win[50])->DrawNormal2();
		((CNCLSecButton*)m_sec_win[50])->m_focus = 0;
		((CNCLSecButton*)m_sec_win[50])->CloseToolTip();
	}
}

void CNCLFormView::OnItemMouseMove(int itemnum)
{
/*
.....highlight this section item if not highlight
*/
	for(int i = 0; i < m_secno; i++ )
	{
		if (i==itemnum)
		{
			if (((CNCLSecButton*)m_sec_win[i])->m_focus==0)
			{
				((CNCLSecButton*)m_sec_win[i])->DrawHighLight2();
				((CNCLSecButton*)m_sec_win[i])->m_focus = 1;
			}
		}
		else
		{
			if (((CNCLSecButton*)m_sec_win[i])->m_focus==1)
			{
				((CNCLSecButton*)m_sec_win[i])->DrawNormal2();
				((CNCLSecButton*)m_sec_win[i])->m_focus = 0;
				((CNCLSecButton*)m_sec_win[i])->CloseToolTip();
			}
		}
	}
	if (itemnum==50)
	{
		if (((CNCLSecButton*)m_sec_win[50])->m_focus==0)
		{
			((CNCLSecButton*)m_sec_win[50])->DrawHighLight2();
			((CNCLSecButton*)m_sec_win[50])->m_focus = 1;
		}
	}
	else
	{
		if (((CNCLSecButton*)m_sec_win[50])->m_focus==1)
		{
			((CNCLSecButton*)m_sec_win[50])->DrawNormal2();
			((CNCLSecButton*)m_sec_win[50])->m_focus = 0;
			((CNCLSecButton*)m_sec_win[50])->CloseToolTip();
		}
	}
}

void CNCLFormView::SetSection(int type)
{
	int visible_sec;
	if (m_box1!=NULL)
		visible_sec = m_box1->IsWindowVisible();
	else
		visible_sec = 0;
/*
.....only display type have section, not dockable type
*/
	if (type)
		m_type = 0;

	if (m_right_form!=NULL)
		m_right_form->m_frmtyp = m_type;
	if (m_init==0)
		return;
	int i, delx;
	CRect rect, rect1, rect2;
	
	delx = 0;
	if ((m_box1!=NULL)&&(m_box2!=NULL))
	{
		if ((m_box1->IsWindowVisible())&&(m_box2->IsWindowVisible()))
		{
			m_box1->GetWindowRect(&rect1);
			m_box2->GetWindowRect(&rect2);
			delx = rect2.left - rect1.left;
		}
	}
	if (type)
	{
/*
......there is no box and section create it, create it
*/
		if ((m_secno==0)&&(m_box1==NULL))
		{
			CRect temp2 (0,0, 20, 20);
//			((CDialog*)m_parent_dlg)->MapDialogRect(&temp2);
//			uw_MapDialogRect(temp2);
			uw_MapDialogRect(temp2, ((CDialog*)m_parent_dlg));

			rect.top = m_rect.top;
			rect.bottom = m_rect.bottom - temp2.Height();// - (2+8);
			rect.left = m_rect.left;
//temp 120 = 60 units of section button len
			rect.right = m_rect.right - 120 - 13 - 4;
			CreateSectionButton(&rect);
		}
		for (i=0; i<50; i++)
		{
			if (m_sec_win[i]!=NULL)
				m_sec_win[i]->ShowWindow(SW_SHOW);
			else
				break;
		}
		m_del_id = -1;
		if (m_sec_win[50]!=NULL)
			m_sec_win[50]->ShowWindow(SW_SHOW);
		m_secno = i;
		m_right_form->ModifyStyle(WS_BORDER, 0, 0);
	}
	else  //No section, hide the section button
	{
		for (i=0; i<50; i++)
		{
			if (m_sec_win[i]!=NULL)
				m_sec_win[i]->ShowWindow(SW_HIDE);
		}
		m_del_id = -1;
		if (m_sec_win[50]!=NULL)
			m_sec_win[50]->ShowWindow(SW_HIDE);
		m_secno = 0;
		m_right_form->ModifyStyle(0, WS_BORDER, 0);
	}
	m_sec_flag = type;

	if ((m_type!=0)&&(m_box1!=NULL))
		m_box1->ShowWindow(SW_HIDE);
	else if ((m_sec_flag==0)&&(m_box1!=NULL))
		m_box1->ShowWindow(SW_HIDE);
	else if ((m_sec_flag)&&(m_box1!=NULL))
		m_box1->ShowWindow(SW_SHOW);

	if ((m_type!=0)&&(m_box2!=NULL))
		m_box2->ShowWindow(SW_HIDE);
	else if ((m_sec_flag==0)&&(m_box2!=NULL))
		m_box2->ShowWindow(SW_HIDE);
	else if (m_box2!=NULL)
		m_box2->ShowWindow(SW_SHOW);
	if ((m_sec_flag)&&(visible_sec==0))
	{
		delx = 0;
		if ((m_box1!=NULL)&&(m_box2!=NULL))
		{
			if ((m_box1->IsWindowVisible())&&(m_box2->IsWindowVisible()))
			{
				m_box1->GetWindowRect(&rect1);
				m_box2->GetWindowRect(&rect2);
				delx = rect2.left - rect1.left;
			}
		}
	}
	if (m_sec_flag)
		OnSectionSelItem(m_selsec);
	else
		m_right_form->DisplayAll();
}
void CNCLFormView::SetType(int type)
{
	int visible_sec;
	if (m_box1!=NULL)
		visible_sec = m_box1->IsWindowVisible();
	else
		visible_sec = 0;
	m_type = type;
	if (m_right_form!=NULL)
		m_right_form->m_frmtyp = type;
	if (m_init==0)
		return;
	int i, delx;
	CRect rect, rect1, rect2;
	
	delx = 0;
	if ((m_box1!=NULL)&&(m_box2!=NULL))
	{
		if ((m_box1->IsWindowVisible())&&(m_box2->IsWindowVisible()))
		{
			m_box1->GetWindowRect(&rect1);
			m_box2->GetWindowRect(&rect2);
			delx = rect2.left - rect1.left;
		}
	}
	if ((m_sec_flag)&&(type==0)) //dockable Yes, hide the section button
	{
		for (i=0; i<50; i++)
		{
			if (m_sec_win[i]!=NULL)
				m_sec_win[i]->ShowWindow(SW_SHOW);
			else
				break;
		}
		m_del_id = -1;
		if (m_sec_win[50]!=NULL)
			m_sec_win[50]->ShowWindow(SW_SHOW);
		m_secno = i;
	}
	else
	{
		for (i=0; i<50; i++)
		{
			if (m_sec_win[i]!=NULL)
				m_sec_win[i]->ShowWindow(SW_HIDE);
		}
		m_del_id = -1;
		if (m_sec_win[50]!=NULL)
			m_sec_win[50]->ShowWindow(SW_HIDE);
		m_secno = 0;
		m_right_form->ModifyStyle(0, WS_BORDER, 0);
	}
	m_type = type;
	if (((m_type!=0)||(m_sec_flag==0))&&(m_box1!=NULL))
		m_box1->ShowWindow(SW_HIDE);
	else if ((m_type==0)&&(m_box1!=NULL)&&(m_sec_flag))
		m_box1->ShowWindow(SW_SHOW);

	if (((m_type!=0)||(m_sec_flag==0))&&(m_box2!=NULL))
		m_box2->ShowWindow(SW_HIDE);
	else if ((m_type==0)&&(m_box2!=NULL)&&(m_sec_flag))
		m_box2->ShowWindow(SW_SHOW);

	if ((visible_sec)&&((m_type!=0)||(m_sec_flag==0))&&(delx!=0))
	{
		((CNCLFdsnFrame*) (UW_Dform_frame))->MoveFormFrameRect(0,-delx, 0,0);
	}
	if ((m_sec_flag)&&(m_type==0)&&(m_secno==0))
	{
		CRect temp2 (0,0, 20, 20);
//		((CDialog*)m_parent_dlg)->MapDialogRect(&temp2);
//		uw_MapDialogRect(temp2);
		uw_MapDialogRect(temp2, ((CDialog*)m_parent_dlg));

		rect.top = m_rect.top;
		rect.bottom = m_rect.bottom - temp2.Height();//  - (2+8);
		rect.left = m_rect.left;
		rect.right = m_rect.right;

		CreateSectionButton(&rect);
		m_right_form->ModifyStyle(WS_BORDER, 0, 0);
	}
	if ((m_sec_flag)&&(m_type==0)&&(visible_sec==0))
	{
		delx = 0;
		if ((m_box1!=NULL)&&(m_box2!=NULL))
		{
			if ((m_box1->IsWindowVisible())&&(m_box2->IsWindowVisible()))
			{
				m_box1->GetWindowRect(&rect1);
				m_box2->GetWindowRect(&rect2);
				delx = rect2.left - rect1.left;
			}
		}
		if (delx!=0)
			((CNCLFdsnFrame*) (UW_Dform_frame))->MoveFormFrameRect(0,delx, 0,0);
	}
	if ((m_sec_flag)&&(m_type==0))
		OnSectionSelItem(m_selsec);
	else
		m_right_form->DisplayAll();
}
//secnum: page number, not index number page=indx-sepno
void CNCLFormView::OnDragDropSection(int secnum, char *drop_text)
{
	m_right_form->OnDragDropSection(secnum, drop_text);
}
/////////////////////////////////////////////////////////////////////////////
// CNCLFormView diagnostics

#ifdef _DEBUG
void CNCLFormView::AssertValid() const
{
	CFormView::AssertValid();
}


CNCLFormDoc* CNCLFormView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CNCLFormDoc)));
	return (CNCLFormDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CNCLFormView message handlers
