#if UU_COMP == UU_WIN2K
/************************************************************************
**		FILE NAME: wsntheaderctrl.cpp
**
**	 Description - Functions and implementations for
**		CNCLHeaderCtrl class (NCL table list header)
**		
**	 CONTAINS:
**		member function of CNCLHeaderCtrl
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntheaderctrl.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:27
**********************************************************************
*/
#include "wsntstdafx.h"
#include "wsntres.h"
#include "wsntmenudsndlg.h"
#include "wsntheaderctrl.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
/////////////////////////////////////////////////////////////////////////////
// CNCLHeaderCtrl

/***********************************************************************
c
c   SUBROUTINE:  CNCLHeaderCtrl
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLHeaderCtrl::CNCLHeaderCtrl()
{
	m_pWndEdit = new CEdit;
	VERIFY(m_pWndEdit != NULL);
	m_editting = -1;
	m_combo_open = 0;
	m_pWndCombo = new CComboBox;
	VERIFY(m_pWndCombo != NULL);
	m_pWndKType = new CComboBox;
	VERIFY(m_pWndKType != NULL);
	m_CombBand = NULL;
	m_pWndBand0 = NULL;
	m_pWndBand1 = NULL;
	m_pWndBand3 = NULL;
	m_first = 1;
}

CNCLHeaderCtrl::~CNCLHeaderCtrl()
{
	delete m_pWndCombo;
	delete m_pWndKType;
	delete m_CombBand;
	delete m_pWndBand0;
	delete m_pWndBand1;
	delete m_pWndBand3;
}


BEGIN_MESSAGE_MAP(CNCLHeaderCtrl, CHeaderCtrl)
	//{{AFX_MSG_MAP(CNCLHeaderCtrl)
	ON_WM_PAINT()
	ON_WM_ERASEBKGND()
	ON_WM_LBUTTONDOWN()
	ON_CONTROL(CBN_SELCHANGE, IDC_COMBOCTL, OnItemType)
	ON_CONTROL(CBN_SELCHANGE, IDC_KTYPECTL, OnKeyType)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDITCTL, OnELossFocus)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNCLHeaderCtrl message handlers

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
/*
.....not used now
*/
void CNCLHeaderCtrl::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	CHeaderCtrl::DrawItem(lpDrawItemStruct);
}

/***********************************************************************
**
**   FUNCTION: OnItemType()
**
**         callback function for "Menu type" choice filter
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLHeaderCtrl::OnItemType()
{
	int type = m_pWndCombo->GetCurSel();
	((CNCLMenuDsnDlg*)m_parent)->ChangeMenuType(type);
}

/***********************************************************************
**
**   FUNCTION: OnKeyType()
**
**         callback function for "Key/menu type" choice box (second choice box)
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLHeaderCtrl::OnKeyType()
{
	int type = m_pWndKType->GetCurSel();
	((CNCLMenuDsnDlg*)m_parent)->ChangeKeyType(type);
}
/***********************************************************************
**
**   FUNCTION: OnELossFocus()
**
**         callback function for edit field (filter text input) loss the focus
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLHeaderCtrl::OnELossFocus()
{
	EndEdit();
}
/***********************************************************************
**
**   FUNCTION: OnHandleReturn(char *filter, int &subItem)
**
**         function for handle the return key (get the filter input)
**
**   INPUT:  filter:
**
**   OUTPUT :   filter: filter string
**				subItem: filter item number
**   RETURN:    1: filter changed
**
**********************************************************************/
int CNCLHeaderCtrl::OnHandleReturn(char *filter, int &subItem)
{
	CString str;
	int bChanged;
	if (m_pWndEdit->GetSafeHwnd() == NULL)
		return 0;
	m_pWndEdit->GetWindowText(str);
	m_pWndEdit->GetWindowText(filter, UX_MAX_FILE_LEN);
	subItem = m_editting;
	bChanged = EndEdit();
	return bChanged;
}
/***********************************************************************
**
**   FUNCTION: HideFiltEdit()
**
**        hide the filter editor window
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLHeaderCtrl::HideFiltEdit()
{
	if (m_pWndEdit->GetSafeHwnd() != NULL)
		m_pWndEdit->ShowWindow(SW_HIDE);
}
/***********************************************************************
**
**   FUNCTION: HideFilterArea()
**
**        hide the filter area
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLHeaderCtrl::HideFilterArea()
{
	if ((m_pWndEdit!=NULL) && (m_pWndEdit->GetSafeHwnd()!=NULL))
		m_pWndEdit->ShowWindow(SW_HIDE);
	if ((m_pWndKType!=NULL) && (m_pWndKType->GetSafeHwnd()!=NULL))
		m_pWndKType->ShowWindow(SW_HIDE);
	if ((m_pWndBand0!=NULL) && (m_pWndBand0->GetSafeHwnd()!=NULL))
		m_pWndBand0->ShowWindow(SW_HIDE);
	if ((m_pWndBand1!=NULL) && (m_pWndBand1->GetSafeHwnd()!=NULL))
		m_pWndBand1->ShowWindow(SW_HIDE);
	if ((m_pWndBand3!=NULL) && (m_pWndBand3->GetSafeHwnd()!=NULL))
		m_pWndBand3->ShowWindow(SW_HIDE);
	m_editting = -1;
	m_combo_open = 0;
}	

/***********************************************************************
**
**   FUNCTION: AddFilterArea
**
**        add the filter area into the header
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLHeaderCtrl::AddFilterArea(int flag, int type, CWnd *parent, char *filter1,
								char *filter2, char *filter3 )
{
	if (flag==0)
		return;
	m_parent = parent;
/*
.....create the filter static area
*/
	CRect rcRect, rcRect1, rcRect2, rcRect3, rcRect4, rcRect5;
	GetItemRect(0, &rcRect1);
	GetItemRect(1, &rcRect2);
	GetItemRect(2, &rcRect3);
	GetItemRect(3, &rcRect4);
	GetItemRect(4, &rcRect5);

	rcRect = rcRect1;
	rcRect.top = rcRect.bottom/2;
	m_comborc = rcRect;

	if (type==3)
	{
		if (m_pWndBand0==NULL)
		{
			m_pWndBand0 = new CStatic;
			m_pWndBand0->Create(" ", WS_CHILD|WS_VISIBLE|WS_BORDER|SS_SUNKEN,CRect(0, 0, 1, 1), this, -1);
		}
		rcRect = rcRect2;
		rcRect.top = rcRect.bottom/2 + 1;
		m_pWndBand0->MoveWindow(&rcRect);
		m_pWndBand0->ShowWindow(SW_SHOW);
		m_bandrec0 = rcRect;
		m_pWndBand0->SetFont(GetFont());
		m_pWndBand0->SetWindowText(" Single Bar");
		return;
	}
	if (m_pWndBand0==NULL)
	{
		m_pWndBand0 = new CStatic;
		m_pWndBand0->Create(" ", WS_CHILD|WS_VISIBLE|WS_BORDER|SS_SUNKEN,CRect(0, 0, 1, 1), this, -1);
	}
	rcRect = rcRect2;
	rcRect.top = rcRect.bottom/2;
	m_pWndBand0->MoveWindow(&rcRect);
	m_pWndBand0->SetFont(GetFont());
	m_pWndBand0->SetWindowText(" ");
	m_pWndBand0->ShowWindow(SW_SHOW);
	m_bandrec0 = rcRect;

	if (m_pWndBand1==NULL)
	{
		m_pWndBand1 = new CStatic;
		m_pWndBand1->Create(" ", WS_CHILD|WS_VISIBLE|WS_BORDER|SS_SUNKEN,CRect(0, 0, 1, 1), this, -1);
	}
	rcRect = rcRect3;
	rcRect.top = rcRect.bottom/2;
	m_pWndBand1->MoveWindow(&rcRect);
	m_pWndBand1->ShowWindow(SW_SHOW);
	m_bandrec1 = rcRect;

	rcRect = rcRect4;
	rcRect.top = rcRect.bottom/2;
	m_bandrec2 = rcRect;

	if (m_pWndBand3==NULL)
	{
		m_pWndBand3 = new CStatic;
		m_pWndBand3->Create(" ", WS_CHILD|WS_VISIBLE|WS_BORDER|SS_SUNKEN,CRect(0, 0, 1, 1), this, -1);
	}
	rcRect = rcRect5;
	rcRect.top = rcRect.bottom/2;
	m_pWndBand3->MoveWindow(&rcRect);
	m_pWndBand3->ShowWindow(SW_SHOW);
	m_bandrec3 = rcRect;
	m_pWndBand3->SetFont(GetFont());
	m_pWndBand1->SetFont(GetFont());
	m_pWndBand1->SetWindowText(filter1);
	m_pWndBand3->SetWindowText(filter3);
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
void CNCLHeaderCtrl::OnLButtonDown(UINT nFlags, CPoint point) 
{
	LVHITTESTINFO hti;
	hti.pt = point;

	int bandclick = -1;
	if (m_bandrec1.PtInRect(point))
		bandclick = 0;
	if (m_bandrec2.PtInRect(point))
		bandclick = 1;
	if (m_bandrec3.PtInRect(point))
		bandclick = 2;
	if (m_comborc.PtInRect(point))
		bandclick = 3;
	OnHandleMouseclick(WM_LBUTTONDOWN, nFlags, point, TRUE, bandclick);
}

/***********************************************************************
c
c   FUNCTION: OnLButtonDblClk(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user double click the left mouse button.
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
void CNCLHeaderCtrl::OnLButtonDblClk(UINT nFlags, CPoint point) 
{
	OnHandleMouseclick(WM_LBUTTONDBLCLK, nFlags, point, TRUE);
}

/***********************************************************************
c
c   FUNCTION: OnMButtonDown(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user click the middle mouse button.
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
void CNCLHeaderCtrl::OnMButtonDown(UINT nFlags, CPoint point) 
{
	OnHandleMouseclick(WM_MBUTTONDOWN, nFlags, point, FALSE);
}

/***********************************************************************
c
c   FUNCTION: OnMButtonDblClk(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user double click the middle mouse button.
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
void CNCLHeaderCtrl::OnMButtonDblClk(UINT nFlags, CPoint point) 
{
	OnHandleMouseclick(WM_MBUTTONDBLCLK, nFlags, point, FALSE);
}

/***********************************************************************
c
c   FUNCTION: OnRButtonDown(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user click the right mouse button.
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
void CNCLHeaderCtrl::OnRButtonDown(UINT nFlags, CPoint point) 
{
	OnHandleMouseclick(WM_RBUTTONDOWN, nFlags, point, FALSE);
}

/***********************************************************************
c
c   FUNCTION: OnRButtonDblClk(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user double click the right mouse button.
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
void CNCLHeaderCtrl::OnRButtonDblClk(UINT nFlags, CPoint point) 
{
	OnHandleMouseclick(WM_RBUTTONDBLCLK, nFlags, point, FALSE);
}

/***********************************************************************
c
c   FUNCTION: OnHandleMouseclick
c
c       Handle all mouse click function here
c
c   INPUT:  
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLHeaderCtrl::OnHandleMouseclick(UINT nMsg, UINT nFlags, CPoint point, BOOL bTriggerEdit, int bandclick)
{
	if (bandclick==-1)
		bTriggerEdit = FALSE;

	EndEdit();
	switch (nMsg)
	{
	case WM_LBUTTONDOWN:
		CHeaderCtrl::OnLButtonDown(nFlags, point);
		break;

	case WM_LBUTTONDBLCLK:
		CHeaderCtrl::OnLButtonDblClk(nFlags, point);
		break;

	case WM_MBUTTONDOWN:
		CHeaderCtrl::OnMButtonDown(nFlags, point);
		break;

	case WM_MBUTTONDBLCLK:
		CHeaderCtrl::OnMButtonDblClk(nFlags, point);
		break;

	case WM_RBUTTONDOWN:
		CHeaderCtrl::OnRButtonDown(nFlags, point);
		break;

	case WM_RBUTTONDBLCLK:
		CHeaderCtrl::OnRButtonDblClk(nFlags, point);
		break;

	default:
		break;		
	}
	if (m_pWndCombo->GetCurSel()==3)
		return;
	if ((bTriggerEdit)&&(bandclick!=3))
	{
		StartEdit(bandclick);
		return;
	}
}

/***********************************************************************
c
c   FUNCTION: AddChoiceArea
c
c       added 2 combo box for menu type and key selection
c
c   INPUT:  
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLHeaderCtrl::AddChoiceArea(int flag, int menu_type, CWnd *parent, char *key_string)
{
	if (flag==0)
		return;
	m_combo_open = 1;
	if (m_pWndCombo->GetSafeHwnd() == NULL
		&& !m_pWndCombo->Create( WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL, CRect(0, 0, 1, 1), this, IDC_COMBOCTL))
	{
		return;
	}
	m_pWndCombo->SetFont(GetFont());

	CRect rcCombo;
	CString text;
		
	rcCombo = m_comborc;
	rcCombo.bottom = rcCombo.bottom + 60;
	m_pWndCombo->MoveWindow(&rcCombo);
	if (m_first)
	{
		m_pWndCombo->AddString("Choice Menu");
		m_pWndCombo->AddString("Menu");
		m_pWndCombo->AddString("Function");
		m_pWndCombo->AddString("Separator");
	}
	m_pWndCombo->SetCurSel(menu_type);
	m_pWndCombo->SetFont(GetFont());
	m_pWndCombo->ShowWindow(SW_SHOW);

	if (menu_type==3)
	{
		m_first = 0;
		return;
	}
	if (m_pWndKType->GetSafeHwnd() == NULL
		&& !m_pWndKType->Create( WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL, CRect(0, 0, 1, 1), this, IDC_KTYPECTL))
	{
		return;
	}
	m_pWndKType->SetFont(GetFont());
		
	rcCombo = m_bandrec2;
	rcCombo.bottom = rcCombo.bottom + 100;
	m_pWndKType->MoveWindow(&rcCombo);
/*
.....delete old input
*/
	int defchc = 0;
	int cont = m_pWndKType->GetCount();
	if (m_first==0)
	{
		for (int j = 0; j<cont; j++)
		{
			m_pWndKType->DeleteString(0);
		}
	}
	if ((menu_type==0)||(menu_type==1))
	{
		m_pWndKType->AddString("PULLDOWN");
		m_pWndKType->AddString("POPUP");
		m_pWndKType->AddString("DOCKED");
		m_pWndKType->AddString("FLOATING");
		m_pWndKType->AddString("ALL");
		if (stricmp(key_string, "PULLDOWN")==0)
			defchc = 0;
		else if (stricmp(key_string, "POPUP")==0)
			defchc = 1;
		else if (stricmp(key_string, "DOCKED")==0)
			defchc = 2;
		else if (stricmp(key_string, "FLOATING")==0)
			defchc = 3;
		else
			defchc = 4;
	}
	else if (menu_type==2)
	{
		m_pWndKType->AddString("DASKEY");
		m_pWndKType->AddString("NISKEY");
		m_pWndKType->AddString("NCLKEY");
		m_pWndKType->AddString("CAMKEY");
		m_pWndKType->AddString("CADKEY");
		m_pWndKType->AddString("IPVKEY");
		m_pWndKType->AddString("ALL");
		if (stricmp(key_string, "DASKEY")==0)
			defchc = 0;
		else if (stricmp(key_string, "NISKEY")==0)
			defchc = 1;
		else if (stricmp(key_string, "NCLKEY")==0)
			defchc = 2;
		else if (stricmp(key_string, "CAMKEY")==0)
			defchc = 3;
		else if (stricmp(key_string, "CADKEY")==0)
			defchc = 4;
		else if (stricmp(key_string, "IPVKEY")==0)
			defchc = 5;
		else
			defchc = 6;
	}
	m_pWndKType->SetCurSel(defchc);
	m_pWndKType->ShowWindow(SW_SHOW);
	if (menu_type==3)
		m_pWndKType->EnableWindow(0);
	else
		m_pWndKType->EnableWindow(1);
	RedrawWindow();
	UpdateWindow();
	m_first = 0;
}


/***********************************************************************
c
c   FUNCTION: StartEdit(int nSubItem)
c
c       start the filter editor input
c
c   INPUT:  nSubItem: which sub item use clicked
c
c   OUTPUT :   None
c   RETURN:    true, start it
c
**********************************************************************/
BOOL CNCLHeaderCtrl::StartEdit(int nSubItem)
{
	if (m_pWndCombo->GetCurSel()==3)
		return 0;
	if ( nSubItem!=0 && nSubItem!=2)
		return FALSE;
	
	if (m_editting == nSubItem)
		return TRUE;

	EndEdit();
	m_editting = nSubItem;
		
	if (m_pWndEdit->GetSafeHwnd() == NULL
		&& !m_pWndEdit->Create(ES_AUTOHSCROLL | ES_NOHIDESEL | WS_CHILD | WS_BORDER | ES_LEFT, CRect(0, 0, 1, 1), this, IDC_EDITCTL))
	{
		return FALSE;
	}
	m_pWndEdit->SetFont(GetFont());

	CRect rcEdit;
	CString text;
	if (nSubItem==0)
	{
		rcEdit = m_bandrec1;
		m_pWndBand1->GetWindowText(text);
	}
	else if (nSubItem==2)
	{
		rcEdit = m_bandrec3;
		m_pWndBand3->GetWindowText(text);
	}
	m_pWndEdit->MoveWindow(&rcEdit);
	m_pWndEdit->SetWindowText(text);
	m_pWndEdit->ShowWindow(SW_SHOW);
	m_pWndEdit->SetSel(0, -1);
	m_pWndEdit->SetFocus();
	RedrawWindow();
	UpdateWindow();
	return TRUE;
}
/***********************************************************************
c
c   FUNCTION: EndEdit()
c
c       end the filter editor input
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    filter changed
c
**********************************************************************/
BOOL CNCLHeaderCtrl::EndEdit()
{
	CString str, str_old;
	char filter[UX_MAX_FILE_LEN];
	if (m_pWndEdit->GetSafeHwnd() == NULL)
		return 0;
	m_pWndEdit->GetWindowText(str);
	m_pWndEdit->GetWindowText(filter, UX_MAX_FILE_LEN);
	m_pWndEdit->ShowWindow(SW_HIDE);
	
	if (m_editting==0)
	{
		m_pWndBand1->GetWindowText(str_old);
		m_pWndBand1->SetWindowText(str);
	}
	else if (m_editting==2)
	{
		m_pWndBand3->GetWindowText(str_old);
		m_pWndBand3->SetWindowText(str);
	}
	m_pWndEdit->ShowWindow(SW_HIDE);
	int change = str.Compare(str_old); 
	if ((change)&&(m_editting!=-1))
	{
/*
.....filter changed, updated the list
*/
		((CNCLMenuDsnDlg*)m_parent)->ChangedFilter(m_editting, filter);
	}
	m_editting = -1;
	return change;
}
/***********************************************************************
c
c   FUNCTION: OnHeadItemEndTrack (NMHDR* pNMHDR, LRESULT* pResult) 
c
c       Callback function for end of adjusting header item
c
c   INPUT:  pNMHDR: data structure include header ajust information
c
c   OUTPUT : 
c			pResult:  
c   RETURN:    None
c
**********************************************************************/
void CNCLHeaderCtrl::OnHeadItemEndTrack (NMHDR* pNMHDR, LRESULT* pResult)
{
	NMHEADER *pHdr = (HD_NOTIFY*)pNMHDR;
	int width, delx;
	if (!((pHdr->pitem)&&(pHdr->pitem->mask&HDI_WIDTH)))
		return;
	width = pHdr->pitem->cxy;
	if (pHdr->iItem==0)
	{
		delx = width - m_comborc.Width();
		if (delx<0)
/*
......move left, only this item the right side column affect
*/
		{
			m_comborc.right = m_comborc.right + delx ;
			m_bandrec0.left = m_bandrec0.left + delx;
			m_bandrec0.right = m_bandrec0.right + delx;
			m_bandrec1.left = m_bandrec1.left + delx;
			m_bandrec1.right = m_bandrec1.right + delx;
			m_bandrec2.left = m_bandrec2.left + delx;
			m_bandrec2.right = m_bandrec2.right + delx;
			m_bandrec3.left = m_bandrec3.left + delx;
			m_bandrec3.right = m_bandrec3.right + delx;
		}
		else
/*
......move right, only this item the right side column affect
*/
		{
			m_comborc.right = m_comborc.right + delx ;
			m_bandrec0.left = m_bandrec0.left + delx;
			m_bandrec0.right = m_bandrec0.right + delx;
			m_bandrec1.left = m_bandrec1.left + delx;
			m_bandrec1.right = m_bandrec1.right + delx;
			m_bandrec2.left = m_bandrec2.left + delx;
			m_bandrec2.right = m_bandrec2.right + delx;
			m_bandrec3.left = m_bandrec3.left + delx;
			m_bandrec3.right = m_bandrec3.right + delx;
		}
	}
	else if (pHdr->iItem==1)
	{
		delx = width - m_bandrec0.Width();
		if (delx<0)
/*
......move left, only this item the right side column affect
*/
		{
			m_bandrec0.right = m_bandrec0.right + delx;
			m_bandrec1.left = m_bandrec1.left + delx;
			m_bandrec1.right = m_bandrec1.right + delx;
			m_bandrec2.left = m_bandrec2.left + delx;
			m_bandrec2.right = m_bandrec2.right + delx;
			m_bandrec3.left = m_bandrec3.left + delx;
			m_bandrec3.right = m_bandrec3.right + delx;
		}
		else
/*
......move right, only this item the right side column affect
*/
		{
			m_bandrec0.right = m_bandrec0.right + delx;
			m_bandrec1.left = m_bandrec1.left + delx;
			m_bandrec1.right = m_bandrec1.right + delx;
			m_bandrec2.left = m_bandrec2.left + delx;
			m_bandrec2.right = m_bandrec2.right + delx;
			m_bandrec3.left = m_bandrec3.left + delx;
			m_bandrec3.right = m_bandrec3.right + delx;
		}
	}
	else if (pHdr->iItem==2)
	{
		delx = width - m_bandrec1.Width();
		if (delx<0)
/*
......move left, only this item the right side column affect
*/
		{
			m_bandrec1.right = m_bandrec1.right + delx;
			m_bandrec2.left = m_bandrec2.left + delx;
			m_bandrec2.right = m_bandrec2.right + delx;
			m_bandrec3.left = m_bandrec3.left + delx;
			m_bandrec3.right = m_bandrec3.right + delx;
		}
		else
/*
......move right, only this item the right side column affect
*/
		{
			m_bandrec1.right = m_bandrec1.right + delx;
			m_bandrec2.left = m_bandrec2.left + delx;
			m_bandrec2.right = m_bandrec2.right + delx;
			m_bandrec3.left = m_bandrec3.left + delx;
			m_bandrec3.right = m_bandrec3.right + delx;
		}
	}
	else if (pHdr->iItem==3)
	{
		delx = width - m_bandrec2.Width();
		if (delx<0)
/*
......move left, only this item the right side column affect
*/
		{
			m_bandrec2.right = m_bandrec2.right + delx;
			m_bandrec3.left = m_bandrec3.left + delx;
			m_bandrec3.right = m_bandrec3.right + delx;
		}
		else
/*
......move right, only this item the right side column affect
*/
		{
			m_bandrec2.right = m_bandrec2.right + delx;
			m_bandrec3.left = m_bandrec3.left + delx;
			m_bandrec3.right = m_bandrec3.right + delx;
		}
	}
	else if (pHdr->iItem==4)
	{
		delx = width - m_bandrec3.Width();
		if (delx<0)
/*
......move left, only this item the right side column affect
*/
		{
			m_bandrec3.right = m_bandrec3.right + delx;
		}
		else
/*
......move right, only this item the right side column affect
*/
		{
			m_bandrec3.right = m_bandrec3.right + delx;
		}
	}
}
/***********************************************************************
c
c   FUNCTION: OnAdjustStatic()
c
c       adjust the size and position of the filter area
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    none
c
**********************************************************************/
void CNCLHeaderCtrl::OnAdjustStatic()
{
	CRect rcRect;
	m_pWndBand0->MoveWindow(&m_bandrec0);
	m_pWndBand0->ShowWindow(SW_SHOW);

	if (m_pWndCombo->GetCurSel()==3)
	{
		m_pWndCombo->MoveWindow(&m_comborc);
		RedrawWindow();
		UpdateWindow();
		return;
	}
	m_pWndBand1->MoveWindow(&m_bandrec1);
	m_pWndBand1->ShowWindow(SW_SHOW);

	m_pWndBand3->MoveWindow(&m_bandrec3);
	m_pWndBand3->ShowWindow(SW_SHOW);
/*
.....move the edit or combo window
*/
	if (m_combo_open == 1)
	{
		m_pWndCombo->MoveWindow(&m_comborc);
		m_pWndKType->MoveWindow(&m_bandrec2);
	}
	if (m_editting==0)
	{
		rcRect = m_bandrec1;
		m_pWndEdit->MoveWindow(&rcRect);
	}
	else if (m_editting==2)
	{
		rcRect = m_bandrec3;
		m_pWndEdit->MoveWindow(&rcRect);
	}
	RedrawWindow();
	UpdateWindow();
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
void CNCLHeaderCtrl::OnPaint() 
{
/*
.....only paint the upper part, not the filter
*/
	CPaintDC dc(this);
	CRect rect;
	GetClientRect(&rect);

	CRect filterRect;
	filterRect = rect;
	filterRect.top = filterRect.bottom/2;
	dc.ExcludeClipRect(&filterRect);

	DefWindowProc(WM_PAINT, (WPARAM)dc.m_hDC, (LPARAM)0);
}

/***********************************************************************
**
**   FUNCTION: OnEraseBkgnd(CDC* pDC) 
**			The framework calls this member function 
**			when the CNCLHeaderCtrl object background needs 
**			erasing (for example, when resized). 
**		    It is called to prepare an invalidated 
**			region for painting.
**   
**	 INPUT:  CDC* pDC: device context
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
/*
.....not changed
*/
BOOL CNCLHeaderCtrl::OnEraseBkgnd(CDC* pDC) 
{
	return CHeaderCtrl::OnEraseBkgnd(pDC); 
//	return Fasle;
}
#endif
