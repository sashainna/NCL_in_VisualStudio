/************************************************************************
**
**   FILE NAME: wsntformFview.cpp
**
**	 Description - Functions and implementations for
**		CNCLFormFView class
**
**	 CONTAINS: 
**		class functions of CNCLFormFView class
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntformFview.cpp , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:35:44
***********************************************************************
*/
#include "stdafx.h"
#include "wsgl.h"
#include "wsntformdoc.h"
#include "wsntfrmFview.h"
#include	"mfort.h"
#include	"wsntres.h"
#include "wsntFdsnFrm.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern CFrameWnd *UW_Dform_frame;

IMPLEMENT_DYNCREATE(CNCLFormFView, CFormView)

BEGIN_MESSAGE_MAP(CNCLFormFView, CFormView)
	//{{AFX_MSG_MAP(CNCLFormFView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
		ON_CONTROL(EN_KILLFOCUS, IDC_EDIT1, OnTitleChange)
		ON_CONTROL(EN_KILLFOCUS, IDC_EDIT2, OnFormSizeChange)
		ON_COMMAND(IDC_CHECK1, OnCheckMacro1)
		ON_COMMAND(IDC_CHECK2, OnCheckMacro2)
		ON_COMMAND(IDC_CHECK3, OnCheckMacro3)
		ON_COMMAND(IDC_CHECK4, OnCheckMacro4)
		ON_COMMAND(IDC_CHECK5, OnCheckMacro5)
		ON_COMMAND(IDC_CHECK7, OnCheckMacro6)
		ON_COMMAND(IDC_CHECK6, OnFormSection)
		ON_CONTROL(CBN_SELCHANGE, IDC_COMBO1, OnFormType)
//		ON_CONTROL(CBN_SELCHANGE, IDC_COMBO2, OnFormSection)
	//}}AFX_MSG_MAP
	// Standard printing commands
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  OnFormType()
c   FUNCTION:  Callback function for form type choicebox
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormFView::OnFormType()
{
	int old_type = ((CNCLFdsnFrame*)UW_Dform_frame)->get_current_type();
	int type = ((CComboBox*)GetDlgItem(IDC_COMBO1))->GetCurSel();
	if ((type==0)&&(m_macro_flag==0))
	{
//		((CComboBox*)GetDlgItem(IDC_COMBO2))->EnableWindow(1);
		GetDlgItem(IDC_CHECK6)->EnableWindow(1);
	}
	else
	{
//		((CComboBox*)GetDlgItem(IDC_COMBO2))->EnableWindow(0);
		GetDlgItem(IDC_CHECK6)->EnableWindow(0);
	}
	if (UW_Dform_frame!=NULL)
	{
/* 
.....when there is type switch, the border adjust need change too
*/
		((CNCLFdsnFrame*)UW_Dform_frame)->m_delx = 0;
		((CNCLFdsnFrame*)UW_Dform_frame)->m_dely = 0;
		int dx = 0;
		int dy = 0;
		if (type!=old_type)
		{
			if ((old_type==0)&&(type!=0))
			{
				dx = -11; //21 - 10; // (borderx*2 + 8 + 13 + 4) - (borderx*2 + 4) = 21;
				dy = 3;  // 7 - 10; // (bordery +  borderx + 5 + 15) - (bordery + borderx*2 + 4) = 7
			}
			else if ((old_type!=0)&&(type==0))
			{
				dx = 11; // = -21 + 10; // 10 is 5 units 
				dy = -3;
			}
			if ((dx!=0)||(dy!=0))
			{
				((CNCLFdsnFrame*)UW_Dform_frame)->Resize_form_frame(dx, dy);
			}
		}
		((CNCLFdsnFrame*)UW_Dform_frame)->OnFormType(type);
		((CNCLFdsnFrame*)UW_Dform_frame)->m_delx = 0;
		((CNCLFdsnFrame*)UW_Dform_frame)->m_dely = 0;
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnFormSection()
c   FUNCTION:  Callback function for form multi-page choicebox
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormFView::OnFormSection()
{
	int old_type = ((CNCLFdsnFrame*)UW_Dform_frame)->get_current_secflag();
//	int type = ((CComboBox*)GetDlgItem(IDC_COMBO2))->GetCurSel();
	int type = ((CButton*)GetDlgItem(IDC_CHECK6))->GetCheck();
	if (type)
	{
		((CComboBox*)GetDlgItem(IDC_COMBO1))->SetCurSel(0);
	}
	int cx, cy, dx=0, dy=0;
	if (UW_Dform_frame!=NULL)
	{
		if (type!=old_type)
		{
			if ((old_type==0)&&(type!=0))
			{
/*
.....'no section' to 'section'
*/			
				cx = 120; //widlabel+10 unit
				cy = 10; // 5 units
				cx -= 10; // (borderx*2 + 15) - (borderx*2 + 8 + 13 + 4);
				cy -= 17; // (bordery +  borderx + 5 - 2) - (bordery +  borderx + 5 + 15);
				dx = 110; //120 - 10;
				dy = -7;  // 10 - 17; 
			}
			else if ((old_type!=0)&&(type==0))
			{
				dx = -110; 
				dy = 7;
			}
			if ((dx!=0)||(dy!=0))
			{
				((CNCLFdsnFrame*)UW_Dform_frame)->Resize_form_frame(dx, dy);
			}
		}
		((CNCLFdsnFrame*)UW_Dform_frame)->OnFormSection(type);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheckMacro1()
c   FUNCTION:  Callback function for checkbox1
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormFView::OnCheckMacro1()
{
	if (UW_Dform_frame!=NULL)
		((CNCLFdsnFrame*)UW_Dform_frame)->OnCheckMacro1();
}
/***********************************************************************
c
c   SUBROUTINE:  OnCheckMacro2()
c   FUNCTION:  Callback function for checkbox2
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormFView::OnCheckMacro2()
{
	if (UW_Dform_frame!=NULL)
		((CNCLFdsnFrame*)UW_Dform_frame)->OnCheckMacro2();
}
/***********************************************************************
c
c   SUBROUTINE:  OnCheckMacro3()
c   FUNCTION:  Callback function for checkbox3
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormFView::OnCheckMacro3()
{
	if (UW_Dform_frame!=NULL)
		((CNCLFdsnFrame*)UW_Dform_frame)->OnCheckMacro3();
}
/***********************************************************************
c
c   SUBROUTINE:  OnCheckMacro4()
c   FUNCTION:  Callback function for checkbox4
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormFView::OnCheckMacro4()
{
	if (UW_Dform_frame!=NULL)
		((CNCLFdsnFrame*)UW_Dform_frame)->OnCheckMacro4();
}
/***********************************************************************
c
c   SUBROUTINE:  OnCheckMacro5()
c   FUNCTION:  Callback function for checkbox5
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormFView::OnCheckMacro5()
{
	if (UW_Dform_frame!=NULL)
		((CNCLFdsnFrame*)UW_Dform_frame)->OnCheckMacro5();
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheckMacro6()
c   FUNCTION:  Callback function for checkbox6
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormFView::OnCheckMacro6()
{
	if (UW_Dform_frame!=NULL)
		((CNCLFdsnFrame*)UW_Dform_frame)->OnCheckMacro6();
}

/***********************************************************************
c
c   SUBROUTINE:  OnFormSizeChange()
c   FUNCTION:  This function called form size edit input is changed
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormFView::OnFormSizeChange()
{
	if (UW_Dform_frame==NULL)
		return;
	char sizestr[41];
	int len = ((CEdit*)GetDlgItem(IDC_EDIT2))->GetWindowText(sizestr, 40);
	if (len==0)
		return;
	CRect rect, dlgrect;
	int cx, cy;
	char *tok;
	tok = strtok(sizestr, ", ");
	if (tok==NULL)
		return;
	cx = atoi(tok);
	tok = strtok(NULL, ", ");
	if (tok==NULL)
		return;
	cy = atoi(tok);
	if ((cx<0)||(cy<0)||(cx>5000)||(cy>5000))
		return;
	((CNCLFdsnFrame*)UW_Dform_frame)->OnFormSizeChange(cx, cy);
}
/***********************************************************************
c
c   SUBROUTINE:  OnTitleChange()
c   FUNCTION:  Callback function for title edit input
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormFView::OnTitleChange()
{
	if (UW_Dform_frame==NULL)
		return;
	CEdit *cwin = (CEdit*)GetDlgItem(IDC_EDIT1);
	if (cwin==NULL)
		return;
	CString sText;
	cwin->GetWindowText(sText);	
	((CNCLFdsnFrame*)UW_Dform_frame)->OnTitleChange(sText);
}
/***********************************************************************
**   FUNCTION: CNCLFormFView
**		Constructor of class CNCLFormFView
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormFView::CNCLFormFView() : CFormView(CNCLFormFView::IDD)
{
}

/***********************************************************************
**
**   FUNCTION: ~CNCLFormFView
**              Destructor of class CNCLFormFView, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormFView::~CNCLFormFView()
{
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
void CNCLFormFView::OnInitialUpdate()
{
	ASSERT_VALID(this);
	if (!UpdateData(FALSE))
		TRACE(traceAppMsg, 0, "UpdateData failed during formview initial update.\n");
	CFormView::OnInitialUpdate();
	m_ListBox.SubclassDlgItem(IDC_ICON_LISTBOX,this);
	if (m_macro_flag==0)
	{
		m_ListBox.AddItem("CHECKBOX", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP2)));
		m_ListBox.AddItem("CHOICEBOX", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP4)));
		m_ListBox.AddItem("CHOICE_LIST", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP4)));
		m_ListBox.AddItem("COLOR", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP3)));
		m_ListBox.AddItem("COMBLIST_DROPDOWN",LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP4)));
		m_ListBox.AddItem("COMBLIST_SIMPLE",LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP4)));
		m_ListBox.AddItem("DATATABLE", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP9)));
		m_ListBox.AddItem("DISPLAY", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP5)));
		m_ListBox.AddItem("EDIT", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP5)));
		m_ListBox.AddItem("EDITBOX", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP5)));
		m_ListBox.AddItem("FRAME", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP6)));
		m_ListBox.AddItem("LABEL", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP7)));
		m_ListBox.AddItem("LISTBOX", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP8)));
		m_ListBox.AddItem("LISTTABLE", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP9)));
		m_ListBox.AddItem("PICTUREBOX", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP10)));
		m_ListBox.AddItem("PROGRESS", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP11)));
		m_ListBox.AddItem("PUSHBUTTON", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP1)));
		m_ListBox.AddItem("SELECT", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP1)));
		m_ListBox.AddItem("SLIDER", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP12)));
		m_ListBox.AddItem("VIDEO", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP13)));
	}
	else
	{
		m_ListBox.AddItem("CHECKBOX", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP2)));
		m_ListBox.AddItem("CHOICEBOX", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP4)));
		m_ListBox.AddItem("EDIT", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP5)));
		m_ListBox.AddItem("FRAME", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP6)));
		m_ListBox.AddItem("PICTUREBOX", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP10)));
		m_ListBox.AddItem("SELECT", LoadBitmap(::AfxGetApp()->m_hInstance, MAKEINTRESOURCE(IDB_BMP1)));
	}
	m_ListBox.SetCurSel(0);
	
	if (m_macro_flag==0)
	{
		((CButton*)GetDlgItem(IDC_CHECK6))->SetCheck(0);
		CRect brect, crect;
		m_ListBox.GetWindowRect(&brect);
		((CButton*)GetDlgItem(IDC_CHECK7))->GetWindowRect(&crect);
		brect.bottom += (crect.bottom - brect.bottom) + 4;
		ScreenToClient(brect);
		m_ListBox.MoveWindow(&brect);
		((CButton*)GetDlgItem(IDC_CHECK1))->ShowWindow(SW_HIDE);
		((CButton*)GetDlgItem(IDC_CHECK2))->ShowWindow(SW_HIDE);
		((CButton*)GetDlgItem(IDC_CHECK3))->ShowWindow(SW_HIDE);
		((CButton*)GetDlgItem(IDC_CHECK4))->ShowWindow(SW_HIDE);
		((CButton*)GetDlgItem(IDC_CHECK5))->ShowWindow(SW_HIDE);
		((CButton*)GetDlgItem(IDC_CHECK7))->ShowWindow(SW_HIDE);
	}
	else
	{
		((CButton*)GetDlgItem(IDC_CHECK6))->ShowWindow(SW_HIDE);
	}
	((CComboBox*)GetDlgItem(IDC_COMBO1))->DeleteString(0);
	((CComboBox*)GetDlgItem(IDC_COMBO1))->AddString("NO");
	((CComboBox*)GetDlgItem(IDC_COMBO1))->AddString("YES");
	((CComboBox*)GetDlgItem(IDC_COMBO1))->AddString("TOP");
	((CComboBox*)GetDlgItem(IDC_COMBO1))->AddString("BOTTOM");
	((CComboBox*)GetDlgItem(IDC_COMBO1))->AddString("LEFT");
	((CComboBox*)GetDlgItem(IDC_COMBO1))->AddString("RIGHT");
	((CComboBox*)GetDlgItem(IDC_COMBO1))->SetCurSel(0);
/*
	((CComboBox*)GetDlgItem(IDC_COMBO2))->DeleteString(0);
	((CComboBox*)GetDlgItem(IDC_COMBO2))->AddString("NO");
	((CComboBox*)GetDlgItem(IDC_COMBO2))->AddString("YES");
	((CComboBox*)GetDlgItem(IDC_COMBO2))->SetCurSel(0);
	if (m_macro_flag)
	{
		((CComboBox*)GetDlgItem(IDC_COMBO2))->EnableWindow(0);
	}
	else
		((CComboBox*)GetDlgItem(IDC_COMBO2))->EnableWindow(1);
*/
}
BOOL CNCLFormFView::PreCreateWindow(CREATESTRUCT& cs)
{
	BOOL bPreCreated = CFormView::PreCreateWindow(cs);
	return bPreCreated;
}
void CNCLFormFView::SetMacroActive(int itemno, int active)
{
	if (itemno==0)
	{
		if (active!=-1)
			((CButton*)GetDlgItem(IDC_CHECK1))->SetCheck(1);
		else
			((CButton*)GetDlgItem(IDC_CHECK1))->SetCheck(0);
	}
	if (itemno==1)
	{
		if (active!=-1)
		((CButton*)GetDlgItem(IDC_CHECK2))->SetCheck(1);
		else
			((CButton*)GetDlgItem(IDC_CHECK2))->SetCheck(0);
	}
	if (itemno==2)
	{
		if (active!=-1)
		((CButton*)GetDlgItem(IDC_CHECK3))->SetCheck(1);
		else
			((CButton*)GetDlgItem(IDC_CHECK3))->SetCheck(0);
	}
	if (itemno==3)
	{
		if (active!=-1)
		((CButton*)GetDlgItem(IDC_CHECK4))->SetCheck(1);
		else
			((CButton*)GetDlgItem(IDC_CHECK4))->SetCheck(0);
	}
	if (itemno==4)
	{
		if (active!=-1)
			((CButton*)GetDlgItem(IDC_CHECK5))->SetCheck(1);
		else
			((CButton*)GetDlgItem(IDC_CHECK5))->SetCheck(0);
	}
	if (itemno==5)
	{
		if (active!=-1)
			((CButton*)GetDlgItem(IDC_CHECK7))->SetCheck(1);
		else
			((CButton*)GetDlgItem(IDC_CHECK7))->SetCheck(0);
	}
}

/*********************************************************************
**    I_FUNCTION:  GetCheckedMacro(int itemno)
**      get the current Macro button activity
**    PARAMETERS   
**       INPUT  : itemno: macro button index
**       OUTPUT :  none
**    RETURNS      : active: 1: checked
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int CNCLFormFView::GetCheckedMacro(int itemno)
{
	int checked;
	if (itemno==0)
		checked = ((CButton*)GetDlgItem(IDC_CHECK1))->GetCheck();
	if (itemno==1)
		checked = ((CButton*)GetDlgItem(IDC_CHECK2))->GetCheck();
	if (itemno==2)
		checked = ((CButton*)GetDlgItem(IDC_CHECK3))->GetCheck();
	if (itemno==3)
		checked = ((CButton*)GetDlgItem(IDC_CHECK4))->GetCheck();
	if (itemno==4)
		checked = ((CButton*)GetDlgItem(IDC_CHECK7))->GetCheck();
	if (itemno==5)
		checked = ((CButton*)GetDlgItem(IDC_CHECK5))->GetCheck();
	return checked;
}

#ifdef _DEBUG
void CNCLFormFView::AssertValid() const
{
	CFormView::AssertValid();
}
CNCLFormDoc* CNCLFormFView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CNCLFormDoc)));
	return (CNCLFormDoc*)m_pDocument;
}
#endif //_DEBUG

