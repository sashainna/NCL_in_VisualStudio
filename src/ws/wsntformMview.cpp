/************************************************************************
**
**   FILE NAME: wsntformMview.cpp
**
**	 Description - Functions and implementations for
**		CNCLFormMView class
**
**	 CONTAINS: 
**		class functions of CNCLFormMView class
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntformMview.cpp , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**			06/17/15 , 14:20:42
***********************************************************************
*/
#include "stdafx.h"
#include <dwmapi.h>
#include <direct.h>
#include "wsntctl.h"
#include "wsgl.h"
#include "wsntformdoc.h"
#include "wsntformfrm.h"
#include "wsntfrmview.h"
#include "wsntfrmMview.h"
#include "wsntformfrm.h"
#include	"mfort.h"
#include	"wsntres.h"
#include "wsntcfunc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define FORMTOPX	5
#define FORMTOPY	5

extern CFrameWnd *UW_Dform_frame;
extern "C" void ud_get_filename(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag);
extern "C" int ul_open_mod_file(char*, char*, char*, char*, char*, int, FILE**);
extern "C" int ud_loadfrm(char *fname, UD_FSTRUCT *fstruct);
extern "C" float uw_form_scalex;
extern "C" float uw_form_scaley;
extern "C" int ul_break_fname(char *fullname, char* dir, char* fname);
extern "C" int ncl_get_curmac (char *mnam, int *nc);
extern "C" int ncl_getmac_pnum(int *num);
extern "C" int ul_to_lower(char *fname);
//extern void uw_MapDialogRect(CRect &rect);
extern void uw_MapDialogRect(CRect &rect, CDialog *wnd);

IMPLEMENT_DYNCREATE(CNCLFormMView, CFormView)

BEGIN_MESSAGE_MAP(CNCLFormMView, CFormView)
	//{{AFX_MSG_MAP(CNCLFormMView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	ON_WM_DESTROY()
	//}}AFX_MSG_MAP
	// Standard printing commands
END_MESSAGE_MAP()

/***********************************************************************
**   FUNCTION: CNCLFormMView
**		Constructor of class CNCLFormMView
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormMView::CNCLFormMView() : CFormView(CNCLFormMView::IDD)
{
	m_parent = NULL;
	m_frame = NULL;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLFormMView
**              Destructor of class CNCLFormMView, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormMView::~CNCLFormMView()
{
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
void CNCLFormMView::OnUndo()
{
	if (m_frame==NULL)
		return;
	CNCLFormView *actview = (CNCLFormView *)(m_frame->GetActiveView());
	actview->OnUndo();
}
/***********************************************************************
c
c   SUBROUTINE:  CreateFrame()
c   FUNCTION:  This function will create the form frame window inside the view
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormMView::CreateFrame()
{
	CCreateContext Context;
	Context.m_pNewViewClass = RUNTIME_CLASS(CNCLFormView);
	m_frame = new CNCLFormFrm();
	m_frame->Create(NULL, "Test", WS_VISIBLE|WS_CAPTION|WS_CHILD|WS_OVERLAPPED| WS_THICKFRAME, CRect(10,10,400,400), this, NULL,0,  &Context);
	CDocument *doc = m_parent->GetActiveDocument(); 
	m_frame->InitialUpdateFrame(doc, FALSE);
	CNCLFormView *actview = (CNCLFormView *)(m_frame->GetActiveView());
	actview->SetPrentDlg(this);
	actview->CreateForm();

	m_frame->SetWindowText("Design Form");
	m_frame->GetClientRect(&m_frmrect);
	actview->GetClientRect(&m_viewrect);

	CRect dlgrect;
	GetWindowRect(&dlgrect);
	CRect rect;
	m_frame->GetWindowRect(&rect);
	m_frmrect = rect;
	m_org_frect.top = rect.top;
	m_org_frect.left = rect.left;
	m_org_frect.right = dlgrect.right - 15;
	m_org_frect.bottom = dlgrect.bottom - 30;

	int cx = rect.Width();
	int cy = rect.Height();

	m_delx = 8;
	m_dely = 2;

	rect.left =  m_delx;
	rect.top =  m_dely;
	rect.right = rect.left + cx;
	rect.bottom = rect.top + cy;
	m_frame->MoveWindow(&rect);
	SetSizeText();
	m_fvdx = m_frmrect.Width() - actview->m_viewrect.Width();
	m_fvdy = m_frmrect.Height() - actview->m_viewrect.Height();
}
/***********************************************************************
c
c   SUBROUTINE:  DeleteFrame()
c   FUNCTION:  This function will delete the form frame window inside the view
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormMView::DeleteFrame()
{
	delete m_frame;
	m_frame = NULL;
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
void CNCLFormMView::OnInitialUpdate()
{
	ASSERT_VALID(this);
	CFormView::OnInitialUpdate();
	if (m_frame==NULL)
		return;
}

BOOL CNCLFormMView::PreCreateWindow(CREATESTRUCT& cs)
{
	BOOL bPreCreated = CFormView::PreCreateWindow(cs);
	return bPreCreated;
}
/***********************************************************************
c
c   SUBROUTINE:  OnDestroy()
c   FUNCTION:  This function called when destroy the view
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormMView::OnDestroy() 
{
	if (m_frame!=NULL)
	{
		delete m_frame;
		m_frame = NULL;
	}
	CView::OnDestroy();
}

/***********************************************************************
c
c   FUNCTION: OpenPropertyPage(CNCLFormProp *prop_dlg, int flag)
c
c		open Property page view
c   INPUT:  prop_dlg: Property value to set
c			flag: 0: update the data and show window
c					1: show window only
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormMView::OpenPropertyPage(CNCLFormProp *prop_dlg, int flag)
{
	if (m_frame==NULL)
		return;
	if (flag==0)
	{
		if (m_frame->IsWindowVisible()==0)
			return;
	}
	((CNCLFdsnFrame*)m_parent)->OpenPropertyPage(prop_dlg, flag);
}

/***********************************************************************
c
c   FUNCTION: UpdatePropertySize(int cx, int cy)
c
c		Update the property size edit field
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormMView::UpdatePropertySize(int cx, int cy)
{
	if (m_parent!=NULL)
		((CNCLFdsnFrame*)m_parent)->UpdatePropertySize(cx, cy);
}

void CNCLFormMView::UpdatePropertyPos(int x, int y)
{
	if (m_parent!=NULL)
		((CNCLFdsnFrame*)m_parent)->UpdatePropertyPos(x, y);
}

void CNCLFormMView::UpdatePropertyHSPTSize(CRect sizerec, float prect[4])
{
	if (m_parent!=NULL)
		((CNCLFdsnFrame*)m_parent)->UpdatePropertyHSPTSize(sizerec, prect);
}

/*********************************************************************
**    I_FUNCTION:  SetSizeText()
**      Set the current form size into the property size edit field
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormMView::SetSizeText()
{
	if (m_parent!=NULL)
		((CNCLFdsnFrame*)m_parent)->SetSizeText();
}

/*********************************************************************
**    I_FUNCTION:  GetPView()
**      Get the current property view
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : current property view
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
CView* CNCLFormMView::GetPView()
{
	if (m_parent!=NULL)
		return ((CNCLFdsnFrame*)m_parent)->GetPView();
	return NULL;
}
/*********************************************************************
**    I_FUNCTION:  EnableAlignBtn(int flag)
**      Enable the current Align button of toolbar
**    PARAMETERS   
**       INPUT  : flag: 0: disable, 1: enabled
**       OUTPUT :  none
**    RETURNS      : current property view
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormMView::EnableAlignBtn(int flag)
{
	if (m_parent!=NULL)
		((CNCLFdsnFrame*)m_parent)->EnableAlignBtn(flag);
}
/*********************************************************************
**    I_FUNCTION:  SetMacroActive(int mwin_no, int active)
**      Set the current Macro button activity
**    PARAMETERS   
**       INPUT  : mwin_no: macro button index
**				active: 1: check the active button
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormMView::SetMacroActive(int mwin_no, int active)
{
	if (m_parent!=NULL)
		((CNCLFdsnFrame*)m_parent)->SetMacroActive(mwin_no, active);
}
/*********************************************************************
**    I_FUNCTION:  GetCheckedMacro(int no)
**      get the current Macro button activity
**    PARAMETERS   
**       INPUT  : no: macro button index
**       OUTPUT :  none
**    RETURNS      : active: 1: checked
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int CNCLFormMView::GetCheckedMacro(int no)
{
	if (m_parent!=NULL)
		return ((CNCLFdsnFrame*)m_parent)->GetCheckedMacro(no);
	return 0;
}
/***********************************************************************
c
c   FUNCTION: FormSave()
c
c		Save the current windows from the form frame into a form file
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CNCLFormMView::FormSave(char *infile, char *helptxt)
{
	char title[80];
	char *tok;
	UX_pathname filename, dir, fname,save_dir, orig_file;
	int nc;
	char mcname[64];
/*
......if it is MACRO form, check if all field matched
*/
	CNCLFormView *actview = (CNCLFormView *)(m_frame->GetActiveView());
	int status;
	char errmsg[256];
	if (m_macro_flag)
	{
		status = actview->m_right_form->CheckMacroForm(errmsg);
		if (status==-1)
		{
/*
.....error message
*/
			MessageBox(errmsg, "Form Save Error", MB_OK);
			return -1;
		}
	}
	if ((infile[0]=='\0')||(strcmp(infile, "INTERNAL.INTERNAL")==0))
	{
		filename[0] = '\0';
		dir[0] = '\0';
		fname[0] = '\0';
		if (m_macro_flag==1)
		{
			ncl_get_curmac (mcname, &nc);
			strcpy(fname, mcname);
			strcat(fname,".frm");
			ul_to_lower(fname);
			strcpy(filename, fname);
		}
	}
	else
	{
		strcpy(filename, infile);
		ul_break_fname(filename, dir, fname);
	}
/*
.....default the saving directory to UU_USER_SETTING directory, if this
.....directory is not there, create one
*/
/*
.....save current directory in case filebrowser change it
*/
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);
	if (dir[0]=='\0')
	{
		if (m_macro_flag==0)
		{
			status = ul_open_mod_file("UU_USER_SETTINGS", "forms", (char*)UU_NULL, (char*)UU_NULL,
				filename, 1, (FILE**)UU_NULL);
		}
		else
		{
			status = ul_open_mod_file("UU_USER_SETTINGS", "forms", "NCL_INCDIR", (char*)UU_NULL,
			filename, 1, (FILE**)UU_NULL);
		}
		if ((status == UU_SUCCESS) && (fname[0]=='\0'))
			strcat_s (filename, "\\*.frm");
	}
	strcpy(orig_file, filename);
	ud_get_filename(this, "Save Form File", "*.frm", filename, &nc, "Form Files (*.frm)", 0);
	_chdir(save_dir);
	if (nc==0)
		return -1;
	m_frame->GetWindowText(title, 80);

	CRect rect;
	int cx, cy;
	m_frame->GetWindowRect(&m_frmrect);
/*
......accept all form and write into a form file and close the dialog
*/
	actview = (CNCLFormView *)(m_frame->GetActiveView());
	actview->GetWindowRect(&rect);
	cx = m_frmrect.Width();
	cy = m_frmrect.Height();
	
	CRect temp (0,0, 7, 7);
//	((CDialog*)this)->MapDialogRect(&temp);
//	uw_MapDialogRect(temp);
	uw_MapDialogRect(temp, ((CDialog*)this));
/////////////
	int borderx = GetSystemMetrics(SM_CXFRAME);
	int bordery = GetSystemMetrics(SM_CYCAPTION);

	cx = cx - m_fvdx - borderx*2;
	cy = cy - m_fvdy - bordery;
//////////////

	int type = ((CNCLFdsnFrame*)UW_Dform_frame)->m_type;
	CString helpstr = helptxt;
	if (stricmp(filename, orig_file)==0)
		status = actview->m_right_form->SaveForm(filename, title, cx, cy, type, helpstr, 0);
	else
		status = actview->m_right_form->SaveForm(filename, title, cx, cy, type, helpstr, 1);

	((CNCLFormView *)(m_frame->GetActiveView()))->m_formchanged = 0;

	if (status==-1)
		return -1;	
	return 0;
}
void CNCLFormMView::FormLoad(char *infile, char **helptxt, char *title)
{
	char *tok, *helpstr;
	UX_pathname filename;
	int nc;
	if ((infile==NULL)||(infile[0]=='\0'))
		filename[0] = '\0';
	else
	{
		strcpy(filename, infile);
		nc = strlen(infile);
	}
	int status;
	if (filename[0]=='\0')
	{
		if (m_macro_flag==0)
		{
			status = ul_open_mod_file("UU_USER_SETTINGS", "forms", (char*)UU_NULL, (char*)UU_NULL,
				filename, 0, (FILE**)UU_NULL);
		}
		else
		{
			status = ul_open_mod_file("UU_USER_SETTINGS", "forms", "NCL_INCDIR", (char*)UU_NULL,
			filename, 0, (FILE**)UU_NULL);
		}
		if (status == UU_SUCCESS)
			strcat_s (filename, "\\*.frm");
		ud_get_filename(this, "Load Form File", "*.frm", filename, &nc, "Form Files (*.frm)", 1);
	}
	if (nc==0)
		return;

	UD_FSTRUCT fstruct;
	status = ud_loadfrm(filename, &fstruct);
	if(status != 0)
	{
		return;
	}
	if (infile!=NULL)
		strcpy(infile, filename);

	int i, chrwid, phgt;
	int widlabel = 0;
	for (i=0; i<fstruct.n_section_fields; i++)
	{
		uw_ntget_strsize(fstruct.section_flds[i].name, 80, "MS Sans Serif", &chrwid, &phgt);
		if (widlabel<chrwid)
			widlabel = chrwid;
	}

	if (widlabel<50)
		widlabel = 50;
	int type;
	if ((fstruct.dockable[0] == 0)&&(fstruct.dockable[1]==0)
		&&(fstruct.dockable[2] == 0)&&(fstruct.dockable[3]==0))
		type = 0;
	else if ((fstruct.dockable[0] == 1)&&(fstruct.dockable[1]==1)
		&&(fstruct.dockable[2] == 1)&&(fstruct.dockable[3]==1))
		type = 1;
	else if ((fstruct.dockable[0] == 1)&&(fstruct.dockable[1]==0)
		&&(fstruct.dockable[2] == 0)&&(fstruct.dockable[3]==0))
		type = 2;
	else if ((fstruct.dockable[0] == 0)&&(fstruct.dockable[1]==1)
		&&(fstruct.dockable[2] == 0)&&(fstruct.dockable[3]==0))
		type = 3;
	else if ((fstruct.dockable[0] == 0)&&(fstruct.dockable[1]==0)
		&&(fstruct.dockable[2] == 1)&&(fstruct.dockable[3]==0))
		type = 4;
	else if ((fstruct.dockable[0] == 0)&&(fstruct.dockable[1]==0)
		&&(fstruct.dockable[2] == 0)&&(fstruct.dockable[3]==1))
		type = 5;

	int cx, cy;
	if ((type==0) && (fstruct.n_section_fields>0))
	{
		cx = (short)(fstruct.ud_frmdf.ud_frc.ud_c*uw_form_scalex)+widlabel+10;
		cy = (short)(fstruct.ud_frmdf.ud_frc.ud_r*uw_form_scaley)+5;
	}
	else if (type==0)
	{
		cx = (short)(fstruct.ud_frmdf.ud_frc.ud_c*uw_form_scalex);
		cy = (short)(fstruct.ud_frmdf.ud_frc.ud_r*uw_form_scaley);
	}
	else
	{
		cx = (short)(fstruct.ud_frmdf.ud_frc.ud_c*uw_form_scalex) + FORMTOPX;
		cy = (short)(fstruct.ud_frmdf.ud_frc.ud_r*uw_form_scaley) + FORMTOPY;
	}
/*
.....to match with NCLform
*/
/*
	if (fstruct.n_section_fields>0)
	{
		cy -= 20;
	}
	else
	{
		cx -= 10;
		cy -= 25;
	}
*/
	if (fstruct.n_section_fields>0)
		((CNCLFdsnFrame*)UW_Dform_frame)->SetSecFlagVal(1);
	else
		((CNCLFdsnFrame*)UW_Dform_frame)->SetSecFlagVal(0);

	CRect temp (0,0, cx, cy);
//	uw_MapDialogRect(temp);
	uw_MapDialogRect(temp, ((CDialog*)this));

	int borderx = GetSystemMetrics(SM_CXFRAME);
	int bordery = GetSystemMetrics(SM_CYCAPTION);

	int ry = GetSystemMetrics(SM_CYFRAME);
	int rx = GetSystemMetrics(SM_CXFRAME);

	ry = GetSystemMetrics(SM_CYDLGFRAME);
	rx = GetSystemMetrics(SM_CXDLGFRAME);
	int ryy = GetSystemMetrics(SM_CYBORDER);
	int rxx = GetSystemMetrics(SM_CXBORDER);

	if ((type==0) && (fstruct.n_section_fields>0))
	{
		cx = temp.Width() + borderx*2 + 15;
		cy = temp.Height() + bordery +  borderx + 5 - 2;
	}
	else
	{
		if (type==0)
		{
			cx = (temp.Width() + borderx*2 + 8 + 13) + 4;
			cy = (temp.Height() + bordery +  borderx + 5) + 15;
		}
		else
		{
			cx = temp.Width() + borderx*2 + 4;
			cy = temp.Height() + bordery + borderx*2 + 4;
		}
	}
	strcpy(title, fstruct.ud_frmdf.ud_fid);

	CRect rect;
	m_frame->GetWindowRect(&rect);

	ScreenToClient(&rect);
	rect.right = rect.left + cx;
	rect.bottom = rect.top + cy;
	m_frame->MoveWindow(&rect);

	int delta_cx = 0;
	int delta_cy = 0;
	CRect rect1;
	RECT r;
	HRESULT ret = DwmGetWindowAttribute(m_frame->m_hWnd, DWMWA_EXTENDED_FRAME_BOUNDS, &r, sizeof(r));
	if (ret==S_OK)
	{
		GetWindowRect(&rect1);
		delta_cx = (r.right - r.left) - (rect1.right - rect1.left);
		delta_cy = (r.bottom - r.top) - (rect1.bottom - rect1.top);
	}
	SetScrollSizes(MM_TEXT, CSize(rect.Width()+20, rect.Height()+20));

	CNCLFormView *actview = (CNCLFormView *)(m_frame->GetActiveView());
	actview->m_right_form->SetMacroFlag(m_macro_flag);
	actview->LoadFormItem(&fstruct);
	((CNCLFdsnFrame*)UW_Dform_frame)->OnFormTypeBox(type);
	((CNCLFdsnFrame*)UW_Dform_frame)->OnFormType(type);
	m_frame->SetWindowText(fstruct.ud_frmdf.ud_fid);

	if (fstruct.frmhelp!=NULL)
	{
		int len = strlen(fstruct.frmhelp);
		helpstr = new char[len+1];
		strcpy_s(helpstr, len+1, fstruct.frmhelp);
		*helptxt = helpstr;
	}
	else
		*helptxt = NULL;

done:;		
	SetSizeText();
	((CNCLFormView *)(m_frame->GetActiveView()))->m_formchanged = 0;
/*
......once file load, reset undo/redo stack
*/
	actview->m_right_form->Reset_Undo_Redo();
}

#ifdef _DEBUG
void CNCLFormMView::AssertValid() const
{
	CFormView::AssertValid();
}
CNCLFormDoc* CNCLFormMView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CNCLFormDoc)));
	return (CNCLFormDoc*)m_pDocument;
}
#endif //_DEBUG

