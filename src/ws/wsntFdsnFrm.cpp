/************************************************************************
c
c   FILE NAME: wsntFdsnFrm.cpp
c
c	 CONTAINS: 
c		Functions for CNCLFdsnFrame class 
c
c    COPYRIGHT 2014 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c         wsntFdsnFrm.cpp , 26.2
c      DATE AND TIME OF LAST  MODIFICATION
c         04/16/18 , 15:33:36
c**********************************************************************
*/
#include "stdafx.h"
#include "usysdef.h"
#include <direct.h>
#include <dwmapi.h>
#include <afxcmn.h>
#include "afxpriv.h"
#include "wsntres.h"
#include "mfort.h"
#include "nconst.h"

#include "wsntformDoc.h"
#include "wsntFdsnFrm.h"
#include "wsntfrmview.h"
#include "wsntfrmPview.h"
#include "wsntfrmFview.h"
#include "wsntfrmMview.h"
#include "wsntModalFrame.h"
#include "wsntgettxt.h"
#include "wsntsecprop.h"

#define WM_INITIALUPDATE    0x0364
#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" void ncl_macro_formdesgn(char*);
extern char UW_formdlg_font[];
int UW_ftext_cy = 16;
CFrameWnd *UW_Dform_frame = NULL;
extern "C" int ul_break_fname(char *fullname, char* dir, char* fname);
extern "C" int ncl_get_curmac (char *mnam, int *nc);
extern "C" int ncl_getmac_pnum(int *num);
extern "C" int ul_to_lower(char *fname);
extern "C" void ncl_getmac_parms(int *, char*, UM_int2 *, char*, int*, char*, UM_int2*, 
		UM_real8*, UM_real8*, UM_int2 *, UM_int2 *, int*, int*, int*, int*);

/////////////////////////////////////////////////////////////////////////////
// CNCLFdsnFrame

IMPLEMENT_DYNCREATE(CNCLFdsnFrame, CFrameWnd)

BEGIN_MESSAGE_MAP(CNCLFdsnFrame, CFrameWnd)
	//{{AFX_MSG_MAP(CNCLFdsnFrame)
	ON_WM_CREATE()
	ON_WM_SIZE()
	ON_WM_CLOSE()
	ON_WM_DESTROY()
	ON_WM_ENTERSIZEMOVE()
	ON_UPDATE_COMMAND_UI_RANGE(IDC_FORM_LMARGIN, IDC_FORM_SAME_SIZE, OnUpdateAlign)
	ON_UPDATE_COMMAND_UI(ID_FORMDSN_UNDO, OnUpdateUndo)
	ON_UPDATE_COMMAND_UI(ID_FORMDSN_REDO, OnUpdateRedo)
	ON_COMMAND(IDCANCEL, OnClose)
	ON_COMMAND(IDC_FORM_LOAD, OnFormLoad)
	ON_COMMAND(IDC_FORM_SAVE, OnFormSave)
	ON_COMMAND(IDC_FORM_LMARGIN, OnFormLMargin)
	ON_COMMAND(IDC_FORM_RMARGIN, OnFormRMargin)
	ON_COMMAND(IDC_FORM_TMARGIN, OnFormTMargin)
	ON_COMMAND(IDC_FORM_BMARGIN, OnFormBMargin)
	ON_COMMAND(IDC_FORM_SAME_WID, OnFormSameW)
	ON_COMMAND(IDC_FORM_SAME_HGT, OnFormSameH)
	ON_COMMAND(IDC_FORM_SAME_SIZE, OnFormSameSize)
	ON_COMMAND(IDC_FORM_TOGGLE_GUIDES, OnFormToggleGuides)
	ON_COMMAND(IDC_FORM_TOGGLE_GRID, OnFormToggleGrid)
	ON_COMMAND(ID_FORMDSN_UNDO, OnUndo)
	ON_COMMAND(ID_FORMDSN_REDO, OnRedo)
	ON_COMMAND(ID_FORMDSN_HELP, OnFormHelp)
	ON_NOTIFY_EX_RANGE(TTN_NEEDTEXTW, 0, 0xFFFF, OnToolTipText)
	ON_NOTIFY_EX_RANGE(TTN_NEEDTEXTA, 0, 0xFFFF, OnToolTipText)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CNCLFdsnFrame::OnUpdateAlign(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(m_align);
}

void CNCLFdsnFrame::OnUpdateUndo(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(m_upd_undo);
}

void CNCLFdsnFrame::OnUpdateRedo(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(m_upd_redo);
}

/////////////////////////////////////////////////////////////////////////////
// CNCLFdsnFrame construction/destruction

/***********************************************************************
c
c   FUNCTION: CNCLFdsnFrame()
c
c              Constructor of class CNCLFdsnFrame
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

CNCLFdsnFrame::CNCLFdsnFrame()
{
	m_fview = NULL;
	m_mview = NULL;
	m_pview = NULL;
	m_rep = 0;
	m_formchanged = 0;
	m_macro_flag = 0;
	m_helptext = NULL;
	m_type = 0;
/*
.....Create user key Accelerator
*/
	m_dlg_accel = (ACCEL *)malloc(20*sizeof(ACCEL));
	m_accelnum = 0;
	m_toggle = 0;
	m_align = 0;
	m_upd_undo = 0;
	m_upd_redo = 0;
	m_filename[0] = '\0';
	m_cx1 = -1;
	m_cx2 = -1; 
	m_cx3 = -1;
	m_sec_flag = 0;
	m_delx = m_dely = 0;
}

CNCLFdsnFrame::~CNCLFdsnFrame()
{
	UW_Dform_frame = NULL;
}
void CNCLFdsnFrame::DeleteFrame()
{
	((CNCLFormMView *)m_mview)->DeleteFrame();
}
/***********************************************************************
c
c   FUNCTION: OnCreate(LPCREATESTRUCT lpCreateStruct)
c
c              overwrite function to create a dialog bar. 
c
c   INPUT:  lpCreateStruct: Points to a CREATESTRUCT structure that 
c			contains information about the CWnd object being created.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CNCLFdsnFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	if (!m_wndToolBar.CreateEx(this, TBSTYLE_FLAT, WS_CHILD | WS_VISIBLE | CBRS_TOP
		| CBRS_TOOLTIPS | CBRS_FLYBY) 
		|| !m_wndToolBar.LoadToolBar(IDR_POPUPFRAME))
	{
		TRACE0("Failed to create toolbar\n");
		return -1; 
	}	
	UW_Dform_frame = this;

	int delta_cx = 0;
	int delta_cy = 0;
	RECT r, windowRect;
	GetWindowRect(&windowRect);
	HRESULT ret = DwmGetWindowAttribute(m_hWnd, DWMWA_EXTENDED_FRAME_BOUNDS, &r, sizeof(r));
	if (ret==S_OK)
	{
		delta_cx = (r.right - r.left) - (windowRect.right - windowRect.left);
		delta_cy = (r.bottom - r.top) - (windowRect.bottom - windowRect.top);
	}

	int mode;
	SIZE sizeTotal1, sizeTotal2, sizeTotal3, sizePage, sizeLine, sizeFrame;
	CRect rect1, rect2, rect3, rect, frect;
	m_fview->GetDeviceScrollSizes(mode, sizeTotal1, sizePage, sizeLine);
	sizeFrame.cx = sizeTotal1.cx;
	sizeFrame.cy = sizeTotal1.cy;

	m_mview->GetDeviceScrollSizes(mode, sizeTotal2, sizePage, sizeLine);
	((CNCLFormMView *)m_mview)->m_frame->GetWindowRect(&frect);
	m_mview->SetScrollSizes(MM_TEXT, CSize(frect.Width()+20, frect.Height()+20));

	sizeFrame.cx += sizeTotal2.cx;
	if (sizeTotal2.cy>sizeFrame.cy)
		sizeFrame.cy = sizeTotal2.cy;

	m_pview->GetDeviceScrollSizes(mode, sizeTotal3, sizePage, sizeLine);
	sizeFrame.cx += sizeTotal3.cx;

	m_fview->GetWindowRect(&rect1);
	m_mview->GetWindowRect(&rect2);
	int split_cx = rect2.left - rect1.right;

	GetWindowRect(&rect);
	rect.right = rect.left + sizeFrame.cx + (rect1.left - rect.left) + 2*GetSystemMetrics(SM_CXFRAME)
			+ 2*split_cx;
	rect.bottom = rect.top + sizeFrame.cy + (rect1.top - rect.top) + GetSystemMetrics(SM_CYFRAME)
				+ GetSystemMetrics(SM_CYCAPTION);
	MoveWindow(&rect);

	m_wndSplitter.SetRowInfo(0, 800, 100);
	m_wndSplitter.SetColumnInfo(0, sizeTotal1.cx, 50);
	m_wndSplitter.SetColumnInfo(1, sizeTotal2.cx, 50);
	m_wndSplitter.SetColumnInfo(2, sizeTotal3.cx, 50);
	m_wndSplitter.RecalcLayout();
	m_fview->GetWindowRect(&rect);
	m_wndSplitter.ScreenToClient(&rect);
/*
......resize the window according to view size
*/
	m_dlg_accel[0].cmd = ID_FORMDSN_UNDO;
	m_dlg_accel[0].fVirt = FVIRTKEY|FCONTROL;
	m_dlg_accel[0].key = 'Z';
	m_dlg_accel[1].cmd = ID_FORMDSN_REDO;
	m_dlg_accel[1].fVirt = FVIRTKEY|FCONTROL;
	m_dlg_accel[1].key = 'Y';
	m_accelnum = 2;
	if (m_accelnum!=0)
		m_accel = CreateAcceleratorTable(m_dlg_accel, m_accelnum);

	EnableAlignBtn(0);
	EnableRedoBtn(0);
	EnableUndoBtn(0);

	CNCLFormFView *fview = (CNCLFormFView *)m_fview;
	((CEdit*)(fview->GetDlgItem(IDC_EDIT1)))->SetWindowText("Design Form");

	if (m_filename[0]!='\0')
		FormLoad(m_filename);
	else
	{
		((CNCLFormPView *)m_pview)->SetDtype(-2);
		((CNCLFormPView *)m_pview)->filldata();
	}
	m_formchanged = 0;
	int cxm;
	m_wndSplitter.GetColumnInfo(0, m_cx1, cxm);
	m_wndSplitter.GetColumnInfo(1, m_cx2, cxm);
	m_wndSplitter.GetColumnInfo(2, m_cx3, cxm);
	return 0;
}
void CNCLFdsnFrame::MoveMViewWindow(CRect &vrect)
{
	CRect rect;
	m_wndSplitter.ScreenToClient(&vrect);
	m_mview->GetWindowRect(&rect);
	m_wndSplitter.ScreenToClient(&rect);
	rect.right = rect.left + vrect.Width();
	rect.bottom = rect.top + vrect.Height();
	m_mview->MoveWindow(&rect, 0);
	m_wndSplitter.RecalcLayout();
}
/***********************************************************************
c
c   FUNCTION: PreCreateWindow(CREATESTRUCT& cs)
c
c         Called by the framework before the creation of the 
c			Windows window attached to this CWnd object.
c			Never call this function directly.
c
c   INPUT:  cs:   A CREATESTRUCT structure.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNCLFdsnFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	cs.cx = 550;
	cs.cy = 400;
	cs.x = 100;
	cs.y = 100;
	return CFrameWnd::PreCreateWindow(cs);
}

/***********************************************************************
c
c   FUNCTION: OnCreateClient(LPCREATESTRUCT lpcs,
c				 CCreateContext* pContext)
c			Create a splitter window
c         Called by the framework during the execution of OnCreate 
c			Never call this function directly.
c
c   INPUT:  lpcs:   A pointer to a Windows CREATESTRUCT structure.
c			pContext:   A pointer to a CCreateContext structure.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNCLFdsnFrame::OnCreateClient(LPCREATESTRUCT lpcs,
	 CCreateContext* pContext)
{
	if (!m_wndSplitter.CreateStatic(this, 1, 3))
	{
		TRACE0("Failed to CreateStaticSplitter\n");
		return FALSE;
	}

	if (!m_wndSplitter.CreateView(0, 0,
		RUNTIME_CLASS(CNCLFormFView), CSize(200, 250), pContext))
	{
		TRACE0("Failed to create second pane\n");
		return FALSE;
	}

	if (!m_wndSplitter.CreateView(0, 1,
		RUNTIME_CLASS(CNCLFormMView), CSize(400, 250), pContext))
	{
		TRACE0("Failed to create first pane\n");
		return FALSE;
	}

	if (!m_wndSplitter.CreateView(0, 2,
		RUNTIME_CLASS(CNCLFormPView), CSize(550, 100), pContext))
	{
		TRACE0("Failed to create first pane\n");
		return FALSE;
	}
	m_fview = (CNCLFormFView *)m_wndSplitter.GetPane(0,0);
	((CNCLFormFView *)m_fview)->SetParentFrm(this);
	((CNCLFormFView *)m_fview)->SetMacroFlag(m_macro_flag);
	m_mview = (CNCLFormMView *)m_wndSplitter.GetPane(0,1);
	((CNCLFormMView *)m_mview)->SetParentFrm(this);
	((CNCLFormMView *)m_mview)->SetMacroFlag(m_macro_flag);
	m_pview = (CNCLFormPView *)m_wndSplitter.GetPane(0,2);
	((CNCLFormPView *)m_pview)->SetParentFrm(this);
	((CNCLFormPView *)m_pview)->SetMacroFlag(m_macro_flag);
	((CNCLFormPView *)m_pview)->CreateColorBut();
	((CNCLFormMView *)m_mview)->CreateFrame();
	return TRUE;
}
	
/***********************************************************************
c
c   SUBROUTINE:  SetFileflag(char *filename, int flag)
c   FUNCTION:  Set the form design filename and mocro flag
c   INPUT:  filename: input filename
c				flag: macro flag
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFdsnFrame::SetFileflag(char *filename, int flag)
{
	int pclr;
	strcpy(m_filename, filename);
	m_macro_flag = flag;
	if (m_macro_flag)
	{
		int nc, indx;
		char mcname[64];
		char label[NCL_MAX_LABEL+15];
		UM_int2 clas, geoflag, prsvalue, lenvalue;
		char prmptr[41], wrdlist[481];
		int counter, substr, pflag, psel;
		UM_real8 min, max;
/*
......get current macro and parameters
*/
		ncl_get_curmac (mcname, &nc);
		ncl_getmac_pnum(&m_macro_parmno);
		if (m_macro_parmno>0)
		{
			m_macro_parms = (char**) malloc(m_macro_parmno*sizeof (char*));
			for (int i=0; i<m_macro_parmno;i++)
			{
				m_macro_parms[i] = (char*) malloc(65*sizeof(char));
				indx = i + 1;
				ncl_getmac_parms(&indx, label, &clas, prmptr, &counter, wrdlist,
					&geoflag, &min, &max, 
					&prsvalue, &lenvalue, &substr, &pflag, &psel, &pclr);
				strcpy(m_macro_parms[i], label);
			}
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnFormHelp
c   FUNCTION:  This function called when Form Help function is called
c				it display a text window to input form help text
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFdsnFrame::OnFormHelp()
{
	CNCLGetText *dlg = new CNCLGetText(this, "Input Form Help Text Here:");
	dlg->SetTextString(m_helptext);
	dlg->DoModal();
	CString helptext;
	dlg->GetTextString(helptext);
	delete dlg;
	delete m_helptext;
	int len = helptext.GetLength();
	m_helptext = new char[len+1];
	strcpy_s(m_helptext, len+1, helptext.GetBuffer(len+1));
	m_formchanged = 1;
}
/***********************************************************************
c
c   SUBROUTINE:  OnFormToggleGuides
c   FUNCTION:  This function called when Toggle Guide function is called
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFdsnFrame::OnFormToggleGuides()
{
	if (m_toggle>=2)
		m_toggle -= 2;
	else
		m_toggle += 2;
	UpdateWindow();
	RedrawWindow();

	CToolBarCtrl &toolctl = m_wndToolBar.GetToolBarCtrl();
	if (m_toggle>=2)
		toolctl.CheckButton(IDC_FORM_TOGGLE_GUIDES, 1);
	else
		toolctl.CheckButton(IDC_FORM_TOGGLE_GUIDES, 0);
}
/***********************************************************************
c
c   SUBROUTINE:  OnFormToggleGrid
c   FUNCTION:  This function called when Toggle Grid function is called
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFdsnFrame::OnFormToggleGrid()
{
	if (m_toggle>=1)
		m_toggle -= 1;
	else
		m_toggle += 1;
	UpdateWindow();
	RedrawWindow();
	CToolBarCtrl &toolctl = m_wndToolBar.GetToolBarCtrl();
	if ((m_toggle==1)||(m_toggle==3))
		toolctl.CheckButton(IDC_FORM_TOGGLE_GRID, 1);
	else
		toolctl.CheckButton(IDC_FORM_TOGGLE_GRID, 0);
}

/*********************************************************************
**    I_FUNCTION:  SetSizeText()
**      Set the size text into a size fields of the dialog
**		
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFdsnFrame::SetSizeText()
{
	if (m_mview==NULL)
		return;
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	if (mview->m_frame==NULL)
		return;
	mview->m_frame->GetWindowRect(&m_frmrect);
	char sizestr[40];
	sprintf(sizestr, "%d,%d", m_frmrect.Width(), m_frmrect.Height());
	CNCLFormFView *fview = (CNCLFormFView *)m_fview;
	((CEdit*)(fview->GetDlgItem(IDC_EDIT2)))->SetWindowText(sizestr);
}
CView* CNCLFdsnFrame::GetPView()
{
	return m_pview;
}

/***********************************************************************
c
c   FUNCTION: OnFormSave()
c
c		callback for form save message
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::OnFormSave()
{
	FormSave();
	m_formchanged = 0;
}

/***********************************************************************
c
c   FUNCTION: OnFormLoad()
c
c		callback for form load message
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::OnFormLoad()
{
/*
......for macro form, first check then load in MACRO select form again
*/
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	if (mview->m_frame==NULL)
		return;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	int view_chg = actview->m_formchanged;

	int result, status;
	char msg[500], title[82];
	if (m_formchanged||view_chg)
	{
/*
......prompt to see if user want to save the form
*/
		CNCLFormFView *fview = (CNCLFormFView *)m_fview;
		((CEdit*)(fview->GetDlgItem(IDC_EDIT1)))->GetWindowText(title, 80);
		sprintf (msg, "Do you want to save the changes to %s", title);
		result = MessageBox(msg, "Save Changes", MB_YESNOCANCEL);
		if (result==IDYES)
		{
			status = FormSave();
			if (status==-1)
				return;
		}
		else if (result==IDCANCEL)
			return;
	}
	if (m_macro_flag==0)
	{
		char filename[1024];
		if (m_filename[0]!='\0')
		{
			strcpy(filename, m_filename);
			m_filename[0] = '\0';
		}
		else
			filename[0] = '\0';
		FormLoad(m_filename);
		if ((m_filename[0]=='\0')&&(filename[0]!='\0'))
			strcpy(m_filename, filename);
		return;
	}
	m_rep = 1;
	CNCLModalFrame::End(this, -1);
}

/***********************************************************************
c
c   FUNCTION: FormLoad(char *infile)
c
c		load form file into the form frame
c   INPUT: infile: form file to be loaded
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::FormLoad(char *infile)
{
	if (m_mview==NULL)
		return;
	char title[256], *helptxt = NULL;
	((CNCLFormMView *)m_mview)->FormLoad(infile, &helptxt, title);
	if (m_helptext!=NULL)
		delete m_helptext;
	m_helptext = helptxt;
	CNCLFormFView *fview = (CNCLFormFView *)m_fview;
	((CEdit*)(fview->GetDlgItem(IDC_EDIT1)))->SetWindowText(title);
	m_formchanged = 0;
}
/***********************************************************************
c
c   FUNCTION: AdjuctMoveWindows(int flag)
c
c		Adjust selected windows position
c   INPUT: flag: 1: FormLMargin
c					2: FormRMargin
c					3: FormTMargin
c					4: OnFormBMargin
c					5: FormSameWidth
c					6: FormSameHeight
c					7: FormSameSize
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::AdjuctMoveWindows(int flag)
{
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	if (mview->m_frame==NULL)
		return;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->m_right_form->AdjuctMoveWindows(flag);
}

/***********************************************************************
c
c   FUNCTION: OnFormLMargin()
c
c		callback for form left Margin message
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::OnFormLMargin()
{
	AdjuctMoveWindows(1);
}

/***********************************************************************
c
c   FUNCTION: OnFormRMargin()
c
c		callback for form right Margin message
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::OnFormRMargin()
{
	AdjuctMoveWindows(2);
}

/***********************************************************************
c
c   FUNCTION: OnFormTMargin()
c
c		callback for form Top Margin message
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::OnFormTMargin()
{
	AdjuctMoveWindows(3);
}

/***********************************************************************
c
c   FUNCTION: OnFormBMargin()
c
c		callback for form bottom Margin message
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::OnFormBMargin()
{
	AdjuctMoveWindows(4);
}

/***********************************************************************
c
c   FUNCTION: OnFormSameW()
c
c		callback for form Same Width message
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::OnFormSameW()
{
	AdjuctMoveWindows(5);
}

/***********************************************************************
c
c   FUNCTION: OnFormSameH()
c
c		callback for form Same Height message
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::OnFormSameH()
{
	AdjuctMoveWindows(6);
}

/***********************************************************************
c
c   FUNCTION: OnFormSameSize()
c
c		callback for form Same Size message
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::OnFormSameSize()
{
	AdjuctMoveWindows(7);
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
int CNCLFdsnFrame::FormSave()
{
	if (m_mview==NULL)
		return 0;
	((CNCLFormMView *)m_mview)->FormSave(m_filename, m_helptext);
}
/***********************************************************************
c
c   FUNCTION: OnToolTipText(UINT, NMHDR* pNMHDR, LRESULT* pResult)
c
c		Called when need tool tip text
c   INPUT: none
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNCLFdsnFrame::OnToolTipText(UINT, NMHDR* pNMHDR, LRESULT* pResult)
{
    ASSERT(pNMHDR->code == TTN_NEEDTEXTA || pNMHDR->code == TTN_NEEDTEXTW);

    TOOLTIPTEXTA* pTTTA = (TOOLTIPTEXTA*)pNMHDR;
    TOOLTIPTEXTW* pTTTW = (TOOLTIPTEXTW*)pNMHDR;
    TCHAR szFullText[512];
    CString strTipText;
    UINT nID = pNMHDR->idFrom;

    if (pNMHDR->code == TTN_NEEDTEXTA && (pTTTA->uFlags & TTF_IDISHWND) ||
        pNMHDR->code == TTN_NEEDTEXTW && (pTTTW->uFlags & TTF_IDISHWND))
    {
        nID = ::GetDlgCtrlID((HWND)nID);
    }

    if (nID != 0)
    {
        AfxLoadString(nID, szFullText);
        strTipText=szFullText;

#ifndef _UNICODE
        if (pNMHDR->code == TTN_NEEDTEXTA)
        {
            lstrcpyn(pTTTA->szText, strTipText, sizeof(pTTTA->szText));
        }
        else
        {
            _mbstowcsz(pTTTW->szText, strTipText, sizeof(pTTTW->szText));
        }
#else
        if (pNMHDR->code == TTN_NEEDTEXTA)
        {
            _wcstombsz(pTTTA->szText, strTipText,sizeof(pTTTA->szText));
        }
        else
        {
            lstrcpyn(pTTTW->szText, strTipText, sizeof(pTTTW->szText));
        }
#endif

        *pResult = 0;

        ::SetWindowPos(pNMHDR->hwndFrom, HWND_TOP, 0, 0, 0, 0,
            SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOMOVE|SWP_NOOWNERZORDER);
        
        return TRUE;
    }
    return FALSE;
}


/////////////////////////////////////////////////////////////////////////////
// CNCLFdsnFrame diagnostics

#ifdef _DEBUG
void CNCLFdsnFrame::AssertValid() const
{
	CFrameWnd::AssertValid();
}

void CNCLFdsnFrame::Dump(CDumpContext& dc) const
{
	CFrameWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CNCLFdsnFrame message handlers
void CNCLFdsnFrame::OnClose() 
{
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	if (mview->m_frame==NULL)
		return;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	int view_chg = actview->m_formchanged;
	int result, status;
	char msg[500], title[82];
	if (m_formchanged||view_chg)
	{
/*
......prompt to see if user want to save the form
*/
		CNCLFormFView *fview = (CNCLFormFView *)m_fview;
		((CEdit*)(fview->GetDlgItem(IDC_EDIT1)))->GetWindowText(title, 80);
		sprintf (msg, "Do you want to save the changes to %s", title);
		result = MessageBox(msg, "Save Changes", MB_YESNOCANCEL);
		if (result==IDYES)
		{
			status = FormSave();
			if (status==-1)
				return;
		}
		else if (result==IDCANCEL)
			return;
	}
	CNCLModalFrame::End(this, -1);
}
void CNCLFdsnFrame::OnDestroy() 
{
	CFrameWnd::OnDestroy();
}

/***********************************************************************
c
c   SUBROUTINE:  ChangePromptType(int type)
c   FUNCTION:  This function change the item string Type
c   INPUT:  
c				type: 0, change the prompt from button to lable
c						1, change the prompt from lable to button
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFdsnFrame::ChangePromptType(int type)
{
	if (m_mview==NULL)
		return;
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	if (mview->m_frame==NULL)
		return;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	if (actview->m_right_form==NULL)
		return;
	actview->m_right_form->ChangePromptType(type);
}
/***********************************************************************
c
c   FUNCTION: SaveProperty()
c		Save the property data into the select window
c   INPUT:  none
c			
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::SaveProperty()
{
/*when button prompt, type=4, should be 3, thus it resize editbox instead of button
*/
	if (m_mview==NULL)
		return;
	CNCLFormPView *pview = (CNCLFormPView *)m_pview;
	CNCLFormProp *prop_dlg = pview->GetPropertyPage();
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	if (mview->m_frame==NULL)
		return;
	CNCLFormView *fview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	CNCLFormProp *page = fview->GetPropertyPage();
	page->CopyPropertyPage(prop_dlg);
// not free here, this prop_dlg is member of pview
//	prop_dlg->free_picdata();
	if (fview->m_right_form==NULL)
		return;
	fview->m_right_form->SaveProperty();
	m_formchanged = 1;
}
void CNCLFdsnFrame::SaveUndoItem(int action)
{
	if (m_mview==NULL)
		return;
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	if (mview->m_frame==NULL)
		return;
	CNCLFormView *fview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	if (fview->m_right_form==NULL)
		return;
	fview->m_right_form->SaveUndoItem2(action);
	m_formchanged = 1;
}

/***********************************************************************
c
c   SUBROUTINE:  OnUndo()
c   FUNCTION:  This function called when undo function is called
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFdsnFrame::OnUndo()
{
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	if (mview->m_frame==NULL)
		return;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->OnUndo();
}

/***********************************************************************
c
c   SUBROUTINE:  OnRedo()
c   FUNCTION:  This function called when undo function is called
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFdsnFrame::OnRedo()
{
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	if (mview->m_frame==NULL)
		return;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->OnRedo();
}

/***********************************************************************
c
c   SUBROUTINE:  OnFormType()
c   FUNCTION:  This function called form type combo selection is changed
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFdsnFrame::OnFormType(int type)
{
	m_type = type;
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->SetType(m_type);
	actview->SizeForm(m_type);
	SetSizeText();
	m_formchanged = 1;
}

void CNCLFdsnFrame::OnFormTypeBox(int type)
{
	CNCLFormFView *fview = (CNCLFormFView *)m_fview;
	((CComboBox*)(fview->GetDlgItem(IDC_COMBO1)))->SetCurSel(type);
	if (type==0)
	{
//		((CComboBox*)(fview->GetDlgItem(IDC_COMBO2)))->EnableWindow(1);
		((CButton*)(fview->GetDlgItem(IDC_CHECK6)))->EnableWindow(1);
	}
	else
	{
//		((CComboBox*)(fview->GetDlgItem(IDC_COMBO2)))->EnableWindow(0);
		((CButton*)(fview->GetDlgItem(IDC_CHECK6)))->EnableWindow(0);
	}
}
void CNCLFdsnFrame::SetSecFlagVal(int flag)
{
	m_sec_flag = flag;
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->SetSecFlag(m_sec_flag);
////
	CNCLFormFView *fview = (CNCLFormFView *)m_fview;
	if (m_sec_flag==1)
	{
		((CComboBox*)(fview->GetDlgItem(IDC_COMBO1)))->SetCurSel(0);
//		((CComboBox*)(fview->GetDlgItem(IDC_COMBO2)))->SetCurSel(1);
		((CButton*)(fview->GetDlgItem(IDC_CHECK6)))->SetCheck(1);
	}
	else
	{
//		((CComboBox*)(fview->GetDlgItem(IDC_COMBO2)))->SetCurSel(0);
		((CButton*)(fview->GetDlgItem(IDC_CHECK6)))->SetCheck(0);
	}
}

void CNCLFdsnFrame::OnFormSection(int type)
{
	m_sec_flag = type;
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	CPoint scrpos = mview->GetScrollPosition();

	actview->SetSection(m_sec_flag);
	if (type)
		m_type = 0;
	actview->SizeForm(m_type);
	SetSizeText();
	m_formchanged = 1;

	CRect frect, vrect;
	mview->m_frame->GetWindowRect(&frect);
	vrect.left = frect.left - 10;
	vrect.right = frect.right + 10;
	vrect.top = frect.top - 10;
	vrect.bottom = frect.bottom + 10;
	MoveMViewWindow(vrect);

	mview->SetScrollSizes(MM_TEXT, CSize(frect.Width()+20, frect.Height()+20));
	mview->ScrollToPosition(scrpos);
	mview->m_frame->RedrawWindow();
/*
......once this section chnaged, reset undo/redo stack
*/
	actview->m_right_form->Reset_Undo_Redo();
}
/***********************************************************************
c
c   SUBROUTINE:  OnTitleChange(CString sText)
c   FUNCTION:  This function called form title is changed
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFdsnFrame::OnTitleChange(CString sText)
{
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	mview->m_frame->SetWindowText(sText);
}

/***********************************************************************
c
c   SUBROUTINE:  OnFormSizeChange()
c   FUNCTION:  This function called form size input is changed
c   INPUT:  none
c				
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFdsnFrame::OnFormSizeChange(int cx, int cy)
{
	CString text;
/*
.....accept size information and reset form
*/
	CRect rect;
	rect.left = 10;
	rect.top = 10;
	rect.right = rect.left + cx;
	rect.bottom = rect.top + cy;
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	mview->m_frame->MoveWindow(&rect);

	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->SizeForm(m_type);
	SetSizeText();
	m_formchanged = 1;
}
void CNCLFdsnFrame::OnCheckMacro1()
{
	int checked = GetCheckedMacro(0);
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->m_right_form->OnCheckMacro(1, checked);
	m_formchanged = 1;
}

void CNCLFdsnFrame::OnCheckMacro2()
{
	int checked = GetCheckedMacro(1);
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->m_right_form->OnCheckMacro(2, checked);
	m_formchanged = 1;
}
void CNCLFdsnFrame::OnCheckMacro3()
{
	int checked = GetCheckedMacro(2);
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->m_right_form->OnCheckMacro(3, checked);
	m_formchanged = 1;
}
void CNCLFdsnFrame::OnCheckMacro4()
{
	int checked = GetCheckedMacro(3);
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->m_right_form->OnCheckMacro(4, checked);
	m_formchanged = 1;
}

void CNCLFdsnFrame::OnCheckMacro5()
{
	int checked = GetCheckedMacro(5);
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->m_right_form->OnCheckMacro(6, checked);
	m_formchanged = 1;
}

void CNCLFdsnFrame::OnCheckMacro6()
{
	int checked = GetCheckedMacro(4);
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->m_right_form->OnCheckMacro(5, checked);
	m_formchanged = 1;
}

int CNCLFdsnFrame::GetCheckedMacro(int itemno)
{
	return ((CNCLFormFView *)m_fview)->GetCheckedMacro(itemno);
}

void CNCLFdsnFrame::SetMacroActive(int itemno, int active)
{
	((CNCLFormFView *)m_fview)->SetMacroActive(itemno, active);
}

/***********************************************************************
c
c   FUNCTION: EnableAlignBtn(int flag)
c
c		In this function enable all align button
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::EnableAlignBtn(int flag)
{
/*
.....the following not work for frame, use UPDATE_COMMAND_UI callbacks
.....
	CToolBarCtrl &toolctl = m_wndToolBar.GetToolBarCtrl();
	toolctl.EnableButton(IDC_FORM_LMARGIN, flag);
	toolctl.EnableButton(IDC_FORM_RMARGIN, flag);
	toolctl.EnableButton(IDC_FORM_TMARGIN, flag);
	toolctl.EnableButton(IDC_FORM_BMARGIN, flag);
	toolctl.EnableButton(IDC_FORM_SAME_WID, flag);
	toolctl.EnableButton(IDC_FORM_SAME_HGT, flag);
	toolctl.EnableButton(IDC_FORM_SAME_SIZE, flag);
*/
	m_align = flag;
}

/***********************************************************************
c
c   FUNCTION: EnableUndoBtn(int flag)
c
c		In this function enable Undo button
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::EnableUndoBtn(int flag)
{
	m_upd_undo = flag;
}

/***********************************************************************
c
c   FUNCTION: EnableRedoBtn(int flag)
c
c		In this function enable Undo button
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::EnableRedoBtn(int flag)
{
	m_upd_redo = flag;
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
void CNCLFdsnFrame::UpdatePropertySize(int cx, int cy)
{
	((CNCLFormPView *)m_pview)->UpdatePropertySize(cx, cy);
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
void CNCLFdsnFrame::OpenPropertyPage(CNCLFormProp *prop_dlg, int flag)
{
	if (flag==0)
	{
		if (m_pview->IsWindowVisible()==0)
			return;
	}
	if (flag==0)
	{
/*
......filling data into the field
*/
		CNCLFormMView *mview = (CNCLFormMView *)m_mview;
		CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
		CNCLFormProp * page = actview->GetPropertyPage();
		page->CopyPropertyPage(prop_dlg);
		((CNCLFormPView *)m_pview)->CopyPropertyPage(prop_dlg);
		((CNCLFormPView *)m_pview)->filldata();
	}
}
void CNCLFdsnFrame::Convert_pic_rect(float in_rect[4], float out_rect[4], int flag)
{
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->m_right_form->Convert_pic_rect(in_rect, out_rect, flag);
}
void CNCLFdsnFrame::LoadActiveHotSpot(int dtype, int itype, int inputno, int hpnum)
{
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->m_right_form->LoadActiveHotSpot(dtype, itype, inputno, hpnum);
}

/***********************************************************************
c
c   FUNCTION: UpdatePropertyPos(int x, int y)
c
c		Update the property pos edit field
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFdsnFrame::UpdatePropertyPos(int x, int y)
{
	((CNCLFormPView *)m_pview)->UpdatePropertyPos(x, y);
}

void CNCLFdsnFrame::UpdatePropertyHSPTSize(CRect sizerec,float prect[4])
{
	((CNCLFormPView *)m_pview)->UpdatePropertyHSPTSize(sizerec, prect);
}
void CNCLFdsnFrame::UpdatePropertyPicture(CString pic_label, CString pic_tooltip,
				float pic_rect[4], CString pic_params, int indx) 
{
	((CNCLFormPView *)m_pview)->UpdatePropertyPicture(pic_label, pic_tooltip, 
		pic_rect, pic_params, indx);
}

void CNCLFdsnFrame::OnEnterSizeMove()
{
	int cxm;
	m_wndSplitter.GetColumnInfo(0, m_cx1, cxm);
	m_wndSplitter.GetColumnInfo(1, m_cx2, cxm);
	m_wndSplitter.GetColumnInfo(2, m_cx3, cxm);
}

/***********************************************************************
**
**   FUNCTION: OnSize(UINT nType, int cx, int cy) 
**
**		call this member function 
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
void CNCLFdsnFrame::OnSize(UINT nType, int cx, int cy) 
{
	int cx1, cx2, cx3, cxm1, cxm2, cxm3;
	CFrameWnd::OnSize(nType, cx, cy);	
/*
.....when on size, we want to change the middle area size but keep the
.....left and right size the same
*/
	if (m_fview==NULL)
		return;
	CRect lrect, mrect, rrect;
	m_wndSplitter.GetColumnInfo(0, cx1, cxm1);
	m_wndSplitter.GetColumnInfo(1, cx2, cxm2);
	m_wndSplitter.GetColumnInfo(2, cx3, cxm3);
	if (m_cx1==-1)
	{
		m_total_dx = cx - (cx1+cx2+cx3);
	}
	if (m_cx1==-1)
		return;
	if (m_cx1!=cx1)
		m_wndSplitter.SetColumnInfo(0, m_cx1, 50);
	if (m_cx3!=cx3)
		m_wndSplitter.SetColumnInfo(2, m_cx3, 50);
	m_cx2 = cx - (m_cx1+m_cx3) - m_total_dx;
	if (m_cx2!=cx2)
		m_wndSplitter.SetColumnInfo(1, m_cx2, 50);
	if ((m_cx1!=cx1)||(m_cx3!=cx3)||(m_cx2!=cx2))
		m_wndSplitter.RecalcLayout();
}
int CNCLFdsnFrame::CheckPos(int &x, int &y)
{
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	if (mview->m_frame==NULL)
		return -1;
	CNCLFormView *fview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	if (fview->m_right_form==NULL)
		return -1;
	return fview->m_right_form->CheckPos(x, y);
}
void CNCLFdsnFrame::reset_selvalue()
{
	CNCLFormMView *mview = (CNCLFormMView *)m_mview;
	CNCLFormView *actview = (CNCLFormView *)(mview->m_frame->GetActiveView());
	actview->m_right_form->reset_selvalue();
}

void CNCLFdsnFrame::MoveFormFrameRect(int left, int right, int top, int bottom)
{
	CRect rect;
	((CNCLFormMView *)m_mview)->m_frame->GetWindowRect(&rect);
	((CNCLFormMView *)m_mview)->ScreenToClient(&rect);
	rect.left += left;
	rect.right += right;
	rect.top += top;
	rect.bottom += bottom;
	((CNCLFormMView *)m_mview)->m_frame->MoveWindow(&rect,1);
}

void  CNCLFdsnFrame::Resize_form_frame(int dx, int dy)
{
	CRect rect;
	((CNCLFormMView *)m_mview)->m_frame->GetWindowRect(&rect);
	((CNCLFormMView *)m_mview)->ScreenToClient(&rect);
	rect.right += dx;
	rect.bottom += dy;
	((CNCLFormMView *)m_mview)->m_frame->MoveWindow(&rect,1);
/*
......set scrolling size
*/
	((CNCLFormMView *)m_mview)->SetScrollSizes(MM_TEXT, CSize(rect.Width()+20, rect.Height()+20));
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
BOOL CNCLFdsnFrame::PreTranslateMessage(MSG* msg)
{
	HWND hWnd = (HWND)*this; 
	UINT message = msg->message;	

	if (m_accelnum==0)
		return CFrameWnd::PreTranslateMessage( msg );

	if (msg->message==WM_KEYDOWN)
		msg->message = msg->message;

	if (TranslateAccelerator(hWnd, m_accel, msg))
	{
		return TRUE;
	}
	else
	{
		return CFrameWnd::PreTranslateMessage( msg );
	}
}
