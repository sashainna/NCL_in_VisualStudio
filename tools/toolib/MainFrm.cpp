/************************************************************************
c
c   FILE NAME: MainFrm.cpp
c
c	 CONTAINS: 
c		Functions for CMainFrame class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       MainFrm.cpp , 25.7
c    DATE AND TIME OF LAST  MODIFICATION
c       10/27/16 , 14:30:01
c
c**********************************************************************
*/
// MainFrm.cpp : implementation of the CMainFrame class
//

#include "toolibstdafx.h"
#include "toolib.h"
#include "xenv1.h"

#include "MainFrm.h"
#include "toolchildvw.h"
#include "toolibcc.h"
#include "DefineDlg.h"
#include "ModListDlg.h"
#include "OperateDlg.h"
#include "ParmsDlg.h"
#include "ToolFindDlg.h"
#include "toolibdata.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

static int Tool_cx, Tool_cy;
extern "C" double NCL_version;
extern "C" int Tool_modify;
extern "C" int toolc_savlib(char*);
extern "C" int tool_loadbat(char*, int*);
extern "C" int toolc_list(char*, int);
extern "C" int tool_createbat(char*);
extern "C" int toolc_del_toollist();
extern "C" int tool_mfyesno(char*, char*, int);
extern "C" void uu_free( char* );
typedef struct Modlisting {
    int heading;
    int toolnum;
	int cutparm;
	int pcutparm;
	int dispparm;
	int loadcom;
	int optcom;
	int paramslb;
	int psymbol;
	int pshank;
	int pholder;
} ListStruct;

ListStruct tl_modlist = {1,1,1,1,1,1,1,1,1,1,1};
CMainFrame *TOOL_main_frame;
CSize tool_sizeTotal1,  tool_sizeTotal2, tool_sizeTotal3,
	tool_sizeTotal4;
extern "C" void tool_load(char*, int*);
extern "C" int tool_addrec(struct TL_tooldata_rec *tool_data);
extern "C" int tool_delrec(double toolnum);
extern "C" struct TL_toolhead_rec Tool_head;
extern "C" struct TL_tooldata_rec Tool_current_data;
extern "C" int ncl_tool_reinitlib();
extern "C" int ncl_del_toollist();
extern "C" int ncl_gettool_head (struct TL_toolhead_rec*);
extern "C" int ncl_getcurrent_tooldata (struct TL_tooldata_rec*);
extern "C" int uu_toolmalloc_term();
extern "C" char * uu_malloc(int);
extern "C" void uu_free( char* );
extern "C" int tool_strip_blanks (char*, int*);	
extern "C" int ncdate (char*);
extern "C" int ftim (char*);
extern "C" int tool_mfmsg_box(CWnd* parent, char *title, char *msg, int flag);
extern "C" int ul_get_full_dir(char *indir, char *fullname);
extern "C" void ul_break_fname(char *fullname,char *dir,char *fname);
extern "C" int ux_decompose_path(char *name, char *farea, char *fname, int options);
extern "C" void tool_get_filename(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag,
	char *paths, char *path_des);

static int Tool_new = 0;

IMPLEMENT_DYNAMIC(CViewExSplitWnd, CSplitterWnd)

CViewExSplitWnd::CViewExSplitWnd()
{
}

CViewExSplitWnd::~CViewExSplitWnd()
{
}

/***********************************************************************
c
c   FUNCTION: CViewExSplitWnd::SaveData()
c			Save the window value
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CViewExSplitWnd::SaveData()
{
	CChildView1 *v1 = (CChildView1 *)GetPane(0,0);
	v1->UpdateData(TRUE);
	CChildView2 *v2 = (CChildView2 *)GetPane(1,0);
	v2->UpdateData(TRUE);
}

CWnd* CViewExSplitWnd::GetActivePane(int* pRow, int* pCol)
{
	ASSERT_VALID(this);

	// attempt to use active view of frame window
	CWnd* pView = NULL;
 	CFrameWnd* pFrameWnd = GetParentFrame();
	ASSERT_VALID(pFrameWnd);
	pView = pFrameWnd->GetActiveView();

	if (pView == NULL)
		pView = GetFocus();

	return pView;
}

IMPLEMENT_DYNAMIC(CPaneChildDlg, CDialogEx)
BEGIN_MESSAGE_MAP(CPaneChildDlg, CDialogEx)
	ON_BN_CLICKED(IDC_ADD, OnTAdd)
	ON_BN_CLICKED(IDC_DELETE, OnTDelete)
	ON_BN_CLICKED(IDC_SEARCH, OnTSearch)
END_MESSAGE_MAP()

CPaneChildDlg::CPaneChildDlg(CWnd* pParent)
	: CDialogEx(CPaneChildDlg::IDD, pParent)
{
}

CPaneChildDlg::~CPaneChildDlg()
{
}
BOOL CPaneChildDlg::OnInitDialog() 
{
	BOOL ret = CDialog::OnInitDialog();
	CRect rect;
	GetClientRect(&rect);
	tool_sizeTotal4.cx = rect.Width();
	tool_sizeTotal4.cy = rect.Height();
	return ret;
}
void CPaneChildDlg::OnTAdd()
{
	AfxGetMainWnd()->PostMessage(WM_COMMAND, IDC_ADD);
}
void CPaneChildDlg::OnTDelete()
{
	AfxGetMainWnd()->PostMessage(WM_COMMAND, IDC_DELETE);
}
void CPaneChildDlg::OnTSearch()
{
	AfxGetMainWnd()->PostMessage(WM_COMMAND, IDC_SEARCH);
}
/**********************************************************************/
IMPLEMENT_DYNAMIC(CDialogPane,CDockablePane)
BEGIN_MESSAGE_MAP(CDialogPane,CDockablePane)
	ON_WM_CREATE()
	ON_WM_SIZE()
END_MESSAGE_MAP()

CDialogPane::CDialogPane(){}
CDialogPane::~CDialogPane(){}

int CDialogPane::OnCreate(LPCREATESTRUCT lp)
{
	if(CDockablePane::OnCreate(lp)==-1)
		return -1;
	if(!m_wndDlg.Create(CPaneChildDlg::IDD,this))
		return -1;
	m_wndDlg.ShowWindow(SW_SHOW);
	return  0;
}
void CDialogPane::OnSize(UINT nType,int cx,int cy)
{
	CDockablePane::OnSize(nType,cx,cy);
	m_wndDlg.SetWindowPos(NULL,0,0,cx,cy,SWP_NOACTIVATE|SWP_NOZORDER);
}

IMPLEMENT_DYNCREATE(CMainFrame, CFrameWndEx)

BEGIN_MESSAGE_MAP(CMainFrame, CFrameWndEx)
	ON_WM_CREATE()
	ON_WM_CLOSE()
	ON_COMMAND(ID_FILE_LOAD, OnFileLoad)
	ON_COMMAND(ID_FILE_DEFINE, OnFileDefine)
	ON_COMMAND(ID_STATUS_STATUS, OnStatusStatus)
	ON_COMMAND(ID_LIST_BRIEF, OnListBrief)
	ON_COMMAND(ID_LIST_FULL, OnListFull)
	ON_COMMAND(IDC_ADD, OnTAdd)
	ON_COMMAND(IDC_DELETE, OnTDelete)
	ON_COMMAND(IDC_SEARCH, OnTSearch)
	ON_COMMAND(ID_APP_EXIT, OnAppExit)
	ON_COMMAND(ID_LIST_MODIFY, OnListModify)
	ON_COMMAND(ID_DESCRIPTIONS_OPERATORMESSAGE, OnDescriptionsOperatormessage)
	ON_COMMAND(ID_DESCRIPTIONS_PARAMETERS, OnDescriptionsParameters)
	ON_COMMAND(ID_EDIT_ADD, OnTAdd)
	ON_COMMAND(ID_EDIT_DELETE, OnTDelete)
	ON_COMMAND(ID_EDIT_SEARCH, OnTSearch)
	ON_COMMAND(IDC_FILE_SAVE, OnFileSave)
	ON_COMMAND(ID_HELP_HELP, OnHelpHelp)
	ON_COMMAND(ID_FILE_INIT, OnFileInit)
END_MESSAGE_MAP()

static UINT indicators[] =
{
	ID_SEPARATOR,           // status line indicator
	ID_INDICATOR_CAPS,
	ID_INDICATOR_NUM,
	ID_INDICATOR_SCRL,
};

// CMainFrame construction/destruction
/***********************************************************************
c
c   FUNCTION: CMainFrame()
c
c              Constructor of class CMainFrame
c				initial cutter array
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

CMainFrame::CMainFrame()
{
	// TODO: add member initialization code here
	m_popmenu = NULL;
	m_current_tpos = -1;
	m_findstr = "";
	m_filename[0] = '\0';
	m_update = 0;
}

CMainFrame::~CMainFrame()
{
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

BOOL CMainFrame::OnCreateClient(LPCREATESTRUCT lpcs,
	 CCreateContext* pContext)
{
	if (!m_wndSplitter.CreateStatic(this, 3, 1))
	{
		TRACE0("Failed to CreateStaticSplitter\n");
		return FALSE;
	}

	if (!m_wndSplitter.CreateView(0, 0,
		RUNTIME_CLASS(CChildView1), CSize(500, 280), pContext))
	{
		TRACE0("Failed to create second pane\n");
		return FALSE;
	}

	if (!m_wndSplitter.CreateView(1, 0,
		RUNTIME_CLASS(CChildView2), CSize(500, 115), pContext))
	{
		TRACE0("Failed to create first pane\n");
		return FALSE;
	}

	if (!m_wndSplitter.CreateView(2, 0,
		RUNTIME_CLASS(CChildView3), CSize(550, 100), pContext))
	{
		TRACE0("Failed to create first pane\n");
		return FALSE;
	}

	CChildView1 *v1 = (CChildView1 *)m_wndSplitter.GetPane(0,0);
	tool_sizeTotal1 = v1->GetTotalSize() ;
	m_wndSplitter.SetRowInfo(0, tool_sizeTotal1.cy, 20 );

	CChildView2 *v2 = (CChildView2 *)m_wndSplitter.GetPane(1,0);
	tool_sizeTotal2 = v2->GetTotalSize() ;
	m_wndSplitter.SetRowInfo(1, tool_sizeTotal2.cy, 20 );

	CChildView3 *v3 = (CChildView3 *)m_wndSplitter.GetPane(2,0);
	tool_sizeTotal3 = v3->GetTotalSize() ;
	m_wndSplitter.SetRowInfo(2, tool_sizeTotal3.cy, 20 );

	return TRUE;
}


int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CFrameWndEx::OnCreate(lpCreateStruct) == -1)
		return -1;
	EnableDocking(CBRS_ALIGN_BOTTOM);

	UINT style = WS_CHILD | CBRS_BOTTOM |CBRS_TOOLTIPS|CBRS_SIZE_FIXED;
	CString strTitle = _T("Child Dialog Pane");
	if (!m_wndDlgPan.Create(strTitle, this, CRect(0, 0, 0, 0), FALSE,
		IDC_DIALOG_PANE, style))
	{
		TRACE0("Failed to create dialog pane\n");
		return 0; 
	}
	m_wndDlgPan.EnableDocking(CBRS_ALIGN_ANY);
	DockPane((CBasePane*)&m_wndDlgPan, AFX_IDW_DOCKBAR_BOTTOM, CRect(0, 0, 300, 20));
	m_wndDlgPan.ShowPane(TRUE,FALSE,TRUE);
	TOOL_main_frame = this;

	CChildView1 *v1 = (CChildView1 *)m_wndSplitter.GetPane(0,0);
	CRect rect, rect1;
	v1->GetWindowRect(rect);
	GetWindowRect(rect1);
	int border = rect.left - rect1.left;

	Tool_cx = tool_sizeTotal1.cx;
	if (tool_sizeTotal2.cx > Tool_cx)
		Tool_cx = tool_sizeTotal2.cx;
	if (tool_sizeTotal3.cx > Tool_cx)
		Tool_cx = tool_sizeTotal3.cx;
	Tool_cx = Tool_cx + 2*border + GetSystemMetrics(SM_CXVSCROLL) + 2;
	Tool_cy = tool_sizeTotal1.cy +  tool_sizeTotal2.cy + tool_sizeTotal3.cy
					+ 2 * GetSystemMetrics(SM_CYCAPTION) + 2*border 
					+ GetSystemMetrics(SM_CYMENU) + tool_sizeTotal4.cy;
	GetWindowRect(&rect);
	MoveWindow(rect.left, rect.top, Tool_cx, Tool_cy);
	RecalcLayout();
	GetWindowRect(&rect);
	m_wndDlgPan.GetWindowRect(&rect);
	m_wndDlgPan.MoveWindow(rect);
	m_wndDlgPan.GetVirtualRect(rect);
	return 0;
}
void CMainFrame::AdjectWindowSize()
{
	CRect rect;
	GetWindowRect(&rect);
	MoveWindow(rect.left, rect.top, Tool_cx, Tool_cy);
	RecalcLayout();
	m_wndDlgPan.GetWindowRect(&rect);
	ScreenToClient(&rect);
	rect.bottom = rect.top + tool_sizeTotal4.cy + 2;
	m_wndDlgPan.MoveWindow(rect);
	RecalcLayout();
}

BOOL CMainFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs
	cs.cx = 550;
	cs.cy = 1000;
	cs.x = 100;
	cs.y = 0;
	if( !CFrameWndEx::PreCreateWindow(cs) )
		return FALSE;
	return TRUE;
}

// CMainFrame diagnostics

#ifdef _DEBUG
void CMainFrame::AssertValid() const
{
	CFrameWndEx::AssertValid();
}

void CMainFrame::Dump(CDumpContext& dc) const
{
	CFrameWndEx::Dump(dc);
}
#endif //_DEBUG

/***********************************************************************
c
c   FUNCTION: OnFileLoad() 
c
c         This function called when "Load" item in "File" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CMainFrame::OnFileLoad() 
{
	UX_pathname dir,file,ext,filename;
	int i2, nc, ityp;
	int stat, kerr;
	char msg[200];
	char title[300];
	LPCTSTR filter;
/*
.....Load Tool Library
*/
	i2 = 2;
	kerr = 0;
	if (m_filename[0]=='\0')
		filename[0] = '\0';
	else
		strcpy(filename, m_filename);
	if (Tool_modify)
	{
		int ret = tool_mfyesno("Save Changes", 
			"The tool library has been modified, Do you wish to save it?", 0);
		if (ret == 0)
		{
/*
......yes
*/
			strcpy(filename, Tool_head.name);
/*
			filter = _T("Tool Library File (*.TLB)|All Files (*.*)|*.*||");		
			tool_mfopt_filename(NULL, "Tool Library File to be Saved", filter,
					".", "NCL_TOOL", filename, &nc, 0);
*/
			nc = strlen(filename);
//			tool_get_filename(this, "Tool Library File to be Saved", "*.TLB", filename, &nc, "Tool Library Files (*.TLB)", 0,
//				"NCL_TOOL", "System");
			tool_get_filename(this, "Tool Library File to be Saved", "*.TLB|*.TLA", filename, &nc, "Tool Library Files (*.TLB) |Tool Library Files (*.TLA)", 0,
				"NCL_TOOL", "System");
			if (filename[0]!='\0')
				stat = toolc_savlib(filename);
			else
				return;
			if (stat!=0) 
			{
				sprintf(msg, "Can not save file %s", filename);
				tool_mfmsg_box(this, "Problem saving file", msg, 0);
				return;
			}
		}
	}
/*
.....get Tool Library name
*/
/*
	filter = _T("Tool Library File (*.TLB)|*.TLB|Tool Batch File (*.TLA)|*.TLA|All Files (*.*)|*.*||");		
	tool_mfopt_filename(NULL, "Input Tool Library File to Load", filter,
			".", "NCL_TOOL", filename, &nc, 1);
*/
	nc = strlen(filename);
//	tool_get_filename(this, "Input Tool Library File to Load", "*.TLB", filename, &nc, "Tool Library Files (*.TLB)", 1,
//				"NCL_TOOL", "System");
	tool_get_filename(this, "Input Tool Library File to Load", "*.TLB|*.TLA", filename, &nc, "Tool Library Files (*.TLB) |Tool Library Files (*.TLA)", 1,
				"NCL_TOOL", "System");
	if (nc==0)
	{
		return;
	}
/*
.....Determine file type
*/
	tool_break_fname(filename,dir,file,ext);
	if (_stricmp(ext,"TLA") == 0)
		ityp = 0;
	else 
	{
		ityp = 1;
	}
	if (ityp == 1)
		tool_load(filename, &kerr);
	else
		tool_loadbat(filename,&kerr);
	if (kerr==0)
	{
		CChildView1 *v1 = (CChildView1 *)m_wndSplitter.GetPane(0,0);
		v1->UpdateCutType();	
		tool_updList();
	}
/*
.....update title
*/
	filename[nc] = '\0';
	strcpy(m_filename, filename);
	tool_short_filename(filename,file,60);
	strcpy(title, "NCL Tool Library:  ");
	strcat(title, file);
#ifdef _UNICODE	
	WCHAR wtitle[300];
	int len1 = strlen (title) + 1;
	int wlen1 = MultiByteToWideChar(CP_ACP, 0, title, -1, 
							wtitle, len1);
	SetWindowText(wtitle);
#else
	SetWindowText(title);
#endif
	Tool_modify = 0;
	CChildView2 *v2 = (CChildView2 *)m_wndSplitter.GetPane(1,0);
	v2->load_cutter_disp();
	UpdateWindow();
}

/***********************************************************************
c
c   FUNCTION: OnFileDefine()
c
c         This function called when "Define" item in "File" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CMainFrame::OnFileDefine() 
{
	char tlib[256], plib[256], slib[256];
	char ctime[9], cdate[12];
	char disp[40], banner1[256], buf[80];
	UX_pathname dir, dir2, farea, dname;
	
	strcpy (disp, Tool_head.description);
	strcpy (tlib, Tool_head.name);
	strcpy (slib, Tool_head.symlib);
	strcpy (plib, Tool_head.proflib);

	CDefineDlg *defdlg = new CDefineDlg(this, tlib, disp, slib, plib);
	if (defdlg->DoModal()==IDCANCEL)
	{
		delete defdlg;
		return;
	}
#ifdef _UNICODE	
	int len;
	WCHAR *wtmpstr;
	len = defdlg->m_tlib.GetLength();
	tlib[0] = '\0';
	if (len>0)
	{
		wtmpstr = defdlg->m_tlib.GetBuffer(len);
		wcstombs(tlib, wtmpstr, len);
		tlib[len] = '\0';
	}
	
	len = defdlg->m_disp.GetLength();
	disp[0] = '\0';
	if (len>0)
	{
		wtmpstr = defdlg->m_disp.GetBuffer(len);
		wcstombs(disp, wtmpstr, len);
		disp[len] = '\0';
	}
	
	len = defdlg->m_slib.GetLength();
	slib[0] = '\0';
	if (len>0)
	{
		wtmpstr = defdlg->m_slib.GetBuffer(len);
		wcstombs(slib, wtmpstr, len);
		slib[len] = '\0';
	}
	
	len = defdlg->m_plib.GetLength();
	plib[0] = '\0';
	if (len>0)
	{
		wtmpstr = defdlg->m_plib.GetBuffer(len);
		wcstombs(plib, wtmpstr, len);
		plib[len] = '\0';
	}
#else
	strcpy(tlib, (LPCTSTR)defdlg->m_tlib);
	strcpy(disp, (LPCTSTR)defdlg->m_disp);
	strcpy(slib, (LPCTSTR)defdlg->m_slib);
	strcpy(plib, (LPCTSTR)defdlg->m_plib);
#endif
/*
......if called from new, just reinit library
......if not just reset file define fields
*/
	if (Tool_new)
	{
		ncl_tool_reinitlib();
		ncl_gettool_head (&Tool_head);
		if (Tool_current_data.command!=NULL)
			uu_free (Tool_current_data.command);
		if (Tool_current_data.plabel!=NULL)
			uu_free (Tool_current_data.plabel);
		if (Tool_current_data.loadtl!=NULL)
			uu_free ((char*)Tool_current_data.loadtl);
		ncl_getcurrent_tooldata(&Tool_current_data);
		Tool_modify = 0;
	}
/*
.....if the tlib path is system (NCL_TOOL) or local, don't save the path, just name
*/
	ul_break_fname(tlib, dir,dname);
	ul_get_full_dir("NCL_TOOL", dir2);
	if (stricmp(dir, dir2)!=0)
	{
		ul_get_full_dir(".", dir2);
		if (stricmp(dir, dir2)==0)
			strcpy(tlib, dname);
	}
	else
		strcpy(tlib, dname);
	strcpy(Tool_head.name, tlib);
	strcpy(Tool_head.description, disp);
/*
.....if the slib path is system (UB_SYS_M_SYMDIR) or local, don't save the path, just name
.....also remove ending '_S'.
*/
	ux_decompose_path(slib, dir, dname, UX_NQUOTES);
	ul_get_full_dir("UB_SYS_M_SYMDIR", dir2);
	if (stricmp(dir, dir2)!=0)
	{
		ul_get_full_dir(".", dir2);
		if (stricmp(dir, dir2)==0)
			strcpy(slib, dname);
	}
	else
		strcpy(slib, dname);
	len = strlen(slib);
	tool_strip_blanks(slib, &len);
	if ((len>=2)&&(slib[len-2]=='_')&&(slib[len-1]=='S'))
	{
		slib[len-2] = '\0';
	}
	strcpy(Tool_head.symlib, slib);
/*
.....if the plib path is system (NCL_TOOL) or local, don't save the path, just name
*/
	ul_break_fname(plib, dir,dname);
	ul_get_full_dir("NCL_TOOL", dir2);
	if (stricmp(dir, dir2)!=0)
	{
		ul_get_full_dir(".", dir2);
		if (stricmp(dir, dir2)==0)
			strcpy(plib, dname);
	}
	else
		strcpy(plib, dname);
	strcpy(Tool_head.proflib, plib);
	ncdate (cdate);
	cdate[11] = '\0';
	ftim (ctime);
	ctime[8] = '\0';
	strcpy (Tool_head.mod_date, cdate);
	strcpy (Tool_head.mod_time, ctime); 
	strcpy (Tool_head.create_date, cdate);
	strcpy (Tool_head.create_time, ctime);
	Tool_head.version = NCL_version;
/*
.....update discription part
*/
	strcpy (tlib, Tool_head.name);
	strcpy (slib, Tool_head.symlib);

	strcpy(banner1, "NCL Tool Library:  ");
	if (tlib[0]!='\0') 
	{
		tool_get_fname(tlib, tlib);
		strcpy(m_filename, tlib);
	}
	tool_short_filename(tlib,buf,60);
	strcat(banner1, buf);
#ifdef _UNICODE	
	WCHAR wbanner1[256];
	int len1 = strlen (banner1) + 1;
	int wlen1 = MultiByteToWideChar(CP_ACP, 0, banner1, -1, 
							wbanner1, len1);
	SetWindowText(wbanner1);
#else
	SetWindowText(banner1);	
#endif
	delete defdlg;
	CChildView2 *v2 = (CChildView2 *)m_wndSplitter.GetPane(1,0);
	v2->load_cutter_disp();
	UpdateWindow();
}

/***********************************************************************
c
c   FUNCTION: OnStatusStatus() 
c
c         This function called when "Status" item in "Status" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnStatusStatus() 
{
	char msg[2000];
	char tempstr[256];

	strcpy(msg, "Tool Library Statistics:\r\n \r\n");

	sprintf (tempstr, "Tool Library: %s\r\n", Tool_head.name);
	strcat (msg, tempstr);
	sprintf (tempstr, "Description: %s\r\n", Tool_head.description);
	strcat (msg, tempstr);
	sprintf (tempstr, "Symbol Library: %s\r\n", Tool_head.symlib);
	strcat (msg, tempstr);
	sprintf (tempstr, "Profile Library: %s\r\n", Tool_head.proflib);
	strcat (msg, tempstr);
	sprintf (tempstr, "Created: %s %s\tUpdated: %s %s\r\n", 
			Tool_head.create_date, Tool_head.create_time, 
			Tool_head.mod_date, Tool_head.mod_time);
	strcat (msg, tempstr);
	sprintf (tempstr, "Tool Entries: %10d\tLow Tool Number: %-15.0f  High Tool Number: %-15.0f\r\n", 
			Tool_head.no_tools, Tool_head.low_tool, Tool_head.high_tool);
	strcat (msg, tempstr);
	if (Tool_head.units==1)
		sprintf (tempstr, "Units: %s\t", "INCH");
	else
		sprintf (tempstr, "Units: %s\t", "MM");
	strcat (msg, tempstr);
	if (Tool_modify)
		strcat (msg, "Modified: YES\r\n");
	else
		strcat (msg, "Modified: NO\r\n");

	tool_mfmsg_box(this, "Tool Library Statistics", msg, 0);
}

/***********************************************************************
c
c   FUNCTION: OnListBrief()  
c
c         This function called when "Brief" item in "List" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CMainFrame::OnListBrief() 
{
	tool_list(1);
}

/***********************************************************************
c
c   FUNCTION: OnListFull()  
c
c         This function called when "Full" item in "List" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnListFull() 
{
	tool_list(2);	
}

/*******************************************************************
**   E_FUNCTION : tool_list(flag)
**           Create a list file 
**					
**   PARAMETERS
**       INPUT  :
**          flag: 1: create a brief listing
**						2: create a full listing
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void CMainFrame::tool_list(int flag)
{
	UX_pathname filename;
	int i, nc;
/*
.....get list file name
*/
	LPCTSTR filter = _T("List Filename(*.lis)|*.lis|All Files (*.*)|*.*||");		
	tool_mf_filename(NULL, "Input List Filename", "lis", filter,
							 filename, &nc);
	if (nc==0)
	{
		return;
	}
	toolc_list(filename, flag);
}

/*******************************************************************
**   E_FUNCTION : save_tool_data()
**           Save the current window input into Tool_current_data value
**					
**   PARAMETERS
**       INPUT  :
**          flag: 1: create a brief listing
**						2: create a full listing
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void CMainFrame::save_tool_data()
{
	struct TL_loadtl_rec	loadtl[20];
	int k, idx, save_idx,savshank;
	char tempstr[80];

	CChildView1 *v1 = (CChildView1 *)m_wndSplitter.GetPane(0,0);
	v1->UpdateData(TRUE);
	v1->OnTacceptEditChange();
	CChildView2 *v2 = (CChildView2 *)m_wndSplitter.GetPane(1,0);
	savshank = v2->m_shank_ck;
	v2->UpdateData(TRUE);
	v2->m_shank_ck = savshank;

	int i;

#ifdef _UNICODE
	int len;
	WCHAR *wtmpstr;
	if (v1->m_tool.GetLength()!=0)
	{
		len = v1->m_tool.GetLength();
		wtmpstr = v1->m_tool.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.toolno = atof (tempstr);
	}
	else
		Tool_current_data.toolno = 0.0;

	if ((v1->m_desp.GetLength()!=0) && (v1->m_desp.GetLength()<=40))
	{
		len = v1->m_desp.GetLength();
		wtmpstr = v1->m_desp.GetBuffer(len);
		wcstombs(Tool_current_data.description, wtmpstr, len);
		Tool_current_data.description[len] = '\0';
	}
	else if (v1->m_desp.GetLength()>40)
	{
		tool_mfmsg_box(this, "Add Tool error!", "Description only allow 40 characters!", 0);
		return;
	}
	else
		Tool_current_data.description[0] = '\0';

	if (v1->m_def1.GetLength()!=0)
	{
		len = v1->m_def1.GetLength();
		wtmpstr = v1->m_def1.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cutter[0] = atof (tempstr);
		Tool_current_data.ncvals = 1;
	}
	else
		Tool_current_data.cutter[0] = 0.0;

	if (v1->m_parm1.GetLength()!=0)
	{
		len = v1->m_parm1.GetLength();
		wtmpstr = v1->m_parm1.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cparms[0] = atoi (tempstr);
		if (Tool_current_data.cparms[0]>0)
			Tool_current_data.ncvals = 1;
	}
	else
		Tool_current_data.cparms[0] = 0;


	if (v1->m_cdef1.GetLength()!=0)
	{
		len = v1->m_cdef1.GetLength();
		wtmpstr = v1->m_cdef1.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.pseudo[0] = atof (tempstr);
		if (Tool_current_data.pseudo[0]!=0.0)
			Tool_current_data.ndvals = 1;
	}
	else
		Tool_current_data.pseudo[0] = 0.0;

	if (v1->m_cparm1.GetLength()!=0)
	{
		len = v1->m_cparm1.GetLength();
		wtmpstr = v1->m_cparm1.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.dparms[0] = atoi (tempstr);
		if (Tool_current_data.dparms[0]>0)
			Tool_current_data.ndvals = 1;
	}
	else
		Tool_current_data.dparms[0] = 0;


	if (v1->m_def2.GetLength()!=0)
	{
		len = v1->m_def2.GetLength();
		wtmpstr = v1->m_def2.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cutter[1] = atof (tempstr);
		if (Tool_current_data.cutter[1]!=0.0)
			Tool_current_data.ncvals = 2;
	}
	else
		Tool_current_data.cutter[1] = 0.0;

	if (v1->m_parm2.GetLength()!=0)
	{
		len = v1->m_parm2.GetLength();
		wtmpstr = v1->m_parm2.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cparms[1] = atoi (tempstr);
		if (Tool_current_data.cparms[1]>0)
			Tool_current_data.ncvals = 2;
	}
	else
		Tool_current_data.cparms[1] = 0;


	if (v1->m_cdef2.GetLength()!=0)
	{
		len = v1->m_cdef2.GetLength();
		wtmpstr = v1->m_cdef2.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.pseudo[1] = atof (tempstr);
		if (Tool_current_data.pseudo[1]!=0.0)
			Tool_current_data.ndvals = 2;
	}
	else
		Tool_current_data.pseudo[1] = 0.0;

	if (v1->m_cparm2.GetLength()!=0)
	{
		len = v1->m_cparm2.GetLength();
		wtmpstr = v1->m_cparm2.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.dparms[1] = atoi (tempstr);
		if (Tool_current_data.dparms[1]>0)
			Tool_current_data.ndvals = 2;
	}
	else
		Tool_current_data.dparms[1] = 0;

	if (v1->m_def3.GetLength()!=0)
	{
		len = v1->m_def3.GetLength();
		wtmpstr = v1->m_def3.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cutter[2] = atof (tempstr);
		if (Tool_current_data.cutter[2]!=0.0)
			Tool_current_data.ncvals = 3;
	}
	else
		Tool_current_data.cutter[2] = 0.0;

	if (v1->m_parm3.GetLength()!=0)
	{
		len = v1->m_parm3.GetLength();
		wtmpstr = v1->m_parm3.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cparms[2] = atoi (tempstr);
		if (Tool_current_data.cparms[2]>0)
			Tool_current_data.ncvals = 3;
	}
	else
		Tool_current_data.cparms[2] = 0;

	if (v1->m_cdef3.GetLength()!=0)
	{
		len = v1->m_cdef3.GetLength();
		wtmpstr = v1->m_cdef3.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.pseudo[2] = atof (tempstr);
		if (Tool_current_data.pseudo[2]!=0.0)
			Tool_current_data.ndvals = 3;
	}
	else
		Tool_current_data.pseudo[2] = 0;

	if (v1->m_cparm3.GetLength()!=0)
	{
		len = v1->m_cparm3.GetLength();
		wtmpstr = v1->m_cparm3.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.dparms[2] = atoi (tempstr);
		if (Tool_current_data.dparms[2]>0)
			Tool_current_data.ndvals = 3;
	}
	else
		Tool_current_data.dparms[2] = 0;


	if (v1->m_def4.GetLength()!=0)
	{
		len = v1->m_def4.GetLength();
		wtmpstr = v1->m_def4.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cutter[3] = atof (tempstr);
		if (Tool_current_data.cutter[3]!=0.0)
			Tool_current_data.ncvals = 4;
	}
	else
		Tool_current_data.cutter[3] = 0.0;

	if (v1->m_parm4.GetLength()!=0)
	{
		len = v1->m_parm4.GetLength();
		wtmpstr = v1->m_parm4.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cparms[3] = atoi (tempstr);
		if (Tool_current_data.cparms[3]>0)
			Tool_current_data.ncvals = 4;
	}
	else
		Tool_current_data.cparms[3] = 0;


	if (v1->m_cdef4.GetLength()!=0)
	{
		len = v1->m_cdef4.GetLength();
		wtmpstr = v1->m_cdef4.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.pseudo[3] = atof (tempstr);
		if (Tool_current_data.pseudo[3]!=0.0)
			Tool_current_data.ndvals = 4;
	}
	else
		Tool_current_data.pseudo[3] = 0.0;

	if (v1->m_cparm4.GetLength()!=0)
	{
		len = v1->m_cparm4.GetLength();
		wtmpstr = v1->m_cparm4.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.dparms[3] = atoi (tempstr);
		if (Tool_current_data.dparms[3]>0)
			Tool_current_data.ndvals = 4;
	}
	else
		Tool_current_data.dparms[3] = 0;


	if (v1->m_def5.GetLength()!=0)
	{
		len = v1->m_def5.GetLength();
		wtmpstr = v1->m_def5.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cutter[4] = atof (tempstr);
		if (Tool_current_data.cutter[4]!=0.0)
			Tool_current_data.ncvals = 5;
	}
	else
		Tool_current_data.cutter[4] = 0.0;

	if (v1->m_parm5.GetLength()!=0)
	{
		len = v1->m_parm5.GetLength();
		wtmpstr = v1->m_parm5.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cparms[4] = atoi (tempstr);
		if (Tool_current_data.cparms[4]>0)
			Tool_current_data.ncvals = 5;
	}
	else
		Tool_current_data.cparms[4] = 0;


	if (v1->m_cdef5.GetLength()!=0)
	{
		len = v1->m_cdef5.GetLength();
		wtmpstr = v1->m_cdef5.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.pseudo[4] = atof (tempstr);
		if (Tool_current_data.pseudo[4]!=0.0)
			Tool_current_data.ndvals = 5;
	}
	else
		Tool_current_data.pseudo[4] = 0.0;

	if (v1->m_cparm5.GetLength()!=0)
	{
		len = v1->m_cparm5.GetLength();
		wtmpstr = v1->m_cparm5.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.dparms[4] = atoi (tempstr);
		if (Tool_current_data.dparms[4]>0)
			Tool_current_data.ndvals = 5;
	}
	else
		Tool_current_data.dparms[4] = 0;

	if (v1->m_def6.GetLength()!=0)
	{
		len = v1->m_def6.GetLength();
		wtmpstr = v1->m_def6.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cutter[5] = atof (tempstr);
		if (Tool_current_data.cutter[5]!=0.0)
			Tool_current_data.ncvals = 6;
	}
	else
		Tool_current_data.cutter[5] = 0.0;

	if (v1->m_parm6.GetLength()!=0)
	{
		len = v1->m_parm6.GetLength();
		wtmpstr = v1->m_parm6.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.cparms[5] = atoi (tempstr);
		if (Tool_current_data.cparms[5]>0)
			Tool_current_data.ncvals = 6;
	}
	else
		Tool_current_data.cparms[5] = 0;


	if (v1->m_cdef6.GetLength()!=0)
	{
		len = v1->m_cdef6.GetLength();
		wtmpstr = v1->m_cdef6.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.pseudo[5] = atof (tempstr);
		if (Tool_current_data.pseudo[5]!=0.0)
			Tool_current_data.ndvals = 6;
	}
	else
		Tool_current_data.pseudo[5] = 0.0;

	if (v1->m_cparm6.GetLength()!=0)
	{
		len = v1->m_cparm6.GetLength();
		wtmpstr = v1->m_cparm6.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
		Tool_current_data.dparms[5] = atoi (tempstr);
		if (Tool_current_data.dparms[5]>0)
			Tool_current_data.ndvals = 6;
	}
	else
		Tool_current_data.dparms[5] = 0;
/*
.....Get data from CChildView2
*/
	Tool_current_data.segments = v2->m_segment;
	Tool_current_data.move = v2->m_moving;

	if (v2->m_tooldraw.GetLength()!=0)
	{
		len = v2->m_tooldraw.GetLength();
		wtmpstr = v2->m_tooldraw.GetBuffer(len);
		wcstombs(Tool_current_data.drawing, wtmpstr, len);
		Tool_current_data.drawing[len] = '\0';
	}
	else
		Tool_current_data.drawing[0] = '\0';

	if (v2->m_com[0].GetLength()!=0)
	{
		len = v2->m_com[0].GetLength();
		wtmpstr = v2->m_com[0].GetBuffer(len);
		wcstombs(Tool_current_data.major, wtmpstr, len);
		Tool_current_data.major[len] = '\0';
	}
	else
		Tool_current_data.major[0] = '\0';
	if ((Tool_current_data.no_loadtl>0) && (Tool_current_data.loadtl!=NULL))
		uu_free ((char*)Tool_current_data.loadtl);
	Tool_current_data.no_loadtl = 0;
	Tool_current_data.loadtl = NULL;
	save_idx = 0;
	for (i=1; i<41; i++)
	{
		idx = Tool_current_data.no_loadtl;
		if (v2->m_com[i].GetLength()!=0)
		{
			len = v2->m_com[i].GetLength();
			wtmpstr = v2->m_com[i].GetBuffer(len);
			wcstombs(tempstr, wtmpstr, len);
			tempstr[len] = '\0';
		}
		else
			tempstr[0] = '\0';
		k = strlen (tempstr);
		while ((tempstr[k]==' ') && (k>0)) k--;
		tempstr[k+1] = '\0';
		if ((i+1)%2)
		{
			if (tempstr[0]!='\0')
			{
				if (tempstr[0]=='=')
					loadtl[idx].parm = -1;
				else
					loadtl[idx].parm = atoi (tempstr);
			}
			else
				loadtl[idx].parm = 0;
			if ((loadtl[idx].parm!=0) || (loadtl[idx].value[0]!='\0'))
				save_idx = idx;
			Tool_current_data.no_loadtl++;
		}
		else
		{
			strcpy(loadtl[idx].value, tempstr);
		}
	}
	Tool_current_data.no_loadtl = save_idx + 1;
	if (Tool_current_data.no_loadtl>0)
	{
		Tool_current_data.loadtl = (struct TL_loadtl_rec*)
						uu_malloc (Tool_current_data.no_loadtl*sizeof (struct TL_loadtl_rec));
		for (i=0; i<Tool_current_data.no_loadtl; i++)
		{
			Tool_current_data.loadtl[i].parm = loadtl[i].parm;
			strcpy(Tool_current_data.loadtl[i].value, loadtl[i].value);
		}
	}
	Tool_current_data.ctype = v1->m_cuttype;
	Tool_current_data.fpseudo = v1->m_def_cut;
	Tool_current_data.shade = v2->m_shade;
	Tool_current_data.floadtl = v2->m_loadcom;

	Tool_current_data.catt[0] = v2->symbol_value.m_value1;
	Tool_current_data.catt[1] = v2->symbol_value.m_value2;
	Tool_current_data.catt[2] = v2->symbol_value.m_value3;
	Tool_current_data.catt[3] = v2->symbol_value.m_value4;

	int nc;
	tempstr[0] = '\0';
	len = v2->symbol_value.m_parm1.GetLength();
	if (len>0)
	{
		wtmpstr = v2->symbol_value.m_parm1.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.yparms[0] = atoi(tempstr);
	else
		Tool_current_data.yparms[0] = 0;

	tempstr[0] = '\0';
	len = v2->symbol_value.m_parm2.GetLength();
	if (len>0)
	{
		wtmpstr = v2->symbol_value.m_parm2.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.yparms[1] = atoi(tempstr);
	else
		Tool_current_data.yparms[1] = 0;

	tempstr[0] = '\0';
	len = v2->symbol_value.m_parm3.GetLength();
	if (len>0)
	{
		wtmpstr = v2->symbol_value.m_parm3.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.yparms[2] = atoi(tempstr);
	else
		Tool_current_data.yparms[2] = 0;

	tempstr[0] = '\0';
	len = v2->symbol_value.m_parm4.GetLength();
	if (len>0)
	{
		wtmpstr = v2->symbol_value.m_parm4.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.yparms[3] = atoi(tempstr);
	else
		Tool_current_data.yparms[3] = 0;

	Tool_current_data.satt[0] = v2->shank_value.m_value1;
	Tool_current_data.satt[1] = v2->shank_value.m_value2;
	Tool_current_data.satt[2] = v2->shank_value.m_value3;
	Tool_current_data.satt[3] = v2->shank_value.m_value4;

	tempstr[0] = '\0';
	len = v2->shank_value.m_parm1.GetLength();
	if (len>0)
	{
		wtmpstr = v2->shank_value.m_parm1.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.sparms[0] = atoi(tempstr);
	else
		Tool_current_data.sparms[0] = 0;

	tempstr[0] = '\0';
	len = v2->shank_value.m_parm2.GetLength();
	if (len>0)
	{
		wtmpstr = v2->shank_value.m_parm2.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.sparms[1] = atoi(tempstr);
	else
		Tool_current_data.sparms[1] = 0;

	tempstr[0] = '\0';
	len = v2->shank_value.m_parm3.GetLength();
	if (len>0)
	{
		wtmpstr = v2->shank_value.m_parm3.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.sparms[2] = atoi(tempstr);
	else
		Tool_current_data.sparms[2] = 0;

	tempstr[0] = '\0';
	len = v2->shank_value.m_parm4.GetLength();
	if (len>0)
	{
		wtmpstr = v2->shank_value.m_parm4.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.sparms[3] = atoi(tempstr);
	else
		Tool_current_data.sparms[3] = 0;
	if (v2->m_shank_ck)
	{
		if (v2->shank_value.m_shade=="Default")
			Tool_current_data.sshade = 0;
		else if (v2->shank_value.m_shade=="On")
			Tool_current_data.sshade = 1;
		else if (v2->shank_value.m_shade=="Off")
			Tool_current_data.sshade = 2;
	}
	Tool_current_data.hatt[0] = v2->holder_value.m_value1;
	Tool_current_data.hatt[1] = v2->holder_value.m_value2;
	Tool_current_data.hatt[2] = v2->holder_value.m_value3;
	Tool_current_data.hatt[3] = v2->holder_value.m_value4;
	
	tempstr[0] = '\0';
	len = v2->holder_value.m_parm1.GetLength();
	if (len>0)
	{
		wtmpstr = v2->holder_value.m_parm1.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.hparms[0] = atoi(tempstr);
	else
		Tool_current_data.hparms[0] = 0;

	tempstr[0] = '\0';
	len = v2->holder_value.m_parm2.GetLength();
	if (len>0)
	{
		wtmpstr = v2->holder_value.m_parm2.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.hparms[1] = atoi(tempstr);
	else
		Tool_current_data.hparms[1] = 0;

	tempstr[0] = '\0';
	len = v2->holder_value.m_parm3.GetLength();
	if (len>0)
	{
		wtmpstr = v2->holder_value.m_parm3.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.hparms[2] = atoi(tempstr);
	else
		Tool_current_data.hparms[2] = 0;

	tempstr[0] = '\0';
	len = v2->holder_value.m_parm4.GetLength();
	if (len>0)
	{
		wtmpstr = v2->holder_value.m_parm4.GetBuffer(len);
		wcstombs(tempstr, wtmpstr, len);
		tempstr[len] = '\0';
	}
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.hparms[3] = atoi(tempstr);
	else
		Tool_current_data.hparms[3] = 0;
	if (v2->m_holder_ck)
	{
		if (v2->holder_value.m_shade=="Default")
			Tool_current_data.hshade = 0;
		else if (v2->holder_value.m_shade=="On")
			Tool_current_data.hshade = 1;
		else if (v2->holder_value.m_shade=="Off")
			Tool_current_data.hshade = 2;
	}
	if (v2->m_shank_ck==0)
		Tool_current_data.fshank = 0;
	Tool_current_data.fsymbol = v2->m_symbol_ck;
	Tool_current_data.fholder = v2->m_holder_ck;
	if (v2->m_symbol.GetLength()!=0)
	{
		len = v2->m_symbol.GetLength();
		wtmpstr = v2->m_symbol.GetBuffer(len);
		wcstombs(Tool_current_data.symbol, wtmpstr, len);
		Tool_current_data.symbol[len] = '\0';
	}
	else
		Tool_current_data.symbol[0] = '\0';
	if (v2->m_holder.GetLength()!=0)
	{
		len = v2->m_holder.GetLength();
		wtmpstr = v2->m_holder.GetBuffer(len);
		wcstombs(Tool_current_data.symhld, wtmpstr, len);
		Tool_current_data.symhld[len] = '\0';
	}
	else
		Tool_current_data.symhld[0] = '\0';
	if (v2->m_shank.GetLength()!=0)
	{
		len = v2->m_shank.GetLength();
		wtmpstr = v2->m_shank.GetBuffer(len);
		wcstombs(Tool_current_data.symshk, wtmpstr, len);
		Tool_current_data.symshk[len] = '\0';
	}
	else
		Tool_current_data.symshk[0] = '\0';
#else
	if (v1->m_tool.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_tool));
		Tool_current_data.toolno = atof (tempstr);
	}
	else
		Tool_current_data.toolno = 0.0;

	if ((v1->m_desp.GetLength()!=0) && (v1->m_desp.GetLength()<=40))
		strcpy(Tool_current_data.description, (LPCTSTR)(v1->m_desp));
	else if (v1->m_desp.GetLength()>40)
	{
		tool_mfmsg_box(this, "Add Tool error!", "Description only allow 40 characters!", 0);
		return;
	}
	else
		Tool_current_data.description[0] = '\0';

	if (v1->m_def1.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_def1));
		Tool_current_data.cutter[0] = atof (tempstr);
		Tool_current_data.ncvals = 1;
	}
	else
		Tool_current_data.cutter[0] = 0.0;

	if (v1->m_parm1.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_parm1));
		Tool_current_data.cparms[0] = atoi (tempstr);
		if (Tool_current_data.cparms[0]>0)
			Tool_current_data.ncvals = 1;
	}
	else
		Tool_current_data.cparms[0] = 0;


	if (v1->m_cdef1.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cdef1));
		Tool_current_data.pseudo[0] = atof (tempstr);
		if (Tool_current_data.pseudo[0]!=0.0)
			Tool_current_data.ndvals = 1;
	}
	else
		Tool_current_data.pseudo[0] = 0.0;

	if (v1->m_cparm1.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cparm1));
		Tool_current_data.dparms[0] = atoi (tempstr);
		if (Tool_current_data.dparms[0]>0)
			Tool_current_data.ndvals = 1;
	}
	else
		Tool_current_data.dparms[0] = 0;


	if (v1->m_def2.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_def2));
		Tool_current_data.cutter[1] = atof (tempstr);
		if (Tool_current_data.cutter[1]!=0.0)
			Tool_current_data.ncvals = 2;
	}
	else
		Tool_current_data.cutter[1] = 0.0;

	if (v1->m_parm2.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_parm2));
		Tool_current_data.cparms[1] = atoi (tempstr);
		if (Tool_current_data.cparms[1]>0)
			Tool_current_data.ncvals = 2;
	}
	else
		Tool_current_data.cparms[1] = 0;


	if (v1->m_cdef2.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cdef2));
		Tool_current_data.pseudo[1] = atof (tempstr);
		if (Tool_current_data.pseudo[1]!=0.0)
			Tool_current_data.ndvals = 2;
	}
	else
		Tool_current_data.pseudo[1] = 0.0;

	if (v1->m_cparm2.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cparm2));
		Tool_current_data.dparms[1] = atoi (tempstr);
		if (Tool_current_data.dparms[1]>0)
			Tool_current_data.ndvals = 2;
	}
	else
		Tool_current_data.dparms[1] = 0;

	if (v1->m_def3.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_def3));
		Tool_current_data.cutter[2] = atof (tempstr);
		if (Tool_current_data.cutter[2]!=0.0)
			Tool_current_data.ncvals = 3;
	}
	else
		Tool_current_data.cutter[2] = 0.0;

	if (v1->m_parm3.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_parm3));
		Tool_current_data.cparms[2] = atoi (tempstr);
		if (Tool_current_data.cparms[2]>0)
			Tool_current_data.ncvals = 3;
	}
	else
		Tool_current_data.cparms[2] = 0;

	if (v1->m_cdef3.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cdef3));
		Tool_current_data.pseudo[2] = atof (tempstr);
		if (Tool_current_data.pseudo[2]!=0.0)
			Tool_current_data.ndvals = 3;
	}
	else
		Tool_current_data.pseudo[2] = 0;

	if (v1->m_cparm3.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cparm3));
		Tool_current_data.dparms[2] = atoi (tempstr);
		if (Tool_current_data.dparms[2]>0)
			Tool_current_data.ndvals = 3;
	}
	else
		Tool_current_data.dparms[2] = 0;


	if (v1->m_def4.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_def4));
		Tool_current_data.cutter[3] = atof (tempstr);
		if (Tool_current_data.cutter[3]!=0.0)
			Tool_current_data.ncvals = 4;
	}
	else
		Tool_current_data.cutter[3] = 0.0;

	if (v1->m_parm4.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_parm4));
		Tool_current_data.cparms[3] = atoi (tempstr);
		if (Tool_current_data.cparms[3]>0)
			Tool_current_data.ncvals = 4;
	}
	else
		Tool_current_data.cparms[3] = 0;


	if (v1->m_cdef4.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cdef4));
		Tool_current_data.pseudo[3] = atof (tempstr);
		if (Tool_current_data.pseudo[3]!=0.0)
			Tool_current_data.ndvals = 4;
	}
	else
		Tool_current_data.pseudo[3] = 0.0;

	if (v1->m_cparm4.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cparm4));
		Tool_current_data.dparms[3] = atoi (tempstr);
		if (Tool_current_data.dparms[3]>0)
			Tool_current_data.ndvals = 4;
	}
	else
		Tool_current_data.dparms[3] = 0;


	if (v1->m_def5.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_def5));
		Tool_current_data.cutter[4] = atof (tempstr);
		if (Tool_current_data.cutter[4]!=0.0)
			Tool_current_data.ncvals = 5;
	}
	else
		Tool_current_data.cutter[4] = 0.0;

	if (v1->m_parm5.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_parm5));
		Tool_current_data.cparms[4] = atoi (tempstr);
		if (Tool_current_data.cparms[4]>0)
			Tool_current_data.ncvals = 5;
	}
	else
		Tool_current_data.cparms[4] = 0;


	if (v1->m_cdef5.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cdef5));
		Tool_current_data.pseudo[4] = atof (tempstr);
		if (Tool_current_data.pseudo[4]!=0.0)
			Tool_current_data.ndvals = 5;
	}
	else
		Tool_current_data.pseudo[4] = 0.0;

	if (v1->m_cparm5.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cparm5));
		Tool_current_data.dparms[4] = atoi (tempstr);
		if (Tool_current_data.dparms[4]>0)
			Tool_current_data.ndvals = 5;
	}
	else
		Tool_current_data.dparms[4] = 0;

	if (v1->m_def6.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_def6));
		Tool_current_data.cutter[5] = atof (tempstr);
		if (Tool_current_data.cutter[5]!=0.0)
			Tool_current_data.ncvals = 6;
	}
	else
		Tool_current_data.cutter[5] = 0.0;

	if (v1->m_parm6.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_parm6));
		Tool_current_data.cparms[5] = atoi (tempstr);
		if (Tool_current_data.cparms[5]>0)
			Tool_current_data.ncvals = 6;
	}
	else
		Tool_current_data.cparms[5] = 0;


	if (v1->m_cdef6.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cdef6));
		Tool_current_data.pseudo[5] = atof (tempstr);
		if (Tool_current_data.pseudo[5]!=0.0)
			Tool_current_data.ndvals = 6;
	}
	else
		Tool_current_data.pseudo[5] = 0.0;

	if (v1->m_cparm6.GetLength()!=0)
	{
		strcpy(tempstr, (LPCTSTR)(v1->m_cparm6));
		Tool_current_data.dparms[5] = atoi (tempstr);
		if (Tool_current_data.dparms[5]>0)
			Tool_current_data.ndvals = 6;
	}
	else
		Tool_current_data.dparms[5] = 0;
/*
.....Get data from CChildView2
*/
	Tool_current_data.segments = v2->m_segment;
	Tool_current_data.move = v2->m_moving;

	if (v2->m_tooldraw.GetLength()!=0)
		strcpy(Tool_current_data.drawing, (LPCTSTR)(v2->m_tooldraw));
	else
		Tool_current_data.drawing[0] = '\0';

	if (v2->m_com[0].GetLength()!=0)
		strcpy(Tool_current_data.major, (LPCTSTR)(v2->m_com[0]));
	else
		Tool_current_data.major[0] = '\0';
	if ((Tool_current_data.no_loadtl>0) && (Tool_current_data.loadtl!=NULL))
		uu_free ((char*)Tool_current_data.loadtl);
	Tool_current_data.no_loadtl = 0;
	Tool_current_data.loadtl = NULL;
	save_idx = 0;
	for (i=1; i<41; i++)
	{
		idx = Tool_current_data.no_loadtl;
		if (v2->m_com[i].GetLength()!=0)
			strcpy(tempstr, (LPCTSTR)(v2->m_com[i]));
		else
			tempstr[0] = '\0';
		k = strlen (tempstr);
		while ((tempstr[k]==' ') && (k>0)) k--;
		tempstr[k+1] = '\0';
		if ((i+1)%2)
		{
			if (tempstr[0]!='\0')
			{
				if (tempstr[0]=='=')
					loadtl[idx].parm = -1;
				else
					loadtl[idx].parm = atoi (tempstr);
			}
			else
				loadtl[idx].parm = 0;
			if ((loadtl[idx].parm!=0) || (loadtl[idx].value[0]!='\0'))
				save_idx = idx;
			Tool_current_data.no_loadtl++;
		}
		else
		{
			strcpy(loadtl[idx].value, tempstr);
		}
	}
	Tool_current_data.no_loadtl = save_idx + 1;
	if (Tool_current_data.no_loadtl>0)
	{
		Tool_current_data.loadtl = (struct TL_loadtl_rec*)
						uu_malloc (Tool_current_data.no_loadtl*sizeof (struct TL_loadtl_rec));
		for (i=0; i<Tool_current_data.no_loadtl; i++)
		{
			Tool_current_data.loadtl[i].parm = loadtl[i].parm;
			strcpy(Tool_current_data.loadtl[i].value, loadtl[i].value);
		}
	}
	Tool_current_data.ctype = v1->m_cuttype;
	Tool_current_data.fpseudo = v1->m_def_cut;
	Tool_current_data.shade = v2->m_shade;
	Tool_current_data.floadtl = v2->m_loadcom;

	Tool_current_data.catt[0] = v2->symbol_value.m_value1;
	Tool_current_data.catt[1] = v2->symbol_value.m_value2;
	Tool_current_data.catt[2] = v2->symbol_value.m_value3;
	Tool_current_data.catt[3] = v2->symbol_value.m_value4;

	int nc;
	strcpy (tempstr, v2->symbol_value.m_parm1);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.yparms[0] = atoi(tempstr);
	else
		Tool_current_data.yparms[0] = 0;

	strcpy (tempstr, v2->symbol_value.m_parm2);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.yparms[1] = atoi(tempstr);
	else
		Tool_current_data.yparms[1] = 0;

	strcpy (tempstr, v2->symbol_value.m_parm3);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.yparms[2] = atoi(tempstr);
	else
		Tool_current_data.yparms[2] = 0;

	strcpy (tempstr, v2->symbol_value.m_parm4);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.yparms[3] = atoi(tempstr);
	else
		Tool_current_data.yparms[3] = 0;

	Tool_current_data.satt[0] = v2->shank_value.m_value1;
	Tool_current_data.satt[1] = v2->shank_value.m_value2;
	Tool_current_data.satt[2] = v2->shank_value.m_value3;
	Tool_current_data.satt[3] = v2->shank_value.m_value4;

	strcpy (tempstr, v2->shank_value.m_parm1);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.sparms[0] = atoi(tempstr);
	else
		Tool_current_data.sparms[0] = 0;

	strcpy (tempstr, v2->shank_value.m_parm2);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.sparms[1] = atoi(tempstr);
	else
		Tool_current_data.sparms[1] = 0;

	strcpy (tempstr, v2->shank_value.m_parm3);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.sparms[2] = atoi(tempstr);
	else
		Tool_current_data.sparms[2] = 0;

	strcpy (tempstr, v2->shank_value.m_parm4);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.sparms[3] = atoi(tempstr);
	else
		Tool_current_data.sparms[3] = 0;
	if (v2->m_shank_ck)
	{
		if (v2->shank_value.m_shade=="Default")
			Tool_current_data.sshade = 0;
		else if (v2->shank_value.m_shade=="On")
			Tool_current_data.sshade = 1;
		else if (v2->shank_value.m_shade=="Off")
			Tool_current_data.sshade = 2;
	}
	Tool_current_data.hatt[0] = v2->holder_value.m_value1;
	Tool_current_data.hatt[1] = v2->holder_value.m_value2;
	Tool_current_data.hatt[2] = v2->holder_value.m_value3;
	Tool_current_data.hatt[3] = v2->holder_value.m_value4;
	
	strcpy (tempstr, v2->holder_value.m_parm1);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.hparms[0] = atoi(tempstr);
	else
		Tool_current_data.hparms[0] = 0;

	strcpy (tempstr, v2->holder_value.m_parm2);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.hparms[1] = atoi(tempstr);
	else
		Tool_current_data.hparms[1] = 0;

	strcpy (tempstr, v2->holder_value.m_parm3);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.hparms[2] = atoi(tempstr);
	else
		Tool_current_data.hparms[2] = 0;

	strcpy (tempstr, v2->holder_value.m_parm4);
	nc = 20;
	tool_strip_blanks (tempstr,&nc);
	if (tempstr[0]!=0)
		Tool_current_data.hparms[3] = atoi(tempstr);
	else
		Tool_current_data.hparms[3] = 0;
	if (v2->m_holder_ck)
	{
		if (v2->holder_value.m_shade=="Default")
			Tool_current_data.hshade = 0;
		else if (v2->holder_value.m_shade=="On")
			Tool_current_data.hshade = 1;
		else if (v2->holder_value.m_shade=="Off")
			Tool_current_data.hshade = 2;
	}
	if (v2->m_shank_ck==0)
		Tool_current_data.fshank = 0;
	Tool_current_data.fsymbol = v2->m_symbol_ck;
	Tool_current_data.fholder = v2->m_holder_ck;
	if (v2->m_symbol.GetLength()!=0)
	{
		strcpy(Tool_current_data.symbol, (LPCTSTR)(v2->m_symbol));
	}
	else
		Tool_current_data.symbol[0] = '\0';
	if (v2->m_holder.GetLength()!=0)
	{
		strcpy(Tool_current_data.symhld, (LPCTSTR)(v2->m_holder));
	}
	else
		Tool_current_data.symhld[0] = '\0';
	if (v2->m_shank.GetLength()!=0)
	{
		strcpy(Tool_current_data.symshk, (LPCTSTR)(v2->m_shank));
	}
	else
		Tool_current_data.symshk[0] = '\0';
#endif
}
/*******************************************************************
**   E_FUNCTION : OnTAdd()
**              Callback function for "add" button. it can also
**					called from other function. This function will add a 
**					tool into toolib
**   PARAMETERS
**       INPUT  : None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/

void CMainFrame::OnTAdd()
{	
	save_tool_data();

	int stat = tool_addrec(&Tool_current_data);
	if (stat==0)
	{
/*
......tool added, need update the whole list and selection
*/
		tool_updList(Tool_current_data.toolno, -1);
	}
	else if (stat==2)
/*
......the tool exist and updated, just update this tool
*/
		tool_updTool(Tool_current_data);
CRect rect;
	GetWindowRect(&rect);
}

/*******************************************************************
**   E_FUNCTION : tool_deleteone(toolno)
**              This function delete a list position
**   PARAMETERS
**       INPUT  : 
**			toolno: position to add or delete
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/

void CMainFrame::tool_deleteone(double toolno)
{
	CChildView3 *v3 = (CChildView3 *)m_wndSplitter.GetPane(2,0);
	v3->tool_deleteone(toolno);	
}
/*******************************************************************
**   E_FUNCTION : tool_updList(cuttype,toolnum, pos)
**              This function update the list windows
**   PARAMETERS
**       INPUT  : 
**						toolnum: if toolnum != -1 the default selecting 
**									will be tool number #toolnum 
**						pos:     if pos!=-1 and toolnum == -1, the default
**									selecting will be position #pos
**									if both toolnum and pos are -1, default selecting
**									will be position 0	
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/

void CMainFrame::tool_updList(double toolnum, int pos)
{
	m_update = 1;
	CChildView3 *v3 = (CChildView3 *)m_wndSplitter.GetPane(2,0);
	v3->UpdateList(toolnum, pos);	
	m_update = 0;
}
/*******************************************************************
**   E_FUNCTION : tool_updTool(tooldata)
**              This function update the tool in the toolib window
**   PARAMETERS
**       INPUT  : 
**						tooldata: tool to be updated 
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/

void CMainFrame::tool_updTool(TL_tooldata_rec tool_data)
{
	CChildView3 *v3 = (CChildView3 *)m_wndSplitter.GetPane(2,0);
	v3->UpdateTool(tool_data);	
}

/*******************************************************************
**   E_FUNCTION : OnTDelete()
**              Callback function for "delete" button. it can also
**					called from other function. This function will delete a 
**					tool into toolib
**   PARAMETERS
**       INPUT  :None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/

void CMainFrame::OnTDelete()
{
	int pos, stat;
	CChildView3 *v3 = (CChildView3 *)m_wndSplitter.GetPane(2,0);

	double toolnum;
	pos = v3->GetSelPos(&toolnum);
	if (pos==-1)
	{
		tool_mfmsg_box(this, "Delete Error!", "No Tool Entry Select!", 0);
		return;
	}
	stat = tool_delrec(toolnum);
	if (stat==0)
	{
		tool_deleteone(toolnum);
	}
}

/*******************************************************************
**   E_FUNCTION : OnTSearch()
**              Callback function for "search" button. it can also
**					called from other function. This function will search a 
**					tool from toolib
**   PARAMETERS
**       INPUT  :
**				None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/

void CMainFrame::OnTSearch()
{
	CToolFindDlg *findbox = new CToolFindDlg(this, m_findstr);
	if (findbox->DoModal()==IDCANCEL)
	{
		delete findbox;
		return;
	}
	delete findbox;
}
//this is the position for original list
int CMainFrame::SetFindPos(int pos)
{
	CChildView3 *v3 = (CChildView3 *)m_wndSplitter.GetPane(2,0);
	v3->SetSelPos(pos);
	return 1;
}
/*******************************************************************
**   E_FUNCTION : OnAppExit() 
**              Processes the saft "exit"
**   PARAMETERS
**       INPUT  :None
**          
**       OUTPUT :
**				err: not 0: errors
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/

void CMainFrame::OnAppExit() 
{
	SendMessage(WM_CLOSE);
}

void CMainFrame::OnClose() 
{
	int nc, stat,ret;
	UX_pathname filename;
	char msg[256];
	LPCTSTR filter;

	filename[0] = '\0';
	if (Tool_modify)
	{
		ret = tool_mfyesno("Save Changes", 
			"The tool library has been modified, Do you wish to save it?", 0);
		if (ret == 0)
		{
/*
......yes
*/
			strcpy(filename, Tool_head.name);
/*			filter = _T("Tool Library File (*.TLB)|All Files (*.*)|*.*||");		
			tool_mfopt_filename(NULL, "Tool Library File to be Saved", filter,
					".", "NCL_TOOL", filename, &nc, 0);
*/
			nc = strlen(filename);
//			tool_get_filename(this, "Input Tool Library File to Save", "*.TLB", filename, &nc, "Tool Library Files (*.TLB)", 0,
//					"NCL_TOOL", "System");
			tool_get_filename(this, "Tool Library File to be Saved", "*.TLB|*.TLA", filename, &nc, "Tool Library Files (*.TLB) |Tool Library Files (*.TLA)", 0,
				"NCL_TOOL", "System");
			if (filename[0]!='\0')
				stat = toolc_savlib(filename);
			else
				return;
			if (stat!=0) 
			{
				sprintf(msg, "Can not save file %s", filename);
				tool_mfmsg_box(this, "Problem saving file", msg, 0);
				return;
			}
		}
	}
	ncl_del_toollist();
	if (Tool_current_data.command!=NULL)
		uu_free (Tool_current_data.command);
	if (Tool_current_data.plabel!=NULL)
		uu_free (Tool_current_data.plabel);
	if (Tool_current_data.loadtl!=NULL)
		uu_free ((char*)Tool_current_data.loadtl);
	uu_toolmalloc_term();
	CFrameWnd::OnClose();
}

/***********************************************************************
c
c   FUNCTION: OnListModify() 
c
c         This function called when "Modify" item in "List" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CMainFrame::OnListModify() 
{
	CModListDlg *moddlg = new CModListDlg(this);
	moddlg->m_cutparm = tl_modlist.cutparm;
	moddlg->m_heading = tl_modlist.heading;
	moddlg->m_tnumdes = tl_modlist.toolnum;
	moddlg->m_pcut = tl_modlist.pcutparm;
	moddlg->m_dispparm = tl_modlist.dispparm;
	moddlg->m_ltool = tl_modlist.loadcom;
	moddlg->m_optcom = tl_modlist.optcom;
	moddlg->m_paramslb = tl_modlist.paramslb;
	moddlg->m_psymbol = tl_modlist.psymbol;
	moddlg->m_pshank = tl_modlist.pshank;
	moddlg->m_pholder = tl_modlist.pholder;
	if (moddlg->DoModal()==IDCANCEL)
	{
		delete moddlg;
		return;
	}
	tl_modlist.loadcom = moddlg->m_ltool;
	tl_modlist.dispparm = moddlg->m_dispparm;
	tl_modlist.pcutparm = moddlg->m_pcut;
	tl_modlist.toolnum = moddlg->m_tnumdes; 
	tl_modlist.cutparm = moddlg->m_cutparm;
	tl_modlist.heading = moddlg->m_heading;
	tl_modlist.optcom = moddlg->m_optcom;
	tl_modlist.paramslb = moddlg->m_paramslb;
	tl_modlist.psymbol = moddlg->m_psymbol;
	tl_modlist.pshank = moddlg->m_pshank;
	tl_modlist.pholder = moddlg->m_pholder;
	delete moddlg;
}

/*******************************************************************
**   E_FUNCTION : getlist_opt(parm1, parm2, parm3,parm4, parm5,parm6)
**             Get the List modify options
**
**   PARAMETERS
**       INPUT  :
**          
**       OUTPUT : 
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
extern "C" int getlist_opt(int* parms)
{
	parms[0] = tl_modlist.heading;
	parms[1] = tl_modlist.toolnum;
	parms[2] = tl_modlist.cutparm;
	parms[3] = tl_modlist.pcutparm;
	parms[4] = tl_modlist.dispparm;
	parms[5] = tl_modlist.loadcom;
	parms[6] = tl_modlist.optcom;
	parms[7] = tl_modlist.paramslb;
	parms[8] = tl_modlist.psymbol;
	parms[9] = tl_modlist.pshank;
	parms[10] = tl_modlist.pholder;
	return 0;
}

/***********************************************************************
c
c   FUNCTION: OnDescriptionsOperatormessage() 
c
c         This function called when "Operator message" item in "Descriptions" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CMainFrame::OnDescriptionsOperatormessage() 
{
/*
.....check if user have select a tool first
*/
	int pos;
	CChildView1 *v1 = (CChildView1 *)m_wndSplitter.GetPane(0,0);
	CChildView2 *v2 = (CChildView2 *)m_wndSplitter.GetPane(1,0);
	CChildView3 *v3 = (CChildView3 *)m_wndSplitter.GetPane(2,0);
	int mod = v1->IsDocModified();
	if (mod==0) 
		mod = v2->IsDocModified();

	double toolnum;
	pos = v3->GetSelPos(&toolnum);

	if ((pos==-1)||((pos!=-1)&&(mod==1)))
	{
/*
.....added the cutter tool first
*/
		OnTAdd();
	}
	COperateDlg *dlg = new COperateDlg(this);
	if (dlg->DoModal()==IDCANCEL)
	{
		delete dlg;
		return;	
	}
}
/***********************************************************************
c
c   FUNCTION: OnDescriptionsParameters() 
c
c         This function called when "Parameters" item in "Descriptions" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnDescriptionsParameters() 
{
/*
.....check if user have select a tool first
*/
	int pos;
	CChildView1 *v1 = (CChildView1 *)m_wndSplitter.GetPane(0,0);
	CChildView2 *v2 = (CChildView2 *)m_wndSplitter.GetPane(1,0);
	CChildView3 *v3 = (CChildView3 *)m_wndSplitter.GetPane(2,0);
	int mod = v1->IsDocModified();
	if (mod==0) 
		mod = v2->IsDocModified();

	double toolnum;
	pos = v3->GetSelPos(&toolnum);

	if ((pos==-1)||((pos!=-1)&&(mod==1)))
	{
/*
.....added the cutter tool first
*/
		OnTAdd();
	}
	CParmsDlg *dlg = new CParmsDlg(this);
	if (dlg->DoModal()==IDCANCEL)
	{
		delete dlg;
		return;	
	}
}
/***********************************************************************
c
c   FUNCTION: OnFileSave() 
c
c         This function called when "Save" item in "File" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnFileSave() 
{
	UX_pathname dir,file,ext,filename, title;
	int nc, ityp;
	int i2, kerr;
	LPCTSTR filter;

	filename[0] = '\0';
	if (Tool_head.name[0]!='\0')
		strcpy(filename, Tool_head.name);
	else if (m_filename[0]!='\0')
		strcpy(filename, m_filename);
/*
.....get Tool Library name
*/
/*
	filter = _T("Tool Library File (*.TLB)|*.TLB|Tool Batch File (*.TLA)|*.TLA|All Files (*.*)|*.*||");	
	nc = strlen(filename);
	tool_mfopt_filename(NULL, "Input Tool Library File to Save", filter,
							 ".", "NCL_TOOL", filename, &nc, 0);
*/
	nc = strlen(filename);
//	tool_get_filename(this, "Input Tool Library File to Save", "*.TLB", filename, &nc, "Tool Library Files (*.TLB)", 0,
//				"NCL_TOOL", "System");
	tool_get_filename(this, "Tool Library File to be Saved", "*.TLB|*.TLA", filename, &nc, "Tool Library Files (*.TLB) |Tool Library Files (*.TLA)", 0,
				"NCL_TOOL", "System");
	if (nc==0)
	{
		return;
	}
/*
.....Determine file type
*/
	tool_break_fname(filename,dir,file,ext);
	if (ext[0]=='\0')
		strcat(filename, ".TLB");
	if (_stricmp(ext,"TLA") == 0)
		ityp = 0;
	else ityp = 1;
/*
.....because we call fortran function and these function
.....does not recogilize '\0' as end, such as strlen1 will
.....give the wrong length if we don't do adding spaces
*/
	i2 = 2;
	kerr = 0;
	if (ityp == 1)
		toolc_savlib(filename);
	else
		tool_createbat(filename);
/*
.....update title
*/
	tool_short_filename(filename,file,60);
	strcpy(title, "NCL Tool Library:  ");
	strcat(title, file);
#ifdef _UNICODE	
	WCHAR wtitle[300];
	int len1 = strlen (title) + 1;
	int wlen1 = MultiByteToWideChar(CP_ACP, 0, title, -1, 
							wtitle, len1);
	SetWindowText(wtitle);
#else
	SetWindowText(title);
#endif
	Tool_modify = 0;
	UpdateWindow();
}

/***********************************************************************
c
c   FUNCTION: OnHelpHelp() 
c
c         This function called when "Help" item in "Help" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnHelpHelp() 
{
	char *p, buf[256];
	int len;

	p = tool_getenv("NCL_TOOLIB_HELP");
	if (p != 0) 
	{
		len = strlen(p);
		if (len==0)
		{
			sprintf(buf,"%s", p); 
			system(buf);
			return;
		}
		system(p);
	}
}

/***********************************************************************
c
c   FUNCTION: OnFileInit() 
c
c         This function called when "New" item in "File" menu picked
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnFileInit() 
{
	int i, nc, stat;
	int i2;
	UX_pathname filename;
	char msg[256];
	LPCTSTR filter;

	i2 = 2;
	filename[0] = '\0';
	if (Tool_modify)
	{
		int ret = tool_mfyesno("Save Changes", 
			"The tool library has been modified, Do you wish to save it?", 0);
		if (ret == 0)
		{
/*
......yes
*/
			strcpy(filename, Tool_head.name);
/*			filter = _T("Tool Library File (*.TLB)|All Files (*.*)|*.*||");		
			tool_mfopt_filename(NULL, "Tool Library File to be Saved", filter,
					".", "NCL_TOOL", filename, &nc, 0);
*/
			nc = strlen(filename);
//			tool_get_filename(this, "Tool Library File to be Saved", "*.TLB", filename, &nc, "Tool Library Files (*.TLB)", 0,
//				"NCL_TOOL", "System");
			tool_get_filename(this, "Tool Library File to be Saved", "*.TLB|*.TLA", filename, &nc, "Tool Library Files (*.TLB) |Tool Library Files (*.TLA)", 0,
				"NCL_TOOL", "System");
			if (filename[0]!='\0')
				stat = toolc_savlib(filename);
			else
				return;
			if (stat!=0) 
			{
				sprintf(msg, "Can not save file %s", filename);
				tool_mfmsg_box(this, "Problem saving file", msg, 0);
				return;
			}
		}
	}
	Tool_new = 1;
	OnFileDefine();
	Tool_new = 0;
	tool_updList();

	CChildView3 *v3 = (CChildView3 *)m_wndSplitter.GetPane(2,0);

	CToolibDoc* pDoc = v3->GetDocument();
	pDoc->m_cuttype = 1;
	pDoc->m_tool = "";
	pDoc->m_desp = "";
	pDoc->m_def1 = "";
	pDoc->m_def2 = "";
	pDoc->m_def4 = "";
	pDoc->m_def5 = "";
	pDoc->m_def6 = "";
	pDoc->m_parm1 = "";
	pDoc->m_parm2 = "";
	pDoc->m_parm3 = "";
	pDoc->m_parm4 = "";
	pDoc->m_parm5 = "";
	pDoc->m_parm6 = "";
	pDoc->m_cdef1 = "";
	pDoc->m_cdef2 = "";
	pDoc->m_cdef3 = "";
	pDoc->m_cdef4 = "";
	pDoc->m_cdef5 = "";
	pDoc->m_cdef6 = "";
	pDoc->m_cparm1 = "";
	pDoc->m_cparm2 = "";
	pDoc->m_cparm3 = "";
	pDoc->m_cparm4 = "";
	pDoc->m_cparm5 = "";
	pDoc->m_cparm6 = "";

	pDoc->m_def_cut = 0;
	pDoc->m_segment = 0;
	pDoc->m_moving = 0;
	pDoc->m_shade = 0;
	pDoc->m_holder_ck = 0;
	pDoc->m_symbol_ck = 0;
	pDoc->m_shank_ck = 0;
	pDoc->m_loadcom = 0;

	pDoc->m_symbol = "";
	pDoc->m_shank = "";
	pDoc->m_holder = "";
	pDoc->m_tooldraw = "";

	for (i=0; i<41; i++)
		pDoc->m_com[i] = "";

	pDoc->symbol_value.m_class = "All";
	pDoc->symbol_value.m_value1 = 0.0;
	pDoc->symbol_value.m_value2 = 0.0;
	pDoc->symbol_value.m_value3 = 0.0;
	pDoc->symbol_value.m_value4 = 0.0;
	pDoc->symbol_value.m_parm1 = "";
	pDoc->symbol_value.m_parm2 = "";
	pDoc->symbol_value.m_parm3 = "";
	pDoc->symbol_value.m_parm4 = "";
	pDoc->symbol_value.m_symbol = "";
	pDoc->symbol_value.m_shade = "Off";

	pDoc->shank_value.m_class = "All";
	pDoc->shank_value.m_value1 = 0.0;
	pDoc->shank_value.m_value2 = 0.0;
	pDoc->shank_value.m_value3 = 0.0;
	pDoc->shank_value.m_value4 = 0.0;
	pDoc->shank_value.m_parm1 = "";
	pDoc->shank_value.m_parm2 = "";
	pDoc->shank_value.m_parm3 = "";
	pDoc->shank_value.m_parm4 = "";
	pDoc->shank_value.m_symbol = "";
	pDoc->shank_value.m_shade = "Off";

	pDoc->holder_value.m_class = "All";
	pDoc->holder_value.m_value1 = 0.0;
	pDoc->holder_value.m_value2 = 0.0;
	pDoc->holder_value.m_value3 = 0.0;
	pDoc->holder_value.m_value4 = 0.0;
	pDoc->holder_value.m_parm1 = "";
	pDoc->holder_value.m_parm2 = "";
	pDoc->holder_value.m_parm3 = "";
	pDoc->holder_value.m_parm4 = "";
	pDoc->holder_value.m_symbol = "";
	pDoc->holder_value.m_shade = "Off";
	
	pDoc->UpdateAllViews(v3);	
}
