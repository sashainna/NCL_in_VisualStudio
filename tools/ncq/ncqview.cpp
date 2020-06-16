/*
**   FILE NAME: NcqView.cpp
**	  
**   CONTAINS:
**		extern "C" ncq_get_avrchr_size(int pt, char *fntname, int *wid, int *hgt)
**		CNcqView::CNcqView()
**		CNcqView::~CNcqView()
**		 CNcqView::PreCreateWindow()
**		CNcqView::OnInitialUpdate()
**		CNcqView::OnAppExit()
**		CNcqView::OnEditDelete() 
**		CNcqView::OnFileOpen()
**		CNcqView::OnOptionOptions()
**		CNcqView::OnViewUpdate() 
**		CNcqView::OnNcqRun() 
**		CNcqView::OnNcqStop() 
**		CNcqView::OnNclUpdate(WPARAM wParam,LPARAM lParam)
**		CNcqView::OnNclExit(WPARAM wParam,LPARAM lParam)
**		CNcqView::OnSelchangeNcqList()
**		CNcqView::OnChangeFilename()
**		CNcqView::OnGetNCLmsg()
**		CNcqView::OnGetNCLInfo()
**		CNcqView::Display_status_message(char *msg)
**		CNcqView::OnViewUpd_rate(int rate)
**		CNcqView::OnClearStatus()
**		CNcqView::OnViewMonitor()
**		CNcqView::OnFileSaveQueue() 
**		CNcqView::OnFileLoadQueue() 
**		
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqview.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 11:26:21
**
************************************************************************
*/
// ncqView.cpp : implementation of the CNcqView class
//

#include "stdafx.h"
#include <process.h>
#include <direct.h>
#include <stdio.h>
#include <stdlib.h>
#include "ncq.h"

#include "ncqDoc.h"
#include "ncqView.h"
#include "NcqOptDlg.h"
#include "Ncqcom.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int NCQ_runver;
CNcqView *NCQ_mainview = 0;
static PROCESS_INFORMATION ncl_info;
extern "C" char ncq_file[MAX_PATH];
extern "C" char ncq_localdir[MAX_PATH];
static int ncl_message = 0;
static char NCQ_current_file[41];
static char save_queue_dir[MAX_PATH] = "";
static char save_pp_dir[MAX_PATH] = "";
extern "C" int NCQ_monitor;
static int NCQ_drag_filenum = 0;

extern "C" int ncq_opnque (int *ierr);
extern "C" int ncq_lodque();
extern "C" int ncq_clsque();
extern "C" int ncq_add (char *fname, int nc);
extern "C" int ncq_del (char *fname, int nc);
extern "C" void ncq_ntdispmsg(char *msg);
extern "C" int ncq_fflush ();
extern "C" int ncq_setlnbuf(char *buf);
extern "C" void ncq_selectlst(int indx);
extern "C" void ncq_parse_filename(char *fin, char *fout, char *lopts);
extern "C" void ncq_loadque_file (char *filen);
extern  char NCL_common_name[256];
extern "C" int ncq_open_common(char *common_file, int ver);
extern "C" int ncq_close_common();
extern "C" int ncq_read_comblk(NCLInfo *info);
extern "C" int ncq_read_comblk96(NCLInfo96 *info);
extern "C" int ncq_read_commsg(int *, int *, int *, char *, char *);
extern int NCQ_filenum;
extern char **NCQ_ppfile;
extern int *NCQ_ppf_post;
extern "C" int ncq_short_filename(char *, char *, int);
extern "C" int ncq_getpost_msg(char *, char *, char*);
extern "C" int ncl_que_is_empty();
extern "C" char NCQ_file_ext[20][256];
extern "C" char NCQ_file_desp[20][256];
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
static NCLInfo old_info;
static NCLInfo96 old_info96;
static int NCQ_keydown = 0;
extern "C" void ncq_additem(char*);
extern "C" int NCQ_time_limit;
/***********************************************************************
c
c   SUBROUTINE:  issame(NCLInfo info, NCLInfo old_info)
c
c   FUNCTION:  This function compare 2 NCL info structure to see if it is the same
c				
c
c   INPUT:  info: NCL info structure to compare
c			old_info: NCL info structure to compare
c
c   OUTPUT: none
c	return: 0: not the same
c			1: The same
c
c***********************************************************************
*/
static int issame(NCLInfo info, NCLInfo old_info)
{
	if (info.flag!=old_info.flag)
		return 0;
	if (strcmp(info.ppfile, old_info.ppfile)!=0)
		return 0;
	if (info.current!=old_info.current)
		return 0;
	if (info.highest!=old_info.highest)
		return 0;
	if (info.lines!=old_info.lines)
		return 0;
	if (strcmp(info.macro, old_info.macro)!=0)
		return 0;
	if (info.warn!=old_info.warn)
		return 0;
	if (info.error!=old_info.error)
		return 0;
	return 1;
}
/***********************************************************************
c
c   SUBROUTINE:  issame96(NCLInfo96 info, NCLInfo96 old_info96)
c
c   FUNCTION:  This function compare 2 NCL info structure to see if it is the same
c				
c
c   INPUT:  info: NCL info structure to compare
c			old_info96: NCL info structure to compare
c
c   OUTPUT: none
c	return: 0: not the same
c			1: The same
c
c***********************************************************************
*/
static int issame96(NCLInfo96 info, NCLInfo96 old_info96)
{
	if (info.flag!=old_info96.flag)
		return 0;
	if (strcmp(info.ppfile, old_info96.ppfile)!=0)
		return 0;
	if (info.current!=old_info96.current)
		return 0;
	if (info.highest!=old_info96.highest)
		return 0;
	if (info.lines!=old_info96.lines)
		return 0;
	if (strcmp(info.macro, old_info96.macro)!=0)
		return 0;
	if (info.warn!=old_info96.warn)
		return 0;
	if (info.error!=old_info96.error)
		return 0;
	return 1;
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_get_avrchr_size(int pt, char *fntname, int *wid, int *hgt)
c
c   FUNCTION:  This function get the average size of font in the NCQ dialog
c				
c
c   INPUT:  pt: Requested font height in tenths of a point. 
c				(For instance, pass 120 to request a 12-point font.)
c			fntname: typeface name of the font.
c
c   OUTPUT: wid, hgt: average size of the font
c
c***********************************************************************
*/
extern "C" int ncq_get_avrchr_size(int pt, char *fntname, int *wid, int *hgt)
{
	CFont aFont;
	aFont.CreatePointFont(pt,fntname, NULL);
	CWnd *picctl = NCQ_mainview;
	CClientDC dc(picctl);

	CFont* savfont = dc.SelectObject(&aFont );

	CSize sizeText = dc.GetTextExtent("Xx",2);
	*wid = sizeText.cx/2;
	*hgt = sizeText.cy;

	dc.SelectObject(&savfont);
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CNcqView

IMPLEMENT_DYNCREATE(CNcqView, CFormView)

BEGIN_MESSAGE_MAP(CNcqView, CFormView)
	//{{AFX_MSG_MAP(CNcqView)
	ON_WM_CTLCOLOR()
	ON_BN_CLICKED(ID_APP_EXIT, OnAppExit)
	ON_BN_CLICKED(ID_EDIT_DELETE, OnEditDelete)
	ON_BN_CLICKED(ID_FILE_OPEN, OnFileOpen)
	ON_COMMAND(ID_OPTION_OPTIONS, OnOptionOptions)
	ON_BN_CLICKED(ID_VIEW_UPDATE, OnViewUpdate)
	ON_BN_CLICKED(IDC_NCQ_RUN, OnNcqRun)
	ON_LBN_SELCHANGE(IDC_NCQ_LIST, OnSelchangeNcqList)
	ON_EN_CHANGE(IDC_FILENAME, OnChangeFilename)
	ON_WM_TIMER()
	ON_WM_DROPFILES()
	ON_WM_SIZE()
	//}}AFX_MSG_MAP
	// Standard printing commands
END_MESSAGE_MAP()
/////////////////////////////////////////////////////////////////////////////
// CNcqView construction/destruction

CNcqView::CNcqView()
	: CFormView(CNcqView::IDD)
{
	//{{AFX_DATA_INIT(CNcqView)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// TODO: add construction code here
	m_selchg = 0;
	m_pEditBkBrush = new CBrush(RGB(150, 150, 150));
	m_pEditBkBrush2 = new CBrush(RGB(255, 255, 255));
	m_run = FALSE;
	m_priority = NORMAL_PRIORITY_CLASS;
	if (NCQ_time_limit==0)
		SetTimeLimit(-1);
	else if (NCQ_time_limit==1)
		SetTimeLimit(15);
	else if (NCQ_time_limit==2)
		SetTimeLimit(30);
	else if (NCQ_time_limit==3)
		SetTimeLimit(60);
	else if (NCQ_time_limit==4)
		SetTimeLimit(120);
	m_montime = 1000;
	m_monitor = NULL;
	NCQ_drag_filenum = 0;
	m_current_pp[0] = '\0';
	
	m_size.x = -1;
	m_size.y = -1;
	m_init = FALSE;
}

CNcqView::~CNcqView()
{
	delete m_pEditBkBrush;
	delete m_pEditBkBrush2;
}

void CNcqView::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNcqView)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BOOL CNcqView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CFormView::PreCreateWindow(cs);
}

/***********************************************************************
c
c   SUBROUTINE:  OnInitialUpdate
c
c   FUNCTION:  This function initialize 
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNcqView::OnInitialUpdate()
{
	int ierr;
/*
.....save the inital control size
*/
	CWnd* pWnd;
	pWnd = GetDlgItem(IDC_STATIC1);
	pWnd->GetWindowRect(&(m_rect[0]));
	ScreenToClient(&(m_rect[0]));
	
	pWnd = GetDlgItem(IDC_FILENAME);
	pWnd->GetWindowRect(&(m_rect[1]));
	ScreenToClient(&(m_rect[1]));

	pWnd = GetDlgItem(IDC_STATIC2);
	pWnd->GetWindowRect(&(m_rect[2]));
	ScreenToClient(&(m_rect[2]));

	pWnd = GetDlgItem(IDC_STATUS);
	pWnd->GetWindowRect(&(m_rect[3]));
	ScreenToClient(&(m_rect[3]));

	pWnd = GetDlgItem(IDC_NCQ_LIST);
	pWnd->GetWindowRect(&(m_rect[4]));
	ScreenToClient(&(m_rect[4]));

	pWnd = GetDlgItem(IDC_STATIC3);
	pWnd->GetWindowRect(&(m_rect[5]));
	ScreenToClient(&(m_rect[5]));

	pWnd = GetDlgItem(IDC_STATIC_WIN);
	pWnd->GetWindowRect(&(m_rect[6]));
	ScreenToClient(&(m_rect[6]));

	CFormView::OnInitialUpdate();
	GetParentFrame()->RecalcLayout();
	ResizeParentToFit();

	GetClientRect(&m_wndrc);

	DragAcceptFiles(TRUE);	
	NCQ_mainview = this;
	ncq_opnque (&ierr);
	if (ierr != 0) return;
	ncq_lodque();
	ncq_clsque();
	SetDlgItemText(IDC_STATUS, "Unknown");
	if (UU_BATCH == 0)
		ncq_selectlst(0);
	OnSelchangeNcqList();

	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	list->DragAcceptFiles(TRUE);	

	int wid, hgt;
	ncq_get_avrchr_size(8, "Courier", &wid, &hgt);
	wid = wid*131;
	list->SetHorizontalExtent(wid);
	((CEdit *)(NCQ_mainview->GetDlgItem(IDC_STATIC_WIN)))->LimitText(10000000);

	CRect rc;
	GetWindowRect(&rc);
	m_size.x = rc.Width();
	m_size.y = rc.Height();
	m_init = TRUE;

	GetParentFrame()->RecalcLayout();
	ResizeParentToFit();
}
void CNcqView::OnSize(UINT nType, int cx, int cy) 
{
	CFormView::OnSize(nType, cx, cy);
	
	if (nType == SIZE_MAXHIDE || nType == SIZE_MAXSHOW)
	{
		return;
	}
	if (m_init)
	{
		resize_fields();
	}
}

void CNcqView::resize_fields()
{
	CRect wndrc;
	GetClientRect(&wndrc);

	CRect objrc, newrc;
	CWnd* wnd;	
	double ratio_x, ratio_y;
	ratio_x = (double)(wndrc.Width()*1.0)/(m_wndrc.Width()*1.0);
	ratio_y = (double)(wndrc.Height()*1.0)/(m_wndrc.Height()*1.0);
/*
.....first item
*/	
	wnd = GetDlgItem(IDC_STATIC1);
	wnd->GetWindowRect(&objrc);
	ScreenToClient(&objrc);
	
	newrc.left   = m_rect[0].left * ratio_x;
	newrc.right = m_rect[0].right * ratio_x;
	newrc.top    = m_rect[0].top * ratio_y;
	newrc.bottom = newrc.top + (m_rect[0].bottom-m_rect[0].top);

	newrc.left   = max(newrc.left, m_rect[0].left);
	newrc.top    = max(newrc.top,  m_rect[0].top);
	newrc.right  = max(newrc.right, m_rect[0].right);
	newrc.bottom = max(newrc.bottom, m_rect[0].bottom);

	if (!newrc.EqualRect(&objrc))
	{
		wnd->MoveWindow(&newrc);
		wnd->Invalidate();
		wnd->UpdateWindow();
	}
/*
.....second item
*/
	wnd = GetDlgItem(IDC_FILENAME);
	wnd->GetWindowRect(&objrc);
	ScreenToClient(&objrc);

	newrc.left   = m_rect[1].left * ratio_x;
	newrc.right = m_rect[1].right * ratio_x;
	newrc.top    = m_rect[1].top * ratio_y;
	newrc.bottom = newrc.top + (m_rect[1].bottom-m_rect[1].top);

	newrc.left   = max(newrc.left, m_rect[1].left);
	newrc.top    = max(newrc.top,  m_rect[1].top);
	newrc.right  = max(newrc.right, m_rect[1].right);
	newrc.bottom = max(newrc.bottom, m_rect[1].bottom);

	if (!newrc.EqualRect(&objrc))
	{
		wnd->MoveWindow(&newrc);
		wnd->Invalidate();
		wnd->UpdateWindow();
	}
/*
.....third item
*/
	wnd = GetDlgItem(IDC_STATIC2);
	wnd->GetWindowRect(&objrc);
	ScreenToClient(&objrc);

	newrc.left   = m_rect[2].left * ratio_x;
	newrc.right = m_rect[2].right * ratio_x;
	newrc.top    = m_rect[2].top * ratio_y;
	newrc.bottom = newrc.top + (m_rect[2].bottom-m_rect[2].top);

	newrc.left   = max(newrc.left, m_rect[2].left);
	newrc.top    = max(newrc.top,  m_rect[2].top);
	newrc.right  = max(newrc.right, m_rect[2].right);
	newrc.bottom = max(newrc.bottom, m_rect[2].bottom);

	if (!newrc.EqualRect(&objrc))
	{
		wnd->MoveWindow(&newrc);
		wnd->Invalidate();
		wnd->UpdateWindow();
	}
/*
.....forth item
*/
	wnd = GetDlgItem(IDC_STATUS);
	wnd->GetWindowRect(&objrc);
	ScreenToClient(&objrc);

	newrc.left   = m_rect[3].left * ratio_x;
	newrc.right = m_rect[3].right * ratio_x;
	newrc.top    = m_rect[3].top * ratio_y;
	newrc.bottom = newrc.top + (m_rect[3].bottom-m_rect[3].top);

	newrc.left   = max(newrc.left, m_rect[3].left);
	newrc.top    = max(newrc.top,  m_rect[3].top);
	newrc.right  = max(newrc.right, m_rect[3].right);
	newrc.bottom = max(newrc.bottom, m_rect[3].bottom);

	if (!newrc.EqualRect(&objrc))
	{
		wnd->MoveWindow(&newrc);
		wnd->Invalidate();
		wnd->UpdateWindow();
	}
/*
.....fifth item
*/
	wnd = GetDlgItem(IDC_NCQ_LIST);
	wnd->GetWindowRect(&objrc);
	ScreenToClient(&objrc);

	newrc.left   = m_rect[4].left * ratio_x;
	newrc.right = m_rect[4].right * ratio_x;
	newrc.top    = m_rect[4].top * ratio_y;
	newrc.bottom = m_rect[4].bottom * ratio_y;

	newrc.left   = max(newrc.left, m_rect[4].left);
	newrc.top    = max(newrc.top,  m_rect[4].top);
	newrc.right  = max(newrc.right, m_rect[4].right);
	newrc.bottom = max(newrc.bottom, m_rect[4].bottom);

	if (!newrc.EqualRect(&objrc))
	{
		wnd->MoveWindow(&newrc);
		wnd->Invalidate();
		wnd->UpdateWindow();
	}
/*
.....Six item
*/
	wnd = GetDlgItem(IDC_STATIC3);
	wnd->GetWindowRect(&objrc);
	ScreenToClient(&objrc);

	newrc.left   = m_rect[5].left * ratio_x;
	newrc.right = m_rect[5].right * ratio_x;
	newrc.top    = m_rect[5].top * ratio_y;
	newrc.bottom = m_rect[5].bottom * ratio_y;

	newrc.left   = max(newrc.left, m_rect[5].left);
	newrc.top    = max(newrc.top,  m_rect[5].top);
	newrc.right  = max(newrc.right, m_rect[5].right);
	newrc.bottom = max(newrc.bottom, m_rect[5].bottom);

	if (!newrc.EqualRect(&objrc))
	{
		wnd->MoveWindow(&newrc);
		wnd->Invalidate();
		wnd->UpdateWindow();
	}
/*
.....Seven item
*/
	wnd = GetDlgItem(IDC_STATIC_WIN);
	wnd->GetWindowRect(&objrc);
	ScreenToClient(&objrc);

	newrc.left   = m_rect[6].left * ratio_x;
	newrc.right = m_rect[6].right * ratio_x;
	newrc.top    = m_rect[6].top * ratio_y;
	newrc.bottom = m_rect[6].bottom * ratio_y;

	newrc.left   = max(newrc.left, m_rect[6].left);
	newrc.top    = max(newrc.top,  m_rect[6].top);
	newrc.right  = max(newrc.right, m_rect[6].right);
	newrc.bottom = max(newrc.bottom, m_rect[6].bottom);

	if (!newrc.EqualRect(&objrc))
	{
		wnd->MoveWindow(&newrc);
		wnd->Invalidate();
		wnd->UpdateWindow();
	}
	RedrawWindow();
	UpdateWindow();
}

void CNcqView::OnDropFiles(HDROP hDropInfo)
{
    UINT i,j,knc;
	char buf[MAX_PATH+20];
    UINT nFiles = ::DragQueryFile(hDropInfo, (UINT) -1, NULL, 0);
    for (i = 0; i < nFiles; i++)
    {
        char szFileName[_MAX_PATH];
        ::DragQueryFile(hDropInfo, i, szFileName, _MAX_PATH);
/*
.....if the NCL batch is running, because we alloc common space for both
.....NCQ/NCL depend on files number, so if we added more files, it will
....have a memory problem for NCL to input more running info to the common area. 
....so we will write the new file into the ncl.que, and update the new added file 
....in the window list, but ncl will stop running when the file number is exceed the
....file number we pass into NCL. When NCL finished the files we input the first time
....NCL run from NCQ, it will run again to finished new draging files.
*/
		ncq_add (szFileName, strlen(szFileName));
		if (m_run)
		{
			NCQ_drag_filenum++;
		}
    } 
    ::DragFinish(hDropInfo);
}

/////////////////////////////////////////////////////////////////////////////
// CNcqView diagnostics

#ifdef _DEBUG
void CNcqView::AssertValid() const
{
	CFormView::AssertValid();
}

void CNcqView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}

CNcqDoc* CNcqView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CNcqDoc)));
	return (CNcqDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CNcqView message handlers

/***********************************************************************
c
c   FUNCTION: OnNclExit()
c
c         Called when it NCL exit 
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNcqView::OnNclExit()
{
/*
......we need continue run if the using added into files while NCL is running
*/
	if (NCQ_drag_filenum==0)
	{
		KillTimer(1);
		KillTimer(2);
		KillTimer(3);
	}
/*
.....before exit, finished the message output
*/
	OnGetNCLmsg();
/*
.....update ncl status field to "idle"
*/
	SetDlgItemText(IDC_STATUS, "Idle");
/*
.....update whole ncq
*/
	OnViewUpdate();
	ncq_close_common();
	_chdir(ncq_localdir);
/*
.....free the job list too
*/
	if (NCQ_ppfile!=NULL)
	{
		for (int i=0; i<NCQ_filenum; i++)
			free(NCQ_ppfile[i]);
		free (NCQ_ppfile);
		NCQ_ppfile = NULL;
	}
	if (NCQ_ppf_post!=NULL)
	{
		free (NCQ_ppf_post);
		NCQ_ppf_post = NULL;
	}
	m_run = FALSE;
/*
.....close monitor
*/
	if (NCQ_drag_filenum==0)
	{
		if (m_monitor!=NULL)
		{
			delete m_monitor;
			m_monitor = NULL;
		}
	}
	if (NCQ_drag_filenum)
	{
		OnNcqRun();
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnAppExit
c
c   FUNCTION:  Called when "exit" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNcqView::OnAppExit() 
{
	ncq_clsque();     
	PostMessage(WM_QUIT);
}

/***********************************************************************
c
c   SUBROUTINE:  OnEditDelete() 
c
c   FUNCTION:  Called when "delete" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNcqView::OnEditDelete() 
{
	char fname[MAX_PATH];
	int len = GetDlgItemText(IDC_FILENAME, fname, MAX_PATH);
	ncq_del (fname,len);
}

/***********************************************************************
c
c   SUBROUTINE:  On_ncq_add()
c
c   FUNCTION:  Added current filename input into a NCQ filelist
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNcqView::On_ncq_add()
{
	char fname[MAX_PATH];
	int len = GetDlgItemText(IDC_FILENAME, fname, MAX_PATH);
	if (len != 0) ncq_add (fname,len);
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileOpen
c
c   FUNCTION:  Called when "Open" menu is picked or "File:" button is pushed
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNcqView::OnFileOpen() 
{
	CString DirName, FileName;
/* 
.....this 'files' could be multiple files, so it need much bigger buffer. when mutiple files,
.....the 'files' buffer contains filename without directory, so MAX_PATH*100 will be holder
.....thousand of file, should be enough, but files[MAX_PATH] will not enough
*/
	char *fname, *temp, files[MAX_PATH*100];
//	char *fname, *temp, files[MAX_PATH];
	char filter_str[10000], tempfilt[256], sufx[256], buff[256], *p;
	int nc;
	LPCTSTR filter;
//	LPCTSTR filter = "Part Program Files (*.pp)|*.pp|Geometry Build Files (*.geo)|*.geo|NCL Files (*.ncl)|*.ncl|All Files (*.*)|*.*||";		
	DWORD dwFlags = OFN_ALLOWMULTISELECT | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT | OFN_ENABLESIZING;
	char save_dir[MAX_PATH], filen[MAX_PATH];

	filter_str[0]= '\0';
	int i = 0;
	while(i<20)
	{
		strcat(filter_str, NCQ_file_desp[i]);
		if (NCQ_file_ext[i][0]=='\0')
			break;
		strcpy(sufx, NCQ_file_ext[i]);
		tempfilt[0] = '\0';
		do
		{
			if ( tempfilt[0] != '\0') strcat(tempfilt,";");
			strcat(tempfilt,"*.");
			strcpy(buff,sufx);
			p = (char *) strchr(buff,',');
			if (p == 0) sufx[0] = '\0';
			else
			{
				strcpy(sufx,p+1);
				*p = '\0';
			}
			strcat(tempfilt,buff);
		} while (strlen(sufx) != 0);

		strcat(filter_str, "(");
		strcat(filter_str, tempfilt);
		strcat(filter_str, ")|");
		strcat(filter_str, tempfilt);
		strcat(filter_str, "|");
		i++;
	}
/*
.....Added *.* for all files. But first check if filter="*.*",
.....if yes, no need to add "*.*"
*/
	strcat(filter_str, "All Files (*.*)|*.*|");
	strcat(filter_str, "|");
	filter = filter_str;
/*
.....save current directory in case filebrowser change it
*/
	GetCurrentDirectory(MAX_PATH, save_dir);

	CFileDialog *filedlg = new CFileDialog(TRUE, "NCQ", NULL, dwFlags,
			filter, this);
	files[0] = '\0';
	filedlg->m_ofn.lpstrFile = files;
	filedlg->m_ofn.nMaxFile = MAX_PATH*100;
	filedlg->m_ofn.lpstrInitialDir = save_pp_dir;

	if (filedlg->DoModal()==IDCANCEL)
	{
		return;
	}
	CString ftitle = filedlg->GetFileTitle();
	if (ftitle!="")
	{
		FileName = filedlg->GetPathName();
		nc = FileName.GetLength();
		temp = FileName.GetBuffer(nc);
		SetDlgItemText(IDC_FILENAME, temp);
		strcpy(filen, temp);
		ncq_add (filen,strlen(filen));
	}
	else
/*
......multiple file select
*/
	{
		DirName = filedlg->GetPathName();
		nc = DirName.GetLength();
		temp = DirName.GetBuffer(nc);
		fname = (filedlg->m_ofn).lpstrFile + (filedlg->m_ofn).nFileOffset;
		while (fname[0]!='\0')
		{
			strcpy(filen, temp);
			strcat(filen, "\\");
			strcat(filen, fname);
			ncq_add (filen,strlen(filen));
			nc = strlen(fname);
			fname = fname + nc + 1;
		}
	}		
	delete filedlg;	
	GetCurrentDirectory(MAX_PATH, save_pp_dir);
	_chdir(save_dir);
}

/***********************************************************************
c
c   SUBROUTINE:  OnOptionOptions() 
c
c   FUNCTION:  Called when "options" menu is picked. Display an option dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNcqView::OnOptionOptions() 
{
	NcqOptDlg*	optdlg = new NcqOptDlg(this);
	if (optdlg->DoModal()==IDCANCEL)
		return;
}

/***********************************************************************
c
c   SUBROUTINE:  OnViewUpdate() 
c
c   FUNCTION:  Called when "Update" menu is picked. Update the filelist display
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNcqView::OnViewUpdate() 
{
	int ierr = 0;
	ncq_opnque (&ierr);
	if (ierr != 0) return;
	ncq_lodque();
	ncq_clsque();
}

/***********************************************************************
c
c   SUBROUTINE:  OnNcqRun() 
c
c   FUNCTION:  Called when "Run" menu is picked. Run NCL batch
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNcqView::OnNcqRun() 
{
	int result, drag, stat, errfl;
	char buf[MAX_PATH+20], cmdparm[80], lopts[20], sbuf[20];
	STARTUPINFO ncl_stinfo;
	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	NCQ_filenum = list->GetCount();
	drag=0;
	if (NCQ_drag_filenum>0) drag = 1;
	NCQ_drag_filenum = 0;
	if (NCQ_filenum<=0)
		return;

	if (NCQ_monitor)
	{
/*
.....display monitor if it is not displayed
*/
		if (m_monitor==NULL)
			OnViewMonitor();
	}
	m_run = m_run ? FALSE : TRUE;
	if (!m_run)
	{
		return;
	}
	ncl_message = 0;

	NCQ_ppfile = (char **) malloc(NCQ_filenum * sizeof (char*));
	NCQ_ppf_post = (int *) malloc(NCQ_filenum * sizeof (int));

	for (int sel=0; sel<NCQ_filenum; sel++)
	{
		buf[0] = '\0';
		list->GetText(sel, buf);
/*
......added those files into ncl.que now if those files are draged into window
......because those draging file have't added into ncl.que yet
*/

		ncq_parse_filename(buf,buf,lopts);
		NCQ_ppf_post[sel] = lopts[5];
		int len = strlen(buf) - 1;
		while ((buf[len]==' ')||(buf[len]=='\t') || (buf[len]=='\r') || (buf[len]=='\n'))
			len--;
		buf[len+1] = '\0';
		NCQ_ppfile[sel] = (char *)malloc((len+2) * sizeof (char));
		strcpy(NCQ_ppfile[sel], buf);
	}
	ncl_stinfo.cb = NULL;
	ncl_stinfo.cbReserved2 = NULL;
	ncl_stinfo.dwFillAttribute = NULL;
	ncl_stinfo.dwFlags = NULL;
	ncl_stinfo.dwX = NULL;
	ncl_stinfo.dwXCountChars = NULL;
	ncl_stinfo.dwXSize = NULL;
	ncl_stinfo.dwY = NULL;
	ncl_stinfo.dwYCountChars = NULL;
	ncl_stinfo.dwYSize = NULL;
	ncl_stinfo.hStdError = NULL;
	ncl_stinfo.hStdInput = NULL;
	ncl_stinfo.hStdOutput = NULL;
	ncl_stinfo.lpDesktop = NULL;
	ncl_stinfo.lpReserved = NULL;
	ncl_stinfo.lpReserved2 = 0;
	ncl_stinfo.lpTitle = NULL;
	ncl_stinfo.wShowWindow = NULL;
	DWORD mid = ::GetCurrentThreadId();
/*
.....before run, make sure it is in the local directory where the ncl.que is
*/		
	_chdir(ncq_localdir);
/*
......check and open a common memory space for NCL info data
......to save. It will read from NCQ
......the name of the memory object will be named with 
......process id to be unique
*/
	sprintf(NCL_common_name, "NCL_NCQ_%d", mid);
	errfl = 0; result = 0;
	stat = ncq_open_common(NCL_common_name, NCQ_runver);
	if (stat != 1)
		errfl = 1;
	else
	{
		old_info.flag = 0;
		old_info96.flag = 0;

		sprintf(sbuf,"NCLEXE%d",NCQ_runver);
		char * com = getenv(sbuf);
		sprintf(cmdparm, "\"%s\" -q=%u_%d", com, mid, NCQ_filenum);
	
		NCQ_current_file[0] = '\0';
	
		result = CreateProcess(NULL, cmdparm, NULL, NULL, FALSE, 
			DETACHED_PROCESS | m_priority, 
			NULL, NULL, &ncl_stinfo, &ncl_info);
	}
	if (result==0)
	{
		sprintf(buf, "Error trying to run NCL");
		ncq_ntdispmsg(buf);
		KillTimer(1);
		KillTimer(2);
		KillTimer(3);
		ncq_close_common();
/*
.....free the job list too
*/
		if (NCQ_ppfile!=NULL)
		{
			for (int i=0; i<NCQ_filenum; i++)
				free(NCQ_ppfile[i]);
			free (NCQ_ppfile);
			NCQ_ppfile = NULL;
		}
		if (NCQ_ppf_post!=NULL)
		{
			free (NCQ_ppf_post);
			NCQ_ppf_post = NULL;
		}
		m_run = FALSE;
/*
.....close monitor
*/
		if (m_monitor!=NULL)
		{
			delete m_monitor;
			m_monitor = NULL;
		}
/*
.....display message on status window too
*/
		if (errfl == 0)
		{
			Display_status_message("Error trying to run NCLEXE");		
			Display_status_message(
				"Please make sure that the NCLEXE variable points to the NCL executable (ncl.exe)");		
		}
		return;
	}
/*
.....update ncl status field to "Running"
*/
	SetDlgItemText(IDC_STATUS, "Running");
	if (drag==0)
	{
		SetTimer(1, m_montime, NULL);
		SetTimer(2, m_runtime, NULL);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnNcqStop() 
c
c   FUNCTION:  Called when "Now" option is picked. Stop running.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNcqView::OnNcqStop(int type) 
{
	if (!m_run) return;
	if (type == 0) CNcqView::OnTimer(4);
	else CNcqView::OnTimer(5);
}
/*
.....we are not use message to communicate anymore, but use common memory
.....so, we need schedule check for memory update
*/
void CNcqView::OnTimer(UINT_PTR nIDEvent) 
{
	char msg[256];
	static int message_check_time = 0;
	if ((nIDEvent!=1) && (nIDEvent!=2) && (nIDEvent!=3) && (nIDEvent!=4) &&
		(nIDEvent!=5))
		return;
	if (nIDEvent==1)
	{
/*
.....check the common memory to get NCL status info
*/
		OnGetNCLmsg();
/*
.....check the common memory to get NCL info
*/
		OnGetNCLInfo();
/*
.....if NCL normal exit is handled by  OnGetNCLInfo, just return
.....else we need check if the process end
*/
		if (m_run == FALSE)
			return;
		if (ncl_message==0)
		{
			message_check_time++;
			if (message_check_time==10)
			{
/*
......tried 10 times but still can't get any NCL message, then treat as no
......message setup from NCL, automately update NCQ every 10 seconds
*/
				KillTimer(1);
				KillTimer(2);
				SetTimer(3, 10000, NULL);
				SetDlgItemText(IDC_STATUS, "Unknown");
				return;
			}
		}
	}
	if (nIDEvent==2 || nIDEvent==4 || nIDEvent==5)
/*
.....reach NCL run time limit
*/
	{
/*
....should be treat as abnormal exit
*/
/*
.....last time check OnGetNCLInfo();
*/
		OnGetNCLmsg();
		OnGetNCLInfo();
		OnViewUpdate();
		TerminateProcess(ncl_info.hProcess, 0);
/*
.....Added option for user to manually stop - ASF 1/7/13.
*/
		if (nIDEvent==2)
		{
			if (NCQ_current_file[0] != '\0')
			{
				sprintf(msg, "%s ended due to time limit exceeded",
					NCQ_current_file);
			}
			else
				strcpy(msg, "NCL aborted due to time limit exceeded");
		}
		else if (nIDEvent==4 || nIDEvent==5)
		{
			if (NCQ_current_file[0] != '\0')
			{
				sprintf(msg, "%s stopped by user",
					NCQ_current_file);
			}
			else
				strcpy(msg, "NCL stopped by user");
			if (nIDEvent==5)
			{
				Display_status_message(msg);		
				strcpy(msg, "Batch processing stopped by user...");
			}
		}
		Display_status_message(msg);		
		if (nIDEvent < 5) SetDlgItemText(IDC_STATUS, "Unknown");
		else SetDlgItemText(IDC_STATUS, "Idle");
		KillTimer(1);
		KillTimer(2);
		KillTimer(3);
		ncq_close_common();
		_chdir(ncq_localdir);
/*
.....free the job list too
*/
		if (NCQ_ppfile!=NULL)
		{
			for (int i=0; i<NCQ_filenum; i++)
				free(NCQ_ppfile[i]);
			free (NCQ_ppfile);
			NCQ_ppfile = NULL;
		}
		if (NCQ_ppf_post!=NULL)
		{
			free (NCQ_ppf_post);
			NCQ_ppf_post = NULL;
		}
		m_run = FALSE;
/*
.....close monitor
*/
		if (m_monitor!=NULL)
		{
			delete m_monitor;
			m_monitor = NULL;
		}
/*
.....Added delay to help timing - ASF 1/8/14.
*/
		_sleep(500);
		if (nIDEvent < 5) OnNcqRun();
		return;
	}
	if (nIDEvent==3)
	{
/*
.....update NCQ until NCL completed
*/
		OnViewUpdate();
/*
......if after 10 second, the ncl.que still have seem number of PP files,
......it means it don't run NCL (or NCL abort before even executed ncl.que)
......or it run a wrong executable
......in this case, show NCL abort with unknow status
......and consider done.
*/
		CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
		int filenum = list->GetCount();
/*
.....check if ncl.que is empty now, if yes, update NCQ
.....and m_nclrun = 0 (so that we can consider NCL is done
.....until next time user run NCL) and update NCQ. If ncl.que is not empty,
.....update NCQ
*/
		if ((ncl_que_is_empty()) || (filenum==NCQ_filenum))
		{
			if (filenum==NCQ_filenum)
			{
				Display_status_message("Error trying to run NCLEXE");		
				Display_status_message(
					"Please make sure that the NCLEXE variable points to the NCL executable (ncl.exe)");		
			}
			else
				SetDlgItemText(IDC_STATUS, "Idle");
			KillTimer(3);
			ncq_close_common();
			_chdir(ncq_localdir);
/*
.....free the job list too
*/
			if (NCQ_ppfile!=NULL)
			{
				for (int i=0; i<NCQ_filenum; i++)
					free(NCQ_ppfile[i]);
				free (NCQ_ppfile);
				NCQ_ppfile = NULL;
			}
			if (NCQ_ppf_post!=NULL)
			{
				free (NCQ_ppf_post);
				NCQ_ppf_post = NULL;
			}
			m_run = FALSE;
/*
.....close monitor
*/
			if (m_monitor!=NULL)
			{
				delete m_monitor;
				m_monitor = NULL;
			}
		}
		return;
	}
/*
.....check if ncl batch completed
.....if goes here, either NCL haven't done
.....or NCL aborted with unknow reason
*/	
	DWORD pr = GetPriorityClass(ncl_info.hProcess);
	if ((pr==0) && (m_run != FALSE))
/*
.....NCL abort. If it is normal return, handled by NCL exit function, so
.....it only goes here if aborted.
*/
	{
		KillTimer(1);
		KillTimer(2);
		KillTimer(3);
/*
.....last time check OnGetNCLInfo();
*/
		OnGetNCLmsg();
		OnGetNCLInfo();
		OnViewUpdate();
		if (m_run != FALSE)
		{
			if (NCQ_current_file[0] != '\0')
			{
				sprintf(msg, "%s ended with status unknown.", NCQ_current_file);
			} 
			else
				strcpy(msg, "NCL aborted with status unknown.");
			Display_status_message(msg);
			SetDlgItemText(IDC_STATUS, "Unknown");
			OnViewUpdate();
		}
		ncq_close_common();
		_chdir(ncq_localdir);
/*
.....free the job list too
*/
		if (NCQ_ppfile!=NULL)
		{
			for (int i=0; i<NCQ_filenum; i++)
				free(NCQ_ppfile[i]);
			free (NCQ_ppfile);
			NCQ_ppfile = NULL;
		}
		if (NCQ_ppf_post!=NULL)
		{
			free (NCQ_ppf_post);
			NCQ_ppf_post = NULL;
		}
		m_run = FALSE;
/*
.....close monitor
*/
		if (m_monitor!=NULL)
		{
			delete m_monitor;
			m_monitor = NULL;
		}
		OnNcqRun();
		return;
	}
	CView::OnTimer(nIDEvent);
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_ntdispmsg(char *msg)
c
c   FUNCTION:  Display a message
c
c   INPUT:  msg: message to be displayed
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void ncq_ntdispmsg(char *msg)

{
	CNcqApp *app = (CNcqApp*)AfxGetApp();
	CWnd *MainDlg = (CWnd*)(app->GetMainWnd());
	MainDlg = MainDlg->GetActiveWindow();
	if (MainDlg==NULL)
	{
		MessageBox(NULL, msg, "Ncq Message", MB_ICONINFORMATION|MB_OK);
			return;
	}
	MainDlg->MessageBox(msg, "Ncq Message", MB_ICONINFORMATION|MB_OK);
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_ntadditem(char *item)
c
c   FUNCTION:  Add a item into a file listbox
c
c   INPUT:  item: item to be added
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void ncq_ntadditem(char *item)
{
	if (NCQ_mainview==0)
		return;
	char tmp[MAX_PATH+20];
	strcpy(tmp, item);
	int nc = strlen(tmp);
	while ((tmp[nc-1]=='\n')||(tmp[nc-1]=='\r')) nc--;
	tmp[nc] = '\0';
	
	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	list->AddString(tmp);
	int cursel = list->SelectString(0, tmp);
	cursel -= 4;
	if (cursel<0) cursel = 0;
	list->SetTopIndex(cursel);
	NCQ_mainview->ListSelchange();
	list->UpdateWindow( );
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_ntdel_pos(pos)
c
c   FUNCTION:  delete a item from a file listbox
c
c   INPUT:  Pos: item position
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void ncq_ntdel_pos(int pos)
{
	if (NCQ_mainview==0)
		return;
	
	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	list->DeleteString(pos);

	if (pos>0)
		pos--;
	else
		pos = 0;
	list->SetCurSel(pos);
	int cursel = pos - 4;
	if (cursel<0) cursel = 0;
	list->SetTopIndex(cursel);
	NCQ_mainview->ListSelchange();
	list->UpdateWindow( );
}


/***********************************************************************
c
c   FUNCTION: ncq_sel_lastpos()
c
c         Select last item in the filelist
c
c   INPUT:  None
c
c   OUTPUT :  None 
c   RETURN:    None
c
**********************************************************************/
extern "C" void ncq_ntsel_lastpos()
{
	if (NCQ_mainview==0)
		return;
	
	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	int pos = list->GetCount() - 1;
	list->SetCurSel(pos);
	int cursel = pos - 4;
	if (cursel<0) cursel = 0;
	list->SetTopIndex(cursel);
	NCQ_mainview->ListSelchange();
	list->UpdateWindow( );
	
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_ntgetsel(int *sel)
c
c   FUNCTION:  Get current select of the file listbox
c
c   INPUT:  none
c
c   OUTPUT: sel: current select
c
c***********************************************************************
*/
extern "C" void ncq_ntgetsel(int *sel)
{
	if (NCQ_mainview==0)
		return;
	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	*sel = list->GetCurSel();
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_ntreset_list()
c
c   FUNCTION:  Reset the file listbox
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void ncq_ntreset_list()
{
	if (NCQ_mainview==0)
		return;
	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	list->ResetContent();
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_ntselectlst(int indx)
c
c   FUNCTION:  Select an item by index of the file listbox
c
c   INPUT:  indx: index number to be selected.
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void ncq_ntselectlst(int indx)
{
	if (NCQ_mainview==0)
		return;
	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	list->SetCurSel(indx);
	indx -= 4;
	if (indx<0) indx = 0;
	list->SetTopIndex(indx);
	NCQ_mainview->ListSelchange();
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_ntselectfile(char *file)
c
c   FUNCTION:  Select an item by string from the file listbox
c
c   INPUT:  file: string to be selected
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void ncq_ntselectfile(char *file)
{
	if (NCQ_mainview==0)
		return;
	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	int indx = list->FindString(-1, file);
	list->SetCurSel(indx);
	indx -= 4;
	if (indx<0) indx = 0;
	list->SetTopIndex(indx);
	NCQ_mainview->ListSelchange();
}

/***********************************************************************
c
c   SUBROUTINE:  OnSelchangeNcqList() 
c
c   FUNCTION:  Callback for selection change of the file listbox
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNcqView::OnSelchangeNcqList() 
{
/*
.....don't set default option buffer and filename now, so just return in this function
.....if we decided using following code, then be care for about set text into file field
.....when first time call from DOS window, the PreTranslateMessage function will count
.....the "RETURN" event for DOS as for 'File field' so it will added the selected item again
*/
	return;

	char buf[MAX_PATH+20],lopts[20];
	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	int sel = list->GetCurSel( ) ;
	buf[0] = '\0';
	if (sel<0)
		return;
	list->GetText(sel, buf);
//don't set default option buffer
//	ncq_setlnbuf(buf);
	ncq_parse_filename(buf,buf,lopts);
	int len = strlen(buf) - 1;
	while ((buf[len]==' ')||(buf[len]=='\t') || (buf[len]=='\r') || (buf[len]=='\n'))
		len--;
	buf[len+1] = '\0';
	m_selchg = 1;
	SetDlgItemText(IDC_FILENAME, buf);
	m_selchg = 0;
}

/***********************************************************************
c
c   SUBROUTINE:  ListSelchange()
c
c   FUNCTION:  Public function: Callback for selection change of the file listbox
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNcqView::ListSelchange()
{
	OnSelchangeNcqList() ;
}

/***********************************************************************
c
c   SUBROUTINE:  OnChangeFilename() 
c
c   FUNCTION:  Callback for input filename edit box of filename change
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNcqView::OnChangeFilename() 
{	
	if (m_selchg==1)
		return;
/*
.....deselect the list 
*/
	CListBox *list = (CListBox *)(NCQ_mainview->GetDlgItem(IDC_NCQ_LIST));
	list->SetCurSel(-1);


	CEdit *cwin = (CEdit*)GetDlgItem(IDC_FILENAME);
	int pos;
	char fname[MAX_PATH];
	cwin->GetSel(pos, pos);
	if (pos<=0)
		return; 
	GetDlgItemText(IDC_FILENAME, fname, MAX_PATH);
/*
.....when user hit return
.....The current position is at '\r'
.....after '\n" 
*/
	if (fname[pos-1]=='\n')
	{
		ncq_add (fname,strlen(fname));
	}
}

/***********************************************************************
c
c   SUBROUTINE:  PreTranslateMessage
c
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CNcqView::PreTranslateMessage(MSG* msg)
{
	HWND hWnd = GetDlgItem(IDC_FILENAME)->m_hWnd; 
/*
......we use NCQ_keydown value here is because sometimes, when we just accept browser
......by hit return/enter key, the browser will close and the focus will give to
......filname fields, then it will call On_ncq_add() when msg->message==WM_KEYUP
......to cause the filenames to be added twice (once from accept from file browser)
......if we input filename into filename field and hit return to add a filename into
......the Ncq list, it should have the msg->message==WM_KEYDOWN first.
......Yurong
*/
	if ((msg->message==WM_KEYDOWN)&&(msg->wParam==VK_RETURN))
		NCQ_keydown = 1;
	if ((msg->message==WM_KEYUP)&&(NCQ_keydown))
	{
		if ((msg->wParam==VK_RETURN) && (msg->hwnd == hWnd))
/*
.....doing add function
*/
			On_ncq_add();
		NCQ_keydown = 0;
	}
	return CFormView::PreTranslateMessage( msg );
}
/***********************************************************************
c
c   SUBROUTINE:  OnCtlColor() 
c
c   FUNCTION:  This function called when a child control 
c				is about to be drawn. We use override this
c				method to change background color oof a control
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
HBRUSH CNcqView::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	CWnd* pStatWnd = (CWnd*)GetDlgItem(IDC_STATUS);
	CWnd* pStatWnd2 = (CWnd*)GetDlgItem(IDC_STATIC_WIN);
	HWND stathnd = pStatWnd->GetSafeHwnd();
	HWND stathnd2 = pStatWnd2->GetSafeHwnd();
	HWND whnd = pWnd->GetSafeHwnd();
	CString statstr;
	switch (nCtlColor)
	{
		case CTLCOLOR_EDIT:
/*
......we set edit control readonly, so window treat it
......as static label.
*/
		case CTLCOLOR_STATIC:
			if (stathnd==whnd)
			{
				GetDlgItemText(IDC_STATUS, statstr);
				if (statstr=="Unknown")
					pDC->SetTextColor(RGB(255, 255, 20));
				else if (statstr=="Idle")
					pDC->SetTextColor(RGB(255, 20, 20));
				else if (statstr=="Running")
					pDC->SetTextColor(RGB(0, 255, 0));
				pDC->SetBkColor(RGB(150, 150, 150));
				return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
			}
			else if (stathnd2==whnd)
			{
				pDC->SetBkColor(RGB(255, 255, 255));
				return (HBRUSH)(m_pEditBkBrush2->GetSafeHandle());
			}
			else
				return CFormView::OnCtlColor(pDC, pWnd, nCtlColor);
		default:
				return CFormView::OnCtlColor(pDC, pWnd, nCtlColor);
	}
}

/***********************************************************************
c
c   FUNCTION: OnFileLoadQueue
c         Called when menu "Load Queue" under "File" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNcqView::OnFileLoadQueue() 
{
	CString DirName, FileName;
	char *temp, files[MAX_PATH];
	int nc;
	LPCTSTR filter = "NCL Queue file (*.que)|*.que|All Files (*.*)|*.*||";		
	DWORD dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT |
		OFN_ENABLESIZING;
	char save_dir[MAX_PATH], filen[MAX_PATH];
/*
.....save current directory in case filebrowser change it
*/
	GetCurrentDirectory(MAX_PATH, save_dir);

	CFileDialog *filedlg = new CFileDialog(TRUE, "Load the queue file", NULL, dwFlags,
			filter, this);
	files[0] = '\0';
	filedlg->m_ofn.lpstrFile = files;
	filedlg->m_ofn.lpstrInitialDir = save_queue_dir;

	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString ftitle = filedlg->GetFileTitle();
		
	FileName = filedlg->GetPathName();
	nc = FileName.GetLength();
	temp = FileName.GetBuffer(nc);
	strcpy(filen, temp);
	
	ncq_loadque_file (filen);

	delete filedlg;	

	GetCurrentDirectory(MAX_PATH, save_queue_dir);
	_chdir(save_dir);
}
/***********************************************************************
c
c   FUNCTION: OnFileSaveQueue
c         Called when menu "Save Queue" under "File" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNcqView::OnFileSaveQueue() 
{
	CString DirName, FileName;
	char *temp, files[MAX_PATH], msg[256];
	int nc, stat;
	LPCTSTR filter = "NCL Queue file (*.que)|*.que|All Files (*.*)|*.*||";		
	DWORD dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT |
		OFN_ENABLESIZING;
	char save_dir[MAX_PATH], filen[MAX_PATH];
/*
.....save current directory in case filebrowser change it
*/
	GetCurrentDirectory(MAX_PATH, save_dir);

	CFileDialog *filedlg = new CFileDialog(TRUE, "Save the queue file", NULL, dwFlags,
			filter, this);
	files[0] = '\0';
	filedlg->m_ofn.lpstrFile = files;
	filedlg->m_ofn.lpstrInitialDir = save_queue_dir;

	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString ftitle = filedlg->GetFileTitle();
		
	FileName = filedlg->GetPathName();
	nc = FileName.GetLength();
	temp = FileName.GetBuffer(nc);
	strcpy(filen, temp);
	delete filedlg;	
	GetCurrentDirectory(MAX_PATH, save_queue_dir);
	_chdir(save_dir);
/*
.....just copy current ncl.que into the saving file
*/
	FILE *fptr ;
	fptr = fopen(filen,"r");
	if (fptr!=0)
	{
/*
.....overwrite?
*/
		sprintf(msg, "File %s exist, overwrite?", filen);
		stat = MessageBox(msg, "Overwrite File?", MB_YESNO | MB_ICONQUESTION);
		if (stat!=IDYES)
			return;
	}
	CopyFile(ncq_file, filen, FALSE);
}
/***********************************************************************
c
c   FUNCTION: OnViewMonitor
c         Called when menu "NCL Monitor" under "View" called
c			if it already open, close it (act like a toggle button)
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNcqView::OnViewMonitor()
{
	if (m_monitor!=NULL)
	{
		delete m_monitor;
		m_monitor = NULL;
		return;
	}
	m_monitor = new NcqMonitor(this);
	m_monitor->Create(IDD_MONITOR, this);
	m_monitor->ShowWindow(SW_SHOW);
}
/***********************************************************************
c
c   FUNCTION: OnClearStatus
c         Called when menu "NCL OnClearStatus" under "View" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNcqView::OnClearStatus()
{
	CEdit *edt;
	edt = (CEdit *) (GetDlgItem(IDC_STATIC_WIN));

	edt->SetWindowText(""); 
	edt->UpdateWindow();
}
/***********************************************************************
c
c   FUNCTION: OnViewUpd_rate (int rate)
c         Update the monitor update rate
c
c   INPUT:  rate: time in second to update the monitor
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNcqView::OnViewUpd_rate(int rate)
{
	m_montime = rate * 1000;
	KillTimer(1);
	SetTimer(1, m_montime, NULL);
}

/***********************************************************************
c
c   FUNCTION: Display_status_message(msg)
c         Display message in status window
c
c   INPUT:  msg: message to be added into status window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNcqView::Display_status_message(char *msg)
{
	CEdit *edt;
	edt = (CEdit *) (GetDlgItem(IDC_STATIC_WIN));

	CString wintxt;
	int len = strlen(msg);
	edt->GetWindowText(wintxt); 
	int pos = wintxt.GetLength();
	edt->SetSel(pos, pos);
	if (pos!=0)
	{
		edt->ReplaceSel("\r\n");
		edt->SetSel(pos+2, pos+2);
	}
	edt->ReplaceSel(msg);	
	edt->UpdateWindow();
}

/***********************************************************************
c
c   FUNCTION: OnGetNCLInfo()
c         get date from common memory and update the monitor
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNcqView::OnGetNCLInfo()
{
	NCLInfo info;
	NCLInfo96 info96;
	int counter = 0;
	int stat = 0;

	int count;
	if (NCQ_runver>=97)
	{
		count = ncq_read_comblk(&info);
		if ((count != sizeof(NCLInfo)) || (info.flag==0))
			return stat;
	}
	else
	{
		count = ncq_read_comblk96(&info96);
		if ((count != sizeof(NCLInfo96)) || (info96.flag==0))
			return stat;
	}

	if (NCQ_runver>=97)
	{
		if (info.flag==1)
		{
			if (strcmp(info.ppfile, "NCL EXIT")==0)
				OnNclExit();
			else if (strcmp(info.ppfile, "NCL UPDATE")==0)
			{
				if (issame(info, old_info)==0)
				{
					OnViewUpdate();
					uu_move_byte((char*)&info, (char*)&old_info, sizeof(NCLInfo));
				}
			}
			stat = 1;
			ncl_message = 1;
		}
		else if (info.flag==3)
		{
/*
.....check the pp file running, if the pp file is new, reset the timer
.....the customer want to time limit set for per pp file running, not the whole ncq run
*/
			if (m_current_pp[0]=='\0')
				strcpy(m_current_pp, info.ppfile);
			else if (strcmp(info.ppfile, m_current_pp)!=0)
			{
				KillTimer(2);
				SetTimer(2, m_runtime, NULL);
				strcpy(m_current_pp, info.ppfile);
			}
			if (m_monitor==NULL)
				return 0;
			m_monitor->UpdateNCLinfo((char*)&info, NCQ_runver);
			stat = 1;
			ncl_message = 1;
		}
	}
	if (NCQ_runver<97)
	{
		if (info96.flag==1)
		{
			if (strcmp(info96.ppfile, "NCL EXIT")==0)
				OnNclExit();
			else if (strcmp(info96.ppfile, "NCL UPDATE")==0)
			{
				if (issame96(info96, old_info96)==0)
				{
					OnViewUpdate();
					uu_move_byte((char*)&info96, (char*)&old_info96, sizeof(NCLInfo96));
				}
			}
			stat = 1;
			ncl_message = 1;
		}
		else if (info96.flag==3)
		{
			if (m_monitor==NULL)
				return 0;
			m_monitor->UpdateNCLinfo((char*)&info96, NCQ_runver);
			stat = 1;
			ncl_message = 1;
		}
	}
	return stat;
}
/***********************************************************************
c
c   FUNCTION: OnGetNCLmsg()
c         get message date from common memory and display those message
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNcqView::OnGetNCLmsg()
{
	char msg[256], msg1[256], msg2[256*20], filename[41], ldate[12], ltim[9];
	int err, warn, jobno;
	int count = 0;
read:;
	ncq_read_commsg(&err, &warn, &jobno, ldate, ltim);
	count++;
	if (jobno<=0)
		return 0;
	ncl_message = 1;
	ncq_short_filename(NCQ_ppfile[jobno-1], filename, 40);
	strcpy(NCQ_current_file, filename);
	if (err==-1)
		sprintf(msg, "%s started at %s %s.", filename, ldate, ltim);
	else if (err>0)
	{
		if (warn>0)
			sprintf(msg, "%s ended with %d errors and %d warnings at %s %s.", 
						filename,err, warn, ldate, ltim);
		else
			sprintf(msg, "%s ended with %d errors at %s %s.", 
						filename, err, ldate, ltim);				
/*
.....update the que window
*/
		OnViewUpdate();
	}
	else if (err==0)
	{
		if (warn>0)
			sprintf(msg, "%s ended with %d warnings at %s %s.", 
							filename, warn, ldate, ltim);
		else
			sprintf(msg, "%s ended at %s %s.", filename, ldate, ltim);
/*
.....update the que window
*/
		OnViewUpdate();
	}
	Display_status_message(msg);
	if ((err!=-1) && (NCQ_ppf_post[jobno-1]=='1'))
	{
/*
.....after pp file finished, it it run post process, check pworks.log last 2 line
.....and displayed in status area
*/
		int stat = ncq_getpost_msg(NCQ_ppfile[jobno-1], msg1, msg2);
		if (stat!=-1)
		{
			if (msg1[0]!='\0')
				Display_status_message(msg1);
			if (msg2[0]!='\0')
				Display_status_message(msg2);
		}
    }
	if (err!=-1) 
	{
        msg1[0] = '\0';
        strcat(msg,"\n");
        Display_status_message(msg1);
	}
/*
.....continue read until there is no NCL message data
*/
	if (count<NCQ_filenum*2)
		goto read;
	return 1;
}

