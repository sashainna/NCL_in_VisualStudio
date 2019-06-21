/************************************************************************
c
c   FILE NAME: PtedWindow.cpp
c
c   CONTAINS:
c		CPtedWindow::CPtedWindow(CWnd* pParent, char *menuname, char *file, int fopen, int type)
c		CPtedWindow::~CPtedWindow()
c		CPtedWindow::ProgramSaveSelectAs(char *fname, PtedRangeStruct sRange)
c		CPtedWindow::LoadProgram(char *filename, int verify)
c		CPtedWindow::OnFileInclude()
c		CPtedWindow::ProgramIncludeSelect(char *FileName, PtedRangeStruct fRange)
c		CPtedWindow::OnIncludeRange()
c		CPtedWindow::OnFileCutter()
c		CPtedWindow::PreTranslateMessage(MSG* msg)
c		CPtedWindow::LoadCutterFile(char *filename)
c		CPtedWindow::Disp_Msg(char *msg, int flag==1)
c		CPtedWindow::ProgramLoadSelect(char *FileName, PtedRangeStruct sRange)
c		CPtedWindow::LoadAPTdes(char *fileName)
c		CPtedWindow::Bad_APTdes()
c		CPtedWindow::PostNcDestroy() 
c		CPtedWindow::OnViewStatus()
c		CPtedWindow::OnToggleBookmark(UINT nCmdID)
c		CPtedWindow::OnGoBookmark(UINT nCmdID)
c		CPtedWindow::OnClearBookmarks()
c		CPtedWindow::OnToggleBookmark()
c		CPtedWindow::OnClearAllBookmarks()
c		CPtedWindow::OnNextBookmark()
c		CPtedWindow::OnPrevBookmark()
c		CPtedWindow::SetViewFocus()
c		CPtedWindow::OnWindowBookMark()
c		CPtedWindow::OnWindowSyntaxClr()
c		CPtedWindow::SetWindowSyntaxClr(int flag)
c		CPtedWindow::ReAdjustWindow()
c		CPtedWindow::OnEditEnableCUndo()
c		CPtedWindow::OnSize( UINT nType, int cx, int cy )
c		CPtedWindow::SizeDialogItem()
c		CPtedWindow::OnInitDialog()
c		CPtedWindow::ShowProcessWindow(char *title)
c		CPtedWindow::Display_as_percent(int num)
c		CPtedWindow::CloseProcessWindow()
c		extern "C" void Pted_disply_ProcessWindow(char *title, CPtedWindow* parent)
c		extern "C" void Pted_Display_as_percent(int num, CPtedWindow* parent)
c		extern "C" void Pted_Display_as_percent2(int num, int* parent)
c		extern "C" void Pted_Display_as_percent3(int* num, int* parent)
c		extern "C" void Pted_Close_ProcessWindow(CPtedWindow* parent)
c
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c           PtedWindow.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c           09/11/13 , 12:59:29
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include "PWBrowser.h"
#include "Pted.h"
#include "PtedWindow.h"
#include "PtedChildWindow.h"
#include "PtedMainWindow.h"
#include "PtdGlobal.h"
#include "Ptedres.h"
#include "PtdFunc.h"
#include "PtedFileDialog.h"
#include "PtedIncFileDialog.h"
#include "PtedTextBuffer.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <direct.h>

#define LABEL_GAP 4
static int TOTALITEMS;
static UINT WM_FILEHELP = ::RegisterWindowMessage(HELPMSGSTRING);
static UINT WM_FINDREPLACE = ::RegisterWindowMessage(FINDMSGSTRING);
int CPtedWindow::m_untitled = 0;

extern CPtedMainWindow *Pted_MainDlg;
extern "C" void Pted_disply_ProcessDlg(char *title);
extern "C" void Pted_Display_Dlg_percent(int num);
extern "C" void Pted_Close_ProcessDlg();
extern "C" char Pted_localdir[UX_MAX_PATH];
extern "C" void Pted_disply_ProcessWindow(char *title, CPtedWindow* parent);
extern "C" void Pted_Display_as_percent(int num, CPtedWindow* parent);
extern "C" void Pted_Close_ProcessWindow(CPtedWindow *parent);

void CPtedWindow::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPtedProcessDlg)
	DDX_Control(pDX, IDC_PROGRESS1, m_pctl);
	//}}AFX_DATA_MAP
}
/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
BEGIN_MESSAGE_MAP(CPtedWindow, CDialog)
	//{{AFX_MSG_MAP(CPtedWindow)
	ON_WM_INITMENUPOPUP()
	ON_WM_SIZE()
	ON_WM_CLOSE()
	ON_REGISTERED_MESSAGE( WM_FILEHELP, OnIncludeRange)
	ON_REGISTERED_MESSAGE( WM_FINDREPLACE, OnFindReplaceCmd)
	ON_WM_SIZE()
	ON_WM_CTLCOLOR()
	ON_EN_CHANGE (ID_ECOMMAND, OnCommandChanged)
	ON_COMMAND(ID_VIEW_STATUS, OnViewStatus)
	ON_COMMAND(ID_FOPEN, OnFileOpen)
	ON_COMMAND(ID_FSAVE, OnSaveFile)
	ON_COMMAND(ID_FSAVEAS, OnSaveAsFile)
	ON_COMMAND(ID_FCANCEL, DlgQuit)
	ON_COMMAND(IDC_UNDO, On_Eundo)
	ON_COMMAND(IDC_REDO, On_Eredo)
	ON_COMMAND(ID_ECUT, On_Ecut)
	ON_COMMAND(ID_ECOPY, On_Ecopy)

	ON_COMMAND(ID_EDIT_COPY, On_Ecopy)

	ON_COMMAND(ID_EPASTE, On_Epaste)
	ON_COMMAND(ID_EDELETE, On_Edelete)
	ON_COMMAND(ID_EDIT_DELETE_BACK, On_Edelete)
	ON_COMMAND(ID_EDELETEL, On_EdeleteL)
	ON_COMMAND(ID_EINSERTL, On_EinsertL)
	ON_COMMAND(ID_FFIND, On_Ffind)
	ON_COMMAND(ID_FFINDNEXT, On_Ffindnext)
	ON_COMMAND(ID_FFINDPREV, On_Ffindprev)
	ON_COMMAND(ID_FFINDALL, On_Ffindall)
	ON_COMMAND(ID_VTOP, On_View_Top)
	ON_COMMAND(ID_VBOTTOM, On_View_Botm)
	ON_COMMAND(ID_FILE_INCLUDE, OnFileInclude)
	ON_COMMAND(ID_FILE_OPENCUTTERFILE, OnFileCutter)
	ON_COMMAND(ID_SEARCH_REPLACE, OnSearchReplace)
	ON_COMMAND(ID_CONVERT_CONVERT, OnConvertConvert)
	ON_COMMAND(ID_CONVERT_BADBLOCKS, OnConvertBadblocks)
	ON_COMMAND(ID_CONVERT_FORMAT, OnConvertFormat)
	ON_COMMAND(ID_CONVERT_UNFORMAT, OnConvertUnformat)
	ON_COMMAND(ID_CONVERT_CONVERT2, OnConvertConvert2)
	ON_COMMAND(ID_CONVERT_BADBLOCKS2, OnConvertBadblocks2)
	ON_COMMAND(ID_CONVERT_FORMAT2, OnConvertFormat2)
	ON_COMMAND(ID_CONVERT_UNFORMAT2, OnConvertUnformat2)
	ON_COMMAND(ID_CONVERT_LENGTH2, OnConvertLength2)
	ON_COMMAND(ID_EDIT_REVERSE, OnEditReverse)
	ON_COMMAND(ID_CONVERT_LENGTH, OnConvertLength)
	ON_COMMAND(ID_CONVERT_RESEQUENCE, OnConvertResequence)
	ON_COMMAND(ID_CONVERT_SETREGISTER, OnConvertSetregister)
	ON_COMMAND(ID_WINDOW_FILETYPE_APTSOURCE, OnWastyp)
	ON_COMMAND(ID_WINDOW_FILETYPE_CONTROLDATA, OnWputyp)
	ON_COMMAND(ID_WINDOW_FILETYPE_TEXTFILE, OnWclatyp)
	ON_COMMAND(ID_WCLTYP, OnWcltyp)
	ON_COMMAND(ID_WINDOW_FILETYPE_SIMULATE, OnWindowFiletypeSimulate)
	ON_COMMAND(ID_WINDOW_FILETYPE_CUTTERFILE, OnWindowFiletypeCutterfile)
	ON_COMMAND(ID_WINDOW_LOADCUTTERDATA, OnWindowLoadCutterData)
	ON_COMMAND(ID_WINDOW_MARK, OnWindowBookMark)
	ON_COMMAND(ID_WINDOW_SYNTAXCLR, OnWindowSyntaxClr)
	ON_COMMAND(ID_CONVERT_NCTOAPT, OnConvertNctoapt)
	ON_COMMAND(ID_CONVERT_APTTONC, OnConvertApttonc)
	ON_COMMAND(ID_CONVERT_NCTOSIM, OnConvertTosim)
	ON_COMMAND(ID_WINDOW_COMMANDLINEON, OnWindowCommandlineon)
	ON_COMMAND(ID_CUNDO_ENABLED, OnEditEnableCUndo)

	ON_COMMAND_RANGE(ID_EDIT_TOGGLE_BOOKMARK0, ID_EDIT_TOGGLE_BOOKMARK9, OnToggleBookmark)
	ON_COMMAND_RANGE(ID_EDIT_GO_BOOKMARK0, ID_EDIT_GO_BOOKMARK9, OnGoBookmark)
	ON_COMMAND(ID_EDIT_CLEAR_BOOKMARKS, OnClearBookmarks)
	ON_COMMAND(ID_EDIT_TOGGLE_BOOKMARK,     OnToggleBookmark)
	ON_COMMAND(ID_EDIT_GOTO_NEXT_BOOKMARK,  OnNextBookmark)
	ON_COMMAND(ID_EDIT_GOTO_PREV_BOOKMARK,  OnPrevBookmark)
	ON_COMMAND(ID_EDIT_CLEAR_ALL_BOOKMARKS, OnClearAllBookmarks)
	ON_COMMAND(ID_FILE_FPRINT, OnFilePrint)	
	ON_COMMAND(ID_FILE_FPAGE_SETUP, OnFilePageSetup)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/***********************************************************************
c
c   SUBROUTINE: CPtedWindow(CWnd* pParent, int type)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			int type:		window type.
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPtedWindow::CPtedWindow(CWnd* pParent, char *menuname, char *file, int fopen, int type)	
{
	m_mark_flag = 1;
	m_syntax_color = 1;
	m_pParent = pParent;
	m_wtype = type; 
	m_actcom = 0;
	m_processor = 0;
	m_curpos = 0;
	m_comcur = 0;
	m_comcursor = 0;
	m_comline = 0;
	m_comlen = 0;
	if ((menuname!=NULL)&&(menuname[0]!='\0'))
		strcpy(m_menuname, menuname);
	CDialog::CDialog();
	if (file!=NULL)
		strcpy(m_file, file);
	else
		m_file[0] = '\0';
	m_openflag = fopen;
/*
.....initial backgroup brush for Edit control
*/
	m_pEditBkBrush = new CBrush(RGB(255, 255, 255));
	m_pFindDlg = NULL;
	m_pReplaceDlg = NULL;
	m_pFindAllDlg = NULL;
	m_bNext = 1;
	m_bCase = 0;
	m_letter = 0;
	m_strFind = "";	
	m_IncFileBox = NULL;
	m_cRangeBox = NULL;
	m_fRangeBox = NULL;
	m_fRange.begin = 1;
	m_fRange.end = 1;
	m_fRange.baddress[0] = '\0';
	m_fRange.bstring[0] = '\0';
	m_fRange.enumber[0] = '\0';
	m_fRange.eaddress[0] = '\0';
	m_fRange.estring[0] = '\0';
/*
.....find/replace string type
.....1: text string
.....2: Letter address
*/
	m_strType = 1;
/*
.....load file type, default to .pu* file
.....2: control data file
*/
	m_ftype = 2;
	m_hIcon = AfxGetApp()->LoadIcon(IDI_PTED_ICON);

	strcpy(m_input_mdf, "PWORKS_0.MDF");
	strcpy(m_output_mdf, "PWORKS_0.MDF");

	m_TextView = NULL;
	m_pmenu = new CMenu();
	m_undo_menu = 0;
	m_convert = 0;
	m_no_undo = 0;
}

/***********************************************************************
c
c   SUBROUTINE:  ~CPtedWindow()
c   FUNCTION:  Deconstructor
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPtedWindow::~CPtedWindow()
{
	m_TextView = NULL;
	if (m_pmenu!=NULL)
	{
		delete m_pmenu;
		m_pmenu = NULL;
	}
}
/***********************************************************************
c
c   SUBROUTINE:  OnSize( UINT nType, int cx, int cy )
c
c   FUNCTION:  This function called after resize window
c
c   INPUT:  ntype: Specifies the type of resizing 
c			cx, cy: new width and height
c   OUTPUT: none
c
c***********************************************************************/
void CPtedWindow::OnSize( UINT nType, int cx, int cy )
{
	CWnd::OnSize(nType, cx, cy );
	if (m_TextView!=NULL)
		SizeDialogItem();
}

/***********************************************************************
c
c   SUBROUTINE:  SizeDialogItem()
c
c   FUNCTION:  resize the dialog window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::SizeDialogItem()
{
	CRect windowRect;
	GetClientRect(windowRect);

	int dx;
	int dy;

	CRect rect,rect1, temp2;
	int borderx, bordery;
	GetWindowRect(&rect1);
	m_TextView->GetWindowRect(&rect);
	borderx = rect.left - rect1.left;
	bordery = rect.top - rect1.top;

	if (m_processor)
	{
		CRect ptemp (0,0, 100, 10);
		MapDialogRect(&ptemp);

		dx = ptemp.right;
		dy = ptemp.bottom;
		temp2 = windowRect;
		temp2.left = 2;
		temp2.right = ptemp.left + dx;
		temp2.bottom = windowRect.bottom - 5;
		temp2.top = temp2.bottom - dy;
		((CWnd*)GetDlgItem(ID_LPROCESSOR1))->MoveWindow(&temp2);
		((CWnd*)GetDlgItem(ID_LPROCESSOR1))->ShowWindow(SW_SHOW);

		temp2.left = temp2.right;
		temp2.right = temp2.left + 1.5*dx;
		((CWnd*)GetDlgItem(IDC_PROGRESS1))->MoveWindow(&temp2);
		((CWnd*)GetDlgItem(IDC_PROGRESS1))->ShowWindow(SW_SHOW);

		temp2.left = temp2.right + 10;
		temp2.right = windowRect.right - 2*borderx;
		((CWnd*)GetDlgItem(ID_LPROCESSOR2))->MoveWindow(&temp2);
		((CWnd*)GetDlgItem(ID_LPROCESSOR2))->ShowWindow(SW_SHOW);
		windowRect.bottom = windowRect.bottom - ptemp.Height();
	}
	else
	{
		((CWnd*)GetDlgItem(ID_LPROCESSOR1))->ShowWindow(SW_HIDE);
		((CWnd*)GetDlgItem(ID_LPROCESSOR2))->ShowWindow(SW_HIDE);
		((CWnd*)GetDlgItem(IDC_PROGRESS1))->ShowWindow(SW_HIDE);
	}

	CRect temp (GetSystemMetrics(SM_CXVSCROLL),0, 30+GetSystemMetrics(SM_CXVSCROLL), 15);
	temp2 = windowRect;
	if (m_actcom==1)
	{
		MapDialogRect(&temp);
		dy = temp.Height() ;
		dx = temp.Width();

		temp2 = windowRect;
		temp2.left = 2;
		temp2.right = temp.left + dx;
		temp2.bottom = windowRect.bottom - 10;
		temp2.top = temp2.bottom - dy;
		((CWnd*)GetDlgItem(ID_LCOMMAND))->MoveWindow(&temp2);
		((CWnd*)GetDlgItem(ID_LCOMMAND))->ShowWindow(SW_SHOW);

		temp2.left = temp2.right;
		temp2.right = windowRect.right - 2*borderx;
		((CWnd*)GetDlgItem(ID_ECOMMAND))->MoveWindow(&temp2);
		((CWnd*)GetDlgItem(ID_ECOMMAND))->ShowWindow(SW_SHOW);

		temp2.left = 0;
		temp2.right = windowRect.right - 2*borderx;
		temp2.bottom = windowRect.bottom - (dy + 20);
		temp2.top = temp2.bottom - 60;
		((CWnd*)GetDlgItem(ID_EMESSAGE))->MoveWindow(&temp2);
		((CWnd*)GetDlgItem(ID_EMESSAGE))->ShowWindow(SW_SHOW);

		temp2.left = 0;
		temp2.right = windowRect.right - 2*borderx;
		temp2.top = 0;
		temp2.bottom = windowRect.bottom - (dy + 20) - 70;
	}
	else
	{
		((CWnd*)GetDlgItem(ID_LCOMMAND))->ShowWindow(SW_HIDE);
		((CWnd*)GetDlgItem(ID_EMESSAGE))->ShowWindow(SW_HIDE);
		((CWnd*)GetDlgItem(ID_ECOMMAND))->ShowWindow(SW_HIDE);
		temp2.bottom = windowRect.bottom - 10;
	}
	if (m_TextView!=NULL)
		m_TextView->MoveWindow(temp2);
	else
		return;
	m_TextView->SetFocus( );
}

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize every fields in
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/

BOOL CPtedWindow::OnInitDialog()
{
	CDialog::OnInitDialog();
	SetIcon(m_hIcon, TRUE);

	m_TextView = new CPtedTextView(m_mark_flag);
	m_TextView->Create( NULL,   
						NULL,  
						AFX_WS_DEFAULT_VIEW | WS_CHILD,
						CRect(0, 0, 0, 0),
						this,   
						0);     
	if (m_TextView == NULL)
		EndDialog(IDCANCEL);
	m_TextView->SetParent(this);
	m_TextView->SetType(m_wtype);

	CRect windowRect, TextRect;

	int dx = (m_TextView->GetCharWidth())*82;
	int dy = (m_TextView->GetLineHeight())*26;
	TextRect.top = 0;
	TextRect.bottom = dy + GetSystemMetrics(SM_CYHSCROLL);;
	TextRect.left = 0;
	if (m_mark_flag)
		TextRect.right = dx + 2 * GetSystemMetrics(SM_CXVSCROLL);
	else
		TextRect.right = dx; 

	CRect rect, rect1, rect2, temp2;
	int borderx, bordery;
	GetWindowRect(&rect1);
	m_TextView->GetWindowRect(&rect);
	borderx = rect.left - rect1.left;
	bordery = rect.top - rect1.top;
	windowRect = rect;

	SystemParametersInfo(SPI_GETWORKAREA,0,rect2,0);
	int maxhgt = rect2.Height() - 100;
	int maxwid = rect2.Width() - 100;
	if (TextRect.Height()>maxhgt)
	{
		TextRect.bottom = TextRect.top + maxhgt;
	}
	if (TextRect.Width()>maxwid)
		TextRect.right = TextRect.left + maxwid;
	m_TextView->MoveWindow(&TextRect);

	CRect temp (0,0, 40+GetSystemMetrics(SM_CXVSCROLL), 15);
	MapDialogRect(&temp);
	dy = temp.Height() ;
	dx = temp.Width();
	temp2.left = temp.left;
	temp2.top += TextRect.bottom + 10;
	temp2.bottom = temp2.top  + 60;
	temp2.right = temp.left + dx;
	((CWnd*)GetDlgItem(ID_EMESSAGE))->MoveWindow(&temp2);


	temp2.left = temp.left;
	temp2.top += temp2.bottom + 10;
	temp2.bottom = temp2.top  + dy;
	temp2.right = temp.left + dx;
	((CWnd*)GetDlgItem(ID_LCOMMAND))->MoveWindow(&temp2);

	temp2.left = temp2.right + 10;
	temp2.right = TextRect.right - GetSystemMetrics(SM_CXVSCROLL);
	((CWnd*)GetDlgItem(ID_ECOMMAND))->MoveWindow(&temp2);


	windowRect.bottom += TextRect.Height() + temp.Height() + 10 + bordery + GetSystemMetrics(SM_CYHSCROLL) + 10;
	windowRect.right = TextRect.Width() + 2*borderx + 10;
	MoveWindow(windowRect);

	m_pmenu->LoadMenu(m_menuname);
	SetMenu(m_pmenu );
	if ((m_wtype==0) || (m_wtype==1))
	{	
		CMenu* emenu = m_pmenu->GetSubMenu(1);
		emenu->EnableMenuItem(2, MF_BYPOSITION|MF_GRAYED);
		SetMenu(m_pmenu );
/*
.....set the window type checked
*/
		SetFtype(m_ftype);
		if (m_no_undo==0)
			emenu->CheckMenuItem(ID_CUNDO_ENABLED, MF_CHECKED | MF_BYCOMMAND );
		else
			emenu->CheckMenuItem(ID_CUNDO_DISABLED, MF_UNCHECKED | MF_BYCOMMAND );
		CMenu* wmenu = m_pmenu->GetSubMenu(4);
		if (m_mark_flag)
			wmenu->CheckMenuItem(ID_WINDOW_MARK, MF_CHECKED | MF_BYCOMMAND );
		else
			wmenu->CheckMenuItem(ID_WINDOW_MARK, MF_UNCHECKED | MF_BYCOMMAND);
		if (m_syntax_color)
			wmenu->CheckMenuItem(ID_WINDOW_SYNTAXCLR, MF_CHECKED | MF_BYCOMMAND );
		else
			wmenu->CheckMenuItem(ID_WINDOW_SYNTAXCLR, MF_UNCHECKED | MF_BYCOMMAND);
	}
	m_pctl.SetRange( 0, 100);
	m_pctl.SetPos(1);
/*
.....if it is APT description file, check the syntax
*/
	if (m_wtype==2)
		Bad_APTdes();
	char title[UX_MAX_PATH+40];
	if (m_wtype==0)
		sprintf(title, "Main: %s", m_file);
	else
		sprintf(title, "Child: %s", m_file);
	SetWindowText(title);
	if (m_wtype==0)
	{
		Pted_MainDlg = (CPtedMainWindow *) this;
		((CPtedMainWindow *)this)->SetPreFile();
	}
	if ((m_pBatch->m_batch_cnt)&&(m_wtype==0))
	{
		m_syntax_color = m_pBatch->m_syntax_color;
		SetWindowSyntaxClr(m_syntax_color);
		if ((m_pBatch->m_pTextBuffer!=NULL) && (m_pBatch->m_pTextBuffer->GetCurrentSize()>0))
		{
			strcpy(m_file, m_pBatch->m_filen);
			m_strFind = m_pBatch->m_findstr;
			m_strReplace = m_pBatch->m_replacestr;
			m_bCase = m_pBatch->m_case;
			int cursor_pos = m_pBatch->m_cursor_pos;
			sprintf(title, "Main: %s", m_file);
			SetWindowText(title);
			m_TextView->LoadFromBuffer(m_pBatch->m_pTextBuffer);
			m_TextView->Get_TextBuffer()->SetModified(0);
		}
		SetFtype(m_pBatch->m_ftype);
		if (strncmp(m_file, "Untitle", 7)!=0)
			m_openflag = 1;
	}
	else if ((m_file[0]!='\0') && (m_wtype==0) && (m_untitled==0))
		LoadProgram(m_file, 0);

	HINSTANCE hInst = AfxFindResourceHandle(m_menuname, RT_ACCELERATOR);
	m_accel = ::LoadAccelerators(hInst, m_menuname);
	m_TextView->SetFocus( );
	return true;
}
/***********************************************************************
c
c   SUBROUTINE:  PostNcDestroy() 
c
c   FUNCTION:  This function called when Destroy window
c				it delete object pointer.
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CPtedWindow::PostNcDestroy() 
{
	m_pmenu->Detach( );
	delete m_pmenu;
	delete this;
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
BOOL CPtedWindow::PreTranslateMessage(MSG *pMsg) 
{
	if (pMsg->message >= WM_KEYFIRST && pMsg->message <= WM_KEYLAST)
	{
		CWnd* pComWnd = (CWnd*)GetDlgItem(ID_ECOMMAND);
		HWND cmdhnd = pComWnd->GetSafeHwnd();
		if (pMsg->hwnd == cmdhnd)
			OnCommandPreKeys();
		if (TranslateAccelerator(m_hWnd, m_accel, pMsg))
			return TRUE;
//	else if (IsDialogMessage( msg ) ) 
//		return TRUE;
	}
	return FALSE;
}
/***********************************************************************
c
c   SUBROUTINE:  ProgramSaveSelectAs(char *fname, PtedRangeStruct sRange)
c
c   FUNCTION:  This function save a select range of current text in to a file.
c				
c
c   INPUT:  sRange: range structure
c			fname:  filename to save as			
c
c   OUTPUT: none
c
c***********************************************************************
*/
int CPtedWindow::ProgramSaveSelectAs(char *fname, PtedRangeStruct sRange)
{
	char *indx, tmp[UX_MAX_PATH];
/*
.....if filename to file extension of ftype and input filename
*/
	if (fname[0]=='\0')
	{
		if (m_file[0]=='\0')
			return 0;
		else
		{
			strcpy(fname, m_file);
			indx = strchr(fname, '.');
			if (indx!=NULL)
				*indx = '\0';
			if (m_ftype==1)
				strcat(fname, ".cla");
			else if (m_ftype==2)
				strcat(fname, ".pu");
			else if (m_ftype==3)
				strcat(fname, ".cl");
			else if (m_ftype==4)
				strcat(fname, ".as");
			else if (m_ftype==5)
				strcat(fname, ".sim");
		}
	}
/*
.....if file have no extension, added following ftype
*/
	indx = strchr(fname, '.');
	if (indx==NULL)
	{
		if (m_ftype==1)
			strcat(fname, ".cla");
		else if (m_ftype==2)
			strcat(fname, ".pu");
		else if (m_ftype==3)
			strcat(fname, ".cl");
		else if (m_ftype==4)
			strcat(fname, ".as");
		else if (m_ftype==5)
			strcat(fname, ".sim");
	}
	else if (fname[0]=='.')
	{
		strcpy(tmp, m_file);
		indx = strchr(tmp, '.');
		if (indx!=NULL)
			*indx = '\0';
/*
.....using the extension provide in fname
*/
		strcat(tmp, fname);
/*
		if (m_ftype==1)
			strcat(tmp, ".cla");
		else if (m_ftype==2)
			strcat(tmp, ".pu");
		else if (m_ftype==3)
			strcat(tmp, ".cl");
		else if (m_ftype==4)
			strcat(tmp, ".as");
		else if (m_ftype==5)
			strcat(tmp, ".sim");
*/
		strcpy(fname, tmp);
	}
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&sRange, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return 0;
	m_TextView->SaveSelectToFile(fname, m_ftype, ptStart, ptEnd, (int *)this, 1);
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE: LoadProgram(char *filename)
c
c   FUNCTION:  This function load a file into current edit window 
c
c   INPUT:  filename: file to load
c			
c   OUTPUT: none
c
c***********************************************************************
*/
int CPtedWindow::LoadProgram(char *filename, int verify)
{
	FILE *fp = NULL;
	int fileLength = 0;
	char *tmpstr = NULL;

	if (m_processor==0)
	{
		Pted_disply_ProcessWindow("Loading program...", this);
		Pted_Display_as_percent(-50, this);
	}
	CWaitCursor wait;
/*
.....check the extension to decide which type of file it is
*/
	char *pp;
		
	pp = strrchr(filename, '.');
	if (pp!=NULL)
	{
		if (_stricmp(pp, ".cla")==0)
		{
			m_ftype = 1;
			SetFtype(1);
		}
		else if (_strnicmp(pp, ".pu", 3)==0)
		{
			m_ftype = 2;
			SetFtype(2);
		}
		else if ((_stricmp(pp, ".cl")==0)||(_stricmp(pp, ".cln")==0))
		{
			m_ftype = 3;
			SetFtype(3);
		}
		else if (_stricmp(pp, ".as")==0)
		{
			m_ftype = 4;
			SetFtype(4);
		}
		else if (_stricmp(pp, ".sim")==0)
		{
			m_ftype = 5;
			SetFtype(5);
		}
/* 
.....leave the type as it is if the file extension can't tell
		else
		{
			m_ftype = 1;
			SetFtype(1);
		}
*/
	}
/* 
.....leave the type as it is if the file extension can't tell
	else
		SetFtype(1);
*/
	m_TextView->LoadFromFile(filename, m_ftype);
	m_TextView->RedrawWindow();
	Reset_menu();

	if (verify)
	{
		OnConvertBadblocks();
	}
	Pted_Close_ProcessWindow(this);
	return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  OnFileInclude()
c
c   FUNCTION:  This function called when user select "Include"
c				from File Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnFileInclude() 
{
	char FileName[UX_MAX_PATH];
	int i;
	CString CFileName;

	if (m_IncFileBox!=0)
	{
		m_IncFileBox->SetActiveWindow();
		m_IncFileBox->ShowWindow(SW_SHOW);
		return;
	}


	DWORD dwFlags;
	dwFlags = OFN_SHOWHELP | OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;
	LPCTSTR filter = "NC Data Files (*.pu*)|*.pu*|All Files (*.*)|*.*||";
//should we?	LPCTSTR filter = "CL Text Files (*.cla)|*.cla|NC Data Files (*.pu*)|*.pu*|CL Binary Files (*.cl)|*.cl|APT Source Files (*.as, *.cls)|*.as;*.cls|Simulation Files (*.sim)|*.sim|Cutter Files (*.dat)|*.dat|All Files (*.*)|*.*||";
	m_IncFileBox = new PtedIncFileDialog(TRUE, "pu1", NULL, dwFlags,
			filter, this);
	(m_IncFileBox->m_fRange).begin = 1;
	(m_IncFileBox->m_fRange).end = 1;
	(m_IncFileBox->m_fRange).baddress[0] = '\0';
	(m_IncFileBox->m_fRange).bstring[0] = '\0';
	(m_IncFileBox->m_fRange).enumber[0] = '\0';
	(m_IncFileBox->m_fRange).eaddress[0] = '\0';
	(m_IncFileBox->m_fRange).estring[0] = '\0';
//should we?	(m_IncFileBox->m_ofn).nFilterIndex = m_ftype;;
	
	if (m_IncFileBox->DoModal()==IDCANCEL)
	{
		delete m_IncFileBox;
		m_IncFileBox = NULL;
		goto done;
	}
	m_fRange.begin = (m_IncFileBox->m_fRange).begin;
	m_fRange.end = (m_IncFileBox->m_fRange).end ;
	strcpy(m_fRange.baddress, (m_IncFileBox->m_fRange).baddress);
	strcpy(m_fRange.bstring, (m_IncFileBox->m_fRange).bstring);
	strcpy(m_fRange.enumber, (m_IncFileBox->m_fRange).enumber);
	strcpy(m_fRange.eaddress, (m_IncFileBox->m_fRange).eaddress);
	strcpy(m_fRange.estring,(m_IncFileBox->m_fRange).estring);

	CFileName = m_IncFileBox->GetPathName();
	delete m_IncFileBox;
	m_IncFileBox = NULL;

	for (i=0; i<CFileName.GetLength(); i++)
		FileName[i] = CFileName.GetAt(i);
	FileName[i] = '\0';
	ProgramIncludeSelect(FileName, m_fRange);
done:;
	_chdir(Pted_localdir);
}
/***********************************************************************
c
c   SUBROUTINE:  ProgramIncludeSelect(char *FileName, PtedRangeStruct fRange)
c
c   FUNCTION:  This function include a select range of a file into current text
c				
c
c   INPUT:  fRange: range structure
c			FileName:  filename to include			
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::ProgramIncludeSelect(char *FileName, PtedRangeStruct fRange)
{
	CPoint ptStart, ptEnd;
	int ftype;
	char *pp = strrchr(FileName, '.');
	if (pp!=NULL)
	{
		if (_stricmp(pp, ".cla")==0)
		{
			ftype = 1;
		}
		else if (_strnicmp(pp, ".pu", 3)==0)
		{
			ftype = 2;
		}
		else if ((_stricmp(pp, ".cl")==0)||(_stricmp(pp, ".cln")==0))
		{
			ftype = 3;
		}
		else if (_stricmp(pp, ".as")==0)
		{
			ftype = 4;
		}
		else if (_stricmp(pp, ".sim")==0)
		{
			ftype = 5;
		}
		else
		{
			ftype = 1;
		}
	}
	else
		ftype = 1;

	CPtedTextBuffer *include_buf = new CPtedTextBuffer();
	include_buf->LoadFromFile(FileName, ftype);
	include_buf->Get_Range_Pos(&fRange, ptStart, ptEnd);

	if (ptStart!=ptEnd)
		m_TextView->Includebuffer(include_buf, ptStart, ptEnd, PTED_INCLUDE);
	delete include_buf;
	if (ptStart!=ptEnd)
		Update_undo_redomenu(1,0);
}

/***********************************************************************
c
c   SUBROUTINE:  OnIncludeRange()
c
c   FUNCTION:  
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
LRESULT CPtedWindow::OnIncludeRange(WPARAM wParam, LPARAM lParam)
{
	DWORD flags = TED_RANGE_BEGIN1 | TED_RANGE_BEGIN6 | TED_RANGE_BEGIN7
				| TED_RANGE_BEGIN4 | TED_RANGE_BEGIN5 | TED_RANGE_END1
				| TED_RANGE_END2 | TED_RANGE_END3 | TED_RANGE_END4 
				| TED_RANGE_END5;

	if (m_fRangeBox!=0)
	{
		m_fRangeBox->SetActiveWindow();
		m_fRangeBox->ShowWindow(SW_SHOW);
		return 0;
	}

	m_fRangeBox = new PtedRangeBox(this, flags, &(m_IncFileBox->m_fRange));

	if (m_fRangeBox->DoModal()==IDCANCEL)
	{
		delete m_fRangeBox;
		m_fRangeBox = NULL;
		return 0;
	}

	if (m_fRangeBox->m_begin1==1)
		(m_IncFileBox->m_fRange).begin = 1;
	if (m_fRangeBox->m_begin2==1)
		(m_IncFileBox->m_fRange).begin = 2;
	if (m_fRangeBox->m_begin3==1)
		(m_IncFileBox->m_fRange).begin = 3;
	if (m_fRangeBox->m_begin4==1)
		(m_IncFileBox->m_fRange).begin = 4;
	if (m_fRangeBox->m_begin5==1)
		(m_IncFileBox->m_fRange).begin = 5;
	if (m_fRangeBox->m_begin6==1)
		(m_IncFileBox->m_fRange).begin = 6;
	if (m_fRangeBox->m_begin7==1)
		(m_IncFileBox->m_fRange).begin = 7;
	if (m_fRangeBox->m_end1==1)
		(m_IncFileBox->m_fRange).end = 1;
	if (m_fRangeBox->m_end2==1)
		(m_IncFileBox->m_fRange).end = 2;
	if (m_fRangeBox->m_end3==1)
		(m_IncFileBox->m_fRange).end = 3;
	if (m_fRangeBox->m_end4==1)
		(m_IncFileBox->m_fRange).end = 4;
	if (m_fRangeBox->m_end5==1)
		(m_IncFileBox->m_fRange).end = 5;
	int i;
	if (m_fRangeBox->m_estring!="")
	{
		for (i=0; i<(m_fRangeBox->m_estring).GetLength(); i++)
			(m_IncFileBox->m_fRange).estring[i] = m_fRangeBox->m_estring[i];
		(m_IncFileBox->m_fRange).estring[i] = '\0';
	}
	if (m_fRangeBox->m_bstring!="")
	{
		for (i=0; i<(m_fRangeBox->m_bstring).GetLength(); i++)
			(m_IncFileBox->m_fRange).bstring[i] = m_fRangeBox->m_bstring[i];
		(m_IncFileBox->m_fRange).bstring[i] = '\0';
	}
	if (m_fRangeBox->m_enumber!="")
	{
		for (i=0; i<(m_fRangeBox->m_enumber).GetLength(); i++)
			(m_IncFileBox->m_fRange).enumber[i] = m_fRangeBox->m_enumber[i];
		(m_IncFileBox->m_fRange).enumber[i]= '\0';	
	}
	if (m_fRangeBox->m_eaddress!="")
	{
		for (i=0; i<(m_fRangeBox->m_eaddress).GetLength(); i++)
			(m_IncFileBox->m_fRange).eaddress[i] = m_fRangeBox->m_eaddress[i];
		(m_IncFileBox->m_fRange).eaddress[i] = '\0';
	}
	if (m_fRangeBox->m_baddress!="")
	{
		for (i=0; i<(m_fRangeBox->m_baddress).GetLength(); i++)
			(m_IncFileBox->m_fRange).baddress[i] = m_fRangeBox->m_baddress[i];
		(m_IncFileBox->m_fRange).baddress[i] = '\0';
	}
	(m_IncFileBox->m_fRange).bline = m_fRangeBox->m_bline;
	(m_IncFileBox->m_fRange).eline = m_fRangeBox->m_eline;
	delete m_fRangeBox;
	m_fRangeBox = NULL;
	return 1;
}

void CPtedWindow::WinHelp(DWORD dwData, UINT nCmd)
{
/*
.....put the empty function here to avoid window help message from MFC
*/
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileCutter
c
c   FUNCTION:  This function is called when user selects "Cutter File"
c              from File Menu to open a cutter file.
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CPtedWindow::OnFileCutter()
{

	int stat, ans;
	CPtedChildWindow* child;
	CString CFileName;

	char msg[400], fname[UX_MAX_PATH];
	stat = 0;

	if (m_wtype!=0)
	{
		if (m_TextView->IsModified())
		{
			sprintf(msg, "Do you want to save changes made to %s?", m_file);
			ans = MessageBox(msg, "Question?", MB_YESNOCANCEL);
			if (ans == IDYES)
			{
				if (m_openflag==1)
					stat = ProgramSaveAs(m_file);
				else
				{
					char FileName[UX_MAX_PATH];
					FileName[0] = '\0';
					stat = browsefile("CL Text Files (*.cla)\0*.cla\0NC Data Files (*.pu*)\0*.pu*\0CL Binary Files (*.cl)\0*.cl\0APT Source Files (*.as, *.cls)\0*.as;*.cls\0Simulation Files (*.sim)\0*.sim\0Cutter Files (*.dat)\0*.dat\0All Files (*.*)\0*.*\0\0", FileName,0, m_ftype,FALSE);
					if (stat==0)
						return;
					stat = ProgramSaveAs(FileName);
				}
			}
			else if (ans == IDCANCEL)
				stat = -1;
		}
		if (stat!=0)
			return;
	}

	DWORD dwFlags;
	dwFlags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;
	dwFlags |= OFN_ENABLETEMPLATE ;
	LPCTSTR filter = "Data Source Files (*.dat)|*.dat|All Files (*.*)|*.*|";
	PtedFileDialog *fdlg = new PtedFileDialog(TRUE, "dat", NULL, dwFlags,
			filter, this);
	(fdlg->m_ofn).lpTemplateName = MAKEINTRESOURCE (IDD_PTD_FILEDLG);
	fdlg->m_verify = 0;
	if (fdlg->DoModal()==IDCANCEL)
	{
		delete fdlg;
		goto done;
	}

	CFileName = fdlg->GetPathName();
	strcpy (fname, CFileName.GetBuffer(1));
	CFileName.ReleaseBuffer();

	if (m_wtype!=0)
	{
		strcpy(m_file, fname);
		LoadCutterFile(m_file);
		sprintf(msg, "Child: Cutter File %s", m_file);
		SetWindowText(msg);
		m_openflag = 1;
	}
	else
	{
		if (m_pParent==NULL)
			child = new CPtedChildWindow(Pted_MainDlg, fname, 0, 1);
		else
			child = new CPtedChildWindow(m_pParent, fname, 0, 1);
		child->Create(IDD_WINDOWDIALOG);;
		child->ShowWindow(TRUE);
		Pted_MainDlg->Add_Child(child);
		child->LoadCutterFile(fname);
		sprintf(msg, "Child: Cutter File %s", m_file);
		child->SetWindowText(msg);
		child->m_openflag = 1;
	}
	delete fdlg;
done:;
	_chdir(Pted_localdir);
	return;
}
/***********************************************************************
c
c   SUBROUTINE: LoadCutterFile(char *filename)
c
c   FUNCTION:  This functions loads a cutter file into current edit window 
c
c   INPUT:  filename: file to load
c
c   OUTPUT: none
c
c***********************************************************************
*/
int CPtedWindow::LoadCutterFile(char *filename)
{
	int err = 0;
	char msg[256];

	if (m_wtype==0)
	{
		CPtedTextBuffer *temp_buf = new CPtedTextBuffer();
		temp_buf->LoadCutterFile(filename, msg, &err);
		if (err)
		{
			MessageBox(msg, "Loading Cutter File Error", MB_OK);
		}
		delete temp_buf;
	}
	else
	{
		m_TextView->LoadCutterFile(filename);
		SetFtype(6);
	}
	return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  Disp_Msg(char *msg, int flag==1)
c
c   FUNCTION:  This function is display a messge
c
c   INPUT:  msg: message string to display
c			flag: 1: error messge
c				  2: warning message
c				  3. info message
c
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::Disp_Msg(char *msg, int flag)
{
	char buf[500];
	if (m_actcom==0)
	{
		switch(flag)
		{
		case 1: 
			MessageBox(msg, "Error!", MB_OK);
			break;
		case 2:
			MessageBox(msg, "Warning!", MB_OK);
			break;
		case 3:
			MessageBox(msg, "Info!", MB_OK);
			break;
		}
	}
	else
	{
		switch(flag)
		{
		case 1: 
			strcpy(buf, "Error: ");
			break;
		case 2:
			strcpy(buf, "Warning: ");
			break;
		case 3:
			strcpy(buf, "Info: ");
			break;
		default:
			buf[0] = '\0';
		}
		strcat(buf, msg);
		strcat(buf, "\r\n");
		CEdit *ewin = (CEdit*)GetDlgItem(ID_EMESSAGE);
		int spos = ewin->GetWindowTextLength() + 1;
		ewin->SetSel(spos, spos);
		ewin->ReplaceSel(buf);
	}
}
/***********************************************************************
c
c   SUBROUTINE: ProgramLoadSelect(char *FileName, PtedRangeStruct sRange)
c
c   FUNCTION:  This function load a file in apecified range into current edit window 
c
c   INPUT:  filename: file to load
c			sRange:   range structure
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::ProgramLoadSelect(char *FileName, PtedRangeStruct sRange)
{
	m_TextView->OnCloseReset();
	ProgramIncludeSelect(FileName, sRange);
}
/***********************************************************************
c
c   SUBROUTINE:  LoadAPTdes(char *fileName)
c				
c   FUNCTION:  Load APT description file
c
c   INPUT:  filename: APT description file to Load
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void CPtedWindow::LoadAPTdes(char *fileName)
{
	CPtedChildWindow* child;
	if (m_pParent==NULL)
		child = new CPtedChildWindow(Pted_MainDlg, fileName, 1, 2);
	else
		child = new CPtedChildWindow(m_pParent, fileName, 1, 2);
	child->Create(IDD_WINDOWDIALOG);;
	child->ShowWindow(TRUE);
	if (m_pParent==NULL)
	{
		Pted_MainDlg->Add_Child(child);
	}
	else
		((CPtedMainWindow*)m_pParent)->Add_Child(child);

}
/***********************************************************************
c
c   SUBROUTINE:  Bad_APTdes()
c				
c   FUNCTION:  Check the current file as APT description file
c				and display bad statments if have any.
c
c   INPUT:  None
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void CPtedWindow::Bad_APTdes()
{
	CWaitCursor wait;
	CPtedChildWindow* child;
	if (m_pParent==NULL)
		child = new CPtedChildWindow(Pted_MainDlg, "Bad APT description", 0, 3);
	else
		child = new CPtedChildWindow(m_pParent, "Bad APT description", 0, 3);
	child->Create(IDD_WINDOWDIALOG);;
	int stat = m_TextView->BadCommand(child->m_TextView);
	if (stat)
	{
		child->ShowWindow(TRUE);
		if (m_pParent==NULL)
		{
			Pted_MainDlg->Add_Child(child);
		}
		else
			((CPtedMainWindow*)m_pParent)->Add_Child(child);
		child->m_TextView->Get_TextBuffer()->SetModified(0);
	}
	else
	{
		delete child;
	}
}


void CPtedWindow::Reset_open()
{
	m_openflag = 1;
}

/***********************************************************************
c
c   SUBROUTINE:  OnViewStatus
c
c   FUNCTION:  This function called when user select "Status"
c				from View Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnViewStatus()
{
	CPtedApp *app = (CPtedApp*)AfxGetApp();
	CPtedMainWindow *MainDlg = (CPtedMainWindow*)(app->GetMainWnd());
	
	MainDlg->ViewStatus(this);
	m_TextView->RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  OnToggleBookmark(UINT nCmdID)
c
c   FUNCTION:  Callback function for 'Book mark #"
c
c   INPUT:  nCmdID: command ID
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnToggleBookmark(UINT nCmdID)
{
	m_TextView->OnToggleBookmark (nCmdID);
	m_TextView->RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  OnGoBookmark(UINT nCmdID)
c
c   FUNCTION:  Callback function for 'Goto Book mark #"
c
c   INPUT:  nCmdID: command ID
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnGoBookmark(UINT nCmdID)
{
	m_TextView->OnGoBookmark (nCmdID);
	m_TextView->RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  OnClearBookmarks()
c
c   FUNCTION:  Callback function for 'Clear Bookmark"
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnClearBookmarks()
{
	m_TextView->OnClearBookmarks ();
	m_TextView->RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  OnToggleBookmark()
c
c   FUNCTION:  Callback function for 'Bookmark"
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnToggleBookmark()
{
	m_TextView->OnToggleBookmark ();
	m_TextView->RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  OnClearAllBookmarks()
c
c   FUNCTION:  Callback function for 'Clear all Bookmark"
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnClearAllBookmarks()
{
	m_TextView->OnClearAllBookmarks ();
	m_TextView->RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  OnNextBookmark()
c
c   FUNCTION:  Callback function for 'Goto Next Bookmark"
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnNextBookmark()
{
	m_TextView->OnNextBookmark ();
	m_TextView->RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  OnPrevBookmark()
c
c   FUNCTION:  Callback function for 'Goto Prev Bookmark"
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnPrevBookmark()
{
	m_TextView->OnPrevBookmark ();
	m_TextView->RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  SetViewFocus()
c
c   FUNCTION:  Set the window focus to the edit view
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::SetViewFocus()
{
	m_TextView->SetFocus();
}
/***********************************************************************
c
c   SUBROUTINE:  OnWindowBookMark()
c
c   FUNCTION:  Set the window display include left margin
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnWindowBookMark()
{
	if (m_mark_flag)
	{
		m_mark_flag = 0;
	}
	else
		m_mark_flag = 1;
	m_TextView->SetBookMarkWindow(m_mark_flag);
	ReAdjustWindow();
	m_TextView->UpdateCaret();
	m_TextView->FlushUndoGroup();
	CMenu* pmenu = GetMenu();
	CMenu* wmenu = pmenu->GetSubMenu(4);
	if (m_mark_flag)
		wmenu->CheckMenuItem(ID_WINDOW_MARK, MF_CHECKED | MF_BYCOMMAND );
	else
		wmenu->CheckMenuItem(ID_WINDOW_MARK, MF_UNCHECKED | MF_BYCOMMAND);
}
/***********************************************************************
c
c   SUBROUTINE:  OnWindowSyntaxClr()
c
c   FUNCTION:  Set the window display with syntax color
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnWindowSyntaxClr()
{
	if (m_syntax_color)
	{
		m_syntax_color = 0;
	}
	else
		m_syntax_color = 1;
	SetWindowSyntaxClr(m_syntax_color);
}

/***********************************************************************
c
c   SUBROUTINE:  SetWindowSyntaxClr(flag)
c
c   FUNCTION:  Set the window display with syntax color
c
c   INPUT:  color: syntax color flag
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::SetWindowSyntaxClr(int flag)
{
	m_syntax_color = flag;
	m_TextView->SetSyntaxClrWindow(m_syntax_color);
	m_TextView->RedrawWindow();
	CMenu* pmenu = GetMenu();
	CMenu* wmenu = pmenu->GetSubMenu(4);
	if (m_syntax_color)
		wmenu->CheckMenuItem(ID_WINDOW_SYNTAXCLR, MF_CHECKED | MF_BYCOMMAND );
	else
		wmenu->CheckMenuItem(ID_WINDOW_SYNTAXCLR, MF_UNCHECKED | MF_BYCOMMAND);
}

/***********************************************************************
c
c   SUBROUTINE:  ReAdjustWindow()
c
c   FUNCTION:  Adjust window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::ReAdjustWindow()
{
	SizeDialogItem();
	m_TextView->RedrawWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  OnEditEnableCUndo()
c
c   FUNCTION:  Called back for 'Active conversion undo'
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnEditEnableCUndo()
{
	if (m_no_undo==0)
	{
		m_no_undo = m_TextView->SetUndoFlag(1);
		Update_undo_redomenu(0, 0);
	}
	else
	{
		m_no_undo = m_TextView->SetUndoFlag(0);
	}

	CMenu* pmenu = GetMenu();
	CMenu* wmenu = pmenu->GetSubMenu(1);
	if (m_no_undo==0)
		wmenu->CheckMenuItem(ID_CUNDO_ENABLED, MF_CHECKED | MF_BYCOMMAND );
	else
		wmenu->CheckMenuItem(ID_CUNDO_ENABLED, MF_UNCHECKED | MF_BYCOMMAND );
}
/***********************************************************************
c
c   SUBROUTINE:  Display_as_percent
c
c   FUNCTION:  This function display process as percent num
c
c   INPUT:  num: percentage to display
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::Display_as_percent(int num)
{
	MSG msg;
	int stat;
	char label[UX_MAX_PATH];
	if (m_processor==0)
		return;
	int pos = abs(num);
	m_pctl.SetPos(pos);

	if (num<0)
		strcpy(label, "Processing ...");
	else
		sprintf(label, "%d%% Completed ...", num);
	if (pos-m_curpos>=5)
	{
		SetDlgItemText(ID_LPROCESSOR2, label);
		m_curpos = pos;
		m_pctl.UpdateWindow();
		while (::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE))
		{
			if ((msg.message==WM_PAINT) || (msg.message==WM_SIZING)
				|| (msg.message==WM_SIZE) || (msg.message==WM_MOVE)
				|| (msg.message==WM_MOVING) || (msg.message==WM_WINDOWPOSCHANGED))
			{
				if (!GetMessage(&msg, NULL, msg.message, msg.message))
					return;
				DispatchMessage(&msg);
			}
			else
			{
				stat = ::PeekMessage(&msg, msg.hwnd, msg.message, msg.message, PM_REMOVE);
			}
		}
	}
}
/***********************************************************************
c
c   SUBROUTINE:  ShowProcessWindow(char *title)
c
c   FUNCTION:  This function show a process window
c
c   INPUT:  char *title: label before processor
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::ShowProcessWindow(char *title)
{
	if (m_processor)
		return;
	CRect rect;
	GetWindowRect(&rect);
	CRect temp (0,0, 10, 10);
	MapDialogRect(&temp);
	int dy = temp.Height() ;
	rect.bottom = rect.bottom + dy;
	m_processor = 1;
	MoveWindow(&rect);
	SetDlgItemText(ID_LPROCESSOR1, title);
	SetDlgItemText(ID_LPROCESSOR2, "1% Completed ...");
	m_curpos = 0;
	UpdateWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  CloseProcessWindow()
c
c   FUNCTION:  This function close a process window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::CloseProcessWindow()
{
	if (m_processor==0)
		return;
	CRect rect;
	GetWindowRect(&rect);
	CRect temp (0,0, 10, 10);
	MapDialogRect(&temp);
	int dy = temp.Height() ;
	rect.bottom = rect.bottom - dy;
	m_processor = 0;
	MoveWindow(&rect);
	UpdateWindow();
	m_curpos = 0;
}
void CPtedWindow::SelParentLine(int line)
{
	((CPtedWindow*)m_pParent)->SelectLine(line);
}
void CPtedWindow::SelectLine(int line)
{

	m_TextView->SelectLine(line);
}
void CPtedWindow::OnFilePrint() 
{
	m_TextView->OnFilePrint();
}

void CPtedWindow::OnFilePageSetup() 
{
	m_TextView->OnFilePageSetup();
}
/***********************************************************************
c
c   SUBROUTINE:  Pted_disply_ProcessWindow(title)
c
c   FUNCTION:  This function display a process awindow
c
c   INPUT:  char *title: label before processor
c			parent: parent window
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void Pted_disply_ProcessWindow(char *title, CPtedWindow* parent)
{
	if (parent!=NULL)
		parent->ShowProcessWindow(title);
	else
		Pted_disply_ProcessDlg(title);
}

/***********************************************************************
c
c   SUBROUTINE:  Display_as_percent
c
c   FUNCTION:  This function display process as percent num
c
c   INPUT:  num: percentage to display
c			parent: parent window
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void Pted_Display_as_percent(int num, CPtedWindow* parent)
{
	static int save_per = -1;
	if (parent!=NULL)
		parent->Display_as_percent(num);
	else
	{
/*
.....only do it when percent change because Pted_Display_Dlg_percent will
.....ignore number but check time message, we don't wnt to check it the message
.....too often because it takes a lot of time
*/
		if (num!=save_per)
		{
			Pted_Display_Dlg_percent(num);
			save_per = num;
		}
	}
}
/***********************************************************************
c
c   SUBROUTINE:  Display_as_percent2
c
c   FUNCTION:  This function display process as percent num
c			This function is the same as Display_as_percent except
c			it could pass from c function (not window cpp function)
c
c   INPUT:  num: percentage to display
c			parent: parent window
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void Pted_Display_as_percent2(int num, int* parent)
{
	static int save_per = -1;
	if (parent!=NULL)
	{
		((CPtedWindow*)parent)->Display_as_percent(num);
	}
/*
.....display processor dialog
*/
	else
	{
/*
.....only do it when percent change because Pted_Display_Dlg_percent will
.....ignore number but check time message, we don't wnt to check it the message
.....too often because it takes a lot of time
*/
		if (num!=save_per)
		{
			Pted_Display_Dlg_percent(num);
			save_per = num;
		}
	}
}
/***********************************************************************
c
c   SUBROUTINE:  Display_as_percent3
c
c   FUNCTION:  This function display process as percent num
c			This function is the same as Display_as_percent except
c			it could pass from functure function (not window cpp function)
c
c   INPUT:  num: percentage to display
c			parent: parent window
c
c   OUTPUT: none
c
c***********************************************************************
*/extern "C" void pted_display_as_percent3(int *perc, int *bufpt)
{
	CPtedWindow* window;
	static int save_per = -1;
	CPtedTextBuffer *buf = (CPtedTextBuffer *) bufpt;
	if (buf!=NULL)
	{
		if (buf->m_attview!=NULL)
		{
			window = (CPtedWindow *)((CPtedTextView*)(buf->m_attview))->GetParent();
		}
		if (window!=NULL)
			window->Display_as_percent(*perc);
		else
		{
/*
.....only do it when percent change because Pted_Display_Dlg_percent will
.....ignore number but check time message, we don't wnt to check it the message
.....too often because it takes a lot of time
*/
			if (*perc!=save_per)
			{
				Pted_Display_Dlg_percent(*perc);
				save_per = *perc;
			}
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  Pted_Close_ProcessWindow
c
c   FUNCTION:  This function close the process window
c
c   INPUT:  parent: parent window
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void Pted_Close_ProcessWindow(CPtedWindow* parent)
{
	if (parent!=NULL)
		parent->CloseProcessWindow();
	else
		Pted_Close_ProcessDlg();
}

