/***********************************************************************
**
**   FILE NAME: PWorksView.cpp
**
**   CONTAINS:
**		CPWorksView::CPWorksView()
**		CPWorksView::~CPWorksView()
**		 CPWorksView::PreCreateWindow()
**		CPWorksView::OnInitialUpdate()
**		CPWorksView::OnCtlColor()
**		CPWorksView::OnFileExit() 
**		CPWorksView::OnFileOpen()
**		CPWorksView::OnOptionOptions()
**		CPWorksView::OnPworksRun() 
**
**    COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
**          All Rights Reserved
**     MODULE NAME AND RELEASE LEVEL
**			PWorksView.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			10/12/15 , 09:13:06
**
**********************************************************************/
#include "direct.h"
#include "pwenv.h"
#include "pwstdafx.h"
#include "Pworks.h"
#include "PostworksDoc.h"
#include "PWorksView.h"
#include "PWfunc.h"
#include "PWorksOptDlg.h"
#include "PWmessagebox.h"
#include "PwNTAboutDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern CWnd *Pw_maindlg;

extern "C" int Pw_dispmsg(char *msg, int flag);

/////////////////////////////////////////////////////////////////////////////
// CPWorksView

IMPLEMENT_DYNCREATE(CPWorksView, CFormView)

BEGIN_MESSAGE_MAP(CPWorksView, CFormView)
	//{{AFX_MSG_MAP(CPWorksView)
		ON_WM_CTLCOLOR()
		ON_COMMAND(ID_FILE_EXIT, OnFileExit)
		ON_COMMAND(ID_FILE_OPEN, OnFileOpen)
		ON_COMMAND(ID_OPTION_OPTIONS, OnOptionOptions)
		ON_COMMAND(ID_HELP_CONTENTS, OnHelpContents)
		ON_COMMAND(ID_HELP_ABOUT, OnHelpAbout)
		ON_BN_CLICKED(ID_PWORKS_RUN, OnPworksRun)
		ON_WM_DROPFILES()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPWorksView construction/destruction

/***********************************************************************
c
c   SUBROUTINE: CPWorksView()	
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPWorksView::CPWorksView()
	: CFormView(CPWorksView::IDD)
{
	//{{AFX_DATA_INIT(CPWorksView)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// TODO: add construction code here
	m_DirName[0] = '\0';
/*
.....initial backgroup brush for Message box control
*/
	m_pEditBkBrush = new CBrush(RGB(255, 255, 255));
}

/***********************************************************************
c
c   SUBROUTINE:  ~CPWorksView()
c   FUNCTION:  Deconstructor
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPWorksView::~CPWorksView()
{
	delete m_pEditBkBrush;
}

void CPWorksView::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPWorksView)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BOOL CPWorksView::PreCreateWindow(CREATESTRUCT& cs)
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
void CPWorksView::OnInitialUpdate()
{
	CFormView::OnInitialUpdate();
	GetParentFrame()->RecalcLayout();
	ResizeParentToFit();

	((CEdit*)GetDlgItem(IDC_POSTWORKS_MESSAGE))->LimitText(10000000);
	Pw_maindlg = this;
	GetDlgItem(IDC_PWORKS_NAME)->SetFocus();
	DragAcceptFiles(TRUE);	
	return; 
}
/***********************************************************************
c
c   SUBROUTINE:  OnFileExit
c
c   FUNCTION:  Called when "exit" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CPWorksView::OnFileExit() 
{
	PostMessage(WM_QUIT);
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileOpen
c
c   FUNCTION:  Called when "Open" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CPWorksView::OnFileOpen() 
{
	CString DirName, FileName;
	char *fname, *temp, fnames[80000], files[80000];
	int nc;
	LPCTSTR filter = "Clfiles (*.cl, *.cln)|*.cl;*.cln|APT files (*.cla, *.as, *.cls)|*.cla;*.as;*.cls|Catia files (*.aptsource, *.clfile)|*.aptsource;*.clfile|MasterCam files (*.nci)|*.nci|All Files (*.*)|*.*||";		
	DWORD dwFlags = OFN_ALLOWMULTISELECT | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;

	files[0] = '\0';
	CFileDialog *filedlg = new CFileDialog(TRUE, "Pworks", NULL, dwFlags,
			filter, this);
	filedlg->m_ofn.lpstrFile = files;
	filedlg->m_ofn.nMaxFile = 80000;
/*
.....get options and initialize browser filter type
*/
	int iopt[48];
	double rusr[50];
	char copt[21][256];
	pw_getoptions(iopt, rusr, copt);

	if (iopt[41]==2 || iopt[41] == 4)
		filedlg->m_ofn.nFilterIndex = 2;
	else if (iopt[41]==1)
		filedlg->m_ofn.nFilterIndex = 1;
	else if ((iopt[41]==3)||(iopt[41]==5))
		filedlg->m_ofn.nFilterIndex = 3;
	else if (iopt[41]==6)
		filedlg->m_ofn.nFilterIndex = 4;
	else
		filedlg->m_ofn.nFilterIndex = 5;

	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString ftitle = filedlg->GetFileTitle();
	if (ftitle!="")
	{
		FileName = filedlg->GetPathName();
	}
	else
/*
......multiple file select
*/
	{
		DirName = filedlg->GetPathName();
		nc = DirName.GetLength();
		temp = DirName.GetBuffer(nc);
		strcpy(m_DirName,temp);
		fnames[0] = '\0';
		fname = (filedlg->m_ofn).lpstrFile + (filedlg->m_ofn).nFileOffset;
		while (fname[0]!='\0')
		{
			strcat(fnames, "\"");
//			strcat(fnames, temp);
//			strcat(fnames, "\\");
			strcat(fnames, fname);
			strcat(fnames, "\" ");
			nc = strlen(fname);
			fname = fname + nc + 1;
		}
		FileName = fnames;
	}		
	GetDlgItem(IDC_PWORKS_NAME)->SetWindowText(FileName);
	delete filedlg;	
}

/***********************************************************************
c
c   SUBROUTINE:  OnOptionOptions
c
c   FUNCTION:  Called when "Option" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CPWorksView::OnOptionOptions() 
{
	PWorksOptDlg dlgopt;
	dlgopt.DoModal();
}
/***********************************************************************
c
c   SUBROUTINE:  OnPworksRun
c
c   FUNCTION:  Called when "Run" button is pushed
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CPWorksView::OnPworksRun() 
{
	int i;
	char *indx, fnames[80000], filename[UX_MAX_PATH];
	HCURSOR cursor;
	char defdir[UX_MAX_PATH],fname[UX_MAX_PATH];

	cursor = LoadCursor(NULL, IDC_WAIT);
	if (cursor!=NULL)
		SetCursor(cursor);
	ShowCursor(TRUE);

	if (m_DirName[0] != '\0') strcpy(defdir,m_DirName);
	else GetCurrentDirectory(UX_MAX_PATH, defdir);
	GetDlgItem(IDC_PWORKS_NAME)->GetWindowText(fnames, 7999);
/*
.....allow multiple files run
*/
	if (fnames[0]=='\0')
	{
		Pw_dispmsg("Empty filename!", 1);
		return;
	}
	while (fnames[0]!='\0')
	{
		indx = strchr(fnames, '\"');
		if (indx==NULL)
		{
			strcpy(filename, fnames);
			fnames[0] = '\0';
		}
		else
		{
			strcpy(fnames, indx+1);
			indx = strchr(fnames, '\"');
			if (indx == NULL)
			{
				Pw_dispmsg("Wrong syntax in input filename! Quotes not match!", 1);
				return;
			}
			*indx = '\0';
			strcpy(filename, fnames);
			strcpy(fnames, indx+1);
		}
/*
.....remove preceding and trailing spaces
*/
		for (i=0; i<strlen(filename); i++)
		{
			if (filename[i]!=' ') break;
		}
		strcpy(filename, &(filename[i]));
		for (i=strlen(filename); i>0; i--)
		{
			if (filename[i-1]==' ')
				filename[i-1] = '\0';
			else
				break;
		}
		if (filename[0]=='\0')
			continue;
/*
......check if filename with directory, if yes, remeber the directory as default
......if not, cat the default directory
*/
		indx = strrchr(filename, '\\');
		if (indx==0)
		{
/*
......cat default directory
*/
			strcpy(fname, defdir);
			strcat(fname, "\\");
			strcat(fname, filename);
		}
		else
		{
/*
......save the directory as defdir
*/
			strcpy(fname, filename);
			*indx = '\0';
			strcpy(defdir, filename);
		}
		if (defdir[0] != '\0') _chdir(defdir);
		pw_putpfile(fname);
		pw_saveopt();
		pworks();
/*
......re initialize postworks data
......ready to rerun
*/
		pw_reinit();
	}
	cursor = LoadCursor(NULL, IDC_ARROW);
	if (cursor!=NULL)
		SetCursor(cursor);
	ShowCursor(TRUE);
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
HBRUSH CPWorksView::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	CWnd* pMessageWnd = (CWnd*)GetDlgItem(IDC_POSTWORKS_MESSAGE);
	HWND cmdhnd = pMessageWnd->GetSafeHwnd();
	HWND whnd = pWnd->GetSafeHwnd();
	switch (nCtlColor)
	{
		case CTLCOLOR_EDIT:
/*
......we set Message edit control readonly, so window treat it
......as static label.
*/
		case CTLCOLOR_STATIC:
			if (cmdhnd==whnd)
			{
				pDC->SetTextColor(RGB(20, 20, 255));
				pDC->SetBkColor(RGB(255, 255, 255));
				return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
			}
			else
				return CFormView::OnCtlColor(pDC, pWnd, nCtlColor);
		default:
				return CFormView::OnCtlColor(pDC, pWnd, nCtlColor);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  OnHelpAbout()
c
c   FUNCTION:  This function called when user select "Help->About"
c				from Main Menu, it will display an About dialog
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPWorksView::OnHelpAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}

/***********************************************************************
c
c   SUBROUTINE:  OnHelpContents()
c
c   FUNCTION:  This function called when user select "Help->Contents"
c				from Main Menu, it will spawn a command
c				"acroread %NCL_DOC%\postworks.pdf"
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPWorksView::OnHelpContents()
{
	char com[UX_MAX_PATH], buf[UX_MAX_PATH];
	int stat;
	strcpy(com, "%acroread% %NCL_DOC%\\postworks.pdf");
	sprintf(buf,"start \"Postworks\" /B %s", com); 
	stat = system(buf);
	if (stat > 0) 
	{
		com[60] = '\0';
		sprintf(buf,"Can't execute \"%s\"", com); 
		MessageBox(buf, "Command Spawn Error",MB_OK);
	}
}
/***********************************************************************
**
**   FUNCTION: OnDropFiles(HDROP hDropInfo)
**
**		The framework calls this member function when the user releases 
**		the left mouse button over a Postworks window as the recipient of dropped files.
**   
**	 INPUT:  hDropInfo: A pointer to an internal data structure that describes 
**			the dropped files. 
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CPWorksView::OnDropFiles(HDROP hDropInfo)
{
	CString FileName = "";
    UINT i,j,knc;
	char buf[MAX_PATH+20];
    UINT nFiles = ::DragQueryFile(hDropInfo, (UINT) -1, NULL, 0);
    for (i = 0; i < nFiles; i++)
    {
        char szFileName[_MAX_PATH];
        ::DragQueryFile(hDropInfo, i, szFileName, _MAX_PATH);
		FileName = FileName + "\"" + szFileName + "\" ";
    } 
    ::DragFinish(hDropInfo);
	GetDlgItem(IDC_PWORKS_NAME)->SetWindowText(FileName);
	OnPworksRun();
}

/////////////////////////////////////////////////////////////////////////////
// CPWorksView diagnostics

#ifdef _DEBUG
void CPWorksView::AssertValid() const
{
	CFormView::AssertValid();
}

void CPWorksView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}

CPostworksDoc* CPWorksView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CPostworksDoc)));
	return (CPostworksDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CPWorksView message handlers
