/***********************************************************************
**
**   FILE NAME: GenpromptView.cpp
**
**   CONTAINS:
**		CGenpromptView::CGenpromptView()
**		CGenpromptView::~CGenpromptView()
**		CGenpromptView::PreCreateWindow()
**		CGenpromptView::OnInitialUpdate()
**		CGenpromptView::OnCtlColor()
**		CGenpromptView::OnFileExit() 
**		CGenpromptView::OnFileOpen()
**		CGenpromptView::OnOptionOptions()
**		CGenpromptView::OnPworksRun() 
**
**    COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
**          All Rights Reserved
**     MODULE NAME AND RELEASE LEVEL
**			genpromptView.cpp , 24.2
**    DATE AND TIME OF LAST  MODIFICATION
**			05/05/14 , 14:46:02
**
**********************************************************************/
#include "pwenv.h"
#include "pwstdafx.h"
#include "genprompt.h"

#include "PostworksDoc.h"
#include "promptres.h"
#include "GenpromptView.h"
#include "PwNTAboutDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int genprompt(char *comstr);
extern CWnd *Pw_maindlg;
extern "C" int Pw_dispmsg(char *msg, int flag);

/////////////////////////////////////////////////////////////////////////////
// CGenpromptView

IMPLEMENT_DYNCREATE(CGenpromptView, CFormView)

BEGIN_MESSAGE_MAP(CGenpromptView, CFormView)
	//{{AFX_MSG_MAP(CGenpromptView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		ON_WM_CTLCOLOR()
		ON_COMMAND(ID_FILE_EXIT, OnFileExit)
		ON_COMMAND(ID_FILE_OPEN, OnFileOpen)
		ON_COMMAND(ID_HELP_CONTENTS, OnHelpContents)
		ON_COMMAND(ID_HELP_ABOUT, OnHelpAbout)
		ON_BN_CLICKED(ID_GENPROMPT_RUN, OnGenpromptRun)
		ON_WM_DROPFILES()
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CGenpromptView construction/destruction

/***********************************************************************
c
c   SUBROUTINE: CGenpromptView()	
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CGenpromptView::CGenpromptView()
	: CFormView(CGenpromptView::IDD)
{
	//{{AFX_DATA_INIT(CGenpromptView)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// TODO: add construction code here
/*
.....initial backgroup brush for Message box control
*/
	m_pEditBkBrush = new CBrush(RGB(255, 255, 255));
}

/***********************************************************************
c
c   SUBROUTINE:  ~CGenpromptView()
c   FUNCTION:  Deconstructor
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CGenpromptView::~CGenpromptView()
{
	delete m_pEditBkBrush;
}

void CGenpromptView::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CGenpromptView)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BOOL CGenpromptView::PreCreateWindow(CREATESTRUCT& cs)
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
void CGenpromptView::OnInitialUpdate()
{
	CFormView::OnInitialUpdate();
	GetParentFrame()->RecalcLayout();
	ResizeParentToFit();

	Pw_maindlg = this; 
	GetDlgItem(IDC_GENPROMPT_NAME)->SetFocus();
	DragAcceptFiles(TRUE);	
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
void CGenpromptView::OnFileExit() 
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
void CGenpromptView::OnFileOpen() 
{
	CString DirName, FileName;
	char *fname, *temp, fnames[80000], files[80000];
	int nc;
	LPCTSTR filter = "Text files (*.txt)|*.txt|All Files (*.*)|*.*||";		
	DWORD dwFlags = OFN_ALLOWMULTISELECT | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;

	files[0] = '\0';
	CFileDialog *filedlg = new CFileDialog(TRUE, "GenPrompt", NULL, dwFlags,
			filter, this);
	filedlg->m_ofn.lpstrFile = files;
	filedlg->m_ofn.nMaxFile = 80000;
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
	GetDlgItem(IDC_GENPROMPT_NAME)->SetWindowText(FileName);
	delete filedlg;	
}

/***********************************************************************
c
c   SUBROUTINE:  OnGenpromptRun
c
c   FUNCTION:  Called when "Run" button is pushed
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CGenpromptView::OnGenpromptRun() 
{
	int i;
	char *indx, fnames[10000], filename[UX_MAX_PATH], msg[UX_MAX_PATH];
	char defdir[UX_MAX_PATH],fname[UX_MAX_PATH];

	HCURSOR cursor;
	cursor = LoadCursor(NULL, IDC_WAIT);
	if (cursor!=NULL)
		SetCursor(cursor);
	ShowCursor(TRUE);

	GetCurrentDirectory(UX_MAX_PATH, defdir);
	GetDlgItem(IDC_GENPROMPT_NAME)->GetWindowText(fnames, 7999);
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
		for (i=strlen(fname);i<UX_MAX_PATH;i++) fname[i] = ' ';
		genprompt(fname);
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
HBRUSH CGenpromptView::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
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
void CGenpromptView::OnHelpAbout()
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
void CGenpromptView::OnHelpContents()
{
	char com[UX_MAX_PATH+40], buf[UX_MAX_PATH+40];
	int stat;
	strcpy(com, "%acroread% %NCL_DOC%\\postworks.pdf");
	sprintf(buf,"start \"Postworks\" /B %s", com); 
	stat = system(buf);
	if (stat > 0) 
	{
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
void CGenpromptView::OnDropFiles(HDROP hDropInfo)
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
	GetDlgItem(IDC_GENPROMPT_NAME)->SetWindowText(FileName);
	OnGenpromptRun();
}

/////////////////////////////////////////////////////////////////////////////
// CGenpromptView diagnostics

#ifdef _DEBUG
void CGenpromptView::AssertValid() const
{
	CFormView::AssertValid();
}

void CGenpromptView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}

CPostworksDoc* CGenpromptView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CPostworksDoc)));
	return (CPostworksDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CGenpromptView message handlers
