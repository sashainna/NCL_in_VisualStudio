/************************************************************************
c
c   FILE NAME: DefineDlg.cpp
c
c	 CONTAINS: 
c		Functions for CDefineDlg class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       DefineDlg.cpp , 25.4
c    DATE AND TIME OF LAST  MODIFICATION
c       01/18/16 , 08:26:16
c
c**********************************************************************
*/
// DefineDlg.cpp : implementation file
//

#include "toolibstdafx.h"
#include "toolib.h"
#include "DefineDlg.h"
#include "xenv1.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern "C" void tool_get_dirname(CWnd* parent, char *title, char* dnam, int *nc,
	char *paths, char *path_des);
extern "C" int ul_get_full_dir(char *indir, char *fullname);
extern "C" void ul_break_fname(char *fullname,char *dir,char *fname);
extern "C" int ux_decompose_path(char *name, char *farea, char *fname, int options);
extern "C" int tool_strip_blanks (char*, int*);	
extern "C" void tool_get_filename(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag,
	char *paths, char *path_des);
/////////////////////////////////////////////////////////////////////////////
// CDefineDlg dialog

/***********************************************************************
c
c   FUNCTION: CDefineDlg(CWnd* pParent)
c
c              Constructor of class CDefineDlg
c
c   INPUT:  pParent: parent window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

CDefineDlg::CDefineDlg(CWnd* pParent, char *tlib, char*disp, char*slib, char*plib)
	: CDialog(CDefineDlg::IDD, pParent)
{
	if (tlib!=NULL)
		m_tlib = tlib;
	else
		m_tlib = "";
	if (disp!=NULL)
		m_disp = disp;
	else
		disp = "";
	if (slib!=NULL)
		m_slib = slib;
	else
		m_slib = "";
	if (plib!=NULL)
		m_plib = plib;
	else
		m_plib = "";
	//{{AFX_DATA_INIT(CDefineDlg)
	//}}AFX_DATA_INIT
}

/***********************************************************************
c
c   FUNCTION: DoDataExchange(CDataExchange* pDX)
c
c         Called by the framework to exchange and validate dialog data.
c
c   INPUT:  pDX   A pointer to a CDataExchange object.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CDefineDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDefineDlg)
		DDX_Text(pDX, IDC_EDIT1, m_tlib);
		DDX_Text(pDX, IDC_EDIT2, m_disp);
		DDX_Text(pDX, IDC_EDIT3, m_slib);
		DDX_Text(pDX, IDC_EDIT4, m_plib);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CDefineDlg, CDialog)
	//{{AFX_MSG_MAP(CDefineDlg)
	ON_BN_CLICKED(IDC_BROWSE, OnBrowse)
	ON_BN_CLICKED(IDC_BROWSE1, OnBrowse1)
	ON_BN_CLICKED(IDC_BROWSE2, OnBrowse2)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDefineDlg message handlers

void CDefineDlg::OnOK() 
{	
/*
.....check if the directory selected is inside system or local directory, if yes
.....only save the name, otherwise, all path, also remove '_S' if any
*/

	CDialog::OnOK();
}
/***********************************************************************
c
c   FUNCTION: CParmsDlg::OnBrowse()
c
c         This is callback function for "Browse" button.
c			This function will open a file browser and accept filename
c			according user input
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/

void CDefineDlg::OnBrowse() 
{
	char fnam[UX_MAX_PATH_LEN];
	int nc;
	
	CString lib;
	GetDlgItem(IDC_EDIT1)->GetWindowText(lib);
	nc = wcslen (lib);
	wcstombs(fnam, lib, nc);
	fnam[nc] = '\0';

	tool_get_filename(this, "Tool Library File", "*.TLB", fnam, &nc, "Tool Library Files (*.TLB)", 1,
						"NCL_TOOL", "System");
	WCHAR *wfnam;
	int len = strlen (fnam) + 1;
	wfnam = new WCHAR[len];
	int wlen = MultiByteToWideChar(CP_ACP, 0, fnam, -1, 
							wfnam, len);
	GetDlgItem(IDC_EDIT1)->SetWindowText(wfnam);
	delete wfnam;
}

/***********************************************************************
c
c   FUNCTION: CParmsDlg::OnBrowse2()
c
c         This is callback function for "Browse" button.
c			This function will open a file browser and accept filename
c			according user input
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/

void CDefineDlg::OnBrowse2() 
{
	char fnam[UX_MAX_PATH_LEN];
	int nc;

	CString lib;
	GetDlgItem(IDC_EDIT4)->GetWindowText(lib);
	nc = wcslen (lib);
	wcstombs(fnam, lib, nc);
	fnam[nc] = '\0';

	tool_get_filename(this, "Tool Profile File", "*.LIB", fnam, &nc, "Profile Library File (*.LIB)", 1,
						"NCL_TOOL", "System");
	WCHAR *wfnam;
	int len = strlen (fnam) + 1;
	wfnam = new WCHAR[len];
	int wlen = MultiByteToWideChar(CP_ACP, 0, fnam, -1, 
							wfnam, len);
	GetDlgItem(IDC_EDIT4)->SetWindowText(wfnam);
	delete wfnam;
}

/***********************************************************************
c
c   FUNCTION: CDefineDlg::OnBrowse1()
c
c         This is callback function for "Symbol Library" button.
c			This function will open a file browser and accept symlib dir
c			according user input
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/

void CDefineDlg::OnBrowse1() 
{
	char dirname[UX_MAX_PATH_LEN], dir[256], dname[256], dir2[UX_MAX_PATH_LEN];
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	int len;

	strcpy(paths, "UB_SYS_M_SYMDIR;.");
	strcpy(path_des, "System;Local");

	CString lib;
	GetDlgItem(IDC_EDIT3)->GetWindowText(lib);
	len = wcslen (lib);
	wcstombs(dirname, lib, len);
	dirname[len] = '\0';

	if ((len>=2)&&(dirname[len-2]=='_')&&(dirname[len-1]=='S'))
	{
		;
	}
	else if (dirname[0]!='\0')
		strcat (dirname, "_S");
	tool_get_dirname(this, "Symbol Library", dirname, &len, paths, path_des);
/*
.....if the slib path is system (UB_SYS_M_SYMDIR) or local, don't save the path, just name
.....also remove ending '_S'.
*/
	ux_decompose_path(dirname, dir, dname, UX_NQUOTES);
	ul_get_full_dir("UB_SYS_M_SYMDIR", dir2);
	if (stricmp(dir, dir2)!=0)
	{
		ul_get_full_dir(".", dir2);
		if (stricmp(dir, dir2)==0)
			strcpy(dirname, dname);
	}
	else
		strcpy(dirname, dname);
	len = strlen(dirname);
	tool_strip_blanks(dirname, &len);
	if ((len>=2)&&(dirname[len-2]=='_')&&(dirname[len-1]=='S'))
	{
		dirname[len-2] = '\0';
	}
	WCHAR *wdname;
	len = strlen (dirname) + 1;
	wdname = new WCHAR[len];
	int wlen = MultiByteToWideChar(CP_ACP, 0, dirname, -1, 
							wdname, len);
	GetDlgItem(IDC_EDIT3)->SetWindowText(wdname);
}


/***********************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c         This member function is called in response to 
c		the WM_INITDIALOG message. This message is sent to 
c		the dialog box during the Create, CreateIndirect, 
c		or DoModal calls, which occur immediately before 
c		the dialog box is displayed. 
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CDefineDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// TODO: Add extra initialization here
	
	char title[256];
//	strcpy(title,"Define Tool Library");
	SetWindowText(_T("Define Tool Library"));
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
