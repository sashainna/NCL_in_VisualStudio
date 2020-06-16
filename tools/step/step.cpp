/************************************************************************
c
c   FILE NAME: step.cpp
c
c	 CONTAINS: 
c		Main C++ file for the STEP application
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       step.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:13:25
c
c**********************************************************************
*/
// step.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include <io.h>
#include "step.h"
#include "stepDlg.h"
#include "stepDlg2.h"
#include "tiges.h"
#include "xenv1.h"
#include "ulist.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int UG_def_line_wt;
extern "C" UU_LIST *UIO_surf_list;
extern "C" int label_type;
extern "C" int MAX_PARA_REC;
extern "C" char iges_fname[UX_MAX_PATH_LEN];
extern "C" UU_LOGICAL UIG_drawing_only;
extern "C" int NAUTSTEP;

extern "C" int ncl_init_auth(char*);
extern "C" int ud_printmsg(char*);
extern "C" int uig_set();
extern "C" int uio_init();
extern "C" char *ux_getenv(char*);
extern "C" int uig_init_sublab();
extern "C" int ux_delete0(char*);
extern "C" int ul_break_fname(char*, char*, char*);

extern "C" int Iges_batch;
extern "C" void utp_batchrun ();
extern "C" int iges_parsecmd(char*);
/////////////////////////////////////////////////////////////////////////////
// CStepApp

BEGIN_MESSAGE_MAP(CStepApp, CWinApp)
	//{{AFX_MSG_MAP(CStepApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStepApp construction

CStepApp::CStepApp()
{
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CStepApp object

CStepApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CStepApp initialization

/***********************************************************************
c
c   FUNCTION: InitInstance()
c
c         initialize new instance of the
c			application running under Windows. 
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CStepApp::InitInstance()
{
	char *p,lmsg[80];
	int ierr;
	BOOL status;
	AfxEnableControlContainer();
/*
......see if there are fillename
......on the command line
*/
	CWinApp* pApp;
	pApp = AfxGetApp();
/*
.....Let accept command line
*/
	char * lpCmdLine, comstr[UX_MAX_PATH_LEN+80];
	lpCmdLine = pApp->m_lpCmdLine;
	strcpy(comstr, lpCmdLine);
	int num = strlen(comstr);
	if (num>0) Iges_batch = 1;
	else Iges_batch = 0;
/*
.....Get STEP license
*/
	ncl_init_auth("STEP");
	if (!NAUTSTEP)
		return FALSE;
/*
.....init data
*/
	UIO_surf_list = NULL;
	xlated_flag = NULL;
	uig_set();
	uio_init();
	dbyte = pbyte = 0;
	if ((p=(char*)ux_getenv("MAX_PARA_REC")) != UU_NULL)
		MAX_PARA_REC = atoi(p);
	UG_def_line_wt = 0;
	if ( (p=(char*)ux_getenv("UG_DEF_LINE_WT")) && !strcmp(p,"TRUE"))
		UG_def_line_wt = 1;
/*
...   Initialize generation of subscripted labels.
*/
	uig_init_sublab();
	label_type = 1;
	UIG_drawing_only = UU_FALSE;
	int nResponse;
/*
......if have a command string, run batch
*/
	int pp;
	if (Iges_batch)
	{
		pp = iges_parsecmd(comstr);
		if (pp==0)
		{
			utp_batchrun();
			return TRUE;
		}
		CStepDlg2 dlg;
		m_pMainWnd = &dlg;
		nResponse = dlg.DoModal();
	}
	else
	{
		CStepDlg dlg;
		m_pMainWnd = &dlg;
		nResponse = dlg.DoModal();
	}
	if (nResponse == IDOK)
	{
		// TODO: Place code here to handle when the dialog is
		//  dismissed with OK
	}
	else if (nResponse == IDCANCEL)
	{
		// TODO: Place code here to handle when the dialog is
		//  dismissed with Cancel
	}
	status = FALSE;
done:;
	if (iges_fname[0]!='\0')
	{
		_close(iges_fd);
	}
	return status;
}
