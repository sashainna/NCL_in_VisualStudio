/************************************************************************
c
c   FILE NAME: plot.cpp
c
c	 CONTAINS: 
c		MainApplication file 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       plot.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:13:20
c
c**********************************************************************
*/
// plot.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "xenv1.h"
#include "plot.h"
#include "plotDlg.h"
#include "wsps.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int plot_wntinit(int argc, char **argv);
extern "C" int ux_init_table(int);
extern "C" int uu_toolmalloc_init();
static BOOL Print_UserAbort;
static BOOL CALLBACK AbortProc(HDC, int);  
extern "C" int utp_interact();
extern "C" int uu_ttput(int fd, char* buf, int len);
extern CDC *PS_pDC;
extern "C" Gsps uw_ps;
#define MAX_PSBUF	1000

/***********************************************************************
c
c   FUNCTION: utp_if_psprinter
c
c         Check if the support postscript printer
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" int utp_if_psprinter(HDC hDC)
{
	int gPrCode = POSTSCRIPT_PASSTHROUGH;
	int stat = Escape(hDC, QUERYESCSUPPORT, sizeof(int), (LPCSTR)&gPrCode, NULL);
	if (stat==1)
		return 1;
	else
		return 0;
}

/***********************************************************************
c
c   FUNCTION: readinenv(char* filename)
c
c         Read a initial file and set enviroment
c			for this application
c
c   INPUT:  filename: initialize file
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
static int readinenv(char* filename)
{
	FILE *fp;
	char envlin[500];
	char *str;
	if ((!strlen(filename)) || ((fp = fopen(filename, "r") )== NULL))
	{
		return -1;
	}
	while (fgets(envlin, 500, fp))
	{
		str = strtok(envlin, "\n\r");
		if (str!=NULL)
		{
			strcpy(envlin, str);
			_putenv(envlin);
		}
	}
	return 0;
}
/////////////////////////////////////////////////////////////////////////////
// Printing Dialog

class CPrintingDialog : public CDialog
{
public:
	//{{AFX_DATA(CPrintingDialog)
	enum { IDD = AFX_IDD_PRINTDLG };
	//}}AFX_DATA
	CPrintingDialog::CPrintingDialog(CWnd* pParent)
		{
			Create(CPrintingDialog::IDD, pParent);      // modeless !
			Print_UserAbort = FALSE;
		}
	virtual ~CPrintingDialog() { }

	virtual BOOL OnInitDialog();
	virtual void OnCancel();
};


BOOL CPrintingDialog::OnInitDialog()
{
	SetWindowText(AfxGetAppName());
	CenterWindow();
	return CDialog::OnInitDialog();
}

void CPrintingDialog::OnCancel()
{
	Print_UserAbort = TRUE;  // flag that user aborted print
	CDialog::OnCancel();
}


/////////////////////////////////////////////////////////////////////////////
// CPlotApp

BEGIN_MESSAGE_MAP(CPlotApp, CWinApp)
	//{{AFX_MSG_MAP(CPlotApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPlotApp construction

CPlotApp::CPlotApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CPlotApp object

CPlotApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CPlotApp initialization

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
BOOL CPlotApp::InitInstance()
{
	char savecmd[500], leftstr[500], tempstr[500], *indx;
	int j,nc;
	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	CWinApp* pApp;
	pApp = AfxGetApp();
/*
.....Let accept command line
.....Yurong 1/20/00
*/
	LPTSTR lpCmdLine;
	lpCmdLine = pApp->m_lpCmdLine;
	char *str, **argv, msg[200],pstr[20];
	int i, status;
/*
.....we will using parse routine like parse a C command
.....line, so the command itself is first arg
.....Yurong 1/20/00
*/
	argv = (char**)malloc(15*sizeof(char*));
	for (i=0; i<15; i++)
		argv[i] = (char*)malloc(UX_MAX_PATH_LEN*sizeof(char));
		
	strcpy(argv[0], "plot");
	i = 1;
	if (lpCmdLine[0]!='\0')
	{
/*
.....the filename may include space, but if ther are spaces,
.....it required quoted
*/
		strcpy (savecmd, lpCmdLine);
		strcpy(leftstr, lpCmdLine);
start:;
		if (leftstr[0] != '-')
		{
/*
.....must be filename
.....if the filename have spaces in it, it must use quotes
*/
			strcpy(tempstr, leftstr);
			if (leftstr[0]=='\"')
			{
				str = strtok(leftstr, "\"");
				if (str!=NULL)
				{
					strcpy(argv[i], str);
				}
			}
			else
			{
				strcpy (leftstr, tempstr);
				str = strtok(leftstr, " \t\n");
				if (str==NULL) 
				{
					argv[i] = NULL;
					goto start2;
				}
				strcpy(argv[i], str);
			}
		}
/*
.....-d=filename
.....if the filename have spaces in it, it must use quotes
*/
		else if ((strncmp(leftstr, "-d", 2)==0) 
				|| (strncmp(leftstr, "-D", 2)==0))
		{
			strcpy(tempstr, leftstr);
			j = 2;
			while ((leftstr[j]==' ') || (leftstr[j]=='='))
				j++;
			strncpy (pstr, leftstr, j);
			strcpy (leftstr, &(leftstr[j]));
			if (leftstr[0] == '\"')
			{
				str = strtok(leftstr, "\"");
				if (str!=NULL)
				{
					sprintf(argv[i], "%s%s", pstr, str);
				}
			}
			else
			{
				strcpy (leftstr, tempstr);
				str = strtok(leftstr, " \t\n");
				if (str==NULL) 
				{
					argv[i] = NULL;
					goto start2;
				}
				strcpy(argv[i], str);
			}
		}
		else
		{
			str = strtok(leftstr, " \t\n");
			if (str==NULL) 
			{
				argv[i] = NULL;
				goto start2;
			}
			strcpy(argv[i], str);
		}
		nc = strlen (str);
		if (strcmp(savecmd, argv[i])!=0)
		{
			strcpy(leftstr, str+nc+1);
			strcpy (savecmd, leftstr);
		}
		else
		{
			i++;
			argv[i] = NULL;
			goto start2;
		}
/*
.....remove prefix spaces
*/
		j=0;
		while (leftstr[j]==' ') j++;
		strcpy (leftstr, &(leftstr[j]));
		i++;
/*
		while(str)
		{
			strcpy(argv[i], str);
			i++;
			str = strtok(NULL, " \t\n");
		}
*/
		if (i>=15) goto start2;
		if ((leftstr!=NULL) && (leftstr[0]!='\0')) goto start;
		argv[i] = NULL;
	}
/*
.....Move those part to function plot_wntinit
.....Yurong
*/
/*	NCLPLOT = 1;
	uu_toolmalloc_init();
	ux_init_table(1);	
*/
start2:;
	status = plot_wntinit(i, argv);
	if (status == 0)
/*
.....we have already finished runing batch,
.....simply return here
*/
	{
		for (i=0; i<15; i++)
			free(argv[i]);
		free(argv);
		return FALSE;
	}
/*
.....printer queue
*/
	else if (status==-1)
	{
		OnPrintPS();		
		for (i=0; i<15; i++)
			free(argv[i]);
		free(argv);
		return FALSE;
	}
/*
.....else start a window
*/
	CPlotDlg dlg;
	m_pMainWnd = &dlg;
	int nResponse = dlg.DoModal();
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
	for (i=0; i<15; i++)
		free(argv[i]);
	free(argv);

	// Since the dialog has been closed, return FALSE so that we exit the
	//  application, rather than start the application's message pump.
	return FALSE;
}

/***********************************************************************
c
c   FUNCTION: DoPreparePrinting(CPrintInfo* pInfo)
c
c         Prepare the print, get the printer information
c			The function is mostly copied from CView::DoPreparePrinting with
c			some change the fit the PLOT application
c
c   INPUT:  pInfo: printer information
c
c   OUTPUT :   pInfo: printer information
c   RETURN:    None
c
**********************************************************************/
BOOL CPlotApp::DoPreparePrinting(CPrintInfo* pInfo)
{
	ASSERT(pInfo != NULL);
	ASSERT(pInfo->m_pPD != NULL);

	if (pInfo->m_pPD->m_pd.nMinPage > pInfo->m_pPD->m_pd.nMaxPage)
		pInfo->m_pPD->m_pd.nMaxPage = pInfo->m_pPD->m_pd.nMinPage;

	if (pInfo->m_bPreview || pInfo->m_bDirect ||
		(pInfo->m_bDocObject && !(pInfo->m_dwFlags & PRINTFLAG_PROMPTUSER)))
	{
		if (pInfo->m_pPD->m_pd.hDC == NULL)
		{
			if (!(this->GetPrinterDeviceDefaults(&pInfo->m_pPD->m_pd)))
			{
				if (!pInfo->m_bDocObject || (pInfo->m_dwFlags & PRINTFLAG_MAYBOTHERUSER))
					if (this->DoPrintDialog(pInfo->m_pPD) != IDOK)
						return FALSE;
			}

			if (pInfo->m_pPD->m_pd.hDC == NULL)
			{
				if (pInfo->m_pPD->CreatePrinterDC() == NULL)
					return FALSE;
			}
		}
		pInfo->m_pPD->m_pd.nFromPage = (WORD)pInfo->GetMinPage();
		pInfo->m_pPD->m_pd.nToPage = (WORD)pInfo->GetMaxPage();
	}
	else
	{
		pInfo->m_pPD->m_pd.nFromPage = (WORD)pInfo->GetMinPage();
		pInfo->m_pPD->m_pd.nToPage = (WORD)pInfo->GetMaxPage();

		if (this->DoPrintDialog(pInfo->m_pPD) != IDOK)
			return FALSE;  
	}

	ASSERT(pInfo->m_pPD != NULL);
	ASSERT(pInfo->m_pPD->m_pd.hDC != NULL);
	if (pInfo->m_pPD->m_pd.hDC == NULL)
		return FALSE;
	if (utp_if_psprinter(pInfo->m_pPD->m_pd.hDC)==0)
	{
		MessageBox(NULL,"Postscript is not supported in this printer", "Info", MB_OK);
		return FALSE;
	}
	pInfo->m_nNumPreviewPages = this->m_nNumPreviewPages;
	VERIFY(pInfo->m_strPageDesc.LoadString(AFX_IDS_PREVIEWPAGEDESC));
	return TRUE;
}

/***********************************************************************
c
c   FUNCTION: OnPrintPS()
c
c         Print the ploting drawing
c			The function is changed by follow CView::OnFilePrint()
c			to fit the PLOT application
c
c   INPUT:  pInfo: printer information
c
c   OUTPUT :   pInfo: printer information
c   RETURN:    None
c
**********************************************************************/
void CPlotApp::OnPrintPS()
{
	CPrintInfo printInfo;
	ASSERT(printInfo.m_pPD != NULL);  

	if (DoPreparePrinting(&printInfo))
	{
		ASSERT(printInfo.m_pPD->m_pd.hDC != NULL);

		CString strOutput;
		if (printInfo.m_pPD->m_pd.Flags & PD_PRINTTOFILE && !printInfo.m_bDocObject)
		{
			CString strDef(MAKEINTRESOURCE(AFX_IDS_PRINTDEFAULTEXT));
			CString strPrintDef(MAKEINTRESOURCE(AFX_IDS_PRINTDEFAULT));
			CString strFilter(MAKEINTRESOURCE(AFX_IDS_PRINTFILTER));
			CString strCaption(MAKEINTRESOURCE(AFX_IDS_PRINTCAPTION));
			CFileDialog dlg(FALSE, strDef, strPrintDef,
				OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT, strFilter);
			dlg.m_ofn.lpstrTitle = strCaption;

			if (dlg.DoModal() != IDOK)
				return;
			strOutput = dlg.GetPathName();
		}

		DOCINFO docInfo;
		memset(&docInfo, 0, sizeof(DOCINFO));
		docInfo.cbSize = sizeof(DOCINFO);
		docInfo.lpszDocName = "test.ps";
		CString strPortName;
		int nFormatID;
		if (strOutput.IsEmpty())
		{
			docInfo.lpszOutput = NULL;
			strPortName = printInfo.m_pPD->GetPortName();
			nFormatID = AFX_IDS_PRINTONPORT;
		}
		else
		{
			docInfo.lpszOutput = strOutput;
			::GetFileTitle(strOutput,
				strPortName.GetBuffer(_MAX_PATH), (WORD)_MAX_PATH);
			nFormatID = AFX_IDS_PRINTTOFILE;
		}

		CDC dcPrint;
		if (!printInfo.m_bDocObject)
		{
			dcPrint.Attach(printInfo.m_pPD->m_pd.hDC);  // attach printer dc
			dcPrint.m_bPrinting = TRUE;
		}

		if (!printInfo.m_bDocObject)
			dcPrint.SetAbortProc(AbortProc);

		if (AfxGetMainWnd()!=NULL)
			AfxGetMainWnd()->EnableWindow(FALSE);

		CPrintingDialog dlgPrintStatus(AfxGetMainWnd());
		CString strTemp;
		dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_DOCNAME, "Ploting file");
		dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_PRINTERNAME,
			printInfo.m_pPD->GetDeviceName());
		AfxFormatString1(strTemp, nFormatID, strPortName);
		dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_PORTNAME, strTemp);
		dlgPrintStatus.ShowWindow(SW_SHOW);
		dlgPrintStatus.UpdateWindow();
		if (!printInfo.m_bDocObject && dcPrint.StartDoc(&docInfo) == SP_ERROR)
		{
			if (AfxGetMainWnd()!=NULL)
				AfxGetMainWnd()->EnableWindow(TRUE);
			dlgPrintStatus.DestroyWindow();
			dcPrint.Detach(); 
			AfxMessageBox(AFX_IDP_FAILED_TO_START_PRINT);
			return;
		}
		UINT nEndPage = printInfo.GetToPage();
		UINT nStartPage = printInfo.GetFromPage();

		if (nEndPage < printInfo.GetMinPage())
			nEndPage = printInfo.GetMinPage();
		if (nEndPage > printInfo.GetMaxPage())
			nEndPage = printInfo.GetMaxPage();

		if (nStartPage < printInfo.GetMinPage())
			nStartPage = printInfo.GetMinPage();
		if (nStartPage > printInfo.GetMaxPage())
			nStartPage = printInfo.GetMaxPage();

		int nStep = (nEndPage >= nStartPage) ? 1 : -1;
		nEndPage = (nEndPage == 0xffff) ? 0xffff : nEndPage + nStep;

		BOOL bError = FALSE;
		if (printInfo.m_bDocObject)
		{
			OnPrepareDC(&dcPrint, &printInfo);
			OnPrint(&dcPrint, &printInfo);
		}
		else
		{
			for (printInfo.m_nCurPage = nStartPage;
				printInfo.m_nCurPage != nEndPage; printInfo.m_nCurPage += nStep)
			{
				OnPrepareDC(&dcPrint, &printInfo);

				if (!printInfo.m_bContinuePrinting)
					break;
				TCHAR szBuf[80];
				wsprintf(szBuf, strTemp, printInfo.m_nCurPage);
				dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_PAGENUM, szBuf);
				printInfo.m_rectDraw.SetRect(0, 0,
					dcPrint.GetDeviceCaps(HORZRES),
					dcPrint.GetDeviceCaps(VERTRES));
				dcPrint.DPtoLP(&printInfo.m_rectDraw);

				if (dcPrint.StartPage() < 0)
				{
					bError = TRUE;
					break;
				}

				OnPrepareDC(&dcPrint, &printInfo);

				ASSERT(printInfo.m_bContinuePrinting);

				OnPrint(&dcPrint, &printInfo);
				if (dcPrint.EndPage() < 0 || !AbortProc(dcPrint.m_hDC, 0))
				{
					bError = TRUE;
					break;
				}
			}
		}
		if (!printInfo.m_bDocObject)
		{
			if (!bError)
				dcPrint.EndDoc();
			else
				dcPrint.AbortDoc();
		}
		dlgPrintStatus.DestroyWindow();
		if (AfxGetMainWnd()!=NULL)
			AfxGetMainWnd()->EnableWindow();
		dcPrint.Detach();   
	}
}

void CPlotApp::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo)
{
	ASSERT_VALID(pDC);
	UNUSED(pDC);

	if (pInfo != NULL)
		pInfo->m_bContinuePrinting =
			(pInfo->GetMaxPage() != 0xffff || (pInfo->m_nCurPage == 1));
}

/***********************************************************************
c
c   FUNCTION: OnPrint()
c
c         Print the ploting drawing on the particular device context
c
c   INPUT:  pInfo: printer information
c			pDC: device context
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CPlotApp::OnPrint(CDC* pDC, CPrintInfo*)
{
	PS_pDC = pDC;
/*
.....The origin and scale for Windows printer drivers is not the 
.....PostScript default (bottom left/72 dpi) but is instead at the 
.....upper left and at the device scale(600 dpi or other). Therefore, 
.....before sending data to the printer, we need to send a couple of 
.....PostScript commands to scale or translate the matrix. 
*/
	int xres = PS_pDC->GetDeviceCaps(LOGPIXELSX);
	int yres = PS_pDC->GetDeviceCaps(LOGPIXELSY);
	int gPrCode = POSTSCRIPT_PASSTHROUGH;
	char szBuf[MAX_PSBUF+sizeof(short)];
	wsprintf(szBuf, "  %d 72 div dup neg scale\n", xres, yres);
	*((short *)szBuf) = strlen(szBuf)-2;
	PS_pDC->Escape(POSTSCRIPT_PASSTHROUGH, strlen(szBuf)-2, szBuf,NULL);

	int wid = PS_pDC->GetDeviceCaps(PHYSICALWIDTH);
	int hgt = PS_pDC->GetDeviceCaps(PHYSICALHEIGHT);
	int dx = PS_pDC->GetDeviceCaps(PHYSICALOFFSETX);
	int dy = PS_pDC->GetDeviceCaps(PHYSICALOFFSETY);
	double shiftx = - (double)dx*72.0/(double)xres;
	double shifty = -(hgt-dy)*72.0/(double)yres;
	sprintf(szBuf, "  %f %f translate\n", shiftx, shifty);
	*((short *)szBuf) = strlen(szBuf)-2;
	PS_pDC->Escape(POSTSCRIPT_PASSTHROUGH, strlen(szBuf)-2, szBuf,NULL);
	utp_interact();
}

static BOOL CALLBACK AbortProc(HDC hdc, int nCode) 
{
	MSG msg;
	while (!Print_UserAbort &&
		::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE))
	{
		if (!AfxGetThread()->PumpMessage())
			return FALSE;   // terminate if WM_QUIT received
	}
	return !Print_UserAbort;
}
