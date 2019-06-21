/************************************************************************
**
**   FILE NAME: wsntdlgbar.cpp
**
**	 Description - Functions implementation for
**		CNCLDialogBar class 
**	 CONTAINS: 
**		all functions declared in wsntdlgbar.h
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdlgbar.cpp , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/12/18 , 10:40:35
**
************************************************************************
*/

#include "toolstdafx.h"
#include <math.h>
#include "wsgl.h"
#include "wsntdlgbar.h"
#include "afxpriv.h"
#include "wsntfuncid.h"
#include "wsntcfunc.h"
#include "dmotif.h"
#include "wsntframe.h"

#ifndef _UNICODE
#define _UNICODE_SUFFIX
#else
#define _UNICODE_SUFFIX _T("u")
#endif

#ifndef _DEBUG
#define _DEBUG_SUFFIX
#else
#define _DEBUG_SUFFIX _T("d")
#endif

#ifdef _AFXDLL
#define _STATIC_SUFFIX
#else
#define _STATIC_SUFFIX _T("s")
#endif

#define AFX_WNDCLASS(s) _T("Afx") _T(s) _T("42") _STATIC_SUFFIX _UNICODE_SUFFIX _DEBUG_SUFFIX

#define AFX_WNDCONTROLBAR   AFX_WNDCLASS("ControlBar")

BOOL AFXAPI AfxEndDeferRegisterClass(LONG fToRegister);

#define AfxDeferRegisterClass(fClass) \
	((afxRegisteredClasses & fClass) ? TRUE : AfxEndDeferRegisterClass(fClass))

#define AFX_WNDCOMMCTLS_REG     (0x0010)
#define AFX_WNDCOMMCTLSNEW_REG          0x3C000 

const TCHAR _afxWndControlBar[] = AFX_WNDCONTROLBAR;

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#ifdef AFX_CORE3_SEG
#pragma code_seg(AFX_CORE3_SEG)
#endif


CSizeBarArray CNCLDialogBar::m_arrBars; // static member
/*
.....general font use 1
.....statusbar use 2
.....command line use 3
*/
static int 	font_num = 1;
extern CMainFrame *NCL_MainFrame;
extern "C" int UW_com_size, UW_error_size, UW_prmpt_size, UW_status_fontsize;
extern "C" char UW_com_font[20], UW_error_font[20], UW_prmpt_font[20], UW_status_font[20];
extern "C" int UL_clswin_flag;
extern "C" int clswin();
///////
#define TEXT_WID	4
extern "C" UD_FSTRUCT *UD_dispfrm[60];
extern "C" UD_FDATA *UD_dispfdata[60];

extern "C" float uw_form_scalex;
extern "C" float uw_form_scaley;
extern "C" int UD_form_bypick;
extern "C"  int UW_auto_cursor;
extern "C" int uw_ntset_curpos(int x, int y);
extern "C" int uw_ntsetcursor(int cursor);
extern "C" int uw_ntgetcur_cursor();
extern "C" void uw_save_layout_pos2(int, int);

extern int UW_reset_menu;
extern int UW_struct_change;

////////////
/***********************************************************************
**
**   FUNCTION: CNCLDialogBar()
**
**              Constructor of class CNCLDialogBar
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDialogBar::CNCLDialogBar()
{
#ifndef _AFX_NO_OCC_SUPPORT
	m_lpszTemplateName = NULL;
	m_pOccDialogInfo = NULL;
#endif
	m_pDockContext = NULL;
	m_visible = 0;
	m_created = 0;
	m_enabled = 1;
	m_TitleCaption = "";
	for (int i=0; i<200; i++)
		m_child_rect[i] = CRect(0,0,0,0);
	m_origrect = CRect(0,0,0,0);
	m_dtype = 1;
/*
......m_bartype: 1. command window
......			2: status window
......			3: formbar window
......			4: errorbar
......			5: promptbar
......			6: command formbar
*/
	m_bartype = 1;
    m_szMin = CSize(14, 14);
    m_szHorz = CSize(200, 200);
    m_szVert = CSize(200, 200);
    m_szFloat = CSize(200, 200);
    m_bTracking = FALSE;
    m_bKeepSize = TRUE;
    m_bParentSizing = FALSE;
    m_cxEdge = 5;
    m_bDragShowContent = FALSE;
    m_nDockBarID = 0;
    m_dwSCBStyle = 0;
/*
.....when m_modal = 1, created modal dialog bar
.....we are not using it now because it make no sence to create
.....modal dialog bar
*/
	m_modal = 0;
	m_helpact = 0;
	m_startdrag = 0;
	m_scroll = 0;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLDialogBar()
**
**              Destructor of class CNCLDialogBar
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDialogBar::~CNCLDialogBar()
{
/*
......when it is a form bar,
......we destroy it at form class level
*/
	if ((m_bartype!=3)&&(m_bartype!=6))
		DestroyWindow();    // avoid PostNcDestroy problems
}
/***********************************************************************
**
**   FUNCTION: Create(CWnd* pParentWnd, UINT nIDTemplate,
**					UINT nStyle, UINT nID)
**
**       Create a dialogbar
**
**   INPUT:  pParentWnd: parent window
**				nIDTemplate: dialog template ID
**				nStyle: dialog style
**				nID: dialog ID
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLDialogBar::Create(CWnd* pParentWnd, UINT nIDTemplate,
		UINT nStyle, UINT nID)
{ 
	return Create(pParentWnd, MAKEINTRESOURCE(nIDTemplate), nStyle, nID); 
}

/***********************************************************************
**
**   FUNCTION: CreateIndirect(CWnd* pParentWnd, DLGTEMPLATE *dlgTempl, 
**					CDialogItem *rgDlgItem, int itemnum)
**
**       This function Create Dynamic Template Dialog
**
**   INPUT:  pParentWnd: parent window
**				dlgTempl: dialog template
**				rgDlgItem: CDialogItem class array
**				itemnum: total field in the dialog
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLDialogBar::CreateIndirect(CWnd* pParentWnd, DLGTEMPLATE *dlgTempl, CDialogItem *rgDlgItem, int itemnum)
{
	
	WCHAR szFontName1[] = L"MS Sans Serif";
	WCHAR szFontName2[] = L"COURIER";
	WCHAR *szFontName3;
	
	WCHAR*	szBoxCaption;
	int		nChars, nActualChars;
	int		nChars2, nActualChars2;
/*
......We will first convert the control captions to UNICODE
*/
	int		nTotalLength = 0;  
	int		i;

	nChars = m_TitleCaption.GetLength() + 1;
	szBoxCaption = new WCHAR[nChars];
	nActualChars = MultiByteToWideChar(CP_ACP, 0, m_TitleCaption, -1, 
							szBoxCaption, nChars);
	ASSERT(nActualChars > 0);

	nChars2 = strlen(UW_com_font) + 1;
	szFontName3 = new WCHAR[nChars2];
	nActualChars2 = MultiByteToWideChar(CP_ACP, 0, UW_com_font, -1, 
							szFontName3, nChars2);
	ASSERT(nActualChars2 > 0);
/*
...... catch memory exceptions and don't worry about allocation failures
*/
	TRY  
	{
		int nBufferSize =  sizeof(DLGTEMPLATE) + + (2*sizeof(WORD))
													 + nActualChars * sizeof(WCHAR);

		if (m_bartype==6)
		{
			nBufferSize += sizeof(WORD) + nActualChars2 * sizeof(WCHAR);
		}
		else if (font_num==1)
			nBufferSize += sizeof(WORD) + sizeof(szFontName1); 
		else if (font_num==2)
		{
			nBufferSize += sizeof(WORD) + sizeof(szFontName2); 
		}
		else if (font_num==3)
		{
			nBufferSize += sizeof(WORD) + sizeof(szFontName1); 
//			nBufferSize += sizeof(WORD) + nfChars * sizeof(WCHAR); 
		}
			
		nBufferSize = (nBufferSize + 3) & ~3; 

		for (i = 0; i < itemnum; i++)
		{
			int nItemLength;
			if (rgDlgItem[i].m_controltype!=0x1080)
				nItemLength = sizeof(DLGITEMTEMPLATE) + 3 * sizeof(WORD);
			else
				nItemLength = sizeof(DLGITEMTEMPLATE) + 1 * sizeof(WORD)
								+ 18*sizeof(WCHAR);
			nItemLength += (rgDlgItem[i].m_strCaption.GetLength() + 1)
											 * sizeof(WCHAR);

			if (i != itemnum -1 )  
/*
...... take into account gap so next control is DWORD aligned
...... but the last control does not need extra bytes
*/
				nItemLength = (nItemLength + 3) & ~3;  

			nBufferSize += nItemLength;
		}

		HLOCAL hLocal = LocalAlloc(LHND, nBufferSize);
		if (hLocal == NULL)
			AfxThrowMemoryException();

		BYTE*	pBuffer = (BYTE*)LocalLock(hLocal);
		if (pBuffer == NULL)
		{
			LocalFree(hLocal);
			AfxThrowMemoryException();
		}

		BYTE*	pdest = pBuffer;
/*
......transfer DLGTEMPLATE structure to the buffer
*/
		memcpy(pdest, dlgTempl, sizeof(DLGTEMPLATE));
		pdest += sizeof(DLGTEMPLATE);
		*(WORD*)pdest = 0; 
/*
.....use default window class
*/
		*(WORD*)(pdest + 1) = 0;  // use default window class
		pdest += 2 * sizeof(WORD);
/*
.....title
*/
		memcpy(pdest, szBoxCaption, nActualChars * sizeof(WCHAR));
		pdest += nActualChars * sizeof(WCHAR);
		delete szBoxCaption ;
		if (((m_dtype==2)||(m_bartype==6)) && ((UW_com_size>0) && (UW_com_size<=48)))
			*(WORD*)pdest = UW_com_size;
		else
			*(WORD*)pdest = 8;//font size
		pdest += sizeof(WORD);
		if (m_bartype==6)
		{
//			nActualChars2 * sizeof(WCHAR);
			memcpy(pdest, szFontName3, nActualChars2 * sizeof(WCHAR));
			pdest += nActualChars2 * sizeof(WCHAR);
		}
		else if  (font_num==2)
		{
			memcpy(pdest, szFontName2, sizeof(szFontName2));
			pdest += sizeof(szFontName2);
		}
/*
		else if (font_num==3)
		{
			memcpy(pdest, szFontName3, nfChars * sizeof(WCHAR));
			pdest += nfChars * sizeof(WCHAR);
			delete szFontName3 ;
		}
*/
		else
		{
			memcpy(pdest, szFontName1, sizeof(szFontName1));
			pdest += sizeof(szFontName1);
		}
/* 
......We will now transfer the information for each one of the item templates
*/
		for (i = 0; i < itemnum; i++)
		{
			pdest = (BYTE*)(((DWORD)pdest + 3) & ~3);  
			memcpy(pdest, (void *)&rgDlgItem[i].m_dlgItemTemplate, 
										sizeof(DLGITEMTEMPLATE));
			pdest += sizeof(DLGITEMTEMPLATE);

			if (rgDlgItem[i].m_controltype!=0x1080)
			{
				*(WORD*)pdest = 0xFFFF; 
				pdest += sizeof(WORD);
				*(WORD*)pdest = rgDlgItem[i].m_controltype;
				pdest += sizeof(WORD);
			}
			else
			{
				WCHAR*	pchClass;
				pchClass = new WCHAR[18];
				nActualChars = MultiByteToWideChar(CP_ACP, 0, 
						"msctls_progress32", -1, pchClass, 18);
				ASSERT(nActualChars > 0);
				memcpy(pdest, pchClass, nActualChars * sizeof(WCHAR));
				pdest += nActualChars * sizeof(WCHAR);
				delete pchClass;
			}

			WCHAR*	pchCaption;
			int		nChars, nActualChars;
/*
......transfer the caption even when it is an empty string
*/
			nChars = rgDlgItem[i].m_strCaption.GetLength() + 1;
			pchCaption = new WCHAR[nChars];
			nActualChars = MultiByteToWideChar(CP_ACP, 0, 
						rgDlgItem[i].m_strCaption, -1, pchCaption, nChars);
			ASSERT(nActualChars > 0);
			memcpy(pdest, pchCaption, nActualChars * sizeof(WCHAR));
			pdest += nActualChars * sizeof(WCHAR);
			delete pchCaption;

			*(WORD*)pdest = 0;  // How many bytes in data for control
			pdest += sizeof(WORD);
		}
		BOOL bEnableParent = FALSE;
		if (m_modal)
		{
			CWinApp* pApp = AfxGetApp();
			if (pApp != NULL)
				pApp->EnableModeless(FALSE);
// disable parent (before creating dialog)
			if (::IsWindowEnabled(pParentWnd->GetSafeHwnd()))
			{
				::EnableWindow(pParentWnd->GetSafeHwnd(), FALSE);
				bEnableParent = TRUE;
			}
		}
		ASSERT(pdest - pBuffer == nBufferSize); 
		HINSTANCE hInst = AfxGetInstanceHandle();
		DLGTEMPLATE* temp = (DLGTEMPLATE*)pBuffer;
		BOOL bSuccess = CreateDlgIndirect((DLGTEMPLATE*)pBuffer, pParentWnd, hInst);
		LocalUnlock(hLocal);
		LocalFree(hLocal);
	}
	CATCH(CMemoryException, e)
	{
		MessageBox("Memory allocation for dialog template failed.  Demo aborted!",
			"Allocation Failure", MB_ICONEXCLAMATION | MB_OK);
	}
	END_CATCH
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: CreateCommand(CWnd* pParentWnd, char *label, char *string, int bar_size[2], 
							   UINT nStyle, UINT pID, UINT eID, UINT nID)
**
**       Create a commandbar
**
**   INPUT:  pParentWnd: parent window
**				label: prompt label for commandbar
**				string: text string in command line
**				bar_size: size of the command bar
**				nIDTemplate: dialog template ID
**				pID: prompt label ID
**				eID: edit field ID
**				nID: dialog ID
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLDialogBar::CreateCommand(CWnd* pParentWnd, char *label, char *string, int bar_size[2], 
							   UINT nStyle, UINT pID, UINT eID, UINT nID)
{
	ASSERT(pParentWnd != NULL);

	m_bartype = 1;
	m_dtype = 2;

	m_dwStyle = (nStyle & CBRS_ALL);

    m_cyGripper = 12; // set the gripper width
	
	CREATESTRUCT cs;
	memset(&cs, 0, sizeof(cs));
	cs.lpszClass = _afxWndControlBar;
	cs.style = (DWORD)nStyle | WS_CHILD;
	cs.hMenu = (HMENU)nID;
	cs.hInstance = AfxGetInstanceHandle();
	cs.hwndParent = pParentWnd->GetSafeHwnd();
	if (!PreCreateWindow(cs))
		return FALSE;

	// create a modeless dialog

	// initialize common controls
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
	AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);

	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));

	CDialogItem rgDlgItem[20];
	DLGTEMPLATE dlgTempl;
	int itemnum;

	itemnum = 2;
	BOOL bSuccess;
	uw_ntinit_dlgtemp2(label, string, bar_size, pID, eID, &dlgTempl, rgDlgItem);
	font_num = 3;
	bSuccess = CreateIndirect(pParentWnd, &dlgTempl, rgDlgItem, itemnum);

	if (!bSuccess)
		return FALSE;
#ifndef _AFX_NO_OCC_SUPPORT
	m_lpszTemplateName = NULL;
#endif

	// dialog template MUST specify that the dialog
	//  is an invisible child window
	SetDlgCtrlID(nID);
	CRect rect;
	GetWindowRect(&rect);
	if (m_dtype==2)
	{
		rect.right = rect.left + UDM_run_layout.command_size[0];
		rect.bottom = rect.top + UDM_run_layout.command_size[1];
	}
	m_sizeDefault = rect.Size();    // set fixed size

    m_szHorz =  m_sizeDefault;
    m_szVert = m_sizeDefault;
    m_szFloat = m_sizeDefault;

	// force WS_CLIPSIBLINGS
	ModifyStyle(0, WS_CLIPSIBLINGS);

	// force the size to zero - resizing bar will occur later
	SetWindowPos(NULL, 0, 0, 0, 0, SWP_NOZORDER|SWP_NOACTIVATE|SWP_SHOWWINDOW);

/*
.....set the edit field font
*/
	if (m_txtfont.m_hObject)
		VERIFY (m_txtfont.DeleteObject ());	
	int stat = m_txtfont.CreatePointFont (UW_com_size*10, UW_com_font);
	if (stat==0)
		m_txtfont.CreatePointFont (UW_com_size*10, "MS Sans Serif");
	GetDlgItem(eID)->SetFont(&m_txtfont);
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: CreateLabel(CWnd* pParentWnd, char *label, int bar_size[2], 
**							   UINT nStyle, UINT lID, UINT nID, int flag)
**
**       Create a label template
**
**   INPUT:  pParentWnd: parent window
**				label: prompt label for commandbar
**				bar_size: size of the command bar
**				nIDTemplate: dialog template ID
**				lID: label ID
**				nID: dialog ID
**				flag: 0: default size
**						1: use pass in size bar_size
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLDialogBar::CreateLabel(CWnd* pParentWnd, char *label, int bar_size[2], 
							   UINT nStyle, UINT lID, UINT nID, int flag /*=0*/)
{
	ASSERT(pParentWnd != NULL);
	// allow chance to modify styles
	m_dwStyle = (nStyle & CBRS_ALL);
	
    m_cyGripper = 12; // set the gripper width
	CREATESTRUCT cs;
	memset(&cs, 0, sizeof(cs));
	cs.lpszClass = _afxWndControlBar;
	cs.style = (DWORD)nStyle | WS_CHILD;
	cs.hMenu = (HMENU)nID;
	cs.hInstance = AfxGetInstanceHandle();
	cs.hwndParent = pParentWnd->GetSafeHwnd();
	if (!PreCreateWindow(cs))
		return FALSE;

	// create a modeless dialog
	// initialize common controls
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
	AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);

	CDialogItem rgDlgItem[20];
	DLGTEMPLATE dlgTempl;
	int itemnum;

	if (label[0]=='\0')
		strcpy(label, " ");
	itemnum = 1;
	uw_ntinit_dlgtemp1(label, bar_size, lID, &dlgTempl, rgDlgItem);
	font_num = 1;
	BOOL bSuccess = CreateIndirect(pParentWnd, &dlgTempl, rgDlgItem, itemnum);

#ifndef _AFX_NO_OCC_SUPPORT
	m_lpszTemplateName = NULL;
#endif

	if (!bSuccess)
		return FALSE;

	// dialog template MUST specify that the dialog
	//  is an invisible child window

	SetDlgCtrlID(nID);
	CRect rect;
	GetWindowRect(&rect);
	if (flag)
	{
		rect.right = rect.left + UDM_run_layout.command_size[0];
		rect.bottom = rect.top + UDM_run_layout.command_size[1];
	}
	m_sizeDefault = rect.Size();    // set fixed size

    m_szHorz =  m_sizeDefault;
    m_szVert = m_sizeDefault;
    m_szFloat = m_sizeDefault;

	// force WS_CLIPSIBLINGS
	ModifyStyle(0, WS_CLIPSIBLINGS);

	// force the size to zero - resizing bar will occur later
	SetWindowPos(NULL, 0, 0, 0, 0, SWP_NOZORDER|SWP_NOACTIVATE|SWP_SHOWWINDOW);
/*
.....set the font
*/
	if (nID==IDD_ERRORBAR)
	{
		if (m_errorfont.m_hObject)
			VERIFY (m_errorfont.DeleteObject ());	
		int stat = m_errorfont.CreatePointFont (UW_error_size*10, UW_error_font);
		if (stat==0)
			m_errorfont.CreatePointFont (UW_error_size*10, "MS Sans Serif");
		GetDlgItem(IDC_LERROR)->SetFont(&m_errorfont);
		m_bartype = 4;
	}
	else
	{
		if (m_prmptfont.m_hObject)
			VERIFY (m_prmptfont.DeleteObject ());	
		int stat = m_prmptfont.CreatePointFont (UW_prmpt_size*10, UW_prmpt_font);
		if (stat==0)
			m_prmptfont.CreatePointFont (UW_prmpt_size*10, "MS Sans Serif");
		GetDlgItem(IDC_LPROMPT)->SetFont(&m_prmptfont);
		m_bartype = 5;
	}
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: Create(CWnd* pParentWnd, LPCTSTR lpszTemplateName,
**					UINT nStyle, UINT nID)
**
**       Create a dialogbar
**
**   INPUT:  pParentWnd: parent window
**			lpszTemplateName,: dialog template name
**				nStyle: dialog style
**				nID: dialog ID
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLDialogBar::Create(CWnd* pParentWnd, LPCTSTR lpszTemplateName,
	UINT nStyle, UINT nID)
{
	ASSERT(pParentWnd != NULL);
	ASSERT(lpszTemplateName != NULL);

	// allow chance to modify styles
	m_dwStyle = (nStyle & CBRS_ALL);

    m_cyGripper = 12; // set the gripper width

	
	CREATESTRUCT cs;
	memset(&cs, 0, sizeof(cs));
	cs.lpszClass = _afxWndControlBar;
	cs.style = (DWORD)nStyle | WS_CHILD;
	cs.hMenu = (HMENU)nID;
	cs.hInstance = AfxGetInstanceHandle();
	cs.hwndParent = pParentWnd->GetSafeHwnd();
	if (!PreCreateWindow(cs))
		return FALSE;

	// create a modeless dialog

#ifndef _AFX_NO_OCC_SUPPORT
	m_lpszTemplateName = lpszTemplateName;
#endif

	// initialize common controls
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
	AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);

	BOOL bSuccess = CreateDlg(lpszTemplateName, pParentWnd);

#ifndef _AFX_NO_OCC_SUPPORT
	m_lpszTemplateName = NULL;
#endif

	if (!bSuccess)
		return FALSE;

	// dialog template MUST specify that the dialog
	//  is an invisible child window
	SetDlgCtrlID(nID);
	CRect rect;
	GetWindowRect(&rect);
	m_sizeDefault = rect.Size();    // set fixed size
    m_szHorz =  m_sizeDefault;
    m_szVert = m_sizeDefault;
    m_szFloat = m_sizeDefault;

	// force WS_CLIPSIBLINGS
	ModifyStyle(0, WS_CLIPSIBLINGS);

	if (!ExecuteDlgInit(lpszTemplateName))
		return FALSE;

	// force the size to zero - resizing bar will occur later
	SetWindowPos(NULL, 0, 0, 0, 0, SWP_NOZORDER|SWP_NOACTIVATE|SWP_SHOWWINDOW);

	return TRUE;
}


void CNCLDialogBar::OnUpdateCmdUI(CFrameWnd* pTarget, BOOL bDisableIfNoHndler)
{
	if ((m_bartype==3)||(m_bartype==6))
		return;
	UpdateDialogControls(pTarget, bDisableIfNoHndler);
}
BEGIN_MESSAGE_MAP(CNCLDialogBar, CControlBar)
	ON_MESSAGE(WM_INITDIALOG, HandleInitDialog)
	ON_WM_SIZE()
    ON_WM_CREATE()
    ON_WM_NCLBUTTONDOWN()
    ON_WM_NCLBUTTONUP()
    ON_WM_LBUTTONUP()
    ON_WM_NCMOUSEMOVE()
    ON_WM_NCCALCSIZE()
    ON_WM_WINDOWPOSCHANGING()
    ON_WM_CAPTURECHANGED()
    ON_WM_SETTINGCHANGE()
    ON_WM_LBUTTONUP()
    ON_WM_MOUSEMOVE()
    ON_WM_NCLBUTTONDOWN()
    ON_WM_LBUTTONDOWN()
    ON_WM_LBUTTONDBLCLK()
    ON_WM_RBUTTONDOWN()
    ON_WM_NCHITTEST()
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  HandleInitDialog(WPARAM, LPARAM)
c
c   FUNCTION:  This function handle initialize 
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
LRESULT CNCLDialogBar::HandleInitDialog(WPARAM, LPARAM)
{
	Default();  
//	MoveWindow(rect1);

/*
......iterate through and remember the child window  size
*/
	int i = 0;
	CWnd* pChildWnd = GetWindow(GW_CHILD);
	while (pChildWnd)
	{
		pChildWnd->GetWindowRect(m_child_rect[i]);
		ScreenToClient(m_child_rect[i]);
		pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
		i++;
	}
	m_dtype = i;
	GetClientRect(m_origrect);
//	if (m_bartype==2)
//		ncl_setwin_act(1);

	return TRUE;
}

BOOL CNCLDialogBar::SetOccDialogInfo(_AFX_OCC_DIALOG_INFO* pOccDialogInfo)
{
	m_pOccDialogInfo = pOccDialogInfo;
	return TRUE;
}

#define CX_BORDER   1
#define CY_BORDER   1


void CNCLDialogBar::DoPaint(CDC* pDC)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);

	// paint inside client area
	CRect rect;
	GetClientRect(rect);
	DrawBorders(pDC, rect);
	DrawGripper(pDC, rect);
}

void CNCLDialogBar::DrawBorders(CDC* pDC, CRect& rect)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);

	int pos = 0;
	DWORD dwStyle = m_dwStyle;
	if (!(dwStyle & CBRS_BORDER_ANY))
		return;
	// prepare for dark lines
	ASSERT(rect.top == 0 && rect.left == 0);
	CRect rect1, rect2;

	rect1 = rect;
	rect2 = rect;
	COLORREF clr=GetSysColor(COLOR_3DSHADOW);

	// draw dark line one pixel back/up
	if (dwStyle & CBRS_BORDER_3D)
	{
		rect1.right -= CX_BORDER;
		rect1.bottom -= CY_BORDER;
	}
	if (dwStyle & CBRS_BORDER_TOP)
		rect2.top += 1;
	if (dwStyle & CBRS_BORDER_BOTTOM)
		rect2.bottom -= 1;
	// draw left and top
	if (dwStyle & CBRS_BORDER_LEFT)
		pDC->FillSolidRect(0, rect2.top, CX_BORDER, rect2.Height(), clr);
	if (dwStyle & CBRS_BORDER_TOP)
		pDC->FillSolidRect(0, 0, rect.right, CY_BORDER, clr);
		
	// draw right and bottom
	if (dwStyle & CBRS_BORDER_RIGHT)
		pDC->FillSolidRect(rect1.right, rect2.top, -CX_BORDER, rect2.Height(), clr);
	if (dwStyle & CBRS_BORDER_BOTTOM)
		pDC->FillSolidRect(0, rect1.bottom, rect.right, -CY_BORDER, clr);

	if (dwStyle & CBRS_BORDER_3D)
	{
		// prepare for hilite lines
		clr=GetSysColor(COLOR_3DHIGHLIGHT);

		// draw left and top
		if (dwStyle & CBRS_BORDER_LEFT)
			pDC->FillSolidRect(1, rect2.top, CX_BORDER, rect2.Height(), clr);
		if (dwStyle & CBRS_BORDER_TOP)
			pDC->FillSolidRect(0, 1, rect.right, CY_BORDER, clr);

		// draw right and bottom
		if (dwStyle & CBRS_BORDER_RIGHT)
			pDC->FillSolidRect(rect.right, rect2.top, -CX_BORDER, rect2.Height(), clr);
		if (dwStyle & CBRS_BORDER_BOTTOM)
			pDC->FillSolidRect(0, rect.bottom, rect.right, -CY_BORDER, clr);
	}

	if (dwStyle & CBRS_BORDER_TOP)
		rect.top += 1;
	if (dwStyle & CBRS_BORDER_RIGHT)
		rect.right -= 1;
	if (dwStyle & CBRS_BORDER_BOTTOM)
		rect.bottom -= 1;
	if (m_nDockBarID == AFX_IDW_DOCKBAR_TOP)
		pos = 1;
	else if (m_nDockBarID == AFX_IDW_DOCKBAR_BOTTOM)
		pos = 2;
	else if (m_nDockBarID == AFX_IDW_DOCKBAR_LEFT)
		pos = 3;
	else if (m_nDockBarID == AFX_IDW_DOCKBAR_RIGHT)
		pos = 4;
	if (pos!=0)
		uw_save_layout_pos2(m_bartype, pos);
}

#define CX_GRIPPER  3
#define CY_GRIPPER  3
#define CX_BORDER_GRIPPER 2
#define CY_BORDER_GRIPPER 2

/***********************************************************************
**
**   FUNCTION: DrawGripper(CWindowDC *pDC, CRect& rectWindow)
**
**       Draw the gripper at left or top
**
**   INPUT:  pDC: toolbar device context 
**			rectWindow: toolbar rectangle
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDialogBar::DrawGripper(CDC* pDC, const CRect& rectWindow)
{
/*
......get the gripper rect (1 pixel smaller than dialogbar)
*/
	CRect gripper = rectWindow;
	gripper.DeflateRect(1,1);
	if (m_dwStyle & CBRS_FLOATING)
	{          
/*
......no grippers
*/
	}
	else if (m_dwStyle & CBRS_ORIENT_HORZ)
	{          
/*
......gripper at left
*/
		gripper.left++;
		gripper.right = gripper.left+3;

		if (m_bartype==2) 
			gripper.bottom = gripper.bottom-1-BAR_GRIPPER;
		else if (m_bartype==3)
			gripper.bottom = gripper.bottom-3-2*BAR_GRIPPER;
			
		pDC->Draw3dRect(gripper,::GetSysColor(COLOR_3DHIGHLIGHT),::GetSysColor(COLOR_3DSHADOW));
		gripper.OffsetRect(+4,0);
		pDC->Draw3dRect(gripper,::GetSysColor(COLOR_3DHIGHLIGHT),::GetSysColor(COLOR_3DSHADOW));
	
		if ((m_bartype==2) || (m_bartype==3))
		{			
			CRect closeRect;
			closeRect.top = rectWindow.bottom-1-BAR_GRIPPER;
			closeRect.bottom = BAR_GRIPPER + closeRect.top;
			closeRect.left = rectWindow.left + 1;
			closeRect.right = closeRect.left + BAR_GRIPPER;

			pDC->DrawFrameControl(closeRect,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE);
			m_rcClose = closeRect;	

			if (m_bartype==3)
			{
				CRect helpRect;
				helpRect.right = closeRect.right;
				helpRect.left = closeRect.left;
				helpRect.bottom = closeRect.top - 1;
				helpRect.top = helpRect.bottom - (closeRect.bottom - closeRect.top);

				if (m_helpact)
				{
					pDC->DrawFrameControl(helpRect,
						DFC_CAPTION,
						DFCS_CAPTIONHELP );
				}
				else
				{
					pDC->DrawFrameControl(helpRect,
						DFC_CAPTION,
						DFCS_CAPTIONHELP| DFCS_INACTIVE );
				}
				m_rcHelp = helpRect;
			}
		}
	}
	else
	{          
/*
......gripper at top
*/
		gripper.top++;
		gripper.bottom = gripper.top+3;

		if (m_bartype==2)
			gripper.right = rectWindow.right- 2 - BAR_GRIPPER;
		else if (m_bartype==3)
			gripper.right = rectWindow.right- 3 - 2*BAR_GRIPPER;
			
		pDC->Draw3dRect(gripper,::GetSysColor(COLOR_3DHIGHLIGHT),::GetSysColor(COLOR_3DSHADOW));
		gripper.OffsetRect(0,+4);
		pDC->Draw3dRect(gripper,::GetSysColor(COLOR_3DHIGHLIGHT),::GetSysColor(COLOR_3DSHADOW));

		if ((m_bartype==2) || (m_bartype==3))
		{
			CRect closeRect;
			closeRect.top = rectWindow.top + 1;
			closeRect.bottom = BAR_GRIPPER + closeRect.top;
			closeRect.right = rectWindow.right - 1;
			closeRect.left = closeRect.right - BAR_GRIPPER;
			pDC->DrawFrameControl(closeRect,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE );
			m_rcClose = closeRect;
			if (m_bartype==3)
			{
				CRect helpRect;
				helpRect.top = closeRect.top;
				helpRect.bottom = closeRect.bottom;
				helpRect.right = closeRect.left - 1;
				helpRect.left = helpRect.right - (closeRect.right - closeRect.left);

				if (m_helpact)
				{
					pDC->DrawFrameControl(helpRect,
						DFC_CAPTION,
						DFCS_CAPTIONHELP );
				}
				else
				{
					pDC->DrawFrameControl(helpRect,
						DFC_CAPTION,
						DFCS_CAPTIONHELP| DFCS_INACTIVE );
				}

				m_rcHelp = helpRect;
			}
		}
	}
}

/***********************************************************************
c
c   FUNCTION: OnNcLButtonDown(UINT nHitTest, CPoint point)
c
c           The framework calls this member function when the user 
c			push the left mouse button while the cursor is within a nonclient area
c
c   INPUT:  nHitTest: Specifies the hit-test code. A hit test is a test that determines the location of the cursor
c			point: Specifies a CPoint object that contains the x and y screen coordinates of the cursor position
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDialogBar::OnNcLButtonDown(UINT nHitTest, CPoint point) 
{
    if (IsFloating())
    {
        CControlBar::OnNcLButtonDown(nHitTest, point);
        return;
    }

    if (m_bTracking) return;

    if ((nHitTest >= HTSIZEFIRST) && (nHitTest <= HTSIZELAST))
        StartTracking(nHitTest); // sizing edge hit
}

/***********************************************************************
c
c   FUNCTION: OnNcLButtonUp(UINT nHitTest, CPoint point)
c
c           The framework calls this member function when the user 
c			releases the left mouse button while the cursor is within a nonclient area
c
c   INPUT:  nHitTest: Specifies the hit-test code. A hit test is a test that determines the location of the cursor
c			point: Specifies a CPoint object that contains the x and y screen coordinates of the cursor position
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDialogBar::OnNcLButtonUp(UINT nHitTest, CPoint point) 
{
    if (nHitTest == HTCLOSE)
	{
        m_pDockSite->ShowControlBar(this, FALSE, FALSE); 
		m_visible = 0;
		if (m_bartype==2)
		{
			UL_clswin_flag = 1;
			clswin();
			UL_clswin_flag = 0;
		}
	}
    CControlBar::OnNcLButtonUp(nHitTest, point);
}

/***********************************************************************
c
c   FUNCTION: OnLButtonUp(UINT nFlags, CPoint point)
c			Callback function for mouse button up
c
c   INPUT:  nFlags: Indicates whether various virtual keys are down.
c					MK_CONTROL   Set if the CTRL key is down.
c					MK_MBUTTON   Set if the middle mouse button is down.
c					MK_RBUTTON   Set if the right mouse button is down.
c					MK_SHIFT   Set if the SHIFT key is down.
c			point:  Specifies the x- and y-coordinate of the cursor.
c
c   OUTPUT :   None.
c   RETURN:    None.
c
**********************************************************************/
void CNCLDialogBar::OnLButtonUp(UINT nFlags, CPoint point) 
{
    if (m_bTracking)
        StopTracking();
	if (m_bartype==1)
	{
		CControlBar::OnLButtonUp(nFlags, point);
		return;
	}
	if ((m_pDockBar != NULL) && (m_pDockBar->m_bFloating==0))
	{	
		CRect wRect;
		GetWindowRect(wRect);
		ScreenToClient(&wRect);
		point.x-=wRect.left;
		point.y-=wRect.top;

		DWORD hitTest = HitTest(point);

		CWindowDC dc(this);

		DrawFrameControl(dc.m_hDC,
				m_rcClose,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE);

		DrawFrameControl(dc.m_hDC,
				m_rcHelp,
				DFC_CAPTION,
				DFCS_CAPTIONHELP);


		switch(hitTest)	
		{
			case DHT_CLOSE:
				if ((m_bartype!=3)&&(m_bartype!=6))
				{
					m_visible = 0;
					m_pDockSite->ShowControlBar(this, FALSE, FALSE);
					if (m_bartype==2)
					{
						UL_clswin_flag = 1;
						clswin();
						UL_clswin_flag = 0;
					}
				}
				else
					((CNCLFormBar*)this)->FormClose();
				break;
			case DHT_HELP:
				if (m_bartype==3)
				{
					((CNCLFormBar*)this)->FormHelp();
				}
				break;
			default:
				break;
		}
		ReleaseCapture();
	}
	else
	    CControlBar::OnLButtonUp(nFlags, point);
}

/***********************************************************************
c
c   FUNCTION: OnMouseMove(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the mouse cursor moves
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDialogBar::OnMouseMove(UINT nFlags, CPoint point) 
{
    if (m_bTracking)
        OnTrackUpdateSize(point);
    
    CControlBar::OnMouseMove(nFlags, point);
}

/***********************************************************************
c
c   FUNCTION: OnCaptureChanged(CWnd *pWnd) 
c
c       The framework calls this member function 
c			to notify the window that is losing the mouse capture. 
c
c   INPUT:  pWnd: A pointer to the window to gain mouse capture
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDialogBar::OnCaptureChanged(CWnd *pWnd) 
{
    if (m_bTracking && (pWnd != this))
        StopTracking();

    CControlBar::OnCaptureChanged(pWnd);
}


/////////////////////////////////////////////////////////////////////////
// CNCLDialogBar message handlers

/***********************************************************************
**
**   FUNCTION: OnCreate(LPCREATESTRUCT lpCreateStruct) 
**
**		Override this member function to perform any needed 
**		initialization of a derived class. 
**   
**		INPUT:  LPCREATESTRUCT lpCreateStruct: contains copies of 
**						the parameters used to create the window.
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CNCLDialogBar::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
    if (CControlBar::OnCreate(lpCreateStruct) == -1)
        return -1;
    
    m_arrBars.Add(this);        // register
    
    return 0;
}

/***********************************************************************
**
**   FUNCTION: DestroyWindow() 
**
**		Destroys the Windows window attached to the this dialogbar object
**   
**		INPUT: none
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLDialogBar::DestroyWindow() 
{
    int nPos = FindSizingBar(this);
    ASSERT(nPos >= 0);

    m_arrBars.RemoveAt(nPos);   // unregister

    return CControlBar::DestroyWindow();
}

/***********************************************************************
**
**   FUNCTION: IsFloating() 
**
**		Check if the dialogbar is floating
**   
**		INPUT: none
**
**   OUTPUT :   none
**   RETURN:    1: yes
**				0: no
**
**********************************************************************/
const BOOL CNCLDialogBar::IsFloating() const
{
    return !IsHorzDocked() && !IsVertDocked();
}

/***********************************************************************
**
**   FUNCTION: IsHorzDocked() 
**
**		Check if the dialogbar is HorzDocked
**   
**		INPUT: none
**
**   OUTPUT :   none
**   RETURN:    1: yes
**				0: no
**
**********************************************************************/
const BOOL CNCLDialogBar::IsHorzDocked() const
{
    return (m_nDockBarID == AFX_IDW_DOCKBAR_TOP ||
        m_nDockBarID == AFX_IDW_DOCKBAR_BOTTOM);
}

/***********************************************************************
**
**   FUNCTION: IsVertDocked() 
**
**		Check if the dialogbar is VertDocked
**   
**		INPUT: none
**
**   OUTPUT :   none
**   RETURN:    1: yes
**				0: no
**
**********************************************************************/
const BOOL CNCLDialogBar::IsVertDocked() const
{
    return (m_nDockBarID == AFX_IDW_DOCKBAR_LEFT ||
        m_nDockBarID == AFX_IDW_DOCKBAR_RIGHT);
}

/***********************************************************************
**
**   FUNCTION: IsSideTracking() 
**
**		Check if the dialogbar is SideTracking
**   
**		INPUT: none
**
**   OUTPUT :   none
**   RETURN:    1: yes
**				0: no
**
**********************************************************************/
const BOOL CNCLDialogBar::IsSideTracking() const
{
    // don't call this when not tracking
    ASSERT(m_bTracking && !IsFloating());

    return (m_htEdge == HTLEFT || m_htEdge == HTRIGHT) ?
        IsHorzDocked() : IsVertDocked();
}

/***********************************************************************
**
**   FUNCTION: CalcFixedLayout(BOOL bStretch, BOOL bHorz)
**
**       Calculate dialogbar's fixed layout
**
**   INPUT:  bStretch: Indicates whether the bar should be stretched to the size of the frame
**			bHorz: Indicates that the bar is horizontally or vertically oriented
**
**   OUTPUT :   None
**   RETURN:    layout size
**
**********************************************************************/
CSize CNCLDialogBar::CalcFixedLayout(BOOL bStretch, BOOL bHorz)
{
    if (bStretch) // the bar is stretched (is not the child of a dockbar)
        if (bHorz)
            return CSize(32767, m_szHorz.cy);
        else
            return CSize(m_szVert.cx, 32767);
	else
	{
		if ((m_bartype==1)||(m_bartype==4)||(m_bartype==5)||(m_bartype==6))
			return m_sizeDefault;
		else if (m_bartype!=2)
		{
			CSize s = m_sizeDefault;
			if (m_dwStyle&CBRS_ORIENT_HORZ)
				s.cx += BAR_GRIPPER - 6;
			else
				s.cy += BAR_GRIPPER - 6;
			return s;
		}
		else 
		{
			CSize s = m_sizeDefault;
			s.cx += BAR_GRIPPER;
			s.cy += BAR_GRIPPER;
			return s;
		}
	}
}


/***********************************************************************
**
**   FUNCTION: CalcDynamicLayout(int nLength, DWORD dwMode)
**
**       Calculate dialogbar's dynamic layout
**
**   INPUT:  nLength: The requested dimension of the control bar, either horizontal or vertical, depending on dwMode
**			dwMode: Layout mode flags
**
**   OUTPUT :   None
**   RETURN:    layout size
**
**********************************************************************/
CSize CNCLDialogBar::CalcDynamicLayout(int nLength, DWORD dwMode)
{
    if (dwMode & (LM_HORZDOCK | LM_VERTDOCK)) // docked ?
    {
        if (nLength == -1)
            m_bParentSizing = TRUE;
        return CControlBar::CalcDynamicLayout(nLength, dwMode);
    }

    if (dwMode & LM_MRUWIDTH) return m_szFloat;
    if (dwMode & LM_COMMIT) return m_szFloat; // already committed

    ((dwMode & LM_LENGTHY) ? m_szFloat.cy : m_szFloat.cx) = nLength;

    m_szFloat.cx = max(m_szFloat.cx, m_szMin.cx);
    m_szFloat.cy = max(m_szFloat.cy, m_szMin.cy);

    m_szHorz = m_szFloat;
    m_szVert = m_szFloat;
	m_sizeDefault = m_szFloat;
    return m_szFloat;
}

/***********************************************************************
**
**   FUNCTION: OnWindowPosChanging(LPWINDOWPOS lpwp)
**		This member function is called When dialogbar position changed, 
**
**   INPUT:   lpwp: WINDOWPOS strcture
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDialogBar::OnWindowPosChanging(WINDOWPOS FAR* lpwndpos)
{
    // force non-client recalc if moved or resized
    lpwndpos->flags |= SWP_FRAMECHANGED;

    CControlBar::OnWindowPosChanging(lpwndpos);
	if (m_modal==0)
	{
		CWnd *win = GetParent();
		 m_nDockBarID = win->GetDlgCtrlID();
		UINT nOldDockBarID = m_nDockBarID;
		m_nDockBarID = GetParent()->GetDlgCtrlID();

		if (!IsFloating())
			if (lpwndpos->flags & SWP_SHOWWINDOW)
				m_bKeepSize = TRUE;
	}
}

/////////////////////////////////////////////////////////////////////////
// Mouse Handling
//
/***********************************************************************
c
c   FUNCTION: HitTest( CPoint point )
c
c       Call this function to find out which area the user is click in
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of point need to be tested. 
c
c   OUTPUT :   None
c   RETURN:    One of the mouse hit-test enumerated values listed below. 
c				DHT_CLOSE	in the title-bar 'X' close box area
c				DHT_HELP		in the title-bar '?' help box area
c				DHT_NCBAR   In a title-bar area.
c
**********************************************************************/
DWORD CNCLDialogBar::HitTest(CPoint pt)
{
	CRect rect=m_rcClose;

	if(rect.PtInRect(pt))
		return (DWORD) DHT_CLOSE;
	else
	{
		if (m_bartype!=3)
			return (DWORD) DHT_NCBAR;
		rect = m_rcHelp;
		if(rect.PtInRect(pt))
			return (DWORD) DHT_HELP;
	}
	return DHT_CAPTION;
}

/***********************************************************************
c
c   FUNCTION: OnLButtonDown(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user presses the left mouse button.
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDialogBar::OnLButtonDown(UINT nFlags, CPoint pt) 
{
	if ((m_bartype==1)||(m_bartype==4)||(m_bartype==5))
	{
		UW_struct_change = 1;
		if (m_pDockBar != NULL)
		{
			// start the drag
			ASSERT(m_pDockContext != NULL);
			ClientToScreen(&pt);
			m_pDockContext->StartDrag(pt);
			AdjustSize();
		    m_pDockSite->RecalcLayout();
		}
		else
			CWnd::OnLButtonDown(nFlags, pt);
		UW_struct_change = 0;
		return;
	}
    if ((m_pDockBar != NULL) && (m_pDockBar->m_bFloating==0))
    {
/*
.....if on 'Close' button
*/
		CRect wRect;
		CPoint point = pt;
		GetWindowRect(wRect);
		ScreenToClient(&wRect);
		point.x-=wRect.left;
		point.y-=wRect.top;
		DWORD hitTest = HitTest(point);
		if (hitTest==DHT_CLOSE)
		{
		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcClose,
			DFC_CAPTION,
			DFCS_CAPTIONCLOSE | DFCS_PUSHED);
			SetCapture();
			return;
		}
		else if (hitTest==DHT_HELP)
		{
		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcHelp,
			DFC_CAPTION,
			DFCS_CAPTIONHELP | DFCS_PUSHED);
			SetCapture();
			return;
		}
		else
		{
			UW_struct_change = 1;
			ASSERT(m_pDockContext != NULL);
			ClientToScreen(&pt);
			m_pDockContext->StartDrag(pt);
			AdjustSize();
			UW_struct_change = 0;
		    m_pDockSite->RecalcLayout();
		}
    }
    else
        CWnd::OnLButtonDown(nFlags, pt);
}

/***********************************************************************
**
**   FUNCTION: OnLButtonDblClk(UINT nFlags, CPoint pt)
**
**       Left mouse button double click callback
**
**   INPUT:  nFlags: Indicates whether various virtual keys are Down
**			point:  Specifies the x- and y-coordinate of the cursor. 
**					These coordinates are always relative to the 
**					upper-left corner of the window.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDialogBar::OnLButtonDblClk(UINT nFlags, CPoint pt) 
{
	if ((m_bartype==1)||(m_bartype==4)||(m_bartype==5)||(m_bartype==6))
	{
		UW_struct_change = 1;
		if (m_pDockBar != NULL)
		{
			// toggle docking
			ASSERT(m_pDockContext != NULL);
			m_pDockContext->ToggleDocking();
		}
		else
			CWnd::OnLButtonDblClk(nFlags, pt);
		UW_struct_change = 0;
		return;
	}
	if ((m_pDockBar != NULL) && (m_pDockBar->m_bFloating==0))
	{
/*
.....if on 'Close' button
*/
		CRect wRect;
		CPoint point = pt;
		GetWindowRect(wRect);
		ScreenToClient(&wRect);
		point.x-=wRect.left;
		point.y-=wRect.top;
		DWORD hitTest = HitTest(point);
		if (hitTest==DHT_CLOSE)
		{
		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcClose,
			DFC_CAPTION,
			DFCS_CAPTIONCLOSE | DFCS_PUSHED);
			SetCapture();
			return;
		}
		else if ((hitTest==DHT_HELP)&&(m_bartype==3))
		{
		    CWindowDC dc(this);
			if (m_helpact)
			{
				DrawFrameControl(dc.m_hDC,
								m_rcHelp,
								DFC_CAPTION,
								DFCS_CAPTIONHELP | DFCS_PUSHED);
			}
			else
			{
				DrawFrameControl(dc.m_hDC,
								m_rcHelp,
								DFC_CAPTION,
								DFCS_CAPTIONHELP | DFCS_INACTIVE);
			}
			SetCapture();
			return;
		}
		else
		{
			// start the drag
			UW_struct_change = 1;
			ASSERT(m_pDockContext != NULL);
			m_pDockContext->ToggleDocking();
			UW_struct_change = 0;
		}
	}
	else
	{
		CWnd::OnLButtonDblClk(nFlags, pt);
	}
}


/***********************************************************************
**
**   FUNCTION: OnRButtonDown(UINT nFlags, CPoint pt)
**
**       Left mouse button double click callback
**
**   INPUT:  nFlags: Indicates whether various virtual keys are Down
**			point:  Specifies the x- and y-coordinate of the cursor. 
**					These coordinates are always relative to the 
**					upper-left corner of the window.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDialogBar::OnRButtonDown(UINT nFlags, CPoint point) 
{
    if (m_bTracking)
        StopTracking();
    
    CControlBar::OnRButtonDown(nFlags, point);
}

/***********************************************************************
**
**   FUNCTION: OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS* lpncsp)
**
**       Calculate the non-client area - adjusting for grippers
**
**   INPUT:  bCalcValidRects: Specifies whether the application should specify 
**							which part of the client area contains valid information. 
**							Windows will copy the valid information to the specified 
**							area within the new client area. If this parameter is TRUE, 
**							the application should specify which part of the client area is valid.
**			lpncsp: Points to a NCCALCSIZE_PARAMS data structure that contains information 
**						an application can use to calculate the new size and position of the CWnd rectangle 
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDialogBar::OnNcCalcSize(BOOL bCalcValidRects,
                                     NCCALCSIZE_PARAMS FAR* lpncsp) 
{
	CRect rcClient = lpncsp->rgrc[0];
    // make room for edges only if they will be painted
    if (m_dwSCBStyle & SCBS_SHOWEDGES)
        rcClient.DeflateRect(
            (m_dwSCBStyle & SCBS_EDGELEFT) ? m_cxEdge : 0,
            (m_dwSCBStyle & SCBS_EDGETOP) ? m_cxEdge : 0,
            (m_dwSCBStyle & SCBS_EDGERIGHT) ? m_cxEdge : 0,
            (m_dwSCBStyle & SCBS_EDGEBOTTOM) ? m_cxEdge : 0);
    lpncsp->rgrc[0] = rcClient;
}

/***********************************************************************
c
c   FUNCTION: OnNcHitTest( CPoint point )
c
c       The framework calls this member function 
c			for the CWnd object that contains the cursor 
c			every time the mouse is moved.
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always screen coordinates.
c
c   OUTPUT :   None
c   RETURN:    One of the mouse hit-test enumerated values listed below. 
c				HTBORDER   In the border of a window that does not have a sizing border.
c				HTBOTTOM   In the lower horizontal border of the window.
c				HTBOTTOMLEFT   In the lower-left corner of the window border.
c				HTBOTTOMRIGHT   In the lower-right corner of the window border.
c				HTCAPTION   In a title-bar area.
c				HTCLIENT   In a client area.
c				HTERROR   On the screen background or on a dividing line between windows (same as HTNOWHERE except that the DefWndProc Windows function produces a system beep to indicate an error).
c				HTGROWBOX   In a size box.
c				HTHSCROLL   In the horizontal scroll bar.
c				HTLEFT   In the left border of the window.
c				HTMAXBUTTON   In a Maximize button.
c				HTMENU   In a menu area.
c				HTMINBUTTON   In a Minimize button.
c				HTNOWHERE   On the screen background or on a dividing line between windows.
c				HTREDUCE   In a Minimize button.
c				HTRIGHT   In the right border of the window.
c				HTSIZE   In a size box (same as HTGROWBOX).
c				HTSYSMENU   In a Control menu or in a Close button in a child window.
c				HTTOP   In the upper horizontal border of the window.
c				HTTOPLEFT   In the upper-left corner of the window border.
c				HTTOPRIGHT   In the upper-right corner of the window border.
c				HTTRANSPARENT   In a window currently covered by another window.
c				HTVSCROLL   In the vertical scroll bar.
c				HTZOOM   In a Maximize button. 
c
**********************************************************************/
LRESULT CNCLDialogBar::OnNcHitTest(CPoint point)
{
    if (IsFloating())
        return CControlBar::OnNcHitTest(point);

    CRect rcBar, rcEdge;
    GetWindowRect(rcBar);

    for (int i = 0; i < 4; i++)
        if (GetEdgeRect(rcBar, GetEdgeHTCode(i), rcEdge))
            if (rcEdge.PtInRect(point)) return GetEdgeHTCode(i);

    return HTCLIENT;
}

/***********************************************************************
**
**   FUNCTION: OnSettingChange(UINT uFlags, LPCTSTR lpszSection) 
**
**       called when it makes changes to system parameters
**
**   INPUT:  uFlags: system-wide parameter flag 
**			lpszSection: name of changed section or registry
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDialogBar::OnSettingChange(UINT uFlags, LPCTSTR lpszSection) 
{
    CControlBar::OnSettingChange(uFlags, lpszSection);

    m_bDragShowContent = FALSE;
    ::SystemParametersInfo(SPI_GETDRAGFULLWINDOWS, 0,
        &m_bDragShowContent, 0); // update
}

/***********************************************************************
**
**   FUNCTION: StartTracking(UINT nHitTest)
**
**       size edge hit
**
**   INPUT:  
**			nHitTest: Specifies the hit-test code. A hit test is a test 
**						that determines the location of the cursor
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDialogBar::StartTracking(UINT nHitTest)
{
    SetCapture();

    // make sure no updates are pending
    RedrawWindow(NULL, NULL, RDW_ALLCHILDREN | RDW_UPDATENOW);
    
    BOOL bHorz = IsHorzDocked();

    m_szOld = bHorz ? m_szHorz : m_szVert;

    CRect rc;
    GetWindowRect(&rc);
    CRect rcEdge;
    VERIFY(GetEdgeRect(rc, nHitTest, rcEdge));
    m_ptOld = rcEdge.CenterPoint();

    m_htEdge = nHitTest;
    m_bTracking = TRUE;

    CSizeBarArray arrSCBars;
    GetRowSizingBars(arrSCBars);

    // compute the minsize as the max minsize of the sizing bars on row
    m_szMinT = m_szMin;
    for (int i = 0; i < arrSCBars.GetSize(); i++)
        if (bHorz)
            m_szMinT.cy = max(m_szMinT.cy, arrSCBars[i]->m_szMin.cy);
        else
            m_szMinT.cx = max(m_szMinT.cx, arrSCBars[i]->m_szMin.cx);

    if (!IsSideTracking())
    {
        // the control bar cannot grow with more than the size of 
        // remaining client area of the mainframe
        m_pDockSite->RepositionBars(0, 0xFFFF, AFX_IDW_PANE_FIRST,
            reposQuery, &rc, NULL, TRUE);
        m_szMaxT = m_szOld + rc.Size() - CSize(4, 4);
    }
    else
    {
		int i;
        // side tracking: max size is the actual size plus the amount
        // the neighbour bar can be decreased to reach its minsize
        for (i = 0; i < arrSCBars.GetSize(); i++)
            if (arrSCBars[i] == this) break;

        CNCLDialogBar* pBar = arrSCBars[i +
            ((m_htEdge == HTTOP || m_htEdge == HTLEFT) ? -1 : 1)];

        m_szMaxT = m_szOld + (bHorz ? pBar->m_szHorz :
            pBar->m_szVert) - pBar->m_szMin;
    }

    OnTrackInvertTracker(); // draw tracker
}

/***********************************************************************
**
**   FUNCTION: StopTracking()
**
**       stop tracking (size edge hit)
**
**   INPUT: none  
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDialogBar::StopTracking()
{
    OnTrackInvertTracker(); // erase tracker

    m_bTracking = FALSE;
    ReleaseCapture();
    
    m_pDockSite->DelayRecalcLayout();
}

/***********************************************************************
**
**   FUNCTION: OnTrackUpdateSize(CPoint& point)
**
**       when tracking and update the  dialogbar size
**
**   INPUT: point: mouse tracking position 
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDialogBar::OnTrackUpdateSize(CPoint& point)
{
    ASSERT(!IsFloating());

    CPoint pt = point;
    ClientToScreen(&pt);
    CSize szDelta = pt - m_ptOld;

    CSize sizeNew = m_szOld;
    switch (m_htEdge)
    {
    case HTLEFT:    sizeNew -= CSize(szDelta.cx, 0); break;
    case HTTOP:     sizeNew -= CSize(0, szDelta.cy); break;
    case HTRIGHT:   sizeNew += CSize(szDelta.cx, 0); break;
    case HTBOTTOM:  sizeNew += CSize(0, szDelta.cy); break;
    }

    // enforce the limits
    sizeNew.cx = max(m_szMinT.cx, min(m_szMaxT.cx, sizeNew.cx));
    sizeNew.cy = max(m_szMinT.cy, min(m_szMaxT.cy, sizeNew.cy));

    BOOL bHorz = IsHorzDocked();
    szDelta = sizeNew - (bHorz ? m_szHorz : m_szVert);
    
    if (szDelta == CSize(0, 0)) return; // no size change

    OnTrackInvertTracker(); // erase tracker

    (bHorz ? m_szHorz : m_szVert) = sizeNew; // save the new size

    CSizeBarArray arrSCBars;
    GetRowSizingBars(arrSCBars);

    for (int i = 0; i < arrSCBars.GetSize(); i++)
        if (!IsSideTracking())
        {   // track simultaneously
            CNCLDialogBar* pBar = arrSCBars[i];
            (bHorz ? pBar->m_szHorz.cy : pBar->m_szVert.cx) =
                bHorz ? sizeNew.cy : sizeNew.cx;
        }
        else
        {   // adjust the neighbour's size too
            if (arrSCBars[i] != this) continue;

            CNCLDialogBar* pBar = arrSCBars[i +
                ((m_htEdge == HTTOP || m_htEdge == HTLEFT) ? -1 : 1)];

            (bHorz ? pBar->m_szHorz.cx : pBar->m_szVert.cy) -=
                bHorz ? szDelta.cx : szDelta.cy;
        }

    OnTrackInvertTracker(); // redraw tracker at new pos

    if (m_bDragShowContent)
        m_pDockSite->DelayRecalcLayout();
}

/***********************************************************************
**
**   FUNCTION: OnTrackInvertTracker()
**
**       draw or erase tracker
**
**   INPUT: none 
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDialogBar::OnTrackInvertTracker()
{
    ASSERT(m_bTracking);

    if (m_bDragShowContent)
        return; // don't show tracker if DragFullWindows is on

    BOOL bHorz = IsHorzDocked();
    CRect rc, rcBar, rcDock, rcFrame;
    GetWindowRect(rcBar);
    m_pDockBar->GetWindowRect(rcDock);
    m_pDockSite->GetWindowRect(rcFrame);
    VERIFY(GetEdgeRect(rcBar, m_htEdge, rc));
    if (!IsSideTracking())
        rc = bHorz ? 
            CRect(rcDock.left + 1, rc.top, rcDock.right - 1, rc.bottom) :
            CRect(rc.left, rcDock.top + 1, rc.right, rcDock.bottom - 1);

    rc.OffsetRect(-rcFrame.TopLeft());

    CSize sizeNew = bHorz ? m_szHorz : m_szVert;
    CSize sizeDelta = sizeNew - m_szOld;
    if (m_nDockBarID == AFX_IDW_DOCKBAR_LEFT && m_htEdge == HTTOP ||
        m_nDockBarID == AFX_IDW_DOCKBAR_RIGHT && m_htEdge != HTBOTTOM ||
        m_nDockBarID == AFX_IDW_DOCKBAR_TOP && m_htEdge == HTLEFT ||
        m_nDockBarID == AFX_IDW_DOCKBAR_BOTTOM && m_htEdge != HTRIGHT)
        sizeDelta = -sizeDelta;
    rc.OffsetRect(sizeDelta);

    CDC *pDC = m_pDockSite->GetDCEx(NULL,
        DCX_WINDOW | DCX_CACHE | DCX_LOCKWINDOWUPDATE);
    CBrush* pBrush = CDC::GetHalftoneBrush();
    CBrush* pBrushOld = pDC->SelectObject(pBrush);

    pDC->PatBlt(rc.left, rc.top, rc.Width(), rc.Height(), PATINVERT);
    
    pDC->SelectObject(pBrushOld);
    m_pDockSite->ReleaseDC(pDC);
}

/***********************************************************************
**
**   FUNCTION: GetEdgeRect(CRect rcWnd, UINT nHitTest,CRect& rcEdge)
**
**       Get the edge rect of the window according the hit code
**
**   INPUT: rcWnd: window rect area which need to get edge rect.
**			 nHitTest: hit test code
**
**   OUTPUT :   rcEdge: the edge rect
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLDialogBar::GetEdgeRect(CRect rcWnd, UINT nHitTest,
                                    CRect& rcEdge)
{
    rcEdge = rcWnd;
    if (m_dwSCBStyle & SCBS_SHOWEDGES)
        rcEdge.DeflateRect(1, 1);
    BOOL bHorz = IsHorzDocked();

    switch (nHitTest)
    {
    case HTLEFT:
        if (!(m_dwSCBStyle & SCBS_EDGELEFT)) return FALSE;
        rcEdge.right = rcEdge.left + m_cxEdge;
        rcEdge.DeflateRect(0, bHorz ? m_cxEdge: 0);
        break;
    case HTTOP:
        if (!(m_dwSCBStyle & SCBS_EDGETOP)) return FALSE;
        rcEdge.bottom = rcEdge.top + m_cxEdge;
        rcEdge.DeflateRect(bHorz ? 0 : m_cxEdge, 0);
        break;
    case HTRIGHT:
        if (!(m_dwSCBStyle & SCBS_EDGERIGHT)) return FALSE;
        rcEdge.left = rcEdge.right - m_cxEdge;
        rcEdge.DeflateRect(0, bHorz ? m_cxEdge: 0);
        break;
    case HTBOTTOM:
        if (!(m_dwSCBStyle & SCBS_EDGEBOTTOM)) return FALSE;
        rcEdge.top = rcEdge.bottom - m_cxEdge;
        rcEdge.DeflateRect(bHorz ? 0 : m_cxEdge, 0);
        break;
    default:
        ASSERT(FALSE); // invalid hit test code
    }
    return TRUE;
}

/***********************************************************************
**
**   FUNCTION: GetEdgeHTCode(int nEdge)
**
**       Get the hit test code according to the edge code
**
**   INPUT: nEdge: edge code
**
**   OUTPUT :
**			 none
**   RETURN:    hit test code
**
**********************************************************************/
UINT CNCLDialogBar::GetEdgeHTCode(int nEdge)
{
    if (nEdge == 0) return HTLEFT;
    if (nEdge == 1) return HTTOP;
    if (nEdge == 2) return HTRIGHT;
    if (nEdge == 3) return HTBOTTOM;
    ASSERT(FALSE); // invalid edge no
    return HTNOWHERE;
}

/***********************************************************************
**
**   FUNCTION: GetRowInfo(int& nFirst, int& nLast, int& nThis)
**
**       Get the row info of the dialog bar
**
**   INPUT: none
**
**   OUTPUT :nFirst: first index of the dialog bar
**			 nLast: last index of the dialog bar
**				nThis: this object's index
**
**   RETURN:  none
**
**********************************************************************/
void CNCLDialogBar::GetRowInfo(int& nFirst, int& nLast, int& nThis)
{
    ASSERT_VALID(m_pDockBar); // verify bounds

    nThis = m_pDockBar->FindBar(this);
    ASSERT(nThis != -1);

    int i, nBars = m_pDockBar->m_arrBars.GetSize();

    // find the first and the last bar in row
    for (nFirst = -1, i = nThis - 1; i >= 0 && nFirst == -1; i--)
        if (m_pDockBar->m_arrBars[i] == NULL)
            nFirst = i + 1;
    for (nLast = -1, i = nThis + 1; i < nBars && nLast == -1; i++)
        if (m_pDockBar->m_arrBars[i] == NULL)
            nLast = i - 1;

    ASSERT((nLast != -1) && (nFirst != -1));
}

/***********************************************************************
**
**   FUNCTION: GetRowSizingBars(CSizeBarArray& arrSCBars)
**
**       Get the bar array
**
**   INPUT: arrSCBars: bar array
**
**   OUTPUT :arrSCBars: bar array
**			 none
**   RETURN:    none
**
**********************************************************************/
void CNCLDialogBar::GetRowSizingBars(CSizeBarArray& arrSCBars)
{
    arrSCBars.RemoveAll();

    int nFirst, nLast, nThis;
    GetRowInfo(nFirst, nLast, nThis);

    for (int i = nFirst; i <= nLast; i++)
    {
        CControlBar* pBar = (CControlBar*)m_pDockBar->m_arrBars[i];
        if (HIWORD(pBar) == 0) continue; // placeholder
        if (!pBar->IsVisible()) continue;
        if (FindSizingBar(pBar) >= 0)
            arrSCBars.Add((CNCLDialogBar*)pBar);
    }
}

/***********************************************************************
c
c   FUNCTION: FindSizingBar(ControlBar* pBar)
c		find dialogbar position
c
c   INPUT: pBar: bar to be found the position
c
c   OUTPUT :   None
c   RETURN:    position index
c
**********************************************************************/
const int CNCLDialogBar::FindSizingBar(CControlBar* pBar) const
{
	int size = m_arrBars.GetSize();
    for (int nPos = 0; nPos < m_arrBars.GetSize(); nPos++)
	{
        if (m_arrBars[nPos] == pBar)
            return nPos; // got it
	}
    return -1; // not found
}

/***********************************************************************
c
c   FUNCTION: AlignControlBars()
c		Align ControlBars
c
c   INPUT: none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDialogBar::AlignControlBars()
{
    int nFirst, nLast, nThis;
    GetRowInfo(nFirst, nLast, nThis);

    BOOL bHorz = IsHorzDocked();
    BOOL bNeedRecalc = FALSE;
    int nPos, nAlign = bHorz ? -2 : 0;

    CRect rc, rcDock;
    m_pDockBar->GetWindowRect(&rcDock);

    for (int i = nFirst; i <= nLast; i++)
    {
        CControlBar* pBar = (CControlBar*)m_pDockBar->m_arrBars[i];
        if (HIWORD(pBar) == 0) continue; // placeholder
        if (!pBar->IsVisible()) continue;

        pBar->GetWindowRect(&rc);
        rc.OffsetRect(-rcDock.TopLeft());

        if ((nPos = FindSizingBar(pBar)) >= 0)
            rc = CRect(rc.TopLeft(), bHorz ?
                m_arrBars[nPos]->m_szHorz : m_arrBars[nPos]->m_szVert);

        if ((bHorz ? rc.left : rc.top) != nAlign)
        {
            if (!bHorz)
                rc.OffsetRect(0, nAlign - rc.top - 2);
            else if (m_nDockBarID == AFX_IDW_DOCKBAR_TOP)
                rc.OffsetRect(nAlign - rc.left, -2);
            else
                rc.OffsetRect(nAlign - rc.left, 0);
            pBar->MoveWindow(rc);
            bNeedRecalc = TRUE;
        }
        nAlign += (bHorz ? rc.Width() : rc.Height()) - 2;
    }

    if (bNeedRecalc)
    {
        m_pDockSite->DelayRecalcLayout();
        TRACE("ccc\n");
    }
}

/***********************************************************************
c
c   FUNCTION: NcPaintGripper(CDC* pDC, CRect rcClient)
c		paint the gripper
c
c   INPUT: CDC* pDC: device context
c			rcClient: client area of the dialog bar
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDialogBar::NcPaintGripper(CDC* pDC, CRect rcClient)
{
    // paints a simple "two raised lines" gripper
    // override this if you want a more sophisticated gripper
    CRect gripper = rcClient;
    BOOL bHorz = IsHorzDocked();
    
    gripper.DeflateRect(1, 1);
    if (bHorz)
    {   // gripper at left
        gripper.left -= m_cyGripper;
        gripper.right = gripper.left + 3;
    }
    else
    {   // gripper at top
        gripper.top -= m_cyGripper;
        gripper.bottom = gripper.top + 3;
    }

    pDC->Draw3dRect(gripper, ::GetSysColor(COLOR_BTNHIGHLIGHT),
        ::GetSysColor(COLOR_BTNSHADOW));

    gripper.OffsetRect(bHorz ? 3 : 0, bHorz ? 0 : 3);
    
    pDC->Draw3dRect(gripper, ::GetSysColor(COLOR_BTNHIGHLIGHT),
        ::GetSysColor(COLOR_BTNSHADOW));

}
/***********************************************************************
**
**   FUNCTION: OnSize(UINT nType, int cx, int cy) 
**
**		The framework calls this member function 
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
void CNCLDialogBar::OnSize(UINT nType, int cx, int cy) 
{
	if ((m_dtype == 2)&&(cx!=0)&&(cy!=0)&&(m_origrect.Height()==0))
	{
		cx = UDM_run_layout.command_size[0];
		cy = UDM_run_layout.command_size[1];
		CControlBar::OnSize(nType, cx, cy);
	}
	else
		CControlBar::OnSize(nType, cx, cy);
/*
.....no resize for formbar and command bar
*/
	if ((m_bartype==3)||(m_bartype==6))
		return;
	if ((cx<=14)||(cy<=14))
		return;
	if (m_origrect.Height()==0)
		return;
    CRect rc;
    GetClientRect(rc);

	if ((UW_reset_menu)&&(m_visible))
	{
		rc.top = m_origrect.top;
		rc.bottom = m_origrect.bottom;
		rc.left = m_origrect.left;
		rc.right = m_origrect.right;
	}	
/*
.....m_type = 1, only label (prompt bar and error bar)
.....            we don't move label in X direction, 
.....            but move label in the Y direction
.....            label size stay the same
.....m_type = 2. label and edit field (Command bar)
.....            we don't move label and edit field in X direction, 
.....            but move label and edit field in the Y direction
.....            label size stay the same, extend edit field size
*/
	CRect rect1 = m_child_rect[0];
	CRect rect2 = m_child_rect[1];
	if (m_bartype==2)
	{
		if (m_dwStyle & CBRS_FLOATING)
			rect1.top = 5;
		else if (m_dwStyle & CBRS_ORIENT_HORZ)
		{
/*
......gripper at left
*/
			rect1.top = 5;
		}
		else
			rect1.top = 13;
	}
	else
		rect1.top = 5;
	rect1.bottom = rc.Height();
	if ((m_bartype==2)&&(!(m_dwStyle & CBRS_ORIENT_HORZ)))
	{
		rect1.bottom = rect1.bottom - 8;
	}
	else if (m_bartype==2)
		rect1.bottom = rect1.bottom - 3;

	if (rect1.bottom <= 5)
		rect1.bottom = 5;
	if (m_bartype==2)
	{
		if (m_dwStyle & CBRS_FLOATING)
			rect1.left = 5;
		else if (m_dwStyle & CBRS_ORIENT_VERT)
			rect1.left = 5;
		else if (m_dwStyle & CBRS_ORIENT_HORZ)
			rect1.left = 13;
		else
			rect1.left = 5;
		rect1.right = rc.right - 5;
	}
	if (m_dtype == 2)
	{
		rect2.top =  5;
		rect2.bottom = rc.Height() - 4;
		if (rect2.bottom <= 4)
			rect2.bottom = 4;
/*
.....edit field right always = window.right - 5 pixel
*/
		rect2.right = rc.right - 5;
		if (rect2.right <= rect2.left)
			rect2.right = rect2.left;
	}	
	CWnd* pChildWnd = GetWindow(GW_CHILD);
	pChildWnd->MoveWindow(rect1);
	if (m_bartype==2)
	{
/*
.....for some reason, the SetScrollInfo not working for the fMask, it can't
.....remove SIF_DISABLENOSCROLL setting, so we may just hide or show the
.....scrollbar ourselives
*/
		SCROLLINFO si;
		si.cbSize = sizeof(si);
/*
.....have to show scrollbar first to get info
*/
		pChildWnd->ShowScrollBar(SB_HORZ, TRUE);
		pChildWnd->GetScrollInfo(SB_HORZ, &si);
		int vsc = GetSystemMetrics(SM_CXVSCROLL);
		int cxf = GetSystemMetrics(SM_CXDLGFRAME);
		if ((rect1.right - rect1.left-vsc-2*cxf)>=si.nMax)
		{
			pChildWnd->ShowScrollBar(SB_HORZ, FALSE);
			m_scroll = 0;
		}
		else
		{
			pChildWnd->ShowScrollBar(SB_HORZ, TRUE);
			m_scroll = 1;
		}
	}

	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(rect2);
	
	if ((UW_reset_menu)&&(m_visible))
	{
		m_sizeDefault = m_origrect.Size();
		m_szHorz =  m_sizeDefault;
		m_szVert = m_sizeDefault;
		m_szFloat = m_sizeDefault;
	}		
}
/***********************************************************************
**
**   FUNCTION: CreateStatusWin(CWnd* pParentWnd, char *string, int bar_size[2], 
							   UINT nStyle, UINT pID, UINT eID, UINT nID)
**
**       Create a status bar window
**
**   INPUT:  pParentWnd: parent window
**			string: text string status window
**			bar_size: size of the status window bar (in rows and cols)
**			nIDTemplate: dialog template ID
**			eID: edit field ID
**			nID: dialog ID
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLDialogBar::CreateStatusWin(CWnd* pParentWnd, char *string, int bar_size[2], 
							   UINT nStyle, UINT eID, UINT nID, int flag /*=0*/)
{
	double wid, hgt;
	ASSERT(pParentWnd != NULL);

	m_dwStyle = (nStyle & CBRS_ALL);
	m_bartype = 2;

    m_cyGripper = 12; // set the gripper width
	
	CREATESTRUCT cs;
	memset(&cs, 0, sizeof(cs));
	cs.lpszClass = _afxWndControlBar;
	cs.style = (DWORD)nStyle | WS_CHILD;
	cs.hMenu = (HMENU)nID;
	cs.hInstance = AfxGetInstanceHandle();
	cs.hwndParent = pParentWnd->GetSafeHwnd();
	if (!PreCreateWindow(cs))
		return FALSE;

	// create a modeless dialog

	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
	AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);

	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));

	CDialogItem rgDlgItem[20];
	DLGTEMPLATE dlgTempl;
	int itemnum;

	itemnum = 2;
//	itemnum = 1;
	BOOL bSuccess;
	uw_ntinit_dlgtemp3(string, bar_size, eID, &dlgTempl, rgDlgItem);
	font_num = 2;
	bSuccess = CreateIndirect(pParentWnd, &dlgTempl, rgDlgItem, itemnum);

	if (!bSuccess)
		return FALSE;
#ifndef _AFX_NO_OCC_SUPPORT
	m_lpszTemplateName = NULL;
#endif

	// dialog template MUST specify that the dialog
	//  is an invisible child window
	SetDlgCtrlID(nID);
	CRect rect;
	GetWindowRect(&rect);

	int vsc = GetSystemMetrics(SM_CXVSCROLL);
	int tsc = GetSystemMetrics(SM_CYCAPTION);
	int cyf = GetSystemMetrics(SM_CYDLGFRAME);
	int cxf = GetSystemMetrics(SM_CXDLGFRAME);

	ModifyStyle(0, WS_CLIPSIBLINGS);

	// force the size to zero - resizing bar will occur later
	SetWindowPos(NULL, 0, 0, 0, 0, SWP_NOZORDER|SWP_NOACTIVATE|SWP_SHOWWINDOW);
/*
.....set the edit field font
*/
	if (m_status_txtfont.m_hObject)
		VERIFY (m_status_txtfont.DeleteObject ());	
	int stat = m_status_txtfont.CreatePointFont (UW_status_fontsize*10, UW_status_font);
	if (stat==0)
		m_status_txtfont.CreatePointFont (UW_status_fontsize*10, "COURIER");
	GetDlgItem(eID)->SetFont(&m_status_txtfont);

	int adjx,adjy;
	CClientDC dc(this);
	dc.SelectObject(&m_status_txtfont);
	CSize sizeText = dc.GetTextExtent("XXXXXxxxxx",10);
	wid = (sizeText.cx/10.0);

	if (flag==0)
	{
		hgt = sizeText.cy;
		m_sizeDefault.cx = (long) ((bar_size[0]/4)*wid + vsc);
		adjx = bar_size[0]/120;
		adjx = (int)(adjx * wid);
		m_sizeDefault.cx = m_sizeDefault.cx + adjx;
		m_sizeDefault.cy = (long)((bar_size[1]/8)*hgt);
		adjy = bar_size[1]/40;
		adjy = (int)(adjy * hgt* 0.85);
		m_sizeDefault.cy = m_sizeDefault.cy + adjy;
	    m_szHorz =  m_sizeDefault;
	    m_szVert = m_sizeDefault;
	    m_szFloat.cx = m_sizeDefault.cx + cxf*2;
	    m_szFloat.cy = m_sizeDefault.cy + tsc - cyf*2;
	}
	else
	{
/*
.....when we uw_ntupd_statusfont(), we still using the previous size (which we saved
.....before recreate
*/
		rect.right = rect.left + UDM_run_layout.statwin_size[0]-cxf*2;
		rect.bottom = rect.top + UDM_run_layout.statwin_size[1]-cyf*2;
		m_sizeDefault = rect.Size();
		m_szHorz =  m_sizeDefault;
	    m_szVert = m_sizeDefault;
		m_szFloat = m_sizeDefault;
	}

	return TRUE;
}
/***********************************************************************
**
**   FUNCTION: CreateFormWin(CWnd* pParentWnd, CDialogItem *rgDlgItem, 
							   DLGTEMPLATE dlgTempl, int itemnum) 
**
**       Create a form bar window
**
**   INPUT:  pParentWnd: parent window
**				rgDlgItem: dialog item information
**				dlgTempl: dialog template information
**				itemnum: item number in form
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
int CNCLDialogBar::CreateFormWin(CWnd* pParentWnd, CDialogItem *rgDlgItem, 
						DLGTEMPLATE dlgTempl, int itemnum, UINT nStyle, UINT nID)
{
	ASSERT(pParentWnd != NULL);
	m_bartype = 3;
	return CreateFormWinSub(pParentWnd, rgDlgItem, dlgTempl, itemnum, nStyle, nID);
}

/***********************************************************************
**
**   FUNCTION: CreateFormWin2(CWnd* pParentWnd, CDialogItem *rgDlgItem, 
							   DLGTEMPLATE dlgTempl, int itemnum) 
**
**       Create a command bar window (formwin without the title bar)
**
**   INPUT:  pParentWnd: parent window
**				rgDlgItem: dialog item information
**				dlgTempl: dialog template information
**				itemnum: item number in form
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
int CNCLDialogBar::CreateFormWin2(CWnd* pParentWnd, CDialogItem *rgDlgItem, 
						DLGTEMPLATE dlgTempl, int itemnum, UINT nStyle, UINT nID)
{
	ASSERT(pParentWnd != NULL);
	m_bartype = 6;
	return CreateFormWinSub(pParentWnd, rgDlgItem, dlgTempl, itemnum, nStyle, nID);
}

/***********************************************************************
**
**   FUNCTION: CreateFormWinSub(CWnd* pParentWnd, CDialogItem *rgDlgItem, 
							   DLGTEMPLATE dlgTempl, int itemnum) 
**
**       Create a form bar window
**
**   INPUT:  pParentWnd: parent window
**				rgDlgItem: dialog item information
**				dlgTempl: dialog template information
**				itemnum: item number in form
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
int CNCLDialogBar::CreateFormWinSub(CWnd* pParentWnd, CDialogItem *rgDlgItem, 
						DLGTEMPLATE dlgTempl, int itemnum, UINT nStyle, UINT nID)
{
	ASSERT(pParentWnd != NULL);

	m_dwStyle = (nStyle & CBRS_ALL);
    m_cyGripper = 12;
	
	CREATESTRUCT cs;
	memset(&cs, 0, sizeof(cs));
	cs.lpszClass = _afxWndControlBar;
	cs.style = (DWORD)nStyle | WS_CHILD;
	cs.hMenu = (HMENU)nID;
	cs.hInstance = AfxGetInstanceHandle();
	cs.hwndParent = pParentWnd->GetSafeHwnd();
	if (!PreCreateWindow(cs))
		return -1;

	// create a modeless dialog

	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
	AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);

	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));

	BOOL bSuccess;
	font_num = 1;
	bSuccess = CreateIndirect(pParentWnd, &dlgTempl, rgDlgItem, itemnum);

	if (!bSuccess)
		return -1;
#ifndef _AFX_NO_OCC_SUPPORT
	m_lpszTemplateName = NULL;
#endif

	// dialog template MUST specify that the dialog
	//  is an invisible child window
	SetDlgCtrlID(nID);
	CRect rect;
	GetWindowRect(&rect);
	m_sizeDefault = rect.Size();    // set fixed size

    m_szHorz =  m_sizeDefault;
    m_szVert = m_sizeDefault;
    m_szFloat = m_sizeDefault;

	// force WS_CLIPSIBLINGS
	ModifyStyle(0, WS_CLIPSIBLINGS);

	// force the size to zero - resizing bar will occur later
	SetWindowPos(NULL, 0, 0, 0, 0, SWP_NOZORDER|SWP_NOACTIVATE|SWP_SHOWWINDOW);
/*
.....set the font
*/
	if (nID==IDD_COMMANDBAR)
	{
/*
.....set the edit field font
*/
		if (m_txtfont.m_hObject)
			VERIFY (m_txtfont.DeleteObject ());	
		int stat = m_txtfont.CreatePointFont (UW_com_size*10, UW_com_font);
		if (stat==0)
			m_txtfont.CreatePointFont (UW_com_size*10, "MS Sans Serif");
		GetDlgItem(IDC_ECOMMAND)->SetFont(&m_txtfont);
		GetDlgItem(IDC_ECOMMAND2)->SetFont(&m_txtfont);
	}
	return IDOK;
}

/***********************************************************************
**
**   FUNCTION: GetWindowSize(int &cols, int &rows)
**
**      get the window size by rows & cols
**
**   INPUT: none
**
**   OUTPUT :
**			 cols, rows: rows and cols of the window
**   RETURN: none
**
**********************************************************************/
void CNCLDialogBar::GetWindowSize(int &cols, int &rows)
{
	double wid, hgt;
	CRect rect;
	GetWindowRect(&rect);

	int vsc = GetSystemMetrics(SM_CXVSCROLL); 
	int cxf = GetSystemMetrics(SM_CXDLGFRAME);

	CClientDC dc(this);

	CFont* pOldFont;
	if (m_bartype==2)
		pOldFont = dc.SelectObject(&m_status_txtfont);
	else
		pOldFont = dc.SelectObject(&m_txtfont);
	CSize sizeText = dc.GetTextExtent("XXXXXxxxxx",10);
	dc.SelectObject(pOldFont);
	wid = (sizeText.cx/10.0);
	hgt = sizeText.cy;
		
	cols = (int)((m_sizeDefault.cx - vsc - 2*cxf - 5 - 4)/wid + 0.5);
	rows = (int)((m_sizeDefault.cy - 2*cxf - 5 - 4)/hgt + 0.5);
}

/***********************************************************************
**
**   FUNCTION: UpdateScrollBar()
**
**       update scrollbar info (for statusbar only)
**
**   INPUT: none
**
**   OUTPUT :
**			 none
**   RETURN: none
**
**********************************************************************/
void CNCLDialogBar::UpdateScrollBar()
{
	if (m_bartype!=2)
		return;
	CWnd* pChildWnd = GetWindow(GW_CHILD);
	if (m_bartype==2)
	{
/*
.....for some reason, the SetScrollInfo not working for the fMask, it can't
.....remove SIF_DISABLENOSCROLL setting, so we may just hide or show the
.....scrollbar ourselives
*/
		SCROLLINFO si;
		si.cbSize = sizeof(si);
/*
.....have to show scrollbar first to get info
*/
		if (m_scroll==0)
			pChildWnd->ShowScrollBar(SB_HORZ, TRUE);

		pChildWnd->GetScrollInfo(SB_HORZ, &si);
		CRect rect;
		pChildWnd->GetWindowRect(&rect);
		int vsc = GetSystemMetrics(SM_CXVSCROLL);
		int cxf = GetSystemMetrics(SM_CXDLGFRAME);

		if ((rect.right - rect.left-vsc-2*cxf)<si.nMax)
		{
			pChildWnd->ShowScrollBar(SB_HORZ, TRUE);
			m_scroll = 1;
		}
		else if (m_scroll==0)
		{
			pChildWnd->ShowScrollBar(SB_HORZ, FALSE);
		}
	}
}
/***********************************************************************
**
**   FUNCTION: AdjustSize()
**
**       adjust dialog bar's size (consider gripper...)
**
**   INPUT: none
**
**   OUTPUT :
**			 none
**   RETURN: none
**
**********************************************************************/
void CNCLDialogBar::AdjustSize()
{
	if ((m_bartype==3)||(m_bartype==6))
		return;
    CRect rc;
    GetClientRect(rc);
	CRect rect1 = m_child_rect[0];
	CRect rect2 = m_child_rect[1];
	if (m_bartype==2)
	{
		if (m_dwStyle & CBRS_FLOATING)
			rect1.top = 5;
		else if (m_dwStyle & CBRS_ORIENT_HORZ)
		{
/*
......gripper at left
*/
			rect1.top = 5;
		}
		else
			rect1.top = 13;
	}
	else
		rect1.top = 5;
	rect1.bottom = rc.Height();
	if ((m_bartype==2)&&(!(m_dwStyle & CBRS_ORIENT_HORZ)))
	{
		rect1.bottom = rect1.bottom - 8;
	}
	else if (m_bartype==2)
		rect1.bottom = rect1.bottom - 3;

	if (rect1.bottom <= 5)
		rect1.bottom = 5;
	if (m_bartype==2)
	{
		if (m_dwStyle & CBRS_FLOATING)
			rect1.left = 5;
		else if (m_dwStyle & CBRS_ORIENT_VERT)
			rect1.left = 5;
		else if (m_dwStyle & CBRS_ORIENT_HORZ)
			rect1.left = 13;
		else
			rect1.left = 5;
		rect1.right = rc.right - 5;
	}
	if (m_dtype == 2)
	{
		rect2.top =  5;
		rect2.bottom = rc.Height() - 4;
		if (rect2.bottom <= 4)
			rect2.bottom = 4;
/*
.....edit field right always = window.right - 5 pixel
*/
		rect2.right = rc.right - 5;
		if (rect2.right <= rect2.left)
			rect2.right = rect2.left;
	}	
	CWnd* pChildWnd = GetWindow(GW_CHILD);
	pChildWnd->MoveWindow(rect1);
	if (m_bartype==2)
	{
/*
.....for some reason, the SetScrollInfo not working for the fMask, it can't
.....remove SIF_DISABLENOSCROLL setting, so we may just hide or show the
.....scrollbar ourselives
*/
		SCROLLINFO si;
		si.cbSize = sizeof(si);
/*
.....have to show scrollbar first to get info
*/
		pChildWnd->ShowScrollBar(SB_HORZ, TRUE);
		pChildWnd->GetScrollInfo(SB_HORZ, &si);
		int vsc = GetSystemMetrics(SM_CXVSCROLL);
		int cxf = GetSystemMetrics(SM_CXDLGFRAME);
		if ((rect1.right - rect1.left-vsc-2*cxf)>=si.nMax)
		{
			pChildWnd->ShowScrollBar(SB_HORZ, FALSE);
			m_scroll = 0;
		}
		else
		{
			pChildWnd->ShowScrollBar(SB_HORZ, TRUE);
			m_scroll = 1;
		}
	}

	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(rect2);		
}

IMPLEMENT_DYNAMIC(CNCLDialogBar, CControlBar)

