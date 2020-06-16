/********************************************************************* 
**  NAME:  nclipvframe.cpp
**
**			Native WinNT main frame functions for NCLIPV
**			implementation of CMainFrame class functions
**	CONTAINS: CMainFrame class functions
**			all functions declared in msmainframe.h
**			uw_ntget_dockrect
**			nclipv_exit
**
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclipvframe.cpp , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**       11/04/16 , 10:43:02
*********************************************************************/
#include "wsntstdafx.h"
#include <afxole.h> 
#include "nclipv.h"
#include "wsntfuncid.h"
#include "wsgl.h"
#include "wsntglfunc.h"
#include "wsntcfunc.h"
#include "wsglfun.h"

#include "NclipvFrm.h"
#include "wsntdoc.h"
#include "NclipvView.h"
#include "xenv1.h"
#include "dmotif.h"
#include "wsntpmenu.h"
#include "wsnttmenu.h"
#include "zkeysym.h"
#include "spmouse.h"
#define UM_MPGM 1
#include "mdrwsize.h"
#undef UM_MPGM
#include "wsntsmouse.h"
#include "wsntmenudsndlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
#define OnNCLFunctions OnIPVFunctions

#define NBUTTONNUM	5000
#define NCHOICEBUTTON 1000

#define NCL_SI_MENU_BUTTON 1
#define NCL_SI_FIT_BUTTON 2
#define NCL_SI_TOP_BUTTON 3
#define NCL_SI_RIGHT_BUTTON 5
#define NCL_SI_FRONT_BUTTON 6
#define NCL_SI_ROLLPLUS_BUTTON 9
#define NCL_SI_BUTTON_1 13
#define NCL_SI_BUTTON_2 14
#define NCL_SI_BUTTON_3 15
#define NCL_SI_BUTTON_4 16
#define NCL_SI_ESC_BUTTON 23
#define NCL_SI_ALT_BUTTON 24
#define NCL_SI_SHIFT_BUTTON 25
#define NCL_SI_CTRL_BUTTON 26
#define NCL_SI_ROT_BUTTON 27

CMainFrame *NCL_MainFrame = NULL;
HWND NCL_cmdwin;
extern CWnd *NCL_Main_View;
extern CWnd *NCL_CurrentPKT_View;
extern "C" int uw_ntmenu(int,int);
extern "C" int uw_display_dragmenu(int );
extern "C" int isquit ();
extern "C" int uz_ntcnvtnum_to_menu(int num, int *menunum);
extern "C" int UW_signon_failed;
extern "C" int UW_init_failed;
extern "C" int uu_toolmalloc_init();
extern "C" int  uu_init_debug();
extern "C" int  NclxDbgInit();
extern "C" int  ncl_reset_mplayback();
extern "C" int ux_init_table(int);
extern "C" int ud_idev();
extern "C" int ud_ogks();
extern "C" int ud_idas();
extern "C" int uu_initerr();
extern "C" int uu_inithep();
extern "C" int ur_init_unibase(), uv_init(), uc_init_relations();
extern "C" int um_init_cpln();
extern "C" int ul_init_basic();
extern "C" int uz_load_keys();
extern "C" int ud_load_accel();
extern "C" int uz_load_mousedef(char*, int);

extern "C" int uz_user_smouse(int event, int data[6], char **indx, int flag);
extern int UW_reset_menu;
extern CNCLToolBar *UW_current_menubar;

int NCL_popmenu;
HWND UW_TrackingWindow = NULL;
extern CNCLMenu *UW_topmenu;
HMENU UW_TrackingMenu = NULL;
extern int NCL_PupupChoice;
extern int PopupChoice[20];
extern int UW_NPopup;
extern "C" int UW_icon_size, UW_menu_fmt;
extern "C" int uz_ntget_dspt(int num, char* descript, int flag);
extern "C" int uz_wnt_callfunc(int num, int flag);
extern "C" int uw_ntwindow();
extern "C" int uw_ntgetcur_cursor();
extern "C" int ul_init_axisseg();
int frm_done = 0;
HMAGELLAN MagellanHandle = NULL;
int DConnexion = 0;
extern "C" ACCEL *UZ_ncl_accel;
extern "C" int UZ_ncl_accelnum;
extern int UU_SM_capture;
extern "C" int IPV_failed;
extern "C" int ul_ipv_end();
extern "C" int NT_FuncEvent, NT_FuncEof;;
HWND NCL_statwin;
extern "C" void uw_nt_nclfunc(UINT id, int jflag);
extern "C" void ncl_reset_pikgeom();

int UW_struct_change = 0;
extern "C" void uw_redisplay_dragmenu(int menunum);
extern "C" void uw_redisplay_popmenu(int menu);
extern "C" void uw_ntpopchoice(int mflag, int *choice);
extern "C" void ud_upt_UDMmenu(int upt_menu, int upt_item, int add_menu, int add_item, int flag);
extern "C" void ud_del_UDMmenu(int, int);
extern "C" void ud_insert_UDMmenu(int menunum, int select, UDM_menu_struc *menu_item);
extern "C" void ud_save_dragmenu(int);
extern  "C" int ud_create_UDMmenu(int menunum1, int itemnum1, char* menufile, int choice, int pos[5]);
extern "C" int uw_ntmenu_desgn(int flag, UDM_menu_struc *menu_item);
extern "C" int ud_create_UDMmenu2(UDM_menu_struc menu_item, char* menufile, int pos[5]);

extern "C" int ud_get_areadockable(char *area_name, int *dockable);
extern "C" int ud_set_areadockable(char *area_name, int dockable);
extern int uw_ntget_ctlbar_type(CControlBar* chkbar);
extern void uw_ntadd_menuborder(CRect *rect);
extern "C" char * uu_malloc(int);
extern int NCL_exit;

/////////////////////////////////////////////////////////////////////////////
// CMainFrame
int WM_3DXWARE = RegisterWindowMessage ("SpaceWareMessage00");

IMPLEMENT_DYNCREATE(CMainFrame, CFrameWnd)

BEGIN_MESSAGE_MAP(CMainFrame, CFrameWnd)
	//{{AFX_MSG_MAP(CMainFrame)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_MEASUREITEM()
	ON_WM_DRAWITEM()
	ON_COMMAND(IDC_ECOMMAND, OnCommandLine)
	ON_EN_CHANGE (IDC_ECOMMAND, OnCommandLine)
	ON_COMMAND(IDF_KEY_NOOP, OnKeyNoop)
	ON_COMMAND_RANGE(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+ NBUTTONNUM + UDM_MAX_MENU, 
					WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+ NBUTTONNUM + UDM_MAX_MENU + NCHOICEBUTTON, OnPupupChoices)
	ON_COMMAND_RANGE(WM_APP, WM_APP+UDM_MAX_MENU-1, OnDisplayMenu)
	ON_COMMAND_RANGE(WM_APP+UDM_MAX_MENU, WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM-1, OnAccelFunctions)
	ON_COMMAND_RANGE(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM, WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM, OnIPVFunctions)
	ON_COMMAND_RANGE(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM, WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM + UDM_MAX_MENU - 1,OnDragMenu)
	ON_WM_CLOSE()
	ON_WM_SIZING()
	ON_NOTIFY_EX_RANGE(TTN_NEEDTEXTW, 0, 0xFFFF, OnDynamicTipText)
	ON_NOTIFY_EX_RANGE(TTN_NEEDTEXTA, 0, 0xFFFF, OnDynamicTipText)
	ON_WM_TIMER()
	ON_MESSAGE(WM_MENUDRAG, OnMenuDrag)
	ON_MESSAGE(WM_MENUGETOBJECT, OnMenuGetObject)
	ON_MESSAGE(WM_MENURBUTTONUP, OnMenuRbuttonup)
	ON_COMMAND(UW_UPDATE_MENUBAR, OnUpdateMenubar)
	ON_COMMAND(UW_ADDNEW_MENUBAR, OnAddNewMenubar)

	ON_REGISTERED_MESSAGE( WM_3DXWARE, On3DxWare )
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
/***********************************************************************
c
c   FUNCTION: CMainFrame()
c
c              Constructor of class CMainFrame
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CMainFrame::CMainFrame()
{
	int i;
	for (i=0; i<200; i++)
	{
		NCL_menu[i] = NULL;
		NCL_menubar[i] = NULL;
	}
	UW_icon_size = 1;
	m_commandBar = NULL;
	m_promptBar = NULL;
	m_errorBar = NULL;
	m_statusBar = NULL;
	for (i=0; i<60; i++)
	{
		m_formbar[i] = NULL;
	}
	NCL_menubar_count = 0;
/*
......malloc space for accelerator
*/
	UZ_ncl_accel = (ACCEL *)malloc(500*sizeof(ACCEL));
	UZ_ncl_accelnum = 0;
	m_TargetDrop = new CNCLMnDropTarget();
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
	m_addmenu = -1;
	m_remove_menu = -1;
	m_additem = -1;
	m_remove_item = -1;
	m_menudsgn_dlg = (CNCLMenuDsnDlg *)UU_NULL;
	m_menu_desgn = 0;
	m_add_menupos[0] = -1;
	m_add_menupos[1] = -1;
	m_add_menupos[2] = -1;
	m_add_menupos[3] = -1;
	m_add_menupos[4] = -1;
}
/***********************************************************************
c
c   FUNCTION: OnClose()
c
c           callback for close frame window. It could be call by click "X" in the window
c			righttop corner or could be called from the application exit problem
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnClose()
{
	int i,status;

	if (NCL_exit==0)
/*
.....called from user clip "close" or "x" in the frame window
*/
	{
		CNclipvApp* pApp;
		pApp = (CNclipvApp* )AfxGetApp();
		pApp->OnIPVSafeExit();
	}
	else
/*
.....called from OnIPVSafeExit() function
*/
	{
		ul_ipv_end();
		GetActiveDocument()->m_bAutoDelete = TRUE;
/*
.....delete menus we created before OnClose function
.....because the OnClose function will destroyed all menus (with delete
.....objects), but we need free some memory (such CImageList in menubar)
.....which we need get from menubar (which will be destroyed in OnClose)
.....Yurong
*/
		if (UW_topmenu!=NULL)
		{
			for (i=0; i<20; i++)
			{
				status = UW_topmenu->DeleteMenu(0, MF_BYPOSITION);
				if (status==0)
					break;
			}
			UW_topmenu = NULL;
		}
		for (i=0; i<200; i++)
		{
			if (NCL_menubar[i] != NULL)
			{
				CImageList *rimglist = NULL;
				CToolBarCtrl &rctrl = NCL_menubar[i]->GetToolBarCtrl();
				rimglist = rctrl.GetImageList();
				if (rimglist!=NULL)
				{
					delete rimglist;
					rimglist = NULL;
				}
				delete NCL_menubar[i];
				NCL_menubar[i] = NULL;
			}
			if (NCL_menu[i] != NULL)
			{
				if (!(::IsMenu(NCL_menu[i]->m_hMenu)))
					continue;
				delete NCL_menu[i];
				NCL_menu[i] = NULL;
			}
		}	
		NCL_MainFrame = NULL;
		CFrameWnd::OnClose();
	}
}

CMainFrame::~CMainFrame()
{
	if (m_menudsgn_dlg!=NULL)
	{
		m_menudsgn_dlg->DestroyWindow();
		delete m_menudsgn_dlg;
	}
	m_menudsgn_dlg = (CNCLMenuDsnDlg *)UU_NULL;
}
/***********************************************************************
c
c   FUNCTION: OnDisplayMenu(UINT id)
c
c           callbacks for menu which asked for display 
c			another menu
c
c   INPUT:  id: menu ID
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnDisplayMenu(UINT id)
{
	int menunum;
	int num = id - WM_APP;
	UX_pathname file, dir;
	uz_ntcnvtnum_to_menu(num, &menunum);
	if (menunum>=0)
	{
		ul_break_fname(UDM_menu[menunum].file, dir, file);
		uw_ntmenu(menunum,1);
	}
}
/***********************************************************************
c
c   FUNCTION: OnStatusChange(UINT id)
c
c           callbacks for field on the statusbar
c
c   INPUT:  id: field ID
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnStatusChange(UINT id)
{
	int sub = id - (WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM);
	uz_ntstatus_calls(sub);
}

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
int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	NCL_MainFrame = this;
	initial_nclipv();
	MENUINFO info;

	for (int i=0; i<200; i++)
	{
		if (NCL_menu[i] != NULL)
		{
			info.cbSize = sizeof(MENUINFO);
			NCL_menu[i]->GetMenuInfo(&info);
			info.dwStyle &= ~(MNS_DRAGDROP);
			info.dwStyle |= MNS_DRAGDROP;
			info.fMask = MIM_APPLYTOSUBMENUS | MIM_STYLE;
			NCL_menu[i]->SetMenuInfo(&info);
		}
	}
	info.cbSize = sizeof(MENUINFO);
	UW_topmenu->GetMenuInfo(&info);

	info.dwStyle &= ~(MNS_DRAGDROP);
	info.dwStyle |= MNS_DRAGDROP;
	info.fMask = MIM_APPLYTOSUBMENUS | MIM_STYLE;
	UW_topmenu->SetMenuInfo(&info);
	
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);

	return 0;
}
/***********************************************************************
**
**   FUNCTION: initial_nclipv
**
**		Doing the NCLIPV related function for displayed in the main window
**   
**		INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CMainFrame::initial_nclipv()
{
/*
.....initial NCLIPV
*/
	uu_toolmalloc_init();
	uu_init_debug();
	ux_init_table(1);
	ud_idev();
	ud_ogks();
	ud_idas();
	uu_inithep();
	ur_init_unibase();
	if (uc_init_relations() != UU_SUCCESS)
		return;	
	um_init_cpln();
	uv_init();
	ul_init_basic();
	uz_load_keys();
	uz_load_mousedef(NULL, 1);
	ud_load_accel();
	uw_ntwindow();
	ncl_reset_mplayback();
	NclxDbgInit();
	CNclipvView * graphic_view = (CNclipvView *)NCL_CurrentPKT_View;
	if (graphic_view==NULL) return;
	graphic_view->Init();
/*
.....Create user key Accelerator
*/
	if (IPV_failed)
		return;	
	if (UZ_ncl_accelnum!=0)
		m_accel = CreateAcceleratorTable(UZ_ncl_accel, UZ_ncl_accelnum);
	Enable_cmdbar(0);
	ul_init_axisseg();

/*
......Initialize 3DxWare
*/
	SiOpenData oData;
    SiInitialize ();
    SiOpenWinInit (&oData, m_hWnd);
	if ( (m_DevHdl = SiOpen ("NCLIPV", SI_ANY_DEVICE, SI_NO_MASK,
                            SI_EVENT, &oData)) == NULL )
	{
		SiTerminate ();
//		MessageBox ("Sorry - No supported 3DxWare device available.\n",
//                NULL, MB_OK);		
	}
	if (m_DevHdl!=NULL)
		SiSetUiMode (&m_DevHdl, SI_UI_ALL_CONTROLS);
	else
	{
		MagellanHandle = MagellanInit( GetSafeHwnd() );
	}
	MagellanCompress MagellanData;
	LPARAM lParam;
	WPARAM wParam;

	if (MagellanHandle!=NULL)
	{
		MagellanHandle->MagellanEvent.MagellanPeriod = 1;
		MagellanData.MagellanValues.ValueAX = 0;;
		MagellanData.MagellanValues.ValueBY = 0;
		MagellanData.MagellanValues.ValueCZ = 0;
		wParam = MagellanData.MagellanDWord;

		MagellanData.MagellanValues.ValueAX = 0;
		MagellanData.MagellanValues.ValueBY = 0;
		MagellanData.MagellanValues.ValueCZ = 0;
		lParam = MagellanData.MagellanDWord;

		PostMessage(MagellanHandle->MagellanMotionEvent,
					   wParam, lParam);
	}
	if (m_DevHdl!=NULL)
		DConnexion = 1;
	else
		DConnexion = 0;
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
BOOL CMainFrame::PreTranslateMessage(MSG* msg)
{
	HWND hWnd = (HWND)*this; 
	

	UINT message = msg->message;
	if (message == WM_COMMAND)
	{
/*
.....check which menu is pushed
*/
		int indx = -1;
		for (int i=0; i<UDM_MAX_MENU; i++)
		{
			if (NCL_MainFrame->NCL_menubar[i]==NULL)
				continue;
			if (msg->hwnd== NCL_MainFrame->NCL_menubar[i]->m_hWnd)
			{
				NCL_MainFrame->NCL_menubar[i]->m_changed = 1;
				break;
			}
		}
	}
	if (((UD_pickmode==1)||(UU_SM_capture))&&(message==ID_NCL_SMEVENT))
	{
		NCL_Main_View->PostMessage(ID_NCL_SMEVENT, msg->wParam, msg->lParam);
		return TRUE;
	}

	if (UZ_ncl_accelnum==0)
		return CFrameWnd::PreTranslateMessage( msg );
	if (TranslateAccelerator(hWnd, m_accel, msg))
	{
		if (UD_pickmode==1)
		{
/*
......do not post this message again since the event handle routine is changed
......and in the input mode, all messges already handled, otherwise, it will get into a loop
......by post the same message again and again
*/
//			NCL_Main_View->PostMessage(WM_KEYDOWN, (WPARAM)msg->wParam, (LPARAM)(msg->lParam));
			;
		}
		return TRUE;
	}
	else
	{
		NCL_Main_View->PreTranslateMessage( msg );
		return CFrameWnd::PreTranslateMessage( msg );
	}
}

/***********************************************************************
**
**   FUNCTION: NCLPreTranslateMessage(MSG* pMsg)
**		Doing same as PreTranslateMessage, but PreTranslateMessage is a
**			protest member function, so the other class and functions can't call
**			so set this function to set other class and function to access
**
**
**   INPUT:  pMsg: message to be pretranslate
**
**   OUTPUT :   pMsg
**   RETURN:    None
**
**********************************************************************/
BOOL CMainFrame::NCLPreTranslateMessage(MSG* pMsg)
{
	return PreTranslateMessage(pMsg);
}

/***********************************************************************
**
**   FUNCTION: NCLPreTranslateMessage2(MSG* pMsg)
**		Doing handle before TranslateMessage for NCL
**
**   INPUT:  pMsg: message to be pretranslate
**
**   OUTPUT :   pMsg
**   RETURN:    None
**
**********************************************************************/
BOOL CMainFrame::NCLPreTranslateMessage2(MSG* pMsg)
{
	HWND hWnd = (HWND)*this; 
	UINT message = pMsg->message;	
/*
.....somehow the user defined MOUSEMOVE function is not called, so
.....we will added here, also we need catch mousemove outside the frame.
*/
	CRect rect;
	GetClientRect(&rect);

	CPoint point = pMsg->pt;
	ScreenToClient(&point);

	if ((point.x<0)||(point.y<0)||(point.x>rect.right)||(point.y>rect.bottom))
	{
//		m_bStatusBar.OnItemMouseMove(pMsg->wParam, pMsg->pt);
	}
	if (message == WM_MOUSEMOVE)
	{
//		m_bStatusBar.OnItemMouseMove(pMsg->wParam, pMsg->pt);
	}
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: PreCreateWindow(CREATESTRUCT& cs)
**
**       Called by the framework before the creation 
**		of the Windows window attached to this CWnd object.
**		Override this member function to modify the 
**		CREATESTRUCT structure before the window is created. 
**
**		Never call this function directly.
**
**
**   INPUT:  CREATESTRUCT& cs
**
**   OUTPUT :   CREATESTRUCT& cs
**   RETURN:    None
**
**********************************************************************/
BOOL CMainFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	CNclipvApp *app = (CNclipvApp *)AfxGetApp();
	cs.lpszClass = (const char*) (app->m_strMyClassName);

	cs.cx = 500;
	cs.cy = 500;
	cs.x = 100;
	cs.y = 0;

	return CFrameWnd::PreCreateWindow(cs);
}
/*
.....junk routine for doing nothing
*/
void CMainFrame::OnKeyNoop()
{
}

/**********************************************************************
**    I_FUNCTION :  OnDragMenu(UINT id)
**			
**		Callback function for all menu draging bar
**
**    PARAMETERS   
**       INPUT  : 
**          UINT id: menu item ID
**
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::OnDragMenu(UINT id)
{
	int num = id - (WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM);
	NCL_popmenu = id - (WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM);
/*
.....create ad display a floating menu according
.....to popup menu NCL_popmenu 
*/
	uw_display_dragmenu(NCL_popmenu);
}
/**********************************************************************
**    I_FUNCTION :  OnCommandLine()
**			
**		Called command line text changed 
**
**    PARAMETERS   
**       INPUT  : 
**          none
**
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::OnCommandLine()
{
	int pos;

	return;
	CControlBar* temp = (CControlBar *)GetControlBar(IDD_COMMANDBAR);
	if (temp==NULL)
		return;
	CEdit *cwin = (CEdit*)(temp->GetDlgItem(IDC_ECOMMAND));
	 
	if (cwin==NULL)
		return;

	CString sText;
	cwin->GetSel(pos, pos);
	if (pos<=0)
		return; 
	cwin->GetWindowText(sText);
/*
.....when user hit return
.....The current position is at '\r'
.....after '\n" 
*/
	if (sText[pos-1]=='\n')
	{
		NT_FuncEvent = '\015';
		NT_FuncEof = 1;
	}
}

/////////////////////////////////////////////////////////////////////////////
// CMainFrame diagnostics

#ifdef _DEBUG
void CMainFrame::AssertValid() const
{
	CFrameWnd::AssertValid();
}

void CMainFrame::Dump(CDumpContext& dc) const
{
	CFrameWnd::Dump(dc);
}

#endif //_DEBUG

/**********************************************************************
**    I_FUNCTION :  OnNotify( WPARAM wParam, LPARAM lParam, LRESULT* pResult )
**       OnNotify processes the message map for control notification.
**
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**			
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
BOOL CMainFrame::OnNotify( WPARAM wParam, LPARAM lParam, LRESULT* pResult )
{
	int i;
	int idCtrl = (int) wParam; 
	NMHDR *lpnm = ((LPNMHDR)lParam);
	NMTOOLBAR *lpnmTB = (LPNMTOOLBAR)lParam;

	switch(lpnm->code)
	{
		case TBN_DROPDOWN:
		{
/*
.....first search for which CNCLToolbar object is this
*/
			int indx = -1;
			for (i=0; i<UDM_MAX_MENU; i++)
			{
				if (NCL_MainFrame->NCL_menubar[i]==NULL)
					continue;
				if ((lpnmTB->hdr).hwndFrom == NCL_MainFrame->NCL_menubar[i]->m_hWnd)
				{
					indx = i;
					break;
				}
			}
/*
.....Then search for corresponding button
*/
			if (indx==-1)
				break;
			int nCount = NCL_MainFrame->NCL_menubar[indx]->GetToolBarCtrl().GetButtonCount();
			UINT nID, nStyle;
			int iImage;
			int bindx = -1;
			for (i=0; i<nCount; i++)
			{
				NCL_MainFrame->NCL_menubar[indx]->GetButtonInfo(i, nID, nStyle, iImage);
				if (nID==(UINT)(lpnmTB->iItem))
				{
					bindx = i;
					break;
				}
			}
			if (bindx==-1)
				break;
			int num = NCL_MainFrame->NCL_menubar[indx]->GetDropId(bindx);
/*
.....it will display a menu
*/
			uw_ntmenu(num,1);
			MSG msg;
 			while (PeekMessage(&msg, NULL, WM_COMMAND, WM_COMMAND, PM_NOREMOVE))
			{
				UINT wID = LOWORD(msg.wParam);
				nCount = NCL_menu[num]->GetMenuItemCount();
				for (i=0; i<nCount; i++)
				{
					if (wID == NCL_menu[num]->m_id[i])
						break;
				}
				if (i>=nCount)
					break;
				if (i!=UDM_menu[indx].menus[bindx].chcdef)
					NCL_MainFrame->NCL_menubar[indx]->m_defchg = 1;
				else
					NCL_MainFrame->NCL_menubar[indx]->m_defchg = 0;
				UDM_menu[indx].menus[bindx].chcdef = i;
/*
.....set the new label and new function
*/
				CRect rect;
				NCL_MainFrame->NCL_menubar[indx]->GetButtonInfo(bindx, nID, nStyle, iImage);
/*
.....update imagelist of NCL_menubar[indx]
*/
				int no_image = 0;
//				if (UW_menu_fmt!=1)
				{
/*
.....first get bitmap from Popup choice menu item
*/
					CBitmap *rpImage;
					CBitmap cBmap;
					int rpImgnum;
					HDC thdc;
					HBITMAP hbmp;
					if (UDM_menu[num].menus[i].bmpfile[0]!='\0')
					{
						thdc = NCL_MainFrame->GetDC()->m_hDC;
						hbmp = uw_get_bitmap(UDM_menu[num].menus[i].bmpfile, thdc,-1);
						rpImage = cBmap.FromHandle(hbmp);
/*
......copy this bitmap filename dropdown menu item
*/
						strcpy(UDM_menu[indx].menus[bindx].bmpfile, UDM_menu[num].menus[i].bmpfile);
					}
					else
					{
						rpImage = NULL;
						UDM_menu[indx].menus[bindx].bmpfile[0] = '\0';
						no_image = 1;
					}
/*
.....Than replace menubar item bitmap with the Popup menu item bitmap
*/
					CToolBarCtrl &rctrl = (NCL_MainFrame->NCL_menubar[indx])->GetToolBarCtrl();
					CImageList *rimglist = rctrl.GetImageList();
					if (rimglist!=NULL)
					{
						if ((iImage==-1)&&(rpImage!=NULL))
/*
.....add new image into image list and set the new image number
*/
						{
							rpImgnum = rimglist->Add(rpImage, 0xC0C0C0);
							if (rpImgnum==-1)
							{
								no_image = 1;
								NCL_MainFrame->NCL_menubar[indx]->SetButtonInfo(bindx, wID, nStyle, -1);
							}
							else
								NCL_MainFrame->NCL_menubar[indx]->SetButtonInfo(bindx, wID, nStyle, 
											rpImgnum + UDM_menu[num].menus[i].bmpnum);
						}
						else if ((iImage!=-1)&&(rpImage!=NULL))
						{
/*
......"replace" is not working
......so use "add", "copy" and "remove" for "replace"
*/
							rpImgnum = rimglist->Add(rpImage, 0xC0C0C0);
							if (rpImgnum==-1)
							{
								no_image = 1;
								NCL_MainFrame->NCL_menubar[indx]->SetButtonInfo(bindx, wID, nStyle, -1);
							}
							else
							{
								rimglist->Copy(iImage, rpImgnum + UDM_menu[num].menus[i].bmpnum);
								for (int k=0; k<=UDM_menu[num].menus[i].bmpnum; k++)
									rimglist->Remove(rpImgnum+k);
								NCL_MainFrame->NCL_menubar[indx]->SetButtonInfo(bindx, wID, nStyle, iImage);
							}
						}
						else if ((iImage!=-1)&&(rpImage==NULL))
						{
							no_image = 1;
							NCL_MainFrame->NCL_menubar[indx]->SetButtonInfo(bindx, wID, nStyle, -1);
						}
						else
						{
							no_image = 1;
							NCL_MainFrame->NCL_menubar[indx]->SetButtonInfo(bindx, wID, nStyle, iImage);
						}
					}
					else if ((rimglist==NULL)&&(rpImage!=NULL))
					{
/*
.....create and set image list
*/
						rimglist = new CImageList();
						if (UW_icon_size==0)
							rimglist->Create(16,16, ILC_COLOR4|ILC_MASK, 0, 1000);
						else if (UW_icon_size==1)
							rimglist->Create(24,24, ILC_COLOR4|ILC_MASK, 0, 1000);
						else if (UW_icon_size==2)
							rimglist->Create(32,32, ILC_COLOR4|ILC_MASK, 0, 1000);
						else
							rimglist->Create(16,16, ILC_COLOR4|ILC_MASK, 0, 1000);
						rpImgnum = rimglist->Add(rpImage, 0xC0C0C0);
						rctrl.SetImageList(rimglist);
						NCL_MainFrame->NCL_menubar[indx]->SetButtonInfo(bindx, wID, nStyle, 
								rpImgnum + UDM_menu[num].menus[i].bmpnum);
					}
					UDM_menu[indx].menus[bindx].bmpnum = UDM_menu[num].menus[i].bmpnum;
				}
//				if ((UW_menu_fmt!=0)
//					||((UW_menu_fmt==0)&&(no_image==1)))
				{
					char label[80];
					strcpy(label, UDM_menu[num].menus[i].name);
					NCL_MainFrame->NCL_menubar[indx]->SetButtonText(bindx, label);
					if ((UW_menu_fmt==1)||(no_image==1))
						NCL_MainFrame->NCL_menubar[indx]->SetButtonInfo(bindx, wID, nStyle, -1);
				}
//				else if (UW_menu_fmt==0)
//				{
//					NCL_MainFrame->NCL_menubar[indx]->RemoveButtonText(bindx);
//				}
				RecalcLayout();
				break;
			}
			*pResult = TBDDRET_DEFAULT ;
			return FALSE;
		}
	}

	return CFrameWnd::OnNotify(wParam, lParam, pResult);
}

/**********************************************************************
**    I_FUNCTION :  OnDynamicTipText(UINT id, 
**			NMHDR* pNMHDR, LRESULT* pResult)
**		Called when tooltip need be displayed
**
**    PARAMETERS   
**       INPUT  : 
**          UINT id: not used
**			pNMHDR: NMHDR structure contain tooltip
**						message information
**
**       OUTPUT :  
**          pResult: always 0
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
BOOL CMainFrame::OnDynamicTipText(UINT id, NMHDR* pNMHDR, LRESULT* pResult)
{
	int num, menunum;
	ASSERT(pNMHDR->code == TTN_NEEDTEXTA || pNMHDR->code == TTN_NEEDTEXTW);

	TOOLTIPTEXTA* pTTTA = (TOOLTIPTEXTA*)pNMHDR;
	TOOLTIPTEXTW* pTTTW = (TOOLTIPTEXTW*)pNMHDR;
	TCHAR szFullText[256];
	CString strTipText; 

	UINT nID = pNMHDR->idFrom;
	if (pNMHDR->code == TTN_NEEDTEXTA && (pTTTA->uFlags & TTF_IDISHWND) ||
		pNMHDR->code == TTN_NEEDTEXTW && (pTTTW->uFlags & TTF_IDISHWND))
	{
		// idFrom is actually the HWND of the tool
		nID = ((UINT)(WORD)::GetDlgCtrlID((HWND)nID));
	}

	if ((nID >= WM_APP)&&(nID<WM_APP+UDM_MAX_MENU)) 
	{
		UX_pathname filename,dir,fname1;
		char fname[UX_MAX_FILE_LEN];
		num = nID-WM_APP;
		uz_ntcnvtnum_to_menu(num, &menunum);
		strcpy(filename, UDM_menu[menunum].file);
		ul_break_fname(filename,dir,fname);
		sprintf(fname1, "Load %s", fname);
		strTipText = fname1;
	}
	else if ((nID >= WM_APP+UDM_MAX_MENU)&&(nID<WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM)) 
	{
		int num, status;
		char descript[256];
		num = nID - (WM_APP+UDM_MAX_MENU);
		status = uz_ntget_dspt(num, descript, 1);
		if (status==-1)
			strcpy(descript, "No description");
		strTipText = descript;
	}
	else if ((nID >= WM_APP+UDM_MAX_MENU + UZ_MAX_KEYITEM)&&(nID<WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM)) 
	{
		int num, status;
		char descript[256];
		num = nID - (WM_APP+UDM_MAX_MENU+ UZ_MAX_KEYITEM);
		status = uz_ntget_dspt(num, descript, 2);
		if (status==-1)
			strcpy(descript, "No description");
		strTipText = descript;
	}
	else
	{
		AfxLoadString(nID, szFullText);
		AfxExtractSubString(strTipText, szFullText, 1, '\n');
	}

	if (pNMHDR->code == TTN_NEEDTEXTA)
		lstrcpyn(pTTTA->szText, strTipText, _countof(pTTTA->szText));
	else
		_mbstowcsz(pTTTW->szText, strTipText, _countof(pTTTW->szText));

	*pResult = 0;

	::SetWindowPos(pNMHDR->hwndFrom, HWND_TOP, 0, 0, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOMOVE);

	return TRUE;   
}

/**********************************************************************
**    I_FUNCTION :  OnIPVFunctions(UINT id)
**		Callback function for all menu functions
**
**    PARAMETERS   
**       INPUT  : 
**          UINT id: menu item ID
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::OnIPVFunctions(UINT id)
{
/*
	int savcmd_enable, sav_cmdcur, save_ptype;
	int* save_inptr;
	char sav_cmdstr[256];
	int jmpflag, sav_cur, sav_mode, ptype_saved;
	int num = id - (WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM);

	SetTimer(1, 1000*10, NULL);

	sav_cur = uw_ntgetcur_cursor();
	sav_mode = UD_pickmode;
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag == 0)
	{
		uw_ntsetcursor(21);
		uz_wnt_callfunc(num,2);
		glFlush_d();
	}
	uw_ntsetcursor(sav_cur);
	UD_pickmode = sav_mode;
	KillTimer(1);
	UD_UNMARK(jmpflag);
*/
/*
.....when in input mode, if we execute the function here within another executed function,
.....the longjump function will have a problem because the object will break the order of the
.....setjmp. So we will not execute the function here but set a ID_NCL_FUNCTION message which 
.....the event loop of the "input" will get the message and execute the function there. Then the execute
.....order will not break and the loopjmp will work.

*/
	ncl_reset_pikgeom();
	if (UD_pickmode==1)
	{
		PostMessage(ID_NCL_FUNCTION, (WPARAM)id, (LPARAM)NULL);
		return;
	}
	uw_nt_nclfunc(id, 0);
}
/**********************************************************************
**    I_FUNCTION :  OnAccelFunctions(UINT id)
**		Callback function for all Accelerator functions
**
**    PARAMETERS   
**       INPUT  : 
**          UINT id: Accelerator ID
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::OnAccelFunctions(UINT id)
{
/*
......use local value
*/
	int jmpflag, sav_cur,sav_mode;
	int num = id - (WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM);

	if (UD_pickmode==1)
		return;
	sav_mode = UD_pickmode;
	sav_cur = uw_ntgetcur_cursor();
/*
.....set timer for every second
*/
	SetTimer(1, 1000*10, NULL);
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag == 0)
	{
		int num = id - (WM_APP+UDM_MAX_MENU);
		uw_ntsetcursor(21);
		uz_wnt_callfunc(num, 1);
	}
	uw_ntsetcursor(sav_cur);
	UD_pickmode = sav_mode;
	KillTimer(1);
	UD_UNMARK(jmpflag);
}
/**********************************************************************
**    I_FUNCTION :  OnPupupChoices(UINT id)
**		Callback function for popup choice menu 
**			item
**
**    PARAMETERS   
**       INPUT  : 
**          UINT id: menu item ID
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::OnPupupChoices(UINT id)
{
	PopupChoice[UW_NPopup] = id - (WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+ NBUTTONNUM + UDM_MAX_MENU);
}
/**********************************************************************
**    I_FUNCTION :  NCLFindPopupMenuFromID(CMenu* pMenu, UINT nID, int flag)
**       find the Popup menu from ID 
**    PARAMETERS   
**       INPUT  : 
**          pMenu: Menu from which to find popup menu
**			nID: Id to be matched
**			flag: 0: don't check itself
**					1: check itself
**       OUTPUT :  
**          none
**    RETURNS      : Popup Menu find
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static CMenu* NCLFindPopupMenuFromID(CMenu* pMenu, UINT nID, int flag)
{
	ASSERT_VALID(pMenu);
	UINT nItems = pMenu->GetMenuItemCount();
	for (int iItem = 0; iItem < (int)nItems; iItem++)
	{
		CMenu* pPopup = pMenu->GetSubMenu(iItem);
		if (pPopup != NULL)
		{
			if (flag==1)
			{
/*
.....compare itself first
*/
				if(pPopup->IsKindOf(RUNTIME_CLASS(CNCLMenu)))
				{
/*
.....the first item always be draging bar
*/
					if ((iItem>=1)&&(((CNCLMenu*)pMenu)->m_id[iItem-1] == nID))
					{
						pMenu = CMenu::FromHandlePermanent(pMenu->m_hMenu);
						return pMenu;
					}
				}
			}
			pPopup = NCLFindPopupMenuFromID(pPopup, nID, flag);
			if (pPopup != NULL)
				return pPopup;
		}
		else if (pMenu->GetMenuItemID(iItem) == nID)
		{
			// it is a normal item inside our popup
			pMenu = CMenu::FromHandlePermanent(pMenu->m_hMenu);
			return pMenu;
		}
	}
	// not found
	return NULL;
}


/**********************************************************************
**    I_FUNCTION :  OnMeasureItem(int , LPMEASUREITEMSTRUCT lpMeasureItemStruct)
**       The framework calls this member function by the framework 
**		for the owner of an owner-draw menu item when the control is created. 
**
**    PARAMETERS   
**       INPUT  : 
**          lpMeasureItemStruct:Points to a MEASUREITEMSTRUCT data structure that contains 
**					the dimensions of the owner-draw control.
**
**       OUTPUT :  
**          none
**    RETURNS      : Popup Menu find
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::OnMeasureItem(int /*nIDCtl*/, LPMEASUREITEMSTRUCT lpMeasureItemStruct)
{
	if (lpMeasureItemStruct->CtlType == ODT_MENU)
	{
		ASSERT(lpMeasureItemStruct->CtlID == 0);
		CMenu* pMenu;
		CMenu* pMenu2;

		if (UW_TrackingWindow == m_hWnd)
		{
			// start from popup
			pMenu = CMenu::FromHandle(UW_TrackingMenu);
		}
		else
		{
			// start from menubar
			pMenu = GetMenu();
		}

		pMenu2 = NCLFindPopupMenuFromID(pMenu, lpMeasureItemStruct->itemID, 0);
		if (pMenu2 != NULL)
			pMenu2->MeasureItem(lpMeasureItemStruct);
		else
		{
			pMenu2 = NCLFindPopupMenuFromID(pMenu, lpMeasureItemStruct->itemID, 1);
			if (pMenu2 != NULL)
				pMenu2->MeasureItem(lpMeasureItemStruct);
			else
			{
				TRACE1("Warning: unknown WM_MEASUREITEM for menu item 0x%04X.\n",
					lpMeasureItemStruct->itemID);
			}
		}
		return;
	}
	else
	{
		CWnd* pChild = GetDescendantWindow(lpMeasureItemStruct->CtlID, TRUE);
		if (pChild != NULL && pChild->SendChildNotifyLastMsg())
			return;     // eaten by child
	}
	// not handled - do default
	Default();
}
/***********************************************************************
**
**   FUNCTION: OnDestroy() 
**
**		OnDestroy is called after the CMainFrame object 
**		is removed from the screen. Free the memory here
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CMainFrame::OnDestroy()
{    
	if ( MagellanHandle != NULL )
	{
		MagellanClose( MagellanHandle );
		MagellanHandle = NULL;    
	}
	int i,status;
	if (UW_topmenu!=NULL)
	{
		for (i=0; i<20; i++)
		{
			status = UW_topmenu->DeleteMenu(0, MF_BYPOSITION);
			if (status==0)
				break;
		}
		UW_topmenu = NULL;
	}
	for (i=0; i<200; i++)
	{
		if (NCL_menubar[i] != NULL)
		{
			CImageList *rimglist = NULL;
			CToolBarCtrl &rctrl = NCL_menubar[i]->GetToolBarCtrl();
			rimglist = rctrl.GetImageList();
			if (rimglist!=NULL)
			{
				delete rimglist;
				rimglist = NULL;
			}
			delete NCL_menubar[i];
			NCL_menubar[i] = NULL;
		}
		if (NCL_menu[i] != NULL)
		{
			if (!(::IsMenu(NCL_menu[i]->m_hMenu)))
				continue;
			delete NCL_menu[i];
			NCL_menu[i] = NULL;
		}
	}	
	NCL_MainFrame = NULL;
	delete UW_topmenu;
	if (m_promptBar!=NULL)
		delete m_promptBar;
	if (m_errorBar!=NULL)
		delete m_errorBar;

	if (UZ_ncl_accel!=NULL)
		free ((char*)UZ_ncl_accel);
	CFrameWnd::OnDestroy();
}



/**********************************************************************
**    I_FUNCTION :  WritePLabel(char *label)
**		write a label to command line prompt
**
**    PARAMETERS   
**       INPUT  : 
**          label: label to write
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::WritePLabel(char *label)
{
	if ((m_commandBar==NULL) || (m_commandBar->m_created!=1))
		return;
	m_commandBar->SetDlgItemText(IDC_PCOMMAND, label);
}

/**********************************************************************
**    I_FUNCTION :  WriteLabel(char *label)
**		write a label to promptBar
**
**    PARAMETERS   
**       INPUT  : 
**          label: label to write
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::WriteLabel(char *label)
{
	if ((m_promptBar==NULL) || (m_promptBar->m_created!=1))
		return;
	m_promptBar->SetDlgItemText(IDC_LPROMPT, label);
}

/**********************************************************************
**    I_FUNCTION :  WriteError(char *label)
**		write a label to errorBar
**
**    PARAMETERS   
**       INPUT  : 
**          label: error to write
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::WriteError(char *label)
{
	if ((m_errorBar==NULL) || (m_errorBar->m_created!=1))
		return;
	m_errorBar->SetDlgItemText(IDC_LERROR, label);
}

/**********************************************************************
**    I_FUNCTION :  SetCommand_focus(int focus)
**		Set the command line focus
**
**    PARAMETERS   
**       INPUT  : 
**          focus: 1: set focus to command line
**					0: remove focus to command line
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::SetCommand_focus(int focus)
{
	if ((m_commandBar==NULL) || (m_commandBar->m_created!=1) || (m_commandBar->m_visible!=1))
		return;
	CWnd *cwin = m_commandBar->GetDlgItem(IDC_ECOMMAND);
	if (cwin==NULL) return;
	if (focus)
		cwin->SetFocus();
	else
		this->SetFocus();
}

/**********************************************************************
**    I_FUNCTION :  SetCommand_Str(CString cmdstr, int &line_num)
**		Set the command line text
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          cmdstr:
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::SetCommand_Str(CString cmdstr, int line_num, int upload)
{
	if ((m_commandBar==NULL) || (m_commandBar->m_created!=1))
	{
		return;
	}
	CString label, text;
	int line,load;
	m_commandBar->getcmdtext(label, text, line, load);
	m_commandBar->setcmdtext(label, cmdstr, line_num, upload);
}
/**********************************************************************
**    I_FUNCTION :  InsertCommand_Str(char *cmdstr)
**		Set the command line text
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          cmdstr:
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::InsertCommand_Str(CString msg)
{
	if ((m_commandBar==NULL) || (m_commandBar->m_created!=1))
	{
		return;
	}
	m_commandBar->insert_cmdstr(msg);
}

/**********************************************************************
**    I_FUNCTION :  SetCommand_Insertpos(cmdcur1, cmdcur2)
**		Set the command line insert position
**
**    PARAMETERS   
**       INPUT  : 
**          cmdcur1, cmdcur2: selected position to be set
**       OUTPUT :  
**          none:
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::SetCommand_Insertpos(int cmdcur1, int cmdcur2)
{
	if ((m_commandBar==NULL) || (m_commandBar->m_created!=1))
	{
		return;
	}
	m_commandBar->setcmdsel(cmdcur1, cmdcur2);
}
/**********************************************************************
**    I_FUNCTION :  GetCommand_Str(char *cmdstr)
**		Get the command line text
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          cmdstr:
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::GetCommand_Str(CString &cmdstr, int &line_num, int &upload)
{
	cmdstr = "";
	if ((m_commandBar==NULL) || (m_commandBar->m_created!=1))
	{
		return;
	}
	CString label;
	m_commandBar->getcmdtext(label, cmdstr, line_num, upload);
}
/**********************************************************************
**    I_FUNCTION : Enable_cmdbar(int flag)
**			enable/disable command bar
**			
**    PARAMETERS   
**       INPUT  : 
**				flag: 1: enable command line
**					  0: disable command line
**				
**       OUTPUT :  
**         			none 
**    RETURNS      : None
**					 
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::Enable_cmdbar(int flag)
{
	if (m_commandBar==NULL) return;
	if (m_commandBar->m_visible!=1)
		return;
	if (m_commandBar->m_enabled == flag)
		return;
	CEdit *cwin = (CEdit*)(m_commandBar->GetDlgItem(IDC_ECOMMAND));
	if (flag)
	{
		cwin->EnableWindow(TRUE);
	}
	else
	{
		cwin->EnableWindow(FALSE);
	}
	m_commandBar->m_enabled = flag;
}
/**********************************************************************
**    I_FUNCTION : Get_CmdEnable()
**			Get command bar enable/disable status
**			
**    PARAMETERS   
**       INPUT  : 
**					none
**       OUTPUT :  
**         			none 
**    RETURNS      : 1: command bar enable
**					0: command bar disable
**					 
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
int CMainFrame::Get_CmdEnable()
{
	if (m_commandBar==NULL) return 0;
	if (m_commandBar->m_visible!=1)
		return 0;
	return m_commandBar->m_enabled;
}
/**********************************************************************
**    I_FUNCTION :  SetCommand_Select(int start, int end)
**		Select the command line text
**
**    PARAMETERS   
**       INPUT  : 
**          start: start position
**			end:	end position
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::SetCommand_Select(int start, int end)
{
	CEdit *cwin = (CEdit *)(m_commandBar->GetDlgItem(IDC_ECOMMAND));
	if (cwin!=NULL)
		cwin->SetSel(start, end);
}

/**********************************************************************
**    I_FUNCTION :  WriteStat(int sub, char *msg)
**       Write a message to a field in the Status Area.
**    PARAMETERS   
**       INPUT  : 
**          sub   = Field sub number to write to.
**			msg     = Message to output.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::WriteStat(int sub, char *msg)
{
	int i,j,k,msg_len, num;
	UINT id;
	UINT nID, nStyle;
	char newmsg[256];
	int iImage;
	int bindx = -1;

	num = 0;
	while (num!=MAX_STAT_FUNC)
	{
		id = UZ_status_table[sub][num];
		if ((id==-1)||(id==IDF_KEY_NOOP))
			return;
		for (i=0;i<UDM_menu_count;i++) 
		{
			if (UDM_menu[i].statflag==0)
				continue;
			if (NCL_MainFrame->NCL_menubar[i]==NULL)
				continue;
			for (j=0; j<UDM_menu[i].num; j++)
			{
				NCL_MainFrame->NCL_menubar[i]->GetButtonInfo(j, nID, nStyle, iImage);
				if (nID==id)
				{
					strcpy(newmsg, UDM_menu[i].menus[j].name);
					strcat(newmsg, msg);
/*
.....remove trailing spaces
*/
					msg_len = strlen(newmsg);
					for (k = (msg_len-1); k > 0; k--)
					{
						if (newmsg[k] != ' ')
						{
							newmsg[k+1] = '\0';
							break;
						}
					}
					NCL_MainFrame->NCL_menubar[i]->SetButtonText(j, newmsg);
/*
......use "continue" if allow same status name in the one status bar 
*/					
					break;  
				}
			}
		}
		num++;
	}
}
/**********************************************************************
**    I_FUNCTION :  WindowProc(UINT message, WPARAM wParam, LPARAM lParam)
**		Provides a Windows procedure (WindowProc) for a CWnd object.
**		 It dispatches messages through the window's message map.       
**
**    PARAMETERS   
**       INPUT  : message: window message to be processed.
**				wParam:   Provides additional information used in
**						 processing the message
**				lParam:   Provides additional information used in 
**							processing the message. 
**
**       OUTPUT :  
**          none
**    RETURNS      : The return value depends on the message.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
LRESULT CMainFrame::WindowProc(UINT message, WPARAM wParam, LPARAM lParam) 
{
	MSG MagellanMessage;
	MagellanIntegerEvent MagellanData;
	HWND hWnd;
	int data[6], event;
	char *indx, buf[20];

	indx = buf;

	if ( MagellanHandle != NULL )
	{
		hWnd = GetSafeHwnd();
		MagellanMessage.hwnd = hWnd;
		MagellanMessage.message = message;
		MagellanMessage.wParam = wParam;
		MagellanMessage.lParam = lParam;
					
		switch( MagellanTranslateEvent( MagellanHandle, &MagellanMessage, &MagellanData, &event ) )
		{   
			case MotionEvent :
				if ((UD_pickmode==1)||(UU_SM_capture))
				{
					NCL_Main_View->SendMessage(message, wParam, lParam);
					return CFrameWnd::WindowProc(message, wParam, lParam);
				}
				data[0] = MagellanData.MagellanData[0];
				data[1] = MagellanData.MagellanData[1];
				data[2] = MagellanData.MagellanData[2];
				data[3] = MagellanData.MagellanData[3];
				data[4] = MagellanData.MagellanData[4];
				data[5] = MagellanData.MagellanData[5];
				uz_user_smouse(event, data, &indx,1);
				break;
      
			case ButtonPressEvent: 
				if ((UD_pickmode==1)||(UU_SM_capture))
				{
					NCL_Main_View->PostMessage(message, wParam, lParam);
					return CFrameWnd::WindowProc(message, wParam, lParam);
				}
				event = MagellanData.MagellanButton;
				data[0] = MagellanData.MagellanData[0];
				data[1] = MagellanData.MagellanData[1];
				data[2] = MagellanData.MagellanData[2];
				data[3] = MagellanData.MagellanData[3];
				data[4] = MagellanData.MagellanData[4];
				data[5] = MagellanData.MagellanData[5];
				uz_user_smouse(event, data, &indx,1);
				break;
       
			case ButtonReleaseEvent:
				if ((UD_pickmode==1)||(UU_SM_capture))
				{
					NCL_Main_View->PostMessage(message, wParam, lParam);
					return CFrameWnd::WindowProc(message, wParam, lParam);
				}
				break;

			default :
				if ((message==WM_CLOSE)&&(UD_pickmode==1))
				{
					OnClose();
					return 0;
				}
				return CFrameWnd::WindowProc(message, wParam, lParam);
		};
	};

	if ((message==WM_CLOSE)&&(UD_pickmode==1))
	{
		OnClose();
		return 0;
	}
	return CFrameWnd::WindowProc(message, wParam, lParam);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntget_dockrect(CRect *rect, int flag)
**       Get the dockbar position and size
**    PARAMETERS   
**       INPUT  : 
**			flag: direction of the dockbar
**					1: Top		2: Bottom
**					3: Left		4: Right
**       OUTPUT :  
**			rect: position and size of dockbar
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_ntget_dockrect(CRect *rect, int flag)
{
	CControlBar* pDockBar = NULL;
	if (flag==1)
	{
		pDockBar = (CControlBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_TOP);
	}
	else if (flag==2)
	{
		pDockBar = (CControlBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_BOTTOM);
	}
	else if (flag==3)
	{
		pDockBar = (CControlBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_LEFT);
	}
	else if (flag==4)
	{
		pDockBar = (CControlBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_RIGHT);
	}
	if (pDockBar!=NULL)
		pDockBar->GetWindowRect(rect);
}

/**********************************************************************
**    I_FUNCTION :  nclipv_exit
**       Safe exit NCLIPV
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void nclipv_exit()
{
	CNclipvApp *app = (CNclipvApp*)AfxGetApp();
	app->OnIPVSafeExit();
}
/**********************************************************************
**    I_FUNCTION :  uw_ntflush_spmouse
**			Flush Space Mouse
**
**    PARAMETERS   
**       INPUT  : none
**
**       OUTPUT :  
**          none
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntflush_spmouse() 
{
	MSG MagellanMessage;
	while ( ::PeekMessage( &MagellanMessage, NULL, MagellanMotionEventType( MagellanHandle ), 
	                           MagellanMotionEventType( MagellanHandle ), PM_REMOVE ) );	
	return 0 ;
}
/***********************************************************************
**
**   FUNCTION: OnTimer() 
**
**		this function will timely check if the main NCL process is running
**		if it is not running, we need the end the sub-NCL process.
**		for sub-NCL process only
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CMainFrame::OnTimer(UINT_PTR nIDEvent) 
{
	if (nIDEvent!=1)
		return CFrameWnd::OnTimer(nIDEvent);
	Ontimer2();
	CFrameWnd::OnTimer(nIDEvent);
	return;
}
void CMainFrame::Ontimer2()
{
	static int start = 0;
	if (start==1)
		return;
	start = 1;
	MSG msg;
	BOOL bIdle = TRUE;
	LONG lIdleCount = 0;
	CNclipvApp *app = (CNclipvApp*)AfxGetApp();
	int exec = 0;
	while (exec==0)
	{
		while (bIdle &&
			!::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE))
		{
			if (!app->OnIdle(lIdleCount++))
			{
				bIdle = FALSE; 
				exec = 1;
			}
		}
		do
		{
			if (msg.message==WM_QUIT)
				break;
			if (msg.message != WM_KICKIDLE && !NCL_MainFrame->NCLPreTranslateMessage(&msg))
			{
				::TranslateMessage(&msg);
				::DispatchMessage(&msg);
/*
.....don't forget this message after dispatch it (GetMessage will remove it from the queue
.....PeekMessage is depend on remove flag
*/
				::PeekMessage(&msg, msg.hwnd, msg.message, msg.message, PM_REMOVE);
			}
/*
......pump message, but quit on WM_QUIT
*/
//		if (!app->PumpMessage())
//		{
//			break;
//		}
			if (app->IsIdleMessage(&msg))
			{
				bIdle = TRUE;
				lIdleCount = 0;
			}
		} while (::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE));
	}
	start = 0;
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
void CMainFrame::OnLButtonUp(UINT nFlags, CPoint point) 
{
	CFrameWnd::OnLButtonUp(nFlags, point);
}

void CMainFrame::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CFrameWnd::OnLButtonDown(nFlags, point);
}

/*
.....junk routine here to match the class NCL used
.....since we need use the NCL ws routine
*/
void CMainFrame::Recreate_AcceleratorTable()
{
}

void CMainFrame::StartNewNCL(char *parm)
{
}

void CMainFrame::remove_subproc(int sub)
{
}
int CMainFrame::GetSubNCL()
{
	return 0;
}

void CMainFrame::OnClose2()
{
}
void CMainFrame::UpdatePOPMenu(CPoint pt, char* input_text)
{
}
/***********************************************************************
c
c   FUNCTION: OnMenuRbuttonup(WPARAM wparm, LPARAM lparm)
c
c           callbacks for right click mouse on menu item 
c
c   INPUT:  wParam:   include the menu item number
c			lParam:   menu handler
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
LRESULT CMainFrame::OnMenuRbuttonup(WPARAM wparm, LPARAM lparm)
{
	int select = (int)wparm;
	HMENU hmenu = (HMENU)lparm;
	int menunum = -1;
	for (int i=0; i<200;i++)
	{
		if (NCL_menu[i]==NULL)
			continue;
		if (hmenu==NCL_menu[i]->m_hMenu)
		{
			menunum = i;
		}
	}
	if (menunum==-1)
		return 0;
	UDM_menu_struc menu_item;
	int status, choice = 0;
/*
......the first item is tear-off bar, use for save the whole menu
*/
	if (select==0)
	{
		SaveMenu(menunum);
		return 0;
	}
	select--;
	if (select>=0)
	{
/*
......display popup menu " Insert Separator, Insert Item, Delete Item, Cancel"
*/
		uw_ntpopchoice(3, &choice);
		if (choice==0)
			return 0;
		status = 0;
		if (choice==1)
		{
/*
......delete item
*/
/*
......updated UDM_menu[m_barnum] with delete one item in menu = menunum1, item = itemnum1
*/
			ud_del_UDMmenu(menunum, select);
			m_addmenu = -1;
			m_remove_menu = menunum;
			m_remove_item = select;
		}
		else if (choice==2)
/*
.....edit select item
*/
		{
			status = uw_ntmenu_desgn(1, &(UDM_menu[menunum].menus[select]));
			if (status!=-1)
			{
/*
......do not automatically save a menu anymore
*/
//				ud_save_dragmenu(menunum);
				m_addmenu = menunum;
				m_additem = select;
				m_remove_menu = menunum;
				m_remove_item = select;
			}
		}
		else if (choice==3)
/*
.....insert new item
*/
		{
			menu_item.toggle_num = 0;
			menu_item.toggle = 0;
			menu_item.chcfile[0] = '\0';
			menu_item.chcdef = 0;
			menu_item.name[0] = '\0';
			menu_item.file[0] = '\0';
			menu_item.statname[0] = '\0';
			menu_item.params = (char *)UU_NULL;
			menu_item.descrip[0] = '\0';
			menu_item.bgcolor[0] = '\0';
			menu_item.color[0] = -1;
			menu_item.color[1] = -1;
			menu_item.color[2] = -1;
			menu_item.pos[0] = -1;
			menu_item.pos[1] = -1;
			menu_item.size[0] = -1;
			menu_item.size[0] = -1;
			menu_item.bmpfile[0] = '\0';
			menu_item.bmpnum = -1;
			menu_item.separator = 0;
			status = uw_ntmenu_desgn(1, &menu_item);
			if (status!=-1)
			{
				ud_insert_UDMmenu(menunum, select, &menu_item);
				m_addmenu = menunum;
				m_additem = select;
				m_remove_menu = -1;
			}
		}
		else if (choice==4)
		{
/*
.....insert separator
*/
			menu_item.toggle_num = 0;
			menu_item.toggle = 0;
			menu_item.chcfile[0] = '\0';
			menu_item.chcdef = 0;
			menu_item.name[0] = '\0';
			menu_item.file[0] = '\0';
			menu_item.statname[0] = '\0';
			menu_item.params = (char*)UU_NULL;
			menu_item.descrip[0] = '\0';
			menu_item.bgcolor[0] = '\0';
			menu_item.color[0] = -1;
			menu_item.color[1] = -1;
			menu_item.color[2] = -1;
			menu_item.pos[0] = -1;
			menu_item.pos[1] = -1;
			menu_item.size[0] = -1;
			menu_item.size[0] = -1;
			menu_item.bmpfile[0] = '\0';
			menu_item.bmpnum = -1;
			menu_item.separator = 1;
			ud_insert_UDMmenu(menunum, select, &menu_item);
			m_addmenu = menunum;
			m_additem = select;
			m_remove_menu = -1;
		}
		if (status!=-1)
			PostMessage(WM_COMMAND, UW_UPDATE_MENUBAR);
	}
	return 0;
}


/***********************************************************************
c
c   FUNCTION: OnMenuDrag(WPARAM wparm, LPARAM lparm)
c
c           callbacks for menu draging
c
c   INPUT:  wParam:   include the menu item number
c			lParam:   menu handler
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
LRESULT CMainFrame::OnMenuDrag(WPARAM wparm, LPARAM lparm)
{
	HMENU hmenu = (HMENU)lparm;
	int dragmenu = -1;
	for (int i=0; i<200;i++)
	{
		if (NCL_menu[i]==NULL)
			continue;
		if (hmenu==NCL_menu[i]->m_hMenu)
		{
			dragmenu = i;
		}
	}
	int dragitem = (int)wparm;
	if (dragmenu==-1)
		return MND_CONTINUE;
/*
......the first item is tear-off bar, ignored
*/
	dragitem--;
	
	char menudata[100];
	COleDataSource*	pSource = new COleDataSource();
	if(pSource)
	{
		CSharedFile	sf(GMEM_MOVEABLE|GMEM_DDESHARE|GMEM_ZEROINIT);
		CString iText;
		sprintf_s(menudata, 100, "CNCLMenu %d, %d", dragmenu, dragitem);
		iText = menudata;

		sf.Write(iText, iText.GetLength());

		HGLOBAL hMem = sf.Detach();
		if (!hMem) 
			return MND_CONTINUE;
		pSource->CacheGlobalData(CF_TEXT, hMem);
		pSource->DoDragDrop();
		delete pSource;
	}
	return MND_CONTINUE;
}

/***********************************************************************
c
c   FUNCTION: OnMenuGetObject(WPARAM wparm, LPARAM lparm)
c
c           handleing menu draging
c
c   INPUT:  wparm:   
c			lparm:   include menu drag information
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
LRESULT CMainFrame::OnMenuGetObject(WPARAM wparm, LPARAM lparm)
{
	MENUGETOBJECTINFO *info = (MENUGETOBJECTINFO*)lparm;

	if (info->hmenu==0)
		return MNGO_NOINTERFACE;
	int dragmenu = -1;
	for (int i=0; i<200;i++)
	{
		if (NCL_menu[i]==NULL)
			continue;
		if (info->hmenu==NCL_menu[i]->m_hMenu)
		{
			dragmenu = i;
		}
	}
	int dragitem = info->uPos;
	if (dragmenu>=0)
	{
		if (m_TargetDrop!=NULL)
		{
			m_TargetDrop->QueryInterface(*(IID*)info->riid, &(info->pvObj));
			return MNGO_NOERROR;
		}
	}

	return MNGO_NOINTERFACE;
}

/***********************************************************************
c
c   FUNCTION: redisp_popmenu(int menu)
c
c           redisplay the popup menu
c
c   INPUT:  menu: popup menu number
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::redisp_popmenu(int menu)
{
	HBITMAP hchkBmap = NULL;
	CRect rect;

	if (NCL_menu[menu]==NULL)
		return;
	HDC thdc = NCL_MainFrame->GetDC()->m_hDC;
	int savcols;
/*
.....if there is drag_off popup menu, redisplay it first
*/
	int num = NCL_menu[menu]->GetDragIdnum();
	if ((num!=-1)&&(NCL_menubar[num]!=0))
	{
/*
......delete drag_off popup menu first, then redisply it
*/
		if (((NCL_MainFrame->NCL_menubar[num])->m_created==1)&&(NCL_MainFrame->NCL_menubar[num]->m_visible==1))
		{
			NCL_menubar[num]->GetWindowRect(&rect);
			savcols = NCL_menubar[num]->GetCols();			
			NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[num],
							FALSE, FALSE);
			(NCL_MainFrame->NCL_menubar[num])->m_visible = 0;

			CImageList *rimglist = NULL;
			CToolBarCtrl &rctrl = (NCL_MainFrame->NCL_menubar[num])->GetToolBarCtrl();
			rimglist = rctrl.GetImageList();
			if (rimglist!=NULL)
			{
				delete rimglist;
				rimglist = NULL;
			}
			delete (NCL_MainFrame->NCL_menubar[num]);
			NCL_MainFrame->NCL_menubar[num] = NULL;
			uw_redisplay_dragmenu(menu);
/*
......get the right position
*/
			CPoint pt(0,0);
			CRect rect1 (&rect);
			uw_ntadd_menuborder(&rect1);
			pt.Offset(rect1.left, rect1.top);	
			NCL_menubar[num]->SetDefFloatWidth(savcols);
			FloatControlBar(NCL_menubar[num], pt);
		}
	}
	uw_redisplay_popmenu(menu);
	return;

}

/***********************************************************************
c
c   FUNCTION: redisp_menubar(int menu)
c
c           redisplay the toolbar menu
c
c   INPUT:  menu: menu number
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::redisp_menubar(int menu)
{
	int sav_float, stype,savcols;
	RECT rect;

	if (NCL_menubar[menu] != NULL)
	{
/*
......save menubar's position and size
*/
		sav_float = NCL_menubar[menu]->IsFloating();
		if (sav_float==0)
			stype = uw_ntget_ctlbar_type(NCL_menubar[menu]);
		else
			savcols = NCL_menubar[menu]->GetCols();			

		NCL_menubar[menu]->GetWindowRect(&rect);
		CImageList *rimglist = NULL;
		CToolBarCtrl &rctrl = NCL_menubar[menu]->GetToolBarCtrl();
		rimglist = rctrl.GetImageList();
		if (rimglist!=NULL)
		{
			delete rimglist;
			rimglist = NULL;
		}
		delete NCL_menubar[menu];
		NCL_menubar[menu] = NULL;
		uw_ntmenu(menu, 0);
		if (sav_float==0)
		{
			if (stype==1)
				DockControlBar(NCL_menubar[menu], AFX_IDW_DOCKBAR_TOP, &rect);
			else if (stype==2)
				DockControlBar(NCL_menubar[menu], AFX_IDW_DOCKBAR_BOTTOM, &rect);
			else if (stype==3)
				DockControlBar(NCL_menubar[menu], AFX_IDW_DOCKBAR_LEFT, &rect);
			else if (stype==4)
				DockControlBar(NCL_menubar[menu], AFX_IDW_DOCKBAR_RIGHT, &rect);
		}
		else
		{
			CPoint pt(0,0);
			CRect rect1 (&rect);
			uw_ntadd_menuborder(&rect1);
			pt.Offset(rect1.left, rect1.top);	
			NCL_menubar[menu]->SetDefFloatWidth(savcols);
			FloatControlBar(NCL_menubar[menu], pt);
		}
		ShowControlBar(NCL_menubar[menu], TRUE, FALSE);
		NCL_menubar[menu]->m_visible = 1;
	}
}
/***********************************************************************
c
c   FUNCTION: OnAddNewMenubar()
c
c           added a new menu bar
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnAddNewMenubar()
{
	int menunum1, itemnum1, choice;
	int menu;
/*
......open a POPUP menu with "Copy Here" (choice=2), "Move Here", "Cancel" (choice=0)
*/
	if (m_menu_desgn==0)
	{
		uw_ntpopchoice(0, &choice);
		if ((choice==0)||(choice==-1))
			return;
	}
/*
......get current position and assigned to menu position
*/
	int pos[5];
	pos[0] = m_add_menupos[0];
	if (pos[0]!=m_add_menupos[0])
		return;
	pos[1] = m_add_menupos[1];
	if (pos[1]!=m_add_menupos[1])
		return;
	pos[2] = m_add_menupos[2];
	if (pos[2]!=m_add_menupos[2])
		return;

	char tempstr[100];
	sprintf_s(tempstr, 100, "%d", m_add_menupos[2]);

	pos[3] = m_add_menupos[3];
	if (pos[3]!=m_add_menupos[3])
		return;
	pos[4] = m_add_menupos[4];
	if (pos[4]!=m_add_menupos[4])
		return;
/*
.....pass in NULL as filename into ud_create_UDMmenu and prompt the menu
.....name type... there.
*/
	menunum1 = NCL_MainFrame->m_remove_menu;
	itemnum1 = NCL_MainFrame->m_remove_item;
	if (m_menu_desgn==0)
		menu = ud_create_UDMmenu(menunum1, itemnum1, (char *)UU_NULL, choice, pos);
	else
	{
		menu = ud_create_UDMmenu2(m_menudsgn_dlg->m_menu_item, (char *)UU_NULL, pos);
	}
	if (menu==-1) 
		return;
	NCL_MainFrame->m_addmenu = menu;
	NCL_MainFrame->m_additem = 0;		
	if (m_menu_desgn==0)
	{
		if (NCL_menubar[menunum1] != NULL)
		{
			redisp_menubar(menunum1);
		}
		else if (NCL_menu[menunum1] != NULL)
		{
			redisp_popmenu(menunum1);
		}
	}
/*
.....for this ICON menu new added, we want it docked in the end of the area
.....instead of defined in layout file, so save it and reset it
*/
	int dockable;
	ud_get_areadockable(UDM_menu[menu].menu_area, &dockable);
	ud_set_areadockable(UDM_menu[menu].menu_area, UDM_AREA_ENDDOCK);
	uw_ntmenu(menu, 1);
	ud_set_areadockable(UDM_menu[menu].menu_area, dockable);
	m_menu_desgn = 0;
}

/***********************************************************************
c
c   FUNCTION: OnUpdateMenubar()
c
c           updated current drag/drop menu bar
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnUpdateMenubar()
{
	int menu1 = m_addmenu;
	int item1 = m_additem;
	int menu2 = m_remove_menu;
	int item2 = m_remove_item;
	if ((menu1==menu2)&&(menu1!=-1)&&(item1==-1)&&(item2==-1))
	{
/*
......updated the whole menu with the new title, size, type
......this only apply to menu bar only
*/
		if (NCL_menubar[menu1] != NULL)
		{
			CImageList *rimglist = NULL;
			CToolBarCtrl &rctrl = NCL_menubar[menu1]->GetToolBarCtrl();
			rimglist = rctrl.GetImageList();
			if (rimglist!=NULL)
			{
				delete rimglist;
				rimglist = NULL;
			}
			delete NCL_menubar[menu1];
			NCL_menubar[menu1] = NULL;
			uw_ntmenu(menu1, 0);
			ShowControlBar(NCL_menubar[menu1], TRUE, FALSE);
			NCL_menubar[menu1]->m_visible = 1;
		}
		return;
	}
	if ((menu1==menu2)&&(menu1!=-1)&&(item1==item2)&&(item1!=-1))
	{
/*
......it mean update item1/item2 in menu1/menu2, not insert or delete item
*/
		if (NCL_menubar[menu1] != NULL)
		{
			redisp_menubar(menu1);
			return;
		}
		if (NCL_menu[menu1] != NULL)
		{
			redisp_popmenu(menu1);
			return;
		}
	}
	if (menu1!=-1)
	{
		if (NCL_menubar[menu1] != NULL)
		{
			redisp_menubar(menu1);
			goto menu2_display;
		}
		if (NCL_menu[menu1] != NULL)
		{
			redisp_popmenu(menu1);
		}
	}
menu2_display:
	if (menu2!=-1)
	{
		if (NCL_menubar[menu2] != NULL)
		{
/*
......do not redisplay
*/
			if (menu2!=menu1)
				redisp_menubar(menu2);
			goto done;
		}
		if (NCL_menu[menu2] != NULL)
		{
			if (menu2!=menu1)
				redisp_popmenu(menu2);
		}
	}
done:;
	m_addmenu = -1;
	m_remove_menu = -1;
}

/***********************************************************************
**
**   FUNCTION: SaveMenu(int menunum)
**
**       option to save/save as a menu index 
**
**   INPUT: menunum: pulldown menu index to be save/save as 
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CMainFrame::SaveMenu(int menunum)
{
	UX_pathname menufile, dir, fdir;
	int status, menu_type, format, dockable, len, choice, saved_menu_type, changed;
	char menu_area[256], title[30];
	uw_ntpopchoice(2, &choice);
	if ((choice!=1)&&(choice!=2))
		return;
/*
......get current menu file name
*/
	strcpy(menufile, UDM_menu[menunum].file);
	if (strncmp(UDM_menu[menunum].file,"Active_Temp_Menu", 16)==0)
		menufile[0] = '\0';
	if (choice==1)
	{
/*
.....save as
*/
		if (menufile[0]=='\0')
		{
			ul_get_full_dir("UU_USER_SETTINGS", dir);
			ul_build_full_dir(dir, "menu", fdir);
			strcpy(menufile, fdir);
			strcat (menufile, "\\*.menu");
		}
		ud_get_filename(NULL,"Save Menu File","*.menu", menufile, &len,
			"NCL Menu File (*.menu)", 0);
		if (len<=0) return;
	}
	else if (choice==2)
	{
/*
.....save
*/
		if (menufile[0]=='\0')
		{
			ul_get_full_dir("UU_USER_SETTINGS", dir);
			ul_build_full_dir(dir, "menu", fdir);
			strcpy(menufile, fdir);
			strcat (menufile, "\\*.menu");
			ud_get_filename(NULL,"Save Menu File","*.menu", menufile, &len,
				"NCL Menu File (*.menu)", 0);
			if (len<=0) return;
		}
	}

	char *p = (char *)strrchr(menufile,'.');
	if (p == 0)
		strcat(menufile, ".menu");
	else
	{
		*p = 0;
		strcat(menufile, ".menu");
	}
	strcpy(UDM_menu[menunum].file, menufile);
	ud_save_dragmenu(menunum);
}

/////////////////////////////////////////////////////////////////////////////
// CWnd will delegate owner draw messages to self drawing controls

void CMainFrame::OnDrawItem(int /*nIDCtl*/, LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	CMenu* pMenu;
	if (lpDrawItemStruct->CtlType == ODT_MENU)
	{
		pMenu = CMenu::FromHandlePermanent(
			(HMENU)lpDrawItemStruct->hwndItem);
		if (pMenu != NULL)
		{
			pMenu->DrawItem(lpDrawItemStruct);
			return;
		}
	}
	else
	{
		if (ReflectLastMsg(lpDrawItemStruct->hwndItem))
			return;
	}
	Default();
}

/*********************************************************************
**	 I_FUNCTION : SPConnexTranslateEvent
**		This function decode incoming event message from Spacemouse (3DConnexion)
**		for NCL
**	 PARAMETERS	
**		 INPUT  :
**					eventData: spacemouse event
**					
**		 OUTPUT :
**					event: event number for NCL
**	 RETURNS: 0 = not spacemouse event
**	          
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int SPConnexTranslateEvent(SiSpwEvent *eventData, int *event_num )
{			    
	int offset_x, offset_y, offset_z, offset_a, offset_b, offset_c; 
	static int last_event = 33;
	static int max_x=0, max_y = 0, max_z = 0,max_a=0, max_b = 0, max_c = 0;

	if (DConnexion==0)
		return -1;

	if (UV_SM_dom==1)
/*
......only save one direction data (largest offset)
*/
	{
		offset_x = eventData->u.spwData.mData[SI_TX];
		offset_y = eventData->u.spwData.mData[SI_TY];
		offset_z = eventData->u.spwData.mData[SI_TZ];
		offset_a = eventData->u.spwData.mData[SI_RX];
		offset_b = eventData->u.spwData.mData[SI_RY];
		offset_c = eventData->u.spwData.mData[SI_RZ];

		if (UV_SM_pan==0)
/*
......disable pan, so ignore x,y,z offset
*/
		{
			offset_x = offset_y = offset_z = 0;
		}
		if (UV_SM_rotate==0)
/*
......disable rotate, so ignore a,b,c offset
*/
		{
			offset_a = offset_b = offset_c = 0;
		}
/*
......no offset on all direction, using the last event number
*/
		if ((offset_a==0)&&(offset_b==0)&&(offset_c==0)
				&&(offset_x==0)&&(offset_y==0)&&(offset_z==0))
		{
			return FALSE;
		}
		if ( (abs(offset_x)>=abs(offset_y)) && (abs(offset_x)>=abs(offset_z)) && 
			(abs(offset_x)>=abs(offset_a)) && (abs(offset_x)>=abs(offset_b))
			&& (abs(offset_x)>=abs(offset_c)) )
		{
/*
......dial_1_left NCL_XPAN_LEFT
*/
			if (offset_x < 0) 
				*event_num = 33;
/*
......dial_1_right NCL_XPAN_RIGHT
*/
			else
				*event_num = 34;
			UV_actsm_dial = 1;
		}
		else if ( (abs(offset_y)>=abs(offset_x)) && (abs(offset_y)>=abs(offset_z)) && 
			(abs(offset_y)>=abs(offset_a)) && (abs(offset_y)>=abs(offset_b))
			&& (abs(offset_y)>=abs(offset_c)) )
		{
/*
.....dial_2_left NCL_ZOOM_DOWN
*/
			if (offset_y < 0) 
				*event_num = 37;
/*
.....dial_2_right NCL_ZOOM_UP
*/
			else
				*event_num = 38;
			UV_actsm_dial = 2;
		}
		else if ( (abs(offset_z)>=abs(offset_x)) && (abs(offset_z)>=abs(offset_y)) && 
			(abs(offset_z)>=abs(offset_a)) && (abs(offset_z)>=abs(offset_b))
			&& (abs(offset_z)>=abs(offset_c)) )
		{
/*
.....dial_3_left
*/
			if (offset_z < 0) 
				*event_num = 35;
/*
.....dial_3_right
*/
			else
				*event_num = 36;
			UV_actsm_dial = 3;
		}
		else if ( (abs(offset_a)>=abs(offset_x)) && (abs(offset_a)>=abs(offset_y)) && 
			(abs(offset_a)>=abs(offset_z)) && (abs(offset_a)>=abs(offset_b))
			&& (abs(offset_a)>=abs(offset_c)) )
		{
/*
.....dial_4_left
*/
			if (offset_a < 0) 
				*event_num = 41;
/*
.....dial_4_right
*/
			else
				*event_num = 42;
			UV_actsm_dial = 4;
		}
		else if ( (abs(offset_b)>=abs(offset_x)) && (abs(offset_b)>=abs(offset_y)) && 
			(abs(offset_b)>=abs(offset_z)) && (abs(offset_b)>=abs(offset_a))
			&& (abs(offset_b)>=abs(offset_c)) )
		{
/*
.....dial_5_left
*/
			if (offset_b < 0) 
				*event_num = 39;
/*
.....dial_5_right
*/
			else
				*event_num = 40;
			UV_actsm_dial = 5;
		}
		else if ( (abs(offset_c)>=abs(offset_x)) && (abs(offset_c)>=abs(offset_y)) && 
			(abs(offset_c)>=abs(offset_z)) && (abs(offset_c)>=abs(offset_a))
			&& (abs(offset_c)>=abs(offset_b)) )
		{
/*
.....dial_6_left NCL_ZROT_LEFT
*/
			if (offset_c < 0) 
				*event_num = 43;
/*
.....dial_6_right NCL_ZROT_RIGHT
*/
			else
				*event_num = 44;
			UV_actsm_dial = 6;
		}
		last_event = *event_num;
	}
	else
	{
/*
.....all motion function need execute
*/
		*event_num = 45;
	}
	return 0;
}

LRESULT CMainFrame::On3DxWare( WPARAM wParam, LPARAM lParam )
{
	SiSpwEvent event; /* 3DxWare Event */
	SiGetEventData eData; /* 3DxWare Event Data */
	int num, event_num;
	char sbuf[80];
	int data[6];
	char *indx, buf[20];

	indx = buf;
	SiGetEventWinInit (&eData, WM_3DXWARE, wParam, lParam);

	if (SiGetEvent (m_DevHdl, 0, &eData, &event) == SI_IS_EVENT)
    {
		switch (event.type)
		{
			case SI_MOTION_EVENT:
				num = event.type;
				data[0] = event.u.spwData.mData[SI_TX];
				data[1] = event.u.spwData.mData[SI_TY];
				data[2] = -event.u.spwData.mData[SI_TZ];
				data[3] = -event.u.spwData.mData[SI_RX];
				data[4] = -event.u.spwData.mData[SI_RY];
				data[5] = event.u.spwData.mData[SI_RZ];
				SPConnexTranslateEvent(&event, &event_num);
				uz_user_smouse(event_num, data, &indx, 1);
				break;

			case SI_ZERO_EVENT:
				num = event.type;
				break;

			case SI_BUTTON_EVENT:
				if ((num = SiButtonPressed (&event)) != SI_NO_BUTTON)	
				{
					switch (num) 
					{
						case NCL_SI_MENU_BUTTON:
							event_num = 13;
							break;
						case NCL_SI_FIT_BUTTON:
							event_num = 14;
							break;
						case NCL_SI_TOP_BUTTON:
							event_num = 15;
							break;
						case NCL_SI_RIGHT_BUTTON:
							event_num = 16;
							break;
						case NCL_SI_FRONT_BUTTON:
							event_num = 17;
							break;
						case NCL_SI_ROLLPLUS_BUTTON:
							event_num = 18;
							break;
						case NCL_SI_BUTTON_1:
							event_num = 1;
							break;
						case NCL_SI_BUTTON_2:
							event_num = 2;
							break;
						case NCL_SI_BUTTON_3:
							event_num = 3;
							break;
						case NCL_SI_BUTTON_4:
							event_num = 4;
							break;
						case NCL_SI_ESC_BUTTON:
							event_num = 19;
							break;
						case NCL_SI_ALT_BUTTON:
							event_num = 20;
							break;
						case NCL_SI_SHIFT_BUTTON:
							event_num = 21;
							break;
						case NCL_SI_CTRL_BUTTON:
							event_num = 22;
							break;
						case NCL_SI_ROT_BUTTON:
							event_num = 23;
							break;
						default: 
							sprintf(sbuf,"Button Released Button=%d", num);
							MessageBox (sbuf);  
							return (TRUE);
					}
					data[0] = 0;
					data[1] = 0;
					data[2] = 0;
					data[3] = 0;
					data[4] = 0;
					data[5] = 0;
					uz_user_smouse(event_num, data, &indx, 1);
				}
				break;
			default:
				break;

		}
	}
	return (TRUE);
}
/*
......doing nothing extra now, but do need declare this function
......to match up function define as NCL does
*/
void CMainFrame::OnMouseMove(UINT nFlags, CPoint point) 
{	
	CFrameWnd::OnMouseMove(nFlags, point);
}
void CMainFrame::OnDropFiles(HDROP hDropInfo)
{
/*
......doing nothing now, but do need declare this function
......to match up function define as NCL does
*/
	return;
/*
    UINT i;
    UINT nFiles = ::DragQueryFile(hDropInfo, (UINT) -1, NULL, 0);
	if (nFiles>1) nFiles = 1;
    for (i = 0; i < nFiles; i++)
    {
        char szFileName[_MAX_PATH];
        ::DragQueryFile(hDropInfo, i, szFileName, _MAX_PATH);
    } 
    ::DragFinish(hDropInfo);
*/
}

/*********************************************************************
**    E_FUNCTION : uw_nt_nclfunc(UINT id, int jflag)
**    DESCRIPTION:
**      This function executable NCL function from the callID
**		if the jflag=0, it normally called from a object class
**		function, so we don't do the long jump, since it will break 
**		long jump order, (but we do need set the mark in order for NCL
**		function to jump back: use UU_TRUE when set mark
**		otherwise, jflag=1, it mean it called from a C function
**		we will setup longjump mark in order to jump back
**        Get the text input event
**
**    PARAMETERS   
**       INPUT  : 
**          id: callID of NCL function
**			jflag: jump flag as describ above
**       OUTPUT :  
**          None
**
**    RETURNS: none      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_nt_nclfunc(UINT id, int jflag)
{
/*
......use local value
*/
	int savcmd_enable, sav_cmdcur1, sav_cmdcur2, save_ptype, sav_cmdmod, line_num;
	int upload;
	int* save_inptr;
	CString sav_cmdstr;
	UM_pkwin_type sarea;
	char *sav_hwnd;
	int jmpflag, sav_cur, sav_mode, ptype_saved;
	int num = id - (WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM);
/*
......reset timer for menubar
*/
	if (UW_current_menubar!=NULL)
		UW_current_menubar->reset_timer();

	sav_cur = uw_ntgetcur_cursor();
	sav_mode = UD_pickmode;
/*	ptype_saved = 0;
	if (! ( (uw_ntis_alphakey(num)) || (num==UZ_form_hotkey[0].key+1)
				|| (num==UZ_form_hotkey[1].key+1)) )
	{	
		savcmd_enable = NCL_MainFrame->Get_CmdEnable();
		if (savcmd_enable==1)
		{
			NCL_MainFrame->GetCommand_Str(sav_cmdstr, line_num, upload);
			uw_ntget_ecurpos(&sav_cmdcur1, &sav_cmdcur2);
			NCL_MainFrame->SetCommand_Str("");
			NCL_MainFrame->Enable_cmdbar(0);
			NCL_MainFrame->SetCommand_focus(0);
		}
	}
*/
	NCL_MainFrame->SetTimer(1, 1000*10, NULL);
	if (jflag==1)
	{
		UD_MARK(jmpflag,UU_FALSE);
	}
	else
	{
		UD_MARK(jmpflag,UU_TRUE);
	}
	if (jmpflag == 0)
	{
//		NCL_cmdmod = sav_cmdmod;
//		NCL_cmdmod = 0;
		uw_ntsetcursor(21);
		uz_wnt_callfunc(num,2);
		glFlush_d();
//		NCL_cmdmod = sav_cmdmod;
	}
	uw_ntsetcursor(sav_cur);
/*
	if (!(uw_ntis_alphakey(num)))
	{	
		if (savcmd_enable==1)
		{
			NCL_MainFrame->Enable_cmdbar(1);
			NCL_MainFrame->SetCommand_Str(sav_cmdstr, line_num, upload);
			NCL_MainFrame->SetCommand_focus(1);
			NCL_MainFrame->SetCommand_Insertpos(sav_cmdcur1, sav_cmdcur2);
		}
	}
*/
	UD_pickmode = sav_mode;
	NCL_MainFrame->KillTimer(1);
	UD_UNMARK(jmpflag);
}
/*
......flag: 1: display a modal menu design dialog, user accept/cancel the menu value
......			then the value will be returned in 'menu_item'
......		0: display a modaless dialog, user can always display this menu
......			dialog and interactive with other menu to do the menu design
*/
/***********************************************************************
c
c   FUNCTION: uw_ntmenu_desgn(int flag, UDM_menu_struc *menu_item)
c
c           display a menu design form and do the menu design
c
c   INPUT:  
c			flag: 1: display a modal menu design dialog, user accept/cancel the menu value
c					then the value will be returned in 'menu_item'
c				  0: display a modaless dialog, user can always display this menu
c					dialog and interactive with other menu to do the menu design
c   OUTPUT :   
c			menu_item: menu structure of the form input
c   RETURN:    -1: failed
c
**********************************************************************/
extern "C" int uw_ntmenu_desgn(int flag, UDM_menu_struc *menu_item)
{
	int len;
	if (flag)
	{
		CNCLMenuDsnDlg *dlg = new CNCLMenuDsnDlg();	
		dlg->m_modal = 1;
		dlg->Set_menu_data(menu_item);
		int nResponse = dlg->DoModal();
		if (nResponse == IDOK)
		{
/*
......copy the local dialog m_menu_item value into 'menu_item' and return
*/
			uu_move_byte((char*)&(dlg->m_menu_item), (char*)menu_item, sizeof(UDM_menu_struc));
			if (dlg->m_menu_item.params!=NULL)
			{
				len = strlen(dlg->m_menu_item.params);
				if (len>0)
				{
					menu_item->params = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy_s((char*)(menu_item->params), len+1, ((char*)dlg->m_menu_item.params));
				}
				else
					menu_item->params = (char*)UU_NULL;
			}
			else
				menu_item->params = (char*)UU_NULL;
			return 0;
		}
		else
			return -1;
	}
	if (NCL_MainFrame->m_menudsgn_dlg==UU_NULL)
	{
		NCL_MainFrame->m_menudsgn_dlg = new CNCLMenuDsnDlg();	
		NCL_MainFrame->m_menudsgn_dlg->m_modal = 0;
		NCL_MainFrame->m_menudsgn_dlg->Create(CNCLMenuDsnDlg::IDD);
		NCL_MainFrame->m_menudsgn_dlg->ShowWindow(SW_SHOW);
	}
	else
	{
		NCL_MainFrame->m_menudsgn_dlg->Reload_menulist();
		NCL_MainFrame->m_menudsgn_dlg->ShowWindow(SW_SHOW);
		NCL_MainFrame->m_menudsgn_dlg->Reset_Ctllist(0);
	}
	return 0;
}
/***********************************************************************
c
c   FUNCTION: uw_ntmenulayout()
c
c           display a menu design form and do the menu design, it will always be a 
c			a modal menu design dialog
c
c   INPUT:  None
c   OUTPUT :   
c			None
c   RETURN:    None
c
**********************************************************************/
extern "C" void uw_ntmenulayout()
{
	uw_ntmenu_desgn(0, 0);
}
