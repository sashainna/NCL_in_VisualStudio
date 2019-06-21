#include "usysdef.h"
/********************************************************************* 
**  NAME:  wsntframe.cpp
**
**			Native WinNT main frame functions
**			implementation of CMainFrame class functions
**	CONTAINS: CMainFrame class functions
**			all functions declared in wsntframe.h
**			uw_ntget_dockrect
**			uw_form_desgn(int flag, char *filename)
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsntframe.cpp , 26.3
**    DATE AND TIME OF LAST  MODIFICATION
**       04/17/18 , 14:27:36
*********************************************************************/
#include "wsntstdafx.h"
#include <afxole.h>         // MFC OLE classes
#include "wsntctl.h"
#include "dmotif.h"
#include "wsntframe.h"
#include "wsntdoc.h"
#include "wsntview.h"
#include "wsnttmenu.h"
#include "wsntpmenu.h"
#include "wsntcfunc.h"
#include "wsgl.h"
#include "wsntglfunc.h"
#include "wsntfuncid.h"
#include "wsntres.h"
#include "zkeysym.h"
#include "wsntbitmap.h"
#include "dmark.h"
#include "wsntsmouse.h"
#include "wsglfun.h"
#include "dselect.h"
#include "wsntmenudsndlg.h"
#include "spmouse.h"
#include "lcom.h"

#include "wsntformdoc.h"
#include "wsntFdsnView.h"
#include "wsntFdsnFrm.h"
#include "wsntModalFrame.h"
#include "wsntstatbtn.h"

// From AFXIMPL.H
//#define _countof(array) (sizeof(array)/sizeof(array[0]))

#define NBUTTONNUM	5000
#define NCHOICEBUTTON 1000

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

HMAGELLAN MagellanHandle = NULL;
int DConnexion = 0;

CMainFrame *NCL_MainFrame = NULL;
extern CWnd *NCL_Main_View;
HWND UW_TrackingWindow = NULL;
HMENU UW_TrackingMenu = NULL;
HWND NCL_cmdwin, NCL_statwin;
int NCL_savcmd_enable;

extern int UU_SM_capture;
extern DWORD UW_theme;
extern "C" int UW_icon_size, UW_menu_fmt;
extern "C" int uw_ntis_alphakey(int num);
extern "C" int uw_ntis_inputfunc(int num);
extern "C" int uw_ntmenu(int,int);
extern "C" int uw_nterror_area(int);
extern "C" int uw_ntprompt_area(int);
extern "C" int uw_display_dragmenu(int );
extern "C" int uw_ntshow_dir(int menu_pos);
extern "C" int uz_status_inittable();
extern "C" int isquit ();
extern "C" int uz_ntcnvtnum_to_menu(int num, int *menunum);
extern "C" int uw_ntgetcur_cursor();

extern "C" int NT_FuncEvent, NT_FuncEof;;
extern "C" int UW_signon_failed;
extern "C" int ul_clear_exit();
extern "C" int uw_ntdispmsg(char *);
int frm_done = 0;
extern int NCL_PupupChoice;
extern int PopupChoice[20];
extern int NCL_exit, NCL_safeexit;
extern int NCL_WNT_BUTDOWN; 
extern int UW_reset_menu;
extern CNCLToolBar *UW_current_menubar;

extern "C" int ud_prmerr(char *);
extern "C" char *ux_getenv(char *,int);

int NCL_popmenu;
extern CNCLMenu *UW_topmenu;
extern "C" int uz_user_smouse(int event, int data[6], char **indx, int flag);
extern "C" int uw_ntsignoff(int restart);

extern "C" int uw_open_gldebug_file();
extern "C" int uw_close_gldebug_file();
int UU_active = 0;
extern "C" int ug_save_input(int** save_inptr_pt);
extern "C" int ug_reset_input(int* save_inptr);
extern "C" void uw_ntget_ecurpos(int *start, int *end);
extern "C" int ud_setpick_type(int);
extern "C" int ud_getpick_type();
extern "C" int uw_glswapbuffer();
extern "C" void uw_ntwindow();
extern "C" int ud_updatews(int);
extern "C" 	int uw_open_keycom(char*);
extern "C" 	int uw_close_keycom();
extern "C" int uw_chkkey_common(char*);

extern int UW_not_paint;
extern "C" ACCEL UZ_form_hotkey[2];
extern "C" unsigned int NCL_subprocess;
extern "C" char NCL_keyinfo[9];
extern "C" UU_REAL NCL_version;
extern CImageList * uw_create_iconimg();
extern int UW_NPopup;

extern "C" void uw_nt_nclfunc(UINT id, int jflag);
extern "C" void ncl_reset_pikgeom();
extern "C" int NCL_mouse_func;
extern "C" char *UM_pocket_hwnd;

extern "C" int NCL_cmdmod;
int UW_struct_change = 0;
extern "C" int UW_live_mouse, UZ_key_pickloc;
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
extern "C" int insmode_val();

int S_update_stat = 0;
extern int UW_addmenu, UW_additem, UW_remove_menu, UW_remove_item;

static CString Sstatus_lable[UZ_MAX_STATNAME];
/////////////////////////////////////////////////////////////////////////////
// 

/////////////////////////////////////////////////////////////////////////////
// CMainFrame

extern int NCL_AddMenuBar(CMainFrame*, char*);
static int fixed_menu = 100;
extern "C" int UD_gotuimsfil;
extern "C" ACCEL *UZ_ncl_accel;
extern "C" int UZ_ncl_accelnum;
extern "C" void um_set_screen_area(UM_pkwin_type UM_IPV_WINDOW);
extern "C" UM_pkwin_type um_get_screen_area();
extern int uw_ntget_ctlbar_type(CControlBar* chkbar);
extern void uw_ntadd_menuborder(CRect *rect);
extern "C" int uz_ntcnvtfunc_to_num(char* func, char *parms, char *descrip, int *num);
extern "C" int uz_ntmenunum_to_num(int current_menu, int *bnum);
extern "C" char * uu_malloc(int);
extern "C" void ud_get_filename(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag);

int UW_drag_pos[5];

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
	ON_WM_MOUSEMOVE()

	ON_COMMAND(IDF_KEY_NOOP, OnKeyNoop)
	ON_COMMAND(ID_WM_CLOSE2, OnClose2)
	ON_COMMAND_RANGE(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+ NBUTTONNUM + UDM_MAX_MENU, 
					WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+ NBUTTONNUM + UDM_MAX_MENU + NCHOICEBUTTON, OnPupupChoices)
	ON_COMMAND_RANGE(WM_APP, WM_APP+UDM_MAX_MENU-1, OnDisplayMenu)
	ON_COMMAND_RANGE(WM_APP+UDM_MAX_MENU, WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM-1, OnAccelFunctions)
	ON_COMMAND_RANGE(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM, WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM, OnNCLFunctions)
	ON_COMMAND_RANGE(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM, WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM + UDM_MAX_MENU - 1,OnDragMenu)
	ON_WM_CLOSE()
	ON_WM_NCLBUTTONUP()
	ON_WM_LBUTTONUP()
	ON_WM_LBUTTONDOWN()
	ON_WM_SIZING()
	ON_WM_TIMER()
	ON_WM_DROPFILES()
	ON_MESSAGE(WM_MENUDRAG, OnMenuDrag)
	ON_MESSAGE(WM_MENUGETOBJECT, OnMenuGetObject)
	ON_MESSAGE(WM_MENURBUTTONUP, OnMenuRbuttonup)
	ON_COMMAND(UW_UPDATE_MENUBAR, OnUpdateMenubar)
	ON_COMMAND(UW_ADDNEW_MENUBAR, OnAddNewMenubar)
	ON_COMMAND(UW_UPDATE_STATUSBAR, OnUpdateStatusbar)

	ON_REGISTERED_MESSAGE( WM_3DXWARE, On3DxWare )
	
	//}}AFX_MSG_MAP
	ON_NOTIFY_EX_RANGE(TTN_NEEDTEXTW, 0, 0xFFFF, OnDynamicTipText)
	ON_NOTIFY_EX_RANGE(TTN_NEEDTEXTA, 0, 0xFFFF, OnDynamicTipText)

END_MESSAGE_MAP()

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
		if (UDM_menu[menu].statflag!=0)
			uz_status();
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
	int len, menu;
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
	if (UDM_menu[menu].statflag==1)
		uz_status();
}

/***********************************************************************
c
c   FUNCTION: OnUpdateMenubar()
c
c           updated current drag/drop menu bar
c			the menubar is accept drop
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
		else if (NCL_menu[menu2] != NULL)
		{
			if (menu2!=menu1)
				redisp_popmenu(menu2);
		}
/*
......it could be a menupane form bottom statusbar
*/
		else
		{
			if (UDM_menu[menu2].statflag==2)
			{
				if (menu2!=menu1)
				{
					if (UDM_menu[menu2].num==0)
						m_bStatusBar.RemovePaneIndx(menu2);
					else
						m_bStatusBar.Redisppane(menu2);
				}
				goto done;
			}
		}
	}
done:;
	m_addmenu = -1;
	m_remove_menu = -1;
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
/*
......reset timer for menubar
*/
	if (UW_current_menubar!=NULL)
		UW_current_menubar->reset_timer();
	uz_ntcnvtnum_to_menu(num, &menunum);
/*
.....menunum could be -1 when the function ID or menu not exist
*/
	if (menunum>=0)
	{
		ul_break_fname(UDM_menu[menunum].file, dir, file);
		ud_rpwrmenu(file, "", UDM_menu[menunum].name);
		uw_ntmenu(menunum,1);
		S_update_stat = 1;
		uz_status();
		S_update_stat = 0;
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
	S_update_stat = 1;
	uz_status();
	S_update_stat = 0;
}
/////////////////////////////////////////////////////////////////////////////
// CMainFrame construction/destruction

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
	m_sub_ncl = 0;
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
	int pos[5];
	pos[0] = m_sub_ncl;
	m_DevHdl = NULL;
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
	if ((UD_pickmode==1)&&(NCL_safeexit==0))
	{
/*
.....when in input mode and click the "X" in the main frame
*/
		PostMessage(WM_COMMAND, ID_WM_CLOSE2);
		uw_close_gldebug_file();
		return;
	}
	if (NCL_exit==0)
/*
.....called from user clip "close" or "x" in the frame window
*/
	{
		CNCLApp* pApp;
		pApp = (CNCLApp* )AfxGetApp();
		pApp->OnNCLSafeExit();
	}
	else
/*
.....called from OnNCLAppExit() function
.....Also all others (such as "click the 'X" in the main frame) to exit
.....will all come to here
*/
	{
		GetActiveDocument()->m_bAutoDelete = TRUE;
		ul_clear_exit();
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
			}
			if (NCL_menu[i] != NULL)
			{
				if (!(::IsMenu(NCL_menu[i]->m_hMenu)))
					continue;
				delete NCL_menu[i];
			}
		}	
		NCL_MainFrame = NULL;
		CFrameWnd::OnClose();
		uw_close_gldebug_file();
	}
}

/***********************************************************************
c
c   FUNCTION: ~CMainFrame()
c
c              Destructor of class CMainFrame
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CMainFrame::~CMainFrame()
{
	DWORD ecode;
	int i,ret;
	for (i=0; i<m_sub_ncl;i++)
	{
		ret = GetExitCodeProcess(m_ncl_info[i].hProcess, &ecode);
		if ((ret!=0)&&(ecode==STILL_ACTIVE))
		{
/*
......if we terminate sub-process here. It will terminate sub-ncl without it have a chance to save it
......so let sub-ncl terminate it itself?
......for sub-process saving, still have "questions" could let NCL stays
......but only stay 1 minute, it will tried to term again. Ask bobby
*/
			TerminateProcess(m_ncl_info[i].hProcess, 0);
		}
	}
	uw_close_keycom();
	if (m_menudsgn_dlg!=NULL)
	{
		m_menudsgn_dlg->DestroyWindow();
		delete m_menudsgn_dlg;
	}
	m_menudsgn_dlg = (CNCLMenuDsnDlg *)UU_NULL;
}
static UINT indicators[] =
{
//	ID_SEPARATOR,  
//	ID_INDICATOR_CAPS,
//	ID_INDICATOR_NUM,
//	ID_INDICATOR_SCRL,
	INDICATOR_WIN1
};

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
	char common_name[80];
	UW_not_paint = 1;
	if (CFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	DragAcceptFiles(TRUE);	
/*
......check main NCL process every minute
*/
	if (NCL_subprocess)
		SetTimer(1, 1000*60, NULL);
	else
/*
......if it's the main NCL, write the keys in the common memory area
*/
	{
		DWORD mid = GetCurrentProcessId();
		sprintf_s(common_name, 80, "NCLkeys_%u", mid);
		uw_open_keycom(common_name);
	}
/*
......set the initial size to 0,0
*/
	NCL_MainFrame = this;
	if (!m_bStatusBar.Create(this) ||
		!m_bStatusBar.SetIndicators(indicators,
		  sizeof(indicators)/sizeof(UINT)))
	{
		TRACE0("Failed to create status bar\n");
		return -1;      // fail to create
	}
	initial_ncl();
/*
.....we have to load m_bStatusBar before, so if there is no
.....statusbar define, just close it
*/
	if (UDM_layout.statbar_no==0)
	{
		m_bStatusBar.ShowWindow(SW_HIDE);
	}
//	else
/*
......display status bar
*/
	{
		DisplayStatusBar();
/*
.....when adjust status bar, the window will call onsize which will reset m_redraw of NCLVIEW
.....to 0 to cause the window to not draw view
*/
		((CNCLView*)NCL_Main_View)->reset_redraw();
	}
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
**   FUNCTION: initial_ncl
**
**		Doing the NCL related function for displayed in the main window
**   
**		INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CMainFrame::initial_ncl()
{
	uz_status_inittable();
	uw_open_gldebug_file();
/*
.....initial NCL
*/
	unicad_();
	uw_ntwindow();
	uw_ntcreate_font(0);
	uw_ntupd_comfont();
	if (isquit ())
		return;
	if (UW_signon_failed)
		return;		
/*
.....Create user key Accelerator
*/
	if (UZ_ncl_accelnum!=0)
		m_accel = CreateAcceleratorTable(UZ_ncl_accel, UZ_ncl_accelnum);
	NCL_cmdwin = m_commandBar->getcmdwin();
	Enable_cmdbar(0);
	CWnd* cwin;
	if (m_statusBar!=NULL)
	{
		cwin = m_statusBar->GetDlgItem(IDC_STEXT);
		NCL_statwin = cwin->m_hWnd;
	}
	uw_ntfirst_resize_graphics();
	frm_done = 1;

/*
......Initialize 3DxWare
*/
	SiOpenData oData;
    SiInitialize ();
    SiOpenWinInit (&oData, m_hWnd);
	if ( (m_DevHdl = SiOpen ("NCL", SI_ANY_DEVICE, SI_NO_MASK,
                            SI_EVENT, &oData)) == NULL )
	{
		SiTerminate ();
	}
	if (m_DevHdl!=NULL)
	{
//we need check out why we add this code, but it will make other software can't use spacemouse while using ncl
//temp remove 1/10/19 Yurong
//		SiGrabDevice(m_DevHdl, SPW_TRUE);
		SiSetUiMode (&m_DevHdl, SI_UI_ALL_CONTROLS);
	}
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
	((CNCLView*)NCL_Main_View)->reset_redraw();
	UW_not_paint = 0;
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
/*
.....somehow the user defined MOUSEMOVE function is not called, so
.....we will added here, also we need catch mousemove outside the frame.
*/
	CRect rect;
	GetClientRect(&rect);

	CPoint point = msg->pt;
	ScreenToClient(&point);

	if ((point.x<0)||(point.y<0)||(point.x>rect.right)||(point.y>rect.bottom))
	{
		m_bStatusBar.OnItemMouseMove(msg->wParam, msg->pt);
	}
	if (message == WM_MOUSEMOVE)
	{
		m_bStatusBar.OnItemMouseMove(msg->wParam, msg->pt);
	}
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
/*
.....if it is in input mode, post the message like UNIX does
.....the TranslateAccelerator will call OnAccelFunctions which will
.....doing nothing if UD_pickmode = 1. Change here just to consistence with UNIX
.....Yurong 3/15/02
*/
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
		m_bStatusBar.OnItemMouseMove(pMsg->wParam, pMsg->pt);
	}
	if (message == WM_MOUSEMOVE)
	{
		m_bStatusBar.OnItemMouseMove(pMsg->wParam, pMsg->pt);
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
	CNCLApp *app = (CNCLApp *)AfxGetApp();
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
**		Callback function for all NCL menu draging bar
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

/////////////////////////////////////////////////////////////////////////////
// CMainFrame message handlers

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
		strcpy_s(filename, UDM_menu[menunum].file);
		ul_break_fname(filename,dir,fname);
		sprintf_s(fname1, "Load %s", fname);
		strTipText = fname1;
	}
	else if ((nID >= WM_APP+UDM_MAX_MENU)&&(nID<WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM)) 
	{
		int num, status;
		char descript[256];
		num = nID - (WM_APP+UDM_MAX_MENU);
		status = uz_ntget_dspt(num, descript, 1);
		if (status==-1)
			strcpy_s(descript, "No description");
		strTipText = descript;
	}
	else if ((nID >= WM_APP+UDM_MAX_MENU + UZ_MAX_KEYITEM)&&(nID<WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM)) 
	{
		int num, status;
		char descript[256];
		num = nID - (WM_APP+UDM_MAX_MENU+ UZ_MAX_KEYITEM);
		status = uz_ntget_dspt(num, descript, 2);
		if (status==-1)
			strcpy_s(descript, "No description");
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
**    I_FUNCTION :  OnNCLFunctions(UINT id)
**		Callback function for all NCL menu functions
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
void CMainFrame::OnNCLFunctions(UINT id)
{
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
**		Callback function for all NCL Accelerator functions
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
	CString sav_cmdstr;
	char *sav_hwnd;
/*
......use local value
*/
	int savcmd_enable, sav_cmdcur1, sav_cmdcur2, save_ptype, ptype_saved, sav_cmdmod, line_num;
	int * save_inptr;
	UM_pkwin_type sarea;
	int jmpflag, sav_cur,sav_mode, upload;
	int num = id - (WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM);
			
	ncl_reset_pikgeom();
	NCL_mouse_func = 0;
/*
.....if it is in input mode, post the message like UNIX does in
.....function PreTranslateMessage to let uw_ntevent to handle it 
.....doing nothing if UD_pickmode = 1
.....Yurong 3/15/02
*/
	if (UD_pickmode==1)
		return;
	sav_mode = UD_pickmode;
	ug_save_event();
	ug_save_input(&save_inptr);
	savcmd_enable = Get_CmdEnable();
	sav_cur = uw_ntgetcur_cursor();
	ptype_saved = 0;
	sarea = um_get_screen_area();
	um_set_screen_area(UM_NCL_WINDOW);
	sav_hwnd = UM_pocket_hwnd;
	UM_pocket_hwnd = 0;

	if (sav_mode==0)
/*
.....if picking already active, don't reset picking type
*/
	{
		ptype_saved = 1;
		save_ptype = ud_getpick_type();
		ud_setpick_type(UD_PICK_NORMAL);
	}
/*
.....if it is in picking mode but executed functions other than
.....'by text', 'by pick' and 'by location' function
.....we still need save and reset the picking type
*/
	else
	{
		if (!(uw_ntis_inputfunc(num)))
		{
			ptype_saved = 1;
			save_ptype = ud_getpick_type();
			ud_setpick_type(UD_PICK_NORMAL);
		}
	}
/*
......can't use longjump since it will break 
......long jump order, (but we do need set the mark in order for NCL
.......function to jump back: use UU_TRUE when set mark
*/
//	UD_MARK(jmpflag,UU_FALSE);
	UD_MARK(jmpflag,UU_TRUE);
	SetTimer(2, 1000*10, NULL);
	if (jmpflag == 0)
	{
		int num = id - (WM_APP+UDM_MAX_MENU);
/*
.....save command text before disable it 
*/
		if (savcmd_enable==1)
		{
			GetCommand_Str(sav_cmdstr, line_num, upload);
/*
.....save text insert position too
*/
			uw_ntget_ecurpos(&sav_cmdcur1, &sav_cmdcur2);
			Enable_cmdbar(0);
			SetCommand_focus(0);
			SetCommand_Str("");
		}
		sav_cmdmod = NCL_cmdmod;
		NCL_cmdmod = 0;
		uw_ntsetcursor(21);
		uz_wnt_callfunc(num, 1);
		NCL_cmdmod = sav_cmdmod;
		Enable_cmdbar(savcmd_enable);
	}
	if (savcmd_enable==1)
	{
		SetCommand_Str(sav_cmdstr, line_num, upload);
		SetCommand_focus(1);
		SetCommand_Insertpos(sav_cmdcur1, sav_cmdcur2);
	}
	uw_ntsetcursor(sav_cur);
	ud_updatews(UG_SUPPRESS);
	UD_pickmode = sav_mode;
	ug_reset_event();
	ug_reset_input(save_inptr);
	if (ptype_saved)
		ud_setpick_type(save_ptype);
	um_set_screen_area(sarea);
	UM_pocket_hwnd = sav_hwnd;
	KillTimer(2);
	S_update_stat = 1;
	uz_status();
	S_update_stat = 0;
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
//	NCL_PupupChoice = id - IDC_POPUP_CHOICES1;
	PopupChoice[UW_NPopup] = id - (WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+ NBUTTONNUM + UDM_MAX_MENU);
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
	m_commandBar->setcmdfocus(focus);
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
	int line, load;
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
**    I_FUNCTION :  GetCommand_Str(char *cmdstr, int &line_num, , int &upload)
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
	if (flag)
		m_commandBar->reshow_edit();
	m_commandBar->enable_win(flag);
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
/*
.....if status mode is idle and update flag is NO, then just return
*/
	if ((S_update_stat==0)&&(UW_stat_mode))
		return;
/*
.....check if the message is the same
*/
	if (Sstatus_lable[sub].CompareNoCase(msg)==0)
		return;
	num = 0;
	int status = -1;
	int drawed = 0;
	while (num!=MAX_STAT_FUNC)
	{
		id = UZ_status_table[sub][num];
		if ((id==-1)||(id==IDF_KEY_NOOP))
			return;
		for (i=0;i<UDM_menu_count;i++) 
		{
			status = -1;
//			if (UDM_menu[i].statflag!=1)
			if (UDM_menu[i].statflag==0)
				continue;
			if (NCL_MainFrame->NCL_menubar[i]==NULL)
				continue;
			for (j=0; j<UDM_menu[i].num; j++)
			{
				NCL_MainFrame->NCL_menubar[i]->GetButtonInfo(j, nID, nStyle, iImage);
				if (nID==id)
				{
					strcpy_s(newmsg, UDM_menu[i].menus[j].name);
					strcat_s(newmsg, msg);
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
					if (((CNCLToolBar*)(NCL_MainFrame->NCL_menubar[i]))->m_visible==1)
					{
						NCL_MainFrame->NCL_menubar[i]->SetButtonText(j, newmsg);
						Sstatus_lable[sub] = msg;
					}
/*
......use "continue" if allow same status name in the one status bar 
*/					
					break;  
				}
			}
		}
/*
.....statflag==2
*/
		status = m_bStatusBar.UpdateLabel(id, msg);
		if (status==0)
			drawed = 1;
		num++;
	}
	if (drawed)
	{
		RecalcLayout();
		Sstatus_lable[sub] = msg;
	}
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
				if (UDM_menu[indx].menu_format!=1)
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
						strcpy_s(UDM_menu[indx].menus[bindx].bmpfile, UDM_menu[num].menus[i].bmpfile);
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
//							rpImgnum = rimglist->Add(rpImage, (CBitmap* )NULL);
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
						rimglist = uw_create_iconimg();
						rpImgnum = rimglist->Add(rpImage, 0xC0C0C0);
						rctrl.SetImageList(rimglist);
						NCL_MainFrame->NCL_menubar[indx]->SetButtonInfo(bindx, wID, nStyle, 
								rpImgnum + UDM_menu[num].menus[i].bmpnum);
					}
					UDM_menu[indx].menus[bindx].bmpnum = UDM_menu[num].menus[i].bmpnum;
				}
//				if ((UW_menu_fmt!=0)
//					||((UW_menu_fmt==0)&&(no_image==1)))
				if ((UDM_menu[indx].menu_format!=0)
					||((UDM_menu[indx].menu_format==0)&&(no_image==1)))
				{
					char label[80];
					strcpy_s(label, UDM_menu[num].menus[i].name);
					NCL_MainFrame->NCL_menubar[indx]->SetButtonText(bindx, label);
//					if ((UW_menu_fmt==1)||(no_image==1))
					if ((UDM_menu[indx].menu_format==1)||(no_image==1))
						NCL_MainFrame->NCL_menubar[indx]->SetButtonInfo(bindx, wID, nStyle, -1);
				}
//				else if (UW_menu_fmt==0)
				else if (UDM_menu[indx].menu_format==0)
				{
					NCL_MainFrame->NCL_menubar[indx]->RemoveButtonText(bindx);
				}
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
	int data[6], event, si_event;
	char *indx, buf[20];

	indx = buf;

	si_event = 0;
/* 
......3DxWare Event 
*/
	SiSpwEvent sevent;
	SiGetEventData eData;
/*
......Check if this is a 3DxWare event
*/
/*
	if (m_DevHdl!=NULL)
	{
		SiGetEventWinInit (&eData, message, wParam, lParam);
		if (SiGetEvent (m_DevHdl, 0, &eData, &sevent) == SI_IS_EVENT)
		{
			switch (sevent.type)
			{
				case SI_MOTION_EVENT:
					si_event = 1;
					break;

				case SI_ZERO_EVENT:
					si_event = 1;
					break;

				case SI_BUTTON_EVENT:
					si_event = 1;
					break;

				case SI_BUTTON_PRESS_EVENT:
					si_event = 1;
					break;

				case SI_BUTTON_RELEASE_EVENT:
					si_event = 1;
					break;

				default:
					si_event = 0;
					break;
			}
	//		return (TRUE);
		}
	}
*/
	if (( MagellanHandle != NULL )&&(si_event==0))
	{
		hWnd = GetSafeHwnd();
		MagellanMessage.hwnd = hWnd;
		MagellanMessage.message = message;
		MagellanMessage.wParam = wParam;
		MagellanMessage.lParam = lParam;
					
		switch( MagellanTranslateEvent( MagellanHandle, &MagellanMessage, &MagellanData, &event ) )
		{   
			case MotionEvent:
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
**    I_FUNCTION : Recreate_AcceleratorTable() 
**		Recreate accelerator table       
**
**    PARAMETERS   
**       INPUT  : none
**          
**
**       OUTPUT :  
**          none
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CMainFrame::Recreate_AcceleratorTable() 
{
	if (m_accel!=NULL)
	{
		DestroyAcceleratorTable(m_accel);
		m_accel = NULL;
	}
	if (UZ_ncl_accelnum!=0)
		m_accel = CreateAcceleratorTable(UZ_ncl_accel, UZ_ncl_accelnum);
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
	m_commandBar->setcmdsel(start, end);
}
/***********************************************************************
c
c   FUNCTION: OnClose2()
c
c      callback for close frame window
c      called when input mode is on and user click "X' of the main window 
c		to close NCL
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnClose2()
{
/*
......use local value
*/
	int jmpflag, sav_mode;

	sav_mode = UD_pickmode;
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag == 0)
	{
		uw_ntsignoff(0);
	}
	UD_pickmode = sav_mode;
	UD_UNMARK(jmpflag);
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
	};
/*
	if (UW_topmenu!=NULL)
	{
		for (i=0; i<20; i++)
		{
			status = UW_topmenu->DeleteMenu(0, MF_BYPOSITION);
			if (status==0)
				break;
		}
		delete UW_topmenu;
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
		}
		if (NCL_menu[i] != NULL)
		{
			if (!(::IsMenu(NCL_menu[i]->m_hMenu)))
				continue;
			delete NCL_menu[i];
		}
	}
*/
	delete UW_topmenu;
	if (m_commandBar!=NULL)
		delete m_commandBar;
	if (m_promptBar!=NULL)
		delete m_promptBar;
	if (m_errorBar!=NULL)
		delete m_errorBar;

	if (UZ_ncl_accel!=NULL)
		free ((char*)UZ_ncl_accel);
	CFrameWnd::OnDestroy();
}
/***********************************************************************
**
**   FUNCTION: StartNewNCL(char *parms)
**
**		This function start a sub-NCL process with
**		'parms' as paramters
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CMainFrame::StartNewNCL(char *parms)
{
	int result, ver_int;
	char cmdparm[120], sbuf[20];
	STARTUPINFO ncl_stinfo;

	if (NCL_subprocess>0)
	{
		uw_ntdispmsg("NCL sub-process cannot begin another NCL session.");
		return;
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
	DWORD mid = GetCurrentProcessId();

	if (m_sub_ncl>=50)
	{
/*
......maxinum 50 sub-ncl process running
*/
		uw_ntdispmsg("We only allow 50 NCL sub-processes to run.\r\nClose some NCL sub-processes in order to run another one.");
		return;
	}
	ver_int = (int)(NCL_version + 0.5);
	sprintf_s(sbuf,"NCLEXE%d",ver_int);
//	char * com = getenv(sbuf);
	char *com;
	size_t len;
	errno_t err = _dupenv_s(&com, &len, sbuf);
	if (parms==NULL)
		sprintf_s(cmdparm, "\"%s\" -n=%u", com, mid);
	else
		sprintf_s(cmdparm, "\"%s\" -n=%u %s", com, mid, parms);
	result = CreateProcess(NULL, cmdparm, NULL, NULL, FALSE, 
		DETACHED_PROCESS | NORMAL_PRIORITY_CLASS, 
		NULL, NULL, &ncl_stinfo, &m_ncl_info[m_sub_ncl]);
	if (result==0)
	{
		sprintf_s(cmdparm,"Error trying to run a new NCL session.\r\nMake sure the %s variable is correctly set.",sbuf);
		uw_ntdispmsg(cmdparm);
		return;
	}
/*
......call timer every 1 minute
*/
//temp
	SetTimer(1, 1000*60, NULL);
	m_sub_ncl++;
}
void CMainFrame::Ontimer2()
{
	return;
	static int start = 0;
/*
......if the window is doing the layout/resizing changing, don't call here.
*/
	if ((m_bInRecalcLayout)||(UW_struct_change))
		return;
	if (start==1)
		return;
	start = 1;
	MSG msg;
	BOOL bIdle = TRUE;
	LONG lIdleCount = 0;
	CNCLApp *app = (CNCLApp*)AfxGetApp();
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
				goto done;
			}
		}
		int loop = 0;
		do
		{
/*
.....don't call application PumpMessage instead
.....handle the message directly here
.....since the PumpMessage will call GetMessage which
.....will wait for message (if there is none)
.....in this function, there is suppose a message there
.....when we called GetMessage
.....since we just PeekMessage and it indicate there is one,
.....but the all the message handling is run at same time
.....so in some other place (function), this message maybe dispatched already
.....(in some other place)
.....when we call GetMessage even though it is very short time
.....after we PeekMessage.
.....So just handle directly to call
.....TranslateMessage & DispatchMessage without call application PumpMessage
*/
			if (msg.message==WM_QUIT)
				break;
			if (msg.message != WM_KICKIDLE && !NCL_MainFrame->NCLPreTranslateMessage(&msg))
			{
				::TranslateMessage(&msg);
				::DispatchMessage(&msg);
			}
/*
.....don't forget this message after dispatch it
.....(GetMessage will remove it from the queue
.....PeekMessage is depend on remove flag,
.....but GetMessage don't remove WM_PAINT until process
.....so leave it alone
*/
			if (msg.message != WM_PAINT)
				::PeekMessage(&msg, msg.hwnd, msg.message, msg.message, PM_REMOVE);
			if (app->IsIdleMessage(&msg))
			{
				bIdle = TRUE;
				lIdleCount = 0;
			}
			loop++;
			if (loop>100) return;
		} while (::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE));
	}
done:;
	start = 0;
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
	int ret;
	DWORD ecode;
	char keys[12];
	int counter = 0;
	int stat = 0;
	static int running = 0;
	
	if (nIDEvent==2)
	{
		Ontimer2();
		CFrameWnd::OnTimer(nIDEvent);
		return;
	}
/*
......don't call this whille ontimer is running
*/
	if (running==1)
		return;
	if (nIDEvent!=1)
		return;
	running = 1;
/*
......if this NCL is a sub-process, we need check if the main program is running
......if it's not, we need close and exit
*/
	if (NCL_subprocess!=0)
	{
/*
.....check the common memory to get NCL info
*/
		ret = uw_chkkey_common(keys);
/*
.....if there is no common memory now, it's mean main NCL is gone,
.....this NCL process have to forced to close
*/
		if (ret==0)
		{
/*
.....reset NCL_keyinfo to let function know it's forced out
*/
			strcpy_s(NCL_keyinfo, "00000000");
			OnClose();
			running = 0;
			return;
		}
/*
......if there is a common memory, check the main processid still running,
......if not, remove this common area and this NCL process have to forced to close
*/
//		HANDLE subncl = OpenProcess(PROCESS_ALL_ACCESS, 0, NCL_subprocess);
		HANDLE subncl = OpenProcess(PROCESS_QUERY_INFORMATION, 0, NCL_subprocess);
		ret = GetExitCodeProcess(subncl, &ecode);
		if ((ret!=0)&&(ecode!=STILL_ACTIVE))
		{
/*
.....reset NCL_keyinfo to let function know it's forced out
*/
			strcpy_s(NCL_keyinfo, "00000000");
			OnClose();
			running = 0;
			return;
		}
	}
/*
......this is a main NCL application, check if some sub-NCL closed and updated the
......NCL sy=ub process array
*/
	else
	{
		for (int i=0; i<m_sub_ncl;i++)
		{
			ret = GetExitCodeProcess(m_ncl_info[i].hProcess, &ecode);
			if ((ret!=0)&&(ecode!=STILL_ACTIVE))
			{
				remove_subproc(i);
				i--;
			}
		}
	}
	CFrameWnd::OnTimer(nIDEvent);
	running = 0;
}
void CMainFrame::remove_subproc(int sub)
{
	int i;
	for (i=sub; i<m_sub_ncl-1;i++)
	{
		m_ncl_info[i] = m_ncl_info[i+1];
	}
	m_sub_ncl--;
}
/***********************************************************************
**
**   FUNCTION: OnDropFiles(HDROP hDropInfo)
**
**		The framework calls this member function when the user releases 
**		the left mouse button over a NCL window as the recipient of dropped files.
**   
**	 INPUT:  hDropInfo: A pointer to an internal data structure that describes 
**			the dropped files. 
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CMainFrame::OnDropFiles(HDROP hDropInfo)
{
    UINT i;
    UINT nFiles = ::DragQueryFile(hDropInfo, (UINT) -1, NULL, 0);
/*
......only put one file
*/
	if (nFiles>1) nFiles = 1;
    for (i = 0; i < nFiles; i++)
    {
        char szFileName[_MAX_PATH];
        ::DragQueryFile(hDropInfo, i, szFileName, _MAX_PATH);
		StartNewNCL(szFileName);
    } 
    ::DragFinish(hDropInfo);
}
/***********************************************************************
c
c   FUNCTION: UpdatePOPMenu()
c
c           not used for now
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::UpdatePOPMenu(CPoint pt, char* input_text)
{
	int menunum1, itemnum1;
	char menudata[100];
	strcpy_s(menudata, sizeof(menudata), input_text);
	sscanf_s (menudata, "%d, %d", &menunum1, &itemnum1);
	UDM_menu_struc copystruc;
		
	uu_move_byte((char*)&(UDM_menu[menunum1].menus[itemnum1]), 
								(char*)&copystruc, 
								sizeof(UDM_menu_struc));
/*
......get the button position of the drop and insert this item into current
......menubar and update the menu bar
*/
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


/***********************************************************************
**
**   FUNCTION: GetSubNCL
**
**		Return the number of sub-NCL process running.
**   
**	 INPUT:  none
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CMainFrame::GetSubNCL()
{
	DWORD ecode;
	int i,ret;

	for (i=0; i<m_sub_ncl;i++)
	{
		ret = GetExitCodeProcess(m_ncl_info[i].hProcess, &ecode);
		if ((ret!=0)&&(ecode!=STILL_ACTIVE))
		{
			remove_subproc(i);
			i--;
		}
	}
	return m_sub_ncl;
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
	int data[6], stat;
	char *indx, buf[20];
	static int function_start = 0;
	MSG msg;
	int x = 0;

	if (function_start>0)
		return 1;
	while (function_start==0)
	{
		indx = buf;
		SiGetEventWinInit (&eData, WM_3DXWARE, wParam, lParam);

		if (SiGetEvent (m_DevHdl, 0, &eData, &event) == SI_IS_EVENT)
		{
			function_start = 1;
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
/*
.....eat this event
*/
					while(::PeekMessage(&msg, NULL, WM_3DXWARE, WM_3DXWARE, PM_REMOVE));
					break;

				case SI_ZERO_EVENT:
					num = event.type;
					break;

				case SI_BUTTON_EVENT:
					if ((num = SiButtonPressed (&event)) != SI_NO_BUTTON)	
					{
						char *p;
						p = ux_getenv("UZ_DEBUG_SMOUSE",UX_NPRTERRS);
						if (p != UU_NULL && strcmp(p,"TRUE") == 0)
						{
							sprintf(sbuf,"SpaceMouse Button_%d", num);
							ud_prmerr(sbuf);
						}
						event_num = num;
						data[0] = 0;
						data[1] = 0;
						data[2] = 0;
						data[3] = 0;
						data[4] = 0;
						data[5] = 0;
						stat = uz_user_smouse(event_num, data, &indx, 1);
	/*
	.....Das key function, need post the message
	*/
						if (stat == 1)
							PostMessage(ID_3DBUTTON_EVENT, (WPARAM)event_num, (LPARAM)0);
					}
					break;
				case SI_EXCEPTION_EVENT:
					x = 1;
					break;
				case SI_OUT_OF_BAND:
					x = 2;
					break;
				case SI_ORIENTATION_EVENT:
					x = 3;
					break;
				case SI_KEYBOARD_EVENT:
					x = 4;
					break;
				case SI_LPFK_EVENT:
					x = 5;
					break;
				case SI_APP_EVENT:
					x = 5;
					break;
				case SI_SYNC_EVENT:
					x = 5;
/*
.....eat this event
*/
					while(::PeekMessage(&msg, NULL, WM_3DXWARE, WM_3DXWARE, PM_REMOVE));
					break;
				case SI_BUTTON_PRESS_EVENT:
					x = 5;
					break;
				case SI_BUTTON_RELEASE_EVENT:
					x = 5;
					break;
				case SI_DEVICE_CHANGE_EVENT:
					x = 5;
					break;
				case SI_MOUSE_EVENT:
					x = 5;
					break;
				case SI_JOYSTICK_EVENT:
					x = 5;
					break;
				default:
					break;
			}
			function_start = 0;
		}
		else
			break;
	}
	function_start = 0;
	return (TRUE);
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

extern "C" void uw_ntnew_session()
{
	NCL_MainFrame->StartNewNCL();
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
	int* save_inptr;
	CString sav_cmdstr;
	UM_pkwin_type sarea;
	char *sav_hwnd;
	int jmpflag, sav_cur, sav_mode, ptype_saved, upload;
	int sav_ins;
	int num = id - (WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM);

	static int run_num = 0;
/*
......reset timer for menubar
*/
	if (UW_current_menubar!=NULL)
		UW_current_menubar->reset_timer();
/*
.....when we execute the NCL function, have to set focus back to main window
.....otherwise, the event window will go somewhere else sometimes (can't figure out why)
*/
	NCL_MainFrame->SetFocus();
	sav_cur = uw_ntgetcur_cursor();
	sav_mode = UD_pickmode;
	sarea = um_get_screen_area();
	ug_save_event();
	ug_save_input(&save_inptr);
	um_set_screen_area(UM_NCL_WINDOW);
	NCL_mouse_func = 0;
	sav_hwnd = UM_pocket_hwnd;
	UM_pocket_hwnd = 0;

	ptype_saved = 0;
	if (sav_mode==0)
/*
.....if picking already active, don't reset picking type
*/
	{
		ptype_saved = 1;
		save_ptype = ud_getpick_type();
		ud_setpick_type(UD_PICK_NORMAL);
	}
/*
.....if it is in picking mode but executed functions other than
.....'by text', 'by pick' and 'by location' function
.....we still need save and reset the picking type
*/
	else
	{
		if (!(uw_ntis_inputfunc(num)))
		{
			ptype_saved = 1;
			save_ptype = ud_getpick_type();
			ud_setpick_type(UD_PICK_NORMAL);
		}
	}
	sav_ins = insmode_val();
	if (! ( (uw_ntis_alphakey(num)) || (num==UZ_form_hotkey[0].key+1)
				|| (num==UZ_form_hotkey[1].key+1)) )
	{	
		savcmd_enable = NCL_MainFrame->Get_CmdEnable();
/*
.....save command text before disable it 
*/
		if (savcmd_enable==1)
		{
			NCL_MainFrame->GetCommand_Str(sav_cmdstr, line_num, upload);
/*
.....save text insert position too
*/
			uw_ntget_ecurpos(&sav_cmdcur1, &sav_cmdcur2);
/*
.....add line to handle insert mode
*/
			if (insmode_val()==2)
			{
				NCL_MainFrame->m_commandBar->handle_insln();
			}
			NCL_MainFrame->Enable_cmdbar(0);
			NCL_MainFrame->SetCommand_focus(0);
			NCL_MainFrame->SetCommand_Str("");
		}
	}
	NCL_MainFrame->SetTimer(2, 1000*10, NULL);
	sav_cmdmod = NCL_cmdmod;
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
		NCL_cmdmod = 0;
		uw_ntsetcursor(21);
		uz_wnt_callfunc(num,2);
	}
	NCL_cmdmod = sav_cmdmod;
	uw_ntsetcursor(sav_cur);
	if (!(uw_ntis_alphakey(num)))
	{	
		if (savcmd_enable==1)
		{
/*
......if Insert Mode change, don't reset command string, it is already handleed by
......Insert/Command mode function
*/
			if (sav_ins==insmode_val())
			{
				NCL_MainFrame->Enable_cmdbar(1);
				NCL_MainFrame->SetCommand_Str(sav_cmdstr, line_num, upload);
				NCL_MainFrame->SetCommand_focus(1);
				NCL_MainFrame->SetCommand_Insertpos(sav_cmdcur1, sav_cmdcur2);
			}
		}
	}
	UD_pickmode = sav_mode;
	ud_updatews(UG_SUPPRESS);
	ug_reset_event();
	ug_reset_input(save_inptr);
	if (ptype_saved)
		ud_setpick_type(save_ptype);
	um_set_screen_area(sarea);
	UM_pocket_hwnd = sav_hwnd;
	NCL_MainFrame->KillTimer(2);
	S_update_stat = 1;
	uz_status();
	S_update_stat = 0;
	UD_UNMARK(jmpflag);
}
/***********************************************************************
**
**   FUNCTION: uw_ntreset_spacemouse
**
**		reset space mouse.
**   
**	 INPUT:  none
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
extern "C" void uw_ntreset_spacemouse()
{
/*
.....close the space mouse first
*/
	if ( MagellanHandle != NULL )
	{
		MagellanClose( MagellanHandle );
		MagellanHandle = NULL;    
	};
/*
.....reset space mouse
*/
	MagellanHandle = MagellanInit( NCL_MainFrame->GetSafeHwnd() );
		
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

		NCL_MainFrame->PostMessage(MagellanHandle->MagellanMotionEvent,
					   wParam, lParam);
	}
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

/**********************************************************************
**    E_FUNCTION :  uw_setact_view()
**       Set the active view to the main window
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_setact_view()
{
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
	::SetActiveWindow(MainWin->GetSafeHwnd());
}

/***********************************************************************
**
**   FUNCTION: uw_ntreset_spacemouse
**
**		reset space mouse.
**   
**	 INPUT:  none
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
extern "C" void uw_ntclose_spacemouse()
{
	if ( MagellanHandle != NULL )
	{
		MagellanClose( MagellanHandle );
		MagellanHandle = NULL;    
	}
}

/***********************************************************************
**
**   FUNCTION: uw_ntset_livemouse
**
**		Set live mouse value.
**   
**	 INPUT:  none
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
extern "C" void uw_ntset_livemouse(int flag)
{
	UW_live_mouse = flag;
	if (UW_live_mouse==0)
	{
		UZ_key_pickloc = 1;
		NCL_mouse_func = 0;
	}
}

int CMainFrame::OpenFormDesgnDlg(int flag, char *filename)
{
	SetThemeAppProperties(UW_theme);
	CCreateContext context;
	context.m_pCurrentDoc = NULL;
	context.m_pNewViewClass = RUNTIME_CLASS(CNCLFdsnView);

	int nResponse = CNCLModalFrame::Run(*RUNTIME_CLASS(CNCLFdsnFrame),
					false, IDR_POPUPFRAME, filename, flag,
					WS_OVERLAPPEDWINDOW, this, 
					&context);
	if (nResponse == -1)
	{
		MessageBox("Error creating frame dialog", "Error", MB_OK);
		return -1;
	}
/*
......reset back
*/
	SetThemeAppProperties(STAP_ALLOW_NONCLIENT);
	return nResponse;
}
/*********************************************************************
**    I_FUNCTION     :  uw_form_desgn(int flag, char *filename)
**       main function for interactively define a form
**		
**    PARAMETERS   
**       INPUT  : 
**          flag: 0, general form design
**					1: Macro form design
**			filename: initial form file to be load in
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_form_desgn(int flag, char *filename)
{
	return NCL_MainFrame->OpenFormDesgnDlg(flag, filename);
}

/*********************************************************************
**    I_FUNCTION     :  CopyPropertyPage(CNCLFormProp *prop_dlg_from, CNCLFormProp *prop_dlg_to)
**      Copy a CNCLFormProp class value to another CNCLFormProp class value
**		
**    PARAMETERS   
**       INPUT  : 
**          prop_dlg_from: class to be copy from
**			prop_dlg_to: class to be copy to
**
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CopyPropertyPage(CNCLFormProp *prop_dlg_from, CNCLFormProp *prop_dlg_to)
{
	prop_dlg_to->CopyPropertyPage(prop_dlg_from);
}

void CMainFrame::ReDisplayStatusBar()
{
	m_bStatusBar.RemoveAllPanes();
	if (UDM_layout.statbar_no==0)
	{
		m_bStatusBar.ShowWindow(SW_HIDE);
	}
	else
	{
		m_bStatusBar.ShowWindow(SW_SHOW);
		DisplayStatusBar();
	}
}
/***********************************************************************
**
**   FUNCTION: OnMouseMove(UINT nFlags, CPoint pt)
**
**       mouse move callback
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CMainFrame::OnMouseMove(UINT nFlags, CPoint point) 
{	
	CFrameWnd::OnMouseMove(nFlags, point);
	m_bStatusBar.OnItemMouseMove(nFlags, point);
}

void CMainFrame::DisplayStatusBar()
{
	int idx;
	CRect rect;	
	if (UDM_layout.statbar_no==0)
		return;
	GetWindowRect(&rect);
	rect.right = rect.right - 2*GetSystemMetrics(SM_CXFRAME) - 2;
	float wid_ratio;
	int i, wid;
	if (UDM_layout.statbar_no>0)
	{	
		wid_ratio = 1.0/UDM_layout.statbar_no;
		idx = m_bStatusBar.CommandToIndex(INDICATOR_WIN1);
		wid = (rect.Width()-20)*wid_ratio - 1;
		m_bStatusBar.SetPaneWidth(idx, wid);
		m_bStatusBar.SetPaneStyle(idx, m_bStatusBar.GetPaneStyle(idx) | SBPS_NOBORDERS );
		m_bStatusBar.SetPaneText(idx, "");
		m_bStatusBar.loadstatbut(UDM_layout.statbar_file[0], INDICATOR_WIN1);
		for (i=1; i<UDM_layout.statbar_no;i++)
		{
			m_bStatusBar.AddIndicator(i, INDICATOR_WIN1+i);
			idx = m_bStatusBar.CommandToIndex(INDICATOR_WIN1+i);
			m_bStatusBar.SetPaneWidth(idx, wid);
			m_bStatusBar.SetPaneStyle(idx, m_bStatusBar.GetPaneStyle(idx) | SBPS_NOBORDERS );
			m_bStatusBar.SetPaneText(idx, "");
			m_bStatusBar.loadstatbut(UDM_layout.statbar_file[i], INDICATOR_WIN1+i);
		}
	}
}
/***********************************************************************
c
c   FUNCTION: OnUpdateStatusbar()
c
c           updated current drag/drop status bar
c			the statusbar is accept drop
c
c   INPUT:  
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnUpdateStatusbar()
{
	int menu1, item1, menu2, item2;
	if (UW_remove_menu==-1000)
	{
/*
......remove the whole status bar
*/
		UDM_layout.statbar_no = 0;
		ReDisplayStatusBar();
		RecalcLayout();
		return;
	}
	menu1 = UW_addmenu;
	item1 = UW_additem;
	menu2 = UW_remove_menu;
	item2 = UW_remove_item;
	if (menu1!=-1)
	{
		if (UDM_menu[menu1].statflag==2)
		{
			m_bStatusBar.Redisppane(menu1);
		}
	}
	if (menu2!=-1)
	{
		if (UDM_menu[menu2].statflag==2)
		{
			if (menu2!=menu1)
			{
				if (UDM_menu[menu2].num==0)
					m_bStatusBar.RemovePaneIndx(menu2);
				else
					m_bStatusBar.Redisppane(menu2);
			}
			goto done;
		}
		if (UDM_menu[menu2].statflag==1)
		{
			if (menu2!=menu1)
			{
				if (NCL_menubar[menu2] != NULL)
					redisp_menubar(menu2);
			}
			goto done;
		}
		if (UDM_menu[menu2].statflag==0)
		{
			if (NCL_menubar[menu2] != NULL)
			{
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
	}
done:;
	m_addmenu = -1;
	m_remove_menu = -1;
	m_bStatusBar.m_changed = 1;
/*
.....display status text also
*/
	S_update_stat = 1;
	uz_status();
	S_update_stat = 0;
}

extern "C" void uw_set_update_stat(int flag)
{
	S_update_stat = flag;
}
