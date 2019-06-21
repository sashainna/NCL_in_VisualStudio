/************************************************************************
c
c   FILE NAME: wsntmenu.cpp
c
c	 CONTAINS: 
c	 all dynamic menu, interface *.cpp functions and 
c			Implementation functions
c			uw_ntget_menurect
c			uw_ntmenu
**			uw_ntchoice_menu
**			uw_ntmenu_reset
**			uw_ntdown_menu
**			uw_display_dragmenu
**			uw_ntmenu_redisp
**			uw_ntpopchoice
**			uw_ntget_menunum
**			uw_redisplay_dragmenu
**			uw_redisplay_popmenu
**			uw_ntremove_menu
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       wsntmenu.cpp , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 16:54:27
c
c**********************************************************************
*/
#include "wsntstdafx.h"
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string>
#include <iostream>

#include "wsntframe.h"
#include "wsntpmenu.h"
#include "wsntres.h"
#include "wsntfuncid.h"
#include "wsntbitmap.h"
#include "dmotif.h"
#include "xenv1.h"
#include "wsntcfunc.h"
#include "wsntglfunc.h"
#include "wsntres.h"
#include "zkeysym.h"

#define SEP_LEN 8
#define NBUTTONNUM	5000
#define NCHOICEBUTTON 1000

int NCL_PupupChoice = -1;
int UW_reset_menu = 1;
CNCLMenu *UW_topmenu = NULL;
extern "C" int UW_icon_size, UW_menu_fmt;
extern CMainFrame *NCL_MainFrame;
extern HWND UW_TrackingWindow;
extern HMENU UW_TrackingMenu;
extern "C" int uw_ntcnvtfunc_to_id(char* func, UINT &fid);
extern "C" int uw_ntshow_rows(int menu_pos[3]);
extern "C" int uz_ntcnvtfunc_to_num(char* func, char *parms, char *descrip, int *num);

extern void uw_ntget_dockrect(CRect *rect, int flag);
extern int uw_ntupd_menuarea(CToolmenu* pBar,char *menu_area);
extern "C" int uw_ntget_gwsize(int *wid, int *hgt);

extern "C" int ud_get_areadockable(char *area_name, int *dockable);

extern void uw_getdock_barrect(CRect *rect, int *newrow, int flag, int bflag);
extern void uw_nthide_bars(int bar_pos);
extern void uw_ntadd_menuborder(CRect *rect);
extern "C" int uz_ntstatname_to_num(char *name, char *func, char *descrip, int* num, int *sub)	;
extern "C" int uz_putid_stattb(int fid, int sub);
extern "C" int uw_ntshow_dir(int menu_pos);
extern "C" int uz_status();
extern "C" int uz_ntmenunum_to_num(int current_menu, int *bnum);

extern int frm_done;
extern "C"  int UW_auto_cursor;
extern "C" int uw_ntset_curpos(int x, int y);
extern "C" int uz_load_keys2(char *keyfile, int flag);
extern "C" int uz_load_accel2(char *keyfile);
extern "C" int ncl_set_statact(int);
extern "C" char * uu_malloc(int);
extern "C" void uu_free(char*);
extern CNCLToolBar *UW_current_menubar;

int UW_NPopup = -1;
static int PopupWin[20];
int PopupChoice[20] = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
				-1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
/**********************************************************************
**    I_FUNCTION : uw_create_iconimg()
**       this function create an icon image for menu
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : the icon image pointer
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
CImageList * uw_create_iconimg()
{
	CImageList *rimglist = new CImageList();
	if (UW_icon_size==0)
		rimglist->Create(16,16, ILC_COLOR16|ILC_MASK, 0, 1000);
	else if (UW_icon_size==1)
		rimglist->Create(24,24, ILC_COLOR16|ILC_MASK, 0, 1000);
	else if (UW_icon_size==2)
		rimglist->Create(32,32, ILC_COLOR16|ILC_MASK, 0, 1000);
	else if (UW_icon_size==3)
		rimglist->Create(40,40, ILC_COLOR16|ILC_MASK, 0, 1000);
	else if (UW_icon_size==4)
		rimglist->Create(48,48, ILC_COLOR16|ILC_MASK, 0, 1000);
	else
		rimglist->Create(16,16, ILC_COLOR16|ILC_MASK, 0, 1000);
	return rimglist;
}
/**********************************************************************
**    I_FUNCTION : uw_ntget_menurect(int bar_pos[5], CRect *rect)
**       this function Get the control bar size 
**
**    PARAMETERS   
**       INPUT  : 
**				bar_pos: default bar position
**				dockable: dockable style: 
**					UDM_AREA_FRONTDOCK: docked at the front of current bar
**					UDM_AREA_ENDDOCK: docked at the end of current bar
**					UDM_AREA_NODOCK: replace the current bar
**       OUTPUT : 
**				rect: the actural control bar position & size (screen position)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_ntget_menurect(int bar_pos, CRect *rect, int dockable)
{
	int newrow;
	RECT *rect1;
	CRect brect;
	
	newrow = 0;

	if (dockable==UDM_AREA_FRONTDOCK)
		uw_getdock_barrect(rect, &newrow, bar_pos, 1);	
	else if (dockable==UDM_AREA_ENDDOCK)
		uw_getdock_barrect(rect, &newrow, bar_pos, 2);	
	else if (dockable==UDM_AREA_NODOCK)
		uw_ntget_dockrect(rect, bar_pos);	
	else
		uw_getdock_barrect(rect, &newrow, bar_pos, 1);	

	if (((bar_pos==1)||(bar_pos==2))&&(newrow==0))
	{
		rect1 = (LPRECT)rect;
		if (dockable==UDM_AREA_FRONTDOCK)
			rect->SetRect(rect1->left-1, rect1->top, rect1->left+10, rect1->bottom);
		else if (dockable==UDM_AREA_ENDDOCK)
			rect->SetRect(rect1->right-10, rect1->top, rect1->right+1, rect1->bottom);
		else if (dockable!=UDM_AREA_NODOCK)
			rect->SetRect(rect1->left-1, rect1->top, rect1->left+10, rect1->bottom);
	}
	if (((bar_pos==3)||(bar_pos==4))&&(newrow==0))
	{
		rect1 = (LPRECT)rect;
		if (dockable==UDM_AREA_FRONTDOCK)
			rect->SetRect(rect1->left, rect1->top-1, rect1->right, rect1->top+10);
		else if (dockable==UDM_AREA_ENDDOCK)
			rect->SetRect(rect1->left, rect1->bottom-10, rect1->right, rect1->bottom+1);
		else if (dockable!=UDM_AREA_NODOCK)
			rect->SetRect(rect1->left, rect1->top-1, rect1->right, rect1->top+10);
	}
	if (((bar_pos==1)||(bar_pos==2))&&(newrow==1))
	{
		rect1 = (LPRECT)rect;
		if (dockable==UDM_AREA_FRONTDOCK)
			rect->SetRect(rect1->left-1, rect1->top, rect1->left+10, rect1->top + 10);
		else if (dockable==UDM_AREA_ENDDOCK)
			rect->SetRect(rect1->left-1, rect1->bottom-10, rect1->left+10, rect1->bottom+1);
		else if (dockable!=UDM_AREA_NODOCK)
			rect->SetRect(rect1->left-1, rect1->top, rect1->left+10, rect1->top + 10);
	}
	if (((bar_pos==3)||(bar_pos==4))&&(newrow==1))
	{
		rect1 = (LPRECT)rect;
		if (dockable==UDM_AREA_FRONTDOCK)
			rect->SetRect(rect1->left, rect1->top-1, rect1->left+10, rect1->top+10);
		else if (dockable==UDM_AREA_ENDDOCK)
			rect->SetRect(rect1->right-10, rect1->top-1, rect1->right+1, rect1->top+10);
		else if (dockable!=UDM_AREA_NODOCK)
			rect->SetRect(rect1->left, rect1->top-1, rect1->left+10, rect1->top+10);
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_ntmenu(kinc,kdis)
**       Activates a menu.
**    PARAMETERS   
**       INPUT  : 
**          kinc    = Menu number to activate.
**			kdis    = 0 = Define the menu only, but do not manage it.
**				      1 = display the menu.
**       OUTPUT :  
**          NONE
**    RETURNS      : -1: FAILED to display menu
**						0: menu displayed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntmenu(int kinc,int kdis)
{
	int i, k, m, current_menu,toolnum, parent, subnum;
	char tempstr[200];
	UINT fid;
	int bnum, status, defnum, sub;
	HBITMAP hBmap, hchkBmap = NULL;
	HBITMAP defhBmap = NULL;
	int defload = 0;
	int bmpnum[100];
	POINT pt;
	int menu_pos[5], menu_size[2], mtype;
	UX_pathname filename,dir;
	char fname[UX_MAX_FILE_LEN];
	CRect rect;
	int sizex, sizey;	
	MENUINFO info;

	HDC thdc = NCL_MainFrame->GetDC()->m_hDC;
	int stat = 0;

	UDM_menu[kinc].drawitem = 1;
/*
.....when it is bottom status bar, it handle differently
*/
	if (UDM_menu[kinc].statflag==2)
		return 0;
	if (kdis)
	{
/*		if (UDM_menu[kinc].key_loaded==0)*/
		{
			UDM_menu[kinc].key_loaded = 1;
			if (UDM_menu[kinc].keyfile[0]!='\0')
			{
				uz_load_keys2(UDM_menu[kinc].keyfile, 0);
				uz_load_accel2(UDM_menu[kinc].keyfile);
			}
		}
	}

	if (NCL_menu[kinc]!=NULL)
	{
		if (kdis==1)
		{
			if ((UDM_menu[kinc].type == UDM_MTYPE_PULLDOWN)
					|| (UDM_menu[kinc].type == UDM_MTYPE_POPUP))
			{
				GetCursorPos(&pt);
				if (UW_reset_menu==0)
				{
					UW_TrackingWindow = NCL_MainFrame->GetSafeHwnd();
					UW_TrackingMenu = NCL_menu[kinc]->m_hMenu;
					NCL_menu[kinc]->TrackPopupMenu(TPM_LEFTALIGN,
						pt.x, pt.y, NCL_MainFrame, NULL);
					UW_TrackingWindow = NULL;
					if (UW_current_menubar!=NULL)
						UW_current_menubar->reset_timer();
				}
			}
			return 0;
		}
	}


	int dockable;
	UINT tearID;
	if ((NCL_MainFrame->NCL_menubar[kinc]!=NULL)
		&&((NCL_MainFrame->NCL_menubar[kinc])->m_created==1))
	{
		if (kdis==1)
		{
			if ((NCL_MainFrame->NCL_menubar[kinc])->m_visible==1)
			{
				if (UW_auto_cursor)
				{
					(NCL_MainFrame->NCL_menubar[kinc])->GetWindowRect(&rect);			
					uw_ntset_curpos(rect.left + rect.Width()/2, rect.top + rect.Height()/2);
				}
				return 0;
			}
			if (UDM_menu[kinc].type == UDM_MTYPE_ICON)
			{
				if ((UW_reset_menu==1) ||
					((UW_reset_menu==0) && ((NCL_MainFrame->NCL_menubar[kinc])->IsFloating()==0))
					|| (((NCL_MainFrame->NCL_menubar[kinc])->m_changed==0)&&(UW_reset_menu==0) && ((NCL_MainFrame->NCL_menubar[kinc])->IsFloating()==1)))
				{
					ud_getpos_from_bararea(UDM_menu[kinc].statflag, UDM_menu[kinc].file, UDM_menu[kinc].menu_area, menu_pos, menu_size);
					if ((menu_pos[2]==1)||(menu_pos[2]==2))	
						NCL_MainFrame->NCL_menubar[kinc]->SetColumns(UDM_menu[kinc].cols, 0);			
					if ((menu_pos[2]==3)||(menu_pos[2]==4))	
						NCL_MainFrame->NCL_menubar[kinc]->SetColumns(UDM_menu[kinc].cols, 2);							
					if (UW_reset_menu==1)
						dockable = UDM_AREA_ENDDOCK;
					else
						ud_get_areadockable(UDM_menu[kinc].menu_area, &dockable);
					uw_ntget_menurect(menu_pos[2], &rect, dockable);
	
					if (dockable==UDM_AREA_NODOCK)
/*
......replace/hide the control bars inside this area
*/
						uw_nthide_bars(menu_pos[2]);

					(NCL_MainFrame->NCL_menubar[kinc])->EnableDocking(CBRS_ALIGN_ANY);
					NCL_MainFrame->EnableDocking(CBRS_ALIGN_ANY);
					if (menu_pos[2]==1)
						NCL_MainFrame->DockControlBar(NCL_MainFrame->NCL_menubar[kinc], AFX_IDW_DOCKBAR_TOP, &rect);
					else if (menu_pos[2]==2)
						NCL_MainFrame->DockControlBar(NCL_MainFrame->NCL_menubar[kinc], AFX_IDW_DOCKBAR_BOTTOM , &rect);
					else if (menu_pos[2]==3)
						NCL_MainFrame->DockControlBar(NCL_MainFrame->NCL_menubar[kinc], AFX_IDW_DOCKBAR_LEFT, &rect);
					else if (menu_pos[2]==4)
					{
						NCL_MainFrame->DockControlBar(NCL_MainFrame->NCL_menubar[kinc], AFX_IDW_DOCKBAR_RIGHT, &rect);
					}
					(NCL_MainFrame->NCL_menubar[kinc])->SetPos(menu_pos);
					NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[kinc],
						TRUE, FALSE);
					(NCL_MainFrame->NCL_menubar[kinc])->m_visible = 1;
					NCL_MainFrame->RecalcLayout();
				}
				else if ((NCL_MainFrame->NCL_menubar[kinc])->IsFloating()==1)
				{
					NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[kinc],
						TRUE, FALSE);
					(NCL_MainFrame->NCL_menubar[kinc])->m_visible = 1;
				}
			}
			else
			{
				NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[kinc],
					TRUE, FALSE);
				(NCL_MainFrame->NCL_menubar[kinc])->m_visible = 1;
			}
			if ((UW_auto_cursor)&&((NCL_MainFrame->NCL_menubar[kinc])->m_visible==1))
			{
				if (UW_auto_cursor)
				{
					(NCL_MainFrame->NCL_menubar[kinc])->GetWindowRect(&rect);			
					uw_ntset_curpos(rect.left + rect.Width()/2, rect.top + rect.Height()/2);
				}
			}
/*
.....if it is statusbar, display status text also
*/
			if (UDM_menu[kinc].statflag==1)
				uz_status();
		}
		else
		{
			NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[kinc],
				FALSE, FALSE);
			(NCL_MainFrame->NCL_menubar[kinc])->m_visible = 0;
		}
		NCL_MainFrame->RecalcLayout();
		return 0;
	}
			
	if (UDM_menu[kinc].type == UDM_MTYPE_MENUBAR)
	{
		if (UW_topmenu==NULL)
		{
			NCL_MainFrame->SetMenu(NULL);
			UW_topmenu = new CNCLMenu();
			UW_topmenu->CreateMenu();
		}
		else
		{
/*
.....at most 20 menu for menubar
*/
			for (i=0; i<20; i++)
			{
				status = UW_topmenu->DeleteMenu(0, MF_BYPOSITION);
				if (status==0)
					break;
			}
		}
	}
	else if ((UDM_menu[kinc].type == UDM_MTYPE_PULLDOWN)
			|| (UDM_menu[kinc].type == UDM_MTYPE_POPUP))
	{
		parent = -1;
		subnum = -1;
		if (NCL_menu[kinc]!=NULL)
		{
/*
......before delete this menu, we need release from it's parent menu
*/
			if (NCL_menu[kinc]->m_parent!=-1)
			{
				parent = NCL_menu[kinc]->m_parent;
				subnum = NCL_menu[kinc]->m_subnum;
				if (NCL_menu[parent]!=NULL)
					NCL_menu[parent]->RemoveMenu((UINT)(NCL_menu[kinc]->m_hMenu), MF_BYCOMMAND);
			}
			delete NCL_menu[kinc];
		}
		NCL_menu[kinc] = new CNCLMenu();
		NCL_menu[kinc]->CreatePopupMenu();
		NCL_menu[kinc]->SetIdnum(kinc);

		info.cbSize = sizeof(MENUINFO);
		stat = NCL_menu[kinc]->GetMenuInfo(&info);
		stat = MNS_DRAGDROP;
		info.dwStyle &= ~(MNS_DRAGDROP);
		info.dwStyle |= MNS_DRAGDROP;
		info.fMask = MIM_APPLYTOSUBMENUS | MIM_STYLE;
		NCL_menu[kinc]->SetMenuInfo(&info);
		if (parent!=-1)
		{
			NCL_menu[parent]->InsertMenu(subnum+1, MF_OWNERDRAW|MF_STRING|MF_ENABLED|MF_POPUP|MF_BYPOSITION, 
							(UINT)(NCL_menu[kinc]->m_hMenu),
							UDM_menu[parent].menus[subnum].name);
			NCL_menu[kinc]->m_parent = parent;
			NCL_menu[kinc]->m_subnum = subnum;			
			NCL_menu[kinc]->SetMenuInfo(&info);
			fid = (UINT)(NCL_menu[kinc]->m_hMenu);
			NCL_menu[parent]->Set_mid(subnum, fid);
		}
	}
	else if ((UDM_menu[kinc].type == UDM_MTYPE_MENU) 
		|| (UDM_menu[kinc].type == UDM_MTYPE_ICON))
	{
		mtype = 2;
		for (int i=0; i<UDM_menu[kinc].num; i++)
		{
			if ((UDM_menu[kinc].menus[i].bmpfile[0]!='\0')
					&&(UDM_menu[kinc].menu_format!=1))
			{
				mtype = 1;
				break;
			}
		}
		NCL_MainFrame->NCL_menubar[kinc] = new CToolmenu(kinc, mtype);
		NCL_MainFrame->NCL_menubar[kinc]->m_created = 1;
		UINT type;
		if (UDM_menu[kinc].statflag==0)
		{
			type = WS_CHILD | WS_VISIBLE | CBRS_SIZE_DYNAMIC |
						 CBRS_TOOLTIPS ;
		}
		else
		{
			type = WS_CHILD | WS_VISIBLE | CBRS_SIZE_FIXED |
						 CBRS_TOOLTIPS ;
		}
/*
.....if it is the ICOM menu, get the position from
.....layout files
*/
		if (UDM_menu[kinc].statflag==0)
		{
			(NCL_MainFrame->NCL_menubar[kinc])->SetButtype(1);
			(NCL_MainFrame->NCL_menubar[kinc])->SetBarType(0);
		}
		else
		{
			(NCL_MainFrame->NCL_menubar[kinc])->SetButtype(2);
			(NCL_MainFrame->NCL_menubar[kinc])->SetBarType(1);
		}

		if (UDM_menu[kinc].type == UDM_MTYPE_ICON)
		{
			ud_getpos_from_bararea(UDM_menu[kinc].statflag, UDM_menu[kinc].file, UDM_menu[kinc].menu_area, menu_pos, menu_size);
			if (menu_pos[2]==1)
				type = type | CBRS_TOP;
			else if (menu_pos[2]==2)
				type = type | CBRS_BOTTOM;
			else if (menu_pos[2]==3)
				type = type | CBRS_LEFT;
			else if (menu_pos[2]==4)
				type = type | CBRS_RIGHT;
		}
		else
		{
			type = type | CBRS_TOP ;
		}
		if (!(NCL_MainFrame->NCL_menubar[kinc]->Create(NCL_MainFrame, 
				type, IDC_NCL_MENUBAR)))
		{
			MessageBox(NULL, "Failed to create menubar", "Warning", MB_ICONINFORMATION|MB_OK);
			return -1;      
		}
	}

	if (UDM_menu[kinc].type == UDM_MTYPE_MENUBAR)
	{
		for (k=0;k<UDM_menu[kinc].num;k++)
		{
			current_menu = UDM_menu_count;
			if (udm_read_menu(UDM_menu[kinc].menus[k].file,
					UDM_menu[kinc].menus[k].pos,
					UDM_menu[kinc].menus[k].size, 
					0, 0, UDM_MTYPE_PULLDOWN) != 0)
			{
				current_menu = -1;
			}
			if (current_menu!=-1)
				UW_topmenu->AppendMenu(MF_STRING|MF_ENABLED|MF_POPUP, 
						(UINT)(NCL_menu[current_menu]->m_hMenu), UDM_menu[kinc].menus[k].name);
		}
		info.cbSize = sizeof(MENUINFO);
		stat = UW_topmenu->GetMenuInfo(&info);
		info.dwStyle &= ~(MNS_DRAGDROP);
		info.dwStyle |= MNS_DRAGDROP;
		info.fMask = MIM_APPLYTOSUBMENUS | MIM_STYLE;

		UW_topmenu->SetMenuInfo(&info);
		NCL_MainFrame->SetMenu(UW_topmenu);
		return 0;
	}
	if ((UDM_menu[kinc].type == UDM_MTYPE_PULLDOWN)
			|| (UDM_menu[kinc].type == UDM_MTYPE_POPUP))
	{
/*
.....added drag button
*/
		tearID = WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM + kinc;
		NCL_menu[kinc]->InsertMenu(-1, MF_STRING|MF_OWNERDRAW|MF_BYPOSITION,tearID, " ");
		for (k=0;k<UDM_menu[kinc].num;k++)
		{
			if (UDM_menu[kinc].menus[k].separator==0)
			{
				stat = uz_ntcnvtfunc_to_num(UDM_menu[kinc].menus[k].file, 
						UDM_menu[kinc].menus[k].params, 
						UDM_menu[kinc].menus[k].descrip, &bnum);
				fid = bnum + WM_APP + UDM_MAX_MENU + UZ_MAX_KEYITEM;
				if (stat==-1)
/*
.....then it must be a menu name
.....load this menu
*/
				{
/*
.....See if this menu is already loaded
*/
					for (m=0;m<UDM_menu_count;m++)
					{
						strcpy(filename, UDM_menu[m].file);
						ul_break_fname(filename,dir,fname);

						if (strcmp(fname, UDM_menu[kinc].menus[k].file) == 0)
						{
							current_menu = m;
							break;
						}
					}
/*
.....not loaded, load it
*/
					if (m >= UDM_menu_count)
					{
						current_menu = UDM_menu_count;
						if (udm_read_menu(UDM_menu[kinc].menus[k].file,
									UDM_menu[kinc].menus[k].pos,
									UDM_menu[kinc].menus[k].size, 
									0, 0, -1) != 0)
						{
							current_menu = -1;
						}
					}
/*
......only if the will be AppendMenu is the "PULLDOWN" type 
......of the menu, we Append this menu
*/
					if (UDM_menu[current_menu].type == UDM_MTYPE_PULLDOWN)
					{
						if (current_menu!=-1)
						{
							NCL_menu[kinc]->AppendMenu(MF_OWNERDRAW|MF_STRING|MF_ENABLED|MF_POPUP, 
								(UINT)(NCL_menu[current_menu]->m_hMenu), UDM_menu[kinc].menus[k].name);
							fid = (UINT)(NCL_menu[current_menu]->m_hMenu);
							NCL_menu[current_menu]->m_parent = kinc;
							NCL_menu[current_menu]->m_subnum = k;
						}
						else
						{
							fid = IDF_KEY_NOOP;
							NCL_menu[kinc]->AppendMenu(MF_OWNERDRAW|MF_STRING|MF_ENABLED|MF_POPUP, 
									fid, UDM_menu[kinc].menus[k].name);
						}
						if (UDM_menu[kinc].menu_format!=1)
						{
							if ((k==0)&&(UDM_menu[kinc].bmpfile[0]!='\0'))
							{
								NCL_menu[kinc]->m_hCBitmap = uw_get_bitmap(UDM_menu[kinc].bmpfile, thdc,-2);
								if (NCL_menu[kinc]->m_hCBitmap!=NULL)
									defload = 1;
								else
									defload = 0;
							}
							if (UDM_menu[kinc].menus[k].bmpfile[0]!='\0')
							{
								if (strcmp(UDM_menu[kinc].menus[k].bmpfile, 
											UDM_menu[kinc].bmpfile)!=0)
								{
									hBmap = uw_get_bitmap(UDM_menu[kinc].menus[k].bmpfile, thdc,-2);
									if (hBmap==NULL)
										bmpnum[k] = -1;
									else
										bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
								}
								else
								{
									if (defload==0)
										bmpnum[k] = -1;
									else
										bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
									hBmap = NULL;
								}
							}
							else
							{
								hBmap = NULL;
								bmpnum[k] = -1;
							}
						}
						else
						{
							hBmap = NULL;
							hchkBmap = NULL;
							bmpnum[k] = -1;
						}
						NCL_menu[kinc]->SetBitmap(hBmap,hchkBmap, k, fid, bmpnum[k]);
						NCL_menu[kinc]->Set_mid(k, fid);
					}
					else
/*
......otherwise, set the callback id, when this callback called
......show current_menu (using TrackPopupMenu for "POPUP"
......menu, using ShowControlBar for "ICON", "MENU" menu)
*/
					{
						stat = uz_ntmenunum_to_num(current_menu, &bnum);
						fid = bnum + WM_APP;
						if (stat==-1)
							fid = IDF_KEY_NOOP;

						if (UDM_menu[kinc].menu_format!=1)
						{
							if ((k==0)&&(UDM_menu[kinc].bmpfile[0]!='\0'))
							{
								NCL_menu[kinc]->m_hCBitmap = uw_get_bitmap(UDM_menu[kinc].bmpfile, thdc,-2);
								if (NCL_menu[kinc]->m_hCBitmap!=NULL)
									defload = 1;
								else
									defload = 0;
							}
							if (UDM_menu[kinc].menus[k].bmpfile[0]!='\0')
							{
								if (strcmp(UDM_menu[kinc].menus[k].bmpfile, 
											UDM_menu[kinc].bmpfile)!=0)
								{
									hBmap = uw_get_bitmap(UDM_menu[kinc].menus[k].bmpfile, thdc, -2);
									if (hBmap==NULL)
										bmpnum[k] = -1;
									else
										bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
								}
								else
								{
									if (defload==0)
										bmpnum[k] = -1;
									else
										bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
									hBmap = NULL;
								}
							}
							else
							{
								hBmap = NULL;
								bmpnum[k] = -1;
							}
						}
						else
						{
							hBmap = NULL;
							hchkBmap = NULL;
							bmpnum[k] = -1;
						}
						NCL_menu[kinc]->SetBitmap(hBmap,hchkBmap, k, fid, bmpnum[k]);
						NCL_menu[kinc]->Set_mid(k, fid);
						NCL_menu[kinc]->AppendMenu(MF_OWNERDRAW|MF_STRING|MF_ENABLED|MF_POPUP, 
								fid, UDM_menu[kinc].menus[k].name);
					}
				}
				else
				{
					if (UDM_menu[kinc].menu_format!=1)
					{
						if ((k==0)&&(UDM_menu[kinc].bmpfile[0]!='\0'))
						{
							NCL_menu[kinc]->m_hCBitmap = uw_get_bitmap(UDM_menu[kinc].bmpfile, thdc, -2);
							if (NCL_menu[kinc]->m_hCBitmap!=NULL)
								defload = 1;
							else
								defload = 0;
						}
						if (UDM_menu[kinc].menus[k].bmpfile[0]!='\0')
						{
							if (strcmp(UDM_menu[kinc].menus[k].bmpfile, 
										UDM_menu[kinc].bmpfile)!=0)
							{
								hBmap = uw_get_bitmap(UDM_menu[kinc].menus[k].bmpfile, thdc,-2);
								if (hBmap==NULL)
									bmpnum[k] = -1;
								else
									bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
							}
							else
							{
								if (defload==0)
									bmpnum[k] = -1;
								else
									bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
								hBmap = NULL;
							}
						}
						else
						{
							hBmap = NULL;
							bmpnum[k] = -1;
						}
					}
					else
					{
						hBmap = NULL;
						hchkBmap = NULL;
						bmpnum[k] = -1;
					}
					NCL_menu[kinc]->SetBitmap(hBmap,hchkBmap, k, fid, bmpnum[k]);
					NCL_menu[kinc]->Set_mid(k, fid);
					NCL_menu[kinc]->InsertMenu(-1, MF_STRING|MF_OWNERDRAW|MF_BYPOSITION,fid, 
							UDM_menu[kinc].menus[k].name);
				}
			}
			else
			{
				NCL_menu[kinc]->InsertMenu(-1, MF_SEPARATOR|MF_BYPOSITION);
				NCL_menu[kinc]->Set_mid(k, 0);
			}
		}
		if (kdis==1)
		{
			GetCursorPos(&pt);
			if (UW_reset_menu==0)
			{
				UW_TrackingWindow = NCL_MainFrame->GetSafeHwnd();
				UW_TrackingMenu = NCL_menu[kinc]->m_hMenu;
				NCL_menu[kinc]->TrackPopupMenu(TPM_LEFTBUTTON|TPM_LEFTALIGN,
					pt.x, pt.y, NCL_MainFrame, NULL);
				UW_TrackingWindow = NULL;
				if (UW_current_menubar!=NULL)
					UW_current_menubar->reset_timer();
			}
		}
		return 0;
	}
	if ((UDM_menu[kinc].type == UDM_MTYPE_MENU)
		|| (UDM_menu[kinc].type == UDM_MTYPE_ICON))
	{
		toolnum = UDM_menu[kinc].num;
		NCL_MainFrame->NCL_menubar[kinc]->SetButtons(NULL, toolnum);
/*
.....Create Imagelist for menubar
*/
		CBitmap *rpImage;
		CBitmap cBmap;
		int rpImgnum;
		CBitmap *defImage;
		CBitmap defcBmap;
		int defImgnum;
		CImageList *rimglist = NULL;
		int first_image = 1;
		CToolBarCtrl &rctrl = (NCL_MainFrame->NCL_menubar[kinc])->GetToolBarCtrl();
		for (i=0; i<toolnum; i++)
		{
			if ((UDM_menu[kinc].menus[i].bmpfile[0]!='\0')
				&&(UDM_menu[kinc].menu_format!=1)
				&&(UDM_menu[kinc].statflag==0))
			{
				if (rimglist==NULL)
				{
					rimglist = uw_create_iconimg();
					rctrl.SetImageList(rimglist);
				}
				if ((first_image)&&(UDM_menu[kinc].bmpfile[0]!='\0'))
				{
					first_image = 0;
					defhBmap = uw_get_bitmap(UDM_menu[kinc].bmpfile, thdc, -1);
					defImage = defcBmap.FromHandle(defhBmap);
					defImgnum = rimglist->Add(defImage, 0xC0C0C0);
				}
				if (strcmp(UDM_menu[kinc].menus[i].bmpfile, 
						UDM_menu[kinc].bmpfile)!=0)
				{
					hBmap = uw_get_bitmap(UDM_menu[kinc].menus[i].bmpfile, thdc, -1);
					rpImage = cBmap.FromHandle(hBmap);
					rpImgnum = rimglist->Add(rpImage, 0xC0C0C0);
					bmpnum[i] = rpImgnum + UDM_menu[kinc].menus[i].bmpnum;
				}
				else
					bmpnum[i] = defImgnum + UDM_menu[kinc].menus[i].bmpnum;
			}
			else
				bmpnum[i] = -1;
		}
/*
.....structure for icon/text mixed menu
*/
		struct tbblabel *labels = new struct tbblabel[toolnum+1];
/*
.....assume icon only
*/
		NCL_MainFrame->NCL_menubar[kinc]->m_mtype = 1;
		for (i=0; i<toolnum; i++)
		{
/*
.....load in choice as popup menu
.....then save this menu index number into 
.....CNCLToolbar data member m_dropID
*/
			if (UDM_menu[kinc].menus[i].chcfile[0] != '\0')
			{
				current_menu = UDM_menu_count;
				stat = ud_ntload_toggle(kinc, i);
				defnum = UDM_menu[kinc].menus[i].chcdef;
				if (stat==0)
					(NCL_MainFrame->NCL_menubar[kinc])->SetDropId(i, current_menu);
				if ((UDM_menu[current_menu].menus!=NULL)
					&&(UDM_menu[current_menu].menus[defnum].bmpfile[0]!='\0')
					&&(UDM_menu[kinc].menu_format!=1)
					&&(UDM_menu[kinc].statflag==0))
				{
/*
......add default bitmap
*/
					if (rimglist==NULL)
					{
						rimglist = uw_create_iconimg();
						rctrl.SetImageList(rimglist);
					}
					if (strcmp(UDM_menu[current_menu].menus[defnum].bmpfile, 
							UDM_menu[kinc].bmpfile)!=0)
					{
						hBmap = uw_get_bitmap(UDM_menu[current_menu].menus[defnum].bmpfile, thdc, -1);
						rpImage = cBmap.FromHandle(hBmap);
						rpImgnum = rimglist->Add(rpImage, 0xC0C0C0);
						bmpnum[i] = rpImgnum + UDM_menu[kinc].menus[i].bmpnum;
					}
					else 
						bmpnum[i] = defImgnum + UDM_menu[kinc].menus[i].bmpnum;
				}
				else
					bmpnum[i] = -1;
			}
			if (UDM_menu[kinc].menus[i].separator==0)
			{
				if (UDM_menu[kinc].statflag==0)
				{
					stat = uz_ntcnvtfunc_to_num(UDM_menu[kinc].menus[i].file, 
						UDM_menu[kinc].menus[i].params, 
						UDM_menu[kinc].menus[i].descrip, &bnum);
					fid = bnum + WM_APP + UDM_MAX_MENU + UZ_MAX_KEYITEM;
					if (stat==-1)
/*
.....then it must be a menu name
.....load this menu and not show it
*/
					{	
/*
.....See if this menu is already loaded
*/
						for (m=0;m<UDM_menu_count;m++)
						{
							strcpy(filename, UDM_menu[m].file);
							ul_break_fname(filename,dir,fname);
							if (strcmp(fname, UDM_menu[kinc].menus[i].file) == 0)
							{
									current_menu = m;
									break;
							}
						}
/*
.....not loaded, load it
*/
						if (m >= UDM_menu_count)
						{
							current_menu = UDM_menu_count;
							if (udm_read_menu(UDM_menu[kinc].menus[i].file,
									UDM_menu[kinc].menus[i].pos,
									UDM_menu[kinc].menus[i].size, 
									0, 0, -1) != 0)
							{
								current_menu = -1;
							}
						}
/*
......set the callback id, when this callback called
......show current_menu (using TrackPopupMenu for Popup/pulldown
......menu, using ShowControlBar for "ICON", "MENU" menu)
*/
						stat = uz_ntmenunum_to_num(current_menu, &bnum);
						fid = bnum + WM_APP;
						if (stat==-1)
							fid = IDF_KEY_NOOP;
					}
				}
				else
/*
.....status button
*/
				{
					stat = uz_ntstatname_to_num(UDM_menu[kinc].menus[i].statname, UDM_menu[kinc].menus[i].file, 
							UDM_menu[kinc].menus[i].descrip, &bnum, &sub);
					fid = bnum + WM_APP + UDM_MAX_MENU + UZ_MAX_KEYITEM;
					if (stat==-1)
/*
.....then it must be a menu name
.....load this menu and not show it
*/
					{	
/*
.....See if this menu is already loaded
*/
						for (m=0;m<UDM_menu_count;m++)
						{
							strcpy(filename, UDM_menu[m].file);
							ul_break_fname(filename,dir,fname);
							if (strcmp(fname, UDM_menu[kinc].menus[i].file) == 0)
							{
									current_menu = m;
									break;
							}
						}
/*
.....not loaded, load it
*/
						if (m >= UDM_menu_count)
						{
							current_menu = UDM_menu_count;
							if (udm_read_menu(UDM_menu[kinc].menus[i].file,
									UDM_menu[kinc].menus[i].pos,
									UDM_menu[kinc].menus[i].size, 
									0, 0, -1) != 0)
							{
								current_menu = -1;
							}
						}
/*
......set the callback id, when this callback called
......show current_menu (using TrackPopupMenu for Popup/pulldown
......menu, using ShowControlBar for "ICON", "MENU" menu)
*/
						stat = uz_ntmenunum_to_num(current_menu, &bnum);
						fid = bnum + WM_APP;
						if (stat==-1)
							fid = IDF_KEY_NOOP;
					}
					if ((fid != IDF_KEY_NOOP)&&(sub!=-1))
						uz_putid_stattb(fid, sub);
				}
				NCL_MainFrame->NCL_menubar[kinc]->SetButtonInfo(i, fid, 
						TBBS_BUTTON, bmpnum[i]);
			}
			else
			{
				NCL_MainFrame->NCL_menubar[kinc]->SetButtonInfo(i, ID_SEPARATOR, 
					TBBS_SEPARATOR, SEP_LEN);
				labels[i].label[0] = '\0';
				continue;
			}
			if (UDM_menu[kinc].menus[i].chcfile[0] != '\0')
			{
				CToolBarCtrl &rctrl = (NCL_MainFrame->NCL_menubar[kinc])->GetToolBarCtrl();
				TBBUTTONINFO tbbi;
				tbbi.dwMask = TBIF_STYLE;
				tbbi.cbSize = sizeof tbbi;
	
				rctrl.GetButtonInfo(fid, &tbbi);
				tbbi.fsStyle |= TBSTYLE_DROPDOWN;

				rctrl.SetButtonInfo(fid, &tbbi);
				rctrl.SetExtendedStyle(rctrl.GetExtendedStyle()|TBSTYLE_EX_DRAWDDARROWS);
			}
			if ((UDM_menu[kinc].menus[i].name[0]!='\0')
				&&(rimglist==NULL))
/*
.....text only "MENU/ICON" menu
*/
			{
				NCL_MainFrame->NCL_menubar[kinc]->m_mtype = 2;
				labels[i].id = fid;
				strcpy(labels[i].label, UDM_menu[kinc].menus[i].name);
			}
			else if (rimglist!=NULL)
/*
.....text and icon mixed "MENU/ICON" menu
*/
			{
/*
.....for icon only, we strictly ICON_ONLY
*/
//				if (!((UDM_menu[kinc].menu_format==0)&&(bmpnum[i]!=-1)))
				if (UDM_menu[kinc].menu_format!=0)
				{
					strcpy(tempstr, UDM_menu[kinc].menus[i].name);
					m = 0;
					while(tempstr[m]==' ')	m++;
					strcpy(labels[i].label, tempstr);
					if (tempstr[m]!='\0')
						NCL_MainFrame->NCL_menubar[kinc]->m_mtype = 3;
					strcpy(labels[i].label, UDM_menu[kinc].menus[i].name);
				}
				else
					labels[i].label[0] = '\0';
				labels[i].id = fid;
				rimglist->SetBkColor(CLR_NONE);
			}
		}
		if (UDM_menu[kinc].type != UDM_MTYPE_MENU)	
			(NCL_MainFrame->NCL_menubar[kinc])->EnableDocking(CBRS_ALIGN_ANY);
		else if ((UDM_menu[kinc].type == UDM_MTYPE_MENU)
					&&(UDM_menu[kinc].dockable!=0))	
			(NCL_MainFrame->NCL_menubar[kinc])->EnableDocking(CBRS_ALIGN_ANY);
		else
			(NCL_MainFrame->NCL_menubar[kinc])->EnableDocking(0);

		NCL_MainFrame->EnableDocking(CBRS_ALIGN_ANY);
		if (UDM_menu[kinc].type == UDM_MTYPE_MENU)
			NCL_MainFrame->NCL_menubar[kinc]->SetColumns(UDM_menu[kinc].cols, 1);			
		else if ((menu_pos[2]==1)||(menu_pos[2]==2))	
			NCL_MainFrame->NCL_menubar[kinc]->SetColumns(UDM_menu[kinc].cols, 0);			
		else if ((menu_pos[2]==3)||(menu_pos[2]==4))	
			NCL_MainFrame->NCL_menubar[kinc]->SetColumns(UDM_menu[kinc].cols, 2);			

		if ((NCL_MainFrame->NCL_menubar[kinc]->m_mtype==3)
			|| (NCL_MainFrame->NCL_menubar[kinc]->m_mtype==2))
		{
			if (UDM_menu[kinc].statflag==1)
			{
				if (UDM_menu[kinc].type == UDM_MTYPE_MENU)
				{
					sizex = (UDM_menu[kinc].size[0])/UDM_menu[kinc].cols;
					sizey = (UDM_menu[kinc].size[1])/UDM_menu[kinc].rows;
				}
				else
				{
					if (menu_size[0]!=-1)
						sizex = menu_size[0];
					else
						sizex = UDM_menu[kinc].size[0];
					if (menu_size[1]!=-1)
						sizey = menu_size[1];
					else
						sizey = UDM_menu[kinc].size[1];
				}
				if ((sizex<=0)&&(sizey<=0))
/*
.....25 character defaults for status bar size
*/
				{
					uw_ntget_strscsize("XXXXXXXXXXXXXxxxxxxxxxxxx", 80, "MS Serif", &sizex, &sizey);
					sizey += 8;
				}
				NCL_MainFrame->NCL_menubar[kinc]->SetTBSize(sizex, sizey);
			}
			NCL_MainFrame->NCL_menubar[kinc]->SetLabels(labels, TRUE);
		}
		else
		{
/*
.....icon only
*/
			SIZE bsize;
			if (UDM_menu[kinc].statflag==1)
			{
				if (UDM_menu[kinc].type == UDM_MTYPE_MENU)
				{
					sizex = (UDM_menu[kinc].size[0])/UDM_menu[kinc].cols;
					sizey = (UDM_menu[kinc].size[1])/UDM_menu[kinc].rows;
				}
				else
				{
					sizex = (menu_size[0])/UDM_menu[kinc].cols;
					sizey = (menu_size[1])/UDM_menu[kinc].rows;
				}
				bsize.cx = sizex;
				bsize.cy = sizey;
			}
			else
			{
				bsize.cx = -1;
				bsize.cy = -1;
			}
			NCL_MainFrame->NCL_menubar[kinc]->SetButSizes(bsize);
		}
				
		delete labels;

		int dockable;
		if (UDM_menu[kinc].type == UDM_MTYPE_MENU)
		{
			NCL_MainFrame->NCL_menubar[kinc]->SetWindowText(UDM_menu[kinc].name);
			CPoint pt(0,0);
			pt.Offset(UDM_menu[kinc].pos[0], UDM_menu[kinc].pos[1]);
			
			NCL_MainFrame->FloatControlBar(NCL_MainFrame->NCL_menubar[kinc], pt);	
			NCL_MainFrame->NCL_menubar[kinc]->m_icon = 0;
			NCL_MainFrame->NCL_menubar[kinc]->pos[0] = UDM_menu[kinc].pos[0];
			NCL_MainFrame->NCL_menubar[kinc]->pos[1] = UDM_menu[kinc].pos[1];
			NCL_MainFrame->NCL_menubar[kinc]->pos[2] = UDM_menu[kinc].pos[2];
		}
		else
		{
			NCL_MainFrame->NCL_menubar[kinc]->SetWindowText(UDM_menu[kinc].name);

			NCL_MainFrame->NCL_menubar[kinc]->pos[0] = menu_pos[0];
			NCL_MainFrame->NCL_menubar[kinc]->pos[1] = menu_pos[1];
			NCL_MainFrame->NCL_menubar[kinc]->pos[2] = menu_pos[2];
			(NCL_MainFrame->NCL_menubar[kinc])->EnableDocking(CBRS_ALIGN_ANY);
			NCL_MainFrame->EnableDocking(CBRS_ALIGN_ANY);
			if (kdis)
			{
				if (UW_reset_menu==1)
					dockable = UDM_AREA_ENDDOCK;
				else
					ud_get_areadockable(UDM_menu[kinc].menu_area, &dockable);
				uw_ntget_menurect(menu_pos[2], &rect, dockable);
				if (dockable==UDM_AREA_NODOCK)
/*
......replace/hide the control bars inside this area
*/
					uw_nthide_bars(menu_pos[2]);
			
				if (menu_pos[2]==1)
					NCL_MainFrame->DockControlBar(NCL_MainFrame->NCL_menubar[kinc], AFX_IDW_DOCKBAR_TOP, &rect);
				else if (menu_pos[2]==2)
					NCL_MainFrame->DockControlBar(NCL_MainFrame->NCL_menubar[kinc], AFX_IDW_DOCKBAR_BOTTOM, &rect);
				else if (menu_pos[2]==3)
					NCL_MainFrame->DockControlBar(NCL_MainFrame->NCL_menubar[kinc], AFX_IDW_DOCKBAR_LEFT, &rect);
				else if (menu_pos[2]==4)
					NCL_MainFrame->DockControlBar(NCL_MainFrame->NCL_menubar[kinc], AFX_IDW_DOCKBAR_RIGHT, &rect);
			}
		}
		if (kdis==1)
		{
			NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[kinc],
				TRUE, FALSE);
			(NCL_MainFrame->NCL_menubar[kinc])->m_visible = 1;
/*
.....if it is statusbar, display status text also
*/
			if (UDM_menu[kinc].statflag==1)
				uz_status();
		}
		else
		{
			NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[kinc],
				FALSE, FALSE);
			(NCL_MainFrame->NCL_menubar[kinc])->m_visible = 0;
		}
		NCL_MainFrame->RecalcLayout();
		if ((UW_auto_cursor)&&((NCL_MainFrame->NCL_menubar[kinc])->m_visible==1))
		{
			if (UW_auto_cursor)
			{
				(NCL_MainFrame->NCL_menubar[kinc])->GetWindowRect(&rect);			
				uw_ntset_curpos(rect.left + rect.Width()/2, rect.top + rect.Height()/2);
			}
		}
		return 0;
	}
	NCL_MainFrame->RecalcLayout();

	return 0;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntchoice_menu(int kinc, int xy[2], int *kk, int *choice, int *menu)
**       Defines and activates a DAS internal POPUP menu.
**    PARAMETERS   
**       INPUT  : 
**          kinc    = choice menu number.
**          
**       OUTPUT :  
**         kk     = ending key or choice value
**         xy     = loc position in dev coords
**         choice = menu choice number, or zero 
**         menu   = the menu number, or zero
**    RETURNS      : always = 3,  kk contains menu choice number. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntchoice_menu(int kinc, int xy[2], int *kk, int *choice, int *menu)
{
	int i,j, k, old_num;
	UINT fid;
	HBITMAP hBmap, hchkBmap;
	POINT pt;
	CRect rect;
	MSG msg;
	int markval;

	HDC thdc = NCL_MainFrame->GetDC()->m_hDC;
	int stat = 0;
/*
.....Take down any current popup menus
*/
	if (UW_NPopup >= 0)
	{
		old_num = PopupWin[UW_NPopup];
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[old_num],
				FALSE, FALSE);
		(NCL_MainFrame->NCL_menubar[old_num])->m_visible = 0;
	}
	if ((NCL_MainFrame->NCL_menubar[kinc]!=NULL)
		&&((NCL_MainFrame->NCL_menubar[kinc])->m_created==1))
	{
		GetCursorPos(&pt);
		NCL_MainFrame->FloatControlBar(NCL_MainFrame->NCL_menubar[kinc], pt);
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[kinc],
				TRUE, FALSE);
		(NCL_MainFrame->NCL_menubar[kinc])->m_visible = 1;
		(NCL_MainFrame->NCL_menubar[kinc])->GetWindowRect(&rect);			
		uw_ntset_curpos(rect.left + rect.Width()/2, rect.top + rect.Height()/2);
		goto handle_popup;
	}

	NCL_MainFrame->NCL_menubar[kinc] = new CToolmenu(kinc, 4);
	NCL_MainFrame->NCL_menubar[kinc]->m_created = 1;
	UINT type;
			
	type = WS_CHILD | WS_VISIBLE | CBRS_SIZE_DYNAMIC |
						 CBRS_TOOLTIPS ;
	(NCL_MainFrame->NCL_menubar[kinc])->SetButtype(1);
	(NCL_MainFrame->NCL_menubar[kinc])->SetBarType(0);

	if (!(NCL_MainFrame->NCL_menubar[kinc]->Create(NCL_MainFrame, 
				type, IDC_NCL_MENUBAR)))
	{
		MessageBox(NULL, "Failed to create popup menubar", "Warning", MB_ICONINFORMATION|MB_OK);
		*choice = -1;	
		return -1;   
	}
	int toolnum = UDM_menu[kinc].num-1;
	NCL_MainFrame->NCL_menubar[kinc]->SetButtons(NULL, toolnum);

	struct tbblabel *labels = new struct tbblabel[toolnum+1];
	NCL_MainFrame->NCL_menubar[kinc]->m_mtype = 4;
	for (i=1; i<toolnum+1; i++)
	{
		fid = WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+ NBUTTONNUM + UDM_MAX_MENU + i;
		NCL_MainFrame->NCL_menubar[kinc]->SetButtonInfo(i-1, fid, 
						TBBS_BUTTON, -1);
		labels[i-1].id = fid;
		strcpy(labels[i-1].label, UDM_menu[kinc].menus[i].name);
	}
	(NCL_MainFrame->NCL_menubar[kinc])->EnableDocking(0);
	NCL_MainFrame->NCL_menubar[kinc]->SetColumns(UDM_menu[kinc].cols, 1);			
	NCL_MainFrame->NCL_menubar[kinc]->SetLabels(labels, TRUE);		
	delete labels;

	NCL_MainFrame->NCL_menubar[kinc]->SetWindowText(UDM_menu[kinc].menus[0].name);
	GetCursorPos(&pt);			
	NCL_MainFrame->FloatControlBar(NCL_MainFrame->NCL_menubar[kinc], pt);	
	NCL_MainFrame->NCL_menubar[kinc]->m_icon = 0;
	(NCL_MainFrame->NCL_menubar[kinc])->GetWindowRect(&rect);			
	uw_ntset_curpos(rect.left + rect.Width()/2, rect.top + rect.Height()/2);
	NCL_MainFrame->RecalcLayout();
handle_popup:;
	UW_NPopup++;
	PopupWin[UW_NPopup] = kinc;
	PopupChoice[UW_NPopup] = -1;
	UD_MARK(markval,UU_FALSE);
	*kk = 1;
	if (markval == 0)
	{

		while (UW_NPopup >= 0 && PopupChoice[UW_NPopup] == -1)
		{
			if (GetMessage( &msg, NULL, 0, 0 ) != -1) 
			{
				if (msg.message == WM_MBUTTONDOWN || msg.message == WM_RBUTTONDOWN)
				{
/*
......take down this popup window  ************
*/
					NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[kinc],
							FALSE, FALSE);
					(NCL_MainFrame->NCL_menubar[kinc])->m_visible = 0;
					PopupWin[UW_NPopup] = -1;
					PopupChoice[UW_NPopup] = 0;
					*kk = 2;
					if (msg.message == WM_RBUTTONDOWN) *kk = 3;
					break;
				}
				else
					DispatchMessage( &msg );
			}
			else
				DispatchMessage( &msg );
		}
	}
	else
	{
		PopupWin[UW_NPopup] = -1;
	}

	if (UW_NPopup < 0)
	{
		*kk = 3;
		*choice = 0;
	}
	else *choice = PopupChoice[UW_NPopup];
/*
......take down this popup menu and restore the old one
*/
	if (UW_NPopup >= 0)
	{
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[kinc],
					FALSE, FALSE);
		(NCL_MainFrame->NCL_menubar[kinc])->m_visible = 0;
		PopupChoice[UW_NPopup] = -1;
		UW_NPopup--;
	}
	if (UW_NPopup >= 0)
	{
		old_num = PopupWin[UW_NPopup];
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[old_num],
				TRUE, FALSE);
		(NCL_MainFrame->NCL_menubar[old_num])->m_visible = 1;
	}
	UD_UNMARK(markval);
	return 3;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntchoice_menu(int kinc, int xy[2], int *kk, int *choice, int *menu)
**       Defines and activates a DAS internal POPUP menu.
**    PARAMETERS   
**       INPUT  : 
**          kinc    = choice menu number.
**          
**       OUTPUT :  
**         kk     = ending key or choice value
**         xy     = loc position in dev coords
**         choice = menu choice number, or zero 
**         menu   = the menu number, or zero
**    RETURNS      : always = 3,  kk contains menu choice number. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntchoice_menu2(int kinc, int xy[2], int *kk, int *choice, int *menu)
{
	int i,j, k;
	UINT fid;
	HBITMAP hBmap, hchkBmap;
	POINT pt;
	CRect rect;
	MENUINFO info;

	HDC thdc = NCL_MainFrame->GetDC()->m_hDC;
	int stat = 0;

	if (NCL_menu[kinc]!=NULL)
	{
		GetCursorPos(&pt);
		UW_TrackingWindow = NCL_MainFrame->GetSafeHwnd();
		UW_TrackingMenu = NCL_menu[kinc]->m_hMenu;
		*choice = NCL_menu[kinc]->TrackPopupMenu(TPM_LEFTBUTTON|TPM_LEFTALIGN,
					pt.x, pt.y, NCL_MainFrame, NULL);
		UW_TrackingWindow = NULL;
		if (UW_current_menubar!=NULL)
			UW_current_menubar->reset_timer();
		*kk = 1;
		if (*choice!=0)
		{
			MSG msg;
			NCL_PupupChoice = -1;
			while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))	
			{
				if ((msg.message==WM_RBUTTONUP)||(msg.message==WM_RBUTTONDOWN))
					*choice = -1;
				DispatchMessage(&msg);
			}
			if (*choice!=-1)
				*choice = NCL_PupupChoice;
			else
			{
				*kk = 3;
			}
		}
		else
			*kk = 3;

		return 3;
	}

	NCL_menu[kinc] = new CNCLMenu();
	NCL_menu[kinc]->CreatePopupMenu();
	NCL_menu[kinc]->SetIdnum(kinc);
		
	info.cbSize = sizeof(MENUINFO);
	stat = NCL_menu[kinc]->GetMenuInfo(&info);
	info.dwStyle &= ~(MNS_DRAGDROP);
	info.dwStyle |= MNS_DRAGDROP;
	info.fMask = MIM_APPLYTOSUBMENUS | MIM_STYLE;
	NCL_menu[kinc]->SetMenuInfo(&info);

	k = 0;
/*
.....added title button without any callback
*/
	hBmap = NULL;
	hchkBmap = NULL;
	NCL_menu[kinc]->SetBitmap(hBmap,hchkBmap, 0, -1, -1);
	fid = WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+ NBUTTONNUM + UDM_MAX_MENU + NCHOICEBUTTON + 1;
	NCL_menu[kinc]->InsertMenu(-1, MF_STRING|MF_OWNERDRAW|MF_BYPOSITION, fid, UDM_menu[kinc].name);
	NCL_menu[kinc]->SetMenuInfo(&info);
	for (i=0;i<UDM_menu[kinc].rows;i++)
	{
		for (j=0;j<UDM_menu[kinc].cols;j++)
		{
			if (k!=0)
			{
				fid = WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+ NBUTTONNUM + UDM_MAX_MENU + k;
				hBmap = NULL;
				hchkBmap = NULL;
				NCL_menu[kinc]->SetBitmap(hBmap,hchkBmap, k-1, fid, -1);
				NCL_menu[kinc]->InsertMenu(-1, MF_STRING|MF_OWNERDRAW|MF_BYPOSITION,fid, 
						UDM_menu[kinc].menus[k].name);
				NCL_menu[kinc]->SetMenuInfo(&info);
			}
			k++;
		}
	}
	GetCursorPos(&pt);
	UW_TrackingWindow = NCL_MainFrame->GetSafeHwnd();
	UW_TrackingMenu = NCL_menu[kinc]->m_hMenu;
	*choice = NCL_menu[kinc]->TrackPopupMenu(TPM_LEFTBUTTON|TPM_LEFTALIGN,
					pt.x, pt.y, NCL_MainFrame, NULL);
	UW_TrackingWindow = NULL;
	if (UW_current_menubar!=NULL)
		UW_current_menubar->reset_timer();
	*kk = 1;
	if (*choice!=0)
	{
		MSG msg;
		NCL_PupupChoice = -1;
		while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
		{
			if ((msg.message==WM_RBUTTONUP)||(msg.message==WM_RBUTTONDOWN))
				*choice = -1;
			DispatchMessage(&msg);
		}
		if (*choice!=-1)
			*choice = NCL_PupupChoice;
		else
		{
			*kk = 3;
		}
	}
	else
		*kk = 3;
	return 3;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntmenu_reset()
**       Performs one or more of the following 
**
**			1.  Takes down all non-layout defined menus (always).
**			2.  Displays all layout defined menus, prompt area,
**			    and status area (kmenu).
**
**    PARAMETERS   
**       INPUT  : 
**              kmenu    = True = Display layout menus.  False = Take down.
**              kgraph   = True = Display graphics area.  False = Take down.
**              kicon    = True = Iconify first layout menu.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
......acturally, if not display graphics area, then, nothing
......should displayed on WinNT
......ignore kicon also on WinNT
*/
extern "C" void uw_mfmenu_reset(int kmenu, int kgraph, int kicon)
{
	int i,j;
	char fname[UX_MAX_FILE_LEN];
	UX_pathname dir, filename;
	frm_done = 0;
/*
.....take down all floating menus and status bar
*/
	for (i=0;i<UDM_menu_count;i++)
	{
		if (NCL_MainFrame->NCL_menubar[i]==NULL)
			continue;
		if ((NCL_MainFrame->NCL_menubar[i]->m_created==1)
			&& (NCL_MainFrame->NCL_menubar[i]->IsFloating()))
		{
			NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[i],
				FALSE, FALSE);
			(NCL_MainFrame->NCL_menubar[i])->m_visible = 0;
		}
	}
/*
.....take down all bars
*/
	uw_nthide_bars(1);
	uw_nthide_bars(2);
	uw_nthide_bars(3);
	uw_nthide_bars(4);
	NCL_MainFrame->RecalcLayout();
	ncl_set_statact(0);
/*
.....Display Graphics area
*/
	if (kgraph==0)
	{
/*
.....hide frame also
*/
		NCL_MainFrame->ShowWindow(SW_HIDE);
		return;
	}
	else
		NCL_MainFrame->ShowWindow(SW_SHOW);
/*
.....move window to original position first
*/
	NCL_MainFrame->MoveWindow(UDM_layout.window_pos[0],UDM_layout.window_pos[1], 
		UDM_layout.window_size[0],UDM_layout.window_size[1]);
	if (kmenu==0)
		goto size_gh;
/*
.....show floating bars again
*/
	for (i=0;i<UDM_layout.nmenu;i++)
	{
/*
......even if there is a menu area define for a menu but if this menu is defined floating type
......we need redisplay floating
*/
		if (strcmp(UDM_layout.menubar, UDM_layout.menu_name[i])==0)
		{
			continue;
		}
		if (UDM_layout.menu_name[i][0] == '\0')
			continue;

		for (j=0;j<UDM_menu_count;j++)
		{
			strcpy(filename, UDM_menu[j].file);
			ul_break_fname(filename,dir,fname);
/*
......check if the menu type is floating
*/
			if ((strcmp(fname, UDM_layout.menu_name[i])==0) 
				&& (UDM_menu[j].type==UDM_MTYPE_MENU))

			{
				CPoint pt(0,0);
				pt.Offset(UDM_menu[j].pos[0], UDM_menu[j].pos[1]);
				NCL_MainFrame->FloatControlBar(NCL_MainFrame->NCL_menubar[j], pt);
				NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[j],
							TRUE, FALSE);
				(NCL_MainFrame->NCL_menubar[j])->m_visible = 1;
				break;
			}
		}
	}
	uu_move_byte((char*)&UDM_layout, (char*)&UDM_run_layout,
								sizeof(UDM_LAYOUT));
/*
.....show Icon bars again
*/
	UW_reset_menu = 1;
	uw_ntshow_dir(1);
	uw_ntshow_dir(2);
	uw_ntshow_dir(3);
	uw_ntshow_dir(4);

size_gh:;
	int wid, hgt;
	uw_ntget_gwsize(&wid, &hgt);	
				
	int clip[4];
	clip[0] = 0;
	clip[1] = 0;
	clip[2] = wid;
	clip[3] = hgt;
	uw_glset_scissor(clip);

	uw_glresize_graphics(wid, hgt, 1);
	
	UW_reset_menu = 0;
	frm_done = 1;
	return;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntdown_menu(fname)
**       Takes down a menu based on its file name.  This routine is
**			usually called for the fixed name menus, such as 'SELECT.menu'.
**    PARAMETERS   
**       INPUT  : 
**          fname   = File name of menu to take down.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntdown_menu(char *fname)
{
	int i,j,args[3], num;
	char buf[256];
/*
.....Search for the requested menu
*/
	for (i=0;i<UDM_menu_count;i++)
	{
		if (UDM_menu[i].type == UDM_MTYPE_MENUBAR)
			continue;
/*
........Allow for directory path on menu file name
*/
		args[0] = 1; args[1] = 0;
		ul_get_base_fname(UDM_menu[i].file,buf,args,UX_NCHK|UX_NQUOTES|UX_NPRTERRS);
		if ((strcmp(UDM_menu[i].file,fname) != 0) && (strcmp(buf,fname) != 0))
			continue;
/*
........Found the menu
........If it's up, let's take it down
*/
		if ((UDM_menu[i].type == UDM_MTYPE_PULLDOWN)
			|| (UDM_menu[i].type == UDM_MTYPE_POPUP))
		{
			if (NCL_menu[i]==NULL)
				continue;
/*
.....check menu handler still there, it may delete by parent menu
*/
			if (!(::IsMenu(NCL_menu[i]->m_hMenu)))
				continue;
/*
......we only take down drag/tear off menu of POPUP menu, the popup menu itself
......should be already down when mouse clicked
*/
			num = NCL_menu[i]->GetDragIdnum();
			if (num==-1)
				continue;
			if (((NCL_MainFrame->NCL_menubar[num])->m_created==1)&&(NCL_MainFrame->NCL_menubar[num]->m_visible==1))
			{
				NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[num],
							FALSE, FALSE);
				(NCL_MainFrame->NCL_menubar[num])->m_visible = 0;
				break;
			}
		}
		else if (((NCL_MainFrame->NCL_menubar[i])->m_created==1)&&(NCL_MainFrame->NCL_menubar[i]->m_visible==1))
		{
			NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[i],
							FALSE, FALSE);
			(NCL_MainFrame->NCL_menubar[i])->m_visible = 0;
			break;
		}
	}
	NCL_MainFrame->RecalcLayout();
}

/**********************************************************************
**    I_FUNCTION :  uw_display_dragmenu(int menunum)
**       Display dragging (tore off) menu
**			
**    PARAMETERS   
**       INPUT  : 
**          menunum   = Menu to be tore off.
**       OUTPUT :  
**          None
**    RETURNS      : -1: no drag menu displayed
**					0: drag menu displayed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_display_dragmenu(int menunum)
{
	int num, nc;
	POINT pt;
	UX_pathname tempfile;
	char *p;

	if (NCL_menu[menunum]==NULL)
		return -1;
	num = NCL_menu[menunum]->GetDragIdnum();
	if (num!=-1)
	{
		if (NCL_MainFrame->NCL_menubar[num]!=NULL)
/*
.....just display it
*/
		{
			NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[num],
					TRUE, FALSE);
			(NCL_MainFrame->NCL_menubar[num])->m_visible = 1;
		}
		return 0;
	}
/*
.....Create floating menu
*/
	num = UDM_menu_count;
/*
.....copy structure
*/
	strcpy(UDM_menu[num].name, UDM_menu[menunum].name);
/*
.....use "file_F.menu" for drag menu
*/
	strcpy(tempfile, UDM_menu[menunum].file);
	p = strchr(tempfile, '.');
	if (p!=NULL)
		*p = '\0';
	strcat(tempfile, "_F.menu");
	strcpy(UDM_menu[num].file, tempfile);
	UDM_menu[num].num = UDM_menu[menunum].num ;
	UDM_menu[num].rows = UDM_menu[menunum].rows ;
	UDM_menu[num].cols = UDM_menu[menunum].cols ;
	UDM_menu[num].size[0] = UDM_menu[menunum].size[0] ;
	UDM_menu[num].size[1] = UDM_menu[menunum].size[1] ;
	strcpy(UDM_menu[num].bmpfile, UDM_menu[menunum].bmpfile);
	strcpy(UDM_menu[num].menu_area, UDM_menu[menunum].menu_area);
	UDM_menu[num].menu_format = UDM_menu[menunum].menu_format;
	UDM_menu[num].menus = (UDM_menu_struc *)uu_malloc(sizeof(UDM_menu_struc)*
				UDM_menu[menunum].num);
	for (int i=0; i<UDM_menu[num].num; i++)
	{
		strcpy(UDM_menu[num].menus[i].name, UDM_menu[menunum].menus[i].name);
		strcpy(UDM_menu[num].menus[i].file, UDM_menu[menunum].menus[i].file);
		strcpy(UDM_menu[num].menus[i].chcfile, UDM_menu[menunum].menus[i].chcfile);
		UDM_menu[num].menus[i].chcdef = UDM_menu[menunum].menus[i].chcdef;
		if (UDM_menu[menunum].menus[i].params==UU_NULL)
			UDM_menu[num].menus[i].params = NULL; 
		else
		{
			nc = strlen(UDM_menu[menunum].menus[i].params);
			UDM_menu[num].menus[i].params = uu_malloc((nc+1)*sizeof(char));
			strcpy(UDM_menu[num].menus[i].params, UDM_menu[menunum].menus[i].params);
		}
		if (UDM_menu[menunum].menus[i].statname[0]!='\0')
			strcpy(UDM_menu[num].menus[i].statname, UDM_menu[menunum].menus[i].statname);
		else
			UDM_menu[num].menus[i].statname[0] = '\0';

		strcpy(UDM_menu[num].menus[i].descrip, UDM_menu[menunum].menus[i].descrip);
		strcpy(UDM_menu[num].menus[i].toggle_def, UDM_menu[menunum].menus[i].toggle_def);
		UDM_menu[num].menus[i].pos[0] = UDM_menu[menunum].menus[i].pos[0];
		UDM_menu[num].menus[i].pos[1] = UDM_menu[menunum].menus[i].pos[1];
		UDM_menu[num].menus[i].size[0] = UDM_menu[menunum].menus[i].size[0];
		UDM_menu[num].menus[i].size[1] = UDM_menu[menunum].menus[i].size[1];
		UDM_menu[num].menus[i].kinc = UDM_menu[menunum].menus[i].kinc;
		UDM_menu[num].menus[i].toggle_num = UDM_menu[menunum].menus[i].toggle_num;
		UDM_menu[num].menus[i].toggle = UDM_menu[menunum].menus[i].toggle;
		strcpy(UDM_menu[num].menus[i].bmpfile, UDM_menu[menunum].menus[i].bmpfile);
		UDM_menu[num].menus[i].bmpnum = UDM_menu[menunum].menus[i].bmpnum;
		UDM_menu[num].menus[i].separator = UDM_menu[menunum].menus[i].separator;
	}
	UDM_menu[num].dockable = UDM_menu[menunum].dockable;
	UDM_menu[num].type = UDM_MTYPE_MENU;
	GetCursorPos(&pt);

	UDM_menu[num].pos[0] = pt.x;
	UDM_menu[num].pos[1] = pt.y;
	UDM_menu_count++;
	uw_ntmenu(num, 1);
	NCL_menu[menunum]->SetDragIdnum(num);
	return 0;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntmenu_redisp(int old_size, int old_fmt)
**       redisplay menu with icon size and text format change
**			
**    PARAMETERS   
**       INPUT  : 
**          old_size: old menu icon size
**			old_fmt: old menu text format
**       OUTPUT :  
**          None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" VOID uw_ntmenu_redisp(int old_size, int old_fmt)
{
	int kinc;
	int i, k, m, toolnum, bImage;
	char tempstr[200];
	HBITMAP hBmap, hchkBmap = NULL;
	HBITMAP defhBmap = NULL;
	int defload = 0;
	int bmpnum[100];
	UINT fid,btype;
	CRect rect;

	HDC thdc = NCL_MainFrame->GetDC()->m_hDC;

	for (kinc=0;kinc<UDM_menu_count;kinc++)
	{
		if ((UDM_menu[kinc].type == UDM_MTYPE_PULLDOWN)
			|| (UDM_menu[kinc].type == UDM_MTYPE_POPUP))
		{
			if (NCL_menu[kinc]==NULL)
				continue;
/*
.....check menu handler still there, it may delete by parent menu
*/
			if (!(::IsMenu(NCL_menu[kinc]->m_hMenu)))
				continue;

			UINT fid, tearID;
			CMenu* menuitem;
			int menucount = NCL_menu[kinc]->GetMenuItemCount();
			tearID = WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM + kinc;
			NCL_menu[kinc]->InsertMenu(-1, MF_STRING|MF_OWNERDRAW|MF_BYPOSITION,tearID, " ");
			for (i=0;i<menucount-1;i++)
			{
				fid = NCL_menu[kinc]->GetMenuItemID(i+1);
				if (fid==-1)
				{
					menuitem = NCL_menu[kinc]->GetSubMenu(i+1); 
					NCL_menu[kinc]->AppendMenu(MF_OWNERDRAW|MF_STRING|MF_ENABLED|MF_POPUP, 
									(UINT)(menuitem->GetSafeHmenu()), UDM_menu[kinc].menus[i].name);
				}
				else if (fid==0)
					NCL_menu[kinc]->InsertMenu(-1, MF_SEPARATOR|MF_BYPOSITION);
				else
					NCL_menu[kinc]->InsertMenu(-1, MF_STRING|MF_OWNERDRAW|MF_BYPOSITION,fid, 
							UDM_menu[kinc].menus[i].name);
			}
			for (k=0;k<UDM_menu[kinc].num;k++)
			{
				if (UDM_menu[kinc].menus[i].separator==0)
				{
					if (UDM_menu[kinc].menu_format!=1)
					{
						if ((k==0)&&(UDM_menu[kinc].bmpfile[0]!='\0'))
						{
							NCL_menu[kinc]->m_hCBitmap = uw_get_bitmap(UDM_menu[kinc].bmpfile, thdc, -2);
							if (NCL_menu[kinc]->m_hCBitmap!=NULL)
								defload = 1;
							else
								defload = 0;
						}
						if (UDM_menu[kinc].menus[k].bmpfile[0]!='\0')
						{
							if (strcmp(UDM_menu[kinc].menus[k].bmpfile, 
											UDM_menu[kinc].bmpfile)!=0)
							{
								hBmap = uw_get_bitmap(UDM_menu[kinc].menus[k].bmpfile, thdc,-2);
								if (hBmap==NULL)
									bmpnum[k] = -1;
								else
									bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
							}
							else
							{
								if (defload==0)
									bmpnum[k] = -1;
								else
									bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
								hBmap = NULL;
							}
						}
						else
						{
							hBmap = NULL;
							bmpnum[k] = -1;
						}
					}
					else
					{
						hBmap = NULL;
						hchkBmap = NULL;
						bmpnum[k] = -1;
					}
					NCL_menu[kinc]->SetBitmap(hBmap,hchkBmap, k, -1, bmpnum[k]);
				}
			}
		} // if
		if ((UDM_menu[kinc].type == UDM_MTYPE_MENU)
			|| (UDM_menu[kinc].type == UDM_MTYPE_ICON))
		{
			toolnum = UDM_menu[kinc].num;
/*
.....Create Imagelist for menubar
*/
			CBitmap *rpImage;
			CBitmap cBmap;
			int rpImgnum;
			CBitmap *defImage;
			CBitmap defcBmap;
			int defImgnum;
			int first_image = 1;
			if ((NCL_MainFrame->NCL_menubar[kinc])==NULL)
				continue;

			CToolBarCtrl &rctrl = (NCL_MainFrame->NCL_menubar[kinc])->GetToolBarCtrl();
			CImageList *rimglist = rctrl.GetImageList();
			if (((old_size!=UW_icon_size)||(UDM_menu[kinc].menu_format==1))
				&&(rimglist!=NULL))
			{
				delete rimglist;
				rimglist = NULL;
			}
			if (UDM_menu[kinc].menu_format!=1)
			{
				for (int i=0; i<toolnum; i++)
				{
					if ((UDM_menu[kinc].menus[i].bmpfile[0]!='\0')
						&&(UDM_menu[kinc].menu_format!=1))
					{
						if (rimglist==NULL)
						{
							rimglist = uw_create_iconimg();
							if (UW_icon_size==0)
							{
								(NCL_MainFrame->NCL_menubar[kinc])->SetBitmapSize(16, 16);
							}
							else if (UW_icon_size==1)
							{
								(NCL_MainFrame->NCL_menubar[kinc])->SetBitmapSize(24, 24);
							}
							else if (UW_icon_size==2)
							{
								(NCL_MainFrame->NCL_menubar[kinc])->SetBitmapSize(32, 32);
							}
							else if (UW_icon_size==3)
							{
								(NCL_MainFrame->NCL_menubar[kinc])->SetBitmapSize(40, 40);
							}
							else if (UW_icon_size==4)
							{
								(NCL_MainFrame->NCL_menubar[kinc])->SetBitmapSize(48, 48);
							}
							else
							{
								(NCL_MainFrame->NCL_menubar[kinc])->SetBitmapSize(16, 16);
							}
						}
						if ((first_image)&&(UDM_menu[kinc].bmpfile[0]!='\0'))
						{
							first_image = 0;
							defhBmap = uw_get_bitmap(UDM_menu[kinc].bmpfile, thdc, -1);
							defImage = defcBmap.FromHandle(defhBmap);
							defImgnum = rimglist->Add(defImage, 0xC0C0C0);
						}
						if (strcmp(UDM_menu[kinc].menus[i].bmpfile, 
									UDM_menu[kinc].bmpfile)!=0)
						{
							hBmap = uw_get_bitmap(UDM_menu[kinc].menus[i].bmpfile, thdc, -1);
							rpImage = cBmap.FromHandle(hBmap);
							rpImgnum = rimglist->Add(rpImage, 0xC0C0C0);
							bmpnum[i] = rpImgnum + UDM_menu[kinc].menus[i].bmpnum;
						}
						else
							bmpnum[i] = defImgnum + UDM_menu[kinc].menus[i].bmpnum;
					}
					else
						bmpnum[i] = -1;
				}
			}
			rctrl.SetImageList(rimglist);
			for (i=0; i<toolnum; i++)
			{
				if (UDM_menu[kinc].menus[i].separator==0)
				{
					NCL_MainFrame->NCL_menubar[kinc]->GetButtonInfo(i, fid, 
							btype, bImage);
					if (rimglist!=NULL)
						NCL_MainFrame->NCL_menubar[kinc]->SetButtonInfo(i, fid, 
							btype, bmpnum[i]);
					else
						NCL_MainFrame->NCL_menubar[kinc]->SetButtonInfo(i, fid, 
							btype, -1);
				}
			}
/*
.....structure for icon/text mixed menu
*/
			struct tbblabel *labels = new struct tbblabel[toolnum+1];
/*
.....assume icon only
*/
			NCL_MainFrame->NCL_menubar[kinc]->m_mtype = 1;
			for (i=0; i<toolnum; i++)
			{
				if ((UDM_menu[kinc].menus[i].name[0]!='\0')
						&&(rimglist==NULL))
/*
.....text only "MENU/ICON" menu
*/
				{
					NCL_MainFrame->NCL_menubar[kinc]->m_mtype = 2;
					labels[i].id = -1;
					strcpy(labels[i].label, UDM_menu[kinc].menus[i].name);
				}
				else if (rimglist!=NULL)
/*
.....text and icon mixed "MENU/ICON" menu
*/
				{
/*
.....for icon only, we strictly ICON_ONLY
*/
//					if (!((UDM_menu[kinc].menu_format==0)&&(bmpnum[i]!=-1)))
					if (UDM_menu[kinc].menu_format!=0)
					{
						strcpy(tempstr, UDM_menu[kinc].menus[i].name);
						m = 0;
						while(tempstr[m]==' ')	m++;
						strcpy(labels[i].label, tempstr);
						if (tempstr[m]!='\0')
							NCL_MainFrame->NCL_menubar[kinc]->m_mtype = 3;
						strcpy(labels[i].label, UDM_menu[kinc].menus[i].name);
					}
					else
						labels[i].label[0] = '\0';
					labels[i].id = -1;
				}
			}
			int savcols = 1;
			if ((NCL_MainFrame->NCL_menubar[kinc])->IsFloating())
				savcols = (NCL_MainFrame->NCL_menubar[kinc])->GetCols();			
			if ((NCL_MainFrame->NCL_menubar[kinc]->m_mtype==3)
				|| (NCL_MainFrame->NCL_menubar[kinc]->m_mtype==2))
			{
				NCL_MainFrame->NCL_menubar[kinc]->SetLabels(labels, TRUE);
			}
			else
			{
/*
.....icon only
*/
				SIZE bsize;
				if (UDM_menu[kinc].statflag!=1)
				{
					bsize.cx = -1;
					bsize.cy = -1;
				}
/*
......remove all the label
*/
				for (i=0; i<toolnum; i++)
				{
					NCL_MainFrame->NCL_menubar[kinc]->RemoveButtonText(i);
				}
				NCL_MainFrame->NCL_menubar[kinc]->SetButSizes(bsize);
			}
			delete labels;
/*
.....Force floating menu to resize itself after changes
.....Icon menu will resize when call NCL_MainFrame->RecalcLayout()
*/
			CPoint pt(0,0);
			if ((NCL_MainFrame->NCL_menubar[kinc])->IsFloating())
			{
				(NCL_MainFrame->NCL_menubar[kinc])->GetWindowRect(&rect);			
				uw_ntadd_menuborder(&rect);
				pt.Offset(rect.left, rect.top);
				(NCL_MainFrame->NCL_menubar[kinc])->SetDefFloatWidth(savcols);
				NCL_MainFrame->FloatControlBar(NCL_MainFrame->NCL_menubar[kinc], pt);	
			}
		}
	}
	NCL_MainFrame->RecalcLayout();
/*
.....delete original Popup menu item
.....after changing
*/
	for (kinc=0;kinc<UDM_menu_count;kinc++)
	{
		if ((UDM_menu[kinc].type == UDM_MTYPE_PULLDOWN)
			|| (UDM_menu[kinc].type == UDM_MTYPE_POPUP))
		{
			if (NCL_menu[kinc]==NULL)
				continue;
/*
.....check menu handler still there, it may delete by parent menu
*/
			if (!(::IsMenu(NCL_menu[kinc]->m_hMenu)))
				continue;

			for (i=0;i<UDM_menu[kinc].num+1;i++)
			{
				NCL_menu[kinc]->RemoveMenu(0, MF_BYPOSITION);
			}
		}
	}
	NCL_MainFrame->RecalcLayout();
	uz_status();
}


/**********************************************************************
**    I_FUNCTION :  uw_ntshow_interface()
**       Show all interface again
**			
**    PARAMETERS   
**       INPUT  : 
**          None
**       OUTPUT :  
**          None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntshow_interface()
{
	int i,j;
	UX_pathname dir,filename;
	char fname[UX_MAX_FILE_LEN];
	frm_done = 0;
	NCL_MainFrame->ShowWindow(SW_SHOW);
/*
.....show floating bars again
*/
	for (i=0;i<UDM_layout.nmenu;i++)
	{
/*
......even if there is a menu area define for a menu but if this menu is defined floating type
......we need redisplay floating
*/
/*		if ((UDM_layout.menu_area[i][0]!='\0') 
			|| (strcmp(UDM_layout.menubar, UDM_layout.menu_name[i])==0)) */
		if (strcmp(UDM_layout.menubar, UDM_layout.menu_name[i])==0)
			continue;
		if (UDM_layout.menu_name[i][0] == '\0')
			continue;

		for (j=0;j<UDM_menu_count;j++)
		{
			strcpy(filename, UDM_menu[j].file);
			ul_break_fname(filename,dir,fname);
/*
......check if the menu type is floating
*/
			if ((strcmp(fname, UDM_layout.menu_name[i])==0) 
				&& (UDM_menu[j].type==UDM_MTYPE_MENU))
			{
				CPoint pt(0,0);
/*
.....the layout position only define icon (menu area) position
.....not for floating type of menu
*/
/*				pt.Offset(UDM_layout.menu_pos[i][0], UDM_layout.menu_pos[i][1]);  */
				pt.Offset(UDM_menu[j].pos[0], UDM_menu[j].pos[1]);
				NCL_MainFrame->FloatControlBar(NCL_MainFrame->NCL_menubar[j], pt);
				NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[j],
							TRUE, FALSE);
				(NCL_MainFrame->NCL_menubar[j])->m_visible = 1;
				break;
			}
		}
	}
/*
.....show Icon bars again
*/
	UW_reset_menu = 1;
	uw_ntshow_dir(1);
	uw_ntshow_dir(2);
	uw_ntshow_dir(3);
	uw_ntshow_dir(4);

	UW_reset_menu = 0;
	frm_done = 1;
	return;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntdown_menunum(menu_num, drag_num)
**       Takes down a menu based on menu index number.  
**    PARAMETERS   
**       INPUT  : 
**          menu_num   = menu index number.
**       OUTPUT :  
**          drag_num: if the menu is popup menu and have the drag menu
**						it  return the drag menu number
**    RETURNS      : 1: the menu is displayed and be taken down
**							0: the menu is not display at all and doing nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntdown_menunum(int menu_num, int *drag_num)
{
	int i,num;
	char buf[256];

	*drag_num = -1;
	if (UDM_menu[menu_num].type == UDM_MTYPE_MENUBAR)
		return 0;
	if ((UDM_menu[menu_num].type == UDM_MTYPE_PULLDOWN)
			|| (UDM_menu[menu_num].type == UDM_MTYPE_POPUP))
	{
		if (NCL_menu[menu_num]==NULL)
			return 0;
/*
.....check menu handler still there, it may delete by parent menu
*/
		if (!(::IsMenu(NCL_menu[menu_num]->m_hMenu)))
			return 0;
/*
......we only take down drag/tear off menu of POPUP menu, the popup menu itself
......should be already down when mouse clicked
*/
		num = NCL_menu[menu_num]->GetDragIdnum();
		if (num==-1)
			return 0;
		if (((NCL_MainFrame->NCL_menubar[num])->m_created==1)&&(NCL_MainFrame->NCL_menubar[num]->m_visible==1))
		{
			NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[num],
							FALSE, FALSE);
			(NCL_MainFrame->NCL_menubar[num])->m_visible = 0;
			*drag_num = num;

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
			return 1;
		}
		return 0;
	}
	else if (((NCL_MainFrame->NCL_menubar[menu_num])->m_created==1)
		&&(NCL_MainFrame->NCL_menubar[menu_num]->m_visible==1))
	{
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[menu_num],
						FALSE, FALSE);
		(NCL_MainFrame->NCL_menubar[menu_num])->m_visible = 0;
		return 1;
	}
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntpopchoice(int mflag, int *choice)
**			display popup menu depend on choice.
**			
**    PARAMETERS   
**       INPUT  : 
**          mflag   = state which popup menu to be displyed
**       OUTPUT :  
**          choice: answer
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntpopchoice(int mflag, int *choice)
{
	POINT pt;
	GetCursorPos(&pt);

	CMenu pmenu;
	pmenu.CreatePopupMenu();
	if (mflag==0)
	{
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_COPYHERE, _T("Copy Here"));
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_MOVEHERE, _T("Move Here"));
	}
	else if (mflag==1)
	{
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_DELETE, _T("Delete"));
	}
	else if (mflag==2)
	{
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_SAVE, _T("Save"));
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_SAVE_AS, _T("Save As"));
	}
	else if (mflag==3)
	{
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_INSERT_SEP, _T("Insert Separator"));
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_INSERT_ITEM, _T("Insert Item"));
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_EDIT_ITEM, _T("Edit Item"));
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_DELETE, _T("Delete Item"));
	}
	else if (mflag==4)
	{
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_SAVE, _T("Save"));
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_SAVE_AS, _T("Save As"));
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_EDIT_MENU, _T("Edit"));
	}
	else if (mflag==5)
	{
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_SAVE, _T("Save"));
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_SAVE_AS, _T("Save As"));
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_EDIT_MENU, _T("Edit"));
		pmenu.AppendMenu (MF_ENABLED, ID_POPUP_DELETE, _T("Delete All"));
	}
	pmenu.AppendMenu (MF_ENABLED, ID_POPUP_CANCEL, _T("Cancel"));

	UINT cmdid = pmenu.TrackPopupMenu(TPM_LEFTBUTTON|TPM_LEFTALIGN|TPM_RETURNCMD|TPM_RECURSE,
					pt.x, pt.y, NCL_MainFrame, NULL);
	if (cmdid==ID_POPUP_INSERT_SEP)
		*choice = 4;
	else if ((cmdid==ID_POPUP_INSERT_ITEM)||(cmdid==ID_POPUP_EDIT_MENU))
		*choice = 3;
	else if ((cmdid==ID_POPUP_COPYHERE)||(cmdid==ID_POPUP_SAVE)||(cmdid==ID_POPUP_EDIT_ITEM))
		*choice = 2;
	else if ((cmdid==ID_POPUP_MOVEHERE)||(cmdid==ID_POPUP_SAVE_AS)||(cmdid==ID_POPUP_DELETE))
		*choice = 1;
	else if (cmdid==ID_POPUP_CANCEL)
		*choice = 0;
	else
		*choice = -1;
	if ((mflag==5)&&(cmdid==ID_POPUP_DELETE))
		*choice = 4;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntget_menunum(int menu)
**			Get the menu's real index. the 'menu' could be a tear-up draging menu, 
**			we need find out and return the real menu number
**			
**    PARAMETERS   
**       INPUT  : 
**          menu   = Menu to be checked.
**       OUTPUT :  
**          None
**    RETURNS      : return -1: menu is not a tear-up menu
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntget_menunum(int menu)
{
	int i;
	if (UDM_menu[menu].type!=UDM_MTYPE_MENU)
		return -1;
/*
......check if the 'menu' is the drag_num of the POPUP menu
*/
	for (i=0; i<UDM_menu_count; i++)
	{
		if ((UDM_menu[i].type == UDM_MTYPE_PULLDOWN)
					|| (UDM_menu[i].type == UDM_MTYPE_POPUP))
		{
			if (NCL_menu[i]==NULL)
				continue;
			if (NCL_menu[i]->GetDragIdnum()==menu)
				return i;
		}
	}
	return -1;
}

/**********************************************************************
**    I_FUNCTION :  uw_redisplay_dragmenu(int menunum)
**       Re-Display dragging (tore off) menu with only menu item updated
**			the tore off menu structure already there but need update menu item
**			following the POPUP menu
**			
**    PARAMETERS   
**       INPUT  : 
**          menunum   = Menu to be tore off.
**       OUTPUT :  
**          None
**    RETURNS      : -1: no drag menu displayed
**					0: drag menu displayed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_redisplay_dragmenu(int menunum)
{
	int i, num, nc;
	POINT pt;
	UX_pathname tempfile;
	char *p;

	if (NCL_menu[menunum]==NULL)
		return;
	num = NCL_menu[menunum]->GetDragIdnum();
	if (num==-1)
		return;
/*
......free the old menu memory first
*/
	for (i=0; i<UDM_menu[num].num; i++)
	{
		if (UDM_menu[num].menus[i].params!=UU_NULL)
			uu_free(UDM_menu[num].menus[i].params); 
	}
	free((char*)UDM_menu[num].menus);
/*
.....copy menu structure
*/
	UDM_menu[num].menu_format = UDM_menu[menunum].menu_format;
	UDM_menu[num].num = UDM_menu[menunum].num;
	UDM_menu[num].menus = (UDM_menu_struc *)malloc(sizeof(UDM_menu_struc)*
				UDM_menu[menunum].num);
	for (int i=0; i<UDM_menu[menunum].num; i++)
	{
		strcpy(UDM_menu[num].menus[i].name, UDM_menu[menunum].menus[i].name);
		strcpy(UDM_menu[num].menus[i].file, UDM_menu[menunum].menus[i].file);
		strcpy(UDM_menu[num].menus[i].chcfile, UDM_menu[menunum].menus[i].chcfile);
		UDM_menu[num].menus[i].chcdef = UDM_menu[menunum].menus[i].chcdef;
		if (UDM_menu[menunum].menus[i].params==UU_NULL)
			UDM_menu[num].menus[i].params = NULL; 
		else
		{
			nc = strlen(UDM_menu[menunum].menus[i].params);
			UDM_menu[num].menus[i].params = uu_malloc((nc+1)*sizeof(char));
			strcpy(UDM_menu[num].menus[i].params, UDM_menu[menunum].menus[i].params);
		}
		strcpy(UDM_menu[num].menus[i].descrip, UDM_menu[menunum].menus[i].descrip);
		strcpy(UDM_menu[num].menus[i].toggle_def, UDM_menu[menunum].menus[i].toggle_def);
		UDM_menu[num].menus[i].pos[0] = UDM_menu[menunum].menus[i].pos[0];
		UDM_menu[num].menus[i].pos[1] = UDM_menu[menunum].menus[i].pos[1];
		UDM_menu[num].menus[i].size[0] = UDM_menu[menunum].menus[i].size[0];
		UDM_menu[num].menus[i].size[1] = UDM_menu[menunum].menus[i].size[1];
		UDM_menu[num].menus[i].kinc = UDM_menu[menunum].menus[i].kinc;
		UDM_menu[num].menus[i].toggle_num = UDM_menu[menunum].menus[i].toggle_num;
		UDM_menu[num].menus[i].toggle = UDM_menu[menunum].menus[i].toggle;
		strcpy(UDM_menu[num].menus[i].bmpfile, UDM_menu[menunum].menus[i].bmpfile);
		UDM_menu[num].menus[i].bmpnum = UDM_menu[menunum].menus[i].bmpnum;
		UDM_menu[num].menus[i].separator = UDM_menu[menunum].menus[i].separator;
	}
	uw_ntmenu(num, 1);
}

/**********************************************************************
**    I_FUNCTION :  uw_redisplay_popmenu(int kinc)
**       redisplay a popup menu based on its menu index. 
**			
**    PARAMETERS   
**       INPUT  : 
**          kinc   = menu index of the menu to be removed.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_redisplay_popmenu(int kinc)
{
	int i, k, m, current_menu,toolnum, parent, subnum;
	char tempstr[200];
	UINT fid;
	int bnum, status, defnum, sub;
	HBITMAP hBmap, hchkBmap = NULL;
	HBITMAP defhBmap = NULL;
	int defload = 0;
	int bmpnum[100];
	POINT pt;
	int menu_pos[5], menu_size[2];
	UX_pathname filename,dir;
	char fname[UX_MAX_FILE_LEN];
	CRect rect;
	int sizex, sizey;	
	MENUINFO info;

	HDC thdc = NCL_MainFrame->GetDC()->m_hDC;
	int stat = 0;
///////////
	if (NCL_menu[kinc]==NULL)
		return;
/*
.....check menu handler still there, it may delete by parent menu
*/
	if (!(::IsMenu(NCL_menu[kinc]->m_hMenu)))
		return;

	UINT tearID;
	CMenu* menuitem;
	int menucount = NCL_menu[kinc]->GetMenuItemCount();

	tearID = WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM + kinc;
	NCL_menu[kinc]->InsertMenu(-1, MF_STRING|MF_OWNERDRAW|MF_BYPOSITION,tearID, " ");

	for (k=0;k<UDM_menu[kinc].num;k++)
	{
		if (UDM_menu[kinc].menus[k].separator==0)
		{
			stat = uz_ntcnvtfunc_to_num(UDM_menu[kinc].menus[k].file, 
						UDM_menu[kinc].menus[k].params, 
						UDM_menu[kinc].menus[k].descrip, &bnum);
			fid = bnum + WM_APP + UDM_MAX_MENU + UZ_MAX_KEYITEM;
			if (stat==-1)
/*
.....then it must be a menu name
.....load this menu
*/
			{
/*
.....See if this menu is already loaded
*/
				for (m=0;m<UDM_menu_count;m++)
				{
					strcpy(filename, UDM_menu[m].file);
					ul_break_fname(filename,dir,fname);

					if (strcmp(fname, UDM_menu[kinc].menus[k].file) == 0)
					{
						current_menu = m;
						break;
					}
				}
/*
.....not loaded, load it
*/
				if (m >= UDM_menu_count)
				{
					current_menu = UDM_menu_count;
					if (udm_read_menu(UDM_menu[kinc].menus[k].file,
							UDM_menu[kinc].menus[k].pos,
							UDM_menu[kinc].menus[k].size, 
							0, 0, -1) != 0)
					{
						current_menu = -1;
					}
				}
/*
......only if the will be AppendMenu is the "PULLDOWN" type 
......of the menu, we Append this menu
*/
				if (UDM_menu[current_menu].type == UDM_MTYPE_PULLDOWN)
				{
					if (current_menu!=-1)
					{
						NCL_menu[kinc]->AppendMenu(MF_OWNERDRAW|MF_STRING|MF_ENABLED|MF_POPUP, 
							(UINT)(NCL_menu[current_menu]->m_hMenu), UDM_menu[kinc].menus[k].name);
						fid = (UINT)(NCL_menu[current_menu]->m_hMenu);
						NCL_menu[current_menu]->m_parent = kinc;
						NCL_menu[current_menu]->m_subnum = k;
					}
					else
					{
						fid = IDF_KEY_NOOP;
						NCL_menu[kinc]->AppendMenu(MF_OWNERDRAW|MF_STRING|MF_ENABLED|MF_POPUP, 
								fid, UDM_menu[kinc].menus[k].name);
					}
					if (UDM_menu[kinc].menu_format!=1)
					{
						if ((k==0)&&(UDM_menu[kinc].bmpfile[0]!='\0'))
						{
							NCL_menu[kinc]->m_hCBitmap = uw_get_bitmap(UDM_menu[kinc].bmpfile, thdc,-2);
							if (NCL_menu[kinc]->m_hCBitmap!=NULL)
								defload = 1;
							else
								defload = 0;
						}
						if (UDM_menu[kinc].menus[k].bmpfile[0]!='\0')
						{
							if (strcmp(UDM_menu[kinc].menus[k].bmpfile, 
										UDM_menu[kinc].bmpfile)!=0)
							{
								hBmap = uw_get_bitmap(UDM_menu[kinc].menus[k].bmpfile, thdc,-2);
								if (hBmap==NULL)
									bmpnum[k] = -1;
								else
									bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
							}
							else
							{
								if (defload==0)
									bmpnum[k] = -1;
								else
									bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
								hBmap = NULL;
							}
						}
						else
						{
							hBmap = NULL;
							bmpnum[k] = -1;
						}
					}
					else
					{
						hBmap = NULL;
						hchkBmap = NULL;
						bmpnum[k] = -1;
					}
					NCL_menu[kinc]->SetBitmap(hBmap,hchkBmap, k, fid, bmpnum[k]);
					NCL_menu[kinc]->Set_mid(k, fid);
				}
				else
/*
......otherwise, set the callback id, when this callback called
......show current_menu (using TrackPopupMenu for "POPUP"
......menu, using ShowControlBar for "ICON", "MENU" menu)
*/
				{
					stat = uz_ntmenunum_to_num(current_menu, &bnum);
					fid = bnum + WM_APP;
					if (stat==-1)
						fid = IDF_KEY_NOOP;

					if (UDM_menu[kinc].menu_format!=1)
					{
						if ((k==0)&&(UDM_menu[kinc].bmpfile[0]!='\0'))
						{
							NCL_menu[kinc]->m_hCBitmap = uw_get_bitmap(UDM_menu[kinc].bmpfile, thdc,-2);
							if (NCL_menu[kinc]->m_hCBitmap!=NULL)
								defload = 1;
							else
								defload = 0;
						}
						if (UDM_menu[kinc].menus[k].bmpfile[0]!='\0')
						{
							if (strcmp(UDM_menu[kinc].menus[k].bmpfile, 
										UDM_menu[kinc].bmpfile)!=0)
							{
								hBmap = uw_get_bitmap(UDM_menu[kinc].menus[k].bmpfile, thdc, -2);
								if (hBmap==NULL)
									bmpnum[k] = -1;
								else
									bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
							}
							else
							{
								if (defload==0)
									bmpnum[k] = -1;
								else
									bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
								hBmap = NULL;
							}
						}
						else
						{
							hBmap = NULL;
							bmpnum[k] = -1;
						}
					}
					else
					{
						hBmap = NULL;
						hchkBmap = NULL;
						bmpnum[k] = -1;
					}
					NCL_menu[kinc]->SetBitmap(hBmap,hchkBmap, k, fid, bmpnum[k]);
					NCL_menu[kinc]->Set_mid(k, fid);
					NCL_menu[kinc]->AppendMenu(MF_OWNERDRAW|MF_STRING|MF_ENABLED|MF_POPUP, 
							fid, UDM_menu[kinc].menus[k].name);
				}
			}
			else
			{
				if (UDM_menu[kinc].menu_format!=1)
				{
					if ((k==0)&&(UDM_menu[kinc].bmpfile[0]!='\0'))
					{
						NCL_menu[kinc]->m_hCBitmap = uw_get_bitmap(UDM_menu[kinc].bmpfile, thdc, -2);
						if (NCL_menu[kinc]->m_hCBitmap!=NULL)
							defload = 1;
						else
							defload = 0;
					}
					if (UDM_menu[kinc].menus[k].bmpfile[0]!='\0')
					{
						if (strcmp(UDM_menu[kinc].menus[k].bmpfile, 
									UDM_menu[kinc].bmpfile)!=0)
						{
							hBmap = uw_get_bitmap(UDM_menu[kinc].menus[k].bmpfile, thdc,-2);
							if (hBmap==NULL)
								bmpnum[k] = -1;
							else
								bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
						}
						else
						{
							if (defload==0)
								bmpnum[k] = -1;
							else
								bmpnum[k] = UDM_menu[kinc].menus[k].bmpnum;
							hBmap = NULL;
						}
					}
					else
					{
						hBmap = NULL;
						bmpnum[k] = -1;
					}
				}
				else
				{
					hBmap = NULL;
					hchkBmap = NULL;
					bmpnum[k] = -1;
				}
				NCL_menu[kinc]->SetBitmap(hBmap,hchkBmap, k, fid, bmpnum[k]);
				NCL_menu[kinc]->Set_mid(k, fid);
				NCL_menu[kinc]->InsertMenu(-1, MF_STRING|MF_OWNERDRAW|MF_BYPOSITION,fid, 
						UDM_menu[kinc].menus[k].name);
			}
		}
		else
		{
			NCL_menu[kinc]->InsertMenu(-1, MF_SEPARATOR|MF_BYPOSITION);
			NCL_menu[kinc]->Set_mid(k, 0);
		}
	}
/*
.....delete original Popup menu item
.....after changing
*/
	for (i=0;i<menucount;i++)
	{
		NCL_menu[kinc]->RemoveMenu(0, MF_BYPOSITION);
	}
	UDM_menu[kinc].drawitem = 1;
	NCL_MainFrame->RecalcLayout();
}

/**********************************************************************
**    I_FUNCTION :  uw_ntremove_menu(menu_num)
**       Remove a menu based on its menu index. 
**			This function do not remove main menubar
**    PARAMETERS   
**       INPUT  : 
**          menu_num   = menu index of the menu to be removed.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntremove_menu(int menu_num)
{
	int i,j,args[3], num;
	char buf[256];
/*
.....Search for the requested menu
*/
	if (UDM_menu[menu_num].type == UDM_MTYPE_MENUBAR)
		return;
	if ((UDM_menu[menu_num].type == UDM_MTYPE_PULLDOWN)
			|| (UDM_menu[menu_num].type == UDM_MTYPE_POPUP))
	{
		if (NCL_menu[menu_num]==NULL)
			return;
/*
.....check menu handler still there, it may delete by parent menu
*/
		if (!(::IsMenu(NCL_menu[menu_num]->m_hMenu)))
			return;
/*
......check drag/tear off menu of POPUP menu
*/
		num = NCL_menu[menu_num]->GetDragIdnum();
		if (num!=-1)
		{
			if (NCL_MainFrame->NCL_menubar[num] != NULL)
			{
				CImageList *rimglist = NULL;
				CToolBarCtrl &rctrl = NCL_MainFrame->NCL_menubar[num]->GetToolBarCtrl();
				rimglist = rctrl.GetImageList();
				if (rimglist!=NULL)
				{
					delete rimglist;
					rimglist = NULL;
				}
				delete (NCL_MainFrame->NCL_menubar[num]);
			}
		}
/*
......remove POPUP/PULLDOWN menu
*/
/*
......before delete this menu, we need release from it's parent menu
*/
		if (NCL_menu[menu_num]->m_parent!=-1)
		{
			int parent = NCL_menu[menu_num]->m_parent;
			int subnum = NCL_menu[menu_num]->m_subnum;
			if (NCL_menu[parent]!=NULL)
				NCL_menu[parent]->RemoveMenu((UINT)(NCL_menu[menu_num]->m_hMenu), MF_BYCOMMAND);
		}
		delete NCL_menu[menu_num];
	}
	else
	{
		if (NCL_MainFrame->NCL_menubar[menu_num] != NULL)
		{
			CImageList *rimglist = NULL;
			CToolBarCtrl &rctrl = NCL_MainFrame->NCL_menubar[menu_num]->GetToolBarCtrl();
			rimglist = rctrl.GetImageList();
			if (rimglist!=NULL)
			{
				delete rimglist;
				rimglist = NULL;
			}
			delete (NCL_MainFrame->NCL_menubar[menu_num]);
		}
	}
	NCL_MainFrame->RecalcLayout();
}
