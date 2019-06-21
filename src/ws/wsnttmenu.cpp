/********************************************************************* 
**  NAME:  wsnttmenu.cpp
**
**			Native WinNT movable and icon menu functions
**			implementation of CToolmenu class functions
**	CONTAINS: CToolmenu class functions
**			all functions declared in wsnttmenu.h
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttmenu.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 18:03:35
*********************************************************************/

#include "wsntstdafx.h"
#include "wsntctl.h"
#include "wsntframe.h"
#include "wsnttmenu.h"
#include "wsntcfunc.h"
#include "zkeysym.h"
#include "wsntres.h"
#include "dmotif.h"
#include "wsntbitmap.h"


int UW_NTDRAG;
extern "C" int UW_icon_size, UW_menu_fmt;

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

extern CMainFrame *NCL_MainFrame;
extern "C" void uw_ntpopchoice(int mflag, int *choice);
extern "C" void ud_upt_UDMmenu(int upt_menu, int upt_item, int add_menu, int add_item, int flag);
extern "C" void ud_del_UDMmenu(int, int);
extern "C" int uw_ntget_menunum(int menu);
extern CImageList * uw_create_iconimg();
extern "C" int uw_ntmenu_desgn(int flag, UDM_menu_struc *menu_item);
extern "C" void ud_insert_UDMmenu(int menunum, int select, UDM_menu_struc *menu_item);
extern "C" void ud_save_dragmenu(int);
extern "C" int ud_frm_getmenu_name(char*, int*, char*, int*, int*, char*, int type);

extern int _afxDropDownWidth;

#define ARROW_X _afxDropDownWidth
/////////////////////////////////////////////////////////////////////////////
// CToolmenu

BEGIN_MESSAGE_MAP(CToolmenu, CToolBar)
	//{{AFX_MSG_MAP(CToolmenu)
	ON_WM_RBUTTONUP()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CToolmenu construction/destruction

/***********************************************************************
**
**   FUNCTION: CToolmenu
**
**              Constructor of class CToolmenu
**			mtype: 1: icon only
**					2: text only
**					3: text and icon mixed
**					4: for NCL POPUP choice menu (text only)
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CToolmenu::CToolmenu(int menunum, int mtype)
{
	int i;
	m_barnum = menunum;
	for (i=0; i<100; i++)
		m_dropid[i] = -1;

	m_visible = 0;
	m_created = 0;
	m_nColumns = 1;
	m_mtype = mtype;
	pos[0] = 100;
	pos[1] = 100;
	pos[2] = 1;
	m_sizex = -1;
	m_sizey = -1;
/*
.....initial backgroup brush for toolbar
*/
	m_ptoolBkBrush = new CBrush(RGB(255, 255, 255));
	m_floating = -1;
	if ((UW_icon_size==0)|| ((m_mtype==2)||(m_mtype==4)))
	{
		m_sizeImage.cx = 16;
		m_sizeImage.cy = 16;
		m_sizeButton.cx = 23;
		m_sizeButton.cy = 23;
	}
	else if ((UW_icon_size==1)&&(m_mtype!=2)&&(m_mtype!=4))
	{
		m_sizeImage.cx = 24;
		m_sizeImage.cy = 24;
		m_sizeButton.cx = 31;
		m_sizeButton.cy = 31;
	}
	else if ((UW_icon_size==2)&&(m_mtype!=2)&&(m_mtype!=4))
	{
		m_sizeImage.cx = 32;
		m_sizeImage.cy = 32;
		m_sizeButton.cx = 39;
		m_sizeButton.cy = 39;
	}
	else if ((UW_icon_size==3)&&(m_mtype!=2)&&(m_mtype!=4))
	{
		m_sizeImage.cx = 40;
		m_sizeImage.cy = 40;
		m_sizeButton.cx = 47;
		m_sizeButton.cy = 47;
	}
	else if ((UW_icon_size==4)&&(m_mtype!=2)&&(m_mtype!=4))
	{
		m_sizeImage.cx = 48;
		m_sizeImage.cy = 48;
		m_sizeButton.cx = 55;
		m_sizeButton.cy = 55;
	}
	else
	{
		m_sizeImage.cx = 16;
		m_sizeImage.cy = 16;
		m_sizeButton.cx = 23;
		m_sizeButton.cy = 23;
	}

	m_first = 1;
	m_defchg = 0;
}

/***********************************************************************
**
**   FUNCTION: SetBitmapSize(int cx, int cy)
**
**              Set Bitmap Size
**
**   INPUT:  cx, cy: bitmap size to be set
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CToolmenu::SetBitmapSize(int cx, int cy)
{
	int dx, dy;
	dx = cx - m_sizeImage.cx ;
	dy = cy - m_sizeImage.cy ;
	m_sizeImage.cx = cx;
	m_sizeImage.cy = cy;
	if (UDM_menu[m_barnum].menu_format==0)
	{
		m_sizeButton.cx = m_sizeImage.cx + 7;
		m_sizeButton.cy = m_sizeImage.cy + 7;
	}
	else
	{
		m_sizeButton.cx += dx;
		m_sizeButton.cy += dy;
	}
	SetSizes(m_sizeButton, m_sizeImage);
}
/***********************************************************************
**
**   FUNCTION: PostNcDestroy
**
**        called after the window has been destroyed
**			free meemory
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CToolmenu::PostNcDestroy() 
{
	delete m_ptoolBkBrush;
}


/***********************************************************************
**
**   FUNCTION: ~CToolmenu
**
**              Destructor of class CToolmenu
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CToolmenu::~CToolmenu()
{
}

/***********************************************************************
**
**   FUNCTION: SetTBSize(int cx, int cy)
**
**        Set toolbar button size
**
**   INPUT:  cx: width
**			cy:: height
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CToolmenu::SetTBSize(int cx, int cy)
{
	m_sizex = cx;
	m_sizey = cy;
}

/***********************************************************************
**
**   FUNCTION: SetPos( menu_pos[5])
**
**        Set toolbar position
**
**   INPUT:  menu_pos: toolbar position
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CToolmenu::SetPos(int menu_pos[5])
{
	pos[0] = menu_pos[0];
	pos[1] = menu_pos[1];
	pos[2] = menu_pos[2];
	pos[3] = menu_pos[3];
	pos[4] = menu_pos[4];
}

/////////////////////////////////////////////////////////////////////////////
// CToolmenu diagnostics

#ifdef _DEBUG
void CToolmenu::AssertValid() const
{
	CToolBar::AssertValid();
}

void CToolmenu::Dump(CDumpContext& dc) const
{
	CToolBar::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////

/***********************************************************************
**
**   FUNCTION: SetColumns(UINT nColumns)
**
**        Set toolbar column number
**
**   INPUT:  nColumns: column number
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CToolmenu::SetColumns(UINT nColumns, int wrap)
{
	m_nColumns = nColumns;
	if (wrap==1)
	{
		int nCount = GetToolBarCtrl().GetButtonCount();
		for (int i = 0; i < nCount; i++)
		{
			UINT nStyle = GetButtonStyle(i);
			BOOL bWrap = (((i + 1) % nColumns) == 0);
			if (bWrap)
				nStyle |= TBBS_WRAPPED;
			else
				nStyle &= ~TBBS_WRAPPED;
			SetButtonStyle(i, nStyle);
		}
		Invalidate();
	}
	else
	{
		SetWraped(wrap);
	}
}

/***********************************************************************
**
**   FUNCTION: SetLabels(tbblabel *labels, BOOL bShow)
**
**        Set toolbar labels
**
**   INPUT:  labels: structure which have label info
**				bShow: show the toolbar
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CToolmenu::SetLabels(tbblabel *labels, BOOL bShow)
{
	UINT nID, nStyle;
	int iImage;
	char temp[256];
	int icon_set_text = 0;
/*
.....try to set the font size, not working
.....the weight will be strong than just normal
*/
//	CFont aFont;
//	aFont.CreatePointFont(80, "MS Sans Serif", NULL);
//	SetFont(&aFont, TRUE);
			
	int cButtons = GetToolBarCtrl().GetButtonCount();

	if ((UDM_menu[m_barnum].menu_format==0)&&(HasAllMenuIcon()==1))
	{
		SIZE bsize;
		bsize.cx = -1;
		bsize.cy = -1;
		for( int nButton = 0; nButton < cButtons; nButton++ )
		{
			RemoveButtonText(nButton);
		}
		m_mtype = 1;
		SetButSizes(bsize);
		return;
	}
	for( int nButton = 0; nButton < cButtons; nButton++ )
	{
		if (labels[nButton].label[0] == '\0')
		{
/*
.....remove label in case this button already have
.....(label stored in button info structure, need removed)
*/
			RemoveButtonText(nButton);
		}
		else
		{
			if (!SetButtonText( nButton, labels[nButton].label ))	
				TRACE("failed SetButtonText(%d, \"%s\").\n",
						nButton, labels[nButton].label);
/*
......if only one button, for some reason,
......the button size will not be set until the second
......button set, but if the second time button label
......is the same as before, it will not set in 
......'SetText' function. So, add a space and set
*/
			else if ((nButton==0)&&(cButtons==1))
			{
				strcpy_s(temp, sizeof(temp), labels[nButton].label);
				strcat_s(temp, " ");
				SetButtonText( nButton,  temp);
			}
		}
	}	
/*
......Get the first button size after adding the labels.
*/
/*
.....Item Rect is not the button size now, 
.....it could be button size plus 'dropdown arrow size'
.....Yurong
*/
	GetItemRect(0, &m_rBigBtn);
	GetButtonInfo(0, nID, nStyle, iImage);
	if (nStyle&TBSTYLE_DROPDOWN)
	{
		m_rBigBtn.right -= ARROW_X;
	}
	ShowLabels(bShow);
} 

void CToolmenu::SetButSizes(SIZE bsize)
{
	if (bsize.cx<=0)
	{
		if ((UW_icon_size==0) || (m_mtype==2) || (m_mtype==4))
		{
			m_sizeImage.cx = 16;
			bsize.cx = 23;
		}
		else if ((UW_icon_size==1)&&(m_mtype!=2)&&(m_mtype!=4))
		{
			m_sizeImage.cx = 24;
			bsize.cx = 31;
		}
		else if ((UW_icon_size==2)&&(m_mtype!=2)&&(m_mtype!=4))
		{
			m_sizeImage.cx = 32;
			bsize.cx = 39;
		}
		else if ((UW_icon_size==3)&&(m_mtype!=2)&&(m_mtype!=4))
		{
			m_sizeImage.cx = 40;
			bsize.cx = 47;
		}
		else if ((UW_icon_size==4)&&(m_mtype!=2)&&(m_mtype!=4))
		{
			m_sizeImage.cx = 48;
			bsize.cx = 55;
		}
		else
		{
			m_sizeImage.cx = 16;
			bsize.cx = 23;
		}
	}
	if (bsize.cy<=0)
	{
		if ((UW_icon_size==0) || (m_mtype==2) | (m_mtype==4) )
		{
			m_sizeImage.cx = 16;
			bsize.cy = 23;
		}
		else if ((UW_icon_size==1)&&(m_mtype!=2)&&(m_mtype!=4))
		{
			m_sizeImage.cx = 24;
			bsize.cy = 31;
		}
		else if ((UW_icon_size==2)&&(m_mtype!=2)&&(m_mtype!=4))
		{
			m_sizeImage.cx = 32;
			bsize.cy = 39;
		}
		else if ((UW_icon_size==3)&&(m_mtype!=2)&&(m_mtype!=4))
		{
			m_sizeImage.cx = 40;
			bsize.cy = 47;
		}
		else if ((UW_icon_size==4)&&(m_mtype!=2)&&(m_mtype!=4))
		{
			m_sizeImage.cx = 48;
			bsize.cy = 55;
		}
		else
		{
			m_sizeImage.cx = 16;
			bsize.cy = 23;
		}
	}
	SetSizes(bsize, m_sizeImage);
}

/***********************************************************************
**
**   FUNCTION: ShowLabels(BOOL bShow)
**
**        show the toolbar with new label
**
**   INPUT:  
**			bShow: show the toolbar
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CToolmenu::ShowLabels(BOOL bShow)
{
	int x = GetSystemMetrics(SM_CXICON);
	int y = GetSystemMetrics(SM_CYICON);

	if ((UW_icon_size==0) || (m_mtype==2) | (m_mtype==4))
	{
		m_sizeImage.cx = 16;
		m_sizeImage.cy = 16;
		m_sizeButton.cx = 23;
		m_sizeButton.cy = 23;
	}
	else if ((UW_icon_size==1)&&(m_mtype!=2)&&(m_mtype!=4))
	{
		m_sizeImage.cx = 24;
		m_sizeImage.cy = 24;
		m_sizeButton.cx = 31;
		m_sizeButton.cy = 31;
	}
	else if ((UW_icon_size==2)&&(m_mtype!=2)&&(m_mtype!=4))
	{
		m_sizeImage.cx = 32;
		m_sizeImage.cy = 32;
		m_sizeButton.cx = 39;
		m_sizeButton.cy = 39;
	}
	else if ((UW_icon_size==3)&&(m_mtype!=2)&&(m_mtype!=4))
	{
		m_sizeImage.cx = 40;
		m_sizeImage.cy = 40;
		m_sizeButton.cx = 47;
		m_sizeButton.cy = 47;
	}
	else if ((UW_icon_size==4)&&(m_mtype!=2)&&(m_mtype!=4))
	{
		m_sizeImage.cx = 48;
		m_sizeImage.cy = 48;
		m_sizeButton.cx = 55;
		m_sizeButton.cy = 55;
	}
	else
	{
		m_sizeImage.cx = 16;
		m_sizeImage.cy = 16;
		m_sizeButton.cx = 23;
		m_sizeButton.cy = 23;
	}
/*
.....Set the toolbar button sizes.
*/
	if (bShow)
	{
		ModifyStyle(0, TBSTYLE_LIST);
		SIZE bsize;
/*
......m_sizeImage.cx + 7 is the space of image on the button
......m_rBigBtn.Size only have space for label
*/
		if ((m_mtype==3)||(m_mtype==1))
		{
			if (m_sizex<=0)
			{
				if (m_first==1)
					bsize.cx = m_rBigBtn.right - m_rBigBtn.left + m_sizeImage.cx + 5;
				else
					bsize.cx = m_rBigBtn.right - m_rBigBtn.left;
			}
			else
			{
				bsize.cx = m_sizex;
			}
			if (m_sizey<=0)
			{
				bsize.cy = m_sizeButton.cy;
			}
			else
			{
				bsize.cy = m_sizey;
			}
			SetSizes(bsize, m_sizeImage);
		}
		else if ((m_mtype==2)||(m_mtype==4))
		{
/*
......text only
......set image size to min
*/
			SIZE sizeImage;
			if (m_sizex<=0)
			{
				if (m_first==1)
					bsize.cx = m_rBigBtn.right - m_rBigBtn.left+5;
				else
					bsize.cx = m_rBigBtn.right - m_rBigBtn.left;
			}
			else
			{
				bsize.cx = m_sizex;
			}
			if (m_sizey<=0)
			{
				CClientDC dc(this);
				CSize sizeText = dc.GetTextExtent("ABCD",4);
//consistent with SetSize from toolbar function: + 6 pixels on y
				bsize.cy = sizeText.cy + 6;
			}
			else
			{
				bsize.cy = m_sizey;
			}
			sizeImage.cx = 0;
			sizeImage.cy = 0;
			SetSizes(bsize, sizeImage);
		}
	}
	SendMessage(TB_AUTOSIZE);
	m_bDelayedButtonLayout=TRUE;
	::InvalidateRect(*this,NULL,FALSE);
	m_first = 0;
} 
/***********************************************************************
**
**   FUNCTION: HasAllMenuIcon
**
**         check if this menu have icon on every item
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    1: yes
**
**********************************************************************/
int CToolmenu::HasAllMenuIcon()
{
	UINT nID, nStyle;
	int iImage;
	int nCount = GetToolBarCtrl().GetButtonCount();
	for (int i = 0; i < nCount; i++)
	{
		GetButtonInfo(i, nID, nStyle, iImage);
		if (iImage==-1)
/*
.....if no icon but no text also, then
.....if no any icon exist, we treat it as if we have a label
.....if no any label exist, we treat it as if we have a icon
*/
		{
			if (UDM_menu[m_barnum].menus[i].name[0]=='\0')
				continue;
			else
				return 0;
		}
	}
	return 1;
}
/***********************************************************************
**
**   FUNCTION: OnRButtonUp(UINT nFlags, CPoint pt)
**
**       callback for right mouse button up
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CToolmenu::OnRButtonUp(UINT nFlags, CPoint pt) 
{
	UDM_menu_struc menu_item;
	int status, choice = 0;
	CNCLToolBar::OnRButtonUp(nFlags, pt);

	int select;
	TOOLINFO pTI;
	pTI.cbSize = sizeof(TOOLINFO);

	select = -1;
	status = GetItemIndx_hit(pt, select);
	if (status==-1)
/*
.....in no-client area, handle the menu save/edit
*/
	{
		HandleRButtonUp(-1);
		return;
	}
	int menunum, menu;
	int savcols;
	if (select!=-1)
	{
		menunum = m_barnum;
		menu = uw_ntget_menunum (menunum);
		if (menu!=-1)
			menunum = menu;
/*
......display popup menu " Insert Separator, Insert Item, Delete Item, Cancel"
*/
		uw_ntpopchoice(3, &choice);
		if (choice==0)
			return;
		status = 0;
		savcols = -1;
		if ((NCL_MainFrame->NCL_menubar[menunum]!=NULL)&&(NCL_MainFrame->NCL_menubar[menunum])->IsFloating())
			savcols = NCL_MainFrame->NCL_menubar[menunum]->GetCols();	
		if (choice==1)
		{
/*
......delete item
*/
/*
......updated UDM_menu[m_barnum] with delete one item in menu = menunum1, item = itemnum1
*/
			if (savcols!=-1)
				UDM_menu[menunum].cols = savcols;
			ud_del_UDMmenu(menunum, select);
			NCL_MainFrame->m_addmenu = -1;
			NCL_MainFrame->m_remove_menu = menunum;
			NCL_MainFrame->m_remove_item = select;
		}
		else if (choice==2)
/*
.....edit select item
*/
		{
			status = uw_ntmenu_desgn(1, &(UDM_menu[menunum].menus[select]));
			if (status!=-1)
			{
				if (savcols!=-1)
					UDM_menu[menunum].cols = savcols;
/*
......do not automatically save a menu anymore
*/
//				ud_save_dragmenu(menunum);
				NCL_MainFrame->m_addmenu = menunum;
				NCL_MainFrame->m_additem = select;
				NCL_MainFrame->m_remove_menu = menunum;
				NCL_MainFrame->m_remove_item = select;
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
				if (savcols!=-1)
					UDM_menu[menunum].cols = savcols;
				ud_insert_UDMmenu(menunum, select, &menu_item);
				NCL_MainFrame->m_addmenu = menunum;
				NCL_MainFrame->m_additem = select;
				NCL_MainFrame->m_remove_menu = -1;
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
			if (savcols!=-1)
				UDM_menu[menunum].cols = savcols;
			ud_insert_UDMmenu(menunum, select, &menu_item);
			NCL_MainFrame->m_addmenu = menunum;
			NCL_MainFrame->m_additem = select;
			NCL_MainFrame->m_remove_menu = -1;
		}
/*
.....send message to Frame level to updated the menu display
.....we use POstMessage because we have to destroy the menu then redisplay
.....but don't want to mess up the message loop
*/
		if (status!=-1)
			NCL_MainFrame->PostMessage(WM_COMMAND, UW_UPDATE_MENUBAR);
	}
}
/***********************************************************************
**
**   FUNCTION: UpdateMenuBar(CPoint pt, char* input_text)
**
**       callback for right mouse button up
**
**   INPUT: pt: the position the float menu display
**			input_text: text include the menu info we need add
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CToolmenu::UpdateMenuBar(CPoint pt, char* input_text)
{
	int menunum1, itemnum1, choice, menunum2, itemnum2, stat_but;
	char menudata[100];
/*
......only to menubar
*/
//	if (m_bartype!=0)
//		return;
/*
.....don't accept status menu bar into a tool menubar
*/
	menunum1 = itemnum1 = -1;
	int menu_desgn = 0;
	strcpy_s(menudata, sizeof(menudata), input_text);
	if ((strncmp(menudata, "CToolmenu2", 10)==0)&&(m_bartype==0))
		return;
	if ((strncmp(menudata, "CNCLStatButton2", 15)==0)&&(m_bartype==0))
		return;
/*
.....don't accept menubar move to statusbar
*/
	if ((strncmp(menudata, "CNCLStatButton", 14)==0)&&
		(strncmp(menudata, "CNCLStatButton2", 15)!=0)&&(m_bartype==1))
		return;
	int menu_status = 0;
	if (strncmp(menudata, "CNCLStatButton2", 15)==0)
	{
/*
.....status bar move here to a status menubar (fix size)
*/
		sscanf_s (menudata, "CNCLStatButton2 %d, %d", &menunum1, &itemnum1);
		menu_status = 1;
	}
	else if (strncmp(menudata, "CNCLStatButton", 14)==0)
	{
		sscanf_s (menudata, "CNCLStatButton %d, %d", &menunum1, &itemnum1);
	}
	else if (strncmp(menudata, "CToolmenu2", 10)==0)
		sscanf_s (menudata, "CToolmenu2 %d, %d", &menunum1, &itemnum1);
	else if (strncmp(menudata, "CToolmenu", 9)==0)
		sscanf_s (menudata, "CToolmenu %d, %d", &menunum1, &itemnum1);
	else if (strncmp(menudata, "CNCLMenu", 8)==0)
		sscanf_s (menudata, "CNCLMenu %d, %d", &menunum1, &itemnum1);
	else if (strcmp(menudata, "MenuDesign")==0)
	{
		CNCLMenuDsnDlg *dlg = ((CMainFrame *)NCL_MainFrame)->m_menudsgn_dlg;
		if (dlg->m_menu_item.statname[0]!='\0')
			stat_but = 1;
		else
			stat_but = 0;
		if (((stat_but==0)&&(m_bartype==1))
			|| ((stat_but==1)&&(m_bartype==0)))
			return;
		menu_desgn = 1;
	}
	else
/*
.....should not come here
*/
		return;
/*
......get the button position of the drop and insert this item into current
......menubar and update the menu bar
*/
/*
.....current menu number m_barnum, item num
*/
	int flag;
	menunum2 = m_barnum;
	CheckClosestBut(pt, itemnum2, flag);
	if (flag)
		itemnum2++;
/*
.....moved in same menu, just move without ask
*/
	if ((m_barnum!=menunum1)||(menu_desgn))
	{
		if (menu_desgn)
		{
			NCL_MainFrame->m_menu_desgn = menu_desgn;
			ud_insert_UDMmenu(menunum2, itemnum2, &((NCL_MainFrame->m_menudsgn_dlg)->m_menu_item));
			NCL_MainFrame->m_addmenu = menunum2;
			NCL_MainFrame->m_additem = itemnum2;
			NCL_MainFrame->m_remove_menu = -1;
			NCL_MainFrame->PostMessage(WM_COMMAND, UW_UPDATE_MENUBAR);
			NCL_MainFrame->RecalcLayout();
			return;
		}
/*
......open a POPUP menu with "Copy Here" (choice=2), "Move Here", "Cancel" (choice=0)
*/
		uw_ntpopchoice(0, &choice);
		if ((choice==0)||(choice==-1))
			return;
	}
/*
.....same menu, same item, ignore
*/
	else if (itemnum2==itemnum1)
		return;
	else
		choice = 1;
	MovedMenuItem(itemnum2, menunum1, itemnum1, choice);
	NCL_MainFrame->RecalcLayout();
}
/***********************************************************************
**
**   FUNCTION: MovedMenuItem(int itemnum2, int menunum1, int itemnum1, int choice)
**
**       move itemnum1 of menunum1 to this menu bar's itemnum2
**
**   INPUT: menunum1, itemnum2: menu item to move from
**			itemnum2: menu item point to
**			choice: 1: move, 2: copy
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CToolmenu::MovedMenuItem(int itemnum2, int menunum1, int itemnum1, int choice)
{
	int menu, menunum2 = m_barnum;
/*
.....this menunum1, menunum2 could be a drag_menu from POP MENU (temp created 
.....when click the draging bar of the NCL POP MENU), so we need find the real
.....POP MENU number
*/
	menu = uw_ntget_menunum (menunum1);
	if (menu!=-1)
		menunum1 = menu;
	menu = uw_ntget_menunum (menunum2);
	if (menu!=-1)
		menunum2 = menu;
/*
......updated UDM_menu[m_barnum] with added one item in menu = menunum1, item = itemnum1
*/
	ud_upt_UDMmenu(menunum2, itemnum2, menunum1, itemnum1,choice);
/*
.....send message to Frame level to updated the menu display
*/
	NCL_MainFrame->m_addmenu = menunum2;
	NCL_MainFrame->m_additem = itemnum2;
	if ((choice==1)&&(menunum2!=menunum1))
	{
		NCL_MainFrame->m_remove_menu = menunum1;
		NCL_MainFrame->m_remove_item = itemnum1;
	}		
	else
		NCL_MainFrame->m_remove_menu = -1;
	NCL_MainFrame->PostMessage(WM_COMMAND, UW_UPDATE_MENUBAR);
}
/***********************************************************************
**
**   FUNCTION: GetItemIndx_hit(CPoint point, int &hit)
**
**       check to see if point is on the item of the bar and return the index of hit item
**
**   INPUT: point: posiiton
**
**   OUTPUT :   hit: item index of hit
**   RETURN:    -1: no hit
**
**********************************************************************/
int CToolmenu::GetItemIndx_hit(CPoint point, int &hit)
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));

	CRect prev_rect, rect;
	int status;
	int nButtons = DefWindowProc(TB_BUTTONCOUNT, 0, 0);
	for (int i = 0; i < nButtons; i++)
	{
		TBBUTTON button;
		status = DefWindowProc(TB_GETITEMRECT, i, (LPARAM)&rect);
		if (status==-1)
			continue;
		status = DefWindowProc(TB_GETBUTTON, i, (LPARAM)&button);
		if (status==-1)
			continue;
		++rect.bottom;
		++rect.right;
		if (rect.PtInRect(point))
		{
			hit = i;
			return 0;
		}
		if (button.fsStyle & TBSTYLE_SEP)
		{
/*
.....for some reason, if it's the seperate, rect is wrong when more than one column
*/
			status = status;
		}
		prev_rect = rect;
	}
	return -1;
}
/***********************************************************************
**
**   FUNCTION: HandleRButtonUp
**
**       handle right mouse button click
**
**   INPUT: none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CToolmenu::HandleRButtonUp(int flag)
{
	UX_pathname menufile, dir, fdir;
	int status, menu_type, format, dockable, len, choice, saved_menu_type, changed;
	char menu_area[256], title[30];
	if (flag==-1)
		uw_ntpopchoice(4, &choice);
	else
		choice = flag;
	if ((choice!=1)&&(choice!=2)&&(choice!=3))
		return;
/*
......get current menu file name
*/
	int menunum = m_barnum;
	strcpy(menufile, UDM_menu[menunum].file);
	if (strncmp(UDM_menu[menunum].file,"Active_Temp_Menu", 16)==0)
		menufile[0] = '\0';
	if (choice==3)
	{
/*
.....edit menu and return
*/
		strcpy(menu_area, UDM_menu[menunum].menu_area);
		dockable = UDM_menu[menunum].dockable;
		format = UDM_menu[menunum].menu_format;
		strcpy(title, UDM_menu[menunum].name);
		menu_type = UDM_menu[menunum].type;
		saved_menu_type = menu_type;
		status = ud_frm_getmenu_name(menufile, &menu_type, menu_area, &format, &dockable, title, m_bartype);
		if (status==-1)
			return;
		changed = 0;
		if (strcmp(UDM_menu[menunum].name, title)!=0)
		{
			strcpy(UDM_menu[menunum].name, title);
			changed = 1;
		}
		if (UDM_menu[menunum].type != menu_type)
		{
			UDM_menu[menunum].type = menu_type;
			changed = 1;
		}
		if (strcmp(UDM_menu[menunum].menu_area, menu_area)!=0)
		{
			strcpy(UDM_menu[menunum].menu_area, menu_area);
			changed = 1;
		}
		if (UDM_menu[menunum].menu_format != format)
		{
			UDM_menu[menunum].menu_format = format;
			changed = 1;
		}
		if (UDM_menu[menunum].dockable != dockable)
		{
			UDM_menu[menunum].dockable = dockable;
			changed = 1;
		}
		if ((changed)||(strcmp(menufile, UDM_menu[menunum].file)!=0))
		{
			if (menufile[0]!='\0')
/*
.....keep the original filename
*/

				strcpy(UDM_menu[menunum].file, menufile);
/*
......do not automatically save a menu anymore
*/
//			ud_save_dragmenu(menunum);
		}
/*
......redisplay the menu if anything changed
*/
		if (changed)
		{
/*
.....redisplay the menu
*/
			NCL_MainFrame->m_addmenu = menunum;
			NCL_MainFrame->m_additem = -1;
			NCL_MainFrame->m_remove_menu = menunum;
			NCL_MainFrame->m_remove_item = -1;
			NCL_MainFrame->PostMessage(WM_COMMAND, UW_UPDATE_MENUBAR);
		}
		return;
	}
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
			if (m_bartype==0)
				strcat (menufile, "\\*.menu");
			else
				strcat (menufile, "\\*.stat");
		}
		if (m_bartype==0)
			ud_get_filename(NULL,"Save Menu File","*.menu", menufile, &len,
				"NCL Menu File (*.menu)", 0);
		else
			ud_get_filename(NULL,"Save Status File","*.stat", menufile, &len,
				"NCL Status File (*.stat)", 0);
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
			if (m_bartype==0)
				strcat (menufile, "\\*.menu");
			else
				strcat (menufile, "\\*.stat");
			if (m_bartype==0)
				ud_get_filename(NULL,"Save Menu File","*.menu", menufile, &len,
					"NCL Menu File (*.menu)", 0);
			else
				ud_get_filename(NULL,"Save Status File","*.stat", menufile, &len,
					"NCL Status File (*.stat)", 0);
			if (len<=0) return;
		}
	}

	char *p = (char *)strrchr(menufile,'.');
	if (p == 0)
	{
		if (m_bartype==0)
			strcat(menufile, ".menu");
		else
			strcat (menufile, ".stat");
	}
/*	else
	{
		*p = 0;
		strcat(menufile, ".menu");
	}
*/
	strcpy(UDM_menu[menunum].file, menufile);
	int rows, cols = 1;
	cols = GetCols();	
	int total = GetCount();
	if (total%cols==0)
		rows = total/cols;
	else
		rows = total/cols + 1;
/*
.....Get Col and Rows
*/
	UDM_menu[menunum].cols = cols;
	UDM_menu[menunum].rows = rows;
	ud_save_dragmenu(menunum);
}
