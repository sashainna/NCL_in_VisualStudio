/********************************************************************* 
**  NAME:  wsntStatusbar.cpp
**
**			Bottom Status bar related class:
**			NCLStatusBarPaneControlInfo, CNCLStatusBarPane and CNCLStatusBar class
**			implementation. 
**
**	CONTAINS: above class functions
**			all functions declared in wsntStatusbar.h
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntStatusBar.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 16:38:49
*********************************************************************/
#include "StdAfx.h"
#include <afxtempl.h>
#include "wsntStatusBar.h"
#include "wsntstatbtn.h"
#include "wsntres.h"
#include "dmotif.h"
#include "zkeysym.h"
#include "wsntcfunc.h"
#include "wsntframe.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern CMainFrame *NCL_MainFrame;
extern "C" int UW_statbar_size;
extern "C" char UW_statbar_font[20];

extern "C" int uw_ntget_menunum(int barno);
extern "C" void uw_ntpopchoice(int mflag, int *choice);
extern "C" void ud_upt_UDMmenu(int upt_menu, int upt_item, int add_menu, int add_item, int flag);
extern "C" int udm_read_menu(char *filename, int pos[3], int size[2], int kdis, int kflag,int menutype);
extern "C" int uz_ntstatname_to_num(char *name, char *func, char *descrip, int* num, int *sub)	;
extern "C" int uz_putid_stattb(int fid, int sub);
extern "C" int uz_ntcnvtfunc_to_num(char* func, char *parms, char *descrip, int *num);
extern "C" int uz_ntmenunum_to_num(int current_menu, int *bnum);
extern int uw_ntsave_menu(int type, UDM_MENU menu_struct);
extern "C" int ud_frm_getmenu_name(char*, int*, char*, int*, int*, char*, int);
////////
extern "C" int uz_ntcnvtnum_to_menu(int num, int *menunum);
extern "C" void ud_del_UDMmenu(int, int);
extern "C" int uw_ntmenu_desgn(int flag, UDM_menu_struc *menu_item);
extern "C" void ud_insert_UDMmenu(int menunum, int select, UDM_menu_struc *menu_item);

int UW_addmenu = -1;
int UW_additem = -1;
int UW_remove_menu = -1;
int UW_remove_item = -1;

/**********************************************************************
**    I_FUNCTION :  get_menunum2(int barno, int statflag)
**			Get the status pane's menu index. (when statflag = 1)
**			Or Get the menu's real index. the 'menu' could be a tear-up draging menu, 
**			we need find out and return the real menu number (when statflag = 0)
**			
**    PARAMETERS   
**       INPUT  : 
**          barno   = Statusbar pane or menu to be checked.
**			statflag: 1: the barno is a statusbar pane number
**       OUTPUT :  
**          None
**    RETURNS      : return -1: menu is not a tear-up menu
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int CNCLStatusBar::get_menunum2(int barno, int statflag)
{
	if (statflag==0)
		return uw_ntget_menunum(barno);

	int i;
	NCLStatusBarPaneControlInfo * pPanInfo;
	pPanInfo = GetPanInfo(barno);
	int indx = -1;
	for (i=0; i<UDM_menu_count; i++)
	{
		if (UDM_menu[i].statflag!=2)
			continue;
		if (stricmp(UDM_menu[i].file, pPanInfo->m_filename)==0)
		{
			indx = i;
			break;
		}
	}
	return indx;
}

////////////////////////////////////////////////////////////////////////
// NCLStatusBarPaneControlInfo
IMPLEMENT_DYNCREATE(NCLStatusBarPaneControlInfo,CObject)

NCLStatusBarPaneControlInfo::NCLStatusBarPaneControlInfo()
{
	m_hWnd = NULL;
	m_subwin_no = 0;
	for (int i=0; i<100; i++)
		m_subhWnd[i] = NULL;
	m_bAutodelete=TRUE;
	m_just = 0;
	m_changed = 0;
}

///////////////////////////////////////////////////////////////////////////////////
// CNCLStatusBarPane
CNCLStatusBarPane::CNCLStatusBarPane(CNCLStatusBarPane & pane )
{
	*this = pane;
}

CNCLStatusBarPane& CNCLStatusBarPane::operator=(CNCLStatusBarPane& pane )
{
	nID		= pane.nID;
	nText	= pane.nText;
	nStyle	= pane.nStyle;
	strText = pane.strText;
	return *this;
}

CNCLStatusBarPane::CNCLStatusBarPane()
{
}

////////////////////////////////////////////////////////////////////////
// CNCLStatusBar
IMPLEMENT_DYNCREATE(CNCLStatusBar,CStatusBar)
BEGIN_MESSAGE_MAP(CNCLStatusBar, CStatusBar)
	//{{AFX_MSG_MAP(CNCLStatusBar)
	ON_WM_SIZE()
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_RBUTTONUP()
    ON_WM_CREATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  CNCLStatusBar
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLStatusBar::CNCLStatusBar()
{
	for (int i=0; i<100; i++)
	{
		m_statbut[i] = NULL;
	}
	m_butno = 0;
	m_changed = 0;
	m_focus = 1;
	m_hititem[0] = m_hititem[1] = -1;

	m_StartPoint = CPoint(-1, -1);
	m_TargetDrop = new CNCLMnDropTarget();
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
	m_TimerID = 0;
}

CNCLStatusBar::~CNCLStatusBar()
{
/*
	for (int i=0; i<100; i++)
	{
		if (m_statbut[i]!=NULL)
			delete m_statbut[i];
		m_statbut[i] = NULL;
	}
*/
	CWnd * pwnd = NULL;
	int i, j, size = m_ctrlinfo_array.GetSize();
	for(i = 0; i < size; i++)
	{
		pwnd = m_ctrlinfo_array[0]->m_hWnd;
		if (pwnd!=NULL)
		{
			pwnd->DestroyWindow();
			delete pwnd;
			m_ctrlinfo_array[0]->m_hWnd = NULL;
		}
		for (j=0; j<m_ctrlinfo_array[0]->m_subwin_no; j++)
		{
			pwnd = m_ctrlinfo_array[0]->m_subhWnd[j];
			if (pwnd!=NULL)
			{
				pwnd->DestroyWindow();
				delete pwnd;
			}
			m_ctrlinfo_array[0]->m_subhWnd[j] = NULL;
		}
		delete m_ctrlinfo_array[0];
		m_ctrlinfo_array.RemoveAt(0);
	}	
	for( int i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
	{
		delete m_ctrlinfo_array[i];
	}
}

/***********************************************************************
**
**   FUNCTION: OnSize(UINT nType, int cx, int cy) 
**
**		this member function called
**		after the statusbar has changed. We will resize
**		the section.
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
void CNCLStatusBar::OnSize(UINT nType, int cx, int cy) 
{
	CStatusBar::OnSize(nType, cx, cy);
/*
.....resize evenly for sections
*/
	float wid_ratio;
	int i, idx, wid;
	if (UDM_layout.statbar_no>0)
	{
		wid_ratio = 1.0/UDM_layout.statbar_no;
		wid = (cx-20)*wid_ratio - 1;
	}
	for (i=0; i<UDM_layout.statbar_no;i++)
	{
		idx = CommandToIndex(INDICATOR_WIN1+i);
		if (idx>=0)
			SetPaneWidth(idx, wid);
	}
	PositionControls();
	RedrawWindow();
}
int CNCLStatusBar::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CStatusBar::OnCreate(lpCreateStruct) == -1)
		return -1;
	if (m_txtfont.m_hObject)
		VERIFY (m_txtfont.DeleteObject ());	
	m_txtfont.CreatePointFont (80, "MS Sans Serif");
	SetFont(&m_txtfont);

	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
}

/***********************************************************************
**
**   FUNCTION: PositionControls()
**
**       position the buttons on the status bar
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLStatusBar::PositionControls()
{
	int h,v,s;
	GetStatusBarCtrl( ).GetBorders( h, v, s ) ;

	UINT nID,nStyle;
	int width, hide;

	NCLStatusBarPaneControlInfo * pPanInfo;
	for(int i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
	{
		CRect rect;
		CRect rect2;
		
		int index = CommandToIndex( m_ctrlinfo_array[i]->m_nPaneID );
		GetItemRect(index,rect);

		pPanInfo = GetPanControl(m_ctrlinfo_array[i]->m_nPaneID);
		if (pPanInfo==NULL)
			continue;
	
		if (pPanInfo->m_hWnd==NULL)
			continue;

		rect2 = rect;
		if (pPanInfo->m_just==0)
		{
			rect2.left = rect.left + pPanInfo->m_rect.left;
			if (pPanInfo->m_rect.right<-1000)
				rect2.right = rect2.left + rect.Width() - 4;
			else
				rect2.right = rect2.left + pPanInfo->m_rect.Width();
/*
......check if this window is within the pane, if yes, move to rect2
......otherwise hide it
*/
			if (rect.PtInRect(CPoint(rect2.right, rect.top+2)))
				hide = 0;
			else
			{
/*
......if half in, change the button size to fit
*/
				if (rect.PtInRect(CPoint(rect2.left, rect.top+2)))
				{
					rect2.right = rect.right - 1;
					hide = 0;
				}
				else
					hide = 1;
			}
		}
		else
		{
			rect2.right = rect2.right + pPanInfo->m_rect.right;
			if (pPanInfo->m_rect.left<-1000)
				rect2.left = rect2.right - rect.Width() + 4;
			else
				rect2.left = rect2.right - pPanInfo->m_rect.Width();
/*
......check if this window is within the pane, if yes, move to rect2
......otherwise hide it
*/
			if (rect.PtInRect(CPoint(rect2.left, rect.top+2)))
				hide = 0;
			else
			{
/*
......if half in, change the button size to fit
*/
				if (rect.PtInRect(CPoint(rect2.right, rect.top+2)))
				{
					rect2.left = rect.left + 1;
					hide = 0;
				}
				else
					hide = 1;
			}
		}
		rect2.bottom = rect.bottom-1;
		if (hide==0)
		{
			pPanInfo->m_hWnd->MoveWindow(rect2);
			pPanInfo->m_hWnd->ShowWindow(SW_SHOW);
		}
		else
			pPanInfo->m_hWnd->ShowWindow(SW_HIDE);

		for (int j=0; j<pPanInfo->m_subwin_no; j++)
		{
			if (pPanInfo->m_subhWnd[j]==NULL)
				continue;
/*
.....display the sub item
*/
			rect2 = rect;
			rect2.bottom = rect.bottom-1;
			if (pPanInfo->m_just==0)
			{
				rect2.left = rect.left + pPanInfo->m_subrect[j].left;
				rect2.right = rect2.left + pPanInfo->m_subrect[j].Width();
/*
......check if this window is within the pane, if yes, move to rect2
......otherwise hide it
*/
				if (rect.PtInRect(CPoint(rect2.right, rect.top+2)))
					hide = 0;
				else
				{
	/*
	......if half in, change the button size to fit
	*/
					if (rect.PtInRect(CPoint(rect2.left, rect.top+2)))
					{
						rect2.right = rect.right - 1;
						hide = 0;
					}
					else
						hide = 1;
				}
			}
			else
			{
				rect2.right = rect2.right + pPanInfo->m_subrect[j].right;
				rect2.left = rect2.right - pPanInfo->m_subrect[j].Width();
/*
......check if this window is within the pane, if yes, move to rect2
......otherwise hide it
*/
				if (rect.PtInRect(CPoint(rect2.left, rect.top+2)))
					hide = 0;
				else
				{
/*
......if half in, change the button size to fit
*/
					if (rect.PtInRect(CPoint(rect2.right, rect.top+2)))
					{
						rect2.left = rect.left + 1;
						hide = 0;
					}
					else
						hide = 1;
				}
			}
			if (hide==0)
			{
				pPanInfo->m_subhWnd[j]->MoveWindow(rect2);
				pPanInfo->m_subhWnd[j]->ShowWindow(SW_SHOW);
			}
			else
				pPanInfo->m_subhWnd[j]->ShowWindow(SW_HIDE);
			}
	}
}

NCLStatusBarPaneControlInfo * CNCLStatusBar::GetPanControl(int nPaneID)
{
	for(int i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
	{
		if( m_ctrlinfo_array[i]->m_nPaneID == nPaneID )
			return m_ctrlinfo_array[i];
	}
	return NULL;
}

/***********************************************************************
**
**   FUNCTION: AddSubControl(CWnd * pWnd, int paneID, CRect &rect)
**
**       Added an window as the sub-control of pane window on the statusbar
**
**   INPUT:  pWnd: window to be add
**			paneID: PaneID of Pane window
**			rect: window's size
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
BOOL CNCLStatusBar::AddSubControl(CWnd * pWnd, int paneID, CRect &rect)
{
	NCLStatusBarPaneControlInfo * pPanInfo = GetPanControl(paneID);
	if( pPanInfo )
	{
		if (pPanInfo->m_subwin_no>=100)
			return FALSE;
		pPanInfo->m_subhWnd[pPanInfo->m_subwin_no] = pWnd;
		pPanInfo->m_subrect[pPanInfo->m_subwin_no] = rect;
		pPanInfo->m_subwin_no++;
		PositionControls();
		Invalidate(TRUE);
		return TRUE;
	}
}


/***********************************************************************
**
**   FUNCTION: AddControl(CWnd * pWnd, int paneID, CRect &rect, char *filen, short just, BOOL bAutodeleteControl)
**
**       Added an window as the sub-control of pane window on the statusbar
**
**   INPUT:  pWnd: window to be add
**			paneID: PaneID of Pane window
**			rect: window's size
**			filen: related menu file.
**			just: justify
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
BOOL CNCLStatusBar::AddControl(CWnd * pWnd, int paneID, CRect &rect, char *filen, short just, BOOL bAutodeleteControl)
{

	NCLStatusBarPaneControlInfo * pPanInfo = GetPanControl(paneID);
	if( pPanInfo )
	{
		return FALSE;
	}
	int idx = CommandToIndex( paneID ) ;
	if( idx == -1 )
		return FALSE;

	NCLStatusBarPaneControlInfo * pPan = new NCLStatusBarPaneControlInfo;
	pPan->m_just = just;
	pPan->m_nPaneID =  paneID;
	pPan->m_rect = rect;
	pPan->m_hWnd    =  pWnd;
	pPan->m_bAutodelete = bAutodeleteControl;
	pPan->m_filename = filen;
					
	m_ctrlinfo_array.Add(pPan);
	PositionControls();
	Invalidate(TRUE);
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: SetPaneWidth(int index, int cxWidth)
**
**       set pane window width
**
**   INPUT:  index: Pane window index
**			cxWidth: width of the pane window
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLStatusBar::SetPaneWidth(int index, int cxWidth)
{
	UINT nID,nStyle;
	int width;
	GetPaneInfo(index,nID,nStyle,width);
	SetPaneInfo(index,nID,nStyle,cxWidth);
}

/***********************************************************************
**
**   FUNCTION: GetStatusPane(int nIndex, CNCLStatusBarPane & pane)
**
**       get pane window
**
**   INPUT:  nIndex: Pane window index
**
**   OUTPUT :   pane: pane window
**   RETURN:    none
**
**********************************************************************/
BOOL CNCLStatusBar::GetStatusPane(int nIndex, CNCLStatusBarPane & pane)
{
	if( nIndex < m_nCount  && nIndex >= 0 )
	{
		GetPaneInfo( nIndex,  pane.nID, pane.nStyle, pane.nText ) ;
		GetPaneText( nIndex , pane.strText );
		return TRUE;
	}
	return FALSE;
}
/***********************************************************************
**
**   FUNCTION: RemoveAllPanes()
**
**       Remove all pane and controls in the pane. But leave one pane with control
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLStatusBar::RemoveAllPanes()
{
	int i,j,size;
	SetRedraw(FALSE);
	CWnd * pwnd = NULL;
	size = m_ctrlinfo_array.GetSize();
	for(i = 0; i < size; i++)
	{
		pwnd = m_ctrlinfo_array[0]->m_hWnd;
		if (pwnd!=NULL)
		{
			pwnd->DestroyWindow();
			delete pwnd;
			m_ctrlinfo_array[0]->m_hWnd = NULL;
		}
		for (j=0; j<m_ctrlinfo_array[0]->m_subwin_no; j++)
		{
			pwnd = m_ctrlinfo_array[0]->m_subhWnd[j];
			if (pwnd!=NULL)
			{
				pwnd->DestroyWindow();
				delete pwnd;
			}
			m_ctrlinfo_array[0]->m_subhWnd[j] = NULL;
		}
		delete m_ctrlinfo_array[0];
		m_ctrlinfo_array.RemoveAt(0);
	}	
	CArray<CNCLStatusBarPane,CNCLStatusBarPane> arPanes;
	CNCLStatusBarPane statusPane;
/*
......remove all pane except one pane
*/
	i = 0;
	while( GetStatusPane(i,statusPane) )
	{
		if ( statusPane.nID == INDICATOR_WIN1)
		{
			arPanes.Add( statusPane );
			break;
		}
		i++;
	}
	UINT * pIndicators = new UINT[arPanes.GetSize()];
	for (i=0;i<arPanes.GetSize();i++)
		pIndicators[i]=arPanes[i].nID;
	SetIndicators(pIndicators, arPanes.GetSize());

	for (i = 0; i < arPanes.GetSize(); i++ )
	{
		SetPaneInfo(i, arPanes[i].nID, arPanes[i].nStyle, arPanes[i].nText);
		SetPaneText(i, arPanes[i].strText);
	}
	delete pIndicators;
	SetRedraw(TRUE);
	PositionControls();
	Invalidate(TRUE);
}

/***********************************************************************
**
**   FUNCTION: AddIndicator( int position, UINT paneID )
**
**       add an pane into a statusbar
**
**   INPUT:  position: pane index
**			paneID: pane ID
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
BOOL CNCLStatusBar::AddIndicator( int position, UINT paneID )
{
	CArray<CNCLStatusBarPane,CNCLStatusBarPane> arPanes;

	CNCLStatusBarPane statusPane;
	int i;

	i = 0;
	while( GetStatusPane(i,statusPane) )
	{
		arPanes.Add( statusPane );
		i++;
	}
	if( position < 0 )
		position = 0;
	if( position > arPanes.GetSize() )
		position = arPanes.GetSize()-1;

	for(i = 0; i < arPanes.GetSize(); i ++ )
	{
		if( paneID == arPanes[i].nID )
		{
			TRACE("StatusBar::AddIndicator(): Pane ID already exists \n");
			return FALSE;
		}
	}

	CNCLStatusBarPane new_statusPane;
	new_statusPane.nID = paneID;
	if( arPanes.GetSize() )
		arPanes.InsertAt(position,new_statusPane);
	else
		arPanes.Add(new_statusPane);

	UINT * pIndicators = new UINT[arPanes.GetSize()];
	for(i=0;i<arPanes.GetSize();i++)
		pIndicators[i]=arPanes[i].nID;

	SetRedraw(FALSE);
	SetIndicators(pIndicators,arPanes.GetSize());

	for(i = 0; i < arPanes.GetSize(); i++ )
	{
		if( arPanes[i].nID != paneID )
		{
			SetPaneInfo(i,arPanes[i].nID,arPanes[i].nStyle,arPanes[i].nText);
			SetPaneText(i,arPanes[i].strText);
		}
		else
		{
			SetPaneWidth(i,50);
			SetPaneText(i,arPanes[i].strText);
		}

	}
	delete pIndicators;
	SetRedraw(TRUE);
	PositionControls();
	Invalidate(TRUE);	
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: RemovePane(int nPaneID)
**
**       Remove an pane from the statusbar
**
**   INPUT: paneID: pane ID
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLStatusBar::RemovePane(int nPaneID)
{
	int i, j;
	SetRedraw(FALSE);
	CWnd * pwnd = NULL;
	for(i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
	{
		if( m_ctrlinfo_array[i]->m_nPaneID == nPaneID )
		{
			pwnd = m_ctrlinfo_array[i]->m_hWnd;
			if (pwnd!=NULL)
			{
				pwnd->DestroyWindow();
				delete pwnd;
				m_ctrlinfo_array[i]->m_hWnd = NULL;
			}
			for (j=0; j<m_ctrlinfo_array[i]->m_subwin_no; j++)
			{
				pwnd = m_ctrlinfo_array[i]->m_subhWnd[j];
				if (pwnd!=NULL)
				{
					pwnd->DestroyWindow();
					delete pwnd;
				}
				m_ctrlinfo_array[i]->m_subhWnd[j] = NULL;
			}
			delete m_ctrlinfo_array[i];
			m_ctrlinfo_array.RemoveAt(i);
			break;
		}
	}	
	CArray<CNCLStatusBarPane,CNCLStatusBarPane> arPanes;
	CNCLStatusBarPane statusPane;
	i = 0;
	while( GetStatusPane(i,statusPane) )
	{
		if( statusPane.nID != (UINT)nPaneID )
			arPanes.Add( statusPane );
		i++;
	}
	UINT * pIndicators = new UINT[arPanes.GetSize()];
	for(i=0;i<arPanes.GetSize();i++)
		pIndicators[i]=arPanes[i].nID;
	SetIndicators(pIndicators,arPanes.GetSize());

	for(i = 0; i < arPanes.GetSize(); i++ )
	{
		SetPaneInfo(i, arPanes[i].nID,arPanes[i].nStyle,arPanes[i].nText);
		SetPaneText(i, arPanes[i].strText);
	}
	delete pIndicators;
	SetRedraw(TRUE);
	PositionControls();
	Invalidate(TRUE);
}

/***********************************************************************
**
**   FUNCTION: RemovePaneIndx(int menu)
**
**       Remove an pane from the statusbar
**
**   INPUT: menu: menu index
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLStatusBar::RemovePaneIndx(int menu)
{
	int i, j;
	int indx = -1;
	for (i=0; i<UDM_layout.statbar_no;i++)
	{
		if (stricmp(UDM_layout.statbar_file[i], UDM_menu[menu].file)==0)
		{
			indx = i;
			break;
		}
	}
	if (indx==-1)
		return;
	SetRedraw(FALSE);
	CWnd * pwnd;
	pwnd = m_ctrlinfo_array[indx]->m_hWnd;
	if (pwnd!=NULL)
	{
		pwnd->DestroyWindow();
		delete pwnd;
		m_ctrlinfo_array[indx]->m_hWnd = NULL;
	}
	for (j=0; j<m_ctrlinfo_array[indx]->m_subwin_no; j++)
	{
		pwnd = m_ctrlinfo_array[indx]->m_subhWnd[j];
		if (pwnd!=NULL)
		{
			pwnd->DestroyWindow();
			delete pwnd;
		}
		m_ctrlinfo_array[indx]->m_subhWnd[j] = NULL;
	}
	delete m_ctrlinfo_array[indx];
	m_ctrlinfo_array.RemoveAt(indx);


	CArray<CNCLStatusBarPane,CNCLStatusBarPane> arPanes;
	CNCLStatusBarPane statusPane;
	i = 0;
	while( GetStatusPane(i,statusPane) )
	{
		if( statusPane.nID != (UINT)(INDICATOR_WIN1+indx))
			arPanes.Add( statusPane );
		i++;
	}
	UINT * pIndicators = new UINT[arPanes.GetSize()];
	for(i=0;i<arPanes.GetSize();i++)
		pIndicators[i]=arPanes[i].nID;
	SetIndicators(pIndicators,arPanes.GetSize());

	for(i = 0; i < arPanes.GetSize(); i++ )
	{
		SetPaneInfo(i, arPanes[i].nID,arPanes[i].nStyle,arPanes[i].nText);
		SetPaneText(i, arPanes[i].strText);
	}
	delete pIndicators;

	SetRedraw(TRUE);
	PositionControls();
	Invalidate(TRUE);
}
/***********************************************************************
**
**   FUNCTION: Redisppane(int indx)
**
**       ReDisplay an pane on the statusbar
**
**   INPUT: paneID: pane ID
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLStatusBar::Redisppane(int menu)
{
	int i, j;
	int indx = -1;
	for (i=0; i<UDM_layout.statbar_no;i++)
	{
		if (stricmp(UDM_layout.statbar_file[i], UDM_menu[menu].file)==0)
		{
			indx = i;
			break;
		}
	}
	SetRedraw(FALSE);
	CWnd * pwnd;
	pwnd = m_ctrlinfo_array[indx]->m_hWnd;
	if (pwnd!=NULL)
	{
		pwnd->DestroyWindow();
		delete pwnd;
		m_ctrlinfo_array[indx]->m_hWnd = NULL;
	}
	for (j=0; j<m_ctrlinfo_array[indx]->m_subwin_no; j++)
	{
		pwnd = m_ctrlinfo_array[indx]->m_subhWnd[j];
		if (pwnd!=NULL)
		{
			pwnd->DestroyWindow();
			delete pwnd;
		}
		m_ctrlinfo_array[indx]->m_subhWnd[j] = NULL;
	}
	m_ctrlinfo_array[indx]->m_subwin_no = 0;
//	delete m_ctrlinfo_array[indx];
//	m_ctrlinfo_array.RemoveAt(indx);
/*
	CArray<CNCLStatusBarPane,CNCLStatusBarPane> arPanes;
	CNCLStatusBarPane statusPane;
	i = 0;
	while( GetStatusPane(i,statusPane) )
	{
		if( statusPane.nID != (UINT)(INDICATOR_WIN1+indx))
			arPanes.Add( statusPane );
		i++;
	}
	UINT * pIndicators = new UINT[arPanes.GetSize()];
	for(i=0;i<arPanes.GetSize();i++)
		pIndicators[i]=arPanes[i].nID;
	SetIndicators(pIndicators,arPanes.GetSize());

	for(i = 0; i < arPanes.GetSize(); i++ )
	{
		SetPaneInfo(i, arPanes[i].nID,arPanes[i].nStyle,arPanes[i].nText);
		SetPaneText(i, arPanes[i].strText);
	}
	delete pIndicators;
*/
	SetRedraw(TRUE);
	PositionControls();
	Invalidate(TRUE);

	ReLoadppane(indx, menu);
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
void CNCLStatusBar::OnMouseMove(UINT nFlags, CPoint point) 
{	
	CStatusBar::OnMouseMove(nFlags, point);
	OnMouseMove2(nFlags, point);
/*

	char menudata[100];
	if ((m_TimerID > 0)&&(m_barnum>=0))
	{
		//	check if we really moved enough
		int iX = m_StartPoint.x - point.x;
		int iY = m_StartPoint.y - point.y;
		if((iX*iX + iY*iY) > 100)
		{
			m_StartPoint.x = -100;
			m_StartPoint.y = -100;
			if(m_TimerID)
			{
				KillTimer(m_TimerID);
				m_TimerID = 0;
				UW_current_menubar = NULL;
			}
			if (m_hititem==-1)
				return;
			COleDataSource*	pSource = new COleDataSource();
			if(pSource)
			{
				CSharedFile	sf(GMEM_MOVEABLE|GMEM_DDESHARE|GMEM_ZEROINIT);
				CString iText;
				sprintf_s(menudata, 100, "CToolmenu %d, %d", m_barnum, m_hititem);
				iText = menudata;

				//	write name to clipboard
				sf.Write(iText, iText.GetLength());

				HGLOBAL hMem = sf.Detach();
				if (!hMem) 
					return;
				pSource->CacheGlobalData(CF_TEXT, hMem);

				//	Do drag and drop!
				pSource->DoDragDrop();

				//	free source
				delete pSource;
			}
		}
	}
*/
}
	
void CNCLStatusBar::OnMouseMove2(UINT nFlags, CPoint point)
{
	CPoint wpt;
/*
.....check if the mouse is inside the button
*/
	NCLStatusBarPaneControlInfo * pPanInfo;
	for(int i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
	{
		CRect rect;
		CRect rect2;
		pPanInfo = GetPanControl(m_ctrlinfo_array[i]->m_nPaneID);
		if (pPanInfo==NULL)
			continue;
	
		pPanInfo->m_hWnd->GetWindowRect(rect);
		ScreenToClient(&rect);
		if(rect.PtInRect(point))
		{
			if((pPanInfo->m_hWnd)->IsKindOf(RUNTIME_CLASS(CNCLStatButton)))
			{
				if (((CNCLStatButton*)pPanInfo->m_hWnd)->m_focus==0)
				{
					((CNCLStatButton*)pPanInfo->m_hWnd)->DrawHighLight2();
					((CNCLStatButton*)pPanInfo->m_hWnd)->m_focus = 1;
					wpt = point;
					ClientToScreen(&wpt);
					((CNCLStatButton*)pPanInfo->m_hWnd)->ShowToolTip(wpt);
				}
			}
		}
		else
		{
			if((pPanInfo->m_hWnd)->IsKindOf(RUNTIME_CLASS(CNCLStatButton)))
			{
				if (((CNCLStatButton*)pPanInfo->m_hWnd)->m_focus==1)
				{
					((CNCLStatButton*)pPanInfo->m_hWnd)->DrawNormal2();
					((CNCLStatButton*)pPanInfo->m_hWnd)->m_focus = 0;
					((CNCLStatButton*)pPanInfo->m_hWnd)->CloseToolTip();
				}
			}
		}
		for (int j=0; j<pPanInfo->m_subwin_no; j++)
		{
			if (pPanInfo->m_subhWnd[j]==NULL)
				continue;
/*
.....display the sub item
*/
			pPanInfo->m_subhWnd[j]->GetWindowRect(rect);
			ScreenToClient(&rect);
			if (rect.PtInRect(point))
			{
				if((pPanInfo->m_subhWnd[j])->IsKindOf(RUNTIME_CLASS(CNCLStatButton)))
				{
					if (((CNCLStatButton*)pPanInfo->m_subhWnd[j])->m_focus==0)
					{
						((CNCLStatButton*)pPanInfo->m_subhWnd[j])->DrawHighLight2();
						((CNCLStatButton*)pPanInfo->m_subhWnd[j])->m_focus = 1;
						wpt = point;
						ClientToScreen(&wpt);
						((CNCLStatButton*)pPanInfo->m_subhWnd[j])->ShowToolTip(wpt);
					}
				}
			}
			else
			{
				if((pPanInfo->m_subhWnd[j])->IsKindOf(RUNTIME_CLASS(CNCLStatButton)))
				{
					if (((CNCLStatButton*)pPanInfo->m_subhWnd[j])->m_focus==1)
					{
						((CNCLStatButton*)pPanInfo->m_subhWnd[j])->DrawNormal2();
						((CNCLStatButton*)pPanInfo->m_subhWnd[j])->m_focus = 0;
						((CNCLStatButton*)pPanInfo->m_subhWnd[j])->CloseToolTip();
					}
				}
			}
		}
	}
}

void CNCLStatusBar::OnItemMouseMove(UINT nFlags, CPoint point)
{
	ScreenToClient(&point);
	CRect rect;
	GetClientRect(&rect);
	if ((point.x<0)||(point.y<0)||(point.x>rect.right)||(point.y>rect.bottom))
	{
		m_hititem[0] = m_hititem[1] = -1;
		if (m_focus==0)
			return;
/*
.....still need call to erase tooltip and high light
*/
		OnMouseMove2(nFlags, point);
		m_focus = 0;
		return;
	}
	OnMouseMove2(nFlags, point);
	m_focus = 1;
}

void CNCLStatusBar::CheckPtOnItem(CPoint point)
{
	CPoint wpt;
/*
.....check if the mouse is inside the button
*/
	NCLStatusBarPaneControlInfo * pPanInfo;
	for(int i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
	{
		CRect rect;
		CRect rect2;
		int index = CommandToIndex( m_ctrlinfo_array[i]->m_nPaneID );
		
		GetItemRect(index,rect2);
		if(rect2.PtInRect(point))
			m_hititem[0] = i;

		pPanInfo = GetPanControl(m_ctrlinfo_array[i]->m_nPaneID);
		if (pPanInfo==NULL)
			continue;
	
		pPanInfo->m_hWnd->GetWindowRect(rect);
		ScreenToClient(&rect);
		if(rect.PtInRect(point))
		{
			if((pPanInfo->m_hWnd)->IsKindOf(RUNTIME_CLASS(CNCLStatButton)))
			{
				m_hititem[0] = i;
				if (pPanInfo->m_just==0)
					m_hititem[1] = 0;
				else
					m_hititem[1] = pPanInfo->m_subwin_no;		
				return;
			}
		}
		for (int j=0; j<pPanInfo->m_subwin_no; j++)
		{
			if (pPanInfo->m_subhWnd[j]==NULL)
				continue;
/*
.....display the sub item
*/
			pPanInfo->m_subhWnd[j]->GetWindowRect(rect);
			ScreenToClient(&rect);
			if (rect.PtInRect(point))
			{
				if((pPanInfo->m_subhWnd[j])->IsKindOf(RUNTIME_CLASS(CNCLStatButton)))
				{
					m_hititem[0] = i;
					if (pPanInfo->m_just==0)
						m_hititem[1] = j+1;
					else
						m_hititem[1] = pPanInfo->m_subwin_no - (j+1);		
					return;
				}
			}
		}
	}
}

/***********************************************************************
**
**   FUNCTION: loadstatbut(char *filename, UINT paneID)
**
**       load the menu/stat file to update buttons of a pane
**
**   INPUT:  filename: menu/stat file to contains button information
**				paneID: pane ID
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLStatusBar::loadstatbut(char *filename, UINT paneID)
{
	int i,j,m,pos[3], size[3], item, start, x,y;
	UX_pathname fullname, fname, dir, bitmap_file;
	char *p;

	int stat;
	if (m_txtfont.m_hObject)
		VERIFY (m_txtfont.DeleteObject ());	
	stat = m_txtfont.CreatePointFont (UW_statbar_size*10, UW_statbar_font);
	if (stat==0)
		m_txtfont.CreatePointFont (UW_statbar_size*10, "COURIER");

	CSize szText;
	CClientDC dc(this);
	CFont* pOldFont = NULL;
	pOldFont = dc.SelectObject(&m_txtfont);

	pos[0] = -1;
	pos[1] = -1;
	size[0] = -1;
	size[1] = -1;
	item = UDM_menu_count;
	udm_read_menu(filename, pos, size, 0, 2, -1);
	UDM_menu[item].statflag = 2;
/*
.....after this read, this menu file because a status menu file
*/
	strcpy(filename, UDM_menu[item].file);
	CRect rect;
	int current_menu, bnum, sub;
	UINT fid;
	
	if (UDM_menu[item].justify==0)
	{
		x = 2;
		for (i=0; i<UDM_menu[item].num;i++)
		{
			m_statbut[m_butno] = new CNCLStatButton();
			((CNCLStatButton*)m_statbut[m_butno])->SetParent(this);
			rect.left = x;
			if (UDM_menu[item].menus[i].separator==0)
			{
				if (UDM_menu[item].menus[i].statname[0]!=0)
				{
					fid = -1;
					stat = uz_ntstatname_to_num(UDM_menu[item].menus[i].statname, 
									UDM_menu[item].menus[i].file, 
									UDM_menu[item].menus[i].descrip, &bnum, &sub);
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
							strcpy(fullname, UDM_menu[m].file);
							ul_break_fname(fullname,dir,fname);
							if (strcmp(fname, UDM_menu[item].menus[i].file) == 0)
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
							if (udm_read_menu(UDM_menu[item].menus[i].file,
									UDM_menu[item].menus[i].pos,
									UDM_menu[item].menus[i].size, 
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
					((CNCLStatButton*)m_statbut[m_butno])->SetType(1);
					m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
						CRect(0,0,0,0), this, fid);
					((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
/*
.....use length of menu size.x define in /SIZE/cx, cy
*/
					if (UDM_menu[item].size[0]>0)
						x = x + UDM_menu[item].size[0];
					else
					{
/*
......use entire section length
*/
						x = -2000;
					}
					((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
					((CNCLStatButton*)m_statbut[m_butno])->SetStatText(UDM_menu[item].menus[i].name);
				}
				else
				{
					stat = uz_ntcnvtfunc_to_num(UDM_menu[item].menus[i].file, 
								UDM_menu[item].menus[i].params, 
								UDM_menu[item].menus[i].descrip, &bnum);
					fid = bnum + WM_APP + UDM_MAX_MENU + UZ_MAX_KEYITEM;
					if (stat==-1)
					{
/*
.....See if this menu is already loaded
*/
						for (m=0;m<UDM_menu_count;m++)
						{
							strcpy(fullname, UDM_menu[m].file);
							ul_break_fname(fullname,dir,fname);
							if (strcmp(fname, UDM_menu[item].menus[i].file) == 0)
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
							if (udm_read_menu(UDM_menu[item].menus[i].file,
											UDM_menu[item].menus[i].pos,
											UDM_menu[item].menus[i].size, 
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
					if (UDM_menu[item].menus[i].bmpfile[0]!='\0')
					{
						((CNCLStatButton*)m_statbut[m_butno])->SetType(0);
						strcpy(bitmap_file, UDM_menu[item].menus[i].bmpfile);
						p = (char *)strchr(bitmap_file,'.');
						if (p != 0 && p != bitmap_file)
						{
							*p = '\0';
							strcat_s(bitmap_file, "_16.bmp");
						}
						else
							strcat_s(bitmap_file, "_16.bmp");
						((CNCLStatButton*)m_statbut[m_butno])->SetBitmapFile(bitmap_file);
						((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
						m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
							CRect(0,0,0,0), this, fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
						if (((CNCLStatButton*)m_statbut[m_butno])->GetPicture()==NULL)
						{
							if (UDM_menu[item].menus[i].name[0]!='\0')
							{
								szText = dc.GetTextExtent(UDM_menu[item].menus[i].name);
								x = x + szText.cx + 4;
								((CNCLStatButton*)m_statbut[m_butno])->SetType(2);
							}
							else
								x = x+18;
						}
						else
							x = x+18;
					}
					else
					{
						((CNCLStatButton*)m_statbut[m_butno])->SetType(2);
						m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
							CRect(0,0,0,0), this, fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
						if (UDM_menu[item].menus[i].name[0]!='\0')
						{
							szText = dc.GetTextExtent(UDM_menu[item].menus[i].name);
							if (UDM_menu[item].size[0]>0)					
								x = x + szText.cx + 4;
							else
								x = -2000;
						}
						else
							x = x + 2;
					}
					rect.right = x;
				}
				rect.right = x;
				rect.top = 0;
				rect.bottom = 18;
			}
			else
			{
				((CNCLStatButton*)m_statbut[m_butno])->SetType(3);
				m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
						CRect(0,0,0,0), this, -1);
				x = x + 6;
				rect.right = x;
				rect.top = 0;
				rect.bottom = 18;
			}
			if (i==0)
			{
				AddControl(m_statbut[m_butno], paneID, rect, UDM_menu[item].file, UDM_menu[item].justify);
			}
			else
				AddSubControl(m_statbut[m_butno], paneID, rect);
			m_butno++;
/*
.....only allow one button
*/
			if (rect.right<=-1000)
			{
				if (i==0)
					break;
				else
				{
/*
......if button cx=0, only one button allow
*/
					MessageBox("Only one button allowed if the cx=0", "Error", MB_OK);
					return;
				}
			}
		}
	}
/*
.....right
*/
	if (UDM_menu[item].justify)
	{
		int idx = CommandToIndex(paneID);
		UINT nID,nStyle;
		int width;
		GetPaneInfo(idx,nID,nStyle,width);
		x = -2;
		for (i=UDM_menu[item].num-1; i>=0;i--)
		{
			m_statbut[m_butno] = new CNCLStatButton();
			((CNCLStatButton*)m_statbut[m_butno])->SetParent(this);
			rect.right = x;
			if (UDM_menu[item].menus[i].separator==0)
			{
				if (UDM_menu[item].menus[i].statname[0]!=0)
				{
					stat = uz_ntstatname_to_num(UDM_menu[item].menus[i].statname, 
									UDM_menu[item].menus[i].file, 
									UDM_menu[item].menus[i].descrip, &bnum, &sub);
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
							strcpy(fullname, UDM_menu[m].file);
							ul_break_fname(fullname,dir,fname);
							if (strcmp(fname, UDM_menu[item].menus[i].file) == 0)
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
							if (udm_read_menu(UDM_menu[item].menus[i].file,
									UDM_menu[item].menus[i].pos,
									UDM_menu[item].menus[i].size, 
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

					((CNCLStatButton*)m_statbut[m_butno])->SetType(1);
					m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
						CRect(0,0,0,0), this, fid);
					((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
/*
.....use length of menu size.x define in /SIZE/cx, cy
*/
					if (UDM_menu[item].size[0]>0)
						x = x - UDM_menu[item].size[0];
					else
						x = -2000;
					((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
					((CNCLStatButton*)m_statbut[m_butno])->SetStatText(UDM_menu[item].menus[i].name);
				}
				else
				{
					stat = uz_ntcnvtfunc_to_num(UDM_menu[item].menus[i].file, 
								UDM_menu[item].menus[i].params, 
								UDM_menu[item].menus[i].descrip, &bnum);
					fid = bnum + WM_APP + UDM_MAX_MENU + UZ_MAX_KEYITEM;
					if (stat==-1)
					{
/*
.....See if this menu is already loaded
*/
						for (m=0;m<UDM_menu_count;m++)
						{
							strcpy(fullname, UDM_menu[m].file);
							ul_break_fname(fullname,dir,fname);
							if (strcmp(fname, UDM_menu[item].menus[i].file) == 0)
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
							if (udm_read_menu(UDM_menu[item].menus[i].file,
											UDM_menu[item].menus[i].pos,
											UDM_menu[item].menus[i].size, 
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
					if (UDM_menu[item].menus[i].bmpfile[0]!='\0')
					{
						((CNCLStatButton*)m_statbut[m_butno])->SetType(0);
						strcpy(bitmap_file, UDM_menu[item].menus[i].bmpfile);
						p = (char *)strchr(bitmap_file,'.');
						if (p != 0 && p != bitmap_file)
						{
							*p = '\0';
							strcat_s(bitmap_file, "_16.bmp");
						}
						else
							strcat_s(bitmap_file, "_16.bmp");
						((CNCLStatButton*)m_statbut[m_butno])->SetBitmapFile(bitmap_file);
						((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
						m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
							CRect(0,0,0,0), this, fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
						if (((CNCLStatButton*)m_statbut[m_butno])->GetPicture()==NULL)
						{
							if (UDM_menu[item].menus[i].name[0]!='\0')
							{
								szText = dc.GetTextExtent(UDM_menu[item].menus[i].name);
								x = x - szText.cx - 4;
								((CNCLStatButton*)m_statbut[m_butno])->SetType(2);
							}
							else
								x = x-18;
						}
						else
							x = x-18;
					}
					else
					{
						((CNCLStatButton*)m_statbut[m_butno])->SetType(2);
						m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
							CRect(0,0,0,0), this, fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
						if (UDM_menu[item].menus[i].name[0]!='\0')
						{
							szText = dc.GetTextExtent(UDM_menu[item].menus[i].name);
							if (UDM_menu[item].size[0]>0)					
								x = x - szText.cx - 4;
							else
								x = -2000;
						}
						else
							x = x - 2;
					}
					rect.left = x;
				}
				rect.left = x;
				rect.top = 0;
				rect.bottom = 18;
			}
			else
			{
				((CNCLStatButton*)m_statbut[m_butno])->SetType(3);
				m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
						CRect(0,0,0,0), this, -1);
				x = x - 6;
				rect.left = x;
				rect.top = 0;
				rect.bottom = 18;
			}
			if (i==UDM_menu[item].num-1)
				AddControl(m_statbut[m_butno], paneID, rect, UDM_menu[item].file, UDM_menu[item].justify);
			else
				AddSubControl(m_statbut[m_butno], paneID, rect);
			m_butno++;
/*
.....only allow one button
*/
			if (rect.left<=-1000)
			{
				if (i==UDM_menu[item].num-1)
					break;
				else
				{
/*
......if button cx=0, only one button allow
*/
					MessageBox("Only one button allowed if the cx=0", "Error", MB_OK);
					return;
				}
			}
		}
	}
}

/***********************************************************************
**
**   FUNCTION: UpdateLabel(UINT id, char *label)
**
**       update the label of the button with ID=id
**
**   INPUT:  id: button ID
**				label: new label
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
int CNCLStatusBar::UpdateLabel(UINT id, char *label)
{
	NCLStatusBarPaneControlInfo * pPanInfo;
	CString newlabel;
	int status = -1;
	for(int i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
	{
		CRect rect;
		CRect rect2;
		pPanInfo = GetPanControl(m_ctrlinfo_array[i]->m_nPaneID);
		if (pPanInfo==NULL)
			continue;
	
		UINT fid = ((CNCLStatButton*)pPanInfo->m_hWnd)->GetButID();
		if (id==fid)
		{
			newlabel = ((CNCLStatButton*)pPanInfo->m_hWnd)->GetStatText() + label;
			((CNCLStatButton*)pPanInfo->m_hWnd)->UpdateLabel(newlabel);
			status = 0;
		}
		for (int j=0; j<pPanInfo->m_subwin_no; j++)
		{
			if (pPanInfo->m_subhWnd[j]==NULL)
				continue;
/*
.....display the sub item
*/
			fid = ((CNCLStatButton*)pPanInfo->m_subhWnd[j])->GetButID();
			if (id==fid)
			{
				newlabel = ((CNCLStatButton*)pPanInfo->m_subhWnd[j])->GetStatText() + label;
				((CNCLStatButton*)pPanInfo->m_subhWnd[j])->UpdateLabel(newlabel);
				status = 0;
			}
		}
	}
	return status;
}

/***********************************************************************
**
**   FUNCTION: SaveChange
**
**       Save the current button info into current menu file. NOT testing
**		yet since we have not doing chnage button function yet. Will DO after
**		drag&drop
**
**   INPUT: none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
int CNCLStatusBar::SaveChange()
{
	int i,j, indx;
	int status = 0;
	char msg[500];
	NCLStatusBarPaneControlInfo * pPanInfo;
	for(i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
	{
		CRect rect;
		CRect rect2;
		pPanInfo = GetPanControl(m_ctrlinfo_array[i]->m_nPaneID);
		if (pPanInfo==NULL)
			continue;
		if (pPanInfo->m_changed==0)
			continue;
						
		sprintf(msg, "%s\r\n %s %s\r\n%s\r\n\r\n %s\r\n%s\r\n%s",
					"You have changed Status Bar Pane",
					"The menu/stat file:", pPanInfo->m_filename,
					"have to be overwritten to reflect this change.",
					"Click ‘Yes’ to overwrite the menu file,",
					"‘No’ to save the layout without the this pane changes,",
					"Or ‘Cancel’ to not save the layout changes");
		int ans = uw_ntyesnocancel(NULL, msg, "Save Statusbar Changes");
		if (ans==-1)
/*
.....Canceled, jump out of this save routine
*/
		{
			return (UU_FALSE);
		}
		else if (ans==0)
		{
/*
......if "No", we not only don't save the status pane, but will
......contiune with other pane
*/
			continue;
		}
/*
......open pPanInfo->m_filename and save the pane info into this file
*/
/*
.....check which menu indx this pane used
*/
		indx = -1;
		for (i=0; i<UDM_menu_count; i++)
		{
			if (UDM_menu[i].statflag!=2)
				continue;
			if (stricmp(UDM_menu[i].file, pPanInfo->m_filename)==0)
			{
				indx = i;
				break;
			}
		}
		if (indx==-1)
		{
/*
.....not possible, something wrong
*/
			MessageBox("Internal error, CNCLStatusBar::SaveChange()", "Error", MB_OK);
			return 0;
		}
/*
......save the currect pan button info into UDM_menu[indx], then save the menu
*/
/*
......the pane info already save into UDM_menu[indx] when the pane changed
......so no need save this, just save the UDM_menu[indx] into the menu files
*/
/*
......save this pane button information
*/
/*		CNCLStatButton* btn;
		if (pPanInfo->m_just==0)
		{
			btn = (CNCLStatButton*)pPanInfo->m_hWnd;
//			btn->SaveBtnToMenu(UDM_menu[indx].menus[
		}
		else
		{
			btn = (CNCLStatButton*)pPanInfo->m_hWnd;
		}
*/
		uw_ntsave_menu(UDM_MTYPE_ICON, UDM_menu[indx]);
		pPanInfo->m_changed = 0;
	}
	m_changed = 0;
	return 1;
}

/***********************************************************************
**
**   FUNCTION: UpdateStatusBar(CPoint pt, char* input_text)
**
**       updated the statusbar while get the dragging object
**
**   INPUT: pt: the position the dragging object display
**			input_text: text include the button info we need add
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatusBar::UpdateStatusBar(CPoint pt, char* input_text)
{
	int barnum1, itemnum1, choice, barnum2, itemnum2;
	char menudata[100];

	barnum1 = itemnum1 = -1;
	int menu_but = 0;
	int menu_desgn = 0;
	strcpy_s(menudata, sizeof(menudata), input_text);
	if (strncmp(menudata, "CToolmenu2", 10)==0)
	{
		sscanf_s (menudata, "CToolmenu2 %d, %d", &barnum1, &itemnum1);
		menu_but = 1;
	}
	else if (strncmp(menudata, "CToolmenu", 9)==0)
	{
		sscanf_s (menudata, "CToolmenu %d, %d", &barnum1, &itemnum1);
		menu_but = 1;
	}
	else if (strncmp(menudata, "CNCLMenu", 8)==0)
	{
		sscanf_s (menudata, "CNCLMenu %d, %d", &barnum1, &itemnum1);
		menu_but = 1;
	}
	else if (strncmp(menudata, "CNCLStatusBar2", 14)==0)
	{
		sscanf_s (menudata, "CNCLStatusBar2 %d, %d", &barnum1, &itemnum1);
		menu_but = 0;
	}
	else if (strncmp(menudata, "CNCLStatusBar", 13)==0)
	{
		sscanf_s (menudata, "CNCLStatusBar %d, %d", &barnum1, &itemnum1);
		menu_but = 0;
	}
	else if (strncmp(menudata, "CNCLStatButton2", 15)==0)
	{
		sscanf_s (menudata, "CNCLStatButton2 %d, %d", &barnum1, &itemnum1);
		menu_but = 0;
	}
	else if (strncmp(menudata, "CNCLStatButton", 14)==0)
	{
		sscanf_s (menudata, "CNCLStatButton %d, %d", &barnum1, &itemnum1);
		menu_but = 0;
	}
	else if (strcmp(menudata, "MenuDesign")==0)
	{
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
.....current status number m_barnum, item num
*/
	CPoint point = pt;
	CheckClosestBut2(point, barnum2, itemnum2);
	if (barnum2==-1)
		return;
	if (menu_desgn)
	{
		NCL_MainFrame->m_menu_desgn = menu_desgn;
		int menunum = get_menunum2(barnum2, 1);
		ud_insert_UDMmenu(menunum, itemnum2, &((NCL_MainFrame->m_menudsgn_dlg)->m_menu_item));
		UW_addmenu = menunum;
		UW_additem = itemnum2;
		UW_remove_menu = -1;
		NCL_MainFrame->PostMessage(WM_COMMAND, UW_UPDATE_STATUSBAR);
		return;
	}	
	if ((menu_but==0)&&(barnum2==barnum1))
		MovedStatusItem(barnum2, itemnum2, barnum1, itemnum1, 1, 1);
	else if ((menu_but==0)&&(barnum2==barnum1)&&(itemnum2==itemnum1))
		return;
	else
	{
//......open a POPUP menu with "Copy Here" (choice=2), "Move Here", "Cancel" (choice=0)
		uw_ntpopchoice(0, &choice);
		if ((choice==0)||(choice==-1))
			return;
		if (menu_but==0)
			MovedStatusItem(barnum2, itemnum2, barnum1, itemnum1, 1, choice);
		else
		{
			MovedStatusItem(barnum2, itemnum2, barnum1, itemnum1, 0, choice);
		}
	}
}

/***********************************************************************
**
**   FUNCTION: OnLButtonDown(UINT nFlags, CPoint pt)
**
**       Left mouse button down callback
**		This function is same as CToolbar::OnLButtonDown
**		we overwrite just for call our own 
**		m_pDockContext->StartDrag function
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatusBar::OnLButtonDown(UINT nFlags, CPoint pt)
{
	CheckPtOnItem(pt);
	m_StartPoint = 	pt;
	m_TimerID = SetTimer(1, 200, NULL);
	CStatusBar::OnLButtonDown(nFlags, pt);
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
void CNCLStatusBar::OnLButtonUp(UINT nFlags, CPoint point) 
{
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;	
	if(m_TimerID)
	{
		KillTimer(m_TimerID);
		m_TimerID = 0;
	}
	CStatusBar::OnLButtonUp(nFlags, point) ;
}
/***********************************************************************
**
**   FUNCTION: MovedStatusItem(int itemnum2, int menunum1, int itemnum1, int choice)
**
**       move an item from one status pane to another barnum1,barnum2 is the menu index number
**
**   INPUT: barnum1, itemnum1: status item to move from (it could be CNCLStatButton or CToolmenu)
**						barnum1 is always menu index number
**			barnum2, itemnum2: status item point to (it is a CNCLStatButton button)
**						barnum2 is always a pane number
**			choice: 1: move, 2: copy
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLStatusBar::MovedStatusItem(int barnum2, int itemnum2, int barnum1, int itemnum1, int statflag, int choice)
{
	int menu;
	NCLStatusBarPaneControlInfo * pPanInfo, *fromPanInfo;
	if (statflag)
	{
		for(int i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
		{
			fromPanInfo = GetPanControl(m_ctrlinfo_array[i]->m_nPaneID);
			if (fromPanInfo==NULL)
				continue;
			if (stricmp(fromPanInfo->m_filename, UDM_menu[barnum1].file)==0)
			{
				fromPanInfo->m_changed = 1;
				break;
			}
		}
	}
/*
.....this menunum1, menunum2 could be a drag_menu from POP MENU (temp created 
.....when click the draging bar of the NCL POP MENU), so we need find the real
.....POP MENU number
*/
	if (statflag==0)
	{
		menu = get_menunum2 (barnum1, statflag);
		if (menu!=-1)
			barnum1 = menu;
	}
/*
......this is status pane
*/
	pPanInfo = GetPanInfo(barnum2);
	if (pPanInfo==NULL)
		return;
	pPanInfo->m_changed = 1;
	menu = get_menunum2 (barnum2, 1);
	if (menu!=-1)
		barnum2 = menu;
/*
......updated UDM_menu[m_barnum] with added one item in menu = menunum1, item = itemnum1
*/
	ud_upt_UDMmenu(barnum2, itemnum2, barnum1, itemnum1, choice);
/*
.....send message to Frame level to updated the menu display
*/
/*	NCL_MainFrame->m_addmenu = barnum2;
	NCL_MainFrame->m_additem = itemnum2;
	if ((choice==1)&&(barnum2!=barnum1))
	{
		NCL_MainFrame->m_remove_menu = barnum1;
		NCL_MainFrame->m_remove_item = itemnum1;
	}		
	else
		NCL_MainFrame->m_remove_menu = -1;
*/
	UW_addmenu = barnum2;
	UW_additem = itemnum2;
	if ((choice==1)&&(barnum2!=barnum1))
	{
		UW_remove_menu = barnum1;
		UW_remove_item = itemnum1;
	}		
	else
		UW_remove_menu = -1;
	NCL_MainFrame->PostMessage(WM_COMMAND, UW_UPDATE_STATUSBAR);
}
NCLStatusBarPaneControlInfo *CNCLStatusBar::GetPanInfo(int barno)
{
	NCLStatusBarPaneControlInfo * pPanInfo;
	pPanInfo = GetPanControl(m_ctrlinfo_array[barno]->m_nPaneID);
	return GetPanControl(pPanInfo->m_nPaneID);
}

/***********************************************************************
**
**   FUNCTION: ReLoadppane(int indx, int menu)
**
**       reload buttons of a pane use a new menu structure
**
**   INPUT:  indx: pane indx
**				menu: menu indx of menu structure
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLStatusBar::ReLoadppane(int indx, int menu)
{
	int i,j,m,pos[3], size[3], item, start, x,y;
	UX_pathname fullname, fname, dir, bitmap_file, filename;
	char *p;

	int stat;
	CSize szText;
	CClientDC dc(this);
	CFont* pOldFont = NULL;
	pOldFont = dc.SelectObject(&m_txtfont);

	pos[0] = -1;
	pos[1] = -1;
	size[0] = -1;
	size[1] = -1;
	item = menu;
	CRect rect;
	int current_menu, bnum, sub;
	UINT fid;
	
	if (UDM_menu[item].justify==0)
	{
		x = 2;
		for (i=0; i<UDM_menu[item].num;i++)
		{
			m_statbut[m_butno] = new CNCLStatButton();
			((CNCLStatButton*)m_statbut[m_butno])->SetParent(this);
			rect.left = x;
			if (UDM_menu[item].menus[i].separator==0)
			{
				if (UDM_menu[item].menus[i].statname[0]!=0)
				{
					fid = -1;
					stat = uz_ntstatname_to_num(UDM_menu[item].menus[i].statname, 
									UDM_menu[item].menus[i].file, 
									UDM_menu[item].menus[i].descrip, &bnum, &sub);
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
							strcpy(fullname, UDM_menu[m].file);
							ul_break_fname(fullname,dir,fname);
							if (strcmp(fname, UDM_menu[item].menus[i].file) == 0)
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
							if (udm_read_menu(UDM_menu[item].menus[i].file,
									UDM_menu[item].menus[i].pos,
									UDM_menu[item].menus[i].size, 
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
					((CNCLStatButton*)m_statbut[m_butno])->SetType(1);
					m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
						CRect(0,0,0,0), this, fid);
					((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
/*
.....use length of menu size.x define in /SIZE/cx, cy
*/
					if (UDM_menu[item].size[0]>0)
						x = x + UDM_menu[item].size[0];
					else
					{
/*
......use entire section length
*/
						x = -2000;
					}
					((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
					((CNCLStatButton*)m_statbut[m_butno])->SetStatText(UDM_menu[item].menus[i].name);
				}
				else
				{
					stat = uz_ntcnvtfunc_to_num(UDM_menu[item].menus[i].file, 
								UDM_menu[item].menus[i].params, 
								UDM_menu[item].menus[i].descrip, &bnum);
					fid = bnum + WM_APP + UDM_MAX_MENU + UZ_MAX_KEYITEM;
					if (stat==-1)
					{
/*
.....See if this menu is already loaded
*/
						for (m=0;m<UDM_menu_count;m++)
						{
							strcpy(fullname, UDM_menu[m].file);
							ul_break_fname(fullname,dir,fname);
							if (strcmp(fname, UDM_menu[item].menus[i].file) == 0)
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
							if (udm_read_menu(UDM_menu[item].menus[i].file,
											UDM_menu[item].menus[i].pos,
											UDM_menu[item].menus[i].size, 
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
					if (UDM_menu[item].menus[i].bmpfile[0]!='\0')
					{
						((CNCLStatButton*)m_statbut[m_butno])->SetType(0);
						strcpy(bitmap_file, UDM_menu[item].menus[i].bmpfile);
						p = (char *)strchr(bitmap_file,'.');
						if (p != 0 && p != bitmap_file)
						{
							*p = '\0';
							strcat_s(bitmap_file, "_16.bmp");
						}
						else
							strcat_s(bitmap_file, "_16.bmp");
						((CNCLStatButton*)m_statbut[m_butno])->SetBitmapFile(bitmap_file);
						((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
						m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
							CRect(0,0,0,0), this, fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
						if (((CNCLStatButton*)m_statbut[m_butno])->GetPicture()==NULL)
						{
							if (UDM_menu[item].menus[i].name[0]!='\0')
							{
								szText = dc.GetTextExtent(UDM_menu[item].menus[i].name);
								x = x + szText.cx + 4;
								((CNCLStatButton*)m_statbut[m_butno])->SetType(2);
							}
							else
								x = x+18;
						}
						else
							x = x+18;
					}
					else
					{
						((CNCLStatButton*)m_statbut[m_butno])->SetType(2);
						m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
							CRect(0,0,0,0), this, fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
						if (UDM_menu[item].menus[i].name[0]!='\0')
						{
							szText = dc.GetTextExtent(UDM_menu[item].menus[i].name);
							if (UDM_menu[item].size[0]>0)					
								x = x + szText.cx + 4;
							else
								x = -2000;
						}
						else
							x = x + 2;
					}
					rect.right = x;
				}
				rect.right = x;
				rect.top = 0;
				rect.bottom = 18;
			}
			else
			{
				((CNCLStatButton*)m_statbut[m_butno])->SetType(3);
				m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
						CRect(0,0,0,0), this, -1);
				x = x + 6;
				rect.right = x;
				rect.top = 0;
				rect.bottom = 18;
			}
			if (i==0)
			{
				InsertControl(m_statbut[m_butno], indx, rect, UDM_menu[item].file, UDM_menu[item].justify);
			}
			else
				InsertSubControl(m_statbut[m_butno], indx, rect);
			m_butno++;
/*
.....only allow one button
*/
			if (rect.right<=-1000)
			{
				if (i==0)
					break;
				else
				{
/*
......if button cx=0, only one button allow
*/
					MessageBox("Only one button allowed if the cx=0", "Error", MB_OK);
					return;
				}
			}
		}
	}
/*
.....right
*/
	if (UDM_menu[item].justify)
	{
//		UINT nID,nStyle;
//		int width;
//		GetPaneInfo(indx,nID,nStyle,width);
		x = -2;
		for (i=UDM_menu[item].num-1; i>=0;i--)
		{
			m_statbut[m_butno] = new CNCLStatButton();
			((CNCLStatButton*)m_statbut[m_butno])->SetParent(this);
			rect.right = x;
			if (UDM_menu[item].menus[i].separator==0)
			{
				if (UDM_menu[item].menus[i].statname[0]!=0)
				{
					stat = uz_ntstatname_to_num(UDM_menu[item].menus[i].statname, 
									UDM_menu[item].menus[i].file, 
									UDM_menu[item].menus[i].descrip, &bnum, &sub);
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
							strcpy(fullname, UDM_menu[m].file);
							ul_break_fname(fullname,dir,fname);
							if (strcmp(fname, UDM_menu[item].menus[i].file) == 0)
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
							if (udm_read_menu(UDM_menu[item].menus[i].file,
									UDM_menu[item].menus[i].pos,
									UDM_menu[item].menus[i].size, 
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

					((CNCLStatButton*)m_statbut[m_butno])->SetType(1);
					m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
						CRect(0,0,0,0), this, fid);
					((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
/*
.....use length of menu size.x define in /SIZE/cx, cy
*/
					if (UDM_menu[item].size[0]>0)
						x = x - UDM_menu[item].size[0];
					else
						x = -2000;
					((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
					((CNCLStatButton*)m_statbut[m_butno])->SetStatText(UDM_menu[item].menus[i].name);
				}
				else
				{
					stat = uz_ntcnvtfunc_to_num(UDM_menu[item].menus[i].file, 
								UDM_menu[item].menus[i].params, 
								UDM_menu[item].menus[i].descrip, &bnum);
					fid = bnum + WM_APP + UDM_MAX_MENU + UZ_MAX_KEYITEM;
					if (stat==-1)
					{
/*
.....See if this menu is already loaded
*/
						for (m=0;m<UDM_menu_count;m++)
						{
							strcpy(fullname, UDM_menu[m].file);
							ul_break_fname(fullname,dir,fname);
							if (strcmp(fname, UDM_menu[item].menus[i].file) == 0)
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
							if (udm_read_menu(UDM_menu[item].menus[i].file,
											UDM_menu[item].menus[i].pos,
											UDM_menu[item].menus[i].size, 
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
					if (UDM_menu[item].menus[i].bmpfile[0]!='\0')
					{
						((CNCLStatButton*)m_statbut[m_butno])->SetType(0);
						strcpy(bitmap_file, UDM_menu[item].menus[i].bmpfile);
						p = (char *)strchr(bitmap_file,'.');
						if (p != 0 && p != bitmap_file)
						{
							*p = '\0';
							strcat_s(bitmap_file, "_16.bmp");
						}
						else
							strcat_s(bitmap_file, "_16.bmp");
						((CNCLStatButton*)m_statbut[m_butno])->SetBitmapFile(bitmap_file);
						((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
						m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
							CRect(0,0,0,0), this, fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
						if (((CNCLStatButton*)m_statbut[m_butno])->GetPicture()==NULL)
						{
							if (UDM_menu[item].menus[i].name[0]!='\0')
							{
								szText = dc.GetTextExtent(UDM_menu[item].menus[i].name);
								x = x - szText.cx - 4;
								((CNCLStatButton*)m_statbut[m_butno])->SetType(2);
							}
							else
								x = x-18;
						}
						else
							x = x-18;
					}
					else
					{
						((CNCLStatButton*)m_statbut[m_butno])->SetType(2);
						m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
							CRect(0,0,0,0), this, fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetButID(fid);
						((CNCLStatButton*)m_statbut[m_butno])->SetText(UDM_menu[item].menus[i].name);
						if (UDM_menu[item].menus[i].name[0]!='\0')
						{
							szText = dc.GetTextExtent(UDM_menu[item].menus[i].name);
							if (UDM_menu[item].size[0]>0)					
								x = x - szText.cx - 4;
							else
								x = -2000;
						}
						else
							x = x - 2;
					}
					rect.left = x;
				}
				rect.left = x;
				rect.top = 0;
				rect.bottom = 18;
			}
			else
			{
				((CNCLStatButton*)m_statbut[m_butno])->SetType(3);
				m_statbut[m_butno]->Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW, 
						CRect(0,0,0,0), this, -1);
				x = x - 6;
				rect.left = x;
				rect.top = 0;
				rect.bottom = 18;
			}
			if (i==UDM_menu[item].num-1)
				InsertControl(m_statbut[m_butno], indx, rect, UDM_menu[item].file, UDM_menu[item].justify);
			else
				InsertSubControl(m_statbut[m_butno], indx, rect);
			m_butno++;
/*
.....only allow one button
*/
			if (rect.left<=-1000)
			{
				if (i==UDM_menu[item].num-1)
					break;
				else
				{
/*
......if button cx=0, only one button allow
*/
					MessageBox("Only one button allowed if the cx=0", "Error", MB_OK);
					return;
				}
			}
		}
	}
	NCLStatusBarPaneControlInfo * pPanInfo = GetPanControl(INDICATOR_WIN1+indx);
	pPanInfo->m_changed = 1;
}


/***********************************************************************
**
**   FUNCTION: InsertSubControl(CWnd * pWnd, int paneID, CRect &rect)
**
**       Added an window as the sub-control of pane window on the statusbar
**
**   INPUT:  pWnd: window to be add
**			paneID: PaneID of Pane window
**			rect: window's size
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
BOOL CNCLStatusBar::InsertSubControl(CWnd * pWnd, int indx, CRect &rect)
{
	NCLStatusBarPaneControlInfo * pPanInfo = GetPanControl(INDICATOR_WIN1+indx);
	if( pPanInfo )
	{
		if (pPanInfo->m_subwin_no>=100)
			return FALSE;
		pPanInfo->m_subhWnd[pPanInfo->m_subwin_no] = pWnd;
		pPanInfo->m_subrect[pPanInfo->m_subwin_no] = rect;
		pPanInfo->m_subwin_no++;
		PositionControls();
		Invalidate(TRUE);
		return TRUE;
	}
}


/***********************************************************************
**
**   FUNCTION: InsertControl(CWnd * pWnd, int paneID, CRect &rect, char *filen, short just, BOOL bAutodeleteControl)
**
**       Added an window as the sub-control of pane window on the statusbar
**
**   INPUT:  pWnd: window to be add
**			paneID: PaneID of Pane window
**			rect: window's size
**			filen: related menu file.
**			just: justify
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
BOOL CNCLStatusBar::InsertControl(CWnd * pWnd, int indx, CRect &rect, char *filen, short just, BOOL bAutodeleteControl)
{
	NCLStatusBarPaneControlInfo * pPanInfo = GetPanControl(INDICATOR_WIN1+indx);
	if( pPanInfo == NULL)
	{
		pPanInfo = new NCLStatusBarPaneControlInfo;
		m_ctrlinfo_array.InsertAt(indx, pPanInfo);
	}
	pPanInfo->m_just = just;
	pPanInfo->m_nPaneID =  INDICATOR_WIN1+indx;
	pPanInfo->m_rect = rect;
	pPanInfo->m_hWnd    =  pWnd;
	pPanInfo->m_bAutodelete = bAutodeleteControl;
	pPanInfo->m_filename = filen;	

	PositionControls();
	Invalidate(TRUE);
	return TRUE;
}

void CNCLStatusBar::CheckClosestBut(CWnd *wnd, CPoint pt, int &barnum, int &itemnum)
{
	int pos_flag;
	int i,j,k;
	CRect rect;
	NCLStatusBarPaneControlInfo * pPanInfo;
	barnum = -1;
	itemnum = 0;
	pos_flag = 0;
	ScreenToClient(&pt);
	for(i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
	{
		CRect rect, rect1;
		CRect rect2;
		pPanInfo = GetPanControl(m_ctrlinfo_array[i]->m_nPaneID);
		if (pPanInfo==NULL)
			continue;
		if (pPanInfo->m_just==0)
		{
			for (k=0; k<pPanInfo->m_subwin_no+1; k++)
			{
				if (k==0)
				{
					((CNCLStatButton*)pPanInfo->m_hWnd)->GetWindowRect(&rect);
				}
				else
				{
					((CNCLStatButton*)pPanInfo->m_subhWnd[k-1])->GetWindowRect(&rect);
				}
				ScreenToClient(&rect);
				++rect.bottom;
				++rect.right;
				if (rect.PtInRect(pt))
				{
					barnum = i;
					itemnum = k;
/*
......we need check out if the pt is at the right-bottom position of the button
*/
					rect1.top = rect.top + (long)(rect.Height()/2.0);
					rect1.bottom = rect.bottom;
					rect1.left = rect.left + (long)(rect.Width()/2.0);
					rect1.right = rect.right;
					if (rect1.PtInRect(pt))
						pos_flag = 1;
					break;
				}
			}
			if (barnum!=-1)
			{
				if (pos_flag)
					itemnum++;
				break;
			}
		}
		if (pPanInfo->m_just)
		{
			for (k=pPanInfo->m_subwin_no; k>=0; k--)
			{
				if (k==pPanInfo->m_subwin_no)
					((CNCLStatButton*)pPanInfo->m_hWnd)->GetWindowRect(&rect);
				else
				{
					((CNCLStatButton*)pPanInfo->m_subhWnd[pPanInfo->m_subwin_no-k-1])->GetWindowRect(&rect);
				}
				ScreenToClient(&rect);
				++rect.bottom;
				++rect.right;
				if (rect.PtInRect(pt))
				{
					barnum = i;
					itemnum = k;
/*
......we need check out if the pt is at the right-bottom position of the button
*/
					rect1.top = rect.top + (long)(rect.Height()/2.0);
					rect1.bottom = rect.bottom;
					rect1.left = rect.left + (long)(rect.Width()/2.0);
					rect1.right = rect.right;
					if (rect1.PtInRect(pt))
						pos_flag = 1;
					break;
				}
			}
			if (barnum!=-1)
			{
				if (pos_flag)
					itemnum++;
				break;
			}
		}
	}
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
void CNCLStatusBar::OnRButtonUp(UINT nFlags, CPoint pt) 
{
	CStatusBar::OnRButtonUp(nFlags, pt);
	UX_pathname menufile, dir, fdir;
	int status, menu_type, format, dockable, len, choice, saved_menu_type, changed;
	char menu_area[256], title[30];

	uw_ntpopchoice(5, &choice);

	if ((choice!=1)&&(choice!=2)&&(choice!=3)&&(choice!=4))
		return;
/*
......get current menu file name
*/
	CheckPtOnItem(pt);
	int menunum = get_menunum2(m_hititem[0], 1);
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
		status = ud_frm_getmenu_name(menufile, &menu_type, menu_area, &format, &dockable, title, 1);
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
		}
/*
......redisplay the menu if anything changed
*/
		if (changed)
		{
/*
.....redisplay the menu
*/
			UW_addmenu = menunum;
			UW_additem = -1;
			UW_remove_menu = menunum;
			UW_remove_item = -1;
			NCL_MainFrame->PostMessage(WM_COMMAND, UW_UPDATE_STATUSBAR);
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
			strcat (menufile, "\\*.menu");
		}
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
			strcat (menufile, "\\*.menu");
			ud_get_filename(NULL,"Save Menu File","*.menu", menufile, &len,
				"NCL Menu File (*.menu)", 0);
			if (len<=0) return;
		}
	}
	else if (choice==4)
	{
/*
.....delete the bottom status bar
*/
		UW_remove_menu = -1000;
		NCL_MainFrame->PostMessage(WM_COMMAND, UW_UPDATE_STATUSBAR);
		return;
	}
	char *p = (char *)strrchr(menufile,'.');
	if (p == 0)
		strcat(menufile, ".menu");
	strcpy(UDM_menu[menunum].file, menufile);
	uw_ntsave_menu(UDM_MTYPE_ICON, UDM_menu[menunum]);
}

void CNCLStatusBar::CheckClosestBut2(CPoint pt, int &barnum, int &itemnum)
{
	int pos_flag;
	int i,j,k;
	NCLStatusBarPaneControlInfo * pPanInfo;
	barnum = -1;
	itemnum = -1;
	pos_flag = 0;
	ScreenToClient(&pt);
	CRect rect, rect1, prect;
	CRect rect2;
	for(i = 0; i < m_ctrlinfo_array.GetSize(); i++ )
	{
		pPanInfo = GetPanControl(m_ctrlinfo_array[i]->m_nPaneID);
		if (pPanInfo==NULL)
			continue;
		
		GetItemRect(i, prect);

		if (pPanInfo->m_just==0)
		{
			for (k=0; k<pPanInfo->m_subwin_no+1; k++)
			{
				if (k==0)
				{
					((CNCLStatButton*)pPanInfo->m_hWnd)->GetWindowRect(&rect);
				}
				else
				{
					((CNCLStatButton*)pPanInfo->m_subhWnd[k-1])->GetWindowRect(&rect);
				}
				ScreenToClient(&rect);
				rect1.left = rect.left + (long)(rect.Width()/2.0);
				rect1.right = rect.right;
				if ((pt.x<rect1.left)&&(pt.x>prect.left))
				{
					barnum = i;
					itemnum = k;
					break;
				}
				if ((pt.x>rect1.left)&&(pt.x<rect.right))
				{
					barnum = i;
					itemnum = k+1;
					break;
				}
			}
			if (barnum!=-1)
				break;
			else
			{
				if ((pt.x>prect.left)&&(pt.x<prect.right))
				{
					barnum = i;
					itemnum = k;
					break;
				}
			}
		}
		if (pPanInfo->m_just)
		{
			for (k=pPanInfo->m_subwin_no; k>=0; k--)
			{
				if (k==pPanInfo->m_subwin_no)
					((CNCLStatButton*)pPanInfo->m_hWnd)->GetWindowRect(&rect);
				else
				{
					((CNCLStatButton*)pPanInfo->m_subhWnd[pPanInfo->m_subwin_no-k-1])->GetWindowRect(&rect);
				}
				ScreenToClient(&rect);
				rect1.left = rect.left + (long)(rect.Width()/2.0);
				rect1.right = rect.right;
				if ((pt.x>rect1.left)&&(pt.x<prect.right))
				{
					barnum = i;
					itemnum = k+1;
					break;
				}
				if ((pt.x<rect1.left)&&(pt.x>rect.left))
				{
					barnum = i;
					itemnum = k;
					break;
				}
			}
			if (barnum!=-1)
				break;
			else
			{
				if ((pt.x>prect.left)&&(pt.x<prect.right))
				{
					barnum = i;
					itemnum = 0;
					break;
				}
			}
		}
	}
}
