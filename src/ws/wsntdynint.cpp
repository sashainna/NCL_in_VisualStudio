/************************************************************************
**   FILE NAME: wsntdynint.cpp
**
**	 all dynamic dialogbar (command bar, error bar )
**			and other dynamic interface functions and 
**
**	 CONTAINS: 
**		uw_ntinit_dlgtemp1()
**		uw_ntinit_dlgtemp2()
**		uw_ntcommand_area(int kdis);
**		uw_ntprompt_area(int kdis)
**		uw_nterror_area(int kdis)
**		uw_ntwindow()
**
**     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**      MODULE NAME AND RELEASE LEVEL
**       wsntdynint.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:22
**
************************************************************************
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
#include "dmotif.h"
#include "xenv1.h"
#include "wsntcfunc.h"
#include "wsntres.h"
#include "lcom.h"

extern "C" int uw_ntinit_view();
extern "C" int UD_cmap_changed;

extern int UW_reset_menu;
extern CMainFrame *NCL_MainFrame;
extern "C" int uw_ntmenu(int kinc,int kdis);
extern "C" int uw_ntcommand_areabar(int kdis);
extern "C" int uw_ntprompt_area(int kdis);
extern "C" int uw_nterror_area(int kdis);
extern "C" int uw_ntdispmsg(char *msg);
extern "C" int uw_findbar_inarea (char *areaname, int *index, int pos[5], int size[2], int * flag);
extern void uw_ntget_menurect(int bar_pos, CRect *rect, int dockable);
extern "C" int opnwin();
extern "C" int ud_get_areadockable(char *area_name, int *dockable);
extern void uw_nthide_bars(int bar_pos);
extern CWnd *NCL_Main_View;
extern HWND NCL_cmdwin;

/***********************************************************************
c
c   SUBROUTINE:  uw_ntinit_dlgtemp1(char *string, int bar_size[2], UINT id, 
				  DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20])
c
c   FUNCTION:  This function initialize a dialog template data
c				of only one label item
c
c   INPUT:  string: label string
c			bar_size:  dialog size
c			id: label id
c   OUTPUT: dlgTempl: dialog template
c			rgDlgItem: dialog item array			
c
c***********************************************************************
*/
void uw_ntinit_dlgtemp1(char *string, int bar_size[2], UINT id, 
				  DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20])
{
	int baseunitX;
	int baseunitY;
	uw_ntget_avrchr_size(80, "MS Sans Serif", &baseunitX, &baseunitY);

	dlgTempl->cx = bar_size[0];
/*
.....convert screen pixel to dialog unit
*/
	dlgTempl->cx = (dlgTempl->cx * 4) / baseunitX;
	dlgTempl->cy = bar_size[1];
	dlgTempl->cy = (dlgTempl->cy * 8) / baseunitY;

	dlgTempl->x = 0;
	dlgTempl->y = 0;
	rgDlgItem[0].Inittemp(string, 4, 
				id, 8, 3, dlgTempl->cx-10, dlgTempl->cy-6);
	dlgTempl->style = WS_CHILD | DS_SETFONT;
	dlgTempl->dwExtendedStyle = 0;
	dlgTempl->cdit = 1;
}
/***********************************************************************
c
c   SUBROUTINE:  uw_ntinit_dlgtemp3(char *string, int bar_size[2], UINT eid, 
c				  DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20])
c
c   FUNCTION:  This function initialize a dialog template data
c				of one text item (status bar)
c
c   INPUT:  
c			string: text string
c			bar_size: size of the status text window bar (in rows and cols)
c			eid: edit field id
c   OUTPUT: dlgTempl: dialog template
c			rgDlgItem: dialog item array			
c
c***********************************************************************
*/
void uw_ntinit_dlgtemp3(char *string, int bar_size[2], UINT eid, 
				  DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20])
{
	int vsc = GetSystemMetrics(SM_CXVSCROLL);
	int cyf = GetSystemMetrics(SM_CYDLGFRAME);
	int cxf = GetSystemMetrics(SM_CXDLGFRAME);
	int tx = GetSystemMetrics(SM_CYCAPTION);

	dlgTempl->cx = bar_size[0] + vsc + cxf + 2;
	dlgTempl->cy = bar_size[1] + cyf + 2;
	dlgTempl->x = 0;
	dlgTempl->y = 0;

	rgDlgItem[0].Inittemp(string, 14, 
				eid, 2, 2, bar_size[0]+vsc, bar_size[1]);	
	dlgTempl->style = WS_CHILD | DS_SETFONT;
	dlgTempl->dwExtendedStyle = 0;
	dlgTempl->cdit = 1;
}

/***********************************************************************
c
c   SUBROUTINE:  uw_ntinit_dlgtemp2(char *label, char *string, int bar_size[2], UINT pid, UINT eid, 
c				  DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20])
c
c   FUNCTION:  This function initialize a dialog template data
c				of one label item and text item (command bar)
c
c   INPUT:  label: label string
c			string: text string
c			bar_size:  dialog size
c			pid: label id
c			eid: edit field id
c   OUTPUT: dlgTempl: dialog template
c			rgDlgItem: dialog item array			
c
c***********************************************************************
*/
void uw_ntinit_dlgtemp2(char *label, char *string, int bar_size[2], UINT pid, UINT eid, 
				  DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20])
{
	int llen, clen;
	int baseunitX;
	int baseunitY;
	int pt;
	if ((UW_com_size>0) && (UW_com_size<=48))
		pt = 10 * UW_com_size;
	else
		pt = 80;
	uw_ntget_avrchr_size(pt, "MS Sans Serif", &baseunitX, &baseunitY);

	dlgTempl->cx = bar_size[0];
/*
.....convert screen pixel to dialog unit
*/
	dlgTempl->cx = (dlgTempl->cx * 4) / baseunitX;
	dlgTempl->cy = bar_size[1];
	dlgTempl->cy = (dlgTempl->cy * 8) / baseunitY;
	dlgTempl->x = 0;
	dlgTempl->y = 0;
/*
......40 is average len for 10 character
......40 = (baseunitX * 10) * (4/baseunitX)  (second part is converting to dialog unit
*/
	if (48>dlgTempl->cx)
		llen = 0;
	else
		llen = 40;

	int ly, lcy;
	if (baseunitY<dlgTempl->cy)
	{
		ly = (40/baseunitY);
		lcy = dlgTempl->cy-(80/baseunitY);
	}
	else
	{
		ly = 0;
		lcy = dlgTempl->cy;
	}
	rgDlgItem[0].Inittemp(label, 4, 
				pid, (48/baseunitX), ly, llen, lcy);
	if (llen==0)
		clen = 0;
	else
		clen = (dlgTempl->cx-(60/baseunitX)) - llen;
	if (clen<0)
		clen = 0;

	rgDlgItem[1].Inittemp(string, 3, 
				eid, (48/baseunitX)+llen, ly, clen, lcy);
	
	dlgTempl->style = WS_CHILD | DS_SETFONT;
	dlgTempl->dwExtendedStyle = 0;
	dlgTempl->cdit = 2;
}


/***********************************************************************
c
c   SUBROUTINE:  uw_ntshow_dir(int menu_dir)
c
c   FUNCTION:  This function display a dockbar area
c
c   INPUT:  menu_dir: dockbar area number
c			1: TOP area
c			2: BOTTOM area
c			3: LEFT area
c			4: RIGHT area
c   OUTPUT: none			
c
c***********************************************************************
*/
extern "C" void uw_ntshow_dir(int menu_dir)
{
	int i, j, indx, command_show;
	CRect rect;
	UX_pathname fname,cdir,filename;
 
	command_show = 0;
	for (i=0;i<UDM_barnum;i++)
	{
		if (UDM_bararea[i].dir!=menu_dir)
			continue;
		if ((UDM_bararea[i].flag==0) || (UDM_bararea[i].flag==-1))
			continue;
		if (UDM_bararea[i].indx == -1)
			continue;
		indx = UDM_bararea[i].indx;
		if (UDM_layout.menu_name[indx][0]=='\0')
			continue;
/*
......menubar
*/
		if (UDM_bararea[i].flag==1)
		{
			for (j=0;j<UDM_menu_count;j++)
			{
				strcpy_s(filename, UX_MAX_PATH_LEN, UDM_menu[j].file);
				ul_break_fname(filename,cdir,fname);
				if (fname[0]=='\0')
					continue;

				if (strcmp(fname, UDM_layout.menu_name[indx])==0)
				{
					if (UDM_menu[j].type != UDM_MTYPE_ICON)
						continue;
					if (NCL_MainFrame->NCL_menubar[j]==NULL)
						continue;
					if ((NCL_MainFrame->NCL_menubar[j])->m_created==0)
						continue;
					if ((NCL_MainFrame->NCL_menubar[j])->pos[2]!=menu_dir)
						continue;
/*
.....show this menu again
*/
					uw_ntmenu(j, 1);
					break;
				}
			} // for j
		} //menubar
/*
.....prompt bar
*/
		else if (UDM_bararea[i].flag==3)
		{
			uw_ntprompt_area(1);
		} 
/*
.....error bar
*/
		else if (UDM_bararea[i].flag==5)
		{
			uw_nterror_area(1);
		} 
/*
.....command bar
*/
		else if (UDM_bararea[i].flag==4)
		{
			if (command_show==0)
			{
				uw_ntcommand_areabar(1);
				command_show = 1;
			}
		} 
		else if (UDM_bararea[i].flag==6)
		{
			if (UDM_layout.statwin_active)
				opnwin();
		} 
	}
}


/***********************************************************************
c
c   SUBROUTINE:  uw_ntprompt_area(int kdis)
c				  
c   FUNCTION:  This function create and/or display a prompt bar
c
c   INPUT:  
c			kdis: 1: display now
c				  0: do not display only create it
c		
c   OUTPUT: None			
c   RETURN: None
c***********************************************************************
*/
extern "C" int uw_ntprompt_area(int kdis)
{
	int bar_pos[5], bar_size[2];
	CRect rect;	

	bar_pos[0] = UDM_run_layout.prompt_pos[0];
	bar_pos[1] = UDM_run_layout.prompt_pos[1];
	bar_pos[2] = UDM_run_layout.prompt_pos[2];
	bar_pos[3] = UDM_run_layout.prompt_pos[3];
	bar_pos[4] = UDM_run_layout.prompt_pos[4];

	bar_size[0] = UDM_run_layout.prompt_size[0];
	bar_size[1] = UDM_run_layout.prompt_size[1];

	if (NCL_MainFrame->m_promptBar==NULL)
	{
		NCL_MainFrame->m_promptBar = new CNCLDialogBar();
		NCL_MainFrame->m_promptBar->pos[0] = bar_pos[0];
		NCL_MainFrame->m_promptBar->pos[1] = bar_pos[1];
		NCL_MainFrame->m_promptBar->pos[2] = bar_pos[2];
		NCL_MainFrame->m_promptBar->pos[3] = bar_pos[3];
		NCL_MainFrame->m_promptBar->pos[4] = bar_pos[4];
		UINT type = WS_CHILD | WS_VISIBLE | CBRS_SIZE_DYNAMIC | CBRS_FLYBY
						| CBRS_TOOLTIPS;
		if (UDM_run_layout.prompt_type==0)
		{
			if (bar_pos[2]==1)
				type = type | CBRS_TOP ;
			else if (bar_pos[2]==2)
				type = type | CBRS_BOTTOM ;
			else if (bar_pos[2]==3)
				type = type | CBRS_LEFT ;
			else if (bar_pos[2]==4)
				type = type | CBRS_RIGHT ;
			else
				type = type | CBRS_TOP ;
		}
		else
			type = type | CBRS_TOP ;
		NCL_MainFrame->m_promptBar->m_TitleCaption = "NCL Prompt Bar";
		if (!(NCL_MainFrame->m_promptBar->CreateLabel(NCL_MainFrame, 
				" ", bar_size, type, IDC_LPROMPT, IDD_PROMPTBAR)))
		{
			MessageBox(NULL, "Failed to create NCL Prompt bar", "Warning", MB_ICONINFORMATION|MB_OK);
			return -1;      
		}
		NCL_MainFrame->m_promptBar->m_created = 1;
		(NCL_MainFrame->m_promptBar)->EnableDocking(CBRS_ALIGN_BOTTOM | CBRS_ALIGN_TOP);
		NCL_MainFrame->EnableDocking(CBRS_ALIGN_ANY);
	}
	NCL_MainFrame->m_promptBar->pos[0] = bar_pos[0];
	NCL_MainFrame->m_promptBar->pos[1] = bar_pos[1];
	NCL_MainFrame->m_promptBar->pos[2] = bar_pos[2];
	NCL_MainFrame->m_promptBar->pos[3] = bar_pos[3];
	NCL_MainFrame->m_promptBar->pos[4] = bar_pos[4];
	if (kdis)
	{
		if (UDM_run_layout.prompt_type==0)
		{
			uw_ntget_menurect(bar_pos[2], &rect, UDM_AREA_ENDDOCK);
	
			if (bar_pos[2]==1)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_promptBar, AFX_IDW_DOCKBAR_TOP, &rect);
			else if (bar_pos[2]==2)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_promptBar, AFX_IDW_DOCKBAR_BOTTOM, &rect);
			else if (bar_pos[2]==3)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_promptBar, AFX_IDW_DOCKBAR_LEFT, &rect);
			else if (bar_pos[2]==4)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_promptBar, AFX_IDW_DOCKBAR_RIGHT, &rect);
		}
		else
		{
			CPoint pt(0,0);
			pt.Offset(bar_pos[3], bar_pos[4]);			
			NCL_MainFrame->FloatControlBar(NCL_MainFrame->m_promptBar, pt);	
		}
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_promptBar,
				TRUE, FALSE);
		(NCL_MainFrame->m_promptBar)->m_visible = 1;
	}
	else
	{
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_promptBar,
				FALSE, FALSE);
		(NCL_MainFrame->m_promptBar)->m_visible = 0;
	}
	NCL_MainFrame->RecalcLayout();
	return 0;
}


/***********************************************************************
c
c   SUBROUTINE:  uw_ntcommand_area(int kdis)
c				  
c   FUNCTION:  This function create and/or display a command bar
c
c   INPUT:  
c			kdis: 1: display now
c				  0: do not display only create it
c		
c   OUTPUT: None			
c   RETURN: None
c***********************************************************************
*/
extern "C" int uw_ntcommand_area(int kdis)
{
	int bar_pos[5], bar_size[2];
	CRect rect;	

	bar_pos[0] = UDM_run_layout.command_pos[0];
	bar_pos[1] = UDM_run_layout.command_pos[1];
	bar_pos[2] = UDM_run_layout.command_pos[2];
	bar_pos[3] = UDM_run_layout.command_pos[3];
	bar_pos[4] = UDM_run_layout.command_pos[4];

	bar_size[0] = UDM_run_layout.command_size[0];
	bar_size[1] = UDM_run_layout.command_size[1];

	if (NCL_MainFrame->m_commandBar==NULL)
	{
		NCL_MainFrame->m_commandBar = new CNCLCmdBar();
		NCL_MainFrame->m_commandBar->pos[0] = bar_pos[0];
		NCL_MainFrame->m_commandBar->pos[1] = bar_pos[1];
		NCL_MainFrame->m_commandBar->pos[2] = bar_pos[2];
		NCL_MainFrame->m_commandBar->pos[3] = bar_pos[3];
		NCL_MainFrame->m_commandBar->pos[4] = bar_pos[4];
		NCL_MainFrame->m_commandBar->m_TitleCaption = "NCL Command Bar";
		NCL_MainFrame->m_commandBar->CreateCmdBar();
		(NCL_MainFrame->m_commandBar)->EnableDocking(CBRS_ALIGN_BOTTOM | CBRS_ALIGN_TOP);
		NCL_MainFrame->EnableDocking(CBRS_ALIGN_ANY);
		NCL_MainFrame->m_commandBar->m_created = 1;
	}

	NCL_MainFrame->m_commandBar->pos[0] = bar_pos[0];
	NCL_MainFrame->m_commandBar->pos[1] = bar_pos[1];
	NCL_MainFrame->m_commandBar->pos[2] = bar_pos[2];
	NCL_MainFrame->m_commandBar->pos[3] = bar_pos[3];
	NCL_MainFrame->m_commandBar->pos[4] = bar_pos[4];
	if (kdis)
	{
		if (UDM_run_layout.command_type==0)
		{
			uw_ntget_menurect(bar_pos[2], &rect, UDM_AREA_ENDDOCK);

			if (bar_pos[2]==1)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_TOP, &rect);
			else if (bar_pos[2]==2)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_BOTTOM, &rect);
			else if (bar_pos[2]==3)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_LEFT, &rect);
			else if (bar_pos[2]==4)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_RIGHT, &rect);
		}
		else
		{
			CPoint pt(0,0);
			pt.Offset(bar_pos[3], bar_pos[4]);			
			NCL_MainFrame->FloatControlBar(NCL_MainFrame->m_commandBar, pt);	
		}
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_commandBar,
				TRUE, FALSE);
		(NCL_MainFrame->m_commandBar)->m_visible = 1;
	}
	else
	{
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_commandBar,
				FALSE, FALSE);
		(NCL_MainFrame->m_commandBar)->m_visible = 0;
	}
	NCL_MainFrame->RecalcLayout();

	return 0;
}


/***********************************************************************
c
c   SUBROUTINE:  uw_nterror_area(int kdis)
c				  
c   FUNCTION:  This function create and/or display a error bar
c
c   INPUT:  
c			kdis: 1: display now
c				  0: do not display only create it
c		
c   OUTPUT: None			
c   RETURN: None
c***********************************************************************
*/
extern "C" int uw_nterror_area(int kdis)
{
	int bar_pos[5], bar_size[2];
	CRect rect;	

	bar_pos[0] = UDM_run_layout.error_pos[0];
	bar_pos[1] = UDM_run_layout.error_pos[1];
	bar_pos[2] = UDM_run_layout.error_pos[2];
	bar_pos[3] = UDM_run_layout.error_pos[3];
	bar_pos[4] = UDM_run_layout.error_pos[4];

	bar_size[0] = UDM_run_layout.error_size[0];
	bar_size[1] = UDM_run_layout.error_size[1];

	if (NCL_MainFrame->m_errorBar==NULL)
	{
		NCL_MainFrame->m_errorBar = new CNCLDialogBar();
		NCL_MainFrame->m_errorBar->pos[0] = bar_pos[0];
		NCL_MainFrame->m_errorBar->pos[1] = bar_pos[1];
		NCL_MainFrame->m_errorBar->pos[2] = bar_pos[2];
		NCL_MainFrame->m_errorBar->pos[3] = bar_pos[3];
		NCL_MainFrame->m_errorBar->pos[4] = bar_pos[4];
		UINT type = WS_CHILD | WS_VISIBLE | CBRS_SIZE_DYNAMIC | CBRS_FLYBY
						| CBRS_TOOLTIPS;
		if (UDM_run_layout.error_type==0)
		{
			if (bar_pos[2]==1)
				type = type | CBRS_TOP ;
			else if (bar_pos[2]==2)
				type = type | CBRS_BOTTOM ;
			else if (bar_pos[2]==3)
				type = type | CBRS_LEFT ;
			else if (bar_pos[2]==4)
				type = type | CBRS_RIGHT ;
			else
				type = type | CBRS_TOP ;
		}
		else
			type = type | CBRS_TOP ;
		NCL_MainFrame->m_errorBar->m_TitleCaption = "NCL Error Bar";
		if (!(NCL_MainFrame->m_errorBar->CreateLabel(NCL_MainFrame, 
				" ", bar_size, type, IDC_LERROR, IDD_ERRORBAR)))
		{
			MessageBox(NULL, "Failed to create NCL Error bar", "Warning", MB_ICONINFORMATION|MB_OK);
			return -1;      
		}
		(NCL_MainFrame->m_errorBar)->EnableDocking(CBRS_ALIGN_BOTTOM | CBRS_ALIGN_TOP);
		NCL_MainFrame->EnableDocking(CBRS_ALIGN_ANY);
		NCL_MainFrame->m_errorBar->m_created = 1;
	}
	NCL_MainFrame->m_errorBar->pos[0] = bar_pos[0];
	NCL_MainFrame->m_errorBar->pos[1] = bar_pos[1];
	NCL_MainFrame->m_errorBar->pos[2] = bar_pos[2];
	NCL_MainFrame->m_errorBar->pos[3] = bar_pos[3];
	NCL_MainFrame->m_errorBar->pos[4] = bar_pos[4];
	if (kdis)
	{
		if (UDM_run_layout.error_type==0)
		{
			uw_ntget_menurect(bar_pos[2], &rect, UDM_AREA_ENDDOCK);
	
			if (bar_pos[2]==1)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_errorBar, AFX_IDW_DOCKBAR_TOP, &rect);
			else if (bar_pos[2]==2)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_errorBar, AFX_IDW_DOCKBAR_BOTTOM, &rect);
			else if (bar_pos[2]==3)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_errorBar, AFX_IDW_DOCKBAR_LEFT, &rect);
			else if (bar_pos[2]==4)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_errorBar, AFX_IDW_DOCKBAR_RIGHT, &rect);
		}
		else
		{
			CPoint pt(0,0);
			pt.Offset(bar_pos[3], bar_pos[4]);			
			NCL_MainFrame->FloatControlBar(NCL_MainFrame->m_errorBar, pt);	
		}
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_errorBar,
				TRUE, FALSE);
		(NCL_MainFrame->m_errorBar)->m_visible = 1;
	}
	else
	{
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_errorBar,
				FALSE, FALSE);
		(NCL_MainFrame->m_errorBar)->m_visible = 0;
	}
	NCL_MainFrame->RecalcLayout();

	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  uw_ntwindow()
c				  
c   FUNCTION:  This function create all the dialog bars
c				in the window
c
c   INPUT:  
c			None
c		
c   OUTPUT: None			
c   RETURN: None
c***********************************************************************
*/
extern "C" void uw_ntwindow()
{
	UX_pathname fname, dir, filename;
/*
.....first, resize the window size
*/
	NCL_MainFrame->MoveWindow(UDM_layout.window_pos[0],UDM_layout.window_pos[1], 
			UDM_layout.window_size[0],UDM_layout.window_size[1]);		

	if (UDM_run_layout.error_type==1)
		uw_nterror_area(1);
	else
		uw_nterror_area(0);

	if (UDM_run_layout.prompt_type==1)
		uw_ntprompt_area(1);
	else
		uw_ntprompt_area(0);

	if (UDM_run_layout.command_type==1)
		uw_ntcommand_areabar(1);
	else
		uw_ntcommand_areabar(0);
/*
.....Create menus
*/
	UW_reset_menu = 1;
	for (int i=0;i<UDM_layout.nmenu;i++)
	{
		if (UDM_layout.menu_name[i][0]=='\0')
			continue;
		for (int j=0;j<UDM_menu_count;j++)
		{
			strcpy_s(filename, UX_MAX_PATH_LEN, UDM_menu[j].file);
			ul_break_fname(filename,dir,fname);

			if (strcmp(fname, UDM_layout.menu_name[i])==0)
			{
				if (UDM_menu[j].type == UDM_MTYPE_ICON)
					uw_ntmenu(j, 0);
				else
					uw_ntmenu(j, 1);
			}
		}
	}
/*
......show all the toolbar, status bar
......we show here after we call uw_ntmenu, uw_ntwindow
......to create all menu, control bars. we need arrange 
......to control bar from row 1 -> up, we can't insert a bar
......between 2 rows (for example, we have displayed row1 and row3
......without row2 displayed (then it will look like we only have 2 rows) 
......and tried to insert row2 between row1 and row3, it does not work
......so, when we first create the control bar, we hide it, then
......here, we show then row by row
*/
	uw_ntshow_dir(1);
	uw_ntshow_dir(2);
	uw_ntshow_dir(3);
	uw_ntshow_dir(4);
	UW_reset_menu = 0;
	uw_ntinit_view();
	UD_cmap_changed = 0;
}

/***********************************************************************
c
c   SUBROUTINE:  uw_getmarea_list(char area_lst[100][80], int *num_area)
c
c   FUNCTION:  This function get the menu area list
c
c   INPUT:  none
c   OUTPUT: area_lst: area list
c			num_area: area number			
c
c***********************************************************************
*/
extern "C" void uw_getmarea_list(char area_lst[100][80], int *num_area)
{
	int i, num;
	num = 0;
	for (i=0; i<UDM_run_layout.nmenu; i++)
	{
		if (UDM_run_layout.menu_area[i][0]!='\0')
		{
			strcpy_s(area_lst[num], 80,UDM_run_layout.menu_area[i]);
			num++;
		}
	}
	*num_area = num;
}

/***********************************************************************
c
c   SUBROUTINE:  uw_ntadd_menuarea(char *areaname, int dir, int rows, int cols, int width, int height)
c
c   FUNCTION:  This function add a menu area in the window layout
c
c   INPUT:  areaname: area name
c			dir: area direction
c			rows, cols: menu area in the dock frame position
c			wid, hgt: menu area width and height
c   OUTPUT: 0: not added
c			1: added			
c
c***********************************************************************
*/
extern "C" int uw_ntadd_menuarea(char *areaname, int dir, int rows, int cols, int width, int height)
{
	int i, laynum, menunum;
	int ans;
	char msg[256];

	if (areaname[0]=='\0')
	{
		uw_ntdispmsg("Empty menu area name!");
		return 0;
	}
	for (i=0; i<UDM_run_layout.nmenu; i++)
	{
		if (UDM_run_layout.menu_area[i][0]!='\0')
		{
			if (strcmp(areaname, UDM_run_layout.menu_area[i])==0)
				break;
		}
	}
	if (i<UDM_run_layout.nmenu)
	{
		sprintf_s(msg, 256, "There is menu area \"%s\" in the layout, overwrite?", areaname);
		ans = uw_ntyes_or_no(NULL, msg, "Add menu area");
		if (ans==0)
			return 0;
		laynum = i;
/*
......check if there is a menu inside this menu area
*/
		menunum = -1;
		for (i=0;i<UDM_menu_count;i++)
		{
			if (strcmp(UDM_menu[i].menu_area, areaname)==0)
				break;
		}
		if (i<UDM_menu_count)
			menunum = i;

		if (menunum==-1) 
		{
/*
.....no menu in this area, just modify this area and return
*/
			UDM_run_layout.menu_pos[laynum][0] = rows;
			UDM_run_layout.menu_pos[laynum][1] = cols;
			UDM_run_layout.menu_pos[laynum][2] = dir;
			UDM_run_layout.menu_pos[laynum][3] = -1;
			UDM_run_layout.menu_pos[laynum][4] = -1;
			UDM_run_layout.menu_size[laynum][0] = width;
			UDM_run_layout.menu_size[laynum][1] = height;
			return 1;
		}
/*
.....There is a menu overwrite this area
.....we need move the menu to a new position
*/
/*
.....hide this menu, then show at new position
*/
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[menunum],
									FALSE, FALSE);
		(NCL_MainFrame->NCL_menubar[menunum])->m_visible = -1;
		uw_ntmenu(menunum, 1);
		return 1;
	}

	int addnum = UDM_run_layout.nmenu;
	strcpy_s (UDM_run_layout.menu_area[addnum], 256, areaname);
	UDM_run_layout.menu_name[addnum][0] = '\0';
	UDM_run_layout.menu_pos[addnum][0] = rows;
	UDM_run_layout.menu_pos[addnum][1] = cols;
	UDM_run_layout.menu_pos[addnum][2] = dir;
	UDM_run_layout.menu_pos[addnum][3] = -1;
	UDM_run_layout.menu_pos[addnum][4] = -1;
	UDM_run_layout.menu_size[addnum][0] = width;
	UDM_run_layout.menu_size[addnum][1] = height;

	UDM_run_layout.nmenu++;
	return 1;	
}


/***********************************************************************
c
c   SUBROUTINE:  uw_ntdelete_menuarea(char *areaname)
c
c   FUNCTION:  This function delete a menu area from the window layout
c
c   INPUT:  areaname: area name
c   OUTPUT: 0: not deleted
c			1: deleted			
c
c***********************************************************************
*/
extern "C" int uw_ntdelete_menuarea(char *areaname)
{
	int i, j, laynum, menunum;
	int ans;
	char msg[UX_MAX_PATH_LEN+40];

	if (areaname[0]=='\0')
	{
		uw_ntdispmsg("Empty menu area name!");
		return 0;
	}
	for (i=0; i<UDM_run_layout.nmenu; i++)
	{
		if (UDM_run_layout.menu_area[i][0]!='\0')
		{
			if (strcmp(areaname, UDM_run_layout.menu_area[i])==0)
				break;
		}
	}
	if (i>=UDM_run_layout.nmenu)
	{
		sprintf_s(msg, 256, "can't find menu area \"%s\" to delete!", areaname);
		uw_ntdispmsg(msg);
		return 0;
	}
	laynum = i;
/*
......check if there is a menu inside this menu area
*/
	menunum = -1;
	for (i=0;i<UDM_menu_count;i++)
	{
		if (strcmp(UDM_menu[i].menu_area, areaname)==0)
			break;
	}
	if (i<UDM_menu_count)
		menunum = i;

	if (menunum==-1) 
	{
/*
.....no menu in this area, just delete this area
*/
		goto delete_area;
	}
/*
.....There is a menu inside this area, ask if user want
.....to remove this menu before remove this area
.....if yes, remove menu and delete menu area
.....if no, doing nothing, simple return
*/
	sprintf_s (msg, UX_MAX_PATH_LEN+40, "There is a menu %s in menu area %s\n, To remove this menu area, we must remove the menu too, continue?",
			UDM_menu[menunum].file, areaname);
	ans = uw_ntyes_or_no(NULL, msg, "Delete menu area");
	if (ans==0)
		return 0;
/*
.....delete this menu, acturally, just hide it now
*/
	NCL_MainFrame->ShowControlBar(NCL_MainFrame->NCL_menubar[menunum],
									FALSE, FALSE);
	(NCL_MainFrame->NCL_menubar[menunum])->m_visible = -1;
	
delete_area:;

	for (j=UDM_run_layout.nmenu-1; j>laynum; j--)
	{
		if (UDM_run_layout.menu_area[j][0]!='\0')
			strcpy_s (UDM_run_layout.menu_area[j-1],256,  UDM_run_layout.menu_area[j]);
		else
			UDM_run_layout.menu_area[j-1][0] = '\0';
		if (UDM_run_layout.menu_name[j][0]!='\0')
			strcpy_s (UDM_run_layout.menu_name[j-1],256, UDM_run_layout.menu_name[j]);
		else
			UDM_run_layout.menu_name[j-1][0] = '\0';
		UDM_run_layout.menu_pos[j-1][0] = UDM_run_layout.menu_pos[j][0];
		UDM_run_layout.menu_pos[j-1][1] = UDM_run_layout.menu_pos[j][1];
		UDM_run_layout.menu_pos[j-1][2] = UDM_run_layout.menu_pos[j][2];
		UDM_run_layout.menu_pos[j-1][3] = UDM_run_layout.menu_pos[j][3];
		UDM_run_layout.menu_pos[j-1][4] = UDM_run_layout.menu_pos[j][4];
		UDM_run_layout.menu_size[j-1][0] = UDM_run_layout.menu_size[j][0];
		UDM_run_layout.menu_size[j-1][1] = UDM_run_layout.menu_size[j][1];
	}
	UDM_run_layout.nmenu--;
	return 1;	
}
extern "C" int uw_ntcommand_areabar(int flag)
{
	int status,stat;
	char filename[80];
	int bar_pos[5], bar_size[2];
	CRect rect;	
	DWORD dwStyle;

	status = 0;
	if (flag==0)
	{
		if (NCL_MainFrame->m_commandBar!=NULL)
		{
			NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_commandBar,
						FALSE, FALSE);
			(NCL_MainFrame->m_commandBar)->m_visible = 0;
			NCL_MainFrame->RecalcLayout();
			(NCL_MainFrame->m_commandBar)->setcmdtext(" ", " ");
			return(status);
		}
	}
	bar_pos[0] = UDM_run_layout.command_pos[0];
	bar_pos[1] = UDM_run_layout.command_pos[1];
	bar_pos[2] = UDM_run_layout.command_pos[2];
	bar_pos[3] = UDM_run_layout.command_pos[3];
	bar_pos[4] = UDM_run_layout.command_pos[4];

	bar_size[0] = UDM_run_layout.command_size[0];
	bar_size[1] = UDM_run_layout.command_size[1];
	if (NCL_MainFrame->m_commandBar!=NULL)
/*
.....Window already created
.....Just show it
*/
	{
		if ((bar_pos[2]!=-1) 
			&& (UDM_run_layout.command_area[0]!='\0')
			&& ((UDM_run_layout.command_doc[0]!=0) 
			|| (UDM_run_layout.command_doc[1]!=0)
			|| (UDM_run_layout.command_doc[2]!=0)
			|| (UDM_run_layout.command_doc[3]!=0)))
		{
/*
.....not floating
*/
			CRect rect;
			int dockable;
			if (UW_reset_menu==1)
				dockable = UDM_AREA_ENDDOCK;
			else
				ud_get_areadockable(UDM_run_layout.command_area, &dockable);
			uw_ntget_menurect(bar_pos[2], &rect, dockable);
			if (dockable==UDM_AREA_NODOCK)
/*
......replace/hide the control bars inside this area
*/
				uw_nthide_bars(bar_pos[2]);
			if (bar_pos[2]==1)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_TOP, &rect);
			else if (bar_pos[2]==2)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_BOTTOM, &rect);
			else if (bar_pos[2]==3)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_LEFT, &rect);
			else if (bar_pos[2]==4)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_RIGHT, &rect);
		}
		else
		{
			CPoint pt(0,0);
			int tsc = GetSystemMetrics(SM_CYCAPTION);
			int cyf = GetSystemMetrics(SM_CYDLGFRAME);
			if (UDM_run_layout.command_att[0]==1)
				bar_pos[4] -= ((NCL_MainFrame->m_commandBar)->m_szFloat.cy
								+ tsc);
			if (UDM_run_layout.command_att[1] ==1)
				bar_pos[3] -= (NCL_MainFrame->m_commandBar)->m_szFloat.cx;
			pt.Offset(bar_pos[3], bar_pos[4]);			
			NCL_MainFrame->FloatControlBar(NCL_MainFrame->m_commandBar, pt);	
		}
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_commandBar,
					TRUE, FALSE);
		(NCL_MainFrame->m_commandBar)->m_visible = 1;
		NCL_MainFrame->RecalcLayout();
		return(status);
	}
	if ((UDM_run_layout.command_area[0]!='\0') && 
			((UDM_run_layout.command_doc[0]!=0) 
			|| (UDM_run_layout.command_doc[1]!=0)
			|| (UDM_run_layout.command_doc[2]!=0)
			|| (UDM_run_layout.command_doc[3]!=0)))
	{
		filename[0] = '\0';
		stat = ud_getpos_from_bararea(1, filename, UDM_run_layout.command_area, bar_pos, bar_size);
		if (stat!=-1)
		{
			if (bar_pos[0]==-1)
				bar_pos[0] = UDM_run_layout.command_pos[0];
			if (bar_pos[1]==-1)
				bar_pos[1] = UDM_run_layout.command_pos[1];
			if (bar_pos[2]==-1)
				bar_pos[2] = UDM_run_layout.command_pos[2];
			if (bar_pos[3]==-1)
				bar_pos[3] = UDM_run_layout.command_pos[3];
			if (bar_pos[4]==-1)
				bar_pos[4] = UDM_run_layout.command_pos[4];
			if (bar_size[0]==-1)
				bar_size[0] = UDM_run_layout.command_size[0];
			if (bar_size[1]==-1)
				bar_size[1] = UDM_run_layout.command_size[1];
		}
		else
/*
.....can't find menuarea, using as float
*/
		{
			bar_pos[2] = -1;
			NCL_Main_View->GetWindowRect(&rect);
			if (UDM_run_layout.command_att[0]==0)
				bar_pos[4] = rect.top + UDM_run_layout.command_pos[4];
			else if (UDM_run_layout.command_att[0] ==1)
			{
				bar_pos[4] = rect.bottom + UDM_run_layout.command_pos[4];
			}
			else
				bar_pos[4] = rect.top + UDM_run_layout.command_pos[4];
			if (UDM_run_layout.command_att[1] ==0)
				bar_pos[3] = rect.left + UDM_run_layout.command_pos[3];
			else if (UDM_run_layout.command_att[1] ==1)
			{
				bar_pos[3] = rect.right + UDM_run_layout.command_pos[3];
			}
			else
				bar_pos[3] = rect.left + UDM_run_layout.command_pos[3];
			bar_size[0] = UDM_run_layout.command_size[0];
			bar_size[1] = UDM_run_layout.command_size[1];
		}
	}
	else
	{
		bar_pos[2] = -1;
		NCL_Main_View->GetWindowRect(&rect);
		if (UDM_run_layout.command_att[0]==0)
			bar_pos[4] = rect.top + UDM_run_layout.command_pos[4];
		else if (UDM_run_layout.command_att[0] ==1)
		{
			bar_pos[4] = rect.bottom + UDM_run_layout.command_pos[4];
		}
		else
			bar_pos[4] = rect.top + UDM_run_layout.command_pos[4];
		if (UDM_run_layout.command_att[1] ==0)
			bar_pos[3] = rect.left + UDM_run_layout.command_pos[3];
		else if (UDM_run_layout.command_att[1] ==1)
		{
			bar_pos[3] = rect.right + UDM_run_layout.command_pos[3];
		}
		else
			bar_pos[3] = rect.left + UDM_run_layout.command_pos[3];
		bar_size[0] = UDM_run_layout.command_size[0];
		bar_size[1] = UDM_run_layout.command_size[1];
	}
	if (NCL_MainFrame->m_commandBar==NULL)
	{
		NCL_MainFrame->m_commandBar = new CNCLCmdBar();
		NCL_MainFrame->m_commandBar->pos[0] = bar_pos[0];
		NCL_MainFrame->m_commandBar->pos[1] = bar_pos[1];
		NCL_MainFrame->m_commandBar->pos[2] = bar_pos[2];
		NCL_MainFrame->m_commandBar->pos[3] = bar_pos[3];
		NCL_MainFrame->m_commandBar->pos[4] = bar_pos[4];
		NCL_MainFrame->m_commandBar->m_TitleCaption = "NCL Command Bar";
		NCL_MainFrame->m_commandBar->CreateCmdBar();
		if ((UDM_run_layout.command_doc[0]==0)
				&& (UDM_run_layout.command_doc[1]==0)
				&& (UDM_run_layout.command_doc[2]==0)
				&& (UDM_run_layout.command_doc[3]==0))
		{
			(NCL_MainFrame->m_commandBar)->EnableDocking(0);
			NCL_MainFrame->EnableDocking(0);
		}
		else if ((UDM_run_layout.command_doc[0]==1)
				&& (UDM_run_layout.command_doc[1]==1)
				&& (UDM_run_layout.command_doc[2]==1)
				&& (UDM_run_layout.command_doc[3]==1))
		{
			(NCL_MainFrame->m_commandBar)->EnableDocking(CBRS_ALIGN_ANY);
			NCL_MainFrame->EnableDocking(CBRS_ALIGN_ANY);
		}
		else 
		{
			dwStyle = 0;
			if (UDM_run_layout.command_doc[0]==1)
				dwStyle |= CBRS_ALIGN_TOP;
			if (UDM_run_layout.command_doc[1]==1)
				dwStyle |= CBRS_ALIGN_BOTTOM;
			if (UDM_run_layout.command_doc[2]==1)
				dwStyle |= CBRS_ALIGN_LEFT;
			if (UDM_run_layout.command_doc[3]==1)
				dwStyle |= CBRS_ALIGN_RIGHT;
			(NCL_MainFrame->m_commandBar)->EnableDocking(dwStyle);
			NCL_MainFrame->EnableDocking(dwStyle);
		}
		NCL_MainFrame->m_commandBar->m_created = 1;
	}

	NCL_MainFrame->m_commandBar->pos[0] = bar_pos[0];
	NCL_MainFrame->m_commandBar->pos[1] = bar_pos[1];
	NCL_MainFrame->m_commandBar->pos[2] = bar_pos[2];
	NCL_MainFrame->m_commandBar->pos[3] = bar_pos[3];
	NCL_MainFrame->m_commandBar->pos[4] = bar_pos[4];
	if (flag==0)
	{
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_commandBar,
				FALSE, FALSE);
		(NCL_MainFrame->m_commandBar)->m_visible = 0;
		NCL_MainFrame->RecalcLayout();
		NCL_cmdwin = 0;
		return status;
	}
	if ((bar_pos[2]!=-1) 
			&& (UDM_run_layout.command_area[0]!='\0')
			&& ((UDM_run_layout.command_doc[0]!=0) 
			|| (UDM_run_layout.command_doc[1]!=0)
			|| (UDM_run_layout.command_doc[2]!=0)
			|| (UDM_run_layout.command_doc[3]!=0)))
	{
/*
.....not floating
*/
		CRect rect;
		int dockable;
		if (UW_reset_menu==1)
			dockable = UDM_AREA_ENDDOCK;
		else
			ud_get_areadockable(UDM_run_layout.command_area, &dockable);
		uw_ntget_menurect(bar_pos[2], &rect, dockable);
		if (dockable==UDM_AREA_NODOCK)
/*
......replace/hide the control bars inside this area
*/
			uw_nthide_bars(bar_pos[2]);
		if (bar_pos[2]==1)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_TOP, &rect);
		else if (bar_pos[2]==2)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_BOTTOM, &rect);
		else if (bar_pos[2]==3)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_LEFT, &rect);
		else if (bar_pos[2]==4)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_RIGHT, &rect);
	}
	else
	{
		CPoint pt(0,0);
		int tsc = GetSystemMetrics(SM_CYCAPTION);
		int cyf = GetSystemMetrics(SM_CYDLGFRAME);
		if (UDM_run_layout.command_att[0]==1)
			bar_pos[4] -= ((NCL_MainFrame->m_commandBar)->m_szFloat.cy
							+ tsc);
		if (UDM_run_layout.command_att[1] ==1)
			bar_pos[3] -= (NCL_MainFrame->m_commandBar)->m_szFloat.cx;
		pt.Offset(bar_pos[3], bar_pos[4]);			
		NCL_MainFrame->FloatControlBar(NCL_MainFrame->m_commandBar, pt);	
	}
	NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_commandBar,
				TRUE, FALSE);
	(NCL_MainFrame->m_commandBar)->m_visible = 1;
	NCL_MainFrame->RecalcLayout();
	if (NCL_MainFrame->m_commandBar!=NULL)
	{
		NCL_cmdwin = (NCL_MainFrame->m_commandBar)->getcmdwin();
	}
	else
		NCL_cmdwin = 0;
	return(status);
}

