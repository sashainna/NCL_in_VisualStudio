#include "usysdef.h"

/********************************************************************* 
**  NAME:  wsnttrm.cpp
**
**       Native WinNT terminal emulations.
**
**  COPYRIGHT  1996  NCCS Inc. (c) All rights reserved.
**
**	 CONTAINS:
**
**		uw_open_nt_window
**		uw_close_nt_window
**		uw_nt_win_out
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttrm.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:30
*********************************************************************/
#include "wsntstdafx.h"
#include "wsgl.h"
#include "udebug.h"
#include "dasnog.h"
#include "uims.h"
#include <stdio.h>
#include "wsntframe.h"
#include "wsntres.h"
#include "dmotif.h"
#include "wsntcfunc.h"

#define MAXLIN 100
static int m_insertpos = 0;

extern "C" void uw_nt_win_out(char *buff);
extern CMainFrame *NCL_MainFrame;
extern CWnd *NCL_Main_View;
extern void uw_ntget_menurect(int bar_pos, CRect *rect, int dockable);
extern int UW_reset_menu;
extern "C" int ud_get_areadockable(char *area_name, int *dockable);
extern void uw_nthide_bars(int bar_pos);
extern "C" int UL_clswin_flag;
extern HWND NCL_statwin;
/*********************************************************************
**    E_FUNCTION     : uw_ntinsert_status(text)
**       Insert text into the status window at current position
**    PARAMETERS
**       INPUT  :
**              text: text to be inserted
**				
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntinsert_status(char *text)
{
	CEdit *edt;
	char *buf;
	CString wintxt, wintxt2;
	int len = strlen(text);

	edt = (CEdit *)NCL_MainFrame->m_statusBar->GetDlgItem(IDC_STEXT);
	edt->GetWindowText(wintxt); 
	buf = wintxt.GetBuffer(200*80);
	if (m_insertpos+len>80*200)
		wintxt2 = buf + len;
	else
		wintxt2 = buf; 
	edt->SetWindowText(wintxt2);
	edt->SetSel(m_insertpos, m_insertpos);
	edt->ReplaceSel(text);
	m_insertpos += len;
	NCL_MainFrame->m_statusBar->UpdateScrollBar();
/*
.....Update the window with the added text
.....otherwise the window may remain blank
.....for some time, for example
.....during motion playback at a slow speed
*/
	edt->UpdateWindow();
}

/**************************************************************************
**
**  E_FUNCTION:  uw_nt_win_out(buff)
**      Write a string to the scrolling window window.
**
**  PARAMETERS   
**      INPUT  :  buff	:	string to be written out
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
extern "C" void uw_nt_win_out(char *buff)
#define tab 9
#define field 7
{
	int incy,i,j,k,r,nc,nspc;
	char tmp[256], tmp2[256];
/*
.....Window is visible
*/
	if (NCL_MainFrame->m_statusBar==NULL)
		return;
	if(NCL_MainFrame->m_statusBar->m_visible == 1)
	{
/*
.....Convert TABS to spaces
*/
		j = 0;
		nc = strlen(buff);
		for (i=0;i<nc;i++)
		{
			if (buff[i] == tab)
			{
				r = j - field * (j/field);
				nspc = field - r;
				for (k=j; nspc-->0;k++)
				{
					tmp[k] = ' ';
				}
				j = j + (field-r);
			}
			else
			{
				tmp[j] = buff[i];
				j++;
			}
		}
		tmp[j] = '\0';
/*
.....Convert CR/LF to NewLine
.....Assume it can only be at the end
.....of the string
*/
		incy = 0;
		for (i=strlen(tmp)-2;i<(int)strlen(tmp);i++)
		{
			switch(tmp[i])
			{
			case '\015':
				tmp[i] = '\0';
				break;
			case '\012':
				incy = 1;
				tmp[i] = '\0';
				break;
			}
		}
		strcat(tmp,"\n");
/*
......convert "\n" to "\r\n" on WinNT
*/
		j = 0;
		nc = strlen(tmp);
		for (i=0;i<nc;i++)
		{
			if (tmp[i] == '\n')
			{
				tmp2[j++] = '\r';
				tmp2[j] = '\n';
				j++;
			}
			else
			{
				tmp2[j] = tmp[i];
				j++;
			}
		}
		tmp2[j] = '\0';
/*
.......Write the line to the window
*/
		uw_ntinsert_status(tmp2);
	}
	return;
}

/*********************************************************************
**	 E_FUNCTION:int uw_open_nt_window(line,cols)
**		This function opens a scrolling window for output.
**			The 'line,cols' is ignored because we used 'SIZE'
**			defined in layout file
**	 PARAMETERS	
**		 INPUT  : line = # of lines in window
**			  col = # of columns in window
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
extern "C" int uw_open_nt_window(int line, int cols)
{
	int status,stat;
	char filename[80];
	int bar_pos[5], bar_size[2];
	CRect rect;	
	DWORD dwStyle;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	if (NCL_MainFrame->m_statusBar!=NULL)
/*
.....Window already created
.....Just show it
*/
	{
		NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_statusBar,
					TRUE, FALSE);
		(NCL_MainFrame->m_statusBar)->m_visible = 1;
		NCL_MainFrame->RecalcLayout();
		uw_nt_win_out(" ");
		return(status);
	}
	if ((UDM_run_layout.statwin_area[0]!='\0') && 
			((UDM_run_layout.statwin_doc[0]!=0) 
			|| (UDM_run_layout.statwin_doc[1]!=0)
			|| (UDM_run_layout.statwin_doc[2]!=0)
			|| (UDM_run_layout.statwin_doc[3]!=0)))
	{
		filename[0] = '\0';
		stat = ud_getpos_from_bararea(1, filename, UDM_run_layout.statwin_area, bar_pos, bar_size);
		if (stat!=-1)
		{
			if (bar_pos[0]==-1)
				bar_pos[0] = UDM_run_layout.statwin_pos[0];
			if (bar_pos[1]==-1)
				bar_pos[1] = UDM_run_layout.statwin_pos[1];
			if (bar_pos[2]==-1)
				bar_pos[2] = UDM_run_layout.statwin_pos[2];
			if (bar_pos[3]==-1)
				bar_pos[3] = UDM_run_layout.statwin_pos[3];
			if (bar_pos[4]==-1)
				bar_pos[4] = UDM_run_layout.statwin_pos[4];
			if (bar_size[0]==-1)
				bar_size[0] = (UDM_run_layout.statwin_size[0])*4;
			if (bar_size[1]==-1)
				bar_size[1] = (UDM_run_layout.statwin_size[1])*8;
		}
		else
/*
.....can't find menuarea, using as float
*/
		{
			bar_pos[2] = -1;
			NCL_Main_View->GetWindowRect(&rect);
			if (UDM_run_layout.statwin_att[0]==0)
				bar_pos[4] = rect.top + UDM_run_layout.statwin_pos[4];
			else if (UDM_run_layout.statwin_att[0] ==1)
			{
				bar_pos[4] = rect.bottom + UDM_run_layout.statwin_pos[4];
			}
			else
				bar_pos[4] = rect.top + UDM_run_layout.statwin_pos[4];
			if (UDM_run_layout.statwin_att[1] ==0)
				bar_pos[3] = rect.left + UDM_run_layout.statwin_pos[3];
			else if (UDM_run_layout.statwin_att[1] ==1)
			{
				bar_pos[3] = rect.right + UDM_run_layout.statwin_pos[3];
			}
			else
				bar_pos[3] = rect.left + UDM_run_layout.statwin_pos[3];
/*
......4 is average len for 1 character
......4 = (baseunitX * 1) * (4/baseunitX)  (second part is converting to dialog unit)
*/
			bar_size[0] = (UDM_run_layout.statwin_size[0])*4;
/*
......8 is average height for 1 character
......8 = (baseunitY * 1 ) * (8/baseunitY)  (second part is converting to dialog unit)
*/
			bar_size[1] = (UDM_run_layout.statwin_size[1])*8;
		}
	}
	else
	{
		bar_pos[2] = -1;
		NCL_Main_View->GetWindowRect(&rect);
		if (UDM_run_layout.statwin_att[0]==0)
			bar_pos[4] = rect.top + UDM_run_layout.statwin_pos[4];
		else if (UDM_run_layout.statwin_att[0] ==1)
		{
			bar_pos[4] = rect.bottom + UDM_run_layout.statwin_pos[4];
		}
		else
			bar_pos[4] = rect.top + UDM_run_layout.statwin_pos[4];
		if (UDM_run_layout.statwin_att[1] ==0)
			bar_pos[3] = rect.left + UDM_run_layout.statwin_pos[3];
		else if (UDM_run_layout.statwin_att[1] ==1)
		{
			bar_pos[3] = rect.right + UDM_run_layout.statwin_pos[3];
		}
		else
			bar_pos[3] = rect.left + UDM_run_layout.statwin_pos[3];
/*
......4 is average len for 1 character
......4 = (baseunitX * 1) * (4/baseunitX)  (second part is converting to dialog unit)
*/
		bar_size[0] = (UDM_run_layout.statwin_size[0])*4;
/*
......8 is average height for 1 character
......8 = (baseunitY * 1 ) * (8/baseunitY)  (second part is converting to dialog unit)
*/
		bar_size[1] = (UDM_run_layout.statwin_size[1])*8;
	}
	if (NCL_MainFrame->m_statusBar==NULL)
	{
		NCL_MainFrame->m_statusBar = new CNCLDialogBar();
		NCL_MainFrame->m_statusBar->pos[0] = bar_pos[0];
		NCL_MainFrame->m_statusBar->pos[1] = bar_pos[1];
		NCL_MainFrame->m_statusBar->pos[2] = bar_pos[2];
		NCL_MainFrame->m_statusBar->pos[3] = bar_pos[3];
		NCL_MainFrame->m_statusBar->pos[4] = bar_pos[4];
		UINT type = WS_CHILD | WS_VISIBLE | CBRS_SIZE_DYNAMIC | CBRS_FLYBY
						 | CBRS_TOOLTIPS;
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
		NCL_MainFrame->m_statusBar->m_TitleCaption = "Status Window";
		if (!(NCL_MainFrame->m_statusBar->CreateStatusWin(NCL_MainFrame, 
				"", bar_size, type, IDC_STEXT, IDD_STATWINBAR)))
		{
			MessageBox(NULL, "Failed to create NCL Status bar", "Warning", MB_ICONINFORMATION|MB_OK);
			return -1;      
		}
		if ((UDM_run_layout.statwin_doc[0]==0)
				&& (UDM_run_layout.statwin_doc[1]==0)
				&& (UDM_run_layout.statwin_doc[2]==0)
				&& (UDM_run_layout.statwin_doc[3]==0))
		{
			(NCL_MainFrame->m_statusBar)->EnableDocking(0);
			NCL_MainFrame->EnableDocking(0);
		}
		else if ((UDM_run_layout.statwin_doc[0]==1)
				&& (UDM_run_layout.statwin_doc[1]==1)
				&& (UDM_run_layout.statwin_doc[2]==1)
				&& (UDM_run_layout.statwin_doc[3]==1))
		{
			(NCL_MainFrame->m_statusBar)->EnableDocking(CBRS_ALIGN_ANY);
			NCL_MainFrame->EnableDocking(CBRS_ALIGN_ANY);
		}
		else 
		{
			dwStyle = 0;
			if (UDM_run_layout.statwin_doc[0]==1)
				dwStyle |= CBRS_ALIGN_TOP;
			if (UDM_run_layout.statwin_doc[1]==1)
				dwStyle |= CBRS_ALIGN_BOTTOM;
			if (UDM_run_layout.statwin_doc[2]==1)
				dwStyle |= CBRS_ALIGN_LEFT;
			if (UDM_run_layout.statwin_doc[3]==1)
				dwStyle |= CBRS_ALIGN_RIGHT;
			(NCL_MainFrame->m_statusBar)->EnableDocking(dwStyle);
			NCL_MainFrame->EnableDocking(dwStyle);
		}
		NCL_MainFrame->m_statusBar->m_created = 1;
	}

	NCL_MainFrame->m_statusBar->pos[0] = bar_pos[0];
	NCL_MainFrame->m_statusBar->pos[1] = bar_pos[1];
	NCL_MainFrame->m_statusBar->pos[2] = bar_pos[2];
	NCL_MainFrame->m_statusBar->pos[3] = bar_pos[3];
	NCL_MainFrame->m_statusBar->pos[4] = bar_pos[4];
	if ((bar_pos[2]!=-1) 
			&& (UDM_run_layout.statwin_area[0]!='\0')
			&& ((UDM_run_layout.statwin_doc[0]!=0) 
			|| (UDM_run_layout.statwin_doc[1]!=0)
			|| (UDM_run_layout.statwin_doc[2]!=0)
			|| (UDM_run_layout.statwin_doc[3]!=0)))
	{
/*
.....not floating
*/
		CRect rect;
		int dockable;
		if (UW_reset_menu==1)
			dockable = UDM_AREA_ENDDOCK;
		else
			ud_get_areadockable(UDM_run_layout.statwin_area, &dockable);
		uw_ntget_menurect(bar_pos[2], &rect, dockable);
		if (dockable==UDM_AREA_NODOCK)
/*
......replace/hide the control bars inside this area
*/
			uw_nthide_bars(bar_pos[2]);
		if (bar_pos[2]==1)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_statusBar, AFX_IDW_DOCKBAR_TOP, &rect);
		else if (bar_pos[2]==2)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_statusBar, AFX_IDW_DOCKBAR_BOTTOM, &rect);
		else if (bar_pos[2]==3)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_statusBar, AFX_IDW_DOCKBAR_LEFT, &rect);
		else if (bar_pos[2]==4)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_statusBar, AFX_IDW_DOCKBAR_RIGHT, &rect);
	}
	else
	{
		CPoint pt(0,0);
	int tsc = GetSystemMetrics(SM_CYCAPTION);
	int cyf = GetSystemMetrics(SM_CYDLGFRAME);
		if (UDM_run_layout.statwin_att[0]==1)
			bar_pos[4] -= ((NCL_MainFrame->m_statusBar)->m_szFloat.cy
							+ tsc);
		if (UDM_run_layout.statwin_att[1] ==1)
			bar_pos[3] -= (NCL_MainFrame->m_statusBar)->m_szFloat.cx;
		pt.Offset(bar_pos[3], bar_pos[4]);			
		NCL_MainFrame->FloatControlBar(NCL_MainFrame->m_statusBar, pt);	
	}
	NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_statusBar,
				TRUE, FALSE);
	(NCL_MainFrame->m_statusBar)->m_visible = 1;
	NCL_MainFrame->RecalcLayout();
	if (NCL_MainFrame->m_statusBar!=NULL)
	{
		CWnd *cwin = NCL_MainFrame->m_statusBar->GetDlgItem(IDC_STEXT);
		NCL_statwin = cwin->m_hWnd;
	}
	return(status);
}

/**************************************************************************
**
**  E_FUNCTION:  uw_close_nt_window
**      Close the text scrolling window.
**
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  1: if closed
**					0: if not close
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
extern "C" int uw_close_nt_window()
{
	if (NCL_MainFrame->m_statusBar==NULL)
		return 1;
/*
.....Window is visible
.....take it down
*/
	if(NCL_MainFrame->m_statusBar->m_visible == 1)
	{
		if ((UDM_run_layout.statwin_type) || 
			((UL_clswin_flag) && (UDM_run_layout.statwin_type==0)))
		{
			NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_statusBar,
					FALSE, FALSE);
			NCL_MainFrame->m_statusBar->m_visible = 0;
			NCL_MainFrame->RecalcLayout();
			UL_clswin_flag = 0;
			return 1;
		}
	}
	else
	{
		UL_clswin_flag = 0;
		return 1;
	}
	return 0;
}

