#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**
**   FILE NAME: wsntcmdbar.cpp
** 
**	 Description - Functions and implementations for
**		CNCLCmdBar class and some form functions
**
**	 CONTAINS:
**		uw_cmd_extend()
**		class functions of CNCLCmdBar class
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntcmdbar.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:20
**********************************************************************
*/
#include "wsntstdafx.h"
#include <math.h>
#include "lcom.h"
#include "dmotif.h"
#include "wsntctl.h"
#include "wsntdoc.h"
#include "wsntview.h"
#include "wsntframe.h"
#include "wsntcmdbar.h"
#include "wsntres.h"
#include "lipv.h"

#define UD_FORMFUZZ (UU_REAL) 1.0e-6
#define TEXT_WID	4

extern CMainFrame *NCL_MainFrame;
extern CWnd *NCL_Main_View;

#define stricmp _stricmp
extern int NCL_KeyReturn;
extern "C" int UD_textpre_curpos;
extern "C" void uw_ntget_avrchr_size(int pt, char *fntname, int *wid, int *hgt);
extern CMainFrame *NCL_MainFrame;
extern "C" int NT_FuncEvent, NT_FuncEof;
extern "C" int NCL_cmdmod;
extern "C" void getnln(int *);
extern "C" int nclf_getsrc(int *, char *, int *, short*, short*);
extern "C" void setnln(int *);
extern "C" void uw_ntwrplabel(char*);
extern "C" void uw_ntsav_plabel(char*);
extern "C" void uw_ntsav_pprm(char*);
extern "C" void uw_ntwrprm(char*); 
extern HWND NCL_cmdwin;
extern int UW_reset_menu;
extern "C" int ud_get_areadockable(char *area_name, int *dockable);
extern void uw_nthide_bars(int bar_pos);
extern void uw_ntget_menurect(int bar_pos, CRect *rect, int dockable);
extern "C" int insmode_val();
extern "C" void nclc_delsrc(int, int);
extern int UW_insert_line;
static int delta_cy, delta_cx;
/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
IMPLEMENT_DYNAMIC(CNCLCmdBar,CNCLDialogBar)

BEGIN_MESSAGE_MAP(CNCLCmdBar, CNCLDialogBar)
	//{{AFX_MSG_MAP(CNCLCmdBar)
	ON_MESSAGE(WM_INITDIALOG, HandleInitDialog)
	ON_WM_PAINT()
/*
......try to make resize working, but can't make it works, if only stay on single line
......seems OK to resize, but after switched edit box, always get a MFC error
......give up for now. The size will always use sigle char line for sigle line edit box
......and use the height value of the multi fields, but make unsiziable for now, same as formbar
*/
	ON_WM_SIZE()
	ON_WM_MOUSEMOVE()
	ON_COMMAND(IDC_ECOMMAND, OnCommandLine)
	ON_EN_CHANGE (IDC_ECOMMAND, OnCommandLine)
	ON_COMMAND(IDC_CMD_ARROW, FormUserCallbacks1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CNCLCmdBar::HandleCommandExe(WPARAM wParam, LPARAM lParam)
{
	m_edit.HandleCommandExe(wParam, lParam);
	return;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntcmd_showmore()
**       Redisplays the form fields.  Usually called when enabling or
**			disabling fields based on a toggle response.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int CNCLCmdBar::uw_ntcmd_showmore()
{
	return 0;
}
void CNCLCmdBar::reshow_edit()
{	
	if (NCL_cmdmod)
	{
		if (m_extend)
		{
			m_edit.ShowWindow(SW_SHOW);
			m_edit.SetFont(&m_txtfont);
			m_edit_single.ShowWindow(SW_HIDE);
			NCL_cmdwin = m_edit.m_hWnd;
			m_type = 1;
			m_edit.Init();
			m_edit.SetShowLine();
			m_arrow.SetType(1);
		}
		else
			m_arrow.SetType(0);
	}
	else
	{
		if (m_extend)
		{
			m_edit.ShowWindow(SW_HIDE);
			m_edit_single.ShowWindow(SW_SHOW);
			m_edit_single.SetFont(&m_txtfont);
			NCL_cmdwin = m_edit_single.m_hWnd;
		}
		else
		{
			m_edit.ShowWindow(SW_HIDE);
			m_edit_single.ShowWindow(SW_HIDE);
			GetDlgItem(IDC_ECOMMAND)->ShowWindow(SW_SHOW);
			GetDlgItem(IDC_ECOMMAND)->SetFont(&m_txtfont);
			NCL_cmdwin = GetDlgItem(IDC_ECOMMAND)->m_hWnd;
		}
		m_type = 0;
		m_arrow.SetType(0);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  HandleInitDialog
c
c   FUNCTION:  This function initialize every fields in
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
LRESULT CNCLCmdBar::HandleInitDialog(WPARAM wparm, LPARAM lparm)
{
	m_init = 0;
	CNCLDialogBar::HandleInitDialog(wparm, lparm);

	CWnd *txtwin = GetDlgItem(IDC_ECOMMAND);
	DWORD dwStyle_single = WS_CHILD | WS_VISIBLE | ES_AUTOHSCROLL | ES_LEFT | WS_BORDER | WS_TABSTOP | DS_SETFONT;
	DWORD dwStyle = WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | ES_AUTOVSCROLL | ES_LEFT | WS_BORDER  |ES_WANTRETURN | DS_SETFONT | ES_MULTILINE;
			
	m_type = 0;
	if (NCL_cmdmod)
	{
		m_type = 1;
	}
	RECT rect, rect_text;
	txtwin->GetWindowRect(&rect_text);
	GetWindowRect(&rect);
	delta_cy = (rect.bottom - rect.top) - (rect_text.bottom - rect_text.top);
	ScreenToClient(&rect_text);
	rect = rect_text;

	if (m_txtfont.m_hObject)
		VERIFY (m_txtfont.DeleteObject ());	
	int stat = m_txtfont.CreatePointFont (UW_com_size*10, UW_com_font);
	if (stat==0)
		m_txtfont.CreatePointFont (UW_com_size*10, "MS Sans Serif");

	SetFont(&m_txtfont);

	CClientDC dc(this);
	CFont* savfont = dc.SelectObject(&m_txtfont);

	double wid, hgt;
	CSize sizeText = dc.GetTextExtent("ABCDEFGHIJKLMNOPQRSTUVWXYZ",26);
	POINT fsize;
	fsize.x = (long)sizeText.cx;
	fsize.y = sizeText.cy;
	dc.LPtoDP(&fsize);
	wid = (double)((double)fsize.x)/26.0;
	hgt = (double)fsize.y;
			
	int cxf = GetSystemMetrics(SM_CXDLGFRAME);

	hgt = UDM_run_layout.command_size[2]*hgt + 2*cxf;
	rect.bottom = (int)(rect.top + hgt);
	m_edit.Create(dwStyle, rect, this, IDC_ECOMMAND2);
	m_edit.SetFont(&m_txtfont);
	
	m_edit_single.Create(dwStyle_single, rect, this, IDC_ECOMMAND3);
	m_edit_single.SetFont(&m_txtfont);

	m_enabled = 0;

	CWnd *butwin = GetDlgItem(IDC_CMD_ARROW);
	butwin->GetWindowRect(&rect);
	ScreenToClient(&rect);
	butwin->ShowWindow(SW_HIDE);
	dwStyle = WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW;
	m_arrow.Create("", dwStyle, rect, this, IDC_CMD_ARROW);
	if (LW_nclipv==LW_STANDALONE)
		m_arrow.ShowWindow(SW_HIDE);
	m_line_no = 0;

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
	m_init = 1;

	if (m_extend)
	{
		txtwin->ShowWindow(SW_HIDE);
		if (m_type==1)
		{
			m_edit.ShowWindow(SW_SHOW);
			m_edit.SetFont(&m_txtfont);
			m_edit_single.ShowWindow(SW_HIDE);
			m_arrow.SetType(1);
		}
		else
		{
			m_edit.ShowWindow(SW_HIDE);
			m_edit_single.ShowWindow(SW_SHOW);
			m_edit_single.SetFont(&m_txtfont);
			m_arrow.SetType(0);
		}
		m_sizeDefault.cy = delta_cy + (m_child_rect[3].bottom-m_child_rect[3].top);
		m_szHorz =  m_sizeDefault;
		m_szVert = m_sizeDefault;
		m_szFloat = m_sizeDefault;
	}
	else
	{
		txtwin->ShowWindow(SW_SHOW);
		m_edit.ShowWindow(SW_HIDE);
		m_edit_single.ShowWindow(SW_HIDE);

		m_sizeDefault.cy = delta_cy + (m_child_rect[1].bottom-m_child_rect[1].top);
		NCL_cmdwin = txtwin->m_hWnd;
		m_szHorz =  m_sizeDefault;
		m_szVert = m_sizeDefault;
		m_szFloat = m_sizeDefault;
		m_arrow.SetType(0);
	}
	m_edit.m_txtfont = &m_txtfont;
	int max = m_edit.GetLimitText();
	m_edit.SetLimitText(50000000);
	max = m_edit.GetLimitText();

	CRect crect;
	GetWindowRect(&crect);
	crect.bottom = crect.top + m_sizeDefault.cy;
	MoveWindow(crect);
	GetClientRect(m_origrect);
	enable_win(0);
	return TRUE;
}	
/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks1
**       command callback routine for arrow button.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLCmdBar::FormUserCallbacks1()
{
	short itype, icmt;
	char sbuf[1024], savstr[1024], prompt[256];
	RECT rect1, rect2;
	char text[1024];
	int nc, iline, ioerr, m_total_line;
	CString whole_text;
	CWnd *txtwin = GetDlgItem(IDC_ECOMMAND);
	txtwin->GetWindowRect(&rect1);
	m_edit.GetWindowRect(&rect2);

	if (LW_nclipv==LW_STANDALONE)
		return;
	if (m_extend)
		m_extend = 0;
	else
		m_extend = 1;
	int sel = 0;
	if (m_extend)
	{
/*
......save the current line (it maybe changed, or new
......replace the old line or add this new line
*/
		txtwin->GetWindowText(savstr, 1023);
		txtwin->ShowWindow(SW_HIDE);
/*
......set the reload flag and let the editor know
*/
		m_edit.m_load_done = 0;
		m_edit.ShowWindow(SW_SHOW);
		m_edit.SetFont(&m_txtfont);
		m_extend = 1;
		m_sizeDefault.cy = delta_cy + (rect2.bottom-rect2.top);
		reshow_edit();
		if (NCL_cmdmod==0)
		{
/*
.....prompt mode
*/
			txtwin->GetWindowText(whole_text);
			m_edit_single.SetWindowText(whole_text);
			goto done;
		}
		getnln(&m_line_no);
/*
.....
/* get current source line */
		icmt = 0;
		iline = 1;
		ioerr = 0;
		whole_text = "";
/*
......load mxinum 1000+ line first (+-300, 600lns)
*/
		int start_line, end_line;
		for (iline=0; (ioerr==0&&(iline<1000000)); iline++)
		{
			ioerr = nclf_getsrc(&iline, sbuf, &nc, &itype, &icmt);
			if (ioerr!=0)
				break;
		}
		m_total_line = iline;
		m_edit.m_total_line = iline;
		if (m_line_no<=UDM_layout.command_load[0])
		{
			start_line = 0;
			end_line = m_line_no + UDM_layout.command_load[0];
			m_edit.m_done_upload = 1;
			m_edit.m_done_dnload = 0;
		}
		else
		{
			start_line = m_line_no - UDM_layout.command_load[0]/2;
			end_line = m_line_no + UDM_layout.command_load[0] - UDM_layout.command_load[0]/2;
			m_edit.m_done_upload = 0;
			m_edit.m_done_dnload = 0;
		}
		icmt = 0;
		ioerr = 0;
		if (end_line>m_total_line)
			end_line = m_total_line;
		for (iline=start_line; (ioerr==0&&(iline<m_total_line)&&(iline<end_line)); iline++)
		{
			ioerr = nclf_getsrc(&iline, sbuf, &nc, &itype, &icmt);
			if (ioerr==0)
			{
				sbuf[nc] = '\0';
				if ((iline+1==m_line_no)&&(insmode_val()!=2))
					strcpy(sbuf, savstr);
				if ((iline!=m_total_line-1)&&(iline==end_line-1))
					whole_text = whole_text + sbuf;
				else
					whole_text = whole_text + sbuf + "\r\n";
			}
			else
				break;
		}
		if (iline+1==m_line_no)
			whole_text = whole_text + savstr;
		m_edit.SetWindowText(whole_text);
		SetScrollBarPos(m_total_line, m_line_no);
		UpdateScrollBar();
		
		m_edit.m_start_line = m_edit.m_end_line = m_line_no-1;
		if (insmode_val()!=2)
		{
			m_edit.m_ncl_insert_ln = m_ncl_insert_ln = -1;
			sel = m_edit.LineIndex(m_line_no-start_line-1);
			m_edit.m_start_char = m_edit.m_end_char = sel;
		}
		else
		{
			m_ncl_insert_ln = m_line_no-1;
			sel = m_edit.LineIndex(m_ncl_insert_ln-start_line-1);
			m_edit.m_start_char = m_edit.m_end_char = sel;
			if (!((m_edit.m_ins_save!=-1)&&(m_edit.m_ncl_insert_ln==m_ncl_insert_ln)))
			{
				sel = m_edit.LineIndex(m_ncl_insert_ln-start_line);
				m_edit.m_start_char = m_edit.m_end_char = sel;
				m_edit.InsertLine();
				m_edit.SetSel(sel, sel, TRUE);
				m_edit.ReplaceSel(savstr);
			}
			m_edit.m_ncl_insert_ln = m_ncl_insert_ln;
			m_edit.m_act_line = m_ncl_insert_ln;
		}
		m_edit.SetSelection(sel, sel, FALSE);
		NCL_cmdwin = m_edit.m_hWnd;
		sprintf_s(prompt, 256, "edit line: %d", m_line_no);
		uw_ntsav_plabel(prompt);
		uw_ntwrplabel(prompt);
		uw_ntsav_pprm("Edit Commands");
		uw_ntwrprm("Edit Commands");
		m_edit.m_whole_text = whole_text;
		if (iline<end_line)
		{
			m_edit.m_done_dnload = 1;
		}
		m_edit.m_load_start = start_line;
		m_edit.m_act_line = m_line_no-1;
		m_edit.m_load_done = 1;
	}
	else
	{   
/*
.....if nothing loaded, ignore
*/
		if (m_edit.m_start_line>=0)
		{
			int len;
			if (insmode_val()!=2)
			{
				m_line_no = m_edit.m_start_line + 1;
				len = m_edit.GetLine(m_edit.m_start_line-m_edit.m_load_start, text, 1024);
				m_ncl_insert_ln = m_edit.m_ncl_insert_ln = -1;
			}
			else
			{
/*
.....delete current insert line if it's empty and in the current line in INSERT mode
*/
				if ((m_edit.m_ncl_insert_ln>=0)&&(m_edit.m_ncl_insert_ln>=0)&&(m_edit.m_ncl_insert_ln==m_edit.m_start_line))
				{
					ioerr = nclf_getsrc(&m_edit.m_ncl_insert_ln, sbuf, &len, &itype, &icmt);
					if (len==0)
						nclc_delsrc(m_edit.m_ncl_insert_ln, m_edit.m_ncl_insert_ln);
					m_edit.m_ins_save = -1;
				}
				else
					UW_insert_line = m_edit.m_ncl_insert_ln;
				m_line_no = m_edit.m_start_line + 1;
				len = m_edit.GetLine(m_edit.m_start_line-m_edit.m_load_start, text, 1024);
			}
			setnln(&m_line_no);
			text[len] = '\0';
		}
		else
		{	
			text[0] = '\0';
/*
......we do this only if the window is already active
......because we set at begin when there is no file loaded but it is active
*/
			if (txtwin->IsWindowEnabled()) 
			{
				m_line_no = 1;
				setnln(&m_line_no);
			}
		}
		txtwin->ShowWindow(SW_SHOW);
		m_extend = 0;
		m_sizeDefault.cy = delta_cy + (rect1.bottom-rect1.top);
		NCL_cmdwin = txtwin->m_hWnd;
		if (NCL_cmdmod==0)
		{
/*
.....prompt mode
*/
			m_edit_single.GetWindowText(whole_text);
			txtwin->SetWindowText(whole_text);
		}
		else
		{
			txtwin->SetWindowText(text);
			sprintf_s(prompt, 256, "edit line %d: ", m_line_no);
			uw_ntsav_pprm(prompt);
			uw_ntwrprm(prompt);
			uw_ntsav_plabel("By Text");
			uw_ntwrplabel("By Text");
		}
		m_edit_single.ShowWindow(SW_HIDE);
		m_edit.ShowWindow(SW_HIDE);
		m_arrow.SetType(0);
		sel = 0;
	}
done:;
    m_szHorz =  m_sizeDefault;
    m_szVert = m_sizeDefault;
    m_szFloat = m_sizeDefault;
/*
......force resize
*/
	NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
	NCL_MainFrame->RecalcLayout();
	if (m_extend)
	{
		if (NCL_cmdmod)
		{
			m_edit.SetFocus();
			m_edit.SetSelection(sel, sel, FALSE);
			m_edit.SelectCurrentCur(UW_text_cursor);
			if (UW_text_select)
			{
				m_edit.SelectCurrentLine();
			}
			m_edit.GetCurPos();
		}
		else
			m_edit_single.SetFocus();
	}
	else
		txtwin->SetFocus();
}
/**********************************************************************
**    I_FUNCTION :  set_text(char *text)
**       Set the text (editing text) in form input field fieldno. 
**    PARAMETERS   
**       INPUT  : 
**          fieldno : field number
**			text:	text to set
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLCmdBar::set_text(char *text)
{
	CWnd *editwin;
	if ((m_extend)&&(NCL_cmdmod))
	{
		editwin = GetDlgItem(IDC_ECOMMAND2);
	}
	else if (m_extend)
	{
		editwin = GetDlgItem(IDC_ECOMMAND3);
	}
	else
		editwin = GetDlgItem(IDC_ECOMMAND);

	editwin->SetWindowText(text);
}

/**********************************************************************
**    I_FUNCTION :  set_label(int fieldno, char* label)
**       Set the label in form display field fieldno. 
**    PARAMETERS   
**       INPUT  : 
**          fieldno : field number
**			label:	label to set
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLCmdBar::set_label(char* label)
{
	CWnd *cwin = GetDlgItem(IDC_PCOMMAND);
	cwin->SetWindowText(label);
}
/***********************************************************************
c
c   SUBROUTINE:  CNCLCmdBar
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			dispflag: display flag
c			
c   OUTPUT: none
c
c***********************************************************************
*/

CNCLCmdBar::CNCLCmdBar(CWnd* pParent, int dispflag)	
{
	m_pParent = pParent;
	m_disptype = dispflag;
	m_extend = UDM_run_layout.command_active;
	m_scrollx = m_scrolly = 0;
	m_pEditBkBrush = new CBrush(RGB(255, 255, 255));
	m_total_line = 0;
	m_init = 0;
	m_ncl_insert_ln = -1;

}

/***********************************************************************
c
c   SUBROUTINE:  ~CNCLCmdBar
c
c   FUNCTION:  Destructor, free class data space
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLCmdBar::~CNCLCmdBar()
{
	DestroyWindow();
}

/***********************************************************************
c
c   FUNCTION: OnPaint()
c
c         The framework calls this member function when Windows 
c			or an application makes a request to repaint a 
c			portion of an application's window. The WM_PAINT 
c			message is sent when the UpdateWindow or RedrawWindow 
c			member function is called.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLCmdBar::OnPaint() 
{
	CNCLDialogBar::OnPaint();
}
/***********************************************************************
c
c   SUBROUTINE:  initcmdwin()
c
c   FUNCTION:  This function initialize class data
c
c   INPUT:  
c			none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLCmdBar::initcmdwin(int bar_size[2])
{
	int llen, clen, blen;
	int baseunitX;
	int baseunitY;
	int pt;
/*
.....header info
*/
	strcpy_s(m_title, "Command Window");
	m_TitleCaption = m_title;
		
	if ((UW_com_size>0) && (UW_com_size<=48))
		pt = 10 * UW_com_size;
	else
		pt = 80;
//	uw_ntget_avrchr_size(pt, "MS Sans Serif", &baseunitX, &baseunitY);
	uw_ntget_avrchr_size(pt, UW_com_font, &baseunitX, &baseunitY);

	m_dlgTempl.cx = bar_size[0];
/*
.....convert screen pixel to dialog unit
*/
	m_dlgTempl.cx = (m_dlgTempl.cx * 4) / baseunitX;
	m_dlgTempl.cy = bar_size[1];
	m_dlgTempl.cy = (m_dlgTempl.cy * 8) / baseunitY;
	m_dlgTempl.x = 0;
	m_dlgTempl.y = 0;
/*
......40 is average len for 10 character
......40 = (baseunitX * 10) * (4/baseunitX)  (second part is converting to dialog unit
*/
	if (48>m_dlgTempl.cx)
		llen = 0;
	else
		llen = 40;

	int ly, lcy;
//	if (baseunitY<m_dlgTempl.cy)
	{
		ly = (40/baseunitY);
		lcy = m_dlgTempl.cy-(80/baseunitY);
	}
/*	else
	{
		ly = 0;
		lcy = m_dlgTempl.cy;
	}
*/
	m_rgDlgItem[0].Inittemp("", 4, 
				IDC_PCOMMAND, (48/baseunitX), ly, llen, lcy);
	if (llen==0)
		clen = 0;
	else
	{
		if (LW_nclipv!=LW_STANDALONE)
			clen = (m_dlgTempl.cx-(60/baseunitX)) - llen - (100/baseunitX);
		else
			clen = m_dlgTempl.cx - llen - (100/baseunitX);
	}
	if (clen<0)
		clen = 0;

	m_rgDlgItem[1].Inittemp("", 3, 
				IDC_ECOMMAND, (48/baseunitX)+llen, ly, clen, lcy);
	
	if (llen==0)
		blen = 0;
	else
		blen = (60/baseunitX);
	m_rgDlgItem[2].Inittemp("", 1, 
				IDC_CMD_ARROW, (48/baseunitX)+llen+clen, ly, blen, lcy);
	m_dlgTempl.cdit = 3;
	m_dlgTempl.style = WS_CHILD | DS_SETFONT;
	m_dlgTempl.dwExtendedStyle = 0;
}

/***********************************************************************
c
c   SUBROUTINE:  CreateCmdBar()
c
c   FUNCTION:  This function Create command window bar
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

BOOL CNCLCmdBar::CreateCmdBar()
{
	UINT type;
	int stat;
	int bar_pos[5], bar_size[2];
	CRect rect;	

	bar_pos[0] = UDM_run_layout.command_pos[0];
	bar_pos[1] = UDM_run_layout.command_pos[1];
	bar_pos[2] = UDM_run_layout.command_pos[2];
	bar_pos[3] = UDM_run_layout.command_pos[3];
	bar_pos[4] = UDM_run_layout.command_pos[4];

	bar_size[0] = UDM_run_layout.command_size[0];
	bar_size[1] = UDM_run_layout.command_size[1];
		
	type = WS_CHILD | WS_VISIBLE | CBRS_SIZE_DYNAMIC | CBRS_FLYBY
						| CBRS_TOOLTIPS;
	if (UDM_run_layout.command_type==0)
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
	initcmdwin(bar_size);
	stat = CreateFormWin2(NCL_MainFrame, m_rgDlgItem, m_dlgTempl, 3, type, IDD_COMMANDBAR);
	if (stat==-1)
		return stat;
	EnableDocking(CBRS_ALIGN_BOTTOM | CBRS_ALIGN_TOP);
	m_created = 1;
	return stat;
}
/***********************************************************************
c
c   SUBROUTINE:  PostNcDestroy() 
c
c   FUNCTION:  This function called when Destroy window
c				it delete object pointer.
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CNCLCmdBar::PostNcDestroy() 
{
	delete m_pEditBkBrush;
}

/***********************************************************************
**
**   FUNCTION: enable_win(int flag)
**
**       Enable/disable the command window
**
**   INPUT:  flag: 1: enable. 0: disable
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::enable_win(int flag)
{
	CWnd *editwin1, *editwin2, *editwin3;
	CWnd *labwin = GetDlgItem(IDC_PCOMMAND);
	editwin1 = GetDlgItem(IDC_ECOMMAND);
	editwin2 = GetDlgItem(IDC_ECOMMAND2);
	editwin3 = GetDlgItem(IDC_ECOMMAND3);
	if (flag)
	{
		NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
		editwin1->EnableWindow(TRUE);
		editwin2->EnableWindow(TRUE);
		editwin3->EnableWindow(TRUE);
		if (LW_nclipv!=LW_STANDALONE)
			m_arrow.EnableWindow(TRUE);
		labwin->EnableWindow(TRUE);
	}
	else
	{
		editwin1->EnableWindow(FALSE);
		editwin2->EnableWindow(FALSE);
		editwin3->EnableWindow(FALSE);
		if (IsFloating())
		{
			NCL_MainFrame->ShowControlBar(this, FALSE, FALSE);
		}
		NCL_MainFrame->RecalcLayout();
	}
}
/***********************************************************************
**
**   FUNCTION: insert_cmd(int sub)
**
**       insert a key into command
**
**   INPUT:  sub: keycode index
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
int CNCLCmdBar::insert_cmd(int sub)
{
	int start, end;
	char Keycode[18][3] = {"0", "1", "2", "3", "4",
		"5", "6", "7", "8", "9", ".", ",", "+", "-",
		"/", "*", VK_BACK, "\n"};
	CEdit *editwin;
	if (m_extend)
	{
		if (NCL_cmdmod)
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND2);
		else
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND3);
	}
	else
		editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND);
	if (editwin==NULL)
		return 0;
	if (editwin->IsWindowEnabled()) 
	{
		if ((sub!=16)&&(sub!=17))
			editwin->ReplaceSel(Keycode[sub]);
		else if (sub==16)
		{
			editwin->GetSel(start, end); 
			if (start==end)
			{
				if ((m_extend)&&(NCL_cmdmod))
				{
					m_edit.SetSelection(start-1, end, FALSE);
					m_edit.ReplaceSel("");
				}
				else
				{
					editwin->SetSel(start-1, end);
					editwin->ReplaceSel("");
				}
			}
		}
		else
		{
			NCL_KeyReturn = 1;
		}
	}
	return 0;
}
/***********************************************************************
**
**   FUNCTION: Set_ecomlin()
**
**       Set cursor at the text end
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::Set_ecomlin()
{
	int len, line, upload;
	CString label, text;
	if ((m_extend)&&(NCL_cmdmod))
	{
		m_edit.GetWindowText(text);
		len = text.GetLength();
		setcmdsel(len, len);
	}
	else
	{
		getcmdtext(label, text, line, upload);
		len = text.GetLength();
		setcmdsel(len, len);
	}
}

/***********************************************************************
**
**   FUNCTION: getcmdtext(CString label, CString text, int line)
**
**       Get the text from the command prompt/editor window, also the current line
**
**   INPUT:  none
**
**   OUTPUT :   label: command line prompt label
**			text: command edit text
**			line: current line
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::getcmdtext(CString &label, CString &text, int &line, int &upload)
{
	CWnd *editwin;
	line = 0;
	if ((m_extend)&&(NCL_cmdmod))
	{
		m_edit.GetWindowText(text);
		line = m_edit.m_end_line;
		if (m_edit.m_done_upload!=0)
			upload = 0;
		else
			upload = m_edit.m_load_start;
	}
	else
	{
		if (m_extend)
			editwin = GetDlgItem(IDC_ECOMMAND3);
		else
			editwin = GetDlgItem(IDC_ECOMMAND);
		editwin->GetWindowText(text);
		upload = 0;
	}
	GetDlgItem(IDC_PCOMMAND)->GetWindowText(label);
}
/***********************************************************************
**
**   FUNCTION: getcmdtext2(CString text, int line)
**
**       Get current line the text from the command editor window, also the current line number
**
**   INPUT:  none
**
**   OUTPUT :   label: command line prompt label
**			text: command edit text
**			line: current line (in whole PP file)
**			
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::getcmdtext2(CString &text, int &line)
{
	CWnd *editwin;
	char textstr[1024];
	int len;
	if ((m_extend)&&(NCL_cmdmod))
	{
		line = m_edit.m_end_line;
		if ((m_edit.m_done_upload==0)&&(m_edit.m_load_start>0))
			line = m_edit.m_end_line - m_edit.m_load_start;
		len = m_edit.GetLine(line, textstr, 1023);
		textstr[len] = '\0';
		text = textstr;
		line = m_edit.m_end_line;
	}
	else
	{
		if (m_extend)
			editwin = GetDlgItem(IDC_ECOMMAND3);
		else
			editwin = GetDlgItem(IDC_ECOMMAND);
		editwin->GetWindowText(text);
		line = 0;
	}
}

/***********************************************************************
**
**   FUNCTION: setcmdtext(CString label, CString text, int line, int upload)
**
**       Set the text into the command editor window and set the current line
**
**   INPUT:  label: command line prompt label
**			text: command edit text
**			line: current line
**			upload: upload flag
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::setcmdtext(CString label, CString text, int line, int upload)
{
	CWnd *editwin;
	if ((m_extend)&&(NCL_cmdmod))
	{
		m_edit.SetWindowText(text);
		int actline = line;
		if (upload>0)
		{
			m_edit.m_load_start = upload;
			m_edit.m_done_upload = 0;
			actline = line - m_edit.m_load_start;
		}
		else
		{
			m_edit.m_load_start = 0;
			m_edit.m_done_upload = 1;
		}
		int charinx = m_edit.LineIndex(actline);
		if (text=="")
			m_edit.SetSel(charinx,charinx, FALSE);
		else
			m_edit.SetSelection(charinx,charinx, FALSE);
	}
	else
	{
		if (m_extend)
			editwin = GetDlgItem(IDC_ECOMMAND3);
		else
			editwin = GetDlgItem(IDC_ECOMMAND);
		editwin->SetWindowText(text);
	}
	GetDlgItem(IDC_PCOMMAND)->SetWindowText(label);
	UpdateScrollBar();
}

/***********************************************************************
**
**   FUNCTION: getcmdwin()
**
**       Get the window handler of the command editor window
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
HWND CNCLCmdBar::getcmdwin()
{
	CWnd *editwin;
	if (m_extend)
	{
		if (NCL_cmdmod)
			editwin = GetDlgItem(IDC_ECOMMAND2);
		else
			editwin = GetDlgItem(IDC_ECOMMAND3);
	}
	else
		editwin = GetDlgItem(IDC_ECOMMAND);
	return editwin->m_hWnd;
}
/***********************************************************************
**
**   FUNCTION: insert_cmdstr(CString string)
**
**       Insert a string for command window
**
**   INPUT:  string: string to be insert
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::insert_cmdstr(CString string)
{
	CEdit *editwin;
	if (m_extend)
	{
		if (NCL_cmdmod)
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND2);
		else
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND3);
	}
	else
		editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND);
	if (editwin==NULL)
		return;
	if (editwin->IsWindowEnabled()) 
	{
		editwin->ReplaceSel(string);
	}
}
/***********************************************************************
**
**   FUNCTION: replace_cmdstr(char *string)
**
**       replace current line with a new string for command window
**
**   INPUT:  string: string to be insert
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::replace_cmdstr(char *string)
{
	CEdit *editwin;
	if (m_extend)
	{
		if (NCL_cmdmod)
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND2);
		else
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND3);
	}
	else
		editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND);
	if (editwin==NULL)
		return;
	if (NCL_cmdmod)
	{
		getnln(&m_line_no);
		m_edit.m_act_line = m_line_no-1;
/*
.....can't use SelectLines since this line number is PP file line number
.....for the editor, we only load part of it, not all line, so the line number
.....will be wrong. Use line = -1 which is current line here
*/
		int sel1 = m_edit.LineIndex(-1);
		int len = m_edit.LineLength(-1);
		int sel2 = sel1 + len;
		m_edit.SetSel(sel1, sel2, TRUE);
//		m_edit.SelectLines(m_line_no-1);
	}
	editwin->ReplaceSel(string);
}
/***********************************************************************
**
**   FUNCTION: setcmdsel(int start, int end)
**
**       Set the current selection position for command window
**
**   INPUT:  start, end: start and end selection
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::setcmdsel(int start, int end)
{
	CEdit *editwin;
	if ((m_extend)&&(NCL_cmdmod))	
	{
		editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND2);
		if (editwin==NULL)
			return;
		m_edit.SetSelection(start, end);
	}
	else
	{
		if (m_extend)
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND3);
		else
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND);
		if (editwin==NULL)
			return;
		if (editwin->IsWindowEnabled()) 
			editwin->SetSel(start, end);
	}
}
/***********************************************************************
**
**   FUNCTION: setcmdlsel(int start, int end)
**
**       Set the current selection position for command window
**			the cursor position is calulate from current line
**
**   INPUT:  start, end: start and end selection
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::setcmdlsel(int start, int end)
{
	if ((m_extend)&&(NCL_cmdmod))	
	{
		int line = m_line_no-1;
		if ((m_edit.m_done_upload==0)&&(m_edit.m_load_start>0))
			line = line - m_edit.m_load_start;
		int charinx = m_edit.LineIndex(line);
		start = start + charinx;
		end = end + charinx;
		m_edit.SetSelection(start, end);
	}
}


/***********************************************************************
**
**   FUNCTION: get_ecurpos(int *start, int *end)
**
**       Get the current selection position
**
**   INPUT:  none
**
**   OUTPUT :   start, end: start and end selection
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::get_ecurpos(int *start, int *end)
{
	CEdit *editwin;
	if (m_extend)
	{
		if (NCL_cmdmod)	
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND2);
		else
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND3);
	}
	else
		editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND);
	if (editwin==NULL)
	{
		*start = *end = 0;
		return;
	}
	int nStartChar, nEndChar;
	if (editwin->IsWindowEnabled()) 
	{
		if ((m_extend)&&(NCL_cmdmod))
		{
			nStartChar = m_edit.m_start_char;
			nEndChar = m_edit.m_end_char;
		}
		else
			editwin->GetSel(nStartChar, nEndChar); 
		*start = nStartChar;
		*end = nEndChar;
		return;
	}
	else
	{
		*start = *end = UD_textpre_curpos;
		return;
	}
}
/***********************************************************************
**
**   FUNCTION: get_lcurpos(int *start, int *end, int *ln)
**
**       Get the current selection position (count from the 0 from the current line) and current line
**
**   INPUT:  none
**
**   OUTPUT :   start, end: start and end selection
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::get_lcurpos(int *start, int *end, int *ln)
{
	CEdit *editwin;
	*ln = 0;
	if (m_extend)
	{
		if (NCL_cmdmod)	
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND2);
		else
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND3);
	}
	else
		editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND);
	if (editwin==NULL)
	{
		*start = *end = 0;
		return;
	}
	int nStartChar, nEndChar;
	int charinx;
	if (editwin->IsWindowEnabled()) 
	{
		if ((m_extend)&&(NCL_cmdmod))
		{
			nStartChar = m_edit.m_start_char;
			nEndChar = m_edit.m_end_char;
			*ln = m_edit.LineFromChar(nEndChar);
			charinx = m_edit.LineIndex(*ln);
			nStartChar = nStartChar - charinx;
			nEndChar = nEndChar - charinx;
			if ((m_edit.m_done_upload==0)&&(m_edit.m_load_start>0))
				*ln = *ln + m_edit.m_load_start;
		}
		else
			editwin->GetSel(nStartChar, nEndChar); 
		*start = nStartChar;
		*end = nEndChar;
		return;
	}
	else
	{
		*start = *end = UD_textpre_curpos;
		*ln = 0;
		return;
	}
}

/***********************************************************************
**
**   FUNCTION: setcmdfocus(int focus)
**
**       Set the focus on the command edit window
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::setcmdfocus(int focus)
{
	CEdit *editwin;
	if (m_extend)
	{
		if (NCL_cmdmod)
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND2);
		else
			editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND3);
	}
	else
		editwin = (CEdit *)GetDlgItem(IDC_ECOMMAND);
	if (editwin==NULL)
		return;
	if (focus)
		editwin->SetFocus();
	else
		NCL_MainFrame->SetFocus();
}
void CNCLCmdBar::OnCommandLine()
{
	int pos;
	return;

	CEdit *cwin = (CEdit*)GetDlgItem(IDC_ECOMMAND);
	 
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

/***********************************************************************
**
**   FUNCTION: UpdateScrollBar()
**
**       update scrollbar info
**
**   INPUT: none
**
**   OUTPUT :
**			 none
**   RETURN: none
**
**********************************************************************/
void CNCLCmdBar::UpdateScrollBar()
{
	CWnd* pChildWnd = GetWindow(GW_CHILD);
	if (pChildWnd==NULL) return;
	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	if (pChildWnd==NULL) return;
	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	if (pChildWnd==NULL) return;
	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	if ((pChildWnd!=NULL)&&(m_extend))
	{
		SCROLLINFO si1, si2;
		si1.cbSize = sizeof(si1);
		si2.cbSize = sizeof(si2);
		if (m_scrolly==0)
		{
			pChildWnd->ShowScrollBar(SB_VERT, TRUE);
		}
		pChildWnd->GetScrollInfo(SB_VERT, &si2);

		CRect rect;
		pChildWnd->GetWindowRect(&rect);
		int vxsc = GetSystemMetrics(SM_CXVSCROLL);
		int cxf = GetSystemMetrics(SM_CXDLGFRAME);
		int vysc = GetSystemMetrics(SM_CYHSCROLL);

		if (si2.nPage<si2.nMax)
		{
			pChildWnd->ShowScrollBar(SB_VERT, TRUE);
			m_scrolly = 1;
		}
		else
		{
			pChildWnd->ShowScrollBar(SB_VERT, FALSE);
			m_scrolly = 0;
		}
	}
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
void CNCLCmdBar::OnSize(UINT nType, int cx, int cy) 
{
//	if ((cx!=0)&&(cy!=0)&&(m_origrect.Height()==0))
	if (m_init==0)
	{
		delta_cx = UDM_run_layout.command_size[0] - cx;
/*
......somehow set cx, cy here is not work, use delta_cx to adjust
*/
		cx = UDM_run_layout.command_size[0];
		cy = UDM_run_layout.command_size[1];
		CControlBar::OnSize(nType, cx, cy);
	}
	else
		CControlBar::OnSize(nType, cx, cy);
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
	CRect rect1 = m_child_rect[0];
	CRect rect2 = m_child_rect[1];
	CRect rect3 = m_child_rect[2];
	CRect rect4 = m_child_rect[3];
	CRect rect5 = m_child_rect[4];
	CRect rect6 = m_child_rect[5];
		
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
/*
.....first item: prompt
*/
	rect1.bottom = rc.Height();
	if (!(m_dwStyle & CBRS_ORIENT_HORZ))
	{
		rect1.bottom = rect1.bottom - 8;
	}
	else
		rect1.bottom = rect1.bottom - 3;

	if (rect1.bottom <= 5)
		rect1.bottom = 5;

	if (m_dwStyle & CBRS_FLOATING)
		rect1.left = 5;
	else if (m_dwStyle & CBRS_ORIENT_VERT)
		rect1.left = 5;
	else if (m_dwStyle & CBRS_ORIENT_HORZ)
		rect1.left = 13;
	else
		rect1.left = 5;
/*
.....Third item and sixth item: buttons, stay the same size as original
.....but position may changed
*/
	rect6.top = rect3.top = rect1.top;
	rect6.bottom = rect3.bottom = rect1.top + m_child_rect[2].Height();
	rect6.right = rect3.right = rc.right - 5;
	rect6.left = rect3.left = rect6.right - 15;
/*
.....forth item: multi line edit box
*/
	int hgt;
	if (m_extend)
	{
/*
.....forth item and fifth item: multi line edit box
*/
		rect4.top =  rect5.top = rect1.top;
		rect4.right = rect5.right = rect6.left - 2;
		if (rect4.right <= rect4.left)
			rect4.right = rect4.left;
		rect5.right = rect4.right;

		rect4.bottom = rect5.bottom = rc.Height() - 4;
		hgt = rect4.bottom - rect4.top;
		if (hgt<14) hgt=14;
		rect4.bottom = rect5.bottom = rect4.top + hgt;
/*
.....second item: single line edit box, stay as orig height
.....but change the width with multi-line
*/
		rect2.right = rect4.right;
	}
	else
	{
/*
.....second item: single line edit box, stay as orig height
*/
		rect2.top =  rect1.top;
		rect2.right = rect6.left - 2;
//		rect2.bottom =  rc.Height() - 4;
		rect2.bottom =  rc.Height() + rect2.top - delta_cy;
		if (rect2.bottom<4) rect2.bottom = 4;
		if (rect2.right <= rect2.left)
			rect2.right = rect2.left;
/*
.....fourth item: multi line edit box, not changed
*/
		rect4.right = rect5.right = rect2.right;
	}
	CWnd* pChildWnd = GetWindow(GW_CHILD);
	if (pChildWnd!=NULL)
	{
		pChildWnd->MoveWindow(rect1);
		m_child_rect[0] = rect1;;	
	}
	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	if (pChildWnd!=NULL)
	{
		pChildWnd->MoveWindow(rect2);
		m_child_rect[1] = rect2;;	
	}
	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	if (pChildWnd!=NULL)
	{
		pChildWnd->MoveWindow(rect3);
		m_child_rect[2] = rect3;;	
	}
	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);

	if (pChildWnd!=NULL)
	{
		pChildWnd->MoveWindow(rect4);
		m_child_rect[3] = rect4;;	
	}
	if ((pChildWnd!=NULL)&&(m_extend)&&(NCL_cmdmod))
	{	
		SCROLLINFO si1, si2;
		si1.cbSize = sizeof(si1);
		si2.cbSize = sizeof(si2);

		if (m_scrolly==0)
		{
			pChildWnd->ShowScrollBar(SB_VERT, TRUE);
		}
		pChildWnd->GetScrollInfo(SB_HORZ, &si1);
		pChildWnd->GetScrollInfo(SB_VERT, &si2);
		int vxsc = GetSystemMetrics(SM_CXVSCROLL);
		int cxf = GetSystemMetrics(SM_CXDLGFRAME);
		int vysc = GetSystemMetrics(SM_CYHSCROLL);

		if (si2.nPage<si2.nMax)
		{
			pChildWnd->ShowScrollBar(SB_VERT, TRUE);
			m_scrolly = 1;
		}
		else
		{
			pChildWnd->ShowScrollBar(SB_VERT, FALSE);
			m_scrolly = 0;
		}
	}
	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	if (pChildWnd!=NULL)
	{
		pChildWnd->MoveWindow(rect5);
		m_child_rect[4] = rect5;	
	}
	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	if (pChildWnd!=NULL)
	{
		pChildWnd->MoveWindow(rect6);
		m_child_rect[5] = rect6;	
	}
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
**   FUNCTION: Load_cmdstr()
**
**       load current part program into the multi-lines window
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
int CNCLCmdBar::Load_cmdstr()
{
	CString whole_text;
	char sbuf[1024], prompt[256];
	int iline, ioerr, nc;
	short itype, icmt;

	if ((m_extend)&&(NCL_cmdmod))
	{
		m_edit.m_load_done = 0;
		getnln(&m_line_no);
/*
.....get current source line 
*/
		icmt = 0;
		ioerr = 0;
		whole_text = "";

		int start_line, end_line;

		for (iline=0; (ioerr==0&&(iline<1000000)); iline++)
		{
			ioerr = nclf_getsrc(&iline, sbuf, &nc, &itype, &icmt);
			if (ioerr!=0)
				break;
		}
		m_total_line = iline;
		m_edit.m_total_line = iline;
////////////////
		if (m_line_no<=UDM_layout.command_load[0])
		{
			start_line = 0;
			end_line = m_line_no + UDM_layout.command_load[0] - 1;
			m_edit.m_done_upload = 1;
			m_edit.m_done_dnload = 0;
		}
		else
		{
			start_line = m_line_no -  UDM_layout.command_load[0]/2;
			end_line = m_line_no + UDM_layout.command_load[0] - UDM_layout.command_load[0]/2;
			m_edit.m_done_upload = 0;
			m_edit.m_done_dnload = 0;
		}
		m_edit.m_load_start = start_line;
		icmt = 0;
		ioerr = 0;
		if (end_line>m_total_line)
			end_line = m_total_line;
		for (iline=start_line; (ioerr==0&&(iline<m_total_line)&&(iline<end_line)); iline++)
		{
			ioerr = nclf_getsrc(&iline, sbuf, &nc, &itype, &icmt);
			if (ioerr==0)
			{
				sbuf[nc] = '\0';
				if ((iline!=m_total_line-1)&&(iline==end_line-1))
					whole_text = whole_text + sbuf;
				else
					whole_text = whole_text + sbuf + "\r\n";
			}
			else
				break;
		}
		m_edit.SetWindowText(whole_text);
		SetScrollBarPos(m_total_line, m_line_no);
		UpdateScrollBar();
	
		int sel = 0;
		m_edit.m_start_line = m_edit.m_end_line = m_line_no-1;
		if (insmode_val()!=2)
		{
			m_edit.m_ncl_insert_ln = m_ncl_insert_ln = -1;
			sel = m_edit.LineIndex(m_line_no-start_line-1);
			m_edit.m_start_char = m_edit.m_end_char = sel;
		}
		else
		{
			m_ncl_insert_ln = m_line_no;
			sel = m_edit.LineIndex(m_ncl_insert_ln-start_line-1);
			m_edit.m_start_char = m_edit.m_end_char = sel;
/*
......do not insert an empty line if the insert line is not done
......before
*/
			if (!((UW_insert_line!=-1)&&(UW_insert_line==m_line_no-1)))
			{
				if (m_line_no<=m_total_line)
					m_edit.InsertLine();
				else
				{
					m_edit.m_ncl_insert_ln = -1;
					m_ncl_insert_ln = -1;
				}
			}
			else
				m_edit.m_ins_save = 1;
		}
		m_edit.SetSelection(sel, sel, FALSE);
		NCL_cmdwin = m_edit.m_hWnd;
		sprintf_s(prompt, 256, "edit line: %d", m_line_no);
		uw_ntsav_plabel(prompt);
		uw_ntwrplabel(prompt);
		uw_ntsav_pprm("Edit Commands");
		uw_ntwrprm("Edit Commands");
		m_edit.m_whole_text = whole_text;
		if (iline<end_line)
		{
			m_edit.m_done_dnload = 1;
		}
		m_edit.m_load_start = start_line;
		m_edit.m_act_line = m_line_no-1;
		m_edit.SelectCurrentCur(UW_text_cursor);
		if (UW_text_select)
		{
			m_edit.SelectCurrentLine();
		}
		m_edit.GetCurPos();
		m_edit.m_load_done = 1;
		m_edit.RedrawColorText();
	}
	return 0;
}

/***********************************************************************
c
c   FUNCTION: InsertLine
c
c         Insert a line before current line on the multi-command window
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLCmdBar::InsertLine()
{
	if (!((m_extend)&&(NCL_cmdmod)))
		return;
	m_edit.InsertLine();
}
/***********************************************************************
c
c   FUNCTION: DeleteLine
c
c         Delete current line on the multi-command window
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLCmdBar::DeleteLine()
{
	if (!((m_extend)&&(NCL_cmdmod)))
		return;
	m_edit.DeleteLine();
}
/***********************************************************************
c
c   FUNCTION: ActLine
c
c         Set the current line as the active line in the multi-command window
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLCmdBar::ActLine()
{
	if (!((m_extend)&&(NCL_cmdmod)))
		return;
	m_edit.ActLine();
}
/***********************************************************************
**
**   FUNCTION: ClearCurrentLine()
**
**       Clear the current text line for multi-lines window
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLCmdBar::ClearCurrentLine()
{
	if (!((m_extend)&&(NCL_cmdmod)))
		return;
	m_edit.ClearCurrentLine();
}

/***********************************************************************
**
**   FUNCTION: GetWindowSize(int &cx, int &cy, int &cols, int &rows)
**
**      get the multi-edit window size by rows and the command window size by pixel
**
**   INPUT: none
**
**   OUTPUT :
**			 cols, rows: rows and cols of the window
**   RETURN: none
**
**********************************************************************/
void CNCLCmdBar::GetWindowMultiSize(int &cx, int &cy, int &rows, int ffloat)
{
	double wid, hgt;
	CRect rect;

	int vsc = GetSystemMetrics(SM_CXVSCROLL); 
	int cxf = GetSystemMetrics(SM_CXDLGFRAME);
	int cyf = GetSystemMetrics(SM_CYDLGFRAME);

	CClientDC dc(this);
	CFont* pOldFont = dc.SelectObject(&m_txtfont);
	CSize sizeText = dc.GetTextExtent("XXXXXxxxxx",10);
	dc.SelectObject(pOldFont);

	wid = (sizeText.cx/10.0);
	hgt = sizeText.cy;
	
	m_edit.GetWindowRect(&rect);

	rows = (int)((rect.Height() - 2*cxf)/hgt + 0.5);

	CWnd *editwin1 = GetDlgItem(IDC_ECOMMAND);
	editwin1->GetWindowRect(&rect);

	cx = m_sizeDefault.cx + delta_cx;
	cy = rect.Height()+delta_cy;
}
/***********************************************************************
**
**   FUNCTION: ShowWindow()
**
**       This function show the command line window
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
int CNCLCmdBar::ShowWindow()
{
	int pos[2];
	if (m_created==0)
		return -1;
	if (m_visible!=1)
	{
		if ((UDM_run_layout.command_pos[2]!=-1) 
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
			uw_ntget_menurect(UDM_run_layout.command_pos[2], &rect, dockable);
			if (dockable==UDM_AREA_NODOCK)
/*
......replace/hide the control bars inside this area
*/
				uw_nthide_bars(UDM_run_layout.command_pos[2]);
			if (UDM_run_layout.command_pos[2]==1)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_TOP, &rect);
			else if (UDM_run_layout.command_pos[2]==2)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_BOTTOM, &rect);
			else if (UDM_run_layout.command_pos[2]==3)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_LEFT, &rect);
			else if (UDM_run_layout.command_pos[2]==4)
				NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_RIGHT, &rect);
		}
		else
		{
			CPoint pt(0,0);
			CRect rect;
			int tsc = GetSystemMetrics(SM_CYCAPTION);
			int cyf = GetSystemMetrics(SM_CYDLGFRAME);
			NCL_Main_View->GetWindowRect(&rect);

			if (UDM_run_layout.command_att[0]==0)
				pos[1] = rect.top + UDM_run_layout.command_pos[4];
			else if (UDM_run_layout.command_att[0] ==1)
			{
				pos[1] = rect.bottom + UDM_run_layout.command_pos[4];
			}
			else
				pos[1] = rect.top + UDM_run_layout.command_pos[4];

			if (UDM_run_layout.command_att[1] ==0)
				pos[0] = rect.left + UDM_run_layout.command_pos[3];
			else if (UDM_run_layout.command_att[1] ==1)
			{
				pos[0] = rect.right + UDM_run_layout.command_pos[3];
			}
			else
				pos[0] = rect.left + UDM_run_layout.command_pos[3];

			if (UDM_run_layout.command_att[0]==1)
				pos[1] -= ((NCL_MainFrame->m_commandBar)->m_szFloat.cy
								+ tsc);
			if (UDM_run_layout.command_att[1] ==1)
				pos[0] -= (NCL_MainFrame->m_commandBar)->m_szFloat.cx;
			pt.Offset(pos[0], pos[1]);			
			NCL_MainFrame->FloatControlBar(NCL_MainFrame->m_commandBar, pt);	
		}
	}	

	CWnd *editwin1;
	editwin1 = GetDlgItem(IDC_ECOMMAND);
	if (m_visible==0)
		m_extend = UDM_run_layout.command_active;
	if (m_extend)
	{
		CRect rect2;
		editwin1->ShowWindow(SW_HIDE);
		m_extend = 1;
		reshow_edit();
		if (m_type==1)
			m_edit.GetWindowRect(&rect2);
		else
			m_edit_single.GetWindowRect(&rect2);
		m_arrow.SetType(1);
	}
	else
	{
		m_edit_single.ShowWindow(SW_HIDE);
		m_edit.ShowWindow(SW_HIDE);
		editwin1->ShowWindow(SW_SHOW);
		CRect rect1;
		editwin1->GetWindowRect(&rect1);
		NCL_cmdwin = editwin1->m_hWnd;
		m_extend = 0;
		m_arrow.SetType(0);
	}

	NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
	NCL_MainFrame->RecalcLayout();
	m_visible = 1;

	return 0;
}
void CNCLCmdBar::reset_active_ln()
{
	m_edit.m_act_line = -1;
}

/***********************************************************************
**
**   FUNCTION: SetScrollBarPos(int total, int pos)
**
**       This function try to similate the scrolling bar position for
**			big files with partial loading, but not doing good and not used now
**
**   INPUT:  total:
**				pos:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLCmdBar::SetScrollBarPos(int total, int pos)
{
return;
	SCROLLINFO si;
	si.cbSize = sizeof(si);
	if (m_scrolly==0)
	{
		m_edit.ShowScrollBar(SB_VERT, TRUE);
	}
	m_edit.GetScrollInfo(SB_VERT, &si);
	if (total>si.nPage)
	{
		si.nMax = total;
		si.nPos = pos;
		m_edit.SetScrollInfo(SB_VERT, &si, TRUE);
	}
	else
		m_edit.ShowScrollBar(SB_VERT, FALSE);
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
void CNCLCmdBar::OnMouseMove(UINT nFlags, CPoint point) 
{	
	CNCLDialogBar::OnMouseMove(nFlags, point);

	CPoint pt=point;
	CRect rect;
	if (LW_nclipv!=LW_STANDALONE)
	{
		m_arrow.GetWindowRect(&rect);
		ScreenToClient(&rect);

		if(rect.PtInRect(pt))
		{
			m_arrow.DrawHighLight(rect);
		}
		else
			m_arrow.DrawNormal(rect);
	}
}
void CNCLCmdBar::set_cmd_lnend()
{
	m_edit.m_end = 1;
}

void CNCLCmdBar::GetSingleRect(RECT &rect)
{
	GetWindowRect(&rect);
	RECT text_rect;
	CWnd *txtwin = GetDlgItem(IDC_ECOMMAND);
	txtwin->GetWindowRect(&text_rect);
	rect.bottom = rect.top + delta_cy + (text_rect.bottom-text_rect.top);
}
int CNCLCmdBar::HandleSpecialKey(MSG* pMsg)
{
	int ret = 0;
	if (m_extend)
	{
		ret = m_edit.HandleSpecialKey(pMsg);
	}
	return ret;
}
void CNCLCmdBar::SetTFont()
{
	CWnd *txtwin = GetDlgItem(IDC_ECOMMAND);
	txtwin->SetFont(&m_txtfont);
	m_edit.SetFont(&m_txtfont);
	m_edit_single.SetFont(&m_txtfont);
}
void CNCLCmdBar::Deset_insert()
{
	if (m_ncl_insert_ln>=0)
		nclc_delsrc(m_ncl_insert_ln, m_ncl_insert_ln);
	m_ncl_insert_ln = m_edit.m_ncl_insert_ln = -1;
	m_edit.m_ins_save = -1;
}
void CNCLCmdBar::set_insline(int line)
{
	m_ncl_insert_ln = line;
}

void CNCLCmdBar::handle_insln()
{
	short itype, icmt;
	int ioerr, len;
	char sbuf[1024];

	if (m_extend==0)
		return;
	if (insmode_val()==2)
	{
		m_edit.SaveInsertLine();
		if ((m_edit.m_ncl_insert_ln>=0)&&(m_edit.m_ncl_insert_ln==m_edit.m_start_line))
		{
			ioerr = nclf_getsrc(&m_edit.m_ncl_insert_ln, sbuf, &len, &itype, &icmt);
			if (len==0)
				nclc_delsrc(m_edit.m_ncl_insert_ln, m_edit.m_ncl_insert_ln);
			m_edit.m_ins_save = -1;
		}
		else
			UW_insert_line = m_edit.m_ncl_insert_ln;
/*
......check if the last time (added to edit) is saved, if yes and it's empty, delete it
*/
			ioerr = nclf_getsrc(&m_edit.m_total_line, sbuf, &len, &itype, &icmt);
			if ((ioerr==0)&&(len==0))
				nclc_delsrc(m_edit.m_total_line, m_edit.m_total_line);
	}
}

/***********************************************************************
**
**   FUNCTION: uw_cmd_extend()
**
**       check if the command line window is the multi-lines
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    1: it is the multi-lines window
**
**********************************************************************/
extern "C" int uw_cmd_extend()
{
	if ((NCL_MainFrame!=NULL)&&(NCL_MainFrame->m_commandBar!=NULL))
		return NCL_MainFrame->m_commandBar->get_extend();
	return 0;
}

extern "C" void set_insline(int *nline)
{
	if ((NCL_MainFrame!=NULL)&&(NCL_MainFrame->m_commandBar!=NULL))
		return NCL_MainFrame->m_commandBar->set_insline(*nline);
}
/* End of wsntcmdbar.cpp */
#endif
