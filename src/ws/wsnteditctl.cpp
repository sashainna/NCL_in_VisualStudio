/********************************************************************* 
**  NAME:  wsnteditctl.cpp
**
**			Native WinNT main graphic view functions
**			implementation of CNCLeditctl class functions
**	CONTAINS: CNCLeditctl  class functions
**			all functions declared in wsnteditctl.h
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**     wsnteditctl.cpp , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**     04/29/15 , 15:12:23
*********************************************************************/
#include "wsntstdafx.h"
#include "wsntctl.h"
#include <GL/gl.h>
#include <GL/glu.h>
#include	"mfort.h"

#include "dmotif.h"
#include "wsnteditctl.h"
#include "wsntcmdbar.h"
#include "wsgl.h"
#include "nclcmd.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" void gtpsrc(UM_int4*, UM_int2*, UM_f77_str);
extern "C" int ncl_cmd_key(char *linestr);
extern "C" void setnln(int *);
extern "C" void getnln(int *);

extern "C" void nclc_delsrc(int, int);
extern "C" void nclc_putsrc(int,char*,int,short,short);
extern "C" int nclf_getsrc(int *, char *, int *, short*, short*);
extern "C" void uw_ntecomlin();
extern "C" void uw_ntwrplabel(char*);
extern "C" void uw_ntsav_plabel(char*);

static int No_paint = 0;
extern "C"  int UW_text_cursor, UW_text_select;
extern "C" int insmode_val();
extern "C" void setifl(short*, short*);
extern "C" void getjfl(short*, int *);
extern "C" void setjfl(short*, int *);
extern "C" void ncl_reset_cmdline();
int UW_insert_line = -1;

static void Sedit_putsrc(int nline, char*sbuf, int nc, short ktype, short kins)
{
	UM_int4 bline;
	int status;
	int include = 0;
	char bbuf[1024];
	UM_int4 bnc;
	UM_int2 btype, bcmt;
	btype = 1;
	bcmt=1;
	bline = nline -1;
	if (bline>=0)
	{
		status = nclf_getsrc(&bline, bbuf, &bnc, &btype, &bcmt);
		if ((status!=-1)&&(btype==1))
			include = 1;
	}
	bline = nline + 1;
	status = nclf_getsrc(&bline, bbuf, &bnc, &btype, &bcmt);
	if ((status!=-1)&&(btype==1))
		include = 1;
	else
		include = 0;
	if (include==1)
		ktype = 1;
	else
		ktype = 0;
	nclc_putsrc(nline,sbuf,nc,ktype,kins);
}

void uw_convert_tabstr(char *linestr, char *linestr_to, int *tab_no, int *tab_value)
{
	int i, j, k, delta, len, total_len, tbint[500];
	total_len = 0;
	len = strlen (linestr);
	for (i=0,j=0; i<len;i++)
	{
		if (linestr[i]!='\t')
		{
			linestr_to[total_len] = linestr[i];
			total_len++;
			continue;
		}
/*
.....8 chars tab
*/
		delta = (8 - total_len%8);
		for (k=total_len; k<total_len+delta;k++)
			linestr_to[k] = ' ';
		total_len = total_len + delta;
		(*tab_no)++;
		tab_value[j] = delta;
	}
	linestr_to[total_len] = '\0';
}
/***********************************************************************
c
c   SUBROUTINE:  CNCLeditctl
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLeditctl::CNCLeditctl()
{
	Init();
	m_load_start = 0; /* loading start line */
	m_start_line = m_end_line = -1; /* selected lines, PP file position = edit lines position + m_load_start */
	m_act_line = -1;  /* active line, PP file position*/
	m_start_char = m_end_char = -1;  /* selected characters, actually edit position */
	m_edit_line = -1; /* edited/changed line, PP file position*/
	m_total_line = 0;
	m_whole_text = "";

	m_executed = m_error = m_changed = 0;
	m_show_lines = 0;
	m_continue_line = -1;
	m_ncl_insert_ln = -1;
	m_ins_save = -1;
	m_exe_start = -1;
}

/***********************************************************************
c
c   SUBROUTINE:  Init
c
c   FUNCTION:  Initial data value of the class
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLeditctl::Init()
{
	m_button_down = 0;
	m_x = m_y = -1;
	m_done_upload = 1;
	m_done_dnload = 1;
	m_end = 0;
	m_load_done = 1;
}

/***********************************************************************
c
c   SUBROUTINE:  ~CNCLeditctl
c
c   FUNCTION:  Destructor, free class data space
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLeditctl::~CNCLeditctl()
{
}

/***********************************************************************
c
c   SUBROUTINE:  SetShowLine
c
c   FUNCTION:  Get the edit showing line number and set the value
c				to the member value m_show_lines
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLeditctl::SetShowLine()
{
	CSize szText;
	CRect rect;
	CClientDC dc(this);
	GetRect(&rect);
	CFont* pOldFont = NULL;
	pOldFont = dc.SelectObject(m_txtfont );

	szText = dc.GetTextExtent("Xx");
	m_show_lines = (int)(((float)rect.Height())/(szText.cy)+0.5);
}


BEGIN_MESSAGE_MAP(CNCLeditctl, CEdit)
	//{{AFX_MSG_MAP(CNCLeditctl)
	ON_WM_PAINT()
	ON_CONTROL_REFLECT(EN_CHANGE, OnChange)
	ON_CONTROL_REFLECT(EN_MAXTEXT, OnMaxText)
	ON_CONTROL_REFLECT(EN_KILLFOCUS, LossFocusCallback)
	ON_WM_VSCROLL()
	ON_WM_HSCROLL()
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_MBUTTONDOWN()
	ON_WM_MBUTTONUP()
	ON_WM_RBUTTONDOWN()
	ON_WM_RBUTTONUP()
	ON_WM_KEYUP()
	ON_WM_KEYDOWN()
	ON_WM_DESTROY()
	ON_WM_MOUSEMOVE()
	ON_CONTROL_REFLECT(EN_UPDATE, OnUpdate)
	ON_WM_CREATE()
	ON_WM_ERASEBKGND()
//	ON_MESSAGE(IDC_EXECUTE, HandleCommandExe)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  isline_wrtpp(char *linestr)
c
c   FUNCTION:  check if the line need to write to PP file
c
c   INPUT:  linestr: line string to be checked
c   OUTPUT: none
c	Return: 1: Yes 0: no
c
c***********************************************************************
*/
static int isline_wrtpp(char *linestr)
{
	char inbuf[1024];
	int i, j;
	int len = strlen (linestr);
	strcpy_s(inbuf, 1024, linestr);
	for (i=0; (i<len && inbuf[i]==' '); i++) ;
	j = i+1;
	if (inbuf[i] == '*' )
	{
		for (j=i+1; (j<len && inbuf[j]==' '); j++) ;
	}             
	if (inbuf[i] != '*' || inbuf[j] == '*')
	{
		return 1;
	}
	return 0;
}
/////////////////////////////////////////////////////////////////////////////
// CNCLeditctl message handlers

/***********************************************************************
c
c   FUNCTION: OnPaint()
c
c         The framework calls this member function when Windows 
c			or an application makes a request to repaint a 
c			portion of an application's window. 
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::OnPaint() 
{
/*
.....the font not working unless setfont here before repaint
.....The SetFont is working but have to set after the window is showing
*/
//	SetFont(m_txtfont);
	CEdit::OnPaint();
/*
.....if selection, don't redraw here
*/
	int start, end;
	GetSel(start, end);
	if ((start==end)&&(No_paint==0)&&(IsWindowEnabled()))
		RedrawColorText();
}
/***********************************************************************
c
c   FUNCTION: MoveToCenter(int line)
c
c         Move the "line" in the center of the edit field. 
c
c   INPUT:  line: line number to be on the center (atcual line number from edit window)
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::MoveToCenter(int line)
{
	int half_lines = (int)(m_show_lines/2 + 0.5);
	if (half_lines>line)
		return;
	int line1 = GetFirstVisibleLine();
	int line2 = line1 + m_show_lines/2;
	int dline = line - line2;
			
//	No_paint = 1;
	LineScroll(dline);
//	No_paint = 0;
}

/***********************************************************************
c
c   FUNCTION: SetSelection(int start, int end, BOOL bNoscroll, int end_flag)
c
c         Set the selection of the edit field
c
c   INPUT:  start, end: start and end selection to be set
c			bNoscroll: if the line need to be scrolled
c			end_flag: 1: the selection need to be set at line end
c						2: the selection need to be set at the previous 
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::SetSelection(int start, int end, BOOL bNoscroll, int end_flag)
{
	int line, sel1, sel2;
/*
.....move the current line to the center
*/
	m_start_line = LineFromChar(start);
	m_end_line = LineFromChar(end);

	if (end_flag==1)
	{
		m_start_char = start;
		m_end_char = end;
		SetCursorAtLineEnd();
		GetSel(start, end);
	}
	else if (end_flag==2)
	{
		line = LineFromChar(end);
		sel1 = LineIndex(line) + m_cursor_pos;
		sel2 = LineIndex(line+1) - 2;
		if (sel1<=sel2)
		{
			SetSel(sel1, sel1, TRUE);
			start = end = sel1;
		}
		else
		{
			SetSel(sel2, sel2, TRUE);
			start = end = sel2;
		}
	}
	else
		SetSel(start, end, bNoscroll);
	if (bNoscroll==FALSE)
		MoveToCenter(m_end_line);

	if ((m_done_upload==0)&&(m_load_start>0))
	{ 
		m_start_line = m_start_line + m_load_start;
		m_end_line = m_end_line + m_load_start;
	}
	m_start_char = start;
	m_end_char = end;
	char prompt[256];
	sprintf_s(prompt,256,"edit line: %d", m_end_line+1);
	uw_ntsav_plabel(prompt);
	uw_ntwrplabel(prompt);
}
void CNCLeditctl::GetCurPos()
{
	int line,sel;
	line = LineFromChar(m_end_char);
	sel = LineIndex(line);
	m_cursor_pos = m_end_char - sel;
}
/***********************************************************************
c
c   FUNCTION: SelectCurrentCur(int flag)
c
c         Set the current cursor at beginning of the line or end of the line
c			depend on the 'flag'
c
c   INPUT:  flag: 0: beginning of the line
c				1: end of the line
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::SelectCurrentCur(int flag)
{
	int start_line = LineFromChar(m_start_char);
	int end_line = LineFromChar(m_end_char);
	int sel1, sel2;
	char linestr[1024];
	sel1 = LineIndex(start_line);
/*
......somehow LineLength not working currently, so use GetLine
......to get the length.
	int len = LineLength(line);
*/
	int len = GetLine(end_line, linestr, 1024);
	sel2 = LineIndex(end_line) + len;

	if (UW_text_cursor)
	{
		SetSel(sel2, sel2, FALSE);		
		m_end = 1;
	}
	else
	{
		SetSel(sel1, sel1, FALSE);	
		m_end = 0;
	}
}
/***********************************************************************
c
c   FUNCTION: InsertLine
c
c         Insert a line before current line
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::InsertLine()
{
	int line = LineFromChar(m_start_char);
	int sel;

	m_ncl_insert_ln = line;
	m_ins_save = -1;
	if ((m_done_upload==0)&&(m_load_start>0))
	{
		m_ncl_insert_ln = m_ncl_insert_ln + m_load_start;
	}
	sel = LineIndex(line);
	SetSel(sel, sel, FALSE);		
	ReplaceSel("\r\n");
	SetSel(sel, sel, FALSE);

	if (insmode_val()==2)
		m_edit_line = m_ncl_insert_ln; 
	SaveEditedLine(1);
	UW_insert_line = -1;
}
/***********************************************************************
c
c   FUNCTION: DeleteLine
c
c         Delete current line on the window
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::DeleteLine()
{
	char linestr[1024];
	int line, sel1, sel2;
	line = LineFromChar(m_start_char);
	sel1 = LineIndex(line);
/*
......somehow LineLength not working currently, so use GetLine
......to get the length.
	int len = LineLength(line);
*/
	int len = GetLine(line, linestr, 1024);
	sel2 = LineIndex(line) + len + 2;
	SetSel(sel1, sel2, TRUE);
	m_end_line++;
	ReplaceSel("");
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
void CNCLeditctl::ActLine()
{
	int line = LineFromChar(m_start_char)+1;
	setnln(&line);
	m_act_line = line-1;
	RedrawColorText();
}

/***********************************************************************
c
c   FUNCTION: SetCursorAtLineEnd()
c
c         Set the current cursor at end of the line
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::SetCursorAtLineEnd()
{
/*
.....see if the cursor need to set at the end
.....when move cursor up/down
*/
	if (m_end==0)
		return;

	int line, sel;
	line = LineFromChar(m_start_char);
/*
......somehow LineLength not working currently, so use GetLine
......to get the length.
	int len = LineLength(line);
*/
	char linestr[1024];
	int len = GetLine(line, linestr, 1024);
	sel = LineIndex(line) + len;
	SetSel(sel, sel, TRUE);
}

/***********************************************************************
c
c   FUNCTION: SelectCurrentLine()
c
c         Select the current line
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::SelectCurrentLine()
{
	char linestr[1024];

	if (UW_text_select==0)
	{
		return;
	}
	int line, sel1, sel2;
	line = LineFromChar(m_start_char);
	sel1 = LineIndex(line);
/*
......somehow LineLength not working currently, so use GetLine
......to get the length.
	int len = LineLength(line);
*/
	int len = GetLine(line, linestr, 1024);
	sel2 = LineIndex(line) + len;
	SetSel(sel1, sel2, TRUE);
}
/***********************************************************************
c
c   FUNCTION: OnChange() 
c
c         called after window updated the edit text change
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::OnChange() 
{
	static int start = 0;
	if (m_executed)
		return;
	if (start==1)
		return;
	start = 1;
	int sav_reset = 0;
	int old_start_line, old_end_line, len;
	char linestr[1024];
/*
.....selected old lines
*/
	old_start_line = m_start_line;
	old_end_line = m_end_line;
/*
.....get new select lines
*/
	GetSel(m_start_char, m_end_char); 
/*
......if m_start_char==m_end_char, then there is no selection
......the starting line should be same as old_start_line
*/
	m_end_line = LineFromChar(m_end_char);

	if (m_start_char==m_end_char)
	{
		if (old_start_line<m_end_line)
			m_start_line = old_start_line;
		else
			m_start_line = m_end_line;
	}
	else
		m_start_line = LineFromChar(m_start_char);

	m_end = 0;
/*
.....adjust position since the files may not fully loaded
*/
	if ((m_done_upload==0)&&(m_load_start>0))
	{
		m_start_line = m_start_line + m_load_start;
		m_end_line = m_end_line + m_load_start;
	}

	int i,j;
	int line_del = 0, line_add = 0;
	if (old_end_line!=old_start_line)
	{
/*
.....line deleted, delete from the temp PP file on line from old_start_line to old_end_line
*/
/*
.....do not delete current line 
*/
		if (old_start_line==m_start_line)
		{
			old_start_line++;
			if ((m_ncl_insert_ln!=-1)&&((old_start_line-1)<=m_ncl_insert_ln))
				m_ncl_insert_ln = -1;
		}
		if (old_start_line<=old_end_line)
		{
			nclc_delsrc(old_start_line, old_end_line);
			line_del = old_end_line - old_start_line + 1;
			if ((m_ncl_insert_ln!=-1)&&  
				(old_start_line<=m_ncl_insert_ln)&&(m_ncl_insert_ln<=old_end_line))
				m_ncl_insert_ln = -1;
		}
	}
	else 
	{
		if ((m_end_line==m_start_line)&&(old_start_line>m_start_line))
		{
/*
......lines deleted from old_start_line to m_start_line
*/
			nclc_delsrc(m_start_line+1, old_start_line);
			line_del = old_start_line - m_start_line;
			if ((m_ncl_insert_ln!=-1)&&  
				(old_start_line<=m_ncl_insert_ln)&&(m_ncl_insert_ln<=old_end_line))
				m_ncl_insert_ln = -1;
		}
	}
	if (m_end_line!=m_start_line)
	{
/*
.....line added, added into the temp PP file on line from m_start_line to m_end_line
*/
		if (m_start_line==old_start_line)
		{
/*
......just update this line
*/
			len = GetLine(m_start_line, linestr, 1024);
			linestr[len] = '\0';
			Sedit_putsrc(m_start_line, linestr, len, 0, 0);
			j = m_start_line+1;
			if ((m_start_line==m_ncl_insert_ln)&&(len>0))
				m_ins_save = 2;
			else if ((m_start_line==m_ncl_insert_ln)&&(len==0))
				m_ins_save = 0;
//			if (m_total_line==m_start_line)
//				m_total_line++;
		}
		else
			j = m_start_line;
		for (i=j; i<=m_end_line; i++)
		{
			len = GetLine(i, linestr, 1024);
			linestr[len] = '\0';
			Sedit_putsrc(i, linestr, len, 0, 1);
			line_add++;
			if ((i==m_ncl_insert_ln)&&(len>0))
				m_ins_save = 2;
			else if ((i==m_ncl_insert_ln)&&(len==0))
				m_ins_save = 0;
		}
		m_edit_line = m_start_line;
		if (line_add>0)
			SaveEditedLine(1);
	}
	else
/*
......updated the current line
*/
	{
/*
......don't update the source if it is edit in the same line
......remember it and updated when the focus changed
*/
		m_edit_line = m_start_line;
/*
......force to updated if this line just delete
*/
		if ((m_end_line==m_start_line)&&(line_del>0))
			SaveEditedLine(1);
	}
	if (m_act_line>=m_end_line)
		m_act_line = m_act_line + line_add - line_del;
	m_total_line = m_total_line + line_add - line_del;
	if (m_total_line<(m_end_line-m_start_line+1))
		m_total_line = m_end_line-m_start_line+1;
/*
......we need update ifl4(1) which is store total line of the PP file
......otherwise, it will get error
*/
	short idx = 1;
	int ival;
	getjfl(&idx, &ival);
	ival = ival + line_add - line_del;
	setjfl(&idx, &ival);
	InvalidateColorText();
	m_changed = 1;
	start = 0;
}
/***********************************************************************
c
c   FUNCTION: HandleCommandLoad(int flag, int &add_lns);
c
c       Handle the command input loading... 
c
c   INPUT:  flag: load flag, 0: load after the end line
c							1: load before the beginning line
c
c   OUTPUT :   add_lns: lines added
c   RETURN:    charecter length loaded
c
**********************************************************************/
int CNCLeditctl::HandleCommandLoad(int flag, int &add_lns)
{	
	short itype, icmt=1;
	char sbuf[1024];
	int nc, iline, ioerr, start, end, len = 0;
	CString load_text = "";

	add_lns = 0;
	if ((m_done_upload)&&(flag==1))
		return 0;
	if ((m_done_dnload)&&(flag==0))
		return 0;
	ioerr = 0;

	GetWindowText(m_whole_text);
	int count = GetLineCount() + m_load_start;
	if (flag==0)
	{
		start = count;
		end = start + UDM_layout.command_load[1];
		load_text = "\r\n";
		for (iline=start; (ioerr==0&&(iline<end)); iline++)
		{
			ioerr = nclf_getsrc(&iline, sbuf, &nc, &itype, &icmt);
			if (ioerr==0)
			{
				sbuf[nc] = '\0';
				if ((iline==end-1)&&(iline!=m_total_line-1))
					load_text = load_text + sbuf;
				else
					load_text = load_text + sbuf + "\r\n";
			}
			else
				break;
		}
		if (iline!=start)
			m_whole_text = m_whole_text + load_text;
		if (iline<end)
		{
			m_done_dnload = 1;
		}
		len = load_text.GetLength();
		add_lns = iline - start;
	}
	else
	{
		start = m_load_start - UDM_layout.command_load[1];
		if (start<0)
			start = 0;
		end = m_load_start;
		for (iline=start; (ioerr==0&&(iline<end)); iline++)
		{
			ioerr = nclf_getsrc(&iline, sbuf, &nc, &itype, &icmt);
			if (ioerr==0)
			{
				sbuf[nc] = '\0';
				load_text = load_text + sbuf + "\r\n";
			}
			else
				break;
		}
		m_whole_text = load_text + m_whole_text;
		if (start==0)
		{
			m_done_upload = 1;
			m_load_start = 0;
		}
		else
			m_load_start = start;
		len = load_text.GetLength();
		add_lns = iline - start;
	}
	SetWindowText(m_whole_text);
	((CNCLCmdBar*)GetParent())->UpdateScrollBar();
//	UpdateWindow();
	return len;
}

/***********************************************************************
c
c   FUNCTION: HandleCommandLoad2(int &add_lns);
c
c       Handle the command input loading... 
c
c   INPUT:  
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CNCLeditctl::HandleCommandLoad2(int &add_lns)
{	
	short itype, icmt=1;
	char sbuf[1024];
	int nc, iline, ioerr, start, end, len = 0;
	CString load_text = "";
	add_lns = 0;
	if ((m_done_upload)&&(m_done_dnload))
		return 0;
	ioerr = 0;
	len = 0;
	GetWindowText(m_whole_text);
	int count = GetLineCount();
	load_text = "";
	if (m_done_dnload==0)
	{
		start = count;
		end = start + UDM_layout.command_load[1];
		load_text = "\r\n";
		for (iline=start; (ioerr==0&&(iline<end)); iline++)
		{
			ioerr = nclf_getsrc(&iline, sbuf, &nc, &itype, &icmt);
			if (ioerr==0)
			{
				sbuf[nc] = '\0';
				load_text = load_text + sbuf + "\r\n";
			}
			else
				break;
		}
		if (iline!=start)
			m_whole_text = m_whole_text + load_text;
		if (iline<end)
		{
			m_done_dnload = 1;
		}
		len = load_text.GetLength();
	}
	load_text = "";
	if (m_done_upload)
	{
		start = m_load_start - UDM_layout.command_load[1];
		if (start<0)
			start = 0;
		end = m_load_start;
		for (iline=start; (ioerr==0&&(iline<end)); iline++)
		{
			ioerr = nclf_getsrc(&iline, sbuf, &nc, &itype, &icmt);
			if (ioerr==0)
			{
				sbuf[nc] = '\0';
				load_text = load_text + sbuf + "\r\n";
			}
			else
				break;
		}
		m_whole_text = load_text + m_whole_text;
		if (start==0)
		{
			m_done_upload = 1;
			m_load_start = 0;
		}
		else
			m_load_start = start;
		len = load_text.GetLength();
		add_lns = iline - start;
	}
	SetWindowText(m_whole_text);
	((CNCLCmdBar*)GetParent())->UpdateScrollBar();
	return len;
}

/***********************************************************************
c
c   FUNCTION: HandleCommandExe(WPARAM wParam, LPARAM lParam)
c
c      function executed after hit return and execute the one of the line 
c
c   INPUT:  wParam, lParam: not used
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::HandleCommandExe(WPARAM wParam, LPARAM lParam)
{
	short itype, icmt;
	char sbuf[1024], prompt[256];
	int nc, iline, ioerr;
	CString whole_text;
/*
......clear and reload the PP files
*/
	CWnd* editwin = ((CNCLCmdBar*)GetParent())->GetDlgItem(IDC_ECOMMAND);
	if (editwin!=0)
		editwin->ShowWindow(SW_HIDE);
	editwin = ((CNCLCmdBar*)GetParent())->GetDlgItem(IDC_ECOMMAND3);
	if (editwin!=0)
		editwin->ShowWindow(SW_HIDE);
	ShowWindow(SW_SHOW);
	Init();
	int line_no;
	getnln(&line_no);
	if (line_no>m_total_line+1)
	{
		line_no = m_total_line+1;
		setnln(&line_no);
	}
	if (m_error==-1)
	{
		line_no = m_continue_line;
	}
/* get current source line */
//	icmt = 0;
	icmt = 1;
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
//m_total_line should not change here since we didn't insert line by execute the functions
//only insert it by execute InsertLine function and change m_total_line there.
//execpt there is error then we used saved string instead of load from pp file
//except when the last line is executed
	if (m_total_line<m_act_line)
		m_total_line = m_act_line;

	if ((m_error>0)&&(m_total_line<m_error))
/*
......use saved string instead of load from pp file
*/
	{
		m_total_line = m_error;
	}
	else if (m_error==-1)
	{
		if (m_total_line<m_continue_line-1)
			m_total_line = m_continue_line-1;
	}
	if (line_no<=UDM_layout.command_load[0])
	{
		start_line = 0;
		end_line = line_no + UDM_layout.command_load[0];
		m_done_upload = 1;
		m_done_dnload = 0;
	}
	else
	{
		start_line = line_no - UDM_layout.command_load[0]/2;
		end_line = line_no + UDM_layout.command_load[0] - UDM_layout.command_load[0]/2;
		m_done_upload = 0;
		m_done_dnload = 0;
	}
//	icmt = 0;
	icmt = 1;
	ioerr = 0;
	if (end_line>m_total_line)
		end_line = m_total_line;
	for (iline=start_line; (ioerr==0&&(iline<m_total_line)&&(iline<end_line)); iline++)
	{
//		if (((m_error-1)==iline)&&(insmode_val()!=2))
		if ((m_error-1)==iline)
/*
......use saved string instead of load from pp file
*/
		{
			if ((iline!=m_total_line-1)&&(iline==end_line-1))
				whole_text = whole_text + m_exestr;
			else
				whole_text = whole_text + m_exestr + "\r\n";
			continue;
		}
		if (((m_error==-1)&&((m_continue_line-2)==iline))&&(insmode_val()!=2))
		{
			if ((iline!=m_total_line-1)&&(iline==end_line-1))
				whole_text = whole_text + m_exestr;
			else
				whole_text = whole_text + m_exestr + "\r\n";
			continue;
		}
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
	SetWindowText(whole_text);
	((CNCLCmdBar*)GetParent())->UpdateScrollBar();
	m_executed = 0;

	int sel = 0;
	m_start_line = m_end_line = line_no-1;
	if (insmode_val()!=2)
	{
		m_ncl_insert_ln = -1;
		sel = LineIndex(line_no-start_line-1);
		m_start_char = m_end_char = sel;
	}
	else
	{
		if (m_error>0)
		{
			sel = LineIndex(line_no-start_line-1);
			m_start_char = m_end_char = sel;
		}
		else if (m_error<0)
		{
			sel = LineIndex(line_no-start_line-1);
			m_start_char = m_end_char = sel;
/*
.....if the active line is at last line, do not insert a new line
*/
			if (m_start_line!=m_total_line)
				InsertLine();
			else
				m_ncl_insert_ln = -1;
		}
		else
		{
			sel = LineIndex(line_no-start_line-1);
			m_start_char = m_end_char = sel;
			m_ncl_insert_ln = line_no;
/*
.....if the active line is at last line, do not insert a new line
*/
			if (m_start_line!=m_total_line)
				InsertLine();
			else
				m_ncl_insert_ln = -1;
		}
	}
	m_load_start = start_line;
	SetSelection(sel, sel, FALSE);
	SelectCurrentCur(UW_text_cursor);
	if (UW_text_select)
	{
		SelectCurrentLine();
	}
	GetCurPos();
	sprintf_s(prompt,256,"edit line: %d", line_no);
	uw_ntsav_plabel(prompt);
	uw_ntwrplabel(prompt);
	if (iline<end_line)
	{
		m_done_dnload = 1;
	}
	m_act_line = line_no-1;
	m_error = 0;
	RedrawColorText();
	return;
}

/***********************************************************************
c
c   FUNCTION: OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
c
c      Callback function for vertical scroll
c
c   INPUT:  nSBCode: scroll code of operation
c			nPos: scrolling position
c			pScrollBar: scrolling bar
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{	
	int dline, line2, line3;
	static int start = 0;
	if (start)
		return;
	start = 1;

	int x,y,len, add_lns;
	GetSel(x,y);
	RedrawWindow();
	CEdit::OnVScroll(nSBCode, nPos, pScrollBar);

	SCROLLINFO si;
	si.cbSize = sizeof(si);
	GetScrollInfo(SB_VERT, &si);	
/*
......nSBCode: SB_BOTTOM, SB_LINEDOWN, SB_PAGEDOWN
*/
	if ((m_done_dnload==0)&& ((nSBCode==SB_BOTTOM)||
				(nSBCode==SB_LINEDOWN)||(nSBCode==SB_PAGEDOWN))&&(nPos==0))
	{
		if (si.nMax==si.nPos+m_show_lines-1)
		{
			No_paint = 1;
			line2 = GetFirstVisibleLine();
			HandleCommandLoad(0, add_lns);
			line3 = GetFirstVisibleLine();
			dline = line2 - line3;
			if (dline!=0)
			{
				LineScroll(dline);
			}
			SetSel(x,y,TRUE);
			No_paint = 0;
			PostMessage(WM_VSCROLL, (WPARAM)MAKEWPARAM(nSBCode, nPos), (LPARAM)pScrollBar);
		}
		goto done;
	}
	if ((m_done_upload==0)&& ((nSBCode==SB_TOP)||
				(nSBCode==SB_LINEUP)||(nSBCode==SB_PAGEUP)) &&(nPos==0))
	{
		if (si.nPos==0)
		{
			No_paint = 1;
			line2 = GetFirstVisibleLine();
			len = HandleCommandLoad(1, add_lns);
			line3 = GetFirstVisibleLine();
			dline = line2 - line3 + add_lns;
			if (dline!=0)
			{
				LineScroll(dline);
			}
			SetSel(x+len,y+len,TRUE);
			No_paint = 0;
			PostMessage(WM_VSCROLL, (WPARAM)MAKEWPARAM(nSBCode, nPos), (LPARAM)pScrollBar);
		}
		goto done;
	}		
done:;
	RedrawColorText();
	start = 0;
}

/***********************************************************************
c
c   FUNCTION: OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
c
c      Callback function for Horizetal scroll
c
c   INPUT:  nSBCode: scroll code of operation
c			nPos: scrolling position
c			pScrollBar: scrolling bar
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	static int start = 0;
	if (start)
		return;
	start = 1;
	RedrawWindow();
	CEdit::OnHScroll(nSBCode, nPos, pScrollBar);
	RedrawColorText();
	start = 0;
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
void CNCLeditctl::OnMouseMove(UINT nFlags, CPoint point) 
{
	CEdit::OnMouseMove(nFlags, point);
}

/***********************************************************************
**
**   FUNCTION: OnLButtonDown(UINT nFlags, CPoint pt)
**
**       callback for left button down
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLeditctl::OnLButtonDown(UINT nFlags, CPoint point) 
{	
	m_button_down = 1;
	m_x = point.x; 
	m_y = point.y;
	int sel1, sel2;
	CEdit::OnLButtonDown(nFlags, point);
	GetSel(sel1, sel2); 
	if (!((sel1==sel2)&&(sel2==m_start_char)&&(m_start_char==m_end_char)))
		m_end = 0;
//	m_current_line = LineFromChar(sel1);
}

/***********************************************************************
**
**   FUNCTION: OnLButtonUp(UINT nFlags, CPoint pt)
**
**       callback for left button up
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLeditctl::OnLButtonUp(UINT nFlags, CPoint point) 
{	
	int sav_start_line, sav_end_line;
	int sav_start_char, sav_end_char;
	sav_start_line = m_start_line;
	sav_end_line = m_end_line;
	sav_start_char = m_start_char;
	sav_end_char = m_end_char;

	m_button_down = 0;
	CEdit::OnLButtonUp(nFlags, point);

	GetSel(m_start_char, m_end_char); 
	m_start_line = LineFromChar(m_start_char);
	m_end_line = LineFromChar(m_end_char);
/*
......don't use it for now
......
	CPoint pt1 = PosFromChar(m_start_char);	
	CPoint pt2 = PosFromChar(m_end_char);	
	int current_line;
	if (point.y >= pt1.y+m_text_hgt)
		current_line = m_end_line;
	else
		current_line = m_start_line;
	if (current_line<m_current_line)
	{
		m_start_char = LineIndex(m_current_line);
		SetSel(m_start_char, m_end_char, TRUE);
		m_start_line = m_current_line;
	}
	else
		m_current_line = current_line;
*/
/*
.....adjust position since the files may not fully loaded
*/
	if ((m_done_upload==0)&&(m_load_start>0))
	{
		m_start_line = m_start_line + m_load_start;
		m_end_line = m_end_line + m_load_start;
	}
	if ((m_start_line==m_end_line)&&(sav_start_line!=m_start_line))
	{
		SelectCurrentLine();
		GetSel(m_start_char, m_end_char); 
	}
	GetCurPos();
	if (!((sav_start_char == m_start_char)&&(sav_end_char == m_end_char)))
		RedrawColorText();		
/*
......save the last active line (the line cursor is on when keydown)
......if the line changes
*/
	SaveEditedLine();
	char prompt[256];
	sprintf_s(prompt,256,"edit line: %d", m_end_line+1);
	uw_ntsav_plabel(prompt);
	uw_ntwrplabel(prompt);
	if (insmode_val()==2)
		m_edit_line = m_end_line;
}
/***********************************************************************
**
**   FUNCTION: OnMButtonDown(UINT nFlags, CPoint pt)
**
**       callback for middle mouse button up
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLeditctl::OnMButtonDown(UINT nFlags, CPoint point) 
{	
	CEdit::OnMButtonDown(nFlags, point);
}

/***********************************************************************
**
**   FUNCTION: OnMButtonUp(UINT nFlags, CPoint pt)
**
**       callback for middle mouse button up
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLeditctl::OnMButtonUp(UINT nFlags, CPoint point) 
{	
	int sav_start_line, sav_end_line;
	sav_start_line = m_start_line;
	sav_end_line = m_end_line;

	CEdit::OnMButtonUp(nFlags, point);
	GetSel(m_start_char, m_end_char); 
	m_start_line = LineFromChar(m_start_char);
	m_end_line = LineFromChar(m_end_char);
/*
.....adjust position since the files may not fully loaded
*/
	if ((m_done_upload==0)&&(m_load_start>0))
	{
		m_start_line = m_start_line + m_load_start;
		m_end_line = m_end_line + m_load_start;
	}
	if ((m_start_line==m_end_line)&&(sav_start_line!=m_start_line))
	{
		SelectCurrentLine();
		GetSel(m_start_char, m_end_char); 
	}
	GetCurPos();
	RedrawColorText();
/*
......save the last active line (the line cursor is on when keydown)
......if the line changes
*/
	SaveEditedLine();
	char prompt[256];
	sprintf_s(prompt,256,"edit line: %d", m_end_line+1);
	uw_ntsav_plabel(prompt);
	uw_ntwrplabel(prompt);
}
/***********************************************************************
**
**   FUNCTION: OnRButtonDown(UINT nFlags, CPoint pt)
**
**       callback for right mouse button down
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLeditctl::OnRButtonDown(UINT nFlags, CPoint point) 
{	
	CEdit::OnRButtonDown(nFlags, point);
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
void CNCLeditctl::OnRButtonUp(UINT nFlags, CPoint point) 
{
	int sav_start_line, sav_end_line;
	sav_start_line = m_start_line;
	sav_end_line = m_end_line;

	CEdit::OnRButtonUp(nFlags, point);
	GetSel(m_start_char, m_end_char); 
	m_start_line = LineFromChar(m_start_char);
	m_end_line = LineFromChar(m_end_char);
/*
.....adjust position since the files may not fully loaded
*/
	if ((m_done_upload==0)&&(m_load_start>0))
	{
		m_start_line = m_start_line + m_load_start;
		m_end_line = m_end_line + m_load_start;
	}
	if ((m_start_line==m_end_line)&&(sav_start_line!=m_start_line))
	{
		SelectCurrentLine();
		GetSel(m_start_char, m_end_char); 
	}
	GetCurPos();
	RedrawColorText();
/*
......save the last active line (the line cursor is on when keydown)
......if the line changes
*/
	SaveEditedLine();
	char prompt[256];
	sprintf_s(prompt,256,"edit line: %d", m_end_line+1);
	uw_ntsav_plabel(prompt);
	uw_ntwrplabel(prompt);
}

void CNCLeditctl::SaveInsertLine()
{
/*
......we force saved already
*/
//	return;
	if (m_executed)
		return;
	if (insmode_val()!=2)
		return;
	if (m_ncl_insert_ln==-1)
		return;
	int i;
	char linestr[1024], inbuf[1024];
	int ln = m_ncl_insert_ln - m_load_start;
	int len = GetLine(ln, linestr, 1024);
//	if (len<=0)
//		return;
	for (i=len-1; (i>=0 && inbuf[i]==' '); i--) ;
	len = i+1;
	linestr[len] = '\0';

	if (m_ins_save==-1)
		Sedit_putsrc(m_ncl_insert_ln, linestr, len, 0, 1);
	else
		Sedit_putsrc(m_ncl_insert_ln, linestr, len, 0, 0);
	if (len>0)
	{
		m_ins_save = 1;
	}
	else if (len==0)
	{
		m_ins_save = 0;
	}
}
/***********************************************************************
**
**   FUNCTION: SaveEditedLine(int force)
**
**       save the edited line
**
**   INPUT:  force: 1: force save
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLeditctl::SaveEditedLine(int force)
{
	char linestr[1024], inbuf[1024];
	int ln, len;
	int i, j;
	int ioerr, nc;
	short itype, icmt=1;
/*
......if it is execute the command, don't doing save
*/
	if (m_executed)
		return;
/*
.....if the command line alredy inactive, just return
*/
	if (IsWindowEnabled()==0)
		return;

//force = 1;
	if (((m_edit_line!=-1)&&(m_edit_line!=m_start_line))||(force))
	{
		if ((m_done_upload==0)&&(m_load_start>0))
			ln = m_edit_line - m_load_start;
		else
			ln = m_edit_line;

		if (ln<0)
			return;

		len = GetLine(ln, linestr, 1024);
		if ((m_edit_line==m_total_line)&&(len==0))
			return;
/*
......if the insert mode is on
......do not save it if there is an empty line
*/
		if (len>=0)
		{
			linestr[len] = '\0';
/*
.....if start with "*", don't change the original line, 
.....reverse back to the original line
*/
			strcpy_s(inbuf, 1024,linestr);
			for (i=0; (i<len && inbuf[i]==' '); i++) ;
			j = i+1;
			if (inbuf[i] == '*' )
			{
				for (j=i+1; (j<len && inbuf[j]==' '); j++) ;
			}             
			if (inbuf[i] != '*' || inbuf[j] == '*')
			{
				if (i<=len)
				{  
					Sedit_putsrc(m_edit_line, linestr, len, 0, 0);
				}
			}
			else
			{
				ioerr = nclf_getsrc(&m_edit_line, inbuf, &nc, &itype, &icmt);
				if (ioerr==0)
				{
					inbuf[nc] = '\0';
				}
				else
				{
					inbuf[0] = '\0';
				}
				int start_inx = LineIndex(ln);
				m_executed = 1;
				SetSel(start_inx, start_inx+len, TRUE);
				ReplaceSel(inbuf);
				m_executed = 0;
				if (ioerr==0)
					SetSelection(m_start_char, m_end_char); 
				RedrawColorText();
			}
		}
		if (len>0)
			m_edit_line = -1;
	}
}
/***********************************************************************
**
**   FUNCTION: is_active_line(int charinx, int *iLineBegins, int *iLineEndPos, int line, int *end_line)
**
**       check if the line is the active line
**
**   INPUT:  charinx: start character index of the line to be checked
**
**   OUTPUT :   iLineBegins, int *iLineEndPos: not used for now
**   RETURN:    1: it is the active line
**
**********************************************************************/
int CNCLeditctl::is_active_line(int charinx, int *iLineBegins, int *iLineEndPos, int line, int *end_line)
{
	int ln;
	*end_line = line;
	if ((m_done_upload==0)&&(m_load_start>0))
		ln = line + m_load_start;
	else
		ln = line;
	if ((m_act_line==ln)&&(line>=0))
	{
		return 1;
	}
	return 0;
}
/***********************************************************************
**
**   FUNCTION: is_curent_line(int charinx, int *iLineBegins, int *iLineEndPos, int line, int *end_line)
**
**       check if the line is the current line
**
**   INPUT:  charinx: start character index of the line to be checked
**
**   OUTPUT :   iLineBegins, int *iLineEndPos: not used for now
**   RETURN:    1: it is the current line
**
**********************************************************************/
int CNCLeditctl::is_curent_line(int charinx, int *iLineBegins,int *iLineEndPos, int line, int *end_line)
{
	int cur_indx = LineIndex(-1);
	
	*end_line = line;
	if (cur_indx==charinx)
	{
		return 1;
	}
	return 0;
}
/*
......line: the text field line, not PP file line
*/
void CNCLeditctl::Redraw_current_lntext(int line)
{
	char linestr[1024], linestr_to[2048];
	CSize szText;
	CPoint pt1, pt2;
	int len, charinx;
	int tab_no, tab_value[500];

	int i, j, total_len;
	CClientDC dc(this);

	int fline = GetFirstVisibleLine();
	if (line<fline)
		return;
	if (line>m_total_line)
		return;

	CRect FillRect;
	CRect rect;
	GetRect(&rect);
	CFont* pOldFont = NULL;
	pOldFont = dc.SelectObject(m_txtfont );

	szText = dc.GetTextExtent("Xx");
	m_text_hgt = szText.cy;
	m_show_lines = (int)( ((float)rect.Height())/(szText.cy)+0.5);
	int wid =(int)( ((float)(szText.cx))/2.0+0.5);
	int start_inx, end_inx;
/*
.....don't draw if outside the show line box
*/
	if (line>fline+m_show_lines)
		return;
	len = GetLine(line, linestr, 1023);
	charinx = LineIndex(line);
	linestr[len] = '\0';

	start_inx = 0;
	end_inx = len - 1;
			
	pt1 = PosFromChar(charinx);	
/*
.....get beginning showing character index
*/		
	while ((pt1.x<1)&&(start_inx<len))
	{
		start_inx++;
		pt1 = PosFromChar(charinx+start_inx);	
	}
/*
.....get endding showing character index
*/
	pt2 = PosFromChar(charinx+end_inx);	
	while ((pt2.x+wid>rect.right)&&(end_inx>start_inx))
	{
		end_inx--;
		pt2 = PosFromChar(charinx+end_inx);	
	}
	len = end_inx - start_inx + 1;
	strncpy_s(linestr, 1024, &(linestr[start_inx]), len);
	linestr[len] = '\0';

	uw_convert_tabstr(linestr, linestr_to, &tab_no, tab_value); 
	szText = dc.GetTextExtent(linestr_to);
				
	FillRect.top = pt1.y;
	FillRect.bottom = pt1.y + szText.cy;
	FillRect.left = pt1.x;
	FillRect.right = pt1.x + szText.cx;
	if (FillRect.bottom>rect.bottom) return;		
	if (FillRect.left<rect.left)
		FillRect.left = rect.left;
	if (FillRect.right>rect.right)
		FillRect.right = rect.right;
//	dc.TabbedTextOut(FillRect.left, FillRect.top, linestr, len, tab_no, (LPINT) &tab_value[0], FillRect.left);
	len = strlen(linestr_to);
	int ln;
	if (len>0)
		dc.TextOut(FillRect.left, FillRect.top, linestr_to, len);
	else 
	{
		szText = dc.GetTextExtent("Xx");
		ln = line;
		if (ln==0)
		{
			pt1.y = 2;
			goto start;
		}
again:;
		if (ln>0)
		{
			charinx = LineIndex(ln);	
			if (charinx<0)
				return;
			pt1 = PosFromChar(charinx);	
			if ((pt1.x==-1)&&(pt1.y==-1))
			{
				if (ln==m_total_line)
/*
......last ln
*/
				{
					ln--;
					goto again;
				}
			}
			pt1.x = 2;
			pt1.y = pt1.y + szText.cy*(line-ln);
		}
start:;
		pt1.x = 2;
/*
.....need erase the highlight color of one of the space
*/
		FillRect.left = pt1.x;
		FillRect.right = pt1.x + 0.5*szText.cx;
		FillRect.top = pt1.y;
		FillRect.bottom = pt1.y + szText.cy;
		dc.FillSolidRect(FillRect,dc.GetBkColor());
		return;
	}		
	dc.SelectObject(pOldFont);
}

/***********************************************************************
**
**   FUNCTION: Redraw_current_line(int line)
**
**       redraw the line as the current line (unless it is the active line,
**				the redraw as the active line)
**
**   INPUT:  line: line to be redraw (it is the text field line loaded, not PP file line)
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLeditctl::Redraw_current_line(int line)
{
	CClientDC dc(this);
	CFont* pOldFont = NULL;
	pOldFont = dc.SelectObject(m_txtfont );

	int fline = GetFirstVisibleLine();
	if (line<fline)
		return;
/*
......see if current line is the active line
*/
	if (line==m_act_line-m_load_start)
		DrawColorText(line, line, dc, 0);
	else
		DrawColorText(line, line, dc, 1);
	dc.SelectObject(pOldFont);
}

/***********************************************************************
**
**   FUNCTION: Redraw_text()
**
**       redraw all text showing on the editor
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLeditctl::Redraw_text()
{
	char linestr[1024], linestr_to[2048];
	int tab_no, tab_value[500],tlen;
	CSize szText;
	CPoint pt1, pt2;
	int len, charinx;
	CClientDC dc(this);
	int line_count = GetLineCount();

	CRect FillRect;
	int first = GetFirstVisibleLine();
	CRect rect;
	GetRect(&rect);
			
	CFont* pOldFont = NULL;
	pOldFont = dc.SelectObject(m_txtfont );

	szText = dc.GetTextExtent("Xx");
	m_show_lines = (int)(((float)rect.Height())/(szText.cy)+0.5);
	int wid = (int)(((float)(szText.cx))/2.0+0.5);
	int start_inx, end_inx;
	for (int i=first; i<line_count&&i<(first+m_show_lines);i++)
	{
		len = GetLine(i, linestr, 1023);
		if (len==0) continue;
		charinx = LineIndex(i);
		linestr[len] = '\0';

		start_inx = 0;
		end_inx = len - 1;
			
		pt1 = PosFromChar(charinx);	
/*
.....get beginning showing character index
*/		
		while ((pt1.x<1)&&(start_inx<len))
		{
			start_inx++;
			pt1 = PosFromChar(charinx+start_inx);	
		}
/*
.....get endding showing character index
*/
		pt2 = PosFromChar(charinx+end_inx);	
		while ((pt2.x+wid>rect.right)&&(end_inx>start_inx))
		{
			end_inx--;
			pt2 = PosFromChar(charinx+end_inx);	
		}
		len = end_inx - start_inx + 1;
		strncpy_s(linestr, 1024, &(linestr[start_inx]), len);
		linestr[len] = '\0';

//		szText = dc.GetTextExtent(linestr);
		uw_convert_tabstr(linestr, linestr_to, &tab_no, tab_value); 
		szText = dc.GetTextExtent(linestr_to);
				
		FillRect.top = pt1.y;
		FillRect.bottom = pt1.y + szText.cy;
		FillRect.left = pt1.x;
		FillRect.right = pt1.x + szText.cx;
		if (FillRect.bottom>rect.bottom) break;		
		if (FillRect.left<rect.left)
			FillRect.left = rect.left;
		if (FillRect.right>rect.right)
			FillRect.right = rect.right;
//		dc.TabbedTextOut(FillRect.left, FillRect.top, linestr, len, tab_no, (LPINT) &tab_value[0], FillRect.left);
		tlen = strlen(linestr_to);
		dc.TextOut(FillRect.left, FillRect.top, linestr_to, tlen);		
	}
	dc.SelectObject(pOldFont);
}

/***********************************************************************
**
**   FUNCTION: RedrawColorText()
**
**       redraw all text showing on the editor with colors
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLeditctl::RedrawColorText()
{
	int active, current, iLineBegins, iLineEndPos;
	CClientDC dc(this);
	CString strText;
	CPoint pt;

	int x,y;
	GetSel(x, y);

	int line = GetFirstVisibleLine();
	int charinx = LineIndex(line);
	int cur_indx = LineIndex(-1);
	int cur_len = LineLength(-1);
	int line_len = LineLength(line);
	int line_count = GetLineCount();
	int end_line = line;
	int sav_ln = line;

	CFont* pOldFont = NULL;
	pOldFont = dc.SelectObject(m_txtfont);

	CRect ClientRect;
	BOOL bContinue = TRUE;

	GetWindowText(strText);
	GetClientRect(ClientRect);

	CRect rect;
	GetRect(&rect);

	active = 0;
	current = 0;
	while (bContinue)
	{
		if ((active==0)&&(is_active_line(charinx, &iLineBegins, &iLineEndPos, line, &end_line)))
		{
			DrawColorText(line, end_line, dc, 0);
			iLineBegins = -1;
			active = 1;
		}
		else if ((current==0)&&(is_curent_line(charinx, &iLineBegins, &iLineEndPos, line, &end_line)))
		{
			DrawColorText(line, end_line, dc, 1);
			iLineBegins = -1;
			current = 1;
		}
		else if (!((m_start_line-m_load_start<=line)&&(line<=m_end_line-m_load_start)))
			Redraw_current_lntext(line);
		line++;
		if (line > line_count)
		{
			break;
		}
		else
		{
			line_len = LineLength(line);
			charinx = LineIndex(line);
			pt = PosFromChar(charinx);
			if (pt.y>rect.bottom) 
				break;
		}
	}
	dc.SelectObject(pOldFont);
}

/***********************************************************************
**
**   FUNCTION: DrawColorText(int start_line, int end_line, CClientDC& dc, int flag)
**
**       redraw the line as the current line (unless it is the active line,
**				the redraw as the active line)
**
**   INPUT:  start_line, end_line: lines to be redraw (it is the text field line loaded, not PP file line)
**				flag: 0: draw active line, 1: draw current line
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLeditctl::DrawColorText(int start_line, int end_line, CClientDC& dc, int flag)
{
	char linestr[1024], linestr1[1024];
	char linestr_to[2048];
	int tab_no, tab_value[500],tlen;
	CSize szText;
	CPoint pt1, pt2;
	int len, len1, charinx;

	CBrush* pNewBrush = NULL;
	CBrush* pOldBrush = NULL;
	CPen* pNewPen = NULL;
	CPen* pOldPen = NULL;
	try
	{
		pNewBrush = new CBrush;
	}
	catch(...)
	{
		pNewBrush = NULL;
	}
	ASSERT(pNewBrush);

	if (flag==0)
		VERIFY(pNewBrush->CreateSolidBrush(
				RGB(uw_color_table[(UDM_layout.command_clr[0])][0],
				uw_color_table[(UDM_layout.command_clr[0])][1],
				uw_color_table[(UDM_layout.command_clr[0])][2])));
	else
		VERIFY(pNewBrush->CreateSolidBrush(
				RGB(uw_color_table[(UDM_layout.command_clr[1])][0],
				uw_color_table[(UDM_layout.command_clr[1])][1],
				uw_color_table[(UDM_layout.command_clr[1])][2])));

	CRect FillRect;
	int nBkMode = dc.SetBkMode(TRANSPARENT);
	int x,y;
	GetSel(x, y);

	CRect rect;
	GetRect(&rect);

	szText = dc.GetTextExtent("Xx");
	int wid = (int)(((float)(szText.cx))/2.0+0.5);
	m_show_lines = (int)(((float)rect.Height())/(szText.cy)+0.5);

	LPINT tabint;
	int tbint;
	int start_inx, end_inx;
	int fline = GetFirstVisibleLine();
	for (int i=0; i<=(end_line-start_line);i++)
	{
		len = GetLine(start_line+i, linestr, 1024);
		linestr[len] = '\0';
		if (len<=0) 
		{
/*
.....if empty ln, still draw the highlight in one char of the spaces
*/
			charinx = LineIndex(start_line+i);
			pt1 = PosFromChar(charinx);	
			if ((pt1.x==-1)&&(pt1.y==-1))
			{
				if (start_line+i==m_total_line)
/*
......last ln
*/
				{
					pt1.x = 2;
					if (start_line!=0)
					{
						charinx = LineIndex(start_line+i-1);
						pt1 = PosFromChar(charinx);	
						pt1.y = pt1.y + szText.cy;
					}
					else
					{
						pt1.y = 2;
					}
				}
				else
					continue;
			}
			FillRect.top = pt1.y;
			FillRect.bottom = pt1.y + szText.cy;
			FillRect.left = pt1.x;
			FillRect.right = pt1.x + 0.5*szText.cx;

			if (FillRect.bottom>rect.bottom) break;
			if ((FillRect.right<rect.left)||(FillRect.bottom<rect.top))
				break;
			if (FillRect.bottom>rect.bottom)
				FillRect.bottom = rect.bottom;
			if (FillRect.top<rect.top)
				FillRect.top = rect.top;
			if (FillRect.left<rect.left)
				FillRect.left = rect.left;
			if (FillRect.right>rect.right)
				FillRect.right = rect.right;
			dc.FillRect(FillRect, pNewBrush);
			continue;
		}
		charinx = LineIndex(start_line+i);
		start_inx = 0;
		end_inx = len - 1;
		pt1 = PosFromChar(charinx);	
/*
.....get beginning showing character index
*/		
		while ((pt1.x<1)&&(start_inx<len))
		{
			start_inx++;
			pt1 = PosFromChar(charinx+start_inx);	
		}
/*
.....get endding showing character index
*/
		pt2 = PosFromChar(charinx+end_inx);	
		while ((pt2.x+wid>rect.right)&&(end_inx>start_inx))
		{
			end_inx--;
			pt2 = PosFromChar(charinx+end_inx);	
		}
		if ((x==y)||(x>charinx+len+1)||(y+1<charinx))
		{
			len = len - start_inx;
			if ((end_inx-start_inx)+1<len)
				len = end_inx-start_inx+1;
			strncpy_s(linestr, 1024, &(linestr[start_inx]), len);
			linestr[len] = '\0';
//			szText = dc.GetTextExtent(linestr);
			uw_convert_tabstr(linestr, linestr_to, &tab_no, tab_value); 
			szText = dc.GetTextExtent(linestr_to);
		
			FillRect.top = pt1.y;
			FillRect.bottom = pt1.y + szText.cy;
			FillRect.left = pt1.x;
			FillRect.right = pt1.x + szText.cx;
			if (FillRect.bottom>rect.bottom) break;
			if (FillRect.left>rect.right) break;
			if ((FillRect.right<rect.left)||(FillRect.bottom<rect.top))
				break;
			if (FillRect.bottom>rect.bottom)
				FillRect.bottom = rect.bottom;
			if (FillRect.top<rect.top)
				FillRect.top = rect.top;
			if (FillRect.left<rect.left)
				FillRect.left = rect.left;
			if (FillRect.right>rect.right)
				FillRect.right = rect.right;
			dc.FillRect(FillRect, pNewBrush);
			
			if (len==0) continue;			
//				dc.DrawText(linestr, len, &FillRect, DT_LEFT|DT_SINGLELINE|DT_TABSTOP);
//			tbint = 32;
//			tabint = (LPINT) &tbint;
//			dc.TabbedTextOut(FillRect.left, FillRect.top, linestr, len, tab_no, (LPINT)&tab_value[0], FillRect.left);
			tlen = strlen(linestr_to);
			dc.TextOut(FillRect.left, FillRect.top, linestr_to, tlen);		
		}
		else if (x!=y)
		{
/*
......draw area before selection
*/
			if ((x>(charinx+start_inx))&&(x<(charinx+end_inx)))
			{
				len1 = x - charinx + 1;
				strncpy_s(linestr1, 1024, linestr, len1);
				linestr1[len1] = '\0';
				len1 = x - (charinx+start_inx);
				strncpy_s(linestr1,1024, &(linestr1[start_inx]),len1);
				linestr1[len1] = '\0';
//				szText = dc.GetTextExtent(linestr1);
				uw_convert_tabstr(linestr1, linestr_to, &tab_no, tab_value); 
				szText = dc.GetTextExtent(linestr_to);
				FillRect.top = pt1.y;
				FillRect.bottom = pt1.y + szText.cy;
				FillRect.left = pt1.x;
				FillRect.right = pt1.x + szText.cx;
				if (FillRect.bottom>rect.bottom) 
					break;
				if (FillRect.left>rect.right) 
					break;
				if ((FillRect.right<rect.left)||(FillRect.bottom<rect.top))
					break;
				if (FillRect.bottom>rect.bottom)
					FillRect.bottom = rect.bottom;
				if (FillRect.top<rect.top)
					FillRect.top = rect.top;
				if (FillRect.left<rect.left)
					FillRect.left = rect.left;
				if (FillRect.right>rect.right)
					FillRect.right = rect.right;
				if (FillRect.left<rect.left)
					FillRect.left = rect.left;
				if (FillRect.right>rect.right)
					FillRect.right = rect.right;
				dc.FillRect(FillRect, pNewBrush);

				if (len1==0) continue;			
//			tbint = 32;
//				dc.TabbedTextOut(FillRect.left, FillRect.top, linestr1, len1, tab_no, (LPINT) &tab_value[0], FillRect.left);
//				dc.TextOut(FillRect.left, FillRect.top, linestr1, len1);
				tlen = strlen(linestr_to);
				dc.TextOut(FillRect.left, FillRect.top, linestr_to, tlen);		
			}
/*
......draw area after selection
*/
			if ((y<(charinx+end_inx+1))&&(y>(charinx+start_inx+1)))
			{
				len1 = charinx+end_inx - y + 1;
				strncpy_s(linestr1,1024, &(linestr[y-charinx]), len1);
				linestr1[len1] = '\0';

//				szText = dc.GetTextExtent(linestr1);
				uw_convert_tabstr(linestr1, linestr_to, &tab_no, tab_value); 
				szText = dc.GetTextExtent(linestr_to);

				pt2 = PosFromChar(y);
				FillRect.top = pt2.y;
				FillRect.bottom = pt2.y + szText.cy;
				FillRect.left = pt2.x;
				FillRect.right = pt2.x + szText.cx;
				if (FillRect.bottom>rect.bottom) 
					break;
				if (FillRect.left>rect.right) 
					break;
				if ((FillRect.right<rect.left)||(FillRect.bottom<rect.top))
					break;
				if (FillRect.bottom>rect.bottom)
					FillRect.bottom = rect.bottom;
				if (FillRect.top<rect.top)
					FillRect.top = rect.top;
				if (FillRect.left<rect.left)
					FillRect.left = rect.left;
				if (FillRect.right>rect.right)
					FillRect.right = rect.right;
				dc.FillRect(FillRect, pNewBrush);

				if (len1==0) continue;			
//				dc.TextOut(FillRect.left, FillRect.top, linestr1, len1);
//				dc.DrawText(linestr1, len1, &FillRect, DT_LEFT|DT_SINGLELINE|DT_TABSTOP);
//				tbint = 32;
//				tabint = (LPINT) &tbint;
//				dc.TabbedTextOut(FillRect.left, FillRect.top, linestr1, len1, tab_no, (LPINT)&tab_value[0], FillRect.left);
				tlen = strlen(linestr_to);
				dc.TextOut(FillRect.left, FillRect.top, linestr_to, tlen);		
			}
		}
	}
	dc.SetBkMode(nBkMode);
	try
	{
		delete pNewBrush;
	}
	catch(...)
	{
		ASSERT(FALSE);
	}
	pNewBrush = NULL;
}

void CNCLeditctl::OnDestroy() 
{
	CEdit::OnDestroy();
}
	
/***********************************************************************
**
**   FUNCTION: ClearCurrentLine()
**
**       Clear the current text line
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLeditctl::ClearCurrentLine()
{
	char linestr[1024];
	int line, sel1, sel2;
	line = LineFromChar(m_start_char);
	sel1 = LineIndex(line);
/*
......somehow LineLength not working currently, so use GetLine
......to get the length.
	int len = LineLength(line);
*/
	int len = GetLine(line, linestr, 1024);
	sel2 = LineIndex(line) + len;
	SetSel(sel1, sel2, TRUE);
	ReplaceSel("");
}

/**********************************************************************
**    I_FUNCTION :  OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags)
**       command callback routine for key down
**    PARAMETERS   
**       INPUT  : 
**				nChar,      nRepCnt,    nFlags               
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLeditctl::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	static int started = 0;
	if (started)
		return;
	started = 1;
	int start_char, end_char, indx, len;
	char linestr[1024];
	GetSel(start_char, end_char); 

	int sav_start_line, sav_end_line;
	sav_start_line = m_start_line;
	sav_end_line = m_end_line;

	CEdit::OnKeyDown(nChar, nRepCnt, nFlags);

	int noerr, start, end, i, m_exe_line1, m_exe_line2;
	UM_int2 nchar;
	UM_int4 line_no;
	UM_f77_str linebuf;
	char  inbuf[NCL_MAX_COMLINE];

	UM_init_f77_str(linebuf, inbuf, NCL_MAX_COMLINE);

	if (nChar==VK_END)
		m_end = 1;
	if (!((nChar==VK_DOWN)||(nChar==VK_UP)||(nChar==VK_END)))
		m_end = 0;
	GetSel(m_start_char, m_end_char); 
	m_start_line = LineFromChar(m_start_char);
	m_end_line = LineFromChar(m_end_char);
/*
.....adjust position since the files may not fully loaded
*/
	if ((m_done_upload==0)&&(m_load_start>0))
	{
		m_start_line = m_start_line + m_load_start;
		m_end_line = m_end_line + m_load_start;
	}
	m_linenum_changed = 0;
	if ((m_start_line==m_end_line)&&(sav_start_line!=m_start_line))
	{
		m_linenum_changed = 1;
	}
	if ((nChar!=VK_RETURN)&&(sav_end_line<m_end_line))
	{
		if ((m_end_line<=m_total_line)&&(sav_end_line!=m_act_line))
			Redraw_current_lntext(sav_end_line-m_load_start);
	}
	else if  ((nChar!=VK_RETURN)&&(sav_end_line>m_end_line))
	{
		if ((m_end_line-m_load_start>=0)&&(sav_end_line!=m_act_line))
		{
			Redraw_current_lntext(sav_end_line-m_load_start);
		}
	}
	m_executed = 0;
	m_error = 0;
/*
......set this value to let the keyup function know if this key changed the text value
......it will set m_changed = 1 when OnChange function called
*/
	m_changed = 0;

	UM_int4 bline;
	char bbuf[1024];
	UM_int4 bnc;
	UM_int2 btype, bcmt;
	btype = 1;
	bcmt=1;

	if (nChar==VK_RETURN)
	{
		int insmod = 0;
/*
......we need saved the edited line first
*/
		SaveInsertLine();
/*
.....when key return is pressed, we execute the command on select lines
.....and will reload the PP. So set the flag here and do the execution
*/
		m_executed = 1;
		GetWindowText(m_whole_text);
		GetSel(start, end);
		m_exe_line1 = LineFromChar(start);
		m_exe_line2 = LineFromChar(end);
		char linestr[1024];
		int line;
		int exe = 0;
		for (i=m_exe_line1; i<=m_exe_line2; i++)
		{
			if ((m_continue_line!=-1)&&(m_continue_line!=i+1))
			{
				ncl_reset_cmdline();
				m_exe_start = -1;
				m_continue_line = -1;
			}			
			len = GetLine(i, linestr, 1023);
			linestr[len] = '\0';
			if ((m_done_upload==0)&&(m_load_start>0))
				line = i + m_load_start+1;
			else
				line = i+1;
			if (m_continue_line==-1)
				setnln(&line);
			else
			{
				setnln(&m_exe_start);
			}
/*
.....save the current execute line string
*/
			strcpy(m_exestr, linestr);
			if (len==0)
			{
/*
.....if it is empty line, we just need keep the active line and do nothing
*/
				m_continue_line = -1;
				m_error = 0;
				m_act_line = line-1;
				int ln = line+1;
				if ((ln>m_total_line+1)&&(insmode_val()!=2))
					ln = m_total_line+1;
				setnln(&ln);
			}
			else
			{
/*
......when we execute the command, use CONSOL mode
*/
				short idx, ival;
				if (insmode_val()==2)
				{
					idx = 25;
					ival = 0;
					setifl(&idx,&ival);
					idx = 37;
					ival = 1;
					setifl(&idx,&ival);
					insmod = 1;
				}
/*
.....if the execute line is the last time which is not count in the total line
.....we need +1 for m_total_line
*/
				if (i>=m_total_line)
					m_total_line = i+1;
/*
......the purpose here is init the line type for fortran execute
*/
				line_no = i+1;
				gtpsrc(&line_no, &nchar, UM_addr_of_f77_str(linebuf));
				noerr = ncl_cmd_key(linestr);
				if (insmod)
				{
/*
......reset back to INSERT mode
*/
					idx = 25;
					ival = 2;
					setifl(&idx,&ival);
					idx = 37;
					ival = 2;
					setifl(&idx,&ival);
				}
				if (noerr==0)
				{
					m_act_line = line-1;
					m_error = line;
					m_exe_start = -1;
				}
				else if (noerr==-1)
				{
					m_error = -1;
					if (m_continue_line==-1)
					{
						m_exe_start = line;
						m_continue_line = line+1;
						m_act_line = line;
					}
					else
					{
						m_continue_line++;
						m_act_line = m_continue_line - 1;
					}
				}
				else
				{
					m_exe_start = -1;
					m_continue_line = -1;
					m_error = 0;
					m_act_line = line;
/*
.....after execute the insert line
*/
					if (insmode_val()==2)
					{
						if ((line-1)==m_ncl_insert_ln)
							m_ncl_insert_ln++;
					}
				}
			}
			exe = 1;
		}
		if (exe)
			PostMessage(IDC_EXECUTE);
	}
	else
	{
		int line1, line2, dline, add_lns;		

		if (sav_end_line!=m_end_line)
		{
			if ((m_linenum_changed)&&(m_end))
			{
				SetSelection(m_start_char, m_end_char, FALSE, 1);
			}
			else
				SetSelection(m_start_char, m_end_char, FALSE, 2);
			goto done;
		}
/*
......in the end and down, load more
*/
		if (((nChar==VK_DOWN)||(nChar==VK_NEXT))&&(m_done_dnload==0))
		{
			No_paint = 1;
			line1 = GetFirstVisibleLine();
			len = HandleCommandLoad(0, add_lns);
			line2 = GetFirstVisibleLine();
			dline = line1 - line2;
			if (dline!=0)
			{
				LineScroll(dline);
				RedrawColorText();
			}
			SetSelection(m_start_char, m_end_char, FALSE);
			No_paint = 0;
/*
......post this message again the reset cursor position since before loading
......it is the end of text, so cursor will stay on the same line
......we need it down one line
*/
			PostMessage(WM_KEYDOWN, (WPARAM)nChar, MAKELPARAM(nRepCnt, nFlags));
		}
/*
......in the beginning and down, load more
*/
		if (((nChar==VK_UP)||(nChar==VK_PRIOR))&&(m_done_upload==0))
		{
			No_paint = 1;
			line1 = GetFirstVisibleLine();
			len = HandleCommandLoad(1, add_lns);
			line2 = GetFirstVisibleLine();
			dline = line1 - line2 + add_lns;
			if (dline!=0)
			{
				LineScroll(dline);
				RedrawColorText();
			}
			SetSelection(m_start_char+len, m_end_char+len, FALSE);
			No_paint = 0;
/*
......post this message again the reset cursor position since before loading
......it is the beginning of text, so cursor will stay on the same line
......we need it up one line
*/
			PostMessage(WM_KEYDOWN, (WPARAM)nChar, MAKELPARAM(nRepCnt, nFlags));
		}
		if ((m_done_upload==0)&&(m_load_start>0))
			m_start_line = m_start_line + m_load_start;
	}
done:;
	RedrawColorText();
	started = 0;
}

/**********************************************************************
**    I_FUNCTION :  OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags)
**       command callback routine for key up
**    PARAMETERS   
**       INPUT  : 
**				nChar,      nRepCnt,    nFlags               
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLeditctl::OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	CEdit::OnKeyUp(nChar, nRepCnt, nFlags);
	if (m_error)
		return;
	int test = 0;
	char inbuf[1024];
	int ioerr, nc;
	short itype, icmt=1;
/*
......if OnChanged called, it will set start/end char and line there, we don't need do anything
......if not we need remember the position
*/
	if (m_changed==0)
	{
		if (m_linenum_changed)		
		{
			if (UW_text_select==0)
				SetCursorAtLineEnd();
			else
				SelectCurrentLine();
			m_linenum_changed = 0;
		}
		GetSel(m_start_char, m_end_char); 
		m_start_line = LineFromChar(m_start_char);
		m_end_line = LineFromChar(m_end_char);
/*
.....adjust position since the files may not fully loaded
*/
		if ((m_done_upload==0)&&(m_load_start>0))
		{
			m_start_line = m_start_line + m_load_start;
			m_end_line = m_end_line + m_load_start;
		}
	}
	m_changed = 0;
	SaveEditedLine();

	char prompt[256];
	sprintf_s(prompt,256,"edit line: %d", m_end_line+1);
	uw_ntsav_plabel(prompt);
	uw_ntwrplabel(prompt);
	RedrawColorText();
/*
.....if it is not UP/DOWN key resave current pos
*/
	if (!((nChar==VK_DOWN)||(nChar==VK_UP)||(nChar==VK_END)))
		GetCurPos();
	if (insmode_val()==2)
		m_edit_line = m_end_line;
}

void CNCLeditctl::InvalidateColorText()
{
		RedrawWindow();
		UpdateWindow();
}

BOOL CNCLeditctl::PreTranslateMessage(MSG* pMsg) 
{
	ASSERT(pMsg);	
	return CEdit::PreTranslateMessage(pMsg);
}
int CNCLeditctl::HandleSpecialKey(MSG* pMsg)
{
	int reset_line, indx, len, start_char, end_char;
	char linestr[1024];
	GetSel(start_char, end_char); 
	if (pMsg->message == WM_KEYDOWN)
	{
		reset_line = 0;
		if ((start_char==end_char)&&((int)(pMsg->wParam)==VK_BACK))
/*
.....if it's selection delete all selection
.....here we only don't allow delete pass the current line
.....for back/delete key to delete one by one
*/
		{		
			indx = LineIndex(m_start_line);
			if (indx == start_char)
			{
				reset_line = 1;
			}
		}
		if ((start_char==end_char)&&((int)(pMsg->wParam)==VK_DELETE))
/*
......we don't allow delete pass the current line
*/
		{		
			indx = LineIndex(m_start_line);
			len = GetLine(m_start_line, linestr, 1024);
			if (start_char == indx + len)
			{
				reset_line = 2;
			}
		}
		if (reset_line!=0)
			return 1;
	}
	return 0;
}

void CNCLeditctl::OnUpdate() 
{
	InvalidateColorText();
}

BOOL CNCLeditctl::OnEraseBkgnd(CDC* pDC) 
{
	return CWnd::OnEraseBkgnd(pDC);
}
/**********************************************************************
**    I_FUNCTION :  SetFocusCallback
**       command callback routine for set focus of extended edit field.
**		save the edited text.
**    PARAMETERS   
**       INPUT  : 
**				none                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLeditctl::SetFocusCallback()
{
}
/**********************************************************************
**    I_FUNCTION :  LossFocusCallback
**       command callback routine for lose focus of extended edit field.
**		save the edited text.
**    PARAMETERS   
**       INPUT  : 
**				none                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLeditctl::LossFocusCallback()
{
/*
......save the last active line (the line cursor is on when focused on)
*/
	if (insmode_val()!=2)
		SaveEditedLine(1);
}
/*
......we will try to save the line when current active line(the line cursor is on when focused on)
......is changed.
*/

void CNCLeditctl::OnMaxText()
{
/*
.....should warn
*/
}
/***********************************************************************
c
c   FUNCTION: SelectCurrentLine()
c
c         Select the line
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLeditctl::SelectLines(int line)
{
	char linestr[1024];
	int sel1, sel2;
	sel1 = LineIndex(line);
/*
......somehow LineLength not working currently, so use GetLine
......to get the length.
	int len = LineLength(line);
*/
	int len = GetLine(line, linestr, 1024);
	sel2 = LineIndex(line) + len;
	SetSel(sel1, sel2, TRUE);
}
