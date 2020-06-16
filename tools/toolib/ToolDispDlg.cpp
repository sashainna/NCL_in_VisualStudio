/************************************************************************
c
c   FILE NAME: ToolDispDlg.cpp
c
c	 CONTAINS: 
c		Functions for all CToolDispDlg class 
c
c    COPYRIGHT 2007 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       ToolDispDlg.cpp , 25.5
c    DATE AND TIME OF LAST  MODIFICATION
c       01/18/16 , 08:27:18
c
c**********************************************************************
*/
#include "toolibstdafx.h"
#include <stdlib.h>
#include <stdio.h>
#include "toolib.h"
#include "ToolDispDlg.h"
#include "xenv1.h"
#include "toolibdata.h"
#include "toolchildvw.h"
#include "xfsys1.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" struct TL_toolhead_rec Tool_head;
extern "C" struct TL_tooldata_rec Tool_current_data;
extern "C" int tool_break_fname(char*,char*,char*,char*);
extern "C" int tool_get_dir(char*, char*);
extern "C" int tool_build_fname(char*,char*,char*, char*);
extern "C" int tool_open_cutprof(char*);
extern "C" int uu_toolmalloc_init();
extern "C" int uu_toolmalloc_term();
extern "C" int tool_strip_blanks (char*, int*);
extern "C" int ul_to_upper(char*);
extern "C" UU_LIST NCL_plib_tools;
extern "C" int ul_open_mod_file(char *user_dir, char *user_subdir, char *sys_dir, char *sys_subdir,
					char *infname, int wr_flag, FILE **);
extern "C" int ncl_next_cutprof(int *inxt, char *sprof, char *sclass);
extern "C" int ul_get_full_dir(char *dir1, char *dir2);
extern "C" int ul_build_full_dir(char *dir1, char *dir2, char *fullpath);
extern "C" int ux_get_flist(char *path, int farea_flag, char *ftype, char **listptr,
				int options);
extern "C" void uu_lsdel(char *l);
extern "C" int ux_nxt_file(char **listptr, char *filename, int options);
extern "C" int ux_get_base_fname(char *pathname, char *basename, int options);
extern "C" char * uu_malloc(int);
extern "C" void uu_free( char* );

struct S_tools_struc
{
	char label[21];
	char clas[21];
	UU_LIST pts;
};
/////////////////////////////////////////////////////////////////////////////
// CToolDispDlg dialog


/***********************************************************************
c
c   FUNCTION: CToolDispDlg()
c
c              Constructor of class CToolDispDlg
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CToolDispDlg::CToolDispDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CToolDispDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CToolDispDlg)
	m_class = _T("All");
	m_value1 = 0.0;
	m_value2 = 0.0;
	m_value3 = 0.0;
	m_value4 = 0.0;
	m_parm1 = "";
	m_parm2 = "";
	m_parm3 = "";
	m_parm4 = "";
	m_shade = _T("Off");
	m_symbol = _T("");
	NCL_plib_tools.item_size = 0;
	NCL_plib_tools.data = NULL;
	m_created = 0;
	//}}AFX_DATA_INIT
}
/***********************************************************************
c
c   FUNCTION: SetParent(CWnd* parent, int flag)
c
c              Set window's parent and window type
c
c   INPUT:  parent: parent to be set
c			flag: type tyo be set: 1: Cutter Display window
c									2: Shank Display window
c									3: Holder Display window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CToolDispDlg::SetParent(CWnd* parent, int flag)
{
	m_parent = parent;
	m_type = flag;
}

/***********************************************************************
c
c   FUNCTION: DoDataExchange(CDataExchange* pDX)
c
c         Called by the framework to exchange and validate dialog data.
c
c   INPUT:  pDX   A pointer to a CDataExchange object.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CToolDispDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CToolDispDlg)
	DDX_CBString(pDX, IDC_CLASS, m_class);
	DDX_Text(pDX, IDC_EDIT_DEF1, m_value1);
	DDX_Text(pDX, IDC_EDIT_DEF2, m_value2);
	DDX_Text(pDX, IDC_EDIT_DEF3, m_value3);
	DDX_Text(pDX, IDC_EDIT_DEF4, m_value4);
	DDX_Text(pDX, IDC_EDIT_PARM1, m_parm1);
	DDX_Text(pDX, IDC_EDIT_PARM2, m_parm2);
	DDX_Text(pDX, IDC_EDIT_PARM3, m_parm3);
	DDX_Text(pDX, IDC_EDIT_PARM4, m_parm4);
	DDX_CBString(pDX, IDC_SHADE_CHC, m_shade);
	DDX_Text(pDX, IDC_SYMBOL_NAME, m_symbol);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CToolDispDlg, CDialog)
	//{{AFX_MSG_MAP(CToolDispDlg)
	ON_CBN_SELCHANGE(IDC_CLASS, OnSelchangeClass)
	ON_CBN_SELCHANGE(IDC_SHADE_CHC, Onshadechange)
	ON_LBN_SELCHANGE(IDC_LIST_PROF, OnSelchangeListProf)
	ON_LBN_SELCHANGE(IDC_LIST_SYMBOLS, OnSelchangeListSymbols)
	ON_EN_CHANGE (IDC_SYMBOL_NAME, OnSymbolChanged)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CToolDispDlg message handlers

/***********************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c         This member function is called in response to 
c		the WM_INITDIALOG message. This message is sent to 
c		the dialog box during the Create, CreateIndirect, 
c		or DoModal calls, which occur immediately before 
c		the dialog box is displayed. 
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CToolDispDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	int i, j, nprofs, same;
	UX_pathname dir,file,ext,filename;
	FILE *fd;

	uu_toolmalloc_init();

	load_tool_prof();

	if (m_type==1)
	{
		m_symbol = Tool_current_data.symbol;
		((CComboBox*)GetDlgItem(IDC_SHADE_CHC))->SetCurSel(Tool_current_data.shade);
	}
	else if (m_type==2)
	{
		m_symbol = Tool_current_data.symshk;
		((CComboBox*)GetDlgItem(IDC_SHADE_CHC))->SetCurSel(Tool_current_data.sshade);
	}
	else
	{
		m_symbol = Tool_current_data.symhld;
		((CComboBox*)GetDlgItem(IDC_SHADE_CHC))->SetCurSel(Tool_current_data.hshade);
	}
	GetDlgItem(IDC_SYMBOL_NAME)->SetWindowText(m_symbol);
	if (m_type==1)
	{
		SetWindowText(_T("Cutter Display"));
	}
	else if (m_type==2)
	{
		SetWindowText(_T("Shank Display"));
	}
	else if (m_type==3)
	{
		SetWindowText(_T("Holder Display"));
	}
/*
......adjust, enable/disable field, set prompt label
*/
	adjust_data_field();
	load_tool_symbols();
	m_created = 1;

	// TODO: Add extra initialization here	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
/***********************************************************************
c
c   FUNCTION: adjust_data_field()
c
c         adjust, enable/disable window field, set prompt label
c 
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CToolDispDlg::adjust_data_field()
{
	int icfl[10];
	int symfl[3];
	char *temp, symbol[81], label[21], classn[21];
	int j, itrv[4], nc;
	CString spr[4];
	int nprofs, inxt;

	UpdateData(FALSE);  

	nprofs = UU_LIST_LENGTH(&NCL_plib_tools);

	nc = m_symbol.GetLength();
	if (nc>0)
	{
#ifdef _UNICODE	
	WCHAR *wtemp= m_symbol.GetBuffer(nc);
	wcstombs(symbol, wtemp, nc);
	symbol[nc] = '\0';
#else
	temp = m_symbol.GetBuffer(81);
	strcpy(symbol, temp);
#endif
	}
	else
		symbol[0] = '\0';
	if (m_type==1)
	{
		if (nc != 0)
		{
			icfl[0] = 3;
			ul_to_upper(symbol);
			inxt = 0;
			for (j=0;j<nprofs;j++)
			{
				ncl_next_cutprof(&inxt, label, classn);
				if (strcmp(symbol, label) == 0)
				{
					icfl[0] = 4;
					break;
				}
			}
		}
		else
		{
			icfl[0] = Tool_current_data.fpseudo;
		}
		GetDlgItem(IDC_STATIC5)->EnableWindow(0);
		GetDlgItem(IDC_CLASHES_CHC)->EnableWindow(0);
	}
	if (m_type==2)
	{
		if (nc != 0)
		{
			icfl[4] = 3;
			ul_to_upper(symbol);
			inxt = 0;
			for (j=0;j<nprofs;j++)
			{
				ncl_next_cutprof(&inxt, label, classn);
				if (strcmp(symbol, label) == 0)
				{
					icfl[4] = 4;
					break;
				}
			}
		}
		else
		{
			icfl[4] = 1;
		}
		GetDlgItem(IDC_STATIC5)->EnableWindow(1);
		GetDlgItem(IDC_CLASHES_CHC)->EnableWindow(1);
		if (Tool_current_data.fshank==1)
			((CComboBox*)GetDlgItem(IDC_CLASHES_CHC))->SetCurSel(0);
		else
			((CComboBox*)GetDlgItem(IDC_CLASHES_CHC))->SetCurSel(1);
	}
	if (m_type==3)
	{
		if (nc != 0)
		{
			icfl[5] = 3;
			ul_to_upper(symbol);
			inxt = 0;
			for (j=0;j<nprofs;j++)
			{
				ncl_next_cutprof(&inxt, label, classn);
				if (strcmp(symbol, label) == 0)
				{
					icfl[5] = 4;
					break;
				}
			}
		}
		else
		{
			icfl[5] = 1;
		}
		GetDlgItem(IDC_STATIC5)->EnableWindow(0);
		GetDlgItem(IDC_CLASHES_CHC)->EnableWindow(0);
	}
	if (m_type==1)
	{
/*
........Mill cutter / Lathe Parameters
*/
		if (Tool_current_data.ctype < 10 || icfl[0] < 2)
		{
			itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_FALSE;
			spr[0] = spr[1] = spr[2] = spr[3] = _T("");
		}
/*
........Lathe symbol cutter
*/
		else
		{
			itrv[0] = itrv[1] = UU_TRUE;
			itrv[2] = itrv[3] = UU_FALSE;
			spr[0] = _T("Z-Attach:");
			spr[1] = _T("Z-Depth:");
			spr[2] = spr[3] = _T("");
		}
	}
/*
.....Shank / Holder
*/
	else
	{
/*
........Mill
*/
		if (Tool_current_data.ctype < 10)
		{
/*
...........Parameters
*/
			if (((icfl[4] < 2) && (m_type==2))
				|| ((icfl[5] < 2) && (m_type==3)))
			{
				itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_TRUE;
				spr[0] = _T("Diameter:");
				spr[1] = _T("Height:");
				spr[2] = _T("Angle:");
				spr[3] = _T("Z-Attach:");
			}
/*
...........Symbol
*/
			else
			{
				itrv[0] = UU_TRUE;
				itrv[1] = itrv[2] = itrv[3] = UU_FALSE;
				spr[0] = _T("Z-Attach:");
				spr[1] = spr[2] = spr[3] = _T("");
			}
		}
/*
........Lathe
*/
		else
		{
/*
...........Parameters
*/
			if (((icfl[4] < 2) && (m_type==2))
				|| ((icfl[5] < 2) && (m_type==3)))
			{
				itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_TRUE;
				spr[0] = _T("Width:");
				spr[1] = _T("Length:");
				spr[2] = _T("Z-Depth:");
				spr[3] = _T("Y-Offset:");
			}
/*
...........Symbol
*/
			else
			{
				itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_TRUE;
				spr[0] = _T("X-Offset:");
				spr[1] =_T("Y-Offset:");
				spr[2] = _T("Z-Attach:");
				spr[3] = _T("Z-Depth:");
			}
		}
	}
	GetDlgItem(IDC_STATIC1)->SetWindowText(spr[0]);
	GetDlgItem(IDC_STATIC2)->SetWindowText(spr[1]);
	GetDlgItem(IDC_STATIC3)->SetWindowText(spr[2]);
	GetDlgItem(IDC_STATIC4)->SetWindowText(spr[3]);
	GetDlgItem(IDC_EDIT_DEF1)->EnableWindow(itrv[0]);
	GetDlgItem(IDC_EDIT_DEF2)->EnableWindow(itrv[1]);
	GetDlgItem(IDC_EDIT_DEF3)->EnableWindow(itrv[2]);
	GetDlgItem(IDC_EDIT_DEF4)->EnableWindow(itrv[3]);
	GetDlgItem(IDC_EDIT_PARM1)->EnableWindow(itrv[0]);
	GetDlgItem(IDC_EDIT_PARM2)->EnableWindow(itrv[1]);
	GetDlgItem(IDC_EDIT_PARM3)->EnableWindow(itrv[2]);
	GetDlgItem(IDC_EDIT_PARM4)->EnableWindow(itrv[3]);
}
/***********************************************************************
c
c   SUBROUTINE:  OnSymbolChanged
c
c   FUNCTION:  This function called when user changed text in "Symbol" field
c				
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CToolDispDlg::OnSymbolChanged()
{
	CEdit *cwin = (CEdit*)GetDlgItem(IDC_SYMBOL_NAME);
	CString sText;
	cwin->GetWindowText(sText);

	m_symbol.TrimLeft(' ');
	sText.TrimLeft(' ');
	int len1 = m_symbol.GetLength();
	int len2 = sText.GetLength();

	if (((len1==0) && (len2==0)) || ((len1>0) && (len2>0)))
/*
.....we need do nothing, just return
*/
		return;
/*
.....else we need reset data field prompt and disable/enable data fields
*/
	sText.TrimRight(' ');
	m_symbol = sText;
	adjust_data_field();
}

/***********************************************************************
c
c   FUNCTION: CToolDispDlg::OnOK() 
c
c         This is callback function for OK button.
c			This function will accept window input
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/
void CToolDispDlg::OnOK() 
{
	// TODO: Add extra validation here
	
	CDialog::OnOK();
	((CChildView2*)m_parent)->SetSymbol(m_symbol, m_type);
/*
.....update main window's shade field if it is "Cutter display form"
*/
	CComboBox* cmbbx = (CComboBox*)GetDlgItem(IDC_SHADE_CHC);
	int pos = cmbbx->GetCurSel();
	if (m_type==1)
		((CChildView2*)m_parent)->SetShade(pos);

	int nc,chc;
	char tempstr[80];
	char parmstr1[80], parmstr2[80], parmstr3[80], parmstr4[80];
	int nc1 = m_parm1.GetLength();
	int nc2 = m_parm2.GetLength();
	int nc3 = m_parm3.GetLength();
	int nc4 = m_parm4.GetLength();
#ifdef _UNICODE	
	WCHAR *wparm;
	if (nc1>0)
	{
		wparm = m_parm1.GetBuffer(nc1);
		wcstombs(parmstr1, wparm, nc1);
		parmstr1[nc1] = '\0';
	}
	else
		parmstr1[0] = '\0';
	if (nc2>0)
	{
		wparm = m_parm2.GetBuffer(nc2);
		wcstombs(parmstr2, wparm, nc2);
		parmstr2[nc2] = '\0';
	}
	else
		parmstr2[0] = '\0';
	if (nc3>0)
	{
		wparm = m_parm3.GetBuffer(nc3);
		wcstombs(parmstr3, wparm, nc3);
		parmstr3[nc3] = '\0';
	}
	else
		parmstr3[0] = '\0';
	if (nc1>0)
	{
		wparm = m_parm4.GetBuffer(nc4);
		wcstombs(parmstr4, wparm, nc4);
		parmstr4[nc4] = '\0';
	}
	else
		parmstr4[0] = '\0';
#else
	char *parm;
	parm = m_parm1.GetBuffer(nc1);
	strcpy(parmstr1, parm);
	parm = m_parm2.GetBuffer(nc2);
	strcpy(parmstr2, parm);
	parm = m_parm3.GetBuffer(nc3);
	strcpy(parmstr3, parm);
	parm = m_parm4.GetBuffer(nc4);
	strcpy(parmstr4, parm);
#endif
	if (m_type==1)
	{
		Tool_current_data.catt[0] = m_value1;
		Tool_current_data.catt[1] = m_value2;
		Tool_current_data.catt[2] = m_value3;
		Tool_current_data.catt[3] = m_value4;
		strcpy (tempstr, parmstr1);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.yparms[0] = atoi(tempstr);
		else
			Tool_current_data.yparms[0] = 0;

		strcpy (tempstr, parmstr2);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.yparms[1] = atoi(tempstr);
		else
			Tool_current_data.yparms[1] = 0;

		strcpy (tempstr, parmstr3);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.yparms[2] = atoi(tempstr);
		else
			Tool_current_data.yparms[2] = 0;

		strcpy (tempstr, parmstr4);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.yparms[3] = atoi(tempstr);
		else
			Tool_current_data.yparms[3] = 0;
		if (m_shade=="Default")
			Tool_current_data.shade = 0;
		else if (m_shade=="On")
			Tool_current_data.shade = 1;
		else if (m_shade=="Off")
			Tool_current_data.shade = 2;
		Tool_current_data.fshank = 0;
		Tool_current_data.fholder = 0;
	}
	else if (m_type==2)
	{
		Tool_current_data.satt[0] = m_value1;
		Tool_current_data.satt[1] = m_value2;
		Tool_current_data.satt[2] = m_value3;
		Tool_current_data.satt[3] = m_value4;
		strcpy (tempstr, parmstr1);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.sparms[0] = atoi(tempstr);
		else
			Tool_current_data.sparms[0] = 0;

		strcpy (tempstr, parmstr2);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.sparms[1] = atoi(tempstr);
		else
			Tool_current_data.sparms[1] = 0;

		strcpy (tempstr, parmstr3);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.sparms[2] = atoi(tempstr);
		else
			Tool_current_data.sparms[2] = 0;

		strcpy (tempstr, parmstr4);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.sparms[3] = atoi(tempstr);
		else
			Tool_current_data.sparms[3] = 0;
		if (m_shade=="Default")
			Tool_current_data.sshade = 0;
		else if (m_shade=="On")
			Tool_current_data.sshade = 1;
		else if (m_shade=="Off")
			Tool_current_data.sshade = 2;			
		chc = ((CComboBox*)GetDlgItem(IDC_CLASHES_CHC))->GetCurSel();
		if (chc==0)
			Tool_current_data.fshank = 1;
		else
		{
			Tool_current_data.fshank = 2;
		}
	}
	else if (m_type==3)
	{
		Tool_current_data.hatt[0] = m_value1;
		Tool_current_data.hatt[1] = m_value2;
		Tool_current_data.hatt[2] = m_value3;
		Tool_current_data.hatt[3] = m_value4;
		strcpy (tempstr, parmstr1);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.hparms[0] = atoi(tempstr);
		else
			Tool_current_data.hparms[0] = 0;

		strcpy (tempstr, parmstr2);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.hparms[1] = atoi(tempstr);
		else
			Tool_current_data.hparms[1] = 0;

		strcpy (tempstr, parmstr3);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.hparms[2] = atoi(tempstr);
		else
			Tool_current_data.hparms[2] = 0;

		strcpy (tempstr, parmstr4);
		nc = 20;
		tool_strip_blanks (tempstr,&nc);
		if (tempstr[0]!=0)
			Tool_current_data.hparms[3] = atoi(tempstr);
		else
			Tool_current_data.hparms[3] = 0;
		if (m_shade=="Default")
			Tool_current_data.hshade = 0;
		else if (m_shade=="On")
			Tool_current_data.hshade = 1;
		else if (m_shade=="Off")
			Tool_current_data.hshade = 2;
		Tool_current_data.fshank = 0;
		Tool_current_data.fholder = 1;
	}

	DestroyWindow();
	uu_toolmalloc_term();
	m_created = 0;
}
/***********************************************************************
c
c   FUNCTION: CToolDispDlg::OnCancel() 
c
c         This is callback function for Cancel button.
c			This function will cancel user input
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/
void CToolDispDlg::OnCancel() 
{
	// TODO: Add extra validation here
	
	CDialog::OnCancel();
	DestroyWindow();
	uu_toolmalloc_term();
	m_created = 0;
}

/***********************************************************************
c
c   FUNCTION: CToolDispDlg::Onshadechange() 
c
c         This is callback function for 'Shade' choice change
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/
void CToolDispDlg::Onshadechange() 
{
	CComboBox* cmbbx = (CComboBox*)GetDlgItem(IDC_SHADE_CHC);
	int pos = cmbbx->GetCurSel();
}

/***********************************************************************
c
c   FUNCTION: SetShade(int pos)
c
c         This is function set position for 'Shade' choice button
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/
void CToolDispDlg::SetShade(int pos)
{
	((CComboBox*)GetDlgItem(IDC_SHADE_CHC))->SetCurSel(pos);
}

/***********************************************************************
c
c   FUNCTION: CToolDispDlg::OnSelchangeClass() 
c
c         This is callback function for 'Class' choice change
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/
void CToolDispDlg::OnSelchangeClass() 
{
	char label[21], classn[21];
	CString clastr;
	CComboBox* cmbbx = (CComboBox*)GetDlgItem(IDC_CLASS);
	int i, cont, nprofs, pos = cmbbx->GetCurSel();

	if (NCL_plib_tools.data==NULL) return;

	cmbbx->GetLBText(pos, clastr);
	nprofs = UU_LIST_LENGTH(&NCL_plib_tools);

	CListBox *listbx = ((CListBox*)GetDlgItem(IDC_LIST_PROF));
	cont = listbx->GetCount();
	for (i = 0; i<cont; i++)
	{
		listbx->DeleteString(0);
	}
#ifdef _UNICODE	
	WCHAR wlabel[21];
#endif
	int len, inxt   = 0;
	for (i=0;i<nprofs;i++)
	{
		ncl_next_cutprof(&inxt, label, classn);
		if ((classn==clastr) || (clastr=="All"))
		{
#ifdef _UNICODE	
			len = MultiByteToWideChar(CP_ACP, 0, label, -1, 
							wlabel, 21);
			listbx->AddString(wlabel);
#else
			listbx->AddString(label);
#endif
		}
	}
	if (nprofs==0)
		listbx->AddString(_T(" "));
}

/***********************************************************************
c
c   FUNCTION: CToolDispDlg::OnSelchangeListProf() 
c
c         This is callback function for selection change of profile list
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/
void CToolDispDlg::OnSelchangeListProf() 
{
	CString namstr;
	CListBox* listbx = (CListBox*)GetDlgItem(IDC_LIST_PROF);
	int pos = listbx->GetCurSel();

	if (NCL_plib_tools.data==NULL) return;

	listbx->GetText(pos, namstr);
	CEdit *edt = ((CEdit*)GetDlgItem(IDC_SYMBOL_NAME));
	edt->SetWindowText(namstr);
/*
.....update main window's symbol/holder/shank text field
*/
	((CChildView2*)m_parent)->SetSymbol(namstr, m_type);
}
/***********************************************************************
c
c   FUNCTION: CToolDispDlg::OnSelchangeListSymbols() 
c
c         This is callback function for selection change of symbols list
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/
void CToolDispDlg::OnSelchangeListSymbols() 
{
	CString namstr;
	CListBox* listbx = (CListBox*)GetDlgItem(IDC_LIST_SYMBOLS);
	int pos = listbx->GetCurSel();

	listbx->GetText(pos, namstr);
	CEdit *edt = ((CEdit*)GetDlgItem(IDC_SYMBOL_NAME));
	edt->SetWindowText(namstr);
/*
.....update main window's symbol/holder/shank text field
*/
	((CChildView2*)m_parent)->SetSymbol(namstr, m_type);
}

void CToolDispDlg::load_tool_prof()
{
	int i, j, nprofs, same;
	UX_pathname dir,file,ext,filename;
	FILE *fd;

	strcpy(filename, Tool_head.proflib);
	if (Tool_head.proflib[0]!='\0')
	{
/*
.....check if filename exist
*/
		fd = fopen (filename,"r");
		if (fd != 0)
			fclose (fd);
		else
		{	
			tool_break_fname(filename,dir,file,ext);
			FILE *fptr;
			if (ext[0]=='\0')
				strcat(file, ".lib");
			if (dir[0]=='\0')
			{
				strcpy(dir, "NCL_TOOL");
				strcat (file, ".");
				strcat (file, ext);
			}
			int status = ul_open_mod_file(NULL, NULL, dir,
							NULL, file, 2,  &fptr);
			if (fptr!=NULL)
				fclose(fptr);
			strcpy(filename, file);
		}
		tool_open_cutprof(filename);
	}

	CListBox *listbx = ((CListBox*)GetDlgItem(IDC_LIST_PROF));
	CComboBox* cmbbx = ((CComboBox*)GetDlgItem(IDC_CLASS));
	CString tmpstr;
	if (NCL_plib_tools.data!=NULL)
	{
		nprofs = UU_LIST_LENGTH(&NCL_plib_tools);
		int cont = listbx->GetCount();
		for (i = 0; i<cont; i++)
		{
			listbx->DeleteString(0);
		}
		int len,len2;
		char label[21], classn[21], **classn_array;
#ifdef _UNICODE	
		WCHAR wlabel[21];
		WCHAR wclassn[21];
#endif
		int inxt   = 0;
		classn_array = (char **) uu_malloc(nprofs*sizeof(char *));
		for (i=0;i<nprofs;i++)
		{
			ncl_next_cutprof(&inxt, label, classn);
#ifdef _UNICODE	
			len2 = strlen (label) + 1;
			len2 = MultiByteToWideChar(CP_ACP, 0, label, -1, 
							wlabel, 21);
			listbx->AddString(wlabel);
#else
			listbx->AddString(label);
#endif
			len = strlen(classn);
			classn_array[i] = (char *) uu_malloc(len*sizeof(char));
			strcpy(classn_array[i], classn);
		}
		if (nprofs==0)
			listbx->AddString(_T(" "));

		cmbbx->AddString(_T("All"));
		for (i=0;i<nprofs;i++)
		{
			same = 0;
			for (j=0; j<i;j++)
			{
				if ((strcmp(classn_array[i], classn_array[j])==0)
					|| (strcmp(classn_array[i], "All")==0))
				{
					same = 1;
					break;
				}
			}
			if (same==1) continue;
#ifdef _UNICODE	
			tmpstr = classn_array[i];
#else
			tmpstr = classn_array[i];
#endif
			cmbbx->AddString(tmpstr);
		}
	}
	else
	{
		nprofs = 0;
		listbx->AddString(_T(" "));
		cmbbx->AddString(_T("All"));
	}
	cmbbx->SetCurSel(0);
}

void CToolDispDlg::load_tool_symbols()
{
	int ispath, len, local, i, j, status;
	UX_pathname dir,symdir, fullpath, dir2, filename;
	char basename[80];
	char *listhead, *list = 0;

	CListBox *listbx = ((CListBox*)GetDlgItem(IDC_LIST_SYMBOLS));
	int cont = listbx->GetCount();
	for (i = 0; i<cont; i++)
	{
		listbx->DeleteString(0);
	}
	strcpy(symdir, Tool_head.symlib);
	local = 0;
	if (Tool_head.symlib[0]!='\0')
	{
/*
.....check if symdir is existing exist path
*/
		len = strlen(symdir);
		if (len<=0) 
			return;
		if ((len>=2)&&(symdir[len-2]=='_')&&(symdir[len-1]=='S'))
		{
			ispath = tool_get_dir(symdir,fullpath);
		}
		else
		{
			strcat (symdir, "_S");
			ispath = tool_get_dir(symdir,fullpath);
		}
		if (ispath==0)
		{
/*
.....symlib could be subpath of local/system lib path
*/
			ul_get_full_dir("UB_SYS_M_SYMDIR", dir2);
			ul_build_full_dir(dir2, symdir, fullpath);
		}
		if (ux_get_flist(fullpath, UX_NFAREA, "sy", &list,
				(UX_PRTERRS|UX_NCHK)) != UU_SUCCESS)
		{
			if (ispath==0)
				goto failed;
			ul_get_full_dir(".", dir2);
			ul_build_full_dir(dir2, symdir, fullpath);
			if (ux_get_flist(fullpath, UX_NFAREA, "sy", &list,
					(UX_PRTERRS|UX_NCHK)) != UU_SUCCESS)
				goto failed;
			local = 1;
		}
list:;
		listhead = list;
		if (list == UU_NULL)	/* the archive contains no files to load */
			goto done;
#ifdef _UNICODE	
		WCHAR *wlabel = new WCHAR[81];
#endif
		while ( (status = ux_nxt_file(&list, filename, UX_PRTERRS)) == UU_SUCCESS)
		{
			if (ux_get_base_fname(filename, basename, (UX_PRTERRS|UX_NCHK)) 
				== UX_FAILURE) goto failed;
#ifdef _UNICODE	
			len = strlen (basename) + 1;
			MultiByteToWideChar(CP_ACP, 0, basename, -1, 
							wlabel, len);
			listbx->AddString(wlabel);
#else
			listbx->AddString(basename);
#endif
		}
		if (status == UX_FAILURE)
			goto failed;
		else
		{
			status = UU_SUCCESS;
			uu_lsdel(listhead);
		}
		if ((local==0)&&(ispath==0))
		{
/*
.....added symbol from local 
*/
			ul_get_full_dir(".", dir2);
			ul_build_full_dir(dir2, symdir, fullpath);
			if (ux_get_flist(fullpath, UX_NFAREA, "sy", &list,
					(UX_PRTERRS|UX_NCHK)) != UU_SUCCESS)
				goto failed;
			local = 1;
			goto list;
		}
	}
failed: status = UX_FAILURE;
done:;
	return;
}
/*
	DDX_CBString(pDX, IDC_CLASS, m_class);
	DDX_Text(pDX, IDC_EDIT_DEF1, m_value1);
	DDX_Text(pDX, IDC_EDIT_DEF2, m_value2);
	DDX_Text(pDX, IDC_EDIT_DEF3, m_value3);
	DDX_Text(pDX, IDC_EDIT_DEF4, m_value4);
	DDX_Text(pDX, IDC_EDIT_PARM1, m_parm1);
	DDX_Text(pDX, IDC_EDIT_PARM2, m_parm2);
	DDX_Text(pDX, IDC_EDIT_PARM3, m_parm3);
	DDX_Text(pDX, IDC_EDIT_PARM4, m_parm4);
	DDX_CBString(pDX, IDC_SHADE_CHC, m_shade);
	DDX_Text(pDX, IDC_SYMBOL_NAME, m_symbol);
*/
