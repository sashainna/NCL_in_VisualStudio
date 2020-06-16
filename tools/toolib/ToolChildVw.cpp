/************************************************************************
c
c   FILE NAME: ToolChildView.cpp
c
c	 CONTAINS: 
c		Functions for all the child view class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c      ToolChildVw.cpp , 26.2
c    DATE AND TIME OF LAST  MODIFICATION
c      04/16/18 , 15:41:18
c
c**********************************************************************
*/
#include "toolibstdafx.h"
#include <math.h>
#include "toolib.h"
#include "MainFrm.h"
#include "toolibDoc.h"
#include "toolibcc.h"
#include "ComDlg.h"
#include "CutDlg.h"
#include "xenv1.h"
#include "toolibdata.h"
#include "toolchildvw.h"

#define HIMETRIC_INCH	2540
#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

static double PI = 3.14159265358979323846;
static CChildView2 *CView2;
static int Scuttype;
static int toolno_click = 0;
static int type_click = 0;
static int descript_click = 0;
static int diam_click = 0;
static int rad_click = 0;
static int hgt_click = 0;
static int angle_click = 0;

extern "C" int Tool_font_size1, Tool_font_size2, Tool_font_size3;
extern "C" char Tool_font_name1[80], Tool_font_name2[80], Tool_font_name3[80]; 
extern "C" void ChangeDialogFont(CWnd* pWnd, CFont* pFont);
extern CMainFrame *TOOL_main_frame;
extern "C" struct TL_tooldata_rec Tool_current_data;
extern "C" double Current_sel_tool;
extern "C" void tool_get_toolno(int*);
extern "C" int ncl_get_tooldata(double, struct TL_tooldata_rec *);
extern "C" int ncl_idx_tooldata(int, double*, int*, char*);
extern "C" int ncl_sprintf(char *strng, double*value, int num);
extern "C" char * uu_malloc(int);
extern "C" void uu_free( char* );
extern "C" int tool_build_fname(char*,char*,char*, char*);
extern "C" int ncl_get_tool_tlist(UD_TLIST *tlist, int filter_type);
extern "C" void ud_tlist_free_idata(UD_ITEMDATA *data);
extern "C" char **ncl_get_tool_type(int *number, char *addstr);
extern "C" struct TL_toolhead_rec Tool_head;
extern "C" int ud_free_tlist(UD_TLIST *formlist);
extern "C" int tool_mfmsg_box(CWnd* parent, char *title, char *msg, int flag);
extern "C" int ul_open_mod_file(char *user_dir, char *user_subdir, char *sys_dir, char *sys_subdir,
					char *infname, int wr_flag, FILE **);
extern "C" void tool_get_filename(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag,
	char *paths, char *path_des);

HBITMAP uw_get_bitmap(LPSTR fname, HDC hDC,int flag);
static char S_typestr_list[400][40];
static int S_typestr_num = 0;
static int 	S_select_update = 1;

/*********************************************************************
**   I_FUNCTION: SortFunc()
**      Sort the list on Select toollist form in partical order
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int CALLBACK SortFunc(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	UD_ITEMDATA* pData1 = (UD_ITEMDATA*)lParam1;
	UD_ITEMDATA* pData2 = (UD_ITEMDATA*)lParam2;

	int nRetVal;
	UU_REAL val1, val2;

	nRetVal = 0;
/*
......"toolno" "type" "Description"
*/
	switch(lParamSort)
	{
	case 0:
/*
.....by value
*/
		val1 = atof (pData1->data_items[0]);
		val2 = atof (pData2->data_items[0]);
		if (val1>val2)
			nRetVal = 1;
		else if (val1==val2)
			nRetVal = 0;
		else
			nRetVal = -1;
		break;
	case 1:	
		nRetVal = strcmp(pData1->data_items[1],
                                 pData2->data_items[1]);
		break;
	case 2:
		nRetVal = strcmp(pData1->data_items[2],
                                 pData2->data_items[2]);
		break;
	case 3:	
		nRetVal = strcmp(pData1->data_items[3],
                                 pData2->data_items[3]);
		break;
	case 4:
		nRetVal = strcmp(pData1->data_items[4],
                                 pData2->data_items[4]);
		break;
	case 5:	
		nRetVal = strcmp(pData1->data_items[5],
                                 pData2->data_items[5]);
		break;
	case 6:
		nRetVal = strcmp(pData1->data_items[6],
                                 pData2->data_items[6]);
		break;
	default:
		break;
	}
	return nRetVal;
}

/*********************************************************************
**   I_FUNCTION: SortFunc2()
**      Sort the list on Select Scalar form in partical order
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int CALLBACK SortFunc2(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	UD_ITEMDATA* pData1 = (UD_ITEMDATA*)lParam1;
	UD_ITEMDATA* pData2 = (UD_ITEMDATA*)lParam2;

	int nRetVal;
	UU_REAL val1, val2;

	nRetVal = 0;
/*
......"toolno" "type" "Description"
*/
	switch(lParamSort)
	{
	case 0:
/*
.....by value
*/
		val1 = atof (pData1->data_items[0]);
		val2 = atof (pData2->data_items[0]);
		if (val1<val2)
			nRetVal = 1;
		else if (val1==val2)
			nRetVal = 0;
		else
			nRetVal = -1;
		break;
	case 1:	
		nRetVal = -strcmp(pData1->data_items[1],
                                 pData2->data_items[1]);
		break;
	case 2:
		nRetVal = -strcmp(pData1->data_items[2],
                                 pData2->data_items[2]);
		break;
	case 3:	
		nRetVal = -strcmp(pData1->data_items[3],
                                 pData2->data_items[3]);
		break;
	case 4:
		nRetVal = -strcmp(pData1->data_items[4],
                                 pData2->data_items[4]);
		break;
	case 5:	
		nRetVal = -strcmp(pData1->data_items[5],
                                 pData2->data_items[5]);
		break;
	case 6:
		nRetVal = -strcmp(pData1->data_items[6],
                                 pData2->data_items[6]);
		break;
	default:
		break;
	}
	return nRetVal;
}

static int S_add_typestr_list(char* newstr, int flag=1)
{
	int i;
	char instr[81];
/*
.....make sure the input string can't change
*/
	strcpy(instr, newstr);
	for (i=0; i<S_typestr_num;i++)
	{
		if (strcmp(instr, S_typestr_list[i])==0)
			return i;
	}
	strcpy(S_typestr_list[i], instr);
	S_typestr_num++;
	if (flag==0)
		return S_typestr_num-1;
	char **utype;
	if (S_typestr_num>15)
	{
		utype = (char **)uu_malloc ((S_typestr_num-15)*sizeof (char*));
		if (Tool_head.utype_no>0)
		{
			for (i=0; i<Tool_head.utype_no; i++)
			{
				utype[i] = (char*)uu_malloc(81*sizeof(char));
				strcpy(utype[i], Tool_head.utype[i]);
				uu_free(Tool_head.utype[i]);
			}
			uu_free((char*)(Tool_head.utype));
		}
		i = Tool_head.utype_no;
		if (i<0) i = 0;
		utype[i] = (char*)uu_malloc(81*sizeof(char));
		strcpy(utype[i], instr);	
		Tool_head.utype = utype;
		Tool_head.utype_no = S_typestr_num - 15;
		return S_typestr_num-1;
	}
	return -1;
}

static void S_reset_typestr_list()
{
	S_typestr_num = 0;
}

/////////////////////////////////////////////////////////////////////////////
// CChildView1

IMPLEMENT_DYNCREATE(CChildView1, CFormView)

/***********************************************************************
c
c   FUNCTION: CChildView1()
c
c              Constructor of class CChildView1
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CChildView1::CChildView1()
	: CFormView(CChildView1::IDD)
{
	//{{AFX_DATA_INIT(CChildView1)
	m_cuttype = 1;
	m_init = 0;
	m_memdc = NULL;
	m_picture = NULL;
	m_tool = "";
	m_desp = "";
	m_def1 = "";
	m_def2 = "";
	m_def3 = "";
	m_def4 = "";
	m_def5 = "";
	m_def6 = "";
	m_parm1 = "";
	m_parm2 = "";
	m_parm3 = "";
	m_parm4 = "";
	m_parm5 = "";
	m_parm6 = "";
	m_def_cut = 1;
	m_cdef1 = "";
	m_cdef2 = "";
	m_cdef3 = "";
	m_cdef4 = "";
	m_cdef5 = "";
	m_cdef6 = "";
	m_cparm1 = "";
	m_cparm2 = "";
	m_cparm3 = "";
	m_cparm4 = "";
	m_cparm5 = "";
	m_cparm6 = "";
	//}}AFX_DATA_INIT
/*
......Create font
*/	
#ifdef _UNICODE	
	WCHAR wfont[80];
	int len1 = strlen (Tool_font_name1) + 1;
	int wlen1 = MultiByteToWideChar(CP_ACP, 0, Tool_font_name1, -1, 
							wfont, len1);
	m_fntPage.CreatePointFont(Tool_font_size1, wfont);
#else
	m_fntPage.CreatePointFont(Tool_font_size1, Tool_font_name1);
#endif
	strcpy(m_file, "toolib_End Mill");
	m_load_file[0] = '\0';
	S_reset_typestr_list();
	S_add_typestr_list("Face Mill");
	S_add_typestr_list("End Mill");
	S_add_typestr_list("Barrel");
	S_add_typestr_list("Cone");
	S_add_typestr_list("Bell");
	S_add_typestr_list("Drill");
	S_add_typestr_list("Boring Tool");
	S_add_typestr_list("Reamer");
	S_add_typestr_list("Chamfer Tool");
	S_add_typestr_list("Blade");
	S_add_typestr_list("Square Insert");
	S_add_typestr_list("Diamond Insert");
	S_add_typestr_list("Triangle Insert");
	S_add_typestr_list("Circular Insert");
	S_add_typestr_list("Grooving Tool");
}
CChildView1::~CChildView1()
{
	if(m_picture != NULL) FreePictureData();
}

/***********************************************************************
c
c   FUNCTION: OnInitialUpdate()
c
c              Called by the framework before the view is initially displayed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CChildView1::OnInitialUpdate()
{
	CFormView::OnInitialUpdate();

	ChangeDialogFont(this, &m_fntPage);

	CWnd *picctl = GetDlgItem(IDC_METAFILE);
//	picctl->SetWindowPos(&wndTop, 0,0,180,170,SWP_NOMOVE);
	picctl->SetWindowPos(&wndTop, 0,0,200,200,SWP_NOMOVE);
	if (m_def_cut==0)
		((CWnd*)GetDlgItem(IDC_CUT_DEFINE))->EnableWindow(FALSE);
	else
		((CWnd*)GetDlgItem(IDC_CUT_DEFINE))->EnableWindow(TRUE);
/*
.....do not limit here since if a empty space there, make it unsure
.....what happened to the editor
*/
//	((CEdit*)GetDlgItem(IDC_EDIT_TOOL))->LimitText(15);
//	((CEdit*)GetDlgItem(IDC_EDIT_DESP))->LimitText(40);
/*
......initial add cutter type
*/
	S_reset_typestr_list();
	S_add_typestr_list("Face Mill");
	S_add_typestr_list("End Mill");
	S_add_typestr_list("Barrel");
	S_add_typestr_list("Cone");
	S_add_typestr_list("Bell");
	S_add_typestr_list("Drill");
	S_add_typestr_list("Boring Tool");
	S_add_typestr_list("Reamer");
	S_add_typestr_list("Chamfer Tool");
	S_add_typestr_list("Blade");
	S_add_typestr_list("Square Insert");
	S_add_typestr_list("Diamond Insert");
	S_add_typestr_list("Triangle Insert");
	S_add_typestr_list("Circular Insert");
	S_add_typestr_list("Grooving Tool");
	char tmpstr[81];
	for (int i=0;i<Tool_head.utype_no;i++)
	{
		strcpy(tmpstr, Tool_head.utype[i]);
		S_add_typestr_list(tmpstr, 0);
	}
	((CComboBox*)GetDlgItem(IDC_CUTTYPE))->SetCurSel(m_cuttype);
	m_init = 1;
}
              
/***********************************************************************
c
c   FUNCTION: OnUpdate(CView*, LPARAM, CObject*)
c
c          Called by the framework after the view's document 
c			has been modified; this function is 
c			called by CDocument::UpdateAllViews
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CChildView1::OnUpdate(CView*, LPARAM, CObject*)
{
	CToolibDoc* pDoc = GetDocument();
	m_cuttype = pDoc->m_cuttype;
	m_tool = pDoc->m_tool;
	m_desp = pDoc->m_desp;
	m_def1 = pDoc->m_def1;
	m_def2 = pDoc->m_def2;
	m_def3 = pDoc->m_def3;
	m_def4 = pDoc->m_def4;
	m_def5 = pDoc->m_def5;
	m_def6 = pDoc->m_def6;
	m_parm1 = pDoc->m_parm1;
	m_parm2 = pDoc->m_parm2;
	m_parm3 = pDoc->m_parm3;
	m_parm4 = pDoc->m_parm4;
	m_parm5 = pDoc->m_parm5;
	m_parm6 = pDoc->m_parm6;
	m_def_cut = pDoc->m_def_cut;
	m_cdef1 = pDoc->m_cdef1;
	m_cdef2 = pDoc->m_cdef2;
	m_cdef3 = pDoc->m_cdef3;
	m_cdef4 = pDoc->m_cdef4;
	m_cdef5 = pDoc->m_cdef5;
	m_cdef6 = pDoc->m_cdef6;
	m_cparm1 = pDoc->m_cparm1;
	m_cparm2 = pDoc->m_cparm2;
	m_cparm3 = pDoc->m_cparm3;
	m_cparm4 = pDoc->m_cparm4;
	m_cparm5 = pDoc->m_cparm5;
	m_cparm6 = pDoc->m_cparm6;
	m_cuttype = pDoc->m_cuttype;
	m_def_cut = pDoc->m_def_cut;

	UpdateData(FALSE);
	CComboBox * comb = (CComboBox*)GetDlgItem(IDC_CUTTYPE);
	comb->SetCurSel(m_cuttype);
	OnTCuttype();
	if (m_def_cut==0)
		((CWnd*)GetDlgItem(IDC_CUT_DEFINE))->EnableWindow(FALSE);
	else
		((CWnd*)GetDlgItem(IDC_CUT_DEFINE))->EnableWindow(TRUE);
}

/***********************************************************************
c
c   FUNCTION: IsDocModified()
c
c          Check if view is modified
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CChildView1::IsDocModified()
{
	UpdateData(TRUE); 
	CToolibDoc* pDoc = GetDocument();

	if (m_cuttype != pDoc->m_cuttype)
		return 1;
	if (m_tool != pDoc->m_tool)
		return 1;
	if (m_desp != pDoc->m_desp)
		return 1;
	if ( (GetDlgItem(IDC_EDIT_DEF1)->IsWindowEnabled()) 
			&& (m_def1 != pDoc->m_def1))
		return 1;
	if ((GetDlgItem(IDC_EDIT_DEF2)->IsWindowEnabled()) 
		&& (m_def2 != pDoc->m_def2))
		return 1;
	if ((GetDlgItem(IDC_EDIT_DEF3)->IsWindowEnabled()) 
		&&	(m_def3 != pDoc->m_def3))
		return 1;
	if ((GetDlgItem(IDC_EDIT_DEF4)->IsWindowEnabled()) 
		&&	(m_def4 != pDoc->m_def4))
		return 1;
	if ((GetDlgItem(IDC_EDIT_DEF5)->IsWindowEnabled()) 
		&&	(m_def5 != pDoc->m_def5))
		return 1;
	if ((GetDlgItem(IDC_EDIT_DEF6)->IsWindowEnabled()) 
		&&	(m_def6 != pDoc->m_def6))
		return 1;
	if ((GetDlgItem(IDC_EDIT_PARM1)->IsWindowEnabled()) 
		&&	(m_parm1 != pDoc->m_parm1))
		return 1;
	if ((GetDlgItem(IDC_EDIT_PARM2)->IsWindowEnabled()) 
		&&	(m_parm2 != pDoc->m_parm2))
		return 1;
	if ((GetDlgItem(IDC_EDIT_PARM3)->IsWindowEnabled()) 
		&&	(m_parm3 != pDoc->m_parm3))
		return 1;
	if ((GetDlgItem(IDC_EDIT_PARM4)->IsWindowEnabled()) 
		&&	(m_parm4 != pDoc->m_parm4))
		return 1;
	if ((GetDlgItem(IDC_EDIT_PARM5)->IsWindowEnabled()) 
		&&	(m_parm5 != pDoc->m_parm5))
		return 1;
	if ((GetDlgItem(IDC_EDIT_PARM6)->IsWindowEnabled()) 
		&&	(m_parm6 != pDoc->m_parm6))
		return 1;
	if (m_def_cut != pDoc->m_def_cut)
		return 1;
	if (m_cdef1 != pDoc->m_cdef1)
		return 1;
	if (m_cdef2 != pDoc->m_cdef2)
		return 1;
	if (m_cdef3 != pDoc->m_cdef3)
		return 1;
	if (m_cdef4 != pDoc->m_cdef4)
		return 1;
	if (m_cdef5 != pDoc->m_cdef5)
		return 1;
	if (m_cdef6 != pDoc->m_cdef6)
		return 1;
	if (m_cparm1 != pDoc->m_cparm1)
		return 1;
	if (m_cparm2 != pDoc->m_cparm2)
		return 1;
	if (m_cparm3 != pDoc->m_cparm3)
		return 1;
	if (m_cparm4 != pDoc->m_cparm4)
		return 1;
	if (m_cparm5 != pDoc->m_cparm5)
		return 1;
	if (m_cparm6 != pDoc->m_cparm6)
		return 1;
	if (m_cuttype != pDoc->m_cuttype)
		return 1;
	if (m_def_cut != pDoc->m_def_cut)
		return 1;
	return 0;
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

void CChildView1::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CChildView1)
	DDX_Text(pDX, IDC_EDIT_TOOL, m_tool);
	DDX_Text(pDX, IDC_EDIT_DESP, m_desp);
	DDX_Check(pDX, IDC_CUTTER, m_def_cut);
	DDX_Text(pDX, IDC_EDIT_DEF1, m_def1);
	DDX_Text(pDX, IDC_EDIT_DEF2, m_def2);
	DDX_Text(pDX, IDC_EDIT_DEF3, m_def3);
	DDX_Text(pDX, IDC_EDIT_DEF4, m_def4);
	DDX_Text(pDX, IDC_EDIT_DEF5, m_def5);
	DDX_Text(pDX, IDC_EDIT_DEF6, m_def6);
	DDX_Text(pDX, IDC_EDIT_PARM1, m_parm1);
	DDX_Text(pDX, IDC_EDIT_PARM2, m_parm2);
	DDX_Text(pDX, IDC_EDIT_PARM3, m_parm3);
	DDX_Text(pDX, IDC_EDIT_PARM4, m_parm4);
	DDX_Text(pDX, IDC_EDIT_PARM5, m_parm5);
	DDX_Text(pDX, IDC_EDIT_PARM6, m_parm6);
	DDX_CBIndex(pDX, IDC_CUTTYPE, m_cuttype);
	//}}AFX_DATA_MAP
//	OnTacceptEditChange();
}

BEGIN_MESSAGE_MAP(CChildView1, CFormView)
	//{{AFX_MSG_MAP(CChildView1)
	ON_COMMAND(IDC_CUTTER, OnPsuedo_Cutter)
	ON_COMMAND(IDC_CUT_DEFINE, OnDefine_Cutter)
	ON_CBN_EDITCHANGE(IDC_CUTTYPE, OnTCutEditChange)
	ON_CBN_KILLFOCUS(IDC_CUTTYPE, OnTacceptEditChange)
	ON_CBN_SELCHANGE(IDC_CUTTYPE, OnTCuttype)
//	ON_CBN_KILLFOCUS(IDC_CUTTYPE, OnTCuttype)
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CChildView1 message handlers

/***********************************************************************
c
c   FUNCTION: OnPsuedo_Cutter()
c
c         Called when user click "Psuedo_Cutter" toggle button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView1::OnPsuedo_Cutter()
{
	if (((CButton*)GetDlgItem(IDC_CUTTER))->GetCheck()==1)
	{
		m_def_cut = 1;
		((CWnd*)GetDlgItem(IDC_CUT_DEFINE))->EnableWindow(TRUE);
		OnDefine_Cutter();
	}
	else
	{
		m_def_cut = 1;
		((CWnd*)GetDlgItem(IDC_CUT_DEFINE))->EnableWindow(FALSE);
	}
}

/***********************************************************************
c
c   FUNCTION: OnDefine_Cutter()
c
c         Called when user click "Define" (define cutter) button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView1::OnDefine_Cutter()
{
	char cuttype[40];
	CComboBox * comb = (CComboBox*)GetDlgItem(IDC_CUTTYPE);
	CString str;
	comb->GetWindowText(str);
	int len = str.GetLength();	
	if (len>=40)
		len = 39;
	cuttype[0] = '\0';
	WCHAR *wtemp;
	if (len>0)
	{
#ifdef _UNICODE	
		wtemp = str.GetBuffer(len);
		wcstombs(cuttype, wtemp, len);
		cuttype[len] = '\0';
#else
		char *temp = str.GetBuffer(len);
		strncpy(cuttype, temp, len);
#endif
	}
	cuttype[len] = '\0';

	int oldnum = S_typestr_num-1;
	int type = S_add_typestr_list(cuttype);
	if (type>oldnum)
	{
#ifdef _UNICODE	
		comb->AddString(wtemp);
#else
		comb->AddString(cuttype);
#endif
		m_cuttype = type;
	}
	CCutDlg *cutdlg = new CCutDlg(this, m_cuttype);
	cutdlg->m_def1 = m_cdef1;
	cutdlg->m_def2 = m_cdef2;
	cutdlg->m_def3 = m_cdef3;
	cutdlg->m_def4 = m_cdef4;
	cutdlg->m_def5 = m_cdef5;
	cutdlg->m_def6 = m_cdef6;
	cutdlg->m_parm1 = m_cparm1;
	cutdlg->m_parm2 = m_cparm2;
	cutdlg->m_parm3 = m_cparm3;
	cutdlg->m_parm4 = m_cparm4;
	cutdlg->m_parm5 = m_cparm5;
	cutdlg->m_parm6 = m_cparm6;
	if (cutdlg->DoModal()==IDCANCEL)
	{
		delete cutdlg;
		return;
	}
	m_cdef1 = cutdlg->m_def1;
	m_cdef2 = cutdlg->m_def2;
	m_cdef3 = cutdlg->m_def3;
	m_cdef4 = cutdlg->m_def4;
	m_cdef5 = cutdlg->m_def5;
	m_cdef6 = cutdlg->m_def6;
	m_cparm1 = cutdlg->m_parm1;
	m_cparm2 = cutdlg->m_parm2;
	m_cparm3 = cutdlg->m_parm3;
	m_cparm4 = cutdlg->m_parm4;
	m_cparm5 = cutdlg->m_parm5;
	m_cparm6 = cutdlg->m_parm6;
	delete cutdlg;
}

/***********************************************************************
**
**   FUNCTION: FreePictureData()
**
**              Free picture data memory
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CChildView1::FreePictureData()
{
	if(m_picture != NULL)
	{
		m_picture->Release();
		m_picture = NULL;
		if (m_BufferBytes!=NULL)
		{
			free(m_BufferBytes);
			m_BufferBytes = NULL;
		}
		if (m_memdc!= NULL) DeleteDC(m_memdc); 
		m_memdc = NULL;
		if (m_hBmp != NULL) DeleteObject(m_hBmp);
		m_hBmp = NULL;
	}
	m_picture = NULL;
}
/***********************************************************************
**
**   FUNCTION: LoadPictureData(BYTE *pBuffer, int nSize)
**
**              Load the picture data from picture file
**
**   INPUT:  pBuffer: data to be loaded into m_picture
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CChildView1::LoadPictureData(BYTE *pBuffer, int nSize)
{
	BOOL bResult = FALSE;

	HGLOBAL hGlobal = GlobalAlloc(GMEM_MOVEABLE, nSize);

	if(hGlobal == NULL)
		{
		HWND hWnd = AfxGetApp()->GetMainWnd()->m_hWnd;
		MessageBoxEx(hWnd, _T("Can not allocate enough memory\t"), _T("CNCLgraphic Error"), MB_OK | MB_ICONSTOP, LANG_ENGLISH);
		return(FALSE);
		}

	void* pData = GlobalLock(hGlobal);
	memcpy(pData, pBuffer, nSize);
	GlobalUnlock(hGlobal);

	IStream* pStream = NULL;

	if(CreateStreamOnHGlobal(hGlobal, TRUE, &pStream) == S_OK)
	{
		HRESULT hr;
		if((hr = OleLoadPicture(pStream, nSize, FALSE, IID_IPicture, (LPVOID *)&m_picture)) == E_NOINTERFACE)
		{
			HWND hWnd = AfxGetApp()->GetMainWnd()->m_hWnd;
			MessageBoxEx(hWnd, _T("IPicture interface is not supported\t"), 
				_T("CNCLgraphic Error"), MB_OK | MB_ICONSTOP, LANG_ENGLISH);
			if (pStream!=NULL)
				pStream->Release();
			return(FALSE);
		}
		else 
		{
			pStream->Release();
			pStream = NULL;
			bResult = TRUE;
		}
	}

	FreeResource(hGlobal); 

	return(bResult);
}

void CChildView1::LoadPictureFile(LPCTSTR szFile)
{
	BOOL bResult = FALSE;
	CFile PictureFile;
	CFileException e;
	int	nSize = 0;

	if(m_picture != NULL) FreePictureData();

	if(PictureFile.Open(szFile, CFile::modeRead | CFile::typeBinary, &e))
	{
		nSize = (int)PictureFile.GetLength();
		BYTE* pBuffer = new BYTE[nSize];
	
		if(PictureFile.Read(pBuffer, nSize) > 0)
		{
			if(LoadPictureData(pBuffer, nSize))	bResult = TRUE;
		}
		PictureFile.Close();
		delete [] pBuffer;
		}
	else
		{
			bResult = FALSE;
		}
	if(m_picture != NULL)
	{ 
/*		m_picture->get_Height(&m_height);
		m_picture->get_Width(&m_wid);
	    m_height = MulDiv(m_height, 96, HIMETRIC_INCH);
	    m_wid  = MulDiv(m_wid,  96, HIMETRIC_INCH);
*/;		}
	else 
		{
/*		m_height = 0;
		m_wid = 0;
*/		bResult = FALSE;
		}
	return;
//	return(bResult);
}

BOOL CChildView1::CreatePicBitmap(CDC *pDC)
{
	BOOL bResult = FALSE;
	ILockBytes *Buffer = 0;
	IStorage   *pStorage = 0;
	IStream    *FileStream = 0;
	BYTE	   *pBufferBytes;
	STATSTG		BytesStatistics;
	DWORD		OutData;
	long		OutStream;
	CFile		BitmapFile;	CFileException e;
	double		SkipFloat = 0;
	DWORD		ByteSkip = 0;
	_ULARGE_INTEGER RealData;

	if (m_picture==NULL)
		return 0;
	CreateILockBytesOnHGlobal(NULL, TRUE, &Buffer);

	HRESULT hr = ::StgCreateDocfileOnILockBytes(Buffer,
				 STGM_SHARE_EXCLUSIVE | STGM_CREATE | STGM_READWRITE, 0, &pStorage);

	hr = pStorage->CreateStream(L"PICTURE",
		 STGM_SHARE_EXCLUSIVE | STGM_CREATE | STGM_READWRITE, 0, 0, &FileStream);

	m_picture->SaveAsFile(FileStream, TRUE, &OutStream);
	FileStream->Release();
	pStorage->Release();
	Buffer->Flush(); 

	Buffer->Stat(&BytesStatistics, STATFLAG_NONAME);

	SkipFloat = (double(OutStream) / 512);
	if ( SkipFloat > DWORD(SkipFloat) ) 
		ByteSkip = (DWORD)SkipFloat + 1;
	else 
		ByteSkip = (DWORD)SkipFloat;
	ByteSkip = ByteSkip * 512;
	
	ByteSkip = (DWORD)(BytesStatistics.cbSize.QuadPart - ByteSkip);

	RealData.LowPart = 0;
	RealData.HighPart = 0;
	RealData.QuadPart = ByteSkip;
	m_BufferBytes = (BYTE*)malloc(OutStream);
	pBufferBytes = m_BufferBytes;
	if (m_BufferBytes == NULL)
	{
		Buffer->Release();
		HWND hWnd = AfxGetApp()->GetMainWnd()->m_hWnd;
		MessageBoxEx(hWnd, _T("Can not allocate enough memory\t"), 
			_T("Error"), MB_OK | MB_ICONSTOP, LANG_ENGLISH);
	}

	Buffer->ReadAt(RealData, m_BufferBytes, OutStream, &OutData);

	BITMAPFILEHEADER  bmfHeader;
	memcpy(&bmfHeader, m_BufferBytes,sizeof(bmfHeader));

	DWORD bmfHeaderSize = sizeof(BITMAPFILEHEADER);
	m_BufferBytes += bmfHeaderSize;

	BITMAPINFOHEADER &bmiHeader = *(LPBITMAPINFOHEADER)m_BufferBytes ;
	BITMAPINFO &bmInfo = *(LPBITMAPINFO)m_BufferBytes ;

	int nColors = bmiHeader.biClrUsed ? bmiHeader.biClrUsed : 
						1 << bmiHeader.biBitCount;

	LPVOID lpDIBBits;
	if ( bmInfo.bmiHeader.biBitCount > 8 )
		lpDIBBits = (LPVOID)((LPDWORD)(bmInfo.bmiColors + bmInfo.bmiHeader.biClrUsed) + 
			((bmInfo.bmiHeader.biCompression == BI_BITFIELDS) ? 3 : 0));
	else
		lpDIBBits = (LPVOID)(bmInfo.bmiColors + nColors);

	if (m_hBmp != NULL) DeleteObject(m_hBmp);
	m_hBmp = CreateDIBitmap(pDC->m_hDC,
				&bmiHeader,			
				CBM_INIT,			
				lpDIBBits,			
				&bmInfo,			
				DIB_RGB_COLORS);	

	Buffer->Release();
	m_BufferBytes = pBufferBytes;
	return TRUE;
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

void CChildView1::OnPaint() 
{
	CView::OnPaint();
	char fullname[UX_MAX_PATH_LEN], fname[UX_MAX_PATH_LEN], direc[256],fext[256], fname2[UX_MAX_PATH_LEN];
	if (m_picture!=NULL)
	{
		if (strcmp(m_load_file, m_file)!=0)
		{
			m_picture->Release();
			m_picture = NULL;
		}
	}
	if (m_picture==NULL)
	{
		tool_break_fname(m_file, direc, fname, fext);
		FILE *fptr;
		if (fext[0]=='\0')
			strcat(fname, ".jpg");
		if (direc[0]=='\0')
			strcpy(direc, "UD_FORMDIR");
		int status = ul_open_mod_file(NULL, NULL, direc,
						NULL, fname, 2,  &fptr);
		if (fptr!=NULL)
			fclose(fptr);
		strcpy(fullname, fname);

#ifdef _UNICODE	
		WCHAR wfullname[UX_MAX_PATH_LEN];
		int len1 = strlen (fullname) + 1;
		int wlen1 = MultiByteToWideChar(CP_ACP, 0, fullname, -1, 
							wfullname, len1);
		LoadPictureFile(wfullname);
#else
		LoadPictureFile(fullname);
#endif
		if (m_picture)
			strcpy(m_load_file, m_file);
		else
		{
/*
......load the default picture
*/
			strcpy(fname, "tool_custom_type.jpg");
			strcpy(direc, "UD_FORMDIR");
			int status = ul_open_mod_file(NULL, NULL, direc,
						NULL, fname, 2,  &fptr);
			if (fptr!=NULL)
				fclose(fptr);
			strcpy(fullname, fname);
#ifdef _UNICODE	
			len1 = strlen (fullname) + 1;
			wlen1 = MultiByteToWideChar(CP_ACP, 0, fullname, -1, 
								wfullname, len1);
			LoadPictureFile(wfullname);
#else
			LoadPictureFile(fullname);
#endif
			if (m_picture)
				strcpy(m_load_file, m_file);
		}
	}
	if (m_picture)
	{
		CWnd *picctl = GetDlgItem(IDC_METAFILE);
//		CClientDC dc(picctl);
		CDC	*dc = picctl->GetDC();
		CreatePicBitmap(dc);

		long hmWidth;
		long hmHeight;
		m_picture->get_Width(&hmWidth);
		m_picture->get_Height(&hmHeight);

		int nWidth	= MulDiv(hmWidth, dc->GetDeviceCaps(LOGPIXELSX), HIMETRIC_INCH);
		int nHeight	= MulDiv(hmHeight, dc->GetDeviceCaps(LOGPIXELSY), HIMETRIC_INCH);

		RECT rc;
		GetDlgItem(IDC_METAFILE)->GetClientRect(&rc);
	
		int right, bottom;
		float rat = (float)hmHeight/(float)hmWidth;
		float rat2 = (float)(rc.bottom-rc.top)/(float)(rc.right-rc.left);
		if (rat<rat2)
		{
			right = (rc.right-rc.left);
			bottom = (rc.right-rc.left)*rat;
		}
		else
		{
			bottom = (rc.bottom-rc.top);
			right = (rc.bottom-rc.top)/rat;
		}
		if (m_memdc==NULL)
			m_memdc=::CreateCompatibleDC(dc->m_hDC);
		::SelectObject(m_memdc, m_hBmp);
		SetStretchBltMode(dc->m_hDC, HALFTONE);

		POINT pt;
		SetBrushOrgEx(dc->m_hDC, 0, 0, &pt);
		StretchBlt(dc->m_hDC, 0,0, right, bottom,  m_memdc, 0, 0, nWidth, nHeight, SRCCOPY );
	}
	else
	{
		CWnd *picctl = GetDlgItem(IDC_METAFILE);
		CClientDC dc(picctl);
		dc.SetBkMode(TRANSPARENT);
		RECT rc;
		picctl->GetClientRect(&rc);	
		CBrush* pBrush;
		rc.bottom++;
		rc.right++;
		pBrush = new CBrush(RGB(220,220,220));
		dc.FillRect(&rc, pBrush );
/*
......output text String
*/
		dc.TextOut(10, 10, _T("Picture not available"), 21);
		delete pBrush;
	}
}

/***********************************************************************
c
c   FUNCTION: OnTCuttype()
c
c         This function called when cutter type changed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView1::OnTCutEditChange()
{
	CString str;
	CComboBox * comb = (CComboBox*)GetDlgItem(IDC_CUTTYPE);
	comb->GetWindowText(str);

	DWORD dwSel =  comb->GetEditSel();
	int x = LOWORD( dwSel );
	if (x<=0)
		return;
	if( ispunct(str[x-1]))
	{
		MessageBeep(0);
		str = str.Left(x-1) + str.Mid(x);
		comb->SetWindowText(str);
		comb->SetEditSel(x, x);
		return;
	}
}

void CChildView1::OnTacceptEditChange()
{
	char cuttype[40];
	CString str;
	CComboBox * comb = (CComboBox*)GetDlgItem(IDC_CUTTYPE);
	comb->GetWindowText(str);

	int len = str.GetLength();	
#ifdef _UNICODE	
	WCHAR *wtemp = str.GetBuffer(len);
	if (len>=40)
		len = 39;
	if (len>0)
	{
		wcstombs(cuttype, wtemp, len);
		cuttype[len] = '\0';
	}
	else
		cuttype[0] = '\0';
	if (len>0)
	{
		int oldnum = S_typestr_num-1;
		int type = S_add_typestr_list(cuttype);
		if (type>oldnum)
		{
			comb->AddString(wtemp);
		}
		m_cuttype = type;
		sprintf(m_file, "toolib_%s", cuttype);
		OnPaint();
		OnTCuttype();
	}
	else if ((m_init)&&(TOOL_main_frame->m_update==0))
	{
/*
.....must define a cutter type or select a cutter type
*/
		sprintf(m_file, "toolib_%s", cuttype);
		tool_mfmsg_box(this, "Tool Type Error", 
			"You must define a cutter type or select a cutter type!", 1);
		GetDlgItem(IDC_CUTTYPE)->SetFocus();
	}
#else	
	char* temp = str.GetBuffer(len);
	if (len>=40)
		len = 39;
	strncpy(cuttype, temp, len);
	cuttype[len] = '\0';

	if (len>0)
	{
		int oldnum = S_typestr_num-1;
		int type = S_add_typestr_list(cuttype);
		if (type>oldnum)
		{
			comb->AddString(cuttype);
			m_cuttype = type;
		}
		sprintf(m_file, "toolib_%s", cuttype);
		OnPaint();
	}
#endif
}

/***********************************************************************
c
c   FUNCTION: OnTCuttype()
c
c         This function called when cutter type changed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView1::OnTCuttype()
{
	char cuttype[40];
	CString str;

	CComboBox * comb = (CComboBox*)GetDlgItem(IDC_CUTTYPE);
	int idx = comb->GetCurSel();
	if( idx >= 0 )
	{
		m_cuttype = idx;
		sprintf(m_file, "toolib_%s", S_typestr_list[idx]);
	}
	if (m_cuttype<0)
		return;
/*
.....all user define type (>=15) will active same fields as Bell (type=4)
*/
/*
.....Field 1 - Diameter,Radius
*/
	((CWnd*)GetDlgItem(IDC_EDIT_DEF1))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_EDIT_PARM1))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
	if (m_cuttype==9)
		((CWnd*)GetDlgItem(IDC_STATIC1))->SetWindowText(_T("Width:"));
//	else if (m_cuttype >= 10)
	else if ((m_cuttype >= 10)&&(m_cuttype < 15))
		((CWnd*)GetDlgItem(IDC_STATIC1))->SetWindowText(_T("Radius:"));
	else
		((CWnd*)GetDlgItem(IDC_STATIC1))->SetWindowText(_T("Diameter:"));
/*
.....Field 2 - Radius,Diameter,Chizel,Width
*/
	if (m_cuttype == 5 || m_cuttype == 8 || m_cuttype == 13)
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF2))->SetWindowText(_T(""));
		((CWnd*)GetDlgItem(IDC_EDIT_PARM2))->SetWindowText(_T(""));
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(TRUE);
	}
	if (m_cuttype==9)
		((CWnd*)GetDlgItem(IDC_STATIC2))->SetWindowText(_T("Chizel:"));
	else if (m_cuttype==14)
		((CWnd*)GetDlgItem(IDC_STATIC2))->SetWindowText(_T("Width:"));
//	else if (m_cuttype >= 10)
	else if ((m_cuttype >= 10)&&(m_cuttype < 15))
		((CWnd*)GetDlgItem(IDC_STATIC2))->SetWindowText(_T("Diameter:"));
	else
		((CWnd*)GetDlgItem(IDC_STATIC2))->SetWindowText(_T("Corner Radius:"));
/*
.....Field 3 - Height
*/
	((CWnd*)GetDlgItem(IDC_EDIT_DEF3))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_EDIT_PARM3))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(TRUE);
/*
.....Field 4 - Side Angle, Side Radius, Angle
*/
	if (m_cuttype == 2 || m_cuttype == 3 || m_cuttype == 4 || m_cuttype == 5 ||
		m_cuttype == 8 || m_cuttype == 9 || m_cuttype == 11 || m_cuttype >=15)
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM4))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(TRUE);
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(FALSE);
		if (m_cuttype == 10)
			((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->SetWindowText(_T("90."));
		else if (m_cuttype == 12)
			((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->SetWindowText(_T("60."));
		else
			((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->SetWindowText(_T(""));
		((CWnd*)GetDlgItem(IDC_EDIT_PARM4))->SetWindowText(_T(""));
	}
	if (m_cuttype==2)
		((CWnd*)GetDlgItem(IDC_STATIC4))->SetWindowText(_T("Side Radius:"));
//	else if (m_cuttype >= 9)
	else if ((m_cuttype >= 9)&&(m_cuttype < 15))
		((CWnd*)GetDlgItem(IDC_STATIC4))->SetWindowText(_T("Angle:"));
	else
		((CWnd*)GetDlgItem(IDC_STATIC4))->SetWindowText(_T("Side Angle:"));
/*
.....Field 5 - Z-Height, Mount Angle, Length
*/
	if (m_cuttype == 2 || (m_cuttype >= 10 && m_cuttype != 13 && m_cuttype < 15))
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF5))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM5))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC5))->EnableWindow(TRUE);
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF5))->SetWindowText(_T(""));
		((CWnd*)GetDlgItem(IDC_EDIT_PARM5))->SetWindowText(_T(""));
	}
	if (m_cuttype==14)
		((CWnd*)GetDlgItem(IDC_STATIC5))->SetWindowText(_T("Length:"));
	else if ((m_cuttype >= 10)&&(m_cuttype < 15))
		((CWnd*)GetDlgItem(IDC_STATIC5))->SetWindowText(_T("Mount Angle:"));
	else
		((CWnd*)GetDlgItem(IDC_STATIC5))->SetWindowText(_T("Z-Height:"));
/*
.....Field 6 - Flat Angle
*/
	if (m_cuttype == 2)
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF6))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM6))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(TRUE);
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF6))->SetWindowText(_T(""));
		((CWnd*)GetDlgItem(IDC_EDIT_PARM6))->SetWindowText(_T(""));
	}
/*
.....Pseudo Cutter
*/
//	if (m_cuttype < 9)
	if ((m_cuttype<9)||(m_cuttype >=15))
	{
		((CWnd*)GetDlgItem(IDC_CUTTER))->EnableWindow(TRUE);
		if (m_def_cut == 1)
			((CWnd*)GetDlgItem(IDC_CUT_DEFINE))->EnableWindow(TRUE);
		else
			((CWnd*)GetDlgItem(IDC_CUT_DEFINE))->EnableWindow(FALSE);
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_CUTTER))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_CUT_DEFINE))->EnableWindow(FALSE);
	}
	CView2->OnSymbol();
	CView2->OnShank();
	CView2->OnHolder();
/*
.....Refresh form
*/
	OnPaint();
}
void CChildView1::UpdateCutType()
{
	int i, oldnum, type;
	CComboBox * comb = (CComboBox*)GetDlgItem(IDC_CUTTYPE);
/*
.....reset all
*/
	m_init = 0;
	S_reset_typestr_list();
	int cont = comb->GetCount();
	for (int j = 0; j<cont; j++)
	{
		comb->DeleteString(0);
	}
	S_add_typestr_list("Face Mill",0);
	comb->AddString(_T("Face Mill"));
	S_add_typestr_list("End Mill",0);
	comb->AddString(_T("End Mill"));
	S_add_typestr_list("Barrel",0);
	comb->AddString(_T("Barrel"));
	S_add_typestr_list("Cone",0);
	comb->AddString(_T("Cone"));
	S_add_typestr_list("Bell",0);
	comb->AddString(_T("Bell"));
	S_add_typestr_list("Drill",0);
	comb->AddString(_T("Drill"));
	S_add_typestr_list("Boring Tool",0);
	comb->AddString(_T("Boring Tool"));
	S_add_typestr_list("Reamer",0);
	comb->AddString(_T("Reamer"));
	S_add_typestr_list("Chamfer Tool",0);
	comb->AddString(_T("Chamfer Tool"));
	S_add_typestr_list("Blade",0);
	comb->AddString(_T("Blade"));
	S_add_typestr_list("Square Insert",0);
	comb->AddString(_T("Square Insert"));
	S_add_typestr_list("Diamond Insert",0);
	comb->AddString(_T("Diamond Insert"));
	S_add_typestr_list("Triangle Insert",0);
	comb->AddString(_T("Triangle Insert"));
	S_add_typestr_list("Circular Insert",0);
	comb->AddString(_T("Circular Insert"));
	S_add_typestr_list("Grooving Tool",0);
	comb->AddString(_T("Grooving Tool"));
	char tmpstr[81];
	for (i=0;i<Tool_head.utype_no;i++)
	{
		oldnum = S_typestr_num-1;
		strcpy(tmpstr, Tool_head.utype[i]);
		type = S_add_typestr_list(tmpstr, 0);
		if (type>oldnum)
		{
#ifdef _UNICODE	
			WCHAR *wtype;
			int len = strlen (Tool_head.utype[i]) + 1;
			wtype = new WCHAR[len];
			int wlen = MultiByteToWideChar(CP_ACP, 0, 
							Tool_head.utype[i], -1, 
							wtype, len);
			comb->AddString(wtype);
			delete wtype;
#else
			comb->AddString(Tool_head.utype[i]);
#endif
		}
	}
	m_init = 1;
}
BOOL CChildView1::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs
//	CtoolibApp *app = (CToolibApp *)AfxGetApp();
//	cs.lpszClass = (const char*) (app->m_strMyClassName);

	return CFormView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
// CChildView2

IMPLEMENT_DYNCREATE(CChildView2, CFormView)
/***********************************************************************
c
c   FUNCTION: CChildView2()
c
c              Constructor of class CChildView2
c
c   INPUT: None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CChildView2::CChildView2()
	: CFormView(CChildView2::IDD)
{
	//{{AFX_DATA_INIT(CChildView2)
	m_segment = 0;
	m_moving = 0;
	m_shade = 0;
	m_loadcom = 0;
	m_symbol_ck = 0;
	m_shank_ck = 0;
	m_holder_ck = 0;
	m_symbol = "";
	m_holder = "";
	m_shank = "";
	m_tooldraw = "";
	//}}AFX_DATA_INIT
	CView2 = this;
	for (int i=0; i<41; i++)
		m_com[i] = "";
	symbol_value.m_class = "All";
	symbol_value.m_value1 = 0.0;
	symbol_value.m_value2 = 0.0;
	symbol_value.m_value3 = 0.0;
	symbol_value.m_value4 = 0.0;
	symbol_value.m_parm1 = "";
	symbol_value.m_parm2 = "";
	symbol_value.m_parm3 = "";
	symbol_value.m_parm4 = "";
	symbol_value.m_symbol = "";
	symbol_value.m_shade = "Off";

	shank_value.m_class = "All";
	shank_value.m_value1 = 0.0;
	shank_value.m_value2 = 0.0;
	shank_value.m_value3 = 0.0;
	shank_value.m_value4 = 0.0;
	shank_value.m_parm1 = "";
	shank_value.m_parm2 = "";
	shank_value.m_parm3 = "";
	shank_value.m_parm4 = "";
	shank_value.m_symbol = "";
	shank_value.m_shade = "Off";

	holder_value.m_class = "All";
	holder_value.m_value1 = 0.0;
	holder_value.m_value2 = 0.0;
	holder_value.m_value3 = 0.0;
	holder_value.m_value4 = 0.0;
	holder_value.m_parm1 = "";
	holder_value.m_parm2 = "";
	holder_value.m_parm3 = "";
	holder_value.m_parm4 = "";
	holder_value.m_symbol = "";
	holder_value.m_shade = "Off";
/*
......Create font
*/	
#ifdef _UNICODE	
	WCHAR wfont[80];
	int len1 = strlen (Tool_font_name2) + 1;
	int wlen1 = MultiByteToWideChar(CP_ACP, 0, Tool_font_name2, -1, 
							wfont, len1);
	m_fntPage.CreatePointFont(Tool_font_size2, wfont);
#else
	m_fntPage.CreatePointFont(Tool_font_size2, Tool_font_name2);
#endif
/*
.....initial backgroup brush for ReadOnly Edit control
*/
	m_pEditBkBrush = new CBrush(RGB(255, 255, 255));
}

CChildView2::~CChildView2()
{
	delete m_pEditBkBrush;
}
BOOL CChildView2::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs
//	CToolibApp *app = (CToolibApp *)AfxGetApp();
//	cs.lpszClass = (const char*) (app->m_strMyClassName);

	return CView::PreCreateWindow(cs);
}
/***********************************************************************
c
c   FUNCTION: OnInitialUpdate()
c
c              Called by the framework before the view is initially displayed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CChildView2::OnInitialUpdate()
{
	CFormView::OnInitialUpdate();
	ChangeDialogFont(this, &m_fntPage);
	if (m_loadcom==0)
		((CWnd*)GetDlgItem(IDC_DISP_DEFINE))->EnableWindow(FALSE);
	else
		((CWnd*)GetDlgItem(IDC_DISP_DEFINE))->EnableWindow(TRUE);
	OnSymbol();
	OnShank();
	OnHolder();
}

/***********************************************************************
c
c   FUNCTION: OnUpdate(CView*, LPARAM, CObject*)
c
c          Called by the framework after the view's document 
c			has been modified; this function is 
c			called by CDocument::UpdateAllViews
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CChildView2::OnUpdate(CView*, LPARAM, CObject*)
{
	CToolibDoc* pDoc = GetDocument();

	m_segment = pDoc->m_segment;
	m_moving = pDoc->m_moving;
	m_shade = pDoc->m_shade;
	m_loadcom = pDoc->m_loadcom;
	m_symbol_ck = pDoc->m_symbol_ck;
	m_shank_ck = pDoc->m_shank_ck;
	m_holder_ck = pDoc->m_holder_ck;

	m_symbol = pDoc->m_symbol;
	m_holder = pDoc->m_holder;
	m_shank = pDoc->m_shank;
	m_tooldraw = pDoc->m_tooldraw;
	for (int i=0; i<41; i++)
		m_com[i] = pDoc->m_com[i];
	symbol_value.m_class = pDoc->symbol_value.m_class;
	symbol_value.m_value1 = pDoc->symbol_value.m_value1;
	symbol_value.m_value2 = pDoc->symbol_value.m_value2;
	symbol_value.m_value3 = pDoc->symbol_value.m_value3;
	symbol_value.m_value4 = pDoc->symbol_value.m_value4;
	symbol_value.m_parm1 = pDoc->symbol_value.m_parm1;
	symbol_value.m_parm2 = pDoc->symbol_value.m_parm2;
	symbol_value.m_parm3 = pDoc->symbol_value.m_parm3;
	symbol_value.m_parm4 = pDoc->symbol_value.m_parm4;
	symbol_value.m_symbol = pDoc->symbol_value.m_symbol;
	symbol_value.m_shade = pDoc->symbol_value.m_shade;

	shank_value.m_class = pDoc->shank_value.m_class;
	shank_value.m_value1 = pDoc->shank_value.m_value1;
	shank_value.m_value2 = pDoc->shank_value.m_value2;
	shank_value.m_value3 = pDoc->shank_value.m_value3;
	shank_value.m_value4 = pDoc->shank_value.m_value4;
	shank_value.m_parm1 = pDoc->shank_value.m_parm1;
	shank_value.m_parm2 = pDoc->shank_value.m_parm2;
	shank_value.m_parm3 = pDoc->shank_value.m_parm3;
	shank_value.m_parm4 = pDoc->shank_value.m_parm4;
	shank_value.m_symbol = pDoc->shank_value.m_symbol;
	shank_value.m_shade = pDoc->shank_value.m_shade;

	holder_value.m_class = pDoc->holder_value.m_class;
	holder_value.m_value1 = pDoc->holder_value.m_value1;
	holder_value.m_value2 = pDoc->holder_value.m_value2;
	holder_value.m_value3 = pDoc->holder_value.m_value3;
	holder_value.m_value4 = pDoc->holder_value.m_value4;
	holder_value.m_parm1 = pDoc->holder_value.m_parm1;
	holder_value.m_parm2 = pDoc->holder_value.m_parm2;
	holder_value.m_parm3 = pDoc->holder_value.m_parm3;
	holder_value.m_parm4 = pDoc->holder_value.m_parm4;
	holder_value.m_symbol = pDoc->holder_value.m_symbol;
	holder_value.m_shade = pDoc->holder_value.m_shade;
	
	UpdateData(FALSE);  // set the data into the controls
	if (m_loadcom==0)
		((CWnd*)GetDlgItem(IDC_DISP_DEFINE))->EnableWindow(FALSE);
	else
		((CWnd*)GetDlgItem(IDC_DISP_DEFINE))->EnableWindow(TRUE);
}
/***********************************************************************
c
c   FUNCTION: IsDocModified()
c
c          Check if view is modified
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

int CChildView2::IsDocModified()
{
	UpdateData(TRUE); 
	CToolibDoc* pDoc = GetDocument();

	if (m_segment != pDoc->m_segment)
		return 1;
	if (m_moving != pDoc->m_moving)
		return 1;
	if (m_shade != pDoc->m_shade)
		return 1;
	if (m_loadcom != pDoc->m_loadcom)
		return 1;
	if (m_symbol_ck != pDoc->m_symbol_ck)
		return 1;
	if (m_shank_ck != pDoc->m_shank_ck)
		return 1;
	if (m_holder_ck != pDoc->m_holder_ck)
		return 1;

	if (m_symbol != pDoc->m_symbol)
		return 1;
	if (m_holder != pDoc->m_holder)
		return 1;
	if (m_shank != pDoc->m_shank)
		return 1;
	if (m_tooldraw != pDoc->m_tooldraw)
		return 1;
	for (int i=0; i<41; i++)
		if (m_com[i] != pDoc->m_com[i])
			return 1;

	if (symbol_value.m_value1 != pDoc->symbol_value.m_value1)
		return 1;
	if (symbol_value.m_value2 != pDoc->symbol_value.m_value2)
		return 1;
	if (symbol_value.m_value3 != pDoc->symbol_value.m_value3)
		return 1;
	if (symbol_value.m_value4 != pDoc->symbol_value.m_value4)
		return 1;
	if (symbol_value.m_parm1 != pDoc->symbol_value.m_parm1)
		return 1;
	if (symbol_value.m_parm2 != pDoc->symbol_value.m_parm2)
		return 1;
	if (symbol_value.m_parm3 != pDoc->symbol_value.m_parm3)
		return 1;
	if (symbol_value.m_parm4 != pDoc->symbol_value.m_parm4)
		return 1;
	if (symbol_value.m_shade != pDoc->symbol_value.m_shade)
		return 1;
	if (shank_value.m_value1 != pDoc->shank_value.m_value1)
		return 1;
	if (shank_value.m_value2 != pDoc->shank_value.m_value2)
		return 1;
	if (shank_value.m_value3 != pDoc->shank_value.m_value3)
		return 1;
	if (shank_value.m_value4 != pDoc->shank_value.m_value4)
		return 1;
	if (shank_value.m_parm1 != pDoc->shank_value.m_parm1)
		return 1;
	if (shank_value.m_parm2 != pDoc->shank_value.m_parm2)
		return 1;
	if (shank_value.m_parm3 != pDoc->shank_value.m_parm3)
		return 1;
	if (shank_value.m_parm4 != pDoc->shank_value.m_parm4)
		return 1;
	if (shank_value.m_shade != pDoc->shank_value.m_shade)
		return 1;
	if (holder_value.m_value1 != pDoc->holder_value.m_value1)
		return 1;
	if (holder_value.m_value2 != pDoc->holder_value.m_value2)
		return 1;
	if (holder_value.m_value3 != pDoc->holder_value.m_value3)
		return 1;
	if (holder_value.m_value4 != pDoc->holder_value.m_value4)
		return 1;
	if (holder_value.m_parm1 != pDoc->holder_value.m_parm1)
		return 1;
	if (holder_value.m_parm2 != pDoc->holder_value.m_parm2)
		return 1;
	if (holder_value.m_parm3 != pDoc->holder_value.m_parm3)
		return 1;
	if (holder_value.m_parm4 != pDoc->holder_value.m_parm4)
		return 1;
	if (holder_value.m_shade != pDoc->holder_value.m_shade)
		return 1;
	return 0;
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

void CChildView2::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CChildView2)
	DDX_CBIndex(pDX, IDC_DISP_SEG, m_segment);
	DDX_CBIndex(pDX, IDC_DISP_MOV, m_moving);
	DDX_CBIndex(pDX, IDC_SHADE, m_shade);
	DDX_Text(pDX, IDC_DISP_DRAW, m_tooldraw);
	DDX_Check(pDX, IDC_DISP_LOAD, m_loadcom);

	DDX_Check(pDX, IDC_SYMBOL_CK, m_symbol_ck);
	DDX_Check(pDX, IDC_SHANK_CK, m_shank_ck);
	DDX_Check(pDX, IDC_HOLDER_CK, m_holder_ck);
	DDX_Text(pDX, IDC_SYLBOL_NAME, m_symbol);
	DDX_Text(pDX, IDC_SHANK_NAME, m_shank);
	DDX_Text(pDX, IDC_HOLDER_NAME, m_holder);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CChildView2, CFormView)
	//{{AFX_MSG_MAP(CChildView2)
	ON_COMMAND(IDC_DISP_LOAD, OnDispLoad)
	ON_COMMAND(IDC_DISP_DEFINE, OnDefine_Command)

	ON_BN_CLICKED(IDC_TOOLDRAW, OnTooldraw)
	ON_CBN_SELCHANGE(IDC_SHADE, OnShdchg)
	ON_WM_CTLCOLOR()

	ON_COMMAND(IDC_SYMBOL_CK, OnSymbol)
	ON_COMMAND(IDC_SHANK_CK, OnShank)
	ON_COMMAND(IDC_HOLDER_CK, OnHolder)

	ON_BN_CLICKED(IDC_SYMBOL_BUTTON, OnSymbol_disp)
	ON_BN_CLICKED(IDC_SHANK_BUTTON, OnShank_disp)
	ON_BN_CLICKED(IDC_HOLDER_BUTTON, OnHolder_disp)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CChildView2 message handlers
/***********************************************************************
c
c   FUNCTION: OnTCuttype()
c
c         This function called when "Filter:cutter type" changed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView2::OnShdchg()
{
	CComboBox * comb = (CComboBox*)GetDlgItem(IDC_SHADE);
	int pos = comb->GetCurSel();
		
	if (pos==0)
	{
		symbol_value.m_shade = "Default";
		shank_value.m_shade = "Default";
		holder_value.m_shade = "Default";
	}
	else if (pos==1)
	{
		symbol_value.m_shade = "On";
		shank_value.m_shade = "On";
		holder_value.m_shade = "On";
	}
	else if (pos==2)
	{
		symbol_value.m_shade = "Off";
		shank_value.m_shade = "Off";
		holder_value.m_shade = "Off";
	}
	if (symbol_value.IsCreated()==1)
	{
		symbol_value.SetShade(pos);
	}
	if (shank_value.IsCreated()==1)
	{
		shank_value.SetShade(pos);
	}
	if (holder_value.IsCreated()==1)
	{
		holder_value.SetShade(pos);
	}
}

/***********************************************************************
c
c   FUNCTION: OnDispLoad()
c
c          function called when "Load command" toggle button is pushed
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView2::OnDispLoad()
{
	if (((CButton*)GetDlgItem(IDC_DISP_LOAD))->GetCheck()==1)
	{
		((CWnd*)GetDlgItem(IDC_DISP_DEFINE))->EnableWindow(TRUE);
		OnDefine_Command();
		m_loadcom = 1;
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_DISP_DEFINE))->EnableWindow(FALSE);
		m_loadcom = 0;
/*
.....clear up the saved load command
*/
		for (int i=0; i<41; i++)
			m_com[i] = "";
	}
}

/***********************************************************************
c
c   FUNCTION: OnDefine_Command()
c
c          function called when "Define" (define command) button is pushed
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CChildView2::OnDefine_Command()
{
	CComDlg * comdlg = new CComDlg(this);
	comdlg->m_com1 = m_com[0];
	comdlg->m_com2 = m_com[1];
	comdlg->m_com3 = m_com[2];
	comdlg->m_com4 = m_com[3];
	comdlg->m_com5 = m_com[4];
	comdlg->m_com6 = m_com[5];
	comdlg->m_com7 = m_com[6];
	comdlg->m_com8 = m_com[7];
	comdlg->m_com9 = m_com[8];
	comdlg->m_com10 = m_com[9];
	comdlg->m_com11 = m_com[10];
	comdlg->m_com12 = m_com[11];
	comdlg->m_com13 = m_com[12];
	comdlg->m_com14 = m_com[13];
	comdlg->m_com15 = m_com[14];
	comdlg->m_com16 = m_com[15];
	comdlg->m_com17 = m_com[16];
	comdlg->m_com18 = m_com[17];
	comdlg->m_com19 = m_com[18];
	comdlg->m_com20 = m_com[19];
	comdlg->m_com21 = m_com[20];
	comdlg->m_com22 = m_com[21];
	comdlg->m_com23 = m_com[22];
	comdlg->m_com24 = m_com[23];
	comdlg->m_com25 = m_com[24];
	comdlg->m_com26 = m_com[25];
	comdlg->m_com27 = m_com[26];
	comdlg->m_com28 = m_com[27];
	comdlg->m_com29 = m_com[28];
	comdlg->m_com30 = m_com[29];
	comdlg->m_com31 = m_com[30];
	comdlg->m_com32 = m_com[31];
	comdlg->m_com33 = m_com[32];
	comdlg->m_com34 = m_com[33];
	comdlg->m_com35 = m_com[34];
	comdlg->m_com36 = m_com[35];
	comdlg->m_com37 = m_com[36];
	comdlg->m_com38 = m_com[37];
	comdlg->m_com39 = m_com[38];
	comdlg->m_com40 = m_com[39];
	comdlg->m_com41 = m_com[40];
	if (comdlg->DoModal()==IDCANCEL)
	{
		delete comdlg;
		return;
	}
	m_com[0] = comdlg->m_com1;
	m_com[1] = comdlg->m_com2;
	m_com[2] = comdlg->m_com3;
	m_com[3] = comdlg->m_com4;
	m_com[4] = comdlg->m_com5;
	m_com[5] = comdlg->m_com6;
	m_com[6] = comdlg->m_com7;
	m_com[7] = comdlg->m_com8;
	m_com[8] = comdlg->m_com9;
	m_com[9] = comdlg->m_com10;
	m_com[10] = comdlg->m_com11;
	m_com[11] = comdlg->m_com12;
	m_com[12] = comdlg->m_com13;
	m_com[13] = comdlg->m_com14;
	m_com[14] = comdlg->m_com15;
	m_com[15] = comdlg->m_com16;
	m_com[16] = comdlg->m_com17;
	m_com[17] = comdlg->m_com18;
	m_com[18] = comdlg->m_com19;
	m_com[19] = comdlg->m_com20;
	m_com[20] = comdlg->m_com21;
	m_com[21] = comdlg->m_com22;
	m_com[22] = comdlg->m_com23;
	m_com[23] = comdlg->m_com24;
	m_com[24] = comdlg->m_com25;
	m_com[25] = comdlg->m_com26;
	m_com[26] = comdlg->m_com27;
	m_com[27] = comdlg->m_com28;
	m_com[28] = comdlg->m_com29;
	m_com[29] = comdlg->m_com30;
	m_com[30] = comdlg->m_com31;
	m_com[31] = comdlg->m_com32;
	m_com[32] = comdlg->m_com33;
	m_com[33] = comdlg->m_com34;
	m_com[34] = comdlg->m_com35;
	m_com[35] = comdlg->m_com36;
	m_com[36] = comdlg->m_com37;
	m_com[37] = comdlg->m_com38;
	m_com[38] = comdlg->m_com39;
	m_com[39] = comdlg->m_com40;
	m_com[40] = comdlg->m_com41;
	delete comdlg;
}
void CChildView2::OnTooldraw() 
{
	UX_pathname name,fname;
	int nc;
/*
.....Set the default file name
*/
#ifdef _UNICODE	
	WCHAR wname[UX_MAX_PATH_LEN];
	nc = ((CWnd*)GetDlgItem(IDC_DISP_DRAW))->GetWindowText(wname,UX_MAX_PATH_LEN);
	if(nc>0)
		wcstombs(name, wname, nc);
	name[nc] = '\0';
#else
	nc = ((CWnd*)GetDlgItem(IDC_DISP_DRAW))->GetWindowText(name,UX_MAX_PATH_LEN);
#endif
	if (nc == 0) 
		name[0] = '\0';
/*
.....Display the browser
*/
/*	LPCTSTR filter = _T("Tool Drawing File (*.dw)|*.dw|All Files (*.*)|*.*||");		
	tool_mfopt_filename(NULL, "Input Tool Symbol", filter,
			".","NCL_TOOL_DRAWING",name, &nc, 0);
*/
	tool_get_filename(this, "Input Tool Symbol", "*.dw", name, &nc, "Tool Drawing File (*.dw)", 0,
				"NCL_TOOL_DRAWING", "System");
/*
.....Store the symbol name
*/
	if (nc!=0)
	{
		tool_get_fname(name,fname);
		nc = strlen(fname);
		if (nc != 0)
		{
#ifdef _UNICODE	
			WCHAR *wfname;
			int len = strlen (fname) + 1;
			wfname = new WCHAR[len];
			int wlen = MultiByteToWideChar(CP_ACP, 0, fname, -1, 
							wfname, len);
			((CEdit*)GetDlgItem(IDC_DISP_DRAW))->SetWindowText(wfname);
			delete wfname;
#else
			((CEdit*)GetDlgItem(IDC_DISP_DRAW))->SetWindowText(fname);
#endif
			((CEdit*)GetDlgItem(IDC_DISP_DRAW))->SetFocus();
			((CEdit*)GetDlgItem(IDC_DISP_DRAW))->SetSel(nc, nc, FALSE);
		}
	}
}

/***********************************************************************
c
c   FUNCTION: OnSymbol()
c
c          Function called when "Symbol" check box is changed.
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView2::OnSymbol()
{
	if (((CButton*)GetDlgItem(IDC_SYMBOL_CK))->GetCheck()==1)
	{
		((CWnd*)GetDlgItem(IDC_SYMBOL_BUTTON))->EnableWindow(TRUE);
		m_symbol_ck = 1;
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_SYMBOL_BUTTON))->EnableWindow(FALSE);
		m_symbol_ck = 0;
/*
.....clear up the saved symbol value
*/
		symbol_value.m_class = "All";
		symbol_value.m_value1 = 0.0;
		symbol_value.m_value2 = 0.0;
		symbol_value.m_value3 = 0.0;
		symbol_value.m_value4 = 0.0;
		symbol_value.m_parm1 = "";
		symbol_value.m_parm2 = "";
		symbol_value.m_parm3 = "";
		symbol_value.m_parm4 = "";
		symbol_value.m_symbol = "";
		symbol_value.m_shade = "Off";
		SetSymbol(_T(""), 1);
	}
}


/***********************************************************************
c
c   FUNCTION: OnShank()
c
c          Function called when "Shank" check box is changed.
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView2::OnShank()
{
	if (((CButton*)GetDlgItem(IDC_SHANK_CK))->GetCheck()==1)
	{
		((CWnd*)GetDlgItem(IDC_SHANK_BUTTON))->EnableWindow(TRUE);
		m_shank_ck = 1;
		if (Tool_current_data.fshank)
			m_shank_ck = Tool_current_data.fshank;
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_SHANK_BUTTON))->EnableWindow(FALSE);
		m_shank_ck = 0;
/*
.....clear up the saved shank value
*/
		shank_value.m_class = "All";
		shank_value.m_value1 = 0.0;
		shank_value.m_value2 = 0.0;
		shank_value.m_value3 = 0.0;
		shank_value.m_value4 = 0.0;
		shank_value.m_parm1 = "";
		shank_value.m_parm2 = "";
		shank_value.m_parm3 = "";
		shank_value.m_parm4 = "";
		shank_value.m_symbol = "";
		shank_value.m_shade = "Off";
		SetSymbol(_T(""), 2);
	}
}

/***********************************************************************
c
c   FUNCTION: OnHolder()
c
c          Function called when "Holder" check box is changed.
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView2::OnHolder()
{
	if (((CButton*)GetDlgItem(IDC_HOLDER_CK))->GetCheck()==1)
	{
		((CWnd*)GetDlgItem(IDC_HOLDER_BUTTON))->EnableWindow(TRUE);
		m_holder_ck = 1;
	}
	else
	{
		m_holder_ck = 0;
		((CWnd*)GetDlgItem(IDC_HOLDER_BUTTON))->EnableWindow(FALSE);
/*
.....clear up the saved symbol value
*/
		holder_value.m_class = "All";
		holder_value.m_value1 = 0.0;
		holder_value.m_value2 = 0.0;
		holder_value.m_value3 = 0.0;
		holder_value.m_value4 = 0.0;
		holder_value.m_parm1 = "";
		holder_value.m_parm2 = "";
		holder_value.m_parm3 = "";
		holder_value.m_parm4 = "";
		holder_value.m_symbol = "";
		holder_value.m_shade = "Off";
		SetSymbol(_T(""), 3);
	}
}
/***********************************************************************
c
c   FUNCTION: OnSymbol_disp()
c
c          Function called when "Symbol" button is pushed.
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView2::OnSymbol_disp()
{
	OnToolDisplay(1);
}


/***********************************************************************
c
c   FUNCTION: OnShank_disp()
c
c          Function called when "Shank" button is pushed.
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView2::OnShank_disp()
{
	OnToolDisplay(2);
}

/***********************************************************************
c
c   FUNCTION: OnHolder_disp()
c
c          Function called when "Holder" button is pushed.
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView2::OnHolder_disp()
{
	OnToolDisplay(3);
}

/***********************************************************************
c
c   FUNCTION: OnToolDisplay(flag)
c
c          Display a "Tool Display" form to accept tool display data 
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView2::OnToolDisplay(int flag)
{
/*
......save value into tool_data first
*/
	TOOL_main_frame->save_tool_data();
	if (flag==1)
	{
		if (symbol_value.IsCreated()==0)
		{
			symbol_value.SetParent(this,1);
			symbol_value.Create(IDD_TOOL_DISPLAY);
		}
		symbol_value.ShowWindow(SW_SHOW);
	}
	else if (flag==2)
	{
		if (shank_value.IsCreated()==0)
		{
			shank_value.SetParent(this,2);
			shank_value.Create(IDD_TOOL_DISPLAY);
		}
		shank_value.ShowWindow(SW_SHOW);
	}
	else if (flag==3)
	{
		if (holder_value.IsCreated()==0)
		{
			holder_value.SetParent(this,3);
			holder_value.Create(IDD_TOOL_DISPLAY);
		}
		holder_value.ShowWindow(SW_SHOW);
	}
}

void CChildView2::load_cutter_disp()
{
	if (symbol_value.IsCreated()!=0)
	{
		if (symbol_value.IsWindowVisible())
		{
			symbol_value.load_tool_symbols();
			symbol_value.load_tool_prof();
		}
	}
	if (shank_value.IsCreated()!=0)
	{
		if (shank_value.IsWindowVisible())
		{
			shank_value.load_tool_symbols();
			shank_value.load_tool_prof();
		}
	}
	if (holder_value.IsCreated()!=0)
	{
		if (holder_value.IsWindowVisible())
		{
			holder_value.load_tool_symbols();
			holder_value.load_tool_prof();
		}
	}
}
void CChildView2::adjust_ddata_field()
{
	if (symbol_value.IsCreated()!=0)
	{
		if (symbol_value.IsWindowVisible())
		{
			symbol_value.adjust_data_field();
		}
	}
	if (shank_value.IsCreated()!=0)
	{
		if (shank_value.IsWindowVisible())
		{
			shank_value.adjust_data_field();
		}
	}
	if (holder_value.IsCreated()!=0)
	{
		if (holder_value.IsWindowVisible())
		{
			holder_value.adjust_data_field();
		}
	}
}

/***********************************************************************
c
c   FUNCTION: SetSymbol(CString sym, int type)
c
c          Set the 'symbol' text field text and tool symbol value
c
c   INPUT:  sym: symbol text to be set
c			type: 1: cutter symbol
c					2: shank symbol
c					3: holder symbol
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CChildView2::SetSymbol(CString sym, int type)
{
#ifdef _UNICODE	
	char tmpstr[81];
	int len = sym.GetLength();
	WCHAR *wsym = sym.GetBuffer(len);
	if (len>80) len = 80;
	if (len>0)
		wcstombs(tmpstr, wsym, len);
	else
		tmpstr[0] = '\0';
	tmpstr[len] = '\0';
	if (type==1)
	{
		((CWnd*)GetDlgItem(IDC_SYLBOL_NAME))->SetWindowText(sym);
		strcpy (Tool_current_data.symbol, tmpstr);
	}
	else if (type==2)
	{
		((CWnd*)GetDlgItem(IDC_SHANK_NAME))->SetWindowText(sym);
		strcpy (Tool_current_data.symshk, tmpstr);
	}
	else if (type==3)
	{
		((CWnd*)GetDlgItem(IDC_HOLDER_NAME))->SetWindowText(sym);
		strcpy (Tool_current_data.symhld, tmpstr);
	}
#else
	if (type==1)
	{
		((CWnd*)GetDlgItem(IDC_SYLBOL_NAME))->SetWindowText(sym);
		strcpy (Tool_current_data.symbol, sym);
	}
	else if (type==2)
	{
		((CWnd*)GetDlgItem(IDC_SHANK_NAME))->SetWindowText(sym);
		strcpy (Tool_current_data.symshk, sym);
	}
	else if (type==3)
	{
		((CWnd*)GetDlgItem(IDC_HOLDER_NAME))->SetWindowText(sym);
		strcpy (Tool_current_data.symhld, sym);
	}
#endif
}
/***********************************************************************
c
c   FUNCTION: SetShade(int pos)
c
c          Set the 'shade' choice
c
c   INPUT:  pos: shade to be set
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CChildView2::SetShade(int pos)
{
	((CComboBox*)GetDlgItem(IDC_SHADE))->SetCurSel(pos);
	OnShdchg();
}
/***********************************************************************
c
c   SUBROUTINE:  OnCtlColor() 
c
c   FUNCTION:  This function called when a child control 
c				is about to be drawn. We use override this
c				method to change background color oof a control
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
HBRUSH CChildView2::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	CWnd *wnd;
	switch (nCtlColor)
	{
/*
......Readonly Edit control is treated 
......as static label. Set this Edit control's 
......background and text color
*/
	case CTLCOLOR_STATIC: 
		wnd = (CWnd*)(GetDlgItem(IDC_SYLBOL_NAME));
		if (wnd->m_hWnd==pWnd->m_hWnd)
		{
			pDC->SetTextColor(RGB(20, 20, 20));
			pDC->SetBkColor(RGB(255, 255, 255));
			return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
		}
		wnd = (CWnd*)(GetDlgItem(IDC_SHANK_NAME));
		if (wnd->m_hWnd==pWnd->m_hWnd)
		{
			pDC->SetTextColor(RGB(20, 20, 20));
			pDC->SetBkColor(RGB(255, 255, 255));
			return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
		}
		wnd = (CWnd*)(GetDlgItem(IDC_HOLDER_NAME));
		if (wnd->m_hWnd==pWnd->m_hWnd)
		{
			pDC->SetTextColor(RGB(20, 20, 20));
			pDC->SetBkColor(RGB(255, 255, 255));
			return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
		}
		return CFormView::OnCtlColor(pDC, pWnd, nCtlColor);
	default:
		return CFormView::OnCtlColor(pDC, pWnd, nCtlColor);
	}
}

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// CChildView3

IMPLEMENT_DYNCREATE(CChildView3, CFormView)

/***********************************************************************
c
c   FUNCTION: CChildView3()
c
c              Constructor of class CChildView3
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CChildView3::CChildView3()
	: CFormView(CChildView3::IDD)
{
	//{{AFX_DATA_INIT(CChildView3)
	//}}AFX_DATA_INIT
/*
......Create font
*/	
#ifdef _UNICODE	
	WCHAR wfont[80];
	int len1 = strlen (Tool_font_name3) + 1;
	int wlen1 = MultiByteToWideChar(CP_ACP, 0, Tool_font_name3, -1, 
							wfont, len1);
	m_fntPage.CreatePointFont(Tool_font_size3, wfont);
#else
	m_fntPage.CreatePointFont(Tool_font_size3, Tool_font_name3);
#endif
	m_cuttype = 0;
}

CChildView3::~CChildView3()
{
}

void CChildView3::OnSize(UINT nType, int cx, int cy) 
{
	CFormView::OnSize(nType, cx, cy);
	if (IsWindow(m_listctl.m_hWnd)==false)
		return;
	CRect rect;
	GetWindowRect(&rect);
	m_listctl.GetWindowRect(&rect);
	ScreenToClient(&rect);
//	rect.bottom = rect.top + cy;
//	rect.right = rect.left + cx;
//	MoveWindow(rect); //movewindow will cause the recursive call onsize
	m_listctl.SetWindowPos(NULL,rect.left,rect.top,cx-30,cy-30,SWP_NOACTIVATE|SWP_NOZORDER);
}

/***********************************************************************
c
c   FUNCTION: OnInitialUpdate()
c
c              Called by the framework before the view is initially displayed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CChildView3::OnInitialUpdate()
{
	CFormView::OnInitialUpdate();
	ChangeDialogFont(this, &m_fntPage);

	CWnd *cwin = GetDlgItem(IDC_TOOL_LIST);
	if (cwin == NULL)
		return;
	CRect rbtn;
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);

	DWORD dwStyle = WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL | WS_HSCROLL | WS_VSCROLL;
	
	ScreenToClient(&rbtn);
	m_listctl.SetParent(this);
	m_listctl.SetFrmFld(0,0);
	m_listctl.Create(dwStyle, rbtn, this, IDC_TOOL_TLIST);
	m_listctl.ShowWindow(SW_SHOW);
/*	
	HBITMAP defhBmap;
	CBitmap *defImage;
	CBitmap defcBmap;
	int defImgnum[8];
	HDC thdc = TOOL_main_frame->GetDC()->m_hDC;
	m_rimglist = new CImageList();
	m_rimglist->Create(16,16, ILC_COLOR16|ILC_MASK, 0, 100);
	defhBmap = uw_get_bitmap("check", thdc, 0);
	defImage = defcBmap.FromHandle(defhBmap);
	defImgnum[0] = m_rimglist->Add(defImage, 0xC0C0C0);
    m_listctl.SetImageList(m_rimglist, LVSIL_SMALL); 
*/	
	LVCOLUMN lvColumn;
/*
.....the first column of HDF_SPLITBUTTON type is not working, so I have to
.....add a dummy column first, then delete it in order for making HDF_SPLITBUTTON
.....type working for column 0
*/
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON;
	lvColumn.iImage = -1;
	lvColumn.cx = 10;
	lvColumn.pszText = _T("dummy");
	m_listctl.InsertColumn(0, &lvColumn);
/*
......first column, toolno
*/
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
//	lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON | LVCFMT_COL_HAS_IMAGES;
//	lvColumn.iImage = 0;
	lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON ;
	lvColumn.iImage = -1;
	lvColumn.cx = 100;
	lvColumn.pszText = _T("Toolno");
	m_listctl.InsertColumn(1, &lvColumn);
/*
......second column, tooltype
*/
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON;
	lvColumn.iImage = -1;
	lvColumn.cx = 100;
	lvColumn.pszText = _T("Type");
	m_listctl.InsertColumn(2, &lvColumn);
/*
......third column, description
*/
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON;
	lvColumn.iImage = -1;
	lvColumn.cx = 100;
	lvColumn.pszText = _T("Description");
	m_listctl.InsertColumn(3, &lvColumn);
/*
......Forth column, Diameter
*/
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON;
	lvColumn.iImage = -1;
	lvColumn.cx = 100;
	lvColumn.pszText = _T("Diameter");
	m_listctl.InsertColumn(4, &lvColumn);
/*
......Fifth column, Radius
*/
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON;
	lvColumn.iImage = -1;
	lvColumn.cx = 100;
	lvColumn.pszText = _T("Radius");
	m_listctl.InsertColumn(5, &lvColumn);
/*
......Sixth column, Height
*/
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON;
	lvColumn.iImage = -1;
	lvColumn.cx = 100;
	lvColumn.pszText = _T("Height");
	m_listctl.InsertColumn(6, &lvColumn);
/*
......Seventh column, Angle
*/
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON;
	lvColumn.iImage = -1;
	lvColumn.cx = 100;
	lvColumn.pszText = _T("Angle");
	m_listctl.InsertColumn(7, &lvColumn);
/*
.....delete dummy column
*/
	m_listctl.DeleteColumn(0);

	UD_TLIST tool_list;

	tool_list.num_item = ncl_get_tool_tlist(&tool_list, -1);
	tool_list.answer = 0;
	tool_list.sort = -1;
	m_listctl.SetTlist(&tool_list);

#ifdef _UNICODE	
	WCHAR *wtmp;
	int len, wlen;
	for(int j=0;j<tool_list.num_item;j++)
	{
		tool_list.data[j].fldno = -1;
		tool_list.data[j].frmid = -1;
		len = strlen (tool_list.data[j].data_items[0]) + 1;
		wtmp = new WCHAR[len];
		wlen = MultiByteToWideChar(CP_ACP, 0, tool_list.data[j].data_items[0], -1, 
					wtmp, len);
		m_listctl.InsertItem(j,  wtmp);
		delete wtmp;
		for (int m=1; m<tool_list.data[j].itemnum;m++)
		{
			len = strlen (tool_list.data[j].data_items[m]) + 1;
			wtmp = new WCHAR[len];
			wlen = MultiByteToWideChar(CP_ACP, 0, tool_list.data[j].data_items[m], -1, 
					wtmp, len);
			m_listctl.SetItemText(j, m, wtmp);
			delete wtmp;
		}
		m_listctl.SetItemData(j, (LPARAM)&(tool_list.data[j]));
	}
#else
	for(int j=0;j<tool_list.num_item;j++)
	{
		tool_list.data[j].fldno = -1;
		tool_list.data[j].frmid = -1;
		m_listctl.InsertItem(j,  tool_list.data[j].data_items[0]);
		for (int m=1; m<tool_list.data[j].itemnum;m++)
		{
			m_listctl.SetItemText(j, m, tool_list.data[j].data_items[m]);
		}
		m_listctl.SetItemData(j, (LPARAM)&(tool_list.data[j]));
	}
/*
......have to set the focus to set a selection
*/
#endif
	m_listctl.SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES);
	m_listctl.SetFocus();
	if ((tool_list.answer>=0)&&(tool_list.answer<tool_list.num_item))
	{
		m_listctl.SetItemState (tool_list.answer, LVIS_SELECTED, LVIS_SELECTED);
		m_listctl.EnsureVisible(tool_list.answer, FALSE);
	}
	TOOL_main_frame->AdjectWindowSize();
	ud_free_tlist(&tool_list);
}

/*******************************************************************
**   E_FUNCTION : tool_deleteone(toolno)
**              This function delete a toolno
**   PARAMETERS
**       INPUT  : 
**			toolno: toolno to delete
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
//ok
void CChildView3::tool_deleteone(double toolno)
{
	char toolstr[80];
	sprintf(toolstr, "%f", toolno);
	m_listctl.DeleteColumnItem(0, toolstr, 0);
	OnSelList();
}
/***********************************************************************
c
c   FUNCTION: UpdateList(int toolnum, int pos)
c
c          This function update the list in the list box
c			and set the default selecting
c
c   INPUT:  
c			toolnum: default selecting tool number if != -1
c			pos:     if toolnum==-1 and pos != -1, default
c					selecting position #pos, if toolnum==-1 
c					and pos == -1, select position 0
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView3::UpdateList(double toolnum, int pos)
{
/*
......don't update list selecting unless we do OnSelList();
*/
	S_select_update = 0;
	m_listctl.DeleteAllItems();

	UD_TLIST tool_list;
	tool_list.num_item = ncl_get_tool_tlist(&tool_list, -1);
	tool_list.answer = 0;
	tool_list.sort = -1;

	for(int j=0;j<tool_list.num_item;j++)
	{
		tool_list.data[j].fldno = -1;
		tool_list.data[j].frmid = -1;
		if ((toolnum!=-1)&&(toolnum==atoi(tool_list.data[j].data_items[0])))
		{
			pos = j;
			tool_list.answer = pos;
		}
#ifdef _UNICODE	
		WCHAR *wtmp;
		int len, wlen;
		len = strlen (tool_list.data[j].data_items[0]) + 1;
		wtmp = new WCHAR[len];
		wlen = MultiByteToWideChar(CP_ACP, 0, tool_list.data[j].data_items[0], -1, 
					wtmp, len);
		m_listctl.InsertItem(j,  wtmp);
		delete wtmp;
		for (int m=1; m<tool_list.data[j].itemnum;m++)
		{
			len = strlen (tool_list.data[j].data_items[m]) + 1;
			wtmp = new WCHAR[len];
			wlen = MultiByteToWideChar(CP_ACP, 0, tool_list.data[j].data_items[m], -1, 
					wtmp, len);
			m_listctl.SetItemText(j, m, wtmp);
			delete wtmp;
		}
//the ITEMDATA we will use for sort later, so can't used local value
//		m_listctl.SetItemData(j, (LPARAM)&(tool_list.data[j]));
#else
		m_listctl.InsertItem(j,  tool_list.data[j].data_items[0]);
		for (int m=1; m<tool_list.data[j].itemnum;m++)
		{
			m_listctl.SetItemText(j, m, tool_list.data[j].data_items[m]);
		}
		m_listctl.SetItemData(j, (LPARAM)&(tool_list.data[j]));
#endif
	}
	m_listctl.SetTlist(&tool_list);
	ud_free_tlist(&tool_list);

	UD_TLIST *cur_list = m_listctl.GetCurrentListPt();
	for(int j=0;j<cur_list->num_item;j++)
	{
		cur_list->data[j].frmid = 0;
		cur_list->data[j].fldno = 0;
		m_listctl.SetItemData(j, (LPARAM)&(cur_list->data[j]));
	}
	m_listctl.SetFocus();
	if ((cur_list->answer>=0)&&(cur_list->answer<cur_list->num_item))
	{
		m_listctl.SetItemState (cur_list->answer, LVIS_SELECTED, LVIS_SELECTED);
		m_listctl.EnsureVisible(cur_list->answer, FALSE);
	}
	m_listctl.m_selitem = cur_list->answer;
	S_select_update = 1;
	OnSelList();
}

/***********************************************************************
c
c   FUNCTION: UpdateTool(toolnum)
c
c          This function update tool in the list box
c			and set the default selecting
c
c   INPUT:  
c			toolnum: selecting tool number
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
//ok
void CChildView3::UpdateTool(TL_tooldata_rec tool_data)
{
	char toolstr[80];
	sprintf(toolstr, "%f", tool_data.toolno);
	int current_pos = m_listctl.FindColumnItemMatch1(0, toolstr, 0);
	int orignal_pos = m_listctl.FindColumnItemMatch2(0, toolstr, 0);

	UD_ITEMDATA data;
	data.itemnum = 7;
	data.data_items = (char **) uu_malloc(data.itemnum*sizeof(char*));

	int len;
	char buf[81];
	UD_LIST type_list;
	int ctype = tool_data.ctype;
/*
......data_items[0] for Toolno
*/
	sprintf (buf, "%-15.0f", tool_data.toolno);
	len = strlen(buf);
	data.data_items[0] = (char*)uu_malloc((len+1)*sizeof(char));
	strcpy(data.data_items[0], buf);
/*
......data_items[2] for Description
*/
	len = strlen(tool_data.description);
	data.data_items[2] = (char*)uu_malloc((len+1)*sizeof(char));
	strcpy(data.data_items[2], tool_data.description);
/*
......data_items[1] for type
*/
	type_list.item = ncl_get_tool_type(&(type_list.num_item), 0);
	if (type_list.num_item>0)
	{
		strcpy (buf, type_list.item[ctype]);
	}
	else
		strcpy (buf, " ");
	len = strlen(buf);
	data.data_items[1] = (char*)uu_malloc((len+1)*sizeof(char));
	strcpy(data.data_items[1], buf);
/*
......data_items[3] for Diameter
*/	
	if (tool_data.cutter[0]==0.0)
		buf[0] = '\0';
	else
		sprintf(buf, "%f", tool_data.cutter[0]);
	len = strlen(buf);
	data.data_items[3] = (char*)uu_malloc((len+1)*sizeof(char));
	strcpy(data.data_items[3], buf);
/*
......data_items[4] for Radius
*/	
	if (tool_data.cutter[1]==0.0)
		buf[0] = '\0';
	else
		sprintf(buf, "%f", tool_data.cutter[1]);
	len = strlen(buf);
	data.data_items[4] = (char*)uu_malloc((len+1)*sizeof(char));
	strcpy(data.data_items[4], buf);
/*
......data_items[5] for Height
*/	
	if (tool_data.cutter[2]==0.0)
		buf[0] = '\0';
	else
		sprintf(buf, "%f", tool_data.cutter[2]);
	len = strlen(buf);
	data.data_items[5] = (char*)uu_malloc((len+1)*sizeof(char));
	strcpy(data.data_items[5], buf);
/*
......data_items[6] for Angle
*/	
	if (tool_data.cutter[3]==0.0)
		buf[0] = '\0';
	else
		sprintf(buf, "%f", tool_data.cutter[3]);
	len = strlen(buf);
	data.data_items[6] = (char*)uu_malloc((len+1)*sizeof(char));
	strcpy(data.data_items[6], buf);
	m_listctl.UpdateItemValue(current_pos, orignal_pos, &data);
	ud_tlist_free_idata(&data);
}


/***********************************************************************
c
c   FUNCTION: GetSelPos()
c
c          This function Get the selct position
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
//ok
int CChildView3::GetSelPos(double *toolnum)
{
	*toolnum = -1;
	int pos = m_listctl.m_selitem;
	UD_TLIST tmplist;
	if (m_listctl.m_selitem>=0)
	{
		m_listctl.GetCurrentTlist(&tmplist);
		if ((tmplist.num_item>0)&&(tmplist.data!=NULL))
			*toolnum = atof(tmplist.data[pos].data_items[0]);
	}
	return pos;
}

/***********************************************************************
c
c   FUNCTION: SetSelPos(int pos)
c
c          This function Set the selct position
c
c   INPUT:  pos:  position to set (from original toolib, not filter yet)
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CChildView3::SetSelPos(int pos)
{
//pos return current position.
	pos = m_listctl.SetSelPosOrig(pos);
	TOOL_main_frame->set_current_toolpos(pos);
	OnSelList();
}
/***********************************************************************
c
c   FUNCTION: OnUpdate(CView*, LPARAM, CObject*)
c
c          Called by the framework after the view's document 
c			has been modified; this function is 
c			called by CDocument::UpdateAllViews
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CChildView3::OnUpdate(CView*, LPARAM, CObject*)
{
	CToolibDoc* pDoc = GetDocument();

	UpdateData(FALSE);  // set the data into the controls
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

void CChildView3::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CChildView3)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CChildView3, CFormView)
	//{{AFX_MSG_MAP(CChildView3)
	ON_WM_SIZE()
	ON_NOTIFY(NM_CLICK, IDC_TOOL_TLIST, OnToolSelected)
	ON_NOTIFY(NM_DBLCLK, IDC_TOOL_TLIST, OnToolSelected)	
	ON_NOTIFY(LVN_ITEMCHANGED, IDC_TOOL_TLIST, OnToolSelected)
	ON_NOTIFY(LVN_COLUMNCLICK, IDC_TOOL_TLIST, OnToolColumnSelected)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CChildView3 message handlers

void CChildView3::OnToolColumnSelected(NMHDR *pNMHDR, LRESULT *pResult)
{
	NMLISTVIEW *pLV = (NMLISTVIEW *) pNMHDR;	
	if ((pLV->iSubItem==0)&&(toolno_click%2==0))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc, pLV->iSubItem);
		toolno_click++;
	}
	else if ((pLV->iSubItem==0)&&(toolno_click%2))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc2, pLV->iSubItem);
		toolno_click--;
	}
	if ((pLV->iSubItem==1)&&(type_click%2==0))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc, pLV->iSubItem);
		type_click++;
	}
	else if ((pLV->iSubItem==1)&&(type_click%2))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc2, pLV->iSubItem);
		type_click--;
	}
	if ((pLV->iSubItem==2)&&(descript_click%2==0))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc, pLV->iSubItem);
		descript_click++;
	}
	else if ((pLV->iSubItem==2)&&(descript_click%2))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc2, pLV->iSubItem);
		descript_click--;
	}
	if ((pLV->iSubItem==3)&&(diam_click%2==0))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc, pLV->iSubItem);
		diam_click++;
	}
	else if ((pLV->iSubItem==3)&&(diam_click%2))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc2, pLV->iSubItem);
		diam_click--;
	}
	if ((pLV->iSubItem==4)&&(rad_click%2==0))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc, pLV->iSubItem);
		rad_click++;
	}
	else if ((pLV->iSubItem==4)&&(rad_click%2))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc2, pLV->iSubItem);
		rad_click--;
	}
	if ((pLV->iSubItem==5)&&(hgt_click%2==0))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc, pLV->iSubItem);
		hgt_click++;
	}
	else if ((pLV->iSubItem==5)&&(hgt_click%2))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc2, pLV->iSubItem);
		hgt_click--;
	}
	if ((pLV->iSubItem==6)&&(angle_click%2==0))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc, pLV->iSubItem);
		angle_click++;
	}
	else if ((pLV->iSubItem==6)&&(angle_click%2))
	{
		m_listctl.SortItems((PFNLVCOMPARE)SortFunc2, pLV->iSubItem);
		angle_click--;
	}
}

//OK
void CChildView3::OnToolSelected(NMHDR *pNMHDR, LRESULT *pResult)
{
	if (S_select_update==0)
		return;
	if (m_listctl.m_update==0)
		return;
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
    if (  ( pNMListView->uOldState & LVIS_SELECTED ) && 
         ( pNMListView->uNewState == 0 )  )
    {
		if ((m_listctl.m_selitem==pNMListView->iItem)&&(m_listctl.m_change==0))
		{
			m_listctl.SetItemState(pNMListView->iItem, 0, LVIS_SELECTED | LVIS_FOCUSED);	
			m_listctl.m_selitem = -1;
			*pResult = 1;
			return;
		}
	}
	m_listctl.m_change = 0;
	m_listctl.m_selitem = pNMListView->iItem;
	OnSelList();
}
/***********************************************************************
c
c   FUNCTION: OnSelList()
c
c          This function called when select a tool in the list box
c
c   INPUT:  None
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
//ok
void CChildView3::OnSelList()
{
	int pos = m_listctl.m_selitem;
	TOOL_main_frame->set_current_toolpos(pos);	
	SelListPos(pos);
}
//ok
/***********************************************************************
c
c   FUNCTION: SelListPos(int pos)
c
c          This function update all view according to list position
c
c   INPUT:  pos:  position to set
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CChildView3::SelListPos(int pos)
{
	CToolibDoc* pDoc = GetDocument();
	char selstr[256], *tok, tempstr[80];
	UD_TLIST tmplist;
	m_listctl.GetCurrentTlist(&tmplist);
	double tool = 1;
	if ((tmplist.num_item>0)&&(tmplist.data!=NULL))
		tool = atof(tmplist.data[pos].data_items[0]);
/*
.....Tool does not exist in tool library so no changes should be
.....made to the display.
.......This prevents memory problems since the command, plabel and
.......loadtl will not be reallocated if no match is found in the
.......call to ncl_get_tooldata. Andrew - 11/16/12
*/
	if (tool <= 0.) return;
	if (Tool_current_data.command!=NULL)
		uu_free (Tool_current_data.command);
	if (Tool_current_data.plabel!=NULL)
		uu_free (Tool_current_data.plabel);
	if (Tool_current_data.loadtl!=NULL)
		uu_free ((char*)Tool_current_data.loadtl);
	ncl_get_tooldata(tool, &Tool_current_data);
	sprintf(tempstr, "%-15.0f", Tool_current_data.toolno);
	pDoc->m_tool = tempstr;
	pDoc->m_desp = Tool_current_data.description;
/*
.....cutter
*/
	ncl_sprintf(tempstr, &(Tool_current_data.cutter[0]), 1);
	pDoc->m_def1 = tempstr;
	if (Tool_current_data.cparms[0]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.cparms[0]);
	pDoc->m_parm1 = tempstr;

	ncl_sprintf(tempstr, &(Tool_current_data.cutter[1]), 1);
	pDoc->m_def2 = tempstr;
	if (Tool_current_data.cparms[1]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.cparms[1]);
	pDoc->m_parm2 = tempstr;

	ncl_sprintf(tempstr, &(Tool_current_data.cutter[2]), 1);
	pDoc->m_def3 = tempstr;
	if (Tool_current_data.cparms[2]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.cparms[2]);
	pDoc->m_parm3 = tempstr;
	
	ncl_sprintf(tempstr, &(Tool_current_data.cutter[3]), 1);
	pDoc->m_def4 = tempstr;
	if (Tool_current_data.cparms[3]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.cparms[3]);
	pDoc->m_parm4 = tempstr;

	ncl_sprintf(tempstr, &(Tool_current_data.cutter[4]), 1);
	pDoc->m_def5 = tempstr;
	if (Tool_current_data.cparms[4]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.cparms[4]);
	pDoc->m_parm5 = tempstr;

	ncl_sprintf(tempstr, &(Tool_current_data.cutter[5]), 1);
	pDoc->m_def6 = tempstr;
	if (Tool_current_data.cparms[5]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.cparms[5]);
	pDoc->m_parm6 = tempstr;
/*
.....Pseudo
*/
	ncl_sprintf(tempstr, &(Tool_current_data.pseudo[0]), 1);
	pDoc->m_cdef1 = tempstr;
	if (Tool_current_data.dparms[0]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.dparms[0]);
	pDoc->m_cparm1 = tempstr;
	
	ncl_sprintf(tempstr, &(Tool_current_data.pseudo[1]), 1);
	pDoc->m_cdef2 = tempstr;
	if (Tool_current_data.dparms[1]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.dparms[1]);
	pDoc->m_cparm2 = tempstr;

	ncl_sprintf(tempstr, &(Tool_current_data.pseudo[2]), 1);
	pDoc->m_cdef3 = tempstr;
	if (Tool_current_data.dparms[2]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.dparms[2]);
	pDoc->m_cparm3 = tempstr;

	ncl_sprintf(tempstr, &(Tool_current_data.pseudo[3]), 1);
	pDoc->m_cdef4 = tempstr;
	if (Tool_current_data.dparms[3]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.dparms[3]);
	pDoc->m_cparm4 = tempstr;

	ncl_sprintf(tempstr, &(Tool_current_data.pseudo[4]), 1);
	pDoc->m_cdef5 = tempstr;
	if (Tool_current_data.dparms[4]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.dparms[4]);
	pDoc->m_cparm5 = tempstr;

	ncl_sprintf(tempstr, &(Tool_current_data.pseudo[5]), 1);
	pDoc->m_cdef6 = tempstr;
	if (Tool_current_data.dparms[5]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.dparms[5]);
	pDoc->m_cparm6 = tempstr;
	
	pDoc->m_segment = Tool_current_data.segments;
	pDoc->m_moving = Tool_current_data.move;
	pDoc->m_holder_ck = Tool_current_data.fholder;
	pDoc->m_tooldraw = Tool_current_data.drawing;
	pDoc->m_com[0] = Tool_current_data.major;

	for (int i=0, j=1; i<20; i++,j++)
	{
		if (i<Tool_current_data.no_loadtl)
		{
			pDoc->m_com[j++] = Tool_current_data.loadtl[i].value;
			if (Tool_current_data.loadtl[i].parm==0)
				tempstr[0] = '\0';
			else if (Tool_current_data.loadtl[i].parm<0)
				strcpy(tempstr, "=");
			else
				sprintf(tempstr, "%d", Tool_current_data.loadtl[i].parm);
			pDoc->m_com[j] = tempstr;
		}
		else
		{
			pDoc->m_com[j++] = "";
			pDoc->m_com[j] = "";
		}
	}
	pDoc->m_cuttype = Tool_current_data.ctype;
	pDoc->m_def_cut = Tool_current_data.fpseudo;
	pDoc->m_shade = Tool_current_data.shade;
	pDoc->m_loadcom = Tool_current_data.floadtl;

	if (Tool_current_data.fshank)
	{
		pDoc->m_shank_ck = Tool_current_data.fshank;
		pDoc->m_shank = Tool_current_data.symshk;
	}
	else
	{
		pDoc->m_shank = "";
		pDoc->m_shank_ck = 0;
	}
	if (Tool_current_data.fholder)
	{
		pDoc->m_holder_ck = 1;
		pDoc->m_holder = Tool_current_data.symhld;
	}
	else
	{
		pDoc->m_holder = "";
		pDoc->m_holder_ck = 0;
	}
	if (Tool_current_data.symbol[0]!='\0')
	{
		pDoc->m_symbol_ck = 1;
	}
	else
		pDoc->m_symbol_ck = 0;
	pDoc->m_symbol = Tool_current_data.symbol;

	pDoc->symbol_value.m_value1 = Tool_current_data.catt[0];
	pDoc->symbol_value.m_value2 = Tool_current_data.catt[1];
	pDoc->symbol_value.m_value3 = Tool_current_data.catt[2];
	pDoc->symbol_value.m_value4 = Tool_current_data.catt[3];
	if (Tool_current_data.yparms[0]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.yparms[0]);
	pDoc->symbol_value.m_parm1 = tempstr;
	if (Tool_current_data.yparms[1]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.yparms[1]);
	pDoc->symbol_value.m_parm2 = tempstr;
	if (Tool_current_data.yparms[2]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.yparms[2]);
	pDoc->symbol_value.m_parm3 = tempstr;
	if (Tool_current_data.yparms[3]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.yparms[3]);
	pDoc->symbol_value.m_parm4 = tempstr;
	if (Tool_current_data.shade==0)
		pDoc->symbol_value.m_shade = "Default";
	else if (Tool_current_data.shade==1)
		pDoc->symbol_value.m_shade = "On";
	else if (Tool_current_data.shade==2)
		pDoc->symbol_value.m_shade = "Off";

	pDoc->shank_value.m_value1 = Tool_current_data.satt[0];
	pDoc->shank_value.m_value2 = Tool_current_data.satt[1];
	pDoc->shank_value.m_value3 = Tool_current_data.satt[2];
	pDoc->shank_value.m_value4 = Tool_current_data.satt[3];
	if (Tool_current_data.sparms[0]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.sparms[0]);
	pDoc->shank_value.m_parm1 = tempstr;
	if (Tool_current_data.sparms[1]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.sparms[1]);
	pDoc->shank_value.m_parm2 = tempstr;
	if (Tool_current_data.sparms[2]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.sparms[2]);
	pDoc->shank_value.m_parm3 = tempstr;
	if (Tool_current_data.sparms[3]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.sparms[3]);
	pDoc->shank_value.m_parm4 = tempstr;
	if (Tool_current_data.sshade==0)
		pDoc->shank_value.m_shade = "Default";
	else if (Tool_current_data.sshade==1)
		pDoc->shank_value.m_shade = "On";
	else if (Tool_current_data.sshade==2)
		pDoc->shank_value.m_shade = "Off";

	pDoc->holder_value.m_value1 = Tool_current_data.hatt[0];
	pDoc->holder_value.m_value2 = Tool_current_data.hatt[1];
	pDoc->holder_value.m_value3 = Tool_current_data.hatt[2];
	pDoc->holder_value.m_value4 = Tool_current_data.hatt[3];
	if (Tool_current_data.hparms[0]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.hparms[0]);
	pDoc->holder_value.m_parm1 = tempstr;
	if (Tool_current_data.hparms[1]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.hparms[1]);
	pDoc->holder_value.m_parm2 = tempstr;
	if (Tool_current_data.hparms[2]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.hparms[2]);
	pDoc->holder_value.m_parm3 = tempstr;
	if (Tool_current_data.hparms[3]==0)
		tempstr[0] = '\0';
	else
		sprintf(tempstr, "%d", Tool_current_data.hparms[3]);
	pDoc->holder_value.m_parm4 = tempstr;
	if (Tool_current_data.hshade==0)
		pDoc->holder_value.m_shade = "Default";
	else if (Tool_current_data.hshade==1)
		pDoc->holder_value.m_shade = "On";
	else if (Tool_current_data.hshade==2)
		pDoc->holder_value.m_shade = "Off";

	pDoc->UpdateAllViews(this);
	CView2->OnSymbol();
	CView2->OnShank();
	CView2->OnHolder();
	Current_sel_tool = Tool_current_data.toolno;
	CView2->adjust_ddata_field();
}
BOOL CChildView3::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs
//	CToolibApp *app = (CToolibApp *)AfxGetApp();
//	cs.lpszClass = (const char*) (app->m_strMyClassName);

//	SetThemeAppProperties(3);
	return CFormView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
