/************************************************************************
**
**   FILE NAME: wsntmenudsndlg.cpp
**
**	 Description - Functions implementation for
**		CNCLMenuDsnDlg class 
**	 CONTAINS: 
**		all functions declared in wsntmenudsndlg.h
**		uw_ntsave_cur_menudegn()
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntmenudsndlg.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			12/14/15 , 08:56:14
**
************************************************************************
*/
#include "wsntstdafx.h"
#include "wsntres.h"
#include "wsgl.h"
#include "wsntframe.h"
#include "wsntbitmap.h"
#include "wsntmenudsndlg.h"
#include "zkeysym.h"
#include "wsntcfunc.h"
#include "lipv.h"

extern CMainFrame *NCL_MainFrame;
extern "C" char * uu_malloc(int);
extern "C" int uz_which_keydef(char*,int*,int*,short*);
extern "C" void uz_func_call(char*func, char*params);
extern "C" int ncl_filter_str2(char*name, char*filter);
extern "C" int ul_get_flist(char *, char *, char **, int, int);
extern "C" int ux_nxt_file(char **, char *, int);
extern "C" int ud_get_menuinfo(char *, int*, char *);

UINT UW_MENU_FILTER = WM_APP + 100009;
extern "C" int UW_icon_size;
extern "C" int uz_ntstatname_to_num(char *name, char *func, char *descrip, int* num, int *sub);

/////////////////////////////////////////////////////////////////////////////
// CNCLMenuDsnDlg dialog

/***********************************************************************
c
c   SUBROUTINE:  CNCLMenuDsnDlg
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLMenuDsnDlg::CNCLMenuDsnDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CNCLMenuDsnDlg::IDD, pParent)
{
	m_menu_item.toggle_num = 0;
	m_menu_item.toggle = 0;
	m_menu_item.chcfile[0] = '\0';
	m_menu_item.chcdef = 0;
	m_menu_item.name[0] = '\0';
	m_menu_item.file[0] = '\0';
	m_menu_item.statname[0] = '\0';
	m_menu_item.params = (char *)UU_NULL;
	m_menu_item.descrip[0] = '\0';
	m_menu_item.bgcolor[0] = '\0';
	m_menu_item.color[0] = -1;
	m_menu_item.color[1] = -1;
	m_menu_item.color[2] = -1;
	m_menu_item.pos[0] = -1;
	m_menu_item.pos[1] = -1;
	m_menu_item.size[0] = -1;
	m_menu_item.size[0] = -1;
	m_menu_item.bmpfile[0] = '\0';
	m_menu_item.bmpnum = 0;
	m_menu_item.separator = 0;
	m_menu_name[0] = '\0';
	m_func_name[0] = '\0';
	strcpy(m_filter, "*");
	m_rimglist = NULL;
	m_menulist_no = 0;
	m_menu_type = 2;
	m_filt_item = -1;
	m_bSortAscending = FALSE;
	m_sort_col = -1;
	m_filter1[0] = '\0';
	m_filter2[0] = '\0';
	m_filter3[0] = '\0';
}


void CNCLMenuDsnDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNCLMenuDsnDlg)
	DDX_Control(pDX, IDC_TABLE_LIST, m_listctl);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CNCLMenuDsnDlg, CDialog)
	//{{AFX_MSG_MAP(CNCLMenuDsnDlg)
	ON_NOTIFY(NM_CLICK, IDC_TABLE_LIST, OnSelTable)
	ON_NOTIFY(NM_DBLCLK, IDC_TABLE_LIST, OnDBclickTable)	
	ON_NOTIFY(LVN_COLUMNCLICK, IDC_TABLE_LIST, OnColumnclick)
	ON_COMMAND(IDF_FORM_RETURN, OnReturnKey)
	ON_NOTIFY(HDN_ENDTRACK, 0, OnHeadItemEndTrack)
	ON_NOTIFY(NM_CUSTOMDRAW, 0, OnCustomDraw)
	ON_COMMAND(IDC_COMBO1, OnButType)
	ON_CONTROL(CBN_SELCHANGE, IDC_COMBO1, OnButType)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CNCLMenuDsnDlg::OnButType()
{
	int type;
	CComboBox* cmbox = ((CComboBox*)GetDlgItem(IDC_COMBO1));
	if (cmbox==NULL)
		return;
	type = cmbox->GetCurSel();
	if (type==0)
	{
		GetDlgItem(IDC_EDIT5)->EnableWindow(0);
	}
	else
	{
		GetDlgItem(IDC_EDIT5)->EnableWindow(1);
	}
}
/***********************************************************************
c
c   FUNCTION: OnHeadItemEndTrack (NMHDR* pNMHDR, LRESULT* pResult) 
c
c       Callback function for end of adjusting header item
c
c   INPUT:  pNMHDR: data structure include header ajust information
c
c   OUTPUT : 
c			pResult:  
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::OnHeadItemEndTrack (NMHDR* pNMHDR, LRESULT* pResult)
{
	NMHEADER *pHdr = (HD_NOTIFY*)pNMHDR;
	m_listctl.OnHeadItemEndTrack(pNMHDR, pResult);
	RedrawWindow();
	UpdateWindow();
	*pResult = 0;
}
/***********************************************************************
c
c   FUNCTION: OnCustomDraw (NMHDR* pNMHDR, LRESULT* pResult)
c		Custom draw for List control
c
c   INPUT:  pNMHDR: list control data information
c
c   OUTPUT :   
c			pResult:  
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::OnCustomDraw (NMHDR* pNMHDR, LRESULT* pResult)
{
	NMHEADER *pHdr = (HD_NOTIFY*)pNMHDR;
	*pResult = 0;
}

/***********************************************************************
c
c   FUNCTION: MenuSortFunc1(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
c
c       Sort the list on control list form in partical order
c
c   INPUT:  lParam1: data number 1
c			lParam2:  data number 1
c			lParamSort: sort field number
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CALLBACK MenuSortFunc1(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	char typstr1[20], typstr2[20];
	UZ_keycalls* pData1 = (UZ_keycalls*)lParam1;
	UZ_keycalls* pData2 = (UZ_keycalls*)lParam2;
	int nRetVal = 0;
	if ((pData1==NULL)||(pData2==NULL))
		return 0;
	switch(lParamSort)
	{
	case 2:
		nRetVal = strcmp(pData1->name,
                                 pData2->name);
		break;
	case 3:	
		if (pData1->type==DASKEY) 
		{
			strcpy(typstr1, "DASKEY");
		}
		else if (pData1->type==NISKEY) 
		{
			strcpy(typstr1, "NISKEY");
		}
		else if (pData1->type==NCLKEY) 
		{
			strcpy(typstr1, "NCLKEY");
		}
		else if (pData1->type==CAMKEY) 
		{
			strcpy(typstr1, "CAMKEY");
		}
		else if (pData1->type==CADKEY) 
		{
			strcpy(typstr1, "CADKEY");
		}
		else if (pData1->type==IPVKEY) 
		{
			strcpy(typstr1, "IPVKEY");
		}

		if (pData2->type==DASKEY) 
		{
			strcpy(typstr2, "DASKEY");
		}
		else if (pData2->type==NISKEY) 
		{
			strcpy(typstr2, "NISKEY");
		}
		else if (pData2->type==NCLKEY) 
		{
			strcpy(typstr2, "NCLKEY");
		}
		else if (pData2->type==CAMKEY) 
		{
			strcpy(typstr2, "CAMKEY");
		}
		else if (pData2->type==CADKEY) 
		{
			strcpy(typstr2, "CADKEY");
		}
		else if (pData2->type==IPVKEY) 
		{
			strcpy(typstr2, "IPVKEY");
		}
		nRetVal = strcmp(typstr1, typstr2);
		break;
	case 4:
		nRetVal = strcmp(pData1->descrip,
                                 pData2->descrip);
		break;
	default:
		break;
	}
	return nRetVal;
}

/***********************************************************************
c
c   FUNCTION: MenuSortFunc1(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
c
c       Sort the list on control list form in partical order
c
c   INPUT:  lParam1: data number 1
c			lParam2:  data number 1
c			lParamSort: sort field number
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CALLBACK MenuSortFunc2(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	char typstr1[20], typstr2[20];
	UZ_keycalls* pData1 = (UZ_keycalls*)lParam1;
	UZ_keycalls* pData2 = (UZ_keycalls*)lParam2;
	int nRetVal = 0;
	if ((pData1==NULL)||(pData2==NULL))
		return 0;

	switch(lParamSort)
	{
	case 2:
		nRetVal = -strcmp(pData1->name,
                                 pData2->name);
		break;
	case 3:	
		if (pData1->type==DASKEY) 
		{
			strcpy(typstr1, "DASKEY");
		}
		else if (pData1->type==NISKEY) 
		{
			strcpy(typstr1, "NISKEY");
		}
		else if (pData1->type==NCLKEY) 
		{
			strcpy(typstr1, "NCLKEY");
		}
		else if (pData1->type==CAMKEY) 
		{
			strcpy(typstr1, "CAMKEY");
		}
		else if (pData1->type==CADKEY) 
		{
			strcpy(typstr1, "CADKEY");
		}
		else if (pData1->type==IPVKEY) 
		{
			strcpy(typstr1, "IPVKEY");
		}
		if (pData2->type==DASKEY) 
		{
			strcpy(typstr2, "DASKEY");
		}
		else if (pData2->type==NISKEY) 
		{
			strcpy(typstr2, "NISKEY");
		}
		else if (pData2->type==NCLKEY) 
		{
			strcpy(typstr2, "NCLKEY");
		}
		else if (pData2->type==CAMKEY) 
		{
			strcpy(typstr2, "CAMKEY");
		}
		else if (pData2->type==CADKEY) 
		{
			strcpy(typstr2, "CADKEY");
		}
		else if (pData2->type==IPVKEY) 
		{
			strcpy(typstr2, "IPVKEY");
		}
		nRetVal = -strcmp(typstr1, typstr2);
		break;
	case 4:
		nRetVal = -strcmp(pData1->descrip,
                                 pData2->descrip);
		break;
	default:
		break;
	}
	return nRetVal;
}
/***********************************************************************
c
c   FUNCTION: MenuSortFunc1(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
c
c       Sort the list on control list form in partical order
c
c   INPUT:  lParam1: data number 1
c			lParam2:  data number 1
c			lParamSort: sort field number
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CALLBACK MenuSortFunc3(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	char typstr1[20], typstr2[20];
	UZ_menuinfo* pData1 = (UZ_menuinfo*)lParam1;
	UZ_menuinfo* pData2 = (UZ_menuinfo*)lParam2;
	int nRetVal = 0;
	if ((pData1==NULL)||(pData2==NULL))
		return 0;

	switch(lParamSort)
	{
	case 2:
		nRetVal = strcmp(pData1->name,
                                 pData2->name);
		break;
	case 3:	
		if (pData1->type==UDM_MTYPE_PULLDOWN) 
		{
			strcpy(typstr1, "PULLDOWN");
		}
		else if (pData1->type==UDM_MTYPE_POPUP) 
		{
			strcpy(typstr1, "POPUP");
		}
		else if (pData1->type==UDM_MTYPE_ICON) 
		{
			strcpy(typstr1, "DOCKED");
		}
		else if (pData1->type==UDM_MTYPE_MENU) 
		{
			strcpy(typstr1, "FLOATING");
		}
		nRetVal = strcmp(typstr1, typstr2);
		break;
	case 4:
		nRetVal = strcmp(pData1->descrip,
                                 pData2->descrip);
		break;
	default:
		break;
	}
	return nRetVal;
}
/***********************************************************************
c
c   FUNCTION: MenuSortFunc1(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
c
c       Sort the list on control list form in partical order
c
c   INPUT:  lParam1: data number 1
c			lParam2:  data number 1
c			lParamSort: sort field number
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CALLBACK MenuSortFunc4(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	char typstr1[20], typstr2[20];
	UZ_menuinfo* pData1 = (UZ_menuinfo*)lParam1;
	UZ_menuinfo* pData2 = (UZ_menuinfo*)lParam2;
	int nRetVal = 0;
	if ((pData1==NULL)||(pData2==NULL))
		return 0;

	switch(lParamSort)
	{
	case 2:
		nRetVal = -strcmp(pData1->name,
                                 pData2->name);
		break;
	case 3:	
		if (pData1->type==UDM_MTYPE_PULLDOWN) 
		{
			strcpy(typstr1, "PULLDOWN");
		}
		else if (pData1->type==UDM_MTYPE_POPUP) 
		{
			strcpy(typstr1, "POPUP");
		}
		else if (pData1->type==UDM_MTYPE_ICON) 
		{
			strcpy(typstr1, "DOCKED");
		}
		else if (pData1->type==UDM_MTYPE_MENU) 
		{
			strcpy(typstr1, "FLOATING");
		}
		nRetVal = -strcmp(typstr1, typstr2);
		break;
	case 4:
		nRetVal = -strcmp(pData1->descrip,
                                 pData2->descrip);
		break;
	default:
		break;
	}
	return nRetVal;
}
/***********************************************************************
c
c   FUNCTION: CNCLMenuDsnDlg::Init_Seperator_list()
c
c       This function initialize a control list with a seperator choice
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::Init_Seperator_list()
{
	LVCOLUMN lvColumn;
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH ;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.iImage = -1;
	lvColumn.cx = 100;
	lvColumn.pszText = _T("Item Type");
	m_listctl.InsertColumn(0, &lvColumn);

	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH; 
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = 400;
	lvColumn.pszText = _T("Seperator");
	m_listctl.InsertColumn(1, &lvColumn);

	m_rimglist = new CImageList();
	if ((UW_icon_size==0)||(UW_icon_size==1)||(UW_icon_size==2))
	{
		m_rimglist->Create(16,16, ILC_COLOR16|ILC_MASK, 0, 1000);
	}
	else
		m_rimglist->Create(24,24, ILC_COLOR16|ILC_MASK, 0, 1000);

	m_listctl.SetImageList(m_rimglist, LVSIL_SMALL); 
	LVITEM lvItem;
	for (int i = 0; i < 1; i++)
	{
		lvItem.mask        = LVIF_TEXT | LVIF_INDENT;
		lvItem.iSubItem    = 0;
		lvItem.iIndent    = -2;
		lvItem.pszText     =  " ";
		lvItem.iItem       = i;
		lvItem.cchTextMax  = strlen(lvItem.pszText);
		m_listctl.SetItem(&lvItem);
		m_listctl.SetItem(i, 1, LVIF_IMAGE, NULL, -1, 0, 0, 0);
	}
	m_listctl.SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_SUBITEMIMAGES|LVS_EX_GRIDLINES);

	m_listctl.InsertItem(0, _T(""));
	m_listctl.SetItemText(0, 1, "Single Bar");
	m_listctl.SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES);
	m_listctl.SetItemState (0, LVIS_SELECTED, LVIS_SELECTED);
/*
....change the m_listctl size in order for CListCtrl to call MeasureItem to set the row height
*/
	m_listctl.ResetRowSize();
}

/***********************************************************************
c
c   FUNCTION: CNCLMenuDsnDlg::Init_function_list(int flag)
c
c       This function initialize a control list with a function type
c
c   INPUT:  flag: 1 reset everything
c					0: only reset the list
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::Init_function_list(int flag)
{
	LVCOLUMN lvColumn;
	CRect rectClient;
	int ex_flag;
	m_listctl.GetClientRect(&rectClient);

	if (flag)
		m_filt_item = -1;

	HBITMAP defhBmap = NULL;
	CBitmap *defImage;
	CBitmap defcBmap;
	int defImgnum[10000];
	HDC thdc = NCL_MainFrame->GetDC()->m_hDC;				
	if (m_rimglist!=NULL)
	{
		delete m_rimglist;
		m_listctl.ReSetBitmap();
	}
	m_rimglist = new CImageList();
	if ((UW_icon_size==0)||(UW_icon_size==1)||(UW_icon_size==2))
	{
		m_rimglist->Create(16,16, ILC_COLOR16|ILC_MASK, 0, 1000);
	}
	else
		m_rimglist->Create(24,24, ILC_COLOR16|ILC_MASK, 0, 1000);

	int i, j;
	i = 0;
	j = 0;
	char bmpfile[256], filtstr1[256], filtstr2[256], filtstr3[256];
	char filtstr1_cap[256], filtstr2_cap[256], filtstr3_cap[256];  
	char filter1_cap[256], filter2_cap[256], filter3_cap[256];
	char type[20];
	int select = -1;

	if (flag)
	{
		lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH ;
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.iImage = -1;
		lvColumn.cx = 100;
		lvColumn.pszText = _T("Item Type");
		m_listctl.InsertColumn(0, &lvColumn);

		lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH ;
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.iImage = -1;
		lvColumn.cx = 40;
		lvColumn.pszText = _T("Icon");
		m_listctl.InsertColumn(1, &lvColumn);

		lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH; 
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.cx = 120;
		lvColumn.pszText = _T("Functions");
		m_listctl.InsertColumn(2, &lvColumn); 

		lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH; 
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.cx = 80;
		lvColumn.pszText = _T("Key");
		m_listctl.InsertColumn(3, &lvColumn); 

		lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH; 
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.cx = rectClient.Width() - 80 - 120 - 40 - 100 - 20 + 50;
		lvColumn.pszText = _T("Description");
		m_listctl.InsertColumn(4, &lvColumn); 
	}
	while (UU_TRUE)
	{
		if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;

//.....no MSLKEY for NCL

		if ((strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) || (UZ_keyfuncs[i].type==MSLKEY) )
		{
			i++;
			continue;
		}
/*
.....if it is NCLIPV standalone, only NCLIPV standlaone callable function will be used
*/
		ex_flag = UZ_keyfuncs[i].app;
		if ((LW_nclipv==LW_STANDALONE)&&(!(ex_flag & NCLIPVFL)))
		{
			i++;
			continue;
		}

		if (m_filt_item!=-1)
		{
			strcpy(filtstr1, UZ_keyfuncs[i].name);
			if (UZ_keyfuncs[i].type==DASKEY) 
			{
				strcpy_s(type, "DASKEY");
			}
			else if (UZ_keyfuncs[i].type==NISKEY) 
			{
				strcpy_s(type, "NISKEY");
			}
			else if (UZ_keyfuncs[i].type==NCLKEY) 
			{
				strcpy_s(type, "NCLKEY");
			}
			else if (UZ_keyfuncs[i].type==CAMKEY) 
			{
				strcpy_s(type, "CAMKEY");
			}
			else if (UZ_keyfuncs[i].type==CADKEY) 
			{
				strcpy_s(type, "CADKEY");
			}
			else if (UZ_keyfuncs[i].type==IPVKEY) 
			{
				strcpy_s(type, "IPVKEY");
			}
			else
				strcpy_s(type, "NOKEY");
			strcpy(filtstr2, type);
			strcpy(filtstr3, UZ_keyfuncs[i].descrip);
			if ((m_filter1[0]!='\0')&&(strcmp(m_filter1, "*")!=0))
			{
				strcpy_s(filtstr1_cap, 256, filtstr1);
				strcpy_s(filter1_cap, 256, m_filter1);		
				ul_to_upper(filtstr1_cap);
				ul_to_upper(filter1_cap);

				if (ncl_filter_str2(filtstr1_cap, filter1_cap)==0)
				{
					i++;
					continue;
				}
			}
			if ((m_filter2[0]!='\0')&&(strcmp(m_filter2, "*")!=0))
			{
				strcpy_s(filtstr2_cap, 256, filtstr2);
				strcpy_s(filter2_cap, 256, m_filter2);		
				ul_to_upper(filtstr2_cap);
				ul_to_upper(filter2_cap);

				if (ncl_filter_str2(filtstr2_cap, filter2_cap)==0)
				{
					i++;
					continue;
				}
			}
			if ((m_filter3[0]!='\0')&&(strcmp(m_filter3, "*")!=0))
			{
				strcpy_s(filtstr3_cap, 256, filtstr3);
				strcpy_s(filter3_cap, 256, m_filter3);		
				ul_to_upper(filtstr3_cap);
				ul_to_upper(filter3_cap);

				if (ncl_filter_str2(filtstr3_cap, filter3_cap)==0)
				{
					i++;
					continue;
				}
			}
		}
		m_listctl.InsertItem(0, _T(""));
		i++;
	}
	i=0;
	j=0;
	while (UU_TRUE)
	{
		if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
/*
.....no MSLKEY for NCL
*/
		if ((strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) || (UZ_keyfuncs[i].type==MSLKEY) )
		{
			i++;
			continue;
		}
/*
.....if it is NCLIPV standalone, only NCLIPV standlaone callable function will be used
*/
		ex_flag = UZ_keyfuncs[i].app;
		if ((LW_nclipv==LW_STANDALONE)&&(!(ex_flag & NCLIPVFL)))
		{
			i++;
			continue;
		}
		if (UZ_keyfuncs[i].type==DASKEY) 
		{
			strcpy_s(type, "DASKEY");
		}
		else if (UZ_keyfuncs[i].type==NISKEY) 
		{
			strcpy_s(type, "NISKEY");
		}
		else if (UZ_keyfuncs[i].type==NCLKEY) 
		{
			strcpy_s(type, "NCLKEY");
		}
		else if (UZ_keyfuncs[i].type==CAMKEY) 
		{
			strcpy_s(type, "CAMKEY");
		}
		else if (UZ_keyfuncs[i].type==CADKEY) 
		{
			strcpy_s(type, "CADKEY");
		}
		else if (UZ_keyfuncs[i].type==IPVKEY) 
		{
			strcpy_s(type, "IPVKEY");
		}
		else
			strcpy_s(type, "NOKEY");
/*
.....check if match the filter
*/
		if (m_filt_item!=-1)
		{
			strcpy(filtstr1, UZ_keyfuncs[i].name);
			strcpy(filtstr2, type);
			strcpy(filtstr3, UZ_keyfuncs[i].descrip);
			if ((m_filter1[0]!='\0')&&(strcmp(m_filter1, "*")!=0))
			{
				strcpy_s(filtstr1_cap, 256, filtstr1);
				strcpy_s(filter1_cap, 256, m_filter1);		
				ul_to_upper(filtstr1_cap);
				ul_to_upper(filter1_cap);

				if (ncl_filter_str2(filtstr1_cap, filter1_cap)==0)
				{
					i++;
					continue;
				}
			}
			if ((m_filter2[0]!='\0')&&(strcmp(m_filter2, "*")!=0))
			{
				strcpy_s(filtstr2_cap, 256, filtstr2);
				strcpy_s(filter2_cap, 256, m_filter2);		
				ul_to_upper(filtstr2_cap);
				ul_to_upper(filter2_cap);

				if (ncl_filter_str2(filtstr2_cap, filter2_cap)==0)
				{
					i++;
					continue;
				}
			}
			if ((m_filter3[0]!='\0')&&(strcmp(m_filter3, "*")!=0))
			{
				strcpy_s(filtstr3_cap, 256, filtstr3);
				strcpy_s(filter3_cap, 256, m_filter3);		
				ul_to_upper(filtstr3_cap);
				ul_to_upper(filter3_cap);

				if (ncl_filter_str2(filtstr3_cap, filter3_cap)==0)
				{
					i++;
					continue;
				}
			}
		}
		strcpy(bmpfile, UZ_keyfuncs[i].name);
		strcat(bmpfile, ".bmp");
		defhBmap = uw_get_bitmap(bmpfile, thdc, -2);
		m_listctl.SetBitmap(defhBmap, j);
		defImage = defcBmap.FromHandle(defhBmap);
		defImgnum[j] = m_rimglist->Add(defImage, 0xC0C0C0);

		m_listctl.SetItemText(j, 0, "Function");
		m_listctl.SetItemText(j, 1, " ");
		m_listctl.SetItemText(j, 2, UZ_keyfuncs[i].name);
		m_listctl.SetItemText(j, 3, type);
		m_listctl.SetItemText(j, 4, UZ_keyfuncs[i].descrip);
		m_listctl.SetItemData(j, (LPARAM)&(UZ_keyfuncs[i]));
		if (stricmp(UZ_keyfuncs[i].name, m_menu_item.file)==0)
		{
			select = j;
			strcpy(m_func_name, UZ_keyfuncs[i].name);
		}
		i++; j++;
	}
    m_listctl.SetImageList(m_rimglist, LVSIL_SMALL); 
	LVITEM lvItem;
	for (int i = 0; i < j; i++)
	{
/*
.....the first column will always show icon or icon space if we set the image list
.....for the second column, and also the lvItem.iIndent=-1 is not working, so in order to
.....not show an empty space before the text, we choice not to show text not all, and set
.....iIdent = -2 (-1 should be perfect but not working)
.....there is not much need for it anyway
*/
		lvItem.mask        = LVIF_TEXT | LVIF_INDENT;
		lvItem.iSubItem    = 0;
		lvItem.iIndent    = -2;
		lvItem.pszText     =  " ";
		lvItem.iItem       = i;
		lvItem.cchTextMax  = strlen(lvItem.pszText);
		m_listctl.SetItem(&lvItem);
		m_listctl.SetItem(i, 1, LVIF_IMAGE, NULL, defImgnum[i], 0, 0, 0);
	}
	m_listctl.SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_SUBITEMIMAGES|LVS_EX_GRIDLINES);
	if (select!=-1)
	{
		m_listctl.SetFocus();
		m_listctl.SetItemState (select, LVIS_SELECTED, LVIS_SELECTED);
		GetDlgItem(IDC_EDIT4)->SetWindowText(m_func_name);
	}
	if (flag)
	{	m_sort_col = -1;
		m_bSortAscending = FALSE;
	}
	else if (m_sort_col!=-1)
	{
		if (m_bSortAscending)
			m_listctl.SortItems((PFNLVCOMPARE)MenuSortFunc1, m_sort_col);
		else
			m_listctl.SortItems((PFNLVCOMPARE)MenuSortFunc2, m_sort_col);
	}
/*
....change the m_listctl size in order for CListCtrl to call MeasureItem to set the row height
*/
	m_listctl.ResetRowSize();
}
/***********************************************************************
c
c   FUNCTION: CNCLMenuDsnDlg::Init_menu_list(int flag)
c
c       This function initialize a control list with a menu type
c
c   INPUT:  flag: 1 reset everything
c					0: only reset the list
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::Init_menu_list(int flag)
{
	LVCOLUMN lvColumn;
	CRect rectClient;
	m_listctl.GetClientRect(&rectClient);
	if (flag)
	{
		lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH ;
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.iImage = 0;
		lvColumn.cx = 100;
		lvColumn.pszText = _T("Item Type");
		m_listctl.InsertColumn(0, &lvColumn);

		lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH; 
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.cx = 40;
		lvColumn.pszText = _T("Icon");
		m_listctl.InsertColumn(1, &lvColumn); 


		lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH; 
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.cx = 120;
		lvColumn.pszText = _T("Menu Name");
		m_listctl.InsertColumn(2, &lvColumn); 
		

		lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH; 
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.cx = 80;
		lvColumn.pszText = _T("Menu Type");
		m_listctl.InsertColumn(3, &lvColumn); 

		lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH; 
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.cx = rectClient.Width() - 80 - 120 - 40 - 100 - 20;
		lvColumn.pszText = _T("Description");
		m_listctl.InsertColumn(4, &lvColumn); 
	}
	HBITMAP defhBmap = NULL;
	CBitmap *defImage;
	CBitmap defcBmap;
	int defImgnum[10000];
	HDC thdc = NCL_MainFrame->GetDC()->m_hDC;	
	if (m_rimglist!=NULL)
	{
		delete m_rimglist;
		m_listctl.ReSetBitmap();
	}
	m_rimglist = new CImageList();
	if ((UW_icon_size==0)||(UW_icon_size==1)||(UW_icon_size==2))
	{
		m_rimglist->Create(16,16, ILC_COLOR16|ILC_MASK, 0, 1000);
	}
	else
		m_rimglist->Create(24,24, ILC_COLOR16|ILC_MASK, 0, 1000);

	int i, j;
	i = 0;
	j = 0;
	LVITEM lvItem;
	char bmpfile[256], filtstr1[256], filtstr2[256], filtstr3[256];
	char filtstr1_cap[256], filtstr2_cap[256], filtstr3_cap[256];  
	char filter1_cap[256], filter2_cap[256], filter3_cap[256];
	char type[20];
	int select = -1;

	m_listctl.SetExtendedStyle(LVS_EX_SUBITEMIMAGES);

	char basename[UX_MAX_FILE_LEN], menutype[10], descrip[40];
	for (i=0; i<m_menulist_no; i++)
	{
/*
.....check if match the filter
*/
		if (m_filt_item!=-1)
		{
			strcpy(filtstr1, m_menulist[i].name);
			if (m_menulist[i].type==UDM_MTYPE_PULLDOWN)
			{
				strcpy_s(type, "PULLDOWN");
			}
			else if (m_menulist[i].type==UDM_MTYPE_POPUP)
			{
				strcpy_s(type, "POPUP");
			}
			else if (m_menulist[i].type==UDM_MTYPE_ICON)
			{
				strcpy_s(type, "DOCKED");
			}
			else if (m_menulist[i].type==UDM_MTYPE_MENU)
			{
				strcpy_s(type, "FLOATING");
			}
			else
				strcpy_s(type, "NOTYPE");
			strcpy(filtstr2, type);
			strcpy(filtstr3, m_menulist[i].descrip);
			if ((m_filter1[0]!='\0')&&(strcmp(m_filter1, "*")!=0))
			{
				strcpy_s(filtstr1_cap, 256, filtstr1);
				strcpy_s(filter1_cap, 256, m_filter1);		
				ul_to_upper(filtstr1_cap);
				ul_to_upper(filter1_cap);

				if (ncl_filter_str2(filtstr1_cap, filter1_cap)==0)
				{
					continue;
				}
			}
			if ((m_filter2[0]!='\0')&&(strcmp(m_filter2, "*")!=0))
			{
				strcpy_s(filtstr2_cap, 256, filtstr2);
				strcpy_s(filter2_cap, 256, m_filter2);		
				ul_to_upper(filtstr2_cap);
				ul_to_upper(filter2_cap);

				if (ncl_filter_str2(filtstr2_cap, filter2_cap)==0)
				{
					continue;
				}
			}
			if ((m_filter3[0]!='\0')&&(strcmp(m_filter3, "*")!=0))
			{
				strcpy_s(filtstr3_cap, 256, filtstr3);
				strcpy_s(filter3_cap, 256, m_filter3);		
				ul_to_upper(filtstr3_cap);
				ul_to_upper(filter3_cap);

				if (ncl_filter_str2(filtstr3_cap, filter3_cap)==0)
				{
					continue;
				}
			}
		}
		m_listctl.InsertItem(0, _T(""));
	}
	for (i=0,j=0; i<m_menulist_no; i++,j++)
	{
		if (m_menulist[i].type==UDM_MTYPE_PULLDOWN)
		{
			strcpy_s(type, "PULLDOWN");
		}
		else if (m_menulist[i].type==UDM_MTYPE_POPUP)
		{
			strcpy_s(type, "POPUP");
		}
		else if (m_menulist[i].type==UDM_MTYPE_ICON)
		{
			strcpy_s(type, "DOCKED");
		}
		else if (m_menulist[i].type==UDM_MTYPE_MENU)
		{
			strcpy_s(type, "FLOATING");
		}
		else
			strcpy_s(type, "NOTYPE");

		if (m_filt_item!=-1)
		{
			strcpy(filtstr1, m_menulist[i].name);
			strcpy(filtstr2, type);
			strcpy(filtstr3, m_menulist[i].descrip);
			if ((m_filter1[0]!='\0')&&(strcmp(m_filter1, "*")!=0))
			{
				strcpy_s(filtstr1_cap, 256, filtstr1);
				strcpy_s(filter1_cap, 256, m_filter1);		
				ul_to_upper(filtstr1_cap);
				ul_to_upper(filter1_cap);

				if (ncl_filter_str2(filtstr1_cap, filter1_cap)==0)
				{
					j--;
					continue;
				}
			}
			if ((m_filter2[0]!='\0')&&(strcmp(m_filter2, "*")!=0))
			{
				strcpy_s(filtstr2_cap, 256, filtstr2);
				strcpy_s(filter2_cap, 256, m_filter2);		
				ul_to_upper(filtstr2_cap);
				ul_to_upper(filter2_cap);

				if (ncl_filter_str2(filtstr2_cap, filter2_cap)==0)
				{
					j--;
					continue;
				}
			}
			if ((m_filter3[0]!='\0')&&(strcmp(m_filter3, "*")!=0))
			{
				strcpy_s(filtstr3_cap, 256, filtstr3);
				strcpy_s(filter3_cap, 256, m_filter3);		
				ul_to_upper(filtstr3_cap);
				ul_to_upper(filter3_cap);

				if (ncl_filter_str2(filtstr3_cap, filter3_cap)==0)
				{
					j--;
					continue;
				}
			}
		}
		strcpy(basename, m_menulist[i].name);
		char *p = (char *)strrchr(basename,'.');
		if (p == 0)
			strcat(basename, ".bmp");
		else
		{
			*p = 0;
			strcat(basename, ".bmp");
		}
		defhBmap = uw_get_bitmap(basename, thdc, -2);
		m_listctl.SetBitmap(defhBmap, j);
		defImage = defcBmap.FromHandle(defhBmap);
		defImgnum[j] = m_rimglist->Add(defImage, 0xC0C0C0);
		if (m_menu_type==0)
			m_listctl.SetItemText(j, 0, "Pulldown Menu");
		else
			m_listctl.SetItemText(j, 0, "Menu");
		m_listctl.SetItemText(j, 1, " ");
		m_listctl.SetItemText(j, 2, m_menulist[i].name);
		m_listctl.SetItemText(j, 3, type);
		m_listctl.SetItemText(j, 4, m_menulist[i].descrip);
		m_listctl.SetItemData(j, (LPARAM)&(m_menulist[i]));
		if (stricmp(m_menulist[i].name, m_menu_item.file)==0)
		{
			select = j;
			strcpy(m_menu_name, m_menulist[i].name);
		}
	}
    m_listctl.SetImageList(m_rimglist, LVSIL_SMALL); 
	for (int i = 0; i < j; i++)
	{
/*
.....the first column will always show icon or icon space if we set the image list
.....for the second column, and also the lvItem.iIndent=-1 is not working, so in order to
.....not show an empty space before the text, we choice not to show text not all, and set
.....iIdent = -2 (-1 should be perfect but not working)
.....there is not much need for it anyway
*/
		lvItem.mask        = LVIF_TEXT | LVIF_INDENT;
		lvItem.iSubItem    = 0;
		lvItem.iIndent    = -2;
		lvItem.pszText     =  " ";
		lvItem.iItem       = i;
		lvItem.cchTextMax  = strlen(lvItem.pszText);
		m_listctl.SetItem(&lvItem);
		m_listctl.SetItem(i, 1, LVIF_IMAGE, NULL, defImgnum[i], 0, 0, 0);
	}
	m_listctl.SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_SUBITEMIMAGES|LVS_EX_GRIDLINES);
	if (select!=-1)
	{
		m_listctl.SetFocus();
		m_listctl.SetItemState (select, LVIS_SELECTED, LVIS_SELECTED);
		GetDlgItem(IDC_EDIT4)->SetWindowText(m_menu_name);
	}
	if (flag)
	{	m_sort_col = -1;
		m_bSortAscending = FALSE;
	}
	else if (m_sort_col!=-1)
	{
		if (m_bSortAscending)
			m_listctl.SortItems((PFNLVCOMPARE)MenuSortFunc3, m_sort_col);
		else
			m_listctl.SortItems((PFNLVCOMPARE)MenuSortFunc4, m_sort_col);
	}
/*
....change the m_listctl size in order for CListCtrl to call MeasureItem to set the row height
*/
	m_listctl.ResetRowSize();
}
/***********************************************************************
c
c   FUNCTION: OnReturnKey()
c
c       callback function for return key
c
c   INPUT:  none
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::OnReturnKey()
{
	m_listctl.m_HeaderCtrl.EndEdit();
}

/***********************************************************************
c
c   FUNCTION: ChangedFilter(int filt_item, char *filter)
c
c       function for change the filter
c
c   INPUT:  filt_item: filter item number
c			filter: filter string
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::ChangedFilter(int filt_item, char *filter)
{
	strcpy(m_filter, filter);
	m_filt_item = filt_item;
	if (filt_item==0)
	{
		strcpy(m_filter1, m_filter);
	}
	else if (filt_item==2)
	{
		strcpy(m_filter3, m_filter);
	}
	Reset_Ctllist(0);
}

/***********************************************************************
c
c   FUNCTION: ChangeKeyType(int type)
c
c       function for change the key type
c
c   INPUT:  type: key type
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::ChangeKeyType(int type)
{
	m_filt_item = 1;
	if (m_menu_type==2)
	{
		if (type==0) 
		{
			strcpy_s(m_filter2, "DASKEY");
		}
		else if (type==1) 
		{
			strcpy_s(m_filter2, "NISKEY");
		}
		else if (type==2) 
		{
			strcpy_s(m_filter2, "NCLKEY");
		}
		else if (type==3) 
		{
			strcpy_s(m_filter2, "CAMKEY");
		}
		else if (type==4) 
		{
			strcpy_s(m_filter2, "CADKEY");
		}
		else if (type==5) 
		{
			strcpy_s(m_filter2, "IPVKEY");
		}
		else
		{
			m_filter2[0] = '\0';
		}
	}
	else
	{
		if (type==0) 
		{
			strcpy_s(m_filter2, "PULLDOWN");
		}
		else if (type==1) 
		{
			strcpy_s(m_filter2, "POPUP");
		}
		else if (type==2) 
		{
			strcpy_s(m_filter2, "DOCKED");
		}
		else if (type==3) 
		{
			strcpy_s(m_filter2, "FLOATING");
		}
		else
		{
			m_filter2[0] = '\0';
		}
	}
	Reset_Ctllist(0);
}

/***********************************************************************
c
c   FUNCTION: HideFiltEdit()
c
c      Hide the filter edit field
c
c   INPUT: none
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::HideFiltEdit()
{
	m_listctl.m_HeaderCtrl.HideFiltEdit();
}
/***********************************************************************
c
c   FUNCTION: ChangeMenuType(int menu_type)
c
c      function for change menu type
c
c   INPUT: menu_type: menu type to be change to
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::ChangeMenuType(int menu_type)
{
	int update_list = 1; 
	if ((menu_type!=m_menu_type)&&(menu_type!=-1))
	{
		if (((m_menu_type==0)&&(menu_type==1))
			|| ((m_menu_type==1)&&(menu_type==0)))
/*
......no need to update the table list
*/
			update_list = 0;
	}
	m_filter1[0] = '\0';
	m_filter2[0] = '\0';
	m_filter3[0] = '\0';
	if ((menu_type!=m_menu_type)&&(menu_type!=-1))
	{
/*
.....reset filter
*/
		strcpy(m_filter, "*");
		m_filt_item = -1;

		m_menu_type = menu_type;
		if (update_list)
			Reset_Ctllist(1);
		if ((m_menu_type==0)||(m_menu_type==1))
			(GetDlgItem(IDC_LABEL))->SetWindowText("Selected Button Menu:");
		else
			(GetDlgItem(IDC_LABEL))->SetWindowText("Selected Button Function:");

		if (m_menu_type==0)
		{
			(GetDlgItem(IDC_LABEL1))->ShowWindow(SW_HIDE);
			(GetDlgItem(IDC_EDIT1))->ShowWindow(SW_HIDE);
			(GetDlgItem(IDC_LABEL2))->ShowWindow(SW_SHOW);
			(GetDlgItem(IDC_EDIT6))->ShowWindow(SW_SHOW);
		}
		else
		{
			(GetDlgItem(IDC_LABEL2))->ShowWindow(SW_HIDE);
			(GetDlgItem(IDC_EDIT6))->ShowWindow(SW_HIDE);
			(GetDlgItem(IDC_LABEL1))->ShowWindow(SW_SHOW);
			(GetDlgItem(IDC_EDIT1))->ShowWindow(SW_SHOW);
		}
	}
}

/***********************************************************************
c
c   FUNCTION: check_menu(char *name)
c
c      check if the menu name already in the menu list
c
c   INPUT: name: menu name to be checked
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
int CNCLMenuDsnDlg::check_menu(char *name)
{
	for (int i=0; i<m_menulist_no; i++)
	{
		if (stricmp(name, m_menulist[i].name)==0)
			return 1;
	}
	return 0;
}
/***********************************************************************
c
c   FUNCTION: Reload_menulist()
c
c      reload the menu list
c
c   INPUT: none
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::Reload_menulist()
{
	for (int i=0; i<m_menulist_no; i++)
	{
		if (m_menulist[i].name!=NULL)
			uu_free (m_menulist[i].name);
		if (m_menulist[i].descrip!=NULL)
			uu_free (m_menulist[i].descrip);
	}
	char *list, pathname[UX_MAX_FILE_LEN];
	if (ul_get_full_dir("NCL_MENU", pathname) != UU_SUCCESS)
		return;
	if (ul_get_flist(pathname, "menu" ,&list, 0,(UX_NPRTERRS|UX_NCHK))
		 != UU_SUCCESS)
	{
		m_menulist_no = 0;
		return;
	}
	if (list==NULL) 
	{
		m_menulist_no = 0;
		return;
	}
	int len, args[2];
	args[0] = args[1] = 0;
	m_menulist_no = 0;
	char fname[UX_MAX_FILE_LEN], basename[UX_MAX_FILE_LEN], descrip[40];
	int menutype, status;
	while ((ux_nxt_file(&list, fname, UX_PRTERRS)) ==
			UU_SUCCESS)
	{
		ul_get_base_fname(fname, basename, args, (UX_NQUOTES|UX_NCHK));
		strcat(basename, ".menu");
		status = ud_get_menuinfo(basename, &menutype, descrip);
		if (status==-1)
			continue;
		if (menutype==UDM_MTYPE_MENUBAR)
			continue;
		len = strlen(basename);
		if (len<=0) continue;
		m_menulist[m_menulist_no].name = uu_malloc((len+1)*sizeof(char));
		strcpy(m_menulist[m_menulist_no].name, basename);
		len = strlen(descrip);
		if (len<=0) 
			m_menulist[m_menulist_no].descrip = NULL;
		else
		{
			m_menulist[m_menulist_no].descrip = uu_malloc((len+1)*sizeof(char));
			strcpy(m_menulist[m_menulist_no].descrip, descrip);
		}
		m_menulist[m_menulist_no].type = menutype;
		m_menulist_no++;
	}
/*
......we need added the local menu too if they have more different menu name
*/
	ul_get_full_dir("UU_USER_SETTINGS", pathname);
	ul_build_full_dir(pathname, "menu", pathname);
	if (ul_get_flist(pathname, "menu" ,&list, 0,(UX_NPRTERRS|UX_NCHK))
		 != UU_SUCCESS)
	{
		return;
	}
	if (list==NULL) 
	{
		return;
	}
	args[0] = args[1] = 0;
	while ((ux_nxt_file(&list, fname, UX_PRTERRS)) ==
			UU_SUCCESS)
	{
		ul_get_base_fname(fname, basename, args, (UX_NQUOTES|UX_NCHK));
/*
......check if the menu file already in the list
*/
		strcat(basename, ".menu");
		if (check_menu(basename)==1)
			continue;
		ud_get_menuinfo(basename, &menutype, descrip);
		len = strlen(basename);
		if (len<=0) continue;
		m_menulist[m_menulist_no].name = uu_malloc((len+1)*sizeof(char));
		strcpy(m_menulist[m_menulist_no].name, basename);
		len = strlen(descrip);
		if (len<=0) 
			m_menulist[m_menulist_no].descrip = NULL;
		else
		{
			m_menulist[m_menulist_no].descrip = uu_malloc((len+1)*sizeof(char));
			strcpy(m_menulist[m_menulist_no].descrip, descrip);
		}
		m_menulist[m_menulist_no].type = menutype;
		m_menulist_no++;
	}
}
/***********************************************************************
c
c   FUNCTION: Reset_function_list(int flag)
c
c      rerest the function table list (by reset list text), not used now, but may use later
c
c   INPUT: 
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::Reset_function_list(int flag)
{
	LVCOLUMN lvColumn;
	CRect rectClient;
	short ex_flag;
	m_listctl.GetClientRect(&rectClient);

	HBITMAP defhBmap = NULL;
	CBitmap *defImage;
	CBitmap defcBmap;
	int defImgnum[10000];
	HDC thdc = NCL_MainFrame->GetDC()->m_hDC;				
	if (m_rimglist!=NULL)
	{
		delete m_rimglist;
		m_listctl.ReSetBitmap();
	}
	m_rimglist = new CImageList();
	if ((UW_icon_size==0)||(UW_icon_size==1)||(UW_icon_size==2))
	{
		m_rimglist->Create(16,16, ILC_COLOR16|ILC_MASK, 0, 1000);
	}
	else
		m_rimglist->Create(24,24, ILC_COLOR16|ILC_MASK, 0, 1000);

	int i, j;
	i = 0;
	j = 0;
	char bmpfile[256], filtstr1[256], filtstr2[256], filtstr3[256];
	char filtstr1_cap[256], filtstr2_cap[256], filtstr3_cap[256];  
	char filter1_cap[256], filter2_cap[256], filter3_cap[256];
	char type[20];
	int select = -1;
/*
......reset the column label
*/
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH; 
	m_listctl.GetColumn(0, &lvColumn);
	lvColumn.pszText = _T("Item Type");
	lvColumn.cchTextMax  = strlen(lvColumn.pszText);
	m_listctl.SetColumn(0, &lvColumn);

	m_listctl.GetColumn(1, &lvColumn);
	lvColumn.pszText = _T("Icon");
	lvColumn.cchTextMax  = strlen(lvColumn.pszText);
	m_listctl.SetColumn(1, &lvColumn);

	m_listctl.GetColumn(2, &lvColumn);
	lvColumn.pszText = _T("Functions");
	lvColumn.cchTextMax  = strlen(lvColumn.pszText);
	m_listctl.SetColumn(2, &lvColumn);

	m_listctl.GetColumn(3, &lvColumn);
	lvColumn.pszText = _T("Key");
	lvColumn.cchTextMax  = strlen(lvColumn.pszText);
	m_listctl.SetColumn(3, &lvColumn);

	m_listctl.GetColumn(4, &lvColumn);
	lvColumn.pszText = _T("Description");
	lvColumn.cchTextMax  = strlen(lvColumn.pszText);
	m_listctl.SetColumn(4, &lvColumn);

	int total = m_listctl.GetItemCount();
	while (UU_TRUE)
	{
		if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
		if ((strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) || (UZ_keyfuncs[i].type==MSLKEY) )
		{
			i++;
			continue;
		}
/*
.....if it is NCLIPV standalone, only NCLIPV standlaone callable function will be used
*/
		ex_flag = UZ_keyfuncs[i].app;
		if ((LW_nclipv==LW_STANDALONE)&&(!(ex_flag & NCLIPVFL)))
		{
			i++;
			continue;
		}
		if (m_filt_item!=-1)
		{
			strcpy(filtstr1, UZ_keyfuncs[i].name);
			if (UZ_keyfuncs[i].type==DASKEY) 
			{
				strcpy_s(type, "DASKEY");
			}
			else if (UZ_keyfuncs[i].type==NISKEY) 
			{
				strcpy_s(type, "NISKEY");
			}
			else if (UZ_keyfuncs[i].type==NCLKEY) 
			{
				strcpy_s(type, "NCLKEY");
			}
			else if (UZ_keyfuncs[i].type==CAMKEY) 
			{
				strcpy_s(type, "CAMKEY");
			}
			else if (UZ_keyfuncs[i].type==CADKEY) 
			{
				strcpy_s(type, "CADKEY");
			}
			else if (UZ_keyfuncs[i].type==IPVKEY) 
			{
				strcpy_s(type, "IPVKEY");
			}
			else
				strcpy_s(type, "NOKEY");
			strcpy(filtstr2, type);
			strcpy(filtstr3, UZ_keyfuncs[i].descrip);
			if ((m_filter1[0]!='\0')&&(strcmp(m_filter1, "*")!=0))
			{
				strcpy_s(filtstr1_cap, 256, filtstr1);
				strcpy_s(filter1_cap, 256, m_filter1);		
				ul_to_upper(filtstr1_cap);
				ul_to_upper(filter1_cap);

				if (ncl_filter_str2(filtstr1_cap, filter1_cap)==0)
				{
					i++;
					continue;
				}
			}
			if ((m_filter2[0]!='\0')&&(strcmp(m_filter2, "*")!=0))
			{
				strcpy_s(filtstr2_cap, 256, filtstr2);
				strcpy_s(filter2_cap, 256, m_filter2);		
				ul_to_upper(filtstr2_cap);
				ul_to_upper(filter2_cap);

				if (ncl_filter_str2(filtstr2_cap, filter2_cap)==0)
				{
					i++;
					continue;
				}
			}
			if ((m_filter3[0]!='\0')&&(strcmp(m_filter3, "*")!=0))
			{
				strcpy_s(filtstr3_cap, 256, filtstr3);
				strcpy_s(filter3_cap, 256, m_filter3);		
				ul_to_upper(filtstr3_cap);
				ul_to_upper(filter3_cap);

				if (ncl_filter_str2(filtstr3_cap, filter3_cap)==0)
				{
					i++;
					continue;
				}
			}
		}
		i++;
	}
	if (i<total)
	{
		for (j=i; j<total;j++)
			m_listctl.DeleteItem(i);
	}
	if (i>total)
	{
		for (j=total; j<i;j++)
			m_listctl.InsertItem(total, _T(""));
	}
	i=0;
	j=0;
	while (UU_TRUE)
	{
		if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
/*
.....no MSLKEY for NCL
*/
		if ((strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) || (UZ_keyfuncs[i].type==MSLKEY) )
		{
			i++;
			continue;
		}
/*
.....if it is NCLIPV standalone, only NCLIPV standlaone callable function will be used
*/
		ex_flag = UZ_keyfuncs[i].app;
		if ((LW_nclipv==LW_STANDALONE)&&(!(ex_flag & NCLIPVFL)))
		{
			i++;
			continue;
		}
		if (UZ_keyfuncs[i].type==DASKEY) 
		{
			strcpy_s(type, "DASKEY");
		}
		else if (UZ_keyfuncs[i].type==NISKEY) 
		{
			strcpy_s(type, "NISKEY");
		}
		else if (UZ_keyfuncs[i].type==NCLKEY) 
		{
			strcpy_s(type, "NCLKEY");
		}
		else if (UZ_keyfuncs[i].type==CAMKEY) 
		{
			strcpy_s(type, "CAMKEY");
		}
		else if (UZ_keyfuncs[i].type==CADKEY) 
		{
			strcpy_s(type, "CADKEY");
		}
		else if (UZ_keyfuncs[i].type==IPVKEY) 
		{
			strcpy_s(type, "IPVKEY");
		}
		else
			strcpy_s(type, "NOKEY");
/*
.....check if match the filter
*/
		if (m_filt_item!=-1)
		{
			strcpy(filtstr1, UZ_keyfuncs[i].name);
			strcpy(filtstr2, type);
			strcpy(filtstr3, UZ_keyfuncs[i].descrip);
			if ((m_filter1[0]!='\0')&&(strcmp(m_filter1, "*")!=0))
			{
				strcpy_s(filtstr1_cap, 256, filtstr1);
				strcpy_s(filter1_cap, 256, m_filter1);		
				ul_to_upper(filtstr1_cap);
				ul_to_upper(filter1_cap);

				if (ncl_filter_str2(filtstr1_cap, filter1_cap)==0)
				{
					i++;
					continue;
				}
			}
			if ((m_filter2[0]!='\0')&&(strcmp(m_filter2, "*")!=0))
			{
				strcpy_s(filtstr2_cap, 256, filtstr2);
				strcpy_s(filter2_cap, 256, m_filter2);		
				ul_to_upper(filtstr2_cap);
				ul_to_upper(filter2_cap);

				if (ncl_filter_str2(filtstr2_cap, filter2_cap)==0)
				{
					i++;
					continue;
				}
			}
			if ((m_filter3[0]!='\0')&&(strcmp(m_filter3, "*")!=0))
			{
				strcpy_s(filtstr3_cap, 256, filtstr3);
				strcpy_s(filter3_cap, 256, m_filter3);		
				ul_to_upper(filtstr3_cap);
				ul_to_upper(filter3_cap);

				if (ncl_filter_str2(filtstr3_cap, filter3_cap)==0)
				{
					i++;
					continue;
				}
			}
		}
		strcpy(bmpfile, UZ_keyfuncs[i].name);
		strcat(bmpfile, ".bmp");
		defhBmap = uw_get_bitmap(bmpfile, thdc, -2);
		m_listctl.SetBitmap(defhBmap, j);
		defImage = defcBmap.FromHandle(defhBmap);
		defImgnum[j] = m_rimglist->Add(defImage, 0xC0C0C0);

		m_listctl.SetItemText(j, 0, "Function");
		m_listctl.SetItemText(j, 1, " ");
		m_listctl.SetItemText(j, 2, UZ_keyfuncs[i].name);
		m_listctl.SetItemText(j, 3, type);
		m_listctl.SetItemText(j, 4, UZ_keyfuncs[i].descrip);
		m_listctl.SetItemData(j, (LPARAM)&(UZ_keyfuncs[i]));
		if (stricmp(UZ_keyfuncs[i].name, m_menu_item.file)==0)
		{
			select = j;
			strcpy(m_func_name, UZ_keyfuncs[i].name);
		}
		i++; j++;
	}
    m_listctl.SetImageList(m_rimglist, LVSIL_SMALL); 
	LVITEM lvItem;
	for (int i = 0; i < j; i++)
	{
/*
.....the first column will always show icon or icon space if we set the image list
.....for the second column, and also the lvItem.iIndent=-1 is not working, so in order to
.....not show an empty space before the text, we choice not to show text not all, and set
.....iIdent = -2 (-1 should be perfect but not working)
.....there is not much need for it anyway
*/
		lvItem.mask        = LVIF_TEXT | LVIF_INDENT;
		lvItem.iSubItem    = 0;
		lvItem.iIndent    = -2;
		lvItem.pszText     =  " ";
		lvItem.iItem       = i;
		lvItem.cchTextMax  = strlen(lvItem.pszText);
		m_listctl.SetItem(&lvItem);
		m_listctl.SetItem(i, 1, LVIF_IMAGE, NULL, defImgnum[i], 0, 0, 0);
	}
	if (select!=-1)
	{
		m_listctl.SetFocus();
		m_listctl.SetItemState (select, LVIS_SELECTED, LVIS_SELECTED);
		GetDlgItem(IDC_EDIT4)->SetWindowText(m_func_name);
	}
}

/***********************************************************************
c
c   FUNCTION: Reset_menu_list(int flag)
c
c      rerest the menu list (by reset list text), not used now, but may use later
c
c   INPUT: 
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::Reset_menu_list(int flag)
{
	LVCOLUMN lvColumn;
	CRect rectClient;
	m_listctl.GetClientRect(&rectClient);

/*
......reset the column label
*/
	lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH; 
	m_listctl.GetColumn(0, &lvColumn);
	lvColumn.pszText = _T("Item Type");
	lvColumn.cchTextMax  = strlen(lvColumn.pszText);
	m_listctl.SetColumn(0, &lvColumn);

	m_listctl.GetColumn(1, &lvColumn);
	lvColumn.pszText = _T("Icon");
	lvColumn.cchTextMax  = strlen(lvColumn.pszText);
	m_listctl.SetColumn(1, &lvColumn);

	m_listctl.GetColumn(2, &lvColumn);
	lvColumn.pszText = _T("Menu Name");
	lvColumn.cchTextMax  = strlen(lvColumn.pszText);
	m_listctl.SetColumn(2, &lvColumn);

	m_listctl.GetColumn(3, &lvColumn);
	lvColumn.pszText = _T("Menu Type");
	lvColumn.cchTextMax  = strlen(lvColumn.pszText);
	m_listctl.SetColumn(3, &lvColumn);

	m_listctl.GetColumn(4, &lvColumn);
	lvColumn.pszText = _T("Description");
	lvColumn.cchTextMax  = strlen(lvColumn.pszText);
	m_listctl.SetColumn(4, &lvColumn);

	int total = m_listctl.GetItemCount();

	HBITMAP defhBmap = NULL;
	CBitmap *defImage;
	CBitmap defcBmap;
	int defImgnum[10000];
	HDC thdc = NCL_MainFrame->GetDC()->m_hDC;	
	if (m_rimglist!=NULL)
	{
		delete m_rimglist;
		m_listctl.ReSetBitmap();
	}
	m_rimglist = new CImageList();
	if ((UW_icon_size==0)||(UW_icon_size==1)||(UW_icon_size==2))
	{
		m_rimglist->Create(16,16, ILC_COLOR16|ILC_MASK, 0, 1000);
	}
	else
		m_rimglist->Create(24,24, ILC_COLOR16|ILC_MASK, 0, 1000);

	int i, j, k;
	i = 0;
	j = 0;
	LVITEM lvItem;
	char bmpfile[256], filtstr1[256], filtstr2[256], filtstr3[256];
	char filtstr1_cap[256], filtstr2_cap[256], filtstr3_cap[256];  
	char filter1_cap[256], filter2_cap[256], filter3_cap[256];
	char type[20];
	int select = -1;

	m_listctl.SetExtendedStyle(LVS_EX_SUBITEMIMAGES);

	char basename[UX_MAX_FILE_LEN], menutype[10], descrip[40];
	for (i=0,j=0;i<m_menulist_no; i++)
	{
/*
.....check if match the filter
*/
		if (m_filt_item!=-1)
		{
			strcpy(filtstr1, m_menulist[i].name);
			if (m_menulist[i].type==UDM_MTYPE_PULLDOWN)
			{
				strcpy_s(type, "PULLDOWN");
			}
			else if (m_menulist[i].type==UDM_MTYPE_POPUP)
			{
				strcpy_s(type, "POPUP");
			}
			else if (m_menulist[i].type==UDM_MTYPE_ICON)
			{
				strcpy_s(type, "DOCKED");
			}
			else if (m_menulist[i].type==UDM_MTYPE_MENU)
			{
				strcpy_s(type, "FLOATING");
			}
			else
				strcpy_s(type, "NOTYPE");
			strcpy(filtstr2, type);
			strcpy(filtstr3, m_menulist[i].descrip);
			if ((m_filter1[0]!='\0')&&(strcmp(m_filter1, "*")!=0))
			{
				strcpy_s(filtstr1_cap, 256, filtstr1);
				strcpy_s(filter1_cap, 256, m_filter1);		
				ul_to_upper(filtstr1_cap);
				ul_to_upper(filter1_cap);

				if (ncl_filter_str2(filtstr1_cap, filter1_cap)==0)
				{
					continue;
				}
			}
			if ((m_filter2[0]!='\0')&&(strcmp(m_filter2, "*")!=0))
			{
				strcpy_s(filtstr2_cap, 256, filtstr2);
				strcpy_s(filter2_cap, 256, m_filter2);		
				ul_to_upper(filtstr2_cap);
				ul_to_upper(filter2_cap);

				if (ncl_filter_str2(filtstr2_cap, filter2_cap)==0)
				{
					continue;
				}
			}
			if ((m_filter3[0]!='\0')&&(strcmp(m_filter3, "*")!=0))
			{
				strcpy_s(filtstr3_cap, 256, filtstr3);
				strcpy_s(filter3_cap, 256, m_filter3);		
				ul_to_upper(filtstr3_cap);
				ul_to_upper(filter3_cap);

				if (ncl_filter_str2(filtstr3_cap, filter3_cap)==0)
				{
					continue;
				}
			}
		}
		j++;
	}
	if (j<total)
	{
		for (k=j; k<total;k++)
			m_listctl.DeleteItem(j);
	}
	if (j>total)
	{
		for (k=total; k<j;k++)
			m_listctl.InsertItem(total, _T(""));
	}
	for (i=0,j=0; i<m_menulist_no; i++,j++)
	{
		if (m_menulist[i].type==UDM_MTYPE_PULLDOWN)
		{
			strcpy_s(type, "PULLDOWN");
		}
		else if (m_menulist[i].type==UDM_MTYPE_POPUP)
		{
			strcpy_s(type, "POPUP");
		}
		else if (m_menulist[i].type==UDM_MTYPE_ICON)
		{
			strcpy_s(type, "DOCKED");
		}
		else if (m_menulist[i].type==UDM_MTYPE_MENU)
		{
			strcpy_s(type, "FLOATING");
		}
		else
			strcpy_s(type, "NOTYPE");

		if (m_filt_item!=-1)
		{
			strcpy(filtstr1, m_menulist[i].name);
			strcpy(filtstr2, type);
			strcpy(filtstr3, m_menulist[i].descrip);
			if ((m_filter1[0]!='\0')&&(strcmp(m_filter1, "*")!=0))
			{
				strcpy_s(filtstr1_cap, 256, filtstr1);
				strcpy_s(filter1_cap, 256, m_filter1);		
				ul_to_upper(filtstr1_cap);
				ul_to_upper(filter1_cap);

				if (ncl_filter_str2(filtstr1_cap, filter1_cap)==0)
				{
					j--;
					continue;
				}
			}
			if ((m_filter2[0]!='\0')&&(strcmp(m_filter2, "*")!=0))
			{
				strcpy_s(filtstr2_cap, 256, filtstr2);
				strcpy_s(filter2_cap, 256, m_filter2);		
				ul_to_upper(filtstr2_cap);
				ul_to_upper(filter2_cap);

				if (ncl_filter_str2(filtstr2_cap, filter2_cap)==0)
				{
					j--;
					continue;
				}
			}
			if ((m_filter3[0]!='\0')&&(strcmp(m_filter3, "*")!=0))
			{
				strcpy_s(filtstr3_cap, 256, filtstr3);
				strcpy_s(filter3_cap, 256, m_filter3);		
				ul_to_upper(filtstr3_cap);
				ul_to_upper(filter3_cap);

				if (ncl_filter_str2(filtstr3_cap, filter3_cap)==0)
				{
					j--;
					continue;
				}
			}
		}
		strcpy(basename, m_menulist[i].name);
		char *p = (char *)strrchr(basename,'.');
		if (p == 0)
			strcat(basename, ".bmp");
		else
		{
			*p = 0;
			strcat(basename, ".bmp");
		}
		defhBmap = uw_get_bitmap(basename, thdc, -2);
		m_listctl.SetBitmap(defhBmap, j);
		defImage = defcBmap.FromHandle(defhBmap);
		defImgnum[j] = m_rimglist->Add(defImage, 0xC0C0C0);
		if (m_menu_type==0)
			m_listctl.SetItemText(j, 0, "Pulldown Menu");
		else
			m_listctl.SetItemText(j, 0, "Menu");
		m_listctl.SetItemText(j, 1, " ");
		m_listctl.SetItemText(j, 2, m_menulist[i].name);
		m_listctl.SetItemText(j, 3, type);
		m_listctl.SetItemText(j, 4, m_menulist[i].descrip);
		m_listctl.SetItemData(j, (LPARAM)&(m_menulist[i]));
		if (stricmp(m_menulist[i].name, m_menu_item.file)==0)
		{
			select = j;
			strcpy(m_menu_name, m_menulist[i].name);
		}
	}
    m_listctl.SetImageList(m_rimglist, LVSIL_SMALL); 
	for (int i = 0; i < j; i++)
	{
/*
.....the first column will always show icon or icon space if we set the image list
.....for the second column, and also the lvItem.iIndent=-1 is not working, so in order to
.....not show an empty space before the text, we choice not to show text not all, and set
.....iIdent = -2 (-1 should be perfect but not working)
.....there is not much need for it anyway
*/
		lvItem.mask        = LVIF_TEXT | LVIF_INDENT;
		lvItem.iSubItem    = 0;
		lvItem.iIndent    = -2;
		lvItem.pszText     =  " ";
		lvItem.iItem       = i;
		lvItem.cchTextMax  = strlen(lvItem.pszText);
		m_listctl.SetItem(&lvItem);
		m_listctl.SetItem(i, 1, LVIF_IMAGE, NULL, defImgnum[i], 0, 0, 0);
	}
	m_listctl.SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_SUBITEMIMAGES|LVS_EX_GRIDLINES);
	if (select!=-1)
	{
		m_listctl.SetFocus();
		m_listctl.SetItemState (select, LVIS_SELECTED, LVIS_SELECTED);
		GetDlgItem(IDC_EDIT4)->SetWindowText(m_menu_name);
	}
}

/***********************************************************************
c
c   FUNCTION: Reset_Ctllist2(int flag)
c
c      reret the whole control list (by reset list text), not used now, but may use later
c
c   INPUT: 
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::Reset_Ctllist2(int flag)
{
	int number;
	number = m_listctl.m_HeaderCtrl.GetItemCount();
	if (flag)
		m_filt_item = -1;
	if ((m_menu_type==2)||(m_menu_type==1)||(m_menu_type==0))
	{
		if (number==2)
/*
......delete the column and list and totally recreate
*/
		{
			if (m_rimglist!=NULL)
			{
				delete m_rimglist;
				m_rimglist = NULL;
				m_listctl.ReSetBitmap();
			}
			if (flag)
			{
				m_listctl.DeleteColumn(0);
				m_listctl.DeleteColumn(0);
			}
			if (m_menu_type==2)
				Init_function_list(1);
			else
				Init_menu_list(1);
			m_listctl.m_HeaderCtrl.AddFilterArea(1, m_menu_type, this, m_filter1, m_filter2, m_filter3);
			m_listctl.m_HeaderCtrl.AddChoiceArea(1, m_menu_type, this, m_filter2);
		}
		else
		{
			if (m_menu_type==2)
				Reset_function_list(0);
			else
				Reset_menu_list(0);
/*			if (m_sort_col!=-1)
			{
				if (m_bSortAscending)
					m_listctl.SortItems((PFNLVCOMPARE)MenuSortFunc1, m_sort_col);
				else
					m_listctl.SortItems((PFNLVCOMPARE)MenuSortFunc2, m_sort_col);
			}
*/			
/*
....change the m_listctl size in order for CListCtrl to call MeasureItem to set the row height
*/
			m_listctl.ResetRowSize();
			m_listctl.m_HeaderCtrl.AddFilterArea(flag, m_menu_type, this, m_filter1, m_filter2, m_filter3);
			m_listctl.m_HeaderCtrl.AddChoiceArea(flag, m_menu_type, this, m_filter2);
		}
	}
	else
	{
		if (number==2)
/*
.....seperator
*/
			return;
		else
		{
			if (m_rimglist!=NULL)
			{
				delete m_rimglist;
				m_rimglist = NULL;
				m_listctl.ReSetBitmap();
			}
			m_listctl.DeleteColumn(0);
			m_listctl.DeleteColumn(0);
			m_listctl.DeleteColumn(0);
			m_listctl.DeleteColumn(0);
			m_listctl.DeleteColumn(0);
			m_listctl.m_HeaderCtrl.HideFilterArea();
			Init_Seperator_list();
			m_listctl.ResetRowSize();
		}
	}
}
/***********************************************************************
c
c   FUNCTION: Reset_Ctllist(int flag)
c
c      reret the whole control list 
c
c   INPUT: flag: 1: recreate the whole list
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::Reset_Ctllist(int flag)
{
/*
.....Reset_Ctllist2(flag) tried to reset list faster, but encount some memory
.....error, now we just go on with the old code and do it later
	Reset_Ctllist2(flag);
	return;
*/
	m_listctl.DeleteAllItems();
	if (m_rimglist!=NULL)
	{
		delete m_rimglist;
		m_rimglist = NULL;
		m_listctl.ReSetBitmap();
	}
	if (flag)
	{
		m_listctl.DeleteColumn(0);
		m_listctl.DeleteColumn(0);
		m_listctl.DeleteColumn(0);
		m_listctl.DeleteColumn(0);
		m_listctl.DeleteColumn(0);
	}
	if (m_menu_type==2)
		Init_function_list(flag);
	else if (m_menu_type==3)
	{
		m_listctl.m_HeaderCtrl.HideFilterArea();
		Init_Seperator_list();
	}
	else
		Init_menu_list(flag);
	m_listctl.m_HeaderCtrl.AddFilterArea(1, m_menu_type, this, m_filter1, m_filter2, m_filter3);
	m_listctl.m_HeaderCtrl.AddChoiceArea(1, m_menu_type, this, m_filter2);
}

/***********************************************************************
c
c   FUNCTION: Set_menu_data(UDM_menu_struc *menu_item)
c
c      copy 'menu_item' into the local dialog m_menu_item value
c
c   INPUT: menu_item: menu data to be copied
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
void CNCLMenuDsnDlg::Set_menu_data(UDM_menu_struc *menu_item)
{
/*
......copy 'menu_item' into the local dialog m_menu_item value
*/
	int len;
	uu_move_byte((char*)menu_item, (char*)&(m_menu_item), sizeof(UDM_menu_struc));
	if (menu_item->params!=NULL)
	{
		len = strlen(menu_item->params);
		if (len>0)
		{
			m_menu_item.params = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(m_menu_item.params, menu_item->params);
		}
		else
			m_menu_item.params = (char*)UU_NULL;
	}
	else
		m_menu_item.params = (char*)UU_NULL;
}

/////////////////////////////////////////////////////////////////////////////
// CNCLMenuDsnDlg message handlers

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
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
BOOL CNCLMenuDsnDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
	int statbar = 0;
	m_filter1[0] = '\0';
	m_filter2[0] = '\0';
	m_filter3[0] = '\0';
	m_listctl.SetParent(this);
	CNCLHeaderCtrl *head = (CNCLHeaderCtrl *)m_listctl.GetHeaderCtrl();
	head->ModifyStyle(0, HDS_FILTERBAR);

	int i, cont;
	CComboBox *cmbox = ((CComboBox*)GetDlgItem(IDC_COMBO1));
	if (cmbox!=NULL)
	{
		statbar = 1;
		cont = cmbox->GetCount();
		for (i = 0; i<cont; i++)
		{
			((CComboBox*)GetDlgItem(IDC_COMBO1))->DeleteString(0);
		}
		((CComboBox*)GetDlgItem(IDC_COMBO1))->AddString("None");
		i = 0;
		while (UU_TRUE)
		{
			if (strcmp(UZ_statfuncs[i].name,"~END~") == 0) break;
			((CComboBox*)GetDlgItem(IDC_COMBO1))->AddString(UZ_statfuncs[i].name);
			i++;
		}
		GetDlgItem(IDC_EDIT5)->SetWindowText("");
	}
	if (m_modal==0)
	{
		GetDlgItem(IDC_EDIT1)->SetWindowText("");
		GetDlgItem(IDC_EDIT2)->SetWindowText("");
		GetDlgItem(IDC_EDIT3)->SetWindowText("");
		GetDlgItem(IDC_EDIT4)->SetWindowText("");
		if (statbar==1)
		{
			((CComboBox*)GetDlgItem(IDC_COMBO1))->SetCurSel(0);
			GetDlgItem(IDC_EDIT5)->EnableWindow(0);
		}
	}
	else
	{
		GetDlgItem(IDC_EDIT1)->SetWindowText(m_menu_item.name);
		if (m_menu_item.params==NULL)
			GetDlgItem(IDC_EDIT2)->SetWindowText("");
		else
			GetDlgItem(IDC_EDIT2)->SetWindowText(m_menu_item.params);
		GetDlgItem(IDC_EDIT3)->SetWindowText(m_menu_item.descrip);
		GetDlgItem(IDC_EDIT4)->SetWindowText("");
		if (statbar==1)
		{
			if (m_menu_item.statname[0]=='\0')
			{
				((CComboBox*)GetDlgItem(IDC_COMBO1))->SetCurSel(0);
				GetDlgItem(IDC_EDIT5)->EnableWindow(0);		
			}
			else
			{
				int i = 0, type = -1;
				while (UU_TRUE)
				{
					if (strcmp(UZ_statfuncs[i].name,"~END~") == 0) break;
					if (strcmp(UZ_statfuncs[i].name, m_menu_item.statname) == 0) 
					{
						type = UZ_statfuncs[i].sub;
						break;
					}
					i++;
				}
				((CComboBox*)GetDlgItem(IDC_COMBO1))->SetCurSel(type+1);
				if (type==-1)
					GetDlgItem(IDC_EDIT5)->EnableWindow(0);		
				else
					GetDlgItem(IDC_EDIT5)->EnableWindow(1);		
			}
		}
	}	

	if (m_modal==0)
		m_listctl.InitDrag();
	Reload_menulist();
	if (m_modal==0)
	{
		Init_function_list(1);
	}
	else
	{
		if (m_menu_item.chcfile[0]!='\0')
		{
			strcpy(m_menu_name, m_menu_item.chcfile);
			m_menu_type = 0;
		}
		else if (m_menu_item.file[0]=='\0')
		{
			if (m_menu_item.separator)
				m_menu_type = 3;
			else
				m_menu_type = 2;
		}
		else
		{
			int type,sub;
			short app;
			if (uz_which_keydef(m_menu_item.file,&type,&sub,&app) == UU_SUCCESS)
				m_menu_type = 2;
			else
				m_menu_type = 1;
		}
		if (m_menu_type==2)
			Init_function_list(1);
		else if (m_menu_type==3)
			Init_Seperator_list();
		else
			Init_menu_list(1);
	}
	if (m_modal==0)
	{
		GetDlgItem(IDOK)->ShowWindow(SW_HIDE);
		GetDlgItem(IDCANCEL)->SetWindowText("Close");
	}
	if ((m_menu_type==0)||(m_menu_type==1))
		(GetDlgItem(IDC_LABEL))->SetWindowText("Selected Button Menu:");
	else
		(GetDlgItem(IDC_LABEL))->SetWindowText("Selected Button Function:");
	char chcstr[20];
	if (m_menu_type==0)
	{
		(GetDlgItem(IDC_LABEL1))->ShowWindow(SW_HIDE);
		(GetDlgItem(IDC_EDIT1))->ShowWindow(SW_HIDE);
		(GetDlgItem(IDC_LABEL2))->ShowWindow(SW_SHOW);
		(GetDlgItem(IDC_EDIT6))->ShowWindow(SW_SHOW);
		if ((m_modal!=0)&&(m_menu_item.chcdef>=0))
		{
			sprintf(chcstr, "%d", m_menu_item.chcdef);
			(GetDlgItem(IDC_EDIT6))->SetWindowText(chcstr);
		}
		else
			(GetDlgItem(IDC_EDIT6))->SetWindowText("");
	}
	else
	{
		(GetDlgItem(IDC_LABEL2))->ShowWindow(SW_HIDE);
		(GetDlgItem(IDC_EDIT6))->ShowWindow(SW_HIDE);
		(GetDlgItem(IDC_LABEL1))->ShowWindow(SW_SHOW);
		(GetDlgItem(IDC_EDIT1))->ShowWindow(SW_SHOW);
	}
	if (statbar==1)
		(GetDlgItem(IDC_STATLABEL))->ShowWindow(SW_SHOW);
/*
.....no default button set which will cause RETURN to execute the default OK button
*/
	SetDefID(IDF_FORM_RETURN);
	m_listctl.m_HeaderCtrl.AddFilterArea(1, m_menu_type, this, m_filter1, m_filter2, m_filter3);
	m_listctl.m_HeaderCtrl.AddChoiceArea(1, m_menu_type, this, m_filter2);

	CenterWindow();
	return FALSE;  // return TRUE  unless you set the focus to a control
}
/***********************************************************************
c
c   FUNCTION: Save_menu_data(int *statflag)
c
c      save the current dialog input into the local dialog m_menu_item value
c
c   INPUT: none
c
c   OUTPUT :   none
c   RETURN:    None
c
**********************************************************************/
int CNCLMenuDsnDlg::Save_menu_data(int *statflag)
{
	*statflag = 0;
	if (m_menu_type==3)
	{
		m_menu_item.separator = 1;
		m_menu_item.toggle_num = 0;
		m_menu_item.toggle = 0;
		m_menu_item.chcfile[0] = '\0';
		m_menu_item.chcdef = 0;
		m_menu_item.name[0] = '\0';
		m_menu_item.file[0] = '\0';
		m_menu_item.statname[0] = '\0';
		m_menu_item.params = (char*)UU_NULL;
		m_menu_item.descrip[0] = '\0';
		m_menu_item.bgcolor[0] = '\0';
		m_menu_item.color[0] = -1;
		m_menu_item.color[1] = -1;
		m_menu_item.color[2] = -1;
		m_menu_item.pos[0] = -1;
		m_menu_item.pos[1] = -1;
		m_menu_item.size[0] = -1;
		m_menu_item.size[0] = -1;
		m_menu_item.bmpfile[0] = '\0';
		m_menu_item.bmpnum = 0;
		return 0;
	}
	int i, nc, status, sub;
	char name[30], numstr[80];
	char strValue[UX_MAX_FILE_LEN];
	char descrip[40];

	m_menu_item.separator = 0;
	if (m_menu_type!=0)
	{
		GetDlgItem(IDC_EDIT1)->GetWindowText(name, 29);
		if (strlen(name)!=0)
			strcpy(m_menu_item.name, name);
		else
			strcpy(m_menu_item.name, " ");
		m_menu_item.chcdef = 0;
	}
	else
	{
		GetDlgItem(IDC_EDIT6)->GetWindowText(name, 29);
		if (name[0]!='\0')
			m_menu_item.chcdef = atoi(name);
		else
			m_menu_item.chcdef = 0;
		m_menu_item.name[0] = '\0';
	}
	int type;
	CComboBox *cmbbox = ((CComboBox*)GetDlgItem(IDC_COMBO1));
	if (cmbbox!=NULL)
		type = cmbbox->GetCurSel() - 1;
	else
		type = -1;
	if (type>=0)
	{	
		int inum;
		UU_REAL rval[2];
		strcpy(m_menu_item.statname, UZ_statfuncs[type].name);
		*statflag = 1;
		GetDlgItem(IDC_EDIT5)->GetWindowText(numstr, 80);
		if (numstr[0]!='\0')
		{
			if ((ul_to_reals(rval, &inum, 2, numstr) != UU_SUCCESS) ||
				inum != 2)
			{
				MessageBox("Button Size Value is Wrong!",
						"Error!", MB_OK);
				return -1;
			}
			m_menu_item.size[0] = rval[0];
			m_menu_item.size[1] = rval[1];
		}
		else
		{
			m_menu_item.size[0] = -1;
			m_menu_item.size[1] = -1;
		}
	}
	else
		m_menu_item.statname[0] = '\0';
	GetDlgItem(IDC_EDIT2)->GetWindowText(strValue, UX_MAX_FILE_LEN);
	nc = strlen(strValue);
	if (nc>0)
	{
		m_menu_item.params = (char*)uu_malloc((nc+1)*sizeof(char));
		strcpy(m_menu_item.params, strValue);
	}
	else
		m_menu_item.params = 0;
	GetDlgItem(IDC_EDIT3)->GetWindowText(descrip, 39);
	nc = strlen(descrip);
	if (nc>0)
	{
		strcpy(m_menu_item.descrip, descrip);
	}
	else
		m_menu_item.descrip[0] = '\0';
	char *p;
	if (m_menu_type==0)
	{
		if (m_menu_name[0]!='\0')
			strcpy(m_menu_item.chcfile, m_menu_name);
		else
		{
			MessageBox("You Must Select A Menu Name from the Menu List!",
						"Error!", MB_OK);
			return -1;
		}
		m_menu_item.file[0] = '\0';
		strcpy(m_menu_item.bmpfile, m_menu_name);
		p = (char *)strrchr(m_menu_item.bmpfile,'.');
		if (p == 0)
			strcat(m_menu_item.bmpfile, ".bmp");
		else
		{
			*p = 0;
			strcat(m_menu_item.bmpfile, ".bmp");
		}
	}
	else if (m_menu_type==1)
	{
		if (m_menu_name[0]!='\0')
			strcpy(m_menu_item.file, m_menu_name);
		else
		{
			MessageBox("You Must Select A Menu Name from the Menu List!",
						"Error!", MB_OK);
			return -1;
		}
		m_menu_item.chcfile[0] = '\0';
		strcpy(m_menu_item.bmpfile, m_menu_name);
		p = (char *)strrchr(m_menu_item.bmpfile,'.');
		if (p == 0)
			strcat(m_menu_item.bmpfile, ".bmp");
		else
		{
			*p = 0;
			strcat(m_menu_item.bmpfile, ".bmp");
		}
	}
	else if (m_menu_type==2)
	{
		if (m_func_name[0]!='\0')
			strcpy(m_menu_item.file, m_func_name);
		else
		{
			MessageBox("You Must Select A Function Name from the Function List!",
						"Error!", MB_OK);
			return -1;
		}
		m_menu_item.chcfile[0] = '\0';
		strcpy(m_menu_item.bmpfile, m_func_name);
		strcat(m_menu_item.bmpfile, ".bmp");
	}
	else
		m_menu_item.bmpfile[0] = '\0';
	return 0;
}
/***********************************************************************
**
**   SUBROUTINE: OnOK()
**
**   FUNCTION:  callback for "OK" button
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLMenuDsnDlg::OnOK()
{	
	int statflag;
	int status = Save_menu_data(&statflag);
	if (m_modal==0)
	{
		NCL_MainFrame->m_menudsgn_dlg->ShowWindow(SW_HIDE);
	}
	else
	{
		if (status==-1)
			return;
		CDialog::OnOK();
	}
}
/***********************************************************************
**
**   SUBROUTINE: OnCancel()
**
**   FUNCTION:  callback for "Cancel" button
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLMenuDsnDlg::OnCancel()
{	
	if (m_modal==0)
	{
		NCL_MainFrame->m_menudsgn_dlg->ShowWindow(SW_HIDE);
	}
	else
	{
		CDialog::OnCancel();
	}
}
/*...not used*/
void CNCLMenuDsnDlg::OnItemChanged(NMHDR *pNMHDR, LRESULT *pResult)
{
	UZ_keycalls *data_ptr;
	*pResult = 0;

	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	if (pNMListView->iItem==0)
		return;
	CNCLTableList *list = (CNCLTableList*)&m_listctl;
	if ((pNMListView->uNewState & LVIS_SELECTED )&&( pNMListView->uOldState==0))
	{
		int selectedItem = pNMListView->iItem;
		data_ptr = (UZ_keycalls*)list->GetItemData(selectedItem);
	}
}
/***********************************************************************
**
**   SUBROUTINE: OnColumnclick(NMHDR* pNMHDR, LRESULT* pResult)
**
**   FUNCTION:  callback for click on header button
**   INPUT:  
**			pNMHDR: header data 
**   OUTPUT: pResult:
**	 RETURN: none
**
***********************************************************************/
void CNCLMenuDsnDlg::OnColumnclick(NMHDR* pNMHDR, LRESULT* pResult)
{
	NMLISTVIEW *pLV = (NMLISTVIEW *) pNMHDR;	
	int col = pLV->iSubItem;
	int bAscending;
	if (col==m_sort_col)
	{
		bAscending = !m_bSortAscending;
	}
	else
		bAscending = TRUE;
	m_sort_col = col;
	m_bSortAscending = bAscending;
	if (m_menu_type==2)
	{
		if (m_bSortAscending)
			m_listctl.SortItems((PFNLVCOMPARE)MenuSortFunc1, col);
		else
			m_listctl.SortItems((PFNLVCOMPARE)MenuSortFunc2, col);
	}
	else
	{
		if (m_bSortAscending)
			m_listctl.SortItems((PFNLVCOMPARE)MenuSortFunc3, col);
		else
			m_listctl.SortItems((PFNLVCOMPARE)MenuSortFunc4, col);
	}
	*pResult = 0;
}

/***********************************************************************
**
**   SUBROUTINE: OnDBclickTable(NMHDR *pNMHDR, LRESULT *pResult)
**
**   FUNCTION:  callback for double click on a list item
**   INPUT:  
**			pNMHDR: list view data
**   OUTPUT: pResult:
**	 RETURN: none
**
***********************************************************************/
void CNCLMenuDsnDlg::OnDBclickTable(NMHDR *pNMHDR, LRESULT *pResult)
{
/*
......the LBUTTONUP event for CNCLListCtl is not called if this callback is set
.....so we need call then to let it know here
*/
	m_listctl.OnCtlClicked();
	*pResult = 0;
	if (m_menu_type==3)
		return;
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	CNCLTableList *list = (CNCLTableList*)&m_listctl;
    if (  ( pNMListView->uOldState & LVIS_SELECTED ) && 
         ( pNMListView->uNewState == 0 )  )
    {
		if ((list->m_selitem==pNMListView->iItem)&&(list->m_change==0))
		{
			list->SetItemState(pNMListView->iItem, 0, LVIS_SELECTED | LVIS_FOCUSED);	
			list->m_selitem = -1;
			*pResult = 1;
			return;
		}
	}
	UZ_keycalls *data_ptr;
	list->m_change = 0;
	list->m_selitem = pNMListView->iItem;
	if (pNMListView->iItem==-1)
		return;
	if ((m_menu_type==0)||(m_menu_type==1))
	{
		UZ_menuinfo *data_ptr;
		data_ptr = (UZ_menuinfo*)list->GetItemData(list->m_selitem);
		strcpy(m_menu_name, data_ptr->name);
		GetDlgItem(IDC_EDIT4)->SetWindowText(m_menu_name);
		
		if (m_modal!=0)
			goto done;
		int size[2], pos[2];
		char buf[UX_MAX_FILE_LEN];
		pos[0] = -1;
		pos[1] = -1;
		size[0] = -1;
		size[1] = -1;
		if (udm_read_menu(m_menu_name,pos,size, 1, 1, -1) != UU_SUCCESS)
		{
			sprintf(buf, "Could not load menu: %s", m_menu_name);
			uw_nterror(buf);
		}
	}
	else if (m_menu_type==2)
	{
		UZ_keycalls *data_ptr;
		data_ptr = (UZ_keycalls*)list->GetItemData(list->m_selitem);
		strcpy(m_func_name, data_ptr->name);
		char params[UX_MAX_FILE_LEN];
		GetDlgItem(IDC_EDIT2)->GetWindowText(params, UX_MAX_FILE_LEN);
		GetDlgItem(IDC_EDIT4)->SetWindowText(m_func_name);
		if (m_modal!=0)
			goto done;
		uz_func_call(m_func_name, params);
	}
done:;
}
/***********************************************************************
**
**   SUBROUTINE: OnSelTable(NMHDR *pNMHDR, LRESULT *pResult)
**
**   FUNCTION:  callback for sigle click (select)
**   INPUT:  
**			pNMHDR: list view data
**   OUTPUT: pResult:
**	 RETURN: none
**
***********************************************************************/
void CNCLMenuDsnDlg::OnSelTable(NMHDR *pNMHDR, LRESULT *pResult)
{
/*
......the LBUTTONUP event for CNCLListCtl is not called if this callback is set
.....so we need call then to let it know here
*/
	m_listctl.OnCtlClicked();
	*pResult = 0;
	if (m_menu_type==3)
		return;
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	CNCLTableList *list = (CNCLTableList*)&m_listctl;
    if (  ( pNMListView->uOldState & LVIS_SELECTED ) && 
         ( pNMListView->uNewState == 0 )  )
    {
		if ((list->m_selitem==pNMListView->iItem)&&(list->m_change==0))
		{
			list->SetItemState(pNMListView->iItem, 0, LVIS_SELECTED | LVIS_FOCUSED);	
			list->m_selitem = -1;
			*pResult = 1;
			return;
		}
	}
	list->m_change = 0;
	if (pNMListView->iItem==-1)
		return;
	list->m_selitem = pNMListView->iItem;	
	UpdatedSel(list->m_selitem);
}
/***********************************************************************
**
**   SUBROUTINE: UpdatedSel(int item)
**
**   FUNCTION:  updated the dialog field followed a selection
**   INPUT:  
**			item: select item
**   OUTPUT: pResult:
**	 RETURN: none
**
***********************************************************************/
void CNCLMenuDsnDlg::UpdatedSel(int item)
{
	CNCLTableList *list = (CNCLTableList*)&m_listctl;
	list->m_selitem = item;
	if ((m_menu_type==0)||(m_menu_type==1))
	{
		UZ_menuinfo *data_ptr;
		data_ptr = (UZ_menuinfo*)list->GetItemData(list->m_selitem);
		strcpy(m_menu_name, data_ptr->name);
		GetDlgItem(IDC_EDIT4)->SetWindowText(m_menu_name);
		GetDlgItem(IDC_EDIT3)->SetWindowText(data_ptr->descrip);
	}
	else if (m_menu_type==2)
	{
		UZ_keycalls *data_ptr;
		data_ptr = (UZ_keycalls*)list->GetItemData(list->m_selitem);
		strcpy(m_func_name, data_ptr->name);
		GetDlgItem(IDC_EDIT4)->SetWindowText(m_func_name);
		GetDlgItem(IDC_EDIT3)->SetWindowText(data_ptr->descrip);
	}
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
/*
.....not really used now
*/
BOOL CNCLMenuDsnDlg::OnNotify( WPARAM wParam, LPARAM lParam, LRESULT* pResult )
{
	int i;
	NMHEADER *NMhdr = ((NMHEADER*)lParam);
	if (NMhdr->hdr.idFrom==0)
			i=i;
		if (NMhdr->hdr.code==NM_CUSTOMDRAW)
			i=i;
		if (NMhdr->hdr.code==HDN_FILTERBTNCLICK)
			i=i;
		if (NMhdr->hdr.code==HDN_FILTERCHANGE)
			i=i;
	if (NMhdr->hdr.idFrom==IDC_TABLE_LIST)
	{
		if (NMhdr->hdr.code==NM_CUSTOMDRAW)
			i=i;
		if (NMhdr->hdr.code==HDN_FILTERBTNCLICK)
			i=i;
	}
	return CDialog::OnNotify(wParam, lParam, pResult);
}

/***********************************************************************
**
**   SUBROUTINE: uw_ntsave_cur_menudegn()
**
**   FUNCTION:  saved currect menu design data
**   INPUT:  
**			item: select item
**   OUTPUT: statflag: 1, the menu item is a status button
**	 RETURN: none
**
***********************************************************************/
extern "C" int uw_ntsave_cur_menudegn(int *statflag)
{
	return NCL_MainFrame->m_menudsgn_dlg->Save_menu_data(statflag);
}
