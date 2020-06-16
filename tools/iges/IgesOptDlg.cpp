/************************************************************************
c
c   FILE NAME: IgesOptDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class IgesOptDlg 
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			IgesOptDlg.cpp , 25.4
c    DATE AND TIME OF LAST  MODIFICATION
c			11/22/17 , 11:38:41
c
c**********************************************************************
*/
// IgesOptDlg.cpp : implementation file
//

#include "wsntstdafx.h"
#include <io.h>
#include "wsntres.h"
#include "iges.h"
#include "IgesOptDlg.h"
#include "tiges.h"
#include "xenv1.h"
#include "ulist.h"
#include "IgesFiltDlg.h"
#include "IgesModalDlg.h"
#include "IgesAttrDlg.h"
#include "wsgl.h"
#include "wsntclrdlg.h"
#include <gdiplus.h>
using namespace Gdiplus; 
struct IG_igesclr_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	red;
	UU_REAL	green;
	UU_REAL	blue;
	char	name[20];
};

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" char iges_fname[UX_MAX_PATH_LEN];
extern "C" char iges_tmpfile[UX_MAX_PATH_LEN];

extern "C" int MAX_PARA_REC;
extern "C" int UG_def_line_wt;
extern "C" int tig_num_range, tig_range[IG_LAYER][2];
extern "C" int UIG_decompose_chc;

static int All_layer[IG_LAYER];

extern "C" int ul_break_fname(char*, char*, char*);
extern "C" int ul_get_full_dir(char* dir, char* fullname);
extern "C" int ux_delete0(char*);
extern "C" int uig_unb_opnfil(char*);
extern "C" int uig_terminal_rec();
extern "C" int uu_toolmalloc(int);
extern "C" int uu_free(UIG_color_struct *);
extern "C" int uig_get_data(int, int, struct dir_rec *, char*);
extern "C" int uu_toolfree(char*);
extern "C" int iges_config(int );
extern "C" int uig_update_surface_prim();
extern "C" int ur_saveu_active(int );
extern "C" int uig_open_secondary(char unifile[],int *index);
extern "C" int uig_bubble_sort(int*, int);
extern "C" char *ux_getenv(char*);
extern "C" int uig_get_global(struct global_rec *);
extern "C" void ul_short_filename(char *fin, char *fout, int maxc);
extern "C" int uig_in_color(struct IG_igesclr_rec *dblk, int *indx, int *icolor);

extern "C" int UIG_color_iges;
extern "C" int UIG_max_lab;
extern "C" int UIG_use_sub;
extern "C" int UIG_concat_sub;
extern "C" int UIG_decompose;

extern "C" int uig_load_color_mode(char *);
extern "C" void uig_save_custom_color();	
extern "C" void uig_append_custom_color(int);
extern "C" void uig_init_color();

/////////////////////////////////////////////////////////////////////////////
// CIgesOptDlg dialog


CIgesOptDlg::CIgesOptDlg(CWnd* pParent, char *fname)
	: CDialog(CIgesOptDlg::IDD, pParent)
{
	if (fname==NULL)
		m_fname[0] = '\0';
	else if (fname[0]=='\0')
		m_fname[0] = '\0';
	else
		strcpy(m_fname, fname);
	m_edge_color = UIG_edge_color;
}


void CIgesOptDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CIgesOptDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CIgesOptDlg, CDialog)
	//{{AFX_MSG_MAP(CIgesOptDlg)
	ON_BN_CLICKED(IDC_SELECT_ALL, OnSelectAll)
	ON_BN_CLICKED(IDC_CLEAR_ALL, OnClearAll)
	ON_CBN_SELCHANGE(IDC_COPTION, OnSelchangeCoption)
	ON_CBN_SELCHANGE(IDC_LABEL_OPTION, OnSelchangeLabelOption)
	ON_BN_CLICKED(IDC_BROWSE, OnBrowse)
	ON_BN_CLICKED(IDC_FILTERE, OnFilter)
	ON_BN_CLICKED(IDC_MODALSN, OnNameModals)
	ON_BN_CLICKED(IDC_ATTRIBUTES, OnAttr)
	ON_BN_CLICKED(IDC_CHECK_NODUPS, OnNoDups)
	ON_BN_CLICKED(IDC_SRF_EDGE, OnEdgeColor)
	ON_BN_CLICKED(IDC_EDGE_COLR_NEW, OnEdgeColorSel)
	ON_BN_CLICKED(IDC_SHADE, OnShade)
	ON_BN_CLICKED(IDC_DECOMPOSE, OnDecompose)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIgesOptDlg message handlers

/***********************************************************************
c
c   FUNCTION: OnSelectAll() 
c
c		This function called when user push the "Select All" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnSelectAll() 
{
	CListBox* lstbox = (CListBox*)GetDlgItem(IDC_LAYER_LIST);
	int cont = lstbox->GetCount();
	for (int i = 0; i<cont; i++)
	{
		lstbox->SetSel(i, TRUE);
	}
}

/***********************************************************************
c
c   FUNCTION: OnOK() 
c
c		This function called when user push the "OK" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnOK() 
{
	int status,index, len;
	CString tmpstr;
	UX_pathname unifile;
	char msg[300],toler[10],in_mm[10],*text;
	int sel_num,srf_edge,edge_color;
	int ans[IG_LAYER];
	int count =0,j,i = 0;
	CListBox* lstbox = (CListBox*)GetDlgItem(IDC_LAYER_LIST);
	if (lstbox->GetCurSel()==0)
		tig_num_range = 0;	
	else
	{
		sel_num = lstbox->GetSelItems(IG_LAYER, ans) ;
		if (sel_num==0) tig_num_range = 0;
		else
		{
			for (i=0; i<sel_num; i++)
			{
				tig_range[i][0] = tig_range[i][1] = All_layer[ans[i]];
			}
			tig_num_range = sel_num;
		}
	}
	label_type = ((CComboBox*)GetDlgItem(IDC_LABEL_OPTION))->GetCurSel()+1;
	UIG_use_sub    = ((CButton*)GetDlgItem(IDC_CHECK_USE_SUBSCRIPT))->GetState();
	UIG_concat_sub = ((CButton*)GetDlgItem(IDC_CHECK_CONCAT_SUBSCRIPT))->GetState();
	UIG_max_lab    = ((CComboBox*)GetDlgItem(IDC_MAX_LAB))->GetCurSel();
/*
.....
*/
	if (label_type==2)
	{
		label_type = 3;
		if (UIG_use_sub==0 && UIG_concat_sub==0)
		{
			if (UIG_max_lab == 6) label_type = 7;
		}
		else if (UIG_use_sub==1 && UIG_concat_sub==0)
			label_type = 4;
		else if (UIG_concat_sub==1)
		{
			if (UIG_max_lab == 1)
				label_type = 5;
			else
				label_type = 9;
		}
	}
	else if (label_type==3)
	{
		label_type = 6;
		if (UIG_use_sub==0) label_type = 10;
	}
	else if (label_type==4)
	{
/*
.....If label type is 4, set to 8, get secondary unibase
*/
		label_type = 8;
		((CWnd*)GetDlgItem(IDC_UNIBASEL))->GetWindowText(tmpstr);
		len = tmpstr.GetLength();
		if (len!=0)
		{
			text = tmpstr.GetBuffer(UX_MAX_PATH_LEN);
			strcpy(unifile, text);
		}
		else
			unifile[0] = '\0';
		status = uig_open_secondary(unifile,&index);
		if (status == UU_SUCCESS)
		{
			ur_saveu_active(2);
			if (index <= 9)
				uig_update_surface_prim();
			ur_saveu_active(1);
			
			UIG_matchlevel = ((CComboBox*)GetDlgItem(IDC_COMBO_MATCHLEVEL))->GetCurSel();
			UIG_unmatch_sec = ((CButton*)GetDlgItem(IDC_CHK_UNMATCH_SEC))->GetState();	
			UIG_start_unmatch = ((CComboBox*)GetDlgItem(IDC_COMBO_STARTUNMATCH))->GetCurSel();
			UIG_regressive = ((CButton*)GetDlgItem(IDC_CHK_REG_MATCH))->GetState();
		}
		else
			label_type =1;
	}
	((CWnd*)GetDlgItem(IDC_MATCHTOL))->GetWindowText(tmpstr);
	len = tmpstr.GetLength();
	text = tmpstr.GetBuffer(len);
/*
.....Parse the string to get the tolerance and the units(mm/in)
*/
	count =0;
	toler[0]='\0';
	in_mm[0]='\0';
	for (i = 0; i < len; i++)
	{
		if (text[i] == '.')
		{
			if(count == 0) count = 1;
			else  
			{
				sprintf(msg,"Text input not in format required by tolerance.");
				MessageBox(msg, "Error!", MB_OK);
				goto use_mod;
			}
		}
		else
		{
			if (isalpha(text[i]))
			{
				strncpy(toler,text,i);
				toler[i]='\0';
				sscanf((text+i), "%s", in_mm);
				if(strlen(in_mm) ==2)
				{
					if((in_mm[0] == 'm' || in_mm[0] == 'M') && 
						(in_mm[1] == 'm'||in_mm[1] == 'M'))
						UIG_units_factor =25.4;
					else if((in_mm[0] == 'i' || in_mm[0] == 'I') && 
								(in_mm[1] == 'n'||in_mm[1] == 'N'))
						UIG_units_factor =1;
					else 
					{
						sprintf(msg,
							"Text input not in format required by tolerance.");
						MessageBox(msg, "Error!", MB_OK);
						goto use_mod;
					}
					break;
				}
				else 
				{
					sprintf(msg,
						"Text input not in format required by tolerance.");
					MessageBox(msg, "Error!", MB_OK);
					goto use_mod;
				}
			}
			else if (isdigit(text[i]))
			{
				if(i == len-1)
					strcpy(toler,text);
			}
			else 
			{
				sprintf(msg,
					"Text input not in the format required by tolerance.");
				MessageBox(msg, "Error!", MB_OK);
				goto use_mod;
			}
		}
	}
	sscanf(toler,"%lf", &UIG_match_tol_disp);
	UIG_match_tol = UIG_match_tol_disp / UIG_units_factor;
	if (UIG_match_tol < .0001)
		UIG_match_tol = .0001;
use_mod:;
	if (UIG_units_factor == 1)
		UIG_match_tol = UIG_match_tol_disp;
	if (UIG_units_factor == 25.4)
		UIG_match_tol = UIG_match_tol_disp / UIG_units_factor;

/*
.....Added for importing surfaces as shaded or unshaded. JLS 7/29/99
*/
	shade_set = ((CButton*)GetDlgItem(IDC_SHADE))->GetState();
/*
......translucency
*/
	((CWnd*)GetDlgItem(IDC_LUCENCY))->GetWindowText(tmpstr);
	len = tmpstr.GetLength();
	text = tmpstr.GetBuffer(len);
	UIG_lucency = atoi(text);
/*
.....Set re-initialize label counter flag.
*/
	UIG_reinit_lab = ((CButton*)GetDlgItem(IDC_CHECK_REINIT_LAB))->GetState();
/*
.....Set no duplicates flag.
*/
	UIG_nodups = ((CButton*)GetDlgItem(IDC_CHECK_NODUPS))->GetState();
/*
.....Set the color scheme.
*/
	UIG_color_iges = ((CButton*)GetDlgItem(IDC_COLOR))->GetState();
/*
.....Set no duplicates flag.
*/
	UIG_decompose = ((CButton*)GetDlgItem(IDC_DECOMPOSE))->GetState();
	if (UIG_decompose)
		UIG_decompose_chc = ((CComboBox*)GetDlgItem(IDC_DECOMPOSE_CHC))->GetCurSel();
/*
...Default; White; Yellow; Blue; Red; Green; Magenta; Cyan; Black; Brown; Tan; Lt Blue; Sea Green; Orange; Pink; Purple; Grey
*/
	srf_edge    = ((CButton*)GetDlgItem(IDC_SRF_EDGE))->GetState();
	if (srf_edge)
	{
		if (m_edge_color == -1)
			UIG_edge_color = UIG_MAXCOLOR;
		else
		{
			UIG_edge_color = m_edge_color;
		}
	}
	UIG_srf_edge = srf_edge;
	CDialog::OnOK();
}


/***********************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c		This function Set the initialized parameter into dialog
c       This member function is called in response to 
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
BOOL CIgesOptDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

/*
.....Initialize these parameters according to the modfile parameters.
*/	
	((CComboBox*)GetDlgItem(IDC_COPTION))->SetCurSel(UIG_conv_opt);
	((CComboBox*)GetDlgItem(IDC_MAX_LAB))->SetCurSel(UIG_max_lab);
	((CButton*)GetDlgItem(IDC_CHECK_USE_SUBSCRIPT))->SetCheck(UIG_use_sub);
	((CButton*)GetDlgItem(IDC_CHECK_CONCAT_SUBSCRIPT))->SetCheck(UIG_concat_sub);
	((CComboBox*)GetDlgItem(IDC_LABEL_OPTION))->SetCurSel(UIG_lab_opt);
	((CComboBox*)GetDlgItem(IDC_COMBO_MATCHLEVEL))->SetCurSel(UIG_matchlevel);
	((CComboBox*)GetDlgItem(IDC_COMBO_STARTUNMATCH))->SetCurSel(UIG_start_unmatch);
	((CButton*)GetDlgItem(IDC_SHADE))->SetCheck(shade_set);
	
	CString ctmp;
	char snum[40];
	sprintf(snum,"%d", UIG_lucency);
	ctmp = snum;
	((CWnd*)GetDlgItem(IDC_LUCENCY))->SetWindowText(ctmp);

	((CButton*)GetDlgItem(IDC_CHECK_REINIT_LAB))->SetCheck(UIG_reinit_lab);
	((CButton*)GetDlgItem(IDC_CHK_UNMATCH_SEC))->SetCheck(UIG_unmatch_sec);
	((CButton*)GetDlgItem(IDC_CHK_REG_MATCH))->SetCheck(UIG_regressive);
	((CButton*)GetDlgItem(IDC_CHECK_NODUPS))->SetCheck(UIG_nodups);
	((CButton*)GetDlgItem(IDC_COLOR))->SetCheck(UIG_color_iges);
	((CButton*)GetDlgItem(IDC_DECOMPOSE))->SetCheck(UIG_decompose);
	if ((UIG_decompose)&&(UIG_nodups))
	{	
		GetDlgItem(IDC_DECOMPOSE_CHC)->EnableWindow(TRUE);
	}
	else
	{
		UIG_decompose_chc = 0;
		GetDlgItem(IDC_DECOMPOSE_CHC)->EnableWindow(FALSE);
	}
	((CComboBox*)GetDlgItem(IDC_DECOMPOSE_CHC))->SetCurSel(UIG_decompose_chc);
	((CButton*)GetDlgItem(IDC_SRF_EDGE))->SetCheck(UIG_srf_edge);

	InitColor();

	int color;

	color = UIG_edge_color;
	if (color == UIG_MAXCOLOR) color = -1;
	CRect rbtn;
	GetDlgItem(IDC_EDGE_COLR)->GetWindowRect(&rbtn);
	GetDlgItem(IDC_EDGE_COLR)->ShowWindow(SW_HIDE);
	ScreenToClient(&rbtn);
	m_button.Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY, 
		rbtn, this, IDC_EDGE_COLR_NEW);
	COLORREF bcolor;
	if (color<0)
	{
		bcolor = ::GetSysColor(COLOR_BTNFACE); 
	}
	else
	{
		bcolor = RGB(uw_color_table[color][0], 
					uw_color_table[color][1], 
					uw_color_table[color][2]);
	}
	m_button.set_color(bcolor, bcolor);
	m_button.ShowWindow(SW_SHOW);

	if (UIG_srf_edge)
	{	
		GetDlgItem(IDC_ECLABEL)->EnableWindow(TRUE);
		GetDlgItem(IDC_EDGE_COLR_NEW)->EnableWindow(TRUE);
	}
	else
	{
		GetDlgItem(IDC_ECLABEL)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDGE_COLR_NEW)->EnableWindow(FALSE);
	}
	InitRangeList();
	OnSelchangeCoption();
	OnSelchangeLabelOption();
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

/***********************************************************************
c
c   FUNCTION: InitRangeList()
c
c		This function get layer range and add in list
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::InitRangeList()
{
	struct global_rec gblk;
	int status,i,j,k;
	UX_pathname filename,fname,dir,tmp;
	char *cc, msg[300],unit[3];
	struct dir_rec dblk;
	char layer_str[IG_LAYER][5];
	char *p;
	CString ctmp;
/*
.....get layer range and add in list
*/
	strcpy(filename, m_fname);
/*
.....check if user entered filename
*/
	ul_break_fname(filename, dir, fname);
	if (strlen(fname)==0)
	{
		MessageBox("No input IGES data file specified! The option dialog may be empty", 
			"WARNING!", MB_OK);
	}
/*
.....if dir is local directory, need add the whole path
.....in order to compare or copy to iges_fname later
.....iges_fname need include the whole path.
*/
	if (dir[0]=='\0')
	{
/*
.....get local directory
*/
		ul_get_full_dir(".", dir);
	}
	strcat(dir, "\\");
	strcat(dir, fname);
	strcpy(filename, dir);
	status = 0;
	if (strcmp(filename, iges_fname)!=0)
	{
		if (iges_fname[0]!='\0')
		{
			_close(iges_fd);
			iges_fd = 0;
			if (ux_delete0(iges_tmpfile))
			{
				ul_short_filename(iges_fname,tmp,50);
				sprintf (msg, "can't delete %s corresponding %s", iges_tmpfile, tmp);
				MessageBox(msg, "Error!", MB_OK);
			}
			iges_fname[0] = '\0';
		}
		strcpy(tmp, filename);
		dbyte = pbyte = 0;
		status = uig_unb_opnfil(filename);
		if ( status == 0)
			strcpy(iges_fname, tmp);
		else
			iges_fname[0] = '\0';
	}
		
	if (iges_fname[0]!='\0')
	{
/*
.....Display the tolerance and the units.
*/
		unit[0] = '\0';
		uig_get_global(&gblk);
		if (gblk.units == 2 ) 
		{
			if(UIG_units_factor==1)
				UIG_match_tol_disp =UIG_match_tol * 25.4;
			UIG_units_factor = 25.4;
			strcpy(unit, "mm");
		}
		else if (gblk.units == 1) 
		{
			if(UIG_units_factor==25.4)
				UIG_match_tol_disp =UIG_match_tol ;
			UIG_units_factor = 1;
			strcpy(unit, "in");
		}
		if (UIG_match_tol < .0001)
		{
			if ((p=(char *)ux_getenv("UIG_MATCH_TOL")))
				UIG_match_tol = atof(p);
			else 
				UIG_match_tol = .001;
		}
		UIG_match_tol_disp = UIG_match_tol * UIG_units_factor;
		sprintf(tmp,"%6.4lf%s",UIG_match_tol_disp,unit);
		ctmp = tmp;
		GetDlgItem(IDC_MATCHTOL)->SetWindowText(ctmp);
		((CComboBox*)GetDlgItem(IDC_COMBO_MATCHLEVEL))->SetCurSel(UIG_matchlevel);

		uig_terminal_rec();
		cc = (char*) uu_toolmalloc(MAX_PARA_REC);
		j = 0;
		dbyte = pbyte = 0;
		for(i=1; i<sect_ptr[2]+1; i++, i++)
		{
			status = uig_get_data(i,1,&dblk,cc);
			for (k=0; k<j; k++)
			{
				if (dblk.level==All_layer[k]) 
				{
					break;
				}
			}
			if (k>=j && j<IG_LAYER)
			{
				All_layer[j] = dblk.level;
				j++;
			}
		}
		uu_toolfree(cc);
/*
.....sort layer number
*/
		uig_bubble_sort(All_layer, j);
		CListBox* lstbox = (CListBox*)GetDlgItem(IDC_LAYER_LIST);
		for (i=0; i<j; i++)
		{
			sprintf(layer_str[i], "%d", All_layer[i]);
			lstbox->AddString(layer_str[i]);
		}
		lstbox->SetCurSel(0);
	}
	else
	{
/*
.....Display the default tolerance and units from the mod file
.....if user does not enter a filename.
*/
		unit[0] = '\0';
		
		if (UIG_units_factor == 25.4 ) 
			strcpy(unit, "mm");
		else if (UIG_units_factor == 1) 
			strcpy(unit, "in");
		sprintf(tmp,"%6.4lf%s",UIG_match_tol_disp,unit);
		ctmp = tmp;
		GetDlgItem(IDC_MATCHTOL)->SetWindowText(ctmp);

	}
}

/***********************************************************************
c
c   FUNCTION: OnClearAll() 
c
c		This function called when user push the "Clear All" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnClearAll() 
{
	CListBox* lstbox = (CListBox*)GetDlgItem(IDC_LAYER_LIST);
	int cont = lstbox->GetCount();
	for (int i = 0; i<cont; i++)
	{
		lstbox->SetSel(i, FALSE);
	}
}

/***********************************************************************
c
c   FUNCTION: OnSelchangeCoption() 
c
c		This function called when user changed "Conversion Option" choices
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnSelchangeCoption() 
{
	CComboBox* cmbbox = (CComboBox*)GetDlgItem(IDC_COPTION);
	UIG_conv_opt=cmbbox->GetCurSel();
	if (cmbbox->GetCurSel()==1)
	{
		((CWnd*)GetDlgItem(IDC_SELECT_ALL))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_CLEAR_ALL))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_LAYER_LIST))->EnableWindow(TRUE);
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_SELECT_ALL))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_CLEAR_ALL))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_LAYER_LIST))->EnableWindow(FALSE);
	}
}

/***********************************************************************
c
c   FUNCTION: OnSelchangeLabelOption() 
c
c		This function called when user changed "Entity Label Option" choices
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnSelchangeLabelOption() 
{
	CComboBox* cmbbox = (CComboBox*)GetDlgItem(IDC_LABEL_OPTION);
	UIG_lab_opt =cmbbox->GetCurSel();
	int nodups,state = FALSE;
	int choice = cmbbox->GetCurSel() + 1;
	if (choice==4) state = TRUE;
	((CWnd*)GetDlgItem(IDC_UNIBASEP))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_UNIBASEL))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_BROWSE))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_MATCHLEVTEXT))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_COMBO_MATCHLEVEL))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_CHK_UNMATCH_SEC))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_CHK_REG_MATCH))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_ATTRIBUTES))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_COMBO_STARTUNMATCH))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_STARTUNMATCHTEXT))->EnableWindow(state);
/*
.....Keep the match tolerance field activated if the "No Duplicates"
.....check box is checked or the Enitity Label Ooption is set to From Existing
.....Unibase.
*/
	nodups = ((CButton*)GetDlgItem(IDC_CHECK_NODUPS))->GetCheck();
	if(nodups)state = TRUE;
	((CWnd*)GetDlgItem(IDC_MATCHTOL))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_MATCHTXT))->EnableWindow(state);
	state = FALSE;
	if (choice==2 || choice==3) state = TRUE;
	((CWnd*)GetDlgItem(IDC_CHECK_USE_SUBSCRIPT))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_CHECK_CONCAT_SUBSCRIPT))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_MAX_LAB))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_MAXLABTEXT))->EnableWindow(state);
	}

/***********************************************************************
c
c   FUNCTION: OnBrowse() 
c
c		This function called when user pushed "Browse" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnBrowse() 
{
	DWORD dwFlags;
	dwFlags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;
	LPCTSTR filter = "Unibase File (*.u)|*.u|All Files (*.*)|*.*||";		
	CFileDialog *filedlg = new CFileDialog(TRUE, "iges", NULL, dwFlags,
			filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	GetDlgItem(IDC_UNIBASEL)->SetWindowText(FileName);
	delete filedlg;	
}

/***********************************************************************
c
c   FUNCTION: OnFilter() 
c
c		This function called when user pushed "Filter Entities" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnFilter() 
{
	CIgesFiltDlg* filtdlg = new CIgesFiltDlg(this);
	filtdlg->DoModal();
	delete filtdlg;
}

/***********************************************************************
c
c   FUNCTION: OnNameModals()
c
c		This function called when user pushed "Name Modals" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnNameModals() 
{
/*
.....initialed on uio_init()

	status = iges_config(0);
	if (status != UU_SUCCESS) return;
*/
	CIgesModalDlg* modaldlg = new CIgesModalDlg(this);
	modaldlg->DoModal();
	delete modaldlg;	
}
/***********************************************************************
c
c   FUNCTION: OnAttr()
c
c		This function is called when user pushes the "Attributes" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnAttr() 
{
	CIgesAttrDlg* attrdlg = new CIgesAttrDlg(this);
	attrdlg->DoModal();
	delete attrdlg;	
}

/***********************************************************************
c
c   FUNCTION: OnNoDups()
c
c		This function is called when user checks the "No Duplicates" Check box
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnNoDups() 
{
	int nodups,state = FALSE;
	nodups = ((CButton*)GetDlgItem(IDC_CHECK_NODUPS))->GetCheck();
	CComboBox* cmbbox = (CComboBox*)GetDlgItem(IDC_LABEL_OPTION);
	
	int choice = cmbbox->GetCurSel() + 1;
/*
.....Keep the match tolerance field activated if the "No Duplicates"
.....check box is checked or the Enitity Label Ooption is set to From Existing
.....Unibase.
*/
	if (choice==4 || nodups) state = TRUE;
	((CWnd*)GetDlgItem(IDC_MATCHTOL))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_MATCHTXT))->EnableWindow(state);
	
	if (nodups)
	{	
		int check = ((CButton*)GetDlgItem(IDC_DECOMPOSE))->GetCheck();
		if (check)
			GetDlgItem(IDC_DECOMPOSE_CHC)->EnableWindow(TRUE);
		else
		{
			GetDlgItem(IDC_DECOMPOSE_CHC)->EnableWindow(FALSE);
/*
.....set to No deplicate always when disabled
*/
			UIG_decompose_chc = 0;
			((CComboBox*)GetDlgItem(IDC_DECOMPOSE_CHC))->SetCurSel(UIG_decompose_chc);
		}
	}
	else
	{
		GetDlgItem(IDC_DECOMPOSE_CHC)->EnableWindow(FALSE);
/*
.....set to No deplicate always when disabled
*/
		UIG_decompose_chc = 0;
		((CComboBox*)GetDlgItem(IDC_DECOMPOSE_CHC))->SetCurSel(UIG_decompose_chc);
	}
}
/***********************************************************************
c
c   FUNCTION: OnEdgeColor() 
c
c		This function called when user pushed "Edge Color" checkbox
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnEdgeColor() 
{
	int check = ((CButton*)GetDlgItem(IDC_SRF_EDGE))->GetCheck();
	if (check)
	{	
		GetDlgItem(IDC_ECLABEL)->EnableWindow(TRUE);
		GetDlgItem(IDC_EDGE_COLR_NEW)->EnableWindow(TRUE);
	}
	else
	{
		GetDlgItem(IDC_ECLABEL)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDGE_COLR_NEW)->EnableWindow(FALSE);
	}
}
void CIgesOptDlg::OnEdgeColorSel()
{
	GdiplusStartupInput gdiplusStartupInput;
	ULONG_PTR gdiplusToken;
	int color;

	GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);

	CNCLColorDlg dlg;
	color = m_edge_color;
	if (color == UIG_MAXCOLOR) color = -1;
	dlg.SetColorIndex(color);
	dlg.SetColorDefault(1, "Default");
	dlg.disable_addcolor();
	int nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
/*
.....set the field color to select color
*/
		SetButColor(dlg.m_current_color);
	}
	else if (nResponse == IDCANCEL)
	{
	}
	GdiplusShutdown(gdiplusToken);
}

/**********************************************************************
**    I_FUNCTION : SetButColor(int color)
**       set color button color
**    PARAMETERS  
**       INPUT  : color
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesOptDlg::SetButColor(int color)
{
	COLORREF bcolor;
	if (color<0)
	{
		bcolor = ::GetSysColor(COLOR_BTNFACE); 
	}
	else
	{
		bcolor = RGB(uw_color_table[color][0], 
					uw_color_table[color][1], 
					uw_color_table[color][2]);
	}
	m_button.set_color(bcolor, bcolor);
	m_button.Invalidate();
	m_button.UpdateWindow();
	m_edge_color = color;
}

/***********************************************************************
c
c   FUNCTION: InitColor()
c
c		This function get the color information
c			from iges file and put into color global value
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::InitColor()
{
	struct global_rec gblk;
	int status,i,j,irec;
	UX_pathname filename,fname,dir,tmp;
	char *cc, msg[300];
	struct dir_rec dblk;
	CString ctmp;
	int indx, set_color;
	struct IG_igesclr_rec *pblk;

	strcpy(filename, m_fname);
	ul_break_fname(filename, dir, fname);
	if (strlen(fname)==0)
	{
		return;
	}
	if (dir[0]=='\0')
	{
		ul_get_full_dir(".", dir);
	}
	strcat(dir, "\\");
	strcat(dir, fname);
	strcpy(filename, dir);
	status = 0;
	if (strcmp(filename, iges_fname)!=0)
	{
		if (iges_fname[0]!='\0')
		{
			_close(iges_fd);
			iges_fd = 0;
			if (ux_delete0(iges_tmpfile))
			{
				ul_short_filename(iges_fname,tmp,50); 
				sprintf (msg, "Can't delete %s corresponding %s", iges_tmpfile, tmp);
				MessageBox(msg, "Error!", MB_OK);
			}
			iges_fname[0] = '\0';
		}
		strcpy(tmp, filename);
		dbyte = pbyte = 0;
		status = uig_unb_opnfil(filename);
		if ( status == 0)
			strcpy(iges_fname, tmp);
		else
			iges_fname[0] = '\0';
	}
		
	if (iges_fname[0]!='\0')
	{
/*
.....Display the tolerance and the units.
*/
		uig_get_global(&gblk);
		uig_terminal_rec();
		cc = (char*) uu_toolmalloc(MAX_PARA_REC);
		j = 0;
		dbyte = pbyte = 0;
		i = indx = 0;
		uig_init_color();	
		uig_load_color_mode("");
/*
......we will add the iges color first, then current custom color later
......so we save it here and after we added iges color, append the current 
......custom color after the iges colors
*/
		uig_save_custom_color();			
		UIG_ncolor_iges = (sect_ptr[2]) / 2;
		if (UIG_color_array==NULL)
			UIG_color_array = (struct UIG_color_struct *)
				uu_toolmalloc(UIG_ncolor_iges*sizeof(struct UIG_color_struct));
		for(irec=1; irec<sect_ptr[2]+1; irec++,irec++)
		{
			pblk = (struct IG_igesclr_rec *)cc;
			pblk->name[0] = '\0';
			status = uig_get_data(irec,2,&dblk,cc);
			if(status == 0)
			{
				status = uig_in_color((struct IG_igesclr_rec *)cc,&indx,&set_color);
				UIG_color_array[i].color = set_color;
				UIG_color_array[i].irec = irec;
				i++;
				if (i == UIG_ncolor_iges) break;
			}
		}
		if (i==0)
		{
			UIG_ncolor_iges = 0;
			uu_free(UIG_color_array);
			UIG_color_array = 0;
		}
//test yurong
		else
			UIG_ncolor_iges = i;
		uu_toolfree(cc);
		uig_append_custom_color(indx);			
	}
}

/***********************************************************************
c
c   FUNCTION: OnShade() 
c
c		This function called when user pushed "Shaded" checkbox
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnShade() 
{
	int check = ((CButton*)GetDlgItem(IDC_SHADE))->GetCheck();
	if (check)
	{	
		GetDlgItem(IDC_LUCENCY)->EnableWindow(TRUE);
		GetDlgItem(IDC_LUCENCY_LABEL)->EnableWindow(TRUE);
	}
	else
	{
		GetDlgItem(IDC_LUCENCY)->EnableWindow(FALSE);
		GetDlgItem(IDC_LUCENCY_LABEL)->EnableWindow(FALSE);
	}
}

/***********************************************************************
c
c   FUNCTION: OnShade() 
c
c		This function called when user pushed "Shaded" checkbox
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesOptDlg::OnDecompose() 
{
	int check = ((CButton*)GetDlgItem(IDC_DECOMPOSE))->GetCheck();
	if (check)
	{	
		int check2 = ((CButton*)GetDlgItem(IDC_CHECK_NODUPS))->GetCheck();
		if (check2)
			GetDlgItem(IDC_DECOMPOSE_CHC)->EnableWindow(TRUE);
		else
		{
			GetDlgItem(IDC_DECOMPOSE_CHC)->EnableWindow(FALSE);
/*
.....set to No deplicate always when disabled
*/
			UIG_decompose_chc = 0;
			((CComboBox*)GetDlgItem(IDC_DECOMPOSE_CHC))->SetCurSel(UIG_decompose_chc);
		}
	}
	else
	{
		GetDlgItem(IDC_DECOMPOSE_CHC)->EnableWindow(FALSE);
/*
.....set to No deplicate always when disabled
*/
		UIG_decompose_chc = 0;
		((CComboBox*)GetDlgItem(IDC_DECOMPOSE_CHC))->SetCurSel(UIG_decompose_chc);
	}
}

