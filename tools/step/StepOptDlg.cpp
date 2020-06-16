/************************************************************************
c
c   FILE NAME: StepOptDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class StepOptDlg 
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			StepOptDlg.cpp , 25.4
c    DATE AND TIME OF LAST  MODIFICATION
c			04/05/18 , 15:03:16
c
c**********************************************************************
*/
// GgesOptDlg.cpp : implementation file
//

#include "wsntstdafx.h"
#include <io.h>
#include "wsntres.h"
#include "step.h"
#include "StepOptDlg.h"
#include "tiges.h"
#include "xenv1.h"
#include "ulist.h"
#include "StepModalDlg.h"
#include "StepAttrDlg.h"
#include <gdiplus.h>
#include "wsntclrdlg.h"
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

extern "C" int MAX_PARA_REC;
extern "C" int UG_def_line_wt;

extern "C" int ul_break_fname(char*, char*, char*);
extern "C" int ul_get_full_dir(char* dir, char* fullname);
extern "C" int uig_terminal_rec();
extern "C" int uu_free(void *);
extern "C" int iges_config(int );
extern "C" int uig_update_surface_prim();
extern "C" int ur_saveu_active(int );
extern "C" int uig_open_secondary(char unifile[],int *index);
extern "C" char *ux_getenv(char*);
extern "C" void ul_short_filename(char *fin, char *fout, int maxc);

extern "C" void utp_reset_shape_range();
extern "C" int utp_shape_label_nstack();
extern "C" char *uu_malloc(int);
extern "C" void utp_set_shape_range(int,int);
extern "C" char *utp_get_shape_label(int);
extern "C" void utp_get_label_method(int *,int *,int *);
extern "C" void utp_set_label_method(int,int,int);
extern "C" int utp_read_step_file(char *);
extern "C" int utp_count_entities();
extern "C" int utp_get_units();

extern "C" int utp_load_color_mode(char *);
extern "C" void uig_init_color();
extern "C" void utp_set_layer_flag(int);
extern "C" int utp_get_layer_flag();
extern "C" int utp_set_start_layer(int);
extern "C" int utp_get_start_layer();

/////////////////////////////////////////////////////////////////////////////
// CStepOptDlg dialog


CStepOptDlg::CStepOptDlg(CWnd* pParent, char *fname)
	: CDialog(CStepOptDlg::IDD, pParent)
{
	if (fname==NULL)
		m_fname[0] = '\0';
	else if (fname[0]=='\0')
		m_fname[0] = '\0';
	else
		strcpy(m_fname, fname);
	m_edge_color = UIG_edge_color;
}


void CStepOptDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStepOptDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CStepOptDlg, CDialog)
	//{{AFX_MSG_MAP(CStepOptDlg)
	ON_BN_CLICKED(IDC_SELECT_ALL, OnSelectAll)
	ON_BN_CLICKED(IDC_CLEAR_ALL, OnClearAll)
	ON_CBN_SELCHANGE(IDC_COPTION, OnSelchangeCoption)
	ON_BN_CLICKED(IDC_CHECK_LAYERS, OnLayers)
	ON_CBN_SELCHANGE(IDC_LABEL_OPTION, OnSelchangeLabelOption)
	ON_BN_CLICKED(IDC_BROWSE, OnBrowse)
	ON_BN_CLICKED(IDC_MODALSN, OnNameModals)
	ON_BN_CLICKED(IDC_ATTRIBUTES, OnAttr)
	ON_BN_CLICKED(IDC_CHECK_NODUPS, OnNoDups)
	ON_BN_CLICKED(IDC_SRF_EDGE, OnEdgeColor)
	ON_BN_CLICKED(IDC_EDGE_COLR_NEW, OnEdgeColorSel)
	ON_BN_CLICKED(IDC_SHADE, OnShade)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStepOptDlg message handlers

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
void CStepOptDlg::OnSelectAll() 
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
void CStepOptDlg::OnOK() 
{
	int status,index,len,nent;
	int label_type,use_sub,concat_lab;
	CString tmpstr;
	UX_pathname unifile;
	char msg[300],toler[10],in_mm[10],*text;
	int sel_num,srf_edge,edge_color;
	int *ans;
	int count =0,j,i = 0;
	int layer_flag,start_layer,temp_layer;
/*
.....Define the shapes to import
*/
	CListBox* lstbox = (CListBox*)GetDlgItem(IDC_LAYER_LIST);
	if (lstbox->GetCurSel()==0)
		utp_reset_shape_range();
	else
	{
		nent = utp_shape_label_nstack();
		ans = (int *)uu_malloc(sizeof(int)*nent);
		sel_num = lstbox->GetSelItems(nent, ans) ;
		if (sel_num==0) utp_reset_shape_range();
		else
		{
			for (i=0; i<sel_num; i++)
				utp_set_shape_range(ans[i],sel_num);
		}
		uu_free(ans);
	}
/*
.....Set unique layer flag for shapes
.....Parse the string to get the starting layer number
*/
	layer_flag = ((CButton*)GetDlgItem(IDC_CHECK_LAYERS))->GetState();
	if (layer_flag)
	{
		((CWnd*)GetDlgItem(IDC_LAYNUM))->GetWindowText(tmpstr);
		len = tmpstr.GetLength();
		text = tmpstr.GetBuffer(len);
		start_layer = utp_get_start_layer();
		for (i = 0; i < len; i++)
		{
			if (text[i] == '.' || isalpha(text[i]))
			{
				sprintf(msg,"Integer value required for layer.");
				MessageBox(msg, "Error!", MB_OK);
				goto use_mod_layer;
			}
		}
		sscanf(text,"%d", &temp_layer);
		if (temp_layer >= 0 && temp_layer <= 9999)
			start_layer = temp_layer;
		else
		{
			sprintf(msg,"Layer number out of range. (0-9999 expected)");
			MessageBox(msg, "Error!", MB_OK);
		}
use_mod_layer:;
		utp_set_start_layer(start_layer);
	}
	utp_set_layer_flag(layer_flag);
/*
.....Label modals
*/
	label_type = ((CComboBox*)GetDlgItem(IDC_LABEL_OPTION))->GetCurSel();
	use_sub    = ((CButton*)GetDlgItem(IDC_CHECK_USE_SUBSCRIPT))->GetState();
	concat_lab = ((CButton*)GetDlgItem(IDC_CHECK_CONCAT_SUBSCRIPT))->GetState();
/*
.....Match existing Unibase labels
.....Load secondary unibase
*/
	if (label_type==4)
	{
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
/*
.....Set the labeling method
*/
	utp_set_label_method(label_type,use_sub,concat_lab);
/*
.....Parse the string to get the tolerance and the units(mm/in)
*/
	((CWnd*)GetDlgItem(IDC_MATCHTOL))->GetWindowText(tmpstr);
	len = tmpstr.GetLength();
	text = tmpstr.GetBuffer(len);
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
BOOL CStepOptDlg::OnInitDialog() 
{
	int label_type,use_sub,concat_lab,layer_flag,start_layer;
	CString ctmp;
	UX_pathname tmp;

	CDialog::OnInitDialog();
/*
.....Get labeling method
*/
	utp_get_label_method(&label_type,&use_sub,&concat_lab);
/*
.....Get unique layer flag.
*/
	layer_flag = utp_get_layer_flag();
	start_layer = utp_get_start_layer();
/*
.....Initialize these parameters according to the modfile parameters.
*/	
	((CButton*)GetDlgItem(IDC_CHECK_LAYERS))->SetCheck(layer_flag);
	((CComboBox*)GetDlgItem(IDC_COPTION))->SetCurSel(UIG_conv_opt);
	((CButton*)GetDlgItem(IDC_CHECK_USE_SUBSCRIPT))->SetCheck(use_sub);
	((CButton*)GetDlgItem(IDC_CHECK_CONCAT_SUBSCRIPT))->SetCheck(concat_lab);
	((CComboBox*)GetDlgItem(IDC_LABEL_OPTION))->SetCurSel(label_type);
	if ((label_type==0)||(label_type==4))
		GetDlgItem(IDC_MODALSN)->EnableWindow(TRUE);
	else
		GetDlgItem(IDC_MODALSN)->EnableWindow(FALSE);
	((CComboBox*)GetDlgItem(IDC_COMBO_MATCHLEVEL))->SetCurSel(UIG_matchlevel);
	((CComboBox*)GetDlgItem(IDC_COMBO_STARTUNMATCH))->SetCurSel(UIG_start_unmatch);
	((CButton*)GetDlgItem(IDC_SHADE))->SetCheck(shade_set);

	char snum[40];
	sprintf(snum,"%d", UIG_lucency);
	ctmp = snum;
	((CWnd*)GetDlgItem(IDC_LUCENCY))->SetWindowText(ctmp);
	
	((CButton*)GetDlgItem(IDC_CHECK_REINIT_LAB))->SetCheck(UIG_reinit_lab);
	((CButton*)GetDlgItem(IDC_CHK_UNMATCH_SEC))->SetCheck(UIG_unmatch_sec);
	((CButton*)GetDlgItem(IDC_CHK_REG_MATCH))->SetCheck(UIG_regressive);
	((CButton*)GetDlgItem(IDC_CHECK_NODUPS))->SetCheck(UIG_nodups);
	((CButton*)GetDlgItem(IDC_COLOR))->SetCheck(UIG_color_iges);
		
	((CButton*)GetDlgItem(IDC_SRF_EDGE))->SetCheck(UIG_srf_edge);
	sprintf(tmp,"%d",start_layer);
	ctmp = tmp;
	GetDlgItem(IDC_LAYNUM)->SetWindowText(ctmp);

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
	OnLayers();
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

/***********************************************************************
c
c   FUNCTION: InitRangeList()
c
c		This function gets all shape labels and places them in the list.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepOptDlg::InitRangeList()
{
	struct global_rec gblk;
	int status,i,j,k,fstat,inc,units,lfl;
	char *sptr;
	UX_pathname filename,fname,dir,tmp;
	char *cc, msg[300],unit[3];
	struct dir_rec dblk;
	char layer_str[IG_LAYER][5];
	char *p;
	CString ctmp;
/*
.....Check if user entered filename
*/
	strcpy(filename,m_fname);
	ul_break_fname(filename, dir, fname);
	if (strlen(fname)==0)
	{
		fstat = UU_FAILURE;
		iges_fname[0] = '\0';
		MessageBox(
			"No STEP file specified! No entities will appear in Components list.",
			"WARNING!", MB_OK);
	}
/*
.....Load step file
*/
	else
	{
		if (dir[0] == '\0') ul_get_full_dir(".",dir);
		strcat(dir,"\\");
		strcat(dir,fname);
		strcpy(filename,dir);
		fstat = utp_read_step_file(filename);
		if (fstat == UU_SUCCESS)
		{
			utp_count_entities();
			strcpy(iges_fname,filename);
		}
		else
			iges_fname[0] = '\0';
	}
/*
.....Display the tolerance and the units.
*/
	if (fstat == UU_SUCCESS)
	{
		unit[0] = '\0';
		units = utp_get_units();
		if (units == 2) 
		{
			if(UIG_units_factor==1)
				UIG_match_tol_disp =UIG_match_tol * 25.4;
			UIG_units_factor = 25.4;
			strcpy(unit, "mm");
		}
		else if (units == 1) 
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
/*
.....Store shape names
*/
		CListBox* lstbox = (CListBox*)GetDlgItem(IDC_LAYER_LIST);
		inc = 0;
		do
		{
			sptr = utp_get_shape_label(inc++);
			if (sptr == UU_NULL) break;
			lstbox->AddString(sptr);
		} while (sptr != UU_NULL);
		inc--;
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
		inc = 0;
	}
	lfl = (inc > 0);
	((CWnd*)GetDlgItem(IDC_COPTION))->EnableWindow(lfl);
	((CWnd*)GetDlgItem(IDC_SELECT_ALL))->EnableWindow(lfl);
	((CWnd*)GetDlgItem(IDC_CLEAR_ALL))->EnableWindow(lfl);
	((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(lfl);
	((CWnd*)GetDlgItem(IDC_LAYER_LIST))->EnableWindow(lfl);
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
void CStepOptDlg::OnClearAll() 
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
void CStepOptDlg::OnSelchangeCoption() 
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
c   RETURN:    None
c
**********************************************************************/
void CStepOptDlg::OnSelchangeLabelOption() 
{
	CComboBox* cmbbox = (CComboBox*)GetDlgItem(IDC_LABEL_OPTION);
	UIG_lab_opt =cmbbox->GetCurSel();
	if ((UIG_lab_opt==0)||(UIG_lab_opt==4))
		GetDlgItem(IDC_MODALSN)->EnableWindow(TRUE);
	else
		GetDlgItem(IDC_MODALSN)->EnableWindow(FALSE);

	int nodups,state = FALSE;
	int choice = cmbbox->GetCurSel() + 1;
	if (choice==5) state = TRUE;
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
	if (choice==2 || choice==3 /*|| choice==4*/) state = TRUE;
	((CWnd*)GetDlgItem(IDC_CHECK_USE_SUBSCRIPT))->EnableWindow(state);
	if (choice != 2) state = FALSE;
	((CWnd*)GetDlgItem(IDC_CHECK_CONCAT_SUBSCRIPT))->EnableWindow(state);
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
void CStepOptDlg::OnBrowse() 
{
	DWORD dwFlags;
	dwFlags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;
	LPCTSTR filter = "Unibase File (*.u)|*.u|All Files (*.*)|*.*||";		
	CFileDialog *filedlg = new CFileDialog(TRUE, "step", NULL, dwFlags,
			filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	GetDlgItem(IDC_UNIBASEL)->SetWindowText(FileName);
	delete filedlg;	
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
void CStepOptDlg::OnNameModals() 
{
/*
.....initialed on uio_init()

	status = iges_config(0);
	if (status != UU_SUCCESS) return;
*/
	CStepModalDlg* modaldlg = new CStepModalDlg(this);
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
void CStepOptDlg::OnAttr() 
{
	CStepAttrDlg* attrdlg = new CStepAttrDlg(this);
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
void CStepOptDlg::OnNoDups() 
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
	if (choice==5 || nodups) state = TRUE;
	((CWnd*)GetDlgItem(IDC_MATCHTOL))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_MATCHTXT))->EnableWindow(state);
	
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
void CStepOptDlg::OnEdgeColor() 
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
void CStepOptDlg::OnEdgeColorSel()
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
void CStepOptDlg::SetButColor(int color)
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
c   FUNCTION: OnLayers()
c
c		This function is called when user checks the "Place Components on
c     Different Layers" Check box
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepOptDlg::OnLayers() 
{
	int on_layers,state = FALSE;
	on_layers = ((CButton*)GetDlgItem(IDC_CHECK_LAYERS))->GetCheck();
	if (on_layers) state = TRUE;
	((CWnd*)GetDlgItem(IDC_LAYNUM))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_LAYTXT))->EnableWindow(state);
}

/***********************************************************************
c
c   FUNCTION: InitColor()
c
c		This function get the color information
c			from step file and put into color global value
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepOptDlg::InitColor()
{
/*
.....Initialize default colors
*/
	uig_init_color();	
	utp_load_color_mode("");
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
void CStepOptDlg::OnShade() 
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

