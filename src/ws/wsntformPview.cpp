/************************************************************************
**
**   FILE NAME: wsntformPview.cpp
**
**	 Description - Functions and implementations for
**		CNCLFormPView class
**
**	 CONTAINS: 
**		class functions of CNCLFormPView class
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntformPview.cpp , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:36:45
***********************************************************************
*/
#include "stdafx.h"
#include <gdiplus.h>
#define UU_OPENGL
#include "wsgl.h"
#include "wsntformdoc.h"
#include "wsntfdsnfrm.h"
#include "wsntfrmpview.h"
#include	"mfort.h"
#include	"wsntres.h"
#include "wsntclrdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
#define FORM_STRING		1
#define FORM_PICK		2
#define FORM_LOCATE		3
#define FORM_RECORD  4
#define FORM_LABEL   5
#define FORM_SUBSCR  6

extern "C" int ul_strip_blanks (char *str, int *size);
extern "C" int ul_to_reals(double *ary, int *inum, int maxnum, char *str);
extern "C" void ud_get_filename(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag);
extern "C" void voccad(char*, UM_int2*);
extern CWnd *NCL_Main_View;
extern "C" void ul_remove_quotes(char*);
extern "C" void ul_to_upper(char*);
extern "C" void ncl_getclr_inx(UM_f77_str_ptr, UM_int2*, UM_int4*, UM_int4*);
extern CFrameWnd *UW_Dform_frame;

/*
.....for choicebox,  if (m_type[fldno]==2)
.....the params could be a number, need change to a choices
.....flag = 0, convert number to text
.....flag = 1, convert text to number
*/
void S_convert_choice_params(CString pic_params, CString choices, int flag, CString &o_pic_params)
{
	int i, j, num, len, start;
	char choice[80];
	int chc = -1;
	if (flag==0)
	{
		o_pic_params = "";
		num = atoi(pic_params);
		if ((num<0)||(num>1000))
		{
			num = 0;
		}
		len = strlen(choices);
		start = 0;
		j = 0;
		for (i=0; i<len; i++)
		{
			if (choices[i]!='\"')
			{
				if (start==1)
					choice[j++] = choices[i];
				else
					continue;
			}
			else if (start==0)
				start = 1;
			else
			{
				choice[j] = '\0';
				chc++;
				if (chc==0)
					o_pic_params = choice;
				if (chc==num)
				{
					o_pic_params = choice;
					return;
				}
				start = 0;
				j = 0;
			}
		}
		return;
	}
	if (flag==1)
	{
		char tempstr[80];
		o_pic_params = "0";
		len = strlen(choices);
		start = 0;
		j = 0;
		for (i=0; i<len; i++)
		{
			if (choices[i]!='\"')
			{
				if (start==1)
					choice[j++] = choices[i];
				else
					continue;
			}
			else if (start==0)
				start = 1;
			else
			{
				choice[j] = '\0';
				chc++;
				if (stricmp(pic_params, choice)==0)
				{
					sprintf(tempstr, "%d", chc); 
					o_pic_params = tempstr;
					return;
				}
				start = 0;
				j = 0;
			}
		}
/*
.....no match, if it is a number, accept as choice
*/
		num = atoi(pic_params);
		if ((num>0)&&(num<200))
		{
			sprintf(tempstr, "%d", num); 
			o_pic_params = tempstr;
		}
		return;
	}
}

void S_format_range(char *tempstr, UD_DASIN range[2], int type)
{
	switch(type) 
	{
		case UD_SCAINT:
		case UD_DASINT:
		{
			sprintf(tempstr, "%d, %d", range[0].dint, range[1].dint);
			break;
		}
		case UD_SCACART:
		case UD_SCAVEC:
		case UD_SCANDC:
		case UD_DASCART:
		case UD_DASVEC:
		case UD_DASNDC:
		{
			sprintf(tempstr, "%g, %g, %g, %g, %g, %g", 
				range[0].cord.cord[0], range[0].cord.cord[1], range[0].cord.cord[2],
				range[1].cord.cord[0], range[1].cord.cord[1], range[1].cord.cord[2]);
			break;
		}
		case UD_SCAVAL:
		case UD_SCADISTANCE:
		case UD_SCAANGLE:
		case UD_SCAUNITLESS:
		case UD_DASSCALAR:
		case UD_DASVAL:
		case UD_DASDISTANCE:
		case UD_DASANGLE:
		case UD_DASUNITLESS:
		default:
		{
			sprintf(tempstr, "%g, %g", range[0].dreal, range[1].dreal);
			break;
		}
	}
}

static int S_get_range(char *tempstr, UD_DASIN range[2], int type)
{
	int inum;
	UU_REAL rval[2];
	UU_REAL rval2[6];
	UU_REAL pval[4];

	switch(type) 
	{
		case UD_SCAINT:
		case UD_DASINT:
		{
			if ((ul_to_reals(rval,&inum,2,tempstr) != UU_SUCCESS) ||
				inum != 2) return -1;
			range[0].dint = (int)rval[0]; 
			range[1].dint = (int)rval[1];
			break;
		}
		case UD_SCACART:
		case UD_SCAVEC:
		case UD_SCANDC:
		case UD_DASCART:
		case UD_DASVEC:
		case UD_DASNDC:
		{
			if ((ul_to_reals(rval2,&inum,6,tempstr) != UU_SUCCESS) ||
				inum != 6) return -1;
			range[0].cord.cord[0] = rval2[0];
			range[0].cord.cord[1] = rval2[1];
			range[0].cord.cord[2] = rval2[2];
			range[1].cord.cord[0] = rval2[3];
			range[1].cord.cord[1] = rval2[4];
			range[1].cord.cord[2] = rval2[5];
			break;
		}
		case UD_SCAVAL:
		case UD_SCADISTANCE:
		case UD_SCAANGLE:
		case UD_SCAUNITLESS:
		case UD_DASSCALAR:
		case UD_DASVAL:
		case UD_DASDISTANCE:
		case UD_DASANGLE:
		case UD_DASUNITLESS:
		default:
		{
			if ((ul_to_reals(rval,&inum,2,tempstr) != UU_SUCCESS) ||
				inum != 2) return -1;
			range[0].dreal = rval[0]; 
			range[1].dreal = rval[1];
			break;
		}
	}
	return 0;
}

/***********************************************************************
c
c   FUNCTION: Scheck_if_voc(char *choice)
c
c       This function check if a string is a vocab word
c
c   INPUT:  choice: string to be checked
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
static int Scheck_if_voc(char *choice)
{
	int i = 1;
	UM_int2 inum;
	int nc;
	char lstr[64];
	UM_f77_str label;

	nc = strlen(choice);
	strcpy(lstr, choice);
	ul_strip_blanks(lstr, &nc);
	ul_to_upper(lstr);
	if (nc > 0)
	{
		for (i=nc;i<8;i++) lstr[i] = ' ';
		UM_init_f77_str(label, lstr, nc);
		voccad(UM_addr_of_f77_str(label), &inum);
		if (inum != 0)
		{
			return 1;
		}
	}
	return 0;
}

/***********************************************************************
c
c   FUNCTION: Scheck_choice_vocab(char *choices) 
c
c       This function check if a choices formated string is valid
c			it has to be all vocab word
c
c   INPUT:  choices: formated choices string to be checked
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int Scheck_choice_vocab(char *choices)
{
	char choice[80];
	int len = strlen(choices);
	int start = 1;
	int choice_num = 0;
	int vocwrd, i, j = 0;

	for (i=0; i<len; i++)
	{
		if (choices[i]!=',')
		{
			choice[j++] = choices[i];
		}
		else
		{
			choice_num++;
			choice[j] = '\0';
/*
.....check if this choice is vocabulary word
*/
/*
.....remove quotes if have any
*/
			ul_remove_quotes(choice);
			vocwrd = Scheck_if_voc(choice);
			if (vocwrd==0)
				return -1;
			j = 0;
		}
	}
	if (j>0)
	{
		choice_num++;
		choice[j] = '\0';
		ul_remove_quotes(choice);
		vocwrd = Scheck_if_voc(choice);
		if (vocwrd==0)
			return -1;
	}
	if (choice_num==0)
		return -1;
	return 0;
}
IMPLEMENT_DYNCREATE(CNCLFormPView, CFormView)

BEGIN_MESSAGE_MAP(CNCLFormPView, CFormView)
	//{{AFX_MSG_MAP(CNCLFormPView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	ON_WM_SIZE()
	ON_COMMAND(IDC_BUTTON1, OnFileBrowse)
	ON_COMMAND(IDC_FFG_CBUT, OnColorBut1)
	ON_COMMAND(IDC_FBG_CBUT, OnColorBut2)
	ON_COMMAND(IDC_PFG_CBUT, OnColorBut3)
	ON_COMMAND(IDC_PBG_CBUT, OnColorBut4)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT1, OnSaveLabel)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT2, OnSavePosition)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT3, OnSaveSize)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT4, OnSaveLimit)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT5, OnSaveChoices)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT6, OnSaveLength)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT7, OnSavePrec)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT8, OnSaveRange)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT11, OnSaveFontSize)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT12, OnSaveInputNo)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT13, OnSavePageNo)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT14, OnSavePicName)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT15, OnSaveTooltip)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT16, OnSavePicPos)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT17, OnSavePicParms)
	ON_CONTROL(EN_KILLFOCUS, IDC_EDIT18, OnLoadActivePic)
	ON_CONTROL(CBN_SELCHANGE, IDC_COMBO1, OnSaveInputType)
	ON_CONTROL(CBN_SELCHANGE, IDC_COMBO2, OnSaveStrType)
	ON_CONTROL(CBN_SELCHANGE, IDC_COMBO3, OnSaveActive)
	//}}AFX_MSG_MAP
	// Standard printing commands
END_MESSAGE_MAP()

/***********************************************************************
**   FUNCTION: CNCLFormPView
**		Constructor of class CNCLFormPView
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormPView::CNCLFormPView() : CFormView(CNCLFormPView::IDD)
{
	m_init = 0;
	m_parent = NULL;
	m_view_reset = 0;
	m_prop_dlg = new CNCLFormProp(-1, -1, this);
	m_dtype = -2;
	m_itype = -1;
	m_label = "";
	m_pos[0] = 0;
	m_pos[1] = 0;
	m_size[0] = 50;
	m_size[1] = 17;

	m_type = UD_DASSTRING;
	m_input = 0;	
	m_justified = 0;
	m_limit = "";
	m_choices = "";
	m_len = 8;
	m_prec = 1;
	m_range_flag = 0;
	m_pcolor = "Default, Default"; 
	m_color = "Default, Default"; 
	m_font = 1.0;
	m_active = 0;
	m_ffg = m_fbg = m_pfg = m_pbg = -1;
	m_inputno = 0;
	m_page = 0;
	m_pic_label = "";
	m_pic_tooltip = "";
	m_pic_rect[0] = -1000;
	m_pic_rect[2] = -1000;
	m_pic_rect[1] = -1000;
	m_pic_rect[3] = -1000;
	m_pic_params = "";
	m_pic_act_area = 0;
	m_reloading = 0;
	m_picarea_no = 0;
	m_picarea = NULL;
	m_prop_dlg = new CNCLFormProp(-1, -1, this);
}

/***********************************************************************
**
**   FUNCTION: ~CNCLFormPView
**              Destructor of class CNCLFormPView, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormPView::~CNCLFormPView()
{
	if (m_prop_dlg!=NULL)
		delete m_prop_dlg;
}

/***********************************************************************
c
c   FUNCTION: PreTranslateMessage(MSG* pMsg) 
c
c       translate window messages before they are dispatch
c
c   INPUT:  pMsg   Points to a MSG structure that contains the 
c					message to process.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNCLFormPView::PreTranslateMessage(MSG* pMsg)
{
	ASSERT(pMsg != NULL);
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);
	return CFormView::PreTranslateMessage(pMsg);
//// it is for dialog form design, now use frame, comment out now
/*
	if (CView::PreTranslateMessage(pMsg))
		return TRUE;
*/
/*
.....the mouse button function not work as expect
*/
/***
	CWnd* pParentWnd = GetParent();
	while (pParentWnd != NULL)
	{
		if ((pParentWnd->IsWindowEnabled())&&(pParentWnd->IsFrameWnd()))
		{
			if ((CFrameWnd*)pParentWnd->PreTranslateMessage(pMsg))
				return TRUE;
		}
		pParentWnd = pParentWnd->GetParent();
	}

	if (::GetWindow(m_hWnd, GW_CHILD) == NULL)
		return FALSE;
	CWnd *wnd;
	if ((pMsg->message==WM_LBUTTONDOWN)||(pMsg->message==WM_MBUTTONDOWN))
	{
		wnd = GetDlgItem(IDC_EDIT1);
		if (pMsg->hwnd==wnd->m_hWnd)
			wnd->SetFocus();
		wnd = GetDlgItem(IDC_EDIT3);
		if (pMsg->hwnd==wnd->m_hWnd)
			wnd->SetFocus();
		wnd = GetDlgItem(IDC_EDIT4);
		if (pMsg->hwnd==wnd->m_hWnd)
			wnd->SetFocus();
		wnd = GetDlgItem(IDC_EDIT5);
		if (pMsg->hwnd==wnd->m_hWnd)
			wnd->SetFocus();
		wnd = GetDlgItem(IDC_EDIT6);
		if (pMsg->hwnd==wnd->m_hWnd)
			wnd->SetFocus();
		wnd = GetDlgItem(IDC_EDIT7);
		if (pMsg->hwnd==wnd->m_hWnd)
			wnd->SetFocus();
		wnd = GetDlgItem(IDC_EDIT8);
		if (pMsg->hwnd==wnd->m_hWnd)
			wnd->SetFocus();
		wnd = GetDlgItem(IDC_EDIT11);
		if (pMsg->hwnd==wnd->m_hWnd)
			wnd->SetFocus();
		wnd = GetDlgItem(IDC_EDIT12);
		if (pMsg->hwnd==wnd->m_hWnd)
			wnd->SetFocus();
	}
	return PreTranslateInput(pMsg);
*/
}
void CNCLFormPView::UpdatePropertyPicture(CString pic_label, CString pic_tooltip,
				float pic_rect[4], CString pic_params, int indx)
{
	char tempstr[81];
	GetDlgItem(IDC_EDIT14)->SetWindowText(pic_label);
	GetDlgItem(IDC_EDIT15)->SetWindowText(pic_tooltip);
	sprintf(tempstr, "%f,%f,%f,%f", pic_rect[0], pic_rect[1], 
					pic_rect[2], pic_rect[3]);
	if (pic_label=="")
		GetDlgItem(IDC_EDIT16)->SetWindowText("");
	else
		GetDlgItem(IDC_EDIT16)->SetWindowText(tempstr);
/*
.....for choicebox,  if (m_type[fldno]==2)
.....the params could be a number, need change to a choices text
*/
	if (m_itype==2)
	{
		CString pic_params2;
		S_convert_choice_params(pic_params, m_choices, 0, pic_params2);
		m_pic_params = pic_params2;
		GetDlgItem(IDC_EDIT17)->SetWindowText(pic_params2);
	}
	else
	{
		GetDlgItem(IDC_EDIT17)->SetWindowText(pic_params);
		m_pic_params = pic_params;
	}
	m_pic_label = pic_label;
	char *name = pic_label.GetBuffer();
	if (m_picarea==NULL)
		Add_picarea(name);
	m_pic_rect[0] = pic_rect[0];
	m_pic_rect[1] = pic_rect[1];
	m_pic_rect[2] = pic_rect[2];
	m_pic_rect[3] = pic_rect[3];
	m_pic_tooltip = pic_tooltip;
	if (indx>=m_picarea_no)
	{
		MessageBox("Index is wrong!", "Error", MB_OK);
		return;
	}
	name = pic_tooltip.GetBuffer();
	int len;
	len = strlen(name);
	if ((m_picarea[indx].tooltext==NULL)&&(len>0))
	{
		m_picarea[indx].tooltext = (char*) uu_malloc((100)*sizeof(char));
	}
	if (len>0)
		strcpy(m_picarea[indx].tooltext, name);
	else if (m_picarea[indx].tooltext!=NULL)
		m_picarea[indx].tooltext[0] = '\0';
	name = pic_params.GetBuffer();
	len = strlen(name);
	if ((m_picarea[indx].params==NULL)&&(len>0))
	{
		m_picarea[indx].params = (char*) uu_malloc((100)*sizeof(char));
	}
	if (len>0)
		strcpy(m_picarea[indx].params, name);
	else if (m_picarea[indx].params!=NULL)
		m_picarea[indx].params[0] = '\0';
	m_picarea[indx].xmin = pic_rect[0];
	m_picarea[indx].xmax = pic_rect[2];
	m_picarea[indx].ymin = pic_rect[1];
	m_picarea[indx].ymax = pic_rect[3];
	m_pic_act_area = indx;
}


void CNCLFormPView::UpdatePropertySize(int cx, int cy)
{
	char tempstr[40];
	sprintf(tempstr, "%d, %d", cx, cy);
	GetDlgItem(IDC_EDIT3)->SetWindowText(tempstr);
	m_size[0] = cx;
	m_size[1] = cy;
}

void CNCLFormPView::UpdatePropertyPos(int x, int y)
{
	char tempstr[40];
	sprintf(tempstr, "%d, %d", x, y);
	GetDlgItem(IDC_EDIT2)->SetWindowText(tempstr);
	m_pos[0] = x;
	m_pos[1] = y;
}

void CNCLFormPView::Convert_rect(float in_rect[4], float out_rect[4], int flag)
{
	((CNCLFdsnFrame*)m_parent)->Convert_pic_rect(in_rect, out_rect, flag);
}
// sizerec is the pixel, m_pic_rect is percentage
void CNCLFormPView::UpdatePropertyHSPTSize(CRect sizerec, float prect[4])
{
	char tempstr[80];
	int changed = 0;
	float size_rect[4], pic_rect[4];

	size_rect[0] = sizerec.left;
	size_rect[1] = sizerec.top;
	size_rect[2] = sizerec.right;
	size_rect[3] = sizerec.bottom;
	Convert_rect(size_rect, pic_rect, 0);

	if ((m_pic_rect[0]!=pic_rect[0])||(m_pic_rect[2]!=pic_rect[2])
		||(m_pic_rect[1]!=pic_rect[1])||(m_pic_rect[3]!=pic_rect[3]))
		changed = 1;

	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(changed==1))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(17);
		m_pic_rect[0] = pic_rect[0];
		m_pic_rect[1] = pic_rect[1];
		m_pic_rect[2] = pic_rect[2];
		m_pic_rect[3] = pic_rect[3];
		sprintf(tempstr, "%f, %f, %f, %f", m_pic_rect[0], m_pic_rect[1], 
					m_pic_rect[2], m_pic_rect[3]);
		GetDlgItem(IDC_EDIT16)->SetWindowText(tempstr);
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
	prect[0] = m_pic_rect[0];
	prect[1] = m_pic_rect[1];
	prect[2] = m_pic_rect[2];
	prect[3] = m_pic_rect[3];
}

/***********************************************************************
c
c   FUNCTION: GetPropertyPage()
c
c       Get the current property page data from this view window
c		and return it
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    current property page data
c
**********************************************************************/
CNCLFormProp* CNCLFormPView::GetPropertyPage()
{
/*
......save all first
*/
	if (m_reloading==0)
		SaveAll();
	m_prop_dlg->m_dtype = m_dtype;
	m_prop_dlg->m_itype = m_itype;
	m_prop_dlg->m_label = m_label;
	m_prop_dlg->m_pos[0] = m_pos[0];
	m_prop_dlg->m_size[0] = m_size[0];
	m_prop_dlg->m_pos[1] = m_pos[1];
	m_prop_dlg->m_size[1] = m_size[1];
	m_prop_dlg->m_range[0] = m_range[0];
	m_prop_dlg->m_range[1] = m_range[1];
	m_prop_dlg->m_range_flag = m_range_flag;
	m_prop_dlg->m_font = m_font;
	m_prop_dlg->m_type = m_type;
	m_prop_dlg->m_input = m_input;
	m_prop_dlg->m_justified = m_justified;
	m_prop_dlg->m_len = m_len; 
	m_prop_dlg->m_prec = m_prec;
	m_prop_dlg->m_active = m_active;
	m_prop_dlg->m_input_itemno = m_input_itemno;
	m_prop_dlg->m_limit = m_limit;
	m_prop_dlg->m_choices = m_choices;
	m_prop_dlg->m_color = m_color;
	m_prop_dlg->m_pcolor = m_pcolor;
	m_prop_dlg->m_page = m_page;
	m_prop_dlg->m_pic_act_area = m_pic_act_area;
	int indx;
	indx = m_pic_act_area;

	if ((m_prop_dlg->m_picarea!=NULL))
	{
/*
		if (m_prop_dlg->m_picarea_no>indx)
		{
			if (m_prop_dlg->m_picarea[indx].params!=NULL)
				uu_free(m_prop_dlg->m_picarea[indx].params);
			if (m_prop_dlg->m_picarea[indx].tooltext!=NULL)
				uu_free(m_prop_dlg->m_picarea[indx].tooltext);
			m_prop_dlg->m_picarea[indx].tooltext = NULL;
			m_prop_dlg->m_picarea[indx].params = NULL;
		}
*/
		for (int k=0; k<m_prop_dlg->m_picarea_no; k++)
		{
			if (m_prop_dlg->m_picarea[k].params!=NULL)
				uu_free(m_prop_dlg->m_picarea[k].params);
			if (m_prop_dlg->m_picarea[k].tooltext!=NULL)
				uu_free(m_prop_dlg->m_picarea[k].tooltext);
			m_prop_dlg->m_picarea[k].tooltext = NULL;
			m_prop_dlg->m_picarea[k].params = NULL;
		}
		uu_free((char*)(m_prop_dlg->m_picarea));
	}
	m_prop_dlg->m_pic_act_area = m_pic_act_area;
	if (m_picarea_no>0)
	{
		m_prop_dlg->m_picarea = (UD_PICAREA *) uu_malloc(m_picarea_no*sizeof(UD_PICAREA));
		for (int k=0; k<m_picarea_no; k++)
		{
			if (m_picarea[k].params!=NULL)
			{
				m_prop_dlg->m_picarea[k].params = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_prop_dlg->m_picarea[k].params, m_picarea[k].params);
			}
			else
				m_prop_dlg->m_picarea[k].params = NULL;

			if (m_picarea[k].tooltext!=NULL)
			{
				m_prop_dlg->m_picarea[k].tooltext = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_prop_dlg->m_picarea[k].tooltext, m_picarea[k].tooltext);
			}
			else
				m_prop_dlg->m_picarea[k].tooltext = NULL;

			strcpy(m_prop_dlg->m_picarea[k].name, m_picarea[k].name);
			m_prop_dlg->m_picarea[k].xmin = m_picarea[k].xmin;
			m_prop_dlg->m_picarea[k].ymin = m_picarea[k].ymin;
			m_prop_dlg->m_picarea[k].xmax = m_picarea[k].xmax;
			m_prop_dlg->m_picarea[k].ymax = m_picarea[k].ymax;
		}
		m_prop_dlg->m_picarea_no = m_picarea_no;
		if (m_pic_params!="")
		{
/*
.....m_pic_params is always display text choice for choicebox, but save into params
.....as number to consistenece with NCL form
*/
			CString pic_params;
			if (m_itype==2)
			{
				S_convert_choice_params(m_pic_params, m_choices, 1, pic_params);
				m_prop_dlg->m_picarea[indx].params = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_prop_dlg->m_picarea[indx].params, pic_params);
			}
			else
			{
				m_prop_dlg->m_picarea[indx].params = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_prop_dlg->m_picarea[indx].params, m_pic_params);
			}
		}
		else
			m_prop_dlg->m_picarea[indx].params = NULL;
		if (m_pic_tooltip!="")
		{
			m_prop_dlg->m_picarea[indx].tooltext = (char*) uu_malloc((100)*sizeof(char));
			strcpy(m_prop_dlg->m_picarea[indx].tooltext, m_pic_tooltip);
		}
		else
			m_prop_dlg->m_picarea[indx].tooltext = NULL;
		strcpy(m_prop_dlg->m_picarea[indx].name, m_pic_label);
		m_prop_dlg->m_picarea[indx].xmin = m_pic_rect[0];
		m_prop_dlg->m_picarea[indx].ymin = m_pic_rect[1];
		m_prop_dlg->m_picarea[indx].xmax = m_pic_rect[2];
		m_prop_dlg->m_picarea[indx].ymax = m_pic_rect[3];
	}
	else
		m_prop_dlg->m_picarea_no = 0;
	return m_prop_dlg;
}

void CNCLFormPView::OnColorBut1()
{
	char fgstr[64], bgstr[64], tempstr[200];
	m_ffg = GetIColor(m_ffg);
	COLORREF bcolor;
	if (m_ffg<0)
	{
		bcolor = ::GetSysColor(COLOR_BTNFACE); 
	}
	else
	{
		bcolor = RGB(uw_color_table[m_ffg][0], 
					uw_color_table[m_ffg][1], 
					uw_color_table[m_ffg][2]);
	}
	m_button[0].set_color(bcolor, RGB(0,0,0));
	m_button[0].Invalidate();
	m_button[0].UpdateWindow();
	if (m_ffg==-1)
	{
		strcpy(fgstr, "Default");
	}
	else
		strcpy(fgstr, uw_color_name[m_ffg]);
	if (m_fbg==-1)
	{
		strcpy(bgstr, "Default");
	}
	else
		strcpy(bgstr, uw_color_name[m_fbg]);
	sprintf(tempstr, "%s, %s", fgstr, bgstr);
	if ((m_parent!=NULL)&&(m_dtype!=-2))
	{
		if (m_color!=tempstr)
		{
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(10);
			m_color = tempstr;
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		}
	}
}

void CNCLFormPView::OnColorBut2()
{
	char fgstr[64], bgstr[64], tempstr[200];
	m_fbg = GetIColor(m_fbg);

	COLORREF bcolor;
	if (m_fbg<0)
	{
		bcolor = ::GetSysColor(COLOR_BTNFACE); 
	}
	else
	{
		bcolor = RGB(uw_color_table[m_fbg][0], 
					uw_color_table[m_fbg][1], 
					uw_color_table[m_fbg][2]);
	}
	m_button[1].set_color(bcolor, RGB(0,0,0));
	m_button[1].Invalidate();
	m_button[1].UpdateWindow();
	if (m_ffg==-1)
	{
		strcpy(fgstr, "Default");
	}
	else
		strcpy(fgstr, uw_color_name[m_ffg]);
	if (m_fbg==-1)
	{
		strcpy(bgstr, "Default");
	}
	else
		strcpy(bgstr, uw_color_name[m_fbg]);
	sprintf(tempstr, "%s, %s", fgstr, bgstr);
	if ((m_parent!=NULL)&&(m_dtype!=-2))
	{
		if (m_color!=tempstr)
		{
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(10);
			m_color = tempstr;
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		}
	}
}

void CNCLFormPView::OnColorBut3()
{
	char fgstr[64], bgstr[64], tempstr[200];
	m_pfg = GetIColor(m_pfg);
	COLORREF bcolor;
	if (m_pfg<0)
	{
		bcolor = ::GetSysColor(COLOR_BTNFACE); 
	}
	else
	{
		bcolor = RGB(uw_color_table[m_pfg][0], 
					uw_color_table[m_pfg][1], 
					uw_color_table[m_pfg][2]);
	}
	m_button[2].set_color(bcolor, RGB(0,0,0));
	m_button[2].Invalidate();
	m_button[2].UpdateWindow();
	if (m_pfg==-1)
	{
		strcpy(fgstr, "Default");
	}
	else
		strcpy(fgstr, uw_color_name[m_pfg]);
	if (m_pbg==-1)
	{
		strcpy(bgstr, "Default");
	}
	else
		strcpy(bgstr, uw_color_name[m_pbg]);
	sprintf(tempstr, "%s, %s", fgstr, bgstr);
	if ((m_parent!=NULL)&&(m_dtype!=-2))
	{
		if (m_pcolor!=tempstr)
		{
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(10);
			m_pcolor = tempstr;
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		}
	}
}

void CNCLFormPView::OnColorBut4()
{
	char fgstr[64], bgstr[64], tempstr[200];
	m_pbg = GetIColor(m_pbg);
	COLORREF bcolor;
	if (m_pbg<0)
	{
		bcolor = ::GetSysColor(COLOR_BTNFACE); 
	}
	else
	{
		bcolor = RGB(uw_color_table[m_pbg][0], 
					uw_color_table[m_pbg][1], 
					uw_color_table[m_pbg][2]);
	}
	m_button[3].set_color(bcolor, RGB(0,0,0));
	m_button[3].Invalidate();
	m_button[3].UpdateWindow();
	if (m_pfg==-1)
	{
		strcpy(fgstr, "Default");
	}
	else
		strcpy(fgstr, uw_color_name[m_pfg]);
	if (m_pbg==-1)
	{
		strcpy(bgstr, "Default");
	}
	else
		strcpy(bgstr, uw_color_name[m_pbg]);
	sprintf(tempstr, "%s, %s", fgstr, bgstr);
	if ((m_parent!=NULL)&&(m_dtype!=-2))
	{
		if (m_pcolor!=tempstr)
		{
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(10);
			m_pcolor = tempstr;
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		}
	}
}

/***********************************************************************
c
c   FUNCTION: OnSaveLabel()
c
c       callback function when lable edit field need saved
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveLabel()
{
	CString label;
	GetDlgItem(IDC_EDIT1)->GetWindowText(label);
	if ((m_parent!=NULL)&&(m_dtype!=-2))
	{
		if (label!=m_label)
		{
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(5);
			m_label = label;
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		}
	}
}

/***********************************************************************
c
c   FUNCTION: OnSavePosition()
c
c       callback function when position edit field need saved
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSavePosition()
{
	char tempstr[80];
	double rval[2];
	int inum;

	GetDlgItem(IDC_EDIT2)->GetWindowText(tempstr, 80);
	int status = ul_to_reals(rval, &inum, 2, tempstr);
	if (status!=0)
	{
		return;
	}
	if ((m_parent!=NULL)&&(m_dtype!=-2))
	{
/*
.....check if the item is off the form window, if it is 
.....give the error message and not change the position
*/
		int x,y;
		x = (int)rval[0];
		y = (int)rval[1];
		int inside = ((CNCLFdsnFrame*)m_parent)->CheckPos(x, y);
		if ((inside==1)&&((m_pos[0] != rval[0])||(m_pos[1] != rval[1])))
		{
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(4);
			m_pos[0] = rval[0];
			m_pos[1] = rval[1];
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		}
		else if ((inside==0)&&((m_pos[0] != x)||(m_pos[1] != y)))
		{
//			sprintf(tempstr, "The position %d, %d is outside the form window, not allowed!", 
//				(int)rval[0], (int)rval[1]);
//			MessageBox(tempstr, "Input Error", MB_OK);
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(4);
			m_pos[0] = x;
			m_pos[1] = y;
			sprintf(tempstr, "%d, %d", m_pos[0], m_pos[1]);
			GetDlgItem(IDC_EDIT2)->SetWindowText(tempstr);
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		}
	}
}
/***********************************************************************
c
c   FUNCTION: OnSaveSize()
c
c       callback function when size edit field need saved
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveSize()
{
	char tempstr[80];
	double rval[2];
	int inum;

	GetDlgItem(IDC_EDIT3)->GetWindowText(tempstr, 80);
	int status = ul_to_reals(rval, &inum, 2, tempstr);
	if (status!=0)
	{
		return;
	}
	if (((m_parent!=NULL)&&(m_dtype!=-2))&&((m_size[0] != rval[0])||(m_size[1] != rval[1])))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(4);
		m_size[0] = rval[0];
		m_size[1] = rval[1];
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
}

/***********************************************************************
c
c   FUNCTION: OnSaveLength()
c
c       callback function when length edit field need saved
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveLength()
{
	char tempstr[80];
	GetDlgItem(IDC_EDIT6)->GetWindowText(tempstr, 80);
	if ((m_parent!=NULL)&&(m_dtype!=-2))
	{
		if (m_len!=atoi(tempstr))
		{
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(12);
			m_len = atoi(tempstr);
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		}
	}
}
/***********************************************************************
c
c   FUNCTION: OnSaveFontSize()
c
c       callback function when font size edit field need saved
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveFontSize()
{
	char tempstr[80];
	GetDlgItem(IDC_EDIT11)->GetWindowText(tempstr, 80);
	if ((m_parent!=NULL)&&(m_dtype!=-2))
	{
		if (m_font!=atof(tempstr))
		{
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(6);
			m_font = atof(tempstr);
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		}
	}
}
/***********************************************************************
c
c   FUNCTION: OnSaveInputNo()
c
c       callback function when input item number edit field need saved
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveInputNo()
{
	char tempstr[80], msg[256];
	static int func_on = 0;
	int i;
	if (func_on)
		return;
	if (m_dtype==5)
		return;
	func_on = 1;
	if (m_itype!=-1)
	{
		GetDlgItem(IDC_EDIT12)->GetWindowText(tempstr, 80);
		if (m_macro_flag==0)
		{
			i = atoi (tempstr);
			if ((i<0)||(i>m_inputno))
			{
/*
......error message
*/
				sprintf(msg, "You must enter a number between 0 to %d.", m_inputno);
				MessageBox(msg, "Input Error", MB_OK);
				GetDlgItem(IDC_EDIT12)->SetFocus();
				func_on = 0;
				return;
			}
			if (i==m_input_itemno)
				return;
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(8);
			m_input_itemno = i;
		}
		else
		{
/*
.....check this input is a valid MACRO parameter name
*/
			int ret = 1;
			for (i=0; i<((CNCLFdsnFrame*)m_parent)->m_macro_parmno;i++)
			{
				if (stricmp(tempstr, ((CNCLFdsnFrame*)m_parent)->m_macro_parms[i])==0)
				{
					ret = 0;
					((CNCLFdsnFrame*)m_parent)->SaveUndoItem(8);
					if (i==m_input_itemno)
						return;
					m_input_itemno = i;
					break;
				}
			}
			if (ret!=0)
			{
/*
......error message
*/
				MessageBox("You must enter a valid MACRO parameter label!", "Input Error", MB_OK);
				GetDlgItem(IDC_EDIT12)->SetFocus();
				func_on = 0;
				return;
			}
		}
	}
	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(m_itype!=-1))
	{
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
	func_on = 0;
}
/***********************************************************************
c
c   FUNCTION: OnSavePageNo()
c
c       callback function when input page number edit field need saved
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSavePageNo()
{
	char tempstr[80], msg[256];
	static int func_on = 0;
	int i;
	if (func_on)
		return;
	func_on = 1;
	GetDlgItem(IDC_EDIT13)->GetWindowText(tempstr, 80);
	i = atoi (tempstr);
	if ((i<0)||(i>m_secno))
	{
/*
......error message
*/
		sprintf(msg, "You must enter a number between 0 to %d.", m_secno);
		MessageBox(msg, "Input Error", MB_OK);
		GetDlgItem(IDC_EDIT13)->SetFocus();	
		func_on = 0;
		return;
	}
	int page_chg;
	if (m_page!=i-1)
		page_chg = 1;
	else
		page_chg = 0;
	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(page_chg))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(11);
		m_page = i-1;
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
		((CNCLFdsnFrame*)m_parent)->reset_selvalue();
	}
	func_on = 0;
}
/***********************************************************************
c
c   FUNCTION: OnSaveActive()
c
c       callback function when Active combobox selection field changed
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveActive()
{
	int active = ((CComboBox*)GetDlgItem(IDC_COMBO3))->GetCurSel() - 1;
	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(active!=m_active))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(13);
		m_active = active;
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
}
/***********************************************************************
c
c   FUNCTION: OnSaveLimit()
c
c       callback function when 'limit' edit field need saved
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveLimit()
{
	CString limit = "";
	if (m_itype!=-1)
	{
		GetDlgItem(IDC_EDIT4)->GetWindowText(limit);
	}
	if ((m_parent!=NULL)&&(m_dtype!=-2))
	{
		if (limit!=m_limit)
		{
			((CNCLFdsnFrame*)m_parent)->SaveUndoItem(14);
			m_limit = limit;
			((CNCLFdsnFrame*)m_parent)->SaveProperty();	
		}
	}
}
/***********************************************************************
c
c   FUNCTION: OnSavePrec()
c
c       callback function when 'Prec' edit field need saved
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSavePrec()
{
	char tempstr[80];
	int prec=1;
	if (m_itype!=-1)
	{
		GetDlgItem(IDC_EDIT7)->GetWindowText(tempstr, 80);
		prec = atoi (tempstr);
	}
	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(m_prec!=prec))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(15);
		m_prec = prec;
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
}
/***********************************************************************
c
c   FUNCTION: OnSaveRange()
c
c       callback function when 'data range' edit field need saved
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveRange()
{
	char tempstr[80];
	int inum, status, nc;
	UD_DASIN range[2]; 
	if (m_itype!=-1)
	{
		GetDlgItem(IDC_EDIT8)->GetWindowText(tempstr, 80);
		nc = strlen(tempstr);
		ul_strip_blanks(tempstr, &nc);
		if (nc==0)
		{
			m_range_flag = 0;
			return;
		}
		status = S_get_range(tempstr, range, m_type);
		if (status!=0)
		{
/*
......error message
*/
			MessageBox("The Range input is wrong!", "Input Error", MB_OK);
			GetDlgItem(IDC_EDIT8)->SetFocus();
			return;
		}
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(16);
		m_range_flag = 1;
	}
	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(m_range_flag==1))
	{
		S_get_range(tempstr, m_range, m_type);
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
}

void CNCLFormPView::OnSavePicName()
{
	if (m_dtype!=4)
		return;
	if (m_itype==13) //not for processor field
		return;

	char tempstr[81], *tok;
	int changed = 0;
	GetDlgItem(IDC_EDIT14)->GetWindowText(tempstr, 80);

	if (m_pic_label.CompareNoCase(tempstr))
		changed = 1;

	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(changed==1))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(17);
		m_pic_label = tempstr;
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
}
void CNCLFormPView::OnSaveTooltip()
{
	if (m_dtype!=4)
		return;
	if (m_itype==13) //not for processor field
		return;

	char tempstr[81], *tok;
	int changed = 0;
	GetDlgItem(IDC_EDIT15)->GetWindowText(tempstr, 80);

	if (m_pic_tooltip.CompareNoCase(tempstr))
		changed = 1;

	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(changed==1))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(17);
		m_pic_tooltip = tempstr;
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
}
void CNCLFormPView::OnSavePicPos()
{
	if (m_dtype!=4)
		return;
	if (m_itype==13) //not for processor field
		return;

	char tempstr[81], *tok;
	float pic_rect[4];
	int changed = 0;
	GetDlgItem(IDC_EDIT16)->GetWindowText(tempstr, 80);
/*
.....in minx, miny, maxx, maxy format
*/
	pic_rect[0] = -1000;
	pic_rect[1] = -1000;
	pic_rect[2] = -1000;
	pic_rect[3] = -1000;
	if (strlen(tempstr)!=0)
	{
		tok = strtok(tempstr, " ,");
		if (tok!=NULL)
		{
			pic_rect[0] = atof(tok);
			tok = strtok(NULL, " ,");
			if (tok!=NULL)
			{
				pic_rect[1] = atof(tok);
				tok = strtok(NULL, " ,");
				if (tok!=NULL)
				{
					pic_rect[2] = atof(tok);
					tok = strtok(NULL, " ,");
					if (tok!=NULL)
					{
						pic_rect[3] = atof(tok);
					}
				}
			}
		}	
	}
	if ((m_pic_rect[0]!=pic_rect[0])||(m_pic_rect[1]!=pic_rect[1])
		||(m_pic_rect[2]!=pic_rect[2])||(m_pic_rect[3]!=pic_rect[3]))
		changed = 1;

	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(changed==1))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(17);
		m_pic_rect[0] = pic_rect[0];
		m_pic_rect[1] = pic_rect[1];
		m_pic_rect[2] = pic_rect[2];
		m_pic_rect[3] = pic_rect[3];
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
}
void CNCLFormPView::OnSavePicParms()
{
	if (m_dtype!=4)
		return;
	if (m_itype==13) //not for processor field
		return;

	char tempstr[81], *tok;
	int changed = 0;
	GetDlgItem(IDC_EDIT17)->GetWindowText(tempstr, 80);

	if (m_pic_params.CompareNoCase(tempstr))
		changed = 1;

	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(changed==1))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(17);
		m_pic_params = tempstr;
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
}

void CNCLFormPView::OnLoadActivePic()
{
	if (m_dtype!=4)
		return;
	if (m_itype==13) //not for processor field
		return;

	char tempstr[81];
	int changed = 0;
	int sav = m_pic_act_area;
	GetDlgItem(IDC_EDIT18)->GetWindowText(tempstr, 80);
	int num = atoi(tempstr);
	if (num==m_pic_act_area)
		return;
	if (num<0)
	{
		MessageBox("The Active Picture index must be >= 0!", "Input Error", MB_OK);
		GetDlgItem(IDC_EDIT18)->SetFocus();
	}
/*
.....when reloading, don't save the form
*/
	m_reloading = 1;
	((CNCLFdsnFrame*)m_parent)->LoadActiveHotSpot(m_dtype, m_itype, m_input_itemno, num);
	m_reloading = 0;
}

/***********************************************************************
c
c   FUNCTION: OnSaveInputType()
c
c       callback function when 'input type' combo field changed
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveInputType()
{
	int type = ((CComboBox*)GetDlgItem(IDC_COMBO1))->GetCurSel();
	if ((m_parent!=NULL)&&(m_dtype!=-2))
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(7);
	if (m_dtype==4)
	{
		if ((m_itype==3)||(m_itype==11)||(m_itype==16))
/*
......EDIT
*/
		{
			if (m_macro_flag)
			{
				if (type==0)
					m_type = UD_DASSTRING;
				else
					m_type = UD_DASUNITLESS;
			}
			else
			{
				if (type==0)
					m_type = UD_DASCART;
				if (type==1)
					m_type = UD_DASVAL;
				if (type==2)
					m_type = UD_DASDISTANCE;
				if (type==3)
					m_type = UD_DASINT;
				if (type==4)
					m_type = UD_DASVEC;
				if (type==5)
					m_type = UD_DASSTRING;
				if (type==6)
					m_type = UD_DASNDC;
				if (type==7)
					m_type = UD_DASANGLE;
				if (type==8)
					m_type = UD_DASUNITLESS;
				if (type==9)
					m_type = UD_DASPLANE;
				if (type==10)
					m_type = UD_DASSCALAR;
				if (type==11)
					m_type = UD_SCACART;
				if (type==12)
					m_type = UD_SCAVAL;
				if (type==13)
					m_type = UD_SCADISTANCE;
				if (type==14)
					m_type = UD_SCAINT;
				if (type==15)
					m_type = UD_SCAVEC;
				if (type==16)
					m_type = UD_SCANDC;
				if (type==17)
					m_type = UD_SCAANGLE;
				if (type==18)
					m_type = UD_SCAUNITLESS;
			}
		}
		else
		{
			if ((m_itype==1)&&(m_itype==7))
				m_type = UD_DASINT;
			else
				m_type = UD_DASSTRING;
		}
	}
	else
	{
		m_type = UD_DASSTRING;
	}
	if (m_type==UD_DASUNITLESS)
	{
		GetDlgItem(IDC_EDIT6)->EnableWindow(1);
		GetDlgItem(IDC_EDIT7)->EnableWindow(1);
		GetDlgItem(IDC_EDIT8)->EnableWindow(1);
		GetDlgItem(IDC_LENLAB)->EnableWindow(1);
		GetDlgItem(IDC_PRELAB)->EnableWindow(1);
		GetDlgItem(IDC_RANGELAB)->EnableWindow(1);
	}
	else
	{
		GetDlgItem(IDC_EDIT6)->EnableWindow(0);
		GetDlgItem(IDC_EDIT7)->EnableWindow(0);
		GetDlgItem(IDC_EDIT8)->EnableWindow(0);
		GetDlgItem(IDC_LENLAB)->EnableWindow(0);
		GetDlgItem(IDC_PRELAB)->EnableWindow(0);
		GetDlgItem(IDC_RANGELAB)->EnableWindow(0);
	}
	if (((m_itype==3)||(m_itype==11)||(m_itype==16))
			&&(m_type==UD_DASSTRING))
	{
		GetDlgItem(IDC_COMBO2)->EnableWindow(1);
		GetDlgItem(IDC_COMLAB2)->EnableWindow(1);
		GetDlgItem(IDC_COMLAB2)->SetWindowText("Input:");
	}
	else if (m_itype!=24)
	{
		GetDlgItem(IDC_COMLAB2)->SetWindowText("Input:");
		((CComboBox *)GetDlgItem(IDC_COMBO2))->SetCurSel(0);
		GetDlgItem(IDC_COMBO2)->EnableWindow(0);
		GetDlgItem(IDC_COMLAB2)->EnableWindow(0);
/*
......change the button to label prompt
*/
		if (m_parent!=NULL)
		{
			((CNCLFdsnFrame*)m_parent)->ChangePromptType(0);
			GetDlgItem(IDC_LIMITLAB)->EnableWindow(0);
			GetDlgItem(IDC_EDIT4)->EnableWindow(0);
			m_input = FORM_STRING;
		}
	}
	else
	{
	}
	if ((m_parent!=NULL)&&(m_dtype!=-2))
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
}


/***********************************************************************
c
c   FUNCTION: SaveJustified()
c
c       callback function when 'Justified' combo field changed
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::SaveJustified()
{
	if (m_itype!=24)
		return;
	int justified = ((CComboBox*)GetDlgItem(IDC_COMBO2))->GetCurSel();
	int savjtf = m_justified;
	if ((m_parent!=NULL)&&(m_dtype!=-2)&&(justified!=m_justified))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(7);
		m_justified = justified;
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
}

/***********************************************************************
c
c   FUNCTION: OnSaveStrType()
c
c       callback function when 'string type' combo field changed
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveStrType()
{
	if (m_itype==24)
	{
		SaveJustified();
		return;
	}
	int type = ((CComboBox*)GetDlgItem(IDC_COMBO2))->GetCurSel();
	int savtyp = m_input;
	if ((m_parent!=NULL)&&(m_dtype!=-2))
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(7);
	if ((m_itype==3)||(m_itype==11)||(m_itype==16))
/*
......EDIT
*/
	{
		if (type==0)
			m_input = FORM_STRING;
		if (type==1)
			m_input = FORM_PICK;
		if (type==2)
			m_input = FORM_LOCATE;
		if (type==3)
			m_input = FORM_LABEL;
		if (type==4)
			m_input = FORM_SUBSCR;
		if (m_type!=UD_DASSTRING)
			m_input = FORM_STRING;
		if ((m_input==FORM_STRING)&&(savtyp!=FORM_STRING)&&(savtyp!=0))
/*
......change the button to label prompt
*/
		{
			((CNCLFdsnFrame*)m_parent)->ChangePromptType(0);
			GetDlgItem(IDC_LIMITLAB)->EnableWindow(0);
			GetDlgItem(IDC_EDIT4)->EnableWindow(0);
		}
		if ((m_input!=FORM_STRING)&&(savtyp==FORM_STRING)&&(m_input!=0))
/*
......change the prompt to label button
*/
		{
			((CNCLFdsnFrame*)m_parent)->ChangePromptType(1);
			GetDlgItem(IDC_LIMITLAB)->EnableWindow(1);
			GetDlgItem(IDC_EDIT4)->EnableWindow(1);
			GetDlgItem(IDC_EDIT4)->SetWindowText(m_limit);
		}
	}
	else if (m_itype==23)
	{
		m_input = FORM_PICK;
		GetDlgItem(IDC_LIMITLAB)->EnableWindow(1);
		GetDlgItem(IDC_EDIT4)->EnableWindow(1);
		GetDlgItem(IDC_EDIT4)->SetWindowText(m_limit);
	}
	else if ((m_itype==5)||(m_itype==8)||(m_itype==9)||(m_itype==15)
			||(m_itype==17)||(m_itype==19))
	{
		if (type==0)
			m_input = FORM_STRING;
		if (type==1)
			m_input = FORM_RECORD;
	}
	else
	{
		m_input = FORM_STRING;
	}
	if ((m_parent!=NULL)&&(m_dtype!=-2))
		((CNCLFdsnFrame*)m_parent)->SaveProperty();
}

/***********************************************************************
c
c   FUNCTION: OnSaveChoices()
c
c       callback function when 'choices' edit field changed
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnSaveChoices()
{
	char tempstr[2001];
	GetDlgItem(IDC_EDIT5)->GetWindowText(tempstr, 2000);
	tempstr[2000] = '\0';

	int status=0;
	if ((m_dtype==2)||(m_itype==25))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(9);
		GetDlgItem(IDC_EDIT5)->GetWindowText(m_choices);
		if (m_parent!=NULL)
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		return;
	}
	if (m_macro_flag)
	{
		status = Scheck_choice_vocab(tempstr);
/*
......error message
*/
		if (status!=0)
		{
			MessageBox("The Choice field must contain a list of quoted vocabulary words!", "Input Error", MB_OK);
			GetDlgItem(IDC_EDIT5)->SetFocus();
		}
	}
	if ((status==0)&&(stricmp(tempstr, m_choices)!=0))
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(9);
		GetDlgItem(IDC_EDIT5)->GetWindowText(m_choices);
		if (m_parent!=NULL)
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
		return;
	}
}

/***********************************************************************
c
c   FUNCTION: OnFileBrowse()
c
c       callback function when 'browser' button clicked
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnFileBrowse()
{
	UX_pathname filename, ext, descrip;
	int nc;
	filename[0] = '\0';
	strcpy(ext,"*.bmp,*.jpg,*.gif,*.ico");
	strcpy(descrip, "Picture Files (*.bmp, *.jpg, *.gif, *.ico)");
	ud_get_filename(this, "Enter Picture File name", ext, filename, &nc, descrip, 1);
/*
.....use m_choices field for filename when it is picture
*/
	if (nc>0)
	{
		((CNCLFdsnFrame*)m_parent)->SaveUndoItem(9);
		GetDlgItem(IDC_EDIT5)->SetWindowText(filename);
		GetDlgItem(IDC_EDIT5)->GetWindowText(m_choices);
		if (m_parent!=NULL)
			((CNCLFdsnFrame*)m_parent)->SaveProperty();
	}
}
static void Sconvert_strcolor_indx(CString color, int *fg, int *bg)
{
	char tempstr[80], *tok, fcolor_str[65], bcolor_str[65];
	UM_int2 nc;
	int isub, len = color.GetLength();
	if (len==0)
	{
		*fg = *bg = -1;
		return;
	}
	strcpy(tempstr, color.GetBuffer());
	tok = (char*)strtok (tempstr, " ,\t\r\n");
	if (tok==NULL)
	{
		*fg = *bg = -1;
		return;
	}
	else
	{
		strcpy(fcolor_str, tok);
		tok = (char*)strtok (NULL, " ,\t\r\n");
		if (tok==NULL)
		{
			bcolor_str[0] = '\0';
		}
		else
			strcpy(bcolor_str, tok);

		if (fcolor_str[0]=='\0')
		{
			*fg = -1;
		}
		else
		{
			nc = strlen(fcolor_str);
			isub = 0;
			ncl_getclr_inx(fcolor_str, &nc, &isub, fg);
			if (*fg==-1000)
				*fg = -1;
		}

		if (bcolor_str[0]=='\0')
		{
			*bg = -1;
		}
		else
		{
			nc = strlen(bcolor_str);
			isub = 0;
			ncl_getclr_inx(bcolor_str, &nc, &isub, bg);
			if (*bg==-1000)
				*bg = -1;
		}
	}
}

/***********************************************************************
c
c   FUNCTION: CopyPropertyPage(CNCLFormProp *prop_dlg_from)
c
c       copy the input property page info into class member value
c
c   INPUT:  prop_dlg_from: input property page info
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::CopyPropertyPage(CNCLFormProp *prop_dlg_from)
{
	m_dtype = prop_dlg_from->m_dtype;
	m_itype = prop_dlg_from->m_itype;
	m_label = prop_dlg_from->m_label;
	m_pos[0] = prop_dlg_from->m_pos[0];
	m_size[0] = prop_dlg_from->m_size[0];
	m_pos[1] = prop_dlg_from->m_pos[1];
	m_size[1] = prop_dlg_from->m_size[1];
	m_range_flag = prop_dlg_from->m_range_flag;
	m_range[0] = prop_dlg_from->m_range[0];
	m_range[1] = prop_dlg_from->m_range[1];
	m_font = prop_dlg_from->m_font;
	m_type = prop_dlg_from->m_type;
	m_input = prop_dlg_from->m_input;
	m_justified = prop_dlg_from->m_justified;
	m_len = prop_dlg_from->m_len; 
	m_prec = prop_dlg_from->m_prec;
	m_active = prop_dlg_from->m_active;
	m_input_itemno = prop_dlg_from->m_input_itemno;
	m_limit = prop_dlg_from->m_limit;
	m_choices = prop_dlg_from->m_choices;
	m_color = prop_dlg_from->m_color;
	m_pcolor = prop_dlg_from->m_pcolor;
	Sconvert_strcolor_indx(m_color, &m_ffg, &m_fbg);
	Sconvert_strcolor_indx(m_pcolor, &m_pfg, &m_pbg);
	m_page = prop_dlg_from->m_page;

	int k;
	if ((m_picarea_no>0)&&(m_picarea!=NULL))
	{
		for (k=0; k<m_picarea_no; k++)
		{
			if (m_picarea[k].params!=NULL)
				uu_free(m_picarea[k].params);
			m_picarea[k].params = NULL;
			if (m_picarea[k].tooltext!=NULL)
				uu_free(m_picarea[k].tooltext);
			m_picarea[k].tooltext = NULL;
		}
		uu_free((char*)m_picarea);
		m_picarea = NULL;
	}
	m_picarea_no = prop_dlg_from->m_picarea_no;
	int indx = prop_dlg_from->m_pic_act_area;
	if ((prop_dlg_from->m_picarea_no>0)&&(prop_dlg_from->m_picarea!=NULL))
	{	
		m_picarea = (UD_PICAREA *) uu_malloc(m_picarea_no*sizeof(UD_PICAREA));
		for (k=0; k<m_picarea_no; k++)
		{
			if (prop_dlg_from->m_picarea[k].params!=NULL)
			{
				m_picarea[k].params = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_picarea[k].params, prop_dlg_from->m_picarea[k].params);
			}
			else
				m_picarea[k].params = NULL;
			if (prop_dlg_from->m_picarea[k].tooltext!=NULL)
			{
				m_picarea[k].tooltext = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_picarea[k].tooltext, prop_dlg_from->m_picarea[k].tooltext);
			}
			else
				m_picarea[k].tooltext = NULL;
			strcpy(m_picarea[k].name, prop_dlg_from->m_picarea[k].name);
			m_picarea[k].xmin = prop_dlg_from->m_picarea[k].xmin;
			m_picarea[k].ymin = prop_dlg_from->m_picarea[k].ymin;
			m_picarea[k].xmax = prop_dlg_from->m_picarea[k].xmax;
			m_picarea[k].ymax = prop_dlg_from->m_picarea[k].ymax;
		}
		m_pic_label = prop_dlg_from->m_picarea[indx].name;
		if (prop_dlg_from->m_picarea[indx].tooltext!=NULL)
			m_pic_tooltip = prop_dlg_from->m_picarea[indx].tooltext;
		else
			m_pic_tooltip = "";	
		m_pic_rect[0] = prop_dlg_from->m_picarea[indx].xmin;
		m_pic_rect[2] = prop_dlg_from->m_picarea[indx].xmax;
		m_pic_rect[1] = prop_dlg_from->m_picarea[indx].ymin;
		m_pic_rect[3] = prop_dlg_from->m_picarea[indx].ymax;
		if (prop_dlg_from->m_picarea[indx].params!=NULL)
		{
			if (m_itype!=2)
				m_pic_params = prop_dlg_from->m_picarea[indx].params;
			else
			{
				CString pic_params, pic_params2;
				pic_params = prop_dlg_from->m_picarea[indx].params;
				S_convert_choice_params(pic_params, m_choices, 0, pic_params2);
				m_pic_params = pic_params2;
			}
		}
		else
			m_pic_params = "";
		m_pic_act_area = prop_dlg_from->m_pic_act_area;	
	}
	else
	{
		m_pic_act_area = 0;
		m_pic_params = "";
		m_pic_label = "";
		m_pic_tooltip = "";
		m_pic_rect[0] = -1000;
		m_pic_rect[2] = -1000;
		m_pic_rect[1] = -1000;
		m_pic_rect[3] = -1000;
	}
}

/***********************************************************************
c
c   FUNCTION: CopyPropertyPage(CNCLFormProp *prop_dlg_from)
c
c       copy the input property page info into class member value
c
c   INPUT:  prop_dlg_from: input property page info
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OpenPropertyPage(CNCLFormProp *prop_dlg, int flag)
{
	if (flag==0)
	{
		if (m_prop_dlg==NULL)
			return;
	}
	if (m_prop_dlg==NULL)
	{
		m_prop_dlg = new CNCLFormProp(-1, -1, this);
	}
	if (flag==0)
	{
		CopyPropertyPage(prop_dlg);
		filldata();
	}
	ShowWindow(TRUE);
}

BOOL CNCLFormPView::PreCreateWindow(CREATESTRUCT& cs)
{
	BOOL bPreCreated = CFormView::PreCreateWindow(cs);
	return bPreCreated;
}

void CNCLFormPView::CreateColorBut()
{
/*
.....create the 4 color button for color
*/
	m_button[0].SubclassDlgItem(IDC_FFG_CBUT,this);
	m_button[0].set_color(GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
	m_button[0].SetFont(GetFont());	
	m_button[0].ShowWindow(SW_SHOW);

	m_button[1].SubclassDlgItem(IDC_FBG_CBUT,this);
	m_button[1].set_color(GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
	m_button[1].SetFont(GetFont());	
	m_button[1].ShowWindow(SW_SHOW);

	m_button[2].SubclassDlgItem(IDC_PFG_CBUT,this);
	m_button[2].set_color(GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
	m_button[2].SetFont(GetFont());	
	m_button[2].ShowWindow(SW_SHOW);

	m_button[3].SubclassDlgItem(IDC_PBG_CBUT,this);
	m_button[3].set_color(GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
	m_button[3].SetFont(GetFont());	
	m_button[3].ShowWindow(SW_SHOW);
}
/***********************************************************************
c
c   FUNCTION: OnInitialUpdate()
c
c       Initial view value here
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::OnInitialUpdate()
{
	ASSERT_VALID(this);
	if (!UpdateData(FALSE))
		TRACE(traceAppMsg, 0, "UpdateData failed during formview initial update.\n");
	CFormView::OnInitialUpdate();
/*
.....init all data
*/
	filldata();
	OnSaveInputType();
	OnSaveStrType();
	m_init = 1;
}

/***********************************************************************
c
c   FUNCTION: filldata()
c
c       Fill all data into the window fields
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::filldata()
{
	int cont, j;
	char tempstr[80];
	COLORREF bcolor;

	if (m_dtype==-2)
/*
......none selected, disable every fields
*/
	{
		GetDlgItem(IDC_BUTTON1)->ShowWindow(0);
		GetDlgItem(IDC_EDIT1)->EnableWindow(0);
		GetDlgItem(IDC_LABEL)->EnableWindow(0);
		GetDlgItem(IDC_EDIT2)->EnableWindow(0);
		GetDlgItem(IDC_POSLABEL)->EnableWindow(0);
		GetDlgItem(IDC_EDIT3)->EnableWindow(0);
		GetDlgItem(IDC_SIZELABLE)->EnableWindow(0);
		GetDlgItem(IDC_COMBO1)->EnableWindow(0);
		GetDlgItem(IDC_COMLAB)->EnableWindow(0);
		GetDlgItem(IDC_COMLAB2)->SetWindowText("Input:");
		GetDlgItem(IDC_COMLAB2)->EnableWindow(0);
		GetDlgItem(IDC_COMBO2)->EnableWindow(0);
		GetDlgItem(IDC_COMLAB3)->EnableWindow(0);
		GetDlgItem(IDC_COMBO3)->EnableWindow(0);
		GetDlgItem(IDC_LIMITLAB)->EnableWindow(0);
		GetDlgItem(IDC_EDIT4)->EnableWindow(0);
		GetDlgItem(IDC_CHOICELAB)->EnableWindow(0);
		GetDlgItem(IDC_EDIT5)->EnableWindow(0);
		GetDlgItem(IDC_LENLAB)->EnableWindow(0);
		GetDlgItem(IDC_EDIT6)->EnableWindow(0);
		GetDlgItem(IDC_PRELAB)->EnableWindow(0);
		GetDlgItem(IDC_EDIT7)->EnableWindow(0);
		GetDlgItem(IDC_RANGELAB)->EnableWindow(0);
		GetDlgItem(IDC_EDIT8)->EnableWindow(0);
		GetDlgItem(IDC_COLORLAB)->EnableWindow(0);
//		GetDlgItem(IDC_EDIT9)->EnableWindow(0);
		m_button[0].set_color(::GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
		m_button[0].Invalidate();
		m_button[0].UpdateWindow();
		m_button[1].set_color(::GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
		m_button[1].Invalidate();
		m_button[1].UpdateWindow();
		m_button[0].EnableWindow(0);
		m_button[1].EnableWindow(0);
		GetDlgItem(IDC_COLORLAB2)->EnableWindow(0);
		m_button[2].EnableWindow(0);
		m_button[3].EnableWindow(0);
		m_button[2].set_color(::GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
		m_button[2].Invalidate();
		m_button[2].UpdateWindow();
		m_button[3].set_color(::GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
		m_button[3].Invalidate();
		m_button[3].UpdateWindow();
		GetDlgItem(IDC_FONTLAB)->EnableWindow(0);
		GetDlgItem(IDC_EDIT11)->EnableWindow(0);
		GetDlgItem(IDC_ITEM_NOLABLE)->EnableWindow(0);
		GetDlgItem(IDC_EDIT12)->EnableWindow(0);
		if (m_macro_flag==0)
		{
			GetDlgItem(IDC_PAGE_NOLABLE)->EnableWindow(0);
			GetDlgItem(IDC_EDIT13)->EnableWindow(0);
		}
		else
		{
			GetDlgItem(IDC_PAGE_NOLABLE)->ShowWindow(SW_HIDE);
			GetDlgItem(IDC_EDIT13)->ShowWindow(SW_HIDE);
		}
		GetDlgItem(IDC_PIC_LNMLABLE)->EnableWindow(0);
		GetDlgItem(IDC_EDIT14)->EnableWindow(0);
		GetDlgItem(IDC_PIC_LTOOLTIP)->EnableWindow(0);
		GetDlgItem(IDC_EDIT15)->EnableWindow(0);
		GetDlgItem(IDC_PIC_LPOS)->EnableWindow(0);
		GetDlgItem(IDC_EDIT16)->EnableWindow(0);
		GetDlgItem(IDC_PIC_LPARMS)->EnableWindow(0);
		GetDlgItem(IDC_EDIT17)->EnableWindow(0);
		GetDlgItem(IDC_PIC_INDEX)->EnableWindow(0);
		GetDlgItem(IDC_EDIT18)->EnableWindow(0);
		return;
	}
	if (m_itype==25)
		GetDlgItem(IDC_LABEL)->SetWindowText("Image:");
	else
		GetDlgItem(IDC_LABEL)->SetWindowText("Label:");
		
	GetDlgItem(IDC_EDIT1)->SetWindowText(m_label);

	if ((m_dtype==2)||(m_dtype==5)||(m_itype==-1)
		|| (!((m_dtype==4)&&
			((m_itype==5) ||(m_itype == 19)||(m_itype==17)))))
	{
		GetDlgItem(IDC_EDIT1)->EnableWindow(1);
		GetDlgItem(IDC_LABEL)->EnableWindow(1);
	}
	else
	{
		GetDlgItem(IDC_EDIT1)->EnableWindow(0);
		GetDlgItem(IDC_LABEL)->EnableWindow(0);
	}
	GetDlgItem(IDC_EDIT2)->EnableWindow(1);
	GetDlgItem(IDC_POSLABEL)->EnableWindow(1);
	sprintf(tempstr, "%d, %d", m_pos[0], m_pos[1]);
	GetDlgItem(IDC_EDIT2)->SetWindowText(tempstr);

	GetDlgItem(IDC_EDIT3)->EnableWindow(1);
	GetDlgItem(IDC_SIZELABLE)->EnableWindow(1);
	sprintf(tempstr, "%d, %d", m_size[0], m_size[1]);
	GetDlgItem(IDC_EDIT3)->SetWindowText(tempstr);

	CComboBox *cmbbx;
	cmbbx = (CComboBox*)GetDlgItem(IDC_COMBO1);
	cont = cmbbx->GetCount();
	for (j = 0; j<cont; j++)
	{
		cmbbx->DeleteString(0);
	}
	int type = 0;
	if (m_dtype==4)
	{
		if ((m_itype==3)||(m_itype==11)||(m_itype==16))
/*
......EDIT
*/
		{
			if (m_type==UD_DASCART)
				type = 0;
			if (m_type==UD_DASVAL)
				type = 1;
			if (m_type==UD_DASDISTANCE)
				type = 2;
			if (m_type==UD_DASINT)
				type = 3;
			if (m_type==UD_DASVEC)
				type = 4;
			if (m_type==UD_DASSTRING)
				type = 5;
			if (m_type==UD_DASNDC)
				type = 6;
			if (m_type==UD_DASANGLE)
				type = 7;
			if (m_type==UD_DASUNITLESS)
				type = 8;
			if (m_type==UD_DASPLANE)
				type = 9;
			if (m_type==UD_DASSCALAR)
				type = 10;
			if (m_type==UD_SCACART)
				type = 11;
			if (m_type==UD_SCAVAL)
				type = 12;
			if (m_type==UD_SCADISTANCE)
				type = 13;
			if (m_type==UD_SCAINT)
				type = 14;
			if (m_type==UD_SCAVEC)
				type = 15;
			if (m_type==UD_SCANDC)
				type = 16;
			if (m_type==UD_SCAANGLE)
				type = 17;
			if (m_type==UD_SCAUNITLESS)
				type = 18;
			if (m_macro_flag)
			{
				cmbbx->AddString("Text");
				cmbbx->AddString("Value");
				if (m_type==UD_DASSTRING)
					cmbbx->SetCurSel(0);
				else
					cmbbx->SetCurSel(1);
			}
			else
			{
				cmbbx->AddString("UD_DASCART");
				cmbbx->AddString("UD_DASVAL");
				cmbbx->AddString("UD_DASDISTANCE");
				cmbbx->AddString("UD_DASINT");
				cmbbx->AddString("UD_DASVEC");
				cmbbx->AddString("UD_DASSTRING");
				cmbbx->AddString("UD_DASNDC");
				cmbbx->AddString("UD_DASANGLE");
				cmbbx->AddString("UD_DASUNITLESS");
				cmbbx->AddString("UD_DASPLANE");
				cmbbx->AddString("UD_DASSCALAR");
				cmbbx->AddString("UD_SCACART");
				cmbbx->AddString("UD_SCAVAL");
				cmbbx->AddString("UD_SCADISTANCE");
				cmbbx->AddString("UD_SCAINT");
				cmbbx->AddString("UD_SCAVEC");
				cmbbx->AddString("UD_SCANDC");
				cmbbx->AddString("UD_SCAANGLE");
				cmbbx->AddString("UD_SCAUNITLESS");
				cmbbx->SetCurSel(type);
			}
			cmbbx->EnableWindow(1);
			GetDlgItem(IDC_COMLAB)->EnableWindow(1);
		}
		else
		{
			if (m_macro_flag==0)
			{
				if ((m_itype==7)||(m_itype==24))
					cmbbx->AddString("UD_DASINT");
				else
					cmbbx->AddString("UD_DASSTRING");
			}
			else
			{
				if (m_itype==7)
					cmbbx->AddString("Value");
				else
					cmbbx->AddString("Text");
			}
			cmbbx->EnableWindow(0);
			GetDlgItem(IDC_COMLAB)->EnableWindow(0);
			cmbbx->SetCurSel(0);
		}
	}
	else
	{
		if (m_macro_flag==0)
			cmbbx->AddString("UD_DASSTRING");
		else
			cmbbx->AddString("Text");
		cmbbx->EnableWindow(0);
		GetDlgItem(IDC_COMLAB)->EnableWindow(0);
		cmbbx->SetCurSel(0);
	}
	if ((m_itype==-1)||(m_dtype==5))
	{
		GetDlgItem(IDC_COMLAB)->EnableWindow(0);
		cmbbx->EnableWindow(0);
	}
	CComboBox *cmbbx2;
	cmbbx2 = (CComboBox*)GetDlgItem(IDC_COMBO2);
	cont = cmbbx2->GetCount();
	for (j = 0; j<cont; j++)
	{
		cmbbx2->DeleteString(0);
	}
	int input = 0;
	if ((m_itype==3)||(m_itype==11)||(m_itype==16))
/*
......EDIT
*/
	{
		if (m_input==FORM_STRING)
			input = 0;
		if (m_input==FORM_PICK)
			input = 1;
		if (m_input==FORM_LOCATE)
			input = 2;
		if (m_input==FORM_LABEL)
			input = 3;
		if (m_input==FORM_SUBSCR)
			input = 4;
		cmbbx2->AddString("By Text");
		cmbbx2->AddString("By Pick");
		cmbbx2->AddString("By Location");
		cmbbx2->AddString("Label Only");
		cmbbx2->AddString("Subscript Only");
		GetDlgItem(IDC_COMLAB2)->SetWindowText("Input:");
		if (m_type==UD_DASSTRING)
		{
			if (m_dtype!=3)
			{
				cmbbx2->EnableWindow(1);
				GetDlgItem(IDC_COMLAB2)->EnableWindow(1);
			}
			else
			{
				cmbbx2->EnableWindow(0);
				GetDlgItem(IDC_COMLAB2)->EnableWindow(0);
			}
		}
		else
		{
			input = 0;
			cmbbx2->EnableWindow(0);
			GetDlgItem(IDC_COMLAB2)->EnableWindow(0);
		}
	}
	else if ((m_itype==5)||(m_itype==8)||(m_itype==9)||(m_itype==15)
			||(m_itype==17)||(m_itype==19))
	{
		if (m_input==FORM_STRING)
			input = 0;
		if (m_input==FORM_RECORD)
			input = 1;
		cmbbx2->AddString("FORM_STRING");
		cmbbx2->AddString("FORM_RECORD");
		cmbbx2->EnableWindow(1);
		GetDlgItem(IDC_COMLAB2)->SetWindowText("Input:");
		GetDlgItem(IDC_COMLAB2)->EnableWindow(1);
	}
	else if (m_itype==23)
	{
		cmbbx2->AddString("FORM_PICK");
		cmbbx2->EnableWindow(0);
		GetDlgItem(IDC_COMLAB2)->SetWindowText("Input:");
		GetDlgItem(IDC_COMLAB2)->EnableWindow(0);
	}
	else if (m_itype==24)
	{
		cmbbx2->AddString("Horizonal");
		cmbbx2->AddString("Vertical");
		cmbbx2->EnableWindow(1);
		GetDlgItem(IDC_COMLAB2)->SetWindowText("Justified");
		GetDlgItem(IDC_COMLAB2)->EnableWindow(1);
	}
	else
	{
		if (m_macro_flag==0)
			cmbbx2->AddString("FORM_STRING");
		else
			cmbbx2->AddString("By Text");
		cmbbx2->EnableWindow(0);
		GetDlgItem(IDC_COMLAB2)->SetWindowText("Input:");
		GetDlgItem(IDC_COMLAB2)->EnableWindow(0);
	}
	if (m_itype!=24)
		cmbbx2->SetCurSel(input);
	else
		cmbbx2->SetCurSel(m_justified);

	if ((m_itype==-1)||(m_dtype==5))
	{
		GetDlgItem(IDC_COMLAB2)->SetWindowText("Input:");
		GetDlgItem(IDC_COMLAB2)->EnableWindow(0);
		cmbbx2->EnableWindow(0);
	}
	if ((m_itype==-1)||(m_dtype==3)||(m_dtype==5))
	{
		GetDlgItem(IDC_EDIT4)->SetWindowText("");
		GetDlgItem(IDC_EDIT5)->SetWindowText("");
		GetDlgItem(IDC_EDIT6)->SetWindowText("");
		GetDlgItem(IDC_EDIT7)->SetWindowText("");
		GetDlgItem(IDC_EDIT8)->SetWindowText("");
		GetDlgItem(IDC_EDIT4)->EnableWindow(0);
		GetDlgItem(IDC_EDIT5)->EnableWindow(0);
		GetDlgItem(IDC_EDIT6)->EnableWindow(0);
		GetDlgItem(IDC_EDIT7)->EnableWindow(0);
		GetDlgItem(IDC_EDIT8)->EnableWindow(0);
		m_button[0].set_color(::GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
		m_button[0].Invalidate();
		m_button[0].UpdateWindow();
		m_button[1].set_color(::GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
		m_button[1].Invalidate();
		m_button[1].UpdateWindow();
		m_button[0].EnableWindow(0);
		m_button[1].EnableWindow(0);
		GetDlgItem(IDC_LIMITLAB)->EnableWindow(0);
		if (m_itype==25)
		{
			GetDlgItem(IDC_EDIT5)->EnableWindow(1);
			GetDlgItem(IDC_CHOICELAB)->EnableWindow(1);
			GetDlgItem(IDC_CHOICELAB)->SetWindowText("Video File:");
			GetDlgItem(IDC_EDIT5)->SetWindowText(m_choices);
		}
		GetDlgItem(IDC_LENLAB)->EnableWindow(0);
		GetDlgItem(IDC_PRELAB)->EnableWindow(0);
		GetDlgItem(IDC_RANGELAB)->EnableWindow(0);
		GetDlgItem(IDC_COLORLAB)->EnableWindow(0);
		if (m_dtype==5)
		{
/*
.....enable field color
*/
			GetDlgItem(IDC_COLORLAB)->EnableWindow(1);
			m_button[0].EnableWindow(1);
			m_button[1].EnableWindow(1);
			if (m_ffg<0)
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE); 
			}
			else
			{
				bcolor = RGB(uw_color_table[m_ffg][0], 
							uw_color_table[m_ffg][1], 
							uw_color_table[m_ffg][2]);
			}
			m_button[0].set_color(bcolor, RGB(0,0,0));
			m_button[0].Invalidate();
			m_button[0].UpdateWindow();
			if (m_fbg<0)
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE); 
			}
			else
			{
				bcolor = RGB(uw_color_table[m_fbg][0], 
							uw_color_table[m_fbg][1], 
							uw_color_table[m_fbg][2]);
			}
			m_button[1].set_color(bcolor, RGB(0,0,0));
			m_button[1].Invalidate();
			m_button[1].UpdateWindow();
		}
	}
	else
	{
		if ((m_itype==3)||(m_itype==11)||(m_itype==16))
		{
			if (m_input==FORM_STRING)
			{
				GetDlgItem(IDC_LIMITLAB)->EnableWindow(0);
				GetDlgItem(IDC_EDIT4)->EnableWindow(0);
			}
			else
			{
				GetDlgItem(IDC_LIMITLAB)->EnableWindow(1);
				GetDlgItem(IDC_EDIT4)->EnableWindow(1);
				GetDlgItem(IDC_EDIT4)->SetWindowText(m_limit);
			}
		}
		else if (m_itype==23)
		{
			GetDlgItem(IDC_LIMITLAB)->EnableWindow(1);
			GetDlgItem(IDC_EDIT4)->EnableWindow(1);
			GetDlgItem(IDC_EDIT4)->SetWindowText(m_limit);
		}
		else
		{
			GetDlgItem(IDC_EDIT4)->EnableWindow(0);
			GetDlgItem(IDC_LIMITLAB)->EnableWindow(0);
		}
//why 18		if ((m_itype==2)||(m_itype==18))
		if (m_itype==2)
		{
			GetDlgItem(IDC_EDIT5)->EnableWindow(1);
			GetDlgItem(IDC_CHOICELAB)->EnableWindow(1);
			GetDlgItem(IDC_CHOICELAB)->SetWindowText("Choices:");
			GetDlgItem(IDC_EDIT5)->SetWindowText(m_choices);
		}
		else if (m_itype==25)
		{
			GetDlgItem(IDC_EDIT5)->EnableWindow(1);
			GetDlgItem(IDC_CHOICELAB)->EnableWindow(1);
			GetDlgItem(IDC_CHOICELAB)->SetWindowText("Video File:");
			GetDlgItem(IDC_EDIT5)->SetWindowText(m_choices);
		}
		else
		{
			GetDlgItem(IDC_CHOICELAB)->EnableWindow(0);
			GetDlgItem(IDC_EDIT5)->EnableWindow(0);
		}

		if ((m_itype==3)||(m_itype==11)||(m_itype==16))
		{
			if ((m_macro_flag!=0)&&(m_type!=UD_DASUNITLESS))
			{
				GetDlgItem(IDC_LENLAB)->EnableWindow(0);
				GetDlgItem(IDC_PRELAB)->EnableWindow(0);
				GetDlgItem(IDC_RANGELAB)->EnableWindow(0);
				GetDlgItem(IDC_EDIT6)->EnableWindow(0);
				GetDlgItem(IDC_EDIT7)->EnableWindow(0);
				GetDlgItem(IDC_EDIT8)->EnableWindow(0);
			}
			else
			{
				GetDlgItem(IDC_LENLAB)->EnableWindow(1);
				GetDlgItem(IDC_PRELAB)->EnableWindow(1);
				GetDlgItem(IDC_RANGELAB)->EnableWindow(1);
				GetDlgItem(IDC_EDIT6)->EnableWindow(1);
				GetDlgItem(IDC_EDIT7)->EnableWindow(1);
				GetDlgItem(IDC_EDIT8)->EnableWindow(1);
			}
			sprintf(tempstr, "%d", m_len);
			GetDlgItem(IDC_EDIT6)->SetWindowText(tempstr);
			sprintf(tempstr, "%d", m_prec);
			GetDlgItem(IDC_EDIT7)->SetWindowText(tempstr);
				
			if (m_range_flag==0)
				tempstr[0] = '\0';
			else
			{
				S_format_range(tempstr, m_range, m_type);
			}
			GetDlgItem(IDC_EDIT8)->SetWindowText(tempstr);
		}
		else if (m_itype==24)
		{
			GetDlgItem(IDC_RANGELAB)->EnableWindow(1);
			GetDlgItem(IDC_EDIT8)->EnableWindow(1);
			if (m_range_flag==0)
				tempstr[0] = '\0';
			else
			{
				S_format_range(tempstr, m_range, UD_DASINT);
			}
			GetDlgItem(IDC_EDIT8)->SetWindowText(tempstr);
		}
		else
		{
			GetDlgItem(IDC_EDIT6)->EnableWindow(0);
			GetDlgItem(IDC_EDIT7)->EnableWindow(0);
			GetDlgItem(IDC_EDIT8)->EnableWindow(0);
			GetDlgItem(IDC_LENLAB)->EnableWindow(0);
			GetDlgItem(IDC_PRELAB)->EnableWindow(0);
			GetDlgItem(IDC_RANGELAB)->EnableWindow(0);
		}
		if ((m_itype==1)||(m_itype==5)||(m_itype==7)||(m_itype==17)||(m_itype==19))
		{
			GetDlgItem(IDC_COLORLAB2)->EnableWindow(0);
			m_button[2].set_color(::GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
			m_button[2].Invalidate();
			m_button[2].UpdateWindow();
			m_button[3].set_color(::GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
			m_button[3].Invalidate();
			m_button[3].UpdateWindow();
			m_button[2].EnableWindow(0);
			m_button[3].EnableWindow(0);
		}
		else
		{
			GetDlgItem(IDC_COLORLAB2)->EnableWindow(1);
			m_button[2].EnableWindow(1);
			m_button[3].EnableWindow(1);
			if (m_pfg<0)
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE); 
			}
			else
			{
				bcolor = RGB(uw_color_table[m_pfg][0], 
							uw_color_table[m_pfg][1], 
							uw_color_table[m_pfg][2]);
			}
			m_button[2].set_color(bcolor, RGB(0,0,0));
			m_button[2].Invalidate();
			m_button[2].UpdateWindow();
			if (m_pbg<0)
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE); 
			}
			else
			{
				bcolor = RGB(uw_color_table[m_pbg][0], 
							uw_color_table[m_pbg][1], 
							uw_color_table[m_pbg][2]);
			}
			m_button[3].set_color(bcolor, RGB(0,0,0));
			m_button[3].Invalidate();
			m_button[3].UpdateWindow();
		}

		if ((m_itype==1)||(m_itype==5)||(m_itype==7)||(m_itype==18)
			||(m_itype==3)||(m_itype==11)||(m_itype==16))
		{
/*
......button, listbox, checkbox, color button,
*/
			GetDlgItem(IDC_COLORLAB)->EnableWindow(1);
			m_button[0].EnableWindow(1);
			if ((m_itype==3)||(m_itype==11)||(m_itype==16))
				m_button[1].EnableWindow(0);
			else
				m_button[1].EnableWindow(1);
			if (m_ffg<0)
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE); 
			}
			else
			{
				bcolor = RGB(uw_color_table[m_ffg][0], 
							uw_color_table[m_ffg][1], 
							uw_color_table[m_ffg][2]);
			}
			m_button[0].set_color(bcolor, RGB(0,0,0));
			m_button[0].Invalidate();
			m_button[0].UpdateWindow();
			if (m_fbg<0)
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE); 
			}
			else
			{
				bcolor = RGB(uw_color_table[m_fbg][0], 
							uw_color_table[m_fbg][1], 
							uw_color_table[m_fbg][2]);
			}
			m_button[1].set_color(bcolor, RGB(0,0,0));
			m_button[1].Invalidate();
			m_button[1].UpdateWindow();
		}
		else
		{
			GetDlgItem(IDC_COLORLAB)->EnableWindow(0);
			m_button[0].EnableWindow(0);
			m_button[1].EnableWindow(0);
			m_button[0].set_color(::GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
			m_button[0].Invalidate();
			m_button[0].UpdateWindow();
			m_button[1].set_color(::GetSysColor(COLOR_BTNFACE), RGB(0,0,0));
			m_button[1].Invalidate();
			m_button[1].UpdateWindow();
		}
	}
	if ((m_dtype==1)||(m_dtype==3))
	{
		GetDlgItem(IDC_COLORLAB2)->EnableWindow(1);
		m_button[2].EnableWindow(1);
		m_button[3].EnableWindow(1);
		if (m_pfg<0)
		{
			bcolor = ::GetSysColor(COLOR_BTNFACE); 
		}
		else
		{
			bcolor = RGB(uw_color_table[m_pfg][0], 
						uw_color_table[m_pfg][1], 
						uw_color_table[m_pfg][2]);
		}
		m_button[2].set_color(bcolor, RGB(0,0,0));
		m_button[2].Invalidate();
		m_button[2].UpdateWindow();
		if (m_pbg<0)
		{
			bcolor = ::GetSysColor(COLOR_BTNFACE); 
		}
		else
		{
			bcolor = RGB(uw_color_table[m_pbg][0], 
						uw_color_table[m_pbg][1], 
						uw_color_table[m_pbg][2]);
		}
		m_button[3].set_color(bcolor, RGB(0,0,0));
		m_button[3].Invalidate();
		m_button[3].UpdateWindow();
	}
	else if (m_dtype==2)
	{
		GetDlgItem(IDC_CHOICELAB)->EnableWindow(1);
		GetDlgItem(IDC_EDIT5)->EnableWindow(1);
		m_button[2].EnableWindow(0);
		m_button[3].EnableWindow(0);
		GetDlgItem(IDC_COLORLAB2)->EnableWindow(0);
	}
	if (m_dtype==1)
	{
		GetDlgItem(IDC_LABEL)->SetWindowText("Label:");
		GetDlgItem(IDC_COLORLAB2)->SetWindowText("Label Color (fg,bg)");
	}
	else if (m_dtype==2)
	{
		GetDlgItem(IDC_LABEL)->SetWindowText("Name:");
		GetDlgItem(IDC_CHOICELAB)->SetWindowText("Picture file name:");
		GetDlgItem(IDC_EDIT5)->SetWindowText(m_choices);
	}
	else
	{
		if (m_itype==25)
			GetDlgItem(IDC_LABEL)->SetWindowText("Image:");
		else
			GetDlgItem(IDC_LABEL)->SetWindowText("Label:");
		GetDlgItem(IDC_COLORLAB2)->SetWindowText("Prompt Color (fg,bg)");
		if (m_itype==25)
			GetDlgItem(IDC_CHOICELAB)->SetWindowText("Video File:");
		else
			GetDlgItem(IDC_CHOICELAB)->SetWindowText("Choices:");
	}
	sprintf(tempstr, "%3.3f", m_font);
	GetDlgItem(IDC_EDIT11)->SetWindowText(tempstr);
	if ((m_dtype==2)||(m_dtype==3))
	{
/*
.....there is no 'prompt' font setting, only for field font
.....also not for picture
*/
		GetDlgItem(IDC_EDIT11)->EnableWindow(0);
		GetDlgItem(IDC_FONTLAB)->EnableWindow(0);
	}
	else
	{
		GetDlgItem(IDC_EDIT11)->EnableWindow(1);
		GetDlgItem(IDC_FONTLAB)->EnableWindow(1);
	}
	CComboBox *cmbbx3;
	cmbbx3 = (CComboBox*)GetDlgItem(IDC_COMBO3);
	cont = cmbbx3->GetCount();
	for (j = 0; j<cont; j++)
	{
		cmbbx3->DeleteString(0);
	}
	cmbbx3->AddString("NO");
	cmbbx3->AddString("DEFAULT");
	cmbbx3->AddString("YES");
	cmbbx3->SetCurSel(m_active+1);
	if (m_dtype==5)
	{
		GetDlgItem(IDC_COMLAB3)->EnableWindow(0);
		cmbbx3->EnableWindow(0);
	}
	else
	{
		GetDlgItem(IDC_COMLAB3)->EnableWindow(1);
		cmbbx3->EnableWindow(1);
	}
	if (m_dtype!=2)
	{
		GetDlgItem(IDC_BUTTON1)->ShowWindow(0);
	}
	else
		GetDlgItem(IDC_BUTTON1)->ShowWindow(1);
/*
......input item field, for normal form, this will display
......the input item number, for MACRO form, this will display
......macro parameter name
*/
	if ((m_itype==-1)||(m_dtype==5)||(m_dtype==3))
	{
		if ((m_input_itemno>=0)&&(m_dtype!=5))
		{
			if (m_macro_flag==0)
				sprintf(tempstr, "%d", m_input_itemno);
			else
				strcpy(tempstr, ((CNCLFdsnFrame*)m_parent)->m_macro_parms[m_input_itemno]);
		}
		else
			tempstr[0] = '\0';
		GetDlgItem(IDC_EDIT12)->SetWindowText(tempstr);
		GetDlgItem(IDC_ITEM_NOLABLE)->EnableWindow(0);
		GetDlgItem(IDC_EDIT12)->EnableWindow(0);
	}
	else
	{
		if (m_input_itemno>=0)
		{
			if (m_macro_flag==0)
				sprintf(tempstr, "%d", m_input_itemno);
			else
				strcpy(tempstr, ((CNCLFdsnFrame*)m_parent)->m_macro_parms[m_input_itemno]);
		}
		else
			tempstr[0] = '\0';
		GetDlgItem(IDC_EDIT12)->SetWindowText(tempstr);
		GetDlgItem(IDC_ITEM_NOLABLE)->EnableWindow(1);
		GetDlgItem(IDC_EDIT12)->EnableWindow(1);
	}
	if (m_macro_flag==0)
	{
		GetDlgItem(IDC_PAGE_NOLABLE)->ShowWindow(SW_SHOW);
		GetDlgItem(IDC_EDIT13)->ShowWindow(SW_SHOW);
		if (m_secno>0)
		{
			sprintf(tempstr, "%d", m_page+1);
		}
		else
			tempstr[0] = '\0';
		GetDlgItem(IDC_EDIT13)->SetWindowText(tempstr);
		if (m_secno>0)
		{
			GetDlgItem(IDC_PAGE_NOLABLE)->EnableWindow(1);
			GetDlgItem(IDC_EDIT13)->EnableWindow(1);
		}
		else
		{
			GetDlgItem(IDC_PAGE_NOLABLE)->EnableWindow(0);
			GetDlgItem(IDC_EDIT13)->EnableWindow(0);
		}
	}
	else
	{
		GetDlgItem(IDC_PAGE_NOLABLE)->ShowWindow(SW_HIDE);
		GetDlgItem(IDC_EDIT13)->ShowWindow(SW_HIDE);
	}
	if ((m_itype!=13)&&(m_itype!=24)&&(m_dtype==4))
	{
		GetDlgItem(IDC_PIC_LNMLABLE)->EnableWindow(1);
		GetDlgItem(IDC_EDIT14)->EnableWindow(1);
		GetDlgItem(IDC_EDIT14)->SetWindowText(m_pic_label);
		GetDlgItem(IDC_PIC_LTOOLTIP)->EnableWindow(1);
		GetDlgItem(IDC_EDIT15)->EnableWindow(1);
		GetDlgItem(IDC_EDIT15)->SetWindowText(m_pic_tooltip);
		GetDlgItem(IDC_PIC_LPOS)->EnableWindow(1);
		GetDlgItem(IDC_EDIT16)->EnableWindow(1);
		if (m_pic_label=="")
			GetDlgItem(IDC_EDIT16)->SetWindowText("");
		else
		{
			sprintf(tempstr, "%f, %f, %f, %f", m_pic_rect[0], m_pic_rect[1], 
					m_pic_rect[2], m_pic_rect[3]);
			GetDlgItem(IDC_EDIT16)->SetWindowText(tempstr);
		}
		GetDlgItem(IDC_PIC_LPARMS)->EnableWindow(1);
		GetDlgItem(IDC_EDIT17)->EnableWindow(1);
		GetDlgItem(IDC_EDIT17)->SetWindowText(m_pic_params);

		GetDlgItem(IDC_PIC_INDEX)->EnableWindow(1);
		GetDlgItem(IDC_EDIT18)->EnableWindow(1);
		if (m_pic_act_area>=0)
			sprintf(tempstr, "%d", m_pic_act_area);
		else
			tempstr[0] = '\0';
		GetDlgItem(IDC_EDIT18)->SetWindowText(tempstr);
	}
	else
	{
		GetDlgItem(IDC_PIC_LNMLABLE)->EnableWindow(0);
		GetDlgItem(IDC_EDIT14)->EnableWindow(0);
		GetDlgItem(IDC_PIC_LTOOLTIP)->EnableWindow(0);
		GetDlgItem(IDC_EDIT15)->EnableWindow(0);
		GetDlgItem(IDC_PIC_LPOS)->EnableWindow(0);
		GetDlgItem(IDC_EDIT16)->EnableWindow(0);
		GetDlgItem(IDC_PIC_LPARMS)->EnableWindow(0);
		GetDlgItem(IDC_EDIT17)->EnableWindow(0);
		GetDlgItem(IDC_PIC_INDEX)->EnableWindow(0);
		GetDlgItem(IDC_EDIT18)->EnableWindow(0);
	}
	GetClientRect(&m_rect);
}
/***********************************************************************
c
c   FUNCTION: SaveAll()
c
c       Save all data from the window fields
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormPView::SaveAll()
{
	GetDlgItem(IDC_EDIT1)->GetWindowText(m_label);

	char tempstr[800];
	double rval[2];
	int inum;

	GetDlgItem(IDC_EDIT3)->GetWindowText(tempstr, 80);
	int status = ul_to_reals(rval, &inum, 2, tempstr);
	if (status==0)
	{
		m_size[0] = rval[0];
		m_size[1] = rval[1];
	}
	int type = ((CComboBox*)GetDlgItem(IDC_COMBO1))->GetCurSel();
	if (m_dtype==4)
	{
		if ((m_itype==3)||(m_itype==11)||(m_itype==16))
/*
......EDIT
*/
		{
			if (m_macro_flag)
			{
				if (type==0)
					m_type = UD_DASSTRING;
				else
					m_type = UD_DASUNITLESS;
			}
			else
			{
				if (type==0)
					m_type = UD_DASCART;
				if (type==1)
					m_type = UD_DASVAL;
				if (type==2)
					m_type = UD_DASDISTANCE;
				if (type==3)
					m_type = UD_DASINT;
				if (type==4)
					m_type = UD_DASVEC;
				if (type==5)
					m_type = UD_DASSTRING;
				if (type==6)
					m_type = UD_DASNDC;
				if (type==7)
					m_type = UD_DASANGLE;
				if (type==8)
					m_type = UD_DASUNITLESS;
				if (type==9)
					m_type = UD_DASPLANE;
				if (type==10)
					m_type = UD_DASSCALAR;
				if (type==11)
					m_type = UD_SCACART;
				if (type==12)
					m_type = UD_SCAVAL;
				if (type==13)
					m_type = UD_SCADISTANCE;
				if (type==14)
					m_type = UD_SCAINT;
				if (type==15)
					m_type = UD_SCAVEC;
				if (type==16)
					m_type = UD_SCANDC;
				if (type==17)
					m_type = UD_SCAANGLE;
				if (type==18)
					m_type = UD_SCAUNITLESS;
			}
		}
		else
		{
			if ((m_itype==1)&&(m_itype==7)&&(m_itype==24))
				m_type = UD_DASINT;
			else
				m_type = UD_DASSTRING;
		}
	}
	else
	{
		m_type = UD_DASSTRING;
	}
	type = ((CComboBox*)GetDlgItem(IDC_COMBO2))->GetCurSel();
	if (m_itype==24)
	{
		m_justified = type;
	}
	else if ((m_itype==3)||(m_itype==11)||(m_itype==16))
/*
......EDIT
*/
	{
		if (type==0)
			m_input = FORM_STRING;
		if (type==1)
			m_input = FORM_PICK;
		if (type==2)
			m_input = FORM_LOCATE;
		if (type==3)
			m_input = FORM_LABEL;
		if (type==4)
			m_input = FORM_SUBSCR;
		if (m_type!=UD_DASSTRING)
			m_input = FORM_STRING;
	}
	else if ((m_itype==5)||(m_itype==8)||(m_itype==9)||(m_itype==15)
			||(m_itype==17)||(m_itype==19))
	{
		if (type==0)
			m_input = FORM_STRING;
		if (type==1)
			m_input = FORM_RECORD;
	}
	else if (m_itype==23)
	{
		m_input = FORM_PICK;
	}
	else
	{
		m_input = FORM_STRING;
	}
	if (m_itype!=-1)
	{
		if ((m_itype==3)||(m_itype==11)||(m_itype==16)||(m_type==23))
		{
			if (m_input!=FORM_STRING)
			{
				GetDlgItem(IDC_EDIT4)->GetWindowText(m_limit);
			}
		}
		if (m_dtype==2)
		{
			GetDlgItem(IDC_EDIT5)->GetWindowText(m_choices);
		}
		status = 0;
//why 18		if ((m_itype==2)||(m_itype==18))
		if ((m_itype==2)||(m_itype==25))
		{
			if (m_macro_flag)
			{
				if (m_itype==2)
				{
					GetDlgItem(IDC_EDIT5)->GetWindowText(tempstr, 80);
					status = Scheck_choice_vocab(tempstr);
				}
				else
					GetDlgItem(IDC_EDIT5)->GetWindowText(m_choices);
			}
			if (status==0)
			{
				GetDlgItem(IDC_EDIT5)->GetWindowText(m_choices);
			}
		}
		if ((m_itype==3)||(m_itype==11)||(m_itype==16))
		{
			if (((m_macro_flag!=0)&&(m_type!=UD_DASUNITLESS))==0)
			{
				GetDlgItem(IDC_EDIT6)->GetWindowText(tempstr, 80);
				m_len = atoi(tempstr);
				GetDlgItem(IDC_EDIT7)->GetWindowText(tempstr, 80);
				m_prec = atoi (tempstr);
				GetDlgItem(IDC_EDIT8)->GetWindowText(tempstr, 80);
				int nc = strlen(tempstr);
				ul_strip_blanks(tempstr, &nc);
				if (nc==0)
				{
					m_range_flag = 0;
				}
				else
				{
					status = S_get_range(tempstr, m_range, m_type);
					if (status!=0)
					{
/*
......error message
*/
						MessageBox("The Range input is wrong!", "Input Error", MB_OK);
						GetDlgItem(IDC_EDIT8)->SetFocus();
					}
					m_range_flag = 1;
				}
			}
		}
		else if (m_itype==24)
		{
			GetDlgItem(IDC_EDIT8)->GetWindowText(tempstr, 80);
			int nc = strlen(tempstr);
			ul_strip_blanks(tempstr, &nc);
			if (nc==0)
			{
				m_range_flag = 0;
			}
			else
			{
				status = S_get_range(tempstr, m_range, m_type);
				if (status!=0)
				{
/*
......error message
*/
					MessageBox("The Range input is wrong!", "Input Error", MB_OK);
					GetDlgItem(IDC_EDIT8)->SetFocus();
				}
				m_range_flag = 1;
			}
		}
//		GetDlgItem(IDC_EDIT9)->GetWindowText(m_color);
	}
//	if (!((m_itype==1)||(m_itype==5)||(m_itype==7)||(m_itype==17)||(m_itype==19)))
//		GetDlgItem(IDC_EDIT10)->GetWindowText(m_pcolor);
	if ((m_dtype==1)||(m_dtype==3))
	{
//		GetDlgItem(IDC_EDIT10)->GetWindowText(m_pcolor);
		;
	}
	else if (m_dtype==2)
	{
		GetDlgItem(IDC_EDIT5)->GetWindowText(m_choices);
	}
	GetDlgItem(IDC_EDIT11)->GetWindowText(tempstr, 80);
	m_font = atof(tempstr);
	m_active = ((CComboBox*)GetDlgItem(IDC_COMBO3))->GetCurSel() - 1;
/*
.....added picture field
*/
	GetDlgItem(IDC_EDIT18)->GetWindowText(tempstr, 80);
	int indx = atoi(tempstr);
	if (indx<0)
	{
		return;
	}
	if (indx>m_picarea_no)
	{
/*
......error, indx excess the picture number
*/
		sprintf(tempstr, "You must enter a number between 0 to %d.", m_picarea_no);
		MessageBox(tempstr, "Input Error", MB_OK);
		GetDlgItem(IDC_EDIT8)->SetFocus();
		return;
	}
	GetDlgItem(IDC_EDIT14)->GetWindowText(tempstr, 80);
/*
.....remove trailing spaces
*/
	for (int i=strlen(tempstr); i>0; i--)
	{
		if (tempstr[i-1]==' ')
			tempstr[i-1] = '\0';
			else
				break;
	}
	m_pic_label = tempstr;
	if (m_pic_label.GetLength()==0)
		return;
/*
.....have a valid picture name, create a picture for this item
*/
	if (indx==m_picarea_no)
	{
/*
.....add the picture area
*/
		Add_picarea(tempstr);	
		m_pic_act_area = indx;
	}
	GetDlgItem(IDC_EDIT15)->GetWindowText(tempstr, 80);
	m_pic_tooltip = tempstr;
	int len;
	len = strlen(tempstr);
	if ((m_picarea[indx].tooltext==NULL)&&(len>0))
	{
		m_picarea[indx].tooltext = (char*) uu_malloc((100)*sizeof(char));
	}
	if (len>0)
		strcpy(m_picarea[indx].tooltext, tempstr);
	else if (m_picarea[indx].tooltext!=NULL)
		m_picarea[indx].tooltext[0] = '\0';

	GetDlgItem(IDC_EDIT16)->GetWindowText(tempstr, 80);
/*
.....in minx, miny, maxx, maxy format
*/
	m_pic_rect[0] = -1000;
	m_pic_rect[2] = -1000;
	m_pic_rect[1] = -1000;
	m_pic_rect[3] = -1000;
	char *tok;
	float pic_rect[4];
	if (strlen(tempstr)!=0)
	{
		tok = strtok(tempstr, " ,");
		if (tok!=NULL)
		{
			pic_rect[0] = atof(tok);
			tok = strtok(NULL, " ,");
			if (tok!=NULL)
			{
				pic_rect[1] = atof(tok);
				tok = strtok(NULL, " ,");
				if (tok!=NULL)
				{
					pic_rect[2] = atof(tok);
					tok = strtok(NULL, " ,");
					if (tok!=NULL)
					{
						pic_rect[3] = atof(tok);
					}
				}
			}
		}	
	}
	m_pic_rect[0] = pic_rect[0];
	m_pic_rect[1] = pic_rect[1];
	m_pic_rect[2] = pic_rect[2];
	m_pic_rect[3] = pic_rect[3];
	m_picarea[indx].xmin = pic_rect[0];
	m_picarea[indx].ymin = pic_rect[1];
	m_picarea[indx].xmax = pic_rect[2];
	m_picarea[indx].ymax = pic_rect[3];

	GetDlgItem(IDC_EDIT17)->GetWindowText(tempstr, 80);
	m_pic_params = tempstr;

	CString pic_params;
	if (m_itype==2)
	{
		S_convert_choice_params(m_pic_params, m_choices, 1, pic_params);
		len =strlen(pic_params);
		if ((m_picarea[indx].params==NULL)&&(len>0))
		{
			m_picarea[indx].params = (char*) uu_malloc((100)*sizeof(char));
		}
		if (len>0)
			strcpy(m_picarea[indx].params, pic_params);
		else
			m_picarea[indx].params[0] = '\0';
	}
	else
	{
		len = strlen(tempstr);
		if ((m_picarea[indx].params==NULL)&&(len>0))
		{
			m_picarea[indx].params = (char*) uu_malloc((100)*sizeof(char));
		}
		if (len>0)
			strcpy(m_picarea[indx].params, tempstr);
		else if (m_picarea[indx].params!=NULL)
			m_picarea[indx].params[0] = '\0';
	}
/*
.....save m_pic_act_area
*/
	if (m_reloading) 
		return;
	GetDlgItem(IDC_EDIT18)->GetWindowText(tempstr, 80);
	m_pic_act_area = atoi(tempstr);
}

void CNCLFormPView::OnSize(UINT nType, int cx, int cy) 
{
	CFormView::OnSize(nType, cx, cy);
}

void CNCLFormPView::OnDraw(CDC* pDC)
{
//	CNCLFormDoc* pDoc = GetDocument();
//	ASSERT_VALID(pDoc);
}

/**********************************************************************
**    I_FUNCTION : GetIColor(int color)
**       color button callback: diplay a color dialog and get a color
**    PARAMETERS  
**       INPUT  : fieldno
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int CNCLFormPView::GetIColor(int color)
{
	int rcolor;
	GdiplusStartupInput gdiplusStartupInput;
	ULONG_PTR gdiplusToken;
	char defstr[64];

	GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);

	CNCLColorDlg dlg;
	dlg.SetColorIndex(color);
	dlg.SetColorDefault(1, "Default Color:");
	int nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
/*
.....set the field color to select color
*/
		rcolor =  dlg.m_current_color;
	}
	else if (nResponse == IDCANCEL)
	{
		rcolor = color;
	}
	GdiplusShutdown(gdiplusToken);
	return rcolor;
}

void CNCLFormPView::Add_picarea(char *name)
{
	int k, indx = -1;
	if (m_picarea_no>0)
		indx = m_picarea_no;
	if (m_picarea_no==0)
		indx = 0;
	UD_PICAREA *picarea;
	int picarea_no = indx+1;
	if (picarea_no>0)
	{
		picarea = (UD_PICAREA *) uu_malloc(picarea_no*sizeof(UD_PICAREA));
	}
	for (k=0; k<m_picarea_no; k++)
	{
		if (m_picarea[k].params!=NULL)
		{
			picarea[k].params = (char*) uu_malloc((100)*sizeof(char));
			strcpy(picarea[k].params, m_picarea[k].params);
		}
		else
			picarea[k].params = NULL;

		if (m_picarea[k].tooltext!=NULL)
		{
			picarea[k].tooltext = (char*) uu_malloc((100)*sizeof(char));
			strcpy(picarea[k].tooltext, m_picarea[k].tooltext);
		}
		else
			picarea[k].tooltext = NULL;

		strcpy(picarea[k].name, m_picarea[k].name);
		picarea[k].xmin = m_picarea[k].xmin;
		picarea[k].ymin = m_picarea[k].ymin;
		picarea[k].xmax = m_picarea[k].xmax;
		picarea[k].ymax = m_picarea[k].ymax;
	}
	picarea[k].params = (char*) uu_malloc(100*sizeof(char));
	picarea[k].params[0] = '\0';
	picarea[k].tooltext = (char*) uu_malloc(100*sizeof(char));
	picarea[k].tooltext[0] = '\0';
	strcpy(picarea[k].name, name);
	picarea[k].xmin = 0;
	picarea[k].ymin = 0;
	picarea[k].xmax = 20;
	picarea[k].ymax = 20;
	m_pic_act_area = k;
/*
.....delete old picture data
*/
	if ((m_picarea_no>0)&&(m_picarea!=NULL))
	{
		for (k=0; k<m_picarea_no; k++)
		{
			if (m_picarea[k].params!=NULL)
				uu_free(m_picarea[k].params);
			if (m_picarea[k].tooltext!=NULL)
				uu_free(m_picarea[k].tooltext);
			m_picarea[k].params = NULL;
			m_picarea[k].tooltext = NULL;
		}
		uu_free((char*)m_picarea);
		m_picarea = NULL;
	}
	m_picarea_no = picarea_no;
	m_picarea = picarea;
}

#ifdef _DEBUG
void CNCLFormPView::AssertValid() const
{
	CFormView::AssertValid();
}


CNCLFormDoc* CNCLFormPView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CNCLFormDoc)));
	return (CNCLFormDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CNCLFormPView message handlers
