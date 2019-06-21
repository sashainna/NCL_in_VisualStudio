#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**
**   FILE NAME: wsntformbar.cpp
** 
**	 Description - Functions and implementations for
**		CNCLFormBarBar class (NCL forms) and some form functions
**
**	 CONTAINS:
**		class functions of CNCLFormBarBar class
**
**    COPYRIGHT 2005 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntformbar.cpp , 25.12
**    DATE AND TIME OF LAST  MODIFICATION
**			01/20/17 , 12:17:38
***********************************************************************
*/
#include "wsntstdafx.h"
#include <math.h>
#include "xenv1.h"
#include "xfsys1.h"
#include "ddef.h"
#include "udfmracs.h"
#include "dinput.h"
#include "mpocket.h"
#include "wsntctl.h"
#include "wsntdoc.h"
#include "wsntview.h"
#include "wsntframe.h"
#include "wsntformbar.h"
#include "wsntform.h"
#include "wsntcfunc.h"
#include "xenv1.h"
#include "wsgl.h"
#include "ddef.h"
#include "udfmracs.h"
#include "dinput.h"
#include "lcom.h"
#include "xenv1.h"
#include "wsntres.h"
#include "wsntpktwin.h"
#include "lcom.h"
#include "wsntglfunc.h"
#include "wsglfun.h"
#include "dselect.h"
#include "mdcpln.h"
#include "wsntclrdlg.h"
#include "dselect.h"
#include "lipv.h"

#define FORMTOPX	5
#define FORMTOPY	5
#define FORM_STRING		1
#define FORM_PICK		2
#define FORM_LOCATE		3
#define FORM_RECORD		4
#define FORM_LABEL		5
#define FORM_SUBSCR		6

#define UD_FORMFUZZ (UU_REAL) 1.0e-6
#define TEXT_WID	4

extern "C" UD_FSTRUCT *UD_dispfrm[60];

extern "C" int ud_set_traverse_mask(int fieldno, int val);
extern "C"  int UW_auto_cursor;
extern "C" int uw_ntset_curpos(int x, int y);
//extern "C" int uw_ntsetcursor(int cursor);
extern "C" int uw_ntgetcur_cursor();
extern char UW_formdlg_font[];
extern "C" int ug_save_input(int** save_inptr_pt);
extern "C" int ug_reset_input(int* save_inptr);
extern "C" int ud_setpick_type(int);
extern "C" int ud_getpick_type();
extern "C" char UW_form_font[20];

extern CMainFrame *NCL_MainFrame;
extern CDialog *Pocket_Win[UM_MAX_POCKET];
extern CWnd *NCL_Main_View;

int Arrow_Len = 8;
double Arrow_Angle = PI/8;
extern "C" UD_FSTRUCT *UD_frm;
extern "C" UD_FDATA *UD_fdata;
extern "C" ACCEL *UZ_ncl_accel;
extern "C" int UZ_ncl_accelnum;
extern "C" ACCEL UZ_form_hotkey[3];

extern "C" UD_FSTRUCT *UD_dispfrm[60];
extern "C" UD_FDATA *UD_dispfdata[60];

extern "C" float uw_form_scalex;
extern "C" float uw_form_scaley;
extern "C" int UD_form_bypick;
extern "C"  int UW_auto_cursor;
extern "C" int uw_ntclose_dispfrm(int frmid);
extern void uw_ntget_menurect(int bar_pos, CRect *rect, int dockable);
extern void uw_nthide_bars(int bar_pos);

extern CWnd *UW_display_frmwin[60];
extern CNCLForm *UW_display_frm[60];
extern int UW_reset_menu;
extern "C" int ud_get_areadockable(char *area_name, int *dockable);
extern int uw_get_rgb (char *color_str, COLORREF &color) ;
extern "C" int uz_ntparse_frmkey(char *buf, int numint, BYTE* ktyp,WORD* ksub);
extern "C" int uw_ntdispmsg (char *msg);
extern "C" int ncl_parse_scalar_values(char *, char*, int);
extern "C" UD_FSTAT ud_scatodas(UD_DASDATA *dsda, UD_DASIN *de, int dtyp);
extern "C" int ud_updatews(int);

static int current_chk_field = -1;
extern int Focused_frmid;
extern int Focused_frm_fldno;
extern "C" void	uw_ntreset_redraw();
extern "C" void ncl_reset_pikgeom();

#define stricmp _stricmp
extern int CALLBACK SortFunc(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort);
extern "C" void ud_free_itemdata(UD_ITEMDATA *data);
extern "C" void uw_ntget_ecurpos(int *start, int *end);
extern "C"  int ud_get_pick_selstr(char *prompt, char *lstr, int flag);
extern "C" void ud_lgeo(int flag, int *bitary);

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
IMPLEMENT_DYNAMIC(CNCLFormBar,CNCLDialogBar)

BEGIN_MESSAGE_MAP(CNCLFormBar, CNCLDialogBar)
	//{{AFX_MSG_MAP(CNCLFormBar)
	ON_MESSAGE(WM_INITDIALOG, HandleInitDialog)
	ON_WM_PAINT()
	ON_WM_CLOSE()
	ON_WM_CTLCOLOR()
	ON_WM_VSCROLL()
	ON_WM_HSCROLL()
	ON_COMMAND(ID_FORM_HIGHLIGHT, OnFormTextHighlight)
	ON_COMMAND(ID_FORM_TABED, OnFormTabbed)
	ON_COMMAND(ID_FORM_STABED, OnFormSTabbed)
	ON_COMMAND(IDC_FORMACCEPT, FormAccept)
	ON_COMMAND(IDC_FORMCANCEL, OnClose)
	ON_COMMAND(IDC_FORMHELP, FormHelp)
	ON_COMMAND(IDF_FORM_RETURN, FormUserCallbacks6)
	ON_COMMAND_RANGE(ID_FORM_HOTKEY1, ID_FORM_HOTKEY4, OnAccelFunctions)
	ON_COMMAND_RANGE(IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks1)
	ON_CONTROL_RANGE(CBN_SELCHANGE, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks3)
	ON_CONTROL_RANGE(LBN_SELCHANGE, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks2)
	ON_CONTROL_RANGE(EN_SETFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks5)
	ON_CONTROL_RANGE(EN_KILLFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks4)
	ON_CONTROL_RANGE(CBN_SETFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks7)
	ON_CONTROL_RANGE(LBN_SETFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks7)
	ON_CONTROL_RANGE(BN_SETFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks7)
	ON_NOTIFY_RANGE(NM_SETFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks12)
	ON_NOTIFY_RANGE(LVN_COLUMNCLICK, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks10)
	ON_NOTIFY_RANGE(LVN_ITEMCHANGED, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks11)
	ON_MESSAGE(ID_LISTITEM_CLICK, OnItemClick)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/***********************************************************************
c
c   SUBROUTINE:  NCL_form_SaveData
c
c   FUNCTION:  This function save every field data 
c				in the form
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
int CNCLFormBar::NCL_form_SaveData()
{
	int i,j,k,status, flag,len;
	char num[20],buf[20], num2[20];
	CButton* cbut;
	CComboBox* cmbbx;
	CListBox* listbx;
	CNCLListCtrl* listctl;
	CNCLListCtrl2* listctl2;
	CEdit* cedt;
	UD_LIST *list;
	UD_TLIST *tlist;
	UD_DLIST *dlist;
	POSITION pos;
	char string [UX_MAX_PATH_LEN];

	status = 1;
	ud_rpwrform("START", NULL, NULL);
	for (i=0; i<m_fldno; i++)
	{
		UINT fid =  m_idtable[i+m_dspno];
		switch(m_type[i])
		{
/*
.....SIMPLE and DROPDOWN, can be edited, get
.....answer from edit box of this COMB
*/
		case 8:
			cmbbx = (CComboBox*)GetDlgItem(fid);
			if (!((cmbbx->IsWindowEnabled()) && (cmbbx->IsWindowVisible())))
				break;
			cmbbx->GetWindowText(string, UX_MAX_PATH_LEN-1);
			len = strlen(string);
/*
......copy list to answer
*/
			list = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			if (list->answer!=NULL)
				strcpy_s(list->answer, len+1, string);
			if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				sprintf_s(num,sizeof(num),"%d", i);
				ud_rpwrform("DATA", num, string);
			}
			break;
		case 9:
			break;
/*
.....COMBBOX, CHOICES, can't be edited.
*/
		case 2:
			cmbbx = (CComboBox*)GetDlgItem(fid);
			if (!((cmbbx->IsWindowEnabled()) && (cmbbx->IsWindowVisible())))
				break;
			*((m_fData.ud_data[i]).ud_delem.frmint) = cmbbx->GetCurSel();
			if ((m_init_data_type_flag)&&(m_data_type[i]==UD_DASSTRING))
			{
				cmbbx->GetLBText(*((m_fData.ud_data[i]).ud_delem.frmint), string) ;
				len = strlen(string);
				strcpy_s(m_fData.ud_data[i].ud_delem.frmstr, len+1, string);
			}
			if (UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				if ((m_init_data_type_flag)&&(m_data_type[i]==UD_DASSTRING))
				{
					strcpy(buf, m_fData.ud_data[i].ud_delem.frmstr);
				}
				else
				{
					sprintf_s(num,sizeof(num),"%d", i);
					sprintf_s(buf,sizeof(buf),"%d",m_fData.ud_data[i].ud_delem.frmint[0]);
				}
				ud_rpwrform("DATA",num,buf);
			}
			break;
/*
......LIST BOX
*/
		case 5:
			listbx = (CListBox*)GetDlgItem(fid);
			if (!((listbx->IsWindowEnabled()) && (listbx->IsWindowVisible())))
				break;
			listbx->GetWindowText(string, UX_MAX_PATH_LEN-1);
			len = strlen(string);
			list = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			if (list->answer!=NULL)
				strcpy_s(list->answer, len+1, string);
			if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				sprintf_s(num,sizeof(num),"%d", i);
				ud_rpwrform("DATA", num, string);
			}
			break;
/*
.....EDIT
*/
		case 3:
		case 11:
		case 16:
			cedt = (CEdit*)GetDlgItem(fid);
			if (!((cedt->IsWindowEnabled()) && (cedt->IsWindowVisible())))
				break;
			cedt->GetWindowText(string, UX_MAX_PATH_LEN-1);
			status = form_ckdata(i,string, &flag);
			if (status==UU_FALSE)
				goto done;
			if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				sprintf_s(num,sizeof(num),"%d", i);
				ud_rpwrform("DATA", num, string);
			}
			break;
/*
.....CHECKBOX
*/
		case 7:
			cbut = (CButton*)GetDlgItem(fid);
			if (!((cbut->IsWindowEnabled()) && (cbut->IsWindowVisible())))
				break;
			*((m_fData.ud_data[i]).ud_delem.frmint) = cbut->GetCheck();
			if (UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				sprintf_s(num,sizeof(num),"%d", i);
				sprintf_s(buf,sizeof(buf),"%d",m_fData.ud_data[i].ud_delem.frmint[0]);
				ud_rpwrform("DATA",num,buf);
			}
			break;
/*
......choice list
*/
		case 15:
			cmbbx = (CComboBox*)GetDlgItem(fid);
			if (!((cmbbx->IsWindowEnabled()) && (cmbbx->IsWindowVisible())))
				break;
			cmbbx->GetWindowText(string, UX_MAX_PATH_LEN-1);
			len = strlen(string);
			list = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			if (list->answer!=NULL)
				strcpy_s(list->answer, len+1, string);
			if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				sprintf_s(num,sizeof(num),"%d", i);
				ud_rpwrform("DATA", num, string);
			}
			break;
/*
......LIST TABLE
*/
		case 17:
			listctl = (CNCLListCtrl*)GetDlgItem(fid);
			if (!((listctl->IsWindowEnabled()) && (listctl->IsWindowVisible())))
				break;
			tlist = (UD_TLIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			pos = listctl->GetFirstSelectedItemPosition();
			if (pos!=NULL)
				tlist->answer = listctl->GetNextSelectedItem(pos);
			else
				tlist->answer = -1;
/*
...added to record form data
...Yurong
*/
			if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				sprintf_s(num, sizeof(num),"%d", i);
				sprintf_s(buf, sizeof(buf),"%d", tlist->answer);
				ud_rpwrform("DATA", num, buf);
/*
.....we need save the list item text too because the whole list could be used as output (answer)
.....for example, when we do layer transfer (nclu_put_layer), if we don't save it, the playback 
.....don't know what those output list are
*/
				if (m_input[i]==FORM_RECORD)
				{
					sprintf_s(num2,sizeof(num2),"%d", tlist->num_col);
					ud_rpwrform("TLISTC", "START", num2);
					for (j=0; j<tlist->num_col; j++)
					{
						ud_rpwrform("TLISTC", num, tlist->col_label[j]);
					}
					sprintf_s(num2,sizeof(num2),"%d", list->num_item);
					ud_rpwrform("TLIST", "START", num2);
					for (j=0; j<tlist->num_item; j++)
					{
						sprintf_s(num2,sizeof(num2),"%d", tlist->data[j].itemnum);
						for (k=0;k<tlist->data[j].itemnum;k++)
							ud_rpwrform("TLIST", num, tlist->data[j].data_items[k]);
					}
					ud_rpwrform("TLIST", "DONE", num);
				}
			}
			break;
/*
......LIST TABLE
*/
		case 19:
			listctl2 = (CNCLListCtrl2*)GetDlgItem(fid);
			if (!((listctl2->IsWindowEnabled()) && (listctl2->IsWindowVisible())))
				break;
			dlist = (UD_DLIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			listctl2->GetSelectedItem(&(dlist->answer[0]), &(dlist->answer[1]));
			if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				sprintf_s(num,sizeof(num), "%d", i);
				sprintf_s(buf,sizeof(buf), "%d %d", dlist->answer[0], dlist->answer[1]);
				ud_rpwrform("DATA", num, buf);
/*
.....we need save the list item text too because the whole list could be used as output (answer)
.....for example, when we do layer transfer (nclu_put_layer), if we don't save it, the playback 
.....don't know what those output list are
*/
				if (m_input[i]==FORM_RECORD)
				{
					sprintf_s(num2,sizeof(num2),"%d", dlist->num_col);
					ud_rpwrform("DLIST", "STARTC", num2);
					for (j=0; j<dlist->num_col; j++)
					{
						sprintf_s(num2,sizeof(num),"%d", j);
						ud_rpwrform("DLIST", num2, dlist->col_label[j]);
					}
					sprintf_s(num2,sizeof(num2),"%d", list->num_item);
					ud_rpwrform("DLIST", "START", num2);
					for (j=0; j<dlist->num_item; j++)
					{
						for (k=0;k<dlist->data[j].itemnum;k++)
						{
							sprintf_s(num2,sizeof(num2),"%d", k);
							ud_rpwrform("DLIST", num2, dlist->data[j].data_items[k]);
						}
					}
					ud_rpwrform("DLIST", "DONE", num);
				}
			}
			break;
		}
	}
done:;
	if (i<m_fldno)
	{
		CWnd *win = GetDlgItem(m_idtable[i+m_dspno]);
		if (win==NULL)
			return status;
		win->SetFocus();
	}
	return status;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntform_redisplay()
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
int CNCLFormBar::uw_ntform_redisplay()
{
	int i;
	char ldat[UX_MAX_PATH_LEN];
	CButton* cbut;
	CComboBox* cmbbx;
	CEdit* cedt;
	char perc_str[10];
	CProgressCtrl* cprocess;
	int defchc, perc;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;

	for (i=0;i<m_fStruct.n_input_fields;i++)
	{
/*
......update prompt label
*/
		if (m_fStruct.input_flds[i].ud_echo==2)
		{
			reset_fldlabel(i, m_fStruct.input_flds[i].prompt);
			m_fStruct.input_flds[i].ud_echo = 0;
		}
		if (m_fStruct.traverse_mask[i] == 0)
			set_traverse_mask(i, 0);
		else
			set_traverse_mask(i, 1);
		if (m_fStruct.ud_display_mask[i+m_dspno] == 1)
			set_display_mask(i+m_dspno, 1);
		if (m_fStruct.ud_display_mask[i+m_dspno] == 0 )
			set_display_mask(i+m_dspno, 0);
	
		UINT fid =  m_idtable[i+m_dspno];
		switch(m_type[i])
		{
/*
.....CHOICES
*/
		case 2:
			cmbbx = (CComboBox*)GetDlgItem(fid);
			defchc = m_fData.ud_data[i].ud_delem.frmint[0];
			cmbbx->SetCurSel(defchc);
			break;
/*
.....EDIT
*/
		case 3:
		case 11:
		case 16:
			if (m_fData.ud_data[i].dflg!=2)
				break;
			m_fData.ud_data[i].dflg = 1;
			cedt = (CEdit*)GetDlgItem(fid);
			if (m_init_data_type_flag)
			{
				ud_formstr_format(m_data_type[i],
					m_fData.ud_data[i].ud_delem,m_fStruct.input_flds[i].ud_fprec,
					m_fStruct.input_flds[i].ud_flen,ldat);
			}
			else
				ud_formstr_format(m_fStruct.input_flds[i].ud_datatyp,
					m_fData.ud_data[i].ud_delem,m_fStruct.input_flds[i].ud_fprec,
					m_fStruct.input_flds[i].ud_flen,ldat);
			m_fData.ud_data[i].dflg = 1;
			cedt->SetWindowText(ldat);
			break;
/*
.....CHECKBOX
*/
		case 7:
			cbut = (CButton*)GetDlgItem(fid);
			defchc = m_fData.ud_data[i].ud_delem.frmint[0];
			cbut->SetCheck(defchc);
			break;
		case 10:
/*
......picture area
*/
			if (m_fData.ud_data[i].dflg==2)
			{
				m_fData.ud_data[i].dflg = 1;
				OnPaint();
			}
		case 13:
			cprocess = (CProgressCtrl*)GetDlgItem(fid);
			perc = m_fData.ud_data[i].ud_delem.frmint[0];
			cprocess->SetPos(perc);
			if (m_fStruct.input_flds[i].ud_datatyp==UD_DASINT)
			{
				fid =  m_idtable[i+m_dspno+m_fldno+m_framno+m_picno];
				sprintf_s(perc_str, sizeof(perc_str),"%d%%", perc);
				GetDlgItem(fid)->SetWindowText(perc_str);
			}
			break;
		}
	}

	for (i=0;i<m_fStruct.n_display_fields;i++)
	{
		if (m_fStruct.ud_display_mask[i] == 1)
			set_display_mask(i, 1);
		if (m_fStruct.ud_display_mask[i] == 0 )
			set_display_mask(i, 0);
	}
	for (i=0;i<m_fStruct.n_picture_fields;i++)
	{
		if (m_fStruct.picture_flds[i].upt_flag)
		{
			Reset_picture_file(i, m_fStruct.picture_flds[i].fname);
		}
		m_fStruct.picture_flds[i].upt_flag = 0;
	}
	return 0;
}

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
LRESULT CNCLFormBar::HandleInitDialog(WPARAM wparm, LPARAM lparm)
{
	int i,j,cont,k,m,len;
	CButton* cbut;
	CComboBox* cmbbx;
	CListBox* listbx;
	CEdit* cedt;
	CWnd *cwin;
	char perc_str[10];
	CProgressCtrl *cprocess;
	int *defchc,defsel, perc;
	UD_LIST *list;
	char string[UX_MAX_PATH_LEN];
	CNCLListCtrl *listctl;
	UD_TLIST *tlist;
	CNCLListCtrl2 *listctl2;
	UD_DLIST *dlist;
	CRect rect;
	int wid;
	int pwid;
	CClientDC *list_dc;
	CSize sizeText;

	m_init = 0;
	CNCLDialogBar::HandleInitDialog(wparm, lparm);
/*
.....adding function to adject form's /COLOR/fg, bg, 
...../FONT/scale and /JUSTIFY/ vlaue
*/
	InitAdjust();
/*
.....initialize form data from input field #1 to field #m_fldno
*/
	for (i=0; i<m_fldno; i++)
	{
		UINT fid =  m_idtable[i+m_dspno];
		switch(m_type[i])
		{
/*
.....COMBBOX, SIMPLE and DROPDOWN, can be edited
*/
		case 8:
		case 9:
			cmbbx = (CComboBox*)GetDlgItem(fid);
			list = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			defsel = -1;
			wid = 0;
			list_dc = new CClientDC(cmbbx);
			for (j=0; j<list->num_item; j++)
			{
				cmbbx->AddString(list->item[j]);
				if (strcmp(list->item[j], list->answer)==0)
					defsel = j;
				sizeText = list_dc->GetTextExtent(list->item[j], strlen(list->item[j]));
				pwid = sizeText.cx;
				if (pwid>wid) wid = pwid;
			}
			if (list->num_item==0)
				cmbbx->AddString(" ");
			if (defsel!=-1)
				cmbbx->SetCurSel(defsel);
/*
......set the horizen text extent to longest text line
*/
			if (m_fStruct.input_flds[i].ud_flen==-1)
				cmbbx->SetHorizontalExtent((UINT)(wid*0.95));
			else
				cmbbx->SetHorizontalExtent(0);
			delete list_dc;
			break;
/*
.....COMBBOX, CHOICES, can't be edited.
*/
		case 2:
			cmbbx = (CComboBox*)GetDlgItem(fid);
			for (j=0; j<m_formchcnum[i]; j++)
				cmbbx->AddString(m_formchoice[i][j]);
					
			
			if ((m_init_data_type_flag)&&(m_data_type[i]==UD_DASSTRING))
			{
/*
........the default is string value
*/
				defsel = 0;
				for (j=0;j<m_formchcnum[i];j++)
				{
					if (strcmp(m_formchoice[i][j], (m_fData.ud_data[i]).ud_delem.frmstr)==0) 
						defsel = j;
				}
				cmbbx->SetCurSel(defsel);
				m_fData.ud_data[i].ud_delem.frmint[0] = defsel;
			}
			else
			{
				if ((m_fData.ud_data[i].dflg==0)||((m_fData.ud_data[i]).ud_delem.frmint == NULL))
					cmbbx->SetCurSel(0);
				else
				{
					defchc = (m_fData.ud_data[i]).ud_delem.frmint;
					cmbbx->SetCurSel(*defchc);
				}
			}			
			break;
/*
......LIST BOX
*/
		case 5:
			listbx = (CListBox*)GetDlgItem(fid);
			list = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
/*
.....we need clear listbox first
*/
			cont = listbx->GetCount();
			for (j = 0; j<cont; j++)
			{
				listbx->DeleteString(0);
			}
			defsel = -1;
			wid = 0;
			list_dc = new CClientDC(listbx);
			for (j=0; j<list->num_item; j++)
			{
				listbx->AddString(list->item[j]);
				if (strcmp(list->item[j], list->answer)==0)
					defsel = j;
				sizeText = list_dc->GetTextExtent(list->item[j], strlen(list->item[j]));
				pwid = sizeText.cx;
				if (pwid>wid) wid = pwid;
			}
			if (list->num_item==0)
				listbx->AddString(" ");
			if (defsel!=-1)
			{
				listbx->SetCurSel(defsel);
			}
/*
......set the horizen text extent to longest text line
*/
			if (m_fStruct.input_flds[i].ud_flen==-1)
				listbx->SetHorizontalExtent((UINT)(wid*0.95));
			else
				listbx->SetHorizontalExtent(0);
			delete list_dc;
			break;
/*
.....EDIT
*/
		case 3:
		case 11:
		case 16:
			if (m_fData.ud_data[i].dflg==0)
				continue;			
			cedt = (CEdit*)GetDlgItem(fid);
			if (m_init_data_type_flag)
			{
				ud_formstr_format(m_data_type[i],
					m_fData.ud_data[i].ud_delem,m_fStruct.input_flds[i].ud_fprec,
					m_fStruct.input_flds[i].ud_flen,string);
			}
			else
				ud_formstr_format(m_fStruct.input_flds[i].ud_datatyp,
					m_fData.ud_data[i].ud_delem,m_fStruct.input_flds[i].ud_fprec,
					m_fStruct.input_flds[i].ud_flen,string);
			if (m_type[i]==16)
/*
......limit text input to m_fStruct.input_flds[i].ud_flen
*/
			{
				len = m_fStruct.input_flds[i].ud_flen;
				if (len<0) len= -len;
				cedt->LimitText(len);
			}
			cedt->SetWindowText(string);
			break;
/*
.....CHECKBOX
*/
		case 7:
			cbut = (CButton*)GetDlgItem(fid);
			if ((m_fData.ud_data[i].dflg==0)||((m_fData.ud_data[i]).ud_delem.frmint == NULL))
				cbut->SetCheck(0);
			else
			{
				defchc = (m_fData.ud_data[i]).ud_delem.frmint;
				cbut->SetCheck(*defchc);
			}
			break;
		case 13:
			cprocess = (CProgressCtrl*)GetDlgItem(fid);
			if ((m_fData.ud_data[i]).ud_delem.frmint!=NULL)
				perc = *((m_fData.ud_data[i]).ud_delem.frmint);
			else
				perc = 1;
			cprocess->SetRange(1, 100);
			cprocess->SetPos(perc);
			if (m_fStruct.input_flds[i].ud_datatyp==UD_DASINT)
			{
				fid =  m_idtable[i+m_dspno+m_fldno+m_framno+m_picno];
				sprintf_s(perc_str, sizeof(perc_str),"%d%%", perc);
				GetDlgItem(fid)->SetWindowText(perc_str);
			}
			break;
		case 15:
			cmbbx = (CComboBox*)GetDlgItem(fid);
			list = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
/*
.....we need clear combobox first
*/
			cont = cmbbx->GetCount();
			for (j = 0; j<cont; j++)
			{
				cmbbx->DeleteString(0);
			}
			defsel = -1;
			for (j=0; j<list->num_item; j++)
			{
				cmbbx->AddString(list->item[j]);
				if (strcmp(list->item[j], list->answer)==0)
					defsel = j;
			}
			if (list->num_item==0)
				cmbbx->AddString(" ");
			if (defsel!=-1)
			{
				cmbbx->SetCurSel(defsel);
			}
			else
				cmbbx->SetCurSel(0);
			break;
/*
......LIST TABLE
*/
		case 17:
			tlist = (UD_TLIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			listctl = (CNCLListCtrl*)GetDlgItem(fid);
/*
.....use same length for all column now, may be use diff value by 
.....use form struct value later
*/
			listctl->GetWindowRect(&rect);
			if (tlist->num_col>0)
				wid = rect.Width()/tlist->num_col;
			for (k=0; k<tlist->num_col; k++)
			{
				listctl->InsertColumn(k, tlist->col_label[k],  LVCFMT_LEFT, wid);
			}
			for(j=0;j<tlist->num_item;j++)
			{
				tlist->data[j].fldno = i;
				tlist->data[j].frmid = m_frmid;
				listctl->InsertItem(j,  tlist->data[j].data_items[0]);
				for (m=1; m<tlist->data[j].itemnum;m++)
				{
					listctl->SetItemText(j, m, tlist->data[j].data_items[m]);
				}
				listctl->SetItemData(j, (LPARAM)&(tlist->data[j]));
			}
			listctl->SetExtendedStyle(LVS_EX_FULLROWSELECT);
//			listctl->SetItemState (0, LVIS_SELECTED, LVIS_SELECTED);
			if ((m_sortfunc[i]!=NULL)&&(tlist->sort>=0)&&(tlist->sort<=tlist->num_col))
			{
				listctl->SortItems((PFNLVCOMPARE)SortFunc, tlist->sort);
			}
			break;
		case 19:
			dlist = (UD_DLIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			listctl2 = (CNCLListCtrl2*)GetDlgItem(fid);
/*
.....use same length for all column now, may be use diff value by 
.....use form struct value later
*/
			listctl2->GetWindowRect(&rect);
			if (dlist->num_col>0)
				wid = rect.Width()/dlist->num_col;
			if (wid < 60)
				wid = 60;
/*
.....we need clear listbox first
*/
			listctl2->DeleteAllItems();
			LV_COLUMN lvc;
			lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;
			for (k=0; k<dlist->num_col; k++)
			{
				lvc.iSubItem = k;
				lvc.pszText = dlist->col_label[k];
				lvc.cx = wid;
				lvc.fmt = LVCFMT_LEFT;
				listctl2->InsertColumn(k,&lvc);
			}
			for(j=0;j<dlist->num_item;j++)
			{
				dlist->data[j].fldno = i;
				dlist->data[j].frmid = m_frmid;
				listctl2->InsertItem(j,  dlist->data[j].data_items[0]);
				for (m=1; m<dlist->data[j].itemnum;m++)
				{
					listctl2->SetItemText(j, m, dlist->data[j].data_items[m]);
				}
				listctl2->SetItemData(j, (LPARAM)&(dlist->data[j]));
			}
			listctl2->m_nNumberOfRows = dlist->num_item;
			listctl2->m_nNumberOfCols = dlist->num_col;
			listctl2->SetExtendedStyle(LVS_EX_FULLROWSELECT|listctl2->GetExtendedStyle());
			listctl2->ModifyStyle(0,LVS_SHOWSELALWAYS);
/*
......have to set the focus to set a selection
*/
			listctl2->SetFocus();
			if (((dlist->answer[0]>=0)&&(dlist->answer[0]<dlist->num_item))
				&& ((dlist->answer[1]>0)&&(dlist->answer[1]<dlist->num_col)))
			{
				listctl2->SetItemSel(dlist->answer[0], dlist->answer[1]);
			}
			else
				listctl2->DeSelAll();
			if ((m_sortfunc[i]!=NULL)&&(dlist->sort>=0)&&(dlist->sort<=dlist->num_col))
			{
				listctl2->SortItems((PFNLVCOMPARE)SortFunc, dlist->sort);
			}
			break;
		}
		if (m_trav_mask[i] == 0)
		{
			cwin = GetDlgItem(fid);
			cwin->EnableWindow(FALSE);
			if ((m_type[i]==3)||(m_type[i]==2)||(m_type[i]==8)||(m_type[i]==16)
				||(m_type[i]==9)||(m_type[i]==10)||(m_type[i]==11)||(m_type[i] == 18)
				||(m_type[i]==13)||(m_type[i]==14) || (m_type[i] == 15)|| (m_type[i] == 23))
			{
				cwin = GetDlgItem(fid-1);
				cwin->EnableWindow(FALSE);
			}
		}
		else
		{
			cwin = GetDlgItem(fid);
			cwin->EnableWindow(TRUE);
			if ((m_type[i]==3)||(m_type[i]==2)||(m_type[i]==8)
				||(m_type[i]==9)||(m_type[i]==10)||(m_type[i]==11)
				||(m_type[i]==13)||(m_type[i]==14) || (m_type[i] == 15)
				|| (m_type[i] == 16)||(m_type[i] == 18)|| (m_type[i] == 23))
			{
				cwin = GetDlgItem(fid-1);
				cwin->EnableWindow(TRUE);
			}
		}
	}
	for (i=0; i<m_fldno; i++)
	{
		UINT fid =  m_idtable[i+m_dspno];
		if (m_disp_mask[i+m_dspno]==0)
		{
			cwin = GetDlgItem(fid);
			cwin->ShowWindow(SW_HIDE);
			if ((m_type[i]==3)||(m_type[i]==2)||(m_type[i]==8)
				||(m_type[i]==9)||(m_type[i]==10)||(m_type[i]==11)
				||(m_type[i]==13)||(m_type[i]==14) || (m_type[i] == 15)
				|| (m_type[i] == 16)||(m_type[i] == 18)|| (m_type[i] == 23))
			{
/*
......hide prompt label
*/
				cwin = GetDlgItem(fid-1);
				cwin->ShowWindow(SW_HIDE);
			}
		}
	}
	for (i=0;i<m_fStruct.n_display_fields;i++)
	{
		if (m_fStruct.ud_display_mask[i] == 1)
			set_display_mask(i, 1);
		if (m_fStruct.ud_display_mask[i] == 0 )
			set_display_mask(i, 0);
	}
/*
......no default button
*/
	if (UW_auto_cursor)
	{
/*
.....we used autocenter, so it will changed this dialog
.....position later, if not, use following statement to get dialog position
		GetWindowRect(&rect);			
*/
		NCL_MainFrame->GetWindowRect(&rect);
		uw_ntset_curpos(rect.left + rect.Width()/2, rect.top + rect.Height()/2);
	}
	if (UZ_form_hotkey[0].cmd!=-1)
	{
		m_hotkey_num[0] = UZ_form_hotkey[0].cmd;
		m_form_accel[m_form_accelnum].cmd = ID_FORM_HOTKEY1;
		m_form_accel[m_form_accelnum].fVirt = UZ_form_hotkey[0].fVirt;
		m_form_accel[m_form_accelnum].key = UZ_form_hotkey[0].key;
		m_form_accelnum++;
	}
	if (UZ_form_hotkey[1].cmd!=-1)
	{
		m_hotkey_num[1] = UZ_form_hotkey[1].cmd;
		m_form_accel[m_form_accelnum].cmd = ID_FORM_HOTKEY2;
		m_form_accel[m_form_accelnum].fVirt = UZ_form_hotkey[1].fVirt;
		m_form_accel[m_form_accelnum].key = UZ_form_hotkey[1].key;
		m_form_accelnum++;
	}
	if (UZ_form_hotkey[2].cmd!=-1)
	{
		m_hotkey_num[2] = UZ_form_hotkey[2].cmd;
		m_form_accel[m_form_accelnum].cmd = ID_FORM_HOTKEY3;
		m_form_accel[m_form_accelnum].fVirt = UZ_form_hotkey[2].fVirt;
		m_form_accel[m_form_accelnum].key = UZ_form_hotkey[2].key;
		m_form_accelnum++;
	}
	if (UZ_form_hotkey[3].cmd!=-1)
	{
		m_hotkey_num[3] = UZ_form_hotkey[3].cmd;
		m_form_accel[m_form_accelnum].cmd = ID_FORM_HOTKEY4;
		m_form_accel[m_form_accelnum].fVirt = UZ_form_hotkey[3].fVirt;
		m_form_accel[m_form_accelnum].key = UZ_form_hotkey[3].key;
		m_form_accelnum++;
	}
/*
......we are not defined our own form 'return' and 'tab' key
......but using window's default
*/
/************
	int no_return = 0, no_tab=0, no_stab=0;
	for (i=0; i<m_form_accelnum; i++)
	{
		if (m_form_accel[i].key==VK_RETURN)
		{
			no_return = 1;
/*
......Set default button	VK_RETURN
*/
//			SetDefID(-1);
/***********
		}
		if ((m_form_accel[i].key==VK_TAB) && 
			(m_form_accel[i].fVirt== (FNOINVERT | FVIRTKEY)))
			no_tab = 1;
		if ((m_form_accel[i].key==VK_TAB) && 
			(m_form_accel[i].fVirt==(FNOINVERT | FVIRTKEY | FSHIFT)))
			no_stab = 1;
	}
	if (no_return==0)
	{
		m_form_accel[m_form_accelnum].cmd = IDF_FORM_RETURN;
		m_form_accel[m_form_accelnum].fVirt = FNOINVERT | FVIRTKEY;
		m_form_accel[m_form_accelnum].key = VK_RETURN;
		m_form_accelnum++;
/*
......Set default button	VK_RETURN
*/
//		SetDefID(IDF_FORM_RETURN);
/***********
	}
	if (no_tab==0)
	{
		m_form_accel[m_form_accelnum].cmd = ID_FORM_TABED;
		m_form_accel[m_form_accelnum].fVirt = FNOINVERT | FVIRTKEY;
		m_form_accel[m_form_accelnum].key = VK_TAB;
		m_form_accelnum++;
	}
	if (no_stab==0)
	{
		m_form_accel[m_form_accelnum].cmd = ID_FORM_STABED;
		m_form_accel[m_form_accelnum].fVirt = FNOINVERT | FVIRTKEY | FSHIFT;
		m_form_accel[m_form_accelnum].key = VK_TAB;
		m_form_accelnum++;
	}
********************/
	if (m_form_accelnum!=0)
		m_accel = CreateAcceleratorTable(m_form_accel, m_form_accelnum);
	m_init = 1;
	Focused_frmid = m_frmid;
	Focused_frm_fldno = 0;
	return 0;
}	
/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks1(UINT id)
**       command callback routine for all fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::FormUserCallbacks1(UINT id)
{
	CButton* chkbx;
	int i, fldno, pikloc;
	int sav_cursor, colordlg;
	UD_FSTAT stat;
	int redisp = 0;
/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return;
/*
.....this callbacks can be assign to accelarator key, but we won't 
.....assign function to normal 'return' key, but assign to kp_enter
.....which is at the right side of the keyboard
*/
	if (m_return_key==1)
/*
.....if it is normal return key, call return callback function
*/
	{
		FormUserCallbacks6();
		return;
	}

	fldno = -1;
	pikloc = 0;
	colordlg = 0;
	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			if (m_type[i]==18)
				colordlg = 1;
			break;
		}
		else if ((m_type[i]==3)&&(m_idtable[i+m_dspno]-1==id))
		{
			fldno = i;
			pikloc = 1;
		}
	}
	if (fldno == -1)
		return;

again:;
	int status = -1;
	if (pikloc == 1)
	{
		if (m_input[fldno]==FORM_PICK || m_input[fldno]==FORM_LABEL ||
			m_input[fldno]==FORM_SUBSCR)
			status = FormPick(fldno);
		else if (m_type[i]==23)
			status = FormSelect(fldno);
		else
			status = FormLoc(fldno);
/*
.....don't return to allow callback function
		return;
*/
	}
	if (colordlg == 1)
	{
		if (m_type[i]==18)
			FormColor(fldno);
		else if (m_type[i]==23)
			PickColor(fldno);
	}
	if (m_type[fldno]==7)
	{
		chkbx = (CButton*)GetDlgItem(id);
/*		*(m_ans[fldno]) = chkbx->GetCheck();
*/
		*((m_fData.ud_data[fldno]).ud_delem.frmint) = chkbx->GetCheck();
	}
	if ((m_method_ret[fldno] & UD_TOGGLE &&
			m_method[fldno] != NULL)&&(m_init) && status != DE_DONE)
	{
/*
.....before we execute function, put wait cursor
*/
		sav_cursor = uw_ntgetcur_cursor();
		uw_ntsetcursor(21);
/*
.....reset pick segment list before callback function
*/
		ncl_reset_pikgeom();
		stat = (m_method[fldno])(&fldno, &(m_fData.ud_data[fldno].ud_delem), UD_FLDOK);
		if (fldno!=-1)
			redisp = 1;			
		uw_ntsetcursor(sav_cursor);
/*
.....Callback routine wants to
.....remain in pick mode
*/
		if (pikloc == 1 && stat == UD_FWD && m_type[fldno] == 3 &&
			(m_input[fldno]==FORM_PICK || m_input[fldno]==FORM_LABEL ||
			m_input[fldno]==FORM_SUBSCR|| (m_type[fldno]==23))) goto again;
	}
	if (redisp==1)
		uw_ntform_redisplay();			
}
/**********************************************************************
**    I_FUNCTION :  FormUserCallback (UINT id)
**       list box select callback for all fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::FormUserCallbacks2(UINT id)
{
	int i, fldno;
	int redisp = 0;
/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return;
	fldno = -1;

	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			break;
		}
	}
	if (fldno==-1)
		return;
	if (m_method_ret[fldno] & UD_TOGGLE &&
			m_method[fldno] != NULL)
	{
/*
.....reset pick segment list before callback function
*/
		ncl_reset_pikgeom();
		(m_method[fldno])(&fldno, &(m_fData.ud_data[fldno].ud_delem), UD_FLDOK);
		if (fldno!=-1)
			redisp = 1;			
	}
	if (redisp==1)
		uw_ntform_redisplay();
}
/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks3(UINT id)
**       COMB box select callback for all fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::FormUserCallbacks3(UINT id)
{
	int i, fldno, indx;
	CComboBox *cmbbx;
	CListBox* lstbx;
	int redisp = 0;
/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return;
	fldno = -1;

	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			break;
		}
	}
	if (fldno==-1)
		return;
	
	if ((m_type[fldno]==2)||(m_type[fldno]==8)||(m_type[fldno]==9))
	{
		cmbbx = (CComboBox*)GetDlgItem(id);
		*((m_fData.ud_data[fldno]).ud_delem.frmint) = cmbbx->GetCurSel();
	}
	if (m_method_ret[fldno] & UD_TOGGLE &&
			m_method[fldno] != NULL)
	{
/*
.....reset pick segment list before callback function
*/
		ncl_reset_pikgeom();
		if (m_type[fldno]==2)
			(m_method[fldno])(&fldno, &(m_fData.ud_data[fldno].ud_delem),UD_FLDOK);
		else if (m_type[fldno]==5)
		{
			UD_DDATA seldata;
			char lstr[UX_MAX_PATH_LEN];
			lstbx = (CListBox*)GetDlgItem(id);
			indx = lstbx->GetCurSel();
			lstbx->GetText(indx, lstr);
			seldata.frmstr = (char*)&lstr;
			(m_method[fldno])(&fldno, &seldata,UD_FLDOK);
		}
		else
		{
			UD_DDATA seldata;
			char lstr[UX_MAX_PATH_LEN];
			cmbbx = (CComboBox*)GetDlgItem(id);
			indx = cmbbx->GetCurSel();
			cmbbx->GetLBText(indx, lstr);
			seldata.frmstr = (char*)&lstr;
			(m_method[fldno])(&fldno, &seldata,UD_FLDOK);
		}
		if (fldno!=-1)
			redisp = 1;			
	}
	if (redisp==1)
		uw_ntform_redisplay();
}

/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks7(UINT id)
**       COMB/LIST box and button Focus callback for all fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::FormUserCallbacks7(UINT id)
{
	int fldno, stat, redisp=0;
/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return;

/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return;

	fldno = -1;
	for (int i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			break;
		}
	}
	if (fldno == -1)
		return;

	Focused_frmid = m_frmid;
	Focused_frm_fldno = fldno;
	if (m_current_fld!=-1)
	{
		stat = chk_field(m_current_fld, 1, &redisp);
		m_current_fld = -1;
		if (stat==-1)
			return;
	}
	if (redisp==1)
		uw_ntform_redisplay();
	m_current_fld = -1;
}

/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks6()
**       "hit return on edit fields" callback for all edit fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				None                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::FormUserCallbacks6()
{
/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return;
	CWnd *win = GetFocus();
	if (win==NULL)
		return;
	UINT id = win->GetDlgCtrlID();

	int i, fldno, stat;

	fldno = -1;
	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			break;
		}
	}
	if (fldno==-1)
		return;
/*
......if it is for multi-line edit box,
......don't do user define function, just return;
*/
	if (m_type[fldno]==16)
	{
		return;
	}
	if ((m_type[fldno]==1)||(m_type[fldno]==7))
	{
		if (m_type[fldno]==1)
		{
			FormUserCallbacks1(id);
		}
		else
		{
			CButton *chkbx = (CButton*)GetDlgItem(id);
			stat = chkbx->GetCheck();
			if (stat)
				chkbx->SetCheck(0);
			else
				chkbx->SetCheck(1);
			FormUserCallbacks1(id);
		}
		return;
	}
/*
......just set the focus to next field, 
......and select all text like '\t' does
*/
	OnFormTabbed();
/************************************************
	CWnd* win = GetFocus()->GetNextWindow();
	if (win==NULL)
		return;
	int i, fldno;
	UINT id = win->GetDlgCtrlID();
	fldno = -1;
	for (i=0; i<m_fldno; i++)
	{
		if ((m_idtable[i+m_dspno]==id)||
				(m_idtable[i+m_dspno]==id+1))
		{
			fldno = i;
			win = GetDlgItem(m_idtable[fldno+m_dspno]);
			if (win==NULL)
				return;
			if ((win->IsWindowEnabled()) && (win->IsWindowVisible()))
				break;
			else
			{
				win = win->GetNextWindow();
				if (win==NULL)
					return;
				id = win->GetDlgCtrlID();
			}
		}
	}
	if (fldno!=-1)
	{
		win = GetDlgItem(m_idtable[fldno+m_dspno]);
		win->SetFocus();
		if (m_type[fldno]==3)
			((CEdit*)win)->SetSel(0, -1);
	}
	else if (id==IDC_DLG_BOX)
	{
		(win->GetNextWindow())->SetFocus();
		return;
	}
	else
		win->SetFocus();
*/
}

/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks4(UINT id)
**       edit text exit callback for all fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::FormUserCallbacks4(UINT id)
{
	int i;
	int fldno = -1;
/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return;
	Focused_frmid = -1;
	Focused_frm_fldno = -1;

	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			break;
		}
	}
	if (fldno==-1)
		return;
/*
......save current focus field
*/
	if (m_type[fldno]==3)
		m_current_fld = fldno;
	return;

}
/**********************************************************************
**    I_FUNCTION :  chk_field(int fldno, int flag)
**       Check form field data format and optional call
**			user callback function
**    PARAMETERS
**       INPUT  :
**				fldno: field number			
**				flag: 1: call user defined function if check data no err
**						0: don't call user defined function	
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int CNCLFormBar::chk_field(int fldno, int flag, int *redisp)
{
	char lstr[UX_MAX_PATH_LEN];
	char ldat[UX_MAX_PATH_LEN];
	int stat, mflag;
	UD_LIST *list;
	UD_FSTAT stat2;

	int id = m_idtable[fldno+m_dspno];
	CEdit *cedt = (CEdit*)GetDlgItem(id);
	m_current_fld = -1;
	cedt->GetWindowText(lstr, UX_MAX_PATH_LEN-1);
	mflag = 0;
	stat = form_ckdata(fldno,lstr, &mflag);

	if (stat==UU_FALSE)
	{
		current_chk_field = fldno;
		cedt->SetFocus();
		current_chk_field = -1;
		return -1;
	}
	if ((!((m_fStruct.input_flds[fldno].ud_datatyp>=UD_DASSCALAR) && 
		 (m_fStruct.input_flds[fldno].ud_datatyp<=UD_SCAUNITLESS))) 
		 && (m_type[fldno]==3))
	{
		ud_formstr_format(m_fStruct.input_flds[fldno].ud_datatyp,
				m_fData.ud_data[fldno].ud_delem, m_fStruct.input_flds[fldno].ud_fprec,
				m_fStruct.input_flds[fldno].ud_flen, ldat);
		cedt->SetWindowText(ldat);
	}
	if (flag==0)
		return 1;
	if (m_method_ret[fldno] & UD_TOGGLE &&
			m_method[fldno] != NULL)
	{
/*
.....reset pick segment list before callback function
*/
		ncl_reset_pikgeom();
		if (m_type[fldno] != 9)
			stat2 = (m_method[fldno])(&fldno, &(m_fData.ud_data[fldno].ud_delem), UD_DONE);
		else
		{
			UD_DDATA seldata;
			char lstr[UX_MAX_PATH_LEN];
			list = (UD_LIST *)(m_fData.ud_data[fldno].ud_delem.frmint) ;
			if (list->answer!=NULL)
				strcpy_s(lstr, sizeof(lstr),list->answer);
			seldata.frmstr = (char*)&lstr;
			stat2 = (m_method[fldno])(&fldno, &seldata,UD_DONE);
		}
		if (fldno!=-1)
/*
......don't call redisplay here, just set flag and call later
*/
			*redisp = 1;
		else
			*redisp = 0;
	}
	if (stat2==UD_BADREQ)
/*
.....if it is bad, then stay the focus in current field
*/
	{
		current_chk_field = fldno;
		cedt->SetFocus();
		current_chk_field = -1;
		return -1;
	}	
	return 1;
}
/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks5(UINT id)
**       Set focus callback for all edit fields in the form.
**			
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::FormUserCallbacks5(UINT id)
{
	int i, fldno, stat;
	int redisp = 0;

/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return;
	fldno = -1;
	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			break;
		}
	}
	if (fldno == -1)
		return;
	Focused_frmid = m_frmid;
	Focused_frm_fldno = fldno;

	if (m_current_fld == fldno)
	{
		return;
	}

	if (m_current_fld!=-1)
	{
		if (current_chk_field==fldno)
			return;
		stat = chk_field(m_current_fld, 1, &redisp);
		m_current_fld = -1;
		if (stat==-1)
			return;
	}
/*
.....never set focus to display edit
*/
	if (m_type[fldno]==11)
	{
		CWnd *win;
		if (fldno==m_fldno-1)
		{
			win = GetDlgItem(m_idtable[0]);
		}
		else if (m_forward)
			win = GetDlgItem(m_idtable[fldno+1+m_dspno]);
		else if (fldno-1>=0)
			win = GetDlgItem(m_idtable[fldno-1+m_dspno]);
		else
			win = GetDlgItem(m_idtable[m_fldno+m_dspno-1]);
		m_forward = 1;
		if (win==NULL)
			return;
		win->SetFocus();
	}		
	if (m_type[fldno]==3)
	{
		m_actedit = (CEdit*)GetDlgItem(id);
/*
.....set highlight if needed
*/
		if (UW_frmtext_select)
			PostMessage(WM_COMMAND, ID_FORM_HIGHLIGHT);
	}
	if (redisp)
		uw_ntform_redisplay();
	m_current_fld = -1;
}

/**********************************************************************
**    I_FUNCTION :  set_list(int fieldno, UD_LIST *form_list)
**       Set the form list field fieldno. 
**    PARAMETERS   
**       INPUT  : 
**          fieldno : field number
**			form_list: UD_LIST structure for redisplay
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::set_list(int fieldno, UD_LIST *form_list)
{
	int i,j,cont, defsel;
	CListBox* listbx;
	CComboBox* cmbbx;
	UINT fid =  m_idtable[fieldno+m_dspno];
	switch(m_type[fieldno])
	{
	case 5:
		listbx = (CListBox*)GetDlgItem(fid);
/*
.....we need clear listbox first
*/
		cont = listbx->GetCount();
		for (i = 0; i<cont; i++)
		{
			listbx->DeleteString(0);
		}
		defsel = -1;
		for (j=0; j<form_list->num_item; j++)
		{
			if (strcmp(form_list->item[j], form_list->answer)==0)
				defsel = j;

			listbx->InsertString(j, form_list->item[j]);
		}
		if (form_list->num_item==0)
			listbx->AddString(" ");

		if (defsel!=-1)
			listbx->SetCurSel(defsel);
		SendMessage(WM_COMMAND,
				MAKEWPARAM(fid, CBN_SELCHANGE),
				(LPARAM)m_hWnd);
		break;
	case 8:
	case 9:
	case 15:
		cmbbx = (CComboBox*)GetDlgItem(fid);
/*
.....we need clear listbox first
*/
		cont = cmbbx->GetCount();
		for (i = 0; i<cont; i++)
		{
			cmbbx->DeleteString(0);
		}
		defsel = -1;
		for (j=0; j<form_list->num_item; j++)
		{
			if (strcmp(form_list->item[j], form_list->answer)==0)
				defsel = j;
			cmbbx->InsertString(j, form_list->item[j]);
		}
		if (form_list->num_item==0)
			cmbbx->AddString(" ");
		if (defsel!=-1)
			cmbbx->SetCurSel(defsel);
		else
			cmbbx->SetCurSel(0);
		SendMessage(WM_COMMAND,
				MAKEWPARAM(fid, CBN_SELCHANGE),
				(LPARAM)m_hWnd);
		break;
	}
}


/**********************************************************************
**    I_FUNCTION :  set_text(int fieldno, char *text)
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
void CNCLFormBar::set_text(int fieldno, char *text)
{
	UINT fid =  m_idtable[fieldno+m_dspno];
	CWnd *cwin = GetDlgItem(fid);
	cwin->SetWindowText(text);
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
void CNCLFormBar::set_label(int fieldno, char* label)
{
	UINT fid =  m_idtable[fieldno];
	CWnd *cwin = GetDlgItem(fid);
	cwin->SetWindowText(label);
}

/**********************************************************************
**    I_FUNCTION :  reset_fldlabel(int fieldno, char* label)
**       Set the label in form input field fieldno. 
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
void CNCLFormBar::reset_fldlabel(int fieldno, char* label)
{
	if ((m_type[fieldno]==3)||(m_type[fieldno]==2)||(m_type[fieldno]==8)
		||(m_type[fieldno]==9)||(m_type[fieldno]==10) ||(m_type[fieldno]==11)
		||(m_type[fieldno]==13)||(m_type[fieldno]==14) || (m_type[fieldno] == 15)
		|| (m_type[fieldno] == 16))
	{
		if (m_input[fieldno] == FORM_STRING)
		{
			UINT fid =  m_idtable[fieldno+m_dspno] - 1;
			CWnd *cwin = GetDlgItem(fid);
			cwin->SetWindowText(label);
		}
	}
	else if (m_type[fieldno]==18)
	{
		UINT fid =  m_idtable[fieldno+m_dspno] - 1;
		CWnd *cwin = GetDlgItem(fid);
		cwin->SetWindowText(label);
	}
}

/**********************************************************************
**    I_FUNCTION :  set_choice(int fieldno, int choice)
**       Set the choice in form input field fieldno. 
**    PARAMETERS   
**       INPUT  : 
**          fieldno : field number
**			choice:	choice to set
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::set_choice(int fieldno, int choice)
{
	CComboBox* cmbbx;
	UINT fid =  m_idtable[fieldno+m_dspno];
	cmbbx = (CComboBox*)GetDlgItem(fid);
	cmbbx->SetCurSel(choice);
}

/**********************************************************************
**    I_FUNCTION :  set_display_mask(int fieldno, int val)
**       Set the display mask in form input/display field fieldno. 
**    PARAMETERS   
**       INPUT  : 
**          fieldno : field number
**			val:	display mask value
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::set_display_mask(int fieldno, int val)
{
	UINT fid =  m_idtable[fieldno];
	CWnd *cwin = GetDlgItem(fid);
	if (cwin==NULL)
		return;
	if (val==0)
	{
		cwin->ShowWindow(SW_HIDE);
		if (fieldno-m_dspno<0)
			return;
		if ((m_type[fieldno-m_dspno]==3)||(m_type[fieldno-m_dspno]==2)
			||(m_type[fieldno-m_dspno]==8)||(m_type[fieldno-m_dspno]==9)
			||(m_type[fieldno-m_dspno]==10) ||(m_type[fieldno-m_dspno]==13)
			||(m_type[fieldno-m_dspno]==11) || (m_type[fieldno-m_dspno]==16) 
			||(m_type[fieldno-m_dspno]==14) || (m_type[fieldno-m_dspno] == 15))
		{
/*
......hide prompt label
*/
			cwin = GetDlgItem(fid-1);
			cwin->ShowWindow(SW_HIDE);
		}
	}
	else
	{
		cwin->ShowWindow(SW_SHOW);
		if (fieldno-m_dspno<0)
			return;
		if ((m_type[fieldno-m_dspno]==3)||(m_type[fieldno-m_dspno]==2)
			||(m_type[fieldno-m_dspno]==8)||(m_type[fieldno-m_dspno]==9)
			||(m_type[fieldno-m_dspno]==10) ||(m_type[fieldno-m_dspno]==13)
			||(m_type[fieldno-m_dspno]==11) ||(m_type[fieldno-m_dspno]==16)
			||(m_type[fieldno-m_dspno]==14) || (m_type[fieldno-m_dspno] == 15))
		{
/*
......hide prompt label
*/
			cwin = GetDlgItem(fid-1);
			cwin->ShowWindow(SW_SHOW);
		}
	}
}

/**********************************************************************
**    I_FUNCTION :  set_traverse_mask(int fieldno, int val)
**       Set the traverse mask in form input field fieldno. 
**    PARAMETERS   
**       INPUT  : 
**          fieldno : field number
**			val:	traverse mask value
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::set_traverse_mask(int fieldno, int val)
{
	UINT fid =  m_idtable[fieldno+m_dspno];
	CWnd *cwin = GetDlgItem(fid);
	if (val==0)
	{
		cwin->EnableWindow(FALSE);
		if ((m_type[fieldno]==3)||(m_type[fieldno]==2)
			||(m_type[fieldno]==8)||(m_type[fieldno]==9)|| (m_type[fieldno] == 16)
			||(m_type[fieldno]==10) ||(m_type[fieldno]==13)||(m_type[fieldno]==11)
			||(m_type[fieldno]==14) || (m_type[fieldno] == 15)|| (m_type[fieldno] == 18))
		{
/*
......disable prompt label
*/
			cwin = GetDlgItem(fid-1);
			cwin->EnableWindow(FALSE);
		}
	}
	else
	{
		cwin->EnableWindow(TRUE);
		if ((m_type[fieldno]==3)||(m_type[fieldno]==2)||(m_type[fieldno]==11)
			||(m_type[fieldno]==8)||(m_type[fieldno]==9)|| (m_type[fieldno] == 16)
			||(m_type[fieldno]==10) ||(m_type[fieldno]==13)
			||(m_type[fieldno]==14) || (m_type[fieldno] == 15)||(m_type[fieldno]==18))
		{
/*
......enable prompt label
*/
			cwin = GetDlgItem(fid-1);
			cwin->EnableWindow(TRUE);
		}
		if (m_type[fieldno]==13)
		{
			fid =  m_idtable[fieldno+m_dspno+m_fldno+m_framno+m_picno];
			GetDlgItem(fid)->EnableWindow(TRUE);
		}
	}
}

/**********************************************************************
**    I_FUNCTION :  form_ckdata(fno,buf)
**       Checks form input data for correct type and values.
**    PARAMETERS   
**       INPUT  : 
**          fno     = Field number to check.
**			buf     = Form input data to check.
**       OUTPUT :  
**          flag: 1: scalar value include
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int CNCLFormBar::form_ckdata(int fno, char *buf, int *flag)
{
	UD_DASDATA dsda;
	UD_DASIN de;
	UD_LIST *list_ans;
	int irtn,typ,stat,nc,i,ifl, len;
	char text[UX_MAX_PATH_LEN], erms[80];
/*
.....Initialize routine
*/
	*flag = 0;
	irtn = UU_TRUE;
	typ = m_fStruct.input_flds[fno].ud_datatyp;

	stat = 0;
	for (i=strlen(buf)-1;i>=0;i--) if (buf[i] != ' ') break;
	buf[i+1] = '\0';
	len = strlen(buf);
	if ((typ>=UD_DASSCALAR) && (typ<=UD_SCAUNITLESS))
	{
/*
.....Check if the string is the scalar value/string
.....even though we may keep and return string, but
.....we still need get the value in order to check the range
*/
		strcpy_s(text,sizeof(text),buf);
/*
.....Get the scalar and convert to value
*/
		stat = ncl_parse_scalar_values(text, text, typ);
		if (stat==-1)
		{
			sprintf_s(erms,sizeof(erms),"%s is not a valid scalar value",buf);
			uw_ntdispmsg(erms);
			return UU_FALSE;
		}
	}

	if (typ == UD_DASVAL || typ == UD_DASUNITLESS ||
		typ == UD_DASDISTANCE || typ == UD_DASANGLE ||
			typ == UD_DASINT || typ == UD_DASVEC || 
			typ == UD_DASCART )
	{
		strcpy_s(text,sizeof(text),buf);
/*
.....Get the scalar and convert to value
*/
		stat = ncl_parse_scalar_values(text, text, typ);
		if (stat==-1)
		{
			sprintf_s(erms,sizeof(erms),"%s is not a valid scalar value",buf);
			uw_ntdispmsg(erms);
			return UU_FALSE;
		}
	}
	if (typ != UD_DASSTRING)
	{
/*
.....if it's a scalar string value, use text get above
*/
		if (stat!=1)
			strcpy_s(text,sizeof(text),buf);
		else
			*flag = 1;
		nc = strlen(text);
		ul_strip_blanks(text,&nc);
		ifl = 0;
		if (typ == UD_DASVAL || typ == UD_DASUNITLESS ||
		    typ == UD_DASINT || typ == UD_DASVEC || typ == UD_DASNDC
			|| typ == UD_DASSCALAR)
		        ifl = UD_UNITLESS;
		else if (typ == UD_DASANGLE) ifl = UD_DANGLE;
		irtn = ud_dasin(text,&dsda,ifl);
		if (!irtn) goto failed;
/*
.....handle for for scalar
*/
		if ((typ>=UD_DASSCALAR) && (typ<=UD_SCAUNITLESS))
		{
			ud_scatodas(&dsda, &de, typ);
			stat = ud_ckdata(m_fStruct, &de, fno);
			if (stat != UD_VALOK) goto failed;
			else
			{
				strcpy_s(m_fData.ud_data[fno].ud_delem.frmstr, len+1,buf);		
				goto done;
			}
		}
	}
/*
.....Verify correct type of input
*/
	switch (typ)
	{
/*
.....Allow for the MODSYS matrix
*/
	case UD_DASVEC:
		if (dsda.dtype != 2) goto failed;
		um_ccstomcs(1,dsda.stval.stcord.coord,m_fData.ud_data[fno].ud_delem.frmvec);
/*
......adjust the value
*/
		UM_cc_exttoint(m_fData.ud_data[fno].ud_delem.frmvec, m_fData.ud_data[fno].ud_delem.frmvec);
		break;
	case UD_DASCART:
		if (dsda.dtype != 2) goto failed;
		um_ccstomcs(0,dsda.stval.stcord.coord,m_fData.ud_data[fno].ud_delem.frmvec);
/*
......adjust the value
*/
		UM_cc_exttoint(m_fData.ud_data[fno].ud_delem.frmvec, m_fData.ud_data[fno].ud_delem.frmvec);
		break;
	case UD_DASNDC:
		if (dsda.dtype != 2) goto failed;
		m_fData.ud_data[fno].ud_delem.frmvec[0] = dsda.stval.stcord.coord[0];
		m_fData.ud_data[fno].ud_delem.frmvec[1] = dsda.stval.stcord.coord[1];
		m_fData.ud_data[fno].ud_delem.frmvec[2] = dsda.stval.stcord.coord[2];
		break;
	case UD_DASSCALAR:
		strcpy_s(m_fData.ud_data[fno].ud_delem.frmstr,len+1,buf);
		break;
	case UD_DASVAL:
	case UD_DASUNITLESS:
		if (dsda.dtype != 1) goto failed;
		m_fData.ud_data[fno].ud_delem.frmflt[0] = dsda.stval.dval;
		break;
	case UD_DASDISTANCE:
		if (dsda.dtype != 1) goto failed;
		m_fData.ud_data[fno].ud_delem.frmflt[0] = dsda.stval.dval;
/*
......adjust the value
*/
		UM_len_exttoint(m_fData.ud_data[fno].ud_delem.frmflt[0],
			m_fData.ud_data[fno].ud_delem.frmflt[0]);
		break;
	case UD_DASANGLE:
		if (dsda.dtype != 1) goto failed;
		m_fData.ud_data[fno].ud_delem.frmflt[0] = dsda.stval.dval;
/*
......adjust the value
*/
		UM_ang_inttoext(m_fData.ud_data[fno].ud_delem.frmflt[0],
			m_fData.ud_data[fno].ud_delem.frmflt[0]);
		break;
	case UD_DASINT:
		if (dsda.dtype != 1) goto failed;
		m_fData.ud_data[fno].ud_delem.frmint[0] = (int)dsda.stval.dval;
		break;
	case UD_DASSTRING:
		for (i=strlen(buf)-1;i>=0;i--) if (buf[i] > ' ') break;
		buf[i+1] = '\0';
		if (m_fStruct.input_flds[fno].toggle==5)
		{
			list_ans = (UD_LIST *)(m_fData.ud_data[fno].ud_delem.frmint) ;
			if ((i!=-1)&&(list_ans->answer!=NULL))
				strcpy_s(list_ans->answer,len+1, buf);
		}
		else
			strcpy_s(m_fData.ud_data[fno].ud_delem.frmstr,len+1,buf);
			
		break;
	default:
		goto failed;
	}
/*
.....Check for valid range
*/
	if (typ != UD_DASSTRING)
	{
		ud_todas(&m_fData.ud_data[fno].ud_delem,&de,typ);
		stat = ud_ckdata(m_fStruct,&de,fno);
		if (stat != UD_VALOK) goto failed;
	}
	goto done;
/*
.....End of routine
*/
failed:;
	irtn = UU_FALSE;
done:;
	return(irtn);
}

/**********************************************************************
**    I_FUNCTION :  reset_picture(int fieldno, int *val)
**       update picture in form input field fieldno
**    PARAMETERS   
**       INPUT  : 
**          fieldno     = Field number to check.
**			val     = pointer to picture value
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::reset_picture(int fieldno, int *val)
{
	(m_fData.ud_data[fieldno]).ud_delem.frmint = val;
	OnPaint();
}

/**********************************************************************
**    I_FUNCTION :  Get_field_data(fieldno, data, str_flag)
**       Get the UD_DDATA from give fieldno 
**    PARAMETERS   
**       INPUT  : 
**          fieldno   : field number
**          data      : UD_DDATA structure
**          str_flag  : UU_TRUE if a character string is to be returned
**                      with each field type.
**       OUTPUT :  
**          data: UD_DDATA structure 
**				if text field: string store in frmstr
**				others: data store in frmint[0]
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::Get_field_data(int fieldno, UD_DDATA data,UU_LOGICAL str_flag)
{
	int indx, len;
	CButton* cbut;
	CComboBox* cmbbx;
	CListBox* listbx;
	CEdit* cedt;
	char string[UX_MAX_PATH_LEN];

	UINT fid =  m_idtable[fieldno+m_dspno];

	switch(m_type[fieldno])
	{
/*
.....COMBBOX, SIMPLE and DROPDOWN, can be edited, get
.....answer from edit box of this COMB
*/
		case 8:
			cmbbx = (CComboBox*)GetDlgItem(fid);
			cmbbx->GetWindowText(string, UX_MAX_PATH_LEN-1);
			len = strlen(string);
			strcpy_s(data.frmstr, len+1,string);
			break;
		case 9:
			break;
/*
.....COMBBOX, CHOICES, can't be edited.
*/
		case 2:
			cmbbx = (CComboBox*)GetDlgItem(fid);
			data.frmint[0] = cmbbx->GetCurSel();
			if ((str_flag) || ((m_init_data_type_flag)&&(m_data_type[fieldno]==UD_DASSTRING)))
			{
				cmbbx->GetLBText(data.frmint[0],string);
				len = strlen(string);
				strcpy_s(data.frmstr, len+1,string);
			}
			break;
/*
......LIST BOX
*/
		case 5:
			listbx = (CListBox*)GetDlgItem(fid);
			indx = listbx->GetCurSel(); 
			if (indx==LB_ERR)
				string[0] = '\0';
			else
				listbx->GetText(indx, string) ;
			len = strlen(string);
			strcpy_s(data.frmstr, len+1,string);
			break;
/*
.....EDIT
*/
		case 3:
		case 11:
		case 16:
			cedt = (CEdit*)GetDlgItem(fid);
			cedt->GetWindowText(string, UX_MAX_PATH_LEN-1);
			len = strlen(string);
			strcpy_s(data.frmstr, len+1,string);
			break;
/*
.....CHECKBOX
*/
		case 7:
			cbut = (CButton*)GetDlgItem(fid);
			data.frmint[0] = cbut->GetCheck();
			break;
		case 18:
			data.frmint[0] = *((m_fData.ud_data[fieldno]).ud_delem.frmint);
			break;
	}
}

/**********************************************************************
**    I_FUNCTION :  form_accept_close()
**       accept form data and close form
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int CNCLFormBar::form_accept_close()
{
	FormAccept();
	return 1;
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
HBRUSH CNCLFormBar::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	int i,status;
	UINT fid;
	CWnd *wnd, *pwnd;
	COLORREF fcolor, bcolor;

	for (i=0; i<m_dspno; i++)
	{
		fid = m_idtable[i];  
		wnd = (CWnd*)(GetDlgItem(fid));
		if (wnd->m_hWnd==pWnd->m_hWnd)
		{
			if (stricmp (m_fStruct.display_flds[i].fcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.display_flds[i].fcolor, fcolor); 
				if (status!=-1)
					pDC->SetTextColor(fcolor);
			}
			if (stricmp (m_fStruct.display_flds[i].bcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.display_flds[i].bcolor, bcolor); 
				if (status!=-1)
				{
					pDC->SetBkColor(bcolor);
					return (HBRUSH)(m_dBkBrush[i]->GetSafeHandle());
				}
			}
			return CNCLDialogBar::OnCtlColor(pDC, pWnd, nCtlColor);
		}
	}
	for (i=0; i<m_fldno; i++)
	{
		fid =  m_idtable[i+m_dspno];
		wnd = (CWnd*)(GetDlgItem(fid));
			if ((m_type[i]==3)||(m_type[i]==2)||(m_type[i]==8)||(m_type[i]==16)
				||(m_type[i]==9)||(m_type[i]==10)||(m_type[i]==11)||(m_type[i] == 18)
				||(m_type[i]==13)||(m_type[i]==14) || (m_type[i] == 15))
		{
			pwnd = (CWnd*)(GetDlgItem(fid-1));
		}
		else
			pwnd = NULL;
		if ((wnd!=NULL)&&(wnd->m_hWnd==pWnd->m_hWnd))
		{
/*
......input field
*/
			if (m_type[i]==11)
			{
				if (stricmp (m_fStruct.input_flds[i].fcolor, "DEFAULT")!=0)
				{
					status = uw_get_rgb (m_fStruct.input_flds[i].fcolor, fcolor); 
					if (status!=-1)
						pDC->SetTextColor(fcolor);
					else
						pDC->SetTextColor(RGB(20, 20, 20));
				}
				else
					pDC->SetTextColor(RGB(20, 20, 20));

				if (stricmp (m_fStruct.input_flds[i].bcolor, "DEFAULT")==0)
				{
					pDC->SetBkColor(RGB(255, 255, 255));
					return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
				}
			}
			if (stricmp (m_fStruct.input_flds[i].fcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.input_flds[i].fcolor, fcolor); 
				if (status!=-1)
					pDC->SetTextColor(fcolor);
			}
			if (stricmp (m_fStruct.input_flds[i].bcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.input_flds[i].bcolor, bcolor); 
				if (status!=-1)
				{
					pDC->SetBkColor(bcolor);
					return (HBRUSH)(m_BkBrush[i]->GetSafeHandle());
				}
			}
			return CNCLDialogBar::OnCtlColor(pDC, pWnd, nCtlColor);
		}
		else if ((pwnd!=NULL)&&(pwnd->m_hWnd==pWnd->m_hWnd))
		{
/*
......prompt field
*/
			if (stricmp (m_fStruct.input_flds[i].pfcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.input_flds[i].pfcolor, fcolor); 
				if (status!=-1)
					pDC->SetTextColor(fcolor);
			}
			if (stricmp (m_fStruct.input_flds[i].pbcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.input_flds[i].pbcolor, bcolor);
				if (status!=-1)
				{
					pDC->SetBkColor(bcolor);
					return (HBRUSH)(m_pBkBrush[i]->GetSafeHandle());
				}
			}
			return CNCLDialogBar::OnCtlColor(pDC, pWnd, nCtlColor);
		}
	}
	for (i=0; i<m_framno; i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno];  
		wnd = (CWnd*)(GetDlgItem(fid));
		if ((wnd!=NULL)&&(wnd->m_hWnd==pWnd->m_hWnd))
		{
			if (stricmp (m_fStruct.frame_flds[i].fcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.frame_flds[i].fcolor, fcolor); 
				if (status!=-1)
					pDC->SetTextColor(fcolor);
			}
			if (stricmp (m_fStruct.frame_flds[i].bcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.frame_flds[i].bcolor, bcolor); 
				if (status!=-1)
				{
					pDC->SetBkColor(bcolor);
					return (HBRUSH)(m_fBkBrush[i]->GetSafeHandle());
				}
			}
			return CNCLDialogBar::OnCtlColor(pDC, pWnd, nCtlColor);
		}
	}
	return CNCLDialogBar::OnCtlColor(pDC, pWnd, nCtlColor);
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
BOOL CNCLFormBar::PreTranslateMessage(MSG* pMsg) 
{
	HWND hWnd = (HWND)*this; 
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;
/*
.....for multi-lines editbox, we want the window execute the default function
.....(get into the next line) instead of user define function
*/
	if (Focused_frmid==m_frmid)
	{
		if ((Focused_frm_fldno>=0) && (m_type[Focused_frm_fldno]==16))
		{
			return CNCLDialogBar::PreTranslateMessage( pMsg );
		}
	}
/*
.....remember this because the accelerator key function only handle VK_RETURN
.....as one key, so we need to know if there is the normal return key
.....or right side function kp_enter or not at all
*/
	m_return_key = 0;
	if ((int)(pMsg->wParam)==VK_RETURN)
	{
		if (HIWORD(pMsg->lParam) & KF_EXTENDED)
/*
.....kp_enter in the right side of keypad,act as function key here
*/
		{
			m_return_key = 2;
		}
		else
/*
.....kp_enter in the left side of keypad, act as normal key enter
*/
		{
			m_return_key = 1;
		}
	}
	if (TranslateAccelerator(hWnd, m_accel, pMsg))
	{
		m_return_key = 0;
		return TRUE;
	}
	else
	{
		m_return_key = 0;
		return CNCLDialogBar::PreTranslateMessage( pMsg );
	}
}

/**********************************************************************
**    E_FUNCTION :  OnFormTextHighlight()
**       Handle highlight of text field.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::OnFormTextHighlight()
{
	CWnd *txtwin, *cedt; 
	int i, fid;
	if (UW_frmtext_select==0)
		return;
	txtwin = GetFocus();
	for (i=0; i<m_fldno; i++)
	{
		fid =  m_idtable[i+m_dspno];
		if (m_type[i]!=3)
			continue;
		cedt = (CEdit*)GetDlgItem(fid);
		if (cedt->m_hWnd==txtwin->m_hWnd)
		{
			((CEdit*)cedt)->SetSel(0, -1);
			break;
		}
	}
}

/**********************************************************************
**    E_FUNCTION :  OnFormTabbed()
**       Handle tab event of form.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::OnFormTabbed()
{
	CWnd *PwndNext = GetNextDlgTabItem(GetFocus());
	if (PwndNext)
	{
		m_forward = 1;
		PwndNext->SetFocus();
	}
}
/**********************************************************************
**    E_FUNCTION :  OnFormSTabbed()
**       Handle tab event of form.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::OnFormSTabbed()
{
	CWnd *PwndPr = GetNextDlgTabItem(GetFocus(), TRUE);
	if (PwndPr)
	{
		m_forward = 0;
		PwndPr->SetFocus();
	}
}
/**********************************************************************
**    E_FUNCTION :  FormVis()
**       Visible the formbar.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::FormVis()
{
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());

	NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
	NCL_MainFrame->RecalcLayout();
	if (m_disptype!=1)
	{
		::EnableWindow(MainWin->GetSafeHwnd(), FALSE);
	}
/*
......need to enable the form again because the form could be child of 
......main window, when disable main window, the form is disable too
*/
	EnableWindow();
	::SetActiveWindow(m_hWnd);
}

/**********************************************************************
**    E_FUNCTION :  FormInvis()
**       Invisible the formbar.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::FormInvis()
{
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
	::SetActiveWindow(MainWin->GetSafeHwnd());

	NCL_MainFrame->ShowControlBar(this, FALSE, FALSE);
	NCL_MainFrame->RecalcLayout();

	if (Pocket_Win[UM_IPV_WINDOW] != NULL)
		::EnableWindow((Pocket_Win[UM_IPV_WINDOW])->GetSafeHwnd(), TRUE);
}
/***********************************************************************
c
c   SUBROUTINE:  CNCLFormBar
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/

CNCLFormBar::CNCLFormBar(CWnd* pParent, int dispflag)	
{
	int i;
	m_frmid = 0;
	m_pParent = pParent;
	m_disptype = dispflag;
	if (m_disptype)
		m_modal = 0;
	else
		m_modal = 1;	
	m_itemnum = 0;
	m_fldno = 0;
	m_TitleCaption = "";
	m_title[0] = '\0';
	m_filename[0] = '\0';
	m_savfrm = NULL;
	for (i=0; i<MAX_FORMITEM; i++)
	{
		m_formchcnum[i] = 0;
		m_formchoice[i] = NULL;
		m_input[i] = FORM_STRING;
		m_pmodal[i] = 0;
		m_prec[i] = 1;
		m_len[i] = 0;
		m_disp_mask[i] = 1;
		m_trav_mask[i] = 1;
		m_method[i] = NULL;
		m_sortfunc[i] = NULL;
	}
	m_helptext = NULL;
	m_helpbox = NULL;
	m_actedit = NULL;
	m_pocket = 0;
	m_current_fld = -1;
/*
.....initial backgroup brush for ReadOnly Edit control
*/
	m_pEditBkBrush = new CBrush(RGB(255, 255, 255));
	for (i=0; i<MAX_FORMITEM; i++)
	{
		m_BkBrush[i] = NULL;
		m_pBkBrush[i] = NULL;
		m_dBkBrush[i] = NULL;
	}
	for (i=0; i<20; i++)
		m_fBkBrush[i] = NULL;
	m_childnum = 0;
	m_parentID = -1;
	for (i=0; i<60; i++)
		m_childfrm[i] = -1;
	m_init = 0;
	m_forward = 1;
	m_form_accel = (ACCEL *)malloc(200*sizeof(ACCEL));
	m_form_accelnum = 0;
	m_hotkey_num[0] = -1;
	m_hotkey_num[1] = -1;
	m_hotkey_num[2] = -1;
	m_hotkey_num[3] = -1;
	for (i=0; i<20; i++)
	{
		m_picture[i] = NULL;
	}
	m_init_data_type_flag = 0;
//	strcpy(UW_formdlg_font, UW_form_font);
	strcpy(UW_formdlg_font, "MS Sans Serif");
}

/***********************************************************************
c
c   SUBROUTINE:  ~CNCLFormBar
c
c   FUNCTION:  Destructor, free class data space
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLFormBar::~CNCLFormBar()
{
	int i, j;
	for (i=0; i<MAX_FORMITEM; i++)
	{
		if (m_formchoice[i] != NULL)
		{
			for (j=0; j<m_formchcnum[i]; j++)
			{
				if (m_formchoice[i][j]!=NULL)
					free(m_formchoice[i][j]);
			}
			free((char*)m_formchoice[i]);
		}
	}
	if (m_pocket)	
	{
		um_close_pocket_window(UM_DRAWING_WINDOW);
		um_close_pocket_window(UM_GRAPHIC_WINDOW);
		m_pocket = 0;
	}
	if (m_helptext != NULL)
		free(m_helptext);
	if (m_helpbox!=NULL)
	{
/*
.....destroy the dialog first before delete, the OnCancel function by
.....'X' button on help dialog only invisible the window but not destroy
.....because the help dialog is a modaless dialog
......even though the delete function will destroy the dialog but call 
......DestroyWindow inside delete function will miss some callbacks for destroywindow.
*/
		m_helpbox->DestroyWindow();
		delete m_helpbox;
		m_helpbox = NULL;
	}
	for (int i=0; i<20; i++)
	{
		if (m_picture[i] != NULL)
			delete m_picture[i];
	}
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
void CNCLFormBar::OnPaint() 
{
	CNCLDialogBar::OnPaint();
/*
.....paint picture item if there is any
*/
	int end, intpt, x,y,n,red, green, blue;
	int x0, y0, x1,y1, x2, y2, x3, y3, x4, y4, r, dir;
	char *textstr;
	int current_x = 0;
	int current_y = 0;
	int current_style = PS_SOLID;
	int current_width = 1;
	COLORREF current_color = RGB (0,0,0);

	int i;
	int *cut_array;
	UINT fid;
	for (i=0; i<m_fldno; i++)
	{
		fid =  m_idtable[i+m_dspno];
		if (m_type[i]!=10)
			continue;
/*
.....Picture item
*/
		cut_array = (int*)((m_fData.ud_data[i]).ud_delem.frmint);
		if (cut_array==NULL)
			continue;
		CWnd *picctl = GetDlgItem(fid);
		CClientDC dc(picctl);
		dc.SetBkMode(TRANSPARENT);
		RECT rc;
		picctl->GetClientRect(&rc);	
		CBrush* pBrush;
		pBrush = new CBrush(RGB(0, 200, 255));
		dc.FillRect(&rc, pBrush );
/*
.....set default pen color
*/
		CPen *aPen = new CPen(PS_SOLID, 1, RGB(0,0,0));
/*
.....save old pen and select default pen for draw
*/
		CPen* oldPen = dc.SelectObject(aPen);

		CFont aFont;
		aFont.CreatePointFont(80,"MS Serif", NULL);

		dc.SelectObject(&aFont );
		intpt = 0;
		end = 0;
draw:	
		switch (cut_array[intpt])
		{
		case 1: 
/*
.....MoveTo
*/
			x = cut_array[intpt+1];
			y = cut_array[intpt+2];
			dc.MoveTo(x,y);
			current_x = x;
			current_y = y;
			intpt = intpt + 3;
			break;
		case 2:
/*
.....LineTo
*/
			x = cut_array[intpt+1];
			y = cut_array[intpt+2];
			dc.LineTo(x,y);
			current_x = x;
			current_y = y;
			intpt = intpt + 3;
			break;
		case 3:
/*
.....arrow
*/
			double ang0, ang1, ang2, temp;
			int down;
			x1 = current_x;
			y1 = current_y;
			x2 = cut_array[intpt+1];
			y2 = cut_array[intpt+2];
			if (y2!=y1)
			{
				down = abs(y2-y1)/(y2-y1);
				temp = (double)(x2-x1)/(double)(y2-y1);
				ang0 = atan(temp);
			}
			else if (x2==x1)
			{
				intpt = intpt + 3;
				break;
			}
			else
			{
				down = abs(x2-x1)/(x2-x1);
				ang0 = PI/2;
			}
			ang1 = ang0 + Arrow_Angle;
			ang2 = Arrow_Angle - ang0;
			y3 = (int)(y2 - down*Arrow_Len*cos(ang1));
			x3 = (int)(x2 - down*Arrow_Len*sin(ang1));
			y4 = (int)(y2 - down*Arrow_Len*cos(ang2));
			x4 = (int)(x2 + down*Arrow_Len*sin(ang2));
		
			dc.LineTo(x2,y2);
			dc.LineTo(x3, y3);
			dc.MoveTo(x2, y2);
			dc.LineTo(x4, y4);
			dc.MoveTo(x2, y2);
			current_x = x2;
			current_y = y2;
			intpt = intpt + 3;
			break;
		case 4:
/*
......Draw Circle
*/
			x0 = cut_array[intpt+1];
			y0 = cut_array[intpt+2];
			r = cut_array[intpt+3];
			x1 = x0 - r;
			y1 = y0 - r;
			x2 = x0 + r;
			y2 = y0 + r;
			dir = cut_array[intpt+4];
			x3 = current_x;
			y3 = current_y;
			x4 = cut_array[intpt+5];
			y4 = cut_array[intpt+6];
			current_x = x4;
			current_y = y4;
			if (dir==1)
				dc.SetArcDirection(AD_COUNTERCLOCKWISE);
			else
				dc.SetArcDirection(AD_CLOCKWISE);
			dc.Arc(x1, y1, x2, y2, x3, y3, x4, y4);
			intpt = intpt + 7;
			break;
		case 6:
			red = cut_array[intpt+1];
			green = cut_array[intpt+2];
			blue = cut_array[intpt+3];
			dc.SetTextColor(RGB(red, blue, green));
			intpt = intpt + 4;
			break;
		case 5:
			n = cut_array[intpt+1];
			textstr = (char*)cut_array[intpt+2];
			dc.TextOut(current_x,current_y, textstr, n);
			intpt = intpt + 3;
			break;
		case 8: 
			if (cut_array[intpt+1]==0)
				current_style = PS_SOLID;
			else if (cut_array[intpt+1]==1)
				current_style = PS_DASH;
			else if (cut_array[intpt+1]==2)
				current_style = PS_DOT;
			else if (cut_array[intpt+1]==3)
				current_style = PS_DASHDOT;
			else if (cut_array[intpt+1]==4)
				current_style = PS_DASHDOTDOT;
			else
				current_style = PS_SOLID;
			intpt = intpt + 2;
			if (aPen!=NULL)
/*
.....release this pen and delete this pen, then set new pen
*/
			{
				dc.SelectObject(oldPen);
				delete aPen;
			}
			aPen = new CPen(current_style, current_width, current_color);
			oldPen = dc.SelectObject(aPen);
			break;
		case 9:
			current_width = cut_array[intpt+1];
			intpt = intpt + 2;
			if (aPen!=NULL)
/*
.....release this pen and delete this pen, then set new pen
*/
			{
				dc.SelectObject(oldPen);
				delete aPen;
			}
			aPen = new CPen(current_style, current_width, current_color);
			oldPen = dc.SelectObject(aPen);
			break;
		case 10:
			current_color = RGB(cut_array[intpt+1], cut_array[intpt+2], cut_array[intpt+3]);
			intpt = intpt + 4;
			if (aPen!=NULL)
/*
.....release this pen and delete this pen, then set new pen
*/
			{
				dc.SelectObject(oldPen);
				delete aPen;
			}
			aPen = new CPen(current_style, current_width, current_color);
			oldPen = dc.SelectObject(aPen);
			break;
		case 99:
			end = 1;
			break;
		}
		if (end!=1)
			goto draw;
/*
.....reset pen back
*/
		dc.SelectObject(oldPen);
		delete aPen;
		delete pBrush;
	}
}
/***********************************************************************
c
c   SUBROUTINE:  initform(UD_FSTRUCT *fstruct, UD_FDATA *fdata)
c
c   FUNCTION:  This function initialize class data from form structure and data
c
c   INPUT:  
c			UD_FSTRUCT *fstruct: form structure
c			UD_FDATA *fdata: form data
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::initform(UD_FSTRUCT *fstruct, UD_FDATA *fdata)
{
	int i, j, len, x,y,wid,twid,hgt, pwid, phgt,posx,posy,inpwid, istat;
	UINT fid, tempid;
	int butlen, chrwid, pwid2;
	char perc_str[10], tempstr[40], msg[400];
	BYTE  ktyp;
	WORD  kkey;
/*
.....header info
*/
	strcpy_s(m_title,sizeof(m_title), (*fstruct).ud_frmdf.ud_fid);
	m_TitleCaption = m_title;
/*
.....if the parent window is IPV window, we don't want the position at the 
.....center of IPV window but at the center of main window. Not working
*/
/*	if (m_pParent==Pocket_Win[UM_IPV_WINDOW])
	{
		uw_ntget_ctrpos(&x, &y, 0);
		m_dlgTempl.x = x;
		m_dlgTempl.y = y;
	}
	else
*/	{
		m_dlgTempl.x = 0;
		m_dlgTempl.y = 0;
	}
	m_dlgTempl.cx = (short)((*fstruct).ud_frmdf.ud_frc.ud_c*uw_form_scalex) +FORMTOPX;
	m_dlgTempl.cy = (short)((*fstruct).ud_frmdf.ud_frc.ud_r*uw_form_scaley) +FORMTOPY;
/*
.....label item info
*/
	m_itemnum = 0;
	m_fldno = fstruct->n_input_fields;
	m_dspno = fstruct->n_display_fields;
	m_framno = fstruct->n_frame_fields;
	m_picno = fstruct->n_picture_fields;
	for (i=0; i<fstruct->n_display_fields; i++)
	{
		fid = IDC_FORMITEM1 + m_itemnum;  
		strcpy_s(m_dsplabel[i], sizeof(m_dsplabel[i]),fstruct->display_flds[i].message);
		x = (int)(fstruct->display_flds[i].pos.ud_c*uw_form_scalex);
		y = (int)(fstruct->display_flds[i].pos.ud_r*uw_form_scaley);
		uw_ntget_strsize(m_dsplabel[i], 80, "MS Sans Serif", &wid, &hgt);
		m_rgDlgItem[m_itemnum++].Inittemp(m_dsplabel[i], 4, 
						fid, x+FORMTOPX, y+FORMTOPY, wid, hgt);
		m_idtable[i] = fid;
	}
	for (i=0; i<fstruct->n_input_fields; i++)
	{
		fid = IDC_FORMITEM1 + m_itemnum;
		strcpy_s(m_fldlabel[i], sizeof(m_fldlabel[i]),fstruct->input_flds[i].prompt);
		x = (int)(fstruct->input_flds[i].ud_prmloc.ud_c*uw_form_scalex);
		y = (int)(fstruct->input_flds[i].ud_prmloc.ud_r*uw_form_scaley);
		wid = (int)(fstruct->input_flds[i].ud_fldloc.ud_c*uw_form_scalex);
		hgt = (int)(fstruct->input_flds[i].ud_fldloc.ud_r*uw_form_scaley);
		if (fstruct->input_flds[i].toggle == 6)
			m_type[i] = 1;  
		else if (fstruct->input_flds[i].toggle == 2)
			m_type[i] = 2;
		else if (fstruct->input_flds[i].toggle == 1)
			m_type[i] = 3;
		else if ((fstruct->input_flds[i].toggle == 5)
			&& (fstruct->input_flds[i].n_defaults == 0))
			m_type[i] = 5;
		else if (fstruct->input_flds[i].toggle == 17)
			m_type[i] = 17;
		else if (fstruct->input_flds[i].toggle == 19)
			m_type[i] = 19;
		else if (fstruct->input_flds[i].toggle == 3)
			m_type[i] = 7;
		else if ((fstruct->input_flds[i].toggle == 5)
			&& (fstruct->input_flds[i].n_defaults == 1))
			m_type[i] = 8;
/*
.....we will use COMBLIST_DROPDOWN
*/
		else if ((fstruct->input_flds[i].toggle == 5)
			&& (fstruct->input_flds[i].n_defaults == 2))
			m_type[i] = 9;
		else if (fstruct->input_flds[i].toggle == 7)
			m_type[i] = 10;
		else if (fstruct->input_flds[i].toggle == 8)
			m_type[i] = 11;
		else if (fstruct->input_flds[i].toggle == 9)
			m_type[i] = 13;
		else if (fstruct->input_flds[i].toggle == 10)
			m_type[i] = 15;
		else if (fstruct->input_flds[i].toggle == 11)
			m_type[i] = 16;
		else if (fstruct->input_flds[i].toggle == 18)
			m_type[i] = 18;
		else if (fstruct->input_flds[i].toggle == 23)
			m_type[i] = 23;
		else if (fstruct->input_flds[i].toggle == 24)
			m_type[i] = 24;
		m_datatype[i] = fstruct->input_flds[i].ud_datatyp;
/*
.....input filed
*/
		if ((m_type[i]==3) || (m_type[i]==5) || (m_type[i]==15))
		{
			m_input[i] = fstruct->input_flds[i].ud_input;
			m_pmodal[i] = fstruct->input_flds[i].ud_modal;
			for (j=0;j<UD_NMENTWD;j++)
				m_pick_mask[i][j] = fstruct->input_flds[i].ud_limit[j];
		}
/*
......choices
*/
		if ((m_type[i]==2)|| ((m_type[i]==18)&&(fstruct->input_flds[i].n_defaults>0)))
		{
			m_formchcnum[i] = fstruct->input_flds[i].n_defaults;
			m_formchoice[i] = (char**)malloc(
				(m_formchcnum[i])*sizeof(char*));
			for (j=0; j<fstruct->input_flds[i].n_defaults; j++)
			{
				len = strlen(fstruct->input_flds[i].defaults[j].dstr);
				m_formchoice[i][j] = (char*)malloc((len+1)*sizeof(char));
				strcpy_s(m_formchoice[i][j], len+1,fstruct->input_flds[i].defaults[j].dstr);
			}
		}
/*
.....LEN
*/
		m_len[i] = fstruct->input_flds[i].ud_flen;
/*
.....PREC
*/
		m_prec[i] = fstruct->input_flds[i].ud_fprec;
/*
.....Range
*/
		m_range[i][0] = fstruct->input_flds[i].range[0];
		m_range[i][1] = fstruct->input_flds[i].range[1];

		if ((m_type[i]==2)||(m_type[i]==8)
			||(m_type[i]==9)||(m_type[i]==10) ||(m_type[i]==13)
			||(m_type[i]==14) || (m_type[i] == 15)||(m_type[i]==16))
		{
/*
......we will going to use x,y as the prompt label position
......and wid. hgt as whole prompt and editbox width as height
......editbox beginning at the end of the prompt label if user 
......doesm't have option pos[2], pos[3].
*/
			if (m_fldlabel[i][0]!='\0')
				uw_ntget_strsize(m_fldlabel[i], 80, "MS Sans Serif", &pwid, &phgt);
			else
			{
/*
.....we will use phgt
*/
				uw_ntget_strsize("X", 80, "MS Sans Serif", &chrwid, &phgt);
				pwid = 0;
			}
/*
.....the calculate string size is little different 
.....from displaying, so it may overlap the input field,
.....so we were not allow "overlap" happened, so, just
.....make label short if label overlap the input field
*/
			if (fstruct->input_flds[i].ud_prmloc.x!=-1)
			{
				if (x+pwid>fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex)
					pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x -1);
			}
			m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 4, 
						fid, x+FORMTOPX, y+FORMTOPY, pwid, phgt);	
			fid = IDC_FORMITEM1 + m_itemnum;
			if (fstruct->input_flds[i].ud_prmloc.x==-1)
			{
				posx = x+pwid;
				inpwid = wid-pwid;
			}
			else
			{
				posx = (int)((fstruct->input_flds[i].ud_prmloc.x)*uw_form_scalex);
				inpwid = wid-(posx-x);
			}
			if (fstruct->input_flds[i].ud_prmloc.y==-1)
				posy = y;
			else
				posy = (int)(fstruct->input_flds[i].ud_prmloc.y*uw_form_scaley);
/*
.....at least display 5 item + 1 select item display
*/
			if ((phgt*6>hgt)&&(m_type[i]==2))
				hgt = phgt*6;
			if ((m_type[i]!=10) && (m_type[i]!=13))
				m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
							fid, posx+FORMTOPX, posy+FORMTOPY, inpwid, hgt);
			else if (m_type[i]==9)
			{
				twid = (int)(abs(m_len[i]) * TEXT_WID * uw_form_scalex + 6);
				m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
							fid, posx, posy, twid, hgt);
			}
			else if (m_type[i]==13)
			{
				if (fstruct->input_flds[i].ud_datatyp==UD_DASINT)
				{
					sprintf_s(perc_str, sizeof(perc_str),"%d%%", 100);
					uw_ntget_strsize(perc_str, 80, "MS Sans Serif", &pwid2, &phgt);
					m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
							fid, posx+FORMTOPX, posy+FORMTOPY, inpwid-pwid2-1, hgt);
				}
				else
					m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
							fid, posx+FORMTOPX, posy+FORMTOPY, inpwid, hgt);
			}
			else
				m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
							fid, posx+FORMTOPX, posy+FORMTOPY, wid, hgt);		
		}
		else if  (m_type[i]==23)
		{
			uw_ntget_strsize(m_fldlabel[i], 80, "MS Sans Serif", &pwid, &phgt);
			if (fstruct->input_flds[i].ud_prmloc.x!=-1)
			{
				pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x - 1);
			}
			if (fstruct->input_flds[i].ud_prmloc.x==-1)
				butlen = (int)(pwid*1.1);
			else
			{
				butlen = (int)(pwid*0.95);
			}
			tempid = IDD_FORM_TEMPBUT1 + i;
			m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 1, 
						tempid, x+FORMTOPX, y+FORMTOPY, butlen, hgt);	
		
			fid = IDC_FORMITEM1 + m_itemnum;
			twid = (int)(abs(m_len[i]) * TEXT_WID * uw_form_scalex + 6);
			if (fstruct->input_flds[i].ud_prmloc.x==-1)
			{
				posx = (int)(x+pwid*1.2);
				inpwid = twid;
			}
			else
			{
				posx = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex);
				inpwid = twid;
			}
			if (fstruct->input_flds[i].ud_prmloc.y==-1)
				posy = y;
			else
				posy = (int)((fstruct->input_flds[i].ud_prmloc.y)*uw_form_scaley);

			m_rgDlgItem[m_itemnum++].Inittemp("", 1, 
						fid, posx+FORMTOPX, posy+FORMTOPY, inpwid, hgt);
		}
/*
......if it is EDIT, we check if user want label or "pick", "locate" button
*/
		else if ((m_type[i]==3)||(m_type[i]==11))
		{
			uw_ntget_strsize(m_fldlabel[i], 80, "MS Sans Serif", &pwid, &phgt);

/*
.....the calculate string size is little different 
.....from displaying, so it may overlap the input field,
.....so we were not allow "overlap" happened, so, just
.....make label short if label overlap the input field
*/
			if (fstruct->input_flds[i].ud_prmloc.x!=-1)
			{
				pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x - 1);
			}

			if (m_input[i]==FORM_STRING)
			{
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 4, 
						fid, x+FORMTOPX, y+FORMTOPY, pwid, phgt);	
			}
			else if (m_input[i]==FORM_PICK || m_input[i]==FORM_LABEL ||
				m_input[i]==FORM_SUBSCR)
			{
				if (fstruct->input_flds[i].ud_prmloc.x==-1)
					butlen = (int)(pwid*1.1);
				else
				{
					butlen = (int)(pwid*0.95);
				}
/*
.....we use color button instead of normal button, so here we set ID = tempid
.....and set up a button here, but we will replace this button with colorbutton
.....with seem pos/size and the real ID = fid
*/
//				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 1, 
//						fid, x+FORMTOPX, y+FORMTOPY, butlen, hgt);	
				tempid = IDD_FORM_TEMPBUT1 + i;
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 1, 
						tempid, x+FORMTOPX, y+FORMTOPY, butlen, hgt);
			}
			else if (m_input[i]==FORM_LOCATE)
			{
				if (fstruct->input_flds[i].ud_prmloc.x==-1)
					butlen = (int)(pwid*1.1);
				else
				{
					butlen = (int)(pwid*0.95);
				}
/*
.....we use color button instead of normal button, so here we set ID = tempid
.....and set up a button here, but we will replace this button with colorbutton
.....with seem pos/size and the real ID = fid
*/
//				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 1, 
//						fid, x+FORMTOPX, y+FORMTOPY, butlen, hgt);	
				tempid = IDD_FORM_TEMPBUT1 + i;
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 1, 
						tempid, x+FORMTOPX, y+FORMTOPY, butlen, hgt);	
			}			
			fid = IDC_FORMITEM1 + m_itemnum;
/*
.....use ud_len value to set text field len
.....but max wid-pwid
*/
			twid = (int)(abs(m_len[i]) * TEXT_WID * uw_form_scalex + 6);
			if (fstruct->input_flds[i].ud_prmloc.x==-1)
			{
				if (m_input[i]==FORM_STRING)
					posx = x+pwid;
				else
					posx = (int)(x+pwid*1.2);
				inpwid = twid;
			}
			else
			{
				posx = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex);
				inpwid = twid;
			}
			if (fstruct->input_flds[i].ud_prmloc.y==-1)
				posy = y;
			else
				posy = (int)((fstruct->input_flds[i].ud_prmloc.y)*uw_form_scaley);

			m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
						fid, posx+FORMTOPX, posy+FORMTOPY, inpwid, hgt);

			DWORD dwStyle = m_rgDlgItem[m_itemnum-1].m_dlgItemTemplate.style;
/*
.....we default at LEFT when set up item type
*/
			if (fstruct->input_flds[i].justified==2)
			{
/*
.....Centered
*/
				dwStyle &= ~ES_LEFT;
				dwStyle |= ES_CENTER;
			}
			if (fstruct->input_flds[i].justified==1)
			{
/*
.....RIGHT
*/
				dwStyle &= ~ES_LEFT;
				dwStyle |= ES_RIGHT;
			}
			m_rgDlgItem[m_itemnum-1].m_dlgItemTemplate.style = dwStyle;
		}
		else if (m_type[i]==18)
		{
			uw_ntget_strsize(m_fldlabel[i], 80, "MS Sans Serif", &pwid, &phgt);
			if (fstruct->input_flds[i].ud_prmloc.x!=-1)
			{
				pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x - 1);
			}
			m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 4, 
						fid, x, y, pwid, phgt);	
			fid = IDC_FORMITEM1 + m_itemnum;
			if (fstruct->input_flds[i].ud_prmloc.x==-1)
			{
				posx = x+pwid;
				inpwid = wid-pwid;
			}
			else
			{
				posx = (int)((fstruct->input_flds[i].ud_prmloc.x)*uw_form_scalex);
				inpwid = wid-(posx-x);
			}
			if (fstruct->input_flds[i].ud_prmloc.y==-1)
				posy = y;
			else
				posy = (int)(fstruct->input_flds[i].ud_prmloc.y*uw_form_scaley);
				
			tempid = IDD_FORM_TEMPBUT1 + i;
			m_rgDlgItem[m_itemnum++].Inittemp("", 1, 
						tempid, posx, posy, inpwid, hgt);	
		}
		else if (m_type[i]==24)
		{
			uw_ntget_strsize(m_fldlabel[i], 80, "MS Sans Serif", &pwid, &phgt);
			if (fstruct->input_flds[i].ud_prmloc.x!=-1)
			{
				pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x - 1);
			}
			m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 4, 
						fid, x, y, pwid, phgt);	
			fid = IDC_FORMITEM1 + m_itemnum;
			if (fstruct->input_flds[i].ud_prmloc.x==-1)
			{
				if (fstruct->input_flds[i].justified==0)
				{
					posx = x+pwid;
					inpwid = wid-pwid;
				}
				else
				{
					posx = x+pwid;
					inpwid = 10;
				}
			}
			else
			{
				if (fstruct->input_flds[i].justified==0)
				{
					posx = (int)((fstruct->input_flds[i].ud_prmloc.x)*uw_form_scalex);
					inpwid = wid-(posx-x);
				}
				else
				{
					posx = (int)((fstruct->input_flds[i].ud_prmloc.x)*uw_form_scalex);
					inpwid = 10;
				}
			}
			if (fstruct->input_flds[i].ud_prmloc.y==-1)
			{
				if (fstruct->input_flds[i].justified==0)
					posy = y;
				else
					posy = y + phgt/2;
			}
			else
			{
				if (fstruct->input_flds[i].justified==0)
					posy = (int)(fstruct->input_flds[i].ud_prmloc.y*uw_form_scaley);
				else
					posy = (int)(fstruct->input_flds[i].ud_prmloc.y*uw_form_scaley) + phgt/2;
			}
/*
.....here create a temp window to have the position here
*/
			tempid = IDD_FORM_TEMPSLIDER1 + i;
			m_rgDlgItem[m_itemnum++].Inittemp("", 1, 
						tempid, posx, posy, inpwid, hgt);	
		}
		else
		{
/*
.....we use color button instead of normal button, so here we set ID = tempid
.....and set up a button here, but we will replace this button with colorbutton
.....with seem pos/size and the real ID = fid
*/
			HTHEME tdata = OpenThemeData(m_hWnd, L"Button");
			if (m_type[i]==1)
			{
				tempid = IDD_FORM_TEMPBUT1 + i;
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], m_type[i], 
						tempid, x+FORMTOPX, y+FORMTOPY, wid, hgt);
			}
			else if ((tdata!=NULL)&&(m_type[i]==7))
			{
				tempid = IDD_FORM_TEMPBUT1 + i;
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], m_type[i], 
						tempid, x+FORMTOPX, y+FORMTOPY, wid, hgt);
			}
			else if ((m_type[i]==17)||(m_type[i]==19))
			{
				tempid = IDD_FORM_TEMPLIST1 + i;
/*
.....create normal listbox first to have the right pos and size
*/
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 5, 
						tempid, x+FORMTOPX, y+FORMTOPY, wid, hgt);
			}
			else
			{
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], m_type[i], 
						fid, x+FORMTOPX, y+FORMTOPY, wid, hgt);
			}
			if ((m_type[i]==1) && (fstruct->input_flds[i].shortcut[0]!='\0'))
/*
......push button
*/
			{
				strcpy_s(tempstr, sizeof(tempstr),fstruct->input_flds[i].shortcut);
				istat = uz_ntparse_frmkey(tempstr, strlen(tempstr), &ktyp, &kkey);
				if (istat != UU_SUCCESS)
				{
					sprintf_s(msg,sizeof(msg),"Key definition syntax error of button. %s",tempstr);
					ud_wrerr(msg);
				}
				else
				{
					m_form_accel[m_form_accelnum].cmd = fid;
					m_form_accel[m_form_accelnum].fVirt = ktyp;
					m_form_accel[m_form_accelnum].key = kkey;
					m_form_accelnum++;
				}
			}
		}
		m_idtable[i+m_dspno] = fid;
		if (m_type[i]==13)
/*
......progress bar, need a % string after it
*/
		{
			if (fstruct->input_flds[i].ud_datatyp==UD_DASINT)
			{
				fid = IDC_FORMITEM1 + m_itemnum;
				m_rgDlgItem[m_itemnum++].Inittemp(perc_str, 4, 
						fid, (posx+FORMTOPX +inpwid-pwid2), posy+FORMTOPY, pwid2, phgt);	
				m_idtable[i+m_dspno+m_fldno+m_framno+m_picno] = fid;
			}
		}
		if (m_type[i]==24)
/*
......slider bar, need a value string after it
*/
		{
			if (fstruct->input_flds[i].ud_datatyp==UD_DASINT)
			{
				fid = IDC_FORMITEM1 + m_itemnum;
				sprintf_s(perc_str, sizeof(perc_str), "%d%%", 100);
				uw_ntget_strsize(perc_str, 80, "MS Sans Serif", &pwid2, &phgt);
				if (fstruct->input_flds[i].justified==0)
					m_rgDlgItem[m_itemnum++].Inittemp(perc_str, 4, 
						fid, (posx +inpwid-pwid2+1), posy, pwid2, phgt);	
				else
					m_rgDlgItem[m_itemnum++].Inittemp(perc_str, 4, 
						fid, (posx+10), posy+hgt, pwid2, phgt);	
				m_idtable[i+m_dspno+m_fldno+m_framno+m_picno] = fid;
			}
		}
	}
	for (i=0; i<fstruct->n_frame_fields; i++)
	{
		fid = IDC_FORMITEM1 + m_itemnum;  
		strcpy_s(m_frame_title[i], sizeof(m_frame_title[i]),fstruct->frame_flds[i].title);
		x = (int)(fstruct->frame_flds[i].x*uw_form_scalex);
		y = (int)(fstruct->frame_flds[i].y*uw_form_scaley);
		wid = (int)(fstruct->frame_flds[i].cx*uw_form_scalex);
		hgt = (int)(fstruct->frame_flds[i].cy*uw_form_scaley);
		m_rgDlgItem[m_itemnum++].Inittemp(m_frame_title[i], 12, 
						fid, x+FORMTOPX, y+FORMTOPY, wid, hgt);
		m_idtable[i+m_fldno+m_dspno] = fid;
	}
	for (i=0; i<fstruct->n_picture_fields; i++)
	{
		fid = IDC_FORMTEMPPIC + i; 
		x = (int)(fstruct->picture_flds[i].x*uw_form_scalex);
		y = (int)(fstruct->picture_flds[i].y*uw_form_scaley);
		wid = (int)(fstruct->picture_flds[i].cx*uw_form_scalex);
		hgt = (int)(fstruct->picture_flds[i].cy*uw_form_scaley);
		m_rgDlgItem[m_itemnum++].Inittemp("", 12, 
						fid, x, y, wid, hgt);
		m_idtable[i+m_fldno+m_dspno+m_framno] = IDC_FORMITEM1 + m_itemnum;
	}
	if (m_disptype==1)
	{
		if (fstruct->helpflag==1)
			m_helpact = 1;
	}
	m_dlgTempl.style = WS_CHILD | DS_SETFONT;
	m_dlgTempl.dwExtendedStyle = 0;
	m_dlgTempl.cdit = m_itemnum;
}

/***********************************************************************
c
c   SUBROUTINE:  CreateFormBar()
c
c   FUNCTION:  This function Create NCL Form
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

BOOL CNCLFormBar::CreateFormBar()
{
	UINT type;
	int stat;
	DWORD dwStyle;
		
	type = WS_CHILD | WS_VISIBLE | CBRS_SIZE_FIXED | CBRS_FLYBY
						 | CBRS_TOOLTIPS | CBRS_TOP ;
	stat = CreateFormWin(m_pParent, m_rgDlgItem, m_dlgTempl, m_itemnum, type, IDD_FORMBAR+m_frmid);
	if (stat==-1)
		return stat;
	if (m_modal)
	{
			EnableDocking(0);	
			CPoint pt(50,50);
			NCL_MainFrame->FloatControlBar(this, pt);	
			NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
			NCL_MainFrame->RecalcLayout();

			DWORD dwFlags = MLF_SHOWONIDLE;
			VERIFY(RunModalLoop(dwFlags) == m_nModalResult);
		::EnableWindow(NCL_MainFrame->GetSafeHwnd(), TRUE);
		if (::GetActiveWindow() == m_hWnd)
			::SetActiveWindow(NCL_MainFrame->GetSafeHwnd());
		CWinApp* pApp = AfxGetApp();
		if (pApp != NULL)
			pApp->EnableModeless(TRUE);
		return m_nModalResult;
	}
	if (m_disptype==1)
	{
/*
.....dockable in all
*/
		if ((m_fStruct.dockable[0]!=0) 
			|| (m_fStruct.dockable[1]!=0)
			|| (m_fStruct.dockable[2]!=0)
			|| (m_fStruct.dockable[3]!=0))
		{
			if ((m_fStruct.dockable[0]==0)
				&& (m_fStruct.dockable[1]==0)
				&& (m_fStruct.dockable[2]==0)
				&& (m_fStruct.dockable[3]==0))
			{
				EnableDocking(0);
				NCL_MainFrame->EnableDocking(0);
			}
			else if ((m_fStruct.dockable[0]==1)
				&& (m_fStruct.dockable[1]==1)
				&& (m_fStruct.dockable[2]==1)
				&& (m_fStruct.dockable[3]==1))
			{
				EnableDocking(CBRS_ALIGN_ANY);
				NCL_MainFrame->EnableDocking(CBRS_ALIGN_ANY);
			}
			else 
			{
				dwStyle = 0;
				if (m_fStruct.dockable[0]==1)
					dwStyle |= CBRS_ALIGN_TOP;
				if (m_fStruct.dockable[1]==1)
					dwStyle |= CBRS_ALIGN_BOTTOM;
				if (m_fStruct.dockable[2]==1)
					dwStyle |= CBRS_ALIGN_LEFT;
				if (m_fStruct.dockable[3]==1)
					dwStyle |= CBRS_ALIGN_RIGHT;
				EnableDocking(dwStyle);
				NCL_MainFrame->EnableDocking(dwStyle);
			}
			NCL_MainFrame->EnableDocking(CBRS_ALIGN_ANY);
			if (m_fStruct.win_area[0]=='\0')
			{
/*
.....we always center the form, so ingore the positon
*/
/*
				CPoint pt((int)(m_fStruct.ud_frmdf.ud_fwin.ll.x), (int)(m_fStruct.ud_frmdf.ud_fwin.ll.y));
*/
				CPoint pt(0,0);
				NCL_MainFrame->FloatControlBar(this, pt);	
				NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
				NCL_MainFrame->RecalcLayout();

				CRect rcDlg;
				GetWindowRect(&rcDlg);				
				CRect rcArea;
				CRect rcCenter;
				NCL_MainFrame->GetWindowRect(&rcCenter);	
				int xLeft = (rcCenter.left + rcCenter.right) / 2 - rcDlg.Width() / 2;
				int yTop = (rcCenter.top + rcCenter.bottom) / 2 - rcDlg.Height() / 2;
				CPoint pt2(xLeft,yTop);
				NCL_MainFrame->FloatControlBar(this, pt2);	
				NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
				NCL_MainFrame->RecalcLayout();
			}
			else
			{
				CRect rect;
				int dockable;
				if (UW_reset_menu==1)
					dockable = UDM_AREA_ENDDOCK;
				else
					ud_get_areadockable(m_fStruct.win_area, &dockable);
				uw_ntget_menurect(m_fStruct.dock_dir, &rect, dockable);
				if (dockable==UDM_AREA_NODOCK)
/*
......replace/hide the control bars inside this area
*/
					uw_nthide_bars(m_fStruct.dock_dir);
				if (m_fStruct.dock_dir==1)
					NCL_MainFrame->DockControlBar(this, AFX_IDW_DOCKBAR_TOP, &rect);
				else if (m_fStruct.dock_dir==2)
					NCL_MainFrame->DockControlBar(this, AFX_IDW_DOCKBAR_BOTTOM, &rect);
				else if (m_fStruct.dock_dir==3)
					NCL_MainFrame->DockControlBar(this, AFX_IDW_DOCKBAR_LEFT, &rect);
				else if (m_fStruct.dock_dir==4)
					NCL_MainFrame->DockControlBar(this, AFX_IDW_DOCKBAR_RIGHT, &rect);
			}
		}
		else
		{
			CPoint pt((int)(m_fStruct.ud_frmdf.ud_fwin.ll.x), (int)(m_fStruct.ud_frmdf.ud_fwin.ll.y));
			NCL_MainFrame->FloatControlBar(this, pt);	
		}
		NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
		NCL_MainFrame->RecalcLayout();
	}
	return stat;
}

/***********************************************************************
c
c   SUBROUTINE:  FormCancel
c   FUNCTION:  This function called when "Cancel" button pushed
c				it destroy and remove child window and itself
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CNCLFormBar::FormCancel() 
{
	ud_delform(&m_fStruct);
	uu_free((char *)m_fData.ud_data);
/*
.....Hide for modal form now, the delete form bar function will destroy the window later
*/
	if (m_disptype==0)
	{
		if (m_nFlags & (WF_MODALLOOP|WF_CONTINUEMODAL))
			EndModalLoop(IDCANCEL);
	}
	m_visible = 0;
	NCL_MainFrame->ShowControlBar(this, FALSE, TRUE);
	NCL_MainFrame->RecalcLayout();
	if (m_disptype)
		delete this;
	uw_ntreset_redraw();
}
/***********************************************************************
c
c   SUBROUTINE:  FormClose
c   FUNCTION:  This function close the NCL formbar
c				it destroy and remove child window and itself
c
c   INPUT:  none
c   OUTPUT: none
c	RETURN: none
c***********************************************************************
*/
void CNCLFormBar::FormClose()
{
	OnClose();
}

/***********************************************************************
c
c   SUBROUTINE:  addchild(int formID)
c   FUNCTION:  This function added a form to its child list
c				
c
c   INPUT:  formID: form to be added
c   OUTPUT: none
c	RETURN: none
c***********************************************************************
*/
void CNCLFormBar::addchild(int formID)
{
	m_childfrm[m_childnum] = formID;
	m_childnum++;
}

/***********************************************************************
c
c   SUBROUTINE:  removechild(int formID)
c   FUNCTION:  This function remove a form from its child list
c				
c
c   INPUT:  formID: form to be removed
c   OUTPUT: none
c	RETURN: none
c***********************************************************************
*/
void CNCLFormBar::removechild(int formID)
{
	int i, rmnum;
	rmnum = -1;
	for (i=0; i<m_childnum;i++)
	{
		if (m_childfrm[i] == formID)
		{
			m_childfrm[i] = -1;
			rmnum = i;
			break;
		}
	}
	if (rmnum==-1)
		return;
	for (i=rmnum; i<m_childnum-1;i++)
	{
		m_childfrm[i] = m_childfrm[i+1];
	}
	m_childnum--;
}

/***********************************************************************
c
c   FUNCTION: OnClose()
c
c           callback for close formbar window.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormBar::OnClose() 
{
/*
.....close child form first
*/
	if (m_childnum!=0)
	{
		for (int i=m_childnum-1; i>=0;i--)
			uw_ntclose_dispfrm(m_childfrm[i]);
	}
	m_childnum = 0;
/*
.....remove this form from parent form's child list child
*/
	if (m_parentID!=-1)
	{
		if (UW_display_frmwin[m_parentID]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[m_parentID]->removechild(m_frmid);
		else
			UW_display_frm[m_parentID]->removechild(m_frmid);
		m_parentID = -1;
	}
/*
.....We need save the data if close in display type form
*/
	if (NCL_form_SaveData())
	{
		if (m_method!=NULL && m_method[m_fldno] != NULL)
		{
/*
.....reset pick segment list before callback function
*/
			ncl_reset_pikgeom();
			(m_method[m_fldno])(&m_frmid, NULL,UD_DONE);
		}
		UD_dispfrm[m_frmid] = NULL;
		UD_dispfdata[m_frmid] = NULL;
/*
......free form structure & data space
*/
		ud_delform(&m_fStruct);
		uu_free((char *)m_fData.ud_data);
		m_visible = 0;
		uw_ntreset_redraw();
		NCL_MainFrame->ShowControlBar(this, FALSE, TRUE);
		NCL_MainFrame->RecalcLayout();
//		uw_ntreset_redraw();
	}
	if (m_helptext != NULL)
	{
		free(m_helptext);
		m_helptext = NULL;
	}
	if (m_helpbox!=NULL)
	{
/*
.....destroy the dialog first before delete, the OnCancel function by
.....'X' button on help dialog only invisible the window but not destroy
.....because the help dialog is a modaless dialog
......even though the delete function will destroy the dialog but call 
......DestroyWindow inside delete function will miss some callbacks for destroywindow.
*/
		m_helpbox->DestroyWindow();
		delete m_helpbox;
		m_helpbox = NULL;
	}
	delete this;
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

void CNCLFormBar::PostNcDestroy() 
{
	int i;
	delete m_pEditBkBrush;
	for (i=0; i<MAX_FORMITEM; i++)
	{
		if (m_BkBrush[i]!=NULL)
			delete m_BkBrush[i];
		if (m_pBkBrush[i]!=NULL)
			delete m_pBkBrush[i];
		if (m_dBkBrush[i]!=NULL)
			delete m_dBkBrush[i];
	}
	for (i=0; i<20; i++)
	{
		if (m_fBkBrush[i]!=NULL)
			delete m_fBkBrush[i];
	}

	if (m_frmid!=0)
	{
		if (NCL_MainFrame!=NULL)
			NCL_MainFrame->m_formbar[m_frmid] = NULL;
		UW_display_frmwin[m_frmid] = NULL;
	}
}

/***********************************************************************
c
c   SUBROUTINE:  FormPick
c
c   FUNCTION:  This function called when "Pick" button pushed.
c				
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
int CNCLFormBar::FormPick(int fldno)
{
	char lstr[82];
	char prompt[256];
/*
.....Initialize routine
*/
	int i,j,markval,status,lmtgeo;
	if (LW_nclipv==LW_STANDALONE) return(DE_DONE);
	i = fldno;
	UINT fid = m_idtable[i+m_dspno];
	strcpy_s(prompt, sizeof(prompt), m_fldlabel[i]);
/*
.....initialize the string
*/
	GetDlgItem(fid)->GetWindowText(lstr, 80);
/*
.....Take down form
*/
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
	::SetActiveWindow(MainWin->GetSafeHwnd());
	NCL_MainFrame->ShowControlBar(this, FALSE, FALSE);
	NCL_MainFrame->RecalcLayout();
	uw_ntreset_redraw();
/*
.....set UD_form_bypick = 1
.....to let ud_gevt1 know we don't
.....need "BY TEXT" active
*/
	UD_form_bypick = 1;	
/*
.....Trap Reject Op
*/
	UD_MARK(markval,UU_TRUE);
	if (markval != 0) 	
	{
		status = DE_DONE;
		goto done;
	}
	while (1)
	{
		lmtgeo = UU_FALSE;
		for (j=0;j<UD_NMENTWD;j++)
		{
			if (m_pick_mask[i][j] != 0)
			{
				ud_lgeo(UU_TRUE,m_pick_mask[i]);
				lmtgeo = UU_TRUE;
				break;
			}
		}
		status = ud_get_pickstr(prompt, m_datatype[i], lstr, m_prec[i], m_len[i], m_input[i]-1);
		if (UD_form_bypick==0)
			goto done;
		if (status == DE_DONE)
		{
			goto done;
		}
/*
.....Set the new text string
*/
		GetDlgItem(fid)->SetWindowText(lstr);
/*
....set the focus to this field also
*/
		GetDlgItem(fid)->SetFocus();

		int flag;
		form_ckdata(i, lstr, &flag);			
		if (m_pmodal[i]==0)
			goto done;
		i++;
		if (!((i<m_fldno) && ((m_input[i]==FORM_PICK) ||
			(m_input[i]==FORM_LOCATE)) || (m_input[i]==FORM_LABEL) ||
			(m_input[i]==FORM_SUBSCR)|| (m_type[i]==23)))
				goto done;
		
		fid = m_idtable[i+m_dspno];
		strcpy_s(prompt, sizeof(prompt),m_fldlabel[i]);	
/*
.....initialize the string
*/
		GetDlgItem(fid)->GetWindowText(lstr, 80);
/*
.....Disable geometry limit
*/
		if (lmtgeo) ud_lgeo(UU_FALSE,m_pick_mask[i]);
		lmtgeo = UU_FALSE;
	}
/*
.....Bring back the form
*/
done:;
	UD_UNMARK(markval);
	NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
	NCL_MainFrame->RecalcLayout();
	::EnableWindow(MainWin->GetSafeHwnd(), FALSE);
/*
......need to enable the form again because the form could be child of 
......main window, when disable main window, the form is disable too
*/
	EnableWindow();
	::SetActiveWindow(m_hWnd);

	UD_form_bypick = 0;
	return(status);
}

/***********************************************************************
c
c   SUBROUTINE:  FormLoc
c
c   FUNCTION:  This function called when "Locate" button pushed.
c			
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
int CNCLFormBar::FormLoc(int fldno)
{
	int i,markval,status;
	char prompt[256], lstr[82];
/*
.....Initialize routine
*/
	i = fldno;
	UINT fid = m_idtable[i+m_dspno];
	strcpy_s(prompt, sizeof(prompt),m_fldlabel[i]);
/*
.....initialize the string
*/
	GetDlgItem(fid)->GetWindowText(lstr, 80);
/*
.....Take down form
*/
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
	::SetActiveWindow(MainWin->GetSafeHwnd());
	NCL_MainFrame->ShowControlBar(this, FALSE, FALSE);
	NCL_MainFrame->RecalcLayout();
/*
.....Trap Reject Op
*/
	UD_MARK(markval,UU_TRUE);
	if (markval != 0)
	{
		status = DE_DONE;
		goto done;
	}
	while (1)
	{
		status = ud_get_pickstr(prompt, m_datatype[i], lstr, m_prec[i], m_len[i], m_input[i]-1);
		if (UD_form_bypick==0)
			goto done;
		if (status == DE_DONE)
		{
			goto done;
		}
/*
.....Set the new text string
*/
		GetDlgItem(fid)->SetWindowText(lstr);
/*
....set the focus to this field also
*/
		GetDlgItem(fid)->SetFocus();
		if (m_pmodal[i]==0)
			goto done;
		i++;
		if (!((i<m_fldno) && ((m_input[i]==FORM_PICK) ||
			(m_input[i]==FORM_LOCATE)) || (m_input[i]==FORM_LABEL) ||
			(m_input[i]==FORM_SUBSCR) || (m_type[i]==23)))
			goto done;
		fid = m_idtable[i+m_dspno];
		strcpy_s(prompt, sizeof(prompt),m_fldlabel[i]);	
/*
.....initialize the string
*/
		GetDlgItem(fid)->GetWindowText(lstr, 80);
	}
/*
.....Bring back the form
*/
done:;
	UD_UNMARK(markval);
	NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
	NCL_MainFrame->RecalcLayout();
/*
......need to enable the form again because the form could be child of 
......main window, when disable main window, the form is disable too
*/
	EnableWindow();
	::EnableWindow(MainWin->GetSafeHwnd(), FALSE);
	::SetActiveWindow(m_hWnd);

	return(status);
}

/***********************************************************************
c
c   SUBROUTINE:  FormSelect
c
c   FUNCTION:  This function called when "Pick" button pushed.
c				
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
int CNCLFormBar::FormSelect(int fldno)
{
	char lstr[82];
	char prompt[256];
/*
.....Initialize routine
*/
	int i,j,markval,status,lmtgeo;
	if (LW_nclipv==LW_STANDALONE) return(DE_DONE);
	i = fldno;
	UINT fid = m_idtable[i+m_dspno];
	strcpy_s(prompt, sizeof(prompt), m_fldlabel[i]);
/*
.....initialize the string
*/
	GetDlgItem(fid)->GetWindowText(lstr, 80);
/*
.....Take down form
*/
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
	::SetActiveWindow(MainWin->GetSafeHwnd());
	NCL_MainFrame->ShowControlBar(this, FALSE, FALSE);
	NCL_MainFrame->RecalcLayout();
	uw_ntreset_redraw();
/*
.....set UD_form_bypick = 1
.....to let ud_gevt1 know we don't
.....need "BY TEXT" active
*/
	UD_form_bypick = 1;	
/*
.....Trap Reject Op
*/
	UD_MARK(markval,UU_TRUE);
	if (markval != 0) 	
	{
		status = DE_DONE;
		goto done;
	}
	while (1)
	{
		lmtgeo = UU_FALSE;
		for (j=0;j<UD_NMENTWD;j++)
		{
			if (m_pick_mask[i][j] != 0)
			{
				ud_lgeo(UU_TRUE,m_pick_mask[i]);
				lmtgeo = UU_TRUE;
				break;
			}
		}
		status = ud_get_pick_selstr(prompt, lstr, m_input[i]-1);
		if (UD_form_bypick==0)
			goto done;
		if (status == DE_DONE)
		{
			goto done;
		}
		i++;
		if (!((i<m_fldno) && ((m_input[i]==FORM_PICK) ||
			(m_input[i]==FORM_LOCATE)) || (m_input[i]==FORM_LABEL) ||
			(m_input[i]==FORM_SUBSCR)|| (m_type[i]==23)))
				goto done;
		
		fid = m_idtable[i+m_dspno];
		strcpy_s(prompt, sizeof(prompt),m_fldlabel[i]);	
/*
.....initialize the string
*/
		GetDlgItem(fid)->GetWindowText(lstr, 80);
/*
.....Disable geometry limit
*/
		if (lmtgeo) ud_lgeo(UU_FALSE,m_pick_mask[i]);
		lmtgeo = UU_FALSE;
	}
/*
.....Bring back the form
*/
done:;
	UD_UNMARK(markval);
	NCL_MainFrame->ShowControlBar(this, TRUE, FALSE);
	NCL_MainFrame->RecalcLayout();
	::EnableWindow(MainWin->GetSafeHwnd(), FALSE);
/*
......need to enable the form again because the form could be child of 
......main window, when disable main window, the form is disable too
*/
	EnableWindow();
	::SetActiveWindow(m_hWnd);

	UD_form_bypick = 0;
	return(status);
}

/***********************************************************************
c
c   SUBROUTINE:  FormAccept
c
c   FUNCTION:  This function called when "Accept" button pushed.
c				it save data and destroy window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CNCLFormBar::FormAccept()
{
/*
.....close child form first
*/
	if (m_childnum!=0)
	{
		for (int i=0; i<m_childnum;i++)
			uw_ntclose_dispfrm(m_childfrm[i]);
	}
	m_childnum = 0;
/*
.....remove this form from parent form's child list child
*/
	if (m_parentID!=-1)
	{
		if (UW_display_frmwin[m_parentID]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[m_parentID]->removechild(m_frmid);
		else
			UW_display_frm[m_parentID]->removechild(m_frmid);
		m_parentID = -1;
	}

	if (NCL_form_SaveData())
	{
		ud_delform(&m_fStruct);
		uu_free((char *)m_fData.ud_data);
		if (m_nFlags & (WF_MODALLOOP|WF_CONTINUEMODAL))
			EndModalLoop(IDOK);
/*
.....Hide now, the delete form bar function will destroy the window later
*/
		m_visible = 0;
		NCL_MainFrame->ShowControlBar(this, FALSE, TRUE);
		::EnableWindow(NCL_MainFrame->GetSafeHwnd(), TRUE);
		uw_ntreset_redraw();
		NCL_MainFrame->RecalcLayout();
		if (m_disptype)
			delete this;
	}
}
/***********************************************************************
c
c   SUBROUTINE:  FormHelp()
c
c   FUNCTION:  This function called when "Help" button pushed.
c				it display a help window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::FormHelp()
{
	OnHelp();
}
/***********************************************************************
c
c   SUBROUTINE:  OnHelp()
c
c   FUNCTION:  This function called when "Help" button pushed.
c				it display a help window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::OnHelp()
{
	UX_pathname dwfile, dir, fdir;
	char *indx;
	int len, status;
	char *dsptext;
	int i,j;
/*
......always display text help window first since we will position the graphic window
......under the text help window
*/
textdisp:;
	if (m_helpbox!=NULL)
	{
		m_helpbox->ShowWindow(SW_SHOW);
		goto graphic_display;
	}
	if (m_helptext==NULL)
		goto graphic_display;

	m_helpbox = new CNCLTextWin (this, 0, m_title);
	m_helpbox->Create(IDD_TEXTWIN, this);
	m_helpbox->ShowWindow(SW_SHOW);
/*
......set the help window position
*/
	CRect rect;
	GetWindowRect(&rect);
	int x = rect.left;
	int y = rect.top;
	m_helpbox->GetWindowRect(&rect);
	m_helpbox->MoveWindow(x-100, y, rect.Width(), rect.Height());

	if (m_helptext!=NULL)
	{
/*
......replace '\n" with '\r\n'
*/
		len = strlen(m_helptext);
		dsptext = new char[len+10000];

		for (i=0,j=0; i<len; i++,j++)
		{
			if (m_helptext[i]=='\n')
				dsptext[j++] = '\r';
			dsptext[j] = m_helptext[i];
		}
		dsptext[j] = '\0';
		m_helpbox->SetText(dsptext);
		delete (dsptext);
	}
graphic_display:;
/*
......first check if we have a drawing file need to be displayed
......check the current directory first, then NCL_FORMS directory
*/
	if (m_pocket == 1) 
	{
		if ((Pocket_Win[UM_DRAWING_WINDOW]!=NULL) ||
			(Pocket_Win[UM_GRAPHIC_WINDOW]!=NULL))
			return;
	}
	strcpy_s(dwfile, sizeof(dwfile),m_filename);
	indx = strstr(dwfile, ".frm");
	if (indx!=NULL)
		*indx = '\0';
/*
.....don't use NCL_FORMS now, but use UU_USER_SETTINGS/forms directory first, 
.....don't display error message if not load
.....use UD_FORMDIR (which we use for loading form file)
*/
	ul_get_full_dir("UU_USER_SETTINGS",dir);
	ul_build_full_dir(dir,"forms",fdir);
	status = um_load_pocket_drawing(NULL, dwfile, dwfile, fdir, 0);
	if (status != UU_SUCCESS)
	{
		status = um_load_pocket_drawing(NULL, dwfile, dwfile, "UD_FORMDIR", 0);
	}
/*
.....Restore the modelling views
*/
	if (status == UU_SUCCESS)
	{
/*
......display under help window or unfer form window
*/
		if (m_helpbox!=NULL)
			m_helpbox->GetWindowRect(&rect);
		else
			GetWindowRect(&rect);
		x = rect.left;
		y = rect.bottom + 5;
		if (Pocket_Win[UM_DRAWING_WINDOW]!=NULL)
			Pocket_Win[UM_DRAWING_WINDOW]->MoveWindow(x, y, UW_pic_size[0], UW_pic_size[1]);
		if (Pocket_Win[UM_GRAPHIC_WINDOW]!=NULL)
			Pocket_Win[UM_GRAPHIC_WINDOW]->MoveWindow(x, y, UW_pic_size[0], UW_pic_size[1]);
		m_pocket = 1;
	}			
}

/***********************************************************************
c
c   SUBROUTINE:  InitAdjust
c
c   FUNCTION:  This function initialize adjust COLOR, FONT, JUSTIFY data
c				of form fields
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNCLFormBar::InitAdjust()
{
	int i,fldno, font_size,status;
	CEdit* cedt;
	CWnd *cwin;
	UINT fid, color;
	COLORREF bcolor, fcolor, pbcolor, pfcolor;

	fldno = m_frmid;

	for (i=0; i<m_dspno; i++)
	{
		fid =  m_idtable[i];
//		font_size = (int)(80 * m_fStruct.display_flds[i].font_scale);
		font_size = (int)(UW_form_fontsize*10 * m_fStruct.display_flds[i].font_scale);
		m_dfieldFont[i].CreatePointFont(font_size, UW_formdlg_font);
		cwin = (CWnd*)GetDlgItem(fid);
		cwin->SetFont(&m_fieldFont[i]);
/*
.....init background brush
*/
		if (stricmp (m_fStruct.display_flds[i].bcolor, "DEFAULT")!=0)
		{
			status = uw_get_rgb (m_fStruct.display_flds[i].bcolor, bcolor); 
			if (status!=-1)
				m_dBkBrush[i] = new CBrush(bcolor);
			else
				m_dBkBrush[i] = NULL;
		}
		else
			m_dBkBrush[i] = NULL;
	}
/*
.....initialize form data from input field #1 to field #m_fldno
*/
	HTHEME tdata = OpenThemeData(m_hWnd, L"Button");
	for (i=0; i<m_fldno; i++)
	{
		fid =  m_idtable[i+m_dspno];
		font_size = (int)(UW_form_fontsize*10 * m_fStruct.input_flds[i].font_scale);
		m_fieldFont[i].CreatePointFont(font_size, UW_formdlg_font);

		if (stricmp (m_fStruct.input_flds[i].bcolor, "DEFAULT")!=0)
		{
			status = uw_get_rgb (m_fStruct.input_flds[i].bcolor, bcolor); 
			if (status!=-1)
				m_BkBrush[i] = new CBrush(bcolor);
			else
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE); 
				m_BkBrush[i] = NULL;
			}
		}
		else
		{
			bcolor = ::GetSysColor(COLOR_BTNFACE); 
			m_BkBrush[i] = NULL;
		}
		if (stricmp (m_fStruct.input_flds[i].pbcolor, "DEFAULT")!=0)
		{
			status = uw_get_rgb (m_fStruct.input_flds[i].pbcolor, bcolor); 
			if (status!=-1)
				m_pBkBrush[i] = new CBrush(bcolor);
			else
				m_pBkBrush[i] = NULL;
		}
		else
			m_pBkBrush[i] = NULL;
		if (stricmp (m_fStruct.input_flds[i].fcolor, "DEFAULT")!=0)
		{
			status = uw_get_rgb (m_fStruct.input_flds[i].fcolor, fcolor); 
			if (status==-1)
				fcolor = ::GetSysColor(COLOR_BTNTEXT);
		}
		else
			fcolor = ::GetSysColor(COLOR_BTNTEXT);
		switch(m_type[i])
		{
/*
.....EDIT
*/
		case 3:
		case 11:
			if ((m_input[i]==FORM_PICK) || (m_input[i]==FORM_LOCATE) ||
				(m_input[i]==FORM_LABEL) || (m_input[i]==FORM_SUBSCR))
			{
				init_button3(i, fid-1, bcolor, fcolor);
			}
		case 16:
/*
.....no font changes for prompt
*/
			cedt = (CEdit*)(GetDlgItem(fid));
			cedt->SetFont(&m_fieldFont[i]);
			break;
		case 2:
		case 8:
		case 9:
		case 10:
		case 13:
/*
.....no font changes for prompt
*/
			cwin = (CWnd*)(GetDlgItem(fid));
			cwin->SetFont(&m_fieldFont[i]);
			if (m_type[i]==13)
			{
				fid =  m_idtable[i+m_dspno+m_fldno+m_framno+m_picno];
				cwin = (CWnd*)(GetDlgItem(fid));
				cwin->SetFont(&m_fieldFont[i]);
			}
			break;
/*
......all others
*/
		case 1:
		case 7:
			if (tdata!=NULL)
				init_button3(i, fid, bcolor, fcolor);
			cwin = (CWnd*)(GetDlgItem(fid));
			cwin->SetFont(&m_fieldFont[i]);
			break;
		case 17:
			init_listctl(i, fid);
			cwin = (CWnd*)(GetDlgItem(fid));
			cwin->SetFont(&m_fieldFont[i]);
			break;
		case 19:
			init_listctl2(i, fid);
			cwin = (CWnd*)GetDlgItem(fid);
			cwin->SetFont(&m_fieldFont[i]);
			break;
		case 18:
			color = *((m_fData.ud_data[i]).ud_delem.frmint);
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
			init_button(i, fid, bcolor, bcolor);
			break;
		case 23:
			if (stricmp (m_fStruct.input_flds[i].pbcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.input_flds[i].pbcolor, pbcolor); 
				if (status==-1)
				{
					pbcolor = ::GetSysColor(COLOR_BTNFACE); 
				}
			}
			else
			{
				pbcolor = ::GetSysColor(COLOR_BTNFACE); 
			}			
			if (stricmp (m_fStruct.input_flds[i].pfcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.input_flds[i].pfcolor, pfcolor); 
				if (status==-1)
				{
					pfcolor = ::GetSysColor(COLOR_BTNTEXT); 
				}
			}
			else
			{
				pfcolor = ::GetSysColor(COLOR_BTNTEXT); 
			}
			init_button2(i, fid-1, pfcolor, pbcolor, fid, fcolor, bcolor);
			break;
		case 24:
			init_slider(i, fid);
			break;
		default:
			cwin = (CWnd*)(GetDlgItem(fid));
			cwin->SetFont(&m_fieldFont[i]);
		}
	}
	for (i=0; i<m_framno; i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno];  
		font_size = (int)(UW_form_fontsize*10 * m_fStruct.frame_flds[i].font_scale);
		m_dfieldFont[i].CreatePointFont(font_size, UW_formdlg_font);
		cwin = (CWnd*)(GetDlgItem(fid));
		cwin->SetFont(&m_fieldFont[i]);
		if (stricmp (m_fStruct.frame_flds[i].bcolor, "DEFAULT")!=0)
		{
			status = uw_get_rgb (m_fStruct.frame_flds[i].bcolor, bcolor); 
			if (status!=-1)
				m_fBkBrush[i] = new CBrush(bcolor);
			else
				m_fBkBrush[i] = NULL;
		}
	}
	for (i=0; i<m_picno; i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno+m_framno]; 
		init_picarea(i, fid);
	}
/*
......Set the picture area
*/
	for (i=0; i<m_fldno; i++)
	{
		fid =  m_idtable[i+m_dspno];
		if (m_fStruct.input_flds[i].picarea!=NULL)
			set_picarea(m_picno, m_fStruct.input_flds[i].n_picarea, m_fStruct.input_flds[i].picarea, fid); 
	}	
}
/**********************************************************************
**    I_FUNCTION :  set_butlabel(int fieldno, char* label)
**       Set the button label in form input field fieldno. 
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
void CNCLFormBar::set_butlabel(int fieldno, char* label)
{
	UINT fid =  m_idtable[fieldno+m_dspno];
	if (m_type[fieldno]!=1)
		return;

	CWnd *cwin = (GetDlgItem(fid));
	cwin->SetWindowText(label);
}

void CNCLFormBar::init_slider(int i, UINT fid)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPSLIDER1 + i;
	if (m_type[i]!=24)
		return;
	CWnd *cwin = GetDlgItem(tempid);
	if (cwin == NULL)
		return;
	CRect rbtn;
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	DWORD dwStyle;
	if (m_fStruct.input_flds[i].justified==0)
		dwStyle = WS_VISIBLE | WS_CHILD | TBS_HORZ;
	else
		dwStyle = WS_VISIBLE | WS_CHILD | TBS_VERT;
	int value;
	if ((m_fData.ud_data[i]).ud_delem.frmint!=NULL)
		value = (m_fData.ud_data[i]).ud_delem.frmint[0];
	else
		value = 50;			
	int range1 = m_fStruct.input_flds[i].range[0].dint; 
	int range2 = m_fStruct.input_flds[i].range[1].dint;
	UINT budid = m_idtable[i+m_dspno+m_fldno+m_framno+m_picno];
	Create_Slider(i, fid, dwStyle, rbtn, range1, range2, value, budid,
		m_fStruct.input_flds[i].justified);
}
void CNCLFormBar::Create_Slider(int i, UINT fid, DWORD dwStyle, RECT& rbtn, 
		int range1, int range2, int init_value, UINT budid, int vert)
{
	ScreenToClient(&rbtn);
/*
	if (vert==0)
	{
		if (rbtn.bottom-rbtn.top>20)
			rbtn.bottom = rbtn.top + 20;
	}
	else
	{
		if (rbtn.right-rbtn.left>20)
			rbtn.right = rbtn.left + 20;
	}
*/
	m_slider[i].Create(dwStyle, rbtn, this, fid);
	m_slider[i].SetParent(this);
	m_slider[i].SetBuddy(GetDlgItem(budid));
	m_slider[i].SetRange(range1, range2);
	m_slider[i].SetPos(init_value);
	m_slider[i].ShowWindow(SW_SHOW);
}

void CNCLFormBar::init_button(int i, UINT fid, COLORREF bcolor, COLORREF fcolor)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPBUT1 + i;
	if ((m_type[i]!=1) && (m_type[i]!=3)&&(m_type[i]!=11)&&(m_type[i]!=18)) 
		return;

	CWnd *cwin = GetDlgItem(tempid);
	if (cwin == NULL)
		return;
	CRect rbtn;
	CFont* aFont = cwin->GetFont();
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	if (m_type[i]==18)
		Recreate_button(i, "", 
			WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY,
			rbtn, fid, bcolor, fcolor);	
	else
		Recreate_button(i, m_fldlabel[i], 
			WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY,
			rbtn, fid, bcolor, fcolor);	
	if ((m_type[i]==3)||(m_type[i]==11))
	{
		cwin = GetDlgItem(fid);
		cwin->SetFont(aFont);
	}
}
void CNCLFormBar::init_button2(int i, UINT pfid, COLORREF pbcolor, COLORREF pfcolor, 
			UINT fid, COLORREF bcolor, COLORREF fcolor)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPBUT1 + i;
	if (m_type[i]!=23)
		return;

	CWnd *cwin = GetDlgItem(tempid);
	if (cwin == NULL)
		return;
	CRect rbtn;
	CFont* aFont = cwin->GetFont();
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	Recreate_button(i, m_fldlabel[i], 
			WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY,
			rbtn, pfid, pbcolor, pfcolor);	
	cwin = GetDlgItem(pfid);
	cwin->SetFont(aFont);

	cwin = GetDlgItem(fid);
	if (cwin == NULL)
		return;
	aFont = cwin->GetFont();
	cwin->GetWindowRect(&rbtn);
	cwin->DestroyWindow();
	Recreate_button2(i, "", 
			WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY,
			rbtn, fid, bcolor, fcolor);	
	cwin = (GetDlgItem(fid));
	cwin->SetFont(aFont);
}

void CNCLFormBar::init_button3(int i, UINT fid, COLORREF bcolor, COLORREF fcolor)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPBUT1 + i;
	if ((m_type[i]!=1) && (m_type[i]!=3)&&(m_type[i]!=11)&&(m_type[i]!=7)) 
		return;

	CWnd *cwin = GetDlgItem(tempid);
	if (cwin == NULL)
		return;
	CRect rbtn;
	CFont* aFont = cwin->GetFont();
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	if (m_type[i]==7)
		Recreate_button3(i, m_fldlabel[i], 
			WS_VISIBLE | WS_CHILD | BS_AUTOCHECKBOX | BS_CENTER | WS_TABSTOP | BS_NOTIFY,
			rbtn, fid, bcolor, fcolor, m_type[i]);	
	else
		Recreate_button3(i, m_fldlabel[i], 
			WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_NOTIFY,
			rbtn, fid, bcolor, fcolor, m_type[i]);	
	if ((m_type[i]==3)||(m_type[i]==11))
	{
		cwin = GetDlgItem(fid);
		cwin->SetFont(aFont);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  Recreate_button() 
c
c   FUNCTION:  This function create a button using input caption, ID and attributes
c
c   INPUT:  i: button index
c			lpszCaption: button caption
c			dwStyle: button style
c			rect: button size
c			nID: button ID
c			bcolor: button background color
c			fcolor: button foreground color
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::Recreate_button(int i, LPCTSTR lpszCaption, DWORD dwStyle, 
										 RECT& rect, UINT nID, COLORREF bcolor, COLORREF fcolor)
{
	CFont aFont;
	ScreenToClient(&rect);
	m_button[i].Create(lpszCaption, dwStyle, rect, this, nID);
	m_button[i].set_color(bcolor, fcolor);
	m_button[i].ShowWindow(SW_SHOW);
}
/***********************************************************************
c
c   SUBROUTINE:  Recreate_button2() 
c
c   FUNCTION:  This function create a button using input caption, ID and attributes
c
c   INPUT:  i: button index
c			lpszCaption: button caption
c			dwStyle: button style
c			rect: button size
c			nID: button ID
c			bcolor: button background color
c			fcolor: button foreground color
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::Recreate_button2(int i, LPCTSTR lpszCaption, DWORD dwStyle, 
										 RECT& rect, UINT nID, COLORREF bcolor, COLORREF fcolor)
{
	CFont aFont;
	ScreenToClient(&rect);
	m_button2[i].Create(lpszCaption, dwStyle, rect, this, nID);
	m_button2[i].set_color(bcolor, fcolor);
	m_button2[i].ShowWindow(SW_SHOW);
}

void CNCLFormBar::Recreate_button3(int i, LPCTSTR lpszCaption, DWORD dwStyle, 
										 RECT& rect, UINT nID, COLORREF bcolor, COLORREF fcolor, int type)
{
	CFont aFont;
	ScreenToClient(&rect);
	m_button3[i].SetType(type);
	m_button3[i].set_color(bcolor, fcolor);
	m_button3[i].Create(lpszCaption, dwStyle, rect, this, nID);
	m_button3[i].ShowWindow(SW_SHOW);
}

/**********************************************************************
**    I_FUNCTION :  OnAccelFunctions(WPARAM wparm, LPARAM lparm)
**		Callback function for hotkey for CAM_SCALAR
**
**    PARAMETERS   
**       INPUT  : 
**          UINT id: Accelerator ID
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::OnAccelFunctions(UINT id)
{
	CString sav_cmdstr;
/*
......use local value
*/
	int savcmd_enable;
	int * save_inptr;
	int jmpflag, sav_cur,sav_mode,save_ptype;
	int line_num, upload, sav_cmdcur1, sav_cmdcur2;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return;
	if (m_return_key==1)
/*
.....if it is normal return key, call return callback function
*/
	{
		FormUserCallbacks6();
		return;
	}

	if ((m_hotkey_num[0]==-1) && (m_hotkey_num[1]==-1) 
		&& (m_hotkey_num[2]==-1)&& (m_hotkey_num[3]==-1))
		return;

	sav_mode = UD_pickmode;
	ug_save_event();
	ug_save_input(&save_inptr);
	save_ptype = ud_getpick_type();
	ud_setpick_type(UD_PICK_NORMAL);
	savcmd_enable = NCL_MainFrame->Get_CmdEnable();
	if (savcmd_enable==1)
	{
		NCL_MainFrame->GetCommand_Str(sav_cmdstr, line_num, upload);
		uw_ntget_ecurpos(&sav_cmdcur1, &sav_cmdcur2);
		NCL_MainFrame->SetCommand_Str("");
		NCL_MainFrame->Enable_cmdbar(0);
		NCL_MainFrame->SetCommand_focus(0);
	}

	sav_cur = uw_ntgetcur_cursor();
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag == 0)
	{
		uw_ntsetcursor(21);
		if (id==ID_FORM_HOTKEY1)
			uz_wnt_callfunc(m_hotkey_num[0], 1);
		else if (id==ID_FORM_HOTKEY2)
			uz_wnt_callfunc(m_hotkey_num[1], 1);
		else if (id==ID_FORM_HOTKEY3)
			uz_wnt_callfunc(m_hotkey_num[2], 1);
		else if (id==ID_FORM_HOTKEY4)
			uz_wnt_callfunc(m_hotkey_num[3], 1);
	}
	if (savcmd_enable==1)
	{
		NCL_MainFrame->Enable_cmdbar(1);
		NCL_MainFrame->SetCommand_Str(sav_cmdstr, line_num, upload);
		NCL_MainFrame->SetCommand_focus(1);
		NCL_MainFrame->SetCommand_Insertpos(sav_cmdcur1, sav_cmdcur2);
	}
	uw_ntsetcursor(sav_cur);
	ud_updatews(UG_SUPPRESS);
	UD_pickmode = sav_mode;
	ud_setpick_type(save_ptype);
	ug_reset_event();
	ug_reset_input(save_inptr);

	UD_UNMARK(jmpflag);
}
/***********************************************************************
c
c   SUBROUTINE:  Recreate_listctl() 
c
c   FUNCTION:  This function create a listctl using input list struction
c
c   INPUT:  i: list index
c			listptr: list struction
c			dwStyle: window style
c			rect: list size
c			nID: list ID
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::Recreate_listctl(int i, char *listptr, DWORD dwStyle,
										 RECT& rect, UINT nID)
{
	ScreenToClient(&rect);
	m_listctl[i].Create(dwStyle, rect, this, nID);
	m_listctl[i].ShowWindow(SW_SHOW);
}

/***********************************************************************
c
c   SUBROUTINE:  Recreate_listctl2() 
c
c   FUNCTION:  This function create a listctl using input list struction
c
c   INPUT:  i: list index
c			listptr: list struction
c			dwStyle: window style
c			rect: list size
c			nID: list ID
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::Recreate_listctl2(int i, char *listptr, DWORD dwStyle,
										 RECT& rect, UINT nID)
{
	ScreenToClient(&rect);
	m_listctl2[i].Create(dwStyle, rect, this, nID);
	m_listctl2[i].SetParent_id(this, nID);
	m_listctl2[i].ShowWindow(SW_SHOW);
}

void CNCLFormBar::init_listctl2(int i, UINT fid)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPLIST1 + i;
/*
......if not table list, just return
*/
	if (m_type[i]!=19)
		return;

	CWnd *cwin = GetDlgItem(tempid);
	if (cwin == NULL)
		return;
	CRect rbtn;
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	Recreate_listctl2(i, NULL, 
		WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL | WS_HSCROLL | WS_VSCROLL,
		rbtn, fid);	
}
void CNCLFormBar::init_listctl(int i, UINT fid)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPLIST1 + i;
/*
......if not table list, just return
*/
	if (m_type[i]!=17)
		return;

	CWnd *cwin = GetDlgItem(tempid);
	if (cwin == NULL)
		return;
	CRect rbtn;
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	Recreate_listctl(i, NULL, 
		WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL,
		rbtn, fid);	
}
/**********************************************************************
**    I_FUNCTION :  CtrlListCallback(UINT id, UD_TABLEINFO *info)
**       list box select callback for all fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::CtrlListCallback(UINT id, UD_TABLEINFO *info)
{
	int i, fldno;
	UD_FSTAT stat;
	int redisp = 0;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return;

	stat = UD_FLDOK;
/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return;
	fldno = -1;

	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			break;
		}
	}
	if (fldno==-1)
		return;
	if (m_method_ret[fldno] & UD_TOGGLE &&
			m_method[fldno] != NULL)
	{
		UD_DDATA seldata;
/*
.....reset pick segment list before callback function
*/
		ncl_reset_pikgeom();
		seldata.frmint = (int *)info;
		stat = (m_method[fldno])(&fldno, &seldata, UD_FLDOK);
		if (fldno!=-1)
			redisp = 1;			
	}
	return;
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks10(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormBar::FormUserCallbacks10(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
{
	if (m_init==0)
		return;
	NMLISTVIEW *pLV = (NMLISTVIEW *) pNMHDR;	
	CNCLListCtrl *list = (CNCLListCtrl*)GetDlgItem(id);

	UD_TABLEINFO info;
	info.flag = 2;
	info.frmid = m_frmid;
	info.fldid = id;
	info.col = pLV->iSubItem;
	info.row = -1;
	info.data_ptr = NULL;
	CtrlListCallback(id, &info);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks11(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormBar::FormUserCallbacks11(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
{
	*pResult = 0;
	if (m_init==0)
		return;
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	CNCLListCtrl *list;
	CWnd *listwin = (CWnd*)GetDlgItem(id);
	if (listwin->IsKindOf(RUNTIME_CLASS(CNCLListCtrl)))
	{
		list = (CNCLListCtrl*)listwin;
		if( pNMListView->uNewState & LVIS_SELECTED )
		{
			int selectedItem = pNMListView->iItem;
			UD_TABLEINFO info;
			info.flag = 1;
			info.frmid = m_frmid;
			info.fldid = id;
			info.col = -1;
			info.row = selectedItem;
			info.data_ptr = (int*)list->GetItemData(info.row);
			CtrlListCallback(id, &info);
			list->m_selitem = selectedItem;
		}
	}
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks12(UINT id)
c
c          Callback function for table list set focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormBar::FormUserCallbacks12(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
{
	FormUserCallbacks7(id);
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks13(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormBar::FormUserCallbacks13(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
{
	*pResult = 0;
	if (m_init==0)
		return;
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	CNCLListCtrl *list = (CNCLListCtrl*)GetDlgItem(id);
    if (  ( pNMListView->uOldState & LVIS_SELECTED ) && 
         ( pNMListView->uNewState == 0 )  )
    {
		if (list->m_selitem==pNMListView->iItem)
		{
			list->SetItemState(pNMListView->iItem, 0, LVIS_SELECTED | LVIS_FOCUSED);	
			list->m_selitem = -1;
			UD_TABLEINFO info;
			info.flag = 1;
			info.frmid = m_frmid;
			info.fldid = id;
			info.col = -1;
			info.row = -1;
			info.data_ptr = NULL;
			CtrlListCallback(id, &info);
			*pResult = 1;
			return;
		}
	}
}

/**********************************************************************
**    I_FUNCTION :  sort_tlist(UINT fid, int isub, UD_SMETHOD sortfunc)
**       Sort the form table list field fieldno use user defined 'sortfunc'. 
**    PARAMETERS   
**       INPUT  : 
**          fid : field ID
**			sortfunc: sort function to sort
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::sort_tlist(UINT fid, int isub, UD_SMETHOD sortfunc)
{
	int i, fldno = -1;
	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==fid)
		{
			fldno = i;
			break;
		}
	}
	if (fldno==-1)
		return;
	m_sortfunc[fldno] = sortfunc;
	if (m_type[i]==17)
	{
		CNCLListCtrl* listctl = (CNCLListCtrl*)GetDlgItem(fid);
		listctl->SortItems((PFNLVCOMPARE)SortFunc, isub);
		return;
	}
	if (m_type[i]==19)
	{
		CNCLListCtrl2* listctl2 = (CNCLListCtrl2*)GetDlgItem(fid);
		listctl2->SortItems((PFNLVCOMPARE)SortFunc, isub);
		return;
	}
}

/**********************************************************************
**    I_FUNCTION :  set_tlist(int fieldno, int *form_list)
**       Set the form table list field fieldno. 
**    PARAMETERS   
**       INPUT  : 
**          fieldno : field number
**			form_list: UD_LIST structure for redisplay
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::set_tlist(int fieldno, int *form_list)
{
	int m,j;
	CNCLListCtrl* listctl;
	CNCLListCtrl2* listctl2;
	UD_TLIST *tlist;
	UD_DLIST *dlist;
	UINT fid =  m_idtable[fieldno+m_dspno];
	switch(m_type[fieldno])
	{
	case 17:
		tlist = (UD_TLIST *)form_list;
		listctl = (CNCLListCtrl*)GetDlgItem(fid);
/*
.....we need clear listbox first
*/
		listctl->DeleteAllItems();
		for(j=0;j<tlist->num_item;j++)
		{
			tlist->data[j].fldno = fieldno;
			tlist->data[j].frmid = m_frmid;
			listctl->InsertItem(j,  tlist->data[j].data_items[0]);
			for (m=1; m<tlist->data[j].itemnum;m++)
			{
				listctl->SetItemText(j, m, tlist->data[j].data_items[m]);
			}
			listctl->SetItemData(j, (LPARAM)&(tlist->data[j]));
		}
		break;
	case 19:
		dlist = (UD_DLIST *)form_list;
		listctl2 = (CNCLListCtrl2*)GetDlgItem(fid);
/*
.....we need clear listbox item first, also need clean column too
*/
		listctl2->DeleteAllItems();
		int nColumn = listctl2->GetHeaderCtrl()->GetItemCount();
		for (int i=0;i<nColumn; i++)
		{
			listctl2->DeleteColumn(0);
		}
		if ((dlist->num_col<=0)||(dlist->num_item<=0))
			break;
		CRect rect;
		int wid = 0;
		listctl2->GetWindowRect(&rect);
		if (dlist->num_col>0)
			wid = rect.Width()/dlist->num_col;
		if (wid < 60)
			wid = 60;
		LV_COLUMN lvc;
		lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;
		for (int k=0; k<dlist->num_col; k++)
		{
			lvc.iSubItem = k;
			lvc.pszText = dlist->col_label[k];
			lvc.cx = wid;
			lvc.fmt = LVCFMT_LEFT;
			listctl2->InsertColumn(k,&lvc);
		}
		for(j=0;j<dlist->num_item;j++)
		{
			dlist->data[j].fldno = fieldno;
			dlist->data[j].frmid = m_frmid;
			listctl2->InsertItem(j,  dlist->data[j].data_items[0]);
			for (int m=1; m<dlist->data[j].itemnum;m++)
			{
				listctl2->SetItemText(j, m, dlist->data[j].data_items[m]);
			}
			listctl2->SetItemData(j, (LPARAM)&(dlist->data[j]));
		}
		listctl2->m_nNumberOfRows = dlist->num_item;
		listctl2->m_nNumberOfCols = dlist->num_col;
		listctl2->SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES);
		listctl2->ModifyStyle(0,LVS_SHOWSELALWAYS);
/*
......have to set the focus to set a selection
*/
		listctl2->SetFocus();
		if (((dlist->answer[0]>=0)&&(dlist->answer[0]<dlist->num_item))
				&& ((dlist->answer[1]>0)&&(dlist->answer[1]<dlist->num_col)))
		{
			listctl2->SetItemSel(dlist->answer[0], dlist->answer[1]);
		}
		else
			listctl2->DeSelAll();
		if ((m_sortfunc[fieldno]!=NULL)&&(dlist->sort>=0)&&(dlist->sort<=dlist->num_col))
		{
			listctl2->SortItems((PFNLVCOMPARE)SortFunc, dlist->sort);
		}	
		break;
	}
}
/**********************************************************************
**    I_FUNCTION :  sort_tlist(UINT fid, int isub, UD_SMETHOD sortfunc)
**       Sort the form table list field fieldno use user defined 'sortfunc'. 
**    PARAMETERS   
**       INPUT  : 
**          fid : field ID
**			sortfunc: sort function to sort
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::set_sort_func(int fldno, UD_SMETHOD sortfunc)
{
	m_sortfunc[fldno] = sortfunc;
}
/**********************************************************************
**    I_FUNCTION : set_focus(fieldno)
**       set the focus of a form field
**    PARAMETERS  
**       INPUT  : fieldno
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::set_focus(int fieldno)
{
	int start, end;
	CWnd *win = NULL; 
	win = GetDlgItem(m_idtable[fieldno+m_dspno]);
	if (win==NULL)
	{
		return;
	}
	win->SetFocus();
	if (m_type[fieldno]==3)
	{
		((CEdit*)win)->SetSel(0, -1);
		((CEdit*)win)->GetSel(start, end);
		((CEdit*)win)->SetSel(end, end);
	}
}
/**********************************************************************
**    I_FUNCTION : FormColor(int fieldno)
**       form color button callback: diplay a color dialog
**    PARAMETERS  
**       INPUT  : fieldno
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::FormColor(int fieldno)
{
	GdiplusStartupInput gdiplusStartupInput;
	ULONG_PTR gdiplusToken;
	char defstr[64];

	GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);

	CNCLColorDlg dlg;
	dlg.SetColorIndex(*((m_fData.ud_data[fieldno]).ud_delem.frmint));
	if (m_fStruct.input_flds[fieldno].n_defaults==1)
		strcpy_s(defstr, sizeof(defstr), m_formchoice[fieldno][0]);
	else
		defstr[0] = '\0';
	dlg.SetColorDefault(m_fStruct.input_flds[fieldno].n_defaults, defstr);

	int nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
/*
.....set the field color to select color
*/
		SetButColor(fieldno, dlg.m_current_color);
	}
	else if (nResponse == IDCANCEL)
	{
	}
	GdiplusShutdown(gdiplusToken);
}

/**********************************************************************
**    I_FUNCTION : PickColor(int fieldno)
**       form color button callback: diplay a color dialog
**    PARAMETERS  
**       INPUT  : fieldno
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::PickColor(int fieldno)
{
	GdiplusStartupInput gdiplusStartupInput;
	ULONG_PTR gdiplusToken;
	char defstr[64];

	GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);

	CNCLColorDlg dlg;
	dlg.SetColorIndex(*((m_fData.ud_data[fieldno]).ud_delem.frmint));
	if (m_fStruct.input_flds[fieldno].n_defaults==1)
		strcpy_s(defstr, sizeof(defstr), m_formchoice[fieldno][0]);
	else
		defstr[0] = '\0';
	dlg.SetColorDefault(m_fStruct.input_flds[fieldno].n_defaults, defstr);

	int nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
		SetButColor(fieldno, dlg.m_current_color);
	}
	else if (nResponse == IDCANCEL)
	{
	}
	GdiplusShutdown(gdiplusToken);
}

/**********************************************************************
**    I_FUNCTION : SetButColor(int fieldno, int color)
**       set form color button color
**    PARAMETERS  
**       INPUT  : fieldno
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormBar::SetButColor(int fieldno, int color)
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
	m_button[fieldno].set_color(bcolor, bcolor);
	m_button[fieldno].Invalidate();
	m_button[fieldno].UpdateWindow();

	UD_SELECT_STR *dselect = (UD_SELECT_STR *)((m_fData.ud_data[fieldno]).ud_delem.frmint);
	dselect->color = color;
}
int CNCLFormBar::Get_fldno_from_ID(UINT id)
{
	int i, fldno = -1;
	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			break;
		}
	}
	return fldno;
}
/***********************************************************************
c
c   FUNCTION: OnItemClick(WPARAM wParam, LPARAM lParam)
c
c          Callback for listitem click
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
LRESULT CNCLFormBar::OnItemClick(WPARAM wParam, LPARAM lParam)
{
	CNCLListCtrl2 *dlist;
	UINT id = (UINT) lParam;
	UD_TABLEINFO info, *info1 = (UD_TABLEINFO*) wParam;
	CWnd *list = GetDlgItem(id);

	info.flag = 1;
	info.frmid = m_frmid;
	info.fldid = id;
	info.col = info1->col;
	info.row = info1->row;

	UD_ITEMDATA data;

	if (list->IsKindOf(RUNTIME_CLASS(CNCLListCtrl2)))
	{
		dlist = (CNCLListCtrl2*)list;
		data.flag = info.flag;
		data.fldno = Get_fldno_from_ID(info.fldid);
		data.frmid = info.frmid;
		dlist->fill_item_data(info.row, &data);
		info.data_ptr = (int*)&data;
		CtrlListCallback(id, &info);
		dlist->m_selitem = info1->row;
		dlist->m_selSitem = info1->col;
		dlist->m_change = 1;
		ud_free_itemdata(&data);
	}
	return 1;
}

/***********************************************************************
c
c   SUBROUTINE:  Create_PicArea() 
c
c   FUNCTION:  This function create a picture area
c
c   INPUT:  filename: image file to displayed in picture window
c			dwStyle: window style
c			rect: list size
c			nID: list ID
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::Create_PicArea(int indx, char *name,  char* filename, RECT& rect, UINT nID)
{
	ScreenToClient(&rect);
	m_picture[indx] = new CNCLPicSelWin(name, filename, this);
	m_picture[indx]->Create(WS_CHILD|WS_VISIBLE|WS_TABSTOP|WS_BORDER, rect, this, nID);
	m_picture[indx]->ShowWindow(SW_SHOW);
}

/***********************************************************************
c
c   SUBROUTINE:  Reset_picture_file(int indx, char* filename)
c
c   FUNCTION:  Reset the picture field a new image file
c
c   INPUT:  filename: image file to displayed in picture window
c			indx: picture field index
c			
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::Reset_picture_file(int indx, char* filename)
{
	if (m_picture[indx]==NULL)
		return;
	m_picture[indx]->Reset_picture(filename);
	RedrawWindow();
	UpdateWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  set_picarea(int picno, int n_picarea, UD_PICAREA *picarea, UINT pid)
c
c   FUNCTION:  Set picture area information
c
c   INPUT:  filename: image file to displayed in picture window
c			indx: picture field index
c			
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::set_picarea(int picno, int n_picarea, UD_PICAREA *picarea, UINT pid)
{
	for (int i=0; i<picno; i++)
	{
		for (int j=0; j<n_picarea; j++)
		{
			if (stricmp(m_picture[i]->m_name, picarea[j].name)==0)
			{
				m_picture[i]->set_picarea(picarea[j], pid);
			}
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  init_picarea(int i, UINT fid)
c
c   FUNCTION:  Init and create a picture area
c
c   INPUT:  i: picture field index
c			indx: picture field index
c			
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::init_picarea(int i, UINT fid)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDC_FORMTEMPPIC + i;
		
	char filename[UX_MAX_PATH_LEN], name[UX_MAX_PATH_LEN];
	strcpy(filename, m_fStruct.picture_flds[i].fname);
	strcpy(name, m_fStruct.picture_flds[i].name);
	CWnd *cwin = GetDlgItem(tempid);
	if (cwin == NULL)
		return;
	CRect rbtn;
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	Create_PicArea(i, name, filename, rbtn, fid); 
}
/***********************************************************************
c
c   SUBROUTINE:  PicAreaClick(UINT fid, char *params)
c
c   FUNCTION:  execute a picture area relate function
c
c   INPUT:  fid: picture field picked field ID
c			params: picture field picked parameter
c			
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormBar::PicAreaClick(UINT fid, char *params)
{
	UD_LIST *list;
	int i, j, choice, fldno = -1;

	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==fid)
		{
			fldno = i;
			break;
		}
	}
	if (fldno==-1)
		return;
	CWnd *cwin = GetDlgItem(fid);
	if (cwin == NULL)
		return;
	if (params[0]!='\0')
	{
/*
.....remove proceed spaces
*/
		i=0;
		while (params[i]==' ') i++;
		strcpy(params, &(params[i]));
		ul_remove_quotes(params);
	}
/*
.....enable display field
*/
	if ((m_type[fldno]==3)||(m_type[fldno]==2)||(m_type[fldno]==11)
			||(m_type[fldno]==8)||(m_type[fldno]==9)|| (m_type[fldno] == 16)
			||(m_type[fldno]==10) ||(m_type[fldno]==13)
			||(m_type[fldno]==14) || (m_type[fldno] == 15)||(m_type[fldno]==18))
	{
/*
......enable prompt label
*/
		CWnd *pwin = GetDlgItem(fid-1);
		pwin->EnableWindow(TRUE);
	}
/*
......enable input field
*/
	cwin->EnableWindow(TRUE);
	cwin->SetFocus();
/*
......PushButton
*/
	if (m_type[fldno]==1)
	{
		PostMessage(WM_COMMAND, fid);
	}
/*
......CHOICEBOX
*/
	else if (m_type[fldno]==2)
	{
		if (params[0]!='\0')
		{
			choice = atoi(params);
			if (choice!=-1)
				((CComboBox*)cwin)->SetCurSel(choice);
			else
			{
				choice = ((CComboBox*)cwin)->GetCurSel();
				int total = ((CComboBox*)cwin)->GetCount();
				if (choice<(total-1))
					choice++;
				else
					choice = 0;
				((CComboBox*)cwin)->SetCurSel(choice);
			}
			SendMessage(CBN_VIEWSELCHANGE, (WPARAM)fid);		
		}
	}
/*
......LIST BOX
*/
	else if (m_type[fldno]==5)
	{
		if (params[0]!='\0')
		{
			list = (UD_LIST *)((m_fData.ud_data[fldno]).ud_delem.frmint);
			choice = -1;
			for (j=0; j<list->num_item; j++)
			{
				if (strcmp(list->item[j], params)==0)
					choice = j;
			}
			if (choice!=-1)
				((CListBox*)cwin)->SetCurSel(choice);
			SendMessage(LBN_VIEWSELCHANGE, (WPARAM)fid);		
		}
	}
/*
......simple LIST, choice list
*/
	else if ((m_type[fldno]==8)||(m_type[fldno]==9)||(m_type[fldno]==15))
	{
		if (params[0]!='\0')
		{
			list = (UD_LIST *)((m_fData.ud_data[fldno]).ud_delem.frmint);
			choice = -1;
			for (j=0; j<list->num_item; j++)
			{
				if (strcmp(list->item[j], params)==0)
					choice = j;
			}
			if (choice!=-1)
				((CComboBox*)cwin)->SetCurSel(choice);
			SendMessage(CBN_VIEWSELCHANGE, (WPARAM)fid);
		}
	}
/*
......EDIT
*/
	else if ((m_type[fldno]==3)||(m_type[fldno]==11)||(m_type[fldno]==16))
	{
		if (m_input[fldno]==FORM_PICK || m_input[fldno]==FORM_LABEL ||
			m_input[fldno]==FORM_SUBSCR)
			FormPick(fldno);
		else if (m_input[fldno]==FORM_LOCATE)
			FormLoc(fldno);
		else
		{
			((CEdit*)cwin)->SetWindowText(params);
			((CEdit*)cwin)->SetSel(0, -1);
		}
	}
/*
......CHECKBOX
*/
	else if (m_type[fldno]==7)
	{
		if (params[0]!='\0')
		{
			if (stricmp(params, "ON")==0)
				((CButton*)cwin)->SetCheck(1);
			else if (stricmp(params, "OFF")==0)
				((CButton*)cwin)->SetCheck(0);
			else if (stricmp(params, "TOGGLE")==0)
			{
				int checked = ((CButton*)cwin)->GetCheck();
				if (checked)
					((CButton*)cwin)->SetCheck(0);
				else
					((CButton*)cwin)->SetCheck(1);
			}
			SendMessage(WM_VIEWCOMMAND, (WPARAM)fid);
		}
	}
/*
.....table list (LISTTABLE)
*/
	else if (m_type[fldno]==17)
	{
	}
/*
.....DATA list (DATATABLE)
*/	
	else if (m_type[fldno]==19)
	{
	}
}
void CNCLFormBar::GetDataType(int n, int *data_type)
{
	for (int i=0; i<n;i++)
	{
		data_type[i] = m_fStruct.input_flds[i].ud_datatyp;
	}
}
void CNCLFormBar::SetDataInitType(int n, int *data_type)
{
	m_init_data_type_flag = 1;
	for (int i=0; i<n;i++)
	{
		m_data_type[i] = data_type[i];
	}
}
void CNCLFormBar::SetFieldColr(int fieldno, int fg, int bg)
{
	if ((fieldno<0)||(fieldno>=m_fldno))
		return;
/*
.....updated the color value
*/
	if ((fg<0)||(fg>=64))
		strcpy(m_fStruct.input_flds[fieldno].fcolor, "DEFAULT");
	else
		strcpy(m_fStruct.input_flds[fieldno].fcolor, uw_color_name[fg]);
	if ((bg<0)||(bg>=64))
		strcpy(m_fStruct.input_flds[fieldno].bcolor, "DEFAULT");
	else
		strcpy(m_fStruct.input_flds[fieldno].bcolor, uw_color_name[bg]);
/*
.....for owner draw button, set the button color directly
*/
	COLORREF bcolor, fcolor;
	if (m_type[fieldno]==23)
	{
		UD_SELECT_STR *dselect;
/*
.....set the field color to select color
*/
		if (bg<0)
		{
			bcolor = RGB(255, 0, 0);
		}
		else
		{
			bcolor = RGB(uw_color_table[bg][0], 
							uw_color_table[bg][1], 
							uw_color_table[bg][2]);
		}
		m_button2[fieldno].set_color(bcolor, bcolor);
		m_button2[fieldno].Invalidate();
		m_button2[fieldno].UpdateWindow();
		return;
	}
	if ((m_type[fieldno]==1)||(m_type[fieldno]==18))
	{		
		if (bg<0)
		{
			bcolor = ::GetSysColor(COLOR_BTNFACE); 
		}
		else
		{
			bcolor = RGB(uw_color_table[bg][0], 
						uw_color_table[bg][1], 
						uw_color_table[bg][2]);
		}
		if (m_type[fieldno]==18)
		{
			m_button[fieldno].set_color(bcolor, bcolor);
			m_button[fieldno].Invalidate();
			m_button[fieldno].UpdateWindow();
		}
		else
		{
			if (fg<0)
			{
				fcolor = RGB(0,0,0);
			}
			else
			{
				fcolor = RGB(uw_color_table[fg][0], 
							uw_color_table[fg][1], 
							uw_color_table[fg][2]);
			}
			m_button[fieldno].set_color(bcolor, fcolor);
			m_button[fieldno].Invalidate();
			m_button[fieldno].UpdateWindow();
		}
		return;
	}
/*
.....for other standard dialog item, so have to change the color by
.....call OnCtlColor which will called by system if we repaint the view
*/
	OnPaint();
}
void CNCLFormBar::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	int i, fldno;
	UINT fid;
	CWnd *wnd;
	if (pScrollBar==NULL)
		return CNCLDialogBar::OnVScroll(nSBCode, nPos, pScrollBar);
	if ((pScrollBar->IsKindOf(RUNTIME_CLASS(CNCLSliderCtrl)))==0)
		return CNCLDialogBar::OnVScroll(nSBCode, nPos, pScrollBar);

	CNCLSliderCtrl* pSlider = (CNCLSliderCtrl*)pScrollBar;		
	pSlider->ReflectedScrollMessage();
	int pos = pSlider->GetPos();

	fldno = -1;
	for (i=0; i<m_fldno; i++)
	{
		if (m_type[i]!=24)
			continue;
		fid = m_idtable[i+m_dspno];
		wnd = GetDlgItem(fid);
		if (pScrollBar->m_hWnd == wnd->m_hWnd)
		{
			fldno = i;
			break;
		}
	}
	if (fldno == -1)
		return CNCLDialogBar::OnVScroll(nSBCode, nPos, pScrollBar);

	Focused_frmid = m_frmid;
	Focused_frm_fldno = fldno;
/*
......set current form too
*/
//	uw_ntset_curform_id(m_frmid);

	
	CNCLDialogBar::OnVScroll(nSBCode, nPos, pScrollBar);
}

void CNCLFormBar::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	int i, fldno;
	UINT fid;
	CWnd *wnd;
	if (pScrollBar==NULL)
		return CNCLDialogBar::OnHScroll(nSBCode, nPos, pScrollBar);
	if ((pScrollBar->IsKindOf(RUNTIME_CLASS(CNCLSliderCtrl)))==0)
		return CNCLDialogBar::OnHScroll(nSBCode, nPos, pScrollBar);

	CNCLSliderCtrl* pSlider = (CNCLSliderCtrl*)pScrollBar;		
	pSlider->ReflectedScrollMessage();
	int pos = pSlider->GetPos();

	fldno = -1;
	for (i=0; i<m_fldno; i++)
	{
		if (m_type[i]!=24)
			continue;
		fid = m_idtable[i+m_dspno];
		wnd = GetDlgItem(fid);
		if (pScrollBar->m_hWnd == wnd->m_hWnd)
		{
			fldno = i;
			break;
		}
	}
	if (fldno == -1)
		return CNCLDialogBar::OnHScroll(nSBCode, nPos, pScrollBar);

	Focused_frmid = m_frmid;
	Focused_frm_fldno = fldno;
/*
......set current form too
*/
//	uw_ntset_curform_id(m_frmid);

	
	CNCLDialogBar::OnHScroll(nSBCode, nPos, pScrollBar);
}

/* End of wsntformbar.cpp */
#endif

