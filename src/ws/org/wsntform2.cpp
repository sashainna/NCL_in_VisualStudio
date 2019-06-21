#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**
**   FILE NAME: wsntform2.cpp
** 
**	 Description - Functions and implementations for
**		CNCLForm class (NCL forms) and some form functions
**
**	 CONTAINS:
**		class functions of CNCLForm class
**		together with wsntform.cpp, it will
**		includes all CNCLForm class functions
**		extern "C" void uw_dispfrm_wait()
**		extern "C" void uw_ntset_frmpocket()
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntform2.cpp , 25.27
**    DATE AND TIME OF LAST  MODIFICATION
**			11/22/17 , 11:32:43
**********************************************************************
*/
#include "wsntstdafx.h"
#include <dwmapi.h>
#include <math.h>
#include "wsntctl.h"
#include "wsntframe.h"
#include "wsntform.h"
#include "wsntcfunc.h"
#include "xenv1.h"
#include "wsgl.h"
#include "ddef.h"
#include "dmark.h"
#include "udfmracs.h"
#include "dinput.h"
#include "lcom.h"
#include "xenv1.h"
#include "mdcpln.h"
#include "wsntclrdlg.h"
#include "wsntSliderCtrl.h"

#define FORM_STRING		1
#define FORM_PICK		2
#define FORM_LOCATE		3
#define FORM_RECORD		4
#define FORM_LABEL		5
#define FORM_SUBSCR		6

#define UD_FORMFUZZ (UU_REAL) 1.0e-6
extern "C" UD_FSTRUCT *UD_dispfrm[60];

extern "C" int ud_set_traverse_mask(int fieldno, int val);
extern "C" int UW_auto_cursor;
extern "C" int uw_ntset_curpos(int x, int y);
extern "C" int uw_ntsetcursor(int cursor);
extern "C" int uw_ntgetcur_cursor();
extern "C" int ud_updatews(Gregen);

extern CMainFrame *NCL_MainFrame;
extern "C" UD_METHOD UD_initfrm_intry;	
extern CWnd *UW_display_frmwin[60];
extern CNCLForm *UW_display_frm[60];
extern "C" int uw_ntdispmsg (char *msg);
extern "C" int ncl_parse_scalar_values(char *, char*, int);
extern "C" int ncl_get_sclar_frm();

extern char UW_formdlg_font[];
static int current_chk_field = -1;
extern "C" ACCEL *UZ_ncl_accel;
extern "C" int UZ_ncl_accelnum;
extern "C" ACCEL UZ_form_hotkey[4];
extern "C" int UD_form_bypick;
extern "C" int uw_glhicolor;

int Focused_frmid = -1;
int Focused_frm_fldno = -1;
extern "C" int MSLite;

extern "C" UD_FSTAT ud_scatodas(UD_DASDATA *dsda, UD_DASIN *de, int dtyp);
extern "C" int ud_read_posform(char*, int*, int*, int*, int*, int*, int*);
int CALLBACK SortFunc(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort);

extern "C" void uw_ntset_curform_id(int id);
extern "C" void ncl_reset_pikgeom();
extern "C" int ud_free_tlist(UD_TLIST *formlist);
 

#define stricmp _stricmp

//temp here for yurong
//#define FORM_ADJUST

static float S_map_ratx = 2.0;
static float S_map_raty = 2.0;
/*
......we have our own dialog map function because in dialog design the view and the NCL form (dialog) is not mapped right
*/
void uw_MapDialogRect(CRect &rect, CDialog *wnd)
{
#ifdef FORM_ADJUST
	rect.left = S_map_ratx*rect.left;
	rect.right = S_map_ratx*rect.right;
	rect.top = S_map_raty*rect.top;
	rect.bottom = S_map_raty*rect.bottom;
	return;
#endif
	wnd->MapDialogRect(&rect);
}

void uw_revMapDialogRect(CRect &rect, int baseunitX, int baseunitY)
{
#ifdef FORM_ADJUST
	rect.left =rect.left/S_map_ratx;
	rect.right = rect.right/S_map_ratx;
	rect.top = rect.top/S_map_ratx;
	rect.bottom = rect.bottom/S_map_ratx;
	return;
#endif
	rect.left = (rect.left*4)/baseunitX;
	rect.top = (rect.top*8)/baseunitY;				
	rect.right = (rect.right*4)/baseunitX;
	rect.bottom = (rect.bottom*8)/baseunitY;				
}

void uw_revMapLen(int &cx, int &cy, int baseunitX, int baseunitY)
{
#ifdef FORM_ADJUST
	cx = cx/S_map_ratx;
	cy = cy/S_map_raty;
	return;
#endif
	cx = (cx*4)/baseunitX;
	cy = (cy*8)/baseunitY;				
}
/*
......this function chnaged to allow custume color
*/
int uw_get_rgb (char *color_str, COLORREF &color) 
{
/*
	if (stricmp (color_str, "WHITE")==0)
		color = RGB (255,  255, 255);
	else if (stricmp (color_str, "YELLOW")==0)
		color = RGB (255,  255, 0);
	else if (stricmp (color_str, "BLUE")==0)
		color = RGB (0,  0, 255);
	else if (stricmp (color_str, "RED")==0)
		color = RGB (255,  0, 0);
	else if (stricmp (color_str, "GREEN")==0)
		color = RGB (0,  255, 0);
	else if (stricmp (color_str, "MAGENTA")==0)
		color = RGB (255,  0, 255);
	else if (stricmp (color_str, "CYAN")==0)
		color = RGB (0,  255, 255);
	else if (stricmp (color_str, "BLACK")==0)
		color = RGB (0,  0, 0);
	else if (stricmp (color_str, "BROWN")==0)
		color = RGB (184,  134, 11);
	else if (stricmp (color_str, "TAN")==0)
		color = RGB (210,  180, 140);
	else if (stricmp (color_str, "LTBLUE")==0)
		color = RGB (173,  216, 230);
	else if (stricmp (color_str, "SEAGREEN")==0)
		color = RGB (84,   255, 159);
	else if (stricmp (color_str, "ORANGE")==0)
		color = RGB (255,  165, 0);
	else if (stricmp (color_str, "PINK")==0)
		color = RGB (255,  195, 203);
	else if (stricmp (color_str, "PURPLE")==0)
		color = RGB (221,  160, 221);
	else if (stricmp (color_str, "GREY")==0)
		color = RGB (192,  192, 192);
*/
	int i,indx;
/*
.....then find custom color index if the color already defined
*/
	indx = -1;
	if (color_str[0]=='\0')
		return -1;
	for (i=0; i<64; i++)
	{
		if (stricmp(color_str, uw_color_name[i])==0)
		{
			indx = i;
			break;
		}
	}
	if (indx==-1)
	{
		return -1;
	}
	color = RGB (uw_color_table[indx][0],
				uw_color_table[indx][1],
				uw_color_table[indx][2]);
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  CreateDlgView(CCreateContext* pContext, CWnd *pParent, CRect& rect, int wID,
c					DLGTEMPLATE *dlgTempl,  CDialogItem dlgItem[MAX_FORMITEM])
c
c   FUNCTION:  This function create a dialog view 
c
c   INPUT:  pContext: create context to create view
c			pParent: parent window of the view
c			rect: initial pos and size
c			wID: initial ID
c			dlgTempl: dialog template for dialog view
c			dlgItem:  dialog item template for dialog view
c
c   OUTPUT: none
c
c***********************************************************************
*/
CWnd* CreateDlgView(CCreateContext* pContext, CWnd *pParent, CRect& rect, int wID,
					DLGTEMPLATE *dlgTempl,  CDialogItem dlgItem[MAX_FORMITEM])
{
	CWnd* pWnd = NULL;
	if (pContext != NULL)
	{
		if (pContext->m_pNewViewClass != NULL)
		{
			pWnd = (CWnd*)pContext->m_pNewViewClass->CreateObject();
			if (pWnd == NULL)
			{
				TRACE1("Error: Dynamic create of view %Fs failed\n", pContext->m_pNewViewClass->m_lpszClassName);
				return NULL;
			}
			ASSERT(pWnd->IsKindOf(RUNTIME_CLASS(CWnd)));
			if (pWnd->IsKindOf(RUNTIME_CLASS(CNCLFormScrollView)))
				((CNCLFormScrollView*)pWnd)->SetDlgTemp(dlgTempl, dlgItem);

			if (!pWnd->Create(NULL, NULL, AFX_WS_DEFAULT_VIEW, rect, pParent, wID, pContext))
			{
				TRACE0("Error: couldn't create view \n");
				return NULL;
			}
			pWnd->SendMessage(WM_INITIALUPDATE);
		}
	}
	return pWnd;
}

/***********************************************************************
c
c   SUBROUTINE:  NCL_form_SaveData()
c
c   FUNCTION:  This function save every field data 
c				in the form
c
c   INPUT:  none
c
c   OUTPUT: none
c	Return: 0: error
c			1: ssaved
c
c***********************************************************************
*/
int CNCLForm::NCL_form_SaveData()
{
	int i,j,k,status, flag, len;
	char num[4],buf[20], num2[8];
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
		case 9:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
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
/*
...added to record form data
...Yurong
*/
			if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				sprintf_s(num, sizeof(num),"%d", i);
				ud_rpwrform("DATA", num, string);
/*
.....we need save the list item text too if 'INPUT" field is set to FORM_RECORD
......because the whole list could be used as output (answer)
.....for example, when we do layer transfer (nclu_put_layer), if we don't save it, the playback 
.....don't know what those output list are
*/
				if (m_input[i]==FORM_RECORD)
				{
					sprintf_s(num2, sizeof(num2),"%d", list->num_item);
					ud_rpwrform("LIST", "START", num2);
					for (j=0; j<list->num_item; j++)
					{
						ud_rpwrform("LIST", num, list->item[j]);
					}
					ud_rpwrform("LIST", "DONE", num);
				}
			}
			break;
//		case 9:
//			break;
/*
.....COMBBOX, CHOICES, can't be edited.
*/
		case 2:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
			if (!((cmbbx->IsWindowEnabled()) && (cmbbx->IsWindowVisible())))
				break;
			*((m_fData.ud_data[i]).ud_delem.frmint) = cmbbx->GetCurSel();
			if ((m_init_data_type_flag)&&(m_data_type[i]==UD_DASSTRING))
			{
				cmbbx->GetLBText(*((m_fData.ud_data[i]).ud_delem.frmint), string) ;
				len = strlen(string);
				strcpy_s(m_fData.ud_data[i].ud_delem.frmstr, len+1, string);
			}
/*
...added to record form data
...Yurong
*/
			if (UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				if ((m_init_data_type_flag)&&(m_data_type[i]==UD_DASSTRING))
				{
					strcpy(buf, m_fData.ud_data[i].ud_delem.frmstr);
				}
				else
				{
					sprintf_s(num, sizeof(num),"%d", i);
					sprintf_s(buf,sizeof(buf),"%d",m_fData.ud_data[i].ud_delem.frmint[0]);
				}
				ud_rpwrform("DATA",num,buf);
			}
			break;
/*
......LIST BOX
*/
		case 5:
			listbx = (CListBox*)(m_pScrollView->GetDlgItem(fid));
			if (!((listbx->IsWindowEnabled()) && (listbx->IsWindowVisible())))
				break;
			listbx->GetWindowText(string, UX_MAX_PATH_LEN-1);
			len = strlen(string);
			list = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			if (list->answer!=NULL)
				strcpy_s(list->answer, len+1, string);
/*
...added to record form data
...Yurong
*/
			if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				sprintf_s(num,sizeof(num),"%d", i);
				ud_rpwrform("DATA", num, string);
/*
.....we need save the list item text too because the whole list could be used as output (answer)
.....for example, when we do layer transfer (nclu_put_layer), if we don't save it, the playback 
.....don't know what those output list are
*/
				if (m_input[i]==FORM_RECORD)
				{
					sprintf_s(num2,sizeof(num2),"%d", list->num_item);
					ud_rpwrform("LIST", "START", num2);
					for (j=0; j<list->num_item; j++)
					{
						ud_rpwrform("LIST", num, list->item[j]);
					}
					ud_rpwrform("LIST", "DONE", num);
				}
			}
			break;
/*
.....EDIT
*/
		case 3:
		case 11:
		case 16:
			cedt = (CEdit*)(m_pScrollView->GetDlgItem(fid));
			if (!((cedt->IsWindowEnabled()) && (cedt->IsWindowVisible())))
				break;
			cedt->GetWindowText(string, UX_MAX_PATH_LEN-1);
			status = form_ckdata(i,string, &flag);
			if (status==UU_FALSE)
				goto done;
/*
...added to record form data
...Yurong
*/
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
			cbut = (CButton*)(m_pScrollView->GetDlgItem(fid));
			if (!((cbut->IsWindowEnabled()) && (cbut->IsWindowVisible())))
				break;
			*((m_fData.ud_data[i]).ud_delem.frmint) = cbut->GetCheck();
/*
...added to record form data
...Yurong
*/
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
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
			if (!((cmbbx->IsWindowEnabled()) && (cmbbx->IsWindowVisible())))
				break;
			cmbbx->GetWindowText(string, UX_MAX_PATH_LEN-1);
			len = strlen(string);
			list = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			if (list->answer!=NULL)
				strcpy_s(list->answer, len+1,string);
			if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				sprintf_s(num,sizeof(num),"%d", i);
				ud_rpwrform("DATA", num, string);
/*
.....we need save the list item text too because the whole list could be used as output (answer)
.....for example, when we do layer transfer (nclu_put_layer), if we don't save it, the playback 
.....don't know what those output list are
*/
				if (m_input[i]==FORM_RECORD)
				{
					sprintf_s(num2,sizeof(num2),"%d", list->num_item);
					ud_rpwrform("LIST", "START", num2);
					for (j=0; j<list->num_item; j++)
					{
						ud_rpwrform("LIST", num, list->item[j]);
					}
					ud_rpwrform("LIST", "DONE", num);
				}
			}
			break;
/*
......LIST TABLE
*/
		case 17:
			listctl = (CNCLListCtrl*)(m_pScrollView->GetDlgItem(fid));
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
				sprintf_s(num,sizeof(num), "%d", i);
				sprintf_s(buf,sizeof(buf), "%d", tlist->answer);
				ud_rpwrform("DATA", num, buf);
/*
.....we need save the list item text too because the whole list could be used as output (answer)
.....for example, when we do layer transfer (nclu_put_layer), if we don't save it, the playback 
.....don't know what those output list are
*/
				if (m_input[i]==FORM_RECORD)
				{
					sprintf_s(num2,sizeof(num2),"%d", tlist->num_col);
					ud_rpwrform("TLIST", "STARTC", num2);
					for (j=0; j<tlist->num_col; j++)
					{
						sprintf_s(num2,sizeof(num),"%d", j);
						ud_rpwrform("TLIST", num2, tlist->col_label[j]);
					}
					sprintf_s(num2,sizeof(num2),"%d", list->num_item);
					ud_rpwrform("TLIST", "START", num2);
					for (j=0; j<tlist->num_item; j++)
					{
						for (k=0;k<tlist->data[j].itemnum;k++)
						{
							sprintf_s(num2,sizeof(num2),"%d", k);
							ud_rpwrform("TLIST", num2, tlist->data[j].data_items[k]);
						}
					}
					ud_rpwrform("TLIST", "DONE", num);
				}
			}
			break;
/*
......LIST TABLE
*/
		case 19:
			listctl2 = (CNCLListCtrl2*)(m_pScrollView->GetDlgItem(fid));
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
		CWnd *win = m_pScrollView->GetDlgItem(m_idtable[i+m_dspno]);
		if (win==NULL)
			return status;
		win->SetFocus();
		if ((m_type[i]==3)&&(m_type[i]==11)&&(m_type[i]==16))
		{
			((CEdit*)win)->SetSel(0,-1);
		}
		return status;
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
int CNCLForm::uw_ntform_redisplay()
{
	int i,j,defsel,len;
	char ldat[UX_MAX_PATH_LEN], temp[40];
	CButton* cbut;
	CComboBox* cmbbx;
	CEdit* cedt;
	char perc_str[10];
	CProgressCtrl* cprocess;
	int defchc, perc;
	int oldint;
	UD_LIST *list;
	CString tempstr;
	UINT fid;
/*
......only if form exist
*/
	if (IsWindow(m_hWnd)==false)
		return 0;
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
		{
			set_display_mask(i+m_dspno, 0);
			continue;
		}
/*
......only update on the page
*/
		if ((m_fStruct.input_flds[i].section!=m_pScrollView->m_selsec)&&(m_fStruct.input_flds[i].section!=-1))
			continue;
		fid =  m_idtable[i+m_dspno];
		switch(m_type[i])
		{
/*
.....CHOICES
*/
		case 2:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
			defchc = m_fData.ud_data[i].ud_delem.frmint[0];
			oldint = cmbbx->GetCurSel();
			cmbbx->SetCurSel(defchc);
			if (oldint!=defchc)
				SendMessage(CBN_VIEWSELCHANGE, (WPARAM)fid);
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
			cedt = (CEdit*)(m_pScrollView->GetDlgItem(fid));
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
			tempstr = ldat;
			((CEdit*)(m_pScrollView->GetDlgItem(fid)))->SetWindowText(tempstr);
			break;
/*
.....CHECKBOX
*/
		case 7:
			cbut = (CButton*)(m_pScrollView->GetDlgItem(fid));
			defchc = m_fData.ud_data[i].ud_delem.frmint[0];
			oldint = cbut->GetCheck();
			cbut->SetCheck(defchc);
			if (oldint!=defchc)
				SendMessage(WM_VIEWCOMMAND, (WPARAM)fid);
			break;
		case 10:
/*
......picture area
*/
			if (m_fData.ud_data[i].dflg==2)
			{
				m_fData.ud_data[i].dflg = 1;
				if (m_init)
					OnPaint();
			}
			break;
		case 13:
			if (m_fData.ud_data[i].dflg!=2)
				break;
			m_fData.ud_data[i].dflg = 1;
			cprocess = (CProgressCtrl*)(m_pScrollView->GetDlgItem(fid));
			perc = m_fData.ud_data[i].ud_delem.frmint[0];
			cprocess->SetPos(perc);
			if (m_fStruct.input_flds[i].ud_datatyp==UD_DASINT)
			{
				fid =  m_idtable[i+m_dspno+m_fldno+m_framno+m_picno];
				sprintf_s(perc_str, sizeof(perc_str), "%d%%", perc);
				(m_pScrollView->GetDlgItem(fid))->SetWindowText(perc_str);
			}
			break;		
		case 15:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
			list = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			oldint = cmbbx->GetCurSel();
			defsel = -1;
			for (j=0; j<list->num_item; j++)
			{
				if (strcmp(list->item[j], list->answer)==0)
					defsel = j;
			}
			if ((oldint!=defsel)&&(defsel!=-1))
			{
/*
......the combo list could be reset and the defsel be the new selection and set yet
......then SetCurSel will return fail, in this case, do not call the callback function
*/
				int ret = cmbbx->SetCurSel(defsel);
				if (ret>=0)	
					SendMessage(CBN_VIEWSELCHANGE, (WPARAM)fid);
			}
			break;
		case 8:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
			list = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			oldint = cmbbx->GetCurSel();
			defsel = -1;
			for (j=0; j<list->num_item; j++)
			{
				if (strcmp(list->item[j], list->answer)==0)
					defsel = j;
			}
			if ((oldint!=defsel)&&(defsel!=-1))
			{
				cmbbx->SetCurSel(defsel);
				SendMessage(CBN_VIEWSELCHANGE, (WPARAM)fid);
			}
			break;
		case 5:
		case 9:
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
			m_pScrollView->Reset_picture(i, m_fStruct.picture_flds[i].fname);
		}
		m_fStruct.picture_flds[i].upt_flag = 0;
/*
.....hide or show the picture
*/
		fid = m_idtable[i+m_fldno+m_dspno+m_framno];
		if (m_fStruct.picture_flds[i].show_flag)
		{
			if ((m_fStruct.picture_flds[i].section!=m_pScrollView->m_selsec)&&(m_fStruct.input_flds[i].section!=-1))
				m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_HIDE);
			else
				m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_SHOW);
		}
		else
			m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_HIDE);
	}
	return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  InitFormDialog()
c
c   FUNCTION:  This function initialize every fields data
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNCLForm::InitFormDialog()
{
	int i,j,cont,fldno, k, m;
	CButton* cbut;
	CComboBox* cmbbx;
	CListBox* listbx;
	CEdit* cedt;
	CWnd *cwin;
	char perc_str[10];
	CProgressCtrl *cprocess;
	int *defchc,defsel, perc,len, status;
	UD_LIST *list;
	char string[UX_MAX_PATH_LEN];
	CNCLListCtrl *listctl;
	CNCLListCtrl2 *listctl2;
	UD_TLIST *tlist;
	UD_DLIST *dlist;
	UD_SELECT_STR *dselect;
	CRect rect;
	int wid;
	int pwid;
	CClientDC *list_dc;
	CSize sizeText;
	COLORREF bcolor;
	int init_disp = 0;

	m_init = 0;
	fldno = m_frmid;

	if (UD_initfrm_intry!=NULL)
	{
		status = (*UD_initfrm_intry)(&fldno, &(m_fData.ud_data[0].ud_delem),UD_TFWD);
		if (status!=-1)
			init_disp = 1;
		UD_initfrm_intry = NULL;
	}
/*
.....initialize form data from input field #1 to field #m_fldno
*/
	for (i=0; i<m_fldno; i++)
	{
/*
......don't initialize hide field and executable their callbacks
*/
/*
......we still need initialize hide field because when we enable the display-attribute of the field,
......we don't need initial the field again
......BUT don't executable their callbacks
*/
//		if (m_disp_mask[i+m_dspno]==0)
//			continue;
		UINT fid =  m_idtable[i+m_dspno];
/*
......update prompt label
*/
		if (m_fStruct.input_flds[i].ud_echo==2)
		{
			reset_fldlabel(i, m_fStruct.input_flds[i].prompt);
			m_fStruct.input_flds[i].ud_echo = 0;
		}
		switch(m_type[i])
		{
/*
.....COMBBOX, SIMPLE and DROPDOWN, can be edited
*/
		case 8:
		case 9:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
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
			{
				cmbbx->SetCurSel(defsel);
			}
			if (m_disp_mask[i+m_dspno]==0)
				SendMessage(CBN_VIEWSELCHANGE, (WPARAM)fid);
/*
......set the horizen text extent to longest text line
*/
			cmbbx->SetHorizontalExtent((UINT)(wid*0.95));
			delete list_dc;
			break;
/*
.....COMBBOX, CHOICES, can't be edited.
*/
		case 2:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
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
					if ((*defchc<0)||(*defchc>m_formchcnum[i]))
						cmbbx->SetCurSel(0);
					else
						cmbbx->SetCurSel(*defchc);
				}	
			}
			if (m_disp_mask[i+m_dspno]==0)
				SendMessage(CBN_VIEWSELCHANGE, (WPARAM)fid);
			break;
/*
......LIST BOX
*/
		case 5:
			listbx = (CListBox*)(m_pScrollView->GetDlgItem(fid));
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
			if (m_disp_mask[i+m_dspno]==0)
				SendMessage(LBN_VIEWSELCHANGE, (WPARAM)fid);
/*
......set the horizen text extent to longest text line
......for LISTBOX, we set /LEN/ -1 to active scrollbar,
......otherwise, there will be no horizontal scrolling bar
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
			cedt = (CEdit*)(m_pScrollView->GetDlgItem(fid));
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
			cbut = (CButton*)(m_pScrollView->GetDlgItem(fid));
			if ((m_fData.ud_data[i].dflg==0)||((m_fData.ud_data[i]).ud_delem.frmint == NULL))
				cbut->SetCheck(0);
			else
			{
				defchc = (m_fData.ud_data[i]).ud_delem.frmint;
				cbut->SetCheck(*defchc);
			}
			if (m_disp_mask[i+m_dspno]==0)
				SendMessage(WM_VIEWCOMMAND, (WPARAM)fid);
			break;
		case 13:
			cprocess = (CProgressCtrl*)(m_pScrollView->GetDlgItem(fid));
			if ((m_fData.ud_data[i]).ud_delem.frmint!=NULL)
				perc = *((m_fData.ud_data[i]).ud_delem.frmint);
			else
				perc = 1;
			cprocess->SetRange(1, 100);
			cprocess->SetPos(perc);
			if (m_fStruct.input_flds[i].ud_datatyp==UD_DASINT)
			{
				fid =  m_idtable[i+m_dspno+m_fldno+m_framno+m_picno];
				sprintf_s(perc_str,sizeof(perc_str), "%d%%", perc);
				(m_pScrollView->GetDlgItem(fid))->SetWindowText(perc_str);
			}
			break;
		case 15:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
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
			{
				cmbbx->SetCurSel(defsel);
			}
			else
				cmbbx->SetCurSel(0);
			if (m_disp_mask[i+m_dspno]==0)
				SendMessage(CBN_VIEWSELCHANGE, (WPARAM)fid);
/*
......set the horizen text extent to longest text line
*/
			cmbbx->SetHorizontalExtent((UINT)(wid*0.95));
			delete list_dc;
			break;
/*
......LIST TABLE
*/
		case 17:
			tlist = (UD_TLIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			listctl = (CNCLListCtrl*)(m_pScrollView->GetDlgItem(fid));
/*
.....use same length for all column now, may be use diff value by 
.....use form struct value later
*/
			listctl->GetWindowRect(&rect);
			if (tlist->num_col>0)
				wid = rect.Width()/tlist->num_col;
/*
.....we need clear listbox first
*/
			listctl->DeleteAllItems();

			LVCOLUMN lvColumn;
/*
.....the first column of HDF_SPLITBUTTON type is not working, so I have to
.....add a dummy column first, then delete it in order for making HDF_SPLITBUTTON
.....type working for column 0
*/
			lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
			if (m_fStruct.input_flds[i].ud_flen==0)
				lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON;
			else
				lvColumn.fmt = LVCFMT_LEFT;
			lvColumn.iImage = -1;
			lvColumn.cx = wid;
			lvColumn.pszText = _T("dummy");
			listctl->InsertColumn(0, &lvColumn);
			for (k=0; k<tlist->num_col; k++)
			{
				lvColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
				if (m_fStruct.input_flds[i].ud_flen==0)
					lvColumn.fmt = LVCFMT_LEFT | HDF_SPLITBUTTON;
				else
					lvColumn.fmt = LVCFMT_LEFT;
				lvColumn.iImage = -1;
				lvColumn.cx = wid;
				lvColumn.pszText = _T(tlist->col_label[k]);
				listctl->InsertColumn(k+1, &lvColumn);
//				listctl->InsertColumn(k, tlist->col_label[k],  LVCFMT_LEFT, wid);
			}
/*
.....delete dummy column
*/
			listctl->DeleteColumn(0);
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
			listctl->SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES);
/*
......have to set the focus to set a selection
*/
			listctl->SetFocus();
			if ((tlist->answer>=0)&&(tlist->answer<tlist->num_item))
			{
				listctl->SetItemState (tlist->answer, LVIS_SELECTED, LVIS_SELECTED);
				listctl->EnsureVisible(tlist->answer,0);
			}
			if ((m_sortfunc[i]!=NULL)&&(tlist->sort>=0)&&(tlist->sort<=tlist->num_col))
			{
				if (m_fStruct.input_flds[i].ud_fprec==0)
					listctl->SortItems((PFNLVCOMPARE)SortFunc, tlist->sort);
			}
			listctl->SetTlist(tlist);
			break;
		case 19:
			dlist = (UD_DLIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			listctl2 = (CNCLListCtrl2*)(m_pScrollView->GetDlgItem(fid));
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
//			listctl2->SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES|listctl2->GetExtendedStyle());
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
			if ((m_sortfunc[i]!=NULL)&&(dlist->sort>=0)&&(dlist->sort<=dlist->num_col))
			{
				if (m_fStruct.input_flds[i].ud_fprec==0)
					listctl2->SortItems((PFNLVCOMPARE)SortFunc, dlist->sort);
			}
			break;
		case 23:
			dselect = (UD_SELECT_STR *)((m_fData.ud_data[i]).ud_delem.frmint);
			if ((dselect->color<0)||(dselect->color>255))
				break;
			if (dselect->color<0)
			{
				bcolor = RGB(255, 0, 0);
			}
			else
			{
				bcolor = RGB(uw_color_table[dselect->color][0], 
							uw_color_table[dselect->color][1], 
							uw_color_table[dselect->color][2]);
			}
			m_pScrollView->set_button_color2(i, bcolor, bcolor);
			break;
		}
		if (m_trav_mask[i] == 0)
		{
			cwin = (m_pScrollView->GetDlgItem(fid));
			cwin->EnableWindow(FALSE);
			if ((m_type[i]==3)||(m_type[i]==2)||(m_type[i]==8)||(m_type[i]==16)
				||(m_type[i]==9)||(m_type[i]==10)||(m_type[i]==11)||(m_type[i] == 18)
				||(m_type[i]==13)||(m_type[i]==14) || (m_type[i] == 15)|| (m_type[i] == 23))
			{
				cwin = m_pScrollView->GetDlgItem(fid-1);
				cwin->EnableWindow(FALSE);
			}
			if (m_type[i]==13)
			{
				fid =  m_idtable[i+m_dspno+m_fldno+m_framno+m_picno];
				(m_pScrollView->GetDlgItem(fid))->EnableWindow(FALSE);
			}
		}
	}
	int first=1;
	for (i=0; i<m_fldno; i++)
	{		
		UINT fid =  m_idtable[i+m_dspno];
		if (m_disp_mask[i+m_dspno]==0)
		{
			cwin = (m_pScrollView->GetDlgItem(fid));
			cwin->ShowWindow(SW_HIDE);
			if ((m_type[i]==3)||(m_type[i]==2)||(m_type[i]==8)||(m_type[i]==16)
				||(m_type[i]==9)||(m_type[i]==10)||(m_type[i]==11)
				||(m_type[i]==13)||(m_type[i]==14) || (m_type[i] == 15)
				|| (m_type[i]==18)|| (m_type[i] == 23))
			{
/*
......hide prompt label
*/
				cwin = (m_pScrollView->GetDlgItem(fid-1));
				cwin->ShowWindow(SW_HIDE);
			}
			if (m_type[i]==13)
			{
				fid =  m_idtable[i+m_dspno+m_fldno+m_framno+m_picno];
				(m_pScrollView->GetDlgItem(fid))->ShowWindow(SW_HIDE);
			}
		}
		else if ((first) && (m_trav_mask[i] == 1) && (m_type[i]!=11))
		{
			cwin = (m_pScrollView->GetDlgItem(fid));
			cwin->SetFocus();
			first = 0;
		}
	}
	for (i=0;i<m_fStruct.n_display_fields;i++)
	{
		if (m_fStruct.ud_display_mask[i] == 1)
			set_display_mask(i, 1);
		if (m_fStruct.ud_display_mask[i] == 0 )
			set_display_mask(i, 0);
	}
	if (init_disp)
		uw_ntform_redisplay();			
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
......we are not defined our own form 'return'
......but using window's default
*/
/***********
	int no_return = 0 no_tab=0, no_stab=0;
	for (i=0; i<m_form_accelnum; i++)
	{
		if (m_form_accel[i].key==VK_RETURN)
		{
			no_return = 1;
/*
......Set default button	VK_RETURN
*/
/***********
			SetDefID(-1);
		}
		if ((m_form_accel[i].key==VK_TAB) && 
			(m_form_accel[i].fVirt==(FNOINVERT | FVIRTKEY)))
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
/***********
		SetDefID(IDF_FORM_RETURN);
	}
********************/
/*
......we still need our handle the TAB function by our own
......because for dialog_scroll_view, the TAB function does
......not work by its own even we used WS_TABSTOP style
*/
	int no_tab=0, no_stab=0;
	for (i=0; i<m_form_accelnum; i++)
	{
		if ((m_form_accel[i].key==VK_TAB) && 
			(m_form_accel[i].fVirt==(FNOINVERT | FVIRTKEY)))
			no_tab = 1;
		if ((m_form_accel[i].key==VK_TAB) && 
			(m_form_accel[i].fVirt==(FNOINVERT | FVIRTKEY | FSHIFT)))
			no_stab = 1;
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
	if (m_form_accelnum!=0)
		m_accel = CreateAcceleratorTable(m_form_accel, m_form_accelnum);
	m_init = 1;
	SetDefID(IDF_FORM_RETURN);
}
/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog()
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
BOOL CNCLForm::OnInitDialog()
{
//	CMFCVisualManager::SetDefaultManager(RUNTIME_CLASS(CMFCVisualManagerWindows));
	CDialog::OnInitDialog();

	CCreateContext cc;

	cc.m_pNewViewClass = RUNTIME_CLASS(CNCLFormScrollView);
	cc.m_pCurrentDoc = NULL;
	cc.m_pNewDocTemplate = NULL;
	cc.m_pLastView = NULL;
	cc.m_pCurrentFrame = (CFrameWnd * )this;
	m_pScrollView = (CNCLFormScrollView*)CreateDlgView(&cc, this, CRect(0, 0, 0, 0), 0, &m_dlgTempl, m_rgDlgItem);
	if (m_pScrollView == NULL)
		EndDialog(IDCANCEL);

	m_pScrollView->setdlgtyp(m_dlgtyp);
	m_pScrollView->setsecbut(m_secno);

	CRect windowRect, ScrollRect, windowRect2, windowRect3, srect;
	m_pScrollView->GetWindowRect(&ScrollRect);
	m_pScrollView->GetWindowRect(&srect);

	CRect rect, rect1, rect2;
	int borderx, bordery;

	S_map_ratx = (ScrollRect.Width()*1.0)/(m_dlgTempl.cx*1.0);
	S_map_raty = (ScrollRect.Height()*1.0)/(m_dlgTempl.cy*1.0);

	long base = GetDialogBaseUnits();
	short delta = HIWORD(base);
	short delta2 = LOWORD(base);

	GetWindowRect(&rect1);
	int save_cx = rect1.Width();
	int save_cy = rect1.Height();

	GetDlgItem(IDC_SCROLL_FRAME)->GetWindowRect(&rect);
	borderx = GetSystemMetrics(SM_CXFRAME); 
	bordery = GetSystemMetrics(SM_CYCAPTION);

	SystemParametersInfo(SPI_GETWORKAREA,0,rect2,0);

	int maxhgt = rect2.Height() - 100;
	int maxwid = rect2.Width() - 2*borderx;
	if (ScrollRect.Height()>maxhgt)
	{
		ScrollRect.bottom = ScrollRect.top + maxhgt;
		ScrollRect.right += GetSystemMetrics(SM_CXVSCROLL);
	}
	if (ScrollRect.Width()>maxwid)
		ScrollRect.right = ScrollRect.left + maxwid;

	ScreenToClient(rect);
	if (m_dlgtyp==1)
	{
		rect.top += 6;
		rect.left = 10;
		rect.right = rect.left + ScrollRect.Width() + 4;
		rect.bottom = rect.top + ScrollRect.Height() + 15;
	}
	else
	{
		rect.top = 2;
		rect.left = 8;
		rect.right = rect.left + ScrollRect.Width() + 15;
		rect.bottom = rect.top + ScrollRect.Height() + 8;
	}
	windowRect = rect;
	GetDlgItem(IDC_SCROLL_FRAME)->MoveWindow(&rect);

	if (m_dlgtyp==1)
	{
		rect.left += 2;
		rect.top += 8;
		rect.right -= 2;
		rect.bottom -= 2;
	}
	m_pScrollView->MoveWindow(&rect);
	windowRect3 = windowRect;
/*
......leave 25 unit for active bar area height
*/
	CRect temp1 (0,0, 20, 20);
	CRect temp2 (0,0, 22, 22);
	CRect temp3 (0,0, 25, 25);

	MapDialogRect(&temp1);
	MapDialogRect(&temp2);
	MapDialogRect(&temp3);
/*
.....adjust for font size change
*/
	int font_adj = UW_form_fontsize - 8.0;
	if (m_dlgtyp==1)
	{
		windowRect.bottom += temp3.Height() + bordery + borderx + 5 + font_adj;
		windowRect.right += 8 + 13;
	}
	else
		windowRect.bottom += temp1.Height() + bordery + borderx + 5 + font_adj;
	windowRect.right += 2*borderx;
	Adjust_pos(windowRect);
	MoveWindow(windowRect);
/*
.....adjust because of window's shifting problem
*/
	int delta_cx = 0;
	int delta_cy = 0;
	RECT r;
	HRESULT ret = DwmGetWindowAttribute(m_hWnd, DWMWA_EXTENDED_FRAME_BOUNDS, &r, sizeof(r));
	if (ret==S_OK)
	{
		GetWindowRect(&rect1);
		delta_cx = (r.right - r.left) - (rect1.right - rect1.left);
		delta_cy = (r.bottom - r.top) - (rect1.bottom - rect1.top);
		windowRect.left += delta_cx/2;
		windowRect.right -= delta_cx/2;
		windowRect.top += delta_cy/2;
		windowRect.bottom -= delta_cy/2;
		MoveWindow(windowRect);
	}
	int dx = windowRect.Width() - save_cx;
	int dy = windowRect.Height() - save_cy;
/*
......we are not center the form now but use user define position
*/
/*
.....center the dialog
*/
//	CenterWindow();
	GetClientRect(windowRect2);
	int cx = windowRect2.Width();
	int butlen;
	if (m_fStruct.helpflag==1)
	{
		if (m_disptype==1)
			butlen = (cx - 25)/2;
		else
			butlen = (cx - 25)/3;
	}
	else
	{
		if (m_disptype==1)
			butlen = cx - 25;
		else
			butlen = (cx - 25)/2;
	}
	windowRect2.top = windowRect2.bottom - temp2.Height() + 8;
	windowRect2.bottom -= 5;
	windowRect2.right -= 5;
	windowRect2.left = windowRect2.right - butlen;
	CWnd* pChildWnd;

	CFont aFont;
	aFont.CreatePointFont(UW_form_fontsize*10, UW_formdlg_font);
	if (m_fStruct.helpflag==1)
	{
		pChildWnd = (CWnd*)GetDlgItem(IDC_FORMHELP);
		if (pChildWnd!=NULL)
		{
			pChildWnd->MoveWindow(windowRect2);
			pChildWnd->SetFont(&aFont);	
		}
		windowRect2.right = windowRect2.left - 5;
		windowRect2.left = windowRect2.right - butlen;
	}
	else
	{
		pChildWnd = (CWnd*)GetDlgItem(IDC_FORMHELP);
		if (pChildWnd!=NULL)
			pChildWnd->ShowWindow(SW_HIDE);
	}
	pChildWnd = (CWnd*)GetDlgItem(IDC_FORMCANCEL);
	if (pChildWnd!=NULL)
	{
		pChildWnd->MoveWindow(windowRect2);
		pChildWnd->SetFont(&aFont);	
		if (m_disptype==1)
			pChildWnd->SetWindowText("CLOSE");
	}
	windowRect2.right = windowRect2.left - 5;
	windowRect2.left = windowRect2.right - butlen;
	pChildWnd = (CWnd*)GetDlgItem(IDC_FORMACCEPT);
	if (pChildWnd!=NULL)
	{
		pChildWnd->SetFont(&aFont);	
		if (m_disptype==1)
			pChildWnd->ShowWindow(SW_HIDE);
		else
			pChildWnd->MoveWindow(windowRect2);
	}
	SetWindowText(m_title);
/*
.....adding function to adject form's /COLOR/fg, bg, 
...../FONT/scale and /JUSTIFY/ vlaue
*/
	InitAdjust();
	if (m_dlgtyp==0)
	{
/*
......adjust box1 and box2
*/
		CRect save_rect;
		GetDlgItem(IDC_SCROLL_FRAME)->GetWindowRect(&save_rect);
		ScreenToClient(save_rect);

		CRect rect_b;
		m_pScrollView->GetDlgItem(IDC_DLG_BOX1)->GetWindowRect(&rect_b);
		m_pScrollView->ScreenToClient(rect_b);
		rect_b.bottom = save_rect.bottom-6;
		m_pScrollView->GetDlgItem(IDC_DLG_BOX1)->MoveWindow(&rect_b);

		m_pScrollView->GetDlgItem(IDC_DLG_BOX2)->GetWindowRect(&rect_b);
		m_pScrollView->ScreenToClient(rect_b);
		rect_b.bottom = save_rect.bottom-6;
		rect_b.right = save_rect.right-15-10;
		m_pScrollView->GetDlgItem(IDC_DLG_BOX2)->MoveWindow(&rect_b);
		if (m_dlgtyp==0)
			GetDlgItem(IDC_SCROLL_FRAME)->ShowWindow(SW_HIDE);
		InitSectionButtons();
		OnSecButton(0);
	}
	InitFormDialog();
	Focused_frmid = m_frmid;
	Focused_frm_fldno = 0;
/*
......if the scalar form is open, enable it
*/
	int sfrm = ncl_get_sclar_frm();
	if ((sfrm!=-1)&&(UW_display_frmwin[sfrm]!=NULL))
		::EnableWindow(UW_display_frmwin[sfrm]->GetSafeHwnd(), TRUE);
	GetWindowRect(&m_oldRect);
	GetWindowRect(&m_Rect);	

	int i;
	UINT fid;
	if (m_fldno>0)
	{
		m_fldrec = (CRect*) malloc (m_fldno*sizeof(CRect));
		m_fldlrec = (CRect*) malloc (m_fldno*sizeof(CRect));
	}
	if (m_framno>0)
		m_frmrec = (CRect*) malloc (m_framno*sizeof(CRect));
	if (m_dspno>0)
		m_dsprec = (CRect*) malloc (m_dspno*sizeof(CRect));
	if (m_picno>0)
		m_picrec = (CRect*) malloc (m_picno*sizeof(CRect));

	for (i=0; i<m_fldno; i++)
	{
		fid = m_idtable[i+m_dspno];  
		pChildWnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		pChildWnd->GetWindowRect(&(m_fldrec[i]));	
		m_pScrollView->ScreenToClient(m_fldrec[i]);
		if ((m_type[i]==3)||(m_type[i]==2)||(m_type[i]==8)
			||(m_type[i]==9)||(m_type[i]==10) ||(m_type[i]==11)
			||(m_type[i]==13)||(m_type[i]==14) || (m_type[i] == 15)
			||(m_type[i]==16)||(m_type[i]==18)|| (m_type[i] == 23))
		{
/*
.....we need save the prompt button window too
*/
//			if (m_input[i] == FORM_STRING)
			{
				fid =  m_idtable[i+m_dspno] - 1;
				pChildWnd = (m_pScrollView->GetDlgItem(fid));
				pChildWnd->GetWindowRect(&(m_fldlrec[i]));	
				m_pScrollView->ScreenToClient(m_fldlrec[i]);
			}
		}
	}
	for (i=0; i<m_framno; i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno];  
		pChildWnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		pChildWnd->GetWindowRect(&(m_frmrec[i]));	
		m_pScrollView->ScreenToClient(m_frmrec[i]);
	}
	for (i=0; i<m_dspno; i++)
	{
		fid = m_idtable[i];  
		pChildWnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		pChildWnd->GetWindowRect(&(m_dsprec[i]));	
		m_pScrollView->ScreenToClient(m_dsprec[i]);
	}
	for (i=0; i<m_picno; i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno+m_framno];  
		pChildWnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		pChildWnd->GetWindowRect(&(m_picrec[i]));	
		m_pScrollView->ScreenToClient(m_picrec[i]);
	}
	if (m_dlgtyp==0)
	{
		m_pScrollView->GetDlgItem(IDC_DLG_BOX1)->GetWindowRect(&m_oldbox1);	
		m_pScrollView->ScreenToClient(m_oldbox1);
		m_pScrollView->GetDlgItem(IDC_DLG_BOX2)->GetWindowRect(&m_oldbox2);	
		m_pScrollView->ScreenToClient(m_oldbox2);
	}
	if (m_disptype==1)
		m_def_id = IDC_FORMCANCEL;
	else
		m_def_id = IDC_FORMACCEPT;
/*
......resize/position form again using data read from "form_name.pos"
*/
	int att_win, ref,x,y,cy;
	att_win = m_fStruct.att_win;
	ref = m_fStruct.ref;
	x = y = 0;
	GetWindowRect(&rect);
	cx = rect.Width();
	cy = rect.Height();
	int status = ud_read_posform(m_filename, &x, &y, &cx, &cy, &att_win, &ref);
	if (status==0)
	{
		Adjust_pos2(rect, x, y, cx, cy, att_win, ref);
		MoveWindow(rect);
	}
/*
.....check if the form window is off screen, if yes, we need adject to include
.....at least half of the form window is showing
*/
	int maxx = GetSystemMetrics(SM_CXSCREEN);
	int maxy = GetSystemMetrics(SM_CYSCREEN);
	cy = 10*GetSystemMetrics(SM_CYFRAME);
	cx = 10*GetSystemMetrics(SM_CXFRAME);
	dy = dx = 0;
	GetWindowRect(&rect);
	if (rect.bottom<cy)
		dy = cy - rect.bottom;
	if (rect.top>(maxy-cy))
		dy = (maxy-cy) - rect.top;
	if (rect.right<cx)
		dx = cx - rect.right;
	if (rect.left>(maxx-cx))
		dx = (maxx-cx) - rect.left;
	rect.top += dy;
	rect.bottom += dy;
	rect.left += dx;
	rect.right += dx;
	MoveWindow(rect);
/*
......set the scrolling size as original whole size, not the window size
*/
	m_pScrollView->SetScrollSizes(MM_TEXT, CSize(srect.Width(), srect.Height()));
	return 0;
}	
/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks1(WPARAM wparm, LPARAM lparm)
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
LRESULT CNCLForm::FormUserCallbacks1(WPARAM wparm, LPARAM lparm)
{
	UINT id = (UINT)wparm;
	FormUserCallbacks0(id);
	return 0;
}
void CNCLForm::FormUserCallbacks0(UINT id)
{
	int fldno, i;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) 
		return;
/*
......only if form is showing, otherwise, the form value will be wrong
*/
	if (IsWindowVisible()==0) 
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
/*
.....why "don't execute the command if the field is on a edit field"
.....changed back because we want shortcut key of the form to be execute
.....in any field (changed when doing calculator)
.....Yurong 6/27/06
.....................
	if (lparm!=NULL)
	{
/*
.....from accelarator key
.....don't execute the command if the field is on a edit field
*/
/*********
		if (Focused_frmid==m_frmid)
		{
			if ((Focused_frm_fldno>=0) && (m_type[Focused_frm_fldno]==3))
				return 0;
		}
	}
**********/
	fldno = -1;
	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			break;
		}
	}
	if (fldno != -1)
	{
		Focused_frm_fldno = fldno;
/*
......set current form too
*/
		uw_ntset_curform_id(m_frmid);
	}
	Execute_command (id);
	return;
}
/**********************************************************************
**    I_FUNCTION :  Execute_command(UINT id)
**       execute command id.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
LRESULT CNCLForm::Execute_command(UINT id)
{
	CButton* chkbx;
	int i, fldno, pikloc,status;
	int sav_cursor, colordlg;
	UD_FSTAT stat;
	int jmpflag;
	int redisp = 0;
/*
.....when execute the callback function, the window must be still there
*/
	if (IsWindow(m_hWnd)==false)
		return 0;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;

	stat = UD_FLDOK;

	fldno = -1;
	pikloc = 0;
	colordlg = 0;
	for (i=0; i<m_fldno; i++)
	{
		if (m_idtable[i+m_dspno]==id)
		{
			fldno = i;
			if ((m_type[i]==18)||(m_type[i]==23))
				colordlg = 1;
			break;
		}
		else if (((m_type[i]==3)||(m_type[i]==23))&&(m_idtable[i+m_dspno]-1==id))
		{
			fldno = i;
			pikloc = 1;
			break;
		}
	}
	if (fldno == -1)
		return 0;
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag != 0)
		goto done;
again:;
	status = -1;
	if (pikloc == 1)
	{
		if (m_type[i]==23)
			status = FormSelect(fldno);
		else if (m_input[fldno]==FORM_PICK || m_input[fldno]==FORM_LABEL ||
			m_input[fldno]==FORM_SUBSCR) 
			status = FormPick(fldno);
		else
			status = FormLoc(fldno);
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
		chkbx = (CButton*)(m_pScrollView->GetDlgItem(id));
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
/*
......after execute the user function, we need check if the form still there
......it could close during the time executing function
......so check it before proceed
*/
//		if (IsWindow(m_hWnd)==false)
//		{	
//			goto done;
//		}
		if (stat==UD_FRMCLOSE)
			goto done;
		UD_form_bypick = 0;
		if (fldno!=-1)
			redisp = 1;		
/*
......should always reset to 1 (since the MFC will change it to 1 when mouse move)
......so we must be consistent
*/
		uw_ntsetcursor(1);
/*
.....Callback routine wants to
.....remain in pick mode
*/
		if (pikloc == 1 && stat == UD_FWD && m_type[fldno] == 3 &&
			(m_input[fldno]==FORM_PICK || m_input[fldno]==FORM_LABEL ||
			m_input[fldno]==FORM_SUBSCR|| (m_type[i]==23))) goto again;
	}
	if (redisp==1)
		uw_ntform_redisplay();			
	if ((stat==UD_BADREQ)&&(m_init))
/*
.....if it is bad, then stay the focus in current field
*/
	{
		CWnd *win = m_pScrollView->GetDlgItem(m_idtable[fldno+m_dspno]);
		win->SetFocus();
		if (m_type[fldno]==3)
			((CEdit*)win)->SetSel(0, -1);
	}	
/*
.....Update graphics window in case
.....graphics were output from form action
*/
done:;
	ud_updatews(UG_SUPPRESS);
	UD_UNMARK(jmpflag);
	return 0;
}
/**********************************************************************
**    I_FUNCTION :  FormUserCallback2 (WPARAM wparm, LPARAM lparm)
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
LRESULT CNCLForm::FormUserCallbacks2(WPARAM wparm, LPARAM lparm)
{
	UINT id = (UINT)wparm;
	int i, fldno, jmpflag;
	UD_FSTAT stat;
	int redisp = 0;

/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;
/*
......only if form is showing, otherwise, the form value will be wrong
*/
	if (IsWindowVisible()==0) 
		return 0;

	stat = UD_FLDOK;
/*
.....do not call callback function until form displayed
*/
//	if (m_init==0)
//		return 0;
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
		return 0;
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag != 0)
		goto done;
	if ((m_method_ret[fldno] & UD_TOGGLE &&
			m_method[fldno] != NULL)&&(m_init))
	{
		CListBox* lstbx;
		lstbx = (CListBox*)(m_pScrollView->GetDlgItem(id));
		int	indx = lstbx->GetCurSel();
		char lstr[UX_MAX_PATH_LEN];
		if (indx>=0)
			lstbx->GetText(indx, lstr);
		else
			lstr[0] = '\0';
		UD_DDATA seldata;
		seldata.frmstr = (char*)&lstr;
/*
.....reset pick segment list before callback function
*/
		ncl_reset_pikgeom();
		stat = (m_method[fldno])(&fldno, &seldata,UD_FLDOK);
		if (fldno!=-1)
			redisp = 1;		
		if (stat==UD_FRMCLOSE)
			goto done;
	}
	if (redisp==1)
	{
		uw_ntform_redisplay();
		ud_updatews(UG_SUPPRESS);
	}
	if ((stat==UD_BADREQ)&&(m_init))
/*
.....if it is bad, then stay the focus in current field
*/
	{
		CWnd *win = m_pScrollView->GetDlgItem(m_idtable[fldno+m_dspno]);
		win->SetFocus();
	}	
done:;
	UD_UNMARK(jmpflag);
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  FormUserCBLDblclk (WPARAM wparm, LPARAM lparm)
**       Combo box double click callback for all fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				WPARAM wparm, LPARAM lparm                   
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
LRESULT CNCLForm::FormUserCBLDblclk(WPARAM wparm, LPARAM lparm)
{
	UINT id = (UINT)wparm;
	int i, fldno, indx,len, jmpflag;
	CComboBox *cmbbx = NULL;
	CListBox* lstbx;
	UD_LIST *list_ans;
	UD_DDATA seldata;
	char lstr[UX_MAX_PATH_LEN];
	UD_FSTAT stat;
	int redisp = 0;
	if (id<=0)
		return 0;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;
/*
......only if form is showing, otherwise, the form value will be wrong
*/
	if (IsWindowVisible()==0) 
		return 0;

	stat = UD_FLDOK;
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
		return 0;
	
	if (m_type[fldno]==2)
	{
		return 0;
	}
	else if ((m_type[fldno]==8)||(m_type[fldno]==9))
	{
		cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(id));
		indx = cmbbx->GetCurSel();
		list_ans = (UD_LIST *)(m_fData.ud_data[fldno].ud_delem.frmint) ;
		cmbbx->GetLBText(indx, lstr);
		len = strlen(lstr);
		if (list_ans->answer!=NULL)
			strcpy_s(list_ans->answer, len+1, lstr);
	}

	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag != 0)
		goto done;
	if ((m_method_ret[fldno] & UD_TOGGLE &&
			m_method[fldno] != NULL)&&(m_init))
	{
/*
.....reset pick segment list before callback function
*/
		ncl_reset_pikgeom();
		if ((m_type[fldno]==5) || (m_type[fldno]==2) || (m_type[fldno]==17))
		{
/*			lstbx = (CListBox*)(m_pScrollView->GetDlgItem(id));
			indx = lstbx->GetCurSel();
			lstbx->GetText(indx, lstr);
			seldata.frmstr = (char*)&lstr;
			stat = (m_method[fldno])(&fldno, &seldata,UD_FLDOK);
*/
			goto done;
		}
		else if (m_type[fldno]==15)
		{
			UD_DDATA seldata;
			char lstr[UX_MAX_PATH_LEN];
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(id));
			indx = cmbbx->GetCurSel();
			cmbbx->GetLBText(indx, lstr);
			seldata.frmstr = (char*)&lstr;
			stat = (m_method[fldno])(&fldno, &seldata,UD_FLDOK);
		}
		else
		{
			UD_DDATA seldata;
			char lstr[UX_MAX_PATH_LEN];
			if (cmbbx!=NULL)
			{ 
				indx = cmbbx->GetCurSel();
				cmbbx->GetLBText(indx, lstr);
				seldata.frmstr = (char*)&lstr;
				stat = (m_method[fldno])(&fldno, &seldata,UD_FLDOK);
			}
			else
				goto done;
		}
		if (fldno!=-1)
			redisp = 1;			
	}
	if (stat==UD_FRMCLOSE)
		goto done;

	if (redisp==1)
	{
		uw_ntform_redisplay();
		ud_updatews(UG_SUPPRESS);
	}
	if ((stat==UD_BADREQ)&&(m_init))
/*
.....if it is bad, then stay the focus in current field
*/
	{
		CWnd *win = m_pScrollView->GetDlgItem(m_idtable[fldno+m_dspno]);
		win->SetFocus();
		UD_UNMARK(jmpflag);
		return 0;
	}	
/*
.....aceept the form
*/
done:;
	if (((m_type[fldno]==5) || (m_type[fldno]==8) || (m_type[fldno]==17))
		&& (m_fStruct.input_flds[fldno].ud_echo==1))
	{
		if (m_disptype==1)
			PostMessage(WM_COMMAND, IDC_FORMCANCEL);
		else
			PostMessage(WM_COMMAND, IDOK);
	}
	UD_UNMARK(jmpflag);
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks3(WPARAM wparm, LPARAM lparm)
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
LRESULT CNCLForm::FormUserCallbacks3(WPARAM wparm, LPARAM lparm)
{
	UINT id = (UINT)wparm;
	int i, fldno, indx,len, jmpflag;
	CComboBox *cmbbx;
	CListBox* lstbx;
	UD_LIST *list_ans;
	UD_DDATA seldata;
	char lstr[UX_MAX_PATH_LEN];
	UD_FSTAT stat;
	int redisp = 0;

/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;
/*
......only if form is showing, otherwise, the form value will be wrong
*/
	if (IsWindowVisible()==0) 
		return 0;
	stat = UD_FLDOK;
/*
.....do not call callback function until form displayed
*/
//	if (m_init==0)
//		return 0;
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
		return 0;
	
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag != 0)
		goto done;
	if (m_type[fldno]==2)
	{
		cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(id));
		*((m_fData.ud_data[fldno]).ud_delem.frmint) = cmbbx->GetCurSel();
	}
	else if ((m_type[fldno]==8)||(m_type[fldno]==9))
	{
		cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(id));
		indx = cmbbx->GetCurSel();
		list_ans = (UD_LIST *)(m_fData.ud_data[fldno].ud_delem.frmint) ;
		cmbbx->GetLBText(indx, lstr);
		len = strlen(lstr);
		if (list_ans->answer!=NULL)
			strcpy_s(list_ans->answer, len+1, lstr);
	}

	if ((m_method_ret[fldno] & UD_TOGGLE &&
			m_method[fldno] != NULL)&&(m_init))
	{
/*
.....reset pick segment list before callback function
*/
		ncl_reset_pikgeom();
		if (m_type[fldno]==2)
			stat = (m_method[fldno])(&fldno, &(m_fData.ud_data[fldno].ud_delem),UD_FLDOK);
		else if (m_type[fldno]==5)
		{
			lstbx = (CListBox*)(m_pScrollView->GetDlgItem(id));
			indx = lstbx->GetCurSel();
			lstbx->GetText(indx, lstr);
			seldata.frmstr = (char*)&lstr;
			stat = (m_method[fldno])(&fldno, &seldata,UD_FLDOK);
		}
		else if (m_type[fldno]==15)
		{
			UD_DDATA seldata;
			char lstr[UX_MAX_PATH_LEN];
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(id));
			indx = cmbbx->GetCurSel();
			cmbbx->GetLBText(indx, lstr);
			seldata.frmstr = (char*)&lstr;

			list_ans = (UD_LIST *)((m_fData.ud_data[i]).ud_delem.frmint);
			if (list_ans->answer!=NULL)
				strcpy(list_ans->answer, lstr);
			stat = (m_method[fldno])(&fldno, &seldata,UD_FLDOK);
		}
		else
		{
			UD_DDATA seldata;
			char lstr[UX_MAX_PATH_LEN];
			indx = cmbbx->GetCurSel();
			cmbbx->GetLBText(indx, lstr);
			seldata.frmstr = (char*)&lstr;
			stat = (m_method[fldno])(&fldno, &seldata,UD_FLDOK);
		}
		if (fldno!=-1)
			redisp = 1;			
	}
	if (stat==UD_FRMCLOSE)
		goto done;
	if (redisp==1)
	{
		uw_ntform_redisplay();
		ud_updatews(UG_SUPPRESS);
	}
	if ((stat==UD_BADREQ)&&(m_init))
/*
.....if it is bad, then stay the focus in current field
*/
	{
		CWnd *win = m_pScrollView->GetDlgItem(m_idtable[fldno+m_dspno]);
		win->SetFocus();
	}	
done:
	UD_UNMARK(jmpflag);
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks7(WPARAM wparm, LPARAM lparm)
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
LRESULT CNCLForm::FormUserCallbacks7(WPARAM wparm, LPARAM lparm)
{
	CWnd *old_def, *new_def;
	UINT id = (UINT)wparm;
	int jmpflag, fldno, stat, redisp=0;
/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return 0;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;
/*
......only if form is showing, otherwise, the form value will be wrong
*/
	if (IsWindowVisible()==0) 
		return 0;

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
		return 0;
	CWnd *win = m_pScrollView->GetDlgItem(m_idtable[fldno+m_dspno]);
	if (!((win->IsWindowEnabled()) && (win->IsWindowVisible())))
		return 0;
	Focused_frmid = m_frmid;
	Focused_frm_fldno = fldno;
/*
......set current form too
*/
	uw_ntset_curform_id(m_frmid);

	if (m_current_fld!=-1)
	{
		stat = chk_field(m_current_fld, 1, &redisp, UD_TFWD);
		if (stat!=-1)
			m_current_fld = -1;
		else
			return 0; 
	}
	if (redisp==1)
		uw_ntform_redisplay();
	m_current_fld = -1;

	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag != 0)
		goto done;
	old_def = GetDlgItem(m_def_id);
	if (m_type[fldno]==1)
	{
		SendMessage(DM_SETDEFID, (WPARAM)IDF_FORM_RETURN);
		if ((m_def_id!=id)&&(old_def!=NULL))
			old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
		m_def_id = id;
		UD_UNMARK(jmpflag);
		return 0;
	}
/*
......reset default button to OK/CLOSE if the focus is not button
*/
	if (m_disptype==1)
	{
		if ((m_def_id!=IDC_FORMCANCEL)&&(old_def!=NULL))
			old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
		SendMessage(DM_SETDEFID, (WPARAM)IDC_FORMCANCEL);
		m_def_id = IDC_FORMCANCEL;
	}
	else
	{
		if ((m_def_id!=IDC_FORMACCEPT)&&(old_def!=NULL))
			old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
		SendMessage(DM_SETDEFID, (WPARAM)IDC_FORMACCEPT);
		m_def_id = IDC_FORMACCEPT;
	}
	new_def = GetDlgItem(m_def_id);
	if (new_def!=NULL)
		new_def->SendMessage(BM_SETSTYLE, BS_DEFPUSHBUTTON, TRUE);
done:;
	UD_UNMARK(jmpflag);
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks6()
**       handle default button for dialog view.
**    PARAMETERS   
**       INPUT  : 
**				None                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLForm::FormUserCallbacks6()
{
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return;
/*
......only if form is showing, otherwise, the form value will be wrong
*/
	if (IsWindowVisible()==0) 
		return;
/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return;
	if (m_return_key==1)
	{
		m_return_key = 0;
		if ((m_def_id==IDC_FORMHELP) || (m_def_id==IDC_FORMACCEPT)
			|| (m_def_id==IDC_FORMCANCEL))
			PostMessage(WM_COMMAND, (WPARAM)m_def_id);
		else
			PostMessage(WM_VIEWCOMMAND, (WPARAM)m_def_id);
	}
}

/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks4(WPARAM wparm, LPARAM lparm)
**       edit text/ dropdown list text exit callback for all fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
LRESULT CNCLForm::FormUserCallbacks4(WPARAM wparm, LPARAM lparm)
{
	UINT id = (UINT)wparm;
	int i, jmpflag;
	int fldno = -1;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;
/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return 0;

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
		return 0;
/*
......save current focus field
*/
/*
.....should check for every drop down control list type also
*/
	if ((m_type[fldno]==3) || (m_type[fldno]==9))
		m_current_fld = fldno;

	if (m_type[fldno]==9)
/*
.....check if the edit is the new string, if yes, call the user define function
*/
	{
		UD_LIST *list_ans;
		char lstr[UX_MAX_PATH_LEN];
		CComboBox *cmbbx;
		cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(id));
		int indx = cmbbx->GetCurSel();
		list_ans = (UD_LIST *)(m_fData.ud_data[fldno].ud_delem.frmint) ;
		if (indx>=0)
			cmbbx->GetLBText(indx, lstr);
		else
			cmbbx->GetWindowText(lstr, UX_MAX_PATH_LEN);

		int len = strlen(lstr);
		if ((list_ans->answer!=NULL)&&(strcmp(lstr, list_ans->answer)==0))
			return 0;
		strcpy_s(list_ans->answer, len+1, lstr);

		UD_MARK(jmpflag,UU_TRUE);
		if (jmpflag != 0)
			goto done;
		if ((m_method_ret[fldno] & UD_TOGGLE &&
				m_method[fldno] != NULL)&&(m_init))
		{
			UD_DDATA seldata;
			seldata.frmstr = (char*)&lstr;
			int stat = (m_method[fldno])(&fldno, &seldata,UD_FLDOK);
			if (stat==UD_FRMCLOSE)
				goto done;
			if (fldno!=-1)
				uw_ntform_redisplay();		
		}
done:;
		UD_UNMARK(jmpflag);
	}
	return 0;
}
/**********************************************************************
**    I_FUNCTION :  chk_field(int fldno, int flag, int *redisp, UD_FSTAT fstat)
**       Check form field data format and optional call
**			user callback function
**    PARAMETERS
**       INPUT  :
**				fldno: field number			
**				flag: 1: call user defined function if check data no err
**						0: don't call user defined function	
**				fstat: state of the form 
**						UD_DONE is called when trying to close form
**						UD_TFWD: form open, just check the field data 
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int CNCLForm::chk_field(int fldno, int flag, int *redisp, UD_FSTAT fstat)
{
	char lstr[UX_MAX_PATH_LEN];
	char ldat[UX_MAX_PATH_LEN], msg[400];
	int i,stat, mflag,jmpflag;
	UD_LIST *list;
	UD_FSTAT stat2;

	int id = m_idtable[fldno+m_dspno];
	CWnd *cedt = (m_pScrollView->GetDlgItem(id));
	m_current_fld = -1;
	cedt->GetWindowText(lstr, UX_MAX_PATH_LEN-1);
	mflag = 0;

	int typ = m_fStruct.input_flds[fldno].ud_datatyp;
	for (i=strlen(lstr)-1;i>=0;i--) if (lstr[i] > ' ') break;
	lstr[i+1] = '\0';
	if ((typ>=UD_DASSCALAR) && (typ<=UD_SCAUNITLESS))
	{
/*
....allow empty input when not DONE
*/
		if ((lstr[0]=='\0')&&(fstat!=UD_DONE))
		{
/*
.....we need allow the user handle the event, so call use methed before return
*/
			if ((m_method_ret[fldno] & UD_TOGGLE &&
					m_method[fldno] != NULL)&&(m_init))
			{
/*
.....reset pick segment list before callback function
*/
				ncl_reset_pikgeom();
				if (m_type[fldno] ==3)
				{
					m_fData.ud_data[fldno].ud_delem.frmstr[0] = '\0';
					stat2 = (m_method[fldno])(&fldno, &(m_fData.ud_data[fldno].ud_delem), fstat);
				}
			}
			return 1;
		}
/*
....allow empty input when typ==UD_SCAVAL anytime
*/
		if ((lstr[0]=='\0')&&(typ==UD_SCAVAL))
		{
			return 1;
		}
	}
	stat = form_ckdata(fldno,lstr, &mflag);

	stat2 = UD_FLDOK;
	if (stat==UU_FALSE)
	{
		current_chk_field = fldno;
		if (lstr[0]=='\0')
		{
			sprintf_s(msg, sizeof(msg), "Need input data in this #%d form field", fldno);
			ud_wrerr(msg);
		}
		cedt->SetFocus();
		current_chk_field = -1;
		m_current_fld = fldno;
		return -1;
	}
/*
.....if it is a scalar value, leave it as it is
.....do not format to numbers
*/ 
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
	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag != 0)
		goto done;
	if ((m_method_ret[fldno] & UD_TOGGLE &&
			m_method[fldno] != NULL)&&(m_init))
	{
/*
.....reset pick segment list before callback function
*/
		ncl_reset_pikgeom();
		if (m_type[fldno] != 9)
			stat2 = (m_method[fldno])(&fldno, &(m_fData.ud_data[fldno].ud_delem), fstat);
		else
		{
			UD_DDATA seldata;
			char lstr[UX_MAX_PATH_LEN];
			list = (UD_LIST *)(m_fData.ud_data[fldno].ud_delem.frmint) ;
			if (list->answer!=NULL)
				strcpy_s(lstr, sizeof(lstr), list->answer);
			seldata.frmstr = (char*)&lstr;
			stat2 = (m_method[fldno])(&fldno, &seldata,fstat);
		}
		if (fldno!=-1)
/*
......don't call redisplay here, just set flag and call later
*/
			*redisp = 1;
		else
			*redisp = 0;
	}
done:;
	if ((stat2==UD_BADREQ)&&(m_init))
/*
.....if it is bad, then stay the focus in current field
*/
	{
		current_chk_field = fldno;
		cedt->SetFocus();
		current_chk_field = -1;
		UD_UNMARK(jmpflag);
		return -1;
	}	
//////////////
	if (*redisp)
	{
		ud_updatews(UG_SUPPRESS);
	}
	UD_UNMARK(jmpflag);
	return 1;
}
/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks5(WPARAM wparm, LPARAM lparm)
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
LRESULT CNCLForm::FormUserCallbacks5(WPARAM wparm, LPARAM lparm)
{
	UINT id = (UINT)wparm;
	int i, fldno, stat, flag;
	CWnd *old_def, *new_def;
	int redisp = 0;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;

/*
.....do not call callback function until form displayed
*/
	if (m_init==0)
		return 0;
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
		return 0;

	Focused_frmid = m_frmid;
	Focused_frm_fldno = fldno;
/*
......set current form too
*/
	uw_ntset_curform_id(m_frmid);
	if (m_type[fldno]==3)
	{
		m_actedit = (CEdit*)(m_pScrollView->GetDlgItem(id));
	}
	if (m_current_fld == fldno)
	{
		return 0;
	}
	if (m_current_fld!=-1)
	{
		if (current_chk_field==fldno)
			return 0;
		flag = 1;
		if (fldno != m_current_fld && UD_form_bypick == 1) flag = 0;
		stat = chk_field(m_current_fld, flag, &redisp, UD_TFWD);
		if (stat!=-1)
			m_current_fld = -1;
		else
			return 0; 
	}
/*
.....never set focus to display edit
*/
	if (m_type[fldno]==11)
	{
		CWnd *win = NULL; 
		if (fldno==m_fldno-1)
		{
			win = m_pScrollView->GetDlgItem(m_idtable[0]);
		}
		else if (m_forward)
			win = m_pScrollView->GetDlgItem(m_idtable[fldno+1+m_dspno]);
		else if (fldno-1>=0)
			win = m_pScrollView->GetDlgItem(m_idtable[fldno-1+m_dspno]);
		else
			win = m_pScrollView->GetDlgItem(m_idtable[m_fldno+m_dspno-1]);
		m_forward = 1;
		if (win==NULL)
		{
			return 0;
		}
		win->SetFocus();
	}		
	if (m_type[fldno]==3)
	{
		m_actedit = (CEdit*)(m_pScrollView->GetDlgItem(id));
/*
.....set highlight if needed
*/
		if (UW_frmtext_select)
			PostMessage(WM_COMMAND, ID_FORM_HIGHLIGHT);
	}
	if (redisp)
		uw_ntform_redisplay();
	m_current_fld = -1;
/*
......reset default button to OK
*/
	old_def = GetDlgItem(m_def_id);
	if (m_disptype==1)
	{
		if ((m_def_id!=IDC_FORMCANCEL)&&(old_def!=NULL))
			old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
		SendMessage(DM_SETDEFID, (WPARAM)IDC_FORMCANCEL);
		m_def_id = IDC_FORMCANCEL;
	}
	else
	{
		if ((m_def_id!=IDC_FORMACCEPT)&&(old_def!=NULL))
			old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
		SendMessage(DM_SETDEFID, (WPARAM)IDC_FORMACCEPT);
		m_def_id = IDC_FORMACCEPT;
	}
	new_def = GetDlgItem(m_def_id);
	if (new_def!=NULL)
		new_def->SendMessage(BM_SETSTYLE, BS_DEFPUSHBUTTON, TRUE);
	return 0;
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
void CNCLForm::set_list(int fieldno, UD_LIST *form_list)
{
	int i,j,cont, defsel;
	CListBox* listbx;
	CComboBox* cmbbx;
	int wid;
	int pwid;
	CClientDC *list_dc;
	CSize sizeText;

	UINT fid =  m_idtable[fieldno+m_dspno];
	switch(m_type[fieldno])
	{
	case 5:
		listbx = (CListBox*)(m_pScrollView->GetDlgItem(fid));
/*
.....we need clear listbox first
*/
		cont = listbx->GetCount();
		for (i = 0; i<cont; i++)
		{
			listbx->DeleteString(0);
		}
		defsel = -1;
		wid = 0;
		list_dc = new CClientDC(listbx);
		for (j=0; j<form_list->num_item; j++)
		{
			if (strcmp(form_list->item[j], form_list->answer)==0)
				defsel = j;

/*
.....using InsertString to clarify the position we need 
.....added, if use AddString, and the index 0 is a empty
.....item, as we default if no toolib loaded, then
.....when we load a *.tlb file, it will added item beginning
.....at index = 1 (it should remove empty item)
*/
//			listbx->AddString(form_list->item[j]);
			listbx->InsertString(j, form_list->item[j]);		
			sizeText = list_dc->GetTextExtent(form_list->item[j], strlen(form_list->item[j]));
			pwid = sizeText.cx;
			if (pwid>wid) wid = pwid;
		}
		if (form_list->num_item==0)
			listbx->AddString(" ");
		if (defsel!=-1)
		{
			listbx->SetCurSel(defsel);
			SendMessage(LBN_VIEWSELCHANGE, (WPARAM)fid);
		}
/*
......set the horizen text extent to longest text line
......for LISTBOX, we set /LEN/ -1 to active scrollbar,
......otherwise, there will be no horizontal scrolling bar
*/
		if (m_fStruct.input_flds[fieldno].ud_flen==-1)
			listbx->SetHorizontalExtent((UINT)(wid*0.95));
		else
			listbx->SetHorizontalExtent(0);
		delete list_dc;
		break;
	case 8:
	case 9:
	case 15:
		cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
/*
.....we need clear listbox first
*/
		cont = cmbbx->GetCount();
		for (i = 0; i<cont; i++)
		{
			cmbbx->DeleteString(0);
		}
		defsel = -1;
		wid = 0;
		list_dc = new CClientDC(cmbbx);
		for (j=0; j<form_list->num_item; j++)
		{
			if (strcmp(form_list->item[j], form_list->answer)==0)
				defsel = j;
			cmbbx->InsertString(j, form_list->item[j]);
			sizeText = list_dc->GetTextExtent(form_list->item[j], strlen(form_list->item[j]));
			pwid = sizeText.cx;
			if (pwid>wid) wid = pwid;
		}
		if (form_list->num_item==0)
			cmbbx->AddString(" ");
		if (defsel!=-1)
			cmbbx->SetCurSel(defsel);
		else
			cmbbx->SetCurSel(0);
		SendMessage(CBN_VIEWSELCHANGE, (WPARAM)fid);
/*
......set the horizen text extent to longest text line
*/
		cmbbx->SetHorizontalExtent((UINT)(wid*0.95));
		delete list_dc;
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
void CNCLForm::set_text(int fieldno, char *text)
{
	UINT fid =  m_idtable[fieldno+m_dspno];
	CWnd *cwin = (m_pScrollView->GetDlgItem(fid));
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
void CNCLForm::set_label(int fieldno, char* label)
{
	UINT fid =  m_idtable[fieldno];
	CWnd *cwin = (m_pScrollView->GetDlgItem(fid));
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
void CNCLForm::reset_fldlabel(int fieldno, char* label)
{
	if ((m_type[fieldno]==3)||(m_type[fieldno]==2)||(m_type[fieldno]==8)
		||(m_type[fieldno]==9)||(m_type[fieldno]==10) ||(m_type[fieldno]==11)
		||(m_type[fieldno]==13)||(m_type[fieldno]==14) || (m_type[fieldno] == 15)
		||(m_type[fieldno]==16))
	{
		if (m_input[fieldno] == FORM_STRING)
		{
			UINT fid =  m_idtable[fieldno+m_dspno] - 1;
			CWnd *cwin = (m_pScrollView->GetDlgItem(fid));
			cwin->SetWindowText(label);
		}
	}
	else if ((m_type[fieldno]==18)|| (m_type[fieldno] == 23))
	{
		UINT fid =  m_idtable[fieldno+m_dspno] - 1;
		CWnd *cwin = (m_pScrollView->GetDlgItem(fid));
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
void CNCLForm::set_choice(int fieldno, int choice)
{
	CComboBox* cmbbx;
	UINT fid =  m_idtable[fieldno+m_dspno];
	cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
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
void CNCLForm::set_display_mask(int fieldno, int val)
{
	UINT fid =  m_idtable[fieldno];
	CWnd *cwin = (m_pScrollView->GetDlgItem(fid));
	if (cwin==NULL)
		return;
	m_disp_mask[fieldno] = m_fStruct.ud_display_mask[fieldno];
/*
.....check if the display item is in the section displayed
*/
	if (((fieldno-m_dspno)>=0)&&(m_fStruct.input_flds[fieldno-m_dspno].section!=m_pScrollView->m_selsec)
		&&(m_fStruct.input_flds[fieldno-m_dspno].section!=-1))
		val = 0;
	else if ((fieldno<m_dspno)&&(m_fStruct.display_flds[fieldno].section!=m_pScrollView->m_selsec))
		val = 0;
	if (val==0)
	{
		cwin->ShowWindow(SW_HIDE);
		if (fieldno-m_dspno<0)
			return;
		if ((m_type[fieldno-m_dspno]==3)||(m_type[fieldno-m_dspno]==2)
			||(m_type[fieldno-m_dspno]==8)||(m_type[fieldno-m_dspno]==9)
			||(m_type[fieldno-m_dspno]==10) ||(m_type[fieldno-m_dspno]==13)
			||(m_type[fieldno-m_dspno]==11) || (m_type[fieldno-m_dspno] == 16)
			||(m_type[fieldno-m_dspno]==14) || (m_type[fieldno-m_dspno] == 15))
		{
/*
......hide prompt label
*/
			cwin = (m_pScrollView->GetDlgItem(fid-1));
			cwin->ShowWindow(SW_HIDE);
		}
		if (m_type[fieldno-m_dspno]==13)
		{
			fid =  m_idtable[fieldno+m_fldno+m_framno];
			(m_pScrollView->GetDlgItem(fid))->ShowWindow(SW_HIDE);
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
			||(m_type[fieldno-m_dspno]==11) || (m_type[fieldno-m_dspno] == 16)
			||(m_type[fieldno-m_dspno]==14) || (m_type[fieldno-m_dspno] == 15))
		{
/*
......hide prompt label
*/
			cwin = (m_pScrollView->GetDlgItem(fid-1));
			cwin->ShowWindow(SW_SHOW);
		}
		if (m_type[fieldno-m_dspno]==13)
		{
			fid =  m_idtable[fieldno+m_fldno+m_framno];
			(m_pScrollView->GetDlgItem(fid))->ShowWindow(SW_SHOW);
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
void CNCLForm::set_traverse_mask(int fieldno, int val)
{
	UINT fid =  m_idtable[fieldno+m_dspno];
	CWnd *cwin = (m_pScrollView->GetDlgItem(fid));
	if (val==0)
	{
		cwin->EnableWindow(FALSE);
		if ((m_type[fieldno]==3)||(m_type[fieldno]==2)
			||(m_type[fieldno]==8)||(m_type[fieldno]==9)|| (m_type[fieldno] == 16)
			||(m_type[fieldno]==10) ||(m_type[fieldno]==13)||(m_type[fieldno]==11)
			||(m_type[fieldno]==14) || (m_type[fieldno] == 15)|| (m_type[fieldno] == 18)
			|| (m_type[fieldno] == 23))
		{
/*
......disable prompt label
*/
			cwin = (m_pScrollView->GetDlgItem(fid-1));
			cwin->EnableWindow(FALSE);
		}
		if (m_type[fieldno]==13)
		{
			fid =  m_idtable[fieldno+m_dspno+m_fldno+m_framno+m_picno];
			(m_pScrollView->GetDlgItem(fid))->EnableWindow(FALSE);
		}
	}
	else
	{
		cwin->EnableWindow(TRUE);
		if ((m_type[fieldno]==3)||(m_type[fieldno]==2)||(m_type[fieldno]==11)
			||(m_type[fieldno]==8)||(m_type[fieldno]==9)|| (m_type[fieldno] == 16)
			||(m_type[fieldno]==10) ||(m_type[fieldno]==13)
			||(m_type[fieldno]==14) || (m_type[fieldno] == 15)||(m_type[fieldno]==18)
			||(m_type[fieldno] == 23))
		{
/*
......enable prompt label
*/
			cwin = (m_pScrollView->GetDlgItem(fid-1));
			cwin->EnableWindow(TRUE);
		}
		if (m_type[fieldno]==13)
		{
			fid =  m_idtable[fieldno+m_dspno+m_fldno+m_framno+m_picno];
			(m_pScrollView->GetDlgItem(fid))->EnableWindow(TRUE);
		}
	}
}

/**********************************************************************
**    I_FUNCTION :  form_ckdata(int fno, char *buf, int *flag)
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
int CNCLForm::form_ckdata(int fno, char *buf, int *flag)
{
	UD_DASDATA dsda;
	UD_DASIN de;
	UD_LIST *list_ans;
	int irtn,typ,stat,nc,i,ifl,len;
	char text[UX_MAX_PATH_LEN], erms[80];
/*
.....Initialize routine
*/
	*flag = 0;
	irtn = UU_TRUE;
	typ = m_fStruct.input_flds[fno].ud_datatyp;
/*
.....Parse if not string input
*/
	stat = 0;
	for (i=strlen(buf)-1;i>=0;i--) if (buf[i] > ' ') break;
	buf[i+1] = '\0';
	len = strlen(buf);
	if ((typ>=UD_DASSCALAR) && (typ<=UD_SCAUNITLESS))
	{
/*
.....Check if the string is the scalar value/string
.....even though we may keep and return string, but
.....we still need get the value in order to check the range
*/
		if (buf[0]=='\0')
		{
/*
.....why this? It should not allow empty string if it suppose have value input
.....or if allow, allow for all UDSCAxxx value
*/
/*
			if (typ==UD_SCAVAL)
				return UU_TRUE;
			else
				return UU_FALSE;
*/
			return UU_TRUE;
/*
.....or change to follow to display message
*/
/*
			uw_ntdispmsg("This field requires a valid scalar value!");
			return UU_FALSE;
*/
		}
		strcpy_s(text, sizeof(text), buf);
/*
.....Get the scalar and convert to value
*/
		stat = ncl_parse_scalar_values(text, text, typ);
		if (stat==-1)
		{
			sprintf_s(erms, sizeof(erms), "%s is not a valid scalar value",buf);
			uw_ntdispmsg(erms);
			return UU_FALSE;
		}
	}

	if (typ == UD_DASVAL || typ == UD_DASUNITLESS ||
		typ == UD_DASDISTANCE || typ == UD_DASANGLE ||
			typ == UD_DASINT || typ == UD_DASVEC || 
			typ == UD_DASCART )
	{
		strcpy_s(text, sizeof(text), buf);
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
			|| typ == UD_DASSCALAR || typ == UD_SCAINT || typ == UD_SCANDC ||
			typ == UD_SCAUNITLESS)
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
				strcpy_s(m_fData.ud_data[fno].ud_delem.frmstr, len+1, buf);		
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
......remove following code. The value already adjust in ud_dasin
*/
/*
......adjust the value
*/
/*		UM_cc_exttoint(m_fData.ud_data[fno].ud_delem.frmvec, m_fData.ud_data[fno].ud_delem.frmvec); */
		break;
	case UD_DASNDC:
		if (dsda.dtype != 2) goto failed;
		m_fData.ud_data[fno].ud_delem.frmvec[0] = dsda.stval.stcord.coord[0];
		m_fData.ud_data[fno].ud_delem.frmvec[1] = dsda.stval.stcord.coord[1];
		m_fData.ud_data[fno].ud_delem.frmvec[2] = dsda.stval.stcord.coord[2];
		break;
	case UD_DASSCALAR:
		strcpy_s(m_fData.ud_data[fno].ud_delem.frmstr,len+1, buf);
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
......remove following code. The value already adjust in ud_dasin
*/
/*
......adjust the value
*/
/*		UM_len_exttoint(m_fData.ud_data[fno].ud_delem.frmflt[0],
//			m_fData.ud_data[fno].ud_delem.frmflt[0]);  */
		break;
	case UD_DASANGLE:
		if (dsda.dtype != 1) goto failed;
		m_fData.ud_data[fno].ud_delem.frmflt[0] = dsda.stval.dval;
/*
......remove following code. The value already adjust in ud_dasin
*/
/*
......adjust the value
*/
/*		UM_ang_inttoext(m_fData.ud_data[fno].ud_delem.frmflt[0],
//			m_fData.ud_data[fno].ud_delem.frmflt[0]);  */
		break;
	case UD_DASINT:
		if (dsda.dtype != 1) goto failed;
		m_fData.ud_data[fno].ud_delem.frmint[0] = (int)dsda.stval.dval;
		break;
	case UD_DASSTRING:
/*
.....added List here
.....Yurong 8/15/97
*/
		if (m_fStruct.input_flds[fno].toggle==5)
		{
			list_ans = (UD_LIST *)(m_fData.ud_data[fno].ud_delem.frmint) ;
			if (list_ans->answer!=NULL)
				strcpy_s(list_ans->answer,len+1,  buf);
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
void CNCLForm::reset_picture(int fieldno, int *val)
{
	(m_fData.ud_data[fieldno]).ud_delem.frmint = val;
	OnPaint();
}

/**********************************************************************
**    I_FUNCTION :  Get_field_data(int fieldno, UD_DDATA data, UU_LOGICAL str_flag)
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
void CNCLForm::Get_field_data(int fieldno, UD_DDATA data, UU_LOGICAL str_flag)
{
	int indx,len;
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
		case 9:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
			cmbbx->GetWindowText(string, UX_MAX_PATH_LEN-1);
			len = strlen(string);
			strcpy_s(data.frmstr, len+1, string);
			break;
//		case 9:
//			break;
/*
.....COMBBOX, CHOICES, can't be edited.
*/
		case 2:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
			data.frmint[0] = cmbbx->GetCurSel();
			if ((str_flag) || ((m_init_data_type_flag)&&(m_data_type[fieldno]==UD_DASSTRING)))
			{
				cmbbx->GetLBText(data.frmint[0], string) ;
				len = strlen(string);
				strcpy_s(data.frmstr, len+1, string);
			}
			break;
/*
......LIST BOX
*/
		case 5:
			listbx = (CListBox*)(m_pScrollView->GetDlgItem(fid));
			indx = listbx->GetCurSel(); 
			if (indx==LB_ERR)
				string[0] = '\0';
			else
				listbx->GetText(indx, string) ;
			len = strlen(string);
			strcpy_s(data.frmstr, len+1, string);
			break;
/*
.....EDIT
*/
		case 3:
		case 11:
		case 16:
			cedt = (CEdit*)(m_pScrollView->GetDlgItem(fid));
			cedt->GetWindowText(string, UX_MAX_PATH_LEN-1);
			len = strlen(string);
			strcpy_s(data.frmstr, len+1, string);
			break;
/*
.....CHECKBOX
*/
		case 7:
			cbut = (CButton*)(m_pScrollView->GetDlgItem(fid));
			data.frmint[0] = cbut->GetCheck();
			if (str_flag)
				sprintf_s(data.frmstr,8,"%d",data.frmint[0]);
			break;
/*
.....CHOICE_LIST.
*/
		case 15:
			cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
			indx = cmbbx->GetCurSel(); 
			cmbbx->GetLBText(indx, string) ;
			len = strlen(string);
			strcpy_s(data.frmstr, len+1, string);
			break;
		case 18:
			data.frmint[0] = *((m_fData.ud_data[fieldno]).ud_delem.frmint);
			break;
		case 23:
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
int CNCLForm::form_accept_close()
{
	FormAccept();
	return 1;
}
/***********************************************************************
c
c   SUBROUTINE:  OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
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
HBRUSH CNCLForm::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	int i;
	UINT fid;
	CWnd *wnd;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;

	switch (nCtlColor)
	{
		case CTLCOLOR_BTN:
			return CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
//			break;
/*
......Readonly Edit control is treated 
......as static label. Set this Edit control's 
......background and text color
*/
		case CTLCOLOR_STATIC:
			for (i=0; i<m_fldno; i++)
			{
				if (m_type[i]==11)
				{
					fid =  m_idtable[i+m_dspno];
					wnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
					if (wnd->m_hWnd==pWnd->m_hWnd)
					{
						pDC->SetTextColor(RGB(20, 20, 20));
						pDC->SetBkColor(RGB(255, 255, 255));
						return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
					}
				}
			}
			return CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
		default:
			return CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
	}
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
BOOL CNCLForm::PreTranslateMessage(MSG* pMsg) 
{
	int stat = 0;
	HWND hWnd = (HWND)*this; 
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return 0;
	if (hWnd==NULL) return 0;
/*
.....for multi-lines editbox, we want the window execute the default function
.....(get into the next line) instead of user define function
*/
	if (Focused_frmid==m_frmid)
	{
		if ((Focused_frm_fldno>=0) && (m_type[Focused_frm_fldno]==16))
		{
			return CDialog::PreTranslateMessage( pMsg );
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
	if (TranslateAccelerator(m_pScrollView->m_hWnd, m_accel, pMsg))
	{
/*
.....from accelarator key
.....don't execute the command if the field is on a edit field
*/
/*
.....why "don't execute the command if the field is on a edit field"
.....changed back because we want shortcut key of the form to be execute
.....in any field (changed when doing calculator)
.....Yurong 6/27/06
.....................
		if (Focused_frmid==m_frmid)
		{
			if ((Focused_frm_fldno>=0) && (m_type[Focused_frm_fldno]==3))
				return CDialog::PreTranslateMessage( pMsg );
		}
*/
		m_return_key = 0;
		return TRUE;
	}
	else
	{
		return CDialog::PreTranslateMessage( pMsg );
		m_return_key = 0;
	}
}

/***********************************************************************
c
c   FUNCTION: update_current_input(char *text)
c
c       Update the active text field with the new input text
c
c   INPUT:  text: input to be update
c
c   OUTPUT :   None
c   RETURN:    0: update the text
c
**********************************************************************/
int CNCLForm::update_current_input(char *text)
{
	UINT id;
	if (m_current_fld==-1)
		return -1;
	int typ = m_fStruct.input_flds[m_current_fld].ud_datatyp;

	if (m_type[m_current_fld]==3)
	{
		id = m_idtable[m_current_fld+m_dspno];
		m_actedit = (CEdit*)(m_pScrollView->GetDlgItem(id));
	}
	if (m_actedit==NULL)
		return -1;
/*
.....check the field type, if it is not single value, then
.....we append the text, if it is single data/value type, replace it
*/
	if ( (typ==UD_DASVAL) || (typ==UD_DASDISTANCE) 
		|| (typ==UD_DASUNITLESS) || (typ==UD_DASANGLE) || (typ==UD_DASINT)
		|| ((typ>=UD_DASSCALAR) && (typ<=UD_SCAUNITLESS)) )
		m_actedit->SetSel(0, -1);
	m_actedit->ReplaceSel(text);
	return 0;
}
/***********************************************************************
c
c   FUNCTION: uw_dispfrm_wait(int frmid)
c
c       Wait a display form to be finished, this will not disactive other form/window
c		event unless other form/window is disactive before this form display.
c
c   INPUT:  frmid: form id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" void uw_dispfrm_wait(int frmid)
{
	if (frmid<0) return;
	MSG msg;
	if (UW_display_frmwin[frmid]==NULL)
		return;

	while (UW_display_frmwin[frmid]!=NULL)
	{
		if (GetMessage( &msg, NULL, 0, 0 ) != -1) 
		{
			if (msg.message != WM_KICKIDLE && 
				!(UW_display_frmwin[frmid]->PreTranslateMessage(&msg)))			
			{
				TranslateMessage(&msg);
				DispatchMessage(&msg);
			}
		}
	}
}
/***********************************************************************
c
c   FUNCTION: uw_ntset_frmpocket(int frmid, int flag)
c
c       Set the form pocket flag
c
c   INPUT:  frmid: form id
c			flag: pocket flag
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" void uw_ntset_frmpocket(int frmid, int flag)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]==NULL)
		return;
	((CNCLForm*)UW_display_frmwin[frmid])->set_pocket(flag);
}

/***********************************************************************
c
c   FUNCTION: uw_formupd_input(char *text)
c
c      Update the active text field with the new input text
c		insert the text at the insert cursor position
c
c   INPUT:  text: input to be update
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" int uw_formupd_input(char *text)
{
//	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
//	CWnd *wnd = MainWin->GetActiveWindow();
/*
......if there are saved active id
*/
	int uw_ntget_savform_id();
	int fid = uw_ntget_savform_id();
	if (fid<0)
		return -1;
	if (UW_display_frm[fid]==NULL)
		return -1;
	
	if (UW_display_frmwin[fid]->IsKindOf(RUNTIME_CLASS(CNCLForm)))
		return (UW_display_frm[fid])->update_current_input(text);
		
//	if (UW_display_frm[fid]->IsKindOf(RUNTIME_CLASS(CNCLForm)))
//	{
//		return ((CNCLForm*)UW_display_frm[fid])->update_current_input(text);
//	}
	return -1;
}
/***********************************************************************
c
c   SUBROUTINE:  InitAdjust()
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
void CNCLForm::InitAdjust()
{
	int i,fldno, font_size,color,ret;
	CEdit* cedt;
	CWnd *cwin;
	UINT fid;
	COLORREF bcolor, fcolor, pbcolor, pfcolor;

	fldno = m_frmid;

	for (i=0; i<m_dspno; i++)
	{
		fid =  m_idtable[i];
//		font_size = (int)(80 * m_fStruct.display_flds[i].font_scale);
		font_size = (int)(UW_form_fontsize*10 * m_fStruct.display_flds[i].font_scale);
		m_dfieldFont[i].CreatePointFont(font_size, UW_formdlg_font);
		cwin = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		cwin->SetFont(&m_dfieldFont[i]);
/*
.....init background brush
*/
		if (stricmp (m_fStruct.display_flds[i].bcolor, "DEFAULT")!=0)
		{
			ret = uw_get_rgb (m_fStruct.display_flds[i].bcolor, bcolor); 
			if (ret==-1)
				m_dBkBrush[i] = NULL;
			else
				m_dBkBrush[i] = new CBrush(bcolor);
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
//		font_size = (int)(80 * m_fStruct.input_flds[i].font_scale);
		font_size = (int)(UW_form_fontsize*10 * m_fStruct.input_flds[i].font_scale);
		m_fieldFont[i].CreatePointFont(font_size, UW_formdlg_font);

/*
.....if it is color button for #SELECT#
.....the background should same as foreground if fcolor is not default
*/
		if (m_type[i]==23)
		{
			if (stricmp (m_fStruct.input_flds[i].fcolor, "DEFAULT")!=0)
				strcpy(m_fStruct.input_flds[i].bcolor, m_fStruct.input_flds[i].fcolor);
		}
		if (stricmp (m_fStruct.input_flds[i].bcolor, "DEFAULT")!=0)
		{
			ret = uw_get_rgb (m_fStruct.input_flds[i].bcolor, bcolor); 
			if (ret==-1)
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE); 
				m_BkBrush[i] = NULL;
			}
			else
				m_BkBrush[i] = new CBrush(bcolor);
		}
		else
		{
			if (m_type[i]==23)
			{
				bcolor = RGB (uw_color_table[uw_glhicolor][0],
						uw_color_table[uw_glhicolor][1],
						uw_color_table[uw_glhicolor][2]);
			}
			else
				bcolor = ::GetSysColor(COLOR_BTNFACE); 
			m_BkBrush[i] = NULL;
		}
		if (stricmp (m_fStruct.input_flds[i].fcolor, "DEFAULT")!=0)
		{
			ret = uw_get_rgb (m_fStruct.input_flds[i].fcolor, fcolor); 
			if (ret==-1)
				fcolor = ::GetSysColor(COLOR_BTNTEXT);
		}
		else
		{
			if (m_type[i]==23)
			{
				fcolor = RGB (uw_color_table[uw_glhicolor][0],
						uw_color_table[uw_glhicolor][1],
						uw_color_table[uw_glhicolor][2]);
			}
			else
				fcolor = ::GetSysColor(COLOR_BTNTEXT);
		}
		if (stricmp (m_fStruct.input_flds[i].pbcolor, "DEFAULT")!=0)
		{
			ret = uw_get_rgb (m_fStruct.input_flds[i].pbcolor, bcolor); 
			if (ret==-1)
				m_pBkBrush[i] = NULL;
			else
				m_pBkBrush[i] = new CBrush(bcolor);
		}
		else
			m_pBkBrush[i] = NULL;
		switch(m_type[i])
		{
/*
.....EDIT
*/
		case 3:
		case 11:
/*
.....adjust prompt's height as same as the input field
*/
			adjust_height(i);
			if ((m_input[i]==FORM_PICK) || (m_input[i]==FORM_LOCATE) ||
				(m_input[i]==FORM_LABEL) || (m_input[i]==FORM_SUBSCR))
			{
				init_button3(i, fid-1, bcolor, fcolor);
			}
		case 16:
			cedt = (CEdit*)(m_pScrollView->GetDlgItem(fid));
			cedt->SetFont(&m_fieldFont[i]);
			break;
		case 2:
		case 9:
		case 13:
/*
.....adjust prompt's height as same as the input field
*/
			adjust_height(i);
		case 8:
		case 10:
			cwin = (CWnd*)(m_pScrollView->GetDlgItem(fid));
			cwin->SetFont(&m_fieldFont[i]);
			if (m_type[i]==13)
			{
				fid =  m_idtable[i+m_dspno+m_fldno+m_framno+m_picno];
				cwin = (CWnd*)(m_pScrollView->GetDlgItem(fid));
				cwin->SetFont(&m_fieldFont[i]);
			}
			break;
/*
......all others
*/
		case 1:
			init_button3(i, fid, bcolor, fcolor);
			cwin = (CWnd*)(m_pScrollView->GetDlgItem(fid));
			cwin->SetFont(&m_fieldFont[i]);
			break;
		case 7:
			if (tdata!=NULL)
				init_button3(i, fid, bcolor, fcolor);
			cwin = (CWnd*)(m_pScrollView->GetDlgItem(fid));
			cwin->SetFont(&m_fieldFont[i]);
			break;
		case 17:
			init_listctl(i, fid);
			cwin = (CWnd*)(m_pScrollView->GetDlgItem(fid));
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
		case 19:
			init_listctl2(i, fid);
			cwin = (CWnd*)(m_pScrollView->GetDlgItem(fid));
			cwin->SetFont(&m_fieldFont[i]);
			break;
		case 23:
			if (stricmp (m_fStruct.input_flds[i].pbcolor, "DEFAULT")!=0)
			{
				ret = uw_get_rgb (m_fStruct.input_flds[i].pbcolor, pbcolor); 
				if (ret==-1)
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
				ret = uw_get_rgb (m_fStruct.input_flds[i].pfcolor, pfcolor); 
				if (ret==-1)
				{
					pfcolor = ::GetSysColor(COLOR_BTNTEXT); 
				}
			}
			else
			{
				pfcolor = ::GetSysColor(COLOR_BTNTEXT); 
			}
			init_button2(i, fid-1, pbcolor, pfcolor, fid, bcolor, fcolor);
			break;
		case 24:
			init_slider(i, fid);
			break;
		default:
			cwin = (CWnd*)(m_pScrollView->GetDlgItem(fid));
			cwin->SetFont(&m_fieldFont[i]);
			break;
		}
	}
	for (i=0; i<m_framno; i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno];  
//		font_size = (int)(80 * m_fStruct.frame_flds[i].font_scale);
		font_size = (int)(UW_form_fontsize*10 * m_fStruct.frame_flds[i].font_scale);
		m_ffieldFont[i].CreatePointFont(font_size, UW_formdlg_font);
		cwin = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		cwin->SetFont(&m_ffieldFont[i]);
		if (stricmp (m_fStruct.frame_flds[i].bcolor, "DEFAULT")!=0)
		{
			ret = uw_get_rgb (m_fStruct.frame_flds[i].bcolor, bcolor); 
			if (ret==-1)
				m_fBkBrush[i] = NULL;
			else
				m_fBkBrush[i] = new CBrush(bcolor);
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
/***********************************************************************
c
c   SUBROUTINE:  Onctlcolor2(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
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
HBRUSH CNCLForm::Onctlcolor2(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	int i, ret,status;
	UINT fid;
	CWnd *wnd, *pwnd;
	COLORREF fcolor, bcolor;

	if (UD_dispfrm[m_frmid]==NULL) return NULL;

	if (m_pScrollView==NULL)
		return (HBRUSH)NULL;
	for (i=0; i<m_dspno; i++)
	{
		fid = m_idtable[i];  
		wnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		ret = 0;
		if (wnd->m_hWnd==pWnd->m_hWnd)
		{
			if (stricmp (m_fStruct.display_flds[i].fcolor, "DEFAULT")!=0)
			{
	 			status = uw_get_rgb (m_fStruct.display_flds[i].fcolor, fcolor); 
				if (status!=-1)
				{
					pDC->SetTextColor(fcolor);
					ret = 1;
				}
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
		}
		if (ret==1)
		{
			bcolor = ::GetSysColor(COLOR_BTNFACE); 
			if (m_dBkBrush[i]==NULL)
				m_dBkBrush[i] = new CBrush(bcolor);
			pDC->SetBkColor(bcolor);
			return (HBRUSH)(m_dBkBrush[i]->GetSafeHandle());
		}
		else
//			return (HBRUSH)NULL;
			continue;
	}
	for (i=0; i<m_fldno; i++)
	{
		fid =  m_idtable[i+m_dspno];
		if ((m_type[i]==2)||(m_type[i]==3)||(m_type[i]==11)
			||(m_type[i]==8)||(m_type[i]==9)|| (m_type[i] == 16)
			||(m_type[i]==10)||(m_type[i]==13)||(m_type[i]==18)
			||(m_type[i]==14) || (m_type[i] == 15))
		{
			pwnd = (CWnd*)(m_pScrollView->GetDlgItem(fid-1));
		}
		else if (m_type[i]==7)
		{
			fid =  m_idtable[i+m_dspno];
			pwnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		}
		else
			pwnd = NULL;
		wnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		ret = 0;
		if ((wnd!=NULL)&&(wnd->m_hWnd==pWnd->m_hWnd))
		{
/*
......input field
*/
			if (stricmp (m_fStruct.input_flds[i].fcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.input_flds[i].fcolor, fcolor); 
				if (status!=-1)
				{
					pDC->SetTextColor(fcolor);
					ret = 1;
				}
			}
			if (stricmp (m_fStruct.input_flds[i].bcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.input_flds[i].bcolor, bcolor);
				if (status!=-1)
				{
					if (m_BkBrush[i] != NULL)
						delete m_BkBrush[i];
					m_BkBrush[i] = new CBrush(bcolor);
					pDC->SetBkColor(bcolor);
					return (HBRUSH)(m_BkBrush[i]->GetSafeHandle());
				}
			} 
			if (ret==1)
			{
				if (nCtlColor==CTLCOLOR_EDIT)
					bcolor = ::GetSysColor(COLOR_WINDOW);
				else
					bcolor = ::GetSysColor(COLOR_BTNFACE);
				if (m_BkBrush[i] == NULL)
				{
					m_BkBrush[i] = new CBrush(bcolor);
				}
				pDC->SetBkColor(bcolor);
				return (HBRUSH)(m_BkBrush[i]->GetSafeHandle());
			}
			else
//				return (HBRUSH)NULL;
				continue;
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
				{
					pDC->SetTextColor(fcolor);
					ret = 1;
				}
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
			if (ret==1)
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE);
				if (m_pBkBrush[i]==NULL)
					m_pBkBrush[i] = new CBrush(bcolor);
				pDC->SetBkColor(bcolor);
				return (HBRUSH)(m_pBkBrush[i]->GetSafeHandle());
			}
			else
//				return (HBRUSH)NULL;
				continue;
		}
	}
	for (i=0; i<m_framno; i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno];  
		wnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		ret = 0;
		if ((wnd!=NULL)&&(wnd->m_hWnd==pWnd->m_hWnd))
		{
			if (stricmp (m_fStruct.frame_flds[i].fcolor, "DEFAULT")!=0)
			{
				status = uw_get_rgb (m_fStruct.frame_flds[i].fcolor, fcolor); 
				if (status!=-1)
				{
					pDC->SetTextColor(fcolor);
					ret = 1;
				}
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
			if (ret==1)
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE);
				if (m_fBkBrush[i]!=NULL)
					m_fBkBrush[i] = new CBrush(bcolor);
				pDC->SetBkColor(bcolor);
				return (HBRUSH)(m_fBkBrush[i]->GetSafeHandle());
			}
			else
//				return (HBRUSH)NULL;
				continue;
		}
	}
	return (HBRUSH)NULL;
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
void CNCLForm::set_butlabel(int fieldno, char* label)
{
	UINT fid =  m_idtable[fieldno+m_dspno];
	if (m_type[fieldno]!=1)
		return;

	CWnd *cwin = (m_pScrollView->GetDlgItem(fid));
	cwin->SetWindowText(label);
}
extern "C" void uw_ntget_focusfrm(int *frmid, int *fldno)
{
	*frmid = Focused_frmid;
	*fldno = Focused_frm_fldno;
}

void CNCLForm::init_slider(int i, UINT fid)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPSLIDER1 + i;
	if (m_type[i]!=24)
		return;
	CWnd *cwin = (m_pScrollView->GetDlgItem(tempid));
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
	m_pScrollView->Create_Slider(i, fid, dwStyle,rbtn, range1, range2, value, budid,
		m_fStruct.input_flds[i].justified);
}

void CNCLForm::HandleVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	int i, fldno;
	UINT fid;
	CWnd *wnd;
	if (pScrollBar==NULL)
		return;
	if ((pScrollBar->IsKindOf(RUNTIME_CLASS(CNCLSliderCtrl)))==0)
		return;

	fldno = -1;
	for (i=0; i<m_fldno; i++)
	{
		if (m_type[i]!=24)
			continue;
		fid = m_idtable[i+m_dspno];
		wnd = m_pScrollView->GetDlgItem(fid);
		if (pScrollBar->m_hWnd == wnd->m_hWnd)
		{
			fldno = i;
			break;
		}
	}
	if (fldno == -1)
		return;

	Focused_frmid = m_frmid;
	Focused_frm_fldno = fldno;
	uw_ntset_curform_id(m_frmid);
	
	CNCLSliderCtrl* pSlider = (CNCLSliderCtrl*)pScrollBar;		
	int pos = pSlider->GetPos();
	if (*((m_fData.ud_data[fldno]).ud_delem.frmint)==pos)
		return;
	*((m_fData.ud_data[fldno]).ud_delem.frmint) = pSlider->GetPos();
/*
......set current form too
*/
	Execute_command (fid);
}

void CNCLForm::HandleHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	int i, fldno;
	UINT fid;
	CWnd *wnd;
	if (pScrollBar==NULL)
		return;
	if ((pScrollBar->IsKindOf(RUNTIME_CLASS(CNCLSliderCtrl)))==0)
		return;

	fldno = -1;
	for (i=0; i<m_fldno; i++)
	{
		if (m_type[i]!=24)
			continue;
		fid = m_idtable[i+m_dspno];
		wnd = m_pScrollView->GetDlgItem(fid);
		if (pScrollBar->m_hWnd == wnd->m_hWnd)
		{
			fldno = i;
			break;
		}
	}
	if (fldno == -1)
		return;

	Focused_frmid = m_frmid;
	Focused_frm_fldno = fldno;
/*
......set current form too
*/
	uw_ntset_curform_id(m_frmid);

	CNCLSliderCtrl* pSlider = (CNCLSliderCtrl*)pScrollBar;		
	int pos = pSlider->GetPos();
	if (*((m_fData.ud_data[fldno]).ud_delem.frmint)==pos)
		return;
	*((m_fData.ud_data[fldno]).ud_delem.frmint) = pSlider->GetPos();
	Execute_command (fid);
}

void CNCLForm::init_button(int i, UINT fid, COLORREF bcolor, COLORREF fcolor)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPBUT1 + i;
//	if ((m_type[i]!=1) && (m_type[i]!=3)&&(m_type[i]!=11)&&(m_type[i]!=18)) 
	if (m_type[i]!=18)
		return;

	CWnd *cwin = (m_pScrollView->GetDlgItem(tempid));
	if (cwin == NULL)
		return;
	CRect rbtn;
	CFont* aFont = cwin->GetFont();
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);

//	if (m_type[i]==18)
		m_pScrollView->Recreate_button(i, "", 
			WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY,
			rbtn, fid, bcolor, fcolor);	
//	else
//		m_pScrollView->Recreate_button(i, m_fldlabel[i], 
//			WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY,
//			rbtn, fid, bcolor, fcolor);	
/*
	if ((m_type[i]==3)||(m_type[i]==11))
	{
		cwin = (m_pScrollView->GetDlgItem(fid));
		cwin->SetFont(aFont);
	}
*/
}

void CNCLForm::init_button2(int i, UINT pfid, COLORREF pbcolor, COLORREF pfcolor, 
			UINT fid, COLORREF bcolor, COLORREF fcolor)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPBUT1 + i;
	if (m_type[i]!=23)
		return;

	CWnd *cwin = (m_pScrollView->GetDlgItem(tempid));
	if (cwin == NULL)
		return;
	CRect rbtn;
	CFont* aFont = cwin->GetFont();
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);

	m_pScrollView->Recreate_button(i, m_fldlabel[i], 
			WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY,
			rbtn, pfid, pbcolor, pfcolor);	
	cwin = (m_pScrollView->GetDlgItem(pfid));
	cwin->SetFont(aFont);

	cwin = (m_pScrollView->GetDlgItem(fid));
	if (cwin == NULL)
		return;
	aFont = cwin->GetFont();
	cwin->GetWindowRect(&rbtn);
	cwin->DestroyWindow();
	m_pScrollView->Recreate_button2(i, "", 
			WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY,
			rbtn, fid, bcolor, fcolor);	
	cwin = (m_pScrollView->GetDlgItem(fid));
	cwin->SetFont(aFont);
}

void CNCLForm::init_button3(int i, UINT fid, COLORREF bcolor, COLORREF fcolor)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPBUT1 + i;
	if ((m_type[i]!=1) && (m_type[i]!=3)&&(m_type[i]!=11)&&(m_type[i]!=7)) 
		return;

	CWnd *cwin = (m_pScrollView->GetDlgItem(tempid));
	if (cwin == NULL)
		return;
	CRect rbtn;
	CFont* aFont = cwin->GetFont();
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	if ((m_type[i]==1)||(m_type[i]==3)||(m_type[i]==11))
		m_pScrollView->Recreate_button3(i, m_fldlabel[i], 
			WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_NOTIFY,
			rbtn, fid, bcolor, fcolor, m_type[i]);	
	else
		m_pScrollView->Recreate_button3(i, m_fldlabel[i], 
			WS_VISIBLE | WS_CHILD | BS_AUTOCHECKBOX | BS_CENTER | WS_TABSTOP | BS_NOTIFY,
			rbtn, fid, bcolor, fcolor, m_type[i]);	
	if ((m_type[i]==3)||(m_type[i]==11))
	{
		cwin = (m_pScrollView->GetDlgItem(fid));
		cwin->SetFont(aFont);
	}
}

int CNCLForm::get_idtype(UINT id)
{
	int i;
	for (i=0; i<m_fldno; i++)
	{
		UINT fid =  m_idtable[i+m_dspno];
		if (id==fid)
		{
			return m_type[i];
		}
	}
	return -1;
}
void CNCLForm::init_listctl(int i, UINT fid)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPLIST1 + i;
/*
......if not table list, just return
*/
	if (m_type[i]!=17)
		return;

	CWnd *cwin = (m_pScrollView->GetDlgItem(tempid));
	if (cwin == NULL)
		return;
	CRect rbtn;
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	m_pScrollView->Recreate_listctl(i, NULL, 
		WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL | WS_HSCROLL | WS_VSCROLL,
		rbtn, fid);	
}

void CNCLForm::init_listctl2(int i, UINT fid)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDD_FORM_TEMPLIST1 + i;
/*
......if not table list, just return
*/
	if (m_type[i]!=19)
		return;

	CWnd *cwin = (m_pScrollView->GetDlgItem(tempid));
	if (cwin == NULL)
		return;
	CRect rbtn;
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	m_pScrollView->Recreate_listctl2(i, NULL, 
		WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL | WS_HSCROLL | WS_VSCROLL,
		rbtn, fid);	
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
void CNCLForm::set_picarea(int picno, int n_picarea, UD_PICAREA *picarea, UINT pID)
{
	m_pScrollView->set_picarea(picno, n_picarea, picarea, pID); 
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
void CNCLForm::init_picarea(int i, UINT fid)
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	UINT tempid = IDC_FORMTEMPPIC + i;
		
	char filename[UX_MAX_PATH_LEN], name[UX_MAX_PATH_LEN];
	strcpy(filename, m_fStruct.picture_flds[i].fname);
	strcpy(name, m_fStruct.picture_flds[i].name);
	CWnd *cwin = (m_pScrollView->GetDlgItem(tempid));
	if (cwin == NULL)
		return;
	CRect rbtn;
	cwin->GetWindowRect(&rbtn);
	cwin->ShowWindow(SW_HIDE);
	m_pScrollView->Create_PicArea(i, name, filename, rbtn, fid); 
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
void CNCLForm::PicAreaClick(UINT fid, char *params)
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
	CWnd *cwin = (m_pScrollView->GetDlgItem(fid));
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
		CWnd *pwin = (m_pScrollView->GetDlgItem(fid-1));
/*
.....leave the window as it is for editbox prompt
*/
		if (!((m_type[fldno]==3)||(m_type[fldno]==11)||(m_type[fldno]==16)))
			pwin->EnableWindow(TRUE);
	}
/*
......enable input field
*/
/*
.....leave the window as it is for editbox
*/
	if (!((m_type[fldno]==3)||(m_type[fldno]==11)||(m_type[fldno]==16)))
		cwin->EnableWindow(TRUE);
/*
.....this comment will cause error if the cwin is original
.....disabled (even though we could enabled in above) when
.....we display a edit accpet box using GetInputText
	cwin->SetFocus();
*/
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
/*
......if edit type and (m_input[i]==FORM_PICK) || (m_input[i]==FORM_LOCATE) 
*/
		if (m_input[fldno]==FORM_PICK || m_input[fldno]==FORM_LABEL ||
			m_input[fldno]==FORM_SUBSCR)
			FormPick(fldno);
		else if (m_input[fldno]==FORM_LOCATE)
			FormLoc(fldno);
		else
		{
/*
......show text input prompt and edit window at the position
*/
			int size[2];
			CRect rect;
			((CEdit*)cwin)->GetWindowRect(&rect);
			size[0] = rect.Width();
			size[1] = rect.Height();
			CString input_text = params;
/*
.....if params is empty, use current value
*/
			if (input_text=="")
			{
				((CEdit*)cwin)->GetWindowText(input_text);
			}
			m_pScrollView->GetInputText(input_text, m_fldlabel[fldno], size);
			((CEdit*)cwin)->SetWindowText(input_text);
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
void CNCLForm::CtrlListCallback(UINT id, UD_TABLEINFO *info)
{
	int i, fldno,status,jmpflag;
	UD_FSTAT stat;
	int redisp = 0;
	static int start_fldno = -1;
	static int start_flag = -1;
	static int start_ans = -1;
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
/*
....sometime, the tablelist click function happend before the last focus field killfocus function call
....so the m_current_fld but Focused_frm_fldno is not -1, use it
*/
	if (m_current_fld!=-1)
	{
		status = chk_field(m_current_fld, 1, &redisp, UD_TFWD);
		if (status!=-1)
			m_current_fld = -1;
		else
			return; 
	}
	else if ((Focused_frm_fldno!=-1)&&((m_type[Focused_frm_fldno]==3)||(m_type[Focused_frm_fldno]==9)))
	{
		status = chk_field(Focused_frm_fldno, 1, &redisp, UD_TFWD);
	}

	UD_MARK(jmpflag,UU_TRUE);
	if (jmpflag != 0)
		goto done;
	if ((m_method_ret[fldno] & UD_TOGGLE &&
			m_method[fldno] != NULL)&&(m_init))
	{
/*
.....reset pick segment list before callback function
*/
		ncl_reset_pikgeom();
		UD_DDATA seldata;
		seldata.frmint = (int *)info;
/*
.....put those to avoid recursive call
*/
		if ((start_fldno==fldno) && (info->flag==start_flag) && (info->row==start_ans))
			goto done;
		start_fldno = fldno;
		start_flag = info->flag;
		start_ans = info->row;
		if ((info->flag==2)&&(m_fStruct.input_flds[fldno].ud_fprec!=0))
			return;
		stat = (m_method[fldno])(&fldno, &seldata, UD_FLDOK);
		if (fldno!=-1)
			redisp = 1;			
		if (stat==UD_FRMCLOSE)
			goto done;
		if (redisp)
		{
			uw_ntform_redisplay();
			if (info->flag!=2) //do not reflush when doing sort, filter
				ud_updatews(UG_SUPPRESS);
			start_fldno = fldno;
			start_flag = info->flag;
			start_ans = info->row;
			CNCLListCtrl* listctl = (CNCLListCtrl*)(m_pScrollView->GetDlgItem(id));
			listctl->SetFocus();
			if (info->row>=0)
			{
				listctl->SetItemState (info->row, LVIS_SELECTED, LVIS_SELECTED);
				listctl->EnsureVisible(info->row,0);
			}
		}
	}
done:
	start_fldno = -1;
	start_flag = -1;
	start_ans = -1;
	UD_UNMARK(jmpflag);
	return;
}

int CALLBACK SortFunc(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	UD_ITEMDATA* pData1 = (UD_ITEMDATA*)lParam1;
	UD_ITEMDATA* pData2 = (UD_ITEMDATA*)lParam2;
	int frmid = pData1->frmid;
	int fldno = pData1->fldno;
		
	if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
		return (NCL_MainFrame->m_formbar[frmid]->m_sortfunc[fldno])((char*)pData1, (char*)pData2, (int)lParamSort);
	else
		return (UW_display_frm[frmid]->m_sortfunc[fldno])((char*)pData1, (char*)pData2, (int)lParamSort);
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
void CNCLForm::sort_tlist(UINT fid, int isub, UD_SMETHOD sortfunc)
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

	if (m_fStruct.input_flds[fldno].ud_fprec!=0)
		return;

	m_sortfunc[fldno] = sortfunc;
	if (m_type[i]==17)
	{
		if (m_fStruct.input_flds[fldno].ud_fprec==0)
		{
			CNCLListCtrl* listctl = (CNCLListCtrl*)(m_pScrollView->GetDlgItem(fid));
			listctl->SortItems((PFNLVCOMPARE)SortFunc, isub);
		}
		return;
	}
	if (m_type[i]==19)
	{
		CNCLListCtrl2* listctl2 = (CNCLListCtrl2*)(m_pScrollView->GetDlgItem(fid));
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
**			form_list: UD_TLIST/UD_DLIST structure for redisplay
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLForm::set_tlist(int fieldno, int *form_list)
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
		listctl = (CNCLListCtrl*)(m_pScrollView->GetDlgItem(fid));
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
/*
......have to set the focus to set a selection
*/
		listctl->SetFocus();
		if ((tlist->answer>=0)&&(tlist->answer<tlist->num_item))
		{
			listctl->SetItemState (tlist->answer, LVIS_SELECTED, LVIS_SELECTED);
			listctl->EnsureVisible(tlist->answer,0);
		}
		if ((m_sortfunc[fieldno]!=NULL)&&(tlist->sort>=0)&&(tlist->sort<=tlist->num_col))
		{
			if (m_fStruct.input_flds[fieldno].ud_fprec==0)
				listctl->SortItems((PFNLVCOMPARE)SortFunc, tlist->sort);
		}
		listctl->SetTlist(tlist);
		break;
	case 19:
		dlist = (UD_DLIST *)form_list;
		listctl2 = (CNCLListCtrl2*)(m_pScrollView->GetDlgItem(fid));
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
			if (m_fStruct.input_flds[fieldno].ud_fprec==0)
				listctl2->SortItems((PFNLVCOMPARE)SortFunc, dlist->sort);
		}	
		break;
	}
}

void CNCLForm::OntlistFocus(UINT id)
{
	FormUserCallbacks7((WPARAM)id, 0);
}
/**********************************************************************
**    I_FUNCTION :  set_sort_func(int fldno, UD_SMETHOD sortfunc
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
void CNCLForm::set_sort_func(int fldno, UD_SMETHOD sortfunc)
{
	if (m_fStruct.input_flds[fldno].ud_fprec==0)
		m_sortfunc[fldno] = sortfunc;
	else
		m_sortfunc[fldno] = NULL;
}

/**********************************************************************
**    I_FUNCTION :  Adjust_pos(CRect &rect)
**       Adjust form position according to the form structure position definition. 
**    PARAMETERS   
**       INPUT  : 
**          rect : Window old postion and size
**       OUTPUT :  
**          rect : Window new postion and size
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLForm::Adjust_pos(CRect &rect)
{
	int fcx, fcy;
	int fpos[2];

	fcx = rect.Width();
	fcy = rect.Height();
/*
......those position all defined as left->right (positive) and lower->upper (position)
*/
	fpos[0] = (int)m_fStruct.ud_frmdf.ud_fwin.ll.x;
	fpos[1] = (int)m_fStruct.ud_frmdf.ud_fwin.ll.y;

	Adjust_pos2(rect, fpos[0], fpos[1], fcx, fcy, m_fStruct.att_win, m_fStruct.ref);
	return;
}
/**********************************************************************
**    I_FUNCTION :  Adjust_pos2(CRect &rect, int x, int y, int fcx,
**                              int fcy, int att_win, int ref_flag)
**       Adjust form position according to the form structure position definition. 
**    PARAMETERS   
**       INPUT  : 
**          rect : Window old postion and size
**			x,y: relative position
**			cx, cy: form size
**			att_win: relate attach window
**			ref_flag: reference flag
**       OUTPUT :  
**          rect : Window new postion and size
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLForm::Adjust_pos2(CRect &rect, int x, int y, int fcx, int fcy, int att_win, int ref_flag)
{
	int cx, cy;
	int ref[2], fpos[2], pos[2];
	CRect windowRect;
	POINT pt;
	fpos[0] = x;
	fpos[1] = y;
	if ((x==-10000)&&(y==-10000))
/*
.....use current mouse position
*/
	{
		GetCursorPos(&pt);
		pos[0] = pt.x; 
		pos[1] = pt.y;
		goto done;
	}
	if (att_win==1)
/*
......attached to SCREEN
*/
	{
/*
......Get screen size
*/
		cx = GetSystemMetrics(SM_CXSCREEN);
		cy = GetSystemMetrics(SM_CYSCREEN);
		if (ref_flag==0)
/*
......LOWER_LEFT
*/
		{
/*
......screen's lower-left
*/
			ref[0]= 0;
			ref[1] = cy;
/*
......form's lower left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = fpos[1] - fcy;
		}
		else if (ref_flag==1)
/*
......LOWER_CENTER
*/
		{
/*
......screen's lower-center
*/
			ref[0]= (int)(0.5*cx);
			ref[1] = cy;
/*
......form's lower center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = (int)(fpos[0] - 0.5*fcx);
			pos[1] = fpos[1] - fcy;
		}
		else if (ref_flag==2)
/*
......LOWER_RIGHT
*/
		{
/*
......screen's lower-right
*/
			ref[0]= cx;
			ref[1] = cy;
/*
......form's lower right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx;
			pos[1] = fpos[1] - fcy;
		}
		else if (ref_flag==3)
/*
......CENTER_LEFT
*/
		{
/*
......screen's center-left
*/
			ref[0]= 0;
			ref[1] = (int)(0.5*cy);
/*
......form's center left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = (int)(fpos[1] - 0.5*fcy);
		}
		else if (ref_flag==4)
/*
......CENTER_CENTER
*/
		{
/*
......screen's center-center
*/
			ref[0]= (int)(0.5*cx);
			ref[1] = (int)(0.5*cy);
/*
......form's center center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = (int)(fpos[0] - 0.5*fcx);
			pos[1] = (int)(fpos[1] - 0.5*fcy);
		}
		else if (ref_flag==5)
/*
......CENTER_RIGHT
*/
		{
/*
......screen's center-right
*/
			ref[0]= cx;
			ref[1] = (int)(0.5*cy);
/*
......form's center right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx;
			pos[1] = (int)(fpos[1] - 0.5*fcy);
		}
		else if (ref_flag==6)
/*
......UPPER_LEFT
*/
		{
/*
......screen's upper-left
*/
			ref[0]= 0;
			ref[1] = 0;
/*
......form's upper left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = fpos[1];
		}
		else if (ref_flag==7)
/*
......UPPER_CENTER
*/
		{
/*
......screen's upper-center
*/
			ref[0]= (int)(0.5*cx);
			ref[1] = 0;
/*
......form's upper center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = (int)(fpos[0] - 0.5*fcx);
			pos[1] = fpos[1];
		}
		else if (ref_flag==8)
/*
......UPPER_RIGHT
*/
		{
/*
......screen's upper-right
*/
			ref[0]= cx;
			ref[1] = 0;
/*
......form's upper right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx;
			pos[1] = fpos[1];
		}
	}
	else
	{
/*
......Get NCL window size
*/
		NCL_MainFrame->GetWindowRect(&windowRect);
		cx = windowRect.Width();
		cy = windowRect.Height();
		if (ref_flag==0)
/*
......LOWER_LEFT
*/
		{
/*
......window's lower-left
*/
			ref[0]= windowRect.left;
			ref[1] = windowRect.bottom;
/*
......form's lower left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = fpos[1] - fcy;
		}
		else if (ref_flag==1)
/*
......LOWER_CENTER
*/
		{
/*
......window's lower-center
*/
			ref[0]= (int)(windowRect.left + 0.5*cx );
			ref[1] = windowRect.bottom;
/*
......form's lower center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = (int)(fpos[0] - 0.5*fcx);
			pos[1] = fpos[1] - fcy;
		}
		else if (ref_flag==2)
/*
......LOWER_RIGHT
*/
		{
/*
......window's lower-right
*/
			ref[0]= windowRect.right;
			ref[1] = windowRect.bottom;
/*
......form's lower right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx;
			pos[1] = fpos[1] - fcy;
		}
		else if (ref_flag==3)
/*
......CENTER_LEFT
*/
		{
/*
......window's center-left
*/
			ref[0]= windowRect.left;
			ref[1] = (int)(windowRect.top + 0.5*cy);
/*
......form's center left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = (int)(fpos[1] - 0.5*fcy);
		}
		else if (ref_flag==4)
/*
......CENTER_CENTER
*/
		{
/*
......window's center-center
*/
			ref[0]= (int)(windowRect.left + 0.5*cx);
			ref[1] = (int)(windowRect.top + 0.5*cy);
/*
......form's center center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = (int)(fpos[0] - 0.5*fcx);
			pos[1] = (int)(fpos[1] - 0.5*fcy);
		}
		else if (ref_flag==5)
/*
......CENTER_RIGHT
*/
		{
/*
......window's center-right
*/
			ref[0]= windowRect.right;
			ref[1] = (int)(windowRect.top + 0.5*cy);
/*
......form's center right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx;
			pos[1] = (int)(fpos[1] - 0.5*fcy);
		}
		else if (ref_flag==6)
/*
......UPPER_LEFT
*/
		{
/*
......window's upper-left
*/
			ref[0]= windowRect.left;
			ref[1] = windowRect.top;
/*
......form's upper left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = fpos[1];
		}
		else if (ref_flag==7)
/*
......UPPER_CENTER
*/
		{
/*
......window's upper-center
*/
			ref[0]= (int)(windowRect.left + 0.5*cx);
			ref[1] = windowRect.top;
/*
......form's upper center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = (int)(fpos[0] - 0.5*fcx);
			pos[1] = fpos[1];
		}
		else if (ref_flag==8)
/*
......UPPER_RIGHT
*/
		{
/*
......window's upper-right
*/
			ref[0]= windowRect.right;
			ref[1] = windowRect.top;
/*
......form's upper right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx;
			pos[1] = fpos[1];
		}
	}
done:;
	rect.top = pos[1];
	rect.left = pos[0];
	rect.right = rect.left + fcx;
	rect.bottom = rect.top + fcy;
}
/**********************************************************************
**    I_FUNCTION : set_focus(int fieldno)
**       set the focus of a form field
**    PARAMETERS  
**       INPUT  : fieldno
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLForm::set_focus(int fieldno)
{
	int start, end;
	CWnd *win = NULL; 
	win = m_pScrollView->GetDlgItem(m_idtable[fieldno+m_dspno]);
	if (win==NULL)
	{
		return;
	}
/*
......the SetFocus function seems will change the insert position to beginning. 
......and when it is not has focus, GetSel(start, end) will already return 0 too
......We don't want this and rather put the insert position to end
*/
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
void CNCLForm::FormColor(int fieldno)
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
	if (MSLite==1)
	{
/*
......disable the custom color button
*/
		dlg.disable_addcolor();
	}
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
**    I_FUNCTION : FormColor(int fieldno)
**       form color button callback: diplay a color dialog
**    PARAMETERS  
**       INPUT  : fieldno
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLForm::PickColor(int fieldno)
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
	if (MSLite==1)
	{
/*
......disable the custom color button
*/
		dlg.disable_addcolor();
	}
	int nResponse = dlg.DoModal();
	COLORREF bcolor;
	UD_SELECT_STR *dselect;
	if (nResponse == IDOK)
	{
/*
.....set the field color to select color
*/
		if (dlg.m_current_color<0)
		{
			bcolor = ::GetSysColor(COLOR_BTNFACE); 
		}
		else
		{
			bcolor = RGB(uw_color_table[dlg.m_current_color][0], 
						uw_color_table[dlg.m_current_color][1], 
						uw_color_table[dlg.m_current_color][2]);
		}
		m_pScrollView->set_button_color2(fieldno, bcolor, bcolor);
		dselect = (UD_SELECT_STR *)((m_fData.ud_data[fieldno]).ud_delem.frmint);
		dselect->color = dlg.m_current_color;
	}
	else if (nResponse == IDCANCEL)
	{
	}
	GdiplusShutdown(gdiplusToken);
}

int CNCLForm::Get_fldno_from_ID(UINT id)
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
void CNCLForm::SetButColor(int fieldno, int color)
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
	if (m_type[fieldno]==1)
		m_pScrollView->set_button_color3(fieldno, bcolor, bcolor);
	else
		m_pScrollView->set_button_color(fieldno, bcolor, bcolor);
	*((m_fData.ud_data[fieldno]).ud_delem.frmint) = color;
}

/**********************************************************************
**    I_FUNCTION : SetSecColor(char *name, int color[3], int bold)
**       set form section button text color
**    PARAMETERS  
**       INPUT  : name: section name
**				color: color index to be set
**				bold: bold text
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLForm::SetSecColor(char *name, int color[3], int bold)
{
	int i;
	for (i=0; i<m_fStruct.n_section_fields; i++)
	{
		if (strcmp(m_fStruct.section_flds[i].name, name)==0)
		{
			m_pScrollView->SetSecColor(i, color, bold);
			return;
		}
	}
}
/**********************************************************************
**    I_FUNCTION : EnableSection(char *name, int flag)
**       set form section button text color
**    PARAMETERS  
**       INPUT  : name: section name
**				flag: 1: enable the section
**					0: disable the section
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLForm::EnableSection(char *name, int flag)
{
	int i;
	for (i=0; i<m_fStruct.n_section_fields; i++)
	{
		if (strcmp(m_fStruct.section_flds[i].name, name)==0)
		{
			m_pScrollView->EnableSection(i, flag);
			return;
		}
	}
}

void CNCLForm::InitSectionButtons()
{
	if (UD_dispfrm[m_frmid]==NULL) return;

	if (m_fStruct.n_section_fields<=0)
		return;

	int i, indx;
	UINT fid;
	COLORREF ocolor = ::GetSysColor(COLOR_BTNTEXT);
	COLORREF color;
	setsecbut(m_fStruct.n_section_fields);
	int page = 0;
	for (i=0; i<m_fStruct.n_section_fields; i++)
	{
		if (m_fStruct.section_flds[i].type==0)
		{
			fid = IDC_FORMSECTION1 + page;  
			indx = uw_get_rgb (m_fStruct.section_flds[i].color, color);
			if (indx==-1)
				color = ocolor;	
			m_pScrollView->create_secbutton(m_fStruct.section_flds[i].name, i, page, fid, color, 1);
			m_idsection[i] = fid;
			page++;
		}
		else
			m_pScrollView->create_secbutton(m_fStruct.section_flds[i].name, i, page, fid, color, 3);
	}
}
void  CNCLForm::setsecbut(int num) 
{
	m_secno = num;
}	
/*
......here page is not index of select button, but page number (not include seperator)
*/
void CNCLForm::OnSecButton(int page)
{
	int i, chk;
	char ldat[UX_MAX_PATH_LEN];
	CButton* cbut;
	CComboBox* cmbbx;
	CEdit* cedt;
	char perc_str[10];
	CProgressCtrl* cprocess;
	int defchc, perc, redisp, stat;
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return;
	UINT fid;
/*
.....handle the last field callback if any
*/
	if (m_current_fld!=-1)
	{
		stat = chk_field(m_current_fld, 1, &redisp, UD_TFWD);
		if (stat!=-1)
		{
			m_current_fld = -1;
			if (redisp)
				uw_ntform_redisplay();
		}
		else
			return; 
	}

	if (NCL_form_SaveData()==0)
		return;
	m_pScrollView->OnSecButton(page);

	int CmdShow;
	for (i=0;i<m_fStruct.n_input_fields;i++)
	{
		fid =  m_idtable[i+m_dspno];
		CmdShow = SW_SHOW;
		if ((m_fStruct.input_flds[i].section==page)||(m_fStruct.input_flds[i].section==-1))
		{
			CmdShow = SW_SHOW;
		}
		else
			CmdShow = SW_HIDE;
		if (m_disp_mask[i+m_dspno]==0)
			CmdShow = SW_HIDE;
/*
.....we need reset window value here is because when the page is not show,
.....we didn't call there function in redisplay function to avoid recursive value set
*/
		if (CmdShow==SW_SHOW)
		{
/*
......initial data here before showing to avoid callback function to be called
*/
			switch(m_type[i])
			{
			case 2:
				cmbbx = (CComboBox*)(m_pScrollView->GetDlgItem(fid));
				defchc = m_fData.ud_data[i].ud_delem.frmint[0];
				cmbbx->SetCurSel(defchc);
				break;
			case 3:
			case 11:
			case 16:
				if (m_fData.ud_data[i].dflg!=2)
					break;
				m_fData.ud_data[i].dflg = 1;
				cedt = (CEdit*)(m_pScrollView->GetDlgItem(fid));
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
			case 7:
				cbut = (CButton*)(m_pScrollView->GetDlgItem(fid));
				defchc = m_fData.ud_data[i].ud_delem.frmint[0];
				cbut->SetCheck(defchc);
				break;
			case 10:
				if (m_fData.ud_data[i].dflg==2)
				{
					m_fData.ud_data[i].dflg = 1;
					if (m_init)
						OnPaint();
				}
				break;
			case 13:
				if (m_fData.ud_data[i].dflg!=2)
					break;
				m_fData.ud_data[i].dflg = 1;
				cprocess = (CProgressCtrl*)(m_pScrollView->GetDlgItem(fid));
				perc = m_fData.ud_data[i].ud_delem.frmint[0];
				cprocess->SetPos(perc);
				if (m_fStruct.input_flds[i].ud_datatyp==UD_DASINT)
				{
					fid =  m_idtable[i+m_dspno+m_fldno+m_framno+m_picno];
					sprintf_s(perc_str, sizeof(perc_str), "%d%%", perc);
					(m_pScrollView->GetDlgItem(fid))->SetWindowText(perc_str);
				}
				break;		
			case 8:
			case 9:
			case 15:
			case 5:
				break;
			}
		}
		if ((m_type[i]==2)||(m_type[i]==8)
				||(m_type[i]==9)||(m_type[i]==10) ||(m_type[i]==13)
				||(m_type[i]==14) || (m_type[i] == 15)||(m_type[i]==16))
		{
			m_pScrollView->GetDlgItem(fid)->ShowWindow(CmdShow);
			m_pScrollView->GetDlgItem(fid-1)->ShowWindow(CmdShow);
		}
		else if ((m_type[i]==3)||(m_type[i]==11))
		{
			m_pScrollView->GetDlgItem(fid)->ShowWindow(CmdShow);
			m_pScrollView->GetDlgItem(fid-1)->ShowWindow(CmdShow);
		}
		else if (m_type[i]==18)
		{
			m_pScrollView->GetDlgItem(fid)->ShowWindow(CmdShow);
			m_pScrollView->GetDlgItem(fid-1)->ShowWindow(CmdShow);
		}
		else
		{
			m_pScrollView->GetDlgItem(fid)->ShowWindow(CmdShow);
		}
	}
	for (i=0;i<m_fStruct.n_display_fields;i++)
	{
		fid =  m_idtable[i];
		if ((m_fStruct.display_flds[i].section==page)||(m_fStruct.display_flds[i].section==-1))
		{
			if (m_disp_mask[i]==0)
				m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_HIDE);
			else
				m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_SHOW);
		}
		else
		{
			m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_HIDE);
		}
	}
	for (i=0;i<m_fStruct.n_picture_fields;i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno+m_framno];
		if ((m_fStruct.picture_flds[i].section==page)||(m_fStruct.picture_flds[i].section==-1))
		{
			if (m_fStruct.picture_flds[i].show_flag)
				m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_SHOW);
			else
				m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_HIDE);
		}
		else
			m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_HIDE);
	}
	for (i=0; i<m_fStruct.n_frame_fields; i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno];
		if ((m_fStruct.frame_flds[i].section==page)||(m_fStruct.frame_flds[i].section==-1))
		{
			m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_SHOW);
		}
		else
			m_pScrollView->GetDlgItem(fid)->ShowWindow(SW_HIDE);
	}
/*
.....update the helpbox if it is displayed
*/
	if ((m_helpbox==NULL)||(m_secno<=0))
	{
		return;
	}
	if (m_helptext!=NULL)
	{
/*
......replace '\n" with '\r\n'
*/
		int len = strlen(m_helptext);
		char *dsptext = new char[len+10000];

		GetSecHelpText(dsptext);
		m_helpbox->SetText(dsptext);
		delete (dsptext);
	}
}

void CNCLForm::SelectSection(char *sec_name)
{
	int i, sep=0,page=-1;
	for (i=0; i<m_fStruct.n_section_fields;i++)
	{
		if (m_fStruct.section_flds[i].type==3)
			sep++;
		if (strcmp(m_fStruct.section_flds[i].name, sec_name)==0)
		{
			page = i-sep;
			break;
		}
	}
	if (page>=0)
		OnSecButton(page);
}

void CNCLForm::SetFieldColr(int fieldno, int fg, int bg)
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
		m_pScrollView->set_button_color2(fieldno, bcolor, bcolor);
		dselect = (UD_SELECT_STR *)((m_fData.ud_data[fieldno]).ud_delem.frmint);
		dselect->color = bg;
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
			m_pScrollView->set_button_color(fieldno, bcolor, bcolor);
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
			m_pScrollView->set_button_color3(fieldno, bcolor, fcolor);
		}
		return;
	}
/*
.....for other standard dialog item, so have to change the color by
.....call OnCtlColor which will called by system if we repaint the view
*/
	m_pScrollView->OnPaint();
}

/**********************************************************************
**    I_FUNCTION :  uw_ntset_sort_func(int frmid, int fieldno, UD_SMETHOD sortfunc)
**       set the user defined 'sortfunc for table list field
**    PARAMETERS   
**       INPUT  : 
**          frmid : form ID
**			fieldno: field number
**			sortfunc: sort function to be set
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntset_sort_func(int frmid, int fieldno, UD_SMETHOD sortfunc)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->set_sort_func(fieldno, sortfunc);
		else
			UW_display_frm[frmid]->set_sort_func(fieldno, sortfunc);
	}
	return;
}
/**********************************************************************
**    I_FUNCTION : uw_ntfrm_set_focus(frmid, fieldno)
**       set the focus of a form field
**    PARAMETERS  
**       INPUT  : frmid, fieldno
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntfrm_set_focus(int frmid, int fieldno)
{
	Focused_frmid = frmid;
	Focused_frm_fldno = fieldno;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->set_focus(fieldno);
		else
			UW_display_frm[frmid]->set_focus(fieldno);
	}
	return 0;
}

void CNCLForm::OnFilterTlist(int frmfld)
{
	if (m_fStruct.input_flds[frmfld].ud_flen!=0)
		return;
	if (m_type[frmfld]!=17)
		return;
	UD_TLIST *tlist = (UD_TLIST *)((m_fData.ud_data[frmfld]).ud_delem.frmint);
	UD_TLIST tmplist;
	m_pScrollView->GetCurrentTlist(frmfld, &tmplist);
	ud_free_tlist(tlist);
	(m_fData.ud_data[frmfld]).ud_delem.frmint = (int*)&tmplist;
}

void CNCLForm::reset_picarea(int picno, int n_picarea, UD_PICAREA *picarea, int fldno)
{
	UINT fid =  m_idtable[fldno+m_dspno];
	set_picarea(picno, n_picarea, picarea, fid);
}
/***********************************************************************
c
c   FUNCTION: uw_ntform_set_picarea(int frmid, int picno, 
c                                   int n_picarea, UD_PICAREA *picarea,
c                                   int fldno)
c
c       Set the form pocket flag
c
c   INPUT:  frmid: form id
c			flag: pocket flag
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" void uw_ntform_set_picarea(int frmid, int picno, 
			int n_picarea, UD_PICAREA *picarea, int fldno)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]==NULL)
		return;
	((CNCLForm*)UW_display_frmwin[frmid])->reset_picarea(picno, n_picarea, picarea, fldno);
}

IMPLEMENT_DYNAMIC(CNCLForm, CDialog)
/* End of wsntform2.cpp */
#endif
