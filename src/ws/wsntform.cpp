#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**
**   FILE NAME: wsntform.cpp
**
**	 Description - Functions and implementations for
**		CNCLForm class (NCL forms) and some form functions
**
**	 CONTAINS:
**		uw_ntclose_form(int flag)
**		uw_ntform(UD_FSTRUCT *fstruct, UD_FDATA *fdata)
**		uw_ntreset_list(int fieldno, UD_LIST * form_list)
**		uw_ntget_field(fieldno, data)
**		uw_ntform_invis()
**		uw_ntform_vis()
**		uw_ntform_display()
**		uw_ntform1()
**		uw_ntclose_dispfrm(int frmid)
**		uw_ntget_frmfield()
**		uw_ntfrm_setlist()
**		uw_ntupdate_form(int frmid)
**		uw_nt_flush_win()
**		uw_ntdspfrm_invis()
**		uw_ntdspfrm_vis()
**		uw_ntdisfrm_set_label()
**		uw_if_formopen()
**
**		class functions of CNCLForm class
**		together with wsntform2.cpp, it will
**		includes all CNCLForm class functions
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntform.cpp , 26.3
**    DATE AND TIME OF LAST  MODIFICATION
**			09/20/18 , 11:47:02
***********************************************************************
*/
#include "wsntstdafx.h"
#include <math.h>
#include "dinput.h"
#include "wsntframe.h"
#include "wsntdoc.h"
#include "wsntview.h"
#include "wsntform.h"
#include "wsntpktwin.h"
extern "C" int ug_save_event();
extern "C" int ug_reset_event();
extern "C" int uz_wnt_callfunc(int num, int flag); 
#include "wsntcfunc.h"
extern "C" int UW_pic_size[2];
extern "C" int UW_frmtext_select;
#include "dselect.h"
#include "lipv.h"

CNCLForm *UW_active_frm = NULL;
CWnd *UW_display_frmwin[60] = 
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
CNCLForm *UW_display_frm[60] = 	
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };

static int current_frmid = 0;
static int save_formActFrm = 0;
extern "C" UD_FSTRUCT *UD_dispfrm[60];
extern "C" UD_FDATA *UD_dispfdata[60];
extern DWORD UW_theme;
extern char UW_formdlg_font[];
extern "C" int UW_form_fontsize;
extern "C" char UW_form_font[20];

extern "C" void ud_save_frmpos(char*,int, int, int, int, int*, int,int,int,int);
extern "C" void ud_lgeo(int flag, int *bitary);

#define FORM_STRING		1
#define FORM_PICK		2
#define FORM_LOCATE		3
#define FORM_RECORD  4
#define FORM_LABEL   5
#define FORM_SUBSCR  6

#define TEXT_WID	4

static int S_close_save = 0;
extern int Arrow_Len;
extern double Arrow_Angle;
extern CDialog *Pocket_Win[UM_MAX_POCKET];
extern CWnd *NCL_Main_View;

extern "C" UD_FSTRUCT *UD_frm;
extern "C" UD_FDATA *UD_fdata;

extern "C" float uw_form_scalex;
extern "C" float uw_form_scaley;
extern "C" int UD_form_bypick;
extern "C" int UW_auto_cursor;
extern "C" int uw_ntform1(UD_FSTRUCT *fstruct, UD_FDATA *fdata, int dflag);
extern "C" int uw_nt_flush_win();
extern "C" void uw_ntsetcursor(int cursorno);
extern "C" int uw_ntgetcur_cursor();
extern "C" int uw_ntdispmsg (char *msg);
extern "C" void uw_ntclose_dispfrm(int frmid);
extern "C" int uw_ntget_ctrpos(int *x, int *y, int flag);
extern CMainFrame *NCL_MainFrame;
extern "C" void uw_ntset_context(UM_pkwin_type which, UU_LOGICAL force);
extern "C" int ug_save_input(int** save_inptr_pt);
extern "C" int ug_reset_input(int* save_inptr);
extern "C" int ud_setpick_type(int);
extern "C" int ud_getpick_type();
extern "C" int uz_ntparse_frmkey(char *buf, int numint, BYTE* ktyp,WORD* ksub);
extern "C" int ud_updatews(Gregen);
extern "C" void	uw_ntreset_redraw();
extern "C" void ncl_reset_pikgeom();
extern "C" void uw_ntget_ecurpos(int *start, int *end);
extern "C" int ul_open_mod_file2(char *in_dir, char *in_subdir, char *infname, int wr_flag, FILE **fptr, char*, char*);
extern "C" void uw_setform_active(int flag);
extern "C"  int ud_get_pick_selstr(char *prompt, UU_LIST *sflst, int color);
extern "C"  void ud_unhilite_sellist(UU_LIST *select_list);
extern "C" int uw_if_formopen();
extern "C" void uw_ntget_size_ratio(int pt1, char *fntname1, int pt2, char *fntname2,
			double *ratio_x, double *ratio_y);
extern "C" void uw_ntget_pixelsize_from_unit(int pt1, char *fntname1, int unitx, int unity, int *pic_x, int *pic_y);
static double S_pic_ratio_x = 1.0;
static double S_pic_ratio_y = 1.0;
/**********************************************************************
**    E_FUNCTION :  uw_ntclose_form(int flag)
**       Close current NCL Form and save the form data
**       depond on input flag
**       This function can let other functions close the form
**       other than push cancel/accept buttons. 
**			(only for input type)
**    PARAMETERS
**       INPUT  :
**				flag: 1: save data and close form
**					0: don't close form
**       OUTPUT :
**          none
**    RETURNS      : 1 on success.
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntclose_form(int flag)
{ 
	if (flag)
		return UW_active_frm->form_accept_close();
	else
		return 1;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntreset_list(fno,form_list)
**       Redisplays the form list field fno. (only for input type)
**    PARAMETERS   
**       INPUT  : 
**          fno : field number
**			form_list: UD_LIST structure for redisplay
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntreset_list(int fieldno, UD_LIST * form_list)
{
	if (UW_active_frm!=NULL)
		UW_active_frm->set_list(fieldno, form_list);
	return 0 ;
}

/**********************************************************************
**    E_FUNCTION :  int uw_ntform(fstruct,fdata)
**       Displays a Window dialog Style Form and waits for user input.
**    PARAMETERS   
**       INPUT  : 
**          fstruct:	Form structure.
**			fdata		Default form data.
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS on success.
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntform(UD_FSTRUCT *fstruct, UD_FDATA *fdata)
{
	return uw_ntform1(fstruct, fdata, 0);
}

/*********************************************************************
**    E_FUNCTION : uw_ntget_field(fieldno, data,str_flag)
**			Get field UD_DDATA of active form
**			(only for input type)
**    PARAMETERS   
**       INPUT  :
**          fieldno   : Field number
**          data      : UD_DDATA structure 
**          str_flag  : UU_TRUE if a character string is to be returned
**                      with each field type.
**       OUTPUT :
**          data      : UD_DDATA structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntget_field(int fieldno,UD_DDATA data,UU_LOGICAL str_flag)
{
	UW_active_frm->Get_field_data(fieldno, data, str_flag);
	return 0 ;
}

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/

BEGIN_MESSAGE_MAP(CNCLForm, CDialog)
	//{{AFX_MSG_MAP(CNCLForm)
	ON_WM_PAINT()
	ON_WM_CLOSE()
	ON_WM_SIZE()
	ON_WM_MOVE()
	ON_WM_SHOWWINDOW()
    ON_WM_NCLBUTTONDOWN() 
	ON_WM_LBUTTONUP()
	ON_WM_NCHITTEST()
	ON_WM_CTLCOLOR()
//	ON_WM_VSCROLL()
//	ON_WM_HSCROLL()
	ON_COMMAND(ID_FORM_HIGHLIGHT, OnFormTextHighlight)
	ON_COMMAND(ID_FORM_TABED, OnFormTabbed)
	ON_COMMAND(ID_FORM_STABED, OnFormSTabbed)
	ON_COMMAND_RANGE(ID_FORM_HOTKEY1, ID_FORM_HOTKEY4, OnAccelFunctions)
	ON_COMMAND(IDC_FORMACCEPT, FormAccept)
	ON_COMMAND(IDC_FORMCANCEL, OnFormClose)
	ON_COMMAND(IDC_FORMHELP, FormHelp)
	ON_COMMAND(IDF_FORM_RETURN, FormUserCallbacks6)
	ON_COMMAND_RANGE(IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks0)
	ON_MESSAGE(WM_VIEWCOMMAND, FormUserCallbacks1)
	ON_MESSAGE(CBN_VIEWSELCHANGE, FormUserCallbacks3)
	ON_MESSAGE(LBN_VIEWSELCHANGE, FormUserCallbacks2)
	ON_MESSAGE(EN_VIEWSETFOCUS, FormUserCallbacks5)
	ON_MESSAGE(EN_VIEWKILLFOCUS, FormUserCallbacks4)
	ON_MESSAGE(CBN_VIEWKILLFOCUS, FormUserCallbacks4)
	ON_MESSAGE(CBN_VIEWSETFOCUS, FormUserCallbacks7)
	ON_MESSAGE(LBN_VIEWSETFOCUS, FormUserCallbacks7)
	ON_MESSAGE(BN_VIEWSETFOCUS, FormUserCallbacks7)
	ON_MESSAGE(WM_KICKIDLE, OnKickIdle)
	ON_MESSAGE(ID_LISTITEM_CLICK, OnItemClick)
//LBN_DBLCLK goes to FormUserCBLDblclk too
	ON_MESSAGE(CBN_DBLCLK, FormUserCBLDblclk)
	ON_MESSAGE(NM_DBLCLK_NCL, FormUserCBLDblclk)
	ON_MESSAGE(WM_CTLCOLORSTATIC, OnCtrlColorStatic)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

LRESULT CNCLForm::OnCtrlColorStatic(WPARAM wParam, LPARAM lParam)
{
	int i, len,ret;
	char tempstr[256], fcolor_str[256], bcolor_str[256], *tok;
	COLORREF fcolor, bcolor;
		
	fcolor = RGB(255,0,0);
//	bcolor = ::GetSysColor(COLOR_BTNFACE);
	bcolor = RGB(255,0,0);
	SetTextColor((HDC)wParam, fcolor);
	SetBkColor((HDC)wParam, bcolor);
	static HBRUSH bh = CreateSolidBrush(bcolor);
	return (LRESULT)bh;
}

void CNCLForm::OnShowWindow(BOOL bShow, UINT nStatus)
{
/*
......when hide the form, we need close the relate tooltip (such as picture area might have tooltip window)
*/
	if (bShow==0)
	{
		m_pScrollView->close_tooltip(); 
	}
}

LRESULT CNCLForm::OnKickIdle(WPARAM wparm, LPARAM lparm)
{
/*
.....for the modals dialog, when hide it, it treat as minimized
.....sometime if we purposed hide it, we don't want it display
*/
	if ((IsIconic()==0)&&(m_show==1))
	{
		ShowWindow(SW_SHOWNORMAL);
		UpdateWindow();
	}
	return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  CNCLForm
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/

CNCLForm::CNCLForm(CWnd* pParent, int dispflag)	: CDialog(IDD_FORMDIALOG)
{
	int i;
	m_frmid = 0;
	m_pParent = pParent;
	m_pParentWnd = pParent;
	m_disptype = dispflag;
	m_itemnum = 0;
	m_fldno = 0;
	m_title[0] = '\0';
	m_filename[0] = '\0';
	m_savfrm = NULL;
	for (i=0; i<MAX_FORMITEM; i++)
	{
		m_formchcnum[i] = 0;
		m_formchoice[i] = NULL;
		m_input[i] = FORM_STRING;
		m_modal[i] = 0;
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
	for (i=0; i<MAX_FORMITEM; i++)
		m_fBkBrush[i] = NULL;
	m_childnum = 0;
	m_parentID = -1;
	for (i=0; i<60; i++)
		m_childfrm[i] = -1;
	m_init = 0;
	m_oldsize.top = m_oldsize.bottom = m_oldsize.left = m_oldsize.right = 0;
	m_pScrollView = NULL;
	m_form_accel = (ACCEL *)malloc(200*sizeof(ACCEL));
	m_form_accelnum = 0;
	m_hotkey_num[0] = -1;
	m_hotkey_num[1] = -1;
	m_hotkey_num[2] = -1;
	m_hotkey_num[3] = -1;
	m_forward = 1;
	m_fldrec = m_fldlrec = m_dsprec = m_frmrec = m_picrec = NULL;
	m_def_id = IDC_FORMACCEPT;
	m_dlgtyp = 1;
	m_secno = 0;
	m_init_data_type_flag = 0;
	m_show = 1;
	SetThemeAppProperties(UW_theme);
//	strcpy(UW_formdlg_font, UW_form_font);
	strcpy(UW_formdlg_font, "MS Sans Serif");
}

/***********************************************************************
c
c   SUBROUTINE:  ~CNCLForm
c
c   FUNCTION:  Destructor, free class data space
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLForm::~CNCLForm()
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
	if (m_helptext != NULL)
		free(m_helptext);
	if (m_pocket)	
	{
		um_close_pocket_window(UM_DRAWING_WINDOW);
		um_close_pocket_window(UM_GRAPHIC_WINDOW);
		m_pocket = 0;
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
	if (m_form_accel!=NULL)
		free ((char*)m_form_accel);
	if (m_fldrec != NULL)
		free (m_fldrec);
	if (m_fldlrec != NULL)
		free (m_fldlrec);
	if (m_dsprec != NULL)
		free (m_dsprec);
	if (m_frmrec != NULL)
		free (m_frmrec);
	if (m_picrec != NULL)
		free (m_picrec);
/*
.....if there is still form not closed, do not do this
.....it only reset when last form is closed
*/
	if (uw_if_formopen()==0)
		SetThemeAppProperties(STAP_ALLOW_NONCLIENT);
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
void CNCLForm::OnPaint() 
{
	CDialog::OnPaint();
}

/***********************************************************************
c
c   FUNCTION: repaint_pic()
c		repaint the picture area if have any.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLForm::repaint_pic() 
{
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
		CWnd *picctl = m_pScrollView->GetDlgItem(fid);
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
void CNCLForm::initform(UD_FSTRUCT *fstruct, UD_FDATA *fdata)
{
	int i, j, len, x,y,wid,twid,hgt, pwid, phgt,posx,posy,inpwid, istat;
	UINT fid, tempid;
	int pic_x1, pic_x2, pic_y1, pic_y2;
	int butlen, chrwid, pwid2;
	char perc_str[10], tempstr[40], msg[400];
	BYTE  ktyp;
	WORD  kkey;
	CRect rect2;
/*
.....header info
*/
	strcpy_s(m_title, sizeof(m_title), (*fstruct).ud_frmdf.ud_fid);
	m_dlgTempl.x = 0;
	m_dlgTempl.y = 0;
	m_dlgTempl.cx = (short)((*fstruct).ud_frmdf.ud_frc.ud_c*uw_form_scalex);
	m_dlgTempl.cy = (short)((*fstruct).ud_frmdf.ud_frc.ud_r*uw_form_scaley);
	if (fstruct->n_section_fields>0)
	{
		m_dlgTempl.cy -= 20;
	}
	else
	{
		m_dlgTempl.cy -= 25;
	}
/*
.....label item info
*/
	m_itemnum = 0;
/*
......create 2 boxs
*/
	int start_x = 0;
	uw_ntget_size_ratio(80, "MS Sans Serif", UW_form_fontsize*10, UW_formdlg_font,
			&S_pic_ratio_x, &S_pic_ratio_y);
/*
......this buttion will be created when initial dialog form
*/
	int widlabel = 0;
	for (i=0; i<fstruct->n_section_fields; i++)
	{
		uw_ntget_strsize(fstruct->section_flds[i].name, UW_form_fontsize*10, UW_formdlg_font, &chrwid, &phgt);
		if (widlabel<chrwid)
			widlabel = chrwid;
	}
/*
.....we need consider bold text, so adjust by widlabel*1.02
*/
	widlabel = widlabel*1.02;
	if (widlabel<50)
		widlabel = 50;
	if (fstruct->n_section_fields>0)
	{
		start_x = widlabel + 10;
		m_dlgTempl.cx += start_x;
		m_rgDlgItem[m_itemnum++].Inittemp("", 12, IDC_DLG_BOX1, 0, 0, start_x-5, m_dlgTempl.cy);
		m_rgDlgItem[m_itemnum++].Inittemp("", 17, IDC_DLG_BOX2, start_x, 0, m_dlgTempl.cx-start_x, m_dlgTempl.cy);
	}
	m_fldno = fstruct->n_input_fields;
	m_dspno = fstruct->n_display_fields;
	m_framno = fstruct->n_frame_fields;
	m_picno = fstruct->n_picture_fields;
	for (i=0; i<fstruct->n_display_fields; i++)
	{
		fid = IDC_FORMITEM1 + m_itemnum;  
		strcpy_s(m_dsplabel[i], sizeof(m_dsplabel[i]), fstruct->display_flds[i].message);
		x = (int)(fstruct->display_flds[i].pos.ud_c*uw_form_scalex);
		y = (int)(fstruct->display_flds[i].pos.ud_r*uw_form_scaley);
		uw_ntget_strsize(m_dsplabel[i], 80, "MS Sans Serif", &wid, &hgt);
//temp yu		uw_ntget_strsize(m_dsplabel[i], UW_form_fontsize*10, UW_formdlg_font, &wid, &hgt);
		if ((fstruct->display_flds[i].pos.x>0) 
			&& (fstruct->display_flds[i].pos.y>0))
		{
			wid = (int)(fstruct->display_flds[i].pos.x);
			hgt = (int)(fstruct->display_flds[i].pos.y);
		}
		m_rgDlgItem[m_itemnum++].Inittemp(m_dsplabel[i], 4, 
						fid, x+start_x, y, wid, hgt);
		m_idtable[i] = fid;
	}
	for (i=0; i<fstruct->n_input_fields; i++)
	{
		fid = IDC_FORMITEM1 + m_itemnum;
		strcpy_s(m_fldlabel[i], sizeof(m_fldlabel[i]), fstruct->input_flds[i].prompt);
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
		else if (fstruct->input_flds[i].toggle == 19)
			m_type[i] = 19;
		else if (fstruct->input_flds[i].toggle == 23)
			m_type[i] = 23;
		else if (fstruct->input_flds[i].toggle == 24)
			m_type[i] = 24;
		else if (fstruct->input_flds[i].toggle == 25)
			m_type[i] = 25;
		m_datatype[i] = fstruct->input_flds[i].ud_datatyp;
/*
.....input filed
*/
		if ((m_type[i]==3) || (m_type[i]==5) || (m_type[i]==11)|| (m_type[i]==23))
		{
			m_input[i] = fstruct->input_flds[i].ud_input;
			for (j=0;j<UD_NMENTWD;j++)
				m_pick_mask[i][j] = fstruct->input_flds[i].ud_limit[j];
			m_modal[i] = fstruct->input_flds[i].ud_modal;
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
				strcpy_s(m_formchoice[i][j], len+1, fstruct->input_flds[i].defaults[j].dstr);
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
//temp yu				uw_ntget_strsize(m_fldlabel[i], UW_form_fontsize*10, UW_formdlg_font, &pwid, &phgt);
			else
			{
/*
.....we will use phgt
*/
				uw_ntget_strsize("X", 80, "MS Sans Serif", &chrwid, &phgt);
//temp yu				uw_ntget_strsize("X", UW_form_fontsize*10, UW_formdlg_font, &chrwid, &phgt);
				pwid = 0;
			}
/*
.....the calculate string size is little different 
.....from displaying, so it may overlap the input field,
.....so we were not allow "overlap" happened, so, just
.....make label short if label overlap the input field
*/
			if ((fstruct->input_flds[i].ud_prmloc.x!=-1)
				&&((fstruct->input_flds[i].ud_prmloc).ud_r==(fstruct->input_flds[i].ud_prmloc).y))
			{
				if (x+pwid>fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex)
					pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x -1);
			}
			m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 4, 
						fid, x+start_x, y, pwid, phgt);	
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
//			if ((phgt*13>hgt)&&(m_type[i]==15))
//				hgt = phgt*13;
			if ((phgt*12>hgt)&&(m_type[i]==15))
				hgt = phgt*12;
			if ((m_type[i]!=10) && (m_type[i]!=13) && (m_type[i]!=9))
				m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
							fid, posx+start_x, posy, inpwid, hgt);
			else if (m_type[i]==9)
			{
				twid = (int)(abs(m_len[i]) * TEXT_WID * uw_form_scalex + 6);
				m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
							fid, posx+start_x, posy, inpwid, hgt);
			}
			else if (m_type[i]==13)
			{
				if (fstruct->input_flds[i].ud_datatyp==UD_DASINT)
				{
					sprintf_s(perc_str, sizeof(perc_str), "%d%%", 100);
					uw_ntget_strsize(perc_str, 80, "MS Sans Serif", &pwid2, &phgt);
//temp yu					uw_ntget_strsize(perc_str, UW_form_fontsize*10, UW_formdlg_font, &pwid2, &phgt);
					m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
							fid, posx+start_x, posy, inpwid-pwid2-1, hgt);
				}
				else
					m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
							fid, posx+start_x, posy, inpwid, hgt);
			}
			else
				m_rgDlgItem[m_itemnum++].Inittemp("", m_type[i], 
							fid, posx+start_x, posy, wid, hgt);		
		}
		else if  (m_type[i]==23)
		{
			uw_ntget_strsize(m_fldlabel[i], 80, "MS Sans Serif", &pwid, &phgt);
//temp yu			uw_ntget_strsize(m_fldlabel[i], UW_form_fontsize*10, UW_formdlg_font, &pwid, &phgt);
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
						tempid, x+start_x, y, butlen, hgt);	
		
			fid = IDC_FORMITEM1 + m_itemnum;
			if (fstruct->input_flds[i].ud_prmloc.x==-1)
			{
				posx = (int)(x+pwid*1.2);
				inpwid = wid-pwid*1.2;
			}
			else
			{
				posx = (int)((fstruct->input_flds[i].ud_prmloc.x)*uw_form_scalex);
				inpwid = wid-(posx-x);
			}
			if (fstruct->input_flds[i].ud_prmloc.y==-1)
				posy = y;
			else
				posy = (int)((fstruct->input_flds[i].ud_prmloc.y)*uw_form_scaley);

			m_rgDlgItem[m_itemnum++].Inittemp("", 1, 
						fid, posx+start_x, posy, inpwid, hgt);
		}
/*
......if it is EDIT, we check if user want label or "pick", "locate" button
*/
		else if ((m_type[i]==3)||(m_type[i]==11))
		{
			uw_ntget_strsize(m_fldlabel[i], 80, "MS Sans Serif", &pwid, &phgt);
//temp yu			uw_ntget_strsize(m_fldlabel[i], UW_form_fontsize*10, UW_formdlg_font, &pwid, &phgt);

/*
.....the calculate string size is little different 
.....from displaying, so it may overlap the input field,
.....so we were not allow "overlap" happened, so, just
.....make label short if label overlap the input field
*/
			if (fstruct->input_flds[i].ud_prmloc.x!=-1)
			{
/*
.....even if the label/button size is shorter as it should be,
.....we still use size longer to reach the second position (look better
.....for button if line up)
*/
//				if (x+pwid>fstruct->input_flds[i].ud_prmloc.x*uw_form_scale)
					pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x - 1);
			}

			if (m_input[i]==FORM_STRING)
			{
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 4, 
						fid, x+start_x, y, pwid, phgt);	
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
//						fid, x, y, butlen, hgt);
				tempid = IDD_FORM_TEMPBUT1 + i;
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 1, 
						tempid, x+start_x, y, butlen, hgt);	
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
//						fid, x, y, butlen, hgt);
				tempid = IDD_FORM_TEMPBUT1 + i;
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 1, 
						tempid, x+start_x, y, butlen, hgt);	
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
						fid, posx+start_x, posy, inpwid, hgt);
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
//temp yu			uw_ntget_strsize(m_fldlabel[i], UW_form_fontsize*10, UW_formdlg_font, &pwid, &phgt);
			if (fstruct->input_flds[i].ud_prmloc.x!=-1)
			{
				pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x - 1);
			}
			m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 4, 
						fid, x+start_x, y, pwid, phgt);	
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
						tempid, posx+start_x, posy, inpwid, hgt);	
		}
		else if (m_type[i]==24)
		{
			uw_ntget_strsize(m_fldlabel[i], 80, "MS Sans Serif", &pwid, &phgt);
//temp yu			uw_ntget_strsize(m_fldlabel[i], UW_form_fontsize*10, UW_formdlg_font, &pwid, &phgt);
			if (fstruct->input_flds[i].ud_prmloc.x!=-1)
			{
				pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x - 1);
			}
			m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 4, 
						fid, x+start_x, y, pwid, phgt);	
			fid = IDC_FORMITEM1 + m_itemnum;
			if (fstruct->input_flds[i].ud_prmloc.x==-1)
			{
//				if (fstruct->input_flds[i].justified==0)
				{
					posx = x+pwid;
					inpwid = wid-pwid;
				}
/*				else
				{
					posx = x+pwid;
					inpwid = 10;
				}
*/
			}
			else
			{
//				if (fstruct->input_flds[i].justified==0)
				{
					posx = (int)((fstruct->input_flds[i].ud_prmloc.x)*uw_form_scalex);
					inpwid = wid-(posx-x);
				}
/*				else
				{
					posx = (int)((fstruct->input_flds[i].ud_prmloc.x)*uw_form_scalex);
					inpwid = 10;
				}
*/			}
			if (fstruct->input_flds[i].ud_prmloc.y==-1)
			{
//				if (fstruct->input_flds[i].justified==0)
					posy = y;
//				else
//					posy = y + phgt/2;
			}
			else
			{
//				if (fstruct->input_flds[i].justified==0)
					posy = (int)(fstruct->input_flds[i].ud_prmloc.y*uw_form_scaley);
//				else
//					posy = (int)(fstruct->input_flds[i].ud_prmloc.y*uw_form_scaley) + phgt/2;
			}
/*
.....here create a temp window to have the position here
*/
			tempid = IDD_FORM_TEMPSLIDER1 + i;
			m_rgDlgItem[m_itemnum++].Inittemp("", 1, 
						tempid, posx+start_x, posy, inpwid, hgt);	
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
						tempid, x+start_x, y, wid, hgt);	
			}
			else if ((tdata!=NULL)&&(m_type[i]==7))
			{
				tempid = IDD_FORM_TEMPBUT1 + i;
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], m_type[i], 
						tempid, x+start_x, y, wid, hgt);	
			}
/*
.....image botton
*/
			else if ((tdata!=NULL)&&(m_type[i]==25))
			{
				tempid = IDD_FORM_TEMPBUT1 + i;
				m_rgDlgItem[m_itemnum++].Inittemp(" ", m_type[i], 
						tempid, x+start_x, y, wid, hgt);	
			}
			else if ((m_type[i]==17)||(m_type[i]==19))
			{
				tempid = IDD_FORM_TEMPLIST1 + i;
/*
.....create normal listbox first to have the right pos and size
*/
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], 5, 
						tempid, x+start_x, y, wid, hgt);	
			}
			else
			{
				m_rgDlgItem[m_itemnum++].Inittemp(m_fldlabel[i], m_type[i], 
						fid, x+start_x, y, wid, hgt);
			}
			if ((m_type[i]==1) && (fstruct->input_flds[i].shortcut[0]!='\0'))
/*
......push button
*/
			{
				strcpy_s(tempstr, sizeof(tempstr), fstruct->input_flds[i].shortcut);
				istat = uz_ntparse_frmkey(tempstr, strlen(tempstr), &ktyp, &kkey);
				if (istat != UU_SUCCESS)
				{
					sprintf_s(msg, sizeof(msg), "Key definition syntax error of button. %s",tempstr);
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
						fid, (posx +inpwid-pwid2)+start_x, posy, pwid2, phgt);	
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
//temp yu				uw_ntget_strsize(perc_str, UW_form_fontsize*10, UW_formdlg_font, &pwid2, &phgt);
				if (fstruct->input_flds[i].justified==0)
					m_rgDlgItem[m_itemnum++].Inittemp(perc_str, 4, 
						fid, (posx+inpwid+1)+start_x, posy, pwid2, phgt);	
				else
					m_rgDlgItem[m_itemnum++].Inittemp(perc_str, 4, 
						fid, (posx+start_x-1), posy+hgt, pwid2, phgt);	
				m_idtable[i+m_dspno+m_fldno+m_framno+m_picno] = fid;
			}
		}
	}

	for (i=0; i<fstruct->n_frame_fields; i++)
	{
		fid = IDC_FORMITEM1 + m_itemnum;  
		strcpy_s(m_frame_title[i], sizeof(m_frame_title[i]), fstruct->frame_flds[i].title);
		x = (int)(fstruct->frame_flds[i].x*uw_form_scalex);
		y = (int)(fstruct->frame_flds[i].y*uw_form_scaley);
		wid = (int)(fstruct->frame_flds[i].cx*uw_form_scalex);
		hgt = (int)(fstruct->frame_flds[i].cy*uw_form_scaley);
		m_rgDlgItem[m_itemnum++].Inittemp(m_frame_title[i], 12, 
						fid, x+start_x, y, wid, hgt);
		m_idtable[i+m_fldno+m_dspno] = fid;
	}
	int pic_x, pic_y;
	for (i=0; i<fstruct->n_picture_fields; i++)
	{
		fid = IDC_FORMTEMPPIC + i; 
		x = (int)(fstruct->picture_flds[i].x*uw_form_scalex);
		y = (int)(fstruct->picture_flds[i].y*uw_form_scaley);
		wid = (int)(fstruct->picture_flds[i].cx*uw_form_scalex);
		hgt = (int)(fstruct->picture_flds[i].cy*uw_form_scaley);

		uw_ntget_pixelsize_from_unit(80, "MS Sans Serif", wid, hgt, &pic_x1, &pic_y1);
		uw_ntget_pixelsize_from_unit(UW_form_fontsize*10, UW_formdlg_font, wid, hgt, &pic_x2, &pic_y2);
/*
		float rat = (float)pic_y1/(float)pic_x1;
		float rat2 = (float)pic_y2/(float)pic_x2;

		if (rat<=rat2)
		{
			wid = wid;
			hgt = hgt*(S_pic_ratio_y/S_pic_ratio_x);
		}
		else if (rat>rat2)
		{
			hgt = hgt; 
			wid = hgt*(S_pic_ratio_x/S_pic_ratio_y);
		}
*/	
		double ratiox = (pic_x2*1.0)/(pic_x1*1.0);
		double ratioy = (pic_y2*1.0/pic_y1*1.0);
		if (ratiox>ratioy)
		{
			wid = wid * (ratioy/ratiox);
		}
		else
			hgt = hgt * (ratiox/ratioy);
		m_rgDlgItem[m_itemnum++].Inittemp("", 12, 
						fid, x+start_x, y, wid, hgt);
		m_idtable[i+m_fldno+m_dspno+m_framno] = IDC_FORMITEM1 + m_itemnum;
	}
//	m_dlgTempl.style = WS_SIZEBOX| WS_CHILD|DS_SETFONT | DS_CENTER | DS_CENTERMOUSE ;
	m_dlgTempl.style = WS_CHILD | DS_SETFONT | DS_CENTERMOUSE | WS_TABSTOP;
	m_dlgTempl.dwExtendedStyle = 0;
	m_dlgTempl.cdit = m_itemnum;
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
void CNCLForm::OnSize(UINT nType, int cx, int cy) 
{
	CDialog::OnSize(nType, cx, cy);
	int dx, dy, flag;
	CRect rect, rect1, rect2, rect_b;
	CRect windowRect, windowRect2;
	if (IsIconic()!=0)
	{
		return;
	}
	GetWindowRect(&windowRect);	

	dx = (windowRect.right - windowRect.left) - (m_Rect.right-m_Rect.left);
	dy = (windowRect.bottom - windowRect.top) - (m_Rect.bottom-m_Rect.top);
	if ((dx==0)&&(dy!=0))
		flag = 3;
	else if ((dx!=0)&&(dy==0))
		flag = 1;
	else
		flag = 2;
	if ((m_pScrollView!=NULL)&&(m_init))
	{
		GetDlgItem(IDC_SCROLL_FRAME)->GetWindowRect(&rect);
		rect.bottom += dy;
		rect.right += dx;
		ScreenToClient(rect);

		GetDlgItem(IDC_SCROLL_FRAME)->MoveWindow(&rect);
		if (m_dlgtyp==1)
		{
			rect.left += 2;
			rect.top += 8;
			rect.right -= 2;
			rect.bottom -= 2;
		}
		m_pScrollView->MoveWindow(&rect);				
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
		CRect temp1 (0,0, 22, 22);
		MapDialogRect(&temp1);
		GetClientRect(&windowRect);	
		if (m_dlgtyp==0)
			windowRect.top = windowRect.bottom - temp1.Height() + 4;
		else
			windowRect.top = windowRect.bottom - temp1.Height() + 8;

		windowRect.bottom -= 5;
		windowRect.right -= 5;
		windowRect.left = windowRect.right - butlen;
		CWnd* pChildWnd;
		if (m_fStruct.helpflag==1)
		{
			pChildWnd = (CWnd*)GetDlgItem(IDC_FORMHELP);
			if (pChildWnd!=NULL)
				pChildWnd->MoveWindow(windowRect);
			windowRect.right = windowRect.left - 5;
			windowRect.left = windowRect.right - butlen;
		}
		pChildWnd = (CWnd*)GetDlgItem(IDC_FORMCANCEL);
		if (pChildWnd!=NULL)
		{
			pChildWnd->MoveWindow(windowRect);
			if (m_disptype==1)
				pChildWnd->SetWindowText("CLOSE");
		}
		windowRect.right = windowRect.left - 5;
		windowRect.left = windowRect.right - butlen;
		pChildWnd = (CWnd*)GetDlgItem(IDC_FORMACCEPT);
		if (pChildWnd!=NULL)
		{
			if (m_disptype!=1)
				pChildWnd->MoveWindow(windowRect);
		}
		GetWindowRect(&windowRect);	
		dx = (windowRect.right - windowRect.left) - (m_oldRect.right-m_oldRect.left);
		dy = (windowRect.bottom - windowRect.top) - (m_oldRect.bottom-m_oldRect.top);
		if ((dx<0) || (dy<0))
		{
/*
......set scrolling view size
*/
			CRect rectTemplate;
//only set scrolsize when first displayed in initial dialog
//resize window does not change the scrolling size
//			m_pScrollView->GetWindowRect(rectTemplate);
//			cx1 = rectTemplate.Width();
//			cy1 = rectTemplate.Height();			
//			m_pScrollView->SetScrollSizes(MM_TEXT, CSize(cx1-dx, cy1-dy));
			if (m_dlgtyp==0)
				GetDlgItem(IDC_SCROLL_FRAME)->ShowWindow(SW_HIDE);
			goto done;
		}
		HandleOnSize(dx, dy, flag);
		if (m_dlgtyp==0)
			GetDlgItem(IDC_SCROLL_FRAME)->ShowWindow(SW_HIDE);
	}
done:;
	GetWindowRect(&m_Rect);
	if ((m_pScrollView==NULL)||(m_init==0)) return;
/*
......save position and size on form.pos file
*/
	int x = m_Rect.left;
	int y = m_Rect.top;
	int fcx = m_Rect.Width();
	int fcy = m_Rect.Height();
	int wrect[4];
	if (m_fStruct.ref==1)
/*
......SCREEN
*/
	{
		wrect[0] = 0;
		wrect[1] = 0;
		wrect[2] = GetSystemMetrics(SM_CXSCREEN);
		wrect[3] = GetSystemMetrics(SM_CYSCREEN);
	}
	else
/*
......WINDOW
*/
	{
		NCL_MainFrame->GetWindowRect(&windowRect);
		wrect[0] = windowRect.left;
		wrect[1] = windowRect.top;
		wrect[2] = windowRect.Width();
		wrect[3] = windowRect.Height();
	}
	ud_save_frmpos(m_filename, x, y, fcx, fcy, &wrect[0], m_fStruct.att_win, m_fStruct.ref,0,0);
}
/***********************************************************************
c
c   FUNCTION: HandleOnSize(int dx, int dy, int flag)
c
c          resize view item when view size changed
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLForm::HandleOnSize(int dx, int dy, int flag)
{
	int i, pright, width;
	UINT fid;
	CRect rect;
	CRect windowRect, windowRect2;
	float cx, cy, rat_x, rat_y;

	cx = (float)(m_oldRect.right-m_oldRect.left);
	cy = (float)(m_oldRect.bottom-m_oldRect.top);
	rat_x = (cx+dx)/cx;
	rat_y = (cy+dy)/cy;

	if (m_dlgtyp==0)
	{
		CRect rect_b;
		rect_b = m_oldbox1;
		rect_b.bottom += dy;
		m_pScrollView->GetDlgItem(IDC_DLG_BOX1)->MoveWindow(&rect_b);

		rect_b = m_oldbox2;
		rect_b.bottom += dy;
		rect_b.right += dx;
		m_pScrollView->GetDlgItem(IDC_DLG_BOX2)->MoveWindow(&rect_b);
	}	
/*
......iterate through and change their pos and size.
*/
	CWnd* pChildWnd;
	for (i=0; i<m_framno; i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno];  
		pChildWnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		pChildWnd->GetWindowRect(&windowRect);	
		m_pScrollView->ScreenToClient(windowRect);
/*
......size left
......size right
*/
		if (flag==1)
		{
			windowRect.left = (long)(m_frmrec[i].left*rat_x+.499);
			windowRect.right = (long)( m_frmrec[i].right*rat_x+.499);
		}
/*
......left up or left down or right up or right down 
*/
		else if (flag==2)
		{
			windowRect.left = (long)( m_frmrec[i].left*rat_x+.499);
			windowRect.right =  (long)(m_frmrec[i].right*rat_x+.499);
			windowRect.top =  (long)(m_frmrec[i].top*rat_y+.499);
			windowRect.bottom =  (long)(m_frmrec[i].bottom*rat_y+.499);
		}
/*
......size up
......size down
*/
		else if (flag==3)
		{
			windowRect.top =  (long)(m_frmrec[i].top*rat_y+.499);
			windowRect.bottom =  (long)(m_frmrec[i].bottom*rat_y+.499);
		}
		pChildWnd->MoveWindow(windowRect);
	}
	for (i=0; i<m_dspno; i++)
	{
		fid = m_idtable[i];
		pChildWnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		pChildWnd->GetWindowRect(&windowRect);	
		m_pScrollView->ScreenToClient(windowRect);
/*
......size left
......size right
*/
		if (flag==1)
		{
			windowRect.left =  (long)(m_dsprec[i].left*rat_x+.499);
			windowRect.right =  (long)(m_dsprec[i].right*rat_x+.499);
		}
/*
......left up or left down or right up or right down
*/
		else if (flag==2)
		{
			windowRect.left =  (long)(m_dsprec[i].left*rat_x+.499);
			windowRect.right =  (long)(m_dsprec[i].right*rat_x+.499);
			windowRect.top =  (long)(m_dsprec[i].top*rat_y+0.5);
			windowRect.bottom =  (long)(m_dsprec[i].bottom*rat_y+0.5);
		}
/*
......size up
......size down
*/
		else if (flag==3)
		{
			windowRect.top =  (long)(m_dsprec[i].top*rat_y+.499);
			windowRect.bottom =  (long)(m_dsprec[i].bottom*rat_y+.499);
		}
		pChildWnd->MoveWindow(windowRect);
	}
	for (i=0; i<m_picno; i++)
	{
		fid = m_idtable[i+m_fldno+m_dspno+m_framno];
		pChildWnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		pChildWnd->GetWindowRect(&windowRect);	
		m_pScrollView->ScreenToClient(windowRect);
/*
......size left
......size right
*/
		if (flag==1)
		{
			windowRect.left = (long)(m_picrec[i].left*rat_x+.499);
			windowRect.right = (long)( m_picrec[i].right*rat_x+.499);
		}
/*
......left up or left down or right up or right down 
*/
		else if (flag==2)
		{
			windowRect.left = (long)( m_picrec[i].left*rat_x+.499);
			windowRect.right =  (long)(m_picrec[i].right*rat_x+.499);
			windowRect.top =  (long)(m_picrec[i].top*rat_y+.499);
			windowRect.bottom =  (long)(m_picrec[i].bottom*rat_y+.499);
		}
/*
......size up
......size down
*/
		else if (flag==3)
		{
			windowRect.top =  (long)(m_picrec[i].top*rat_y+.499);
			windowRect.bottom =  (long)(m_picrec[i].bottom*rat_y+.499);
		}
		pChildWnd->MoveWindow(windowRect);
	}

	for (i=0; i<m_fldno; i++)
	{
/*
......need resize prompt/button label first 
*/
		pright = -1;
		if ((m_type[i]==3)||(m_type[i]==2)||(m_type[i]==8)
			||(m_type[i]==9)||(m_type[i]==10) ||(m_type[i]==11)
			||(m_type[i]==13)||(m_type[i]==14) || (m_type[i] == 15)
			||(m_type[i]==16)||(m_type[i]==18))
		{
			fid =  m_idtable[i+m_dspno] - 1;
			pChildWnd = (m_pScrollView->GetDlgItem(fid));
			pChildWnd->GetWindowRect(&windowRect);	
			m_pScrollView->ScreenToClient(windowRect);
/*
......size left
......size right
*/
			width = m_fldlrec[i].right - m_fldlrec[i].left;
			if (flag==1)
			{
				windowRect.left =  (long)(m_fldlrec[i].left*rat_x);
				if (m_fStruct.input_flds[i].ud_prmloc.x==-1)
					windowRect.right = windowRect.left + width;
				else
					windowRect.right =  (long)(m_fldlrec[i].right*rat_x);
			}
/*
......left up or left down or right up or right down
*/
			else if (flag==2)
			{
				windowRect.left =  (long)(m_fldlrec[i].left*rat_x+.499);
				if (m_fStruct.input_flds[i].ud_prmloc.x==-1)
					windowRect.right =  (long)(windowRect.left + width);
				else
					windowRect.right =  (long)(m_fldlrec[i].right*rat_x+.499);
				windowRect.top =  (long)(m_fldlrec[i].top*rat_y+.499);
				windowRect.bottom =  (long)(m_fldlrec[i].bottom*rat_y+.499);
			}
/*
......size up
......size down
*/
			else if (flag==3)
			{
				windowRect.top =  (long)(m_fldlrec[i].top*rat_y+.499);
				windowRect.bottom =  (long)(m_fldlrec[i].bottom*rat_y+.499);
			}
			pright = windowRect.right;
			pChildWnd->MoveWindow(windowRect);
		}
		fid =  m_idtable[i+m_dspno];
		pChildWnd = (CWnd*)(m_pScrollView->GetDlgItem(fid));
		pChildWnd->GetWindowRect(&windowRect);	
		m_pScrollView->ScreenToClient(windowRect);
/*
......size left
......size right
*/
		if (flag==1)
		{
/*
......if the field is attached to the field, default x = y = -1
......don't use ratio but just added +5 to the prompt window
*/
			if ((m_fStruct.input_flds[i].ud_prmloc.x==-1)&&(m_input[i] == FORM_STRING)
				&&(pright!=-1))
				windowRect.left = pright + 5;
			else
				windowRect.left =  (long)(m_fldrec[i].left*rat_x);
			windowRect.right =  (long)(m_fldrec[i].right*rat_x);
		}
/*
......left up or left down or right up or right down
*/
		else if (flag==2)
		{
/*
......if the field is attached to the field, default x = y = -1
......don't use ratio but just added +5 to the prompt window
*/
			if ((m_fStruct.input_flds[i].ud_prmloc.x==-1)&&(m_input[i] == FORM_STRING)
				&&(pright!=-1))
				windowRect.left = pright + 5;
			else
				windowRect.left =  (long)(m_fldrec[i].left*rat_x+.499);
			windowRect.right =  (long)(m_fldrec[i].right*rat_x+.499);
			windowRect.top =  (long)(m_fldrec[i].top*rat_y+.499);
/*
.....dropdownlist is different
*/
			if ((m_type[i]!=2)&&(m_type[i]!=15))
				windowRect.bottom =  (long)(m_fldrec[i].bottom*rat_y+.499);
			else
			{
				((CComboBox*)pChildWnd)->GetDroppedControlRect(&rect);
				windowRect.bottom =  (long)(windowRect.top + (rect.bottom-rect.top));
			}
		}
/*
......size up
......size down
*/
		else if (flag==3)
		{
			windowRect.top =  (long)(m_fldrec[i].top*rat_y+.499);
/*
.....dropdownlist is different
*/
			if ((m_type[i]!=2)&&(m_type[i]!=15))
				windowRect.bottom =  (long)(m_fldrec[i].bottom*rat_y+.499);
			else
			{
				((CComboBox*)pChildWnd)->GetDroppedControlRect(&rect);
				windowRect.bottom = windowRect.top + (rect.bottom-rect.top);
			}
		}
		pChildWnd->MoveWindow(windowRect);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  Create()
c
c   FUNCTION:  This function Create NCL Form
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

BOOL CNCLForm::Create()
{	
	CString TitleCaption = m_title;
	WCHAR szFontName[] = L"MS Sans Serif";

	if (m_disptype==0)
	{
		 return DoModal();
	}
	else
	{
		CDialog::Create(IDD_FORMDIALOG, m_pParent);
		ShowWindow(TRUE);
	}
	return 1;
}
BOOL CNCLForm::PreCreateWindow(CREATESTRUCT& cs)
{
/*
......An OpenGL window must be created with the following flags and must not
......include CS_PARENTDC for the class style. 
*/
	cs.style &= ~WS_THICKFRAME;
	return CDialog::PreCreateWindow(cs);
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

void CNCLForm::FormCancel() 
{
	int frmid;
/*
.....before we close the form, un-highlight the selected geometries
*/
	Form_unhilite_selected();
/*
.....after cancel the form, we need check if there is
.....an active form before this form opened, 
.....if yes, sets back global frm structure to that
.....form's local structure again b
*/
	if (m_savfrm!=NULL)
	{
		frmid = ((CNCLForm* )m_savfrm)->m_frmid;
		UW_active_frm = (CNCLForm* )m_savfrm;
		UW_active_frm->m_frmid = 0;
		UW_display_frm[0] = UW_active_frm;
		UD_dispfrm[0] = UD_dispfrm[frmid];
		UD_dispfdata[0] = UD_dispfdata[frmid];
/*
.....reset value
*/
		UD_dispfrm[frmid] = NULL;
		UD_dispfdata[frmid] = NULL;
	}
	else
	{
		UD_dispfrm[m_frmid] = NULL;
		UW_active_frm = NULL;
	}
	ud_delform(&m_fStruct);
	uu_free((char *)m_fData.ud_data);
/*
.....free those before destroy the dialog because
.....m_helpbox and pocket window could be child of the form
*/
	if (m_helptext != NULL)
	{
		free(m_helptext);
		m_helptext = NULL;
	}
/*
......we need delete pocket window first if have any since it
......display after text help window as active window, so it is
......the childen of help window
*/
	if (m_pocket)	
	{
		um_close_pocket_window(UM_DRAWING_WINDOW);
		um_close_pocket_window(UM_GRAPHIC_WINDOW);
		m_pocket = 0;
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
/*
.....don't call base Oncancel function for modalless dialog
.....which will call because it calls EndDialog, which 
.....will make the dialog box invisible but not destroy it.
*/
	if (m_disptype==1)
	{
/*
.....set the active window back to parent window to avoid pop up disk top window
*/
		if (m_pParent!=NULL)
			m_pParent->SetActiveWindow();
		delete m_pScrollView;
		DestroyWindow ();
	}
	else
	{
		delete m_pScrollView;
		CDialog::OnCancel();
	}
	uw_ntreset_redraw();
}

/***********************************************************************
c
c   SUBROUTINE:  FormClose
c   FUNCTION:  This function close the NCL form
c				it destroy and remove child window and itself
c
c   INPUT:  none
c   OUTPUT: none
c	RETURN: none
c***********************************************************************
*/
void CNCLForm::FormClose()
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
void CNCLForm::addchild(int formID)
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
void CNCLForm::removechild(int formID)
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
c   FUNCTION: OnFormClose()
c
c           callback for close button of form window. Not from system call
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLForm::OnFormClose()
{
	S_close_save = 1;
	OnClose();
	S_close_save = 0;
}

extern "C" void uw_set_curfrm_sav(int flag)
{
	S_close_save = flag;
}

/***********************************************************************
c
c   FUNCTION: OnClose()
c
c           callback for close form window.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLForm::OnClose() 
{
	UD_FSTAT stat;
	int cont, fno, status, redisp;
/*
......if it is displayable type form, we need check and save the data
*/
	if ((m_disptype==1)&&(m_current_fld!=-1)&&(S_close_save))
	{
		status = chk_field(m_current_fld, 1, &redisp, UD_DONE);
/*
.....field check error, just return and not close the form
*/
		if (status==-1)
			return; 
	}
	stat = UD_FLDOK;
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
.....if this is a display type form,
.....the last method callback is "CLOSE FORM" callback
*/
	if (m_disptype==1)
	{
/*
.....We need save the data if close in display type form
*/
		if (S_close_save==0)
			cont = 1;
		else
			cont = NCL_form_SaveData();
		if (cont)
		{
			if (m_method!=NULL && m_method[m_fldno] != NULL)
			{
/*
.....reset pick segment list before callback function
*/
				ncl_reset_pikgeom();
				if (S_close_save==0)
				{
					fno = -1;
					stat = (m_method[m_fldno])(&fno, NULL, UD_DONE);
				}
				else
					stat = (m_method[m_fldno])(&m_frmid, NULL, UD_DONE);
/*
.....have saving problem, do not close form
*/
				if (stat==UD_BADREQ)
				{
					return;
				}
			}
/*
.....before we close the form, un-highlight the selected geometries
*/
			Form_unhilite_selected();
			UD_dispfrm[m_frmid] = NULL;
			UD_dispfdata[m_frmid] = NULL;
			UW_display_frm[m_frmid] = NULL;
			UW_display_frmwin[m_frmid] = NULL;
/*
......free form structure & data space
*/
			ud_delform(&m_fStruct);
			uu_free((char *)m_fData.ud_data);
/*
.....free those before destroy the dialog because
.....m_helpbox and pocket window could be child of the form
*/
			if (m_helptext != NULL)
			{
				free(m_helptext);
				m_helptext = NULL;
			}
			if (m_pocket)	
			{
				um_close_pocket_window(UM_DRAWING_WINDOW);
				um_close_pocket_window(UM_GRAPHIC_WINDOW);
				m_pocket = 0;
			}
			if ((m_helpbox!=NULL)&&(::IsWindow(m_helpbox->m_hWnd)))
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
/*
.....the m_method call could destroy the parent window, thus the form
.....so we need check if it is already destroyed
*/
/*
.....set the active window back to parent window to avoid pop up disk top window
*/
			if ((m_pParent!=NULL)&&(::IsWindow(m_pParent->m_hWnd)))
				m_pParent->SetActiveWindow();
			if (::IsWindow(m_hWnd)==0)
			{
				uw_ntreset_redraw();
				if (current_frmid==m_frmid)
					current_frmid = 0;
				return;
			}
			delete m_pScrollView;
			DestroyWindow();
			uw_ntreset_redraw();
/*
......if current form is this form, default back current_frmid to 0
......the set focus function of form will reset current_frmid later
*/
			if (current_frmid==m_frmid)
				current_frmid = 0;
		}
		else
/*
.....have saving problem, do not close form
*/
		{
			return;
		}
	}
	else
	{ 
		FormCancel();
/*
......if current form is this form, default back current_frmid to 0
......the set focus function of form will reset current_frmid later
*/
		if (current_frmid==m_frmid)
			current_frmid = 0;
		::EnableWindow(NCL_MainFrame->GetSafeHwnd(), TRUE);
		::SetActiveWindow(NCL_MainFrame->GetSafeHwnd());
	}
	if (m_disptype==1)
	{
		delete this;
	}
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

void CNCLForm::PostNcDestroy() 
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
int CNCLForm::FormPick(int fldno)
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
	(m_pScrollView->GetDlgItem(fid))->GetWindowText(lstr, 80);
/*
.....Take down form
*/
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
	::SetActiveWindow(MainWin->GetSafeHwnd());
	ShowWindow(SW_HIDE);
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
		(m_pScrollView->GetDlgItem(fid))->SetWindowText(lstr);
/*
....set the focus to this field also
*/
		(m_pScrollView->GetDlgItem(fid))->SetFocus();
/*
.....need save the accept into the m_fData
*/
		int flag;
		form_ckdata(i, lstr, &flag);			
		if (m_modal[i]==0)
			goto done;
		i++;
		if (!((i<m_fldno) && ((m_input[i]==FORM_PICK) ||
			(m_input[i]==FORM_LOCATE)) || (m_input[i]==FORM_LABEL) ||
			(m_input[i]==FORM_SUBSCR)|| (m_type[i]==23)))
				goto done;
		fid = m_idtable[i+m_dspno];
		strcpy_s(prompt, sizeof(prompt), m_fldlabel[i]);	
/*
.....initialize the string
*/
		(m_pScrollView->GetDlgItem(fid))->GetWindowText(lstr, 80);
/*
....set the focus to this field also
*/
		(m_pScrollView->GetDlgItem(fid))->SetFocus();
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
	ShowWindow(SW_SHOW);
	::EnableWindow(MainWin->GetSafeHwnd(), FALSE);
/*
......need to enable the form again because the form could be child of 
......main window, when disable main window, the form is disable too
*/
	EnableWindow();
	::SetActiveWindow(m_hWnd);
/*
......when form active, the mouse is always arrow, reset saved mouse value
*/
	uw_ntsetcursor(1);

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
int CNCLForm::FormLoc(int fldno)
{
	int i,markval,status;
	char prompt[256], lstr[82];
/*
.....Initialize routine
*/
	i = fldno;
	UINT fid = m_idtable[i+m_dspno];
	strcpy_s(prompt, sizeof(prompt), m_fldlabel[i]);
/*
.....initialize the string
*/
	(m_pScrollView->GetDlgItem(fid))->GetWindowText(lstr, 80);
/*
.....Take down form
*/
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
	::SetActiveWindow(MainWin->GetSafeHwnd());
	ShowWindow(SW_HIDE);
	uw_ntreset_redraw();
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
		(m_pScrollView->GetDlgItem(fid))->SetWindowText(lstr);
/*
....set the focus to this field also
*/
		(m_pScrollView->GetDlgItem(fid))->SetFocus();
/*
.....need save the accept into the m_fData
*/
		int flag;
		form_ckdata(i, lstr, &flag);			
		if (m_modal[i]==0)
			goto done;
		i++;
		if (!((i<m_fldno) && ((m_input[i]==FORM_PICK) ||
			(m_input[i]==FORM_LOCATE)) || (m_input[i]==FORM_LABEL) ||
			(m_input[i]==FORM_SUBSCR) || (m_type[i]==23)))
				goto done;
		fid = m_idtable[i+m_dspno];
		strcpy_s(prompt, sizeof(prompt), m_fldlabel[i]);	
/*
.....initialize the string
*/
		(m_pScrollView->GetDlgItem(fid))->GetWindowText(lstr, 80);
		(m_pScrollView->GetDlgItem(fid))->SetFocus();
	}
/*
.....Bring back the form
*/
done:;
	UD_UNMARK(markval);
	ShowWindow(SW_SHOW);
	::EnableWindow(MainWin->GetSafeHwnd(), FALSE);
/*
......need to enable the form again because the form could be child of 
......main window, when disable main window, the form is disable too
*/
	EnableWindow();
	::SetActiveWindow(m_hWnd);
/*
......when form active, the mouse is always arrow, reset saved mouse value
*/
	uw_ntsetcursor(1);

	return(status);
}

void CNCLForm::Form_unhilite_selected()
{
	UD_SELECT_STR *dselect;
	int i;
	for (i=0;i<m_fStruct.n_input_fields;i++)
	{
		if (m_type[i]==23)
		{
			dselect = (UD_SELECT_STR *)((m_fData.ud_data[i]).ud_delem.frmint);
			if (dselect!=NULL)
				ud_unhilite_sellist(&(dselect->select_list));
		}
	}
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
int CNCLForm::FormSelect(int fldno)
{
	char prompt[256];
	UD_SELECT_STR *dselect;
/*
.....Initialize routine
*/
	int i,j,markval,status,lmtgeo;
	if (LW_nclipv==LW_STANDALONE) return(DE_DONE);
	i = fldno;
	UINT fid = m_idtable[i+m_dspno];
/*
.....Take down form
*/
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
	::SetActiveWindow(MainWin->GetSafeHwnd());
	ShowWindow(SW_HIDE);

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
		strcpy_s(prompt, sizeof(prompt), m_fldlabel[i]);
		dselect = (UD_SELECT_STR *)((m_fData.ud_data[i]).ud_delem.frmint);
		status = ud_get_pick_selstr(prompt, &(dselect->select_list), dselect->color);
		if (UD_form_bypick==0)
			goto done;
		if (status==DE_DONE)
		{
			goto done;
		}
		i++;
		if (m_type[i]!=23)
			goto done;
		fid = m_idtable[i+m_dspno];
/*
....set the focus to this field also
*/
		(m_pScrollView->GetDlgItem(fid))->SetFocus();
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
	ShowWindow(SW_SHOW);
	::EnableWindow(MainWin->GetSafeHwnd(), FALSE);
/*
......need to enable the form again because the form could be child of 
......main window, when disable main window, the form is disable too
*/
	EnableWindow();
	::SetActiveWindow(m_hWnd);
/*
......when form active, the mouse is always arrow, reset saved mouse value
*/
	uw_ntsetcursor(1);
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

void CNCLForm::FormAccept()
{
	int frmid, redisp, stat;

	S_close_save = 1;
	if (m_current_fld!=-1)
	{
		stat = chk_field(m_current_fld, 1, &redisp, UD_DONE);
/*
.....field check error, just return and not close the form
*/
		if (stat==-1)
		{
			S_close_save = 0;
			return; 
		}
	}
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
/*
.....before we close the form, un-highlight the selected geometries
*/
		Form_unhilite_selected();
/*
.....after accept the form, we need check if there is
.....an active form before this form opened, 
.....if yes, sets back global frm structure to that
.....form's local structure again b
*/
		if (m_savfrm!=NULL)
		{
			frmid = ((CNCLForm* )m_savfrm)->m_frmid;
			UW_active_frm = (CNCLForm* )m_savfrm;
			UW_active_frm->m_frmid = 0;
			UW_display_frm[0] = UW_active_frm;
			UD_dispfrm[0] = UD_dispfrm[frmid];
			UD_dispfdata[0] = UD_dispfdata[frmid];
/*
.....reset value
*/
			UD_dispfrm[frmid] = NULL;
			UD_dispfdata[frmid] = NULL;
		}
		else
		{
			UD_dispfrm[m_frmid] = NULL;
			UW_active_frm = NULL;
		}
		ud_delform(&m_fStruct);
		uu_free((char *)m_fData.ud_data);
		if (m_pocket)	
		{
			um_close_pocket_window(UM_DRAWING_WINDOW);
			um_close_pocket_window(UM_GRAPHIC_WINDOW);
			m_pocket = 0;
		}
/*
.....close children window help-box, pocket-win if has it
*/
		if (m_helpbox!=NULL)
		{
			delete m_helpbox;
			m_helpbox = NULL;
		}
		CDialog::OnOK();
		::EnableWindow(NCL_MainFrame->GetSafeHwnd(), TRUE);
		::SetActiveWindow(NCL_MainFrame->GetSafeHwnd());
		uw_ntreset_redraw();
		uw_ntset_context(UM_NCL_WINDOW,UU_TRUE);
	}
	S_close_save = 0;
}
int CNCLForm::Getindx(int page)
{
	return m_pScrollView->Getindx(page);
}

void CNCLForm::GetSecHelpText(char *dsptext)
{
	int i,j,len;
	char *p, *tmp_text, *tmp_text2, *tmp_text3, *pall, *psec;
	char name[80];
	int sec = m_pScrollView->m_selsec;
	if (m_secno!=0)
	{
/*
......get the indx number from the page number
*/
		sec = Getindx(sec);
		sprintf(name, "<%s>", m_fStruct.section_flds[sec].name);
	}
	len = strlen(m_helptext);
	dsptext[0] = '\0';
	if (len==0)
		return;
	tmp_text = new char[len+10000];
	tmp_text2 = new char[len+10000];
	tmp_text3 = new char[len+10000];
	if (m_secno==0)
		strcpy(tmp_text, m_helptext);
	else
	{
		strcpy(tmp_text2, m_helptext);
		pall = strstr(tmp_text2, "<ALL>");
		psec = strstr(tmp_text2, name);
		p = strstr(tmp_text2, "<ALL>");
		if (p==NULL)
		{
			tmp_text[0] = '\0';
		}
		else
		{
			if (p[5]=='\n')
				strcpy(tmp_text2, &(p[6]));
			else
				strcpy(tmp_text2, &(p[5]));
			p = strstr(tmp_text2, "<END_SEC>");
			if (p!=NULL)
				*p = 0;
			strcpy(tmp_text, tmp_text2);
		}
		strcpy(tmp_text2, m_helptext);
		p = strstr(tmp_text2, name);
		if (p==NULL)
		{
			goto next;
		}
		len = strlen(name);
		if (p[len]=='\n')
			strcpy(tmp_text2, &(p[len+1]));
		else
			strcpy(tmp_text2, &(p[len]));
		p = strstr(tmp_text2, "<END_SEC>");
		if (p!=NULL)
			*p = 0;
		if (pall<=psec)
		{
			strcat(tmp_text, "\r\n");
			strcat(tmp_text, tmp_text2);
		}
		else
		{
			strcpy (tmp_text3, tmp_text2);
			strcat (tmp_text3, "\r\n");
			strcat (tmp_text3, tmp_text);
			strcpy(tmp_text, tmp_text3);
		}
	}
next:;
	len = strlen(tmp_text);
	for (i=0,j=0; i<len; i++,j++)
	{
		if (tmp_text[i]=='\n')
			dsptext[j++] = '\r';
		dsptext[j] = tmp_text[i];
	}
	dsptext[j] = '\0';

	delete tmp_text;
	delete tmp_text2;
	delete tmp_text3;
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
void CNCLForm::FormHelp()
{
	UX_pathname dwfile, fdir;
	char *indx;
	int len, status;
	char *dsptext;
/*
......set the help window position
*/
	CRect rect;
	GetWindowRect(&rect);
/*
......always display text help window first since we will position the graphic window
......under the text help window
*/
	if (m_helpbox!=NULL)
	{
		m_helpbox->ShowWindow(SW_SHOW);
/*
......we need update the help text for section
*/
		if (m_secno>0)
			goto disp_text;
		goto graphic_display;
	}
	if (m_helptext==NULL)
		goto graphic_display;

	m_helpbox = new CNCLTextWin (this, 1, m_title);
	m_helpbox->Create(IDD_TEXTWIN, this);
	m_helpbox->ShowWindow(SW_SHOW);

	int x = rect.left;
	int y = rect.top;
	m_helpbox->GetWindowRect(&rect);
	m_helpbox->MoveWindow(x-100, y, rect.Width(), rect.Height());
disp_text:;	
	if (m_helptext!=NULL)
	{
/*
......replace '\n" with '\r\n'
*/
		len = strlen(m_helptext);
		dsptext = new char[len+10000];

		GetSecHelpText(dsptext);
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
	strcpy_s(dwfile, sizeof(dwfile), m_filename);
	indx = strstr(dwfile, ".frm");
	if (indx!=NULL)
		*indx = '\0';
/*
.....don't use NCL_FORMS now, but use UU_USER_SETTINGS/forms directory first, 
.....don't display error message if not load
.....use UD_FORMDIR (which we use for loading form file)
*/
/*
......allow muti-path for directory now
	ul_get_full_dir("UU_USER_SETTINGS",dir);
	ul_build_full_dir(dir,"forms",fdir);
*/
	FILE *fptr = NULL;
	strcat(dwfile, ".dw");
	status = ul_open_mod_file2("UU_USER_SETTINGS", "forms", dwfile, 0,  &fptr, (char *)UU_NULL, (char *)UU_NULL);
	if (status==-1)
		return;
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
c   FUNCTION: OnNcPaint()
c			paint the no-client area
c
c   INPUT:  None.
c
c   OUTPUT :   None.
c   RETURN:    None.
c
**********************************************************************/
void CNCLForm::OnNcPaint() 
{
	Default();
	CWindowDC dc(this);

	CRect rc;
	GetWindowRect(rc);

	rc.bottom = GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);

	CRect closeRect;
	closeRect.top = GetSystemMetrics(SM_CYFRAME) + 1;
	closeRect.bottom = GetSystemMetrics( SM_CYSIZE ) + 1;
	closeRect.left = rc.right - rc.left - closeRect.bottom;
	closeRect.right = rc.right - rc.left - closeRect.top;

	dc.DrawFrameControl(closeRect,
	DFC_CAPTION,
	DFCS_CAPTIONCLOSE |DFCS_TRANSPARENT);

	m_rcClose = closeRect;
	
	CRect minRect;
	minRect.top = closeRect.top;
	minRect.bottom = closeRect.bottom;
	minRect.right = rc.right - rc.left - closeRect.bottom;
	minRect.left = minRect.right - (closeRect.bottom - closeRect.top);


	dc.DrawFrameControl(minRect,
	DFC_CAPTION,
	DFCS_CAPTIONMIN);
	m_rcMin = minRect;
}


/***********************************************************************
c
c   FUNCTION: HitTest( CPoint point )
c
c       Call this function to find out which area the user is click in
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of point need to be tested. 
c
c   OUTPUT :   None
c   RETURN:    One of the mouse hit-test enumerated values listed below. 
c				DHT_CLOSE	in the title-bar 'X' close box area
c				DHT_MIN		in the title-bar '-' minimize box area
c				DHT_CAPTION   In a title-bar area.
c
**********************************************************************/
DWORD CNCLForm::HitTest(CPoint pt)
{
	CRect rect=m_rcClose;
	CRect rect2=m_rcMin;

	if(rect.PtInRect(pt))
		return (DWORD) DHT_CLOSE;
	else if (rect2.PtInRect(pt))
		return (DWORD) DHT_MIN;
	else
		return (DWORD) DHT_CAPTION;
}

/***********************************************************************
c
c   FUNCTION: OnNcLButtonDown(UINT nHitTest, CPoint point)
c
c           The framework calls this member function when the user 
c			push the left mouse button while the cursor is within a nonclient area
c
c   INPUT:  nHitTest: Specifies the hit-test code. A hit test is a test that determines the location of the cursor
c			point: Specifies a CPoint object that contains the x and y screen coordinates of the cursor position
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLForm::OnNcLButtonDown(UINT nHitTest, CPoint point) 
{ 
	CPoint pt=point;
	ScreenToClient(&pt);
	if (IsIconic()==0)
	{
		pt.y += GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);
	}
	pt.x += 5;

	DWORD hitTest = HitTest(pt);

	switch(hitTest)
	{
	case DHT_CLOSE:
			{
		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcClose,
			DFC_CAPTION,
			DFCS_CAPTIONCLOSE | DFCS_PUSHED |DFCS_TRANSPARENT);
			m_LastHit = hitTest;
			m_ButtonDown = hitTest;
			SetCapture();
			}
			break;
	case DHT_MIN:
			{
		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcMin,
			DFC_CAPTION,
			DFCS_CAPTIONMIN | DFCS_PUSHED |DFCS_TRANSPARENT);
			m_LastHit = hitTest;
			m_ButtonDown = hitTest;
			SetCapture();
			}
			break;
	default:
			Default(); 
			break;
	}
} 

/***********************************************************************
c
c   FUNCTION: OnLButtonUp(UINT nFlags, CPoint point)
c			Callback function for mouse button up
c
c   INPUT:  nFlags: Indicates whether various virtual keys are down.
c					MK_CONTROL   Set if the CTRL key is down.
c					MK_MBUTTON   Set if the middle mouse button is down.
c					MK_RBUTTON   Set if the right mouse button is down.
c					MK_SHIFT   Set if the SHIFT key is down.
c			point:  Specifies the x- and y-coordinate of the cursor.
c
c   OUTPUT :   None.
c   RETURN:    None.
c
**********************************************************************/
void CNCLForm::OnLButtonUp(UINT nFlags, CPoint point) 
{
/*
......only if form exist
*/
	if (UD_dispfrm[m_frmid] == NULL) return;


	CWnd *win = GetCapture();
	if ((this != GetCapture()) && (win!=NULL))
		return;
	
	CPoint pt=point;
	if (IsIconic()==0)
	{
		point.y += GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);
	}
	point.x += 5;

	DWORD hitTest = HitTest(point);

	switch(m_ButtonDown)
	{
	case DHT_MIN:
			{
		    CWindowDC dc(this);

			DrawFrameControl(dc.m_hDC,
				m_rcMin,
				DFC_CAPTION,
				DFCS_CAPTIONMIN|DFCS_TRANSPARENT);
			}
			break;
	default:
			break;
	}
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());

	switch(hitTest)	
	{
	case DHT_CLOSE:
/*
.....release the key capture before close
*/
			ReleaseCapture();
//			SendMessage(WM_CLOSE, 0,0);
/*
.....return in this case because this object already being deleted
*/ 
			PostMessage(WM_CLOSE, 0,0);
			return;
	case DHT_MIN:
		if (IsIconic()==0)
		{
			::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
			::SetActiveWindow(MainWin->GetSafeHwnd());
			ShowWindow(SW_MINIMIZE);
			uw_ntreset_redraw();
			if (m_disptype!=1)
				::EnableWindow(MainWin->GetSafeHwnd(), FALSE);
/*
......we have to enable the form icon window though in order to active
......the form icon (this window is disabled when disable main window
......the for is its child
*/
			::EnableWindow(this->GetSafeHwnd(), TRUE);
			::SetActiveWindow(this->GetSafeHwnd());		
		}
		else
			ShowWindow(SW_SHOWNORMAL);
		break;
	default:
			break;
	}
	m_ButtonDown = 0;
	ReleaseCapture();
}

/***********************************************************************
c
c   FUNCTION: OnNcActivate(BOOL bActive)
c			called when active no-client area of the dialog
c
c   INPUT:  bActive: not used here.
c
c   OUTPUT :   None.
c   RETURN:    True
c
**********************************************************************/
BOOL CNCLForm::OnNcActivate(BOOL bActive) 
{
   // If you want different look when inactive change this
   OnNcPaint(); 
   return TRUE; 
}

/***********************************************************************
c
c   FUNCTION: OnNcHitTest( CPoint point )
c
c       The framework calls this member function 
c			for the CWnd object that contains the cursor 
c			every time the mouse is moved.
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always screen coordinates.
c
c   OUTPUT :   None
c   RETURN:    One of the mouse hit-test enumerated values listed below. 
c				DHT_CLOSE	in the title-bar 'X' close box area
c				DHT_MIN		in the title-bar '-' minimize box area
c				HTBORDER   In the border of a window that does not have a sizing border.
c				HTBOTTOM   In the lower horizontal border of the window.
c				HTBOTTOMLEFT   In the lower-left corner of the window border.
c				HTBOTTOMRIGHT   In the lower-right corner of the window border.
c				HTCAPTION   In a title-bar area.
c				HTCLIENT   In a client area.
c				HTERROR   On the screen background or on a dividing line between windows (same as HTNOWHERE except that the DefWndProc Windows function produces a system beep to indicate an error).
c				HTGROWBOX   In a size box.
c				HTHSCROLL   In the horizontal scroll bar.
c				HTLEFT   In the left border of the window.
c				HTMAXBUTTON   In a Maximize button.
c				HTMENU   In a menu area.
c				HTMINBUTTON   In a Minimize button.
c				HTNOWHERE   On the screen background or on a dividing line between windows.
c				HTREDUCE   In a Minimize button.
c				HTRIGHT   In the right border of the window.
c				HTSIZE   In a size box (same as HTGROWBOX).
c				HTSYSMENU   In a Control menu or in a Close button in a child window.
c				HTTOP   In the upper horizontal border of the window.
c				HTTOPLEFT   In the upper-left corner of the window border.
c				HTTOPRIGHT   In the upper-right corner of the window border.
c				HTTRANSPARENT   In a window currently covered by another window.
c				HTVSCROLL   In the vertical scroll bar.
c				HTZOOM   In a Maximize button. 
c
**********************************************************************/
LRESULT CNCLForm::OnNcHitTest(CPoint point) 
{
	CWnd *win = GetCapture();
	if ((this != GetCapture()) && (win!=NULL))
		return CDialog::OnNcHitTest(point);

	CPoint pt=point;
	ScreenToClient(&pt);
	if (IsIconic()==0)
	{
		pt.y += GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);
	}
	pt.x += 5;

	DWORD hitTest = HitTest(pt);

	if(hitTest == m_LastHit)
		return CDialog::OnNcHitTest(point);

	m_LastHit = hitTest;

	UINT pushed = 0;
	if(m_ButtonDown == hitTest)
		pushed = DFCS_PUSHED;

	CWindowDC dc(this);
	switch(hitTest)
	{
	case DHT_CLOSE:
			{
			DrawFrameControl(dc.m_hDC,
				m_rcClose,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE | pushed |DFCS_TRANSPARENT);
			}
			break;
	case DHT_MIN:
			{
			DrawFrameControl(dc.m_hDC,
				m_rcMin,
				DFC_CAPTION,
				DFCS_CAPTIONMIN | pushed |DFCS_TRANSPARENT);
			}
			break;
	default:
			DrawFrameControl(dc.m_hDC,
				m_rcClose,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE|DFCS_TRANSPARENT);
			break;
	}
	return hitTest;
}

/*
int CNCLForm::RunModalLoop(DWORD dwFlags)
{
	ASSERT(::IsWindow(m_hWnd)); // window must be created
	ASSERT(!(m_nFlags & WF_MODALLOOP)); // window must not already be in modal state

	// for tracking the idle time state
	BOOL bIdle = TRUE;
	LONG lIdleCount = 0;
	BOOL bShowIdle = (dwFlags & MLF_SHOWONIDLE) && !(GetStyle() & WS_VISIBLE);
	HWND hWndParent = ::GetParent(m_hWnd);
	m_nFlags |= (WF_MODALLOOP|WF_CONTINUEMODAL);
	MSG *pMsg = AfxGetCurrentMessage();

	// acquire and dispatch messages until the modal state is done
	for (;;)
	{
		ASSERT(ContinueModal());

		// phase1: check to see if we can do idle work
		while (bIdle &&
			!::PeekMessage(pMsg, NULL, NULL, NULL, PM_NOREMOVE))
		{
			ASSERT(ContinueModal());

			// show the dialog when the message queue goes idle
			if (bShowIdle)
			{
				ShowWindow(SW_SHOWNORMAL);
				UpdateWindow();
				bShowIdle = FALSE;
			}
			// call OnIdle while in bIdle state
			if (!(dwFlags & MLF_NOIDLEMSG) && hWndParent != NULL && lIdleCount == 0)
			{
				// send WM_ENTERIDLE to the parent
				::SendMessage(hWndParent, WM_ENTERIDLE, MSGF_DIALOGBOX, (LPARAM)m_hWnd);
			}
			if ((dwFlags & MLF_NOKICKIDLE) ||
				!SendMessage(WM_KICKIDLE, MSGF_DIALOGBOX, lIdleCount++))
			{
				// stop idle processing next time
				bIdle = FALSE;
			}
		}

		// phase2: pump messages while available
		do
		{
			ASSERT(ContinueModal());

			// pump message, but quit on WM_QUIT
			if (!AfxPumpMessage())
			{
				AfxPostQuitMessage(0);
				return -1;
			}

			// show the window when certain special messages rec'd
			if (bShowIdle &&
				(pMsg->message == 0x118 || pMsg->message == WM_SYSKEYDOWN))
			{
				ShowWindow(SW_SHOWNORMAL);
				UpdateWindow();
				bShowIdle = FALSE;
			}

			if (!ContinueModal())
				goto ExitModal;

			// reset "no idle" state after pumping "normal" message
			if (AfxIsIdleMessage(pMsg))
			{
				bIdle = TRUE;
				lIdleCount = 0;
			}

		} while (::PeekMessage(pMsg, NULL, NULL, NULL, PM_NOREMOVE));
	}

ExitModal:
	m_nFlags &= ~(WF_MODALLOOP|WF_CONTINUEMODAL);
	return m_nModalResult;
}

INT_PTR CNCLForm::DoModal()
{
	// can be constructed with a resource template or InitModalIndirect
	ASSERT(m_lpszTemplateName != NULL || m_hDialogTemplate != NULL ||
		m_lpDialogTemplate != NULL);

	// load resource as necessary
	LPCDLGTEMPLATE lpDialogTemplate = m_lpDialogTemplate;
	HGLOBAL hDialogTemplate = m_hDialogTemplate;
	HINSTANCE hInst = AfxGetResourceHandle();
	if (m_lpszTemplateName != NULL)
	{
		hInst = AfxFindResourceHandle(m_lpszTemplateName, RT_DIALOG);
		HRSRC hResource = ::FindResource(hInst, m_lpszTemplateName, RT_DIALOG);
		hDialogTemplate = LoadResource(hInst, hResource);
	}
	if (hDialogTemplate != NULL)
		lpDialogTemplate = (LPCDLGTEMPLATE)LockResource(hDialogTemplate);

	// return -1 in case of failure to load the dialog template resource
	if (lpDialogTemplate == NULL)
		return -1;

	// disable parent (before creating dialog)
	HWND hWndParent = PreModal();
	AfxUnhookWindowCreate();
	BOOL bEnableParent = FALSE;
#ifndef _AFX_NO_OLE_SUPPORT
	CWnd* pMainWnd = NULL;
	BOOL bEnableMainWnd = FALSE;
#endif
	if (hWndParent && hWndParent != ::GetDesktopWindow() && ::IsWindowEnabled(hWndParent))
	{
		::EnableWindow(hWndParent, FALSE);
		bEnableParent = TRUE;
#ifndef _AFX_NO_OLE_SUPPORT
		pMainWnd = AfxGetMainWnd();
		if (pMainWnd && pMainWnd->IsFrameWnd() && pMainWnd->IsWindowEnabled())
		{
			//
			// We are hosted by non-MFC container
			// 
			pMainWnd->EnableWindow(FALSE);
			bEnableMainWnd = TRUE;
		}
#endif
	}

	TRY
	{
		// create modeless dialog
		AfxHookWindowCreate(this);
		if (CreateDlgIndirect(lpDialogTemplate,
						CWnd::FromHandle(hWndParent), hInst))
		{
			if (m_nFlags & WF_CONTINUEMODAL)
			{
				// enter modal loop
				DWORD dwFlags = MLF_SHOWONIDLE;
				if (GetStyle() & DS_NOIDLEMSG)
					dwFlags |= MLF_NOIDLEMSG;
				VERIFY(RunModalLoop(dwFlags) == m_nModalResult);
			}

			// hide the window before enabling the parent, etc.
			if (m_hWnd != NULL)
				SetWindowPos(NULL, 0, 0, 0, 0, SWP_HIDEWINDOW|
					SWP_NOSIZE|SWP_NOMOVE|SWP_NOACTIVATE|SWP_NOZORDER);
		}
	}
	CATCH_ALL(e)
	{
//		DELETE_EXCEPTION(e);
		m_nModalResult = -1;
	}
	END_CATCH_ALL

#ifndef _AFX_NO_OLE_SUPPORT
	if (bEnableMainWnd)
		pMainWnd->EnableWindow(TRUE);
#endif
	if (bEnableParent)
		::EnableWindow(hWndParent, TRUE);
	if (hWndParent != NULL && ::GetActiveWindow() == m_hWnd)
		::SetActiveWindow(hWndParent);

	// destroy modal window
	DestroyWindow();
	PostModal();

	// unlock/free resources as necessary
	if (m_lpszTemplateName != NULL || m_hDialogTemplate != NULL)
		UnlockResource(hDialogTemplate);
	if (m_lpszTemplateName != NULL)
		FreeResource(hDialogTemplate);

	return m_nModalResult;
}
**/
/**********************************************************************
**    E_FUNCTION :  uw_ntform_invis()
**       Invisibles the active form.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntform_invis()
{
/*
.....Take down form
*/
/*
.....make it same as UNIX
*/
//	UW_active_frm->FormInvis();
	if (UW_display_frm[0]!=NULL)
		UW_display_frm[0]->FormInvis();
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
void CNCLForm::FormInvis()
{
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
	::SetActiveWindow(MainWin->GetSafeHwnd());

	m_show = 0;
	ShowWindow(SW_HIDE);
	uw_ntreset_redraw();

	if (Pocket_Win[UM_IPV_WINDOW] != NULL)
		::EnableWindow((Pocket_Win[UM_IPV_WINDOW])->GetSafeHwnd(), TRUE);
}
/**********************************************************************
**    E_FUNCTION :  uw_ntform_vis()
**       Visible the form.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntform_vis()
{
/*
.....make it same as UNIX
*/
	if (UW_display_frm[0]!=NULL)
		UW_display_frm[0]->FormVis();
}

/**********************************************************************
**    E_FUNCTION :  FormVis()
**       Visible the form.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void CNCLForm::FormVis()
{
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());

	m_show = 1;
	ShowWindow(SW_SHOW);
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
	int frmid;
	if (m_childnum!=0)
	{
		for (int i=m_childnum-1; i>=0;i--)
		{
			frmid = m_childfrm[i];
			if (UW_display_frmwin[frmid]!=NULL)
			{
				if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
					(NCL_MainFrame->m_formbar[frmid])->EnableWindow();
				else
					UW_display_frm[frmid]->EnableWindow();
			}
		}
	}
/*
......when form active, the mouse is always arrow, reset saved mouse value
*/
	uw_ntsetcursor(1);
}

/**********************************************************************
**    E_FUNCTION :  int uw_ntform_display(fstruct,fdata)
**       Displays a Window dialog Style Form and not waits for user input.
**    PARAMETERS   
**       INPUT  : 
**          fstruct:	Form structure.
**			fdata		Default form data.
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS on success.
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntform_display(UD_FSTRUCT *fstruct, UD_FDATA *fdata)
{
	return uw_ntform1(fstruct, fdata, 1);
}	
/**********************************************************************
**    E_FUNCTION :  int uw_ntform1(fstruct,fdata, flag)
**       Displays a Window dialog Style Form and not waits for user input.
**    PARAMETERS   
**       INPUT  : 
**          fstruct:	Form structure.
**			fdata		Default form data.
**			flag: 1: display type form
**					0: "Input" type form
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS on success.
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntform1(UD_FSTRUCT *fstruct, UD_FDATA *fdata, int dflag)
{
	
	int i,helplen, status, stat;	
	int frmid_sav;
	CWnd *Form_parent;
	int sav_cursor = uw_ntgetcur_cursor();
	
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
/*
.....we need remember this window because it will because 
.....the new form's parent window (in the database), when we 
.....remove the parent window (in the database), we need close
.....this window too. But we may set the form's window parent as 
.....we decide later in the code
*/
	Form_parent = MainWin->GetActiveWindow();
/*
.....If focus is transfered to another window other that an NCL form,then
.....Form_parent is NULL. In such a case set the parent window to the Main
..... NCL window or the IPV pocket window. 
*/
	if (Form_parent != NULL)
		MainWin = MainWin->GetActiveWindow();

/*
.....if ipv pocket window is active, sets ipv pocket 
......window as parent window
*/
	if (Pocket_Win[UM_IPV_WINDOW] != NULL)
		MainWin = Pocket_Win[UM_IPV_WINDOW];
	if (Form_parent == NULL)
		Form_parent = MainWin;
/*
......we will have a problem if the parent window is iconed
......so we need show this parent window first to create form
*/
	if (NCL_MainFrame->IsIconic()!=0)
	{
		NCL_MainFrame->ShowWindow(SW_SHOWNORMAL);
		NCL_MainFrame->UpdateWindow();
		if (Pocket_Win[UM_IPV_WINDOW] != NULL)
		{
			Pocket_Win[UM_IPV_WINDOW]->ShowWindow(SW_SHOWNORMAL);
			Pocket_Win[UM_IPV_WINDOW]->UpdateWindow();
		}
	}
/*
......record form name if record on
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
		ud_rpwrform( "NAME", (*fstruct).frmname, NULL);
	{
		if (dflag==0)
			ud_rpwrform( "NAME", (*fstruct).frmname, "0");
		else
			ud_rpwrform( "NAME", (*fstruct).frmname, "1");
	}

	if(UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)
	{ 
		status = ud_rdform(fstruct, fdata, fstruct, fdata);
		if (status==0)
		{	
			return 0;
		}
		else if (status==1)
			goto form;
		else if (status==-1)
			return  -1;
	}
form:
	if (dflag==0)
	{
/*
.....save the global frm structure for active form
.....because we will overwrite it when we display
.....this form, after this form closed, we need set
.....the saved data back to global frm structure
*/
/*
.....in this case, we must see if there are input type form
.....opened, if yes, then save all information of this form
.....into 50+n form_structure, and the new one become the active form 0
*/
		if (UW_active_frm != NULL)
		{
/*
.....find available input type id
*/
			for (i=50; i<60; i++)
			{
				if (UD_dispfrm[i]==NULL)
					break;
			}
			if (i>=60)
				return NULL;
/*
.....save all UW_active_frm into m_savfrm
*/
			UD_dispfrm[i] = &(UW_display_frm[0]->m_fStruct);
			UD_dispfdata[i] = &(UW_display_frm[0]->m_fData);
			UW_display_frm[0] = new CNCLForm(MainWin, 0);
			UW_display_frmwin[0] = UW_display_frm[0];
			UW_active_frm->m_frmid = i;
			UW_display_frm[0]->m_savfrm = (int *) UW_active_frm;
		}
		else
		{
			UW_display_frm[0] = new CNCLForm(MainWin, 0);
			UW_display_frmwin[0] = UW_display_frm[0];
		}
		current_frmid = 0;
	}
	else
	{
/*
......search for available form
*/
		for (i=1; i<50; i++)
		{
			if (UW_display_frmwin[i]==NULL)
				break;
		}
		current_frmid = i;
		if (current_frmid>=50)
			return NULL;
		if (((fstruct->dockable[0]==0) 
			&& (fstruct->dockable[1]==0)
			&& (fstruct->dockable[2]==0)
			&& (fstruct->dockable[3]==0)) ||  (dflag==0))
		{
			UW_display_frm[current_frmid] = new CNCLForm(MainWin, 1);
			UW_display_frmwin[current_frmid] = UW_display_frm[current_frmid];
		}
		else
		{
			NCL_MainFrame->m_formbar[current_frmid] = new CNCLFormBar(NCL_MainFrame, 1);
			UW_display_frmwin[current_frmid] = NCL_MainFrame->m_formbar[current_frmid];
		}
	}
/*
.....we need to remember the parent window, if it is a form
.....we need add this to parent form's children list
*/
	if (((fstruct->dockable[0]!=0) 
			|| (fstruct->dockable[1]!=0)
			|| (fstruct->dockable[2]!=0)
			|| (fstruct->dockable[3]!=0)) && (dflag))

		NCL_MainFrame->m_formbar[current_frmid]->m_parentID = -1;
	else
		UW_display_frm[current_frmid]->m_parentID = -1;
	for (i=0; i<60; i++)
	{
		if (UW_display_frmwin[i]==Form_parent)
		{
			if (((fstruct->dockable[0]!=0) 
				|| (fstruct->dockable[1]!=0)
				|| (fstruct->dockable[2]!=0)
				|| (fstruct->dockable[3]!=0)) && (dflag))
				NCL_MainFrame->m_formbar[current_frmid]->m_parentID = i;
			else
				UW_display_frm[current_frmid]->m_parentID = i;
			break;
		}
		else if ((Form_parent==Pocket_Win[UM_IPV_WINDOW]) 
				&& (current_frmid>0) && (UW_display_frmwin[0] != NULL))
		{
/*
.....there could be no parent form (main form) display, 
.....why assign 0 which is main form?
.....it will cause problem in following line
		UW_display_frm[UW_display_frm[current_frmid]->m_parentID]->addchild(current_frmid);
.....
*/
			if (((fstruct->dockable[0]!=0) 
				|| (fstruct->dockable[1]!=0)
				|| (fstruct->dockable[2]!=0)
				|| (fstruct->dockable[3]!=0)) && (dflag))
				NCL_MainFrame->m_formbar[current_frmid]->m_parentID = 0;
			else
				UW_display_frm[current_frmid]->m_parentID = 0;
			break;
		}
	}
/*
.....added this form to parent form's children list
*/
	if (((fstruct->dockable[0]!=0) 
			|| (fstruct->dockable[1]!=0)
			|| (fstruct->dockable[2]!=0)
			|| (fstruct->dockable[3]!=0)) && (dflag))
	{
		if (NCL_MainFrame->m_formbar[current_frmid]->m_parentID!=-1)
		{
			if (NCL_MainFrame->m_formbar[NCL_MainFrame->m_formbar[current_frmid]->m_parentID]!=NULL)
				NCL_MainFrame->m_formbar[NCL_MainFrame->m_formbar[current_frmid]->m_parentID]->addchild(current_frmid);
			else
				UW_display_frm[NCL_MainFrame->m_formbar[current_frmid]->m_parentID]->addchild(current_frmid);
		}
		strcpy_s(NCL_MainFrame->m_formbar[current_frmid]->m_filename, UX_MAX_PATH_LEN, fstruct->frmname);
		if (fstruct->frmhelp!=NULL)
		{
			helplen = strlen (fstruct->frmhelp);
			NCL_MainFrame->m_formbar[current_frmid]->m_helptext = (char* )malloc ((helplen+1)*sizeof (char));
			strcpy_s(NCL_MainFrame->m_formbar[current_frmid]->m_helptext, helplen+1, fstruct->frmhelp);
		}
		NCL_MainFrame->m_formbar[current_frmid]->initform(fstruct, fdata);
/*
.....copy structure to local class data member
*/	
/*
.....We use uu_move_byte to copy structure (include the pointer), so we don't
.....call ud_delform to free the pointer space after this routine now, but we
.....need free there space after we close the form
*/
		uu_move_byte((char*)fstruct, (char*)&(NCL_MainFrame->m_formbar[current_frmid]->m_fStruct),
									sizeof(UD_FSTRUCT));
		uu_move_byte((char*)fdata, (char*)&(NCL_MainFrame->m_formbar[current_frmid]->m_fData),
									sizeof(UD_FDATA));

		UD_dispfrm[current_frmid] = &(NCL_MainFrame->m_formbar[current_frmid]->m_fStruct);
		UD_dispfdata[current_frmid] = &(NCL_MainFrame->m_formbar[current_frmid]->m_fData);

		for (i=0; i<fstruct->n_input_fields; i++)
		{
			NCL_MainFrame->m_formbar[current_frmid]->m_method[i] = fstruct->input_flds[i].method;
			NCL_MainFrame->m_formbar[current_frmid]->m_method_ret[i] = fstruct->input_flds[i].method_returns;
			NCL_MainFrame->m_formbar[current_frmid]->m_trav_mask[i] = fstruct->traverse_mask[i];
		}
		NCL_MainFrame->m_formbar[current_frmid]->m_method[i] = fstruct->input_flds[i].method;
		NCL_MainFrame->m_formbar[current_frmid]->m_method_ret[i] = fstruct->input_flds[i].method_returns;
		for(i = 0; i<fstruct->n_display_fields+fstruct->n_input_fields; i++) 
		{
			NCL_MainFrame->m_formbar[current_frmid]->m_disp_mask[i] = fstruct->ud_display_mask[i];
		}
		if (current_frmid!=0)
			NCL_MainFrame->m_formbar[current_frmid]->m_frmid = current_frmid;
	}
	else
	{
		if (UW_display_frm[current_frmid]->m_parentID!=-1)
		{
			if (NCL_MainFrame->m_formbar[UW_display_frm[current_frmid]->m_parentID]!=NULL)
				NCL_MainFrame->m_formbar[UW_display_frm[current_frmid]->m_parentID]->addchild(current_frmid);
			else
				UW_display_frm[UW_display_frm[current_frmid]->m_parentID]->addchild(current_frmid);
		}
		strcpy_s(UW_display_frm[current_frmid]->m_filename, UX_MAX_PATH_LEN, fstruct->frmname);
		if (fstruct->frmhelp!=NULL)
		{
			helplen = strlen (fstruct->frmhelp);
			UW_display_frm[current_frmid]->m_helptext = (char* )malloc ((helplen+1)*sizeof (char));
			strcpy_s(UW_display_frm[current_frmid]->m_helptext, helplen+1, fstruct->frmhelp);
		}
		UW_display_frm[current_frmid]->initform(fstruct, fdata);
		if (dflag==0)
			UW_active_frm = UW_display_frm[current_frmid];
/*
.....copy structure to local class data member
*/	
/*
.....We use uu_move_byte to copy structure (include the pointer), so we don't
.....call ud_delform to free the pointer space after this routine now, but we
.....need free there space after we close the form
*/
		uu_move_byte((char*)fstruct, (char*)&(UW_display_frm[current_frmid]->m_fStruct),
									sizeof(UD_FSTRUCT));
		uu_move_byte((char*)fdata, (char*)&(UW_display_frm[current_frmid]->m_fData),
									sizeof(UD_FDATA));

		UD_dispfrm[current_frmid] = &(UW_display_frm[current_frmid]->m_fStruct);
		UD_dispfdata[current_frmid] = &(UW_display_frm[current_frmid]->m_fData);

		for (i=0; i<fstruct->n_input_fields; i++)
		{
			UW_display_frm[current_frmid]->m_method[i] = fstruct->input_flds[i].method;
			UW_display_frm[current_frmid]->m_method_ret[i] = fstruct->input_flds[i].method_returns;
			UW_display_frm[current_frmid]->m_trav_mask[i] = fstruct->traverse_mask[i];
		}
		if (dflag==1)
		{
			UW_display_frm[current_frmid]->m_method[i] = fstruct->input_flds[i].method;
			UW_display_frm[current_frmid]->m_method_ret[i] = fstruct->input_flds[i].method_returns;
		}
		for(i = 0; i<fstruct->n_display_fields+fstruct->n_input_fields; i++) 
		{
			UW_display_frm[current_frmid]->m_disp_mask[i] = fstruct->ud_display_mask[i];
		}
		if (current_frmid!=0)
			UW_display_frm[current_frmid]->m_frmid = current_frmid;
	}
/*
.....Protect against display forms
.....opened from main form and
.....changing current_frmid
*/
	uw_setform_active(1);
	frmid_sav = current_frmid;
	if ((MainWin!=NCL_MainFrame)&&(dflag==0))
	{
		::EnableWindow(NCL_MainFrame->GetSafeHwnd(), FALSE);
	}
	if (((fstruct->dockable[0]==0) 
			&& (fstruct->dockable[1]==0)
			&& (fstruct->dockable[2]==0)
			&& (fstruct->dockable[3]==0)) ||  (dflag==0))
	{
		if (fstruct->n_section_fields==0)
		{
			UW_display_frm[current_frmid]->setdlgtyp(1);
		}
		else
		{
			UW_display_frm[current_frmid]->setdlgtyp(0);
		}
		UW_display_frm[current_frmid]->setsecbut(fstruct->n_section_fields);
		stat = UW_display_frm[current_frmid]->Create();
	}
	else
		stat = NCL_MainFrame->m_formbar[current_frmid]->CreateFormBar();
	current_frmid = frmid_sav;

	if (dflag==0)
	{
		uu_move_byte((char*)&(UW_display_frm[current_frmid]->m_fStruct), 
								(char*)fstruct, 
								sizeof(UD_FSTRUCT));
		uu_move_byte((char*)&(UW_display_frm[current_frmid]->m_fData), 
								(char*)fdata, 
								sizeof(UD_FDATA));
		if (UW_active_frm!=UW_display_frm[current_frmid])
		{
			delete UW_display_frm[current_frmid];
			UW_display_frm[current_frmid] = NULL;
			UW_display_frmwin[current_frmid] = NULL;
		}
	}
/*
...added for recording
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
	{
		if (dflag==1)
			ud_rpwrform("DONE","1",NULL);
		else
			ud_rpwrform("DONE","0",NULL);
	}
	uw_ntsetcursor(sav_cursor);
	uw_setform_active(0);
	if (dflag==0)
	{
		if (stat==IDOK)
			return 0;
		else
			return -1;
	}
	else if (stat!=-1)
		return current_frmid;
	else
		return -1;
}


/**********************************************************************
**    E_FUNCTION :  int uw_ntclose_dispfrm(frmid)
**       close a Window dialog Style Form
**    PARAMETERS   
**       INPUT  : 
**          frmid:	Form ID.
**       OUTPUT :  
**          none
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntclose_dispfrm(int frmid)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->FormClose();
		else
			UW_display_frm[frmid]->FormClose();
	}
	return;
}

/**********************************************************************
**    E_FUNCTION :  uw_ntget_frmfield(int frmid, int fieldno, UD_DDATA data,
**                     str_flag)
**       Get a field data
**    PARAMETERS   
**       INPUT  : 
**          frmid     : Form ID.
**          fieldno   : field number
**          str_flag  : UU_TRUE if a character string is to be returned
**                      with each field type.
**       OUTPUT :
**          data: field data
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntget_frmfield(int frmid, int fieldno, UD_DDATA data,
	UU_LOGICAL str_flag)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->Get_field_data(fieldno,data,str_flag);
		else
			UW_display_frm[frmid]->Get_field_data(fieldno, data,str_flag);
	}
}

/**********************************************************************
**    E_FUNCTION :  uw_ntfrm_setlist(int frmid, int fieldno, UD_LIST *form_list)
**       Update a list field with new data
**    PARAMETERS   
**       INPUT  : 
**          frmid:	Form ID.
**			fieldno: field number
**			form_list: new list data
**       OUTPUT :  
**          none
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntfrm_setlist(int frmid, int fieldno, UD_LIST *form_list)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->set_list(fieldno, form_list);
		else
			UW_display_frm[frmid]->set_list(fieldno, form_list);
	}
	return;
}
/**********************************************************************
**    E_FUNCTION :  uw_ntupdate_form(int frmid)
**       Update a form display
**    PARAMETERS   
**       INPUT  : 
**          frmid:	Form ID.
**       OUTPUT :  
**          none
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntupdate_form(int frmid)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->uw_ntform_redisplay();
		else
			UW_display_frm[frmid]->uw_ntform_redisplay();
	}
	uw_nt_flush_win();
	return;
}
/*********************************************************************
**    E_FUNCTION : uw_nt_flush_form(int frmid)
**      
**    DESCRIPTION:
**        Flush form display.
**
**    PARAMETERS   
**       INPUT  : 
**          frmid: form ID
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

extern "C" int uw_nt_flush_win()
{      
	MSG msg;

	while (::PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE))
	{
		if (!GetMessage(&msg, NULL, 0, 0))
			return -1;
		if (msg.message != WM_KICKIDLE && !NCL_MainFrame->NCLPreTranslateMessage(&msg))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	return 0 ;
}
/**********************************************************************
**    E_FUNCTION :  uw_ntdspfrm_invis()
**       Invisibles the display form.
**    PARAMETERS   
**       INPUT  :
**         frmid: form ID
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntdspfrm_invis(int frmid)
{
	if (frmid<0) return;
/*
.....Take down form
*/
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->FormInvis();
		else
			UW_display_frm[frmid]->FormInvis();
	}
}

/**********************************************************************
**    E_FUNCTION :  uw_ntdspfrm_vis(frmid)
**       Redisplays the display form.
**    PARAMETERS   
**       INPUT  :
**          frmid: form ID
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntdspfrm_vis(int frmid)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->FormVis();
		else
			UW_display_frm[frmid]->FormVis();
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
void CNCLForm::OnFormTextHighlight()
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
		cedt = (CEdit*)(m_pScrollView->GetDlgItem(fid));
/*
......the form could have no focus, so txtwin could be NULL
......it will cause a segmentation error sometimes
......Yurong
*/
		if ((txtwin==NULL)||(cedt==NULL))
			continue;
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
void CNCLForm::OnFormTabbed()
{
	int i,k, count;
	CWnd *PwndNext=NULL, *tmpwin, *cwin, *dwin;
	CWnd *first=NULL, *last=NULL;
	int first_item;
	UINT did = 0, def_id = 0;
	int def_chg = 1;
	CWnd *old_def, *new_def;
	POINT pt; 

	for (i=0; i<m_fldno; i++)
	{
		tmpwin = m_pScrollView->GetDlgItem(m_idtable[i+m_dspno]);
		if (tmpwin->IsWindowEnabled()) 
		{
            pt.x = 10; 
            pt.y = 10; 
			if ((m_type[i]!=17)&&(m_type[i]!=19))
				tmpwin = tmpwin->ChildWindowFromPoint(pt); 
			if (first==NULL)
			{
				first = tmpwin;
				first_item = i;
			}
			last = tmpwin;
		}
	}
	CWnd *win = GetFocus();
	if (win==GetDlgItem(IDC_FORMHELP))
	{
		PwndNext = first;
		if (m_type[first_item] == 1)
			def_id = m_idtable[first_item+m_dspno];
		did = IDC_FORMHELP;
	}
	else if (win==GetDlgItem(IDC_FORMCANCEL))
	{
		did = IDC_FORMCANCEL;
		PwndNext = GetDlgItem(IDC_FORMHELP);
		if ((PwndNext->IsWindowEnabled()==0) || (PwndNext->IsWindowVisible()==0))
		{
			PwndNext = first;
			if (m_type[first_item] == 1)
				def_id = m_idtable[first_item+m_dspno]; 
		}
		else
			def_chg = 0;
	}
	else if (win==GetDlgItem(IDC_FORMACCEPT))
	{
		did = IDC_FORMACCEPT;
		PwndNext = GetDlgItem(IDC_FORMCANCEL);
		def_chg = 0;
	}
	else
	{
/*
.....item from m_pScrollView
*/
		win = m_pScrollView->GetFocus();
		count = 0;
get_next:;
		if (count > 500)
		{
			PwndNext = NULL;
			return;
		}
		if (win==last)
		{
			if (m_disptype==0)
				PwndNext = (CWnd*)GetDlgItem(IDC_FORMACCEPT);
			else
				PwndNext = GetDlgItem(IDC_FORMCANCEL);
		}
		else
		{
/*
......this GetNextDlgTabItem function is not working, that's why we need handle the tab
......by ourselvies
*/
//			PwndNext = m_pScrollView->GetNextDlgTabItem(win);
			for (k=0; k<m_fldno; k++)
			{
				cwin = m_pScrollView->GetDlgItem(m_idtable[k+m_dspno]);
				pt.x = 10; 
				pt.y = 10; 
				dwin = cwin->ChildWindowFromPoint(pt);
				if ((win==cwin) || (win==dwin))
				{
					PwndNext = m_pScrollView->GetDlgItem(m_idtable[k+m_dspno+1]);
					if (PwndNext->IsWindowEnabled()) 
					{
						if (m_type[k+1] == 1)
							def_id = m_idtable[k+m_dspno+1];
						break;
					}
					else
					{
						if ((m_type[i]!=17)&&(m_type[i]!=19))
							win = PwndNext->ChildWindowFromPoint(pt);
						else
							win = PwndNext;
						count++;
						goto get_next;
					}
				}
			}
		}
	}
	if (PwndNext)
	{
		m_forward = 1;
		if (def_id!=0)
		{
/*
.....sice def_id is a ID of the dialogview, not dialog itsself
.....the SetDefID(def_id) will not working because the SetDefID(def_id)
.....send a WM_COMMAND message, but it will cause a segmentation
.....error because it's view message, should send WM_VIEWCOMMAND,
.....so we will set defID to 
.....IDF_FORM_RETURN and in IDF_FORM_RETURN callback function,
.....sent view command message for def_id
*/
//			SetDefID(def_id);
			did = m_def_id;
			m_def_id = def_id;
			old_def = GetDlgItem(did);
			SendMessage(DM_SETDEFID, (WPARAM)IDF_FORM_RETURN);
			m_pScrollView->SendMessage(WM_NEXTDLGCTL, (WPARAM)PwndNext->GetSafeHwnd(), TRUE);
			if (old_def!=NULL)
				old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
			new_def = GetDlgItem(m_def_id);
			if (new_def!=NULL)
				new_def->SendMessage(BM_SETSTYLE, BS_DEFPUSHBUTTON, TRUE);
		}
		else if (def_chg==1)
		{
/*
......if it is not a dialog button active, reset to Accept/Close button
......as default button
*/
			m_pScrollView->SendMessage(WM_NEXTDLGCTL, (WPARAM)PwndNext->GetSafeHwnd(), TRUE);
			old_def = GetDlgItem(did);
			if (m_disptype==1)
			{
				if ((did!=IDC_FORMCANCEL)&&(old_def!=NULL))
					old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
				SendMessage(DM_SETDEFID, (WPARAM)IDC_FORMCANCEL);
				m_def_id = IDC_FORMCANCEL;
			}
			else
			{
				if ((did!=IDC_FORMACCEPT)&&(old_def!=NULL))
					old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
				SendMessage(DM_SETDEFID, (WPARAM)IDC_FORMACCEPT);
				m_def_id = IDC_FORMACCEPT;
			}
			new_def = GetDlgItem(m_def_id);
			if (new_def!=NULL)
				new_def->SendMessage(BM_SETSTYLE, BS_DEFPUSHBUTTON, TRUE);
		}
		else
		{
			SendMessage(WM_NEXTDLGCTL, (WPARAM)PwndNext->GetSafeHwnd(), TRUE);
		}
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
void CNCLForm::OnFormSTabbed()
{
	int i,k, count;
	CWnd *PwndPr, *tmpwin, *cwin, *dwin;
	CWnd *first=NULL, *last=NULL;
	int last_item;
	int def_chg = 1;
	UINT did = 0, def_id = 0;
	CWnd *old_def, *new_def;
	POINT pt; 

	for (i=0; i<m_fldno; i++)
	{
		tmpwin = m_pScrollView->GetDlgItem(m_idtable[i+m_dspno]);
		if (tmpwin->IsWindowEnabled()) 
		{
            pt.x = 10; 
            pt.y = 10; 
			if ((m_type[i]!=17)&&(m_type[i]!=19))
				tmpwin = tmpwin->ChildWindowFromPoint(pt); 
			if (first==NULL)
				first = tmpwin;
			last = tmpwin;
			last_item = i;
		}
	}

	CWnd *win = GetFocus();
	count = 0;
get_next:;
	if (count > 500)
	{
		PwndPr = NULL;
		return;
	}
	if (win==GetDlgItem(IDC_FORMACCEPT))
	{
		did = IDC_FORMACCEPT;
		PwndPr = last;
		if (m_type[last_item] == 1)
			def_id = m_idtable[last_item+m_dspno]; 
	}
	else if (win==GetDlgItem(IDC_FORMCANCEL))
	{
		did = IDC_FORMCANCEL;
		if (m_disptype==0)
		{
			PwndPr = GetDlgItem(IDC_FORMACCEPT);
			def_chg = 0;
		}
		else
		{
			PwndPr = last;
			if (m_type[last_item] == 1)
				def_id = m_idtable[last_item+m_dspno]; 
		}
	}
	else if (win==GetDlgItem(IDC_FORMHELP))
	{
		did = IDC_FORMHELP;
		PwndPr = GetDlgItem(IDC_FORMCANCEL);
		def_chg = 0;
	}
	else if (win==first)
	{
		PwndPr = (CWnd*)GetDlgItem(IDC_FORMHELP);
		if ((PwndPr->IsWindowEnabled()==0) || (PwndPr->IsWindowVisible()==0))
		{
			PwndPr = GetDlgItem(IDC_FORMCANCEL);
			def_id = IDC_FORMCANCEL;
		}
		else
			def_id = IDC_FORMHELP;
	}
	else
	{
//		PwndPr = m_pScrollView->GetNextDlgTabItem(GetFocus(), TRUE);
		for (k=0; k<m_fldno; k++)
		{
			cwin = m_pScrollView->GetDlgItem(m_idtable[k+m_dspno]);
            pt.x = 10; 
            pt.y = 10; 
			dwin = cwin->ChildWindowFromPoint(pt);
			if ((win==cwin) || (win==dwin))
			{
				PwndPr = m_pScrollView->GetDlgItem(m_idtable[k+m_dspno-1]);
				if (PwndPr->IsWindowEnabled()) 
				{
					if (m_type[k+1] == 1)
						def_id = m_idtable[k+m_dspno-1];
					break;
				}
				else
				{
					if ((m_type[i]!=17)&&(m_type[i]!=19))
						win = PwndPr->ChildWindowFromPoint(pt);
					else
						win = PwndPr;
					count++;
					goto get_next;
				}
			} 
		}
	}
	if (PwndPr)
	{
		m_forward = 0;
		if (def_id!=0)
		{
/*
.....sice def_id is a ID of the dialogview, not dialog itsself
.....the SetDefID(def_id) will not working because the SetDefID(def_id)
.....send a WM_COMMAND message, but it will cause a segmentation
.....error because it's view message, should send WM_VIEWCOMMAND,
.....so we will set defID to 
.....IDF_FORM_RETURN and in IDF_FORM_RETURN callback function,
.....sent view command message for def_id
*/
//			SetDefID(def_id);
			did = m_def_id;
			m_def_id = def_id;
			old_def = GetDlgItem(did);
			SendMessage(DM_SETDEFID, (WPARAM)IDF_FORM_RETURN);
			m_pScrollView->SendMessage(WM_NEXTDLGCTL, (WPARAM)PwndPr->GetSafeHwnd(), TRUE);
			if (old_def!=NULL)
				old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
			new_def = GetDlgItem(m_def_id);
			if (new_def!=NULL)
				new_def->SendMessage(BM_SETSTYLE, BS_DEFPUSHBUTTON, TRUE);
		}
		else if (def_chg==1)
		{
/*
......if it is not a dialog button active, reset to Accept/Close button
......as default button
*/
			m_pScrollView->SendMessage(WM_NEXTDLGCTL, (WPARAM)PwndPr->GetSafeHwnd(), TRUE);
			old_def = GetDlgItem(did);
			if (m_disptype==1)
			{
				if ((did!=IDC_FORMCANCEL)&&(old_def!=NULL))
					old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
				SendMessage(DM_SETDEFID, (WPARAM)IDC_FORMCANCEL);
				m_def_id = IDC_FORMCANCEL;
			}
			else
			{
				SetDefID(IDC_FORMACCEPT);
				if ((did!=IDC_FORMACCEPT)&&(old_def!=NULL))
					old_def->SendMessage(BM_SETSTYLE, BS_PUSHBUTTON, TRUE);
				m_def_id = IDC_FORMACCEPT;
			}
			new_def = GetDlgItem(m_def_id);
			if (new_def!=NULL)
				new_def->SendMessage(BM_SETSTYLE, BS_DEFPUSHBUTTON, TRUE);
		}
		else
			PostMessage(WM_NEXTDLGCTL, (WPARAM)PwndPr->GetSafeHwnd(), TRUE);
	}
}
/**********************************************************************
**    I_FUNCTION :  OnAccelFunctions(UINT id)
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
void CNCLForm::OnAccelFunctions(UINT id)
{
	CString sav_cmdstr;
/*
......use local value
*/
	int savcmd_enable,sav_cmdcur1, sav_cmdcur2;
	int * save_inptr;
	int jmpflag, sav_cur,sav_mode,save_ptype, line_num, upload;
/*
.....if it is in input mode, post the message like UNIX does in
.....function PreTranslateMessage to let uw_ntevent to handle it 
.....doing nothing if UD_pickmode = 1
.....Yurong 3/15/02
*/
//	if (UD_pickmode==1)
//		return;
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
/*
......should always reset to 1 (since the MFC will change it to 1 when mouse move)
......so we must be consistent
*/
	uw_ntsetcursor(1);
	ud_updatews(UG_SUPPRESS);
	UD_pickmode = sav_mode;
	ud_setpick_type(save_ptype);
	ug_reset_event();
	ug_reset_input(save_inptr);

	UD_UNMARK(jmpflag);
}
/***********************************************************************
**
**   FUNCTION: OnMove(int x, int y)
**
**		The framework calls this member function 
**		after the window's posiiton has changed. 
**   
**	 INPUT:  
**			  x:   Specifies the new x of position.
**			  y:   Specifies the new y of position.
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLForm::OnMove(int x, int y)
{
	CDialog::OnMove(x, y);
	if ((m_pScrollView==NULL)||(m_init==0)) return;
/*
......save position and size on form.pos file
*/
	int fcx = m_Rect.Width();
	int fcy = m_Rect.Height();
	int wrect[4];
	CRect windowRect;
	GetWindowRect(&windowRect);
	x = windowRect.left;
	y = windowRect.top;

	if (m_fStruct.att_win==1)
/*
......SCREEN
*/
	{
		wrect[0] = 0;
		wrect[1] = 0;
		wrect[2] = GetSystemMetrics(SM_CXSCREEN);
		wrect[3] = GetSystemMetrics(SM_CYSCREEN);
	}
	else
/*
......WINDOW
*/
	{
		NCL_MainFrame->GetWindowRect(&windowRect);
		wrect[0] = windowRect.left;
		wrect[1] = windowRect.top;
		wrect[2] = windowRect.Width();
		wrect[3] = windowRect.Height();
	}
	ud_save_frmpos(m_filename, x, y, fcx, fcy, &wrect[0], m_fStruct.att_win, m_fStruct.ref,0,0);
}
void CNCLForm::GetDataType(int n, int *data_type)
{
	for (int i=0; i<n;i++)
	{
		data_type[i] = m_fStruct.input_flds[i].ud_datatyp;
	}
}
void CNCLForm::SetDataInitType(int n, int *data_type)
{
	m_init_data_type_flag = 1;
	for (int i=0; i<n;i++)
	{
		m_data_type[i] = data_type[i];
	}
}
/*
.....the static text prompt don't have a center chararter for drawing in height
.....only SS_LEFT, SS_CENTER for align text, so we have to adject the position to
.....get the result as center in window height
*/
void CNCLForm::adjust_height(int fieldno)
{
	int dy;
	CRect rect, prect;
	UINT fid =  m_idtable[fieldno+m_dspno];
	switch(m_type[fieldno])
	{
/*
.....EDIT
*/
		case 3:
		case 11:
			if (m_input[fieldno]!=FORM_STRING)
				break;
			m_pScrollView->GetDlgItem(fid)->GetWindowRect(&rect);
			m_pScrollView->GetDlgItem(fid-1)->GetWindowRect(&prect);
/*
.....the top is always the same
*/
			if (rect.Height()>prect.Height())
			{
				dy = (rect.Height() - prect.Height())*0.5;
				prect.top += dy;
				prect.bottom += dy;
			}
			else
			{
/*
.....use choicebox size since the label size is calculate size
*/
				prect.top =  rect.top + 2;
				prect.bottom = rect.bottom;
			}
			m_pScrollView->ScreenToClient(prect);
			m_pScrollView->GetDlgItem(fid-1)->MoveWindow(&prect);
			break;
		case 2:
		case 9:
		case 13:
/*
.....choicebox
*/
			m_pScrollView->GetDlgItem(fid)->GetWindowRect(&rect);
			m_pScrollView->GetDlgItem(fid-1)->GetWindowRect(&prect);
/*
.....the top is always the same
*/
			if (rect.Height()>prect.Height())
			{
				dy = (rect.Height() - prect.Height())*0.5;
				prect.top += dy;
				prect.bottom += dy;
			}
			else
			{
/*
.....use choicebox size since the label size is calculate size
*/
/*
.....we could purposely put the choicebox under the text field
.....(the prompt label bottom is higher than choicebox, add '-2' to adjust little) 
.....in that case, don't adjust
*/
				if (prect.bottom-2 < rect.top)
					return;
				prect.top =  rect.top + 2;
				prect.bottom = rect.bottom;
			}
			m_pScrollView->ScreenToClient(prect);
			m_pScrollView->GetDlgItem(fid-1)->MoveWindow(&prect);
			break;
	}
}
/**********************************************************************
**    E_FUNCTION :  uw_ntfrm_setlist(int frmid, int fieldno, char *label)
**       Update a list field with new data
**    PARAMETERS   
**       INPUT  : 
**          frmid:	Form ID.
**			fieldno: field number
**			label: label to be set
**       OUTPUT :  
**          none
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntdisfrm_set_label(int frmid, int fieldno, char *label)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->set_label(fieldno, label);
		else
			UW_display_frm[frmid]->set_label(fieldno, label);
	}
}

/**********************************************************************
**    E_FUNCTION :  uw_ntdisfrm_set_butlabel(int frmid, int fieldno, char *label)
**       set the button new label 
**    PARAMETERS   
**       INPUT  : 
**          frmid:	Form ID.
**			fieldno: field number
**			label: label to be set
**       OUTPUT :  
**          none
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntdisfrm_set_butlabel(int frmid, int fieldno, char *label)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->set_butlabel(fieldno, label);
		else
			UW_display_frm[frmid]->set_butlabel(fieldno, label);
	}
}

/**********************************************************************
**    E_FUNCTION :  uw_if_formopen
**       check if there is any form opened
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**          none
**    RETURNS      : 1: yes
**						0: no
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_if_formopen()
{
	int i;
/*
......search for available form
*/
	for (i=0; i<50; i++)
	{
		if (UW_display_frmwin[i]!=NULL)
		{
			return 1;
		}
	}
	return 0;
}
/**********************************************************************
**    E_FUNCTION :  uw_ntfrm_settlist(int frmid, int fieldno, UD_TLIST *form_list)
**       Update a table list field with new data
**    PARAMETERS   
**       INPUT  : 
**          frmid:	Form ID.
**			fieldno: field number
**			form_list: new list data
**       OUTPUT :  
**          none
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntfrm_settlist(int frmid, int fieldno, int *form_list)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->set_tlist(fieldno, form_list);
		else
			UW_display_frm[frmid]->set_tlist(fieldno, form_list);
	}
	return;
}

extern "C" void uw_ntfrm_sorttable(UD_TABLEINFO *info, UD_SMETHOD sortfunc)
{
	int frmid = info->frmid;
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->sort_tlist(info->fldid, 
				info->col, sortfunc);
		else
			UW_display_frm[frmid]->sort_tlist(info->fldid, info->col, sortfunc);
	}
	return;

}
extern "C" void uw_ntset_curform_id(int id)
{
	current_frmid = id;
}

extern "C" void uw_ntsave_actform_id()
{
	save_formActFrm = current_frmid;
}

extern "C" int uw_ntget_savform_id()
{
	return save_formActFrm;
}

/**********************************************************************
**    E_FUNCTION :  uw_ntfrm_setcolor(int frmid, int fieldno, int color)
**       Update a color field with new data (this is only for set the color button color)
**    PARAMETERS   
**       INPUT  : 
**          frmid:	Form ID.
**			fieldno: field number
**			color: new color data
**       OUTPUT :  
**          none
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntfrm_setcolor(int frmid, int fieldno, int color)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->SetButColor(fieldno, color);
		else
			UW_display_frm[frmid]->SetButColor(fieldno, color);
	}
	return;
}

extern "C" void uw_ntfrm_enable_sec(int frmid, char *sec_name, int flag)
{
	if ((frmid<0)||(frmid>=60))
		return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		UW_display_frm[frmid]->EnableSection(sec_name, flag);
	}
}

extern "C" void uw_ntfrm_sec_color(int frmid, char *sec_name, int color[3], int bold)
{
	if ((frmid<0)||(frmid>=60))
		return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		UW_display_frm[frmid]->SetSecColor(sec_name, color, bold);
	}
}

extern "C" void uw_ntfrm_active_sec(int frmid, char *sec_name)
{
	if ((frmid<0)||(frmid>=60))
		return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		UW_display_frm[frmid]->SelectSection(sec_name);
	}
}

LRESULT CNCLForm::OnItemClick(WPARAM wParam, LPARAM lParam)
{
	return m_pScrollView->OnItemClick(wParam, lParam);
}

void CNCLForm::EnableOKBut(UU_LOGICAL flag)
{
	CWnd *wnd = GetDlgItem(IDC_FORMACCEPT);
	if (wnd!=NULL)
		wnd->EnableWindow(flag);
}

void CNCLForm::EnableCloseBut(UU_LOGICAL flag)
{
	CWnd *wnd = GetDlgItem(IDC_FORMCANCEL);
	if (wnd!=NULL)
		wnd->EnableWindow(flag);
}

/*********************************************************************
**    E_FUNCTION : ud_dispfrm_set_init_datatyp(frmid, n, data_type)
**			Set the pass in form data type
**			sometime, we need form routine know the pass in default type
**			in order for the form to set the correct default data
**    PARAMETERS   
**       INPUT  :  frmid	:	Form ID number, start from 0
**				n: total number setting in data_type
**				data_type: data array of the data type
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntfrm_set_init_datatyp(int frmid, int n, int *data_type)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->SetDataInitType(n, data_type);
		else
			UW_display_frm[frmid]->SetDataInitType(n, data_type);
	}
	return;
}
/*********************************************************************
**    E_FUNCTION : ud_dispfrm_get_form_datatyp(frmid, n, data_type)
**			Get the data type used in the form.
**			sometime, we need know the current form data type
**			in order for save the correct input data
**    PARAMETERS   
**       INPUT  :  frmid	:	Form ID number, start from 0
**				n: total number setting in data_type
**       OUTPUT :  
**				data_type: data array to save the data type
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntfrm_get_form_datatyp(int frmid, int n, int *data_type)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->GetDataType(n, data_type);
		else
			UW_display_frm[frmid]->GetDataType(n, data_type);
	}
	return;
}
/*********************************************************************
**    E_FUNCTION : uw_ntfrm_set_form_attribs(frmid, fieldno, fg, bg)
**			reset field fieldno new foreground and background color
**    PARAMETERS   
**       INPUT  :  frmid: form ID/index
**				fieldno	:	Field number
**				fg, bg : new foreground and background color index 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntfrm_set_form_attribs(int frmid, int fieldno, int fg, int bg)
{
	if (frmid<0) return;
	if (UW_display_frmwin[frmid]!=NULL)
	{
		if (UW_display_frmwin[frmid]->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			NCL_MainFrame->m_formbar[frmid]->SetFieldColr(fieldno, fg, bg);
		else
			UW_display_frm[frmid]->SetFieldColr(fieldno, fg, bg);
	}
	return;
}
/*********************************************************************
**    E_FUNCTION : uw_ntfrm_enable_ok(flag)
**			reset field fieldno new foreground and background color
**    PARAMETERS   
**       INPUT  :  flag: 0: disable OK button 
**						1: enable OK button
**				
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntfrm_enable_ok(UU_LOGICAL flag)
{
	if (UW_display_frmwin[0]!=NULL)
	{
		UW_display_frm[0]->EnableOKBut(flag);
	}
}

/*********************************************************************
**    E_FUNCTION : uw_ntfrm_enable_close(frmid, flag)
**			reset field fieldno new foreground and background color
**    PARAMETERS   
**       INPUT  :  frmid: form id
**					flag: 0: disable OK button 
**						1: enable OK button
**				
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntfrm_enable_close(int frmid, UU_LOGICAL flag)
{
	if (UW_display_frmwin[frmid]!=NULL)
	{
		UW_display_frm[frmid]->EnableCloseBut(flag);
	}
}

extern "C" int uw_ifvisual_ctrl()
{
	if ((UW_theme & STAP_ALLOW_CONTROLS)!=0)
		return 1;
	else
		return 0;
}
extern "C" void uw_ntget_pixelsize_from_unit(int pt, char *fntname, int unitx, int unity, int *pic_x, int *pic_y)
{
	CFont aFont;
	CWnd *picctl = NCL_MainFrame;
	CClientDC dc(picctl);
	aFont.CreatePointFont(pt,fntname, &dc);
	SIZE size;

	CFont* savfont = dc.SelectObject(&aFont );
	CSize sizeText = dc.GetTextExtent("XXXXXXXXXXxxxxxxxxxx",20);
//	double basex = ((double)sizeText.cx)/20;
//	double basey = (double)sizeText.cy;

	sizeText = dc.GetTextExtent("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",52);
	double basex = ((double)sizeText.cx)/52;
	double basey = (double)sizeText.cy;


	*pic_x = unitx*basex/4.0;
	*pic_y = unity*basey/8.0;

	dc.SelectObject(&savfont);
}

extern "C" void uw_ntget_size_ratio(int pt1, char *fntname1, int pt2, char *fntname2,
			double *ratio_x, double *ratio_y)
{
	CFont aFont, bFont;
	float ratio;
	int wid1, hgt1, wid2, hgt2;
	CWnd *ctl = NCL_MainFrame;
	CClientDC dc(ctl);
	aFont.CreatePointFont(pt1,fntname1, &dc);
	bFont.CreatePointFont(pt2,fntname2, &dc);

	CFont* savfont = dc.SelectObject(&aFont);

//	CString itemtext = ( LPCTSTR)string;
	CSize sizeText = dc.GetTextExtent("ABCDEFGabcdefgXxHhIi",20);
	wid1 = sizeText.cx;
	hgt1 = sizeText.cy;

	dc.SelectObject(&bFont);
	sizeText = dc.GetTextExtent("ABCDEFGabcdefgXxHhIi",20);
	wid2 = sizeText.cx;
	hgt2 = sizeText.cy;

	*ratio_x = (wid1*1.0)/(wid2*1.0);
	*ratio_y = (hgt1*1.0)/(hgt2*1.0);

	dc.SelectObject(&savfont);
}


#endif

