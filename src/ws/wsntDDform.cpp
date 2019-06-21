/************************************************************************
**		FILE NAME: CNCLDDForm.cpp
**
**	 Description - Functions and implementations for
**		CNCLDDform class with Drag&Drop
**	 CONTAINS:
**		member function of CNCLDDform
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDform.cpp , 26.3
**    DATE AND TIME OF LAST  MODIFICATION
**			05/22/18 , 10:30:40
**********************************************************************
*/
#include "stdafx.h"
#include "usysdef.h"
#include <afxole.h>         // MFC OLE classes
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxdisp.h >       // MFC OLE automation classes
#include <afxpriv.h>

#include "mfort.h"
#include "nconst.h"

#include "wsntDDform.h"
#include "wsntdropsource.h"
#include "wsntddedit.h"
#include "wsntddbutton.h"
#include "wsntddlistbox.h"
#include "wsntddcombo.h"
#include "wsntddclrbtn.h"
#include "wsntddstatic.h"
#include "wsntddpic.h"
#include "wsntDDlstctl.h"
#include "wsntDDlstctl2.h"
#include "wsntDDProcess.h"
#include "wsntDDgroup.h"
#include "wsntDDSlider.h"
#include "wsntsecbtn.h"
#include "wsntcfunc.h"
#include "wsntfrmview.h"
#include "wsntwininfo.h"
#include "wsntfrmpview.h"
#include "wsntfrmmview.h"
#include "mdrel.h"

extern "C" int uu_tst_bit(unsigned long word_ptr[], int ibit);
extern void uw_revMapDialogRect(CRect &rect, int baseunitX, int baseunitY);
extern void uw_MapDialogRect(CRect &rect, CDialog *wnd);
extern void uw_revMapLen(int &cx, int &cy, int baseunitX, int baseunitY);
#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define FORM_STRING		1
#define FORM_PICK		2
#define FORM_LOCATE		3
#define FORM_RECORD		4
#define FORM_LABEL		5
#define FORM_SUBSCR		6

#define TEXT_WID	4

#define BARLEN	4
#define HT_NONE		0
#define HT_INSIDE	1
#define HT_TOPLEFT	2
#define HT_LEFT		3
#define HT_BOTTOMLEFT	4
#define HT_BOTTOM	5
#define HT_BOTTOMRIGHT	6
#define HT_RIGHT	7
#define HT_TOPRIGHT	8
#define HT_TOP	9
#define HT_UPDOWN	10

extern CFrameWnd *UW_Dform_frame;

extern char UW_formdlg_font[];
extern "C" float uw_form_scalex;
extern "C" float uw_form_scaley;
extern HBITMAP WINAPI  CopyWindowToBitmap(LPRECT lpRect, CWnd *pWnd);

extern "C" int ul_open_mod_file(char*, char*, char*, char*, char*, int, FILE**);
extern int uw_get_rgb (char *color_str, COLORREF &color);
extern void RGBtoHSL( COLORREF rgb, double *H, double *S, double *L );
extern void HLStoRGB(double H, double L, double S,  int &red, int &green, int &blue);

extern "C" int ncl_getmac_pnum(int *num);
extern "C" void ncl_getmac_parms(int *, char*, UM_int2 *, char*, int*, char*, UM_int2*, 
		UM_real8*, UM_real8*, UM_int2 *, UM_int2 *, int*, int*, int*, int*);
extern "C" void ncl_getmac_values(int *, UM_int2 *, UM_int4 *, char *, UM_int4 *, UM_real8 *, UM_int2 *, UM_int2 *);
extern "C" int ul_break_fname(char *fullname, char* dir, char* fname);

extern int UW_form_hotspot_item;
/***********************************************************************
**   FUNCTION: convert_limit_str(int in_limit[UD_NMENTWD], char *limit_str)
**		Convert a picking limit value from integer value (saved in fstruct->input_flds[i].ud_limit)
**		into a string format (such as "LINE, POINT,...")
**
**   INPUT:  in_limit: picking limit value
**
**   OUTPUT :   limit_str: limit string format
**   RETURN:    None
**
**********************************************************************/
int convert_limit_str(int in_limit[UD_NMENTWD], char *limit_str)
{
	unsigned long limit[UD_NMENTWD];
	int i, j;
	static char geotyp[14][10]={"POINT","LINE","CIRCLE","PLANE","VECTOR",
		"PNTVEC","CURVE","SURF","PATERN","SYMBOL","ANOTE","MATRIX","SHAPE",
		"SOLID"};
	limit_str[0] = '\0';
	for (i=0; i<UD_NMENTWD;i++)
	{
		limit[i] = in_limit[i];
	}
	if (uu_tst_bit(limit, NCL_POINT_REL-1))
	{
		strcat(limit_str, "POINT,");
	}
	if (uu_tst_bit(limit, NCL_LINE_REL-1))
	{
		strcat(limit_str, "LINE,");
	}
	if (uu_tst_bit(limit, NCL_CIRCLE_REL-1))
	{
		strcat(limit_str, "CIRCLE,");
	}
	if (uu_tst_bit(limit, NCL_PLN_REL-1))
	{
		strcat(limit_str, "PLANE,");
	}
	if (uu_tst_bit(limit, NCL_VECTOR_REL-1))
	{
		strcat(limit_str, "VECTOR,");
	}
	if (uu_tst_bit(limit, NCL_POINTVEC_REL-1))
	{
		strcat(limit_str, "PNTVEC,");
	}
	if (uu_tst_bit(limit, NCL_CURVE_REL-1))
	{
		strcat(limit_str, "CURVE,");
	}
	if (uu_tst_bit(limit, NCL_SURF_REL-1))
	{
		strcat(limit_str, "SURF,");
	}
	if (uu_tst_bit(limit, NCL_PATERN_REL-1))
	{
		strcat(limit_str, "PATERN,");
	}
	if (uu_tst_bit(limit, UA_SYMBOLS_REL-1))
	{
		strcat(limit_str, "SYMBOL,");
	}
	if (uu_tst_bit(limit, UA_TEXT_REL-1))
	{
		strcat(limit_str, "ANOTE,");
	}
	if (uu_tst_bit(limit, NCL_MATRIX_REL-1))
	{
		strcat(limit_str, "MATRIX,");
	}
	if (uu_tst_bit(limit, NCL_SHAPE_REL-1))
	{
		strcat(limit_str, "SHAPE,");
	}
	if (uu_tst_bit(limit, NCL_SHAPE_REL-1))
	{
		strcat(limit_str, "SHAPE,");
	}
	if (uu_tst_bit(limit, UM_SOLID_REL-1))
	{
		strcat(limit_str, "SOLID,");
	}
	int len = strlen(limit_str);
	if (len>0)
	{
		len--;
		limit_str[len] = '\0';
	}
	return len; 
}
static int Sskip_num = 0;
static int Sskip_array[200];

static void S_skip_reset()
{
	Sskip_num = 0;
}
static void S_skip_next_save(int itemno)
{
	Sskip_array[Sskip_num] = itemno;
	Sskip_num++;
}

static int S_skip_next(int itemno)
{
	for (int i = 0; i<Sskip_num; i++)
	{
		if (itemno==Sskip_array[i])
			return 1;
	}
	return 0;
}

IMPLEMENT_DYNAMIC(CNCLDDform, CWnd)

/***********************************************************************
**   FUNCTION: CNCLDDform
**		Constructor of class CNCLDDform
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDDform::CNCLDDform(CWnd *parent)
	:	m_TargetDrop(NULL)
{
	m_TargetDrop = new CNCLMnDropTarget2();
	m_parent = parent;
	m_selnum = 0;
	m_multi_drag = 0;
	for (int i=0; i<200; i++)
	{
		m_inpwin[i] = NULL;
		m_dinpwin[i] = NULL;
		m_disp_win[i] = NULL;
		m_frame[i] = NULL;
		m_picture[i] = NULL;
		m_selarray[i] = -1;
		m_disp_secpage[i] = 0;
		m_inp_secpage[i] = 0;
		m_pic_secpage[i] = 0;
		m_frm_secpage[i] = 0;
	}
	for (int i=0; i<6; i++)
	{
		m_macro_win[i] = NULL;
	}
	m_dispno = 0;
	m_inputno = 0;
	m_frameno = 0;
	m_picno = 0;
	m_mwin_no = 0;
	m_cursor[HT_NONE] = LoadCursor(NULL, IDC_ARROW);
	m_cursor[HT_INSIDE] = LoadCursor(NULL, IDC_SIZEALL);
	m_cursor[HT_TOPLEFT] = LoadCursor(NULL, IDC_SIZENWSE);
	m_cursor[HT_LEFT] = LoadCursor(NULL, IDC_SIZEWE);
	m_cursor[HT_BOTTOMLEFT] = LoadCursor(NULL, IDC_SIZENESW);
	m_cursor[HT_BOTTOM] = LoadCursor(NULL, IDC_SIZENS);
	m_cursor[HT_BOTTOMRIGHT] = LoadCursor(NULL, IDC_SIZENWSE);
	m_cursor[HT_RIGHT] = LoadCursor(NULL, IDC_SIZEWE);
	m_cursor[HT_TOPRIGHT] = LoadCursor(NULL, IDC_SIZENESW);
	m_cursor[HT_TOP] = LoadCursor(NULL, IDC_SIZENS);
	m_cursor[HT_UPDOWN] = LoadCursor(NULL, IDC_SIZENS);
	m_buttondown = 0;
	m_sizedir = 0;
	m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
	m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
	
	for (int i=0; i<8; i++)
	{
		m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
	}
	m_selitem = -1;
	m_selwin = NULL;
	m_resize = 0;
	m_update_area = 0;
/*
.....0, no combox
.....1, combo box without listbox show
.....2, combo with listbox show
*/
	m_dropdown = 0;
	m_select_draw = 1;
	m_mwin_no = 0;
	m_range_flag = 0;
	m_macro_flag = 0;
	m_current_sec = -1;
	m_no_undo_save = 0;
	m_no_redo = 0;
	m_reset_redo = 1;
	m_hotspot_rect.left = -1000;
	UW_form_hotspot_item = 0;
	m_current_pic = NULL;
	m_reset_color = 0;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLDDform
**              Destructor of class CNCLDDform, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDDform::~CNCLDDform()
{
	while( m_pUndoList.GetCount() )
	{
		CNCLWinInfo* pItem = m_pUndoList.GetHead();
		delete pItem;
		m_pUndoList.RemoveHead();
	}
	while( m_pRedoList.GetCount() )
	{
		CNCLWinInfo* pItem = m_pRedoList.GetHead();
		delete pItem;
		m_pRedoList.RemoveHead();
	}
	for (int i=0; i<m_dispno; i++)
	{
		if (m_disp_win[i] != NULL)
			delete m_disp_win[i];
	}	
	for (int i=0; i<m_frameno; i++)
	{
		if (m_frame[i] != NULL)
			delete m_frame[i];
	}
	for (int i=0; i<m_picno; i++)
	{
		if (m_picture[i] != NULL)
			delete m_picture[i];
	}
	for (int i=0; i<m_inputno; i++)
	{
		if (m_inpwin[i] != NULL)
			delete m_inpwin[i];
		if (m_dinpwin[i] != NULL)
			delete m_dinpwin[i];
	}
	for (int i=0; i<6; i++)
	{
		if (m_macro_win[i]!=NULL)
			delete m_macro_win[i];
	}
}

BEGIN_MESSAGE_MAP(CNCLDDform, CWnd)
	//{{AFX_MSG_MAP(CNCLDDform)
	ON_WM_PAINT()
	ON_WM_DESTROY()
	ON_WM_CREATE()
	ON_WM_TIMER()
	ON_WM_LBUTTONUP()
	ON_WM_LBUTTONDOWN()
	ON_WM_RBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_COMMAND(ID_DELETE_FRMITEM, OnDeleteItems)
	ON_MESSAGE(WM_CTLCOLOREDIT, OnCtrlColorEdit)
	ON_MESSAGE(WM_CTLCOLORBTN, OnCtrlColorBtn)
	ON_MESSAGE(WM_CTLCOLORLISTBOX, OnCtrlColorListBox)
	ON_MESSAGE(WM_CTLCOLORSCROLLBAR, OnCtrlColorScroll)
	ON_MESSAGE(WM_CTLCOLORSTATIC, OnCtrlColorStatic)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
/***********************************************************************
**   FUNCTION: OnCtrlColorEdit(WPARAM wParam, LPARAM lParam)
**			This function called when edit field color changed
**			this is working for editbox and CNCLDDCombo
**   INPUT:  WPARAM wParam: include HDC information of the control 
**			LPARAM lParam: Window handler of the control
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
LRESULT CNCLDDform::OnCtrlColorEdit(WPARAM wParam, LPARAM lParam)
{
	int i, len, ret;
	char tempstr[256], fcolor_str[256], bcolor_str[256], *tok;
	COLORREF fcolor, bcolor;
	CNCLFormProp *prop_dlg;
	HWND ctl;
	ctl = (HWND)lParam;
	for (i=0; i<m_inputno; i++)
	{
		fcolor = RGB(0,0,0);
		bcolor = RGB (255, 255, 255);
		if ((m_type[i]==3)||(m_type[i]==11)||(m_type[i]==16)
			||(m_type[i]==2)||(m_type[i]==8)||(m_type[i]==9)||(m_type[i]==15) )
		{
			if ((m_inpwin[i]!=NULL)&&(m_inpwin[i]->m_hWnd==ctl))
			{
				if ((m_type[i]==3)||(m_type[i]==11)||(m_type[i]==16))
					prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[i]))->GetPropertyPage());
				else
					prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[i]))->GetPropertyPage());
				len = prop_dlg->m_color.GetLength();
				if (len==0)
				{
					fcolor_str[0] = '\0';
					bcolor_str[0] = '\0';
					goto set_color;
				}
				strcpy(tempstr, prop_dlg->m_color.GetBuffer());
				tok = (char*)strtok (tempstr, " ,\t\r\n");
				if (tok==NULL)
				{
					fcolor_str[0] = '\0';
					bcolor_str[0] = '\0';
					goto set_color;
				}
				strcpy(fcolor_str, tok);
				tok = (char*)strtok (NULL, " ,\t\r\n");
				if (tok==NULL)
				{
					bcolor_str[0] = '\0';
				}
				else
					strcpy(bcolor_str, tok);
set_color:;
				if (fcolor_str[0]=='\0')
				{
					fcolor = RGB(0,0,0);
				}
				else
				{
					ret = uw_get_rgb (fcolor_str, fcolor);
					if (ret==-1)
						fcolor = RGB(0,0,0);
				}
				if (bcolor_str[0]=='\0')
				{
					bcolor = RGB(255,255,255);
				}
				else
				{
					ret = uw_get_rgb (bcolor_str, bcolor);
					if (ret==-1)
						bcolor = RGB(255,255,255);
				}
				if (prop_dlg->m_active==-1)
				{
					fcolor = RGB(125,125,125);
					bcolor = GetSysColor(COLOR_BTNFACE);
				}
				SetTextColor((HDC)wParam, fcolor);
				SetBkColor((HDC)wParam, bcolor);
				static HBRUSH bh = CreateSolidBrush(bcolor);
				return (LRESULT)bh;
			}
		}
	}
	return TRUE;
}

/***********************************************************************
**   FUNCTION: OnCtrlColorBtn(WPARAM wParam, LPARAM lParam)
**			This function called when button color changed
**			
**   INPUT:  WPARAM wParam: include HDC information of the control 
**			LPARAM lParam: Window handler of the control
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
LRESULT CNCLDDform::OnCtrlColorBtn(WPARAM wParam, LPARAM lParam)
{
	int i, len,ret;
	char tempstr[256], fcolor_str[256], bcolor_str[256], *tok;
	COLORREF fcolor, bcolor;
	CNCLFormProp *prop_dlg;
	HWND ctl;
	ctl = (HWND)lParam;
	for (i=0; i<m_inputno; i++)
	{
		if ((m_dinpwin[i]!=NULL)&&(m_dinpwin[i]->m_hWnd==ctl))
		{
			if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDButton))==0)
				continue;
			fcolor = RGB(0,0,0);
			bcolor = GetSysColor(COLOR_BTNFACE);
			prop_dlg = ((CNCLDDButton*)m_dinpwin[i])->m_prop_dlg;
			len = prop_dlg->m_pcolor.GetLength();
			if (len==0)
			{
				fcolor_str[0] = '\0';
				bcolor_str[0] = '\0';
			}
			else
			{
				strcpy(tempstr, prop_dlg->m_pcolor.GetBuffer());
				tok = (char*)strtok (tempstr, " ,\t\r\n");
				if (tok==NULL)
				{
					fcolor_str[0] = '\0';
					bcolor_str[0] = '\0';
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
				}
			}
			if (fcolor_str[0]=='\0')
			{
				fcolor = RGB(0,0,0);
			}
			else
			{
				ret = uw_get_rgb (fcolor_str, fcolor);
				if (ret==-1)
				{
					fcolor = RGB(0,0,0);
				}
			}
			if (bcolor_str[0]=='\0')
			{
				bcolor = GetSysColor(COLOR_BTNFACE);
			}
			else
			{
				ret = uw_get_rgb (bcolor_str, bcolor);
				if (ret==-1)
				{
					bcolor = GetSysColor(COLOR_BTNFACE);
				}
			}
			SetTextColor((HDC)wParam, fcolor);
			SetBkColor((HDC)wParam, bcolor);
			((CNCLDDButton*)(m_dinpwin[i]))->set_color(bcolor, fcolor);
			static HBRUSH bh = CreateSolidBrush(bcolor);
			return (LRESULT)bh;
		}
	}
	int r,g,b;
	double H, S, L;
	for (i=0; i<m_inputno; i++)
	{
		fcolor = RGB(0,0,0);
		bcolor = GetSysColor(COLOR_BTNFACE);
/*
......checkbox here also
*/
		if ((m_type[i]==1)||(m_type[i]==7)||(m_type[i]==25)
			||(m_type[i]==18)||(m_type[i]==23))
		{
			if (m_type[i]==18)
			{
				fcolor = RGB(255,128,0);
				bcolor = RGB(255,128,0);
			}
			if (m_type[i]==23)
			{
				fcolor = RGB(128,128,0);
				bcolor = RGB(128,128,0);
			}
			if ((m_inpwin[i]!=NULL)&&(m_inpwin[i]->m_hWnd==ctl))
			{
				if ((m_type[i]==1)||(m_type[i]==7)||(m_type[i]==25))
				{
					prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[i]))->GetPropertyPage());
				}
				else
				{
					prop_dlg = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[i]))->GetPropertyPage());
				}
				len = prop_dlg->m_color.GetLength();
				if (len==0)
				{
					fcolor_str[0] = '\0';
					bcolor_str[0] = '\0';
					goto set_color;
				}
				strcpy(tempstr, prop_dlg->m_color.GetBuffer());
				tok = (char*)strtok (tempstr, " ,\t\r\n");
				if (tok==NULL)
				{
					fcolor_str[0] = '\0';
					bcolor_str[0] = '\0';
					goto set_color;
				}
				strcpy(fcolor_str, tok);
				tok = (char*)strtok (NULL, " ,\t\r\n");
				if (tok==NULL)
				{
					bcolor_str[0] = '\0';
				}
				else
					strcpy(bcolor_str, tok);
set_color:;
				if (fcolor_str[0]=='\0')
				{
					fcolor = RGB(0,0,0);
				}
				else
				{
					ret = uw_get_rgb (fcolor_str, fcolor);
					if (ret==-1)
					{
						fcolor = RGB(0,0,0);
						if (m_type[i]==18)
						{
							fcolor = RGB(255,128,0);
						}
						if (m_type[i]==23)
						{
							fcolor = RGB(128,128,0);
						}
					}
				}
				if (bcolor_str[0]=='\0')
				{
					bcolor = GetSysColor(COLOR_BTNFACE);
				}
				else
				{
					ret = uw_get_rgb (bcolor_str, bcolor);
					if (ret==-1)
					{
						bcolor = GetSysColor(COLOR_BTNFACE);
						if (m_type[i]==18)
						{
							bcolor = RGB(255,128,0);
						}
						if (m_type[i]==23)
						{
							bcolor = RGB(128,128,0);
						}
					}
				}
				SetTextColor((HDC)wParam, fcolor);
				SetBkColor((HDC)wParam, bcolor);
				if ((m_type[i]==1)||(m_type[i]==7)||(m_type[i]==25))
				{
					((CNCLDDButton*)(m_inpwin[i]))->set_color(bcolor, fcolor);
				}
				else if ((m_type[i]==18)||(m_type[i]==23))
				{
					if (prop_dlg->m_active==-1)
					{
						RGBtoHSL(bcolor, &H, &S, &L);
						L = L*1.3;
						HLStoRGB(H, L, S,  r, g, b);
						bcolor = RGB(r,g,b);
					}
					((CNCLDDColorButton*)(m_inpwin[i]))->set_color(bcolor, fcolor);
				}
				static HBRUSH bh = CreateSolidBrush(bcolor);
				return (LRESULT)bh;
			}
		}
	}
	for (i=0; i<m_mwin_no; i++)
	{
		fcolor = RGB(0,0,0);
		bcolor = GetSysColor(COLOR_BTNFACE);
		if ((m_macro_win[i]!=NULL)&&(m_macro_win[i]->m_hWnd==ctl))
		{
			prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_macro_win[i]))->GetPropertyPage());
			len = prop_dlg->m_color.GetLength();
			if (len==0)
			{
				fcolor_str[0] = '\0';
				bcolor_str[0] = '\0';
				goto set_color2;
			}
			strcpy(tempstr, prop_dlg->m_color.GetBuffer());
			tok = (char*)strtok (tempstr, " ,\t\r\n");
			if (tok==NULL)
			{
				fcolor_str[0] = '\0';
				bcolor_str[0] = '\0';
				goto set_color2;
			}
			strcpy(fcolor_str, tok);
			tok = (char*)strtok (NULL, " ,\t\r\n");
			if (tok==NULL)
			{
				bcolor_str[0] = '\0';
			}
			else
				strcpy(bcolor_str, tok);
set_color2:;
			if (fcolor_str[0]=='\0')
			{
				fcolor = RGB(0,0,0);
			}
			else
			{
				ret = uw_get_rgb (fcolor_str, fcolor);
				if (ret==-1)
				{
					fcolor = RGB(0,0,0);
				}
			}
			if (bcolor_str[0]=='\0')
			{
				bcolor = GetSysColor(COLOR_BTNFACE);
			}
			else
			{
				ret = uw_get_rgb (bcolor_str, bcolor);
				if (ret==-1)
				{
					bcolor = GetSysColor(COLOR_BTNFACE);
				}
			}
			SetTextColor((HDC)wParam, fcolor);
			SetBkColor((HDC)wParam, bcolor);
			((CNCLDDButton*)(m_macro_win[i]))->set_color(bcolor, fcolor);
			static HBRUSH bh = CreateSolidBrush(bcolor);
			return (LRESULT)bh;
		}
	}
	return TRUE;
}

/***********************************************************************
**   FUNCTION: OnCtrlColorListBox(WPARAM wParam, LPARAM lParam)
**			This function called when list box color changed
**			
**   INPUT:  WPARAM wParam: include HDC information of the control 
**			LPARAM lParam: Window handler of the control
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
LRESULT CNCLDDform::OnCtrlColorListBox(WPARAM wParam, LPARAM lParam)
{
	int i, len,ret;
	char tempstr[256], fcolor_str[256], bcolor_str[256], *tok;
	COLORREF fcolor, bcolor;
	CNCLFormProp *prop_dlg;
	HWND ctl;
	ctl = (HWND)lParam;
	for (i=0; i<m_inputno; i++)
	{
		fcolor = RGB(0,0,0);
		bcolor = RGB (255, 255, 255);
		if (m_type[i]==5)
		{
			if ((m_inpwin[i]!=NULL)&&(m_inpwin[i]->m_hWnd==ctl))
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListBox*)(m_inpwin[i]))->GetPropertyPage());
				len = prop_dlg->m_color.GetLength();
				if (len==0)
				{
					fcolor_str[0] = '\0';
					bcolor_str[0] = '\0';
					goto set_color;
				}
				strcpy(tempstr, prop_dlg->m_color.GetBuffer());
				tok = (char*)strtok (tempstr, " ,\t\r\n");
				if (tok==NULL)
				{
					fcolor_str[0] = '\0';
					bcolor_str[0] = '\0';
					goto set_color;
				}
				strcpy(fcolor_str, tok);
				tok = (char*)strtok (NULL, " ,\t\r\n");
				if (tok==NULL)
				{
					bcolor_str[0] = '\0';
				}
				else
					strcpy(bcolor_str, tok);
set_color:;
				if (fcolor_str[0]=='\0')
				{
					fcolor = RGB(0,0,0);
				}
				else
				{
					ret = uw_get_rgb (fcolor_str, fcolor);
					if (ret==-1)
					{
						fcolor = RGB(0,0,0);
					}
				}
				if (bcolor_str[0]=='\0')
				{
					bcolor = RGB(255,255,255);
				}
				else
				{
					ret = uw_get_rgb (bcolor_str, bcolor);
					if (ret==-1)
					{
						bcolor = RGB(255,255,255);
					}
				}
				if (prop_dlg->m_active==-1)
				{
					fcolor = RGB(125,125,125);
					bcolor = ::GetSysColor(COLOR_BTNFACE);
				}
				SetTextColor((HDC)wParam, fcolor);
				SetBkColor((HDC)wParam, bcolor);
				static HBRUSH bh = CreateSolidBrush(bcolor);
				return (LRESULT)bh;
			}
		}
	}
	return TRUE;
}

LRESULT CNCLDDform::OnCtrlColorScroll(WPARAM wParam, LPARAM lParam)
{
	return TRUE;
}
/***********************************************************************
**   FUNCTION: OnCtrlColorStatic(WPARAM wParam, LPARAM lParam)
**			This function called when "Static" window color changed
**			This works for checkbox, groupbox
**   INPUT:  WPARAM wParam: include HDC information of the control 
**			LPARAM lParam: Window handler of the control
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
LRESULT CNCLDDform::OnCtrlColorStatic(WPARAM wParam, LPARAM lParam)
{
	int i, len,ret;
	char tempstr[256], fcolor_str[256], bcolor_str[256], *tok;
	COLORREF fcolor, bcolor;
	CNCLFormProp *prop_dlg;
	HWND ctl;
	ctl = (HWND)lParam;
	HDC  hdc = (HDC) wParam;
	for (i=0; i<m_frameno; i++)
	{
		fcolor = RGB(0,0,0);
		bcolor = ::GetSysColor(COLOR_BTNFACE);
		if ((m_frame[i]!=NULL)&&(m_frame[i]->m_hWnd==ctl))
		{
			prop_dlg = ((CNCLFormProp *)((CNCLDDGroup*)(m_frame[i]))->GetPropertyPage());
			len = prop_dlg->m_pcolor.GetLength();
			if (len==0)
			{
				fcolor_str[0] = '\0';
				bcolor_str[0] = '\0';
			}
			else
			{
				strcpy(tempstr, prop_dlg->m_pcolor.GetBuffer());
				tok = (char*)strtok (tempstr, " ,\t\r\n");
				if (tok==NULL)
				{
					fcolor_str[0] = '\0';
					bcolor_str[0] = '\0';
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
				}
			}
			if (fcolor_str[0]=='\0')
			{
				fcolor = RGB(0,0,0);
			}
			else
			{
				ret = uw_get_rgb (fcolor_str, fcolor);
				if (ret==-1)
					fcolor = RGB(0,0,0);
			}
			if (bcolor_str[0]=='\0')
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE);
			}
			else
			{
				ret = uw_get_rgb (bcolor_str, bcolor);
				if (ret==-1)
					bcolor = ::GetSysColor(COLOR_BTNFACE);
			}
			if (prop_dlg->m_active==-1)
				fcolor = RGB(125,125,125);
			SetTextColor((HDC)wParam, fcolor);
			SetBkColor((HDC)wParam, bcolor);
			static HBRUSH bh = CreateSolidBrush(bcolor);
			return (LRESULT)bh;
		}
	}
	for (i=0; i<m_inputno; i++)
	{
		fcolor = RGB(0,0,0);
		bcolor = ::GetSysColor(COLOR_BTNFACE);
		if ((m_type[i]!=7)&&(m_type[i]!=24))
			continue;
		if ((m_inpwin[i]!=NULL)&&(m_inpwin[i]->m_hWnd==ctl))
		{
			if (m_type[i]==24)
				prop_dlg = ((CNCLFormProp *)((CNCLDDSlider*)(m_inpwin[i]))->GetPropertyPage());
			else
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[i]))->GetPropertyPage());
			len = prop_dlg->m_color.GetLength();
			if (len==0)
			{
				fcolor_str[0] = '\0';
				bcolor_str[0] = '\0';
				goto set_color2;
			}
			strcpy(tempstr, prop_dlg->m_color.GetBuffer());
			tok = (char*)strtok (tempstr, " ,\t\r\n");
			if (tok==NULL)
			{
				fcolor_str[0] = '\0';
				bcolor_str[0] = '\0';
				goto set_color2;
			}
			strcpy(fcolor_str, tok);
			tok = (char*)strtok (NULL, " ,\t\r\n");
			if (tok==NULL)
			{
				bcolor_str[0] = '\0';
			}
			else
				strcpy(bcolor_str, tok);
set_color2:;
			if (fcolor_str[0]=='\0')
			{
				if (m_type[i]==24)
					fcolor = RGB(255,100,100);
				else
					fcolor = RGB(0,0,0);
			}
			else
			{
				ret = uw_get_rgb (fcolor_str, fcolor);
				if (ret==-1)
				{
					if (m_type[i]==24)
						fcolor = RGB(255,100,100);
					else
						fcolor = RGB(0,0,0);
				}
			}
			if (bcolor_str[0]=='\0')
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE);
			}
			else
			{
				ret = uw_get_rgb (bcolor_str, bcolor);
				if (ret==-1)
					bcolor = ::GetSysColor(COLOR_BTNFACE);
			}
/*
......we temp set Grey background if it is set to disabled
*/
			if (prop_dlg->m_active==-1)
				fcolor = RGB(125,125,125);
			SetTextColor((HDC)wParam, fcolor);
			SetBkColor((HDC)wParam, bcolor);
			static HBRUSH bh = CreateSolidBrush(bcolor);
			return (LRESULT)bh;
		}
	}
	for (i=0; i<m_mwin_no; i++)
	{
		fcolor = RGB(0,0,0);
		bcolor = ::GetSysColor(COLOR_BTNFACE);
/*
......we temp set Grey background if it is set to disabled
*/
		if ((m_macro_win[i]!=NULL)&&(m_macro_win[i]->m_hWnd==ctl))
		{
			prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_macro_win[i]))->GetPropertyPage());
			len = prop_dlg->m_color.GetLength();
			if (len==0)
			{
				fcolor_str[0] = '\0';
				bcolor_str[0] = '\0';
				goto set_color3;
			}
			strcpy(tempstr, prop_dlg->m_color.GetBuffer());
			tok = (char*)strtok (tempstr, " ,\t\r\n");
			if (tok==NULL)
			{
				fcolor_str[0] = '\0';
				bcolor_str[0] = '\0';
				goto set_color3;
			}
			strcpy(fcolor_str, tok);
			tok = (char*)strtok (NULL, " ,\t\r\n");
			if (tok==NULL)
			{
				bcolor_str[0] = '\0';
			}
			else
				strcpy(bcolor_str, tok);
set_color3:;
			if (fcolor_str[0]=='\0')
			{
				fcolor = RGB(0,0,0);
			}
			else
			{
				ret = uw_get_rgb (fcolor_str, fcolor);
				if (ret==-1)
					fcolor = RGB(0,0,0);
			}
			if (bcolor_str[0]=='\0')
			{
				bcolor = ::GetSysColor(COLOR_BTNFACE);
			}
			else
			{
				ret = uw_get_rgb (bcolor_str, bcolor);
				if (ret==-1)
					bcolor = ::GetSysColor(COLOR_BTNFACE);
			}
/*
......we temp set Grey background if it is set to disabled
*/
			if (prop_dlg->m_active==-1)
				fcolor = RGB(125,125,125);
			SetTextColor((HDC)wParam, fcolor);
			SetBkColor((HDC)wParam, bcolor);
			static HBRUSH bh = CreateSolidBrush(bcolor);
			return (LRESULT)bh;
		}
	}
	return false;
}

/***********************************************************************
c
c   FUNCTION: OnRButtonUp(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the user release the right mouse button.
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDDform::OnRButtonUp(UINT nFlags, CPoint point) 
{
	int status = Handle_frame_event(4, nFlags, point);
	if (status==0)
		return;
/*
.....if point outside the selecting area, deselect
*/
	CRect rect = m_selrec;
	rect.top = rect.top - BARLEN - 1;
	rect.bottom = rect.bottom + BARLEN + 1;
	rect.left = rect.left - BARLEN - 1;
	rect.right = rect.right + BARLEN + 1;

	if (!(rect.PtInRect(point)))
	{
		m_sizedir = 0;
		m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
		m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
		for (int i=0; i<8; i++)
		{
			m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
		}
		m_selitem = -1;
		m_selwin = NULL;
		if (m_current_pic!=NULL)
		{
			((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
			m_current_pic = NULL;
			updateWindow_draw(0);
		}
		else
/*
......only update the affect area
*/
			updateWindow_draw(1);
	}
	CWnd::OnRButtonUp(nFlags, point);
}

/***********************************************************************
**
**   FUNCTION: updateWindow_draw(int flag, CRect *orect)
**
**		Updated window by area (avoid paint the whole screen to cause flash)
**   
**	 INPUT:  flag: 0: Updated whole window 
**					1: updated selected window area
**			orect: updated area if flag =2
**
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLDDform::updateWindow_draw(int flag, CRect *orect)
{
	int item_no;
	CRect rect2;
	CRect rect;

	if (flag==0)
	{
		UpdateWindow();
		RedrawWindow();
		m_parent->RedrawWindow();
		m_parent->UpdateWindow();
	}
	if (flag==2)
	{
		rect2 = *orect;
		ScreenToClient(&rect2);
		InvalidateRect(&rect2);
		UpdateWindow();
		rect2 = *orect;
		m_parent->ScreenToClient(&rect2);
		m_parent->InvalidateRect(&rect2);
		m_parent->UpdateWindow();
		return;
	}
	for (int i=0; i<m_update_area; i++)
	{
		item_no = m_selarray[i];
		if (m_seltype[i]==0)
			rect = m_update_rect0[item_no];
		if (m_seltype[i]==1)
			rect = m_update_rect1[item_no];
		if (m_seltype[i]==2)
			rect = m_update_rect2[item_no];
		if ((m_seltype[i]==3)||(m_seltype[i]==4))
		{
			rect = m_update_rect3[item_no];
			if (rect.Width()>0)
			{
				rect = m_update_rect3[item_no];
				rect.left = rect.left - BARLEN - 2;
				rect.right = rect.right + BARLEN + 2;
				rect.top = rect.top - BARLEN - 2;
				rect.bottom = rect.bottom + BARLEN + 2;
				rect2 = rect;
				ScreenToClient(&rect2);
				InvalidateRect(&rect2);
				UpdateWindow();
				rect2 = rect;
				m_parent->ScreenToClient(&rect2);
				m_parent->InvalidateRect(&rect2);
				m_parent->UpdateWindow();
			}
			rect = m_update_rect4[item_no];
		}
		if (m_seltype[i]==5)
			rect = m_update_rect5[item_no];
		if (rect.Width()>0)
		{
			rect.left = rect.left - BARLEN - 2;
			rect.right = rect.right + BARLEN + 2;
			rect.top = rect.top - BARLEN - 2;
			rect.bottom = rect.bottom + BARLEN + 2;
			rect2 = rect;
			ScreenToClient(&rect2);
			InvalidateRect(&rect2);
			UpdateWindow();
			rect2 = rect;
			m_parent->ScreenToClient(&rect2);
			m_parent->InvalidateRect(&rect2);
			m_parent->UpdateWindow();
		}
	}
}
/***********************************************************************
c
c   FUNCTION: OnCheckMacro(int num, int checked)
c
c         This function is called when the macro field checkbox is changed
c
c   INPUT:  num: Macro auto button number
c			checked: if the checkbox is checked
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDDform::OnCheckMacro(int num, int checked)
{
	CNCLFormProp *prop_dlg;
	CRect upt_rect;
	int item = num - 1;
	if (m_macro_win[item]!=NULL)
	{
		prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_macro_win[item]))->GetPropertyPage());
		m_macro_win[item]->GetWindowRect(&upt_rect);
		if (checked==0)
		{
			prop_dlg->m_active = -1;
			m_macro_win[item]->ShowWindow(SW_HIDE);
		}
		else
		{
			prop_dlg->m_active = 1;
			m_macro_win[item]->ShowWindow(SW_SHOW);
		}
		updateWindow_draw(2, &upt_rect);
	}
}
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
void CNCLDDform::OnPaint() 
{
	CWnd::OnPaint();
	CClientDC dc(this);
	if ((m_selnum>0)&&(m_select_draw==1))
	{
		DrawSelectArray(&dc);
	}
	if ((m_selitem!=-1)&&(m_select_draw==1))
		DrawSelect(1);
	if (m_resize==1)
		DrawSizeRect(&dc, m_sizerec, 1);
}

/***********************************************************************
**
**   FUNCTION: IsWindowSelected(CWnd *wnd)
**
**		check if the window is selected
**   
**	 INPUT:  item: item to be deleted
**
**   OUTPUT :   none
**   RETURN:    1: Yes, 0: No
**
**********************************************************************/
int CNCLDDform::IsWindowSelected(CWnd *wnd)
{
	int item_no;
	for (int i=0; i<m_selnum; i++)
	{
		item_no = m_selarray[i];
		if (m_seltype[i]==0)
		{
			if (m_disp_win[item_no]==wnd)
				return 1;
		}
		if (m_seltype[i]==1)
		{
			if (m_frame[item_no]==wnd)
				return 1;
		}
		if (m_seltype[i]==2)
		{
			if (m_picture[item_no]==wnd)
				return 1;
		}
		if (m_seltype[i]==3)
		{
			if (m_dinpwin[item_no]==wnd)
				return 1;
		}
		if (m_seltype[i]==4)
		{
			if (m_inpwin[item_no]==wnd)
				return 1;
		}
		if (m_seltype[i]==5)
		{
			if (m_macro_win[item_no]==wnd)
			{
				return 1;
			}
		}
	}
	return 0;
}

/***********************************************************************
**
**   FUNCTION: DeleteSelItem(int item)
**
**		Delete selected item
**   
**	 INPUT:  item: item to be deleted
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::DeleteSelItem(int item)
{
/*
......deselect the item
*/
	int item_no;
	CRect rect;
	for (int i=item; i<m_selnum-1; i++)
	{
		m_selarray[i] = m_selarray[i+1];
		m_seltype[i] = m_seltype[i+1];
		item_no = m_selarray[i];
		if (m_seltype[i]==0)
		{
			if (m_disp_win[item_no]!=NULL)
			{
				m_disp_win[item_no]->GetWindowRect(&rect);
				m_update_rect0[item_no] = rect;
			}
		}
		if (m_seltype[i]==1)
		{
			if (m_frame[item_no]!=NULL)
			{
				m_frame[item_no]->GetWindowRect(&rect);
				m_update_rect1[item_no] = rect;
			}
		}
		if (m_seltype[i]==2)
		{
			if (m_picture[item_no]!=NULL)
			{
				m_picture[item_no]->GetWindowRect(&rect);
				m_update_rect2[item_no] = rect;
			}
		}
		CRect listrect;
		if (m_seltype[i]==3)
		{
			if (m_dinpwin[item_no]!=NULL)
			{
				m_dinpwin[item_no]->GetWindowRect(&rect);
				m_update_rect3[item_no] = rect;
				if (m_inpwin[item_no]!=NULL)
				{
					m_inpwin[item_no]->GetWindowRect(&rect);
					if(m_inpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDCombo)))
					{
						((CNCLDDCombo*)m_inpwin[item_no])->GetDroppedControlRect(&listrect);
						rect = listrect;
					}
					m_update_rect4[item_no] = rect;
				}
			}
		}
		if (m_seltype[i]==4)
		{
			if (m_inpwin[item_no]!=NULL)
			{
				m_inpwin[item_no]->GetWindowRect(&rect);
				m_update_rect4[item_no] = rect;
				if (m_dinpwin[item_no]!=NULL)
				{
					m_dinpwin[item_no]->GetWindowRect(&rect);
					m_update_rect3[item_no] = rect;
				}
			}
		}
		if (m_seltype[i]==5)
		{
			if (m_macro_win[item_no]!=NULL)
			{
				m_macro_win[item_no]->GetWindowRect(&rect);
				m_update_rect5[item_no] = rect;
			}
		}
	}
	if (m_selnum>0)
		m_selnum--;
	m_update_area = m_selnum;
	if (m_current_pic!=NULL)
	{
		((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
		m_current_pic = NULL;
		updateWindow_draw(0);
	}

	if (m_selnum<=0)
	{
		CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
		pview->SetDtype(-2);
		pview->filldata();
	}
}

/***********************************************************************
c
c   FUNCTION: DrawGuides(CRect rect)
c
c       This function draw Guides line
c
c   INPUT:  CRect rect
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDDform::DrawGuides(CRect rect)
{
	int toggle = ((CNCLFormView*)m_parent)->GetToggle();
	if (toggle<2)
		return;
	updateWindow_draw();
	ScreenToClient(&rect);

	CRect frect;
	GetClientRect(&frect);
	CClientDC dc(this);
	CPen newPen;
	newPen.CreatePen(PS_SOLID, 0, RGB(0,255,0));
	CPen *oldPen = dc.SelectObject(&newPen);
	int px, py;
	px = rect.left;
	py = 0;
	dc.MoveTo(px, py);
	py = rect.top;
	dc.LineTo(px, py);
	
	px = rect.left;
	py = rect.bottom;
	dc.MoveTo(px, py);
	py = frect.bottom;
	dc.LineTo(px, py);
	px = rect.right;
	py = 0;
	dc.MoveTo(px, py);
	py = rect.top;
	dc.LineTo(px, py);
	
	px = rect.right;
	py = rect.bottom;
	dc.MoveTo(px, py);
	py = frect.bottom;
	dc.LineTo(px, py);
	px = 0;
	py = rect.top;
	dc.MoveTo(px, py);
	px = rect.left;
	dc.LineTo(px, py);

	px = rect.right;
	py = rect.top;
	dc.MoveTo(px, py);
	px = frect.right;
	dc.LineTo(px, py);
	px = 0;
	py = rect.bottom;
	dc.MoveTo(px, py);
	px = rect.left;
	dc.LineTo(px, py);

	px = rect.right;
	py = rect.bottom;
	dc.MoveTo(px, py);
	px = frect.right;
	dc.LineTo(px, py);

	dc.SelectObject(oldPen);
    newPen.DeleteObject();

	newPen.CreatePen(PS_SOLID, 2, RGB(255,0,0));
	oldPen = dc.SelectObject(&newPen);
	dc.MoveTo(rect.left, rect.top);
	dc.LineTo(rect.left, rect.bottom);
	dc.LineTo(rect.right, rect.bottom);
	dc.LineTo(rect.right, rect.top);
	dc.LineTo(rect.left, rect.top);
	dc.SelectObject(oldPen);
    newPen.DeleteObject();
}

/***********************************************************************
**
**   FUNCTION: SelectItem(int item_no, int type)
**
**		Select an Item
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::SelectItem(int item_no, int type)
{
	CRect rect;
	int i;
/*
......before we select item
......check if the property page opened
......if yes, save that data and redisplay that item
......then we will fill in new data for the current item
*/
	if ((m_selitem!=item_no)&&(m_seldtyp!=type))
		SaveProperty();
	m_selitem = item_no;
	m_seldtyp = type;
/*
.....check if the item is already in selection array, 
.....if not, add it
*/
	for (i=0; i<m_selnum; i++)
	{
		if ((m_selarray[i]==item_no)&&(m_seltype[i]==type))
			break;
	}
	if (i==m_selnum)
	{
		m_selarray[m_selnum] = item_no;
		m_seltype[m_selnum] = type;
		m_selnum++;
	}
	CNCLFormProp *prop_dlg, *dprop_dlg;
	if (type==0)
	{
		if (m_disp_win[item_no]!=NULL)
		{
			m_disp_win[item_no]->GetWindowRect(&rect);
			m_selwin = m_disp_win[item_no];
			prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_disp_win[item_no]))->GetPropertyPage());
		}
		prop_dlg->m_page = m_disp_secpage[item_no];
	}
	if (type==1)
	{
		if (m_frame[item_no]!=NULL)
		{
			m_frame[item_no]->GetWindowRect(&rect);
			m_selwin = m_frame[item_no];
			prop_dlg = ((CNCLFormProp *)((CNCLDDGroup*)(m_frame[item_no]))->GetPropertyPage());
		}
		prop_dlg->m_page = m_frm_secpage[item_no];
	}
	if (type==2)
	{
		if (m_picture[item_no]!=NULL)
		{
			m_picture[item_no]->GetWindowRect(&rect);
			m_selwin = m_picture[item_no];
			prop_dlg = ((CNCLFormProp *)((CNCLDDPicWin*)(m_picture[item_no]))->GetPropertyPage());
		}
		prop_dlg->m_page = m_pic_secpage[item_no];
	}
	if (type==3)
	{
		if (m_dinpwin[item_no]!=NULL)
		{
			m_dinpwin[item_no]->GetWindowRect(&rect);
			m_selwin = m_dinpwin[item_no];
			if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item_no]))->GetPropertyPage());
			else if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());
		}
		prop_dlg->m_page = m_inp_secpage[item_no];
	}
	if (type==4)
	{
		if (m_inpwin[item_no]!=NULL)
		{
			m_inpwin[item_no]->GetWindowRect(&rect);
			m_selwin = m_inpwin[item_no];
			if ((m_type[item_no]==1)||(m_type[item_no]==25))
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==2)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==3)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==5)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListBox*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==7)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==8)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==9)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==11)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==13)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDProcess*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==15)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==16)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==17)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if ((m_type[item_no]==18)||(m_type[item_no]==23))
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==19)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl2*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==24)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDSlider*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			prop_dlg->m_page = m_inp_secpage[item_no];
			if (!((type==5) ||(type == 19)||(type==17)))
			{
/*
......set prompt
*/
				if (m_dinpwin[item_no]!=NULL)
				{
					if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
						dprop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item_no]))->GetPropertyPage());
					else if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
						dprop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());
					prop_dlg->SetLabelValue(dprop_dlg->m_label, dprop_dlg->m_pcolor);
				}
			}
		}
	}
	if (type==5)
	{
		if (m_macro_win[item_no]!=NULL)
		{
			m_macro_win[item_no]->GetWindowRect(&rect);
			m_selwin = m_macro_win[item_no];
			prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_macro_win[item_no]))->GetPropertyPage());
			prop_dlg->m_page = -1;
		}
	}
	updateWindow_draw(2, &rect);
	OpenPropertyPage(prop_dlg, 0); 
	ScreenToClient(&rect);
	Reset_selrec(rect);
}
/***********************************************************************
**
**   FUNCTION: OnSelectItem(int item_no, int type, int itype, int flag)
**		Select an Item
/*
.....flag = 0, select this item (but reset all other selects)
.....flag = 1, added this item to the selecting array and select this item
.....			ctl key used
.....flag = 2, added/remove this item to/from the selecting array 
.....			but the select item not change if it is not the select item
.....			if it is the select item, it will deselect
.....			shift key used
......if SHIFT SELECT on same item, deselect
**   INPUT:  dc:
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::OnSelectItem(int item_no, int type, int itype, int flag)
{
	int i, selitem;
	int selected = 0;
/*
.....select this item
*/
	if ((flag==0)||(flag==1))
	{
		if (flag==0)
		{
/*
.....if the select mouse is not inside a selected item
.....reset all other selects first
*/
			for (i=0; i<m_selnum; i++)
			{
/*
.....if the item is already there, don't reset select value
*/
				if ((m_selarray[i]==item_no)&&(m_seltype[i]==type))
				{
					selected = 1;
					break;
				}
			}
			if (selected==0)
				reset_selvalue();
		}
		SelectItem(item_no, type);
		m_sizedir = 0;
	}
/*
......added this item to the select array and show it
*/
	int remove = 0;
	for (i=0; i<m_selnum; i++)
	{
/*
.....if the item is already there, if flag is 0 or 1, don't do any thing
.....if flag==2, need deselect the item
*/
		if ((m_selarray[i]==item_no)&&(m_seltype[i]==type))
		{
			if (flag==2)
			{
/*
......remove the selection from window by update this area
*/
				CRect rect;
				if (m_seltype[i]==0)
					rect = m_update_rect0[item_no];
				if (m_seltype[i]==1)
					rect = m_update_rect1[item_no];
				if (m_seltype[i]==2)
					rect = m_update_rect2[item_no];
				if (m_seltype[i]==3)
					rect = m_update_rect3[item_no];
				if (m_seltype[i]==4)
					rect = m_update_rect4[item_no];
				if (m_seltype[i]==5)
					rect = m_update_rect5[item_no];

				rect.left = rect.left - BARLEN - 2;
				rect.right = rect.right + BARLEN + 2;
				rect.top = rect.top - BARLEN - 2;
				rect.bottom = rect.bottom + BARLEN + 2;
				m_select_draw = 0;
				updateWindow_draw(2, &rect);
				m_select_draw = 1;
/*
......if it is select item
*/
				if ((m_selitem==item_no)&&(m_seldtyp==type))
				{
					m_sizedir = 0;
					m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
					m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
					for (int j=0; j<8; j++)
					{
						m_selsize[j].left = m_selsize[j].top = m_selsize[j].right = m_selsize[j].bottom = -1000;
					}
					m_selitem = -1;
					m_selwin = NULL;
				
					selitem = -1;
					if (m_selnum-1>0)
/*
.....select the previous item as select item
*/
					{
						if (i>0)
							selitem = i-1;
						else
							selitem = i;
					}
					DeleteSelItem(i);
					if (selitem>=0)
						SelectItem(m_selarray[selitem], m_seltype[selitem]);
					remove = 1;
				}
/*
......else remove this item from the select array
*/
				else
					DeleteSelItem(i);
			}
			goto done;
		}
	}
	if ((remove==0)&&(flag==2))
	{
		m_selarray[i] = item_no;
		m_seltype[i] = type;
		m_selnum++;
	}
done:;
	updateWindow_draw(1);
}
/***********************************************************************
**
**   FUNCTION: DrawSizeRect(CDC *dc, CRect &rect, int vis)
**		Draw Size Rect
**   INPUT:  dc:
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::DrawSizeRect(CDC *dc, CRect &rect, int vis)
{
	CPen *aPen;
	if (vis==1)
		aPen= new CPen(PS_DOT, 1, RGB(0,0,0));
	else if (vis==2)
		aPen= new CPen(PS_DOT, 1, RGB(255,0,0));
	else
	{
		aPen= new CPen(PS_SOLID, 1, RGB(0,0,0));
	}
/*
.....save old pen and select default pen for draw
*/
	CPen* oldPen = dc->SelectObject(aPen);
	dc->MoveTo(rect.left, rect.top);
	dc->LineTo(rect.left, rect.bottom);
	dc->LineTo(rect.right, rect.bottom);
	dc->LineTo(rect.right, rect.top);
	dc->LineTo(rect.left, rect.top);
	dc->SelectObject(oldPen);
	delete aPen;
}
/***********************************************************************
**
**   FUNCTION: DrawSizeRect(CDC *dc, CRect &rect, int vis)
**		Draw Size Rect
**   INPUT:  dc:
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::DrawHotspotRect(CDC *dc, CRect &rect)
{
	CPen *aPen;
	aPen= new CPen(PS_DOT, 1, RGB(0,0,200));
/*
.....save old pen and select default pen for draw
*/
	CPen* oldPen = dc->SelectObject(aPen);
	dc->MoveTo(rect.left, rect.top);
	dc->LineTo(rect.left, rect.bottom);
	dc->LineTo(rect.right, rect.bottom);
	dc->LineTo(rect.right, rect.top);
	dc->LineTo(rect.left, rect.top);
	dc->SelectObject(oldPen);
	delete aPen;
}

/***********************************************************************
**
**   FUNCTION: DrawSelectArray(CDC *dc)
**		Draw all selection
**   INPUT:  dc:
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::DrawSelectArray(CDC *dc)
{
	if (m_selnum<0)
		return;
	CRect rect, rect2, prect, irect;
	int item_no;
	m_update_area = m_selnum;
	for (int i=0; i<m_selnum; i++)
	{
		item_no = m_selarray[i];
		if (m_seltype[i]==0)
		{
			if (m_disp_win[item_no]!=NULL)
			{
				m_disp_win[item_no]->GetWindowRect(&rect);
				m_update_rect0[item_no] = rect;
			}
		}
		if (m_seltype[i]==1)
		{
			if (m_frame[item_no]!=NULL)
			{
				m_frame[item_no]->GetWindowRect(&rect);
				m_update_rect1[item_no] = rect;
			}
		}
		if (m_seltype[i]==2)
		{
			if (m_picture[item_no]!=NULL)
			{
				m_picture[item_no]->GetWindowRect(&rect);
				m_update_rect2[item_no] = rect;
			}
		}
		if (m_seltype[i]==3)
		{
			if (m_dinpwin[item_no]!=NULL)
			{
/*
......the update area should include input window area, because if delete both delete and need update
*/
				m_dinpwin[item_no]->GetWindowRect(&rect);
				m_update_rect3[item_no] = rect;
				if (m_inpwin[item_no]!=NULL)
				{
					m_inpwin[item_no]->GetWindowRect(&irect);
					if(m_inpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDCombo)))
					{
						((CNCLDDCombo*)m_inpwin[item_no])->GetDroppedControlRect(&irect);
					}
					m_update_rect4[item_no] = irect;
				}
			}
		}
		if (m_seltype[i]==4)
		{
			if (m_inpwin[item_no]!=NULL)
			{
/*
......the update area should include prompt, because if delete both delete and need update
*/
				m_inpwin[item_no]->GetWindowRect(&rect);
				if(m_inpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDCombo)))
				{
					((CNCLDDCombo*)m_inpwin[item_no])->GetDroppedControlRect(&rect);
				}
				m_update_rect4[item_no] = rect;
				if (m_dinpwin[item_no]!=NULL)
				{
					m_dinpwin[item_no]->GetWindowRect(&prect);
					m_update_rect3[item_no] = prect;
				}
			}
		}
		if (m_seltype[i]==5)
		{
			if (m_macro_win[item_no]!=NULL)
			{
				m_macro_win[item_no]->GetWindowRect(&rect);
				m_update_rect5[item_no] = rect;
			}
		}
		ScreenToClient(&rect);
		
		rect2.left = rect.left - BARLEN;
		rect2.right = rect.left;
		rect2.top = rect.top - BARLEN;
		rect2.bottom = rect.top;
		DrawSizeRect(dc, rect2, 3);

		rect2.left = rect.left - BARLEN;
		rect2.right = rect.left;
		rect2.top = rect.top+rect.Height()/2 - BARLEN;
		rect2.bottom = rect2.top + BARLEN;
		DrawSizeRect(dc, rect2, 3);

		rect2.left = rect.left - BARLEN;
		rect2.right = rect.left;
		rect2.top = rect.bottom;
		rect2.bottom = rect2.top + BARLEN;
		DrawSizeRect(dc, rect2, 3);

		rect2.left = rect.left + rect.Width()/2- BARLEN;
		rect2.right = rect2.left + BARLEN;
		rect2.top = rect.bottom;
		rect2.bottom = rect2.top + BARLEN;
		DrawSizeRect(dc, rect2, 3);

		rect2.left = rect.right;
		rect2.right = rect.right + BARLEN;
		rect2.top = rect.bottom;
		rect2.bottom = rect2.top + BARLEN;
		DrawSizeRect(dc, rect2, 3);
	
		rect2.left = rect.right;
		rect2.right = rect.right + BARLEN;
		rect2.top = rect.top + rect.Height()/2 - BARLEN;
		rect2.bottom = rect2.top + BARLEN;
		DrawSizeRect(dc, rect2, 3);
	
		rect2.left = rect.right;
		rect2.right = rect.right + BARLEN;
		rect2.top = rect.top - BARLEN;
		rect2.bottom = rect2.top + BARLEN;
		DrawSizeRect(dc, rect2, 3);
	
		rect2.left = rect.left  + rect.Width()/2- BARLEN;
		rect2.right = rect2.left + BARLEN;
		rect2.top = rect.top - BARLEN;
		rect2.bottom = rect.top;
		DrawSizeRect(dc, rect2, 3);
	}
}
/***********************************************************************
**
**   FUNCTION: DrawSelect(int focus)
**
**       drawing the cell as select
**
**   INPUT:  CDC* pDC: device context
**				focus: if the cell have the focus
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::DrawSelect(int focus)
{
	CClientDC dc(this);
	CBrush* pBrush;
	pBrush = new CBrush(RGB(0, 0, 0));

	CRect rect;
	dc.FillRect(&m_selsize[1], pBrush );
	dc.FillRect(&m_selsize[5], pBrush );
	if ((m_dropdown==0)||(m_dropdown==2))
	{
		dc.FillRect(&m_selsize[0], pBrush );
		dc.FillRect(&m_selsize[2], pBrush );
		dc.FillRect(&m_selsize[3], pBrush );
		dc.FillRect(&m_selsize[4], pBrush );
		dc.FillRect(&m_selsize[6], pBrush );
		dc.FillRect(&m_selsize[7], pBrush );
		delete pBrush;
	}
	else
	{
		delete pBrush;
		rect = m_selsize[0];
		DrawSizeRect(&dc, rect, 3);
		rect = m_selsize[2];
		DrawSizeRect(&dc, rect, 3);
		rect = m_selsize[3];
		DrawSizeRect(&dc, rect, 3);
		rect = m_selsize[4];
		DrawSizeRect(&dc, rect, 3);
		rect = m_selsize[6];
		DrawSizeRect(&dc, rect, 3);
		rect = m_selsize[7];
		DrawSizeRect(&dc, rect, 3);
	}
}

/***********************************************************************
c
c   FUNCTION: HitTest( CPoint point )
c
c       Call this function to find out if the point is within a selectbox
c			or selectbox sizebar
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of point need to be tested. 
c
c   OUTPUT :   None
c   RETURN:    HT_NONE: 
c				HT_INSIDE (inside the select box)
c				HT_TOPLEFT (topleft sizebar)
c				HT_LEFT 
c				HT_BOTTOMLEFT
c				HT_BOTTOM
c				HT_BOTTOMRIGHT
c				HT_RIGHT
c				HT_TOPRIGHT
c				HT_TOP
c
**********************************************************************/
int CNCLDDform::HitTest(CPoint point)
{
	int ret;
	if (m_selrec.top == -1000)
		return HT_NONE;
	CRect arrow_rect;
	CRect rect1, rect2, rect3, rect4;
		
	ret = HT_NONE;
	if (m_seldtyp==1)
	{
		rect1.top = m_selrec.top;
		rect1.bottom = rect1.top + 12;
		rect1.left = m_selrec.left;
		rect1.right = m_selrec.right;

		rect2.top = m_selrec.bottom-4;
		rect2.bottom = rect2.top + 8;
		rect2.left = m_selrec.left;
		rect2.right = m_selrec.right;

		rect3.top = m_selrec.top;
		rect3.bottom = m_selrec.bottom;
		rect3.left = m_selrec.left - 4;
		rect3.right = rect3.left + 8;

		rect4.top = m_selrec.top;
		rect4.bottom = m_selrec.bottom;
		rect4.left = m_selrec.right - 4;
		rect4.right = rect4.left + 8;
		if (rect1.PtInRect(point))
			ret = HT_INSIDE;
		if (rect2.PtInRect(point))
			ret = HT_INSIDE;
		if (rect3.PtInRect(point))
			ret = HT_INSIDE;
		if (rect4.PtInRect(point))
			ret = HT_INSIDE;
	}
	else if (m_selrec.PtInRect(point))
	{
		arrow_rect = m_selrec;
		arrow_rect.left = arrow_rect.right - 20;
		if ((m_dropdown)&&(arrow_rect.PtInRect(point)))
			ret = HT_UPDOWN;
		ret = HT_INSIDE;
	}
	else if ((m_current_pic!=NULL)&&(m_hotspot_rect.PtInRect(point)))
	{
		ret = HT_INSIDE;
	}
	if (m_selsize[1].PtInRect(point))
		ret = HT_LEFT;
	if (m_selsize[5].PtInRect(point))
		ret = HT_RIGHT;
	if ((m_dropdown==0)||(m_dropdown==2))
	{
		if (m_selsize[0].PtInRect(point))
			ret = HT_TOPLEFT;
		if (m_selsize[2].PtInRect(point))
			ret = HT_BOTTOMLEFT;
		if (m_selsize[3].PtInRect(point))
			ret = HT_BOTTOM;
		if (m_selsize[4].PtInRect(point))
			ret = HT_BOTTOMRIGHT;
		if (m_selsize[6].PtInRect(point))
			ret = HT_TOPRIGHT;
		if (m_selsize[7].PtInRect(point))
			ret = HT_TOP;
	}
	return ret;
}


/***********************************************************************
c
c   FUNCTION: OnLButtonDown(UINT nFlags, CPoint point) 
c
c			Callback function for left mouse button down
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDDform::OnLButtonDown(UINT nFlags, CPoint point) 
{
	int status = Handle_frame_event(1, nFlags, point);
	if (status==0)
		return;
/*
.....if point outside the selecting area, deselect
*/
	CRect rect = m_selrec;
	rect.top = rect.top - BARLEN - 1;
	rect.bottom = rect.bottom + BARLEN + 1;
	rect.left = rect.left - BARLEN - 1;
	rect.right = rect.right + BARLEN + 1;
/*
.....check if the cursor is inside valid area, otherwise
.....reset seelction
*/
	int ret1 = is_valid_area(rect, point);
	int ret2 = is_valid_area(m_sizerec, point);

	if ((ret1==0)&&(ret2==0)&&(m_hittest==0))
	{
		m_sizedir = 0;
		m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
		m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
		for (int i=0; i<8; i++)
		{
			m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
		}
		m_selitem = -1;
		m_selwin = NULL;
		if (m_current_pic!=NULL)
		{
			((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
			m_current_pic = NULL;
			updateWindow_draw(0);
		}
		else
			updateWindow_draw(1);
	}
	CWnd::OnLButtonDown(nFlags, point);
	HandleButtonDown(nFlags, point, m_dropdown);
}

/***********************************************************************
**
**   FUNCTION: HandleButtonDown(UINT nFlags, CPoint point, int dropdown)
**		Handle mouse Button Down event
**   INPUT:  nFlags: Indicates whether various virtual keys are Down
**			point:  Specifies the x- and y-coordinate of the cursor. 
**					These coordinates are always relative to the 
**					upper-left corner of the window.
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::HandleButtonDown(UINT nFlags, CPoint point, int dropdown)
{
	RECT listrect;
	m_StartPoint = 	point;
	m_buttondown = 1;	
	m_sizedir = m_hittest;
	if ((dropdown==0)&&(m_sizedir==0))
	{
		m_dropdown = 0;
	}
	if (m_selwin!=NULL)
	{
		m_selwin->GetWindowRect(&m_sizerec);
		ScreenToClient(&m_sizerec);
		if (dropdown==2)
		{
			if ((m_sizedir==0)||(m_sizedir==10))
			{
				m_dropdown = 2;
				((CNCLDDCombo*)(m_selwin))->GetDroppedControlRect(&listrect);
				m_sizerec.bottom = m_sizerec.top + (listrect.bottom - listrect.top);
			}
			else
			{
				if (m_dropdown==1)
				{
					((CNCLDDCombo*)(m_selwin))->GetWindowRect(&m_sizerec);
					ScreenToClient(&m_sizerec);
				}
				else
				{
					((CNCLDDCombo*)(m_selwin))->GetDroppedControlRect(&listrect);
					m_sizerec.bottom = m_sizerec.top + (listrect.bottom - listrect.top);
				}
			}
		}
		else if (dropdown==1)
			m_dropdown = 1;
		else 
			m_dropdown = 0;
		SetCursor(m_cursor[m_hittest]);
	}
}

/***********************************************************************
c
c   FUNCTION: OnLButtonUp(UINT nFlags, CPoint point)
c			Callback function for left mouse button up
c
c   INPUT:  nFlags: Indicates whether various virtual keys are down.
c					MK_CONTROL   Set if the CTRL key is down.
c					MK_SHIFT   Set if the SHIFT key is down.
c			point:  Specifies the x- and y-coordinate of the cursor.
c
c   OUTPUT :   None.
c   RETURN:    None.
c
**********************************************************************/
void CNCLDDform::OnLButtonUp(UINT nFlags, CPoint point) 
{
	int status = Handle_frame_event(2, nFlags, point);
	if (status==0)
		return;
	CWnd::OnLButtonUp(nFlags, point);
	HandleButtonup(nFlags, point);
}
/***********************************************************************
**
**   FUNCTION: HandleButtonup(UINT nFlags, CPoint point, int dropdown)
**		Handle mouse Button Up event
**   INPUT:  nFlags: Indicates whether various virtual keys are Down
**			point:  Specifies the x- and y-coordinate of the cursor. 
**					These coordinates are always relative to the 
**					upper-left corner of the window.
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::HandleButtonup(UINT nFlags, CPoint point, int dropdown)
{
	RECT listrect;
	CRect sizerec, rectClient;
	int hgt;
	CNCLFormProp *prop_dlg = ((CNCLFormView*)m_parent)->GetPropertyPage();
/*
......resize the select item there is one
*/	
	m_resize = 0;
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;
	m_buttondown = 0;	
/*
......if no combobox or not click on combobox arrow
*/
	if  (m_selwin!=NULL)
	{
		if (!(m_selwin->IsKindOf(RUNTIME_CLASS(CNCLDDCombo))))
			m_dropdown = 0;
	}
	else
		m_dropdown = 0;

	if ((m_selwin!=NULL)&&((m_sizedir!=HT_NONE)&&(m_sizedir!=HT_INSIDE)&&(m_sizedir!=HT_UPDOWN))||(m_dropdown))
	{
		int sizing = 0;
		CRect old_rect;
		m_selwin->GetWindowRect(&old_rect);
		old_rect.left = old_rect.left - BARLEN - 2;
		old_rect.right = old_rect.right + BARLEN + 2;
		old_rect.top = old_rect.top - BARLEN - 2;
		old_rect.bottom = old_rect.bottom + BARLEN + 2;
/*
......save undo resizing
*/
		SaveUndoItem2(4);

		if ((m_sizedir!=HT_NONE)&&(m_sizedir!=HT_INSIDE)&&(m_sizedir!=HT_UPDOWN))
			sizing = 1;

		if (m_dropdown==0)
		{
			m_selwin->MoveWindow(&m_sizerec);
			m_selrec = m_sizerec;
		}
		else if (m_dropdown==2)
		{
			((CNCLDDCombo*)(m_selwin))->MoveWindow(&m_sizerec);
			m_selwin->GetWindowRect(&m_selrec);
			ScreenToClient(&m_selrec);
		}
		else
		{
			m_selwin->GetWindowRect(&sizerec);
			hgt = sizerec.Height();
			sizerec = m_sizerec;
			sizerec.bottom = m_sizerec.top + hgt;
			m_selwin->MoveWindow(&sizerec);
			m_selrec = m_sizerec;
		}
		rectClient = m_sizerec;
/*
......set size
*/	
		if (m_selwin->IsKindOf(RUNTIME_CLASS(CNCLDDCombo)))
		{
			CRect listrect;
			((CNCLDDCombo*)(m_selwin))->GetDroppedControlRect(&listrect);
			prop_dlg->m_size[0] = listrect.right - listrect.left;
			prop_dlg->m_size[1] = listrect.bottom - listrect.top;
		}
		else
		{
			prop_dlg->m_size[0] = rectClient.right - rectClient.left;
			prop_dlg->m_size[1] = rectClient.bottom - rectClient.top;
		}
/*
......set pos
*/	
		prop_dlg->m_pos[0] = rectClient.left;
		prop_dlg->m_pos[1] = rectClient.top;
		((CNCLFormView*)m_parent)->UpdatePropertySize(prop_dlg->m_size[0], prop_dlg->m_size[1]);
		CRect rect = m_sizerec;
/*
.....left-top
*/
		m_selsize[0].left = rect.left - BARLEN;
		m_selsize[0].right = rect.left;
		m_selsize[0].top = rect.top - BARLEN;
		m_selsize[0].bottom = rect.top;
/*
.....left-middle
*/	
		m_selsize[1].left = rect.left - BARLEN;
		m_selsize[1].right = rect.left;
		m_selsize[1].top = rect.top+rect.Height()/2 - BARLEN;
		m_selsize[1].bottom = m_selsize[1].top + BARLEN;
/*
.....left-bottom
*/	
		m_selsize[2].left = rect.left - BARLEN;
		m_selsize[2].right = rect.left;
		m_selsize[2].top = rect.bottom;
		m_selsize[2].bottom = m_selsize[2].top + BARLEN;
/*
.....-middle-bottom
*/	
		m_selsize[3].left = rect.left + rect.Width()/2- BARLEN;
		m_selsize[3].right = m_selsize[3].left + BARLEN;
		m_selsize[3].top = rect.bottom;
		m_selsize[3].bottom = m_selsize[3].top + BARLEN;
/*
.....right-bottom
*/	
		m_selsize[4].left = rect.right;
		m_selsize[4].right = rect.right + BARLEN;
		m_selsize[4].top = rect.bottom;
		m_selsize[4].bottom = m_selsize[4].top + BARLEN;
/*
.....right-middle
*/	
		m_selsize[5].left = rect.right;
		m_selsize[5].right = rect.right + BARLEN;
		m_selsize[5].top = rect.top + rect.Height()/2 - BARLEN;
		m_selsize[5].bottom = m_selsize[5].top + BARLEN;
/*
.....right-top
*/	
		m_selsize[6].left = rect.right;
		m_selsize[6].right = rect.right + BARLEN;
		m_selsize[6].top = rect.top - BARLEN;
		m_selsize[6].bottom = m_selsize[6].top + BARLEN;
/*
.....middle-top
*/	
		m_selsize[7].left = rect.left  + rect.Width()/2- BARLEN;
		m_selsize[7].right = m_selsize[7].left + BARLEN;
		m_selsize[7].top = rect.top - BARLEN;
		m_selsize[7].bottom = rect.top;
	
		DrawSelect(1);
		updateWindow_draw(1);
		if (sizing)
		{
			CRect upt_rect = m_sizerec;
			ClientToScreen(&upt_rect);
			upt_rect.left = upt_rect.left - BARLEN - 2;
			upt_rect.right = upt_rect.right + BARLEN + 2;
			upt_rect.top = upt_rect.top - BARLEN - 2;
			upt_rect.bottom = upt_rect.bottom + BARLEN + 2;
			updateWindow_draw(2, &upt_rect);
			updateWindow_draw(2, &old_rect);		
			return;
		}
	}
	int flag;
	if (nFlags&MK_CONTROL)
		flag = 1;
	else if (nFlags&MK_SHIFT)
		flag = 2;
	else
		flag = 0;
	if (flag==0)
	{
/*
......if the m_StartPoint and point is all outside the selecting window
......the unselect the window item
*/
		CRect rect1;
		rect1 = m_selrec;
		rect1.left--;
		rect1.right++;
		rect1.top--;
		rect1.bottom++;
		int sel_groupbox;
		sel_groupbox = is_frame_clicked(point);

		if (((!(rect1.PtInRect(point)))
			&& (!(rect1.PtInRect(m_StartPoint)))) 
			|| ((sel_groupbox==1)&&(rect1.PtInRect(point)==0)))
		{
			m_sizedir = 0;
			m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
			m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
			for (int i=0; i<8; i++)
			{
				m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
			}
			m_selitem = -1;
			m_selwin = NULL;
			m_dropdown = 0;
			if (m_current_pic!=NULL)
			{
				((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
				m_current_pic = NULL;
			}
		}
/*
......reset all other select if it is not dragging, if it is dragging, this
......event will not called, dragging event will eat it
*/
		m_select_draw = 0;
		updateWindow_draw(1);
		m_select_draw = 1;

		for (int i=0; i<m_selnum;i++)
		{
			m_selarray[i] = -1;
			m_seltype[i] = -1;
		}

		m_selarray[0] = m_selitem;
		m_seltype[0] = m_seldtyp;
		if (m_selitem!=-1)
		{
			m_selnum = 1;
			m_update_area = 1;
		}
		else
		{
			m_selnum = 0;
			m_update_area = 0;
			CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
			pview->SetDtype(-2);
			pview->filldata();
		}
		updateWindow_draw(1);
	}
	if (m_selnum<=1)
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(0);
	}
	else
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(1);
	}
}
/***********************************************************************
**
**   FUNCTION: DrawAutoWindowFrame(CDC *dc, int macro_num)
**
**		draw Selected auto Window Frame
**   
**	 INPUT:  dc:
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::DrawAutoWindowFrame(CDC *dc, int macro_num)
{
	CRect rect;
	m_macro_win[macro_num]->GetWindowRect(&rect);
	ScreenToClient(&rect);
	DrawSizeRect(dc, rect, 2);
}

void CNCLDDform::DrawHotspotFrame(CDC *dc, int item_no)
{
	CRect rect = m_hotspot_rect;
	CRect prect;
	m_picture[item_no]->GetWindowRect(&prect);
	ScreenToClient(&prect);
/*
......can't shift the points when drawing since it will draw outline,
......need adjust the drop point when dragging
*/
//	int dx = m_hotspot_rect.left - prect.left;
//	int dy = m_hotspot_rect.top - prect.top;
	int dx = 0;
	int dy = 0;
	rect.left += dx;
	rect.right += dx;
	rect.top += dy;
	rect.bottom += dy;
	DrawSizeRect(dc, rect, 2);
}

/***********************************************************************
**
**   FUNCTION: DrawSelectWindowFrame(CDC *dc)
**
**		draw Selected Window Frame
**   
**	 INPUT:  dc:
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::DrawSelectWindowFrame(CDC *dc)
{
	if (m_selnum<1)
		return;
	CRect rect;
	int item_no;
	for (int i=0; i<m_selnum; i++)
	{
		item_no = m_selarray[i];
		if (m_seltype[i]==0)
		{
			if (m_disp_win[item_no]!=NULL)
			{
				m_disp_win[item_no]->GetWindowRect(&rect);
			}
		}
		if (m_seltype[i]==1)
		{
			if (m_frame[item_no]!=NULL)
			{
				m_frame[item_no]->GetWindowRect(&rect);
			}
		}
		if (m_seltype[i]==2)
		{
			if (m_picture[item_no]!=NULL)
			{
				m_picture[item_no]->GetWindowRect(&rect);
			}
		}
		if (m_seltype[i]==3)
		{
			if (m_dinpwin[item_no]!=NULL)
			{
				m_dinpwin[item_no]->GetWindowRect(&rect);
			}
		}
		if (m_seltype[i]==4)
		{
			if (m_inpwin[item_no]!=NULL)
			{
				m_inpwin[item_no]->GetWindowRect(&rect);
			}
		}
		if (m_seltype[i]==5)
		{
			if (m_macro_win[item_no]!=NULL)
			{
				m_macro_win[item_no]->GetWindowRect(&rect);
			}
		}
		ScreenToClient(&rect);
		DrawSizeRect(dc, rect, 2);
	}
}
/*************************************************************************
**
**  Function:  CopyWindowToBitmap(LPRECT lpRect, CWnd* pWnd)
**		Copy Window to a Bitmap
**		
**			 .
**	 INPUT: CWnd *pWnd
**			lpRect: Window's rect
**			
**   OUTPUT : none
**   RETURN:   Bitmap handle
**
**
*************************************************************************/
HBITMAP CNCLDDform::CreateAllToBitmap(int *cx, int *cy, int macro_num)
{
	HDC hMemDC,hMemDC2, hMemDC3, hMemDC4;
	HBITMAP hBitmap, hOldBitmap, hBitmap2, hBitmap3, hBitmap4,hOldBitmap2;
	int nX, nY, nX2, nY2;
	int nWidth, nHeight; 
	int xScrn, yScrn,i,j;

	CRect lpRect;
	GetClientRect(&lpRect);

	CPaintDC dc(this);

	hMemDC = CreateCompatibleDC(dc.m_hDC);
/* 
......get points of rectangle to grab 
*/
	nX = lpRect.left;
	nY = lpRect.top;
	nX2 = lpRect.right;
	nY2 = lpRect.bottom;
/* 
......get screen resolution 
*/
	xScrn = GetDeviceCaps(dc.m_hDC, HORZRES);
	yScrn = GetDeviceCaps(dc.m_hDC, VERTRES);

	if (nX < 0)
		nX = 0;
	if (nY < 0)
		nY = 0;
	if (nX2 > xScrn)
		nX2 = xScrn;
	if (nY2 > yScrn)
		nY2 = yScrn;

	nWidth = nX2 - nX;
	nHeight = nY2 - nY;

	hBitmap = CreateCompatibleBitmap(dc.m_hDC, nWidth, nHeight);
	hOldBitmap = (HBITMAP)SelectObject(hMemDC, hBitmap);

	*cx = nWidth;
	*cy = nHeight;
	BitBlt(hMemDC, 0, 0, nWidth, nHeight, dc.m_hDC, nX, nY, BLACKNESS) ;
	CDC *dc2 = CDC::FromHandle(hMemDC);
/*
.....draw all select window
*/
	if (UW_form_hotspot_item==0)
		DrawSelectWindowFrame(dc2);
	else
		DrawHotspotFrame(dc2, macro_num); //macro_num is picture item number here

	hBitmap = (HBITMAP)SelectObject(hMemDC, hOldBitmap);
	DeleteDC(hMemDC);
	return hBitmap;
}


CImageList * CNCLDDform::GetDragAllImage()
{
/*	CImageList *list = new CImageList();		
	list->Create(300, 300, ILC_COLOR32|ILC_MASK, 0, 10);
	CRect rectClient;
	GetClientRect(&rectClient);
	HBITMAP bitmap = CreateAllToBitmap(&rectClient, this);
	CBitmap *defImage, defcBmap;
	defImage = defcBmap.FromHandle(bitmap);
	list->Add(defImage, 0xC0C0C0);
	return list;
*/
	return NULL;
}

/***********************************************************************
c
c   FUNCTION: OnMouseMove(UINT nFlags, CPoint point) 
c
c       Tthis member function will be called
c			when the mouse cursor moves on this control
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDDform::OnMouseMove(UINT nFlags, CPoint point) 
{
	int status = Handle_frame_event(5, nFlags, point);
	if (status==0)
		return;
	CWnd::OnMouseMove(nFlags, point);
	HandleMouseMove(nFlags, point);
}
/***********************************************************************
**
**   FUNCTION: HandleMouseMove(UINT nFlags, CPoint point)
**		Handle mouse move event
**   INPUT:  nFlags: Indicates whether various virtual keys are Down
**			point:  Specifies the x- and y-coordinate of the cursor. 
**					These coordinates are always relative to the 
**					upper-left corner of the window.
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::HandleMouseMove(UINT nFlags, CPoint point)
{
	if ((m_buttondown==1)&&(m_sizedir!=HT_NONE)&&(m_sizedir!=HT_INSIDE)&&(m_sizedir!=HT_UPDOWN))
	{
		SetCursor(m_cursor[m_sizedir]);
	}
	else
//	if (m_buttondown==0)
	{
		m_hittest = HitTest(point);
		switch (m_hittest)
		{
		case HT_NONE:
		case HT_INSIDE:
		case HT_TOPLEFT:
		case HT_LEFT:
		case HT_BOTTOMLEFT:
		case HT_BOTTOM:
		case HT_BOTTOMRIGHT:
		case HT_RIGHT:
		case HT_TOPRIGHT:
		case HT_TOP:
		case HT_UPDOWN:
			SetCursor(m_cursor[m_hittest]);
		}
	}
	if (m_sizerec.top==-1000)
		return;

	CRect rect = m_sizerec;
	if (m_buttondown==1)
	{
		if (m_sizedir==HT_LEFT)
		{
			rect.left = point.x;
		}
		if (m_sizedir==HT_TOPLEFT)
		{
			rect.left = point.x;
			rect.top = point.y;
		}
		if (m_sizedir==HT_BOTTOMLEFT)
		{
			rect.left = point.x;
			rect.bottom = point.y;
		}
		if (m_sizedir==HT_BOTTOM)
		{
			rect.bottom = point.y;
		}
		if (m_sizedir==HT_BOTTOMRIGHT)
		{
			rect.right = point.x;
			rect.bottom = point.y;
		}
		if (m_sizedir==HT_RIGHT)
		{
			rect.right = point.x;
		}
		if (m_sizedir==HT_TOPRIGHT)
		{
			rect.right = point.x;
			rect.top = point.y;
		}
		if (m_sizedir==HT_TOP)
		{
			rect.top = point.y;
		}
		if (m_sizerec!=rect)
		{
			CRect upt_rect = m_sizerec;
			m_resize = 1;
			m_sizerec = rect;
			ClientToScreen(&upt_rect);
			upt_rect.left = upt_rect.left - BARLEN - 2;
			upt_rect.right = upt_rect.right + BARLEN + 2;
			upt_rect.top = upt_rect.top - BARLEN - 2;
			upt_rect.bottom = upt_rect.bottom + BARLEN + 2;
			updateWindow_draw(2, &upt_rect);
			upt_rect = m_sizerec;
			ClientToScreen(&upt_rect);
			upt_rect.left = upt_rect.left - BARLEN - 2;
			upt_rect.right = upt_rect.right + BARLEN + 2;
			upt_rect.top = upt_rect.top - BARLEN - 2;
			upt_rect.bottom = upt_rect.bottom + BARLEN + 2;
			updateWindow_draw(2, &upt_rect);
		}
	}
}
/***********************************************************************
**
**   FUNCTION: OnTimer() 
**
**		this function will timely check if the mouse point is on the 
**		item, if not reset the time
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::OnTimer(UINT_PTR nIDEvent) 
{
	CWnd::OnTimer(nIDEvent);
}

/***********************************************************************
**
**   FUNCTION: Delete_dispwin(int item_no, int sel)
**		delete an label item
**	 INPUT:  item_no:
**			sel: 1: item selected
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::Delete_dispwin(int item_no, int sel)
{
/*
......we need update the selecting list, no delete of select item
*/
	if (sel==1)
	{
		for (int j=0; j<m_selnum;j++)
		{
			if ((m_selarray[j]>=item_no)&&(m_seltype[j]==0))
			{
				m_selarray[j] = m_selarray[j] - 1;
				break;
			}
		}
	}
	for (int i=item_no; i<m_dispno-1; i++)
	{
		m_disp_win[i] = m_disp_win[i+1];
		m_disp_secpage[i] = m_disp_secpage[i+1];
		if (m_dinpwin[i]!=NULL)
			((CNCLDDStatic*)m_disp_win[i])->SetItemNo(i);
	}
	m_dispno--;
	m_disp_win[m_dispno] = NULL;
}
	
/***********************************************************************
**
**   FUNCTION: Delete_frmwin(int item_no, int sel)
**		delete an frame item
**	 INPUT:  item_no:
**			sel: 1: item selected
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::Delete_frmwin(int item_no, int sel)
{
	if (sel==1)
	{
		for (int j=0; j<m_selnum;j++)
		{
			if ((m_selarray[j]>=item_no)&&(m_seltype[j]==1))
			{
				m_selarray[j] = m_selarray[j] - 1;
				break;
			}
		}
	}
	for (int i=item_no; i<m_frameno-1; i++)
	{
		m_frame[i] = m_frame[i+1];
		m_frm_secpage[i] = m_frm_secpage[i+1];
		if (m_frame[i]!=NULL)
			((CNCLDDGroup*)m_frame[i])->SetItemNo(i);
	}
	m_frameno--;
	m_frame[m_frameno] = NULL;
}
/***********************************************************************
**
**   FUNCTION: Delete_picwin(int item_no, int sel)
**		delete an picture item
**	 INPUT:  item_no:
**			sel: 1: item selected
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::Delete_picwin(int item_no, int sel)
{
	if (sel==1)
	{
		for (int j=0; j<m_selnum;j++)
		{
			if ((m_selarray[j]>=item_no)&&(m_seltype[j]==2))
			{
				m_selarray[j] = m_selarray[j] - 1;
				break;
			}
		}
	}
	for (int i=item_no; i<m_picno-1; i++)
	{
		m_picture[i] = m_picture[i+1];
		m_pic_secpage[i] = m_pic_secpage[i+1];
		if (m_picture[i]!=NULL)
			((CNCLDDPicWin*)m_picture[i])->SetItemNo(i);
	}
	m_picno--;
	m_picture[m_picno] = NULL;
}
/***********************************************************************
**
**   FUNCTION: Delete_inpwin(int item_no, int sel)
**		delete an input item
**	 INPUT:  item_no:
**			sel: 1: item selected
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::Delete_inpwin(int item_no, int sel)
{
	int i,j,type;
	if (sel==1)
	{
		for (j=0; j<m_selnum;j++)
		{
			if ((m_selarray[j]>=item_no)&&
				((m_seltype[j]==3)||(m_seltype[j]==4)))
			{
				m_selarray[j] = m_selarray[j] - 1;
				break;
			}
		}
	}
	for (i=item_no; i<m_inputno-1; i++)
	{
		m_dinpwin[i] = m_dinpwin[i+1];
		m_inpwin[i] = m_inpwin[i+1];
		m_type[i] = m_type[i+1];
		m_inp_secpage[i] = m_inp_secpage[i+1];
		type = m_type[i];
		if (m_dinpwin[i]!=NULL)
		{
			if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
				((CNCLDDStatic*)(m_dinpwin[i]))->SetItemNo(i);
			else if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
				((CNCLDDButton*)(m_dinpwin[i]))->SetItemNo(i);
		}
		if (m_inpwin[i]==NULL)
			continue;
		if ((type==1)||(type==25))
		{
			((CNCLDDButton*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==2)
		{
			((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==3)
		{
			((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==5)
		{
			((CNCLDDListBox*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==7)
		{
			((CNCLDDButton*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==8)
		{
			((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==9)
		{
			((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==11)
		{
			((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==13)
		{
			((CNCLDDProcess*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==15)
		{
			((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==16)
		{
			((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==17)
		{
			((CNCLDDListCtrl*)m_inpwin[i])->SetItemNo(i);
		}
		else if ((type==18)||(type==23))
		{
			((CNCLDDColorButton*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==19)
		{
			((CNCLDDListCtrl2*)m_inpwin[i])->SetItemNo(i);
		}
		else if (type==24)
		{
			((CNCLDDSlider*)m_inpwin[i])->SetItemNo(i);
		}
	}
	m_inputno--;
	m_dinpwin[m_inputno] = NULL;
	m_inpwin[m_inputno] = NULL;					
	CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
	pview->SetFormInputNo(m_inputno);
	pview->SetFormSecNo(m_secno);
}
void CNCLDDform::UpdateWinProp(CNCLWinInfo* pItem, CNCLWinInfo* out_pItem, int dtype, int action)
{
	int itemno = pItem->m_input_itemno;
	int itype = pItem->m_itype;
/*
.....copy pItem to out_pItem
*/
	out_pItem->CopyInfo(pItem);
	CNCLFormProp *page;
	if (dtype==0)
	{
		page = ((CNCLDDStatic*)m_disp_win[itemno])->GetPropertyPage();
	}
	else if (dtype==1)
	{
		page = ((CNCLDDGroup*)m_frame[itemno])->GetPropertyPage();
	}
	else if (dtype==2)
	{
		page = ((CNCLDDPicWin*)m_picture[itemno])->GetPropertyPage();
	}
	else if (dtype==3)
	{
		if (m_input[itemno]==FORM_STRING)
		{
			page = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[itemno]))->GetPropertyPage());
		}
		else
		{
			page = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[itemno]))->GetPropertyPage());			
		}
	}
	else if (dtype==4)
	{
		if ((itype==1)||(itype==25))
		{
			page = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==2)
		{
			page = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==3)
		{
			page = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==5)
		{
			page = ((CNCLFormProp *)((CNCLDDListBox*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==7)
		{
			page = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==8)
		{
			page = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==9)
		{
			page = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==11)
		{
			page = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==13)
		{
			page = ((CNCLFormProp *)((CNCLDDProcess*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==15)
		{
			page = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==16)
		{
			page = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==17)
		{
			page = ((CNCLFormProp *)((CNCLDDListCtrl*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==18)
		{
			page = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==19)
		{
			page = ((CNCLFormProp *)((CNCLDDListCtrl2*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==23)
		{
			page = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[itemno]))->GetPropertyPage());
		}
		else if (itype==24)
		{
			page = ((CNCLFormProp *)((CNCLDDSlider*)(m_inpwin[itemno]))->GetPropertyPage());
		}
	}
	if ((action==2)||(action==4))
	{
		out_pItem->m_pos[0] = page->m_pos[0];
		out_pItem->m_pos[1] = page->m_pos[1];
		out_pItem->m_size[0] = page->m_size[0];
		out_pItem->m_size[1] = page->m_size[1];
	}
	else if (action==5)
	{
		out_pItem->m_label = page->m_label;
	}
	else if (action==6)
	{
		out_pItem->m_font = page->m_font;
	}
	else if (action==7)
	{
		out_pItem->m_input = page->m_input;
		out_pItem->m_justified = page->m_justified;
	}
	else if (action==8)
	{
		out_pItem->m_input_itemno = page->m_input_itemno;
	}
	else if (action==9)
	{
		out_pItem->m_choices = page->m_choices;
	}
	else if (action==10)
	{
		out_pItem->m_color = page->m_color;
		out_pItem->m_pcolor = page->m_pcolor;
	}
	else if (action==11)
	{
		out_pItem->m_page = page->m_page;
	}
	else if (action==12)
	{
		out_pItem->m_len = page->m_len;
	}
	else if (action==13)
	{
		out_pItem->m_active = page->m_active;
	}
	else if (action==14)
	{
		out_pItem->m_limit = page->m_limit;
	}
	else if (action==15)
	{
		out_pItem->m_prec = page->m_prec;
	}
	else if (action==16)
	{
		out_pItem->m_range[0] = page->m_range[0];
		out_pItem->m_range[1] = page->m_range[1];
	}
	else if ((action==17)||(action==17))
	{
		out_pItem->m_picarea_no = page->m_picarea_no;
		out_pItem->m_pic_act_area = page->m_pic_act_area;
		int k;
		if ((page->m_picarea_no>0)&&(page->m_picarea!=NULL))
		{
			out_pItem->m_picarea = (UD_PICAREA *) uu_malloc(page->m_picarea_no*sizeof(UD_PICAREA));
			for (k=0; k<page->m_picarea_no; k++)
			{
				if (page->m_picarea[k].params!=NULL)
				{
					out_pItem->m_picarea[k].params = (char*) uu_malloc((100)*sizeof(char));
					strcpy(out_pItem->m_picarea[k].params, page->m_picarea[k].params);
				}
				else
					out_pItem->m_picarea[k].params = NULL;
				if (page->m_picarea[k].tooltext!=NULL)
				{
					out_pItem->m_picarea[k].tooltext = (char*) uu_malloc((100)*sizeof(char));
					strcpy(out_pItem->m_picarea[k].tooltext, page->m_picarea[k].tooltext);
				}
				else
					out_pItem->m_picarea[k].tooltext = NULL;
				strcpy(out_pItem->m_picarea[k].name, page->m_picarea[k].name);
				out_pItem->m_picarea[k].xmin = page->m_picarea[k].xmin;
				out_pItem->m_picarea[k].ymin = page->m_picarea[k].ymin;
				out_pItem->m_picarea[k].xmax = page->m_picarea[k].xmax;
				out_pItem->m_picarea[k].ymax = page->m_picarea[k].ymax;
			}
		}
		else
		{
			out_pItem->m_picarea_no = 0;
			out_pItem->m_picarea = NULL;
			out_pItem->m_pic_act_area = 0;
		}
	}
}
void CNCLDDform::SetActionItem(int action,int itemno, int type, int itype)
{
	m_action = action;
	m_action_item = itemno;
	m_action_typ = type;
	m_action_ityp = itype;
}
/***********************************************************************
**
**   FUNCTION: ReplaceDispItem(int olditem_no, int newitem_no)
**		switch 2 label items with different item no
**	 INPUT:  olditem_no, newitem_no
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::ReplaceDispItem(int olditem_no, int newitem_no)
{
	if (m_selitem==olditem_no)
	{
		m_selitem = newitem_no;
	}
	if (olditem_no==newitem_no) return;
	CWnd *dispwin = m_disp_win[olditem_no];
	int page = m_disp_secpage[olditem_no];
	if (olditem_no<newitem_no)
	{
		for (int i=olditem_no; i<m_dispno; i++)
		{
			if (i<newitem_no)
			{
				m_disp_win[i] = m_disp_win[i+1];
				m_disp_secpage[i] = m_disp_secpage[i+1];
			}
			if (i==newitem_no)
			{
				m_disp_win[i] = dispwin;
				m_disp_secpage[i] = page;
			}
			if (m_dinpwin[i]!=NULL)
				((CNCLDDStatic*)m_disp_win[i])->SetItemNo(i);
			if (i==newitem_no)
				return;
		}
	}
	if (olditem_no>newitem_no)
	{
		for (int i=olditem_no; i>=newitem_no; i--)
		{
			if ((i<=olditem_no)&&(i>=1))
			{
				m_disp_win[i] = m_disp_win[i-1];
				m_disp_secpage[i] = m_disp_secpage[i-1];
			}
			if (i==newitem_no)
			{
				m_disp_win[i] = dispwin;
				m_disp_secpage[i] = page;
				goto done;
			}
done:;
			if (m_disp_win[i]!=NULL)
				((CNCLDDStatic*)m_disp_win[i])->SetItemNo(i);
			if (i==newitem_no)
				return;
		}
	}
}
/***********************************************************************
**
**   FUNCTION: ReplaceFrameItem(int olditem_no, int newitem_no)
**		switch 2 frame items with different item no
**	 INPUT:  olditem_no, newitem_no
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::ReplaceFrameItem(int olditem_no, int newitem_no)
{
	CWnd *fwin = m_frame[olditem_no];
	int page = m_frm_secpage[olditem_no];

	if (m_selitem==olditem_no)
	{
		m_selitem = newitem_no;
	}
	if (olditem_no==newitem_no) return;
	if (olditem_no<newitem_no)
	{
		for (int i=olditem_no; i<m_frameno; i++)
		{
			if (i<newitem_no)
			{
				m_frame[i] = m_frame[i+1];
				m_frm_secpage[i] = m_frm_secpage[i+1];
			}
			if (i==newitem_no)
			{
				m_frame[i] = fwin;
				m_frm_secpage[i] = page;
			}
			if (m_frame[i]!=NULL)
				((CNCLDDGroup*)m_frame[i])->SetItemNo(i);
			if (i==newitem_no)
				return;
		}
	}
	if (olditem_no>newitem_no)
	{
		for (int i=olditem_no; i>=newitem_no; i--)
		{
			if ((i<=olditem_no)&&(i>=1))
			{
				m_frame[i] = m_frame[i-1];
				m_frm_secpage[i] = m_frm_secpage[i-1];
			}
			if (i==newitem_no)
			{
				m_frame[i] = fwin;
				m_frm_secpage[i] = page;
				goto done;
			}
done:;
			if (m_frame[i]!=NULL)
				((CNCLDDGroup*)m_frame[i])->SetItemNo(i);
			if (i==newitem_no)
				return;
		}
	}
}
void CNCLDDform::ReplacePicItem(int olditem_no, int newitem_no)
{
	CWnd *pwin = m_picture[olditem_no];
	int page = m_pic_secpage[olditem_no];

	if (m_selitem==olditem_no)
	{
		m_selitem = newitem_no;
	}
	if (olditem_no==newitem_no) return;
	if (olditem_no<newitem_no)
	{
		for (int i=olditem_no; i<m_picno; i++)
		{
			if (i<newitem_no)
			{
				m_picture[i] = m_picture[i+1];
				m_pic_secpage[i] = m_pic_secpage[i+1];
			}
			if (i==newitem_no)
			{
				m_picture[i] = pwin;
				m_pic_secpage[i] = page;
			}
			if (m_picture[i]!=NULL)
				((CNCLDDPicWin*)m_picture[i])->SetItemNo(i);
			if (i==newitem_no)
				return;
		}
	}
	if (olditem_no>newitem_no)
	{
		for (int i=olditem_no; i>=newitem_no; i--)
		{
			if ((i<=olditem_no)&&(i>=1))
			{
				m_picture[i] = m_picture[i-1];
				m_pic_secpage[i] = m_pic_secpage[i-1];
			}
			if (i==newitem_no)
			{
				m_picture[i] = pwin;
				m_pic_secpage[i] = page;
				goto done;
			}
done:;
			if (m_picture[i]!=NULL)
				((CNCLDDPicWin*)m_picture[i])->SetItemNo(i);
			if (i==newitem_no)
				return;
		}
	}
}
/***********************************************************************
**
**   FUNCTION: ReplaceItem(int olditem_no, int newitem_no, int dtype)
**		switch 2 item with different item no
**	 INPUT:  olditem_no, newitem_no, dtype
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::ReplaceItem(int olditem_no, int newitem_no, int dtype)
{
	if (dtype==0)
	{
		ReplaceDispItem(olditem_no, newitem_no);
		goto done;
	}
	if (dtype==1)
	{
		ReplaceFrameItem(olditem_no, newitem_no);
		goto done;
	}
	if (dtype==2)
	{
		ReplacePicItem(olditem_no, newitem_no);
		goto done;
	}
	CWnd *dinpwin = m_dinpwin[olditem_no];
	CWnd *inpwin = m_inpwin[olditem_no];
	int type, save_type = m_type[olditem_no];
	int page = m_inp_secpage[olditem_no];

	if (olditem_no==newitem_no) 				
		goto done;

	if (olditem_no<newitem_no)
	{
		for (int i=olditem_no; i<m_inputno; i++)
		{
			if (i<newitem_no)
			{
				m_dinpwin[i] = m_dinpwin[i+1];
				m_inpwin[i] = m_inpwin[i+1];
				m_type[i] = m_type[i+1];
				m_inp_secpage[i] = m_inp_secpage[i+1];
			}
			if (i==newitem_no)
			{
				m_dinpwin[i] = dinpwin;
				m_inpwin[i] = inpwin;
				m_type[i] = save_type;
				m_inp_secpage[i] = page;
			}
			type = m_type[i];
			if (m_dinpwin[i]!=NULL)
			{
				if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
					((CNCLDDStatic*)(m_dinpwin[i]))->SetItemNo(i);
				else if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
					((CNCLDDButton*)(m_dinpwin[i]))->SetItemNo(i);
			}
			if (m_inpwin[i]==NULL)
			{
				if (i==newitem_no)
					goto done;
				continue;
			}
			if ((type==1)||(type==25))
			{
				((CNCLDDButton*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==2)
			{
				((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==3)
			{
				((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==5)
			{
				((CNCLDDListBox*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==7)
			{
				((CNCLDDButton*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==8)
			{
				((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==9)
			{
				((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==11)
			{
				((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==13)
			{
				((CNCLDDProcess*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==15)
			{
				((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==16)
			{
				((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==17)
			{
				((CNCLDDListCtrl*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==18)
			{
				((CNCLDDColorButton*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==19)
			{
				((CNCLDDListCtrl2*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==23)
			{
				((CNCLDDColorButton*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==24)
			{
				((CNCLDDSlider*)m_inpwin[i])->SetItemNo(i);
			}
			if (i==newitem_no)
				goto done;
		}
	}
	if (olditem_no>newitem_no)
	{
		for (int i=olditem_no; i>=newitem_no; i--)
		{
			if ((i<=olditem_no)&&(i>=1))
			{
				m_dinpwin[i] = m_dinpwin[i-1];
				m_inpwin[i] = m_inpwin[i-1];
				m_type[i] = m_type[i-1];
				m_inp_secpage[i] = m_inp_secpage[i-1];
			}
			if (i==newitem_no)
			{
				m_dinpwin[i] = dinpwin;
				m_inpwin[i] = inpwin;
				m_type[i] = save_type;
				m_inp_secpage[i] = page;
				goto done_1;
			}
done_1:;
			type = m_type[i];
			if (m_dinpwin[i]!=NULL)
			{
				if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
					((CNCLDDStatic*)(m_dinpwin[i]))->SetItemNo(i);
				else if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
					((CNCLDDButton*)(m_dinpwin[i]))->SetItemNo(i);
			}
			if (m_inpwin[i]==NULL)
			{
				if (i==newitem_no)
					goto done;
				continue;
			}
			if ((type==1)||(type==25))
			{
				((CNCLDDButton*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==2)
			{
				((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==3)
			{
				((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==5)
			{
				((CNCLDDListBox*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==7)
			{
				((CNCLDDButton*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==8)
			{
				((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==9)
			{
				((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==11)
			{
				((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==13)
			{
				((CNCLDDProcess*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==15)
			{
				((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==16)
			{
				((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==17)
			{
				((CNCLDDListCtrl*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==18)
			{
				((CNCLDDColorButton*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==19)
			{
				((CNCLDDListCtrl2*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==23)
			{
				((CNCLDDColorButton*)m_inpwin[i])->SetItemNo(i);
			}
			else if (type==24)
			{
				((CNCLDDSlider*)m_inpwin[i])->SetItemNo(i);
			}
			if (i==newitem_no)
				goto done;
		}
	}
done:;
/*
.....need update the select item and select array
*/
	CRect rect;
	if (m_selitem==olditem_no)
	{
		m_selitem = newitem_no;
		for (int i=0; i<m_selnum; i++)
		{
			if ((dtype==m_seltype[i])&&(olditem_no==m_selarray[i]))
			{
				m_selarray[i] = newitem_no;
			}
		}
		if (dtype==0)
		{
			m_disp_win[m_selitem]->GetWindowRect(&rect);
		}
		if (dtype==1)
		{
			m_frame[m_selitem]->GetWindowRect(&rect);
		}
		if (dtype==2)
		{
			m_picture[m_selitem]->GetWindowRect(&rect);
		}
		if (dtype==3)
		{
			m_dinpwin[m_selitem]->GetWindowRect(&rect);
		}
		if (dtype==4)
		{
			m_inpwin[m_selitem]->GetWindowRect(&rect);
		}
		if (dtype==5)
		{
			m_macro_win[m_selitem]->GetWindowRect(&rect);
		}
		ScreenToClient(&rect);
		Reset_selrec(rect);
	}
	CPaintDC dc(this);
	if ((m_selnum>0)&&(m_select_draw==1))
	{
		DrawSelectArray(&dc);
	}
	if ((m_selitem!=-1)&&(m_select_draw==1))
		DrawSelect(1);
}

/***********************************************************************
**
**   FUNCTION: CreateWinFromInfo(CNCLWinInfo* info, int dispno, int frameno, int picno, int inputno)
**			Create an window item from the saved window info structure
**		
**	 INPUT:  info: window info structure
**			dispno, frameno, picno, inputno: index number of the Window need created
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::CreateWinFromInfo(CNCLWinInfo* info, int dispno, int frameno, int picno, int inputno)
{
	CRect rect;
	POINT point;
	UX_pathname vfile;
	int item_no;
	int insert_item_no = info->m_input_itemno;
	int type = info->m_dtype;
	if (type==0)
	{
/*
......create display label
*/
		rect.left = info->m_pos[0];
		rect.top = info->m_pos[1];
		rect.right = info->m_pos[0] + info->m_size[0];
		rect.bottom = info->m_pos[1] + info->m_size[1];
		item_no = m_dispno + dispno;

		m_disp_win[item_no] = new CNCLDDStatic();
		m_disp_secpage[item_no] = info->m_page;
		((CNCLDDStatic*)m_disp_win[item_no])->SetParent(this);
		((CNCLDDStatic*)m_disp_win[item_no])->m_prompt = 0;
		((CNCLDDStatic*)m_disp_win[item_no])->m_itemno = info->m_input_itemno;
		((CNCLDDStatic*)m_disp_win[item_no])->Create(NULL, NULL, WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_DISP + info->m_input_itemno));
		((CNCLDDStatic*)m_disp_win[item_no])->ShowWindow(SW_SHOW);
		((CNCLDDStatic*)m_disp_win[item_no])->SetLabelText(info->m_label);
		((CNCLDDStatic*)m_disp_win[item_no])->InitDrag();
		CNCLFormProp *page = ((CNCLDDStatic*)m_disp_win[item_no])->GetPropertyPage();
		page->SetWinInfo(info);
		m_dispno++;
	}
	if (type==1)
	{
/*
......create Framebox
*/
		rect.left = info->m_pos[0];
		rect.top = info->m_pos[1];
		rect.right = info->m_pos[0] + info->m_size[0];
		rect.bottom = info->m_pos[1] + info->m_size[1];
		item_no = frameno + m_frameno;
		m_frame[item_no] = new CNCLDDGroup();
		m_frm_secpage[item_no] = info->m_page;
		((CNCLDDGroup*)m_frame[item_no])->SetParent(this);
		((CNCLDDGroup*)m_frame[item_no])->m_itemno = info->m_input_itemno;
		((CNCLDDGroup*)m_frame[item_no])->Create("group", WS_VISIBLE|WS_CHILD|WS_GROUP|BS_GROUPBOX, rect, this,  (UINT)(IDC_FORMDESGN_FRAME + info->m_input_itemno));
		((CNCLDDGroup*)m_frame[item_no])->ShowWindow(SW_SHOW);
		((CNCLDDGroup*)m_frame[item_no])->InitDrag();
		m_frameno++;
		CNCLFormProp *page = ((CNCLDDGroup*)m_frame[item_no])->GetPropertyPage();
		page->SetWinInfo(info);
	}
	if (type==2)
	{
/*
......create picture box
*/
		rect.left = info->m_pos[0];
		rect.top = info->m_pos[1];
		rect.right = info->m_pos[0] + info->m_size[0];
		rect.bottom = info->m_pos[1] + info->m_size[1];
		item_no = picno + m_picno;
		m_picture[item_no] = new CNCLDDPicWin(info->m_label.GetBuffer(), info->m_choices.GetBuffer(), this);
		m_pic_secpage[item_no] = info->m_page;
		((CNCLDDPicWin*)m_picture[item_no])->SetParent(this);
		((CNCLDDPicWin*)m_picture[item_no])->m_itemno = info->m_input_itemno;
		((CNCLDDPicWin*)m_picture[item_no])->Create(WS_CHILD|WS_VISIBLE|WS_TABSTOP|WS_BORDER, rect, this, (UINT)(IDC_FORMDESGN_PIC + info->m_input_itemno));
		((CNCLDDPicWin*)m_picture[item_no])->ShowWindow(SW_SHOW);
		((CNCLDDPicWin*)m_picture[item_no])->InitDrag();
		m_picno++;
		CNCLFormProp *page = ((CNCLDDPicWin*)m_picture[item_no])->GetPropertyPage();
		page->SetWinInfo(info);
	}
	else if (type==3)
	{
		item_no = inputno + m_inputno;
		rect.left = info->m_pos[0];
		rect.top = info->m_pos[1];
		rect.right = info->m_pos[0] + info->m_size[0];
		rect.bottom = info->m_pos[1] + info->m_size[1];
		m_type[item_no] = info->m_itype;
		m_input[item_no] = info->m_input;
		m_inp_secpage[item_no] = info->m_page;
		if (m_type[item_no]==23)
		{
			m_dinpwin[item_no] = new CNCLDDButton(m_type[item_no]);
			((CNCLDDButton*)m_dinpwin[item_no])->SetParent(this);
			((CNCLDDButton*)m_dinpwin[item_no])->set_prompt(1);
			((CNCLDDButton*)m_dinpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDButton*)m_dinpwin[item_no])->Create(info->m_label,
					WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this,  (UINT)(IDC_FORMDESGN_STATIC + info->m_input_itemno));
			((CNCLDDButton*)m_dinpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDButton*)m_dinpwin[item_no])->InitDrag();
			CNCLFormProp *page;
			page = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());			
			page->SetWinInfo(info);
		}
		else if (m_input[item_no]==FORM_STRING)
		{
			m_dinpwin[item_no] = new CNCLDDStatic();
			((CNCLDDStatic*)m_dinpwin[item_no])->SetParent(this);
			((CNCLDDStatic*)m_dinpwin[item_no])->m_prompt = 1;
			((CNCLDDStatic*)m_dinpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDStatic*)m_dinpwin[item_no])->Create(NULL, NULL, WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_STATIC + info->m_input_itemno));
			((CNCLDDStatic*)m_dinpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDStatic*)m_dinpwin[item_no])->SetLabelText(info->m_label);
			((CNCLDDStatic*)m_dinpwin[item_no])->InitDrag();
			CNCLFormProp *page = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item_no]))->GetPropertyPage());
			page->SetWinInfo(info);
		}
		else
		{
			m_dinpwin[item_no] = new CNCLDDButton(3);
			((CNCLDDButton*)m_dinpwin[item_no])->SetParent(this);
			((CNCLDDButton*)m_dinpwin[item_no])->set_prompt(1);
			((CNCLDDButton*)m_dinpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDButton*)m_dinpwin[item_no])->Create(info->m_label,
					WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this,  (UINT)(IDC_FORMDESGN_STATIC + info->m_input_itemno));
			((CNCLDDButton*)m_dinpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDButton*)m_dinpwin[item_no])->InitDrag();
			CNCLFormProp *page;
			page = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());			
			page->SetWinInfo(info);
		}
	}
	else if (type==4)
	{
		item_no = inputno + m_inputno;
		rect.left = info->m_pos[0];
		rect.top = info->m_pos[1];
		rect.right = info->m_pos[0] + info->m_size[0];
		rect.bottom = info->m_pos[1] + info->m_size[1];
		m_type[item_no] = info->m_itype;
		m_inp_secpage[item_no] = info->m_page;
		if ((m_type[item_no]==1)||(m_type[item_no]==25))
		{
/*
.......PUSHBUTTON
*/
			m_inpwin[item_no] = new CNCLDDButton(m_type[item_no]);
			((CNCLDDButton*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDButton*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			if (m_type[item_no]==1)
				((CNCLDDButton*)m_inpwin[item_no])->Create(info->m_label, WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_NOTIFY,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			else
			{
				char *temp = (info->m_label).GetBuffer();
				strcpy(vfile, temp);
				((CNCLDDButton*)m_inpwin[item_no])->SetBitMapFile(vfile);
				((CNCLDDButton*)m_inpwin[item_no])->Create(" ", WS_VISIBLE | WS_CHILD | BS_BITMAP, rect, this, 
						(UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			}
			((CNCLDDButton*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDButton*)m_inpwin[item_no])->InitDrag();
			CNCLFormProp *page = ((CNCLDDButton*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==2)
		{
/*
.......CHOICEBOX
*/
			m_inpwin[item_no] = new CNCLDDCombo(m_type[item_no]);
			((CNCLDDCombo*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDCombo*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDCombo*)m_inpwin[item_no])->Create(WS_CHILD|WS_VISIBLE|CBS_DROPDOWNLIST|WS_VSCROLL|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDCombo*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDCombo*)m_inpwin[item_no])->InitDrag();			
			CNCLFormProp *page = ((CNCLDDCombo*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==3)
		{
/*
.......EDIT
*/
			m_inpwin[item_no] = new CNCLDDEdit(m_type[item_no]);
			((CNCLDDEdit*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDEdit*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDEdit*)m_inpwin[item_no])->CreateEx(WS_EX_CLIENTEDGE, "EDIT", "EDIT", 
						WS_CHILD | WS_VISIBLE | ES_AUTOHSCROLL | ES_LEFT | WS_BORDER | WS_TABSTOP | DS_SETFONT, 
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno), 0);
			((CNCLDDEdit*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDEdit*)m_inpwin[item_no])->InitDrag();						
			CNCLFormProp *page = ((CNCLDDEdit*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==5)
		{
/*
.......LISTBOX, no prompt, so no change for 'rect.left' and 'rect.top'
*/
			m_inpwin[item_no] = new CNCLDDListBox(m_type[item_no], 1);
			((CNCLDDListBox*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDListBox*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDListBox*)m_inpwin[item_no])->CreateEx(WS_EX_CLIENTEDGE, "LISTBOX", "listbox", 
						WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_BORDER | LBS_NOTIFY | WS_VSCROLL | LBS_USETABSTOPS | WS_HSCROLL,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDListBox*)m_inpwin[item_no])->InitDrag();
			((CNCLDDListBox*)m_inpwin[item_no])->AddString("test");
			((CNCLDDListBox*)m_inpwin[item_no])->AddString("test2");
			((CNCLDDListBox*)m_inpwin[item_no])->SetCurSel(0);
			CNCLFormProp *page = ((CNCLDDListBox*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==7)
		{
/*
.......CHECKBOX
*/
			m_inpwin[item_no] = new CNCLDDButton(m_type[item_no]);
			((CNCLDDButton*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDButton*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDButton*)m_inpwin[item_no])->Create(info->m_label, WS_CHILD|WS_VISIBLE|BS_LEFT|BS_AUTOCHECKBOX|WS_TABSTOP|BS_NOTIFY, rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDButton*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDButton*)m_inpwin[item_no])->InitDrag();
			CNCLFormProp *page = ((CNCLDDButton*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==8)
		{
/*
.......COMBLIST_SIMPLE
*/
			m_inpwin[item_no] = new CNCLDDCombo(m_type[item_no]);
			((CNCLDDCombo*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDCombo*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDCombo*)m_inpwin[item_no])->Create(WS_CHILD|WS_VISIBLE|CBS_SIMPLE|WS_VSCROLL|CBS_AUTOHSCROLL|WS_TABSTOP|WS_HSCROLL, rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDCombo*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDCombo*)m_inpwin[item_no])->InitDrag();			
			CNCLFormProp *page = ((CNCLDDCombo*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
//since we didn't added when we create new, so don't add here
//			((CNCLDDCombo*)m_inpwin[item_no])->AddString("test");
		}
		else if (m_type[item_no]==9)
		{
/*
.......COMBLIST_DROPDOWN
*/
			m_inpwin[item_no] = new CNCLDDCombo(m_type[item_no]);
			((CNCLDDCombo*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDCombo*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDCombo*)m_inpwin[item_no])->Create(WS_CHILD|WS_VISIBLE|CBS_DROPDOWN|WS_VSCROLL|CBS_AUTOHSCROLL|WS_TABSTOP|WS_HSCROLL|CBS_NOINTEGRALHEIGHT, rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDCombo*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDCombo*)m_inpwin[item_no])->InitDrag();			
			CNCLFormProp *page = ((CNCLDDCombo*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
			((CNCLDDCombo*)m_inpwin[item_no])->AddString("test");
		}
		else if (m_type[item_no]==11)
		{
/*
.......DISPLAY
*/
			m_inpwin[item_no] = new CNCLDDEdit(m_type[item_no]);
			((CNCLDDEdit*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDEdit*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDEdit*)m_inpwin[item_no])->CreateEx(WS_EX_CLIENTEDGE, "EDIT", "DISPLAY", 
						WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | ES_AUTOVSCROLL | ES_LEFT | WS_BORDER | ES_MULTILINE |ES_WANTRETURN | ES_READONLY | DS_SETFONT, rect, 
						this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDEdit*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDEdit*)m_inpwin[item_no])->InitDrag();						
			CNCLFormProp *page = ((CNCLDDEdit*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==13)
		{
/*
.......PROGRESS
*/
			m_inpwin[item_no] = new CNCLDDProcess(m_type[item_no]);
			((CNCLDDProcess*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDProcess*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDProcess*)m_inpwin[item_no])->Create( WS_CHILD | WS_VISIBLE | WS_BORDER | PBS_SMOOTH, rect, 
							this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDProcess*)m_inpwin[item_no])->SetRange(1, 100);
			((CNCLDDProcess*)m_inpwin[item_no])->SetPos(50);
			((CNCLDDProcess*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDProcess*)m_inpwin[item_no])->InitDrag();						
			((CNCLDDProcess*)m_inpwin[item_no])->SetWindowText("50");
			CNCLFormProp *page = ((CNCLDDProcess*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==15)
		{
/*
.......CHOICE_LIST
*/
			m_inpwin[item_no] = new CNCLDDCombo(m_type[item_no]);
			((CNCLDDCombo*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDCombo*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDCombo*)m_inpwin[item_no])->Create(WS_CHILD|WS_VISIBLE|CBS_DROPDOWNLIST|WS_VSCROLL|WS_TABSTOP|CBS_AUTOHSCROLL|WS_HSCROLL, 
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDCombo*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDCombo*)m_inpwin[item_no])->InitDrag();			
			CNCLFormProp *page = ((CNCLDDCombo*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==16)
		{
/*
.......EDITBOX
*/
			m_inpwin[item_no] = new CNCLDDEdit(m_type[item_no]);
			((CNCLDDEdit*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDEdit*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDEdit*)m_inpwin[item_no])->CreateEx(WS_EX_CLIENTEDGE, "EDIT", "EDITBOX", 
				WS_CHILD|WS_VISIBLE|WS_TABSTOP|ES_AUTOHSCROLL|ES_AUTOVSCROLL|ES_LEFT|WS_BORDER|ES_MULTILINE|ES_WANTRETURN|DS_SETFONT, 
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno), 0);
			((CNCLDDEdit*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDEdit*)m_inpwin[item_no])->InitDrag();						
			CNCLFormProp *page = ((CNCLDDEdit*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==17)
		{
/*
.......LISTTABLE, no prompt, so no change for 'rect.left' and 'rect.top'
*/
			m_inpwin[item_no] = new CNCLDDListCtrl();
			((CNCLDDListCtrl*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDListCtrl*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDListCtrl*)m_inpwin[item_no])->Create(WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL | WS_HSCROLL | WS_VSCROLL, 
					rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDListCtrl*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDListCtrl*)m_inpwin[item_no])->InitDrag();
			CNCLDDListCtrl *listctl = (CNCLDDListCtrl*)m_inpwin[item_no];
			listctl->InsertColumn(0, "col1",  LVCFMT_LEFT, 50);
			listctl->InsertColumn(1, "col2",  LVCFMT_LEFT, 50);
			listctl->InsertItem(0,  "data1");
			listctl->SetItemText(0, 1, "data2");
			listctl->SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES);
			CNCLFormProp *page = ((CNCLDDListCtrl*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==18)
		{
/*
.......COLOR
*/
			m_inpwin[item_no] = new CNCLDDColorButton(m_type[item_no]);
			((CNCLDDColorButton*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDColorButton*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDColorButton*)m_inpwin[item_no])->Create("", WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_OWNERDRAW |BS_NOTIFY,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDColorButton*)m_inpwin[item_no])->set_color(RGB(255,128,0), RGB(255,128,0));
			((CNCLDDColorButton*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDColorButton*)m_inpwin[item_no])->InitDrag();
			CNCLFormProp *page = ((CNCLDDColorButton*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==24)
		{
/*
.......SLIDER
*/
			m_inpwin[item_no] = new CNCLDDSlider(m_type[item_no]);
			((CNCLDDSlider*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDSlider*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			DWORD dwStyle;
			if (info->m_justified==0)
				dwStyle = WS_VISIBLE | WS_CHILD | TBS_HORZ;
			else
				dwStyle = WS_VISIBLE | WS_CHILD | TBS_VERT;		
			((CNCLDDSlider*)m_inpwin[item_no])->Create(dwStyle,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDSlider*)m_inpwin[item_no])->SetFocus();
			((CNCLDDSlider*)m_inpwin[item_no])->set_color(RGB(255,0,0), RGB(255,128,0));
			((CNCLDDSlider*)m_inpwin[item_no])->SetPos(20);
			((CNCLDDSlider*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDSlider*)m_inpwin[item_no])->InitDrag();
			CNCLFormProp *page = ((CNCLDDSlider*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
			SetFocus();
//			((CNCLDDSlider*)m_inpwin[item_no])->SetFocus();
		}
		else if (m_type[item_no]==23)
		{
/*
.......COLOR
*/
			m_inpwin[item_no] = new CNCLDDColorButton(m_type[item_no]);
			((CNCLDDColorButton*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDColorButton*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDColorButton*)m_inpwin[item_no])->Create("", WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_OWNERDRAW |BS_NOTIFY,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDColorButton*)m_inpwin[item_no])->set_color(RGB(128,128,0), RGB(128,128,0));
			((CNCLDDColorButton*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDColorButton*)m_inpwin[item_no])->InitDrag();
			CNCLFormProp *page = ((CNCLDDColorButton*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		else if (m_type[item_no]==19)
		{
/*
.......DATATABLE, no prompt, so no change for 'rect.left' and 'rect.top'
*/
			m_inpwin[item_no] = new CNCLDDListCtrl2();
			((CNCLDDListCtrl2*)m_inpwin[item_no])->SetParent(this);
			((CNCLDDListCtrl2*)m_inpwin[item_no])->m_itemno = info->m_input_itemno;
			((CNCLDDListCtrl2*)m_inpwin[item_no])->Create(WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL | WS_HSCROLL | WS_VSCROLL, 
				rect, this, (UINT)(IDC_FORMDESGN_INPUT + info->m_input_itemno));
			((CNCLDDListCtrl2*)m_inpwin[item_no])->ShowWindow(SW_SHOW);
			((CNCLDDListCtrl2*)m_inpwin[item_no])->InitDrag();
			((CNCLDDListCtrl2*)m_inpwin[item_no])->SetType(1);

			CNCLDDListCtrl2 *listctl = (CNCLDDListCtrl2*)m_inpwin[item_no];
			LV_COLUMN lvc;
			lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;

			lvc.iSubItem = 0;
			lvc.pszText = "col1";
			lvc.cx = 40;
			lvc.fmt = LVCFMT_LEFT;
			listctl->InsertColumn(0,&lvc);
	
			lvc.iSubItem = 1;
			lvc.pszText = "col2";
			lvc.cx = 40;
			lvc.fmt = LVCFMT_LEFT;
			listctl->InsertColumn(1,&lvc);

			listctl->InsertItem(0,  "data1");
			listctl->SetItemText(0, 1, "data2");
			listctl->m_nNumberOfRows = 1;
			listctl->m_nNumberOfCols = 2;
			listctl->ModifyStyle(0,LVS_SHOWSELALWAYS);
			CNCLFormProp *page = ((CNCLDDListCtrl2*)m_inpwin[item_no])->GetPropertyPage();
			page->SetWinInfo(info);
		}
		m_inputno++;
	}
	updateWindow_draw();
	CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
	pview->SetFormInputNo(m_inputno);
	pview->SetFormSecNo(m_secno);
}
/*
......move the item from old_indx to new indx
......it will place the item from old index to the new index and reset old index to NULL
......if the new index already have item, insert the item there.
*/
void CNCLDDform::MoveItemPlace(int new_indx, int old_indx, int dtype)
{
	int i, j, tmp_type[200], tmp_page[200];
	CWnd *tmp_win[200], *tmp_win2[200];
	if (old_indx==new_indx)
		return;
	if (dtype==0)
	{
		for (i=0, j=0; i<200, j<200; i++,j++)
		{
			if (j!=new_indx)
			{
				tmp_win[j] = m_disp_win[i];
				tmp_page[j] = m_disp_secpage[i];
				if (tmp_win[j]!=NULL)
					((CNCLDDStatic*)tmp_win[j])->SetItemNo(j);
				m_disp_win[i] = NULL;
				m_disp_secpage[i] = -1;
				continue;
			}
			if (j==new_indx)
			{
				tmp_win[j] = m_disp_win[old_indx];
				tmp_page[j] = m_disp_secpage[old_indx];
				if (tmp_win[j]!=NULL)
					((CNCLDDStatic*)tmp_win[j])->SetItemNo(j);
				m_disp_win[old_indx] = NULL;
				m_disp_secpage[old_indx] = -1;
				if (m_disp_win[j]==NULL)
					continue;
				i--;
			}
		}
		for (i=0; i<200; i++)
		{
			m_disp_win[i] = tmp_win[i];
			m_disp_secpage[i] = tmp_page[i];
		}
		return;
	}
	if (dtype==1)
	{
		for (i=0, j=0; i<200, j<200; i++,j++)
		{
			if (j!=new_indx)
			{
				tmp_win[j] = m_frame[i];
				tmp_page[j] = m_frm_secpage[i];
				if (tmp_win[j]!=NULL)
					((CNCLDDGroup*)tmp_win[j])->SetItemNo(j);
				m_frame[i] = NULL;
				m_frm_secpage[i] = -1;
				continue;
			}
			if (j==new_indx)
			{
				tmp_win[j] = m_frame[old_indx];
				tmp_page[j] = m_frm_secpage[old_indx];
				if (tmp_win[j]!=NULL)
					((CNCLDDGroup*)tmp_win[j])->SetItemNo(j);
				m_frame[old_indx] = NULL;
				m_frm_secpage[old_indx] = -1;
				if (m_frame[j]==NULL)
					continue;
				i--;
			}
		}
		for (i=0; i<200; i++)
		{
			m_frame[i] = tmp_win[i];
			m_frm_secpage[i] = tmp_page[i];
		}
		return;
	}
	
	if (dtype==2)
	{
		for (i=0, j=0; i<200, j<200; i++,j++)
		{
			if (j!=new_indx)
			{
				tmp_win[j] = m_picture[i];
				tmp_page[j] = m_pic_secpage[i];
				if (tmp_win[j]==NULL)
					continue;
				((CNCLDDPicWin*)tmp_win[j])->SetItemNo(j);
				continue;
			}
			if (j==new_indx)
			{
				tmp_win[j] = m_picture[old_indx];
				tmp_page[j] = m_pic_secpage[old_indx];
				if (tmp_win[j]!=NULL)
					((CNCLDDPicWin*)tmp_win[j])->SetItemNo(j);
				m_picture[old_indx] = NULL;
				m_pic_secpage[old_indx] = -1;
				if (m_picture[j]==NULL)
					continue;
				i--;
			}
		}
		for (i=0; i<200; i++)
		{
			m_picture[i] = tmp_win[i];
			m_pic_secpage[i] = tmp_page[i];
		}
		return;
	}

	for (i=0, j=0; i<200, j<m_inputno; i++,j++)
	{
		if (j!=new_indx)
		{
			tmp_win[j] = m_dinpwin[i];
			tmp_win2[j] = m_inpwin[i];
			tmp_type[j] = m_type[i];
			tmp_page[j] = m_inp_secpage[i];
			m_dinpwin[i] = NULL;
			m_inpwin[i] = NULL;
			m_inp_secpage[i] = -1;
			continue;
		}
		if (j==new_indx)
		{
			tmp_win[j] = m_dinpwin[old_indx];
			tmp_win2[j] = m_inpwin[old_indx];
			tmp_type[j] = m_type[old_indx];
			tmp_page[j] = m_inp_secpage[old_indx];
			m_dinpwin[old_indx] = NULL;
			m_inpwin[old_indx] = NULL;
			m_type[old_indx] = -1;
			m_inp_secpage[old_indx] = -1;
/*
......need set the assigned item to NULL too
*/
			if (new_indx>old_indx)
			{
				tmp_win[old_indx] = NULL;
				tmp_win2[old_indx] = NULL;
				tmp_type[old_indx] = -1;
				m_inp_secpage[old_indx] = -1;
			}
			if (m_inpwin[i]==NULL)
				continue;
			i--;
		}
	}
	for (i=0; i<m_inputno; i++)
	{
		m_dinpwin[i] = tmp_win[i];
		m_inpwin[i] = tmp_win2[i];
		m_type[i] = tmp_type[i];
		m_inp_secpage[i] = tmp_page[i];
		if (m_dinpwin[i]!=NULL)
		{
			if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
				((CNCLDDStatic*)(m_dinpwin[i]))->SetItemNo(i);
			else if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
				((CNCLDDButton*)(m_dinpwin[i]))->SetItemNo(i);
		}
		if (m_inpwin[i]==NULL)
			continue;
		if ((m_type[i]==1)||(m_type[i]==25))
		{
			((CNCLDDButton*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==2)
		{
			((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==3)
		{
			((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==5)
		{
			((CNCLDDListBox*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==7)
		{
			((CNCLDDButton*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==8)
		{
			((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==9)
		{
			((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==11)
		{
			((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==13)
		{
			((CNCLDDProcess*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==15)
		{
			((CNCLDDCombo*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==16)
		{
			((CNCLDDEdit*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==17)
		{
			((CNCLDDListCtrl*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==18)
		{
			((CNCLDDColorButton*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==19)
		{
			((CNCLDDListCtrl2*)m_inpwin[i])->SetItemNo(i);
		}	
		else if (m_type[i]==23)
		{
			((CNCLDDColorButton*)m_inpwin[i])->SetItemNo(i);
		}
		else if (m_type[i]==24)
		{
			((CNCLDDSlider*)m_inpwin[i])->SetItemNo(i);
		}
	}
}
/***********************************************************************
**
**   FUNCTION: SaveUndoNumber(int undo_num, int frameno, int picno, int dispno, int inputno, int pageno)
**			Save an undo item info into the undo list
**		
**	 INPUT:  none
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::SaveUndoNumber(int undo_num, int frameno, int picno, int dispno, int inputno, 
	int pageno, int selnum, int seldtyp, int selityp)
{
	if (m_no_undo_save==1)
		return;
	if (frameno<0)
		frameno = 0;
	if (picno<0)
		picno = 0;
	if (dispno<0)
		dispno = 0;
	if (inputno<0)
		inputno = 0;
	CNCLWinInfo* temp;
	POSITION pos = m_pUndoList.GetHeadPosition();
	while (pos!=NULL)
	{
		temp = m_pUndoList.GetNext(pos);
		if (temp==NULL)
			break;
	}
	CNCLWinInfo *infoItem;
	infoItem = new CNCLWinInfo();
	infoItem->m_dtype = -1;
	infoItem->m_itype = undo_num;
	infoItem->m_type = frameno;
	infoItem->m_len = picno;
	infoItem->m_prec = dispno;
	infoItem->m_input_itemno = inputno;
	infoItem->m_page = pageno;
	infoItem->m_action = m_action;
/*
.....save selection info
*/
	infoItem->m_active = selnum;
	infoItem->m_pos[0] = seldtyp;
	infoItem->m_pos[1] = selityp;	
	if ((m_action==20)||(m_action==21))
	{
		infoItem->m_label = ((CNCLFormView*)m_parent)->GetSectionLabel(pageno);
		infoItem->m_color = ((CNCLFormView*)m_parent)->GetSectionColor(pageno);
	}
	m_pUndoList.AddTail(infoItem);
	if (undo_num!=0)
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableUndoBtn(1);
}

/***********************************************************************
**
**   FUNCTION: SaveRedoNumber(int undo_num, int frameno, int picno, int dispno, int inputno, int pageno)
**			Save an undo item info into the undo list
**		
**	 INPUT:  none
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::SaveRedoNumber(int redo_num, int frameno, int picno, int dispno, int inputno, 
	int pageno, int selitem, int seldtyp, int selityp)
{
	if (m_no_redo==1)
		return;
	if (frameno<0)
		frameno = 0;
	if (picno<0)
		picno = 0;
	if (dispno<0)
		dispno = 0;
	if (inputno<0)
		inputno = 0;
	CNCLWinInfo* temp;
	POSITION pos = m_pRedoList.GetHeadPosition();
	while (pos!=NULL)
	{
		temp = m_pRedoList.GetNext(pos);
		if (temp==NULL)
			break;
	}
	CNCLWinInfo *infoItem;
	infoItem = new CNCLWinInfo();
	infoItem->m_dtype = -1;
	infoItem->m_itype = redo_num;
	infoItem->m_type = frameno;
	infoItem->m_len = picno;
	infoItem->m_prec = dispno;
	infoItem->m_input_itemno = inputno;
	infoItem->m_page = pageno;
	infoItem->m_action = m_action;
/*
.....save selection info
*/
	infoItem->m_active = selitem;
	infoItem->m_pos[0] = seldtyp;
	infoItem->m_pos[1] = selityp;	
	m_pRedoList.AddTail(infoItem);
	if (redo_num!=0)
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableRedoBtn(1);
}


/***********************************************************************
**
**   FUNCTION: RedoItem()
**			Redo the last action
**		
**	 INPUT:  none
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::RedoItem()
{
	if (m_pRedoList.IsEmpty())
		return;
	CNCLWinInfo* pItem, *pItem2, *current_pItem, *current_pItem2;
	int selected, selitem, seldtyp, selityp, redo_num, frameno, picno, dispno, inputno, action, pageno;

	m_reset_redo = 0;
	pItem = m_pRedoList.GetTail();
	if (pItem->m_dtype!=-1)
	{
		MessageBox("Internal problem with Undo Function.", "Error", MB_OK);
		m_reset_redo = 1;
		return;
	}
	action = pItem->m_action;
	redo_num = pItem->m_itype;
	frameno = pItem->m_type;
	picno = pItem->m_len;
	dispno = pItem->m_prec;
	inputno = pItem->m_input_itemno;
	pageno = pItem->m_page;
	selitem = pItem->m_active;
	seldtyp = pItem->m_pos[0];
	selityp = pItem->m_pos[1];
	delete pItem;
	m_pRedoList.RemoveTail();
	selected = 0;
	m_reset_color = 1;
/*
.....following code will undo the action the end of the list with correct item no
*/
	int i, tempno, number;
	int old_frameno = m_frameno;
	int old_picno = m_picno;
	int old_dispno = m_dispno;
	int old_inputno = m_inputno;

	if (action==30)
	{
/*
.......REDO Add item by replace the existing one	(Redo delete and add)	
*/
		m_selnum = 0;
		pItem = m_pRedoList.GetTail();
		m_selarray[0] = pItem->m_input_itemno;
		m_seltype[0] = pItem->m_dtype;
		if (pItem->m_dtype==3)
		{
			m_pRedoList.RemoveTail();
			pItem2 = m_pRedoList.GetTail();
			if (pItem2==NULL)
			{
/*
......something wwrong with code
*/
				MessageBox("Wrong internally", "Error", MB_OK);
				m_reset_redo = 1;
				m_reset_color = 0;
				return;
			}
			SaveUndoItemInfo(pItem2);
		}
		m_selnum++;
		m_pRedoList.RemoveTail();
		m_selitem = selitem;
		m_seldtyp = seldtyp;
		m_no_undo_save = 1;
		OnDeleteItems();
		m_no_undo_save = 0;
/*
.....save after delete since delete will reset redo list
*/
		SaveUndoItemInfo(pItem);
/*
......then do the add back
*/
		pItem = m_pRedoList.GetTail();
/*
......undo a delete action
*/
/*
......Always put the input field field, then prompt, since some field don't have prompt field, easier for popup the list
......and undo
*/
		CreateWinFromInfo(pItem, dispno, frameno, picno, inputno);
		if (pItem->m_dtype==3)
		{
			m_pRedoList.RemoveTail();
			pItem2 = m_pRedoList.GetTail();
			if (pItem2==NULL)
			{
/*
......something wwrong with code
*/
				MessageBox("Wrong internally", "Error", MB_OK);
				m_reset_redo = 1;
				m_reset_color = 0;
				return;
			}
			CreateWinFromInfo(pItem2, dispno, frameno, picno, inputno);
			SaveUndoItemInfo(pItem2);
		}
		SaveUndoItemInfo(pItem);
		m_pRedoList.RemoveTail();
		m_action = 30;
		SaveUndoNumber(redo_num, frameno, picno, dispno, inputno, pageno, selitem, seldtyp, selityp);
		goto re_range;
	}
	if (action==31)
	{
/*
.......REDO Move item by replace the existing one	(Redo: delete and move)	
*/
		m_selnum = 0;
		pItem = m_pRedoList.GetTail();
		m_selarray[0] = pItem->m_input_itemno;
		m_seltype[0] = pItem->m_dtype;
		if (pItem->m_dtype==3)
		{
			m_pRedoList.RemoveTail();
			pItem2 = m_pRedoList.GetTail();
			if (pItem2==NULL)
			{
/*
......something wwrong with code
*/
				MessageBox("Wrong internally", "Error", MB_OK);
				m_reset_redo = 1;
				m_reset_color = 0;
				return;
			}
			SaveUndoItemInfo(pItem2);
		}
		m_selnum++;
		m_pRedoList.RemoveTail();
		m_selitem = selitem;
		m_seldtyp = seldtyp;
		m_no_undo_save = 1;
		OnDeleteItems();
		m_no_undo_save = 0;
/*
.....save after delete since delete will reset redo list
*/
		SaveUndoItemInfo(pItem);
/*
......then do item moving back
*/
		pItem = m_pRedoList.GetTail();
/*
......Redo a moving action
*/
		m_select_draw = 0;
		current_pItem = new CNCLWinInfo();
		UpdateWinProp(pItem, current_pItem, pItem->m_dtype, action);
		if (pItem->m_dtype==3)
		{
			SetWinPropFromInfo(pItem, dispno, frameno, picno, inputno);
			m_pRedoList.RemoveTail();
			pItem2 = m_pRedoList.GetTail();
			if ((pItem2==NULL)||(action != pItem2->m_action))
			{
/*
......something wwrong with code
*/
				MessageBox("Wrong internally", "Error", MB_OK);
				m_reset_redo = 1;
				m_reset_color = 0;
				return;
			}
			current_pItem2 = new CNCLWinInfo();
			UpdateWinProp(pItem2, current_pItem2, 4, action);
			SetWinPropFromInfo(pItem2, dispno, frameno, picno, inputno);
			SaveUndoItemInfo(current_pItem2);
			i = i++;
			delete pItem2;
		}
		SaveUndoItemInfo(current_pItem);
		SetWinPropFromInfo(pItem, dispno, frameno, picno, inputno);
		m_pRedoList.RemoveTail();
		delete pItem;
		m_action = 31;
		SaveUndoNumber(redo_num, frameno, picno, dispno, inputno, pageno, selitem, seldtyp, selityp);
		m_select_draw = 1;
		goto re_range;
	}
	if (action==21)
	{
/*
.....remove all items from redo list
*/
		for (i=0; i<redo_num;i++)
		{
			pItem = m_pRedoList.GetTail();
			if (pItem->m_dtype==3)
			{
				m_pRedoList.RemoveTail();
				pItem2 = m_pRedoList.GetTail();
				delete pItem2;
				i++;
			}
			delete pItem;
			m_pRedoList.RemoveTail();
		}
/*
......redo an section delete, delete the section with items
......and return
*/
		((CNCLFormView*)m_parent)->OnDeleteSec(pageno);
		goto done;
	}
	if (action==20)
	{
/*
......redo an section addition, add the section with no items in this section, 
......No items in this section, so just return;
*/
		((CNCLFormView*)m_parent)->OnInsertSec(pageno);
		goto done;
	}
	if (action==3)
	{
/*
......redo a delete action, for delete multi-item, we have to following OndeleteItems logic
......save all, then delete all, so use OndeleteItems, here we will just mark the delete items here 
......as selected, them deleted all selected
*/
		m_selnum = 0;
		for (i=0; i<redo_num;i++)
		{
			pItem = m_pRedoList.GetTail();
//temp 0119
//			if ((pItem->m_input_itemno==selitem)&&(pItem->m_dtype==seldtyp)&&(pItem->m_page==m_current_sec))
//				selected = 1;
			m_selarray[i] = pItem->m_input_itemno;
			m_seltype[i] = pItem->m_dtype;
			m_selnum++;
			if (pItem->m_dtype==3)
			{
				m_pRedoList.RemoveTail();
				pItem2 = m_pRedoList.GetTail();
				delete pItem2;
				i++;
			}
			delete pItem;
			m_pRedoList.RemoveTail();
		}
		m_selitem = selitem;
		m_seldtyp = seldtyp;
		OnDeleteItems();
		goto done;
	}
	for (i=0; i<redo_num;i++)
	{
		pItem = m_pRedoList.GetTail();
//temp 0119
//		if ((pItem->m_input_itemno==selitem)&&(pItem->m_dtype==seldtyp)&&(pItem->m_page==m_current_sec))
//			selected = 1;

		if ((action==1)||(action==20))
		{
/*
......redo an add action, add the item again
*/
/*
......Always put the input field field, then prompt, since some field don't have prompt field, easier for popup the list
......and undo
*/
			CreateWinFromInfo(pItem, dispno, frameno, picno, inputno);
			if (pItem->m_dtype==3)
			{
				m_pRedoList.RemoveTail();
				pItem2 = m_pRedoList.GetTail();
				if (pItem2==NULL)
				{
/*
......something wwrong with code
*/
					MessageBox("Wrong internally", "Error", MB_OK);
					m_reset_color = 0;
					return;
				}
				CreateWinFromInfo(pItem2, dispno, frameno, picno, inputno);
				SaveUndoItemInfo(pItem2);
				i++;
			}
			SaveUndoItemInfo(pItem);
			m_pRedoList.RemoveTail();
		}
		if (action==2)
		{
			m_select_draw = 0;
/*
......redo a moving (move to the old position)
*/
			current_pItem = new CNCLWinInfo();
			UpdateWinProp(pItem, current_pItem, pItem->m_dtype, action);
			if (pItem->m_dtype==3)
			{
/*note wrong page number save for prompt and input, should be same but different//
need check here, how undo is save the page*/
				SetWinPropFromInfo(pItem, dispno, frameno, picno, inputno);
				m_pRedoList.RemoveTail();
				pItem2 = m_pRedoList.GetTail();
				if ((pItem2==NULL)||(action != pItem2->m_action))
				{
/*
......something wwrong with code
*/
					MessageBox("Wrong internally", "Error", MB_OK);
					m_reset_color = 0;
					return;
				}
				current_pItem2 = new CNCLWinInfo();
				UpdateWinProp(pItem2, current_pItem2, 4, action);
				SetWinPropFromInfo(pItem2, dispno, frameno, picno, inputno);
				SaveUndoItemInfo(current_pItem2);
				i = i++;
				delete pItem2;
			}
			SaveUndoItemInfo(current_pItem);
			SetWinPropFromInfo(pItem, dispno, frameno, picno, inputno);
			m_pRedoList.RemoveTail();
/*
.....reselect to item number only
*/
//			OnSelectItem(pItem->m_input_itemno, pItem->m_dtype, pItem->m_itype, 1);
			delete pItem;
			m_select_draw = 1;
		}
		if ((action>=4)&&(action<20))
		{
/*
......redo a edit action
*/
			current_pItem = new CNCLWinInfo();
			UpdateWinProp(pItem, current_pItem, pItem->m_dtype, action);
			if (pItem->m_dtype==3)
			{
				m_pRedoList.RemoveTail();
				pItem2 = m_pRedoList.GetTail();
				if ((pItem2==NULL)||(action != pItem2->m_action))
				{
/*
......something wwrong with code
*/
					MessageBox("Wrong internally", "Error", MB_OK);
					m_reset_color = 0;
					return;
				}
				current_pItem2 = new CNCLWinInfo();
				UpdateWinProp(pItem2, current_pItem2, 4, action);
				SaveUndoItemInfo(current_pItem2);
				SetWinPropFromInfo(pItem2, dispno, frameno, picno, inputno);
				i = i++;
				delete pItem2;
			}
			SaveUndoItemInfo(current_pItem);
			SetWinPropFromInfo(pItem, dispno, frameno, picno, inputno);
			m_pRedoList.RemoveTail();
/*
.....reselect to item number only
*/
//			OnSelectItem(pItem->m_input_itemno, pItem->m_dtype, pItem->m_itype, 0);
			delete pItem;
		}
	}
	m_action = action;
	SaveUndoNumber(redo_num, frameno, picno, dispno, inputno, pageno, selitem, seldtyp, selityp);
selected:;
//temp 0119
/*
	if (selected==1)
	{
		if ((redo_num==1)&& (action!=1))
		{
			OnSelectItem(selitem, seldtyp, selityp, 0);
		}
		else
		{
			OnSelectItem(selitem, seldtyp, selityp, 1);
		}
	}
*/
	if ((action!=1)&&(action!=20))
	{
		goto done;
	}
re_range:;
/*
......re-arrange the item by merge the new item (just created above) into the current list
......from the low index
*/
	int type, itemno, insert_item_no;
/*
......find the smallest index, then merge
*/
check_disp:;
	if (m_dispno==0)
		goto check_frame;
	insert_item_no=201;
	for (i=old_dispno; i<dispno+m_dispno; i++)
	{
		if (m_disp_win[i] != NULL)
		{
			if (insert_item_no>=((CNCLDDStatic*)m_disp_win[i])->m_itemno)
			{
				insert_item_no = ((CNCLDDStatic*)m_disp_win[i])->m_itemno;
				itemno = i;
			}
		}
		else if (i<m_dispno)
		{
			insert_item_no = i;
		}
	}
/*
......merge this into list
*/
	if (insert_item_no!=201)
	{
		if (insert_item_no==itemno)
			goto check_frame;
		type = 0;
		MoveItemPlace(insert_item_no, itemno, type);
		goto check_disp;
	}

check_frame:;
	if (m_frameno==0)
		goto check_pic;
	insert_item_no=201;
	for (i=old_frameno; i<frameno+m_frameno; i++)
	{
		if (m_frame[i] != NULL)
		{
			if (insert_item_no>=((CNCLDDGroup*)m_frame[i])->m_itemno)
			{
				insert_item_no = ((CNCLDDGroup*)m_frame[i])->m_itemno;
				itemno = i;
			}
		}
		else if (i<m_frameno)
		{
			insert_item_no = i;
		}
	}
/*
......merge this into list
*/
	if (insert_item_no!=201)
	{
		if (insert_item_no==itemno)
			goto check_pic;
		type = 1;
		MoveItemPlace(insert_item_no, itemno, type);
		goto check_frame;
	}
check_pic:;
	if (m_picno==0)
		goto check_input;
	insert_item_no=201;
	for (i=old_picno; i<picno+m_picno; i++)
	{
		if (m_picture[i] != NULL)
		{
			if (insert_item_no>=((CNCLDDPicWin*)m_picture[i])->m_itemno)
			{
				insert_item_no = ((CNCLDDPicWin*)m_picture[i])->m_itemno;
				itemno = i;
			}
		}
		else if (i<m_picno)
		{
			insert_item_no = i;
		}
	}
/*
......merge this into list
*/
	if (insert_item_no!=201)
	{
		if (insert_item_no==itemno)
			goto check_input;
		type = 2;
		MoveItemPlace(insert_item_no, itemno, type);
		goto check_pic;
	}
check_input:;
	if (m_inputno==0)
		goto done;
	insert_item_no=201;
	for (i=old_inputno; i<inputno+m_inputno; i++)
	{
		if (m_inpwin[i] != NULL)
		{
			if ((m_type[i]==1)||(m_type[i]==25))
			{
				tempno = ((CNCLDDButton*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==2)
			{
				tempno = ((CNCLDDCombo*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==3)
			{
				tempno = ((CNCLDDEdit*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==5)
			{
				tempno = ((CNCLDDListBox*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==7)
			{
				tempno = ((CNCLDDButton*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==8)
			{
				tempno = ((CNCLDDCombo*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==9)
			{
				tempno = ((CNCLDDCombo*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==11)
			{
				tempno = ((CNCLDDEdit*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==13)
			{
				tempno = ((CNCLDDProcess*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==15)
			{
				tempno = ((CNCLDDCombo*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==16)
			{
				tempno = ((CNCLDDEdit*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==17)
			{
				tempno = ((CNCLDDListCtrl*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==18)
			{
				tempno = ((CNCLDDColorButton*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==19)
			{
				tempno = ((CNCLDDListCtrl2*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==23)
			{
				tempno = ((CNCLDDColorButton*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==24)
			{
				tempno = ((CNCLDDSlider*)m_inpwin[i])->m_itemno;
			}
			else
				tempno = 201;

			if (insert_item_no>=tempno)
			{
				insert_item_no = tempno;
				itemno = i;
			}
		}
		else if (i<m_inputno)
		{
			insert_item_no = i;
		}
	}
/*
......merge this into list
*/
	if (insert_item_no!=201)
	{
		if (insert_item_no==itemno)
			goto done;
		type = 4;
		MoveItemPlace(insert_item_no, itemno, type);
		goto check_input;
	}
done:;
	if (selected==1)
	{
		if ((redo_num==1)&& (action!=1))
		{
			OnSelectItem(selitem, seldtyp, selityp, 0);
		}
		else
		{
			OnSelectItem(selitem, seldtyp, selityp, 1);
		}
	}
	updateWindow_draw();
	if (m_selnum<=1)
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(0);
	}
	else
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(1);
	}
	CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
	pview->SetFormInputNo(m_inputno);
	pview->SetFormSecNo(m_secno);
	if (m_selnum<=0)
	{
		pview->SetDtype(-2);
		pview->filldata();
	}
	if (m_pRedoList.IsEmpty())
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableRedoBtn(0);
	}
	m_reset_redo = 1;
	m_reset_color = 0;
}

/***********************************************************************
**
**   FUNCTION: UndoItem()
**			Undo the last undo action
**		
**	 INPUT:  none
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::UndoItem()
{
	if (m_pUndoList.IsEmpty())
		return;
	CNCLWinInfo* pItem;
	int selected, undo_num, frameno, picno, dispno, inputno, action, pageno;
	int i, selitem, seldtyp, selityp;

	pItem = m_pUndoList.GetTail();
	if (pItem->m_dtype!=-1)
	{
		MessageBox("Internal problem with Undo Function.", "Error", MB_OK);
		return;
	}
	m_reset_redo = 0;
	m_reset_color = 1;
	selected = 0;
	action = pItem->m_action;
	undo_num = pItem->m_itype;
	frameno = pItem->m_type;
	picno = pItem->m_len;
	dispno = pItem->m_prec;
	inputno = pItem->m_input_itemno;
	pageno = pItem->m_page;
	selitem = pItem->m_active;
	seldtyp = pItem->m_pos[0];
	selityp = pItem->m_pos[1];
	CString label = pItem->m_label;
	CString color = pItem->m_color;

	if (action==20)
	{
/*
......undo section addition
......save this item to redo stack
*/
		SaveRedoItem(pItem);
	}
	else
		delete pItem;
	m_pUndoList.RemoveTail();
	CNCLWinInfo* pItem2, *pItem3, *pItem4;
	CNCLWinInfo* current_pItem, * current_pItem2;
/*
.....following code will undo the action the end of the list with correct item no
*/
	int tempno, number;
	int old_frameno = m_frameno;
	int old_picno = m_picno;
	int old_dispno = m_dispno;
	int old_inputno = m_inputno;
	
	if (action==30)
	{
/*
.......Add item by replace the existing one	(Undo add and delete)	
*/
		m_selnum = 0;
		pItem = m_pUndoList.GetTail();
		m_selarray[0] = pItem->m_input_itemno;
		m_seltype[0] = pItem->m_dtype;
		if (pItem->m_dtype==3)
		{
			m_pUndoList.RemoveTail();
			pItem2 = m_pUndoList.GetTail();
			if (pItem2==NULL)
			{
/*
......something wwrong with code
*/
				MessageBox("Wrong internally", "Error", MB_OK);
				m_reset_color = 0;
				return;
			}
			SaveRedoItem(pItem2);
		}
		m_selnum++;
		m_pUndoList.RemoveTail();
		m_selitem = selitem;
		m_seldtyp = seldtyp;
		m_no_undo_save = 1;
		OnDeleteItems();
		m_no_undo_save = 0;
/*
.....save after delete since delete will reset redo list
*/
		SaveRedoItem(pItem);
/*
......then do the add back
*/
		pItem = m_pUndoList.GetTail();
/*
......undo a delete action
*/
/*
......Always put the input field field, then prompt, since some field don't have prompt field, easier for popup the list
......and undo
*/
		CreateWinFromInfo(pItem, dispno, frameno, picno, inputno);
		if (pItem->m_dtype==3)
		{
			m_pUndoList.RemoveTail();
			pItem2 = m_pUndoList.GetTail();
			if (pItem2==NULL)
			{
/*
......something wwrong with code
*/
				MessageBox("Wrong internally", "Error", MB_OK);
				m_reset_color = 0;
				return;
			}
			CreateWinFromInfo(pItem2, dispno, frameno, picno, inputno);
			SaveRedoItem(pItem2);
		}
		SaveRedoItem(pItem);
		m_pUndoList.RemoveTail();
		m_action = 30;
		SaveRedoNumber(undo_num, frameno, picno, dispno, inputno, pageno, selitem, seldtyp, selityp);
		goto re_range;
	}
	if (action==31)
	{
/*
.......Move item & replace the existing one	(Undo add and Moving)	
*/
/*
......first undo moving
*/
		m_select_draw = 0;
		pItem = m_pUndoList.GetTail();
		current_pItem = new CNCLWinInfo();
		UpdateWinProp(pItem, current_pItem, pItem->m_dtype, pItem->m_action);
		if (pItem->m_dtype==3)
		{
			SetWinPropFromInfo(pItem, dispno, frameno, picno, inputno);
			m_pUndoList.RemoveTail();
			pItem2 = m_pUndoList.GetTail();
			if (pItem2==NULL)
			{
/*
......something wwrong with code
*/
				MessageBox("Wrong internally", "Error", MB_OK);
				m_reset_color = 0;
				return;
			}
			current_pItem2 = new CNCLWinInfo();
			UpdateWinProp(pItem2, current_pItem2, 4, action);
			SetWinPropFromInfo(pItem2, dispno, frameno, picno, inputno);
			SaveRedoItem(current_pItem2);
			delete pItem2;
		}
		SetWinPropFromInfo(pItem, dispno, frameno, picno, inputno);
		SaveRedoItem(current_pItem);
		m_pUndoList.RemoveTail();
		delete pItem;
		m_select_draw = 1;
/*
......then do the add back
*/
		pItem = m_pUndoList.GetTail();
/*
......undo a delete action
*/
/*
......Always put the input field field, then prompt, since some field don't have prompt field, easier for popup the list
......and undo
*/
		CreateWinFromInfo(pItem, dispno, frameno, picno, inputno);
		if (pItem->m_dtype==3)
		{
			m_pUndoList.RemoveTail();
			pItem2 = m_pUndoList.GetTail();
			if (pItem2==NULL)
			{
/*
......something wwrong with code
*/
				MessageBox("Wrong internally", "Error", MB_OK);
				m_reset_color = 0;
				return;
			}
			CreateWinFromInfo(pItem2, dispno, frameno, picno, inputno);
			SaveRedoItem(pItem2);
		}
		SaveRedoItem(pItem);
		m_pUndoList.RemoveTail();
		m_action = 31;
		SaveRedoNumber(undo_num, frameno, picno, dispno, inputno, pageno, selitem, seldtyp, selityp);
		goto re_range;
	}
	if (action==20)
	{
/*
......Undo an section addition, delete the section (no items in this section yet), so just delete this section
......and return
*/
		m_no_undo_save = 1;
		((CNCLFormView*)m_parent)->OnDeleteSec(pageno);
		m_no_undo_save = 0;
		m_reset_color = 0;
		return;
	}
	if (action==21)
	{
/*
......Undo an section delete, add the section (with no items in this section), 
......then continue to add all items deleted from this section
*/
		m_no_undo_save = 1;
		((CNCLFormView*)m_parent)->OnInsertSec(pageno);
		((CNCLFormView*)m_parent)->SetSectionAttr(pageno, label, color);
		m_no_undo_save = 0;
		goto undo_items;
	}
undo_items:;
	if (action==1)
	{
/*
......Undo an add action (delete those items), for delete multi-item, we have to following OndeleteItems logic
......save all, then delete all, so use OndeleteItems, here we will just mark the delete items here 
......as selected, them deleted all selected
*/
		m_selnum = 0;
		for (i=0; i<undo_num;i++)
		{
			pItem = m_pUndoList.GetTail();
//temp 0119
//			if ((pItem->m_input_itemno==selitem)&&(pItem->m_dtype==seldtyp)&&(pItem->m_page==m_current_sec))
//				selected = 1;
			m_selarray[i] = pItem->m_input_itemno;
			m_seltype[i] = pItem->m_dtype;
			if (pItem->m_dtype==3)
			{
				m_pUndoList.RemoveTail();
				pItem2 = m_pUndoList.GetTail();
				if (pItem2==NULL)
				{
/*
......something wwrong with code
*/
					MessageBox("Wrong internally", "Error", MB_OK);
					m_reset_color = 0;
					return;
				}
				SaveRedoItem(pItem2);
				i++;
			}
			m_selnum++;
			SaveRedoItem(pItem);
			m_pUndoList.RemoveTail();
		}
		m_selitem = selitem;
		m_seldtyp = seldtyp;
		m_no_undo_save = 1;
		OnDeleteItems();
		m_no_undo_save = 0;
		m_action = action;
		SaveRedoNumber(undo_num, frameno, picno, dispno, inputno, pageno, selitem, seldtyp, selityp);
		goto done;
	}
	for (int i=0; i<undo_num;i++)
	{
		pItem = m_pUndoList.GetTail();
//temp 0119
//		if ((pItem->m_input_itemno==selitem)&&(pItem->m_dtype==seldtyp)&&(pItem->m_page==m_current_sec))
//			selected = 1;
		if ((action==3)||(action==21))
		{
/*
......undo a delete action
*/
/*
......Always put the input field field, then prompt, since some field don't have prompt field, easier for popup the list
......and undo
*/
			CreateWinFromInfo(pItem, dispno, frameno, picno, inputno);
			if (pItem->m_dtype==3)
			{
				m_pUndoList.RemoveTail();
				pItem2 = m_pUndoList.GetTail();
				if (pItem2==NULL)
				{
/*
......something wwrong with code
*/
					MessageBox("Wrong internally", "Error", MB_OK);
					m_reset_color = 0;
					return;
				}
				CreateWinFromInfo(pItem2, dispno, frameno, picno, inputno);
				SaveRedoItem(pItem2);
				i++;
			}
			SaveRedoItem(pItem);
			m_pUndoList.RemoveTail();
		}
		if (action==2)
		{
/*
......undo a moving (move to the old position)
*/
			m_select_draw = 0;
			current_pItem = new CNCLWinInfo();
			UpdateWinProp(pItem, current_pItem, pItem->m_dtype, action);
			if (pItem->m_dtype==3)
			{
				SetWinPropFromInfo(pItem, dispno, frameno, picno, inputno);
				m_pUndoList.RemoveTail();
				pItem2 = m_pUndoList.GetTail();
				if ((pItem2==NULL)||(action != pItem2->m_action))
				{
/*
......something wwrong with code
*/
					MessageBox("Wrong internally", "Error", MB_OK);
					m_reset_color = 0;
					return;
				}
				current_pItem2 = new CNCLWinInfo();
				UpdateWinProp(pItem2, current_pItem2, 4, action);
				SetWinPropFromInfo(pItem2, dispno, frameno, picno, inputno);
				SaveRedoItem(current_pItem2);
				i = i++;
				delete pItem2;
			}
			SetWinPropFromInfo(pItem, dispno, frameno, picno, inputno);
			SaveRedoItem(current_pItem);
			m_pUndoList.RemoveTail();
			delete pItem;
			m_select_draw = 1;
		}
		if ((action>=4)&&(action<20))
		{
/*
......undo a edit action
*/
/*
.....get the current win item info
*/
			current_pItem = new CNCLWinInfo();
			UpdateWinProp(pItem, current_pItem, pItem->m_dtype, action);
			if (pItem->m_dtype==3)
			{
				m_pUndoList.RemoveTail();
				pItem2 = m_pUndoList.GetTail();
				if ((pItem2==NULL)||(action != pItem2->m_action))
				{
/*
......something wwrong with code
*/
					MessageBox("Wrong internally", "Error", MB_OK);
					m_reset_color = 0;
					return;
				}
				current_pItem2 = new CNCLWinInfo();
				UpdateWinProp(pItem2, current_pItem2, 4, action);
				SaveRedoItem(current_pItem2);
				SetWinPropFromInfo(pItem2, dispno, frameno, picno, inputno);
				i = i++;
				delete pItem2;
			}
			SaveRedoItem(current_pItem);
			SetWinPropFromInfo(pItem, dispno, frameno, picno, inputno);
			m_pUndoList.RemoveTail();
			delete pItem;
		}
//temp 0119
//		if ((selected==1)&&(action!=3))
//		{
//			if (undo_num==1)
//				OnSelectItem(selitem, seldtyp, selityp, 0);
//			else
//				OnSelectItem(selitem, seldtyp, selityp, 1);
//		}
	}
	m_action = action;
	SaveRedoNumber(undo_num, frameno, picno, dispno, inputno, pageno, selitem, seldtyp, selityp);
	if ((action!=3)&&(action!=21))
	{
		goto done;
	}
re_range:;
/*
......re-arrange the item by merge the new item (just created above) into the current list
......from the low index
*/
	int j,type, itemno, insert_item_no;
/*
......find the smallest index, then merge
*/
check_disp:;
	if (m_dispno==0)
		goto check_frame;
	insert_item_no=201;
	for (i=old_dispno; i<dispno+m_dispno; i++)
	{
		if (m_disp_win[i] != NULL)
		{
			if (insert_item_no>((CNCLDDStatic*)m_disp_win[i])->m_itemno)
			{
				insert_item_no = ((CNCLDDStatic*)m_disp_win[i])->m_itemno;
				itemno = i;
			}
		}
		else
		{
			insert_item_no = i;
		}
	}
/*
......merge this into list
*/
	if (insert_item_no!=201)
	{
		if (insert_item_no==itemno)
			goto check_frame;
		type = 0;
		MoveItemPlace(insert_item_no, itemno, type);
		goto check_disp;
	}

check_frame:;
	if (m_frameno==0)
		goto check_pic;
	insert_item_no=201;
	for (i=old_frameno; i<frameno+m_frameno; i++)
	{
		if (m_frame[i] != NULL)
		{
			if (insert_item_no>((CNCLDDGroup*)m_frame[i])->m_itemno)
			{
				insert_item_no = ((CNCLDDGroup*)m_frame[i])->m_itemno;
				itemno = i;
			}
		}
		else
		{
			insert_item_no = i;
		}
	}
/*
......merge this into list
*/
	if (insert_item_no!=201)
	{
		if (insert_item_no==itemno)
			goto check_pic;
		type = 1;
		MoveItemPlace(insert_item_no, itemno, type);
		goto check_frame;
	}
check_pic:;
	if (m_picno==0)
		goto check_input;
	insert_item_no=201;
	for (i=old_picno; i<picno+m_picno; i++)
	{
		if (m_picture[i] != NULL)
		{
			if (insert_item_no>((CNCLDDPicWin*)m_picture[i])->m_itemno)
			{
				insert_item_no = ((CNCLDDPicWin*)m_picture[i])->m_itemno;
				itemno = i;
			}
		}
		else
		{
			insert_item_no = i;
		}
	}
/*
......merge this into list
*/
	if (insert_item_no!=201)
	{
		if (insert_item_no==itemno)
			goto check_input;
		type = 2;
		MoveItemPlace(insert_item_no, itemno, type);
		goto check_pic;
	}
check_input:;
	if (m_inputno==0)
		goto done;
	insert_item_no=201;
	for (i=old_inputno; i<inputno+m_inputno; i++)
	{
		if (m_inpwin[i] != NULL)
		{
			if ((m_type[i]==1)||(m_type[i]==25))
			{
				tempno = ((CNCLDDButton*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==2)
			{
				tempno = ((CNCLDDCombo*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==3)
			{
				tempno = ((CNCLDDEdit*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==5)
			{
				tempno = ((CNCLDDListBox*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==7)
			{
				tempno = ((CNCLDDButton*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==8)
			{
				tempno = ((CNCLDDCombo*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==9)
			{
				tempno = ((CNCLDDCombo*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==11)
			{
				tempno = ((CNCLDDEdit*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==13)
			{
				tempno = ((CNCLDDProcess*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==15)
			{
				tempno = ((CNCLDDCombo*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==16)
			{
				tempno = ((CNCLDDEdit*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==17)
			{
				tempno = ((CNCLDDListCtrl*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==18)
			{
				tempno = ((CNCLDDColorButton*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==19)
			{
				tempno = ((CNCLDDListCtrl2*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==23)
			{
				tempno = ((CNCLDDColorButton*)m_inpwin[i])->m_itemno;
			}
			else if (m_type[i]==24)
			{
				tempno = ((CNCLDDSlider*)m_inpwin[i])->m_itemno;
			}
			else
				tempno = 201;

			if (insert_item_no>=tempno)
			{
				insert_item_no = tempno;
				itemno = i;
			}
		}
		else if (i<m_inputno)
		{
			insert_item_no = i;
		}
	}
/*
......merge this into list
*/
	if (insert_item_no!=201)
	{
		if (insert_item_no==itemno)
			goto done;
		type = 4;
		MoveItemPlace(insert_item_no, itemno, type);
		goto check_input;
	}
done:;
//temp 0119	
	if ((selected==1)&&(action!=3))
	{
		if (undo_num==1)
			OnSelectItem(selitem, seldtyp, selityp, 0);
		else
			OnSelectItem(selitem, seldtyp, selityp, 1);
	}
	updateWindow_draw();
	if (m_selnum<=1)
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(0);
	}
	else
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(1);
	}
	CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
	pview->SetFormInputNo(m_inputno);
	pview->SetFormSecNo(m_secno);
	if (m_selnum<=0)
	{
		pview->SetDtype(-2);
		pview->filldata();
	}
	if (m_pUndoList.IsEmpty())
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableUndoBtn(0);
	}
	m_reset_redo = 1;
	m_reset_color = 0;
}
void CNCLDDform::SaveUndoItem2(int action)
{
	if (m_no_undo_save==1)
		return;
	if (m_selnum==0)
		return;
		
	m_reset_redo = 1;
	Remove_redoList();

	int frameno=0, picno=0, dispno=0, inputno=0;
	int item_no = m_selarray[0];
	int type = m_seltype[0];
	m_action_item = item_no;
	m_action_typ = type;
	m_action = action;
	int number = SaveUndoItem();
	if (type==0)
		dispno++;
	else if (type==1)
		frameno++;
	else if (type==2)
		picno++;
	else if ((number>0)&&((type==3)||(type==4)))
		inputno++;
	if (m_seldtyp!=4)
		SaveUndoNumber(number, frameno, picno, dispno, inputno, m_current_sec, 
				m_selitem, m_seldtyp, -1);
	else
		SaveUndoNumber(number, frameno, picno, dispno, inputno, m_current_sec, 
				m_selitem, m_seldtyp, m_type[m_selitem]);
}

void CNCLDDform::SaveUndoItems(int action /*add,delete, edit..*/, int dtype, int itype, int item_no, int extra_no)
{
	if (m_no_undo_save==1)
		return;
	m_action_item = item_no;
	m_action_typ = dtype;
	m_action = action;
	int type = dtype;
	int frameno=0, picno=0, dispno=0, inputno=0;
	int number, selityp = -1;
	int selitem = m_selitem;
	int seldtyp = m_seldtyp;
	if ((seldtyp==4)&&(selitem>=0))
		selityp = m_type[selitem];

	if ((action==1)||(action>=4))
	{
		m_action_item = item_no; 
		m_action_typ = dtype;
		number = SaveUndoItem();
/*
......add(new) or edit a field
*/
		if (type==0)
		{
			dispno++;
		}
		else if (type==1)
		{
			frameno++;
		}
		else if (type==2)
		{
			picno++;
		}
		else if (type==4)
		{
			inputno++;
		}
		if (extra_no>0)
			m_action = 30;
		SaveUndoNumber(number+extra_no, frameno, picno, dispno, inputno, m_current_sec, 
				selitem, seldtyp, selityp);
		return;
	}
	int selected, orig_num, orig_type;
	orig_num = m_action_item;
	orig_type = m_action_typ;
	selected = 0;
	m_undo_num = 0;
	int sel = m_selnum-1;
	for (int i=0; i<m_selnum; i++)
	{
		item_no = m_selarray[i];
		type = m_seltype[i];
		m_action_item = item_no;
		m_action_typ = type;
		number = SaveUndoItem();
		m_undo_num += number;
		if (type==0)
			dispno++;
		else if (type==1)
			frameno++;
		else if (type==2)
			picno++;
		else if ((number>0)&&((type==3)||(type==4)))
			inputno++;
		if ((orig_num==m_action_item)&&(orig_type==m_action_typ))
			selected = 1;
	}
	if (selected==0)
	{
		m_action_item = orig_num;
		m_action_typ = orig_type;
		number = SaveUndoItem();
		if (m_action_typ==0)
		{
			dispno++;
		}
		else if (m_action_typ==1)
		{
			frameno++;
		}
		else if (m_action_typ==2)
		{
			picno++;
		}
		else if (m_action_typ==4)
		{
			inputno++;
		}
		m_undo_num += number;
	}
	if (extra_no>0)
		m_action = 30;
	SaveUndoNumber(m_undo_num+extra_no, frameno, picno, dispno, inputno, m_current_sec, 
			selitem, seldtyp, selityp);
}

/***********************************************************************
**
**   FUNCTION: SaveUndoItem()
**			Save the undo item
**		
**	 INPUT:  none
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CNCLDDform::SaveUndoItem()
{
	if (m_no_undo_save==1)
		return 0;
	int number = 0;
	int item_no = m_action_item;
	int type = m_action_typ;
/*
.....if the item_no&type is already saved
.....ignore
*/
	CNCLWinInfo* temp;
	POSITION pos = m_pUndoList.GetHeadPosition();
	while (pos!=NULL)
	{
		temp = m_pUndoList.GetNext(pos);
		if (temp==NULL)
			break;
	}
	m_undo_itemno = item_no;
	m_undo_typ = type;

	CNCLFormProp *prop_dlg, *dprop_dlg;
	CNCLWinInfo *pItem, *iItem;
	if (type==0)
	{
		if (m_disp_win[item_no]!=NULL)
		{
			pItem = new CNCLWinInfo();
			prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_disp_win[item_no]))->GetPropertyPage());
			prop_dlg->GetWinInfo(pItem);
			pItem->m_action = m_action;
			pItem->m_page = m_current_sec;
			m_pUndoList.AddTail(pItem);
			number++;
		}
	}
	if (type==1)
	{
		if (m_frame[item_no]!=NULL)
		{
			pItem = new CNCLWinInfo();
			prop_dlg = ((CNCLFormProp *)((CNCLDDGroup*)(m_frame[item_no]))->GetPropertyPage());
			prop_dlg->GetWinInfo(pItem);
			pItem->m_action = m_action;
			pItem->m_page = m_current_sec;
			m_pUndoList.AddTail(pItem);
			number++;
		}
	}
	if (type==2)
	{
		if (m_picture[item_no]!=NULL)
		{
			pItem = new CNCLWinInfo();
			prop_dlg = ((CNCLFormProp *)((CNCLDDPicWin*)(m_picture[item_no]))->GetPropertyPage());
			prop_dlg->GetWinInfo(pItem);
			pItem->m_action = m_action;
			pItem->m_page = m_current_sec;
			m_pUndoList.AddTail(pItem);
			number++;
		}
	}
/*
......if it is input type, need save both
*/
	if ((type==3)||(type==4))
	{
/*
......put the input field field, then prompt, since some field don't have prompt field, easier for popup the list
......and undo
*/
		if (m_inpwin[item_no]!=NULL)
		{
			iItem = new CNCLWinInfo();
			if ((m_type[item_no]==1)||(m_type[item_no]==25))
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==2)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==3)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==5)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListBox*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==7)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==8)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==9)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==11)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==13)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDProcess*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==15)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==16)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==17)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==18)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==23)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==24)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDSlider*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==19)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl2*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			if (!((m_type[item_no]==5) ||(m_type[item_no] == 19)||(m_type[item_no]==17)))
			{
				if (m_dinpwin[item_no]!=NULL)
				{
					if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
						dprop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item_no]))->GetPropertyPage());
					else if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
						dprop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());
/*
......input label must same as prompt label (when first time, the input will be "", so make it consistense
*/
					prop_dlg->SetLabelValue(dprop_dlg->m_label, dprop_dlg->m_pcolor);
				}
			}
			prop_dlg->GetWinInfo(iItem);
			iItem->m_action = m_action;
			iItem->m_page = m_current_sec;
			m_pUndoList.AddTail(iItem);
			number++;
		}
		if (m_dinpwin[item_no]!=NULL)
		{
			pItem = new CNCLWinInfo();
			if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item_no]))->GetPropertyPage());
			else if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());					
			prop_dlg->GetWinInfo(pItem);
			pItem->m_action = m_action;
			pItem->m_page = m_current_sec;
			m_pUndoList.AddTail(pItem);
			number++;
		}
	}
	return number;
}

void CNCLDDform::GetInputItemLabel(int item_no, CString &label)
{
	CNCLFormProp *prop_dlg = NULL;
	label = "";
	if (m_dinpwin[item_no] != NULL)
	{
		if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
			prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item_no]))->GetPropertyPage());
		else if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
			prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());					
		if (prop_dlg!=NULL)
			label = prop_dlg->m_label;
		return;
	}
/*
.....only few item does have prompt
*/
	if (m_inpwin[item_no] != NULL)
	{
		if ((m_type[item_no]==1)||(m_type[item_no]==5)||(m_type[item_no]==7)||(m_type[item_no]==17)
			|| (m_type[item_no]==25)||(m_type[item_no]==19))
		{
			if (m_inpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDListBox)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDListBox*)(m_inpwin[item_no]))->GetPropertyPage());
			else if (m_inpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());					
			else if (m_inpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDListCtrl)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());					
			else if (m_inpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDListCtrl2)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());					
		}
		if (prop_dlg!=NULL)
			label = prop_dlg->m_label;
	}
}
int CNCLDDform::GetItemWinInfo(int item, CNCLWinInfo *del_info1, CNCLWinInfo *del_info2)
{
	CRect iRect;
	CNCLFormProp *prop_dlg;
	int num=0;

	if (m_dinpwin[item] != NULL)
	{
		if (del_info1!=NULL)
		{
			if (m_dinpwin[item]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item]))->GetPropertyPage());
			else if (m_dinpwin[item]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item]))->GetPropertyPage());
			prop_dlg->GetWinInfo(del_info1);
			num++;
		}
	}
	if (m_inpwin[item] != NULL)
	{
		if (del_info2!=NULL)
		{
			if (m_inpwin[item]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_inpwin[item]))->GetPropertyPage());
			else if (m_inpwin[item]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item]))->GetPropertyPage());
			else if (m_inpwin[item]->IsKindOf(RUNTIME_CLASS(CNCLDDCombo)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item]))->GetPropertyPage());
			else if (m_inpwin[item]->IsKindOf(RUNTIME_CLASS(CNCLDDEdit)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item]))->GetPropertyPage());
			else if (m_inpwin[item]->IsKindOf(RUNTIME_CLASS(CNCLDDListBox)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDListBox*)(m_inpwin[item]))->GetPropertyPage());
			else if (m_inpwin[item]->IsKindOf(RUNTIME_CLASS(CNCLDDListCtrl)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl*)(m_inpwin[item]))->GetPropertyPage());
			else if (m_inpwin[item]->IsKindOf(RUNTIME_CLASS(CNCLDDListCtrl2)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl2*)(m_inpwin[item]))->GetPropertyPage());
			else if (m_inpwin[item]->IsKindOf(RUNTIME_CLASS(CNCLDDColorButton)))
				prop_dlg = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[item]))->GetPropertyPage());		
			prop_dlg->GetWinInfo(del_info2);
			num++;
		}
	}
	return num;
}

int CNCLDDform::HandleHotSpot(CPoint pt, char *class_str, int itemno)
{
/*
......first check if the current type is the right type;
*/
	if (!((strcmp(class_str, "CNCLDDButton")==0)
		|| (strcmp(class_str, "CNCLDDColorButton")==0)
		|| (strcmp(class_str, "CNCLDDCombo")==0)
		|| (strcmp(class_str, "CNCLDDEdit")==0)
		|| (strcmp(class_str, "CNCLDDListBox")==0)
		|| (strcmp(class_str, "CNCLDDListCtrl")==0)
		|| (strcmp(class_str, "CNCLDDListCtrl2")==0)))
	{
		return -1;
	}
/*
.....not handle prompt
*/
	if ((m_dinpwin[itemno]!=NULL)&&(strcmp(class_str, "CNCLDDButton")==0)
			&&(m_dinpwin[itemno]->IsKindOf(RUNTIME_CLASS(CNCLDDButton))))
		return -1;
/*
......check if the pt is on the picture item
*/
	int pic_indx = -1;
	CRect iRect;
	for (int i=0; i<m_picno; i++)
	{
		if (m_picture[i] != NULL)
		{
			m_picture[i]->GetWindowRect(&iRect);
			if (iRect.PtInRect(pt))
			{
				pic_indx = i;
				break;
			}
		}
	}
	if (pic_indx==-1)
		return -1;
	m_action_item = itemno;
	m_action_typ = m_seltype[0];
	m_action = 18;
	int undo_num = SaveUndoItem();
	SaveUndoNumber(undo_num, m_frameno, m_picno, m_dispno, m_inputno, m_current_sec, 
								m_selitem, m_seldtyp, m_type[itemno]);

	CNCLFormProp *prop_dlg;
	CRect rect;
	if (strcmp(class_str, "CNCLDDButton")==0)
	{
		prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[itemno]))->GetPropertyPage());	
	}
	else if (strcmp(class_str, "CNCLDDColorButton")==0)
	{
		prop_dlg = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[itemno]))->GetPropertyPage());	
	}
	else if (strcmp(class_str, "CNCLDDCombo")==0)
	{
		prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[itemno]))->GetPropertyPage());	
	}
	else if (strcmp(class_str, "CNCLDDEdit")==0)
	{
		prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[itemno]))->GetPropertyPage());	
	}
	else if (strcmp(class_str, "CNCLDDListBox")==0)
	{
		prop_dlg = ((CNCLFormProp *)((CNCLDDListBox*)(m_inpwin[itemno]))->GetPropertyPage());	
	}
	else if (strcmp(class_str, "CNCLDDListCtrl")==0)
	{
		prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl*)(m_inpwin[itemno]))->GetPropertyPage());	
	}
	else if (strcmp(class_str, "CNCLDDListCtrl2")==0)
	{
		prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl2*)(m_inpwin[itemno]))->GetPropertyPage());	
	}
	CPoint pt2 = pt;
	ScreenToClient(&pt2);
	m_picture[pic_indx]->ScreenToClient(&pt);
	if ((m_current_pic!=NULL)&&(m_current_pic!=m_picture[pic_indx]))
	{
		((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
		m_current_pic = NULL;
	}
	m_current_pic = m_picture[pic_indx];
	float pic_rect[4];
	float rect_float[4], rect2_float[4];

	CRect inp_rect;
	m_inpwin[itemno]->GetWindowRect(&inp_rect);
	rect.left = pt.x - inp_rect.Width()*.5+0.499;
	rect.right = pt.x + inp_rect.Width()*.5+0.499;
	rect.top = pt.y - inp_rect.Height()*.5+0.499;
	rect.bottom = pt.y + inp_rect.Height()*.5+0.499;
	CRect rect2 = rect;
	rect2_float[0] = rect.left;
	rect2_float[1] = rect.top;
	rect2_float[2] = rect.right;
	rect2_float[3] = rect.bottom;
	Convert_pic_rect(rect2_float, pic_rect, 0);
	int pic_indx2 = prop_dlg->SetNewPicRect(((CNCLDDPicWin*)m_picture[pic_indx])->m_name, pic_rect, -1);
/*	int pic_indx2 = prop_dlg->GetPicRect(((CNCLDDPicWin*)m_picture[pic_indx])->m_name, rect_float, -1);
	Convert_pic_rect(rect_float, rect2_float, 1);
	rect.left = pt.x - ((rect2_float[2]-rect2_float[0])*.5+.5);
	rect.right = pt.x + ((rect2_float[2]-rect2_float[0])*.5+.5);
	rect.top = pt.y - ((rect2_float[3]-rect2_float[1])*.5+.5);
	rect.bottom = pt.y + ((rect2_float[3]-rect2_float[1])*.5+.5);
	CRect rect2 = rect;
	rect2_float[0] = rect.left;
	rect2_float[1] = rect.top;
	rect2_float[2] = rect.right;
	rect2_float[3] = rect.bottom;
	Convert_pic_rect(rect2_float, pic_rect, 0);
	prop_dlg->m_picarea[pic_indx2].xmin = pic_rect[0];
	prop_dlg->m_picarea[pic_indx2].xmax = pic_rect[2];
	prop_dlg->m_picarea[pic_indx2].ymin = pic_rect[1];
	prop_dlg->m_picarea[pic_indx2].ymax = pic_rect[3];
*/	
	((CNCLDDPicWin*)m_picture[pic_indx])->SetHotSpotRect(rect2);
	m_picture[pic_indx]->ClientToScreen(&rect);
	ScreenToClient(rect);
	CreateHotSpotRect(rect);
	InvalidateRect(rect);
	UpdateWindow();
	m_parent->ScreenToClient(rect);
	m_parent->InvalidateRect(rect);
	m_parent->UpdateWindow();
	OpenPropertyPage(prop_dlg, 0); 
	return 0;
}

void CNCLDDform::CreateHotSpotRect(CRect rect)
{
	m_hotspot_rect = rect;
}

void CNCLDDform::ReseteHotSpotRect()
{
	m_hotspot_rect.left = -1000;
}

/***********************************************************************
**
**   FUNCTION: DeleteItemOnPt(CPoint pt, CWnd *wnd, int flag)
**			Delete the item on the cursor exclude 'wnd'
**		
**	 INPUT:  flag: 0: don't delete selected window
**				pt: cursor point
**				wnd: ecluded window
**   OUTPUT :   delnum: deleted item num
**					deltyp: delete item type
**   RETURN:    number of item deleted
**
**********************************************************************/
int CNCLDDform::DeleteItemOnPt(CPoint pt, CWnd *wnd, int flag, int *delnum, int *deltyp, CNCLWinInfo *del_info1, CNCLWinInfo *del_info2)
{
/*
......when move multi-select item, ignore this function. Since when multi-replacement 
......happened in dragging, cause problem.
*/
	if (m_multi_drag)
		return 0;
	CRect iRect;
	int num = 0;
	CNCLFormProp *prop_dlg;
	for (int i=0; i<m_dispno; i++)
	{
		if (m_disp_win[i] != NULL)
		{
			m_disp_win[i]->GetWindowRect(&iRect);
			if (iRect.PtInRect(pt))
			{
/*
......delete this item if not the same
*/
				if ((flag!=0)&&(wnd!=m_disp_win[i]))
				{
					if (del_info1!=NULL)
					{
						prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_disp_win[i]))->GetPropertyPage());
						prop_dlg->GetWinInfo(del_info1);
					}	
					if ((del_info1->m_page==m_current_sec)||(del_info2->m_page==m_current_sec))
					{
						DeleteItem(i, 0);
						if (delnum!=NULL)
							*delnum = i;
						if (deltyp!=NULL)
							*deltyp = 0;
						return 1;
					}
					return 0;
				}
/*
.....flag==0, check if the window with all selected window
.....if the window is selected windw, do not delete
*/
				else if ((flag==0)&&(IsWindowSelected(m_disp_win[i])==0))
				{
					if (del_info1!=NULL)
					{
						prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_disp_win[i]))->GetPropertyPage());
						prop_dlg->GetWinInfo(del_info1);
					}	
					if ((del_info1->m_page==m_current_sec)||(del_info2->m_page==m_current_sec))
					{
						DeleteItem(i, 0);
						if (delnum!=NULL)
							*delnum = i;
						if (deltyp!=NULL)
							*deltyp = 0;
						return 1;
					}
					return 0;
				}
				else
					return 0;
			}
		}
	}
/*
......never delete Groupbox and replace
......also never delete picture and replace, we will allow
......move item into it for picture hotspot.
*/
/*****************
	for (int i=0; i<m_picno; i++)
	{
		if (m_picture[i] != NULL)
		{
			m_picture[i]->GetWindowRect(&iRect);
			if (iRect.PtInRect(pt))
			{
				if ((flag!=0)&&(wnd!=m_picture[i]))
				{
					if (del_info1!=NULL)
					{
						prop_dlg = ((CNCLFormProp *)((CNCLDDPicWin*)(m_picture[i]))->GetPropertyPage());
						prop_dlg->GetWinInfo(del_info1);
					}	
					if ((del_info1->m_page==m_current_sec)||(del_info2->m_page==m_current_sec))
					{
						DeleteItem(i, 2);
						if (delnum!=NULL)
							*delnum = i;
						if (deltyp!=NULL)
							*deltyp = 2;
						return 1;
					}
					return 0;
				}
				else if ((flag==0)&&(IsWindowSelected(m_picture[i])==0))
				{
					if (del_info1!=NULL)
					{
						prop_dlg = ((CNCLFormProp *)((CNCLDDPicWin*)(m_picture[i]))->GetPropertyPage());
						prop_dlg->GetWinInfo(del_info1);
					}	
					if ((del_info1->m_page==m_current_sec)||(del_info2->m_page==m_current_sec))
					{
						DeleteItem(i, 2);
						if (delnum!=NULL)
							*delnum = i;
						if (deltyp!=NULL)
							*deltyp = 2;
						return 1;
					}
					return 0;
				}
				else
					return 0;
			}
		}
	}
******************/
	for (int i=0; i<m_inputno; i++)
	{
		if (m_inpwin[i] != NULL)
		{
			m_inpwin[i]->GetWindowRect(&iRect);
			if (iRect.PtInRect(pt))
			{
/*
......delete this item if not the same
*/
				if ((flag!=0)&&(wnd!=m_inpwin[i]))
				{
					num = GetItemWinInfo(i, del_info1, del_info2);
					if ((del_info1->m_page==m_current_sec)||(del_info2->m_page==m_current_sec))
					{
						DeleteItem(i, 4);
						if (delnum!=NULL)
							*delnum = i;
						if (deltyp!=NULL)
							*deltyp = 4;
						return num;
					}
					return 0;
				}
				else if ((flag==0)&&(IsWindowSelected(m_inpwin[i])==0))
				{
/*
......only delete it if it is on the same page
*/
					num = GetItemWinInfo(i, del_info1, del_info2);
					if ((del_info1->m_page==m_current_sec)||(del_info2->m_page==m_current_sec))
					{
						DeleteItem(i, 4);
						if (delnum!=NULL)
							*delnum = i;
						if (deltyp!=NULL)
							*deltyp = 4;
						return num;
					}
					return 0;
				}
				else
					return 0;
			}
		}
		if (m_dinpwin[i] != NULL)
		{
			m_dinpwin[i]->GetWindowRect(&iRect);
			if (iRect.PtInRect(pt))
			{
/*
......delete this item if not the same
*/
				if ((flag!=0)&&(wnd!=m_dinpwin[i]))
				{
					num = GetItemWinInfo(i, del_info1, del_info2);
					if ((del_info1->m_page==m_current_sec)||(del_info2->m_page==m_current_sec))
					{
						DeleteItem(i, 3);
						if (delnum!=NULL)
							*delnum = i;
						if (deltyp!=NULL)
							*deltyp = 4;
						return num;
					}
					return 0;
				}
				else if ((flag==0)&&(IsWindowSelected(m_dinpwin[i])==0))
				{
					num = GetItemWinInfo(i, del_info1, del_info2);
					if ((del_info1->m_page==m_current_sec)||(del_info2->m_page==m_current_sec))
					{
						DeleteItem(i, 3);
						if (delnum!=NULL)
							*delnum = i;
						if (deltyp!=NULL)
							*deltyp = 4;
						return num;
					}
					return 0;
				}
				return 0;
			}
		}
	}
	return 0;
}
/*
.....type = 0: display lable
.....type = 1: frame
.....type = 2: picture
.....type = 3: prompt
.....type = 4: input field
*/
/***********************************************************************
**
**   FUNCTION: OnDeleteItems()
**			Callbacks for delete items.
**		
**	 INPUT:  none
**					
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::OnDeleteItems()
{
	int number, item_no, type;
	int selected, orig_num, orig_type;
	int frameno=0, picno=0, dispno=0, inputno=0, delnum=0;
	int selityp = -1;
	int selitem = m_selitem;
	int seldtyp = m_seldtyp;
	if ((seldtyp==4)&&(selitem>=0))
		selityp = m_type[selitem];
	m_action = 3;
	if (m_selnum==0)
	{
		SaveUndoItem();
		item_no = m_action_item;
		type = m_action_typ;
		if (type==0)
		{
			dispno++;
		}
		else if (type==1)
		{
			frameno++;
		}
		else if (type==2)
		{
			picno++;
		}
		else if (type==4)
		{
			inputno++;
		}
		number = DeleteItem(item_no, type);
		m_undo_num = number;
		SaveUndoNumber(m_undo_num, frameno, picno, dispno, inputno, m_current_sec, -1, -1, -1);
		goto done;
	}
	orig_num = m_action_item;
	orig_type = m_action_typ;
	selected = 0;
	m_undo_num = 0;
	int sel = m_selnum-1;

/*
.....first, save all undo list before delete anything
*/
	for (int i=0; i<m_selnum; i++)
	{
		item_no = m_selarray[i];
		type = m_seltype[i];
		m_action_item = item_no;
		m_action_typ = type;
		number = SaveUndoItem();
		m_undo_num += number;
		if (type==0)
			dispno++;
		else if (type==1)
			frameno++;
		else if (type==2)
			picno++;
		else if ((number>0)&&((type==3)||(type==4)))
			inputno++;
		if ((orig_num==m_action_item)&&
			((orig_type==m_action_typ) || (orig_type==3)&&(m_action_typ==4)
				||  (orig_type==4)&&(m_action_typ==3)))
			selected = 1;
	}
	if (selected==0)
	{
		m_action_item = orig_num;
		m_action_typ = orig_type;
		number = SaveUndoItem();
		if (m_action_typ==0)
		{
			dispno++;
		}
		else if (m_action_typ==1)
		{
			frameno++;
		}
		else if (m_action_typ==2)
		{
			picno++;
		}
		else if (m_action_typ==4)
		{
			inputno++;
		}
		m_undo_num += number;
	}
	SaveUndoNumber(m_undo_num, frameno, picno, dispno, inputno, m_current_sec, 
			selitem, seldtyp, selityp);
/*
.....then delete all select items
*/
	for (int i=0; i<m_selnum; i++)
	{
		delnum++;
		item_no = m_selarray[i];
		type = m_seltype[i];
		if (type==0)
		{
			if (m_disp_win[item_no]!=NULL)
			{
				delete m_disp_win[item_no];
				m_disp_win[item_no] = NULL;
			}
		}
		if (type==1)
		{
			if (m_frame[item_no]!=NULL)
			{
				delete m_frame[item_no];
				m_frame[item_no] = NULL;
			}
		}
		if (type==2)
		{
			if (m_picture[item_no]!=NULL)
			{
				delete m_picture[item_no];
				m_picture[item_no] = NULL;
			}
		}
		if ((type==3)||(type==4))
		{
			if (m_dinpwin[item_no]!=NULL)
			{
				delete m_dinpwin[item_no];
				m_dinpwin[item_no] = NULL;
			}
			if (m_inpwin[item_no]!=NULL)
			{
				delete m_inpwin[item_no];
				m_inpwin[item_no] = NULL;
			}
		}
	}
	Re_arrange_item();
	m_selnum = 0;
	if ((selected==0)&&(delnum==0))
	{
		number = DeleteItem(orig_num, orig_type);
		delnum = number;
	}
	m_selitem = -1;
	m_selwin = NULL;
	m_sizedir = 0;
	m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
	m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
	
	for (int i=0; i<8; i++)
	{
		m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
	}
	if (m_current_pic!=NULL)
	{
		((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
		m_current_pic = NULL;
	}
done:;
	updateWindow_draw(1);
	if (m_selnum<=1)
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(0);
	}
	else
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(1);
	}
	CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
	pview->SetFormInputNo(m_inputno);
	pview->SetFormSecNo(m_secno);
	if (m_selnum<=0)
	{
		pview->SetDtype(-2);
		pview->filldata();
	}
	if (delnum>0)
		Remove_redoList();
}
void CNCLDDform::Remove_redoList()
{
	if (m_reset_redo==0)
		return;
	if (m_pRedoList.IsEmpty()==0)
	{
		while( m_pRedoList.GetCount())
		{
			CNCLWinInfo* pItem = m_pRedoList.GetHead();
			delete pItem;
			m_pRedoList.RemoveHead();
		}
	}
	((CNCLFdsnFrame*)UW_Dform_frame)->EnableRedoBtn(0);
}

void CNCLDDform::Reset_Undo_Redo()
{
	if (m_pUndoList.IsEmpty()==0)
	{
		while( m_pUndoList.GetCount())
		{
			CNCLWinInfo* pItem = m_pUndoList.GetHead();
			delete pItem;
			m_pUndoList.RemoveHead();
		}
	}
	((CNCLFdsnFrame*)UW_Dform_frame)->EnableUndoBtn(0);
	if (m_pRedoList.IsEmpty()==0)
	{
		while( m_pRedoList.GetCount())
		{
			CNCLWinInfo* pItem = m_pRedoList.GetHead();
			delete pItem;
			m_pRedoList.RemoveHead();
		}
	}
	((CNCLFdsnFrame*)UW_Dform_frame)->EnableRedoBtn(0);
}

void CNCLDDform::Re_arrange_item()
{
/*
.....re-arrange window item
*/
	int i,j;
	for (i=0, j=0; i<m_dispno; i++, j++)
	{
		if (m_disp_win[i] != NULL)
		{
			m_disp_win[j] = m_disp_win[i];
			m_disp_secpage[j] = m_disp_secpage[i];
			if (m_disp_win[j]!=NULL)
				((CNCLDDStatic*)m_disp_win[j])->SetItemNo(j);
			if (i!=j)
			{
				m_disp_win[i] = NULL;
			}
			continue;
		}
		while ((i<m_dispno)&&(m_disp_win[i]==NULL))
		{
			i++;
		}
		if (i<m_dispno)
		{
			m_disp_win[j] = m_disp_win[i];
			m_disp_secpage[j] = m_disp_secpage[i];
			if (m_disp_win[j]!=NULL)
				((CNCLDDStatic*)m_disp_win[j])->SetItemNo(j);
			if (i!=j)
			{
				m_disp_win[i] = NULL;
			}
		}
		else
			break;
	}	
	m_dispno = j;
	for (i=0, j=0; i<m_frameno; i++, j++)
	{
		if (m_frame[i] != NULL)
		{
			m_frame[j] = m_frame[i];
			m_frm_secpage[j] = m_frm_secpage[i];
			if (m_frame[j]!=NULL)
				((CNCLDDGroup*)m_frame[j])->SetItemNo(j);
			if (i!=j)
			{
				m_frame[i] = NULL;
			}
			continue;
		}
		while ((i<m_frameno)&&(m_frame[i]==NULL))
		{
			i++;
		}
		if (i<m_frameno)
		{
			m_frame[j] = m_frame[i];
			m_frm_secpage[j] = m_frm_secpage[i];
			if (m_frame[j]!=NULL)
				((CNCLDDGroup*)m_frame[j])->SetItemNo(j);
			if (i!=j)
			{
				m_frame[i] = NULL;
			}
		}
		else
			break;
	}
	m_frameno = j;
	for (i=0, j=0; i<m_picno; i++, j++)
	{
		if (m_picture[i] != NULL)
		{
			m_picture[j] = m_picture[i];
			m_pic_secpage[j] = m_pic_secpage[i];
			if (m_picture[j]!=NULL)		
				((CNCLDDPicWin*)m_picture[j])->SetItemNo(j);
			if (i!=j)
			{
				m_picture[i] = NULL;
			}
			continue;
		}
		while ((i<m_picno)&&(m_picture[i]==NULL))
		{
			i++;
		}
		if (i<m_picno)
		{
			m_picture[j] = m_picture[i];
			m_pic_secpage[j] = m_pic_secpage[i];
			if (m_picture[j]!=NULL)		
				((CNCLDDPicWin*)m_picture[j])->SetItemNo(j);
			if (i!=j)
			{
				m_picture[i] = NULL;
			}
		}
		else
			break;
	}
	m_picno = j;
	for (i=0, j=0; i<m_inputno; i++, j++)
	{
		if (m_inpwin[i] != NULL)
		{
			m_inpwin[j] = m_inpwin[i];
			m_dinpwin[j] = m_dinpwin[i];
			m_type[j] = m_type[i];
			m_inp_secpage[j] = m_inp_secpage[i];
			if (m_dinpwin[j]!=NULL)
			{
				if (m_dinpwin[j]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
					((CNCLDDStatic*)m_dinpwin[j])->SetItemNo(j);
				if (m_dinpwin[j]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
					((CNCLDDButton*)m_dinpwin[j])->SetItemNo(j);
			}
			if (m_inpwin[j]==NULL)		
			{
				m_inpwin[i] = NULL;
				m_dinpwin[i] = NULL;			
				continue;
			}
			if ((m_type[j]==1)||(m_type[j]==25))
			{
				((CNCLDDButton*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==2)
			{
				((CNCLDDCombo*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==3)
			{
				((CNCLDDEdit*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==5)
			{
				((CNCLDDListBox*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==7)
			{
				((CNCLDDButton*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==8)
			{
				((CNCLDDCombo*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==9)
			{
				((CNCLDDCombo*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==11)
			{
				((CNCLDDEdit*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==13)
			{
				((CNCLDDProcess*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==15)
			{
				((CNCLDDCombo*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==16)
			{
				((CNCLDDEdit*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==17)
			{
				((CNCLDDListCtrl*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==18)
			{
				((CNCLDDColorButton*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==19)
			{
				((CNCLDDListCtrl2*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==23)
			{
				((CNCLDDColorButton*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==24)
			{
				((CNCLDDSlider*)m_inpwin[j])->SetItemNo(j);
			}
			if (i!=j)
			{
				m_inpwin[i] = NULL;
				m_dinpwin[i] = NULL;	
			}
			continue;
		}
		while ((i<m_inputno)&&(m_inpwin[i]==NULL))
		{
			i++;
		}
		if (i<m_inputno)
		{
			m_inpwin[j] = m_inpwin[i];
			m_dinpwin[j] = m_dinpwin[i];
			m_type[j] = m_type[i];
			m_inp_secpage[j] = m_inp_secpage[i];
			if (m_dinpwin[j]!=NULL)
			{
				if (m_dinpwin[j]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
					((CNCLDDStatic*)m_dinpwin[j])->SetItemNo(j);
				if (m_dinpwin[j]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
					((CNCLDDButton*)m_dinpwin[j])->SetItemNo(j);
			}
			if (m_inpwin[j]==NULL)	
			{
				m_inpwin[i] = NULL;
				m_dinpwin[i] = NULL;
				continue;
			}
			if ((m_type[j]==1)||(m_type[j]==25))
			{
				((CNCLDDButton*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==2)
			{
				((CNCLDDCombo*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==3)
			{
				((CNCLDDEdit*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==5)
			{
				((CNCLDDListBox*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==7)
			{
				((CNCLDDButton*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==8)
			{
				((CNCLDDCombo*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==9)
			{
				((CNCLDDCombo*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==11)
			{
				((CNCLDDEdit*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==13)
			{
				((CNCLDDProcess*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==15)
			{
				((CNCLDDCombo*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==16)
			{
				((CNCLDDEdit*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==17)
			{
				((CNCLDDListCtrl*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==18)
			{
				((CNCLDDColorButton*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==19)
			{
				((CNCLDDListCtrl2*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==23)
			{
				((CNCLDDColorButton*)m_inpwin[j])->SetItemNo(j);
			}
			else if (m_type[j]==24)
			{
				((CNCLDDSlider*)m_inpwin[j])->SetItemNo(j);
			}
			if (i!=j)
			{
				m_inpwin[i] = NULL;
				m_dinpwin[i] = NULL;
			}
		}
		else
			break;
	}
	m_inputno = j;
}

/***********************************************************************
**
**   FUNCTION: DeleteItem(int item_no, int type, int sel)
**			Delete an item
**		
**	 INPUT:  item_no: item to be deleted
**				type: delete item type
**				sel:
**					
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CNCLDDform::DeleteItem(int item_no, int type, int sel)
{
	CRect rect;
	int number = 0;
	if (type==0)
	{
		if (m_disp_win[item_no]!=NULL)
		{
			m_disp_win[item_no]->GetWindowRect(&rect);
			delete m_disp_win[item_no];
			m_disp_win[item_no] = NULL;
/*
......re-arrange the current m_disp_win list
*/
			Delete_dispwin(item_no, sel);
			updateWindow_draw(2, &rect);
			number = 1;
		}
	}
	if (type==1)
	{
		if (m_frame[item_no]!=NULL)
		{
			m_frame[item_no]->GetWindowRect(&rect);
			delete m_frame[item_no];
			m_frame[item_no] = NULL;
/*
......re-arrange the current m_frame list
*/
			Delete_frmwin(item_no, sel);
			updateWindow_draw(2, &rect);
			number = 1;
		}
	}
	if (type==2)
	{
		if (m_picture[item_no]!=NULL)
		{
			m_picture[item_no]->GetWindowRect(&rect);
			delete m_picture[item_no];
			m_picture[item_no] = NULL;
/*
......re-arrange the current m_picture list
*/
			Delete_picwin(item_no, sel);
			updateWindow_draw(2, &rect);
			number = 1;
		}
	}
	if ((type==3)||(type==4))
	{
		if (m_dinpwin[item_no]!=NULL)
		{
			m_dinpwin[item_no]->GetWindowRect(&rect);
			delete m_dinpwin[item_no];
			m_dinpwin[item_no] = NULL;
			updateWindow_draw(2, &rect);
			number++;
		}
		if (m_inpwin[item_no]!=NULL)
		{
			m_inpwin[item_no]->GetWindowRect(&rect);
			delete m_inpwin[item_no];
			m_inpwin[item_no] = NULL;
/*
......re-arrange the current m_dinpwin & m_inpwin list
*/
			Delete_inpwin(item_no, sel);
			updateWindow_draw(2, &rect);
			number++;
		}
	}
	if (m_selitem==item_no)
	{
		m_selitem = -1;
		m_selwin = NULL;
		m_sizedir = 0;
		m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
		m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
	
		for (int i=0; i<8; i++)
		{
			m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
		}
		if (m_current_pic!=NULL)
		{
			((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
			m_current_pic = NULL;
		}
	}
/*
......if there is a delete action, empty the redo list and
......disable Redo button since the redo action could be used 
......a delete item and cause errors only if there delete is from
*/
	if (number>0)
		Remove_redoList();
	return number;
}
/***********************************************************************
**
**   FUNCTION: OnCreate(LPCREATESTRUCT lpCreateStruct) 
**
**		Override this member function to perform any needed 
**		initialization of the class.
**   
**		INPUT:  LPCREATESTRUCT lpCreateStruct: contains copies of 
**						the parameters used to create the window.
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CNCLDDform::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	m_Font.CreatePointFont(80, UW_formdlg_font);
	SetFont(&m_Font);
	return 0;
}

/***********************************************************************
**
**   FUNCTION: OnDestroy() 
**
**		OnDestroy is called after the this object 
**		is removed from the screen. Free the memory
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::OnDestroy() 
{
	if(m_TargetDrop)
	{
		m_TargetDrop->Revoke();
		delete m_TargetDrop;
	}
	m_TargetDrop = NULL;
	CWnd::OnDestroy();	
}
/***********************************************************************
**
**   FUNCTION: Reset_selrec(CRect rect)
**			reset selection dot position 
**		
**	 INPUT:  rect: new selection rect
**					
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::Reset_selrec(CRect rect) 
{
	m_selrec = rect;
	m_selsize[0].left = rect.left - BARLEN;
	m_selsize[0].right = rect.left;
	m_selsize[0].top = rect.top - BARLEN;
	m_selsize[0].bottom = rect.top;
	m_selsize[1].left = rect.left - BARLEN;
	m_selsize[1].right = rect.left;
	m_selsize[1].top = rect.top+rect.Height()/2 - BARLEN;
	m_selsize[1].bottom = m_selsize[1].top + BARLEN;
	m_selsize[2].left = rect.left - BARLEN;
	m_selsize[2].right = rect.left;
	m_selsize[2].top = rect.bottom;
	m_selsize[2].bottom = m_selsize[2].top + BARLEN;
	m_selsize[3].left = rect.left + rect.Width()/2- BARLEN;
	m_selsize[3].right = m_selsize[3].left + BARLEN;
	m_selsize[3].top = rect.bottom;
	m_selsize[3].bottom = m_selsize[3].top + BARLEN;
	m_selsize[4].left = rect.right;
	m_selsize[4].right = rect.right + BARLEN;
	m_selsize[4].top = rect.bottom;
	m_selsize[4].bottom = m_selsize[4].top + BARLEN;
	m_selsize[5].left = rect.right;
	m_selsize[5].right = rect.right + BARLEN;
	m_selsize[5].top = rect.top + rect.Height()/2 - BARLEN;
	m_selsize[5].bottom = m_selsize[5].top + BARLEN;
	m_selsize[6].left = rect.right;
	m_selsize[6].right = rect.right + BARLEN;
	m_selsize[6].top = rect.top - BARLEN;
	m_selsize[6].bottom = m_selsize[6].top + BARLEN;
	m_selsize[7].left = rect.left  + rect.Width()/2- BARLEN;
	m_selsize[7].right = m_selsize[7].left + BARLEN;
	m_selsize[7].top = rect.top - BARLEN;
	m_selsize[7].bottom = rect.top;
	DrawSelect(1);
}
/***********************************************************************
c
c   FUNCTION: InitDrag()
c		Init this control as a drag and drop control. The h_Wnd MUST be valid now
c
c   INPUT:  none
c
c   OUTPUT :  none 
c   RETURN:    None
c
**********************************************************************/
void CNCLDDform::InitDrag()
{
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
}
/***********************************************************************
**
**   FUNCTION: ChangePromptType(int type)
**			change the selection window's input type, only for EDIT/DISPLAY/EDITBOX
**		
**	 INPUT:  type: 0: change from button to label prompt
**					1: change from label prompt to button
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::ChangePromptType(int type)
{
	if (m_selitem==-1)
		return;
	int item_no = m_selitem;
	if ((m_seldtyp==3)||(m_seldtyp==4))
	{
		if (m_dinpwin[item_no]!=NULL)
		{
			if (!((m_type[item_no]==3)||(m_type[item_no]==11)||(m_type[item_no]==16)))
				return;
			CRect rect;
			CNCLFormProp *prop_dlg;
			CNCLWinInfo *pItem;
			m_dinpwin[item_no]->GetWindowRect(&rect);
			ScreenToClient(&rect);
			if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
			{
				if (type==0)
					return;
				prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item_no]))->GetPropertyPage());
			}
			else if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
			{
				if ((type==1)||(type==25))
					return;
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());	
			}
			pItem = new CNCLWinInfo();
			prop_dlg->GetWinInfo(pItem);
			delete m_dinpwin[item_no];
			if (type==0)
			{
				m_dinpwin[item_no] = new CNCLDDStatic();
				((CNCLDDStatic*)m_dinpwin[item_no])->SetParent(this);
				((CNCLDDStatic*)m_dinpwin[item_no])->m_prompt = 1;
				((CNCLDDStatic*)m_dinpwin[item_no])->m_itemno = pItem->m_input_itemno;
				((CNCLDDStatic*)m_dinpwin[item_no])->Create(NULL, NULL, WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_DISP + pItem->m_input_itemno));
				((CNCLDDStatic*)m_dinpwin[item_no])->ShowWindow(SW_SHOW);
				((CNCLDDStatic*)m_dinpwin[item_no])->SetLabelText(pItem->m_label);
				((CNCLDDStatic*)m_dinpwin[item_no])->InitDrag();
			}
			else
			{
				m_dinpwin[item_no] = new CNCLDDButton(3);
				((CNCLDDButton*)m_dinpwin[item_no])->SetParent(this);
				((CNCLDDButton*)m_dinpwin[item_no])->set_prompt(1);
				((CNCLDDButton*)m_dinpwin[item_no])->m_itemno = pItem->m_input_itemno;
				((CNCLDDButton*)m_dinpwin[item_no])->Create(pItem->m_label,
						WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this,  (UINT)(IDC_FORMDESGN_STATIC + pItem->m_input_itemno));
				((CNCLDDButton*)m_dinpwin[item_no])->ShowWindow(SW_SHOW);
				((CNCLDDButton*)m_dinpwin[item_no])->InitDrag();
			}
			CNCLFormProp *page;
			if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
				page = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item_no]))->GetPropertyPage());
			else if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
				page = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());			
			page->SetWinInfo(pItem);
			delete pItem;
		}
	}
}
/***********************************************************************
**
**   FUNCTION: OnDragDropHotSpotCallback(CPoint pt, char *input_text)
**
**         Called by the the mouse drop point is on the window
**
**   INPUT:  pt: current cursor point (drop point)
**			input_text: the text string data contains draging window information
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::OnDragDropHotSpotCallback(CPoint pt, char *input_text)
{
	char *tok, datastr[100],class_str[40];
	CRect rect;
	int selityp = -1, delete_itempt = 1;
	CNCLFormProp *prop_dlg = ((CNCLFormView*)m_parent)->GetPropertyPage();

	int undo_num, total_undono = 0;
	int action;

	strcpy(datastr, input_text);

	CPoint pt_from;
	int itemno;
	tok = strtok(datastr, " \t\r\n");
	tok = strtok(NULL, " \t\r\n");
	strcpy(class_str, tok);	
	tok = strtok(NULL, " \t\r\n");
	if (tok!=NULL)
	{
		itemno = atoi(tok);
		tok = strtok(NULL, " \t\r\n");
		pt_from.x = atoi(tok);
		tok = strtok(NULL, " \t\r\n");
		pt_from.y = atoi(tok);
/*
.....first check if the drop point is inside current picture item
*/
		((CNCLDDPicWin*)m_picture[itemno])->GetWindowRect(&rect);
		if (rect.PtInRect(pt)==0)
			return;
/*
.....so move window from pt_from to pt
*/
		int delta_x = pt.x - pt_from.x;
		int delta_y = pt.y - pt_from.y;
				
		CRect rect = m_hotspot_rect;
		rect.left = m_hotspot_rect.left + delta_x;
		rect.right = m_hotspot_rect.right + delta_x;
		rect.top = m_hotspot_rect.top + delta_y;
		rect.bottom = m_hotspot_rect.bottom + delta_y;
		
		ClientToScreen(&rect);
		CRect rect2 = rect;
		((CNCLDDPicWin*)m_picture[itemno])->ScreenToClient(&rect2);
		((CNCLDDPicWin*)m_picture[itemno])->SetHotSpotRect(rect2);

		UpdatePropertyHSPTSize(rect2, itemno);
		ScreenToClient(rect);
		InvalidateRect(rect);
		UpdateWindow();
		m_parent->ScreenToClient(rect);
		m_parent->InvalidateRect(rect);
		m_parent->UpdateWindow();
	}
}
/***********************************************************************
**
**   FUNCTION: OnDragDropCallback(CPoint pt, char *input_text, int auto_flag)
**
**         Called by the the mouse drop point is on the window
**
**   INPUT:  pt: current cursor point (drop point)
**			input_text: the text string data contains draging window information
**			flag = 1: macro form auto item
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::OnDragDropCallback(CPoint pt, char *input_text, int auto_flag)
{
	char *tok, datastr[100],class_str[40], temp_name[80];
	CRect rect;
	int selityp = -1, delete_itempt = 1;
	CNCLFormProp *prop_dlg = ((CNCLFormView*)m_parent)->GetPropertyPage();

	int undo_num, total_undono = 0;
	int action;

	strcpy(datastr, input_text);

	POINT point;
	GetCursorPos(&point);
	ScreenToClient(&point);

	if (m_selnum>1)
		m_multi_drag = 1;
	
	CClientDC dc(this);
	CSize sizeText = dc.GetTextExtent("Prompt",6);

	rect.left = point.x;
	rect.top = point.y;

	tok = strtok(datastr, " \t\r\n");
	if (strcmp(tok, "New")==0)
	{
		m_reset_redo = 1;
		Remove_redoList();
		m_action = 1;
		action = 1;
/*
.....before we create/delete any item here, delselect items first
*/	
		m_sizedir = 0;
		m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
		m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
		for (int i=0; i<8; i++)
		{
			m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
		}
		m_selitem = -1;
		m_selwin = NULL;
		if (m_current_pic!=NULL)
		{
			((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
			m_current_pic = NULL;
		}
		m_select_draw = 0;
		updateWindow_draw(1);
		m_select_draw = 1;

		for (int i=0; i<m_selnum;i++)
		{
			m_selarray[i] = -1;
			m_seltype[i] = -1;
		}
		m_selnum = 0;
		m_update_area = 0;
		CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
		pview->SetDtype(-2);
		pview->filldata();
		updateWindow_draw(1);
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(0);
/*
.....about statement could disrupt the strtok function, so redo it
*/
		strcpy(datastr, input_text);
		tok = strtok(datastr, " \t\r\n");
		if (strcmp(tok, "New")!=0)
/*
.....something wrong
*/
		{
			MessageBox("Internal error in function CNCLDDform::OnDragDropCallback.", "Error", MB_OK);
		}
		tok = strtok(NULL, " \t\r\n");		
		if (strcmp(tok, "LABEL")==0)
		{
/*
......create display label
*/
			rect.right = rect.left + sizeText.cx + 1;
			rect.bottom = rect.top + sizeText.cy + 1;
			m_disp_secpage[m_dispno] = m_current_sec;
			m_disp_win[m_dispno] = new CNCLDDStatic();
			((CNCLDDStatic*)m_disp_win[m_dispno])->SetParent(this);
			((CNCLDDStatic*)m_disp_win[m_dispno])->m_prompt = 0;
			((CNCLDDStatic*)m_disp_win[m_dispno])->m_itemno = m_dispno;
			((CNCLDDStatic*)m_disp_win[m_dispno])->Create(NULL, NULL, WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_DISP + m_dispno));
			((CNCLDDStatic*)m_disp_win[m_dispno])->ShowWindow(SW_SHOW);
			((CNCLDDStatic*)m_disp_win[m_dispno])->SetLabelText("label");
			((CNCLDDStatic*)m_disp_win[m_dispno])->InitDrag();
			m_disp_win[m_dispno]->GetWindowRect(&rect);
			updateWindow_draw(2, &rect);
			SaveUndoItems(1, 0, -1, m_dispno);
			m_dispno++;
			goto done;
		}
		if (strcmp(tok, "FRAME")==0)
		{
/*
......create Framebox
*/
			rect.right = rect.left + 50;
			rect.bottom = rect.top + 50;
			m_frm_secpage[m_frameno] = m_current_sec;
			m_frame[m_frameno] = new CNCLDDGroup();
			((CNCLDDGroup*)m_frame[m_frameno])->SetParent(this);
			((CNCLDDGroup*)m_frame[m_frameno])->m_itemno = m_frameno;
			((CNCLDDGroup*)m_frame[m_frameno])->Create("group", WS_VISIBLE|WS_CHILD|WS_GROUP|BS_GROUPBOX, rect, this,  (UINT)(IDC_FORMDESGN_FRAME + m_frameno));
			((CNCLDDGroup*)m_frame[m_frameno])->ShowWindow(SW_SHOW);
			((CNCLDDGroup*)m_frame[m_frameno])->InitDrag();
			m_frame[m_frameno]->GetWindowRect(&rect);
			updateWindow_draw(2, &rect);
			m_frameno++;
			SaveUndoItems(1, 1, -1, m_frameno-1);
			goto done;
		}
		if (strcmp(tok, "PICTUREBOX")==0)
		{
/*
......create picture box
*/
			rect.right = rect.left + 48;
			rect.bottom = rect.top + 48;
			m_pic_secpage[m_picno] = m_current_sec;
			sprintf(temp_name, "TEMP_PIC%d", m_picno);
			m_picture[m_picno] = new CNCLDDPicWin(temp_name, "test.dib", this);
			((CNCLDDPicWin*)m_picture[m_picno])->SetParent(this);
			((CNCLDDPicWin*)m_picture[m_picno])->m_itemno = m_picno;
			((CNCLDDPicWin*)m_picture[m_picno])->Create(WS_CHILD|WS_VISIBLE|WS_TABSTOP|WS_BORDER, rect, this, (UINT)(IDC_FORMDESGN_PIC + m_picno));
			((CNCLDDPicWin*)m_picture[m_picno])->ShowWindow(SW_SHOW);
			((CNCLDDPicWin*)m_picture[m_picno])->InitDrag();
			m_picture[m_picno]->GetWindowRect(&rect);
			updateWindow_draw(2, &rect);
			m_picno++;
			SaveUndoItems(1, 2, -1, m_picno-1);
			goto done;
		}
/*
......check if the new move point is on the existing item, if it is, replace it
......by delete this existing item first.
*/
		int new_indx = -1, deltyp = -1, delnum = -1;
		CString del_label;
		CNCLWinInfo *del_win_info1 = new CNCLWinInfo();
		CNCLWinInfo *del_win_info2 = new CNCLWinInfo();
		int del = DeleteItemOnPt(pt, NULL, 1, &delnum, &deltyp, del_win_info1, del_win_info2);
		if (del>0)
		{
/*
......save the delete into undo list and this action will be 30
*/
			SaveUndoInfo(2, del_win_info1, del_win_info2);
			action = 30;
		}
		if ((del!=0)&&(deltyp==4))
		{
			new_indx = delnum;
			if (del==1)
			{
				if (del_win_info1!=NULL)
				{
					delete del_win_info1;
					del_win_info1 = NULL;
				}
			}
		}
		else
		{
			if (del_win_info1!=NULL)
				delete del_win_info1;
			if (del_win_info2!=NULL)
				delete del_win_info2;
			del_win_info1 = NULL;
			del_win_info2 = NULL;
		}
		int macnum;
		if (m_macro_flag) 
		{
			ncl_getmac_pnum(&macnum);
			if (m_inputno>=macnum)
			{
/*
......Can't create more input items than the MACRO paramters
*/
				MessageBox("There are more fields defined than Macro parameters", "Error", MB_OK);
				goto done;
			}
		}
		if (strcmp(tok, "PUSHBUTTON")==0)
		{
			m_type[m_inputno] = 1;
		}
		else if (strcmp(tok, "CHOICEBOX")==0)
		{
			m_type[m_inputno] = 2;
		}
		else if (strcmp(tok, "EDIT")==0)
		{
			m_type[m_inputno] = 3;
		}
		else if (strcmp(tok, "SELECT")==0)
		{
			m_type[m_inputno] = 23;
		}
		else if (strcmp(tok, "LISTBOX")==0)
		{
			m_type[m_inputno] = 5;
		}
		else if (strcmp(tok, "CHECKBOX")==0)
		{
			m_type[m_inputno] = 7;
		}
		else if (strcmp(tok, "COMBLIST_SIMPLE")==0)
		{
			m_type[m_inputno] = 8;
		}
		else if (strcmp(tok, "COMBLIST_DROPDOWN")==0)
		{
			m_type[m_inputno] = 9;
		}
		else if (strcmp(tok, "DISPLAY")==0)
		{
			m_type[m_inputno] = 11;
		}
		else if (strcmp(tok, "PROGRESS")==0)
		{
			m_type[m_inputno] = 13;
		}
		else if (strcmp(tok, "CHOICE_LIST")==0)
		{
			m_type[m_inputno] = 15;
		}
		else if (strcmp(tok, "EDITBOX")==0)
		{
			m_type[m_inputno] = 16;
		}
		else if (strcmp(tok, "LISTTABLE")==0)
		{
			m_type[m_inputno] = 17;
		}
		else if (strcmp(tok, "COLOR")==0)
		{
			m_type[m_inputno] = 18;
		}
		else if (strcmp(tok, "DATATABLE")==0)
		{
			m_type[m_inputno] = 19;
		}
		else if (strcmp(tok, "SLIDER")==0)
		{
			m_type[m_inputno] = 24;
		}
		else if (strcmp(tok, "VIDEO")==0)
		{
			m_type[m_inputno] = 25;
		}
		m_inp_secpage[m_inputno] = m_current_sec;
		if ((m_type[m_inputno]==2)||(m_type[m_inputno]==8)
			||(m_type[m_inputno]==9)||(m_type[m_inputno]==10) ||(m_type[m_inputno]==13)
			||(m_type[m_inputno]==14) || (m_type[m_inputno] == 15)||(m_type[m_inputno]==16)
			||(m_type[m_inputno]==3)||(m_type[m_inputno]==11)
			||(m_type[m_inputno]==18)||(m_type[m_inputno]==24))
		{
/*
......create prompt label
*/
			if (new_indx!=-1)
			{
				if (del_win_info1!=NULL)
				{
					rect.left = del_win_info1->m_pos[0];
					rect.top = del_win_info1->m_pos[1];
					rect.right = rect.left + del_win_info1->m_size[0];
					rect.bottom = rect.top + del_win_info1->m_size[1];
				}
				else if (del_win_info2!=NULL)
				{
					rect.left = del_win_info2->m_pos[0];
					rect.top = del_win_info2->m_pos[1];
					rect.right = rect.left + sizeText.cx + 2;
					rect.bottom = rect.top + sizeText.cy + 2;
				}
			}
			else
			{
				rect.right = rect.left + sizeText.cx + 2;
				rect.bottom = rect.top + sizeText.cy + 2;
			}
			m_dinpwin[m_inputno] = new CNCLDDStatic();
			((CNCLDDStatic*)m_dinpwin[m_inputno])->SetParent(this);
			((CNCLDDStatic*)m_dinpwin[m_inputno])->m_prompt = 1;
			((CNCLDDStatic*)m_dinpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDStatic*)m_dinpwin[m_inputno])->Create(NULL, NULL, WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_STATIC + m_inputno));
			((CNCLDDStatic*)m_dinpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDStatic*)m_dinpwin[m_inputno])->SetLabelText("Prompt");
			if (new_indx!=-1)
			{
				if (del_win_info1!=NULL)
					((CNCLDDStatic*)m_dinpwin[m_inputno])->SetLabelText(del_win_info1->m_label);
				else
					((CNCLDDStatic*)m_dinpwin[m_inputno])->SetLabelText(del_win_info2->m_label);
			}
			((CNCLDDStatic*)m_dinpwin[m_inputno])->InitDrag();
			CRect drect;
			m_dinpwin[m_inputno]->GetWindowRect(&drect);
			updateWindow_draw(2, &drect);
		}
		if (m_type[m_inputno]==23)
		{
/*
......create prompt label
*/
			if (new_indx!=-1)
			{
				if (del_win_info1!=NULL)
				{
					rect.left = del_win_info1->m_pos[0];
					rect.top = del_win_info1->m_pos[1];
					rect.right = rect.left + del_win_info1->m_size[0];
					rect.bottom = rect.top + del_win_info1->m_size[1];
				}
				else if (del_win_info2!=NULL)
				{
					rect.left = del_win_info2->m_pos[0];
					rect.top = del_win_info2->m_pos[1];
					rect.right = rect.left + sizeText.cx + 2;
					rect.bottom = rect.top + sizeText.cy + 2;
				}
			}
			else
			{
				rect.right = rect.left + sizeText.cx + 2;
				rect.bottom = rect.top + sizeText.cy + 2;
			}
			m_dinpwin[m_inputno] = new CNCLDDButton(m_type[m_inputno]);
			((CNCLDDButton*)m_dinpwin[m_inputno])->SetParent(this);
			((CNCLDDButton*)m_dinpwin[m_inputno])->set_prompt(1);
			((CNCLDDButton*)m_dinpwin[m_inputno])->m_itemno = m_inputno;
//			((CNCLDDButton*)m_dinpwin[m_inputno])->Create("Prompt",
//					WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_OWNERDRAW |BS_NOTIFY, 
//					rect, this,  (UINT)(IDC_FORMDESGN_STATIC + m_inputno));
			((CNCLDDButton*)m_dinpwin[m_inputno])->Create("Prompt",
					WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP |BS_NOTIFY, 
					rect, this,  (UINT)(IDC_FORMDESGN_STATIC + m_inputno));
			((CNCLDDButton*)m_dinpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDButton*)m_dinpwin[m_inputno])->InitDrag();
			((CNCLDDButton*)m_dinpwin[m_inputno])->set_prop_values(1.0, -1, 1, "Default, Default", "Default, Default");
			CRect drect;
			m_dinpwin[m_inputno]->GetWindowRect(&drect);
			updateWindow_draw(2, &drect);

			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
			}
			m_inpwin[m_inputno] = new CNCLDDColorButton(m_type[m_inputno]);
			((CNCLDDColorButton*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDColorButton*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDColorButton*)m_inpwin[m_inputno])->Create("", WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_OWNERDRAW |BS_NOTIFY,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDColorButton*)m_inpwin[m_inputno])->set_color(RGB(255,128,0), RGB(255,128,0));
			((CNCLDDColorButton*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDColorButton*)m_inpwin[m_inputno])->InitDrag();
			((CNCLDDColorButton*)m_inpwin[m_inputno])->set_prop_values(1.0, 1, "Default,Default", "Default,Default", "", FORM_PICK);
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		if ((m_type[m_inputno]==1)||(m_type[m_inputno]==25))
		{
/*
.......PUSHBUTTON
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = rect.left + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
				if (del_win_info1!=NULL)
				{
					rect.left = del_win_info1->m_pos[0];
					rect.top = del_win_info1->m_pos[1];
				}
			}
			else
			{
				rect.right = rect.left + sizeText.cx + 4;
				rect.bottom = rect.top + sizeText.cy + 2;
			}
			m_inpwin[m_inputno] = new CNCLDDButton(m_type[m_inputno]);
			((CNCLDDButton*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDButton*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			CString label = "button";
			if (m_type[m_inputno]==25)
				label = "<Video>";
			if (new_indx!=-1)
			{
				if (del_win_info1!=NULL)
					label = del_win_info1->m_label;
				else if (del_win_info2!=NULL)
					label = del_win_info2->m_label;
			}
			if (m_type[m_inputno]==25)
			{
				((CNCLDDButton*)m_inpwin[m_inputno])->SetBitMapFile("Video.bmp");
				((CNCLDDButton*)m_inpwin[m_inputno])->Create(" ", WS_VISIBLE | WS_CHILD | BS_BITMAP, rect, this, 
						(UINT)(IDC_FORMDESGN_INPUT + m_mwin_no + 200));
			}
			else
			{
				((CNCLDDButton*)m_inpwin[m_inputno])->Create(label, WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_NOTIFY,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_mwin_no + 200));
			}
			((CNCLDDButton*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDButton*)m_inpwin[m_inputno])->InitDrag();
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==2)
		{
/*
.......CHOICEBOX
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
				rect.bottom = rect.top + 80;
			}
			m_inpwin[m_inputno] = new CNCLDDCombo(m_type[m_inputno]);
			((CNCLDDCombo*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDCombo*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDCombo*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|CBS_DROPDOWNLIST|WS_VSCROLL|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDCombo*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDCombo*)m_inpwin[m_inputno])->InitDrag();			
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==3)
		{
/*
.......EDIT
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
				rect.top -= 4;
			}
			m_inpwin[m_inputno] = new CNCLDDEdit(m_type[m_inputno]);
			((CNCLDDEdit*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDEdit*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDEdit*)m_inpwin[m_inputno])->CreateEx(WS_EX_CLIENTEDGE, "EDIT", "EDIT", 
						WS_CHILD | WS_VISIBLE | ES_AUTOHSCROLL | ES_LEFT | WS_BORDER | WS_TABSTOP | DS_SETFONT, 
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno), 0);
			((CNCLDDEdit*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDEdit*)m_inpwin[m_inputno])->InitDrag();						
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==5)
		{
/*
.......LISTBOX, no prompt, so no change for 'rect.left' and 'rect.top'
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = rect.left + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
				if (del_win_info1!=NULL)
				{
					rect.left = del_win_info1->m_pos[0];
					rect.top = del_win_info1->m_pos[1];
				}
			}
			else
			{
				rect.right = rect.left + 50;
				rect.bottom = rect.top + 50;
			}
			m_inpwin[m_inputno] = new CNCLDDListBox(m_type[m_inputno], 1);
			((CNCLDDListBox*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDListBox*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDListBox*)m_inpwin[m_inputno])->CreateEx(WS_EX_CLIENTEDGE, "LISTBOX", "listbox", 
						WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_BORDER | LBS_NOTIFY | WS_VSCROLL | LBS_USETABSTOPS | WS_HSCROLL,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDListBox*)m_inpwin[m_inputno])->InitDrag();
			((CNCLDDListBox*)m_inpwin[m_inputno])->AddString("test");
			((CNCLDDListBox*)m_inpwin[m_inputno])->AddString("test2");
			((CNCLDDListBox*)m_inpwin[m_inputno])->SetCurSel(0);
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==7)
		{
/*
.......CHECKBOX
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = rect.left + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
				if (del_win_info1!=NULL)
				{
					rect.left = del_win_info1->m_pos[0];
					rect.top = del_win_info1->m_pos[1];
				}
			}
			else
			{
				rect.right = rect.left + sizeText.cx + 50;
				rect.bottom = rect.top + sizeText.cy + 2;
			}
			m_inpwin[m_inputno] = new CNCLDDButton(m_type[m_inputno]);
			((CNCLDDButton*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDButton*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			CString label = "checkbox";
			if (new_indx!=-1)
			{
				if (del_win_info1!=NULL)
					label = del_win_info1->m_label;
				else if (del_win_info2!=NULL)
					label = del_win_info2->m_label;
			}
			((CNCLDDButton*)m_inpwin[m_inputno])->Create(label, WS_CHILD|WS_VISIBLE|BS_LEFT|BS_AUTOCHECKBOX|WS_TABSTOP|BS_NOTIFY, rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDButton*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDButton*)m_inpwin[m_inputno])->InitDrag();
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==8)
		{
/*
.......COMBLIST_SIMPLE
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
				rect.bottom = rect.top + 80;
			}
			m_inpwin[m_inputno] = new CNCLDDCombo(m_type[m_inputno]);
			((CNCLDDCombo*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDCombo*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDCombo*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|CBS_SIMPLE|WS_VSCROLL|CBS_AUTOHSCROLL|WS_TABSTOP|WS_HSCROLL, rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDCombo*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDCombo*)m_inpwin[m_inputno])->InitDrag();			
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==9)
		{
/*
.......COMBLIST_DROPDOWN
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
				rect.bottom = rect.top + 80;
			}
			m_inpwin[m_inputno] = new CNCLDDCombo(m_type[m_inputno]);
			((CNCLDDCombo*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDCombo*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDCombo*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|CBS_DROPDOWN|WS_VSCROLL|CBS_AUTOHSCROLL|WS_TABSTOP|WS_HSCROLL|CBS_NOINTEGRALHEIGHT, rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDCombo*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDCombo*)m_inpwin[m_inputno])->InitDrag();			
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==11)
		{
/*
.......DISPLAY
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
			}
			m_inpwin[m_inputno] = new CNCLDDEdit(m_type[m_inputno]);
			((CNCLDDEdit*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDEdit*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDEdit*)m_inpwin[m_inputno])->CreateEx(WS_EX_CLIENTEDGE, "EDIT", "DISPLAY", 
						WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | ES_AUTOVSCROLL | ES_LEFT | WS_BORDER | ES_MULTILINE |ES_WANTRETURN | ES_READONLY | DS_SETFONT, rect, 
						this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDEdit*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDEdit*)m_inpwin[m_inputno])->InitDrag();						
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==13)
		{
/*
.......PROGRESS
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
			}
			m_inpwin[m_inputno] = new CNCLDDProcess(m_type[m_inputno]);
			((CNCLDDProcess*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDProcess*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDProcess*)m_inpwin[m_inputno])->Create( WS_CHILD | WS_VISIBLE | WS_BORDER | PBS_SMOOTH, rect, 
							this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDProcess*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDProcess*)m_inpwin[m_inputno])->InitDrag();						
			((CNCLDDProcess*)m_inpwin[m_inputno])->SetRange(1, 100);
			((CNCLDDProcess*)m_inpwin[m_inputno])->SetPos(50);
			((CNCLDDProcess*)m_inpwin[m_inputno])->SetWindowText("50");
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==15)
		{
/*
.......CHOICE_LIST
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
				rect.bottom = rect.top + 50;
			}
			m_inpwin[m_inputno] = new CNCLDDCombo(m_type[m_inputno]);
			((CNCLDDCombo*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDCombo*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDCombo*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|CBS_DROPDOWNLIST|WS_VSCROLL|WS_TABSTOP|CBS_AUTOHSCROLL|WS_HSCROLL, 
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDCombo*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDCombo*)m_inpwin[m_inputno])->InitDrag();			
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==16)
		{
/*
.......EDITBOX
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
				rect.bottom = rect.top + 50;
			}
			m_inpwin[m_inputno] = new CNCLDDEdit(m_type[m_inputno]);
			((CNCLDDEdit*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDEdit*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDEdit*)m_inpwin[m_inputno])->CreateEx(WS_EX_CLIENTEDGE, "EDIT", "EDITBOX", 
				WS_CHILD|WS_VISIBLE|WS_TABSTOP|ES_AUTOHSCROLL|ES_AUTOVSCROLL|ES_LEFT|WS_BORDER|ES_MULTILINE|ES_WANTRETURN|DS_SETFONT, 
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno), 0);
			((CNCLDDEdit*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDEdit*)m_inpwin[m_inputno])->InitDrag();						
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==17)
		{
/*
.......LISTTABLE, no prompt, so no change for 'rect.left' and 'rect.top'
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = rect.left + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
				if (del_win_info1!=NULL)
				{
					rect.left = del_win_info1->m_pos[0];
					rect.top = del_win_info1->m_pos[1];
				}
			}
			else
			{
				rect.right = rect.left + 120;
				rect.bottom = rect.top + 50;
			}
			m_inpwin[m_inputno] = new CNCLDDListCtrl();
			((CNCLDDListCtrl*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDListCtrl*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDListCtrl*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL | WS_HSCROLL | WS_VSCROLL, 
					rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDListCtrl*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDListCtrl*)m_inpwin[m_inputno])->InitDrag();

			CNCLDDListCtrl *listctl = (CNCLDDListCtrl*)m_inpwin[m_inputno];
			listctl->InsertColumn(0, "col1",  LVCFMT_LEFT, 50);
			listctl->InsertColumn(1, "col2",  LVCFMT_LEFT, 50);

			listctl->InsertItem(0,  "data1");
			listctl->SetItemText(0, 1, "data2");
			listctl->SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES);
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==18)
		{
/*
.......COLOR
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
			}
			m_inpwin[m_inputno] = new CNCLDDColorButton(m_type[m_inputno]);
			((CNCLDDColorButton*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDColorButton*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDColorButton*)m_inpwin[m_inputno])->Create("", WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_OWNERDRAW |BS_NOTIFY,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDColorButton*)m_inpwin[m_inputno])->set_color(RGB(255,128,0), RGB(255,128,0));
			((CNCLDDColorButton*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDColorButton*)m_inpwin[m_inputno])->InitDrag();
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==24)
		{
/*
.......SLIDER
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				if (del_win_info1==NULL)
				{
					rect.left = rect.right + 5;
				}
				else
					rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = del_win_info2->m_pos[0] + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
			}
			else
			{
				rect.left = rect.right + 5;
				rect.right = rect.right + 50;
			}
			m_inpwin[m_inputno] = new CNCLDDSlider(m_type[m_inputno]);
			((CNCLDDSlider*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDSlider*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDSlider*)m_inpwin[m_inputno])->Create(WS_VISIBLE | WS_CHILD | TBS_HORZ,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDSlider*)m_inpwin[m_inputno])->set_color(RGB(255,0,0), RGB(255,128,0));
//			((CNCLDDSlider*)m_inpwin[m_inputno])->SetPos(20);
			((CNCLDDSlider*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDSlider*)m_inpwin[m_inputno])->InitDrag();
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		else if (m_type[m_inputno]==19)
		{
/*
.......DATATABLE, no prompt, so no change for 'rect.left' and 'rect.top'
*/
			if ((new_indx!=-1)&&(del_win_info2!=NULL))
			{
				rect.left = del_win_info2->m_pos[0];
				rect.top = del_win_info2->m_pos[1];
				rect.right = rect.left + del_win_info2->m_size[0];
				rect.bottom = rect.top + del_win_info2->m_size[1];
				if (del_win_info1!=NULL)
				{
					rect.left = del_win_info1->m_pos[0];
					rect.top = del_win_info1->m_pos[1];
				}
			}
			else
			{
				rect.right = rect.left + 120;
				rect.bottom = rect.top + 80;
			}
			m_inpwin[m_inputno] = new CNCLDDListCtrl2();
			((CNCLDDListCtrl2*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDListCtrl2*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDListCtrl2*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL | WS_HSCROLL | WS_VSCROLL, 
				rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDListCtrl2*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDListCtrl2*)m_inpwin[m_inputno])->InitDrag();
			((CNCLDDListCtrl2*)m_inpwin[m_inputno])->SetType(1);

			CNCLDDListCtrl2 *listctl = (CNCLDDListCtrl2*)m_inpwin[m_inputno];
			LV_COLUMN lvc;
			lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;

			lvc.iSubItem = 0;
			lvc.pszText = "col1";
			lvc.cx = 40;
			lvc.fmt = LVCFMT_LEFT;
			listctl->InsertColumn(0,&lvc);
	
			lvc.iSubItem = 1;
			lvc.pszText = "col2";
			lvc.cx = 40;
			lvc.fmt = LVCFMT_LEFT;
			listctl->InsertColumn(1,&lvc);

			listctl->InsertItem(0,  "data1");
			listctl->SetItemText(0, 1, "data2");
			listctl->m_nNumberOfRows = 1;
			listctl->m_nNumberOfCols = 2;
			listctl->ModifyStyle(0,LVS_SHOWSELALWAYS);
			m_inpwin[m_inputno]->GetWindowRect(&rect);
		}
		if (new_indx!=-1)
		{
			if (m_inputno!=new_indx)
				ReplaceItem(m_inputno, new_indx, 4);
			if (del_win_info1!=NULL)
				delete del_win_info1;
			if (del_win_info2!=NULL)
				delete del_win_info2;
		}
		if (new_indx!=-1)
			SaveUndoItems(1, 4, m_type[new_indx], new_indx, del);
		else
			SaveUndoItems(1, 4, m_type[m_inputno], m_inputno, del);
		m_inputno++;
		updateWindow_draw(2, &rect);
	}
	else if (strcmp(tok, "Move")==0)
	{
		int extra=0, del = 0, hotspot = 1;
		int deltyp = -1, delnum = -1;
		CNCLWinInfo *del_win_info1;
		CNCLWinInfo *del_win_info2;

		S_skip_reset();
		m_reset_redo = 1;
		Remove_redoList();
		m_action = 2;
		tok = strtok(NULL, " \t\r\n");
		strcpy(class_str, tok);
		int display = 0;
		if (strcmp(tok, "CNCLDDStatic")==0)
		{
			display = 1;
			hotspot = 0;
		}
		int pic = 0;
		if (strcmp(tok, "CNCLDDPicWin")==0)
		{
			pic = 1;
			hotspot = 0;
		}
		int frame = 0;
		if (strcmp(tok, "CNCLDDGroup")==0)
		{
			frame = 1;
			hotspot = 0;
		}
		if (strcmp(tok, "CNCLDDButton2")==0)
		{
			auto_flag = 1;
			hotspot = 0;
		}
		if (strcmp(tok, "CNCLDDProcess")==0)
		{
			hotspot = 0;
		}
		char class_str[80];
		strcpy(class_str, tok);
		int combo = 0;
		CPoint pt_from;
		int itemno;
		tok = strtok(NULL, " \t\r\n");
		if (tok!=NULL)
		{
			itemno = atoi(tok);
			tok = strtok(NULL, " \t\r\n");
			pt_from.x = atoi(tok);
			tok = strtok(NULL, " \t\r\n");
			pt_from.y = atoi(tok);
/*
.....so move window from pt_from to pt
*/
			int delta_x = pt.x - pt_from.x;
			int delta_y = pt.y - pt_from.y;
			int move_item = m_selnum;
			int indx = 0;
/*
......it could be a button prompt
*/
			if (strcmp(class_str, "CNCLDDButton")==0)
			{
				if ((m_dinpwin[itemno]!=NULL)&&(m_dinpwin[itemno]->IsKindOf(RUNTIME_CLASS(CNCLDDButton))))
				{
					if (((CNCLDDButton*)(m_dinpwin[itemno]))->get_prompt())
						display = 1;
				}
			}
moveit:;
			if (move_item<=0)
				goto done;
/*
.....check if it is picture hotspot drop
*/
			if (hotspot)
			{
/*
.....only handle one item, the first selected one
*/
				int status = HandleHotSpot(pt, class_str, itemno); 
				if (status==0)
					goto done;
			}
			if (UW_form_hotspot_item==1)
			{
			}
/*
.....before move, remove all selection draw,
.....will draw later
*/
			m_select_draw = 0;
			updateWindow_draw(1);
			m_select_draw = 1;
			del = 0;
			del_win_info1 = new CNCLWinInfo();
			del_win_info2 = new CNCLWinInfo();
			if ((pic==1)&&((CNCLDDPicWin*)m_picture[itemno]!=NULL))
			{
/*
......check if the new move point is on the existing item, if it is, replace it
......by delete this existing item first.
*/
/*
......we need check if the move item is the same item as exist item, if it is
......do not delete it, just move the current one (existing one)
*/
				if (m_selitem==itemno)
				{
					del = DeleteItemOnPt(pt, m_picture[itemno], 0, &delnum, &deltyp, del_win_info1, del_win_info2);
				}
				else
					del = 0;
				if (del>0)
				{
/*
......save the delete into undo list and this action will be 31
*/
					SaveUndoInfo(2, del_win_info1, del_win_info2);
					action = 31;
					extra+=del;
					if (itemno>delnum)
						itemno--;
				}
				((CNCLDDPicWin*)m_picture[itemno])->GetWindowRect(&rect);
				m_action_item = itemno;
				m_action_typ = 2;
				m_action = 2;
/*
.....only save once
*/
				if (S_skip_next(itemno)==0)
				{
					undo_num = SaveUndoItem();
					total_undono += undo_num;
					if (undo_num>1)
						S_skip_next_save(itemno);
				}
				rect.top += delta_y;
				rect.left += delta_x;
				rect.bottom += delta_y;
				rect.right += delta_x;
				ScreenToClient(rect);
				((CNCLDDPicWin*)m_picture[itemno])->MoveWindow(&rect,1);
		
				((CNCLDDPicWin*)m_picture[itemno])->RedrawWindow();
				((CNCLDDPicWin*)m_picture[itemno])->UpdateWindow();
				if (m_selitem==itemno)
					Reset_selrec(rect);
				goto again;
			}
			if ((frame==1)&&((CNCLDDGroup*)m_frame[itemno]!=NULL))
			{
				if (m_selitem==itemno)
					del = DeleteItemOnPt(pt, m_frame[itemno], 0, &delnum, &deltyp, del_win_info1, del_win_info2);
				else
					del = 0;
				if (del>0)
				{
/*
......save the delete into undo list and this action will be 31
*/
					SaveUndoInfo(2, del_win_info1, del_win_info2);
					action = 31;
					extra+=del;
					if (itemno>delnum)
						itemno--;
				}
				((CNCLDDGroup*)m_frame[itemno])->GetWindowRect(&rect);
				m_action_item = itemno;
				m_action_typ = 1;
				m_action = 2;
				undo_num = SaveUndoItem();
				total_undono += undo_num;
				rect.top += delta_y;
				rect.left += delta_x;
				rect.bottom += delta_y;
				rect.right += delta_x;
				ScreenToClient(rect);
				((CNCLDDGroup*)m_frame[itemno])->MoveWindow(&rect,1);
				((CNCLDDGroup*)m_frame[itemno])->RedrawWindow();
				((CNCLDDGroup*)m_frame[itemno])->UpdateWindow();
				if (m_selitem==itemno)
					Reset_selrec(rect);
				goto again;
			}
			if ((auto_flag)&&(m_macro_win[itemno]!=NULL))
			{
				m_macro_win[itemno]->GetWindowRect(&rect);
				m_action_item = itemno;
				m_action_typ = 5;
				m_action = 2;
				undo_num = SaveUndoItem();
				total_undono += undo_num;
				CRect oldrect = rect;
				rect.top += delta_y;
				rect.left += delta_x;
				rect.bottom += delta_y;
				rect.right += delta_x;
				ScreenToClient(rect);
				m_macro_win[itemno]->MoveWindow(&rect,1);
				m_macro_win[itemno]->RedrawWindow();
				m_macro_win[itemno]->UpdateWindow();
				if (m_selitem==itemno)
					Reset_selrec(rect);
				goto again;
			}
			if (display==0)
			{
				if ((m_selitem==itemno)&&(delete_itempt))
					del = DeleteItemOnPt(pt, m_inpwin[itemno], 0, &delnum, &deltyp, del_win_info1, del_win_info2);
				else
					del = 0;
				if (del>0)
				{
/*
......save the delete into undo list and this action will be 31
*/
					SaveUndoInfo(2, del_win_info1, del_win_info2);
					action = 31;
					extra+=del;
					if (itemno>delnum)
						itemno--;
				}
				m_inpwin[itemno]->GetWindowRect(&rect);
				m_action_item = itemno;
				m_action_typ = 4;
				m_action = 2;
/*
.....only save once
*/
				if (S_skip_next(itemno)==0)
				{
					undo_num = SaveUndoItem();
					total_undono += undo_num;
					if (undo_num>1)
						S_skip_next_save(itemno);
				}
				rect.top += delta_y;
				rect.left += delta_x;
				rect.bottom += delta_y;
				rect.right += delta_x;

				ScreenToClient(rect);
				m_inpwin[itemno]->MoveWindow(&rect,1);
				m_inpwin[itemno]->RedrawWindow();
				m_inpwin[itemno]->UpdateWindow();
				if (m_inpwin[itemno]->IsKindOf(RUNTIME_CLASS(CNCLDDCombo)))
				{
					combo = 1;
				}
				if ((m_selitem==itemno)&&(m_inpwin[itemno]==m_selwin))
				{
					Reset_selrec(rect);
				}
			}
			else
			{
				int prompt = 0;
				if (m_dinpwin[itemno]!=NULL)
				{
					if (m_dinpwin[itemno]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
					{
						if (((CNCLDDStatic*)m_dinpwin[itemno])->m_prompt==1)
							prompt = 1;
					}
					if (m_dinpwin[itemno]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
					{
						prompt = 1;
					}
				}
				if (prompt==1)
				{
					if ((m_selitem==itemno)&&(delete_itempt))
						del = DeleteItemOnPt(pt, m_dinpwin[itemno], 0, &delnum, &deltyp, del_win_info1, del_win_info2);
					else
						del = 0;
					if (del>0)
					{
/*
......save the delete into undo list and this action will be 31
*/
						SaveUndoInfo(2, del_win_info1, del_win_info2);
						action = 31;
						extra+=del;
						if (itemno>delnum)
							itemno--;
					}
					m_dinpwin[itemno]->GetWindowRect(&rect);
					m_action_item = itemno;
					m_action_typ = 3;
					m_action = 2;
					undo_num = SaveUndoItem();
					total_undono += undo_num;
					rect.top += delta_y;
					rect.left += delta_x;
					rect.bottom += delta_y;
					rect.right += delta_x;

					ScreenToClient(rect);
					m_dinpwin[itemno]->MoveWindow(&rect,1);
					m_dinpwin[itemno]->RedrawWindow();
					m_dinpwin[itemno]->UpdateWindow();
					if ((m_selitem==itemno)&&(m_dinpwin[itemno]==m_selwin))
					{
						Reset_selrec(rect);
					}
				}
				else
				{
					if ((m_selitem==itemno)&&(delete_itempt))
						del = DeleteItemOnPt(pt, m_disp_win[itemno], 0, &delnum, &deltyp, del_win_info1, del_win_info2);
					else
						del = 0;
					if (del>0)
					{
/*
......save the delete into undo list and this action will be 31
*/
						SaveUndoInfo(2, del_win_info1, del_win_info2);
						extra += del;
						action = 31;
					}
					m_disp_win[itemno]->GetWindowRect(&rect);
					m_action_item = itemno;
					m_action_typ = 0;
					m_action = 2;
					undo_num = SaveUndoItem();
					total_undono += undo_num;
					rect.top += delta_y;
					rect.left += delta_x;
					rect.bottom += delta_y;
					rect.right += delta_x;

					ScreenToClient(rect);
					m_disp_win[itemno]->MoveWindow(&rect,1);
					m_disp_win[itemno]->RedrawWindow();
					m_disp_win[itemno]->UpdateWindow();
					if ((m_selitem==itemno)&&(m_disp_win[itemno]==m_selwin))
					{
						Reset_selrec(rect);
					}
				}
			}
/*
.....we need check if we need move the other selected item
*/
again:;
			if ((m_selitem==itemno)&&(m_inpwin[itemno]==m_selwin))
			{
/*
......set size
*/	
				if (combo==1)
				{
					CRect listrect;
					((CNCLDDCombo*)(m_inpwin[itemno]))->GetDroppedControlRect(&listrect);
					prop_dlg->m_size[0] = listrect.right - listrect.left;
					prop_dlg->m_size[1] = listrect.bottom - listrect.top;
				}
				else
				{
					prop_dlg->m_size[0] = rect.right - rect.left;
					prop_dlg->m_size[1] = rect.bottom - rect.top;
				}
/*
......set pos
*/	
				prop_dlg->m_pos[0] = rect.left;
				prop_dlg->m_pos[1] = rect.top;
				((CNCLFormView*)m_parent)->UpdatePropertySize(prop_dlg->m_size[0], prop_dlg->m_size[1]);
			}
			move_item--;
			if (move_item>0)
			{
				if ((m_selarray[indx]==m_selitem)&&(m_seltype[indx]==m_seldtyp))
					indx++;
				if (indx>=m_selnum)
					goto done;
				display = 0;
				if ((m_seltype[indx]==0)||(m_seltype[indx]==3))
					display = 1;
				pic = 0;
				if (m_seltype[indx]==2)
					pic = 1;
				frame = 0;
				if (m_seltype[indx]==1)
					frame = 1;
				delete_itempt = 1;
				if ((m_selarray[indx]==itemno)&&((m_seltype[indx]==3)||(m_seltype[indx]==4)))
					delete_itempt = 0;
				auto_flag = 0;
				if (m_seltype[indx]==5)
					auto_flag = 1;
				itemno = m_selarray[indx];
				indx++;
				goto moveit;
			}
		}
		m_action = 2;
		if ((m_seldtyp==4)&&(m_selitem>=0))
			selityp = m_type[m_selitem];
		if (extra>0)
			m_action = action;
		SaveUndoNumber(total_undono+ extra, m_frameno, m_picno, m_dispno, m_inputno, m_current_sec, 
				m_selitem, m_seldtyp, selityp);
		updateWindow_draw();
	}
done:;
/*
......we add follow statement here because the dragging event will eat the buttonup event
*/
	if (m_selnum<=1)
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(0);
	}
	else
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(1);
	}
	CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
	pview->SetFormInputNo(m_inputno);
	pview->SetFormSecNo(m_secno);
	m_buttondown = 0;
	m_resize = 0;
	((CNCLFormView*)m_parent)->m_formchanged = 1;
	m_multi_drag = 0;
}

void CNCLDDform::UpdateDDFormView()
{
	updateWindow_draw(1);
	((CNCLFormView*)m_parent)->m_formchanged = 1;
}
/***********************************************************************
**
**   FUNCTION: LoadFormItem(UD_FSTRUCT *fstruct)
**			Create all form item according to the form structure
**		
**	 INPUT:  fstruct: form structure
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::LoadFormItem(UD_FSTRUCT *fstruct)
{
	char tmpstr[80], tmpstr2[80], limit_str[500], name[40];
	double range[2];
	int active, type, macro_add=0;
/*
.....first remove all current item
*/
	while( m_pUndoList.GetCount() )
	{
		CNCLWinInfo* pItem = m_pUndoList.GetHead();
		delete pItem;
		m_pUndoList.RemoveHead();
	}
	for (int i=0; i<m_dispno; i++)
	{
		if (m_disp_win[i] != NULL)
			delete m_disp_win[i];
	}	
	for (int i=0; i<m_frameno; i++)
	{
		if (m_frame[i] != NULL)
			delete m_frame[i];
	}
	for (int i=0; i<m_picno; i++)
	{
		if (m_picture[i] != NULL)
			delete m_picture[i];
	}
	for (int i=0; i<m_inputno; i++)
	{
		if (m_inpwin[i] != NULL)
			delete m_inpwin[i];
		if (m_dinpwin[i] != NULL)
			delete m_dinpwin[i];
	}
	for (int i=0; i<6; i++)
	{
		if (m_macro_win[i]!=NULL)
			delete m_macro_win[i];
	}
/*
......reinit everything
*/
	for (int i=0; i<200; i++)
	{
		m_inpwin[i] = NULL;
		m_dinpwin[i] = NULL;
		m_disp_win[i] = NULL;
		m_frame[i] = NULL;
		m_picture[i] = NULL;
		m_selarray[i] = -1;
		m_disp_secpage[i] = 0;
		m_inp_secpage[i] = 0;
		m_pic_secpage[i] = 0;
		m_frm_secpage[i] = 0;
	}
	for (int i=0; i<6; i++)
	{
		m_macro_win[i] = NULL;
	}
	m_current_sec = -1;
	m_mwin_no = 0;
	m_selnum = 0;
	m_dispno = 0;
	m_inputno = 0;
	m_frameno = 0;
	m_picno = 0;
	m_buttondown = 0;
	m_sizedir = 0;
	m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
	m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
	
	for (int i=0; i<8; i++)
	{
		m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
	}
	m_selitem = -1;
	m_selwin = NULL;
	m_resize = 0;
	m_dropdown = 0;
	m_current_pic = NULL;

	int i,j,x,y,cx,cy,pwid2;
	CRect rect;
	char perc_str[10];

	int chrwid, phgt;

	for (i=0; i<fstruct->n_display_fields; i++)
	{
		rect.left = (int)(fstruct->display_flds[i].pos.ud_c*uw_form_scalex);
		rect.top = (int)(fstruct->display_flds[i].pos.ud_r*uw_form_scaley);
		if ((fstruct->display_flds[i].pos.x>0) 
			&& (fstruct->display_flds[i].pos.y>0))
		{
			cx = (int)(fstruct->display_flds[i].pos.x);
			cy = (int)(fstruct->display_flds[i].pos.y);
		}
		else
		{
			uw_ntget_strsize(fstruct->display_flds[i].message, 80, "MS Sans Serif", &cx, &cy);
		}
		rect.right = rect.left + cx;
		rect.bottom = rect.top + cy;
//		((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg))->MapDialogRect(&rect);
//		uw_MapDialogRect(rect);
		uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
		m_disp_secpage[m_dispno] = fstruct->display_flds[i].section;
		m_disp_win[m_dispno] = new CNCLDDStatic();
		((CNCLDDStatic*)m_disp_win[m_dispno])->SetParent(this);
		((CNCLDDStatic*)m_disp_win[m_dispno])->m_itemno = m_dispno;
		((CNCLDDStatic*)m_disp_win[m_dispno])->m_prompt = 0;
		((CNCLDDStatic*)m_disp_win[m_dispno])->Create(NULL, NULL, WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_DISP + m_dispno));
		((CNCLDDStatic*)m_disp_win[m_dispno])->ShowWindow(SW_SHOW);
		((CNCLDDStatic*)m_disp_win[m_dispno])->SetLabelText(fstruct->display_flds[i].message);
		((CNCLDDStatic*)m_disp_win[m_dispno])->InitDrag();
		sprintf(tmpstr, "%s, %s", fstruct->display_flds[i].fcolor, fstruct->display_flds[i].bcolor);
		((CNCLDDStatic*)m_disp_win[m_dispno])->set_prop_values(fstruct->display_flds[i].font_scale, 1, tmpstr);
		m_dispno++;
	}
	for (i=0; i<fstruct->n_frame_fields; i++)
	{
		rect.left = (int)(fstruct->frame_flds[i].x*uw_form_scalex);
		rect.top = (int)(fstruct->frame_flds[i].y*uw_form_scaley);
		cx = (int)(fstruct->frame_flds[i].cx*uw_form_scalex);
		cy = (int)(fstruct->frame_flds[i].cy*uw_form_scaley);
		rect.right = rect.left + cx;
		rect.bottom = rect.top + cy;
//		((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//		uw_MapDialogRect(rect);
		uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
		m_frm_secpage[m_frameno] = fstruct->frame_flds[i].section;
		m_frame[m_frameno] = new CNCLDDGroup();
		((CNCLDDGroup*)m_frame[m_frameno])->SetParent(this);
		((CNCLDDGroup*)m_frame[m_frameno])->m_itemno = m_frameno;
		((CNCLDDGroup*)m_frame[m_frameno])->Create("group", WS_VISIBLE|WS_CHILD|WS_GROUP|BS_GROUPBOX, rect, this,  (UINT)(IDC_FORMDESGN_FRAME + m_frameno));
		((CNCLDDGroup*)m_frame[m_frameno])->ShowWindow(SW_SHOW);
		((CNCLDDGroup*)m_frame[m_frameno])->InitDrag();
		((CNCLDDGroup*)m_frame[m_frameno])->SetLabelText(fstruct->frame_flds[i].title);
		sprintf(tmpstr, "%s, %s", fstruct->frame_flds[i].fcolor, fstruct->frame_flds[i].bcolor);
		((CNCLDDGroup*)m_frame[m_frameno])->set_prop_values(fstruct->frame_flds[i].font_scale, 1, tmpstr);
		m_frameno++;	
	}
	for (i=0; i<fstruct->n_picture_fields; i++)
	{
		rect.left = (int)(fstruct->picture_flds[i].x*uw_form_scalex);
		rect.top = (int)(fstruct->picture_flds[i].y*uw_form_scaley);
		cx = (int)(fstruct->picture_flds[i].cx*uw_form_scalex);
		cy = (int)(fstruct->picture_flds[i].cy*uw_form_scaley);
		rect.right = rect.left + cx;
		rect.bottom = rect.top + cy;
//		((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//		uw_MapDialogRect(rect);
		uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
		m_pic_secpage[m_picno] = fstruct->picture_flds[i].section;
		m_picture[m_picno] = new CNCLDDPicWin(fstruct->picture_flds[i].name, fstruct->picture_flds[i].fname, this);
		((CNCLDDPicWin*)m_picture[m_picno])->SetParent(this);
		((CNCLDDPicWin*)m_picture[m_picno])->m_itemno = m_picno;
		((CNCLDDPicWin*)m_picture[m_picno])->Create(WS_CHILD|WS_VISIBLE|WS_TABSTOP|WS_BORDER, rect, this, (UINT)(IDC_FORMDESGN_PIC + m_picno));
		((CNCLDDPicWin*)m_picture[m_picno])->ShowWindow(SW_SHOW);
		((CNCLDDPicWin*)m_picture[m_picno])->InitDrag();
		m_picno++;
	}
	int pwid, posx, posy, inpwid, butlen, twid;
	int macro_start;
	if ((m_macro_flag)&&(fstruct->macroflag))
		macro_start = fstruct->n_input_fields - 6;
	else
		macro_start = fstruct->n_input_fields - 5;
	for (i=0; i<fstruct->n_input_fields; i++)
	{
		m_inp_secpage[m_inputno] = fstruct->input_flds[i].section;
		active = (int)fstruct->input_flds[i].active_flag;
		if (fstruct->input_flds[i].toggle == 6)
			type = 1;  
		else if (fstruct->input_flds[i].toggle == 2)
			type = 2;
		else if (fstruct->input_flds[i].toggle == 1)
			type = 3;
		else if ((fstruct->input_flds[i].toggle == 5)
			&& (fstruct->input_flds[i].n_defaults == 0))
			type = 5;
		else if (fstruct->input_flds[i].toggle == 17)
			type = 17;
		else if (fstruct->input_flds[i].toggle == 3)
			type = 7;
		else if ((fstruct->input_flds[i].toggle == 5)
			&& (fstruct->input_flds[i].n_defaults == 1))
			type = 8;
		else if ((fstruct->input_flds[i].toggle == 5)
			&& (fstruct->input_flds[i].n_defaults == 2))
			type = 9;
		else if (fstruct->input_flds[i].toggle == 7)
			type = 10;
		else if (fstruct->input_flds[i].toggle == 8)
			type = 11;
		else if (fstruct->input_flds[i].toggle == 9)
			type = 13;
		else if (fstruct->input_flds[i].toggle == 10)
			type = 15;
		else if (fstruct->input_flds[i].toggle == 11)
			type = 16;
		else if (fstruct->input_flds[i].toggle == 18)
			type = 18;
		else if (fstruct->input_flds[i].toggle == 19)
			type = 19;
		else if (fstruct->input_flds[i].toggle == 23)
			type = 23;
		else if (fstruct->input_flds[i].toggle == 24)
			type = 24;
		else if (fstruct->input_flds[i].toggle == 25)
			type = 25;
		else
			MessageBox("Internal Type error!","Internal Error!",MB_OK);
		m_len[i] = fstruct->input_flds[i].ud_flen;
		m_prec[i] = fstruct->input_flds[i].ud_fprec;
		m_range_flag = fstruct->input_flds[i].range_flag;
		if (fstruct->input_flds[i].range_flag==1)
		{
			m_range[i][0] = fstruct->input_flds[i].range[0];
			m_range[i][1] = fstruct->input_flds[i].range[1];
		}
/*
.....input filed
*/
		if ((type==3) || (type==5) || (type==11))
		{
			m_input[i] = fstruct->input_flds[i].ud_input;
		}
		else if (type==23)
			m_input[i] = FORM_PICK;
		x = (int)(fstruct->input_flds[i].ud_prmloc.ud_c*uw_form_scalex);
		y = (int)(fstruct->input_flds[i].ud_prmloc.ud_r*uw_form_scaley);
		cx = (int)(fstruct->input_flds[i].ud_fldloc.ud_c*uw_form_scalex);
		cy = (int)(fstruct->input_flds[i].ud_fldloc.ud_r*uw_form_scaley);

		rect.left = x;
		rect.top = y;
		rect.right = rect.left + 1;
		rect.bottom = rect.top + cy;
		pwid = 0;
		if ((type==2)||(type==8)
			||(type==9)||(type==10) ||(type==13)
			||(type==14) || (type == 15)||(type==16)
			||(type==18)||(type==24))
		{
/*
......create prompt label
*/
			if (fstruct->input_flds[i].prompt[0]!='\0')
				uw_ntget_strsize(fstruct->input_flds[i].prompt, 80, "MS Sans Serif", &pwid, &phgt);
			else
			{
				uw_ntget_strsize("X", 80, "MS Sans Serif", &chrwid, &phgt);
				pwid = 0;
			}
			if ((fstruct->input_flds[i].ud_prmloc.x!=-1)
				&&((fstruct->input_flds[i].ud_prmloc).ud_r==(fstruct->input_flds[i].ud_prmloc).y))
			{
				if (x+pwid>fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex)
					pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x -1);
			}
			rect.left = x;
			rect.top = y;
			rect.right = rect.left + pwid;
			rect.bottom = rect.top + phgt;
//			((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//			uw_MapDialogRect(rect);
			uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));

			m_dinpwin[m_inputno] = new CNCLDDStatic();
			((CNCLDDStatic*)m_dinpwin[m_inputno])->SetParent(this);
			((CNCLDDStatic*)m_dinpwin[m_inputno])->m_prompt = 1;
			((CNCLDDStatic*)m_dinpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDStatic*)m_dinpwin[m_inputno])->Create(NULL, NULL, WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_STATIC + m_inputno));
			((CNCLDDStatic*)m_dinpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDStatic*)m_dinpwin[m_inputno])->SetLabelText(fstruct->input_flds[i].prompt);
			((CNCLDDStatic*)m_dinpwin[m_inputno])->InitDrag();
			sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
			((CNCLDDStatic*)m_dinpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr);
			if (fstruct->input_flds[i].ud_prmloc.x==-1)
			{
				posx = x+pwid;
				inpwid = cx-pwid;
			}
			else
			{
				posx = (int)((fstruct->input_flds[i].ud_prmloc.x)*uw_form_scalex);
				inpwid = cx-(posx-x);
			}
			if (fstruct->input_flds[i].ud_prmloc.y==-1)
				posy = y;
			else
				posy = (int)(fstruct->input_flds[i].ud_prmloc.y*uw_form_scaley);
/*
.....at least display 5 item + 1 select item display
*/
			if ((phgt*6>cy)&&(type==2))
				cy = phgt*6;
			
			rect.left = posx;
			rect.top = posy;
			if (type==2)
			{
/*
.......CHOICEBOX
*/
				rect.right = rect.left + inpwid;
				rect.bottom = rect.top + cy;
//				((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//				uw_MapDialogRect(rect);
				uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
				m_inpwin[m_inputno] = new CNCLDDCombo(type);
				((CNCLDDCombo*)m_inpwin[m_inputno])->SetParent(this);
/*
.....assigned the initial data
*/
				((CNCLDDCombo*)m_inpwin[m_inputno])->m_itemno = m_inputno;
				((CNCLDDCombo*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|CBS_DROPDOWNLIST|WS_VSCROLL|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
				((CNCLDDCombo*)m_inpwin[m_inputno])->adddata(fstruct->input_flds[i].n_defaults, fstruct->input_flds[i].defaults);
				((CNCLDDCombo*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDCombo*)m_inpwin[m_inputno])->InitDrag();	
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDCombo*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr, tmpstr2);
				((CNCLDDCombo*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
			}
			else if (type==8)
			{
/*
.......COMBLIST_SIMPLE
*/
				rect.right = rect.left + inpwid;
				rect.bottom = rect.top + cy;
//				((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//				uw_MapDialogRect(rect);
				uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
				m_inpwin[m_inputno] = new CNCLDDCombo(type);
				((CNCLDDCombo*)m_inpwin[m_inputno])->m_itemno = m_inputno;
				((CNCLDDCombo*)m_inpwin[m_inputno])->SetParent(this);
				((CNCLDDCombo*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|CBS_SIMPLE|WS_VSCROLL|CBS_AUTOHSCROLL|WS_TABSTOP|WS_HSCROLL, rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
				((CNCLDDCombo*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDCombo*)m_inpwin[m_inputno])->InitDrag();			
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDCombo*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr, tmpstr2);
				((CNCLDDCombo*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
			}
			else if (type==9)
			{
/*
.......COMBLIST_DROPDOWN
*/
				rect.right = rect.left + (int)(abs(m_len[i]) * TEXT_WID * uw_form_scalex + 6);
				rect.bottom = rect.top + cy;
//				((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//				uw_MapDialogRect(rect);
				uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
				m_inpwin[m_inputno] = new CNCLDDCombo(type);
				((CNCLDDCombo*)m_inpwin[m_inputno])->SetParent(this);
				((CNCLDDCombo*)m_inpwin[m_inputno])->m_itemno = m_inputno;
				((CNCLDDCombo*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|CBS_DROPDOWN|WS_VSCROLL|CBS_AUTOHSCROLL|WS_TABSTOP|WS_HSCROLL|CBS_NOINTEGRALHEIGHT, rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
				((CNCLDDCombo*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDCombo*)m_inpwin[m_inputno])->InitDrag();			
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDCombo*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr, tmpstr2);
				((CNCLDDCombo*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
			}
			else if (type==13)
			{
/*
.......PROGRESS
*/
				if (fstruct->input_flds[i].ud_datatyp==UD_DASINT)
				{
					sprintf_s(perc_str, sizeof(perc_str), "%d%%", 100);
					uw_ntget_strsize(perc_str, 80, "MS Sans Serif", &pwid2, &phgt);
					rect.right = rect.left + inpwid-pwid2-1;
					rect.bottom = rect.top + cy;
				}
				else
				{
					rect.right = rect.left + inpwid;
					rect.bottom = rect.top + cy;
				}
//				((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//				uw_MapDialogRect(rect);
				uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
				m_inpwin[m_inputno] = new CNCLDDProcess(type);
				((CNCLDDProcess*)m_inpwin[m_inputno])->SetParent(this);
				((CNCLDDProcess*)m_inpwin[m_inputno])->m_itemno = m_inputno;
				((CNCLDDProcess*)m_inpwin[m_inputno])->Create( WS_CHILD | WS_VISIBLE | WS_BORDER | PBS_SMOOTH, rect, 
								this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
				((CNCLDDProcess*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDProcess*)m_inpwin[m_inputno])->InitDrag();						
				((CNCLDDProcess*)m_inpwin[m_inputno])->SetRange(1, 100);
				((CNCLDDProcess*)m_inpwin[m_inputno])->SetPos(50);
				((CNCLDDProcess*)m_inpwin[m_inputno])->SetWindowText("50");
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDProcess*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr, tmpstr2);
			}
			else if (type==15)
			{
/*
.......CHOICE_LIST
*/
				rect.right = rect.left + inpwid;
				rect.bottom = rect.top + cy;
//				((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//				uw_MapDialogRect(rect);
				uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
				((CNCLDDCombo*)m_inpwin[m_inputno])->m_itemno = m_inputno;
				m_inpwin[m_inputno] = new CNCLDDCombo(type);
				((CNCLDDCombo*)m_inpwin[m_inputno])->SetParent(this);
				((CNCLDDCombo*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|CBS_DROPDOWNLIST|WS_VSCROLL|WS_TABSTOP|CBS_AUTOHSCROLL|WS_HSCROLL, 
							rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
				((CNCLDDCombo*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDCombo*)m_inpwin[m_inputno])->InitDrag();			
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDCombo*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr, tmpstr2);
				((CNCLDDCombo*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
			}
			else if (type==16)
			{
/*
.......EDITBOX
*/
				rect.right = rect.left + inpwid;
				rect.bottom = rect.top + cy;
//				((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//				uw_MapDialogRect(rect);
				uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
				m_inpwin[m_inputno] = new CNCLDDEdit(type);
				((CNCLDDEdit*)m_inpwin[m_inputno])->SetParent(this);
				((CNCLDDEdit*)m_inpwin[m_inputno])->m_itemno = m_inputno;
				((CNCLDDEdit*)m_inpwin[m_inputno])->CreateEx(WS_EX_CLIENTEDGE, "EDIT", "EDITBOX", 
					WS_CHILD|WS_VISIBLE|WS_TABSTOP|ES_AUTOHSCROLL|ES_AUTOVSCROLL|ES_LEFT|WS_BORDER|ES_MULTILINE|ES_WANTRETURN|DS_SETFONT, 
							rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno), 0);
				((CNCLDDEdit*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDEdit*)m_inpwin[m_inputno])->InitDrag();						
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				convert_limit_str(fstruct->input_flds[i].ud_limit, limit_str);
				((CNCLDDEdit*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].range_flag,
					fstruct->input_flds[i].range, fstruct->input_flds[i].font_scale, 
						fstruct->input_flds[i].ud_datatyp, m_input[i], m_len[i], m_prec[i], active, limit_str, tmpstr, tmpstr2);
				((CNCLDDEdit*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
			}
			else if (type==18)
			{
/*
.......COLOR
*/
				rect.right = rect.left + inpwid;
				rect.bottom = rect.top + cy;
//				((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//				uw_MapDialogRect(rect);
				uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
				m_inpwin[m_inputno] = new CNCLDDColorButton(type);
				((CNCLDDColorButton*)m_inpwin[m_inputno])->SetParent(this);
/*
.....always empty string for color button
*/
				((CNCLDDColorButton*)m_inpwin[m_inputno])->m_itemno = m_inputno;
				((CNCLDDColorButton*)m_inpwin[m_inputno])->Create("", 
							WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_OWNERDRAW |BS_NOTIFY,
							rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
				((CNCLDDColorButton*)m_inpwin[m_inputno])->set_color(RGB(255,128,0), RGB(255,128,0));
				((CNCLDDColorButton*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDColorButton*)m_inpwin[m_inputno])->InitDrag();
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDColorButton*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr, tmpstr2);
				((CNCLDDColorButton*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
			}
			else if (type==24)
			{
/*
.......SLIDER
*/
				rect.right = rect.left + inpwid;
				rect.bottom = rect.top + cy;
				uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
				m_inpwin[m_inputno] = new CNCLDDSlider(type);
				((CNCLDDSlider*)m_inpwin[m_inputno])->SetParent(this);
				((CNCLDDSlider*)m_inpwin[m_inputno])->m_itemno = m_inputno;
/*
				if (fstruct->input_flds[i].justified==0)
				{
					if (rect.bottom-rect.top>20)
						rect.bottom = rect.top + 20;
				}
				else
				{
					if (rect.right-rect.left>20)
						rect.right = rect.left + 20;
				}
*/
				DWORD dwStyle;
				if (fstruct->input_flds[i].justified==0)
					dwStyle = WS_VISIBLE | WS_CHILD | TBS_HORZ;
				else
					dwStyle = WS_VISIBLE | WS_CHILD | TBS_VERT;

				((CNCLDDSlider*)m_inpwin[m_inputno])->Create(dwStyle,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
				((CNCLDDSlider*)m_inpwin[m_inputno])->set_color(RGB(255,0,0), RGB(255,128,0));
				((CNCLDDSlider*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDSlider*)m_inpwin[m_inputno])->InitDrag();
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDSlider*)m_inpwin[m_inputno])->set_prop_values(active, tmpstr, tmpstr2, 
					fstruct->input_flds[i].range_flag, fstruct->input_flds[i].range, fstruct->input_flds[i].justified);
				((CNCLDDSlider*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
			}
		}
		else if (type==23)
		{
			uw_ntget_strsize(fstruct->input_flds[i].prompt, 80, "MS Sans Serif", &pwid, &phgt);
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
			rect.left = x;
			rect.top = y;
			rect.right = rect.left + butlen;
			rect.bottom = rect.top + cy;
			uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
			m_dinpwin[m_inputno] = new CNCLDDButton(type);
			((CNCLDDButton*)m_dinpwin[m_inputno])->SetParent(this);
			((CNCLDDButton*)m_dinpwin[m_inputno])->set_prompt(1);
			((CNCLDDButton*)m_dinpwin[m_inputno])->m_itemno = m_inputno;
//			((CNCLDDButton*)m_dinpwin[m_inputno])->Create(fstruct->input_flds[i].prompt,
//					WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_OWNERDRAW |BS_NOTIFY, 
//					rect, this,  (UINT)(IDC_FORMDESGN_STATIC + m_inputno));
			((CNCLDDButton*)m_dinpwin[m_inputno])->Create(fstruct->input_flds[i].prompt,
					WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP |BS_NOTIFY, 
					rect, this,  (UINT)(IDC_FORMDESGN_STATIC + m_inputno));
			((CNCLDDButton*)m_dinpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDButton*)m_dinpwin[m_inputno])->InitDrag();
			sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
			sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
			((CNCLDDButton*)m_dinpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, m_input[i], active, tmpstr, tmpstr2);
/*
.....color button as input field
*/
			if (fstruct->input_flds[i].ud_prmloc.x==-1)
			{
				posx = x+pwid;
				inpwid = cx-pwid;
			}
			else
			{
				posx = (int)((fstruct->input_flds[i].ud_prmloc.x)*uw_form_scalex);
				inpwid = cx-(posx-x);
			}
			
			if (fstruct->input_flds[i].ud_prmloc.y==-1)
				posy = y;
			else
				posy = (int)(fstruct->input_flds[i].ud_prmloc.y*uw_form_scaley);

			rect.left = posx;
			rect.top = posy;
			rect.right = rect.left + inpwid;
			rect.bottom = rect.top + cy;

			uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
			m_inpwin[m_inputno] = new CNCLDDColorButton(type);
			((CNCLDDColorButton*)m_inpwin[m_inputno])->SetParent(this);
/*
.....always empty string for color button
*/
			((CNCLDDColorButton*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDColorButton*)m_inpwin[m_inputno])->Create("", 
						WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_OWNERDRAW |BS_NOTIFY,
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
			((CNCLDDColorButton*)m_inpwin[m_inputno])->set_color(RGB(255,128,0), RGB(255,128,0));
			((CNCLDDColorButton*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDColorButton*)m_inpwin[m_inputno])->InitDrag();
			sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
			sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
			convert_limit_str(fstruct->input_flds[i].ud_limit, limit_str);
			((CNCLDDColorButton*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr, tmpstr2, limit_str, FORM_PICK);
			((CNCLDDColorButton*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
		}
		else if ((type==3)||(type==11))
		{
			uw_ntget_strsize(fstruct->input_flds[i].prompt, 80, "MS Sans Serif", &pwid, &phgt);
			if (fstruct->input_flds[i].ud_prmloc.x!=-1)
			{
				pwid = (int)(fstruct->input_flds[i].ud_prmloc.x*uw_form_scalex - x - 1);
			}
			if (m_input[i]==FORM_STRING)
			{
				rect.left = x;
				rect.top = y;
				rect.right = rect.left + pwid;
				rect.bottom = rect.top + phgt;
//				((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//				uw_MapDialogRect(rect);
				uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
				m_dinpwin[m_inputno] = new CNCLDDStatic();
				((CNCLDDStatic*)m_dinpwin[m_inputno])->SetParent(this);
				((CNCLDDStatic*)m_dinpwin[m_inputno])->m_prompt = 1;
				((CNCLDDStatic*)m_dinpwin[m_inputno])->m_itemno = m_inputno;
				((CNCLDDStatic*)m_dinpwin[m_inputno])->Create(NULL, NULL, WS_CHILD|WS_VISIBLE|WS_TABSTOP, rect, this, (UINT)(IDC_FORMDESGN_STATIC + m_inputno));
				((CNCLDDStatic*)m_dinpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDStatic*)m_dinpwin[m_inputno])->SetLabelText(fstruct->input_flds[i].prompt);
				((CNCLDDStatic*)m_dinpwin[m_inputno])->InitDrag();
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDStatic*)m_dinpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr);
			}
			else if (m_input[i]==FORM_PICK || m_input[i]==FORM_LOCATE || 
				m_input[i]==FORM_LABEL || m_input[i]==FORM_SUBSCR)
//				|| m_input[i]==FORM_SELECT)
			{
				if (fstruct->input_flds[i].ud_prmloc.x==-1)
					butlen = (int)(pwid*1.1);
				else
				{
					butlen = (int)(pwid*0.95);
				}
				rect.left = x;
				rect.top = y;
				rect.right = rect.left + butlen;
				rect.bottom = rect.top + cy;
//				((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//				uw_MapDialogRect(rect);
				uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
				m_dinpwin[m_inputno] = new CNCLDDButton(type);
				((CNCLDDButton*)m_dinpwin[m_inputno])->SetParent(this);
				((CNCLDDButton*)m_dinpwin[m_inputno])->set_prompt(1);
				((CNCLDDButton*)m_dinpwin[m_inputno])->m_itemno = m_inputno;
//				((CNCLDDButton*)m_dinpwin[m_inputno])->Create(fstruct->input_flds[i].prompt,
//						WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP|BS_OWNERDRAW |BS_NOTIFY, 
//						rect, this,  (UINT)(IDC_FORMDESGN_STATIC + m_inputno));
				((CNCLDDButton*)m_dinpwin[m_inputno])->Create(fstruct->input_flds[i].prompt,
						WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP |BS_NOTIFY, 
						rect, this,  (UINT)(IDC_FORMDESGN_STATIC + m_inputno));
				((CNCLDDButton*)m_dinpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDButton*)m_dinpwin[m_inputno])->InitDrag();
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDButton*)m_dinpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, m_input[i], active, tmpstr, tmpstr2);
			}
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

			rect.left = posx;
			rect.top = posy;
			rect.right = rect.left + inpwid;
			rect.bottom = rect.top + cy;
//			((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//			uw_MapDialogRect(rect);
			uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
						
			DWORD dwStyle = WS_CHILD | WS_VISIBLE | ES_AUTOHSCROLL | ES_LEFT | WS_BORDER | WS_TABSTOP | DS_SETFONT;
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
			if (type==11)
				dwStyle |= ES_MULTILINE |ES_WANTRETURN | ES_READONLY;

			m_inpwin[m_inputno] = new CNCLDDEdit(type);
			((CNCLDDEdit*)m_inpwin[m_inputno])->SetParent(this);
			((CNCLDDEdit*)m_inpwin[m_inputno])->m_itemno = m_inputno;
			((CNCLDDEdit*)m_inpwin[m_inputno])->CreateEx(WS_EX_CLIENTEDGE, "EDIT", "EDIT", 
						dwStyle, rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno), 0);
			((CNCLDDEdit*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
			((CNCLDDEdit*)m_inpwin[m_inputno])->InitDrag();						
			sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
			sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
			convert_limit_str(fstruct->input_flds[i].ud_limit, limit_str);
			((CNCLDDEdit*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].range_flag,
				fstruct->input_flds[i].range, fstruct->input_flds[i].font_scale, 
					fstruct->input_flds[i].ud_datatyp, m_input[i], m_len[i], m_prec[i], active, limit_str, tmpstr, tmpstr2);
			((CNCLDDEdit*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
		}
		else
		{
			rect.left = x;
			rect.top = y;
			rect.right = rect.left + cx;
			rect.bottom = rect.top + cy;
//			((CDialog*)((CNCLFormView*)m_parent)->m_parent_dlg)->MapDialogRect(&rect);
//			uw_MapDialogRect(rect);
			uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));

			if ((type==1)||(type==25))
			{
/*
......when in MACRO form define,the form include 6 auto generate item
......which will act different with other item, it only allow moving
......so we save into m_macro_win
*/
				if ((m_macro_flag)&&(i>=macro_start))
				{
					m_macro_win[m_mwin_no] = new CNCLDDButton(type);
					((CNCLDDButton*)m_macro_win[m_mwin_no])->SetParent(this);
					((CNCLDDButton*)m_macro_win[m_mwin_no])->m_itemno = m_mwin_no;
					((CNCLDDButton*)m_macro_win[m_mwin_no])->SetMacroFlag(1);
					if (type==1)
						((CNCLDDButton*)m_macro_win[m_mwin_no])->Create(fstruct->input_flds[i].prompt, 
							WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP |BS_NOTIFY, 
							rect, this,  (UINT)(IDC_FORMDESGN_INPUT + m_mwin_no + 200));
					else
					{
						((CNCLDDButton*)m_macro_win[m_mwin_no])->SetBitMapFile(fstruct->input_flds[i].prompt);
						((CNCLDDButton*)m_macro_win[m_mwin_no])->Create(" ", WS_VISIBLE | WS_CHILD | BS_BITMAP, rect, this, 
								(UINT)(IDC_FORMDESGN_INPUT + m_mwin_no + 200));
					}
					sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
					sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
					active = (int)fstruct->input_flds[i].active_flag;
					((CNCLDDButton*)m_macro_win[m_mwin_no])->set_prop_values(1, 0, active, tmpstr, tmpstr2);
					((CNCLDDButton*)m_macro_win[m_mwin_no])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
					if (type==25)
						((CNCLDDButton*)m_macro_win[m_mwin_no])->set_prop_VideoValue(fstruct->input_flds[i].vfile, fstruct->input_flds[i].prompt);
					((CNCLFdsnFrame*)UW_Dform_frame)->SetMacroActive(m_mwin_no, active);
					if (active!=-1)
						((CNCLDDButton*)m_macro_win[m_mwin_no])->ShowWindow(SW_SHOW);
					else
						((CNCLDDButton*)m_macro_win[m_mwin_no])->ShowWindow(SW_HIDE);
					((CNCLDDButton*)m_macro_win[m_mwin_no])->InitDrag();
					m_mwin_no++;
					macro_add = 1;
/*
......for old macro form, it only have 5 system items instead of 6 (video button added)
......so we need added one after "view button" (video button is 5th item)
*/
					if ((m_mwin_no==4)&&(fstruct->macroflag==0))
					{
						type = 25;
						m_macro_win[m_mwin_no] = new CNCLDDButton(type);
						((CNCLDDButton*)m_macro_win[m_mwin_no])->SetParent(this);
						((CNCLDDButton*)m_macro_win[m_mwin_no])->m_itemno = m_mwin_no;
						((CNCLDDButton*)m_macro_win[m_mwin_no])->SetMacroFlag(1);

						((CNCLDDButton*)m_macro_win[m_mwin_no])->SetBitMapFile("");
/*
.....use "view button" position/size and doing little shift
*/
						rect.left = x;
						rect.top = y;
						rect.right = rect.left + cx;
						rect.bottom = rect.top + cy;
						rect.left = rect.right + 10;
						rect.right = rect.left + cx;
						uw_MapDialogRect(rect, ((CDialog*)(((CNCLFormView*)m_parent)->m_parent_dlg)));
						((CNCLDDButton*)m_macro_win[m_mwin_no])->Create(" ", WS_VISIBLE | WS_CHILD | BS_BITMAP, rect, this, 
								(UINT)(IDC_FORMDESGN_INPUT + m_mwin_no + 200));		
						sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, "default");
						sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, "default");
						active = 1;
						((CNCLDDButton*)m_macro_win[m_mwin_no])->set_prop_values(1, 0, active, tmpstr, tmpstr2);
						((CNCLDDButton*)m_macro_win[m_mwin_no])->set_prop_HSPvalues(0, 0);
						((CNCLDDButton*)m_macro_win[m_mwin_no])->set_prop_VideoValue("", "");
						((CNCLFdsnFrame*)UW_Dform_frame)->SetMacroActive(m_mwin_no, active);
						if (active!=-1)
							((CNCLDDButton*)m_macro_win[m_mwin_no])->ShowWindow(SW_SHOW);
						else
							((CNCLDDButton*)m_macro_win[m_mwin_no])->ShowWindow(SW_HIDE);
						((CNCLDDButton*)m_macro_win[m_mwin_no])->InitDrag();
						m_mwin_no++;
						macro_add = 1;
					}
				}
				else
				{
					m_inpwin[m_inputno] = new CNCLDDButton(type);
					((CNCLDDButton*)m_inpwin[m_inputno])->SetParent(this);
					((CNCLDDButton*)m_inpwin[m_inputno])->m_itemno = m_inputno;
					if (type==1)
						((CNCLDDButton*)m_inpwin[m_inputno])->Create(fstruct->input_flds[i].prompt, 
							WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON|BS_CENTER|WS_TABSTOP |BS_NOTIFY, 
							rect, this,  (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
					else
					{
						((CNCLDDButton*)m_inpwin[m_inputno])->SetBitMapFile(fstruct->input_flds[i].prompt);
						((CNCLDDButton*)m_inpwin[m_inputno])->Create(" ", WS_VISIBLE | WS_CHILD | BS_BITMAP, rect, this, 
								(UINT)(IDC_FORMDESGN_INPUT + m_inputno));
					}
					sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
					sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
					((CNCLDDButton*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, m_input[i], active, tmpstr, tmpstr2);
					if (type==25)
						((CNCLDDButton*)m_inpwin[m_inputno])->set_prop_VideoValue(fstruct->input_flds[i].vfile, fstruct->input_flds[i].prompt);
					((CNCLDDButton*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
					((CNCLDDButton*)m_inpwin[m_inputno])->InitDrag();
					((CNCLDDButton*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
				}
			}
			else if (type==17)
			{
				m_inpwin[m_inputno] = new CNCLDDListCtrl();
				((CNCLDDListCtrl*)m_inpwin[m_inputno])->SetParent(this);
				((CNCLDDListCtrl*)m_inpwin[m_inputno])->m_itemno = m_inputno;
				((CNCLDDListCtrl*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL | WS_HSCROLL | WS_VSCROLL, 
						rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
				((CNCLDDListCtrl*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDListCtrl*)m_inpwin[m_inputno])->InitDrag();

				CNCLDDListCtrl *listctl = (CNCLDDListCtrl*)m_inpwin[m_inputno];
				listctl->InsertColumn(0, "col1",  LVCFMT_LEFT, 50);
				listctl->InsertColumn(1, "col2",  LVCFMT_LEFT, 50);

				listctl->InsertItem(0,  "data1");
				listctl->SetItemText(0, 1, "data2");
				listctl->SetExtendedStyle(LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES);
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDListBox*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr, tmpstr2);
				((CNCLDDListBox*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
			}
			if (type==19)
			{
				m_inpwin[m_inputno] = new CNCLDDListCtrl2();
				((CNCLDDListCtrl2*)m_inpwin[m_inputno])->SetParent(this);
				((CNCLDDListCtrl2*)m_inpwin[m_inputno])->m_itemno = m_inputno;
				((CNCLDDListCtrl2*)m_inpwin[m_inputno])->Create(WS_CHILD|WS_VISIBLE|WS_BORDER|LVS_REPORT | WS_TABSTOP | LVS_SINGLESEL | WS_HSCROLL | WS_VSCROLL, 
					rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
				((CNCLDDListCtrl2*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
				((CNCLDDListCtrl2*)m_inpwin[m_inputno])->InitDrag();
				((CNCLDDListCtrl2*)m_inpwin[m_inputno])->SetType(1);

				CNCLDDListCtrl2 *listctl = (CNCLDDListCtrl2*)m_inpwin[m_inputno];
				LV_COLUMN lvc;
				lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;

				lvc.iSubItem = 0;
				lvc.pszText = "col1";
				lvc.cx = 40;
				lvc.fmt = LVCFMT_LEFT;
				listctl->InsertColumn(0,&lvc);
	
				lvc.iSubItem = 1;
				lvc.pszText = "col2";
				lvc.cx = 40;
				lvc.fmt = LVCFMT_LEFT;
				listctl->InsertColumn(1,&lvc);

				listctl->InsertItem(0,  "data1");
				listctl->SetItemText(0, 1, "data2");
				listctl->m_nNumberOfRows = 1;
				listctl->m_nNumberOfCols = 2;
				listctl->ModifyStyle(0,LVS_SHOWSELALWAYS);
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDListBox*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr, tmpstr2);
				((CNCLDDListBox*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
			}
			else if (type==5)
			{
				m_inpwin[m_inputno] = new CNCLDDListBox(type, 1);
				((CNCLDDListBox*)m_inpwin[m_inputno])->SetParent(this);
				((CNCLDDListBox*)m_inpwin[m_inputno])->m_itemno = m_inputno;
				((CNCLDDListBox*)m_inpwin[m_inputno])->CreateEx(WS_EX_CLIENTEDGE, "LISTBOX", "listbox", 
							WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_BORDER | LBS_NOTIFY | WS_VSCROLL | LBS_USETABSTOPS | WS_HSCROLL,
							rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
				((CNCLDDListBox*)m_inpwin[m_inputno])->InitDrag();
				((CNCLDDListBox*)m_inpwin[m_inputno])->AddString("test");
				((CNCLDDListBox*)m_inpwin[m_inputno])->AddString("test2");
				((CNCLDDListBox*)m_inpwin[m_inputno])->SetCurSel(0);
				sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
				sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
				((CNCLDDListBox*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, active, tmpstr, tmpstr2);
				((CNCLDDListBox*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
			}
			else if (type==7)
			{
/*
......when in MACRO form define,the form include 6 auto generate item
......which will act different with other item, it only allow moving
......so we save into m_macro_win
*/
				if ((m_macro_flag)&&(i>=macro_start))
				{
					m_macro_win[m_mwin_no] = new CNCLDDButton(type);
					((CNCLDDButton*)m_macro_win[m_mwin_no])->SetParent(this);
					((CNCLDDButton*)m_macro_win[m_mwin_no])->m_itemno = m_mwin_no;
					((CNCLDDButton*)m_macro_win[m_mwin_no])->SetMacroFlag(1);
					((CNCLDDButton*)m_macro_win[m_mwin_no])->Create(fstruct->input_flds[i].prompt, WS_CHILD|WS_VISIBLE|BS_LEFT|BS_AUTOCHECKBOX|WS_TABSTOP|BS_NOTIFY, rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
					sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
					sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
					active = (int)fstruct->input_flds[i].active_flag;
					((CNCLDDButton*)m_macro_win[m_mwin_no])->set_prop_values(1, 0, active, tmpstr, tmpstr2);
					((CNCLDDButton*)m_macro_win[m_mwin_no])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
					((CNCLFdsnFrame*)UW_Dform_frame)->SetMacroActive(m_mwin_no, active);
					if (active!=-1)
						((CNCLDDButton*)m_macro_win[m_mwin_no])->ShowWindow(SW_SHOW);
					else
						((CNCLDDButton*)m_macro_win[m_mwin_no])->ShowWindow(SW_HIDE);
					((CNCLDDButton*)m_macro_win[m_mwin_no])->InitDrag();
					m_mwin_no++;
					macro_add = 1;
				}
				else
				{
					m_inpwin[m_inputno] = new CNCLDDButton(type);
					((CNCLDDButton*)m_inpwin[m_inputno])->SetParent(this);
					((CNCLDDButton*)m_inpwin[m_inputno])->m_itemno = m_inputno;
					((CNCLDDButton*)m_inpwin[m_inputno])->Create(fstruct->input_flds[i].prompt, WS_CHILD|WS_VISIBLE|BS_LEFT|BS_AUTOCHECKBOX|WS_TABSTOP|BS_NOTIFY, rect, this, (UINT)(IDC_FORMDESGN_INPUT + m_inputno));
					sprintf(tmpstr, "%s, %s", fstruct->input_flds[i].fcolor, fstruct->input_flds[i].bcolor);
					sprintf(tmpstr2, "%s, %s", fstruct->input_flds[i].pfcolor, fstruct->input_flds[i].pbcolor);
					active = (int)fstruct->input_flds[i].active_flag;
					((CNCLDDButton*)m_inpwin[m_inputno])->set_prop_values(fstruct->input_flds[i].font_scale, m_input[i], active, tmpstr, tmpstr2);
					((CNCLDDButton*)m_inpwin[m_inputno])->ShowWindow(SW_SHOW);
					((CNCLDDButton*)m_inpwin[m_inputno])->InitDrag();
					((CNCLDDButton*)m_inpwin[m_inputno])->set_prop_HSPvalues(fstruct->input_flds[i].n_picarea, fstruct->input_flds[i].picarea);
				}
			}
		}
		if (macro_add==0)
		{
			m_type[m_inputno] = type;
			m_inputno++;
		}
	}
	updateWindow_draw();
	((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(0);

	CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
	pview->SetDtype(-2);
	pview->filldata();
	pview->SetFormInputNo(m_inputno);
	pview->SetFormSecNo(m_secno);
}

static int S_IsDefaultColor(CString color)
{
	color.Remove(' ');
	if (color.CompareNoCase("Default,Default")==0)
		return 1;
	else
		return 0;
}
/***********************************************************************
**
**   FUNCTION: AdjuctMoveWindows(int flag)
**		Adjust window position accord to flag input
**		
**	 INPUT:  flag: 1: left margin
**				 2: right margin
**				 3: top margin
**				 4: bottom margin
**				 5: same width
**				 6: same height
**				 7: same size
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::AdjuctMoveWindows(int flag)
{
	if ((m_selwin==NULL)||(m_selnum==1))
		return;
	CRect rect, orig_rect;
	m_selwin->GetWindowRect(&orig_rect);
	int i, item_no, delx, dely;
	CWnd * winitem;
	CRect old_rect;
	for (i=0;i<m_selnum;i++)
	{
		item_no = m_selarray[i];
		winitem = NULL;
		if (m_seltype[i]==0)
		{
			if (m_disp_win[item_no]!=NULL)
			{
				winitem = m_disp_win[item_no];
			}
		}
		if (m_seltype[i]==1)
		{
			if (m_frame[item_no]!=NULL)
			{
				winitem = m_frame[item_no];
			}
		}
		if (m_seltype[i]==2)
		{
			if (m_picture[item_no]!=NULL)
			{
				winitem = m_picture[item_no];
			}
		}
		if (m_seltype[i]==3)
		{
			if (m_dinpwin[item_no]!=NULL)
			{
				winitem = m_dinpwin[item_no];
			}
		}
		if (m_seltype[i]==4)
		{
			if (m_inpwin[item_no]!=NULL)
			{
				winitem = m_inpwin[item_no];
			}
		}
		if (m_seltype[i]==5)
		{
			if (m_macro_win[item_no]!=NULL)
			{
				winitem = m_macro_win[item_no];
			}
		}
		if (winitem==NULL)
			return;
		winitem->GetWindowRect(&rect);
		if (flag==1)
/*
.....left margin
*/
		{
			delx = orig_rect.left - rect.left;
			rect.left = orig_rect.left;
			rect.right = rect.right + delx;
		}
		if (flag==2)
/*
.....right margin
*/
		{
			delx = orig_rect.right - rect.right;
			rect.right = orig_rect.right;
			rect.left = rect.left + delx;
		}
		if (flag==3)
/*
.....top margin
*/
		{
			dely = orig_rect.top - rect.top;
			rect.top = orig_rect.top;
			rect.bottom = rect.bottom + dely;
		}
		if (flag==4)
/*
.....bottom margin
*/
		{
			dely = orig_rect.bottom - rect.bottom;
			rect.bottom = orig_rect.bottom;
			rect.top = rect.top + dely;
		}
		if (flag==5)
/*
.....same width
*/
		{
			rect.right = rect.left + orig_rect.Width();
		}
		if (flag==6)
/*
.....same height
*/
		{
			rect.bottom = rect.top + orig_rect.Height();
		}
		if (flag==7)
/*
.....same size
*/
		{
			rect.right = rect.left + orig_rect.Width();
			rect.bottom = rect.top + orig_rect.Height();
		}
		ScreenToClient(&rect);
		winitem->MoveWindow(&rect,1);
		winitem->RedrawWindow();
		winitem->UpdateWindow();
	}
	RedrawWindow();
	UpdateWindow();
	m_parent->RedrawWindow();
	m_parent->UpdateWindow();
}
/***********************************************************************
**
**   FUNCTION: SaveForm(char* filename, char *title, int fcx, int fcy, int frmtype, CString helpstr)
**		save the current window form into a form file
**		
**	 INPUT:  filename: file to be saved
**			title: form title
**			fcx, fcy: form size
**			frmtype: form type
**			helpstr: help string to be save into form
**			flag: save as flag, 1, act as SAVE AS, 0: Saved replace the old file without ask
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CNCLDDform::SaveForm(char* filename, char *title, int fcx, int fcy, int frmtype, 
	CString helpstr, int flag)
{
/*
.....save all into a form file
*/
	UX_pathname fname, dir, fullname, tempstr;
	char* temp, msg[UX_MAX_PATH_LEN+40], *hlpstr;
	int i, k, len, status, ans,forms,x,y,cx,cy, x1,y1;
	FILE *fd;
	CRect rect, drect;
	strcpy(fname, filename);
/*
.....open for read to check if file exist
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "forms", (char*)UU_NULL, (char*)UU_NULL,
		fname, 0, (FILE**)UU_NULL);
	if ((status == UU_SUCCESS) && (fname[0]!='\0'))
	{
		if (flag)
		{
			sprintf_s(msg, sizeof(msg), "File %s exists, overwrite?", fname);
			ans = uw_ntyes_or_no(NULL, msg, "Overwrite file?");
			if (ans!=1)
				return 0;
		}
		remove(fname);
	}
	status = ux_create_file(fname, 0666, 0, "STREAM", "ASCII",
		"UX_NOHEADER", &forms, UX_PRTERRS);
	if (status != 0)
	{
		sprintf_s(msg, sizeof(msg), "Can not create file %s!", fname);
		uw_nterror(msg);
		return -1;
	}
	int baseunitX, baseunitY;
	CClientDC dc(this);
	CFont* savfont = dc.SelectObject(&m_Font );
		
	CSize sizeText = dc.GetTextExtent("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",52);
	baseunitX = sizeText.cx/52.0;
	baseunitY = sizeText.cy;
	dc.SelectObject(&savfont);

	if (status == 0)
	{
		ux_get_os_filedesc(forms, &fd, UX_PRTERRS);
/*
.....save #HEADER# section
*/
		ux_fputs0("\n#HEADER#\n", fd);
		sprintf_s(tempstr, sizeof(tempstr), "/TITLE/ %s\n", title);
		ux_fputs0(tempstr, fd);
		sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", 0,0);
		ux_fputs0(tempstr, fd);

		CRect rect;
		GetWindowRect(&rect);
		((CNCLFormView*)m_parent)->ScreenToClient(&rect);
/*
.....convert screen pixel to dialog unit
*/
		if (frmtype==0)
		{
/*
.....dockable=0, floating
*/
			if (((CNCLFormView*)m_parent)->m_sec_flag)
			{
				cx = rect.Width()+8-2;
				cy = rect.Height()+8;
//				uw_revMapLen(cx, cy);
				uw_revMapLen(cx, cy, baseunitX, baseunitY);
				cy += 20;
			}
			else
			{
				cx = rect.Width()-1;
				cy = rect.Height()-6-1;
//				uw_revMapLen(cx, cy);
				uw_revMapLen(cx, cy, baseunitX, baseunitY);
				cy += 25;
			}
		}
		else
		{
/*
.....dockable
*/
			cx = rect.Width();
			cy = rect.Height();
//			uw_revMapLen(cx, cy);
			uw_revMapLen(cx, cy, baseunitX, baseunitY);
		}	
		sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", cx, cy);
		ux_fputs0(tempstr, fd);

		if (frmtype==1)
		{
			ux_fputs0("/DOCKABLE/ YES\n", fd);
		}
		if (frmtype==2)
		{
			ux_fputs0("/DOCKABLE/ TOP\n", fd);
		}
		if (frmtype==3)
		{
			ux_fputs0("/DOCKABLE/ BOTTOM\n", fd);
		}
		if (frmtype==4)
		{
			ux_fputs0("/DOCKABLE/ LEFT\n", fd);
		}
		if (frmtype==5)
		{
			ux_fputs0("/DOCKABLE/ RIGHT\n", fd);
		}
		char label[256];
		int active;
		CWnd *parent_dlg;
/*
......saved by section
*/
		char name[40], color[40];
		CNCLFormProp* dprop = NULL, *prop=NULL;
		int type, no_prompt;
		int curpage = 0;
		int secpage;
		int all_sec = isallsec();
start_sec:;
		secpage = m_secno - curpage;
		if ((frmtype!=0)||(secpage>=0)||((secpage<=0)&&(all_sec)))
		{
			if (((m_secno>0)&&(secpage>0))&&(frmtype==0)&&(((CNCLFormView*)m_parent)->m_sec_flag))
			{
				ux_fputs0("\n#SECTION#\n", fd);
				((CNCLFormView*)m_parent)->GetSecInfo(curpage, name, color);
				sprintf_s(tempstr, sizeof(tempstr), "/NAME/ %s\n", name);
				ux_fputs0(tempstr, fd);
				if (color[0]!='\0')
					sprintf_s(tempstr, sizeof(tempstr), "/COLOR/ *%s\n", color);
				else
					strcpy(tempstr, "/COLOR/ *DEFAULT\n");
				ux_fputs0(tempstr, fd);
			}
			if (((secpage<=0)&&(all_sec))&&(frmtype==0))
			{
				if ((m_secno>0)&&(((CNCLFormView*)m_parent)->m_sec_flag))
				{
					ux_fputs0("\n#SECTION#\n", fd);
					ux_fputs0("/NAME/ ALL\n", fd);
				}
				curpage = -1;
			}
			if (frmtype!=0)
				curpage = -1;
/*
.....save the frame
*/
			for (i=0; i<m_frameno;i++)
			{
				if ((m_frm_secpage[i]!=curpage)&&(frmtype==0))
					continue;
				if (m_frame[i]!=NULL)
				{
					CNCLFormProp* prop = ((CNCLDDGroup*)m_frame[i])->m_prop_dlg;
					((CNCLDDGroup*)m_frame[i])->GetWindowText(label, 256);
					ux_fputs0("\n#FRAME#\n", fd);
					sprintf_s(tempstr, sizeof(tempstr), "/TITLE/ %s\n", label);
					ux_fputs0(tempstr, fd);
					if ((prop!=NULL)&&(prop->m_pcolor[0]!=0))
					{
						if (S_IsDefaultColor(prop->m_pcolor)==0)
						{
							sprintf_s(tempstr, sizeof(tempstr), "/COLOR/ %s\n", prop->m_pcolor);
							ux_fputs0(tempstr, fd);
						}
					}
					if ((prop!=NULL)&&(prop->m_font!=1))
					{
						sprintf_s(tempstr, sizeof(tempstr), "/FONT/ %f\n", prop->m_font);
						ux_fputs0(tempstr, fd);
					}
					m_frame[i]->GetWindowRect(&rect);
					ScreenToClient(&rect);
					x = rect.left;
					y = rect.top;
					cx = rect.Width();
					cy = rect.Height();
/*
.....convert screen pixel to dialog unit
*/
/*					x = (x*4)/baseunitX;
					y = (y*8)/baseunitY;				
					cx = (cx*4)/baseunitX;
					cy = (cy*8)/baseunitY;				
*/
					uw_revMapLen(x, y, baseunitX, baseunitY);
					uw_revMapLen(cx, cy, baseunitX, baseunitY);
					sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", x, y);
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", cx, cy);
					ux_fputs0(tempstr, fd);
				}
			}
/*
.....save pictures
*/
			for (i=0; i<m_picno;i++)
			{
				if ((m_pic_secpage[i]!=curpage)&&(frmtype==0))
					continue;
				if (m_picture[i]!=NULL)
				{
					ux_fputs0("\n#PICTUREBOX#\n", fd);
					if (((CNCLDDPicWin*)m_picture[i])->m_name[0]=='\0')
					{
						strcpy(tempstr, "/NAME/ PIC_1\n");
					}
					else
						sprintf_s(tempstr, sizeof(tempstr), "/NAME/ %s\n", ((CNCLDDPicWin*)m_picture[i])->m_name);
					ux_fputs0(tempstr, fd);
					if (((CNCLDDPicWin*)m_picture[i])->m_filename!="")
					{
/*
.....only save the filename, no path
*/
						strcpy(fullname, ((CNCLDDPicWin*)m_picture[i])->m_filename);
						ul_break_fname(fullname, dir, fname);
						sprintf_s(tempstr, sizeof(tempstr), "/FILE/ %s\n", fname);
						ux_fputs0(tempstr, fd);
					}
					m_picture[i]->GetWindowRect(&rect);
					ScreenToClient(&rect);
					x = rect.left;
					y = rect.top;
					cx = rect.Width();
					cy = rect.Height();
/*
.....convert screen pixel to dialog unit
*/
/*					x = (x*4)/baseunitX;
					y = (y*8)/baseunitY;				
					cx = (cx*4)/baseunitX;
					cy = (cy*8)/baseunitY;				
*/
					uw_revMapLen(x, y, baseunitX, baseunitY);
					uw_revMapLen(cx, cy, baseunitX, baseunitY);
					sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", x, y);
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", cx, cy);
					ux_fputs0(tempstr, fd);
				}
			}
			for (i=0; i<m_dispno; i++)
			{
				if ((m_disp_secpage[i]!=curpage)&&(frmtype==0))
					continue;
				if (m_disp_win[i]!=NULL)
				{
					CNCLFormProp* prop = ((CNCLDDStatic*)m_disp_win[i])->m_prop_dlg;
					m_disp_win[i]->GetWindowText(label, 256);
					ux_fputs0("\n#LABEL#\n", fd);
					sprintf_s(tempstr, sizeof(tempstr), "/LABEL/ %s\n", label);
					ux_fputs0(tempstr, fd);

					m_disp_win[i]->GetWindowRect(&rect);
					ScreenToClient(&rect);
					x = rect.left;
					y = rect.top;
					cx = rect.Width();
					cy = rect.Height();
/*
.....convert screen pixel to dialog unit
*/
/*					x = (x*4)/baseunitX;
					y = (y*8)/baseunitY;				
					cx = (cx*4)/baseunitX;
					cy = (cy*8)/baseunitY;				
*/
					uw_revMapLen(x, y, baseunitX, baseunitY);
					uw_revMapLen(cx, cy, baseunitX, baseunitY);
					sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", x, y);
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", cx, cy);
					ux_fputs0(tempstr, fd);
					ux_fputs0("/TYPE/UD_DASSTRING\n", fd);
					if ((prop!=NULL)&&(prop->m_active==-1))
					{
						strcpy(tempstr, "/ACTIVE/ NO\n");
						ux_fputs0(tempstr, fd);
					}
					else if ((prop!=NULL)&&(prop->m_active==1))
					{
						strcpy(tempstr, "/ACTIVE/ YES\n");
						ux_fputs0(tempstr, fd);
					}
				}
			}
			dprop = NULL, prop=NULL;
			for (i=0; i<m_inputno; i++)
			{
				if ((m_inp_secpage[i]!=curpage)&&(frmtype==0))
					continue;
				no_prompt = 0;
				prop = NULL;
				dprop = NULL;
				if (m_inpwin[i]!=NULL)
				{
					type = m_type[i];
					if (type==1)
					{
						ux_fputs0("\n#PUSHBUTTON#\n", fd);
						prop = ((CNCLDDButton*)m_inpwin[i])->m_prop_dlg;
						no_prompt = 1;
					}
					else if (type==25)
					{
						ux_fputs0("\n#IMGBUTTON#\n", fd);
						prop = ((CNCLDDButton*)m_inpwin[i])->m_prop_dlg;
						no_prompt = 1;
					}
					else if (type==2)
					{
						ux_fputs0("\n#CHOICEBOX#\n", fd);
						dprop = ((CNCLDDStatic*)m_dinpwin[i])->m_prop_dlg;
						prop = ((CNCLDDCombo*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==3)
					{
						ux_fputs0("\n#EDIT#\n", fd);

						if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
						{
							dprop = ((CNCLDDStatic*)m_dinpwin[i])->m_prop_dlg;
						}
						if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
						{
							dprop = ((CNCLDDButton*)m_dinpwin[i])->m_prop_dlg;
						}
						prop = ((CNCLDDEdit*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==23)
					{
						ux_fputs0("\n#SELECT#\n", fd);
						dprop = ((CNCLDDButton*)m_dinpwin[i])->m_prop_dlg;
						prop = ((CNCLDDColorButton*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==24)
					{
						ux_fputs0("\n#SLIDER#\n", fd);
						dprop = ((CNCLDDStatic*)m_dinpwin[i])->m_prop_dlg;
						prop = ((CNCLDDSlider*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==5)
					{
						ux_fputs0("\n#LISTBOX#\n", fd);
						prop = ((CNCLDDListBox*)m_inpwin[i])->m_prop_dlg;
						no_prompt = 1;
					}
					else if (type==7)
					{
						ux_fputs0("\n#CHECKBOX#\n", fd);
						prop = ((CNCLDDButton*)m_inpwin[i])->m_prop_dlg;
						no_prompt = 1;
					}
					else if (type==8)
					{
						ux_fputs0("\n#COMBLIST_SIMPLE#\n", fd);
						dprop = ((CNCLDDStatic*)m_dinpwin[i])->m_prop_dlg;
						prop = ((CNCLDDCombo*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==9)
					{
						ux_fputs0("\n#COMBLIST_DROPDOWN#\n", fd);
						dprop = ((CNCLDDStatic*)m_dinpwin[i])->m_prop_dlg;
						prop = ((CNCLDDCombo*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==11)
					{
						ux_fputs0("\n#DISPLAY#\n", fd);
						if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
						{
							dprop = ((CNCLDDStatic*)m_dinpwin[i])->m_prop_dlg;
						}
						if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
						{
							dprop = ((CNCLDDButton*)m_dinpwin[i])->m_prop_dlg;
						}
						prop = ((CNCLDDEdit*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==13)
					{
						ux_fputs0("\n#PROGRESS#\n", fd);
						dprop = ((CNCLDDStatic*)m_dinpwin[i])->m_prop_dlg;
						prop = ((CNCLDDProcess*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==15)
					{
						ux_fputs0("\n#CHOICE_LIST#\n", fd);
						dprop = ((CNCLDDStatic*)m_dinpwin[i])->m_prop_dlg;
						prop = ((CNCLDDCombo*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==16)
					{
						ux_fputs0("\n#EDITBOX#\n", fd);
						if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
						{
							dprop = ((CNCLDDStatic*)m_dinpwin[i])->m_prop_dlg;
						}
						if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
						{
							dprop = ((CNCLDDButton*)m_dinpwin[i])->m_prop_dlg;
						}
						prop = ((CNCLDDEdit*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==17)
					{
						ux_fputs0("\n#LISTTABLE#\n", fd);
						prop = ((CNCLDDListCtrl*)m_inpwin[i])->m_prop_dlg;
						no_prompt = 1;
					}
					else if (type==18)
					{
						ux_fputs0("\n#COLOR#\n", fd);
						dprop = ((CNCLDDStatic*)m_dinpwin[i])->m_prop_dlg;
						prop = ((CNCLDDColorButton*)m_inpwin[i])->m_prop_dlg;
					}
					else if (type==19)
					{
						ux_fputs0("\n#DATATABLE#\n", fd);
						prop = ((CNCLDDListCtrl2*)m_inpwin[i])->m_prop_dlg;
						no_prompt = 1;
					}
					else
						MessageBox("Invalid Field Type Entered!", "Error", MB_OK);

					if (dprop==NULL)
					{
						if (no_prompt==0)
						{
							if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
							{
								((CNCLDDStatic*)m_dinpwin[i])->GetLabelText(label,256);
							}
							if (m_dinpwin[i]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
							{
								((CNCLDDButton*)m_dinpwin[i])->GetWindowText(label,256);
							}
						}
						else
						{
//							m_inpwin[i]->GetWindowTextA(label,256);
							char *temp = (prop->m_label).GetBuffer();
							strcpy(label, temp);
						}
						sprintf_s(tempstr, sizeof(tempstr), "/LABEL/ %s\n", label);
					}
					else
					{
						sprintf_s(tempstr, sizeof(tempstr), "/LABEL/ %s\n", dprop->m_label);
					}
					ux_fputs0(tempstr, fd);
					if (type==25)
					{
						sprintf_s(tempstr, sizeof(tempstr), "/FILE/ %s\n", prop->m_choices);
						ux_fputs0(tempstr, fd);
					}
					if (m_dinpwin[i]!=NULL)
						m_dinpwin[i]->GetWindowRect(&drect);
					m_inpwin[i]->GetWindowRect(&rect);
					ScreenToClient(&drect);
					ScreenToClient(&rect);
					x1 = drect.left;
					y1 = drect.top;
					x = rect.left;
					y = rect.top;
/*
.....convert screen pixel to dialog unit
*/
/*					x = (x*4)/baseunitX;
					y = (y*8)/baseunitY;				
*/
					uw_revMapLen(x, y, baseunitX, baseunitY);
					RECT listrect;
					if (m_dinpwin[i]!=NULL)
					{
/*						x1 = (x1*4)/baseunitX;
						y1 = (y1*8)/baseunitY;				
*/
						uw_revMapLen(x1, y1, baseunitX, baseunitY);
						sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d,%d,%d\n", x1, y1, x, y);
						cx = rect.right - drect.left + 1;
						cy = rect.Height();
						if ((type==2)||(type==9)||(type==15))
						{
							((CNCLDDCombo*)(m_inpwin[i]))->GetDroppedControlRect(&listrect);
							cy = (listrect.bottom - listrect.top);
						}
					}
					else
					{
						sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", x, y);
						cx = rect.Width();
						cy = rect.Height();
						if ((type==2)||(type==9)||(type==15))
						{
							((CNCLDDCombo*)(m_inpwin[i]))->GetDroppedControlRect(&listrect);
							cy = (listrect.bottom - listrect.top);
						}
					}
					ux_fputs0(tempstr, fd);
/*
.....convert screen pixel to dialog unit
*/
/*					cx = (cx*4)/baseunitX;
					cy = (cy*8)/baseunitY;				
*/
					uw_revMapLen(cx, cy, baseunitX, baseunitY);
					sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", cx, cy);
					ux_fputs0(tempstr, fd);
				
					if ((prop!=NULL)&&((type==3)||(type==11)||(type==16)))
/*
......EDIT
*/
					{
						if (prop->m_type==UD_DASCART)
							ux_fputs0("/TYPE/UD_DASCART\n", fd);
						if (prop->m_type==UD_DASVAL)
							ux_fputs0("/TYPE/UD_DASVAL\n", fd);
						if (prop->m_type==UD_DASDISTANCE)
							ux_fputs0("/TYPE/UD_DASDISTANCE\n", fd);
						if (prop->m_type==UD_DASINT)
							ux_fputs0("/TYPE/UD_DASINT\n", fd);
						if (prop->m_type==UD_DASVEC)
							ux_fputs0("/TYPE/UD_DASVEC\n", fd);
						if (prop->m_type==UD_DASSTRING)
							ux_fputs0("/TYPE/UD_DASSTRING\n", fd);
						if (prop->m_type==UD_DASNDC)
							ux_fputs0("/TYPE/UD_DASNDC\n", fd);
						if (prop->m_type==UD_DASANGLE)
							ux_fputs0("/TYPE/UD_DASANGLE\n", fd);
						if (prop->m_type==UD_DASUNITLESS)
							ux_fputs0("/TYPE/UD_DASUNITLESS\n", fd);
						if (prop->m_type==UD_DASPLANE)
							ux_fputs0("/TYPE/UD_DASPLANE\n", fd);
						if (prop->m_type==UD_DASSCALAR)
							ux_fputs0("/TYPE/UD_DASSCALAR\n", fd);
						if (prop->m_type==UD_SCACART)
							ux_fputs0("/TYPE/UD_SCACART\n", fd);
						if (prop->m_type==UD_SCAVAL)
							ux_fputs0("/TYPE/UD_SCAVAL\n", fd);
						if (prop->m_type==UD_SCADISTANCE)
							ux_fputs0("/TYPE/UD_SCADISTANCE\n", fd);
						if (prop->m_type==UD_SCAINT)
							ux_fputs0("/TYPE/UD_SCAINT\n", fd);
						if (prop->m_type==UD_SCAVEC)
							ux_fputs0("/TYPE/UD_SCAVEC\n", fd);
						if (prop->m_type==UD_SCANDC)
							ux_fputs0("/TYPE/UD_SCANDC\n", fd);
						if (prop->m_type==UD_SCAANGLE)
							ux_fputs0("/TYPE/UD_SCAANGLE\n", fd);
						if (prop->m_type==UD_SCAUNITLESS)
							ux_fputs0("/TYPE/UD_SCAUNITLESS\n", fd);
					}
					else
					{
						if ((type==1)&&(type==7)&&(type==24))
							ux_fputs0("/TYPE/UD_DASINT\n", fd);
						else
							ux_fputs0("/TYPE/UD_DASSTRING\n", fd);
					}
					if ((prop!=NULL)&&((type==3)||(type==11)||(type==16)))
/*
......EDIT
*/
					{
						if (prop->m_input==FORM_STRING)
							ux_fputs0("/INPUT/FORM_STRING\n", fd);
						if (prop->m_input==FORM_PICK)
							ux_fputs0("/INPUT/FORM_PICK\n", fd);
						if (prop->m_input==FORM_LOCATE)
							ux_fputs0("/INPUT/FORM_LOCATE\n", fd);
						if (prop->m_input==FORM_LABEL)
							ux_fputs0("/INPUT/FORM_LABEL\n", fd);
						if (prop->m_input==FORM_SUBSCR)
							ux_fputs0("/INPUT/FORM_SUBSCR\n", fd);
//						if (prop->m_input==FORM_SELECT)
//							ux_fputs0("/INPUT/FORM_SELECT\n", fd);
					}
					else if ((prop!=NULL)&&((type==5)||(type==8)||(type==9) ||(type==15)
						||(type==17)||(type==19)))
					{
						if (prop->m_input==FORM_STRING)
							ux_fputs0("/INPUT/FORM_STRING\n", fd);
						if (prop->m_input==FORM_RECORD)
							ux_fputs0("/INPUT/FORM_RECORD\n", fd);
					}
					else if ((prop!=NULL)&&(type==24))
					{
						if (prop->m_justified==1)
							ux_fputs0("/JUSTIFIED/ VERT\n", fd);
						else
							ux_fputs0("/JUSTIFIED/ HORZ\n", fd);
					}
					if ((prop!=NULL)&&((type==3)||(type==11)||(type==16)||(type==23)))
						len = prop->m_limit.GetLength();
					else
						len = 0;
					if (prop->m_input==FORM_STRING)
						len = 0;
					if ((len>0)&&((type==3)||(type==11)||(type==16)||(type==23)))
					{
						sprintf(tempstr, "/LIMIT/ %s\n", prop->m_limit.GetBuffer());
						ux_fputs0(tempstr, fd);
					}
//why 18					if ((prop!=NULL)&&((type==2)||(type==18)))
					if ((prop!=NULL)&&(type==2))
						len = prop->m_choices.GetLength();
					else
						len = 0;
					if ((len==0)&&(type==2))
					{
	/*
	......we must have choice setup for CHOICEBOX
	*/
						MessageBox("CHOICEBOX fields must have defined choices.","Error",
							MB_OK);
						status = -1;
						goto done;
					}
//why 18					if ((len>0)&&((type==2)||(type==18)))
					if ((len>0)&&(type==2))
					{
						sprintf(tempstr, "/CHOICES/ %s\n", prop->m_choices.GetBuffer());
						ux_fputs0(tempstr, fd);
					}
					if (type==24)
					{
						if (prop->m_range_flag)
						{
							sprintf(tempstr, "/RANGE/ %d, %d\n", prop->m_range[0].dint, prop->m_range[1].dint);
							ux_fputs0(tempstr, fd);			
						}
					}
					if ((type==3)||(type==11)||(type==16))
					{
						if (prop->m_type==UD_DASUNITLESS)
						{
							if (prop!=NULL)
							{
								sprintf(tempstr, "/LEN/ %d\n", prop->m_len);
								ux_fputs0(tempstr, fd);					
								sprintf(tempstr, "/PREC/ %d\n", prop->m_prec);
								ux_fputs0(tempstr, fd);		
								if (prop->m_range_flag)
								{
									sprintf(tempstr, "/RANGE/ %f, %f\n", prop->m_range[0].dreal, prop->m_range[1].dreal);
									ux_fputs0(tempstr, fd);			
								}
							}
							else
							{
								sprintf(tempstr, "/LEN/ %d\n", 4);
								ux_fputs0(tempstr, fd);					
								sprintf(tempstr, "/PREC/ %d\n", 3);
								ux_fputs0(tempstr, fd);					
							}
						}
						else
						{
/*
......we still need save the LEN value since we used for EDIT cx size
*/
							m_inpwin[i]->GetWindowRect(&rect);
							cx = rect.Width();
							uw_revMapLen(cx, cy, baseunitX, baseunitY);
							len = (cx - 6)/(TEXT_WID * uw_form_scalex) + 0.5;
							sprintf(tempstr, "/LEN/ %d\n", len);
							ux_fputs0(tempstr, fd);					
						}
					}
					if (prop!=NULL)
						len = prop->m_color.GetLength();
					else
						len = 0;

					if (len>0)
					{
						if (S_IsDefaultColor(prop->m_color)==0)
						{
							sprintf(tempstr, "/COLOR/ %s\n", prop->m_color);
							ux_fputs0(tempstr, fd);
						}
					}
					if (type!=1)
					{
						if (prop!=NULL)
							len = prop->m_pcolor.GetLength();
						else
							len = 0;
						if (len>0)
						{
							if (S_IsDefaultColor(prop->m_pcolor)==0)
							{
								sprintf(tempstr, "/PCOLOR/ %s\n", prop->m_pcolor);
								ux_fputs0(tempstr, fd);
							}
						}
					}
					if ((prop!=NULL)&&(prop->m_font!=1))
					{
						sprintf(tempstr, "/FONT/ %f\n", prop->m_font);
						ux_fputs0(tempstr, fd);					
					}				
					if ((prop!=NULL)&&(prop->m_active==-1))
					{
						strcpy(tempstr, "/ACTIVE/ NO\n");
						ux_fputs0(tempstr, fd);
					}
					else if ((prop!=NULL)&&(prop->m_active==1))
					{
						strcpy(tempstr, "/ACTIVE/ YES\n");
						ux_fputs0(tempstr, fd);
					}
/*
.....picture area
*/
//					if ((prop->m_pic_label!="")&&(prop->m_pic_rect.left!=-1000))
					for (k=0; k<prop->m_picarea_no;k++)
					{
						if (prop->m_picarea[k].name!="")
						{
							if (prop->m_picarea[k].tooltext!=NULL)
								sprintf(tempstr, "/PICTURE/%s,\"%s\",%d,%d,%d,%d", 
										prop->m_picarea[k].name, 
										prop->m_picarea[k].tooltext,
										(int)prop->m_picarea[k].xmin, (int)prop->m_picarea[k].ymin, 
										(int)prop->m_picarea[k].xmax, (int)prop->m_picarea[k].ymax);
							else
								sprintf(tempstr, "/PICTURE/%s,\"\",%d,%d,%d,%d", 
										prop->m_picarea[k].name, 
										(int)prop->m_picarea[k].xmin, (int)prop->m_picarea[k].ymin, 
										(int)prop->m_picarea[k].xmax, (int)prop->m_picarea[k].ymax);
						
							if (prop->m_picarea[k].params!=NULL)
							{		
								strcat(tempstr, ",");
								strcat(tempstr, prop->m_picarea[k].params);
							}
							strcat(tempstr, "\n");
							ux_fputs0(tempstr, fd);
						}
					}
				}
			}
			if (curpage==-1)
			{
				if (m_macro_flag==0)
					goto help_sec;
				if (m_mwin_no>0)					
					ux_fputs0("\n#MACRO#\n", fd);

				parent_dlg = ((CNCLFormView*)m_parent)->m_parent_dlg;
				for (i=0; i<m_mwin_no; i++)
				{
					if (m_macro_win[i]!=NULL)
					{
						type = ((CNCLDDButton*)m_macro_win[i])->m_type;
						if (type==1)
						{
							ux_fputs0("\n#PUSHBUTTON#\n", fd);
						}
						else if (type==25)
						{
							ux_fputs0("\n#IMGBUTTON#\n", fd);
						}
						else if (type==7)
						{
							ux_fputs0("\n#CHECKBOX#\n", fd);
						}
						else
							MessageBox("Invalid Form Field Type!", "Error", MB_OK);
	/*
	.....we allow user chnage the label
	*/
//						((CNCLDDButton*)m_macro_win[i])->GetWindowText(label,256);
//						sprintf_s(tempstr, sizeof(tempstr), "/LABEL/ %s\n", label);
						prop = ((CNCLDDButton*)m_macro_win[i])->m_prop_dlg;
						sprintf_s(tempstr, sizeof(tempstr), "/LABEL/ %s\n", prop->m_label);
						ux_fputs0(tempstr, fd);
						if (type==25)
						{
							sprintf_s(tempstr, sizeof(tempstr), "/FILE/ %s\n", prop->m_choices);
							ux_fputs0(tempstr, fd);
						}
						m_macro_win[i]->GetWindowRect(&rect);
						ScreenToClient(&rect);
						uw_revMapDialogRect(rect, baseunitX, baseunitY);
						x = rect.left;
						y = rect.top;
/*
.....convert screen pixel to dialog unit
*/
//have problem use MapDialogRect of MFC and baseunitX way
/*
						x = (x*4)/baseunitX;
						y = (y*8)/baseunitY;				
*/
						sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", x, y);
						ux_fputs0(tempstr, fd);
						cx = rect.Width();
						cy = rect.Height();

/*
.....convert screen pixel to dialog unit
*/
/*						cx = (cx*4)/baseunitX;
						cy = (cy*8)/baseunitY;				
*/
						sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", cx, cy);
						ux_fputs0(tempstr, fd);		
						ux_fputs0("/TYPE/UD_DASINT\n", fd);
						active = ((CNCLFdsnFrame*)UW_Dform_frame)->GetCheckedMacro(i);
						if (active)
							strcpy(tempstr, "/ACTIVE/ YES\n");
						else
							strcpy(tempstr, "/ACTIVE/ NO\n");
						ux_fputs0(tempstr, fd);

						prop = ((CNCLDDButton*)m_macro_win[i])->m_prop_dlg;
						if ((prop!=NULL)&&(prop->m_font!=1))
						{
							sprintf(tempstr, "/FONT/ %f\n", prop->m_font);
							ux_fputs0(tempstr, fd);					
						}				
						if (prop!=NULL)
							len = prop->m_color.GetLength();
						else
							len = 0;
						if (len>0)
						{
							if (S_IsDefaultColor(prop->m_color)==0)
							{
								sprintf(tempstr, "/COLOR/ %s\n", prop->m_color);
								ux_fputs0(tempstr, fd);
							}
						}
					}
				}
				if ((m_mwin_no<=0)&&(m_macro_flag))
/*
......we need add in auto field for macro form
*/
				{
					ux_fputs0("\n#MACRO#\n", fd);

					ux_fputs0("\n#CHECKBOX#\n", fd);
					strcpy(tempstr, "/LABEL/ Place in Source\n");
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", 50, 50);
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", 100, 10);
					ux_fputs0(tempstr, fd);		
					ux_fputs0("/TYPE/UD_DASINT\n", fd);
					ux_fputs0("/ACTIVE/ YES\n", fd);

					ux_fputs0("\n#CHECKBOX#\n", fd);
					strcpy(tempstr, "/LABEL/ Remember Input Values\n");
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", 50, 50);
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", 100, 10);
					ux_fputs0(tempstr, fd);		
					ux_fputs0("/TYPE/UD_DASINT\n", fd);
					ux_fputs0("/ACTIVE/ YES\n", fd);

					ux_fputs0("\n#CHECKBOX#\n", fd);
					strcpy(tempstr, "/LABEL/ Output Default Values\n");
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", 50, 50);
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", 100, 10);
					ux_fputs0(tempstr, fd);		
					ux_fputs0("/TYPE/UD_DASINT\n", fd);
					ux_fputs0("/ACTIVE/ YES\n", fd);

					ux_fputs0("\n#PUSHBUTTON#\n", fd);
					strcpy(tempstr, "/LABEL/ View\n");
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", 50, 50);
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", 100, 10);
					ux_fputs0(tempstr, fd);		
					ux_fputs0("/TYPE/UD_DASINT\n", fd);
					ux_fputs0("/ACTIVE/ YES\n", fd);

					ux_fputs0("\n#IMGBUTTON#\n", fd);
					strcpy(tempstr, "/LABEL/ \n");
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", 50, 50);
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", 100, 10);
					ux_fputs0(tempstr, fd);		
					ux_fputs0("/TYPE/UD_DASSTRING\n", fd);
					ux_fputs0("/ACTIVE/ YES\n", fd);


					ux_fputs0("\n#PUSHBUTTON#\n", fd);
					strcpy(tempstr, "/LABEL/ Dynamic View\n");
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/POSITION/ %d,%d\n", 50, 50);
					ux_fputs0(tempstr, fd);
					sprintf_s(tempstr, sizeof(tempstr), "/SIZE/ %d,%d\n", 100, 10);
					ux_fputs0(tempstr, fd);		
					ux_fputs0("/TYPE/UD_DASINT\n", fd);
					ux_fputs0("/ACTIVE/ YES\n", fd);
				}
				goto help_sec;
			}
			curpage++;
			if (frmtype!=0)
				goto help_sec;
			goto start_sec;
		}	
help_sec:;
		if (helpstr.GetLength()!=0)
		{
			ux_fputs0("\n#HELP#\n", fd);
			hlpstr = helpstr.GetBuffer(100000); 
			ux_fputs0(hlpstr, fd);
		}
	}
done:;
	ux_close(forms, UX_PRTERRS);
	return status;
}

/***********************************************************************
**
**   FUNCTION: OpenPropertyPage(CNCLFormProp *prop_dlg, int flag)
**
**		open current property page window and initial the value with input prop_dlg
**		structure
**	 INPUT:  prop_dlg: property page data structure
**			flag: when = 1, only reset label, font and active value
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::OpenPropertyPage(CNCLFormProp *prop_dlg, int flag)
{
/*
.....show the picture area in the picture item if any
*/
	int i, indx, item=-1;
	indx = prop_dlg->m_pic_act_area;
	if ((indx==-1)||(prop_dlg->m_picarea_no==0))
		goto open;
	if (prop_dlg->m_picarea[indx].name[0]!='\0')
	{
/*
.....find the picture item with same name, then show the picture area there
*/
		for (i=0; i<m_picno;i++)
		{
			if (m_picture[i]==NULL)
				continue;
			if (stricmp(prop_dlg->m_picarea[indx].name, ((CNCLDDPicWin*)(m_picture[i]))->m_name)==0)
			{
				item = i;
				break;
			}
		}
		if (item<0)
			return;
/*
......reset the old active picture area
*/
		if ((m_current_pic!=NULL)&&(m_current_pic!=m_picture[item]))
			((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
		m_current_pic = m_picture[item];
		float pic_rect[4], rect_float[4];
		int pic_indx = prop_dlg->m_pic_act_area;
		pic_rect[0] = prop_dlg->m_picarea[pic_indx].xmin;
		pic_rect[2] = prop_dlg->m_picarea[pic_indx].xmax;
		pic_rect[1] = prop_dlg->m_picarea[pic_indx].ymin;
		pic_rect[3] = prop_dlg->m_picarea[pic_indx].ymax;
		Convert_pic_rect(pic_rect, rect_float, 1);
		CRect rect, rect2;
		rect2.left = rect_float[0];
		rect2.top = rect_float[1];
		rect2.right = rect_float[2];
		rect2.bottom = rect_float[3];
		rect = rect2;
		((CNCLDDPicWin*)m_picture[item])->SetHotSpotRect(rect2);
		m_picture[item]->ClientToScreen(&rect);
		ScreenToClient(rect);
		CreateHotSpotRect(rect);
		InvalidateRect(rect);
		UpdateWindow();
		m_parent->ScreenToClient(rect);
		m_parent->InvalidateRect(rect);
		m_parent->UpdateWindow();
	}
open:;
	((CNCLFormView*)m_parent)->OpenPropertyPage(prop_dlg, flag);
}

void CNCLDDform::SetPicAreaFromProp(CNCLFormProp *prop)
{
	int i, item = -1, indx=prop->m_pic_act_area;
	if ((indx<0)||(prop->m_picarea_no==0))
		return;
	if (prop->m_picarea[indx].name[0]!='\0')
	{
/*
.....find the picture item with same name, then show the picture area there
*/
		for (i=0; i<m_picno;i++)
		{
			if (m_picture[i]==NULL)
				continue;
			if (stricmp(prop->m_picarea[indx].name, ((CNCLDDPicWin*)(m_picture[i]))->m_name)==0)
			{
				item = i;
				break;
			}
		}
		if (item==-1)
			return;
/*
......reset the old active picture area
*/
		if ((m_current_pic!=NULL)&&(m_current_pic!=m_picture[item]))
			((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
		m_current_pic = m_picture[item];
		CRect rect, rect2;
		float pic_rect[4], rect_float[4];
		int pic_indx = prop->m_pic_act_area;
		pic_rect[0] = prop->m_picarea[pic_indx].xmin;
		pic_rect[2] = prop->m_picarea[pic_indx].xmax;
		pic_rect[1] = prop->m_picarea[pic_indx].ymin;
		pic_rect[3] = prop->m_picarea[pic_indx].ymax;
		Convert_pic_rect(pic_rect, rect_float, 1);
		rect2.left = rect_float[0];
		rect2.top = rect_float[1];
		rect2.right = rect_float[2];
		rect2.bottom = rect_float[3];
		rect = rect2;
		((CNCLDDPicWin*)m_picture[item])->SetHotSpotRect(rect2);
		m_picture[item]->ClientToScreen(&rect);
		ScreenToClient(rect);
		CreateHotSpotRect(rect);
		InvalidateRect(rect);
		UpdateWindow();
		m_parent->ScreenToClient(rect);
		m_parent->InvalidateRect(rect);
		m_parent->UpdateWindow();
	}
}
/***********************************************************************
**
**   FUNCTION: SaveProperty()
**
**		Save current property page data into current select item property
**		and reset that values into window
**   
**	 INPUT:  None
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLDDform::SaveProperty()
{
	int type;
	int item_no = m_selitem;
	if (m_selitem<0)
		return;
	m_reset_color = 1;
	CNCLFormProp *prop_dlg = ((CNCLFormView*)m_parent)->GetPropertyPage();
/*
......we need update the older item area too
*/
	CRect oldrect, rect;
	int repaint = 0;
	if (prop_dlg->m_dtype==0)
	{
		if (m_disp_win[item_no]!=NULL)
		{
			m_disp_win[item_no]->GetWindowRect(&oldrect);
			((CNCLDDStatic*)(m_disp_win[item_no]))->SetProperty(prop_dlg);
			m_disp_win[m_selitem]->GetWindowRect(&rect);
			m_disp_secpage[item_no] = prop_dlg->m_page;
			if ((m_disp_secpage[item_no]!=m_current_sec)&&(m_disp_secpage[item_no]!=-1))
				m_disp_win[item_no]->ShowWindow(SW_HIDE);
			else
				m_disp_win[item_no]->ShowWindow(SW_SHOW);
			repaint = 1;
		}
	}
	if (prop_dlg->m_dtype==1)
	{
		if (m_frame[item_no]!=NULL)
		{
			m_frame[item_no]->GetWindowRect(&oldrect);
			((CNCLDDGroup*)(m_frame[item_no]))->SetProperty(prop_dlg);
			m_frame[m_selitem]->GetWindowRect(&rect);
			m_frm_secpage[item_no] = prop_dlg->m_page;
			if ((m_frm_secpage[item_no]!=m_current_sec)&&(m_frm_secpage[item_no]!=-1))
				m_frame[item_no]->ShowWindow(SW_HIDE);
			else
				m_frame[item_no]->ShowWindow(SW_SHOW);
			repaint = 1;
		}
	}
	if (prop_dlg->m_dtype==2)
	{
		if (m_picture[item_no]!=NULL)
		{
			m_picture[item_no]->GetWindowRect(&oldrect);
			((CNCLDDPicWin*)(m_picture[item_no]))->SetProperty(prop_dlg);
			m_picture[m_selitem]->GetWindowRect(&rect);
			m_pic_secpage[item_no] = prop_dlg->m_page;
			if ((m_pic_secpage[item_no]!=m_current_sec)&&(m_pic_secpage[item_no]!=-1))
				m_picture[item_no]->ShowWindow(SW_HIDE);
			else
				m_picture[item_no]->ShowWindow(SW_SHOW);
			repaint = 1;
		}
	}
	if (prop_dlg->m_dtype==3)
	{
		if (m_dinpwin[item_no]!=NULL)
		{
			m_dinpwin[item_no]->GetWindowRect(&oldrect);
			if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
				((CNCLDDStatic*)(m_dinpwin[item_no]))->SetProperty(prop_dlg);
			if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
				((CNCLDDButton*)(m_dinpwin[item_no]))->SetProperty(prop_dlg);
			m_dinpwin[m_selitem]->GetWindowRect(&rect);
			m_inp_secpage[item_no] = prop_dlg->m_page;
			if ((m_inp_secpage[item_no]!=m_current_sec)&&(m_inp_secpage[item_no]!=-1))
				m_dinpwin[item_no]->ShowWindow(SW_HIDE);
			else
				m_dinpwin[item_no]->ShowWindow(SW_SHOW);
			repaint = 1;
/*
......we need always set the active value and prompt color
......the same for prompt and input field
*/
			if (m_inpwin[item_no]!=NULL)
			{
				type = 	m_type[m_selitem];
				if ((type==1)||(type==25))
				{
					((CNCLDDButton*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==2)
				{
					((CNCLDDCombo*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDCombo*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==3)
				{
					((CNCLDDEdit*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDEdit*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==5)
				{
					((CNCLDDListBox*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDListBox*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==7)
				{
					((CNCLDDButton*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==8)
				{
					((CNCLDDCombo*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDCombo*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==9)
				{
					((CNCLDDCombo*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDCombo*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==11)
				{
					((CNCLDDEdit*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDEdit*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==13)
				{
					((CNCLDDProcess*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDProcess*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==15)
				{
					((CNCLDDCombo*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDCombo*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==16)
				{
					((CNCLDDEdit*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDEdit*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==17)
				{
					((CNCLDDListCtrl*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDListCtrl*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==18)
				{
					((CNCLDDColorButton*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDColorButton*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==19)
				{
					((CNCLDDListCtrl2*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDListCtrl2*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==23)
				{
					((CNCLDDColorButton*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDColorButton*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==24)
				{
					((CNCLDDSlider*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDSlider*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				m_inp_secpage[item_no] = prop_dlg->m_page;
				if ((m_inp_secpage[item_no]!=m_current_sec)&&(m_inp_secpage[item_no]!=-1))
					m_inpwin[item_no]->ShowWindow(SW_HIDE);
				else
					m_inpwin[item_no]->ShowWindow(SW_SHOW);
			}
		}
	}
	if (prop_dlg->m_dtype==4)
	{
		type = prop_dlg->m_itype;
		if (!((type==5) ||(type == 19)||(type==17)))
		{
/*
......set prompt
*/
			if (m_dinpwin[item_no]!=NULL)
			{
				if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
					((CNCLDDStatic*)(m_dinpwin[item_no]))->SetProperty(prop_dlg, 1);
				if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
					((CNCLDDButton*)(m_dinpwin[item_no]))->SetProperty(prop_dlg, 1);
				m_inp_secpage[item_no] = prop_dlg->m_page;
				if ((m_inp_secpage[item_no]!=m_current_sec)&&(m_inp_secpage[item_no]!=-1))
					m_dinpwin[item_no]->ShowWindow(SW_HIDE);
				else
					m_dinpwin[item_no]->ShowWindow(SW_SHOW);
			}
		}
		if (m_inpwin[item_no]!=NULL)
		{
			m_inpwin[item_no]->GetWindowRect(&oldrect);
			if ((type==1)||(type==25))
			{
				((CNCLDDButton*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==2)
			{
				((CNCLDDCombo*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==3)
			{
				((CNCLDDEdit*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==5)
			{
				((CNCLDDListBox*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==7)
			{
				((CNCLDDButton*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==8)
			{
				((CNCLDDCombo*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==9)
			{
				((CNCLDDCombo*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==11)
			{
				((CNCLDDEdit*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==13)
			{
				((CNCLDDProcess*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==15)
			{
				((CNCLDDCombo*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==16)
			{
				((CNCLDDEdit*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==17)
			{
				((CNCLDDListCtrl*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==18)
			{
				((CNCLDDColorButton*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==19)
			{
				((CNCLDDListCtrl2*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==23)
			{
				((CNCLDDColorButton*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==24)
			{
				((CNCLDDSlider*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			m_inpwin[m_selitem]->GetWindowRect(&rect);
			m_inp_secpage[item_no] = prop_dlg->m_page;
			if ((m_inp_secpage[item_no]!=m_current_sec)&&(m_inp_secpage[item_no]!=-1))
				m_inpwin[item_no]->ShowWindow(SW_HIDE);
			else
				m_inpwin[item_no]->ShowWindow(SW_SHOW);
			repaint = 1;
/*
...... update the picture area
*/
//			if ((prop_dlg->m_pic_label!="")&&(prop_dlg->m_pic_rect.left!=-1000))
			if (prop_dlg->GetActivePicArea()>=0)
			{
				SetPicAreaFromProp(prop_dlg);
				UpdatePropertyPicture(prop_dlg);
			}
			else
			{
				if (m_current_pic!=NULL)
				{
					((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
					m_current_pic = NULL;
					UpdatePropertyPicture(prop_dlg);
				}
			}
		}
	}
	if (prop_dlg->m_dtype==5)
	{
		if (m_macro_win[item_no]!=NULL)
		{
			m_macro_win[item_no]->GetWindowRect(&oldrect);
			((CNCLDDButton*)(m_macro_win[item_no]))->SetProperty(prop_dlg);
			m_macro_win[m_selitem]->GetWindowRect(&rect);
			repaint = 1;
		}
	}
	if (repaint)
	{
		ScreenToClient(&rect);
		Reset_selrec(rect); 
		oldrect.left = oldrect.left - BARLEN - 2;
		oldrect.right = oldrect.right + BARLEN + 2;
		oldrect.top = oldrect.top - BARLEN - 2;
		oldrect.bottom = oldrect.bottom + BARLEN + 2;
		m_select_draw = 0;
		updateWindow_draw(2, &oldrect);
		m_select_draw = 1;
		updateWindow_draw(1);
	}
	m_reset_color = 0;
}
/***********************************************************************
**
**   FUNCTION: is_valid_area(CRect rect, CPoint point)
**
**		this function will check if the point is inside a valid area 'rect'
**		when check for groupbox, not the whole box	
**   
**	 INPUT:  rect: area to be checked
**			point: cursor current point
**
**   OUTPUT :   none
**   RETURN:    return 1 if it is valid
**				return 0 if it is not valid
**
**********************************************************************/
int CNCLDDform::is_valid_area(CRect rect, CPoint point)
{
	CRect rect1, rect2, rect3, rect4;
	if (m_seldtyp!=1)
	{
		return rect.PtInRect(point);
	}
	rect1.top = rect.top;
	rect1.bottom = rect1.top + 8;
	rect1.left = rect.left;
	rect1.right = rect.right;

	rect2.top = rect.bottom-4;
	rect2.bottom = rect2.top + 8;
	rect2.left = rect.left;
	rect2.right = rect.right;

	rect3.top = rect.top;
	rect3.bottom = rect.bottom;
	rect3.left = rect.left - 4;
	rect3.right = rect3.left + 8;

	rect4.top = rect.top;
	rect4.bottom = rect.bottom;
	rect4.left = rect.right - 4;
	rect4.right = rect4.left + 8;

	if (rect1.PtInRect(point))
	{
		return 1;
	}
	if (rect2.PtInRect(point))
	{
		return 1;
	}
	if (rect3.PtInRect(point))
	{
		return 1;
	}
	if (rect4.PtInRect(point))
	{
		return 0;
	}
	return 0;
}

/***********************************************************************
**
**   FUNCTION: is_frame_clicked(CPoint point)
**
**		this function will check if the point is on the CNCLDDFrame window
**			on this form window
**   
**	 INPUT:  
**			point: cursor current point
**
**   OUTPUT :   none
**   RETURN:    return 1 if it is clicked
**				return 0 if it is not clicked
**
**********************************************************************/
int CNCLDDform::is_frame_clicked(CPoint point)
{
	if (m_frameno<=0)
		return 0;
	CRect rect, rect1, rect2, rect3, rect4;
	for (int i=0; i<m_frameno; i++)
	{
		((CNCLDDGroup*)m_frame[i])->GetWindowRect(&rect);
		ScreenToClient(&rect);
/*
.....we can't check for the whole group box, that is too big
.....only event on the edge of the 4 border of box will be count
*/
		rect1.top = rect.top;
		rect1.bottom = rect1.top + 8;
		rect1.left = rect.left;
		rect1.right = rect.right;

		rect2.top = rect.bottom-4;
		rect2.bottom = rect2.top + 8;
		rect2.left = rect.left;
		rect2.right = rect.right;

		rect3.top = rect.top;
		rect3.bottom = rect.bottom;
		rect3.left = rect.left - 4;
		rect3.right = rect3.left + 8;

		rect4.top = rect.top;
		rect4.bottom = rect.bottom;
		rect4.left = rect.right - 4;
		rect4.right = rect4.left + 8;

		if (rect1.PtInRect(point))
		{
			return 1;
		}
		if (rect2.PtInRect(point))
		{
			return 1;
		}
		if (rect3.PtInRect(point))
		{
			return 1;
		}
		if (rect4.PtInRect(point))
		{
			return 1;
		}
	}
	return 0;
}
/***********************************************************************
**
**   FUNCTION: Handle_frame_event(int evt, UINT nFlags, CPoint point)
**
**		this function will handle the event happened in CNCLDDFrame class
**		since the GroupBox itself can't accept any event.
**   
**	 INPUT:  evt: event ID
**			nFlags: event flag
**			point: cursor current point
**
**   OUTPUT :   none
**   RETURN:    return 0 if handled
**
**********************************************************************/
int CNCLDDform::Handle_frame_event(int evt, UINT nFlags, CPoint point)
{
	CRect rect, rect1, rect2, rect3, rect4;
	int ret;
	if (m_frameno<=0)
		return 1;
	int range = 8;
	if (evt==5)
		range = 12;
	for (int i=0; i<m_frameno; i++)
	{
		((CNCLDDGroup*)m_frame[i])->GetWindowRect(&rect);
		ScreenToClient(&rect);
/*
.....we can't check for the whole group box, that is too big
.....only event on the edge of the 4 border of box will be count
*/
		rect1.top = rect.top - (range-8);
		rect1.bottom = rect1.top + 2*(range-8) + 12;
		rect1.left = rect.left;
		rect1.right = rect.right;

		rect2.top = rect.bottom - (range-4);
		rect2.bottom = rect2.top + 2*(range-4);
		rect2.left = rect.left;
		rect2.right = rect.right;

		rect3.top = rect.top;
		rect3.bottom = rect.bottom;
		rect3.left = rect.left - (range-4);
		rect3.right = rect3.left + 2*(range-4);

		rect4.top = rect.top;
		rect4.bottom = rect.bottom;
		rect4.left = rect.right - (range-4);
		rect4.right = rect4.left + 2*(range-4);

		if (rect1.PtInRect(point))
		{
			((CNCLDDGroup*)m_frame[i])->Handle_frame_event(evt, nFlags, point);
			return 0;
		}
		if (rect2.PtInRect(point))
		{
			((CNCLDDGroup*)m_frame[i])->Handle_frame_event(evt, nFlags, point);
			return 0;
		}
		if (rect3.PtInRect(point))
		{
			((CNCLDDGroup*)m_frame[i])->Handle_frame_event(evt, nFlags, point);
			return 0;
		}
		if (rect4.PtInRect(point))
		{
			((CNCLDDGroup*)m_frame[i])->Handle_frame_event(evt, nFlags, point);
			return 0;
		}
	}
	return 1;
}


int CNCLDDform::CheckMacroForm(char *err)
{
	if (m_macro_flag==0)
		return 0;
	int i, macnum, indx;
	char label[NCL_MAX_LABEL+15], dlabel[NCL_MAX_LABEL+15];
	UM_int2 clas, geoflag, prsvalue, lenvalue;
	char prmptr[41], wrdlist[481];
	int counter, substr, pflag, psel, pclr;
	UM_real8 min, max,dvalue;
	UM_int2 iflg, detype, pclas;
	UM_int4 dsub, psub;
	CNCLFormProp *prop;

	ncl_getmac_pnum(&macnum);
	if (m_inputno<macnum)
	{		
		indx = m_inputno;
		ncl_getmac_parms(&indx, label, &clas, prmptr, &counter, wrdlist,
			&geoflag, &min, &max, 
			&prsvalue, &lenvalue, &substr, &pflag, &psel, &pclr);
		sprintf(err, "Macro parameters %s must be assigned to a field.", label);
		return -1;
	}
	if (m_inputno>macnum)
	{
		strcpy(err, "The form has more fields than the Macro parameters.");
		return -1;
	}
/*
.....now we need check the form item type match with Macro parameter type
*/
	for(i=0; i<macnum;i++)
	{
		indx = i + 1;
		ncl_getmac_parms(&indx, label, &clas, prmptr, &counter, wrdlist,
			&geoflag, &min, &max, 
			&prsvalue, &lenvalue, &substr, &pflag, &psel, &pclr);
		ncl_getmac_values(&indx, &iflg, &psub, dlabel, &dsub, &dvalue, &pclas, &detype);			
		if ((pclas == 0) || ((pclas>0) && geoflag)) 
		{
/*
......String type #EDIT# or #CHOICEBOX
*/
			if ((m_type[i]!=3)&&(m_type[i]!=2)&&(m_type[i]!=7)&&(m_type[i]!=23))
			{
				sprintf(err, "Macro parameters %s must be assigned to a #EDIT#, #SELECT#, #CHOICEBOX# or #CHECKBOX# field", label);
				return -1;
			}
		}
		if (pclas == -1)
		{
/* 
...String  & min & max or String with default scalar value
*/ 
			if (min == 0 && max == 0)
			{
				if (m_type[i]!=7)
				{
					sprintf(err, "Macro parameters %s must be assigned to a #CHECKBOX# type", label);
					return -1;
				}
			}
			else
			{
/*
......String type #EDIT#
*/
				if (m_type[i]!=3)
				{
					sprintf(err, "Macro parameters %s must be assigned to a #EDIT# field with UD_DASUNITLESS type", label);
					return -1;
				}
				prop = ((CNCLDDEdit*)m_inpwin[i])->m_prop_dlg;
				if (prop->m_type!=UD_DASUNITLESS)
				{
					sprintf(err, "Macro parameters %s must be assigned to a #EDIT# field with UD_DASUNITLESS type", label);
					return -1;
				}
			}
		}
		if ((pclas > 0) && (geoflag==0))
		{
/* 
...String  and vocabulary words #CHOICEBOX#
*/  
			if (m_type[i]!=2)
			{
				sprintf(err, "Macro parameters %s must be assigned to a #CHOICEBOX# type", label);
				return -1;
			}
		}
	}
	return 0;
}

void CNCLDDform::UpdatePropertyPos(int x, int y)
{
	CNCLFormProp *prop_dlg = ((CNCLFormView*)m_parent)->GetPropertyPage();
	prop_dlg->m_pos[0] = x;
	prop_dlg->m_pos[1] = y;
	((CNCLFormView*)m_parent)->UpdatePropertyPos(x, y);
}

void CNCLDDform::UpdatePropertySize(int cx, int cy)
{
	CNCLFormProp *prop_dlg = ((CNCLFormView*)m_parent)->GetPropertyPage();
	prop_dlg->m_size[0] = cx;
	prop_dlg->m_size[1] = cy;
	((CNCLFormView*)m_parent)->UpdatePropertySize(cx, cy);
}

void CNCLDDform::UpdatePropertyPicture(CNCLFormProp *prop_dlg)
{
	int indx = prop_dlg->m_pic_act_area;
	if ((indx<0)||(prop_dlg->m_picarea_no==0))
		return;
	CString pic_label, pic_params, pic_tooltip;
	float pic_rect[4];
	pic_label = prop_dlg->m_picarea[indx].name;
	pic_params = prop_dlg->m_picarea[indx].params;
	pic_tooltip = prop_dlg->m_picarea[indx].tooltext;
	pic_rect[0] = prop_dlg->m_picarea[indx].xmin;
	pic_rect[2] = prop_dlg->m_picarea[indx].xmax;
	pic_rect[1] = prop_dlg->m_picarea[indx].ymin;
	pic_rect[3] = prop_dlg->m_picarea[indx].ymax;

	((CNCLFdsnFrame*)UW_Dform_frame)->UpdatePropertyPicture(
				pic_label, pic_tooltip, 
				pic_rect, pic_params, indx);
}

int CNCLDDform::IsSelItem(int itemno, int type, int itype)
{
	if ((m_selitem==itemno)&&(m_seldtyp==type))
		return 1;
	else
		return 0;
}

int CNCLDDform::CheckPos(int &x, int &y)
{
	int type;
	int item_no = m_selitem;
	if (m_selitem<0)
		return -1;
	POINT pt;
	pt.x = x;
	pt.y = y;
	CRect frect;
	GetClientRect(&frect);

	CNCLFormProp *prop_dlg = ((CNCLFormView*)m_parent)->GetPropertyPage();
	CRect oldrect, rect;
	if (prop_dlg->m_dtype==0)
	{
		if (m_disp_win[item_no]!=NULL)
		{
			m_disp_win[item_no]->GetClientRect(&oldrect);
		}
	}
	if (prop_dlg->m_dtype==1)
	{
		if (m_frame[item_no]!=NULL)
		{
			m_frame[item_no]->GetClientRect(&oldrect);
		}
	}
	if (prop_dlg->m_dtype==2)
	{
		if (m_picture[item_no]!=NULL)
		{
			m_picture[item_no]->GetClientRect(&oldrect);
		}
	}
	if (prop_dlg->m_dtype==3)
	{
		if (m_dinpwin[item_no]!=NULL)
		{
			m_dinpwin[item_no]->GetClientRect(&oldrect);
		}
	}
	if (prop_dlg->m_dtype==4)
	{
		if (m_inpwin[item_no]!=NULL)
		{
			m_inpwin[item_no]->GetClientRect(&oldrect);
		}
	}
	if (prop_dlg->m_dtype==5)
	{
		if (m_macro_win[item_no]!=NULL)
		{
			m_macro_win[item_no]->GetClientRect(&oldrect);
		}
	}
	int left = frect.left - oldrect.Width() + 10;
	int right = frect.right - 10;
	int top = frect.top - oldrect.Height() + 10;
	int bottom = frect.bottom - 10;
	if ((x>left)&&(x<right)&&(y>top)&&(y<bottom))
		return 1;
/*
.....adject (x,y) to maxinum allow position
*/
	if (x<left)
		x = left;
	if (x>right)
		x = right;
	if (y<top)
		y = top;
	if (y>bottom)
		y = bottom;
	return 0;
}
void CNCLDDform::OnSecButton(int page)
{
	UINT fid;
/*
......reset all selections
*/
	reset_selvalue();

	int CmdShow;
	for (int i=0; i<m_dispno; i++)
	{
		if ((m_disp_secpage[i]==page)||(m_disp_secpage[i]==-1))
		{
			CmdShow = SW_SHOW;
		}
		else
			CmdShow = SW_HIDE;
		m_disp_win[i]->ShowWindow(CmdShow);
	}	
	for (int i=0; i<m_frameno; i++)
	{
		if ((m_frm_secpage[i]==page)||(m_frm_secpage[i]==-1))
		{
			CmdShow = SW_SHOW;
		}
		else
			CmdShow = SW_HIDE;
		m_frame[i]->ShowWindow(CmdShow);
	}
	for (int i=0; i<m_picno; i++)
	{
		if ((m_pic_secpage[i]==page)||(m_pic_secpage[i]==-1))
		{
			CmdShow = SW_SHOW;
		}
		else
			CmdShow = SW_HIDE;
		m_picture[i]->ShowWindow(CmdShow);
	}
	for (int i=0; i<m_inputno; i++)
	{
		if ((m_inp_secpage[i]==page)||(m_inp_secpage[i]==-1))
		{
			m_inpwin[i]->ShowWindow(SW_SHOW);
			if (m_dinpwin[i] != NULL)
				m_dinpwin[i]->ShowWindow(SW_SHOW);
		}
		else
		{
			m_inpwin[i]->ShowWindow(SW_HIDE);
			if (m_dinpwin[i] != NULL)
				m_dinpwin[i]->ShowWindow(SW_HIDE);
		}
	}
	if ((page==50)||(page==-1))
		m_current_sec = -1;
	else
		m_current_sec = page;
}

int CNCLDDform::isallsec()
{
	for (int i=0; i<m_dispno; i++)
	{
		if ((m_disp_secpage[i]==-1)&&(m_disp_win[i]!=NULL))
		{
			return 1;
		}
	}	
	for (int i=0; i<m_frameno; i++)
	{
		if ((m_frm_secpage[i]==-1)&&(m_frame[i]!=NULL))
		{
			return 1;
		}
	}
	for (int i=0; i<m_picno; i++)
	{
		if ((m_pic_secpage[i]==-1)&&(m_picture[i]!=NULL))
		{
			return 1;
		}
	}
	for (int i=0; i<m_inputno; i++)
	{
		if ((m_inp_secpage[i]==-1)&&(m_inpwin[i]!=NULL))
		{
			return 1;
		}
	}
	return 0;
}
//indx of section button, not page number
void CNCLDDform::DeleteSec(int indx)
{
/*
......reset redo list and
......add this action into undo stack
*/
	Remove_redoList();
/*
.....first, save all undo list before delete anything
.....m_action = 21 section delete
.....when undo it will insert section first, the add all items back
*/
	m_undo_num = 0;
	int number = 0, dispno = 0, frameno = 0, picno = 0, inputno=0;
	for (int i=0; i<m_dispno; i++)
	{
		if ((m_disp_secpage[i]==indx)&&(m_disp_win[i]!=NULL))
		{
			m_action_item = i;
			m_action_typ = 0;
			number = SaveUndoItem();
			if (number>0)
				dispno++;
			m_undo_num += number;
		}
	}	
	for (int i=0; i<m_frameno; i++)
	{
		if ((m_frm_secpage[i]==indx)&&(m_frame[i]!=NULL))
		{
			m_action_item = i;
			m_action_typ = 1;
			number = SaveUndoItem();
			if (number>0)
				frameno++;
			m_undo_num += number;
		}
	}
	for (int i=0; i<m_picno; i++)
	{
		if ((m_pic_secpage[i]==indx)&&(m_picture[i]!=NULL))
		{
			m_action_item = i;
			m_action_typ = 2;
			number = SaveUndoItem();
			if (number>0)
				picno++;
			m_undo_num += number;
		}
	}
	for (int i=0; i<m_inputno; i++)
	{
		if ((m_inp_secpage[i]==indx)&&(m_inpwin[i]!=NULL))
		{
			m_action_item = i;
			m_action_typ = 4;
			number = SaveUndoItem();
			if (number>0)
				inputno++;
			m_undo_num += number;
		}
	}
	m_action = 21;
	SaveUndoNumber(m_undo_num, frameno, picno, dispno, inputno, indx, 
			-1, -1, -1);
	if (indx==50)
		indx = -1;
	for (int i=0; i<m_dispno; i++)
	{
		if ((m_disp_secpage[i]==indx)&&(m_disp_win[i]!=NULL))
		{
			delete m_disp_win[i];
			m_disp_win[i] = NULL;
		}
	}	
	for (int i=0; i<m_frameno; i++)
	{
		if ((m_frm_secpage[i]==indx)&&(m_frame[i]!=NULL))
		{
			delete m_frame[i];
			m_frame[i] = NULL;
		}
	}
	for (int i=0; i<m_picno; i++)
	{
		if ((m_pic_secpage[i]==indx)&&(m_picture[i]!=NULL))
		{
			delete m_picture[i];
			m_picture[i] = NULL;
		}
	}
	for (int i=0; i<m_inputno; i++)
	{
		if ((m_inp_secpage[i]==indx)&&(m_inpwin[i]!=NULL))
		{
			if (m_dinpwin[i]!=NULL)
			{
				delete m_dinpwin[i];
				m_dinpwin[i] = NULL;
			}
			if (m_inpwin[i]!=NULL)
			{
				delete m_inpwin[i];
				m_inpwin[i] = NULL;
			}
		}
	}
	Re_arrange_item();
/*	
	m_selitem = -1;
	m_selwin = NULL;
	m_sizedir = 0;
	m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
	m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
	
	for (int i=0; i<8; i++)
	{
		m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
	}
*/
	reset_selvalue();
done:;
	updateWindow_draw(1);
	if (m_selnum<=1)
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(0);
	}
	else
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(1);
	}
}

void CNCLDDform::ReplaceSecNo(int old_no, int new_no)
{
	for (int i=0; i<m_dispno; i++)
	{
		if ((m_disp_secpage[i]==old_no)&&(m_disp_win[i]!=NULL))
		{
			m_disp_secpage[i] = new_no;
		}
	}	
	for (int i=0; i<m_frameno; i++)
	{
		if ((m_frm_secpage[i]==old_no)&&(m_frame[i]!=NULL))
		{
			m_frm_secpage[i] = new_no;
		}
	}
	for (int i=0; i<m_picno; i++)
	{
		if ((m_pic_secpage[i]==old_no)&&(m_picture[i]!=NULL))
		{
			m_pic_secpage[i] = new_no;
		}
	}
	for (int i=0; i<m_inputno; i++)
	{
		if ((m_inp_secpage[i]==old_no)&&(m_inpwin[i]!=NULL))
		{
			m_inp_secpage[i] = new_no;
		}
	}
}
void CNCLDDform::reset_selvalue()
{
	m_selitem = -1;
	m_selnum = 0;
	m_selwin = NULL;
	m_sizedir = 0;
	m_selrec.left = m_selrec.top = m_selrec.right = m_selrec.bottom = -1000;
	m_sizerec.left = m_sizerec.top = m_sizerec.right = m_sizerec.bottom = -1000;
	
	for (int i=0; i<8; i++)
	{
		m_selsize[i].left = m_selsize[i].top = m_selsize[i].right = m_selsize[i].bottom = -1000;
	}
	if (m_current_pic!=NULL)
	{
		((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
		m_current_pic = NULL;
	}

	m_select_draw = 0;
	updateWindow_draw(1);
	m_select_draw = 1;
	CNCLFormPView *pview = (CNCLFormPView *)(((CNCLFdsnFrame*)UW_Dform_frame)->GetPView());
	pview->SetDtype(-2);
	pview->filldata();
}
void CNCLDDform::DisplayAll()
{
/*
......reset all selections
*/
	reset_selvalue();

	for (int i=0; i<m_dispno; i++)
	{
		m_disp_win[i]->ShowWindow(SW_SHOW);
	}	
	for (int i=0; i<m_frameno; i++)
	{
		m_frame[i]->ShowWindow(SW_SHOW);
	}
	for (int i=0; i<m_picno; i++)
	{
		m_picture[i]->ShowWindow(SW_SHOW);
	}
	for (int i=0; i<m_inputno; i++)
	{
		m_inpwin[i]->ShowWindow(SW_SHOW);
		if (m_dinpwin[i] != NULL)
			m_dinpwin[i]->ShowWindow(SW_SHOW);
	}
}

//secnum: page number, not index number page=indx-sepno
void CNCLDDform::OnDragDropSection(int secnum, char *drop_text)
{
	char *tok, datastr[100],class_str[40];
	int selityp = -1, auto_flag = 0;
	int total=0, number = 0;

	strcpy(datastr, drop_text);

	if (m_selnum>1)
		m_multi_drag = 1;

	if (secnum==50)
		secnum = -1;
	
	S_skip_reset();

	tok = strtok(datastr, " \t\r\n");
	if (strcmp(tok, "Move")!=0)
		return;

	if (strcmp(tok, "Move")==0)
	{
		m_reset_redo = 1;
		Remove_redoList();
		tok = strtok(NULL, " \t\r\n");
		strcpy(class_str, tok);
		int display = 0;
		if (strcmp(tok, "CNCLDDStatic")==0)
		{
			display = 1;
		}
		int pic = 0;
		if (strcmp(tok, "CNCLDDPicWin")==0)
		{
			pic = 1;
		}
		int frame = 0;
		if (strcmp(tok, "CNCLDDGroup")==0)
		{
			frame = 1;
		}
		if (strcmp(tok, "CNCLDDButton2")==0)
		{
			auto_flag = 1;
		}
		int combo = 0;
		int itemno;
		tok = strtok(NULL, " \t\r\n");
		if (tok!=NULL)
		{
			itemno = atoi(tok);
/*
.....so move window from pt_from to pt
*/
			int move_item = m_selnum;
			int indx = 0;
/*
......it could be a button prompt
*/
			if (strcmp(class_str, "CNCLDDButton")==0)
			{
				if ((m_dinpwin[itemno]!=NULL)&&(m_dinpwin[itemno]->IsKindOf(RUNTIME_CLASS(CNCLDDButton))))
				{
					if (((CNCLDDButton*)(m_dinpwin[itemno]))->get_prompt())
						display = 1;
				}
			}
moveit:;
			if (move_item<=0)
				goto done;
/*
.....before move, remove all selection draw,
*/
			m_select_draw = 0;
			updateWindow_draw(1);
			m_select_draw = 1;
			if ((pic==1)&&((CNCLDDPicWin*)m_picture[itemno]!=NULL))
			{
				number = SaveUndoSecItem(2, itemno);
				m_pic_secpage[itemno] = secnum;
				goto again;
			}
			if ((frame==1)&&((CNCLDDGroup*)m_frame[itemno]!=NULL))
			{
				number = SaveUndoSecItem(1, itemno);
				m_frm_secpage[itemno] = secnum;
				goto again;
			}
			if ((auto_flag)&&(m_macro_win[itemno]!=NULL))
			{
// auto type always on ALL page for now 
				goto again;
			}
			if (display==0)
			{
/*
.....only save once
*/
				if (S_skip_next(itemno)==0)
				{
					number = SaveUndoSecItem(4, itemno);
					total += number;
					if (number>1)
						S_skip_next_save(itemno);
				}
				m_inp_secpage[itemno] = secnum;
				goto again;
			}
			else
			{
				int prompt = 0;
				if (m_dinpwin[itemno]!=NULL)
				{
					if (m_dinpwin[itemno]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
					{
						if (((CNCLDDStatic*)m_dinpwin[itemno])->m_prompt==1)
							prompt = 1;
					}
					if (m_dinpwin[itemno]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
					{
						prompt = 1;
					}
				}
				if (prompt==1)
				{
/*
.....only save once for prompt and input
*/
					if (S_skip_next(itemno)==0)
					{
						number = SaveUndoSecItem(3, itemno);
						total += number;
						if (number>1)
							S_skip_next_save(itemno);
					}
					m_inp_secpage[itemno] = secnum;
					goto again;
				}
				else
				{
					number = SaveUndoSecItem(0, itemno);
					m_disp_secpage[itemno] = secnum;
					goto again;
				}
			}
/*
.....we need check if we need move the other selected item
*/
again:;
			move_item--;
			if (move_item>0)
			{
				if ((m_selarray[indx]==m_selitem)&&(m_seltype[indx]==m_seldtyp))
					indx++;
				if (indx>=m_selnum)
					goto done;
				display = 0;
				if ((m_seltype[indx]==0)||(m_seltype[indx]==3))
					display = 1;
				pic = 0;
				if (m_seltype[indx]==2)
					pic = 1;
				frame = 0;
				if (m_seltype[indx]==1)
					frame = 1;
				auto_flag = 0;
				if (m_seltype[indx]==5)
					auto_flag = 1;
				itemno = m_selarray[indx];
				indx++;
				goto moveit;
			}
		}
		if ((m_seldtyp==4)&&(m_selitem>=0))
			selityp = m_type[m_selitem];
		m_action = 11;
		SaveUndoNumber(total, m_frameno, m_picno, m_dispno, m_inputno, m_current_sec, 
			m_selitem, m_seldtyp, selityp);
	}
done:;
	((CNCLFormView*)m_parent)->OnSectionSelItem(m_current_sec);
/*
......we add follow statement here because the dragging event will eat the buttonup event
*/
	if (m_selnum<=1)
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(0);
	}
	else
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->EnableAlignBtn(1);
	}
	m_buttondown = 0;
	m_resize = 0;
	m_multi_drag = 0;
}
void CNCLDDform::SaveRedoItem(CNCLWinInfo* pItem)
{
	if (m_no_redo==1)
		return;
	CNCLWinInfo* temp;
	POSITION pos = m_pRedoList.GetHeadPosition();
	while (pos!=NULL)
	{
		temp = m_pRedoList.GetNext(pos);
		if (temp==NULL)
			break;
	}			
	m_pRedoList.AddTail(pItem);
}

void CNCLDDform::SaveUndoItemInfo(CNCLWinInfo* pItem)
{
	if (m_no_undo_save==1)
		return;
	CNCLWinInfo* temp;
	POSITION pos = m_pUndoList.GetHeadPosition();
	while (pos!=NULL)
	{
		temp = m_pUndoList.GetNext(pos);
		if (temp==NULL)
			break;
	}			
	m_pUndoList.AddTail(pItem);
}

void CNCLDDform::AddUndo_addSection(int secno)
{
	if (m_no_undo_save==1)
		return;
	CNCLWinInfo *infoItem;
	infoItem = new CNCLWinInfo();
	infoItem->m_dtype = -1;
	infoItem->m_itype = 0;
	infoItem->m_type = m_frameno;
	infoItem->m_len = m_picno;
	infoItem->m_prec = m_dispno;
	infoItem->m_input_itemno = m_inputno;
	infoItem->m_page = secno;
	infoItem->m_action = 20;
/*
.....save selection info
*/
	infoItem->m_active = -1;
	infoItem->m_pos[0] = -1;
	infoItem->m_pos[1] = -1;	
	m_pUndoList.AddTail(infoItem);
	((CNCLFdsnFrame*)UW_Dform_frame)->EnableUndoBtn(1);
}

void CNCLDDform::SetWinPropFromInfo(CNCLWinInfo* pItem, int dispno, int frameno, int picno, int inputno)
{
	CNCLFormProp *prop_dlg;
	int dtype = pItem->m_dtype;
	int type = pItem->m_itype;
	int item_no = pItem->m_input_itemno;
/*
......we need update the older item area too
*/
	CRect oldrect, rect;
	int repaint = 0;
	if (dtype==0)
	{
		if (m_disp_win[item_no]!=NULL)
		{
			m_disp_win[item_no]->GetWindowRect(&oldrect);
			prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_disp_win[item_no]))->GetPropertyPage());
			prop_dlg->SetWinInfo(pItem);
			((CNCLDDStatic*)(m_disp_win[item_no]))->SetProperty(prop_dlg);
			m_disp_win[m_selitem]->GetWindowRect(&rect);
			m_disp_secpage[item_no] = prop_dlg->m_page;
			if ((m_disp_secpage[item_no]!=m_current_sec)&&(m_disp_secpage[item_no]!=-1))
				m_disp_win[item_no]->ShowWindow(SW_HIDE);
			else
				m_disp_win[item_no]->ShowWindow(SW_SHOW);
			repaint = 1;
		}
	}
	if (dtype==1)
	{
		if (m_frame[item_no]!=NULL)
		{
			m_frame[item_no]->GetWindowRect(&oldrect);
			prop_dlg = ((CNCLFormProp *)((CNCLDDGroup*)(m_frame[item_no]))->GetPropertyPage());
			prop_dlg->SetWinInfo(pItem);
			((CNCLDDGroup*)(m_frame[item_no]))->SetProperty(prop_dlg);
			m_frame[m_selitem]->GetWindowRect(&rect);
			m_frm_secpage[item_no] = prop_dlg->m_page;
			if ((m_frm_secpage[item_no]!=m_current_sec)&&(m_frm_secpage[item_no]!=-1))
				m_frame[item_no]->ShowWindow(SW_HIDE);
			else
				m_frame[item_no]->ShowWindow(SW_SHOW);
			repaint = 1;
		}
	}
	if (dtype==2)
	{
		if (m_picture[item_no]!=NULL)
		{
			m_picture[item_no]->GetWindowRect(&oldrect);
			prop_dlg = ((CNCLFormProp *)((CNCLDDPicWin*)(m_picture[item_no]))->GetPropertyPage());
			prop_dlg->SetWinInfo(pItem);		
			((CNCLDDPicWin*)(m_picture[item_no]))->SetProperty(prop_dlg);
			m_picture[item_no]->GetWindowRect(&rect);
			m_pic_secpage[item_no] = prop_dlg->m_page;
			if ((m_pic_secpage[item_no]!=m_current_sec)&&(m_pic_secpage[item_no]!=-1))
				m_picture[item_no]->ShowWindow(SW_HIDE);
			else
				m_picture[item_no]->ShowWindow(SW_SHOW);
			repaint = 1;
		}
	}
	if (dtype==3)
	{
		if (m_dinpwin[item_no]!=NULL)
		{
			m_dinpwin[item_no]->GetWindowRect(&oldrect);
			if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDStatic*)(m_dinpwin[item_no]))->SetProperty(prop_dlg);
			}
			if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDButton*)(m_dinpwin[item_no]))->SetProperty(prop_dlg);
			}
			m_dinpwin[item_no]->GetWindowRect(&rect);
			m_inp_secpage[item_no] = prop_dlg->m_page;
			if ((m_inp_secpage[item_no]!=m_current_sec)&&(m_inp_secpage[item_no]!=-1))
				m_dinpwin[item_no]->ShowWindow(SW_HIDE);
			else
				m_dinpwin[item_no]->ShowWindow(SW_SHOW);
			repaint = 1;
/*
......we need always set the active value and prompt color
......the same for prompt and input field
*/
			if (m_inpwin[item_no]!=NULL)
			{
				type = m_type[item_no];
				if ((type==1)||(type==25))
				{
					((CNCLDDButton*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==2)
				{
					((CNCLDDCombo*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDCombo*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==3)
				{
					((CNCLDDEdit*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDEdit*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==5)
				{
					((CNCLDDListBox*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDListBox*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==7)
				{
					((CNCLDDButton*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==8)
				{
					((CNCLDDCombo*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDCombo*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==9)
				{
					((CNCLDDCombo*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDCombo*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==11)
				{
					((CNCLDDEdit*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDEdit*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==13)
				{
					((CNCLDDProcess*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDProcess*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==15)
				{
					((CNCLDDCombo*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDCombo*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==16)
				{
					((CNCLDDEdit*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDEdit*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==17)
				{
					((CNCLDDListCtrl*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDListCtrl*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==18)
				{
					((CNCLDDColorButton*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDColorButton*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==19)
				{
					((CNCLDDListCtrl2*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDListCtrl2*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==23)
				{
					((CNCLDDColorButton*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDColorButton*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				else if (type==24)
				{
					((CNCLDDSlider*)m_inpwin[item_no])->SetPColorValue(prop_dlg->m_pcolor);
					((CNCLDDSlider*)m_inpwin[item_no])->SetActive(prop_dlg->m_active);
				}
				m_inp_secpage[item_no] = pItem->m_page;
				if ((m_inp_secpage[item_no]!=m_current_sec)&&(m_inp_secpage[item_no]!=-1))
					m_inpwin[item_no]->ShowWindow(SW_HIDE);
				else
					m_inpwin[item_no]->ShowWindow(SW_SHOW);
			}
		}
	}
	if (dtype==4)
	{
		if (m_inpwin[item_no]!=NULL)
		{
			m_inpwin[item_no]->GetWindowRect(&oldrect);
			if ((type==1)||(type==25))
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDButton*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==2)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDCombo*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==3)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDEdit*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==5)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListBox*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDListBox*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==7)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDButton*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==8)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDCombo*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==9)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDCombo*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==11)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDEdit*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==13)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDProcess*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDProcess*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==15)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDCombo*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==16)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDEdit*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==17)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDListCtrl*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==18)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDColorButton*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==19)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl2*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDListCtrl2*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==23)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDColorButton*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			else if (type==24)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDSlider*)(m_inpwin[item_no]))->GetPropertyPage());
				prop_dlg->SetWinInfo(pItem);		
				((CNCLDDSlider*)m_inpwin[item_no])->SetProperty(prop_dlg);
			}
			m_inpwin[item_no]->GetWindowRect(&rect);
			m_inp_secpage[item_no] = prop_dlg->m_page;
			if ((m_inp_secpage[item_no]!=m_current_sec)&&(m_inp_secpage[item_no]!=-1))
				m_inpwin[item_no]->ShowWindow(SW_HIDE);
			else
				m_inpwin[item_no]->ShowWindow(SW_SHOW);
			repaint = 1;
/*
...... update the picture area
*/
//			if ((prop_dlg->m_pic_label!="")&&(prop_dlg->m_pic_rect.left!=-1000))
			if (prop_dlg->GetActivePicArea()>=0)
			{
				SetPicAreaFromProp(prop_dlg);
				UpdatePropertyPicture(prop_dlg);
			}
			else
			{
				if (m_current_pic!=NULL)
				{
					((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
					m_current_pic = NULL;
					UpdatePropertyPicture(prop_dlg);
				}
			}
		}
	}
	if (dtype==5)
	{
		if (m_macro_win[item_no]!=NULL)
		{
			m_macro_win[item_no]->GetWindowRect(&oldrect);
			prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_macro_win[item_no]))->GetPropertyPage());
			prop_dlg->SetWinInfo(pItem);		
			((CNCLDDButton*)(m_macro_win[item_no]))->SetProperty(prop_dlg);
			m_macro_win[item_no]->GetWindowRect(&rect);
			repaint = 1;
		}
	}
	if (repaint)
	{
		ScreenToClient(&rect);
		Reset_selrec(rect); 
		oldrect.left = oldrect.left - BARLEN - 2;
		oldrect.right = oldrect.right + BARLEN + 2;
		oldrect.top = oldrect.top - BARLEN - 2;
		oldrect.bottom = oldrect.bottom + BARLEN + 2;
		m_select_draw = 0;
		updateWindow_draw(2, &oldrect);
		m_select_draw = 1;
		updateWindow_draw(1);
	}
}
int CNCLDDform::SaveUndoSecItem(int dtype, int item_no)
{
	CRect rect;
	CNCLFormProp *prop_dlg, *dprop_dlg;
	CNCLWinInfo *pItem, *iItem;
	int number = 0;
	CNCLWinInfo* temp;

	POSITION pos = m_pUndoList.GetHeadPosition();
	while (pos!=NULL)
	{
		temp = m_pUndoList.GetNext(pos);
		if (temp==NULL)
			break;
	}
	if (dtype==0)
	{
		if (m_disp_win[item_no]!=NULL)
		{
			pItem = new CNCLWinInfo();
			prop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_disp_win[item_no]))->GetPropertyPage());
			prop_dlg->GetWinInfo(pItem);
			pItem->m_action = 11;
/*
......just make sure the current section is saved in the prop_dlg before save
*/
			pItem->m_page = m_current_sec;
			m_pUndoList.AddTail(pItem);
			number++;
		}
	}
	if (dtype==1)
	{
		if (m_frame[item_no]!=NULL)
		{
			pItem = new CNCLWinInfo();
			prop_dlg = ((CNCLFormProp *)((CNCLDDGroup*)(m_frame[item_no]))->GetPropertyPage());
			prop_dlg->GetWinInfo(pItem);
			pItem->m_action = 11;
			pItem->m_page = m_current_sec;
			m_pUndoList.AddTail(pItem);
			number++;
		}
	}
	if (dtype==2)
	{
		if (m_picture[item_no]!=NULL)
		{
			pItem = new CNCLWinInfo();
			prop_dlg = ((CNCLFormProp *)((CNCLDDPicWin*)(m_picture[item_no]))->GetPropertyPage());
			prop_dlg->GetWinInfo(pItem);
			pItem->m_action = 11;
			pItem->m_page = m_current_sec;
			m_pUndoList.AddTail(pItem);
			number++;
		}
	}
	if ((dtype==4)||(dtype==3))
	{
		if (m_inpwin[item_no]!=NULL)
		{
			iItem = new CNCLWinInfo();
			if ((m_type[item_no]==1)||(m_type[item_no]==25))
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==2)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==3)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==5)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListBox*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==7)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==8)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==9)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==11)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==13)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDProcess*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==15)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==16)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==17)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==18)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==19)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl2*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			prop_dlg->GetWinInfo(iItem);
			iItem->m_action = 11;
			iItem->m_page = m_current_sec;
			m_pUndoList.AddTail(iItem);
			number++;
			if (!((m_type[item_no]==5) ||(m_type[item_no] == 19)||(m_type[item_no]==17)))
			{
				if (m_dinpwin[item_no]!=NULL)
				{
					pItem = new CNCLWinInfo();
					if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDStatic)))
						dprop_dlg = ((CNCLFormProp *)((CNCLDDStatic*)(m_dinpwin[item_no]))->GetPropertyPage());
					else if (m_dinpwin[item_no]->IsKindOf(RUNTIME_CLASS(CNCLDDButton)))
						dprop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_dinpwin[item_no]))->GetPropertyPage());
					dprop_dlg->GetWinInfo(pItem);
					pItem->m_action = 11;
					pItem->m_page = m_current_sec;
					m_pUndoList.AddTail(pItem);
					number++;
				}
			}
		}
	}
/*
	if (dtype==5)
	{
		if (m_macro_win[item_no]!=NULL)
		{
			prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_macro_win[item_no]))->GetPropertyPage());
			prop_dlg->m_page = -1;
		}
	}
*/
	return number;
}

int CNCLDDform::SaveUndoInfo(int action, CNCLWinInfo *info1, CNCLWinInfo *info2)
{
	CRect rect;
	CNCLWinInfo *pItem, *pItem2;
	CNCLWinInfo* temp;
	int number = 1;

	if (info1==NULL)
		return 0;
	if (info1->m_dtype!=-1)
	{
		pItem = new CNCLWinInfo();
		pItem->CopyInfo(info1);
		pItem->m_action = action;
		pItem->m_page = m_current_sec;
	}
/*	POSITION pos = m_pUndoList.GetHeadPosition();
	while (pos!=NULL)
	{
		temp = m_pUndoList.GetNext(pos);
		if (temp==NULL)
			break;
	}
*/
	if ((info1->m_dtype==0)||(info1->m_dtype==1)||(info1->m_dtype==2))
	{
//		m_pUndoList.AddTail(pItem);
		SaveUndoItemInfo(pItem);
		return 1;
	}
	if (((info1->m_dtype==3)||(info1->m_dtype==4)))
	{
		if (info1->m_dtype==3)
		{
			pItem2 = new CNCLWinInfo();
			pItem2->CopyInfo(info2);
			pItem2->m_action = action;
			pItem2->m_page = m_current_sec;
			SaveUndoItemInfo(pItem2);
			number = 2;
		}
		SaveUndoItemInfo(pItem);
	}
	else if ((info1->m_dtype==-1)&&(info2->m_dtype==4))
	{
		pItem2 = new CNCLWinInfo();
		pItem2->CopyInfo(info2);
		pItem2->m_action = action;
		pItem2->m_page = m_current_sec;
		SaveUndoItemInfo(pItem2);
		number = 1;
	}
	return number;
}

void CNCLDDform::LoadActiveHotSpot(int dtype, int itype, int item_no, int hpnum)
{
	CNCLFormProp *prop_dlg=NULL;
	if (dtype==4)
	{
		if (m_inpwin[item_no]!=NULL)
		{
			if ((m_type[item_no]==1)||(m_type[item_no]==25))
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==2)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==3)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==5)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListBox*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==7)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==8)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==9)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==11)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==15)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDCombo*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==16)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDEdit*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==17)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==18)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDColorButton*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			else if (m_type[item_no]==19)
			{
				prop_dlg = ((CNCLFormProp *)((CNCLDDListCtrl2*)(m_inpwin[item_no]))->GetPropertyPage());
			}
			int item=-1;
			if ((prop_dlg->m_picarea_no!=0)&&
				(prop_dlg->m_picarea[hpnum].name[0]!='\0'))
			{
/*
.....find the picture item with same name, then show the picture area there
*/
				for (int i=0; i<m_picno;i++)
				{
					if (m_picture[i]==NULL)
						continue;
					if (stricmp(prop_dlg->m_picarea[hpnum].name, ((CNCLDDPicWin*)(m_picture[i]))->m_name)==0)
					{
						item = i;
						break;
					}
				}
				if (item<0)
					return;
/*
......reset the old active picture area
*/
//				if ((m_current_pic!=NULL)&&(m_current_pic!=m_picture[item]))
					((CNCLDDPicWin*)m_current_pic)->Reset_picture_area();
				m_current_pic = m_picture[item];

				float pic_rect[4], rect_float[4];
				int pic_indx = prop_dlg->m_pic_act_area = hpnum;
				pic_rect[0] = prop_dlg->m_picarea[pic_indx].xmin;
				pic_rect[2] = prop_dlg->m_picarea[pic_indx].xmax;
				pic_rect[1] = prop_dlg->m_picarea[pic_indx].ymin;
				pic_rect[3] = prop_dlg->m_picarea[pic_indx].ymax;
				Convert_pic_rect(pic_rect, rect_float, 1);
				CRect rect, rect2;
				rect2.left = rect_float[0];
				rect2.top = rect_float[1];
				rect2.right = rect_float[2];
				rect2.bottom = rect_float[3];
				rect = rect2;
				((CNCLDDPicWin*)m_picture[item])->SetHotSpotRect(rect2);
				m_picture[item]->ClientToScreen(&rect);
				ScreenToClient(rect);
				CreateHotSpotRect(rect);
				InvalidateRect(rect);
				UpdateWindow();
				m_parent->ScreenToClient(rect);
				m_parent->InvalidateRect(rect);
				m_parent->UpdateWindow();
				prop_dlg->m_pic_act_area = hpnum;
				((CNCLFormView*)m_parent)->OpenPropertyPage(prop_dlg, 0);
//////////////
			}
		}
	}
}


void CNCLDDform::UpdatePropertyHSPTSize(CRect sizerec, int itemno)
{
	if (itemno<0)
		return;
	float rect[4];
	((CNCLFormView*)m_parent)->UpdatePropertyHSPTSize(sizerec, rect);
	m_picture[itemno]->ClientToScreen(&sizerec);
	ScreenToClient(sizerec);
	m_hotspot_rect = sizerec;
	CNCLFormProp *prop_dlg = ((CNCLFormView*)m_parent)->GetPropertyPage();
	if (prop_dlg->m_picarea==NULL)
		return;
	int indx = prop_dlg->m_pic_act_area;
	prop_dlg->m_picarea[indx].xmin = rect[0];
	prop_dlg->m_picarea[indx].ymin = rect[1];
	prop_dlg->m_picarea[indx].xmax = rect[2];
	prop_dlg->m_picarea[indx].ymax = rect[3];
}
//flag=0: convert pixel rectage in current picture item into percentage
//flag=1: convert picture area percentage of rectage into pixel
void CNCLDDform::Convert_pic_rect(float in_rect[4], float out_rect[4], int flag)
{
	
	if (m_current_pic==NULL)
	{
		out_rect[0] = -1000;
		return;
	}
	CRect rect;
	m_current_pic->GetClientRect(&rect);
	if (flag==0)
	{
		out_rect[0] = (in_rect[0]*1.0)/rect.Width()*100;
		out_rect[2] = (in_rect[2]*1.0)/rect.Width()*100;
		out_rect[1] = (in_rect[1]*1.0)/rect.Height()*100;
		out_rect[3] = (in_rect[3]*1.0)/rect.Height()*100;
	}
	else
	{
		out_rect[0] = in_rect[0]*rect.Width()/100.0;
		out_rect[2] = in_rect[2]*rect.Width()/100.0;
		out_rect[1] = in_rect[1]*rect.Height()/100.0;
		out_rect[3] = in_rect[3]*rect.Height()/100.0;
	}
}
