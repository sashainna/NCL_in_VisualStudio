/************************************************************************
c
c   FILE NAME: IgesAttrDlg.cpp
c
c   CONTAINS: 
c     Functions for the class CIgesAttrDlg, which is
c     class for "matching Atributes" dialog
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			IgesAttrDlg.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c			04/29/15 , 15:12:55
c
c**********************************************************************/
// IgesAttrDlg.cpp : implementation file
//

#include "wsntstdafx.h"
#include "iges.h"
#include "IgesAttrDlg.h"
#include "tiges.h"
#include "wsntclrdlg.h"
#include <gdiplus.h>
using namespace Gdiplus; 

extern "C" void ncl_check_label_prefix(char *,int,int);

extern "C" int UIG_match_color_array[];
extern "C" int UIG_match_layer_array[];

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CIgesAttrDlg dialog


CIgesAttrDlg::CIgesAttrDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CIgesAttrDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CIgesAttrDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CIgesAttrDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CIgesAttrDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CIgesAttrDlg, CDialog)
	//{{AFX_MSG_MAP(CIgesAttrDlg)
	ON_BN_CLICKED(IDC_ATTRIB_COLOR1, OnColorSel1)
	ON_BN_CLICKED(IDC_ATTRIB_COLOR2, OnColorSel2)
	ON_BN_CLICKED(IDC_ATTRIB_COLOR3, OnColorSel3)
	ON_BN_CLICKED(IDC_ATTRIB_COLOR4, OnColorSel4)
	ON_BN_CLICKED(IDC_ATTRIB_COLOR5, OnColorSel5)
	ON_BN_CLICKED(IDC_ATTRIB_COLOR6, OnColorSel6)
	ON_BN_CLICKED(IDC_ATTRIB_COLOR7, OnColorSel7)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIgesAttrDlg message handlers

/*********************************************************************
**    I_FUNCTION     :  CIgesAttrDlg::OnOK() 
**       Callback function for the OK button on the iges match
**       attributes form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesAttrDlg::OnOK() 
{
	CString tmpstr;
	char *txtp;
	int i, len, layer, color;

	for (i=0;i<7;i++)
	{
		if(i==6)
			((CWnd*)GetDlgItem(IDC_ATTRIB_LAYER_EDIT7))->GetWindowText(tmpstr);
		else
			((CWnd*)GetDlgItem(IDC_ATTRIB_LAYER_EDIT1+i))->GetWindowText(tmpstr);
		len = tmpstr.GetLength();
		txtp = tmpstr.GetBuffer(256);
		layer = -1;
		if (len) sscanf(txtp,"%d",&layer);
		if (layer > 9999) layer = 9999;
		UIG_match_layer_array[i] = layer;
	}
	for (i=0;i<7;i++)
	{
		UIG_match_color_array[i] = m_color[i];
	}
	CDialog::OnOK();
}
/************************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c     This function Set the initialized parameter into dialog
c       This member function is called in response to 
c     the WM_INITDIALOG message. This message is sent to 
c     the dialog box during the Create, CreateIndirect, 
c     or DoModal calls, which occur immediately before 
c     the dialog box is displayed. 
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CIgesAttrDlg::OnInitDialog() 
{
	int i,j;
	char buf[256];
	
	CDialog::OnInitDialog();

	for (i = 0; i < 7; i++)
	{
		if (UIG_match_layer_array[i] >= 0)
			sprintf(buf,"%d",UIG_match_layer_array[i]);
		else
			buf[0] = '\0';
		if(i == 6)
			((CWnd*)GetDlgItem(IDC_ATTRIB_LAYER_EDIT7))->SetWindowText(buf);
		else
			((CWnd*)GetDlgItem(IDC_ATTRIB_LAYER_EDIT1+i))->SetWindowText(buf);
	}
	CRect rbtn;
	GetDlgItem(IDC_ATTRIB_COLOR_COMBO1)->GetWindowRect(&rbtn);
	GetDlgItem(IDC_ATTRIB_COLOR_COMBO1)->ShowWindow(SW_HIDE);
	ScreenToClient(&rbtn);
	m_button[0].Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY, 
		rbtn, this, IDC_ATTRIB_COLOR1);

	GetDlgItem(IDC_ATTRIB_COLOR_COMBO2)->GetWindowRect(&rbtn);
	GetDlgItem(IDC_ATTRIB_COLOR_COMBO2)->ShowWindow(SW_HIDE);
	ScreenToClient(&rbtn);
	m_button[1].Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY, 
		rbtn, this, IDC_ATTRIB_COLOR2);

	GetDlgItem(IDC_ATTRIB_COLOR_COMBO3)->GetWindowRect(&rbtn);
	GetDlgItem(IDC_ATTRIB_COLOR_COMBO3)->ShowWindow(SW_HIDE);
	ScreenToClient(&rbtn);
	m_button[2].Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY, 
		rbtn, this, IDC_ATTRIB_COLOR3);

	GetDlgItem(IDC_ATTRIB_COLOR_COMBO4)->GetWindowRect(&rbtn);
	GetDlgItem(IDC_ATTRIB_COLOR_COMBO4)->ShowWindow(SW_HIDE);
	ScreenToClient(&rbtn);
	m_button[3].Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY, 
		rbtn, this, IDC_ATTRIB_COLOR4);

	GetDlgItem(IDC_ATTRIB_COLOR_COMBO5)->GetWindowRect(&rbtn);
	GetDlgItem(IDC_ATTRIB_COLOR_COMBO5)->ShowWindow(SW_HIDE);
	ScreenToClient(&rbtn);
	m_button[4].Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY, 
		rbtn, this, IDC_ATTRIB_COLOR5);

	GetDlgItem(IDC_ATTRIB_COLOR_COMBO6)->GetWindowRect(&rbtn);
	GetDlgItem(IDC_ATTRIB_COLOR_COMBO6)->ShowWindow(SW_HIDE);
	ScreenToClient(&rbtn);
	m_button[5].Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY, 
		rbtn, this, IDC_ATTRIB_COLOR6);

	GetDlgItem(IDC_ATTRIB_COLOR_COMBO7)->GetWindowRect(&rbtn);
	GetDlgItem(IDC_ATTRIB_COLOR_COMBO7)->ShowWindow(SW_HIDE);
	ScreenToClient(&rbtn);
	m_button[6].Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_CENTER | WS_TABSTOP | BS_OWNERDRAW | BS_NOTIFY, 
		rbtn, this, IDC_ATTRIB_COLOR7);

	COLORREF bcolor;
	int color;
	for (i=0; i<7;i++)
	{
		color = UIG_match_color_array[i];
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
		m_button[i].set_color(bcolor, bcolor);
		m_button[i].ShowWindow(SW_SHOW);
	}
	return TRUE;

}
/**********************************************************************
**    I_FUNCTION : SetButColor(int color)
**       set color button color
**    PARAMETERS  
**       INPUT  : indx, color
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesAttrDlg::SetButColor(int indx, int color)
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
	m_button[indx].set_color(bcolor, bcolor);
	m_button[indx].Invalidate();
	m_button[indx].UpdateWindow();
	m_color[indx] = color;
}
/*********************************************************************
**    I_FUNCTION     :  CIgesAttrDlg::OnColorSel(indx) 
**       handle Callback function for the color button on the iges match
**       attributes form.
**    PARAMETERS
**       INPUT  :
**          indx : button indx
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesAttrDlg::OnColorSel(int indx)
{	
	GdiplusStartupInput gdiplusStartupInput;
	ULONG_PTR gdiplusToken;
	char defstr[64];

	GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);

	CNCLColorDlg dlg;
	dlg.SetColorIndex(UIG_match_color_array[indx]);
	dlg.SetColorDefault(1, "Default");
	dlg.disable_addcolor();
	int nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
/*
.....set the field color to select color
*/
		SetButColor(indx, dlg.m_current_color);
	}
	else if (nResponse == IDCANCEL)
	{
	}
	GdiplusShutdown(gdiplusToken);
}
/*********************************************************************
**    I_FUNCTION     :  CIgesAttrDlg::OnColorSel1() 
**       Callback function for the color button on the iges match
**       attributes form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesAttrDlg::OnColorSel1()
{
	OnColorSel(0);
}
/*********************************************************************
**    I_FUNCTION     :  CIgesAttrDlg::OnColorSel2() 
**       Callback function for the color button on the iges match
**       attributes form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesAttrDlg::OnColorSel2()
{
	OnColorSel(1);
}
/*********************************************************************
**    I_FUNCTION     :  CIgesAttrDlg::OnColorSel3() 
**       Callback function for the color button on the iges match
**       attributes form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesAttrDlg::OnColorSel3()
{
	OnColorSel(2);
}
/*********************************************************************
**    I_FUNCTION     :  CIgesAttrDlg::OnColorSel4() 
**       Callback function for the color button on the iges match
**       attributes form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesAttrDlg::OnColorSel4()
{
	OnColorSel(3);
}
/*********************************************************************
**    I_FUNCTION     :  CIgesAttrDlg::OnColorSel5() 
**       Callback function for the color button on the iges match
**       attributes form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesAttrDlg::OnColorSel5()
{
	OnColorSel(4);
}
/*********************************************************************
**    I_FUNCTION     :  CIgesAttrDlg::OnColorSel6() 
**       Callback function for the color button on the iges match
**       attributes form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesAttrDlg::OnColorSel6()
{
	OnColorSel(5);
}
/*********************************************************************
**    I_FUNCTION     :  CIgesAttrDlg::OnColorSel7() 
**       Callback function for the color button on the iges match
**       attributes form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesAttrDlg::OnColorSel7()
{
	OnColorSel(6);
}
#ifdef TEMP
/************************************************************************
c
c   FILE NAME: IgesModalDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class CIgesModalDlg, which is
c			class for "Name Modals" dialog
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       IgesModalDlg.cpp , 17.1
c    DATE AND TIME OF LAST  MODIFICATION
c       10/10/3 , 11:05:35               
c
c**********************************************************************/
// IgesModalDlg.cpp : implementation file
//

#include "stdafx.h"
#include "iges.h"
#include "IgesModalDlg.h"
#include "mdrel.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" iges_config(int );
extern "C" ul_to_upper(char *);
extern "C" um_update_rel_label(int, char[3], int);

extern "C" char geo_lab[11][7];
extern "C" int lab_flag[11];

/////////////////////////////////////////////////////////////////////////////
// CIgesModalDlg dialog


CIgesModalDlg::CIgesModalDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CIgesModalDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CIgesModalDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CIgesModalDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CIgesModalDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CIgesModalDlg, CDialog)
	//{{AFX_MSG_MAP(CIgesModalDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIgesModalDlg message handlers

void CIgesModalDlg::OnCancel() 
{	
	CDialog::OnCancel();
}
/*********************************************************************
**    I_FUNCTION     :  CIgesModalDlg::OnOK() 
**			Callback function for the ACCEPT button on the iges_name_modal
**			Motif form.  Read in info from global array of textbox widgets
**			that are displayed in the form.  This is to find out what
**			abbreviations the user wants for the geometry (ex. pt for points).
**			The abbreviations must be between 2 and 6 characters.  Pass 
**			the global array lab_flag[] and the geometry labels to a
**			function to update the INIT file containing the defaults.
**			Next, modify the global data structure UM_labelmdl.
**			Finally, destroy the form. 
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : 
**			Changes the global default naming modal convention by 
**			changing UM_labelmdl 
**    WARNINGS     : none
*********************************************************************/
void CIgesModalDlg::OnOK() 
{
	CString tmpstr;
	char *text;
	int i, status, len;
	static int relnum[] = {UM_POINT_REL, NCL_POINTVEC_REL, UM_LINE_REL,
      NCL_VECTOR_REL, NCL_PLN_REL, UM_CIRCLE_REL, UM_RBSPLCRV_REL, 
		UM_RBSPLSRF_REL, NCL_SHAPE_REL, NCL_MATRIX_REL, NCL_PATERN_REL};
/*
..... Get the abbreviations that the user wants to use by getting the 
..... text from the global array of textbox widgets that are used in the
..... iges_name_modal form.  Check to make sure that they are between 2
..... and 6 characters long.
*/
	for (i = 0; i < 11; i++)
	{
		lab_flag[i] = ((CComboBox*)GetDlgItem(IDC_COMBO1+i))->GetCurSel();
		((CWnd*)GetDlgItem(IDC_EDIT1+i))->GetWindowText(tmpstr);
		len = tmpstr.GetLength();
		if ((lab_flag[i] == 0) && (len != 2))
		{
			MessageBox("Prefixes must be 2 characters!", "Error!", MB_OK);
			return;
		}
		if ((lab_flag[i] == 1) && (len > 6))
		{
			MessageBox("Labels for subscripting must be <= 6 characters!", "Error!", MB_OK);
			return;
		}
		if (len!=0)
		{
			text = tmpstr.GetBuffer(256);
			strcpy(geo_lab[i], text);
			ul_to_upper(geo_lab[i]);
		}
		else
		{
			geo_lab[i][0] = '\0';
		}
		ncl_check_label_prefix(geo_lab[i],lab_flag[i],TRUE);
		um_update_rel_label (relnum[i], geo_lab[i], lab_flag[i]);
	}
/*
.....following MOTIF version, do nothing here
	status = iges_config(1);
	if (status != 0) return;
*/
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
BOOL CIgesModalDlg::OnInitDialog() 
{
	int i;
	CDialog::OnInitDialog();
	for (i = 0; i < 11; i++)
	{
		((CWnd*)GetDlgItem(IDC_EDIT1+i))->SetWindowText(geo_lab[i]);
	}
	for (i = 0; i < 11; i++)
	{
		((CComboBox*)GetDlgItem(IDC_COMBO1+i))->SetCurSel(lab_flag[i]);
	}
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
#endif
