/************************************************************************
c
c   FILE NAME: StepAttrDlg.cpp
c
c   CONTAINS: 
c     Functions for the class CStepAttrDlg, which is
c     class for "matching Atributes" dialog
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			StepAttrDlg.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c			04/29/15 , 15:13:25
c
c**********************************************************************/
// StepAttrDlg.cpp : implementation file
//

#include "wsntstdafx.h"
#include "step.h"
#include "StepAttrDlg.h"
#include "tiges.h"
#include "wsntclrdlg.h"
#include <gdiplus.h>
using namespace Gdiplus; 


extern "C" int UIG_match_color_array[];
extern "C" int UIG_match_layer_array[];

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CStepAttrDlg dialog


CStepAttrDlg::CStepAttrDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CStepAttrDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CStepAttrDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CStepAttrDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStepAttrDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CStepAttrDlg, CDialog)
	//{{AFX_MSG_MAP(CStepAttrDlg)
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
// CStepAttrDlg message handlers

/*********************************************************************
**    I_FUNCTION     :  CStepAttrDlg::OnOK() 
**       Callback function for the OK button on the Step match
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
void CStepAttrDlg::OnOK() 
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
BOOL CStepAttrDlg::OnInitDialog() 
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
		m_color[i] = color = UIG_match_color_array[i];
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
void CStepAttrDlg::SetButColor(int indx, int color)
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
**    I_FUNCTION     :  CStepAttrDlg::OnColorSel(indx) 
**       handle Callback function for the color button on the Step match
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
void CStepAttrDlg::OnColorSel(int indx)
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
**    I_FUNCTION     :  CStepAttrDlg::OnColorSel1() 
**       Callback function for the color button on the Step match
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
void CStepAttrDlg::OnColorSel1()
{
	OnColorSel(0);
}
/*********************************************************************
**    I_FUNCTION     :  CStepAttrDlg::OnColorSel2() 
**       Callback function for the color button on the Step match
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
void CStepAttrDlg::OnColorSel2()
{
	OnColorSel(1);
}
/*********************************************************************
**    I_FUNCTION     :  CStepAttrDlg::OnColorSel3() 
**       Callback function for the color button on the Step match
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
void CStepAttrDlg::OnColorSel3()
{
	OnColorSel(2);
}
/*********************************************************************
**    I_FUNCTION     :  CStepAttrDlg::OnColorSel4() 
**       Callback function for the color button on the Step match
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
void CStepAttrDlg::OnColorSel4()
{
	OnColorSel(3);
}
/*********************************************************************
**    I_FUNCTION     :  CStepAttrDlg::OnColorSel5() 
**       Callback function for the color button on the Step match
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
void CStepAttrDlg::OnColorSel5()
{
	OnColorSel(4);
}
/*********************************************************************
**    I_FUNCTION     :  CStepAttrDlg::OnColorSel6() 
**       Callback function for the color button on the Step match
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
void CStepAttrDlg::OnColorSel6()
{
	OnColorSel(5);
}
/*********************************************************************
**    I_FUNCTION     :  CStepAttrDlg::OnColorSel7() 
**       Callback function for the color button on the Step match
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
void CStepAttrDlg::OnColorSel7()
{
	OnColorSel(6);
}
