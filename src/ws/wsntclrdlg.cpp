/************************************************************************
**
**   FILE NAME: wsntclrdlg.cpp
**
**	 Description - Functions implementation for
**		CNCLColorDlg class 
**	 CONTAINS: 
**		all functions declared in wsntclrdlg.h
**
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntclrdlg.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:20
**
************************************************************************
*/
#include "wsntstdafx.h"
#include "wsntres.h"
#include "wsgl.h"
#include "wsntclrdlg.h"

int UW_clr_selected = -1;

void HLStoRGB( double H, double L, double S,  int &red, int &green, int &blue);
void RGBtoHSL( COLORREF rgb, double *H, double *S, double *L );

extern "C" void ncl_update_colors(int flag);
extern "C" void ul_to_upper(char*);
extern "C" int uw_ntyes_or_no(CWnd *parent, char* msg, char *title);

int CNCLColorDlg::IsColorNameDefined(char *nstr)
{
	int i;
	for (i=0;i<16;i++)
		if (_stricmp(nstr, uw_color_name[i])==0)
			return (i+1);
	for (i=0;i<48;i++)
		if (_stricmp(nstr, m_color_cname[i])==0)
			return (i+1+16);
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CNCLColorDlg dialog

/***********************************************************************
c
c   SUBROUTINE:  CNCLColorDlg
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLColorDlg::CNCLColorDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CNCLColorDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CNCLColorDlg)
	m_lum = 0;
	m_hue = 0;
	m_sat = 0;
	m_crColor = RGB(255,255,255);
	m_red = 255;
	m_blue = 255;
	m_green = 255;

	//}}AFX_DATA_INIT
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	m_saved_color = 0;
	for (int i=16; i<64;i++)
	{
		m_cust_color[i-16][0] = uw_color_table[i][0];
		m_cust_color[i-16][1] = uw_color_table[i][1];
		m_cust_color[i-16][2] = uw_color_table[i][2];
		strcpy_s(m_color_cname[i-16], 64, uw_color_name[i]);
	}
	m_defflag = 0;
	m_addcolor = 1;
}


void CNCLColorDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNCLColorDlg)
	DDX_Text(pDX, IDC_EDIT1, m_lum);
	DDV_MinMaxInt(pDX, m_lum, 0, 255);
	DDX_Text(pDX, IDC_EDIT3, m_hue);
	DDV_MinMaxInt(pDX, m_hue, 0, 255);
	DDX_Text(pDX, IDC_EDIT2, m_sat);
	DDV_MinMaxInt(pDX, m_sat, 0, 255);

	DDX_Text(pDX, IDC_RED, m_red);
	DDV_MinMaxInt(pDX, m_lum, 0, 255);
	DDX_Text(pDX, IDC_GREEN, m_green);
	DDV_MinMaxInt(pDX, m_hue, 0, 255);
	DDX_Text(pDX, IDC_BLUE, m_blue);
	DDV_MinMaxInt(pDX, m_sat, 0, 255);

	//}}AFX_DATA_MAP
	DDX_Control(pDX, IDC_LUMCHOOSER, m_LumChooser);
	DDX_Control(pDX, IDC_HUESAT, m_HueSatChooser);
}

BEGIN_MESSAGE_MAP(CNCLColorDlg, CDialog)
	//{{AFX_MSG_MAP(CNCLColorDlg)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_WM_DESTROY()
	ON_WM_SYSCOLORCHANGE()

	//}}AFX_MSG_MAP
	ON_NOTIFY(CNCLHueSatChooser::NM_COLORCHANGE, IDC_HUESAT, OnHueSatChange)
	ON_NOTIFY(CNCLLumChooser::NM_COLORCHANGE, IDC_LUMCHOOSER, OnLumChange)
	ON_MESSAGE(WM_NCL_SELECTCOLOROK, OnSelectColorOK)
	ON_EN_CHANGE(IDC_EDIT2, &CNCLColorDlg::OnSatchanged)
	ON_EN_CHANGE(IDC_EDIT3, &CNCLColorDlg::OnHueChanged)
	ON_EN_CHANGE(IDC_EDIT1, &CNCLColorDlg::OnLumChanged)
	ON_EN_CHANGE(IDC_RED, &CNCLColorDlg::OnEnChangeRed)
	ON_EN_CHANGE(IDC_GREEN, &CNCLColorDlg::OnEnChangeGreen)
	ON_EN_CHANGE(IDC_BLUE, &CNCLColorDlg::OnEnChangeBlue)
	ON_COMMAND(IDC_NCL_DEFAULT_CLR, &CNCLColorDlg::OnCheckDef)
	ON_COMMAND(IDC_NCL_ADD_COLOR, &CNCLColorDlg::OnAddColor)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNCLColorDlg message handlers

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
BOOL CNCLColorDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	CRect theRect;
	GetDlgItem(IDC_NCL_COLORS)->GetWindowRect( &theRect );
	ScreenToClient(&theRect);	
	m_wndColor_basic.Create(WS_CHILD|WS_VISIBLE,theRect, this,4232,RGB(255,255,255),0);
	m_wndColor_basic.GetColorControl()->InitColorPalette();

	CRect theRect2;
	GetDlgItem(IDC_NCL_COLORS2)->GetWindowRect( &theRect2 );
	ScreenToClient(&theRect2);	
	m_wndColor_custom.Create(WS_CHILD|WS_VISIBLE,theRect2, this,4232,0,1);
	m_wndColor_custom.GetColorControl()->InitColorPalette();

	if (m_defflag==0)
	{
		((CButton*)GetDlgItem(IDC_NCL_DEFAULT_CLR))->ShowWindow(SW_HIDE);
		if (m_current_color<0)
			m_current_color = 0;
	}
	else
	{
		((CButton*)GetDlgItem(IDC_NCL_DEFAULT_CLR))->SetWindowText(m_defstr);
	}
	m_btnColor.Attach(IDC_NCL_BUTTON_PREVIEW,this);
	int current_color;
	if (m_current_color<0)
	{
		((CButton*)GetDlgItem(IDC_NCL_DEFAULT_CLR))->SetCheck(1);
		m_btnColor.SetOldColordef(1);
		m_btnColor.SetButtondef(1);
		m_saved_color = 0;
		m_current_color = -1;
		m_wndColor_custom.GetColorControl()->Deselect();
		m_wndColor_basic.GetColorControl()->Deselect();
		GetDlgItem(IDC_NCL_CLR_NAME)->SetWindowText("DEFAULT");
	}
	else if (m_current_color<16)
	{
		m_crColor = RGB(uw_color_table[m_current_color][0], 
						uw_color_table[m_current_color][1], 
						uw_color_table[m_current_color][2]);
		m_wndColor_basic.GetColorControl()->SetRGB(m_crColor);
		m_wndColor_basic.GetColorControl()->Setselcell(m_current_color);
		GetDlgItem(IDC_NCL_CLR_NAME)->SetWindowText(uw_color_name[m_current_color]);
		m_wndColor_custom.GetColorControl()->Deselect();
		((CButton*)GetDlgItem(IDC_NCL_DEFAULT_CLR))->SetCheck(0);
	}
	else
	{
		current_color = m_current_color;
		m_crColor = RGB(m_cust_color[current_color-16][0], 
						m_cust_color[current_color-16][1], 
						m_cust_color[current_color-16][2]);
		m_wndColor_custom.GetColorControl()->SetRGB(m_crColor);
		m_wndColor_custom.GetColorControl()->Setselcell(current_color-16);
		GetDlgItem(IDC_NCL_CLR_NAME)->SetWindowText(m_color_cname[current_color-16]);
		m_wndColor_basic.GetColorControl()->Deselect();
		((CButton*)GetDlgItem(IDC_NCL_DEFAULT_CLR))->SetCheck(0);
	}
	if (m_current_color!=-1)
	{
		m_btnColor.SetButtonColor(m_crColor);
		m_btnColor.SetOldColor(m_crColor);
		m_red = GetRValue(m_crColor);
		m_green = GetGValue(m_crColor);
		m_blue = GetBValue(m_crColor);

		double h, s, l;
		RGBtoHSL(m_crColor, &h, &s, &l);
		m_LumChooser.SetColor(CHls(h, l, s));
		SetHueSatBox(h, l, s);
		m_lum = (int) l*255;
		m_hue = (int)h*255;
		m_sat = (int)s*255;
	}
	CWinApp* pApp = AfxGetApp();
	ASSERT(pApp);
	if (pApp)
	{
		m_hCursor = pApp->LoadCursor(IDC_NCL_SELECTCOLOR_CURSOR);
	}
	if (m_current_color<0)
	{
/*
.....disable color value field
*/
		GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT2)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT3)->EnableWindow(FALSE);
		GetDlgItem(IDC_RED)->EnableWindow(FALSE);
		GetDlgItem(IDC_BLUE)->EnableWindow(FALSE);
		GetDlgItem(IDC_GREEN)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC5)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC6)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC4)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC3)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC2)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC1)->EnableWindow(FALSE);
		m_HueSatChooser.EnableWindow(FALSE);
		m_LumChooser.EnableWindow(FALSE);
		GetDlgItem(IDC_NCL_ADD_COLOR)->EnableWindow(FALSE);
	}
	if (m_addcolor==0)
/*
......disable add custom color button
*/
	{
		GetDlgItem(IDC_NCL_ADD_COLOR)->EnableWindow(FALSE);
	}
	UpdateData(FALSE);
	return TRUE;  // return TRUE  unless you set the focus to a control
}

/***********************************************************************
**
**   SUBROUTINE: OnSelectColorOK
**
**   FUNCTION:  callback for WM_NCL_SELECTCOLOROK message
**
**   INPUT:  
**			none
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
LRESULT CNCLColorDlg::OnSelectColorOK(WPARAM wParam, LPARAM lParam)
{
	int current_color;
	lParam;
	COLORREF crNewColor = (COLORREF) wParam;
	if (UW_clr_selected==0)
	{
		current_color = m_wndColor_basic.GetColorControl()->Getselcell();
		m_wndColor_custom.GetColorControl()->Deselect();
		GetDlgItem(IDC_NCL_CLR_NAME)->SetWindowText(uw_color_name[current_color]);
		((CButton*)GetDlgItem(IDC_NCL_DEFAULT_CLR))->SetCheck(0);
		m_current_color = current_color;
		GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
		GetDlgItem(IDC_EDIT2)->EnableWindow(TRUE);
		GetDlgItem(IDC_EDIT3)->EnableWindow(TRUE);
		GetDlgItem(IDC_RED)->EnableWindow(TRUE);
		GetDlgItem(IDC_BLUE)->EnableWindow(TRUE);
		GetDlgItem(IDC_GREEN)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC5)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC6)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC4)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC3)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC2)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC1)->EnableWindow(TRUE);
		m_HueSatChooser.EnableWindow(TRUE);
		m_LumChooser.EnableWindow(TRUE);
		GetDlgItem(IDC_NCL_ADD_COLOR)->EnableWindow(TRUE);
	}
	else if (UW_clr_selected==1)
	{
		current_color = m_wndColor_custom.GetColorControl()->Getselcell()+16;
		m_wndColor_basic.GetColorControl()->Deselect();
		GetDlgItem(IDC_NCL_CLR_NAME)->SetWindowText(m_color_cname[current_color-16]);
		((CButton*)GetDlgItem(IDC_NCL_DEFAULT_CLR))->SetCheck(0);
		m_current_color = current_color;
		GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
		GetDlgItem(IDC_EDIT2)->EnableWindow(TRUE);
		GetDlgItem(IDC_EDIT3)->EnableWindow(TRUE);
		GetDlgItem(IDC_RED)->EnableWindow(TRUE);
		GetDlgItem(IDC_BLUE)->EnableWindow(TRUE);
		GetDlgItem(IDC_GREEN)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC5)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC6)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC4)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC3)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC2)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC1)->EnableWindow(TRUE);
		m_HueSatChooser.EnableWindow(TRUE);
		m_LumChooser.EnableWindow(TRUE);
		GetDlgItem(IDC_NCL_ADD_COLOR)->EnableWindow(TRUE);
	}
	m_wndColor_basic.Invalidate(TRUE);
	m_wndColor_custom.Invalidate(TRUE);
	m_btnColor.SetButtonColor(crNewColor);
	SetColor(crNewColor);

	m_red = GetRValue(crNewColor);
	m_green = GetGValue(crNewColor);
	m_blue = GetBValue(crNewColor);

	double h, s, l;
	RGBtoHSL(crNewColor, &h, &s, &l);
	m_LumChooser.SetColor(CHls(h, l, s));
	SetHueSatBox(h, l, s);

	m_lum = (int)l*255;
	m_hue = (int) h*255;
	m_sat = (int)s*255;
	UpdateData(FALSE);
	return 0L;
}
void CNCLColorDlg::OnDestroy() 
{
	if(m_Palette.GetSafeHandle()!=NULL)
		m_Palette.DeleteObject();
	CDialog::OnDestroy();
}

/***********************************************************************
**
**   SUBROUTINE: OnSysColorChange
**
**   FUNCTION:  called when system color changed
**
**   INPUT:  
**			none
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnSysColorChange() 
{
	CDialog::OnSysColorChange();

	gfxData.OnSysColorChange();
	
	CreatePalette();
	Invalidate();
	UpdateWindow();
}

void CNCLColorDlg::CreatePalette(BOOL bRedraw)
{
	CPaintDC dc (this);

	CDC *pDC = &dc;
	
	if(pDC->GetDeviceCaps(RASTERCAPS) & RC_PALETTE)
	{
		LOGPALETTE *pPal=(LOGPALETTE *)new unsigned char[sizeof(LOGPALETTE)+257*sizeof(PALETTEENTRY)];
	
		pPal->palVersion=0x300;
		pPal->palNumEntries=256;

		for(int i=0;i<256;i++)
		{
			pPal->palPalEntry[i].peRed=(unsigned char)(255 * (i & 0x07) / 7 );
			pPal->palPalEntry[i].peGreen=(unsigned char)(255 * ((i >> 3) & 0x07)/ 7 );
			pPal->palPalEntry[i].peBlue=(unsigned char)(255 * ((i >> 6) & 0x03) / 3 );
			pPal->palPalEntry[i].peFlags = 0;
		}

		m_Palette.CreatePalette(pPal);//Halftone

		pDC->SelectPalette(&m_Palette,FALSE);
		pDC->RealizePalette();
		delete pPal;
	}
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
void CNCLColorDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}
/***********************************************************************
**
**   SUBROUTINE: OnQueryDragIcon
**
**   FUNCTION:  The system calls this to obtain the cursor to display while the user drags
**				the minimized window.
**   INPUT:  
**			none
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
HCURSOR CNCLColorDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

/***********************************************************************
**
**   SUBROUTINE: OnHueSatChange
**
**   FUNCTION:  callback for NM_COLORCHANGE.
**   INPUT:  
**			not used, 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnHueSatChange( NMHDR * pNotifyStruct, LRESULT * result )
{
	m_LumChooser.SetHueSat(m_HueSatChooser.GetHue(), m_HueSatChooser.GetSat());
	CHls hls = m_LumChooser.GetColor();
	m_lum =(int)( hls.GetLuminance()*255);
	m_hue = (int)(hls.GetHue()*255);
	m_sat = (int)(hls.GetSaturation()*255);
	HLStoRGB(hls.GetHue(), hls.GetLuminance(), hls.GetSaturation(), m_red, m_green, m_blue);
	
	COLORREF crNewColor = RGB(m_red, m_green, m_blue);
/*
......if the default color checked, don't reset button color
*/
	if (m_current_color!=-1)
	{
		m_btnColor.SetButtonColor(crNewColor);
		SetColor(crNewColor);
	}
	UpdateData(FALSE);
	*result = 0;
}

/***********************************************************************
**
**   SUBROUTINE: OnLumChange
**
**   FUNCTION:  callback for CNCLLumChooser::NM_COLORCHANGE
**   INPUT:  
**			not used, 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnLumChange(NMHDR * pNotifyStruct, LRESULT * result )
{
	CHls hls = m_LumChooser.GetColor();
	m_lum = (int)(hls.GetLuminance()*255);
	m_hue = (int)(hls.GetHue()*255);
	m_sat = (int)(hls.GetSaturation()*255);
	HLStoRGB(hls.GetHue(), hls.GetLuminance(), hls.GetSaturation(), m_red, m_green, m_blue);

	COLORREF crNewColor = RGB(m_red, m_green, m_blue);
/*
......if the default color checked, don't reset button color
*/
	if (m_current_color!=-1)
	{
		m_btnColor.SetButtonColor(crNewColor);
		SetColor(crNewColor);
	}
	UpdateData(FALSE);
	*result = 0;
}
void CNCLColorDlg::SetHueSatBox(double h, double l, double s)
{
	m_HueSatChooser.SetColor(CHls(h, l, s));
	m_HueSatChooser.UpdateWindow();
}

/***********************************************************************
**
**   SUBROUTINE: OnSatchanged()
**
**   FUNCTION:  callback for SAT value changed
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnSatchanged()
{
	char strValue[256];
	GetDlgItem(IDC_EDIT2)->GetWindowText(strValue, 256);
	m_sat = atoi(strValue);
	double h, s, l;

	h = (double)m_hue/255.0;
	l = (double)m_lum/255.0;
	s = (double)m_sat/255.0;

	HLStoRGB(h, l, s, m_red, m_green, m_blue);
	COLORREF crNewColor = RGB(m_red, m_green, m_blue);
/*
......if the default color checked, don't reset button color
*/
	if (m_current_color!=-1)
	{
		m_btnColor.SetButtonColor(crNewColor);
		SetColor(crNewColor);
	}
	UpdateData(FALSE);
	SetHueSatBox(h, l, s);
}


/***********************************************************************
**
**   SUBROUTINE: OnHueChanged()
**
**   FUNCTION:  callback for Hue value changed
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnHueChanged()
{
	char strValue[256];
	GetDlgItem(IDC_EDIT3)->GetWindowText(strValue, 256);
	m_hue = atoi(strValue);
	double h, s, l;

	h = (double)m_hue/255.0;
	l = (double)m_lum/255.0;
	s = (double)m_sat/255.0;

	HLStoRGB(h, l, s, m_red, m_green, m_blue);
	COLORREF crNewColor = RGB(m_red, m_green, m_blue);
/*
......if the default color checked, don't reset button color
*/
	if (m_current_color!=-1)
	{
		m_btnColor.SetButtonColor(crNewColor);
		SetColor(crNewColor);
	}
	UpdateData(FALSE);
	SetHueSatBox(h, l, s);
}

/***********************************************************************
**
**   SUBROUTINE: OnLumChanged()
**
**   FUNCTION:  callback for Lum value changed
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnLumChanged()
{
	char strValue[256];
	GetDlgItem(IDC_EDIT1)->GetWindowText(strValue, 256);
	m_lum = atoi(strValue);
	double h, s, l;

	h = (double)m_hue/255.0;
	l = (double)m_lum/255.0;
	s = (double)m_sat/255.0;
	m_LumChooser.SetColor(CHls(h, l, s));

	HLStoRGB(h, l, s, m_red, m_green, m_blue);
	COLORREF crNewColor = RGB(m_red, m_green, m_blue);
/*
......if the default color checked, don't reset button color
*/
	if (m_current_color!=-1)
	{
		m_btnColor.SetButtonColor(crNewColor);
		SetColor(crNewColor);
	}
	UpdateData(FALSE);
}

/***********************************************************************
**
**   SUBROUTINE: OnEnChangeRed()
**
**   FUNCTION:  callback for Red value changed
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnEnChangeRed()
{
	char strValue[256];
	GetDlgItem(IDC_RED)->GetWindowText(strValue, 256);
	m_red = atoi(strValue);
	COLORREF crNewColor = RGB(m_red, m_green, m_blue);
/*
......if the default color checked, don't reset button color
*/
	if (m_current_color!=-1)
	{
		m_btnColor.SetButtonColor(crNewColor);
		SetColor(crNewColor);
	}
	double h, s, l;
	RGBtoHSL(crNewColor, &h, &s, &l);
	m_LumChooser.SetColor(CHls(h, l, s));
	m_lum = (int)l*255;
	m_hue = (int)h*255;
	m_sat = (int)s*255;
	UpdateData(FALSE);
	SetHueSatBox(h, l, s);
}

/***********************************************************************
**
**   SUBROUTINE: OnEnChangeGreen()
**
**   FUNCTION:  callback for Green value changed
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnEnChangeGreen()
{
	char strValue[256];
	GetDlgItem(IDC_GREEN)->GetWindowText(strValue, 256);
	m_green = atoi(strValue);
	COLORREF crNewColor = RGB(m_red, m_green, m_blue);
/*
......if the default color checked, don't reset button color
*/
	if (m_current_color!=-1)
	{
		m_btnColor.SetButtonColor(crNewColor);
		SetColor(crNewColor);
	}
	double h, s, l;
	RGBtoHSL(crNewColor, &h, &s, &l);
	m_LumChooser.SetColor(CHls(h, l, s));
	m_lum = (int)l*255;
	m_hue =(int) h*255;
	m_sat =(int) s*255;
	UpdateData(FALSE);
	SetHueSatBox(h, l, s);
}

/***********************************************************************
**
**   SUBROUTINE: OnEnChangeBlue()
**
**   FUNCTION:  callback for blue value changed
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnEnChangeBlue()
{
	char strValue[256];
	GetDlgItem(IDC_BLUE)->GetWindowText(strValue, 256);
	m_blue = atoi(strValue);
	COLORREF crNewColor = RGB(m_red, m_green, m_blue);
/*
......if the default color checked, don't reset button color
*/
	if (m_current_color!=-1)
	{
		m_btnColor.SetButtonColor(crNewColor);
		SetColor(crNewColor);
	}
	double h, s, l;
	RGBtoHSL(crNewColor, &h, &s, &l);
	m_LumChooser.SetColor(CHls(h, l, s));
	m_lum = (int)l*255;
	m_hue = (int)h*255;
	m_sat = (int)s*255;
	UpdateData(FALSE);
	SetHueSatBox(h, l, s);
}
/***********************************************************************
**
**   SUBROUTINE: OnAddColor()
**
**   FUNCTION:  callback for add color button
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnAddColor()
{
	int i, indx, ans, nxt_indx;
	CString msg;
	char msg1[1024];
/*
......get color name
*/
	int current_color = m_wndColor_custom.GetColorControl()->Getselcell();
/*
.....get available custom color index
*/
	nxt_indx = -1;
	for (i=0; i<48; i++)
	{
		if (m_color_cname[i][0]=='\0')
		{
			nxt_indx = i;
			break;
		}
	}
	char nstr[65];
	int len = GetDlgItem(IDC_NCL_CLR_NAME)->GetWindowText(nstr, 64);
	if (len<=0)
	{
		MessageBox("Color name is not entered!", "Error");
		return;
	}
	indx = IsColorNameDefined(nstr);
	if ((indx>0)&&(indx<=16))
	{
		MessageBox("Basic color cannot be changed!", "Error");
		return;
	}
	else if (indx>16)
	{
		ans = uw_ntyes_or_no(this, 
				"Custom color is already defined.\r\nDo you want to redefine it?",
				"Update color?");
		if (ans==0)
			return;
		current_color = indx - 16 - 1;
	}
	else if ((current_color==-1)||(indx==0))
	{
/*
.....if there is no custom selection or new color name defined 
.....even there is a custom selection, use the next avaliable one
*/
		if ((nxt_indx==-1)&&(current_color==-1))
		{
/*
......there is no more availible custom color to be added (maxinum 48)
......and there is no custom color selected to be modify
*/
			msg = "There is no more availible custom color to be added (maxinum 48)\r\n";
			msg = msg + "You must select a custom color to modify the color name";
			MessageBox(msg, "Error");
			return;
		}
		else if (nxt_indx>=0)
			current_color = nxt_indx;
		else
		{
			sprintf_s(msg1, 1024, "Do you want to rename the color %s to %s?", 
				m_color_cname[current_color], nstr);
			ans = uw_ntyes_or_no(this, msg1,"Update color?");
			if (ans==0)
				return;
		}
	}				
	strcpy_s(m_color_cname[current_color], 64, nstr);
	m_cust_color[current_color][0] = m_red;
	m_cust_color[current_color][1] = m_green;
	m_cust_color[current_color][2] = m_blue;
	m_crColor = RGB(m_red, m_green, m_blue);
	m_wndColor_basic.GetColorControl()->Deselect();
	m_wndColor_custom.GetColorControl()->Setselcell(current_color);
	m_wndColor_custom.GetColorControl()->SetSelCellColor(m_crColor);
	m_wndColor_custom.Invalidate(TRUE);
	m_current_color = current_color + 16;
}
/***********************************************************************
**
**   SUBROUTINE: OnCheckDef()
**
**   FUNCTION:  callback for "default" checkbox
**   INPUT:  
**			none 
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLColorDlg::OnCheckDef()
{
	int checked = ((CButton*)GetDlgItem(IDC_NCL_DEFAULT_CLR))->GetCheck();
	if (checked)
	{
		m_btnColor.SetButtondef(1);
		m_saved_color = m_current_color;
		m_current_color = -1;
		m_wndColor_custom.GetColorControl()->Deselect();
		m_wndColor_basic.GetColorControl()->Deselect();
		GetDlgItem(IDC_NCL_CLR_NAME)->SetWindowText("DEFAULT");
		GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT2)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT3)->EnableWindow(FALSE);
		GetDlgItem(IDC_RED)->EnableWindow(FALSE);
		GetDlgItem(IDC_BLUE)->EnableWindow(FALSE);
		GetDlgItem(IDC_GREEN)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC5)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC6)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC1)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC2)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC3)->EnableWindow(FALSE);
		GetDlgItem(IDC_STATIC4)->EnableWindow(FALSE);
		m_HueSatChooser.EnableWindow(FALSE);
		m_LumChooser.EnableWindow(FALSE);
		GetDlgItem(IDC_NCL_ADD_COLOR)->EnableWindow(FALSE);
	}
	else
	{
		GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
		GetDlgItem(IDC_EDIT2)->EnableWindow(TRUE);
		GetDlgItem(IDC_EDIT3)->EnableWindow(TRUE);
		GetDlgItem(IDC_RED)->EnableWindow(TRUE);
		GetDlgItem(IDC_BLUE)->EnableWindow(TRUE);
		GetDlgItem(IDC_GREEN)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC5)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC6)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC4)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC3)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC2)->EnableWindow(TRUE);
		GetDlgItem(IDC_STATIC1)->EnableWindow(TRUE);
		m_HueSatChooser.EnableWindow(TRUE);
		m_LumChooser.EnableWindow(TRUE);
		GetDlgItem(IDC_NCL_ADD_COLOR)->EnableWindow(TRUE);
		if (UW_clr_selected==0)
		{
			m_crColor = RGB(uw_color_table[m_saved_color][0], 
						uw_color_table[m_saved_color][1], 
						uw_color_table[m_saved_color][2]);
			m_wndColor_basic.GetColorControl()->SetRGB(m_crColor);
			m_wndColor_basic.GetColorControl()->Setselcell(m_saved_color);
			GetDlgItem(IDC_NCL_CLR_NAME)->SetWindowText(uw_color_name[m_saved_color]);
			m_current_color = m_saved_color;
		}
		else if (UW_clr_selected==1)
		{
			int current_color = m_saved_color;
			if (current_color<16) current_color = 16;
			m_current_color = m_saved_color;
			m_crColor = RGB(m_cust_color[current_color-16][0], 
						m_cust_color[current_color-16][1], 
						m_cust_color[current_color-16][2]);
			m_wndColor_custom.GetColorControl()->SetRGB(m_crColor);
			m_wndColor_custom.GetColorControl()->Setselcell(current_color-16);
			GetDlgItem(IDC_NCL_CLR_NAME)->SetWindowText(m_color_cname[current_color-16]);
			m_wndColor_basic.GetColorControl()->Deselect();
		}
	}
	m_wndColor_basic.Invalidate(TRUE);
	m_wndColor_custom.Invalidate(TRUE);
	if (checked)
		return;
	m_btnColor.SetButtonColor(m_crColor);
	SetColor(m_crColor);

	m_red = GetRValue(m_crColor);
	m_green = GetGValue(m_crColor);
	m_blue = GetBValue(m_crColor);

	double h, s, l;
	RGBtoHSL(m_crColor, &h, &s, &l);
	m_LumChooser.SetColor(CHls(h, l, s));
	SetHueSatBox(h, l, s);

	m_lum = (int)l*255;
	m_hue =(int) h*255;
	m_sat =(int) s*255;
	UpdateData(FALSE);
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
void CNCLColorDlg::OnOK()
{
	int current_color = -1;
	int checked = ((CButton*)GetDlgItem(IDC_NCL_DEFAULT_CLR))->GetCheck();
	if (UW_clr_selected==0)
	{
		current_color = m_wndColor_basic.GetColorControl()->Getselcell();
		if ((current_color<0)&&(checked==0))
		{
			MessageBox("The Color Picked is not defined! Please select a defined color.", "Error");
			return;
		}
	}
	else if (UW_clr_selected==1)
	{
		current_color = m_wndColor_custom.GetColorControl()->Getselcell();
/*
.....if current color is not defined, just give message and return
*/
		if (((current_color>=0)&&(m_color_cname[current_color][0]=='\0'))||(current_color<0)&&(checked==0))
		{
			MessageBox("The Color Picked is not defined! Please select a defined color.", "Error");
			return;
		}
	}
	
	int changed = 0;
	for (int i=0; i<48;i++)
	{
		if (uw_color_table[i+16][0]!=m_cust_color[i][0])
		{
			uw_color_table[i+16][0] = m_cust_color[i][0];
			changed = 1;
		}
		if (uw_color_table[i+16][1]!=m_cust_color[i][1])
		{
			uw_color_table[i+16][1] = m_cust_color[i][1];
			changed = 1;
		}
		if (uw_color_table[i+16][2]!=m_cust_color[i][2])
		{
			uw_color_table[i+16][2] = m_cust_color[i][2];
			changed = 1;
		}
		if (stricmp(uw_color_name[i+16], m_color_cname[i])!=0)
		{
			strcpy_s(uw_color_name[i+16], 64, m_color_cname[i]);
			ul_to_upper(uw_color_name[i+16]);
			changed = 1;
		}
	}
	CDialog::OnOK();
/*
.....we update the color only if the colors changed
*/
	if (changed)
		ncl_update_colors(1);
}
