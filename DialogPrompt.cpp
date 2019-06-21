/************************************************************************
c
c   FILE NAME: DialogPrompt.cpp
c
c	 CONTAINS: 
c	 all CDialogPrompt class override functions and 
c			Implementation functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        DialogPrompt.cpp , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/18 , 13:59:18
c
c**********************************************************************
*/

#include "PwStdafx.h"
#include "Mpost.h"
#include "mpostres.h"
#include "DialogTemplate.h"
#include "DialogPrompt.h"
#include "DialogHelp.h"
#include "mpostres.h"
#include "NpwHeaders.h"

#define LABEL_GAP 4
static int TOTALITEMS;
//char Pw_promptdlg_font[] = "Courier";
char Pw_promptdlg_font[80];
extern int MpChanged;
static int m_return_key = 0;
HACCEL m_accel;
ACCEL *m_form_accel;
/*
.....use this function when need get string size, considering gap 
.....and convert to dialog unit
*/
void pw_getstr_size(CWnd *win, char *string, int pt, char *fntname, int *wid, int* hgt, int flag)
{
	CFont aFont;
	CClientDC dc(win);
	aFont.CreatePointFont(pt,fntname, &dc);

	CFont* savfont = dc.SelectObject(&aFont );
	CString itemtext = ( LPCTSTR)string;

	CSize sizeText2 = dc.GetTextExtent("XXXXXXXXXXxxxxxxxxxx", 20);
	double basex = sizeText2.cx/20;
	double basey = sizeText2.cy;
	CSize sizeText = dc.GetTextExtent(itemtext,itemtext.GetLength());

	if (strlen(string)>=20)
	{
		*wid = sizeText.cx*4/basex + 11;
		*hgt = sizeText.cy*8/basey + 3;
		if ((strcmp(fntname, "Arial")==0)||(strcmp(fntname, "MS Serif")==0))
/*
.....I don't know why, but those font seems display longer than it calculated
.....when the string is long, and look like more unregular
*/
		{
			*wid += 5;
		}
		else if ((strcmp(fntname, "Garamond")==0)||(strcmp(fntname, "Arial Narrow")==0)
			|| (strcmp(fntname, "Times New Roman")==0))
			*wid += 18;
	}
	else
	{
		*wid = sizeText.cx*4/basex + strlen(string)/2 + 1;
		*hgt = sizeText.cy*8/basey + 3;
	}

	dc.SelectObject(&savfont);
}

/*
.....use this function when need get averige character size, 
.....not considering gap 
.....and not convert to dialog unit
*/
void pw_getavr_chrsize(CWnd *win, int pt, char *fntname, int *wid, int* hgt)
{
	CFont aFont;
	aFont.CreatePointFont(pt,fntname, NULL);
	CClientDC dc(win);

	CFont* savfont = dc.SelectObject(&aFont );
	CSize sizeText = dc.GetTextExtent("Xx", 2);
	int basex = sizeText.cx/2;
	int basey = sizeText.cy;
	*wid = (sizeText.cx*4/basex)/2;
	*hgt = (sizeText.cy) * 8/basey;
	dc.SelectObject(&savfont);
}
void pw_getavr_strsize(CWnd *win, int cnum, int pt, char *fntname, int *wid, int* hgt)
{
	CFont aFont;
	aFont.CreatePointFont(pt,fntname, NULL);
	CClientDC dc(win);

	CFont* savfont = dc.SelectObject(&aFont );
	CSize sizeText = dc.GetTextExtent("Xx", 2);
	int basex = sizeText.cx/2;
	int basey = sizeText.cy;
	*wid = (sizeText.cx*cnum/2)*4/basex;
	*hgt = (sizeText.cy) * 8/basey;
	dc.SelectObject(&savfont);
}

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
BEGIN_MESSAGE_MAP(CDialogPrompt, CDialog)
	//{{AFX_MSG_MAP(CDialogPrompt)
	ON_COMMAND(ID_APPLY, OnApply)
	ON_COMMAND(ID_LHELP, OnAHelp)
	ON_MESSAGE(CBN_VIEWSELCHANGE, OnViewChoicePicked)
	ON_MESSAGE(EN_VIEWSETFOCUS, FormUserCallbacks1)
	ON_MESSAGE(EN_VIEWKILLFOCUS, FormUserCallbacks2)
	ON_MESSAGE(CBN_VIEWKILLFOCUS, FormUserCallbacks2)
	ON_MESSAGE(CBN_VIEWSETFOCUS, FormUserCallbacks3)
	ON_COMMAND(ID_FORM_TABED, OnFormTabbed)
	ON_COMMAND(ID_FORM_STABED, OnFormSTabbed)
	ON_WM_SIZE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CWnd* CreateDlgView(CCreateContext* pContext, CWnd *pParent, CRect& rect, int wID,
					DLGTEMPLATE *dlgTempl,  CDialogItem dlgItem[200])
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
			if (pWnd->IsKindOf(RUNTIME_CLASS(CPWScrollView)))
				((CPWScrollView*)pWnd)->SetDlgTemp(dlgTempl, dlgItem);

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
c   SUBROUTINE:  CDialogPrompt
c
c   FUNCTION:  constructor
c
c   INPUT: none
c
c   OUTPUT: none
c
c***********************************************************************
*/
CDialogPrompt::CDialogPrompt(CWnd* pParent, int *level, int cur_stage, int id)	
{
	m_pParent = pParent;
	m_curStage = cur_stage;
	m_ChildId = id;
	for(int i=0;i<cur_stage;i++)
		m_pLevel[i] = level[i];
	CDialog::CDialog();
	m_pHelpDialog = NULL;
	m_hIcon = AfxGetApp()->LoadIcon(IDI_DLGICON);
	m_pScrollView = NULL;
	m_form_accel = (ACCEL *)malloc(10*sizeof(ACCEL));
	m_def_id = IDOK;
}

/***********************************************************************
c
c   SUBROUTINE:  init(CWnd* pParent, int *level, int cur_stage, int id, 
c			NpwDynWinStruct* winStruct )
c
c   FUNCTION:  This function initialize class data
c
c   INPUT:  CWnd* pParent : parent window
c			int *level	
c			int cur_stage:	
c			int id:		window id.
c			NpwDynWinStruct* winStruct: window structure of Dynamic form
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogPrompt::init(NpwDynWinStruct* winStruct )
{
	int i,k, gapy;
/*
.....save winStruct in class local data
*/
	m_DynWinStruct.winType = winStruct->winType;
	sprintf(m_DynWinStruct.winTitle,"%s",winStruct->winTitle);
	sprintf(m_DynWinStruct.winId, "%s", winStruct->winId);
	m_DynWinStruct.numButtons = winStruct->numButtons;
	for(i=0;i<winStruct->numButtons;i++)
	{
		sprintf(m_DynWinStruct.Buttonname[i],"%s",winStruct->Buttonname[i]); 
		m_DynWinStruct.ButtonId[i] = winStruct->ButtonId[i];
		m_DynWinStruct.Buttype[i] = winStruct->Buttype[i];
		sprintf(m_DynWinStruct.Butdefault[i],"%s",winStruct->Butdefault[i]);
		if(winStruct->Buttype[i]==NpwEChoiceB ||
			winStruct->Buttype[i]==NpwEDynChoiceB) /* choice button */
		{
			m_DynWinStruct.Butnumchoice[i] = winStruct->Butnumchoice[i];
			m_DynWinStruct.Butdefchoice[i] = winStruct->Butdefchoice[i];
			for(int j=0;j<winStruct->Butnumchoice[i];j++)
			{
				sprintf(m_DynWinStruct.Butchoicestring[i][j],"%s",winStruct->Butchoicestring[i][j]); 
			}
		}
		else
		{
			m_DynWinStruct.Butnumchoice[i] = 0;
			m_DynWinStruct.Butdefchoice[i] = 0;
		}
/*
.....Initialize the return structure
*/
		m_RetValStruct.ButtonId[i] = m_DynWinStruct.ButtonId[i];
		strcpy(m_RetValStruct.Butanswer[i], m_DynWinStruct.Butdefault[i]);
	}
/*
......Initialize the return value struct
*/
	sprintf(m_RetValStruct.winId, "%s",m_DynWinStruct.winId);
	m_RetValStruct.numButtons = m_DynWinStruct.numButtons;
/*
......setup DLGTEMPLATE structure
*/
	int style;
	int i1, j, current_y;
	int len = 0;
	current_y = 8;
	for( i1=0, j=0;j<winStruct->numButtons;i1++, j++)
	{
		switch(winStruct->Buttype[j])
		{
			case NpwEDynChoiceB:
			case NpwEChoiceB:
			{
				int len1, sizex, sizey;
				pw_getstr_size(m_pParent, winStruct->Buttonname[j], 80, Pw_promptdlg_font, &sizex, &sizey,1);
				gapy = sizey + 4;
				m_rgDlgItem[i1].setctl(2);
				style = WS_CHILD | WS_VISIBLE | SS_LEFT | WS_TABSTOP;
				m_rgDlgItem[i1].setId(i1);
				m_rgDlgItem[i1].settemp2(10, current_y, sizey, sizex, 0, style);
				m_rgDlgItem[i1].setcaption(winStruct->Buttonname[j]);
				len1 = sizex + LABEL_GAP;
				i1++;
/*
.....following is for choice
*/
				int len2 = 0;
				int tmp;
				for(k=0; k < winStruct->Butnumchoice[j]; k++)
				{
					pw_getstr_size(m_pParent, winStruct->Butchoicestring[j][k], 80, Pw_promptdlg_font, &sizex, &sizey,1);
					gapy = sizey + 4;
					if (sizex+LABEL_GAP>len2) len2 = sizex+LABEL_GAP;
				}
				m_rgDlgItem[i1].setctl(5);
				m_rgDlgItem[i1].setId(i1);
				style = WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL | WS_TABSTOP;
				m_rgDlgItem[i1].settemp2(len1 + 2, current_y, (k+1)*gapy, 
							len2+15, ID_CHOICE0+j, style);
				m_rgDlgItem[i1].setcaption(winStruct->Butchoicestring[j][0]);
				if (len < len1+len2+15)
					len = len1+len2+15;
				current_y += gapy;
				break;
			}
			case NpwETextB:
			{
				int len1, sizex, sizey;
				pw_getstr_size(m_pParent, winStruct->Buttonname[j], 80, Pw_promptdlg_font, &sizex, &sizey, 1);
				gapy = sizey + 4;
				m_rgDlgItem[i1].setctl(2);
				style = WS_CHILD | WS_VISIBLE | SS_LEFT | WS_TABSTOP;
				m_rgDlgItem[i1].setId(i1);
				m_rgDlgItem[i1].settemp2(10, current_y, sizey, sizex, 0,
								 style);
				m_rgDlgItem[i1].setcaption((LPCTSTR)winStruct->Buttonname[j]);
				len1 = sizex+LABEL_GAP;
				i1++;
/*
.....for edit text
*/
				int len2 = (strlen(winStruct->Butdefault[j])+2 > 5) ?
					strlen(winStruct->Butdefault[j])+2: 5;
				len2 = len2*4;
				m_rgDlgItem[i1].setctl(1);
				m_rgDlgItem[i1].setId(i1);
				style = WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | ES_LEFT |WS_BORDER;
				m_rgDlgItem[i1].settemp2(len1+2, current_y, sizey, len2+1,
					 ID_PRMP0+j, style);
				m_rgDlgItem[i1].setcaption(winStruct->Butdefault[j]);
				if (len < len1+len2+4)
					len = len1+len2+4;
				current_y += gapy;
				break;
			}
			case NpwELabelB:
			{
				int len1, sizex, sizey;
				pw_getstr_size(m_pParent, winStruct->Buttonname[j], 80, Pw_promptdlg_font, &sizex, &sizey, 1);
				gapy = sizey;
				len1 = sizex  + LABEL_GAP;
				m_rgDlgItem[i1].setctl(2);
				style = WS_CHILD | WS_VISIBLE | SS_LEFT | WS_TABSTOP;
				m_rgDlgItem[i1].setId(i1);
				m_rgDlgItem[i1].settemp2(10, current_y, sizey, len1, 0, style);
				m_rgDlgItem[i1].setcaption((LPCTSTR)winStruct->Buttonname[j]);
				if (len < len1+4)
					len = len1+4;
				current_y += gapy;
			}
		}	
	}
	int total = len+25 ;
	int ht = current_y;
/*
.....setup page size and style
*/
	m_dlgTempl.cx = total;
	m_dlgTempl.cy = ht;
	m_dlgTempl.style = WS_CHILD | DS_SETFONT | DS_CENTERMOUSE | WS_TABSTOP;
	m_dlgTempl.dwExtendedStyle = 0;
	m_dlgTempl.x = 100;
	m_dlgTempl.y = 100;
	m_dlgTempl.cdit = i1;
}

void CDialogPrompt::OnSize( UINT nType, int cx, int cy )
{
	CWnd::OnSize(nType, cx, cy );
	if (m_pScrollView!=NULL)
		SizeDialogItem(cx,cy);
}

void CDialogPrompt::SizeDialogItem(int cx, int cy )
{
	CRect windowRect, windowRect2;
	GetClientRect(windowRect);
	GetClientRect(windowRect2);

/*
......leave 22 unit for active bar area height
*/
	CRect temp (0,0, 22, 20);
	MapDialogRect(&temp);
	
	windowRect.bottom -= temp.Height();

	CWnd* pChildWnd = (CWnd*)GetDlgItem(IDC_SCROLL_FRAME);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(windowRect);
	else
		return;

	int butlen = (cx - 25)/4;
	windowRect2.top = windowRect.bottom + 8;
	windowRect2.bottom -= 5;
	windowRect2.right -= 5;
	windowRect2.left = windowRect2.right - butlen;

	pChildWnd = (CWnd*)GetDlgItem(ID_LHELP);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(windowRect2);

	windowRect2.right = windowRect2.left - 5;
	windowRect2.left = windowRect2.right - butlen;
	pChildWnd = (CWnd*)GetDlgItem(ID_APPLY);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(windowRect2);

	windowRect2.right = windowRect2.left - 5;
	windowRect2.left = windowRect2.right - butlen;
	pChildWnd = (CWnd*)GetDlgItem(IDCANCEL);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(windowRect2);

	windowRect2.right = windowRect2.left - 5;
	windowRect2.left = windowRect2.right - butlen;
	pChildWnd = (CWnd*)GetDlgItem(IDOK);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(windowRect2);

	CRect rect;
	GetDlgItem(IDC_SCROLL_FRAME)->GetWindowRect(&rect);
	ScreenToClient(rect);
	rect.left += 2;
	rect.right -= 2;
	rect.top += 2;
	rect.bottom -= 2;
	m_pScrollView->MoveWindow(&rect);
}
/***********************************************************************
c
c   SUBROUTINE:  OnAHelp
c
c   FUNCTION:  This function display a help dialog
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogPrompt::OnAHelp()
{
	if (m_pHelpDialog!=NULL)
		m_pHelpDialog->SetActiveWindow();
	else
	{
		m_pHelpDialog = new CDialogHelp(this, 1);
		((CDialogHelp*)m_pHelpDialog)->sethelp(&m_DynWinStruct);
		((CDialogHelp*)m_pHelpDialog)->inittemp();
		((CDialogHelp*)m_pHelpDialog)->Create();
	}		
}

/***********************************************************************
c
c   SUBROUTINE:  SaveData
c
c   FUNCTION:  This function save every field data 
c				in the Prompt Dialog
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

int CDialogPrompt::SaveData()
{
	char pText[80], msg[82];
	int id, butPos, retVal;
/*
.....The return value struct is updated for all choice
.....buttons through the choice button callback 
.....We will just loop through the text control
.....and get and copy text field data into the return struct 
*/
	for(int i=0; i<m_DynWinStruct.numButtons;i++)
	{
		if(m_DynWinStruct.Buttype[i]==NpwETextB)
		{
			id = ID_PRMP0 + i;
			
			if (m_pScrollView->GetDlgItem(id)->GetWindowText(pText, 80))
			{
				sprintf(m_RetValStruct.Butanswer[i], "%s", pText);
			}
			else
			{
				sprintf(m_RetValStruct.Butanswer[i], "%s", "");
			}
		}
		if (strcmp(m_RetValStruct.Butanswer[i], m_DynWinStruct.Butdefault[i])!=0)
			MpChanged = 1;
	}
	retVal = NpwPutAnswer(&m_RetValStruct,&butPos,msg);
	if(retVal)
	{
/*
......We have an error
*/
		 MessageBox(msg, "Error!", MB_OK);		 
		  return 0;
	}
	return 1;
}
LRESULT CDialogPrompt::OnViewChoicePicked(WPARAM wparm, LPARAM lparm)
{
	UINT id = (UINT)wparm;
	OnChoicePicked(id);
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  OnChoicePicked(UINT id)
c
c   FUNCTION:  This function called when choce button selected
c
c   INPUT:  num:  choice num
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogPrompt::OnChoicePicked(UINT id)
{
	char data[80];
	int num;
	int len;
	NpwDynFormStruct formStruct;
	NpwDynWinStruct winStruct;
	char message[82];

	num = id - ID_CHOICE0;
	if ((m_DynWinStruct.Buttype[num]==NpwEDynChoiceB)||
		(m_DynWinStruct.Buttype[num]==NpwEChoiceB))
	{
		len = (m_pScrollView->GetDlgItem(id))->GetWindowText(data, 80);
	}
	data[len] = '\0';
	m_RetValStruct.ButtonId[num]= m_DynWinStruct.ButtonId[num];
	sprintf(m_RetValStruct.Butanswer[num],"%s",data);
/*
......Dynamic Field
......Relay entire another prompt window
*/
	if (m_DynWinStruct.Buttype[num] == NpwEDynChoiceB &&
		strcmp(m_RetValStruct.Butanswer[num],
			m_DynWinStruct.Butdefault[num]) != 0)
	{
		if (SaveData())
		{
/*
.....destroy current view and create a new one
*/
			if (m_pScrollView!=NULL)
				delete m_pScrollView;
			m_pScrollView = NULL;
			int j = m_curStage-1;
			int result = NpwGetWinLayout(m_pLevel,&j,
					&winStruct,&formStruct, message);
			init(&winStruct);

			CCreateContext cc;

			cc.m_pNewViewClass = RUNTIME_CLASS(CPWScrollView);
			cc.m_pCurrentDoc = NULL;
			cc.m_pNewDocTemplate = NULL;
			cc.m_pLastView = NULL;
			cc.m_pCurrentFrame = NULL;
			m_pScrollView = (CPWScrollView*)CreateDlgView(&cc, this, CRect(0, 0, 0, 0), 0, &m_dlgTempl, m_rgDlgItem);
			if (m_pScrollView == NULL)
				EndDialog(IDCANCEL);

			CRect rect;
			GetDlgItem(IDC_SCROLL_FRAME)->GetWindowRect(&rect);
			ScreenToClient(rect);
			rect.left += 2;
			rect.right -= 2;
			rect.top += 2;
			rect.bottom -= 2;

			m_pScrollView->MoveWindow(&rect);
	
			SetWindowText(m_DynWinStruct.winTitle);
			m_pScrollView->initview(m_DynWinStruct);
/*
.....I think initial add SetFocus here to reset back the focus after form redisplay
.....but in some case, the new updated form don't have same form ID for that item anymore
.....so check if the window is there before set focus, otherwise will cause error
*/
			CWnd *win = m_pScrollView->GetDlgItem(id);
			if (win)
				m_pScrollView->GetDlgItem(id)->SetFocus();
		}
	}
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

BOOL CDialogPrompt::OnInitDialog()
{
	CDialog::OnInitDialog();
	SetIcon(m_hIcon, TRUE);			// Set big icon

	CCreateContext cc;

	cc.m_pNewViewClass = RUNTIME_CLASS(CPWScrollView);
	cc.m_pCurrentDoc = NULL;
	cc.m_pNewDocTemplate = NULL;
	cc.m_pLastView = NULL;
	cc.m_pCurrentFrame = NULL;
	m_pScrollView = (CPWScrollView*)CreateDlgView(&cc, this, CRect(0, 0, 0, 0), 0, &m_dlgTempl, m_rgDlgItem);
	if (m_pScrollView == NULL)
		EndDialog(IDCANCEL);

	CRect windowRect, ScrollRect;
	m_pScrollView->GetWindowRect(&ScrollRect);

	CRect rect, rect1, rect2;
	int borderx, bordery;
	GetWindowRect(&rect1);
	GetDlgItem(IDC_SCROLL_FRAME)->GetWindowRect(&rect);
	borderx = rect.left - rect1.left;
	bordery = rect.top - rect1.top;

	SystemParametersInfo(SPI_GETWORKAREA,0,rect2,0);
	int maxhgt = rect2.Height() - 100;
	int maxwid = rect2.Width() - borderx;
	if (ScrollRect.Height()>maxhgt)
	{
		ScrollRect.bottom = ScrollRect.top + maxhgt;
		ScrollRect.right += GetSystemMetrics(SM_CXVSCROLL);
	}
	if (ScrollRect.Width()>maxwid)
		ScrollRect.right = ScrollRect.left + maxwid;

	ScreenToClient(rect);
	rect.right = rect.left + ScrollRect.Width() + 4;
	rect.bottom = rect.top + ScrollRect.Height() + 4;

	windowRect = rect;
	GetDlgItem(IDC_SCROLL_FRAME)->MoveWindow(&rect);

	rect.left += 2;
	rect.top += 2;
	rect.right -= 2;
	rect.bottom -= 2;
	m_pScrollView->MoveWindow(&rect);
/*
......leave 22 unit for active bar area height
*/
	CRect temp (0,0, 25, 25);
	MapDialogRect(&temp);

	windowRect.bottom += temp.Height() + bordery;

	windowRect.right += borderx;
	MoveWindow(windowRect);

	SetWindowText(m_DynWinStruct.winTitle);
	
	m_form_accel[0].cmd = ID_FORM_TABED;
	m_form_accel[0].fVirt = FNOINVERT | FVIRTKEY;
	m_form_accel[0].key = VK_TAB;
	m_form_accel[1].cmd = ID_FORM_STABED;
	m_form_accel[1].fVirt = FNOINVERT | FVIRTKEY | FSHIFT;
	m_form_accel[1].key = VK_TAB;
	m_accel = CreateAcceleratorTable(m_form_accel, 2);

	m_pScrollView->initview(m_DynWinStruct);
	UINT id;
	CWnd *win;
	for(int j=0;j<m_DynWinStruct.numButtons;j++)
	{
		if(m_DynWinStruct.Buttype[j]==NpwETextB)
		{
			id = ID_PRMP0+j;			
		}
		else if (m_DynWinStruct.Buttype[j]!=NpwELabelB)
		{
			id = ID_CHOICE0+j;
		}
		else
			continue;
		win = m_pScrollView->GetDlgItem(id);
		if (win!=NULL)
		{
			win->SetFocus();
			return 0;
		}
	}
	return 1;
}
/***********************************************************************
c
c   SUBROUTINE:  Remove_Help
c
c   FUNCTION:  This function Remove help dialog (pointer from
c				propertySheet (but not delete it and destroy it
c				it destroy and delete by Help dialog itself.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogPrompt::Remove_Help()
{
	m_pHelpDialog = NULL;
}


/***********************************************************************
c
c   SUBROUTINE:  OnApply
c
c   FUNCTION:  This function called when "Apply" button pushed.
c				it save data
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogPrompt::OnApply()
{
	SaveData();
}

/***********************************************************************
c
c   SUBROUTINE:  OnOK
c
c   FUNCTION:  This function called when "OK" button pushed.
c				it save data and destroy window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogPrompt::OnOK()
{
	SaveData();
		if (m_pHelpDialog!=NULL)
	{
		((CDialogHelp*)m_pHelpDialog)->DlgCancel();
		m_pHelpDialog = NULL;
	}
	((CDialogButton*)m_pParent)->Remove_Child(m_ChildId);
	DestroyWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  OnCancel
c   FUNCTION:  This function called when "Cancel" button pushed
c				it destroy and remove child window and itself
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogPrompt::OnCancel() 
{
	if (m_pHelpDialog!=NULL)
	{
		((CDialogHelp*)m_pHelpDialog)->DlgCancel();
		m_pHelpDialog = NULL;
	}	
	((CDialogButton*)m_pParent)->Remove_Child(m_ChildId);
	DestroyWindow();
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

void CDialogPrompt::PostNcDestroy() 
{
	delete this;
}


/********
***************************************************************
c
c   SUBROUTINE:  DlgOK
c
c   FUNCTION:  This function simply call OnOK because OnOK
c				is protected function
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogPrompt::DlgOK()
{
	OnOK();
}

/***********************************************************************
c
c   SUBROUTINE:  DlgCancel
c
c   FUNCTION:  This function simply call OnCancel because OnCancel
c				is protected function
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogPrompt::DlgCancel()
{
	OnCancel();
}
/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks1(UINT id)
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
LRESULT CDialogPrompt::FormUserCallbacks1(WPARAM wparm, LPARAM lparm)
{
	UINT id = (UINT)wparm;
	int i, fldno, stat, flag;
	CWnd *old_def, *new_def;
	int redisp = 0;
/*
.....do not call callback function until form displayed
*/
//	if (m_init==0)
//		return 0;
	fldno = id - ID_PRMP0;
/*	Focused_frm_fldno = fldno;
	
	if (m_type[fldno]==3)
	{
		m_actedit = (CEdit*)(m_pScrollView->GetDlgItem(id));
//		if (UW_frmtext_select)
			PostMessage(WM_COMMAND, ID_FORM_HIGHLIGHT);
	}
*/
/*
......reset default button to OK
*/
/*	old_def = GetDlgItem(m_def_id);
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
*/
	return 0;
}
/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks2(UINT id)
**       COMB box and button Lose Focus callback for all fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
LRESULT CDialogPrompt::FormUserCallbacks2(WPARAM wparm, LPARAM lparm)
{
	CWnd *old_def, *new_def;
	UINT id = (UINT)wparm;
	int fldno, stat, redisp=0;
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  FormUserCallbacks3(UINT id)
**       COMB box and button Focus callback for all fields in the form.
**    PARAMETERS   
**       INPUT  : 
**				id: field ID                       
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
LRESULT CDialogPrompt::FormUserCallbacks3(WPARAM wparm, LPARAM lparm)
{
	CWnd *old_def, *new_def;
	UINT id = (UINT)wparm;
	int fldno, stat, redisp=0;
/*
.....do not call callback function until form displayed
*/
//	if (m_init==0)
//		return 0;
/*
......only if form exist
*/
//	if (UD_dispfrm[m_frmid] == NULL) return 0;

	fldno = id - ID_CHOICE0;
/*	if ((m_DynWinStruct.Buttype[fldno]==NpwEDynChoiceB)||
		(m_DynWinStruct.Buttype[fldno]==NpwEChoiceB))
	{
		len = (m_pScrollView->GetDlgItem(id))->GetWindowText(data, 80);
	}

	Focused_frm_fldno = fldno;
*/	
	return 0;
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
BOOL CDialogPrompt::PreTranslateMessage(MSG* pMsg) 
{
	int stat = 0;
	HWND hWnd = (HWND)*this; 
	if (hWnd==NULL) return 0;
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
		m_return_key = 0;
		return TRUE;
	}
	else
	{
		return CDialog::PreTranslateMessage( pMsg );
		m_return_key = 0;
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
void CDialogPrompt::OnFormTabbed()
{
	int i,k, count;
	CWnd *PwndNext=NULL, *tmpwin, *cwin, *dwin;
	CWnd *first=NULL, *last=NULL;
	int first_item;
	UINT did = 0, def_id = 0;
	int def_chg = 1;
	CWnd *old_def, *new_def;
	POINT pt; 
	UINT id;
	
	for(k=0; k<m_DynWinStruct.numButtons;k++)
	{
		if(m_DynWinStruct.Buttype[k]==NpwETextB)
		{
			id = ID_PRMP0 + k;
		}
		else
			id = ID_CHOICE0 + k;

		tmpwin = m_pScrollView->GetDlgItem(id);
		if (tmpwin==NULL) 
			continue;
		if (tmpwin->IsWindowEnabled()) 
		{
			if (first==NULL)
			{
				first = tmpwin;
				first_item = k;
			}
			last = tmpwin;
		}
	}
	CWnd *win = GetFocus();
	if (win==GetDlgItem(ID_LHELP))
	{
		PwndNext = first;
		did = ID_LHELP;
	}
	else if (win==GetDlgItem(IDOK))
	{
		did = IDOK;
		PwndNext = GetDlgItem(IDCANCEL);
		def_chg = 0;
	}
	else if (win==GetDlgItem(IDCANCEL))
	{
		did = IDCANCEL;
		PwndNext = GetDlgItem(ID_APPLY);
	}
	else if (win==GetDlgItem(ID_APPLY))
	{
		did = ID_LHELP;
		PwndNext = GetDlgItem(ID_LHELP);
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
			PwndNext = (CWnd*)GetDlgItem(IDOK);
		}
		else
		{
/*
......this GetNextDlgTabItem function is not working, that's why we need handle the tab
......by ourselvies
*/
//			PwndNext = m_pScrollView->GetNextDlgTabItem(win);
			for(k=0; k<m_DynWinStruct.numButtons;k++)
			{
				if(m_DynWinStruct.Buttype[k]==NpwETextB)
				{
					id = ID_PRMP0 + k;
				}
				else
					id = ID_CHOICE0 + k;
				cwin = m_pScrollView->GetDlgItem(id);
				pt.x = 10; 
				pt.y = 10; 
				if (cwin!=NULL)
					dwin = cwin->ChildWindowFromPoint(pt);
				else
					dwin = NULL;
				if ((win==cwin) || (win==dwin))
				{
/*
......get next item
*/
get_next_item:;
					if(m_DynWinStruct.Buttype[k+1]==NpwETextB)
					{
						id = ID_PRMP0 + k + 1;
					}
					else
						id = ID_CHOICE0 + k + 1;
					PwndNext = m_pScrollView->GetDlgItem(id);
					if (PwndNext!=NULL)
					{
						if (PwndNext->IsWindowEnabled()) 
						{
							break;
						}
						else
						{
							win = PwndNext;
							count++;
							goto get_next;
						}
					}
					else
					{
						k++;
						goto get_next_item;
					}
				}
			}
		}
	}
	if (PwndNext)
	{
		SendMessage(WM_NEXTDLGCTL, (WPARAM)PwndNext->GetSafeHwnd(), TRUE);
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
void CDialogPrompt::OnFormSTabbed()
{
	int i,k, count;
	CWnd *PwndPr, *tmpwin, *cwin, *dwin;
	CWnd *first=NULL, *last=NULL;
	int last_item;
	int def_chg = 1;
	UINT did = 0, def_id = 0;
	CWnd *old_def, *new_def;
	POINT pt; 
	UINT id;

	for(k=0; k<m_DynWinStruct.numButtons;k++)
	{
		if(m_DynWinStruct.Buttype[k]==NpwETextB)
		{
			id = ID_PRMP0 + k;
		}
		else
			id = ID_CHOICE0 + k;

		tmpwin = m_pScrollView->GetDlgItem(id);
		if (tmpwin==NULL) 
			continue;
		if (tmpwin->IsWindowEnabled()) 
		{
			if (first==NULL)
			{
				first = tmpwin;
			}
			last = tmpwin;
			last_item = k;
		}
	}
	CWnd *win = GetFocus();
	count = 0;

	if (win==GetDlgItem(IDOK))
	{
		did = IDOK;
		PwndPr = last;
	}
	else if (win==GetDlgItem(IDCANCEL))
	{
		did = IDCANCEL;
		PwndPr = GetDlgItem(IDOK);
	}
	else if (win==GetDlgItem(ID_APPLY))
	{
		did = ID_APPLY;
		PwndPr = GetDlgItem(IDCANCEL);
	}
	else if (win==GetDlgItem(ID_LHELP))
	{
		did = ID_LHELP;
		PwndPr = GetDlgItem(ID_APPLY);
	}
	else if (win==first)
	{
		PwndPr = (CWnd*)GetDlgItem(ID_LHELP);
	}
	else
	{
get_next:;
		if (count > 500)
		{
			PwndPr = NULL;
			return;
		}
		if (win==first)
		{
			PwndPr = (CWnd*)GetDlgItem(ID_LHELP);
		}
		else
		{
			for(int k=0; k<m_DynWinStruct.numButtons;k++)
			{
				if(m_DynWinStruct.Buttype[k]==NpwETextB)
				{
					id = ID_PRMP0 + k;
				}
				else
					id = ID_CHOICE0 + k;
				cwin = m_pScrollView->GetDlgItem(id);
				pt.x = 10; 
				pt.y = 10; 
				if (cwin!=NULL)
					dwin = cwin->ChildWindowFromPoint(pt);
				else
					dwin = NULL;
				if ((win==cwin) || (win==dwin))
				{
/*
......get next item
*/
get_next_item:;
	
					if(m_DynWinStruct.Buttype[k-1]==NpwETextB)
					{
						id = ID_PRMP0 + k - 1;
					}
					else
						id = ID_CHOICE0 + k - 1;
					PwndPr = m_pScrollView->GetDlgItem(id);
					if (PwndPr!=NULL)
					{
						if (PwndPr->IsWindowEnabled()) 
						{
							break;
						}
						else
						{
							win = PwndPr;
							count++;
							goto get_next;
						}
					}
					else
					{
						k++;
						goto get_next_item;
					}
				}
			}
		}
	}
	if (PwndPr)
	{
		SendMessage(WM_NEXTDLGCTL, (WPARAM)PwndPr->GetSafeHwnd(), TRUE);
	}
}

/* End File DialogPrompt.cpp */
