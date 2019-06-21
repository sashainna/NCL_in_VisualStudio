/************************************************************************
c
c   FILE NAME: DialogForm.cpp
c
c	 CONTAINS: 
c	 all CDialogForm class override functions and 
c			Implementation functions
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        DialogForm.cpp , 24.3
c     DATE AND TIME OF LAST  MODIFICATION
c        04/08/14 , 08:21:00
c
c**********************************************************************
*/

#include "Pwstdafx.h"
#include "Mpost.h"
#include "DialogForm.h"
#include "DialogTemplate.h"
#include "DialogHelp.h"
#include "mpostres.h"
#include "NpwHeaders.h"

static int TOTALITEMS;
extern int MpChanged;
char Pw_formdlg_font[132];
extern void pw_getavr_chrsize(CWnd *win, int pt, char *fntname, int *wid, int* hgt);
extern void pw_getstr_size(CWnd *win, char *string, int pt, char *fntname, int *wid, int* hgt, int flag);

/***********************************************************************
c
c   SUBROUTINE:  RemoveSpaces(char *cbuf,int *knc)
c
c   FUNCTION:  This function remove space from string
c
c   INPUT:  char *cbuf: string to remove space
c			int *knc: string length after remove space from string
c   OUTPUT: none
c
c***********************************************************************
*/

void RemoveSpaces(char *cbuf,int *knc)
{
	int nc,i;
	nc = strlen(cbuf);
	if (nc > 0)
	{
		for (i=nc-1;i>=0;i--) if (cbuf[i] != ' ') break;
		*knc = i + 1;
		cbuf[*knc] = '\0';
	}
}

/***********************************************************************
c
c   SUBROUTINE:  AddSpaces(char *cbuf,int knc)
c
c   FUNCTION:  This function remove space from string
c
c   INPUT:  char *cbuf: string to add space
c			int knc: expected string length.
c   OUTPUT: none
c
c***********************************************************************
*/

void AddSpaces(char *cbuf,int knc)
{
	int nc,i;
	nc = strlen(cbuf);
	if (nc < knc-1)
	{
		for (i=nc;i<knc-1;i++) cbuf[i] = ' ';
		cbuf[knc-1] = '\0';
	}
}


/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/

BEGIN_MESSAGE_MAP(CDialogForm, CDialog)
	//{{AFX_MSG_MAP(CDialogForm)
	ON_COMMAND(ID_APPLY, OnApply)
	ON_COMMAND(ID_LHELP, OnAHelp)
	ON_COMMAND(ID_INSERT, OnInsert)
	ON_COMMAND(ID_DELETE, OnDelete)
	ON_LBN_SELCHANGE(ID_LIST, OnPicked)
	ON_CONTROL_RANGE(CBN_SELCHANGE, ID_CHOICE0, ID_CHOICE39, OnChoicePicked)
	ON_CONTROL_RANGE(EN_SETFOCUS, ID_PRMP0, ID_PRMP99, OnInputFocus)
	ON_WM_SIZE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  CDialogForm
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			int *level	
c			int cur_stage:	
c			int id:		window id.
c			
c   OUTPUT: none
c
c***********************************************************************
*/

CDialogForm::CDialogForm(CWnd* pParent, int *level, int cur_stage, int id)	
{
	m_pParent = pParent;
	m_curStage = cur_stage;
	m_ChildId = id;
	for(int i=0;i<cur_stage;i++)
		m_pLevel[i] = level[i];
	CDialog::CDialog();
	m_pHelpDialog = NULL;

	if (m_listfont.m_hObject)
		VERIFY (m_listfont.DeleteObject ());	
//	m_listfont.CreateFont(12, 7,0,0,500, FALSE, FALSE, FALSE, 
//				DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
//				DEFAULT_QUALITY, DEFAULT_PITCH, "Lucida Console");
	VERIFY (m_listfont.CreatePointFont (100, "COURIER"));
	m_hIcon = AfxGetApp()->LoadIcon(IDI_DLGICON);
	m_init = 0;
}

/***********************************************************************
c
c   SUBROUTINE:  initform(NpwDynWinStruct* winStruct, npwDynFormStruct* formStruct)
c
c   FUNCTION:  This function initialize class data
c
c   INPUT:  
c			NpwDynWinStruct* winStruct: window structure of Dynamic form
c			npwDynFormStruct* formStruct: form structure of Dynamic form
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogForm::initform(NpwDynWinStruct* winStruct, npwDynFormStruct* formStruct)
{
	int i,j,k,total, num, len1, ht, tmp, Nlin[4], len, nc, start,frmwid;
	DWORD style;
	char sbuf[132];
	int sizex, sizey, gapy, char_sizex, char_sizey;
	int nc1,list_hgt;
/*
......Set line breaks
*/
	Nlin[0] = 0;
	Nlin[1] = formStruct->totalFields;
	Nlin[2] = Nlin[1] * 2;
	Nlin[3] = Nlin[2] + Nlin[1];
/*
.....save winStruct and formStruct in class local data
*/
	m_DynWinStruct.winType = winStruct->winType;
	sprintf(m_DynWinStruct.winTitle,"%s",winStruct->winTitle);
	sprintf(m_DynWinStruct.winId, "%s", winStruct->winId);

	m_DynFormStruct.numRecords = formStruct->numRecords;
	m_DynFormStruct.numFields = formStruct->numFields;
	m_DynFormStruct.totalFields = formStruct->totalFields;
	m_DynFormStruct.numChoiceFields = formStruct->numChoiceFields;
	len = 0;
	frmwid = strlen(formStruct->recordString[0]) + 4;
	if (frmwid < 80) frmwid = 80;
	nc = 132;
	for (i=0;i<formStruct->numRecords;i++)
	{
		strcpy(m_DynFormStruct.recordString[i],formStruct->recordString[i]);
		AddSpaces(m_DynFormStruct.recordString[i],nc);
		tmp = strlen(m_DynFormStruct.recordString[i]);
		if (len<tmp) len = tmp;
	}
	for (i=0;i<formStruct->numFields;i++)
	{
		strcpy(m_DynFormStruct.fieldLabel[i],formStruct->fieldLabel[i]);
		m_DynFormStruct.fieldStart[i] = formStruct->fieldStart[i];
		m_DynFormStruct.fieldEnd[i] = formStruct->fieldEnd[i];
		m_DynFormStruct.fieldType[i] = formStruct->fieldType[i];
		if (formStruct->fieldType[i] == 2)
		{
			m_DynFormStruct.numChoice[i] = formStruct->numChoice[i];
			for (j=0;j<formStruct->numRecords;j++)
			{
				m_DynFormStruct.defaultChoice[j][i] = formStruct->defaultChoice[j][i];
			}
			for (j=0;j<formStruct->numChoice[i];j++)
			{
				strcpy(m_DynFormStruct.choiceString[i][j],
					formStruct->choiceString[i][j]);
			}
		}
	}
	pw_getavr_chrsize(m_pParent, frmwid, "Courier", &char_sizex, &char_sizey);
/*
......Initialize the return value struct
*/
	sprintf(m_RetValStruct.winId, "%s",m_DynWinStruct.winId);
	if (m_DynWinStruct.winType == 4 || m_DynWinStruct.winType == 5)
	{
		initTable(len, ht, num);
		goto initpanel;
	}

	m_rgDlgItem[0].setctl(3);
	m_rgDlgItem[0].setId(0);
	style = WS_CHILD | WS_VISIBLE |WS_BORDER | WS_VSCROLL | WS_HSCROLL | LBS_HASSTRINGS |LBS_DISABLENOSCROLL|LBS_NOTIFY;

	list_hgt = char_sizey*10 + 21;
	m_rgDlgItem[0].settemp2(10,8,list_hgt,frmwid*char_sizex + 10,ID_LIST,style);
	m_rgDlgItem[0].setcaption("");
	currentListPos = 0;

	pw_getavr_chrsize(m_pParent, frmwid, "Courier", &char_sizex, &char_sizey);

	pw_getstr_size(m_pParent, "Xx", frmwid, Pw_formdlg_font, &sizex, &sizey,0);
	gapy = sizey + 4;
	for(i=1, j=0;j<m_DynFormStruct.numFields;i++, j++)
	{
/*
......Start a new line, reset start point
*/
		for (int m = 0; m<4; m++)
		{
			if (j == Nlin[m]) 
			{
				start = 10;
				ht = m*gapy + list_hgt + 10;
			}
		}
		switch(m_DynFormStruct.fieldType[j])
		{
/*
......Multiple Choice Button
*/
			case NpwEChoiceB:
			{
				int len1 = 0;
				int len2 = 0;
				pw_getstr_size(m_pParent, m_DynFormStruct.fieldLabel[j], 80, Pw_formdlg_font, &sizex, &sizey,0);
				len1 = sizex;
				if (len1>0)
				{
					m_rgDlgItem[i].setctl(2);
					style = WS_CHILD | WS_VISIBLE | SS_LEFT;
					m_rgDlgItem[i].setId(i);
					m_rgDlgItem[i].settemp2(start, ht, gapy, len1, 0, style);
					m_rgDlgItem[i].setcaption((LPCTSTR)m_DynFormStruct.fieldLabel[j]);
				}
				if (len1!=0)
					start = start + len1;
				i++;

				for(k=0; k < m_DynFormStruct.numChoice[j]; k++)
				{
					pw_getstr_size(m_pParent, m_DynFormStruct.choiceString[j][k], 80, Pw_formdlg_font, &sizex, &sizey,0);
					if (sizex>len2) len2 = sizex;
				}
				m_rgDlgItem[i].setctl(5);
				m_rgDlgItem[i].setId(i);
				style = WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST| WS_TABSTOP;
				m_rgDlgItem[i].settemp2(start, ht, (k+1)*gapy,len2+15, ID_CHOICE0+j, style);
				m_rgDlgItem[i].setcaption(m_DynFormStruct.fieldLabel[j]);
				start = start + len2 + 15 + 10;
/*
.......Now update the retValue struct with the default answer
*/
				m_RetValStruct.ButtonId[j]= m_DynFormStruct.defaultChoice[0][j];
				sprintf(m_RetValStruct.Butanswer[j],"%s",
					m_DynFormStruct.choiceString[j]
						[m_DynFormStruct.defaultChoice[0][j]]); 
				break;
			}
			case NpwELabelB:
			{
/*
.....prompt
*/
				pw_getstr_size(m_pParent, m_DynFormStruct.fieldLabel[j], 80, Pw_formdlg_font, &sizex, &sizey,0);
				len1 = sizex;
				m_rgDlgItem[i].setctl(2);
				style = WS_CHILD | WS_VISIBLE | SS_LEFT;
				m_rgDlgItem[i].setId(i);
				m_rgDlgItem[i].settemp2(start, ht, gapy, len1, 0, style);
				m_rgDlgItem[i].setcaption((LPCTSTR)m_DynFormStruct.fieldLabel[j]);
				if (len1!=0)
					start = start + len1;
				i++;
/*
......actual label
*/
				nc = m_DynFormStruct.fieldEnd[j] -  m_DynFormStruct.fieldStart[j]
					+ 1;
				strncpy(sbuf,&m_DynFormStruct.recordString[j]
							[m_DynFormStruct.fieldStart[j]],nc);
				sbuf[nc] = '\0';
				RemoveSpaces(sbuf,&nc1);
				pw_getstr_size(m_pParent, sbuf, frmwid, Pw_formdlg_font, &sizex, &sizey,0);
				m_rgDlgItem[i].setctl(2);
				style = WS_CHILD | WS_VISIBLE | SS_LEFT;
				m_rgDlgItem[i].setId(i);
				m_rgDlgItem[i].settemp2(start, ht, sizey, sizex+2, ID_PRMP0+j, style);
				m_rgDlgItem[i].setcaption((LPCTSTR)sbuf);
				start = start + sizex + 10;
/*
......Now update the retValue struct with the default answer
*/
				m_RetValStruct.ButtonId[j]= j;
				strcpy(m_RetValStruct.Butanswer[j],sbuf);
				break;
			}
/*
......Text Entry
......Create a label and Text Entry field
*/
			case NpwETextB:
			{
/*
......Create the text label (prompt)
*/
				pw_getstr_size(m_pParent, m_DynFormStruct.fieldLabel[j], 80, Pw_formdlg_font, &sizex, &sizey,0);
				len1 = sizex;
				m_rgDlgItem[i].setctl(2);
				style = WS_CHILD | WS_VISIBLE | SS_LEFT;
				m_rgDlgItem[i].setId(i);
				m_rgDlgItem[i].settemp2(start, ht, gapy, len1, 0, style);
				m_rgDlgItem[i].setcaption((LPCTSTR)m_DynFormStruct.fieldLabel[j]);
				if (len1!=0)
					start = start + len1;
				i++;
/*
.....for edit text
*/
				nc = m_DynFormStruct.fieldEnd[j] -  m_DynFormStruct.fieldStart[j]
					+ 1;
				strncpy(sbuf,&(m_DynFormStruct.recordString[0]
					[m_DynFormStruct.fieldStart[j]]),nc);
				sbuf[nc] = '\0';
				if (sbuf[0]!='\0')
					pw_getstr_size(m_pParent, sbuf, frmwid, Pw_formdlg_font, &sizex, &sizey,0);				
				else
					sizex = 10;
				RemoveSpaces(sbuf,&nc1);
/*
.....added 2 for edit box border
*/
				sizex += 2;
				m_rgDlgItem[i].setctl(1);
				m_rgDlgItem[i].setId(i);
				style = 
		WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | ES_LEFT |WS_BORDER | WS_TABSTOP;
				m_rgDlgItem[i].settemp2(start, ht, gapy-2, sizex, ID_PRMP0+j, style);
				m_rgDlgItem[i].setcaption(sbuf);
				start = start + sizex + 10;
/*
.......Now update the retValue struct with the default answer
*/
				m_RetValStruct.ButtonId[j]= j;
				sprintf(m_RetValStruct.Butanswer[j],"%s",
					sbuf); 

				break;
			}
		}

	}
	num = i-1;
	len = frmwid*char_sizex + 10 + 10;
initpanel:
	num++;
	ht = ht + 10;
	total =  len + 10;
/*
.....init group box
*/
	m_rgDlgItem[num].InitBox(5, 5, total-5, ht+ 10);
	num++;
/*
.....init panel button
*/
	int offset = 5;
	int butlen = 38;
	int end_x = total-5;
	int bg_x = end_x - butlen;
	int bg_y = ht+12;
	int end_y = ht +27;

	m_rgDlgItem[num++].Initpanel("Help", ID_LHELP,bg_x, bg_y, end_x, end_y);
	end_x = bg_x - offset;
	bg_x = end_x - butlen;
	m_rgDlgItem[num++].Initpanel("Apply", ID_APPLY, bg_x, bg_y, end_x, end_y);
	end_x = bg_x - offset;
	bg_x = end_x - butlen;
	m_rgDlgItem[num++].Initpanel("Cancel", IDCANCEL,bg_x, bg_y, end_x, end_y);
	end_x = bg_x - offset;
	bg_x = end_x - butlen;
	m_rgDlgItem[num++].Initpanel("OK",IDOK,bg_x, bg_y, end_x, end_y);


	TOTALITEMS = num;
	m_dlgTempl.cx = total;
	m_dlgTempl.cy = ht +30;
	m_dlgTempl.style =
	 WS_CAPTION | WS_VISIBLE | WS_DLGFRAME | WS_POPUP |WS_SYSMENU | WS_MINIMIZEBOX | DS_SETFONT | WS_THICKFRAME;
	m_dlgTempl.dwExtendedStyle = 0;
	m_dlgTempl.x = 100;
	m_dlgTempl.y = 100;
	m_dlgTempl.cdit = num;
}

/***********************************************************************
c
c   SUBROUTINE:  Create()
c
c   FUNCTION:  This function Create Dynamic Form
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

BOOL CDialogForm::Create()
{
	
	CString TitleCaption = m_DynWinStruct.winTitle;
	WCHAR*	szBoxCaption;
	int		nChars, nActualChars, nfChars;
	WCHAR* szFontName; 

	nChars = strlen(Pw_formdlg_font) + 1;
	szFontName = new WCHAR[nChars];
	nfChars = MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, Pw_formdlg_font, nChars, 
							szFontName, nChars);

	nChars = TitleCaption.GetLength() + 1;
	szBoxCaption = new WCHAR[nChars];
	nActualChars = MultiByteToWideChar(CP_ACP, 0, TitleCaption, -1, 
							szBoxCaption, nChars);
	ASSERT(nActualChars > 0);
/*
......We will first convert the control captions to UNICODE
*/
	int		nTotalLength = 0;  
	int		i;

/*
...... catch memory exceptions and don't worry about allocation failures
*/
	TRY  
	{
		int nBufferSize =  sizeof(DLGTEMPLATE) + (2*sizeof(WORD))
													 + nActualChars * sizeof(WCHAR);
		nBufferSize += sizeof(WORD) + nfChars * sizeof(WCHAR); 

		nBufferSize = (nBufferSize + 3) & ~3; 

		for (i = 0; i < TOTALITEMS; i++)
		{
			int nItemLength = sizeof(DLGITEMTEMPLATE) + 3 * sizeof(WORD);
			nItemLength += (m_rgDlgItem[i].m_strCaption.GetLength() + 1)
												 * sizeof(WCHAR);

			if (i != TOTALITEMS -1 )  
/*
...... take into account gap so next control is DWORD aligned
...... but the last control does not need extra bytes
*/
				nItemLength = (nItemLength + 3) & ~3;  

			nBufferSize += nItemLength;
		}


		HLOCAL hLocal = LocalAlloc(LHND, nBufferSize);
		if (hLocal == NULL)
			AfxThrowMemoryException();

		BYTE*	pBuffer = (BYTE*)LocalLock(hLocal);
		if (pBuffer == NULL)
		{
			LocalFree(hLocal);
			AfxThrowMemoryException();
		}

		BYTE*	pdest = pBuffer;
/*
......transfer DLGTEMPLATE structure to the buffer
*/
		memcpy(pdest, &m_dlgTempl, sizeof(DLGTEMPLATE));
		pdest += sizeof(DLGTEMPLATE);
		*(WORD*)pdest = 0; // no menu
/*
.....use default window class
*/
		*(WORD*)(pdest + 1) = 0;  // use default window class
		pdest += 2 * sizeof(WORD);

		memcpy(pdest, szBoxCaption, nActualChars * sizeof(WCHAR));
		pdest += nActualChars * sizeof(WCHAR);
		delete szBoxCaption ;
		*(WORD*)pdest = 8;//font size
		pdest += sizeof(WORD);

		memcpy(pdest, szFontName, nfChars * sizeof(WCHAR));
		pdest += nfChars * sizeof(WCHAR);
		delete szFontName ;
/* 
......We will now transfer the information for each one of the item templates
*/
		for (i = 0; i < TOTALITEMS; i++)
		{
			pdest = (BYTE*)(((DWORD)pdest + 3) & ~3);  
			memcpy(pdest, (void *)&m_rgDlgItem[i].m_dlgItemTemplate, 
										sizeof(DLGITEMTEMPLATE));
			pdest += sizeof(DLGITEMTEMPLATE);
			*(WORD*)pdest = 0xFFFF; 
			pdest += sizeof(WORD);
			*(WORD*)pdest = m_rgDlgItem[i].m_controltype;
			pdest += sizeof(WORD);

			WCHAR*	pchCaption;
			int		nChars, nActualChars;
/*
......transfer the caption even when it is an empty string
*/
			nChars = m_rgDlgItem[i].m_strCaption.GetLength() + 1;
			pchCaption = new WCHAR[nChars];
			nActualChars = MultiByteToWideChar(CP_ACP, 0, 
						m_rgDlgItem[i].m_strCaption, -1, pchCaption, nChars);
			ASSERT(nActualChars > 0);
			memcpy(pdest, pchCaption, nActualChars * sizeof(WCHAR));
			pdest += nActualChars * sizeof(WCHAR);
			delete pchCaption;

			*(WORD*)pdest = 0;  // How many bytes in data for control
			pdest += sizeof(WORD);
		}
		ASSERT(pdest - pBuffer == nBufferSize); 
		CDialog::CreateIndirect((DLGTEMPLATE*)pBuffer, m_pParent);
		LocalUnlock(hLocal);
		LocalFree(hLocal);
	}
	CATCH(CMemoryException, e)
	{
		MessageBox("Memory allocation for dialog template failed.  Demo aborted!",
			"Allocation Failure", MB_ICONEXCLAMATION | MB_OK);
	}
	END_CATCH
	return TRUE;
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

void CDialogForm::OnCancel() 
{
	if (m_pHelpDialog!=NULL)
	{
		((CDialogHelp*)m_pHelpDialog)->DlgCancel();
		m_pHelpDialog = NULL;
	}	
	((CDialogButton*)m_pParent)->Remove_Child(m_ChildId);
	CDialog::OnCancel();
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

void CDialogForm::PostNcDestroy() 
{
	delete this;
}

/***********************************************************************
c
c   SUBROUTINE:  OnAHelp
c
c   FUNCTION:  This function called when "Help" button pushed.
c				it display a help dialog
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogForm::OnAHelp()
{
	if (m_pHelpDialog!=NULL)
		m_pHelpDialog->SetActiveWindow();
	else
	{
		m_pHelpDialog = new CDialogHelp(this, 2);
		((CDialogHelp*)m_pHelpDialog)->sethelp(&m_DynWinStruct);
		((CDialogHelp*)m_pHelpDialog)->inittemp();
		((CDialogHelp*)m_pHelpDialog)->Create();
	}				
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
void CDialogForm::OnApply()
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

void CDialogForm::OnOK()
{
	SaveData();
		if (m_pHelpDialog!=NULL)
	{
		((CDialogHelp*)m_pHelpDialog)->DlgCancel();
		m_pHelpDialog = NULL;
	}
	((CDialogButton*)m_pParent)->Remove_Child(m_ChildId);
	CDialog::OnOK();
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
void CDialogForm::DlgCancel()
{
	OnCancel();
}

/***********************************************************************
c
c   SUBROUTINE:  SaveData
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
int CDialogForm::SaveData()
{
	int nitems,ist,nc;
	char tbuf[132],msg[82],eMsg[180], pText[132],xbuf[132];
	char**tmpbuf;
	tmpbuf = (char**)malloc(MAXFMT*sizeof(char*));
	for (int i=0; i<MAXFMT; i++)
	{
		tmpbuf[i] = (char*)malloc(132*sizeof(char));
	}
/*
......Save the current selection
*/
	if (m_DynWinStruct.winType == 3)
	{
		SaveSelection();
		nitems = ((CListBox*)GetDlgItem(ID_LIST))->GetCount();
		m_RetValStruct.numButtons = nitems;
/*
......Set return strings
......Scrolling Form
*/
		for(int i=0;i<nitems;i++)
		{
			((CListBox*)GetDlgItem(ID_LIST))->GetText(i, tbuf);
			strcpy(tmpbuf[i],tbuf);
			nc = 132;
			AddSpaces(tmpbuf[i],nc);
			if (strcmp(tmpbuf[i],m_DynFormStruct.recordString[i]) != 0)
				MpChanged = 1;
		}
	}
/*
......Table Form
*/
	else if (m_DynWinStruct.winType == 4 || m_DynWinStruct.winType == 5)
	{
		int nfld=0,knc;
		char fmt[10];
		m_RetValStruct.numButtons = m_DynFormStruct.numRecords;
	 	for (int i=0;i<m_DynFormStruct.numRecords;i++)
		{
			strcpy(xbuf,m_DynFormStruct.recordString[i]);
			for (int j=0;j<m_DynFormStruct.numFields;j++)
			{
				if (nfld >= m_DynFormStruct.totalFields) break;
				ist = m_DynFormStruct.fieldStart[j];
				knc = m_DynFormStruct.fieldEnd[j] - ist + 1;
				switch (m_DynFormStruct.fieldType[j])
				{
/*
......Text Field
*/
				case NpwETextB:
					nc = (GetDlgItem(ID_PRMP0+nfld))->GetWindowText(pText, 132);
					if (nc!=0)
					{
						if (nc > knc)
						{
							strncpy(&xbuf[ist],pText,knc);
							xbuf[ist+knc] = '\0';
						}
						else
						{
							fmt[0] = '%';
							sprintf(&fmt[1],"-%ds",knc);
							sprintf(&xbuf[ist],fmt,pText);
						}
					}
					else
					{
						for (nc=ist;nc<ist+knc;nc++) xbuf[nc] = ' ';
						xbuf[ist+knc] = '\0';
					}
					nc = strlen(xbuf);
					xbuf[nc] = ' ';
					break;
				default:
					break;
				}
				nfld++;
			}
			xbuf[nc] = '\0';
			strcpy(tmpbuf[i],xbuf);
			nc = 132;
			AddSpaces(tmpbuf[i],nc);
			if (strcmp(tmpbuf[i],m_DynFormStruct.recordString[i]) != 0)
				MpChanged = 1;
		}
	}
/*
......Return the form data to the application
*/
	int ipos,retVal=0;

	retVal = NpwPutForm(&m_RetValStruct,tmpbuf,&ipos,msg);
/*
......An error occurred
......Let's compose the message and report it
*/
	if(retVal)
	{
		if (m_DynWinStruct.winType == 3)
			sprintf(eMsg,"%s\n %s ",msg,tmpbuf[ipos-1]);
		else
			sprintf(eMsg,"%s",msg);
		MessageBox(eMsg, "Error!", MB_OK);
		return 0;
	}
/*
......Normal return
*/
	return 1;
}

/***********************************************************************
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

void CDialogForm::DlgOK()
{
	OnOK();
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


BOOL CDialogForm::OnInitDialog()
{
	int i, j, k,defsel;
	int id = ID_LIST;
	CDialog::OnInitDialog();
	SetIcon(m_hIcon, TRUE);
/*
.....change form list font to always use 'Courier' 
.....in order to line up
*/
	CWnd *list = GetDlgItem(ID_LIST);
	if (list!=NULL)
	{
		list->SetFont(&m_listfont);
	}

	if (m_DynWinStruct.winType!=3)
		return 1;
	for (i=0;i<m_DynFormStruct.numRecords;i++)
	{
		((CListBox*)GetDlgItem(ID_LIST))->AddString(
								m_DynFormStruct.recordString[i]);
	}
	((CListBox*)GetDlgItem(ID_LIST))->SetCurSel(0);
	for(i=1, j = 0;j<m_DynFormStruct.numFields;i++, j++)
	{
		switch(m_DynFormStruct.fieldType[j])
		{
			case NpwEChoiceB:
			{
				id = ID_CHOICE0 + j;
				for(k=0; k < m_DynFormStruct.numChoice[j]; k++)
				{
					((CComboBox*)GetDlgItem(id))->AddString(
									m_DynFormStruct.choiceString[j][k]);
				}
				defsel = m_DynFormStruct.defaultChoice[0][j];
				((CComboBox*)GetDlgItem(id))->SetCurSel(defsel); 
				break;
			}
			case NpwETextB:
			case NpwELabelB:
				break;
		}
	}
	m_init = 1;
	CRect rect;
	GetClientRect(&rect);
	m_cx = rect.Width();
	m_cy = rect.Height();
	GetDlgItem(ID_LIST)->GetWindowRect(&m_listrect);
	ScreenToClient(m_listrect);
	return 1;
}	

/***********************************************************************
c
c   SUBROUTINE:  OnPicked
c
c   FUNCTION:  This function called when list item been picked
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogForm::OnPicked()
{
	int i,j,ist,ien,inc,nc, len, id;
	char sbuf[132],tbuf[32];
/*
......Save current selection
*/
	SaveSelection();
/*
......Get the selected text and position
*/
	int sel = ((CListBox*)GetDlgItem(ID_LIST))->GetCurSel();
	if (sel != -1 ) 
		currentListPos =  sel;
//	else
//		return;
	len = ((CListBox*)GetDlgItem(ID_LIST))->GetText(currentListPos, sbuf);
	sbuf[len] = '\0';
/*
......Set the form entries to this selection
*/
	for (i=0;i<m_DynFormStruct.numFields;i++)
	{
		switch(m_DynFormStruct.fieldType[i])
		{
/*
......Choice Button
*/
		case NpwEChoiceB:
			inc = m_DynFormStruct.defaultChoice[currentListPos][i];	
			
			id = ID_CHOICE0 + i;
			((CComboBox*)GetDlgItem(id))->SetCurSel(inc); 
			m_RetValStruct.ButtonId[i]= inc;
			sprintf(m_RetValStruct.Butanswer[i],"%s",
					m_DynFormStruct.choiceString[i][inc]);
			break;
/*
......Text Field
*/
		case NpwETextB:
			ist = m_DynFormStruct.fieldStart[i];
			ien = m_DynFormStruct.fieldEnd[i];
			nc = ien - ist + 1;
			strncpy(tbuf,&sbuf[ist],nc);
			tbuf[nc] = '\0';
			RemoveSpaces(tbuf,&nc);
			(GetDlgItem(ID_PRMP0+i))->SetWindowText(tbuf);
			break;
/*
......Label Field
*/
		case NpwELabelB:
			ist = m_DynFormStruct.fieldStart[i];
			ien = m_DynFormStruct.fieldEnd[i];
			nc = ien - ist + 1;
  			if (ist >= nc)
  				for (j=0;j<nc;j++) tbuf[j] = ' ';
  			else
				strncpy(tbuf,&sbuf[ist],nc);
			tbuf[nc] = '\0';
			(GetDlgItem(ID_PRMP0+i))->SetWindowText(tbuf);
			strcpy(m_RetValStruct.Butanswer[i],tbuf);
			break;
		default:
			break;
		}
	}
/*
......End of routine
*/
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  SaveSelection()
c
c   FUNCTION:  This function save current selection data
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogForm::SaveSelection()
{
	int i,ist,ien,nc;
	char tbuf[132], pText[132];
/*
......Initialize routine
*/
	for (i=0;i<132;i++)
	{
		tbuf[i] = ' ';
	}
/*
......Loop thru fields to create form record
*/
	for (i=0;i<m_DynFormStruct.numFields;i++)
	{
/*
......Get the field's position
*/
		ist = m_DynFormStruct.fieldStart[i];
		ien = m_DynFormStruct.fieldEnd[i];
		switch(m_DynFormStruct.fieldType[i])
		{
/*
......Choice Button
*/
		case NpwEChoiceB:
			strcpy(&tbuf[ist],m_RetValStruct.Butanswer[i]);
			m_DynFormStruct.defaultChoice[currentListPos][i] = 
				m_RetValStruct.ButtonId[i];
			break;
/*
......Text Field
*/
		case NpwETextB:
			(GetDlgItem(ID_PRMP0+i))->GetWindowText(pText, 132);
			strcpy(&tbuf[ist],pText);
			break;
//
//......Label
//
		case NpwELabelB:
			strcpy(&tbuf[ist],m_RetValStruct.Butanswer[i]);
			break;
		default:
			break;
		}
		nc = strlen(tbuf);
		tbuf[nc] = ' ';
	}
	tbuf[nc] = '\0';
/*
......Put the form entry back in the scrolled selection list
*/
	((CListBox*)GetDlgItem(ID_LIST))->DeleteString(currentListPos);
	((CListBox*)GetDlgItem(ID_LIST))->InsertString(currentListPos, tbuf);
/*
......End of routine
*/
	return;
}
	
/***********************************************************************
c
c   SUBROUTINE:  Remove_Help
c
c   FUNCTION:  This function Remove help dialog (pointer from
c				Dynamic Form (but not delete it and destroy it
c				it destroy and delete by Help dialog itself.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogForm::Remove_Help()
{
	m_pHelpDialog = NULL;
}

/***********************************************************************
c
c   SUBROUTINE:  initTable
c
c   FUNCTION:  initialize class data for form table
c
c   INPUT:  none
c
c   OUTPUT: wid: width of table
c			ht:  height of table
c			ctl_num: total number of controls
c
c***********************************************************************
*/

void CDialogForm::initTable(int &wid, int &ht, int &ctl_num)
{
	int i, j, nc,nfld,nc1, start, sizex, sizey, gapy, char_sizey;
	DWORD style;
	char sbuf[132];
	nfld = 0;
	wid = 0;
	start = 0;
	pw_getstr_size(m_pParent, "Xx", 132, Pw_formdlg_font, &sizex, &char_sizey,0);
	gapy = char_sizey + 4;

	for(i=0;i<m_DynFormStruct.numRecords;i++)
	{
		if (start>wid) 
			wid = start;
		start = 10;
		ht = i*gapy+10;
		for(j=0;j<m_DynFormStruct.numFields;j++)
		{
			if (nfld >= m_DynFormStruct.totalFields)
				break;
			switch(m_DynFormStruct.fieldType[j])
			{
				case NpwELabelB:
				{
					nc = m_DynFormStruct.fieldEnd[j] -  m_DynFormStruct.fieldStart[j]
						+ 1;
					strncpy(sbuf,&m_DynFormStruct.recordString[i]
						[m_DynFormStruct.fieldStart[j]],nc);
					sbuf[nc] = '\0';
					RemoveSpaces(sbuf,&nc1);
					if (sbuf[0]!='\0')
						pw_getstr_size(m_pParent, sbuf, 132, Pw_formdlg_font, &sizex, &sizey,0);				
					else
						sizex = 5;
					m_rgDlgItem[nfld].setctl(2);
					style = WS_CHILD | WS_VISIBLE | SS_LEFT;
					m_rgDlgItem[nfld].setId(nfld);
					m_rgDlgItem[nfld].settemp2(start, ht, char_sizey, sizex, 0, style);
					m_rgDlgItem[nfld].setcaption((LPCTSTR)sbuf);
					start = start + sizex;
					break;
				}
				case NpwETextB:
				{

					nc = m_DynFormStruct.fieldEnd[j] - m_DynFormStruct.fieldStart[j]
						+ 1;
					strncpy(sbuf,&m_DynFormStruct.recordString[i]
						[m_DynFormStruct.fieldStart[j]],nc);
					sbuf[nc] = '\0';
					if (sbuf[0]!='\0')
						pw_getstr_size(m_pParent, sbuf, 132, Pw_formdlg_font, &sizex, &sizey,0);				
					else
						sizex = 10;
/*
.....added 2 for edit box border
*/
					sizex += 2;
					m_rgDlgItem[nfld].setctl(1);
					m_rgDlgItem[nfld].setId(nfld);
					style =
		 WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | ES_LEFT |WS_BORDER;
					m_rgDlgItem[nfld].settemp2(start, ht, char_sizey, sizex, ID_PRMP0+nfld,
						 style);
					m_rgDlgItem[nfld].setcaption(sbuf);
					start = start + sizex + 5;
				}
			}
			nfld++;
		}
	}
	if (start>wid) 
		wid = start;
/*
.....we added 12 for next start, but for wid, we need take it out
*/
	wid = wid - 5;
/*
......Create the INSERT/DELETE push buttons
*/
	if (m_DynWinStruct.winType == 5)
	{
		int total = wid+10;  
		int offset = 10;
		int butlen = (total-offset*3)/2;
		int bg_x = offset;
		int bg_y = ht+gapy;
		int end_x = bg_x + butlen ;
		int end_y = bg_y + gapy;
		m_rgDlgItem[nfld++].Initpanel("INSERT",ID_INSERT, bg_x, bg_y, end_x, 
					end_y);		
		bg_x = end_x + offset;
		end_x = bg_x + butlen ;
		m_rgDlgItem[nfld++].Initpanel("DELETE",ID_DELETE, bg_x, bg_y, end_x,
               end_y); 
		ht = ht + gapy;
		currentCursorPos = 0;
	}
	ctl_num = nfld - 1;
}

/***********************************************************************
c
c   SUBROUTINE:  OnInsert
c
c   FUNCTION:  This function called when "INSERT" button pushed
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogForm::OnInsert()
{
	int i, len;
	char pText[132];
	for (i=m_DynFormStruct.totalFields-1;i>currentCursorPos;i--)
	{
		len = (GetDlgItem(ID_PRMP0+i-1))->GetWindowText(pText, 132);
		if (len>0)
			(GetDlgItem(ID_PRMP0+i))->SetWindowText(pText);
	}
	(GetDlgItem(ID_PRMP0+currentCursorPos))->SetWindowText("");
	currentCursorPos++;
	(GetDlgItem(ID_PRMP0+currentCursorPos))->SetFocus();
}

/***********************************************************************
c
c   SUBROUTINE:  OnDelete
c
c   FUNCTION:  This function called when "DELETE" button pushed
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogForm::OnDelete()
{
	int i, len;
	char pText[132];
	for (i=currentCursorPos;i<m_DynFormStruct.totalFields-1;i++)
	{
		len = (GetDlgItem(ID_PRMP0+i+1))->GetWindowText(pText, 132);
		if (len>0)
			(GetDlgItem(ID_PRMP0+i))->SetWindowText(pText);
		else
			(GetDlgItem(ID_PRMP0+i))->SetWindowText("");
	}
	(GetDlgItem(ID_PRMP0+m_DynFormStruct.totalFields-1))->SetWindowText("");
	(GetDlgItem(ID_PRMP0+currentCursorPos))->SetFocus();
}

/***********************************************************************
c
c   SUBROUTINE:  OnChoicePicked(UINT id)
c
c   FUNCTION:  This function called when choce button selected
c
c   INPUT:  num:  Choice num
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogForm::OnChoicePicked(UINT id)
{
	char data[132];
	int num;
	int len;
	num = id - ID_CHOICE0;
	len = (GetDlgItem(id))->GetWindowText(data, 132);
	data[len] = '\0';
	m_RetValStruct.ButtonId[num]= ((CComboBox*)GetDlgItem(id))->GetCurSel();
	sprintf(m_RetValStruct.Butanswer[num],"%s",data);
}

/***********************************************************************
c
c   SUBROUTINE:  OnInputFocus(UINT id)
c
c   FUNCTION:  These functions called when Edit field get input focus.
c				%d correspond to different Edit field number.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogForm::OnInputFocus(UINT id)
{
	currentCursorPos = id - ID_PRMP0;		
}

void CDialogForm::OnSize( UINT nType, int cx, int cy )
{
		if (m_init==0)

	CWnd::OnSize(nType, cx, cy );
	if (m_init)
	{
		SizeListBox(cx - m_cx, cy - m_cy);
		m_cx = cx;
		m_cy = cy;
	}
}

void CDialogForm::SizeListBox(int cx, int cy )
{
	if ((cx==0)&&(cy==0))
		return;
/*
......iterate through and change their position and size(size only for listbox)
*/
	CRect rect;
	CWnd *pChildWnd = GetDlgItem(ID_LIST);

	m_listrect.bottom += cy;
	m_listrect.right += cx;
	pChildWnd->MoveWindow(&m_listrect);
	pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	while (pChildWnd)
	{
		pChildWnd->GetWindowRect(&rect);
		ScreenToClient(rect);
		rect.top += cy;
		rect.bottom += cy;
		pChildWnd->MoveWindow(&rect);
		pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	}
	pChildWnd = GetDlgItem(IDC_GROUP_BOX);
	pChildWnd->GetWindowRect(&rect);
	ScreenToClient(rect);
	rect.top -= cy;
	rect.right += cx;
	pChildWnd->MoveWindow(&rect);

	RedrawWindow();
}

/* End of DialofForm.cpp */
