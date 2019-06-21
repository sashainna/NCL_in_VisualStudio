/************************************************************************
c
c   FILE NAME: DialogHelp.cpp
c
c	 CONTAINS: 
c	 all CDialogHelp class override functions and 
c			Implementation functions
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        DialogHelp.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:01 
c
c**********************************************************************
*/ 


#include "Pwstdafx.h"
#include "Mpost.h"
#include "DialogHelp.h"
#include "DialogPrompt.h"
#include "DialogForm.h"
#include "DialogTemplate.h"
#include "mpostres.h"
#include "NpwHeaders.h"
#include <sys/stat.h>
#include <sys/types.h>

static int TOTALITEMS;
char Pw_helpdlg_font[80];
extern void pw_getavr_strsize(CWnd *win, int num, int pt, char *fntname, int *wid, int* hgt);

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/

BEGIN_MESSAGE_MAP(CDialogHelp, CDialog)
	//{{AFX_MSG_MAP(CDialogHelp)	
		ON_WM_SIZE( )
		ON_WM_CTLCOLOR()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()



/***********************************************************************
c
c   SUBROUTINE: CDialogHelp(CWnd* pParent, int type)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			int type:		window type.
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CDialogHelp::CDialogHelp(CWnd* pParent, int type)	
{
	m_pParent = pParent;
	m_ParentType = type;
	CDialog::CDialog();
/*
.....initial backgroup brush for Edit control
*/
	m_pEditBkBrush = new CBrush(RGB(255, 255, 255));
	if (type==3)
		strcpy(m_winTitle, "Document View");
	m_hIcon = AfxGetApp()->LoadIcon(IDI_DLGICON);
}
/***********************************************************************
c
c   SUBROUTINE:  settext(char* filename)
c
c   FUNCTION:  This function open a file and save the data
c				into class data m_pFileData
c
c   INPUT:  filename
c			
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogHelp::settext(char* filename)
{
	FILE *fp;
	int fileLength, i, j;
	char errMsg[160];
	struct _stat fileInfo; 
	char*tmpstr;
/*
.....open file
*/
	if ((!strlen(filename)) || ((fp = fopen(filename, "r") )== NULL))
	{
		sprintf(errMsg,"%s\n%s","Error: Unable to open file:",filename);
		MessageBox(errMsg, "Error!", MB_OK);
		return;
	}
/*
......get file size
*/
	if (_stat(filename, &fileInfo) == 0)
		fileLength = int(fileInfo.st_size);
	else
		fileLength = 1000000; 
/*
......put file data into class data m_pFileData.
*/
	tmpstr = new char[fileLength+1];
	m_pFileData = new char[fileLength+1];
	if(tmpstr != NULL)
	{
		fread(tmpstr, sizeof(char), fileLength, fp);
		tmpstr[fileLength] = '\0';
		for (i=0, j=0; j<fileLength; i++, j++)
		{
			if (tmpstr[i]!='\n')
				m_pFileData[j] = tmpstr[i];
			else
			{
/*
.....When display on Edit control field
.....the return need both '\r' and '\n'
*/
				m_pFileData[j++] = 13;
				m_pFileData[j] = tmpstr[i];
			}
		}
		m_pFileData[j] = '\0';
 		delete tmpstr;
	}
	else
	{
		sprintf(errMsg,"%s\n%s","Memory Allocation Error: Unable to open file:",filename);
		MessageBox(errMsg, "Error!", MB_OK);
		return;
	}
	if (fclose(fp) != NULL)
	{ 
		sprintf(errMsg,"%s\n%s","File Closing Error: Unable to close file:",filename);
		MessageBox(errMsg, "Error!", MB_OK);
		return;
	}
}	
	
/***********************************************************************
c
c   SUBROUTINE:  sethelp(NpwDynWinStruct* winStruct)
c
c   FUNCTION:  This function set class data m_pFileData
c				for help text
c
c   INPUT: winStruct: window struction of which asked display
c			help window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogHelp::sethelp(NpwDynWinStruct* winStruct)
{

	m_pFileData = new char[100000];
	
	strcpy(m_winTitle, winStruct->winTitle);
	int status = NpwGetAllHelp(winStruct, m_pFileData, m_ParentType);
	if (status==-1)
	{
		MessageBox("Error reading Help File.", "Error!", MB_OK);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  inittemp()
c
c   FUNCTION:  This function initialize class data
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogHelp::inittemp()
{
/*
.....set help dialog structure
*/
	m_rgDlgItem[0].setctl(1);
	m_rgDlgItem[0].setId(0);
	DWORD style = 
WS_CHILD | WS_VISIBLE | ES_MULTILINE |ES_WANTRETURN | ES_LEFT | ES_READONLY |WS_VSCROLL | WS_HSCROLL| WS_BORDER;
	m_rgDlgItem[0].settemp(0, 0, 10, 60, ID_HTEXT, style);
	m_rgDlgItem[0].setcaption(m_pFileData);
	int cwid, chgt, len = 80;
	int ht = 25;

	pw_getavr_strsize(m_pParent, 80, 80, Pw_helpdlg_font, &cwid, &chgt);

	TOTALITEMS = 1;
	m_dlgTempl.cx = cwid + GetSystemMetrics(SM_CXVSCROLL);
	m_dlgTempl.cy = (ht)*chgt + GetSystemMetrics(SM_CYHSCROLL);

	m_dlgTempl.style = 
		WS_CAPTION | WS_VISIBLE | WS_DLGFRAME | WS_OVERLAPPEDWINDOW | DS_SETFONT;
	m_dlgTempl.dwExtendedStyle = 0;
	m_dlgTempl.x = 100;
	m_dlgTempl.y = 100;
	m_dlgTempl.cdit = 1;
}

/***********************************************************************
c
c   SUBROUTINE:  Create()
c
c   FUNCTION:  This function Create Dynamic Template Dialog
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CDialogHelp::Create()
{
	
	WCHAR szBoxCaption[] = L"Dialog Template";
//	WCHAR szFontName[] = L"COURIER";
	WCHAR* szFontName; 
	int		nChars, nfChars;

	nChars = strlen(Pw_helpdlg_font) + 1;
	szFontName = new WCHAR[nChars];
	nfChars = MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, Pw_helpdlg_font, nChars, 
							szFontName, nChars);

	int		nTotalLength = 0;  
	int		i;

	TRY  
	{
		int nBufferSize =  sizeof(DLGTEMPLATE) + (2 * sizeof(WORD))
										+ sizeof(szBoxCaption);
		nBufferSize += sizeof(WORD) + nfChars * sizeof(WCHAR); 

		nBufferSize = (nBufferSize + 3) & ~3; 
		for (i = 0; i < TOTALITEMS; i++)
		{
			int nItemLength = sizeof(DLGITEMTEMPLATE) + 3 * sizeof(WORD);
			nItemLength += (m_rgDlgItem[i].m_strCaption.GetLength() + 1)
							 * sizeof(WCHAR);

			if (i != TOTALITEMS -1 )   
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

		memcpy(pdest, &m_dlgTempl, sizeof(DLGTEMPLATE));
		pdest += sizeof(DLGTEMPLATE);
		*(WORD*)pdest = 0; // no menu
		*(WORD*)(pdest + 1) = 0; 
		pdest += 2 * sizeof(WORD);
		memcpy(pdest, szBoxCaption, sizeof(szBoxCaption));
		pdest += sizeof(szBoxCaption);
		*(WORD*)pdest = 8;  // font size
		pdest += sizeof(WORD);
		memcpy(pdest, szFontName, nfChars * sizeof(WCHAR));
		pdest += nfChars * sizeof(WCHAR);
		delete szFontName ;

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

			nChars = m_rgDlgItem[i].m_strCaption.GetLength() + 1;
			pchCaption = new WCHAR[nChars];
			nActualChars = MultiByteToWideChar(CP_ACP, 0,
								 m_rgDlgItem[i].m_strCaption, -1, pchCaption, nChars);
			ASSERT(nActualChars > 0);
			memcpy(pdest, pchCaption, nActualChars * sizeof(WCHAR));
			pdest += nActualChars * sizeof(WCHAR);
			delete pchCaption;

			*(WORD*)pdest = 0;  
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
c   FUNCTION:  This function called when close window
c				it destroy and remove child window and itself
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogHelp::OnCancel() 
{
	if (m_ParentType == 1)
		((CDialogPrompt*)m_pParent)->Remove_Help();
	else if (m_ParentType == 2)
		((CDialogForm*)m_pParent)->Remove_Help();
	else
		((CDialogButton*)m_pParent)->Remove_Text();
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
void CDialogHelp::PostNcDestroy() 
{
	delete m_pEditBkBrush;
	delete m_pFileData;
	delete this;
}

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize window text
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CDialogHelp::OnInitDialog()
{
	CDialog::OnInitDialog();
 	SetIcon(m_hIcon, TRUE);

	int width = 80*CHAR_WID;
	SetWindowText(m_winTitle);
	((CEdit*)GetDlgItem(ID_HTEXT))->LimitText(1000000);
	GetDlgItem(ID_HTEXT)->SetWindowText(m_pFileData);
	GetDlgItem(ID_HTEXT)->SetFocus();
	return 0;
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
void CDialogHelp::DlgCancel()
{
	OnCancel();
}
/***********************************************************************
c
c   SUBROUTINE:  OnSize( UINT nType, int cx, int cy )
c
c   FUNCTION:  This function called after resize window
c
c   INPUT:  ntype: Specifies the type of resizing 
c			cx, cy: new width and height
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogHelp::OnSize( UINT nType, int cx, int cy )
{
	CWnd::OnSize(nType, cx, cy );
	CRect windowRect;
	GetClientRect(windowRect);
	CWnd* pChildWnd = GetWindow(GW_CHILD);
	pChildWnd->MoveWindow(windowRect);
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
HBRUSH CDialogHelp::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	switch (nCtlColor)
	{
/*
......we set Edit control readonly, so window treat it
......as static label. Set this Edit control's 
......background and text color
*/
		case CTLCOLOR_STATIC:
			pDC->SetTextColor(RGB(20, 20, 20));
			pDC->SetBkColor(RGB(255, 255, 255));
			return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
		default:
			return CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
	}
}
