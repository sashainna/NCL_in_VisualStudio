/************************************************************************
c
c   FILE NAME: PtedWindow5.cpp
c
c   CONTAINS:
c		CPtedWindow::FindText(LPCTSTR lpszFind, CPoint &ptStart, CPoint &ptEnd,
c                                BOOL bNext, BOOL bCase)
c		CPtedWindow::OnFindReplaceCmd(WPARAM, LPARAM lParam)
c		CPtedWindow::OnFindNext(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
c					int fletter, CPoint &ptStart, CPoint &ptEnd)
c		CPtedWindow::OnFindAll(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
c					int fletter, CPoint &ptStart, CPoint &ptEnd)
c		CPtedWindow::OnReplaceSel(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
c					LPCTSTR lpszReplace, int fletter, CPoint &ptStart, CPoint &ptEnd)
c		CPtedWindow::OnReplaceAll(LPCTSTR lpszFind, LPCTSTR lpszReplace, BOOL bCase,
c			int fletter, CPoint &ptStart, CPoint &ptEnd, int vflag)
c		CPtedWindow::OnTextNotFound(LPCTSTR)
c		CPtedWindow::On_Ffindnext()
c		CPtedWindow::On_Ffindprev()
c		CPtedWindow::On_Ffind()
c		CPtedWindow::On_Ffindall()
c		CPtedWindow::OnSearchReplace()
c		CPtedWindow::FindStrAdds(LPCTSTR lpszFind, char *adds, CPoint &ptStart, CPoint &ptEnd, BOOL bNext, BOOL bCase,
c						int *fadd, double* fval)
c		CPtedWindow::SameAsSelected(LPCTSTR lpszCompare, BOOL bCase)
c		CPtedWindow::BeginUndoGroup(BOOL bMergeWithPrevious)
c		CPtedWindow::FlushUndoGroup()
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c           PtedWindow5.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c           09/11/13 , 12:59:30
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include "Pted.h"
#include "PtedWindow.h"
#include "PtedChildWindow.h"
#include "PtedMainWindow.h"
#include "PtdGlobal.h"
#include "Ptedres.h"
#include "PtdFunc.h"
#include "PtedFindReplaceDialog.h"
#include <sys/stat.h>
#include <sys/types.h>

extern CPtedMainWindow *Pted_MainDlg;
extern "C" int Ptd_FindStringInTextLine(char* pszFindWhere, char* pszFindWhat, int bWholeWord);
typedef int (WINAPI* AFX_COMPARE_PROC)(LPCTSTR str1, LPCTSTR str2);

/***********************************************************************
c
c   SUBROUTINE:  FindText(LPCTSTR lpszFind, CPoint &ptStart, CPoint &ptEnd, 
c                 BOOL bNext, BOOL bCase)
c
c   FUNCTION:  This function find the text in specified range
c              and select these find
c
c   INPUT:  lpszFind: text string to find
c           ptStart, ptEnd:    Range to search
c           bNext:    find direction
c           bCase:    case sensitivity
c   OUTPUT: None
c   RETURN: None
c
c***********************************************************************
*/
BOOL CPtedWindow::FindText(LPCTSTR lpszFind, CPoint &ptStart, CPoint &ptEnd,
                                BOOL bNext, BOOL bCase)
{
	return m_TextView->FindTextInRange(lpszFind, ptStart, ptEnd, bNext, bCase);
}

/***********************************************************************
c
c   SUBROUTINE:  OnFindReplaceCmd(WPARAM, LPARAM lParam)
c
c   FUNCTION:  Callback function for findreplace dialog
c
c   INPUT:  lParam
c
c   OUTPUT: None
c   RETURN: None
c
c***********************************************************************
*/
LRESULT CPtedWindow::OnFindReplaceCmd(WPARAM, LPARAM lParam)
{
	ASSERT_VALID(this);
	CPoint ptStart, ptEnd;
	PtedFindReplaceDialog* pDialog = (PtedFindReplaceDialog*)PtedFindReplaceDialog::GetNotifier(lParam);
	pDialog->UpdateData();
	ASSERT(pDialog != NULL);
	ASSERT((pDialog == m_pReplaceDlg)||(pDialog == m_pFindDlg) 
				||(pDialog == m_pFindAllDlg) );
	if (!(pDialog->IsTerminating()))
	{
		m_frRange.begin = pDialog->m_fRange.begin;
		m_frRange.end = pDialog->m_fRange.end;
		strcpy(m_frRange.baddress, pDialog->m_fRange.baddress);
		strcpy(m_frRange.bstring, pDialog->m_fRange.bstring);
		strcpy(m_frRange.enumber, pDialog->m_fRange.enumber);
		strcpy(m_frRange.eaddress, pDialog->m_fRange.eaddress);
		strcpy(m_frRange.estring, pDialog->m_fRange.estring);
		m_TextView->Get_Range_Pos(&(pDialog->m_fRange), ptStart, ptEnd);
	}
	if (pDialog->IsTerminating())
	{
		if (pDialog == m_pReplaceDlg)
			m_pReplaceDlg = NULL;
		else if (pDialog == m_pFindDlg)
			m_pFindDlg = NULL;
		else
			m_pFindAllDlg = NULL;

		EnableWindow(TRUE);
		m_TextView->m_bShowInactiveSelection = FALSE;
	}
	else if (pDialog->FindNext())
	{
		if (ptStart==ptEnd)
			return 0;
		if (pDialog != m_pFindAllDlg)
		{
			OnFindNext(pDialog->GetFindString(),
				pDialog->m_down, pDialog->MatchCase(),
				pDialog->m_letter, ptStart, ptEnd);
		}
		else
		{
			OnFindAll(pDialog->GetFindString(),
				pDialog->m_down, pDialog->MatchCase(),
				pDialog->m_letter, ptStart, ptEnd);
			m_pFindAllDlg->SendMessage(WM_CLOSE);
		}
	}
	else if (pDialog->ReplaceCurrent())
	{
		if (ptStart==ptEnd)
			return 0;
		OnReplaceSel(pDialog->GetFindString(),
			pDialog->m_down, pDialog->MatchCase(),
			pDialog->GetReplaceString(),
			pDialog->m_letter, 
			ptStart, ptEnd);
	}
	else if (pDialog->ReplaceAll())
	{
		if (ptStart==ptEnd)
			return 0;
		OnReplaceAll(pDialog->GetFindString(), pDialog->GetReplaceString(),
			pDialog->MatchCase(),
			pDialog->m_letter, 
			ptStart, ptEnd);
	}
	Update_undo_redomenu(1,0);
	m_TextView->SetFocus( );
	ASSERT_VALID(this);
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  OnFindNext(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
c					int fletter, CPoint &ptStart, CPoint &ptEnd)
c				
c   FUNCTION:  Callback function for "Find Next" on findreplace dialog
c
c   INPUT:  fletter: match register
c           ptStart, ptEnd:    Range to search
c			lpszFind: find string
c			bNext:  find direction
c			bCase:  case sensitivity
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/

void CPtedWindow::OnFindNext(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
					int fletter, CPoint &ptStart, CPoint &ptEnd)
{
	double fval;
	ASSERT_VALID(this);
	m_strFind = lpszFind;
	m_bCase = bCase;
	m_bNext = bNext;
	m_letter = fletter;

	if (m_letter==0)
	{
		m_strType = 1;
		if (!FindText(m_strFind, ptStart, ptEnd, m_bNext, m_bCase))
			OnTextNotFound(m_strFind);
		ASSERT_VALID(this);
		return;
	}
	char *in, *out;
	int i, len;
	int fadd = 0;
	len = strlen(lpszFind);
	in = new char[len+1];
	for (i=0; i<len ; i++)
		in[i] = lpszFind[i];
	in[i] = '\0';
	out = new char[len+200];

	Ptd_GetFindStr(in, &out, bCase, &m_strType);
	if (m_strType==1)
	{
		if (!FindText(m_strFind, ptStart, ptEnd, m_bNext, m_bCase))
			OnTextNotFound(m_strFind);
	}
	else
	{
		if (!FindStrAdds(NULL, out, ptStart, ptEnd, m_bNext, bCase, &fadd, &fval))
			OnTextNotFound(m_strFind);
	}
	ASSERT_VALID(this);
	delete in;
	delete out;
}

/***********************************************************************
c
c   SUBROUTINE:  OnReplaceSel(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
c			LPCTSTR lpszReplace, int fletter, CPoint &ptStart, CPoint &ptEnd)
c				
c   FUNCTION:  Callback function for "Replace" on findreplace dialog
c
c   INPUT:  fletter: match register
c           ptStart, ptEnd:    Range to search
c			lpszFind: find string
c			lpszReplace: replace string
c			bNext:  find direction
c			bCase:  case sensitivity
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void CPtedWindow::OnReplaceSel(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
	LPCTSTR lpszReplace, int fletter, CPoint &ptStart, CPoint &ptEnd)
{
	int i;
	int nc;
	double fval;
	char selstr[256];
	char *temp;
	ASSERT_VALID(this);
	m_strFind = lpszFind;
	m_strReplace = lpszReplace;
	m_bCase = bCase;
	m_bNext = bNext;
	m_letter = fletter;
	selstr[0] = '\0';
	CString strSel = "";
	GetSelected_FindText(strSel);
	nc = strSel.GetLength();
	if (nc>0)
	{
		temp = strSel.GetBuffer(nc);
		strcpy(selstr, temp);
		if ((strcmp(selstr, m_strFind)==0) ||
			((_stricmp(selstr, m_strFind)==0) && (m_bCase==0)))
		{
			BeginUndoGroup();
			m_TextView->ReplaceSelection(lpszReplace);
			FlushUndoGroup();
			return;
		}
	}
	if (fletter==0)
	{
		m_strType = 1;
		m_strFind = lpszFind;
		if (!FindText(m_strFind, ptStart, ptEnd, m_bNext, m_bCase)) 
			OnTextNotFound(m_strFind);
		else
		{
			BeginUndoGroup();
			m_TextView->ReplaceSelection(lpszReplace);
			FlushUndoGroup();
		}
		ASSERT_VALID(this);
		return;
	}
	char *in1, *out1;
	char *in2, *out2;
	int len1, len2, rtype, fadd;
	fadd = 0;
	len1 = m_strFind.GetLength();
	len2 = m_strReplace.GetLength();
	in1 = new char[200];
	in2 = new char[200];
	for (i=0; i<len1 ; i++)
		in1[i] = m_strFind[i];
	in1[i] = '\0';
	for (i=0; i<len2 ; i++)
		in2[i] = m_strReplace[i];
	in2[i] = '\0';

	out1 = new char[200];
	out2 = new char[200];
	Ptd_GetFindStr(in1, &out1, bCase, &m_strType);
	Ptd_GetFindStr(in2, &out2, bCase, &rtype);
	if (m_strType==1)
	{
		if (!FindText(m_strFind, ptStart, ptEnd, m_bNext, m_bCase))
			OnTextNotFound(m_strFind);
		else
		{
			BeginUndoGroup();
			m_TextView->ReplaceSelection(lpszReplace);
			FlushUndoGroup();
		}
	}
	else
	{
		if (!FindStrAdds(NULL, out1, ptStart, ptEnd, m_bNext, bCase, &fadd, &fval))
			OnTextNotFound(m_strFind);
		else 
		{
			BeginUndoGroup();
			if ((rtype==2)&&(fadd==1))
			{
				Ptd_GetAdds(in2, &out2, bCase, fval);
				m_TextView->ReplaceSelection(out2);
			}
			else
			{
				m_TextView->ReplaceSelection(lpszReplace);
			}
			FlushUndoGroup();
		}
	}
	ASSERT_VALID(this);
}
/***********************************************************************
c
c   SUBROUTINE:  OnReplaceAll(LPCTSTR lpszFind, BOOL bCase,
c			LPCTSTR lpszReplace, int fletter, CPoint &ptStart, CPoint &ptEnd)
c				
c   FUNCTION:  Callback function for "Replace All" on findreplace dialog
c
c   INPUT:  fletter: match register
c           cRange:    Range to search
c			lpszFind: find string
c			lpszReplace: replace string
c			bCase:  case sensitivity
c			vflag: verify flag
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void CPtedWindow::OnReplaceAll(LPCTSTR lpszFind, LPCTSTR lpszReplace, BOOL bCase,
			int fletter, PtedRangeStruct* cRange, int vflag)
{
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(cRange, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return;
	OnReplaceAll(lpszFind, lpszReplace, bCase, fletter, ptStart, ptEnd, vflag);
}

/***********************************************************************
c
c   SUBROUTINE:  OnReplaceAll(LPCTSTR lpszFind, BOOL bCase,
c			LPCTSTR lpszReplace, int fletter, CPoint &ptStart, CPoint &ptEnd)
c				
c   FUNCTION:  Callback function for "Replace All" on findreplace dialog
c
c   INPUT:  fletter: match register
c           ptStart, ptEnd:    Range to search
c			lpszFind: find string
c			lpszReplace: replace string
c			bCase:  case sensitivity
c			vflag: verify flag
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void CPtedWindow::OnReplaceAll(LPCTSTR lpszFind, LPCTSTR lpszReplace, BOOL bCase,
			int fletter, CPoint &ptStart, CPoint &ptEnd, int vflag)
{
	m_strFind = lpszFind;
	m_strReplace = lpszReplace;
	m_bCase = bCase;
	m_bNext = TRUE;
	m_letter = fletter;

	m_TextView->ReplaceAll(lpszFind, lpszReplace, bCase,fletter, ptStart,
						ptEnd, vflag);
	ASSERT_VALID(this);
}

/**********************************************************************
c
c   FUNCTION:  OnTextNotFound
c				beep when text not find
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnTextNotFound(LPCTSTR text)
{
	char msg[256];
	ASSERT_VALID(this);
	MessageBeep(0);
	sprintf(msg, "Can't find %s", text);
	MessageBox(msg, "Pted Message", MB_OK);
}

/***********************************************************************
c
c   SUBROUTINE:  On_Ffindnext()
c
c   FUNCTION:  This function called when user select "Find Next"
c				from Search Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_Ffindnext()
{
	double fval;
	int fadd = 0;
	if (m_strFind == "") 
	{
		On_Ffind();
		return;
	}
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&m_frRange, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return;

	if (m_letter==0)
	{
		m_strType = 1;
		if (!FindText(m_strFind, ptStart, ptEnd, 1, m_bCase))
			OnTextNotFound(m_strFind);
		ASSERT_VALID(this);
		return;
	}
	char *in, *out;
	int i, len;

	len = m_strFind.GetLength();
	in = new char[200];
	for (i=0; i<len ; i++)
		in[i] = m_strFind[i];
	in[i] = '\0';
	out = new char[200];

	Ptd_GetFindStr(in, &out, m_bCase, &m_strType);
	if (m_strType==1)
	{
		if (!FindText(m_strFind, ptStart, ptEnd, 1, m_bCase))
			OnTextNotFound(m_strFind);
	}
	else
	{
		if (!FindStrAdds(NULL, out, ptStart, ptEnd, 1, m_bCase, &fadd, &fval))
			OnTextNotFound(m_strFind);
	}
	ASSERT_VALID(this);
	delete in;
	delete out;
}

/***********************************************************************
c
c   SUBROUTINE:  On_Ffindprev()
c
c   FUNCTION:  This function called when user select "Find Prev"
c				from Search Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_Ffindprev()
{
	double fval;
	int fadd = 0;
	if (m_strFind == "") 
	{
		On_Ffind();
		return;
	}
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&m_frRange, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return;

	if (m_letter==0)
	{
		m_strType = 1;
		if (!FindText(m_strFind, ptStart, ptEnd, 0, m_bCase))
			OnTextNotFound(m_strFind);
		ASSERT_VALID(this);
		return;
	}
	char *in, *out;
	int i, len;

	len = m_strFind.GetLength();
	in = new char[200];
	for (i=0; i<len ; i++)
		in[i] = m_strFind[i];
	in[i] = '\0';
	out = new char[200];

	Ptd_GetFindStr(in, &out, m_bCase, &m_strType);
	if (m_strType==1)
	{
		if (!FindText(m_strFind, ptStart, ptEnd, 0, m_bCase))
			OnTextNotFound(m_strFind);
	}
	else
	{
		if (!FindStrAdds(NULL, out, ptStart, ptEnd, 0, m_bCase, &fadd, &fval))
			OnTextNotFound(m_strFind);
	}
	ASSERT_VALID(this);
}

/***********************************************************************
c
c   SUBROUTINE:  On_Ffind()
c
c   FUNCTION:  This function called when user select "Find"
c				from Search Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_Ffind()
{
	if (m_pFindDlg != NULL)
	{
		m_pFindDlg->SetActiveWindow();
		m_pFindDlg->ShowWindow(SW_SHOW);
		return;
	}
	CString strFind = " ";
	GetSelected_FindText(strFind);
	if (strFind.IsEmpty())
		strFind = m_strFind;
	CString strReplace = m_strReplace;

	m_pFindDlg = new PtedFindReplaceDialog;
	DWORD dwFlags = FR_HIDEWHOLEWORD;

	if (m_bCase)
		dwFlags |= FR_MATCHCASE;
	if (m_bNext)
		dwFlags |= FR_DOWN;

	dwFlags |= FR_ENABLETEMPLATE ;

	(m_pFindDlg->m_fr).lpTemplateName = MAKEINTRESOURCE (IDD_FIND_DIALOG);
	(m_pFindDlg->m_fRange).begin = 1;
	if (m_bNext)
	{
		m_pFindDlg->m_down = 1;
		m_pFindDlg->m_up = 0;
	}
	else
	{
		m_pFindDlg->m_down = 0;
		m_pFindDlg->m_up = 1;
	}
	if (m_letter)
	{
		m_pFindDlg->m_letter = 1;
	}
	else
		m_pFindDlg->m_letter = 0;

	if (!m_pFindDlg->Create(1, strFind, strReplace, dwFlags, this))
	{
		m_pFindDlg = NULL;
		ASSERT_VALID(this);
		return;
	}

	m_pFindDlg->SetActiveWindow();
	m_pFindDlg->ShowWindow(SW_SHOW);
	EnableWindow(FALSE);
	ASSERT(m_pFindDlg != NULL);
	m_TextView->m_bShowInactiveSelection = TRUE;
}

/***********************************************************************
c
c   SUBROUTINE:  On_Ffindall()
c
c   FUNCTION:  This function called when user select "Find All"
c				from Search Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_Ffindall()
{
	if (m_pFindAllDlg != NULL)
	{
		m_pFindAllDlg->SetActiveWindow();
		m_pFindAllDlg->ShowWindow(SW_SHOW);
		return;
	}
	CString strFind = " ";
	GetSelected_FindText(strFind);
	if (strFind.IsEmpty())
		strFind = m_strFind;

	m_pFindAllDlg = new PtedFindReplaceDialog;
	DWORD dwFlags = FR_HIDEWHOLEWORD;

	if (m_bCase)
		dwFlags |= FR_MATCHCASE;
	if (m_bNext)
		dwFlags |= FR_DOWN;

	dwFlags |= FR_ENABLETEMPLATE ;

	(m_pFindAllDlg->m_fr).lpTemplateName = MAKEINTRESOURCE (IDD_FINDALL_DIALOG);
	(m_pFindAllDlg->m_fRange).begin = 1;
	if (m_bNext)
	{
		m_pFindAllDlg->m_down = 1;
		m_pFindAllDlg->m_up = 0;
	}
	else
	{
		m_pFindAllDlg->m_down = 0;
		m_pFindAllDlg->m_up = 1;
	}
	if (m_letter)
	{
		m_pFindAllDlg->m_letter = 1;
	}
	else
		m_pFindAllDlg->m_letter = 0;

	if (!m_pFindAllDlg->Create(1, strFind, "", dwFlags, this))
	{
		m_pFindAllDlg = NULL;
		ASSERT_VALID(this);
		return;
	}
	m_pFindAllDlg->SetActiveWindow();
	m_pFindAllDlg->ShowWindow(SW_SHOW);
	EnableWindow(FALSE);
	ASSERT(m_pFindAllDlg != NULL);
	m_TextView->m_bShowInactiveSelection = TRUE;
}

/***********************************************************************
c
c   SUBROUTINE:  OnFindAll(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
c					int fletter, CPoint &ptStart, CPoint &ptEnd)
c				
c   FUNCTION:  Callback function for "Find All" on findreplace dialog
c
c   INPUT:  fletter: match register
c           ptStart, ptEnd:    Range to search
c			lpszFind: find string
c			bNext:  find direction
c			bCase:  case sensitivity
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/

void CPtedWindow::OnFindAll(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
					int fletter, CPoint &ptStart, CPoint &ptEnd)
{
	int stat;
	ASSERT_VALID(this);
	m_strFind = lpszFind;
	m_bCase = bCase;
	m_bNext = bNext;
	m_letter = fletter;

	CPtedChildWindow* child;
	if (m_pParent==NULL)
		child = new CPtedChildWindow(Pted_MainDlg, "Find All", 0, 4);
	else
		child = new CPtedChildWindow(m_pParent, "Find All", 0, 4);
	child->Create(IDD_WINDOWDIALOG);
	child->m_strFind = m_strFind;
	child->m_bCase = m_bCase;
	child->m_bNext = m_bNext;
	child->m_letter = m_letter;

//	DWORD dwFlags = FIND_WHOLE_WORD;
	DWORD dwFlags = 0;
	if (m_bNext==0) 
		dwFlags = FIND_DIRECTION_UP | dwFlags;
	if (m_bCase) 
		dwFlags = FIND_MATCH_CASE | dwFlags;
	stat = m_TextView->FindAll(m_strFind, dwFlags, m_letter, 
								ptStart, ptEnd, child->m_TextView);
	if (stat)
	{
		child->ShowWindow(TRUE);
		if (m_pParent==NULL)
		{
			Pted_MainDlg->Add_Child(child);
		}
		else
			((CPtedMainWindow*)m_pParent)->Add_Child(child);
	}
	else
	{
		delete child;
		OnTextNotFound(m_strFind);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  OnSearchReplace()
c
c   FUNCTION:  This function called when user select "Replace"
c				from Search Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnSearchReplace() 
{
	if (m_pReplaceDlg != NULL)
	{
		m_pReplaceDlg->SetActiveWindow();
		m_pReplaceDlg->ShowWindow(SW_SHOW);
		return;
	}
	CString strFind = " ";

	GetSelected_FindText(strFind);
	if (strFind.IsEmpty())
		strFind = m_strFind;
	CString strReplace = m_strReplace;


	m_pReplaceDlg = new PtedFindReplaceDialog;
	DWORD dwFlags = FR_HIDEWHOLEWORD;
	if (m_bNext)
		dwFlags |= FR_DOWN;
	if (m_bCase)
		dwFlags |= FR_MATCHCASE;
	dwFlags |= FR_ENABLETEMPLATE ;

	(m_pReplaceDlg->m_fr).lpTemplateName = MAKEINTRESOURCE (IDD_REPLACE_DIALOG);
	(m_pReplaceDlg->m_fRange).begin = 1;
	if (m_bNext)
	{
		m_pReplaceDlg->m_down = 1;
		m_pReplaceDlg->m_up = 0;
	}
	else
	{
		m_pReplaceDlg->m_down = 0;
		m_pReplaceDlg->m_up = 1;
	}
	if (m_letter)
	{
		m_pReplaceDlg->m_letter = 1;
	}
	else
		m_pReplaceDlg->m_letter = 0;

	if (!m_pReplaceDlg->Create(0, strFind,
		strReplace, dwFlags, this))
	{
		m_pReplaceDlg = NULL;
		ASSERT_VALID(this);
		return;
	}

	m_pReplaceDlg->SetActiveWindow();
	m_pReplaceDlg->ShowWindow(SW_SHOW);
	EnableWindow(FALSE);
	ASSERT(m_pReplaceDlg != NULL);
	m_TextView->m_bShowInactiveSelection = TRUE;
}
/***********************************************************************
c
c   SUBROUTINE:  FindStrAdds(LPCTSTR lpszFind, char *adds, CPoint &ptStart, CPoint &ptEnd, 
c						BOOL bNext, BOOL bCase, int *fadd, double *fval)
c
c   FUNCTION:  This function find the text or letter address in 
c				specified range whichever come first 
c				and select these find
c
c   INPUT:  lpszFind: text string to find
c			adds:     Letter address to find
c           ptStart, ptEnd:    Range to search
c			bNext:	  find direction
c			bCase:    case sensitivity
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
BOOL CPtedWindow::FindStrAdds(LPCTSTR lpszFind, char *adds, CPoint &ptStart, CPoint &ptEnd, BOOL bNext, BOOL bCase,
						int *fadd, double* fval)
{
	return m_TextView->FindStrAdds(lpszFind, adds, ptStart, ptEnd, bNext, bCase, fadd, fval);
}
/***********************************************************************
c
c   FUNCTION: SameAsSelected(LPCTSTR lpszCompare, BOOL bCase)
c
c				check if string is same as select string
c
c   INPUT:  lpszCompare : compared string
c			bCase:        case sensitive
c			
c   OUTPUT: 1: same as select
c			0: not same
c
c***********************************************************************
*/
BOOL CPtedWindow::SameAsSelected(LPCTSTR lpszCompare, BOOL bCase)
{
	int result;
	CString strSelect;
	GetSelected_FindText(strSelect);
	result = (bCase && lstrcmp(lpszCompare, strSelect) == 0) ||
		(!bCase && lstrcmpi(lpszCompare, strSelect) == 0);
	if (result==1) return result;
	int nLen = lstrlen(lpszCompare);
	char *adds, *selstr;
	int i;
	adds = new char[nLen+1];
	for (i=0; i<nLen; i++)
		adds[i] = lpszCompare[i];
	adds[i] = '\0';
	nLen = strSelect.GetLength();
	selstr = new char[nLen+1];
	for (i=0; i<nLen; i++)
		selstr[i] = strSelect[i];
	selstr[i] = '\0';

	result = Ptd_SameAdds(adds, selstr, bCase);
	delete selstr;
	delete adds;
	return result;
}
/***********************************************************************
c
c   FUNCTION: BeginUndoGroup(BOOL bMergeWithPrevious)
c
c			Set the undo group begin
c
c   INPUT:  bMergeWithPrevious: if this undo group merge with previous group
c			
c   OUTPUT: none
c		
c
c***********************************************************************
*/
void CPtedWindow::BeginUndoGroup(BOOL bMergeWithPrevious)
{
	m_TextView->BeginUndoGroup(bMergeWithPrevious);
}
/***********************************************************************
c
c   FUNCTION: FlushUndoGroup()
c
c			Flush the UndoGroup
c
c   INPUT:  none
c			
c   OUTPUT: none
c		
c
c***********************************************************************
*/
void CPtedWindow::FlushUndoGroup()
{
	m_TextView->FlushUndoGroup();
}

/***********************************************************************
c
c   FUNCTION: SetModifiedFlag(BOOL bModified)
c
c			This function set the window title as modified (add '*')
c
c   INPUT:  bModified: modify flag to be set
c			
c   OUTPUT: none
c		
c
c***********************************************************************
*/
void CPtedWindow::SetModifiedFlag(BOOL bModified /*= TRUE*/)
{
	char title[UX_MAX_PATH+40], newtitle[UX_MAX_PATH+40];
	int len = GetWindowText(title, UX_MAX_PATH+40);

	if (bModified)
		sprintf (newtitle, "%s*", title);
	else
	{
		if (title[len-1]=='*')
			title[len-1] = '\0';
		strcpy (newtitle, title);
	}
	SetWindowText(newtitle);
}
int CPtedWindow::IsFindString(const char *str, int knc, int *pos, int *nc)
{
	char *temp;
	char linestr[256], find_str[256];
	CString what = m_strFind;
	if (m_bCase == 0)
		what.MakeUpper();
	*nc = what.GetLength();
	if (*nc==0)
		return 0;
	temp = what.GetBuffer(*nc);
	strcpy(find_str, temp);

	strncpy(linestr, str, knc);
	linestr[knc] = '\0';
	*pos = ::Ptd_FindStringInTextLine(linestr, find_str, 0);
	if (*pos>=0) 
	{
		return 1;
	}
	else
		return 0;
}
