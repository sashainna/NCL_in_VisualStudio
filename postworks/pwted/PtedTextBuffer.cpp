/************************************************************************
c
c   FILE NAME: PtedTextBuffer.cpp
c
c   CONTAINS:
c		CPtedTextBuffer::SUndoRecord::SetText(LPCTSTR pszText)
c		CPtedTextBuffer::SUndoRecord::FreeText()
c		CPtedTextBuffer::SUndoRecord::SetBuffer(CPtedTextBuffer *buffer)
c		CPtedTextBuffer::SUndoRecord::FreeBuffer()
c		CPtedTextBuffer::CInsertContext::RecalcPoint(CPoint &ptPoint)
c		CPtedTextBuffer::CDeleteContext::RecalcPoint(CPoint &ptPoint)
c		CPtedTextBuffer::CPtedTextBuffer()
c		CPtedTextBuffer::~CPtedTextBuffer()
c		CPtedTextBuffer::Copy (CPtedTextBuffer *buffer)
c		CPtedTextBuffer::InsertLine(LPCTSTR pszLine, int nLength, int nPosition)
c		CPtedTextBuffer::AppendLine(int nLineIndex, LPCTSTR pszChars, int nLength, int &newline, int &newchar)
c		void CPtedTextBuffer::FreeAll()
c		CPtedTextBuffer::InitNew(int fStyle)
c		CPtedTextBuffer::GetReadOnly() const
c		CPtedTextBuffer::SetReadOnly(BOOL bReadOnly)
c		CPtedTextBuffer::LoadCutterFile(char *filename, char* msg, int *err)
c		CPtedTextBuffer::LoadCutterData(int *err, char *msg)
c		CPtedTextBuffer::LoadFromFile(LPCTSTR pszFileName, int ftype, int fStyle)
c		CPtedTextBuffer::SaveToFile(LPCTSTR pszFileName, int ftype, int *classpt, int class_type, int fStyle, BOOL bClearModifiedFlag)
c		CPtedTextBuffer::SaveSelectToFile(LPCTSTR pszFileName, int ftype, CPoint &ptStart, CPoint &ptEnd, int *classpt, int class_type, int fStyle, BOOL bClearModifiedFlag)
c		CPtedTextBuffer::GetFILEMode()
c		CPtedTextBuffer::SetFILEMode(int nFILEMode)
c		CPtedTextBuffer::GetLineCount()
c		CPtedTextBuffer::GetLineLength(int nLine)
c		CPtedTextBuffer::GetLineChars(int nLine)
c		CPtedTextBuffer::GetLineFlags(int nLine)
c		CPtedTextBuffer::FindLineWithFlag(DWORD dwFlag)
c		CPtedTextBuffer::GetLineWithFlag(DWORD dwFlag)
c		CPtedTextBuffer::SetLineFlag(int nLine, DWORD dwFlag, BOOL bSet, BOOL bRemoveFromPreviousLine)
c		CPtedTextBuffer::GetText(int nStartLine, int nStartChar, int nEndLine, int nEndChar, CString &text, LPCTSTR pszFILE)
c		CPtedTextBuffer::AttachView(CPtedTextView *pView)
c		CPtedTextBuffer::UpdateViews(CUpdateContext *pContext, DWORD dwUpdateFlags, int nLineIndex) 
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c	MODULE NAME AND RELEASE LEVEL
c       PtedTextBuffer.cpp , 24.1
c	DATE AND TIME OF LAST  MODIFICATION
c       09/11/13 , 12:59:28
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include <malloc.h>
#include "PtedTextBuffer.h"
#include "PtedTextView.h"
#include <sys/stat.h>
#include <sys/types.h>
#include "PtdFunc.h"
#include "PtdGlobal.h"
#include <time.h>
#include <direct.h>

#ifndef __AFXPRIV_H__
#include <afxpriv.h>
#endif

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define		UNDO_ACTION_SIZE				1024
#define		UNDO_GROUP_SIZE				10
const TCHAR rdsvf[] = _T("\r\n");

#ifdef _DEBUG
#define _ADVANCED_BUGCHECK	1
#endif

extern "C" int Pted_Disp_Msg(char *msg, int flag, int *classpt, int class_type);
extern "C" void ptd_tolinit();
extern "C" int Ptd_clload(char *filename, char **outdata);
extern "C" char Pted_localdir[UX_MAX_PATH];
/////////////////////////////////////////////////////////////////////////////
// CPtedTextBuffer::SUndoRecord

/***********************************************************************
c
c   SUBROUTINE: SUndoRecord::SetText(LPCTSTR pszText)
c
c   FUNCTION:  Set the text buffer for a undo record
c
c   INPUT:  pszText: text to be saved
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::SUndoRecord::SetText(LPCTSTR pszText)
{
	m_pszText = NULL;
	if (pszText != NULL && pszText[0] != _T('\0'))
	{
		int nLength = _tcslen(pszText);
		if (nLength > 1)
		{
			m_pszText = new TCHAR[(nLength + 1) * sizeof(TCHAR)];
			_tcscpy(m_pszText, pszText);
		}
		else
		{
			m_szText[0] = pszText[0];
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE: SUndoRecord::FreeText()	
c
c   FUNCTION:  Free the text buffer for a undo record
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::SUndoRecord::FreeText()
{
	if (HIWORD((DWORD) m_pszText) != 0)
		delete m_pszText;
}

/***********************************************************************
c
c   SUBROUTINE: SUndoRecord::SetBuffer(CPtedTextBuffer *buffer)
c
c   FUNCTION:  Set the text buffer for a undo record
c
c   INPUT:  CPtedTextBuffer *buffer: buffer to be set
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::SUndoRecord::SetBuffer(CPtedTextBuffer *buffer)
{
	m_tBuffer = new CPtedTextBuffer();
	m_tBuffer->InitNew();
	int size = buffer->GetLineCount();
	m_tBuffer->SetSize(size);
	m_tBuffer->Copy(buffer);
}

/***********************************************************************
c
c   SUBROUTINE: SUndoRecord::FreeBuffer()
c
c   FUNCTION:  Free the text buffer for a undo record
c
c   INPUT:  CWnd* pParent : parent window
c			int type:		window type.
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::SUndoRecord::FreeBuffer()
{
	if (m_tBuffer!=NULL)
	{
		delete m_tBuffer;
		m_tBuffer = NULL;
	}
}


/////////////////////////////////////////////////////////////////////////////
// CPtedTextBuffer::CUpdateContext

/***********************************************************************
c
c   SUBROUTINE: CPtedTextBuffer::CInsertContext::RecalcPoint(CPoint &ptPoint)	
c
c   FUNCTION:  Recalculate the cursor point
c
c   INPUT:  ptPoint: cursor point to be recalculted
c			
c   OUTPUT: ptPoint: calculeted cursor point
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextBuffer::CInsertContext::RecalcPoint(CPoint &ptPoint)
{
	ASSERT(m_ptEnd.y > m_ptStart.y ||
		   m_ptEnd.y == m_ptStart.y && m_ptEnd.x >= m_ptStart.x);
	if (ptPoint.y < m_ptStart.y)
		return;
	if (ptPoint.y > m_ptStart.y)
	{
		ptPoint.y += (m_ptEnd.y - m_ptStart.y);
		return;
	}
	if (ptPoint.x <= m_ptStart.x)
		return;
	ptPoint.y += (m_ptEnd.y - m_ptStart.y);
	ptPoint.x = m_ptEnd.x + (ptPoint.x - m_ptStart.x);
	if (ptPoint.x>LINE_MAX_CHARS)
		ptPoint.x = ptPoint.x - LINE_MAX_CHARS;
}

/***********************************************************************
c
c   SUBROUTINE: CPtedTextBuffer::CDeleteContext::RecalcPoint(CPoint &ptPoint)	
c
c   FUNCTION:  Recalculate the cursor point
c
c   INPUT:  ptPoint: cursor point to be recalculted
c			
c   OUTPUT: ptPoint: calculeted cursor point
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextBuffer::CDeleteContext::RecalcPoint(CPoint &ptPoint)
{
	ASSERT(m_ptEnd.y > m_ptStart.y ||
		   m_ptEnd.y == m_ptStart.y && m_ptEnd.x >= m_ptStart.x);
	if (ptPoint.y < m_ptStart.y)
		return;
	if (ptPoint.y > m_ptEnd.y)
	{
		ptPoint.y -= (m_ptEnd.y - m_ptStart.y);
/*
......if the ptPoint is continued line of m_ptEnd, then we need adjust X too
......temp not adjust here
*/
//		ptPoint.x = ptPoint.x  - (m_ptEnd.x - m_ptStart.x);
		return;
	}
	if (ptPoint.y == m_ptEnd.y && ptPoint.x >= m_ptEnd.x)
	{
		ptPoint.y = m_ptStart.y;
		ptPoint.x = m_ptStart.x + (ptPoint.x - m_ptEnd.x);
		while (ptPoint.x>=LINE_MAX_CHARS) 
			ptPoint.x = ptPoint.x - LINE_MAX_CHARS;
		return;
	}
	if (ptPoint.y == m_ptStart.y)
	{
		if (ptPoint.x > m_ptStart.x)
			ptPoint.x = m_ptStart.x;
		return;
	}
	ptPoint = m_ptStart;
}


/////////////////////////////////////////////////////////////////////////////
// CPtedTextBuffer

IMPLEMENT_DYNCREATE(CPtedTextBuffer, CCmdTarget)

/***********************************************************************
c
c   SUBROUTINE: CPtedTextBuffer()	
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPtedTextBuffer::CPtedTextBuffer()
{
	m_bInit = FALSE;
	m_bReadOnly = FALSE;
	m_bModified = FALSE;
	m_bCreateBackupFile = FALSE;
	m_nUndoPosition = 0;
	m_current_size = 0;
	m_max_size = 5000;
	m_attview = NULL;
	m_no_undo = 0;
	m_bMergeWithPrevious = 1;
	m_group_num = 0;
	m_group_count = 0;
}

/***********************************************************************
c
c   SUBROUTINE: ~CPtedTextBuffer()	
c
c   FUNCTION:  Deconstructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPtedTextBuffer::~CPtedTextBuffer()
{
	FreeAll();
	ASSERT(! m_bInit);			//	You must call FreeAll() before deleting the object
}


BEGIN_MESSAGE_MAP(CPtedTextBuffer, CCmdTarget)
	//{{AFX_MSG_MAP(CPtedTextBuffer)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CPtedTextBuffer message handlers


/***********************************************************************
c
c   SUBROUTINE: CPtedTextBuffer::Copy (CPtedTextBuffer *buffer)
c
c   FUNCTION:  Copy a CPtedTextBuffer into current text buffer
c
c   INPUT:  buffer: buffer to be copied
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::Copy (CPtedTextBuffer *buffer)
{
	int endchar = buffer->GetLineLength(3);
	int size = buffer->GetLineCount();
	for (int i=0; i<size; i++)
	{
		m_aLines[i].m_nLength = buffer->m_aLines[i].m_nLength;
		m_aLines[i].m_spaces = buffer->m_aLines[i].m_spaces;
		m_aLines[i].m_nMax = LINE_MAX_CHARS;
		m_aLines[i].m_linkline = buffer->m_aLines[i].m_linkline;
		m_aLines[i].m_pcLine = new TCHAR[m_aLines[i].m_spaces];
		memcpy(m_aLines[i].m_pcLine, buffer->m_aLines[i].m_pcLine, sizeof(TCHAR) * (buffer->m_aLines[i].m_nLength));
	}
	SetCurrentSize(size);
}
/***********************************************************************
c
c   SUBROUTINE: CPtedTextBuffer::InsertLine(LPCTSTR pszLine, int nLength, int nPosition, DWORD flag)
c
c   FUNCTION:  insert a line into current text buffer
c
c   INPUT:  pszLine:line to be inserted
c			nLength: line length
c			nPosition: position to insert line
c			flag: insert line flag
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::InsertLine(LPCTSTR pszLine, int nLength /*= -1*/, int nPosition /*= -1*/, DWORD flag /*=0 */, int linkline /*-1*/)
{
	if (nLength == -1)
	{
		if (pszLine == NULL)
			nLength = 0;
		else
			nLength = lstrlen(pszLine);
	}

	SLineInfo li;
	if (flag!=0)
		li.m_dwFlags = flag;
	li.m_linkline = linkline;
	li.m_nLength = nLength;
	li.m_nMax = LINE_MAX_CHARS;
	ASSERT(li.m_nMax >= li.m_nLength);

	li.m_spaces = nLength * 1.50;
	if (li.m_spaces>li.m_nMax)
		li.m_spaces = li.m_nMax;
	if (li.m_nMax > 0)
		li.m_pcLine = new TCHAR[li.m_spaces];
	if (li.m_nLength > 0)
		memcpy(li.m_pcLine, pszLine, sizeof(TCHAR) * li.m_nLength);

	if (nPosition == -1)
	{
		if (m_current_size<m_max_size)
		{
			m_aLines[m_current_size] = li;
		}
		else
			m_aLines.SetAtGrow(m_current_size, li);
		m_current_size++;
	}
	else
	{
		if ((m_current_size==nPosition) && (m_current_size<m_max_size))
			m_aLines[nPosition] = li;
		else
			m_aLines.InsertAt(nPosition, li);
		m_current_size++;
	}
}

/***********************************************************************
c
c   SUBROUTINE: AppendLine(int nLineIndex, LPCTSTR pszChars, int nLength, int &newline, int &newchar)
c
c   FUNCTION:  Append a line into a line in the text buffer
c
c   INPUT:  nLineIndex:line index to be appended
c			pszChars: line string to be appended
c			nLength: line string length
c
c   OUTPUT: newline: new line number after appended
c			newchar: new chararter number (horizon) after appended
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextBuffer::AppendLine(int nLineIndex, LPCTSTR pszChars, int nLength, int &newline, int &newchar)
{
	int tempPos = 0;
	DWORD flag = 0;
	newline = nLineIndex;
	newchar = m_aLines[nLineIndex].m_nLength;
	if (nLength == -1)
	{
		if (pszChars == NULL)
			return;
		nLength = lstrlen(pszChars);
	}

	if (nLength == 0)
		return;

	int nCurrentLine = nLineIndex;
	int nTextPos,old_llen, insert;
	register SLineInfo &li = m_aLines[nLineIndex];
	int nBufNeeded = li.m_nLength + nLength;
	if (nBufNeeded < LINE_MAX_CHARS)
		goto AppendLine;
/*
.....for longer line
*/
	tempPos = 0;
	nTextPos = 0;
	insert = 0;
	for (;;)
	{
		nTextPos = 0;
		if (nCurrentLine == nLineIndex)
			old_llen = m_aLines[nCurrentLine].m_nLength;
		else
			old_llen = 0;
		while (pszChars[nTextPos+tempPos] != 0 && pszChars[nTextPos+tempPos] != _T('\r')
				&& (nTextPos + old_llen < LINE_MAX_CHARS - 1) && (nTextPos+tempPos<nLength))
			nTextPos ++;
		if (nCurrentLine == nLineIndex)
		{
			if (insert==1)
				InternalInsertText(nCurrentLine, 0, &(pszChars[tempPos]), nTextPos, newline, newchar,0);
			else
				AppendLine(nLineIndex, &(pszChars[tempPos]), nTextPos, newline,newchar);
			if ((newchar==LINE_MAX_CHARS - 1) || (newline!=nLineIndex))
			{
				insert = 0;
				nLineIndex = newline + 1;
				if (m_current_size>nLineIndex)
				{
					flag = GetLineFlags(nLineIndex);
					if ((flag & PTED_APPEND_LINE) == 0)
					{
						InsertLine("", 0, nLineIndex, PTED_APPEND_LINE);
					}
					insert = 1;
				}
				else
				{
					InsertLine("", 0, nLineIndex, PTED_APPEND_LINE);
					insert = 1;
				}
				tempPos += nTextPos;
				nTextPos = 0;
			}
		}
		else
		{
			InsertLine(&(pszChars[tempPos]), nCurrentLine, -1, flag);
			insert = 0;
		}
		if ( (nTextPos+tempPos>=nLength) || (pszChars[nTextPos+tempPos] == 0))
		{
			newline = nCurrentLine;
			newchar = m_aLines[nCurrentLine].m_nLength;
			break;
		}
		nCurrentLine ++;
/*
.....which mean we cut the line by LINE_MAX_CHARS
*/
		if (pszChars[nTextPos+tempPos] != 0 && pszChars[nTextPos+tempPos] != _T('\r'))
		{
/*			flag = PTED_APPEND_LINE;
			nLineIndex++;
			if (m_current_size>nLineIndex)
			{
				flag = GetLineFlags(nLineIndex);
				if ((flag & PTED_APPEND_LINE) == 0)
					InsertLine("", 0, nLineIndex, PTED_APPEND_LINE);
			}
			else
				InsertLine("", 0, nLineIndex, PTED_APPEND_LINE);
			tempPos += nTextPos;
			nTextPos = 0;
*/			
			continue;
		}
		nTextPos ++;

		if (pszChars[nTextPos+tempPos] == _T('\n'))
		{
			nTextPos ++;
		}
		pszChars += nTextPos;
	}
	return;
AppendLine:;
/*
.....if the need alloc more space for longer line.
*/
	if (nBufNeeded > li.m_spaces)
	{
		li.m_nMax = LINE_MAX_CHARS;
		li.m_linkline = -1;
		ASSERT(li.m_nMax >= li.m_nLength + nLength);
		li.m_spaces = nBufNeeded*1.50;
		if (li.m_spaces>li.m_nMax)
			li.m_spaces = li.m_nMax;
		TCHAR *pcNewBuf = new TCHAR[li.m_spaces];
		if (li.m_nLength > 0)
			memcpy(pcNewBuf, li.m_pcLine, sizeof(TCHAR) * li.m_nLength);
		delete li.m_pcLine;
		li.m_pcLine = pcNewBuf;
	}
	memcpy(li.m_pcLine + li.m_nLength, pszChars, sizeof(TCHAR) * nLength);
	li.m_nLength += nLength;
	newline = nLineIndex;
	newchar = li.m_nLength;
	ASSERT(li.m_nLength <= li.m_nMax);
}

void CPtedTextBuffer::FreeAll()
{
	int I;

	for (I = 0; I < m_current_size; I ++)
	{
		if (m_aLines[I].m_nMax > 0)
			delete m_aLines[I].m_pcLine;
	}
	m_aLines.RemoveAll();
	m_current_size = 0;
	m_max_size = 0;

	int nBufSize = m_aUndoBuf.GetSize();
	for (I = 0; I < nBufSize; I ++)
	{
		m_aUndoBuf[I].FreeText();
		m_aUndoBuf[I].FreeBuffer();
	}
	m_aUndoBuf.RemoveAll();

	m_bInit = FALSE;
}

/***********************************************************************
c
c   SUBROUTINE:  InitNew(int fStyle)
c
c   FUNCTION:  This function initial view with new text buffer .
c
c   INPUT:  fStyle: file style: noe only FILE_STYLE_DOS
c			
c   OUTPUT: True
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::InitNew(int fStyle /*= FILE_STYLE_DOS*/)
{
	ASSERT(! m_bInit);
//	ASSERT(m_aLines.GetSize() == 0);
	ASSERT(m_current_size == 0);
	ASSERT(fStyle >= 0 && fStyle <= 2);
	m_aLines.SetSize(5000, 5000);
	InsertLine(_T(""));
	m_bInit = TRUE;
	m_bReadOnly = FALSE;
	m_nFILEMode = fStyle;
	m_bModified = FALSE;
	m_nSyncPosition = m_nUndoPosition = 0;
	m_bUndoGroup = m_bUndoBeginGroup = FALSE;
	m_nUndoGroupSize = UNDO_GROUP_SIZE;
	m_nUndoBufSize = UNDO_ACTION_SIZE * m_nUndoGroupSize;
	ASSERT(m_aUndoBuf.GetSize() == 0);
	UpdateViews(NULL, UPDATE_RESET);
	return TRUE;
}

BOOL CPtedTextBuffer::GetReadOnly() const
{
	ASSERT(m_bInit);
	return m_bReadOnly;
}

void CPtedTextBuffer::SetReadOnly(BOOL bReadOnly /*= TRUE*/)
{
	ASSERT(m_bInit);	//	Text buffer not yet initialized.
						//	You must call InitNew() or LoadFromFile() first!
	m_bReadOnly = bReadOnly;
}

static const char *rdsvfs[] =
{
	"\x0d\x0a",			//	DOS/Windows style
	"\x0a\x0d",			//	UNIX style
	"\x0a"				//	Macintosh style
};

/***********************************************************************
c
c   SUBROUTINE: LoadCutterFile(char *filename, char* msg, int *err)
c
c   FUNCTION:  This functions loads a cutter file into current edit window 
c
c   INPUT:  filename: file to load
c
c   OUTPUT: err: if loading error
c			msg: error message if error
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextBuffer::LoadCutterFile(char *filename, char* msg, int *err)
{
	int stat = LoadFromFile(filename, 6);
	if (stat)
	{
		LoadCutterData(err, msg);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  LoadCutterData(int *err, char *msg) 
c
c   FUNCTION:  Load the cutter data from the current cutter window,
c              if any into the fortran cutter common.
c
c   INPUT:  none
c   OUTPUT: err: if loading error
c			msg: error message if error
c	RETURN: none
c
c***********************************************************************
*/
void CPtedTextBuffer::LoadCutterData(int *err, char *msg)
{
	char *tmp;
	int i, nLength;
	ptd_tolinit();
	*err = 0;
	for (i=0; i<m_current_size;i++)
	{
		nLength = m_aLines[i].m_nLength;
		if (nLength>0)
		{
			tmp = T2A(m_aLines[i].m_pcLine);
			ptd_toldo(tmp, &nLength, err);
			if (*err)
			{
				strcpy (msg, "Syntax error: ");
				strncat(msg,tmp,nLength);
				msg[nLength+14] = 0;
				break;
			}
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  LoadFromFile(LPCTSTR pszFileName, int ftype, int fStyle)
c
c   FUNCTION:  This function load the file into the current edit text buffer 
c             
c
c   INPUT:  pszFileName: file to be load
c			ftype: file type to be load
c			fStyle: file style: now only FILE_STYLE_DOS
c
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::LoadFromFile(LPCTSTR pszFileName, int ftype, int fStyle /*= FILE_STYLE_AUTOMATIC*/)
{
	int lines_num;
	int i=0,I;
	struct _stat fileInfo;
	char *tmpstr = NULL;

	HANDLE hFile = NULL;
	int nLength, nCurrentMax = LINE_MAX_CHARS;
	char *pcLineBuf = new char[nCurrentMax];

	BOOL bSuccess = FALSE;

	char *filen;
	if (pszFileName == NULL)
	{
		bSuccess = FALSE;
		goto done;
	}
	TCHAR filet[LINE_MAX_CHARS];
	nLength = lstrlen(pszFileName);

	memcpy(filet, pszFileName, sizeof(TCHAR) * nLength);
	filen = T2A(filet);
	filen[nLength] = '\0';


	if (ftype==3)
	{
		Ptd_clload(filen, &tmpstr);
		if (tmpstr) 
		{
			lines_num = strlen(tmpstr)/25;
			lines_num = lines_num * 1.15;
			if (lines_num<=5000)
				lines_num = 5000;
			m_max_size = lines_num;
			m_current_size = 0;
			m_aLines.SetSize(lines_num, 5000);
			if (strlen(tmpstr)==0)
				InsertLine(_T(""));
			else
				LoadDataFromString(tmpstr);
			m_bInit = TRUE;
			m_bModified = FALSE;
			m_bUndoGroup = m_bUndoBeginGroup = FALSE;
			m_nUndoGroupSize = UNDO_GROUP_SIZE;
			m_nUndoBufSize = UNDO_ACTION_SIZE * m_nUndoGroupSize;
			m_nSyncPosition = m_nUndoPosition = 0;
			ASSERT(m_aUndoBuf.GetSize() == 0);

			UpdateViews(NULL, UPDATE_RESET);
			bSuccess = TRUE;
		}
		if (tmpstr)
			free (tmpstr);
		return 1;
	}

	if (pszFileName[0]=='\0')
	{
		bSuccess = FALSE;
		goto done;
	}		
	else if ((pszFileName[0]=='\\')|| (pszFileName[0]=='/'))
	{
		strcpy(filen, "c:\\");
		strcat(filen, &(pszFileName[1]));
	}
	__try
	{
		DWORD dwFileAttributes = ::GetFileAttributes(filen);
		if (dwFileAttributes == (DWORD) -1)
			__leave;

		hFile = ::CreateFile(filen, GENERIC_READ, FILE_SHARE_READ, NULL,
					OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL);
		if (hFile == INVALID_HANDLE_VALUE)
			__leave;

		filen = T2A(filet);
		filen[nLength] = '\0';
		if (_stat(filen, &fileInfo) == 0)
			lines_num = (int(fileInfo.st_size))/25;
		else
			lines_num = 5000;

		lines_num = lines_num * 1.15;
		m_max_size = lines_num;
		m_current_size = 0;

		int nCurrentLength = 0;

		const DWORD dwBufSize = 32768;
		char *pcBuf = (char *) _alloca(dwBufSize);
		DWORD dwCurSize;
		if (! ::ReadFile(hFile, pcBuf, dwBufSize, &dwCurSize, NULL))
			__leave;

		if (fStyle == FILE_STYLE_AUTOMATIC)
		{
/*
......Try to determine current FILE mode
*/
			for (I = 0; I < dwCurSize; I ++)
			{
				if (pcBuf[I] == _T('\x0a'))
					break;
			}
			if (I == dwCurSize)
			{
/*
......By default (or in the case of empty file), set DOS style
*/
				fStyle = FILE_STYLE_DOS;
			}
			else
			{
/*
......Otherwise, analyse the first occurance of line-feed character
*/
				if (I > 0 && pcBuf[I - 1] == _T('\x0d'))
				{
					fStyle = FILE_STYLE_DOS;
				}
				else
				{
					if (I < dwCurSize - 1 && pcBuf[I + 1] == _T('\x0d'))
						fStyle = FILE_STYLE_UNIX;
					else
						fStyle = FILE_STYLE_MAC;
				}
			}
		}

		ASSERT(fStyle >= 0 && fStyle <= 2);
		m_nFILEMode = fStyle;
		const char *rdsvf = rdsvfs[fStyle];

		int tt = sizeof(SLineInfo);
		m_aLines.SetSize(lines_num, 5000);
		DWORD dwBufPtr = 0;
		int fPtr = 0;
		USES_CONVERSION;

		while (dwBufPtr < dwCurSize)
		{
			int c = pcBuf[dwBufPtr];
			dwBufPtr ++;
			if (dwBufPtr == dwCurSize && dwCurSize == dwBufSize)
			{
				if (! ::ReadFile(hFile, pcBuf, dwBufSize, &dwCurSize, NULL))
					__leave;
				dwBufPtr = 0;
			}

			pcLineBuf[nCurrentLength] = (char) c;
			nCurrentLength ++;
			if (nCurrentLength == nCurrentMax)
			{
				nCurrentMax += LINE_MAX_CHARS;
				char *pcNewBuf = new char[nCurrentMax];
				memcpy(pcNewBuf, pcLineBuf, nCurrentLength);
				delete pcLineBuf;
				pcLineBuf = pcNewBuf;
			}

			if ((char) c == rdsvf[fPtr])
			{
				fPtr ++;
				if (rdsvf[fPtr] == 0)
				{
					pcLineBuf[nCurrentLength - fPtr] = 0;
					InsertLine(A2T(pcLineBuf));
					nCurrentLength = 0;
					fPtr = 0;
				}
			}
			else
				fPtr = 0;
		}

		pcLineBuf[nCurrentLength] = 0;
		InsertLine(A2T(pcLineBuf));

		ASSERT(m_current_size > 0);		

		m_bInit = TRUE;
		m_bReadOnly = (dwFileAttributes & FILE_ATTRIBUTE_READONLY) != 0;
		m_bModified = FALSE;
		m_bUndoGroup = m_bUndoBeginGroup = FALSE;
		m_nUndoGroupSize = UNDO_GROUP_SIZE;
		m_nUndoBufSize = UNDO_ACTION_SIZE * m_nUndoGroupSize;
		m_nSyncPosition = m_nUndoPosition = 0;
		ASSERT(m_aUndoBuf.GetSize() == 0);

		UpdateViews(NULL, UPDATE_RESET);
		bSuccess = TRUE;
	}
	__finally
	{
		if (pcLineBuf != NULL)
			delete pcLineBuf;
		if (hFile != NULL)
			::CloseHandle(hFile);
	}
done:;
	if (bSuccess==FALSE)
	{
		InitNew();
	}
	return bSuccess;
}

/***********************************************************************
c
c   SUBROUTINE:  SaveToFile(LPCTSTR pszFileName, int ftype, int *classpt, 
c				int class_type, int fStyle, BOOL bClearModifiedFlag)
c
c   FUNCTION:  This function save the current edit text 
c              into a file fileName
c
c   INPUT:  fileName: save file
c			ftype: file type
c			class_type: batch or window
c			classpt: class pointer
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::SaveToFile(LPCTSTR pszFileName, int ftype, int *classpt, int class_type, int fStyle /*= FILE_STYLE_AUTOMATIC*/, BOOL bClearModifiedFlag /*= TRUE*/)
{
	ASSERT(fStyle == FILE_STYLE_AUTOMATIC || fStyle == FILE_STYLE_DOS||
			fStyle == FILE_STYLE_UNIX || fStyle == FILE_STYLE_MAC);
	ASSERT(m_bInit);
	HANDLE hTempFile = INVALID_HANDLE_VALUE;
	HANDLE hSearch = INVALID_HANDLE_VALUE;
	TCHAR szTempFileDir[_MAX_PATH + 1];
	TCHAR szTempFileName[_MAX_PATH + 1];
	TCHAR szBackupFileName[_MAX_PATH + 1];
	BOOL bSuccess = FALSE, bTemp = FALSE;
	char clafile[UX_MAX_PATH], *tempfile;

	if (ftype!=3)
	{
		strcpy(clafile, pszFileName);
	}
	else
	{
		bTemp = TRUE;
/*
......don't use tmpnam since this just generate a unique tmpnam, but it could
......be in top directory (such as c:) but have problems since
......since a lot of machine don't allowed to create a new file by default, 
......so I changed to use _tempnam (which will generate a unique name too)
......to have a tempname in local Pted directory
*/
//		tmpnam(clafile);
		tempfile = (char*)malloc(UX_MAX_PATH*sizeof(char));
		tempfile = _tempnam(Pted_localdir, "CRE");
		strcpy(clafile, tempfile);
		free(tempfile);
	}

	__try
	{
		TCHAR drive[_MAX_PATH], dir[_MAX_PATH], name[_MAX_PATH], ext[_MAX_PATH];
#ifdef _UNICODE
		_wsplitpath(clafile, drive, dir, name, ext);
#else
		_splitpath(clafile, drive, dir, name, ext);
#endif
		lstrcpy(szTempFileDir, drive);
		lstrcat(szTempFileDir, dir);
		lstrcpy(szBackupFileName, clafile);
		lstrcat(szBackupFileName, _T(".bak"));

		if (szTempFileDir[0]=='\0')
		{
/*
.....get current directory
*/
			strcpy(szTempFileDir, Pted_localdir);
		}
		else if ((szTempFileDir[0]=='\\')|| (szTempFileDir[0]=='/'))
		{
			lstrcpy(szTempFileDir, "c:\\");
		}
		if (::GetTempFileName(szTempFileDir, _T("CRE"), 0, szTempFileName) == 0)
			__leave;

		hTempFile = ::CreateFile(szTempFileName, GENERIC_WRITE, 0, NULL,
					CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
		if (hTempFile == INVALID_HANDLE_VALUE)
			__leave;

		if (fStyle == FILE_STYLE_AUTOMATIC)
			fStyle = FILE_STYLE_DOS;

		ASSERT(fStyle >= 0 && fStyle <= 2);
		const char *pszFILE = rdsvfs[fStyle];
		int nFILELength = strlen(pszFILE);

		int nLineCount = m_current_size;
		USES_CONVERSION;
		for (int nLine = 0; nLine < nLineCount; nLine ++)
		{
			int nLength = m_aLines[nLine].m_nLength;
			DWORD dwWrittenBytes;
			if (nLength > 0)
			{
				if (! ::WriteFile(hTempFile, T2A(m_aLines[nLine].m_pcLine), nLength, &dwWrittenBytes, NULL))
					__leave;
				if (nLength != (int) dwWrittenBytes)
					__leave;
			}
			if (nLine < nLineCount - 1)	
			{
				if (! ::WriteFile(hTempFile, pszFILE, nFILELength, &dwWrittenBytes, NULL))
					__leave;
				if (nFILELength != (int) dwWrittenBytes)
					__leave;
			}
		}
		::CloseHandle(hTempFile);
		hTempFile = INVALID_HANDLE_VALUE;

		if (m_bCreateBackupFile)
		{
			WIN32_FIND_DATA wfd;
			hSearch = ::FindFirstFile(clafile, &wfd);
			if (hSearch != INVALID_HANDLE_VALUE)
			{
				::DeleteFile(szBackupFileName);
				if (! ::MoveFile(clafile, szBackupFileName))
					__leave;
				::FindClose(hSearch);
				hSearch = INVALID_HANDLE_VALUE;
			}
		}
		else
		{
			::DeleteFile(clafile);
		}

		if (! ::MoveFile(szTempFileName, clafile))
			__leave;
		int kerr = 0;
		char errMsg[UX_MAX_PATH+40], clfile[UX_MAX_PATH];
		if (ftype==3)
		{
			strcpy (clfile, pszFileName);
			Ptd_clsave(clafile, clfile, &kerr);
			if (kerr)
			{
				sprintf(errMsg, "Error: Unable to save file %s", clafile);
				Pted_Disp_Msg(errMsg, 1, classpt, class_type);
			}
		}

		if (bClearModifiedFlag)
		{
			SetModified(FALSE);
			m_nSyncPosition = m_nUndoPosition;
		}
		bSuccess = TRUE;
	}
	__finally
	{
		if (hSearch != INVALID_HANDLE_VALUE)
			::FindClose(hSearch);
		if (hTempFile != INVALID_HANDLE_VALUE)
			::CloseHandle(hTempFile);
		::DeleteFile(szTempFileName);
		if (bTemp) ::DeleteFile(clafile);
	}
	return bSuccess;
}



/***********************************************************************
c
c   SUBROUTINE:  SaveSelectToFile(char *filename, int ftype, 
c				CPoint &ptStart, CPoint &ptEnd, int *classpt, int class_type)
c
c   FUNCTION:  This function save the current edit text 
c              in certain range into a file fileName
c
c   INPUT:  fileName: save file
c			ftype: file type
c			class_type: batch or window
c			classpt: class pointer
c			ptStart, ptEnd: range to be save
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::SaveSelectToFile(LPCTSTR pszFileName, int ftype, CPoint &ptStart, CPoint &ptEnd, int *classpt, int class_type, int fStyle /*= FILE_STYLE_AUTOMATIC*/, BOOL bClearModifiedFlag /*= TRUE*/)
{
	ASSERT(fStyle == FILE_STYLE_AUTOMATIC || fStyle == FILE_STYLE_DOS||
			fStyle == FILE_STYLE_UNIX || fStyle == FILE_STYLE_MAC);
	ASSERT(m_bInit);
	HANDLE hTempFile = INVALID_HANDLE_VALUE;
	HANDLE hSearch = INVALID_HANDLE_VALUE;
	TCHAR szTempFileDir[_MAX_PATH + 1];
	TCHAR szTempFileName[_MAX_PATH + 1];
	TCHAR szBackupFileName[_MAX_PATH + 1];
	BOOL bSuccess = FALSE;
	char clafile[UX_MAX_PATH], *tempfile, line_str[LINE_MAX_CHARS], *tmp;

	if (ftype!=3)
	{
		strcpy(clafile, pszFileName);
	}
	else
	{
/*
......don't use tmpnam since this just generate a unique tmpnam, but it could
......be in top directory (such as c:) but have problems since
......since a lot of machine don't allowed to create a new file by default, 
......so I changed to use _tempnam (which will generate a unique name too)
......to have a tempname in local Pted directory
*/
//		tmpnam(clafile);
		tempfile = (char*)malloc(UX_MAX_PATH*sizeof(char));
		tempfile = _tempnam(Pted_localdir, "CRE");
		strcpy(clafile, tempfile);
		free(tempfile);
	}

	__try
	{
		TCHAR drive[_MAX_PATH], dir[_MAX_PATH], name[_MAX_PATH], ext[_MAX_PATH];
#ifdef _UNICODE
		_wsplitpath(clafile, drive, dir, name, ext);
#else
		_splitpath(clafile, drive, dir, name, ext);
#endif
		lstrcpy(szTempFileDir, drive);
		lstrcat(szTempFileDir, dir);
		lstrcpy(szBackupFileName, clafile);
		lstrcat(szBackupFileName, _T(".bak"));

		if (szTempFileDir[0]=='\0')
		{
/*
.....get current directory
*/
			strcpy(szTempFileDir, Pted_localdir);
		}
		if (::GetTempFileName(szTempFileDir, _T("CRE"), 0, szTempFileName) == 0)
			__leave;

		hTempFile = ::CreateFile(szTempFileName, GENERIC_WRITE, 0, NULL,
					CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
		if (hTempFile == INVALID_HANDLE_VALUE)
			__leave;

		if (fStyle == FILE_STYLE_AUTOMATIC)
			fStyle = FILE_STYLE_DOS;

		const char *pszFILE = rdsvfs[fStyle];
		int nFILELength = strlen(pszFILE);

		int nLineCount = ptEnd.y + 1;
		USES_CONVERSION;
		for (int nLine = ptStart.y; nLine < nLineCount; nLine ++)
		{
			int nLength = m_aLines[nLine].m_nLength;
			DWORD dwWrittenBytes;
			if (nLength > 0)
			{
				tmp = T2A(m_aLines[nLine].m_pcLine);
				if ((nLine==ptStart.y) && (ptStart.x!=0))
				{
					nLength = nLength - ptStart.x;
					strncpy(line_str, &(tmp[ptStart.x]), nLength);
					line_str[nLength] = '\0';
				}
				else if ((nLine==ptEnd.y) && (ptEnd.x!=nLength))
				{
					nLength = ptEnd.x;
					strncpy(line_str, &(tmp[0]), nLength);
					line_str[nLength] = '\0';
				}
				else
				{
					strncpy(line_str, &(tmp[0]), nLength);
					line_str[nLength] = '\0';
				}
				if (! ::WriteFile(hTempFile, line_str, nLength, &dwWrittenBytes, NULL))
					__leave;
				if (nLength != (int) dwWrittenBytes)
					__leave;
			}
			if (nLine < nLineCount - 1)	
			{
				if (! ::WriteFile(hTempFile, pszFILE, nFILELength, &dwWrittenBytes, NULL))
					__leave;
				if (nFILELength != (int) dwWrittenBytes)
					__leave;
			}
		}
		::CloseHandle(hTempFile);
		hTempFile = INVALID_HANDLE_VALUE;

		if (m_bCreateBackupFile)
		{
			WIN32_FIND_DATA wfd;
			hSearch = ::FindFirstFile(clafile, &wfd);
			if (hSearch != INVALID_HANDLE_VALUE)
			{
				::DeleteFile(szBackupFileName);
				if (! ::MoveFile(clafile, szBackupFileName))
					__leave;
				::FindClose(hSearch);
				hSearch = INVALID_HANDLE_VALUE;
			}
		}
		else
		{
			::DeleteFile(clafile);
		}

		if (! ::MoveFile(szTempFileName, clafile))
			__leave;

		char errMsg[UX_MAX_PATH+40], clfile[UX_MAX_PATH];
		int kerr = 0;
		if (ftype==3)
		{
			strcpy (clfile, pszFileName);
			Ptd_clsave(clafile, clfile, &kerr);
			if (kerr)
			{
				sprintf(errMsg, "Error: Unable to save file %s", clafile);
				Pted_Disp_Msg(errMsg, 1, classpt, class_type);
			}
		}

		if (bClearModifiedFlag)
		{
			SetModified(FALSE);
			m_nSyncPosition = m_nUndoPosition;
		}
		bSuccess = TRUE;
	}
	__finally
	{
		if (hSearch != INVALID_HANDLE_VALUE)
			::FindClose(hSearch);
		if (hTempFile != INVALID_HANDLE_VALUE)
			::CloseHandle(hTempFile);
		::DeleteFile(szTempFileName);
	}
	return bSuccess;
}

int CPtedTextBuffer::GetFILEMode()
{
	return m_nFILEMode;
}

void CPtedTextBuffer::SetFILEMode(int nFILEMode)
{
	ASSERT(nFILEMode == FILE_STYLE_DOS||
			nFILEMode == FILE_STYLE_UNIX ||
			nFILEMode == FILE_STYLE_MAC);
	m_nFILEMode = nFILEMode;
}

/***********************************************************************
c
c   SUBROUTINE: GetLineCount
c
c   FUNCTION:  Get Line Count
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: Line Count
c
c***********************************************************************
*/
int CPtedTextBuffer::GetLineCount()
{
	return m_current_size;
}

/***********************************************************************
c
c   SUBROUTINE: GetLineLength
c
c   FUNCTION:  Get Line Length
c
c   INPUT:  none
c			none
c
c   OUTPUT: none
c	RETURN: Line Length
c
c***********************************************************************
*/
int CPtedTextBuffer::GetLineLength(int nLine)
{
	ASSERT(m_bInit);
	return m_aLines[nLine].m_nLength;
}

/***********************************************************************
c
c   SUBROUTINE: GetLineChars(int nLineIndex)
c
c   FUNCTION:  Get Line characters
c
c   INPUT:  nLineIndex: line index
c
c   OUTPUT: none
c	RETURN: line characters
c
c***********************************************************************
*/
LPTSTR CPtedTextBuffer::GetLineChars(int nLine)
{
	ASSERT(m_bInit);	
	return m_aLines[nLine].m_pcLine;
}

/***********************************************************************
c
c   SUBROUTINE: GetLineFlags(int nLine)
c
c   FUNCTION:  Get Line flag
c
c   INPUT:  nLine: line index
c
c   OUTPUT: none
c	RETURN: line flag
c
c***********************************************************************
*/
DWORD CPtedTextBuffer::GetLineFlags(int nLine)
{
	ASSERT(m_bInit);	
					
	return m_aLines[nLine].m_dwFlags;
}

static int FlagToIndex(DWORD dwFlag)
{
	int nIndex = 0;
	while ((dwFlag & 1) == 0)
	{
		dwFlag = dwFlag >> 1;
		nIndex ++;
		if (nIndex == 32)
			return -1;
	}
	dwFlag = dwFlag & 0xFFFFFFFE;
	if (dwFlag != 0)
		return -1;
	return nIndex;

}

/***********************************************************************
c
c   SUBROUTINE:  FindLineWithFlag(DWORD dwFlag)
c
c   FUNCTION:  find the line index with certain flag
c
c   INPUT:  dwFlag: line flag
c   OUTPUT: none: 
c	RETURN: line index
c
c***********************************************************************
*/
int CPtedTextBuffer::FindLineWithFlag(DWORD dwFlag)
{
	for (int L = 0; L < m_current_size; L ++)
	{
		if ((m_aLines[L].m_dwFlags & dwFlag) != 0)
			return L;
	}
	return -1;
}

/***********************************************************************
c
c   SUBROUTINE:  GetLineWithFlag(DWORD dwFlag)
c
c   FUNCTION:  find the line index with certain flag
c
c   INPUT:  dwFlag: line flag
c   OUTPUT: none: 
c	RETURN: line index
c
c***********************************************************************
*/
int CPtedTextBuffer::GetLineWithFlag(DWORD dwFlag)
{
	int nFlagIndex = ::FlagToIndex(dwFlag);
	if (nFlagIndex < 0)
	{
		ASSERT(FALSE);
		return -1;
	}
	return FindLineWithFlag(dwFlag);
}

/***********************************************************************
c
c   SUBROUTINE:  SetLineFlag(int nLine, DWORD dwFlag, BOOL bSet, BOOL bRemoveFromPreviousLine)
c
c   FUNCTION:  Set the line Flag
c
c   INPUT:  nLine: line to set the flag
c			dwFlag: line flag to be set
c			bSet: set or unset the flag
c			bRemoveFromPreviousLine: if Remove From Previous Line
c   OUTPUT: none: 
c	RETURN: line index
c
c***********************************************************************
*/
void CPtedTextBuffer::SetLineFlag(int nLine, DWORD dwFlag, BOOL bSet, BOOL bRemoveFromPreviousLine /*= TRUE*/)
{
	ASSERT(m_bInit);	

	int nFlagIndex = ::FlagToIndex(dwFlag);
	if (nFlagIndex < 0)
	{
		ASSERT(FALSE);	
		return;
	}

	if (nLine == -1)
	{
		ASSERT(! bSet);
		nLine = FindLineWithFlag(dwFlag);
		if (nLine == -1)
			return;
		bRemoveFromPreviousLine = FALSE;
	}

	DWORD dwNewFlags = m_aLines[nLine].m_dwFlags;
	if (bSet)
		dwNewFlags = dwNewFlags | dwFlag;
	else
		dwNewFlags = dwNewFlags & ~dwFlag;

	if (m_aLines[nLine].m_dwFlags != dwNewFlags)
	{
		if (bRemoveFromPreviousLine)
		{
			int nPrevLine = FindLineWithFlag(dwFlag);
			if (bSet)
			{
				if (nPrevLine >= 0)
				{
					ASSERT((m_aLines[nPrevLine].m_dwFlags & dwFlag) != 0);
					m_aLines[nPrevLine].m_dwFlags &= ~dwFlag;
					UpdateViews(NULL, UPDATE_SINGLELINE | UPDATE_FLAGSONLY, nPrevLine);
				}
			}
			else
			{
				ASSERT(nPrevLine == nLine);
			}
		}

		m_aLines[nLine].m_dwFlags = dwNewFlags;
		UpdateViews(NULL, UPDATE_SINGLELINE | UPDATE_FLAGSONLY, nLine);
	}
}

/***********************************************************************
c
c   SUBROUTINE: GetText(int nStartLine, int nStartChar, int nEndLine, 
c				int nEndChar, CString &text, LPCTSTR pszFILE)
c
c   FUNCTION:  get the text from certain range
c
c   INPUT:  nStartLine, nStartChar, nEndLine,nEndChar: range o get text
c			pszFILE: end line char string, such as "\r\n"
c			
c   OUTPUT: text: text to get
c	RETURN: none
c
c***********************************************************************/
void CPtedTextBuffer::GetText(int nStartLine, int nStartChar, int nEndLine, int nEndChar, CString &text, LPCTSTR pszFILE /*= NULL*/)
{
	text = "";
/*
.....to the end
*/	
	if (nEndLine==-1)
		nEndLine = GetLineCount() - 1;
	if (nEndChar==-1)
		nEndChar = GetLineLength(nEndLine);

	if (pszFILE == NULL)
		pszFILE = rdsvf;
	int nFILELength = lstrlen(pszFILE);
	ASSERT(nFILELength > 0);

	int nBufSize = 0;
	for (int L = nStartLine; L <= nEndLine; L ++)
	{
		nBufSize += m_aLines[L].m_nLength;
		nBufSize += nFILELength;
	}

	if (nBufSize==0)
		return;
	LPTSTR pszBuf = text.GetBuffer(nBufSize);
	LPTSTR pszCurPos = pszBuf;

	if (nStartLine < nEndLine)
	{
		int nCount = m_aLines[nStartLine].m_nLength - nStartChar;
		if (nCount > 0)
		{
			memcpy(pszBuf, m_aLines[nStartLine].m_pcLine + nStartChar, sizeof(TCHAR) * nCount);
			pszBuf += nCount;
		}
		memcpy(pszBuf, pszFILE, sizeof(TCHAR) * nFILELength);
		pszBuf += nFILELength;
		for (int I = nStartLine + 1; I < nEndLine; I ++)
		{
			nCount = m_aLines[I].m_nLength;
			if (nCount > 0)
			{
				memcpy(pszBuf, m_aLines[I].m_pcLine, sizeof(TCHAR) * nCount);
				pszBuf += nCount;
			}
			memcpy(pszBuf, pszFILE, sizeof(TCHAR) * nFILELength);
			pszBuf += nFILELength;
		}
		if (nEndChar > 0)
		{
			memcpy(pszBuf, m_aLines[nEndLine].m_pcLine, sizeof(TCHAR) * nEndChar);
			pszBuf += nEndChar;
		}
	}
	else
	{
		int nCount = nEndChar - nStartChar;
		memcpy(pszBuf, m_aLines[nStartLine].m_pcLine + nStartChar, sizeof(TCHAR) * nCount);
		pszBuf += nCount;
	}
	pszBuf[0] = 0;
	text.ReleaseBuffer();
	text.FreeExtra();
}

void CPtedTextBuffer::AttachView(CPtedTextView *pView)
{
	m_attview = pView;
}
void CPtedTextBuffer::UpdateViews(CUpdateContext *pContext, DWORD dwUpdateFlags, int nLineIndex /*= -1*/)
{
	if (m_attview!=NULL)
		m_attview->UpdateView(pContext, dwUpdateFlags, nLineIndex);
}
