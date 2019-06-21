/************************************************************************
c
c   FILE NAME: PtedTextBuffer3.cpp
c
c   CONTAINS:
c		CPtedTextBuffer::AddUndoRecord(BOOL bInsert, const CPoint &ptStartPos, CPtedTextBuffer *include_buf, 
c			CPoint &ptStart, CPoint &ptEnd, int nActionType)
c		CPtedTextBuffer::Get_Range_Pos(PtedRangeStruct* cRange, 
c						CPoint &ptStart, CPoint & ptEnd)
c		CPtedTextBuffer::FindStrAddrInBlock(LPCTSTR findstr, char*FindAddr,  CPoint &ptCurrentPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, int *flen, double *fval, CPoint *pptFoundPos)
c		CPtedTextBuffer::FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPosition, 
c									   CPoint &ptBlockBegin, CPoint &ptBlockEnd,
c										DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos)
c		CPtedTextBuffer::ConvertMathRange(char **adds, int num, CPoint &ptStart, CPoint &ptEnd, int mflag)
c		CPtedTextBuffer::ConvertRange(CPoint &ptStart, CPoint &ptEnd)
c		CPtedTextBuffer::BadCommand(CPtedTextBuffer *text_buffer)
c		CPtedTextBuffer::ConvertBadBlockRange(CPoint &ptStart, CPoint &ptEnd, int ftype, CPtedTextBuffer *text_buffer)
c		CPtedTextBuffer::ConvertFormatRange(CPoint &ptStart, CPoint &ptEnd)
c		CPtedTextBuffer::SetSize(int size)
c		CPtedTextBuffer::ConvertUnFormatRange(CPoint &ptStart, CPoint &ptEnd)
c		CPtedTextBuffer::SetUndoFlag(int flag)
c		CPtedTextBuffer::GetTextLength(int nStartLine, int nStartChar, int nEndLine, int nEndChar, LPCTSTR pszFILE)
c		CPtedTextBuffer::ReseqRange(CPoint &ptStart, CPoint &ptEnd, int bseq, int seqinc,int seqn, int nonly)
c		CPtedTextBuffer::NctoSimfile(char *file, CPtedTextBuffer *text_buffer)
c		CPtedTextBuffer::ConvertNctoapt(CPtedTextBuffer *text_buffer)
c		CPtedTextBuffer::TextReplace(CPoint &rStartpos, CPoint &rEndpos, char *rstr, CPoint &ptNewPos)
c		CPtedTextBuffer::LoadDataFromString(char *pszChars)
c		CPtedTextBuffer::EndNoUndo()
c		CPtedTextBuffer::BeginNoUndo()
c		CPtedTextBuffer::get_current_str(int *ln, char *ldat, int *nc)
c		extern "C" void ptd_getstr(int *bufpt, int *ln, char *ldat, int *nc)
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c	MODULE NAME AND RELEASE LEVEL
c       PtedTextBuffer3.cpp , 24.1
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
#include "PtedWindow.h"
#include <sys/stat.h>
#include <sys/types.h>
#include "PtdFunc.h"
#include "PtdGlobal.h"
#include <time.h>

#ifndef __AFXPRIV_H__
#include <afxpriv.h>
#endif

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

const TCHAR rdsvf[] = _T("\r\n");

#ifdef _DEBUG
#define _ADVANCED_BUGCHECK	1
#endif

typedef struct
{
	int item_size;          /* storage required by each item in bytes */
	int cur_cnt;            /* current item count */
	int max_cnt;            /* maximum item count */
	int exp_cnt;            /* expansion item count */
	char *data;             /* pointer to data array */
} Ptd_list;
#define PTD_LIST_ARRAY(list) ((*list).data)
#define PTD_LIST_INDEX(list,index) (&((list)->data[(index)*((list)->item_size)]))
#define PTD_END_OF_LIST(list) PTD_LIST_INDEX((list),((list)->cur_cnt))
extern "C" void Ptd_list_init(Ptd_list *list, int item_size, int init_cnt, int exp_cnt);
extern "C" void Ptd_list_free(Ptd_list *list);
extern "C" void Ptd_list_push(Ptd_list *list, char * item);
extern "C" void Ptd_list_push_multiple(Ptd_list *list, int count, char *items);
extern "C" void Ptd_list_compact(Ptd_list *list);


extern "C" int Pted_Disp_Msg(char *msg, int flag, int *classpt, int class_type);
extern "C" int Ptd_FindStringInTextLine(char* pszFindWhere, char* pszFindWhat, int bWholeWord);
extern "C" int Ptd_FindAddrInTextLine2(char* pszFindWhere, int kreg, double gval, int regonly, int* flen, double *fval,int *fpos);
extern "C" int Ptd_FindAddrInTextLine(char* pszFindWhere, char* pszFindAddr, int* flen, double *fval,int *fpos);
extern "C" void Pted_Display_as_percent(int num, CPtedWindow *parent);

void CPtedTextBuffer::AddUndoRecord(BOOL bInsert, const CPoint &ptStartPos, CPtedTextBuffer *include_buf, 
			CPoint &ptStart, CPoint &ptEnd, int nActionType)
{

	int i,j;

	if (m_no_undo)
		return;
	int inc_lines = include_buf->GetLineCount();
	if (inc_lines==0)
		return;
	CPoint ptEndPos;
	int endchar, endline;
	CString include_text;
	endline = inc_lines-1;
	endchar = include_buf->GetLineLength(inc_lines-1);
	CArray <SLineInfo, SLineInfo&> m_newLines;
	CArray <SLineInfo, SLineInfo&> m_incLines;
	SLineInfo temp;
	int new_size = ptEnd.y - ptStart.y + 1;

	CPtedTextBuffer *temp_buf = new CPtedTextBuffer();
	temp_buf->InitNew();
	temp_buf->SetSize(new_size);

	for (j=0, i=ptStart.y; i<=ptEnd.y; i++, j++)
	{
		temp_buf->m_aLines[j].m_nLength = include_buf->m_aLines[i].m_nLength;
		temp_buf->m_aLines[j].m_spaces = include_buf->m_aLines[i].m_spaces;
		temp_buf->m_aLines[j].m_nMax = LINE_MAX_CHARS;
		temp_buf->m_aLines[j].m_linkline = -1;
		temp_buf->m_aLines[j].m_pcLine = new TCHAR[temp_buf->m_aLines[j].m_spaces];
		memcpy(temp_buf->m_aLines[j].m_pcLine, include_buf->m_aLines[i].m_pcLine, sizeof(TCHAR) * (include_buf->m_aLines[i].m_nLength));
	}
	temp_buf->SetCurrentSize(new_size);
	ptEndPos.y = ptStartPos.y + j - 1;
	ptEndPos.x = include_buf->m_aLines[ptEnd.y].m_nLength;
	AddUndoRecord(bInsert, ptStartPos, ptEndPos, NULL, temp_buf, nActionType);
	delete temp_buf;
}

/***********************************************************************
c
c   SUBROUTINE:  Get_Range_Pos(PtedRangeStruct* cRange, 
c					CPoint &ptStart, CPoint & ptEnd)
c
c   FUNCTION:  This function Get Range start and end position
c			
c
c   INPUT:  
c			PtedRangeStruct* cRange:  
c
c   OUTPUT: CPoint ptStart: rang start position (line, char)
c			CPoint ptEnd: range end position (line, char)
c			blin: range start line
c			elin: range end line
c			rflag:	1: return position include '\r' chars
c					0: return position not include '\r' chars
c
c***********************************************************************
*/

void CPtedTextBuffer::Get_Range_Pos(PtedRangeStruct* cRange, 
		CPoint &ptStart, CPoint & ptEnd)
{
	int stat, flen;
	CPoint ptFindPos;
	double fval;

	ptStart.x = 0;
	ptStart.y = 0;
	ptEnd.y = GetLineCount() - 1;
	ptEnd.x = GetLineLength(ptEnd.y);
	
	if (cRange->begin==1)
	{
		return;
	}
	else if (cRange->begin==4)
	{
		ptStart.x = 0;
		ptStart.y = 0;
	}
	else if (cRange->begin==5)
	{
		char *out;
		out = new char[200];
		int ftype;
		Ptd_GetFindStr(cRange->baddress, &out, 1, &ftype);
		DWORD dwFlags = 0;
		stat = FindStrAddrInBlock(NULL, out, ptStart, ptStart, ptEnd, dwFlags, &flen, &fval, &ptFindPos);
		if (stat)
			ptStart = ptFindPos;
		delete out;
	}
	else if (cRange->begin==6)
	{
		DWORD dwFlags = 0;
		stat = FindTextInBlock(cRange->bstring, ptStart, ptStart, ptEnd,
									dwFlags, 0, &ptFindPos);
		if (stat)
			ptStart = ptFindPos;
	}
	else if (cRange->begin==7)
	{
		ptStart.y = GetLineWithFlag(PTED_BOOKMARK(cRange->bline));
	}

	if (cRange->end==1)
	{
		ptEnd.y = ptStart.y + atoi(cRange->enumber) - 1;
		if (ptEnd.y>GetLineCount()-1) ptEnd.y = GetLineCount()-1;
		if (ptEnd.y<ptStart.y) ptEnd.y = ptStart.y;
		ptEnd.x = GetLineLength(ptEnd.y);
	}
	else if (cRange->end==2)
	{
		ptEnd.y = GetLineCount()-1;
		ptEnd.x = GetLineLength(ptEnd.y);
	}
	else if (cRange->end==3)
	{
		char *out;
		out = new char[200];
		int ftype;
		Ptd_GetFindStr(cRange->eaddress, &out, 1, &ftype);
		DWORD dwFlags = 0;
		stat = FindStrAddrInBlock(NULL, out, ptStart, ptStart, ptEnd, dwFlags, &flen, &fval, &ptFindPos);
		if (stat)
			ptStart = ptFindPos;
		delete out;
	}
	else if (cRange->end==4)
	{
		DWORD dwFlags = 0;
		stat = FindTextInBlock(cRange->bstring, ptStart, ptStart, ptEnd,
									dwFlags, 0, &ptFindPos);
		if (stat)
			ptStart = ptFindPos;
	}
	else if (cRange->end==5)
	{
		ptEnd.y = GetLineWithFlag(PTED_BOOKMARK(cRange->eline));
	}
}
/***********************************************************************
c
c   SUBROUTINE:  FindAddrInBlock(int kreg, double gval, int regonly, CPoint &ptCurrentPos, 
c					CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, int *flen, 
c					double *fval, CPoint *pptFoundPos)
c
c   FUNCTION:  This function find the letter adrress 
c
c   INPUT:  kreg: letter address register to be found
c			gval: letter address value to be found
c			regonly: register only flag (ignore the value if regonly=1)
c			ptCurrentPos: currect position to start searching
c           ptBlockBegin, ptBlockEnd:    Range to search
c			dwFlags: search flag
c   OUTPUT: flen: find letter address length
c			fval: find letter address value
c			pptFoundPos: found position
c   RETURN: true if found
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::FindAddrInBlock(int kreg, double gval, int regonly,CPoint &ptCurrentPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, int *flen, double *fval, CPoint *pptFoundPos)
{
	char line_str[LINE_MAX_CHARS], *temp;
	int nc;
	int find_addr = 1, find_str = 1;
	*flen = 0;

	*pptFoundPos = ptBlockBegin;

	ASSERT(ptBlockBegin.y < ptBlockEnd.y || ptBlockBegin.y == ptBlockEnd.y && 
		ptBlockBegin.x <= ptBlockEnd.x);
	if (ptBlockBegin == ptBlockEnd)
		return FALSE;
	if (dwFlags & FIND_DIRECTION_UP)
	{
		for (;;)
		{
			while (ptCurrentPos.y >= 0)
			{

				int nLineLength = GetLineLength(ptCurrentPos.y);
				nLineLength -= ptCurrentPos.x;
				if (nLineLength <= 0)
				{
					ptCurrentPos.x = 0;
					ptCurrentPos.y --;
					continue;
				}

				LPCTSTR pszChars = GetLineChars(ptCurrentPos.y);
				pszChars += ptCurrentPos.x;

				CString line;
				lstrcpyn(line.GetBuffer(nLineLength + 1), pszChars, nLineLength + 1);
				line.ReleaseBuffer();
				if ((dwFlags & FIND_MATCH_CASE) == 0)
					line.MakeUpper();
				nc = line.GetLength();
				temp = line.GetBuffer(nc);
				strcpy(line_str, temp);
				
				int stat, nPos;
				nPos = -1;
				if (find_addr)
					stat = Ptd_FindAddrInTextLine2(line_str, kreg, gval, regonly, flen, fval,&nPos);

				if (nPos>=0)		
				{
					ptCurrentPos.x += nPos;
					*pptFoundPos = ptCurrentPos;
					return TRUE;
				}
				ptCurrentPos.x = 0;
				ptCurrentPos.y --;
			}
			return FALSE;
		}
	}
	else
	{
		if ( (ptBlockBegin.x == GetLineLength(GetLineCount() - 1)) &&
				(ptBlockBegin.y == GetLineCount() - 1) )
			return FALSE;

		for (;;)
		{
			while (ptCurrentPos.y <= ptBlockEnd.y)
			{
				int nLineLength = GetLineLength(ptCurrentPos.y);
				nLineLength -= ptCurrentPos.x;
				if (nLineLength <= 0)
				{
					ptCurrentPos.x = 0;
					ptCurrentPos.y ++;
					continue;
				}

				LPCTSTR pszChars = GetLineChars(ptCurrentPos.y);
				pszChars += ptCurrentPos.x;

				CString line;
				lstrcpyn(line.GetBuffer(nLineLength + 1), pszChars, nLineLength + 1);
				line.ReleaseBuffer();
				line.MakeUpper();

				nc = line.GetLength();
				temp = line.GetBuffer(nc);
				strcpy(line_str, temp);
				int stat, nPos;
				nPos = -1;
				stat = Ptd_FindAddrInTextLine2(line_str, kreg, gval, regonly, flen, fval,&nPos);

				if (nPos>=0)		
				{
					ptCurrentPos.x += nPos;
					if (ptCurrentPos.y == ptBlockEnd.y && ptCurrentPos.x >= ptBlockEnd.x)
						break;
					*pptFoundPos = ptCurrentPos;
					return TRUE;
				}
				ptCurrentPos.x = 0;
				ptCurrentPos.y ++;
			}
			return FALSE;
		}
	}
	ASSERT(FALSE);
	return FALSE;
}



/***********************************************************************
c
c   SUBROUTINE:  FindStrAddrInBlock(LPCTSTR findstr, char *FindAddr, CPoint &ptCurrentPos, 
c					CPoint &ptBlockBegin, CPoint &ptBlockEnd, 
c					DWORD dwFlags,int *flen, double *fval,CPoint *pptFoundPos)
c
c   FUNCTION:  This function find the text or adrress whichever first
c				in specified range
c
c   INPUT:  findstr: text string to find
c			FindAddr: address to find
c			ptCurrentPos: currect position to start searching
c           ptBlockBegin, ptBlockEnd:    Range to search
c			dwFlags: search flag
c   OUTPUT: flen: find letter address length
c			fval: find letter address value
c			pptFoundPos: found position
c   RETURN: true if found
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::FindStrAddrInBlock(LPCTSTR findstr, char*FindAddr,  CPoint &ptCurrentPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, int *flen, double *fval, CPoint *pptFoundPos)
{
	char line_str[LINE_MAX_CHARS], *temp, find_txt[256];
	int nc;
	int find_addr = 1, find_str = 1;
	*flen = 0;

	if ( (FindAddr == NULL) || (strlen(FindAddr) == 0)) 
		find_addr = 0;
	if ((findstr==NULL) || (strlen(findstr) == 0)) 
		find_str = 0;
	if ((find_addr==0) && (find_str==0))
	{
		*pptFoundPos = ptBlockBegin;
		return 0;
	}
	if (find_str)
		strcpy(find_txt, findstr);

	ASSERT(ptBlockBegin.y < ptBlockEnd.y || ptBlockBegin.y == ptBlockEnd.y && 
		ptBlockBegin.x <= ptBlockEnd.x);
	if (ptBlockBegin == ptBlockEnd)
		return FALSE;
	if (dwFlags & FIND_DIRECTION_UP)
	{
		for (;;)
		{
			while (ptCurrentPos.y >= 0)
			{

				int nLineLength = GetLineLength(ptCurrentPos.y);
				nLineLength -= ptCurrentPos.x;
				if (nLineLength <= 0)
				{
					ptCurrentPos.x = 0;
					ptCurrentPos.y --;
					continue;
				}

				LPCTSTR pszChars = GetLineChars(ptCurrentPos.y);
				pszChars += ptCurrentPos.x;

				CString line;
				lstrcpyn(line.GetBuffer(nLineLength + 1), pszChars, nLineLength + 1);
				line.ReleaseBuffer();
				if ((dwFlags & FIND_MATCH_CASE) == 0)
					line.MakeUpper();
				nc = line.GetLength();
				temp = line.GetBuffer(nc);
				strcpy(line_str, temp);
				
				int stat, nPos, nPos1, nPos2;
				nPos = nPos1 = nPos2 = -1;
				if (find_str)
				{
					nPos1 = ::Ptd_FindStringInTextLine(line_str, find_txt, (dwFlags & FIND_WHOLE_WORD) != 0);
				}
				if (find_addr)
					stat = Ptd_FindAddrInTextLine(line_str, FindAddr, flen, fval,&nPos2);

				if (nPos1>=0)
					nPos = nPos1;
				if ((stat) && ((nPos<nPos2) || (nPos==-1)))
 					nPos = nPos2;							
				if (nPos>=0)		
				{
					ptCurrentPos.x += nPos;
					*pptFoundPos = ptCurrentPos;
					return TRUE;
				}
				ptCurrentPos.x = 0;
				ptCurrentPos.y --;
			}
			return FALSE;
		}
	}
	else
	{
		if ( (ptBlockBegin.x == GetLineLength(GetLineCount() - 1)) &&
				(ptBlockBegin.y == GetLineCount() - 1) )
			return FALSE;

		for (;;)
		{
			while (ptCurrentPos.y <= ptBlockEnd.y)
			{
				int nLineLength = GetLineLength(ptCurrentPos.y);
				nLineLength -= ptCurrentPos.x;
				if (nLineLength <= 0)
				{
					ptCurrentPos.x = 0;
					ptCurrentPos.y ++;
					continue;
				}

				LPCTSTR pszChars = GetLineChars(ptCurrentPos.y);
				pszChars += ptCurrentPos.x;

				CString line;
				lstrcpyn(line.GetBuffer(nLineLength + 1), pszChars, nLineLength + 1);
				line.ReleaseBuffer();
				line.MakeUpper();

				nc = line.GetLength();
				temp = line.GetBuffer(nc);
				strcpy(line_str, temp);
				int stat, nPos, nPos1, nPos2;
				nPos = nPos1 = nPos2 = -1;
				if (find_str)
				{
					nPos1 = ::Ptd_FindStringInTextLine(line_str, find_txt, (dwFlags & FIND_WHOLE_WORD) != 0);
				}
				if (find_addr)
					stat = Ptd_FindAddrInTextLine(line_str, FindAddr, flen, fval,&nPos2);

				if (nPos1>=0)
					nPos = nPos1;
				if ((stat) && ((nPos>nPos2) || (nPos==-1)))
					nPos = nPos2;							
				if (nPos>=0)		
				{
					ptCurrentPos.x += nPos;
					if (ptCurrentPos.y == ptBlockEnd.y && ptCurrentPos.x >= ptBlockEnd.x)
						break;
					*pptFoundPos = ptCurrentPos;
					return TRUE;
				}
				ptCurrentPos.x = 0;
				ptCurrentPos.y ++;
			}
			return FALSE;
		}
	}
	ASSERT(FALSE);
	return FALSE;
}

/***********************************************************************
c
c   SUBROUTINE:  FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPosition,
c					CPoint &ptBlockBegin, CPoint &ptBlockEnd, 
c					DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos)
c
c   FUNCTION:  This function find the text in specified range
c
c   INPUT:  pszText: text string to find
c			ptStartPosition: start searching position
c           ptBlockBegin, ptBlockEnd:    Range to search
c			dwFlags: search flag
c           bWrapSearch:    if search wrapped
c   OUTPUT: pptFoundPos: found position
c   RETURN: true if found
c
c***********************************************************************
*/
BOOL CPtedTextBuffer::FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPosition, 
									   CPoint &ptBlockBegin, CPoint &ptBlockEnd,
										DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos)
{
	int nc;
	char *temp;
	char line_str[LINE_MAX_CHARS], find_str[256];
	CPoint ptCurrentPos = ptStartPosition;

	ASSERT(pszText != NULL && lstrlen(pszText) > 0);
	ASSERT(ptBlockBegin.y < ptBlockEnd.y || ptBlockBegin.y == ptBlockEnd.y && 
		ptBlockBegin.x <= ptBlockEnd.x);
	if (ptBlockBegin == ptBlockEnd)
		return FALSE;

	if (ptCurrentPos.y < ptBlockBegin.y || ptCurrentPos.y == ptBlockBegin.y && 
		ptCurrentPos.x < ptBlockBegin.x)
		ptCurrentPos = ptBlockBegin;

	CString what = pszText;
	if ((dwFlags & FIND_MATCH_CASE) == 0)
		what.MakeUpper();
	nc = what.GetLength();
	temp = what.GetBuffer(nc);
	strcpy(find_str, temp);

	if (dwFlags & FIND_DIRECTION_UP)
	{
		for (;;)
		{
			while (ptCurrentPos.y >= 0)
			{
				int nLineLength = GetLineLength(ptCurrentPos.y);
				nLineLength -= ptCurrentPos.x;
				if (nLineLength <= 0)
				{
					ptCurrentPos.x = 0;
					ptCurrentPos.y --;
					continue;
				}

				LPCTSTR pszChars = GetLineChars(ptCurrentPos.y);
				pszChars += ptCurrentPos.x;

				CString line;
				lstrcpyn(line.GetBuffer(nLineLength + 1), pszChars, nLineLength + 1);
				line.ReleaseBuffer();
				if ((dwFlags & FIND_MATCH_CASE) == 0)
					line.MakeUpper();
				nc = line.GetLength();
				temp = line.GetBuffer(nc);
				strcpy(line_str, temp);
				int nPos = ::Ptd_FindStringInTextLine(line_str, find_str, (dwFlags & FIND_WHOLE_WORD) != 0);
				if (nPos >= 0)		
				{
					ptCurrentPos.x += nPos;
					*pptFoundPos = ptCurrentPos;
					return TRUE;
				}

				ptCurrentPos.x = 0;
				ptCurrentPos.y --;
			}

			if (! bWrapSearch)
				return FALSE;

			bWrapSearch = FALSE;
			ptCurrentPos = ptBlockEnd;
		}
	}
	else
	{
		for (;;)
		{
			while (ptCurrentPos.y <= ptBlockEnd.y)
			{
				int nLineLength = GetLineLength(ptCurrentPos.y);
				nLineLength -= ptCurrentPos.x;
				if (nLineLength <= 0)
				{
					ptCurrentPos.x = 0;
					ptCurrentPos.y ++;
					continue;
				}

				LPCTSTR pszChars = GetLineChars(ptCurrentPos.y);
				pszChars += ptCurrentPos.x;

				CString line;
				lstrcpyn(line.GetBuffer(nLineLength + 1), pszChars, nLineLength + 1);
				line.ReleaseBuffer();
				if ((dwFlags & FIND_MATCH_CASE) == 0)
					line.MakeUpper();

				nc = line.GetLength();
				temp = line.GetBuffer(nc);
				strcpy(line_str, temp);
				int nPos = ::Ptd_FindStringInTextLine(line_str, find_str, (dwFlags & FIND_WHOLE_WORD) != 0);
				if (nPos >= 0)
				{
					ptCurrentPos.x += nPos;
					if (ptCurrentPos.y == ptBlockEnd.y && ptCurrentPos.x >= ptBlockEnd.x)
						break;

					*pptFoundPos = ptCurrentPos;
					return TRUE;
				}
				ptCurrentPos.x = 0;
				ptCurrentPos.y ++;
			}
			if (! bWrapSearch)
				return FALSE;
			bWrapSearch = FALSE;
			ptCurrentPos = ptBlockBegin;
		}
	}
	ASSERT(FALSE);
	return FALSE;
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertMathRange(char **adds, int num, PtedRangeStruct cRange, 
c					int mflag)
c				
c   FUNCTION:  Do math convert function in specified range
c
c   INPUT:  adds: array of input
c			ptStart, ptEnd:  range
c			num: number of input
c			mflag: which math funtion.
c				1:	ADD
c				2:	Mirror
c				3:	Multiply
c				4:	Rotate
c				5:	Scale
c				6:	Translate
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void CPtedTextBuffer::ConvertMathRange(char **adds, int num, CPoint &ptStart, CPoint &ptEnd, int mflag)
{
	char *tmp, *new_text, msg[256];
	int i, nLength,x,y,len;
	int nAction;
	char line_str[LINE_MAX_CHARS];
	int status, total,perc,flag;
	char *cout = (char *)malloc (512*sizeof(char));
	int nStartLine, nStartChar, nEndLine, nEndChar;
	int temp_line = 0,temp_char = 0;
	int nBufSize = 0;
	Ptd_list l1;
	CPtedWindow *window;
	if (m_attview!=NULL)
	{
		window = (CPtedWindow *)((CPtedTextView*)m_attview)->GetParent();
	}
	else
		window = NULL;
	if (m_no_undo==0)
	{
		int nFILELength = lstrlen(rdsvf);
		for (int L = ptStart.y; L <= ptEnd.y; L ++)
		{
			nBufSize += m_aLines[L].m_nLength;
			nBufSize += nFILELength;
		}
		Ptd_list_init (&l1, sizeof(char), nBufSize, nBufSize);
	}
	total = ptEnd.y - ptStart.y + 1;
	int count = 0;
	if (m_no_undo==0)
		flag = 1;
	else
		flag = 0;
	for (i=ptStart.y; i<ptEnd.y+1 + count;i++)
	{
		nLength = m_aLines[i].m_nLength;
		if (nLength>0)
		{
			tmp = T2A(m_aLines[i].m_pcLine);
			if (i==ptStart.y)
			{
				strncpy(line_str, &(tmp[ptStart.x]), nLength-ptStart.x);
				line_str[nLength-ptStart.x] = '\0';
			}
			else if (i==ptEnd.y)
			{
				strncpy(line_str, &(tmp[0]), ptEnd.x);
				line_str[ptEnd.x] = '\0';
			}
			else
			{
				strncpy(line_str, tmp, nLength);
				line_str[nLength] = '\0';
			}
			status = -1;
			cout[0] = '\0';
			if (mflag==1)
			{
				status = Ptd_Add(line_str, adds, num, &cout, msg);
				nAction = PTED_ADD;
			}
			else if (mflag==2)
			{
				status = Ptd_Mirror(line_str, adds, num, &cout, msg);
				nAction = PTED_MIR;
			}
			else if (mflag==3)
			{
				status = Ptd_Multiply(line_str, adds, num, &cout, msg);
				nAction = PTED_MULT;
			}
			else if (mflag==4)
			{
				status = Ptd_Rotate(line_str, adds, num, &cout, msg);
				nAction = PTED_ROTATE;
			}
			else if (mflag==5)
			{
				status = Ptd_Scale(line_str, adds, num, &cout, msg);
				nAction = PTED_SCALE;
			}
			else if (mflag==6)
			{
				status = Ptd_Trans(line_str, adds, num, &cout, msg);
				nAction = PTED_TRANS;
			}
			len = strlen(cout);
			if (status==-1)
			{
				Pted_Disp_Msg(msg, 1, (int*)m_attview, 2);
				free(cout);
				return;
			}
			if (cout[0]!='\0')
			{
				if (m_no_undo==0)
				{
					Ptd_list_push_multiple (&l1, len, cout);
				}
				else
				{
					nStartLine = nEndLine = i;
					nStartChar = 0;
					if (i==ptStart.y) nStartChar = ptStart.x;
					nEndChar = nLength;
					if (i==ptEnd.y) nEndChar = ptEnd.x;
					InternalDeleteText(nStartLine, nStartChar, nEndLine, nEndChar);
					InternalInsertText(nStartLine, nStartChar, cout, y, x);
					i += (y - nStartLine);
					count += (y - nStartLine);
				}
			}
			perc = 100*(i - ptStart.y)/(total+ count);
			Pted_Display_as_percent(perc, window);
		}
	}
	free (cout);
	if (m_no_undo==0)
	{
		BeginUndoGroup();
		DeleteText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x, PTED_FORMAT);
		Ptd_list_push (&l1, "\0");
		Ptd_list_compact (&l1);
		new_text = NULL;
		new_text = PTD_LIST_ARRAY(&l1);
		InsertText(ptStart.y, ptStart.x, new_text, y, x, PTED_FORMAT);
		FlushUndoGroup(m_attview);
		Ptd_list_free(&l1);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function convert all text in current window
c				in specified range
c
c   INPUT:  ptStart, ptEnd: range
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::ConvertRange(CPoint &ptStart, CPoint &ptEnd)
{
	char *tmp, *new_text;
	int i, nLength,x,y,len,istrt, iend;
	char line_str[LINE_MAX_CHARS];
	int status, total,perc,flag;
	char *cout = (char *)malloc (512*sizeof(char));
	int nStartLine, nStartChar, nEndLine, nEndChar;
	int temp_line = 0,temp_char = 0;
	int nBufSize = 0;
	Ptd_list l1;
	CPtedWindow *window;
	if (m_attview!=NULL)
		window = (CPtedWindow *)((CPtedTextView*)m_attview)->GetParent();
	else
		window = NULL;
	if (m_no_undo==0)
	{
		int nFILELength = lstrlen(rdsvf);
		for (int L = ptStart.y; L <= ptEnd.y; L ++)
		{
			nBufSize += m_aLines[L].m_nLength;
			nBufSize += nFILELength;
		}
		Ptd_list_init (&l1, sizeof(char), nBufSize, nBufSize);
	}
	total = ptEnd.y - ptStart.y + 1;
	int count = 0;
	if (m_no_undo==0)
		flag = 1;
	else
		flag = 0;
	istrt = 1;
	iend = 0;
	for (i=ptStart.y; i<ptEnd.y+1 + count;i++)
	{
		nLength = m_aLines[i].m_nLength;
		if (nLength>0)
		{
			tmp = T2A(m_aLines[i].m_pcLine);
			if (i==ptStart.y)
			{
				strncpy(line_str, &(tmp[ptStart.x]), nLength-ptStart.x);
				line_str[nLength-ptStart.x] = '\0';
			}
			else if (i==ptEnd.y)
			{
				strncpy(line_str, &(tmp[0]), ptEnd.x);
				line_str[ptEnd.x] = '\0';
			}
			else
			{
				strncpy(line_str, tmp, nLength);
				line_str[nLength] = '\0';
			}
			status = -1;
			cout[0] = '\0';
			if (i==ptEnd.y+count) iend = 1;
			Ptd_Convert(line_str, istrt, iend, &cout, flag);
			istrt = 0;
			len = strlen(cout);
			if (cout[0]!='\0')
			{
				if (m_no_undo==0)
				{
					Ptd_list_push_multiple (&l1, len, cout);
				}
				else
				{
					nStartLine = nEndLine = i;
					nStartChar = 0;
					if (i==ptStart.y) nStartChar = ptStart.x;
					nEndChar = nLength;
					if (i==ptEnd.y) nEndChar = ptEnd.x;
					InternalDeleteText(nStartLine, nStartChar, nEndLine, nEndChar);
					InternalInsertText(nStartLine, nStartChar, cout, y, x);
					i += (y - nStartLine);
					count += (y - nStartLine);
				}
			}
			perc = 100*(i - ptStart.y)/(total+ count);
			Pted_Display_as_percent(perc, window);
		}
		else if (i == ptEnd.y+count)
		{
			line_str[0] = '\0';
			iend = 1;
			Ptd_Convert(line_str, istrt, iend, &cout, flag);
		}
	}
	free (cout);
	if (m_no_undo==0)
	{
		BeginUndoGroup();
		DeleteText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x, PTED_CONVERT);
		Ptd_list_push (&l1, "\0");
		Ptd_list_compact (&l1);
		new_text = NULL;
		new_text = PTD_LIST_ARRAY(&l1);
		InsertText(ptStart.y, ptStart.x, new_text, y, x, PTED_CONVERT);
		FlushUndoGroup(m_attview);
		Ptd_list_free(&l1);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  BadCommand(CPtedTextBuffer *text_buffer)
c				
c   FUNCTION:  Check the current file as APT description file
c				and put bad statments into the view text buffer if have any.
c
c   INPUT:  None
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
int CPtedTextBuffer::BadCommand(CPtedTextBuffer *text_buffer)
{
	char *tmp;
	int i, nLength,x,y,line_num = 0;
	char line_str[LINE_MAX_CHARS];
	int status, total,perc;
	char *cout;
	CPtedWindow *window;
	if (m_attview!=NULL)
	{
		window = (CPtedWindow *)((CPtedTextView*)m_attview)->GetParent();
	}
	else
		window = NULL;
	total = m_current_size;
	int temp_line = 0,temp_char = 0;
	for (i=0; i<total;i++)
	{
		cout = NULL;
		nLength = m_aLines[i].m_nLength;
		if (nLength>0)
		{
			tmp = T2A(m_aLines[i].m_pcLine);
			strncpy(line_str, tmp, nLength);
			line_str[nLength] = '\0';
			status = -1;
			Ptd_BadCommand(line_str, &cout, 0);
			if (cout!=NULL)
			{
				text_buffer->InternalInsertText(temp_line, temp_char, cout, y, x);
				temp_line = y;
				temp_char = x;
				line_num++;
				free(cout);
			}
			perc = 100*i/total;
			Pted_Display_as_percent(perc, window);
		}
	}
	if (line_num>0)
		return 1;
	else
		return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertBadBlockRange(CPoint &ptStart, CPoint &ptEnd, 
c					int ftype, , CPtedTextBuffer *text_buffer)
c
c   FUNCTION:  This function check the bad block and put the bad block into 
c				the view buffer
c
c   INPUT:  ptStart, ptEnd: range
c			ftype: file type
c   OUTPUT: text_buffer: with the bad block text
c
c***********************************************************************
*/
int CPtedTextBuffer::ConvertBadBlockRange(CPoint &ptStart, CPoint &ptEnd, int ftype, CPtedTextBuffer *text_buffer)
{
	char *tmp;
	int i,len, nLength,x,y,line_num = -1;
	char line_str[LINE_MAX_CHARS];
	int status, total,perc;
	char *cout = (char *)malloc (256*sizeof(char));
	char *new_text = NULL;
	CPoint ptSelStart, ptSelEnd;
	char *fdata;
	CString tmpstr;	
	
	if (ftype==2)
		goto badctl;
	if (ftype==4)
		goto badapt;
	Ptd_BadCL(&fdata, ptStart.y, ptEnd.y, (int *)this);
	if (strlen(fdata)==0)
/*
.....no bad blocks
*/
	{
		Pted_Disp_Msg("No bad blocks found", 3, (int*)m_attview, 2);
		free(fdata);
		return 0;
	}
	else
	{
		text_buffer->InternalInsertText(0, 0, fdata, y, x);
		text_buffer->SetModified(0);
		return 1;
	}
badapt:;
	Ptd_BadAPT(&fdata, ptStart.y, ptEnd.y, (int *)this);
	if ((fdata==NULL) || ((fdata!=NULL) && (strlen(fdata)==0)))
	{
		Pted_Disp_Msg("No bad blocks found", 3, (int*)m_attview, 2);
		return 0;
	}
	text_buffer->InternalInsertText(0, 0, fdata, y, x);
	text_buffer->SetModified(0);
	line_num = y;
	return 1;
badctl:;
	 CPtedWindow *window;
	if (m_attview!=NULL)
		window = (CPtedWindow *)((CPtedTextView*)m_attview)->GetParent();
	else
		window = NULL;
	Ptd_list l1;
	int nBufSize, nFILELength = lstrlen(rdsvf);
	nBufSize = 0;
	for (int L = ptStart.y; L <= ptEnd.y; L ++)
	{
		nBufSize += m_aLines[L].m_nLength;
		nBufSize += nFILELength;
	}
	Ptd_list_init (&l1, sizeof(char), nBufSize, nBufSize);
	int temp_line = 0,temp_char = 0;

	total = ptEnd.y - ptStart.y + 1;
	for (i=ptStart.y; i<ptEnd.y+1;i++)
	{
		cout[0] = '\0';
		nLength = m_aLines[i].m_nLength;
		if (nLength>0)
		{
			tmp = T2A(m_aLines[i].m_pcLine);
			ptSelStart.y = ptSelEnd.y = i;
			if (i==ptStart.y)
			{
				strncpy(line_str, &(tmp[ptStart.x]), nLength-ptStart.x);
				line_str[nLength-ptStart.x] = '\0';
				ptSelStart.x  = ptStart.x;
				ptSelEnd.x = nLength;
			}
			else if (i==ptEnd.y)
			{
				strncpy(line_str, &(tmp[0]), ptEnd.x);
				line_str[ptEnd.x] = '\0';
				ptSelStart.x  = 0;
				ptSelEnd.x = ptEnd.x;
			}
			else
			{
				strncpy(line_str, tmp,nLength);
				line_str[nLength] = '\0';
				ptSelStart.x  = 0;
				ptSelEnd.x = nLength;
			}
			status = -1;
			Ptd_BadCTL(line_str, &cout);
			len = strlen(cout);
			if (cout[0]!='\0')
			{
				Ptd_list_push_multiple (&l1, len, cout);
			}
			perc = 100*(i - ptStart.y)/total;
			Pted_Display_as_percent(perc, window);
		}
	}
	free(cout);
	Ptd_list_push (&l1, "\0");
	Ptd_list_compact (&l1);
	new_text = PTD_LIST_ARRAY(&l1);
	if ((new_text!=0) && (new_text[0]!='\0'))
	{
		text_buffer->InternalInsertText(temp_line, temp_char, new_text, y, x);
		text_buffer->SetModified(0);
		return 1;
	}
	else
	{
		Pted_Disp_Msg("No bad blocks found", 3, (int*)m_attview, 2);
		return 0;
	}
}

/***********************************************************************
c
c   SUBROUTINE:  ConvertFormatRange(CPoint &ptStart, CPoint &ptEnd)
c
c   FUNCTION:  This function Format all text in current view
c				in specified range
c
c   INPUT:  ptStart, ptEnd: range
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::ConvertFormatRange(CPoint &ptStart, CPoint &ptEnd)
{
	char *tmp, *new_text;
	int i, nLength,x,y,len;
	char line_str[LINE_MAX_CHARS];
	int status, total,perc,flag;
	char *cout = (char *)malloc (512*sizeof(char));
	int nStartLine, nStartChar, nEndLine, nEndChar;
	int temp_line = 0,temp_char = 0;
	int nBufSize = 0;
	Ptd_list l1;
	CPtedWindow *window;
	if (m_attview!=NULL)
	{
		window = (CPtedWindow *)((CPtedTextView*)m_attview)->GetParent();
	}
	else
		window = NULL;

	if (m_no_undo==0)
	{
		int nFILELength = lstrlen(rdsvf);
		for (int L = ptStart.y; L <= ptEnd.y; L ++)
		{
			nBufSize += m_aLines[L].m_nLength;
			nBufSize += nFILELength;
		}
		Ptd_list_init (&l1, sizeof(char), nBufSize, nBufSize);
	}
	total = ptEnd.y - ptStart.y + 1;
	int count = 0;
	if (m_no_undo==0)
		flag = 1;
	else
		flag = 0;
	for (i=ptStart.y; i<ptEnd.y+1 + count;i++)
	{
		nLength = m_aLines[i].m_nLength;
		if (nLength>0)
		{
			tmp = T2A(m_aLines[i].m_pcLine);
			if (i==ptStart.y)
			{
				strncpy(line_str, &(tmp[ptStart.x]), nLength-ptStart.x);
				line_str[nLength-ptStart.x] = '\0';
			}
			else if (i==ptEnd.y)
			{
				strncpy(line_str, &(tmp[0]), ptEnd.x);
				line_str[ptEnd.x] = '\0';
			}
			else
			{
				strncpy(line_str, tmp, nLength);
				line_str[nLength] = '\0';
			}
			status = -1;
			cout[0] = '\0';
			Ptd_Format(line_str, &cout, flag);
			len = strlen(cout);
			if (cout[0]!='\0')
			{
				if (m_no_undo==0)
				{
					Ptd_list_push_multiple (&l1, len, cout);
				}
				else
				{
					nStartLine = nEndLine = i;
					nStartChar = 0;
					if (i==ptStart.y) nStartChar = ptStart.x;
					nEndChar = nLength;
					if (i==ptEnd.y) nEndChar = ptEnd.x;
					InternalDeleteText(nStartLine, nStartChar, nEndLine, nEndChar);
					InternalInsertText(nStartLine, nStartChar, cout, y, x);
					i += (y - nStartLine);
					count += (y - nStartLine);
				}
			}
			perc = 100*(i - ptStart.y)/(total+ count);
			Pted_Display_as_percent(perc, window);
		}
	}
	free (cout);
	if (m_no_undo==0)
	{
		BeginUndoGroup();
		DeleteText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x, PTED_FORMAT);
		Ptd_list_push (&l1, "\0");
		Ptd_list_compact (&l1);
		new_text = NULL;
		new_text = PTD_LIST_ARRAY(&l1);
		InsertText(ptStart.y, ptStart.x, new_text, y, x, PTED_FORMAT);
		FlushUndoGroup(m_attview);
		Ptd_list_free(&l1);
	}
}

void CPtedTextBuffer::SetSize(int size)
{
	m_aLines.SetSize(size, 5000);
	m_max_size = size;
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertUnFormatRange(CPoint &ptStart, CPoint &ptEnd)
c
c   FUNCTION:  This function Unformat all text in current view
c				in specified range
c
c   INPUT:  ptStart, ptEnd: range
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::ConvertUnFormatRange(CPoint &ptStart, CPoint &ptEnd)
{
	char *tmp, *new_text;
	int i, nLength,x,y,len;
	char line_str[LINE_MAX_CHARS];
	int status, total,perc,flag;
	char *cout = (char *)malloc (512*sizeof(char));
	int nStartLine, nStartChar, nEndLine, nEndChar;
	int temp_line = 0,temp_char = 0;
	int nBufSize = 0;
	Ptd_list l1;
	CPtedWindow *window;
	if (m_attview!=NULL)
	{
		window = (CPtedWindow *)((CPtedTextView*)m_attview)->GetParent();
	}
	else
		window = NULL;

	if (m_no_undo==0)
	{
		int nFILELength = lstrlen(rdsvf);
		for (int L = ptStart.y; L <= ptEnd.y; L ++)
		{
			nBufSize += m_aLines[L].m_nLength;
			nBufSize += nFILELength;
		}
		Ptd_list_init (&l1, sizeof(char), nBufSize, nBufSize);
	}
	total = ptEnd.y - ptStart.y + 1;
	int count = 0;
	if (m_no_undo==0)
		flag = 1;
	else
		flag = 0;
	for (i=ptStart.y; i<ptEnd.y+1 + count;i++)
	{
		nLength = m_aLines[i].m_nLength;
		if (nLength>0)
		{
			tmp = T2A(m_aLines[i].m_pcLine);
			if (i==ptStart.y)
			{
				strncpy(line_str, &(tmp[ptStart.x]), nLength-ptStart.x);
				line_str[nLength-ptStart.x] = '\0';
			}
			else if (i==ptEnd.y)
			{
				strncpy(line_str, &(tmp[0]), ptEnd.x);
				line_str[ptEnd.x] = '\0';
			}
			else
			{
				strncpy(line_str, tmp, nLength);
				line_str[nLength] = '\0';
			}
			status = -1;
			cout[0] = '\0';
			Ptd_Unformat(line_str, &cout, flag);
			len = strlen(cout);
			if (cout[0]!='\0')
			{
				if (m_no_undo==0)
				{
					Ptd_list_push_multiple (&l1, len, cout);
				}
				else
				{
					nStartLine = nEndLine = i;
					nStartChar = 0;
					if (i==ptStart.y) nStartChar = ptStart.x;
					nEndChar = nLength;
					if (i==ptEnd.y) nEndChar = ptEnd.x;
					InternalDeleteText(nStartLine, nStartChar, nEndLine, nEndChar);
					InternalInsertText(nStartLine, nStartChar, cout, y, x);
					i += (y - nStartLine);
					count += (y - nStartLine);
				}
			}
			perc = 100*(i - ptStart.y)/(total+ count);
			Pted_Display_as_percent(perc, window);
		}
	}
	free (cout);
	if (m_no_undo==0)
	{
		BeginUndoGroup();
		DeleteText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x, PTED_UNFORMAT);
		Ptd_list_push (&l1, "\0");
		Ptd_list_compact (&l1);
		new_text = NULL;
		new_text = PTD_LIST_ARRAY(&l1);
		InsertText(ptStart.y, ptStart.x, new_text, y, x, PTED_UNFORMAT);
		FlushUndoGroup(m_attview);
		Ptd_list_free(&l1);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  SetUndoFlag(int flag)
c
c   FUNCTION:  This function set the undo flag
c				Called back for 'Active conversion undo'
c
c   INPUT:  flag
c   OUTPUT: none
c
c***********************************************************************
*/
int CPtedTextBuffer::SetUndoFlag(int flag)
{
	if (m_no_undo!=flag)
		m_no_undo = flag;
	if (m_no_undo)
	{
/*
.....delete undo buffer
*/
		int nBufSize = m_aUndoBuf.GetSize();
		for (int I = 0; I < nBufSize; I ++)
		{
			m_aUndoBuf[I].FreeText();
			m_aUndoBuf[I].FreeBuffer();
		}
		m_aUndoBuf.RemoveAll();
		m_nSyncPosition = m_nUndoPosition = 0;
		m_bUndoGroup = m_bUndoBeginGroup = FALSE;
	}
	return m_no_undo;
}
/***********************************************************************
c
c   SUBROUTINE:  GetTextLength(int nStartLine, int nStartChar, int nEndLine, int nEndChar, LPCTSTR pszFILE)
c
c   FUNCTION:  Get the text string line drom certain range
c
c   INPUT:  nStartLine, nStartChar, nEndLine, nEndChar: text range 
c			pszFILE: endline char string
c			
c   OUTPUT: none
c	RETURN: text length
c
c***********************************************************************
*/
/*
.....if nEndChar==-1, to end char
*/
int CPtedTextBuffer::GetTextLength(int nStartLine, int nStartChar, int nEndLine, int nEndChar, LPCTSTR pszFILE)
{
	if (pszFILE == NULL)
		pszFILE = rdsvf;
	int nFILELength = lstrlen(pszFILE);
	ASSERT(nFILELength > 0);

	int nBufSize = 0;
	for (int i = nStartLine; i <= nEndLine; i++)
	{
		if ((i==nStartLine) && (nStartChar!=-1))
			nBufSize += (m_aLines[i].m_nLength-nStartChar);
		else if ((i==nEndLine) && (nEndChar!=-1))
			nBufSize += nEndChar;
		else
			nBufSize += m_aLines[i].m_nLength;
		nBufSize += nFILELength;
	}
	return nBufSize;
}

/***********************************************************************
c
c   SUBROUTINE:  ReseqRange(PtedRangeStruct sRange, int bseq, int seqinc,
c				int seqn, int nonly)
c
c   FUNCTION:  This function Reseqence all text in current window
c				in specified range
c
c   INPUT:  ptStart, ptEnd: range
c			bseq:  beginning sequence number
c			seqinc: sequence increment
c			seqn:	output sequence number every seqn
c			nonly:  sequence numbered block only?		
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedTextBuffer::ReseqRange(CPoint &ptStart, CPoint &ptEnd, int bseq, int seqinc,int seqn, int nonly)
{
/*
....this function does not ReseqRange one by one is because of
....it will ReseqRange the whole range of the file
*/
	CString sText;
	int x,y;
	char *out = NULL;
	CPtedWindow *window;
	if (m_attview!=NULL)
	{
		window = (CPtedWindow *)((CPtedTextView*)m_attview)->GetParent();
	}
	else
		window = NULL;

//	GetText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x, sText);
//	Ptd_Resequence(sText.GetBuffer(1), bseq, seqinc, seqn, nonly, &out, (int*)window);
	Ptd_Resequence((int*)this, ptStart.y, ptEnd.y, bseq, seqinc, seqn, nonly, &out, (int*)window);
	if ((out!=NULL) &&(out[0]!='\0'))
	{
		if (m_no_undo)
		{
			InternalDeleteText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x);
			InternalInsertText(ptStart.y, ptStart.x, out, y, x);
		}
		else
		{
			BeginUndoGroup();
			DeleteText(ptStart.y, ptStart.x, ptEnd.y, ptEnd.x, PTED_RESEQUENCE);
			InsertText(ptStart.y, ptStart.x, out, y, x, PTED_RESEQUENCE);
			FlushUndoGroup(m_attview);
		}
		free (out);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertNctosim(char *file, CPtedTextBuffer *text_buffer, CPtedWindow *parent) 
c
c   FUNCTION:  This function converts the current control data file
c              into a simultation file .
c
c   INPUT:  file: current NC filename
c			parent: parent window of new view to be generate with simulnation file
c			
c   OUTPUT: 
c			text_buffer: new text buffer to be generate with simulnation file
c
c***********************************************************************
*/
int CPtedTextBuffer::NctoSimfile(char *file, CPtedTextBuffer *text_buffer, CPtedWindow *parent)
{
/*
....this function does not convert apt one by one is because of
....it will took much longer time because of list-handle inside NctoSimfile
....if we used it. For now, this is better
*/
	CString sText;
	int nEndLine, nEndChar;
	char *out = NULL;
	int nLength = m_aLines[m_current_size-1].m_nLength;
	GetText(0, 0, m_current_size-1, nLength, sText);
	Ptd_nctosimfile(sText.GetBuffer(1), file, &out, (int*)parent);
	if ((out!=NULL) &&(out[0]!='\0'))
	{
		text_buffer->InternalInsertText(0, 0, out, nEndLine, nEndChar);
		return 0;
	}
	else
		return 1;
}
/***********************************************************************
c
c   SUBROUTINE:  ptd_getstr(int *bufpt, int *ln, char *ldat, int *nc)
c
c   FUNCTION:  Get the string from pertical line in a text buffer 
c
c   INPUT:  bufpt: text buffer pointer 
c			ln: line index
c			
c   OUTPUT: 
c			ldat: line string
c			nc: string length
c
c***********************************************************************
*/
extern "C" void ptd_getstr(int *bufpt, int *ln, char *ldat, int *nc)
{
	CPtedTextBuffer * buf = (CPtedTextBuffer*) bufpt;
	buf->get_current_str(ln, ldat, nc);
}
/***********************************************************************
c
c   SUBROUTINE:  get_current_str(int *ln, char *ldat, int *nc)
c
c   FUNCTION:  Get the string from pertical line 
c
c   INPUT:  
c			ln: line index
c			
c   OUTPUT: 
c			ldat: line string
c			nc: string length
c
c***********************************************************************
*/
void CPtedTextBuffer::get_current_str(int *ln, char *ldat, int *nc)
{
	*nc = m_aLines[*ln].m_nLength;			
	char *tmp = T2A(m_aLines[*ln].m_pcLine);			
	strncpy(ldat, tmp, *nc);
	ldat[*nc] = '\0';
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertNctoapt(CPtedTextBuffer* text_buffer, CPtedWindow *parent)
c
c   FUNCTION:  This function converts the current control data file
c              into a APT file .
c
c   INPUT:  
c			parent: parent window of new view to be generate with APT file
c			
c   OUTPUT: 
c			text_buffer: new text buffer to be generate with APT file
c
c***********************************************************************
*/
int CPtedTextBuffer::ConvertNctoapt(CPtedTextBuffer *text_buffer, CPtedWindow *parent)
{
/*
....this function does not convert apt one by one is because of
....it will took much longer time because of list-handle inside Ptd_NCtoAPT
....if we used it. For now, this is better
*/
	CString sText;
	int nEndLine, nEndChar;
	char *out = NULL;
	int nLength = m_aLines[m_current_size-1].m_nLength;
	GetText(0, 0, m_current_size-1, nLength, sText);

	Ptd_NCtoAPT(sText.GetBuffer(1), &out, (int *)parent);
	if ((out!=NULL) &&(out[0]!='\0'))
	{
		text_buffer->InternalInsertText(0, 0, out, nEndLine, nEndChar);
		return 1;
	}
	else
		return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  TextReplace(CPoint &rStartpos, CPoint &rEndpos, char *rstr, CPoint &ptNewPos)
c
c   FUNCTION:  TReplace the text from (rStartpos, rEndpos) with new text string rstr, and 
c				return the new ending position.
c
c   INPUT:  
c			(rStartpos, rEndpos): replace range
c			rstr: replace new string
c			
c   OUTPUT: 
c			ptNewPos: new ending position after replace
c
c***********************************************************************
*/
void CPtedTextBuffer::TextReplace(CPoint &rStartpos, CPoint &rEndpos, char *rstr, CPoint &ptNewPos)
{
	DeleteText(rStartpos.y, rStartpos.x, rEndpos.y, rEndpos.x, PTED_DELETE_SEL);
	int x, y;
	InsertText(rStartpos.y, rStartpos.x, rstr, y, x, PTED_REPLACE);
	ptNewPos.x = x;
	ptNewPos.y = y;
}

/***********************************************************************
c
c   SUBROUTINE:  LoadDataFromString(char *pszChars)
c
c   FUNCTION:  Load the text buffer data from a string
c
c   INPUT:  
c			pszChars: string to be load
c			
c   OUTPUT: 
c			true if loaded
c
c***********************************************************************
*/
int CPtedTextBuffer::LoadDataFromString(char *pszChars)
{
	int lines_num, nTextPos, newline, newchar;

	int nCurrentLine, nLineIndex;
	int nCurrentMax = LINE_MAX_CHARS;
	char *pcLineBuf = new char[nCurrentMax];

	BOOL bSuccess = FALSE;
	int len = lstrlen(pszChars);
	if (len==0)
		return bSuccess;
	else
		lines_num = len/25;

	lines_num = lines_num * 1.15;
	m_max_size = lines_num;
	m_current_size = 0;
	nCurrentLine = nLineIndex = 0;
	for (;;)
	{
		nTextPos = 0;
		while (pszChars[nTextPos] != 0 && pszChars[nTextPos] != _T('\r')
				&& (nTextPos < LINE_MAX_CHARS - 1))
			nTextPos ++;

//		if (nCurrentLine == nLineIndex)
//		{
//			AppendLine(nLineIndex, pszChars, nTextPos, newline,newchar);
//		}
//		else
		{
			InsertLine(pszChars, nTextPos);
		}

		if (pszChars[nTextPos] == 0)
		{
			newline = nCurrentLine;
			newchar = m_aLines[nCurrentLine].m_nLength;
			break;
		}
		nCurrentLine ++;
/*
.....which mean we cut the line by LINE_MAX_CHARS
*/
		if (pszChars[nTextPos] != 0 && pszChars[nTextPos] != _T('\r'))
			continue;

		nTextPos ++;

		if (pszChars[nTextPos] == _T('\n'))
		{
			nTextPos ++;
		}
		else
		{
			ASSERT(FALSE);			
		}
		pszChars += nTextPos;
	}
	return bSuccess;
}

void CPtedTextBuffer::EndNoUndo()
{
	m_no_undo = 0;
}

void CPtedTextBuffer::BeginNoUndo()
{
	m_no_undo = 1;
}
