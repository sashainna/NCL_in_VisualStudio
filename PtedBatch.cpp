/********************************************************************
*  NAME:  PtdBatch.cpp
**  Description:
**			all function defintion for class PtedBatch
**				
**    CONTAINS:
**			PtedBatch::ProgramSaveSelectAs(char *fname, PtedRangeStruct sRange)
**			PtedBatch::FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPosition, 
**									   CPoint &ptBlockBegin, CPoint &ptBlockEnd,
**										DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos)
**			PtedBatch::FindStrAddrInBlock(LPCTSTR findstr, char*FindAddr,  
**						CPoint &ptCurrentPos, CPoint &ptBlockBegin, 
**						CPoint &ptBlockEnd, DWORD dwFlags, int *flen, double *fval,
**						CPoint *pptFoundPos)
**			PtedBatch::FindAddrInBlock(int kreg, double gval, int regonly, CPoint &ptCurrentPos, 
**						CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, 
**						int *flen, double *fval, CPoint *pptFoundPos)
**			PtedBatch::ConvertRange(PtedRangeStruct* rangeStruct)
**			PtedBatch::FormatRange(PtedRangeStruct* rangeStruct)
**			PtedBatch::UnformatRange(PtedRangeStruct* rangeStruct)
**			PtedBatch::ReverseRange(PtedRangeStruct* rangeStruct)
**			PtedBatch::ReseqRange(PtedRangeStruct* rangeStruct,
**				int bseq, int seqinc, int seqn, int nonly)
**			PtedBatch::ProgramIncludeSelect(char* FileName, 
**							PtedRangeStruct sRange)
**			PtedBatch::ProgramLoadSelect(char* FileName, 
**							PtedRangeStruct sRange)
**			PtedBatch::OnReplaceText(char *fstr, char *rstr, int bCase, int bNext,
**							int fletter, PtedRangeStruct *pRange, int allflag, int vflag)
**			PtedBatch::ConvertMathRange(char **adds, int num, PtedRangeStruct cRange,
**				 int mflag)
**			LoadProgram(char *filename)
**			ProgramSaveAs( char *fileName)
**			OnConvertNctoapt() 
**			OnConvertNctosim()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c        PtedBatch.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:25
**********************************************************************/
#include "pwstdafx.h"
#include "Pted.h"
#include "PtedRangeBox.h"
#include "PtedBatch.h"
#include "PtdGlobal.h"
#include "PtdFunc.h"
#include "PtedProcessDlg.h"

#include <conio.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>

extern CPtedProcessDlg *Ptd_pdlg;
extern "C" int Pw_dispmsg(char *msgline, int flag);
extern "C" int Pted_Disp_Msg(char *msg, int flag, int *classpt, int class_type);
extern "C" void Pted_disply_ProcessDlg(char *title);
extern "C" void Pted_Display_Dlg_percent(int num);
extern "C" void Pted_Close_ProcessDlg();

/***********************************************************************
c
c   FUNCTION:  PtedBatch(char *name)
c
c              Constructor of class PtedBatch
c
c   INPUT:  name: class name
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

PtedBatch::PtedBatch(char *name)
{
	m_findstr[0] = '\0';
	m_replacestr[0] = '\0';
	m_case = 1;
	m_reg = 1;
	m_downdir = 1;
	m_modified = 0;
/*
.....load file type, default to .pu* file
.....2: control data file
*/
	m_ftype = 2;
	m_cursor_pos = 0;
	m_filen[0] = '\0';
	m_exit = 0;
	m_batch_cnt = 0;
	m_pTextBuffer = NULL;
	m_pParent = NULL;
	m_syntax_color = 1;
}

/***********************************************************************
c
c   FUNCTION:  ~PtedBatch()
c
c              Deconstructor of class PtdBatch
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
*********************************************************************
*/

PtedBatch::~PtedBatch()
{
/*
.....if batch continued, it mean the buffer asigned to window
.....so don't delete it
*/
	if ((m_batch_cnt==0)&&(m_pTextBuffer != NULL))
	{
		delete m_pTextBuffer;
		m_pTextBuffer = NULL;
	}
}
/***********************************************************************
c
c   SUBROUTINE:  ProgramSaveSelectAs(char *fname, PtedRangeStruct sRange)
c
c   FUNCTION:  This function save a select range of current text in to a file.
c				
c
c   INPUT:  sRange: range structure
c			fname:  filename to save as			
c
c   OUTPUT: none
c
c***********************************************************************
*/
int PtedBatch::ProgramSaveSelectAs(char *fname, PtedRangeStruct sRange)
{
	char *indx, tmp[UX_MAX_PATH];;
	
/*
.....if filename to file extension of ftype and input filename
*/
	if (fname[0]=='\0')
	{
		if (m_filen[0]=='\0')
			return 0;
		else
		{
			strcpy(fname, m_filen);
			indx = strchr(fname, '.');
			if (indx!=NULL)
				*indx = '\0';
			if (m_ftype==1)
				strcat(fname, ".cla");
			else if (m_ftype==2)
				strcat(fname, ".pu");
			else if (m_ftype==3)
				strcat(fname, ".cl");
			else if (m_ftype==4)
				strcat(fname, ".as");
			else if (m_ftype==5)
				strcat(fname, ".sim");
		}
	}
/*
.....if file have no extension, added following ftype
*/
	indx = strchr(fname, '.');
	if (indx==NULL)
	{
		if (m_ftype==1)
			strcat(fname, ".cla");
		else if (m_ftype==2)
			strcat(fname, ".pu");
		else if (m_ftype==3)
			strcat(fname, ".cl");
		else if (m_ftype==4)
			strcat(fname, ".as");
		else if (m_ftype==5)
			strcat(fname, ".sim");
	}
	else if (fname[0]=='.')
	{
		strcpy(tmp, m_filen);
		indx = strchr(tmp, '.');
		if (indx!=NULL)
			*indx = '\0';
/*
.....using the extension provide in fname
*/
		strcat(tmp, fname);
/*		if (m_ftype==1)
			strcat(tmp, ".cla");
		else if (m_ftype==2)
			strcat(tmp, ".pu");
		else if (m_ftype==3)
			strcat(tmp, ".cl");
		else if (m_ftype==4)
			strcat(tmp, ".as");
		else if (m_ftype==5)
			strcat(tmp, ".sim");
*/
		strcpy(fname, tmp);
	}
	CPoint ptStart, ptEnd;
	m_pTextBuffer->Get_Range_Pos(&sRange, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return 0;
	m_pTextBuffer->SaveSelectToFile(fname, m_ftype, ptStart, ptEnd, (int *)this, 0);
	return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function convert all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c   OUTPUT: none
c
c***********************************************************************
*/
void PtedBatch::ConvertRange(PtedRangeStruct* rangeStruct)
{
	CPoint ptStart, ptEnd;
	m_pTextBuffer->Get_Range_Pos(rangeStruct, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return;
/*	if (Ptd_pdlg!=NULL)
	{
		Ptd_pdlg->ShowWindow(SW_SHOW);
		return;
	}
	else
	{
		Pted_disply_ProcessDlg("Pted Input/Output Conversion");
		Pted_Display_Dlg_percent(1);
	}
*/
	m_pTextBuffer->ConvertRange(ptStart, ptEnd);
//	Pted_Close_ProcessDlg();
}
/***********************************************************************
c
c   SUBROUTINE:  FormatRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function Format all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c   OUTPUT: none
c
c***********************************************************************
*/
void PtedBatch::FormatRange(PtedRangeStruct* rangeStruct)
{
	CPoint ptStart, ptEnd;
	m_pTextBuffer->Get_Range_Pos(rangeStruct, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return;
/*	if (Ptd_pdlg!=NULL)
	{
		Ptd_pdlg->ShowWindow(SW_SHOW);
		return;
	}
	else
	{
		Pted_disply_ProcessDlg("Pted Format");
		Pted_Display_Dlg_percent(1);
	}
*/
	m_pTextBuffer->ConvertFormatRange(ptStart, ptEnd);
//	Pted_Close_ProcessDlg();
}
/***********************************************************************
c
c   SUBROUTINE:  UnformatRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function Unformat all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c   OUTPUT: none
c
c***********************************************************************
*/
void PtedBatch::UnformatRange(PtedRangeStruct* rangeStruct)
{
	CPoint ptStart, ptEnd;
	m_pTextBuffer->Get_Range_Pos(rangeStruct, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return;
/*	if (Ptd_pdlg!=NULL)
	{
		Ptd_pdlg->ShowWindow(SW_SHOW);
		return;
	}
	else
	{
		Pted_disply_ProcessDlg("Pted Unformat");
		Pted_Display_Dlg_percent(1);
	}
*/
	m_pTextBuffer->ConvertUnFormatRange(ptStart, ptEnd);
//	Pted_Close_ProcessDlg();
}
/***********************************************************************
c
c   SUBROUTINE:  ReverseRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function Reverse all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c   OUTPUT: none
c
c***********************************************************************
*/
void PtedBatch::ReverseRange(PtedRangeStruct* rangeStruct)
{
	CPoint ptStart, ptEnd;
	m_pTextBuffer->Get_Range_Pos(rangeStruct, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return;
/*	if (Ptd_pdlg!=NULL)
	{
		Ptd_pdlg->ShowWindow(SW_SHOW);
		return;
	}
	else
	{
		Pted_disply_ProcessDlg("Pted Reverse");
		Pted_Display_Dlg_percent(1);
	}
*/
	m_pTextBuffer->ReverseRange(ptStart, ptEnd);
//	Pted_Close_ProcessDlg();
}
/***********************************************************************
c
c   SUBROUTINE:  ReseqRange(PtedRangeStruct sRange, int bseq, int seqinc,
c				int seqn, int nonly)
c
c   FUNCTION:  This function Reseqence all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c			bseq:  beginning sequence number
c			seqinc: sequence increasement
c			seqn:	output sequence number every seqn
c			nonly:  sequence numbered block only?		
c
c   OUTPUT: none
c
c***********************************************************************
*/
void PtedBatch::ReseqRange(PtedRangeStruct* rangeStruct,
	int bseq, int seqinc, int seqn, int nonly)
{
	CPoint ptStart, ptEnd;
	m_pTextBuffer->Get_Range_Pos(rangeStruct, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return;
/*	if (Ptd_pdlg!=NULL)
	{
		Ptd_pdlg->ShowWindow(SW_SHOW);
		return;
	}
	else
	{
		Pted_disply_ProcessDlg("Pted Resequence");
		Pted_Display_Dlg_percent(1);
	}
*/
	m_pTextBuffer->ReseqRange(ptStart, ptEnd, bseq, seqinc,seqn,nonly);
//	Pted_Close_ProcessDlg();
}
/***********************************************************************
c
c   SUBROUTINE:  ProgramIncludeSelect(char *FileName, PtedRangeStruct fRange)
c
c   FUNCTION:  This function include a select range of a file into current text
c				
c
c   INPUT:  fRange: range structure
c			FileName:  filename to include			
c
c   OUTPUT: none
c
c***********************************************************************
*/
void PtedBatch::ProgramIncludeSelect(char* FileName, 
							PtedRangeStruct sRange)
{
	CPoint ptStart, ptEnd, m_ptCursorPos;
	int ftype;
	char *pp = strrchr(FileName, '.');
	if (pp!=NULL)
	{
		if (_stricmp(pp, ".cla")==0)
		{
			ftype = 1;
		}
		else if (_strnicmp(pp, ".pu", 3)==0)
		{
			ftype = 2;
		}
		else if ((_stricmp(pp, ".cl")==0)||(_stricmp(pp, ".cln")==0))
		{
			ftype = 3;
		}
		else if (_stricmp(pp, ".as")==0)
		{
			ftype = 4;
		}
		else if (_stricmp(pp, ".sim")==0)
		{
			ftype = 5;
		}
		else
		{
			ftype = 1;
		}
	}
	else
		ftype = 1;
	CPtedTextBuffer *include_buf = new CPtedTextBuffer();
	include_buf->LoadFromFile(FileName, ftype);
	include_buf->Get_Range_Pos(&sRange, ptStart, ptEnd);
	m_ptCursorPos.x = 0;
	m_ptCursorPos.y = 0;
	if (ptStart!=ptEnd)
		m_pTextBuffer->Includebuffer(include_buf, m_ptCursorPos, ptStart, ptEnd, PTED_INCLUDE);
	delete include_buf;
}
/***********************************************************************
c
c   SUBROUTINE: ProgramLoadSelect(char *FileName, PtedRangeStruct sRange)
c
c   FUNCTION:  This function load a file in apecified range into current edit window 
c
c   INPUT:  filename: file to load
c			sRange:   range structure
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void PtedBatch::ProgramLoadSelect(char* FileName, 
							PtedRangeStruct sRange)
{	
	if (strlen(FileName)==0)
		return;
	OnCloseReset();
	ProgramIncludeSelect(FileName, sRange);
}

/*********************************************************************
**    E_FUNCTION     : FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPosition,
**						   CPoint &ptBlockBegin, CPoint &ptBlockEnd,
**							DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos)
**       Search string "pszText" from current text buffer in certain range
**				
**    PARAMETERS
**       INPUT  : 
**				pszText: String to search
**				ptBlockBegin, ptBlockEnd: search range
**				dwFlags: searching flag
**				bWrapSearch: if Wrap search
**       OUTPUT :  pptFoundPos: found osition
**    RETURNS      : find position (0: not found)
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
BOOL PtedBatch::FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPosition, 
									   CPoint &ptBlockBegin, CPoint &ptBlockEnd,
										DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos)
{
	CPoint ptCurrentPos = ptStartPosition;

	if (ptBlockBegin == ptBlockEnd)
		return FALSE;
	return m_pTextBuffer->FindTextInBlock(pszText, ptStartPosition, ptBlockBegin, ptBlockEnd,
									dwFlags, bWrapSearch, pptFoundPos);
}

/*********************************************************************
**    E_FUNCTION     : FindStrAddrInBlock(LPCTSTR findstr, char*FindAddr,
**							CPoint &ptCurrentPos,
**						   CPoint &ptBlockBegin, CPoint &ptBlockEnd,
**							DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos)
**       Search string "findstr" or address "FindAddr" from current text buffer 
**			in certain range and return whichever found first
**				
**    PARAMETERS
**       INPUT  : 
**				findstr: String to search
**				FindAddr: address to search
**				ptCurrentPos: search starting position
**				ptBlockBegin, ptBlockEnd: search range
**				dwFlags: searching flag
**				bWrapSearch: if Wrap search
**       OUTPUT :  pptFoundPos: found osition
**    RETURNS      : find position (0: not found)
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
BOOL PtedBatch::FindStrAddrInBlock(LPCTSTR findstr, char*FindAddr,  CPoint &ptCurrentPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, int *flen, double *fval, CPoint *pptFoundPos)
{
	return m_pTextBuffer->FindStrAddrInBlock(findstr, FindAddr,  ptCurrentPos, ptBlockBegin, ptBlockEnd, dwFlags, flen, fval, pptFoundPos);
}

/*********************************************************************
**    E_FUNCTION     : FindAddrInBlock(int kreg, double gval, int regonly, 
**						CPoint &ptCurrentPos, CPoint &ptBlockBegin, 
**						CPoint &ptBlockEnd, DWORD dwFlags, 
**						int *flen, double *fval, CPoint *pptFoundPos)
**       Search a letetr address from current text buffer in certain range
**				
**    PARAMETERS
**       INPUT  : 
**				kreg: Letter address register number
**				gval: letter adrress value
**				regonly: if this is the register only, ignore gval if yes
**				ptBlockBegin, ptBlockEnd: search range
**				dwFlags: searching flag
**				flen: found address length
**				fval: find address value
**       OUTPUT :  pptFoundPos: found osition
**    RETURNS      : find position (0: not found)
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
BOOL PtedBatch::FindAddrInBlock(int kreg, double gval, int regonly, CPoint &ptCurrentPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd, DWORD dwFlags, int *flen, double *fval, CPoint *pptFoundPos)
{
	return m_pTextBuffer->FindAddrInBlock(kreg, gval, regonly,  ptCurrentPos, ptBlockBegin, ptBlockEnd, dwFlags, flen, fval, pptFoundPos);
}

/***********************************************************************
c
c   SUBROUTINE:  OnReplaceText(char *fstr, char *rstr, int bCase, int bNext,
		int fletter, PtedRangeStruct *pRange, int allflag, int vflag)
c				
c   FUNCTION:  Callback function for "Replace All" on findreplace dialog
c
c   INPUT:  fletter: match register
c			pRange:  range struction
c			fstr: find string
c			rstr: replace string
c			bCase:  case sensitivity
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void PtedBatch::OnReplaceText(char *fstr, char *rstr, int bCase, int bNext,
		int fletter, PtedRangeStruct *pRange, int allflag, int vflag)
{
	CPoint ptFindPos, ptCurrent, rStartpos, rEndpos;
	double fval;
	int flen, perc;
	m_reg = fletter;
	m_case = bCase;
	strcpy(m_replacestr, rstr);
	strcpy(m_findstr, fstr);
	if (allflag)
	{
		bNext = 1;
/*
.....set cursor position to beginning
.....so we don't need search down and up
*/
		ptCurrent.x = 0;
		ptCurrent.y = 0;
	}
	CPoint ptStart, ptEnd, ptNewPos;
	m_pTextBuffer->Get_Range_Pos(pRange, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return;
	DWORD dwFlags = 0;
	if (bNext==0) 
		dwFlags = FIND_DIRECTION_UP | dwFlags;
	if (bCase) 
		dwFlags = FIND_MATCH_CASE | dwFlags;

	ptCurrent.x = ptStart.x;
	ptCurrent.y = ptStart.y;
	if (allflag)
	{
//		if (Ptd_pdlg!=NULL)
//		{
//			Ptd_pdlg->ShowWindow(SW_SHOW);
//		}
//		else
//		{
//			Pted_disply_ProcessDlg("Pted Now Executing 'Replace ALL'");
			Pted_Display_Dlg_percent(1);
//		}
	}
repeat1:
	if (m_reg==0)
	{
		if (!FindTextInBlock(m_findstr, ptCurrent, ptStart, ptEnd, dwFlags, 0, 
					&ptFindPos))
		{
			goto done;
		}
		else
		{
			rStartpos.x = ptFindPos.x;
			rStartpos.y = ptFindPos.y;
			rEndpos.x = ptFindPos.x + strlen(m_findstr);
			rEndpos.y = ptFindPos.y;
			m_pTextBuffer->TextReplace(rStartpos, rEndpos, m_replacestr, ptNewPos);
			ptCurrent.x = ptNewPos.x;
			ptCurrent.y = ptNewPos.y;
			if ((ptNewPos.y-rEndpos.y)>0)
				ptEnd.y = ptEnd.y + (ptNewPos.y-rEndpos.y);
		}
		if (allflag)
		{
			perc = 100*(ptCurrent.y - ptStart.y)/(ptEnd.y - ptStart.y);
			Pted_Display_Dlg_percent(perc);
			goto repeat1;
		}
		else
			goto done;
	}
/*
.....Letter address
*/
	char *in1, *out1;
	char *in2, *out2;
	double gval, gval2;
	int nc, kreg,kreg2,kerr,len1, len2, ftype, rtype, regonly;
	len1 = strlen(m_findstr);
	len2 = strlen(m_replacestr);
	in1 = new char[200];
	in2 = new char[200];
	strcpy(in1, m_findstr);
	strcpy(in2, m_replacestr);
	out1 = new char[200];
	out2 = new char[200];
	Ptd_GetFindStr(in1, &out1, m_case, &ftype);
	Ptd_GetFindStr(in2, &out2, m_case, &rtype);

	nc = strlen(out1);
	ptd_defmtcod(&kreg, &gval, out1, &nc, &bCase, &kerr);
	if (kerr == 2)
		regonly = 1;
	else if (kerr==-1)
		ftype = 1;
	if (rtype==2)
	{
		nc = strlen(out2);
		ptd_defmtcod(&kreg2, &gval2, out2, &nc, &bCase, &kerr);
	}
repeat2:
	if (ftype==1)
	{
		if (!FindTextInBlock(m_findstr, ptCurrent, ptStart, ptEnd, dwFlags, 0, 
					&ptFindPos))
		{
			goto done;
		}
		else
		{
			rStartpos.x = ptFindPos.x;
			rStartpos.y = ptFindPos.y;
			rEndpos.x = ptFindPos.x + strlen(m_findstr);
			rEndpos.y = ptFindPos.y;
			m_pTextBuffer->TextReplace(rStartpos, rEndpos, m_replacestr, ptNewPos);
			ptCurrent.x = ptNewPos.x;
			ptCurrent.y = ptNewPos.y;
			if ((ptNewPos.y-rEndpos.y)>0)
				ptEnd.y = ptEnd.y + (ptNewPos.y-rEndpos.y);
		}
	}
	else
	{
/*
.....search for Letter address and string whichever comes first
*/
//		if (!FindStrAddrInBlock(m_findstr, out1, ptCurrent, ptStart, ptEnd, dwFlags, 
//					&flen, &fval, &ptFindPos))
		if (!FindAddrInBlock(kreg, gval, regonly, ptCurrent, ptStart, ptEnd, dwFlags, 
					&flen, &fval, &ptFindPos))
		{
			goto done;
		}
/*
.....if we find address, then we replace with address if it is address,
.....otherwise
.....replace with text string
.....12/17/99
*/
		else if ((flen>0)&&(rtype==2))
		{
/*
.....we need comvert the letter address first
*/
//			Ptd_GetAdds(in2, &out2, bCase, fval);
			ptd_fmticod(&kreg2, &fval, out2, &nc);

			rStartpos.x = ptFindPos.x;
			rStartpos.y = ptFindPos.y;
			rEndpos.x = ptFindPos.x + flen;
			rEndpos.y = ptFindPos.y;
			m_pTextBuffer->TextReplace(rStartpos, rEndpos, out2, ptNewPos);
			ptCurrent.x = ptNewPos.x;
			ptCurrent.y = ptNewPos.y;
		}
		else	
		{
			rStartpos.x = ptFindPos.x;
			rStartpos.y = ptFindPos.y;
			rEndpos.x = ptFindPos.x + strlen(m_findstr);
			rEndpos.y = ptFindPos.y;
			m_pTextBuffer->TextReplace(rStartpos, rEndpos, m_replacestr, ptNewPos);
			ptCurrent.x = ptNewPos.x;
			ptCurrent.y = ptNewPos.y;
		}
		if ((ptNewPos.y-rEndpos.y)>0)
			ptEnd.y = ptEnd.y + (ptNewPos.y-rEndpos.y);
	}
	if (allflag)
	{
		perc = 100*(ptCurrent.y - ptStart.y)/(ptEnd.y - ptStart.y);
		int test = (ptEnd.y - ptStart.y)/100;
		if ((ptCurrent.y - ptStart.y)%test==0)
			Pted_Display_Dlg_percent(perc);
		goto repeat2;
	}
done:
//	if (allflag)
//		Pted_Close_ProcessDlg();
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertMathRange(char **adds, int num, PtedRangeStruct cRange, 
c					int mflag)
c				
c   FUNCTION:  Do math convert function in specified range
c
c   INPUT:  adds: array of input
c			range:  range struction
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
void PtedBatch::ConvertMathRange(char **adds, int num, PtedRangeStruct cRange,
	 int mflag)
{
	CPoint ptStart, ptEnd;
	m_pTextBuffer->Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart==ptEnd)
		return;
/*	if (Ptd_pdlg!=NULL)
	{
		Ptd_pdlg->ShowWindow(SW_SHOW);
		return;
	}
	else
	{
		Pted_disply_ProcessDlg("Pted Convert Math");
		Pted_Display_Dlg_percent(1);
	}
*/
	m_pTextBuffer->ConvertMathRange(adds, num, ptStart, ptEnd, mflag);
//	Pted_Close_ProcessDlg();
}

/***********************************************************************
c
c   SUBROUTINE: LoadProgram(char *filename)
c
c   FUNCTION:  This function load a file into current batch class 
c
c   INPUT:  filename: file to load
c			
c   OUTPUT: none
c
c***********************************************************************
*/
int PtedBatch::LoadProgram(char *filename)
{
	char msg[UX_MAX_PATH+40];

	strcpy(m_filen, filename);

/*
.....close the text view first, then load again
*/
	OnCloseReset();

/*
.....check the extension to decide which type of file it is
*/
	char *pp;
		
	pp = strrchr(filename, '.');
	if (pp!=NULL)
	{
		if (_stricmp(pp, ".cla")==0)
		{
			m_ftype = 1;
		}
		else if (_strnicmp(pp, ".pu", 3)==0)
		{
			m_ftype = 2;
		}
		else if ((_stricmp(pp, ".cl")==0)||(_stricmp(pp, ".cln")==0))
		{
			m_ftype = 3;
		}
		else if (_stricmp(pp, ".as")==0)
		{
			m_ftype = 4;
		}
		else if (_stricmp(pp, ".sim")==0)
		{
			m_ftype = 5;
		}
/* 
.....leave the type as it is if the file extension can't tell
		else 
			m_ftype = 1;
*/	}
/* 
.....leave the type as it is if the file extension can't tell
	else
		m_ftype = 1;
*/
	int stat = m_pTextBuffer->LoadFromFile(filename, m_ftype);
	if (stat==0 && strlen(filename) != 0)
	{
		sprintf(msg, "Could not load file: '%s'\n",  filename);
		Pted_Disp_Msg(msg, 1, (int*)this, 0);
	}
	return stat;
}

/***********************************************************************
c
c   SUBROUTINE: OnCloseReset()
c
c   FUNCTION:  This function close the current text buffer and craete a new one
c   INPUT:  filename: file to load
c			
c   OUTPUT: none
c
c***********************************************************************/
void PtedBatch::OnCloseReset()
{
	if (m_pTextBuffer != NULL)
	{
		delete m_pTextBuffer;
		m_pTextBuffer = NULL;
	}
	m_pTextBuffer = new CPtedTextBuffer();
}


/***********************************************************************
c
c   SUBROUTINE:  Run(int argc, char** argv)
c
c   FUNCTION:  This function run command line 
c				
c
c   INPUT:  argc: number of command line argments
c			argv: array of command line argments
c			
c   OUTPUT: none
c
c***********************************************************************
*/
int PtedBatch::Run(int argc, char** argv)
{
	char command[200], tempstr1[200], tempstr2[200], tempstr3[200];
	int i, k, first, stat;

	if (argc==0) return 0;

/*
first = 1;
do
{
	first = 1;
} while (first == 1);
*/
	first = 1;
	for (i=0; i<argc; i++)
	{
		if (argv[i]==0)
			continue;
		if ((argv[i][0] != '/') && (argv[i][0] != '-'))
		{
			if (first==1)
			{
				first = 0;
				strcpy(m_filen, argv[i]);
/*
.....Before loading file
.....Check the command line to see
.....If there is an option for file type
.....and Units
*/
				for (k=i+1; k<argc; k++)
				{
					strcpy(command, &(argv[k][1]));
					StrtoUpper(command,tempstr1);
					if (strncmp(tempstr1,"FILE",4)==0 ||
						strncmp(tempstr1,"UNI",3)==0)
					{
						::Run_command(command, (int *)this, 0);
						break;
					}
				}
				stat = LoadProgram(m_filen);
				if (stat==0)
				{
					m_batch_cnt = 1;
					return 1;
				}
			}
			else
			{
				printf("Invalid Command %s\r\n", argv[i]);
			}
		}
		else
		{
			Pted_Display_Dlg_percent(1);
			strcpy(command, &(argv[i][1]));
			if (m_filen[0]!='\0')
				::Run_command(command, (int *)this, 0);
			else
			{
/*
.....only allow filetype and syntax color command
*/
				strncpy(tempstr1, command, 4);
				tempstr1[4] = '\0';
				strncpy(tempstr2, command, 5);
				tempstr2[5] = '\0';
				if ((_stricmp(tempstr1, "FILE")==0) ||
						(_stricmp(tempstr2, "COLOR")==0) )
					::Run_command(command, (int *)this, 0);
			}
			if (m_exit)
				break;
		}
	}

	if (m_exit)
		return 0;
	m_batch_cnt = 1;
	return 1;
}

/***********************************************************************
c
c   SUBROUTINE: LoadCutterFile(char *filename)
c
c   FUNCTION:  This functions loads a cutter file into current edit window 
c
c   INPUT:  filename: file to load
c
c   OUTPUT: none
c
c***********************************************************************
*/
void PtedBatch::LoadCutterFile(char *filename)
{
	int err = 0;
	char msg[256];
		
	CPtedTextBuffer *temp_buf = new CPtedTextBuffer();
	temp_buf->LoadCutterFile(filename, msg, &err);
	if (err)
	{
		Pted_Disp_Msg(msg, 1, (int*)this, 0);
	}
	delete temp_buf;
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  ProgramSaveAs( char *fileName)
c
c   FUNCTION:  This function save the current edit text 
c              into a file fileName
c
c   INPUT:  fileName: save file
c   OUTPUT: none
c
c***********************************************************************
*/
int 
PtedBatch::ProgramSaveAs( char *fileName)
{		
	CString tmpstr;
	char *indx, tmp[UX_MAX_PATH];
	if (fileName[0]=='\0')
	{
		if (m_filen[0]=='\0')
			return 0;
		else
		{
			strcpy(fileName, m_filen);
			indx = strchr(fileName, '.');
			if (indx!=NULL)
				*indx = '\0';
			if (m_ftype==1)
				strcat(fileName, ".cla");
			else if (m_ftype==2)
				strcat(fileName, ".pu");
			else if (m_ftype==3)
				strcat(fileName, ".cl");
			else if (m_ftype==4)
				strcat(fileName, ".as");
			else if (m_ftype==5)
				strcat(fileName, ".sim");
		}
	}
	indx = strchr(fileName, '.');
	if (indx==NULL)
	{
		if (m_ftype==1)
			strcat(fileName, ".cla");
		else if (m_ftype==2)
			strcat(fileName, ".pu");
		else if (m_ftype==3)
			strcat(fileName, ".cl");
		else if (m_ftype==4)
			strcat(fileName, ".as");
		else if (m_ftype==5)
			strcat(fileName, ".sim");
	}
	else if (fileName[0]=='.')
	{
		strcpy(tmp, m_filen);
		indx = strchr(tmp, '.');
		if (indx!=NULL)
			*indx = '\0';
		if (m_ftype==1)
			strcat(tmp, ".cla");
		else if (m_ftype==2)
			strcat(tmp, ".pu");
		else if (m_ftype==3)
			strcat(tmp, ".cl");
		else if (m_ftype==4)
			strcat(tmp, ".as");
		else if (m_ftype==5)
			strcat(tmp, ".sim");
		strcpy(fileName, tmp);
	}
	m_pTextBuffer->SaveToFile(fileName, m_ftype, (int *)this, 0);
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertNctoapt() 
c
c   FUNCTION:  This function convert current Control data file into a APT file
c				
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void PtedBatch::OnConvertNctoapt() 
{
	char title[UX_MAX_PATH+40], *indx;
	CPtedTextBuffer *tempbuffer = new CPtedTextBuffer();
	tempbuffer->InitNew();
	int size = GetBufferSize();

	tempbuffer->SetSize(size*2);
	
/*	if (Ptd_pdlg!=NULL)
	{
		Ptd_pdlg->ShowWindow(SW_SHOW);
	}
	else
	{
		Pted_disply_ProcessDlg("Pted Convert NC to APT");
		Pted_Display_Dlg_percent(1);
	}
*/
	if (m_pTextBuffer->ConvertNctoapt(tempbuffer))
	{
		delete m_pTextBuffer;
		m_pTextBuffer = tempbuffer;
		strcpy(title, m_filen);
		indx = strchr(title, '.');
		if (indx==NULL)
		{
			strcat(title, ".as");
		}
		else
		{
			*indx = '\0';
			strcat(title, ".as");
		}
		strcpy(m_filen, title);
		SetFtype(4);
	}
//	Pted_Close_ProcessDlg();
}

/***********************************************************************
c
c   SUBROUTINE:  GetBufferSize() 
c
c   FUNCTION:  This function Get the current text buffer size (lines of the text files)
c				
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
int PtedBatch::GetBufferSize() 
{
	return m_pTextBuffer->GetLineCount();
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertNctosim(fileName)
c
c   FUNCTION:  This function convert current Control data file into a
c              simulation file.
c				
c
c   INPUT:
c     fileName = Name of simulation file to create.  If blank, then the
c                input file name with an extension of '.sim' will be used.
c			
c   OUTPUT: None
c
c***********************************************************************
*/
int PtedBatch::OnConvertNctosim(char *fileName)
{
	char title[UX_MAX_PATH+40], *indx,fname[UX_MAX_PATH];
	CPtedTextBuffer *tempbuffer = new CPtedTextBuffer();
	tempbuffer->InitNew();
	int size = GetBufferSize();

	tempbuffer->SetSize(size*2);
	
/*	if (Ptd_pdlg!=NULL)
	{
		Ptd_pdlg->ShowWindow(SW_SHOW);
	}
	else
	{
		Pted_disply_ProcessDlg("Pted Convert NC to SIM");
		Pted_Display_Dlg_percent(1);
	}
*/
	if (fileName[0] == '\0') strcpy(fname,m_filen);
	else strcpy(fname,fileName);
	if (m_pTextBuffer->NctoSimfile(fname, tempbuffer))
	{
		delete m_pTextBuffer;
		m_pTextBuffer = tempbuffer;
		strcpy(title, fname);
		indx = strchr(title, '.');
		if (indx==NULL)
		{
			strcat(title, ".sim");
		}
		else
		{
			*indx = '\0';
			strcat(title, ".sim");
		}
		strcpy(m_filen, title);
//		Pted_Close_ProcessDlg();
		return 1;
	}
//	Pted_Close_ProcessDlg();
	return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  LoadDataFromString(char *str)
c
c   FUNCTION:  This function load a string into the current text buffer
c				
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
int PtedBatch::LoadDataFromString(char *str)
{
	return m_pTextBuffer->LoadDataFromString(str);
}
/***********************************************************************
c
c   SUBROUTINE:  SetWindowSyntaxClr(flag)
c
c   FUNCTION:  Set the window display with syntax color
c
c   INPUT:  color: syntax color flag
c
c   OUTPUT: none
c
c***********************************************************************
*/
void PtedBatch::SetWindowSyntaxClr(int flag)
{
	m_syntax_color = flag;
}

