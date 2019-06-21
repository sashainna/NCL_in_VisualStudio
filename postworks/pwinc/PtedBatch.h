/*********************************************************************
**  NAME:  PtedBatch.h
**  Description:
**				all member function and variable for class PtedBatch
**				which encapsulation of all application for running batch
**				
**    CONTAINS:
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtedBatch.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:33
*********************************************************************/
#ifndef PTEDBATCHH
#define PTEDBATCHH
#include "PtedRangeBox.h"
#include "PtedTextBuffer.h"

class PtedBatch  
{
	friend class CPtedWindow;
	private:
	//	Text search helpers
	BOOL FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd,
						DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos);
	BOOL FindStrAddrInBlock(LPCTSTR lpszFind, char*addr,  CPoint &ptCurrent, CPoint &ptStart, CPoint &ptEnd, DWORD dwFlags, int *flen, double *fval, CPoint *ptFindPos);
	BOOL FindAddrInBlock(int kreg, double gval, int regonly, CPoint &ptCurrent, CPoint &ptStart, CPoint &ptEnd, DWORD dwFlags, int *flen, double *fval, CPoint *ptFindPos);
	void TextReplace(CPoint &rStartpos, CPoint &rEndpos, char *replacestr, CPoint ptEnd);	
	void BatchToSim();
	void OnCloseReset();
	protected:
		char m_findstr[256];
		char m_replacestr[256];
		int m_case, m_reg, m_downdir;
		int m_modified;
		int m_ftype;
		int m_cursor_pos;
		int m_syntax_color;
		CWnd* m_pParent;
		
	public:
		int m_batch_cnt;
		CPtedTextBuffer *m_pTextBuffer;
		int m_exit;

		PtedBatch (char *);
		~PtedBatch();
		int Run(int argc, char** argv);
		void LoadCutterFile(char *filename);
		void SetFtype(int type) { m_ftype = type; }
		void ConvertMathRange(char **adds, int num, PtedRangeStruct sRange, int mflag);
		int LoadProgram(char *m_file);
		int ProgramSaveAs( char *fileName);
		int ProgramSaveSelectAs(char *fname, PtedRangeStruct sRange);
		void UnformatRange(PtedRangeStruct *cRange);
		void FormatRange(PtedRangeStruct *cRange);
		void ConvertRange(PtedRangeStruct *cRange);
		void ReseqRange(PtedRangeStruct *sRange, int bseq, int seqinc,
				int seqn, int nonly);
		void ReverseRange(PtedRangeStruct *sRange);
		void ProgramLoadSelect(char *FileName, PtedRangeStruct sRange);
		void ProgramIncludeSelect(char *FileName, PtedRangeStruct fRange);
		void OnConvertNctoapt();
		void OnReplaceText(char *fstr, char *rstr, int bCase, int bNext,
					int fletter, PtedRangeStruct *pRang, int allflag, int vflag=0);	
		char m_filen[256];
		int GetBufferSize();
		int OnConvertNctosim(char *file);
		int LoadDataFromString(char *str);
		void SetWindowSyntaxClr(int flag);
};

#endif

