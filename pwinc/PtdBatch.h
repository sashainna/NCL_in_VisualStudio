/*********************************************************************
**  NAME:  PtdBatch.h
**  Description:
**				all member function and variable for class PtdBatch
**				which encapsulation of all application for running batch
**				
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdBatch.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:28
*********************************************************************/
#ifndef PTDBATCHH
#define PTDBATCHH
#include "pwenv.h"
#include "PtdRangeDialogManager.h"

class PtdBatch  
{
	friend class PtdFrameWindow;
	friend class PtdMainView;
	public:
		void Get_Range_Pos(char *fdata, RangeDlgStruct *range, int*spos,
				int*epos, int *blin, int *elin);
		void ReseqRange(RangeDlgStruct* rangeStruct,
				int bseq, int seqinc, int seqn, int nonly);
		void ReverseRange(RangeDlgStruct* rangeStruct);
		void FormatRange(RangeDlgStruct* rangeStruct);
		void UnformatRange(RangeDlgStruct* rangeStruct);
		void ConvertRange(RangeDlgStruct* rangeStruct);
		int ProgramSaveSelectAs(char* FileName, RangeDlgStruct sRange);
		void ProgramIncludeSelect(char* FileName, RangeDlgStruct sRange);
		int Programload(char *filename);
		int ProgramSaveAs( char *fileName);
		void ProgramLoadSelect(char* FileName, RangeDlgStruct sRange);
		void Substitute(char *fstr, char *rstr, int vflag,
					RangeDlgStruct range);
		void OnReplaceText(char *fstr, char *rstr, int bCase, int bNext,
					int fletter, RangeDlgStruct *pRang, int allflag, int vflag=0);
		void ConvertMathRange(char **adds, int num, RangeDlgStruct cRange,
					int mflag);
		int FindText(char *findstr, RangeDlgStruct *range, int bNext,
				int bCase, int *fpos);
		int FindStrAdds(char *findstr, char *adds, RangeDlgStruct *range,
				int bNext, int bCase, int *fpos, int *fadd, int *flen,
				double *fval);
		void LoadCutterFile(char *filename);

	protected:
		char m_findstr[256];
		char m_replacestr[256];
		int m_case, m_reg, m_downdir;
		int m_modified;
		int m_cursor_pos;
		
	public:
		char *m_pFileData;
		char m_filen[UX_MAX_PATH];
		int m_ftype;
		int m_exit;
		int m_batch_cnt;

		PtdBatch (char *);
		~PtdBatch();
		int Run(int argc, char** argv);
		int OnConvertNctoapt();
		void Set_ftype(int type) { m_ftype = type; }
		
};

#endif

