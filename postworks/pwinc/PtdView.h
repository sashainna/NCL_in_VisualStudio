/*********************************************************************
**  NAME:  PtdView.h
**  Description:
**				all member function and variable for class PtdView
**			
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdView.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:33
*********************************************************************/
#ifndef PTDVIEWH
#define PTDVIEWH

#include "PtdBase.h"
#include "PtdRangeDialogManager.h"

class PtdFrameWindow;
class PtdView;

class PtdView : public PtdBase 
{
	public:
		PtdView *m_wParent;
		int m_type;
		char *m_pFileData;
/*
.....indecate typing 
*/
		int m_typing; 
		int m_convert;

	
	enum PtdEViewType 
	{
		PtdEGraphicsView, 
		PtdETextView, 
		PtdEAssemblyView
	};
	
	protected:

	PtdEViewType m_eViewType;
	Dimension   m_Width;
	Dimension   m_Height;
	Widget m_wMessageDlg;
	Widget m_wTextWidget;
	Widget m_wEcommand;
	Widget m_wEmessage;
	
	char m_findstr[256];
	char m_replacestr[256];
	int m_case, m_reg, m_downdir;
	int m_modified;

	int m_ftype;
	int m_actcom;
	int m_comcur;
	int m_runcom;
	char *m_command_line;
	RangeDlgStruct m_frRange;
/*
.....added variables for Undo/Redo
*/
/*
.....we don't use MFC clipboard now because we will
.....undo up to 5 operations
*/
	char *m_pastestr;
/*
.....five undo command and five redo command
*/
	int m_command[5];
	char m_label1[21][30];
	char m_label2[21][30];
	char m_label3[21][30];
/*
.....change text's beginning position
*/ 
	int m_chgpos[5];
/*
.....store change text
*/ 
	char *m_chgstr[5];
/*
.....store new text length
*/
	int m_chglen[5];
/*
.....flag for undo or redo
.....REDO_COM/UNDO_COM
*/
	int m_comflag[5];
/*
.....store typing characters
*/
	char m_typstr[1000];
	char m_oldstr[1000];
	int m_typlen, m_oldlen;
	int m_typbpos, m_typepos;
/*
.....store replace position
*/
	int *m_rpos[5];
	int m_rnum[5];
/*
.....store replaced string
*/
	char **m_rstr[5];
/*
.....added to store new string (replace string)
*/
	char **m_newstr[5];

	private:

		void InitData();
		static void FileChangedCallback(Widget,XtPointer,XtPointer);
		static void CommChangedCallback(Widget,XtPointer,XtPointer);
		static void CommVerifyCallback(Widget,XtPointer,XtPointer);
		static void CmotVerifyCallback(Widget,XtPointer,XtPointer);
	public:

		void ReseqRange(RangeDlgStruct* rangeStruct,
				int bseq, int seqinc, int seqn, int nonly);
		void ReverseRange(RangeDlgStruct* rangeStruct);
		void FormatRange(RangeDlgStruct* rangeStruct);
		void UnformatRange(RangeDlgStruct* rangeStruct);
		void ConvertRange(RangeDlgStruct* rangeStruct);
		int ProgramSaveSelectAs(char* FileName, RangeDlgStruct sRange);
		void ProgramIncludeSelect(char* FileName, RangeDlgStruct sRange);
		void ProgramLoadSelect(char* FileName, RangeDlgStruct sRange);
		void OnReplaceText(char *fstr, char *rstr, int bCase, int bNext,
      		int fletter, RangeDlgStruct *pRang, int allflag, int vflag=0);
		void Substitute(char *fstr, char *rstr, int vflag,
      		RangeDlgStruct range);
		void ConvertMathRange(char **adds, int num, RangeDlgStruct cRange, 
				int mflag);
		void Disp_find_line(int *rpos, char** frstr, int rnum);

	public:

		PtdFrameWindow *m_pFrame;
		
		PtdView(char *, int type=0);    
		~PtdView();
		virtual void SetFrame(PtdFrameWindow *pFrame)
			{	m_pFrame = pFrame;}
		PtdFrameWindow *Frame() { return m_pFrame;}
		void SetType(PtdEViewType type)
			{ m_eViewType = type;}
		PtdEViewType Type() { return m_eViewType;}
	    
		virtual void Create(Widget);
		virtual void InitialUpdate();
		virtual void  Update(PtdView *view = NULL);
		virtual Widget WorkArea() { return m_wBaseWidget; }
		void Set_ftype(int ftype);
		int Get_ftype()  { return m_ftype; } 
		void Set_WaitCurs(int flag);
		void Get_Range_Pos(char *fdata, RangeDlgStruct *range, int*spos,
				int*epos, int *blin, int *elin);
		void SetText(char*);
		void GetText(char**, int*);
		void TextCut_click(XtPointer);
		void TextCopy_click(XtPointer);
		void TextPaste_click();
		void TextDelete_click();
		void SetModifiedFlag(int modify = 1)
			{  m_modified = modify ; }
		int   IsModified() { return m_modified;}
		void TextInsert_line();
		void TextDelete_line();
		void TextView_Top();
		void TextView_Bottom();
		void TextFind(XtPointer);
		void TextFind_Next(XtPointer,int);
		void TextFind_All(char *title);
		void TextReplace();
		void SetParent(PtdView *view) { m_wParent = view; }
		int FindText(char *findstr, RangeDlgStruct *range , int bNext,
					int bCase, int *fpos);
		int FindStrAdds(char *findstr, char *adds, RangeDlgStruct *range, 
				int bNext, int bCase, int *fpos, int *fadd, int *flen,
				double *fval);
		void InsertText(char *data);
		void Convert_Convert();
		void Convert_Convert2();
		void Convert_Format();
		void Convert_Format2();
		void Convert_Unform();
		void Convert_Unform2();
		void Convert_Bad();
		void Convert_Bad2();
		int SameAsSelected(char*, int);
		void Convert_Length();
		void Convert_Length2();
		void Edit_Reverse();
		void Convert_Reseq();
		void Reset_typing();
		void On_Erepeat(Time time);
		void On_Eredo(Time time);
		void On_Eundo(Time time);
		int Get_current_pos();
		int Get_current_select(int *start, int *end);
		void Top_command(int com, int chgpos, char *chgstr, 
			int chglen=0, int*rpos = NULL, char **rstr=NULL, int rnum = 0,
				char **newstr=NULL);
		void Trail_command(int com, int chgpos, char *chgstr, 
			int chglen=0, int*rpos = NULL, char **rstr=NULL, int rnum = 0,
			char **newstr=NULL);
		void PopTopCommand(int *command, int *chgpos, char **chgstr, 
				int* chglen = NULL, int**rpos = NULL, char***rstr=NULL, 
				int *rnum = NULL, char ***newstr=NULL);
		void PopTrailCom(int *command, int *chgpos, char **chgstr, 
				int* chglen = NULL, int**rpos = NULL, char***rstr=NULL, 
				int *rnum = NULL, char ***newstr=NULL);
		void Add_typstr_into_command(char *typstr, char *oldstr, int bpos);
		void Reset_Undo_Redo();
		void GetUndoCommand(int *command);
		void GetRedoCommand(int *command);
		void GetCommand(int pos, int *command);
		void GetTrailIndex(int *index);
		void Eredo_command(int command, Time time);
		void Eundo_command(int command, Time time);
		void Erepeat_command(int command, Time time);
		void Clear_Redo_command();
		void APTtoNC(char *fname);
		void NCtoAPT(char *fname);
		void OnConvertAptToSim(char *fname);
		void OnConvertClToSim(char *fname);
		void OnCommandOn(int actcom);
		void Disp_Msg(char *msg, int flag=1);
		int APTdes_Bad(int fsave);
		void LoadAPTdes(char *fileName);
		void NewChildWindow(char *title, char *data);
		int OnConvertNctoapt(char *fname);
};
#endif
