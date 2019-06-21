 /************************************************************************
c
c   FILE NAME: PtedWindow.h
c
c	 Description - Functions and struct declarations for
c		CPtedWindow class (Windows for display text)
c		This class will be the basic class for Pted Window
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c           PtedWindow.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c           09/11/13 , 12:58:35
c
c**********************************************************************
*/

#ifndef PTDWINDOW_H
#define PTDWINDOW_H
#include "pwenv.h"
#include "PtedRangeBox.h"
#include "PtedIncFileDialog.h"
#include "PtedFindReplaceDialog.h"
#include "PtedTextView.h"

#define PTED_ACTION_UNKNOWN	 0
#define PTED_TYPING		1
#define PTED_CUT		2
#define PTED_PASTE		3
#define PTED_REPLACE	4
#define PTED_DELETE		5
#define PTED_DELETE_SEL 6
#define PTED_INSERT		7
#define PTED_AUTOINDENT 8
#define PTED_CONVERT	9
#define PTED_RESEQUENCE	10
#define PTED_FORMAT		11
#define PTED_UNFORMAT	12
#define PTED_REVERSE	13
#define PTED_INCLUDE	14
#define PTED_GET		15
#define PTED_ADD		16
#define PTED_MIR		17
#define PTED_MULT		18
#define PTED_ROTATE		19
#define PTED_SCALE		20
#define PTED_TRANS		21
#define PTED_INDENT		22
#define PTED_DRAGDROP	23
#define PTED_NEW		24

#define REDO_COM	2
#define UNDO_COM	1

#define IDC_UNDO	3100
#define IDC_REPEAT	3101
#define IDC_REDO	3102

class CPtedWindow : public CDialog
{
public:
	CPtedWindow(CWnd* pParent = NULL, char *menuname = NULL, char *filename = NULL, int fopen = 0, int type=0);
	~CPtedWindow();
	CPtedTextView *m_TextView;
	CWnd* m_pParent;
	int m_modify;
	char m_file[UX_MAX_PATH];
	int m_ftype;
	int m_wtype;
	int m_convert;

	void SelParentLine(int line);
	void SelectLine(int line);
	int GetUndo_Menu() { return m_undo_menu;} 
	void SetModifiedFlag(BOOL bModified = TRUE);
	void SetViewFocus();
	void ReAdjustWindow();
	void Update_undo_redomenu(int undo_stat, int redo_stat);
	void Reset_menu();
	void SetId(int id) { m_id=id;};
	void Reset_open();
	virtual BOOL FindText(LPCTSTR lpszFind, CPoint &ptStart, CPoint &ptEnd, BOOL bNext = TRUE, BOOL bCase = TRUE);
	void GetSelected_FindText(CString& strResult);
	virtual void SetFtype(int type);
	void ConvertMathRange(char **adds, int num, PtedRangeStruct sRange, int mflag);
	void Disp_Msg(char *msg, int flag=1);
	virtual int LoadCutterFile(char *m_file);
	virtual int LoadProgram(char *m_file, int verify = 0);
	virtual int ProgramSaveAs( char *fileName);
	int ProgramSaveSelectAs(char *fname, PtedRangeStruct sRange);
	void UnformatRange(PtedRangeStruct cRange);
	void FormatRange(PtedRangeStruct cRange);
	void ConvertRange(PtedRangeStruct cRange);
	void ReseqRange(PtedRangeStruct sRange, int bseq, int seqinc,
				int seqn, int nonly);
	void ReverseRange(PtedRangeStruct sRange);
	void ProgramLoadSelect(char *FileName, PtedRangeStruct sRange);
	void ProgramIncludeSelect(char *FileName, PtedRangeStruct fRange);	
	virtual void OnConvertNctoapt();
	virtual void OnConvertTosim();
	virtual void OnConvertNctosim();
	virtual void OnConvertApttosim();
	virtual void OnConvertCltosim();
	virtual void DlgQuit();
	virtual void OnReplaceAll(LPCTSTR lpszFind, LPCTSTR lpszReplace,
		BOOL bCase, int fletter, CPoint &ptStart, CPoint &ptEnd, int vflag=0);
	virtual void OnReplaceAll(LPCTSTR lpszFind, LPCTSTR lpszReplace, BOOL bCase,
			int fletter, PtedRangeStruct* cRange, int vflag);
	virtual void WinHelp(DWORD dwData, UINT nCmd = HELP_CONTEXT);

	void ShowProcessWindow(char *title);
	void CloseProcessWindow();
	void Display_as_percent(int num);
	void SetCutCopyDel_menu(int flag);
	int IsFindString(const char *str, int knc, int *pos, int *nc);
	void SetWindowSyntaxClr(int flag);
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDialogPrompt)
	virtual void PostNcDestroy();
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

protected:

	static int m_untitled;
	HICON m_hIcon;
	int m_ChildId;
	CMenu* m_pmenu;

	HACCEL m_accel;
	CString m_strFind, m_strReplace; 
	BOOL m_bNext, m_bCase;
	PtedRangeStruct m_fRange;
	PtedRangeStruct m_frRange;
	CString m_winTitle;
	CBrush* m_pEditBkBrush;
	char m_input_mdf[256], m_output_mdf[256];
	int m_id;
	int m_openflag;
	int m_strType;
	int m_letter;
	int m_comcur, m_comcursor,m_comline, m_comlen;
	int m_actcom;
	int m_processor, m_curpos;
	CProgressCtrl	m_pctl;
	int m_undo_menu;
	int m_no_undo;
	int m_mark_flag;
	int m_syntax_color;
	char m_menuname[80];
	int m_CutterDialogIndex;
//	void SizeDialogItem(int cx, int cy );
	void SizeDialogItem();

	PtedFindReplaceDialog *m_pFindDlg;
	PtedFindReplaceDialog *m_pFindAllDlg;
	PtedFindReplaceDialog *m_pReplaceDlg;

	PtedRangeBox *m_fRangeBox;
	PtedIncFileDialog *m_IncFileBox;
	PtedRangeBox *m_cRangeBox;
	
	virtual BOOL SameAsSelected(LPCTSTR lpszCompare, BOOL bCase);

	virtual void OnFindNext(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
			int fletter, CPoint &ptStart, CPoint &ptEnd);
	virtual void OnFindAll(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
			int fletter, CPoint &ptStart, CPoint &ptEnd);
	virtual void OnReplaceSel(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
		LPCTSTR lpszReplace, int fletter, CPoint &ptStart, CPoint &ptEnd);
	virtual void OnTextNotFound(LPCTSTR lpszFind);
	virtual BOOL FindStrAdds(LPCTSTR lpszFind, char *adds, CPoint &ptStart, CPoint &ptEnd, BOOL bNext, BOOL bCase, int *fadd, double* fval);
	void LoadAPTdes(char *fileName);
	void Bad_APTdes();
	void Eredo_command(int command);
	void Eundo_command(int command);
	void Erepeat_command(int command);
	void BeginUndoGroup(BOOL bMergeWithPrevious = FALSE);
	void FlushUndoGroup();
	void OnCommandPreKeys();
	// Generated message map functions
	//{{AFX_MSG(CDialogPrompt)
	virtual BOOL OnInitDialog();
	afx_msg void OnSize( UINT nType, int cx, int cy );
	virtual BOOL PreTranslateMessage(MSG* pMsg);


	virtual LRESULT OnIncludeRange(WPARAM wParam, LPARAM lParam);
	virtual LRESULT OnFindReplaceCmd(WPARAM wParam, LPARAM lParam);

	virtual HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	virtual void OnFileOpen();
	virtual void OnSaveFile();
	virtual void OnSaveAsFile();
	virtual void OnCommandChanged();
	virtual void On_Ecut();
	virtual void On_Ecopy();
	virtual void On_Epaste();
	virtual void On_Edelete();
	virtual void On_EdeleteL();
	virtual void On_EinsertL();

	virtual void On_Ffind();
	virtual void On_Ffindnext();
	virtual void On_Ffindprev();
	virtual void On_Ffindall();
	virtual void OnEditEnableCUndo();

	virtual void On_View_Top();
	virtual void On_View_Botm();
	virtual void OnSearchReplace();
	virtual void OnFileInclude();
	virtual void OnConvertConvert();
	virtual void OnConvertBadblocks();
	virtual void OnConvertFormat();
	virtual void OnConvertUnformat();
	virtual void OnConvertConvert2();
	virtual void OnConvertBadblocks2();
	virtual void OnConvertFormat2();
	virtual void OnConvertUnformat2();
	virtual void OnConvertLength2();
	virtual void OnEditReverse();
	virtual void OnConvertLength();
	virtual void OnConvertResequence();
	virtual void OnConvertSetregister();
	virtual void OnFileCutter();
	virtual void On_Eundo();
	virtual void On_Eredo();
	virtual void OnWclatyp();
	virtual void OnWcltyp();
	virtual void OnWastyp();
	virtual void OnWputyp();
	virtual void OnConvertApttonc();
	virtual void OnWindowCommandlineon();
	virtual void OnWindowFiletypeSimulate();
	virtual void OnWindowFiletypeCutterfile();
	virtual void OnWindowLoadCutterData();
	virtual void OnViewStatus();

	afx_msg void OnToggleBookmark(UINT nCmdID);
	afx_msg void OnGoBookmark(UINT nCmdID);
	afx_msg void OnClearBookmarks();

	afx_msg void OnToggleBookmark();	
	afx_msg void OnClearAllBookmarks();
	afx_msg void OnNextBookmark();
	afx_msg void OnPrevBookmark();
	afx_msg void OnWindowBookMark();
	afx_msg void OnWindowSyntaxClr();

	afx_msg void OnFilePrint();
	afx_msg void OnFilePageSetup();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif

