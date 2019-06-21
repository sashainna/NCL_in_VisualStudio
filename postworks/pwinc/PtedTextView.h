 /************************************************************************
c
c   FILE NAME: PtedTextView.h
c
c	 Description - Functions and struct declarations for
c		CPtedTextView class (View for display edit text)
c		This class will be the basic class for Pted editable view
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c           PtedTextView.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c           09/11/13 , 12:58:35
c
c**********************************************************************
*/
#if !defined PTEDTEXTVIEW_H
#define PTEDTEXTVIEW_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "PtedRangeBox.h"


////////////////////////////////////////////////////////////////////////////
// Forward class declarations

class CPtedWindow;
class CPtedTextBuffer;
class CUpdateContext;
//class CEditDropTargetImpl;


////////////////////////////////////////////////////////////////////////////
// CPtedTextView class declaration

enum
{
	FIND_MATCH_CASE		= 0x0001,
	FIND_WHOLE_WORD		= 0x0002,
	FIND_DIRECTION_UP	= 0x0010,
	REPLACE_SELECTION	= 0x0100
};

enum
{
	UPDATE_HORZRANGE	= 0x0001,
	UPDATE_VERTRANGE	= 0x0002,
	UPDATE_SINGLELINE	= 0x0100,
	UPDATE_FLAGSONLY	= 0x0200,
	UPDATE_RESET		= 0x1000
};

class CPtedTextView : public CView
{
	friend class  CPtedWindow;
	DECLARE_DYNCREATE(CPtedTextView)

private:
	
	char m_menuname[80];	
	BOOL m_bCursorHidden;
	CBitmap *m_pCacheBitmap;

	int m_nLineHeight, m_nCharWidth;
	void CalcLineCharDim();
	int m_nTabSize;
	BOOL m_bViewTabs;
	BOOL m_bSelMargin, m_syntax_color;
	int m_nScreenLines, m_nScreenChars;
	int m_nMaxLineLength;
	int m_nIdealCharPos;

	BOOL m_bFocused;
	CPoint m_ptAnchor;
	LOGFONT m_lfBaseFont;
	CFont *m_apFonts[4];

/*	
......Parsing
*/
	DWORD *m_pdwParseCookies;
	int m_nParseArraySize;
	DWORD GetParseCookie(int nLineIndex);

	int m_nActualLengthArraySize;
	int *m_pnActualLineLength;

	BOOL m_bPreparingToDrag;
	BOOL m_bDraggingText;
	BOOL m_bDragSelection, m_bWordSelection, m_bLineSelection;
	UINT m_nDragSelTimer;
	CPoint WordToRight(CPoint pt);
	CPoint WordToLeft(CPoint pt);

	CPoint m_ptDrawSelStart, m_ptDrawSelEnd;
	CPoint m_ptCursorPos;
	CPoint m_ptSelStart, m_ptSelEnd;
	void PrepareSelBounds();

	void ExpandChars(LPCTSTR pszChars, int nOffset, int nCount, CString &line);
	int ApproxActualOffset(int nLineIndex, int nOffset);
	void AdjustTextPoint(CPoint &point);
	void DrawTextLineImpl(CDC *pdc, CPoint &ptOrigin, const CRect &rcClip,
							LPCTSTR pszChars, int nOffset, int nCount);
	BOOL IsInsideSelBlock(CPoint ptTextPos);
	BOOL m_bBookmarkExist, m_bFindmarkExist;     

	BOOL	m_bOvrMode;
	BOOL	m_bDropPosVisible;
	CPoint	m_ptSavedCaretPos;
	CPoint	m_ptDropPos;
	BOOL	m_bSelectionPushed;
	CPoint	m_ptSavedSelStart, m_ptSavedSelEnd;
	BOOL	m_bAutoIndent;
	BOOL	m_bDisableBSAtSOL;								
	BOOL	DeleteCurrentSelection();

protected:
/*
.....for marker
*/
	CImageList *m_pIcons;
	CPtedTextBuffer *m_pTextBuffer;
	HACCEL m_hAccel;
//	CEditDropTargetImpl *m_pDropTarget;
//	virtual DROPEFFECT GetDropEffect();
//	virtual void OnDropSource(DROPEFFECT de);
	void Paste();
	void Cut();
	virtual void ResetView();
	BOOL GetAutoIndent() const; 
	void SetAutoIndent(BOOL bAutoIndent);

	void SetDisableBSAtSOL(BOOL bDisableBSAtSOL);
	BOOL GetDisableBSAtSOL() const;

	BOOL m_bVertScrollBarLocked, m_bHorzScrollBarLocked;
	CPoint m_ptDraggedTextBegin, m_ptDraggedTextEnd;
	void UpdateCaret();
	void SetAnchor(const CPoint &ptNewAnchor);
	int GetMarginWidth();

	BOOL m_bShowInactiveSelection;
	BOOL m_bDisableDragAndDrop;

	CPoint ClientToText(const CPoint &point);
	CPoint TextToClient(const CPoint &point);
	void InvalidateLines(int nLine1, int nLine2, BOOL bInvalidateMargin = FALSE);
	int CalculateActualOffset(int nLineIndex, int nCharIndex);

/*
.....Not Testing Printing yet
*/
	int m_nPrintPages;
	int *m_pnPages;
	CFont *m_pPrintFont;
	int m_nPrintLineHeight;
	BOOL m_bPrintHeader, m_bPrintFooter;
	char m_bPrintHeaderText[256], m_bPrintFooterText[256];
	CRect m_ptPageArea, m_rcPrintArea;
	int PrintLineHeight(CDC *pdc, int nLine);
	void RecalcPageLayouts(CDC *pdc, CPrintInfo *pInfo);
	virtual void PrintHeader(CDC *pdc, int nPageNum);
	virtual void PrintFooter(CDC *pdc, int nPageNum);
	virtual void GetPrintHeaderText(int nPageNum, CString &text, UINT &flag);
	virtual void GetPrintFooterText(int nPageNum, CString &text, UINT &flag);
/*	
.....Keyboard handlers
*/
	void MoveLeft(BOOL bSelect);
	void MoveRight(BOOL bSelect);
	void MoveWordLeft(BOOL bSelect);
	void MoveWordRight(BOOL bSelect);
	void MoveUp(BOOL bSelect);
	void MoveDown(BOOL bSelect);
	void MoveHome(BOOL bSelect);
	void MoveEnd(BOOL bSelect);
	void MovePgUp(BOOL bSelect);
	void MovePgDn(BOOL bSelect);
	void MoveCtrlHome(BOOL bSelect);
	void MoveCtrlEnd(BOOL bSelect);

	void SelectAll();
	void Copy();

	BOOL IsSelection();
	BOOL IsInsideSelection(const CPoint &ptTextPos);
	void GetSelection(CPoint &ptStart, CPoint &ptEnd);
	void SetSelection(const CPoint &ptStart, const CPoint &ptEnd);
	void HighLightSelLine(int line);

	int m_nTopLine, m_nOffsetChar;
	BOOL m_bSmoothScroll;

	int GetLineHeight();
	int GetCharWidth();
	int GetMaxLineLength();
	int GetScreenLines();
	int GetScreenChars();
	CFont *GetFont(BOOL bItalic = FALSE, BOOL bBold = FALSE);

	void RecalcVertScrollBar(BOOL bPositionOnly = FALSE);
	void RecalcHorzScrollBar(BOOL bPositionOnly = FALSE);
	void ScrollToChar(int nNewOffsetChar, BOOL bNoSmoothScroll = FALSE, BOOL bTrackScrollBar = TRUE);
	void ScrollToLine(int nNewTopLine, BOOL bNoSmoothScroll = FALSE, BOOL bTrackScrollBar = TRUE);

	virtual int GetLineCount();
	virtual int GetLineLength(int nLineIndex);
	virtual int GetLineActualLength(int nLineIndex);
	virtual LPCTSTR GetLineChars(int nLineIndex);
	virtual DWORD GetLineFlags(int nLineIndex);
	virtual void GetText(const CPoint &ptStart, const CPoint &ptEnd, CString &text);
	int GetTextLength(int nStartLine, int nStartChar, int nEndLine, int nEndChar);

	virtual BOOL PutToClipboard(LPCTSTR pszText);
	virtual BOOL GetFromClipboard(CString &text);

/*
	Drag-n-drop not working yet
*/
	virtual HGLOBAL PrepareDragData();
	BOOL IsDraggingText() const
	{
		return m_bDraggingText;
	};

	virtual COLORREF GetColor(int nColorIndex);
	virtual void GetLineColors(int nLineIndex, COLORREF &crBkgnd,
					COLORREF &crText, BOOL &bDrawWhitespace);
	virtual BOOL GetItalic(int nColorIndex);
	virtual BOOL GetBold(int nColorIndex);

	void DrawTextLine(CDC *pdc, CPoint &ptOrigin, const CRect &rcClip, int nColorIndex,
						LPCTSTR pszChars, int nOffset, int nCount, CPoint ptTextPos);
	virtual void DrawSingleLine(CDC *pdc, const CRect &rect, int nLineIndex);
	virtual void DrawMargin(CDC *pdc, const CRect &rect, int nLineIndex);

	struct TEXTBLOCK
	{
		int	m_nCharPos;
		int m_nColorIndex;
	};

	virtual DWORD ParseLine(DWORD dwCookie, int nLineIndex, TEXTBLOCK *pBuf, int &nActualItems);
	virtual HINSTANCE GetResourceHandle();

// Attributes
public:
	int GetTabSize();
	void SetTabSize(int nTabSize);
	BOOL GetSelectionMargin();
	void SetSelectionMargin(BOOL bSelMargin);
	void GetFont(LOGFONT &lf);
	void SetFont(const LOGFONT &lf);
//	BOOL GetDisableDragAndDrop() const;
//	void SetDisableDragAndDrop(BOOL bDDAD);
	void OnCloseReset();
	void SelectLine(int line);
	CPtedTextBuffer * Get_TextBuffer()
	{
		return m_pTextBuffer;
	};
	void SetModifiedFlag(BOOL bModified = TRUE);
	static HINSTANCE s_hResourceInst;

	enum
	{
		COLORINDEX_WHITESPACE,
		COLORINDEX_BKGND,
		COLORINDEX_NORMALTEXT,
		COLORINDEX_SELMARGIN,
		COLORINDEX_SELBKGND,
		COLORINDEX_SELTEXT,
		COLORINDEX_FINDTEXT,

		COLORINDEX_MSG,
		COLORINDEX_MSGTEXT,
		COLORINDEX_NUMBER,
		COLORINDEX_SYMBOLS,
		COLORINDEX_MAJOR,
		COLORINDEX_MINOR,
		COLORINDEX_EOB,
		COLORINDEX_OPSKIP,
		COLORINDEX_TAB,
		COLORINDEX_REWIND,

		COLORINDEX_A1,
		COLORINDEX_A2,
		COLORINDEX_A3,
		COLORINDEX_B1,
		COLORINDEX_B2,
		COLORINDEX_B3,
		COLORINDEX_C1,
		COLORINDEX_C2,
		COLORINDEX_C3,
		COLORINDEX_C4,
		COLORINDEX_C5,
		COLORINDEX_C6,
		COLORINDEX_D,
		COLORINDEX_E,
		COLORINDEX_F1,
		COLORINDEX_F2,
		COLORINDEX_F3,
		COLORINDEX_G0,
		COLORINDEX_G1,
		COLORINDEX_G2,
		COLORINDEX_G3,
		COLORINDEX_G4,
		COLORINDEX_G5,
		COLORINDEX_G6,
		COLORINDEX_G7,
		COLORINDEX_G8,
		COLORINDEX_G9,
		COLORINDEX_GA,
		COLORINDEX_H,
		COLORINDEX_I1,
		COLORINDEX_I2,
		COLORINDEX_J1,
		COLORINDEX_J2,
		COLORINDEX_K1,
		COLORINDEX_K2,
		COLORINDEX_L,
		COLORINDEX_M0,
		COLORINDEX_M1,
		COLORINDEX_M2,
		COLORINDEX_M3,
		COLORINDEX_M4,
		COLORINDEX_M5,
		COLORINDEX_M6,
		COLORINDEX_M7,
		COLORINDEX_M8,
		COLORINDEX_M9,
		COLORINDEX_MA,
		COLORINDEX_N,
		COLORINDEX_O,
		COLORINDEX_P,
		COLORINDEX_Q,
		COLORINDEX_R,
		COLORINDEX_S,
		COLORINDEX_T,
		COLORINDEX_U1,
		COLORINDEX_U2,
		COLORINDEX_V1,
		COLORINDEX_V2,
		COLORINDEX_W1,
		COLORINDEX_W2,
		COLORINDEX_X1,
		COLORINDEX_X2,
		COLORINDEX_Y1,
		COLORINDEX_Y2,
		COLORINDEX_Z1,
		COLORINDEX_Z2,
		COLORINDEX_AA,
		COLORINDEX_AB,
		COLORINDEX_AC,
		COLORINDEX_AD,
		COLORINDEX_AE,
		COLORINDEX_AF,
		COLORINDEX_AG,
		COLORINDEX_AH,
		COLORINDEX_AI,
		COLORINDEX_AJ,
		COLORINDEX_AK,
		COLORINDEX_AL,
		COLORINDEX_AM,
		COLORINDEX_AN,
		COLORINDEX_AO,
		COLORINDEX_AP,
		COLORINDEX_AQ,
		COLORINDEX_AR,
		COLORINDEX_AS,
		COLORINDEX_AT,
		COLORINDEX_AU,
		COLORINDEX_AV,
		COLORINDEX_AW,
		COLORINDEX_AX,
		COLORINDEX_AY,
		COLORINDEX_AZ,

		COLORINDEX_UNKNOWN
		
		//	...
		//	Expandable: custom elements are allowed.
	};

// Operations
public:
	void InitNew();
	void AttachToBuffer(CPtedTextBuffer *pBuf = NULL);
	void DetachFromBuffer();
	void SaveToFile(char *fileName, int ftype, int *classpt, int class_type);
	void SaveSelectToFile(char *filename, int ftype, CPoint &ptStart, CPoint &ptEnd, int *classpt, int class_type);
	void LoadFromFile(char *filename, int ftype);
	virtual void UpdateView(CUpdateContext *pContext, DWORD dwFlags, int nLineIndex = -1);

//	Attributes
	CPoint GetCursorPos();
	void SetCursorPos(const CPoint &ptCursorPos);
	void ShowCursor();
	void HideCursor();
	BOOL IsModified() const;
	void GetSelected_FindText(CString& strResult);
	void Get_Range_Pos(PtedRangeStruct* cRange, 
		CPoint &ptStart, CPoint & ptEnd);
	void Includebuffer(CPtedTextBuffer *include_buf, CPoint &ptStart, CPoint & ptEnd, int nAction);
//	Operations
	void EnsureVisible(CPoint pt);

	BOOL FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd,
						DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos);
	BOOL FindStrAddrInBlock(LPCTSTR lpszFind, char*addr,  CPoint &ptCurrent, CPoint &ptStart, CPoint &ptEnd, DWORD dwFlags, int *flen, double *fval, CPoint *ptFindPos);
	BOOL FindAddrInBlock(int kreg, double gval, int regonly, CPoint &ptCurrent, CPoint &ptStart, CPoint &ptEnd, DWORD dwFlags, int *flen, double *fval, CPoint *ptFindPos);
	BOOL FindTextInRange(LPCTSTR lpszFind, CPoint &ptStart, CPoint &ptEnd,
                                BOOL bNext, BOOL bCase);
	BOOL FindStrAddrInRange(LPCTSTR lpszFind, char *adds, CPoint &ptStart, CPoint &ptEnd,
                                BOOL bNext, BOOL bCase, int *flen, double *fval);
	BOOL FindAll(CString strFind, DWORD dwFlags, int letter, CPoint &ptStart, CPoint &ptEnd, CPtedTextView *textview);

	BOOL HighlightText(const CPoint &ptStartPos, int nLength);
	virtual void OnEditOperation(int nAction, LPCTSTR pszText);
	int LoadCutterFile(char *filename);
	int BadCommand(CPtedTextView *textview);
	void ConvertMathRange(char **adds, int num, CPoint &ptStart, CPoint &ptEnd, int mflag);
	void ConvertRange(CPoint &ptStart, CPoint &ptEnd);
	int ConvertBadBlockRange(CPoint &ptStart, CPoint &ptEnd, int ftype, CPtedTextView *textview);	
	void ConvertFormatRange(CPoint &ptStart, CPoint &ptEnd);
	void ConvertUnFormatRange(CPoint &ptStart, CPoint &ptEnd);
	void ReseqRange(CPoint &ptStart, CPoint &ptEnd, int bseq, int seqinc,int seqn, int nonly);
	int NctoSimfile(char *file, CPtedTextView* textview, CPtedWindow *parent=NULL);
	void Disp_Msg(char *msg, int flag=1);
	int ConvertNctoapt(CPtedTextView* textview, CPtedWindow *parent=NULL);
	void LoadFromBuffer(CPtedTextBuffer *TextBuffer);

void OnFilePrint();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPtedTextView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	virtual void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo = NULL);
	protected:
	virtual void OnInitialUpdate(); // called first time after construct
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnPrint(CDC* pDC, CPrintInfo* pInfo);
	//}}AFX_VIRTUAL

// Implementation
public:
	CPtedTextView(int flag=0);
	~CPtedTextView();
	int m_no_update;
	void SetBookMarkWindow(int flag);
	void SetSyntaxClrWindow(int flag);

	void SetParent(CWnd *parent);
	CWnd *GetParent() { return m_parent; };
	void SetType (int type) { 	m_wtype = type; };
	BOOL GetOverwriteMode() const;
	void SetOverwriteMode(BOOL bOvrMode = TRUE);

//	void ShowDropIndicator(const CPoint &point);
//	void HideDropIndicator();

//	BOOL DoDropText(COleDataObject *pDataObject, const CPoint &ptClient);
//	void DoDragScroll(const CPoint &point);

	virtual BOOL QueryEditable();

	BOOL ReplaceSelection(LPCTSTR pszNewText);
	POSITION GetUndoDescription(CString &desc, POSITION pos = NULL);
	POSITION GetRedoDescription(CString &desc, POSITION pos = NULL);
	void BeginUndoGroup(BOOL bMergeWithPrevious = FALSE);
	void FlushUndoGroup();
	void ReplaceAll(LPCTSTR lpszFind, LPCTSTR lpszReplace,
		BOOL bCase, int fletter, CPoint &ptStart, CPoint &ptEnd, int vflag=0);
	BOOL FindStrAdds(LPCTSTR lpszFind, char *adds, CPoint &ptStart, CPoint &ptEnd, BOOL bNext, BOOL bCase, int *fadd, double* fval);
	int SetUndoFlag(int flag);
	void OnFilePageSetup();

protected:
	CWnd *m_parent;
	CMenu *	m_textpopup;
	int m_wtype;
	
// Generated message map functions
protected:
	void OnEditDeleteLine();
	void OnEditInsertLine();
	void OnEditReverse();

#ifdef _DEBUG
	void AssertValidTextPos(const CPoint &pt);
#endif

	//{{AFX_MSG(CPtedTextView)
	afx_msg void OnDestroy();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnSetFocus(CWnd* pOldWnd);
	afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg BOOL OnMouseWheel(UINT nFlags, short zDelta, CPoint pt);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnKillFocus(CWnd* pNewWnd);
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);

	afx_msg void OnEditSelectAll();
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnSysColorChange();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnEditDelete();
	afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnEditDeleteBack();
	afx_msg void OnEditUntab();
	afx_msg void OnEditTab();
	afx_msg void OnEditSwitchOvrmode();
	afx_msg void OnEditUndo();
	afx_msg void OnEditRedo();

	//}}AFX_MSG
	afx_msg void OnCharLeft();
	afx_msg void OnExtCharLeft();
	afx_msg void OnCharRight();
	afx_msg void OnExtCharRight();
	afx_msg void OnWordLeft();
	afx_msg void OnExtWordLeft();
	afx_msg void OnWordRight();
	afx_msg void OnExtWordRight();
	afx_msg void OnLineUp();
	afx_msg void OnExtLineUp();
	afx_msg void OnLineDown();
	afx_msg void OnExtLineDown();
	afx_msg void OnPageUp();
	afx_msg void OnExtPageUp();
	afx_msg void OnPageDown();
	afx_msg void OnExtPageDown();
	afx_msg void OnLineEnd();
	afx_msg void OnExtLineEnd();
	afx_msg void OnHome();
	afx_msg void OnExtHome();
	afx_msg void OnTextBegin();
	afx_msg void OnExtTextBegin();
	afx_msg void OnTextEnd();
	afx_msg void OnExtTextEnd();
	afx_msg void OnToggleBookmark(UINT nCmdID);
	afx_msg void OnGoBookmark(UINT nCmdID);
	afx_msg void OnClearBookmarks();

	afx_msg void OnToggleBookmark();	
	afx_msg void OnClearAllBookmarks();
	afx_msg void OnNextBookmark();
	afx_msg void OnPrevBookmark();

	afx_msg void ScrollUp();
	afx_msg void ScrollDown();
	afx_msg void ScrollLeft();
	afx_msg void ScrollRight();
	DECLARE_MESSAGE_MAP()
};

#ifdef _DEBUG
#define ASSERT_VALIDTEXTPOS(pt)		AssertValidTextPos(pt);
#else
#define ASSERT_VALIDTEXTPOS(pt)
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif 
