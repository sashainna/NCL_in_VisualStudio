 /************************************************************************
c
c   FILE NAME: PtedTextBuffer.h
c
c	 Description - Functions and struct declarations for
c		CPtedTextBuffer class (Text Buffer for edit text)
c		
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c           PtedTextBuffer.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c           09/11/13 , 12:58:35
c
c**********************************************************************
*/
#if !defined PTEDTEXTBUFFER_H
#define PTEDTEXTBUFFER_H

#if _MSC_VER >= 1000
#pragma once
#endif 

#include "PtedTextView.h"
#include "PtedWindow.h"

#ifndef __AFXTEMPL_H__
#include <afxtempl.h>
#endif

#include "PtedRangeBox.h"

#define UNDO_DESCRIP_BUF        32

enum LINEFLAGS
{
	PTED_BOOKMARK_FIRST			= 0x00000001L,
	PTED_BOOKMARKS	        	= 0x00080000L,
	PTED_APPEND_LINE				= 0x00100000L,
	PTED_FINDMARKS	        	= 0x01000000L,
	PTED_SELMARKS	        	= 0x10000000L
};

#define PTED_BOOKMARK(id)		(PTED_BOOKMARK_FIRST << id)

enum FILESTYLE
{
	FILE_STYLE_AUTOMATIC	= -1,
	FILE_STYLE_DOS			= 0,
	FILE_STYLE_UNIX			= 1,
	FILE_STYLE_MAC			= 2
};

/////////////////////////////////////////////////////////////////////////////
// CUpdateContext class

class CUpdateContext
{
public:
	virtual void RecalcPoint(CPoint &ptPoint) = 0;
};


/////////////////////////////////////////////////////////////////////////////
// CPtedTextBuffer command target

class CPtedTextBuffer : public CCmdTarget
{
	friend class  CPtedTextView;
	DECLARE_DYNCREATE(CPtedTextBuffer)

private:
	BOOL m_bInit;
	BOOL m_bReadOnly;
	BOOL m_bModified;
	int m_nFILEMode;
	BOOL m_bCreateBackupFile;
	int m_nUndoBufSize, m_nUndoGroupSize;
	int FindLineWithFlag(DWORD dwFlag);
	int m_current_size, m_max_size;
	int m_no_undo, m_group_num, m_group_count;
protected:
#pragma pack(push, 1)
/*	Nested class declarations */
	class CInsertContext : public CUpdateContext
	{
	public:
		CPoint m_ptStart, m_ptEnd;
		virtual void RecalcPoint(CPoint &ptPoint);
	};

	class CDeleteContext : public CUpdateContext
	{
	public:
		CPoint m_ptStart, m_ptEnd;
		virtual void RecalcPoint(CPoint &ptPoint);
	};

	struct SLineInfo
	{
		TCHAR	*m_pcLine;
		int		m_nLength, m_nMax, m_spaces, m_linkline;
		DWORD	m_dwFlags;

		SLineInfo() { memset(this, 0, sizeof(SLineInfo)); };
	};

	enum
	{
		UNDO_INSERT			= 0x0001,
		UNDO_BEGINGROUP		= 0x0100
	};

	struct SUndoRecord
	{
		DWORD	m_dwFlags;

		CPoint	m_ptStartPos, m_ptEndPos;
		int		m_nAction;		

	private:
		CPtedTextBuffer *m_tBuffer; 
		union
		{
			TCHAR	*m_pszText;		/*	For cases when we have > 1 character strings */
			TCHAR	m_szText[2];	/*	For single-character strings */
		};

	public:
		SUndoRecord() { memset(this, 0, sizeof(SUndoRecord)); 
		m_tBuffer = NULL;
		};

		void SetText(LPCTSTR pszText);
		void SetBuffer(CPtedTextBuffer *buffer);
		void FreeText();
		void FreeBuffer();
		CPtedTextBuffer * GetBuffer()
		{
			return m_tBuffer;
		};

		LPCTSTR GetText() const
		{
			if (HIWORD((DWORD) m_pszText) != 0)
				return m_pszText;
			return m_szText;
		};
	};

#pragma pack(pop)

	CArray <SLineInfo, SLineInfo&> m_aLines;
	CArray <SUndoRecord, SUndoRecord&> m_aUndoBuf;
	int m_nUndoPosition;
	int m_nSyncPosition;
	BOOL m_bUndoGroup, m_bUndoBeginGroup, m_bMergeWithPrevious;

	void InsertLine(LPCTSTR pszLine, int nLength = -1, int nPosition = -1, DWORD flag = 0, int linkline = -1);
	void AppendLine(int nLineIndex, LPCTSTR pszChars, int nLength, int &newline, int &newchar);
	BOOL InternalInsertText(int nLine, int nPos, LPCTSTR pszText, int len, int &nEndLine, int &nEndChar, int update=1);
	BOOL InternalInsertText(int nLine, int nPos, LPCTSTR pszText, int &nEndLine, int &nEndChar, int update=1);
	BOOL InternalDeleteText(int nStartLine, int nStartPos, int nEndLine, int nEndPos, int update=1);
	BOOL InternalInsertBuffer(int nLine, int nPos, CPtedTextBuffer *tBuffer, int &nEndLine, int &nEndChar);

	void AddUndoRecord(BOOL bInsert, const CPoint &ptStartPos, const CPoint &ptEndPos, 
						LPCTSTR pszText, CPtedTextBuffer *include_buf, int nActionType = PTED_ACTION_UNKNOWN);
	void AddUndoRecord(BOOL bInsert, const CPoint &ptStartPos, CPtedTextBuffer *include_buf, 
			CPoint &ptStart, CPoint &ptEnd, int nActionType = PTED_ACTION_UNKNOWN);
	virtual BOOL GetActionDescription(int nAction, CString &desc);

// Operations
public:
	CPtedTextView *m_attview;
	CPtedTextBuffer();
	~CPtedTextBuffer();

	BOOL InitNew(int nFILEStyle = FILE_STYLE_DOS);
	BOOL LoadFromFile(LPCTSTR pszFileName, int ftype, int nFILEStyle = FILE_STYLE_AUTOMATIC);
	BOOL SaveToFile(LPCTSTR pszFileName, int ftype, int *classpt, int class_type, int nFILEStyle = FILE_STYLE_AUTOMATIC, BOOL bClearModifiedFlag = TRUE);
	BOOL SaveSelectToFile(LPCTSTR pszFileName, int ftype, CPoint &ptStart, CPoint &ptEnd, int *classpt, int class_type, int nFILEStyle = FILE_STYLE_AUTOMATIC, BOOL bClearModifiedFlag = TRUE);
	void ReverseRange(CPoint &ptStart, CPoint &ptEnd);
	void FreeAll();
	int GetUndoActive();
	void SetSize(int size);
	void SetCurrentSize(int size)
	{
		m_current_size = size;
	};
	int GetCurrentSize()
	{
		return m_current_size;
	};
	int GetLinkLine(int line) 
	{
		return m_aLines[line].m_linkline;
	};
	void SetLinkLine(int line_num, int linkline)
	{
		m_aLines[line_num].m_linkline = linkline;
	}
	void Copy (CPtedTextBuffer *buffer);
	virtual void SetModified(BOOL bModified = TRUE);
	BOOL IsModified() const
	{
		return m_bModified;
	}

	void AttachView(CPtedTextView *pView);
	void UpdateViews(CUpdateContext *pContext, DWORD dwUpdateFlags, int nLineIndex = -1);
	int GetLineCount();
	int GetLineLength(int nLine);
	LPTSTR GetLineChars(int nLine);
	DWORD GetLineFlags(int nLine);
	int GetLineWithFlag(DWORD dwFlag);
	void SetLineFlag(int nLine, DWORD dwFlag, BOOL bSet, BOOL bRemoveFromPreviousLine = TRUE);
	void GetText(int nStartLine, int nStartChar, int nEndLine, int nEndChar, CString &text, LPCTSTR pszFILE = NULL);
	int GetTextLength(int nStartLine, int nStartChar, int nEndLine, int nEndChar, LPCTSTR pszFILE = NULL);

	int GetFILEMode();
	void SetFILEMode(int nFILEMode);
	BOOL GetReadOnly() const;
	void SetReadOnly(BOOL bReadOnly = TRUE);

	BOOL InsertText(int nLine, int nPos, LPCTSTR pszText, int &nEndLine, int &nEndChar, int nAction = PTED_ACTION_UNKNOWN);
	BOOL DeleteText(int nStartLine, int nStartPos, int nEndLine, int nEndPos, int nAction = PTED_ACTION_UNKNOWN);

	BOOL CanUndo();
	BOOL CanRedo();
	BOOL Undo(CPoint &ptCursorPos);
	BOOL Redo(CPoint &ptCursorPos);
	void BeginNoUndo();
	void EndNoUndo();
	void BeginUndoGroup(BOOL bMergeWithPrevious = FALSE);
	void FlushUndoGroup(CPtedTextView *pSource);
	int SetUndoFlag(int flag);
	int GetUndoFlag() { return m_no_undo; };

	POSITION GetUndoDescription(CString &desc, POSITION pos = NULL);
	POSITION GetRedoDescription(CString &desc, POSITION pos = NULL);

	int FindNextBookmarkLine(int nCurrentLine = 0);
	int FindPrevBookmarkLine(int nCurrentLine = 0);

	void Get_Range_Pos(PtedRangeStruct* cRange, 
		CPoint &ptStart, CPoint & ptEnd);
	void Includebuffer(CPtedTextBuffer *include_buf, CPoint &currentPt, CPoint &ptStart, CPoint & ptEnd, int nAction, int flag=1);
	BOOL FindTextInBlock(LPCTSTR pszText, CPoint &ptStartPos, CPoint &ptBlockBegin, CPoint &ptBlockEnd,
						DWORD dwFlags, BOOL bWrapSearch, CPoint *pptFoundPos);
	BOOL FindStrAddrInBlock(LPCTSTR lpszFind, char*addr,  CPoint &ptCurrent, CPoint &ptStart, CPoint &ptEnd, DWORD dwFlags, int *flen, double *fval, CPoint *ptFindPos);
	BOOL FindAddrInBlock(int kreg, double gval, int regonly, CPoint &ptCurrent, CPoint &ptStart, CPoint &ptEnd, DWORD dwFlags, int *flen, double *fval, CPoint *ptFindPos);
	void LoadCutterFile(char *filename, char* msg, int *err);
	void LoadCutterData(int *err, char *msg);
	void ConvertMathRange(char **adds, int num, CPoint &ptStart, CPoint &ptEnd, int mflag);
	void ConvertRange(CPoint &ptStart, CPoint &ptEnd);
	int BadCommand(CPtedTextBuffer *text_buffer);
	int ConvertBadBlockRange(CPoint &ptStart, CPoint &ptEnd, int ftype, CPtedTextBuffer *text_buffer);
	void ConvertFormatRange(CPoint &ptStart, CPoint &ptEnd);
	void ConvertUnFormatRange(CPoint &ptStart, CPoint &ptEnd);
	void ReseqRange(CPoint &ptStart, CPoint &ptEnd, int bseq, int seqinc,int seqn, int nonly);
	int NctoSimfile(char *file, CPtedTextBuffer *text_buffer, CPtedWindow *parent=NULL);
	int ConvertNctoapt(CPtedTextBuffer *text_buffer, CPtedWindow *parent=NULL);
	void TextReplace(CPoint &rStartpos, CPoint &rEndpos, char *rstr, CPoint &ptNewPos);
	int LoadDataFromString(char *str);

	void get_current_str(int *ln, char *ldat, int *nc);
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPtedTextBuffer)
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CPtedTextBuffer)
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
#endif 
