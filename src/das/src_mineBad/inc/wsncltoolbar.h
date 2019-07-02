/********************************************************************* 
**  NAME:  wsncltoolbar.h
**
**			Native WinNT CNCLToolBar class
**			definitions. This class is a subclass of
**			CToolBar but have some specified character for NCL
**	CONTAINS: 
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsncltoolbar.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:12
*********************************************************************/

#if !defined WSNCLTOOLBAR_H
#define WSNCLTOOLBAR_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
#include "wsntdockcont.h"
#include "wsntmdroptarget.h"


#define DHT_CLOSE		0x01
#define DHT_NCBAR		0x02

class CNCLToolBar : public CToolBar  
{
	DECLARE_DYNAMIC(CNCLToolBar)
public:
	CNCLToolBar();
	virtual ~CNCLToolBar();
	void SetBarType(int btype) { m_bartype = btype; };
	int GetBarType() { return m_bartype;};
	void SetWraped(int wrap);
	UINT GetColumns() { return m_nColumns; };
	void SetRows(int rows) { m_nRows = rows; };
	void SetButtype(int buttype) { m_mbuttype = buttype; };
	void SetDelayedBut(int flag) { m_bDelayedButtonLayout = flag; };	
	BOOL Create( CWnd* pParentWnd, DWORD dwStyle = WS_CHILD | WS_VISIBLE | CBRS_TOP,
		UINT nID = AFX_IDW_TOOLBAR ) 
	{ 
		if (m_mbuttype==1)
			return CreateEx(pParentWnd,TBSTYLE_EX_DRAWDDARROWS|TBSTYLE_FLAT|TBSTYLE_TRANSPARENT,dwStyle,nID); 
		else
			return CreateEx(pParentWnd,0,dwStyle,nID); 
	}
	void SetStandardType(int type)
	{
		m_standard_type = 1;
	};
	BOOL CreateEx(CWnd* pParentWnd, DWORD dwExStyle = TBSTYLE_FLAT | TBSTYLE_TRANSPARENT,
		DWORD dwStyle = WS_CHILD | WS_VISIBLE | CBRS_TOP,
		UINT nID = AFX_IDW_TOOLBAR, LPCTSTR szTitle = NULL);

// CToolBar implementation changes
	BOOL SetButtonText(int nIndex, LPCTSTR lpszText);
	void RemoveButtonText(int nIndex);
	void SetButSizes(SIZE bsize);
	void SetSizes(SIZE sizeButton, SIZE sizeImage);
	void Set_Changed(int flag) { m_changed = flag; };
	int Get_Changed() { return m_changed; };
	void SetDefFloatWidth(); 
	void SetDefFloatWidth(int cols); 
	int GetCols();
	void reset_timer();

	int m_visible;
	int m_floating;
	int m_row;
	int m_changed;
	int m_StartDrag;
	int m_bartype;
	int m_barnum;
	CRect m_hitRect;
	int m_hititem;
	CNCLDockContext* m_pDockContext;   // used during dragging
	void EnableDocking(DWORD dwDockStyle);
	BOOL RecalcSize();
	virtual CSize CalcFixedLayout(BOOL bStretch, BOOL bHorz);
	void CheckClosestBut(CPoint point, int &itemnum, int &flag);

protected:
	int m_standard_type;
	UINT m_nColumns;
	UINT m_nRows;
	DWORD m_dwMode;
	int m_mbuttype;
	CRect m_rcClose;

	struct Accel { UINT nKey; UINT nId; };
	CMenu m_pop;
	BOOL m_bHasBitmaps,m_bForceText;
	CArray<int,int> m_TextIds;
	CArray<Accel,Accel&> m_accelList;
	CWnd* m_pParent;
	static BOOL m_bButtons;
	static BOOL m_bText;
	static CArray<CNCLToolBar*,CNCLToolBar*> m_ToolbarList;

	virtual void SettingChange();
	void _SetButton(int nIndex, TBBUTTON* pButton);
	void DrawGripper(CWindowDC *pDC, CRect& rectWindow);
	void EraseNonClient(BOOL bRaised);
	void DrawBorders(CDC* pDC, CRect& rect);
	void RepaintBackground();
	virtual void OnUpdateCmdUI(CFrameWnd* pTarget, BOOL bDisableIfNoHndler);
	void _GetButton(int nIndex, TBBUTTON* pButton) const;
	int WrapToolBar(TBBUTTON* pData, int nCount, int nWidth);
	void SizeToolBar(TBBUTTON* pData, int nCount, int nLength, BOOL bVert = FALSE);
	CSize CalcLayout(DWORD nMode, int nLength = -1);
	CSize CalcSize(TBBUTTON* pData, int nCount);
	virtual CSize CalcDynamicLayout(int nLength, DWORD dwMode);

protected:
	DWORD HitTest(CPoint pt);
	virtual void HideSeparators(CDC* pDC);
	virtual CBrush* GetBackgroundBrush();
	//{{AFX_MSG(CNCLToolBar)
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnDestroy();
	afx_msg LRESULT OnNcHitTest(CPoint point);
	afx_msg void OnPaint();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	afx_msg void OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS* lpncsp);
	afx_msg void OnRecalcSize();
	afx_msg void OnNcPaint();
	afx_msg void OnWindowPosChanging(LPWINDOWPOS lpWndPos);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint pt );
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint pt);

	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);	
	afx_msg BOOL OnNcActivate(BOOL bActive);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	
	CNCLMnDropTarget		*m_TargetDrop;
	CPoint					m_StartPoint;
	UINT					m_TimerID;
	friend class CNCLMnDropTarget;
};

#undef AFX_DATA
#define AFX_DATA

#endif 
