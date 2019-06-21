/********************************************************************* 
**  NAME:  wsntStatusbar.h
**
**			Bottom Status bar related class definitions
**			NCLStatusBarPaneControlInfo, CNCLStatusBarPane and CNCLStatusBar class
**	CONTAINS: 
**			definitions for above 3 classes
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntStatusBar.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:14
*********************************************************************/
#ifndef CNCLSTATUSBAR_h_
#define CNCLSTATUSBAR_h_

#include <afxtempl.h>
#include "wsntmdroptarget.h"
///////////////////////////////////////////////////////////////////////////////////
// NCLStatusBarPaneControlInfo
class NCLStatusBarPaneControlInfo:public CObject
{
	DECLARE_DYNCREATE(NCLStatusBarPaneControlInfo)
public:
	CWnd	* m_hWnd;
	CRect m_rect;
	int m_subwin_no;
	CWnd	* m_subhWnd[100];
	CRect m_subrect[100];
	int		m_nPaneID;
	BOOL	m_bAutodelete;
	short m_just, m_changed;
	CString m_filename;
	NCLStatusBarPaneControlInfo();
};

///////////////////////////////////////////////////////////////////////////////////
// NCLStatusBarPaneControlInfo
class CNCLStatusBarPane: public CObject
{
public:
	UINT	nID;
	int		nText;
	UINT	nStyle;
	UINT	nFlags;
	CString strText;
	CNCLStatusBarPane ( CNCLStatusBarPane& pane );
	CNCLStatusBarPane& operator=(CNCLStatusBarPane& pane);
	CNCLStatusBarPane();
} ;

///////////////////////////////////////////////////////////////////////////////////
// CNCLStatusBar
class CNCLStatusBar:public CStatusBar
{
	DECLARE_DYNCREATE(CNCLStatusBar)
private:
	CFont m_txtfont;
	CArray<NCLStatusBarPaneControlInfo*, NCLStatusBarPaneControlInfo*>	m_ctrlinfo_array;
	CButton *m_statbut[100];	
	int m_butno;
	int m_focus;
	int m_hititem[2];
public:
	int m_changed;
	void PositionControls();
	CNCLStatusBar();
	~CNCLStatusBar();
	NCLStatusBarPaneControlInfo * GetPanControl(int nPaneID);
	BOOL AddControl(CWnd * pWnd, int paneID, CRect &rect, char *filename, short just=0, BOOL bAutodeleteControl = TRUE );
	BOOL AddSubControl(CWnd * pWnd, int paneID, CRect &rect);
	BOOL InsertControl(CWnd * pWnd, int indx, CRect &rect, char *filename, short just=0, BOOL bAutodeleteControl = TRUE );
	BOOL InsertSubControl(CWnd * pWnd, int indx, CRect &rect);
	void SetPaneWidth(int index, int width);
	BOOL AddIndicator( int position, UINT paneID);
	BOOL GetStatusPane(int nIndex, CNCLStatusBarPane& pane);
	void RemovePane(int paneID);
	void OnItemMouseMove(UINT nFlags, CPoint point);
	void OnMouseMove2(UINT nFlags, CPoint point);
	void loadstatbut(char *filename, UINT paneID);
	int UpdateLabel(UINT id, char *newlabel);
	int SaveChange();
	void RemoveAllPanes();
	void UpdateStatusBar(CPoint pt, char* input_text);
	void CheckPtOnItem(CPoint point);
	void MovedStatusItem(int barnum2, int itemnum2, int barnum1, int itemnum1, int statflag, int choice);
	NCLStatusBarPaneControlInfo *GetPanInfo(int barno);
	void RemovePaneIndx(int indx);
	void Redisppane(int indx);
	void ReLoadppane(int indx, int menu);
	void CheckClosestBut(CWnd *wnd, CPoint pt, int &barnum2, int &itemnum2);
	void CheckClosestBut2(CPoint pt, int &barnum2, int &itemnum2);
	int get_menunum2(int barno, int statflag=1);
	void getbutnum(int &bar, int &item)
	{
		bar = m_hititem[0];
		item = m_hititem[1];
	}
	//{{AFX_MSG(CNCLStatusBar)
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint pt );
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);	
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	CNCLMnDropTarget		*m_TargetDrop;
	CPoint					m_StartPoint;
	UINT					m_TimerID;
	friend class CNCLMnDropTarget;
};

#endif
