/************************************************************************
**
**   FILE NAME: wsntdlgbar.h
** 
**	 Description - Functions and struct declarations for
**		CNCLDialogBar class 
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdlgbar.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:16
**
************************************************************************
*/
#if !defined(AFX_NCLDIALOGBAR_H__2A544764_4B70_11D4_81AD_00C04F336F5E__INCLUDED_)
#define AFX_NCLDIALOGBAR_H__2A544764_4B70_11D4_81AD_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include <afxpriv.h>   
#include <afxtempl.h>  

#include "wsntdlgitem.h"
#include "udfdata.h"
#include "udforms.h"
#include "wsnttxtwin.h"

class CNCLDialogBar;
typedef CTypedPtrArray <CPtrArray, CNCLDialogBar*> CSizeBarArray;

class CSizeDockBar : public CDockBar
{
    friend class CNCLDialogBar;
};

/////////////////////////////////////////////////////////////////////////

#define SCBS_EDGELEFT       0x00000001
#define SCBS_EDGERIGHT      0x00000002
#define SCBS_EDGETOP        0x00000004
#define SCBS_EDGEBOTTOM     0x00000008
#define SCBS_EDGEALL        0x0000000F
#define SCBS_SHOWEDGES      0x00000010
#define SCBS_GRIPPER        0x00000020

#define DHT_CLOSE		0x01
#define DHT_NCBAR		0x02
#define DHT_HELP		0x03

/////////////////////////////////////////////////////////////////////////////
// CNCLDialogBar dialog

class CNCLDialogBar : public CControlBar
//class CNCLDialogBar : public CDialogBar
{
	DECLARE_DYNAMIC(CNCLDialogBar)
// Construction
public:
	CNCLDialogBar();   // standard constructor
	BOOL Create(CWnd* pParentWnd, LPCTSTR lpszTemplateName,
			UINT nStyle, UINT nID);
	BOOL Create(CWnd* pParentWnd, UINT nIDTemplate,
			UINT nStyle, UINT nID);
	BOOL CreateIndirect(CWnd* pParentWnd, DLGTEMPLATE *lpDialogTemplate, 
			CDialogItem *rgDlgItem, int itemnum);
	BOOL CreateFromFile(CWnd* pParentWnd, char *dlgfile, 
			UINT nStyle, UINT nID, int size[2]);
	BOOL CNCLDialogBar::CreateCommand(CWnd* pParentWnd, char *label, char *string, int bar_size[2], 
							   UINT nStyle, UINT pID, UINT eID, UINT nID);
	BOOL CNCLDialogBar::CreateLabel(CWnd* pParentWnd, char *label, int bar_size[2], 
							   UINT nStyle, UINT lID, UINT nID, int flag = 0);
	BOOL CreateStatusWin(CWnd* pParentWnd, char *string, int bar_size[2], 
							   UINT nStyle, UINT eID, UINT nID, int flag=0);
	int CreateFormWin(CWnd* pParentWnd, CDialogItem *rgDlgItem, DLGTEMPLATE dlgTempl, int itemnum, UINT nStyle, UINT nID);
	int CreateFormWin2(CWnd* pParentWnd, CDialogItem *rgDlgItem, DLGTEMPLATE dlgTempl, int itemnum, UINT nStyle, UINT nID);
	int CreateFormWinSub(CWnd* pParentWnd, CDialogItem *rgDlgItem, DLGTEMPLATE dlgTempl, int itemnum, UINT nStyle, UINT nID);
	int pos[5];
	int m_created;
	int m_visible;
	int m_enabled;
	CString m_TitleCaption;
	int m_bartype, m_helpact, m_startdrag;

    CSize m_szHorz;
    CSize m_szVert;
    CSize m_szFloat;
    const BOOL IsFloating() const;
    const BOOL IsHorzDocked() const;
    const BOOL IsVertDocked() const;
    const BOOL IsSideTracking() const;
	void GetWindowSize(int &cols, int &rows);	

// Implementation
public:
	virtual ~CNCLDialogBar();
	virtual CSize CalcFixedLayout(BOOL bStretch, BOOL bHorz);
    virtual CSize CalcDynamicLayout(int nLength, DWORD dwMode);
	CSize m_sizeDefault;
	virtual void OnUpdateCmdUI(CFrameWnd* pTarget, BOOL bDisableIfNoHndler);
	virtual void DoPaint(CDC* pDC);
	void DrawBorders(CDC* pDC, CRect& rect);
	void DrawGripper(CDC* pDC, const CRect& rect);
    virtual BOOL DestroyWindow();
	void UpdateScrollBar();

protected:
    
	DWORD HitTest(CPoint pt);
    UINT GetEdgeHTCode(int nEdge);
    BOOL GetEdgeRect(CRect rcWnd, UINT nHitTest, CRect& rcEdge);
    virtual void StartTracking(UINT nHitTest);
    virtual void StopTracking();
    virtual void OnTrackUpdateSize(CPoint& point);
    virtual void OnTrackInvertTracker();
    virtual void AlignControlBars();
    const int FindSizingBar(CControlBar* pBar) const;
    void GetRowInfo(int& nFirst, int& nLast, int& nThis);
    void GetRowSizingBars(CSizeBarArray& arrSCBars);
    virtual void NcPaintGripper(CDC* pDC, CRect rcClient);
	void AdjustSize();

protected:

    static CSizeBarArray    m_arrBars;
    DWORD   m_dwSCBStyle;
    UINT    m_htEdge;

    CSize   m_szMin;
    CSize   m_szMinT;
    CSize   m_szMaxT;
    CSize   m_szOld;
    CPoint  m_ptOld;
    BOOL    m_bTracking;
    BOOL    m_bKeepSize;
    BOOL    m_bParentSizing;
    BOOL    m_bDragShowContent;
    UINT    m_nDockBarID;
    int     m_cxEdge;
    int     m_cyGripper;

	CRect m_child_rect[200];
	CRect m_origrect;
	int m_dtype;
	CRect m_rcClose, m_rcHelp;
	int m_modal;
	int m_scroll;
	CFont m_txtfont, m_status_txtfont, m_prmptfont, m_errorfont;

protected:
//	void DrawGripper(CWindowDC *pDC, CRect& rectWindow);
	void EraseNonClient(BOOL bRaised);
//	void DrawBorders(CDC* pDC, CRect& rect);
	// data and functions necessary for OLE control containment
	_AFX_OCC_DIALOG_INFO* m_pOccDialogInfo;
	LPCTSTR m_lpszTemplateName;
	virtual BOOL SetOccDialogInfo(_AFX_OCC_DIALOG_INFO* pOccDialogInfo);
	afx_msg LRESULT HandleInitDialog(WPARAM, LPARAM);
    afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS* lpncsp);
	afx_msg void OnNcLButtonDown( UINT nHitTest, CPoint point );
    afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
    afx_msg void OnMouseMove(UINT nFlags, CPoint point);
    afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
    afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
    afx_msg void OnNcLButtonUp(UINT nHitTest, CPoint point);
    afx_msg LRESULT OnNcHitTest(CPoint point);
    afx_msg void OnCaptureChanged(CWnd *pWnd);
    afx_msg void OnWindowPosChanging(WINDOWPOS FAR* lpwndpos);
    afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
    afx_msg void OnSettingChange(UINT uFlags, LPCTSTR lpszSection);
    afx_msg void OnSize(UINT nType, int cx, int cy);
	DECLARE_MESSAGE_MAP()
};

#endif // !defined(AFX_NCLDIALOGBAR_H__2A544764_4B70_11D4_81AD_00C04F336F5E__INCLUDED_)
