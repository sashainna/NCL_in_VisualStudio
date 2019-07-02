/********************************************************************* 
**  NAME:  wsntpktwin.h
**
**			Native WinNT pocket window 
**			interface of CNCLPockWin class
**	CONTAINS: CNCLPockWin class functions and structure
**				declarations
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntpktwin.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			02/08/16 , 09:21:23
*********************************************************************/

/////////////////////////////////////////////////////////////////////////////
#ifndef NCLPOCKWIN_H
#define NCLPOCKWIN_H
#include "wsntres.h"
#include "mpocket.h"
#include "wsntbitmap.h"
#include "wsnttooltip.h"

#define DHT_CLOSE		0x01
#define DHT_CAPTION		0x04
#define DHT_MIN		0x03


class CNCLPockWin : public CDialog
{
public:
	CNCLPockWin(CWnd* pParentWnd, UM_pkwin_type type);
	~CNCLPockWin();

	int  inicolormap();
	void Init();
    BOOL bSetupPixelFormat(void);
	char * GetWin();
	void SetContext();
	void PrintIPVDIB(CDC* pDC);
	void SaveToBMP(char *filename);
	void GetDIB();
	void FreeDIB();
	void SetCursor(HCURSOR cursor);
	BOOL NCLPreTranslateMessage(MSG* pMsg);

// Dialog Data
	int m_papersize;
	int m_fit, m_bcolor, m_pcenter;
	HCURSOR m_current_cursor;

	CPalette*	m_palDIB;
	HDIB		m_hDIB;
 
	CPalette*    m_cPalette;
    CPalette    *m_pOldPalette;
	CRect		m_oldRect, m_oldSRect;
	CClientDC	*m_pDC;
	HGLRC	m_hrc;
	UM_pkwin_type m_Wtype;
	int m_x, m_y, m_timer;
	CPoint m_mpoint;
	CNCLToolTip m_RectTips;
	char *m_tiptext;

	//{{AFX_DATA(CNCLPockWin)
	enum { IDD = IDD_POCKET };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLTextWin)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HACCEL m_accel;
	CRect m_rcClose, m_rcMin;
	DWORD m_LastHit;
	DWORD m_ButtonDown;
	DWORD HitTest(CPoint pt);

	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//{{AFX_MSG(CNCLTextWin)
	virtual BOOL OnInitDialog();
	virtual void OnCancel();
	afx_msg void OnPaint();
	virtual void OnSize( UINT nType, int cx, int cy );
	afx_msg int  OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnDestroy();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg LRESULT OnNcHitTest( CPoint point );
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnAccelFunctions(UINT id);
	afx_msg void OnNcPaint();
    afx_msg void OnNcLButtonDown(UINT nHitTest, CPoint point);
	afx_msg BOOL OnNcActivate(BOOL bActive);
	afx_msg void OnNcLButtonUp(UINT nHitTest, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);	
	afx_msg void OnMButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg BOOL OnMouseWheel(UINT nFlags, short zDelta, CPoint pt);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif
