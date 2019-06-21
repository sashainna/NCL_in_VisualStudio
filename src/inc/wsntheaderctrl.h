/************************************************************************
**
**   FILE NAME: wsntheaderctrl.h
**
**       Description - Functions and struct declarations for
**              CNCLHeaderCtrl class
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntheaderctrl.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:19
**********************************************************************
*/
#if !defined(WSNTHEADCTL_INCLUDE)
#define WSNTHEADCTL_INCLUDE

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include "wsnttblst.h"

/////////////////////////////////////////////////////////////////////////////
// CNCLHeaderCtrl window

class CNCLHeaderCtrl : public CHeaderCtrl
{
// Construction
public:
	CNCLHeaderCtrl();

// Attributes
	int m_editting, m_combo_open, m_first;

public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLHeaderCtrl)
	//}}AFX_VIRTUAL

// Implementation
	BOOL StartEdit(int nSubItem);
	BOOL EndEdit();
	CEdit* GetEditControl();	
	void OnHandleMouseclick(UINT nMsg, UINT nFlags, CPoint point, BOOL bTriggerEdit, int bandclick=-1);
	int OnHandleReturn(char *filter, int &subItem);

	void AddFilterArea(int flag, int type, CWnd *parent, char *filter1, char*filter2, char*filter3);
	void AddChoiceArea(int flag, int type, CWnd *parent, char *key_string);
	void OnHeadItemEndTrack (NMHDR* pNMHDR, LRESULT* pResult);
	void HideFiltEdit();
	BOOL IsPtInTheFilter(POINT);
	BOOL IsItemFirst(int item);
	void HideFilterArea();

public:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	virtual ~CNCLHeaderCtrl();

	// Generated message map functions
protected:
	//{{AFX_MSG(CNCLHeaderCtrl)
	afx_msg void OnPaint();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnMButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnAdjustStatic();
	afx_msg void OnItemType();
	afx_msg void OnKeyType();
	afx_msg void OnELossFocus();
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()

	CWnd *m_parent;
	CEdit* m_pWndEdit;
	CComboBox* m_pWndCombo;
	CComboBox* m_pWndKType;
	CStatic *m_pWndBand0, *m_pWndBand1, *m_pWndBand3, *m_CombBand;
	CRect m_comborc, m_bandrec, m_bandrec0, m_bandrec1, m_bandrec2, m_bandrec3;
	friend class CNCLTableList;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(WSNTHEADCTL_INCLUDE)
