/************************************************************************
**
**   FILE NAME: wsntIconListBox.h
**
**       Description - Functions and struct declarations for
**              CIconListBox class (ListBox with Icon)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntIconListBox.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:14
**********************************************************************
*/
#if !defined(WSNTICONLISTBOX__INCLUDED_)
#define WSNTICONLISTBOX__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif 

/////////////////////////////////////////////////////////////////////////////
// CIconListBox window
#define MAXITEMSTRING	256
#include <afxtempl.h>
#include "wsntmDropTarget2.h"
class CExtItem
{
public:
	CExtItem(LPCSTR szItemName,HICON hIcon)
	{
		lstrcpy(m_szItemName,szItemName);
		m_hIcon = hIcon;
	}
	CExtItem(LPCSTR szItemName, HBITMAP	hBitmap)
	{
		lstrcpy(m_szItemName,szItemName);
		m_hBitmap = hBitmap;
	}
public:
	TCHAR   m_szItemName[MAXITEMSTRING];
	HICON	m_hIcon;
	HBITMAP	m_hBitmap;
};
class CIconListBox : public CListBox
{
	DECLARE_DYNAMIC(CIconListBox)
// Construction
public:
	CIconListBox();
	virtual ~CIconListBox();
	void	InitDrag();
	void OnDragDropCallback(CPoint pt, char *input_text);
	void SetParent(CWnd *parent) { m_parent = parent; };
	void SetProperty(CNCLFormProp *prop_dlg);

// Attributes
public:

// Operations
public:
	void AddItem(LPCSTR lpszItemName,HICON hIcon); 
	void AddItem(LPCSTR lpszItemName,HBITMAP hBitmap); 
	void SetSelColor(COLORREF clr);
	void SetBgColor(COLORREF clr);
	void SetCurSel(int curSel);
	void SetTextColor(COLORREF clr);
	void EnableEdge(BOOL bEnable);
	int  GetCurSel();
	void GetText(int nIdex, CString &rString) const;
	int  GetText(int nIdex, LPTSTR lpszBuffer) const;
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CIconListBox)
	public:
	virtual void MeasureItem(LPMEASUREITEMSTRUCT lpMIS);
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	virtual int VKeyToItem(UINT nKey, UINT nIndex);
	protected:
	virtual void PreSubclassWindow();
	//}}AFX_VIRTUAL

// Implementation
public:

	// Generated message map functions
protected:
	//{{AFX_MSG(CIconListBox)
	afx_msg void OnSelchange();
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);

	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnDestroy();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	//}}AFX_MSG
	CTypedPtrList<CPtrList,CExtItem *> m_pItemList;
	COLORREF			m_clrSel;
	COLORREF			m_clrBg;
	COLORREF			m_clrText;
	BOOL				m_bEdge;
	int					m_curSel;
	
	DECLARE_MESSAGE_MAP()
private:
	int m_itemno;
	CFont m_fieldFont;
	int m_flag;
	int m_type;
	CWnd *m_parent;
	CNCLMnDropTarget2		*m_TargetDrop;
	CPoint					m_StartPoint;
	UINT					m_TimerID;
	CImageList *m_DragImage;
	CImageList *m_DragAllImage;
	int m_dragimg_create;
	friend class CNCLMnDropTarget2;
};

/////////////////////////////////////////////////////////////////////////////

#endif // !defined(WSNTICONLISTBOX__INCLUDED_)
