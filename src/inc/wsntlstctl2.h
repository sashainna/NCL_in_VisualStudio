/************************************************************************
**
**   FILE NAME: wsntlstctl2.h
**
**       Description - Functions and struct declarations for
**              CNCLListCtrl2 class
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntlstctl2.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:19
**********************************************************************
*/
#if !defined(WSNTLSTCTL2_INCLUDE)
#define WSNTLSTCTL2_INCLUDE

#if _MSC_VER > 1000
#pragma once
#endif 

#include "udforms.h"
/////////////////////////////////////////////////////////////////////////////
//

class CNCLListCtrl2 : public CListCtrl
{
	DECLARE_DYNAMIC(CNCLListCtrl2)
// Construction
public:
	CNCLListCtrl2();

// Attributes
public:
	int m_selitem, m_selSitem;
	int m_change;
	CWnd *m_parent;
	UINT m_id;
	int m_nNumberOfRows;
	int m_nNumberOfCols;
// Operations
public:
	void SetParent_id(CWnd *parent, UINT id)
	{
		m_parent = parent;
		m_id = id;
	};
	void fill_item_data(int row, UD_ITEMDATA *data);
	void GetSelectedItem(int *row, int *col);
	void SetItemSel(int row, int col);
	void DeSelAll();
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLListCtrl2)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLListCtrl2();

	// Generated message map functions
protected:
	//{{AFX_MSG(CNCLListCtrl2)
	afx_msg void OnClick(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnKeydown(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS FAR* lpncsp);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
private:
	void invalidate_grid(int row,int col);
	afx_msg void OnCustomDraw(NMHDR *pNMHDR, LRESULT *pResult);
	int m_sel_row;
	int m_sel_col;
};

/////////////////////////////////////////////////////////////////////////////

#endif // !defined(WSNTLSTCTL2_INCLUDE)
