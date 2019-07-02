/************************************************************************
**
**   FILE NAME: wsntlstctl.h
**
**       Description - Functions and struct declarations for
**              CNCLListCtrl class
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntlstctl.h , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:25:49
**********************************************************************
*/
#if !defined(WSNTLSTCTL_INCLUDE)
#define WSNTLSTCTL_INCLUDE

#if _MSC_VER > 1000
#pragma once
#endif 
#include "afxlistctrl.h"
#include "udforms.h"

/////////////////////////////////////////////////////////////////////////////
//

class CNCLListCtrl : public CMFCListCtrl
{
	DECLARE_DYNAMIC(CNCLListCtrl)
// Construction
public:
	CNCLListCtrl();

// Attributes
public:
	CWnd *m_parent;
	int m_selitem;
	int m_change;
	int m_type;
	int m_filter_flag;
	int m_filter_type[50];
	int m_update;
							
	CString m_filter[50]; // store the filter string for all column
							
	UD_TLIST m_orig_list; //always keep the original list to reset list using filter
							//the display list will updated use original list from 
							//ud_form call answer pointer
	UD_TLIST m_current_list; //current list displayed
	int m_frmfld, m_frmid;
// Operations
public:
	int GetInputText(CString &input_text, CString prompt_text, int size[2]);
	void SetTlist(UD_TLIST *list);
	void FilterList(); //filter and reset the list
	void SetFrmFld(int frmid, int frmfld)
	{
		m_frmfld = frmfld;
		m_frmid = frmid;
	}
	void SetParent(CWnd *parent)
	{
			m_parent = parent;
	};
	void GetCurrentTlist(UD_TLIST *list);
	LRESULT OnFilterTlist(WPARAM wParam, LPARAM lParam);
	// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLListCtrl)
	//}}AFX_VIRTUAL

// Implementation
public:
virtual ~CNCLListCtrl();
	// Generated message map functions
protected:
	//{{AFX_MSG(CNCLListCtrl)
	afx_msg void OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS FAR* lpncsp);
	afx_msg void OnDropdown(NMHDR* pNMHDR, LRESULT* pResult);
	virtual COLORREF OnGetCellTextColor(int /*nRow*/, int /*nColum*/) ;
	virtual COLORREF OnGetCellBkColor(int /*nRow*/, int /*nColum*/) ;
	afx_msg void OnCustomDraw(NMHDR *pNMHDR, LRESULT *pResult);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif // !defined(WSNTLSTCTL_INCLUDE)
