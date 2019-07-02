/************************************************************************
**
**   FILE NAME: toollstctl.h
**
**       Description - Functions and struct declarations for
**              CTLListCtrl class 
**    COPYRIGHT 2015 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			toollstctl.h , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:17:47
**********************************************************************
*/
#if !defined(TOOLLSTCTL_INCLUDE)
#define TOOLLSTCTL_INCLUDE

#if _MSC_VER > 1000
#pragma once
#endif 
#include "afxlistctrl.h"
#include "udforms.h"

/////////////////////////////////////////////////////////////////////////////
//

class CTLListCtrl : public CMFCListCtrl
{
	DECLARE_DYNAMIC(CTLListCtrl)
// Construction
public:
	CTLListCtrl();

// Attributes
public:
	CWnd *m_parent;
	int m_selitem;
	int m_change;
	int m_type;
	int m_update;
	int m_filter_flag;
	int m_filter_type[50]; // define here in order for if we want to add more filter type
							// sunch as checkbox or choicebox, not use now
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
	void FilterList(); 
	void SetFrmFld(int frmid, int frmfld)
	{
		m_frmfld = frmfld;
		m_frmid = frmid;
	}
	void SetParent(CWnd *parent)
	{
			m_parent = parent;
	};
	int SetSelPosOrig(int pos);
	void GetCurrentTlist(UD_TLIST *list);
	void DeleteItemPos(int pos, int flag=1, UD_ITEMDATA *data = NULL);
	void DeleteColumnItem(int column, char *toolstr, int type=1);
	void UpdateItemValue(int current_pos, int orignal_pos, UD_ITEMDATA *data);
	int FindColumnItemMatch1(int column, char *toolstr, int type=1);
	int FindColumnItemMatch2(int column, char *toolstr, int type=1);
	UD_TLIST *GetCurrentListPt()
	{
		return &m_current_list;
	}
	// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTLListCtrl)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CTLListCtrl();

	// Generated message map functions
protected:
	//{{AFX_MSG(CTLListCtrl)
	afx_msg void OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS FAR* lpncsp);
	afx_msg void OnDropdown(NMHDR* pNMHDR, LRESULT* pResult);
	LRESULT OnFilterTlist(WPARAM wParam, LPARAM lParam);
	afx_msg void OnCustomDraw(NMHDR *pNMHDR, LRESULT *pResult);
//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif // !defined(WSNTLSTCTL_INCLUDE)
