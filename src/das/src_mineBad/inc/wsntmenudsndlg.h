/************************************************************************
**
**   FILE NAME: wsntmenudsndlg.h
**
**       Description - Functions and struct declarations for
**              CNCLMenuDsnDlg class
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntmenudsndlg.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:20
**********************************************************************
*/
#ifndef NCLMENUDSNDLG_H_
#define NCLMENUDSNDLG_H_

#include "dmotif.h"
#include "wsnttblst.h"
#include "wsntres.h"
#pragma once

typedef struct
{
	char *name;
	int type;
	char *descrip;
} UZ_menuinfo;

/////////////////////////////////////////////////////////////////////////////
// CNCLMenuDsnDlg dialog

class CNCLMenuDsnDlg : public CDialog
{
public:
	CNCLMenuDsnDlg(CWnd* pParent = NULL);
	//{{AFX_DATA(CNCLMenuDsnDlg)
	enum { IDD = IDD_MENUDESIGN };
	CNCLTableList		m_listctl;
	//}}AFX_DATA

	int m_modal;
	UDM_menu_struc m_menu_item;
	int Save_menu_data(int *statflag);
	void Set_menu_data(UDM_menu_struc *menu_item);
	int check_menu(char *name);

	int m_sort_col;
	BOOL m_bSortAscending;
	void ChangeMenuType(int selitem);
	void HideFiltEdit();
	void ChangeKeyType(int type);
	void UpdatedSel(int item);
	void ChangedFilter(int filt_item, char *filter);

protected:
	char m_menu_name[UX_MAX_FILE_LEN];
	char m_func_name[UX_MAX_FILE_LEN];
	char m_filter[UX_MAX_FILE_LEN], m_filter1[UX_MAX_FILE_LEN], m_filter2[UX_MAX_FILE_LEN],
		m_filter3[UX_MAX_FILE_LEN];
	int m_menu_type;
	CImageList *m_rimglist;
	UZ_menuinfo m_menulist[UDM_MAX_MENU];
	int m_menulist_no;
	int m_filt_item;
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLMenuDsnDlg)
	public:
	//{{AFX_VIRTUAL(CNCLMenuDsnDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	virtual BOOL OnNotify( WPARAM wParam, LPARAM lParam, LRESULT* pResult);
	//}}AFX_VIRTUAL
protected:
	// Generated message map functions
	//{{AFX_MSG(CNCLMenuDsnDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnOK();
	afx_msg void OnCancel();
	afx_msg void OnItemChanged(NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void OnSelTable(NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void OnDBclickTable(NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void OnColumnclick(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnHeadItemEndTrack (NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnReturnKey();
	afx_msg void OnCustomDraw (NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnButType();
	//}}AFX_MSG

	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

public:
	void Init_function_list(int flag);
	void Init_menu_list(int flag);
	void Init_Seperator_list();
	void Reset_function_list(int flag);
	void Reset_menu_list(int flag);
	void Reset_Ctllist(int flag);
	void Reset_Ctllist2(int flag);
	void Reload_menulist();
};
//{{AFX_INSERT_LOCATION}}
#endif
