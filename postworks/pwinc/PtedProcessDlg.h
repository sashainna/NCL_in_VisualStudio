/************************************************************************
c
c   FILE NAME: PtedProcessdlg.h
c
c	 CONTAINS: 
c	 all Pted Process Dialog class
c
c     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        PtedProcessDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:34
c
c**********************************************************************
*/
/////////////////////////////////////////////////////////////////////////////

#include "Pted.h"

class CPtedProcessDlg : public CDialog
{
// Construction
public:
	CPtedProcessDlg(CWnd* pParent = NULL, char *title = NULL);	// standard constructor
	void Display_as_percent(int num);
	void CloseWindow();

// Dialog Data
	//{{AFX_DATA(CPtedProcessDlg)
	enum { IDD = IDD_PROCESSDIALOG };
	CProgressCtrl	m_pctl;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPtedProcessDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

protected:
	HICON m_hIcon;
	CString m_label, m_title;
	int m_curpos;

	// Generated message map functions
	//{{AFX_MSG(CFireDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnDestroy();
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	virtual void OnCancel();
	virtual void OnClose();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
