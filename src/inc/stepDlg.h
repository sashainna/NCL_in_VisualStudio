/************************************************************************
c
c   FILE NAME: stepDlg.h
c
c	 CONTAINS: 
c		Header file for the class CStepDlg 
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       stepDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:53
c
c**********************************************************************
*/
// stepDlg.h : header file
//

#if !defined(AFX_STEPDLG_H__C82828DA_C207_11D3_810A_00C04F336F5E__INCLUDED_)
#define AFX_STEPDLG_H__C82828DA_C207_11D3_810A_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

/////////////////////////////////////////////////////////////////////////////
// CStepDlg dialog

class CStepDlg : public CDialog
{
// Construction
public:
	CStepDlg(CWnd* pParent = NULL);	// standard constructor

	void SetProcessLabel(char *label1, char *label2);
	void ShowProcessWindow(char *title);
	void CloseProcessWindow();
	void Display_as_percent(int num);
	void SetProcPos(int pos) { m_curpos = pos; }

// Dialog Data
	//{{AFX_DATA(CStepDlg)
	enum { IDD = IDD_STEP_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CStepDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	void OnHelpHelp();
	void OnAppAbout();
	//}}AFX_VIRTUAL
// Implementation
protected:
	HICON m_hIcon;
	CProgressCtrl	m_pctl;
	int m_processor, m_curpos;
	
	// Generated message map functions
	//{{AFX_MSG(CStepDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnBrowse1();
	afx_msg void OnBrowse2();
	afx_msg void OnBrowse3();
	afx_msg void OnBrowse4();
	afx_msg void OnCheckIn();
	afx_msg void OnCheckOut();
	afx_msg void OnSelchangeDrawOnly();
	afx_msg void OnFsummary();
	afx_msg void OnInOptions();
	afx_msg void OnFilter();
	afx_msg void OnOutFilter();
	afx_msg void OnSelchangeOutputUnit();
	afx_msg void OnSelchangeOutputNormals();
	virtual void OnCancel();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STEPDLG_H__C82828DA_C207_11D3_810A_00C04F336F5E__INCLUDED_)
