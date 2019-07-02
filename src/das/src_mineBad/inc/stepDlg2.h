/************************************************************************
c
c   FILE NAME: stepDlg2.h
c
c	 CONTAINS: 
c		Header file for the class CStepDlg2 
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c         stepDlg2.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c         04/29/15 , 15:06:53
c**********************************************************************
*/
// stepDlg2.h : header file
//

#if !defined(AFX_STEPDLG_H__C82828DA_C207_11D3_810A_00C04F336F5E__2INCLUDED_)
#define AFX_STEPDLG_H__C82828DA_C207_11D3_810A_00C04F336F5E__2INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

/////////////////////////////////////////////////////////////////////////////
// CStepDlg dialog

class CStepDlg2 : public CDialog
{
// Construction
public:
	CStepDlg2(CWnd* pParent = NULL);	// standard constructor

	void SetProcessLabel(char *label1, char *label2);
	void ShowProcessWindow(char *title);
	void CloseProcessWindow();
	void Display_as_percent(int num);
	void SetProcPos(int pos) { m_curpos = pos; }

// Dialog Data
	//{{AFX_DATA(CStepDlg)
	enum { IDD = IDD_STEP_DIALOG2 };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CStepDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
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
	virtual void OnCancel();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STEPDLG_H__C82828DA_C207_11D3_810A_00C04F336F5E__2INCLUDED_)
