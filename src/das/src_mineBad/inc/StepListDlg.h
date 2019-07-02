/************************************************************************
c
c   FILE NAME: StepListDlg.h
c
c	 CONTAINS: 
c		Header file for the class CStepListDlg 
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       StepListDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:07
c
c**********************************************************************
*/
#if !defined(AFX_STEPLISTDLG_H__51B54AD5_C8FB_11D3_8112_00C04F336F5E__INCLUDED_)
#define AFX_STEPLISTDLG_H__51B54AD5_C8FB_11D3_8112_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// StepListDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CStepListDlg dialog

class CStepListDlg : public CDialog
{
protected:
	int m_single, m_snum, m_lnum, m_flag;
	CString m_title, m_msg;
	int m_select[100];
	char** m_list;
// Construction
public:
	CStepListDlg(CWnd* pParent = NULL);   // standard constructor
	~CStepListDlg();

	int SetDlgValue(int flag=0, char *title = NULL, char *msg=NULL, 
		char **list = NULL, int num = 0);
	void GetSelect(int *select, int *num);

// Dialog Data
	//{{AFX_DATA(CStepListDlg)
	enum { IDD = IDD_LIST };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CStepListDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	
	// Generated message map functions
	//{{AFX_MSG(CStepListDlg)
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STEPLISTDLG_H__51B54AD5_C8FB_11D3_8112_00C04F336F5E__INCLUDED_)
