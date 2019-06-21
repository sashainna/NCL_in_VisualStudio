/************************************************************************
c
c   FILE NAME: IgesListDlg.h
c
c	 CONTAINS: 
c		Header file for the class CIgesListDlg 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       IgesListDlg.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:05               
c
c**********************************************************************
*/
#if !defined(AFX_IGESLISTDLG_H__51B54AD5_C8FB_11D3_8112_00C04F336F5E__INCLUDED_)
#define AFX_IGESLISTDLG_H__51B54AD5_C8FB_11D3_8112_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// IgesListDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CIgesListDlg dialog

class CIgesListDlg : public CDialog
{
protected:
	int m_single, m_snum, m_lnum, m_flag;
	CString m_title, m_msg;
	int m_select[100];
	char** m_list;
// Construction
public:
	CIgesListDlg(CWnd* pParent = NULL);   // standard constructor
	~CIgesListDlg();

	int SetDlgValue(int flag=0, char *title = NULL, char *msg=NULL, 
		char **list = NULL, int num = 0);
	void GetSelect(int *select, int *num);

// Dialog Data
	//{{AFX_DATA(CIgesListDlg)
	enum { IDD = IDD_LIST };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CIgesListDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	
	// Generated message map functions
	//{{AFX_MSG(CIgesListDlg)
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_IGESLISTDLG_H__51B54AD5_C8FB_11D3_8112_00C04F336F5E__INCLUDED_)
