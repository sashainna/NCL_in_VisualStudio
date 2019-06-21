/********************************************************************* 
**  NAME:  nclrecDlg.h
**
**			functions and datas declaration for  class CNclrecDlg
**			
**	CONTAINS: CNclrecDlg class functions
**			functions and datas declaration for  class CNclrecDlg
**
**    COPYRIGHT 2005 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			nclrecDlg.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:06:36
*********************************************************************/
#if !defined(AFX_NCLRECDLG_H__5D05A029_8B4E_11D9_A320_0800690F48C1__INCLUDED_)
#define AFX_NCLRECDLG_H__5D05A029_8B4E_11D9_A320_0800690F48C1__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CNclrecDlg dialog

class CNclrecDlg : public CDialog
{
// Construction
public:
	CNclrecDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CNclrecDlg)
	enum { IDD = IDD_NCLREC_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNclrecDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;
	char m_inputfile[MAX_PATH_LEN], m_outputfile[MAX_PATH_LEN];
	// Generated message map functions
	//{{AFX_MSG(CNclrecDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnInputbut();
	afx_msg void OnOutputbut();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCLRECDLG_H__5D05A029_8B4E_11D9_A320_0800690F48C1__INCLUDED_)
