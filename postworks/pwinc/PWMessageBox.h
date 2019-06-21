//
//     MODULE NAME AND RELEASE LEVEL
//       PWMessageBox.h , 24.1
//    DATE AND TIME OF LAST  MODIFICATION
//       09/11/13 , 12:58:27
//
#if !defined(AFX_PWMESSAGEBOX_H__E033F577_C888_11D5_908A_00C04F336F5E__INCLUDED_)
#define AFX_PWMESSAGEBOX_H__E033F577_C888_11D5_908A_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// PWMessageBox.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// PWMessageBox dialog

class PWMessageBox : public CDialog
{
// Construction
public:
	PWMessageBox(CWnd* pParent = NULL);   // standard constructor
	~PWMessageBox();

// Dialog Data
	//{{AFX_DATA(PWMessageBox)
	enum { IDD = IDD_BATCH_MESSAGE };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(PWMessageBox)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CBrush* m_pEditBkBrush;

	// Generated message map functions
	//{{AFX_MSG(PWMessageBox)
	virtual BOOL OnInitDialog();
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	virtual void OnCancel();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PWMESSAGEBOX_H__E033F577_C888_11D5_908A_00C04F336F5E__INCLUDED_)
