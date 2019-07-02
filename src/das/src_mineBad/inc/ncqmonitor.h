/************************************************************************
**
**   FILE NAME: NcqMonitor.h
**	  
**   CONTAINS:
**		interface of the NcqMonitor class
**
**     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqmonitor.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:39
**
************************************************************************
*/
#if !defined(AFX_NCQMONITOR_H__87770137_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
#define AFX_NCQMONITOR_H__87770137_16D5_11D7_9C47_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// NcqMonitor.h : header file
//
#include "ncqcom.h"
class CNcqView;
/////////////////////////////////////////////////////////////////////////////
// NcqMonitor dialog

class NcqMonitor : public CDialog
{
// Construction
public:
	NcqMonitor(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(NcqMonitor)
	enum { IDD = IDD_MONITOR };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(NcqMonitor)
	//}}AFX_VIRTUAL

// Implementation
	int UpdateNCLinfo(char *info, int ver);
protected:
	char m_ppfile[256];
	int m_current, m_highest, m_lines;
	char m_macro[64];
	int m_warn, m_error;
	void UpdateField();
	// Generated message map functions
	//{{AFX_MSG(NcqMonitor)
	virtual void OnClose();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	friend class CNcqView;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCQMONITOR_H__87770137_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
