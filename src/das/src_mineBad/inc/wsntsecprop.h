/********************************************************************* 
**  NAME:  wsntsecprop.h
**
**			Native WinNT section property dialog
**			interface of CNCLSecProp class
**
**	CONTAINS: CNCLSecProp class functions and structure
**			declarations
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntsecprop.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 18:03:07
*********************************************************************/
#ifndef NCLSECPROP_H
#define NCLSECPROP_H
#include "wsntres.h"
#include "wsntclrbtn.h"

class CNCLSecProp : public CDialog
{
public:
	CNCLSecProp(CWnd* pParentWnd, char *title=NULL);
	~CNCLSecProp();

// Dialog Data
	//{{AFX_DATA(CNCLTextWin)
	enum { IDD = IDD_FORMSEC_PRO };
	//}}AFX_DATA

	void SetTextString(char* text)
	{
		m_text = text;
	}
	void GetTextString(CString &text)
	{
		text = m_text;
	}
	void SetTextColor(int color)
	{
		m_color = color;
	}
	void GetTextColor(int *color)
	{
		*color = m_color;
	}
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLTextWin)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CString m_title, m_text;
	int m_color;
	CNCLColorButton m_button;
	//{{AFX_MSG(CNCLTextWin)
	virtual BOOL OnInitDialog();
	afx_msg void OnColorBut();
	void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif
