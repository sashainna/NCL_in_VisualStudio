/********************************************************************* 
**  NAME:  wsntgettxt.h
**
**			Native WinNT edit text scrolling window dialog
**			interface of CNCLGetText class
**
**	CONTAINS: CNCLGetText class functions and structure
**			declarations
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntgettxt.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:18
*********************************************************************/
#ifndef NCLGETTEXT_H
#define NCLGETTEXT_H
#include "wsntres.h"

// HitTest Constants
#define DHT_CLOSE		0x01
#define DHT_CAPTION		0x04
#define DHT_MIN		0x03

class CNCLGetText : public CDialog
{
public:
	CNCLGetText(CWnd* pParentWnd, char *title=NULL);
	~CNCLGetText();

// Dialog Data
	//{{AFX_DATA(CNCLTextWin)
	enum { IDD = IDD_Form_HELPTEXT };
	//}}AFX_DATA

	void SetTextString(char* text)
	{
		m_text = text;
	}
	void GetTextString(CString &text)
	{
		text = m_text;
	}
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLTextWin)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CString m_title, m_text;

	//{{AFX_MSG(CNCLTextWin)
	virtual void OnSize( UINT nType, int cx, int cy );
	virtual BOOL OnInitDialog();
	void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif
