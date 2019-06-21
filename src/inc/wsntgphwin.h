/********************************************************************* 
**  NAME:  wsntgphwin.h
**
**			Native WinNT graphic/picture pocket window 
**			interface of CNCLGraphicWin class
**	CONTAINS: CNCLPockWin class functions and structure
**				declarations
**
**    COPYRIGHT 2005 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntgphwin.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:19
*********************************************************************/

/////////////////////////////////////////////////////////////////////////////
#ifndef NCLGRAPHICWIN_H
#define NCLGRAPHICWIN_H
#include "wsntres.h"
#include "mpocket.h"
#include "wsntgraphic.h"

class CNCLGraphicWin : public CDialog
{
public:
	CNCLGraphicWin(CWnd* pParentWnd, char *filename, UM_pkwin_type type);
	~CNCLGraphicWin();
	char * GetWin();

	//{{AFX_DATA(CNCLGraphicWin)
	enum { IDD = IDD_PICTURE };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLGraphicWin)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
 	int m_init;
	CDC	*m_pDC;
	CWnd *m_picwin;

	UM_pkwin_type m_Wtype;
	char m_file[256];
	CNCLgraphic m_picture;
	//{{AFX_MSG(CNCLGraphicWin)
	virtual BOOL OnInitDialog();
	virtual void OnClose();
	afx_msg void OnPaint();
	virtual void OnSize( UINT nType, int cx, int cy );
	afx_msg int  OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnDestroy();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif
