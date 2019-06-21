 /************************************************************************
c
c   FILE NAME: PtedChildWindow.h
c
c	 Description - Functions and struct declarations for
c		CPtedChildWindow class (Windows for display text)
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PtedChildWindow.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:33
c
c**********************************************************************
*/

#ifndef PTDCHILDWINDOW_H
#define PTDCHILDWINDOW_H

#include "PtedWindow.h"

class CPtedChildWindow : public CPtedWindow
{
public:
	CPtedChildWindow(CWnd* pParent = NULL, char *filename = NULL, int fopen = 0, int type=1);
	int DlgClose();
	virtual void SetFtype(int type);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPtedChildWindow)
	protected:
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

protected:

	int m_ChildType;
	// Generated message map functions
	//{{AFX_MSG(PtedChildWindow)
	virtual void OnFileOpen();
	virtual void DlgQuit();
	virtual void OnClose();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif

