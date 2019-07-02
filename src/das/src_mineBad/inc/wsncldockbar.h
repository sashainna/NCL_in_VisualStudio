/********************************************************************* 
**  NAME:  wsncldockbar.h
**
**			Header file for CNCLDockBar
**	CONTAINS: 
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsncldockbar.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:12
*********************************************************************/
#ifndef WSNCLDOCKBAR__H
#define WSNCLDOCKBAR__H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "afxpriv.h"
#include "wsntmdroptarget.h"

class CNCLDockBar : public CDockBar
{
	DECLARE_DYNCREATE(CNCLDockBar)
public:
	CPoint m_iPoint;
	char m_iText[100];

	CNCLDockBar();
	virtual ~CNCLDockBar();
	virtual CSize CalcFixedLayout( BOOL bStretch, BOOL bHorz );
	BOOL RemoveControlBar(CControlBar*, int nPosExclude = -1, int nAddPlaceHolder = 0);
	CControlBar* GetDockedControlBar(int nPos) const;
	void DockControlBar(CControlBar* pBar, LPCRECT lpRect = NULL);
	void ReDockControlBar(CControlBar* pBar, LPCRECT lpRect = NULL);
	void Get_BarRect(CRect *rect, int *newline, int flag);
	void AddMenuBar(CPoint pt, char* input_text);
protected:
	//{{AFX_MSG(CNCLDockBar)
		void OnAddMenuBar();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

//	CNCLMnDropTarget		*m_TargetDrop;
//	friend class CNCLMnDropTarget;
};

#endif 
