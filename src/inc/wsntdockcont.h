/************************************************************************
**
**   FILE NAME: wsntdockcont.h
**
**	 Description - Functions and struct declarations for
**		CNCLDialogBar class 
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdockcont.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:16
**
************************************************************************
*/
#if !defined WSNTDOCKCONT_H
#define WSNTDOCKCONT_H

#if _MSC_VER >= 1000
#pragma once
#endif 
#include "afxpriv.h"

/////////////////////////////////////////////////////////////////////////////
// CNCLDockContext

class CNCLDockContext : public CDockContext
{
//	DECLARE_DYNAMIC(CNCLDockContext)
// Construction
public:
	CNCLDockContext(CControlBar* pBar=NULL);

// Implementation
public:
	virtual ~CNCLDockContext();
/*
.....redefined those function for our own draging routine
*/
	virtual void StartDrag(CPoint pt);
	void Move(CPoint pt);       
	void EndDrag();        
	BOOL Track();
	virtual void ToggleDocking();

protected:
};

#endif
