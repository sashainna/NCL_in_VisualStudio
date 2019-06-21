/*********************************************************************
**  NAME:  PtdTemplate.h
**  Description:
**				all member function and variable for class PtdTemplate
**			
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdTemplate.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:32
*********************************************************************/
#ifndef PTDTEMPLATEH
#define PTDTEMPLATEH
 
#include "PtdString.h"
#include "PtdView.h"
#include "PtdFrameWindow.h"

class PtdTemplate
{
	protected:
	
	PtdView			*m_pView;
	PtdFrameWindow	*m_pFrame;
	
	PtdTemplate(PtdString docString);
	
	public:
	
	PtdTemplate();
	PtdFrameWindow *GetCurrentFrame() { return m_pFrame; }
	
	virtual ~PtdTemplate();	
	virtual void Manage();

	virtual void SetFrame(PtdFrameWindow *pFrame) { m_pFrame = pFrame;}
	virtual void SetView(PtdView *pView) { m_pView = pView;}
	PtdFrameWindow *GetFrame() { return m_pFrame;}
	PtdView *GetView() { return m_pView;}
	void SetModifiedFlag(int flag)
	{
		m_pView->SetModifiedFlag(flag);
	}
};
#endif
