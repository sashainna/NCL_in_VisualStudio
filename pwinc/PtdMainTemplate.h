/*********************************************************************
**  NAME:  PtdMainTemplate.h
**  Description:
**				all member function and variable for class PtdMainTemplate
**				Mainwindow template for the application
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdMainTemplate.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:30
*********************************************************************/
#ifndef PTDMAINTMPH
#define PTDMAINTMPH

#include "PtdString.h"
#include "PtdTemplate.h"

class PtdFrameWindow;

class PtdMainTemplate : public PtdTemplate
{
	friend class PtdApplication;
	private:
	PtdFrameWindow	*m_pFrame;

	protected:
	
	public:
		PtdMainTemplate(PtdString docString);
	
		virtual ~PtdMainTemplate();	

		virtual int OpenDocumentFile(const char *);

		virtual int CloseDocument();
};
#endif
