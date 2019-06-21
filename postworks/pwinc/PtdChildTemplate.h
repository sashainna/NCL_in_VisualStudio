/*********************************************************************
**  NAME:  PtdChildTemplate.h
**  Description:
**				all member function and variable for class PtdChildTemplate
**				Childwindow template for the application
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdChildTemplate.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:28
*********************************************************************/
#ifndef PTDCHILDTMPH
#define PTDCHILDTMPH

#include "PtdTemplate.h"
#include "PtdView.h"
#include "PtdFrameWindow.h"


class PtdChildTemplate : public PtdTemplate
{
	friend class PtdChildView;
	friend class PtdMainView;
	private:
		static int m_num;
		static int untitle_doc;
		int m_id;
		PtdView* m_parent;
/*
....type = 1: normal doc text file window
....type = 2: backup file window
....type = 3: text read only file window
....type = 4: main window for mpost
*/
		int m_type;

	protected:
	
	public:

		PtdChildTemplate(char* docString, PtdView*, int type=1);
		virtual ~PtdChildTemplate();	
		virtual int OpenDocumentFile(const char *);
		virtual int CloseDocument();
		int getid() { return m_id; }
		int GetType() { return m_type; }
		int SetId(int id);
		int setTextandTitle(char *, char *);
		int setTitle(char *title);
		int setFilename(char *);
		int getFilename(char *);
		void setFileType(int);
		int getFileType();
};
#endif
