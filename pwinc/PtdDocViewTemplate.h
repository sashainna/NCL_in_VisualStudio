/*********************************************************************
**  NAME:  PtdDocViewTemplate.h
**  Description:
**				all member function and variable for class PtdDocViewTemplate
**			
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdDocViewTemplate.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:29
*********************************************************************/
#ifndef PTDDOCVIEWTMPH
#define PTDDOCVIEWTMPH
 
#include "PtdString.h"
#include "PtdDocument.h"
#include "PtdView.h"

#include "PtdFrameWindow.h"


  
class PtdDocViewTemplate
{
	protected:
	
	PtdString 	m_sDocString;	// '\n' separated names
	PtdDocument		*m_pDocument;
	PtdView			*m_pView;
	PtdFrameWindow	*m_pFrame;
	
	PtdDocViewTemplate(PtdString docString);
	
	public:
	
	enum PtdEDocStringIndex 
	{
		PtdEWindowTitle,	// default window title
		PtdEDocName,		// user visible name for default document
		PtdEFileNewName,	// user visible name for FileNew
		
		
		PtdEFilterName,		// user visible name for FileOpen
		PtdEFilterExt
	};
	
	enum Confidence
	{
		noAttempt, 
		maybeAttemptForeign, 
		maybeAttemptNative, 
		yesAttemptForeign, 
		yesAttemptNative, 
		yesAlreadyOpen
	};
	
	virtual Confidence MatchDocumentType(const char *pathName, 
							PtdDocument*& matchType);
		
	PtdDocViewTemplate();
	
	virtual ~PtdDocViewTemplate();	

	virtual void AddDocument(PtdDocument *doc);	// must override
	virtual void RemoveDocument(PtdDocument *doc);	// must override

	virtual int SaveModified();
	virtual int OpenDocumentFile(const char *)=0;
	virtual void GetDocString(PtdString& ,PtdEDocStringIndex)const;	
	virtual void Manage();

	virtual void SetDocument(PtdDocument *pDoc) { m_pDocument = pDoc;}
	virtual void SetFrame(PtdFrameWindow *pFrame) { m_pFrame = pFrame;}
	virtual void SetView(PtdView *pView) { m_pView = pView;}
	PtdDocument *GetDocument() { return m_pDocument;}
	PtdFrameWindow *GetFrame() { return m_pFrame;}
	PtdView *GetView() { return m_pView;}
};
#endif
