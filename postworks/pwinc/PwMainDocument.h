/* 
 * Class Name:          PwMainDocument 
 * 
 * Description:         Class to manage a text document
 * 
 * Notes/Warnings:      concrete
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwMainDocument.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:36
c
 */

#ifndef document_h
#define document_h
#include "Document.h"
#endif

class PwMainView;

class PwMainDocument : public Document
{
	friend class PwMainView;
	
	private:
	void *m_pData;
	
	protected:
	
	public:
	
	/*
	 * Function:	PwMainDocument, ~PwMainDocument
	 *
	 * Description: Constructor, Destructor
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
    PwMainDocument();
	~PwMainDocument();
	
	public:
    
	/*
	 * Function:	OpenDocument
	 *
	 * Description:
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	virtual MsStatus OpenDocument(char *fileName = NULL);
	
	/*
	 * Function:	NewDocument
	 *
	 * Description: 
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	virtual void NewDocument();

	/*
	 * Function:	SaveDocument
	 *
	 * Description: 
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	virtual MsStatus SaveDocument();

	/*
	 * Function:	CloseDocument
	 *
	 * Description: 
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	virtual void CloseDocument();
	
	/*
	 * Function:	Update
	 *
	 * Description: Update the document with any new data
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	void Update();

	/*
	 * Function:	
	 *
	 * Description: Access functions
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	virtual void *Data() { return m_pData;}
	virtual void SetData(void *pData){ m_pData = pData;}
	 
};
