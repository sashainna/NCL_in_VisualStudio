/* 
 * Class Name:          PwMainTemplate 
 * 
 * Description:         main window template for postworks
 * 
 * Notes/Warnings:      
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwMainTemplate.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:37
c
 */

#ifndef suidocviewtemplate_h
#define suidocviewtemplate_h
#include "SUiDocViewTemplate.h"
#endif

class PwMainFrame;

class PwMainTemplate : public SUiDocViewTemplate
{
	friend class PwApplication;
	private:
	PwMainFrame	*m_pFrame;

	protected:
	
	public:
	/*
	 * Function:	PwMainTemplate
	 *
	 * Description: Constructor
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	PwMainTemplate(MsString docString);
	
	/*
	 * Function:	~PwMainTemplate
	 *
	 * Description: Destructor
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	virtual ~PwMainTemplate();	

	/*
	 * Function:	OpenDocumentFile
	 *
	 * Description: open document file of that name. If name is null, 
	 *				open a new document of that type
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	virtual MsBoolean OpenDocumentFile(const char *);

	/*
	 * Function:	CloseDocument
	 *
	 * Description: close the document and all its associated views
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	virtual MsBoolean CloseDocument();

	/*
	 * Function:	Access functions
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
};
