/*********************************************************************
**  NAME:  PtdBase.h
**  Description:
**          all member function and variable for class PtdBase
**          base class for PTED application
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdBase.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:28
*********************************************************************/
#ifndef PTDBASEH
#define PTDBASEH


#include "PtdProtocol.h"
#include "PtdString.h"

class PtdBase : public PtdProtocol 
{
	private:
    
	static void WidgetDestroyedCallback (Widget, 
										XtPointer, 
										XtPointer );
    
	protected:
    
		PtdBase (char *);
    	void InstallDestroyHandler();
    
    	virtual void WidgetDestroyed(); 
    
	public:
    
		virtual ~PtdBase();  
    
		virtual void Manage();
    
};
#endif
