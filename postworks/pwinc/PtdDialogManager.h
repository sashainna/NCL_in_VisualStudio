/*********************************************************************
**  NAME:  PtdDialogManager.h
**  Description:
**				all member function and variable for class PtdDialogManager
**				A base class for dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:29
*********************************************************************/
#ifndef PTDDIALOGMGTH
#define PTDDIALOGMGTH
                           
#include "PtdBase.h"
#include "PtdDialogCbData.h"

class PtdDialogManager : public PtdBase 
{
	protected:
		Widget m_parent;

		static void DestroyTmpDialogCallback ( Widget, 
					  XtPointer, 
					  XtPointer );
		static void OkCallback ( Widget, 
			    XtPointer, 
			    XtPointer );
    
		static void CancelCallback ( Widget, 
				XtPointer, 
				XtPointer );
    
		static void HelpCallback ( Widget, 
			      XtPointer, 
			      XtPointer );
    
		virtual void CleanUp(Widget, PtdDialogCbData*);
    
		virtual Widget GetDialog()=0; 
    
		virtual Widget CreateDialog(Widget) = 0;   
    
		PtdDialogManager(Widget, char *);
    
};
#endif
