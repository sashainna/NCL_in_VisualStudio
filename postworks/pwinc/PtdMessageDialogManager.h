/*********************************************************************
**  NAME:  PtdMessageDialogManager.h
**  Description:
**				all member function and variable for class PtdMessageDialogManager
**				A base class for message dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdMessageDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:31
*********************************************************************/
#ifndef PTDMSGDIALOGMGRH
#define PTDMSGDIALOGMGRH
                           
#include "PtdDialogManager.h"

class PtdMessageDialogManager : public PtdDialogManager
{
	protected:
		int m_select;
		int m_answer;
    
		virtual Widget GetDialog();
    
	public:
    
		PtdMessageDialogManager(Widget, char *);
    
		virtual int Post(char *, 
						void *clientData  = NULL,
						int type = 1);
		static void YesCallback(void *);
		static void NoCallback(void *);
		static void CancelCallback(void *);
};
#endif
