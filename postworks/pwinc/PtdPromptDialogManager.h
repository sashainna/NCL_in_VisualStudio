/*********************************************************************
**  NAME:  PtdPromptDialogManager.h
**  Description:
**				all member function and variable for class PtdPromptDialogManager
**				A class for File Selection dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdPromptDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:32
*********************************************************************/
#ifndef PTDPROMPTDIALOGMGRH
#define PTDPROMPTDIALOGMGRH
                           
#include "PtdDialogManager.h"

typedef struct
{
	char *title;
	char *label;
	char *notes;
	char *prompt;
} promptDlgStruct;

class PtdPromptDialogManager : public PtdDialogManager
{
	protected:
		int m_select;
		int m_answer;
		char m_prompt[256];
	public:
    
		PtdPromptDialogManager(Widget parent, char *);
		virtual Widget GetDialog();
		virtual Widget CreateDialog(Widget);
    
		virtual int Post(char *, 
			 void *clientData      = NULL,
			 int type = 1 );
		static void OkCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void CancelCallback(Widget, XtPointer clientData,
                        XtPointer callData);
};
#endif
