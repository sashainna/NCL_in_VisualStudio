/*********************************************************************
**  NAME:  PtdIncFileDialogManager.h
**  Description:
**				all member function and variable for class PtdIncFileDialogManager
**				A class for File Selection dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdIncFileDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:30
*********************************************************************/
#ifndef PTDINCFILEDIALOGMGRH
#define PTDINCFILEDIALOGMGRH
                           
#include "PtdFileDialogManager.h"
#include "PtdRangeDialogManager.h"

typedef struct
{
	char *title;
	char *pattern;
	char *dirMask;
	char *dirLabel;
	char *fileLabel;
	char *filterLabel;
	void *cPtr;
	void *cmdPtr;
	char *filename;
	RangeDlgStruct *rangeStruct;
} IncfileDlgStruct;

class PtdIncFileDialogManager : public PtdFileDialogManager
{
	protected:
		PtdRangeDialogManager *m_rangeBox;
		RangeDlgStruct *m_rangeStruct;
	public:
    
   PtdIncFileDialogManager(Widget, char *);
   ~PtdIncFileDialogManager();
    
   virtual int Post(char *, 
			 void *clientData      = NULL,
			 int type = 1 );
	static void RangeCallback(Widget, XtPointer clientData,
                        XtPointer callData);
	static void OkCallback(Widget, XtPointer clientData,
                        XtPointer callData);
	static void CancelCallback(Widget, XtPointer clientData,
                        XtPointer callData);
};
#endif
