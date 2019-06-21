/*********************************************************************
**  NAME:  PtdFileDialogManager.h
**  Description:
**				all member function and variable for class PtdFileDialogManager
**				A class for File Selection dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdFileDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:29
*********************************************************************/
#ifndef PTDFILEDIALOGMGRH
#define PTDFILEDIALOGMGRH
                           
#include "pwenv.h"
#include "PtdDialogManager.h"

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
}fileDlgStruct;

class PtdFileDialogManager : public PtdDialogManager
{
	protected:
		int m_select;
		int m_answer;
		char m_selfile[UX_MAX_PATH];
		int m_vflag, m_lstflag;
		Widget m_dialog, m_wfilter;
		Widget m_wverify, m_wftyp, m_wtylst;
/*
.....not used any more
		void do_search2(char *ext);
*/
	public:
    
		PtdFileDialogManager(Widget parent, char *, int lstflag=1, int vflag=0,
				int verify = 0);
		char m_lstlabel[20][200];
		char m_lstext[20][80];
		int m_verify;
		virtual Widget GetDialog();
		virtual Widget CreateDialog(Widget);
		virtual int Post(char *, 
					void *clientData      = NULL,
			 		int type = 1 );
		static void OkCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void CancelCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void flistCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void ftyCallback(Widget, XtPointer clientData,
                        XtPointer callData);
};
#endif
