/*********************************************************************
**  NAME:  PtdFindDialogManager.h
**  Description:
**				all member function and variable for class PtdFindDialogManager
**				A class for File Selection dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdFindDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:30
*********************************************************************/
#ifndef PTDFINDDIALOGMGRH
#define PTDFINDDIALOGMGRH

#include "PtdBase.h"
#include "PtdMainWindow.h"
#include "PtdRangeDialogManager.h"
                           
typedef struct
{
	char *title;
	char *replacestr;
	char *findstr;
	int *fcase;
	int *downdir;
	int *freg;
/*
.....flag = 1: replace next
.....flag = 2: replace all
*/
	int *fflag; 
	RangeDlgStruct *rangeStruct;
} findDlgStruct;

class PtdFindDialogManager : public PtdBase
{
	protected:
		int m_select;
		int m_answer;
		char m_findstr[200];
		char m_replacestr[200];
		int m_case;
		int m_downdir;
		int m_ffind;
		int m_reg;
		Widget m_parent;
		Widget m_wfindtext;
		Widget m_wreplacetext;
		Widget m_wcase;
		Widget m_wreg;
		Widget m_wup_opt;
		Widget m_wdown_opt;
		Widget m_wrange;
		Widget m_wok;
		Widget m_wcancel;
		Widget m_wall;
		int m_type;
		RangeDlgStruct *m_rangeStruct;
		PtdRangeDialogManager *m_rangeBox;
	public:
    
	PtdFindDialogManager(Widget, char *, int type = 1);
	~PtdFindDialogManager();
    
		int Post(char *, 
			 void *clientData      = NULL);
		static void FindNextCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void CancelCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void ReplaceNextCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void ReplaceAllCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void RangeCallback(Widget, XtPointer clientData,
								XtPointer callData);

};
#endif
