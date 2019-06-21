/*********************************************************************
**  NAME:  PtdReseqDialogManager.h
**  Description:
**				all member function and variable for class PtdReseqDialogManager
**				A class for Reseqence dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdReseqDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:32
*********************************************************************/
#ifndef PTDRESEQDIALOGMGRH
#define PTDRESEQDIALOGMGRH

#include "PtdBase.h"
#include "PtdMainWindow.h"
#include "PtdRangeDialogManager.h"
                           

class PtdReseqDialogManager : public PtdBase
{
	protected:
		int m_select;
		int m_answer;
		Widget m_parent;
		Widget m_wbseq;
		Widget m_wseqinc;
		Widget m_wseqn;
		Widget m_wnonly;
		Widget m_wrange;
		Widget m_wok;
		Widget m_wcancel;
	public:
		int m_seqinc, m_bseq, m_seqn, m_nonly;
		RangeDlgStruct *m_rangeStruct;
    
		PtdReseqDialogManager(Widget, char *);
		~PtdReseqDialogManager();
    
		int Post(char *, 
			 void *clientData      = NULL);
		static void OkCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void CancelCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void RangeCallback(Widget, XtPointer clientData,
								XtPointer callData);
};
#endif
