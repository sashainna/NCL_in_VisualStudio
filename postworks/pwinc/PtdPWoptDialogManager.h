/*********************************************************************
**  NAME:  PtdPWoptDialogManager.h
**  Description:
**				all member function and variable for class PtdPWoptDialogManager
**				A class for PWorks Option Specify dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdPWoptDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:32
*********************************************************************/
#ifndef PTDPWOPTDIALOGMGRH
#define PTDPWOPTDIALOGMGRH

#include "pwenv.h"
#include "PtdBase.h"
                           
class PtdPWoptDialogManager : public PtdBase
{
	public:
		char m_adjfile[UX_MAX_PATH];
		int m_ftype, m_llen, m_warn, m_error, m_fatal, m_ident, m_list,
				m_plen, m_print, m_punch, m_simul, m_machine;
		int m_cwarn, m_cerror, m_cfatal;
		char m_clist[20], m_cprint[20], m_cpunch[20], m_csimul[20], 
				m_option[80];

	protected:
		int m_select;
		int m_answer;
		Widget m_parent;
		Widget m_wcheck1;
		Widget m_wcheck2;
		Widget m_wcheck3;
		Widget m_wcheck4;
		Widget m_wcheck5;
		Widget m_wcheck6;
		Widget m_wcheck7;
		Widget m_wcheck8;
		Widget m_wtext1;
		Widget m_wtext2;
		Widget m_wtext3;
		Widget m_wtext4;
		Widget m_wtext5;
		Widget m_wtext6;
		Widget m_wtext7;
		Widget m_wtext8;
		Widget m_wtext9;
		Widget m_wtext10;
		Widget m_wtext11;
		Widget m_wtext12;
		Widget m_wftype;
	public:
    
		PtdPWoptDialogManager(Widget parent, char *, char *);
    
		int Post(char * = NULL, 
				void *clientData      = NULL);
		static void OnOkCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void OnCancelCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void OnBrowseCallback(Widget, XtPointer clientData,
                        XtPointer callData);
};
#endif
