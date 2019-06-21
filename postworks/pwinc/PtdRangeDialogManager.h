/*********************************************************************
**  NAME:  PtdRangeDialogManager.h
**  Description:
**				all member function and variable for class PtdRangeDialogManager
**				A class for Range Specify dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdRangeDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:32
*********************************************************************/
#ifndef PTDRANGEDIALOGMGRH
#define PTDRANGEDIALOGMGRH

#include "PtdBase.h"
                           
#define TED_RANGE_BEGIN1   0x00000001
#define TED_RANGE_BEGIN2   0x00000002
#define TED_RANGE_BEGIN3   0x00000004
#define TED_RANGE_BEGIN4   0x00000008
#define TED_RANGE_BEGIN5   0x00000010
#define TED_RANGE_BEGIN6   0x00000020
#define TED_RANGE_END1     0x00000040
#define TED_RANGE_END2     0x00000080
#define TED_RANGE_END3     0x00000100
#define TED_RANGE_END4     0x00000200
typedef struct
{
	char *title;
	char *baddress;
	char *bstring;
	char *enumber;
	char *eaddress;
	char *estring;
	int begin;
	int end;
} RangeDlgStruct;

typedef struct
{
	void *wptr;
	int choice;
} ToggleStruct;

class PtdRangeDialogManager : public PtdBase
{
	protected:
		int m_select;
		int m_answer;
		char m_enumber[200];
		char m_eaddress[200];
		char m_estring[200];
		char m_baddress[200];
		char m_bstring[200];
		int m_begin;
		int m_end;
		int m_flags;
		Widget m_parent;
		Widget m_wchoice1;
		Widget m_wchoice2;
		Widget m_wchoice3;
		Widget m_wchoice4;
		Widget m_wchoice5;
		Widget m_wchoice6;
		Widget m_wchoice7;
		Widget m_wchoice8;
		Widget m_wchoice9;
		Widget m_wchoice10;
		Widget m_wtext1;
		Widget m_wtext2;
		Widget m_wtext3;
		Widget m_wtext4;
		Widget m_wtext5;
		void Enable_End();
		void Disable_End();
	public:
    
	PtdRangeDialogManager(Widget parent, char *, int flags=0);
    
		int Post(char *, 
			 void *clientData      = NULL,
			 int type = 1 );
		void DialogRaised();
		static void OnOkCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void OnCancelCallback(Widget, XtPointer clientData,
                        XtPointer callData);
		static void ToggleChangeCallback(Widget, XtPointer clientData,
                        XtPointer callData);
};
#endif
