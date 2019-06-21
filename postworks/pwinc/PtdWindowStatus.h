/*********************************************************************
**  NAME:  PtdWindowStatus.h
**  Description:
**				all member function and variable for class PtdWindowStatus
**				A class for Window Status dialogs
**    CONTAINS:
**
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			PtdWindowStatus.h , 24.1       
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:33
*********************************************************************/
#ifndef PTDWINDOWSTATUSH
#define PTDWINDOWSTATUSH

#include "pwenv.h"
#include "PtdBase.h"
#include "PtdMainWindow.h"
                           
typedef struct
{
   int type;
   char filename[UX_MAX_PATH];
   int wtype;
} Ptd_File_status;

class PtdWindowStatus : public PtdBase
{
	protected:
		int m_select;
		int m_answer;
		int m_filenum;
		Ptd_File_status *m_files;
		char m_input_mdf[UX_MAX_PATH], m_output_mdf[UX_MAX_PATH];
		Widget m_parent;
		Widget m_inputwin;
		Widget m_outputwin;
		Widget m_wok;
		Widget m_labelwin1[20], m_labelwin2[20], m_labelwin3[20];
	public:
		PtdWindowStatus(Widget, char *, Ptd_File_status *files = NULL, 
						int file_num=0, char * input = NULL, char* output=NULL);
		~PtdWindowStatus();
    
		int Post(char *, 
			 void *clientData      = NULL);
		static void OkCallback(Widget, XtPointer clientData,
                        XtPointer callData);
};
#endif
