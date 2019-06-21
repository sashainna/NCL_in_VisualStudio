/*********************************************************************
**  NAME:  PtdMainView.h
**  Description:
**				all member function and variable for class PtdMainView
**				
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdMainView.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:30
*********************************************************************/
#ifndef PTDMAINVIEWH
#define PTDMAINVIEWH

#ifndef Ptdchildtemplate_h
#define Ptdchildtemplate_h
#include "PtdChildTemplate.h"
#endif

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "PtdView.h"
#include "PtdRangeDialogManager.h"
#include "PtdWindowStatus.h"

class PtdMainView : public PtdView
{
	protected:
		PtdChildTemplate **m_pChildList;
		int m_numChild;
		int m_mpost;
		int m_answer;
		int m_CutterDialogIndex;
	
	private:

		void InitData();

	public:
    
		PtdMainView(char *);    
		~PtdMainView();

		void Set_ftype(int ftype);
		int HandleSafeClose();
		virtual void InitialUpdate();
    	virtual void  Update(PtdView *view = NULL);
		void ProgramOpenBackup(char*);
		void ProgramNewCallback();
		void RemoveChild(PtdChildTemplate*);
		void RemoveChild(int id);
		void AttachChild(PtdChildTemplate*);
		void SetCutterIndex(int);
		void ResetCutterIndex(int);
		void LoadCutterData();
		int Get_subwindow_name(char *ofile, char *infile, char *fname,
				char * fext);
		int Get_window_status(Ptd_File_status **files, int *filenum, 
					char *input, char *output);


};
#endif

