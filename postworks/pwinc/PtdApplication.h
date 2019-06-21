/*********************************************************************
**  NAME:  PtdApplication.h
**  Description:
**				all member function and variable for class PtdApplication
**				which encapsulation of all application
**				specific initialization for PTD
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdApplication.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:28
*********************************************************************/
#ifndef PTDAPPLICATIONH
#define PTDAPPLICATIONH

#include "PtdString.h"
#include "PtdBase.h"
#include "PtdMainWindowArray.h"
#include "PtdBatch.h"

class PtdFrameWindow;
class PtdMainTemplate;

class PtdApplication : public PtdBase 
{
/*
.....Allow main and PtdMainWindow to access protected member
.....functions
*/
	friend class PtdMainWindow;
   friend int main(unsigned int, char **);

    private:

		PtdMainTemplate		*m_pMainTemplate;
		int ApplicationInit();

	protected:
	
		static PtdApplication *theApplication;
		Display          *m_pDisplay;
		XtAppContext     m_AppContext;
		XVisualInfo         m_VisualInfo;
		PtdString  m_sApplicationClass; 
		int        m_bFileSave;
		PtdFrameWindow *m_pMainWindow;

		virtual void Initialize(unsigned int *, char **);  
		virtual void HandleEvents();

    public:

		PtdApplication (char *);
		~PtdApplication();
		void ApplicationExit();
		char *m_argv[10];
	
		Display      *Get_Display()  {return m_pDisplay;}
		XtAppContext  AppContext()         {return m_AppContext;}
		virtual const PtdString ClassName() {return m_sClassName;}
		const PtdString ApplicationClass()  {return m_sApplicationClass;}
		XVisualInfo VisualInfo()         {return m_VisualInfo;}

		static PtdApplication *Application() { return theApplication;}
		virtual void SafeApplicationExit();
		virtual void SetMainTitle(char *);
		virtual void SetMainWindow(PtdFrameWindow *pWin)
			{ m_pMainWindow = pWin;}
		PtdFrameWindow* GetMainWindow()
			{ return m_pMainWindow;}
		PtdMainTemplate *MainTemplate() { return m_pMainTemplate;}
};

extern PtdApplication *m_pPtdApplication;
extern PtdBatch *m_pBatch;
#endif

