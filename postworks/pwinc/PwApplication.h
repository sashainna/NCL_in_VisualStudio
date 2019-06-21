/* 
 * Class Name:          PwApplication 
 * 
 * Description:         Encapsulation of all application 
 *						specific initialization for postworks
 * 
 * Notes/Warnings:      
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwApplication.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:36
c
 */

#ifndef suiapplication_h
#define suiapplication_h
#include "SUiApplication.h"
#endif

class PwMainTemplate;

class PwApplication : public SUiApplication 
{
    private:
	
	// templates
	
	PwMainTemplate		*m_pMainTemplate;
	 
	/*
	 * Function:	ApplicationInit
	 *
	 * Description: Initialize all application specific variables
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: private function
	 *
	 */
	MsStatus ApplicationInit();

    protected:
	
		Widget m_wMessageDlg;
	/*
	 * Function:	Initialize
	 *
	 * Description: application initialization..
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: overridden for application specific initialization
	 *
	 */
    virtual void Initialize(unsigned int *, char **);  
    
    public:

	/*
	 * Function:	PwApplication
	 *
	 * Description: constructor
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	PwApplication (char *);
	
	/*
	 * Function:	PwApplication
	 *
	 * Description: destructor
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	~PwApplication();
	
	/*
	 * Function:	ApplicationExit
	 *
	 * Description: do clean up before exiting the application
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	void ApplicationExit();
	
	/*
	 * Function:	VerifyExit
	 *
	 * Description: check if application termination is desired
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	void VerifyExit();

	/*
	 * Function:	YesCallback, NoCallback
	 *
	 * Description: callbacks to handle application exit
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	static void YesCallback(Widget, XtPointer, XtPointer);
	static void NoCallback(Widget, XtPointer, XtPointer);
	static void CancelCallback(Widget, XtPointer, XtPointer);

	/*
	 * Function:	SafeApplicationExit
	 *
	 * Description: called to exit from the application safely
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: virtual function implementation
	 *
	 */
	virtual void SafeApplicationExit();
	
	/*
	 * Function:	Access functions
	 *
	 * Description: destructor
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	PwMainTemplate *MainTemplate() { return m_pMainTemplate;}
};

extern PwApplication *m_pPwApplication;


