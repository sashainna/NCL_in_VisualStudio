/* 
 * Class Name:          PwMainFrame
 * 
 * Description:         Mainwindow for the application
 * 
 * Notes/Warnings:      
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwMainFrame.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:36
c
 */

#ifndef suimainworkwindow_h
#define suimainworkwindow_h
#include "SUiMainWorkWindow.h"
#endif

#ifndef suicommandlist_h
#define suicommandlist_h
#include "SUiCommandList.h"
#endif

class PwAboutDlg;
class PwTextDlg;

class PwMainFrame : public SUiMainWorkWindow 
{
	friend class PwApplication;
	friend class PwMainTemplate;
	private:
	MsString m_sLogMessage;
	SUiCommandList	*m_pFileCommands;
	SUiCommandList	*m_pViewCommands;
	SUiCommandList	*m_pHelpCommands;
	PwAboutDlg	*m_pAboutDialog;
	PwTextDlg	*m_pTextDialog;
	int exitOk;
	
	protected:

    virtual void CreateMenuPanes();
    virtual void CreateToolButtons(){}
	
	public:
	
	// The following enumerated types are used to utilize a single callback to handle
	//	a wide variety of commands in the main menu
	
	enum MainFrameMenu
	{
		PW_FILE_SAVE=1, 
		PW_FILE_DOCUMENT,
		PW_FILE_MACHINE,
		PW_FILE_QUIT,
		PW_VIEW_DOC,  
		PW_VIEW_MACH,  
		PW_MAIN_HELP_ABOUT  
	};
	
	PwMainFrame(char *);
	
	void MainFrameCallback(MainFrameMenu); 
	
	private:
	
	/*
	 * Function:	OnLoadJob
	 *
	 * Description: Saving and loading of job files (just a sample function)
	 *
	 */
	void OnLoadJob(char *, char *);
	
	/*
	 * Function:	ProgramLoadCallback
	 *
	 * Description: sample callback for handling a file open dialog 
	 *
	 */
	static void ProgramLoadCallback(void *,char *, char *);
	
	/*
	 * Function:	DoAbout
	 *
	 * Description: Bring up the about dialog for PostWorks
	 *
	 */
	void DoAbout();

	/*
	 * Function:	ViewDocumentFile
	 *
	 * Description: Bring up a trext dialog to view the document file
	 *
	 */
	void ViewDocumentFile();

/*
.....Clean this up
*/
	int SaveMachine(int);
	int CreateMachineSim();
	void ViewMachineSim();
	void CreateDocument();
	static void OwYesCallback(void *);
	static void OwNoCallback(void *clientData);
	static void MsYesCallback(void *);
	static void MsNoCallback(void *clientData);
	static void CdOkCallback(Widget widget, XtPointer client_data,
		XtPointer call_data);
};

