/* 
 * Class Name:		PwHelpDlg
 * 
 * Description:		A dialog to bring up the Help Viewer
 * 
 * Notes/Warnings:	concrete class 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwHelpDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:36
c
 */

#ifndef suiappdialogmanager_h
#define suiappdialogmanager_h
#include "SUiAppDialogManager.h"
#endif

#include <Xm/Xm.h>

class SUiActionManager;

class PwHelpDlg : public SUiAppDialogManager 
{
	protected:
	int m_numPrompts;
	char m_PromptData[60][20], m_winTitle[80];
	char *m_pFileData;
	
	Widget m_wTextWidget;
	SUiActionManager *m_pActionManager;
	
	
	/*
	 * Function:	CreateDialog
	 *
	 * Description: create the dialog
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    Widget CreateDialog(Widget parent);
	
	
  public:
    
	static void Close(Widget, XtPointer, XtPointer);
	static void ScreenUp(Widget, XtPointer, XtPointer);
	static void ScreenDown(Widget, XtPointer, XtPointer);

 	/*
	 * Function: PwTextDlg	
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
    PwHelpDlg(Widget, char *);

 	/*
	 * Function: ~PwHelpDlg	
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
    ~PwHelpDlg();
	/*
	 * Function:	SetData
	 *
	 * Description: load the file into the dialog
	 *
	 * Input:		
	 *
	 * Output:
	 *
	 * Warning: virtual function implementation
	 *
	 */
	 
    void SetData(char cPrompt[60][20], int, char *);

	/*
	 * Function:	InitDialog
	 *
	 * Description: Initialize the dialog
	 *
	 * Input:		
	 *
	 * Output:
	 *
	 * Warning: virtual function implementation
	 *
	 */
	 
    virtual void InitDialog();
	void CloseDown();

};
