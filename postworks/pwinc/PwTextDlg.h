/* 
 * Class Name:		PwTextDlg
 * 
 * Description:		A dialog to bring up the document for PostTool
 * 
 * Notes/Warnings:	concrete class 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwTextDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:37
c
 */

#ifndef suiappdialogmanager_h
#define suiappdialogmanager_h
#include "SUiAppDialogManager.h"
#endif

#include <Xm/Xm.h>

class SUiActionManager;

class PwTextDlg : public SUiAppDialogManager 
{
	protected:
    char m_CurFile[80]; 
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
	static void PageUp(Widget, XtPointer, XtPointer);
	static void PageDown(Widget, XtPointer, XtPointer);

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
    PwTextDlg(Widget, char *);

 	/*
	 * Function: ~PwTextDlg	
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
    ~PwTextDlg();
	/*
	 * Function:	LoadFile
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
	 
    void LoadFile(char *);

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
