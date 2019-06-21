/* 
 * Class Name:		PwAboutDlg
 * 
 * Description:		The famous About Dialog...
 * 
 * Notes/Warnings:	concrete class 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwAboutDlg.h , 24.1
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

class PwAboutDlg : public SUiAppDialogManager 
{
	protected:
    
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
    
	static void Ok(Widget, XtPointer, XtPointer);
	static void Cancel(Widget, XtPointer, XtPointer);
	static void Help(Widget, XtPointer, XtPointer);

 	/*
	 * Function: PwAboutDlg	
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
    PwAboutDlg(Widget, char *);

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

};
