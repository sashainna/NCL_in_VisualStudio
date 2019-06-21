/* 
 * Class Name:		PwDynDlg
 * 
 * Description:		A class to encapsulate dynamic dialogs
 * 
 * Notes/Warnings:	concrete class 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwDynPromptDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:36
c
 */

#ifndef suidyndlg_h
#define suidyndlg_h
#include "SUiDynDlg.h"
#endif

#ifndef npwheaders_h
#define npwheaders_h
#include "NpwHeaders.h"
#endif

#include <Xm/Xm.h>

class SUiActionManager;
class PwHelpDlg;

class PwDynPromptDlg : public SUiDynDlg
{
	protected:
	
    int m_pLevel[10];
	int m_curStage;
	void *m_pParent;
	int m_parentType;
	PwHelpDlg *m_pHelpDialog;
	SUiActionManager *m_pActionManager;	
	NpwDynWinStruct m_DynWinStruct;
	NpwReturnStruct m_RetValStruct;
	
	Widget *m_pChildWidget;	/* Keep a list of the widgets whose values
							 * we are interested in retrieving later 
							 */
	Widget m_wScrWin;
	Widget m_wControlBoard;
	
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
	
	/*
	 * Function:	Quit
	 *
	 * Description: close this window
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: pure virtual implementation
	 *
	 */
    virtual void Quit();

	/*
	 * Function:	RecalcLayout()
	 *
	 * Description: Recalculate the layout for a dynamic Choice button
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: Currently called from choice button callback.
	 *
	 */
    void RecalcLayout();
	void MakeNewDialog();


  public:
	static void OptionCallback(Widget, XtPointer, XtPointer);
	static void Ok(Widget, XtPointer, XtPointer);
	static void Apply(Widget, XtPointer, XtPointer);
	static void Cancel(Widget, XtPointer, XtPointer);
	static void Help(Widget, XtPointer, XtPointer);

 	/*
	 * Function: PwDynPromptDlg	
	 *
	 * Description: constructor
	 *
	 * Input:		
	 *
	 * Output: 	
	 *
	 * Warning: constructor is overloaded
	 *
	 */
	PwDynPromptDlg(void *,int, Widget, char *,int*, int, int, NpwDynWinStruct *);
 	/*
	 * Function: ~PwDynPromptDlg	
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
    ~PwDynPromptDlg();
	
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
	
	/*
	 * Function:	SaveData
	 *
	 * Description: Save the data into the application
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: pure virtual implementation
	 *
	 */
    virtual int SaveData();

	/*
	 * Function:	Access functions
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
};
