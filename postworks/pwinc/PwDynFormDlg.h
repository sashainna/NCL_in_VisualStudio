/* 
 * Class Name:		PwDynFormDlg
 * 
 * Description:		A class to encapsulate dynamic dialogs
 * 
 * Notes/Warnings:	concrete class 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwDynFormDlg.h , 24.1
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

class PwDynFormDlg : public SUiDynDlg
{
	protected:
	
	int m_pLevel[10];
	int m_curStage;
	void *m_pParent;
	int m_parentType;
	int currentListPos;
	PwHelpDlg *m_pHelpDialog;
	SUiActionManager *m_pActionManager;	
	NpwDynWinStruct m_DynWinStruct;
	NpwDynFormStruct m_DynFormStruct;
	NpwReturnStruct m_RetValStruct;
	Widget scrollList;
	Widget *m_pMenuEntry;
	Widget *m_pChoice;
	Widget *m_pChildWidget;	/* Keep a list of the widgets whose values
							 * we are interested in retrieving later 
							 */
	
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
	 * Function:	CreateTable
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
    Widget CreateTable(Widget parent);

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
	virtual void SaveSelection();


  public:
	static void OptionCallback(Widget, XtPointer, XtPointer);
	static void SelectCallback(Widget, XtPointer, XtPointer);
	static void ArrowCallback(Widget, XtPointer, XtPointer);
	static void TableCallback(Widget, XtPointer, XtPointer);
	static void AdjustCallback(Widget, XtPointer, XtPointer);
	static void Ok(Widget, XtPointer, XtPointer);
	static void Apply(Widget, XtPointer, XtPointer);
	static void Cancel(Widget, XtPointer, XtPointer);
	static void Help(Widget, XtPointer, XtPointer);

 	/*
	 * Function: PwDynFormDlg	
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
	PwDynFormDlg(void *,int, Widget, char *,int*, int, int, NpwDynWinStruct *,
		NpwDynFormStruct *);
 	/*
	 * Function: ~PwDynFormDlg	
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
    ~PwDynFormDlg();
	
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
	 * Function:	SaveSelection
	 *
	 * Description: Send the form data back to the selection list
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: pure virtual implementation
	 *
	 */
//    int SaveSelection();

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
    void RemoveSpaces(char *cbuf, int *knc);
    void AddSpaces(char *cbuf, int knc);
};
