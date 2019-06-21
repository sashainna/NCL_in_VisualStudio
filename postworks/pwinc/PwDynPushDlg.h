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
c        PwDynPushDlg.h , 24.1
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

class PwDynPushDlg : public SUiDynDlg
{
	protected:
	
    int m_pLevel[10];
	int m_curStage;
	void *m_pParent;
	int m_parentType;
	NpwDynWinStruct m_DynWinStruct;
	NpwReturnStruct m_RetValStruct;
	
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


  public:
    static void QuitCallback(Widget, XtPointer, XtPointer);
	static void CreateChildCallback(Widget, XtPointer, XtPointer);
	static void OptionCallback(Widget, XtPointer, XtPointer);

 	/*
	 * Function: PwDynPushDlg	
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
	PwDynPushDlg(void *,int, Widget, char *,int*, int, int, NpwDynWinStruct *);

 	/*
	 * Function: ~PwDynPushDlg	
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
    ~PwDynPushDlg();
	
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
