/* 
 * Class Name:		SUiDynDlg
 * 
 * Description:		A class to encapsulate dynamic dialogs
 * 
 * Notes/Warnings:	abstract class 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        SUiDynDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:38
c
 */

#ifndef suiappdialogmanager_h
#define suiappdialogmanager_h
#include "SUiAppDialogManager.h"
#endif

#include <Xm/Xm.h>

class SUiDynDlg : public SUiAppDialogManager 
{
	protected:

	int m_winID;
	int m_numChildren;
	SUiDynDlg **m_pChildren;
	Widget m_wMessageDlg;
	
	/*
	 * Function:	AddChild
	 *
	 * Description: add a child dialog to the existing list
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    virtual void AddChild(SUiDynDlg *);
	
	/*
	 * Function:	RemoveChild
	 *
	 * Description: remove a child dialog from the existing list
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    virtual void RemoveChild(SUiDynDlg *);

	/*
	 * Function:	IsChild
	 *
	 * Description: query if the child is available
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    virtual int IsChild(int);

	/*
	 * Function:	GetChild
	 *
	 * Description: get the child
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    virtual SUiDynDlg *GetChild(int);

	/*
	 * Function:	Quit
	 *
	 * Description: close this window
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:pure virtual function
	 *
	 */
    virtual void Quit()=0;


	/*
	 * Function:	HandleSafeClose
	 *
	 * Description: Handles the safe closing of this dialog
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: 
	 *
	 */
	void HandleSafeClose();
	
	/*
	 * Function:	SaveDataAndClose, CancelDataAndClose, DoNotClose
	 *
	 * Description: static functions to handle safe closing of this dialog
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
	static void SaveDataAndClose(Widget, XtPointer, XtPointer);
	static void CancelDataAndClose(Widget, XtPointer, XtPointer);
	static void DoNotClose(Widget, XtPointer, XtPointer);
  public:

 	/*
	 * Function: SUiDynDlg	
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
    SUiDynDlg(Widget, char *, int);

 	/*
	 * Function: ~SUiDynDlg	
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
    ~SUiDynDlg();
	
	/*
	 * Function:	SaveData
	 *
	 * Description: Save the data into the application
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: pure virtual function
	 *
	 */
    virtual int SaveData()=0;

	/*
	 * Function:	Remove
	 *
	 * Description: remove a child dialog 
	 *
	 * Input:		
	 *
	 * Output:
	 *
	 * Warning: 
	 *
	 */
	virtual void Remove(SUiDynDlg *);

	/*
	 * Function:	Access functions
	 *
	 * Description: 
	 *
	 * Input:		
	 *
	 * Output:
	 *
	 * Warning:
	 *
	 */
	int ID() { return m_winID;}
	
};
