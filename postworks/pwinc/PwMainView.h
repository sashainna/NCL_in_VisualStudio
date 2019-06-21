/* 
 *						Change History 
 *---------------------------------------------------------------- 
 * Version	Date		Release		Author		Description 
 *---------------------------------------------------------------- 
 *	 1.0	08/31/94	1.0			NKN			Initial version                  
 * 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwMainView.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:37
c
 */ 

/* 
 * Class Name:          PwMainView 
 * 
 * Description:         Defines protocol for a view to display primary information
 * 
 * Notes/Warnings:      concrete class. 
 */
                           
#ifndef suiview_h
#define suiview_h
#include "SUiView.h"
#endif

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

class SUiDynDlg;

class PwMainView : public SUiView
{
	protected:
    Dimension	m_Width;
	Dimension	m_Height;
	SUiDynDlg **m_pDlgList;	// dynamic dialogs created in this level	
	int		m_numDlgs;
	int m_level[10];
	int m_curStage;
	Widget m_wMessageDlg;
	
	private:

	/*
	 * Function:	AttachDialog
	 *
	 * Description: attach a new dynamic dialog to the class
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	void AttachDialog(SUiDynDlg *);

	/*
	 * Function:	RemoveDialog
	 *
	 * Description: remove the dialog from the list
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	void RemoveDialog(SUiDynDlg *);

	/*
	 * Function:	DoesDialogExist
	 *
	 * Description: remove the dialog from the list
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	int DoesDialogExist(int);

	/*
	 * Function:	GetDialog
	 *
	 * Description: remove the dialog from the list
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	SUiDynDlg *GetDialog(int);
	/*
	 * Function:	InitData
	 *
	 * Description: initialize all view data members
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	void InitData();
	
	/*
	 * Function:	RefreshCallback, ResizeCallback
	 *
	 * Description: called when the widget is resized and refreshed
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	static void RefreshCallback(Widget,XtPointer, XtPointer);
	static void ResizeCallback(Widget,XtPointer, XtPointer);

	/*
	 * Function:	Quit
	 *
	 * Description: handle closing the application
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
	void Quit();

	/*
	 * Function:	AddCallbacks, RemoveCallbacks
	 *
	 * Description: called to add and remove callbacks
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	void AddCallbacks();
	void RemoveCallbacks();
	
	public:
    

	/*
	 * Function:	HandleSafeClose
	 *
	 * Description: Handle the safe closing of this view
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
	 * Description: static callback functions to handle safe closing of this view
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

	/*	
	 * Function:	PwMainView
	 *
	 * Description: Constructor
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	PwMainView(char *);    

	/*
	 * Function:	~PwMainView
	 *
	 * Description: Destructor
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	~PwMainView();
	    
	/*
	 * Function:	Create
	 *
	 * Description: Actual creation of the view
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	void Create(Widget);
	
	/*
	 * Function:	InitialUpdate
	 *
	 * Description: A one time initialization of the view
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	void InitialUpdate();

	/*
	 * Function:	Update
	 *
	 * Description: Update the view with the changed document data
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: pure virtual function to be implemented by derived classes
	 *			
	 *
	 */
    virtual void  Update(SUiView *view = NULL);

	/*
	 * Function:	UpdateDocument
	 *
	 * Description: Update the document with modified data
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:pure virtual function to be implemented by derived classes 
	 *			
	 *
	 */
	virtual void  UpdateDocument();

	/*
	 * Function:	
	 *
	 * Description: Access functions
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	 virtual Widget WorkArea() { return m_wBaseWidget; }
	 
	/*
	 * Function:	MainMenuCallback	
	 *
	 * Description: Handles all the menu buttons in the main view
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	 static void MainMenuCallback(Widget, XtPointer, XtPointer);

	/*
	 * Function:	QuitCallback	
	 *
	 * Description: Handles all the quit button in the main view
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	static void QuitCallback(Widget, XtPointer, XtPointer);
	/*
	 * Function:	Remove	
	 *
	 * Description: called by dynamic dialogs when their okay or 
	 *				cancel buttons are pressed
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
	 void Remove(SUiDynDlg *);

};

