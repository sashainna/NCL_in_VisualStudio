/*********************************************************************
**  NAME:  PtdInfoDialogManager.h
**  Description:
**				all member function and variable for class PtdInfoDialogManager
**				dialog manager for Information pop-up dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdInfoDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:30
*********************************************************************/
#ifndef PTDINFODIALOGMGTH
#define PTDINFODIALOGMGTH

#include "PtdMessageDialogManager.h"

class PtdInfoDialogManager : public PtdMessageDialogManager 
{
    
  protected:
    
	/*
	 * Function:	CreateDialog
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
    Widget CreateDialog(Widget);
    
  public:
    
	/*
	 * Function:	PtdInfoDialogManager
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
    PtdInfoDialogManager(Widget, char *);
};
#endif
