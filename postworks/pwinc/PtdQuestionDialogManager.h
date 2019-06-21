/*********************************************************************
**  NAME:  PtdQuestionDialogManager.h
**  Description:
**				all member function and variable for class PtdQuestionDialogManager
**				dialog manager for question pop-up dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdQuestionDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:32
*********************************************************************/
#ifndef PTDQUESTIONDIALOGMGTH
#define PTDQUESTIONDIALOGMGTH

#include "PtdMessageDialogManager.h"

class PtdQuestionDialogManager : public PtdMessageDialogManager 
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
	 * Function:	PtdQuestionDialogManager
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
    PtdQuestionDialogManager(Widget, char *);
};
/*
extern PtdQuestionDialogManager *theQuestionDialogManager;
*/
#endif
