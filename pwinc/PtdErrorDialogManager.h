/*********************************************************************
**  NAME:  PtdErrorDialogManager.h 
**  Description:
**				all member function and variable for class PtdErrorDialogManager
**				dialog manager for error pop-up dialogs
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdErrorDialogManager.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:29
*********************************************************************/
#ifndef PTDERRORDIALOGMGRH
#define PTDERRORDIALOGMGRH

#include "PtdMessageDialogManager.h"


class PtdErrorDialogManager : public PtdMessageDialogManager 
{
    
  protected:
    
    Widget CreateDialog(Widget);
    
  public:
    
    PtdErrorDialogManager(Widget, char *);
};
#endif
