                           
/*********************************************************************
**  NAME:  PtdButton.h 
**  Description:
**				all member function and variable for class PtdButton
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdButton.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:28
*********************************************************************/
#ifndef PTDBUTTONH
#define PTDBUTTONH

#include "PtdCommandInterface.h"

class PtdCommand;



#define TOGGLE_ON(BUTTON) \
    XmToggleButtonSetState((Widget) BUTTON, TRUE, FALSE)

#define TOGGLE_OFF(BUTTON) \
    XmToggleButtonSetState((Widget) BUTTON, FALSE, FALSE)

#define TOGGLE_STATE(BUTTON) \
	XmToggleButtonGetState((Widget)BUTTON)
	
class PtdButton : public PtdCommandInterface 
{
	protected:
	
	public:
    
	PtdButton(Widget,PtdCommand *,PtdCmdInterfaceStruct *butDetail);

};
#endif
