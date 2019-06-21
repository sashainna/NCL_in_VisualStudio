/*********************************************************************
**  NAME:  PtdNoUndoCommand.h
**  Description:
**				all member function and variable for class PtdNoUndoCommand
**				Base class for all commands without undo
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdNoUndoCommand.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:31
*********************************************************************/
#ifndef PTDNOUNDOCMDH
#define PTDNOUNDOCMDH

#include "PtdCommand.h"

class PtdNoUndoCommand : public PtdCommand 
{
    
	protected:

	/*
	 * Function:	UndoIt
	 *
	 * Description: virtual function defined here
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    virtual void UndoIt();
    
  public:
    
	/*
	 * Function:	PtdNoUndoCommand
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
    PtdNoUndoCommand(char *, int);
};
#endif
