/*********************************************************************
**  NAME:  PtdUndoCommand.h
**  Description:
**				all member function and variable for class PtdUndoCommand
**				
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdUndoCommand.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:33
*********************************************************************/
#ifndef PTDUNDOCOMMANDH
#define PTDUNDOCOMMANDH

#include "PtdNoUndoCommand.h"

class PtdUndoCommand : public PtdNoUndoCommand {
    
  protected:

	/*
	 * Function:	DoIt
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
    virtual void DoIt();
    
  public:
    
	/*
	 * Function:	PtdUndoCommand
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
    PtdUndoCommand(char *);
};

extern PtdUndoCommand *theUndoCommand;
#endif
