/*********************************************************************
**  NAME:  PtdMenuSeparator.h
**  Description:
**          all member function and variable for class PtdMenuSeparator
**          class to encapsulate a menu separator
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdMenuSeparator.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:31
*********************************************************************/

#ifndef PTDMENUSEPARATORH
#define PTDMENUSEPARATORH
#include "PtdCommand.h"

class PtdMenuSeparator : public PtdCommand
{
  protected:    
    virtual void DoIt();   
    virtual void UndoIt(); 
  public:
    PtdMenuSeparator( char *, int state = 1);
	~PtdMenuSeparator();
};

#endif
