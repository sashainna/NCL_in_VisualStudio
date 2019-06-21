/*********************************************************************
**  NAME:  PtdMenuBar.h
**  Description:
**				all member function and variable for class PtdMenuBar
**				A menu bar, with panes supporting PtdCommands
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdMenuBar.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:31
*********************************************************************/
#ifndef PTDMENUBAR
#define PTDMENUBAR

#include "PtdBase.h"
#include "PtdString.h"

class PtdCommand;
class PtdCommandList;

struct PtdCmdInterfaceStruct;

class PtdMenuBar : public PtdBase 
{

  public:
    
	/*
	 * Function:	PtdMenuBar
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
    PtdMenuBar (Widget,char *);

	/*
	 * Function:	AddCommands
	 *
	 * Description: Create a named menu pane from a list of Cmd objects
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
    virtual void AddCommands (PtdCommandList *,
						PtdCmdInterfaceStruct *ciStruct = NULL, 
						int count =0);

	/*
	 * Function:	CreatePulldown
	 *
	 * Description: Create a pull down from a command list
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning: 
	 *			
	 *
	 */
    virtual void CreatePulldown(Widget, PtdCommandList *,
								PtdCmdInterfaceStruct *,int);

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
	 */
    virtual const PtdString ClassName() { return m_sClassName; }
	virtual ~PtdMenuBar();
};
#endif
