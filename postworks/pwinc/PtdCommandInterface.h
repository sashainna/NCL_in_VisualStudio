/*********************************************************************
**  NAME:  PtdCommandInterface.h 
**  Description:
**				all member function and variable for class PtdCommandInterface.h
**				Interface class to handle commands
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdCommandInterface.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:29
*********************************************************************/
#ifndef PTDCOMMANDINTERFACEH
#define PTDCOMMANDINTERFACEH
                           
#include "PtdBase.h"

enum PtdECmdInterfaceType
{
	PtdEPushButton, 
	PtdERadioButton, 
	PtdEToggleButton,
	PtdECascadeButton,  
	PtdEDrawButton, 
	PtdESeparator 
};

struct PtdCmdInterfaceStruct;

struct PtdCmdInterfaceStruct 
{
    char    *name;
    PtdECmdInterfaceType  type;
    char	*accelerator;
    char	*accelText;
	PtdCmdInterfaceStruct *ciStruct;
	int subMenuCount;
};

class PtdCommand;

class PtdCommandInterface : public PtdBase 
{
    friend PtdCommand;
    
	protected:
    
    PtdCommand	*m_pCommand;
    int	m_bActive;
	PtdECmdInterfaceType m_eInterfaceType;
	
	/*
	 * Function:	ExecuteCommandCb
	 *
	 * Description: command callback
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    static void ExecuteCommandCb(Widget, 
				    XtPointer, 
				    XtPointer );
    
	/*
	 * Function:	PtdCommandInterface
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
    PtdCommandInterface(PtdCommand * );
    
	/*
	 * Function:	Activate
	 *
	 * Description: activate the command
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    virtual void Activate();

	/*
	 * Function:	DeActivate
	 *
	 * Description: deactivate the command
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    virtual void DeActivate();
	void SetToggle_On();
	void SetToggle_Off();

	public:
	
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
	 
	 PtdECmdInterfaceType Type() { return m_eInterfaceType;}
	 void SetType(PtdECmdInterfaceType type) { m_eInterfaceType = type;} 
};
#endif
