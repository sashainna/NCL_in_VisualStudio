/*********************************************************************
**  NAME:  PtdCommand.h  
**  Description:
**				all member function and variable for class PtdCommand
**				A base class for all command objects
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdCommand.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:28
*********************************************************************/
#ifndef PTDCOMMANDH
#define PTDCOMMANDH

#include "PtdCommandBase.h"
#include "PtdCommandIfArray.h"
class PtdCommandList;
class PtdCommandInterface;


struct PtdCommandCbStruct{
	void *cPtr;
	int	id;
};

class PtdCommand : public PtdCommandBase
{
    
    friend PtdCommandInterface;
    
  private:
    
    // Lists of other commands to be activated or deactivated
    
    PtdCommandList	*m_pActivationList;
    PtdCommandList	*m_pDeactivationList;
    int		m_bActive;     // Is this command currently active?
    int		m_bPreviouslyActive; // Previous value of m_bActive
	
    void  Revert();

  protected:
	PtdCommandCbStruct *m_pCbStruct;
    PtdCommandInterfaceArray m_CommandIfArray;            
    void			*m_pCallData;

    virtual void DoIt();
    
    virtual void DeactivateInterface();
	
  public:

	PtdCommand(char *, int, PtdCommandCbStruct *cb = NULL);

	virtual ~PtdCommand ();
    
	virtual void Execute(void *callData = NULL);  
    
	void    Activate();
	void    Deactivate();
	void Set_ToggleOff();
	void Set_ToggleOn();
    
	void    AddToActivationList(PtdCommand *);
	void    AddToDeactivationList(PtdCommand *);
    
	void    RegisterInterface(PtdCommandInterface *);
    
	int Active() { return m_bActive; }
	void SetState(int state) { m_bActive = state; }
};
#endif
