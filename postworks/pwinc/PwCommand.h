/* 
 * Class Name:          PwCommand 
 * 
 * Description:         Common command class for main frame
 * 
 * Notes/Warnings:      
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwCommand.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:36
c
 */

#ifndef suinoundocommand_h
#define suinoundocommand_h
#include "SUiNoUndoCommand.h"
#endif

class PwCommand : public SUiNoUndoCommand
{
	protected:
	
	SUiCommandCbStruct *m_pCbStruct;
	
	/*
	 * Function:	DoIt
	 *
	 * Description: Specific actions defined by this class
	 *
	 */
    virtual void DoIt();
	
	public:
	/*
	 * Function:	PwCommand
	 *
	 * Description: constructor
	 *
	 *
	 */
	PwCommand(char *, MsBoolean,SUiCommandCbStruct *);

	/*
	 * Function:	~PwCommand
	 *
	 * Description: destructor
	 *
	 */
	~PwCommand();
};
