/*********************************************************************
**  NAME:  PtdCommandBase.h 
**  Description:
**				all member function and variable for class PtdCommandBase
**				A base class for handling commands
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdCommandBase.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:29
*********************************************************************/

#ifndef PTDCOMMANDBASEH
#define PTDCOMMANDBASEH

#include "PtdString.h"
#include <stdlib.h>
#ifdef IBM
#include <string.h>
#else
#include <strings.h>
#endif

enum PtdECommandType {
	PtdERegularCommand, 
	PtdEHelpCommand
};


class PtdCommandBase
{
	protected:
	
	PtdString		m_sClassName;
	PtdString 	m_sCommandName;
	PtdECommandType	m_eCommandType;
	
	PtdCommandBase(char *name)
	{
		m_sCommandName = name;
		m_sClassName = "PtdCommandBase";
		m_eCommandType = PtdERegularCommand;
	}
	
	public:
	
		virtual PtdString  CommandName() { return m_sCommandName; }
		virtual const PtdString  ClassName() { return m_sClassName;}
		PtdECommandType CommandType() { return m_eCommandType;}
		void SetCommandType(PtdECommandType type)
			{	m_eCommandType = type;}
};
#endif
