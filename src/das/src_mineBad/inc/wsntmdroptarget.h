/********************************************************************* 
**  NAME:  wsntmdroptarget.h
**
**  Description - Functions and struct declarations for
**              CNCLDropTarget class (Class that inherits from IDropTarget)
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**     wsntmdroptarget.h , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**     04/29/15 , 15:07:20
*********************************************************************/
#ifndef WSNTMDROPTARGET_H
#define WSNTMDROPTARGET_H

#include "wsntdroptarget.h"

class CNCLMnDropTarget : public CNCLDropTarget
{
public:
	CNCLMnDropTarget();

	void	GotDrop(void);
	DWORD	GotDrag(void);
	void	GotLeave(void);
	DWORD	GotEnter(void);
};


#endif
