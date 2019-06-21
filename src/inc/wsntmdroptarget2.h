/********************************************************************* 
**  NAME:  wsntmdroptarget2.h
**
**  Description - Functions and struct declarations for
**              CNCLDropTarget2 class (Class that inherits from IDropTarget)
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**     wsntmdroptarget2.h , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**     04/29/15 , 15:07:20
*********************************************************************/
#ifndef WSNTMDROPTARGET2_H
#define WSNTMDROPTARGET2_H

#include "wsntdroptarget.h"

class CNCLMnDropTarget2 : public CNCLDropTarget
{
public:
	CNCLMnDropTarget2();

	void	GotDrop(void);
	DWORD	GotDrag(void);
	void	GotLeave(void);
	DWORD	GotEnter(void);
};


#endif
