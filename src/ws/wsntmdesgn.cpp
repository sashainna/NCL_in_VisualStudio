
/********************************************************************* 
**  NAME:  wsntmdesgn.cpp
**
**			C functions which used C++ class functions
**			it is the interface between C and C++
**	CONTAINS: 
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntmdesgn.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:28
*********************************************************************/
#include "wsntstdafx.h"
#include <conio.h>
#include <winspool.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include "wsntframe.h"
#include "wsgl.h"
#include "wsntarea.h"

extern CMainFrame *NCL_MainFrame;

/**********************************************************************
**    I_FUNCTION :  uw_edit_menuarea()
**       Add/Delete a menu area
**
**    PARAMETERS   
**       INPUT  : 
**          none
**		
**       OUTPUT :  
**          none
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_edit_menuarea()
{
	CAreaEditDialog areaDlg;
	areaDlg.DoModal();
}
