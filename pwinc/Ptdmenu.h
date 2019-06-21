/*********************************************************************
**  NAME:  Ptdmenu.h
**  Description:
**				functions for adjust PtdCmdInterfaceStruct
**				Not for Windows NT	
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       Ptdmenu.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:33
*********************************************************************/

#ifndef WNT
#ifndef PTDMENUH
#define PTDMENUH

extern int Ptd_insert_menu(int menu, int index, 
		PtdCmdInterfaceStruct *changeMenu, char *label);
extern int Ptd_remove_menu(int menu, int index, 
		PtdCmdInterfaceStruct *changeMenu);


#endif

#endif

