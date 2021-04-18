/********************************************************************* 
**  NAME:  wsntdum.c
**
**      GKS workstation: Dummy routines
**
**            CONTAINS:
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wsntdum.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:13
*********************************************************************/
#include "usysdef.h"
#include "view.h"
UV_view MSL_view[20];
//UV_view MSL_view[60];
#include <stdlib.h>
#if UU_COMP == UU_WIN2K
#include <string.h>
#include "nclfc.h"
 

int uw_758_longplot;
int uw_758fd_page;
float uw_758_maxxsize;
/*
.....following for menu design, called from
.....d7menu.c not implement yet
*/
uw_mfpmenu_menu()
{
}

uw_mfpmenu_desc()
{
}

uw_mfdsn_minc()
{
}

uw_mfpmenu_color()
{
}
uw_mfpmenu_toggle()
{}
uw_mfinit_toggle()
{}

char* index(char*string, char ic)
{
	return strchr(string, ic);
}

char* rindex(char*string, char ic)
{
	return strrchr(string, ic);
}

sleep(int n)
{
	_sleep(n);
}


uw_ntprint_screen()
{
}


#endif
