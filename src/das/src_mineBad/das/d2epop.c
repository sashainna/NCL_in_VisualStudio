/*********************************************************************
**    NAME         :  d2epop.c
**       CONTAINS: user interface routines to handle popup menus
**    		int ud_popupmenu(menunumber, choice)
**    		ud_initpopupmenus()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d2epop.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:04
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "dasnog.h"
#include "dasg.h"
#include "uhep.h"
#include "ustdio.h"
#include "mdebug.h"
#include "mpopmenu.h"

#define UD_NUMPOPUPMENU 1

/*
....Extend popup menu choices to include entry for picking an NCL plane - RAZ
*/
static char *UD_menu1[6] = {
	 	"DEFINE PLANE",
		"Pick Plane",
		"Pick entities",
		"Input origin/normal",
		"Origin, pick entities ",
		"Use Default"
	};

static UD_POPUPREC UD_popup[1] = {
		 {6, UD_menu1, "", {.17, .127 }, 0, 0 }
};

/*********************************************************************
**    E_FUNCTION     : int ud_popupmenu(menunumber, choice)
**       Call DAS to display popup menu MENUNUMBER and return the
**			CHOICE that the user made.
**    PARAMETERS   
**       INPUT  : 
**          menunumber				number of menu to display
**       OUTPUT :  
**          choice					menu item indicated
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ud_popupmenu(menunumber, choice)
int	menunumber;
int	*choice;
{
	int outchoices;
	int status;

	uu_denter(UU_DTRC,(us,"ud_popupmenu(menu=%d)",menunumber));

	status = ud_ddas(UD_POPUP, &UD_popup[menunumber], choice, 1,
					&outchoices, UD_NODEFAULT);

	uu_dprint(UU_DTRC,(us,"exit ud_popupmenu, status=%d, choice=%d",
			status, *choice));
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ud_initpopupmenus()
**       Initialize all of the DAS pop up menus.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ud_initpopupmenus()
{
	int i;

	uu_denter(UU_DTRC,(us,"ud_initpopupmenus(), num=%d",
		UD_NUMPOPUPMENU));
	for(i=0; i<UD_NUMPOPUPMENU; i++)
		ud_initpp(&UD_popup[i]);

	uu_dprint(UU_DTRC,(us,"leave ud_initpopupmenus()"));
	uu_dexit;
}
