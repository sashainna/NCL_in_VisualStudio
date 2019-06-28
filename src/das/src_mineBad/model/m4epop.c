/*********************************************************************
**    NAME         :  m4epop.c
**       CONTAINS: user interface routines to handle popup menus
**							for AG operations
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m4epop.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:03
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "dasg.h"
#include "mdebug.h"

#define UAG_NUMPOPUPMENU 2

static char *UAG_menu1[4] = {
		"EXTRACT SURFACE CURVE",
		"U curve",
		"V curve"};

static char *UAG_menu2[4] = {
		"FACE FEATURES",
		"curve",
		"surface"};

static UD_POPUPREC UAG_popup[2] = {
		 { 3, UAG_menu1, "", {.17, .127 }, 0, 0 },
		 { 3, UAG_menu2, "", {.17, .127 }, 0, 0 }
	    };

/*********************************************************************
**    E_FUNCTION     : int um_ag_popupmenu(menunumber, choice)
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
int um_ag_popupmenu(menunumber, choice)
	int	menunumber;
	int	*choice;

	{
	int outchoices;
	int status;

	uu_denter(UU_MTRC,(us,"um_ag_popupmenu(menu=%d)",menunumber));
	status = ud_ddas(UD_POPUP, &UAG_popup[menunumber], choice, 1,
					&outchoices, UD_NODEFAULT);
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : um_ag_initpopupmenus()
**       Initialize all of the pop up menus which interface to AG
**			creation techniques.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ag_initpopupmenus()

	{
	int i;

	uu_denter(UU_MTRC,(us,"um_ag_initpopupmenus()"));
	for(i=0; i<UAG_NUMPOPUPMENU; i++)
		ud_initpp(&UAG_popup[i]);
	uu_dexit;
	}
