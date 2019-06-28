/*********************************************************************
**    NAME         :  m2upop.c
**       CONTAINS: user interface routines to handle popup menus
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m2epop.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:46
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "dasnog.h"
#include "dasg.h"
#include "uhep.h"
#include "ustdio.h"
#include "mdebug.h"
#include "mpopmenu.h"

#define UM_NUMPOPUPMENU 10

static char *UM_menu1[5] = {
		"SET VIEW NORMAL",
		"View from X",
		"View from Y",
		"View from Z",
		"Specify normal"};

static char *UM_menu2[4] = {
		"ALIGN ALONG A VEC",
		"Change x-axis",
		"Change y-axis",
		"Change z-axis"};

static char *UM_menu3[4] = {
		"SNAP TO A LINE",
		"Snap x-axis",
		"Snap y-axis",
		"Snap z-axis"};

static char *UM_menu4[4] = {
		"ROTATE ABOUT VEC",
		"Rotate about x",
		"Rotate about y",
		"Rotate about z"};

static char *UM_menu5[5] = {
		"MODIFY LINE",
		"Move end point",
		"Extend by distance",
		"Extend to point projection"};

static char *UM_menu6[8] = {
		"MODIFY CIRCLE/ARC",
		"Move End point",
		"Angle",
		"Radius",
		"Break",
		"Close",
		"Move center",
		"Complement"};

static char *UM_menu7[5] = {
		"MODIFY ELLIPSE",
		"Major/minor axis length",
		"Close"};

static char *UM_menu8[3] = {
		"MODIFY POLYGON",
		"Move vertex",
		"Change fill color"};

static char *UM_menu9[4] = {
		"DEFINE SHAPE",
		"N sided",
		"Rectangle",
		"Right angle"};

static char *UM_menu10[7] = {
		"CREATE ELLIPSE",
		"Center, axes lengths",
		"Foci, point on curve",
		"Center, endpoint, length",
		"With start and end angles",
		"Projected circle",
		"Inside box"};

static UD_POPUPREC UM_popup[10] = {
		 { 5, UM_menu1, "", {.17, .127 }, 0, 0 },
		 { 4, UM_menu2, "", {.17, .11 }, 0, 0 },
		 { 4, UM_menu3, "", {.17, .11 }, 0, 0 },
		 { 4, UM_menu4, "", {.17, .11 }, 0, 0 },
		 { 4, UM_menu5, "", {.17, .127 }, 0, 0 },
		 { 8, UM_menu6, "", {.17, .11 }, 0, 0 },
		 { 3, UM_menu7, "", {.17, .127 }, 0, 0 },
		 { 3, UM_menu8, "", {.17, .127 }, 0, 0 },
		 { 4, UM_menu9, "", {.17, .11 }, 0, 0 },
		 { 7, UM_menu10, "", {.17, .11 }, 0, 0 }
	    };

/*********************************************************************
**    E_FUNCTION     : int um_popupmenu(menunumber, choice)
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
int um_popupmenu(menunumber, choice)
	int	menunumber;
	int	*choice;

	{
	int outchoices;
	int status;

	uu_denter(UU_MTRC,(us,"um_popupmenu(menu=%d)",menunumber));
	status = ud_ddas(UD_POPUP, &UM_popup[menunumber], choice, 1,
					&outchoices, UD_NODEFAULT);
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : um_initpopupmenus()
**       Initialize all of the modeling pop up menus.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_initpopupmenus()

	{
	int i;

	uu_denter(UU_MTRC,(us,"um_initpopupmenus()"));
#if UU_COMP==UU_VAXVMS
/*   Mills Data Systems - M. Gump - commented out to defeat loading pop-up menus at
     initialization time.
*/
	for(i=0; i<UM_NUMPOPUPMENU; i++)
		ud_initpp(&UM_popup[i]);
#endif
	uu_dexit;
	}
