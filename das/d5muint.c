/*********************************************************************
**
**    NAME         :  dmuinit.c
**       CONTAINS:
**       	ud_menuinit
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**      d5muint.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**      04/29/15 , 15:05:12
**
*********************************************************************/
/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                             !!!!!!
!!!!!!   NOTE:                                                     !!!!!!
!!!!!!      This routine (D5MUINT.C) needs to be recompiled to     !!!!!!
!!!!!!      cause any changes made in the DASNOG.H file to the     !!!!!!
!!!!!!      variables:                                             !!!!!!
!!!!!!        UD_MENU_CHC_DEV_ST                                   !!!!!!
!!!!!!        UD_st_menu_num                                       !!!!!!
!!!!!!        UD_POP_CHC_DEV_ST                                    !!!!!!
!!!!!!        UD_st_popup_num                                      !!!!!!
!!!!!!      to be reflected in the executable.                     !!!!!!
!!!!!!                                                             !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

#include		"usysdef.h"
#include		"dmenucom.h"
#include		"dasnog.h"
#include 	"xenv1.h"		/* used by "ux" calls: UX_PRTERRS value */

/*********************************************************************
**
**    E_FUNCTION     :  ud_menuinit()
**       Initialization routine for menu navigator
**
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_menuinit ()
{
	char *p, *ux_getenv();

	p = ux_getenv("UU_MENUUI",UX_PRTERRS);
	if (p == UU_NULL)
		UDI_nav_mode = 1;
	else if((ud_strcomp(p, "text") == -1) || (ud_strcomp(p, "TEXT") == -1))
		UDI_nav_mode = 1;
	else if((ud_strcomp(p, "icon") == -1) || (ud_strcomp(p, "ICON") == -1))
		UDI_nav_mode = 2;
	else
		UDI_nav_mode = 1;

	UDI_Muex_ptr = -1;
	UDI_mu_base = UD_st_menu_num;
	UDI_pop_base = UD_st_popup_num;
	UDI_bitcont = 0;
	UDI_muload[0] = 0;
	UDI_muload[1] = 0;
}	/* ud_menuinit */
