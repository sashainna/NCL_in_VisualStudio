/*********************************************************************
**
**    NAME         :  d2popup
**
**       CONTAINS:
**       	ud_initpp		
**				ud_popup		
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d2popup.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:05
**
*********************************************************************/

#include	"usysdef.h"
#include	"dasnog.h"
#include	"dasg.h"
#include	"dinput.h"
#include	"dmenucom.h"
#include	"udebug.h"
#include "ustdio.h"
#include "uims.h"
#include "gobas.h"
#include "uhep.h"
int UD_popup = 0;
/*********************************************************************
**
**    E_FUNCTION     :  ud_initpp(poprec)
**       Initialize a pop-up menu
**
**    PARAMETERS   
**       INPUT  : 
**          poprec = popup menu control block
**
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_initpp(poprec)
UD_POPUPREC *poprec;						/* pointer to popup menu structure */
{

	char *ind;
	Gdrect placed;					/* where icon menu was placed */

 	uu_denter(UU_DTRC,(us, "enter ud_initpp"));

/*	-- initialization -- */

/*	-- use next free choice device number -- */

	(*poprec).devnum = UDI_pop_base;
	if(UDI_pop_base >= UD_st_menu_num)
	{

/*		-- system error recovery, exit system -- */

		uu_sys_err_recovery(1, UU_SIGNON, 14, 0, 0);
	}
	else
	{
		(*poprec).popet = UD_chcech + 30;
		 uu_dprint(UU_DTRC,(us, "ud_initpp: devnum = %d, popbase = %d",
			(*poprec).devnum, UDI_pop_base));

		switch (UDI_nav_mode)
		{
			
/*		-- text menus -- */
			
			case 1:
				ud_initmu3(poprec);
				break;
			
			/*		-- icon menus simultaneously active -- */
			
			case 2:
				ind = (*poprec).iconfile;		/* Pointer to icon menu file name	*/
				if (ind[0] == '\0')
				{

/*				-- no icon file supplied -- */

					ud_initmu3(poprec);
				}
				else
				{
					if(ud_initmu2(UDI_pop_base, ind, &placed) != 0)
					{
/*					-- icon file not found -- */

						ud_initmu3(poprec);
						(*poprec).iconfile[0] = '\0';
					}
					else
					{
						(*poprec).popet = 35;
						(*poprec).menusz.x = placed.ur.x - placed.ll.x;
						(*poprec).menusz.y = placed.ur.y - placed.ll.y;
					}
				}
				break;
		}
		UDI_pop_base++;

		uu_denter2(UU_DTRC,(us, " in ud_initpp, file=%s, ext=(%g %g) dev=%d, pet=%d, num=%d, title=%s",
				(*poprec).iconfile, (*poprec).menusz.x, (*poprec).menusz.y,
				(*poprec).devnum, (*poprec).popet, (*poprec).numchoice,
				(*poprec).text[0] ));
		uu_dexit;

		uu_dexit;
	}
}	/* ud_initpp */

/*********************************************************************
**
**    E_FUNCTION     :  ud_popup(postr, choice)
**       bring up a popup menu and return the choice
**
**    PARAMETERS   
**       INPUT  : 
**          poprec = popup menu control block
**       OUTPUT :  
**          choice made
**
**    RETURNS      : status
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

UD_DASTAT ud_popup(poprec, choice)
UD_POPUPREC *poprec;						/* pointer to popup menu structure */
int *choice;								/* choice number to return */
{

/*		--- local variables ---    */

	UD_DASTAT status;						/* temp status variable */
	UD_DASTAT ud_chc1();					/* semantic interpreter */
	UD_DEVENT event;	/* event buffer */
	int jmpflag;

/*
**			--- start of executable code ---
*/

	uu_denter(UU_DTRC,(us,"entering ud_popup"));

	UD_popup = 1;
	*choice = -1;
	UD_MARK(jmpflag, UU_FALSE);
	if (jmpflag == 0)
	{
		if((*poprec).devnum == 0)
		{
			ud_initpp(poprec);
		}

/*	-- main loop -- */

		do
		{
			ud_gevt(&event, UD_chcint, poprec, (*poprec).numchoice, 
						(*poprec).devnum, (*poprec).popet, NULL);
			status = ud_chc1(&event, choice);

			/* MILLS: implement numeric nav of popup's - 
			   similar to numeric menu nav ... */
			if ((*choice >= 49) && (*choice <= 57))
				*choice = *choice - 48;
			else if (*choice == 48) 
				*choice = 10;

		}
		while(status == DE_AGAIN);
	}
	UD_popup = 0;
	UD_UNMARK(jmpflag);
	uu_dexit;
	return(status);
}

int ud_ifpopup()
{
	return UD_popup;
}
