/*********************************************************************
**
**    NAME         :  d5mudsp.c
**       CONTAINS:
**			ud_munavg
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d5mudsp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:11
**
*********************************************************************/
/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                             !!!!!!
!!!!!!   NOTE:                                                     !!!!!!
!!!!!!      This routine (d5mudsp.C) needs to be recompiled to     !!!!!!
!!!!!!      cause any changes made in the DMENUCOM.H file to       !!!!!!
!!!!!!      take effect.                                           !!!!!!
!!!!!!                                                             !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

#define MENUPGM 1
#include	"usysdef.h"
#include	"dinput.h"
#include	"dmenucom.h"
#include	"dmark.h"
#include	"ustdio.h"
#include	"udebug.h"

/*********************************************************************
**
**    E_FUNCTION     :  ud_munavg (mujtb,mustrtb,mustbsize,muftb,
**												mucases, mujsize,base,mubit, muflag)
**
**       menu navigator dispatcher
**
**    PARAMETERS   
**       INPUT  : 
**          mujtb - menu's jump table
**          mustrtb - menu's string table
**          mustbsize - number of strings in the menu
**          muftb - array of menu name
**          mucases - action routines for the leaves
**          mujsize - number of menu
**          base - base number for the current menu
**          mubit - menu's reload flag
**       OUTPUT :  
**          base - base nember for the current menu
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_munavg(mujtb, mustrtb, mustbsize, muftb, mucases, 
		  mujsize, base, mubit, muflag)
UDI_MUJTB mujtb[];		/* that holds state info for each node table */
UDI_MUSTR mustrtb[];		/* goto and call string table  */
int	mustbsize;			/* menu string table size */
char	*muftb[];			/* menu character string text array */
int	(*mucases)();		/* menu semantic action routines for leaves */
int	mujsize,				/* maximum menu number */
		*base,				/* base menu number */
		mubit,				/* current menu bit representation */
		muflag;				/* flag to decide which menu level the second reject 
									should go */
{

	int save_nav_mode;		/* save menu navigator switch */
	int markval;				/* mark variable */
	char *savetut;				/* save current tutorial */
	char us[100];

/*	-- if not initializing menus -- */

	if(UD_menu_init != 0)
	{

/*		-- set up for auto test -- */

		if(!(		UD_autotest==UU_TRUE && 
					UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK &&
					UDI_muwhich>0
			))
		{
			UD_MARK(markval, UU_FALSE);
			if(markval == 0)
			{

/*			-- take down previous icon menu if up -- */

				ud_icmustk_dn(UU_FALSE);
				save_nav_mode = UDI_nav_mode;
				savetut = UD_tut_ptr;

/*				-- dispatch to menu navigator -- */

				switch (UDI_nav_mode)
				{
			
/*					-- text menus -- */
			
					case 1:
						ud_munavg1(mujtb, mustrtb, mustbsize, muftb, mucases, 
					  			mujsize, base, mubit, muflag);
						break;
			
/*					-- icon menus simultaneously active -- */
			
					case 2:

						if(mujtb[0].iconnm == NULL)
						{
							ud_munavg1(mujtb, mustrtb, mustbsize, muftb, mucases, 
					  			mujsize, base, mubit, muflag);
						}
						else
						{
							ud_munavg2(mujtb, mustrtb, mustbsize, muftb, mucases, 
					  			mujsize, base, mubit, muflag);
						}

						break;

					default:
						uu_dprint(UU_DTRC,(us,"ud_munavg ERROR: nav_mode = %d",
							UDI_nav_mode));
						ud_munavg1(mujtb, mustrtb, mustbsize, muftb, mucases, 
					  			mujsize, base, mubit, muflag);
						break;
				}
			}

			UDI_nav_mode = save_nav_mode;
			UD_tut_ptr = savetut;

 			UD_UNMARK(markval);
			ud_icmustk_up();
		}
	}
}
