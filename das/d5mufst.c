/*********************************************************************
**
**    NAME         :  d5mufst.c
**
**       CONTAINS:
**       	ud_mu_chk_init		
**				ud_mu_gks_init		
**				udi_mu_reset
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d5mufst.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:12
**
*********************************************************************/

#include "usysdef.h"
#include "usysg.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "dasnog.h"
#include "dmenucom.h"
#include "udebug.h"
#include "ustdio.h"
#include "uhep.h"
#include "gobas.h"

/*********************************************************************
**
**    I_FUNCTION     :  ud_mu_chk_init (mujsize,base,mubit)
**       Check whether a certain menu information need to be reinitialized
**
**    PARAMETERS   
**       INPUT  : 
**          mubit - address of the menu reload flag
**          base  - base number for the menu
**          mujsize - max menu number
**       OUTPUT :  
**          base  - new base number for the menu
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_mu_chk_init(mujsize, base, mubit)
int mubit;
int *base;						/* return base choice device number */
int mujsize;					/* size of this tree */
{
	int *muload_ptr;
	unsigned int mubitmask;
	int muindex = 0;
	int shift_bitcont = mubit;

	/* NCL: modified initialization checking to handle more that 32 subsystems */
	/* NCL: 5-18-89 roberta ; See also das/d5muint.c, inc/dmenucom.h, das/d5mudsp.c */
	/*      and /ncl502/sbin/nclmutool */
	if (shift_bitcont > 31)
		{
		muindex = 1;
		shift_bitcont = shift_bitcont - 32;
		}

	muload_ptr = &UDI_muload[muindex];
	mubitmask = 1 << shift_bitcont;

	if ((*muload_ptr & mubitmask) == 0)	/* menu has to be initialized */
	/* NCL: end of mod. */
	{
		*base = UDI_mu_base;

/*		-- test if we want to apply modulo algorithm to choice device
			allocation -- */

		if(UD_menu_mod == UU_TRUE)
		{
			UDI_mu_base = UDI_mu_base + UD_max_node;
			ud_test_over();
		}
		else
		{
			UDI_mu_base = UDI_mu_base + mujsize;
			ud_test_over();
		}

		/* NCL: modified initialization checking for more than 32 menus. */
		*muload_ptr = *muload_ptr | mubitmask;

		return(UU_TRUE);
	}
	else
		return(UU_FALSE);
}	/* ud_mu_chk_init */

/*********************************************************************
**
**    I_FUNCTION     :  ud_mu_gks_init(mujtb,mujsize,muftb,base)
**       call DAS to initialize the menu 
**
**    PARAMETERS   
**       INPUT  : 
**          mujtb - jump table of the current menu
**          mujsize - size of the jump table
**          muftb - array of the menu name
**          base - base number of the menu
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_mu_gks_init(mujtb, mujsize, muftb, base)
UDI_MUJTB mujtb[];				/* main jump table */
int mujsize;
int base;
char *muftb[];
{
	int i;
	char *p, *ind;
	Gdrect position;				/* position where menu is located */
	int status;						/* status cell */
	int mode;						/* state save cell */
								

 	uu_denter(UU_DTRC,(us, "enter gks_int, mujsize=%d, base=%d", 
					mujsize, base));

	for (i=0; i<mujsize; i++)
	{
		switch (UDI_nav_mode)
		{
	
	/*		-- text menus -- */
	
			case 1:
				ud_initmu(base, mujtb[i].menuno-1, &muftb[mujtb[i].menuind], 
								mujtb[i].muarea);
				break;
	
	/*		-- icon menus simultaneously active -- */
	
			case 2:
				ind = mujtb[i].iconnm;
				if (ind == NULL)
					ud_initmu(base, mujtb[i].menuno-1, &muftb[mujtb[i].menuind],
								mujtb[i].muarea);
				else
				{
					if( ud_initmu2(base, ind, &position) != 0)
					{
						ud_initmu(base, mujtb[i].menuno-1, &muftb[mujtb[i].menuind],
								mujtb[i].muarea);
						mujtb[i].iconnm = NULL;
					}
				}
				break;
		}
		base++;
	}
 	uu_dexit;
}	/* ud_mu_gks_init */

/*********************************************************************
**
**    I_FUNCTION     :  udi_mu_reset()
**       reset the base number of the menu  system and the 
**       initialization flag
**
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

udi_mu_reset()
{
 	uu_denter(UU_DTRC,(us, "enter udi_mu_reset" ));
	UDI_mu_base = UD_st_menu_num;
	UDI_pop_base = UD_st_popup_num;
/* NCL: UDI_muload enlarged to accomodate more than 32 menu subsystems */
	UDI_muload[0] = 0;
	UDI_muload[1] = 0;
	uu_dexit;
}	/* udi_mu_reset */

/*********************************************************************
**
**    I_FUNCTION     :  ud_test_over()
**       test if we have exceeded the number of choice devices
**
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

static ud_test_over()
{
	int cntbuf[6];				/* gqndev return buffer */
	int markval;				/* MARK cell */
	char us[100];

	gqndev(UD_ksws, cntbuf);
	if(UDI_mu_base > cntbuf[3])
	{

/*	-- system error recovery, exit system -- */

		uu_sys_err_recovery(1, UU_SIGNON, 13, 0, 0);
	}
}
