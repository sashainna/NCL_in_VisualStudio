
/*********************************************************************
**    NAME         :  m2init.c
**       CONTAINS: routines to control the initialization of modeling
**       um_init_model
**			um_init_romulus
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m2init.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:47
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "usysg.h"
#include "mattr.h"
#include "mromcom.h"
#include "mfcifdef.h"

/*********************************************************************
**    E_FUNCTION     :  int um_init_model()
**       Initialize the modeling subsystem.
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_init_model()
	{

/*	um_open_psfile();			   open debugging file "psfile" */
	um_init_cpln();			/* init basic contruction plane */
	um_ini_dispattr();		/*	init display attributes		*/
	um_feainit();				/*	init features subsystem		*/
/*	um_cplinit();				  	init unibase construction plane		*/
	um_layerinit();			/* init layer tuple	*/
	ncl_init_color();
	um_idrwmodals();			/*	init drawing modals			*/
/*	um_modaxisinit();			  	init model axis				*/
	um_reninit();				/* init rendering package		*/
/*
.....for WinNT, initial draw after
.....graphic view setup, the layout file
.....only have size for window (include menu area, status area)
.....Yurong 9/12/00
*/
#if UU_COMP!=UU_WIN2K
	um_drawinit();				/* init drawing subsystem		*/
#endif
	ag_init();					/* init Applied Geometry		*/
	/*um_initpopupmenus();		 init pop up menus; done in uappinit.c */

	}

/*********************************************************************
**    E_FUNCTION     :  int um_init_romulus()
**       Initialize the ROMULUS subsystem.
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_init_romulus()
	{
	int i;

	ur_put_dispattr_hidn_lines(UU_FALSE);	/*	set hidden lines to false	*/
	roinit();										/* ROMULUS fortran init routine	*/
	UM_NUM_OF_SOLIDS = 0;

	for (i=0; i<UM_MAX_HIDDEN_XFORMS; i++)
		{
		UM_hidden.key[i] = -1;
		UM_hidden.xform[i] = -1;
		UM_hidden.seg[i] = -1;
		}
	}
