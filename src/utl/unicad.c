/*********************************************************************
**
**    NAME         :  unicad.c
**
**       CONTAINS:
**				unicad_()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       unicad.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:54
**
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "dmark.h"
#include "udebug.h"
#include "mfcifdef.h"

extern int NCL_vx_flag;

/*********************************************************************
**
**    I_FUNCTION       :  unicad_()
**       main routine to UNICAD system
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void unicad_()
{
	int i;

/* -- Initialize unicad -- */

	 uu_init_unicad();

/*	-- initialize mark subsystem -- */

#if UU_COMP!=UU_WIN2K
	 UD_enablejmp = UU_TRUE;
	 UD_MARK(i, UU_TRUE);
#endif
/*	-- invoke the root subsystem -- */
	
	iuni();

/*	-- clean-up and get out -- */

	return;
}
