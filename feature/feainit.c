/*********************************************************************
**    NAME         :  feainit
**       CONTAINS:
**       feainit
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       feainit.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:45
*********************************************************************/

#define UM_FEAINIT
#include "usysdef.h"
#include "mfeatcom.h"
#include "mattr.h"
#include "mdebug.h"
#undef UM_FEAINIT

/*********************************************************************
**    E_FUNCTION     :  int um_feainit()
**      initialize the features system
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
um_feainit()

	{
	int i,vmin,vmax,cmin,cmax,rmin,rmax;

	for (i=0; i<UM_MAXFPICK; i++)
		UM_Features[i].used = 0;	/* entry is not in use */

   UM_F_vect_count = 0;
   UM_F_coord_count = 0;
   UM_F_real_count = 0;

	/* set the default number of decimal places for real features */
	UM_dispattr.fea_decpl = 4;

	vmin = UM_VECTMIN;
	vmax = UM_VECTMAX;
	cmin = UM_COORDMIN;
	cmax = UM_COORDMAX;
	rmin = UM_REALMIN;
	rmax = UM_REALMAX;
	sprintf(UM_sbuf,"feainit: vec=[%d,%d] coord=[%d,%d] real=[%d,%d]",
		vmin,vmax,cmin,cmax,rmin,rmax);
	um_pscroll(UM_sbuf);

	}
