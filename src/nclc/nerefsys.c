/*********************************************************************
**    NAME         :  nerefsys.c
**       CONTAINS:  routines to handle NCL refsys and UNICAD wp.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**         int stwpmx (mx)
**     MODULE NAME AND RELEASE LEVEL 
**       nerefsys.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:45
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "mdebug.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     :stwpmx((mx)
**         Set the current working plane.
**    PARAMETERS   
**       INPUT  : 
**            mx        - matrix with which to set wp.
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
stwpmx (mx)
   UM_real8 mx[12];

   {
   int i, j;
   UM_coord origin;
   UM_vector xaxis;
   UM_vector yaxis;
   UM_vector zaxis;
   UM_int2 ifl, ifl35=35;

   uu_denter(UU_MTRC,(us,"stwpmx()"));

   for (i=0;i<3;i++)
     {
     j = i*4;
     xaxis[i] = mx[j];
     yaxis[i] = mx[j+1];
     zaxis[i] = mx[j+2];
     origin[i] = mx[j+3];
     }

   um_setcpln(origin, xaxis, yaxis, zaxis);

/*
.....Do not draw cpl axis  in batch
*/
	getifl(&ifl35,&ifl);
	if (ifl == 0)
		um_drw_cpl_axis(UU_FALSE, UU_TRUE);

  uu_dexit;
  }
