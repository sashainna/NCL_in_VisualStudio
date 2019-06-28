/*********************************************************************
**    NAME         :  nuglobal.c
**       CONTAINS: define the global structures for NCL
**       ncli_dummy
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neglobal.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:35
*********************************************************************/

/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                             !!!!!!
!!!!!!   NOTE:                                                     !!!!!!
!!!!!!      Recompiling this routine will cause any changes        !!!!!!
!!!!!!      made to the include files NCLMODALS.H and NKEYWD.H to  !!!!!!
!!!!!!      be reflected in the executable.                        !!!!!!
!!!!!!                                                             !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

#define NCL_MPGM 1
#include "usysdef.h"
#include "xenv1.h"
#include "nccs.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "nclcmd.h"
#include "nclmodals.h"
#include "ngeom.h"
#undef NCL_MPGM

#include "gobas.h"
#define DEF
#include "tplot.h"
#undef DEF

/*********************************************************************
**    E_FUNCTION     :  int ncli_dummy()
**       Dummy routine to force this file to be loaded (for some real
**       stupid machines).
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncli_dummy()
   {
   }
