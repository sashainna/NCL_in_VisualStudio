/*********************************************************************
**    NAME:  nuunits.c
**       CONTAINS: NCL units
**    
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nuunits.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:17
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "modef.h"
#include "mdpick.h"
#include "mdebug.h"

#include "nccs.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "nclcmd.h"
#include "nclmodals.h"
#include "mdunits.h"
#include "nclfc.h"
#include "mfort.h"

extern UM_int2 NCL_ubas_unit;

/*********************************************************************
**    E_FUNCTION: nclu_units(inormm)
**       change units to inches or mm by passing units/.. to ncl
**    PARAMETERS   
**       INPUT  : 
**          inormm         unit type flag;   0=inches, 1=mm
**       OUTPUT :  
**          none
**    RETURNS: none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_units(inormm)
   int inormm;
 {
   NCL_cmdbuf cmdbuf;
   int status;
   UM_int2 ivl1, ivl2;

   uu_denter(UU_MTRC,(us,"nclu_units()"));

/*
...When in CADD do not submit ncl command line
   if (UU_application == UU_NCLCAM)
     {
*/
   ivl1=2;ivl2=2;
   setins(&ivl1, &ivl2);
   ncl_init_cmdbuf(&cmdbuf);

   if (inormm==0)
       {
        status = ncl_add_token(&cmdbuf, NCL_unitsin, NCL_nocomma);
       }
   else
       {
        status = ncl_add_token(&cmdbuf, NCL_unitsmm, NCL_nocomma);
       }        
   ncl_add_cmdbuf(&cmdbuf);
   ncl_call(&cmdbuf);
   ivl1=1;ivl2=0;
   setins(&ivl1, &ivl2);
/*   }
   else
     {
      if (inormm == 0) inches();
      else millim();
     }
*/
   uu_dexit;
 }
setin()
   {
   UM_int2 ifl, val;
   um_setunits(UM_INCH);

   /* update status area */
/*   Do not attempt to update graphical status area if running in batch. */
	ifl=35;
	getifl(&ifl, &val);
	if (!val) uz_actunits(UM_INCH);
   }
setmm()
   {
   UM_int2 ifl, val;
   um_setunits(UM_MM);

   /* update status area */
/*   Do not attempt to update graphical status area if running in batch. */
	ifl=35;
	getifl(&ifl, &val);
	if (!val) uz_actunits(UM_MM);
   }
/*********************************************************************
**    E_FUNCTION: unbuni()
**       Change units to inches or mm using unibase setting. 
**       Fortran callable routine.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS: none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
unbuni()
  {
   UM_int2 idx, val;
/*
...Check CAM if ifl(264) is different than unibase units
*/
   idx = 264;
   getifl (&idx,&val);
   if (val != NCL_ubas_unit)
    {
     if (NCL_ubas_unit == 0) inches();
     if (NCL_ubas_unit == 1) millim();
    }
  }
