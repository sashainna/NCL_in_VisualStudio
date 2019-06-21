/*********************************************************************
**    NAME         :  nurevers.c
**       CONTAINS: User interface routines for redefinition statements creation.
**       nclu_revers_geom()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nurevers.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:14
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "modef.h"

#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION     : nclu_revers_geom()
**       generate revers geometry statements
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

nclu_revers_geom()

   {
   NCL_cmdbuf cmdbuf;
   int status;
   int defining_rel;
   int target_rel;

   uu_denter(UU_MTRC,(us,"nclu_reverse_geom()"));

   while (UU_TRUE)
      {
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;

      status = ncl_add_name(&cmdbuf, 1);
		
	  if (status == NCL_DONE) status = NCL_OKINPUT;

/*       add revers keyword to command buffer */
      if (status == NCL_OKINPUT)
         status = ncl_add_token(&cmdbuf, NCL_revers, NCL_nocomma);

      if (status == NCL_OKINPUT)
         status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 242, UD_ncl_revers, 
         &target_rel);

      if (status == NCL_OKINPUT)
		 {
         status = ncl_add_modifier(&cmdbuf, NCL_XYZ_DIRECTION);
		 if (status == NCL_NOINPUT) status=NCL_OKINPUT;
		 }

      if ((status == NCL_OKINPUT) || (status == NCL_DONE))
         {
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
         }
      }

   uu_dexit;
   }
