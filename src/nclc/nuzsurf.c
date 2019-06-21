

/*********************************************************************
**    NAME         :  nuzsurf.c
**       CONTAINS: User interface routines for zsurf manipulation.
**       nclu_zsurf()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuzsurf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:18
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
#include "nkeywd.h"
#include "nclinp.h"
#include "nclcmd.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION     : nclu_zsurf()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_zsurf()

   {
   NCL_cmdbuf cmdbuf;
   int status;
   int choice;

   uu_denter(UU_MTRC,(us,"nclu_zsurf()"));

   ncl_init_cmdbuf(&cmdbuf);

   ncl_add_token(&cmdbuf, NCL_zsurf, NCL_nocomma);

   status = ncl_popup(NCL_ZSURF, &choice);

   if (status == NCL_OKINPUT) switch (choice)
      {
      case 1:
         status = ncl_add_length(&cmdbuf, 179);
         break;
      case 2:
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 180, UD_ncl_pl);
         break;
      case 3:
         status = ncl_add_token(&cmdbuf, NCL_nomore, NCL_nocomma);
         break;
      default:
         status = NCL_NOINPUT;
         break;
      }

   if ((status == NCL_OKINPUT) || (status == NCL_DONE))
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }

   uu_dexit;
   }

