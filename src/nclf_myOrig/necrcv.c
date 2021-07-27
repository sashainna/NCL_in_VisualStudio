/*********************************************************************
**    NAME         :  necrcv.c
**       CONTAINS:
**       ncl_curve_pts
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       necrcv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:27
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
/* #include "mdclass.h" */
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mattr.h"
#include "dmark.h"
#include "nkeywd.h"
#include "nclcmd.h"

/* #define UA_NCL 15 */
/*********************************************************************
**    E_FUNCTION     : ncl_curve_pts(fit)
**       Create an NCL curve entity via picked points.
**    PARAMETERS   
**       INPUT  : 
**          fit                  logical ( TRUE fit ; FALSE no fit)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_curve_pts(fit)
   UU_LOGICAL fit;
   {
   struct UM_point_rec ptr;    
   UM_PLOCREC pick;           /* pick record */
   int numint;                /* number of das entries returned */
   int status;
   int count;
   char str[NCL_MAX_COMLINE];

   uu_denter( UU_MTRC,(us,"ncl_curve_pts()"));

   ncl_init_cmdbuf();
   strcpy(str, NCL_cv);
   if(fit) strcat(str, NCL_fit);
   count = 0;
   while (UU_TRUE)
      {
      um_dl_pldas(UD_DASPCKLOC, /*pick point*/UA_NCL, 2, &pick, 
                  1, &numint, 2);
      if (numint <= 0) goto done;
      ptr.key = um_get_pickkey(&pick.pent, 2);
      ur_retrieve_data_relnum(ptr.key, &ptr.rel_num);
      if (ptr.rel_num  !=  NCL_POINT_REL)
         uu_uerror0(/*you must pick a point*/UA_NCL,2);
      else
         {
         um_get_all_geom(&ptr, sizeof(ptr));
         ncl_add_token(str, ptr.label);
         if(strlen(str) > 60 )
            {
            strcat(str,"$");
            ncl_add_cmdbuf(str);
            str[0] = '\0';
            }
         count++;
         }
      }
done:;
   if(count > 0)
      {
      count = strlen(str);
      str[count-1] = '\0';
      status = ncl_edit_line(str);
      ncl_add_cmdbuf(str);
      ncl_call();
      }
   uu_dexit;
   }
