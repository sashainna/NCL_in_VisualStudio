/*********************************************************************
**    NAME         :  necrsf.c
**       CONTAINS:
**       ncl_surf_ruled
**       ncl_surf_regular
**       ncl_surf_regular_zero
**       ncl_surf_fit
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       necrsf.c , 25.1
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
#include "class.h"
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mattr.h"
#include "dmark.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclcmd.h"
#include "lcom.h"

/*********************************************************************
**    E_FUNCTION     : ncl_surf_ruled()
**       Create an NCL ruled surface.
**    PARAMETERS   
**       INPUT  : 
**          none        
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_surf_ruled()
   {
   struct UC_entitydatabag ptr;    
   struct UM_point_rec *e;    
   UM_PLOCREC pick;           /* pick record */
   int numint;                /* number of das entries returned */
   int status;
   int count;
   char str[NCL_MAX_COMLINE];

   uu_denter( UU_MTRC,(us,"ncl_surf_ruled()"));

   ncl_init_cmdbuf();
   str[0] = '\0';
   strcpy(str, NCL_sf);
   count = 0;
   e = (struct UM_point_rec*)&ptr;
   while (count < 2)
      {
      um_dl_pldas(UD_DASPCKLOC, /*pick curve*/UA_NCL, 3, &pick, 
                  1, &numint, 2);
      if (numint <= 0) goto done;
      ptr.key = um_get_pickkey(&pick.pent, 2);
      ur_retrieve_data_fixed(&ptr);
      ncl_add_token(str, e->label, NCL_comma);
      count++;
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
/*********************************************************************
**    E_FUNCTION     : ncl_surf_regular()
**       Create an NCL regular surface.
**    PARAMETERS   
**       INPUT  : 
**          none        
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_surf_regular()
   {
   struct UC_entitydatabag ptr;    
   struct UM_point_rec *e;    
   UM_PLOCREC pick;           /* pick record */
   int numint;                /* number of das entries returned */
   int status;
   int count;
   char str[NCL_MAX_COMLINE];

   uu_denter( UU_MTRC,(us,"ncl_surf_regular()"));

   ncl_init_cmdbuf();
   str[0] = '\0';
   strcpy(str, NCL_sf);
   count = 0;
   e = (struct UM_point_rec*)&ptr;
   while (UU_TRUE)
      {
      um_dl_pldas(UD_DASPCKLOC, /*pick curve*/UA_NCL, 3, &pick, 
                  1, &numint, 2);
      if (numint <= 0) goto done;
      ptr.key = um_get_pickkey(&pick.pent, 2);
      ur_retrieve_data_fixed(&ptr);
      ncl_add_token(str, e->label, NCL_comma);
/*
.....don't use NCL_MAX_COMLINE
.....but use actual coomand line length allowed (UL_line_len)
*/
/*		if(strlen(str) > NCL_MAX_COMLINE-12)  */
		if(strlen(str) >= UL_line_len-1)
         {
         strcat(str,"$");
         ncl_add_cmdbuf(str);
         str[0] = '\0';
         }
      um_dl_pldas(UD_DASPCKLOC, /*pick slope*/UA_NCL, 4, &pick, 
                  1, &numint, 2);
      if(numint <= 0)
         {
         strcat(str,"0,");
         }
      else
         {
         ptr.key = um_get_pickkey(&pick.pent, 2);
         ur_retrieve_data_fixed(&ptr);
         ncl_add_token(str, e->label, NCL_comma);
         }
/*
.....don't use NCL_MAX_COMLINE
.....but use actual coomand line length allowed (UL_line_len)
*/
/*		if(strlen(str) > NCL_MAX_COMLINE-12)  */
		if(strlen(str) >= UL_line_len-1)
         {
         strcat(str,"$");
         ncl_add_cmdbuf(str);
         str[0] = '\0';
         }
      count++;
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
/*********************************************************************
**    E_FUNCTION     : ncl_surf_regular_zero()
**       Create an NCL regular surface with zero slopes.
**    PARAMETERS   
**       INPUT  : 
**          none        
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_surf_regular_zero()
   {
   struct UC_entitydatabag ptr;    
   struct UM_point_rec *e;    
   UM_PLOCREC pick;           /* pick record */
   int numint;                /* number of das entries returned */
   int status;
   int count;
   char str[NCL_MAX_COMLINE];

   uu_denter( UU_MTRC,(us,"ncl_surf_regular_zero()"));

   ncl_init_cmdbuf();
   str[0] = '\0';
   strcpy(str, NCL_sf);
   count = 0;
   e = (struct UM_point_rec*)&ptr;
   while (UU_TRUE)
      {
      um_dl_pldas(UD_DASPCKLOC, /*pick curve*/UA_NCL, 3, &pick, 
                  1, &numint, 2);
      if (numint <= 0) goto done;
      ptr.key = um_get_pickkey(&pick.pent, 2);
      ur_retrieve_data_fixed(&ptr);
      ncl_add_token(str, e->label, NCL_comma);
      strcat(str,"0,");
      if(strlen(str) > 60 )
         {
         strcat(str,"$");
         ncl_add_cmdbuf(str);
         str[0] = '\0';
         }
      count++;
      }
done:;
   if(count > 0)
      {
      count = strlen(str);
      str[count] = '\0';
      status = ncl_edit_line(str);
      ncl_add_cmdbuf(str);
      ncl_call();
      }
   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : ncl_surf_fit()
**       Create an NCL fit surface.
**    PARAMETERS   
**       INPUT  : 
**          none        
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_surf_fit()
   {
   struct UC_entitydatabag ptr;    
   struct UM_point_rec *e;    
   UM_PLOCREC pick;           /* pick record */
   int numint;                /* number of das entries returned */
   int status;
   int count;
   char str[NCL_MAX_COMLINE];

   uu_denter( UU_MTRC,(us,"ncl_surf_fit()"));

   ncl_init_cmdbuf();
   str[0] = '\0';
   strcpy(str, NCL_sf);
   strcpy(str, NCL_fit);
   count = 0;
   e = (struct UM_point_rec*)&ptr;
   while (UU_TRUE)
      {
      um_dl_pldas(UD_DASPCKLOC, /*pick curve*/UA_NCL, 3, &pick, 
                  1, &numint, 2);
      if (numint <= 0) goto done;
      ptr.key = um_get_pickkey(&pick.pent, 2);
      ur_retrieve_data_relnum(ptr.key, &ptr.rel_num);
      if (ptr.rel_num  !=  NCL_CURVE_REL)
         uu_uerror0(/*you must pick a curve*/UA_NCL,3);
      else
         {
         ur_retrieve_data_fixed(&ptr);
         ncl_add_token(str, e->label, NCL_comma);
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
