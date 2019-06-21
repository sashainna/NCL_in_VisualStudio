/*********************************************************************
**    NAME         :  nuremove.c
**       CONTAINS: User interface routines for remove geoemetry 
**                 statement creation.
**       nclu_redef()
**       nclu_rem_sufint()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuremove.c , 25.1
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
**    E_FUNCTION     : nclu_remove
**       generate remove statements
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_remove()

   {
   struct UC_entitydatabag e1;
   NCL_cmdbuf cmdbuf;
   int numint;
   UU_LOGICAL found, lstatus;
   char str[256];
   int   status;
   UU_KEY_ID okey;

   uu_denter(UU_MTRC,(us,"nclu_remove"));

	/* pick geometry to remove */
   ud_lgeo(UU_TRUE, UD_ncl_labels);
   status = ud_ldas(UD_DASSELECT, UA_NCL, 200, UU_NULL, 0, &numint, UD_NODEFAULT);

   if (status == UU_ALTACTION)
    	{
		/* add REMOVE/ keyword to command buffer */
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf, NCL_remove, NCL_nocomma);
		status = NCL_OKINPUT;
		while (status == NCL_OKINPUT)
			{
			status=ncl_add_str(&cmdbuf, 511, NCL_comma);
			/* ncl_add_cmdbuf(&cmdbuf);*/
			if (status == NCL_DONE)
				{
			    ncl_add_cmdbuf(&cmdbuf);
			    ncl_call(&cmdbuf);
				goto done;
				}
			}
	}

   if (numint <= 0) goto done;

	/* generate REMOVE/ statements with picked geometry names */
   lstatus = ud_gnxt(UU_TRUE, UU_NULL, &e1.key, 1);
   okey = 0;
   while (lstatus == UU_TRUE)
      {
      ncl_init_cmdbuf(&cmdbuf);

/*       add REMOVE/ keyword to command buffer */
      ncl_add_token(&cmdbuf, NCL_remove, NCL_nocomma);

/*       get names of picked geometry to remove */
      found = UU_FALSE;
      while ((lstatus == UU_TRUE) && (cmdbuf.num_cmd < 3))
         {
		 /*RAH: test if key is still alive.  also, test if key is same
		   as previous key - this avoids duplicate labels */
		 if ((!ur_test_tuple(e1.key)) &&
         (okey != e1.key) && (uc_retrieve_data(&e1, sizeof(e1)) == UU_SUCCESS))
            {

			/* added for wireframe geometry. kathy */
            if (ncl_label_type(e1.rel_num) == UU_SUCCESS)
               {
               ncl_get_label(&e1, str);
               ncl_add_token(&cmdbuf, str, NCL_comma);
               found = UU_TRUE;
			   okey = e1.key;
               }
            }
         lstatus = ud_gnxt(UU_FALSE, UU_NULL, &e1.key, 1);
         }
      if (found == UU_TRUE)
         {
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
         }
      if (lstatus != UU_TRUE) break;
      }

   done:
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_rem_sufint
**       generate "REDEF/surf,REMOVE,ALL" statement
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_rem_sufint()
{
	NCL_cmdbuf cmdbuf;

	struct UC_entitydatabag e1;
   int numint;
   UU_LOGICAL found, lstatus;
   char str[256];
   int   status;
   UU_KEY_ID okey;

/* 
.....pick a surface
*/
	ud_lgeo(UU_TRUE, UD_ncl_allsf);
	status = ud_ldas(UD_DASSELECT, UA_NCL, 478, UU_NULL, 0, &numint, UD_NODEFAULT);
	if (numint <= 0) goto done;

	lstatus = ud_gnxt(UU_TRUE, UU_NULL, &e1.key, 1);
	okey = 0;
	while (lstatus == UU_TRUE)
	{
		found = UU_FALSE;
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf, NCL_redef, NCL_nocomma);

		if ((!ur_test_tuple(e1.key)) &&
			(okey != e1.key) && (uc_retrieve_data(&e1, sizeof(e1)) == UU_SUCCESS))
		{
			ncl_get_label(&e1, str);
			ncl_add_token(&cmdbuf, str, NCL_comma);
			found = UU_TRUE;
			okey = e1.key;
		}
		if (found == UU_TRUE)
		{
			ncl_add_token(&cmdbuf, "REMOVE", NCL_comma);
			ncl_add_token(&cmdbuf, "ALL", NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
		lstatus = ud_gnxt(UU_FALSE, UU_NULL, &e1.key, 1);
	}
done:;
}
