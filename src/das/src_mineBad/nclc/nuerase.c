/*********************************************************************
**    NAME         :  nuerase.c
**       CONTAINS: User interface routines for remove geoemetry 
**                 statement creation.
**       nclu_redef()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuerase.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:07
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
#include "view.h"


/*********************************************************************
**    E_FUNCTION     : nclu_erase
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
nclu_erase(option)

	int  option;
   {
   struct UC_entitydatabag e1;
   NCL_cmdbuf cmdbuf;
   int length, numint;
   UU_LOGICAL found, lstatus;
   UV_vport   vport;
   char str[256];
   int status;
   unsigned long mask[4];					/* Das select mask */
   int icount=1;

   uu_denter(UU_MTRC,(us,"nclu_erase"));

	/*  if invisible is picked */
	/* Set DAS select mask for all relations other than lights */

	mask[0] = mask[1] = mask[2] = mask[3] = -1;
	uu_clr_bit(mask, UM_LIGHT_REL-1);
	ud_lgeo(UU_TRUE, mask);

   if (option == 0)
	   {

	/* have user pick entities to blank 
	   if (uvu_pickvp(&vport) != UU_SUCCESS)
			{
			uu_dexit;
			return;
			}
			*/

	   status = ud_ldas(UD_DASSELECT, /*pick entities to blank:*/UM_MODEL, 
			  259, UU_NULL, 0, &numint,UD_NODEFAULT);

       if (status == UU_ALTACTION)
		{
		/* add INVIS/ keyword to command buffer */
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf, NCL_invis, NCL_nocomma);
		status = NCL_OKINPUT;
		while (status == NCL_OKINPUT)
			{
			status=ncl_add_str(&cmdbuf, 510, NCL_comma);
			if (status == NCL_DONE)
				{
			    ncl_add_cmdbuf(&cmdbuf);
			    ncl_call(&cmdbuf);
				goto done;
				}
			}
		}

	   if (numint <= 0) goto done;

	   /* generate INVIS/ statements with picked geometry names */
	   lstatus = ud_gnxt(UU_TRUE, UU_NULL, &e1.key, 1);
	   while (lstatus == UU_TRUE)
		  {
          icount = 1;
	 	  ncl_init_cmdbuf(&cmdbuf);
		  /* get names of picked geometry to invisible */
		  found = UU_FALSE;
		  while ((lstatus == UU_TRUE) && (cmdbuf.num_cmd < 3))
			 {
			 if (uc_retrieve_data(&e1, sizeof(e1)) == UU_SUCCESS)
				{

				/* generate statement if not drafting */
				if (e1.rel_num != 48)
					{
 					if (icount == 1)
						{
						/* add INVIS/ keyword to command buffer */
		 				ncl_add_token(&cmdbuf, NCL_invis, NCL_nocomma);
						icount = 0;
						}

					/* added for wireframe geometry. kahty */
					if (ncl_geom_type(e1.rel_num) == UU_SUCCESS || 
					wf_geom_type(e1.rel_num) == UU_SUCCESS)
				   		{
				   		ncl_get_label(&e1, str);
				   		ncl_add_token(&cmdbuf, str, NCL_comma);
				   		found = UU_TRUE;
				   		}
					}
				/* do not generate a source statement if drafting */
			 	else if (e1.rel_num == 48)
					{
					ncl_sea_ent_blank(0,e1.key);
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
	   }
   else if (option == 1)
	   {
	   ncl_init_cmdbuf(&cmdbuf);

		/* add INVIS/ keyword to command buffer */
	   ncl_add_token(&cmdbuf, NCL_invis, NCL_nocomma);
	   ncl_add_token(&cmdbuf, NCL_all, NCL_nocomma);
	   ncl_add_cmdbuf(&cmdbuf);
	   ncl_call(&cmdbuf);
	   }

   done:
   uu_dexit;
   }
