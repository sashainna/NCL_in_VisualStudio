/*********************************************************************
**    NAME         :  nudisplay.c
**       CONTAINS: User interface routines for remove geoemetry
**                 statement creation.
**       nclu_redef()
**       nclu_redisp()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nudisplay.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:06
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselect.h"
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
**    E_FUNCTION     : nclu_disply
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
void nclu_disply(option)

	int  option;
   {
   struct UC_entitydatabag e1;
   NCL_cmdbuf cmdbuf;
   UU_LOGICAL found, lstatus;
   UV_vport   vport;
   char str[256];
   int stat;
   unsigned long mask[4];					/* Das select mask */
   int icount = 1;

   uu_denter(UU_MTRC,(us,"nclu_disply"));

	/* Set DAS select mask for all relations other than lights */

	mask[0] = mask[1] = mask[2] = mask[3] = -1;
	uu_clr_bit(mask, UM_LIGHT_REL-1);
	ud_lgeo(UU_TRUE, mask);

	/* if visible is picked */

   if (option == 0)
	   {

		/* have user pick entities to unblank */
		if (uvu_pickvp(&vport) != UU_SUCCESS)
			{
			uu_dexit;
			return;
			}
		stat = ud_calsel(UU_TRUE, 1, vport.xform, UU_FALSE);
		if (stat==-1)
			return;

		/* generate VISIBL/ statements with picked geometry names */
	   lstatus = ud_gnxt(UU_TRUE, UU_NULL, &e1.key, 1);
	   while (lstatus == UU_TRUE)
		  {
		  icount=1;
		  ncl_init_cmdbuf(&cmdbuf);
		  /* get names of picked geometry to visible */
		  found = UU_FALSE;
		  while ((lstatus == UU_TRUE) && (cmdbuf.num_cmd < 3))
			 {
			 if (uc_retrieve_data(&e1, sizeof(e1)) == UU_SUCCESS)
				{

				/* generate a source statement only if not drafting */
				if (e1.rel_num != 48)
					{
					if (icount == 1)
						{
						/* add VISIBL/ keyword to command buffer */
						 ncl_add_token(&cmdbuf, NCL_visibl, NCL_nocomma);
						 icount=0;
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
					ncl_sea_ent_blank(2,e1.key);
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

		/* add VISIBL/ keyword to command buffer */
	   ncl_add_token(&cmdbuf, NCL_visibl, NCL_nocomma);
	   ncl_add_token(&cmdbuf, NCL_all, NCL_nocomma);
	   ncl_add_cmdbuf(&cmdbuf);
	   ncl_call(&cmdbuf);
	   }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_redisp()
**       generate DISPLY/...,REDRAW command
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_redisp()
{
	UU_KEY_ID key;
	struct NCL_fixed_databag e1;
	NCL_cmdbuf cmdbuf;
	int numint,stype,inum;
	UU_LOGICAL initialize;
	char str[256];
	unsigned long mask[4];					/* Das select mask */

/* Set DAS select mask for all relations other than lights */
	mask[0] = mask[1] = mask[2] = mask[3] = -1;
	uu_clr_bit(mask, UM_LIGHT_REL-1);
	ud_lgeo(UU_TRUE, mask);

	stype = ud_getpick_type();

/* have user pick entities to blank */
	ud_setpick_type(UD_PICK_NORMAL);

	ud_ldas(UD_DASSELECT, UM_MODEL, 349, UU_NULL, 0, &numint,UD_NODEFAULT);

	ud_setpick_type(stype);
	inum = 0;
	if (numint > 0)
	{
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf,"DISPLY/", NCL_nocomma);
		initialize = UU_TRUE;
		while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
		{
			initialize = UU_FALSE;
			inum++;
			e1.key = key;
			if (ncl_retrieve_data_fixed (&e1) == UU_SUCCESS)
			{
				if (wf_geom_type(e1.rel_num) == UU_SUCCESS)
				{
					ncl_get_label(&e1, str);
					ncl_add_token(&cmdbuf, str, NCL_comma);
					if (cmdbuf.num_cmd >= 3)
					{
						ncl_add_token(&cmdbuf, "REDRAW", NCL_nocomma);
						ncl_add_cmdbuf(&cmdbuf);
						ncl_call(&cmdbuf);
						if (inum < numint)
						{
							ncl_init_cmdbuf(&cmdbuf);
							ncl_add_token(&cmdbuf,"DISPLY/", NCL_nocomma);
						}
					}
				}
			}
		}
		ncl_add_token(&cmdbuf, "REDRAW", NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
}
