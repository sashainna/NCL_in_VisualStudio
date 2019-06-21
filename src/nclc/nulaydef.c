/*********************************************************************
**    NAME         :  nulaydef.c
**       CONTAINS: User interface routines for "DRAFT/....,LAYER"
**                 statement creation.
**       nclu_laycmd
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nulaydef.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:08
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "ulist.h"
#include "mdpick.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"

static int *Slay = UU_NULL;

/*********************************************************************
**    E_FUNCTION     : void nclu_laycmd(sflist,layer,flag)
**       generate "DRAFT/MODIFY=sf3,sf5,...,LAYER=3" statements.
**    PARAMETERS   
**       INPUT  : 
**          sflist  - list of surfaces
**          layer   - the layer
**          flag    - FALSE iff Previewing
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_laycmd(sflist,layer,flag)
UU_LIST *sflist;
char *layer;
UU_LOGICAL flag;
{
	NCL_cmdbuf cmdbuf;
	int i,numsf;
	UM_sgeo *geo;

   uu_denter(UU_MTRC,(us,"nclu_laycmd"));

	numsf = sflist->cur_cnt;
	if (numsf <= 0) goto done;
	geo = (UM_sgeo *) UU_LIST_ARRAY(sflist);

	if (!flag) 
	{
		int dum;

		Slay = (int *) uu_malloc (numsf * sizeof(int));
		for (i = 0; i < numsf; i++)
		{
			um_get_attrib (&geo[i].key,&dum,&dum,&dum,&dum,&Slay[i]);
		}
	}

	i = 0;
	while (i < numsf)
	{
		ncl_init_cmdbuf(&cmdbuf);
		if (!flag)
			ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_draft, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_modify, NCL_nocomma);
		ncl_add_token(&cmdbuf, "=", NCL_nocomma);

		for (; i < numsf && cmdbuf.num_cmd < 3; i++)
		{
			ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);
		}
		ncl_add_token(&cmdbuf, NCL_layer, NCL_nocomma);
		ncl_add_token(&cmdbuf, layer, NCL_nocomma);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}

done:
	uu_dexit;
	return;
}

/*********************************************************************
**    E_FUNCTION     : void nclu_restore_layers (sflist)
**       Restore surface layers after Preview.
**    PARAMETERS   
**       INPUT  : 
**          sflist  - list of surfaces that may have their layer attribute
**                    changed
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_restore_layers (sflist)
UU_LIST *sflist;
{
	int i,numsf;
	UM_sgeo *geo;

   uu_denter(UU_MTRC,(us,"ncl_restore_layers"));

	numsf = sflist->cur_cnt;
	if (numsf <= 0 || !Slay) goto done;
	geo = (UM_sgeo *) UU_LIST_ARRAY(sflist);

	for (i = 0; i < numsf; i++)
	{
		um_update_layer (geo[i].key,Slay[i]);
	}

done:
	if (Slay)
	{
		uu_free(Slay); Slay = UU_NULL;
	}
	return;
}
