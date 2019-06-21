/*********************************************************************
**    NAME         :  m3ugroup.c
**       CONTAINS: user interface routines for GROUP manipulation
**			int umu_create_group()
**			int umu_dissolve_group()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ugroup.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:59
*********************************************************************/
#include "udebug.h"
#include "uhep.h"
#include "dmark.h"
#include "dselmask.h"
#include "dasnog.h"
#include "class.h"
#include "canbe.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mxxx.h"
#include "mdpick.h"
#include "mdcoord.h"

UU_LOGICAL	ud_gnxt();

/*********************************************************************
**    E_FUNCTION     : int umu_create_group()
**       Create a group.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umu_create_group()

	{
	struct UM_grouper_rec group;
	UU_KEY_ID subent_key;
	int numret;
	int i;
	int initialize;
	int status;
	int dsegid;
	static int first = UU_TRUE;

	uu_denter(UU_MTRC,(us,"umu_create_group()"));

	if(first == UU_TRUE)
	{
		uc_build_select_mask(UC_CANBE_AGROUP, UD_canbe_agroup);
		first = UU_FALSE;
	}

	status = UU_SUCCESS;

   /* setup the entity in unibase */
  	ur_setup_data(UM_GROUP_REL, &group, sizeof(struct UM_grouper_rec));

	/* limit entities to be included in group */
	ud_lgeo(UU_TRUE, UD_canbe_agroup);

/*	-- Limit DAS to only editable entities -- */

	ud_leditable(UU_TRUE);

	ud_ldas(UD_DASSELECT, /*pick entities to include in group */ UM_MODEL, 300,
		UU_NULL, 1, &numret, UD_NODEFAULT);
	if (numret > 0)
		{
		group.no_member = 0;
		initialize = UU_TRUE;
		while (ud_gnxt(initialize, UU_NULL, &subent_key, 1) == UU_TRUE)
			{
			initialize = UU_FALSE;

			/* put the key into the group structure */
			group.member[group.no_member] = subent_key;
			group.no_member++;
			
			/* change the displayability of the sub-entity */
    	   ur_retrieve_disp_segid(subent_key, &dsegid);
			if (dsegid >= 0) uv_delsegs(dsegid);                                 
       	ur_update_disp_segid(subent_key, -1);
			ur_update_displayable(subent_key, UM_NEVERDISPLAYABLE);
	  		}

		/* add the group to UNIBASE */
		status = um_create_geom(&group, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		if (status != UU_SUCCESS) goto failed;
	
		/* call to the function used to display an entity */
		status = uc_display(&group);
		if (status != UU_SUCCESS) goto failed;

		}

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexit;
	return(status);
	}  

/**********************************************************************
**  E_FUNCTION: int umu_dissolve_group()
**		This procedure will dissolve a group of entities
**  PARAMETERS   
**      INPUT  : none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int umu_dissolve_group()
	{
	struct UM_grouper_rec group;
	struct UC_entitydatabag subent;
	UM_PLOCREC  pick;
	int numret;
	int i;
	int dsegid;
	int status;

	uu_denter(UU_MTRC,(us,"umu_dissolve_group()"));
	status = UU_SUCCESS;

	/* limit the pickable entities to groups */
	ud_lgeo(UU_TRUE, UD_group);

	/* prompt the user for the group to dissolve */
	um_dl_pldas(UD_DASPCKLOC, /* pick group to dissolve */ UM_MODEL, 301,
		&pick, 1, &numret, 1);
   if (numret != 0)
	   {
		/* get the key from the pick record */
		group.key = um_get_pickkey(&pick.pent, 1);

		/* retrieve the group information */
		uc_retrieve_data(&group, sizeof(struct UM_grouper_rec));

		/* delete the display segment of the group and delete from UNIBASE */
  	   ur_retrieve_disp_segid(group.key,&dsegid);
		if (dsegid >= 0) uv_delsegs(dsegid);                                 
		ur_delete_all(group.key);

		/* process through the list of subentities to redisplay */
		for (i=0; i<group.no_member; i++)
			{
			/* retrieve the subentitity information */
			subent.key = group.member[i];
			uc_retrieve_data(&subent, sizeof(struct UC_entitydatabag));

			/* set the displayability for the subentity and display it */
			ur_update_displayable(subent.key, UM_DISPLAYABLE);
			uc_display(&subent);
			}
		}
	status = UU_SUCCESS;
	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexit;
	return(status);
	}  
