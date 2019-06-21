/*********************************************************************
**    NAME         :  n2uattr.c
**    NOTES: taken from model/m2uattr.c.
**       CONTAINS: user interface routines to change attributes
**			blankg
**			ncl_sea_ent_blank
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       n2uattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:21
*********************************************************************/

#include "vsegbf.h"
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdebug.h"
#include "view.h"
#include "mfort.h"
#include "nclfc.h"

UU_LOGICAL	ud_gnxt();
void ncl_sea_ent_blank();

/*********************************************************************
**    E_FUNCTION     : blankg()
**       blank/unblank entities.
**    PARAMETERS   
**       INPUT  : 
**          option				0 = Blank filtered
**										1 = Blank all
**										2 = Unblank filtered
**										3 = Unblank all
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void blankg(opt,nclkey)
	UM_int2 *opt;
	UM_int4 *nclkey;

	{
	UU_KEY_ID key;								/* key of retrieved data */
	int       option;

	uu_denter(UU_MTRC,(us,"blankg(%d)",option));

	key = *nclkey;
	option = *opt;
	ncl_sea_ent_blank(option,key);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ncl_sea_ent_blank()
**       blank/unblank entities.
**    PARAMETERS   
**       INPUT  : 
**          option				0 = Blank filtered
**										1 = Blank all
**										2 = Unblank filtered
**										3 = Unblank all
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sea_ent_blank(option,key)
	int option;
	UU_KEY_ID key;								/* key of retrieved data */

	{
	Gseg dsegid;								/* display segment identifier */
	int status, ur_update_blanked();
	UU_LOGICAL initialize;
	UU_LOGICAL blanked;
	int rel_num;
	/* unsigned long mask[4];*/					/* Das select mask */

	uu_denter(UU_MTRC,(us,"ncl_sea_ent_blank(%d)",option));

	/* Set DAS select mask for all relations other than lights */

	switch (option)
		{
		case 0: /* blank filtered */
			{
			if (ur_update_blanked(key, UU_TRUE) == 0)
				{
				ur_retrieve_disp_segid(key, &dsegid);
				uv_blanksegs(dsegid, key);
				}
			break;
			}
		case 1: /* blank all */
			{
			/* Get the key from delgeo and not from ud_gnxt. */
			initialize = UU_TRUE;
			while (uv_getobjs(initialize, &key, UU_FALSE) == 0)
				{
				initialize = UU_FALSE;
				ur_retrieve_blanked(key, &blanked);
				if (blanked == UU_FALSE)
					{
					ur_retrieve_data_relnum(key, &rel_num);
					if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
						{
						status = ur_update_blanked(key, UU_TRUE);
						ur_retrieve_disp_segid(key,&dsegid);
						uv_blanksegs(dsegid, key);
						}
					}
				}
			break;
			}
		
		case 2: /* unblank filtered */
			{

			if (ur_update_blanked(key,UU_FALSE) == 0)
				{
				ur_retrieve_disp_segid(key,&dsegid);
				uv_unblanksegs(dsegid, key);
				}
			break;
			}
		case 3: /* unblank all */
			{
			initialize = UU_TRUE;
			while (uv_getobjs(initialize, &key, UU_FALSE) == 0)
				{
				initialize = UU_FALSE;
				ur_retrieve_blanked(key, &blanked);
				if (blanked == UU_TRUE)
					{
					ur_retrieve_data_relnum(key, &rel_num);
					if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
						{
						status = ur_update_blanked(key, UU_FALSE);
						ur_retrieve_disp_segid(key,&dsegid);
						uv_unblanksegs(dsegid, key);
						}
					}
				}
			break;
			}
		}
	uu_dexit;
	}

