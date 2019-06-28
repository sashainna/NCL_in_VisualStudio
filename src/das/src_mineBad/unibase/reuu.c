/*********************************************************************
**    NAME         :  reuu
**       CONTAINS:
**       ur_unibase_used
**       ur_unibase_not_used
**       ur_unibase_change
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reuu.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:41
*********************************************************************/

#include "usysdef.h"
#include "mdrel.h"

extern UU_LOGICAL	UR_changed;	/* Unibase changed flag */
extern int	UR_chg_cnt;

/*********************************************************************
**    E_FUNCTION     :  count = ur_unibase_used()
**      return true(active entries) if rbase.has been used
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS : non-zero(UU_TRUE) if active unibase entries, 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL ur_unibase_used()
{
	int			i;			/* index to look from */
	UU_KEY_ID	key_id;	/* the returned key_id */

	i = 1 ;
	ur_get_next_key(&i,&key_id) ;	/* i = 0 if no active entries		*/
	if(i >= 1) /* if unibase used */
		return(UR_changed);
	else
		return(UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     :  ur_unibase_not_used()
**      Marks the Unibase as not changed.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ur_unibase_not_used()
{
	UR_changed = UU_FALSE;
	UR_chg_cnt = 0;
}

/*********************************************************************
**    E_FUNCTION     :  ur_unibase_change(relnum)
**      Marks the Unibase as changed if the Relation is not a View,
**      Viewport, Screen, or Light.
**    PARAMETERS   
**       INPUT  : 
**          relnum  = Relation number of Unibase entity that is being
**                    changed in the Unibase.
**       OUTPUT :  
**          none
**    RETURNS : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ur_unibase_change(relnum)
int relnum;
{
	int i;			/* index to look from */
	UU_KEY_ID key_id;	/* the returned key_id */
	char sbuf[80];
/*
.....Additional restrictions were made so the change counter does
.....not include extraneous counts - ASF 2/25/14.
*/
	if (relnum != UV_VIEW_REL && relnum != UV_VPORT_REL &&
		relnum != UV_SCREEN_REL && relnum != UM_LIGHT_REL &&
		relnum != NCL_COLOR_REL && relnum != UM_MTID_REL &&
		relnum != UQ_CALC_REL && relnum != UM_TRANSFORM_REL &&
		relnum != UM_ATTR_REL)
	{
		UR_changed = UU_TRUE;
		UR_chg_cnt++;
	}
}
