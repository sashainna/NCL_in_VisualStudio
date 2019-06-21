
/*********************************************************************
**    NAME         :  vedba.c
**       CONTAINS: Routines to traverse master tuples for viewing
**  		uv_getobjs(init, key, editable_flag)
**			int uv_getalldispinrelation(rel_num,init,key)
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**   MODULE NAME AND RELEASE LEVEL 
**       vedba.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:57
**************************************************************************/
#include "udebug.h"
#include "usysdef.h"
#include "view.h"
#include "mdattr.h"
#include "mdebug.h"

/**************************************************************************
**  E_FUNCTION:  uv_getobjs(init, key, editable_flag)
**      Use Unibase to get all geometry while filtering out viewing entities
**  PARAMETERS   
**      INPUT  :  init	:	set to UU_TRUE to reset to start of unibase
**						editable_flag = if UU_TRUE, only return editable entities
**      OUTPUT :  key	:	next key to be drawn
**  RETURNS      :	UU_SUCCESS			:	if a valid key was found
**							UU_FAILURE	:	if there is no more geometry
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int
uv_getobjs(init, key, editable_flag)
	UU_LOGICAL init;
	UU_KEY_ID  *key;
	{
	int irtn;
	static int next_tupleid;
	int relnum;
	int displayable;
	UU_LOGICAL flag;

	uu_denter(UU_MTRC,(us,"uv_getobjs(init=%d,key=%x)",init,*key));

	if (init == UU_TRUE) next_tupleid = 1; else next_tupleid = *key + 1;
	irtn = UU_FAILURE;

	/* query Unibase for next key */
	while (ur_get_next_key(&next_tupleid, key) > -1)
		{
		next_tupleid++;

		ur_retrieve_displayable(*key, &displayable);
		if (displayable == UM_DISPLAYABLE) irtn = UU_SUCCESS;
		if(editable_flag)
		{
			ur_retrieve_editability(*key, &flag);
			uu_dprint(UU_DTRC,(us,
				"uv_getobjs, ur_retrieve_editability: *key=%x, flag=%d",
					*key, flag));
			if(!flag)
				irtn = UU_FAILURE;
		}
		
		if (irtn == UU_SUCCESS) break;
		}


	/* sprintf(UM_sbuf,"uv_getobjs: key=%d, irtn=%d",*key,irtn);
	um_pscroll(UM_sbuf); */

	uu_dexit;
	return(irtn);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_getalldispinrelation(init,rel_num,key)
**			Get all displayable master tuples for the specified relation.
**			If INIT is set to UU_TRUE, start from the begining of the
**			relation; otherwise, start from the specified key.
**    PARAMETERS   
**       INPUT  : 
**				init							UU_TRUE => start at begining
**												UU_FALSE => start from key
**          rel_num						relation number
**				key							key in the relation to start 
**												from (if init = UU_TRUE)
**       OUTPUT :  
**				key							key of next displayable entity
**												in the relation
**    RETURNS      : 
**			UU_SUCCESS iff next key in relation returned
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_getalldispinrelation(init,rel_num,key)
	UU_LOGICAL init;
	int rel_num;
	UU_KEY_ID *key;

	{
	int status;
	static int next_tupleid;
	int ret_rel_num;
	int displayable;

	uu_denter(UU_MTRC,(us,"uv_getalldispinrelation(init=%d,rel_num=%d,key=%x",
		init, rel_num, *key));

	status = UU_FAILURE;
	if (init) next_tupleid = 1;

	/* query Unibase for next key in the relation which is displayable*/
	while (ur_get_next_data_key(rel_num, &next_tupleid, key) > -1)
		{
		next_tupleid++;
		ur_retrieve_displayable(*key, &displayable);
		if (displayable == UM_DISPLAYABLE)
			{
			status = UU_SUCCESS;
			break;
			}
		if (status == UU_SUCCESS) break;
		}

	uu_dexit;
	return(status);
	}
