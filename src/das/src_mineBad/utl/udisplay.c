/*********************************************************************
**    NAME:  udisplay.c
**       CONTAINS:
**				int uu_post4Display(key)
**				int uu_displayPostedEntities()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       udisplay.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:53
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION: int uu_post4Display(key)
**		This function posts the key of the entity to be (re)displayed on 
**		the display bulletin board.
**    PARAMETERS   
**		INPUT: 
**			key		Key of the entity to be (re)displayed.
**		OUTPUT:	none.
**    RETURNS:		UU_SUCCESS if a message can be displayed, UU_FAILURE
**						otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int uu_post4Display(key)
	UU_KEY_ID key;
{
	int status = UU_SUCCESS;
	uu_denter(UU_UITRC,(us,"uu_post4Display(%s,%d)", 
		"not implemented, key is:", key));
	uu_dexitstatus("uu_post4Display", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int uu_displayPostedEntities()
**		This function displays the entities posted and deletes them from the 
**		bulletin board.
**    PARAMETERS   
**		INPUT:			none.
**		OUTPUT:		none.
**    RETURNS:		UU_SUCCESS if a message can be displayed, UU_FAILURE
**						otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int uu_displayPostedEntities()
{
	int status = UU_SUCCESS;
	uu_denter(UU_UITRC,(us,"uu_displayPostedEntities(%s)", "not implemented"));
	uu_dexitstatus("uu_displayPostedEntities", status);
	return(status);
}

