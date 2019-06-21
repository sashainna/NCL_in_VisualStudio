/*********************************************************************
**    NAME         :  necattr.c   ( Change ATTRibutes )
**       CONTAINS: Support routines for changing entity attributes.
**            ncl_get_geo_color()
**            ncl_update_geo_color()
**            ncl_highlighting_geo()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       necattr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:25
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "mattr.h"
#include "class.h"
/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SEE D1INITD.C MODULE IN DAS DIRECTORY FOR A FULL EXPLANATION OF
	COLOR TABLE USAGE.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

static UU_LOGICAL S_highlighting = UU_FALSE;

/******************************************************************************
**    E_FUNCTION     :  ncl_get_geo_color(nclkey,color)
**       Returns the NCL interface color number for the requested entity.
**    PARAMETERS   
**       INPUT  : 
**          nclkey   = Key of entity.
**          color    = Entity's color.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
******************************************************************************/
void ncl_get_geo_color(nclkey,color)
UU_KEY_ID nclkey;
int *color;
{
	struct UC_attributedatabag attr1;

	uc_retrieve_attr(nclkey,&attr1);
	*color = attr1.color;

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_update_geo_color (key, color,flag)
**       Update color of this entity.
**    PARAMETERS   
**       INPUT  : 
**          key          - key of entity
**          color        - color
**          flag         - UU_TRUE = Color is being modified to highlight
**                         it during a picking operation.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_update_geo_color(key,color,flag)
UU_KEY_ID key;
int color;
UU_LOGICAL flag;
{
   int lcolor, status;

   uu_denter(UU_MTRC,(us,"ncl_update_color_()"));

   status = UU_SUCCESS;

	S_highlighting = flag;
   lcolor = color;
   if (lcolor == -1)
      ncl_get_entity_color(key, &lcolor);

   status = ur_update_color(key,lcolor);

   uu_dexit;
   return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_highlighting_geo ()
**       Update color of this entity.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_highlighting_geo()
{
	return(S_highlighting);
}
