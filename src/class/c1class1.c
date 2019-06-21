/*********************************************************************
**    NAME         :  c1class1.c
**       CONTAINS: class method dispatchers
**          int uc_span_entity(eptr, dimptr, space)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       c1class1.c , 25.1          
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:57            
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "class.h"
#include "canbe.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdebug.h"
#include "misect.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_span_entity(eptr, dimptr, space)
**       A method-- returns the affine span of an entity
**    PARAMETERS   
**       INPUT  : 
**          eptr	-	entity 
**       OUTPUT :  
**          *dimptr-	dimension of affine space
**				space	--	definition of affine space (point/vector)
**				NOTE:	vector space[1] will be made unit length
**    RETURNS      : 0, of no problem, -2 if not defined 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_span_entity(eptr, dimptr, space)
	struct	UC_entitydatabag	*eptr;
	int		*dimptr;
	UU_REAL	space[2][3];

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC,(us,"uc_span_entity(key=%x)", eptr->key));

	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_SPAN_ENTITY);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(eptr, dimptr, space);
		}

	uu_dexitstatus("uc_span_entity", status);
	return(status);
	}

