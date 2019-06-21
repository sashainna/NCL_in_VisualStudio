/*********************************************************************
**
**    NAME         :  axhint.c
**
**       CONTAINS:	routines to interface between drafting code and
**				the xhatch dispatch routines
**
**						ua_copy_hatch_rec()
**						ua_create_hatch_rec()
**						ua_transf_hatch_rec()
**						ua_translate_hatch_rec()
**						ua_rotate_hatch_rec()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       axhint.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:43
**
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) axhint.c 2.4 11/19/85 09:33:41 single"};
#else
static char uu_sccsident[]={"@(#) axhint.c 2.4 11/19/85 09:33:41 double"};
#endif


#include "usysdef.h"
#include "udebug.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "adrf.h"

/*********************************************************************
**
**    E_FUNCTION  :	ua_copy_hatch_rec(key1,key2)
**
**				makes a Unibase copy of the hatchlin_rec reference by
**				key1 and returns the key of the copy in key2
**
**    PARAMETERS   
**
**       INPUT  :	key1 = key of Unibase original  
**
**       OUTPUT :  key2 = key of Unibase copy
**
**          none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ua_copy_hatch_rec(key1, key2)
UU_KEY_ID key1, *key2;
{
	struct UA_hatchlin_rec e1, e2;

	uu_denter(UU_STRC,(us, "copy_hatch_rec(%d %x)", key1, key2));

	/* get hatchlin_rec for key1 */
	e1.key = key1;
	if (ua_retrieve_xhatch(&e1, sizeof(struct UA_hatchlin_rec))
		!= UU_SUCCESS)
	{
		uu_denter2(UU_STRC,(us,"ua_copy_drafting: uc_retrieve_data ERROR"));
		uu_dexit;
	}
	/* make copy in Unibase */
	ua_copy_xhatch(&e1,&e2);

	/* get key2 */
	*key2 = e2.key;

	uu_free(e1.wt);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ua_create_hatch_rec(xhlist, len, key)
**				sets up UA_hatchlin_rec and calls ua_create_xhatch()
**				to put enitity in Unibase 
**
**    PARAMETERS   
**       INPUT  : 
**						UU_REAL 		*xhlist;		SAL LIST of COORD
**						int			len;			length of xhlist
**       OUTPUT :  
**          			UU_KEY_ID	int	*key;	 key of newly created entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_create_hatch_rec(xhlist, len, key)
UU_REAL 	*xhlist;
int			len;
UU_KEY_ID	*key;
{
	struct UA_hatchlin_rec	hatchrec;

	uu_denter(UU_STRC,(us,"ua_create_hatch_rec(%x %d %x) 1st element %g",
		xhlist, len, key, *xhlist));
	
	hatchrec.rel_num = UA_HATCHING_REL;
	hatchrec.wt = xhlist;

	/* change len in coords to len in reals */
	hatchrec.no_wt = len * 3;	

	/* put hatch_rec in Unibase */
	ua_create_xhatch(&hatchrec, key);

	uu_dexit;
	}

/*********************************************************************
**
**    E_FUNCTION  :	ua_transf_hatch_rec(key, tf, upd)
**
**			transform the coords in the hatchlin_rec referenced by key
**			by the matrix tf
**
**    PARAMETERS   
**
**       INPUT  :	key - key of hatchlin_rec to xform
**						tf - xform matrix
**						upd - flag to update Unibase
**
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ua_transf_hatch_rec(key, tf, upd)
UU_KEY_ID	key;
UU_REAL	tf[4][3];
UU_LOGICAL		upd;
{
	struct UA_hatchlin_rec e;

	uu_denter(UU_STRC,(us, "ua_transf_hatch_rec: key = %d", key));

	/* get hatchlin_rec for key */
	e.key = key;
	if (ua_retrieve_xhatch(&e, sizeof(struct UA_hatchlin_rec))
		!= UU_SUCCESS)
	{
		uu_denter2(UU_STRC,(us,"ua_transf_hatch_rec: uc_retrieve_data ERROR"));
		uu_dexit;
	}
	/* transform coords */
	ua_transf_xhatch(&e, tf, upd);

	uu_free(e.wt);
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION  :	ua_translate_hatch_rec(key, offset)
**
**			translate the coords in the hatchlin_rec referenced by key
**			by offset
**
**    PARAMETERS   
**
**       INPUT  :	key - key of hatchlin_rec to translate
**						offset - translation offset
**
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ua_translate_hatch_rec(key, offset)
UU_KEY_ID	key;
UM_vector offset;
{
	struct UA_hatchlin_rec e;

	uu_denter(UU_STRC,(us, "ua_translate_hatch_rec: key = %d", key));

	/* get hatchlin_rec for key */
	e.key = key;
	if (ua_retrieve_xhatch(&e, sizeof(struct UA_hatchlin_rec))
		!= UU_SUCCESS)
	{
		uu_denter2(UU_STRC,(us,"ua_translate_hatch_rec: uc_retrieve_data ERROR"));
		uu_dexit;
	}
	/* translate coords */
	ua_translate_xhatch(&e, offset);

	uu_free(e.wt);
	uu_dexit;
}


/*********************************************************************
**
**    E_FUNCTION  :	ua_rotate_hatch_rec(key, pt, dir, angle, rotmat)
**
**			rotates the coords in the hatchlin_rec referenced by key
**			by the matrix rotmat
**
**    PARAMETERS   
**
**       INPUT  :	key - key of hatchlin_rec to rotate
**						pt	 - point on rotation axis
**						dir - direction vector
**						angle - rotation angle
**						rotmat - rotation matrix
**
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ua_rotate_hatch_rec(key, pt, dir, angle, rotmat)
UU_KEY_ID	key;
UM_coord 	pt;
UM_vector	dir;
UM_angle	angle;
UM_transf	rotmat;
{
	struct UA_hatchlin_rec e;

	uu_denter(UU_STRC,(us, "ua_rotate_hatch_rec: key = %d", key));

	/* get hatchlin_rec for key */
	e.key = key;
	if (ua_retrieve_xhatch(&e, sizeof(struct UA_hatchlin_rec))
		!= UU_SUCCESS)
	{
		uu_denter2(UU_STRC,(us,"ua_rotate_hatch_rec: uc_retrieve_data ERROR"));
		uu_dexit;
	}
	/* rotate coords */
	ua_rotate_xhatch(&e, pt, dir, angle, rotmat);

	uu_free(e.wt);
	uu_dexit;
}



