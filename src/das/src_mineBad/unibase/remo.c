/*********************************************************************
**    NAME         :remo.c
**       CONTAINS:
**       ur_mark_old()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       remo.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:32
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) remo.c 3.2 11/2/87 15:04:36 single"};
#else
static char uu_sccsident[]={"@(#) remo.c 3.2 11/2/87 15:04:36 double"};
#endif
#endif

#include  "usysdef.h"
#include  "udebug.h"
#include  "ribase.h"

/*********************************************************************
**    E_FUNCTION     :  ur_mark_old(key)
**       reset newly loaded tuple bit for this key
**    PARAMETERS   
**       INPUT  : 
**          key,	can be either a master(primary) key, or an 
**						actual relation key
**       OUTPUT :  
**				none
**    RETURNS      :	0 if able to set, -1 if specified tuples was
**							inactive(not used)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_mark_old(key)
UU_KEY_ID	key;		/* a master key, or relation key		*/
{
	int				status;	/* return status */
	UR_REL_NUM		rel;		/* what relation to set save bit */
	UR_TUPLE_INDX	tuple;	/* what tuple within relation to set save bit */
	UU_KEY_ID		rel_key;	/* a relation key */
	int				i;			/* an index */

	uu_denter(UU_RTRC,(us,"ur_mark_old(key:0x%x)",key));
	status = 0;
	ur_k2rt(key,&rel,&tuple);
	if(UR_rcb[rel].status == 0)
	{
		if(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_ALLOC_MAP]),tuple-1))
		{
			uu_clr_bit(&(UR_rcb[rel].bmap_ptr[UR_SVLD_MAP *
							UR_rcb[rel].bmap_size]),tuple-1);
		}
		else
		{
			uu_dprint(-1,(us,"ERROR:ur_mark_old for inactive tuple %d", tuple));
			status = -1 ; /* attempted to clr for load an inactive tuple	*/
		}
	}
	else
	{
		uu_dprint(-1,(us,"ERROR:ur_load_set for inactive relation %d", rel)) ;
		status = -1 ;	/* tried to set save for inactive relation		*/
	}
	uu_dexit ;
	return(status) ;
}
