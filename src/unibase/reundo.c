/*********************************************************************
**    NAME         :  reundo.c
**       CONTAINS:
**			ur_undo()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reundo.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:38
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include	"udebug.h"
#include	"rstack.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_undo() 
**  undo the deletion of Unibase entities which were recently deleted
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful
**				<0,	Error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_undo()
{
	extern	UR_STACK(UR_del_stack,UR_DEL_STK_DEPTH,dstack_element) ;

	int		status				;	/* holds status of unibase calls		*/
	UU_KEY_ID	key_id			;	/* a key_id tuple							*/
	long		rel_id				;	/* the relation identifier				*/
	long		tuple_indx			;	/* tuple index into the relation		*/
	int	dsegid					;	/* display segment id					*/
	int	undo						;	/* true if undo to continue			*/
	int	i							;	/* an index									*/
	UU_KEY_ID	mkeys[UR_DEL_STK_DEPTH]	;	/* holder of master keys	*/
	int	mkey_cnt					;	/* number of master keys undeleted	*/
	struct	UR_attr	*attr_ptr;	/* ptr to attr or transf bundle		*/
	int		lst_len				;	/* length of a list, a dummy			*/
	struct	UR_data	geom_entity	;	/* a entity to draw				*/
	dstack_element	*dstk_ptr			;

/*--------------------------------------------------------------------
** Start of Executable Code
**--------------------------------------------------------------------
**
*/

	uu_denter(UU_RTRC,(us,"ur_undo"));
	status = 0;
	mkey_cnt = 0;
	undo = UU_TRUE;

	/* first pop all keys off the stack and reset their delete status */
	uu_dprint(UU_RITRC,(us,"stack curpos = %d",UR_del_stack.curpos));
	dstk_ptr = ur_pop(UR_del_stack);
	while(dstk_ptr != NULL && undo)
	{
		key_id = dstk_ptr->del_key;
		ur_k2rt(key_id,&rel_id,&tuple_indx);
		ur_del_reset(rel_id,tuple_indx);

		/* if a master key put on to a list to display later */
		if(rel_id == 0 && tuple_indx > 0) /* if master key */
		{
			mkeys[mkey_cnt] = key_id;
			mkey_cnt++;
		}
		/* if we hit a marked element on the stack, it is the end of */
		/* a delete session, so we are done undoing else pop another one */
		/* and continue */
		if(dstk_ptr->mark) 
		{
			undo = UU_FALSE;
		}
		else
		{
			dstk_ptr = ur_pop(UR_del_stack);
			if (dstk_ptr != NULL)
			{
				uu_dprint(UU_RITRC,(us,"dstk_ptr=0x%x mark=%d",
							dstk_ptr,dstk_ptr->mark));
			}
			else
			{
				status = -1;
				uu_dprint(-1,(us,"ERROR:ur_undo - delete stack underflow"));
			}
		}
	}	/* while dstk_ptr != NULL	*/
	/* now go back and redisplay all the undeleted data */
	for(i = 0; i < mkey_cnt; i++)
	{
		key_id = mkeys[i];
		ur_update_disp_segid(key_id,-1);	/* set neg. to force display etc. */
		geom_entity.key_id = key_id;
		uc_retrieve_data(&geom_entity,sizeof(geom_entity)) ;
		uc_display(&geom_entity);
	} /* for i < mkey_cnt */
	uu_dexit;
	return(status);
}
