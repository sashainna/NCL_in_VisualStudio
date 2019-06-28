/*********************************************************************
**    NAME         :  rerds.c
**       CONTAINS:
**       ur_retrieve_disp_segid()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rerds.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:34
*********************************************************************/

#include	"usysdef.h"
#include	"rmtuple.h"
#include	"riddle.h"
#include	"ribase.h"
#include	"udebug.h"
extern int UR_active;

/*********************************************************************
**    E_FUNCTION     :   status = ur_retrieve_disp_segid(key_id,&dsegid)
**      retrieve display segment id
**    PARAMETERS   
**       INPUT  : 
**       key_id,	key_id to to use in retrieving the display seg id
**			dsegid,	a pointer to where the display seg id 
**						is to be put
**       OUTPUT :  
**			dsegid, the appropriate display segment id
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_disp_segid(key_id,dsegid)
UU_KEY_ID 	key_id;			/* the key_id to update to					*/
int			*dsegid;			/* pointer to the display segment id	*/
{
	int						status;		/* status, -1 if error, 0 otherwise		*/
	int						rel_id;		/* relation id from key_id, better = 0	*/
	int						tuple_indx;	/* entry id from key_id, better = 0		*/
	struct UR_rcb_rec		*rcb_ptr;	/* pointer to an rcb				*/
	struct UR_MTID_rec	*m_ptr;		/* ptr to relation 0 entry		*/
/*	int UR_active_save, first = 1;*/
/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC,(us,"ur_retrieve_disp_segid(key_id 0x%x)",key_id));
	status = 0;
/*
	UR_active_save = UR_active;
start:;
	if (first==0)
	{
		first=2;
		if (UR_active == 1)
		{
			ur_getu_second();
		}
		else
		{
			ur_getu_work();
		}
	}
*/
/*
	get key_id as rel,entry
*/	
	ur_k2rt(key_id,&rel_id,&tuple_indx);
	rcb_ptr = &UR_rcb[rel_id];
/* if (rcb_ptr->bmap_ptr==NULL) goto done; */
	if((rel_id == UR_MTUPLE_REL) && uu_tst_bit(rcb_ptr->bmap_ptr,tuple_indx-1)) 
	{
/*
		got a valid key_id relation and an active entry,
		now get the display segment id
*/
		status = ur_get_tuple_ptr(rel_id, tuple_indx,&m_ptr);
		if(status == 0)
		{
			*dsegid = m_ptr->dsegid;
		}
	}
	else
	{
		*dsegid = 0;
		status = -1;
	}
/*	if (first==1)
	{
		first = 0;
		goto start;
	}
done:;
	if (UR_active_save==1)
	{
		ur_getu_work();
	}
	else
		ur_getu_second();
*/
	uu_dexit;
	 return(status);
}
/*********************************************************************
**    E_FUNCTION     :  int uu_unireset2() 
**       Reset UNICAD system
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_unireset2()
	{

	unsigned long key_id;				/* current entity key */
	int nxt_ent;							/* index of next master key */
	int get_nxt_status;					/* status of next get opertation */
	int dsegid;								/* display segment id of current entity */

	int save_active =  UR_active;
	if (UR_active==1)
		ur_getu_second();
	/* loop over all entities in the data base
		and delete from the gks segment file */

	nxt_ent = 1;
	get_nxt_status = 0;
	while(get_nxt_status == 0)
		{
		get_nxt_status = ur_get_next_key(&nxt_ent,&key_id);  /* get next entity */
		if(get_nxt_status == 0)
			{
			ur_retrieve_disp_segid(key_id,&dsegid);   /* get entities gks seg-id */
			ur_update_disp_segid(key_id,-1); 
			}
		nxt_ent++;
		}
	ur_reset_unibase();							/* reset unibase */
	if (save_active==1)
		ur_getu_work();
}
