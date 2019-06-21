/*********************************************************************
**
**    NAME         :  d2dpl.c
**
**       CONTAINS:
**  			ud_plane(prompt, ret_plane)
**  			ud_plane1(prompt, ret_plane)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d2dpl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:04
**
*********************************************************************/

#include "usysdef.h"
#include "derror.h"
#include "dasnog.h"
#include "dselect.h"
#include "dinput.h"
#include "ddef.h"
#include "mdpick.h"
#include "udebug.h"
#include "uhep.h"
#include "zsysdep.h"

#define SELREC (*event).indata.pickdata

/********************************************************************* 
**
**  I_FUNCTION			:  ud_plane(prompt, ret_plane)
**      plane high level DAS
**
**  PARAMETERS   
**
**      input:  
**					prompt = prompt buffer
**
**      output: 
**					 ret_plane =  plane definition
**
**  RETURNS      :  status of the operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_plane(prompt, ret_plane)
char *prompt; 								/* operator prompt string */
UD_PLANE_REC *ret_plane;				/* plane definition */
{
	UD_DASTAT status;						/* temp status variable */
	UD_DASTAT ud_plane1();				/* semantic interpreter */
	UD_DEVENT event;						/* event buffer */
	int count = 0;							/* number of times called counter */

	uu_denter(UU_DTRC,(us,"entering ud_plane"));

	do
	{
		ud_gevt(&event, UD_pckint, prompt, 1, UD_pckdev, UD_pckech, UU_NULL);
		status = ud_plane1(&event, ret_plane, &count);
	}
	while(status == DE_AGAIN);

	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION			:  ud_plane1(event, ret_chc)
**      plane sematic interpreter DAS
**
**  PARAMETERS   
**
**      input:  event = event buffer
**					 count = number of times we have iterated
**
**      output: ret_plane = choice number returned
**
**  RETURNS      :  status of the operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_plane1(event, ret_plane, count)
UD_DEVENT *event;						/* event structure */
UD_PLANE_REC *ret_plane;			/* plane definition */
int *count;								/* number of times we have iterated */
{
	UD_DASTAT status, ud_auxm(), ud_cart1();
	UM_PICKENT pent;
	UD_PLANE_REC loc_plane, loc_plane1;	/* plane definition */
	UD_NDCLOCREC ret_cord[3];		/* coordinate to return  */
	UD_PPICKREC pickrec;				/* pick record for pickresolve */

	UU_KEY_ID key_buf, um_get_pickkey(); 	/* mtid buffer for span routine */

	int badent;							/* bad entity pointer for span routine */
	int dim1, dim2;					/* dimension input */
	static int dimension;			/* dimension input */
	int i, stat;

	uu_denter(UU_DTRC,(us,"entering ud_plane1, count = %d", *count));

	if(*count == 0)
		dimension = -1;

	status = DE_TRUE;
	switch((*event).evclass)
	{

/*		-- convert to world plane, create a dummy point record,
			and pass to affine span routine -- */

		case UD_STRING:
		case UD_LOCATOR:

/*		-- get the key of the entity and pass to affine span routine -- */

		case UD_PICK:

/*			-- resolve the pick id to the unibase key id -- */

			if((*event).evclass == UD_PICK)
			{
				pickrec.depth = SELREC.depth;
				for(i=0; i<pickrec.depth; i++)
					pickrec.pickpath[i] = SELREC.pickpath[i];
				stat = um_d_pickresolve(&pickrec, 2, &pent);

				if(stat == 0)
					key_buf = um_get_pickkey(&pent, 2);
				else
					key_buf = 0;

/*				-- call the affine span routine to determine plane -- */

				stat = um_span_keylist(1, &key_buf, &dim2, &loc_plane, &badent);
				if(stat == UU_FAILURE)
				{

/*					-- no span method for this entity -- */

					uu_uerror0(UD_DASHEP, 101);
					status = DE_AGAIN;
					goto errout;
				}

/*				-- set the normtran -- */

				(*ret_plane).transform = SELREC.transform;
			}
			else
			{
				status = ud_cart1(event, ret_cord, UU_FALSE, ret_cord, UU_TRUE);
				if(status == DE_TRUE)
					um_span_ptlist(1, ret_cord, &dim2, &loc_plane, &badent);
				else
				{
					status = DE_AGAIN;
					goto errout;
				}

/*				-- set the normtran -- */

				if((*event).evclass == UD_STRING)
					(*ret_plane).transform = 1;
				else
					(*ret_plane).transform = (*event).indata.locdata.transform;
			}

/*			-- accumulate spans. we are done if a plane is defined,
				if not pick another entity -- */

			dim1 = dimension;
			zbytecp(loc_plane1, *ret_plane);

			uu_dprint(UU_DTRC,(us,"ud_plane1 before netspan: dim1=%d, dim2=%d",
							dim1, dim2));
			stat = um_netspan(dim1, &loc_plane1, dim2, &loc_plane, &dimension,
							ret_plane);

			uu_dprint(UU_DTRC,(us,"ud_plane1 from netspan: dim=%d, stat=%d",
							dimension, stat));

			if(dimension < 2)
				status = DE_AGAIN;
			else if(dimension == 2)
				status = DE_TRUE;
			else
			{

/*				-- 3-D space defined -- */

				uu_uerror0(UD_DASHEP, 103);
				*count = 0;
				status = DE_AGAIN;
				goto errout;
			}

			break;

		case UD_CHOICE:
			if((*event).evdev == UD_AUXMENU)
			{
				status = ud_auxm(event);
				if(status == DE_DONE)
				{

/*					-- plane not yet defined -- */

					uu_uerror0(UD_DASHEP, 102);
					status = DE_AGAIN;
				}
			}
			else
			{

/* --			invalid event		-- */

				uu_uerror0(UD_DASHEP, 23);
				status = DE_AGAIN;
			}
			break;

/*--	invalid event --*/

		default:
			uu_uerror0(UD_DASHEP, 23);
			status = DE_AGAIN;
			break;
	}

	(*count)++;
errout:;
	uu_dexit;
	return(status);
}
