/*********************************************************************
**    NAME         :  riwtrcb.c
**       CONTAINS:
**       ur_wrt_txt_rcb()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riwtrcb.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:49
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) riwtrcb.c 2.1 11/19/86 20:01:22 single"};
#else
static char uu_sccsident[]={"@(#) riwtrcb.c 2.1 11/19/86 20:01:22 double"};
#endif
#endif

#include "udebug.h"
#include "ribase.h"
#include "xenv1.h"

/*********************************************************************
**    I_FUNCTION :  status = ur_wrt_txt_rcb(lu, rcb_ptr);
**       write a Relation Control Block(RCB) record in text format
**				into the file specified by lu.
**    PARAMETERS   
**       INPUT  : 
**          lu			int		xio logical unit number
**				rcb_ptr	UR_rcb *	pointer to the rcb to write
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful < 0 otherwise
**    SIDE EFFECTS : rcb record in file
**    WARNINGS     : none
*********************************************************************/

ur_wrt_txt_rcb(lu, rcb_ptr)
int					lu;			/* logical unit number */
struct UR_rcb_rec	*rcb_ptr;	/* pointer to the rcb to write */
{
	char	buf[80];		/* buffer to convert fields to string */
	int	status;		/* return status */
	int	length;

	uu_denter(UU_RTRC,(us,"ur_wrt_txt_rcb(lu=%d, ptr=0x%x)",lu,rcb_ptr));
	sprintf(buf, "%d %s\n",rcb_ptr->rel_num, rcb_ptr->relname);
	length = strlen(buf);
	status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
	if (status == 0)
	{
		sprintf(buf, "%d %d %d %d\n",rcb_ptr->status,rcb_ptr->n_ent,
					rcb_ptr->init_ent,rcb_ptr->n_varl);
		length = strlen(buf);
		status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
		if (status == 0)
		{
			sprintf(buf, "%d %d %d\n",rcb_ptr->var_info_size,rcb_ptr->tuple_size,
						rcb_ptr->rel_flags);
			length = strlen(buf);
			status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
			if (status == 0)
			{
				sprintf(buf, "%d %d %d\n",rcb_ptr->last_accessed_index,
							rcb_ptr->last_active_index, rcb_ptr->active_tuple_cnt);
				length = strlen(buf);
				status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
				if (status == 0)
				{
					sprintf(buf, "%d\n",rcb_ptr->bmap_size);
					length = strlen(buf);
					status = ux_write(lu, buf, 1, &length, UX_PRTERRS);
				}
			}
		}
	}
	uu_dexit;
	return(status);
}

