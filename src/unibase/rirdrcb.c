/*********************************************************************
**    NAME         :  rirdrcb.c
**       CONTAINS:
**       ur_rd_txt_rcb()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rirdrcb.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:46
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) rirdrcb.c 2.2 2/2/87 14:15:24 single"};
#else
static char uu_sccsident[]={"@(#) rirdrcb.c 2.2 2/2/87 14:15:24 double"};
#endif
#endif

#include <stdio.h>
#include "udebug.h"
#include "ribase.h"
#include "xfsys0.h"
#include "xenv1.h"

/*********************************************************************
**    I_FUNCTION :  status = ur_rd_txt_rcb(lu, rcb_ptr);
**       read a Relation Control Block(RCB) record in text format
**				into the file specified by lu.
**    PARAMETERS   
**       INPUT  : 
**          lu			int		xio logical unit number
**				rcb_ptr	UR_rcb *	pointer to the rcb to read
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful < 0 otherwise
**    SIDE EFFECTS : rcb record in file
**    WARNINGS     : none
*********************************************************************/

ur_rd_txt_rcb(lu, rcb_ptr)
int					lu;			/* logical unit number */
struct UR_rcb_rec	*rcb_ptr;	/* pointer to the rcb to read */
{
	char str[80];
	int	status;		/* return status */
	FILE	*fd;

	uu_denter(UU_RTRC,(us,"ur_rd_txt_rcb(lu=%d, ptr=0x%x)",lu,rcb_ptr));
	status = 0;
	ux_get_os_filedesc(lu, &fd, UX_PRTERRS);
	uu_dprint(UU_RITRC,(us,"OS file descriptor = 0x%x",fd));
	UX_FSCANF0(
			(fd, "%d %s", &rcb_ptr->rel_num, rcb_ptr->relname),
			status);
	if (status == 0)
	{
		fscanf (fd, "%s", str);
		if (!strncmp(str,"#QNAN",5))
		{
			UX_FSCANF0(
					(fd, "%d %s", &rcb_ptr->rel_num, rcb_ptr->relname),
					status);
		}
	}
	if (status == 2)
	{
		uu_dprint(UU_RITRC,(us,"rel num %d, name %s",rcb_ptr->rel_num,
						rcb_ptr->relname));
		UX_FSCANF0( (fd, "%d %d %d %d",
			&rcb_ptr->status, &rcb_ptr->n_ent, &rcb_ptr->init_ent,
			&rcb_ptr->n_varl), status);
		if (status == 4)
		{
			uu_dprint(UU_RITRC,(us,"status %d, n_ent %d, n_varl %d",
							rcb_ptr->status, rcb_ptr->n_ent,
							rcb_ptr->n_varl));
			UX_FSCANF0((fd, "%d %d %d", &rcb_ptr->var_info_size,
				&rcb_ptr->tuple_size, &rcb_ptr->rel_flags), status);
			if (status == 3)
			{
				UX_FSCANF0((fd, "%d %d %d", &rcb_ptr->last_accessed_index,
					&rcb_ptr->last_active_index, &rcb_ptr->active_tuple_cnt),
					status);
				if (status == 3)
				{
					uu_dprint(UU_RITRC,(us,"active count %d",
									rcb_ptr->active_tuple_cnt));
					UX_FSCANF0((fd, "%d", &rcb_ptr->bmap_size), status);
					if (status != 1)
					{
						uu_dprint(-1,(us,
							"ERROR status of %d from UX_FSCANF0 reading bmap_size",
							status));
					}
					else status = 0;
				}
				else
				{
					uu_dprint(-1,(us,
						"ERROR status of %d from UX_FSCANF0 reading last_accessed,..",
						status));
				}
			}
			else
			{
				uu_dprint(-1,(us,
					"ERROR status of %d from UX_FSCANF0 reading var_info_size,..",
					status));
			}
		}
		else
		{
			uu_dprint(-1,(us,
				"ERROR status of %d from UX_FSCANF0 reading status,n_ent..",
				status));
		}
	}
	else
	{
		uu_dprint(-1,(us,
			"ERROR status of %d from UX_FSCANF0 reading rel_num,name", status));
		if (status == 1)
			uu_dprint(-1,(us,"...rel_num read = %d", rcb_ptr->rel_num));
	}
	uu_dexit;
	return(status);
}

