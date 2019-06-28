
/*********************************************************************
**    NAME         :  m9edrw3.c
**       CONTAINS:  routines to remove duplicate drawing geometry 
**			um_init_dup_list(list)
**			um_free_dup_list()
**			int um_duplicate(eptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m9edrw3.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:12
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdgenent.h"
#include "modef.h"
#include "mdebug.h"

struct UM_drw_2dline {
	UU_REAL spt[2];
	UU_REAL ept[2];
	};

UU_LIST duplist;
UU_LIST *list;

/*********************************************************************
**    E_FUNCTION     : um_init_dup_list()
**       initialize list for duplicate geometry removal from
**			a drawing
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_init_dup_list()

	{
	uu_denter(UU_MTRC,(us,"um_init_dup_list()"));
	uu_list_init(&duplist, sizeof (struct UM_drw_2dline), 1000, 1000);
	list = &duplist;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_free_dup_list()
**       free list for duplicate geometry removal from a drawing
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_free_dup_list()

	{
	uu_denter(UU_MTRC,(us,"um_free_dup_list()"));
	uu_list_free(list);
	list = UU_NULL;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int um_duplicate(eptr)
**       Determine if the geometric entity (EPTR) which is to be
**		 	included in a drawing (i.e. has already been projected
**			into the drawing space) is a duplicate of previously 
**			defined geometry. At present, only lines are considered.
**    PARAMETERS   
**       INPUT  : 
**          eptr							pointer to a geometric entity
**       OUTPUT :  
**          none
**    RETURNS      :
**				0 if not a duplicate; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_duplicate(eptr)
	struct UM_entitydatabag *eptr;

	{
	int status;
	struct UM_line_rec *line3d;
	struct UM_drw_2dline *line2d;
	struct UM_drw_2dline line;
	UU_LOGICAL equalx;
	UU_LOGICAL equaly;
	UU_LOGICAL duplicate;
	int i;

	uu_denter(UU_MTRC,(us,"um_duplicate(key=%d)",eptr->key));

	status = 0;
	if (eptr->rel_num == UM_LINE_REL)
		{
		line3d = (struct UM_line_rec *) eptr;
		line2d = (struct UM_drw_2dline *) UU_LIST_ARRAY(list);
		duplicate = UU_FALSE;
		for (i=0; i<list->cur_cnt; i++)
			{
			equalx = UM_ZILCH(line3d->spt[0] - line2d[i].spt[0]);
			equaly = UM_ZILCH(line3d->spt[1] - line2d[i].spt[1]);
			if (equalx && equaly)
				{
				equalx = UM_ZILCH(line3d->ept[0] - line2d[i].ept[0]);
				equaly = UM_ZILCH(line3d->ept[1] - line2d[i].ept[1]);
				duplicate = (equalx && equaly);
				}
			else
				{
				equalx = UM_ZILCH(line3d->spt[0] - line2d[i].ept[0]);
				equaly = UM_ZILCH(line3d->spt[1] - line2d[i].ept[1]);
				if (equalx && equaly)
					{
					equalx = UM_ZILCH(line3d->ept[0] - line2d[i].spt[0]);
					equaly = UM_ZILCH(line3d->ept[1] - line2d[i].spt[1]);
					duplicate = (equalx && equaly);
					}
				}
			if (duplicate)
				{
				/*um_pscroll("new line is a duplicate");*/
				goto done;
				}
			}
		line.spt[0] = line3d->spt[0];
		line.spt[1] = line3d->spt[1];
		line.ept[0] = line3d->ept[0];
		line.ept[1] = line3d->ept[1];
		uu_list_push(list, &line);
		}
	/*um_pscroll("new entity not a line or not a duplicate line");*/
	status = -1;
done:
	uu_dexit;
	return (status);
	}

