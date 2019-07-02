
/*********************************************************************
**    NAME         :  view0.h
**		CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       view0.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:07
*********************************************************************/
#ifndef VIEW0H

#include "gtblopst.h"
#include "view.h"

UV_screen UV_act_screen[UG_MAXOPWS];	/* active screens					*/
UV_init_views UV_view_to_vport;			/* initial relation of views to vports */
UV_vport_info	UV_act_vports[UG_MAXOPWS][UV_NVPORTS];	/* current active viewports	*/
int			UV_no_act_screens = {0};	/* number of active screens	*/

#define VIEW0H
#endif
