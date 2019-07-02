/*********************************************************************
**    NAME         : view.h 
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       view.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:07
*********************************************************************/

#ifndef VIEWH


#include "usysdef.h"
#include "go.h"
#include "gtblseg.h"
#include "viewddl.h"
#include "vconst.h"
#include "vsegbf.h"

typedef struct UV_viewdef_rec UV_view;
typedef struct UV_vport_rec UV_vport;
typedef struct UV_screen_rec UV_screen;

typedef struct {
	UU_KEY_ID	key;					/* unibase id of viewport							*/
	UU_KEY_ID	view_id;				/* unibase id of reference view					*/
	UU_KEY_ID	view;					/* unibase id of contained view					*/
	int			xform;				/* norm transformation for this viewport		*/
	UU_LOGICAL  disp_all;			/* display all geomtry								*/
} UV_vport_info;

typedef struct {
	UU_KEY_ID	vport_key[UV_NVPORTS];
	UU_KEY_ID	view_key[UV_NVPORTS];
} UV_init_views;

extern UV_vport_info	UV_act_vports[][UV_NVPORTS];	/* current active viewports	*/

extern UV_screen UV_act_screen[];	/* current active screen */
extern UV_init_views UV_view_to_vport;	/* initial relation of views to vports */

UU_KEY_ID uv_vdefine();
UU_KEY_ID uv_vpdefine();
UU_KEY_ID uv_scdefine();

extern int	UV_no_act_screens;				/* number of active screens	*/

#define UV_SYSDEF_VIEW	0
#define UV_USERDEF_VIEW	1
#define UV_LAYER_VIEW	2
#define UV_INVISIBLE_VIEW	3
#define UV_SECONDARY_VIEW	4

#define UV_PARALLEL		0
#define UV_PERSPECTIVE	1

#define UV_NOCLIP			UU_FALSE
#define UV_CLIP			UU_TRUE

#define VIEWH
#endif
