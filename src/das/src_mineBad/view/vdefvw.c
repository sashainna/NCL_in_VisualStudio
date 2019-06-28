/*********************************************************************
**    NAME         :  vdefvw.c
**       CONTAINS: routines to initialize views
**			uv_front_view
**			uv_back_view
**			uv_top_view
**			uv_bottom_view
**			uv_right_view
**			uv_left_view
**			uv_ldimetric_view
**			uv_rdimetric_view
**			uv_lisometric_view
**			uv_risometric_view
**			uv_nclipv_view
**			uv_special_view(viewkey, viewname, flag)
**
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL 
**       vdefvw.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:57
**************************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "view.h"
#include "mdrel.h"
/* NCL */
UU_REAL UM_model_size = 11.0;		/* 11.0 inches */

/*********************************************************************
**    E_FUNCTION     : int uv_front_view(viewkey)
**       Create a front view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the front view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_front_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;

	uu_denter(UU_MTRC,(us,"uv_front_view()"));

	status = uv_create_view("Front", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, UM_yaxis, UM_zaxis, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_back_view(key)
**       Create a back view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the back view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_back_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;
	UM_vector normal;

	uu_denter(UU_MTRC,(us,"uv_back_view()"));


	um_vctmsc(UM_zaxis, (UU_REAL) -1.0, normal);
	status = uv_create_view("Back", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, UM_yaxis, normal, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_top_view(viewkey)
**       Create a top view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the top view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_top_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;
	UM_vector upvector;

	uu_denter(UU_MTRC,(us,"uv_top_view()"));

	um_vctmsc(UM_zaxis, (UU_REAL) -1.0, upvector);
	status = uv_create_view("Top", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, upvector, UM_yaxis, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_bottom_view(viewkey)
**       Create a bottom view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the bottom view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_bottom_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;
	UM_vector normal;

	uu_denter(UU_MTRC,(us,"uv_bottom_view()"));

	um_vctmsc(UM_yaxis, (UU_REAL) -1.0, normal);
	status = uv_create_view("Bottom", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, UM_zaxis, normal, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_right_view(viewkey)
**       Create a right side view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the right side view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_right_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;
	UM_vector normal;

	uu_denter(UU_MTRC,(us,"uv_right_view()"));

	status = uv_create_view("Right", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, UM_yaxis, UM_xaxis, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_left_view(viewkey)
**       Create a left side view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the left side view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_left_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;
	UM_vector normal;

	uu_denter(UU_MTRC,(us,"uv_left_view()"));

	um_vctmsc(UM_xaxis, (UU_REAL) -1.0, normal);
	status = uv_create_view("Left", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, UM_yaxis, normal, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_ldimetric_view(viewkey)
**       Create a left dimetric view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the left dimetric view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_ldimetric_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;
	UM_vector up;
	UM_vector normal;

	uu_denter(UU_MTRC,(us,"uv_ldimetric_view()"));

	um_xyztovc((UU_REAL) -1.0, (UU_REAL) 0.6, (UU_REAL) 1.0, normal);
	um_unitvc(normal, normal);
	um_nptpln(UM_yaxis, UM_zerovec, normal, up);
	um_unitvc(up, up);
	status = uv_create_view("Left Dimetric", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, up, normal, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_rdimetric_view(viewkey)
**       Create a right dimetric view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the right dimetric view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_rdimetric_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;
	UM_vector up;
	UM_vector normal;

	uu_denter(UU_MTRC,(us,"uv_rdimetric_view()"));

	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 0.6, (UU_REAL) 1.0, normal);
	um_unitvc(normal, normal);
	um_nptpln(UM_yaxis, UM_zerovec, normal, up);
	um_unitvc(up, up);
	status = uv_create_view("Dimetric", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, up, normal, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_lisometric_view(viewkey)
**       Create a left isometric view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the left isometric view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_lisometric_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;
	UM_vector up;
	UM_vector normal;

	uu_denter(UU_MTRC,(us,"uv_lisometric_view()"));

	um_xyztovc((UU_REAL) -1.0, (UU_REAL) 1.0, (UU_REAL) 1.0, normal);
	um_unitvc(normal, normal);
	um_nptpln(UM_yaxis, UM_zerovec, normal, up);
	um_unitvc(up, up);
	status = uv_create_view("Left Iso", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, up, normal, UM_model_size, 
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_risometric_view(viewkey)
**       Create a right isometric view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the right isometric view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_risometric_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;
	UM_vector up;
	UM_vector normal;

	uu_denter(UU_MTRC,(us,"uv_risometric_view()"));

	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 1.0, normal);
	um_unitvc(normal, normal);
	um_nptpln(UM_yaxis, UM_zerovec, normal, up);
	um_unitvc(up, up);
	status = uv_create_view("Isometric", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, up, normal, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_nclipv_view(viewkey)
**       Create a front view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          viewkey					key of the front view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_nclipv_view(viewkey)
	UU_KEY_ID *viewkey;

	{
	UV_view view;
	int status;

	uu_denter(UU_MTRC,(us,"uv_nclipv_view()"));

	status = uv_create_view("Nclipv", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, UM_yaxis, UM_zaxis, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);

	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int uv_special_view(viewkey, viewname)
**       Create a layer or invisble or secondary view initialized to current default setting.
**    PARAMETERS   
**       INPUT  : 
**          viewname: layer view name
**			flag:	2: layer view
**					3: invisible view
**					4: secondary view
**       OUTPUT :  
**          viewkey					key of the viewkey view created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_special_view(viewkey, viewname, flag)
UU_KEY_ID *viewkey;
char *viewname;
int flag;
{
	UV_view view, newview;
	int status;
	int vprojection;
	UM_length veyedist;
	UM_coord vrefpt;
	UM_vector vup;
	UM_vector vpn;
	UM_length vaperture;
	UU_LOGICAL vdoclip;
	UM_length vfclip;
	UM_length vbclip;

	status = -1;
/*
.....using "Front" view data for special view?
*/
/*	uv_getvnm("Front", &view);
	uu_move_byte(&view, &newview, sizeof(UV_view));

	newview.key = UU_NULL;
	newview.rel_num = UV_VIEW_REL;

	strcpy(newview.name, viewname);
	if (flag==2)
		newview.vtype = UV_LAYER_VIEW;
	else if (flag==3)
		newview.vtype = UV_INVISIBLE_VIEW;
	else if (flag==4)
		newview.vtype = UV_SECONDARY_VIEW;

	newview.can_save = (newview.vtype != UV_SYSDEF_VIEW);
	newview.modified = UU_FALSE;


	if (ur_create_data(&newview, sizeof(newview)) != 0)
	{
		uu_uerror1(UM_MODEL, 128, viewname);
		return -1;
	}
	status = UU_SUCCESS;
	*viewkey = newview.key;
*/
	if (flag==2)
		status = uv_create_view(viewname, UV_LAYER_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, UM_yaxis, UM_zaxis, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);
	else if (flag==3)
		status = uv_create_view(viewname, UV_INVISIBLE_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, UM_yaxis, UM_zaxis, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);
	else if (flag==4)
		status = uv_create_view(viewname, UV_SECONDARY_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
					UM_zerovec, UM_yaxis, UM_zaxis, UM_model_size,
					UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
					viewkey, 0);
	return (status);
}

