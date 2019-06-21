/*********************************************************************
**    NAME         :  veview.c
**       CONTAINS:  Routines to manipulate views
**			int uv_create_view(vname, vtype, vprojection, veyedist, 
**									vrefpt, vup, vpn, vaperture,
**									vdoclip, vfclip, vbclip, viewkey)
**			UU_KEY_ID uv_vdefine(name, view)
**			uv_setrefpt(view, ref_pt)
**			uv_setvup(view, up_vect)
**			uv_setvpn(view, pln_norm)
**			uv_setfbclip(view, front, back)
**			uv_setclip(view, do_clip)
**			uv_setvaperture(view, aperture)
**			uv_setvtype(view, vtype)
**			uv_setprojection(view, projection, eyedist)
**			uv_save_ref_pt(view, ref_pt)
**			uv_save_up_vect(view, up_vect)
**			uv_save_pln_norm(view, pln_norm)
**			uv_save_fbclip(view, front, back)
**			uv_save_clip(view, do_clip)
**			uv_save_vaperture(view, aperture)
**			uv_update_ref_pt(view, ref_pt)
**			uv_update_up_vect(view, up_vect)
**			uv_update_pln_norm(view, pln_norm)
**			uv_update_fbclip(view, front, back)
**			uv_update_clip(view, do_clip)
**			uv_update_vaperture(view, aperture)
**			uv_getvnm(name, view)
**			uv_getvid(key, view)
**			uv_vrename(view, name)
**			uv_vdelete(view)
**			uv_putv(view)
**			uv_vsave(view, save_view)
**			uv_vrestore(view)
**			uv_vupdate(view, update_view)
**			char **uv_get_viewname(number)
**			uv_update_secondview(int flag)
**			uv_upd_scdv(flag)
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       veview.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:58
**************************************************************************/
#include "udebug.h"
#include "usysdef.h"
#include "uhep.h"
#include "mdrel.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "view.h"
#include "nclfc.h"
#include <ctype.h>
extern int UR_active;
void uv_setrefpt();
void uv_setvup();
void uv_setvpn();
void uv_setfbclip();
void uv_setvaperture();
void uv_setprojection();
void uv_save_ref_pt();
void uv_save_up_vect();
void uv_save_pln_norm();
void uv_save_fbclip();
void uv_save_clip();
void uv_save_vaperture();
void uv_update_ref_pt();
void uv_update_up_vect();
void uv_update_pln_norm();
void uv_update_fbclip();
void uv_update_vaperture();
void uv_vdelete();
void uv_setclip();
void uv_update_clip();
/*********************************************************************
**    E_FUNCTION     : int uv_create_view(vname, vtype, vprojection, veyedist, 
**									vrefpt, vup, vpn, vaperture,
**									vdoclip, vfclip, vbclip, viewkey)
**			Create a view entity in UNIBASE.        
**    PARAMETERS   
**       INPUT  : 
**          vname					name of view (must be unique)
**				vtype					UV_SYS_VIEW or UV_USER_VIEW
**				vprojection			UV_PARALLEL  or UV_PERSPECTIVE
**				veyedist				distance of eye to ref point
**				vrefpt				view reference point
**				vup					view up vector
**				vpn					view plane normal
**				vaperture			view aperture
**				vdoclip				UV_CLIP or UV_NOCLIP
**				vfclip				front clipping plane distance
**				vbclip				back clipping plane distance
**				flag: overwrite flag: 1: overwrite the view
**										  0: not overwrite
**       OUTPUT :  
**          viewkey				key of view entity created
**    RETURNS      : 
**			UU_SUCCESS						if no error occurs
*(*		UU_FAILURE				otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_create_view(vname, vtype, vprojection, veyedist,
	vrefpt, vup, vpn, vaperture,
	vdoclip, vfclip, vbclip,
	viewkey, flag)
	char *vname;
	int vtype;
	int vprojection;
	UM_length veyedist;
	UM_coord vrefpt;
	UM_vector vup;
	UM_vector vpn;
	UM_length vaperture;
	UU_LOGICAL vdoclip;
	UM_length vfclip;
	UM_length vbclip;
	UU_KEY_ID *viewkey;
	int flag;
	{
	UV_view view;
	int status;

	uu_denter(UU_MTRC,(us,"uv_create_view(name=%s)",vname));

	status = UU_FAILURE;
	if (uv_getvnm(vname, &view) == UU_SUCCESS)
	{
/*
.....view already exist
*/
/*
......if not overwrite, just return UU_FAILURE;
*/
		if (flag==0)
			return UU_FAILURE;
/*
.....delete the old view
*/
		uv_vdelete(&view);
	}

	view.key = 0;
	view.rel_num = UV_VIEW_REL;

	strcpy(view.name,vname);

	view.vtype = vtype;
	view.can_save = (vtype != UV_SYSDEF_VIEW);
	view.modified = UU_FALSE;

	uv_setprojection(&view, vprojection, veyedist);
	uv_setrefpt(&view, vrefpt);
	uv_setvup(&view, vup);
	uv_setvpn(&view, vpn);
	uv_setvaperture(&view, vaperture);
	uv_setfbclip(&view, vfclip, vbclip);
	uv_setclip(&view, vdoclip);

	if (ur_create_data(&view, sizeof(view)) != 0)
		{
		uu_uerror1(/* error in creating view entity */ UM_MODEL, 128, vname);
		goto done;
		}

	status = UU_SUCCESS;
	*viewkey = view.key;

done:
	uu_dexit;
	return (status);
	}

/**************************************************************************
**  E_FUNCTION:  UU_KEY_ID uv_vdefine(name, view)
**      Define a default view and create the view entity in Unibase
**  PARAMETERS   
**      INPUT  :  name	: name to be given to view
**						view	: pointer to a UV_view structure
**      OUTPUT :  none
**  RETURNS      :  key of view defined
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
UU_KEY_ID uv_vdefine(name, view)
	char name[15];
	UV_view	*view;

	{

	uu_denter(UU_MTRC,(us,"uv_vdefine(name=%s)",name));

	uv_setrefpt(view, UM_zerovec);
	uv_setvup(view, UM_yaxis);
	uv_setvpn(view, UM_zaxis);

	switch (UM_cpln.length_unit)
			{
		case UM_INCH:
		case UM_FEET:
		case UM_MILE:
			view->sav_aperture = view->cur_aperture = 254.;
			break;
		case UM_MM:
		case UM_CM:
		case UM_M:
		case UM_KM:
		case UM_MIL:
			view->sav_aperture = view->cur_aperture = 200.;
			break;
			}

	view->sav_front_clip = view->cur_front_clip = 10000.0;
	view->sav_back_clip  = view->cur_back_clip  = -10000.0;
	view->sav_do_clip    = view->cur_do_clip    = 0;

	strcpy(view->name, name);

	view->rel_num = UV_VIEW_REL;
	view->vtype = 0;								/* parallel view default */
	view->modified = UU_FALSE;
	view->can_save = UU_TRUE;

	ur_create_data(view);

	uu_dexit;
	return(view->key);
	}

/**************************************************************************
**  E_FUNCTION:  uv_setrefpt(view, ref_pt)
**      Set the reference point for this view
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						ref_pt: view reference point to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setrefpt(view, ref_pt)
	UV_view		*view;
	UM_coord		ref_pt;

	{
	uu_denter(UU_MTRC,(us,"uv_setrefpt(key=%x)",view->key));
	uv_save_ref_pt(view, ref_pt);
	uv_update_ref_pt(view, ref_pt);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvup(view, up_vect)
**      Set the up vector for this view
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						up_vect	: view up vector to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvup(view, up_vect)
	UV_view		*view;
	UM_vector	up_vect;

	{
	uu_denter(UU_MTRC,(us,"uv_setvup(key=%x)",view->key));
	uv_save_up_vect(view, up_vect);
	uv_update_up_vect(view, up_vect);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvpn(view, pln_norm)
**      Set the view plane normal for this view
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						pln_norm	: view plane normal to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpn(view, pln_norm)
	UV_view		*view;
	UM_vector	pln_norm;

	{
	uu_denter(UU_MTRC,(us,"uv_setvpn(key=%x)",view->key));
	uv_save_pln_norm(view, pln_norm);
	uv_update_pln_norm(view, pln_norm);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setfbclip(view, front, back)
**      Set the front and back clipping distances in this view
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						front	: front distance to clipping plane
**						back	: back distance to clipping plane
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setfbclip(view, front, back)
	UV_view  *view;
	UU_REAL 	front, back;

	{
	uu_denter(UU_MTRC,(us,"uv_setfbclip(key=%x)",view->key));
	uv_save_fbclip(view, front, back);
	uv_update_fbclip(view, front, back);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setclip(view, do_clip)
**      Set the enabling of front and back clipping in this view
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						do_clip	: if > 0 clip, if =0 don't clip
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setclip(view, do_clip)
	UV_view  *view;
	int	  	do_clip;

	{
	uu_denter(UU_MTRC,(us,"uv_setclip(key=%x)",view->key));
	uv_save_clip(view, do_clip);
	uv_update_clip(view, do_clip);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvaperture(view, aperture)
**      Set the aperture for this view
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						aperture:	length of window in "x" axis
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvaperture(view, aperture)
	UV_view  *view;
	UU_REAL  aperture;

	{
	uu_denter(UU_MTRC,(us,"uv_setvaperture(key=%x)",view->key));
	uv_save_vaperture(view, aperture);
	uv_update_vaperture(view, aperture);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvtype(view, vtype)
**      Set the view type planar/3D for this view.
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						vtype	: =0 planar, >0 3D
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvtype(view, vtype)
	UV_view   *view;
	int  	 	vtype;

	{
	uu_denter(UU_MTRC,(us,"uv_setvtype(key=%x)",view->key));
	view->vtype = vtype;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setprojection(view, projection, eyedist)
**      Set the view projection type (parallel, perspective) for this view
**			and the distance of the eye to the view reference point.
**  PARAMETERS   
**      INPUT  : 
*				view					pointer to a view structure
**				projection			UV_PARALLEL or UV_PERSPECTIVE
**				eyedist				distance of eyepoint
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setprojection(view, projection, eyedist)
	UV_view   *view;
	int  	 	projection;
	UU_REAL	eyedist;

	{
	uu_denter(UU_MTRC,(us,"uv_setprojection(key=%x)",view->key));
	view->projection = projection;
	view->sav_eye_dist = eyedist;
	view->cur_eye_dist = eyedist;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_save_ref_pt(view, ref_pt)
**      Update the reference point for this view in the current portion
**			of the view record
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						ref_pt: view reference point to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_save_ref_pt(view, ref_pt)
	UV_view		*view;
	UM_coord		ref_pt;

	{
	uu_denter(UU_MTRC,(us,"uv_save_ref_pt(key=%x)",view->key));
	um_vctovc(ref_pt, view->sav_ref_pt);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_save_up_vect(view, up_vect)
**      Update the up vector for this view in the current portion 
**			of the view record
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						up_vect	: view up vector to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_save_up_vect(view, up_vect)
	UV_view		*view;
	UM_vector	up_vect;

	{
	UM_vector vup;

	uu_denter(UU_MTRC,(us,"uv_save_up_vect(key=%x)",view->key));
	um_unitvc(up_vect, vup);
	um_vctovc(vup, view->sav_up_vect);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_save_pln_norm(view, pln_norm)
**      Update the view plane normal for this view in the current
**			portion of the view record
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						pln_norm	: view plane normal to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_save_pln_norm(view, pln_norm)
	UV_view		*view;
	UM_vector	pln_norm;

	{
	UM_vector vpn;

	uu_denter(UU_MTRC,(us,"uv_save_pln_norm(key=%x)",view->key));
	um_unitvc(pln_norm, vpn);
	um_vctovc(vpn, view->sav_pln_norm);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_save_fbclip(view, front, back)
**      Update the front and back clipping distances in this view in the
**			current portion of the view record
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						front	: front distance to clipping plane
**						back	: back distance to clipping plane
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_save_fbclip(view, front, back)
	UV_view  *view;
	UU_REAL 	front, back;

	{
	uu_denter(UU_MTRC,(us,"uv_save_fbclip(key=%x)",view->key));
	view->sav_front_clip = front;
	view->sav_back_clip = back;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_save_clip(view, do_clip)
**      Update the enabling of front and back clipping in this view in 
**			the current portion of the view record
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						do_clip	: if > 0 clip, if =0 don't clip
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_save_clip(view, do_clip)
	UV_view  *view;
	int	  	do_clip;

	{
	uu_denter(UU_MTRC,(us,"uv_save_clip(key=%x)",view->key));
	view->sav_do_clip = do_clip;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_save_vaperture(view, aperture)
**      Update the aperture for this view in the current portion of the
**			view record
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						aperture	: length of window in "x" axis
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_save_vaperture(view, aperture)
	UV_view  *view;
	UU_REAL	aperture;

	{
	uu_denter(UU_MTRC,(us,"uv_save_vaperture(key=%x)",view->key));
	view->sav_aperture = aperture;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_update_ref_pt(view, ref_pt)
**      Update the reference point for this view in the current portion
**			of the view record
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						ref_pt: view reference point to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_ref_pt(view, ref_pt)
	UV_view		*view;
	UM_coord		ref_pt;

	{
	uu_denter(UU_MTRC,(us,"uv_update_ref_pt(key=%x)",view->key));
	um_vctovc(ref_pt, view->cur_ref_pt);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_update_up_vect(view, up_vect)
**      Update the up vector for this view in the current portion 
**			of the view record
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						up_vect	: view up vector to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_up_vect(view, up_vect)
	UV_view		*view;
	UM_vector	up_vect;

	{
	UM_vector vup;

	uu_denter(UU_MTRC,(us,"uv_update_up_vect(key=%x)",view->key));
	um_unitvc(up_vect, vup);
	um_vctovc(vup, view->cur_up_vect);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_update_pln_norm(view, pln_norm)
**      Update the view plane normal for this view in the current
**			portion of the view record
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						pln_norm	: view plane normal to use
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_pln_norm(view, pln_norm)
	UV_view		*view;
	UM_vector	pln_norm;

	{
	UM_vector vpn;

	uu_denter(UU_MTRC,(us,"uv_update_pln_norm(key=%x)",view->key));
	um_unitvc(pln_norm, vpn);
	um_vctovc(vpn, view->cur_pln_norm);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_update_fbclip(view, front, back)
**      Update the front and back clipping distances in this view in the
**			current portion of the view record
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						front	: front distance to clipping plane
**						back	: back distance to clipping plane
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_fbclip(view, front, back)
	UV_view  *view;
	UU_REAL 	front, back;

	{
	uu_denter(UU_MTRC,(us,"uv_update_fbclip(key=%x)",view->key));
	view->cur_front_clip = front;
	view->cur_back_clip = back;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_update_clip(view, do_clip)
**      Update the enabling of front and back clipping in this view in 
**			the current portion of the view record
**  PARAMETERS   
**      INPUT  :  view		: pointer to a view structure
**						do_clip	: if > 0 clip, if =0 don't clip
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_clip(view, do_clip)
	UV_view  *view;
	int	  	do_clip;

	{
	uu_denter(UU_MTRC,(us,"uv_update_clip(key=%x)",view->key));
	view->cur_do_clip = do_clip;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_update_vaperture(view, aperture)
**      Update the aperture for this view in the current portion of the
**			view record
**  PARAMETERS   
**      INPUT  :  view	: pointer to a view structure
**						aperture	: length of window in "x" axis
**      OUTPUT :  view	: changed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_vaperture(view, aperture)
	UV_view  *view;
	UU_REAL	aperture;
	{

	uu_denter(UU_MTRC,(us,"uv_update_vaperture(key=%x)",view->key));
	view->cur_aperture = aperture;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_getvnm(name, view)
**      Search through Unibase for a reference view with this name
**  PARAMETERS   
**      INPUT  :  name	:	name of view to be found
**						view	:	pointer to view structure where view is to be put
**      OUTPUT :  none
**  RETURNS      :  UU_SUCCESS		: if the view was found,
**						  UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int uv_getvnm(name, view)
	char *name;
	UV_view *view;

	{
	UU_KEY_ID key;
	int next_tupleid, switched;
	char msg[256];

	uu_denter(UU_MTRC,(us,"uv_getvnm(%s)", name));

	switched = 0;
	if (UR_active==2)
/*
.....always retrieve vport info from working unibase
*/
	{
		ur_getu_work();
		switched = 1;
	}
	/* search unibase list to the view with this name */
	next_tupleid = 1;
	while (ur_get_next_data_key(UV_VIEW_REL, &next_tupleid, &key) > -1)
		{
		next_tupleid++;
		
		/* retrieve view to check */
		if (uv_getvid(key, view) == UU_FAILURE)
			{
			sprintf (msg, "failure in uv_getvnm, %x not retrieved\n", key);
			ud_printmsg(msg);
			exit(1);
			}

		/* if names are the same, we're done */
		if (strcmp(view->name, name) == 0)
		{
			if (switched)
				ur_getu_second();
			uu_dexit;
			return(UU_SUCCESS);
		}
	}

	if (switched)
		ur_getu_second();
	uu_dexit;
	return(UU_FAILURE);
	}
 
/**************************************************************************
**  E_FUNCTION:  uv_getvnm1(name, view)
**      Does exactly the same as uv_getvnm (described above) but converts
**      the names to the UPPER case. We need it for the DRAFT/FORMAT statement.
**  PARAMETERS   
**      INPUT  :  name  :   name of view to be found
**                view  :   pointer to view structure where view is to be put
**      OUTPUT :  none
**  RETURNS      :  UU_SUCCESS : if the view was found,
**                  UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int uv_getvnm1(name, view)
    char *name;
    UV_view *view;
 
    {
    UU_KEY_ID key;
    int next_tupleid;
    int i;
    char source [21], target[21], msg[256];
 
    uu_denter(UU_MTRC,(us,"uv_getvnm(%s)", name));
/*       
..... 
.....Convert the examined  name to the upper case. 
..... 
*/ 
        for (i=0; name[i]> ' ' && i < 20; i++) 
        {
             target[i] = 0;
             target[i] = islower(name[i]) ? toupper(name[i]) : name[i];
        }
        target[i] = 0;
 
    /* search unibase list to the view with this name */
    next_tupleid = 1;
    while (ur_get_next_data_key(UV_VIEW_REL, &next_tupleid, &key) > -1)
        {
        next_tupleid++;
        
        /* retrieve view to check */
        if (uv_getvid(key, view) == UU_FAILURE)
            {
            sprintf (msg, "failure in uv_getvnm, %x not retrieved\n", key);
            ud_printmsg(msg);
			exit(1);
            }
 
/*
.....
.....Convert the unibase view name to the upper case.
.....
*/
        for (i=0; i<strlen(view->name); i++)
        source[i] = islower(view->name[i]) ? toupper(view->name[i]) : view->name[i];
        source[i] = 0;

/*
.....
..... if names are the same, we're done
.....
*/
        if (strcmp(source, target) == 0)
            {
            uu_dexit;
            return(UU_SUCCESS);
            }
        }
 
    uu_dexit;
    return(UU_FAILURE);
    }

/**************************************************************************
**  E_FUNCTION:  uv_getvid(key, view)
**      Retrieve a view with this key
**  PARAMETERS   
**      INPUT  :  key	:	Unibase id of view to be found
**						view	:	pointer to view structure where view is to be put
**      OUTPUT :  none
**  RETURNS      :  UU_SUCCESS		: if the view was found,
**						  UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int uv_getvid(key, view)
	UU_KEY_ID key;
	UV_view *view;
	{
	int irtn,switched;

	uu_denter(UU_MTRC,(us,"uv_getvid(key=%x)", key));

	switched = 0;
	if (UR_active==2)
/*
.....always retrieve vport info from working unibase
*/
	{
		ur_getu_work();
		switched = 1;
	}
	view->key = key;
	ur_retrieve_data(view, sizeof(UV_view));
	if (ur_retrieve_data(view, sizeof(UV_view)) < 0)
		irtn = UU_FAILURE;
	else
		irtn = UU_SUCCESS;

	if (switched)
		ur_getu_second();
	uu_dexit;
	return(irtn);
	}
/**************************************************************************
**  E_FUNCTION:  uv_vrename(view, name)
**      Rename a view
**  PARAMETERS   
**      INPUT  :  view		: view to be renamed
**						name		: new name
**      OUTPUT :  view		: renamed view
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_vrename(view, name)
	UV_view	*view;
	char		*name;
	{
	uu_denter(UU_MTRC,(us,"uv_vrename(key=%x)",view->key));
	strcpy(view->name, name);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_vdelete(view)
**      Delete a view, also deletes its saved view if one exists
**  PARAMETERS   
**      INPUT  :  view		: view to be deleted
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_vdelete(view)
	UV_view *view;
{
	uu_denter(UU_MTRC,(us,"uv_vdelete(key=%x)",view->key));

	/* delete this view if it is not one of the ProCADD predefined views */
	if ((view->can_save == UU_TRUE))
		ur_delete_all(view->key);

	uu_dexit;
}

/**************************************************************************
**  E_FUNCTION:  uv_putv(view)
**      Save this view in Unibase
**  PARAMETERS   
**      INPUT  :  view to be saved
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_putv(view)
	UV_view *view;
	{
	int stat,switched;
	char msg[256];

	uu_denter(UU_MTRC,(us,"uv_putv(%s)",view->name));
	
	switched = 0;
	if (UR_active==2)
/*
.....always save view info into working unibase
*/
	{
		ur_getu_work();
		switched = 1;
	}
	stat = ur_update_data(view);
	if (stat < 0)
		{
		sprintf(msg, "uv_putv view %s not updated by unibase\n", view->name);
		ud_printmsg(msg);
		exit(1);
		}
	if (switched)
		ur_getu_second();
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_vsave(view, save_view)
**      Save view record view into view record save_view
**  PARAMETERS   
**      INPUT  :  view :	view to be saved
**						save_view:	view to save into
**      OUTPUT :  none
**  RETURNS      :  UU_FAILURE	: if view is a reference view
**						  UU_SUCCESS		: otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int uv_vsave(view, save_view)
	UV_view *view;
	UV_view *save_view;
	{
	int stat;

	uu_denter(UU_MTRC,(us,"uv_vsave(%s, %s)",view->name, save_view->name));

	/* check if view is a reference view, can't be saved */
	if (save_view->can_save == UU_FALSE)
		stat = UU_FAILURE;
	else
		{
		uv_save_ref_pt(save_view, view->cur_ref_pt);
		uv_save_pln_norm(save_view, view->cur_pln_norm);
		uv_save_up_vect(save_view, view->cur_up_vect);
		uv_save_vaperture(save_view, view->cur_aperture);
		uv_save_fbclip(save_view, view->cur_front_clip, view->cur_back_clip);
		uv_save_clip(save_view, view->cur_do_clip);
		uv_putv(save_view);
		}

	uu_dexit;
	return(stat);
	}

/**************************************************************************
**  E_FUNCTION:  uv_vrestore(view)
**      Restore this view, saved view is not modified
**  PARAMETERS   
**      INPUT  :  view : view to be restored
**      OUTPUT :  none
**  RETURNS      :  UU_SUCCESS		:	if a saved view existed and restore was done
**						  UU_FAILURE	: otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_vrestore(view)
	UV_view *view;
	{

	uu_denter(UU_MTRC,(us,"uv_vrestore(%s)", view->name));

	uv_update_ref_pt(view, view->sav_ref_pt);
	uv_update_pln_norm(view, view->sav_pln_norm);
	uv_update_up_vect(view, view->sav_up_vect);
	uv_update_vaperture(view, view->sav_aperture);
	uv_update_fbclip(view, view->sav_front_clip, view->sav_back_clip);
	uv_update_clip(view, view->sav_do_clip);
	uv_putv(view);

	uu_dexit;
	}
/**************************************************************************
**  E_FUNCTION:  uv_vupdate(view, update_view)
**      Update view record view into view record update_view
**  PARAMETERS   
**      INPUT  :  view :	view with current values set
**						update_view:	view record to update
**      OUTPUT :  none
**  RETURNS      :  UU_FAILURE	: if view is a reference view
**						  UU_SUCCESS		: otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_vupdate(view, update_view)
	UV_view *view;
	UV_view *update_view;
	{
	uu_denter(UU_MTRC,(us,"uv_vupdate(%s, %s)",view->name, update_view->name));

	uv_update_ref_pt(update_view, view->cur_ref_pt);
	uv_update_pln_norm(update_view, view->cur_pln_norm);
	uv_update_up_vect(update_view, view->cur_up_vect);
	uv_update_vaperture(update_view, view->cur_aperture);
	uv_update_fbclip(update_view, view->cur_front_clip, view->cur_back_clip);
	uv_update_clip(update_view, view->cur_do_clip);
/*
.....Here wrong?
//	uv_putv(view);
*/
	uv_putv(update_view);

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION         :  uv_get_viewname2(number)
**       get all the view names, not used.
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT : number: number of view name 
**    RETURNS      : a list of view name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added by Yurong
.....8/18/97
*/
char **uv_get_viewname2(number)
int *number;
{
	int status, entnum, len, switched;
	UV_view view;
	char **view_name;
	status = 0;
	entnum = 0;
	*number = 0;	
	switched = 0;
	if (UR_active==2)
/*
.....always retrieve vport info from working unibase
*/
	{
		ur_getu_work();
		switched = 1;
	}
	view.rel_num = UV_VIEW_REL;
/*
.....count view nunber first in order to dynamic allocate space
.....added yurong 2/10/1998
*/
	while (status == 0)
	{
		entnum++;
		len = entnum;
		status = ur_get_next_data_key(view.rel_num, &entnum,
								&view.key);
	}
	view_name = (char **) uu_malloc(len *sizeof(char *));

	status = 0;
	entnum = 0;
/*	view_name = (char **) uu_malloc(20 *sizeof(char *)); */
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(view.rel_num, &entnum,
								&view.key);
		if (status == 0)
		{
			ur_retrieve_data(&view, sizeof(view));
			if ((strcmp(view.name, "drw_view") != 0)
				&& (strcmp(view.name, "Nclipv") != 0))
			{	
				view_name[*number] = (char *)uu_malloc(sizeof(view.name));	
				strcpy(view_name[*number], view.name);
				(*number)++;
			}
		}
	}
	if (switched)
		ur_getu_second();
	return (view_name);
}

/*********************************************************************
**    E_FUNCTION         :  uv_get_screenname(number)
**       get all the screen names 
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT : number: number of screen name 
**    RETURNS      : a list of screen name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char **uv_get_screenname(number)
int *number;
{
	int status, entnum, len, switched ;
	UV_screen	screen;
	char **screen_name;
	status = 0;
	entnum = 0;
	*number = 0;	
	switched = 0;
	if (UR_active==2)
/*
.....always retrieve vport info from working unibase
*/
	{
		ur_getu_work();
		switched = 1;
	}
	screen.rel_num = UV_SCREEN_REL;
	while (status == 0)
	{
		entnum++;
		len = entnum;
		status = ur_get_next_data_key(screen.rel_num, &entnum,
								&screen.key);
	}
	screen_name = (char **) uu_malloc(len *sizeof(char *));

	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(screen.rel_num, &entnum,
								&screen.key);
		if (status == 0)
		{
			ur_retrieve_data(&screen, sizeof(screen));
			if ((strcmp(screen.name, "drw_screen") != 0)
				&& (strcmp(screen.name, "nclipv") != 0))
			{	
				screen_name[*number] = (char *)uu_malloc(sizeof(screen.name));	
				strcpy(screen_name[*number], screen.name);
				(*number)++;
			}
		}
	}
	if (switched)
		ur_getu_second();
	return (screen_name);
}
/*********************************************************************
**    E_FUNCTION         :  uv_get_viewname(number, flag)
**       get all the view names (seem as uv_get_viewname2 except
**			it will put as "UV_SYSDEF_VIEW", "UV_USERDEF_VIEW", "UV_LAYER_VIEW"
**			"UV_INVISIBLE_VIEW" and "UV_SECONDARY_VIEW" order 
**    PARAMETERS   
**       INPUT  : 
**				flag: 0: include all view
**						1: exclude layer view
**       OUTPUT : number: number of view name 
**    RETURNS      : a list of view name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char **uv_get_viewname(number, flag)
int *number, flag;
{
	int status, entnum, len, switched;
	UV_view view;
	char **view_name;
	status = 0;
	entnum = 0;
	*number = 0;	
	switched = 0;
	if (UR_active==2)
/*
.....always retrieve vport info from working unibase
*/
	{
		ur_getu_work();
		switched = 1;
	}
	view.rel_num = UV_VIEW_REL;

	while (status == 0)
	{
		entnum++;
		len = entnum;
		status = ur_get_next_data_key(view.rel_num, &entnum,
								&view.key);
	}
	view_name = (char **) uu_malloc(len *sizeof(char *));
/*
.....get UV_SYSDEF_VIEW
*/
	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(view.rel_num, &entnum,
								&view.key);
		if (status == 0)
		{
			ur_retrieve_data(&view, sizeof(view));
			if ((view.vtype==UV_SECONDARY_VIEW) || (view.vtype==UV_INVISIBLE_VIEW)
				|| (view.vtype==UV_LAYER_VIEW) || (view.vtype==UV_USERDEF_VIEW))
					continue;
			if ((strcmp(view.name, "drw_view") != 0)
				&& (strcmp(view.name, "Nclipv") != 0))
			{	
				view_name[*number] = (char *) uu_malloc(sizeof(view.name));	
				strcpy(view_name[*number], view.name);
				(*number)++;
			}
		}
	}
/*
.....get UV_USERDEF_VIEW
*/
	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(view.rel_num, &entnum,
								&view.key);
		if (status == 0)
		{
			ur_retrieve_data(&view, sizeof(view));
			if ((view.vtype==UV_SECONDARY_VIEW) || (view.vtype==UV_INVISIBLE_VIEW)
				|| (view.vtype==UV_LAYER_VIEW) || (view.vtype==UV_SYSDEF_VIEW))
					continue;
			view_name[*number] = (char *)uu_malloc(sizeof(view.name));	
			strcpy(view_name[*number], view.name);
			(*number)++;
		}
	}
/*
.....get UV_LAYER_VIEW
*/
/*
.....sometimes, we need to remove the layer view from the layer list because it could be too much
.....we will include "Layer view" as a simpler name when we list view list in the 
.....form list and pop the layer name as the choice when we need display layer view
.....so we add flag to see if we include/exclude the layer view
.....Yurong 10/24/05
*/
	if (flag==0)
	{
		status = 0;
		entnum = 0;
		while (status == 0)
		{
			entnum++;
			status = ur_get_next_data_key(view.rel_num, &entnum,
									&view.key);
			if (status == 0)
			{
				ur_retrieve_data(&view, sizeof(view));
				if ((view.vtype==UV_SECONDARY_VIEW) || (view.vtype==UV_INVISIBLE_VIEW)
					|| (view.vtype==UV_USERDEF_VIEW) || (view.vtype==UV_SYSDEF_VIEW))
						continue;
				view_name[*number] = (char *)uu_malloc(sizeof(view.name));	
				strcpy(view_name[*number], view.name);
				(*number)++;
			}
		}
	}
/*
.....get UV_INVISIBLE_VIEW and UV_SECONDARY_VIEW
*/
	view_name[*number] = (char *)uu_malloc(sizeof(view.name));	
	strcpy(view_name[*number], "Invisible");
	(*number)++;

	view_name[*number] = (char *)uu_malloc(sizeof(view.name));	
	strcpy(view_name[*number], "Extern Unibase");
	(*number)++;

	if (switched)
		ur_getu_second();
	return (view_name);
}
/**************************************************************************
**  E_FUNCTION:  uv_update_secondview(int flag)
**			Update the secondary view display
**			
**  PARAMETERS   
**      INPUT  :  flag: 1: delete all segment in secondary view
**						2: redisplay the secondary view
**						3: just update the secondary vport without 
**							redisplay the segment
**				  
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_update_secondview(flag)
int flag;
{
	int i,j,status;
	UV_view view, update_view;
	UV_vport vport;
/*
.....check if the secondary view is active, if yes, update it
*/
	if (uv_getvnm("Extern Unibase", &update_view) != UU_SUCCESS)
		return;
	for (j = 0; j < UV_no_act_screens; j++)
	{
		for (i = 0; i < UV_act_screen[j].nvports; i++)
		{
			status = uv_getvpid(UV_act_screen[j].vports[i], &vport);
			status = uv_getvid(UV_act_vports[j][i].view, &view);
			if (status == UU_SUCCESS)
			{
				if (view.key == update_view.key)
				{
					if (flag==1)
					{
						uv_delvp(&vport);
					}
					else
					{
						uv_updatevp(&vport, UU_TRUE);
/*
.....don't know why but call this instead of uv_dispobjs2 will have error will delete
.....viewport later
*/
/*						uv_dispvp(&vport); */
					}
				}
			}
		}
	}
	if (flag==2)
	{
		uv_dispobjs2();
	}
	ud_updatews(UG_SUPPRESS);
}
/**************************************************************************
**  E_FUNCTION:  uv_upd_scdv(flag)
**			Update the secondary view display, fortran callable funtion.
**			
**  PARAMETERS   
**      INPUT  :  flag: 1: delete all segment in secondary view
**						2: redisplay the secondary view
**						3: just update the secondary vport without 
**							redisplay the segment
**				  
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_upd_scdv(flag)
int *flag;
{
	uv_update_secondview(*flag);
}
