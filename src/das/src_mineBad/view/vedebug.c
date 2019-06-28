
/*********************************************************************
**    NAME         :  vedebug.c
**       CONTAINS: Routines to print viewing structures
**			uv_print_screen(screen)
**			uv_print_viewport(vport)
**			uv_print_view(view)
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**   MODULE NAME AND RELEASE LEVEL 
**       vedebug.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:57
**************************************************************************/
#include "usysdef.h"
#include "mdebug.h"
#include "view.h"

/*********************************************************************
**  E_FUNCTION:  uv_print_screen(screen)
**			Print the contents of a screen entity.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_print_screen(screen)
	UV_screen *screen;

	{
	int i;
	UV_vport vport;
	UV_view view;
	UU_KEY_ID key_id;

	sprintf(UM_sbuf,"SCREEN %x", screen->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf,"name: %s", screen->name);
	um_pscroll(UM_sbuf);

	um_p_ary(UM_PINT,"active",&screen->active);
	um_p_ary(UM_PINT,"wstation",&screen->wstation);
	um_p_ary(UM_PINT,"nvports",&screen->nvports);
	um_p_ary(UM_PINT,"viewports",screen->nvports, screen->vports);

	}

/**************************************************************************
**  E_FUNCTION:  uv_print_viewport(vport)
**		Print the contents of a viewport entity.
**  PARAMETERS   
**      INPUT  :  vport
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_print_viewport(vport)
	UV_vport *vport;

	{
	UV_view view;
	UU_KEY_ID key_id;

	key_id = vport->cur_view;
	uv_getvid(key_id, &view);

	sprintf(UM_sbuf,"VIEWPORT entity: key = %x", vport->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "vport name: %s ", vport->name);
	um_pscroll(UM_sbuf);

	um_p_ary(UM_PINT,"xform", 1, &vport->xform);
	um_p_ary(UM_PFLOAT,"llf", 3, vport->llf);
	um_p_ary(UM_PFLOAT,"urb", 3, vport->urb);
	um_p_ary(UM_PHEX,"cur_view", 1, &vport->cur_view);
	um_p_ary(UM_PINT,"disp_prio", 1, &vport->disp_prio);
	um_p_ary(UM_PINT,"input_prio", 1, &vport->input_prio);
	um_p_ary(UM_PINT,"disp_all", 1, &vport->disp_all);

	}

/**************************************************************************
**  E_FUNCTION:  uv_print_view(view)
**		Print the contents of a view entity.
**  PARAMETERS   
**      INPUT  :  view
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_print_view(view)
	UV_view *view;

	{

	sprintf(UM_sbuf,"View key=%x", view->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "name:  %s ", view->name);
	um_pscroll(UM_sbuf);

	um_p_ary(UM_PINT,"vtype", 1, &view->vtype);
	um_p_ary(UM_PINT,"can_save", 1, &view->can_save);
	um_p_ary(UM_PINT,"modified", 1, &view->modified);
	um_p_ary(UM_PINT,"projection", 1, &view->projection);

	um_p_ary(UM_PFLOAT,"cur refpt", 3, view->cur_ref_pt);
	um_p_ary(UM_PFLOAT,"cur normal", 3, view->cur_pln_norm);
	um_p_ary(UM_PFLOAT,"cur upvec", 3, view->cur_up_vect);
	um_p_ary(UM_PFLOAT,"cur eyedist", 1, &view->cur_eye_dist);
	um_p_ary(UM_PFLOAT,"cur aperture", 1, &view->cur_aperture);
	um_p_ary(UM_PFLOAT,"cur fclip", 1, &view->cur_front_clip);
	um_p_ary(UM_PFLOAT,"cur bclip", 1, &view->cur_back_clip);
	um_p_ary(UM_PINT,"cur doclip", 1, &view->cur_do_clip);

	um_p_ary(UM_PFLOAT,"sav refpt", 3, view->sav_ref_pt);
	um_p_ary(UM_PFLOAT,"sav normal", 3, view->sav_pln_norm);
	um_p_ary(UM_PFLOAT,"sav upvec", 3, view->sav_up_vect);
	um_p_ary(UM_PFLOAT,"sav eyedist", 1, &view->sav_eye_dist);
	um_p_ary(UM_PFLOAT,"sav aperture", 1, &view->sav_aperture);
	um_p_ary(UM_PFLOAT,"sav fclip", 1, &view->sav_front_clip);
	um_p_ary(UM_PFLOAT,"sav bclip", 1, &view->sav_back_clip);
	um_p_ary(UM_PINT,"sav doclip", 1, &view->sav_do_clip);

	}
