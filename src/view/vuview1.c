/*********************************************************************
**    NAME         :  vuview1.c
**       CONTAINS: User interface routines for modifying view data
**						for a viewport in a screen
**			uvu_change_view()
**			uvu_snap_view()
**			uvu_swap_view()
**			uvu_pickvp(vport)
**			uvu_vwpnorm(option)
**			uvu_vwrefpt()
**			uvu_vppan()
**			uvu_vpzoomout()
**			uvu_vpzoomin()
**			uvu_scale_xy()
**			uvu_vpshaded()
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**   MODULE NAME AND RELEASE LEVEL 
**      vuview1.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**      04/29/15 , 15:11:59
**************************************************************************/

#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "usysdef.h"
#include "uims.h"
#include "dasnog.h"
#include "dinput.h"
#include "drubber.h"
#include "modef.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "dmark.h"
#include "view.h"
#include "lipv.h"

#define NO_DEFAULT 0
#define DEFAULT 1
#define NCLIPV_VPORT 19

extern int UZ_nclipv_view;
static char Slast_viewname[15]={"Front"};
static UU_LOGICAL Sactive_vp=UU_FALSE;
static UV_vport Sactive_vport;
extern int NCL_mouse_func;
/***********************************************************************
**  E_FUNCTION:  uvu_change_view()
**			Allow the user to change the viewing parameters defined in
**			the change view form (reference point, normal, up vector).
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void uvu_change_view()

	{
	UV_vport vport;
	UV_view view;
	UU_REAL old_aperture;
	UU_LOGICAL drw_vp_info;

	uu_denter(UU_MTRC,(us,"uv_change_view()"));

	if (uvu_pickvp(&vport) == UU_SUCCESS)
	{
		uv_getvid(vport.cur_view, &view);
		/* Save the view parameters before changing. kathy */
		nclu_update_pre_view(view);

		old_aperture = view.cur_aperture;
		if(uvu_change_view_form(&view) == UU_SUCCESS)
		{
			view.modified = UU_TRUE;
			/* NCL:changed the flag to true to get rid of the TEK error. kathy */
			/*drw_vp_info = (old_aperture != view.cur_aperture);*/
			drw_vp_info = UU_TRUE;
			uv_delete_hidden(&vport);
			uv_autofact7_redrawvp(&vport, &view, drw_vp_info);
		}
	}
	uu_dexit;
	}

/***********************************************************************
**  E_FUNCTION:  uvu_snap_view()
**			Allow the user to change the viewing parameters according to
**			the user defined plane.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void uvu_snap_view()

	{
	UV_vport vport;
	UV_view view;
	UD_PLANE_REC   plane;				/* data return buffer */
	int numint;							/* number of DAS entries returned */
	UM_vector	norm, up;
	UM_vector	xaxis, yaxis;

	uu_denter(UU_MTRC,(us,"uvu_snap_view()"));

	if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
		uv_getvid(vport.cur_view, &view);

		/* Save the view parameters before changing. kathy */
		nclu_update_pre_view(view);

		um_getcpln(plane.org, xaxis, yaxis, plane.normal_vector);
		plane.transform = 0;
   	ud_get_plane(UM_MODEL, 182, &plane, 1, &numint, UD_DEFAULT);
		if (numint <= 0) goto done;    

		um_unitvc(plane.normal_vector, norm);
		if (um_vcparall(norm, view.cur_up_vect))
			{ 
		  	uu_uerror0(/* normal vector cannot be parallel to the up
						  vector */ UM_MODEL,209);
		   goto done;
		  	}
		else
		  	{
			um_cross(view.cur_up_vect, norm, up);
			um_unitvc(up,up);
			um_cross(norm, up, up);
			um_unitvc(up,up);
		  	}

		uu_dprint(UU_MTRC,(us,"view.norm=%g,%g,%g; up=%g,%g,%g",
			view.cur_pln_norm[0], view.cur_pln_norm[1], view.cur_pln_norm[2],
			view.cur_up_vect[0], view.cur_up_vect[1], view.cur_up_vect[2]));
		uu_dprint(UU_MTRC,(us,"plane.norm=%g,%g,%g; up=%g,%g,%g; norm=%g,%g,%g",
			plane.normal_vector[0], plane.normal_vector[1], plane.normal_vector[2],
			up[0],up[1],up[2],norm[0],norm[1],norm[2]));

		uv_update_up_vect(&view, up);
		uv_update_pln_norm(&view, norm);
		uv_update_ref_pt(&view, plane.org);
		view.modified = UU_TRUE;
		uv_delete_hidden(&vport);
		uv_autofact7_redrawvp(&vport, &view, UU_FALSE);
	}
done:
	uu_dexit;
	}

/***********************************************************************
**  E_FUNCTION:  uvu_swap_view()
**			Allow the user to swap the visible and invisible views.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void uvu_swap_view()
{
	int status;
	UV_view view;
/*
.....Get the active viewport and view
*/
	uv_get_sview(&Sactive_vport,&view);
/*
.....Save this view
*/
	nclu_update_pre_view(view);
/*
.....Invisible view is active
.....restore previous view
*/
	Sactive_vp = UU_TRUE;
	if (view.vtype == UV_INVISIBLE_VIEW)
		uvu_loadv(Slast_viewname,UU_FALSE);
/*
.....Load Invisible view
*/
	else
	{
		status = uvu_loadv("Invisible",UU_FALSE);
		if (status == UU_SUCCESS) strcpy(Slast_viewname,view.name);
	}
/*
.....Reset view settings
*/
	nclu_reset_prev(UU_FALSE);
}

/**************************************************************************
**  E_FUNCTION:  uvu_pickvp(vport)
**      Pick a viewport to be changed, if there is only one screen and
**			one viewport on it then no user action is required, otherwise
**			the user picks a point inside one of the viewports
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  vport : viewport picked
**  RETURNS      :	UU_SUCCESS			:	if a viewport was picked
**							UU_FAILURE	: otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uvu_pickvp(vport)
	UV_vport *vport;

	{
	int stat,save_mouse;
	UD_NDCLOCREC ploc;
	int numint;
	
	uu_denter(UU_MTRC,(us,"uvu_pickvp()"));
/*
.....NCLIPV viewing is active
*/
	if (UZ_nclipv_view == 1)
	{
		stat = uv_getvpid(LW_vport.key,vport);
	}
	else
	{
		if (NCL_mouse_func)
		{
			return uv_get_vport_atcursor(vport);
		}
		stat = UU_FAILURE;
		if ((UV_act_screen[0].nvports == 1) && (UV_no_act_screens == 1))
			stat = uv_getvpid(UV_act_screen[0].vports[0], vport);
		else if (Sactive_vp)
		{
			*vport = Sactive_vport;
			Sactive_vp = UU_FALSE;
			stat = UU_SUCCESS;
		}
		else
		{
			save_mouse = NCL_mouse_func;
			ud_dtcord(UD_LOCATOR, 1, 1);
			while (stat == UU_FAILURE)
			{
				ud_ldas(UD_DASNDC,/* pick viewport */UM_MODEL,196,
					&ploc,1,&numint,UD_NODEFAULT) ;
				if (numint != 0) 
					stat = uv_pickvp(ploc.cord[0], ploc.cord[1], vport);
			}
			NCL_mouse_func = save_mouse;
		}
	}
	uu_dexit;
	return(stat);
}

/**************************************************************************
**  E_FUNCTION:  uvu_vwpnorm(option)
**      Change the view normal of a view contained in a viewport in an
**			active screen
**  PARAMETERS   
**      INPUT  :  option		= 0 view from X
**										= 1 view from Y
**										= 2 view from Z
**										= 3 user specfies normal
**										= 4 view from NEG-X
**										= 5 view from NEG-Y
**										= 6 view from NEG-Z
**										= 7 flip view
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uvu_vwpnorm(option)
	int option;

	{
	UM_vector norm;
	UM_vector up;
	UV_vport vport;
	UV_view view;
	int numint;
	int status;

	uu_denter(UU_MTRC,(us,"uvu_vwpnorm(option = %d)",option));

	status = 0;
	
	if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
		uv_getvid(vport.cur_view, &view);
		/* Save the view parameters before changing. kathy */
		nclu_update_pre_view(view);

		if (option == 0)
		{
			um_vctovc(UM_xaxis, norm);
			um_vctovc(UM_zaxis, up);
		}
		else if (option == 1)
		{
			um_vctovc(UM_zaxis, up);
			/*um_vctmsc(up, (UU_REAL) -1.0, up);*/
			um_vctovc(UM_yaxis, norm);
		}
		else if (option == 2)
		{
			um_vctovc(UM_yaxis, up);
			um_vctovc(UM_zaxis, norm);
		}
		else if (option == 3)
		{
			ud_ldas(UD_DASVEC, /* enter view normal */ UM_MODEL, 87,
				norm, 1, &numint, UD_NODEFAULT);
			if (numint != 0)
			{
				um_unitvc(norm, norm);
				if (um_vcparall(norm, view.cur_up_vect))
				{
					uu_uerror0(/* normal vector cannot be parallel to the up
									  vector */ UM_MODEL,209);
					status = -1;
				}
				else
				{
					um_cross(view.cur_up_vect, norm, up);
					um_unitvc(up,up);
					um_cross(norm, up, up);
					um_unitvc(up,up);
				}
			}
		}
		else if (option == 4)
		{
			um_vctmsc(UM_xaxis, (UU_REAL) -1.0, norm);
			um_vctovc(UM_zaxis, up);
		}
		else if (option == 5)
		{
			um_vctmsc(UM_yaxis, (UU_REAL) -1.0, norm);
			um_vctovc(UM_zaxis, up);
		}
		else if (option == 6)
		{
			um_vctmsc(UM_zaxis, (UU_REAL) -1.0, norm);
			um_vctovc(UM_yaxis, up);
		}
		else if (option == 7)
		{
			um_vctmsc(view.cur_pln_norm, (UU_REAL) -1.0, norm);
			um_vctovc(view.cur_up_vect, up);
		}
		if (status == 0)
		{
			uv_update_up_vect(&view, up);
			uv_update_pln_norm(&view, norm);
			view.modified = UU_TRUE;
			uv_delete_hidden(&vport);
			uv_autofact7_redrawvp(&vport, &view, UU_FALSE);
		}
	}
	uu_dexit;
}

/**************************************************************************
**  E_FUNCTION:  uvu_vwrefpt()
**      Change the reference point of a view contained in a viewport in an
**			active screen
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uvu_vwrefpt()

	{
/**	UM_coord point; **/
    UD_NDCLOCREC point;

	UV_vport vport;
	UV_view view;
	int numint;

	uu_denter(UU_MTRC,(us,"uvu_vwrefpt()"));

	if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
		ud_ldas(UD_DASCART, /* enter view reference point */ UM_MODEL, 86,
			&point, 1, &numint, UD_NODEFAULT);
		if (numint != 0)
			{
			uv_getvid(vport.cur_view, &view);
			/* Save the view parameters before changing. kathy */
			nclu_update_pre_view(view);

			uv_update_ref_pt(&view, &point);
			view.modified = UU_TRUE;
			uv_delete_hidden(&vport);
			uv_autofact7_redrawvp(&vport, &view, UU_FALSE);
			}
		}
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uvu_vppan()
**			Prompt the user for a viewport and a new center for the view.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uvu_vppan()

	{
	UV_vport vport;						/* view port structure */
	UV_view view;							/* view structure */
	int numint;
	UD_NDCLOCREC ndcloc;
	UU_REAL loc[3],norm[3];
	int button,event;

	uu_denter( UU_MTRC,(us,"uvu_vppan()"));

	if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
		if (UZ_nclipv_view == 1)
		{
			event = ul_ipv_locate("Indicate lower left corner",2,loc,norm,&button);
			if (button == 1) numint = 1;
			else numint = 0;
		}
		else
		{
			ud_ldas(UD_DASNDC, /*enter window center*/UM_MODEL, 82, &ndcloc,
					1, &numint, UD_NODEFAULT);
		}
		if (numint > 0)
			{
			uv_getvid(vport.cur_view, &view);
			/* Save the view parameters before changing. kathy */
			nclu_update_pre_view(view);

			if (UZ_nclipv_view != 1)
			{
				ndcloc.transform = vport.xform;
				um_ploctocc(&ndcloc, loc);
			}
			uv_update_ref_pt(&view, loc);
			view.modified = UU_TRUE;
			uv_delete_hidden(&vport);
			uv_autofact7_redrawvp(&vport, &view, UU_FALSE);
			}
		}
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uvu_vpzoomout()
**			Prompt the user for a viewport and a magnification factor for the
**			view.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uvu_vpzoomout()

	{
	UU_REAL magnification;							/* magnification factor */
	int numint;
	UV_vport vport;									/* view port */
	UV_view view;										/* view */
	UU_REAL aperture;									/* x length of view */
	int isav;

	uu_denter( UU_MTRC,(us,"uvu_zoomout()"));

	isav = UZ_nclipv_view;
	if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
		ud_ldas(UD_DASUNITLESS,/*enter magnification factor */
					UM_MODEL,85,&magnification, 1,&numint,UD_NODEFAULT);
		if (numint > 0)
			{
			if (magnification < UM_FUZZ)
				uu_uerror0(/*magnification factor must be greater than zero*/
							UM_MODEL,117);
			else
				{
				uv_getvid(vport.cur_view, &view);
				/* Save the view parameters before changing. kathy */
				nclu_update_pre_view(view);

				aperture = view.cur_aperture / magnification;
				uv_update_vaperture(&view, aperture);
				view.modified = UU_TRUE;
				UZ_nclipv_view = isav;
				uv_delete_hidden(&vport);
				uv_autofact7_redrawvp(&vport, &view, UU_TRUE);
				}
			}
		}
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uvu_vpzoomin()
**			Prompt the user for a viewport and the lower left and upper right
**			corners of a new view.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uvu_vpzoomin()
	{
	UU_REAL llf[3];
	UU_REAL urb[3];
	UD_NDCLOCREC ndcll;
	UD_NDCLOCREC ndcur;
	UU_REAL midpt[3];
	UV_vport vport;
	UV_view view;
	int numint;
	int i;
	UU_REAL aspect_ratio;					/* aspect ratio of current viewport */
	UU_REAL wxlen;								/* x length of view port */
	UU_REAL wylen;								/* y length of view port */
	UU_REAL aperture;							/* x length of view */
	UD_RUBBER rubber;							/* rubber box control block */
	int curr_scrn;
	Gnrect *gr_area;
	int button,event;
	UM_vector norm;
	int ifl;
	UM_transf tf;

	uu_denter(UU_MTRC,(us,"uvu_vpzoomin()"));
/*
.....Get MODSYS matrix if NCLIPV is active
*/
	if (UZ_nclipv_view == 1) ncl_mcstf(&ifl,tf);

	if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
		curr_scrn = UD_curlayout.curr_screen;
		gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);
		if (UZ_nclipv_view == 1)
		{
			RUBBER_BOX(&rubber);
			event = ul_ipv_locate("Indicate lower left corner",2,llf,norm,&button);
			if (button == 1)
			{
				numint = 1;
				if (ifl) um_cctmtf(llf,tf,llf);
			}
			else numint = 0;
		}
		else
		{
			RUBBER_BOX(&rubber);
			ud_ldas(UD_DASNDC, /* enter lower left */ UM_MODEL, 83,
				&ndcll, 1, &numint, UD_NODEFAULT);
		}
		if (numint != 0)
			{
			if (UZ_nclipv_view == 1)
			{
				RUBBER_ON(&rubber);
				event = ul_ipv_locate("Indicate upper right corner",5,urb,norm,&button);
				if (button == 1)
				{
					numint = 1;
					if (ifl) um_cctmtf(urb,tf,urb);
				}
				else numint = 0;
			}
			else
			{
				RUBBER_ON(&rubber);
				ud_ldas(UD_DASNDC,/* enter upper right */ UM_MODEL, 84,
					&ndcur, 1, &numint, UD_NODEFAULT);
			}
			if (numint != 0)
				{
				/* get the corresponding view */
				uv_getvid(vport.cur_view, &view);

				/* Save the view parameters before changing. kathy */
				nclu_update_pre_view(view);

				if (UZ_nclipv_view != 1)
				{
					um_ploctocc(&ndcll, llf);
					um_ploctocc(&ndcur, urb);
				}

				/* get the new midpoint */
				for (i = 0; i < 3; i++) midpt[i] = (urb[i] + llf[i]) / 2.;
				uv_update_ref_pt(&view, midpt);

				/* get the current aspect ratio */

				aspect_ratio = ((vport.urb[1] - vport.llf[1]) *
									(gr_area->ur.y - gr_area->ll.y))
								 / ((vport.urb[0] - vport.llf[0]) *
									(gr_area->ur.x - gr_area->ll.x));

				/* get the new aspect ratio */
				wxlen = fabs(urb[0] - llf[0]) ;
				wylen = fabs(urb[1] - llf[1]) ;
				if (aspect_ratio > 0.0) wylen /= aspect_ratio;
				aperture = (wxlen < wylen)? wylen: wxlen;
				if (UZ_nclipv_view == 1) aperture *= 1.1;

				uv_update_vaperture(&view, aperture);

				view.modified = UU_TRUE;
				uv_delete_hidden(&vport);
				uv_autofact7_redrawvp(&vport, &view, UU_TRUE);
				}
			}
			RUBBER_OFF(&rubber);
		}
	uu_dexit;
	}



/**************************************************************************
**  E_FUNCTION:  uvu_scale_xy()
**			Prompt the user for a viewport and the new view's X & Y size. Then
**			display the new view.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uvu_scale_xy()

{
	UU_REAL xscale, yscale;
	Gwrect3 window;
	Gnrect3 viewport;
	UV_vport vport;
	UV_view view;
	int numint;
	int curr_scrn;
	Gnrect *gr_area;

	uu_denter(UU_MTRC,(us,"uvu_scale_xy()"));

	if (uvu_pickvp(&vport) == UU_SUCCESS)
	  {
		curr_scrn = UD_curlayout.curr_screen;
		gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);
		ud_ldas(UD_DASVAL, /* X length*/ UM_MODEL, 179,
			  											 &xscale, 1, &numint, UD_NODEFAULT); 
		if (numint != 0)
		  {
			ud_ldas(UD_DASVAL,/* Y length*/ UM_MODEL, 180, 
					  									&yscale, 1, &numint, UD_NODEFAULT);
			if (numint != 0)
			  {
								/* get the corresponding view */
				uv_getvid(vport.cur_view, &view);


				/* Save the view parameters before changing. kathy */
				nclu_update_pre_view(view);

				gsvref3(vport.xform, view.cur_ref_pt);	 /* set the reference point*/
				gsvpn3 (vport.xform, view.cur_pln_norm);/* set the view plane norm*/
				gsvup3 (vport.xform, view.cur_up_vect); /* set the view up vector */
				window.llf.x = -(xscale/2.);
				window.llf.y = -(yscale/2.);
				window.llf.z = view.cur_front_clip - view.cur_ref_pt[2];
				window.urb.x = xscale/2.0;
				window.urb.y = yscale/2.0;
				window.urb.z = view.cur_back_clip - view.cur_ref_pt[2];
				gswindow3(vport.xform, &window);
				uv_calcvp(&vport, &viewport);
				gsview3(vport.xform, &viewport);

				view.cur_aperture = xscale;
				view.modified = UU_TRUE;
				uv_putv(&view);
				uv_drawvp(&vport);
			 }
		 }
	  }
	uu_dexit;
} /* uvu_scale_xy */

/**************************************************************************
**  E_FUNCTION:  uvu_vpshaded(flag)
**			Prompt the user for a viewport and then set the shaded attribute.
**  PARAMETERS   
**      INPUT  :  mode   = 1 = Wireframe
**                         2 = Shaded
**                         3 = Hidden Line
**                         4 = Shaded Only
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uvu_vpshaded(mode)
int mode;

{
	UU_LOGICAL chg;
	UV_vport vport;
/*
.....Get the viewport
*/
	if (uvu_pickvp(&vport) == UU_SUCCESS)
	{
/*
.....Determine if setting has changed
*/
		chg = UU_FALSE;
		if (mode != 4 && mode != vport.disp_mode) chg = UU_TRUE;
		if (mode == 4 && vport.wireframe == 1) chg = UU_TRUE;
		if (mode == 2 && vport.wireframe == 0) chg = UU_TRUE;
/*
.....Modify the viewport
*/
		if (chg)
		{
			vport.disp_mode = mode;
			if (mode == 4)
			{
				vport.wireframe = 0;
				vport.disp_mode = 2;
			}
			else vport.wireframe = 1;
			uv_delvp(&vport);
			uv_putvp(&vport);
			uv_updatevp(&vport,UU_TRUE);
/*
.....Redisplay the viewport
*/
			uv_dispobjs();
/*
....Display the grid
*/
			if (vport.cur_view == UM_cpln.grid.viewid)
				um_actgrid(vport.key,vport.cur_view);
		}
	}
}
/*
.....this function is not used by NCL but used by NCLIPV application
.....in file lipvview.c
*/
uv_getdef_ipvvw(){}
