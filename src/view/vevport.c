/*********************************************************************
**    NAME         :  vevport.c
**       CONTAINS:  Routines to manipulate viewports
**			uv_pickvp(ploc, vport)
**			UU_KEY_ID uv_vpdefine(name, vport)
**			uv_setvpmot(vport, motion)
**			uv_setvpvaxis(vport, v_axis_on)
**			uv_setvpbord(vport, bord_on)
**			uv_setvpname(vport, name_on)
**			uv_setvpbox(vport, llf, urb)
**			uv_setvpvert(vport, vertices, nverts)
**			uv_setvpprio(vport, disp_prio, input_prio)
**			uv_setvpdisp(viewport, all)
**			uv_setvpmot(vport, motion)
**			uv_setvpshad(vport, shade)
**			uv_vtovp(vport, view)
**			uv_getxform(vport, xform)
**			uv_getvinfo(vport, ref_pt, view_norm, up_vect)
**			uv_getvnorm(vport, view_norm)
**			uv_getrefpt(vport, ref_pt)
**			uv_getupvect(vport, up_vect)
**			uv_updatevp(vport, display)
**			uv_setxform(vport)
**			uv_drawvp(vport)
**			uv_drawborder(vport)
**			uv_drawname(vport)
**			uv_drawvaxis(vport,len)
**			uv_drawaperture(vport)
**			uv_getvpnm(name, vport)
**			uv_getvpid(key, vport)
**			uv_getvofvp(vport, view)
**			uv_vprename(vport, name)
**			uv_putvp(vport)
**			uv_reset_view_key(vport)
**			uv_resetv(vport)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       vevport.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       02/08/16 , 09:19:02
**************************************************************************/

#include "udebug.h"
#include "uhep.h"
#include "usysdef.h"
#include "umath.h"
#include "uims.h"
#include "go.h"
#include "dasnog.h"
#include "math.h"
#include "mdrel.h"
#include "mdcpln.h"
#include "mdattr.h"
#include "view.h"
#include	"gerrorst.h"
#include "usysg.h"
#include "gdidd.h"
#include "gtblvar4.h"
#include "ginqxf.h"
#include "ginqatt.h"
#include "xenv1.h"
#include "lipv.h"

extern int UV_current_sview;
extern int UR_active;
extern char UBopen[100];
extern int lub2;
//temp
static Gnrect Sdirty_box = {10000.,10000., -10000.,-10000.};

int ug_cur_vp = -1;
#define	UM_MAX(A, B)	((A)>(B)?(A):(B))
#define	UM_MIN(A, B)	((A)<(B)?(A):(B))

void uv_setxform();
void uv_calcvp();
void uv_drawvp();
void uv_drawborder();
void uv_drawname();
void uv_drawvaxis();
void uv_drawaperture();
void uv_putvp();
extern int UZ_nclipv_view;
/**************************************************************************
**  E_FUNCTION:  uv_get_vport_atcursor(vport)
**			get viewport at the mouse cursor
**			
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :
**				vport					viewport at the cursor
**  RETURNS      :	UU_SUCCESS			:	if a viewport was picked
**							UU_FAILURE	: otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_get_vport_atcursor(vport)
UV_vport *vport;
{
	int stat;
	int x,y,z;
	int rast[2];
	Gfloat ndc[2];
	if (UZ_nclipv_view == 1)
	{
		stat = uv_getvpid(LW_vport.key,vport);
	}
	else
	{
		uw_get_curdc_xy(0, &x, &y);
		rast[0] = x; rast[1] = y;
		uw_gldevtondc(rast, ndc);
		stat = uv_pickvp(ndc[0], ndc[1], vport);
	}
	return(stat);
}
/**************************************************************************
**  E_FUNCTION:  uv_pickvp(xndc, yndc, vport)
**			Given a pick location (PLOC), determine which viewport was
**			picked.
**  PARAMETERS   
**      INPUT  :  
**				xndc					x coordinate (NDC location)
**				yndc					y coordinate (NDC location)
**      OUTPUT :
**				vport					viewport picked
**  RETURNS      :	UU_SUCCESS			:	if a viewport was picked
**							UU_FAILURE	: otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_pickvp(xndc, yndc, vport)
	UU_REAL xndc;
	UU_REAL yndc;
	UV_vport *vport;

	{
	int stat;
	int i;
	int curr_scrn;
	Gnrect *gr_area;
	UU_REAL lleft[2], uright[2];
	
	uu_denter(UU_MTRC,(us,"uv_pickvp()"));

	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	stat = UU_FAILURE;
	if ((UV_act_screen[0].nvports == 1) && (UV_no_act_screens == 1))
		stat = uv_getvpid(UV_act_screen[0].vports[0], vport);
	else
		{
		for (i = 0; i < UV_act_screen[0].nvports; i++)
			{
			uv_getvpid(UV_act_screen[0].vports[i], vport);
			lleft[0] = vport->llf[0] * (gr_area->ur.x - gr_area->ll.x) +
						  gr_area->ll.x;
			lleft[1] = vport->llf[1] * (gr_area->ur.y - gr_area->ll.y) +
						  gr_area->ll.y;
			uright[0] = vport->urb[0] * (gr_area->ur.x - gr_area->ll.x) +
							gr_area->ll.x;
			uright[1] = vport->urb[1] * (gr_area->ur.y - gr_area->ll.y) +
							gr_area->ll.y;
			if ((xndc >= lleft[0]) && (xndc <= uright[0]) &&
				(yndc >= lleft[1]) && (yndc <= uright[1]))
				{
				stat = UU_SUCCESS;
				break;
				}
			}
		}

	uu_dexit;
	return(stat);
	}

/**************************************************************************
**  E_FUNCTION:  UU_KEY_ID uv_vpdefine(name, vport, flag)
**      Define a default viewport and create the viewport entity in Unibase
**  PARAMETERS   
**      INPUT  :  name	: name to be given to viewport
**						vport	: pointer to a UV_viewport structure
**                flag  : 1 - Overwrite the viewport.
**                        0 - Do not overwrite.
**      OUTPUT :  none
**  RETURNS      :  key of viewport defined
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
UU_KEY_ID uv_vpdefine(name, vport, flag)
char name[15];
UV_vport *vport;
int flag;
{
	int i;
	UV_vport oldvport;

	uu_denter(UU_MTRC,(us,"uv_vpdefine(name=%s)",name));

	if (uv_getvpnm(name, &oldvport)==UU_SUCCESS)
	{
/*
.....if not overwrite, the not define vport, return -1;
*/
		if (flag==0)
			return -1;
/*
......remove this vport
*/
		else
			ur_delete_all(oldvport.key);
	}
	strcpy(vport->name, name);

	vport->llf[0] = 1.0;
	vport->llf[1] = 1.0;
	vport->llf[2] = 1.0;

	vport->urb[0] = 1.0;
	vport->urb[1] = 1.0;
	vport->urb[2] = 1.0;

	vport->cur_view = 0;

	vport->disp_prio = 0;
	vport->input_prio = 0;

	vport->disp_all = UU_TRUE;

	vport->bord_seg = -1;
	vport->aperture_on = 0;
	vport->v_axis_on = 0;
	vport->name_on = 0;
	vport->bord_on = UU_TRUE;
	vport->nverts = 0;
	for (i=0; i<60; i++) vport->vertices[i] = 0.0;
	vport->grid_seg = -1;
	vport->grid_on = UU_FALSE;
	vport->motion = UU_TRUE;

	vport->rel_num = UV_VPORT_REL;
	vport->xform = -1;

	ur_create_data(vport);

	uu_dexit;
	return(vport->key);
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvpmot(vport, motion)
**      Set the enabling of the viewport motion display
**  PARAMETERS   
**      INPUT  :  vport		: pointer to a viewport structure
**                motion	: if > 0 display viewport motion display
**      OUTPUT :  vport : changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpmot(vport, motion)
	UV_vport  *vport;
	UU_LOGICAL motion;
	{
	vport->motion = motion;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvpshad(vport, shade)
**      Set the viewport shading mode.
**  PARAMETERS   
**      INPUT  :  vport		: pointer to a viewport structure
**                motion	: if > 0 display viewport motion display
**      OUTPUT :  vport : changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpshad(vport, shade)
	UV_vport  *vport;
	UU_LOGICAL shade;
	{
	vport->disp_mode = shade;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvpframe(vport, wireframe)
**      Set the viewport shading mode.
**  PARAMETERS   
**      INPUT  :  vport		: pointer to a viewport structure
**                wireframe	: wireframe mode to be set
**      OUTPUT :  vport : changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpframe(vport, wireframe)
	UV_vport  *vport;
	UU_LOGICAL wireframe;
{
	vport->wireframe = wireframe;
}

/**************************************************************************
**  E_FUNCTION:  uv_setvpvaxis(vport, v_axis_on)
**      Set whether to display the view axis in this viewport 
**  PARAMETERS   
**      INPUT  :  vport		: pointer to a viewport structure
**						v_axis_on: logical specifying view axis on or off
**      OUTPUT :  vport : changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpvaxis(vport, v_axis_on)
	UV_vport 	 *vport;
	UU_LOGICAL	 v_axis_on;
	{
	uu_denter(UU_MTRC,(us,"uv_setvpaxis(key=%x)",vport->key));
	vport->v_axis_on = v_axis_on;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvpbord(vport, bord_on)
**      Set the enabling of the viewport border
**  PARAMETERS   
**      INPUT  :  vport		: pointer to a viewport structure
**						bord_on	: if > 0 display viewport border
**      OUTPUT :  vport : changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpbord(vport, bord_on)
	UV_vport  *vport;
	UU_LOGICAL bord_on;
	{
	uu_denter(UU_MTRC,(us,"uv_setvpbord(key=%x)",vport->key));
	vport->bord_on = bord_on;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvpname(vport, name_on)
**      Set the enalbing of the viewport name
**  PARAMETERS   
**      INPUT  :  vport		: pointer to a viewport structure
**						name_on	: if >0 display viewport name
**      OUTPUT :  vport : changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpname(vport, name_on)
	UV_vport  *vport;
	UU_LOGICAL name_on;
	{
	uu_denter(UU_MTRC,(us,"uv_setvpname(key=%x)",vport->key));
	vport->name_on = name_on;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvpbox(vport, llf, urb)
**      Set the viewport box for this viewport
**  PARAMETERS   
**      INPUT  :  vport	: pointer to a viewport structure
**						llf	: lower left front corner of viewport
**						urb	: upper right back corner of viewport
**      OUTPUT :  vport : changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpbox(vport, llf, urb)
	UV_vport  *vport;
	UU_REAL	llf[3];
	UU_REAL  urb[3];
	{

	uu_denter(UU_MTRC,(us,"uv_setvpbox(key=%x)",vport->key));
	um_vctovc(llf, vport->llf);
	um_vctovc(urb, vport->urb);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvpvert(vport, vertices, nverts)
**      Set the vertices of polygonal clipping area for this viewport
**  PARAMETERS   
**      INPUT  :  vport		: pointer to a viewport structure
**						vertices	: NDC points describing a polygonal region
**						nverts	: number of vertices
**      OUTPUT :  vport : changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpvert(vport, vertices, nverts)
	UV_vport  *vport;
	UU_REAL	vertices[];
	int		nverts;
	{
	int i;

	uu_denter(UU_MTRC,(us,"uv_setvpvert(key=%x)",vport->key));
	for (i = 0; i < nverts*2; i++)
		vport->vertices[i] = vertices[i];

	vport->nverts = nverts;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvpprio(vport, disp_prio, input_prio)
**      Set the input and output priority for this viewport
**  PARAMETERS   
**      INPUT  :  vport	: pointer to a viewport structure
**      OUTPUT :  vport : changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpprio(vport, disp_prio, input_prio)
	UV_vport  *vport;
	int		disp_prio;
	int		input_prio;
	{
	uu_denter(UU_MTRC,(us,"uv_setvpprio(key=%x)",vport->key));
	vport->disp_prio = disp_prio;
	vport->input_prio = input_prio;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_setvpdisp(viewport, all)
**      Set whether all objects will be displayed in this viewport
**  PARAMETERS   
**      INPUT  :  vport	: pointer to a viewport structure
**      OUTPUT :  vport : changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setvpdisp(vport, all)
	UV_vport   *vport;
	UU_LOGICAL all;
	{
	uu_denter(UU_MTRC,(us,"uv_setvpdisp(key=%x)",vport->key));
	vport->disp_all = all;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_vtovp(vport, view)
**      Associate this view with the viewport
**  PARAMETERS   
**      INPUT  :  vport	: pointer to viewport structure
**						view	: key of view to be associated with viewport
**      OUTPUT :  vport	: changed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_vtovp(vport, view)
	UV_vport   *vport;
	UU_KEY_ID   view;
	{
	uu_denter(UU_MTRC,(us,"uv_vtovp(vpkey=%x,vkey=%x)",vport->key,view));
	vport->cur_view = view;
	uu_dexit;
	}
/**************************************************************************
**  E_FUNCTION:  uv_getxform(vport, xform)
**      Get the transformation number for this viewport
**  PARAMETERS   
**      INPUT  :
**				vport		viewport to get transform number from
**      OUTPUT :
**				xform 	transformation number
**  RETURNS      :	UU_SUCCESS	: if viewport is currently used on a workstation
**							UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int
uv_getxform(vport, xform)
	UU_KEY_ID	vport;
	int			*xform;

	{
	int i;
	int stat;
	int j;

	uu_denter(UU_MTRC,(us,"uv_getxform(key=%x)",vport));
	stat = UU_FAILURE;
	for (j = 0; j < UV_no_act_screens; j++)
		for (i = 0; i < UV_act_screen[j].nvports; i++)
			if (vport == UV_act_vports[j][i].key)
				{
				stat = UU_SUCCESS;
				*xform = UV_act_vports[j][i].xform;
				break;
				}

	uu_dexit;
	return(stat);
	}

/**************************************************************************
**  E_FUNCTION:  uv_getvinfo(vport, ref_pt, view_norm, up_vect)
**      Retrieve the view reference point, view plane normal and
**			up vector associated with this viewport
**  PARAMETERS   
**      INPUT  :
**				vport	:	viewport to get view information from
**      OUTPUT :
**				ref_pt	:	view reference point
**				view_norm:	view plane normal
**				up_vect	:	view up vector
**  RETURNS      :	UU_SUCCESS	: if viewport is currently used on a workstation
**							UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int
uv_getvinfo(vport, ref_pt, view_norm, up_vect)
	UU_KEY_ID	vport;
	UM_coord		ref_pt;
	UM_vector	view_norm;
	UM_vector	up_vect;

	{
	int i;
	int j;
	int stat;
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_getvinfo(key=%x)",vport));
	stat = UU_FAILURE;
	for (j = 0; j < UV_no_act_screens; j++)
		for (i = 0; i < UV_act_screen[j].nvports; i++)
			if (vport == UV_act_vports[j][i].key)
				{
				stat = uv_getvid(UV_act_vports[j][i].view, &view);
				if (stat == UU_SUCCESS)
					{
					um_vctovc(view.cur_pln_norm, view_norm);
					um_vctovc(view.cur_ref_pt, ref_pt);
					um_vctovc(view.cur_up_vect, up_vect);
					}
				break;
				}

	uu_dexit;
	return(stat);
	}

/**************************************************************************
**  E_FUNCTION:  uv_getvnorm(vport, view_norm)
**      Retrieve the view plane normal associated with this viewport
**  PARAMETERS   
**      INPUT  :  vport			:	viewport to get view information from
**      OUTPUT :  view_norm	:	view plane normal
**  RETURNS      :	UU_SUCCESS	: if viewport is currently used on a workstation
**							UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int
uv_getvnorm(vport, view_norm)
	UU_KEY_ID	vport;
	UM_vector	view_norm;

	{
	int i;
	int j;
	int stat;
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_getvnorm(key=%x)",vport));
	stat = UU_FAILURE;
	for (j = 0; j < UV_no_act_screens; j++)
		for (i = 0; i < UV_act_screen[j].nvports; i++)
			if (vport == UV_act_vports[j][i].key)
				{
				stat = uv_getvid(UV_act_vports[j][i].view, &view);
				if (stat == UU_SUCCESS) um_vctovc(view.cur_pln_norm, view_norm);
			break;
			}

	uu_dexit;
	return(stat);
	}

/**************************************************************************
**  E_FUNCTION:  uv_getrefpt(vport, ref_pt)
**      Retrieve the view reference point associated with this viewport
**  PARAMETERS   
**      INPUT  :  vport	:	viewport to get view information from
**      OUTPUT :  ref_pt	:	view reference point
**  RETURNS      :	UU_SUCCESS	: if viewport is currently used on a workstation
**							UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int
uv_getrefpt(vport, ref_pt)
	UU_KEY_ID	vport;
	UM_coord		ref_pt;

	{
	int i;
	int j;
	int stat;
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_getrefpt(key=%x)",vport));
	stat = UU_FAILURE;
	for (j = 0; j < UV_no_act_screens; j++)
		for (i = 0; i < UV_act_screen[j].nvports; i++)
			if (vport == UV_act_vports[j][i].key)
				{
				stat = uv_getvid(UV_act_vports[j][i].view, &view);
				if (stat == UU_SUCCESS) um_vctovc(view.cur_ref_pt, ref_pt);
				break;
				}

	uu_dexit;
	return(stat);
	}

/**************************************************************************
**  E_FUNCTION:  uv_getupvect(vport, up_vect)
**      Retrieve the view up vector associated with this viewport
**  PARAMETERS   
**      INPUT  :  vport	:	viewport to get view information from
**      OUTPUT :  up_vect	:	view up vector
**  RETURNS      :	UU_SUCCESS	: if viewport is currently used on a workstation
**							UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int
uv_getupvect(vport, up_vect)
	UU_KEY_ID	vport;
	UM_vector	up_vect;

	{
	int i;
	int j;
	int stat;
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_getupvect(key=%x)",vport));
	stat = UU_FAILURE;
	for (j = 0; j < UV_no_act_screens; j++)
		for (i = 0; i < UV_act_screen[j].nvports; i++)
			if (vport == UV_act_vports[j][i].key)
				{
				stat = uv_getvid(UV_act_vports[j][i].view, &view);
				if (stat == UU_SUCCESS) um_vctovc(view.cur_up_vect, up_vect);
				break;
				}

	uu_dexit;
	return(stat);
	}

/**************************************************************************
**  E_FUNCTION:  uv_updatevp(vport, display)
**      Updates the screen representation of a viewport
**  PARAMETERS   
**      INPUT  :  vport	:	vport entity to be updated
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  Only to be used on viewports in an active screen
**************************************************************************/
void uv_updatevp(vport, display)
	UV_vport		*vport;
	UU_LOGICAL	display;
{
	UM_pkwin_type cx;
	uu_denter(UU_MTRC,(us,"uv_updatevp(key=%x)", vport->key));

	if ((UZ_nclipv_view == 1)|| (LW_nclipv==LW_STANDALONE))
	{
		cx = uw_glget_context();
		uw_glset_context(UM_IPV_WINDOW,UU_FALSE);
	}
	else
	{
		cx = uw_glget_context();
		uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
	}
	/* set up norm transformation */
	uv_setxform(vport);
	

	/* draw viewport border, name, axis, etc. */
/*
.....chaned for reset cutseg if has any.
.....Yurong
*/
/*
	if (display)
		uv_drawvp(vport);
*/
	if (display)
	{
		ncl_reset_cutseg(vport->xform-1);
		uv_drawvp(vport);
	}
/*
.....Reset the current graphics mode
*/
	uw_glset_context(cx,UU_FALSE);
	uu_dexit;
}
/**************************************************************************
**  I_FUNCTION:  uv_setxform(vport)
**      Use DIGS calls to set up normalization transformation.
**			Map the UU_REAL viewing data to GKS data types
**  PARAMETERS   
**      INPUT  :  vport	: viewport entity to use to set xform
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setxform(vport)
	UV_vport		*vport;
	{
	Gwrect3	window;
	Gnrect3	viewport;
	UV_view	view;
	int		xform;
	int 		stat;
	Gwpoint3	pt;
	int curr_scrn;
	Gnrect *gr_area;
	char msg[256];

	uu_denter(UU_MTRC,(us,"uv_setxform(key=%x)",vport->key));
	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);
	
	xform = vport->xform;
/*
...For resize window, we need draw 
...only if the vport has already reset
...yurong
*/
	ug_cur_vp = xform;

	stat = uv_getvid(vport->cur_view, &view);
	if (stat == UU_FAILURE)
		{
		sprintf(msg, "Viewport %s does not assoc view or it can not be found\n",
				vport->name);
		ud_printmsg(msg);
		exit(1);
		}

	/* set the reference point */
	pt.x = view.cur_ref_pt[0];
	pt.y = view.cur_ref_pt[1];
	pt.z = view.cur_ref_pt[2];
	gsvref3(xform, &pt);

	/* set the view plane normal */
	pt.x = view.cur_pln_norm[0];
	pt.y = view.cur_pln_norm[1];
	pt.z = view.cur_pln_norm[2];
	gsvpn3(xform, &pt);

	/* set the view up vector */
	pt.x = view.cur_up_vect[0];
	pt.y = view.cur_up_vect[1];
	pt.z = view.cur_up_vect[2];
	gsvup3(xform, &pt);


	/* set the window */
	uv_get_window_bounds(vport, &window.llf, &window.urb);
	gswindow3(xform, &window);
/*
.....display the leader line at the correct position before the redisplaying 
.....the view
*/
	ncl_display_ldr(vport);
	/* set the viewport box */
	uv_calcvp(vport, &viewport);
	gsview3(xform, &viewport);

	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_calcvp(vport)
**      Calculate the digs viewport size for the specified viewport.
**  PARAMETERS   
**      INPUT  :  vport	:	vport entity to be updated
**      OUTPUT :  llf	:	lower left front of viewport.
**						urb	:	upper right front of viewport.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_calcvp(vport, viewport)
UV_vport		*vport;
Gnrect3	*viewport;
{
	UM_coord	llf;							/* lower left front window bounds
														( ndc coord.) */
	UM_vector urb;							/* upper right back window bounds
														( ndc coord. */
	UM_ndc	lleft;
	UM_ndc	uright;
	Gint		ll[2], ur[2];
	Gnrect *gr_area;
	int curr_scrn;
	UU_REAL  zwidth;

	uu_denter(UU_MTRC,(us,"uv_calcvp(%x)", vport));

	/* Find the graphics area */
	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	/* set the viewport box */
	lleft[0] = vport->llf[0] * (gr_area->ur.x - gr_area->ll.x) + gr_area->ll.x;
	lleft[1] = vport->llf[1] * (gr_area->ur.y - gr_area->ll.y) + gr_area->ll.y;
	uright[0] = vport->urb[0] * (gr_area->ur.x - gr_area->ll.x) + gr_area->ll.x;
	uright[1] = vport->urb[1] * (gr_area->ur.y - gr_area->ll.y) + gr_area->ll.y;

	/* Push viewport towards center by one pixel, keeps graphics from
	 * overwriting viewport borders.
	 */
	(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DNDCDEV]) (lleft,ll,*UD_ksws);
	(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DNDCDEV]) (uright,ur,*UD_ksws);

	ll[0]++;
	ll[1]++;
	ur[0]--;
	ur[1]--;

	(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DDEVNDC]) (ll,lleft,*UD_ksws);
	(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DDEVNDC]) (ur,uright,*UD_ksws);

	/* set z length of viewport in same ratio as x */
	uv_get_window_bounds(vport, llf, urb);
	uu_dprint(UU_MTRC,(us,"window llf %f %f %f, to %f %f %f",
		llf[0], llf[1], llf[2], urb[0], urb[1], urb[2]));
/*
.....Protect against divide by zero
.....Bobby  -  5/19/94
*/
	if (urb[0] == llf[0]) zwidth = 0;
	else
		zwidth = (llf[2] - urb[2]) * (uright[0]-lleft[0]) / (urb[0] - llf[0]);

	uu_dprint(UU_MTRC,(us,"zwidth = %f",zwidth));
	lleft[2]  =  zwidth / 2.0;
	uright[2] = -zwidth / 2.0;

	viewport->llf.x = lleft[0];
	viewport->llf.y = lleft[1];
	viewport->llf.z = lleft[2];

	viewport->urb.x = uright[0];
	viewport->urb.y = uright[1];
	viewport->urb.z = uright[2];

	uu_dexit;
}

/**************************************************************************
**  E_FUNCTION:  uv_drawvp(vport)
**      Update the display for the specified viewport.
**  PARAMETERS   
**      INPUT  :  vport	:	vport entity to be updated
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  Only to be used on viewports in an active screen
**************************************************************************/
void uv_drawvp(vport)
	UV_vport		*vport;
	{
	Glntype line_style;
	int bdr_clr;
	UV_vport tmpvp;
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_drawvp(key=%x)",vport->key));

/*
.....Do not create a new segment if plotting
.....Bobby  -  7/23/91
*/
	if (!uj_miplotting())
	{
		/* delete old border if it exists */
		if (vport->bord_seg != -1)
			gdeleteseg(vport->bord_seg);

		/* create the DIGS segment */
		vport->bord_seg = gnseg();
		uv_putvp(vport);

		if( gcreateseg(vport->bord_seg) != NCL_NO_ERROR )
			uu_sys_err_recovery(/* Out of digs segments */ -1, UM_MODEL, 273, 0, 0);
		gssegdet(vport->bord_seg,UG_UNDETECTABLE);
	}

	bdr_clr = dinqgrafbrdr();
	uu_denter2(UU_MTRC,(us,"uv_drawvp: bdr_clr = %d",bdr_clr));
	uu_dexit;
/* NCL */
	gslinecolor(1);
	gslinewidth((UU_REAL) 1.0);

	line_style.typeno = UM_SOLID_LINE;
	line_style.npatn = 0;
	gslinetype(&line_style);

	gstextcolor(1);

	/* use the identiy norm tran */
	gsnormtran(0);

	/* draw view axis if desired */
	if (vport->v_axis_on == UU_TRUE)
		uv_drawvaxis(vport,(UU_REAL).25,(UU_REAL)0.,(UU_REAL)0.);

	/* draw border if desired */
	if (vport->bord_on == UU_TRUE)
	{
/*
.....Draw dynamics viewport in BLUE
.....Bobby  -  2/11/94
*/
		uv_select_sview(0);
		uv_getvid(vport->cur_view,&view);
		if (view.vtype == UV_INVISIBLE_VIEW)
		{
			gslinecolor(3);
			gslinewidth(2.0);
			uv_drawborder(vport);
			gslinecolor(1);
			gslinewidth(0.0);
		}
		else if (UV_current_sview == vport->xform)
		{
			gslinecolor(2);
			gslinewidth(2.0);
			uv_drawborder(vport);
			gslinecolor(1);
			gslinewidth(0.0);
		}
		else
		{
			uv_drawborder(vport);
		}
	}

	/* draw name if desired */
	if (vport->name_on == UU_TRUE)
		uv_drawname(vport);

	/* draw aperture if desired */
	if (vport->aperture_on == UU_TRUE)
		uv_drawaperture(vport);

	gcloseseg();

	/* set segment not pickable */
	gssegdet(vport->bord_seg, UG_UNDETECTABLE);

/*
.....If this was not active dynamic viewport
.....then redraw the active viewport
*/
	if (UV_current_sview != vport->xform && UV_no_act_screens != 0)
	{
		uv_getvpid(UV_act_screen[0].vports[UV_current_sview-1],&tmpvp);
		gslinecolor(2);
		gslinewidth(2.0);
		uv_drawborder(&tmpvp);
		gslinecolor(1);
		gslinewidth(0.0);
	}
	uu_dexit;
	}

/**************************************************************************
**  I_FUNCTION:  uv_drawborder(vport)
**      Draw the border of a viewport on the workstation
**  PARAMETERS   
**      INPUT  :  vport	:	viewport to have border drawn for
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_drawborder(vport)
	UV_vport  *vport;
	{
	int nverts;
	Gwpoint3 verts[30];
	int curr_scrn;
	Gnrect *gr_area;
	UM_ndc lleft;
	UM_ndc uright;

	uu_denter(UU_MTRC,(us,"uv_drawborder(key=%x)",vport->key));
	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	/* if no polygonal viewport region, just draw box */
	if (vport->nverts == 0)
		{
		lleft[0]= vport->llf[0] * (gr_area->ur.x - gr_area->ll.x) + gr_area->ll.x;
		lleft[1]= vport->llf[1] * (gr_area->ur.y - gr_area->ll.y) + gr_area->ll.y;
		lleft[2]= 1;
		uright[0]=vport->urb[0] * (gr_area->ur.x - gr_area->ll.x) + gr_area->ll.x;
		uright[1]=vport->urb[1] * (gr_area->ur.y - gr_area->ll.y) + gr_area->ll.y;
		uright[2] = 0;

		/*verts[0].x = vport->llf[0] + 0.0000;
		verts[0].y = vport->llf[1] + 0.0000;
		verts[0].z = 0.0000;

		verts[1].x = vport->urb[0] - 0.0000;
		verts[1].y = vport->llf[1] + 0.0000;
		verts[1].z = 0.0000;

		verts[2].x = vport->urb[0] - 0.0000;
		verts[2].y = vport->urb[1] - 0.0000;
		verts[2].z = 0.0000;

		verts[3].x = vport->llf[0] + 0.0000;
		verts[3].y = vport->urb[1] - 0.0000;
		verts[3].z = 0.0000;

		verts[4].x = vport->llf[0] + 0.0000;
		verts[4].y = vport->llf[1] + 0.0000;
		verts[4].z = 0.0000;

		nverts = 5;*/

		verts[0].x = lleft[0] + 0.0000;
		verts[0].y = lleft[1] + 0.0000;
		verts[0].z = 0.0000;

		verts[1].x = uright[0] - 0.0000;
		verts[1].y = lleft[1] + 0.0000;
		verts[1].z = 0.0000;

		verts[2].x = uright[0] - 0.0000;
		verts[2].y = uright[1] - 0.0000;
		verts[2].z = 0.0000;

		verts[3].x = lleft[0] + 0.0000;
		verts[3].y = uright[1] - 0.0000;
		verts[3].z = 0.0000;

		verts[4].x = lleft[0] + 0.0000;
		verts[4].y = lleft[1] + 0.0000;
		verts[4].z = 0.0000;

		nverts = 5;
		}

	/* else draw polygonal region */
	/*	else
		{
		for (i = 0; i < vport->nverts; i++)
			{
			verts[i].x = vport->vertices[i*3];
			verts[i].y = vport->vertices[i*3+1];
			verts[i].z = vport->vertices[i*3+2];
			}

		nverts = vport->nverts;
		}	*/

	/* draw border */
	gpolyline3(nverts, verts);

	uu_dexit;
}

/**************************************************************************
**  I_FUNCTION:  uv_drawname(vport)
**      Draw the name of the view in this viewport, NOT YET IMPLEMENTED RIGHT
**  PARAMETERS   
**      INPUT  :  vport	:	viewport to have border drawn for
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_drawname(vport)
	UV_vport  *vport;
	{
	UV_view view;
	int stat;
	UU_REAL char_height;
	UU_REAL x, y;
	char string[25];
	Gwpoint3 textpt;
	int curr_scrn;
	Gnrect *gr_area;
	UX_pathname filename,dir,fname;

	uu_denter(UU_MTRC,(us,"uv_drawname(key=%x)",vport->key));

	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	/* set the character height */
	y = (vport->urb[1] - vport->llf[1]) * (gr_area->ur.y - gr_area->ll.y);
	x = (vport->urb[0] - vport->llf[0]) * (gr_area->ur.x - gr_area->ll.x);
	char_height = UM_MIN(x, y);
	char_height = char_height / 40.;
	char_height = UM_MIN(char_height, (UU_REAL) .07);
	gscharheight(char_height);

	textpt.x = vport->llf[0] * (gr_area->ur.x - gr_area->ll.x) + gr_area->ll.x
				  + char_height;
	textpt.y = vport->llf[1] * (gr_area->ur.y - gr_area->ll.y) + gr_area->ll.y
				  + char_height;
	textpt.z = 0.0;

	stat = uv_getvid(vport->cur_view, &view);
/*
......if the view is "extenal unibase view, drawing the secondary unibase
......filename instead of just "External Unibase" as viewname
*/
	if ((view.vtype==UV_SECONDARY_VIEW)&&(UBopen[0]!='\0')&&(lub2>0))
	{
		strcpy(filename, UBopen);
		ul_break_fname(filename,dir,fname);
		sprintf(string, "External Unibase: %s", fname);
	}
	else
		sprintf(string, "%s", view.name);
	gtext(&textpt, string);

	uu_dexit;
	}

/**************************************************************************
**  I_FUNCTION:  uv_drawvaxis(vport,len,aper_in,ratio_in)
**      Draw the view axis of the viewport.  NOT YET IMPLEMENTED
**  PARAMETERS   
**      INPUT  :  vport	    :	viewport to have view axis drawn for
**                len       :  length of axis
**                aper_in   :  Window aperture in X or 0 if the aperture
**                             stored in the view should be used.
**                ratio_in  :  Ratio of Window X to Y if 'aper_in' is not
**                             set to 0.
**          
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_drawvaxis(vport,len,aper_in,ratio_in)
UV_vport  *vport;
UU_REAL len,aper_in,ratio_in;
	{
	int i;										/* indicies */
	int stat;
	UU_REAL char_height;							/* character height in view units */
	UU_REAL axis_length;							/* axis length */
	UU_REAL xoffset;								/* x offset from left edge of box */
	UU_REAL yoffset;								/* y offset from left edge of box */
	UU_REAL dx, dy;
	UU_REAL aperture;
	UU_REAL aspect_ratio;						/* aspect ratio of vport */
	UM_coord origin;								/* model coordiate system origin */
	UM_coord up_vector;
	UM_coord temp;
	UM_coord refpt;
	UM_vector xaxis;								/* view plane x axis */
	UM_vector upvect,plnnorm,mxaxis,myaxis,mzaxis;
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_drawvaxis(key=%x)",vport->key));

	if (aper_in == 0.) gsnormtran(vport->xform);
	stat = uv_getvid(vport->cur_view, &view);
/*
.....Adjust view for MODSYS
.....if coming from NCLIPV
*/
	if (aper_in != 0.)
	{
		ncl_wcstomcs(0,view.cur_ref_pt,refpt);
		ncl_wcstomcs(1,view.cur_up_vect,upvect);
		ncl_wcstomcs(1,view.cur_pln_norm,plnnorm);
		ncl_wcstomcs(1,UM_xaxis,mxaxis);
		ncl_wcstomcs(1,UM_yaxis,myaxis);
		ncl_wcstomcs(1,UM_zaxis,mzaxis);
	}
	else
	{
		um_vctovc(view.cur_ref_pt,refpt);
		um_vctovc(view.cur_up_vect,upvect);
		um_vctovc(view.cur_pln_norm,plnnorm);
		um_vctovc(UM_xaxis,mxaxis);
		um_vctovc(UM_yaxis,myaxis);
		um_vctovc(UM_zaxis,mzaxis);
	}
	/* get the view plane axis vectors */
	um_cross(upvect, plnnorm, xaxis);
	um_unitvc(xaxis, xaxis);

	/* calculate the character height (used for ofsetting the origin) */

	if (aper_in == 0.)
	{
		aperture = view.cur_aperture;
		uv_vp_aspect_ratio(vport, &aspect_ratio);
	}
	else
	{
		aperture = aper_in;
		aspect_ratio = 1. / ratio_in;
	}
	dx = aperture/2.0;
	dy = aspect_ratio * dx ;
	char_height = UM_MIN(dx, dy);
	char_height = char_height / 20.;
	char_height = UM_MIN(char_height, (UU_REAL) 14.);

	xoffset = 0.0;
	for (i = 0; i < 3; i++) if (-xaxis[i] > xoffset) xoffset = -xaxis[i] ;

	yoffset = 0.0;
	for (i = 0; i < 3; i++)
		if (-upvect[i] > yoffset)
			yoffset = -upvect[i] ;

	if (aper_in == 0.)
/*
......when aper_in == 0. the len pass in is not adjust by aspect_ratio
......and aperture, so, we don't use it directly, otherwise, the offset
......will be too off (only called by uv_drawvp so far)
*/
		axis_length = 2.5 * char_height;
	else
/*
.....only called by ul_ipv_view_segs function so far
*/
		axis_length = fabs(len);

	xoffset = xoffset * axis_length;
	yoffset = yoffset * axis_length;

	um_vctmsc(xaxis, dx, temp);
	um_vcmnvc(refpt, temp, origin);
	um_vctmsc(upvect, dy, up_vector);
	um_vcmnvc(origin, up_vector, origin);

	origin[0] = origin[0] + xaxis[0] * (xoffset + char_height)
					+ upvect[0] * (yoffset + char_height)
					+ upvect[0] * ( 2 * char_height * vport->name_on)
					+ upvect[0] * ( 2 * char_height * vport->aperture_on);

	origin[1] = origin[1] + xaxis[1] * (xoffset + char_height)
					+ upvect[1] * (yoffset + char_height)
					+ upvect[1] * ( 2 * char_height * vport->name_on)
					+ upvect[1] * ( 2 * char_height * vport->aperture_on);

	origin[2] = origin[2] + xaxis[2] * (xoffset + char_height)
					+ upvect[2] * (yoffset + char_height)
					+ upvect[2] * ( 2 * char_height * vport->name_on)
					+ upvect[2] * ( 2 * char_height * vport->aperture_on);
	if (strcmp(vport->name, "nclipv")!=0)
		um_drwaxis(origin,mxaxis,myaxis,mzaxis, len);
	else
		ul_drw_vaxis_ipv(origin,mxaxis,myaxis,mzaxis, len);

	/* restore and exit */
	if (aper_in == 0.) gsnormtran(0);
	uu_dexit;
	}
/**************************************************************************
**  I_FUNCTION:  uv_drawaperture(vport)
**      Draw the aperture size of the view in this viewport
**  PARAMETERS   
**      INPUT  :  vport	:	viewport to have border drawn for
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_drawaperture(vport)
	UV_vport  *vport;
	{
	UV_view view;
	int stat;
	UU_REAL char_height;
	UU_REAL x, y, y_size, x_size, x_scal, y_scal;
	char string[40];
	char	units[4];
	Gwpoint3 textpt;
	int curr_scrn;
	Gnrect *gr_area;
	Gwrect3 *window;

	uu_denter(UU_MTRC,(us,"uv_drawaperture(key=%x)",vport->key));

	/* set the character height */
	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	y = ((vport->urb[1] - vport->llf[1]) * (gr_area->ur.y - gr_area->ll.y));
	x = ((vport->urb[0] - vport->llf[0]) * (gr_area->ur.x - gr_area->ll.x));
	char_height = UM_MIN(x, y);
	char_height = char_height / 40.;
	char_height = UM_MIN(char_height, (UU_REAL) .07);
	gscharheight(char_height);

	textpt.x = vport->llf[0] * (gr_area->ur.x - gr_area->ll.x) + gr_area->ll.x
				  + char_height;
	textpt.y = vport->llf[1] * (gr_area->ur.y - gr_area->ll.y) + gr_area->ll.y
				  + char_height + (2 * char_height * vport->name_on);
	textpt.z = 0.0;

	stat = uv_getvid(vport->cur_view, &view);

	um_linear_units_str(UM_cpln.length_unit, units);
/*
	UM_len_inttoext(view.cur_aperture, aperture);
	y_size = aperture*(y/x);
*/
	window = gqwindow3(vport->xform);
	x_size = window->urb.x - window->llf.x;
	y_size = window->urb.y - window->llf.y;
	UM_len_inttoext(x_size, x_scal);
	UM_len_inttoext(y_size, y_scal);

	sprintf(string, "X/Y: %.2f/%.2f %s", x_scal, y_scal,  units);
	gtext(&textpt, string);

	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_getvpnm(name, vport)
**      Retrieve a reference viewport with this name
**  PARAMETERS   
**      INPUT  :  name	:	name of viewport to be found
**						vport	:	pointer to vport structure where
**									viewport is to be put
**  RETURNS      :  UU_SUCCESS		: if the viewport was found,
**						  UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_getvpnm(name, vport)
	char *name;
	UV_vport *vport;
	{
	UU_KEY_ID key;
	int next_tupleid, switched;
	char msg[256];

	uu_denter(UU_MTRC,(us,"uv_getvpnm(%s)", name));

/*
.....always retrieve vport info from working unibase
*/
	switched = UU_FALSE;
	if (UR_active==2)
	{
		ur_getu_work();
		switched = UU_TRUE;
	}
	/* search unibase list to the vport with this name */
	next_tupleid = 1;
	while (ur_get_next_data_key(UV_VPORT_REL, &next_tupleid, &key) > -1)
		{
		next_tupleid++;
		
		/* retrieve vport to check */
		if (uv_getvpid(key, vport) == UU_FAILURE)
			{
			sprintf (msg, "failure in uv_getvpnm, %x not retrieved\n", key);
			ud_printmsg(msg);
			exit(1);
			}

		/* if names are the same, we're done */
		if (strcmp(vport->name, name) == 0)
			{
			if (switched)
			{
				ur_getu_second();
				switched = UU_FALSE;
			}
			uu_dexit;
			return(UU_SUCCESS);
			}
		}

	if (switched)
	{
		ur_getu_second();
		switched = UU_FALSE;
	}
	uu_dexit;
	return(UU_FAILURE);
	}

/**************************************************************************
**  E_FUNCTION:  uv_getvpid(key, vport)
**      Retrieve a viewport with this key
**  PARAMETERS   
**      INPUT  :  key	:	Unibase id of viewport to be found
**						vport	:	pointer to vport structure where 
**									viewport is to be put
**      OUTPUT :  none
**  RETURNS      :  UU_SUCCESS		: if the viewport was found,
**						  UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_getvpid(key, vport)
	UU_KEY_ID key;
	UV_vport *vport;
	{
	int irtn,switched;

	uu_denter(UU_MTRC,(us,"uv_getvpid(%x)", key));

	vport->key = key;
/*
.....always retrieve vport info from working unibase
*/
	switched = UU_FALSE;
	if (UR_active==2)
	{
		ur_getu_work();
		switched = UU_TRUE;
	}

	if (ur_retrieve_data(vport, sizeof(UV_vport)) < 0)
		irtn = UU_FAILURE;
	else
		irtn = UU_SUCCESS;

	if (switched)
		ur_getu_second();
	uu_dexit;
	return(irtn);
	}

/**************************************************************************
**  E_FUNCTION:  uv_getvofvp(vport, view)
**      Retrieve the key of the view associated with this viewport
**  PARAMETERS   
**      INPUT  :  vport	:	vport to get view from
**      OUTPUT :  view	:	id of contained view
**  RETURNS      :  UU_SUCCESS		:	If viewport has contained view
**						  UU_FAILURE	: otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_getvofvp(vport, view)
	UV_vport *vport;
	UU_KEY_ID *view;
	{
	int irtn;

	uu_denter(UU_MTRC,(us,"uv_getvofvp(%s)", vport->name));

	if (vport->cur_view == 0)
		irtn = UU_FAILURE;
	else
		{
		*view = vport->cur_view;
		irtn = UU_SUCCESS;
		}

	uu_dexit;
	return(irtn);
	}

/**************************************************************************
**  E_FUNCTION:  uv_vprename(vport, name)
**      Rename a viewport
**  PARAMETERS   
**      INPUT  :  vport		: viewport to be renamed
**						name		: new name
**      OUTPUT :  vport		: renamed viewport
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_vprename(vport, name)
	UV_vport	*vport;
	char		*name;
	{
	uu_denter(UU_MTRC,(us,"uv_vprename(key=%x)",vport->key));
	strcpy(vport->name, name);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_putvp(vport)
**      Save this viewport in Unibase
**  PARAMETERS   
**      INPUT  :  viewport to be saved
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_putvp(vport)
	UV_vport *vport;
	{
	int stat,switched;
	char msg[256];

	uu_denter(UU_MTRC,(us,"uv_putvp(%s)",vport->name));

	switched = 0;
	if (UR_active==2)
/*
.....always save vport info into working unibase
*/
	{
		ur_getu_work();
		switched = 1;
	}
	stat = ur_update_data(vport);
	if (stat < 0)
		{
		sprintf(msg, "uv_putvp vport %s not updated by unibase\n", vport->name);
		ud_printmsg(msg);
		exit(1);
		}
	if (switched)
		ur_getu_second();

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     :		uv_reset_view_key(vport)
**       Reset the view port vport so that it is associated with
**			the same view it was associated with upon initialization
**    PARAMETERS   
**       INPUT  : 
**          vport				vport with view key to be reset
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_reset_view_key(vport)
	UV_vport	*vport;

	{
	int	i;

	uu_denter(UU_MTRC,(us,"uv_reset_view_key()"));

	for (i = 0; i < UV_NVPORTS; i++)
		{
		if (vport->key == UV_view_to_vport.vport_key[i])
			{
			vport->cur_view = UV_view_to_vport.view_key[i];
			uv_putvp(vport);
			}
		}
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_resetv(vport)
**      Restore a view corresponding to view port vport
**  PARAMETERS   
**      INPUT  :  vport					view port with view to be restored
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_resetv(vport)
UV_vport *vport;							/* vport containing view to be restored */
	{
	UV_view view;							/* view to be restored */

	uu_denter(UU_MTRC,(us,"uv_resetv(key=%x)",vport->key));

	if (uv_getvid(vport->cur_view, &view) != UU_SUCCESS)
		{
		uu_dexit;
		return;
		}
	uv_vrestore(&view);
	uu_dexit;
	}
