/*********************************************************************
**    NAME         :  m9eview
**       CONTAINS: routine to switching between subsystems
**			int um_nxtinview(view_key, initialize, key)
**			int um_forall_set_disp(new_displayability)
**			int um_fordrawing_set_disp(key, new_displayability)
**			int um_fordrawing_display(drawing)
**			int um_fordrawing_delseg(drawing)
**			int um_forallinrelation_set_disp(
**			um_init_drawing_screen()
**			um_view_drawing(drawing)
**			um_view_model()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m9eview.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:12
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "gsegac.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mxxx.h"
#include "mdebug.h"
#include "mdraw.h"
#include "view.h"
#include "uims.h"

extern Gseg NCL_mot_seg;

/*********************************************************************
**    E_FUNCTION     : int um_nxtinview(view_key, initialize, key)
**       Find the next entity which is visible in the specified
**			view.
**    PARAMETERS   
**       INPUT  : 
**          view_key					key of view
**				initialize				UU_TRUE => find first
**											UU_FALSE => find next
**       OUTPUT :  
**          key						key of entity in view
**    RETURNS      : 
**			0  if no error;
**			-1 if no more data;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_nxtinview(view_key, initialize, key)
	UU_KEY_ID view_key;
	UU_LOGICAL initialize;
	UU_KEY_ID *key;

	{
	int status;
	UU_LOGICAL initsearch;
	UU_KEY_ID  view_in;
	int displayable;
	UU_LOGICAL blanked;

	uu_denter(UU_MTRC,(us,"um_nxtinview(key=%d, init=%d)",view_key,initialize));

	initsearch = initialize;
	status = 1;
	while (status == 1)
		{
		if (um_getallobjs(initsearch, key) != 0)
			status = -1;
		else
			{
			initsearch = UU_FALSE;
			ur_retrieve_view_key(*key, &view_in);
			ur_retrieve_displayable(*key, &displayable);
			ur_retrieve_blanked(*key, &blanked);
			if ((displayable != UM_NEVERDISPLAYABLE)
				&& (!blanked)
				&& ((view_in == 0) || (view_in == view_key)))
				status = 0;
			}
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_forall_set_disp(new_displayability)
**       Change the displayable attribute for each master tuple which is
**			currently displayable.
**    PARAMETERS   
**       INPUT  : 
**          new_displayabilty			new attribute setting
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
um_forall_set_disp(new_displayability)
	int new_displayability;

	{
	int status;
	int key;
	int displayability;
	int dsegid;
	UU_LOGICAL initialize;

	uu_denter(UU_MTRC,(us,"um_forall_set_disp(new=%d)",
		new_displayability));

	status = 0;
	initialize = UU_TRUE;
	/*
	sprintf(UM_sbuf,"forall: new=%d",new_displayability);
	um_pscroll(UM_sbuf);
	*/
	while (um_getallobjs(initialize, &key) == 0)
		{
		initialize = UU_FALSE;
		ur_retrieve_displayable(key, &displayability);
		/*
		sprintf(UM_sbuf,"forall: key=%d, old=%d",key,displayability);
		um_pscroll(UM_sbuf);
		*/
		if (displayability != UM_NEVERDISPLAYABLE)
			{
			ur_retrieve_disp_segid(key, &dsegid);
			if (dsegid != -1)
				{
				uv_delsegs(dsegid);
				ur_update_disp_segid(key, -1);
				}
			ur_update_displayable(key, new_displayability);
			}
		}

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_fordrawing_set_disp(key, new_displayability)
**       Set the displayability flag for each of the entities in the
**			specified drawing.
**    PARAMETERS   
**       INPUT  : 
**          key							UNIBASE drawing key
**				new_displayability		new value of displayability flag
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_fordrawing_set_disp(key, new_displayability)
	UU_KEY_ID key;
	int new_displayability;

	{
	struct UM_drawing_rec drawing;
	int status;
	int i;

	uu_denter(UU_MTRC,(us,"um_fordrawing_set_disp(key=%d, new=%d)",
		key, new_displayability));

	status = 0;
	drawing.key = key;
	um_get_all_geom(&drawing.key, sizeof(drawing));
	ur_update_displayable(drawing.key, new_displayability);
	for (i=0; i<drawing.no_member; i++)
		{
/*
... Test for zero key to prevent fatal error when accessing a drawing
... which was somehow saved in a unibase after having members deleted
... and without having um_update_active_drawing() called. IJD 4-DEC-1992
*/
		if (drawing.member[i] > 0)
			ur_update_displayable(drawing.member[i], new_displayability);
		}

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_fordrawing_display(drawing)
**       Displays each of the entities in the specified drawing.
**    PARAMETERS   
**       INPUT  : 
**          drawing       Drawing to display.
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_fordrawing_display(drawing)
struct UM_drawing_rec *drawing;
{
	int status;
	int i;
	struct UC_entitydatabag ent;
/*
.....Display all entities in drawing
*/
	status = 0;
	ug_setredrwflag(0);
	uc_display(drawing);
	for (i=0; i<drawing->no_member; i++)
	{
		if (drawing->member[i] > 0)
		{
			ent.key = drawing->member[i];
			um_retrieve_data_fixed(&ent);
			uc_display(&ent);
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int um_fordrawing_delseg(drawing)
**       Deletes all DIGS segments associtated with the current drawing.
**    PARAMETERS   
**       INPUT  : 
**          drawing       Drawing to delete display segments from.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_fordrawing_delseg(drawing)
struct UM_drawing_rec *drawing;
{
	int status,i;
	int dsegid;
/*
.....Delete drawing segment
*/
	status = 0;
	ur_retrieve_disp_segid(drawing->key, &dsegid);
	if (dsegid != -1)
	{
		uv_delsegs(dsegid);
		ur_update_disp_segid(drawing->key, -1);
	}
/*
.....Delete drawing entities segments
*/
	for (i=0; i<drawing->no_member; i++)
	{
		if (drawing->member[i] > 0)
		{
			ur_retrieve_disp_segid(drawing->member[i], &dsegid);
			if (dsegid != -1)
			{
				uv_delsegs(dsegid);
				ur_update_disp_segid(drawing->member[i], -1);
			}
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int um_forallinrelation_set_disp(
**											rel_num, new_displayability)
**       Set the displayablility flag for each tuple in a specified 
**			relation.
**    PARAMETERS   
**       INPUT  : 
**          rel_num						relation number
**				new_displayability		new value for displayability flag
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_forallinrelation_set_disp(rel_num, new_displayability)
	int rel_num;
	int new_displayability;

	{
	int status;
	int next_tupleid;
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"um_forallinrelation_set_disp(rel_num=%d,num=%d",
		rel_num, new_displayability));

	status = 0;
	next_tupleid = 1;
	while (ur_get_next_data_key(rel_num, &next_tupleid, &key) > -1)
		{
		next_tupleid++;
		switch (rel_num)
			{
			case UM_DRAWING_REL:
				um_fordrawing_set_disp(key, new_displayability);
				break;
			default:
				break;
			}
		}
	uu_dexit;
	return (status);
	}

/**************************************************************************
**  E_FUNCTION:  um_init_drawing_screen()
**			Create the following entities for displaying a drawing:
**				1. a single format screen
**				2. a single viewport covering the entire screen
**				3. a single view which has its viewing parameters
**					(re)set depending upon the drawing size visible
**					in the viewport
**  PARAMETERS   
**      INPUT  :
**      OUTPUT :
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void um_init_drawing_screen()

	{
	UU_KEY_ID drw_view;
	UU_KEY_ID drw_vport;
	UU_KEY_ID drw_screen;
	UU_REAL drw_aspect;
	UV_screen screen;					/* screen record */
	UM_coord	vpurb;					/* upper right back of viewport */
	UM_coord	vpllf;					/* lower left front of viewport */
	UM_coord ref_pt;					/* view reference point */
	UM_length xlen;					/* half of the length of view window */
	UM_length ylen;					/* half of the height of view window */
	UM_length aperture;				/* view window aperture */
	UU_REAL x,y,xm,ym;				/* extents of default drawing size */
	int curr_scrn;						/* current screen layout */
	Gnrect *gr_area;					/* extents of current graphics area */
	int drwunits;

	uu_denter(UU_MTRC,(us,"um_init_drawing_screen()"));

	/* get the extent of the current graphics area */
	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	/* these values define the viewport to display a drawing */
	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, vpllf);
	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 0.0, vpurb);
	drw_aspect = (gr_area->ur.y - gr_area->ll.y) /
						 (gr_area->ur.x - gr_area->ll.x);
	ur_put_drwmdl_aspect(drw_aspect);

	drwunits = ur_get_drwmdl_drwunits();

	/* create a single view in UNIBASE for displaying a drawing */
	/* RAH: modified UM_drawing_size[][] so that original size '0' is
	   now size '36'; added drwunits to call to scale extents according
	   to effective drawing units. */
	um_get_drawing_extents(36, &x, &y, &xm ,&ym);
	xlen = x;
	ylen = y;
	um_xyztovc(x/2.0, y/2.0, (UU_REAL) 0.0, ref_pt);
	if (xlen > ylen) aperture = xlen; else aperture = ylen;
	uv_create_view("drw_view", UV_SYSDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
						ref_pt, UM_yaxis, UM_zaxis, aperture,
						UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
						&drw_view, 0);


	/* create a single screen format */
	drw_screen = uv_scdefine("drw_screen", &screen, 0);

	/* create a single viewport covering the entire screen and associate it
		with the view and screen */
	uv_set_vport(drw_view, "drw_vport", vpllf, vpurb, &screen, 0);
	drw_vport = screen.vports[0];

	/* update UNIBASE with screen */
	uv_putsc(&screen);

	/* update global drawing modals */
	ur_put_drwmdl_drwsn(drw_screen);
	ur_put_drwmdl_drwvp(drw_vport);
	ur_put_drwmdl_drwvw(drw_view);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_view_drawing(drawing)
**			Display the specified drawing on the "drawing" screen.
**    PARAMETERS   
**       INPUT  : 
**          drawing						pointer to drawing entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_view_drawing(drawing)
	struct UM_drawing_rec *drawing;

	{
	int status;
	UV_view view;
	UU_REAL xlen, ylen;
	UU_REAL drw_aspect;
	UM_coord ref_pt;
	UU_REAL x, y, xm, ym;
	UU_KEY_ID active_drawing;
	UU_KEY_ID drw_view;
	UU_KEY_ID cpln_key;
	char cur_model_screen_name[20];
	char drw_units_str[16],mod_units_str[16],str[64];
	char orig_label[80];

	uu_denter(UU_MTRC,(us,"um_view_drawing(key=%d)",drawing->key));

	/* remove  the display segments from DIGS for all displayable objects */
/*
.....If call this function while we are
.....plottting in plotter, we do not need
.....clear the screen
.....Yurong 9/8/97
*/
	if (uj_miplotting() != UU_TRUE)  
		uv_clear();

	active_drawing = ur_get_drwmdl_curdrw();
/*
.....Model mode
*/
	if (active_drawing == 0)
	{
		/* reset construction plane to default */
		umu_setcpln(0, orig_label);

		/* save the current model mode screen */
		uv_current_screen_name(cur_model_screen_name);
		ur_put_drwmdl_modsname(cur_model_screen_name);

		/* mark ALL displayable objects non-displayable  */
		um_forall_set_disp(UM_UNDISPLAYABLE);
	}
/*
.....Drawing currently displayed
*/
	else
	{
		/* update the current active drawing */
		um_update_active_drawing();

		/* turn off the current active drawing */
		um_fordrawing_set_disp(active_drawing, UM_UNDISPLAYABLE);
	}

	/* mark all objects on the new drawing displayable */
	um_fordrawing_set_disp(drawing->key, UM_DISPLAYABLE);

	/* mark construction plane displayable */
	cpln_key = ur_get_dispattr_cpln_key();
	ur_update_displayable(cpln_key, UM_DISPLAYABLE);

	/* finally, activate the single view and display all displayable objects 
		(i.e. objects on drawing) */
	drw_view = ur_get_drwmdl_drwvw();
	uv_getvid(drw_view, &view);
	/* RAH: added drwunits to scale extents according to current drawing units */
	um_get_drawing_extents(drawing->drwsize, &x, &y, &xm , &ym);
	drw_aspect = ur_get_drwmdl_aspect();
	xlen = x;
	ylen = y/drw_aspect;
	if (ylen > xlen) xlen = ylen;
	xlen *= 1.05;
	um_xyztovc(x/2.0, y/2.0, (UU_REAL) 0.0, ref_pt);
	uv_setrefpt(&view, ref_pt);
	uv_setvaperture(&view, xlen);
	uv_putv(&view);
	uv_chgsc("drw_screen");

	/* finally, make all new geometry constructed in "drawing" mode
		be associated with the single "drawing" view and update the
		key of the current drawing */
	ur_put_dispattr_view_in(drw_view);
	ur_put_drwmdl_curdrw(drawing->key);
/*
.....Update status area with this
.....drawing's scale
.....Bobby  -  11/18/99
*/
	
	um_linear_units_str(drawing->drwunits,drw_units_str);
	um_linear_units_str(drawing->modunits,mod_units_str);
	sprintf(str,"%.3f%s = %.3f%s",drawing->drwscale,
		drw_units_str,drawing->modscale,mod_units_str);
	ur_put_drwmdl_unitsstr(str);
		
	uu_dexit;
	status = UU_SUCCESS;	/* OSF */
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : um_view_model()
**			Change into model viewing mode.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_view_model()

	{
	UU_KEY_ID active_drawing;
	int modunit,drwunit;
	UU_REAL modscl,drwscl;
	char cur_model_screen_name[20];
	char drw_units_str[16],mod_units_str[16],str[64];
	UG_segstli *segptr;
	char orig_label[80];

	uu_denter(UU_MTRC,(us,"um_view_model()"));

	active_drawing = ur_get_drwmdl_curdrw();
	if (active_drawing != 0)
		{
		/* reset various system data */
		um_update_active_drawing();
		umu_setcpln(0, orig_label);

		/* remove  the display segments from DIGS for all displayable objects */
		uv_clear();
	
		/* mark all objects displayable */
		um_forall_set_disp(UM_DISPLAYABLE);
	
		/* mark all drawings nondisplayable */
		um_forallinrelation_set_disp(UM_DRAWING_REL,UM_UNDISPLAYABLE);
	
		/* put up "current" modeling screen (i.e. last one displayed) */
		ur_get_drwmdl_modsname(cur_model_screen_name);
		uv_chgsc(cur_model_screen_name);
	
		/* finally, make all geometry be associated with "all" views 
			and indicate that there is no current drawing */
		ur_put_dispattr_view_in(0);
		ur_put_drwmdl_curdrw(0);
/*
.....Update status area with the
.....default drawing scale
.....Bobby  -  11/18/99
*/
		drwscl = ur_get_drwmdl_drwscale();
		modscl = ur_get_drwmdl_modscale();
		drwunit = ur_get_drwmdl_drwunits();
		modunit = ur_get_drwmdl_drwunits();
		um_linear_units_str(drwunit,drw_units_str);
		um_linear_units_str(modunit,mod_units_str);
		sprintf(str,"%.3f%s = %.3f%s",drwscl,
			drw_units_str,modscl,mod_units_str);
		ur_put_drwmdl_unitsstr(str);
/*
.....Set motion segment displayable
.....Bobby  -  10/10/06
*/
		segptr = ug_segac(NCL_mot_seg);
		if (segptr != UU_NULL) gssegvis(NCL_mot_seg,UG_VISIBLE);
		}

	uu_dexit;
	}

