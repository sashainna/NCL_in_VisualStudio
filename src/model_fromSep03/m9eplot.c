/*********************************************************************
**    NAME         : m9eplot.c 
**       CONTAINS:	plotting support
**			int uv_current_screen_name(name)
**			int uv_plot_screen()
**			uv_draw_allvp(eptr, tfmat, attr)
**			uv_plot_allvp(eptr, tfmat, attr)
**			int um_plot_drawing(key)
**			int um_get_drawing_to_plot(key, drawing)
**			um_set_drafting_view(view_key)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m9eplot.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:12
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "class.h"
#include "mdcoord.h"
#include "mplot.h"
#include "mdraw.h"
#include "mdattr.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mxxx.h"
#include "view.h"
/*NCL: added for quick plotting motion - kathy */
#include "mdrel.h"
#include "ulist.h"


/*
extern  UU_LOGICAL NCL_motion_init ;
extern  UU_LOGICAL NCL_motion_closed ;
extern  int NCL_motion_vport;
extern  int NCL_start_index;
extern  Gseg NCL_cur_motion_seg;
*/

/*
.....Temp  -  Bobby
*/
/*UU_LIST NCL_motion_segs;*/
/*extern UU_LIST NCL_motion_segs;*/

/*NCL: end of add for now */

/*********************************************************************
**    E_FUNCTION     : int uv_current_screen_name(name)
**       Return the name of the current screen.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          name						name of current screen
**    RETURNS      : 
**			currently always returns 0
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_current_screen_name(name)
	char *name;

	{
	uu_denter(UU_MTRC,(us,"uv_current_screen_name()"));
	strcpy(name, UV_act_screen[0].name);
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : int uv_plot_screen()
**       Call the drawing routines to plot whatever is displayed on
**			the current screen.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_plot_screen()

	{
	struct UC_entitydatabag e;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	int status;
	UU_LOGICAL initialize;
	UU_LOGICAL blanked;

	uu_denter(UU_MTRC,(us,"uv_plot_screen()"));

	status = 0;
	UM_plotting = UU_TRUE;
	UM_plotprec = ur_get_drwmdl_plotprec();

	initialize = UU_TRUE;
	while ((uv_getobjs(initialize, &e.key, UU_FALSE) != -1) && (status == 0))
		{
		initialize = UU_FALSE;
		ur_retrieve_blanked(e.key, &blanked);

		/* Get the information about motion and put it in the plotfile. kathy */
		if (!blanked)
			{
			ur_retrieve_data_relnum(e.key, &e.rel_num);
			uc_retrieve_data(&e, sizeof(struct UC_entitydatabag));
			uc_retrieve_transf(e.key, tfmat);
			uc_retrieve_attr(e.key, &attr);
			uv_plot_allvp(&e, tfmat, &attr);
			}
		}
		ncl_display_motion(-1,0,0,0,UU_TRUE,UU_FALSE,UU_NULL);

	UM_plotting = UU_FALSE;
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : uv_draw_allvp(eptr, tfmat, attr)
**			Draw (stroke) the specified entity in all active viewports.
**			No DIGS display segment is created. The entity is drawn 
**			under the normtran for all active viewports in the currently
**			active segment (which is typically the non-retained segment).
**    PARAMETERS   
**       INPUT  : 
**				eptr						entity to draw in all active vports
**				tfmat						transformation matrix
**				attr						display attributes
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_draw_allvp(eptr, tfmat, attr)
	struct UC_entitydatabag *eptr;
	UM_transf tfmat;
	struct UC_attributedatabag *attr;

	{ 
	int i;

	uu_denter(UU_MTRC,(us,"uv_draw_allvp(key=%d, tfmat=%x, attr=%x)", 
		eptr->key, tfmat, attr));

	for (i=0; i<UV_act_screen[0].nvports; i++)
		if (UV_act_vports[0][i].disp_all == UU_TRUE)
			{
			gsnormtran(UV_act_vports[0][i].xform);
			uc_draw(eptr,tfmat,attr);
			}

	uu_dexit;
	return;
	}
/*********************************************************************
**    E_FUNCTION     : int uv_plot_allvp(eptr, tfmat, attr)
**			PLOT the specified entity in all active viewports.
**    PARAMETERS   
**       INPUT  : 
**				eptr						entity to plot in all active vports
**				tfmat						transformation matrix
**				attr						display attributes
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_plot_allvp(eptr, tfmat, attr)
	struct UC_entitydatabag *eptr;
	UM_transf tfmat;
	struct UC_attributedatabag *attr;

	{ 
	int i;
	UU_KEY_ID onlyview;
	UU_LOGICAL plot_in_view;

	uu_denter(UU_MTRC,(us,"uv_plot_allvp(key=%d, tfmat=%x, attr=%x)", 
		eptr->key, tfmat, attr));

	ur_retrieve_view_key(eptr->key, &onlyview);
	for (i=0; i<UV_act_screen[0].nvports; i++)
		{
		if (onlyview != 0)
			plot_in_view = (onlyview == UV_act_vports[0][i].view);
		else
			plot_in_view = (UV_act_vports[0][i].disp_all == UU_TRUE);
		if (plot_in_view)
			{
			attr->color = uj_setpen(attr->pen);
			gsnormtran(UV_act_vports[0][i].xform);
			uc_draw(eptr,tfmat,attr);
			}
		}

	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_plot_drawing(key)
**       Plot the specified drawing.
**    PARAMETERS   
**       INPUT  : 
**          key					key of drawing to plot
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_plot_drawing(key)
	UU_KEY_ID key;

	{
	int status;
	struct UM_drawing_rec drawing;
	struct UC_entitydatabag e;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	UM_transf drwtfmat;
	UU_REAL scale[3];
	int i;
	UU_LOGICAL blanked;

	uu_denter(UU_MTRC,(us,"um_plot_drawing(key=%d)",key));

	status = UU_FAILURE;
	UM_plotting = UU_TRUE;

	if (key == ur_get_drwmdl_curdrw()) 
		um_update_active_drawing();

	drawing.key = key;
	if (um_get_all_geom(&drawing, sizeof(drawing)) != UU_SUCCESS) goto done;
	if (drawing.no_member == 0) goto done;
	UM_plotprec= drawing.plotprec;
	if (uc_retrieve_transf(drawing.key, tfmat) != UU_SUCCESS) goto done;
	if (uc_retrieve_attr(drawing.key, &attr) != UU_SUCCESS) goto done;
	scale[2] = scale[1] = scale[0] = 1.0 / UM_cpln.conv_factors[UM_CM];
	um_scaletf(scale, drwtfmat);
	um_tftmtf(tfmat, drwtfmat, tfmat);
	ur_retrieve_blanked(drawing.key, &blanked);
	if (!blanked) uv_plot_allvp((struct UC_entitydatabag *)&drawing,tfmat,&attr);

	for (i=0; i<drawing.no_member; i++)
		{
/*
... Test for zero key to prevent fatal error when accessing a drawing
... which was somehow saved in a unibase after having members deleted
... and without having um_update_active_drawing() called. IJD 4-DEC-1992
*/
		if ((e.key = drawing.member[i]) > 0)
			{
			ur_retrieve_blanked(e.key, &blanked);
			if (!blanked)
				{
				if (uc_retrieve_data(&e, sizeof(e)) != UU_SUCCESS) goto next;
				if (uc_retrieve_transf(e.key, tfmat) != UU_SUCCESS) goto next;
				if (uc_retrieve_attr(e.key, &attr) != UU_SUCCESS) goto next;
				um_tftmtf(tfmat, drwtfmat, tfmat);
				uv_plot_allvp(&e, tfmat, &attr);
				}
			}
next:;
		}
	status = UU_SUCCESS;

done:
	UM_plotting = UU_FALSE;
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_get_drawing_to_plot(key, drawing)
**       Retrieve the specified drawing data.
**    PARAMETERS   
**       INPUT  : 
**          key					key of drawing to plot
**       OUTPUT :  
**          drawing				drawing entity
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_get_drawing_to_plot(key, drawing)
	UU_KEY_ID key;
	struct UM_drawing_rec *drawing;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_get_drawing_to_plot(key=%d)", key));

	if (key == ur_get_drwmdl_curdrw()) 
		um_update_active_drawing();

	drawing->key = key;
	status = um_get_all_geom(drawing, sizeof(struct UM_drawing_rec));

	uu_dexit;
	return (status);
	}

/**************************************************************************
**  E_FUNCTION:  um_set_drafting_view(view_key,cpln_draw)
**			Prompt the user for the view which will be associated with
**			a drafting entity and change the construction plane to
**			correspond with the view plane of this view.
**  PARAMETERS   
**      INPUT  :  cpln_draw         LOGICAL draw/don't draw cpln
**      OUTPUT :  
**				view_key						key of view to associate with
**												drafting entities
**  RETURNS      :  
**			UU_SUCCESS iff all ok; else UU_FAILURE
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int
um_set_drafting_view(view_key, cpln_draw)
	UU_KEY_ID *view_key;
	UU_LOGICAL  cpln_draw;

	{
	int status;
	UV_vport vport;						/* view port structure */

	uu_denter(UU_MTRC,(us,"um_set_drafting_view()"));

	status = UU_FAILURE;
	if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
		uv_getvofvp(&vport, view_key);
/*
.....09/25/92
.....Commented out by Paul to fix the FST 51190. One more file was changed -
.....dtafting/adrfclas.c.
.....
.....   if (cpln_draw) um_drw_cpl_axis(UU_TRUE, UU_FALSE);
*/
		status = UU_SUCCESS;
		}
	uu_dexit;
	return (status);
	}
