/*********************************************************************
**    NAME         :  m9udrw2.c
**       CONTAINS: user interface routines for drawings
**			umu_m46_place_view()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m9udrw2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:13
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "dasnog.h"
#include "class.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mxxx.h"
#include "mdcpln.h"
#include "mdraw.h"
#include "mdebug.h"
#include "view.h"
#include "drubber.h"
#include "mdattr.h"
#include "gsegac.h"
#include "udforms.h"
#include "mdgenent.h"
#include "mattrddl.h"
#include "gtblseg.h"
#include "mplot.h"

UU_LOGICAL	ud_lyesno();

UM_vector xaxis = {1.0,0.0,0.0};
UM_vector yaxis = {0.0,1.0,0.0};
UM_vector zaxis = {0.0,0.0,1.0};
extern int motion_pen, rapid_pen, cutr_pen;
/*********************************************************************
**    E_FUNCTION     : umu_m46_place_view()
**       Prompt the user for an area of the drawing to into which
**			a view will be projected (i.e. flattened into 2D geometry).
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_m46_place_view()
{
	struct UM_drawing_rec drawing;
	struct UC_entitydatabag edata;
	struct UC_attributedatabag eattr;
	UM_transf etfmat;
	int status;
	int entnum;
	UU_LIST keylist;
	UV_view view;
	UM_vector view_xaxis;
	UM_transf vpln_tfmat;
	UM_transf scale_tfmat;
	UM_transf area_tfmat, refs_tfmat;
	UM_transf temp_tfmat;
	UM_transf hidden_tfmat;
	UM_transf tfmat, stfmat, fit_tfmat, ref_tfmat, mtfmat;
	UM_vector drawing_scale;
	UD_NDCLOCREC pt_on_drawing;

	UU_LOGICAL initialize;
	UU_LOGICAL render_solids;
	UU_KEY_ID key;
	UU_KEY_ID *newkeys;
	UU_REAL mod1scale, mod1cm;
	UU_REAL mod2scale, mod2cm;
	UU_REAL conv_factor;
	int modunits, drwunits, mod1, mod2;
	int *ans[2];
	int nseg;
	UD_RUBBER draginfo;
	UU_REAL origin[3], ref[3];
	UU_LOGICAL  blanked;
	UU_REAL points[5][3];
	UU_REAL drw_x, drw_y, dx, dy;
	UU_REAL scale, scale_x, scale_y;
	Gwrect3 rect;

	UU_REAL refpt[3];
	UG_segstli *segptr;
	UU_REAL rect_dx, rect_dy;
	int cmdreject;
	char **uv_get_viewname();
	static int view_placement = 0;
	static char view_name[16] = "Front";

	UD_LIST view_name_list ;
	int numint = 0;
	UU_LOGICAL dragon = UU_FALSE;

	uu_denter(UU_MTRC,(us,"umu_m46_place_view()"));

/*
.....initial data
*/
	origin[0] =0.0;
	origin[1] =0.0;
	origin[2] =0.0;
	refpt[0] = 0;
	refpt[1] = 0;
	refpt[2] = 0;
	render_solids = UU_FALSE;

	view_name_list.item = uv_get_viewname(&(view_name_list.num_item), 0);
/*
.....set max view name string len to 80 chars
*/
	view_name_list.answer = (char *) uu_malloc(80 * sizeof(char));
	strcpy(view_name_list.answer, view_name);
	ans[0] = (int *)&view_placement;
	ans[1] = (int *)&view_name_list;

	drawing.key = ur_get_drwmdl_curdrw();
	if (drawing.key == 0)
		{
		uu_uerror0(/* no active drawing */ UM_MODEL, 224);
		goto done;
		}
	else
		{
		um_get_all_geom(&drawing, sizeof(drawing));
		}

/*
.....added form to get user answer
.....Yurong 8/11/97
*/
	status = ud_form("mplaceview.frm", ans, ans);
	if (status==-1)
	{
		ud_free_flist(&view_name_list);
		return -1;
	}
	strcpy(view_name, view_name_list.answer);
		
	status = uv_getvnm(view_name, &view);
	if (status != 0) 
	{
		uu_uerror1(/* view not defined */ UM_MODEL, 199, view_name);
		return(UU_SUCCESS);
	}
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
/*
..... calculate the transformation 
..... which will transform the geometry
.....visible in the view to the XY plane 
*/
		um_cross(view.cur_up_vect, view.cur_pln_norm, view_xaxis);
		um_unitvc(view_xaxis, view_xaxis);
		um_chgcstf(view.cur_ref_pt, view_xaxis, view.cur_up_vect, 
			view.cur_pln_norm, UM_zerovec, xaxis, yaxis, zaxis, vpln_tfmat); 

/*
..... determine the scaling transformation
..... to go from "model" units
.....to "drawing" units 
*/
/*
.....These values should get 
.....active drawing
.....changed by Yurong
.....8/19/97
*/
/*	mod1scale = ur_get_drwmdl_drwscale();
	mod2scale = ur_get_drwmdl_modscale();
	modunits = ur_get_drwmdl_modunits();
	drwunits = ur_get_drwmdl_drwunits(); */
		UM_plotprec = drawing.plotprec;
		mod1scale = drawing.drwscale;
		mod2scale = drawing.modscale;
		modunits = drawing.modunits;
		drwunits = drawing.drwunits;

		mod1 = modunits/100;
		mod2 = modunits - (mod1 * 100 );
		mod1cm = mod1scale * UM_cpln.conv_factors[mod1];
		mod2cm = mod2scale * UM_cpln.conv_factors[mod2];
		conv_factor = mod1cm/mod2cm;
/*
.....For FIT, we will not use this scale
.....so set to 1.0 
*/
		if (view_placement==3) 
			conv_factor = 1.0;
		drawing_scale[0] = conv_factor;
		drawing_scale[1] = conv_factor;
		drawing_scale[2] = conv_factor;
/*
..... the concatenated transformation will
..... place the geometry visible
..... in the view in the area of the drawing 
*/
		um_scaletf(drawing_scale, scale_tfmat);
		um_tftmtf(vpln_tfmat, scale_tfmat, temp_tfmat); 
/*
.....In order to get segments that drawing
.....in the center of screen (note: here
.....view already change to drawing, and (0,0)
.....is not center but left corner. we 
.....need added following codes for set in the
.....center when draw this segment
.....otherwise, for X window (it use copy pixel
.....in drag segment, it clipped part of segment
.....because drawing shift outside the screen
.....Yurong 8/22/97
*/
		um_get_drawing_extents(drawing.drwsize, &drw_x, &drw_y, &dx, &dy);
		ref[0] = drw_x/2;
		ref[1] = drw_y/2;
		ref[2] =0;
		um_disptf(&ref, refs_tfmat);
		um_tftmtf(temp_tfmat, refs_tfmat, mtfmat);
/*
.....added DRAG, BOX, FIT placement
.....Yurong 8/11/97
*/
		if (view_placement==0)
/*
.....LOCATE
*/
		{			
/*
..... prompt the user for the point on the drawing to map the view center 
*/
			ud_ldas(UD_DASCART, /* point on drawing to place view center */ UM_MODEL,
			263, &pt_on_drawing, 1 , &numint, UD_NODEFAULT );
			if (numint <= 0) goto done;
		}
		else 
/*
.....Drag, FIT,BOX
*/
		{
/*
.....make sure we are in locator mode
*/
			ud_dtcord(UD_LOCATOR, 1, 1);
/*
.....Create segment of viewing for drag
.....Yurong 8/13/97
*/

			initialize = UU_TRUE;
			nseg = gnseg();
			gcreateseg(nseg);
/*
.....set visiblity to not displayed
.....at this time
.....Yurong 8/13/97
*/
			blanked = 1; 
			gssegvis(nseg,blanked);
			gsnormtran(1); 
			while (um_nxtinview(view.key, initialize, &edata.key) > -1)
			{
				initialize = UU_FALSE;
				status = uc_retrieve_data(&edata, sizeof(edata));
				if (status == UU_SUCCESS)
					status = uc_retrieve_transf(edata.key, etfmat);
				if (status == UU_SUCCESS)
					status = uc_retrieve_attr(edata.key, &eattr);
				um_tftmtf(temp_tfmat, etfmat, stfmat); 
				um_tftmtf(stfmat, refs_tfmat, stfmat);
				uc_draw(&edata, stfmat, &eattr); 
			}	
/*
......added for motion
......Yurong 6/2/98
*/
			um_tftmtf(temp_tfmat, refs_tfmat, mtfmat);
			status = ncl_create_drwmot(mtfmat, view);
			gcloseseg();
/*
.....get segment position and rect
.....Yurong
*/
			segptr = ug_segac(nseg);
/*
.....initialize rect 
*/
			rect.llf.x=10000;
			rect.llf.y=10000;
			rect.llf.z=10000;
			rect.urb.x=-10000;
			rect.urb.y=-10000;
			rect.urb.z=-10000;
			ug_segextrema(segptr, &rect);
/*
.....DRAG
*/
			if(view_placement==1)
			{
/*
.....make origin center to box center
.....so that we can get the segment
.....that is far away from drawing center
.....Yurong 8/19/97
*/
				origin[0] = (rect.llf.x + rect.urb.x)/2 ;
				origin[1] = (rect.llf.y + rect.urb.y)/2 ; 
				DRAG_GRAPHICS(&draginfo, nseg, origin,1);
				DRAG_ON(&draginfo);
				dragon = UU_TRUE;
				ud_ldas(UD_DASCART, UB_SYMBOL, 11, &pt_on_drawing, 1, &numint,
					 UD_NODEFAULT);
/*
.....shift points back
*/
				pt_on_drawing.cord[0] = pt_on_drawing.cord[0] - origin[0];
				pt_on_drawing.cord[1] = pt_on_drawing.cord[1] - origin[1]; 
/*
.....delete the segment
.....because we no longer need it
*/
				gdeleteseg(nseg);
			}
			else if (view_placement==3)
/*
.....FIT
*/
			{
/*
.....delete the segment
.....because we no longer need it
*/
				gdeleteseg(nseg);
				rect_dx = rect.urb.x-rect.llf.x;
				rect_dy = rect.urb.y-rect.llf.y;
/*
.....put drawing inside border
.....0.1 inch, 2.54 mm
*/
				if (rect_dx!=0)
					scale_x = (drw_x-0.1)/rect_dx;
				if (rect_dy!=0)
					scale_y = (drw_y-0.1)/rect_dy;
				if (rect_dx==0)
					scale = scale_y;
				else if (rect_dy==0)
					scale = scale_x;
				else
					scale = (scale_x<scale_y)? scale_x : scale_y;

/*
.....we need adjust center of points
*/
				refpt[0] = drw_x/2 - (rect.llf.x+ rect_dx/2);
				refpt[1] = drw_y/2 - (rect.llf.y+ rect_dy/2);
				refpt[2] = 0;
				drawing_scale[0] = scale;
				drawing_scale[1] = scale;
				drawing_scale[2] = scale;
				um_scaletf(drawing_scale, fit_tfmat);
				um_disptf(refpt, ref_tfmat);
				um_tftmtf(temp_tfmat, ref_tfmat, temp_tfmat);             
				um_tftmtf(temp_tfmat, fit_tfmat, temp_tfmat);
				um_tftmtf(scale_tfmat,fit_tfmat, scale_tfmat); 
				pt_on_drawing.cord[0] = 0;
				pt_on_drawing.cord[1] = 0;
				pt_on_drawing.cord[2] = 0;
			}	
			else
/*
.....BOX
*/
			{
/*
.....delete the segment
.....because we no longer need it
*/
				gdeleteseg(nseg);
/*
.....Create BOX segment for drag
*/
				nseg = gnseg();
				gcreateseg(nseg);
/*
.....set visiblity to not displayed
.....Yurong 8/14/97
*/
				blanked = 1;
				gssegvis(nseg,blanked);
				gsnormtran(1);
				points[0][0] = rect.llf.x;
				points[0][1] = rect.llf.y;
				points[0][2] = rect.llf.z;
				points[1][0] = rect.llf.x;
				points[1][1] = rect.urb.y;
				points[1][2] = rect.llf.z;
				points[2][0] = rect.urb.x;
				points[2][1] = rect.urb.y;
				points[2][2] = rect.llf.z;
				points[3][0] = rect.urb.x;
				points[3][1] = rect.llf.y;
				points[3][2] = rect.llf.z;
				points[4][0] = rect.llf.x;
				points[4][1] = rect.llf.y;
				points[4][2] = rect.llf.z;
				gpolyline3(5, points);
				gcloseseg();
/*
.....make origin center to box center
.....so that we can get the segment
.....that is far away from drawing center
.....Yurong 8/19/97
*/
				origin[0] = (rect.llf.x + rect.urb.x)/2 ;
				origin[1] = (rect.llf.y + rect.urb.y)/2 ;
				DRAG_GRAPHICS(&draginfo, nseg, origin,1);
				DRAG_ON(&draginfo);
				dragon = UU_TRUE;
				ud_ldas(UD_DASCART, UB_SYMBOL, 11, &pt_on_drawing, 1, &numint,
					 UD_NODEFAULT);
/*
.....shift points back
*/
				pt_on_drawing.cord[0] = pt_on_drawing.cord[0] - origin[0];
				pt_on_drawing.cord[1] = pt_on_drawing.cord[1] - origin[1];
/*
.....delete the segment
.....because we no longer need it
*/
				gdeleteseg(nseg);
			}
/*
.....shift back center from (0, 0) to (drw_x/2, drw_y/2)
.....Yurong 8/22/97
*/
			pt_on_drawing.cord[0] = pt_on_drawing.cord[0] + ref[0];
			pt_on_drawing.cord[1] = pt_on_drawing.cord[1] + ref[1];

		}
/* 
.....check to see if there are any solids in UNIBASE. 
.....If so then ask user if the solids should be drawn
..... with hidden lines on or off 
*/
		entnum = 1;
		status = ur_get_next_data_key(UM_BODY_REL, &entnum, &key);
		if (status == 0)
		{
			render_solids = ud_lyesno(UM_MODEL, 286);
		}

/*
.... calculate the translation transformation 
.....to place the geometry
.....on the drawing 
*/
		um_disptf(&pt_on_drawing, area_tfmat);

/*
..... the concatenated transformation will
..... place the geometry visible
..... in the view in the area of the drawing 
*/
		um_tftmtf(temp_tfmat, area_tfmat, tfmat); 
		
		um_tftmtf(scale_tfmat, area_tfmat, hidden_tfmat);

/* initialize list to check for duplicate geometry */
		um_init_dup_list();

	/* for each entity visible in the view, obtain a list of keys of
			entities which have been projected onto the drawing */


		initialize = UU_TRUE;
		while (um_nxtinview(view.key, initialize, &edata.key) > -1)
		{
			initialize = UU_FALSE;
			uu_list_init(&keylist, sizeof(UU_KEY_ID), 1000, 1000); 
			status = uc_retrieve_data(&edata, sizeof(edata));
			if (status == UU_SUCCESS)
				status = uc_retrieve_transf(edata.key, etfmat);
			if (status == UU_SUCCESS)
				status = uc_retrieve_attr(edata.key, &eattr);
			if (status == UU_SUCCESS)
			{
				uc_proj_to_drawing(&edata, etfmat, &eattr,
					tfmat, view.cur_ref_pt, view.cur_pln_norm,
					&keylist, render_solids); 
/*
.....Check for successful status
.....Bobby  -  6/2/92
*/
				if (status == UU_SUCCESS)
				{
					newkeys = (UU_KEY_ID *) UU_LIST_ARRAY(&keylist);
					ur_update_app_data_varlist(&drawing, 1, newkeys, 
						drawing.no_member+1, keylist.cur_cnt);
				}
			}
			uu_list_free(&keylist); 
		}
/*
......added for motion
......Yurong 6/2/98
*/
		if (view_placement==3)
			ncl_proj_motion_to_draw(vpln_tfmat, scale, view, drawing,
					pt_on_drawing,refpt, 1);
		else
			ncl_proj_motion_to_draw(vpln_tfmat, conv_factor, view, drawing,
					pt_on_drawing,refpt, 0);

		/* free list to check for duplicate geometry */
		um_free_dup_list();

		/* render hidden line drawing of solids in the view */
		if (render_solids)
			um_hiddn_lns_drwng(&drawing, view.cur_ref_pt, view.cur_pln_norm,
		 		view.cur_up_vect, hidden_tfmat);

		/* update the drawing */
		um_update_geom(&drawing, UM_DEFAULT_TF); 
	}
	else
/*
.....if command reject
*/
	{ }
	if (dragon)
		DRAG_OFF(&draginfo);
done:
	if (drawing.key != 0)
	{
		ur_free_app_data(&drawing);
	}
	ud_free_flist(&view_name_list);
	UD_UNMARK(cmdreject);
	return(UU_SUCCESS);
	uu_dexit;
}
