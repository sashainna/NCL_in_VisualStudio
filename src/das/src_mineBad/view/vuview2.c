/*********************************************************************
**    NAME         :  vuview2.c
**       CONTAINS: User interface routines for modifying view data
**						for a viewport in a screen
**			uvu_vpzoomextrema()
**			uvu_repaint_viewport()
**			uvu_deletev()
**			uvu_display_views()
**			uvu_savev()
**			uvu_restorev(option)
**			uvu_loadv()
**			uv_getzoom_large_apert()
**
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       vuview2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:59
**************************************************************************/

#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "usysdef.h"
#include "uims.h"
#include "ginqxf.h"
#include "dwindow.h"
#include "dasnog.h"
#include "modef.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "view.h"
#include "gcolors.h"         /* color definitions - ud_crwin */
#include "xenv1.h"
#include "udforms.h"
#include "ulist.h"
#include "mxxx.h"

#define NO_DEFAULT 0
#define DEFAULT 1

extern int UZ_nclipv_view;
extern UU_LOGICAL NCL_waterline;
extern int UR_active;

/**************************************************************************
**  E_FUNCTION:  uvu_vpzoomextrema()
**			Using the bounding boxes of the current segments for the
**			lower left and upper right of the viewport
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uvu_vpzoomextrema()

	{
	UU_REAL midpt[3];
	UU_REAL del_x,del_y,del_z;
	UU_LOGICAL ok;
	UV_vport vport;
	UV_view view;
	UU_REAL aspect_ratio;					/* aspect ratio of current viewport */
	UU_REAL wylen;								/* y length of view port */
	UU_REAL aperture;							/* x length of view */
	UU_REAL urbvs[3], llfvs[3];
	UU_REAL urb[3], llf[3],scale;
	UU_LOGICAL um_cceqcc(), cent_modified;
	int curr_scrn;
	Gnrect *gr_area;
	Gwrect3 box;

	uu_denter(UU_MTRC,(us,"uvu_vpzoomextrema()"));

	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	if (uvu_pickvp(&vport) == UU_SUCCESS) 	{
		/* get the corresponding view */
		uv_getvid(vport.cur_view, &view);

		/* Save the view parameters before changing. kathy */
		nclu_update_pre_view(view);

		if (vport.bord_seg != -1)
			gssegvis(vport.bord_seg,UG_INVISIBLE);
		if (vport.grid_on)
			gssegvis(vport.grid_seg,UG_INVISIBLE);

		scale = 1.;
		if (UZ_nclipv_view == 1)
			ok = ul_ipv_view_extrema(&vport,&box,&scale);
		else if (NCL_waterline)
			ok = ncl_waterline_extrema(&box);
		else
			ok = gextrema(vport.xform,&box);

		del_x = fabs (box.llf.x - box.urb.x);
		del_y = fabs (box.llf.y - box.urb.y);
		del_z = fabs (box.llf.z - box.urb.z);

		if( ok )
			{

			gsnormtran(vport.xform);

			uv_extrema_project(&box.llf,&box.urb,&view);

			/* get the new midpoint */
			uvu_vcstomcs(&view, 0, &box.urb, urb);
			uvu_vcstomcs(&view, 0, &box.llf, llf);
			midpt[0] = (urb[0] + llf[0]) / 2.0;
			midpt[1] = (urb[1] + llf[1]) / 2.0;
			midpt[2] = (urb[2] + llf[2]) / 2.0;
			if(um_cceqcc(midpt, view.cur_ref_pt))
				{
				cent_modified = UU_FALSE;
				}
			else
				{
				cent_modified = UU_TRUE;
				}
	
			/*  if not single point get the current aspect ratio */

			if( (del_x > .042) || (del_y > .042 ) )
			{
				aspect_ratio = ((vport.urb[1] - vport.llf[1]) *
									(gr_area->ur.y - gr_area->ll.y))
								 / ((vport.urb[0] - vport.llf[0]) *
									(gr_area->ur.x - gr_area->ll.x));
	
				/* get the new aspect ratio */

				um_vctovc(&box.llf, llfvs);
				um_vctovc(&box.urb, urbvs);

				aperture = fabs(urbvs[0] - llfvs[0]);

				wylen = fabs(urbvs[1] - llfvs[1])/aspect_ratio;
				if(wylen > aperture ) aperture = wylen;

				aperture = aperture * scale * 1.05; /* allow 2.5% border */
				view.modified = (fabs(aperture - view.cur_aperture) > UM_FUZZ);
				view.modified = UU_TRUE;
			}
			else
			{
				view.modified = UU_TRUE;
				aperture = 0.0;
			}
	
			if (view.modified || cent_modified) 
			{
				view.modified = UU_TRUE;
				uv_update_ref_pt(&view, midpt);
				if (aperture > UM_FUZZ)
					uv_update_vaperture(&view, aperture);
					uv_delete_hidden(&vport);
					uv_autofact7_redrawvp(&vport, &view, UU_TRUE);
			}
		}
		if (vport.bord_seg != -1)
			gssegvis(vport.bord_seg,UG_VISIBLE);
		if (vport.grid_on)
			gssegvis(vport.grid_seg,UG_VISIBLE);
		}
	uu_dexit;
	return 0;
	}
/*********************************************************************
**    E_FUNCTION     : uvu_repaint_viewport()
**       Repaint a single viewport.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uvu_repaint_viewport()
	{
	UV_vport vport;
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_repaint_viewport()"));

	if (uvu_pickvp(&vport) == UU_SUCCESS)
	{
		uv_getvid(vport.cur_view, &view);
		view.modified = UU_TRUE;
		uv_delete_hidden(&vport);
		/* NCL:Changed UU_FALSE to UU_TRUE to redraw the border.kathy */
		uv_autofact7_redrawvp(&vport, &view, UU_TRUE);
	}
	uu_dexit;
	return 0;
	}

/**************************************************************************
**  E_FUNCTION:  uvu_deletev()
**      Delete a view
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uvu_deletev()
	{
	UV_view delete_view;				/* view to delete */
	UV_view view;						/* current active view */
	UV_vport vport;					/* vport to update the view key in */
	int status;							/* 0 => operation OK, -1 => otherwise */
	int numint;							/* number of entities returned from DAS */
	int i, j;
	int entnum;							/* next entity to retrieve from unibase */
	UU_LOGICAL cmdreject;
/*
.....adde for use form
*/
	int *ans[1];
	char **uv_get_viewname();
	UD_LIST view_name_list ;
	static char viewname[20] = "Front";

	uu_denter(UU_MTRC,(us,"uvu_deletev()"));

	/* specify view name to delete */
/*
.....change to use a form with view list
.....Yurong 9/25/98
*/
/*
	ud_ldas(UD_DASSTRING,UM_MODEL,246,viewname,20,&numint,UD_NODEFAULT);
*/
	view_name_list.item = uv_get_viewname(&(view_name_list.num_item), 0);
	view_name_list.answer = (char *) uu_malloc(80 * sizeof(char));
	strcpy(view_name_list.answer, viewname);
	ans[0] = (int *)&view_name_list;
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
		status = ud_form("vdelview.frm", ans, ans);
		if (status==-1)
			goto done;
	}
	else
		goto done;
	numint = strlen(view_name_list.answer);
	if (numint <= 0) goto done;
	strcpy(viewname, view_name_list.answer);
	status = uv_getvnm(viewname, &delete_view);
	if (status == -1) 
	{
		uu_uerror1(/* View does not exist */ UM_MODEL, 208, viewname);
		uu_dexit;
		goto done;
	}
	if (delete_view.can_save != UU_TRUE)
	{
		uu_uerror0(/* You cannot delete a reference view */ UM_MODEL, 211);
		uu_dexit;
		goto done;
	}
	status = UU_FAILURE;
	for (j = 0; j < UV_no_act_screens; j++)
	{
		for (i = 0; i < UV_act_screen[j].nvports; i++)
		{
			status = uv_getvid(UV_act_vports[j][i].view, &view);
			if (status == UU_SUCCESS)
				if (view.key == delete_view.key)
				{
					uu_uerror0(/* You cannot delete a displayed view */
					UM_MODEL, 212);
					uu_dexit;
					goto done;
				}
		}
	}
/* 
.....for all the vport records that point to this view key:
.....reset that vport to the view it pointed to initially	
*/

 	vport.rel_num = UV_VPORT_REL;
	status = 1;
	entnum = 0;
		
	while (status >= 0)
	{
		entnum++;
		/* get the next vport record	*/
		status = ur_get_next_data_key(vport.rel_num, &entnum, &vport.key);
		if (status >= 0)
		{
			ur_retrieve_data(&vport, sizeof(UV_vport));
			/* check to see which view it is associated with */
			if (vport.cur_view == delete_view.key)
				uv_reset_view_key(&vport);
		}
	}

/* 
.....delete the requested view 
*/
	ur_delete_all(delete_view.key);
done:
	ud_free_flist(&view_name_list);
	UD_UNMARK(cmdreject);
	uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION         :  uvu_display_views()
**       Write all the view names to the screen
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uvu_display_views()
	{
	int linectr,switched;							/* line counter */
	char msg[120];							/* message return pointer */
	int status;								/* status of UNIBASE return */
	int entnum;								/* next tuple index */
	UV_view view;							/* view record */
	int args[2];

	uu_denter(UU_MTRC,(us,"uv_display_views"));

	switched = 0;
	if (UR_active==2)
/*
.....always retrieve vport info from working unibase
*/
	{
		ur_getu_work();
		switched = 1;
	}
/*	-- open a window -- */

/*	bckgrnd = dqwinback(); */
/*	WINDOW_INIT(&wcb, UD_HELPWIN, UG_C_WHITE, bckgrnd); */
/*	WINDOW_ON(&wcb, &UD_winrow, &UD_wincol); */
	args[1] = 1;
	UD_winrow = 20; UD_wincol = 80;
	ul_open_window(UD_winrow,UD_wincol,args);

	linectr = UD_winrow - 3;

	status = 0;
	entnum = 0;
		
	view.rel_num = UV_VIEW_REL;

	while (status == 0)
		{
		entnum++;
		status = ur_get_next_data_key(view.rel_num, &entnum,
								&view.key);
		if (status == 0)
			{
			ur_retrieve_data(&view, sizeof(view));
/*
.....Changed Tab to 3 spaces
.....Bobby  -  5/19/94
*/
			sprintf(msg,"   View name = %s\n", view.name);

			linectr--;
			if(linectr == 0)
				{

/*				-- enter any key to continue -- */

/*				ud_hakt(UD_DASHEP, 16);*/
				linectr = UD_winrow - 3;
				}
/*			WINDOW_WRITE(&wcb, msg);*/
			ul_win_out(msg,0);
			}
		}

/*		-- enter any key to terminate -- */

	ud_hakt(UD_DASHEP, 17);
/*	WINDOW_OFF(&wcb);*/
	ul_close_window();

	if (switched)
		ur_getu_second();
	uu_dexit;
	return 0;
	}

/**************************************************************************
**  E_FUNCTION:  uvu_savev()
**      Save a view corresponding to a currently displayed view port.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uvu_savev()
	{
	UV_vport vport;					/* vport with view to be saved */
	UV_view view;						/* view to be saved */
	UV_view save_view;				/* view to be saved into */
	UX_pathname bname;            /* base name*/
	char viewname[15];				/* view name of save_view */
	int numint;							/* number of entities returned from DAS */
	int status;							/* 0 => operation OK, -1 => otherwise */

	uu_denter(UU_MTRC,(us,"uvu_savev()"));

	if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
		if (uv_getvid(vport.cur_view, &view) != UU_SUCCESS)
			{
			uu_dexit;
			return 0;
			}

		/* Save the view parameters before changing. kathy */
		nclu_update_pre_view(view);

		/* specify view name to save into */
		ud_ldas(UD_DASSTRING,UM_MODEL,246,viewname,15,&numint,UD_NODEFAULT);
		while (ux_get_base_fname(viewname, bname, (UX_NPRTERRS | UX_NCHK)) 
				 != UU_SUCCESS)
			{
			uu_uerror0(UX_UDOS,24);
			ud_ldas(UD_DASSTRING,UM_MODEL,246,viewname,15,&numint,
						UD_NODEFAULT);
			}
		if (numint == 0) 
			{
			status = uv_vsave(&view, &view);
			if (status == -1) 
				uu_uerror0(/*reference view cannot be saved*/ UM_MODEL, 207);
			}
		else
			{
			status = uv_getvnm(viewname, &save_view);

			/* if view does not yet exist, define it */
			if (status == -1)
				{
				uv_vdefine(viewname, &save_view);
				save_view.can_save = UU_TRUE;
				}

			/* save view */
			uv_vupdate(&view, &save_view);
			status = uv_vsave(&view, &save_view);

			if (status == UU_FAILURE) 
				uu_uerror0(/*reference view cannot be saved*/ UM_MODEL, 207);
			}
		}
	uu_dexit;
	return 0;
	}

/**************************************************************************
**  E_FUNCTION:  uvu_restorev(option)
**      Restore a view corresponding to a currently displayed view port
**  PARAMETERS   
**      INPUT  :  option			option = 0 ->	restore a view corresponding
**																to a picked view port
**											option = 1 ->	restore all views corresponding
**																to the view ports on the 
**																current screen
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uvu_restorev(option)
	int	option;

	{
	UV_vport vport;						/* vport containing view to be restored */
	UV_view view;							/* view to be restored */
	int i, j;

	uu_denter(UU_MTRC,(us,"uvu_restorev(option = %d)",option));

	if (option == 0)
		{
		if (uvu_pickvp(&vport) == UU_SUCCESS)
			{
			uv_getvid(vport.cur_view, &view);
			/* Save the view parameters before changing. kathy */
			nclu_update_pre_view(view);

			uv_update_ref_pt(&view, view.sav_ref_pt);
			uv_update_pln_norm(&view, view.sav_pln_norm);
			uv_update_up_vect(&view, view.sav_up_vect);
			uv_update_vaperture(&view, view.sav_aperture);
			uv_update_fbclip(&view, view.sav_front_clip, view.sav_back_clip);
			uv_update_clip(&view, view.sav_do_clip);
			uv_delete_hidden(&vport);
			uv_autofact7_redrawvp(&vport, &view, UU_TRUE);
			}
		}
	else if (option == 1)
		{
		for (i = 0; i < UV_no_act_screens; i++)
			{
			for (j = 0; j < UV_act_screen[i].nvports; j++)
				{
				uv_getvpid(UV_act_screen[i].vports[j], &vport);
				uv_delete_hidden(&vport);
				uv_resetv(&vport);
				uv_updatevp(&vport, UU_TRUE);
				}
			}
			}
	uu_dexit;
	return 0;
	}

/**************************************************************************
**  E_FUNCTION:  uvu_loadv(in_viewname,printerr)
**      Load a view into a currently displayed view port.
**  PARAMETERS   
**      INPUT  :
**         in_viewname  = Optional view name to load.  If it is not specified,
**                        then the user will be prompted for the view name.
**         printerr     = UU_TRUE = Output error message if any.
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uvu_loadv(in_viewname,printerr)
char *in_viewname;
UU_LOGICAL printerr;
{
	UV_vport vport;					/* vport with view to be saved */
	UV_view load_view;				/* view to load */
	UV_view view;						/* current displayed view(s) */
	int numint;							/* number of entities returned from DAS */
	int status;							/* 0 => operation OK, -1 => otherwise */
	int index;							/* index into the dsegid array */
	int uv_delvp();
	int i, j;
	UU_LOGICAL cmdreject,init;
	UU_KEY_ID layer;
/*
.....adde for use form
*/
	int *ans[1];
	char **uv_get_viewname();
	UD_LIST view_name_list ;
	static char viewname[20] = "Front";

	uu_denter(UU_MTRC,(us,"uvu_loadv()"));
	init = UU_FALSE;
	if ((in_viewname!=NULL) && (in_viewname[0]!='\0'))
	{
		strcpy(viewname, in_viewname);
		numint = 1;
		goto load_view;
	}
/* 
.....specify name of view to load 
*/
/*
.....change to use a form with view list
.....Yurong 9/25/98
*/
/*
	ud_ldas(UD_DASSTRING,UM_MODEL,246,viewname,15,&numint,UD_NODEFAULT);
*/
	init = UU_TRUE;
	view_name_list.item = uv_get_viewname(&(view_name_list.num_item),  1);
	view_name_list.answer = (char *) uu_malloc(80 * sizeof(char));
	strcpy(view_name_list.answer, viewname);
/*
.....Added "layer list" as end of the view
*/		
	uv_add_layer_list(&view_name_list);	

	ans[0] = (int *)&view_name_list;
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
		status = ud_form("vloadview.frm", ans, ans);
		if (status==-1)
			goto done;
	}
	else
		goto done;
	numint = strlen(view_name_list.answer);
	if (numint <= 0) goto done;
	strcpy(viewname, view_name_list.answer);
load_view:;
	/* pick vieport to change */
	if (uvu_pickvp(&vport) != UU_SUCCESS) goto done;

	if (numint <=  0)  goto done;
	status = uv_getvnm(viewname, &load_view);
	if (status == UU_FAILURE) 
	{
/*
.....we didn't create layer vew when user add layers (because t may too many views
.....but only if user want to see/load the view
.....check if this is the layer view, if this layer view not exist, create it
*/
/*
.....first check if this is a layer vew
*/
		if (uv_valid_layer_view(viewname))
		{
			uv_special_view(&layer, viewname, 2);
			status = uv_getvnm(viewname, &load_view);
		}
		else
		{
			status = UU_FAILURE;
			uu_uerror1(/* View does not exist */ UM_MODEL, 208, viewname);
			goto done;
		}
	}

	status = UU_FAILURE;
	for (j = 0; j < UV_no_act_screens; j++)
	{
		for (i = 0; i < UV_act_screen[j].nvports; i++)
		{
			status = uv_getvid(UV_act_vports[j][i].view, &view);

				/* Save the view parameters before changing. kathy */
			nclu_update_pre_view(view);

			if (status == UU_SUCCESS)
			{
				if (view.key == load_view.key)
				{
					if (printerr)
						uu_uerror0(/* You cannot load a displayed view */
										UM_MODEL, 271);
					status = UU_FAILURE;
					goto done;
				}
			}
		}
	}
	/* find index of picked viewport in global structure */
	for (index = 0; index < UV_act_screen[0].nvports; index++)
	{
		if (UV_act_screen[0].vports[index] == vport.key)
			break;
	}

	/* delete displayed objects in current viewport */
	status = uv_delvp(&vport);

	/* update viewport (in UNIBASE and DIGS) with new view */
	if (status == UU_SUCCESS)
	{
		uv_vtovp(&vport, load_view.key);
		uv_putvp(&vport);

		/* save some information for fast retrieval */
		UV_act_vports[0][index].view = vport.cur_view;

		uv_updatevp(&vport, UU_TRUE);
		uv_dispvp(&vport);
	}
	else
		um_pscroll(" uv_loadv: bad status return from uv_delvp");
done:
	if (init)
	{
		ud_free_flist(&view_name_list);
		UD_UNMARK(cmdreject);
	}
	uu_dexit;
	return(status);
}

/**************************************************************************
**  E_FUNCTION:  uvu_curvpzoomextrema(vport, flag, aspect_ratio)
**			Using the bounding boxes of the current segments for the
**			lower left and upper right of the specified viewport 
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uvu_curvpzoomextrema(vport, flag, aperture)
UV_vport *vport;
int flag;
UU_REAL aperture;
	{
	UU_REAL midpt[3];
	UU_REAL del_x,del_y,del_z;
	UU_LOGICAL ok;
	UV_view view;
	UU_REAL wylen;								/* y length of view port */
	UU_REAL aspect_ratio;
	UU_REAL urbvs[3], llfvs[3];
	UU_REAL urb[3], llf[3];
	UU_LOGICAL um_cceqcc(), cent_modified;
	int curr_scrn;
	Gnrect *gr_area;
	Gwrect3 box;

	uu_denter(UU_MTRC,(us,"uvu_curvpzoomextrema()"));

	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	uv_getvid(vport->cur_view, &view);

	/* Save the view parameters before changing. kathy */
	nclu_update_pre_view(view);

	if (vport->bord_seg != -1)
		gssegvis(vport->bord_seg,UG_INVISIBLE);
	if (vport->grid_on)
		gssegvis(vport->grid_seg,UG_INVISIBLE);

	ok = gextrema(vport->xform,&box);

	del_x = fabs (box.llf.x - box.urb.x);
	del_y = fabs (box.llf.y - box.urb.y);
	del_z = fabs (box.llf.z - box.urb.z);

	if( ok )
	{

		gsnormtran(vport->xform);

		uv_extrema_project((UU_REAL *)&box.llf,(UU_REAL *)&box.urb,&view);

		/* get the new midpoint */
		uvu_vcstomcs(&view, 0, (UU_REAL *)&box.urb, urb);
		uvu_vcstomcs(&view, 0, (UU_REAL *)&box.llf, llf);
		midpt[0] = (urb[0] + llf[0]) / 2.0;
		midpt[1] = (urb[1] + llf[1]) / 2.0;
		midpt[2] = (urb[2] + llf[2]) / 2.0;
		if(um_cceqcc(midpt, view.cur_ref_pt))
		{
			cent_modified = UU_FALSE;
		}
		else
		{
			cent_modified = UU_TRUE;
		}
	
		/*  if not single point get the current aspect ratio */

		if( (del_x > .042) || (del_y > .042 ) )
		{
			aspect_ratio = ((vport->urb[1] - vport->llf[1]) *
									(gr_area->ur.y - gr_area->ll.y))
								 / ((vport->urb[0] - vport->llf[0]) *
									(gr_area->ur.x - gr_area->ll.x));
	
    		/* get the new aspect ratio */

			um_vctovc(&box.llf, llfvs);
			um_vctovc(&box.urb, urbvs);
			if (flag==0)
			{
				aperture = fabs(urbvs[0] - llfvs[0]);
				wylen = fabs(urbvs[1] - llfvs[1])/aspect_ratio;
				if(wylen > aperture ) aperture = wylen;
			}
			aperture *= 1.05; /* allow 2.5% border */
			view.modified = (fabs(aperture - view.cur_aperture) > UM_FUZZ);
			view.modified = UU_TRUE;
		}
		else
		{
			view.modified = UU_TRUE;
			aperture = 0.0;
		}
	
		if (view.modified || cent_modified) 
		{
			view.modified = UU_TRUE;
			uv_update_ref_pt(&view, midpt);
			if (aperture > UM_FUZZ)
				uv_update_vaperture(&view, aperture);
			uv_delete_hidden(vport);
			uv_autofact7_redrawvp(vport, &view, UU_TRUE);
		}
	}
	if (vport->bord_seg != -1)
		gssegvis(vport->bord_seg,UG_VISIBLE);
	if (vport->grid_on)
		gssegvis(vport->grid_seg,UG_VISIBLE);
	uu_dexit;
	return 0;
	}

/**************************************************************************
**  E_FUNCTION:  uv_getzoom_large_apert(vports, new_num, out_aperture)
**			Get the largest aperture from a vport list
**			
**  PARAMETERS   
**      INPUT  :  vports: vport list
**				  new_num: vport number
**      OUTPUT :  out_aperture: largest aperture
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_getzoom_large_apert(vports, new_num, out_aperture)
UV_vport  vports[20];
int new_num;
UU_REAL *out_aperture;
{
	UU_REAL del_x,del_y,del_z;
	UU_LOGICAL ok;
	UV_view view;
	UU_REAL wylen;								/* y length of view port */
	UU_REAL aperture;							/* x length of view */
	UU_REAL aspect_ratio;
	UU_REAL urbvs[3], llfvs[3];
	int i, curr_scrn;
	Gnrect *gr_area;
	Gwrect3 box;

	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	*out_aperture = 0.0;
	for (i=0; i<new_num;i++)
	{
		uv_getvid(vports[i].cur_view, &view);
		ok = gextrema(vports[i].xform,&box);

		del_x = fabs (box.llf.x - box.urb.x);
		del_y = fabs (box.llf.y - box.urb.y);
		del_z = fabs (box.llf.z - box.urb.z);

		if( ok )
		{
			gsnormtran(vports[i].xform);
			uv_extrema_project((UU_REAL *)&box.llf,(UU_REAL *)&box.urb,&view);
			if( (del_x > .042) || (del_y > .042 ) )
			{
				aspect_ratio = ((vports[i].urb[1] - vports[i].llf[1]) *
										(gr_area->ur.y - gr_area->ll.y))
									 / ((vports[i].urb[0] - vports[i].llf[0]) *
										(gr_area->ur.x - gr_area->ll.x));

				um_vctovc(&box.llf, llfvs);
				um_vctovc(&box.urb, urbvs);
				aperture = fabs(urbvs[0] - llfvs[0]);
				wylen = fabs(urbvs[1] - llfvs[1])/aspect_ratio;
				if(wylen > aperture ) aperture = wylen;
				if (aperture>*out_aperture)
					*out_aperture = aperture;
			}
		}
	}
}

/*********************************************************************
**   I_FUNCTION: uv_valid_layer_view(view_name)
**      Check a view name as the valid layer view name (the layer already exst)
**   PARAMETERS
**       INPUT  : view_name  : view name to be checked
**       OUTPUT : none
**   RETURNS: 0: NO, 1: YES
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int uv_valid_layer_view(view_name)
char *view_name;
{
	UU_LIST layer;
	int nlayer, cur_active_layer;
	struct UM_layer_rec *sptr;
	int i,status;
	char tempstr[40];

	status = umu_load_layers(&layer,&nlayer,&cur_active_layer);
	if (nlayer<1)
		return 0;
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&layer);
	for (i=0;i<nlayer;i++)
	{
		sprintf(tempstr, "Layer%d", sptr[i].num);
		if (strcmp (view_name, tempstr)==0)
			return 1;
	}
	return 0;
}
