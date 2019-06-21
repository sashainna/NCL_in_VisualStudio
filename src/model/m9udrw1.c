/*********************************************************************
**    NAME         :  m9udrw1.c
**       CONTAINS: user interface routines for drawings
**			umu_c46_create_drawing()
**			umu_46_delete_drawing()
**			umu_46_list_drawings()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m9udrw1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:13
*********************************************************************/
#include "usysdef.h"
#include "ustdio.h"
#include "udebug.h"
#include "uims.h"
#include "dwindow.h"
#include "dasnog.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "mxxx.h"
#include "mdraw.h"
#include "mdebug.h"
#include "gcolors.h"         /* color definitions - ud_crwin */
#include "nclfc.h"
#include "udforms.h"

/*********************************************************************
**    E_FUNCTION     : umu_c46_create_drawing()
**       Prompt the user for the information defining a drawing.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c46_create_drawing()

	{
	int status;
	struct UM_drawing_rec drawing;
	struct UM_line_rec line;
	struct UM_attrdata_rec attr;
	UU_KEY_ID drw_view;
	UU_KEY_ID key, newkeys[4];
	UU_REAL x, y, xm, ym;
	UM_coord corner[5];
	int i;

	uu_denter(UU_MTRC,(us,"umu_c46_create_drawing()"));
	ur_setup_app_data(UM_DRAWING_REL, &drawing, sizeof(drawing));

	/* prompt user for data defining the new drawing */
	status = umu_create_drawing_form(&drawing);
	if (status != 0) goto done;

	/* check if a drawing with this name already exists */
	status = um_key_from_drawing_name(drawing.name, &key);
	if (status == 0 )
		{
		uu_uerror1(/* drawing already exists */ UM_MODEL, 204, drawing.name);
		goto done;
		}

	/* set view new entities are viewed in to be the drawing view */
	drw_view = ur_get_drwmdl_drwvw();
	ur_put_dispattr_view_in(drw_view);

	/* get current attributes, but make sure lines are solid */
	um_current_default_attr(UM_DRAWING_REL, &attr);
	attr.line_style = UM_SOLID_LINE;

	/* store margin lines in UNIBASE */
	/* COMMENTED OUT: do we want these four lines? There keys are lower
	 than the drawing key, so when a ul_reset_unibase is done, they get
	 deleted first, then an attempt is made to delete the drawing and each
	 entity associated with it.  Since the first four 'members' of the
	 drawing are already deleted, a SEGV occurs..... */
/**
/*	um_get_drawing_extents(drawing.drwsize, &x, &y, &xm, &ym);
/*	um_xyztovc(xm, ym, (UU_REAL) 0.0, corner[0]);
/*	um_xyztovc(x-xm, ym, (UU_REAL) 0.0, corner[1]);
/*	um_xyztovc(x-xm, y-ym, (UU_REAL) 0.0, corner[2]);
/*	um_xyztovc(xm, y-ym, (UU_REAL) 0.0, corner[3]);
/*	um_xyztovc(xm, ym, (UU_REAL)  0.0, corner[4]);
/*
/*	ur_setup_data(UM_LINE_REL, &line, sizeof(line));
/*	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
/*	strcpy (line.label, "");
/*	line.subscr = 0;
/*	for (i=0; i<4; i++)
/*		{
/*		um_vctovc(corner[i], line.spt);
/*		um_vctovc(corner[i+1], line.ept);
/*		um_create_geom(&line, UM_DEFAULT_TF, &attr);
/*		ur_update_displayable(line.key, UM_UNDISPLAYABLE);
/*		newkeys[i] = line.key;
/*		}
/*
/*	/* initialize and store the drawing in UNIBASE */
/*	ur_update_app_data_varlist(&drawing, 1, newkeys, drawing.no_member+1, 4);
**/
	um_create_geom(&drawing, UM_DEFAULT_TF, &attr);
	ur_update_displayable(drawing.key, UM_UNDISPLAYABLE);

	/* If motion exists erase it before creating the drawing. */
	motdel();

	/* display the drawing (e.g. extent and margin borders) */
	um_view_drawing(&drawing);

done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_46_delete_drawing()
UU_KEY_ID key;
**       Prompt the user for the drawing to delete.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_46_delete_drawing()
{
	char name[16];
	int numint;
	UU_KEY_ID key;
	UU_KEY_ID active_drawing;
	int status, len;
	int *ans[1];
	UD_LIST drawing_name_list;
	static char draw_name[16] = " ";
	uu_denter(UU_MTRC,(us,"umu_46_delete_drawing()"));
/*
.....Use form instead of prompt
.....Yurong 8/16/97
*/
	active_drawing = ur_get_drwmdl_curdrw();
	drawing_name_list.item =
                (char **) um_get_drawing_name(&(drawing_name_list.num_item));
	if (drawing_name_list.num_item == 0)
	{
		uu_uerror1(/* no drawing exist */ UM_MODEL, 319);
		return;
	}
	len = strlen(draw_name);
	drawing_name_list.answer = (char *) uu_malloc(len * sizeof(char));
	strcpy(drawing_name_list.answer, draw_name);
	ans[0] = (int *)&drawing_name_list;
	status = ud_form("mdeletedraw.frm", ans, ans);
	if (status==-1)
	{
		ud_free_flist(&drawing_name_list);
		return -1;
	}
	strcpy(draw_name, drawing_name_list.answer);
	strcpy(name, drawing_name_list.answer);

	status = um_key_from_drawing_name(name, &key);
	if (key == active_drawing)
		um_update_active_drawing();
	uc_delete(key);
	if (key == active_drawing)
	{
		ur_put_drwmdl_curdrw(0);
		um_view_model();
	}
	ud_free_flist(&drawing_name_list);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : umu_46_list_drawings()
**			List all of the drawings in the current database.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_46_list_drawings()

	{
	UU_KEY_ID next_tupleid;
	struct UM_drawing_rec drawing;
	char drawing_units_str[5];
	char drawing_plotter_str[10];
	char modeling_units_str[5];
	char drawing_size_str[7];	/*RAH: increased to correct display */
	int linectr;
	char msg[120];
	int status;
	int markval;
	int mod, mod1, mod2;
	int bckgrnd;			/* bckgrnd color of ansi window */
	UD_WINDOW_REC wcb;	/* window control block */
	UD_AREA *areapt;			/* pointer to an array of areas */

/*	static Gdrect view_win = {.3,.2, .8,.7}; /* view window bounds */
/*	static Gdrect view_win = {.2,.0, .99,.63}; /* NCL - view window bounds */
	static Gdrect view_win; /* NCL - determine from GRAPHICS area */
	int args[2];

	uu_denter(UU_MTRC,(us,"umu_46_list_drawings()"));

	/* get location of GRAPHICS area */
	areapt= &UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_GRAF][0];
	view_win.ll.x = (*areapt).posn.ll.x + 0.002;/* RAH: increased to correct display */
	view_win.ll.y = (*areapt).posn.ll.y;
	view_win.ur.x = (*areapt).posn.ur.x;
	view_win.ur.y = view_win.ll.y + 0.63; 

	next_tupleid = 1;
/*	bckgrnd = dqwinback();
/*	WINDOW_INIT(&wcb, &view_win, UG_C_WHITE, bckgrnd);
/*	WINDOW_ON(&wcb, &UD_winrow, &UD_wincol);*/
	args[1] = 1;
	UD_winrow = 20; UD_wincol = 80;
	ul_open_window(UD_winrow,UD_wincol,args);
	linectr = UD_winrow - 3;
	while (ur_get_next_data_key(UM_DRAWING_REL, &next_tupleid, &drawing.key) > -1)
		{
		next_tupleid++;
		status = um_get_all_geom(&drawing, sizeof(drawing));
		if (status == 0)
			{
			um_drawing_size_str(drawing.drwsize, drawing_size_str, drawing_plotter_str);
			mod = drawing.modunits;
			mod1 = mod/100;
			mod2 = mod - (mod1 * 100 );
			um_linear_units_str(mod1, drawing_units_str);
			um_linear_units_str(mod2, modeling_units_str);
			sprintf(msg,"%-16s   %2s size %8.3f %-2s = %8.3f %-2s %-10s\n",
				drawing.name, drawing_size_str, drawing.drwscale,
				drawing_units_str, drawing.modscale, modeling_units_str,
				drawing_plotter_str);
			linectr--;
			if(linectr == 0)
				{
/*				ud_hakt(UD_DASHEP, 16);*/
				linectr = UD_winrow - 3;
				}
/*			WINDOW_WRITE(&wcb, msg);*/
			ul_win_out(msg,0);
			}
		}
	ud_hakt(/*enter any key to terminate*/ UD_DASHEP, 17);
/*	WINDOW_OFF(&wcb);*/
	ul_close_window();
	uu_dexit;
	}
