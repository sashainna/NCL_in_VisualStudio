/*********************************************************************
**    NAME         :  m9edrw
**       CONTAINS: support routines for manageing drawings
**			um_drawing_size_str(size, str)
**			int um_get_area_picked(drawing, pt)
**			um_drawinit()
**			int um_dl46_deldrawing(key)
**			um_drw46_drawing(eptr,tfmat,attrptr)
**			um_drw_2drectangle(lx, ly, ux, uy)
**			um_get_drawing_extents(size, x, y, xm, ym)
**			um_p46_drawing(ptr)
**			int um_key_from_drawing_name(name, key)
**			um_update_active_drawing()
**			um_update_drwunits(draw_units)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       m9edrw.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:12
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "go.h"
#include "mdcoord.h"
#include "mdunits.h"
#include "mdrel.h"
#include "mdclass.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mxxx.h"
#include "modef.h"
#include "mdraw.h"
#include "mdebug.h"
#include "mplot.h"
#include "mdrwsize.h"

/*********************************************************************
**    E_FUNCTION     : um_drawing_size_str(size, str)
**       Return the name (i.e. A, B, etc) of the specified drawing
**			size.
**    PARAMETERS   
**       INPUT  : 
**          size					size of drawing
**       OUTPUT :  
**          str					string indicating drawing size (A, etc)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_drawing_size_str(size, str, pltstr)
	int size;
	char *str, *pltstr;

	{
	int drwsize, plotter = -1;
	char buf[20];

	uu_denter(UU_MTRC,(us,"um_drawing_size_str(%d)",size));

	/* RAH: added support to write out plotter type used to build drawing */
/*
.....change max size to 16
.....Yurong 7/31/97
*/
	plotter = (int)(size/16);
	drwsize = size - (plotter*16);

	if ((drwsize >= 0) && (drwsize <= 15))
		strcpy(str, UM_drawing_size_name[drwsize]);
	else
		strcpy(str,"err");
	
/*
.....removed by Yurong
.....8/19/97
*/
/*	if ((plotter > 0) && (plotter < 4))
		plotter = 3;	/*GENERIC */

	strcpy(pltstr, UM_drawing_plotter_name[plotter]);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_drawinit()
**       Initialize the drawing subsystem.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_drawinit()

	{
	uu_denter(UU_MTRC,(us,"um_drawinit()"));

	ur_put_drwmdl_curdrw(0);

	ur_put_drwmdl_unitsstr("1.0 in = 1.0 in");
	ur_put_drwmdl_drwsize(0);
	ur_put_drwmdl_drwscale((UU_REAL) 1.0);
	ur_put_drwmdl_drwunits(UM_INCH);
	ur_put_drwmdl_modscale((UU_REAL) 1.0);
	ur_put_drwmdl_modunits(UM_INCH*100 + UM_INCH);
	ur_put_drwmdl_plotprec((UU_REAL) 0.01);

	um_init_drawing_screen();

	UM_plotting = UU_FALSE;		
	UM_long_dash = 0.25;					/* 1/4 inch */
	UM_long_gap = UM_long_dash / 2.0;			/* 1/8 inch */
	UM_short_dash = 0.125;					/* 1/8 inch */
	UM_short_gap = UM_short_dash / 2.0;			/* 1/16 inch */

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int um_dl46_deldrawing(key)
**       Delete the specified drawing from UNIBASE and DIGS.
**    PARAMETERS   
**       INPUT  : 
**          key						Key of drawing to delete
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_dl46_deldrawing(key)
	UU_KEY_ID key;

	{
	int status;
	struct UM_drawing_rec drawing;
	int i, ret;

	uu_denter(UU_MTRC,(us,"um_dl46_deldrawing(key=%d)",key));

	/* update current drawing if necessary and view model */
	if (key == ur_get_drwmdl_curdrw())
		um_view_model();

	/* get drawing data */
	drawing.key = key;
	status = um_get_all_geom(&drawing, sizeof(struct UM_drawing_rec));
	if (status != UU_SUCCESS) goto done;

	/* then, delete the drawing */
	for (i=0; i<drawing.no_member; i++)
		{
/*
... Test for zero key to prevent fatal error when accessing a drawing
... which was somehow saved in a unibase after having members deleted
... and without having um_update_active_drawing() called. IJD 4-DEC-1992
*/
		if (drawing.member[i] > 0)
/*
.....when not SUCCESS, it already delete part of it, we can't just get
.....out without delete the rest of it
.....For some drawing (test2.u drawing sun8 when i=24, class=95, 
.....labal="NCL panel") 
.....ucu_3d_validate(UC_3d_descriptor, class, rel_num, UC_DELETE)
.....return UU_NULL, so return UU_FAILED. So, after we delete this drawing
.....and view this drawing again, the drawing still there with less drawing
.....members.
.....Yurong 2/10/1998
*/
			ret = uc_delete(drawing.member[i]);
/*		  if (uc_delete(drawing.member[i]) != UU_SUCCESS) goto done; */
		}
	ur_delete_all(key);

done:
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION: int um_drw46_drawing(eptr,tfmat,attrptr)
**				Draws a drawing.
**		PARAMETERS   
**			INPUT:
**				eptr			pointer to entity record
**				tfmat			transformation matrix
**				attrptr		pointer to attribute record
**			OUTPUT :       none. 
**		RETURNS      : none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int
um_drw46_drawing(eptr,tfmat,attrptr)
	struct UM_drawing_rec *eptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;

	{
	int i;
	UM_coord ll, ur;
	UU_REAL x, y;				/* horizontal and vertical sizes */
	UU_REAL xm, ym;			/* horizontal and vertical margins */

	uu_denter(UU_MTRC,(us,"um_drw46_drawing(%x,%x,%x)", eptr,tfmat,attrptr));

	um_set_disp_attr(attrptr);

	/* get size and margin borders of drawing */
	/* RAH: added drwunits to scale extents according to current drawing units */
	um_get_drawing_extents(eptr->drwsize, &x, &y, &xm, &ym);

	/* apply current transformation matrix */
	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 0.0, ll);
	um_xyztovc(x, y, (UU_REAL) 0.0, ur);
	um_cctmtf(ll, tfmat, ll);
	um_cctmtf(ur, tfmat, ur);

	/* draw border */
	um_drw_2drectangle(ll[0], ll[1], ur[0], ur[1]);

	uu_dexit	;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : um_drw_2drectangle(lx, ly, ux, uy)
**       Draw a 2d rectangle.
**    PARAMETERS   
**       INPUT  : 
**          lx							lower left x
**          ly							lower left y
**          ux							upper left x
**          uy							upper left y
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_drw_2drectangle(lx, ly, ux, uy)
	UU_REAL lx, ly, ux, uy;

	{
	Gwpoint3 gpt[5];

	uu_denter(UU_MTRC,(us,"um_drw_2drectangle(ll=(%f,%f),ur=(%f,%f))",
		lx, ly, ux, uy));

	gpt[0].x = lx;
	gpt[0].y = ly;
	gpt[0].z = 0.0;
	gpt[1].x = ux;
	gpt[1].y = ly;
	gpt[1].z = 0.0;
	gpt[2].x = ux;
	gpt[2].y = uy;
	gpt[2].z = 0.0;
	gpt[3].x = lx;
	gpt[3].y = uy;
	gpt[3].z = 0.0;
	gpt[4].x = lx;
	gpt[4].y = ly;
	gpt[4].z = 0.0;
	gpolyline3(5, gpt);

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : um_get_drawing_extents(size, x, y, xm, ym)
**       Return the drawing extents (e.g. horizontal and vertical
**			drawing size and horizontal and vertical margin size).
**
**    PARAMETERS   
**       INPUT  : 
**          size							size of drawing (e.g. A-D A0-A4)
**       OUTPUT :  
**          x								horizontal size
**          y								vertical size
**          xm								horizontal margin
**          ym								vertical margin
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_get_drawing_extents(size, x, y, xm, ym)
	int size;
	UU_REAL *x;
	UU_REAL *y;
	UU_REAL *xm;
	UU_REAL *ym;

	{
	UU_REAL conv_factor = 2.54;

	uu_denter(UU_MTRC,(us,"um_get_drawing_extents(size=%d)",size));

	/* RAH: UM_drawing_size[][] values are stored in CENTIMETERS!!
	   (has historical significance - UNICAD was originally in CM)
	   We must convert these values into the current units 
	   which for us is always INCHES!!! */

	*x = UM_drawing_size[size][0] / conv_factor;
	*y = UM_drawing_size[size][1] / conv_factor;
	*xm = UM_drawing_size[size][2] / conv_factor;
	*ym = UM_drawing_size[size][3] / conv_factor;

	uu_dexit;
	}


/*********************************************************************
**    E_FUNCTION: int um_p46_drawing(ptr)
**			Print the parameters defining a drawing.
**			PARAMETERS   
**				INPUT: 
**					ptr          pointer to the entity record.
**				OUTPUT :  
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
int
um_p46_drawing(ptr)
	struct UM_drawing_rec  *ptr;

	{
	int i;

	uu_denter(UU_MTRC,(us,"um_p46_drawing(%8x)",ptr));
	sprintf(UM_sbuf, "DRAWING %d", ptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf,"name:     %s",ptr->name);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PINT, "size", 1, &ptr->drwsize);
	um_p_ary(UM_PFLOAT, "drwscale", 1, &ptr->drwscale);
	um_p_ary(UM_PINT, "drwunits", 1, &ptr->drwunits);
	um_p_ary(UM_PFLOAT, "modscale", 1, &ptr->modscale);
	um_p_ary(UM_PINT, "modunits", 1, &ptr->modunits);
	um_p_ary(UM_PFLOAT, "plotprec", 1, &ptr->plotprec);
	um_p_ary(UM_PINT,"members ", ptr->no_member, ptr->member);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_key_from_drawing_name(name, key)
**       Search the current database for a drawing with the specified
**			name and return the key of the drawing if it is found.
**    PARAMETERS   
**       INPUT  : 
**          name					name of the drawing
**       OUTPUT :  
**          key					UNIBASE key of the drawing
**    RETURNS      : 
**			 0 if no error (i.e. key found)
**			-1 otherwise (i.e. key not found)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_key_from_drawing_name(name, key)
	char *name;
	UU_KEY_ID *key;

	{
	int status;
	int next_tupleid;
	struct UM_drawing_rec drawing;

	uu_denter(UU_MTRC,(us,"um_key_from_drawing_name(name=%s)",name));
	next_tupleid = 1;
	ur_setup_data(UM_DRAWING_REL, &drawing, sizeof(struct UM_drawing_rec));
	while (ur_get_next_data_key(UM_DRAWING_REL, &next_tupleid, key) > -1)
		{
		next_tupleid++;
		drawing.key = *key;
		status = um_get_all_geom(&drawing, sizeof(struct UM_drawing_rec));
		if (status == 0)
			{
			if (strcmp(drawing.name, name) == 0)
				{
				uu_dexit;
				return(0);
				}
			}
		}
	uu_dexit;
	return (-1);
	}
/*********************************************************************
**    E_FUNCTION     : int um_update_active_drawing()
**       Update the current active drawing to include all of the
**			currently displayable objects in UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if no errors
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_update_active_drawing()

	{
	struct UM_drawing_rec drawing;
	UU_LOGICAL init;
	UU_KEY_ID key;
	UU_KEY_ID cpln_key;
	int status;

	uu_denter(UU_MTRC,(us,"um_update_active_drawing()"));

	status = UU_FAILURE;
	drawing.key = ur_get_drwmdl_curdrw();
	if (drawing.key != 0)
		{
		status = um_get_all_geom(&drawing, sizeof(drawing));
		if (status == UU_FAILURE) goto done;
	
		init = UU_TRUE;
		ur_free_app_data(&drawing);
		cpln_key = ur_get_dispattr_cpln_key();
		while(uv_getobjs(init, &key, UU_FALSE) == 0)
			{
			init = UU_FALSE;
			if ((key != drawing.key) && (key != cpln_key))
				{
				ur_update_app_data_varlist(&drawing, 1, &key, drawing.no_member+1, 1);
				}
			}
		um_update_geom(&drawing, UM_DEFAULT_TF);
		status = UU_SUCCESS;
		}

done:
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : um_update_drwunits()
**       Update the drawing units.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_update_drwunits()
	{
	int drw_units;
	uu_denter(UU_MTRC,(us,"um_update_drwunits()"));

	ua_get_linear_units(&drw_units);
	ur_put_drwmdl_drwunits(drw_units);

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION         :  um_get_drawing_name(number)
**       get all the Draw names
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT : number: number of drawing name
**    RETURNS      : a list of Draw name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added by Yurong
.....8/18/97
*/
char **um_get_drawing_name(number)
int *number;
{
	int status, len;
	int next_tupleid;
	struct UM_drawing_rec drawing;
	UU_KEY_ID key;
	char **drawing_name;

	*number = 0;
	next_tupleid = 1;
	drawing_name = (char **) uu_malloc(100*sizeof(char *));
	ur_setup_data(UM_DRAWING_REL, &drawing, sizeof(struct UM_drawing_rec));
	while (ur_get_next_data_key(UM_DRAWING_REL, &next_tupleid, &key) > -1)
	{
		next_tupleid++;
		drawing.key = key;
		status = um_get_all_geom(&drawing, sizeof(struct UM_drawing_rec));
		if (status == 0)
		{
			len = strlen(drawing.name);
			drawing_name[*number] = (char *) uu_malloc((len+1) * sizeof(char));
			strcpy(drawing_name[*number], drawing.name);
			(*number)++;
		}
	}
	return drawing_name;
}
