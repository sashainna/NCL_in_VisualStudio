/*********************************************************************
**    NAME         :  m9udrwf.c.c
**       CONTAINS: form handling routines for drawings
**			int umu_create_drawing_form(drawing, display)
**			int umu_set_drwscale()
**			int umu_set_def_drwscale()
**			um_get_drwscale(drwscale)
**			int umu_drawing_filename_form(formfile, drawname, filename)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m9udrwf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:13
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "mxxx.h"
#include "mdebug.h"
#include "mdraw.h"
#include "udforms.h"
#include "udfdata.h"
#include "mdrwsize.h"

/* static storage for handling units etc. in forms */
static int drawing_size[20];
static int drawing_units[20];
static int model_units[20];
static int model1_units[20];
static int model2_units[20];

static int NCL_plotter_model=0;
static int plotter_model;
static int old_plotter_type;
static int old_drawing_size[5]= {0,0,0,0,0};

static UD_FSTAT um_chk()
{return(UD_FLDOK);}
/*********************************************************************
**    S_FUNCTION     :  static um_change_size(filedno, val, stat)
**       Method called at 'Drawing size' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT um_change_size(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_REAL size_l, size_w;
	UU_REAL um_getdrw_lenth(), um_getdrw_width();
/*
.....set the new size
*/
	size_l = um_getdrw_lenth(plotter_model, drawing_size[0]);
	size_w = um_getdrw_width(plotter_model, drawing_size[0]);
	ud_update_answer(9, (int*)&size_l);	
	ud_update_answer(10, (int*)&size_w);	

	old_drawing_size[plotter_model] = drawing_size[0];
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  static um_change_plotter(filedno, val, stat)
**
**       Method called at 'Plotter type' toggle field.  Sets display
**    mask for a new plotter source.
**
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT um_change_plotter(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_REAL size_l, size_w;
	UU_REAL um_getdrw_lenth(), um_getdrw_width();
	old_drawing_size[old_plotter_type] = drawing_size[0];
/*
.....set the new size
*/
	size_l = um_getdrw_lenth(plotter_model, drawing_size[0]);
	size_w = um_getdrw_width(plotter_model, drawing_size[0]);
	ud_update_answer(9, (int*)&size_l);	
	ud_update_answer(10, (int*)&size_w);	
	old_drawing_size[plotter_model] = old_drawing_size[old_plotter_type];
	old_plotter_type = plotter_model;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : int umu_create_drawing_form(drawing, display)
**       Put up a form for the user to define a drawing.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          drawing					drawing entity with fields filled in
**    RETURNS      : 
**			0 iff no error and all data filled in
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umu_create_drawing_form(drawing)
	struct UM_drawing_rec *drawing;

	{
	int status;
	int modunits, drwunits;
	UU_REAL mod1scale, mod2scale;
	UU_REAL plotprec, drw_length, drw_width;
	int *ans[11];
	int i, j;
	int  mod, mod1, mod2;
	int drw_units, mdl1_units, mdl2_units;
	char drw_units_str[16];
	char mod_units_str[16];
	char drw_scale_str[64];
	UU_REAL drwscale;
	UU_REAL modscale;
	UU_REAL um_getdrw_lenth(), um_getdrw_width();
/*
.....function changed to dymanicly display draw size for plotter
.....Yurong 7/31/97
*/
	static UD_METHOD methods[11] = {			 
		um_chk, um_change_size, um_chk, um_change_plotter, um_chk,
		um_chk, um_chk, um_chk, um_chk, um_chk, um_chk};
	static char called[169] = { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 };
	static char traverse[] = { 1,1,1,1,1,1,1,1,1,0, 0}
	uu_denter(UU_MTRC,(us,"umu_create_drawing_form()"));
	status = 0;

start:
	/* get current drawing modal variables */
	drawing_size[0] = ur_get_drwmdl_drwsize();;
	plotprec = ur_get_drwmdl_plotprec();
	drw_units = ur_get_drwmdl_drwunits();
	drwscale = ur_get_drwmdl_drwscale();
	modscale = ur_get_drwmdl_modscale();

	/*MILLS: get draw scale, convert units into form units  */
	mod = ur_get_drwmdl_modunits();
	mod1 = mod/100;
	mod2 = mod - (mod1 * 100 );

	if (drw_units == 0)
		drawing_units[0]=0;
	else if (drw_units == 3)
		drawing_units[0]=1;
	else if (drw_units == 4)
		drawing_units[0]=2;
	else if (drw_units == 5)
		drawing_units[0]=3;

	if (mod1 == 0) mod1=0;
	else if (mod1 == 3) mod1=1;
	else if (mod1 == 4) mod1=2;
	else if (mod1 == 5) mod1=3;

	if (mod2 == 0) mod2=0;
	else if (mod2 == 3) mod2=1;
	else if (mod2 == 4) mod2=2;
	else if (mod2 == 5) mod2=3;

	/* Initialize values into form */
	model1_units[0]= mod1;
	model2_units[0]= mod2;

	strcpy(drawing->name, "");
	plotter_model = NCL_plotter_model;
	ans[0] = (int *) drawing->name;
	ans[1] = (int *) drawing_size;
	ans[2] = (int *) &plotprec;
/*
.....changed usinf tmp variable
.....Yurong 9/4/97
*/
	ans[3]=(int *)&plotter_model;
	ans[4]=(int *)&drwscale;
	ans[5]=(int *)model1_units;
	ans[6]=(int *)&modscale;
	ans[7]=(int *)model2_units;
	ans[8]=(int *)drawing_units;

	old_plotter_type = plotter_model;
	for (i = 0; i<5; i++)
		old_drawing_size[i] = 0;
	
/*
.....get the length and width, we will use later
.....we we set form field
*/
	drw_length = um_getdrw_lenth(plotter_model, drawing_size[0]);
	drw_width = um_getdrw_width(plotter_model, drawing_size[0]);
	ans[9] =(int *) &(drw_length);
	ans[10] = (int *) &(drw_width);

	status = ud_form1("mcredraw.frm", ans, ans, methods, called, NULL, traverse);
	if (status==-1)
		return -1;
	if( drwscale < 0.00001 || modscale < 0.00001 ) goto start;
	NCL_plotter_model = plotter_model;

	/* adjust Units from form into units of modelling */

	if  (model1_units[0] == 0)
		mdl1_units = 0;	/* IN */
	else if (model1_units[0] == 1)
		mdl1_units = 3;	/* CM */
	else if (model1_units[0] == 2)
		mdl1_units = 4;
	else if (model1_units[0] == 3)
		mdl1_units = 5;

	if  (model2_units[0] == 0)
		mdl2_units = 0;
	else if (model2_units[0] == 1)
		mdl2_units = 3;
	else if (model2_units[0] == 2)
		mdl2_units = 4;
	else if (model2_units[0] == 3)
		mdl2_units = 5;

	if (drawing_units[0] == 0)
		drw_units = 0;
	else if (drawing_units[0] == 1)
		drw_units = 3;
	else if (drawing_units[0] == 2)
		drw_units = 4;
	else if (drawing_units[0] == 3)
		drw_units = 5;

	/* store values from form into drawing entity */
/*
......change max size type to 16
......Yurong 7/31/97
*/
/*	drawing->drwsize=(NCL_plotter_model*12)+UM_form_to_drwsize[drawing_size[0]];
*/
	drawing->drwsize=(NCL_plotter_model*16)+UM_form_to_drwsize[drawing_size[0]];
	drawing->plotprec = plotprec;
	drawing->drwscale = drwscale;
	drawing->modscale = modscale;
	mod = mdl1_units * 100 + mdl2_units;
	drawing->modunits = mod;
	drawing->drwunits = drw_units;

	/* Update current drawing model global variables according to form */
	ur_put_drwmdl_plotprec(plotprec);
	ur_put_drwmdl_drwunits(drw_units);
	ur_put_drwmdl_drwscale(drwscale);
	ur_put_drwmdl_modunits(mod);
	ur_put_drwmdl_modscale(modscale);
/*
.....added by Yurong
*/
	ur_put_drwmdl_drwsize(drawing_size[0]);
	 
	um_linear_units_str(mdl1_units, drw_units_str);
	um_linear_units_str(mdl2_units, mod_units_str);
	sprintf(drw_scale_str,"%.3f%s = %.3f%s", drwscale,
		drw_units_str, modscale, mod_units_str);
	ur_put_drwmdl_unitsstr(drw_scale_str);
/*
.....added by Yurong 8/19/97
*/
	if (strlen(drawing->name)==0)
	{
/*
.....Empty drawing name is not allowed
*/
		uu_uerror1(UM_MODEL, 320);
		return UU_FAILURE;
	}

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int umu_set_drwscale()
**       Put up a form for the user to define the current values for
**			the view associated drawing/model units and scale factors
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**    RETURNS      : 
**			0 iff no error and all data filled in
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umu_set_drwscale()

	{
	int status;
	int  mod, mod1, mod2;
	int drw_units;
	char drw_units_str[16];
	char mod_units_str[16];
	char drw_scale_str[64];
	UU_REAL drwscale;
	UU_REAL modscale;
	int *ans[5];
	UU_KEY_ID active_drawing;
	struct UM_drawing_rec drawing;

	uu_denter(UU_MTRC,(us,"umu_set_drwscale()"));

/*
.....Initialize routine
*/	
start:
	status = 0;
	active_drawing = ur_get_drwmdl_curdrw();

/*
.....A drawing is active
.....Change the drawing's scale
*/
	if (active_drawing != 0)
	{
		drawing.key = active_drawing;
		status = ncl_retrieve_data_fixed(&drawing);
		drw_units = drawing.drwunits;
		mod = drawing.modunits;
		drwscale = drawing.drwscale;
		modscale = drawing.modscale;
	}
/*
.....No drawing active
.....Change default drawing scale
*/
	else
	{
		drw_units = ur_get_drwmdl_drwunits();
		drwscale = ur_get_drwmdl_drwscale();
		modscale = ur_get_drwmdl_modscale();
		mod = ur_get_drwmdl_modunits();
	}
/*
.....Set appropriate drawing units
*/
	if (drw_units == 0)
		drawing_units[0] = 0;
	else if (drw_units == 3)
		drawing_units[0] = 1;
	else if (drw_units == 4)
		drawing_units[0] = 2;
	else if (drw_units == 5)
		drawing_units[0] = 3;

	mod1 = mod/100;
	mod2 = mod - (mod1 * 100 );
	model1_units[0] =  mod1;
	model2_units[0] =  mod2;

	/*MILLS: adjust model units to reflect order on form */
	if (model1_units[0] == 3)
		model1_units[0] = 1;

	if (model2_units[0] == 3)
		model2_units[0] = 1;

	ans[0] = (int *) &drwscale;
	ans[1] = (int *) model1_units;
	ans[2] = (int *) &modscale;
	ans[3] = (int *) model2_units;
	ans[4] = (int *) drawing_units;

	status = ud_form("mdrwscal.frm", ans, ans);
	if (status==-1)
		return -1;
	if( drwscale < 0.00001 || modscale < 0.00001 ) goto start;
/*
.....Adjust the drawing units
*/
	if	(drawing_units[0] == 0)
		drw_units = 0;
	else if (drawing_units[0] == 1)
		drw_units = 3;
	else if (drawing_units[0] == 2)
		drw_units = 4;
	else if (drawing_units[0] == 3)
		drw_units = 5;

	/*MILLS: adjust model units to reflect order on form */
	if (model1_units[0] == 1)
		model1_units[0] = 3;

	if (model2_units[0] == 1)
		model2_units[0] = 3;
	mod = model1_units[0] * 100 + model2_units[0];
/*
.....Store new scale in drawing
*/
	if (active_drawing != 0)
	{
		drawing.drwunits = drw_units;
		drawing.drwscale = drwscale;
		drawing.modunits = mod;
		drawing.modscale = modscale;
		status = ur_update_data_fixed(&drawing);
	}
/*
.....Store new scale as default
*/
	else
	{
		ur_put_drwmdl_drwunits(drw_units);
		ur_put_drwmdl_drwscale(drwscale);
		ur_put_drwmdl_modunits(mod);
		ur_put_drwmdl_modscale(modscale);
	}
/*
.....Define new drawing scale string
*/
	um_linear_units_str(model1_units[0], drw_units_str);
	um_linear_units_str(model2_units[0], mod_units_str);
	sprintf(drw_scale_str,"%.3f%s = %.3f%s", drwscale,
		drw_units_str, modscale, mod_units_str);
	ur_put_drwmdl_unitsstr(drw_scale_str);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int umu_set_def_drwscale()
**       Create a default drawing scale (1)
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**    RETURNS      : 
**			0 iff no error and all data filled in
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umu_set_def_drwscale()

	{
	uu_denter(UU_MTRC,(us,"umu_set_drwscale()"));

	
	ur_put_drwmdl_drwunits(1);
	ur_put_drwmdl_drwscale((UU_REAL) 1.0);
	ur_put_drwmdl_modunits(101);
	ur_put_drwmdl_modscale((UU_REAL) 1.0);

	uu_dexit;
	return (0);
	}
/*********************************************************************
**    E_FUNCTION     : um_get_drwscale(drwscale)
**       Calculate the current drawing scale.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          drwscale				current drawing scale
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_get_drwscale(drwscale)
	UU_REAL *drwscale;

	{
	UU_REAL drw_to_cm,rval;
	UU_REAL mod_to_cm;
	UU_REAL mod1scale, mod1cm;
	UU_REAL mod2scale, mod2cm;
	int modunits, drwunits, mod1, mod2;
	UU_KEY_ID active_drawing;

	uu_denter(UU_MTRC,(us,"um_get_drwscale()"));

	active_drawing = ur_get_drwmdl_curdrw();
/*
.....The drawing scale should be active
.....when drawings are not in effect also
.....FSR - 61374
*/
/*
	if (active_drawing == 0)
		{
		*drwscale = 1.;
		}
	else
*/
		{
		mod1scale = ur_get_drwmdl_drwscale();
		mod2scale = ur_get_drwmdl_modscale();
		modunits = ur_get_drwmdl_modunits();
		drwunits = ur_get_drwmdl_drwunits();
		mod1 = modunits/100;
		mod2 = modunits - (mod1 * 100 );
		mod1cm = mod1scale * UM_cpln.conv_factors[mod1];
		mod2cm = mod2scale * UM_cpln.conv_factors[mod2];
		uu_dprint(UU_MTRC,(us,"s1 %g, s2 %g, munit %d, dunit %d", mod1cm,
									mod2cm, modunits, drwunits));
		rval = mod2cm * UM_cpln.conv_factors[drwunits];
		if (rval == 0.)
			*drwscale = 1.;
		else
			*drwscale = mod1cm/rval;
		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int umu_drawing_filename_form(formfile, 
**										drawname, filename)
**       Put up a form for the user to fill in to get a drawing
**			name and file name.
**    PARAMETERS   
**       INPUT  : 
**          formfile					name of form file
**       OUTPUT :  
**				drawname					name of a drawing
**				filename					name of a file
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umu_drawing_filename_form(formfile, drawname, filename)
	char formfile[];
	char drawname[];
	char filename[];

	{
	int status;
	int *ans[2];

	uu_denter(UU_MTRC,(us,"umu_drawing_filename_form(????)"));

	status = UU_SUCCESS;

	ans[0] = (int *) drawname;
	ans[1] = (int *) filename;

	ud_form(formfile, ans, ans);

	uu_dexit;
	return(status);
	}



/*********************************************************************
**    E_FUNCTION     : UU_REAL um_getdrw_lenth(plot_type, size) 
**		get draw size lenth	
**    PARAMETERS   
**       INPUT  : 
**         plot_type:		plotter type
**			  size:           size of drawing 
**       OUTPUT :  
**				lenth of drawing
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_getdrw_lenth(plot_type, size)
int plot_type;
int size;
{
UU_REAL drw_length;
/*
.....get current unit useing for model
.....and change UM_drawing_size (use cm)
.....to current unit
*/
	if (UM_cpln.length_unit == UM_CM)
	{
		drw_length = UM_drawing_size[16*plot_type+size][0];
	}
	else if (UM_cpln.length_unit == UM_MM)
	{
		drw_length = UM_drawing_size[16*plot_type+size][0]*10;
	}
	else
/*
.....Inch
*/
	{
		drw_length = UM_drawing_size[16*plot_type+size][0]/2.54;
	}
	return drw_length;
}

/*********************************************************************
**    E_FUNCTION     : UU_REAL um_getdrw_width(plot_type, size) 
**		get draw size lenth	
**    PARAMETERS   
**       INPUT  : 
**         plot_type:		plotter type
**			  size:           size of drawing 
**       OUTPUT :  
**				width of drawing
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_getdrw_width(plot_type, size)
int plot_type;
int size;
{
UU_REAL drw_width;
/*
.....get current unit useing for model
.....and change UM_drawing_size (use cm)
.....to current unit
*/
	if (UM_cpln.length_unit == UM_CM)
	{
		drw_width = UM_drawing_size[16*plot_type+size][1];
	}
	else if (UM_cpln.length_unit == UM_MM)
	{
		drw_width = UM_drawing_size[16*plot_type+size][1]*10;
	}
	else
/*
.....Inch
*/
	{
		drw_width = UM_drawing_size[16*plot_type+size][1]/2.54;
	}
	return drw_width;
}
