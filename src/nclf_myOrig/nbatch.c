/*********************************************************************
**    NAME         : nbatch.c
**       CONTAINS:
**                ncl_init_batch()
**                ncl_reset_unicad()
**                rstuni()
**
**       FOLLOWING ROUTINES: Routines to reset the Unibase when doing a 
**       	LOADU with a Unibase without VIEWING, TRANSFORMATIONS,
**       	MODELING ATTRIBUTES, etc.
**          (created from a batch run or IGES/VDA)
**
**       ncl_reset_unibase()
**       ncl_reset_default_trans()
**       ncl_reset_model()
**       ncl_reset_views()
**       nclreset_sing_screen(view)
**       This handles Unibases from other hardware or MPE releases:
**       ncl_pre_load_environ()
**
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nbatch.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:21
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "adrfcom.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "mfort.h"
#include "nclfc.h"
#include "nccs.h"
#include "nclcmd.h"
/* #include "nclinp.h" */
#include "nkeywd.h"
#include "ualloc.h"
#include "dsubcom.h"
#include "mdunits.h"
#include "xenv1.h"

#include "mdraw.h"
#include "mdrel.h"
#include "mattr.h" 
#include "mdcoord.h"
#include "ribase.h"
#include "view.h"
#include "vconst.h"
#include "mxxx.h"


extern UU_STORE *uu_toolstore;          /* pointer to unicad tool store */
extern int UU_application;
extern char NCL_init_fstr[20];
extern UU_LOGICAL NCL_batch_run;

/*NCL: flag == UU_TRUE if we are loading a unibase without view rels */
extern UU_LOGICAL NCL_view_load, NCL_copy_assoc, NCL_cpln_load;

/*MILLS: When loading a Unibase which came through pcnvtool and/or 
dcnvtool and/or from a different sized monitor, need to save the
aspect set at initialization and reset after a unibase load. This
controls the size of the drawing when in drawing mgmt.            */
UU_REAL save_aspect;

void ncl_reset_unicad(),ncl_reset_model(),ncl_reset_views();
void nclreset_sing_screen();

/*********************************************************************
**    E_FUNCTION     : ncl_init_batch()
**       description: Initialize unicad sub system to run batch.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_init_batch()
{
	int stat;
   UM_int2 iunit, ival, idx;
   char *p, *ux_getenv();
	UX_pathname dir,fname;
	FILE *fptr;

/*
.....Set batch flag
*/
	NCL_batch_run = UU_TRUE;

/* -- initialize the tool storage manager -- */  
   uu_toolmalloc_init();

/* -- Initialize udos system -- */

   ux_init_table(1);

/*   -- Initialize GKS and the DAS   -- */

/*   ud_init_das(); */

/* Initialize the user interface    */

   ud_init_ui();

/* Initialize the system independent IO subsystem     */

/*   ux_init_xio(); */


/* Reset system */

   ncl_reset_unicad();
/*
.....Initialize layers
*/
	um_layerinit();
	ncl_init_color();
	um_set_active_layer(1);

/*
....Initialize construction plane
*/
	um_init_cpln();
/*
.....Load required modals
*/
/*
	ul_break_fname("UL_NIS_MODALS",dir,fname);
	strcpy(fname,"ncl_cmdline.mod");
	if (dir[0] == '\0') strcpy(dir,NCL_init_fstr);
*/
	fname[0] = '\0';
	stat = ul_open_mod_file(UU_NULL,UU_NULL,"UL_NIS_MODALS",UU_NULL,
		fname,2,&fptr);
	if (stat == UU_SUCCESS)
	{
		ul_load_modfile(fptr);
		ux_fclose0(fptr);
	}
	else
	{
		idx = 106; ival = 72; setifl (&idx, &ival);
		idx = 387; ival = 73; setifl (&idx, &ival);
	}
/*
........Load Custom colors
*/
/*
	strcpy(fname,"ncl_color.mod");
	if (dir[0] == '\0') strcpy(dir,NCL_init_fstr);
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",dir,UU_NULL,
		fname,2,&fptr);
	if (stat == UU_SUCCESS)
	{
		ul_load_modfile(fptr);
		ux_fclose0(fptr);
	}
*/

/* -- set DD1 active -- */

   UU_application = UU_NCLCAM;
/*
.....NCCS
.....SET UNITS
*/
/*   um_setunits(UM_INCH); */
    /* added to set units in batch. kathy */
	ival = 0;
	iunit = 264;
	p = ux_getenv("U_UNITS",UX_NPRTERRS);
	if (p != NULL)
	{
		if (ud_strcomp(p,"MM") == -1 || (ud_strcomp(p,"mm") == -1))
			{
			um_setunits(UM_MM);
			ival = 1;
			}
		else um_setunits(UM_INCH);
	}
	else um_setunits(UM_INCH);
	setifl (&iunit, &ival);
/*   um_setunits(UM_INCH);*/
/*
.....Initialize note text
*/
	umu_set_def_drwscale();
	ua_init_drafting();
	ua_init_text();
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_reset_unicad()
**       This routine resets the UNICAD system.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : 
**                   The order that the routines are called in
**                   is very important and should not be changed.
*********************************************************************/

void ncl_reset_unicad()
{

   uu_denter(UU_DTRC,(us,"enter ncl_reset_unicad()"));

/* Initialize UNIBASE      */

   ur_init_unibase();

/* --   Initialize the modeling subsystem   -- */

/*   um_init_model();  */
   ag_init();
/* NCL: added to properly initialize the modeling attributes for 
		display/picking */
	um_idrwmodals();
	ua_reset_text_pos();

/* Initialize ALL relations in UNIBASE */

   uc_init_relations();
/*
.....Free tesselation saved data
*/
	nclc_tessmgr_free();

   uu_dexit;
}
/*********************************************************************
**    E_FUNCTION     : rstuni()
**       description: Fortran callable routine to reset unibase.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*rstuni()
{
   ncl_reset_unicad();
}
*/

/*********************************************************************
** E_FUNCTION : ncl_reset_unibase()
**      Reset the unibase on a loadu if new unibase is missing viewing, etc
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_reset_unibase(loadoperation)
UU_LOGICAL loadoperation;
{
	/*MILLS: used to reset aspect since it is dependant on size of
	graphics area/screen for each hardware-size and MPE release ... */
	ur_put_drwmdl_aspect(save_aspect);

	NCL_copy_assoc = UU_TRUE;

	if ( loadoperation )
		if (NCL_view_load == 0)
		{
		ncl_reset_default_trans(UM_TRANSFORM_REL);
		ncl_reset_model();
		ncl_reset_views();
		ub_init_sym_data();
		uqi_tbreset();
		}
		else
/*
.....vp 3/26/98 make sure that construction plane exists in unibase
.....so initialize it or get its key if loaded but not stored in
.....display attributes
*/
		{
			if (NCL_cpln_load == 0)
			{
				um_cplinit();
			}
			else
			{
				UU_KEY_ID cplnkey;
				cplnkey = ur_get_dispattr_cpln_key();
				if (cplnkey == 0) um_get_axis ("CPLN",&cplnkey);
/*
.....if cpln not found initialize it
.....othervise store key in environment bundle (UM_dispattr)
*/
				if (cplnkey == 0) um_cplinit();
				else ur_put_dispattr_cpln_key(cplnkey);
			}
		}
/*
.....Free tesselation saved data
*/
	nclc_tessmgr_free();
	return;
}

/*********************************************************************
** E_FUNCTION : ncl_reset_default_trans()
**      Create the default transformation in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_reset_default_trans(rel_num)
int rel_num;
	{
	int i;
	int					status;	/* return status */
	struct	UM_transf_rec	transf_packet	;	/* a dummy transformation	*/
	struct UM_transf_rec transfpacket;

	uu_clr_bit( &UR_rcb[rel_num].rel_flags, UR_MTUPLE_REQD);
	uu_clr_bit( &UR_rcb[rel_num].rel_flags, UR_DATA_REL);
	uu_clr_bit( &UR_rcb[rel_num].rel_flags, UR_ATTR_REL);
	uu_set_bit( &UR_rcb[rel_num].rel_flags, UR_TRANSF_REL);

	/* set THE transformation relation, and */
	/* reserve the default transformation matrix	*/
	UR_transf_relnum = rel_num;
	UR_default_transf = UU_FALSE	;	/* no default defined yet	*/
	transf_packet.key = 0 ;
	transf_packet.rel_num = rel_num ;
	transf_packet.use_count = 0;    /* reset use count to 1 */
	i = 0;
	status = ur_create_tuple(rel_num,&i,&transf_packet);

	/* set default transformation */
	um_tftotf(UM_idmat, transfpacket.tfmat);
	if (status == 0)
		status = ur_set_default_transf(&transfpacket);

	return(status);
	}

/*********************************************************************
**    E_FUNCTION     :  int ncl_reset_model()
**       Reset the modeling subsystem.
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ncl_reset_model()
	{
	um_init_cpln();			/* init basic contruction plane */

	um_ini_dispattr();		/*	init display attributes		*/

	um_feainit();			/*	init features subsystem		*/
	um_cplinit();			/*	init unibase construction plane		*/

	um_idrwmodals();		/*	init drawing modals			*/
	ncl_init_attrmdl_color();
	um_modaxisinit();		/*	init model axis				*/
	um_reninit();			/* init rendering package		*/
	um_layerinit();			/* init layer tuple				*/
	ncl_init_color();

	um_drawinit();			/* init drawing subsystem		*/
	return;
	}
 
/**************************************************************************
**  E_FUNCTION:  ncl_reset_views()
**      Reset the VIEWING entities in the Unibase.
**  PARAMETERS   
**      INPUT  : 
**				none
**      OUTPUT : 
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void ncl_reset_views()
	{
	int	i;
	UU_KEY_ID front;
	UU_KEY_ID back;
	UU_KEY_ID top;
	UU_KEY_ID bottom;
	UU_KEY_ID left;
	UU_KEY_ID right;
	UU_KEY_ID risometric;
	UU_KEY_ID lisometric;
	UU_KEY_ID rdimetric;
	UU_KEY_ID ldimetric;

	/* initialize the number server */
	/*
	uu_nserv_init(0, 14, UU_XFORM_NM);
	uu_nserv_resv(UU_XFORM_NM, 0);
	*/

	/* clear the view to view port array */
	for (i = 0; i < UV_NVPORTS; i++)
		{
		UV_view_to_vport.vport_key[i] = 0;
		UV_view_to_vport.view_key [i] = 0;
		}

	/* initialize the screens, views, and view ports */
	/* define all default views */
	uv_front_view(&front);
	uv_back_view(&back);
	uv_top_view(&top);
	uv_bottom_view(&bottom);
	uv_left_view(&left);
	uv_right_view(&right);
	uv_risometric_view(&risometric);
	uv_lisometric_view(&lisometric);
	uv_rdimetric_view(&rdimetric);
	uv_ldimetric_view(&ldimetric);

	/* define all default screen formats and associated viewports */
	uv_quad_screen(front, right, top, risometric);
	uv_hdual_screen(front, risometric);
	uv_vdual_screen(front, risometric);

	/* doen't activate the screen yet ... */
	nclreset_sing_screen(front);

	uv_six_equal_screen(top, bottom, front, risometric, left, right);
	uv_six_unequal_screen(bottom, top, risometric, left, right, front);

	return;
	}


/**************************************************************************
**  E_FUNCTION:  nclreset_sing_screen(view)
**      Define a single screen format and associate the
**			specified views with the defined viewports.
**  PARAMETERS   
**      INPUT  : 
**				view						view to associate with viewport
**      OUTPUT : 
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void nclreset_sing_screen(view)
UU_KEY_ID view;
	{
	UV_screen screen;
	UU_KEY_ID screen1;
	UM_ndc lleft;
	UM_ndc uright;

	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, lleft);
	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 0.0, uright);

	/* make a single view screen */
	screen1 = uv_scdefine("single", &screen, 0);

	/* define the viewport */
	uv_set_vport(view, "single", lleft, uright, &screen, 0);

	/* update screen in UNIBASE */
	uv_putsc(&screen);
	/*
	uv_activsc(&screen);
	*/
	/* define this screen to be the active screen */
	uv_set_screen(&screen, UU_TRUE);

	return;
	}

/*********************************************************************
** E_FUNCTION : ncl_pre_load_environ()
**      Save environmental variables to be reset after load of Unibase
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pre_load_environ()
	{
	/*MILLS: used to reset aspect since it is dependant on size of
	graphics area/screen for each hardware-size and MPE release ... */
	save_aspect = ur_get_drwmdl_aspect();

	return;
	}
