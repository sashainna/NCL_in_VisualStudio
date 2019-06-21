/*********************************************************************
**	FILENAME: lncl.c
**	CONTAINS:    ldefpp
**              lpstnm
**              lsavpp
**              lsavsf
**              ul_reset_unibase
**              ul_default_views
**              ul_umb_screen
**              ul_drawreset
**     MODULE NAME AND RELEASE LEVEL
**        lncl.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**        01/20/17 , 12:03:59
*********************************************************************/

#include "usysdef.h"
#include "rerrdef.h"
#include "view.h"
#include "mdrel.h"
#include "lcom.h"
#include "mfort.h"
#include "mdunits.h"	/* ul_drawreset() */
#include "mplot.h"	    /* ul_drawreset() */
#include "mdraw.h"	    /* ul_drawreset() */
#include "nclfc.h"

extern char *nisstr;
/*extern int NCL_plotter_model;*/
static void ul_default_views();
void ul_umb_screen();
void ul_drawreset();
/*********************************************************************
**	 E_FUNCTION : ldefpp()
**			This function sets up the FORTRAN defualt file
**			names from the C filename variables.
**	 PARAMETERS	
**		 INPUT  :  mxc   = Array size of provided filenames.
**		 OUTPUT :  pp    = default program name
**               cl = default clfile name
**               as = default apt source name
**	 RETURNS: none
**	 SIDE EFFECTS: Initializes NCL variables.
**	 WARNINGS:
*********************************************************************/
void ldefpp(pp,cl,as,mxc)
UM_f77_str_ptr pp,cl,as;
UM_int4 *mxc;
{
	char prog[UX_MAX_FILE_LEN];
	char *cstr;
	int i;
/*
.....Default Part Program Name
*/
	if (nisstr != NULL)
		{
		strcpy (prog,UL_program);
		strcat (prog,".");
		strcat (prog,UL_program_suffix);
		}
	else prog[0] = '\0';

	cstr = UM_cstr_of_f77_str(pp);
	for (i=0; i<strlen(prog); i++) cstr[i] = prog[i];
	for (i=strlen(prog); i<*mxc; i++) cstr[i] = ' ';
/*
.....Default CL File Name
*/
	cstr = UM_cstr_of_f77_str(cl);
	for (i=0; i<strlen(UL_program); i++) cstr[i] = UL_program[i];
	for (i=strlen(UL_program); i<*mxc; i++) cstr[i] = ' ';
/*
.....Default APT Source File Name
*/
	cstr = UM_cstr_of_f77_str(as);
	for (i=0; i<strlen(UL_program); i++) cstr[i] = UL_program[i];
	for (i=strlen(UL_program); i<*mxc; i++) cstr[i] = ' ';
	return;
}
 
/*********************************************************************
**	E_FUNCTION:	lpstnm (pstnam)
**			This function stores the MACHIN name in a post-
**			processor name stack.
**    PARAMETERS   
**       INPUT  :	pstnam = name of post-processor.
**                nc     = Number of characters in 'pstnam'.
**       OUTPUT :	none. 
**
**    RETURNS      : none.
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none.
*********************************************************************/

void lpstnm(pstnam,nc)
UM_f77_str_ptr pstnam;
UM_int4 *nc;
{
	char *cstr,*p;
	int i,j;
/*
.....Store Post Name
*/
	if (UL_nposts < 5)
	{
		cstr = UM_cstr_of_f77_str(pstnam);
		strncpy(UL_posts[UL_nposts],cstr,*nc);
		UL_posts[UL_nposts][*nc] = '\0';
		UL_nposts++;
	}
	return;
}

/*********************************************************************
**	 E_FUNCTION : lsavpp(pp,ncf)
**			This is a FORTRAN callable function to up the
**			 UL_program variable
**	 PARAMETERS	
**		 INPUT  :  pp    = name to store in UL_program
**               ncf   = Number of chars in 'pp'.
**		 OUTPUT :  none
**	 RETURNS: none
**	 SIDE EFFECTS: sets UL_program
**	 WARNINGS: none
*********************************************************************/

void lsavpp(pp,ncf)
UM_f77_str_ptr pp;
int *ncf;
{
	char *cstr;
	int i;
	UM_int2 ifl,val;

	cstr = UM_cstr_of_f77_str(pp);
	for (i=0; i<*ncf; i++) UL_program[i] = cstr[i];
	UL_program[*ncf]='\0';

	/* update STATUS area with new program name */
	/* check to see if we are running batch */
	ifl=35;
	getifl(&ifl,&val);
	if (!val)
		uz_actprogram("");
	return;
}

/*********************************************************************
**   E_FUNCTION : lsavsf(pp,nci)
**          This is a FORTRAN callable function to up the
**           UL_program_suffix variable
**   PARAMETERS   
**       INPUT  :  pp   = suffix of this name to store in UL_program_suffix
**                 nci  = Number of chars in 'pp'.
**       OUTPUT :  none
**   RETURNS: none
**   SIDE EFFECTS: sets UL_program_suffix
**   WARNINGS: none
*********************************************************************/

void lsavsf(pp,nci)
UM_f77_str_ptr pp;
int *nci;
{
	char *cstr;
	UM_int2 ifl,val;
	char *strrchr(),*p;

	cstr = UM_cstr_of_f77_str(pp);
	cstr[*nci] = '\0';
	p = strrchr(cstr,'.');
	if (p != 0)
	{
		p++;
		strcpy(UL_program_suffix,p);
	}
	else
	{
		strcpy(UL_program_suffix,"pp");
	}
/*
.....Update STATUS area with new program name
.....if not running batch
*/
	ifl=35;
	getifl(&ifl,&val);
	if (!val)
		uz_actprogram("");
	return;
}
 
/*********************************************************************
**    I_FUNCTION     :  ul_reset_unibase()
**			Delete all entities (except Viewing) from the
**			unibase.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS if no problems, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/* WHY WAS THIS STATIC */
void ul_reset_unibase()
{
	UU_KEY_ID key;
	int next_tupleid,relnum;
	char orig_label[80];
	extern UU_LOGICAL UR_changed;
	extern char UR_dpn[];
/*
.....Delete all entities in unibase
.....(except certain entities) by
.....looping thru data base
*/
	UR_dpn[0] = '\000';
/*
.....Get next entity in Unibase
*/
	next_tupleid = 1;
	while (ur_get_next_key(&next_tupleid,&key) > -1)
	{
		next_tupleid++;
/*
.....Get entity type
*/
		ur_retrieve_data_relnum(key,&relnum);
		switch (relnum)
		{
/*
.....Do not delete Viewing or Coordinate axis entities
*/
		case UV_VIEW_REL:
		case UV_VPORT_REL:
		case UV_SCREEN_REL:
		case UM_COORDSYS_REL:
			break;
/*
.....Delete drawings
*/
		case UM_DRAWING_REL:
			uc_delete(key);
			break;
/*
.....Delete all other types of entities
*/
		default:
			ur_delete_all(key);
			break;
		}
	}
/*
.....Delete layers
*/
	next_tupleid = 1;
	while (ur_get_next_tuple_index(UM_LAYER_REL,&next_tupleid) == 0)
	{
		ur_delete_tuple_abs(UM_LAYER_REL,next_tupleid);
		next_tupleid++;
	}
/*
.....Delete colors
*/
	next_tupleid = 1;
	if (ur_get_next_tuple_index(NCL_COLOR_REL,&next_tupleid) == 0)
	{
		ur_delete_tuple_abs(NCL_COLOR_REL,next_tupleid);
	}	
/*
.....Reset coordinate system axis
*/
	um_drw_mod_axis(UU_FALSE);
	um_drw_cpl_axis(UU_FALSE,UU_FALSE);
	um_setcpln_reset();
	umu_setcpln(0, orig_label);
/*
.....Reset Source Control
*/
	ncl_srcctl_free();
/*
.....Initialize modeling
*/
	um_ini_dispattr();
	um_feainit();
	um_idrwmodals();
	um_reninit();
	um_layerinit();
	ncl_init_color();
	ul_drawreset();
	ua_reset_text_pos();
/*
.....Reset Viewing
*/
	ul_default_views();
/*
.....Reset symbols
*/
	ub_init_sym_data();
/*
.....Reset picking modals
*/
	ncl_init_pick();
/*
.....Mark the Unibase as unused
*/
	UR_changed = UU_FALSE;
/*
.....Free tessellation saved data
*/
	nclc_tessmgr_free();
	return;
}
 
/*********************************************************************
**    I_FUNCTION     :  ul_default_views()
**			Resets the views back to their default settings.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void ul_default_views()
{
#define NVIEW 10
	UV_view view;
	int i;
	static char vnm[NVIEW][14]={"Front","Back","Top","Bottom","Right","Left",
		"Left Dimetric","Dimetric","Left Iso","Isometric"};
/*
.....Reset the default views
*/
	for (i=0;i<NVIEW;i++)
	{
		uv_getvnm(vnm[i],&view);
		uv_vrestore (&view);
	}
/*
.....Restore single screen view
*/
	ul_umb_screen(UU_TRUE);
/*
	uv_getvnm(vnm[0],&view);
	uv_getscnm("single",&screen);
	true = UU_TRUE;
	false = UU_FALSE;
	uv_swap_screen(&screen,&false,&false,&false,&false,&view.key);
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     :  ul_umb_screen(flag)
**			Changes the default view (single Front) to have
**			or not have a border drawn.
**    PARAMETERS   
**       INPUT  :	flag =	TRUE  = draw border.
**				FALSE = don't draw border.
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ul_umb_screen(flag)
int flag;
{
	UV_view view;
	UV_screen screen;
	static char vnm[14]={"Front"};
	int logi;
	UU_LOGICAL false,true;
/*
.....Set "Draw Border" flag.
*/
	logi = flag;
	true = UU_TRUE;
	false = UU_FALSE;
/*
.....Set screen to "Back" in order to force
....."Front" view to change graphically
*/
	strcpy (UV_act_screen[0].name,"Back");
/*
.....Set screen to "single" & "Front"
*/
	uv_getvnm(vnm,&view);
	uv_getscnm("single",&screen);
/*
.....added disp_mode for screen
.....default to shading
.....Yurong 1/15/98
*/
	uv_swap_screen(&screen,&true,&logi,&false,&false,&false, &true, &view.key);
	return;
}

/*********************************************************************
**    E_FUNCTION     : ul_drawreset()
**       Reset the drawing subsystem global variables for Unbase reset
**       (taken from um_drawinit() in model/m9edrw.c)
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_drawreset()
	{
	uu_denter(UU_MTRC,(us,"ul_drawreset()"));

	ur_put_drwmdl_curdrw(0);

	ur_put_drwmdl_unitsstr("1.0 in = 1.0 in");
	ur_put_drwmdl_drwsize(0);
	ur_put_drwmdl_drwscale((UU_REAL) 1.0);
	ur_put_drwmdl_drwunits(UM_INCH);
	ur_put_drwmdl_modscale((UU_REAL) 1.0);
	ur_put_drwmdl_modunits(UM_INCH*100 + UM_INCH);
	ur_put_drwmdl_plotprec((UU_REAL) 0.01);

	UM_plotting = UU_FALSE;		
	UM_long_dash = 0.25;					/* 1/4 inch */
	UM_long_gap = UM_long_dash / 2.0;			/* 1/8 inch */
	UM_short_dash = 0.125;					/* 1/8 inch */
	UM_short_gap = UM_short_dash / 2.0;			/* 1/16 inch */

	/* Re-initialize the drawing plotter model */
/*	NCL_plotter_model = 0;*/

	uu_dexit;
	}

