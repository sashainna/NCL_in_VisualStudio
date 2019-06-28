
/*********************************************************************
**    NAME         :  vdd1init.c
**       CONTAINS: routines to initialize DD1 viewing subsystem.
**			uv_dd1init()
**			uv_quad_screen()
**			uv_hdual_screen()
**			uv_vdual_screen()
**			uv_sing_screen()
**			uv_six_equal_screen()
**			uv_six_unequal_screen()
**			uv_set_vport()
**			uv_load_scr_layout()
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**   MODULE NAME AND RELEASE LEVEL 
**       vdd1init.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:57
**************************************************************************/
#include "usysdef.h"
#include "zsysdep.h"
#include "dinput.h"
#include "view.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "lipv.h"
#include "xfsys1.h"


void uv_quad_screen();
void uv_hdual_screen();
void uv_vdual_screen();
void uv_sing_screen();
void uv_six_equal_screen();
void uv_six_unequal_screen();
void uv_nclipv_screen();
void uv_set_vport();

typedef struct {
	char	name[40];
	UM_coord vrefpt;
	UM_vector normal;
	UM_vector vup;
	UM_length vaperture;
	int err;
} UV_view_info;

typedef struct {
	char	name[40];
	UU_REAL	x,y;
	UU_REAL	cx, cy;
	char	viewname[40];
	UU_LOGICAL	motion;
	UU_LOGICAL	bord_on;
	UU_LOGICAL	v_axis_on;
	UU_LOGICAL	name_on;
	UU_LOGICAL	aperture_on;
	int	disp_mode;
	int err;
} UV_viewport_info;
static UV_view_info view_info;
static UV_viewport_info viewport_info;
static int view_start = 0;
static int viewport_start = 0;
static int screen_start = 0;
static int view_num = 0;
static UU_KEY_ID UV_viewkey[100];
static UV_screen cur_screen;
static int screen_first = 1;

/**************************************************************************
**  E_FUNCTION:  uv_dd1init()
**      Set up default views, viewports, and screens for DD1.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_dd1init()

	{
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
	UU_KEY_ID nclipv;
	UU_KEY_ID invisible, secondary;
	UV_screen screen;
	UM_ndc lleft, vpurb;
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_dd1init()"));
/* 
......define all default views */
/*
.....ready screen layout file to define view but using fixes as default?
*/
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
	uv_nclipv_view(&nclipv);
	/* define all default screen formats and associated viewports */
/*
.....ready screen layout file to define screen  but using fixes as default?
*/
	uv_quad_screen(front, right, top, risometric);
	uv_hdual_screen(front, risometric);
	uv_vdual_screen(front, risometric);
	uv_sing_screen(front);
	uv_six_equal_screen(top, bottom, front, risometric, left, right);
	uv_six_unequal_screen(bottom, top, risometric, left, right, front);
	uv_nclipv_screen(nclipv);
	uv_special_view(&invisible, "Invisible", 3);
	uv_special_view(&secondary, "Extern Unibase", 4);
/*
.....load screen layout file
*/
	uv_load_scr_layout(0);

	uv_getscnm("single",&screen);
	uv_activsc(&screen);

	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_quad_screen(lleftview, lrightview, uleftview, urightview);
**      Define a quad screen format and associate the specified views
**			with the defined viewports.
**  PARAMETERS   
**      INPUT  : 
**				lleftview				view to assoicate with lower left viewport
**				lrightview				view to assoicate with right left viewport
**				uleftview				view to assoicate with upper left viewport
**				urightview				view to assoicate with upper right viewport
**      OUTPUT :
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_quad_screen(lleftview, lrightview, uleftview, urightview)
	UU_KEY_ID lleftview;
	UU_KEY_ID lrightview;
	UU_KEY_ID uleftview;
	UU_KEY_ID urightview;

	{
	UV_screen screen;
	UU_KEY_ID screen1;
	UM_ndc vpurb;
	UM_ndc vpllf;
	UM_ndc lleft;
	UM_ndc uright;
	UM_ndc midpt;

	uu_denter(UU_MTRC,(us,"uv_quad_screen()"));

	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, lleft);
	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 0.0, uright);
	um_xyztovc((UU_REAL) 0.5, (UU_REAL) 0.5, (UU_REAL) 0.5, midpt);

	/* create a quad screen format in UNIBSE */
	screen1 = uv_scdefine("quad", &screen, 0);

	/* define the lower left viewport */
	um_xyztovc(midpt[0], midpt[1], uright[2], vpurb);
	uv_set_vport(lleftview, "quad_lleft", lleft, vpurb, &screen, 0);

	/* define the lower right viewport */
	um_xyztovc(midpt[0], lleft[1], lleft[2], vpllf);
	um_xyztovc(uright[0], midpt[1], uright[2], vpurb);
	uv_set_vport(lrightview, "quad_lright", vpllf, vpurb, &screen, 0);

	/* define the upper left viewport */
	um_xyztovc(lleft[0], midpt[1], lleft[2], vpllf);
	um_xyztovc(midpt[0], uright[1], uright[2], vpurb);
	uv_set_vport(uleftview, "quad_uleft", vpllf, vpurb, &screen, 0);

	/* make a square isometric view */
	um_xyztovc(midpt[0], midpt[1], lleft[2], vpllf);
	uv_set_vport(urightview, "quad_uright", vpllf, uright, &screen, 0);

	/* update the quad screen format */
	uv_putsc(&screen);

	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_hdual_screen(bottomview, topview)
**      Define a horizontal dual screen format and associate the
**			specified views with the defined viewports.
**  PARAMETERS   
**      INPUT  : 
**				bottomview					view to associate with bottom viewport
**				topview						view to associate with top viewport
**      OUTPUT :
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_hdual_screen(bottomview, topview)
	UU_KEY_ID bottomview;
	UU_KEY_ID topview;

	{
	UV_screen screen;
	UU_KEY_ID screen1;
	UM_ndc vpurb;
	UM_ndc vpllf;
	UM_ndc lleft;
	UM_ndc uright;
	UM_ndc midpt;

	uu_denter(UU_MTRC,(us,"uv_hdual_screen()"));

	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, lleft);
	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 0.0, uright);
	um_xyztovc((UU_REAL) 0.5, (UU_REAL) 0.5, (UU_REAL) 0.5, midpt);

	/* make a horizontal dual view screen */
	screen1 = uv_scdefine("horiz. dual", &screen, 0);

	/* define the bottom viewport */
	um_xyztovc(uright[0], midpt[1], uright[2], vpurb);
	uv_set_vport(bottomview, "hdual_bottom", lleft, vpurb, &screen, 0);

	/* define the top viewport */
	um_xyztovc(lleft[0], midpt[1], lleft[2], vpllf);
	uv_set_vport(topview, "hdual_top", vpllf, uright, &screen, 0);

	uv_putsc(&screen);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_vdual_screen(leftview, rightview)
**      Define a vertical dual screen format and associate the
**			specified views with the defined viewports.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_vdual_screen(leftview, rightview)
	UU_KEY_ID leftview;
	UU_KEY_ID rightview;

	{
	UV_screen screen;
	UU_KEY_ID screen1;
	UM_ndc vpurb;
	UM_ndc vpllf;
	UM_ndc lleft;
	UM_ndc uright;
	UM_ndc midpt;

	uu_denter(UU_MTRC,(us,"uv_vdual_screen()"));

	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, lleft);
	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 0.0, uright);
	um_xyztovc((UU_REAL) 0.5, (UU_REAL) 0.5, (UU_REAL) 0.5, midpt);

	/* make a vertical dual view screen */
	screen1 = uv_scdefine("vert. dual", &screen, 0);

	/* define the left viewport */
	um_xyztovc(midpt[0], uright[1], uright[2], vpurb);
	uv_set_vport(leftview, "vdual_left", lleft, vpurb, &screen, 0);

	/* define the right viewport */
	um_xyztovc(midpt[0], lleft[1], lleft[2], vpllf);
	uv_set_vport(rightview, "vdual_right", vpllf, uright, &screen, 0);

	/* update the screen in UNIBASE */
	uv_putsc(&screen);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_sing_screen(view)
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
void uv_sing_screen(view)
	UU_KEY_ID view;

	{
	UV_screen screen;
	UU_KEY_ID screen1;
	UM_ndc lleft;
	UM_ndc uright;

	uu_denter(UU_MTRC,(us,"uv_sing_screen()"));

	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, lleft);
	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 0.0, uright);

	/* make a single view screen */
	screen1 = uv_scdefine("single", &screen, 0);

	/* define the viewport */
	uv_set_vport(view, "single", lleft, uright, &screen, 0);

	/* update screen in UNIBASE */
	uv_putsc(&screen);

	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_six_equal_screen(lleftview, lmiddleview, lrightview,
**												uleftview, umiddleview, urightview)
**      Define a six equal screen format and associate the
**			specified views with the defined viewports.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_six_equal_screen(lleftview, lmiddleview, lrightview,
	uleftview, umiddleview, urightview)
	UU_KEY_ID lleftview;
	UU_KEY_ID lmiddleview;
	UU_KEY_ID lrightview;
	UU_KEY_ID uleftview;
	UU_KEY_ID umiddleview;
	UU_KEY_ID urightview;

	{
	UV_screen screen ;
	UU_KEY_ID screen1;
	UM_ndc vpurb;
	UM_ndc vpllf;
	UM_ndc midpt;
	UM_ndc lleft, uright;
	UM_ndc lefthird, righthird, lowerthird, upperthird;

	uu_denter(UU_MTRC,(us,"uv_six_equal_screen()"));

	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, lleft);
	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 0.0, uright);
	um_xyztovc((UU_REAL) 0.5, (UU_REAL) 0.5, (UU_REAL) 0.5, midpt);
	um_xyztovc((UU_REAL) 0.3333, (UU_REAL) 0.5, (UU_REAL) 0.5, lefthird);
	um_xyztovc((UU_REAL) 0.6667, (UU_REAL) 0.5, (UU_REAL) 0.5, righthird);
	um_xyztovc((UU_REAL) 0.3333, (UU_REAL) 0.3333, (UU_REAL) 0.5, lowerthird);
	um_xyztovc((UU_REAL) 0.3333, (UU_REAL) 0.6667, (UU_REAL) 0.5, upperthird);

	/* Set up six equal size windows */
	screen1 = uv_scdefine("six equal", &screen, 0);

	/* define the left viewport */
	um_xyztovc(lefthird[0], midpt[1], uright[2], vpurb);
	uv_set_vport(lleftview, "6eq_lleft", lleft, vpurb, &screen, 0);

	/* make a square bottom view */
	um_xyztovc(lefthird[0], lleft[1], lleft[2], vpllf);
	um_xyztovc(righthird[0], midpt[1], uright[2], vpurb);
	uv_set_vport(lmiddleview, "6eq_lmiddle", vpllf, vpurb, &screen, 0);

	/* make the lower right viewport */
	um_xyztovc(righthird[0], lleft[1], lleft[2], vpllf);
	um_xyztovc(uright[0], midpt[1], uright[2], vpurb);
	uv_set_vport(lrightview, "6eq_lright", vpllf, vpurb, &screen, 0);

	/* make the upper left viewport */
	um_xyztovc(lleft[0], midpt[1], lleft[2], vpllf);
	um_xyztovc(lefthird[0], uright[1], uright[2], vpurb);
	uv_set_vport(uleftview, "6eq_uleft", vpllf, vpurb, &screen, 0);

	/* make the upper middle viewport */
	um_xyztovc(lefthird[0], midpt[1], lleft[2], vpllf);
	um_xyztovc(righthird[0], uright[1], uright[2], vpurb);
	uv_set_vport(umiddleview, "6eq_umiddle", vpllf, vpurb, &screen, 0);

	/* make the upper right viewport */
	um_xyztovc(righthird[0], midpt[1], lleft[2], vpllf);
	uv_set_vport(urightview, "6eq_uright", vpllf, uright, &screen, 0);

	/* update screen in UNIBASE */
	uv_putsc(&screen);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_six_unequal_screen(lleftview, lmiddleview,
**								uleftview, umiddleview, urightview, largeview)
**      Set up default views, viewports, for six unequal size viewports 
**			on the screen
**  PARAMETERS   
**      INPUT  :  
**				lleftview					view to associate with lower left viewport
**				lmiddleview					view to associate with left middle viewport
**				uleftview					view to associate with upper left viewport
**				umiddleview					view to associate with upper middle viewport
**				urightview					view to associate with upper right viewport
**				largeview					view to associate with large viewport
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_six_unequal_screen(lleftview, lmiddleview,
	uleftview, umiddleview, urightview, largeview)
	UU_KEY_ID lleftview;
	UU_KEY_ID lmiddleview;
	UU_KEY_ID uleftview;
	UU_KEY_ID umiddleview;
	UU_KEY_ID urightview;
	UU_KEY_ID largeview;

	{
	UV_screen screen;
	UU_KEY_ID screen1;
	UM_ndc vpurb;
	UM_ndc vpllf;
	UM_ndc lleft, uright;
	UM_ndc midpt;
	UM_ndc lefthird, righthird, lowerthird, upperthird;

	uu_denter(UU_MTRC,(us,"uv_six_unequal_screen()"));

	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, lleft);
	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 0.0, uright);
	um_xyztovc((UU_REAL) 0.5, (UU_REAL) 0.5, (UU_REAL) 0.5, midpt);
	um_xyztovc((UU_REAL) 0.3333, (UU_REAL) 0.5, (UU_REAL) 0.5, lefthird);
	um_xyztovc((UU_REAL) 0.6667, (UU_REAL) 0.5, (UU_REAL) 0.5, righthird);
	um_xyztovc((UU_REAL) 0.3333, (UU_REAL) 0.3333, (UU_REAL) 0.5, lowerthird);
	um_xyztovc((UU_REAL) 0.3333, (UU_REAL) 0.6667, (UU_REAL) 0.5, upperthird);

	/* Set up six equal size windows */
	screen1 = uv_scdefine("five and one", &screen, 0);

	/* define the lower left viewport */
	um_xyztovc(lefthird[0], lowerthird[1], uright[2], vpurb);
	uv_set_vport(lleftview, "6uneq_lleft", lleft, vpurb, &screen, 0);

	/* make a middle left viewport */
	um_xyztovc(lleft[0], lowerthird[1], lleft[2], vpllf);
	um_xyztovc(lefthird[0], upperthird[1], uright[2], vpurb);
	uv_set_vport(lmiddleview, "6uneq_lmiddle", vpllf, vpurb, &screen, 0);

	/* make a upper left viewport */
	um_xyztovc(lleft[0], upperthird[1], lleft[2], vpllf);
	um_xyztovc(lefthird[0], uright[1], uright[2], vpurb);
	uv_set_vport(uleftview, "6uneq_uleft", vpllf, vpurb, &screen, 0);

	/* make a upper middle viewport */
	um_xyztovc(lefthird[0], upperthird[1], lleft[2], vpllf);
	um_xyztovc(righthird[0], uright[1], uright[2], vpurb);
	uv_set_vport(umiddleview, "6uneq_umiddle", vpllf, vpurb, &screen, 0);

	/* make the upper right viewport */
	um_xyztovc(righthird[0], upperthird[1], lleft[2],vpllf);
	uv_set_vport(urightview, "6uneq_uright", vpllf, uright, &screen, 0);

	/* make a large viewport */
	um_xyztovc(lefthird[0], lleft[1], lleft[2], vpllf);
	um_xyztovc(uright[0], upperthird[1], uright[2], vpurb);
	uv_set_vport(largeview, "6uneq_large", vpllf, vpurb, &screen, 0);

	/* update screen in UNIBASE */
	uv_putsc(&screen);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_nclipv_screen(view)
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
void uv_nclipv_screen(view)
UU_KEY_ID view;
{
	UU_KEY_ID vport1 ;
	UM_ndc lleft;
	UM_ndc uright;

	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, lleft);
	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 1.0, (UU_REAL) 0.0, uright);

/*
.....Define the nclipv viewport
*/
	vport1 = uv_vpdefine("nclipv", &LW_vport,0);
	uv_setvpbox(&LW_vport, lleft, uright);
	uv_vtovp(&LW_vport, view);
	uv_putvp(&LW_vport);
}

/**************************************************************************
**  E_FUNCTION:  uv_set_vport()
**      Set up views, viewports.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_set_vport(viewkey, vpname, vpllf, vpurb, screen, flag)
	UU_KEY_ID viewkey;
	char	vpname[15];
	UM_ndc vpllf;
	UM_ndc vpurb;
	UV_screen *screen;
	int flag;

	{
	UU_KEY_ID	vport1;
	UV_vport		vport;
	int			i,vps,wire;
	char *p,*ux_getenv(),buf[80];

	uu_denter(UU_MTRC,(us,"uv_set_vport()"));

	vport1 = uv_vpdefine(vpname, &vport, flag);
	if  (vport1==-1)
		return;
	uv_setvpbox(&vport, vpllf, vpurb);
	uv_vtovp(&vport, viewkey);
	uv_setvpbord(&vport, 1);
/*
.....Enable motion in viewport
.....Bobby  -  11/30/92
*/
	uv_setvpmot(&vport,1);
/*
.....Get shaded default
*/
	vps = 2; wire = 1;
	p = ux_getenv("UV_VPORT_DISPLAY",UX_NPRTERRS);
	if (p != UU_NULL)
	{
		strcpy(buf,p);
		ul_to_upper(buf);
		if (strcmp(buf,"WIREFRAME") == 0) vps = 1;
		else if (strcmp(buf,"HIDDEN") == 0) vps = 3;
		else if (strcmp(buf,"SHADED_ONLY") == 0) wire = 0;
	}
	uv_setvpshad(&vport,vps);
	uv_setvpframe(&vport,wire);
	uv_putvp(&vport);

	for (i = 0; i < UV_NVPORTS; i++)
		{
		if (UV_view_to_vport.vport_key[i] == 0)
			{
			UV_view_to_vport.vport_key[i] = vport1;
			UV_view_to_vport.view_key [i] = viewkey;
			break;
			}
		}

	uv_vptosc(screen, vport1);
	uu_dexit;
	}
/*********************************************************************
**       I_FUNCTION : uv_playout_view(ctyp,cmsg)
**              This function defines the view.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int uv_playout_view(ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,n,status,inum;
	char msg[256];
	int maxsub=5;
	static char csub[5][20] = {"NAME","CENTER", "NORMAL", "UP-AXIS", "SCALE"};
	UU_REAL rval[3];
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		if (screen_first)
		{
			sprintf (msg, "Not a valid View pararmeter.  /%s/ %s\n", ctyp,cmsg);
			ud_printmsg(msg);
		}
		goto failed;
	}
	switch(i)
	{
/*
.....name
*/
	case 0:
		strcpy(view_info.name, cmsg);
		break;
/*
.....center
*/
	case 1:
		if ((ul_to_reals(rval,&inum,3,cmsg) != UU_SUCCESS) ||
			inum != 3) goto bad_parm;
		view_info.vrefpt[0] = rval[0];
		view_info.vrefpt[1] = rval[1];
		view_info.vrefpt[2] = rval[2];
		break;
/*
.....normal
*/
	case 2:
		if ((ul_to_reals(rval,&inum,3,cmsg) != UU_SUCCESS) ||
			inum != 3) goto bad_parm;
		view_info.normal[0] = rval[0];
		view_info.normal[1] = rval[1];
		view_info.normal[2] = rval[2];
		break;
/*
.....up-axis
*/
	case 3:
		if ((ul_to_reals(rval,&inum,3,cmsg) != UU_SUCCESS) ||
			inum != 3) goto bad_parm;
		view_info.vup[0] = rval[0];
		view_info.vup[1] = rval[1];
		view_info.vup[2] = rval[2];
		break;
/*
.....scale
*/
	case 4:
		if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		view_info.vaperture = rval[0];
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	if (screen_first)
	{
		sprintf (msg, "Not a valid View pararmeter.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
	}
/*
.....Failure
*/
failed:;
	view_info.err = 1;
	status = UU_FAILURE;
done:;
	return(status);
}



/*********************************************************************
**       I_FUNCTION : uv_playout_viewport(ctyp,cmsg)
**              This function defines the viewport.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int uv_playout_viewport(ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,n,status,inum;
	char msg[256];
	int maxsub=10;
	static char csub[10][20] = {"VIEWPORT","POSITION", "SIZE", "VIEW",
			"MOTION", "BORDER", "AXIS","NAME","APERTURE", "DISPLAY"};
	UU_REAL rval[3];
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		if (screen_first)
		{
			sprintf (msg, "Not a valid Viewport pararmeter.  /%s/ %s\n", ctyp,cmsg);
			ud_printmsg(msg);
		}
		goto failed;
	}
	switch(i)
	{
/*
.....viewport name
*/
	case 0:
		strcpy(viewport_info.name, cmsg);
		break;
/*
.....position
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		viewport_info.x = rval[0];
		viewport_info.y = rval[1];
		break;
/*
.....size
*/
	case 2:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		viewport_info.cx = rval[0];
		viewport_info.cy = rval[1];
		break;
/*
.....view name
*/
	case 3:
		strcpy(viewport_info.viewname, cmsg);
		break;
/*
......motion
*/
	case 4:
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "*YES")==0)
			viewport_info.motion = 1;
		else
			viewport_info.motion = 0;
		break;
/*
......border
*/
	case 5:
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "*YES")==0)
			viewport_info.bord_on = 1;
		else
			viewport_info.bord_on = 0;
		break;
/*
......Axis
*/
	case 6:
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "*YES")==0)
			viewport_info.v_axis_on = 1;
		else
			viewport_info.v_axis_on = 0;
		break;
/*
......name label on
*/
	case 7:
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "*YES")==0)
			viewport_info.name_on = 1;
		else
			viewport_info.name_on = 0;
		break;
/*
......aperture on
*/
	case 8:
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "*YES")==0)
			viewport_info.aperture_on = 1;
		else
			viewport_info.aperture_on = 0;
		break;
/*
......display type
*/
	case 9:
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "*SHADED")==0)
			viewport_info.disp_mode = 2;
		else if (strcmp(cmsg, "*WIRE")==0)
			viewport_info.disp_mode = 1;
		else if (strcmp(cmsg, "HIDDLE")==0)
			viewport_info.disp_mode = 3;
		else if (strcmp(cmsg, "*SHADED_ONLY")==0)
			viewport_info.disp_mode = 4;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	if (screen_first)
	{
		sprintf (msg, "Not a valid Viewport pararmeter.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
	}
/*
.....Failure
*/
failed:;
	viewport_info.err = 1;
	status = UU_FAILURE;
done:;
	return(status);
}

/**************************************************************************
**  E_FUNCTION:  uv_load_scr_layout(int flag)
**      Load the scrren layout file
**		
**  PARAMETERS   
**      INPUT  : 
**				flag: 1: call from initial uv_dd1init roution
**							will overwrite the default screen/view definition
**					  0: if the screen already exist in the unibase
**							will not overwrite the screen/view definition
**      OUTPUT :
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_load_scr_layout(flag)
int flag;
{	
	UV_screen screen;
	UM_ndc lleft, vpurb;
	UV_view view;
	UU_KEY_ID keyid;
	UV_vport		vport;
	UX_pathname filename, mfilename,dir,fname;
	char *pathptr;
	char buf[80],ctyp[80],cmsg[80], msg[UX_MAX_PATH_LEN+40];
	int status,stat,numint,ityp,i,m, isub,istat;
	FILE *fptr = NULL;
	int maxsub=2;
	static char csub[2][10]={"VIEW","SCREEN"};
/*
.....Assume success
*/
	status = UU_SUCCESS;
	pathptr = UU_NULL;

	status = ux_get_syspath("UV_SCREEN_LAYOUT",&pathptr,filename,&fptr,UX_PRTERRS);		
	ux_strip_quotes(filename);
	if(filename[0]=='\0')
	{
/*
......don't output error message
*/
/*
  		ud_printmsg("symbol UV_SCREEN_LAYOUT not defined\n");
*/
		return;
  	}
/*
.....Check for screen file
*/
	fptr = fopen(filename,"r");
	if (fptr == 0) 
	{
/*
......don't output error message
*/
/*		sprintf (msg, "Cannot open screen Layout file %s.\n",filename);
		ud_printmsg(msg);
*/
		goto done;
	}
	viewport_info.err = 0;
	view_info.err = 0;
/*
.....Read a record
*/
	do
	{
		stat = ul_fread (fptr,buf,sizeof(buf),&numint);
		if (stat == UX_EOF)
		{ 
			if (numint<=0)
				goto done;
			buf[numint+1] = '\0';
		}
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			if (feof(fptr)) goto done;
			if (screen_first)
			{
				sprintf (msg, "Error reading from Layout file %s.\n",filename);
				ud_printmsg(msg);
			}
			goto done;
		}
/*
.....Check for record type
*/
		istat = ul_modal_check (buf,&ityp,ctyp,cmsg);
/*
.....Invalid syntax
*/
		if ((istat != UU_SUCCESS) && (screen_first))
		{
			sprintf (msg, "Screen layout file syntax error. %s\n",buf);
			ud_printmsg(msg);
		}
/*
.....Subsystem type
*/
		switch (ityp)
		{
		case 1:
			for (i=0;i<maxsub;i++)
			{
				ul_to_upper(ctyp);
				if (strcmp(ctyp,csub[i]) == 0) break;
			}
			if (i >= maxsub)
			{
				if (screen_first)
				{
					sprintf (msg, "Not a valid Screen layout parameter. %s\n",buf);
					ud_printmsg(msg);
				}
				break;
			}
			isub = i + 1;
				
			if (view_start==1)
			{			
/*
.....have finished reading view, create this view
*/
				if (view_info.err==0)
				{
					status = uv_create_view(view_info.name, UV_USERDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
								view_info.vrefpt, view_info.vup, view_info.normal, view_info.vaperture,
								UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
								&(UV_viewkey[view_num++]), flag);
				}
				view_info.err = 0;
				view_start = 0;
			}
			if (viewport_start==1)
			{
				status = uv_getvpnm(viewport_info.name, &vport);
/*
.....only if there is no syntax errs reading viewport info 
.....and the viewport doesn't defined before, we set the vport 
*/
				if ((viewport_info.err==0) && (status==UU_FAILURE))
				{
					

					lleft[0] = viewport_info.x;
					lleft[1] = viewport_info.y;
					lleft[2] = 0;
					vpurb[0] = viewport_info.x + viewport_info.cx;
					vpurb[1] = viewport_info.y + viewport_info.cy;
					vpurb[2] = 0;
/*
.....find the view key from viewname (viewport_info.name)
*/
					status = uv_getvnm(viewport_info.viewname, &view);
					if (status != UU_SUCCESS )
/*
.....use 'Front" view
*/				
					{
						strcpy(viewport_info.viewname, "Front");
						uv_getvnm(viewport_info.viewname, &view);
					}
					uv_set_vport(view.key, viewport_info.name, lleft, vpurb, &cur_screen,flag);
					uv_getvpnm(viewport_info.name, &vport);
					vport.motion = viewport_info.motion;
					vport.bord_on = viewport_info.bord_on;
					vport.v_axis_on = viewport_info.v_axis_on;
					vport.name_on = viewport_info.name_on;
					vport.aperture_on = viewport_info.aperture_on;
					if (viewport_info.disp_mode==1)
					{
						vport.disp_mode = 1;
						vport.wireframe = 1;
					}
					else if (viewport_info.disp_mode==2)
					{
						vport.disp_mode = 2;
						vport.wireframe = 1;
					}
					else if (viewport_info.disp_mode==4)
					{
						vport.disp_mode = 2;
						vport.wireframe = 0;
					}
					else if (viewport_info.disp_mode==3)
					{
						vport.disp_mode = 3;
						vport.wireframe = 1;
					}
					uv_vtovp(&vport, view.key);
					uv_putvp(&vport);
				}
				else if (screen_first)
				{
					if (viewport_info.err==1)
						sprintf (msg, "Viewport %s in Screen %s can't created\n (Bad Viewport info).\n",
							viewport_info.name, cur_screen.name);
					else
						sprintf (msg, "Viewport %s in Screen %s can't created\n (Viewport have already defined).\n",
							viewport_info.name, cur_screen.name);
					ud_printmsg(msg);
				}
				viewport_info.err = 0;
				viewport_start = 0;
			}
			if (screen_start==1)
			{
/*
......adjust screen viewport before put into database
*/
				status = uv_adjust_screen(&cur_screen);
				if (status!=-1)
					uv_putsc(&cur_screen);
				else
				{
/*
.....remove this error screen
*/
					ur_delete_all(cur_screen.key);
				}
				screen_start = 0;
			}
			if (isub==1)
				view_start = 1;
			if (isub==2)
				screen_start = 1;
			break;
		case 2:
			switch (isub)
			{
			case 1:
				uv_playout_view (ctyp,cmsg);
				break;
			case 2:
				{
				if (view_start==1)
				{
					if (screen_first)
					{
						sprintf (msg, "Not a valid view parameter. %s\n",buf);
						ud_printmsg(msg);
					}
					break;
				}
				ul_to_upper(ctyp);
				if ((screen_start)&&(viewport_start==0) && (strcmp(ctyp,"NAME") == 0))
				{
					keyid = uv_scdefine(cmsg, &cur_screen, flag);
					if (keyid==-1)
						screen_start = 0;
				}
				else if ((screen_start)&&(viewport_start==0) && (strcmp(ctyp,"VIEWPORT") == 0))
				{
					uv_playout_viewport(ctyp,cmsg);
					viewport_start = 1;
				}
				else if ((screen_start)&&(viewport_start==1) && (strcmp(ctyp,"VIEWPORT") == 0))
				{
					status = uv_getvpnm(viewport_info.name, &vport);
/*
.....only if there is no syntax errs reading viewport info 
.....and the viewport doesn't defined before, we set the vport 
*/
					if ((viewport_info.err==0) && (status==UU_FAILURE))
					{
						lleft[0] = viewport_info.x;
						lleft[1] = viewport_info.y;
						lleft[2] = 0;
						vpurb[0] = viewport_info.x + viewport_info.cx;
						vpurb[1] = viewport_info.y + viewport_info.cy;
						vpurb[2] = 0;
/*
.....find the view key from viewname (viewport_info.name)
*/
						status = uv_getvnm(viewport_info.viewname, &view);
						if (status != UU_SUCCESS )
/*
.....use 'Front" view
*/		
						{
							strcpy(viewport_info.viewname, "Front");
							uv_getvnm(viewport_info.viewname, &view);
						}
						uv_set_vport(view.key, viewport_info.name, lleft, vpurb, &cur_screen,flag);
						uv_getvpnm(viewport_info.name, &vport);
						vport.motion = viewport_info.motion;
						vport.bord_on = viewport_info.bord_on;
						vport.v_axis_on = viewport_info.v_axis_on;
						vport.name_on = viewport_info.name_on;
						vport.aperture_on = viewport_info.aperture_on;
						if (viewport_info.disp_mode==1)
						{
							vport.disp_mode = 1;
							vport.wireframe = 1;
						}
						else if (viewport_info.disp_mode==2)
						{
							vport.disp_mode = 2;
							vport.wireframe = 1;
						}
						else if (viewport_info.disp_mode==4)
						{
							vport.disp_mode = 2;
							vport.wireframe = 0;
						}
						else if (viewport_info.disp_mode==3)
						{
							vport.disp_mode = 3;
							vport.wireframe = 1;
						}
						uv_vtovp(&vport, view.key);
						uv_putvp(&vport);
					}
					else if (screen_first)
					{
						if (viewport_info.err==1)
							sprintf (msg, "Viewport %s in Screen %s can't created\n (Bad Viewport info).\n",
								viewport_info.name, cur_screen.name);
						else
							sprintf (msg, "Viewport %s in Screen %s can't created\n (Viewport have already defined).\n",
								viewport_info.name, cur_screen.name);
						ud_printmsg(msg);
					}
					viewport_info.err = 0;
					uv_playout_viewport(ctyp,cmsg);
				}
				else if (screen_start)
					uv_playout_viewport(ctyp,cmsg);
				break;
				}
			}
			break;
		}
	}
	while (stat == UU_SUCCESS);
done:;
	if (view_start==1)
	{			
/*
.....have finished reading view, create this view
*/
		if (view_info.err==0)
		{
			status = uv_create_view(view_info.name, UV_USERDEF_VIEW, UV_PARALLEL, (UU_REAL) 0.0,
							view_info.vrefpt, view_info.vup, view_info.normal, view_info.vaperture,
							UV_NOCLIP, (UU_REAL) 10000.0, (UU_REAL) -10000.0,
							&(UV_viewkey[view_num++]), flag);
		}
		view_info.err = 0;
		view_start = 0;
	}
	if ((viewport_start==1) && (screen_start))
	{
		status = uv_getvpnm(viewport_info.name, &vport);
/*
.....only if there is no syntax errs reading viewport info 
.....and the viewport doesn't defined before, we set the vport 
*/
		if ((viewport_info.err==0) && (status==UU_FAILURE))
		{
			lleft[0] = viewport_info.x;
			lleft[1] = viewport_info.y;
			lleft[2] = 0;
			vpurb[0] = viewport_info.x + viewport_info.cx;
			vpurb[1] = viewport_info.y + viewport_info.cy;
			vpurb[2] = 0;
/*
.....find the view key from viewname (viewport_info.name)
*/
			status = uv_getvnm(viewport_info.viewname, &view);
			if (status != UU_SUCCESS )
/*
.....use 'Front" view
*/		
			{
				strcpy(viewport_info.viewname, "Front");
				uv_getvnm(viewport_info.viewname, &view);
			}
			uv_set_vport(view.key, viewport_info.name, lleft, vpurb, &cur_screen,flag);
					
			uv_getvpnm(viewport_info.name, &vport);
			vport.motion = viewport_info.motion;
			vport.bord_on = viewport_info.bord_on;
			vport.v_axis_on = viewport_info.v_axis_on;
			vport.name_on = viewport_info.name_on;
			vport.aperture_on = viewport_info.aperture_on;
			if (viewport_info.disp_mode==1)
			{
				vport.disp_mode = 1;
				vport.wireframe = 1;
			}
			else if (viewport_info.disp_mode==2)
			{
				vport.disp_mode = 2;
				vport.wireframe = 1;
			}
			else if (viewport_info.disp_mode==4)
			{
				vport.disp_mode = 2;
				vport.wireframe = 0;
			}
			else if (viewport_info.disp_mode==3)
			{
				vport.disp_mode = 3;
				vport.wireframe = 1;
			}
			uv_vtovp(&vport, view.key);
			uv_putvp(&vport);
		}
		else if (screen_first)
		{
			if (viewport_info.err==1)
				sprintf (msg, "Viewport %s in Screen %s can't created\n (Bad Viewport info).\n",
							viewport_info.name, cur_screen.name);
			else
				sprintf (msg, "Viewport %s in Screen %s can't created\n (Viewport have already defined).\n",
							viewport_info.name, cur_screen.name);
			ud_printmsg(msg);
		}
		viewport_info.err = 0;
		viewport_start = 0;
	}
	if (screen_start==1)
	{
/*
......adjust screen viewport before put into database
*/
		status = uv_adjust_screen(&cur_screen);
		if (status!=-1)
			uv_putsc(&cur_screen);
		else
		{
/*
.....remove this error screen
*/
			ur_delete_all(cur_screen.key);
		}
		screen_start = 0;
	}
	if (fptr!=NULL)
		fclose(fptr);
	screen_first = 0;
}
/**************************************************************************
**  E_FUNCTION:  uv_adjust_screen(screen)
**      adjust vports in this screen
**  PARAMETERS   
**      INPUT  :  screen to be adjusted
**      OUTPUT :  none
**  RETURNS      :  0: OK
**					-1: problems
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_adjust_screen(screen)
UV_screen *screen;
{
	int i, status, nc;
	UV_vport	vport;
	UU_REAL llf[20][2], urf[20][2];
	for (i=0;i<screen->nvports;i++)
	{
		status = uv_getvpid(screen->vports[i], &vport);
		llf[i][0] = vport.llf[0];
		llf[i][1] = vport.llf[1];
		urf[i][0] = vport.urb[0];
		urf[i][1] = vport.urb[1];
	}
	nc = screen->nvports;
	if (nc<=0) return -1;
	ncl_fix_boxes (llf, urf, nc);
	for (i=0;i<screen->nvports;i++)
	{
		status = uv_getvpid(screen->vports[i], &vport);
		vport.llf[0] = llf[i][0];
		vport.llf[1] = llf[i][1];
		vport.urb[0] = urf[i][0];
		vport.urb[1] = urf[i][1];
		uv_putvp(&vport);
	}
}
