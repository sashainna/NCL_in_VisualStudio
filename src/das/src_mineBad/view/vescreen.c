
/*********************************************************************
**    NAME         :  vescreen.c
**       CONTAINS:  Routines to manipulate screens
**			UU_KEY_ID uv_scdefine(name, screen)
**			uv_setscws(screen, ws)
**			uv_vptosc(screen, vport)
**			uv_activsc(screen)
**			uv_set_screen(screen, display)
**			uv_updatesc_display()
**			uv_deactivsc()
**			uv_chgsc(name)
**			uv_getscnm(name, screen)
**			uv_getscid(key, screen)
**			uv_swap_screen(newscreen, display_motion,
**				draw_vp_border, draw_vp_axis,
**				draw_vp_name, draw_vp_aperture, new_vp_cur_view)
**			uv_screname(screen, name)
**			uv_putsc(screen)
**			uv_actscreen(screen)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       vescreen.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:58
**************************************************************************/
#include "udebug.h"
#include "usysdef.h"
#include "unserve.h"
#include "view.h"
#include "zsysdep.h"
#include "mdrel.h"
#include "mdcpln.h"
#include "mromcom.h"
#include "lipv.h"

#include "go1.h"
#include "ginqst.h"
#include "gsegac.h"

#include "nclfc.h"
#include "uhep.h"

extern int UD_ksws;
extern Gseg NCL_mot_seg;
extern int UZ_nclipv_view;
extern int UR_active;

void uv_set_screen();
void uv_deactivsc();
void uv_putsc();

/**************************************************************************
**  E_FUNCTION:  UU_KEY_ID uv_scdefine(name, screen, flag)
**      Define a default screen and create the screen entity in Unibase
**  PARAMETERS   
**      INPUT  :  name	: name to be given to screen
**					screen: pointer to a UV_screen structure
**					flag: overwrite flag: 1: overwrite the screen
**										  0: not overwrite
**      OUTPUT :  none
**  RETURNS      :  key of screen defined
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
UU_KEY_ID uv_scdefine(name, screen, flag)
	char 			name[15];			/* name of screen								*/
	UV_screen	*screen;				/* screen to be defined						*/
	int flag;
	{
	int i;
	UV_screen oldsc;
	int stat;

	uu_denter(UU_MTRC,(us,"uv_scdefine(name=%s)",name));

	stat = uv_getscnm(name, &oldsc);
	if (stat == UU_SUCCESS)
	{
/*
.....screen already exist
*/
/*
......if not overwrite, just return -1;
*/
		if (flag==0)
			return -1;
/*
.....remove old screen first
*/
		ur_delete_all(oldsc.key);
	}

	for (i = 0; i < UV_NVPORTS; i++)
		screen->vports[i] = 0;

	screen->nvports = 0;
	screen->active = UV_INACTIVE;
	screen->wstation = -1;
	strcpy(screen->name, name);
	screen->rel_num = UV_SCREEN_REL;
	ur_create_data(screen);

	uu_dexit;
	return(screen->key);
}

/**************************************************************************
**  E_FUNCTION:  uv_setscws(screen, ws)
**      Set the workstation for this screen
**  PARAMETERS   
**      INPUT  :  none
**      INPUT  :  screen	: pointer to a screen structure
**      OUTPUT :  screen	: changed screen
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setscws(screen, ws)
	UV_screen  *screen;
	int			ws;

	{
	uu_denter(UU_MTRC,(us,"uv_setscws(key=%x)",screen->key));
	screen->wstation = ws;
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_vptosc(screen, vport)
**      Associate this viewport with the screen.
**  PARAMETERS   
**      INPUT  :  screen	: pointer to a screen structure
**						vport		: key of viewport to be associated with view
**      OUTPUT :  screen	: changed screen
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_vptosc(screen, vport)
	UV_screen  *screen;
	UU_KEY_ID  vport;

	{
	int i;

	uu_denter(UU_MTRC,(us,"uv_vptosc(sckey=%x,vpkey=%x)",
							screen->key,vport));

	for (i = 0; i < UV_NVPORTS; i++)
		if (screen->vports[i] == 0) break;

	if (i != UV_NVPORTS)
		{
		screen->nvports++;
		screen->vports[i] = vport;
		}

	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_activsc(screen)
**      Use this screen to display geometry on the workstation
**  PARAMETERS   
**      INPUT  :  screen	:	screen entity to use to display geometry
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_activsc(screen)
	UV_screen	*screen;

	{
	uu_denter(UU_MTRC,(us,"uv_activsc(%s)",screen->name));
/*
.....If now it is plotting to plotter
.....We don't need drawing in the scrren
.....Yurong 9/8/97
*/
	if (uj_miplotting() == UU_TRUE || um_is_pocket_graphics())
	{
		uv_set_screen(screen, UU_FALSE);
		return ;
	}
	uv_clear();

	/* set up the new screen and the corresponding global values */
	uv_set_screen(screen, UU_TRUE);

	/* redisplay all objects that are stored in Unibase */
	uv_dispobjs();
/*
.....redisplay motion if has any
.....Yurong 6/10/98
*/
	ncl_display_motion(-1,0,0,0,UU_TRUE,UU_FALSE,UU_NULL);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_set_screen(screen, display)
**      Set up this screen in unibase
**  PARAMETERS   
**      INPUT  :  screen	:	screen entity to set up as current in unibase
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_set_screen(screen, display)
	UV_screen	*screen;
	UU_LOGICAL	display;

	{
	UV_vport vport;
	UV_view view,view1;
	int i;

	uu_denter(UU_MTRC,(us,"uv_set_screen(%s)",screen->name));

	/* see which active screen to replace, only neccessary for multiple
		workstations.  normally screen->wstation is -1 */
	for (i = 0; i < UV_no_act_screens; i++)
		if (UV_act_screen[i].wstation == screen->wstation)
			uv_deactivsc(i);

/* TEMPORARY UNTIL XFORMS DONE RIGHT */
uu_nserv_reset(UU_XFORM_NM);
uu_nserv_resv(UU_XFORM_NM, 0);

	/* keep frequently used information in global variables */
	for (i = 0; i < screen->nvports; i++)
		{
		uv_getvpid(screen->vports[i], &vport);

		/* get transformation number */
		vport.xform = uu_nserv_req(UU_XFORM_NM);

		gsviewchar(UD_ksws, vport.xform, UG_CLIP, UG_CLIP, UG_CLIP, 1);

	uu_dprint(UU_MTRC,(us,"uv_activsc %s xform = %d",vport.name, vport.xform));

		/* save some information for fast retrieval */
		UV_act_vports[UV_no_act_screens][i].key = screen->vports[i];
		UV_act_vports[UV_no_act_screens][i].view = vport.cur_view;
		UV_act_vports[UV_no_act_screens][i].xform = vport.xform;
		UV_act_vports[UV_no_act_screens][i].disp_all = vport.disp_all;

		/* put viewport on screen */
		uv_updatevp(&vport, display);

		/* save the changes to the viewport */
		uv_putvp(&vport);
		}

	/* mark screen as active */
	screen->active = UV_ACTIVE;
	uv_putsc(screen);

	/* put screen information in global array */
	zbytecp(UV_act_screen[UV_no_act_screens], *screen);

	UV_no_act_screens++;
/*
.....Setup the NCLIPV viewport
*/
	LW_vport.xform = uu_nserv_req(UU_XFORM_NM);
	uv_putvp(&LW_vport);
	
/*
.....When we set the new screen, we need update the IPV View
.....Yurong 8/3/03
*/
	if (LW_active)
	{
		uv_getvid(LW_vport.cur_view,&view);
		uv_getvpid(LW_vport.key, &vport);
		uv_getvid(vport.cur_view, &view1);
		uv_vupdate(&view1,&view);
		uv_putv(&view);
		uv_setxform(&LW_vport);
	}
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_updatesc_display()
**			Update all of the viewport definitions (in DIGS) for the 
**			current screen.
**  PARAMETERS   
**      INPUT  : none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_updatesc_display()

	{
	UV_vport vport;
	int i;

	uu_denter(UU_MTRC,(us,"uv_updatesc_display()"));

	for (i=0; i<UV_act_screen[0].nvports; i++)
		{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_updatevp(&vport, UU_TRUE);
		}
	uu_dexit;
	}

/**************************************************************************
**  I_FUNCTION:  uv_deactivsc()
**      Bring down the old screen, return xform numbers to number server,
**		  delete border segs for viewports
**  PARAMETERS   
**      INPUT  :  screen	: index into UV_act_screens of screen 
**									  to be deactivated
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_deactivsc(screen)
	int screen;

	{
	UV_vport vport;
	int i;
	UV_screen old_screen;

	uu_denter(UU_MTRC,(us,"uv_deactivsc(%d)",screen));

	/* mark screen as being inactive */
	if (uv_getscid(UV_act_screen[screen].key, &old_screen) == UU_SUCCESS)
		{
		old_screen.active = UV_INACTIVE;
		uv_putsc(&old_screen);
		}

	for (i = 0; i < UV_act_screen[screen].nvports; i++)
		{
		uv_getvpid(UV_act_screen[screen].vports[i], &vport);

		/* remove any hidden line drawings in this viewport */
		uv_delete_hidden(&vport);

		/* set priority to 0 */
		gsviewchar(UD_ksws, vport.xform, UG_CLIP, UG_CLIP, UG_CLIP, 0);

		/* return transformation number to number server */
		uu_nserv_ret(UU_XFORM_NM, vport.xform);

		/* if there is a border up, delete the segment containing it */
		if (vport.bord_seg != -1)
			{
			gdeleteseg(vport.bord_seg); 
			vport.bord_seg = -1;

			/* save changes */
			uv_putvp(&vport);
			}

		}

	UV_no_act_screens--;

	/* make sure all active screens are in first UV_no_act_screens positions
		in global array */
	if (screen != UV_no_act_screens)
		zbytecp(UV_act_screen[screen], UV_act_screen[UV_no_act_screens]);

	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_chgsc(name)
**      Put up the named screen
**  PARAMETERS   
**      INPUT  :  name	: name of screen to be put up
**      OUTPUT :  none
**  RETURNS      :  UU_SUCCESS : if screen was found
**						  UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_chgsc(name)
	char *name;

	{
	UV_screen newsc;
	int stat;

	uu_denter(UU_MTRC,(us,"uv_chgsc(%s)",name));

	/* get the named screen */
	stat = uv_getscnm(name, &newsc);
	if (stat == UU_SUCCESS)
		uv_activsc(&newsc);

	uu_dexit;
	return(stat);
	}
/**************************************************************************
**  E_FUNCTION:  uv_getscnm(name, screen)
**      Retrieve a used screen with this name
**  PARAMETERS   
**      INPUT  :  name		:	name of screen to be found
**						screen	:	pointer to screen structure where 
**										screen is to be put
**      OUTPUT :  none
**  RETURNS      :  UU_SUCCESS : if the screen was found,
**						  UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_getscnm(name, screen)
	char *name;
	UV_screen *screen;
	{
	UU_KEY_ID key;
	int next_tupleid;
	char msg[256];

	uu_denter(UU_MTRC,(us,"uv_getscnm(%s)", name));

	/* search unibase list to the screen with this name */
	next_tupleid = 1;
	while (ur_get_next_data_key(UV_SCREEN_REL, &next_tupleid, &key) > -1)
		{
		next_tupleid++;
		
		/* retrieve screen to check */
		if (uv_getscid(key, screen) == UU_FAILURE)
			{
			sprintf (msg, "failure in uv_getscnm, %d not retrieved\n", key);
			ud_printmsg(msg);
			exit(1);
			}

/* if names are the same, we're done 
.....vp 3/4/98 make sure that string is cut off
..... at last valid character
*/
		screen->name[ul_cut_string(screen->name,strlen(name))] = '\0';
		if (strcmp(screen->name, name) == 0)
			{
			uu_dexit;
			return(UU_SUCCESS);
			}
		}

	uu_dexit;
	return(UU_FAILURE);
	}

/**************************************************************************
**  E_FUNCTION:  uv_getscid(key, screen)
**      Retrieve a screen with this key
**  PARAMETERS   
**      INPUT  :  key		:	Unibase id of screen to be found
**						screen	:	pointer to screen structure where 
**										screen is to be put
**      OUTPUT :  none
**  RETURNS      :  UU_SUCCESS : if the screen was found,
**						  UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_getscid(key, screen)
	UU_KEY_ID key;
	UV_screen *screen;
	{
	int irtn;

	uu_denter(UU_MTRC,(us,"uv_getscid(key=%x)", key));

	screen->key = key;
	
	if (ur_retrieve_data(screen, sizeof(UV_screen)) < 0)
		irtn = UU_FAILURE;
	else
		irtn = UU_SUCCESS;

	uu_dexit;
	return(irtn);
	}

/*********************************************************************
**    E_FUNCTION     : uv_swap_screen(newscreen, display_motion,
**										draw_vp_border,
**										draw_vp_axis, draw_vp_name,
**										draw_vp_aperture, view_key);
**			Remove the current screen and display the new one (NEWSCREEN).
**			The DRAW_VP_xx and VIEW_KEY are arrays which specify display
**			information about each of the viewports in the new screen.
**    PARAMETERS   
**       INPUT  : 
**				newscreen					new screen to display
**				display_motion				UU_TRUE => display motion
**				draw_vp_border				UU_TRUE => draw viewport border
**				draw_vp_axis				UU_TRUE => draw viewport axis
**				draw_vp_name				UU_TRUE => draw viewport name
**				draw_vp_aperture			UU_TRUE => draw viewport aperture
**				view_key						key of view to display in viewport
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added display_mod
.....Yurong 1/13/98
*/
uv_swap_screen(newscreen, display_motion,draw_vp_border, draw_vp_axis,
	draw_vp_name, draw_vp_aperture, display_mod, view_key)
	UV_screen *newscreen;
	UU_LOGICAL display_motion[];
	UU_LOGICAL draw_vp_border[];
	UU_LOGICAL draw_vp_axis[];
	UU_LOGICAL draw_vp_name[];
	UU_LOGICAL draw_vp_aperture[];
	int display_mod[];
	UU_KEY_ID view_key[];	
	{
	int	status;					/* status from get view (port) id */
	int	i, second;							/* loop counter */
	UU_LOGICAL new;				/* UU_TRUE iff new screen format */
	UU_LOGICAL display;			/* UU_TRUE iff vport display to be updated */
	UU_LOGICAL replace;			/* UU_TRUE iff objects in vport are to be 
											deleted and redisplayed */
	UU_KEY_ID new_view_key;		/* key of new view entity */
	UU_KEY_ID old_view_key;		/* key of old view entity */
	UV_vport	oldvport;			/* view port structure for old screen */
	UV_vport	newvport;			/* view port structure for new screen */
	UV_screen oldscreen;
	UU_LOGICAL grid_was_on;
	UU_KEY_ID grid_was_in_vp;
	UU_KEY_ID grid_was_in_view;
	UU_LOGICAL grid_on;
	UU_KEY_ID grid_in_vp;
	UU_KEY_ID grid_in_view;
	UG_segstli *segptr;
	int shade_switch;
	UV_view view;
	int redisp = 0;
	int recreat = 0;
/*
.....Added display motion flag
.....Bobby  -  11/30/92
*/

	uu_denter(UU_MTRC,(us,"uv_swap_screen(newscreen=%x)", newscreen));

	shade_switch = 0;
	grid_was_on = UM_cpln.grid.snap;
	grid_was_in_view = UM_cpln.grid.viewid;
	grid_was_in_vp = UM_cpln.grid.vpid;

	zbytecp(oldscreen, UV_act_screen[0]);
	new = (strcmp(oldscreen.name, newscreen->name) != 0);
	if (new)
		{
		/* delete all hidden lines that may be visible on the screen */
		for (i=0; i<UV_act_screen[0].nvports; i++)
			{
			uv_getvpid(UV_act_vports[0][i].key, &oldvport);
			uv_delete_hidden(&oldvport);
			}
		uv_clear();
		uv_set_screen(newscreen, UU_FALSE);
		}

	grid_on = UU_FALSE;
	for (i = 0; i < newscreen->nvports; i++)
	{
		redisp = 0;
		second = 0;
		new_view_key = view_key[i];
		status = uv_getvpid(newscreen->vports[i], &newvport);

		uv_getvid(new_view_key, &view);
		if (view.vtype==UV_SECONDARY_VIEW)
			second = 1;
		else
			second = 0;
/*
.....display_mode has 4 values, wireframe =0 , shaded =1 , hiden line = 2
.....shade only = 3, when 1 or 3, should set up for shading
.....2/2/99  Yurong
*/	
		if (newvport.disp_mode != display_mod[i]+1)
			shade_switch = 1;
		if ((newvport.disp_mode==2)&&(newvport.wireframe==0)
								&&(display_mod[i]==1))
			shade_switch = 1;

		if ((newvport.disp_mode==2)&&(display_mod[i]==3))
		{
			shade_switch = 0;
			if (newvport.wireframe!=0)
				redisp = 1;
		}

		if (new)
			{
			if (grid_was_on && !grid_on)
				{
				grid_on = grid_on || (grid_was_in_view == new_view_key);
				grid_in_vp = newvport.key;
				grid_in_view = new_view_key;
				}
			old_view_key = 0;
			}
		else
			{
			status = uv_getvpid(oldscreen.vports[i], &oldvport);
			old_view_key = oldvport.cur_view;
			}

		newvport.motion    = display_motion[i];
		newvport.bord_on   = draw_vp_border[i];
		newvport.v_axis_on = draw_vp_axis[i];
		newvport.name_on   = draw_vp_name[i];
		newvport.aperture_on  = draw_vp_aperture[i];
/*
.....added display_mod
.....wire_frame=0, shading=1, hiden line = 2, shade only = 3
.....Yurong 1/13/98
*/
		if (display_mod[i]==3)
		{
			newvport.wireframe = 0;
			newvport.disp_mode = 2;
		}
		else
		{
			newvport.wireframe = 1;
			newvport.disp_mode = display_mod[i]+1;
		}
/*
need update view key, doesn't update when display object in extern view
*/
		display =	new || shade_switch || 
						(newvport.bord_on != oldvport.bord_on) ||
						(newvport.v_axis_on != oldvport.v_axis_on) ||
						(newvport.name_on != oldvport.name_on) ||
						(newvport.aperture_on);

		replace = (!new) && (old_view_key != new_view_key);

		if ((replace)||(shade_switch)||(redisp))  uv_delvp(&oldvport);

		UV_act_vports[0][i].view = new_view_key;
		uv_vtovp(&newvport, view_key[i]);


		uv_putvp(&newvport);

		if (new || replace || shade_switch || redisp)
			uv_updatevp(&newvport, UU_TRUE);
		else if (display)
			uv_drawvp(&newvport);
		if (replace||redisp) uv_dispvp(&newvport);
		if (shade_switch==1)
			recreat = 1;		
				
	}

	if (new||recreat)
	{
		if (grid_on)
		{
			um_actgrid(grid_in_vp, grid_in_view);
		}
		uv_dispobjs();
	}
/*
.....Visible motion display
.....Bobby  -  9/1/92
*/
	segptr = ug_segac(NCL_mot_seg);
	if (segptr != NULL) gssegvis(NCL_mot_seg,UG_VISIBLE);
	uu_dexit;
	return (status);
	}

/**************************************************************************
**  E_FUNCTION:  uv_screname(screen, name)
**      Rename a screen
**  PARAMETERS   
**      INPUT  :  screen	: screen to be renamed
**						name		: new name
**      OUTPUT :  screen	: renamed screen
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_screname(screen, name)
	UV_screen	*screen;
	char			*name;
	{
	uu_denter(UU_MTRC,(us,"uv_screname(key=%x)",screen->key));
	strcpy(screen->name, name);
	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uv_putsc(screen)
**      Save this screen in Unibase
**  PARAMETERS   
**      INPUT  :  screen to be saved
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_putsc(screen)
	UV_screen *screen;
	{
	int stat, switched;
	char msg[256];

	uu_denter(UU_MTRC,(us,"uv_putsc(%s)",screen->name));

	switched = 0;
	if (UR_active==2)
/*
.....always retrieve vport info from working unibase
*/
	{
		ur_getu_work();
		switched = 1;
	}
	stat = ur_update_data(screen);
	if (stat < 0)
		{
		sprintf(msg, "uv_putsc screen %s not updated by unibase\n", screen->name);
		ud_printmsg(msg);
		exit(1);
		}
	if (switched)
		ur_getu_second();

	uu_dexit;
	}


/*********************************************************************
**    E_FUNCTION :  uv_actscreen() --activate another layout screen and
**								and update view
**    PARAMETERS   
**       INPUT  :  int screen - layout screen to activate
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_actscreen(screen)
int screen;
{
	int tmpcolor;

	uu_denter(UU_MTRC,(us,"uv_actscreen(%d)",screen));
	tmpcolor = gqsegerasecolor();
	gsegerasecolor(0);
	ud_actscrn(screen);
	uv_updatesc_display();
	gsegerasecolor(tmpcolor);
	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION     : uv_change_vport_attr(iviewkey, draw_vp_border,
**          draw_vp_axis, draw_vp_name, draw_vp_aperture, 
**				disp_mode, display_motion)
**
**			Change the parameters for the views from the iviewkey list    
**			if they are on the screen.                                  
**    PARAMETERS   
**       INPUT  : 
**				iviewkey 					the list of views to work with
**				display_motion				UU_TRUE => display motion
**				draw_vp_border				UU_TRUE => draw viewport border
**				draw_vp_axis				UU_TRUE => draw viewport axis
**				draw_vp_name				UU_TRUE => draw viewport name
**				draw_vp_aperture			UU_TRUE => draw viewport aperture
**				view_key						key of view to display in viewport
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
....added disp_mode
.....wire_frame=0, shading=1, hiden line = 2, shade only = 3
.... 4/15/99 Yurong
*/
uv_change_vport_attr(iviewkey, draw_vp_border,
            draw_vp_axis, draw_vp_name, draw_vp_aperture, 
				disp_mode, display_motion)
	UU_KEY_ID  *iviewkey;
	UU_LOGICAL *display_motion;
	UU_LOGICAL *draw_vp_border;
	UU_LOGICAL *draw_vp_axis;
	UU_LOGICAL *draw_vp_name;
	UU_LOGICAL *draw_vp_aperture;
	UU_LOGICAL *disp_mode;
	
	{
	int	status,switched;					/* status from get view (port) id */
	int	i,j,k;					/* loop counters */
	UV_vport  vport,  curvport;	/* view port structure for screen */
	UV_screen screen, curscreen;
	UU_LOGICAL shaded;
	static char *sc_name [] = {"single","vert. dual","horiz. dual",
                               "quad","five and one","six equal"};

	switched = 0;
	if (UR_active==2)
/*
.....always change vport info from working unibase
*/
	{
		ur_getu_work();
		switched = 1;
	}
    for(k=0; k < 6; k++)
    {
        status = uv_getscnm(sc_name[k], &screen);
        if (status != UU_SUCCESS)
            uu_uerror1(/* illegal screen name */ UM_MODEL, 228, sc_name[k]);
        for (i = 0; i < screen.nvports; i++)
        {
	       status = uv_getvpid(screen.vports[i], &vport);
           for(j=0; j < 36 && *(iviewkey + j) > 0; j++)
           {
              if(*(iviewkey + j) == vport.cur_view) goto cont;
           }
           continue;
cont:; 

		   if(*display_motion >=0)  vport.motion      = *display_motion;
		   if(*draw_vp_border >=0)  vport.bord_on     = *draw_vp_border;
		   if(*draw_vp_axis   >=0)  vport.v_axis_on   = *draw_vp_axis;
		   if(*draw_vp_name   >=0)  vport.name_on     = *draw_vp_name;
		   if(*draw_vp_aperture>=0) vport.aperture_on = *draw_vp_aperture;
			shaded = UU_FALSE;	
		   if (*disp_mode == 3)
			{
				if (vport.wireframe != 0 || vport.disp_mode != 2) shaded = UU_TRUE;
				vport.wireframe = 0;
				vport.disp_mode = 2;
			}
			else if (*disp_mode >= 0)
			{
				if (*disp_mode != vport.disp_mode-1) shaded = UU_TRUE;
				vport.wireframe = 1;
				vport.disp_mode = *disp_mode + 1;
			}
			if (shaded) uv_delvp(&vport);
			ur_update_data(&vport);
			if (shaded) uv_dispobjs();
       }
    }
    zbytecp(curscreen, UV_act_screen[0]);
    for (i = 0; i < curscreen.nvports; i++)
    {
        status = uv_getvpid(curscreen.vports[i], &curvport);
        for(j=0; j < 36 && *(iviewkey + j) > 0; j++)
        {
           if(*(iviewkey + j) == curvport.cur_view) goto cont1;
        }
        continue;
cont1:;
        uv_drawvp(&curvport);
    }
	if (switched)
		ur_getu_second();
	uu_dexit;
	return (status);
    }
/**************************************************************************
**  E_FUNCTION:  uv_getscnm1(name, screen)
**      Does exactly the same as uv_getscnm (described above) but converts
**      the names to the UPPER case. We need it for the DRAFT/FORMAT statement.
**  PARAMETERS   
**      INPUT  :  name		:	name of screen to be found
**				screen	:	pointer to screen structure where 
**										screen is to be put
**      OUTPUT :  none
**  RETURNS      :  UU_SUCCESS : if the screen was found,
**						  UU_FAILURE : otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uv_getscnm1(name, screen)
	char *name;
	UV_screen *screen;
	{
	UU_KEY_ID key;
	int i, next_tupleid;
    char source [21], target[21], msg[256];

	uu_denter(UU_MTRC,(us,"uv_getscnm(%s)", name));
/*       
..... 
.....Convert the examined  name to the upper case. 
..... 
*/ 
	for (i=0; i<strlen(name) && i < 20; i++) 
	{
		target[i] = 0;
		target[i] = islower(name[i]) ? toupper(name[i]) : name[i];
	}
	target[i] = 0;

	/* search unibase list to the screen with this name */
	next_tupleid = 1;
	while (ur_get_next_data_key(UV_SCREEN_REL, &next_tupleid, &key) > -1)
	{
		next_tupleid++;
		
		/* retrieve screen to check */
		if (uv_getscid(key, screen) == UU_FAILURE)
		{
			sprintf (msg, "failure in uv_getscnm, %d not retrieved\n", key);
			ud_printmsg(msg);
			return -1;
		}
/*
.....Convert the unibase screen name to the upper case.
*/
		screen->name[ul_cut_string(screen->name,strlen(name))] = '\0';
		for (i=0; i<strlen(screen->name); i++)
			source[i] = islower(screen->name[i]) ? toupper(screen->name[i]) : screen->name[i];
		source[i] = 0;

/* if names are the same, we're done 
.....vp 3/4/98 make sure that string is cut off
..... at last valid character
*/
		if (strcmp(source, target) == 0)
		{
			uu_dexit;
			return(UU_SUCCESS);
		}
	}

	uu_dexit;
	return(UU_FAILURE);
}
