/*********************************************************************
**    NAME         :  m8irsol6.c
**       CONTAINS:
**			um_view_mat(xform, matrix)
**   		um_build_viewmat(view_refpt, view_norm, view_up, matrix)
**			um_hidden_normtran(vport)
**			um_unblanked_by_namelist(namelist, numofby)
**    	um_setup_rom_viewmat(cur_view_key)
**			um_add_bodies_to_romdraw(numofby)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m8irsol6.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:11
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "usysdef.h"
#include "uims.h"
#include "mdcoord.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mromcom.h"
#include "modef.h"
#include "mdebug.h"
#include "mromcom.h"
#include "mdrel.h"
#include "msol.h"
#include "mfcifdef.h"
#include "view.h"

/*********************************************************************
**    E_FUNCTION     : um_view_mat(xform, matrix)
**			Calculate the current ROMULUS view matrix for the specified
**			DIGS normtran (XFORM). This does not include perspective.
**    PARAMETERS   
**       INPUT  : 
**          xform						DIGS normtran for the view
**       OUTPUT :  
**				matrix					current view matrix for the view
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_view_mat(key, matrix)
	int key;
	UU_REAL  matrix[4][4];

	{
	UV_view	view;
	UM_coord vrefpt;
	UM_vector vpnorm;
 	UM_vector vup;

	view.key = key;
	ur_retrieve_data(&view, sizeof(UV_view));

	vpnorm[0] = view.cur_pln_norm[0];
	vpnorm[1] = view.cur_pln_norm[1];
	vpnorm[2] = view.cur_pln_norm[2];

	vrefpt[0] = view.cur_ref_pt[0];
	vrefpt[1] = view.cur_ref_pt[1];
	vrefpt[2] = view.cur_ref_pt[2];

 	vup[0] = view.cur_up_vect[0];
 	vup[1] = view.cur_up_vect[1];
 	vup[2] = view.cur_up_vect[2];

 	um_build_viewmat(vrefpt, vpnorm, vup, matrix);
	}

/*********************************************************************
**    E_FUNCTION     : um_build_viewmat(view_refpt, view_norm, view_up, matrix)
**			Calculate the current ROMULUS view matrix form the viewing
**			parameters view_refpt, view_norm, view_up.
**    PARAMETERS   
**       INPUT  : 
**          view_refpt						view reference point
**				view_norm						view normal
**				view_up							view up vector
**       OUTPUT :  
**				matrix					current view matrix for the view
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_build_viewmat(view_refpt, view_norm, view_up, matrix)
	UM_coord		view_refpt;					/* view reference point	*/
	UM_vector	view_norm;					/* view normal	*/
 	UM_vector	view_up;						/* view up vector */
	UU_REAL  	matrix[4][4];

	{
 	UM_vector	view_xaxis;
 	UM_transf tftemp;
 	int i,j;
/* UU_REAL cosaz, sinaz;					sine and cosine of azimuthal angle
	UU_REAL cosel, sinel;					sine and cosine of elevation angle 
	UU_REAL tempx, tempy, tempz;			
*/

 /* calculate the transformation which will transform the geometry */
 	um_cross(view_up, view_norm, view_xaxis);
 	um_unitvc(view_xaxis, view_xaxis);
 	um_chgcstf(view_refpt, view_xaxis, view_up, view_norm,
 		UM_zerovec, UM_xaxis, UM_yaxis, UM_zaxis, tftemp);

 	for (i=0;i<4;i++)
 	{
 		for (j=0;j<3;j++)
 			matrix[i][j] = tftemp[i][j];
 	}
 
 	matrix[0][3] = 0.0;
 	matrix[1][3] = 0.0;
 	matrix[2][3] = 0.0;
 	matrix[3][3] = 1.0;
 
/*	sinel = view_norm[1];
	cosel = sqrt(1.0 -  view_norm[1]*view_norm[1]);
	if (fabs(cosel) < UM_FUZZ)
		{
		cosaz = 1.0;
		sinaz = 0.0;
		}
	else
		{
		sinaz =  view_norm[0]/cosel;
		cosaz =  view_norm[2]/cosel;
		}

	matrix[0][0] = cosaz;
	matrix[0][1] = -sinaz * sinel;
	matrix[0][2] = sinaz * cosel;
	matrix[0][3] = 0.0;

	matrix[1][0] = 0.0;
	matrix[1][1] = cosel;
	matrix[1][2] = sinel;
	matrix[1][3] = 0.0;

	matrix[2][0] = -sinaz;
	matrix[2][1] = -cosaz * sinel;
	matrix[2][2] = cosaz * cosel;
	matrix[2][3] = 0.0;

	tempx = -view_refpt[0];
	tempy = -view_refpt[1];
	tempz = -view_refpt[2];


	matrix[3][0] = (tempx*cosaz) - (tempz*sinaz);
	tempz = (tempx*sinaz) + (tempz*cosaz);
	matrix[3][1] = (tempy*cosel) - (tempz*sinel);
	matrix[3][2] = (tempy*sinel) + (tempz*cosel);
	matrix[3][3] = 1.0;
*/
	}

/**************************************************************************
**  I_FUNCTION:  um_hidden_normtran(vport)
**      Set up the normtran for the specified viewport (VPORT).
**  PARAMETERS   
**      INPUT  :  vport				view port record corresponding to the view
**											the hidden lines are calculated for
**						hidden_xform	transformation number for the hidden lines
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
um_hidden_normtran(vport, hidden_xform)
	UV_vport	*vport;
	int hidden_xform;

	{
	UM_coord	llf;							/* lower left front window bounds
														( model coord.) */
	UM_vector urb;							/* upper right back window bounds
														( model coord. */
	UM_ndc	lleft;
	UM_ndc	uright;
	Gwrect3	window;
	Gnrect3	viewport;
	UV_view	view;
	int 		stat;
	Gwpoint3	pt;
	int curr_scrn;
	Gnrect *gr_area;
	UU_REAL  zwidth;
	char msg[256];

	uu_denter(UU_MTRC,(us,"um_hidden_normtran(key=%x, hidden_xform = %d)",
				vport->key, hidden_xform));
	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);

	stat = uv_getvid(vport->cur_view, &view);
	if (stat == UU_FAILURE)
		{
		sprintf(msg, "Viewport %s does not assoc view or it can not be found\n",
				vport->name);
		ud_printmsg(msg);
		exit(1);
		}

	/* set the reference point */
	pt.x = 0.0;
	pt.y = 0.0;
	pt.z = 0.0;
	gsvref3(hidden_xform, &pt);

	/* set the view plane normal */
	pt.x = 0.0;
	pt.y = 0.0;
	pt.z = 1.0;
	gsvpn3(hidden_xform, &pt);

	/* set the view up vector */
	pt.x = 0.0;
	pt.y = 1.0;
	pt.z = 0.0;
	gsvup3(hidden_xform, &pt);

	/* set the window */
	uv_get_window_bounds(vport, llf, urb);
/*
	window.llf.x = llf[0] - view.cur_ref_pt[0];
	window.llf.y = llf[1] - view.cur_ref_pt[1];
	window.llf.z = llf[2] - view.cur_ref_pt[2];

	window.urb.x = urb[0] - view.cur_ref_pt[0];
	window.urb.y = urb[1] - view.cur_ref_pt[1];
	window.urb.z = urb[2] - view.cur_ref_pt[2];
*/

	window.llf.x = llf[0];
	window.llf.y = llf[1];
	window.llf.z = llf[2];

	window.urb.x = urb[0];
	window.urb.y = urb[1];
	window.urb.z = urb[2];

	gswindow3(hidden_xform, &window);

	/* set the viewport box */
	lleft[0] = vport->llf[0] * (gr_area->ur.x - gr_area->ll.x) + gr_area->ll.x;
	lleft[1] = vport->llf[1] * (gr_area->ur.y - gr_area->ll.y) + gr_area->ll.y;
	uright[0] = vport->urb[0] * (gr_area->ur.x - gr_area->ll.x) + gr_area->ll.x;
	uright[1] = vport->urb[1] * (gr_area->ur.y - gr_area->ll.y) + gr_area->ll.y;

	zwidth = (llf[2] - urb[2]) * (uright[0]-lleft[0]) / (urb[0] - llf[0]);
	lleft[2]  =  zwidth / 2.0;
	uright[2] = -zwidth / 2.0;

	viewport.llf.x = lleft[0];
	viewport.llf.y = lleft[1];
	viewport.llf.z = 1.0;

	viewport.urb.x = uright[0];
	viewport.urb.y = uright[1];
	viewport.urb.z = 0.0;
	gsview3(hidden_xform, &viewport);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_unblanked_by_namelisty(namelist, numofby)
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**				namelist					list of unblanked solids names
**				numofby					number of unblanked solids
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_unblanked_by_namelist(namelist, numofby)
char *namelist;
int *numofby;

	{
	int status, entnum;
	struct UM_body_rec b;
	UU_LOGICAL blanked;

	uu_denter( UU_MTRC,(us,"um_unblanked_by_namelist()"));

	/* build the namelist of the nonblanked Romulus bodies */
	strcpy(namelist,"");
	b.rel_num = UM_BODY_REL;
	status = 1;
	entnum = 0;
	*numofby = 0;
	while (status >= 0)
		{
		entnum++;
		/* get all the nonblanked bodies */
		status = ur_get_next_data_key(b.rel_num, &entnum, &b.key);
		if (status >= 0)
			{
			um_get_all_geom(&b, sizeof(struct UM_body_rec));
			/* find out if this body is blanked or not */
			ur_retrieve_blanked(b.key, &blanked);
			if (!blanked)
				{
				/* form a list of their names */
				strcat(namelist,",");
				strcat(namelist,b.name);
				(*numofby)++;
				}
			}
		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_setup_rom_viewmat(cur_view_key)
**			Set the viewing commands for ROMULUS using the current
**			view key.
**    PARAMETERS   
**       INPUT  : 
**          cur_view_key
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_setup_rom_viewmat(cur_view_key)
UU_KEY_ID cur_view_key;
{

	UU_REAL mat[4][4];						/* view transformation matrix */
	char cmd[120];								/* ROMULUS command */

/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
**  block comment
*/
	uu_denter( UU_MTRC,(us,"um_setup_rom_viewmat()"));

		um_view_mat(cur_view_key, mat);
		um_init_rombuf();
		um_add_rombuf("OPT TERM 4014");
		sprintf(cmd,"VIEW MATRIX %f %f %f %f@",mat[0][0],mat[0][1],mat[0][2],mat[0][3]);
		um_add_rombuf(cmd);
		sprintf(cmd,"%f %f %f %f@",mat[1][0],mat[1][1],mat[1][2],mat[1][3]);
		um_add_rombuf(cmd);
		sprintf(cmd,"%f %f %f %f@",mat[2][0],mat[2][1],mat[2][2],mat[2][3]);
		um_add_rombuf(cmd);
		sprintf(cmd,"%f %f %f %f",mat[3][0],mat[3][1],mat[3][2],mat[3][3]);
		um_add_rombuf(cmd);
		sprintf(cmd,"VIEW INFINITY");
		um_add_rombuf("VIEW SCALE MANUAL 1.0");
		um_add_rombuf(cmd);
		um_callromulus();

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : um_add_bodies_to_romdraw(numofby)
**			Fill the ROMULUS drawing list with all the unblanked solids
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          numofby			number of bodies in drawing list
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_add_bodies_to_romdraw(numofby)
int *numofby;

	{
	int status, entnum;
	struct UM_body_rec b;
	UU_LOGICAL first, blanked;

	uu_denter( UU_MTRC,(us,"um_add_bodies_to_romdraw()"));

	/* build the namelist of the nonblanked Romulus bodies */
	b.rel_num = UM_BODY_REL;

	status = 1;			/* get_next_data_key status */
	entnum = 0;			/* entity number */
	*numofby = 0;		/* body (solid) number */
	first = UU_TRUE;	/* first time call to ufdrwlst */

	while (status >= 0)
		{
		entnum++;
		/* get all the nonblanked bodies */
		status = ur_get_next_data_key(b.rel_num, &entnum, &b.key);
		if (status >= 0)
			{
			um_get_all_geom(&b, sizeof(struct UM_body_rec));
			/* find out if this body is blanked or not */
			ur_retrieve_blanked(b.key, &blanked);
			if (!blanked)
				{
				(*numofby)++;
				ufdlst(&b.id, &first);
				first = UU_FALSE;
				}
			}
		}

	uu_dexit;
	}
