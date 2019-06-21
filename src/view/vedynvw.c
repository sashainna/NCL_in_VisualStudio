/*********************************************************************
**    NAME         :  vedynvw.c
**       CONTAINS: dynamic viewing routines
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**			uv_dynpan(vport)
**			uv_dynzoom(vport)
**			uv_dynzrot(vport)
**			uv_dynxyrot(vport)
**			uv_dynvecrot(vport)
**			uv_dynmouse(vport)
**			uv_locwin(rect)
**			uv_get_box_midz
**     MODULE NAME AND RELEASE LEVEL 
**       vedynvw.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       02/08/16 , 09:18:26
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "usysg.h"
#include "udebug.h"
#include "ginq.h"
#include "mdcoord.h"
#include "uims.h"
#include "g.h"
#include "gsegac.h"
#include "view.h"
#include "dasg.h"
#include "dinput.h"
#include "dasnog.h"
#include "xenv1.h"
#include "modef.h"
#include "mpocket.h"
#include  "lipv.h"

extern UU_REAL UV_dyncp[3];
extern UU_REAL LW_dyncp[3];
extern int UZ_nclipv_view;
extern int LW_nclipv;

Gerror ginitloc();

void uv_locwin();
extern int UD_picking_active;
int NCL_dyn_ldr =0;

/********************************************************************* 
**  E_FUNCTION:        uv_dynpan(vport)
**      Invoke dynamic pan on the terminal
**  PARAMETERS   
**      INPUT: 
**				vport							viewport entity
**      OUTPUT: 
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uv_dynpan(vport)
	UV_vport	*vport;

	{
	Glocrec locrec ;
	Gqloc *greqloc();
	Gwrect3 *window;
	Gdrect locwin;
	Gerror rtn;
	UV_view	view;						/* view associated with the picked view port */
	UM_vector delta;
	UM_coord xaxis;
	UM_coord yaxis;
	UM_coord zaxis;
	UM_coord csav;
	FILE *fd;
	int status,nc, pick_save;
	char inbuffer[500], tmpstr[200], *tok;
	float t1, t2, t3, t4, t5, t6, t7, t8, t9;
	static char panprompt[] = "Dynamic pan in effect";

	uu_denter(UU_MTRC,(us,"uv_dynpan()"));
	pick_save = 0;
	if (UD_picking_active)
	{
/*		ud_unhilight_pickseg();
		pick_save = 1;*/
	}
	gsnormtran(vport->xform);

	locrec.prompt = panprompt;				/* locator data record */
	if (UZ_nclipv_view)
	{
		csav[0] = LW_dyncp[0];
		csav[1] = LW_dyncp[1];
		csav[2] = LW_dyncp[2];
	}
	else
	{
		csav[0] = UV_dyncp[0];
		csav[1] = UV_dyncp[1];
		csav[2] = UV_dyncp[2];
	}
	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
		status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status != UU_SUCCESS)
			goto done;
		tok = (char*)strtok(inbuffer, " \n");
		if (tok==NULL)	goto done;
		tok = (char*)strtok(NULL, " \n");
		if (tok==NULL)			
			goto done;
		strcpy(view.name, tok);	
		nc = strlen(tok);
		strcpy(tmpstr, tok+nc+1);
		sscanf(tmpstr, "%f %f %f %f %f %f %f %f %f\n",
				&t1, &t2, &t3,
				&t4, &t5, &t6,
				&t7, &t8, &t9);			
		view.cur_ref_pt[0] = t1;
		view.cur_ref_pt[1] = t2;
		view.cur_ref_pt[2] = t3;

		view.cur_pln_norm[0] = t4;
		view.cur_pln_norm[1] = t5;
		view.cur_pln_norm[2] = t6;

		view.cur_up_vect[0] = t7;
		view.cur_up_vect[1] = t8;
		view.cur_up_vect[2] = t9;

		gsvref3(vport->xform, &(view.cur_ref_pt));
		gsvpn3(vport->xform, &(view.cur_pln_norm[0]));
		gsvup3(vport->xform, &(view.cur_up_vect));				
		
		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);

		if (UZ_nclipv_view)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
		goto done;
	}
	gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO) ;
	uv_locwin(&locwin);
	rtn=ginitloc(UD_ksws, 1, &UD_nxhair, 40, &locwin, &locrec);
	if (rtn==NCL_NO_ERROR)
		{
		greqloc(UD_ksws, 1);

		/* set the reference point */
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		window = gqwindow3(vport->xform);
		delta[0] = (window->urb.x + window->llf.x) / 2.0;
		delta[1] = (window->urb.y + window->llf.y) / 2.0;
		delta[2] = (window->urb.z + window->llf.z) / 2.0;
		um_cross(view.cur_up_vect, view.cur_pln_norm, xaxis);
		um_vctmsc(xaxis, delta[0], xaxis);
		um_vctmsc(view.cur_up_vect, delta[1], yaxis);
		um_vctmsc(view.cur_pln_norm, delta[2], zaxis);
		um_vcplvc(view.cur_ref_pt, xaxis, view.cur_ref_pt);
		um_vcplvc(view.cur_ref_pt, yaxis, view.cur_ref_pt);
		um_vcplvc(view.cur_ref_pt, zaxis, view.cur_ref_pt);
		if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		{
			sprintf(inbuffer, "DYNAMIC %s %f %f %f %f %f %f %f %f %f\n",
				view.name, view.cur_ref_pt[0], view.cur_ref_pt[1], view.cur_ref_pt[2],
				view.cur_pln_norm[0], view.cur_pln_norm[1], view.cur_pln_norm[2],
				view.cur_up_vect[0], view.cur_up_vect[1], view.cur_up_vect[2]);			
			ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
			ux_fputs0(inbuffer, fd);
		}
		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);

		gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO) ;
		if (UZ_nclipv_view)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
	}
done:;
	if (pick_save)
	{
		ud_restore_hilgt_seg();
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:        uv_dynzoom(vport)
**      Let the user dynamically change the magnification of the display.
**  PARAMETERS   
**      INPUT: 
**				vport							viewport entity
**      OUTPUT: 
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uv_dynzoom(vport)
	UV_vport	*vport;

	{
	Gdrect locwin;
	Glocrec locrec ;
	Gqloc *greqloc();
	Gwrect3 *window;
	static char panprompt[] = "Dynamic zoom in effect";
	Gerror rtn;
	UV_view	view;						/* view associated with the picked view port */
	UM_vector delta;
	UM_coord xaxis;
	UM_coord yaxis;
	UM_coord zaxis;
	UM_coord csav;
	FILE *fd;
	int status,nc, pick_save;
	char inbuffer[500], tmpstr[200], *tok;
	float t1, t2, t3, t4, t5, t6, t7, t8, t9;
	Gwrect3 wswin;

	uu_denter( UU_MTRC,(us,"uv_dynzoom()"));

/*
.....during dynamic rotaion attach the leader line to the left corner of the
.....label box so that the leader lines always appear attached to the labels
*/
	NCL_dyn_ldr =1;
	ncl_display_ldr(vport);

	pick_save = 0;
	if (UD_picking_active)
	{
/*		ud_unhilight_pickseg();
		pick_save = 1;*/
	}
	gsnormtran(vport->xform);

	locrec.prompt = panprompt;				/* locator data record */
	if (UZ_nclipv_view)
	{
		csav[0] = LW_dyncp[0];
		csav[1] = LW_dyncp[1];
		csav[2] = LW_dyncp[2];
	}
	else
	{
		csav[0] = UV_dyncp[0];
		csav[1] = UV_dyncp[1];
		csav[2] = UV_dyncp[2];
	}
	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
		status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status != UU_SUCCESS)
			goto done;
		tok = (char*)strtok(inbuffer, " \n");
		if (tok==NULL) goto done;
		tok = (char*)strtok(NULL, " \n");
		if (tok==NULL) goto done;
		strcpy(view.name, tok);	
		nc = strlen(tok);
		strcpy(tmpstr, tok+nc+1);
		sscanf(tmpstr, "%f %f %f %f %f %f %f %f %f\n",
				&t1, &t2, &t3,
				&t4, &t5, &t6,
				&t7, &t8, &t9);			

		view.cur_ref_pt[0] = t1;
		view.cur_ref_pt[1] = t2;
		view.cur_ref_pt[2] = t3;

		view.cur_pln_norm[0] = t4;
		view.cur_pln_norm[1] = t5;
		view.cur_pln_norm[2] = t6;

		view.cur_up_vect[0] = t7;
		view.cur_up_vect[1] = t8;
		view.cur_up_vect[2] = t9;

		status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status != UU_SUCCESS)
			goto done;
		sscanf(inbuffer, "DYNAMIC %f %f %f %f %f %f\n",
				&t1, &t2, &t3,
				&t4, &t5, &t6);			
		wswin.llf.x = t1;
		wswin.llf.y = t2;
		wswin.llf.z = t3;
		wswin.urb.x = t4;
		wswin.urb.y = t5;
		wswin.urb.z = t6;

		gswindow3(vport->xform,&wswin);		
		window = gqwindow3(vport->xform);
		view.cur_aperture = (window->urb.x - window->llf.x);
/*
.....Set the reference point
*/
		delta[0] = (window->urb.x + window->llf.x) / 2.0;
		delta[1] = (window->urb.y + window->llf.y) / 2.0;
		delta[2] = (window->urb.z + window->llf.z) / 2.0;
		um_cross(view.cur_up_vect, view.cur_pln_norm, xaxis);
		um_vctmsc(xaxis, delta[0], xaxis);
		um_vctmsc(view.cur_up_vect, delta[1], yaxis);
		um_vctmsc(view.cur_pln_norm, delta[2], zaxis);
		um_vcplvc(view.cur_ref_pt, xaxis, view.cur_ref_pt);
		um_vcplvc(view.cur_ref_pt, yaxis, view.cur_ref_pt);
		um_vcplvc(view.cur_ref_pt, zaxis, view.cur_ref_pt);

		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);

		if (UZ_nclipv_view)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
		goto done;
	}
	gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO) ;
	uv_locwin(&locwin);
	rtn=ginitloc(UD_ksws, 1, &UD_nxhair, 43, &locwin, &locrec);
	if (rtn==NCL_NO_ERROR)
		{
		greqloc(UD_ksws, 1);
		NCL_dyn_ldr =0;

/*
.....Set the aperture
*/
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		window = gqwindow3(vport->xform);
		view.cur_aperture = (window->urb.x - window->llf.x);
/*
.....Set the reference point
*/
		delta[0] = (window->urb.x + window->llf.x) / 2.0;
		delta[1] = (window->urb.y + window->llf.y) / 2.0;
		delta[2] = (window->urb.z + window->llf.z) / 2.0;
		um_cross(view.cur_up_vect, view.cur_pln_norm, xaxis);
		um_vctmsc(xaxis, delta[0], xaxis);
		um_vctmsc(view.cur_up_vect, delta[1], yaxis);
		um_vctmsc(view.cur_pln_norm, delta[2], zaxis);
		um_vcplvc(view.cur_ref_pt, xaxis, view.cur_ref_pt);
		um_vcplvc(view.cur_ref_pt, yaxis, view.cur_ref_pt);
		um_vcplvc(view.cur_ref_pt, zaxis, view.cur_ref_pt);
		if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		{
			sprintf(inbuffer, "DYNAMIC %s %f %f %f %f %f %f %f %f %f\n",
				view.name, view.cur_ref_pt[0], view.cur_ref_pt[1], view.cur_ref_pt[2],
				view.cur_pln_norm[0], view.cur_pln_norm[1], view.cur_pln_norm[2],
				view.cur_up_vect[0], view.cur_up_vect[1], view.cur_up_vect[2]);						
			ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
			ux_fputs0(inbuffer, fd);
			sprintf(inbuffer, "DYNAMIC %f %f %f %f %f %f\n",
				window->llf.x, window->llf.y, window->llf.z,
				window->urb.x, window->urb.y, window->urb.z);	
			ux_fputs0(inbuffer, fd);
		}
		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);

		gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO) ;
		if (UZ_nclipv_view)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
	}
done:;
	if (pick_save)
	{
		ud_restore_hilgt_seg();
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:        uv_dynzrot(vport)
**      Dynamically change the z axis rotation.
**  PARAMETERS   
**      INPUT:
**				vport						viewport entity
**      OUTPUT: 
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uv_dynzrot(vport)
	UV_vport	*vport;

	{
	Glocrec locrec ;
	Gdrect locwin;
	Gqloc *greqloc();
	static char zrotprompt[] = "Dynamic Z axis rotation in effect";
	Gwpoint3 *vup;
	Gerror rtn;
	UV_view	view;
	Gwpoint3 pt;
	UM_coord csav;
	FILE *fd;
	int status,nc, pick_save;
	char inbuffer[500], tmpstr[200], *tok;
	float t1, t2, t3, t4, t5, t6, t7, t8, t9;

	uu_denter( UU_MTRC,(us,"uv_dynzrot()"));

/*
.....during dynamic rotaion attach the leader line to the left corner of the
.....label box so that the leader lines always appear attached to the labels
*/
	NCL_dyn_ldr =1;
	ncl_display_ldr(vport);

	pick_save = 0;
	if (UD_picking_active)
	{
/*		ud_unhilight_pickseg();
		pick_save = 1;*/
	}
	gsnormtran(vport->xform);

	locrec.prompt = zrotprompt;				/* locator data record */
	if (UZ_nclipv_view)
	{
		csav[0] = LW_dyncp[0];
		csav[1] = LW_dyncp[1];
		csav[2] = LW_dyncp[2];
	}
	else
	{
		csav[0] = UV_dyncp[0];
		csav[1] = UV_dyncp[1];
		csav[2] = UV_dyncp[2];
	}

	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
		status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status != UU_SUCCESS)
			goto done;
		tok = (char*)strtok(inbuffer, " \n");
		if (tok==NULL) goto done;
		tok = (char*)strtok(NULL, " \n");
		if (tok==NULL) goto done;
		strcpy(view.name, tok);	
		nc = strlen(tok);
		strcpy(tmpstr, tok+nc+1);
		sscanf(tmpstr, "%f %f %f %f %f %f %f %f %f\n",
				&t1, &t2, &t3,
				&t4, &t5, &t6,
				&t7, &t8, &t9);			
		view.cur_ref_pt[0] = t1;
		view.cur_ref_pt[1] = t2;
		view.cur_ref_pt[2] = t3;

		view.cur_pln_norm[0] = t4;
		view.cur_pln_norm[1] = t5;
		view.cur_pln_norm[2] = t6;

		view.cur_up_vect[0] = t7;
		view.cur_up_vect[1] = t8;
		view.cur_up_vect[2] = t9;

		gsvref3(vport->xform, &(view.cur_ref_pt));
		gsvpn3(vport->xform, &(view.cur_pln_norm[0]));
		gsvup3(vport->xform, &(view.cur_up_vect));
		
		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);

		if (UZ_nclipv_view)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
		goto done;
	}
	gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO) ;
	uv_locwin(&locwin);
	rtn=ginitloc(UD_ksws, 1, &UD_nxhair, 42, &locwin, &locrec);
	if (rtn==NCL_NO_ERROR)
		{
		greqloc(UD_ksws, 1);
		NCL_dyn_ldr =0;

		/* set the view plane normal */
		givpn3(vport->xform, &pt);
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		view.cur_pln_norm[0] = pt.x;
		view.cur_pln_norm[1] = pt.y;
		view.cur_pln_norm[2] = pt.z;
	
		/* set the view up vector */
		vup = gqvup3(vport->xform);
		view.cur_up_vect[0] = (*vup).x;
		view.cur_up_vect[1] = (*vup).y;
		view.cur_up_vect[2] = (*vup).z;
/*
.....Update the view reference point
*/
		if (UZ_nclipv_view)
		{
			view.cur_ref_pt[0] = LW_dyncp[0];
			view.cur_ref_pt[1] = LW_dyncp[1];
			view.cur_ref_pt[2] = LW_dyncp[2];
		}
		else
		{
			view.cur_ref_pt[0] = UV_dyncp[0];
			view.cur_ref_pt[1] = UV_dyncp[1];
			view.cur_ref_pt[2] = UV_dyncp[2];
		}
		if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		{
			sprintf(inbuffer, "DYNAMIC %s %f %f %f %f %f %f %f %f %f\n",
				view.name, view.cur_ref_pt[0], view.cur_ref_pt[1], view.cur_ref_pt[2],
				view.cur_pln_norm[0], view.cur_pln_norm[1], view.cur_pln_norm[2],
				view.cur_up_vect[0], view.cur_up_vect[1], view.cur_up_vect[2]);			
			ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
			ux_fputs0(inbuffer, fd);
		}
		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);

		gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO) ;
		if (UZ_nclipv_view)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
	}
done:;
	if (pick_save)
	{
		ud_restore_hilgt_seg();
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:        uv_dynxyrot(vport)
**      Change the view plane normal.
**  PARAMETERS   
**      INPUT:    
**				vport						viewport entity
**      OUTPUT:  
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uv_dynxyrot(vport)
	UV_vport	*vport;

	{
	Glocrec locrec ;
	Gdrect locwin;
	Gqloc *greqloc();
	static char panprompt[] = "Dynamic xy rotate in effect";
	Gerror rtn;
	UV_view	view;
	Gwpoint3 pt;
	Gwpoint3 *vup;
	UM_coord csav;
	FILE *fd;
	int status,nc, pick_save;
	char inbuffer[500], tmpstr[200], *tok;
	float t1, t2, t3, t4, t5, t6, t7, t8, t9;

	uu_denter( UU_MTRC,(us,"uv_dynxyrot()"));

/*
.....during dynamic rotaion attach the leader line to the left corner of the
.....label box so that the leader lines always appear attached to the labels
*/
	NCL_dyn_ldr =1;
	ncl_display_ldr(vport);

	pick_save = 0;
	if (UD_picking_active)
	{
/*		ud_unhilight_pickseg();
		pick_save = 1;*/
	}
	gsnormtran(vport->xform);

	locrec.prompt = panprompt;				/* locator data record */
	if ((UZ_nclipv_view == 1)|| (LW_nclipv==LW_STANDALONE))
	{
		csav[0] = LW_dyncp[0];
		csav[1] = LW_dyncp[1];
		csav[2] = LW_dyncp[2];
	}
	else
	{
		csav[0] = UV_dyncp[0];
		csav[1] = UV_dyncp[1];
		csav[2] = UV_dyncp[2];
	}
	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
		status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status != UU_SUCCESS)
			goto done;
		tok = (char*)strtok(inbuffer, " \n");
		if (tok==NULL) goto done;
		tok = (char*)strtok(NULL, " \n");
		if (tok==NULL) goto done;
		strcpy(view.name, tok);	
		nc = strlen(tok);
		strcpy(tmpstr, tok+nc+1);
/* 
.....following statement will get wrong value
*/
/*		sscanf(tmpstr, "%f %f %f %f %f %f %f %f %f\n",
				&(view.cur_ref_pt[0]), &(view.cur_ref_pt[1]), &(view.cur_ref_pt[2]),
				&(view.cur_pln_norm[0]), &(view.cur_pln_norm[1]), &(view.cur_pln_norm[2]),
				&(view.cur_up_vect[0]), &(view.cur_up_vect[1]), &(view.cur_up_vect[2]));			
*/		
		sscanf(tmpstr, "%f %f %f %f %f %f %f %f %f\n",
				&t1, &t2, &t3,
				&t4, &t5, &t6,
				&t7, &t8, &t9);			
		view.cur_ref_pt[0] = t1;
		view.cur_ref_pt[1] = t2;
		view.cur_ref_pt[2] = t3;

		view.cur_pln_norm[0] = t4;
		view.cur_pln_norm[1] = t5;
		view.cur_pln_norm[2] = t6;

		view.cur_up_vect[0] = t7;
		view.cur_up_vect[1] = t8;
		view.cur_up_vect[2] = t9;

		gsvref3(vport->xform, &(view.cur_ref_pt));
		gsvpn3(vport->xform, &(view.cur_pln_norm));
		gsvup3(vport->xform, &(view.cur_up_vect));
		
		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);
		if ((UZ_nclipv_view == 1)|| (LW_nclipv==LW_STANDALONE))
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
		goto done;
	}
	gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO) ;
	uv_locwin(&locwin);
	rtn=ginitloc(UD_ksws, 1, &UD_nxhair, 41, &locwin, &locrec);
	if (rtn==NCL_NO_ERROR)
		{
		greqloc(UD_ksws, 1);
		NCL_dyn_ldr =0;

		/* set the view plane normal */
		givpn3(vport->xform, &pt);
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		view.cur_pln_norm[0] = pt.x;
		view.cur_pln_norm[1] = pt.y;
		view.cur_pln_norm[2] = pt.z;
	
		/* set the view up vector */
		vup = gqvup3(vport->xform);
		view.cur_up_vect[0] = (*vup).x;
		view.cur_up_vect[1] = (*vup).y;
		view.cur_up_vect[2] = (*vup).z;
/*
.....Update the view reference point
*/
		if ((UZ_nclipv_view == 1)|| (LW_nclipv==LW_STANDALONE))
		{
			view.cur_ref_pt[0] = LW_dyncp[0];
			view.cur_ref_pt[1] = LW_dyncp[1];
			view.cur_ref_pt[2] = LW_dyncp[2];
		}
		else
		{
			view.cur_ref_pt[0] = UV_dyncp[0];
			view.cur_ref_pt[1] = UV_dyncp[1];
			view.cur_ref_pt[2] = UV_dyncp[2];
		}
		if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		{
			sprintf(inbuffer, "DYNAMIC %s %f %f %f %f %f %f %f %f %f\n",
				view.name, view.cur_ref_pt[0], view.cur_ref_pt[1], view.cur_ref_pt[2],
				view.cur_pln_norm[0], view.cur_pln_norm[1], view.cur_pln_norm[2],
				view.cur_up_vect[0], view.cur_up_vect[1], view.cur_up_vect[2]);			
			ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
			ux_fputs0(inbuffer, fd);
		}
		uv_putv(&view);
		uv_updatevp(vport, UU_TRUE);

		gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO) ;
		if ((UZ_nclipv_view == 1)|| (LW_nclipv==LW_STANDALONE))
		{	
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
	}
done:;
	if (pick_save)
	{
		ud_restore_hilgt_seg();
	}
	uu_dexit;
}


/********************************************************************* 
**  E_FUNCTION:        uv_dyntumble(vport)
**      Change the view plane normal.
**  PARAMETERS   
**      INPUT:    
**				vport						viewport entity
**      OUTPUT:  
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uv_dyntumble(vport)
	UV_vport	*vport;

	{
	Glocrec locrec ;
	Gdrect locwin;
	Gqloc *greqloc();
	static char panprompt[] = "Dynamic xy rotate in effect";
	Gerror rtn;
	UV_view	view;
	Gwpoint3 pt;
	Gwpoint3 *vup;
	UM_coord csav;
	FILE *fd;
	int status,nc, pick_save;
	char inbuffer[500], tmpstr[200], *tok;
	float t1, t2, t3, t4, t5, t6, t7, t8, t9;

	uu_denter( UU_MTRC,(us,"uv_dyntumble()"));

/*
.....during dynamic rotaion attach the leader line to the left corner of the
.....label box so that the leader lines always appear attached to the labels
*/
	NCL_dyn_ldr =1;
	ncl_display_ldr(vport);

	pick_save = 0;
	if (UD_picking_active)
	{
/*		ud_unhilight_pickseg();
		pick_save = 1;*/
	}
	gsnormtran(vport->xform);

	locrec.prompt = panprompt;				/* locator data record */
	if (UZ_nclipv_view)
	{
		csav[0] = LW_dyncp[0];
		csav[1] = LW_dyncp[1];
		csav[2] = LW_dyncp[2];
	}
	else
	{
		csav[0] = UV_dyncp[0];
		csav[1] = UV_dyncp[1];
		csav[2] = UV_dyncp[2];
	}
	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
		status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status != UU_SUCCESS)
			goto done;
		tok = (char*)strtok(inbuffer, " \n");
		if (tok==NULL) goto done;
		tok = (char*)strtok(NULL, " \n");
		if (tok==NULL) goto done;
		strcpy(view.name, tok);	
		nc = strlen(tok);
		strcpy(tmpstr, tok+nc+1);
		sscanf(tmpstr, "%f %f %f %f %f %f %f %f %f\n",
				&t1, &t2, &t3,
				&t4, &t5, &t6,
				&t7, &t8, &t9);			
		view.cur_ref_pt[0] = t1;
		view.cur_ref_pt[1] = t2;
		view.cur_ref_pt[2] = t3;

		view.cur_pln_norm[0] = t4;
		view.cur_pln_norm[1] = t5;
		view.cur_pln_norm[2] = t6;

		view.cur_up_vect[0] = t7;
		view.cur_up_vect[1] = t8;
		view.cur_up_vect[2] = t9;

		gsvref3(vport->xform, &(view.cur_ref_pt));
		gsvpn3(vport->xform, &(view.cur_pln_norm[0]));
		gsvup3(vport->xform, &(view.cur_up_vect));
		
		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);

		if (UZ_nclipv_view)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
		goto done;
	}
	gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO) ;
	uv_locwin(&locwin);
	rtn=ginitloc(UD_ksws, 1, &UD_nxhair, 44, &locwin, &locrec);
	if (rtn==NCL_NO_ERROR)
		{
		greqloc(UD_ksws, 1);
		NCL_dyn_ldr =0;

		/* set the view plane normal */
		givpn3(vport->xform, &pt);
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		view.cur_pln_norm[0] = pt.x;
		view.cur_pln_norm[1] = pt.y;
		view.cur_pln_norm[2] = pt.z;
	
		/* set the view up vector */
		vup = gqvup3(vport->xform);
		view.cur_up_vect[0] = (*vup).x;
		view.cur_up_vect[1] = (*vup).y;
		view.cur_up_vect[2] = (*vup).z;
/*
.....Update the view reference point
*/
		if (UZ_nclipv_view)
		{
			view.cur_ref_pt[0] = LW_dyncp[0];
			view.cur_ref_pt[1] = LW_dyncp[1];
			view.cur_ref_pt[2] = LW_dyncp[2];
		}
		else
		{
			view.cur_ref_pt[0] = UV_dyncp[0];
			view.cur_ref_pt[1] = UV_dyncp[1];
			view.cur_ref_pt[2] = UV_dyncp[2];
		}
		if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		{
			sprintf(inbuffer, "DYNAMIC %s %f %f %f %f %f %f %f %f %f\n",
				view.name, view.cur_ref_pt[0], view.cur_ref_pt[1], view.cur_ref_pt[2],
				view.cur_pln_norm[0], view.cur_pln_norm[1], view.cur_pln_norm[2],
				view.cur_up_vect[0], view.cur_up_vect[1], view.cur_up_vect[2]);			
			ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
			ux_fputs0(inbuffer, fd);
		}
		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);

		gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO) ;
		if (UZ_nclipv_view)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
	}
done:;
	if (pick_save)
	{
		ud_restore_hilgt_seg();
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:        uv_dynvecrot(vport)
**      Change the view plane normal.
**  PARAMETERS   
**      INPUT:    
**				vport						viewport entity
**      OUTPUT:  
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uv_dynvecrot(vport)
	UV_vport	*vport;

{
	Glocrec locrec ;
	Gdrect locwin;
	Gqloc *greqloc();
	static char panprompt[] = "Dynamic vector rotate in effect";
	Gerror rtn;
	UV_view	view;
	Gwpoint3 pt;
	Gwpoint3 *vup;
	FILE *fd;
	int status,nc,pick_save;
	char inbuffer[500], tmpstr[200], *tok;
	float t1, t2, t3, t4, t5, t6, t7, t8, t9;
/*	UM_coord csav;*/

/*
.....during dynamic rotaion attach the leader line to the left corner of the
.....label box so that the leader lines always appear attached to the labels
*/
	NCL_dyn_ldr =1;
	ncl_display_ldr(vport);

	pick_save = 0;
	if (UD_picking_active)
	{
/*		ud_unhilight_pickseg();
		pick_save = 1;*/
	}
	gsnormtran(vport->xform);

	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
		status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status != UU_SUCCESS)
			goto done;
		tok = (char*)strtok(inbuffer, " \n");
		if (tok==NULL) goto done;
		tok = (char*)strtok(NULL, " \n");
		if (tok==NULL) goto done;
		strcpy(view.name, tok);	
		nc = strlen(tok);
		strcpy(tmpstr, tok+nc+1);
		sscanf(tmpstr, "%f %f %f %f %f %f %f %f %f\n",
				&t1, &t2, &t3,
				&t4, &t5, &t6,
				&t7, &t8, &t9);			
		view.cur_ref_pt[0] = t1;
		view.cur_ref_pt[1] = t2;
		view.cur_ref_pt[2] = t3;

		view.cur_pln_norm[0] = t4;
		view.cur_pln_norm[1] = t5;
		view.cur_pln_norm[2] = t6;

		view.cur_up_vect[0] = t7;
		view.cur_up_vect[1] = t8;
		view.cur_up_vect[2] = t9;

		gsvref3(vport->xform, &(view.cur_ref_pt));
		gsvpn3(vport->xform, &(view.cur_pln_norm[0]));
		gsvup3(vport->xform, &(view.cur_up_vect));
		
		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);
		goto done;
	}
	locrec.prompt = panprompt;				/* locator data record */
	gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO) ;
	uv_locwin(&locwin);
	rtn=ginitloc(UD_ksws, 1, &UD_nxhair, 45, &locwin, &locrec);
	if (rtn==NCL_NO_ERROR)
	{
		greqloc(UD_ksws, 1);
		NCL_dyn_ldr =0;

		/* set the view plane normal */
		givpn3(vport->xform, &pt);
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		view.cur_pln_norm[0] = pt.x;
		view.cur_pln_norm[1] = pt.y;
		view.cur_pln_norm[2] = pt.z;
	
		/* set the view up vector */
		vup = gqvup3(vport->xform);
		view.cur_up_vect[0] = (*vup).x;
		view.cur_up_vect[1] = (*vup).y;
		view.cur_up_vect[2] = (*vup).z;
/*
.....Update the view reference point
*/
		if (UZ_nclipv_view)
		{
			view.cur_ref_pt[0] = LW_dyncp[0];
			view.cur_ref_pt[1] = LW_dyncp[1];
			view.cur_ref_pt[2] = LW_dyncp[2];
		}
		else
		{
			view.cur_ref_pt[0] = UV_dyncp[0];
			view.cur_ref_pt[1] = UV_dyncp[1];
			view.cur_ref_pt[2] = UV_dyncp[2];
		}
		if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		{
			sprintf(inbuffer, "DYNAMIC %s %f %f %f %f %f %f %f %f %f\n",
				view.name, view.cur_ref_pt[0], view.cur_ref_pt[1], view.cur_ref_pt[2],
				view.cur_pln_norm[0], view.cur_pln_norm[1], view.cur_pln_norm[2],
				view.cur_up_vect[0], view.cur_up_vect[1], view.cur_up_vect[2]);			
			ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
			ux_fputs0(inbuffer, fd);
		}
		uv_putv(&view);

		uv_updatevp(vport, UU_TRUE);

		gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO) ;
/*		UV_dyncp[0] = csav[0];
		UV_dyncp[1] = csav[1];
		UV_dyncp[2] = csav[2];*/
	}
done:;
	if (pick_save)
	{
		ud_restore_hilgt_seg();
	}
}

/********************************************************************* 
**  E_FUNCTION:        uv_dynmouse(vport)
**      Change the view plane normal.
**  PARAMETERS   
**      INPUT:    
**				vport						viewport entity
**      OUTPUT:  
**				none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uv_dynmouse(vport)
	UV_vport	*vport;

{
	Glocrec locrec ;
	Gdrect locwin;
	Gqloc *greqloc();
	static char panprompt[] = "Dynamic viewing in effect";
	Gerror rtn;
	UV_view	view;
	Gwpoint3 pt;
	Gwpoint3 *vup;
	UM_coord csav;
	Gwrect3 *window;
	FILE *fd;
	int status,nc, pick_save;
	char inbuffer[500], tmpstr[200], *tok;
	float t1, t2, t3, t4, t5, t6, t7, t8, t9;
	Gwrect3 wswin;
/*
.....during dynamic rotaion attach the leader line to the left corner of the
.....label box so that the leader lines always appear attached to the labels
*/
	NCL_dyn_ldr =1;
	ncl_display_ldr(vport);
	pick_save = 0;
	if (UD_picking_active)
	{
/*		ud_unhilight_pickseg();
		pick_save = 1;*/
	}
	gsnormtran(vport->xform);

	locrec.prompt = panprompt;				/* locator data record */
	vport = vport;
	if (UZ_nclipv_view == 1)
	{
		csav[0] = LW_dyncp[0];
		csav[1] = LW_dyncp[1];
		csav[2] = LW_dyncp[2];
	}
	else
	{
		csav[0] = UV_dyncp[0];
		csav[1] = UV_dyncp[1];
		csav[2] = UV_dyncp[2];
	}
	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);

		ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
		status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status != UU_SUCCESS)
			goto done;
		tok = (char*)strtok(inbuffer, " \n");
		if (tok==NULL) goto done;
		tok = (char*)strtok(NULL, " \n");
		if (tok==NULL) goto done;
		strcpy(view.name, tok);	
		nc = strlen(tok);
		strcpy(tmpstr, tok+nc+1);
		sscanf(tmpstr, "%f %f %f %f %f %f %f %f %f\n",
				&t1, &t2, &t3,
				&t4, &t5, &t6,
				&t7, &t8, &t9);			
		view.cur_ref_pt[0] = t1;
		view.cur_ref_pt[1] = t2;
		view.cur_ref_pt[2] = t3;

		view.cur_pln_norm[0] = t4;
		view.cur_pln_norm[1] = t5;
		view.cur_pln_norm[2] = t6;

		view.cur_up_vect[0] = t7;
		view.cur_up_vect[1] = t8;
		view.cur_up_vect[2] = t9;

		gsvref3(vport->xform, &(view.cur_ref_pt));
		gsvpn3(vport->xform, &(view.cur_pln_norm[0]));
		gsvup3(vport->xform, &(view.cur_up_vect));
		
		status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status != UU_SUCCESS)
			goto done;
		sscanf(inbuffer, "DYNAMIC %f %f %f %f %f %f\n",
				&t1, &t2, &t3,
				&t4, &t5, &t6);			
		wswin.llf.x = t1;
		wswin.llf.y = t2;
		wswin.llf.z = t3;
		wswin.urb.x = t4;
		wswin.urb.y = t5;
		wswin.urb.z = t6;
		gswindow3(vport->xform,&wswin);		
		window = gqwindow3(vport->xform);
		view.cur_aperture = (window->urb.x - window->llf.x);

		uv_putv(&view);

		if (UZ_nclipv_view == 1)
		{
			um_set_screen_area(UM_IPV_WINDOW);
			uv_setxform(vport);
			ul_ipv_view_same(vport->xform);
		}
		else uv_updatevp(vport, UU_TRUE);


		if (UZ_nclipv_view == 1)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
		goto done;
	}
	gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO) ;
	uv_locwin(&locwin);
	rtn=ginitloc(UD_ksws, 1, &UD_nxhair, 46, &locwin, &locrec);
	if (rtn==NCL_NO_ERROR)
	{
		
		greqloc(UD_ksws, 1);
		NCL_dyn_ldr =0;

		/* set the view plane normal */
		givpn3(vport->xform, &pt);
		uv_getvid(vport->cur_view, &view);
/*
.....Save the previous view
.....Bobby  -  9/30/97
*/
		nclu_update_pre_view(view);
/*
.....Set the view normal
*/
		view.cur_pln_norm[0] = pt.x;
		view.cur_pln_norm[1] = pt.y;
		view.cur_pln_norm[2] = pt.z;
/*
.....Set the view up vector
*/
		vup = gqvup3(vport->xform);
		view.cur_up_vect[0] = (*vup).x;
		view.cur_up_vect[1] = (*vup).y;
		view.cur_up_vect[2] = (*vup).z;
/*
.....Set the aperture
*/
		window = gqwindow3(vport->xform);
		view.cur_aperture = (window->urb.x - window->llf.x);
/*
.....Update the view reference point
*/
		if (UZ_nclipv_view == 1)
		{
			view.cur_ref_pt[0] = LW_dyncp[0];
			view.cur_ref_pt[1] = LW_dyncp[1];
			view.cur_ref_pt[2] = LW_dyncp[2];
		}
		else
		{
			view.cur_ref_pt[0] = UV_dyncp[0];
			view.cur_ref_pt[1] = UV_dyncp[1];
			view.cur_ref_pt[2] = UV_dyncp[2];
		}
		if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		{
			sprintf(inbuffer, "DYNAMIC %s %f %f %f %f %f %f %f %f %f\n",
				view.name, view.cur_ref_pt[0], view.cur_ref_pt[1], view.cur_ref_pt[2],
				view.cur_pln_norm[0], view.cur_pln_norm[1], view.cur_pln_norm[2],
				view.cur_up_vect[0], view.cur_up_vect[1], view.cur_up_vect[2]);			
			ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
			ux_fputs0(inbuffer, fd);
			sprintf(inbuffer, "DYNAMIC %f %f %f %f %f %f\n",
				window->llf.x, window->llf.y, window->llf.z,
				window->urb.x, window->urb.y, window->urb.z);	
			ux_fputs0(inbuffer, fd);
		}
		uv_putv(&view);
		if (UZ_nclipv_view == 1)
		{
			um_set_screen_area(UM_IPV_WINDOW);
			uv_setxform(vport);
			ul_ipv_view_same(vport->xform);
		}
		else 
		{
			um_set_screen_area(UM_NCL_WINDOW);
			gswswindow(UD_ksws,&(UD_duimsdeflt.screen[0].wswind));
			uv_updatevp(vport, UU_TRUE);
		}
		gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO) ;
		if (UZ_nclipv_view == 1)
		{
			LW_dyncp[0] = csav[0];
			LW_dyncp[1] = csav[1];
			LW_dyncp[2] = csav[2];
		}
		else
		{
			UV_dyncp[0] = csav[0];
			UV_dyncp[1] = csav[1];
			UV_dyncp[2] = csav[2];
		}
	}
done:;
	if (pick_save)
	{
		ud_restore_hilgt_seg();
	}
}

/*********************************************************************
**    I_FUNCTION :  uv_locwin(rect) -- get UD_LPRMT echo area.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  Gdrect *rect -- echo area is put here.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_locwin(rect)
Gdrect *rect;
	{
	int scrn;
	UD_AREA *areapt;
/*	char us[150];*/

	scrn=UD_curlayout.curr_screen;
	areapt= &(UD_duimsdeflt.screen[scrn].areas[UD_LPRMT])[UD_curlayout.curr_lex_prompt_area];
	ud_devrect(&(*areapt).posn,rect);
/*	uu_dprint(UU_MTRC,(us,"uv_locwin returns %g %g, %g %g",(*rect).ll.x,
		(*rect).ll.y,(*rect).ur.x,(*rect).ur.y));*/
	}

/*********************************************************************
**    E_FUNCTION :  uv_get_box_midz(llf, urb, midpt)
**       Calculates the NDC midpoint of the active viewport as defined
**       by its corner points.  The Z-level of the midpoint is calculated
**       by averaging the Z-depths of all geometry displayed in the
**       viewport.
**    PARAMETERS   
**       INPUT  :
**          llf    = Lower left of viewport.
**          urb    = Upper right of viewport.
**       OUTPUT :
**          midpt  = Middle point of viewport.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_get_box_midz(llf, urb, midpt)
UU_REAL midpt[3], llf[3], urb[3];
{
	int i,n;
	UU_LOGICAL first;
	UU_REAL nx, ny, nz, minz, maxz, midz,z;
	UM_coord pt1;
	Gnrect rect;
	UG_segstli *p;
char sbuf[80];
/*
.....Loop through the displayed segments
*/
	minz = maxz = 0;
	first = UU_TRUE;
	ug_seginitscan();
	while( (p=ug_segscan()) != NULL ) 
	{
		if (ud_isassist_seg(p->segid)!=0)
			continue;
		if((p->segatts.gvis   == UG_VISIBLE) &&
			(p->segatts.gdtect == UG_DETECTABLE) )
		{	
			if (p->wcboxok==1)
			{
				ug_segndcbox(p->segid,&rect);
/*
......Boxes overlap
*/
				if (urb[0] >= rect.ll.x && llf[0] <= rect.ur.x &&
					 urb[1] >= rect.ll.y && llf[1] <= rect.ur.y)
				{
/*
.........Calculate average NDC Z-level
.........for intersecting box
*/

					um_vctovc(&p->wcbox.llf,pt1);
					z = 0.; n = 0;
					for (i=0;i<9;i++)
					{
						gwndc3(&nx,&ny,&nz,pt1[0],pt1[1],pt1[2]);
						if (urb[0] >= nx && llf[0] <= nx &&
							 urb[1] >= ny && llf[1] <= ny)
						{
							z = z + nz;
							n++;
						}
						if (i == 0) pt1[0] = p->wcbox.urb.x;
						else if (i == 1) pt1[2] = p->wcbox.urb.z;
						else if (i == 2) pt1[1] = p->wcbox.urb.y;
						else if (i == 3) pt1[2] = p->wcbox.llf.z;
						else if (i == 4) pt1[0] = p->wcbox.llf.x;
						else if (i == 5) pt1[0] = p->wcbox.urb.x;
						else if (i == 6) pt1[1] = p->wcbox.llf.y;
						else
						{
							pt1[0]=p->wcbox.llf.x + (p->wcbox.urb.x-p->wcbox.llf.x)/2.;
							pt1[1]=p->wcbox.llf.y + (p->wcbox.urb.y-p->wcbox.llf.y)/2.;
							pt1[2]=p->wcbox.llf.z + (p->wcbox.urb.z-p->wcbox.llf.z)/2.;
						}
					}
					if (n == 0)
					{
						z = nz;
						n = 1;
					}
					z = z / n;
/*
.........Save this Z-level and compare min/max Z
*/
					if (first)
					{
						minz = z;
						maxz = z;
						first = UU_FALSE;
					}
					else
					{
						if (z < minz) minz = z;
						if (z > maxz) maxz = z;
					}
				}
			}
		}
	}
/*
.....Calculate viewport midpoint
*/
	midpt[0] = (urb[0] + llf[0]) / 2.0;
	midpt[1] = (urb[1] + llf[1]) / 2.0;
	midpt[2] = (maxz + minz) / 2.0;
}

