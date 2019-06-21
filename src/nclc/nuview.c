/*********************************************************************
**    NAME         :  nuview.c
**       CONTAINS:
**         nclu_change_view
**         nclu_view_from_axis
**         nclu_get_mx_view
**         nclu_tracut_view
**         nclu_refsys_view
**         nclu_tool_view
**         nclu_update_view
**         nclu_update_pre_view
**         nclu_reset_prev
**         nclu_modify_view(viewname,rsorig, rszvec, rsyvec, scale,stat)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuview.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:17
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "dmark.h"
#include "lipv.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclfc.h"
#include "view.h"
#include "mfort.h"
#include "mdcoord.h"
#include "modef.h"
#include "class.h"

extern int UZ_nclipv_view;
extern UU_REAL UM_model_size;

struct UV_pre_view_rec
	{
	UU_REAL	pre_ref_pt[3];
	UU_REAL	pre_pln_norm[3];
	UU_REAL	pre_up_vect[3];
	UU_REAL	pre_eye_dist;
	UU_REAL	pre_aperture;
	UU_REAL	pre_front_clip;
	UU_REAL	pre_back_clip;
	UU_LOGICAL	pre_do_clip;
	};
/*
.....Initialize previous view
.....Bobby  -  2/24/92
*/
static struct UV_pre_view_rec pre_view={0,0,0, 0,0,0, 0,0,0, 0,0,0,0,0};

void nclu_update_view(),nclu_update_pre_view();

/*********************************************************************
**    E_FUNCTION     : ncl_change_view()
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* This routine is not used anymore
nclu_change_view()
   {
   int status;
   int choice;

   uu_denter( UU_MTRC,(us,"nclu_change_view()"));

   status = NCL_OKINPUT;

   while (status != NCL_DONE)
		{
		status = ncl_popup(NCL_VIEW_CHANGE, &choice);
		switch(choice)
       		{
     		case 1:   /* view parameters 
				uvu_change_view();
     			break;

	      case 2:   /* current tracut 
				nclu_tracut_view();
        		break;

     		case 3:  /* current refsys 
				nclu_refsys_view();
          	break;

     		case 4:  /* predefined matrix 
				nclu_get_mx_view();
           	break;
			}
		 } 
   uu_dexit;
   }

*/
/*********************************************************************
**    E_FUNCTION     : ncl_view_from_axis()
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* This routine is not used anymore
nclu_view_from_axis()
   {
   int status;
   int choice;

   uu_denter( UU_MTRC,(us,"nclu_view_from_axis()"));

   status = NCL_OKINPUT;

   while (status != NCL_DONE)
		{
     	status = ncl_popup(NCL_VIEW_AXIS, &choice);
    	switch(choice)
   	    {
   	    case 1:   /* view from X 
				uvu_vwpnorm(0);
       		break;

          case 2:   /* view from Y 
				uvu_vwpnorm(1);
            break;

          case 3:  /* view from Z
				uvu_vwpnorm(2);
            break;
		    } 
		}  	 
   uu_dexit;
   }

*/
/***********************************************************************
**  E_FUNCTION:  nclu_get_mx_view()
**			Change the viewing parameters by a predefined matrix.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
nclu_get_mx_view()

	{
	UM_real8 ncl_origin[3];
	UM_real8 ncl_zaxis[3];
	UM_real8 ncl_yaxis[3];
	UU_LOGICAL drw_vp_info, loop_par;
	UU_REAL magnification;		/* magnification factor */
	int status,isav;

    char mx_name[16];

	uu_denter(UU_MTRC,(us,"nclu_get_mx_view()"));
loop:;

	magnification = 1.;
	loop_par = UU_FALSE;
	isav = UZ_nclipv_view;
	status = ncl_get_str(mx_name, 416);
	gtmx(mx_name,ncl_origin,ncl_zaxis,ncl_yaxis,&loop_par);
	if (loop_par) goto loop;

	UZ_nclipv_view = isav;
	nclu_update_view(ncl_origin, ncl_zaxis, ncl_yaxis);

	uu_dexit;
	}
/***********************************************************************
**  E_FUNCTION:  nclu_tracut_view()
**			Change the viewing parameters by the current tracut 
**			matrix.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
nclu_tracut_view()

	{
	UM_int2 option, status;
	UM_real8 ncl_origin[3];
	UM_real8 ncl_zaxis[3];
	UM_real8 ncl_yaxis[3];
	UU_REAL magnification;		/* magnification factor */

	uu_denter(UU_MTRC,(us,"nclu_tracut_view()"));

	magnification = 1.;
	option=2;
	gtview(ncl_origin, ncl_zaxis, ncl_yaxis, &option, &status);
	if (status == 0)
		nclu_update_view(ncl_origin, ncl_zaxis, ncl_yaxis);
	else
	   uu_uerror0(UA_NCL, 15);	
	uu_dexit;

	}
/***********************************************************************
**  E_FUNCTION:  nclu_refsys_view()
**			Change the viewing parameters by the current refsys 
**			matrix.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
nclu_refsys_view()

	{

	UM_int2 option, status;
	UM_real8 ncl_origin[3];
	UM_real8 ncl_zaxis[3];
	UM_real8 ncl_yaxis[3];
	UU_REAL magnification;		/* magnification factor */

	uu_denter(UU_MTRC,(us,"nclu_refsys_view()"));

	magnification = 1.;
	option = 1;
	gtview(ncl_origin, ncl_zaxis, ncl_yaxis, &option, &status);
	if (status == 0)
		nclu_update_view(ncl_origin, ncl_zaxis, ncl_yaxis);
	else
	   uu_uerror0(UA_NCL, 16);	
	uu_dexit;
	}

/***********************************************************************
**  E_FUNCTION:  nclu_tool_view()
**     Change the viewing parameters to the current tool.
**  PARAMETERS
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void nclu_tool_view()

	{
	UM_int2 option, status;
	UM_real8 ncl_origin[3];
	UM_real8 ncl_zaxis[3];
	UM_real8 ncl_yaxis[3];
	UM_real8 ncl_fwd[3];
	UU_REAL um_mag();

	if (UZ_nclipv_view == 1)
	{
		um_vctovc(LW_tool_pos,ncl_origin);
		um_vctovc(&LW_tool_pos[3],ncl_zaxis);
		um_vcmnvc(LW_tool_pos,LW_last_tool_pos,ncl_fwd);
		um_unitvc(ncl_fwd,ncl_fwd);
	}
	else
	{
		option=3;
		gtview(ncl_origin, ncl_zaxis, ncl_fwd, &option, &status);
	}
	um_cross(ncl_zaxis, ncl_fwd, ncl_yaxis);
	if (um_mag(ncl_yaxis) < UM_FUZZ)
	{
		um_perpvc(ncl_zaxis, ncl_yaxis);
	}
	um_unitvc(ncl_yaxis, ncl_yaxis);
	nclu_update_view(ncl_origin, ncl_zaxis, ncl_yaxis);

	}

/***********************************************************************
**  E_FUNCTION:  nclu_update_view()
**			Update the viewing by the chosen methode.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void nclu_update_view(ncl_origin,ncl_zaxis,ncl_yaxis)
UM_real8 ncl_origin[3];
UM_real8 ncl_zaxis[3];
UM_real8 ncl_yaxis[3];

	{
	static UM_vector norm;				/* normal vector */
	static UM_coord ref_pt;				/* reference point */
	static UM_vector up_vec;			/* up vector */

	UU_REAL magnification;		/* magnification factor */
	UU_REAL wxlen, wylen;

	UV_vport vport;
	UV_view view;
	int status;
	UU_REAL old_aperture;
	UU_LOGICAL drw_vp_info;


	uu_denter(UU_MTRC,(us,"nclu_update_view()"));

	ncl_real8_to_uureal(3, ncl_origin, ref_pt);
	ncl_real8_to_uureal(3, ncl_zaxis, norm);
	ncl_real8_to_uureal(3, ncl_yaxis, up_vec);

	if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
		uv_getvid(vport.cur_view, &view);

		nclu_update_pre_view(view);

		old_aperture = view.cur_aperture;
		magnification= um_mag(norm);

		um_unitvc(norm, norm);
		um_unitvc(up_vec, up_vec);

		if (!um_vcparall(norm, up_vec))
			{
			um_cross(up_vec, norm, up_vec);
			um_cross(norm, up_vec, up_vec);
			um_unitvc(up_vec, up_vec);
			uv_update_pln_norm(&view, norm);
			uv_update_up_vect(&view, up_vec);
			uv_update_ref_pt(&view, ref_pt);
		
			/* set the new view boundries */
			wxlen = view.cur_aperture;
			/*wxlen = wxlen / magnification;*/
			uv_update_vaperture(&view, wxlen);

			view.modified = UU_TRUE;
			drw_vp_info = UU_TRUE;
			uv_delete_hidden(&vport);
			uv_autofact7_redrawvp(&vport, &view, drw_vp_info);
			}
		}

	uu_dexit;
	}

/***********************************************************************
**  E_FUNCTION:  nclu_update_pre_view()
**			Update the viewing by the chosen methode.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
void nclu_update_pre_view(view)

UV_view view;
	{
	/*static struct UV_pre_view_rec pre_view;*/
	UM_vector vup,vpn;


	uu_denter(UU_MTRC,(us,"nclu_update_pre_view()"));

	um_unitvc(view.cur_pln_norm, vpn);
	um_vctovc(vpn, pre_view.pre_pln_norm);
	um_unitvc(view.cur_up_vect, vup);
	um_vctovc(vup, pre_view.pre_up_vect);
	um_vctovc(view.cur_ref_pt, pre_view.pre_ref_pt);
	pre_view.pre_back_clip = view.cur_back_clip;
	pre_view.pre_front_clip = view.cur_front_clip;
	pre_view.pre_do_clip = view.cur_do_clip;
	pre_view.pre_aperture = view.cur_aperture;

	uu_dexit;
	}

/***********************************************************************
**  E_FUNCTION:  nclu_reset_prev(flag)
**			Update the viewing by the chosen methode.
**  PARAMETERS   
**      INPUT  : 
**         flag   = UU_TRUE = Prompt for viewport, UU_FALSE = Use active
**                  viewport.
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
nclu_reset_prev(flag)
UU_LOGICAL flag;

	{
	UV_view view;
	/*struct UV_pre_view_rec pre_view;*/
	UV_vport vport;
	int status;
	UU_LOGICAL drw_vp_info;
	UM_vector vup,vpn;
	UU_REAL	tmp_ref_pt[3];
	UU_REAL	tmp_pln_norm[3];
	UU_REAL	tmp_up_vect[3];
	UU_REAL	tmp_eye_dist;
	UU_REAL	tmp_aperture;
	UU_REAL	tmp_front_clip;
	UU_REAL	tmp_back_clip;
	UU_LOGICAL	tmp_do_clip;


	uu_denter(UU_MTRC,(us,"nclu_reset_prev()"));

/*
.....Ignore "Reset Previous View" if
.....this is the first view
.....Bobby  -  12/2/92
*/
	if (pre_view.pre_aperture == 0) goto done;

	if (flag)
	{
		if (uvu_pickvp(&vport) != UU_SUCCESS) goto done;
		uv_getvid(vport.cur_view, &view);
	}
	else
		 uv_get_sview(&vport,&view);
		
	um_unitvc(view.cur_pln_norm, vpn);
	um_vctovc(vpn, tmp_pln_norm);
	um_unitvc(view.cur_up_vect, vup);
	um_vctovc(vup, tmp_up_vect);
	um_vctovc(view.cur_ref_pt, tmp_ref_pt);
	tmp_back_clip = view.cur_back_clip;
	tmp_front_clip = view.cur_front_clip;
	tmp_do_clip = view.cur_do_clip;
	tmp_aperture = view.cur_aperture;

	uv_update_pln_norm(&view, pre_view.pre_pln_norm);
	uv_update_up_vect(&view, pre_view.pre_up_vect);
	uv_update_ref_pt(&view, pre_view.pre_ref_pt);
	uv_update_vaperture(&view, pre_view.pre_aperture);
	uv_update_fbclip(&view, pre_view.pre_front_clip, pre_view.pre_back_clip);
	uv_update_clip(&view, pre_view.pre_do_clip);

	um_vctovc(tmp_pln_norm, pre_view.pre_pln_norm);
	um_vctovc(tmp_up_vect, pre_view.pre_up_vect);
	um_vctovc(tmp_ref_pt, pre_view.pre_ref_pt);
	pre_view.pre_back_clip = tmp_back_clip;
	pre_view.pre_front_clip = tmp_front_clip;
	pre_view.pre_do_clip = tmp_do_clip;
	pre_view.pre_aperture = tmp_aperture;

	view.modified = UU_TRUE;
	drw_vp_info = UU_TRUE;
	uv_delete_hidden(&vport);
	uv_autofact7_redrawvp(&vport, &view, drw_vp_info);

done:;
	uu_dexit;
	}

/***********************************************************************
**  E_FUNCTION:  nclu_modify_view(viewname,rsorig, rszvec, rsyvec, scale)
**          To modify/create view from the DRAFT/VIEW command
**  PARAMETERS
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
***********************************************************************/
nclu_modify_view(viewname, ptkey, rsorig, zveckey, rszvec, upveckey, 
				 rsyvec, fscale, scale,stat)
UM_f77_str_ptr viewname;
UM_real8 *rsorig;
UM_real8 *rszvec;
UM_real8 *rsyvec;
UM_real8 *scale;
int      *stat;
int *ptkey, *zveckey, *upveckey, *fscale;
{
	char *p;
	int status,i;
	UV_view view;
	static UM_vector norm;              /* normal vector */
	static UM_coord ref_pt;             /* reference point */
	static UM_vector up_vec;            /* up vector */
	UU_REAL wxlen;
	/* NCL */
	UV_screen curscreen;
	UV_vport  vport;
	UU_KEY_ID view_key;
	UU_REAL in_scale;

	p = UM_cstr_of_f77_str(viewname);

/*....
......Try to get given view first. If it does not exist - create it.
......
*/
	status = uv_getvnm1(p, &view);
 
	if (status == -1)
	{
		uv_vdefine(p, &view);
		view.can_save = UU_TRUE;
	}
/*
.....if not change, using the old one.
*/
	if (*ptkey)
		ncl_real8_to_uureal(3, rsorig, ref_pt);
	else
	{
		ref_pt[0] = view.cur_ref_pt[0];
		ref_pt[1] = view.cur_ref_pt[1];
		ref_pt[2] = view.cur_ref_pt[2];
	}
	if (*zveckey)
		ncl_real8_to_uureal(3, rszvec, norm);
	else
	{
		norm[0] = view.cur_pln_norm[0];
		norm[1] = view.cur_pln_norm[1];
		norm[2] = view.cur_pln_norm[2];
	}
	if (*upveckey)
		ncl_real8_to_uureal(3, rsyvec, up_vec);
	else
	{
		up_vec[0] = view.cur_up_vect[0];
		up_vec[1] = view.cur_up_vect[1];
		up_vec[2] = view.cur_up_vect[2];
	}
	if (*fscale)
		in_scale = *scale;
	else
		in_scale = 1.0;
	nclu_update_pre_view(view);

	um_unitvc(norm, norm);
	um_unitvc(up_vec, up_vec);
	if (!um_vcparall(norm, up_vec))
	{
		um_cross(up_vec, norm, up_vec);
		um_cross(norm, up_vec, up_vec);
		um_unitvc(up_vec, up_vec);
		uv_update_pln_norm(&view, norm);
		uv_update_up_vect(&view, up_vec);
		uv_update_ref_pt(&view, ref_pt);
		uv_setvaperture(&view, UM_model_size);
      
         /* set the new view boundries */
		wxlen = view.cur_aperture;
		wxlen = wxlen / (in_scale);
		uv_update_vaperture(&view, wxlen);

		view.modified = UU_TRUE;
		view.can_save = UU_TRUE;
		uv_putv(&view);

		for (i = 0; i < UV_act_screen[0].nvports; i++)
		{
			status = uv_getvpid(UV_act_screen[0].vports[i], &vport);
            view_key = vport.cur_view;
            if(view.key == view_key)
            {
               uv_updatevp(&vport, UU_TRUE); 
            }
		}
		*stat = 0;
	}
	else
	{
		*stat = -1;
	}
}
