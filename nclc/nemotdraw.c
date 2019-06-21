/*********************************************************************
**    NAME         :  nemotdraw.c
**       CONTAINS:
**          ncl_create_drwmot
**          ncl_proj_motion_to_draw
**          ncl_put_cutterseg
**          ncl_plot_cutterseg
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nemotdraw.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:38
*********************************************************************/
#include "usysdef.h"
#include <math.h>
#include "bsym.h"
#include "driver.h"
#include "nclfc.h"
#include "gtbl.h"
#include "gobas.h"
#include "view.h"
#include "ginqatt.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdattr.h"
#include "modef.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "zsysdep.h"
#include "mplot.h"
#include "mdraw.h"
#include "nclmplay.h"

#include "dasnog.h"
#include "ulist.h"
#include "mxxx.h"
#include "mdgenent.h"

extern UN_motseg *mlist_ptr;
extern UN_motseg *mlist_first_ptr;
extern UU_REAL Temp_vpn[3], Temp_vup[3];
extern int moving_part;
extern UM_transf Temp_tfmat;
extern int plot_num[1000], plot_seg;
extern int Proj_motion;
extern UU_REAL Temp_scale;
extern UN_cutdef_view_struc ncl_drawing_cutstct;

void ncl_plot_motion_plyseg();
void ncl_plot_cutterseg();

/*********************************************************************
**    I_FUNCTION     :  ncl_create_drwmot(tfmat, view )
**       create motion segment according to the view and transformation.
**				this segment will used for drag when PlaceView on the drawing
**    PARAMETERS
**       INPUT  :
**             tfmat      = motion's transformation. 
**					view		  = view that we will place on the drawing
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_create_drwmot(tfmat, view)
UM_transf tfmat;
UV_view view;
{
#define MXINC 100
	int status,inc,ifirst,cfl;
	UN_motseg *mpt,cur_motatt;
	Gwpoint3 gpt[MXINC];
	UM_real8 cpt[9];
	int emov;
	Glntype lintyp,linestyle;
	Gcolor lincol;
	Gscale linwid;
	UV_vport vport;
	UM_coord pt, ptout;
	UN_motseg_cutter cutseg;
	UN_motseg_attr motattr,cur_motattr;
/*
.....No motion displayed
*/
	status = UU_FAILURE;
	if (mlist_first_ptr == (UN_motseg *)-1) goto done;
	if (moving_part == 1) goto done;
	Temp_vpn[0] = view.cur_pln_norm[0];
	Temp_vpn[1] = view.cur_pln_norm[1];
	Temp_vpn[2] = view.cur_pln_norm[2];
	Temp_vup[0] = view.cur_up_vect[0];
	Temp_vup[1] = view.cur_up_vect[1];
	Temp_vup[2] = view.cur_up_vect[2];
	Temp_tfmat[0][0] = tfmat[0][0];
	Temp_tfmat[0][1] = tfmat[0][1];
	Temp_tfmat[0][2] = tfmat[0][2];
	Temp_tfmat[1][0] = tfmat[1][0];
	Temp_tfmat[1][1] = tfmat[1][1];
	Temp_tfmat[1][2] = tfmat[1][2];
	Temp_tfmat[2][0] = tfmat[2][0];
	Temp_tfmat[2][1] = tfmat[2][1];
	Temp_tfmat[2][2] = tfmat[2][2];
	Temp_tfmat[3][0] = tfmat[3][0];
	Temp_tfmat[3][1] = tfmat[3][1];
	Temp_tfmat[3][2] = tfmat[3][2];
/*
.....Set up default motion display attributes
*/
	zbytecp(lintyp,*gqlinetype());
	lincol = gqlinecolor(); 
	linwid = gqlinewidth();
	ncl_motattr_get(&cur_motattr,last_motatt.attr);
	linestyle.typeno = cur_motattr.lnstyle;
	linestyle.npatn = 0;
	gslinetype(&linestyle);
	gslinecolor(cur_motattr.color); 
	gslinewidth(1.0);
	cur_motatt = last_motatt;
/*
......Set this view port's transormation matrix
......always use current vport
*/
	ug_sntran(1);
	uv_getvpid(UV_act_screen[0].vports[0],&vport);
/*
........Loop through motion list array
*/
	mpt = mlist_first_ptr;
	ncl_reset_cutseg(0);
	inc = 0;
	ifirst = 0;
	emov = 0;
	do
	{
		mpt = (UN_motseg *)uu_lsnext(mpt);
		if (mpt == 0) goto done;
		ncl_cutter_get(&cutseg,mpt->cutter);
		ncl_motattr_get(&motattr,mpt->attr);
/*
...........Set new motion display attributes
*/
		if (motattr.color != cur_motattr.color ||
			motattr.lnstyle != cur_motattr.lnstyle)
		{
/*
..............Display points currently buffered
*/
			if (inc > 1)
			{
				if (cur_motattr.color != 0) gpolyline3(inc,gpt);
				gpt[0].x = gpt[inc-1].x;
				gpt[0].y = gpt[inc-1].y;
				gpt[0].z = gpt[inc-1].z;
				inc = 1;
			}
			cur_motatt = *mpt;
			cur_motattr = motattr;
		}
/*
......added transformation and save motion point
*/
		pt[0] = mpt->tend.x;
		pt[1] = mpt->tend.y;
		pt[2] = mpt->tend.z;
		um_cctmtf(pt, tfmat, ptout);
		gpt[inc].x = ptout[0];
		gpt[inc].y = ptout[1];
		gpt[inc].z = ptout[2];
		
		cpt[0] = mpt->tend.x;
		cpt[1] = mpt->tend.y;
		cpt[2] = mpt->tend.z;
		cpt[3] = mpt->taxis.x;
		cpt[4] = mpt->taxis.y;
		cpt[5] = mpt->taxis.z;

		emov = cutseg.cattr->mov;
		inc++;
/*
..............Set motion display attributes
*/
		linestyle.typeno = cur_motattr.lnstyle;
		gslinetype(&linestyle);
		gslinecolor(cur_motattr.color); 
/*
......Determine if we need to
......display the cutter
*/
		cfl = 0;
		if (cutseg.cutsym->geo->type > 0 && cutseg.cattr->mov == 0 ) cfl = 1;
/*
.................Output current motion buffer
.................if at buffer limit or if
.................cutter is being displayed
*/
		if (inc == MXINC || (cfl == 1 && inc > 1))
		{
			if (cur_motattr.color != 0) gpolyline3(inc,gpt);
			gpt[0].x = gpt[inc-1].x;
			gpt[0].y = gpt[inc-1].y;
			gpt[0].z = gpt[inc-1].z;
			inc = 1;
		}
		last_motatt = cur_motatt;
/*
......Display cutter
*/
		if (cfl == 1)
		{
			ncl_cutter_post(cpt,mpt->blade,&cutseg,0,0);
			gslinetype(&linestyle);
			gslinecolor(cur_motattr.color); 
			gslinewidth(1.0);
		}
		ifirst = 1;
	} while (mpt != mlist_ptr && mpt != UN_step_ptr);
/*
...........Display last points in buffer
*/
	if (inc > 1) gpolyline3(inc,gpt);
/*
......Position moving cutter
......If Moving cutter, Dynamic View, and this viewport has motion
......Bobby  -  2/6/95
*/
	if (emov > 0 )
	{
		ncl_cutter_post(cpt,mpt->blade,&cutseg,0,0);
	}
/*
.....Reset line attributes
*/
	gslinetype(&lintyp);
	gslinecolor(lincol); 
	gslinewidth(linwid);
	status = UU_SUCCESS;
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  ncl_proj_motion_to_draw(vtfmat, scale, view,
**						drawing, pt_on_drawing,refpt, flag )
**       Put motion segment in the drawing.
**    PARAMETERS
**       INPUT  :
**             vtfmat      = draw vport's transformation. 
**					scale			= drawing scale
**					view			= view that we will projection
**					drawing		= drawing that we will projection to
**					pt_on_drawing = location points that selected for drawing
**					refpt			= shift point we need adjust
**					flag			= 1: project motion to fit in the draw
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_proj_motion_to_draw(vtfmat, scale, view, drawing, pt_on_drawing, 
refpt, flag)
struct UM_drawing_rec drawing;
UD_NDCLOCREC pt_on_drawing;
UU_REAL refpt[];
int flag;
UU_REAL scale;
UM_transf vtfmat;
UV_view view;
{
#define MXINC 100
	Gwpoint3 gpt[100];
	int status,inc,ifirst,cfl;
	UN_motseg *mpt,cur_motatt;
	UM_real8 cpt[9];
	int emov;
	UM_coord pt, ptout;
	UU_REAL ref[3], drw_x, drw_y, dx, dy;
	UM_transf scale_tfmat, tfmat, ref_tfmat, refs_tfmat, area_tfmat;
	UM_vector drawing_scale;
	UN_motseg_cutter cutseg;
	UN_motseg_attr motattr,cur_motattr;

	Proj_motion = 1;
/*
.....No motion displayed
*/
	status = UU_FAILURE;
	if (mlist_first_ptr == (UN_motseg *)-1) goto done;
	if (moving_part == 1) goto done;        
	um_get_drawing_extents(drawing.drwsize, &drw_x, &drw_y, &dx, &dy);
	ref[0] = drw_x/2;
	ref[1] = drw_y/2;
	ref[2] =0;
	um_disptf(&ref, refs_tfmat);
	drawing_scale[0] = scale;
	drawing_scale[1] = scale;
	drawing_scale[2] = scale;
	um_scaletf(drawing_scale, scale_tfmat);
	if (flag!=1)
		um_tftmtf(vtfmat, scale_tfmat, vtfmat);
	um_tftmtf(vtfmat, refs_tfmat, tfmat);
	Temp_vpn[0] = view.cur_pln_norm[0];
	Temp_vpn[1] = view.cur_pln_norm[1];
	Temp_vpn[2] = view.cur_pln_norm[2];
	Temp_vup[0] = view.cur_up_vect[0];
	Temp_vup[1] = view.cur_up_vect[1];
	Temp_vup[2] = view.cur_up_vect[2];
	Temp_tfmat[0][0] = tfmat[0][0];
	Temp_tfmat[0][1] = tfmat[0][1];
	Temp_tfmat[0][2] = tfmat[0][2];
	Temp_tfmat[1][0] = tfmat[1][0];
	Temp_tfmat[1][1] = tfmat[1][1];
	Temp_tfmat[1][2] = tfmat[1][2];
	Temp_tfmat[2][0] = tfmat[2][0];
	Temp_tfmat[2][1] = tfmat[2][1];
	Temp_tfmat[2][2] = tfmat[2][2];
	Temp_tfmat[3][0] = tfmat[3][0];
	Temp_tfmat[3][1] = tfmat[3][1];
	Temp_tfmat[3][2] = tfmat[3][2];
	Temp_scale = scale;

	um_disptf(refpt, ref_tfmat);
	um_disptf(&pt_on_drawing, area_tfmat);
/*
........Loop through motion list array
*/
	mpt = mlist_first_ptr;
	cur_motatt = last_motatt;
	ncl_motattr_get(&cur_motattr,mpt->attr);
	ncl_reset_cutseg(0);
	inc = 0;
	ifirst = 0;
	emov = 0;
	do
	{
		mpt = (UN_motseg *)uu_lsnext(mpt);
		if (mpt == 0) goto done;
		ncl_cutter_get(&cutseg,mpt->cutter);
		ncl_motattr_get(&motattr,mpt->attr);
/*
...........Set new motion display attributes
*/
		if (motattr.color != cur_motattr.color ||
			motattr.lnstyle != cur_motattr.lnstyle)
		{
/*
..............Display points currently buffered
*/
			if (inc > 1)
			{
				if (cur_motattr.color != 0) 
				{
					ncl_plot_motion_plyseg(gpt, inc, drawing, cur_motattr.color, 
								cur_motattr.pen,
								cur_motattr.lnstyle,  
								area_tfmat, ref_tfmat, flag);
				}					
				gpt[0].x = gpt[inc-1].x;
				gpt[0].y = gpt[inc-1].y;
				gpt[0].z = gpt[inc-1].z;
				inc = 1;
			}
			cur_motatt = *mpt;
			cur_motattr = motattr;
		}
/*
......added transformation and save motion point
*/
		pt[0] = mpt->tend.x;
		pt[1] = mpt->tend.y;
		pt[2] = mpt->tend.z;
		um_cctmtf(pt, tfmat, ptout);
		gpt[inc].x = ptout[0];
		gpt[inc].y = ptout[1];
		gpt[inc].z = ptout[2];
		
		cpt[0] = mpt->tend.x;
		cpt[1] = mpt->tend.y;
		cpt[2] = mpt->tend.z;
		cpt[3] = mpt->taxis.x;
		cpt[4] = mpt->taxis.y;
		cpt[5] = mpt->taxis.z;

		emov = cutseg.cattr->mov;
		inc++;
/*
......Determine if we need to
......display the cutter
*/
		cfl = 0;
		if (cutseg.cutsym->geo->type > 0 && cutseg.cattr->mov == 0 ) cfl = 1;
/*
.................Output current motion buffer
.................if at buffer limit or if
.................cutter is being displayed
*/
		if (inc == MXINC || (cfl == 1 && inc > 1))
		{
			if (cur_motattr.color != 0) 
			{
				ncl_plot_motion_plyseg(gpt, inc, drawing, cur_motattr.color, 
						cur_motattr.pen, cur_motattr.lnstyle,  
						area_tfmat, ref_tfmat, flag);
			}
			gpt[0].x = gpt[inc-1].x;
			gpt[0].y = gpt[inc-1].y;
			gpt[0].z = gpt[inc-1].z;
			inc = 1;
		}
		last_motatt = cur_motatt;
/*
......Display cutter
*/
		if (cfl == 1)
		{
			ncl_cutter_post(cpt,mpt->blade,&cutseg,0,0);
/*
.....put cutter segment into drawing
*/
			ncl_plot_cutterseg(cpt,drawing,cutseg.cattr->color,cutseg.cattr->pen, 
						UM_SOLID_LINE, area_tfmat, ref_tfmat, flag);
		}
		ifirst = 1;
	} while (mpt != mlist_ptr && mpt != UN_step_ptr);
/*
...........Display last points in buffer
*/
	if (inc > 1) 
	{
		ncl_plot_motion_plyseg(gpt, inc, drawing, cur_motattr.color, 
			cur_motattr.pen,cur_motattr.lnstyle, area_tfmat,ref_tfmat,flag);
	}
/*
......Position moving cutter
......If Moving cutter, Dynamic View, and this viewport has motion
*/
	if (emov > 0 )
	{
		ncl_cutter_post(cpt,mpt->blade,&cutseg,0,0);
/*
.....put cutter segment into drawing
*/
		ncl_plot_cutterseg(cpt, drawing, cutseg.cattr->color, cutseg.cattr->pen, 
						UM_SOLID_LINE, area_tfmat, ref_tfmat, flag);

	}
	status = UU_SUCCESS;
done:;
	Proj_motion = 0;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  ncl_put_cutterseg(projgeom)
**       Put cutter segment in the databag.
**    PARAMETERS
**       INPUT  :
**					projgeom		= segment databag.
**       OUTPUT :
**					projgeom		= segment databag created .
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_plot_motion_plyseg(gpt, num, drawing, color, pen, lntyp, area_tfmat, ref_tfmat, flag)
struct UM_drawing_rec drawing;
UM_transf area_tfmat,ref_tfmat;
int flag, pen, color, lntyp, num;
Gwpoint3 *gpt;
{
#define MXINC 100
	struct UM_attrdata_rec matt;
	UU_REAL drw_x, drw_y, dx, dy;
	UM_transf refs_tfmat;
	UU_REAL cur_ref[3], ref[3],cur_pln[3];
	UM_vector drawing_scale;
	UM_transf tfmat, fit_tfmat;
	struct UM_entitydatabag *projgeom;
	UU_LIST keylist;
	UU_KEY_ID *newkeys, mkeys;
/*
.....put motion segment into drawing
*/
	uu_list_init(&keylist, sizeof(UU_KEY_ID), 1000, 1000);
	projgeom = (struct UM_entitydatabag *) uu_toolmalloc
							(sizeof (struct UM_polyline_rec));
	ug_sntran(1);
	cur_ref[0] = 0;
	cur_ref[1] = 0;
	cur_ref[2] = 0;
	cur_pln[0] = 0;
	cur_pln[1] = 0;
	cur_pln[2] = 1;
	ncl_proj_polyline_to_plane(gpt, num, cur_ref, cur_pln,projgeom);
	ncl_create_geom_drw (projgeom, UM_DEFAULT_TF, UM_CURRENT_ATTR);
	um_get_drawing_extents(drawing.drwsize, &drw_x, &drw_y, &dx, &dy);
	if (flag!=1)
	{
		ref[0] = -drw_x/2;
		ref[1] = -drw_y/2;
		ref[2] =0;
	}
	else
	{
/*
.....if it is FIT, it should shift to left corlor, not the center
.....because it will use scale correspont to (0,0,0)
.....it will shift back drw_x/2 drw_y/2 later
*/
		ref[0] = -drw_x;
		ref[1] = -drw_y;
		ref[2] =0;
	}
	um_disptf(&ref, refs_tfmat);
	if (flag!=1)
	{
		um_tftmtf(area_tfmat, refs_tfmat, tfmat);
	}
	else
	{
		um_tftmtf(area_tfmat, refs_tfmat, tfmat);
		um_tftmtf(tfmat, ref_tfmat, tfmat);
	}
	uc_transform (projgeom, tfmat, UU_TRUE);
	if (flag==1)
	{
		drawing_scale[0] = Temp_scale;
		drawing_scale[1] = Temp_scale;
		drawing_scale[2] = Temp_scale;
		um_scaletf(drawing_scale, fit_tfmat);
		uc_transform (projgeom, fit_tfmat, UU_TRUE); 
		ref[0] = drw_x/2;
		ref[1] = drw_y/2;
		ref[2] =0;
		um_disptf(&ref, refs_tfmat);
		uc_transform (projgeom, refs_tfmat, UU_TRUE);
	}
	uc_retrieve_data (projgeom, sizeof(struct UM_entitydatabag));
	mkeys = projgeom->key;
	matt.key = projgeom->key;
	ur_retrieve_attr(&matt);
	matt.color = color;
	matt.pen = pen;
	matt.line_style = lntyp;
	ur_update_attr(&matt);
	ur_update_displayable (projgeom->key, UM_DISPLAYABLE);
	uc_display (projgeom); 
	uu_list_push (&keylist, &mkeys);
	if (!projgeom) uu_toolfree (projgeom);
	newkeys = (UU_KEY_ID *) UU_LIST_ARRAY(&keylist);
	ur_update_app_data_varlist(&drawing, 1, newkeys,
				drawing.no_member+1, keylist.cur_cnt);
	uu_list_free(&keylist);
}
/*********************************************************************
**    I_FUNCTION     :  ncl_plot_cutterseg(cpt, drawing, color, pen, lntyp,
**						area_tfmat, ref_tfmat, flag)
**       Put motion (cutter) segment in the drawing.
**    PARAMETERS
**       INPUT  :
**					drawing		= drawing that we will motion segment in.
**					incolor		= segment color for each tool part
**					inpen			= segment pen# for each tool part
**					lntyp		= segment line type
**					area_tfmat  = transformation. 
**					ref_tfmat  = transformation. 
**					flag			= 1: project motion to fit in the draw
**					cutflag:    = 1: plot cutter
**								= 0: plot motions
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_plot_cutterseg(cpt,drawing,incolor,inpen,lntyp,area_tfmat,ref_tfmat,
	flag)
UM_real8 cpt[];
struct UM_drawing_rec drawing;
UM_transf area_tfmat,ref_tfmat;
int flag, inpen[], incolor[], lntyp;
{
	int i,j,ipt,*vnpt,which,pen,color,maxpt;
	Gwpoint3 *vgpt,*pt,gpt;

	ipt = 0;
	which = 0;
	maxpt = 0;
	vnpt = (int *)UU_LIST_ARRAY(&ncl_drawing_cutstct.npt);
	vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&ncl_drawing_cutstct.gpt);
	for (i=0; i<ncl_drawing_cutstct.ngeo; i++)
	{
/*
.....Set color and pen values
*/
		if (vnpt[i] < 0)
		{
			color = incolor[which];
			pen = inpen[which];
			which++;
			i = i + 2;
			continue;
		}
/*
.....Calculate projection points for cutter
*/
		if (vnpt[i] > maxpt)
		{
			if (maxpt != 0) uu_free(pt);
			pt = (Gwpoint3 *)uu_malloc(vnpt[i]*sizeof(Gwpoint3));
		}
		for (j=0; j<vnpt[i]; j++)
		{
			um_vcplvc(cpt,&vgpt[ipt],&gpt);
			um_cctmtf(&gpt, Temp_tfmat, &pt[j]);
			ipt++;
		}
		ncl_plot_motion_plyseg(pt, vnpt[i], drawing, color, pen, 
					lntyp, area_tfmat, ref_tfmat, flag);
	}
/*
.....Free memory
*/
	if (maxpt != 0) uu_free(pt);
}
