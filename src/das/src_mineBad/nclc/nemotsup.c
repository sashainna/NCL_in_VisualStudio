/*********************************************************************
**    NAME         :  nemotsup.c
**       CONTAINS:
**			ncl_motvwp
**			ncl_motion_extrema
**			ncl_which_vprect
**			ncl_drag_cutter_on
**			ncl_drag_cutter_off
**			ncl_add_override_geo
**			ncl_remove_override_geo
**			ncl_hide_geo
**			ncl_redraw_geo
**			ncl_erase_cur_cutter
**    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nemotsup.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       07/28/15 , 11:07:47
*********************************************************************/

#include "usysdef.h"
#include "drubber.h"
#include "nclfc.h"
#include "gobas.h"
#include "gtbl.h"
#include "gsegac.h"
#include "gdidd.h"
#include "view.h"
#include "mfort.h"
#include "mdcoord.h"
#include "nclmplay.h"
#include "driver.h"
#include "ulist.h"

extern int NCL_mot_seg;

void ncl_erase_cur_cutter();
void ncl_redraw_geo();

/*********************************************************************
**    E_FUNCTION     : ncl_which_vprect(rect,vpary,nvp)
**       Determines if two rectangles intersect.  Used for determining
**       which viewports to display motion in.
**    PARAMETERS   
**       INPUT  : 
**          rect     = Graphics area rectangle in NDC coords, which
**			           is compared to the viewport rectangles for
**			           overlap.
**       OUTPUT :  
**			vpary    = Array containing which viewports overlap 'rect'.
**			nvp      = Number of viewports in 'vpary'.  Returns -1
**			           when all viewports overlap 'rect'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_which_vprect(rect,vpary,nvp)
Gnrect *rect;
int vpary[],*nvp;
{
	int i;
	Gfloat ll[2],ur[2];
/*
.....Loop to find which viewports
.....overlap the input rectangle
*/
	*nvp = 0;
	for (i=1;i<=UV_act_screen[0].nvports;i++)
	{
		ll[0] = ug_gksstli.vtran[i].vport.llf.x;
		ll[1] = ug_gksstli.vtran[i].vport.llf.y;
		ur[0] = ug_gksstli.vtran[i].vport.urb.x;
		ur[1] = ug_gksstli.vtran[i].vport.urb.y;
		if ((*rect).ll.x < ur[0] && (*rect).ll.y < ur[1] &&
			(*rect).ur.x > ll[0] && (*rect).ur.y > ll[1])
		{
			vpary[*nvp] = i;
			*nvp = *nvp + 1;
		}
	}
	if (*nvp == UV_act_screen[0].nvports) *nvp = -1;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drag_cutter_on(draginfo,segno,dragon)
**       Determines if the screen/view configuration is correct for
**       for dragging a user defined cursor.  If it is this routine
**       creates a user defined cursor using the current cutter
**       definition for dragging.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**			draginfo = Dragging information for the cutter cursor.
**			segno    = Segment number of the cutter symbol.
**          dragon   = Returns UU_TRUE when a cutter cursor has been
**			           defined and dragging is enabled.  Returns
**			           UU_FALSE otherwise.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_drag_cutter_on(draginfo,segno,dragon)
UD_RUBBER *draginfo;
UU_LOGICAL *dragon;
int *segno;
{

	UM_coord origin;
	int i,nc,vp,vpm,idc[3];
	UU_LOGICAL ido;
	UM_f77_str fsym,fssym,fhsym;
	char sym[MAXSYMLEN],ssym[MAXSYMLEN],hsym[MAXSYMLEN];
	UM_int2 ifl,ival;
	UM_int4 cfl[10],icfl;
	UM_real8 cutr[20],dcutr[20],cnv,vps,tend[6];
	UM_vector vpn,vup;
	UV_vport vport;
	UN_motseg_cutter cutseg;
	UU_KEY_ID symkey[3];
/*
.....There must only be 1 viewport on the screen
.....and it must be in the XY plane
.....for dragging to take place
*/
	*dragon = UU_FALSE;
	if (UV_act_screen[0].nvports > 1) goto done;
	if (!ud_qlocpet(2,21)) goto done;
	ncl_motvwp(&vp,vpn,vup,&vps,&vpm);
	if (uv_cpln_parallel_vpln(vp) != UU_SUCCESS) goto done;
	getend(tend);
	vup[0] = tend[3]; vup[1] = tend[4]; vup[2] = tend[5];
	if (!um_vcparall(vpn,vup)) goto done;
/*
.....Get the current cutter info
*/
	UM_init_f77_str(fsym,sym,MAXSYMLEN);
	UM_init_f77_str(fssym,ssym,MAXSYMLEN);
	UM_init_f77_str(fhsym,hsym,MAXSYMLEN);
	ifl = 264;
	getifl(&ifl,&ival);
	if (ival == 0) cnv = 1.;
	else cnv = 25.4;
	for (i=0;i<20;i++) cutr[i] = dcutr[i] = 0.;
	gdscut (cutr,UM_addr_of_f77_str(fsym),UM_addr_of_f77_str(fssym),
		UM_addr_of_f77_str(fhsym),symkey,cfl);
	nc = MAXSYMLEN;
	ul_strip_blanks(sym,&nc);
	nc = MAXSYMLEN;
	ul_strip_blanks(ssym,&nc);
	nc = MAXSYMLEN;
	ul_strip_blanks(hsym,&nc);
	ncl_cutter_get(&cutseg,UN_MOTSEG_ACTIVE);
	if (cutseg.cattr == UU_NULL) ido = UU_TRUE;
	else
	{
		for (i=0;i<6;i++)
		{
			if (fabs(cutr[i]-cutseg.cutter->parms[i]) > UM_FUZZ) ido = UU_TRUE;
		}
	}
	if (ido)
	{
		idc[0] = 0;
		if (idc[0] == 0) idc[0] = 1;
		idc[1] = cfl[4];
		idc[2] = cfl[5];
		ncl_cutter_set(cutr,idc,&cfl[1],sym,ssym,fhsym,symkey);
		ncl_cutter_get(&cutseg,UN_MOTSEG_ACTIVE);
	}
	if (cutseg.cattr == UU_NULL) goto done;
	icfl = cfl[0] + 1;
/*
.........Get cutter symbol
*/
	if (nc == 0 && cutr[0] < UM_FUZZ && cutr[2] < UM_FUZZ) goto done;
	if (cutr[0] < 0.) goto done;
/*
.....Erase the current cutter
.....Bobby  -  9/23/99
*/
/*	ncl_erase_cur_cutter(vp);*/
/*
.....Generate cutter cursor segment
*/
	uv_getvpid(UV_act_screen[0].vports[0],&vport);
	*segno = gnseg();
	gcreateseg(*segno);
	gssegdet(*segno,UG_UNDETECTABLE);
	gssegvis(*segno,UG_INVISIBLE);
	gsnormtran(vport.xform);
	ncl_cutter_post_drag(&cutseg);
	gcloseseg();
/*
.....Initiate user defined cursor dragging
*/
	origin[0] = 0; origin[1] = 0; origin[2] = 0;
	DRAG_GRAPHICS(draginfo,*segno,origin,1);
	DRAG_ON(draginfo);
	*dragon = UU_TRUE;
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drag_cutter_off(draginfo,segno)
**       Terminates the dragging of the cutter symbol cursor and
**       deletes the segment created for the symbol.
**    PARAMETERS   
**       INPUT  : 
**			draginfo = Dragging information for the cutter cursor.
**			segno    = Segment number of the cutter symbol.
**       OUTPUT :  
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_drag_cutter_off(draginfo,segno)
int segno;
UD_RUBBER *draginfo;
{
/*
.....Terminate dragging mode
*/
	DRAG_OFF(draginfo);
/*
.....Delete cutter symbol cursor
*/
	gdeleteseg(segno);
}

/*********************************************************************
**    E_FUNCTION     : ncl_add_override_geo(key)
**			Adds geometry keys to the that should not have their
**       attributes changed when overriding geometry attributes.
**    PARAMETERS   
**       INPUT  : 
**			   key    = Key of geometry to add to list.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_add_override_geo(key)
int key;
{
/*
.....Add key to override list
*/
	if (key != 0)
	{
		if (UN_override_geo_nkeys == 0)
			uu_list_init(&UN_override_geo_keys,sizeof(UU_KEY_ID),1000,1000);
		uu_list_push(&UN_override_geo_keys,&key);
		UN_override_geo_nkeys++;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_remove_override_geo(key)
**			Removes geometry keys from the list that should not have their
**       attributes changed when overriding geometry attributes.
**    PARAMETERS   
**       INPUT  : 
**			   key    = Key of geometry to remove to list.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_remove_override_geo(key)
{
	int i;
	UU_KEY_ID *keys;
/*
.....Remove key from override list
*/
	if (key != 0 && UN_override_geo_nkeys > 0)
	{
		keys = (UU_KEY_ID *)UU_LIST_ARRAY(&UN_override_geo_keys);
		for (i=0;i<UN_override_geo_nkeys;i++)
		{
			if (key == keys[i])
			{
				uu_list_delete(&UN_override_geo_keys,i,1);
				UN_override_geo_nkeys--;
				break;
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_hide_geo(mode,color,linestyle,translucency,redraw,
**                                  segids,nsegs)
**			Redraws all geometry in the motion display viewports.
**    PARAMETERS   
**       INPUT  : 
**			mode         = -1 = Reset geometry attributes, 0 = Hide (invisible)
**                      geometry, 1 = Change geometry attributes.
**			color        = Highlighted color of geometry.
**			linestyle    = Higlighted linestyle of geomtry.
**			translucency = Highlighted translucency of geometry.
**			redraw       = UU_TRUE = Redraws the window after changing the
**                      geometry attributes.  Usually only needed when the
**                      redraw was initiated by a form and otherwise would
**                      not be redrawn.
**       segids       = List of display segment numbers that should not
**                      be affected by the new attributes.
**       nsegs        = Number of segments in 'segids'.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_hide_geo(mode,color,linestyle,translucency,redraw,segids,nsegs)
int mode,color,linestyle,translucency,segids[],nsegs;
UU_LOGICAL redraw;
{
	int i;
	UV_vport vport;
	UV_view view;
/*
.....Restore geometry attributes
*/
	switch (mode)
	{
	case -1:
		UN_override_geo_attr[0] = -1;
		UN_override_geo_attr[1] = -1;
		UN_override_geo_attr[2] = -1;
		UN_override_geo_mask = 0;
		ncl_redraw_geo(-1,1,0,0,redraw);
		uu_list_free(&UN_override_geo_keys);
		UN_override_geo_nkeys = 0;
		break;
/*
.....Hide/Highlight geometry
*/
	case 0:
	case 1:
		UN_override_geo_attr[0] = mode == 0 ? -2 : color;
		UN_override_geo_attr[1] = mode == 0 ? -2 : linestyle;
		UN_override_geo_attr[2] = mode == 0 ? -2 : translucency;
		UN_override_geo_mask = 0;
		for (i=1;i<=UV_act_screen[0].nvports;i++)
		{
			uv_getvpid(UV_act_screen[0].vports[i-1],&vport);
			if (vport.motion)
			{
				UN_override_geo_mask = UN_override_geo_mask | (1 << i) |
					UG_SEGINNTRAN;
			}
		}
		ncl_redraw_geo(-1,1,0,1,redraw);
		break;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_redraw_geo(vp,anlz,motfl,movfl,redraw)
**			Redraws all geometry in the specified viewport.  Used for
**			motion playback analyzation routines and immediate mode
**			dynamic viewing.
**    PARAMETERS   
**       INPUT  : 
**			vp      = Viewport to redraw geometry in.  -1 = all view
**			          ports.
**			anlz    = 1 = Analyzation mode is turned on.  Only redraw
**			          geometry which matches analyzation mask.
**			motfl   = 1 = Redraw displayed motion.  0 = Don't.
**			movfl   = 2 = Moving part is in effect.
**			redraw  = UU_TRUE = Redraws the window after changing the
**                 geometry attributes.  Usually only needed when the
**                 redraw was initiated by a form and otherwise would
**                 not be redrawn.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_redraw_geo(vp,anlz,motfl,movfl,redraw)
int vp,anlz,motfl,movfl;
UU_LOGICAL redraw;
{
	int i,ist,ien;
	unsigned mask;
	UG_segstli *p;
	UV_vport vport;
	UV_view view;
/*
.....Set viewports to redraw
*/
	if (vp == -1)
	{
		ist = 1;
		ien = UV_act_screen[0].nvports;
	}
	else
	{
		ist = vp;
		ien = vp;
	}
/*
.....Redraw all segments in analyzation color
.....If in Moving Part playback, then the geometry
.....will be displayed later
*/
	if (movfl != 2)
	{
		for (i=ist;i<=ien;i++)
		{
			mask = (1 << i) | UG_SEGINNTRAN;
			ug_seginitscan();
			while ((p=ug_segscan()) != NULL)
			{
				if (p->segid != NCL_mot_seg && p->segatts.gvis == UG_VISIBLE &&
					(p->xforms & UN_override_geo_mask || anlz == 0) &&
					(p->xforms & mask))
				{
					ug_view_seg(p->segid);
				}
				else if (p->segid == NCL_mot_seg && motfl == 1)
				{
					ncl_display_motion(i,0,0,0,UU_TRUE,UU_FALSE,UU_NULL);
				}
			}
/*
.....This code takes too long
*/
			if (redraw)
			{
				uv_getvpid(UV_act_screen[0].vports[i-1],&vport);
				uv_getvid(vport.cur_view,&view);
				view.modified = UU_TRUE;
				uv_delete_hidden(&vport);
				uv_autofact7_redrawvp(&vport,&view,UU_TRUE);
				ud_updatews(UG_SUPPRESS);
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_erase_cur_cutter(vp)
**			Erases the moving cutter from the specified viewport.
**    PARAMETERS   
**       INPUT  : 
**			vp     = viewport to erase cutter from.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_erase_cur_cutter(vp)
int vp;
{
/*
.....Erase moving cutter
.....from specified viewport
*/
	if (cutdef[vp].mov == 1)
		ncl_erase_cutseg(vp,&cutdef[vp].view.box);
}
