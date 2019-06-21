/*********************************************************************
**    NAME         :  nemotws.c
**       CONTAINS:
**			ncl_close_cutseg
**			ncl_closeobj
**			ncl_delete_cutseg
**			ncl_deleteobj
**			ncl_display_buffer
**			ncl_draw_part
**			ncl_erase_cutseg
**			ncl_frontbuffer
**			ncl_backbuffer
**			ncl_makeobj
**			ncl_move_part
**			ncl_open_cutseg
**			ncl_postn_cutseg
**			ncl_redrawws
**       ncl_cutseg_front
**       ncl_cutseg_get_buffer
**       ncl_cutseg_set_buffer
**			ncl_reset_cutseg
**			ncl_reset_lighting
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nemotws.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       11/22/17 , 10:51:24
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "driver.h"
#include "gtbl.h"
#include "mfort.h"
#include "mdattr.h"
#include "nclfc.h"
#include "gdidd.h"
#include "ginqatt.h"
#include "go1.h"
#include "lcom.h"
#include "lumb.h"
#include "xfsys1.h"
#include "xenv1.h"
#include "nclmplay.h"
#include "view.h"
#include "wsgl.h"
#include "zsysdep.h"

int NCL_cut_erasing=UU_FALSE;

extern int NCLHOST;
extern int NCL_move_save;

static int Sobj_pt = -1;
static int Sobj_list[UV_NVPORTS] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

static UU_REAL Ssav_cutbox_ll[UV_NVPORTS][3], Ssav_cutbox_ur[UV_NVPORTS][3];
static UU_REAL Ssegpos[UV_NVPORTS][6]={
	0.,0.,0.,0.,0.,0., 0.,0.,0.,0.,0.,0.,  0.,0.,0.,0.,0.,0., 0.,0.,0.,0.,0.,0.,
	0.,0.,0.,0.,0.,0., 0.,0.,0.,0.,0.,0.,  0.,0.,0.,0.,0.,0., 0.,0.,0.,0.,0.,0.,
	0.,0.,0.,0.,0.,0., 0.,0.,0.,0.,0.,0.,  0.,0.,0.,0.,0.,0., 0.,0.,0.,0.,0.,0.,
	0.,0.,0.,0.,0.,0., 0.,0.,0.,0.,0.,0.,  0.,0.,0.,0.,0.,0., 0.,0.,0.,0.,0.,0.,
	0.,0.,0.,0.,0.,0., 0.,0.,0.,0.,0.,0.,  0.,0.,0.,0.,0.,0., 0.,0.,0.,0.,0.,0.};
static Gnrect Ssave_cutarea[UV_NVPORTS] = {
   1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1,
   1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1,
   1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1,
   1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1, 1,1,-1,-1};

void ncl_closeobj();
void ncl_deleteobj();

static void S_calc_ndcbox();
static void S_box_add();

/*********************************************************************
**    E_FUNCTION     : ncl_close_cutseg(vp)
**			Closes the graphics device segment which was opened for the
**			purpose of storing a moving cutter in.
**    PARAMETERS   
**       INPUT  : 
**          vp    = Current viewport.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_close_cutseg(vp)
int vp;
{
	ncl_closeobj();
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_closeobj()
**			Closes a graphics device segment.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_closeobj()
{
	glEndList_d();
	uw_gllighting_reset();
	uw_glset_dirty_flag(UU_TRUE);
	Sobj_pt = -1;
}

/*********************************************************************
**    E_FUNCTION     : ncl_delete_cutseg(obj)
**			Deletes a previously created graphics device segment which
**			was used as a moving cutter or part.
**    PARAMETERS   
**       INPUT  : 
**          obj    = Object number to delete.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_delete_cutseg(obj)
int obj;
{
	ncl_deleteobj(obj);
}

/*********************************************************************
**    E_FUNCTION     : ncl_deleteobj(obj)
**			Deletes a graphics device segment.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_deleteobj(obj)
{
	if (Sobj_list[obj] != 0)
		glDeleteLists_d(Sobj_list[obj],1);
	Sobj_list[obj] = 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_display_buffer()
**			Set the buffer display method and save into ncl_graphic.mod
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_display_buffer()
{
	int status, stat;
	int dbuf,ebuf,cbuf;
	int *ans[3];	/* default answers/answers for form */
	UU_LOGICAL cmdreject;
	UX_pathname fname;
	FILE *fptr;
/*
.....Load the input values into
.....local storage area
*/
	dbuf = UW_disp_buf;
	ebuf = UW_erase_method;
	cbuf = UW_cutter_method;
	ans[0] = (int *)&dbuf;
	ans[1] = (int *)&ebuf;
	ans[2] = (int *)&cbuf;
/*
.....Get the Form input
*/
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
		status = ud_form("dispbufmod.frm", ans, ans);
		if (status==-1)
			goto done;
	}	
	else
		goto done;
	UW_disp_buf = dbuf;
	UW_erase_method = ebuf;
	UW_cutter_method = cbuf;
/*
.....save the setting into ncl_graphic.mod
*/
	stat = UU_SUCCESS;
	strcpy(fname,"ncl_graphic.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
		fname, 3, &fptr);
	if (stat!=UU_SUCCESS || fptr==UU_NULL) goto done;
/*
.....Store modals
*/
	ux_fputs0("#DISPLAY_BUFFER#\n", fptr);

	if (UW_disp_buf==0)
		ux_fputs0("/BUFFER/ *Swap\n", fptr);
	else
		ux_fputs0("/BUFFER/ *Pixel\n", fptr);

	if (UW_erase_method==0)
		ux_fputs0("/ERASE/ *Redraw\n", fptr);
	else
		ux_fputs0("/ERASE/ *Erase\n", fptr);

	if (UW_cutter_method==0)
		ux_fputs0("/CUTTER/ *Fast\n", fptr);
	else
		ux_fputs0("/CUTTER/ *Smooth\n", fptr);
	ux_fclose0 (fptr);
done:
	UD_UNMARK(cmdreject);
	uu_dexit;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_draw_part(obj,ll,ur)
**			Draws the part geometry in a device segment for the
**			purpose of moving the part during motion playback.
**    PARAMETERS   
**       INPUT  : 
**          obj    = Device segment number of moving part.
**			ll     = Lower left of view port in DC.
**			ur     = Upper right of view port in DC.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_draw_part(obj,ll,ur)
int obj,ll[],ur[];
{
	int ipt;
	int clip[4], clip_sav[4];
	ipt = obj + 1;
/*
.....using scissor function to limit draw area
.....Yurong 
*/
	ug_get_clip(clip_sav);
	clip[0] = ll[0];
	clip[1] = ll[1];
	clip[2] = ur[0] - ll[0];
	clip[3] = ur[1] - ll[1];
	ug_dyndraw(ipt,ll,ur,0);
	ug_set_clip(clip_sav);
}

/*********************************************************************
**    E_FUNCTION     : ncl_frontbuffer()
**			Sets the drawing buffer to the Front buffer.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_frontbuffer()
{
	uw_gldrawbuffer(UG_FRONT_BUFFER);
}

/*********************************************************************
**    E_FUNCTION     : ncl_backbuffer()
**			Sets the drawing buffer to the Back buffer.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_backbuffer()
{
	uw_gldrawbuffer(UG_BACK_BUFFER);
}

/*********************************************************************
**    E_FUNCTION     : ncl_erase_cutseg(vp,box)
**			Erases the moving cutter symbol currently displayed in a
**			specific view port.
**    PARAMETERS   
**       INPUT  : 
**				vp     = View port to erase the cutter symbol from.
**				box    = Cutter symbol 3-D bounding box.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_erase_cutseg(vp,box)
int vp;
UN_mot_vpbox_struc *box;
{
	UU_REAL um_dist_2d();
	Gnrect rect;
/*
.....Don't erase if there is no cutter
*/
	if (um_dist_2d(&Ssave_cutarea[vp].ll,&Ssave_cutarea[vp].ur) != 0. &&
		Ssave_cutarea[vp].ll.x <= Ssave_cutarea[vp].ur.x)
	{
/*
........Define redraw rectangle
*/
			rect.ll.x = Ssave_cutarea[vp].ll.x - .005;
			rect.ll.y = Ssave_cutarea[vp].ll.y - .005;
			rect.ur.x = Ssave_cutarea[vp].ur.x + .005;
			rect.ur.y = Ssave_cutarea[vp].ur.y + .005;
/*
........Shaded cutter or
........Smooth wireframe cutter
*/
		if (cutdef[vp].shaded[0] == 1 || cutdef[vp].shaded[1] == 1 ||
			cutdef[vp].shaded[2] == 1 || UW_cutter_method == 1)
		{
/*
.....need set draw flag = true before set the update rect
.....otherwise, the drawing rect will only be cutter
.....but we need motion too, set true will merge current 
.....drawing and cutter area
*/
			ug_setredrwflag(UU_TRUE);
			ug_setredrwrect2(&rect);
//			ug_setredrwflag(UU_TRUE);
		}
/*
........Fast wireframe method
*/
		else
		{
			uw_glmark_dirty_rect(&rect);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_makeobj(obj)
**			Opens a graphics device segment for the purpose of storing
**			a moving part in.
**    PARAMETERS   
**       INPUT  : 
**          obj    = Object number to open.  Usually view port number.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_makeobj(obj)
int obj;
{
/*
.....added for OpenGL
.....Yurong
*/
	Sobj_list[obj] = glGenLists(1);
	glNewList_d(Sobj_list[obj], GL_COMPILE);
	uw_gllighting_reset();
	uw_glset_dirty_flag(UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_move_part(obj)
**			Redisplays the device segment containing the part geometry
**			during motion display with a moving part.  All xforms
**			should already be completed before calling this routine.
**    PARAMETERS   
**       INPUT  : 
**          obj    = Device segment number of moving part.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_move_part(obj)
int obj;
{
	int ll[2],ur[2],ipt;
	Gnrect rect;
	ipt = obj + 1;
	if (Sobj_list[obj] != 0)
	{
		uw_glndctodev(&ug_gksstli.vtran[ipt].vport.llf,ll);
		uw_glndctodev(&ug_gksstli.vtran[ipt].vport.urb,ur);
		uw_glrasput(ll,ur,UU_NULL,ug_segerasecolor);
		glCallList(Sobj_list[obj]);
		uw_gllighting_reset();
		rect.ll.x = ug_gksstli.vtran[ipt].vport.llf.x;
		rect.ll.y = ug_gksstli.vtran[ipt].vport.llf.y;
		rect.ur.x = ug_gksstli.vtran[ipt].vport.urb.x;
		rect.ur.y = ug_gksstli.vtran[ipt].vport.urb.y;
		uw_glmark_dirty_rect(&rect);
		ncl_display_motion(ug_gksstli.curvwindex,0,0,0,UU_FALSE,UU_FALSE,UU_NULL);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_open_cutseg(obj,box)
**			Opens a graphics device segment for the purpose of storing
**			a moving cutter in.
**    PARAMETERS   
**       INPUT  : 
**          obj    = View port number and also object number to open.
**          box    = Bounding box.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_open_cutseg(obj,box)
int obj;
UN_mot_vpbox_struc *box;
{
	int i;
/*
.....If erasing cutter
.....then don't display it
*/
	if (NCL_cut_erasing) return;
/*
.....Delete previous cutter segment
*/
/*	ncl_erase_cutseg(obj);*/
	ncl_deleteobj(obj);
/*
.....Open new cutter segment
*/
	ncl_makeobj(obj);
	Sobj_pt = obj;
/*
.....Save bounding box of cutter
*/
	for (i=0;i<3;i++)
	{
		Ssav_cutbox_ll[obj][i] = box->ll[i];
		Ssav_cutbox_ur[obj][i] = box->ur[i];
	}
/*
.....Set misc flags
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_postn_cutseg(vp,obj,pos,update)
**          Positions the graphics device segment which contains a
**          moving cutter.
**    PARAMETERS
**       INPUT  :
**          vp     = View port number to position cutter in.
**          obj    = Graphics device segment of moving cutter to
**                   position.
**          pos    = XYZ position of moving cutter.
**          update = UU_TRUE = Update workstation prior to posting cutter.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_postn_cutseg(vp,obj ,pos,update)
int vp,obj ;
UU_REAL pos[];
UU_LOGICAL update;
{
	int i;
	UU_LOGICAL mask,pushd;
	UU_REAL ang1,ang2;
	Glntype lintyp,linestyle;
	Gscale linwid;
	Gbuffer dbuf,uw_glgetbuffer();
	static int Svp = -1;
/*
.....Don't draw cutter if we are
.....currently erasing it
*/
	if (NCL_cut_erasing) return;
	pushd = UU_FALSE;
/*
.....Save cutter position
*/
	if (!ncl_cutseg_front(vp) || (!update && (Svp == -1 || vp == Svp)))
	{
		if (Svp != vp)
		{
			for (i=0;i<6;i++) Ssegpos[vp][i] = pos[i];
		}
/*
.....Calculte bounding box of cutter
.....in NDC units
*/
		S_calc_ndcbox(vp,Ssegpos[vp],&ang1,&ang2);
/*
.....Prepare for cutter display
*/
		uw_glpushx();
		pushd = UU_TRUE;
/*
.....Position the cutter
*/
		glTranslatef_d(Ssegpos[vp][0],Ssegpos[vp][1],Ssegpos[vp][2]);
		if (cutdef[vp].shaded[0] == 1 || cutdef[vp].shaded[1] == 1 ||
			cutdef[vp].shaded[2] == 1 || cutdef[vp].seguse)
		{
			glRotated_d(ang1,(GLdouble)0,(GLdouble)1,(GLdouble)0);
			glRotated_d(ang2,(GLdouble)0,(GLdouble)0,(GLdouble)1);
		}
	}
/*
.....Display the cutter
........Using fast wireframe mode
*/
	if (ncl_cutseg_front(vp))
	{
		if (update)
		{
			Svp = vp;
			for (i=0;i<6;i++) Ssegpos[vp][i] = pos[i];
			ud_updatews(UG_SUPPRESS);
			Svp = -1;
		}
		else if (Svp == -1 || vp == Svp)
		{
			
			dbuf = uw_glgetbuffer();
			ncl_frontbuffer();
			mask = ug_getdepth_mask();
			ug_setdepth_mask(UU_FALSE);
			zbytecp(lintyp,*gqlinetype());
			linwid = gqlinewidth();
			linestyle.typeno = UM_SOLID_LINE;
			linestyle.npatn = 0;
			gslinetype(&linestyle);
			gslinewidth(1.0);
			glCallList_d(Sobj_list[obj]);
			uw_gllighting_reset();
			ug_setdepth_mask(mask);
			gslinetype(&lintyp);
			gslinewidth(linwid);
			uw_gldrawbuffer(dbuf);
		}
	}
/*
........Using the standard drawing mode
*/
	else
	{
		glCallList_d(Sobj_list[obj]);
		uw_gllighting_reset();
		uw_glmark_dirty_rect(&Ssave_cutarea[vp]);
	}
	if (pushd) uw_glpopx();
}

/*********************************************************************
**    E_FUNCTION     : ncl_redrawws()
**			Redraws all viewports if a repaint is necessary.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_redrawws()
{
	ud_updatews(UG_SUPPRESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutseg_front(vp)
**			Determines if the cutter is being drawn in the front or back
**			buffer.
**    PARAMETERS   
**       INPUT  : 
**          vp     = View port to inquire cutter about.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_TRUE = Cutter is drawn in the front buffer.
**                   UU_FALSE = Cutter is drawn in the back buffer.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_cutseg_front(vp)
int vp;
{
	if (cutdef[vp].shaded[0] == 1 || cutdef[vp].shaded[1] == 1 ||
		cutdef[vp].shaded[2] == 1 || UW_cutter_method == 1 ||
		cutdef[vp].mov == 0)
		return(UU_FALSE);
	else
		return(UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutseg_get_buffer()
**			Returns the wireframe cutter buffer setting.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : 0 = Front buffer, 1 = Back buffer.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cutseg_get_buffer()
{
	return(UW_cutter_method);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutseg_set_buffer(buffer)
**			Sets the wireframe cutter to the front or back buffer.
**    PARAMETERS   
**       INPUT  : 
**          buffer   = 0 = Front buffer, 1 = Back buffer.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutseg_set_buffer(buffer)
int buffer;
{
	UW_cutter_method = buffer;
}

/*********************************************************************
**    E_FUNCTION     : ncl_reset_cutseg(vp)
**			Marks the cutter symbol as not displayed.  Used when we do
**			not have an overlay plane.
**    PARAMETERS   
**       INPUT  : 
**          vp     = View port to erase the cutter symbol from.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_reset_cutseg(vp)
int vp;
{
	Ssave_cutarea[vp].ll.x = Ssave_cutarea[vp].ll.y = 1;
	Ssave_cutarea[vp].ur.x = Ssave_cutarea[vp].ur.y = -1;
	NCL_move_save = 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_reset_lighting()
**			Forces the lighting setting out on the next call.  Used to
**       toggle between shaded and wireframe display and typically
**       used after moving shaded cutter is displayed to reset wireframe
**       mode.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_reset_lighting()
{
	uw_gllighting_reset();
}

/*********************************************************************
**    E_FUNCTION     : S_calc_ndcbox(vp,pos,ang1,ang2)
**			Calculates the NDC bounding box of the cutter.
**    PARAMETERS   
**       INPUT  : 
**          vp     = View port number of cutter.
**          pos    = XYZ position of moving cutter.
**       OUTPUT :  
**          ang1   = 1st rotation angle of cutter.
**          ang2   = 2nd rotation angle of cutter.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_calc_ndcbox(vp,pos,ang1,ang2)
int vp;
UU_REAL pos[],*ang1,*ang2;
{
	UU_REAL x,y,z,nll[2],nur[2];
	UM_transf tfr,tfr1,tfr2;
	Gnpoint3 np1;
	Gwpoint3 np2;
/*
.....Calculate box of wireframe cutter
*/
	if (cutdef[vp].shaded[0] == 0 && cutdef[vp].shaded[1] == 0 &&
		cutdef[vp].shaded[2] == 0 && !cutdef[vp].seguse)
	{
		x = pos[0] + cutdef[vp].view.box.ll[0];
		y = pos[1] + cutdef[vp].view.box.ll[1];
		z = pos[2] + cutdef[vp].view.box.ll[2];
		ug_xform(x,y,z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		nll[0] = np1.x; nll[1] = np1.y;
		nur[0] = np1.x; nur[1] = np1.y;
	
		x = pos[0] + cutdef[vp].view.box.ur[0];
		ug_xform(x,y,z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);
	
		y = pos[1] + cutdef[vp].view.box.ur[1];
		ug_xform(x,y,z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);
	
		x = pos[0] + cutdef[vp].view.box.ll[0];
		ug_xform(x,y,z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);
	
		z = pos[2] + cutdef[vp].view.box.ur[2];
		ug_xform(x,y,z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);
	
		y = pos[1] + cutdef[vp].view.box.ll[1];
		ug_xform(x,y,z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);
	
		x = pos[0] + cutdef[vp].view.box.ur[0];
		ug_xform(x,y,z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);
	
		y = pos[1] + cutdef[vp].view.box.ur[1];
		ug_xform(x,y,z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);

		*ang1 = *ang2 = 0.;
	}
/*
.....Full or shaded cutter
.....take into account the rotation to
.....create the bounding box
*/
	else
	{
		um_disptf(pos,tfr1);
		ncl_cutsym_tf(pos,&pos[3],tfr2,ang1,ang2,0);
		um_tftmtf(tfr1,tfr2,tfr);
/*
........Calculate NDC area from
........lower left and upper right (World Coordinates)
........of will display cutter
*/
		np2.x = Ssav_cutbox_ll[vp][0];
		np2.y = Ssav_cutbox_ll[vp][1];
		np2.z = Ssav_cutbox_ll[vp][2];
		um_cctmtf(&np2, tfr, &np2);
		ug_xform(np2.x,np2.y,np2.z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		nll[0] = np1.x; nll[1] = np1.y;
		nur[0] = np1.x; nur[1] = np1.y;

		np2.x = Ssav_cutbox_ur[vp][0];
		np2.y = Ssav_cutbox_ll[vp][1];
		np2.z = Ssav_cutbox_ll[vp][2];
		um_cctmtf(&np2, tfr, &np2);
		ug_xform(np2.x,np2.y,np2.z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);

		np2.x = Ssav_cutbox_ur[vp][0];
		np2.y = Ssav_cutbox_ur[vp][1];
		np2.z = Ssav_cutbox_ll[vp][2];
		um_cctmtf(&np2, tfr, &np2);
		ug_xform(np2.x,np2.y,np2.z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);

		np2.x = Ssav_cutbox_ll[vp][0];
		np2.y = Ssav_cutbox_ur[vp][1];
		np2.z = Ssav_cutbox_ll[vp][2];
		um_cctmtf(&np2, tfr, &np2);
		ug_xform(np2.x,np2.y,np2.z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);

		np2.x = Ssav_cutbox_ll[vp][0];
		np2.y = Ssav_cutbox_ur[vp][1];
		np2.z = Ssav_cutbox_ur[vp][2];
		um_cctmtf(&np2, tfr, &np2);
		ug_xform(np2.x,np2.y,np2.z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);

		np2.x = Ssav_cutbox_ll[vp][0];
		np2.y = Ssav_cutbox_ll[vp][1];
		np2.z = Ssav_cutbox_ur[vp][2];
		um_cctmtf(&np2, tfr, &np2);
		ug_xform(np2.x,np2.y,np2.z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);

		np2.x = Ssav_cutbox_ur[vp][0];
		np2.y = Ssav_cutbox_ll[vp][1];
		np2.z = Ssav_cutbox_ur[vp][2];
		um_cctmtf(&np2, tfr, &np2);
		ug_xform(np2.x,np2.y,np2.z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);

		np2.x = Ssav_cutbox_ur[vp][0];
		np2.y = Ssav_cutbox_ur[vp][1];
		np2.z = Ssav_cutbox_ur[vp][2];
		um_cctmtf(&np2, tfr, &np2);
		ug_xform(np2.x,np2.y,np2.z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		S_box_add(nll,nur,&np1);
	}
/*
.....Store NDC bounding box
*/
	Ssave_cutarea[vp].ll.x = nll[0];
	Ssave_cutarea[vp].ll.y = nll[1];
	Ssave_cutarea[vp].ur.x = nur[0];
	Ssave_cutarea[vp].ur.y = nur[1];
}

/*********************************************************************
**    I_FUNCTION     : S_box_add(ll,ur,pt)
**       Expands a 2-D box by a single point.
**    PARAMETERS
**       INPUT  :
**          ll     = Lower left of box to expand.
**          ur     = Upper right of box to expand.
**          pt     = Point used to expand bounding box.
**       OUTPUT :
**          ll     = Updated Lower left of box.
**          ur     = Updated Upper right of box.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_box_add(ll,ur,pt)
UU_REAL ll[],ur[],pt[];
{
	if (pt[0] < ll[0]) ll[0] = pt[0];
	if (pt[1] < ll[1]) ll[1] = pt[1];
	if (pt[0] > ur[0]) ur[0] = pt[0];
	if (pt[1] > ur[1]) ur[1] = pt[1];
}
