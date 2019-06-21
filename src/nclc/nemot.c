/*********************************************************************
**    NAME         :  nemot.c
**       CONTAINS:
**          pltmot
**          ncl_save_mdisplay
**          ncl_store_mdisplay
**          ncl_display_motion
**          ncl_display_cutter_front
**          ncl_mark_mdisplay
**          ncl_erase_mdisplay
**          motdel
**          motbgn
**          motend
**          motisn
**          motisn_set
**          mottln
**          motspn
**          motcln
**          motccm
**          moters
**          ncl_erase_motion
**          filers
**          filmrk
**          ncl_get_mlist_ptrs
**          ncl_set_mlist_ptrs
**          ncl_motion_displayed
**          ncl_step_motion
**    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nemot.c , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**       11/22/17 , 10:49:28
*********************************************************************/
#include "usysdef.h"
#include <math.h>
#include "drubber.h"
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
#include "bsym.h"
#include "nclmplay.h"
#include "lcom.h"

#include "dasnog.h"
#include "ulist.h"
#include "mxxx.h"
#include "mdgenent.h"

int NCL_mot_seg = -1;
extern double UN_motion_width;
UN_motseg *mlist_ptr = (UN_motseg *)-1;
UN_motseg *mlist_ptr2 = (UN_motseg *)-1;
UN_motseg *mlist_first_ptr = (UN_motseg *)-1;
UN_motseg *mlist_first_ptr2 = (UN_motseg *)-1;
//UN_motseg *Smlist_first_ptr_sav = (UN_motseg *)-1;
//UN_motseg *Smlist_ptr_sav = (UN_motseg *)-1;
UN_motseg *NCL_save_mptr = (UN_motseg *)-1;

static UN_motseg *mlist_begin = (UN_motseg *)0;
static UN_motseg *mlist_prev[3] = {(UN_motseg *)0,(UN_motseg *)0,(UN_motseg *)0};
static UN_motseg save_motatt;
static UN_mot_vpbox_struc save_vpbox[20];

static UU_LOGICAL Ssave_mdisp_flag = UU_FALSE;
static UN_motseg *Ssave_prev2,*Ssave_mdisp,*Ssave_prev[3];
static UN_motseg_isn Sisn={0,UU_NULL};
/* UN_mot_attr have 8 members */
/*static UN_mot_attr Smattr = {0,0.,0,0.,0,0,0}; */
static UN_mot_attr Smattr = {0,0,0.,0,0.,0,0,0}; 
extern int DRAW_DISPLAY;
extern int UN_motion_color,UN_motion_line,UN_rapid_color,UN_rapid_line;
extern int moving_part;
extern int UN_motion_pen, UN_rapid_pen;

int NCL_move_save = 0;
int NCL_cutdisp = 1;

void motend();
void moters();
void ncl_mark_mdisplay();
void ncl_step_motion();

static void S_mot_add_box();
static void S_flush_motion();

int NCL_preview_mot = 0;

void ncl_set_cutdisp(int *cutdsp)
{
	NCL_cutdisp = *cutdsp;
}

/*********************************************************************
**    E_FUNCTION     : pltmot(spt,cpt,vfd)
**			FORTRAN callable routine to display a single motion move.
**			Displays the move and optionally a cutter.
**    PARAMETERS   
**       INPUT  : 
**          spt     = Point coming from.
**          cpt     = Point going to, including tool axis.
**          vfd     = Forward directional vector.
**
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void pltmot(spt,cpt,vfd)
UM_real8 cpt[],spt[],vfd[];
{
	int i,mfl,ipt;
	Gwpoint3 gpt[2],gvc;
	Glntype lintyp,linestyle;
	Gcolor lincol;
	Gscale linwid;
	UM_angle um_angle(),ang;
	UM_transf mtf;
	UU_REAL um_mag();
	UN_motseg_cutter cutseg;
	UN_motseg_attr mattr;
	UN_motseg_view mview;
	Gfloat vpn[3],vup[3],vco[3];
	int draw_cutter_only;
/*
.....Flush step mode motion display
*/
	if (UN_step_ptr != (UN_motseg *)0) ncl_step_displayed(2,0,UU_NULL);
/*
.....Store last cl position
*/
	gpt[0].x = spt[0];
	gpt[0].y = spt[1];
	gpt[0].z = spt[2];
/*
.....Store cl point and tool axis vector
*/
	gpt[1].x = cpt[0];
	gpt[1].y = cpt[1];
	gpt[1].z = cpt[2];
	gvc.x = cpt[3];
	gvc.y = cpt[4];
	gvc.z = cpt[5];
/*
.....Get active cutter
*/
	ncl_cutter_get(&cutseg,UN_MOTSEG_ACTIVE);
/*
.....Save this motion display
.....in active list
*/
	ncl_save_mdisplay(spt,cpt,vfd);
/*
.....Get motion attributes
*/
	ncl_motattr_get(&mattr,UN_MOTSEG_ACTIVE);
	ncl_motview_get(&mview,UN_MOTSEG_ACTIVE);
/*
.....Save current line attributes
*/
	zbytecp(lintyp,*gqlinetype());
	lincol = gqlinecolor();
	linwid = gqlinewidth();
/*
.....Use double buffering for moving part
*/
	draw_cutter_only = 0;
/*
.....Plot motion in each active view port
*/
start_draw:;
	for (i=0;i<mview.nview;i++)
	{
/*
........Set up active viewport
*/
		ug_sntran(mview.vinc[i]);
/*
........Set motion display attributes
*/
		linestyle.typeno = mattr.lnstyle;
		linestyle.npatn = 0;
		gslinetype(&linestyle);
		gslinecolor(mattr.color);
/*
.....add line width setting for motion draw
.....Yurong
		gslinewidth(1.0);
*/
		gslinewidth(UN_motion_width);
/*
........Display move
*/
		if (mattr.color != 0 && draw_cutter_only == 0)
			gpolyline3(2,gpt);
/*
.....Moving part
.....Calculate new viewport parameters
.....based on original view and
.....current tool axis
*/
		mfl = cutseg.cattr->mov;
		if (cutseg.cattr->mov == 2) 
		{
			mfl = 0;
			um_cross(UN_playtax[i],&cpt[3],vco);
			ang = um_mag(vco);
			if (ang > UM_FUZZ)
			{
				ang = um_angle(UN_playtax[i],&cpt[3]);
				um_rottf(vco,ang,mtf);
				um_vctmtf(UN_playvpn[i],mtf,vpn);
				um_vctmtf(UN_playvup[i],mtf,vup);
			}
			else
			{
				um_vctovc(UN_playvpn[i],vpn);
				um_vctovc(UN_playvup[i],vup);
			}
			gsvref3(mview.vinc[i],cpt);
			gsvpn3(mview.vinc[i],vpn);
			gsvup3(mview.vinc[i],vup);
/*
.....Set up transformation in order to move correctly
*/
			if (draw_cutter_only == 0)
			{
				ug_sntran(mview.vinc[i]);
				ncl_move_part(mview.vinc[i]-1);
			}			
		}
/*
.....Display cutter
*/
		ipt = mview.vinc[i] - 1;
/*
.....If we are drawing in back buffer (in this case, moving part),
.....don't draw cutter now (the cutter should never be displayed in the 
.....back buffer because we use the back buffer as the original area 
.....(for erasing the moving cutter)
*/
		if (((cutseg.cattr->mov != 2 || draw_cutter_only) &&
			(cutseg.cutsym->geo->type != 0 || cutseg.cattr->mov == 1) &&
			cutseg.cattr->mov != -1)&&(NCL_cutdisp))
		{
			ncl_cutter_post(cpt,UN_MOTSEG_ACTIVE,&cutseg,mfl,0);
/*
........If moving part, ncl_cut will draw cutter not using a list
........(open_cutseg and postn_cutseg are not called,
........so that it will not save the cutter area,
........we need do that in order to erase this cutter later
........(changed postn_cutseg to allow -1 as segment number,
........in that case it will only calculate/save area
........but not draw cutter in postn_cutseg)
*/
			if (draw_cutter_only)
				ncl_postn_cutseg(ug_gksstli.curvwindex-1,-1,cpt,UU_FALSE);
/*
........Save motion bounding box for
........extrema view
*/
			S_mot_add_box(cpt,mfl,ipt);
		}
	}
/*
.....Moving part
.....Perform double buffering
*/
	if (draw_cutter_only) goto done;
/*
.....We need drawing on both back and front
*/
	if (cutseg.cattr->mov == 2)
	{
/*
.....Now, we need draw cutter in the front buffer
*/
		draw_cutter_only = 1;
		goto start_draw;
	}
/*
.....Reset default attributes
*/
done:;
	gslinetype(&lintyp);
	gslinecolor(lincol);
	gslinewidth(linwid);
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_save_mdisplay(spt,cpt,vfd)
**			Saves the current displayed motion move in a list array.
**			This list array is used when doing a Repaint, Change View,
**			etc.
**    PARAMETERS   
**       INPUT  : 
**				spt        = Point coming from.
**				cpt        = Point going to.
**				vfd        = Forward directional vector.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_save_mdisplay(spt,cpt,vfd)
UM_real8 spt[],cpt[],vfd[];
{
	int status,mfl,mfl2,siz,iattr,iview,iblade,icutr,isn,mattr,ftyp;
	UU_REAL feed;
/*
.....List storage is broken
.....Don't save anything
*/
	status = UU_FAILURE;
	if (mlist_first_ptr == 0) goto done;
/*
.....Allocate original list storage for
.....motion display if not already done
*/
	mfl = mfl2 = 0;
	if (mlist_first_ptr == (UN_motseg *)-1)
	{
		mlist_first_ptr = (UN_motseg *)uu_lsnew();
		if (mlist_first_ptr == 0) goto done;
		mlist_ptr = mlist_first_ptr;
		UN_step_ptr = (UN_motseg *)0;
		mfl = 1;
/*
........Set up dummy segment for visibility flag
*/
		if (NCL_mot_seg==-1)
		{
			NCL_mot_seg = gnseg();
			gcreateseg(NCL_mot_seg);
			gssegdet(NCL_mot_seg,UG_UNDETECTABLE);
			gcloseseg();
		}
	}
/*
......setup motion list for preview
*/
	if ((NCL_preview_mot)&&(mlist_first_ptr2 == (UN_motseg *)-1))
	{
		mlist_first_ptr2 = mlist_ptr;
		UN_step_ptr = (UN_motseg *)0;
		mfl = 1;
	}
/*
.....Set up attribute bundle for this motion block
*/
	fdchk(&ftyp,&feed);
/*
........Rapid move
*/
	if (ftyp == 0)
	{
		if (UN_rapid_line == 0) UN_rapid_line = UM_DASHED_LINE;
		iattr = ncl_motattr_set(UN_rapid_color,UN_rapid_line,UN_rapid_pen);
	}
/*
........Feedrate move
*/
	else
	{
		if (UN_motion_line == 0) UN_motion_line = UM_SOLID_LINE;
		iattr = ncl_motattr_set(UN_motion_color,UN_motion_line,UN_motion_pen);
	}
/*
.....Get active Attribute bundles
*/
	iview = ncl_motview_set();
	icutr = ncl_cutter_get_ptr();
	iblade = ncl_motblade_set(vfd);
	isn = ncl_motisn_set(&Sisn,UU_TRUE);
	mattr = ncl_motmattr_set(&Smattr);
/*
.....Allocate memory for this motion segment
*/
	siz = sizeof(UN_motseg);
	mlist_ptr = (UN_motseg *)uu_lsinsrt((char *)mlist_ptr,siz);
	if (mlist_ptr == 0) goto done;
/*
.....First time here
.....Save point coming from
*/
	if (mfl == 1)
	{
		mlist_ptr->tend.x = spt[0];
		mlist_ptr->tend.y = spt[1];
		mlist_ptr->tend.z = spt[2];
		mlist_ptr->taxis.x = cpt[3];
		mlist_ptr->taxis.y = cpt[4];
		mlist_ptr->taxis.z = cpt[5];
		mlist_ptr->fr_mode = ftyp;
		mlist_ptr->fr_val = feed;
		mlist_ptr->attr = iattr;
		mlist_ptr->view = iview;
		mlist_ptr->blade = iblade;
		mlist_ptr->cutter = icutr;
		mlist_ptr->isn = isn;
		mlist_ptr->mattr = mattr;
		
		siz = sizeof(UN_motseg);
		mlist_ptr = (UN_motseg *)uu_lsinsrt((char *)mlist_ptr,siz);
		if (mlist_ptr == 0) goto done;
	}
/*
.....Save this motion record
*/
	mlist_ptr->tend.x = cpt[0];
	mlist_ptr->tend.y = cpt[1];
	mlist_ptr->tend.z = cpt[2];
	mlist_ptr->taxis.x = cpt[3];
	mlist_ptr->taxis.y = cpt[4];
	mlist_ptr->taxis.z = cpt[5];
	mlist_ptr->fr_mode = ftyp;
	mlist_ptr->fr_val = feed;
	mlist_ptr->attr = iattr;
	mlist_ptr->view = iview;
	mlist_ptr->blade = iblade;
	mlist_ptr->cutter = icutr;
	mlist_ptr->isn = isn;
	mlist_ptr->mattr = mattr;
/*
.....Save current attribute bundle
*/
	last_motatt = *mlist_ptr;
/*
.....End of routine
*/
	status = UU_SUCCESS;
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_store_mdisplay(mpt)
**			Saves a motion display segment in the global display list.
**    PARAMETERS   
**       INPUT  : 
**			   mpt     = Motion display structure.
**       OUTPUT : 
**   			none.
**    RETURNS      :
**          UU_SUCCESS on success, otherwise UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_store_mdisplay(mpt)
UN_motseg *mpt;
{
	int status,siz;
/*
.....List storage is broken
.....Don't save anything
*/
	status = UU_FAILURE;
	if (mlist_first_ptr == UU_NULL) goto done;
/*
.....Allocate original list storage for
.....motion display if not already done
*/
	if (mlist_first_ptr == (UN_motseg *)-1)
	{
		mlist_first_ptr = (UN_motseg *)uu_lsnew();
		if (mlist_first_ptr == 0) goto done;
		mlist_ptr = mlist_first_ptr;
/*
........Set up dummy segment for visibility flag
*/
		NCL_mot_seg = gnseg();
		gcreateseg(NCL_mot_seg);
		gssegdet(NCL_mot_seg,UG_UNDETECTABLE);
		gcloseseg();
	}
/*
.....Allocate memory for this motion segment
*/
	siz = sizeof(UN_motseg);
	mlist_ptr = (UN_motseg *)uu_lsinsrt((char *)mlist_ptr,siz);
	if (mlist_ptr == 0) goto done;
/*
.....Save this motion record
*/
	*mlist_ptr = *mpt;
/*
.....Save current attribute bundle
*/
	last_motatt = *mlist_ptr;
/*
.....End of routine
*/
	status = UU_SUCCESS;
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_display_motion(vp,strt,erase,dview,dcutter,
**                                        clipflag,cliprect)
**			Redisplays or erases the motion displayed in the specified
**			view port.  This motion redisplay uses the motion list
**			array to determine the motion currently displayed in the
**			view port.
**    PARAMETERS   
**       INPUT  : 
**			vp       = View port number to display/erase motion in.
**			strt     = Starting point in list arrary to start display
**			           from.  Usually used when erasing motion.
**			erase    = 1 = Erase motion.
**			dview    = 1 = Dynamic viewing is in effect.
**			dcutter  = UU_TRUE = Display cutter.
**			clipflag = UU_TRUE  = display motion in provided rectangle only.
**                  UU_FALSE = display all motion.
**			cliprect = NDC rectangle to display motion in.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_display_motion(vp,strt,erase,dview,dcutter,clipflag,cliprect)
int vp,erase,dview;
UN_motseg *strt;
UU_LOGICAL dcutter,clipflag;
Gnrect *cliprect;
{
#define MXINC 100
	int status,inc,vfl,ifirst,cfl,dfl,ecfl,drew_in_vp;
	int ii,ist,ien,ipt,j;
	UU_LOGICAL frontfl;
	UU_REAL um_dist_2d();
	UN_motseg *mpt,cur_motatt;
	Gwpoint3 gpt[MXINC];
	UM_real8 cpt[9];
	int emov,cmov,mov;
	Glntype lintyp,linestyle;
	Gcolor lincol;
	Gscale linwid;
	Gnrect localrect;
	UV_vport vport;
	UN_motseg_cutter cutseg;
	UN_motseg_attr motattr,cur_motattr;
	UN_motseg_view motview;
/*
.....Initialize routine
*/
	zbytecp(lintyp,*gqlinetype());
	lincol = gqlinecolor();
	linwid = gqlinewidth();
/*
.....No motion displayed
*/
	status = UU_SUCCESS;
	if ( DRAW_DISPLAY==1) goto done;
	if (mlist_first_ptr == (UN_motseg *)-1) goto done;
/*
.....Expand clipping rectangle by a bit
*/
	if (cliprect)
	{
		if (um_dist_2d(&cliprect->ll,&cliprect->ur) == 0. ||
			cliprect->ll.x > cliprect->ur.x) return(UU_SUCCESS);
		localrect.ll.x = cliprect->ll.x - .005;
		localrect.ll.y = cliprect->ll.y - .005;
		localrect.ur.x = cliprect->ur.x + .005;
		localrect.ur.y = cliprect->ur.y + .005;
	}
/*
.....Set range of viewports to display motion in
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
.....Set up default motion display attributes
*/
	status = UU_FAILURE;
	ncl_motattr_get(&cur_motattr,last_motatt.attr);
	if (erase == 1) linestyle.typeno = UM_SOLID_LINE;
	else linestyle.typeno = cur_motattr.lnstyle;
	linestyle.npatn = 0;
	gslinetype(&linestyle);
	if (erase == 1) gslinecolor(0);
	else gslinecolor(cur_motattr.color);
/*
.....add line width setting for motion draw
.....Yurong
	gslinewidth(1.0);
*/
	gslinewidth(UN_motion_width);
	cur_motatt = last_motatt;
/*
.....Get current cutter definition
*/
	ncl_cutter_get(&cutseg,cur_motatt.cutter);
	cmov = cutseg.cattr->mov; if (cmov == -1) cmov = 1;
/*
.....Loop to display motion
*/
	for (ii=ist;ii<=ien;ii++)
	{
		drew_in_vp = 0;
		frontfl = ncl_cutseg_front(ii-1) && dview == 0 && !UM_plotting;
/*
........Set this view port's transormation matrix
*/
		if (!UM_plotting)
		{
			ug_sntran(ii);
		}
		else
		{
			gsnormtran(UV_act_vports[0][ii-1].xform);
		}
		uv_getvpid(UV_act_screen[0].vports[ii-1],&vport);
/*
........Loop through motion list array
*/
		mpt = mlist_first_ptr;
		if (strt > (UN_motseg *)0) mpt = strt;
/*
.....Reset cutter segment
*/

		inc = 0;
		ifirst = 0;
		emov = 0;
		do
		{
			mpt = (UN_motseg *)uu_lsnext(mpt);
			if (mpt == 0) goto done;
			ncl_cutter_get(&cutseg,mpt->cutter);
			mov = cutseg.cattr->mov; if (mov == -1) mov = 1;
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
						S_flush_motion(inc,gpt,clipflag,&localrect);

					gpt[0].x = gpt[inc-1].x;
					gpt[0].y = gpt[inc-1].y;
					gpt[0].z = gpt[inc-1].z;
					inc = 1;
				}
				cur_motatt = *mpt;
				cur_motattr = motattr;
			}
/*
...........Save motion point
*/
			gpt[inc].x = mpt->tend.x;
			gpt[inc].y = mpt->tend.y;
			gpt[inc].z = mpt->tend.z;
/*
...........Only display motion which is
...........visible in this view
*/
			vfl = 0;
			ncl_motview_get(&motview,mpt->view);
			for (j=0;j<motview.nview;j++)
			{
				if (vport.cur_view == motview.view[j]) vfl = 1;
			}
			if (vfl == 1)
			{
/*
..............Show that we drew in Viewport
*/
				drew_in_vp = 1;

				cpt[0] = mpt->tend.x;
				cpt[1] = mpt->tend.y;
				cpt[2] = mpt->tend.z;
				cpt[3] = mpt->taxis.x;
				cpt[4] = mpt->taxis.y;
				cpt[5] = mpt->taxis.z;
				ecfl = cutseg.cutsym->geo->type;
				emov = cutseg.cattr->mov; if (emov == -1) emov = 1;
				inc++;
/*
..............Set motion display attributes
*/
				if (erase == 1) linestyle.typeno = UM_SOLID_LINE;
				else linestyle.typeno = cur_motattr.lnstyle;
				gslinetype(&linestyle);
				if (erase == 1) gslinecolor(0);
				else gslinecolor(cur_motattr.color);
/*
.................Determine if we need to
.................display the cutter
*/
				cfl = 0;
				if (cutseg.cutsym->geo->type > 0 && (erase == 0 || ifirst != 0) &&
					mov <= 0 ) cfl = 1;
/*
.................Output current motion buffer
.................if at buffer limit or if
.................cutter is being displayed
*/
				if (inc == MXINC || (cfl == 1 && inc > 1))
				{
					if (cur_motattr.color != 0)
						S_flush_motion(inc,gpt,clipflag,&localrect);
					gpt[0].x = gpt[inc-1].x;
					gpt[0].y = gpt[inc-1].y;
					gpt[0].z = gpt[inc-1].z;
					inc = 1;
				}
				last_motatt = cur_motatt;
/*
.................Display cutter
*/
				if (moving_part == 1 || !dcutter || frontfl) cfl = 0;
				if (NCL_cutdisp==0) cfl = 0;
				if (cfl == 1)
				{
					ncl_cutter_post(cpt,mpt->blade,&cutseg,0,erase);
					gslinetype(&linestyle);
					if (erase == 1) gslinecolor(0);
					else gslinecolor(cutseg.cattr->color);
/*
.....add line width setting for motion draw
.....Yurong
					gslinewidth(1.0);
*/
					gslinewidth(UN_motion_width);
/*
.................Save motion bounding box for
.................extrema view
*/
					ipt = ii - 1;
					S_mot_add_box(cpt,mov,ipt);
				}
			}
/*
..............Output current motion buffer
..............if at buffer limit or if
..............cutter is being displayed
*/
			else
			{
				if (inc > 1)
				{
					if (cur_motattr.color != 0)
						S_flush_motion(inc,gpt,clipflag,&localrect);

				}
				gpt[0].x = gpt[inc].x;
				gpt[0].y = gpt[inc].y;
				gpt[0].z = gpt[inc].z;
				inc = 1;
			}
			ifirst = 1;
		} while (mpt != mlist_ptr && mpt != UN_step_ptr);
/*
...........Display last points in buffer
*/
		if (inc > 1 && cur_motattr.color != 0)
			S_flush_motion(inc,gpt,clipflag,&localrect);
/*
...........Position moving cutter
*/
		if (moving_part == 1) emov = 0;
		if (dcutter && (emov > 0 && erase == 0 && drew_in_vp == 1) &&
			!frontfl)
		{
			dfl = 1;
			if (dview == 1 || UM_plotting) dfl = 0;
//draw cutter last? test
			ncl_cutter_post(cpt,mpt->blade,&cutseg,dfl,erase);
/*
..............Save motion bounding box for
..............extrema view
*/
			ipt = ii - 1;
			S_mot_add_box(cpt,mov,ipt);
		}
	}
/*
.....Reset line attributes
*/
	status = UU_SUCCESS;
done:;
	gslinetype(&lintyp);
	gslinecolor(lincol);
	gslinewidth(linwid);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_display_cutter_front()
**			Displays the current moving cutter if it is displayed in the
**       front buffer (fast wireframe).
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_display_cutter_front()
{
#define MXINC 100
	int i,ist,ien;
	UU_REAL cpt[6];
/*
.....No motion displayed
*/
	if (mlist_first_ptr == (UN_motseg *)-1 || DRAW_DISPLAY == 1) goto done;
/*
.....Set range of viewports to display motion in
*/
	ist = 0;
	ien = UV_act_screen[0].nvports;
/*
.....Loop to display cutters
*/
	for (i=ist;i<ien;i++)
	{
		if (!ncl_cutseg_front(i)) continue;
		{
/*
........Set this view port's transormation matrix
*/
			ug_sntran(i+1);
/*
........Display the cutter
*/
			if (NCL_move_save != 0)
			{
				cpt[0] = NCL_save_mptr->tend.x;
				cpt[1] = NCL_save_mptr->tend.y;
				cpt[2] = NCL_save_mptr->tend.z;
				cpt[3] = NCL_save_mptr->taxis.x;
				cpt[4] = NCL_save_mptr->taxis.y;
				cpt[5] = NCL_save_mptr->taxis.z;
			}
			else if (UN_step_ptr != (UN_motseg *)0)
			{
				cpt[0] = UN_step_ptr->tend.x;
				cpt[1] = UN_step_ptr->tend.y;
				cpt[2] = UN_step_ptr->tend.z;
				cpt[3] = UN_step_ptr->taxis.x;
				cpt[4] = UN_step_ptr->taxis.y;
				cpt[5] = UN_step_ptr->taxis.z;
			}
			else
			{
				cpt[0] = mlist_ptr->tend.x;
				cpt[1] = mlist_ptr->tend.y;
				cpt[2] = mlist_ptr->tend.z;
				cpt[3] = mlist_ptr->taxis.x;
				cpt[4] = mlist_ptr->taxis.y;
				cpt[5] = mlist_ptr->taxis.z;
			}
			ncl_postn_cutseg(i,i,cpt,UU_FALSE);
		}
	}
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_mark_mdisplay(mptr,motatt,vpbox,filflag)
**			Returns the pointer to the current motion display and the
**			bounding box of motion display.  Used to erase motion in the
**			routine 'ncl_erase_mdisplay'.
**    PARAMETERS   
**       INPUT  : 
**				filflag =  UU_TRUE = Save motion display if ARCSLP/FILLET
**                     is in effect.  Typically used when previewing
**                     motion.
**       OUTPUT : 
**				mptr    =  Current position of motion display.
**				motatt  =  Current motion attributes.
**				vpbox   =  Bounding box of motion display.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mark_mdisplay(mptr,motatt,vpbox,filflag)
UN_motseg **mptr,*motatt;
UN_mot_vpbox_struc vpbox[];
UU_LOGICAL filflag;
{
	int i;
	UM_int2 ifl347,ival;
	UN_motseg *mpt,*sptr;
	void ncl_mark_mdisplay2();

	if (NCL_preview_mot)
	{
		ncl_mark_mdisplay2(mptr,motatt,vpbox,filflag);
		return;
	}

/*
.....Return pointer to motion display
*/
	*mptr = mlist_ptr;
	*motatt = last_motatt;
/*
.....Return bounding box of motion
*/
	for (i=0;i<UV_act_screen[0].nvports;i++)
	{
		vpbox[i].ll[0] = mot_vpbox[i].ll[0];
		vpbox[i].ur[0] = mot_vpbox[i].ur[0];
		vpbox[i].ll[1] = mot_vpbox[i].ll[1];
		vpbox[i].ur[1] = mot_vpbox[i].ur[1];
		vpbox[i].ll[2] = mot_vpbox[i].ll[2];
		vpbox[i].ur[2] = mot_vpbox[i].ur[2];
	}
/*
.....If filleting is active
.....then save last motion display
*/
	ifl347 = 347;
	getifl(&ifl347,&ival);
	if (filflag && ival != 0 && mlist_first_ptr != (UN_motseg *)-1)
	{
		mpt = mlist_prev[2];
		if (mpt == (UN_motseg *)-1 || mpt == UU_NULL)
			mpt = (UN_motseg *)uu_lsnext(mlist_first_ptr);
		Ssave_prev2 = mpt;
		if (mpt != (UN_motseg *)-1 && mpt != UU_NULL)
		{
			Ssave_mdisp = (UN_motseg *)uu_lsnew();
			sptr = Ssave_mdisp;
			do
			{
				sptr = (UN_motseg *)uu_lsinsrt((char *)sptr,sizeof(UN_motseg));
				*sptr = *mpt;
				if (mpt == mlist_prev[0]) Ssave_prev[0] = sptr;
				if (mpt == mlist_prev[1]) Ssave_prev[1] = sptr;
				if (mpt == mlist_prev[2]) Ssave_prev[2] = sptr;
				mpt = (UN_motseg *)uu_lsnext(mpt);
			} while (mpt != UU_NULL);
			Ssave_mdisp_flag = UU_TRUE;
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_erase_mdisplay(mptr,motatt,vpbox)
**			Erases the motion display from a previously marked position
**			and restores the bounding box of motion display.
**    PARAMETERS   
**       INPUT  : 
**				mptr    =  Current position of motion display.
**				motatt  =  Saved motion attributes.
**				vpbox   =  Saved bounding box of motion display.
**       OUTPUT : 
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_erase_mdisplay(mptr,motatt,vpbox)
UN_motseg **mptr,*motatt;
UN_mot_vpbox_struc vpbox[];
{
	int i;
	UN_motseg *mpt,*mbeg,*sptr;
/*
.....Reset pointer to motion display
.....and attrbutes
*/
	if (Ssave_mdisp_flag) mlist_begin = Ssave_prev2;
	else mlist_begin = *mptr;
	save_motatt = *motatt;;
/*
.....Reset bounding box of motion
*/
	for (i=0;i<UV_act_screen[0].nvports;i++)
	{
		save_vpbox[i].ll[0] = vpbox[i].ll[0];
		save_vpbox[i].ur[0] = vpbox[i].ur[0];
		save_vpbox[i].ll[1] = vpbox[i].ll[1];
		save_vpbox[i].ur[1] = vpbox[i].ur[1];
		save_vpbox[i].ll[2] = vpbox[i].ll[2];
		save_vpbox[i].ur[2] = vpbox[i].ur[2];
	}
/*
.....Erase motion
*/
	moters();
/*
.....Redisplay saved motion that
.....was altered by filleting
*/
	if (Ssave_mdisp_flag)
	{
		mpt = mlist_ptr;
		if (mpt == (UN_motseg *)-1 || mpt == UU_NULL)
			mpt = mlist_first_ptr;
		if (mpt != (UN_motseg *)-1 && mpt != UU_NULL)
		{
			sptr = (UN_motseg *)uu_lsnext(Ssave_mdisp);
			mbeg = mpt;
			do
			{
				sptr = (UN_motseg *)uu_lsnext(sptr);
				if (sptr == UU_NULL) break;
				mpt = (UN_motseg *)uu_lsinsrt((char *)mpt,sizeof(UN_motseg));
				*mpt = *sptr;
				if (sptr == Ssave_prev[0]) mlist_prev[0] = mpt;
				if (sptr == Ssave_prev[1]) mlist_prev[1] = mpt;
				if (sptr == Ssave_prev[2]) mlist_prev[2] = mpt;
			} while (sptr != UU_NULL);
			mlist_ptr = mpt;
			ncl_display_motion(-1,mbeg,0,0,UU_TRUE,UU_FALSE,(Gnrect *)UU_NULL);
		}
		uu_lsdel(Ssave_mdisp);
		Ssave_mdisp_flag = UU_FALSE;
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : motdel()
**			Erases all motion from the screen and deletes the motion
**			list array.  Pretty much processes the *ERASE/MOTION
**			command.
**    PARAMETERS   
**       INPUT  : 
**			none.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int motdel()
{
	int i,mov;
	UV_vport vport;
	UV_view view;
	UN_motseg_cutter cutseg;
	if (DRAW_DISPLAY == 1)
		return 1;
/*
.....Motion is displayed
.....Get rid of it
*/
	if (mlist_first_ptr != (UN_motseg *)-1)
	{
/*
.....Free up displayed motion list
*/
		uu_lsdel(mlist_first_ptr);
		mlist_first_ptr = (UN_motseg *)-1;
		mlist_ptr = mlist_first_ptr;
		mlist_begin = (UN_motseg *)0;
		mlist_prev[0] = (UN_motseg *)0;
		mlist_prev[1] = (UN_motseg *)0;
		mlist_prev[2] = (UN_motseg *)0;
		UN_step_ptr = (UN_motseg *)0;
		ncl_cutter_get(&cutseg,last_motatt.cutter);
		mov = cutseg.cattr->mov; if (mov == -1) mov = 1;
/*
.....Erase motion from screen
*/
		gdeleteseg(NCL_mot_seg); 
		NCL_mot_seg = -1;
		for (i=1;i<=UV_act_screen[0].nvports;i++)
		{
			uv_getvpid(UV_act_screen[0].vports[i-1],&vport);
			uv_getvid(vport.cur_view,&view);
			view.modified = UU_TRUE;
			uv_delete_hidden(&vport);
			uv_autofact7_redrawvp(&vport,&view,UU_TRUE);
		}
/*
.....Reset extrema zoom box
*/
		for (i=0;i<UV_act_screen[0].nvports;i++)
			ncl_cutter_box_init(&mot_vpbox[i]);
	}
/*
.....Erase the motion stack display pointers
*/
	ncl_mot_stack_erase(UU_NULL);
/*
.....Delete the blade direction and
.....ISN call/loop stack lists
*/
	ncl_motblade_reset();
	if (Sisn.nent != 0) uu_free(Sisn.line);
	Sisn.nent = 0;
	ncl_motisn_reset();
/*
.....reset value
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : motbgn(cpt,gpt)
**			Marks the beginning of a motion tool path.  This routine
**			should be called at the start of a motion command (GF/...,
**			SCRUB, GT, etc.).
**    PARAMETERS   
**       INPUT  : 
**			   cpt    = Pointer to current clfile record.
**			   gpt    = From location (without TRACUT).
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void motbgn(cpt,gpt)
UN_clstruc **cpt;
UU_REAL gpt[6];
{
	int i;
	UN_clstruc *ptr;
/*
.....Make sure motend has been called
*/
	motend(cpt);
/*
.....Push motion display onto stack
*/
	ptr = *cpt;
	ncl_mot_stack_beg(mlist_ptr,ptr,gpt);
/*
.....Save motion display pointer &
.....attributes
*/
	mlist_begin = mlist_ptr;
	save_motatt = last_motatt;
/*
.....Save extrema zoom box
*/
	for (i=0;i<UV_act_screen[0].nvports;i++)
	{
		save_vpbox[i].ll[0] = mot_vpbox[i].ll[0];
		save_vpbox[i].ur[0] = mot_vpbox[i].ur[0];
		save_vpbox[i].ll[1] = mot_vpbox[i].ll[1];
		save_vpbox[i].ur[1] = mot_vpbox[i].ur[1];
		save_vpbox[i].ll[2] = mot_vpbox[i].ll[2];
		save_vpbox[i].ur[2] = mot_vpbox[i].ur[2];
	}
}

/*********************************************************************
**    E_FUNCTION     : motend(cpt)
**			Terminates a motion display tool path.  This routine
**			should be called at the end of a motion command, after all
**			tool positions have been calculated and displayed.
**    PARAMETERS   
**       INPUT  : 
**			   cpt    = Pointer to current clfile record.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void motend(cpt)
UN_clstruc **cpt;
{
	UN_clstruc *ptr;
/*
.....Push motion display onto stack
*/
	if (mlist_begin != 0)
	{
		ptr = *cpt;
		ncl_mot_stack_end(mlist_ptr,ptr);
/*
.....Save motion display pointer &
.....attributes
*/
		mlist_prev[2] = mlist_prev[1];
		mlist_prev[1] = mlist_prev[0];
		mlist_prev[0] = mlist_begin;
	}
	mlist_begin = (UN_motseg *)0;
}

/*********************************************************************
**    E_FUNCTION     : motisn(jbuf)
**			Stores the current ISN stack to be stored with the motion
**			display.
**    PARAMETERS   
**       INPUT  : 
**			   jbuf   = ISN call/loop stack.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void motisn(jbuf)
int *jbuf;
{
	int i;
/*
.....Allocate memory for isn stack
*/
	if (jbuf[0] > Sisn.nent)
	{
		if (Sisn.nent != 0) uu_free(Sisn.line);
		Sisn.line = (int *)uu_malloc(jbuf[0]*sizeof(int));
	}
/*
.....Store isn stack
*/
	Sisn.nent = jbuf[0];
	for (i=1;i<=Sisn.nent;i++)
		Sisn.line[i-1] = jbuf[i];
}

/*********************************************************************
**    E_FUNCTION     : motisn_set()
**			Stores the current ISN stack.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int motisn_set()
{
	int isn;
/*
.....Store isn stack
*/
	if (Sisn.nent > 0)
		isn = ncl_motisn_set(&Sisn,UU_TRUE);
	else
		isn = -1;
	return(isn);
}

/*********************************************************************
**    E_FUNCTION     : mottln(tlno,tlen)
**			Stores the current LOADTL to be stored with the motion
**			display.
**    PARAMETERS   
**       INPUT  : 
**			   tlno   = Current tool number.
**			   tlen   = Current tool length.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void mottln(tlno,tlen)
int *tlno;
UU_REAL *tlen;
{
/*
.....Store toolno
*/
	Smattr.loadtl = *tlno;
	Smattr.tlen = *tlen;
}

/*********************************************************************
**    E_FUNCTION     : motspn(mode,rpm)
**			Stores the current SPINDL to be stored with the motion
**			display.
**    PARAMETERS   
**       INPUT  : 
**			   mode   = Current spindle mode.
**			   rpm    = Current spindle speed.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void motspn(mode,rpm)
int *mode;
UU_REAL *rpm;
{
/*
.....Store spindle
*/
	Smattr.sp_mode = *mode;
	Smattr.sp_val = *rpm;
}

/*********************************************************************
**    E_FUNCTION     : motcln(mode)
**			Stores the current COOLNT to be stored with the motion
**			display.
**    PARAMETERS   
**       INPUT  : 
**			   mode   = Current coolant mode.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void motcln(mode)
int *mode;
{
/*
.....Store coolant
*/
	Smattr.coolnt = *mode;
}

/*********************************************************************
**    E_FUNCTION     : motccm(mode,dir)
**			Stores the current CUTCOM to be stored with the motion
**			display.
**    PARAMETERS   
**       INPUT  : 
**			   mode   = Current cutcom mode.
**			   dir    = Current cutcom direction.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void motccm(mode,dir)
int *mode,*dir;
{
/*
.....Store cutcom
*/
	Smattr.cc_mode = *mode;
	Smattr.cc_dir = *dir;
}

/*********************************************************************
**    E_FUNCTION     : moters()
**			Erases all motion generated since the last call to 
**			'motbgn'.
**    PARAMETERS   
**       INPUT  : 
**			none.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void moters()
{
	int i;
	UN_motseg *pd;
	last_motatt = save_motatt;;
/*
.....At the beginning of the list array
.....Erase all displayed motion
*/
	if (mlist_begin == (UN_motseg *)-1 || mlist_begin == mlist_first_ptr)
	{
		motdel();
	}
/*
.....Erase only the motion displayed
.....for the current tool path
.....(since motbgn() was called last)
*/
	else
	{
		if (mlist_begin != 0)
		{
			pd = (UN_motseg *)uu_lsprev(mlist_begin);
			ncl_display_motion(-1,pd,1,0,UU_TRUE,UU_FALSE,(Gnrect *)UU_NULL);
			pd = mlist_begin;
			while (pd != mlist_ptr)
			{
				pd = (UN_motseg *)uu_lsnext(mlist_begin);
				if (pd == 0) goto done;
				if (pd == mlist_prev[0]) mlist_prev[0] = mlist_begin;
				if (pd == mlist_prev[1]) mlist_prev[1] = mlist_begin;
				if (pd == mlist_prev[2]) mlist_prev[2] = mlist_begin;
				uu_lsdele(pd);
			}
/*
........Erase motion stack display pointers
*/
			ncl_mot_stack_erase(mlist_begin);
			mlist_ptr = mlist_begin;
			mlist_begin = UU_NULL;
/*
........Reset extrema zoom box
*/
			for (i=0;i<UV_act_screen[0].nvports;i++)
			{
				mot_vpbox[i].ll[0] = save_vpbox[i].ll[0];
				mot_vpbox[i].ur[0] = save_vpbox[i].ur[0];
				mot_vpbox[i].ll[1] = save_vpbox[i].ll[1];
				mot_vpbox[i].ur[1] = save_vpbox[i].ur[1];
				mot_vpbox[i].ll[2] = save_vpbox[i].ll[2];
				mot_vpbox[i].ur[2] = save_vpbox[i].ur[2];
			}
		}
	}
done:;
	NCL_save_mptr = UU_NULL;
/*	motend();*/
}

/*********************************************************************
**    E_FUNCTION     : ncl_erase_motion(kst)
**			Erases all motion generated since that pointed to by 'kst'.
**    PARAMETERS   
**       INPUT  : 
**			   kst     = Erase motion from this point.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_erase_motion(kst)
UN_motseg *kst;
{
	UN_motseg *mpt;
	mpt = mlist_begin;
	mlist_begin = kst;
	moters();
	if (mlist_ptr != (UN_motseg *)0 && mlist_ptr != (UN_motseg *)-1)
		last_motatt = *mlist_ptr;
	mlist_begin = mpt;
}

/*********************************************************************
**    E_FUNCTION     : filers(spt,flag)
**			Erases all motion modified and generated during an ARCSLP/FILLET
**			sequence.
**    PARAMETERS   
**       INPUT  : 
**			    spt    = Point coming from.
**           flag   = UU_TRUE if we need to find the motion list that
**                    contains the same location as 'spt'.  Used
**                    with commands that generate multiple motions,
**                    such as GOFWDA and RMILL.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void filers(spt,flag)
UM_real8 spt[6];
UM_int4 *flag;
{
	UU_LOGICAL found;
	UN_motseg *mpt;
	UM_coord gpt;
/*
.....Find the motion list entry
.....that contains the specified point
.....This is necessary for motion commands
.....that generate multiple moves, such as
.....GOFWDA, RMILL, etc.
*/
	mpt = mlist_prev[1];
	found = UU_TRUE;
	if (*flag)
	{
		if (mpt == (UN_motseg *)-1 || mpt == UU_NULL) 
/*
.....replaced  mpt = mlist_first_ptr;
.....since first value in the list is not used.
*/
			mpt = (UN_motseg *)uu_lsnext(mlist_first_ptr);
		if (mpt != (UN_motseg *)-1 && mpt != UU_NULL)
		{
			found = UU_FALSE;
			do
			{
				gpt[0] = mpt->tend.x; gpt[1] = mpt->tend.y; gpt[2] = mpt->tend.z;
				if (um_dcccc(gpt,spt) < UM_FUZZ)
				{
					found = UU_TRUE;
					break;
				}
				mpt = (UN_motseg *)uu_lsnext(mpt);
			} while (mpt != mlist_ptr);
		}
	}
/*
.....If the requested point was not found
.....then erase motion from beginning
.....This is necessary when an ERASE/MOTION
.....command was issued just prior to this fillet motion
.....Bobby  -  4/28/06
*/
	if (found) ncl_erase_motion(mpt);
	else ncl_erase_motion(mlist_first_ptr);
	ncl_mot_stack_delete(UU_TRUE,mlist_ptr);
}

/*********************************************************************
**    E_FUNCTION     : filmrk()
**			Marks the start of a fillet motion.  Used when ARCSLP/COMBIN
**			is in effect.
**    PARAMETERS   
**       INPUT  : 
**			none.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void filmrk()
{
	
/*	if ((int)mlist_prev[1] != -1)*/
		mlist_prev[1] = mlist_prev[2];
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_mlist_ptrs(mary)
**			Returns the 'mlist_prev' array, which is used when redisplaying
**       fillet motion (and other motion display adjustments).
**    PARAMETERS   
**       INPUT  : 
**			none.
**       OUTPUT : 
**			    mary     = Array of 3 to receive the 'mlist_prev' pointers.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_mlist_ptrs(mary)
UN_motseg *mary[3];
{
	mary[0] = mlist_prev[0];
	mary[1] = mlist_prev[1];
	mary[2] = mlist_prev[2];
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_mlist_ptrs(mary)
**			Sets the 'mlist_prev' array which should have been saved using
**       the 'ncl_get_mlist_ptrs' routine.
**    PARAMETERS   
**       INPUT  : 
**			    mary     = Array of 3 to use for the 'mlist_prev' pointers.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_mlist_ptrs(mary)
UN_motseg *mary[3];
{
	mlist_prev[0] = mary[0];
	mlist_prev[1] = mary[1];
	mlist_prev[2] = mary[2];
}

/*********************************************************************
**    E_FUNCTION     : ncl_motion_displayed()
**			Determines if motion is currently displayed and returns
**			this setting to the calling routine.
**    PARAMETERS   
**       INPUT  : 
**			none.
**       OUTPUT : 
**			none.
**    RETURNS      : UU_TRUE if motion is displayed, UU_FALSE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_motion_displayed()
{
	if (mlist_first_ptr != (UN_motseg *)-1) return(UU_TRUE);
	return(UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_step_motion(mpt,dir)
**			Displays a single move from the motion display list, either
**			forwards or backwards.  When the move is a backwards step
**			in the display list, then the move will actually be erased.
**
**			Works entirely off of the display list and increments/
**			decrements the display list pointer argument upon return.
**    PARAMETERS   
**       INPUT  : 
**				mpt   = Current motion structure within display list to
**				        plot.
**				dir   = -1 = Step backward.  1 = Step forward.
**				        -2 = Step backward to beginning of displayed motion.
**				        2 = Step forward to end of displayed motion.
**                  -3 = Step backward to the selected motion.
**       OUTPUT : 
**				mpt   = Returns next or previous motion structure within
**				        display list depending on direction.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_step_motion(mpt,dir)
UN_motseg **mpt;
int dir;
{
	int i,j,mfl,ers,vfl,lmov;
	UV_vport vport;
	UV_view view;
	Gwpoint3 gpt[2];
	Glntype lintyp,linestyle;
	Gcolor lincol,mcolor;
	Gscale linwid;
	UM_angle um_angle(),ang;
	UM_transf mtf;
	UU_REAL um_mag();
	Gfloat vpn[3],vup[3],vco[3],cpt[6];
	UN_motseg *lpt;
	UN_motseg_cutter lcutseg,mcutseg;
	UN_motseg_attr lattr;
	UN_motseg_view lview;
	void ncl_step_motion2();

	if (NCL_preview_mot)
	{
		ncl_step_motion2(mpt,dir);
		return;
	}

/*
......save motion pointer for use after we erase/display cutter
*/
/*	if (NCL_move_save==0)*/
	{
		if (dir==-1)
		{
			NCL_move_save = 4;
			NCL_save_mptr = *mpt;
		}
		else if (dir == 1)
		{
			NCL_move_save = 2;
			NCL_save_mptr = *mpt;
		}
		else if (dir == 2)
		{
			NCL_move_save = 3;
			NCL_save_mptr = *mpt;
		}
		else if (dir == -2)
		{
			NCL_move_save = 5;
			NCL_save_mptr = *mpt;
		}
		else if (dir == -3)
		{
			NCL_move_save = 6;
			NCL_save_mptr = *mpt;
			if (UN_step_ptr != (UN_motseg *)0) *mpt = UN_step_ptr;
			else *mpt = mlist_ptr;
			ncl_reset_lighting();
		}
	}
/*
.....Save line style attributes
*/
	zbytecp(lintyp,*gqlinetype());
	lincol = gqlinecolor();
	linwid = gqlinewidth();
/*
.....Stepping backwards
.....Get previous position
*/
begin:;
	if (dir < 0)
	{
		if (*mpt == (UN_motseg *)-1 || *mpt == mlist_first_ptr) goto done;
		lpt = *mpt;
		*mpt = (UN_motseg *)uu_lsprev(lpt);
		if (*mpt == (UN_motseg *)0)
		{
			*mpt = lpt;
			goto done;
		}
		if (dir == -3 && lpt == NCL_save_mptr) goto done;
		ncl_cutter_get(&lcutseg,lpt->cutter);
		if (lcutseg.cattr->mov == 0)
		{
			cpt[0] = lpt->tend.x;
			cpt[1] = lpt->tend.y;
			cpt[2] = lpt->tend.z;
			cpt[3] = lpt->taxis.x;
			cpt[4] = lpt->taxis.y;
			cpt[5] = lpt->taxis.z;
			ers = 1;
		}
		else
		{
			cpt[0] = (*mpt)->tend.x;
			cpt[1] = (*mpt)->tend.y;
			cpt[2] = (*mpt)->tend.z;
			cpt[3] = (*mpt)->taxis.x;
			cpt[4] = (*mpt)->taxis.y;
			cpt[5] = (*mpt)->taxis.z;
			ers = 0;
		}
		mcolor = 0;
	}
/*
.....Stepping forwards
.....Get next position
*/
	else if (dir > 0)
	{
		if (*mpt == mlist_first_ptr)
		{
			*mpt = (UN_motseg *)uu_lsnext(*mpt);
			if (*mpt == (UN_motseg *)0)
			{
				*mpt = mlist_first_ptr;
				goto done;
			}
		}
		lpt = (UN_motseg *)uu_lsnext(*mpt);
		if (lpt == (UN_motseg *)0)
		{
			*mpt = lpt;
			 goto done;
		}
		cpt[0] = lpt->tend.x;
		cpt[1] = lpt->tend.y;
		cpt[2] = lpt->tend.z;
		cpt[3] = lpt->taxis.x;
		cpt[4] = lpt->taxis.y;
		cpt[5] = lpt->taxis.z;
		ncl_motattr_get(&lattr,lpt->attr);
		mcolor = lattr.color;
		ers = 0;
	}
/*
.....Store last cl position
*/
	gpt[0].x = (*mpt)->tend.x;
	gpt[0].y = (*mpt)->tend.y;
	gpt[0].z = (*mpt)->tend.z;
/*
.....Store cl point and tool axis vector
*/
	gpt[1].x = lpt->tend.x;
	gpt[1].y = lpt->tend.y;
	gpt[1].z = lpt->tend.z;
/*
........Save the current cutter symbol
*/
	ncl_cutter_get(&mcutseg,(*mpt)->cutter);
	ncl_cutter_get(&lcutseg,lpt->cutter);
	lmov = lcutseg.cattr->mov; if (lmov == -1) lmov = 1;
	ncl_motattr_get(&lattr,lpt->attr);
	ncl_motview_get(&lview,lpt->view);
/*
.....Save this move's motion attributes
*/
start_draw:;
	last_motatt = *lpt;
/*
.....Plot motion in each active view port
*/
	for (i=1;i<=UV_act_screen[0].nvports;i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i-1],&vport);
		vfl = 0;
		for (j=0;j<lview.nview;j++)
		{
			if (vport.cur_view == lview.view[j]) vfl = 1;
		}
		if (vfl == 1)
		{
/*
........Set up active viewport
*/
			ug_sntran(i);
/*
........Set motion display attributes
*/
			if (dir < 0) linestyle.typeno = UM_SOLID_LINE;
			else linestyle.typeno = lattr.lnstyle;
			linestyle.npatn = 0;
			gslinetype(&linestyle);
			gslinecolor(mcolor);
/*
.....add line width setting for motion draw
.....Yurong
			gslinewidth(1.0);
*/
			gslinewidth(UN_motion_width);
/*
........Moving part
........Calculate new viewport parameters
........based on original view and
........current tool axis
*/
			mfl = lcutseg.cattr->mov; if (mfl == -1) mfl = 1;
			if (mfl == 2)
			{
				mfl = 0;
				um_cross(UN_playtax[i-1],&cpt[3],vco);
				ang = um_mag(vco);
				if (ang > UM_FUZZ)
				{
					ang = um_angle(UN_playtax[i-1],&cpt[3]);
					um_rottf(vco,ang,mtf);
					um_vctmtf(UN_playvpn[i-1],mtf,vpn);
					um_vctmtf(UN_playvup[i-1],mtf,vup);
				}
				else
				{
					um_vctovc(UN_playvpn[i-1],vpn);
					um_vctovc(UN_playvup[i-1],vup);
				}
				uv_getvid(vport.cur_view,&view);
				view.cur_pln_norm[0] = vpn[0];
				view.cur_pln_norm[1] = vpn[1];
				view.cur_pln_norm[2] = vpn[2];
				view.cur_up_vect[0] = vup[0];
				view.cur_up_vect[1] = vup[1];
				view.cur_up_vect[2] = vup[2];
				view.cur_ref_pt[0] = cpt[0];
				view.cur_ref_pt[1] = cpt[1];
				view.cur_ref_pt[2] = cpt[2];
				uv_putv(&view);
				uv_updatevp(&vport,UU_FALSE);
			}
/*
........Display move
*/
			if (lattr.color != 0)
			{
				gpolyline3(2,gpt);
			}
/*
........Display cutter
*/
			if ((lcutseg.cutsym->geo->type != 0 || lmov == 1) &&
			    (dir != 2 || lmov == 0 || lpt == mlist_ptr) &&
				 (dir != -3 || *mpt == NCL_save_mptr))
//test do we display cutter in step view? if (NCL_cutdisp==1)
			{
				if (dir < 0 && lmov == 1)
					ncl_cutter_post(cpt,(*mpt)->blade,&mcutseg,mfl,0);
				else
				{
					ncl_cutter_post(cpt,lpt->blade,&lcutseg,mfl,0);
					lmov = lcutseg.cattr->mov; if (lmov == -1) lmov = 1;
				}
			}
		}
	}
/*
.....Reset default attributes
*/
	gslinetype(&lintyp);
	gslinecolor(lincol);
	gslinewidth(linwid);
/*
.....Save this move's position
*/
	if (dir == 1 || dir == 2) *mpt = lpt;
	if (dir == 2 || dir == -3) goto begin;
/*
.....End of routine
.....Reset graphic surface
*/
done:;
/*	ncl_setsurf(plmask);*/
	return;
}

/*********************************************************************
**    I_FUNCTION     : S_mot_add_box(cpt,mfl,vp)
**			Adjusts the motion display bounding box by the current tool
**			location.
**    PARAMETERS   
**       INPUT  : 
**				cpt   = Tool end point and tool axis.
**				mfl   = 1 = Moving cutter.
**				vp    = Active viewport.
**       OUTPUT : none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_mot_add_box(cpt,mfl,vp)
UU_REAL cpt[];
int mfl,vp;
{
	int i;
	UU_REAL ang1,ang2;
	UM_coord gpt,opt;
	UM_transf mtf;
/*
.....Static cutter
*/
	if (mfl == 0)
	{
		ncl_cutter_box_add(&mot_vpbox[vp],cutdef[vp].view.box.ll);
		ncl_cutter_box_add(&mot_vpbox[vp],cutdef[vp].view.box.ur);
	}
/*
.....Moving full/shaded cutter
*/
	else if (cutdef[vp].seguse)
	{
		ncl_cutsym_tf(cpt,&cpt[3],mtf,&ang1,&ang2,0);
		um_vctovc(cutdef[vp].view.box.ll,gpt);

		for (i=0;i<2;i++)
		{
			um_cctmtf(gpt,mtf,opt);
			ncl_cutter_box_add(&mot_vpbox[vp],opt);

			gpt[0] = cutdef[vp].view.box.ur[0];
			um_cctmtf(gpt,mtf,opt);
			ncl_cutter_box_add(&mot_vpbox[vp],opt);

			gpt[1] = cutdef[vp].view.box.ur[1];
			um_cctmtf(gpt,mtf,opt);
			ncl_cutter_box_add(&mot_vpbox[vp],opt);
		
			gpt[0] = cutdef[vp].view.box.ll[0];
			um_cctmtf(gpt,mtf,opt);
			ncl_cutter_box_add(&mot_vpbox[vp],opt);
		
			gpt[1] = cutdef[vp].view.box.ll[1];
			um_cctmtf(gpt,mtf,opt);
			ncl_cutter_box_add(&mot_vpbox[vp],opt);
		
			gpt[2] = cutdef[vp].view.box.ur[2];
		}
	}
/*
.....Moving partial cutter
*/
	else
	{
		gpt[0] = cpt[0] + cutdef[vp].view.box.ll[0];
		gpt[1] = cpt[1] + cutdef[vp].view.box.ll[1];
		gpt[2] = cpt[2] + cutdef[vp].view.box.ll[2];
		opt[0] = cpt[0] + cutdef[vp].view.box.ur[0];
		opt[1] = cpt[1] + cutdef[vp].view.box.ur[1];
		opt[2] = cpt[2] + cutdef[vp].view.box.ur[2];
		ncl_cutter_box_add(&mot_vpbox[vp],gpt);
		ncl_cutter_box_add(&mot_vpbox[vp],opt);
	}
/*
.....Add motion point to box
*/
	ncl_cutter_box_add(&mot_vpbox[vp],cpt);
}

/*********************************************************************
**    I_FUNCTION     : S_flush_motion(np,gpt,clipflag,cliprect)
**			Outputs the displayed motion lines if they are within the
**			clipping rectangle.
**    PARAMETERS   
**       INPUT  : 
**			np       = Number of points in output array.
**			gpt      = Array of points to output.
**			clipflag = UU_TRUE  = display motion in provided rectangle only.
**                  UU_FALSE = display all motion.
**			cliprect = NDC rectangle to display motion in.
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_flush_motion(np,gpt,clipflag,cliprect)
int np;
Gwpoint3 gpt[];
UU_LOGICAL clipflag;
Gnrect *cliprect;
{
	int i,isp,inc;
	UU_REAL lseg[2][2],z;
	Gwpoint3 opt[MXINC+1];
/*
.....Only draw motion segments that
.....are within the rectangle
*/
	if (clipflag)
	{
		isp = 0;
		inc = 0;
		for (i=0;i<np;i++)
		{
			gwndc3(&lseg[isp][0],&lseg[isp][1],&z,gpt[i].x,gpt[i].y,
				gpt[i].z);
			if (isp == 0) isp = 1;
/*
........See if motion segment
........is within rectangle
*/
			else
			{
				if (ug_lineinrect2(lseg[0],lseg[1],cliprect))
				{
					if (inc == 0)
					{
						um_vctovc(&gpt[i-1],&opt[inc]); inc++;
					}
					um_vctovc(&gpt[i],&opt[inc]); inc++;
					if (inc >= MXINC)
					{
						gpolyline3(inc,opt);
						inc = 0;
					}
				}
/*
........Motion segment is outside of rectangle
........Output any buffered motion
*/
				else
				{
					if (inc > 1) gpolyline3(inc,opt);
					inc = 0;
				}
				lseg[0][0] = lseg[1][0];
				lseg[0][1] = lseg[1][1];
			}
		}
/*
........Output any buffered motion
*/
		if (inc > 1) gpolyline3(inc,opt);
	}
/*
.....Output all motion
*/
	else
		gpolyline3(np,gpt);
}

void ncl_reset_temp_motptr ()
{
	mlist_first_ptr2 = -1;
	if (NCL_preview_mot)
		UN_step_ptr = (UN_motseg *)0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_mark_mdisplay(mptr,motatt,vpbox,filflag)
**			Returns the pointer to the current motion display and the
**			bounding box of motion display.  Used to erase motion in the
**			routine 'ncl_erase_mdisplay'.
**    PARAMETERS   
**       INPUT  : 
**				filflag =  UU_TRUE = Save motion display if ARCSLP/FILLET
**                     is in effect.  Typically used when previewing
**                     motion.
**       OUTPUT : 
**				mptr    =  Current position of motion display.
**				motatt  =  Current motion attributes.
**				vpbox   =  Bounding box of motion display.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mark_mdisplay2(mptr,motatt,vpbox,filflag)
UN_motseg **mptr,*motatt;
UN_mot_vpbox_struc vpbox[];
UU_LOGICAL filflag;
{
	int i;
	UM_int2 ifl347,ival;
	UN_motseg *mpt,*sptr;
/*
.....Return pointer to motion display
*/
	*mptr = mlist_ptr;
	*motatt = last_motatt;;
/*
.....Return bounding box of motion
*/
	for (i=0;i<UV_act_screen[0].nvports;i++)
	{
		vpbox[i].ll[0] = mot_vpbox[i].ll[0];
		vpbox[i].ur[0] = mot_vpbox[i].ur[0];
		vpbox[i].ll[1] = mot_vpbox[i].ll[1];
		vpbox[i].ur[1] = mot_vpbox[i].ur[1];
		vpbox[i].ll[2] = mot_vpbox[i].ll[2];
		vpbox[i].ur[2] = mot_vpbox[i].ur[2];
	}
/*
.....If filleting is active
.....then save last motion display
*/
	ifl347 = 347;
	getifl(&ifl347,&ival);
	if (filflag && ival != 0 && mlist_first_ptr2 != (UN_motseg *)-1)
	{
		mpt = mlist_prev[2];
		if (mpt == (UN_motseg *)-1 || mpt == UU_NULL)
			mpt = (UN_motseg *)uu_lsnext(mlist_first_ptr2);
		Ssave_prev2 = mpt;
		if (mpt != (UN_motseg *)-1 && mpt != UU_NULL)
		{
			Ssave_mdisp = (UN_motseg *)uu_lsnew();
			sptr = Ssave_mdisp;
			do
			{
				sptr = (UN_motseg *)uu_lsinsrt((char *)sptr,sizeof(UN_motseg));
				*sptr = *mpt;
				if (mpt == mlist_prev[0]) Ssave_prev[0] = sptr;
				if (mpt == mlist_prev[1]) Ssave_prev[1] = sptr;
				if (mpt == mlist_prev[2]) Ssave_prev[2] = sptr;
				mpt = (UN_motseg *)uu_lsnext(mpt);
			} while (mpt != UU_NULL);
			Ssave_mdisp_flag = UU_TRUE;
		}
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : ncl_step_motion2(mpt,dir)
**			Displays a single move from the motion display list, either
**			forwards or backwards.  When the move is a backwards step
**			in the display list, then the move will actually be erased.
**
**			Works entirely off of the display list and increments/
**			decrements the display list pointer argument upon return.
**    PARAMETERS   
**       INPUT  : 
**				mpt   = Current motion structure within display list to
**				        plot.
**				dir   = -1 = Step backward.  1 = Step forward.
**				        -2 = Step backward to beginning of displayed motion.
**				        2 = Step forward to end of displayed motion.
**                  -3 = Step backward to the selected motion.
**       OUTPUT : 
**				mpt   = Returns next or previous motion structure within
**				        display list depending on direction.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_step_motion2(mpt,dir)
UN_motseg **mpt;
int dir;
{
	int i,j,mfl,ers,vfl,lmov;
	UV_vport vport;
	UV_view view;
	Gwpoint3 gpt[2];
	Glntype lintyp,linestyle;
	Gcolor lincol,mcolor;
	Gscale linwid;
	UM_angle um_angle(),ang;
	UM_transf mtf;
	UU_REAL um_mag();
	Gfloat vpn[3],vup[3],vco[3],cpt[6];
	UN_motseg *lpt;
	UN_motseg_cutter lcutseg,mcutseg;
	UN_motseg_attr lattr;
	UN_motseg_view lview;
/*
......save motion pointer for use after we erase/display cutter
*/
/*	if (NCL_move_save==0)*/
	{
		if (dir==-1)
		{
			NCL_move_save = 4;
			NCL_save_mptr = *mpt;
		}
		else if (dir == 1)
		{
			NCL_move_save = 2;
			NCL_save_mptr = *mpt;
		}
		else if (dir == 2)
		{
			NCL_move_save = 3;
			NCL_save_mptr = *mpt;
		}
		else if (dir == -2)
		{
			NCL_move_save = 5;
			NCL_save_mptr = *mpt;
		}
		else if (dir == -3)
		{
			NCL_move_save = 6;
			NCL_save_mptr = *mpt;
			if (UN_step_ptr != (UN_motseg *)0) *mpt = UN_step_ptr;
			else *mpt = mlist_ptr;
			ncl_reset_lighting();
		}
	}
/*
.....Save line style attributes
*/
	zbytecp(lintyp,*gqlinetype());
	lincol = gqlinecolor();
	linwid = gqlinewidth();
/*
.....Stepping backwards
.....Get previous position
*/
begin:;
	if (dir < 0)
	{
		if (*mpt == (UN_motseg *)-1 || *mpt == mlist_first_ptr2) goto done;
		lpt = *mpt;
		*mpt = (UN_motseg *)uu_lsprev(lpt);
		if (*mpt == (UN_motseg *)0)
		{
			*mpt = lpt;
			goto done;
		}
		if (dir == -3 && lpt == NCL_save_mptr) goto done;
		ncl_cutter_get(&lcutseg,lpt->cutter);
		if (lcutseg.cattr->mov == 0)
		{
			cpt[0] = lpt->tend.x;
			cpt[1] = lpt->tend.y;
			cpt[2] = lpt->tend.z;
			cpt[3] = lpt->taxis.x;
			cpt[4] = lpt->taxis.y;
			cpt[5] = lpt->taxis.z;
			ers = 1;
		}
		else
		{
			cpt[0] = (*mpt)->tend.x;
			cpt[1] = (*mpt)->tend.y;
			cpt[2] = (*mpt)->tend.z;
			cpt[3] = (*mpt)->taxis.x;
			cpt[4] = (*mpt)->taxis.y;
			cpt[5] = (*mpt)->taxis.z;
			ers = 0;
		}
		mcolor = 0;
	}
/*
.....Stepping forwards
.....Get next position
*/
	else if (dir > 0)
	{
		if (*mpt == mlist_first_ptr2)
		{
			*mpt = (UN_motseg *)uu_lsnext(*mpt);
			if (*mpt == (UN_motseg *)0)
			{
				*mpt = mlist_first_ptr2;
				goto done;
			}
		}
		lpt = (UN_motseg *)uu_lsnext(*mpt);
		if (lpt == (UN_motseg *)0)
		{
			*mpt = lpt;
			 goto done;
		}
		cpt[0] = lpt->tend.x;
		cpt[1] = lpt->tend.y;
		cpt[2] = lpt->tend.z;
		cpt[3] = lpt->taxis.x;
		cpt[4] = lpt->taxis.y;
		cpt[5] = lpt->taxis.z;
		ncl_motattr_get(&lattr,lpt->attr);
		mcolor = lattr.color;
		ers = 0;
	}
/*
.....Store last cl position
*/
	gpt[0].x = (*mpt)->tend.x;
	gpt[0].y = (*mpt)->tend.y;
	gpt[0].z = (*mpt)->tend.z;
/*
.....Store cl point and tool axis vector
*/
	gpt[1].x = lpt->tend.x;
	gpt[1].y = lpt->tend.y;
	gpt[1].z = lpt->tend.z;
/*
........Save the current cutter symbol
*/
	ncl_cutter_get(&mcutseg,(*mpt)->cutter);
	ncl_cutter_get(&lcutseg,lpt->cutter);
	lmov = lcutseg.cattr->mov; if (lmov == -1) lmov = 1;
	ncl_motattr_get(&lattr,lpt->attr);
	ncl_motview_get(&lview,lpt->view);
/*
.....Save this move's motion attributes
*/
start_draw:;
	last_motatt = *lpt;
/*
.....Plot motion in each active view port
*/
	for (i=1;i<=UV_act_screen[0].nvports;i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i-1],&vport);
		vfl = 0;
		for (j=0;j<lview.nview;j++)
		{
			if (vport.cur_view == lview.view[j]) vfl = 1;
		}
		if (vfl == 1)
		{
/*
........Set up active viewport
*/
			ug_sntran(i);
/*
........Set motion display attributes
*/
			if (dir < 0) linestyle.typeno = UM_SOLID_LINE;
			else linestyle.typeno = lattr.lnstyle;
			linestyle.npatn = 0;
			gslinetype(&linestyle);
			gslinecolor(mcolor);
/*
.....add line width setting for motion draw
.....Yurong
			gslinewidth(1.0);
*/
			gslinewidth(UN_motion_width);
/*
........Moving part
........Calculate new viewport parameters
........based on original view and
........current tool axis
*/
			mfl = lcutseg.cattr->mov; if (mfl == -1) mfl = 1;
			if (mfl == 2)
			{
				mfl = 0;
				um_cross(UN_playtax[i-1],&cpt[3],vco);
				ang = um_mag(vco);
				if (ang > UM_FUZZ)
				{
					ang = um_angle(UN_playtax[i-1],&cpt[3]);
					um_rottf(vco,ang,mtf);
					um_vctmtf(UN_playvpn[i-1],mtf,vpn);
					um_vctmtf(UN_playvup[i-1],mtf,vup);
				}
				else
				{
					um_vctovc(UN_playvpn[i-1],vpn);
					um_vctovc(UN_playvup[i-1],vup);
				}
				uv_getvid(vport.cur_view,&view);
				view.cur_pln_norm[0] = vpn[0];
				view.cur_pln_norm[1] = vpn[1];
				view.cur_pln_norm[2] = vpn[2];
				view.cur_up_vect[0] = vup[0];
				view.cur_up_vect[1] = vup[1];
				view.cur_up_vect[2] = vup[2];
				view.cur_ref_pt[0] = cpt[0];
				view.cur_ref_pt[1] = cpt[1];
				view.cur_ref_pt[2] = cpt[2];
				uv_putv(&view);
				uv_updatevp(&vport,UU_FALSE);
			}
/*
........Display move
*/
			if (lattr.color != 0)
			{
				gpolyline3(2,gpt);
			}
/*
........Display cutter
*/
			if ((lcutseg.cutsym->geo->type != 0 || lmov == 1) &&
			    (dir != 2 || lmov == 0 || lpt == mlist_ptr) &&
				 (dir != -3 || *mpt == NCL_save_mptr))
//test do we display cutter in step view? if (NCL_cutdisp==1)
			{
				if (dir < 0 && lmov == 1)
					ncl_cutter_post(cpt,(*mpt)->blade,&mcutseg,mfl,0);
				else
				{
					ncl_cutter_post(cpt,lpt->blade,&lcutseg,mfl,0);
					lmov = lcutseg.cattr->mov; if (lmov == -1) lmov = 1;
				}
			}
		}
	}
/*
.....Reset default attributes
*/
	gslinetype(&lintyp);
	gslinecolor(lincol);
	gslinewidth(linwid);
/*
.....Save this move's position
*/
	if (dir == 1 || dir == 2) *mpt = lpt;
	if (dir == 2 || dir == -3) goto begin;
/*
.....End of routine
.....Reset graphic surface
*/
done:;
/*	ncl_setsurf(plmask);*/
	return;
}

