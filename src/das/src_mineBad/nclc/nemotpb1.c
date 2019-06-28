/*********************************************************************
**    NAME         :  nemotpb1.c
**       CONTAINS:
**				ncl_pb_circul
**				ncl_pb_step
**				ncl_pb_delay
**				ncl_pb_acolor
**				ncl_clip_motion
**				ncl_step_displayed
**				ncl_motion_undraw
**				ncl_play_initscan
**				ncl_play_resetscan
**				ncl_count_clrec
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nemotpb1.c , 25.6
**    DATE AND TIME OF LAST  MODIFICATION
**       11/04/16 , 10:38:58
*********************************************************************/

#include "usysdef.h"
#include "dmark.h"
#include "mgeom.h"
#include "lcom.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclfile.h"
#include "uhep.h"

#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"

#include "dselect.h"

extern int UN_playback_speed;
extern UN_motseg *mlist_ptr,*mlist_first_ptr, *mlist_first_ptr2;
extern int UN_motion_color,UN_motion_line,UN_rapid_color,UN_rapid_line;
extern int NCL_preview_mot;

void ncl_pb_delay();
void ncl_motion_undraw();
extern int NCL_cmdmod;
/*********************************************************************
**    E_FUNCTION     : ncl_pb_circul(krec,iclw,rclw,mblock,cnv,trafl,tracut,npt,
**                                   cirrec)
**			Determines if a circular record is planar and sets up the
**			circle structure if it is.
**    PARAMETERS   
**       INPUT  : 
**				krec        = Current clfile record to read.
**				iclw        = Clfile integer parameters of the circle record.
**				rclw        = Clfile real parameters of the circle record.
**				mblock.spt  = Previous tool position and vector.
**				cnv         = Units conversion factor.
**				trafl       = 1 = Apply TRACUT.
**				tracut      = Tracut matrix.
**				npt         = 3 = Multax/Off, 6 = Multax/On.
**       OUTPUT :  
**          mblock.ept  = Final tool position and vector on circle.
**          mblock.axis = Final machine axes position on circle.
**				cirrec      = Circular record if moves are within tolerance and
**				              are planar.
**    RETURNS      : 0 if invalid circular record, 1 if horizontal circle,
**                   2 if vertical circle.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pb_circul(krec,iclw,rclw,mblock,cnv,trafl,tracut,npt,cirrec)
UN_clstruc **krec;
UM_int4 iclw[];
UM_real8 rclw[];
UN_motion_block *mblock;
UU_REAL cirrec[],cnv,tracut[];
int trafl,npt;
{
	int iret,i,j,nptt,icnt,colr[5],np,ist;
	UM_int2 jerr,itra;
	UM_int4 icl[6];
	UU_LOGICAL first;
	UU_REAL dis,ang,opt[5][6],tpt[3],tol1,tol2;
	UM_real8 rcl[420];
	UM_coord ptx;
	UM_vector svec,tvec;
	UM_plane plane;
	UN_clstruc *irecsv,*irec;
/*
.....Initialize routine
*/
	first = UU_TRUE;
	iret = 1;
	irec = *krec;
	icnt = 0;
	tol1 = .001 * cnv;
	tol2 = tol1 * 2.;
/*
.....Remove tracut from circle record
*/
	if (trafl == 0)
	{
		itra = 3; conent(rclw,tracut,&itra);
		itra = 4; conent(&rclw[3],tracut,&itra);
	}
/*
.....Initialize circle parameters
*/
	um_vctovc(rclw,cirrec);
	um_vctovc(&rclw[3],&cirrec[3]);
	um_vctmsc(mblock->spt,cnv,tpt);
/*
.....Initialize plane
*/
	um_vctovc(tpt,plane.p0);
	um_vctovc(&cirrec[3],plane.n);
	if (um_vcparall(plane.n,&mblock->spt[3]))
	{
		um_vctovc(&mblock->spt[3],&cirrec[3]);
		um_vctovc(&mblock->spt[3],plane.n);
	}
	else
	{
		if (!um_vcperp(plane.n,&mblock->spt[3])) goto failed;
		um_vcortho(&mblock->spt[3],plane.n); um_vctovc(plane.n,&cirrec[3]);
		iret = 2;
	}
	um_proj_pt_on_plane(1,cirrec,&plane,cirrec);
/*
.....Calculate circular radius
*/
	cirrec[6] = um_dcccc(cirrec,tpt);
	cirrec[7] = 0.;
	um_vcmnvc(tpt,cirrec,svec);
/*
.....Read next clfile record
*/
	irecsv = irec;
	do
	{
		clread(&UN_clfile,&irec,icl,rcl,&jerr);
		if (icl[2] == 1000)
		{
			icnt++;
			continue;
		}
		if (jerr == 1) break;
		if (icl[2] == 1000) continue;
		if (icl[2] != 5000 && icl[2] != 5001 && icl[2] != 5200) break;
		if ((!first && icl[3] != 6) && (icl[2] != 5001 || icl[3] != 7)) break;
		first = UU_FALSE;
		icnt++;
		nptt = npt;
		ist = 0;
		if (icl[2] == 5200) nptt = 21;
		if (icl[2] == 5001)
		{
			nptt = icl[4];
			ist = 2;
		}
/*
.....Determine if all points in record lie on circle
*/
		for (i=ist;i<icl[4];i=i+nptt)
		{
			if (trafl == 0)
			{
				itra = 3; conent(&rcl[i],tracut,&itra);
				if (npt > 3)
				{
					itra = 4; conent(&rcl[i+3],tracut,&itra);
				}
			}
			um_proj_pt_on_plane(1,&rcl[i],&plane,ptx);
			dis = um_dcccc(&rcl[i],ptx);
			if (dis > tol1) goto failed;
			dis = fabs(um_dcccc(&rcl[i],cirrec)-cirrec[6]);
			if (dis > tol2) goto failed;
/*
........Make sure motion is not clipped
*/
			if (UN_clip_enable == 1)
			{
				if (!ncl_clip_motion(tpt,&rcl[i],opt,colr,&np)) goto failed;
				um_vctovc(&rcl[i],tpt);
			}
/*
........Get angular position
*/
			um_vcmnvc(&rcl[i],cirrec,tvec);
			ang = um_angle2p(svec,tvec,&cirrec[3]) * UM_RADIAN;
			if (ang > 180.) ang = ang - 360.;
			cirrec[7] = cirrec[7] + ang;
			um_vctovc(tvec,svec);
/*
.........Push axes onto stack if machine simulation
*/
			if (LW_mach_simul)
			{
				for (j=i+6;j<i+12;j++) rcl[j] = rcl[j] / cnv;
				ul_ipv_push_axis(&rcl[i+6]);
				for (j=0;j<10;j++) mblock->axis[j] = rcl[i+6+j];
			}
			irecsv = irec;
		}
	} while (irec != UU_NULL && irec != UN_clfile_end);
/*
.....Successful circular record was built
*/
	if (cirrec[7] != 0.)
	{
		*krec = irecsv;
		LW_progress_count = LW_progress_count + icnt;
		for (i=0;i<3;i++)
		{
			mblock->ept[i] = ptx[i] / cnv;
			mblock->ept[i+3] = mblock->spt[i+3];
			cirrec[i] = cirrec[i] / cnv;
		}
		cirrec[6] = cirrec[6] / cnv;
		goto done;
	}
/*
.....Could not calculate circle
*/
failed:;
	iret = 0;
	if (LW_mach_simul) ul_ipv_clear_axis();
/*
.....End of routine
*/
done:;
	return(iret);
}

/*********************************************************************
**    E_FUNCTION     : ncl_pb_step(pmod,nstep,vis,scan)
**			Prompts the user to step through motion playback if Step is
**			active and/or delays the motion playback if the speed is less
**			than 100%.
**    PARAMETERS   
**       INPUT  : 
**				pmod    = [1] = Playback speed, [2] = Step mode
**				          [3] = Maximum number of moves per step.
**				nstep   = Current step number.
**				vis     = 1 = Move is visible in viewport.
**				scan    = Clfile scan flag.
**       OUTPUT :  
**				nstep   = Updated step number.
**    RETURNS      : UU_FALSE if user interrupted step mode.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_pb_step(pmod,nstep,vis,scan)
int pmod[],*nstep,vis,scan;
{
	int iret,numint, savmod, markval;
	char buf[256];
	char *lpr="Hit Enter key to step through motion playback";
/*
.....Initialize routine
*/
	iret = UU_TRUE;

	UD_MARK(markval,UU_FALSE);
	if (markval != 0) goto done;

	savmod = NCL_cmdmod;
	NCL_cmdmod = 1;
/*
.....Step mode
.....Prompt the user
*/
	if (pmod[2] == 1 && scan == 0 && vis == 1)
	{
		*nstep = *nstep + 1;
		if (*nstep >= pmod[3])
		{
			ud_das(UD_DASSTRING,lpr,buf,256,&numint);
			*nstep = 0;
			if (buf[0] == 'q' || buf[0] == 'Q') iret = UU_FALSE;
		}
		else
		{
			ncl_pb_delay(UN_playback_speed,vis);
		}
	}
/*
........Single step mode
*/
	else if (pmod[2] == -1 && scan == 0 && vis == 1)
		iret = UU_FALSE;
/*
........Go mode
........Delay if necessary
*/
	else if (scan == 0)
	{
		ncl_pb_delay(UN_playback_speed,1);
	}
/*
.....End of routine
*/
done:;
	NCL_cmdmod = savmod;
	UD_UNMARK(markval);
	return(iret);
}

/*********************************************************************
**    E_FUNCTION     : ncl_pb_delay(spd,vis)
**			Delays if the playback speed is less than 99% and the move is
**			visible.
**    PARAMETERS   
**       INPUT  : 
**				spd     = Playback speed.
**				vis     = 1 = Move is visible in viewport.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pb_delay(spd,vis)
int spd,vis;
{
	int itim;
/*
.....Delay if speed is slowed down and move is visible
*/
	if (spd < 99 && vis == 1)
	{
		itim = 100 / (350 / (99-spd));
		itim = 99 - spd;
		ud_updatews(UG_SUPPRESS);
		uu_delay_milli(itim);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_pb_acolor(analy,irap,feed,cattr,type)
**			Sets the motion color based on the type of active motion analyzation.
**    PARAMETERS   
**       INPUT  : 
**				analy   = 1 = Feed rate analyzation, 2 = Interpolation analyzation.
**				irap    = 1 = Rapid is active.
**				feed    = Current feed rate.
**				cattr   = Active cutter attributes.
**				type    = Active motion interpolation type.
**					0 = Linear move
**					1 = Circular move
**					2 = Rapid move
**					3 = Cycle move
**					4 = Tool axis change move
**					100+ = Stringer Head 2,3,4 move
**       OUTPUT :
**				cattr.color   = Updated cutter color.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pb_acolor(analy,irap,feed,cattr,type)
int analy;
int irap;
UU_REAL feed;
UN_motseg_cutattr *cattr;
int type;
{
	int i,icolr;
/*
.....Feedrate Analyzation
.....Set proper color for current speed
*/
	if (analy == 1)
	{
		for (i=0;i<10;i++)
		{
			if (feed <= UN_anlz_feed[i]) break;
		}
		if (i >= 10) i = 9;
		if (UN_anlz_fcolor[i] == -1)
		{
			UN_motion_color = UN_motsav_color;
			UN_rapid_color = UN_rapsav_color;
			cattr->color[0] = UN_cutsav_color;
		}
		else
		{
			UN_motion_color = UN_anlz_fcolor[i];
			UN_rapid_color = UN_rapsav_color;
			cattr->color[0] = UN_anlz_fcolor[i];
		}
		if (irap == 1) cattr->color[0] = UN_rapid_color;
	}
/*
.....Interpolation Analyzation
.....Set proper color and line type
*/
	else if (analy == 2)
	{
/*
........Determine interpolation mode
*/
		icolr = type;
		if (icolr >= 100) icolr = icolr - 100;
/*
........Set color scheme
*/
		if (UN_anlz_icolor[icolr] == -1)
		{
			if (icolr == 2)
			{
				UN_motion_color = UN_rapsav_color;
				UN_rapid_color = UN_rapsav_color;
				cattr->color[0] = UN_rapsav_color;
			}
			else
			{
				UN_motion_color = UN_motsav_color;
				UN_rapid_color = UN_motsav_color;
				cattr->color[0] = UN_motsav_color;
			}
		}
		else
		{
			UN_motion_color = UN_anlz_icolor[icolr];
			UN_rapid_color = UN_anlz_icolor[icolr];
			cattr->color[0] = UN_anlz_icolor[icolr];
		}
/*
........Set line style
*/
		if (UN_anlz_istyle[icolr] == 0)
		{
			if (icolr == 2)
			{
				UN_motion_line = UN_rapsav_line;
				UN_rapid_line = UN_rapsav_line;
			}
			else
			{
				UN_motion_line = UN_motsav_line;
				UN_rapid_line = UN_motsav_line;
			}
		}
		else
		{
			UN_motion_line = UN_anlz_istyle[icolr];
			UN_rapid_line = UN_anlz_istyle[icolr];
		}
	}
/*
.....Set the analyzation color
*/
	if (LW_active) LW_cut_material = cattr->color[0];
	else ncl_cutter_set_attr(cattr);
}

/*********************************************************************
**    E_FUNCTION     : ncl_clip_motion(spt,ept,pto,colr,npt)
**			Clips the current motion line to a maximum of five user
**			defined planes.
**    PARAMETERS   
**       INPUT  : 
**			spti    = Point coming from.
**
**			epti    = Point going to.
**
**       OUTPUT :  
**			pto     = Array of output points, including intersection
**			          points of motion line and clipping planes.
**
**			colr    = Array of color flags for each output point.
**			          -1 = This is a positioning (pen up) move.
**			          1  = This move should be plotted using the current
**			               user defined colors.
**
**			npt     = Number of output points in 'pto'.
**
**    RETURNS      :
**			UU_TRUE if entire move is within the clipping boundaries.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_clip_motion(spti,epti,pto,colr,npt)
UU_REAL spti[6],epti[6],pto[5][6];
int *npt,colr[5];
{
	UU_LOGICAL ept_is_in,both_are_in,both_in_one[5];
	int iside[5],inc,i;
	int is1,isw;
	UU_REAL d1[5],d2[5],tl,tv[3],pti[5][3],rat,vdis,svc[3],spt[6],ept[6];
/*
.....Initialize routine
*/
	ept_is_in = UU_TRUE;
	both_are_in = UU_TRUE;
	ncl_wcstomcs(0,spti,spt);
	ncl_wcstomcs(0,epti,ept);
	UM_cc_inttoext(spt,spt);
	UM_cc_inttoext(ept,ept);
	um_vctovc(&spti[3],&spt[3]);
	um_vctovc(&epti[3],&ept[3]);
	*npt = 0;
/*
.....Loop through clipping planes
*/
	for (inc=0;inc<UN_clip_npl;inc++)
	{
		both_in_one[inc] = UU_FALSE;
/*
........Determine side of plane to keep
*/
		iside[inc] = 1;
		if (UN_clip_side[inc] == 0 && UN_clip_plane[inc][0] > 0.)
			iside[inc] = 0;
		else if (UN_clip_side[inc] == 1 && UN_clip_plane[inc][0] < 0.)
			iside[inc] = 0;
		else if (UN_clip_side[inc] == 2 && UN_clip_plane[inc][1] > 0.)
			iside[inc] = 0;
		else if (UN_clip_side[inc] == 3 && UN_clip_plane[inc][1] < 0.)
			iside[inc] = 0;
		else if (UN_clip_side[inc] == 4 && UN_clip_plane[inc][2] > 0.)
			iside[inc] = 0;
		else if (UN_clip_side[inc] == 5 && UN_clip_plane[inc][2] < 0.)
			iside[inc] = 0;
/*
........Determine which side of plane
........points are on
*/
		d1[inc] = (spt[0]*UN_clip_plane[inc][0] +
		           spt[1]*UN_clip_plane[inc][1] +
		           spt[2]*UN_clip_plane[inc][2]) - UN_clip_plane[inc][3];
		d2[inc] = (ept[0]*UN_clip_plane[inc][0] +
		           ept[1]*UN_clip_plane[inc][1] +
		           ept[2]*UN_clip_plane[inc][2]) - UN_clip_plane[inc][3];
/*
...........Check plane side condition
*/
		if ((iside[inc] == 0 && d1[inc] <= 0. && d2[inc] <= 0.) ||
		    (iside[inc] == 1 && d1[inc] >= 0. && d2[inc] >= 0.))
		{
			both_in_one[inc] = UU_TRUE;
		}
		else if ((iside[inc] == 0 && d1[inc] <= 0.) ||
		         (iside[inc] == 1 && d1[inc] >= 0.))
		{
			ept_is_in = UU_FALSE;
			both_are_in = UU_FALSE;
		}
		else if ((iside[inc] == 0 && d2[inc] <= 0.) ||
		         (iside[inc] == 1 && d2[inc] >= 0.))
		{
			both_are_in = UU_FALSE;
		}
		else
		{
			both_are_in = UU_FALSE;
			goto none_in;
		}
	}
/*
.....Both sets of points are
.....within clipping planes
*/
	if (both_are_in) goto none_in;
/*
.....Get plane intersection points
*/
	*npt = 0;
	for (inc=0;inc<UN_clip_npl;inc++)
	{
		if (!both_in_one[inc])
		{
			um_vcmnvc(ept,spt,tv);
			um_unitvc(tv,tv);
			tl = d1[inc] / (tv[0]*UN_clip_plane[inc][0] +
			                tv[1]*UN_clip_plane[inc][1] +
			                tv[2]*UN_clip_plane[inc][2]);
			pti[*npt][0] = spt[0] - tl*tv[0];
			pti[*npt][1] = spt[1] - tl*tv[1];
			pti[*npt][2] = spt[2] - tl*tv[2];
			*npt = *npt + 1;
		}
	}
/*
.....Loop through intersection points
.....to determine which ones to keep
*/
	is1 = 0;
	for (inc=0;inc<*npt;inc++)
	{
/*
........Determine which side of planes
........intersect points are on
*/
		for (i=0;i<UN_clip_npl;i++)
		{
			d1[i] = (pti[inc][0]*UN_clip_plane[i][0] +
			         pti[inc][1]*UN_clip_plane[i][1] +
			         pti[inc][2]*UN_clip_plane[i][2]) - UN_clip_plane[i][3];
			if (iside[i] == 0 && d1[i] > UM_FUZZ) goto drop_point;
			else if (iside[i] == 1 && d1[i] < -UM_FUZZ) goto drop_point;
		}
/*
........Save this intersection point
*/
		pto[is1][0] = pti[inc][0];
		pto[is1][1] = pti[inc][1];
		pto[is1][2] = pti[inc][2];
		is1++;
drop_point:;
	}
/*
.....Save point going to
*/
	for (i=0;i<3;i++) pto[is1][i] = ept[i];
	is1++;
/*
.....Purge points and put them in order
*/
	do
	{
		isw = 0;
		*npt = 0;
		for (i=0;i<is1;i++) d1[i] = um_dcccc(pto[i],spt);
		for (inc=0;inc<is1-1;inc++)
		{
/*
........Points are in correct order
*/
			if (d1[inc] <= d1[inc+1])
			{
				*npt = *npt + 1;
			}
/*
........Points are the same
........Purge one of them
*/
/*
			else if (d1[inc] == d1[inc+1])
			{
				for (i=inc+1;i<is1-1;i++)
				{
					for (j=0;j<3;j++) pto[i][j] = pto[i+1][j];
				}
				is1--;
				isw = 1;
				break;
			}
*/
/*
........Reverse the points
*/
			else
			{
				isw = 1;
				*npt = *npt + 1;
				for (i=0;i<3;i++)
				{
					pti[0][i] = pto[inc+1][i];
					pto[inc+1][i] = pto[inc][i];
					pto[inc][i] = pti[0][i];
				}
			}
		}
		*npt = *npt + 1;
	} while (isw == 1);
/*
.....Add tool axis vectors to calculated points
*/
	um_vcmnvc(&ept[3],&spt[3],tv);
/*	um_unitvc(tv,tv);*/
	vdis = um_dcccc(&ept[3],&spt[3]);
	svc[0] = spt[0]; svc[1] = spt[1]; svc[2] = spt[2];
	d2[0] = um_dcccc(ept,spt);
	for (inc=0;inc<*npt-1;inc++)
	{
		rat = d1[inc] / d2[inc];
		pto[inc][3] = spt[3] + rat * tv[0];
		pto[inc][4] = spt[4] + rat * tv[1];
		pto[inc][5] = spt[5] + rat * tv[2];
		um_unitvc(&pto[inc][3],&pto[inc][3]);
	}
	pto[*npt-1][3] = ept[3];
	pto[*npt-1][4] = ept[4];
	pto[*npt-1][5] = ept[5];
/*
.....Set colors for moves
*/
	if (*npt == 1)
	{
		colr[0] = -1;
	}
	else if (*npt == 2)
	{
		if (ept_is_in)
		{
			colr[0] = -1;
			colr[1] = 1;
		}
		else
		{
			colr[0] = 1;
			colr[1] = -1;
		}
	}
	else if (*npt == 3)
	{
		colr[0] = -1;
		colr[1] = 1;
		colr[2] = -1;
	}
	goto done;
/*
.....No points were within clipping planes
.....Return final point in black
*/
none_in:;
	*npt = 1;
	for (i=0;i<6;i++) pto[0][i] = ept[i];
	if (!both_are_in) colr[0] = -1;
	else colr[0] = 1;
/*
.....End of routine
*/
done:;
	for (i=0;i<*npt;i++)
	{
		UM_cc_exttoint(pto[i],pto[i]);
		ncl_mcstowcs(0,pto[i],pto[i]);
	}
	return(both_are_in);
}

/*********************************************************************
**    E_FUNCTION     : ncl_step_displayed(dir,flag,speed)
**			Steps forward and backward through the displayed motion
**			list.  Can be used in interactive (prompting) mode or
**			for stepping a single segment (dynamic) only.
**    PARAMETERS   
**       INPUT  : 
**          dir     = -1 = Step backward.  1 = Step forward.
**                    -2 = Step to beginning.  2 = Step to end.
**                    -3 = Step to selected motion.
**          flag    = 0 = Dynamic single step only.
**                    1 = Interactive mode.  This routine will prompt for
**                        each step.
**                    2 = Playback motion to end.
**          speed   = Speed of playback when flag=2.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_step_displayed(dir,flag,speed)
int dir,flag;
int *speed;
{
	int numint,imark,markval,nsels;
	UM_int2 ifl,ifl86,ifl35;
	UN_motseg *mpt;
	UN_motseg_cutter cutseg;
	static char *lpr = "Reverse stepping mode";
	char buf[80];
	UD_PLOCREC pick;
/*
.....Interactive mode
.....Trap Reject Op
*/
	imark = 0;
	if (mlist_first_ptr == (UN_motseg *)-1) goto done;
	if (flag == 1)
	{
		imark = 1;
		UD_MARK(markval,UU_TRUE);
		if (markval != 0)
		{
			ncl_step_motion(&mpt,2);
			UN_step_ptr = (UN_motseg *)0;
			goto done;
		}
	}
/*
.....Default prompt response based on direction
.....(For use when DONE key is entered)
*/
	buf[0] = 'B';
	if (dir == 1) buf[0] = 'F';
/*
.....Step to beginning of displayed motion
*/
	if (dir == -2)
	{
		if (NCL_preview_mot)
			UN_step_ptr = (UN_motseg *)uu_lsnext(mlist_first_ptr2);
		else
			UN_step_ptr = (UN_motseg *)uu_lsnext(mlist_first_ptr);
		ncl_motion_undraw(&UN_step_ptr,UU_TRUE);
		goto done;
	}
/*
.....Step to user selected motion
*/
	if (dir == -3)
	{
		ud_setpick_type(UD_PICK_MOTSEG);
		ud_ldas(UD_DASPCKLOC, UA_NCL, 649, &pick, 1, &nsels, 1);
		if (nsels > 0 && pick.ppath.depth != 0)
		{
			mpt = (UN_motseg *)pick.ppath.pickpath[1];
			ncl_motion_undraw(&mpt,UU_FALSE);
			UN_step_ptr = (UN_motseg *)uu_lsnext(mpt);
		}
		goto done;
	}
/*
.....Get first motion segment to display
*/
	mpt = mlist_ptr;
	if (flag != 1 && UN_step_ptr != (UN_motseg *)0) mpt = UN_step_ptr;
	if (mpt != (UN_motseg *)-1 && mpt != (UN_motseg *)0)
		ncl_cutter_get(&cutseg,mpt->cutter);
/*
.....Loop through the motion display list
.....until we are at the end of the list or
.....the user wants out
*/
	while (mpt != (UN_motseg *)-1 && mpt != (UN_motseg *)0)
	{
		ncl_step_motion(&mpt,dir);
/*
........Remember our motion display list location
........For dynamic stepping
*/
		UN_step_ptr = mpt;
		if (flag == 0) goto done;
		if (mpt == (UN_motseg *)-1 || mpt == (UN_motseg *)0) break;
/*
........Let the user step through the display list
*/
		if (flag == 1)
		{
			ud_das(UD_DASSTRING,lpr,buf,1,&numint);
			dir = 1;
			if (buf[0] == 'b' || buf[0] == 'B') dir = -1;
/*
........The user wants out
........Display the rest of the list
*/
			if (buf[0] == 'q' || buf[0] == 'Q')
			{
				ncl_step_motion(&mpt,2);
				UN_step_ptr = (UN_motseg *)0;
				goto done;
			}
		}
/*
........In playback mode
........See if the user interrupted the motion
*/
		else if (flag == 2)
		{
			ifl = 86;
			ifl35 = 0;
			ckintr(&ifl86,&ifl35);
			getifl(&ifl,&ifl86);
			if (ifl86 != 0)
			{
				ifl86 = 0;
				setifl(&ifl,&ifl86);
				goto done;
			}
/*
........Honor speed setting
*/
			ncl_pb_delay(*speed,1);
		}
	}
/*
.....End of routine
*/
done:;
	if (imark == 1)
	{
		UD_UNMARK(markval);
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_motion_undraw(mpt,flag)
**			Erases the motion from the screen without removing the moves
**       from the displayed motion list.
**    PARAMETERS   
**       INPUT  : 
**          mpt     = Pointer to motion record to erase to when 'flag'
**                    is set to UU_FALSE.
**          flag    = 0 = Dynamic single step only.  1 = Interactive
**                    mode.  This routine will prompt for each step.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motion_undraw(mpt,flag)
UN_motseg **mpt;
int flag;
{
	int i;
	UV_vport vport;
	UV_view view;
/*
.....Erase all motion
*/
	if (flag)
	{
		for (i=1;i<=UV_act_screen[0].nvports;i++)
		{
			uv_getvpid(UV_act_screen[0].vports[i-1],&vport);
			uv_getvid(vport.cur_view,&view);
			view.modified = UU_TRUE;
			uv_delete_hidden(&vport);
			uv_autofact7_redrawvp(&vport,&view,UU_TRUE);
		}
		ncl_step_motion(mpt,-1);
	}
/*
.....Erase motion to specified location
*/
	else
	{
		ncl_step_motion(mpt,-3);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_play_initscan(modals,iclpt,icurpt,flag)
**			Initializes variables used to scan a clfile.
**    PARAMETERS   
**       INPUT  : 
**			iclpt   = Array of clfile pointers to set when flag = UU_TRUE.
**			          1 = UN_clfile_start.  2 = UN_clfile_current.
**			          3 = UN_clfile_curpt.  4 = UN_clfile_end.
**			icurpt  = Clfile location to start scan when 'flag = UU_TRUE.
**			flag    = UU_TRUE when clfile pointers are furnished.  If set to
**			          UU_FALSE, then the entire clfile is scanned.
**
**       OUTPUT :  
**			modals   = Playback modals array set for scanning.
**			iclpt    = Array of current clfile pointers.
**
**    RETURNS      :
**			none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_play_initscan(modals,iclpt,icurpt,flag)
int modals[],flag;
UN_clstruc *iclpt[4];
int *icurpt;
{
	UN_clstruc *istrt,*icur,*iend;
	int ipt,i;
/*
.....Set modals
*/
	for (i=0;i<18;i++) modals[i] = 0;
	modals[2] = 100; modals[3] = 1;
/*
.....Save the clfile pointers provided
*/
	if (flag)
	{
		istrt = iclpt[0];
		icur = iclpt[1];
		ipt = *icurpt;
		iend = iclpt[3];
	}
/*
.....Save the clfile settings
*/
	iclpt[0] = UN_clfile_start;
	iclpt[1] = UN_clfile_current;
	*icurpt = UN_clfile_curpt;
	iclpt[3] = UN_clfile_end;
/*
.....Set the clfile pointers
*/
	if (flag)
	{
		UN_clfile_start = istrt;
		UN_clfile_current = icur;
		UN_clfile_curpt = ipt;
		UN_clfile_end = iend;
	}
	else
	{
		UN_clfile_start = UU_NULL;
		UN_clfile_current = UU_NULL;
		UN_clfile_curpt = 0;
		UN_clfile_end = UU_NULL;
	}
/*
.....Save the active clfile record
*/
	ncl_save_iclw();
}

/*********************************************************************
**    E_FUNCTION     : ncl_play_resetscan(iclpt,icurpt)
**			Resets clfile pointers after a scan.
**    PARAMETERS   
**       INPUT  : 
**			iclpt   = Array of saved clfile pointers.
**			icurpt  = Current clfile position to set.
**
**       OUTPUT :  
**			none.
**
**    RETURNS      :
**			none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_play_resetscan(iclpt,icurpt)
UN_clstruc *iclpt[4];
int icurpt;
{
/*
.....Reset the clfile pointers
*/
	UN_clfile_start = iclpt[0];
	UN_clfile_current = iclpt[1];
	UN_clfile_curpt = icurpt;
	UN_clfile_end = iclpt[3];
/*
.....Restore the active clfile record
*/
	ncl_reset_iclw();
}

/*********************************************************************
**    E_FUNCTION     : ncl_count_clrec()
**			Counts the number of clfile records in the current Motion
**       Playback range.
**    PARAMETERS   
**       INPUT  : 
**			  none.
**       OUTPUT :  
**			  none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_count_clrec()
{
	int nclrec;
	UN_clstruc *irec;
	UM_int2 jerr;
	UM_int4 iclw[6];
	UM_real8 rclw[420];
/*
.....Loop to read clfile
*/
	nclrec = 0;
	irec = UN_clfile_current;
	do
	{
/*
.....Read a clfile record
*/
		clread(&UN_clfile,&irec,iclw,rclw,&jerr);
		if (jerr == 1) goto done;
		nclrec++;
	} while (irec != UU_NULL && irec != UN_clfile_end);
/*
.....End of routine
*/
done:;
	return(nclrec);
}
