/*********************************************************************
**	 NAME:  necdraw.c
**		 CONTAINS:
**			ncl_draw_cutter_parts
**			ncl_draw_cutter
**			ncl_draw_blade_cutter
**			ncl_draw_holder
**			ncl_display_cutter
**			ncl_display_blade_cutter
**			ncl_cutter_circle
**			ncl_cutter_nsides
**			ncl_cutr_arcc2p
**			ncl_blade_get_fwd
**			ncl_blade_orient
**	 COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**	 MODULE NAME AND RELEASE LEVEL 
**       necdraw.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:25
*********************************************************************/

#include "usysdef.h"
#include "lipv.h"
#include "zsysdep.h"
#include "class.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "mdcoord.h"
#include "gomisc.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gviw.h"
#include "gsegop.h"
#include "gmat4.h"
#include "gconvert.h"
#include "mdcoord.h"
#include "mfort.h"
#include "modef.h"
#include "mplot.h"
#include "mgeom.h"
#include "nclfc.h"
#include "nclmplay.h"

#define DTOR (UM_PI/180.0)

extern int DRAW_DISPLAY;

void ncl_cutter_nsides();
void ncl_blade_get_fwd();
void ncl_blade_orient();

static UM_vector Svr = {0.0, 0.0, 0.0};

static void S_calc_toprad();

/*********************************************************************
**    E_FUNCTION     : ncl_draw_cutter_parts(cutseg,recalc,vp,ipv)
**			Calculates an the cutter, shank, and holder profiles and stores
**       them in the cutter curve arrays.
**    PARAMETERS   
**       INPUT  : 
**          cutseg = Current cutter definition.
**				recalc = Flags for recalculating cutter shape [0] = Cutter,
**                   [1] = Shank, [2] = Holder.
**				vp     = View port to calculate cutter for.
**				ipv    = 1 = Cutter is drawn for IPV.
**       OUTPUT :  
**          cutseg = Updated curve lists for cutter.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_draw_cutter_parts(cutseg,recalc,vp,ipv)
UN_motseg_cutter *cutseg;
UU_LOGICAL recalc[];
int vp;
int ipv;
{
	UU_REAL toprad;
/*
.....Recalculate cutter shape
*/
	toprad = 0.;
	if (recalc[0] == 1) ncl_draw_cutter(cutseg,vp,ipv,&toprad);
/*
.....Recalculate shank shape
*/
	if (recalc[1] == 1)
	{
		if (toprad == 0) S_calc_toprad(cutseg->cutsym,&toprad);
		ncl_draw_holder(cutseg->shank,vp,&toprad);
	}
/*
.....Recalculate holder shape
*/
	if (recalc[2] == 1)
	{
		if (toprad == 0)
		{
			S_calc_toprad(cutseg->cutsym,&toprad);
			S_calc_toprad(cutseg->shank,&toprad);
		}
		ncl_draw_holder(cutseg->holder,vp,&toprad);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_draw_cutter(cutseg,vp,ipv,toprad)
**			Calculates an NCL defined cutter profile and stores it in the
**			cutter curve array.
**    PARAMETERS   
**       INPUT  : 
**          cutseg = Current cutter definition.
**				vp     = View port to calculate cutter for.
**				ipv    = 1 = Cutter is drawn for IPV.
**       OUTPUT :  
**          cutseg = Updated curve list for cutter.
**          toprad = Radius at top of cutter.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_draw_cutter(cutseg,vp,ipv,toprad)
int vp;
int ipv;
UN_motseg_cutter *cutseg;
UU_REAL *toprad;
{
	int status,i,npts,n,ibar,nint;
	UU_REAL rad,dist,scal,si,hsidtn;
	UM_vector taxis,temp,temp2,vpn,right_side,tx;
	UM_length tdiam,tradius,tcrad,theight,tangle,x2,z2,r2,sina,cosa;
	UM_coord c_right,p1_right,p2_right,p2a_right,p3_right,p2b_right;
	UM_coord tend,tstart,cb_right,npt,*curve;
	UU_LIST *cvlist,*cnlist;
	struct UM_circle_rec c,c2;
/*
.....Lathe cutter
*/
	if (cutdef[vp].cutr[8] > 10)
	{
		ncl_draw_lathe_cutter(cutseg,vp,ipv);
		return(UU_SUCCESS);
	}
/*
.....Diameter less than zero
.....Blade cutter
*/
	if (cutdef[vp].cutr[0] < 0.)
	{
		ncl_draw_blade_cutter(cutseg,vp,ipv);
		return(UU_SUCCESS);
	}
/*
.....Store cutter parameters in local variables
*/
	tend[0] = 0.;
	tend[1] = 0.;
	tend[2] = 0.;
	taxis[0] = 0.;
	taxis[1] = 0.;
	taxis[2] = 1.;
	vpn[0] = 0.;
	vpn[1] = 1.;
	vpn[2] = 0.;
	tdiam = cutdef[vp].cutr[0];
	tradius = tdiam / 2.0;
	tcrad = cutdef[vp].cutr[1];
	theight = cutdef[vp].cutr[2];
	if (theight < UM_FUZZ) theight = tcrad;
	tangle = cutdef[vp].cutr[3];
	if (fabs(tangle) < UM_FUZZ) tangle = 0.;
	x2 = cutdef[vp].cutr[4];
	z2 = cutdef[vp].cutr[5];
	r2 = cutdef[vp].cutr[6];
	sina = cutdef[vp].cutr[7];
	ibar = (int)cutdef[vp].cutr[8];
	tangle = tangle * UM_TWOPI / 360.0;
	*toprad = 0.;
	if (ipv == 1) scal = 1.;
	else scal = cutdef[vp].view.scale;
/*
.....Initialize cutter storage
*/
	UU_LIST_EMPTY(&cutseg->cutsym->geo->curve);
	UU_LIST_EMPTY(&cutseg->cutsym->geo->cnorm);
	curve = (UM_coord *)UU_LIST_ARRAY(&cutseg->cutsym->geo->curve);
	cvlist = &cutseg->cutsym->geo->curve;
	cnlist = &cutseg->cutsym->geo->cnorm;
	npts = 0;
/*
.....If both the tool diameter and tool height are zero,
.....don't do anything
*/
   if ((tdiam < UM_FUZZ) && (theight < UM_FUZZ)) goto done;
/*
.....Adjust angle for disk cutter
*/
	si = sin(tangle);
	hsidtn = tcrad * (1.-si);
	if (theight < hsidtn-UM_FUZZ)
	{
		si = (tcrad-theight) / tcrad;
		tangle = asin(si);
	}
/*
........Calculate maximum height
........For bell shaped cutter
*/
	if (tangle < 0. && ibar == 0)
	{
		dist = ((tdiam / (2.*tan(-tangle))) + (tcrad*(1.+tan(-tangle/2))));
		if (dist < theight) theight = dist;
	}
   um_vctmsc(taxis, theight, tx);
   um_vcplvc(tend, tx, tstart);
/*
.....No diameter
.....draw a straight line
*/
	if (tdiam < UM_FUZZ && fabs(tangle) < UM_FUZZ)
	{
		uu_list_push(cvlist,tend); npts++;
		uu_list_push(cnlist,taxis);
		uu_list_push(cvlist,tstart); npts++;
		uu_list_push(cnlist,taxis);
	}
/*
.....Draw cutter profile
*/
	else
	{
		if (tdiam < UM_FUZZ) tdiam = .001;
/*
.....Determine invariant circular
.....cross-section of tool geometry
*/
		um_vctovc(taxis, c.nvec);
		um_cross(taxis, vpn, c.svec);
		um_unitvc(c.svec, c.svec);
		um_vctovc(c.svec, tx);
		c.dang = UM_TWOPI;
/*
........Calculate vector defining right
........hand side of profile
........(as viewed from view plane normal)
*/
		if (fabs(tangle) < UM_FUZZ)
		{
			um_vctovc(taxis, right_side);
		}
		else
		{
			um_vctmsc(c.svec, sin(tangle), right_side);
			um_vctmsc(taxis, cos(tangle), temp);
			um_vcplvc(right_side, temp, right_side);
			um_unitvc(right_side, right_side);
		}
/*
........Draw tool end geometry
*/
		c.radius = tradius - tcrad;
		um_vctovc(tend, c.center);
		um_vctmsc(c.svec, c.radius, temp);
		um_vcplvc(c.center, temp, p1_right);
		uu_list_push(cvlist,tend); npts++;
		uu_list_push(cnlist,taxis);
		uu_list_push(cvlist,p1_right); npts++;
		uu_list_push(cnlist,taxis);
/*
........Draw tool mid geometry
...........Calc right barrel side arc centers
*/
		if (ibar == 1)
		{
			um_vctmsc(tx, x2, temp);
			um_vctmsc(taxis, z2, temp2);
			um_vcplvc(temp2, temp, cb_right);
			um_vcplvc(cb_right, tend, cb_right);
		}
/*
...........Calculate corner radius circles
...........and end points
*/
		if (tcrad < UM_FUZZ)
		{
			um_vctovc(p1_right, p2_right);
			um_vctovc(p1_right, p2b_right);
		}
		else
		{
			um_vctmsc(taxis, tcrad, temp);
			um_vcplvc(p1_right, temp, c_right);
/*
...........Height is greater than corner radius
*/
			if (theight > tcrad)
			{
				um_vctmsc(c.svec, tcrad, temp);
				um_vcplvc(c_right, temp, p2_right);
				if (fabs(tangle) > UM_FUZZ)
				{
					um_nptln(c_right, p2_right, right_side, npt);
					um_vcmnvc(npt, c_right, temp);
					um_unitvc(temp, temp);
					um_vctmsc(temp, tcrad, temp);
					um_vcplvc(c_right, temp, p2_right);
				}
			}
/*
...........Height is less than corner radius
*/
			else
			{
				um_vctmsc(taxis, theight, temp);
				um_vcplvc(p1_right, temp, p2_right);
				rad = sqrt(theight*((2.0*tcrad) - theight ));
				um_vctmsc(c.svec, rad, temp);
				um_vcplvc(p2_right, temp, p2_right);
			}
/*
...........Calculate p2 points if
...........Second radius is smaller than
...........first radius with barrel cutter
*/
			um_vctovc(p2_right,p2b_right);
			if (ibar == 1 && r2 < tcrad)
			{
				um_vcmnvc(cb_right,c_right,temp);
				um_unitvc(temp,temp);
				um_vctmsc(temp,r2,temp);
				um_vcplvc(cb_right,temp,p2_right);
			}
			*toprad = c.radius;
		}
/*
........Draw corner radius of tool profile
*/
		if (tcrad > UM_FUZZ)
		{
			status = ncl_cutr_arcc2p(c_right, p1_right, p2_right, &c2);
			n = 0;
			if (status == UU_SUCCESS)
			{
				ncl_cutter_nsides(&c2,scal,&n);
				uu_list_expand(cvlist,n);
				curve = (UM_coord *)UU_LIST_ARRAY(cvlist);
				ncl_cutter_circle(&c2,&curve[npts],n);
				for (i=0;i<n;i++)
				{
					um_vcmnvc(curve[i+npts],c2.center,temp);
					uu_list_push(cnlist,temp);
				}
				npts = npts + n;
				uu_list_push(cvlist,p2_right); npts++;
				uu_list_push(cnlist,temp);
			}
		}
		else
		{
			uu_list_push(cvlist,p2_right); npts++;
			uu_list_push(cnlist,temp);
		}
/*
........Draw barrel cutter
*/
		if (ibar == 1)
		{
/*
...........Calc right vectors of
...........straight side segment (if any)
*/
			cosa = sqrt(1-sina*sina);
			um_vctmsc(tx, sina, temp);
			um_vctmsc(taxis, cosa, temp2);
			um_vcplvc(temp, temp2, right_side);
			um_unitvc(right_side, right_side);
/*
...........Calc p2a points at top of barrel segment
*/
			um_nptln (cb_right, p2b_right, right_side, npt);
			um_vcmnvc(npt, cb_right, temp);
			um_unitvc(temp,temp);
			um_vctmsc(temp,r2,temp);
			um_vcplvc(cb_right, temp, p2a_right);
/*
...........Draw circle around midsection of barrel
*/
			if (sina < 0. && z2 > 0.)
			{
				um_vctmsc(taxis,z2,c.center);
				um_vcplvc(tend,c.center,c.center);
				dist = um_dcccc(cb_right,c.center);
				if (r2 >= tcrad) c.radius = r2 - dist;
				else c.radius = r2 + dist;
			}
/*
...........Put circle thru points p2 & p2a 
*/
			status = ncl_cutr_arcc2p(cb_right, p2_right, p2a_right, &c2);
			if (status == UU_SUCCESS)
			{
				ncl_cutter_nsides(&c,scal,&n);
				uu_list_expand(cvlist,n);
				curve = (UM_coord *)UU_LIST_ARRAY(cvlist);
				ncl_cutter_circle(&c2,&curve[npts],n);
				for (i=0;i<n;i++)
				{
					um_vcmnvc(curve[i+npts],c2.center,temp);
					um_unitvc(temp,temp);
					uu_list_push(cnlist,temp);
				}
				npts = npts + n;
				uu_list_push(cvlist,p2a_right); npts++;
				uu_list_push(cnlist,temp);
			}
/*
...........Move points p2a to p2 so straight part
...........of side will be drawn below
*/
			um_vctovc(p2a_right, p2_right);
		}
/*
........Draw tool top geometry
*/
		um_ilnpln(p2_right, right_side, tstart, taxis, &nint, p3_right);
		*toprad = c.radius = um_dcccc(tstart, p3_right);
		um_vctovc(tstart, c.center);
		uu_list_push(cvlist,p3_right); npts++;
		uu_list_push(cvlist,p3_right); npts++;
		uu_list_push(cvlist,tstart); npts++;
		uu_list_push(cnlist,temp);
		uu_list_push(cnlist,taxis);
		uu_list_push(cnlist,taxis);
	}
	status = UU_SUCCESS;
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_draw_blade_cutter(cutseg,vp,ipv)
**			Calculates an NCL defined blade cutter profile and stores it in
**			the cutter curve array.
**    PARAMETERS   
**       INPUT  : 
**          cutseg = Current cutter definition.
**				vp     = View port to calculate cutter for.
**				ipv    = 1 = Cutter is drawn for IPV.
**       OUTPUT :  
**          cutseg = Updated curve list for cutter.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_draw_blade_cutter(cutseg,vp,ipv)
int vp;
int ipv;
UN_motseg_cutter *cutseg;
{
	int npts;
	UU_REAL dist,sina,cosa;
	UM_coord tend,pt1,p1_right,p1_left;
	UM_vector temp,vc0;
	UM_length theight,r2;
	UU_LIST *cvlist,*cnlist;
/*
.....Initialize routine
*/
	theight = cutdef[vp].cutr[2];
	r2 = cutdef[vp].cutr[6] / 2.;
	sina = cutdef[vp].cutr[7];
	cosa = cos(asin(sina));
	tend[0] = tend[1] = tend[2] = 0.;
	vc0[0] = vc0[1] = vc0[2] = 0.;
/*
.....Initialize cutter storage
*/
	UU_LIST_EMPTY(&cutseg->cutsym->geo->curve);
	UU_LIST_EMPTY(&cutseg->cutsym->geo->cnorm);
	cvlist = &cutseg->cutsym->geo->curve;
	cnlist = &cutseg->cutsym->geo->cnorm;
	npts = 0;
/*
.....Store attach point
*/
	uu_list_push(cvlist,tend);
/*
.....Draw to bottom right point
*/
	p1_right[0] = tend[0] + r2;
	p1_right[1] = tend[1];
	p1_right[2] = tend[2];
	p1_left[0] = tend[0] - r2;
	p1_left[1] = tend[1];
	p1_left[2] = tend[2];
	uu_list_push(cvlist,p1_right);
	uu_list_push(cnlist,vc0);
	npts++;
/*
.....Draw to top right point
*/
	if (cosa < UM_DFUZZ) cosa = dist = 0.;
	else dist = theight / cosa;
	temp[0] = sina * dist;
	temp[1] = cosa * dist;
	temp[2] = 0.;
	um_vcplvc(p1_right,temp,pt1);
	uu_list_push(cvlist,pt1);
	uu_list_push(cnlist,vc0);
	npts++;
/*
.....Draw to top left point
*/
	temp[0] = -(sina*dist);
	um_vcplvc(p1_left,temp,pt1);
	uu_list_push(cvlist,pt1);
	uu_list_push(cnlist,vc0);
	npts++;
/*
.....Draw to bottom left point
*/
	uu_list_push(cvlist,p1_left);
	uu_list_push(cnlist,vc0);
	npts++;
/*
.....Close at botton right point
*/
	if (r2 > UM_FUZZ)
	{
		uu_list_push(cvlist,p1_right);
		uu_list_push(cnlist,vc0);
		npts++;
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_draw_holder(cutsym,vp,toprad)
**			Calculates an NCL defined shank/holder profile and stores it in
**			in the cutter curve array.
**    PARAMETERS   
**       INPUT  : 
**          cutsym = Current cutter symbol structure.
**				vp     = View port to calculate shank/holder for.
**				toprad = Radius at top of previous cutter part.
**       OUTPUT :  
**          cutsym = Updated curve list for cutter symbol.
**				toprad = Updated radius at top of this cutter shank/holder.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_draw_holder(cutsym,vp,toprad)
int vp;
UN_motseg_symbol *cutsym;
UU_REAL *toprad;
{
	int status,i,ix0,ix1;
	UU_REAL trad,thgt;
	UM_coord pt;
	UM_vector taxis,xvec;
	UU_LIST *cvlist,*cnlist;
/*
.....Zero height specified
.....Don't do anything
*/
   if (cutsym->parms[1] < UM_FUZZ) goto done;
/*
.....Initialize cutter storage
*/
	UU_LIST_EMPTY(&cutsym->geo->curve);
	UU_LIST_EMPTY(&cutsym->geo->cnorm);
	cvlist = &cutsym->geo->curve;
	cnlist = &cutsym->geo->cnorm;
/*
.....Mill holder
*/
	if (cutdef[vp].cutr[8] <= 10)
	{
/*
.....Store simple curve
*/
		trad = cutsym->parms[0] / 2.0;
		if (trad == 0.) trad = *toprad;
		pt[0] = 0.;
		pt[1] = 0.;
		pt[2] = 0.;
		uu_list_push(cvlist,pt);
		taxis[0] = 0.;
		taxis[1] = 0.;
		taxis[2] = 1.;
		uu_list_push(cnlist,taxis);
		pt[0] = trad;
		uu_list_push(cvlist,pt);
		uu_list_push(cvlist,pt);
		xvec[0] = 1.;
		xvec[1] = 0.;
		xvec[2] = 0.;
		uu_list_push(cnlist,taxis);
		uu_list_push(cnlist,xvec);
		pt[0] = pt[0] + cutsym->parms[1]*tan(cutsym->parms[2]/UM_RADIAN);
		pt[2] = pt[2] + cutsym->parms[1];
		*toprad = pt[0];
		uu_list_push(cvlist,pt);
		uu_list_push(cvlist,pt);
		pt[0] = 0.;
		uu_list_push(cvlist,pt);
		uu_list_push(cnlist,xvec);
		uu_list_push(cnlist,taxis);
		uu_list_push(cnlist,taxis);
	}
/*
.....Lathe holder
*/
	else
	{
/*
.....Store simple curve
.....First point is attach point
*/
		if (cutsym->parms[0] <= cutsym->parms[1])
		{
			trad = cutsym->parms[0] / 2.;
			thgt = cutsym->parms[1];
			ix0 = 0; ix1 = 1;
			cutsym->geo->axis[0] = cutsym->geo->axis[2] = 0.;
			cutsym->geo->axis[1] = 1;
		}
		else
		{
			trad = -cutsym->parms[1] / 2.;
			thgt = cutsym->parms[0];
			ix0 = 1; ix1 = 0;
			cutsym->geo->axis[1] = cutsym->geo->axis[2] = 0.;
			cutsym->geo->axis[0] = 1;
		}
		pt[ix0] = 0.;
		pt[ix1] = thgt;
		pt[2] = 0.;
		uu_list_push(cvlist,pt);
		pt[ix0] = -trad;
		pt[ix1] = 0.;
		pt[2] = 0.;
		uu_list_push(cvlist,pt);
		pt[ix0] = trad;
		uu_list_push(cvlist,pt);
		pt[ix1] = pt[ix1] + thgt;
		uu_list_push(cvlist,pt);
		pt[ix0] = -trad;
		uu_list_push(cvlist,pt);
		pt[ix1] = pt[ix1] - thgt;
		uu_list_push(cvlist,pt);
		xvec[0] = 0.;
		xvec[1] = 0.;
		xvec[2] = 0.;
		for (i=0;i<6;i++) uu_list_push(cnlist,xvec);
	}
done:
	status = UU_SUCCESS;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_display_cutter(cutseg,cutsym,vp,color,shaded,trans,
**		                                    ngpt,ofs,nface,itype)
**			Calculates the tool part display (cutter,shank,holder) for the
**			specified viewport.
**    PARAMETERS   
**       INPUT  : 
**          cutseg = Current cutter definition.
**          cutsym = Current cutter symbol structure.
**				vp     = View port to draw cutter for.
**				color  = Color to display tool part in.
**				shaded = 1 = Tool part is shaded.
**				trans  = Transparency of tool part.
**				ngpt   = Number of points currently in tool display storage.
**				ofs    = XYZ offsets for tool part.
**				nface  = Current number of faces (sides) generated for tool
**				         parts.
**				itype = 1 = Cutter definition, 2 = Shank, 3 = Holder.
**       OUTPUT :  
**				ngpt   = Updated number of tool display points.
**				ofs    = Updated XYZ offsets for tool part.
**				nface  = Updated number of faces (sides) generated for tool
**				         part if greater than input value.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_display_cutter(cutseg,cutsym,vp,color,shaded,trans,ngpt,ofs,nface,itype)
int vp;
int color;
int shaded;
int trans;
int *ngpt;
int *nface;
int itype;
UU_REAL ofs[];
UN_motseg_cutter *cutseg;
UN_motseg_symbol *cutsym;
{
#define MAXCIRC 50
	int status,i,j,nc,iend,npts,ngeo,n,isparl,isperp,segfl,ncirc,nsides;
	UU_LOGICAL ifl,ifl1,ifl2,iuse;
	UU_REAL rad,dist,scal,angl,cirrad[MAXCIRC],zhgt,xrad,rinc,pofs[3];
	UM_transf tm;
	UM_coord tend,pt1,pt2,cvpt[500],*curve;
	UM_vector taxis,temp,vpn,*cnorm,cvvc[500];
	Gwpoint3 *vgpt;
	struct UM_circle_rec c;
/*
.....Store cutter color
*/
	if (color != 0)
	{
		i = -color;
		uu_list_push(&cutdef[vp].view.npt,&i); cutdef[vp].view.ngeo++;
		i = -shaded;
		uu_list_push(&cutdef[vp].view.npt,&i); cutdef[vp].view.ngeo++;
		i = -trans;
		uu_list_push(&cutdef[vp].view.npt,&i); cutdef[vp].view.ngeo++;
	}
/*
.....Lathe cutter
*/
	if (cutdef[vp].cutr[8] > 10 && cutsym->geo->type != 5)
	{
		ncl_display_lathe_cutter(cutseg,cutsym,vp,shaded,ngpt,ofs);
		goto padit;
	}
/*
.....Diameter less than zero
.....Blade cutter
*/
	if (cutdef[vp].cutr[0] < 0. && itype == 1)
	{
		ncl_display_blade_cutter(cutseg,cutsym,vp,shaded,ngpt,ofs);
		goto padit;
	}
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	curve = (UM_coord *)UU_LIST_ARRAY(&cutsym->geo->curve);
	cnorm = (UM_vector *)UU_LIST_ARRAY(&cutsym->geo->cnorm);
	npts = UU_LIST_LENGTH(&cutsym->geo->curve);
	nc = *ngpt;
	ngeo = cutdef[vp].view.ngeo;
	zhgt = 0.;
	if (npts == 0) goto done;
/*
.....Store cutter parameters in local variables
*/
	if (cutsym->geo->type == 5)
	{
		if (cutdef[vp].seguse)
		{
			tend[0] = ofs[2] + cutsym->zlim[0];
			tend[1] = ofs[1];
			tend[2] = -ofs[0];
			taxis[0] = cutsym->geo->axis[2];
			taxis[1] = cutsym->geo->axis[1];
			taxis[2] = -cutsym->geo->axis[0];
		}
		else
		{
			tend[0] = ofs[0];
			tend[1] = ofs[1];
			tend[2] = ofs[2] + cutsym->zlim[0];
			um_vctovc(cutsym->geo->axis,taxis);
/*
			taxis[0] = cutdef[vp].tlax[0];
			taxis[1] = cutdef[vp].tlax[2];
			taxis[2] = cutdef[vp].tlax[1];
*/
		}
		pofs[0] = pofs[1] = pofs[2] = 0.;
	}
	else
	{
		tend[0] = tend[1] = tend[2] = 0.;
		taxis[0] = cutdef[vp].tlax[0];
		taxis[1] = cutdef[vp].tlax[1];
		taxis[2] = cutdef[vp].tlax[2];
		pofs[0] = ofs[0];
		pofs[1] = ofs[1];
		pofs[2] = ofs[2];
	}
	segfl = cutdef[vp].segfl;
	vpn[0] = cutdef[vp].view.vpn[0];
	vpn[1] = cutdef[vp].view.vpn[1];
	vpn[2] = cutdef[vp].view.vpn[2];
	scal = cutdef[vp].view.scale;
	xrad = 0.;
/*
.....Determine invariant circular
.....cross-section of tool geometry
*/
	um_vctovc(tend,c.center);
	um_vctovc(taxis,c.nvec);
	if (um_vcparall(taxis,vpn))
		um_perpvc(taxis,c.svec);
	else
		um_cross(taxis,vpn,c.svec);
	um_unitvc(c.svec,c.svec);
	if (um_mag(c.svec) < UM_DFUZZ)
	{
		um_perpvc(taxis,c.svec);
		um_unitvc(c.svec,c.svec);
	}
	c.dang = UM_TWOPI;
/*
.....Transform curve to tool cross section
*/
	for (i=0;i<npts;i++)
	{
		if (fabs(curve[i][0]) > xrad) xrad = fabs(curve[i][0]);
		if (curve[i][2] > zhgt) zhgt = curve[i][2];
		rad = curve[i][2]+pofs[2] /*- tend[2]*/;
		um_vctmsc(c.nvec,rad,temp);
		um_vcplvc(c.center,temp,pt1);
		rad = curve[i][0]+pofs[0] /*- tend[0]*/;
		um_vctmsc(c.svec,rad,temp);
		um_vcplvc(pt1,temp,cvpt[i]);
/*
........Transform curve normals
*/
		if (shaded)
		{
			um_vctmsc(c.nvec,cnorm[i][2],pt1);
			rad = curve[i][0]+pofs[0] /*- tend[0]*/;
			um_vctmsc(c.svec,cnorm[i][0],temp);
			um_vcplvc(pt1,temp,cvvc[i]);
		}
	}
	c.center[2] = tend[2];
/*
.....Determine if tlaxis is parallel or perpto view normal axis
.....Used for optimizing cutter display
*/
	if (shaded)
	{
		isparl = 0;
		isperp = 1;
	}
	else if (cutdef[vp].seguse || segfl == 2)
	{
		isparl = 0;
		isperp = 0;
	}
	else
	{
		isparl = um_vcparall(taxis,vpn);
		isperp = um_vcperp(taxis,vpn);
	}
/*
.....Calculate consistent number of circle points
.....along length of cutter
*/
	c.radius = xrad;
	ncl_cutter_nsides(&c,scal,&nsides);
	if (xrad == 0.) nsides = 0;
/*
.....Draw cutter profile
*/
	if (nsides == 0)
	{
		iend = 1;
		rinc = 0.;
	}
	else if (shaded)
	{
		rinc = 360.;
		iend = 1;
	}
	else if (segfl == 1 && isperp)
	{
		iend = (nsides+1) / 2 - 1;
		if (iend > 6) iend = 6;
		rinc = 180. / iend;
	}
	else if (segfl != 0)
	{
		iend = nsides - 1;
		if (iend > 12) iend = 12;
		rinc = 360. / iend;
	}
	else
	{
		rinc = 180.;
		iend = 2;
	}
	if (fabs(curve[npts-1][0]-curve[npts-2][0]) <= UM_FUZZ) ifl = UU_FALSE;
	else ifl = UU_TRUE;
	if (fabs(curve[0][2]-curve[1][2]) <= UM_FUZZ) ifl1 = UU_FALSE;
	else ifl1 = UU_TRUE;
	if (fabs(curve[npts-1][2]-curve[npts-2][2]) <= UM_FUZZ) ifl2 = UU_FALSE;
	else ifl2 = UU_TRUE;
	angl = 0.;
	for (i=0;i<iend;i++)
	{
		angl = rinc * (i+1) * DTOR;
		n = 0;
		for (j=0;j<npts;j++)
		{
/*
........Do we need to use this point?
*/
			iuse = UU_FALSE;
			if (j == npts-1) dist = 1.;
			else dist = um_dcccc(cvpt[j],cvpt[j+1]);
			if (isparl)
			{
				 if (dist <= UM_FUZZ || j == 1 || (j == npts-2 && ifl) ||
					(j == 0 && ifl1)) iuse = UU_TRUE;
			}
			else
			{
				if (dist > UM_FUZZ) iuse = UU_TRUE;
				if (j == 0 && ifl1) iuse = UU_TRUE;
				else if (j == npts-1 && ifl2) iuse = UU_TRUE;
				else if ((j == 0 || j == npts-1) && iend != 1) iuse = UU_FALSE;
			}
			if (shaded) iuse = 1;
/*
........Transfer point
*/
			if (iuse)
			{
				um_vctovc(cvpt[j],pt1);
				um_rotatept(pt1,taxis,tend,angl,1,tm);
				uu_list_push(&cutdef[vp].view.gpt,pt1);
				if (shaded)
				{
					um_vctovc(cvvc[j],pt1);
					um_rotatept(pt1,taxis,tend,angl,1,tm);
					uu_list_push(&cutdef[vp].view.gnorm,pt1);
				}
				nc++; n++;
			}
		}
		if (n == 1) nc--;
		else if (n != 0)
		{
			uu_list_push(&cutdef[vp].view.npt,&n);
			ngeo++;
		}
	}
/*
.....Return number of sides for shaded cutter
*/
	if (shaded != 0)
	{
		if (nsides > 20) nsides = 20;
		if (nsides > *nface) *nface = nsides;
		goto done;
	}
/*
.....Draw cutter circles
.....or lines if perpto view
*/
	ncirc = 0;
	for (i=0;i<npts;i++)
	{
		ifl = UU_FALSE;
		if (i == 1 && curve[0][2] == curve[1][2]) ifl = UU_TRUE;
		else if (i == npts-2 && curve[npts-2][2] == curve[npts-1][2])
			ifl = UU_TRUE;
		else if (i < npts-1 && um_dcccc(curve[i],curve[i+1]) < UM_DFUZZ)
			ifl = UU_TRUE;
		if (ifl)
		{
			rad = curve[i][0]+pofs[0] /*- tend[0]*/;
			if (fabs(rad) > UM_DFUZZ)
			{
				dist = curve[i][2]+pofs[2] /* - tend[2]*/;
				um_vctmsc(c.nvec,dist,temp);
				um_vcplvc(c.center,temp,pt1);
/*
.....Draw straight line
*/
				if (isperp)
				{
					uu_list_push(&cutdef[vp].view.gpt,cvpt[i]); nc++;
					rad = -rad;
					um_vctmsc(c.svec,rad,temp);
					um_vcplvc(pt1,temp,pt2);
					uu_list_push(&cutdef[vp].view.gpt,pt2); nc++;
					j = 2; uu_list_push(&cutdef[vp].view.npt,&j);
					ngeo++;
				}
/*
.....Draw circle
*/
				else
				{
					c.radius = fabs(rad);
					j = ncirc;
					if (isparl && ncirc > 0 && ncirc < 10)
					{
						for (j=0;j<ncirc;j++)
						{
							if (fabs(c.radius-cirrad[j]) < UM_DFUZZ) break;
						}
					}
					if (j == ncirc && ncirc < MAXCIRC)
					{
						cirrad[ncirc] = c.radius; ncirc++;
						um_vctovc(c.center,pt2);
						um_vctovc(pt1,c.center);
						uu_list_expand(&cutdef[vp].view.gpt,nsides);
						vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gpt);
						ncl_cutter_circle(&c,&vgpt[nc],nsides);
						um_vctovc(pt2,c.center);
						uu_list_push(&cutdef[vp].view.npt,&nsides);
						nc = nc + nsides;
						ngeo++;
					}
				}
			}
		}
	}
/*
.....End of routine
*/
done:
	*ngpt = nc;
	if (cutsym->geo->type == 5)
		um_translate_point(ofs,zhgt,cutsym->geo->axis,ofs);
	else ofs[2] = ofs[2] + zhgt;
	cutdef[vp].view.ngeo = ngeo;
/*
.....If not shaded pad normal vectors
.....in case following cutter part is shaded
*/
padit:
	if (!shaded)
	{
		pt2[0] = pt2[1] = pt2[2] = 0.;
		n = UU_LIST_LENGTH(&cutdef[vp].view.gpt);
		j = UU_LIST_LENGTH(&cutdef[vp].view.gnorm);
		for (i=j;i<n;i++) uu_list_push(&cutdef[vp].view.gnorm,pt2);
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_display_blade_cutter(cutseg,cutsym,vp,shaded,ngpt,
**		                                          ofs)
**			Calculates the tool display for a blade cutter.
**    PARAMETERS   
**       INPUT  : 
**          cutseg = Current cutter definition.
**          cutsym = Current cutter symbol structure.
**				vp     = View port to draw cutter for.
**				shaded = 1 = Tool part is shaded.
**				ngpt   = Number of points currently in tool display storage.
**				ofs    = XYZ offsets for tool part.
**       OUTPUT :  
**				ngpt   = Updated number of tool display points.
**				ofs    = Updated XYZ offsets for tool part.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_display_blade_cutter(cutseg,cutsym,vp,shaded,ngpt,ofs)
UN_motseg_cutter *cutseg;
UN_motseg_symbol *cutsym;
int vp;
int shaded;
int *ngpt;
UU_REAL ofs[];
{
	int status,npts,ngeo,isperp,i,j,nptx,segfl,nc,ist,ntess,ngeo_tess,cnpt;
	UU_LOGICAL um_plane1();
	UU_REAL um_mag();
	UM_coord tend,*curve,rpt,gpt[500],pt2,*cdef;
	UM_vector taxis,vpn,*cnorm,vfd,vc1,vc2,vr,gnorm[500];
	UM_plane pln;
	UM_length theight,tradius,tdiam;
	Gwpoint3 *vgpt;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	curve = (UM_coord *)UU_LIST_ARRAY(&cutsym->geo->curve);
	cnorm = (UM_vector *)UU_LIST_ARRAY(&cutsym->geo->cnorm);
	npts = UU_LIST_LENGTH(&cutsym->geo->curve);
	nc = *ngpt;
	ist = nc;
	ngeo = cutdef[vp].view.ngeo;
	nptx = 0;
	cdef = (UM_coord *)uu_malloc(sizeof(UM_coord)*npts);
	cnpt = 0.;
/*
.....Store cutter parameters in local variables
*/
	tdiam = cutdef[vp].cutr[5];
	tradius = tdiam / 2.;
	tend[0] = tend[1] = tend[2] = 0.;
	segfl = cutdef[vp].segfl;
	um_vctovc(cutdef[vp].view.vpn,vpn);
	um_vctovc(cutdef[vp].tlax,taxis);
/*
.....Determine if tlaxis is parallel or perpto view normal axis
.....Used for optimizing cutter display
*/
	if (cutdef[vp].seguse) segfl = 2;
	if (shaded != 0) isperp = 1;
	else if (segfl == 2) isperp = 0;
	else isperp = um_vcperp(taxis,vpn);
/*
.....Calculate right vector
*/
	ncl_blade_get_fwd(0,vfd,vr);
/*
.....Offset the profile and
.....adjust for forward direction
*/
	um_vctmsc(vr,tradius,vc1);
	theight = 0.;
	for (i=1;i<npts;i++)
	{
		if (curve[i][1] > theight) theight = curve[i][1];
		um_vctmsc(vfd,curve[i][0],rpt);
		um_vctmsc(taxis,curve[i][1],vc2);
		um_vcplvc(rpt,vc2,rpt);
		um_vcplvc(rpt,vc1,cdef[i-1]);
	}
	cnpt = npts - 1;
	if (um_dcccc(curve[1],curve[npts-1]) > UM_FUZZ)
	{
		um_vctovc(cdef[0],cdef[npts-1]);
		cnpt++;
	}
	nptx = cnpt;
/*
.....Shaded cutter
.....Tessellate the top of the cutter
*/
	if (shaded)
	{
		ncl_tessel_polyline(&curve[1],gpt,gnorm,nptx,&ntess);
		ngeo_tess = 0;
		for (i=0;i<ntess;i++)
		{
			rpt[0] = gpt[i][0]; rpt[1] = tradius; rpt[2] = gpt[i][1];
			ncl_blade_orient(rpt,vfd,vr,taxis,pt2);
			uu_list_push(&cutdef[vp].view.gpt,pt2);
			uu_list_push(&cutdef[vp].view.gnorm,vr);
		}
		j = 3;
		for (i=0;i<ntess;i=i+3)
		{
			uu_list_push(&cutdef[vp].view.npt,&j);
			ngeo++; ngeo_tess++;
		}
		nc = nc + ntess;
	}
/*
.....Store the unshaded profile
.....in the cutter definition
*/
	else
	{
		for (i=0;i<nptx;i++) uu_list_push(&cutdef[vp].view.gpt,cdef[i]);
		uu_list_push(&cutdef[vp].view.npt,&nptx);
		ngeo_tess = 1;
		ntess = nptx;
		nc = nc + nptx;
		ngeo++;
	}
/*
.....Wireframe cutter
........Extrude the cutter shape
*/
	if ((!isperp || segfl == 2) && !shaded)
	{
		um_vctmsc(vr,-tdiam,vc1);
		nptx = ntess;
		for (i=0;i<nptx;i++)
		{
			vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gpt);
			um_vcplvc(&vgpt[ist+i],vc1,pt2);
			uu_list_push(&cutdef[vp].view.gpt,pt2);
			nc++;
		}
		uu_list_push(&cutdef[vp].view.npt,&nptx); ngeo++;
/*
........Calculate the extrusion lines
*/
		nptx = cnpt;
		j = 2;
		for (i=0;i<nptx-1;i++)
		{
			vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gpt);
			uu_list_push(&cutdef[vp].view.gpt,&vgpt[ist+i]); nc++;
			vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gpt);
			uu_list_push(&cutdef[vp].view.gpt,&vgpt[ist+i+nptx]); nc++;
			uu_list_push(&cutdef[vp].view.npt,&j); ngeo++;
		}
	}
/*
.....Shaded cutter
*/
	else if (shaded)
	{
		nptx = ntess;
/*
........Copy top to bottom
*/
		um_vctmsc(vr,-tdiam,vc1);
		for (i=0;i<nptx;i++)
		{
			vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gpt);
			um_vcplvc(&vgpt[ist+i],vc1,pt2);
			uu_list_push(&cutdef[vp].view.gpt,pt2);
			uu_list_push(&cutdef[vp].view.gnorm,vr);
			nc++;
		}
		if (ngeo_tess > 1)
		{
			j = 3;
			for (i=0;i<ngeo_tess;i++)
			{
				uu_list_push(&cutdef[vp].view.npt,&j); ngeo++;
			}
		}
		else
		{
			uu_list_push(&cutdef[vp].view.npt,&nptx); ngeo++;
		}
/*
........Calculate sides of cutter
*/
		um_vctmsc(vr,-tdiam,vc1);
		nptx = cnpt;
		for (i=0;i<nptx-1;i++)
		{
			uu_list_push(&cutdef[vp].view.gpt,cdef[i]);
			um_vcplvc(cdef[i],vc1,pt2);
			uu_list_push(&cutdef[vp].view.gpt,pt2);
			um_vcplvc(cdef[i+1],vc1,pt2);
			uu_list_push(&cutdef[vp].view.gpt,pt2);
			uu_list_push(&cutdef[vp].view.gpt,cdef[i+1]);
/*
........Calculate vectors for flat
........sides of cutter
*/
			vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gpt);
			if (um_plane1(&vgpt[nc],&vgpt[nc+1],&vgpt[nc+2],&pln))
			{
				um_vctovc(pln.n,pt2);
			}
			else
			{
				pt2[0] = 1.; pt2[1] = 0.; pt2[2] = 0.;
			}
			uu_list_push(&cutdef[vp].view.gnorm,pt2);
			uu_list_push(&cutdef[vp].view.gnorm,pt2);
			uu_list_push(&cutdef[vp].view.gnorm,pt2);
			uu_list_push(&cutdef[vp].view.gnorm,pt2);
			nc = nc + 4;
			j = 4;
			uu_list_push(&cutdef[vp].view.npt,&j);
			ngeo++;
		}
	}
/*
.....End of routine
*/
	ofs[2] = ofs[2] + theight;
	*ngpt = nc;
	cutdef[vp].view.ngeo = ngeo;
	status = UU_SUCCESS;
	uu_free(cdef);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_circle(ptr,gpt,npt)
**       Calculates an arc in for display in a cutter symbol.
**    PARAMETERS   
**       INPUT  : 
**				ptr    = Pointer to circle record.
**				npt    = Number of points to calculate.
**       OUTPUT :  
**				gpt    = Points in calculated arc.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_cutter_circle(ptr,gpt,npt)
struct  UM_circle_rec  *ptr;
Gwpoint3 gpt[];
int npt;
{
	int i,n;
	UM_vector yaxis,xcomp,ycomp;
	UM_coord pt,mcpt;
	UM_angle ang,deltang;
	UM_length xlen,ylen;
/*
.....Get view plane of circle
*/
	n = npt - 1;
	um_vctmsc(ptr->svec, ptr->radius, pt);
	um_vcplvc(pt, ptr->center, pt);
	um_cross(ptr->nvec, ptr->svec, yaxis);
/*
.....Store first point
*/
	gpt[0].x = pt[0];
	gpt[0].y = pt[1];
	gpt[0].z = pt[2];
/*
.....Calculate number of points on circle
*/
	deltang = ptr->dang / n;
	ang = deltang;
/*
.....Calculate points around circle
*/
	for (i=0; i<n; i++)
	{
		xlen = ptr->radius * cos(ang);
		um_vctmsc(ptr->svec, xlen, xcomp);
		ylen = ptr->radius * sin(ang);
		um_vctmsc(yaxis, ylen, ycomp);
		um_vcplvc(xcomp, ycomp, pt);
		um_vcplvc(ptr->center, pt, mcpt);
		gpt[i+1].x = mcpt[0];
		gpt[i+1].y = mcpt[1];
		gpt[i+1].z = mcpt[2];
		ang = ang + deltang;
	}
	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : ncl_cutter_nsides(ptr,scal,npt)
**       Calculates the number of points to generate for a circle,
**			dependant on radius and tolerance.
**    PARAMETERS   
**       INPUT  : 
**				ptr    = Pointer to circle record.
**				scal   = Scale of current viewport.
**       OUTPUT :  
**				npt    = Number of points to calculate for circle.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_nsides(ptr,scal,npt)
struct  UM_circle_rec  *ptr;
int *npt;
UU_REAL scal;
{
	UU_REAL prec,cosa,rad,dang;
	UM_angle deltang;
/*
.....Calculate number of points on circle
*/
	if ((!UM_plotting)&&(DRAW_DISPLAY!=1)) 
	{
		if (LW_active) prec = LW_toler;
		else prec = .020;
	}
	else
	{
		prec = UM_plotprec;   
	}
	rad = fabs(ptr->radius) * scal;
	dang = fabs(ptr->dang);
/*
.....Calculate number of points on circle
*/
	um_circle_nsides(rad,dang,prec,npt);
}

/*********************************************************************
**    E_FUNCTION     : ncl_blade_get_fwd(vp,vfd,vr)
**			Calculates the blade's forward and right directions based on
**			the current forward direction and the tool axis.
**    PARAMETERS   
**       INPUT  : 
**				vp     = View port tool is being displayed in.
**       OUTPUT :  
**          vfd    = Blade forward direction.
**          vr     = Blade right direction.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_blade_get_fwd(vp,vfd,vr)
int vp;
UM_vector vfd,vr;
{
	UM_coord tend;
	UM_vector taxis;
	UM_plane pln;
/*
.....Initialize routine
*/
	um_vctovc(cutdef[vp].vfd,vfd);
	um_vctovc(cutdef[vp].tlax,taxis);
	tend[0] = tend[1] = tend[2] = 0.;
/*
.....Adjust forward vector to lie in tool axis plane
.....if hardware rotations are used to position the cutter
*/
	if (cutdef[vp].seguse)
	{
		um_vctovc(taxis,pln.n);
		um_vctovc(tend,pln.p0);
		um_proj_pt_on_plane(1,vfd,&pln,vfd);
		um_unitvc(vfd,vfd);
	}
/*
.....Calculate right vector
*/
	um_cross(vfd,taxis,vr);
	if (um_mag(vr) < UM_DFUZZ)
	{
		if (um_mag(Svr) < UM_DFUZZ)
		{
			um_perpvc(taxis,Svr);
			um_vctmsc(Svr,-1.,Svr);
		}
		um_cross(taxis,Svr,vfd);
		um_cross(vfd,taxis,vr);
	}
	um_unitvc(vr,vr);
	um_vctovc(vr,Svr);
	um_cross(taxis,vr,vfd);
}

/*********************************************************************
**    E_FUNCTION     : ncl_blade_get_orient(pti,vfd,vr,taxis,pto)
**			Translates a point along the forward, right, and tool axis vectors.
**			Used for drawing a blade cutter using a symbol or profile.
**    PARAMETERS   
**       INPUT  : 
**          pti    = Input point.
**          vfd    = Blade forward direction.
**          vr     = Blade right direction.
**          taxis  = Tool axis vector.
**       OUTPUT :  
**          pto    = Adjusted point.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_blade_orient(pti,vfd,vr,taxis,pto)
{
	UM_coord tmp,rpt;
/*
.....Translate point along forward, right, and tool axis vectors
*/
	um_vctovc(pti,tmp);
	um_vctmsc(vfd,tmp[0],rpt);
	um_translate_point(rpt,tmp[1],vr,rpt);
	um_translate_point(rpt,tmp[2],taxis,pto);
}

/*********************************************************************
**    E_FUNCTION     : S_calc_toprad(cutsym,toprad)
**			Calculates the top radius of the provided cutter part based
**       on the stored point list.
**    PARAMETERS   
**       INPUT  : 
**          cutsym = Cutter geometry structure.
**       OUTPUT :  
**          toprad = Top radius of this cutter part.
**    RETURNS      : none
**    SIDE EFFECTS : If the provided cutter part is not defined then
**                   'toprad' is unchanged.
**    WARNINGS     : none
*********************************************************************/
static void S_calc_toprad(cutsym,toprad)
UN_motseg_symbol *cutsym;
UU_REAL *toprad;
{
	int i;
	UM_coord *pts;
/*
.....Loop through points to determine
.....Top radius for this cutter part
*/
	if (cutsym->geo->type != 0)
	{
		pts = (UM_coord *)UU_LIST_ARRAY(&cutsym->geo->curve);
		for (i=0;i<UU_LIST_LENGTH(&cutsym->geo->curve);i++)
		{
			if (pts[i][0] != 0.) *toprad = pts[i][0];
		}
	}
}

