/*********************************************************************
**    NAME  :  mtessel.c
**       CONTAINS: surface tessellation routines
**
**    um_pan_cross_bndr
**    um_pan_cross_bndr1
**    um_point_inside_panel
**    um_to_edge1
**    um_weed_chain
**    um_fit_chain
**    um_chain_cuts_polygon
**    um_panel_ccw_order
**    um_tess_polygon
**    um_merge_tess
**    um_add_tess_points
**    um_find_edge
**    um_create_tess1
**    um_clean_tess1
**    um_free_tess1
**    um_set_tess_toler
**    um_get_tess_toler
**
**
** These functions are not used, they are kept in SCCS91:
**
**  um_uvline_cross_bndry
**  um_uvline_cross_bndry1
**  um_uv_compare
**  um_tessellate_strip
**  um_tess_find_io
**  um_improve_tessellation
**  um_tessellate_polygon
**  um_itsa_cone
**  um_split_edge
**  um_chord
**  um_swap_edges
**  um_swap_edg_nums
**  um_reint_swap
**
**    COPYRIGHT 1998 (c) Numerical Control Computer Sciences Inc.
**                       All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       mtessel.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:14
*********************************************************************/

#include "nccs.h"
#include "ulist.h"
#include "uminmax.h"
#include "mgeom.h"
#include "mdeval.h"
#include "nclfc.h"
#include "mdcoord.h"
#include "ngeom.h"

typedef struct
{
	UU_LOGICAL inner;
	int n1;
	int n2;
	int n3;
} S_tript;

static UU_REAL UM_TESSELLATION_TOLERANCE = 0.;
static UU_LIST chain,v_list,bps,Strilst;
static struct UM_evsrfout Sevsrf;

void um_clean_tess1();

static int um_pan_cross_bndr1();
static void um_weed_chain();
static int S_fit_chain();
static int um_find_edge();

/*********************************************************************
**    FUNCTION : int um_pan_cross_bndr (u0,u1,v0,v1,bound,polygon)
**
**    Finds intersection of a rectangular panel with a srf bndry
**
**    PARAMETERS
**       INPUT  :
**          u0,u1,v0,v1 - the panel
**          bound - surface boundary structure
**       OUTPUT :
**				polygon  - the closed polyline boundary of the surface-panel
**                     intersection, if nontrivial
**    RETURNS      : -1 if panel is outside, 0 if it crosses boundary,
**                    10 if it needs splitting, 1 if it is inside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_pan_cross_bndr (srf,tfmat,u0,u1,v0,v1,bound,polygon,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_REAL u0,u1,v0,v1,tol;
UM_srf_boundary *bound;
UU_LIST *polygon;
{
	int i,inn,inn1,n;
	int *np;
	UM_coord uv,*uvdat,*uvptr;
	UM_2Dcoord pt;
	UU_REAL bxx[2],bxy[2];

	inn = inn1 = -1;

	pt[0] = u0; pt[1] = v0;
	uu_list_push (polygon, &pt);
	pt[0] = u1; pt[1] = v0;
	uu_list_push (polygon, &pt);
	pt[0] = u1; pt[1] = v1;
	uu_list_push (polygon, &pt);
	pt[0] = u0; pt[1] = v1;
	uu_list_push (polygon, &pt);
/*
..... the bps list is used in um_fit_chain
*/
	bps.cur_cnt = 0;
	for (i = 1; i < 4; i++)
		uu_list_push (&bps, &i);

	bxx[0] = u0; bxx[1] = u1;
	bxy[0] = v0; bxy[1] = v1;

	uvdat = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	uvptr = uvdat;
	np = (int *) bound->np;
	n = 0;

	for (i = 0; i < bound->nb; i++)
	{
		uvdat += n;
		n = np[i];
		if (i > 0 &&
		um_isect_boxes (bxx,bxy,bound->ummx[i],bound->vmmx[i],0.001) != 1)
			continue;
		inn1 = um_pan_cross_bndr1 (srf,tfmat,u0,u1,v0,v1,uvdat,n-1,polygon,&bps,tol);
		if (inn1 == 10) return (inn1);
		if (inn1 > inn) inn = inn1;
	}

	if (inn == -1)
	{
		uv[0] = (u0 + u1)/2.;
		uv[1] = (v0 + v1)/2.;
		uv[2] = 0.;
		inn = um_cshape_inschk (uvptr,bound->nb,np,uv,
				bound->ummx,bound->vmmx);
	}

	return (inn);
}

/*********************************************************************
**    FUNCTION : int um_pan_cross_bndr1 (u0,u1,v0,v1,pt,np,polygon,bps)
**
**    Intersects two polygons: the given 'main' one with the
**    one defined by a closed polyline. We assume that the polyline
**    intersects the 'main' polygon at the rectangular panel boundary.
**
**    PARAMETERS
**       INPUT  :
**          u0,u1,v0,v1 - the panel
**          polygon - the main polygon
**          pt     - oriented 2D-boundary of the intersecting polygon
**          np      - number of pts points
**       OUTPUT :
**				polygon  - the resulting intersection
**
**    RETURNS      : -1 if the polygon boundaries do not intersect,
**                    0 if they do
**                   10 if there is more than one polygon
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_pan_cross_bndr1 (srf,tfmat,u0,u1,v0,v1,pt,np,polygon,bps,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_REAL u0,u1,v0,v1,tol;
UM_coord *pt;
int np;
UU_LIST *polygon, *bps;
{
	int i,ibeg,j0,j1,inside,inside0,inn,ichain;
	UM_2Dcoord pt0,pt1,v;
	UU_REAL d,dsq,eps,epsq,tolsq,pantol;
/*
.....Softened tolerance slightly to fix tesselation - Andrew 4/2/13
*/
	if (ncl_setver(96))
		eps = UM_FUZZ;
	else
		eps = 25.e-6;
	if (ncl_setver(97))
		pantol = UM_DFUZZ;
	else
		pantol = eps;
	epsq = eps*eps;
	tolsq = tol*tol;
/*
..... Find the first point not inside - to use as a new start
*/
	for (ibeg = 0; ibeg < np; ibeg++)
	{
		inside = um_point_inside_panel (u0,u1,v0,v1,pt[ibeg],pantol);
		if (inside < 1)
			break;
	}

	if (ibeg >= np-1) return (-1);

	inn = -1;
	ichain = -1;
/*
..... Go over the boundary points until the current segment intersects
..... the panel. The first intersection is the beginning of a chain.
..... Add points to a chain until the next point is outside the
..... panel.
*/
	for (i = 0; i < np; i++)
	{
		UU_LIST_EMPTY(&chain);
		j0 = um_mod (ibeg+i, np);
		j1 = um_mod (j0+1, np);
		inside0 = inside;
		inside = um_point_inside_panel (u0,u1,v0,v1,pt[j1],pantol);

		um_vcmnvc_2d (pt[j1],pt[j0],v);
		dsq = UM_DOT_2D (v,v);

		if (dsq < epsq) continue;

		if (inside <= 0)
		{
			if (fabs (v[0]) < eps && (fabs (pt[j0][0] - u0) < eps ||
				fabs (pt[j0][0] - u1) < eps)) continue;
			if (fabs (v[1]) < eps && (fabs (pt[j0][1] - v0) < eps ||
				fabs (pt[j0][1] - v1) < eps)) continue;
		}

		if (inside0 == 0)
			um_vctovc_2d (pt[j0],pt0);
		else
		{
			if (um_to_edge1 (u0,u1,v0,v1,pt[j0],v,pt0) == 0)
				continue;
		}

		if (inside <= 0)
		{
			if (inside == 0)
				um_vctovc_2d (pt[j1],pt1);
			else
			{
				um_vctmsc_2d (v, -1.0, v);
				if (um_to_edge1 (u0,u1,v0,v1,pt[j1],v,pt1) == 0)
					continue;
			}

			d = um_sqdis_2d (pt0, pt1);
			if (d < epsq) continue;

			uu_list_push (&chain, &pt0);
			uu_list_push (&chain, &pt1);
		}
		else
		{
			uu_list_push (&chain, &pt0);
			while (inside == 1 && i < np)
			{
				uu_list_push (&chain, &pt[j1]);
				i++;
				j0 = j1;
				j1 = um_mod (j1+1, np);
				inside = um_point_inside_panel (u0,u1,v0,v1,pt[j1],pantol);
			}
			um_vcmnvc_2d (pt[j1],pt[j0],v);
			if (um_to_edge1 (u0,u1,v0,v1,pt[j0],v,pt0) == 0)
			{
				if (inside == 0)
					um_vctovc_2d (pt[j1],pt0);
				else
					break;
			}
			uu_list_push (&chain, &pt0);
			um_weed_chain(srf,tfmat,tolsq);
		}
		if (chain.cur_cnt >= 2)
		{
			inn = S_fit_chain (u0,v0,polygon,bps);
			if (inn == 10) goto Done;
							
			if (ichain > -1 && ichain == i-1 && d > epsq && d < tolsq && polygon->cur_cnt < 3)
			{
				inn = 1;
				goto Done;
			}					
			ichain = i;
		}
	}

Done:
	return (inn);
}

/*********************************************************************
**    FUNCTION : int um_point_inside_panel (u0,u1,v0,v1,pt,tol)
**
**    Find whether the point is inside the rectangular panel, within tolerance
**
**    PARAMETERS
**       INPUT  :
**          u0,u1,v0,v1       the panel
**          pt  - the point
**          tol - tolerance
**       OUTPUT :
**          none
**
**    RETURNS  : 1 if inside; 0 if on boundary; -1 if outside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_point_inside_panel (u0,u1,v0,v1,pt,tol)
UU_REAL u0,u1,v0,v1,tol;
UM_coord pt;
{
	UU_REAL x,y;

	x = pt[0]; y = pt[1];
	if ((x-u0) > tol && (u1-x) > tol && (y-v0) > tol && (v1-y) > tol)
		return (1);
	if ((fabs (x-u0) < tol || fabs (x-u1) < tol) &&
		 (y > v0-tol) && (y < v1 + tol))
		return (0);
	if ((fabs (y-v0) < tol || fabs (y-v1) < tol) &&
		 (x > u0-tol) && (x < u1 + tol))
		return (0);

	return (-1);
}

/*********************************************************************
**    FUNCTION : int um_to_edge1 (u0,u1,v0,v1,pt0,v,pti)
**
**    Given a point pt0, a direction vector v, and a rectangular panel
**    [u0,u1]x[v0,v1], extrapolates the point to the closest panel boundary
**    along the direction vector (the main difference with um_to_edge()
**    in m7math2d.c - the point pt0 may not be inside the panel)
**
**    PARAMETERS
**       INPUT  :
**          u0,u1,v0,v1       the panel
**          pt0  - the point
**          v  - direction vector
**       OUTPUT :
**          pti  - the nearest intersection point on the panel boundary
**
**    RETURNS  : 1 if intersection found; 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_to_edge1 (u0,u1,v0,v1,pt0,v,pti)
UU_REAL u0,u1,v0,v1;
UM_coord pt0;
UM_2Dcoord v,pti;
{
	UU_REAL x0,y0,x,y,a,b,t,tmin,eps;

	x0 = pt0[0]; y0 = pt0[1];

	a = v[0]; b = v[1];

	pti[0] = pti[1] = tmin = 10000000.;

	if (ncl_setver(96))
		eps = UM_FUZZ;
	else
		eps = 1.e-5;

	if (fabs(a) > eps)
	{
		t = (u0 - x0)/a;
		if (t > -UM_DFUZZ && t < 1. + UM_DFUZZ)
		{
			x = u0;
			y = y0 + t * b;
			if (y >= v0 - UM_DFUZZ && y <= v1 + UM_DFUZZ)
			{
				tmin = t;
				pti[0] = x; pti[1] = y;
			}
		}
		t = (u1 - x0)/a;
		if (t > -UM_DFUZZ && t < 1. + UM_DFUZZ)
		{
			x = u1;
			y = y0 + t * b;
			if (y >= v0 - UM_DFUZZ && y <= v1 + UM_DFUZZ && t < tmin)
			{
				tmin = t;
				pti[0] = x; pti[1] = y;
			}
		}
	}
	if (fabs(b) > eps)
	{
		t = (v0 - y0)/b;
		if (t > -UM_DFUZZ && t < 1. + UM_DFUZZ)
		{
			y = v0;
			x = x0 + t * a;
			if (x >= u0 - UM_DFUZZ && x <= u1 + UM_DFUZZ && t < tmin)
			{
				tmin = t;
				pti[0] = x; pti[1] = y;
			}
		}
		t = (v1 - y0)/b;
		if (t > -UM_DFUZZ && t < 1. + UM_DFUZZ)
		{
			y = v1;
			x = x0 + t * a;
			if (x >= u0 - UM_DFUZZ && x <= u1 + UM_DFUZZ && t < tmin)
			{
				tmin = t;
				pti[0] = x; pti[1] = y;
			}
		}
	}

	if (tmin < 9000000.)
		return (1);

	if (um_point_inside_panel (u0,u1,v0,v1,pt0,eps) == 0)
	{
		um_vctovc_2d (pt0,pti);
		return (1);
	}
	else
		return (0);
}

/*********************************************************************
**    FUNCTION : int um_fit_chain (u0,v0,polygon,bps)
**
**    Cuts the polygon with a chain (an intersection of a surface
**    boundary with a panel) - and calculates the resulting smaller
**    polygon. The polygon's boundary is CCW, and so it remains after cutting
**
**    PARAMETERS
**       INPUT  :
**          u0,v0       - the lower-left corner of the panel
**          polygon
**          bps         - polygon points on the panel boundary
**       OUTPUT :
**          polygon     - the new polygon
**          bps         - the updated list
**
**    RETURNS  : 0 if OK, 10 if the chain does not cut the polygon
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_fit_chain (u0,v0,polygon,pc,nc,bps)
UU_REAL u0,v0;
UU_LIST *polygon,*bps;
int nc;
UM_2Dcoord *pc;
{
	int i,j,np,nb,next,after;
	int *jbps;
	UM_2Dcoord *pp;
	UU_REAL eps,epsq;
/*
.....Softened tolerance slightly to fix tesselation - Andrew 4/2/13
*/
	if (ncl_setver(96))
		eps = UM_FUZZ;
	else
		eps = 25.e-6;
	epsq = eps*eps;

	np = polygon->cur_cnt;
	pp = (UM_2Dcoord *) UU_LIST_ARRAY (polygon);
	nb = bps->cur_cnt;
	jbps = (int *) UU_LIST_ARRAY (bps);
/*
..... the order on a panel is: the first point is (u0,v0), then points are
..... CCW-ordered.
.....
..... At the first call with any panel: np = 4, the polygon consists of the four
..... panel corners in order; nb = 3, jbps[0]=1,jbps[1]=2,jbps[2]=3
.....
..... polygon points are always ordered - before and after.
..... jbps[0] is always the second polygon point on the panel boundary,
..... jbps[1] is the third one, etc.
*/
	if (um_sqdis_2d(pc[0],pp[0]) < epsq)
		um_vctovc_2d (pp[0], pc[0]);

	if (um_panel_ccw_order(u0,v0,pc[nc-1],pc[0],eps) == 1)
	{
/*
..... If the last chain point is after the first one
*/
		for (i = 0; i < nb; i++)
		{
			if (um_panel_ccw_order(u0,v0,pp[jbps[i]],pc[0],eps) >= 0)
				break;
		}
/*
..... find first panel point after the beginning of the chain
*/
		if (i >= nb)
		{
/*
..... if not found, just push the chain on top
*/
			if (um_sqdis_2d(pc[nc-1],pp[0]) < epsq)
				um_vctovc_2d (pp[0], pc[nc-1]);
			uu_list_push_multiple (polygon,nc,pc);
			uu_list_push (bps, &np);
			next = polygon->cur_cnt - 1;
			uu_list_push (bps, &next);
		}
		else
		{
			if (um_sqdis_2d(pc[0],pp[jbps[i]]) < epsq)
				um_vctovc_2d (pp[jbps[i]], pc[0]);

			for (j = i; j < nb; j++)
			{
				after = um_panel_ccw_order(u0,v0,pp[jbps[j]],pc[nc-1],eps);
				if (after == 1 || (j == nb-1 && after == 0 && jbps[j]+1 < np))
					break;
			}
/*
..... find first panel point after the last point of the chain
*/
			if (j >= nb)
			{
/*
..... if not found, just push the chain on top
*/
				if (um_sqdis_2d(pc[nc-1],pp[0]) < epsq)
					um_vctovc_2d (pp[0], pc[nc-1]);
				polygon->cur_cnt = jbps[i];
				bps->cur_cnt = i + 1;
				uu_list_push_multiple (polygon,nc,pc);
				next = polygon->cur_cnt - 1;
				uu_list_push (bps, &next);
			}
			else
			{
				int jcut, ncut;
/*
..... replace a piece by the chain
*/
				if (j > 0 &&
					um_sqdis_2d(pc[nc-1],pp[jbps[j-1]]) < epsq)
				{
					after = 0;
					um_vctovc_2d (pp[jbps[j-1]], pc[nc-1]);
					j--;
				}
				ncut = jbps[j] - jbps[i];
				if (after == 0) ncut++;
				if (ncut > 0)
					uu_list_delete (polygon, jbps[i], ncut);
				jcut = j - i - 1;
				if (jcut > 0)
				{
					uu_list_delete (bps, i+1, jcut);
					jbps = (int *) UU_LIST_ARRAY (bps);
				}
				uu_list_insert_multiple (polygon, jbps[i], nc,pc);

				pp = (UM_2Dcoord *) UU_LIST_ARRAY (polygon);

				if (jcut == -1)
				{
					jcut = jbps[i];
					uu_list_insert (bps, i, &jcut);
					jbps = (int *) UU_LIST_ARRAY (bps);
				}

				next = jbps[i] + nc - 1;
				uu_list_insert (bps, i+1, &next);
				jbps = (int *) UU_LIST_ARRAY (bps);

				nb = bps->cur_cnt;
				for (j = i+2; j < nb; j++)
					jbps [j] += (nc - ncut);

				if (i == 0 &&
						um_panel_ccw_order(u0,v0,pc[0],pp[0],eps) <= 0)
				{
					uu_list_delete (polygon, 0, 1);
					uu_list_delete (bps, 0, 1);
					jbps = (int *) UU_LIST_ARRAY (bps);
					for (i = 0; i < bps->cur_cnt; i++)
						jbps [i]--;
				}
			}
		}
	}
	else
	{
/*
..... If the last chain point is before the first one
*/
		int found = 0;
/*
..... cut off the piece after the first chain point
*/
		for (j = nb - 1; j >= 0; j--)
		{
			if (um_panel_ccw_order(u0,v0,pc[0],pp[jbps[j]],eps) > 0)
				break;
		}
		if (j+1 < nb)
		{
			if (um_sqdis_2d(pc[0],pp[jbps[j+1]]) < epsq)
				um_vctovc_2d (pp[jbps[j+1]], pc[0]);

			np = polygon->cur_cnt = jbps[j+1];
			nb = bps->cur_cnt = j+1;
		}
/*
..... delete the piece before the last chain point
*/
		if (um_sqdis_2d(pc[nc-1],pp[jbps[0]]) < epsq)
		{
			j = 0;
		}
		else
		{
			for (j = 0; j < nb; j++)
			{
				if (um_panel_ccw_order(u0,v0,pp[jbps[j]],pc[nc-1],eps) > 0)
				{
					found = 1; break;
				}
			}
		}
		if (j > 0)
		{
			if (um_sqdis_2d(pc[nc-1],pp[jbps[j-1]]) < epsq)
			{
				um_vctovc_2d (pp[jbps[j-1]], pc[nc-1]);
				if (found == 1)
				{
					j--; found = 2;
				}
			}

			if (found == 1)
				next = jbps[j] - 1;
			else if (found == 2)
			{
				next = jbps[j]; found = 1; j++;
			}
			else
				next = jbps[nb-1];
			uu_list_delete (polygon, 1, next);
			pp = (UM_2Dcoord *) UU_LIST_ARRAY (polygon);
			np -= next;
			if (found == 1)
			{
			 	uu_list_delete (bps, 0, j);
				jbps = (int *) UU_LIST_ARRAY (bps);
				nb = bps->cur_cnt;
			 	for (j = 0; j < nb; j++)
				 	jbps[j] -= next;
			}
			else
				bps->cur_cnt = 0;
		}
/*
..... replace the first polygon point with the last chain point
*/
		if (um_panel_ccw_order(u0,v0,pc[nc-1],pp[0],eps) > 0)
			um_vctovc_2d (pc[nc-1],pp[0]);
/*
..... add the rest of the chain at the end of the polygon
*/
		uu_list_push (bps, &np);
		uu_list_push_multiple (polygon, nc-1, pc);
	}
}

/*********************************************************************
**    FUNCTION : int um_chain_cuts_polygon(np,pp,nc,pc)
**
**    Determines if a chain (a UV-polyline) intersects a polygon (a UV-polyline
**    made closed by joining its first point with its last)
**
**    PARAMETERS
**       INPUT  :
**          np - number of points in polygon
**          pp - the polygon points
**          nc - number of points in chain
**          pc - the chain points
**       OUTPUT :
**          none
**
**    RETURNS  : 1 if chain does cut polygon;
**               0 if both chain endpoints are on polygon boundary;
**              -1 if chain does not cut polygon
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL um_chain_cuts_polygon(np,pp,nc,pc)
int nc,np;
UM_2Dcoord *pp, *pc;
{
	UM_2Dcoord pcmid;
	int i,j,ins;
	UU_REAL eps;

	if (ncl_setver(96))
		eps = UM_FUZZ;
	else
		eps = 1.e-5;

	if (nc < 1) return (UU_TRUE);
/*
..... we just check one chain "middlepoint" - whether it is inside, outside,
..... or on boundary.
*/
	i = (nc - 1)/2;
	j = nc/2;
	um_middlept_2d (pc[i],pc[j],pcmid);

	ins = um_check_inside(pp,np,pcmid,UU_NULL,eps);

	if (ins == -1)
		return (UU_FALSE);
	else
		return (UU_TRUE);
}

/*********************************************************************
**    FUNCTION : int S_fit_chain (u0,v0,polygon,bps)
**
**    Cuts the polygon with a chain (an intersection of a surface
**    boundary with a panel) - and calculates the resulting smaller
**    polygon. The polygon's boundary is CCW, and so it remains after cutting
**
**    PARAMETERS
**       INPUT  :
**          u0,v0       - the lower-left corner of the panel
**          polygon
**          bps         - polygon points on the panel boundary
**       OUTPUT :
**          polygon     - the new polygon
**          bps         - the updated list
**
**    RETURNS  : 0 if OK, 10 if the chain does not cut the polygon
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_fit_chain (u0,v0,polygon,bps)
UU_REAL u0,v0;
UU_LIST *polygon, *bps;
{
	int nc,np;
	UM_2Dcoord *pp, *pc;

	np = polygon->cur_cnt;
	pp = (UM_2Dcoord *) UU_LIST_ARRAY (polygon);
	nc = chain.cur_cnt;
	pc = (UM_2Dcoord *) UU_LIST_ARRAY (&chain);
/*
..... If the chain does not cut the current polygon, we have more than one polygon
..... in the panel. So we go back and make smaller panels, until each panel produces
..... no more than one polygon.
*/
	if (um_chain_cuts_polygon(np,pp,nc,pc) == UU_FALSE)
		return (10);
	else
	{
		um_fit_chain (u0,v0,polygon,pc,nc,bps);
		return (0);
	}
}

/*********************************************************************
**    E_FUNCTION     : int um_panel_ccw_order(u0,v0,a,b,tol)
**       Order points on the boundary of a 2D-panel, oriented CCW,
**       with the starting position at the lower left corner.
**    PARAMETERS
**       INPUT  :
**          a,b               2D points to compare
**          u0,v0             the lower left corner of the panel
**          tol               tolerance
**       OUTPUT :
**          none
**    RETURNS      :
**          -1    if a < b
**           0    if a = b
**           1    if a > b
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_panel_ccw_order(u0,v0,a,b,tol)
UU_REAL u0,v0,tol;
UM_2Dcoord a,b;
{
	UU_REAL x,y,s,t,tolsq,d;
	UM_2Dcoord origin;

	tolsq = tol*tol;

	d = um_sqdis_2d(a,b);
	if (d < tolsq) return (0);

	origin[0] = u0; origin[1] = v0;
	d = um_sqdis_2d(a,origin);
	if (d < tolsq) return (-1);
	d = um_sqdis_2d(b,origin);
	if (d < tolsq) return (1);

	x = a[0]; y = a[1];
	s = b[0]; t = b[1];

	if (fabs(x - u0) < tol && fabs(s - u0) < tol)
	{
		if (t > y) return (1);
		else return (-1);
	}
	else if (fabs(y - v0) < tol && fabs(t - v0) < tol)
	{
		if (x > s) return (1);
		else return (-1);
	}
	else
	{
		d = um_triangle_signed_area (origin,b,a);
		if (d > 0.) return (1);
		else return (-1);
	}
}

/*********************************************************************
**    FUNCTION : void um_weed_chain96()
**      Weed 2-D polyline points, within UM_FUZZ
*********************************************************************/
void um_weed_chain96()
{
	int i, nn;
	UM_2Dcoord prev;
	UM_2Dcoord *pt;
	UU_REAL eps,epsq;
	UU_REAL d,d1,del,co,co2,si2;
	UM_2Dcoord v0,v1;
	UU_LOGICAL ldel;

	nn = chain.cur_cnt;
	if (nn < 2) return;

	eps = UM_FUZZ;
	epsq = eps*eps;

	pt = (UM_2Dcoord *) UU_LIST_ARRAY (&chain);
	um_vctovc_2d (pt[0],prev);
/*
..... do not attempt to weed the last pt
*/
	for (i = 1; i < nn-1; i++)
	{
		ldel = UU_FALSE;
/*
..... determine whether to delete a point depending on the distance from the
..... segment between the previous and the next
*/
		um_vcmnvc_2d (pt[i], prev, v0);
		d = UM_DOT_2D (v0,v0);
		if (d < epsq)
			ldel = UU_TRUE;
		else
		{
			um_vcmnvc_2d (pt[i+1], pt[i], v1);
			d1 = UM_DOT_2D (v1,v1);
			if (d1 > epsq)
			{
				d = sqrt(d);
				if (d < 10.*eps)
				{
					um_unitvc_2d (v0, v0);
					um_unitvc_2d (v1, v1);
					co = UM_DOT_2D (v0, v1);
					if (co > 0.866)
					{
						co2 = sqrt ((co + 1.)/2.);
						si2 = sqrt ((1. - co)/2.);
						if (si2 < UM_FUZZ)
							ldel = UU_TRUE;
						else
						{
							del = (d * (1. - co2))/(2. * si2);
							if (del < eps) ldel = UU_TRUE;
						}
					}
				}
			}
			else
				if (i == nn-2) ldel = UU_TRUE;
		}

		if (ldel)
		{
			uu_list_delete (&chain,i,1);
			pt = (UM_2Dcoord *) UU_LIST_ARRAY (&chain);
			i--; nn--;
		}
		else
			um_vctovc_2d (pt[i],prev);
	}

	pt = (UM_2Dcoord *) UU_LIST_ARRAY (&chain);
	if (chain.cur_cnt == 2 && um_sqdis_2d(pt[0],pt[1]) < epsq)
			chain.cur_cnt = 0;
}

/*********************************************************************
**  I_FUNCTION : S_evpt(uv,srf,tfmat,pt)
**
**      Evaluate point on surface
**
*********************************************************************/
static int S_evpt(srf,tfmat,uv,pp)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_2Dcoord uv;
UM_coord pp;
{
	int j,istat;
	UM_2Dcoord uvi;

	um_vctovc_2d (uv,uvi);
	if (ncl_scaled()) ncl_rescale_uv (uvi);
	for (j = 0; j < 2; j++)
	{
		if (uvi[j] < -0.05 || uvi[j] > 1.05)
			return (UU_FAILURE);
		if (uvi[j] < 0.) uvi[j] = 0.;
		else if (uvi[j] > 1.) uvi[j] = 1.;
	}

	istat = uc_evsrf(UM_POINT, uvi[0], uvi[1], srf, tfmat, &Sevsrf);
	if (istat == 0) um_vctovc (Sevsrf.sp,pp);
	return (istat);
}

/*********************************************************************
**    FUNCTION : void um_weed_chain (tol)
**
**      Weed 2-D polyline points, within tolerance
**
**    PARAMETERS
**       INPUT  :
**          chain  - a polyline (now global)
**          tol    - tolerance
**       OUTPUT :
**          weeded chain
**    RETURNS : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_weed_chain (srf,tfmat,tolsq)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_REAL tolsq;
{
	int i,istat,nn;
	UM_2Dcoord *pt;
	UU_REAL eps,d0,d1,d2,co,a,dd0,dd1,dd2,cco,aa;
	UM_2Dcoord vc0,vc1,vc2;
	UM_coord pp0,pp1,pp2;
	UM_vector vv0,vv1,vv2;
	UU_LOGICAL ldel;

	nn = chain.cur_cnt;
	if (nn < 2) return;

	if (ncl_setver(96))
	{
		um_weed_chain96(); return;
	}

 	eps = 1.e-10;

	pt = (UM_2Dcoord *) UU_LIST_ARRAY (&chain);
/*
..... do not attempt to weed the last pt
*/
	for (i = 1; i < nn-1; i++)
	{
		ldel = UU_FALSE;
/*
..... determine whether to delete a point depending on the distance from the
..... segment between the previous and the next - 2D, then 3D if needed
*/
		um_vcmnvc_2d (pt[i], pt[i-1], vc0);
		d0 = UM_DOT_2D (vc0,vc0);
		if (d0 < eps)
			ldel = UU_TRUE;
		else
		{
			um_vcmnvc_2d (pt[i+1], pt[i], vc1);
			d1 = UM_DOT_2D (vc1,vc1);
			if (d1 < eps && i == nn-2)
				ldel = UU_TRUE;
			else
			{
				co = UM_DOT_2D (vc0, vc1);
				if (co > 0)
				{
					a = d0*d1 - co*co;
					um_vcmnvc_2d (pt[i+1], pt[i-1], vc2);
					d2 = UM_DOT_2D (vc2,vc2);
					if (d2 > eps && a < d2*eps)
						ldel = UU_TRUE;
					else if (co*co > 0.5*d0*d1)
					{
						istat = S_evpt(srf,tfmat,pt[i-1],pp0);
						if (istat != 0) return;
						istat = S_evpt(srf,tfmat,pt[i],pp1);
						if (istat != 0) return;
						istat = S_evpt(srf,tfmat,pt[i+1],pp2);
						if (istat != 0) return;

						um_vcmnvc (pp1, pp0, vv0);
						dd0 = UM_DOT (vv0,vv0);
						um_vcmnvc (pp2, pp1, vv1);
						dd1 = UM_DOT (vv1,vv1);

						if (dd0 < tolsq || (dd1 < tolsq && i == nn-2))
							ldel = UU_TRUE;
						else
							cco = UM_DOT (vv0, vv1);
						if (cco > 0)
						{
							aa = dd0*dd1 - cco*cco;
							um_vcmnvc (pp2, pp0, vv2);
							dd2 = UM_DOT (vv2,vv2);
							ldel = (dd2 > tolsq && aa < dd2*tolsq);
						}
					}
				}
			}
		}

		if (ldel)
		{
			uu_list_delete (&chain,i,1);
			pt = (UM_2Dcoord *) UU_LIST_ARRAY (&chain);
			i--; nn--;
		}
	}

	if (nn == 2)
	{
		pt = (UM_2Dcoord *) UU_LIST_ARRAY (&chain);

		istat = S_evpt(srf,tfmat,pt[0],pp0);
		if (istat != 0) return;
		istat = S_evpt(srf,tfmat,pt[1],pp1);
		if (istat != 0) return;

		dd0 = um_sqdis(pp0,pp1);
		if (dd0 < eps) UU_LIST_EMPTY(&chain);
	}
}

/*********************************************************************
**   FUNCTION : int um_tess_polygon_96 (srf,tfmat,polygon,tess,tol)
**
**      Tessellates a simple (non-self-intersecting) 2D-polygon
**
**    PARAMETERS
**       INPUT  :
**          srf       - Surface being tessellated.  If 'srf' is UU_NULL,
**                      then the 2-D polygon is an XY-plane set of points.
**          tfmat     - Surface transformation matrix.
**          polygon   - list of polygon vertices
**          tol       - tolerance to generate tessellation.
**       OUTPUT :
**          tess - resulting tessellation
**    RETURNS :
**             UU_SUCCESS/UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_tess_polygon_96 (srf,tfmat,polygon,tess,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_LIST *polygon;
UM_tess1 *tess;
UU_REAL tol;
{
	UU_LIST *edges;
	UM_edge edge,*edge0;
	UM_2Dcoord *pt,p1,p2,p3,p4,w1,w3;
	int np,count,i,i1,i2,i3,imin,nver,eprev,enext;
	int *n;
	UU_REAL tmin,turn,dmin,d,area;
	UU_LOGICAL bad_triangle, check_angle, check_ang1, lv95, lv96;
	int status = UU_SUCCESS;

	lv96 = ncl_setver(96);
	np = UU_LIST_LENGTH (polygon);
	if (np < 3) return (status);

	pt = (UM_2Dcoord *) UU_LIST_ARRAY (polygon);

	if (np == 3)
	{
		area = um_triangle_signed_area (pt[0],pt[1],pt[2]);
		if (lv96 && area < UM_DFUZZ) return (status);
	}
/*
... polygon vertices are numbered from 0 to np-1;
...
... sense = +1 if the polygon is oriented CCW; -1 if CW.
... Enforce CCW by reversing order
	sense = um_polygon_orientation (np, pt);
	if(sense < 0) um_reverse_array_2d (np,pt);
*/
	if (UU_LIST_LENGTH(&tess->uv) == 0 && UU_LIST_SMALL (&tess->uv,np))
	{
		UU_LIST_SETEXP (&tess->vertices,np);
		UU_LIST_SETEXP (&tess->normals,np);
		UU_LIST_SETEXP (&tess->uv,np);
		UU_LIST_SETEXP (&tess->edges,2*np);
	}

	status = um_add_tess_points_96 (srf,tfmat,&np,pt,tess);
	if (status != UU_SUCCESS) return (UU_FAILURE);
	if (np == 3)
		area = um_triangle_signed_area (pt[0],pt[1],pt[2]);
/*
... init v_list: (0,1,2, ..., np-1)
... and write edges of the perimeter to the edges list;
*/
	UU_LIST_EMPTY (&v_list);
	if (UU_LIST_SMALL (&v_list,np))	UU_LIST_SETEXP (&v_list,np);

	edges = &tess->edges;

	for (i1=0; i1<np; i1++)
	{
		i2 = um_mod (i1 + 1, np);
		i3 = um_mod (i1 - 1, np);
		edge.south = i1;
		edge.north = i2;
		edge.s_e = edge.n_e = -1;
		edge.n_w = i2;
		edge.s_w = i3;

		uu_list_push (edges, &edge);
		uu_list_push (&v_list, &i1);
	}
/*
... During each pass of the while(nver>3) loop, the polygon is updated
... as follows:
...
... 1. Find triplet of 3 successive vertices (i1,i2,i3) of the current
...    polygon such that:
...      a. angle (i1,i2,i3) is convex;
...      b. there are no vertices of the current polygon inside triangle
...         (i1,i2,i3);
...      c. angle i1,i2,i3 is not close to PI.
...         (this improves quality of tessellation)
...      d. chordal deviation of surface between i1 & i3 vertices
...         is min of all triplets satisfying a,b,c
...      e. if there are no triangles satisfying a+b+c, this pass will be
...         repeated again without condition c.
... 2. When such triplet is found, cut the angle out:
...      a. make new edge (i1,i3);
...      b. delete vertex i2 from v_list (list of vertices of the current
...         polygon);
...      c. go to step 1 with the updated polygon (without vertex i2);
...
... Repeat until the original polygon is reduced to triangle;
*/

	nver = UU_LIST_LENGTH (&v_list);
	if (nver > 3) area = 0.;
	lv95 = ncl_setver(95);

	while ( nver > 3 )
	{
		check_angle = UU_TRUE;
/*
... cyclic enumeration of vertices: 0 1 2 ... (np-1) 0 1 2 ... (np-1) ...
... n[i] is the number of the vertex of the original polygon
... (from 0,1,2,... np-1 ) that is now vertex # i of the updated polygon
... ( 0 <= i <= nver-1 )
*/
Again:;
		n = (int *) UU_LIST_ARRAY (&v_list);
		check_ang1 = (!lv95 && check_angle && nver == 4);

		imin = -1;
		dmin = 1.e+10;
		for (i1 = 0; i1 < nver; i1++)
		{
			i2 = um_mod (i1+1, nver);
			i3 = um_mod (i2+1, nver);
			um_vctovc_2d(pt[n[i1]],p1);
			um_vctovc_2d(pt[n[i2]],p2);
			um_vctovc_2d(pt[n[i3]],p3);

			if (lv95)
			{
				UM_2Dcoord *pp1,*pp3;

				pp1 = pt + n[i1]; pp3 = pt + n[i3];
				d = UM_DIST_2D (pp1,pp3);
			}
			else
				d = um_sqdis_2d (p1,p3);

			if (d >= dmin) continue;
/*
... if turn <= 0 -> angle (i1,i2,i3) is concave; go to the next angle
... if turn > 0 -> angle (i1,i2,i3) is convex and is a candidate to be
... cut out.
*/
			turn = um_triangle_signed_area (p1,p2, p3);
			if (check_angle && turn <= 0. || turn < -1.e-12) continue;
/*
... first, try to avoid triangles with middle angle close to 180 degrees;
... if there is no triangle satisfying this (unnecessary) condition and
... the turn (necessary) condition, go over this polygon again
... with check_angle = 0;
*/
			bad_triangle = UU_FALSE;
			if (check_angle)
			{
				um_vcmnvc_2d (p1,p2,w1);
				um_vcmnvc_2d (p3,p2,w3);
				um_unitvc_2d (w1,w1); um_unitvc_2d (w3,w3);
				bad_triangle = (UM_DOT_2D (w1,w3) < -0.98);
			}
			if (bad_triangle) continue;
/*
... make sure that there are no points of the current polygon inside the
... triangle (i1,i2,i3):
*/
			for (i = 0; i < nver && !bad_triangle; i++)
			{
				if ( (i==i1) || (i==i2) || (i==i3) ) continue;
				bad_triangle =
					um_point_isin_triangle (pt[n[i]],p1,p2,p3,UM_DFUZZ);
/*
..... if only four points left, check the angle on the other side too
*/
				if (check_ang1 && !bad_triangle)
				{
					um_vctovc_2d(pt[n[i]],p4);
					um_vcmnvc_2d (p1,p4,w1);
					um_vcmnvc_2d (p3,p4,w3);
					um_unitvc_2d (w1,w1); um_unitvc_2d (w3,w3);
					bad_triangle = (UM_DOT_2D (w1,w3) < -0.98);
				}
			}
/*
... if the triangle is empty, cut vertex i2 out.
*/
			if (!bad_triangle)
			{
				imin = i1;
				dmin = d;
				tmin = turn;
			}
		}

		if (imin < 0)
		{
			if (!check_angle) goto Done;
			else
			{
				check_angle = UU_FALSE;
				goto Again;
			}
		}

		i1 = imin;
		i2 = um_mod(i1+1, nver);
		i3 = um_mod(i2+1, nver);
		count = UU_LIST_LENGTH(edges);

		area += tmin;

		edge0 = (UM_edge *) UU_LIST_ARRAY(edges);
		edge.south = n[i1];
		edge.north = n[i3];

		edge.s_e = um_find_edge (n[i1],n[i2], edges);
		edge.n_e = um_find_edge (n[i2],n[i3], edges);

		eprev = (edge0 + (int)edge.s_e)->s_w;
		enext = (edge0 + (int)edge.n_e)->n_w;
		edge.s_w = eprev;
		edge.n_w = enext;

		(edge0 + (int)edge.s_e)->s_w = count;
		(edge0 + (int)edge.n_e)->n_w = count;
		(edge0 + (int)eprev)->n_w = count;
		(edge0 + (int)enext)->s_w = count;

		uu_list_push (edges, &edge);
		uu_list_delete (&v_list,i2,1);

		nver = UU_LIST_LENGTH(&v_list);

	}  /* while */

Done:;

	if (nver < 4)
	{
		status = UU_SUCCESS;
		if (lv96 && area < UM_DFUZZ) um_clean_tess1 (tess);
		tess->np = UU_LIST_LENGTH (&tess->vertices);
		tess->nedges = UU_LIST_LENGTH (&tess->edges);
	}
	else
		status = UU_FAILURE;

	return (status);
}

/*********************************************************************
**   I-FUNCTION : void S_arrange (trian,np)
**
**      Rearrange triangle vertices, so that the first two form an edge of the
**      current polygon
**
**    PARAMETERS
**       INPUT  :
**          trian     - triangle
**          np        - total number of points in the polygon
**       OUTPUT :
**          trian     - resulting triangle
**    RETURNS : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_arrange (trian,np)
S_tript *trian;
int np;
{
	int is,nx;

	is = np;
/*
..... Find a vertex such that the next one is also a vertex, and so they form
..... an edge. If several such, find the smallest
*/
	nx = (trian->n1 + 1)%np;
	if (nx == trian->n2)
		is = trian->n1;

	nx = (trian->n2 + 1)%np;
	if (nx == trian->n3)
	{
		if (trian->n2 < is) is = trian->n2;
	}
	
	nx = (trian->n3 + 1)%np;
	if (nx == trian->n1)
	{
		if (trian->n3 < is) is = trian->n3;
	}

	trian->inner = (is == np);
	if (trian->inner)
	{
		is = trian->n1;
		if (trian->n2 < is) is = trian->n2;
		if (trian->n3 < is) is = trian->n3;
	}

/*
..... Renumber the vertices if needed, so that the found vertex is first.
*/
	if (is == trian->n2)
	{
		is = trian->n1;
		trian->n1 = trian->n2;
		trian->n2 = trian->n3;
		trian->n3 = is;
	}
	else if (is == trian->n3)
	{
		is = trian->n3;
		trian->n3 = trian->n2;
		trian->n2 = trian->n1;
		trian->n1 = is;
	}
}

/*********************************************************************
**   I-FUNCTION : void S_add (trian)
**
**      Insert a triangle into a list, sorted by the first vertex number.
**
**    PARAMETERS
**       INPUT  :
**          trian     - triangle
**          Strilst   - current list of triangles
**       OUTPUT :
**          Strilst     - updated list
**    RETURNS : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_add (trian)
S_tript *trian;
{
	int i,ntri;
	S_tript *tri;

	tri = (S_tript *) UU_LIST_ARRAY (&Strilst);
	ntri = UU_LIST_LENGTH(&Strilst);

	for (i = 0; i < ntri; i++)
	{
		if ((trian->inner == UU_FALSE && tri[i].inner == UU_TRUE) ||
		    (trian->inner == tri[i].inner && trian->n1 < tri[i].n1))
		{
			uu_list_insert (&Strilst,i,trian);
			return;
		}
	}

	uu_list_push (&Strilst,trian);
}

/*********************************************************************
**   FUNCTION : int um_tess_polygon (srf,tfmat,polygon,tess,tol)
**
**      Tessellates a simple (non-self-intersecting) 2D-polygon
**
**    PARAMETERS
**       INPUT  :
**          srf       - Surface being tessellated.  If 'srf' is UU_NULL,
**                      then the 2-D polygon is an XY-plane set of points.
**          tfmat     - Surface transformation matrix.
**          polygon   - list of polygon vertices
**          uvtes     - list for tessellation UV-points, of NULLST
**          tol       - tolerance to generate tessellation.
**       OUTPUT :
**          tess  - resulting tessellation
**          uvtes - tessellation UV-points if list is provided NULLST
**    RETURNS :
**             UU_SUCCESS/UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_tess_polygon (srf,tfmat,polygon,tess,uvtes,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_LIST *polygon;
UM_tessellation *tess;
UU_LIST *uvtes;
UU_REAL tol;
{
	UM_2Dcoord *pt,p1,p2,p3,p4,w1,w3;
	UM_tript tript;
	S_tript trian;
	S_tript *tri;
	int np0,np,i,i1,i2,i3,imin,nver,ntri;
	int *n;
	UU_REAL tmin,turn,dmin,d,area;
	UU_LOGICAL bad_triangle, check_angle, check_ang1;
	int status = UU_SUCCESS;

	np = UU_LIST_LENGTH (polygon);
	if (np < 3) return (status);

	pt = (UM_2Dcoord *) UU_LIST_ARRAY (polygon);

	if (np == 3)
	{
		area = um_triangle_signed_area (pt[0],pt[1],pt[2]);
	}
/*
... polygon vertices are numbered from 0 to np-1;
...
... sense = +1 if the polygon is oriented CCW; -1 if CW.
... Enforce CCW by reversing order
	sense = um_polygon_orientation (np, pt);
	if(sense < 0) um_reverse_array_2d (np,pt);
*/
	np0 = UU_LIST_LENGTH (&tess->vertices);
	if (np0 == 0 && UU_LIST_SMALL (&tess->vertices,np))
	{
		UU_LIST_SETEXP (&tess->vertices,np);
		UU_LIST_SETEXP (&tess->normals,np);
		if (uvtes != NULLST)
		UU_LIST_SETEXP (uvtes,np);
		UU_LIST_SETEXP (&tess->tri,np);
	}

	status = um_add_tess_points (srf,tfmat,&np,pt,tess,uvtes);
	if (status != UU_SUCCESS) return (UU_FAILURE);
/*
... init v_list: (0,1,2, ..., np-1)
... and write edges of the perimeter to the edges list;
*/
	UU_LIST_EMPTY (&v_list);
	if (UU_LIST_SMALL (&v_list,np)) UU_LIST_SETEXP (&v_list,np);

	for (i1 = 0; i1 < np; i1++)
	{
		uu_list_push (&v_list, &i1);
	}
/*
... During each pass of the while(nver>3) loop, the polygon is updated
... as follows:
...
... 1. Find triplet of 3 successive vertices (i1,i2,i3) of the current
...    polygon such that:
...      a. angle (i1,i2,i3) is convex;
...      b. there are no vertices of the current polygon inside triangle
...         (i1,i2,i3);
...      c. angle i1,i2,i3 is not close to PI.
...         (this improves quality of tessellation)
...      d. chordal deviation of surface between i1 & i3 vertices
...         is min of all triplets satisfying a,b,c
...      e. if there are no triangles satisfying a+b+c, this pass will be
...         repeated again without condition c.
... 2. When such triplet is found, cut the angle out:
...      a. make new edge (i1,i3);
...      b. delete vertex i2 from v_list (list of vertices of the current
...         polygon);
...      c. go to step 1 with the updated polygon (without vertex i2);
...
... Repeat until the original polygon is reduced to triangle;
*/

	nver = UU_LIST_LENGTH (&v_list);
	area = 0.;

	UU_LIST_EMPTY (&Strilst);
	if (UU_LIST_SMALL (&Strilst,np)) UU_LIST_SETEXP (&Strilst,np);

	while (nver > 3)
	{
		check_angle = UU_TRUE;
		n = (int *) UU_LIST_ARRAY (&v_list);
		check_ang1 = (check_angle && nver == 4);

		imin = -1;
		dmin = 1.e+10;
/*
... cyclic enumeration of vertices: 0 1 2 ... (np-1) 0 1 2 ... (np-1) ...
... n[i] is the number of the vertex of the original polygon
... (from 0,1,2,... np-1 ) that is now vertex # i of the updated polygon
... ( 0 <= i <= nver-1 )
*/
		while (imin < 0)
		{
			check_ang1 = (check_angle && nver == 4);

			for (i1 = 0; i1 < nver; i1++)
			{
				i2 = um_mod (i1+1, nver);
				i3 = um_mod (i2+1, nver);
				um_vctovc_2d(pt[n[i1]],p1);
				um_vctovc_2d(pt[n[i2]],p2);
				um_vctovc_2d(pt[n[i3]],p3);

				d = um_sqdis_2d (p1,p3);

				if (d >= dmin) continue;
/*
... if turn <= 0 -> angle (i1,i2,i3) is concave; go to the next angle
... if turn > 0 -> angle (i1,i2,i3) is convex and is a candidate to be
... cut out.
*/
				turn = um_triangle_signed_area (p1,p2, p3);
				if (check_angle && turn <= 0. || turn < -1.e-12) continue;
/*
... first, try to avoid triangles with middle angle close to 180 degrees;
... if there is no triangle satisfying this (unnecessary) condition and
... the turn (necessary) condition, go over this polygon again
... with check_angle = 0;
*/
				bad_triangle = UU_FALSE;
				if (check_angle)
				{
					um_vcmnvc_2d (p1,p2,w1);
					um_vcmnvc_2d (p3,p2,w3);
					um_unitvc_2d (w1,w1); um_unitvc_2d (w3,w3);
					bad_triangle = (UM_DOT_2D (w1,w3) < -0.98);
				}
				if (bad_triangle) continue;
/*
... make sure that there are no points of the current polygon inside the
... triangle (i1,i2,i3):
*/
				for (i = 0; i < nver && !bad_triangle; i++)
				{
					if ( (i==i1) || (i==i2) || (i==i3) ) continue;
					bad_triangle =
						um_point_isin_triangle (pt[n[i]],p1,p2,p3,UM_DFUZZ);
/*
..... if only four points left, check the angle on the other side too
*/
					if (check_ang1 && !bad_triangle)
					{
						um_vctovc_2d(pt[n[i]],p4);
						um_vcmnvc_2d (p1,p4,w1);
						um_vcmnvc_2d (p3,p4,w3);
						um_unitvc_2d (w1,w1); um_unitvc_2d (w3,w3);
						bad_triangle = (UM_DOT_2D (w1,w3) < -0.98);
					}
				}
/*
... if the triangle is empty, cut vertex i2 out.
*/
				if (!bad_triangle)
				{
					imin = i1;
					dmin = d;
					tmin = turn;
				}
			}

			if (imin < 0)
			{
				if (!check_angle) goto Done;
				check_angle = UU_FALSE;
			}
		}

		i1 = imin;
		i2 = um_mod(i1+1, nver);
		i3 = um_mod(i2+1, nver);

		area += tmin;

		trian.n1 = n[i1];
		trian.n2 = n[i2];
		trian.n3 = n[i3];
		S_arrange (&trian,np);
		S_add (&trian);

		uu_list_delete (&v_list,i2,1);

		nver = UU_LIST_LENGTH(&v_list);

	}  /* while */

	if (nver == 3)
	{
		n = (int *) UU_LIST_ARRAY (&v_list);
		i1 = 0;
		i2 = 1;
		i3 = 2;
		um_vctovc_2d(pt[n[i1]],p1);
		um_vctovc_2d(pt[n[i2]],p2);
		um_vctovc_2d(pt[n[i3]],p3);

		turn = um_triangle_signed_area (p1,p2,p3);
		if (turn >= -1.e-12)
		{
			area += turn;

			trian.n1 = n[i1];
			trian.n2 = n[i2];
			trian.n3 = n[i3];
			S_arrange (&trian,np);
			S_add (&trian);
			nver--;
		}
	}
Done:
	if (nver < 4)
	{
		status = UU_SUCCESS;

		tri = (S_tript *) UU_LIST_ARRAY (&Strilst);

		ntri = UU_LIST_LENGTH(&Strilst);

		for (i = 0; i < ntri; i++)
		{
			tript.n1 = np0 + tri[i].n1;
			tript.n2 = np0 + tri[i].n2;
			tript.n3 = np0 + tri[i].n3;
			uu_list_push (&tess->tri,&tript);
		}

	}
	else
		status = UU_FAILURE;

	return (status);
}

/*********************************************************************
**    FUNCTION : int um_merge_tess (tess1,tess)
**     Add the tess1 data to tess; then clean the tess1 structure
**     for further use (tess1 is not deallocated)
**
**    PARAMETERS
**       INPUT  :
**          tess - original tessellation
**          tess1 - new tessellation
**       OUTPUT :
**          tess - updated tessellation
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_merge_tess (tess1,tess)
UM_tess1 *tess1,*tess;
{
	int i,nedges,nver,ntedges;
	UM_edge *edge;
	UU_LIST *edges;

	edges = &tess1->edges;
	nver = UU_LIST_LENGTH (&tess->vertices);

	uu_list_push_list (&tess->vertices,&tess1->vertices);
	uu_list_push_list (&tess->normals,&tess1->normals);
	uu_list_push_list (&tess->uv,&tess1->uv);
/*
... if tessellation empty, just write into it
*/
	if (nver != 0)
	{
		edge = (UM_edge *) UU_LIST_ARRAY (edges);
		nedges = UU_LIST_LENGTH (&tess1->edges);
		ntedges = UU_LIST_LENGTH (&tess->edges);

		for (i = 0; i<nedges; i++, edge++)
		{
			edge->south += nver; edge->north += nver;
			if (edge->s_e >= 0) edge->s_e += ntedges;
			if (edge->n_e >= 0) edge->n_e += ntedges;
			if (edge->n_w >= 0) edge->n_w += ntedges;
			if (edge->s_w >= 0) edge->s_w += ntedges;
		}
	}

	uu_list_push_list (&tess->edges,edges);

	tess->np = UU_LIST_LENGTH (&tess->vertices);
	tess->nedges = UU_LIST_LENGTH (&tess->edges);

	um_clean_tess1 (tess1);

	return (UU_SUCCESS);
}

/*********************************************************************
**  FUNCTION :  int S_add_tess_points (srf,tfmat,np,p2D,vertices,normals,uvs,tolsq)
**    Appends set {u,v,r(u,v),norm(u,v)} to the tessellation
**
**  PARAMETERS
**     INPUT  :
**        srf    - Surface being tessellated.  If 'srf' is UU_NULL,
**                 then the 2-D polygon is an XY-plane set of points.
**        tfmat  - Surface transformation matrix.
**        np     -    number of points to be appended
**        p2D    -    u,v coord. of the new points
**        tess   - tessellation
**     OUTPUT :
**        np     - updated number of points
**        tess   - updated tesselation
**  RETURNS      :
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
static int S_add_tess_points (srf,tfmat,np,p2D,vertices,normals,uvs,tolsq)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
int *np;
UM_2Dcoord *p2D;
UU_LIST *vertices,*normals,*uvs;
UU_REAL tolsq;
{
	int status, i, j;
	UU_REAL u,v,dd;
	UM_2Dcoord uvi;
	UM_coord pt0,pt1;

	for (i = 0; i < *np; i++)
	{
		um_vctovc_2d (p2D[i],uvi);
/*
.....Evaluate surface for 3-D point
*/
		if (srf != UU_NULL)
		{
			if (ncl_scaled())
			{
				ncl_rescale_uv (uvi);
			}
			for (j = 0; j < 2; j++)
			{
				if (uvi[j] < -0.05 || uvi[j] > 1.05)
					return (UU_FAILURE);
				if (uvi[j] < 0.) uvi[j] = 0.;
				else if (uvi[j] > 1.) uvi[j] = 1.;
			}

			u = uvi[0]; v = uvi[1];

			status = uc_evsrf(UM_NORM, u, v, srf, tfmat, &Sevsrf);
			if (status != UU_SUCCESS) return (UU_FAILURE);
		}
/*
.....Planar polygon is being tessellated
.....Use input point
*/
		else
		{
			Sevsrf.sp[0] = uvi[0]; Sevsrf.sp[1] = uvi[1]; Sevsrf.sp[2] = 0.;
			Sevsrf.snorm[0] = Sevsrf.snorm[1] = 0; Sevsrf.snorm[2] = 1.;
		}
/*
..... eduard 01/15/2000. We skip a point if it is too close to the
..... previous one, both in 2D and 3D. To fix FSR 91078.
*/
		if (i > 0)
		{
			um_vctovc (Sevsrf.sp, pt1);
			dd = um_sqdis_2d (p2D[i],p2D[i-1]);

			if (dd < 0.01 * UM_DFUZZ)
			{
				dd = UM_SQDIS (pt0, pt1);
				if (dd < 0.01 * tolsq)
				{
					for (j = i+1; j < *np; j++)
						um_vctovc_2d (p2D[j],p2D[j-1]);
					(*np)--;
					i--;
					continue;
				}
			}
		}
		um_vctovc (Sevsrf.sp,pt0);
		uu_list_push (vertices, &Sevsrf.sp);

		if (srf != UU_NULL)
		{
			if (NCL_triansf > 0)
				ncl_triansf_fix_norm (u,v,srf,tfmat,Sevsrf.snorm,tolsq);

			um_unitvc (Sevsrf.snorm,Sevsrf.snorm);
		}

		uu_list_push (normals, &Sevsrf.snorm);
		if (uvs != NULLST) uu_list_push (uvs, uvi);
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**  FUNCTION :  int um_add_tess_points (srf,tfmat,np,p2D,tess)
**    Appends set {u,v,r(u,v),norm(u,v)} to the tessellation
**
**  PARAMETERS
**     INPUT  :
**        srf    - pointer to surface entity
**        tfmat  - surface matrix
**        np     -    number of points to be appended
**        p2D    -    u,v coord. of the new points
**        tess   - tessellation
**     OUTPUT :
**        np     - updated number of points
**        tess   - updated tesselation
**  RETURNS      :
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int um_add_tess_points_96 (srf,tfmat,np,p2D,tess)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
int *np;
UM_2Dcoord *p2D;
UM_tess1 *tess;
{
	int status;
	UU_REAL tol,tolsq;
	
	tol = UM_TESSELLATION_TOLERANCE;
	tolsq = tol*tol;

	status = S_add_tess_points (srf,tfmat,np,p2D,&tess->vertices,&tess->normals,
		&tess->uv,tolsq);

	return (status);
}

/*********************************************************************
**  FUNCTION :  int um_add_tess_points (srf,tfmat,np,p2D,tess)
**    Appends set {u,v,r(u,v),norm(u,v)} to the tessellation
**
**  PARAMETERS
**     INPUT  :
**        srf    - pointer to surface entity
**        tfmat  - surface matrix
**        np     -    number of points to be appended
**        p2D    -    u,v coord. of the new points
**        tess   - tessellation
**     OUTPUT :
**        np     - updated number of points
**        tess   - updated tesselation
**  RETURNS      :
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int um_add_tess_points (srf,tfmat,np,p2D,tess,uvtes)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
int *np;
UM_2Dcoord *p2D;
UM_tessellation *tess;
UU_LIST *uvtes;
{
	int status;
	UU_REAL tol,tolsq;
	
	tol = UM_TESSELLATION_TOLERANCE;
	tolsq = tol*tol;

	status = S_add_tess_points (srf,tfmat,np,p2D,&tess->vertices,&tess->normals,
		uvtes,tolsq);

	return (status);
}

/*********************************************************************
**  FUNCTION : int um_find_edge (south, north, edges)
**
**    Finds number of edge with given north&south points in a list of edges
**
**  PARAMETERS
**     INPUT  :
**        south, north - numbers of south/north vertices of an edge
**        edges - list of edges
**     OUTPUT :
**        none
**  RETURNS :
**     number of the edge in the list; -1 if not found
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int um_find_edge (south, north, edges)
int south, north;
UU_LIST *edges;
{
	int i,res, n;
	UM_edge *edge;

	n = UU_LIST_LENGTH(edges);
	edge = (UM_edge *) UU_LIST_ARRAY(edges);

	for(i=0; i<n; i++, edge++)
	{
		if( (south == edge->south) && (north == edge->north) ) break;
		if( (north == edge->south) && (south == edge->north) ) break;
	}

	res = (i<n) ? i : -1 ;

	return (res);
}

/*********************************************************************
**    FUNCTION : void um_clean_tess1 (tess)
**      Sets counters to zero, does not deallocate memory.
**    PARAMETERS
**       INPUT :
**          tess - tessellation
**       OUTPUT :
**          tess - counters reset to zero
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_clean_tess1 (tess)
UM_tess1 *tess;
{
	tess->np = tess->nedges = 0;

	UU_LIST_EMPTY (&tess->vertices);
	UU_LIST_EMPTY (&tess->edges);
	UU_LIST_EMPTY (&tess->uv);
	UU_LIST_EMPTY (&tess->normals);
}
/*********************************************************************
**    FUNCTION : int um_init_tess_lists (tess,knt)
**       Initializes a UM_tessellation structure and allocates memory
**       for the lists within the structure.
**    PARAMETERS
**       INPUT :
**          tess - tessellation
**          knt  - initial size to allocate lists at.
**       OUTPUT :
**          allocates lists in the tessellation
**    RETURNS      :
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_init_tess_lists (tess,uvlist,knt)
UM_tessellation *tess;
UU_LIST *uvlist;
int knt;
{
	int status = UU_SUCCESS;

	if (UU_LIST_NULLPTR (&tess->vertices))
	{
		status = uu_list_init1 (&tess->vertices, sizeof (UM_coord), knt, knt);
		if (status != UU_SUCCESS) return (status);
	}
	if (UU_LIST_NULLPTR (&tess->normals))
	{
		status = uu_list_init1 (&tess->normals, sizeof (UM_vector), knt, knt);
		if (status != UU_SUCCESS) return (status);
	}
	if (uvlist != NULLST)
	{
		if (UU_LIST_NULLPTR (uvlist))
		{
			status = uu_list_init1 (uvlist, sizeof (UM_2Dcoord), knt, knt);
			if (status != UU_SUCCESS) return (status);
		}
	}
	if (UU_LIST_NULLPTR (&tess->tri))
	{
		status = uu_list_init1 (&tess->tri, sizeof (UM_tript), knt, knt);
	}

	return (status);
}

/*********************************************************************
**    FUNCTION : um_create_tess1(tess,maxcnt)
**       Initializes a tess1 structure and allocates memory
**       for the lists within the structure.
**    PARAMETERS
**       INPUT :
**          tess - tessellation
**          max_cnt - Initial size to allocate lists at.
**       OUTPUT :
**          allocates lists in the tessellation
**    RETURNS      :
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_create_tess1(tess,max_cnt)
UM_tess1 *tess;
int max_cnt;
{
	int exp_cnt,status;

	tess->toler = 0.;
	tess->key = tess->np = tess->nedges = 0;

	exp_cnt = (max_cnt > 0)? max_cnt: 100;

	status = uu_list_init1 (&tess->vertices, sizeof(UM_coord), max_cnt, exp_cnt);
	if (status != UU_SUCCESS) return (status);
	status = uu_list_init1 (&tess->normals, sizeof(UM_vector), max_cnt, exp_cnt);
	if (status != UU_SUCCESS) return (status);
	status = uu_list_init1 (&tess->uv, sizeof (UM_2Dcoord), max_cnt, exp_cnt);
	if (status != UU_SUCCESS) return (status);

	max_cnt = 125*max_cnt/100;
	exp_cnt = 125*exp_cnt/100;
	status = uu_list_init1 (&tess->edges, sizeof (UM_edge), max_cnt, exp_cnt);

	return (status);
}
/*********************************************************************
**    FUNCTION : int um_free_tess1 (tess)
**
**    PARAMETERS
**       INPUT :
**          tess - tessellation
**       OUTPUT :
**          deallocates lists in the tessellation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_free_tess1 (tess)
UM_tess1 *tess;
{
	tess->toler = 0.;
	tess->key = tess->np = tess->nedges = 0;
	uu_list_free (&tess->vertices);
	uu_list_free (&tess->normals);
	uu_list_free (&tess->edges);
	uu_list_free (&tess->uv);
}

/*********************************************************************
**    FUNCTION     : void um_set_tess_toler (tol)
**    Sets current tolerance used for srf tessellation
**    PARAMETERS
**       INPUT  :
**          tol - tolerance to set
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_set_tess_toler (tol)
UU_REAL tol;
{
	UU_REAL toler;
	UM_tess_settype typ;
	int kupts,kvpts;

	ncl_get_tess_parms (&typ,&toler,&kupts,&kvpts);
	toler = MAX2 (tol, UM_BOUNDARY_TOLER);
	UM_TESSELLATION_TOLERANCE = toler;
	ncl_set_tess_parms (typ,toler,kupts,kvpts);
}

/*********************************************************************
**    FUNCTION     : UU_REAL ncl_get_tess_toler ()
**    Returns current tolerance used for srf tessellation
*********************************************************************/
UU_REAL um_get_tess_toler ()
{
   return (UM_TESSELLATION_TOLERANCE);
}

/*********************************************************************
*********************************************************************/
int um_init_aux_lists()
{
	int status;

	status =
	uu_list_init1 (&chain, sizeof(UM_2Dcoord), 0, 200);
	if (status != UU_SUCCESS) return (status);
	status =
	uu_list_init1 (&v_list, sizeof(int), 200, 200);
	if (status != UU_SUCCESS) return (status);
	status =
	uu_list_init1 (&Strilst, sizeof(S_tript), 200, 200);
	if (status != UU_SUCCESS) return (status);
	status =
	uu_list_init1 (&bps, sizeof(int), 10, 10);

	return (status);
}

/*********************************************************************
*********************************************************************/
void um_free_aux_lists()
{
	uu_list_free (&chain);
	uu_list_free (&v_list);
	uu_list_free (&Strilst);
	uu_list_free (&bps);
}
