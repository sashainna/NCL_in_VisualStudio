/*********************************************************************
**    NAME         :  neproj4.c
**       CONTAINS: functions to revolve NCL patterns onto cones and
**                     surfaces of revolution
**  These functions are very similar to the pattern wrapping onto
**  primitives (see nclc/neproj3.c).  The main difference is that distances
**  between attach point and projected pattern point are not so well
**  maintained.  These involve using the axis of symmetry.  You break
**  up the motion into a part that goes along the axis of symmetry (vertical)
**  and the remainder (horizontal).
**
**      ncl_patt_revolve
**      ncl_revolve_dist_cone
**      ncl_revolve_patt_sfrev
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       neproj4.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:43
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "ginq.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mplot.h"
#include "ncl.h"
#include "nccs.h"
#include "nclfc.h"
#include "nproj.h"
#include "mdeval.h"
#include "msrf.h"
#include "modef.h"

/*********************************************************************
**    E_FUNCTION     : ncl_patt_revolve(surf, pt_in, cplist, cnlist, ulist,
**                          flag, start, sf_begin, uv_pt0)
**  Revolving a pattern onto a cone or surface of revolution.
**  The distance vector is broken down into a vertical part (related to
**  the axis of symmetry) and a horizontal part (perpendicular to the axis).
**  The total distance travelled is not necessarily the same as the
**  magnitude of the distance vector.
**
**    PARAMETERS
**       INPUT  :
**              surf     = pointer to surface
**              pt_in    = original pattern points
**              flag[0]  = attach_flag
**                           1 -> START
**                           2 -> MIDDLE
**                           3 -> END
**                           4 -> CENTER
**                           5 -> AT
**              flag[1]  = pt_new_patt (number of points already projected)
**              flag[2]  = side (+1 vector part of point-vectors go along
**                           surface normal at the projected surface point)
**                           (-1 vectors go opposite to surface normal)
**              flag[3]  = first  (abstracted in case starting from END)
**              flag[4]  = second (abstracted in case starting from END)
**              flag[5]  = last   (abstracted in case starting from END)
**              flag[6]  = REVOLVE (= 2) or RADIAL (= 3)
**              start    = starting point in original pattern (attach point)
**              sf_begin = projection of start onto the surface
**                           all following points will be measured from here
**              uv_pt0   = parametric surface coordinates for sf_begin
**
**        OUTPUT :
**              cplist   = projection on surface of pt_in
**              cnlist   = Vectors if original pattern was made of
**                         point-vectors. Vectors point along surface normal at
**                         corresponding surface points (or opposite the
**                         normal if flag[2]=-1).
**              ulist    = UV-parametric coordinates for projected surf pts
**
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR	   : Ed Ames  20 Nov 00
*********************************************************************/
int ncl_patt_revolve(surf, proj, cplist, cnlist, ulist, flag, start,
			sf_begin, uv_pt0)
struct NCL_fixed_databag *surf;
NCL_proj_struc *proj;
int flag[6];
UM_coord start, sf_begin;
UU_LIST *cplist,*cnlist,*ulist;
UM_2Dcoord uv_pt0;
{
	int i, current, status, default_err, rev_rad_flag;
	int attach_flag, pt_new_patt, side, first, second, last;
	int closdinu, closdinv,closed,primitive,circ_uv,ruledinuv,uv_data[3];
	UU_REAL angle, alpha, prim_data[5];
	UM_coord center, surfpt, edge_pt, base, pts[3], uv_pt3;
	UM_vector axis, dist_vec, loc_proj_vec, prj_pl_norm;
	UM_vector zero_rad_vec, one_rad_vec, vec[5],vc1;
	UM_2Dcoord uv_pt1, uv_pt2;
	struct UM_evsrfout evsrf;
	UM_transf tfmat;
	UM_int2 idx = 169;
	UM_real8 ver;
	int vectype;

	getsc(&idx, &ver);

	default_err = 419;  /* Error code:  FAILED TO FIND LINE-CURVE INTERSECTION */

	uc_init_evsrfout (surf, &evsrf);
	status = uc_retrieve_transf(surf->key,tfmat);
	if (status != UU_SUCCESS) return default_err;

	status = uc_evsrf(UM_NORM, uv_pt0[0], uv_pt0[1], surf, tfmat, &evsrf);
	if (status != UM_VALID) return default_err;
	um_vctovc(evsrf.snorm, loc_proj_vec);

	attach_flag  = proj->attach;
	pt_new_patt  = flag [1];
	side         = flag [2];
	first        = flag [3];
	second       = flag [4];
	last         = flag [5];
	rev_rad_flag = proj->wrap;
	vectype      = proj->vectype;

	primitive = ncl_get_prim_data(surf, center, axis, prim_data, uv_data);
	if (primitive == UU_FAILURE) return default_err;
	angle        = prim_data[0];
	alpha        = prim_data[3];
	closdinu     =   uv_data[0];
	closdinv     =   uv_data[1];
	circ_uv      =   uv_data[2];
	um_vctovc(center,pts[1]);
	um_vctovc(axis,  vec[1]);

	if (primitive == NCLSF_CONE)
	{
		um_unitvc(axis, axis);

		status = uc_evsrf(UM_POINT, 0.0, 0.0, surf, tfmat, &evsrf);
		if (status != UM_VALID) return default_err;
		um_vctovc(evsrf.sp, edge_pt);
		um_nptln(edge_pt, center, axis, base);
		um_vcmnvc(edge_pt, base, zero_rad_vec);
/*
..... circ_uv = UU_TRUE -> v goes along axis and u goes along circular arc
..... circ_uv = UU_FALSE -> opposite
*/
		if (circ_uv)
		{
			status = uc_evsrf(UM_POINT, 1.0, 0.0, surf, tfmat, &evsrf);
			closed = closdinu;
		}
		else
		{
			status = uc_evsrf(UM_POINT, 0.0, 1.0, surf, tfmat, &evsrf);
			closed = closdinv;
		}
		if (status != UM_VALID) return UU_FAILURE;
		um_vctovc(evsrf.sp, edge_pt);
		um_nptln(edge_pt, center, axis, base);
		um_vcmnvc(edge_pt, base, one_rad_vec);

		um_vctovc(zero_rad_vec, vec[3]);
		um_vctovc(one_rad_vec,  vec[4]);
	}
	if (surf->rel_num == NCL_REVSURF_REL)
	{
		if (primitive != NCLSF_CONE || ver < 9.349)
		{
		primitive = NCLSF_REVOLV;
/*
..... circ_uv = UU_TRUE -> v goes along axis and u goes along circular arc
..... circ_uv = UU_FALSE -> opposite
*/
		if (circ_uv)
			closed = closdinu;
		else
			closed = closdinv;
		}
	}
	else if (primitive == NCLSF_RULED)
	{
		closed = (closdinu == 1 || closdinu == 1);
		ruledinuv = prim_data[0];
		if (ruledinuv == 1)
			um_vctovc (evsrf.dsdu,vec[1]);
		else
			um_vctovc (evsrf.dsdv,vec[1]);
	}

/*
.....  ------------------->  MAIN LOOP  <--------------------------
*/
	for (i = second; i != last; i += (second - first))
	{
		if ((pt_new_patt == 0) && \
		    ((attach_flag == 2) || (attach_flag == 4) || (attach_flag == 5)))
			i = first;
		current = i;

		um_vcmnvc(proj->inpt[current], start, dist_vec);

		um_cross(dist_vec, loc_proj_vec, prj_pl_norm);
		um_unitvc(prj_pl_norm, prj_pl_norm);

		um_vctovc(sf_begin,    pts[0]);
		um_vctovc(dist_vec,    vec[0]);
		um_vctovc(prj_pl_norm, vec[2]);
		um_vctovc_2d(uv_pt0,   uv_pt1);

		switch (primitive)
		{
			case NCLSF_CONE:
			{
				status = ncl_revolve_dist_cone(surf->key, pts, vec, alpha, \
					angle, circ_uv, closed, uv_pt1, rev_rad_flag, uv_pt2);
				break;
			}
			case NCLSF_REVOLV:
			{
				status = ncl_revolve_patt_sfrev (surf, pts, vec, angle, \
					circ_uv, closed, uv_pt1, rev_rad_flag, uv_pt2);
				break;
			}
			case NCLSF_RULED:
			{
				status = ncl_revolve_patt_ruled (surf, pts, vec, ruledinuv, \
					closed, uv_pt1, uv_pt2);
				break;
			}
			default:
				return default_err;
		}
		if (status != UU_SUCCESS) return default_err;
		um_vctovc(pts[2], surfpt);

		uu_list_push(cplist,surfpt);
		if (ulist != UU_NULL)
		{
			um_xyztovc(uv_pt2[0],uv_pt2[1],0.,uv_pt3);
			uu_list_push(ulist,uv_pt3);
		}
/*		um_vctovc(surfpt, pt_out[pt_new_patt]);*/
/*		u[pt_new_patt] = uv_pt2[0]; v[pt_new_patt] = uv_pt2[1];*/

		if (cnlist != UU_NULL) /* Point vectors */
		{
			if (vectype == 2)
			{
				ncl_wrap_addvec (cnlist,proj->tvec[current]);
			}
			else
			{
				status =
				uc_evsrf(UM_NORM, uv_pt2[0], uv_pt2[1], surf, tfmat, &evsrf);
				if (status != UM_VALID) return default_err;
				if (side == 0)
					um_vcmnvc(proj->inpt[first], sf_begin, dist_vec);

				ncl_proj_pv_out(&side, dist_vec, evsrf.snorm, vc1);
				uu_list_push(cnlist,vc1);
			}
		}

		pt_new_patt++;
	}

	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : ncl_revolve_dist_cone (surf_key, pts, vec, alpha, angle, \
**                                circ_uv, closed, uv_pt1, rev_rad, uv_pt2)
**  Given a cone surface, axis of symmetry, apex of cone, angle of cone
**  (from axis to surface -> alpha), angle cone goes around horizontally,
**  [all this stuff just specifies the cone - gives a complete description],
**  a starting surface point with corresponding u,v coordinates, and a distance
**  vector (scaled to the desired distance); travel around the cone.  Output
**  the resulting surface point and its' u,v coordinates.
**
**  For revolving onto the cone, the distance vector is broken down into
**  two parts:  horizontal and vertical.  Where you are on the surface
**  determines what those mean.  The horizontal component is always the same:
**  it is the component of the distance vector perpendicular to the axis of
**  the cone.  The important thing is that the horizontal vector doesn't
**  always point the same way.  If you are on the flat part of the extention
**  surface (assuming the cone is not closed --> angle < 2Pi), the horizontal
**  vector will remain constant while on this flat part.  When you go around
**  the curving part of the surface and its extention, the horizontal vector
**  rotates with you (it is tangent to the surface at that point).  On the
**  flat part, the vertical vector is perpendicular to the horizontal and also
**  in the extention plane.  When on the curving part of the surface the vertical
**  vector is still perpendicular to the horizontal and also in the surface, but
**  now it keeps pointing at the apex.
**
**    PARAMETERS
**       INPUT  :
**              surf_key  =   UNIBASE key for surface pattern projected onto
**              pts[0]    =   start point -- travel dist_vect from this point
**              pts[1]    =   apex of cone
**              vec[0]    =   distance vector to go
**              vec[1]    =   axis of cone
**              vec[2]    =   projection plane normal
**              vec[3]    =   radius vector from axis point to (u = 0 , v = 0)
**              vec[4]    =   radius vector from axis point to (u/v = 0, v/u = 1)
**              alpha     =   the angle that the cone opens (based at apex,
**                               from axis to surface
**              angle     =   angle of completion (if complete -> 2Pi)
**                               for example a hemisphere angle = Pi
**              circ_uv   =
**              closed    =   UU_TRUE if complete (angle should = 2PI)
**              uv_pt1    =   starting u,v coordinates for base point
**              rev_rad =   2 if REVOLVE
**                          3 if RADIAL
**       OUTPUT :
**              pts[2]    =   ending point on surface (or extension)
**              uv_pt2    =   ending u,v coordinates for end_pt.
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  30 Aug 00
*********************************************************************/
int ncl_revolve_dist_cone(surf_key, pts, vec, alpha, angle, circ_uv, closed, \
                           uv_pt1, rev_rad, uv_pt2)
UU_KEY_ID surf_key;
UM_coord pts[3];
UM_vector vec[5];
UU_REAL alpha, angle;
int circ_uv, closed, rev_rad;
UM_2Dcoord uv_pt1, uv_pt2;
{
	int status, intersect;
	UM_vector unit_axis, unit_dir, edge, vert_dir, horz_dir, base_to_apex;
	UM_vector radial_vec, move, axis, dist_vec, prj_pl_norm;
	UM_vector zero_rad_vec, one_rad_vec, temp_vec, ext_pts[4];
	UM_coord apex, start_pt, end_pt, surfpt, axial_pt, intersect_pt, temp_surfpt;
	UU_REAL dist, horz_uv, vert_comp, horz_comp, max_angle;
	UU_REAL vert_flip_flag, horz_flip_flag, sin_alpha, cos_alpha;
	UU_REAL stuff, radius, test_ang, tol, starting_radius;
	UM_real4 side, proj_u, proj_v, ss[9];
	UM_int2 itype, isf;
	UM_real8 packed_desc;

	um_vctovc(pts[0], start_pt);
	um_vctovc(pts[1], apex);
	um_vctovc(vec[0], dist_vec);
	um_vctovc(vec[1], axis);
	um_vctovc(vec[2], prj_pl_norm);
	um_vctovc(vec[3], zero_rad_vec);
	um_vctovc(vec[4], one_rad_vec);

	gettol (&tol);
/*
..... Do the Fortran thing.  itype = 9 -> surface.  isf = 3 -> check surface
..... (rather than part surface or drive surface).  side = 0 first time
..... thru the function; however, I only call it once while inside of this
..... function.
*/
	itype = 9; isf =3; side = 0.0;
	ptdesc(&surf_key, &itype, &packed_desc);
	proj_u = uv_pt1[0]; proj_v = uv_pt1[1];

	um_unitvc (axis, unit_axis);
	um_unitvc (dist_vec, unit_dir);
	dist = um_mag (dist_vec);
	um_vctovc (start_pt, surfpt);

	um_nptln(surfpt, apex, unit_axis, axial_pt);
	um_vcmnvc(surfpt, axial_pt, radial_vec);
	starting_radius = um_mag(radial_vec);
	um_unitvc(radial_vec, radial_vec);

	sin_alpha = sin(alpha);
	cos_alpha = cos(alpha);
/*
..... circ_uv = UU_TRUE -> v goes along axis and u goes along circular arc
..... circ_uv = UU_FALSE -> opposite
*/
	if (circ_uv)
		horz_uv = uv_pt1[0];
	else
		horz_uv = uv_pt1[1];
/*
..... See if on flat part of extention surfaces (edge planes).
..... If so, find out which side (horz_uv == 1.0 or == 0.0), and
..... see if going towards or away from the main surface.
*/
	if (((horz_uv == 0.0) || (horz_uv == 1.0)) && !closed)
	{
/*
..... Get a vector from apex to bottom of cone along edge of surface.
..... stuff = height of cone (distance from base to apex along axis)
..... Use appropriate radial vector to build the correct edge (horz_uv = 0 or 1).
*/
		stuff = um_mag(one_rad_vec) * (cos_alpha / sin_alpha);
		um_vctmsc(unit_axis, stuff, temp_vec);
		if (horz_uv == 1.0)
			um_vcplvc(one_rad_vec, temp_vec, edge);
		if (horz_uv == 0.0)
			um_vcplvc(zero_rad_vec, temp_vec, edge);

		um_unitvc(edge, edge);
		um_ilnln1(apex, edge, surfpt, unit_dir, &intersect, intersect_pt);
/*
..... Find intersection of distance vector and the edge of the main
..... surface.  If it is behind me, I am going away from the surface.
..... If it is too far away, I will finish the movement before reaching
..... the surface.  In both these cases, I will stay on the flat part of
..... the extention surface.  Calculations are simple here.
..... If I am heading toward the surface and am close enough, go to the
..... intersection point (edge of surface and distance vector) and subtract
..... off the distance just travelled from total distance needed to go.
*/
		if (um_dcccc(intersect_pt, surfpt) > tol)
		{
			um_vctovc(surfpt, ext_pts[0]);
			um_vctovc(intersect_pt, ext_pts[1]);
			um_vctovc(dist_vec, ext_pts[2]);
			status = ncl_flat_extention (surf_key, pts, intersect, \
						circ_uv, uv_pt1, uv_pt2);
			um_vctovc(ext_pts[3], surfpt);
			if (status == UU_TRUE)
			{
				um_vctovc(surfpt, pts[2]);
				return UU_SUCCESS;
			}
		}
	}
/*
..... Get the vertical component of the direction vector.  By that
..... I mean the part of the vector that goes up and down the surface.
..... You find that by making a line from the surface point to the
..... apex of the cone (center).  Get the part of Local_dist vector
..... that goes along this "vertical" vector and use the sign of
..... vert_vec .dot. local_dist to get the correct direction
..... (towards or away from the apex).
.....
..... Must be carful to isolate the direction of the axis vector.
..... This direction keeps changing; therefore, make base_to_apex
..... vector.  This will always maintain the same direction.
*/
	um_nptln(surfpt, apex, unit_axis, axial_pt);
	um_vcmnvc(apex, axial_pt, base_to_apex);
	um_unitvc(base_to_apex, base_to_apex);
	um_vcmnvc(apex, surfpt, vert_dir);
	if ((um_dot (base_to_apex, dist_vec) + UM_FUZZ) > 0.0)
		vert_flip_flag = 1.0;
	else
		vert_flip_flag = -1.0;
	um_unitvc(vert_dir, vert_dir);
	vert_comp = fabs( um_dot(base_to_apex, dist_vec) );
	um_vctmsc(vert_dir, (vert_flip_flag * vert_comp), vert_dir);
	um_vcplvc(surfpt, vert_dir, temp_surfpt);

	um_nptln(temp_surfpt, apex, unit_axis, axial_pt);
	um_vcmnvc(temp_surfpt, axial_pt, radial_vec);
	radius = um_mag(radial_vec);
	um_unitvc(radial_vec, radial_vec);

	um_vcmnvc(apex, axial_pt, base_to_apex);
	um_unitvc(base_to_apex, base_to_apex);
	um_cross(base_to_apex, radial_vec, horz_dir);
	if ((um_dot (horz_dir, dist_vec) + UM_FUZZ) > 0.0)
		horz_flip_flag = 1.0;
	else
		horz_flip_flag = -1.0;
	um_unitvc(horz_dir, horz_dir);
	horz_comp = dist*dist - vert_comp*vert_comp;
	if (horz_comp < 0.) horz_comp = 0.;
	else horz_comp = sqrt (horz_comp);
/*
..... Make fraction of circle to go around.  I have already done the vertical part.
.....
..... Need to determine if wraps around to extention plane on other side.
..... In general, I need to find out how much horizontal angular distance
..... the curve travels.  If it travels more than ->  angle  (the completion
..... angle of the cone.  In other words, if the cone is complete  angle = 2PI.
..... It is the angle that you are asked when you make the cone), then the
..... curve travelled farther than the surface: it is on the extension plane.
*/
	if (rev_rad == 2)  /* REVOLVE  */
		test_ang = horz_comp / radius;
	else               /* RADIAL   */
		test_ang = horz_comp / starting_radius;

	if (horz_flip_flag == 1.0)	/* Going in positive dir */
		max_angle = angle * (1.0 - horz_uv);
	else			  	/* Going in negative dir */
		max_angle = angle * horz_uv;
	if (closed)
		max_angle = UM_TWOPI;
/*
..... Onto extention plane on far side.  It wrapped completely around the
..... surface and off the edge onto the other extention plane.
*/
	if ((test_ang > max_angle) && (!closed))
	{
		vert_comp *= (max_angle / test_ang);
		um_unitvc(vert_dir, vert_dir);
		um_vctmsc(vert_dir, vert_comp, vert_dir);
		um_vcplvc(surfpt, vert_dir, surfpt);
/*
..... Moved up this smaller amount.  Now do horizontal.
*/
		um_nptln (surfpt, apex, unit_axis, axial_pt);
		um_vcmnvc(surfpt, axial_pt, radial_vec);
		radius = um_mag(radial_vec);
		um_unitvc(radial_vec, radial_vec);

		um_vctmsc(horz_dir, (horz_flip_flag * sin(max_angle) * radius), horz_dir);
		um_vctmsc(radial_vec, (cos(max_angle) * radius), radial_vec);
		um_vcplvc(horz_dir, radial_vec, move);
		um_vcplvc(axial_pt, move, surfpt);
/*
..... Made it to far edge of surface (or curving part of extention).
..... Determine how much further to go on flat extention plane.
..... Also, must determine direction.
*/
		um_vcmnvc(surfpt, axial_pt, radial_vec);
		um_unitvc(radial_vec, radial_vec);
		um_cross (unit_axis, radial_vec, horz_dir);
		um_vcmnvc(apex, surfpt, vert_dir);
		um_unitvc(vert_dir, vert_dir);
		um_unitvc(horz_dir, horz_dir);
		horz_comp = sqrt (dist * dist - (vert_comp * vert_comp));

		um_vctmsc(horz_dir, (horz_flip_flag * horz_comp), horz_dir);
		um_vctmsc(vert_dir, (vert_flip_flag * vert_comp), vert_dir);
		um_vcplvc(horz_dir, vert_dir, move);
		um_unitvc(move, move);
		um_vcplvc(surfpt, move, end_pt);
		um_vctovc(end_pt, pts[2]);
/*
..... Output end_pt and uv_pt2.
*/
		sfpt(&packed_desc, end_pt, &isf, &side, &proj_u, &proj_v, ss);
		if (circ_uv)
		{
			if (horz_flip_flag > 0.0)
				uv_pt2[0] = 1.0;
			else
				uv_pt2[0] = 0.0;
			uv_pt2[1] = proj_v;
		}
		else
		{
			uv_pt2[0] = proj_u;
			if (horz_flip_flag > 0.0)
				uv_pt2[1] = 1.0;
			else
				uv_pt2[1] = 0.0;
		}

		return UU_SUCCESS;
	}
	else
	{
/*
..... Ending on curving part of surface (rather than flat extention planes).
*/
		um_vctmsc(horz_dir, (horz_flip_flag * sin(test_ang) * radius), horz_dir);
		um_vctmsc(radial_vec, (cos(test_ang) * radius), radial_vec);
		um_vcplvc(horz_dir, radial_vec, move);
		um_vcplvc(axial_pt, move, end_pt);
		um_vctovc(end_pt, pts[2]);
/*
..... Output end_pt and uv_pt2.
*/
		sfpt(&packed_desc, end_pt, &isf, &side, &proj_u, &proj_v, ss);
		uv_pt2[0] = proj_u; uv_pt2[1] = proj_v;

		return UU_SUCCESS;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_revolve_patt_sfrev (surf, pts, vec, angle, \
**                              circ_uv, closed, uv_pt1, rev_rad, uv_pt2)
**  Given a surface of revolution and info like axis of symmetry, point
**  on that axis, starting surface point, distance vector (scaled to
**  distance to travel), starting uv values, and angle of revolution
**  for the surface, find and output the ending surface point and
**  corresponding u,v coordinates.
**  The movement will be in two parts.  The first is "vertical"; this
**  means taking the component of the distance vector along the axis of
**  symmetry and travelling that arclength.  The second part is "horizontal".
**
**  There are some different situations for the horizontal part.  First, you
**  can start on the surface extention.  If so, you can travel the entire
**  horizontal component on the extention (towards or away from the main
**  surface) or you could make onto the main surface.  Second, you might
**  start on the surface.  The next issue is do you continue off the surface
**  onto the extention.  After all horizontal movement is finished, you
**  are done.  Compute the uv coordinates and output final position and
**  u,v's.
**
**    PARAMETERS
**       INPUT  :
**              surf    =   UNIBASE key for surface pattern projected onto
**              pts[0]  =   start point -- travel dist_vect from this point
**              pts[1]  =   point on axis of revolution
**              vec[0]  =   distance vector to go
**              vec[1]  =   axis of revolution
**              vec[2]  =   projection plane normal
**              vec[3]  =   radius vector from axis point to (u = 0 , v = 0)
**              vec[4]  =   radius vector from axis point to (u/v = 0, v/u = 1)
**              angle   =   angle of completion (if complete -> 2Pi)
**                               for example a hemisphere angle = Pi
**              circ_uv =
**              closed  =   UU_TRUE if complete (angle should = 2PI)
**              uv_pt1  =   starting u,v coordinates for base point
**              rev_rad =   2 if REVOLVE
**                          3 if RADIAL
**       OUTPUT :
**              pts[2]  =   ending point on surface (or extension)
**              uv_pt2  =   ending u,v coordinates for end_pt.
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  22 Nov 00
*********************************************************************/
int ncl_revolve_patt_sfrev (surf, pts, vec, angle, circ_uv, closed, uv_pt1, \
                            rev_rad, uv_pt2)
struct NCL_fixed_databag *surf;
UM_coord pts[3];
UM_vector vec[5];
UU_REAL angle;
int circ_uv, closed, rev_rad;
UM_2Dcoord uv_pt1, uv_pt2;
{
	int status, toward;
	UU_REAL tol, dist, radius, vert_comp, horz_comp, horz_flip_flag, horz_uv;
	UU_REAL travel_angle, max_angle, ext_dist, starting_radius;
	UM_coord start_pt, center, surfpt, axial_pt, end_pt, arcpts[5];
	UM_coord edge_pt, temp_surfpt;
	UM_vector dist_vec, axis, prj_pl_norm, unit_axis, unit_dir;
	UM_vector radial_vec, horz_dir, vert_dir, move, sfnorm;
	UM_2Dcoord temp_uv;
	UM_real4 side, proj_u, proj_v, ss[9];
	UM_int2 itype, isf;
	UM_real8 packed_desc;
	struct UM_evsrfout evsrf;
	UM_transf tfmat;
	UU_REAL co;
	UM_int2 idx = 169;
	UM_real8 ver;

	getsc(&idx, &ver);

	uc_init_evsrfout (surf, &evsrf);
	status = uc_retrieve_transf(surf->key,tfmat);
	if (status != UU_SUCCESS) return UU_FAILURE;

	um_vctovc(pts[0], start_pt);
	um_vctovc(pts[1], center);
	um_vctovc(vec[0], dist_vec);
	um_vctovc(vec[1], axis);
	um_vctovc(vec[2], prj_pl_norm);
	um_vctovc_2d(uv_pt1, temp_uv);
	gettol (&tol);
/*
..... Do the Fortran thing.  itype = 9 -> surface.  isf = 3 -> check surface
..... (rather than part surface or drive surface).  side = 0 first time
..... thru the function; however, I only call it once while inside of this
..... function.
*/
	itype = 9; isf =3; side = 0.0;
	ptdesc(&surf->key, &itype, &packed_desc);
	proj_u = uv_pt1[0]; proj_v = uv_pt1[1];

	um_unitvc (axis, unit_axis);
	um_unitvc (dist_vec, unit_dir);
	dist = um_mag (dist_vec);
	um_vctovc (start_pt, surfpt);
/*
..... Get the vertical component of the direction vector.  By that
..... I mean the part of the vector that goes up and down the surface
..... along the curve of intersection between the surface and the
..... plane containing the surface point and the axis of symmetry.
*/
	status = uc_evsrf(UM_NORM, temp_uv[0], temp_uv[1], surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_unitvc(evsrf.snorm, sfnorm);

	um_nptln(evsrf.sp, center, unit_axis, axial_pt);
	starting_radius = um_dcccc(axial_pt, evsrf.sp);

	um_cross(unit_axis, sfnorm, horz_dir);
	um_unitvc(horz_dir, horz_dir);
	horz_comp = UM_DOT (horz_dir,dist_vec);
	if (horz_comp >= 0.0)
		horz_flip_flag = 1.0;
	else
		horz_flip_flag = -1.0;

	um_translate_point (dist_vec, -horz_comp, horz_dir, vert_dir);
	horz_comp = fabs(horz_comp);
	vert_comp = UM_MAG (vert_dir);
	if (vert_comp > UM_FUZZ && ver >= 9.349)
	{
		um_unitvc(vert_dir,vert_dir);
		co = UM_DOT(vert_dir,unit_axis);
		if (fabs(co) < 0.0175)
		{
			vert_comp = 0;
			horz_comp = dist;
		}
	}
/*
..... Travel vertically (along the curve of intersection of
..... the surface and a plane containing the surface point and
..... the axis of symmetry).
*/
	if (vert_comp > UM_FUZZ)
	{
		um_vctovc(vert_dir, arcpts[0]); /* dist_vect */
		um_vctovc(horz_dir, arcpts[1]); /* prj_pl_norm */
		um_vctovc(surfpt, arcpts[2]);

		status =  ncl_go_along_surf_arclen(surf, arcpts, temp_uv, vert_comp);
		if(status != UU_SUCCESS) return UU_FAILURE;

		um_vctovc(arcpts[3], temp_surfpt); /* endpt after vertical travel */
	}
	else
		um_vctovc(surfpt, temp_surfpt);
/*
..... circ_uv = UU_TRUE -> v goes along axis and u goes along circular arc
..... circ_uv = UU_FALSE -> opposite
..... Surfaces of revolution are a special case.  Right now, they are defined
..... with v being the horizontal component while u goes vertically along
..... the axis.  Since all the other primitives use the circ_uv logic and
..... the definitions might change for u,v on surf_of_rev, I will keep the
..... same logic here.
..... Should be:         horz_uv = uv_pt1[1]  ->  v
*/
	if (circ_uv)
		horz_uv = uv_pt1[0];
	else
		horz_uv = uv_pt1[1];
/*
..... See if on the horizontal part of the extention surface.
..... If so, find out which side (horz_uv == 1.0 or == 0.0), and
..... see if going towards or away from the main surface.
*/
	if (((horz_uv == 0.0) || (horz_uv == 1.0)) && !closed)
	{
/*
..... This is for getting the horizontal vector on the extention
..... surface.  It is not just axis X radial_vec.
*/
		um_cross (unit_axis, sfnorm, horz_dir);
		if (UM_DOT (horz_dir, dist_vec) >= 0.0)
			horz_flip_flag = 1.0;
		else
			horz_flip_flag = -1.0;
		um_unitvc(horz_dir, horz_dir);
/*
..... Given the setup of u and v parameters on the surface,
..... the surface normal will point INSIDE (TOWARDS the axis
..... of revolution).  (Surface with normals will look like a
..... cave with stalagtites and stalagmites (on inside of surface
..... pointed towards center) rather than porcupine (on outside
..... of surface pointed away).)
..... This means that the horz_dir (axis cross sfnorm) will point
..... in the negative v direction.  If horz_flip_flag < 0, going
..... in positive v direction; meanwhile, if horz_flip_flag > 0,
..... you are going in a negative v direction.
*/
		toward = UU_FALSE;
		if (horz_flip_flag == -1.0)	/* Going in positive dir */
			if (horz_uv == 0.0) toward = UU_TRUE;
		else			  	/* Going in negative dir */
			if (horz_uv == 1.0) toward = UU_TRUE;
/*
..... If going away from the surface (!toward), just travel horizontally.
..... If going towards the surface but ending before reaching it, just
..... travel horizontally.
..... If going towards the surface and reach it, go to the edge and subtract
..... the horizontal distance just traveled from the horz_comp.
*/
		if (!toward)
		{
			um_vctmsc (horz_dir, (horz_flip_flag * horz_comp), horz_dir);
			um_vcplvc (temp_surfpt, horz_dir, end_pt);
			um_vctovc (end_pt, pts[2]);
			um_vctovc_2d(temp_uv, uv_pt2);

			return UU_SUCCESS;
		}
		else
		{
			um_nptln(center, temp_surfpt, horz_dir, edge_pt);
			ext_dist = um_dcccc (edge_pt, temp_surfpt);

			if ((ext_dist - fabs(horz_comp) + UM_FUZZ) > 0)
			{
/*
..... Finished before reaching the edge.
*/
				um_vctmsc(horz_dir, (horz_flip_flag * horz_comp), horz_dir);
				um_vcplvc(temp_surfpt, horz_dir, pts[2]);
				um_vctovc_2d(temp_uv, uv_pt2);

				return UU_SUCCESS;
			}

			horz_comp -= ext_dist;
			um_vctovc(edge_pt, temp_surfpt);
		}
	}
/*
..... Travelled vertically (along the curve of intersection of
..... the surface and a plane containing the surface point and
..... the axis of symmetry -> constant horz_uv).  Now must go horizontally.
*/
	um_nptln(temp_surfpt, center, unit_axis, axial_pt);
	um_vcmnvc(temp_surfpt, axial_pt, radial_vec);
	radius = um_mag(radial_vec);
	um_unitvc(radial_vec, radial_vec);
/*
..... Regenerate horizontal direction vector.  Take cross product
..... of surface normal and axis of revolution.  Can't use radial
..... vector because the surface normal points inward for surfaces
..... of revolution.  The horz_flip_flag was decided using normals
..... not outward pointing radial vectors.
*/
	status = uc_evsrf(UM_NORM, temp_uv[0], temp_uv[1], surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_unitvc(evsrf.snorm, sfnorm);
	um_cross(unit_axis, sfnorm, horz_dir);
	um_unitvc(horz_dir, horz_dir);

	if (horz_flip_flag == 1.0)	/* Going in negative dir */
		max_angle = angle * horz_uv;
	else			  	/* Going in positive dir */
		max_angle = angle * (1.0 - horz_uv);
	if (closed)
		max_angle = UM_TWOPI;
/*
..... If angle = 2Pi, scale the distance by 2Pi*r = circumfrance.
..... If not, just scale by fraction of circumfrance.
*/
	if ((horz_comp > (max_angle * radius)) || (closed && ver < 9.349))
		travel_angle = max_angle;
	else
	{
		if (rev_rad == 2)  /* REVOLVE */
			travel_angle = horz_comp / radius;
		else               /* RADIAL  */
			travel_angle = horz_comp / starting_radius;
	}
/*
..... The picture is a circle with horiz_vec being the y-axis and radial_vec
..... being the x-axis.  Go around the circle travel_angle and take the
..... x and y positions (r*cos , r*sin ).
*/
	um_vctmsc(horz_dir,  (horz_flip_flag * sin(travel_angle) * radius), horz_dir);
	um_vctmsc(radial_vec, (cos(travel_angle) * radius), radial_vec);
	um_vcplvc(horz_dir, radial_vec, move);
	um_vcplvc(axial_pt, move, surfpt);

	if (rev_rad == 2)  /* REVOLVE */
		horz_comp  -= travel_angle * radius;
	else               /* RADIAL  */
		horz_comp  -= travel_angle * starting_radius;

	if (horz_comp > UM_FUZZ)
	{
/*
..... Onto extention plane on far side.  It wrapped completely around the
..... surface and off the edge onto the other extention plane.
*/
		um_vcmnvc(surfpt, axial_pt, radial_vec);
		um_unitvc(radial_vec, radial_vec);
		um_cross (unit_axis, radial_vec, horz_dir);
		um_unitvc(horz_dir, horz_dir);
		um_vctmsc(horz_dir, (horz_flip_flag * horz_comp), horz_dir);
/*
..... Output end_pt and uv_pt2.
*/
		um_vcplvc(surfpt, horz_dir, end_pt);
		um_vctovc(end_pt, pts[2]);
		if (circ_uv)
		{
			if (horz_flip_flag > 0.0)
				uv_pt2[0] = 1.0;
			else
				uv_pt2[0] = 0.0;
			uv_pt2[1] = temp_uv[1];
		}
		else
		{
			uv_pt2[0] = temp_uv[0];
			if (horz_flip_flag > 0.0)
				uv_pt2[1] = 1.0;
			else
				uv_pt2[1] = 0.0;
		}

		return UU_SUCCESS;
	}
	else
	{
		um_vctovc(surfpt, end_pt);
		um_vctovc(end_pt, pts[2]);
/*
..... Output end_pt and uv_pt2.
*/
		sfpt(&packed_desc, end_pt, &isf, &side, &proj_u, &proj_v, ss);
		uv_pt2[0] = proj_u; uv_pt2[1] = proj_v;

		return UU_SUCCESS;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_revolve_patt_ruled (surf, pts, vec, angle, \
**                                    circ_uv, closed, uv_pt1, uv_pt2)
**  Given a ruled surface, starting surface point, distance vector,
**  and starting uv values, find and output the ending surface point and
**  corresponding u,v coordinates.
**  The movement will be in two parts.  The first is "vertical"; this
**  means taking the component of the distance vector along the "ruled"
**  direction, the second part is "horizontal".
**
**  There are some different situations for the horizontal part.  First, you
**  can start on the surface extention.  If so, you can travel the entire
**  horizontal component on the extention (towards or away from the main
**  surface) or you could make onto the main surface.  Second, you might
**  start on the surface.  The next issue is do you continue off the surface
**  onto the extention.  After all horizontal movement is finished, you
**  are done.  Compute the uv coordinates and output final position and
**  u,v's.
**
**    PARAMETERS
**       INPUT  :
**              surf      - UNIBASE key for surface pattern projected onto
**              pts[0]    - start point -- travel dist_vect from this point
**              vec[0]    - distance vector to go
**              vec[1]    - "ruled" direction at pts[0]
**              vec[2]    - projection plane normal
**              ruledinuv - 1 if ruled in u, i.e., v=const are lines, 0 else
**              closed    - UU_TRUE iff surface is closed in the "horizontal"
**                          direction
**              uv_pt1    - starting u,v coordinates for base point
**       OUTPUT :
**              pts[2]  -   ending point on surface (or extension)
**              uv_pt2  -   ending u,v coordinates for end_pt.
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Eduard Vaysleb  15 Jun 01
*********************************************************************/
int ncl_revolve_patt_ruled (surf,pts,vec,ruledinuv,closed, uv_pt1, uv_pt2)
struct NCL_fixed_databag *surf;
UM_coord pts[3];
UM_vector vec[5];
int ruledinuv;
int closed;
UM_2Dcoord uv_pt1, uv_pt2;
{
	int status;
	UU_REAL tol, dist, vert_comp, horz_comp, horz_flip_flag;
	UM_coord surfpt, temp_surfpt, arcpts[5];
	UM_vector dist_vec, axis, prj_pl_norm, unit_axis, unit_dir;
	UM_vector horz_dir, vert_dir, sfnorm;
	UM_2Dcoord temp_uv;
	UM_int2 itype;
	UM_real8 packed_desc;
	struct UM_evsrfout evsrf;
	UM_transf tfmat;

	uc_init_evsrfout (surf, &evsrf);
	status = uc_retrieve_transf(surf->key,tfmat);
	if (status != UU_SUCCESS) return UU_FAILURE;

	um_vctovc(pts[0], surfpt);
	um_vctovc(vec[0], dist_vec);
	um_vctovc(vec[1], axis);
	um_vctovc(vec[2], prj_pl_norm);
	um_vctovc_2d(uv_pt1, temp_uv);
	gettol (&tol);

	itype = 9;
	ptdesc(&surf->key, &itype, &packed_desc);

	um_unitvc (axis, unit_axis);
	um_unitvc (dist_vec, unit_dir);
	dist = um_mag (dist_vec);
/*
..... Get the vertical component of the direction vector, i.e., the
..... component along the "ruled" direction.
*/
	vert_comp = um_dot(unit_axis, dist_vec);
	um_vctmsc (unit_axis, vert_comp, vert_dir);

	status = uc_evsrf(UM_NORM, temp_uv[0], temp_uv[1], surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_unitvc(evsrf.snorm, sfnorm);

	um_cross(unit_axis, sfnorm, horz_dir);
	if (um_dot (horz_dir, dist_vec) < 0.0)
		horz_flip_flag = 1.0;
	else
		horz_flip_flag = -1.0;
	um_unitvc(horz_dir, horz_dir);
	horz_comp = sqrt (dist * dist - (vert_comp * vert_comp));
/*
..... Travel vertically (along the "ruled" direction)
*/
	if (fabs(vert_comp) > UM_FUZZ)
	{
		um_vctovc(vert_dir, arcpts[0]); /* dist_vect */
		if (vert_comp > 0.0)
			um_vctovc(horz_dir, arcpts[1]); /* prj_pl_norm */
		else
			um_vctmsc(horz_dir, -1.0, arcpts[1]); /* prj_pl_norm */
		um_vctovc(surfpt, arcpts[2]);

		status = ncl_go_along_surf_arclen(surf, arcpts, temp_uv, fabs(vert_comp));
		if(status != UU_SUCCESS) return UU_FAILURE;

		um_vctovc(arcpts[3], temp_surfpt); /* endpt after vertical travel */
	}
	else
		um_vctovc(surfpt, temp_surfpt);
/*
..... Travel horizontally (perpendicular to the "ruled" direction)
*/
	status = uc_evsrf(UM_NORM, temp_uv[0], temp_uv[1], surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_unitvc(evsrf.snorm, sfnorm);
	um_cross(unit_axis, sfnorm, horz_dir);
	um_unitvc(horz_dir, horz_dir);

	if (horz_flip_flag == 1.0)	/* Going in negative dir */
		um_vctmsc (horz_dir,-1.,horz_dir);

	um_vctovc (horz_dir,arcpts[0]);
	um_vctovc (unit_axis,arcpts[1]);
	um_vctovc (temp_surfpt,arcpts[2]);
	status = ncl_go_along_surf_arclen(surf, arcpts, temp_uv, horz_comp);

	if (status != UU_SUCCESS) return UU_FAILURE;

	um_vctovc (arcpts[3], pts[2]);
	uv_pt2[0] = temp_uv[0]; uv_pt2[1] = temp_uv[1];

	return (UU_SUCCESS);
}
