/*********************************************************************
**    NAME         :  neproj3.c
**       CONTAINS: functions to project NCL patterns onto primitive
**               surfaces (i.e. cone, cylinder, sphere, etc.).
**  These functions are for wrapping patterns of points or point-vectors
**  onto primitive surfaces.  Wrapping is maintaining the distance (arc
**  length) between the point and a given base (starting) point in the
**  original pattern.  The basic idea is to use algebraic calculations
**  rather than iterative methods to calculate the resulting (wrapped)
**  position.
**
**       ncl_patt_proj_prim
**       ncl_get_prim_type
**       ncl_get_prim_data
**       ncl_angle_cadd_obj
**       ncl_cone_extra_height
**       ncl_proj_dist_sphere
**       ncl_proj_dist_cylinder
**       ncl_proj_dist_cone
**       ncl_flat_extention
**       ncl_proj_cone_curve
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       neproj3.c , 25.1
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
**    E_FUNCTION     : ncl_patt_proj_prim(surf, pt_in, cplist, cnlist, ulist,
**                        flag, start,sf_begin, uv_pt0)
**    PARAMETERS
**        INPUT  :
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
**              start    = starting point in original pattern (attach point)
**              sf_begin = projection of start onto the surface
**                           all following points will be measured from here
**              uv_pt0   = parametric surface coordinates for sf_begin
**
**        OUTPUT :
**              cplist   = projection on surface of pt_in
**              cnlist   = Vectors if original pattern was made of point-vectors
**                         Vectors point along surface normal at corresponding
**                         surface point -> cplist[].  Direction (along or opposite)
**                         normal is determined by the flag -> side.
**              ulist    = UV-parametric coordinates for projected surf pts
**
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  22 Sep 00
*********************************************************************/
int ncl_patt_proj_prim(surf, proj, cplist, cnlist, ulist, flag, start,
			sf_begin, uv_pt0)
struct NCL_fixed_databag *surf;
NCL_proj_struc *proj;
int flag[6];
UM_coord start, sf_begin;
UU_LIST *cplist,*cnlist,*ulist;
UM_2Dcoord uv_pt0;
{
	int i, status, current,ist;
	int closdinu, closdinv, closed, primitive, circ_uv, uv_data[3];
	int side, attach_flag, pt_new_patt, first, second, last;
	struct UM_evsrfout evsrf;
	UM_transf tfmat;
	UU_REAL angle, alpha, prim_data[5];
	UM_coord center, surfpt, edge_pt, base, pts[3],uv_pt3;
	UM_vector axis, dist_vec, loc_proj_vec, prj_pl_norm,vc1;
	UM_vector zero_rad_vec, one_rad_vec, vec[5];
	UM_2Dcoord uv_pt1, uv_pt2;
	int vectype;

	uc_init_evsrfout (surf, &evsrf);
	status = uc_retrieve_transf(surf->key,tfmat);
	if (status != UU_SUCCESS) return UU_FAILURE;

	status = uc_evsrf(UM_NORM, uv_pt0[0], uv_pt0[1], surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_vctovc(evsrf.snorm, loc_proj_vec);

	attach_flag = proj->attach;
	pt_new_patt = flag [1];
	side        = flag [2];
	first       = flag [3];
	second      = flag [4];
	last        = flag [5];
	vectype      = proj->vectype;

	primitive = ncl_get_prim_data(surf, center, axis, prim_data, uv_data);
	if (primitive == UU_FAILURE) return UU_FAILURE;
	angle        = prim_data[0];
	alpha        = prim_data[3];
	closdinu     =   uv_data[0];
	closdinv     =   uv_data[1];
	circ_uv      =   uv_data[2];
	um_vctovc(center,pts[1]);
	um_vctovc(axis,  vec[1]);

	if (primitive == NCLSF_CYLINDER)
	{
		um_unitvc(axis, axis);

		status = uc_evsrf(UM_POINT, 0.0, 0.0, surf, tfmat, &evsrf);
		if (status != UM_VALID) return UU_FAILURE;
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

	ist = second;
	if (attach_flag == 2 || attach_flag == 4 || attach_flag == 5) ist = first;
	for (i = ist; i != last; i += (second - first))
	{
		current = i;

		um_vcmnvc(proj->inpt[current], start, dist_vec);

		um_cross(dist_vec, loc_proj_vec, prj_pl_norm);
		um_unitvc(prj_pl_norm, prj_pl_norm);

		um_vctovc(sf_begin,    pts[0]);
		um_vctovc(dist_vec,    vec[0]);
		um_vctovc(prj_pl_norm, vec[2]);
		um_vctovc_2d(uv_pt0,   uv_pt1);

		if (primitive == NCLSF_SPHERE)
		{
			um_vctovc_2d(uv_pt2, uv_pt1);
			status = ncl_proj_dist_sphere(surf->key, pts, vec, \
							uv_pt1, uv_pt2);
		}

		if (primitive == NCLSF_CYLINDER)
		{
			status = ncl_proj_dist_cylinder(surf->key, pts, vec, angle, \
					circ_uv, closed, uv_pt1, uv_pt2);
		}

		if (primitive == NCLSF_CONE)
		{
			status = ncl_proj_dist_cone(surf->key, pts, vec, alpha, angle, \
					circ_uv, closed, uv_pt1, uv_pt2);
		}
		if (primitive == NCLSF_PLANE)
		{
			status = ncl_proj_dist_plane(surf->key, pts, vec, uv_pt1, uv_pt2);
		}
		if (status != UU_SUCCESS) return UU_FAILURE;
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
				if (status != UM_VALID) return UU_FAILURE;
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
**    E_FUNCTION     : ncl_get_prim_type(surf,prim_param,closdinu,closdinv)
**       Returns the primitive type and parameters for a surface.
**    PARAMETERS
**       INPUT  :
**          surf       =  Pointer to surface.
**       OUTPUT :
**          prim_param =  Primitive data stored with surface.
**          closdinu   =  1 = Surface is closed in the U-direction.
**          closdinv   =  2 = Surface is closed in the V-direction.
**    RETURNS      : Primitive type or UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_prim_type(surf,prim_param,closdinu,closdinv)
struct NCL_fixed_databag *surf;
UU_REAL prim_param[9];
int *closdinu,*closdinv;
{
	int i,primitive;
	UM_int2 idx=169;
	UM_real8 ver;
/*
.....Return the Primitive type
.....and parameters based on the surface type
*/
	getsc(&idx, &ver);
	switch (surf->rel_num)
	{
		case NCL_SURF_REL:
		{
			struct NCL_surface_rec *sfrec;
			sfrec = (struct NCL_surface_rec *) surf;
			*closdinu = sfrec->closdinu;
			*closdinv = sfrec->closdinv;
			primitive = sfrec->primitive;
			for (i=0; i<9; i++) prim_param[i] = sfrec->prim_param[i];
			break;
		}
		case UM_RBSPLSRF_REL:
		{
			struct UM_rbsplsrf_rec *sfrec;
			sfrec = (struct UM_rbsplsrf_rec *) surf;
			*closdinu = sfrec->closdinu;
			*closdinv = sfrec->closdinv;
			primitive = sfrec->primitive;
			for (i=0; i<9; i++) prim_param[i] = sfrec->prim_param[i];
			break;
		}
		case NCL_REVSURF_REL:
		{
			struct NCL_revsurf_rec *sfrec;
			sfrec = (struct NCL_revsurf_rec *) surf;
			*closdinu = sfrec->closdinu;
			*closdinv = sfrec->closdinv;
			primitive = sfrec->primitive;
			if ((primitive == NCLSF_CONE || primitive == NCLSF_CYLINDER) &&
				ver >= 9.349)
			{
				for (i=0; i<9; i++) prim_param[i] = sfrec->prim_param[i];
			}
			else
			{
				primitive = NCLSF_REVOLV;
				um_vctovc(sfrec->pta, prim_param);
				um_vctovc(sfrec->vca, &prim_param[3]);
				prim_param[6] = sfrec->sa;
				prim_param[7] = sfrec->ta;
			}
			break;
		}
		default:
			return UU_FAILURE;
	}
	return(primitive);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_prim_data (surf, center, axis, prim_data, uv_data)
**       Supply a pointer to a surface structure, and this will collect
**       a bunch of data about the primitive.  The center is the center
**       of a sphere or the base point for a cylinder or cone.  The base
**       point is the point on the axis the is closest to point on surface
**       at (u=0.0, v=0.0).  This is the lowest point on the axis when
**       up direction is increasing parameters.
**       The angle of completion is for when the primitive is not complete.
**       It doesn't go all the way around.
**       Alpha is the angle that the cone opens (from axis to surface).
**       Extra height is when the cone is truncated vertically (chop off
**       the top of the cone -> how tall was the piece chopped off).
**       Circ_uv describes the orientation of the parameters (is u the
**       verical (or latitude on the sphere) is it the horizontal
**       (longitude on the sphere)).
**    PARAMETERS
**       INPUT  :
**              surf         =  pointer to surface
**        OUTPUT :
**              center       =  spatial coodinates for center (or base pt
**                                on axis of primitive)
**              axis         =  3D vector for axis of symmetry (or from
**                                south pole to center on a sphere)
**              prim_data[0] =  angle of completion
**                                (Not all primitives are totally closed.)
**                                (0 <= angle <= 2Pi)
**              prim_data[1] =  radius
**              prim_data[2] =  height
**                                cone -> from base to apex
**                                sphere -> not meaningful
**              prim_data[3] =  alpha
**                                cone -> angle of opening (0 <= alpha <= Pi/2)
**                                cylinder, sphere -> not meaningful
**              prim_data[4] =  extra_height
**                                cone -> distance from apex to point on axis
**                                        closest to top of surface
**                                        (trucated cone)
**                                cylinder, sphere -> not meaningful
**                uv_data[0] =  closdinu
**                uv_data[1] =  closdinv
**                uv_data[2] =  circ_uv
**                                UU_TRUE -> v goes along axis and
**                                           u goes along circular arc
**                                UU_FALSE -> opposite
**
**    RETURNS      : primitive or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  1 Nov 00
*********************************************************************/
int ncl_get_prim_data(surf, center, axis, prim_data, uv_data)
struct NCL_fixed_databag *surf;
UM_coord center;
UM_vector axis;
UU_REAL prim_data[5];
int uv_data[3];
{
	int i, status, closdinu, closdinv, primitive, circ_uv;
	struct UM_evsrfout evsrf;
	UM_transf tfmat;
	UU_REAL angle, radius, height, alpha, extra_height, prim_param[9];
	UM_coord pt00, pt01, pt10, pt11, half_u, half_v;
/*
.....Get the primitive type
*/
	primitive = ncl_get_prim_type(surf,prim_param,&closdinu,&closdinv);
	if (primitive == UU_FAILURE) return(primitive);
/*
.....Store the primitive data
*/
	if (primitive == NCLSF_RULED)
	{
		prim_data [0] = prim_param[0];
		for (i = 1; i < 5; i++)
			prim_data [i] = 0.0;
		uv_data [0] = closdinu;
		uv_data [1] = closdinv;
		uv_data [2] = 0.;
		return primitive;
	}

	if (primitive == NCLSF_PLANE)
	{
		um_vctovc(&prim_param[4],center);
		um_vctovc(&prim_param[0],axis);

		for (i = 0; i < 5; i++)
			prim_data [i] = 0.0;
		for (i = 0; i < 3; i++)
			uv_data [i]   = 0;

		return primitive;
	}

	uc_init_evsrfout (surf, &evsrf);
	status = uc_retrieve_transf(surf->key,tfmat);
	if (status != UU_SUCCESS) return UU_FAILURE;

	um_xyztovc (prim_param[0],prim_param[1],prim_param[2], center);
	um_xyztovc (prim_param[3],prim_param[4],prim_param[5], axis);
	um_unitvc  (axis, axis);

	status = uc_evsrf(UM_POINT, 0.0, 0.0, surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_vctovc(evsrf.sp, pt00);
	status = uc_evsrf(UM_POINT, 0.0, 1.0, surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_vctovc(evsrf.sp, pt01);
	status = uc_evsrf(UM_POINT, 1.0, 0.0, surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_vctovc(evsrf.sp, pt10);
	status = uc_evsrf(UM_POINT, 1.0, 1.0, surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_vctovc(evsrf.sp, pt11);
	status = uc_evsrf(UM_POINT, 0.5, 0.0, surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_vctovc(evsrf.sp, half_u);
	status = uc_evsrf(UM_POINT, 0.0, 0.5, surf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_vctovc(evsrf.sp, half_v);
	status = ncl_angle_cadd_obj(center, axis, pt00, pt01, pt10, \
		half_u, half_v, &circ_uv, &angle);
	if (status != UU_SUCCESS) return UU_FAILURE;

	if (primitive == NCLSF_CONE)
	{
		status = ncl_cone_extra_height (center, axis, pt00, pt01, pt10, \
						circ_uv, &extra_height);
		if (status == UU_FAILURE) return UU_FAILURE;
/*
..... Get angle of cone.  That is, get angle based at the apex,
..... from the axis to the surface (0 <= alpha <= Pi/2).
*/
		alpha = prim_param[6];
		height = prim_param[7];
		radius = 0.0;

		if (alpha < 0.0) alpha *= -1.0;
		alpha = fmod (alpha, UM_TWOPI);
		if (alpha > UM_PI) alpha = UM_TWOPI - alpha;
		if (alpha > UM_HALFPI) alpha = UM_PI - alpha;
		if (fabs(UM_HALFPI - alpha) < UM_FUZZ) return UU_FAILURE;
		if (fabs(alpha) < UM_FUZZ) return UU_FAILURE;
	}
	else
	{
		alpha = 0.0;
		extra_height = 0.0;
	}

	if (primitive == NCLSF_SPHERE)
	{
		um_vcmnvc(center, pt00, axis);
		radius = prim_param[3];
		if (closdinu)
			circ_uv = UU_TRUE;
		else
			circ_uv = UU_FALSE;
		height = 0.0;
	}

	if (primitive == NCLSF_CYLINDER)
	{
		um_nptln(pt00, center, axis, center);
		um_unitvc(axis, axis);
		radius = prim_param[6];
		height = prim_param[7];
	}
	else if (primitive == NCLSF_REVOLV)
		angle = fabs(prim_param[7]-prim_param[6]);

	prim_data [0] = angle;
	prim_data [1] = radius;
	prim_data [2] = height;
	prim_data [3] = alpha;
	prim_data [4] = extra_height;
	uv_data [0]   = closdinu;
	uv_data [1]   = closdinv;
	uv_data [2]   = circ_uv;

	return(primitive);
}

/*********************************************************************
**    E_FUNCTION     : ncl_angle_cadd_obj (center, axis, pt00, pt01, pt10 \
**				half_u, half_v, circ_uv, angle)
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  30 Aug 00
*********************************************************************/
int ncl_angle_cadd_obj(center, axis, pt00, pt01, pt10, half_u, half_v, circ_uv, angle)
UM_coord center, pt00, pt01, pt10, half_u, half_v;
UM_vector axis;
int *circ_uv;
UU_REAL *angle;
{
	int which_dir; /* u (=UU_TRUE) or v (=UU_FALSE) */
	UU_REAL height;
	UM_vector u_line, v_line, height_vect, unit_axis;
	UM_vector x, r1, r2, rad_ang, half_ang;
	UM_coord center_pt, halfway;

/*
..... One of these lines must be perpendicular to the axis.
..... This tells if the axis runs along u or along v.
*/
	um_vcmnvc (pt00, pt10, u_line);
	um_vcmnvc (pt00, pt01, v_line);
	which_dir = um_vcperp (u_line, axis);
	if ((which_dir != UU_TRUE) && (which_dir != UU_FALSE))
	{
		*angle = 0.0;
		return UU_FAILURE;
	}
	*circ_uv = which_dir;

	um_unitvc (axis, unit_axis);
	um_vcmnvc (pt00, center, x);
	height = um_dot (unit_axis, x);
	um_vctmsc (unit_axis, height, height_vect);
	um_vcplvc (center, height_vect, center_pt);

	um_vcmnvc (pt00, center_pt, r1);
	if (which_dir == UU_TRUE)
	{
		um_vcmnvc (pt10, center_pt, r2);
		um_vcmnvc (half_u, center_pt, halfway);
	}
	else
	{
		um_vcmnvc (pt01, center_pt, r2);
		um_vcmnvc (half_v, center_pt, halfway);
	}
	*angle = um_angle (r1, r2);
/*
..... Don't know CW/CCW direction, so compare cross-products of
..... vector from center to 1st point (r1) X vector from center to 2nd
..... point (r2).  The other cross-product is vector from center to
..... 1st point (r1) X vector from center to middle of arc containing
..... both 1st and 2nd points (halfway).
.....
..... If the dot product of the two cross-products is positive, both
..... cross-products point in same direction, and both r2 and halfway
..... are on the same side of the line generated by r1.  Angle < Pi.
..... If the dot product is negative, r2 is on one side of the line,
..... while halfway is on the other side.  Angle > Pi.
*/
	um_cross (r1, r2, rad_ang);
	if (um_mag (rad_ang) < UM_FUZZ)
	{
		if (um_dot (r1, r2) > 0.0) {*angle = UM_TWOPI; return UU_SUCCESS;}
		*angle = UM_PI;
		return UU_SUCCESS;
	}
	um_cross (r1, halfway, half_ang);
	if (um_dot (rad_ang, half_ang) < 0.0)
		*angle = UM_TWOPI - *angle;
	/* Otherwise, leave the angle unchanged. */

	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cone_extra_height(apex, axis, pt00, pt01, pt10, \
**						circ_uv, height)
**    PARAMETERS
**       INPUT  :
**			apex	=	coordinates for apex of cone
**			axis	=	vector for the axis of cone
**			pt00, pt01, pt10 = coordinates on cone surface
**			circ_uv =	flag telling which (u or v) goes along
**					axis and which goes along the circular arc.
**       OUTPUT :
**			height	=	distance along axis from apex to start of surf.
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  7 Sep 00
*********************************************************************/
int ncl_cone_extra_height(apex, axis, pt00, pt01, pt10, circ_uv, height)
UM_coord apex, pt00, pt01, pt10;
UM_vector axis;
int circ_uv;
UU_REAL *height;
{
	UU_REAL dist0, dist1;
	UM_vector dist_vect, unit_axis, x;
/*
..... Determine if the vectical parameter (u or v - whichever goes along
..... the vertical axis of the cone) = 0 or = 1 when closest to the apex.
..... In other words, vertical parameter increases or decreases as you
..... go away from the apex.  This part determines which of those happens.
..... It should decrease. (e.g. u=1.0 closer to apex than u=0.0) Check to be sure.
*/
	um_vcmnvc(pt00, apex, dist_vect);
	dist0 = um_mag(dist_vect);
/*
..... circ_uv = UU_TRUE -> v goes along axis and u goes along circular arc
..... circ_uv = UU_FALSE -> opposite
*/
	if(circ_uv == UU_TRUE)
		um_vcmnvc(pt01, apex, dist_vect);
	if (circ_uv == UU_FALSE)
		um_vcmnvc(pt10, apex, dist_vect);
	if ((circ_uv != UU_TRUE) && (circ_uv != UU_FALSE))
		return UU_FAILURE;
	dist1 = um_mag (dist_vect);

	um_unitvc (axis, unit_axis);
	if (dist0 <= dist1)
		um_vcmnvc (pt00, apex, x);
	if (dist0 > dist1)
	{
		if (circ_uv == UU_TRUE)
			um_vcmnvc (pt01, apex, x);
		if (circ_uv == UU_FALSE)
			um_vcmnvc (pt10, apex, x);
	}

	*height = fabs (um_dot (unit_axis, x));

	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : ncl_proj_dist_sphere (radius, dist, uv_pt1, uv_pt2)
**    PARAMETERS
**       INPUT  :
**              surf_key  =   UNIBASE key for surface pattern projected onto
**              pts[0]    =   start point -- travel dist_vect from this point
**              pts[1]    =   base point of the cylinder (point on axis closest
**                               to (u = 0, v = 0) )
**              vec[0]    =   distance vector to go
**              vec[1]    =   axis of cylinder
**              vec[2]    =   projection plane normal
**              uv_pt1    =   starting u,v coordinates for base point
**
**              vec[3], vec[4] ignored
**
**       OUTPUT :
**              pts[2]    =   ending point on surface (or extension)
**              uv_pt2    =   ending u,v coordinates for end_pt.
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  30 Aug 00
*********************************************************************/
int ncl_proj_dist_sphere(surf_key, pts, vec, uv_pt1, uv_pt2)
UU_KEY_ID surf_key;
UM_coord pts[3];
UM_vector vec[5];
UM_2Dcoord uv_pt1, uv_pt2;
{
	UU_REAL tol, dist, radius, angle, x, y, stuff;
	UM_coord center, surfpt, start, axialpt;
	UM_vector radial_vec, unit_radial, norm, perp_vec, move_vec;
	UM_vector unit_dist, polar_vec, dist_vec, prj_pl_norm;
	UM_real4 side, proj_u, proj_v, ss[9];
	UM_int2 itype, isf;
	UM_real8 packed_desc;

	um_vctovc(pts[0], start);
	um_vctovc(pts[1], center);
	um_vctovc(vec[0], dist_vec);
	um_vctovc(vec[1], polar_vec);
	um_vctovc(vec[2], prj_pl_norm);

	gettol(&tol);
	dist = um_mag (dist_vec);
	um_unitvc (prj_pl_norm, norm);
	um_unitvc (dist_vec, unit_dist);

	um_nptpln(start, center, norm, axialpt);
	stuff = um_dcccc(start, axialpt) / um_dcccc(start, center);
	if (stuff < (10.0*tol))
		um_vcmnvc(start, center, radial_vec);
	else
		um_vcmnvc(start, axialpt, radial_vec);
	um_unitvc(radial_vec, unit_radial);
	radius = um_dot (radial_vec);
	um_vctmsc(unit_dist, radius, perp_vec);
/*
..... Fraction of the perimeter of the circle to go around (measured in
..... radians).
*/
	angle = dist / radius;
	x = cos(angle);
	y = sin(angle);
	um_avcplbvc(x, radial_vec, y, perp_vec, move_vec);
	um_vcplvc(center, move_vec, surfpt);
/*
..... Do the Fortran thing.  itype = 9 -> surface.  isf = 3 -> check surface
..... (rather than part surface or drive surface).  side = 0 first time
..... thru the function; however, I only call it once while inside of this
..... function.
*/
	itype = 9; isf =3; side = 0.0;
	ptdesc(&surf_key, &itype, &packed_desc);
	proj_u = uv_pt1[0]; proj_v = uv_pt1[1];
	sfpt(&packed_desc, surfpt, &isf, &side, &proj_u, &proj_v, ss);

	um_vctovc(surfpt, pts[2]);
	uv_pt2[0] = proj_u; uv_pt2[1] = proj_v;

	return UU_SUCCESS;
}


/*********************************************************************
**    E_FUNCTION     : ncl_proj_dist_cylinder (surf_key, base, axis_vec, \
**                         start_rad_vec, dist_vec, prj_pl_norm, angle, \
**                         start_pt, end_pt, circ_uv, uv_pt1, uv_pt2)
**    PARAMETERS
**       INPUT  :
**              surf_key  =   UNIBASE key for surface pattern projected onto
**              pts[0]    =   start point -- travel dist_vect from this point
**              pts[1]    =   base point of the cylinder (point on axis closest
**                               to (u = 0, v = 0) )
**              vec[0]    =   distance vector to go
**              vec[1]    =   axis of cylinder
**              vec[2]    =   projection plane normal
**              vec[3]    =   radius vector from base point to (u = 0 , v = 0)
**              vec[4]    =   radius vector from axial point to
**                               (u/v = 0, v/u = 1)
**              angle     =   angle of completion (if complete -> 2Pi)
**                               for example a hemisphere angle = Pi
**              circ_uv   =
**              closed    =   UU_TRUE if complete (angle should = 2PI)
**              uv_pt1    =   starting u,v coordinates for base point
**       OUTPUT :
**              pts[2]    =   ending point on surface (or extension)
**              uv_pt2    =   ending u,v coordinates for end_pt.
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  3 Nov 00
*********************************************************************/
int ncl_proj_dist_cylinder(surf_key, pts, vec, angle, circ_uv, closed, \
			uv_pt1, uv_pt2)
UU_KEY_ID surf_key;
UM_coord pts[3];
UM_vector vec[5];
UU_REAL angle;
UM_2Dcoord uv_pt1, uv_pt2;
int circ_uv, closed;
{
	int status, intersect;
	UU_REAL radius, dist, horiz_comp, vert_comp, horz_uv, travel_angle;
	UU_REAL horiz_flip_flag, max_angle;
	UM_vector unit_axis, horiz_vec, horz_move, vert_move, move;
	UM_vector axis_vec, zero_rad_vec, one_rad_vec, dist_vec, prj_pl_norm;
	UM_vector radial_vec, unit_dir;
	UM_coord base, end_pt, axial_pt, intersect_pt, surfpt, ext_pts[4], edge_pt;
	UM_real4 side, proj_u, proj_v, ss[9];
	UM_int2 itype, isf;
	UM_real8 packed_desc;

	um_vctovc(pts[0], surfpt);
	um_vctovc(pts[1], base);
	um_vctovc(vec[0], dist_vec);
	um_vctovc(vec[1], axis_vec);
	um_vctovc(vec[2], prj_pl_norm);
	um_vctovc(vec[3], zero_rad_vec);
	um_vctovc(vec[4], one_rad_vec);
/*
..... Do the Fortran thing.  itype = 9 -> surface.  isf = 3 -> check surface
..... (rather than part surface or drive surface).  side = 0 first time
..... thru the function; however, I only call it once while inside of this
..... function.
*/
	itype = 9; isf =3; side = 0.0;
	ptdesc(&surf_key, &itype, &packed_desc);
	proj_u = uv_pt1[0]; proj_v = uv_pt1[1];

	um_unitvc (axis_vec, unit_axis);
	radius = 0.5 * (um_mag(zero_rad_vec) + um_mag(one_rad_vec));
	dist = um_mag (dist_vec);
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
		if (horz_uv == 1.0)
			um_vcplvc(base, one_rad_vec, edge_pt);
		if (horz_uv == 0.0)
			um_vcplvc(base, zero_rad_vec, edge_pt);
		um_ilnln1(edge_pt, unit_axis, surfpt, \
				unit_dir, &intersect, intersect_pt);

		um_vctovc(surfpt, ext_pts[0]);
		um_vctovc(intersect_pt, ext_pts[1]);
		um_vctovc(dist_vec, ext_pts[2]);
		status = ncl_flat_extention (surf_key, ext_pts, intersect, \
						circ_uv, uv_pt1, uv_pt2);
		um_vctovc(ext_pts[2], dist_vec);
		um_vctovc(ext_pts[3], surfpt);
		if (status == UU_TRUE)
		{
/*
..... Finished before reaching the curved part of surface (or extention).
*/
			um_vctovc(surfpt, pts[2]);
			return UU_SUCCESS;
		}
	}
/*
..... Moving on surface (or at least curving part of extension surf).
..... Take care of vertical movement (up or down the axis).
*/
	vert_comp  = um_dot (dist_vec, unit_axis);
	um_vctmsc(unit_axis, vert_comp, vert_move);
	um_vcplvc(surfpt, vert_move, surfpt);
	um_nptln(surfpt, base, unit_axis, axial_pt);
/*
..... Get the horizontal component (go around cylinder).
*/
	um_vcmnvc(surfpt, axial_pt, radial_vec);
	um_unitvc (radial_vec, radial_vec);
	um_cross (unit_axis, radial_vec, horiz_vec);
	um_unitvc(horiz_vec, horiz_vec);
	if (um_dot(dist_vec, horiz_vec) < 0.0)
		horiz_flip_flag = -1.0;
	else
		horiz_flip_flag =  1.0;
	horiz_comp = sqrt(dist * dist - vert_comp * vert_comp);
	um_vctmsc(horiz_vec, horiz_flip_flag, horiz_vec);

	if (horiz_flip_flag > 0.0)
		max_angle = (1.0 - horz_uv) * angle;  /* go towards u/v = 1.0 */
	else
		max_angle = horz_uv * angle;          /* go towards u/v = 0.0 */
	if(closed)
		max_angle = UM_TWOPI;
/*
..... If angle = 2Pi, scale the distance by 2Pi*r = circumfrance.
..... If not, just scale by fraction of circumfrance.
*/
	if (horiz_comp > (max_angle * radius))
		travel_angle = max_angle;
	else
		travel_angle = horiz_comp / radius;
/*
..... The picture is a circle with horiz_vec being the y-axis and radial_vec
..... being the x-axis.  Go around the circle travel_angle and take the
..... x and y positions (r*cos , r*sin ).
*/
	um_vctmsc(horiz_vec,  (sin(travel_angle) * radius), horz_move);
	um_vctmsc(radial_vec, (cos(travel_angle) * radius), radial_vec);
	um_vcplvc(horz_move, radial_vec, move);
	um_vcplvc(axial_pt, move, surfpt);

	horiz_comp  -= travel_angle * radius;

	if (horiz_comp > UM_FUZZ)
	{
/*
..... Went all the way across the surface to the other flat extention plane.
*/
		um_vcmnvc(surfpt, axial_pt, radial_vec);
		um_unitvc(radial_vec, radial_vec);
		um_cross (unit_axis, radial_vec, horiz_vec);
		um_unitvc(horiz_vec, horiz_vec);
		um_vctmsc(horiz_vec, (horiz_flip_flag * horiz_comp), horz_move);

		um_vcplvc(surfpt, horz_move, end_pt);
		um_vctovc(end_pt, pts[2]);

		sfpt(&packed_desc, end_pt, &isf, &side, &proj_u, &proj_v, ss);
		if (circ_uv)
		{
			if (horiz_flip_flag > 0.0)
				uv_pt2[0] = 1.0;
			else
				uv_pt2[0] = 0.0;
			uv_pt2[1] = proj_v;
		}
		else
		{
			uv_pt2[0] = proj_u;
			if (horiz_flip_flag > 0.0)
				uv_pt2[1] = 1.0;
			else
				uv_pt2[1] = 0.0;
		}

		return UU_SUCCESS;
	}
	else
	{
/*
..... On the surface (or at least the curving part of the extention
..... surface).  Not on the flat extention planes.  Output end_pt.
..... Make a guess at some u,v coordinates and project point onto
..... surface.  If you don't give a good guess at starting u,v , you
..... might get point on other side of surface (but still is perpendicular
..... to -> surf norm goes thru) the point.
*/
		um_vctovc(surfpt, end_pt);
		um_vctovc(end_pt, pts[2]);
		if (circ_uv)
		{
			if (horiz_flip_flag > 0.0)
				proj_u = horz_uv + (travel_angle / max_angle);
			else
				proj_u = horz_uv - (travel_angle / max_angle);
		}
		else
		{
			if (horiz_flip_flag > 0.0)
				proj_v = horz_uv + (travel_angle / max_angle);
			else
				proj_v = horz_uv - (travel_angle / max_angle);
		}
		sfpt(&packed_desc, end_pt, &isf, &side, &proj_u, &proj_v, ss);
		uv_pt2[0] = proj_u; uv_pt2[1] = proj_v;

		return UU_SUCCESS;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_proj_dist_cone (surf_key, pts, vec, alpha, angle, \
**                                    circ_uv, closed, uv_pt1, uv_pt2)
**    PARAMETERS
**       INPUT  :
**              surf_key  =   UNIBASE key for surface pattern projected onto
**              pts[0]    =   start point -- travel dist_vect from this point
**              pts[1]    =   apex of cone
**              vec[0]    =   distance vector to go
**              vec[1]    =   axis of cone
**              vec[2]    =   projection plane normal
**              vec[3]    =   radius vector from axis point to (u = 0 , v = 0)
**              vec[4]    =   radius vector from axis point to
**                               (u/v = 0, v/u = 1)
**              alpha     =   the angle that the cone opens (based at apex,
**                               from axis to surface
**              angle     =   angle of completion (if complete -> 2Pi)
**                               for example a hemisphere angle = Pi
**              circ_uv   =
**              closed    =   UU_TRUE if complete (angle should = 2PI)
**              uv_pt1    =   starting u,v coordinates for base point
**       OUTPUT :
**              pts[2]    =   ending point on surface (or extension)
**              uv_pt2    =   ending u,v coordinates for end_pt.
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  30 Aug 00
*********************************************************************/
int ncl_proj_dist_cone(surf_key, pts, vec, alpha, angle, circ_uv, closed, \
			uv_pt1, uv_pt2)
UU_KEY_ID surf_key;
UM_coord pts[3];
UM_vector vec[5];
UU_REAL alpha, angle;
int circ_uv, closed;
UM_2Dcoord uv_pt1, uv_pt2;
{
	int status, intersect;
	UM_vector unit_axis, unit_dir, edge, vert_dir, horz_dir, base_to_apex;
	UM_vector radial_vec, move, axis, dist_vec, prj_pl_norm;
	UM_vector zero_rad_vec, one_rad_vec, temp_vec, curve_vec[2];
	UM_coord apex, start_pt, end_pt, surfpt, axial_pt, intersect_pt, temp_surfpt;
	UM_coord ext_pts[4], curve_pts[3];
	UU_REAL dist, horz_uv, vert_comp, horz_comp, max_angle;
	UU_REAL vert_flip_flag, horz_flip_flag, sin_alpha, cos_alpha;
	UU_REAL stuff, radius, test_ang;
	UU_REAL curve_num[3], tol;
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
	radius = um_mag(radial_vec);
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
	horz_comp = sqrt (dist * dist - (vert_comp * vert_comp));
/*
..... Make fraction of circle to go around.  I have already done the vertical part.
.....
..... Need to determine if wraps around to extention plane on other side.
..... In general, I need to find out how much horizontal angular distance
..... the curve travels.  If it travels more than ->  angle  (the completion
..... angle of the cone.  In other words, if the cone is complete  angle = 2PI.
..... It is the angle that you are asked when you make the cone), then the
..... curve travelled farther than the surface: it is on the extension plane.
.....
..... Horizontal component actually breaks down into part that go around the
..... circle perpendicular to the axis and another part the will go up or down
..... the axis.  The horizontal part is the component that is in the tangent
..... plane at that point and is perdendicular to the axis.  As you move along
..... the horizontal vector, the vector wraps down the side of the cone until
..... it picks up a vertical component.Take that into account with the cos_alpha
.....               test_ang = (horz_comp/cos_alpha) / radius
.....
*/
	test_ang = (horz_comp * cos_alpha) / radius;

	if (horz_flip_flag == 1.0)	/* Going in positive dir */
		max_angle = angle * (1.0 - horz_uv);
	else			  	/* Going in negative dir */
		max_angle = angle * horz_uv;
	if (closed)
		max_angle = UM_TWOPI;

	um_vctovc(apex, curve_pts[0]);
	um_vctovc(horz_dir, curve_vec[0]);
	um_vctovc(unit_axis, curve_vec[1]);
	curve_num[1] = alpha;
	curve_num[2] = horz_flip_flag;
/*
..... Onto extention plane on far side.  It wrapped completely around the
..... surface and off the edge onto the other extention plane.
*/
	if ((test_ang > max_angle) && (!closed))
	{
		vert_comp *= (max_angle / test_ang);
		horz_comp *= (max_angle / test_ang);

		um_unitvc(vert_dir, vert_dir);
		um_vctmsc(vert_dir, vert_comp, vert_dir);
		um_vcplvc(surfpt, vert_dir, surfpt);
/*
..... Moved up this smaller amount.  Now do horizontal.
*/
		um_vctovc(surfpt, curve_pts[1]);
		curve_num[0] = max_angle;

		ncl_proj_cone_curve(curve_pts, curve_vec, curve_num);

		um_vctovc(curve_pts[2], surfpt);
/*
..... Made it to far edge of surface (or curving part of extention).
..... Determine how much further to go on flat extention plane.
..... Also, must determine direction.
*/
		dist -= sqrt ((horz_comp * horz_comp) + (vert_comp * vert_comp));
		if (dist < 0.0)
			return UU_FAILURE;

		um_vcmnvc(apex, surfpt, vert_dir);
		um_unitvc(vert_dir, vert_dir);
		um_nptln (surfpt, apex, unit_axis, axial_pt);
		um_vcmnvc(surfpt, axial_pt, radial_vec);
		um_unitvc(radial_vec, radial_vec);
		um_cross (unit_axis, radial_vec, horz_dir);

		um_vctmsc(horz_dir, (horz_flip_flag * horz_comp), horz_dir);
		um_vctmsc(vert_dir, (vert_flip_flag * vert_comp), vert_dir);
		um_vcplvc(horz_dir, vert_dir, move);
		um_unitvc(move, move);
		um_vctmsc(move, dist, move);

		um_vcplvc(surfpt, move, end_pt);
		um_vctovc(end_pt, pts[2]);

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
		um_vctovc(temp_surfpt, curve_pts[1]);
		curve_num[0] = test_ang;

		ncl_proj_cone_curve(curve_pts, curve_vec, curve_num);

		um_vctovc(curve_pts[2], end_pt);
		um_vctovc(end_pt, pts[2]);

		sfpt(&packed_desc, end_pt, &isf, &side, &proj_u, &proj_v, ss);
		uv_pt2[0] = proj_u; uv_pt2[1] = proj_v;

		return UU_SUCCESS;
	}
}



/*********************************************************************
**    E_FUNCTION     : ncl_flat_extention(surf_key, pts, intersect, circ_uv,\
**                                                  uv_pt1, uv_pt2)
**    PARAMETERS
**       INPUT  :
**              surf_key  =   UNIBASE key for surface pattern projected onto
**              pts[0]    =   start point -- travel dist_vect from this point
**              pts[1]    =   intersection point (dist_vec and edge of surf)
**              pts[3]    =   dist_vec
**              circ_uv   =
**              uv_pt1    =   starting u,v coordinates for base point
**       OUTPUT :
**              pts[3]    =   ending point on surface (or extension)
**              uv_pt2    =   ending u,v coordinates for end_pt.
**    RETURNS      : UU_TRUE or UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  13 Nov 00
*********************************************************************/
int ncl_flat_extention(surf_key, pts, intersect, circ_uv, uv_pt1, uv_pt2)
UU_KEY_ID surf_key;
int intersect, circ_uv;
UM_coord pts[4];
UM_2Dcoord uv_pt1, uv_pt2;
{
	UU_REAL dist, flat_dist;
	UM_coord surfpt, intersect_pt, end_pt;
	UM_vector dist_vec, intersect_vec;
	UM_real4 side, proj_u, proj_v, ss[9];
	UM_int2 itype, isf;
	UM_real8 packed_desc;
/*
..... Do the Fortran thing.  itype = 9 -> surface.  isf = 3 -> check surface
..... (rather than part surface or drive surface).  side = 0 first time
..... thru the function; however, I only call it once while inside of this
..... function.
*/
	itype = 9; isf =3; side = 0.0;
	ptdesc(&surf_key, &itype, &packed_desc);
	proj_u = uv_pt1[0]; proj_v = uv_pt1[1];

	um_vctovc(pts[0], surfpt);
	um_vctovc(pts[1], intersect_pt);
	um_vctovc(pts[2], dist_vec);

	dist = um_mag (dist_vec);

	if (intersect > 0)
	{
		um_vcmnvc(intersect_pt, surfpt, intersect_vec);
		flat_dist = um_mag(intersect_vec);
/*
..... Goes away from the surface (on flat part of extention -> extention
..... plane) or finishes before reaching surface.  If so, go to endpt of
..... move and finish.
*/
		if (((um_dot(intersect_vec, dist_vec) + UM_FUZZ) < 0.0) || \
		                          ((flat_dist + UM_FUZZ) > dist))
			um_vcplvc(surfpt, dist_vec, end_pt);
		else
		{
/*
..... Does make it to surface (or at least to curved part of extention).
..... Get remaining distance and rescale distance vector.
*/
			um_vctovc(intersect_pt, end_pt);
			dist -= flat_dist;
			um_unitvc(dist_vec, dist_vec);
			um_vctmsc(dist_vec, dist, dist_vec);

			um_vctovc(dist_vec, pts[2]);
			um_vctovc(end_pt,   pts[3]);

			sfpt(&packed_desc, end_pt, &isf, &side, &proj_u, &proj_v, ss);
			if (circ_uv)
			{
				uv_pt2[0] = uv_pt1[0];
				uv_pt2[1] = proj_v;
			}
			else
			{
				uv_pt2[0] = proj_u;
				uv_pt2[1] = uv_pt1[1];
			}

			return UU_FALSE;
		}
	}
/*
..... Goes straight up or down surface (never intersects edge).
*/
	else
		um_vcplvc(surfpt, dist_vec, end_pt);
/*
..... Did not get to curved part of surface.  Travel dist_vect.
..... Output end_pt and find corresponding uv.
*/
	um_vctovc(end_pt, pts[3]);

	sfpt(&packed_desc, end_pt, &isf, &side, &proj_u, &proj_v, ss);
	if (circ_uv)
	{
		uv_pt2[0] = uv_pt1[0];
		uv_pt2[1] = proj_v;
	}
	else
	{
		uv_pt2[0] = proj_u;
		uv_pt2[1] = uv_pt1[1];
	}

	return UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION     : ncl_proj_cone_curve(pts, vec, num)
**    PARAMETERS
**       INPUT  :
**              pts[0]    =   cone apex
**              pts[1]    =   surfpt - point on cone surface to start
**              pts[3]    =   end_pt - point on cone surface at end
**              vec[0]    =   horz_dir -> starting direction around cone
**              vec[1]    =   cone axis
**              num[0]    =   travel_angle - how far to travel (perp to axis)
**              num[1]    =   alpha - angle from cone axis to cone surface
**              num[2]    =   horiz_flip_flag +-1.0 for generating horizontal
**                               vectors going the proper direction
**       OUTPUT :
**              pts[2]    =   end_pt - ending point on surface (or extension)
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  13 Nov 00
*********************************************************************/
int ncl_proj_cone_curve(pts, vec, num)
UM_coord pts[3];
UM_vector vec[2];
UU_REAL num[3];
{
	UM_coord apex, surfpt, axial_pt, end_pt;
	UM_vector horz_dir, unit_axis, move, radial_vec, vert_dir;
	UU_REAL travel_ang, alpha, horz_flip_flag, height;
	UU_REAL cos_alpha, sin_alpha, cot_alpha, radius;
	UU_REAL stuff, max_dist_down, surf_vec_mag;

	um_vctovc(pts[0], apex);
	um_vctovc(pts[1], surfpt);
	um_unitvc(vec[0], horz_dir);
	um_unitvc(vec[1], unit_axis);
	travel_ang = num[0];
	alpha = num[1];
	horz_flip_flag = num[2];

	cos_alpha = cos(alpha);
	sin_alpha = sin(alpha);
	cot_alpha = cos_alpha / sin_alpha;

	um_nptln(surfpt, apex, unit_axis, axial_pt);
	um_vcmnvc(surfpt, axial_pt, radial_vec);
	radius = um_mag (radial_vec);
	um_unitvc(radial_vec, radial_vec);
/*
..... I have my radius and horizontal directions at the starting
..... point, axial point, and radial_vec.
.....
..... The picture is a circle with horz_dir being the y-axis and radial_vec
..... being the x-axis.  Go around the circle test_angle and take the
..... x and y positions (r*cos , r*sin ).
*/
	um_vctmsc(horz_dir, (horz_flip_flag * sin(travel_ang) * radius), horz_dir);
	um_vctmsc(radial_vec, (cos(travel_ang) * radius), radial_vec);
	um_vcplvc(horz_dir, radial_vec, move);
	um_vcplvc(axial_pt, move, surfpt);
/*
..... Part of the horizontal vector wraps down (away from the apex along the
..... axis).  I have turned the correct angle, now must go down.
..... Want to go down the side of the cone, so build vector down side of cone.
*/
	um_vcmnvc(surfpt, apex, vert_dir);
	um_unitvc(vert_dir, vert_dir);

	travel_ang = fabs(travel_ang);
	while (travel_ang > UM_TWOPI) travel_ang -= UM_TWOPI;
	if (travel_ang > UM_PI)
		travel_ang = UM_TWOPI - travel_ang;
	height = um_dcccc(apex, axial_pt);
/*
..... Calculation for horizontal distance down cone.
..... Draw isoceles triangle.  Height from base to apex = height.
..... Angle at apex = 2*alpha (alpha angle from bisector -> perpendicular
..... to base to side).
..... Now draw a perpendicular line from side at the point where base and
..... side intersect until this line intersects the opposite side.
..... Draw a parallel line to base from this second intersection point back
..... to the first side.  This new parallel line should be further away from
..... apex than the original base.  This is the new triangle (sides, and
..... new base).  It is similar to the original one (same angles).
..... The important characteristic is that the line connecting original base
..... (point on first side) to the new base (point on second side) is
..... PERPENDICULAR to first side.  When you wrap a straight, horizontal
..... line (no component along vertical axis of cone) around a cone, it
..... will not stay horizontal the entire way around.  Looking from the side,
..... it will follow the line you just made.  The intersection point on
..... first side is the original attach point.
.....
..... length of original base = 2 * radius = 2 * height * tan_alpha
..... new_base - old_base = 2 * (x)
.....
..... Make little triangle using second side, part of original base, and
..... line perpendicular to new base, starting at intersection of second
..... side and new base.  Triangle will be outside cone on far side from
..... starting point.  Height of little triangle = max_height_change
..... base = x, and last side is part of cone.  The angle between side of
..... cone and perpendicular is alpha.
.....
..... x = max_height_change * tan_alpha
.....
..... Make another triangle using new base, diagonal line drawn above and
..... line perpendicular to both bases at starting point.  Angle between
..... new base and diagonal line is alpha.  The part of new base that is
..... in this triangle is (2*radius + x).  The other x is not included.
.....
..... max_height_change / (2*radius + x) = tan_alpha
..... max_height_change = (2*radius + x) * tan_alpha
..... max_height_change = (2*height*tan_alpha + max_height_change*tan_alpha) *
.....                       tan_alpha
..... max_height_change = (2*height + max_height_change) * tan_alpha^2
..... max_height_change * (1 - tan_alpha^2) = 2 * height * tan_alpha^2
..... max_height_change = (2 * height * tan_alpha^2) / (1 - tan_alpha^2)
..... max_height_change = (2 * height) / (cot_alpha^2 - 1)
.....
..... Wow!
..... Be aware that this number is the height along the axis; however,
..... I need the height along the cone surface.
..... surf_vec_mag = max_height_change / cos_alpha
..... I go down that far iff travel_angle = Pi/2.
..... Scale by (1 - cos(travel_angle)).
*/
	stuff = (cot_alpha * cot_alpha) - 1.0;
	max_dist_down = (2.0 * height) / stuff;
	surf_vec_mag = max_dist_down / cos_alpha;
	surf_vec_mag = (0.5 * (1.0 - cos(travel_ang))) * surf_vec_mag;

	um_vctmsc(vert_dir, (0.5 * surf_vec_mag), vert_dir);
	um_vcplvc(surfpt, vert_dir, end_pt);
	um_vctovc(end_pt, pts[2]);

	return UU_SUCCESS;
}


/*********************************************************************
**    E_FUNCTION     : ncl_proj_dist_plane (radius, dist, uv_pt1, uv_pt2)
**    PARAMETERS
**       INPUT  :
**              surf_key  =   UNIBASE key for surface pattern projected onto
**              pts[0]    =   start point -- travel dist_vect from this point
**              pts[1]    =   point on surface (plane)
**              vec[0]    =   distance vector to go
**              vec[1]    =   normal to surface (plane)
**              vec[2]    =   projection plane normal
**              uv_pt1    =   starting u,v coordinates for base point
**
**              vec[3], vec[4] ignored
**
**       OUTPUT :
**              pts[2]    =   ending point on surface (or extension)
**              uv_pt2    =   ending u,v coordinates for end_pt.
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  30 Aug 00
*********************************************************************/
int ncl_proj_dist_plane(surf_key, pts, vec, uv_pt1, uv_pt2)
UU_KEY_ID surf_key;
UM_coord pts[3];
UM_vector vec[5];
UM_2Dcoord uv_pt1, uv_pt2;
{
	UU_REAL dist;
	UM_coord start, end_pt, pl_pt;
	UM_vector dist_vec, surf_norm, prj_pl_norm, move;
	UM_real4 side, proj_u, proj_v, ss[9];
	UM_int2 itype, isf;
	UM_real8 packed_desc;

	um_vctovc(pts[0], start);
	um_vctovc(pts[1], pl_pt);
	um_vctovc(vec[0], dist_vec);
	um_unitvc(vec[1], surf_norm);
	um_unitvc(vec[2], prj_pl_norm);

	dist = um_mag (dist_vec);
	um_cross (surf_norm, prj_pl_norm, move);
	if (um_mag(move) < UM_FUZZ) return UU_FAILURE;
	um_unitvc(move, move);
	if (um_dot(dist_vec, move) < 0.0)
		um_vctmsc(move, (-1.0*dist), move);
	else
		um_vctmsc(move, dist, move);
	um_vcplvc(start, move, end_pt);
	um_vctovc(end_pt, pts[2]);
/*
..... Do the Fortran thing.  itype = 9 -> surface.  isf = 3 -> check surface
..... (rather than part surface or drive surface).  side = 0 first time
..... thru the function; however, I only call it once while inside of this
..... function.
*/
	itype = 9; isf =3; side = 0.0;
	ptdesc(&surf_key, &itype, &packed_desc);
	proj_u = uv_pt1[0]; proj_v = uv_pt1[1];
	sfpt(&packed_desc, end_pt, &isf, &side, &proj_u, &proj_v, ss);

	uv_pt2[0] = proj_u; uv_pt2[1] = proj_v;

	return UU_SUCCESS;
}
