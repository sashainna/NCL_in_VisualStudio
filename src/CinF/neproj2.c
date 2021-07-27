/*********************************************************************
**    NAME         :  neproj2.c
**       CONTAINS: functions for projecting NCL paterns onto surfaces
**  These functions project patterns of points or point-vectors onto
**  surfaces.  The user can supply projection vectors, near points, or
**  surface u,v values.  The patterns can be projected as individual
**  points, or they can be wrapped (maintaining distance (arc length)
**  between the point and the base (starting) point).  When wrapping
**
**
**         ncl_geo_project_nowrap
**         ncl_geo_project_wrap
**         ncl_go_along_surf_arclen
**         ncl_load_pattern_arrays
**         ncl_get_center_patt
**         ncl_get_middle_patt
**         ncl_proj_pv_out
**         ncl_ve_proj_sf
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       neproj2.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:42
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
#define DEBUGX 0

static int S_ck_move_pl(),S_close_to_curv(),S_setup_wrap(),S_wrap_travel();
static int S_next_travel_vec(),S_get_prvec(),S_get_sfnorm(),S_check_dis();
static void S_check_first_vec();

/*********************************************************************
**    E_FUNCTION    : ncl_geo_project_nowrap (proj,cplist,cnlist,ulist,npts)
**      Projects an array of points onto a surface using the nowrap method.
**    PARAMETERS
**       INPUT  :
**              proj         =  Projection structure.
**
**       OUTPUT :
**              cplist       =  Projected points.
**              cnlist       =  Projected normal vectors for points or
**                              tangent vectors for curves.
**              ulist        =  UV-parameters of projected points.
**              npts         =  Number of points in output lists.
**
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_geo_project_nowrap(proj,cplist,cnlist,ulist,npts)
NCL_proj_struc *proj;
UU_LIST *cplist,*cnlist,*ulist;
int *npts;
{
	int i,j,status,nuv,default_err,iside,nrptfl;
	UM_int2 primitive,ifl,ival;
	UU_LOGICAL atangfl,lfl,ido;
	UU_REAL dis,u,v,umax,ang,delta,side;
	UM_vector vec,snorm,*vcs,prvec,vc1,dist_vect;
	UM_coord *ptuv, uvp;
	UM_coord spt,nrp;
	UM_transf tfmat,afmat;
	struct NCL_fixed_databag surf,srfa;
	struct UM_evsrfout evsrf,evsrf1;
	UM_int2 err_flag = 0;

/*
.....Initialize routine
*/
	default_err = 163;
	umax = 1.;
	if (proj->ptype == 0) vcs = UU_NULL;
	else if (proj->ptype == 1) vcs = &proj->pvec;
	else vcs = &prvec;
	u = proj->uv[0]; v = proj->uv[1];
	proj->onsrf = UU_TRUE;
/*
.....Get projection surface
*/
	surf.key = proj->sfkey;
	status = ncl_retrieve_data_fixed (&surf);
	status += uc_retrieve_transf (surf.key, tfmat);
	if (status != UU_SUCCESS) return default_err;
	if (surf.rel_num != NCL_PLN_REL) status = um_init_evsrfout (&surf,&evsrf1);
/*
.....Determine if atangl projection
*/
	atangfl = UU_FALSE;
	if (proj->ptype == 2)
	{
		atangfl = UU_TRUE;
		ang = proj->sang;
		delta = proj->eang - proj->sang;
		if (fabs(delta) < UM_FUZZ) delta = 0.;
		srfa.key = proj->sakey;
		status = ncl_retrieve_data_fixed (&srfa);
		status += uc_retrieve_transf (srfa.key, afmat);
		if (status != UU_SUCCESS) return default_err;
		um_unitvc(proj->tvec[0],vec);
		S_get_prvec(&srfa,afmat,u,v,proj->inpt[0],ang,vec,prvec);
	}
/*
.....Project first point
.....to make sure it is not at apex point
.....of revolved surface
.....If it is, then work way back to apex point
*/
	side = 0.;
	status = ncl_pt_project_sf(proj->inpt[0],&surf,tfmat,&u,&v,&side,proj->ptype,
		vcs,proj->nrptfl,proj->nrpt,proj->tol,spt,snorm,&err_flag);
	if (status != UU_SUCCESS && surf.rel_num != NCL_PLN_REL)
	{
		ifl = 2; ival = 0; setifl(&ifl,&ival);
		status = ncl_get_sf_primtyp(&surf.key,&primitive);
/*
........Projection failed
........Use UV setting from 2nd-nth point
........to calculate first point projection
*/
		if (status == UU_SUCCESS && (primitive == NCLSF_SPHERE ||
			primitive == NCLSF_CONE) && surf.rel_num == NCL_REVSURF_REL)
		{
			u = proj->uv[0]; v = proj->uv[1];
			side = 0.;
			for (i=1;i<proj->npts;i++)
			{
				status = ncl_pt_project_sf(proj->inpt[0],&surf,tfmat,&u,&v,&side,
					proj->ptype,vcs,proj->nrptfl,proj->nrpt,proj->tol,spt,snorm,
					&err_flag);
				if (status == UU_SUCCESS)
				{
					for (j=i-1;j>=0;j--)
					{
						status = ncl_pt_proj_sf(proj->inpt[j],&surf,&u,&v,&side,
							proj->ptype,vcs,proj->tol,spt,snorm,&err_flag);
						if (status != UU_SUCCESS) goto done;
					}
					break;
				}
			}
			if (status != UU_SUCCESS) goto done;
		}
	}
	uvp[0] = u; uvp[1] = v;
	uvp[2] = 0;
/*
.....If this routine was called for redef,
.....Ignore the projected points on the surface extension
*/
	ido = UU_FALSE;
	if (surf.rel_num == NCL_PLN_REL)
		ido = UU_TRUE;
	else
	{
		uc_evsrf (UM_POINT, u, v, &surf, tfmat, &evsrf1);
		dis=um_dcccc(spt,evsrf1.sp);
		if(!proj->trimext || (dis<proj->tol) || (u != 0.0 && u != 1.0 &&
			v != 0.0 && v != 1.0)) ido = UU_TRUE;
	}
	if (ido)
	{
		lfl = S_check_dis(proj->inpt[0],spt,snorm,proj->tol);
		if (!lfl) proj->onsrf = UU_FALSE;
		uu_list_push (cplist,spt);
		if (proj->vectype == 1)
		{
			iside = 0;
			um_vcmnvc(proj->inpt[0], spt, dist_vect);
			ncl_proj_pv_out(&iside,dist_vect,snorm,vc1);
			uu_list_push(cnlist,vc1);
		}
		else if (proj->vectype == 2)
		{
			ncl_ve_proj_sf(proj->inpt[0],proj->tvec[0],&surf,tfmat,u,v,&side,
				proj->ptype,vcs,UU_FALSE,proj->tol,spt,vec);
			uu_list_push(cnlist,vec);
		}
		if (ulist != UU_NULL) uu_list_push(ulist,uvp);
	}
/*
.....Project rest of points onto surface
*/
	nrptfl = 0;
	if (proj->nrptfl == 1) nrptfl = 2;
	for (i=1;i<proj->npts;i++)
	{
/*
........Get atangl projection vector
*/
		if (atangfl)
		{
			um_unitvc(proj->tvec[i],vec);
			ang = proj->sang + proj->uprm[i]*delta;
			S_get_prvec(&srfa,afmat,u,v,proj->inpt[i],ang,vec,prvec);
			side = 0.;
		}
/*
........Project point onto surface
*/
		um_vctovc(spt,nrp);
		status = ncl_pt_project_sf(proj->inpt[i],&surf,tfmat,&u,&v,&side,
			proj->ptype,vcs,nrptfl,nrp,proj->tol,spt,snorm,&err_flag);
		uvp[0] = u; uvp[1] = v;
/*
........Special case :
........surface primitive type for surface of revolution is a
........sphere or cone, point is on apex ie. u/v = 0/1
........where u/v derivatives are very small
*/
		if (status == UU_FAILURE)
		{
			ifl = 2; ival = 0; setifl(&ifl,&ival);
			status = ncl_get_sf_primtyp(&surf.key,&primitive);
/*
........Recalculate the point by
........Extending the vector formed by the previous 2 projections
*/
			if (status == UU_SUCCESS && primitive == NCLSF_SPHERE &&
				surf.rel_num == NCL_REVSURF_REL )
			{
				ptuv = (UM_coord *)UU_LIST_ARRAY(cplist);
				nuv= UU_LIST_LENGTH(cplist);
				status = ncl_get_apex(surf.key,uvp,ptuv,nuv);
				uc_evsrf (UM_FRSTDERIV, uvp[0], uvp[1], &surf, tfmat, &evsrf);
				um_vctovc (evsrf.sp,spt); um_vctovc (evsrf.snorm,snorm);
			}
		}
		ido = UU_FALSE;
		if (surf.rel_num == NCL_PLN_REL)
			ido = UU_TRUE;
		else
		{
			uc_evsrf (UM_POINT, u, v, &surf, tfmat, &evsrf1);
			dis=um_dcccc(spt,evsrf1.sp);
			if(!proj->trimext || (dis<proj->tol) || (u != 0.0 && u != 1.0 &&
				v != 0.0 && v != 1.0)) ido = UU_TRUE;
		}
		if (ido)
		{
			lfl = S_check_dis(proj->inpt[i],spt,snorm,proj->tol);
			if (!lfl) proj->onsrf = UU_FALSE;
			uu_list_push (cplist,spt);
			if (proj->vectype == 1)
			{
				ncl_proj_pv_out(&iside,dist_vect,snorm,vc1);
				uu_list_push(cnlist,vc1);
			}
			else if (proj->vectype == 2)
			{
				if (i == 1) S_check_first_vec (proj,cplist,cnlist);
				ncl_ve_proj_sf(proj->inpt[i],proj->tvec[i],&surf,tfmat,u,v,&side,
					proj->ptype,vcs,UU_FALSE,proj->tol,spt,vec);
				uu_list_push(cnlist,vec);
			}
			if (ulist != UU_NULL) uu_list_push(ulist,uvp);
		}
	}
#if DEBUGX == 1
	ncl_dbx_print_proj("$$ Projected points",cplist,cnlist);
#endif
/*
.....End of routine
*/
done:;
	*npts = UU_LIST_LENGTH(cplist);
	if (status != UU_SUCCESS) status = default_err;
	return (status);
}

/*********************************************************************
*********************************************************************/
void ncl_wrap_addvec (cnlist,tvec)
UU_LIST *cnlist;
UM_vector tvec;
{
	UU_REAL dd;
	UM_vector vc1;

	dd = UM_DOT (tvec,tvec);
	if (dd > 9.e9)
		um_vctovc (tvec,vc1);
	else
		um_nullvc (vc1);

	uu_list_push(cnlist,vc1);
}

/*********************************************************************
*********************************************************************/
static UU_LOGICAL S_need_vec (vci)
UM_vector vci;
{
	UU_REAL dd;

	dd = UM_DOT (vci,vci);
	return (dd > 0.5);
}

/*********************************************************************
**    E_FUNCTION    : ncl_geo_project_wrap (proj,cplist,cnlist,ulist,npts)
**      Projects an array of points onto a surface using the wrap method.
**    PARAMETERS
**       INPUT  :
**              proj         =  Projection structure.
**
**       OUTPUT :
**              cplist       =  Projected points.
**              cnlist       =  Projected normal vectors.
**              ulist        =  UV-parameters of projected points.
**              npts         =  Number of points in output lists.
**
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR	   : Ed Ames  24 Oct 00
*********************************************************************/
int ncl_geo_project_wrap(proj,cplist,cnlist,ulist,npts)
NCL_proj_struc *proj;
UU_LIST *cplist,*cnlist,*ulist;
int *npts;
{
	int i, status, closed_fl, use_uv,iside;
	int pt_new_patt, first, second, next, current, last, previous;
	int use_same, linear, flag[7], wrap_param[5], travel_int[7], next_flags[6];
	int at_angle;
	UU_REAL dist,side;
	UM_int2 primitive, default_err;
	UU_REAL u_temp, v_temp;
	UM_vector dist_vect, loc_proj_vect, prj_pl_norm, curv_deriv;
	UM_vector old_dist_vec, travel_vec[7], next_vecs[6],vc1,*vcs;
	UM_coord pt_coord, finish, center, middle, begin, loc_attach_pt;
	UM_coord *pts,uvt,*uvp;
	UM_2Dcoord uv_pt0, uv_pt1, uv_init;
	struct NCL_fixed_databag surf;
	struct UM_evsrfout evsrf;
	UM_transf tfmat;
	UM_vector vnul;
	UM_int2 err_flag = 0;

	default_err = 163;
	pt_new_patt = 0;
	closed_fl = 0;
	*npts = 0;
	um_nullvc (vnul);

	if (proj->ptype == 1) um_vctovc(proj->pvec, loc_proj_vect);

	if (proj->attach == 5)
		um_vctovc(proj->atpt, loc_attach_pt);
	use_uv = UU_TRUE;
	if (ulist == UU_NULL) use_uv = UU_FALSE;
	uv_init[0] = proj->uv[0]; uv_init[1] = proj->uv[1];
/*
..... Ensure that if closed in (u or v), surface structure has
..... closdinu/v flag set correctly.
*/
	surf.key = proj->sfkey;
	status = ncl_retrieve_data_fixed (&surf);
	status += uc_retrieve_transf (surf.key, tfmat);
	if (status != UU_SUCCESS) return default_err;
	if (surf.rel_num == NCL_PLN_REL)
	{
		struct NCL_nclpl_rec *pln;

		pln = (struct NCL_nclpl_rec *) &surf;
		um_vctovc(pln->nvec,evsrf.snorm);
	}
	else
	{
		srfcls (&proj->sfkey);
		uc_init_evsrfout (&surf, &evsrf);
	}
/*
..... Check to see if pattern is closed (i.e. first and last points have
..... same 3D spatial coordinates (each is on top of the other)).
..... If so, set the closed flag.  It is used at end of projection loop.
..... Terminates the projection one point earily -> just use the first
..... projected pattern point.
*/
	if (um_dcccc(proj->inpt[0], proj->inpt[(proj->npts-1)]) < UM_FUZZ)
		closed_fl = 1;
/*
..... Setup looping information based on the attach flag.  The flag
..... will determine which points are first, second, and last.  If
..... wrapping from center, middle, or attach_pt, then dist_vect
..... will be made.  If not wrapping from center, middle, or attach_pt,
..... dist_vect will remain null until after the first point is projected.
*/
	um_nullvc(dist_vect);
	wrap_param[0] = proj->attach;
	wrap_param[1] = proj->npts;

	status = S_setup_wrap (wrap_param, proj->inpt, loc_attach_pt, \
	                          pt_coord, dist_vect);
	if (status != UU_SUCCESS) return default_err;

	first = wrap_param[2];
	second = wrap_param[3];
	last = wrap_param[4];

	if (proj->attach == 2)  /* attach in MIDDLE */
		um_vctovc(pt_coord, middle);
	if (proj->attach == 4)  /* attach in CENTER */
		um_vctovc(pt_coord, center);
	dist = um_mag(dist_vect);
/*
..... Project first point down onto surface.
..... Point should be in pt_coord.  Make sure to use local u's and v's
..... since the u and v will be changed (want to save original if user
..... supplied them).  Save the surface u and v (parametric coordinates for
..... surface point -- or point on extension) in uv_pt0.
*/
	u_temp = uv_init[0]; v_temp = uv_init[1];
	side = 0.;
	status = ncl_pt_project_sf(pt_coord, &surf, tfmat, &u_temp, &v_temp, &side,
		proj->ptype, loc_proj_vect, proj->nrptfl, proj->nrpt, proj->tol,
		begin, vc1, &err_flag);
	if (status != UU_SUCCESS) return default_err;
	um_xytovc_2d (u_temp, v_temp, uv_pt0);
/*
..... If no user-supplied projection vector, make my own.
..... This keeps everything in the plane defined by the projecting pattern
..... point, its projection in the surface, and the next pattern point.
*/
	if (proj->ptype == 0) um_unitvc (vc1, loc_proj_vect);
/*
..... Put point (or pointvector) into output arrays.  If attaching at middle or
..... center, don't use this point; this is not an actual point in the pattern.
..... Use side flag to determine which sign to give surface normal (+- 1).
..... First time through, use 0; it will be changed.
*/
	if ((proj->attach != 2) && (proj->attach != 4) && (proj->attach != 5))
	{
		uu_list_push(cplist,begin);
		if (use_uv)
		{
			um_xyztovc(u_temp,v_temp,0.,uvt);
			uu_list_push(ulist,uvt);
		}
		if (proj->vectype == 1)
		{
			iside = 0;
			S_get_sfnorm(&surf,tfmat,u_temp,v_temp,vc1);
			um_vcmnvc(proj->inpt[first], begin, dist_vect);
			ncl_proj_pv_out(&iside,dist_vect,vc1,vc1);
			uu_list_push(cnlist,vc1);
		}
		else if (proj->vectype == 2)
		{
			ncl_wrap_addvec (cnlist,vnul);
		}

		++pt_new_patt;
/*
..... Get vector from 1st to 2nd points of projecting pattern.
*/
		um_vcmnvc (proj->inpt[second],proj->inpt[first], dist_vect);
		dist = um_mag (dist_vect); /* Needed for arc length */
	}
/*
..... See if surface is a CADD primitive (primitive != 1).
..... Used in FOR loop below.
*/
	if (surf.rel_num == NCL_PLN_REL)
		primitive = NCLSF_PLANE;
	else
		status = ncl_get_sf_primtyp(&proj->sfkey, &primitive);
	if (status != UU_SUCCESS) return default_err;

	if (primitive == NCLSF_PLANE ||
		(primitive == NCLSF_SPHERE && surf.rel_num != NCL_REVSURF_REL))
	{
		if (proj->wrap > 1) proj->wrap = 1;
	}
	flag [0] = proj->attach;
	flag [1] = pt_new_patt;
	flag [2] = side;
	flag [3] = first;
	flag [4] = second;
	flag [5] = last;
	flag [6] = proj->wrap;
/*
..... Don't want different results for wrapping primitives.
..... For instance, take an old unibase that was made before
..... primitives were implimented, wrap pattern (uses general surface
..... technique), analyze surface (discovers that surface is a primitive),
..... and then wrap same pattern again.  The two patterns will be
..... different.  Don't want that.  Therefore, don't use special
..... primitive logic for wrapping ... only revolving and radial.
*/
	if (((proj->wrap == 2) || (proj->wrap == 3)) && \
		((primitive == NCLSF_CONE) || (primitive == NCLSF_CYLINDER) \
		|| (primitive == NCLSF_RULED) || (surf.rel_num == NCL_REVSURF_REL)))
	{
		if (primitive == NCLSF_CYLINDER)
		{
			status = ncl_patt_proj_prim (&surf,proj,cplist,cnlist,ulist,flag,
				pt_coord, begin, uv_pt0);
		}
/*
..... Revolve pattern
*/
		else
		{
			status = ncl_patt_revolve (&surf,proj,cplist,cnlist,ulist,flag,
				pt_coord, begin, uv_pt0);
		}
		if (status != UU_SUCCESS) status = default_err;
		goto done;
	}
	if (proj->wrap == 2)
	{
		return 480;  /* Error code:  MUST BE A SURFACE OF REVOLUTION */
	}
/*
..... Loop through remaining points in the pattern.
..... This assumes that you have a distance vector, dist, begining pt,
..... and uv_pt0 ready to go.
*/
	um_cross(dist_vect, loc_proj_vect, prj_pl_norm);
	um_unitvc(prj_pl_norm, prj_pl_norm);
	um_nullvc(old_dist_vec);
	use_same = UU_FALSE;
	linear = UU_FALSE;
	at_angle = UU_FALSE;
	um_nullvc(curv_deriv);
/*
.....  ------------------->  MAIN LOOP  <--------------------------
*/
	for (i = second; i != last; i += (second - first))
	{
		if ((pt_new_patt == 0) && \
		    ((proj->attach == 2) || (proj->attach == 4) || (proj->attach == 5)))
			i = first;
		current = i;
		next = current + (second - first);
		previous = current - (second - first);

		travel_int[0] = first;
		travel_int[1] = previous;
		travel_int[2] = current;
		travel_int[3] = linear;
		travel_int[4] = use_same;
		travel_int[5] = pt_new_patt;
		travel_int[6] = at_angle;

		um_vctovc(begin,         travel_vec[0]);
		um_vctovc(dist_vect,     travel_vec[1]);
		um_vctovc(old_dist_vec,  travel_vec[2]);
		um_vctovc(loc_proj_vect, travel_vec[3]);
		um_vctovc(prj_pl_norm,   travel_vec[4]);
/*
..... QAR 92381 - save current curv_deriv in case it is not recalculated
..... in the call below; fixed the indexing of travel_vec
*/
		um_vctovc(curv_deriv,    travel_vec[6]);
/*
..... Wrap along the surface a given distance in a given direction.
*/
		pts = (UM_coord *)UU_LIST_ARRAY(cplist);
		status = S_wrap_travel (&surf, proj->inpt, pts, travel_int, \
				travel_vec, dist, uv_pt0, uv_pt1);
		if (status != UU_SUCCESS) return default_err;

		um_vctovc(travel_vec[5], finish);
		um_vctovc(travel_vec[6], curv_deriv);
/*
..... The point finish is an arc length dist (linear distance between
..... the 2 pattern points) away from begin (projection of 1st pattern
..... point).  Give the uv coordinates for begin, and get back the uv
..... coordinates for finish.
*/
		uu_list_push(cplist,finish);
		if (use_uv)
		{
			um_xyztovc(uv_pt1[0],uv_pt1[1],0.,uvt);
			uu_list_push(ulist,uvt);
		}
		if (proj->vectype == 1)
		{
			S_get_sfnorm(&surf,tfmat,uv_pt1[0],uv_pt1[1],vc1);
			ncl_proj_pv_out(&iside,dist_vect,vc1,vc1);
			uu_list_push(cnlist,vc1);
		}
		else if (proj->vectype == 2)
		{
			ncl_wrap_addvec (cnlist,proj->tvec[current]);
		}
		++pt_new_patt;
/*
..... Get the next travelling vector.  Go from the 1st projected
..... point (begin) along the same vector as from the 1st pattern
..... (original, projecting) point to the current pattern point.
..... Only bother to do this if the two pattern points (pt_in[current]
..... and pt_in[next]) are separated by a sizable distance.  If not,
..... set dist = 0.0.  This ensures that the travel_arc function
..... won't execute and the same result will be put in pt_out.
*/
		linear = UU_FALSE;  use_same = UU_FALSE;

		if (next != last)
		{
			next_flags[0] = first;
			next_flags[1] = current;
			next_flags[2] = next;
			next_flags[3] = proj->attach;
			next_flags[4] = use_same;
			next_flags[5] = linear;

			um_vctovc (dist_vect,     next_vecs[0]);
			um_vctovc (old_dist_vec,  next_vecs[1]);
			um_vctovc (middle,        next_vecs[2]);
			um_vctovc (center,        next_vecs[3]);
			um_vctovc (loc_attach_pt, next_vecs[4]);
			um_vctovc (curv_deriv,    next_vecs[5]);
/*
..... Find the next travelling vector and distance.  If the two corresponding
..... points in the original pattern are very close together, use_same =
..... UU_TRUE. If the last point and this point are colinear, linear = UU_TRUE.
..... Also, update the curve derivative (make sure it points the right way).
*/
			status = S_next_travel_vec (proj->inpt,next_vecs, &dist, next_flags);
			if(status != UU_SUCCESS) return default_err;

			use_same    = next_flags[4];
			linear      = next_flags[5];
			um_vctovc (next_vecs[0], dist_vect);
			um_vctovc (next_vecs[1], old_dist_vec);
			um_vctovc (next_vecs[5], curv_deriv);
		}
/*
..... If pattern is closed (i.e. the first point and the last point
..... have the same 3D coordinates (are on top of each other)), ensure
..... that the projected pattern is also closed.  Don't bother with
..... projecting the last point; just use the first point in the
..... new, projected pattern.  Be sure to break out of loop.
*/
		if ((closed_fl == 1) && (i == (last - 2*(second-first))))
		{
			pts = (UM_coord *)UU_LIST_ARRAY(cplist);
			uu_list_push(cplist,pts[0]);
			if (use_uv)
			{
				uvp = (UM_coord *)UU_LIST_ARRAY(ulist);
				uu_list_push(ulist,uvp[0]);
			}
			if (proj->vectype == 1 || proj->vectype == 2)
			{
				vcs = (UM_vector *)UU_LIST_ARRAY(cnlist);
				uu_list_push(cnlist,vcs[0]);
			}
			++pt_new_patt;

			break;
		}
	}  /* End of projection loop. */
	status = UU_SUCCESS;
/*
.....Create tangent vectors
*/
done:;
	*npts = cplist->cur_cnt;
	if (proj->vectype == 2 && status == UU_SUCCESS)
	{
		vcs = (UM_vector *)UU_LIST_ARRAY(cnlist);
		pts = (UM_coord *)UU_LIST_ARRAY(cplist);
		for (i = 0; i < *npts - 1; i++)
		{
			if (S_need_vec(vcs[i]))
				um_vcmnvc(pts[i+1],pts[i],vcs[i]);
		}

		if (closed_fl == 1)
		{
			if (S_need_vec (vcs[*npts - 1]))
				um_vctovc(vcs[0],vcs[*npts - 1]);
		}
	}
#if DEBUGX == 1
	ncl_dbx_print_proj("$$ Projected points",cplist,cnlist);
#endif
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : S_get_step (unit_dir, du, dv, d2u, d2v,tol, step)
**      Get step size (for adaptive step size routine) based on curvature
**      in the direction of travel along surface.
**    PARAMETERS
**       INPUT  :
**              unit_dir = unit vector in direction of curvature
**              du       = 1st derivative for u (const v) on surf
**              dv       = 1st derivative for v (const u) on surf
**              d2u      = 2nd derivative for u (const v) on surf
**              d2v      = 2nd derivative for v (const u) on surf
**              tol      = tolerance
**       OUTPUT :
**              step     = step size
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR	   : Ed Ames  18 Oct 00
*********************************************************************/
static int S_get_step(unit_dir, du, dv, d2u, d2v,tol,step)
UM_vector unit_dir, du, dv, d2u, d2v;
UU_REAL tol,*step;
{
	int status;
	UU_REAL a, b, curvat, rad_curv, step1;
	UM_vector dsdt, d2sdt2, unit_du, unit_dv;
	UM_vector unit_d2u, unit_d2v;

	um_unitvc(du, unit_du);
	um_unitvc(dv, unit_dv);
	um_unitvc(d2u, unit_d2u);
	um_unitvc(d2v, unit_d2v);
/*
..... Get 1st derivative of curve.
*/
	a = um_dot(unit_dir, unit_du);
	b = um_dot(unit_dir, unit_dv);
	um_avcplbvc(a, unit_du, b, unit_dv, dsdt);
/*
..... Get 2nd derivative of curve.
*/
	a = um_dot(unit_dir, unit_d2u);
	b = um_dot(unit_dir, unit_d2v);
	um_avcplbvc(a, unit_d2u, b, unit_d2v, d2sdt2);

	status = um_get_curvature(dsdt, d2sdt2, &curvat);
	if (status == UU_FAILURE)
		curvat = UM_FUZZ;
/*
..... Really, want the radius of curvature.
*/
	if (curvat > UM_FUZZ)
		rad_curv = 1.0 / curvat;
	else
		rad_curv = 1.e16;

	if (tol < (rad_curv - UM_FUZZ))
		*step = 2.0 * sqrt (tol * (2.0 * rad_curv - tol));
	else
		*step = 2.0 * rad_curv;

	if (*step < (0.5 * tol))
		*step = 0.5 * tol;

	a = fabs(um_dot(unit_dir, du));
	b = fabs(um_dot(unit_dir, dv));
	step1 = 0.5 * (a + b);

	if ((*step > step1) && (step1 > 10.0*tol))
		*step = step1;

	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : ncl_go_along_surf_arclen(surf, arcpts, uv, arc_len)
**  Start at surfpt and travel a distance of arc_len across the surface.
**  Make sure to stay within chordal tolerance of the surface.  Also, stay
**  within the projection plane defined by the point surfpt and the
**  plane normal prj_pl_norm.  Travel along the curve of intersection
**  between the surface and the projection plane.
**  This uses dynamic step sizing based on curvature of the surface.
**  It uses the same logic as uvmove.
**
**    PARAMETERS
**       INPUT  :
**              surf      =  pointer to surface
**              arcpts[0] =  dir -> vector tangent to surface to
**                             travel along
**              arcpts[1] =  prj_pl_norm -> projection plane normal
**              arcpts[2] =  surfpt -> starting point on surface
**                             (or extention plane)
**              uv        =  2D parametric coordinates to start curve
**              arc_len   =  the distance to travel along the surface
**       OUTPUT :
**              uv        =  ending u,v coordinates after going arc_len
**              arcpts[3] =  endpt -> ending point on surface (or
**                             extention plane)
**              arcpts[4] =  curv_deriv -> 3D, spatial vector tangent to
**                              plane at end of travel and also in
**                              projecting plane
**    RETURNS      : UU_SUCCESS  or  UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR	   : Ed Ames  9 Oct 00
*********************************************************************/
int ncl_go_along_surf_arclen(surf, arcpts, uv, arc_len)
struct NCL_fixed_databag *surf;
UM_2Dcoord uv;
UM_coord arcpts[5];
UU_REAL arc_len;
{
	int no_vec;
	int i, status, no_inf_lup;
	UU_REAL tol, local_arc, step, dist, last_local_arc,side,u,v;
	UM_coord surfpt, pt, proj_pt, temp_pt, endpt;
	UM_2Dcoord uv_pt, old_uv_pt;
	UM_int2 err_flag = 0;
	UM_vector unit_dir, snorm, deriv_vec1, temp_dir, du, dv;
	UM_vector prj_pl_norm, deriv_vec2, proj_vect, surf_dir;
	UM_vector d2u, d2v, ck_vecs[5];
	UM_transf tfmat;
	struct UM_evsrfout evsrf;
	struct NCL_nclpl_rec *pln = UU_NULL;


	gettol(&tol);
	no_vec = 0; um_nullvc(proj_vect); /* Don't use projection vect */
	if (arc_len < 0.0)
		return UU_FAILURE;
/*
..... Protect the input variables:  use local ones.
*/
	um_unitvc   (arcpts[0], unit_dir);
	um_vctovc   (arcpts[1], prj_pl_norm);
	um_vctovc   (arcpts[2], pt);
	um_vctovc   (pt,        temp_pt);
	um_vctovc_2d(uv,        uv_pt);

	if (surf->rel_num == NCL_PLN_REL)
	{
		pln = (struct NCL_nclpl_rec *) surf;
		um_vctovc(pln->nvec,evsrf.snorm);
		um_unitvc (evsrf.snorm,snorm);
		um_perpvc(snorm,evsrf.dsdu);
		um_cross (snorm,evsrf.dsdu,evsrf.dsdv);
		um_nullvc(evsrf.d2sdu2); um_nullvc(evsrf.d2sdv2);
	}
	else
	{
		uc_init_evsrfout(surf, &evsrf);
		status = uc_retrieve_transf(surf->key,tfmat);
		if (status != UU_SUCCESS) return UU_FAILURE;
		status = uc_evsrf(UM_SECDERIV, uv_pt[0], uv_pt[1], surf, tfmat, &evsrf);
		if (status != UM_VALID) return UU_FAILURE;
		um_unitvc(evsrf.snorm, snorm);
	}
/*
..... Need derivatives for checking chordal tolerance.  Make
..... sure that it goes the correct direcion.
*/
	um_cross(snorm, prj_pl_norm, deriv_vec1);
	um_unitvc(deriv_vec1, deriv_vec1);
	if (um_dot(deriv_vec1, unit_dir) < 0.0)
		um_vctmsc(deriv_vec1, -1.0, deriv_vec1);
/*
..... Need 1st and 2nd derivatives for getting step size.  It
..... is based on curvature.
*/
	um_vctovc(evsrf.dsdu, du);
	um_vctovc(evsrf.dsdv, dv);
	um_vctovc(evsrf.d2sdu2, d2u);
	um_vctovc(evsrf.d2sdv2, d2v);

	status = S_get_step(unit_dir, du, dv, d2u, d2v, tol, &step);
	if (status != UU_SUCCESS) return (status);
/*
..... Start the loop.  While the distance between two surface points along
..... the given direction can be approximated by a straight line, get the linear
..... separation between the points and add it to the local_arc.
..... After that, move the base point to the last point along the vector and
..... do it again.
..... Be sure that the point after stepping is in the intersection of
..... the surface and the projection plane.  This is based on logic
..... in uvmove.
..... The idea is to get the final u,v coordinates and the point on the
..... surface a given arc_len away from the starting point.
..... This uses an adaptive step size.
*/
	local_arc = 0.0;
	last_local_arc = 0.0;
	no_inf_lup = 0;
	while (local_arc < arc_len)
	{
		if (no_inf_lup > 5000)
			return UU_FAILURE;
		else
			++no_inf_lup;

		um_vctmsc (deriv_vec1, step, surf_dir);
		um_vcplvc(temp_pt, surf_dir, proj_pt);
		um_vctovc_2d(uv_pt, old_uv_pt);

		u = uv_pt[0]; v = uv_pt[1];
		side = 0.;
		for (i = 0; i < 21; i++)
		{
			if (i == 20) return UU_FAILURE;
			status = ncl_pt_proj_sf(proj_pt,surf,&u,&v,&side,no_vec,proj_vect,tol,
				surfpt,snorm,&err_flag);
			if (status != UU_SUCCESS) return (status);
/*
..... Check to see if still on intersection of surface tangent
..... plane and the projection plane.  This uses same logic as
..... uvmove.
..... If surfpt too far away from projection plane, the point
..... nearest to surfpt on line of intersection between projection
..... plane and surface tangent plane is used
*/
			um_vctovc(proj_pt, ck_vecs[0]);
			um_unitvc(snorm, ck_vecs[1]);
			um_vctovc(prj_pl_norm, ck_vecs[2]);
			um_vctovc(pt, ck_vecs[3]);
			um_vctovc(surfpt, ck_vecs[4]);

			status = S_ck_move_pl(ck_vecs, tol);

			um_vctovc (ck_vecs[0], proj_pt);
			if (status == UU_SUCCESS)
				break;
		}
/*
..... Get the derivative of the curve.
*/
		if (surf->rel_num != NCL_PLN_REL)
		{
			status = uc_evsrf(UM_SECDERIV, u, v, surf, tfmat, &evsrf);
			if (status != UM_VALID) return UU_FAILURE;
			um_unitvc(evsrf.snorm, snorm);
		}
		um_cross(snorm, prj_pl_norm, deriv_vec2);
		um_unitvc(deriv_vec2, deriv_vec2);
		if (um_dot(deriv_vec2, deriv_vec1) < 0.0)
			um_vctmsc(deriv_vec2, -1.0, deriv_vec2);
/*
		if (um_dot(deriv_vec2, unit_dir) < 0.0;
..... Check to see if new position greater than chordal tolerance away
..... from curve.
*/
		status =
			S_close_to_curv(temp_pt, deriv_vec1, surfpt, deriv_vec2, (0.5*tol));
/*
..... Dynamic step size part.  If a straight line segment is close
..... enough to curve (maximum distance is less than chordal tolerance),
..... then take the linear separation between the two points and add
..... it to the local arc length.  Then update the surface point and
..... u,v coordinates.  If the line segment is not close enough to
..... the curve, shrink the step size by 1/2 and do the loop again.
*/
		if (status == UU_FALSE)
			step *= 0.5;
		else
		{
			um_vcmnvc(surfpt, temp_pt, temp_dir);
			dist = um_mag(temp_dir);
			local_arc += dist;
			uv_pt[0] = u; uv_pt[1] = v;

			if (local_arc < arc_len)
			{
				last_local_arc = local_arc;
/*
..... Get a new step size base on curvature.
*/
				um_vctovc(evsrf.dsdu, du);
				um_vctovc(evsrf.dsdv, dv);
				um_vctovc(evsrf.d2sdu2, d2u);
				um_vctovc(evsrf.d2sdv2, d2v);
				status = S_get_step (deriv_vec1,du,dv,d2u,d2v,tol,&step);
				if (status != UU_SUCCESS) return (status);

				um_vctovc(surfpt, temp_pt);
				um_vctovc(deriv_vec2, deriv_vec1);
			}
		}
	}

/*
..... Went too far; must go back.  Find out how much over it went:
..... local_arc - arc_len, and get the fraction of the step size:
..... (local_arc + step_(n-1)) < arc_len    , but
..... (local_arc + step_n )    > arc_len.
..... Trim off the excess.
*/
	if (dist > (0.1 * tol))
		um_vctmsc(temp_dir, ((arc_len - last_local_arc)/ dist), temp_dir);

	um_vcplvc(temp_pt, temp_dir, endpt);
	local_arc = last_local_arc + ((arc_len - last_local_arc)/ dist);
	um_vctovc(endpt, arcpts[3]);
/*
.... Output endpt.
*/
	status = ncl_pt_proj_sf(endpt,surf,&u,&v,&side,no_vec,proj_vect,tol,surfpt,
		snorm,&err_flag);
	if (status != UU_SUCCESS) return (status);
/*
..... Get the derivative of the curve.
*/
	um_cross(snorm, prj_pl_norm, deriv_vec2);
	um_unitvc(deriv_vec2, deriv_vec2);
	if (um_dot(deriv_vec2, deriv_vec1) < 0.0)
		um_vctmsc(deriv_vec2, -1.0, deriv_vec2);

	um_xytovc_2d(u, v, uv);
	um_vctovc(deriv_vec2, arcpts[4]);
/*
..... Output uv and curv_deriv.
*/
	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_ck_move_pl (ck_vecs, tol)
**      Checks to see if the projected surface point (surfpt) is less
**      than a distance (tol) away from the projection plane.
**      If too far away, give back the nearest point to surfpt on the
**      line of intersection between the projection plane and surface
**      tangent plane.
**      Projecting plane is given by: prj_pl_norm and pt (a point on it)
**      Surface tangent plane: snorm and surfpt.
**      Uses similar logic to uvmove.f
**    PARAMETERS
**       INPUT  :
**              ck_vecs[0] = proj_pt -> point near intersection of the
**                             planes
**              ck_vecs[1] = snorm -> unit normal vector to surface
**              ck_vecs[2] = prj_pl_norm -> unit normal vector to
**                             projection plane
**              ck_vecs[3] = pt -> point at start of move, known to be
**                            in projection plane and on surface.
**              ck_vecs[4] = surfpt -> surface point projected from
**                             proj_pt assumed that proj_pt was projected
**                             along the surface normal
**              tol       = how close surfpt must be to projection plane
**       OUTPUT :
**              ck_vecs[0] = proj_pt -> either original, input pt (if
**		              within tol) or nearest point to surfpt on
**                            line of intersection between projection
**                            plane and surface tangent plane
**    RETURNS      : UU_SUCCESS or 2
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR	   : Ed Ames  18 Oct 00
*********************************************************************/
static int S_ck_move_pl(ck_vecs, tol)
UM_vector ck_vecs[5];
UU_REAL tol;
{
	UU_REAL sin_alpha, P, Q, A, B, C;
	UM_vector snorm, prj_pl_norm, dist_vec;
	UM_coord proj_pt, pt, surfpt;

	um_vctovc(ck_vecs[0], proj_pt);
	um_vctovc(ck_vecs[1], snorm);
	um_vctovc(ck_vecs[2], prj_pl_norm);
	um_vctovc(ck_vecs[3], pt);
	um_vctovc(ck_vecs[4], surfpt);

	sin_alpha = um_dot(snorm, prj_pl_norm);
	P = um_dot(snorm, proj_pt) - um_dot(snorm, surfpt);
/*
..... P = distance of proj_pt to surface tangent plane
*/
	Q = um_dot(prj_pl_norm, proj_pt) - um_dot(prj_pl_norm, pt);
/*
..... Q = distance of proj_pt to projection plane.
*/
	A = (P - (Q * sin_alpha)) / (1.0 - (sin_alpha * sin_alpha));
	B =  Q - (A * sin_alpha);
	C = B * sqrt(1.0 - (sin_alpha * sin_alpha));
/*
..... A = distance along normal to surface tangent plane (snorm)
..... B = distance along normal to projection plane (prj_pl_norm)
..... for proj_pt to get to the nearest point on the line of
..... intersection between the projection plane and the surface
..... tangent plane.
..... C = distance of surfpt to that intersection point.
*/
	if (fabs(C) > tol)
	{
		um_avcplbvc(A, snorm, B, prj_pl_norm, dist_vec);
		um_vcmnvc(proj_pt, dist_vec, ck_vecs[0]);
		return 2;
	}
	else
		return UU_SUCCESS;
}

/*********************************************************************
**    I_FUNCTION     : S_close_to_curv (pt1, ds1, pt2, ds2, tol)
**      Make a straight line approximation to curve between points
**      pt1 and pt2.  Check to see if maximum distance away from
**      true curve is less than tolerance.
**      Basically, you are approximating the curve as a 2nd order
**      polynomial (conic -> circle) and you want to find the max
**      distance to the (approximation of the) curve from the chord
**      (straight line between the end points).
**      This is legitimate because, if you are close enough, any curve
**      looks like a circular arc, and if you get even closer, any curve
**      looks like a straight line.  Think of Taylor series approximation.
**    PARAMETERS
**       INPUT  :
**              pt1  =  starting point of curve
**              ds1  =  1st (3D) derivative of curve at staring pt
**              pt2  =  ending point of curve
**              ds2  =  1st (3D) derivative of curve at ending pt
**              tol  =  chordal tolerance (how far the straight line
**                        approximation can get from true curve)
**       OUTPUT :
**              none
**    RETURNS      : UU_TRUE  or  UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR	   : Ed Ames  9 Oct 00
*********************************************************************/
static int S_close_to_curv(pt1, ds1, pt2, ds2, tol)
UM_coord pt1, pt2;
UM_vector ds1, ds2;
UU_REAL tol;
{
	UU_REAL dist, angle1, angle2, h;
	UM_vector vec;

	um_vcmnvc(pt2, pt1, vec);
	dist = um_mag(vec);
	angle1 = um_angle(ds1, vec);
	angle2 = um_angle(ds2, vec);

	if (angle1 + UM_FUZZ < 0.5*UM_PI && angle2+UM_FUZZ < 0.5*UM_PI)
	{
		if (angle1 < UM_FUZZ && angle2 < UM_FUZZ)
			h = 0.0;
		else
			h = (dist / 2.0) * tan((angle1+angle2)/4.0);
/*
..... Use average of angles:  (angle1+angle2) / 2.0, as the angle
..... in the chord distance for a circle where dist is the length
..... of the chord.
*/
	}
	else
		h = dist;

	if (h < tol)
		return UU_TRUE;
	else
		return UU_FALSE;
}


/*********************************************************************
**    E_FUNCTION     : ncl_load_pattern_arrays (patt_key, patt_type, \
**                                  pt_array, vec_array, num_patt_pts)
**       Fill arrays with pattern data.
**    PARAMETERS
**       INPUT  :
**              patt_key      =  pointer to pattern of points
**                                 or point vectors
**              patt_type     =  1 if pattern of points
**                               2 if pattern of point vectors
**              pt_array      =  pointer to empty array of point coordinates
**              vec_array     =  pointer to empty array of vectors
**              num_patt_pts  =  number of points or pointvectors in pattern
**       OUTPUT :
**              pt_array      =  array of point coordinates for the pattern
**              vec_array     =  array of vectors if pattern is
**                                 of point vectors
**    RETURNS      : UU_SUCCESS  or  UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR	   : Ed Ames  15 Aug 00
*********************************************************************/
int
ncl_load_pattern_arrays(patt_key, patt_type, pt_array, vec_array, num_patt_pts)
UM_int4 *patt_key;
int patt_type;
UM_coord *pt_array;
UM_vector *vec_array;
int num_patt_pts;
{
	UM_int2 i;
	int j, status = UU_FAILURE;
	UU_REAL ptrec[6];

	for (i = 1; i <= num_patt_pts; i++)
	{
		status = gtpnpt1 (ptrec, &patt_type, patt_key, &i);
		if (status != UU_SUCCESS) return (status);
		for (j = 0; j < 3; j++)
		{
/*
..... Remember that the pattern points go from 1 to n.  C arrays go
..... from 0 to n-1.
*/
			pt_array[i-1][j] = ptrec[j];
			if (patt_type == 2 && vec_array != UU_NULL)
			{
				vec_array[i-1][j]=ptrec[j+3];
			}
		}
	}

	return UU_SUCCESS;
}



/*********************************************************************
**    E_FUNCTION     : ncl_get_center_patt (patt_pts, num_pts, center)
**      Find the center of a pattern (set) of points.  The center is a
**      3D coordinate in the middle of the bounding box.  Find the
**      maximum and minimum values for x,y, and z. Add each components'
**      max and min and divide by 2.0.  This gives the center coordinates.
**      In other words, find the median of pattern component by component.
**      (Rather than mean (gives center of mass) or mode.)
**    PARAMETERS
**       INPUT  :
**              patt_pts  =  array of the 3d coordinates of the pattern
**              num_pts   =  number of points in the pattern
**       OUTPUT :
**              center    =  coordinate of center of pattern.
**                             (different from middle)
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  15 Sep 00
*********************************************************************/
int ncl_get_center_patt (patt_pts, num_pts, center)
UM_coord *patt_pts, center;
int num_pts;
{
	int i, j;
	UU_REAL min[3], max[3];

	for (i = 0; i < 3; i++)
		min[i] = max[i] = patt_pts[0][i];

	for (i = 0; i < num_pts; i++)
	{
		for (j = 0; j < 3; j++)
		{
			if (min[j] > patt_pts[i][j]) min[j] = patt_pts[i][j];
			if (max[j] < patt_pts[i][j]) max[j] = patt_pts[i][j];
		}
	}

	for (i = 0; i < 3; i++)
		center[i] = (min[i] + max[i]) / 2.0;

	return UU_SUCCESS;
}


/*********************************************************************
**    E_FUNCTION     : ncl_get_middle_patt (patt_pts, num_pts, middle)
**      Find the middle of a pattern (set) of points.  The middle is a
**      3D coordinate halfway along the (poly)line connecting the points
**      of the pattern.  It will be on a line segment joining two of the
**      pattern points.
**    PARAMETERS
**       INPUT  :
**              patt_pts  =  array of the 3d coordinates of the pattern
**              num_pts	  =  number of points in the pattern
**       OUTPUT :
**              middle    =  coordinate of middle of pattern.
**                            (different from center)
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  15 Sep 00
*********************************************************************/
int ncl_get_middle_patt (pt_array, num_patt_pts, middle)
UM_coord *pt_array, middle;
int num_patt_pts;
{
	int i;
	UU_REAL temp, diff, arc, tot_arc_len;
	UM_vector dist_vect;

	tot_arc_len = 0.0;
	for (i = 1; i < num_patt_pts; i++)
	{
		um_vcmnvc (pt_array[i], pt_array[i-1], dist_vect);
		tot_arc_len += um_mag (dist_vect);
	}

	arc = 0.0;
	for (i = 1; i < num_patt_pts; i++)
	{
		um_vcmnvc (pt_array[i], pt_array[i-1], dist_vect);
		temp = um_mag (dist_vect);
		if ((temp+arc) >= (tot_arc_len/2.0))
		{
			diff = (tot_arc_len/2.0) - arc;
			um_unitvc (dist_vect, dist_vect);
			um_vctmsc (dist_vect, diff, dist_vect);
			um_vcplvc (pt_array[i-1], dist_vect, middle);
			return UU_SUCCESS;
		}
		arc += temp;
	}

	return UU_FAILURE;
}

/*********************************************************************
**    I_FUNCTION     : S_setup_wrap (wrap_param, pt_in, attach_pt, \
**                                       pt_coord, dist_vect)
**    PARAMETERS
**       INPUT  :
**              wrap_param[0] = attach_flag -> 1 = START
**                                             2 = MIDDLE
**                                             3 = END
**                                             4 = MIDDLE
**                                             5 = AT (attach_pt)
**              wrap_param[1] = numpts -> number of point in pattern
**              pt_in[]       = array of input points
**              attach_pt     = coordinates for the attach point if used
**
**       OUTPUT :
**              wrap_param[3] = first -> # of first pattern point to project
**              wrap_param[4] = second -> # of second point to project
**              wrap_param[5] = last -> # of last point to project
**              pt_coord      = coordinates of first point to project
**              dist_vect     = distance vector to go from pt_coord to next
**                                only used for attaching at MIDDLE, CENTER
**                                and AT (attach_pt)
**
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  17 Nov 00
*********************************************************************/
static int S_setup_wrap(wrap_param, pt_in, attach_pt, pt_coord, dist_vect)
int wrap_param[5];
UM_coord pt_in[], attach_pt, pt_coord;
UM_vector dist_vect;
{
	int status, attach_flag, numpts, first, second, last;
	UM_coord middle, center;

	attach_flag = wrap_param[0];
	numpts      = wrap_param[1];

	switch(attach_flag)
	{
		case 1: /* attach at START */
			first = 0;
			second = first + 1;
			last = numpts;
			um_vctovc(pt_in[first], pt_coord);

			break;

		case 2: /* attach in MIDDLE */
			first = 0;
			second = first + 1;
			last = numpts;
			status = ncl_get_middle_patt(pt_in, numpts, middle);
			if (status != UU_SUCCESS) return (status);

			um_vcmnvc(pt_in[first], middle, dist_vect);

			um_vctovc(middle, pt_coord);

			break;

		case 3: /* attach at END */
			first = numpts - 1;
			second = first - 1;
			last = - 1;

			um_vctovc(pt_in[first], pt_coord);

			break;

		case 4: /* attach in CENTER */
			first = 0;
			second = first + 1;
			last = numpts;
			status = ncl_get_center_patt (pt_in, numpts, center);
			if (status != UU_SUCCESS) return (status);

			um_vcmnvc(pt_in[first], center, dist_vect);

			um_vctovc(center, pt_coord);

			break;

		case 5: /* attach at given ATTACHPT */
			first = 0;
			second = first + 1;
			last = numpts;
			um_vcmnvc(pt_in[first], attach_pt, dist_vect);

			um_vctovc(attach_pt, pt_coord);

			break;

		default: /* attach at START */
			first = 0;
			second = first + 1;
			last = numpts;

			um_vctovc(pt_in[first], pt_coord);

			break;
	}

	wrap_param[2] = first;
	wrap_param[3] = second;
	wrap_param[4] = last;

	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : ncl_proj_pv_out (side, dist_vect, norm, pv_vec)
**    PARAMETERS
**       INPUT  :
**              side      = +- 1.0 or 0.0
**              dist_vect = vector pointing from surface point to original point
**              norm      = surface normal at the surface point
**       OUTPUT :
**              side      = +- 1.0 times the surface normal
**              pv_vec    = the output vectors for the output point-vectors
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  17 Nov 00
*********************************************************************/
int ncl_proj_pv_out(side, dist_vect, norm, pv_vec)
int *side;
UM_vector dist_vect, norm, pv_vec[];
{
	if (*side == 0)
	{
		if(um_dot(dist_vect, norm) < 0.0)
			*side = -1;
		else
			*side = 1;
	}
	um_unitvc(norm, pv_vec);
	if (*side == -1)
		um_vctmsc(pv_vec, -1.0, pv_vec);

	return UU_SUCCESS;
}


/*********************************************************************
**    I_FUNCTION     : S_wrap_travel ()
**    PARAMETERS
**       INPUT  :
**              surf          = surface to project pattern onto
**              pt_in         = set of points in original pattern
**              pt_out        = set of points on surface (projected)
**
**              travel_int[0] = first
**              travel_int[1] = previous
**              travel_int[2] = current
**              travel_int[3] = linear
**              travel_int[4] = use_same
**              travel_int[5] = pt_new_patt
**              travel_int[6] = at_angle -> 0  ordinary
**                                       -> 1  Use ATANGL
**                                             Dynamic projection vectors
**
**              travel_vec[0] = begin
**              travel_vec[1] = dist_vect
**              travel_vec[2] = old_dist_vec
**              travel_vec[3] = loc_proj_vect
**              travel_vec[4] = prj_pl_norm
**
**              dist          = distance to travel
**              uv_pt0        = starting u,v coordinates on surface
**       OUTPUT :
**              travel_vec[5] = finish
**              travel_vec[6] = curv_deriv
**              uv_pt1        = u,v coordinates for projected point
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  20 Nov 00
*********************************************************************/
static int S_wrap_travel(surf,pt_in,pt_out,travel_int,travel_vec,dist,
uv_pt0,uv_pt1)
struct NCL_fixed_databag *surf;
UM_coord pt_in[],pt_out[];
int travel_int[7];
UM_vector travel_vec[8];
UM_2Dcoord uv_pt0, uv_pt1;
UU_REAL dist;
{
	int status, first, previous, current, linear, use_same, pt_new_patt;
	int at_angle;
	UU_REAL u, v,side;
	UM_vector dist_vect, old_dist_vec, loc_proj_vect;
	UM_vector prj_pl_norm, start, curv_deriv, temp_vec;
	UM_coord begin, finish, arcpts[5];
	UM_int2 err_flag = 0;

	first       = travel_int[0];
	previous    = travel_int[1];
	current     = travel_int[2];
	linear      = travel_int[3];
	use_same    = travel_int[4];
	pt_new_patt = travel_int[5];
	at_angle    = travel_int[6];

	um_vctovc(travel_vec[0], begin);
	um_vctovc(travel_vec[1], dist_vect);
	um_vctovc(travel_vec[2], old_dist_vec);
	um_vctovc(travel_vec[3], loc_proj_vect);
	um_vctovc(travel_vec[4], prj_pl_norm);
/*
..... QAR 92381 - initialized curv_deriv;
..... fixed the indexing of travel_vec
*/
	um_vctovc(travel_vec[6], curv_deriv);

	if (use_same == UU_FALSE)
	{
		if (dist > UM_FUZZ)
		{
			um_cross(dist_vect, loc_proj_vect, prj_pl_norm);
			um_unitvc(prj_pl_norm, prj_pl_norm);
			if ((linear == UU_FALSE) || (current == first) || \
					(um_mag(prj_pl_norm) < UM_FUZZ))
			{
				um_vctovc_2d(uv_pt0, uv_pt1);
				um_vctovc(begin, start);
				if (linear == UU_TRUE)
				{
					um_vctovc(old_dist_vec, dist_vect);
					linear = UU_FALSE;
				}
			}
			else
			{
/*
..... Start from the last projected point instead of the beginning.
..... uv_pt1 should still have the info from last projected point.
..... Find the distance (between the two consecutive pattern points)
..... and scale the dist_vect to the new distance.
..... Be sure to use the new surface point rather than beginning,
..... and also keep the same prj_pl_norm.
*/
				dist = um_dcccc(pt_in[previous], pt_in[current]);
				um_unitvc(dist_vect, dist_vect);
				um_vctmsc(dist_vect, dist, dist_vect);
				if (pt_new_patt > 0)
					um_vctovc(pt_out[(pt_new_patt - 1)], start);
				else
					um_vctovc(begin, start);
			}
/*
..... Mainly here for ATANGL projections.  When doing ATANGL, you
..... are constantly modifing the projection vector.  Need to get
..... the surface point for this projection vector.
*/
			if(at_angle == UU_TRUE)
			{
				u = uv_pt0[0];  v = uv_pt0[1];
				side = 0.;
				status = ncl_pt_proj_sf(pt_in[current],surf,&u,&v,&side,1,
					loc_proj_vect,0.,start,temp_vec,&err_flag);
				if (status != UU_SUCCESS) return (status);

				um_xytovc_2d(u, v, uv_pt0);
			}

			um_vctovc(dist_vect,   arcpts[0]);
			um_vctovc(prj_pl_norm, arcpts[1]);
			um_vctovc(start,       arcpts[2]);

			status =  ncl_go_along_surf_arclen(surf, arcpts, uv_pt1, dist);
			if (status != UU_SUCCESS) return (status);

			um_vctovc(arcpts[3], finish);
			um_vctovc(arcpts[4], curv_deriv);
		}
		else
		{
/*
..... Distance is too small to bother travelling.  Just use the data from base
..... point.  This is done for speed optimization.
*/
			um_vctovc(begin, finish);
			um_vctovc_2d(uv_pt0, uv_pt1);
		}
	}
	else
	{
/*
..... Use the same finishing points as last time.
*/
		um_vctovc(travel_vec[6], finish);
		um_vctovc(travel_vec[7], curv_deriv);
	}

	um_vctovc(finish, travel_vec[5]);
	um_vctovc(curv_deriv, travel_vec[6]);

	return UU_SUCCESS;
}

/*********************************************************************
**    I_FUNCTION     : S_next_travel_vec (pt_in, next_vecs, dist, next_flags)
**    PARAMETERS
**       INPUT  :
**              pt_in         = array of points in original pattern
**
**              next_flags[0] = first
**              next_flags[1] = current
**              next_flags[2] = next
**              next_flags[3] = attach_flag
**              next_flags[4] = use_same
**              next_flags[5] = linear
**
**              next_vecs[0] = dist_vect
**              next_vecs[1] = old_dist_vec
**              next_vecs[2] = middle
**              next_vecs[3] = center
**              next_vecs[4] = loc_attach_pt
**              next_vecs[5] = curv_deriv
**
**              dist          = old distance to travel
**       OUTPUT :
**              next_flags[4] = use_same
**              next_flags[5] = linear
**
**              next_vecs[0] = dist_vect
**              next_vecs[1] = old_dist_vec
**              next_vecs[5] = curv_deriv
**
**              dist          = new distance to travel
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR      : Ed Ames  20 Nov 00
*********************************************************************/
static int S_next_travel_vec (pt_in, next_vecs, dist, next_flags)
UM_coord pt_in[];
UM_vector next_vecs[6];
UU_REAL *dist;
int next_flags[6];
{
	int first, current, next, attach_flag, use_same, linear;
	UU_REAL tol;
	UM_coord middle, center, loc_attach_pt;
	UM_vector dist_vect, old_dist_vec, curv_deriv, old_unit_dist;
	UM_vector span_vec;

	gettol (&tol);
	um_vctovc (next_vecs[0], dist_vect);
	um_vctovc (next_vecs[1], old_dist_vec);
	um_vctovc (next_vecs[2], middle);
	um_vctovc (next_vecs[3], center);
	um_vctovc (next_vecs[4], loc_attach_pt);
	um_vctovc (next_vecs[5], curv_deriv);

	first       = next_flags[0];
	current     = next_flags[1];
	next        = next_flags[2];
	attach_flag = next_flags[3];
	use_same    = next_flags[4];
	linear      = next_flags[5];
/*
..... Get the next travelling vector.  Go from the 1st projected
..... point (begin) along the same vector as from the 1st pattern
..... (original, projecting) point to the current pattern point.
..... Only bother to do this if the two pattern points (pt_in[current]
..... and pt_in[next]) are separated by a sizable distance.  If not,
..... set dist = 0.0.  This ensures that the travel_arc function
..... won't execute and the same result will be put in pt_out.
*/
	if(um_dcccc(pt_in[next], pt_in[current]) > tol)
	{
		if (um_mag(dist_vect) > UM_FUZZ)
			um_vctovc(dist_vect, old_dist_vec);

		switch (attach_flag)
		{
			case 2:
				um_vcmnvc(pt_in[next], middle, dist_vect);
				break;
			case 4:
				um_vcmnvc(pt_in[next], center, dist_vect);
				break;
			case 5:
				um_vcmnvc(pt_in[next], loc_attach_pt, dist_vect);
				break;
			case 1:
			case 3:
			default:
				um_vcmnvc(pt_in[next], pt_in[first], dist_vect);
				break;
		}
		*dist = um_mag(dist_vect);	/* Needed for arc length */
		if (*dist < UM_FUZZ) um_nullvc(dist_vect);
		use_same = UU_FALSE;
/*
..... Check to see if continuing on same line.  If so,
..... the two distance vectors will be colinear (parallel) and
..... their cross product will be = 0.  If they go in opposite
..... directions but are colinear, the dot product will be = 0.
..... If either distance vector = 0, the dot product will be = 0.
*/
		um_unitvc(old_dist_vec, old_unit_dist);
		um_cross(dist_vect, old_unit_dist, span_vec);
		if ((um_mag(span_vec) < UM_FUZZ) && \
		    (fabs(um_dot(old_dist_vec, dist_vect)) > UM_FUZZ))
		{
			linear = UU_TRUE;
/*
..... Handle two cases; what if direction changes so much on a long
..... curve that you need to go the opposite direction.  The second case
..... is what happens if you begin in the MIDDLE or CENTER of a linear
..... pattern.  The first half of the points will be going in one direction
..... while the second half of the points must go in the other.  Curve
..... derivatives will give opposite answers.
*/
			um_vcmnvc(pt_in[next], pt_in[current], dist_vect);
			if (um_dot(dist_vect, old_dist_vec) < 0.0)
				um_vctmsc(curv_deriv, -1.0, curv_deriv);
			if (um_dot(dist_vect, curv_deriv) < 0.0)
				um_vctmsc(dist_vect, -1.0, dist_vect);
		}
		else
			linear = UU_FALSE;
	}
	else
	{
		use_same = UU_TRUE;
	}

	next_flags[4] = use_same;
	next_flags[5] = linear;

	um_vctovc (dist_vect,    next_vecs[0]);
	um_vctovc (old_dist_vec, next_vecs[1]);
	um_vctovc (curv_deriv,   next_vecs[5]);

	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : ncl_calc_tanvecs(inpt,npts,ovec)
**       Calculates the forward (tangent) vectors for an array of points.
**    PARAMETERS
**       INPUT  :
**          inpt   - Input points.
**          npts   - Number of input points.
**       OUTPUT :
**          ovec   - Tangent vectors.
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*******************************************************************/
void ncl_calc_tanvecs(inpt,npts,ovec)
UM_coord *inpt;
int npts;
UM_vector *ovec;
{
	int i;
	UU_LOGICAL closed_fl;
/*
.....Calculate forward vectors
*/
	closed_fl = UU_FALSE;
	if (um_dcccc(inpt[0],inpt[1]) < UM_FUZZ) closed_fl = UU_TRUE;
	for (i=0; i<npts; i++)
	{
		if (i == 0)
		{
			if (closed_fl)
				um_tanvc(inpt[npts-2],inpt[0],inpt[1],ovec[i]);
			else
				um_tanvc(inpt[0],inpt[1],inpt[1],ovec[i]);
		}
		else if (i == npts-1)
		{
			if (closed_fl)
				um_tanvc(inpt[npts-2],inpt[npts-1],inpt[1],ovec[i]);
			else
				um_tanvc(inpt[npts-2],inpt[npts-1],inpt[npts-1],ovec[i]);
		}
		else
		{
			um_tanvc(inpt[i-1],inpt[i],inpt[i+1],ovec[i]);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_ve_proj_sf(cp,dcdu,surf,tfmat,u,v,ptype,pvec,ssplin,
**                                  dtol,spt,vec)
**       Projects a curve tangent vector on a surface
**    PARAMETERS
**       INPUT  :
**          cp        - Original point on curve.
**          dcdu      - Curve tangent vector.
**          surf      - Surface to project to.
**          tfmat     - Surface xform matrix.
**          u,v       - UV of surface for projection.
**          ptype     - 0 = Normal projection, Non-zero = Project along vector.
**          pvec      - Vector to project along when 'ptype' is non-zero.
**          ssplin    - UU_FALSE = return vector in XYZ space.
**                      UU_TRUE = return vector in UV-space.
**          dtol      - Tolerance to user for projection.
**          spt       - Projected point associated with 'cp'.
**       OUTPUT :
**          vec       - Projected tangent vector.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*******************************************************************/
int ncl_ve_proj_sf(cp,dcdu,surf,tfmat,u,v,side,ptype,pvec,ssplin,dtol,spt,vec)
struct NCL_fixed_databag *surf;
UM_coord cp,spt;
UM_vector dcdu,vec,pvec;
UU_REAL u,v,dtol,*side;
UU_LOGICAL ssplin;
int ptype;
{
	UM_coord cp1,uv1,uv2;
	UM_vector uvc,spt1,sn1,vec1;
	UU_REAL del;
	int status;
	UM_int2 err_flag = 0;

/*
.....Initialize routine
*/
	um_nullvc (vec);
	if (um_mag(dcdu) < UM_FUZZ) goto done;
	um_unitvc (dcdu,uvc);
	del = dtol * 5.;
/*
.....Calculate new point offset
.....from original point along vector
*/
	um_translate_point (cp,del,uvc,cp1);
	uv1[0] = uv2[0] = u; uv1[1] = uv2[1] = v;
	uv1[2] = uv2[2] = 0.;
/*
.....Project offset point onto surface
*/
	status = ncl_pt_project_sf(cp1,surf,tfmat,&uv1[0],&uv1[1],side,ptype,pvec,
		2,spt,dtol,spt1,sn1,&err_flag);
	if (status != UU_SUCCESS) return (status);
/*
.....Calculate vector in UV-space
*/
	if (ssplin)
	{
		del = um_mag(dcdu);
		um_vcmnvc(uv1,uv2,vec1);
		um_unitvc(vec1,vec);
		um_vctmsc(vec,del,vec);
	}
/*
.....Calculate vector in XYZ-space
*/
	else
	{
		um_vcmnvc (spt1,spt,vec1);
		del = 1./del;
		um_vctmsc (vec1,del,vec);
	}
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_get_prvec(surf,tfmat,u,v,pt,ang,vs,prvec)
**        determine surface projection vector as a surface normal rotated
**        about the current forward direction by a given angle
**    PARAMETERS
**       INPUT  :
**                surf  - Surface to project onto.
**                tfmat - Surface xform matrix.
**                u,v   - current surface parameters
**                pt    - current curve point
**                vs    - current curve tangent
**                ang   - angle
**       OUTPUT :
**                prvec - projection vector
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*******************************************************************/
static int S_get_prvec(surf,tfmat,u,v,pt,ang,vs,prvec)
struct NCL_fixed_databag *surf;
UM_transf tfmat;
UU_REAL u,v,ang;
UM_coord pt, vs;
UM_vector prvec;
{
	UM_vector norm;
	UM_coord sfp;
	UM_transf rottf;
	UU_REAL uua,vva,side;
	int status;
	UM_int2 err_flag = 0;

	uua = u; vva = v;
	side = 0.;
	status = ncl_pt_proj_sf(pt,surf,&uua,&vva,&side,0,UU_NULL,0.,sfp,norm,
		&err_flag);
	if (status != UU_SUCCESS) return (status);
	um_unitvc (norm,norm);

	um_vcmnvc (sfp,pt,prvec);
	if (um_dot(prvec,norm) < 0.)
		um_vctmsc (norm,-1.,norm);

	um_rottf (vs,ang,rottf);
	um_vctmtf(norm,rottf,prvec);

	return (0);
}

/*********************************************************************
**    I_FUNCTION     : S_get_sfnorm(surf,tfmat,u,v,norm)
**        Obtains the surface normal at the specified UV parameters.
**    PARAMETERS
**       INPUT  :
**                surf  - Surface/Plane to obtain normal from.
**                tfmat - Surface xform matrix.
**                u,v   - current surface parameters
**       OUTPUT :
**                norm  - Surface normal.
**    RETURNS      : UU_FAILURE on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*******************************************************************/
static int S_get_sfnorm(surf,tfmat,u,v,norm)
struct NCL_fixed_databag *surf;
UM_transf tfmat;
UU_REAL u,v;
UM_vector norm;
{
	int status;
	struct NCL_nclpl_rec *pln;
	struct UM_evsrfout evsrf;
/*
.....Entity is a plane
*/
	if (surf->rel_num == NCL_PLN_REL)
	{
		pln = (struct NCL_nclpl_rec *)surf;
		um_unitvc(pln->nvec,norm);
		status = UU_SUCCESS;
	}
/*
.....Entity is a surface
*/
	else
	{
		status = uc_evsrf(UM_NORM,u,v,surf,tfmat,&evsrf);
		um_vctovc(evsrf.snorm,norm);
	}
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_check_dis(inpt,spt,snorm,tol)
**        Determines if a projected point is already on the surface.
**    PARAMETERS
**       INPUT  :
**                inpt  - Initial point.
**                spt   - Surface projection point.
**                snorm - Surface normal at projection point.
**                tol   - Tolerance.
**       OUTPUT :
**                none
**    RETURNS      : UU_TRUE if initial point is already on surface.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*******************************************************************/
static UU_LOGICAL S_check_dis(inpt,spt,snorm,tol)
UM_coord inpt,spt;
UM_vector snorm;
UU_REAL tol;
{
	UU_LOGICAL iret;
	UU_REAL del;
	UM_vector norm,vec;
/*
.....Determine if point is on surface
*/
	iret = UU_FALSE;
	um_unitvc(snorm,norm);
	um_vcmnvc(spt,inpt,vec);
	del = UM_DOT(vec,norm);
	if (fabs(del) <= tol) iret = UU_TRUE;
	return(iret);
}

/*********************************************************************
**    I_FUNCTION     : S_check_first_vec (proj,cplist,cnlist)
**        Determines if a projected first vector should be nulled.
**    PARAMETERS
**       INPUT  :
**              proj         -  Projection structure.
**              cplist       -  Projected points.
**              cnlist       -  Projected tangent vectors.
**       OUTPUT :
**               cnlist  -  first vector possibly nulled
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_check_first_vec (proj,cplist,cnlist)
NCL_proj_struc *proj;
UU_LIST *cplist,*cnlist;
{
	UM_coord *pts;
	UM_vector *vcs;
	UM_vector vdi,vdp,uvdi,uvdp,uvci,uvcp;
	UU_REAL di,dp;

	pts = (UM_coord *)UU_LIST_ARRAY(cplist);
	vcs = (UM_vector *)UU_LIST_ARRAY(cnlist);

	um_vcmnvc (proj->inpt[1],proj->inpt[0],vdi);
	um_vcmnvc (pts[1],pts[0],vdp);

	um_unitvc (vdi,uvdi);
	um_unitvc (vdp,uvdp);

	um_unitvc (proj->tvec[0],uvci);
	um_unitvc (vcs[0],uvcp);

	di = UM_DOT (uvci,uvdi);
	dp = UM_DOT (uvcp,uvdp);

	if (di > UM_COS30 && dp < -UM_COS30)
		um_nullvc (vcs[0]);
}
