/*********************************************************************
**    NAME         :  neproj1.c
**       CONTAINS: functions for projecting points and point-vectors
**                 onto surfaces.
**
**       nclf_pt_project_sf
**       ncl_pt_project_sf
**       ncl_pt_proj_sf
**       ncl_pt_proj_sf1
**       ncl_pt_proj_pl
**       ncl_load_pt_vect
**       S_pt_proj_nearpt
**       ncl_pv_project_sf1
**       ncl_pt_project_surf
**       ncl_pt_project_so
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       neproj1.c , 25.1
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
#include "mdeval.h"
#include "msrf.h"
#include "modef.h"
#include "rbase.h"
#include "mgeom.h"
#include "msol.h"

UU_REAL ncl_pttridis();

/*********************************************************************
**    E_FUNCTION   : nclf_pt_project_sf (pt, surf_key,u,v 
**      proj_vec_flag,proj_vec, nearpt_flag, pto, err_flag)
** Fortran callable routine to project a point onto a surface.
** Solids are now valid input.
**    PARAMETERS
**       INPUT  :
**              pt              Given point that will be projected
**              surf_key        key to surface
**              u,v             starting parametric coordinate on surface
**              proj_vec_flag   1 if using a projection vector
**                              0 if using the default (normal vector)
**              proj_vect       projection vector
**              nearpt_flag     1 if using a near point
**                              0 near point not used -> ignore
**              near_pt         near point
**       OUTPUT :
**              pto             Calculated point, maybe with the normal.
**              u, v            Parametric coordinates of projected pt
**              err_flag        Returns non-zero if an error occured.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_pt_project_sf(space_pt,surf_key,u,v,proj_vec_flag,proj_vect,
	nearpt_flag,near_pt,snorm_flag,pto,err_flag)
UM_int4 *surf_key;
UM_int2 *proj_vec_flag, *nearpt_flag, *snorm_flag;
UM_real8 space_pt[],near_pt[],proj_vect[],*u,*v,pto[];
UM_int2 *err_flag;
{
	int pfl,nfl,sfl,status;
	UU_REAL alpha,side;
	UM_coord pt,surfpt,nearpt;
	UM_vector dist_vect,snorm;
	UM_transf tfmat;
	struct NCL_fixed_databag surf;
/*
.....Initialize the routine
*/
	*err_flag = 0;
	pfl = *proj_vec_flag;
	nfl = *nearpt_flag;
	sfl = *snorm_flag;
	um_vctovc(space_pt, pt);
	um_vctovc(near_pt, nearpt);
/*
.....Get the surface structure
*/
	surf.key = *surf_key;
	status = ncl_retrieve_data_fixed(&surf);
	if (surf.rel_num == UM_SOLID_REL) sfl = UU_FALSE;
	status += uc_retrieve_transf (surf.key, tfmat);
	if (status != UU_SUCCESS)
	{
		*err_flag = 113;
		return;
	}
/*
.....Project the point
*/
	side = 0.;
	status = ncl_pt_project_sf(pt,&surf,tfmat,u,v,&side,pfl,proj_vect,nfl,
			nearpt,0.,surfpt,snorm,err_flag);
	if (status == UU_SUCCESS)
	{
		um_vctovc(surfpt,pto);
		if (sfl == 1)
		{
			um_vcmnvc(pt,surfpt,dist_vect);
			alpha = UM_DOT(dist_vect,snorm);
/*
..... If surface normal is pointing away from projecting point, flip it.
*/
			if (alpha < -UM_DFUZZ) um_negvc(snorm,snorm);
			um_vctovc(snorm,&pto[3]);
		}
		else if (surf.rel_num == UM_SOLID_REL)
			um_vctovc(snorm,&pto[3]);
	}
}

/*********************************************************************
**    E_FUNCTION   : ncl_pt_project_sf (pt,surf,tfmat,u,v,side,ptype,pvec,
**                      nrptfl,nearpt,dtol,pto,norm)
** Calls appropriate projection routine.
**    PARAMETERS
**       INPUT  :
**              pt              Given point that will be projected
**              surf            Surface structure
**              tfmat           Surface transformation matrix
**              u,v             starting parametric coordinate on surface
**              side            0 = Initialize projection routine.
**              ptype           1 if using a projection vector
**                              0 if using the default (normal vector)
**              pvec            projection vector
**              nrptfl          1 if using a near point
**                              0 near point not used -> ignore
**              nearpt          near point
**              dtol            Tolerance when a projection vector is specified.
**                              A value of 0. will use the default tolerance.
**       OUTPUT :
**              u, v            Parametric coordinates of projected pt
**              side            Non-zero after projection.
**              pto             Calculated point.
**              norm            Surface normal at projection.
**    RETURNS      :
**       UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pt_project_sf(pt,surf,tfmat,u,v,side,ptype,pvec,nrptfl,nearpt,dtol,pto,
	norm,err_flag)
UM_coord pt, nearpt;
struct NCL_fixed_databag *surf;
UM_transf tfmat;
UM_vector pvec,norm;
int ptype, nrptfl;
UU_REAL *u, *v, pto[],dtol,*side;
UM_int2 *err_flag;
{
	int status;
	UR_REL_NUM rel_num;

	ur_retrieve_data_relnum(surf->key,&rel_num);
	if (rel_num == UM_SOLID_REL)
	{
		status = ncl_pt_project_so(pt,surf,ptype,pvec,nrptfl,nearpt,dtol,pto,
			err_flag);
		if (*err_flag == 0) um_vctovc(&pto[3],norm);
	}
	else
		status = ncl_pt_project_surf(pt,surf,tfmat,u,v,side,ptype,pvec,nrptfl,
			nearpt,dtol,pto,norm,err_flag);
	return status;
}

/*********************************************************************
**    E_FUNCTION   : ncl_pt_project_surf (pt,surf,tfmat,u,v,side,ptype,pvec,
**                      nrptfl,nearpt,dtol,pto,norm)
** Projects point onto a surface along a projection vector.
** The projection vector is optional.  If not used, use normal vector.
** A near point is also optional. 
**    PARAMETERS
**       INPUT  :
**              pt              Given point that will be projected
**              surf            Surface structure
**              tfmat           Surface transformation matrix
**              u,v             starting parametric coordinate on surface
**              side            0 = Initialize projection routine.
**              ptype           1 if using a projection vector
**                              0 if using the default (normal vector)
**              pvec            projection vector
**              nrptfl          1 if using a near point
**                              0 near point not used -> ignore
**              nearpt          near point
**              dtol            Tolerance when a projection vector is specified.
**                              A value of 0. will use the default tolerance.
**       OUTPUT :
**              u, v            Parametric coordinates of projected pt
**              side            Non-zero after projection.
**              pto             Calculated point.
**              norm            Surface normal at projection.
**    RETURNS      :
**       UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pt_project_surf(pt,surf,tfmat,u,v,side,ptype,pvec,nrptfl,nearpt,dtol,pto,
	norm,err_flag)
UM_coord pt, nearpt;
struct NCL_fixed_databag *surf;
UM_transf tfmat;
UM_vector pvec,norm;
int ptype, nrptfl;
UU_REAL *u, *v, pto[],dtol,*side;
UM_int2 *err_flag;
{
	int i,status,istat,cldu,cldv,primtyp;
	UU_REAL dist,dist_min,d,dgr,ugr,vgr,prims[9];
	UM_int2 errgr;
	UM_coord temp_pt, sfout, pgr;
	struct UM_evsrfout evsrf;
	UU_REAL u8, v8, u_test, v_test, good_u, good_v;
	UM_vector dist_vect, loc_proj_vec;
	UM_vector normgr;
	UU_LOGICAL found;
/*
.....Initialize routine
*/
	status = 0;
	um_vctovc(pt, temp_pt);
	if (ptype != 0) um_vctovc(pvec, loc_proj_vec);
	u8 = *u;
	v8 = *v;
/*
.....If there is both a near point and a projection vector, project the
.....near point onto the vector and supply that point to the ordinary 
.....surface projection function.
.....The basic idea is that there might be multiple intersections of
.....the surface and the projection vector.  The near point will help
.....resolve which one is the correct one (remove the ambiguity,
.....lift the degeneracy, ...).  The surface is assumed not to be
.....highly corregated (the different intersection points are not
.....too close together).  Find out which surface/proj_vect 
.....intersection point is closest to the image of the near_pt in
.....the projection vector.
*/
	primtyp = ncl_get_prim_type(surf,prims,&cldu,&cldv);
	if (nrptfl != 0 && surf->rel_num != NCL_PLN_REL && primtyp != NCLSF_PLANE)
	{
		uc_init_evsrfout (surf, &evsrf);
		if (ptype)
		{
			um_unitvc (loc_proj_vec, loc_proj_vec);
			um_nptln(nearpt,temp_pt,loc_proj_vec,temp_pt);
			status = ncl_pt_proj_sf(temp_pt, surf, &u8, &v8, side,ptype,
			     	loc_proj_vec, dtol,sfout,norm,err_flag);
			if (status != UU_SUCCESS) return (status);
			for (i = 0; i < 3; i++) pto [i] = sfout[i];
			good_u = u8;
			good_v = v8;
		}
/*
.....Near point without projection vector
*/
		else
		{
/*
........Call the nearpt projection function
........with the starting values of u,v
*/
			status = S_pt_proj_nearpt(temp_pt,surf,&u8,&v8,side,nearpt,sfout,norm,
				&d,err_flag);
			if (status != UU_SUCCESS) return (status);
/*
........Do 5x5 grid of u, v points.  Find the Cartesian coordinates for 
........each u,v on surface.  Compare distance from this location to nearpt.
........Keep the u,v that gives closest distance to nearpt.
*/
			if (nrptfl == 1)
			{
				dist_min = dgr = 1.e50;
				found = UU_FALSE;
				ugr = vgr = 0.5;

				for (u_test = 0.0; u_test <= 1.0; u_test += 0.25)
				{
					for (v_test = 0.0; v_test <= 1.0; v_test += 0.25)
					{
						uc_evsrf (UM_POINT, u_test, v_test, surf, tfmat, &evsrf);
						ncl_mcstowcs(0,evsrf.sp,evsrf.sp);
						um_vcmnvc (nearpt, evsrf.sp, dist_vect);
						dist = UM_DOT (dist_vect,dist_vect);
						if (dist < dist_min)
						{
							found = UU_TRUE;
							dist_min = dist;
							ugr = u_test;
							vgr = v_test;
						}
					}
				}
/*
.....Call the nearpt projection function with these close u,v. 
*/
				if (found)
					istat = S_pt_proj_nearpt(temp_pt,surf,&ugr,&vgr,side,nearpt,pgr,
						normgr,&dgr,&errgr);
/*
.....Use the result if it is closer to the near point
*/
				if (found && istat == UU_SUCCESS && dgr+UM_DFUZZ < d)
				{
					for (i = 0; i < 3; i++)
					{
						sfout[i] = pgr[i];
						norm[i] = normgr[i];
					}
					u8 = ugr;
					v8 = vgr;
				}
			}

			for (i = 0; i < 3; i++) pto[i] = sfout[i];
			good_u = u8;
			good_v = v8;
		}
	}
/*
.....No near point
*/
	else
	{
		status = ncl_pt_proj_sf(temp_pt, surf, &u8, &v8, side, ptype,
			loc_proj_vec, dtol, pto,norm,err_flag);
		if (status != UU_SUCCESS) return (status);
		good_u = u8;
		good_v = v8;
	}
	*u = good_u;
	*v = good_v;
/*
.....End of routine
*/
	return (status);
}

/*********************************************************************
**    I_FUNCTION   : S_sfpt2 (key,pt,u,v,unit,side,spt,snorm,err)
** Projects a point onto a surface by calling sfpt2; makes several attemps to
** avoid a convergence failure.
**    PARAMETERS
**       INPUT  :
**              key      NCL key for the surface
**              pt       Point off surface that is being projected
**              u, v     Starting parametric coordinates
**              unit     0 return point in current units and model space
**                       1 return point in inches and world space
**              side     0 for first call, returned value for subsequent calls
**       OUTPUT :
**              spt,snorm    The projected point on surface.
**              u, v         Parametric coordinates for projected pt
**              side         1 or -1 to indicate whether surface normal is
**                           flipped
**              err          error flag
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_sfpt2 (key,pt,u,v,unit,side,spt,snorm,err)
UU_KEY_ID key;
UM_coord pt,spt;
UM_vector snorm;
UM_real8 *u, *v, *side;
UM_int4 unit,*err;
{
	UM_real8 u0,v0,dir0,uc,vc,dirc,du,dv;
	int i;
	UU_REAL alf = 45./UM_RADIAN;

	u0 = *u; v0 = *v; dir0 = *side;

	uc = u0; vc = v0; dirc = dir0;

	sfpt2(&key,pt,&uc,&vc,&unit,&dirc,spt,snorm,err);

	for (i = 0; i < 8 && *err == 128; i++)
	{
		du = 0.1*cos(alf*i);
		dv = 0.1*sin(alf*i);
		uc = u0 + du;
		vc = v0 + dv;
		if (uc > 1 || uc < 0) continue;
		if (vc > 1 || vc < 0) continue;
		dirc = dir0;
		sfpt2(&key,pt,&uc,&vc,&unit,&dirc,spt,snorm,err);
	}

	if (*err <= 0)
	{
		*u = uc; *v = vc; *side = dirc;
	}
}

/*********************************************************************
**    E_FUNCTION   : ncl_pt_proj_sf (pt, surf, u, v, side, ptype, pvec,
**                                dtol, pto, norm) 
** Projects point onto a surface.
**    PARAMETERS
**       INPUT  :
**              pt       Point off surface that is being projected.
**              surf     Surface/Plane structure.
**              u, v     Starting parametric coordinates
**              side     0 = Initialize projection routine.
**              ptype    1 = Project along a vector.
**              pvec     Projection vector.
**              dtol     Tolerance when a projection vector is specified.
**                       A value of 0. will use the default tolerance.
**       OUTPUT :
**              u, v     Parametric coordinates for projected pt
**              side     Non-zero after projection.
**              pto      The projected point on surface.
**              norm     Surface normal at projection.
**    RETURNS      :
**       UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pt_proj_sf(pt, surf, u, v, side, ptype, pvec, dtol, pto, norm, err_flag)
struct NCL_fixed_databag *surf;
UM_coord pt;
UU_REAL *u, *v,dtol,*side;
int ptype;
UM_vector pvec;
UM_vector norm;
UM_coord pto;
UM_int2 *err_flag;
{
	int j,status,nint,uv_data[3],prim;
	UM_int2 idx;
	UM_int4 unit,err;
	UU_REAL tol,dist,prim_data[5];
	UM_real8 u8, v8,ver;
	UM_coord local_pt,plpt;
	UM_vector loc_proj_vec,base,plve;
	struct NCL_nclpl_rec *pln;
	UU_LOGICAL swapped;
/*
.....Get version flag
*/
	idx = 169; getsc(&idx,&ver);
/*
.....Determine if surface is planar
*/
	if (surf->rel_num == NCL_PLN_REL)
	{
		pln = (struct NCL_nclpl_rec *)surf;
		um_vctovc(pln->nvec,plve);
		um_vctovc(pln->pt,plpt);
	}
	else
		prim = ncl_get_prim_data(surf,plpt,plve,prim_data,uv_data);
/*
.....Project to plane
*/
	if (surf->rel_num == NCL_PLN_REL || prim == NCLSF_PLANE)
	{
		status = S_pt_proj_pl(pt,plve,plpt,ptype,pvec,norm,pto);
/*
.....Get the UV for a surface projection
*/
		if (surf->rel_num != NCL_PLN_REL)
		{
			unit = 1;
			S_sfpt2(surf->key,pto,u,v,unit,side,local_pt,loc_proj_vec,&err);
		}
	}
/*
.....Project to surface
*/
	else
	{
		unit = 1;
		um_vctovc(pt, local_pt);
		um_vctovc(pt, base);
		if (ptype != 0) um_vctovc(pvec, loc_proj_vec);
		u8 = *u;  v8 = *v;
/*
........Project point
*/
		S_sfpt2(surf->key,local_pt,&u8,&v8,unit,side,pto,norm,&err);
		if (err > 0)
		{
			*err_flag = err;
			return (UU_FAILURE);
		}
		if (!ptype)
			um_nptpln(local_pt,pto,norm,pto);
/*
........Project point along a vector
*/
		else
		{
			if (dtol == 0.) gettol(&tol);
			else tol = dtol;
			um_unitvc(loc_proj_vec, loc_proj_vec);
			um_unitvc(norm, norm);
/*
...........Use the point on the vector
...........as the projected point must be on the vector
...........and within tolerance of the surface
...........Bobby - 4/7/11
*/
			if (ver < 9.850 || ptype == 2)
			{
				um_nptln (pto, base, loc_proj_vec, local_pt);
				swapped = UU_FALSE;
			}
			else
			{
				um_vctovc(pto,local_pt);
				um_nptln (local_pt, base, loc_proj_vec, pto);
				swapped = UU_TRUE;
			}
			dist = um_dcccc(pto, local_pt);
			j = 0;
/*
...........Try to get close to surface.
...........j is there to prevent infinite loop.
*/
			while ((fabs(dist) > tol) && j < 20)
			{
				if (swapped) um_vctovc(pto,local_pt);
				um_ilnpln(local_pt, loc_proj_vec,pto, norm, &nint, local_pt);
				if (nint < 1) return UU_FAILURE;
/*
...........IMPORTANT PART!! All the work happens here.
*/
				sfpt2(&surf->key,local_pt,&u8,&v8,&unit,side,pto,norm,&err);
				if (err > 0)
				{
					*err_flag = err;
					return (UU_FAILURE);
				}

				um_unitvc(norm,norm); 
/*
...........If on edge of surface, project down onto normal plane (extension)
...........Change pto to be this new extended point, use for distance.
*/
				if ((u8 == 0.0 || u8 == 1.0) || \
					(v8 == 0.0 || v8 == 1.0))
				{
					um_nptpln (local_pt, pto, norm, pto);
				}
/*
...........Temp_pt is used at beginning of loop to ensure the dist_vec
...........points at the surface.
*/
				if (ver < 9.850 || ptype == 2)
					um_nptln (pto, base, loc_proj_vec, local_pt);
				else
				{
					um_vctovc(pto,local_pt);
					um_nptln (local_pt, base, loc_proj_vec, pto);
				}
				dist = um_dcccc(local_pt, pto);

				j++;
/*
........End while loop
*/
			}
		}
		*u = u8;
		*v = v8;
	}
/*
.....End of routine
*/
	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION   : ncl_pt_proj_sf1 (pt,asw,isf,side,u4,v4,spt,norm,vpn,tol)
** Projects point onto a surface along a projection vector. Separate routine
** called during fmill command.
**    PARAMETERS
**       INPUT  :
**              pt              point to be projected
**              asw             surface word
**              isf             surface number in the table
**              side            flag for sfpt (to prevent initializition)
**              u4,v4           starting parameters on surface
**              vpn             projection vector
**              tol             tolerance
**       OUTPUT :
**              spt             point on surface
**              norm            surface normal
**              u4,v4           parameters on surface
**    RETURNS      :
**       UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pt_proj_sf1 (pt,asw,isf,side,u4,v4,spt,norm,vpn,tol)
UM_coord pt,spt;
UM_real8 asw;
UM_real4 *u4, *v4, side;
UM_int2 isf;
UM_vector norm,vpn;
UU_REAL tol;
{
	int i,j,k;
	UM_int2 ifl,ival;
	UM_real8 pt8[3];
	UM_real4 ss[9];
	UU_REAL cosa,del;
	UM_vector vec;
	UU_LOGICAL bon = UU_FALSE;
		
	um_vcmnvc(spt,pt,vec);
	del = UM_DOT (vec,norm);
	bon = (fabs(del) < tol);

	for (j = 0; j < 20 && !bon; j++)
	{
		cosa = UM_DOT (vpn,norm);
		
		um_translate_point (pt,del/cosa,vpn,pt);

		for (k = 0; k < 3; k++) pt8[k] = pt[k];

		sfpt (&asw, pt8, &isf, &side, u4, v4, ss);
		ifl = 2; getifl(&ifl,&ival);
		if (ival == 128)
		{
			bon = UU_FALSE;
			goto done;
		}
		for (i = 0; i < 3; i++)
		{
			norm [i] = ss[i]; spt[i] = ss[4+i];
		}
		um_unitvc(norm,norm);
/*
..... If on edge of surface, project down onto normal plane (extension)
*/
		if (*u4 == 0.0 || *u4 == 1.0 || *v4 == 0.0 || *v4 == 1.0)
		{
			um_vcmnvc(spt,pt,vec);
			del = UM_DOT (vec,norm);
			um_translate_point (pt,del,norm,spt);
		}

		um_vcmnvc(spt,pt,vec);
		del = UM_DOT (vec,norm);
		bon = (fabs(del) < tol);
	}
done:;
	if (bon)
		return (UU_SUCCESS);
	else
		return (UU_FAILURE);
}

/*********************************************************************
**    E_FUNCTION   : ncl_pt_proj_pl(pt,pln,ptype,pvec,norm,pto) 
** Projects a point onto a plane.
**    PARAMETERS
**       INPUT  :
**              pt       Point off surface that is being projected.
**              pln      Plane structure.
**              ptype    1 = Project along a vector.
**              pvec     Projection vector.
**       OUTPUT :
**              norm     Surface normal at projection.
**              pto      The projected point on surface.
**    RETURNS      :
**       UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pt_proj_pl(pt,pln,ptype,pvec,norm,pto) 
UM_coord pt,pto;
UM_vector norm;
struct NCL_nclpl_rec *pln;
{
/*
.....Project point onto plane
*/
	return(S_pt_proj_pl(pt,pln->nvec,pln->pt,ptype,pvec,norm,pto));
}

/*********************************************************************
**    E_FUNCTION   : S_pt_proj_pl(pt,plvec,plpt,ptype,pvec,norm,pto) 
** Projects a point onto a plane as defined by a vector and a point.
**    PARAMETERS
**       INPUT  :
**              pt       Point off surface that is being projected.
**              plvec    Plane normal.
**              plpt     Point on plane normal.
**              ptype    1 = Project along a vector.
**              pvec     Projection vector.
**       OUTPUT :
**              norm     Surface normal at projection.
**              pto      The projected point on surface.
**    RETURNS      :
**       UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_pt_proj_pl(pt,plvec,plpt,ptype,pvec,norm,pto) 
UM_coord pt,pto,plpt;
UM_vector norm,plvec;
{
	int status;
	UU_REAL dist;
	UM_vector loc_proj_vec,vec;
	UU_REAL dot,h;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	um_vctovc(plvec,norm);
	if (ptype != 0)
		um_unitvc (pvec,loc_proj_vec);
	else
		um_vctovc (norm,loc_proj_vec);
/*
.....Project onto plane
*/
	dot = UM_DOT (loc_proj_vec,norm);
	if (fabs(dot) < UM_DFUZZ) return UU_FAILURE;
	um_vcmnvc(plpt,pt,vec);
	h = UM_DOT (vec,norm);
	dist = h/dot;
	um_vctmsc(loc_proj_vec,dist,loc_proj_vec);
	um_vcplvc(pt,loc_proj_vec,pto);
	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION   : ncl_load_pt_vect (nkey, ptvect_flag, obj) 
** Takes an NCL key, figures out if it is a point, vector , or a 
** point-vector.  Then, it loads the point or vector info (i,j,k, or
** (x,y,z) into the output obj.
** If it is a point-vector, the flag tells it which part to take:
** point part or the vector part.
**    PARAMETERS
**       INPUT  :
**              key            NCL key for the surface
**              ptvect_ flag   1 use point part
**                             2 use vector part
**       OUTPUT :
**              obj            (x,y,z) or (i,j,k)
**    RETURNS      :
**		UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR       : Ed Ames 18 Aug 00
*********************************************************************/
int ncl_load_pt_vect (nkey, ptvect_flag, obj)
UM_int4 *nkey;
short ptvect_flag;
double obj[3];
{
	int i;
	struct NCL_fixed_databag e;
	struct NCL_vector_rec *vect_rec;
	struct NCL_nclpv_rec *pv_rec;
	struct NCL_nclpt_rec *pt_rec;

	e.key = *nkey;
	ncl_retrieve_data_fixed(&e);
	if ((e.rel_num == UM_POINT_REL) || (e.rel_num == NCL_POINT_REL))
	{
		pt_rec = (struct NCL_nclpt_rec *) &e;
		for (i = 0; i < 3; i++)
			obj[i] = pt_rec->pt[i];
		return UU_SUCCESS;
	}

	if (e.rel_num == NCL_POINTVEC_REL)
	{
		pv_rec = (struct NCL_nclpv_rec *) &e;
		if (ptvect_flag == 1)
		{
			for (i = 0; i < 3; i++)
				obj[i] = pv_rec->pt[i];
			return UU_SUCCESS;
		}
		if (ptvect_flag == 2)
		{
			for (i = 0; i < 3; i++)
				obj[i] = pv_rec->ve[i];
			return UU_SUCCESS;
		}
		
		return UU_FAILURE;
	}

	if (e.rel_num == NCL_VECTOR_REL)
	{
		vect_rec = (struct NCL_vector_rec *) &e;
		for (i = 0; i < 3; i++)
			obj[i] = vect_rec->vec[i];
		return UU_SUCCESS;
	}

	return UU_FAILURE;
}

#if 0
/*********************************************************************
**    E_FUNCTION :  ncl_pv_project_sf1 (space_pt, surf_key,u,v, proj_vec_flag, 
**		proj_vec, nearpt_flag, near_pt, pto, snorm, err_flag)
**       Evalute a pntvec. 
**    PARAMETERS
**       INPUT  :
**              space_pt        Given point that will be projected
**              surf_key        key to surface
**              u, v            starting parametric coordinate on surface
**              proj_vec_flag   1 if using a projection vector
**                              0 if using the default (normal vector)
**              proj_vec        projection vector
**              nearpt_flag     1 if using a near point
**                              0 near point not used -> ignore
**              near_pt         near point
**       OUTPUT :
**              pto             Calculated point.
**              snorm           Normal to surface at pt
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pv_project_sf1 (space_pt, surf_key,u,v, proj_vec_flag, proj_vec, \
			nearpt_flag, near_pt, pto, snorm, err_flag)
UM_int4 *surf_key;
UM_int2 *proj_vec_flag, *nearpt_flag;
UM_real8 *u, *v, space_pt[], proj_vec[], near_pt[], pto[], snorm[];
UM_int2 *err_flag;
{
	int status;
	UU_REAL alpha, change_dir,side;
	UM_coord pt, surfpt;
	UM_vector dist_vect,norm;
	struct NCL_fixed_databag e1;
	UM_transf tfmat;
	int pfl,nfl;

/*
.....Initialize routine
*/
	pfl = *proj_vec_flag;
	nfl = *nearpt_flag;
	um_vctovc(space_pt, pt);
/*
.....Get the surface data
*/
	status = UU_SUCCESS;
	e1.key = *surf_key;
	status = ncl_retrieve_data_fixed(&e1);
	status += uc_retrieve_transf (e1.key, tfmat);
	if (status != UU_SUCCESS)
	{
		*err_flag = UU_FAILURE;
		return;
	}
/*
.....Get u and v for the projected point on surface.
*/
	side = 0.;
	status = ncl_pt_project_sf(pt,&e1,tfmat,u,v,&side,pfl,proj_vec,nfl,near_pt,
		0.,surfpt,norm);
	if (status != UU_SUCCESS)
	{
		*err_flag = UU_FAILURE;
		return;
	}
	um_vcmnvc(pt,surfpt,dist_vect);
	alpha = um_dot(dist_vect,norm);
/*
..... If surface normal is pointing away from projecting point, flip it.
*/
	if (alpha < 0)
		change_dir = -1.0;
	else
		change_dir = 1.0;
	um_vctovc(surfpt,pto);
	um_vctmsc(norm,change_dir,snorm);
	*err_flag = UU_SUCCESS;
	return;
}
#endif

/*********************************************************************
**    S_FUNCTION   : S_pt_proj_nearpt(pt,surf,u,v,side,nearpt,pto,norm,dis)
**       Projects a point onto a surface, finding the closest projection
**       to a near point.
**    PARAMETERS
**       INPUT  :
**              pt       Point off surface that is being projected.
**              surf     Surface structure.
**              u, v     Starting parametric coordinates
**              side     0 = Initialize projection routine.
**              nearpt   1 = Project along a vector.
**       OUTPUT :
**              u, v     Parametric coordinates for projected pt
**              side     Non-zero after projection.
**              pto      The projected point on surface.
**              norm     Surface normal at projection.
**              dis      Squared distance between the projection and the
**                       near point.
**    RETURNS      :
**       UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_pt_proj_nearpt(pt,surf,u,v,side,nearpt,pto,norm,dis,err_flag)
struct NCL_fixed_databag *surf;
UM_coord pt,nearpt,pto;
UM_vector norm;
UU_REAL *u,*v,*side,*dis;
UM_int2 *err_flag;
{
	int i,status;
	UM_int4 unit,err;
	UM_real8 u0,v0,ui,vi;
	UM_coord surfpt,pi;
	UM_vector normi,vec;
	UU_REAL d0,di,e0,e1;
	UM_int2 idx,ival;

	unit = 1;
	u0 = *u;  v0 = *v;
/*
.....Project the point to the surface.
*/
	S_sfpt2(surf->key,pt,&u0,&v0,unit,side,surfpt,norm,&err);
	if (err > 0)
	{
		*err_flag = err;
		return (UU_FAILURE);
	}
/*
.....Project point onto the surface plane.
*/
	status = UU_SUCCESS;
	um_nptpln (pt, surfpt, norm, pto);
	um_vcmnvc (nearpt, pto, vec);
	d0 = UM_DOT (vec,vec);
	e0 = um_dcccc(pto,surfpt) + UM_FUZZ;
/*
.....Try projecting using non-standard convergence flags
.....Choose the result closest to the near point
*/
	if (surf->rel_num == UM_RBSPLSRF_REL || surf->rel_num == NCL_SURF_REL ||
				surf->rel_num == NCL_REVSURF_REL || surf->rel_num == NCL_TRIMSF_REL)
	{
		idx = 385;
		for (i = 1; i < 4; i++)
		{
			ival = i;
			setifl(&idx,&ival);
			ui = *u;  vi = *v;

			sfpt2(&surf->key,pt,&ui,&vi,&unit,side,surfpt,normi,&err);
			if (err == 0)
			{
				um_nptpln (pt, surfpt, normi, pi);
				um_vcmnvc (nearpt, pi, vec);
				di = UM_DOT (vec,vec);
				e1 = um_dcccc(pi,surfpt);
				if (di < d0 && e1 <= e0)
				{
					d0 = di;
					u0 = ui; v0 = vi;
					um_vctovc(pi,pto);
					um_vctovc(normi,norm);
				}
			}
		}
		ival = 0;
		setifl(&idx,&ival);
	}

	*u = u0; *v = v0;
	*dis = d0;

done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION  : ncl_pt_project_so (pt,surf,side,ptype,pvec,nrptfl,
**                                     nearpt,dtol,pto)
**  Projects point onto a solid along a projection vector.  The
**  projection vector is optional. If no vector is given the point is
**  projected based on the type of solid.  If the solid is a primitive
**  then the point is projected onto the primitive; otherwise, the 
**  point is projected onto the tesselation.  A near point is also
**  optional.
**    PARAMETERS
**       INPUT  :
**           pt     - Given point that will be projected
**           sol    - Solid structure
**           ptype  - 1 if using a projection vector
**                  - 0 if using the default (normal vector)
**           pvec   - projection vector
**           nrptfl - 1 if using a near point
**                  - 0 near point not used -> ignore
**           nearpt - near point
**           dtol   - Tolerance when a projection vector is specified.
**                  - A value of 0. will use the default tolerance.
**       OUTPUT :
**           pto    - Calculated point and normal vector.
**    RETURNS      :
**       UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pt_project_so(pt,sol,ptype,pvec,nrptfl,nearpt,dtol,pto,err_flag)
UM_coord pt, nearpt;
struct UM_solid_rec *sol;
UM_vector pvec;
int ptype, nrptfl;
UU_REAL pto[],dtol;
UM_int2 *err_flag;
{
	UU_REAL dist,dist_min,tolr,tridist,stridist;
	UU_REAL dot,hgt,center_check,angle,max_deg;
	UU_REAL max_dist,rad1,rad2,d1,d2,d3,scal,dot2;
	UM_coord npt,ipt1,ipt2,ipt3,ipt4,svpt,tpt,tpt1,tpt2;
	UM_coord cpt[8];
	int i,j,status,ntri,isec;
	UM_vector proj,vec,tvec,lvec;
	UM_vector tvec1,tvec2,xvec,yvec,zvec;
	NCL_xtriangle *xt;
	UM_trian *ptri;
	UU_LOGICAL found,xdir,ydir,zdir;
/*
.....Initialize routine
*/
	found = UU_FALSE;
	status = UU_SUCCESS;
	max_deg = 0.0523598756;  /* 3 degrees in radians */
	stridist = dist_min = 1.e25;
	if (dtol == 0.) gettol(&tolr);
	else tolr = dtol;
	ncl_set_boundary_toler (tolr);
	um_set_tess_toler (tolr);
/*
.....Set near point for distance comparison
*/
	if (nrptfl) um_vctovc(nearpt,npt);
	else um_vctovc(pt,npt);
	if (ptype || sol->type > 5)
	{
/*
.....Get tessellation list for solid
*/
		status = ncl_get_trilist(sol,tolr,UM_TESS_TOLER,100,&xt,&ntri);
		if (status != UU_SUCCESS || !xt)
		{
			*err_flag = 12; /* Invalid geometry format */
			return UU_FAILURE;
		}
		for (i=0;i<ntri;i++)
		{
			if (ptype)
			{
				isec = 0;
				ncl_segtri_io(pt,pt,pvec,&xt[i],&ipt1,&ipt2,&vec,tolr,&isec);
				if (isec == 1)
				{
					found = UU_TRUE;
					dist = um_dcccc(npt,ipt1);
					if (dist < dist_min)
					{
						dist_min = dist;
						um_vctovc(ipt1,pto);
						um_vctovc(&xt[i].pln,&pto[3]);
						um_vcmnvc(pt,pto,tvec);
						dot = um_dot(&pto[3],tvec);
						if (dot < 0.) um_negvc(&pto[3],&pto[3]);
						if (dist_min < UM_DFUZZ) break;
					}
				}
			}
/*
.....Project the given point to the triangle plane and then find the
.....closest point in/on the triangle for each triangle.  The desired
.....point will minimize the distance between the projected point on
.....the plane and the closest point on the triangle (ideally 0). Note
.....that the point returned is the point on the triangle and not the
.....one projected onto the plane.
*/
			else
			{
				found = UU_TRUE;
				um_nptpln(pt,xt[i].p1,xt[i].pln,ipt1);
				tridist = ncl_pttridis(ipt1,&xt[i],ipt2);
				dist = um_dcccc(npt,ipt2);
				if (tridist > stridist && fabs(tridist - stridist) >= 5.*tolr)
					continue;
				else if ((fabs(tridist - stridist) < 5.*tolr && dist < dist_min)
							|| fabs(tridist - stridist) >= 5.*tolr)
				{
					um_vcmnvc(ipt1,pt,tvec1);
					um_vcmnvc(ipt2,pt,tvec2);
					angle = um_angle(tvec1,tvec2);
					dist_min = dist;
					if (tridist < stridist) stridist = tridist;
					um_vctovc(ipt2,pto);
					um_vctovc(&xt[i].pln,&pto[3]);
					um_vcmnvc(pt,pto,tvec);
					dot = um_dot(&pto[3],tvec);
					if (dot < 0.) um_negvc(&pto[3],&pto[3]);
				}
			}
		}
		d1 = um_dcccc(pt,svpt);
		if (!found || (!ptype && angle > max_deg && d1 != 0))
		{
			*err_flag = 163;
			return(status);
		}
	}
/*
.....Solid is a primitive
*/
	else
	{
		if (sol->type == UM_SPHERE_SOLID)
		{
			rad1 = sol->sdata[3];
			um_vctovc(&sol->sdata[0],svpt);
			center_check = um_dcccc(pt,svpt);
			if (center_check < UM_DFUZZ)
			{
				vec[0] = 1.; vec[1] = vec[2] = 0.;
			}
			else
				um_vcmnvc(pt,svpt,vec);
			um_unitvc(vec,vec);
			um_vctovc(vec,&pto[3]);
			um_translate_point(svpt,rad1,vec,pto);
/*
.....Reverse direction to get other intersection point if there is a
.....near point
*/
			if (nrptfl)
			{
				um_negvc(vec,vec);
				um_translate_point(svpt,rad1,vec,ipt1);
				dist_min = um_dcccc(npt,pto);
				dist = um_dcccc(npt,ipt1);
				if (dist < dist_min)
				{
					um_vctovc(ipt1,pto);
					um_vctovc(vec,&pto[3]);
				}
			}
		}
		else if (sol->type == UM_BOX_SOLID)
		{
			um_vctovc(&sol->sdata[3],xvec);
			um_vctovc(&sol->sdata[6],yvec);
			um_vctovc(&sol->sdata[9],zvec);
/*
.....Center Point
*/
			um_vctmsc(xvec,.5,xvec);
			um_vctmsc(yvec,.5,yvec);
			um_vctmsc(zvec,.5,zvec);
			um_vctovc(&sol->sdata[0],svpt);
			um_vcplvc(svpt,xvec,svpt);
			um_vcplvc(svpt,yvec,svpt);
			um_vcplvc(svpt,zvec,svpt);
			center_check = um_dcccc(pt,svpt);
			if (center_check < UM_DFUZZ)
			{
				um_vcplvc(svpt,xvec,pto);
				return(status);
			}
			um_vctmsc(xvec,2.,xvec);
			um_vctmsc(yvec,2.,yvec);
			um_vctmsc(zvec,2.,zvec);
/*
.....Corner points
*/
			um_vctovc(&sol->sdata[0],cpt[0]);
			um_vcplvc(cpt[0],xvec,cpt[1]);
			um_vcplvc(cpt[1],zvec,cpt[2]);
			um_vcplvc(cpt[0],zvec,cpt[3]);
			um_vcplvc(cpt[0],yvec,cpt[4]);
			um_vcplvc(cpt[4],xvec,cpt[5]);
			um_vcplvc(cpt[5],zvec,cpt[6]);
			um_vcplvc(cpt[4],zvec,cpt[7]);
/*
.....Find closest corners
*/
			d1 = d2 = d3 = 1.e25;
			for (i=0;i<8;i++)
			{
				dist = um_dcccc(pt,cpt[i]);
				if (dist < d1)
				{
					d3 = d2;
					d2 = d1;
					d1 = dist;
					um_vctovc(ipt2,ipt3);
					um_vctovc(ipt1,ipt2);
					um_vctovc(cpt[i],ipt1);
				}
				else if (dist < d2)
				{
					d3 = d2;
					d2 = dist;
					um_vctovc(ipt2,ipt3);
					um_vctovc(cpt[i],ipt2);
				}
				else if (dist < d3)
				{
					d3 = dist;
					um_vctovc(cpt[i],ipt3);
				}
			}
/*
.....Create a line between the two closest points and another
.....parallel to it starting from the third closest.  Then,
.....find the closest point on each line and create a line
.....between them.  The closest projection to the given point
.....will be on this line.
*/
			um_vcmnvc(ipt2,ipt1,lvec);
			um_translate_point(ipt3,1.,lvec,ipt4);
			um_unitvc(lvec,lvec);
			um_nptln(pt,ipt1,lvec,tpt1);
			um_vcmnvc(tpt1,ipt1,tvec1);
			um_vcmnvc(tpt1,ipt2,tvec2);
			dot = um_dot(tvec1,tvec2);
			if (dot > 0.)
			{
				*err_flag = 163;
				return(status);
			}
			um_nptln(pt,ipt3,lvec,tpt2);
			um_vcmnvc(tpt2,ipt3,tvec1);
			um_vcmnvc(tpt2,ipt4,tvec2);
			dot = um_dot(tvec1,tvec2);
			if (dot > 0.)
			{
				*err_flag = 163;
				return(status);
			}
			um_vcmnvc(tpt2,tpt1,lvec);
			um_unitvc(lvec,lvec);
			um_cross(tvec1,lvec,&pto[3]);
			um_unitvc(&pto[3],&pto[3]);
			um_nptln(pt,tpt1,lvec,pto);
			um_vcmnvc(pto,tpt1,tvec1);
			um_vcmnvc(pto,tpt2,tvec2);
			dot = um_dot(tvec1,tvec2);
			if (dot > 0.)
			{
				*err_flag = 163;
				return(status);
			}
			um_vcmnvc(pt,pto,tvec1);
			dot = um_dot(&pto[3],tvec1);
			if (dot < 0.) um_negvc(&pto[3],&pto[3]);
/*
.....Check opposite face if there is a near point.  The found
.....point checked against its corresponding point on the face
.....opposite the one it is on.
*/
			if (nrptfl)
			{
				xdir = ydir = zdir = UU_FALSE;
				scal = 1.;
				um_vcmnvc(pto,pt,tvec);
				um_unitvc(tvec,tvec);
				xdir = um_vcparall(tvec,xvec);
				ydir = um_vcparall(tvec,yvec);
				zdir = um_vcparall(tvec,zvec);
				if (xdir) um_vctovc(xvec,tvec1);
				else if (ydir) um_vctovc(yvec,tvec1);
				else um_vctovc(zvec,tvec1);
				d1 = um_dot(tvec,zvec);
				if (d1 < 0.) scal = -1.;
				um_translate_point(pto,scal,tvec1,ipt1);
				dist_min = um_dcccc(pto,npt);
				dist = um_dcccc(ipt1,npt);
				if (dist < dist_min) 
				{
					um_vctovc(ipt1,pto);
					um_negvc(&pto[3],&pto[3]);
				}
			}
		}
		else if (sol->type == UM_CYLINDER_SOLID || 
					sol->type == UM_CONE_SOLID)
		{
			rad1 = sol->sdata[6];
			if (sol->type == UM_CYLINDER_SOLID)
				rad2 = sol->sdata[6];
			else
				rad2 = sol->sdata[7];
			um_vcmnvc(pt,&sol->sdata[0],tvec);
			um_vctovc(&sol->sdata[3],vec);
			um_unitvc(vec,vec);
			dot = um_dot(vec,tvec);
			hgt = um_mag(&sol->sdata[3]);
			um_nptln(pt,&sol->sdata[0],vec,svpt);
			um_vcplvc(&sol->sdata[0],&sol->sdata[3],ipt2);
/*
.....Given point is past one of the ends so a projection onto a
.....plane is needed
*/
			if (dot < 0. || dot > hgt)
			{
				if (rad1 > 0. || rad2 > 0.)
				{
					if (rad1 > 0.)
					{
						um_nptpln(pt,&sol->sdata[0],vec,pto);
						d1 = um_dcccc(pto,&sol->sdata[0]);
						if (rad2 <= 0. && d1 > rad1)
						{
							*err_flag = 163;
							return(status);
						}
						um_vcmnvc(pt,pto,&pto[3]);
						um_unitvc(&pto[3],&pto[3]);
						if (dot > hgt) um_negvc(&pto[3],&pto[3]);
					}
					if (rad2 > 0.)
					{
						um_nptpln(pt,ipt2,vec,ipt3);
						d2 = um_dcccc(ipt3,ipt2);
						if (rad1 <= 0. && d2 > rad2)
						{
							*err_flag = 163;
							return(status);
						}
						if (rad1 > 0. && d1 <= rad1)
						{
							dist = um_dcccc(npt,ipt3);
							dist_min = um_dcccc(npt,pto);
							if (dist < dist_min && d2 <= rad2)
							{
								um_vctovc(ipt3,pto);
								um_negvc(&pto[3],&pto[3]);
							}
						}
						else if (d2 <= rad2)
						{
							um_vctovc(ipt3,pto);
							um_vcmnvc(pt,pto,&pto[3]);
							um_unitvc(&pto[3],&pto[3]);
							if (dot < 0.) um_negvc(&pto[3],&pto[3]);
						}
						else
						{
							*err_flag = 163;
							return(status);
						}
					}
				}
				else
				{
					*err_flag = 163;
					return(status);
				}
			}
/*
.....Point lies between ends so the projection will be on the side wall
.......A line is created that is on the plane that is defined by the given
.......point and the end points of the entity.  The line represents the 
.......cross section of the surface and the given point is projected
.......onto this line.
*/
			else
			{
				center_check = um_dcccc(pt,svpt);
				if (center_check < UM_DFUZZ)
				{
					for (i=0;i<3;i++) tvec[i] = sol->sdata[i+3] + 1;
					um_vcortho(&sol->sdata[3],tvec);
				}
				else
					um_vcmnvc(pt,svpt,tvec);
				um_unitvc(tvec,tvec);
				um_translate_point(&sol->sdata[0],rad1,tvec,ipt1);
				um_translate_point(ipt2,rad2,tvec,ipt3);
				um_vcmnvc(ipt3,ipt1,lvec);
				um_unitvc(lvec,lvec);
				um_nptln(pt,ipt1,lvec,pto);
				um_vcmnvc(pt,pto,tvec1);
				um_unitvc(tvec1,tvec1);
				um_vctovc(tvec1,&pto[3]);
				um_vcmnvc(pto,ipt1,tvec1);
				um_unitvc(tvec1,tvec1);
				um_vcmnvc(pto,ipt3,tvec2);
				um_unitvc(tvec2,tvec2);
				dot = um_dot(tvec1,tvec2);
/*
.....Reverse direction to see if another intersection is possible
*/
				if (nrptfl)
				{
					um_vctmsc(tvec,-1.,tvec);
					um_translate_point(&sol->sdata[0],rad1,tvec,ipt1);
					um_translate_point(ipt2,rad2,tvec,ipt3);
					um_vcmnvc(ipt3,ipt1,lvec);
					um_unitvc(lvec,lvec);
					um_nptln(pt,ipt1,lvec,tpt);
					um_vcmnvc(tpt,ipt1,tvec1);
					um_vcmnvc(tpt,ipt3,tvec2);
					dot2 = um_dot(tvec1,tvec2);
					d1 = um_mag(tvec1);
					d2 = um_mag(tvec2);
					if (dot2 < 0. || d1 == 0. || d2 == 0.)
					{
						dist = um_dcccc(npt,tpt);
						dist_min = um_dcccc(npt,pto);
						if (dist < dist_min || dot > 0.)
						{
							um_vctovc(tpt,pto);
							um_vcmnvc(pto,pt,&pto[3]);
							um_unitvc(&pto[3],&pto[3]);
						}
					}
					else if (dot > 0.)
					{
						*err_flag = 163;
						return(status);
					}
				}
				else if (dot > 0.)
				{
					*err_flag = 163;
					return(status);
				}
			}
		}
		else if (sol->type == UM_TORUS_SOLID)
		{
			rad1 = sol->sdata[6];
			rad2 = sol->sdata[7];
			um_vctovc(&sol->sdata[3],tvec);
			um_unitvc(tvec,tvec);
			um_vctovc(&sol->sdata[0],svpt);
			um_nptpln(pt,&sol->sdata[0],tvec,ipt1);
			center_check = um_dcccc(pt,svpt);
			dist = um_dcccc(ipt1,svpt);
			if (center_check < UM_DFUZZ || dist < UM_DFUZZ)
			{
				tpt[0] = svpt[0] + 1; tpt[1] = svpt[1] + 1; tpt[2] = svpt[2];
				um_nptpln(tpt,&sol->sdata[0],tvec,ipt1);
			}
			um_vcmnvc(ipt1,svpt,vec);
			um_unitvc(vec,vec);
/*
.....Use near point to determine which half of the torus the projection
.....should be on.
*/
			if (nrptfl)
			{
				um_nptpln(npt,&sol->sdata[0],tvec,ipt2);
				um_vcmnvc(ipt2,svpt,tvec1);
				um_unitvc(tvec1,tvec1);
				d1 = um_dot(vec,tvec1);
				if (d1 < 0.) um_vctmsc(vec,-1.,vec);
			}
			um_translate_point(&sol->sdata[0],rad1,vec,ipt2);
			um_vcmnvc(pt,ipt2,lvec);
			um_unitvc(lvec,lvec);
			um_translate_point(ipt2,rad2,lvec,pto);
			um_vctovc(lvec,&pto[3]);
			if (nrptfl)
			{
				um_negvc(lvec,lvec);
				um_translate_point(ipt2,rad2,lvec,ipt3);
				dist_min = um_dcccc(pto,npt);
				dist = um_dcccc(ipt3,npt);
				if (dist < dist_min) 
				{
					um_vctovc(ipt3,pto);
					um_vctovc(lvec,&pto[3]);
				}
			}
		}
	}
/*
.....End of routine
*/
	return (status);
}
