/*********************************************************************
**    NAME         :  necvoffs.c
**    CONTAINS: routines used to offset curves
**     ncl_offset_lr (points,pto,nvec,told,lr)
**	    ncl_offset_perp2d()
**	    ncl_planar_curve()
**	    ncl_next_offset_norm()
**	    ncl_next_offset_vec();
**	    ncl_offset_uv_point()
**	    ncl_convert_uvpoints
**     ncl_out_dm (points,tangs,dm,lr)
**     ncl_1st_offset (pp,vv1,lr,vmod,vnorm,vvq,up)
**     ncl_cor_offset (i0,n0,cvpoint,cvtang,offs,vv1,vv2,vvq,tol)
**     ncl_offset_contour (tol,npts,dm,dis,cvpoint)
**     ncl_cv_offset()
**     ncl_tool_offset (tol,np,dis,vta,pm,cvpoint,cvtang)
**	    ncl_dm_to_uvdm()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       necvoffs.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:29
*********************************************************************/

#include "nccs.h"
#include "modef.h"
#include "mdcoord.h"
#include "mdeval.h"
#include "nclfc.h"
#include "ncl.h"
#include "mattrddl.h"
#include "mdrel.h"

#define CHAMFER 1
#define ARC 2

#define CSQ0 (UU_REAL) 0.99975328 /* cos^2 of 0.9 degrees */

typedef struct
{
	UU_LOGICAL l2d;
	UU_LOGICAL lshuf;
	int shark;
	UU_REAL cos0;
	UU_REAL rad0;
	UU_REAL dis;
} ncl_offset;

/*********************************************************************
**    E_FUNCTION     : int ncl_offset_lr (points,pto,nvec,told,lr)
**       For a contour, determine the offset direction (in- or out-side),
**       which would move it closer to a given point. Made for PROFIL.
**    PARAMETERS
**       INPUT  : 
**                points     - the original contour
**                pto        - point in the offset direction
**                nvec       - the UP direction
**                told       - tolerance parameter
**       OUTPUT :
**                lr         - +1 if the first segment is offset left,
**                             -1 if right
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_offset_lr (points,pto,nvec,told,lr)
UU_LIST *points;
UM_coord pto;
UM_vector nvec;
UU_REAL told;
int *lr;
{
	int i,n1,iret,click;
	UM_coord *pp,pti;
	UM_vector vc,vrgt,vci;
	UU_REAL d,di,ddir,dmin,tol2;

	n1 = points->cur_cnt;
	tol2 = told*told;
	pp = (UM_coord *) UU_LIST_ARRAY(points);
	*lr = 0;
/*
..... find the segment closest to the near point
*/
	dmin = 1.e10;
	for (i = 0; i < n1-1; i++)
	{
		um_vcmnvc(pp[i+1],pp[i],vc);
		d = UM_MAG(vc);
		if (d < told) continue;
		vc[0] /= d; vc[1] /= d; vc[2] = 0; 
		iret = um_nptsg1 (pto,pp[i],vc,d,pti,&di);
/*
..... if the point is ON the curve, cannot determine so exit
*/
		if (di < tol2) return (-1);
		
		if (di < dmin)
		{
			um_vcmnvc (pto,pti,vci);
			um_cross (nvec,vc,vrgt);
			d = UM_DOT (vrgt,vci);
/*
..... can't determine the direction if the point is along the segment
*/
			if (fabs(d) < tol2) continue;
			click = 1;
			dmin = di;
			ddir = d;
		}
	}

	if (!click) return (-1);

	if (ddir > 0)
		*lr = 1;
	else
		*lr = -1;

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_offset_perp2d(tangs,nvec)
**       For a contour, determine the offset direction if perpendicula
**		to contour plane, and if it is true, the contour is a 2d contour
**    PARAMETERS
**       INPUT  : 
**                tangs     - the original contour tangents
**                nvec      - the offset direction
**    RETURNS      :
**         UU_TRUE if offset direction perpendicular to plane ; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_offset_perp2d(tangs,npts,nvec)
UU_LIST *tangs;
int npts;
UM_vector nvec;
{
	UU_LOGICAL lperp;
	UM_vector *vv;
	int ipts;
	UU_REAL co;

	lperp = UU_TRUE;
	vv = (UM_vector *) UU_LIST_ARRAY(tangs);
	for (ipts = 0; ipts < npts;ipts++)
	{
		co = UM_DOT (vv[ipts], nvec);
		if (fabs (co) > UM_FUZZ)
			return UU_FALSE;
	}

	return lperp;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_planar_contour(points,npts,tol)
**       For a contour, determine if the contour is a plannar or not
**    within given tolerance
**    PARAMETERS
**       INPUT  : 
**			points		- the original contour points
**			npts		- the number of points
**    RETURNS      :
**         UU_TRUE if teh contour is plannar; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_planar_curve(pts,npts,tol)
UM_coord *pts;
int npts;
UU_REAL tol;
{
	int i,status;
	UU_REAL dis;
	UM_coord plpt;
	UM_vector plvc,vec;

	status = um_ptstopln (npts,pts,plpt,plvc,tol);
	if (status == UU_SUCCESS)
	{
		for (i = 0; i < npts; i++)
		{
			um_vcmnvc(pts[i],plpt,vec);
			dis= UM_DOT (vec,plvc);
			if (fabs(dis) > tol) 
				return UU_FALSE;
		}
	}

	return UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION     : ncl_next_offset_norm(points,npts,nvec,vnorm)
**       For a contour, determine the offset normal vector if no-trival found
**    PARAMETERS
**       INPUT  : 
**			points		- the original contour points
**			npts		- teh number of points
**			nvec		- the first point tangent
**       OUTPUT :	
**			vnorm		- the offset normal vector
**    RETURNS      :
**         UU_SUCCESS if found offset direction; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_next_offset_norm(points,npts,nvec,vnorm)
UM_coord *points;
int npts;
UM_vector nvec,vnorm;
{
	int ipts;	
	UM_vector vvp;
	for (ipts = 1; ipts < npts-1; ipts++)
	{
		um_vcmnvc (points[ipts+1], points[ipts], vvp);
		um_unitvc (vvp, vvp);
		um_triple_cross (nvec,vvp, nvec, vnorm);
		if (UM_MAG (vnorm) > UM_FUZZ) 
			return UU_SUCCESS;
	}					

	return UU_FAILURE;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_next_offset_vec(vv,npts,nvec,vnorm)
**       For a contour, determine the offset normal vector if no-trival found
**    PARAMETERS
**       INPUT  : 
**			vv			- the original contour tangents
**			npts		- the number of points
**			nvec		- the offset direction modifer 
**       OUTPUT :	
**			vnorm		- the offset normal vector
**    RETURNS      :
**         UU_SUCCESS if found offset direction; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_next_offset_vec(vv,npts,nvec,vnorm)
UM_vector *vv;
int npts;
UM_vector nvec,vnorm;
{
	int ipts;
	for (ipts = 0; ipts < npts; ipts++)
	{
		um_triple_cross (vv[ipts],nvec,vv[ipts], vnorm);
		if (UM_MAG (vnorm) > UM_FUZZ) 
			return UU_SUCCESS;
	}

	return UU_FAILURE;
}

/*********************************************************************
** FUNCTION : int ncl_offset_uv_point (sfkey,uv, dis, vec, puv)
**       offset a uv-point along vec with dis on the surface
** PARAMETERS:
**    INPUT :
**		sfkey		-surface key
**		uv			-uv point
**		dis			-offset distance in xys space
**		vec			-offset vector
**    OUTPUT :
**		puv			-offseted uv point on surface
** RETURNS:  none
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int ncl_offset_uv_point (sfkey,uv,dis,vec,puv)
UU_KEY_ID sfkey;
UM_coord uv,puv;
UU_REAL dis;
UM_vector vec;
{	
	struct NCL_fixed_databag sf;	
	struct UM_evsrfout evsrf;
	UM_transf tfmat;

	UM_coord arcpts[5],sfpt,next_uv;
	UM_2Dcoord temp_uv;
	UM_coord uv0;
	UM_vector dist_vect, prj_pl_norm,sfnorm;
	int status;

	uv0[0] = uv[0];
	uv0[1] = uv[1];
	uv0[2] = uv[2];
	if (uv0[0] < 0.0) uv0[0] = 0.0;
	if (uv0[1] < 0.0) uv0[1] = 0.0;

	sf.key = sfkey;
	status = ncl_retrieve_data_fixed (&sf);

	if (status == UU_SUCCESS)
		status = uc_retrieve_transf (sf.key, tfmat);
/*
.....Get start point and norm
*/
	status = uc_init_evsrfout(&sf, &evsrf);		
	status = uc_evsrf(UM_NORM, uv0[0], uv0[1], &sf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_unitvc(evsrf.snorm, sfnorm);
	um_vctovc(evsrf.sp, sfpt);
/*
....Get next point and project plane normal
*/
	um_translate_point(uv0, 0.002, vec, next_uv);
	status = uc_evsrf(UM_NORM, next_uv[0], next_uv[1], &sf, tfmat, &evsrf);
	if (status != UM_VALID) return UU_FAILURE;
	um_vcmnvc(evsrf.sp, sfpt, dist_vect);
	um_unitvc(dist_vect, dist_vect);

	um_cross(sfnorm, dist_vect, prj_pl_norm);

/*
.....Calcuate the end uv along surface
*/
	temp_uv[0] = uv[0];
	temp_uv[1] = uv[1];
	um_vctovc(dist_vect,   arcpts[0]);
	um_vctovc(prj_pl_norm, arcpts[1]);
	um_vctovc(sfpt,       arcpts[2]);

	status = ncl_go_along_surf_arclen(&sf, arcpts, temp_uv, dis);
	if(status != UU_SUCCESS)
		return UU_FAILURE;
/*
.....Check if the offset point outside the extension
*/	
	if (fabs(temp_uv[0]-uv0[0]) < 0.1 * UM_FUZZ && 
		fabs(temp_uv[1]-uv0[1]) < 0.1 * UM_FUZZ)
		return UU_FAILURE;

	if (((1.0-temp_uv[0] < UM_FUZZ || temp_uv[0] < UM_FUZZ) &&
		 fabs(temp_uv[0]-uv0[0]) > UM_FUZZ) || 
		((1.0-temp_uv[1] < UM_FUZZ || temp_uv[1] < UM_FUZZ) &&
		 fabs(temp_uv[1]-uv0[1]) > UM_FUZZ))
		return UU_FAILURE;

	puv[0] = temp_uv[0];
	puv[1] = temp_uv[1];
 
	return (status);
}

/*********************************************************************
** FUNCTION : int ncl_convert_uvpoints (sfkey,uvpoints,cvpoints,cvtangs)
**			project uv-points on surface, get the cvpoints and cvtangs
** PARAMETERS:
**    INPUT :
**		sfkey		-surface key
**		uvpoints	-uv points
**		uvtangs		-uv tangents
**    OUTPUT :
**		cvpoints	-projected cv-points on surface
**		cvtangs		-projected cv-tangents on surface
** RETURNS:  none
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int ncl_convert_uvpoints(sfkey,uvpoints,uvtangs,cvpoints,cvtangs)
UU_KEY_ID sfkey;
UU_LIST *uvpoints,*uvtangs,*cvpoints,*cvtangs;
{
	struct NCL_fixed_databag sf;
	UM_transf tfmat;

	UM_int2 type = UM_POINT;
	struct UM_evsrfout evsrf;
	UM_coord *uvs,uv_next,sfpt;
	UM_vector *uvts,vtang;
	int i, npts,status;
	UU_REAL dd;

	sf.key = sfkey;
	status = ncl_retrieve_data_fixed (&sf);

	if (status == UU_SUCCESS)
		status = uc_retrieve_transf (sf.key, tfmat);

	status = uc_init_evsrfout(&sf, &evsrf);

	npts = UU_LIST_LENGTH(uvpoints);
	uvs = (UM_coord*) UU_LIST_ARRAY(uvpoints);
	uvts = (UM_vector*) UU_LIST_ARRAY(uvtangs);
	
	for (i = 0; i < npts; i++)
	{
		status = uc_evsrf(UM_POINT, uvs[i][0], uvs[i][1], &sf, tfmat, &evsrf);
		uu_list_push(cvpoints,evsrf.sp);	
		um_vctovc(evsrf.sp, sfpt);

/*
....Get next point and tangent vector
*/
		dd = UM_MAG (uvts[i]);

		um_translate_point(uvs[i], 0.002, uvts[i], uv_next);
		status = uc_evsrf(UM_POINT, uv_next[0], uv_next[1], &sf, tfmat, &evsrf);
		if (status != UM_VALID) return UU_FAILURE;

		um_vcmnvc(evsrf.sp, sfpt, vtang);
		um_unitvc(vtang, vtang);
		uu_list_push(cvtangs, vtang);	
	}
	
	return status;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_out_dm (points,tangs,dm,lr)
**       For a 2D-contour, determine the offset direction which would
**       make it larger.
**    PARAMETERS
**       INPUT  : 
**                points     - the original contour
**                tangs      - the tangent vectors at contour points
**       OUTPUT :
**                dm         - direction modifier for the first point
**                lr         - +1 if the first segment is offset left,
**                             -1 if right
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_out_dm (points,tangs,dm,lr)
UU_LIST *points,*tangs;
UM_vector dm;
int *lr;
{
	int i,n1,click,nn,nxmin,next,prev;
	UM_coord *pp;
	UU_REAL xmin,yxmin,xi,yi;

	pp = (UM_coord *) UU_LIST_ARRAY(points);
	n1 = points->cur_cnt - 1;
	*lr = 0;
/*
..... find the leftmost point (uppermost among these)
*/
	xmin = pp[0][0];
	yxmin = pp[0][1];
	nxmin = 0;
	for (i = 1; i < n1; i++)
	{
		xi = pp[i][0]; yi = pp[i][1];
		if ((xi<xmin) || (xi==xmin && yi>yxmin))
		{
			nxmin = i;
			xmin = xi; yxmin = yi;
		}
	}
/*
..... find an "ear", i.e., a triangle made by two adjacent boundary segments, the
..... third segment of which is inside the polygon (the polygon may not be convex)
*/
   click = 0;
   for (i = 0, next = nxmin + 1; i < n1; i++, next++)
   {
      next = next % n1;
      if ((pp[next][0]-xmin) + fabs(pp[next][1]-yxmin) > 0.)
      {
         click = 1;
         break;
      }
   }
   if (!click) return (-1);

   nn = (nxmin - next - 1 + n1) % n1;
   click = 0;
   for (prev = nxmin - 1, i = 0; i < nn; prev--, i++)
   {
      prev = (prev + n1) % n1;
      if ((pp[prev][0]-xmin) + fabs(pp[prev][1]-yxmin) > 0.)
      {
         click = 1;
         break;
      }
   }

   if (!click) return (-1);
/*
..... find the offset direction for the segment (nxmin,next), which would 
..... expand the angle (prev,nxmin,next)
*/
	dm[2] = 0.;
	xi = yxmin - pp[next][1];
	yi = pp[next][0] - xmin;
	if (xi*(xmin - pp[prev][0]) + yi*(yxmin - pp[prev][1]) < 0.) click = -1;
	*lr = click;

	if (tangs)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(tangs);
		xi = pp[0][0]; yi = pp[0][1];
	}
	else
	{
		xi = pp[1][0] - pp[0][0]; yi = pp[1][1] - pp[0][1];
	}
	dm[0] = -click*yi; dm[1] = click*xi;

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_contour_area (pp,npts,vz,area)
**    Determine the area of a 2D closed contour (re given vertical direction).
**    PARAMETERS
**       INPUT  :
**    vz         up direction
**    pp         contour points
**    npts       number of points
**       OUTPUT :
**    area 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_contour_area (pp,npts,vz,area)
UM_coord *pp;
int npts;
UM_vector vz;
UU_REAL *area;
{
	int i;
	UU_REAL ar;
	UM_coord v0,v1;
	UM_2Dcoord p0,p1,p2;

	um_perpvc(vz,v0);
	um_unitvc(v0,v0);
	um_cross (vz,v0,v1);
	um_unitvc(v1,v1);
	
	p0[0] = UM_DOT (pp[0],v0);
	p0[1] = UM_DOT (pp[0],v1);
	p1[0] = UM_DOT (pp[1],v0);
	p1[1] = UM_DOT (pp[1],v1);
	for (i = 2, ar = 0.; i < npts; i++)
	{
		p2[0] = UM_DOT (pp[i],v0);
		p2[1] = UM_DOT (pp[i],v1);

		ar += um_triangle_signed_area(p0,p1,p2);
		p1[0] = p2[0]; p1[1] = p2[1];
	}
	*area = ar;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_1st_offset (pp,vv1,lr,vmod,vnorm,vvq,up)
**    Determine the curve plane and offset direction for the first curve point
**    PARAMETERS
**       INPUT  :
**    tol	     tolerance
**    lr         left/right direction modifier
**    vmod       3D direction modifier
**    pp         curve evolved pts
**    vv1        first tangent vector 
**       OUTPUT :
**    vvq        curve plane normal at first point
**    vnorm      offset vector at first point
**    up         1 iff offsetting upward, 0 else
**
**    RETURNS      : 0 iff success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_1st_offset (sfkey,pp,vv,npts,ddp,tol,vv1,lr,lperp2d,vmod,vnorm,vvq,up)
UU_KEY_ID sfkey;
UM_coord *pp;
UM_vector *vv;
UM_vector vv1,vmod,vvq,vnorm;
UU_REAL ddp,tol;
UU_LOGICAL lperp2d;
int lr,npts,*up;
{
	int i;
	UM_vector vvp;
	UU_REAL dd,co,co1;
	UU_LOGICAL lplanar,reset_up=UU_FALSE;
	int status = UU_FAILURE;
	lplanar = UU_FALSE;
/*
.....check if the curve is planar or nor
*/
	lplanar = ncl_planar_curve(pp,npts,tol);
/*
.....If up = 1, then vnorm was provided and should not change unless
.....necessary - Andrew 12/20/12
*/
	if (*up == 1) 
	{
		reset_up = UU_TRUE;
		*up = 0;
	}
	if (lr == 0)
	{
		ddp = sqrt(ddp);
		for (i=0; i<3; i++)
			vvp[i] = (pp[1][i] - pp[0][i])/ddp;
/*
..... vnorm is a curve normal in the curve segment plane
*/
		if (!reset_up) um_triple_cross (vv1,vvp,vv1, vnorm);
/*
..... the triple cross defines vnorm as the vector perpendicular to vv1
..... in the (vv1,vvp)-plane
*/
		dd = UM_MAG (vnorm);
/* 
..... if the (vv1,vvp)-plane is not well-defined (0.0157 is sin(0.9))
*/
		if (dd < 0.0157 && (ddp < 10.*tol || lplanar))
		{
/*
..... try with the next segment if trival normal found
*/
			status  = ncl_next_offset_norm(pp,npts,vv1,vnorm);
			if (status == UU_SUCCESS)			
				dd = UM_MAG (vnorm);
		}

		if (dd < 0.0157)
		{
			um_triple_cross (vv1,vmod,vv1, vnorm);
/*
..... the triple cross defines vnorm as the vector perpendicular to vv1
..... in the (vv1,vmod)-plane
*/
			if (UM_MAG (vnorm) < UM_FUZZ) 
			{
/*
.....get next vnrom as the vector perpendicular to vv1
..... in the (vv1,vmod)-plane
*/
				status = ncl_next_offset_vec(vv,npts,vmod,vnorm);
				if (status != UU_SUCCESS)
					return (-1);
			}
			um_unitvc (vnorm, vnorm);

			co = UM_DOT (vnorm, vmod);
			if (fabs (co) < UM_DFUZZ)
				*up = 1;
			else
			{			
				co1 = UM_DOT (vv1, vmod);
				if (lperp2d)
				{
					um_vctovc (vmod,vvq);
					if (co < 0.)
						um_vctmsc (vvq, -1., vvq);
				}
				else
				{
					um_cross (vv1, vnorm, vvq);	
/*
.....Check the start point vector and norm again
*/
					co1 = UM_DOT (vv1, vnorm);
					if (fabs(co1-1.0) < UM_FUZZ || fabs(co1+1.0) < UM_FUZZ)
					{
						if (UM_MAG (vvq) < UM_DFUZZ)
						{
							if (vmod[0] >= 0.0 && vmod[1] >= 0)						
								vvq[2] = 1;
							else
								vvq[2] = -1;
						}

						um_cross (vvq, vv1, vnorm);	
						um_unitvc (vnorm, vnorm);
					}
				}
			}
		}
		else /* there is a well-defined normal at start */
		{
			um_unitvc (vnorm, vnorm);
			co = UM_DOT (vnorm, vmod);
			if (fabs (co) < UM_DFUZZ && !reset_up)
			{
				um_triple_cross (vv1,vmod,vv1, vnorm);
				if (UM_MAG (vnorm) < UM_FUZZ) 
				{
					status = ncl_next_offset_vec(vv,npts,vmod,vnorm);
					if (status != UU_SUCCESS)	
						return (-1);
				}
				um_unitvc (vnorm, vnorm);

				co = UM_DOT (vnorm, vmod);
				if (fabs (co) < UM_DFUZZ)
					*up = 1;
				else
				{			
					co1 = UM_DOT (vv1, vmod);
					if (lperp2d)
					{
						um_vctovc (vmod,vvq);
						if (co < 0.)
							um_vctmsc (vvq, -1., vvq);
					}
					else
					{
						um_cross (vv1, vnorm, vvq);	
/*
.....Check the start point vector and norm again
*/
						co1 = UM_DOT (vv1, vnorm);
						if (fabs(co1-1.) < UM_FUZZ || fabs(co1+1.) < UM_FUZZ)
						{
							if (UM_MAG (vvq) < UM_DFUZZ)
							{
								if (vmod[0] >= 0.0 && vmod[1] >= 0)						
									vvq[2] = 1;
								else
									vvq[2] = -1;
							}

							um_cross (vvq, vv1, vnorm);	
							um_unitvc (vnorm, vnorm);
						}
					}
				}
			}
			else
			{
				if (co < 0. && !reset_up)
					um_vctmsc (vnorm, -1., vnorm);
				um_cross (vv1, vnorm, vvq);
			}
		}
	}
	else
	{
		if (!reset_up)
		{
		 um_vctovc (vmod,vvq);
/*
..... vnorm is a curve normal in the curve segment plane
*/
			if (lr < 0) um_vctmsc (vvq, -1., vvq);
			um_cross (vvq, vv1, vnorm);
			um_unitvc (vnorm,vnorm);
		}
	}

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_cor_offset (i0,n0,cvpoint,cvtang,offs,
**                                                   vv1,vv2,vvq,tol)
**    Offset a corner (points and vectors).
**    PARAMETERS
**       INPUT  :
**    tol	     tolerance
**    i0         current point
**    n0         current number of points
**    cvpoint    list of curve points
**    cvtang     list of tangent vectors 
**    offs       offset modals
**    vv1, vv2   tangent vectors defining the corner
**    vvq        current offset plane
**       OUTPUT :
**    i0         current point
**    n0         current number of points
**    cvpoint    list of curve points
**    cvtang     list of tangent vectors 
**    vvq        corner plane normal
**
**    RETURNS      : 1 if success, 0 else (then the calling routine will offset
**                   as if not a corner)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_cor_offset (sfkey,i0,n0,cvpoint,cvtang,offs,vv1,vv2,vvq,pshuf,tol)
UU_KEY_ID sfkey;
UU_REAL tol;
ncl_offset *offs;
UM_vector vv1,vv2,vvq;
UM_coord pshuf;
UU_LIST *cvpoint,*cvtang;
int *i0,*n0;
{
	int ipts,npts,itype,nint,i,status;
	UU_REAL calp,dis,alp,bet,dd,rad,pm;
	UM_coord *pp;
	UM_vector *vv;
	UM_coord qt1,qt2,pint,qti;
	UM_vector vvp,vnorm,vn1,vn2,vc1,vc2,vcq;
	UM_vector VNUL;
	UU_LOGICAL lrad,l2d;
	UM_transf rottf;
	UM_int2 idx = 169;
	UM_real8 ver;
	UU_LOGICAL lv94,lshuf;

	char tbuf[80];
	UM_coord tmp;
	UM_2Dcoord p1,p2,q1,q2;

	ipts = *i0;
	npts = *n0;
	um_nullvc (VNUL);

	getsc(&idx, &ver);
	lv94 = (ver < 9.449);
	l2d = offs->l2d;
	um_vctovc (vvq,vcq);
	um_vctovc (vv1,vc1);
	um_vctovc (vv2,vc2);

	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
	dis = offs->dis;
	itype = offs->shark;
	lshuf = offs->lshuf;

	pm = (dis > 0)? 1: -1;

	if (lv94)
		um_vctovc (pp[ipts+1],pint);
	else
	{
		um_ilnln(pp[ipts],vv1,pp[ipts+1],vv2,&nint,pint);
		if (nint == 0) return (0);
	}

	um_cross (vv1,vv2,vcq);
	if (UM_DOT(vcq,vvq) < 0.) um_vctmsc (vcq,-1.,vcq);
/*
..... vcq is the corner plane normal.
*/
	um_unitvc (vcq,vcq);

	if (l2d)
	{
		um_vctovc (vvq,vcq);
	}

	um_uvctopln (vv1,vcq,vv1);
	um_uvctopln (vv2,vcq,vv2);
	calp = UM_DOT (vv1,vv2);

	um_cross (vcq, vv1, vnorm);
	um_vctovc(pp[ipts],tmp);
	
	if (sfkey > NULLKEY)
	{
		dis *= UM_MAG (vnorm);
		status = ncl_offset_uv_point(sfkey,pp[ipts],dis,vnorm,pp[ipts]);
		if (status != UU_SUCCESS) return -1;

		p1[0] = tmp[0];
		p1[1] = tmp[1];
		p2[0] = pp[ipts][0];
		p2[1] = pp[ipts][1];
	}
	else
		um_translate_point (pp[ipts],dis,vnorm, pp[ipts]);

	if (lshuf)
	{
/*
..... calculate the diagonal corner point offset
*/
		dd = sqrt(1 - calp*calp);
		dd = 1./dd;
		rad = pm*dd*dis;
		um_vcmnvc (vv1,vv2,vvp);
		dd = UM_DOT (vnorm,vv2);
		if (pm*dd > 0.) rad = -rad;

		um_translate_point (pp[ipts+1],rad,vvp, pshuf);
	}

/*
..... if we offset outside the corner, offset the old apex point to the
..... new calculated apex point
*/
	dd = UM_DOT (vnorm,vv2);
	if (pm*dd < 0.)
	{
		um_vctovc (pint,pp[ipts+1]); /* just in case - the points should be same */
		if (!lshuf && (itype == ARC || itype == CHAMFER) && calp <= offs->cos0)
		{
			um_vctovc (vv1,vv[ipts]);
			um_vctovc (vv2,vv[ipts+1]);

			if (itype == CHAMFER)
			{
				dd = sqrt((calp+1)/2) + 1;
				dd = 1./dd;

				um_vcmnvc (vv1,vv2,vvp);
				um_unitvc (vvp,vvp);
				um_vctmsc (vvp,pm,vvp);
				um_vcplvc (vnorm,vvp,vn1);
				um_vctmsc (vn1,dd,vn1);
				um_translate_point (pint,dis,vn1, qt1);
				um_cross (vcq, vv2, vnorm);
				um_vcplvc (vnorm,vvp,vn2);
				um_vctmsc (vn2,dd,vn2);
				um_translate_point (pint,dis,vn2, qt2);

				um_vcmnvc (qt2,qt1,vvp);

				ipts++; npts++;
				uu_list_insert (cvpoint, ipts, qt1);
				uu_list_insert (cvtang, ipts, vvp);
				ipts++; npts++;
				uu_list_insert (cvpoint, ipts, qt2);
				uu_list_insert (cvtang, ipts, vv2);
			}
			else
			{
				rad = offs->rad0;

				lrad = (rad > tol && rad < dis-tol);
				if (!lrad) rad = dis;

				if (lrad)
				{
					um_vcmnvc (vv1,vv2,vvp);
					um_unitvc (vvp,vvp);
					dd = sqrt(2/(calp+1));
					dd = (dis - rad)*dd;
					um_translate_point (pint,dd,vvp, pint);
				}
				um_vctovc (vnorm,vn1);
				um_translate_point (pint,rad,vn1, qt1);

				um_cross (vcq, vv2, vnorm); /* this in corner plane */
				um_vctovc (vnorm,vn2);
				um_translate_point (pint,rad,vn2, qt2);

				if (lrad)
				{
					ipts++; npts++;
					uu_list_insert (cvpoint, ipts, qt1);
					uu_list_insert (cvtang, ipts, VNUL);
				}

				alp = acos (calp);
				bet = 2. * acos (1. - 0.9 * tol / rad);
				if (rad*bet < UM_FUZZ) bet = UM_FUZZ/rad;
				nint = ceil (alp / bet);

				if (nint > 1)
				{
					bet = alp/nint;
																		
					um_cross (vv1,vv2,vvp);
					if (UM_DOT(vvp,vcq) < 0.)
						um_vctmsc (vcq,-1.,vvp);
					else
						um_vctovc (vcq,vvp);

					um_rotlntf (pint, vvp, bet, rottf);
					um_vctovc (qt1,qti);
					for (i = 1; i < nint; i++)
					{
						um_cctmtf(qti,rottf,qti);
						ipts++; npts++;
						uu_list_insert (cvpoint, ipts, qti);
						uu_list_insert (cvtang, ipts, VNUL);
					}
				}
				if (lrad)
				{
					ipts++; npts++;
					uu_list_insert (cvpoint, ipts, qt2);
					uu_list_insert (cvtang, ipts, VNUL);
				}
			}
			pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
			vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		}							
		else
		{
			dd = sqrt(1 - calp*calp);
			dd = 1./dd;

			um_vcmnvc (vv1,vv2,vvp);
			um_vctmsc (vvp,pm*dd,vnorm);
		}
	}
	else
	{
		/* inside corner */
		um_cross (vcq, vv2, vnorm);
	}
	ipts++;
	um_vctovc(pp[ipts],tmp);		
	if (sfkey > 0)
	{		
		dis *= UM_MAG (vnorm);
		status = ncl_offset_uv_point(sfkey,pp[ipts],dis,vnorm,pp[ipts]);
		if (status != UU_SUCCESS) return -1;

		q1[0] = tmp[0];
		q1[1] = tmp[1];
		q2[0] = pp[ipts][0];
		q2[1] = pp[ipts][1];
	}
	else
		um_translate_point (pp[ipts],dis,vnorm, pp[ipts]);

	if (!l2d) um_vctovc (vcq,vvq);
	*i0 = ipts;
	*n0 = npts;
	return (1);
}

/*********************************************************************
**    FUNCTION     : int S_nearby_corner(ipts,npts,cvtang)
**      Check if the index point is near sharp corner or not
**    PARAMETERS
**       INPUT  :
**			ipts		- the index of point to check.
**          npts		- the number of points
**			cvtang		- list of the point tangent
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_TRUE iff no sharp found; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_nearby_corner(ipts,npts,cvtang)
int ipts,npts;
UU_LIST *cvtang;
{
	int i,ipts0;
	UU_REAL dd;	
	UM_vector *vv;
	UU_LOGICAL lnearcor;
	lnearcor = UU_FALSE;
	ipts0 = ipts;

	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

	for (i = ipts0; i <= ipts0 + 5; i++)
	{		
		if (i == npts) break;
		dd = UM_DOT (vv[i],vv[i]);
		if (dd > 9.e9)
			return UU_TRUE;
	}

	for (i = ipts0; i >= ipts0 - 5; i--)
	{	
		if (i == 0)
			break;
		dd = UM_DOT (vv[i],vv[i]);
		if (dd > 9.e9)
			return UU_TRUE;
	}

	return lnearcor;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_cv_offset(sfkey,cvpoint,cvtang,npts,lr,
**                        vmod,dis0,ishark,alp0,rad0,tol,normfl,inorm)
**    Offsets the evolved curve points for subsequent curve fitting;
**    then removes loops, fixes sharp corners and does some 
**    "reasonableness" checks 
**    PARAMETERS
**       INPUT  :
**	         sfkey   - surface key for uv-points offset
**          tol     - tolerance
**          npts    - number of evolved pts
**          lr      - left/right direction modifier
**          vmod    - 3D direction modifier
**          dis     - offset distance
**          cvpoint - list of evolved pts
**          cvtang  - list of tangent vectors 
**          itsk    - 1 for NCL curve, 2 for spline
**          normfl  - UU_TRUE : use inorm in first offset routine
**                    UU_FALSE: calculate first offset vector
**          inorm   - input first offset vector
**       OUTPUT :
**          cvpoint    fixed list of evolved pts
**          cvtang     fixed list of tangent vectors 
**    RETURNS      : new number of pts
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cv_offset (sfkey,cvpoint,cvtang,npts,lr,vmod,dis0,ishark,alp0,
	                rad0,tol,normfl,inorm)
UU_KEY_ID sfkey;
UU_REAL dis0,alp0,rad0,tol;
UM_vector vmod,inorm;
UU_LIST *cvpoint,*cvtang;
int npts,ishark,lr,normfl;
{
	int ipts,n1,nint,i,j,k;
	UU_REAL dd,ddp,calp,calp0,co;
	UU_REAL area0,area;

	UM_coord pt1,pt2,qt1,qt2,pint,pshuf;
	UM_vector vvp,vvq,vv1,vv2,vnorm,nvec;
	UU_REAL *ZVEC = UU_NULL;
	UM_coord *pp;
	UM_vector *vv;
	UU_LOGICAL lclone = UU_FALSE;
	int lshuf = 0;
	UU_LOGICAL lexten = UU_FALSE;
	UU_LOGICAL lrev = UU_FALSE;
	UU_LOGICAL l2d,lperp2d,lcor;
	int status = UU_SUCCESS;
	int closed = 0, UP = 0, ifl = 0, ncorner = 0;

	UU_REAL tolsq = tol*tol;
	UU_REAL fact = dis0/(30.*tol);
	UU_REAL dis = dis0;
	char tbuf[80];
	UM_coord tmp,mid_vnorm,prev_vnorm;
	UM_2Dcoord p1,p2,q1,q2;

	ncl_offset offs;

	UU_LOGICAL lnorm = UU_FALSE,lperp = UU_FALSE;
	char sbuf[80];

	if (normfl != 0) lnorm = UU_TRUE;
	if (normfl == 1) lperp = UU_TRUE;

	if (ishark == ARC || ishark == CHAMFER) calp0 = cos(alp0/UM_RADIAN);
/*
..... save first and last points as pt1,pt2 to check for bad input 
*/
	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	um_vctovc (pp[0], pt1);
	um_vctovc (pp[npts-1], pt2);
/*
.....Switched to 3D distance - Andrew 3/14/13
	if (UM_SQDIS (pt1,pt2) < tolsq)
*/
	if (um_dcccc(pt1,pt2) < tol)
	{
		closed = 1;
		um_vctovc (pt1,pt2);
		um_vctovc (pt1,pp[npts-1]);
	}

	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
/*
.....Check if the offset direction is perpendicular to vmod
.....and if the the offset curve is on 2d plane.
*/
	lperp2d = lperp;
	if (abs(lr) == 0 && !lnorm)
		lperp2d = ncl_offset_perp2d(cvtang,npts,vmod);

	for (i = 0; i < npts; i++)
	{
		dd = UM_DOT (vv[i],vv[i]);
		if (dd > 9.e9)
		{
			for (k = 0; k < 3; k++)	vv[i][k] /= 100000;
			if (i > 0)
				j = i-1;
			else if (closed == 1)
				j = npts-2;
			else
				continue;

			ddp = UM_SQDIS (pp[i],pp[j]);
			if (ddp > 62500*tolsq)
			{
				um_vcmnvc (pp[i],pp[j],vvp);
				um_vctovc (vvp,vv[j]);
			}
		}
	}
		
	l2d = (abs(lr) == 1);

	if (fabs(dis) < tol)
	{
		dis = 0;
		lclone = UU_TRUE;
	}

	if (closed == 1 && l2d)
	{
		ncl_contour_area (pp,npts,vmod,&area0);

		if (!lclone)
			lshuf = ncl_cvofs_shuffle (cvpoint,cvtang,tol,&npts);
	}
/*
..... reverse an open curve if starts with a wiggle
*/
	if (closed == 0 && !lclone && npts > 3)
		lrev = ncl_cvofs_rev (cvpoint,cvtang,tol,npts);

	if (sfkey > NULLKEY)
		ncl_fix_corners (cvpoint,cvtang,0.2*tol,3,&npts);
	else
		ncl_fix_corners (cvpoint,cvtang,0.2*tol,2,&npts);

	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

	if (lshuf == 1)
	{
/*
..... find the index of original starting point
*/
		ddp = 25.*tolsq;
		j = -1;
		for (ipts = 0; ipts < npts; ipts++)
		{
			dd = UM_DOT (vv[ipts],vv[ipts]);
			if (dd > 9.e9)
			{
				dd = UM_DOT (vv[ipts+1],vv[ipts+1]);
				if (dd > 9.e9)
				{
					dd = UM_SQDIS (pp[ipts+1],pt1);
					if (dd < ddp)
					{
						j = ipts+1;
						ddp = dd;
						if (ddp < tolsq) break;
					}
				}
			}
		}

		if (j < 0)
			lshuf = 0;
		else
		{
			lshuf = 2;
			um_vctovc (pp[j],pshuf);
		}
	}

/*
..... extend open curve endpoints, then the first and last offset points will
..... be removed
*/
	if (closed == 0 && !lclone)
	{
		dd = UM_MAG (vv[npts-1]);
		for (k = 0; k < 3; k++)
		{
			vv2[k] = vv[npts-1][k];
			pt2[k] = pp[npts-1][k] + 10.*fabs(dis)*vv2[k]/dd;
		}
		uu_list_push (cvpoint, pt2);
		uu_list_push (cvtang, vv2);
			
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		npts++;

		lexten = UU_TRUE;
	}
/*
..... Offset evolved points along normals
*/
	offs.l2d = l2d;
	offs.lshuf = UU_FALSE;
	offs.shark = ishark;
	offs.dis = dis;
	offs.cos0 = calp0;
	if (ishark == ARC && rad0 > 0. && rad0 < dis)
		offs.rad0 = rad0;
	else
		offs.rad0 = 0.;

	um_nullvc (vvq);
	for (ipts = 0; ipts < npts; ipts++)
	{
		if (ipts < npts - 1)
			ddp = UM_SQDIS (pp[ipts+1], pp[ipts]);
		else
			ddp = UM_SQDIS (pp[ipts], pp[ipts-1]);
		um_vctovc (vv[ipts], vv1);
		dd = UM_MAG (vv1);
/*
.....Check offset direction perpendicular to vv1 or not
*/
		co = UM_DOT (vv1, vmod);

		if (ddp < 0.25*tolsq && dd < 99000. && !lperp2d)
		{
			uu_list_delete (cvpoint,ipts,1);
			uu_list_delete (cvtang,ipts,1);
			pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
			vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
			ipts--; npts--;
			continue;
		}
		if (lclone) continue;

		if (dd < UM_FUZZ) 
			return (-1);
		else
		{
			vv1[0] /= dd; vv1[1] /= dd; vv1[2] /= dd;
		}

		if (ipts == 0)
		{
/*
.....Use input norm vector as basis for vnorm - Andrew 3/14/13
*/
			if (lnorm)
			{
				UP = 1;
				um_vctovc(inorm,vnorm);
			}
 			status = ncl_1st_offset (sfkey,pp,vv,npts,ddp,tol,vv1,lr,lperp2d,
									vmod,vnorm,vvq,&UP);
			if (status != 0) return (-1);
			if (l2d)
			{
				um_vctovc (vvq,nvec);
				ZVEC = &nvec[0];
			}
		}
		else /* ipts > 0 */
		{
			if (UP == 1)
			{
				um_triple_cross (vv1,vmod,vv1, vnorm);
				if (UM_MAG (vnorm) < UM_FUZZ)
					return (-1);
				um_unitvc (vnorm, vnorm);
			}
			else
			{
				if (dd > 99000. && !lperp2d)
				{
					ncorner++;
					um_vctovc (vv[ipts+1], vv2);
					dd = UM_MAG (vv2);
/*
..... if we have a sharp corner, it defines its own curve plane and
..... its own offsets
*/
					if (dd > 99000.)
					{
						um_unitvc (vv2,vv2);
						calp = UM_DOT (vv1,vv2);

						if (calp < CSQ0)
						{
							if (lshuf == 2)
							{
								dd = UM_SQDIS (pshuf,pp[ipts+1]);
								if (dd < tolsq)
								{
									offs.lshuf = UU_TRUE;
									lshuf = 1;
								}
							}
							else
								offs.lshuf = UU_FALSE;
							nint = ncl_cor_offset (sfkey,&ipts,&npts,cvpoint,cvtang,
								&offs,vv1,vv2,vvq,pshuf,tol);

							if (nint == UU_FAILURE) 
								return (UU_FAILURE);
							if (nint > 0)
							{
								pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
								vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
								continue;
							}
						}
					}
				}
				if (!lperp2d)
				{
					um_cross (vvq, vv1, vnorm);
					um_unitvc (vnorm, vnorm);
					if (!l2d) um_cross (vv1, vnorm, vvq);
				}
				else
				{		
					um_vctovc (vvq,vnorm);
					um_unitvc (vnorm, vnorm);
				}
			}
		}

		um_vctovc(pp[ipts],tmp);
		if (sfkey > NULLKEY)
		{
			status = ncl_offset_uv_point(sfkey,pp[ipts],dis,vnorm,pp[ipts]);					
			if (status != UU_SUCCESS && !(closed == 0 && ipts == npts-1))
			{
				if (!S_nearby_corner(ipts,npts,cvtang))
					return (UU_FAILURE);
			}

			p1[0] = tmp[0];
			p1[1] = tmp[1];
			p2[0] = pp[ipts][0];
			p2[1] = pp[ipts][1];

			if (ipts > 0)		
				dd = UM_MAG (vv[ipts-1]);
			if (ipts > 0 && dd < 99000.)
			{
				for (j = 0; j < 10; j++)
				{
					if (!um_segments_isect(p1,p2,q1,q2))
						break;
								
					mid_vnorm[0] = 0.5 * (prev_vnorm[0] + vnorm[0]);
					mid_vnorm[1] = 0.5 * (prev_vnorm[1] + vnorm[1]);
					mid_vnorm[2] = 0.5 * (prev_vnorm[2] + vnorm[2]);
					status = ncl_offset_uv_point(sfkey,tmp,dis,mid_vnorm,pp[ipts]);

					um_vctovc_2d(q1,p1);
					um_vctovc_2d(q2,p2);
					q1[0] = tmp[0];
					q1[1] = tmp[1];
					q2[0] = pp[ipts][0];
					q2[1] = pp[ipts][1];
					um_vctovc (mid_vnorm, vnorm);
				}
			}
			q1[0] = tmp[0];
			q1[1] = tmp[1];
			q2[0] = pp[ipts][0];
			q2[1] = pp[ipts][1];
			um_vctovc (vnorm,prev_vnorm);
		}
		else
			um_translate_point (pp[ipts],dis,vnorm,pp[ipts]);
	}
/*
.....Output vnorm for composite curve offset - Andrew 3/14/13
*/
	if (lnorm) um_vctovc (vnorm,inorm);

	if (l2d)
	{
		ncl_cv_weed (cvpoint,cvtang,tol,lexten,&npts);
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
	}
/*
..... if the first and the last offset segments intersect, return error
..... if the original curve is not closed
*/
	if (closed == 0)
	{
		um_vctovc (pp[0], qt1);
		um_vctovc (pp[npts-1], qt2);
		um_isegseg (pt1,qt1,pt2,qt2,&nint,pint,5*tol);
		if (nint > 0)
			return (0);
	}
	else if (UP == 0)
	{
/*
..... for a closed curve - set up a flag showing the ends meet at a corner,
..... and we offset inside this corner
*/
			um_unitvc (vv[0],vv1);
			um_unitvc (vv[npts-1],vv2);
			if (UM_DOT(vv1,vv2) < 0.9999)
			{
				um_vcmnvc (pp[0],pt1,vv1);
				if (UM_DOT(vv1,vv2) < 0.)
					ifl = 1;
			}
			if (ifl == 0 && ncorner > 0) ifl = 2;
	}

	if (closed == 1 && l2d)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		ncl_contour_area (pp,npts,vmod,&area);

		if (fabs(area0) > 10000.*tolsq && fabs(area) > 10000.*tolsq &&
			area*area0 < 0)
			return (-1);
	}

/*
..... Remove loops
*/
	ncl_cv_deloop (cvpoint,cvtang,closed,ifl,ZVEC,fact,tol,tolsq,&npts);

	if (closed == 0 && !lclone)
	{
		npts--;
		cvpoint->cur_cnt--;
		cvtang->cur_cnt--;
	}
/*
..... Check for "folding"
*/
	ncl_cv_defold (cvpoint,cvtang,closed,tolsq,&npts);

	n1 = npts;

	if (closed == 1)
	{		
		ncl_fix_corner0 (cvpoint, cvtang, tol, &n1);
		if (n1 > npts)
			ncl_cv_deloop0 (cvpoint,cvtang,ZVEC,fact,tol,tolsq,&n1);
	}

	if (lshuf == 1)
		ncl_cvofs_reshuffle (cvpoint,cvtang,pshuf,tol,tolsq,n1);
	else if (lrev)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		ncl_revers1_list (n1,0,pp,1);
		ncl_revers1_list (n1,0,vv,2);
	}

	return (n1);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_offset_contour (tol,npts,lr,dis,cvpoint,cvtang)
**    Offsets the evolved curve points for subsequent curve fitting;
**    then removes loops, fixes sharp corners and does some 
**    "reasonableness" checks 
**    PARAMETERS
**       INPUT  :
**    tol	     tolerance
**    npts       number of evolved pts
**    dm         direction modifier vector (currently 2-D)
**    dis        offset distance
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors 
**    itsk       1 for NCL curve, 2 for spline
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors 
**
**    RETURNS      : new number of pts
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_offset_contour (cvpoint,cvtang,npts,lr,dis,tol,tolsq)
UU_REAL dis,tol,tolsq;
UU_LIST *cvpoint,*cvtang;
int npts,lr;
{
	int ipts,nint;
	UU_REAL co,co1,co2,dd,ddp,d1,d2;
	UM_coord pt0,pt1,pt2,qt1,qt2, *pp, pint;
	UM_vector vvp, vvq, vv1,vv2, *vv;
	UM_vector vnorm; 
	int jpts,k;
	int inclosed = 0;
	UU_LOGICAL lclosed;

/*
..... save first and last points as pt1,pt2 to check for bad input 
*/
	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);

	lclosed = (UM_SQDIS_2D (pp[0],pp[npts-1]) < tolsq);
	um_vctovc (pp[0], pt0);
	if (lclosed)
		um_vctovc (pt0,pp[npts-1]);

	ncl_fix_corners (cvpoint,cvtang,0.2*tol,2,&npts);

	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
/*
..... Offset evolved points along normals
*/
	for (k = 0; k < 3; k++)
		vv1[k] = vv2[k] = vnorm[k] = 0;

	if (!lclosed)
	{
		for (k = 0; k < 3; k++)
		{
			vv2[k] = vv[npts-1][k];
			pt2[k] = pp[npts-1][k] + 10.*dis*vv2[k];
		}
		uu_list_push (cvpoint, pt2);
		uu_list_push (cvtang, vv2);
			
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		npts++;
	}

	for (ipts = 0; ipts < npts; ipts++)
	{
		dd = UM_MAG_2D (vv[ipts]);
		if (dd < UM_FUZZ) return (-1);
		vv1[0] = vv[ipts][0]/dd; vv1[1] = vv[ipts][1]/dd;
		vnorm[0] = -lr*vv1[1]; vnorm[1] = lr*vv1[0];
		um_vcplbvc_2d (pp[ipts],dis,vnorm, pp[ipts]);
/*
..... sharp corner - special treatment
*/
		if (ipts > 0 && ipts < npts-1 && dd > 99000.)
		{
			dd = UM_MAG_2D (vv[ipts+1]);
			if (dd > 99000.)
			{
				vv2[0] = vv[ipts+1][0]/dd; vv2[1] = vv[ipts+1][1]/dd;
				co = fabs(vv1[0]*vv2[1] - vv1[1]*vv2[0]);
				if (co > 0.0157)
				{
/*
..... if we offset outside the corner, offset the old apex point to the
..... new calculated apex point
*/
					if (UM_DOT_2D (vnorm,vv2) < 0.)
					{
						vnorm[0] = (vv1[0] - vv2[0])/co;
						vnorm[1] = (vv1[1] - vv2[1])/co;
					}
					else
					{
						vnorm[0] = -lr*vv2[1]; vnorm[1] = lr*vv2[0];

					}
					ipts++;
					um_vcplbvc_2d (pp[ipts],dis,vnorm, pp[ipts]);
					continue;
				}
			}
		}
	}

	if (lclosed)
	{
		um_unitvc_2d (vv[0],vv1);
		um_unitvc_2d (vv[npts-1],vv2);
		if (UM_DOT_2D(vv1,vv2) < 0.9999)
		{
			um_vcmnvc_2d (pp[0],pt0,vv1);
			if (UM_DOT_2D(vv1,vv2) < 0.)
				inclosed = 1;
		}
	}
/*
..... in the case described above, cut the ends sticking out the largest 
..... loop, and keep the loop as the curve offset
*/
	for (ipts = 0; inclosed == 1 && ipts < npts-3; ipts++)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		um_vctovc_2d (pp[ipts], pt1);
		um_vctovc_2d (pp[ipts+1], pt2);

		for (jpts = npts - 2; jpts > ipts+1; jpts--)
		{
			um_vctovc_2d (pp[jpts], qt1);
			um_vctovc_2d (pp[jpts+1], qt2);
			um_isegseg2d (pt1,pt2,qt1,qt2,&nint,pint,tol);
			if (nint > 0)
			{
				inclosed = 0;
				if (UM_SQDIS_2D (pt2,pint) < tolsq)
					ipts++;
				um_vctovc_2d (pint,pp[ipts]);
				if (UM_SQDIS_2D (qt2,pint) < tolsq)
					jpts++;
				jpts++;
				um_vctovc_2d (pint,pp[jpts]);
				npts = jpts+1;
				cvpoint->cur_cnt = cvtang->cur_cnt = npts;
				if (ipts > 0)
				{
					uu_list_delete (cvpoint,0,ipts);
					uu_list_delete (cvtang,0,ipts);
					npts -= ipts;
				}
				break;
			}
		}
	}
	if (inclosed == 1) return (0);
/*
..... Remove loops
*/
	for (ipts = 0; ipts < npts-3; ipts++)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		um_vctovc (pp[ipts], pt1);
		um_vctovc (pp[ipts+1], pt2);

		for (jpts = npts - 2; jpts > ipts+1; jpts--)
		{
			if (jpts - ipts == npts - 2) continue;
			um_vctovc (pp[jpts], qt1);
			um_vctovc (pp[jpts+1], qt2);
			um_isegseg (pt1,pt2,qt1,qt2,&nint,pint,tol);
			if (nint > 0)
			{
				if (UM_SQDIS_2D (pt1,pint) < tolsq)
					ipts--;

				um_vcmnvc (pint,pp[ipts], vvp);
				if (UM_MAG(vvp) > tol)
					um_unitvc (vvp, vv[ipts]);

				um_vctovc (pint, pp[jpts]);

				um_vcmnvc (qt2, qt1, vvq);
				dd = UM_MAG (vvq);
				um_unitvc (vvq,vvq);
				um_unitvc (vv[jpts],vv[jpts]);
				co = UM_DOT (vvq, vv[jpts]);
				if (dd > tol && co > -0.9) 
					um_vctovc (vvq, vv[jpts]);
				k = jpts - ipts - 1;
				if (k > 0)
				{
					npts -= k;
					uu_list_delete (cvpoint,ipts+1,k);
					uu_list_delete (cvtang,ipts+1,k);
				}
				break;
			}
		}
	}

	if (!lclosed)
	{
		npts--;
		cvpoint->cur_cnt--;
		cvtang->cur_cnt--;
	}
/*
..... Check for "folding"
*/
	for (ipts = 0; ipts < npts-1; ipts++)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		um_vcmnvc_2d (pp[ipts+1],pp[ipts],vvp);
		ddp = UM_MAG_2D (vvp);
		if (ddp < tol/2.) 
		{
			uu_list_delete (cvpoint,ipts+1,1);
			uu_list_delete (cvtang,ipts+1,1);
			npts--;
			ipts--;
			continue;
		}
		if (ddp < tol || ipts > npts - 3) continue;
		vvp[0] /= ddp; vvp[1] /= ddp;
		d1 = UM_MAG_2D(vv[ipts]);
		vv1[0] = vv[ipts][0]/d1; vv1[1] = vv[ipts][1]/d1; 
		d2 = UM_MAG_2D(vv[ipts+1]);	
		vv2[0] = vv[ipts+1][0]/d2; vv2[1] = vv[ipts+1][1]/d2; 
		co1 = UM_DOT_2D (vvp,vv1);
		co2 = UM_DOT_2D (vvp,vv2);
		if (ipts < npts-2 && co1 + co2 < -1.8)
		{
			um_middlept (pp[ipts],pp[ipts+1],pp[ipts]);
			um_vcmnvc (pp[ipts+2],pp[ipts],vv[ipts]);
			uu_list_delete (cvpoint,ipts+1,1);
			uu_list_delete (cvtang,ipts+1,1);
			npts--;
			ipts--;
		}
	}

	if (lclosed)
		ncl_fix_corner0 (cvpoint, cvtang, tol, &npts);
	
	return (npts);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_tool_offset (tol,np,dis,vta,pm,cvpoint,cvtang)
**    Offset a closed 3D curve perpendicular to the tool axis vector.
**    PARAMETERS
**       INPUT  :
**    tol	     tolerance
**    npts       number of evolved pts
**    pm         direction modifier vector (currently 2-D)
**    dis        offset distance
**    cvpoint    list of evolved pts
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**
**    RETURNS      : new number of pts
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tool_offset (tol,np,dis,vta,cvpoint,cvtang,inclosed)
UU_REAL tol,dis;
int *np,*inclosed;
UM_vector vta;
UU_LIST *cvpoint,*cvtang;
{
	int ipts,npts;
	UU_REAL co,dd;
	UM_coord pt1, *pp, pint;
	UM_vector vv1,vv2, *vv;
	UM_vector vnorm,vnorm1,vnorm2; 

	npts = *np;
	*inclosed = 0;
/*
..... save first as pt1
*/
	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	um_vctovc (pp[0], pt1);

	ncl_fix_corners (cvpoint,cvtang,0.2*tol,2,&npts);

	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
/*
..... Offset evolved points along normals
*/
	for (ipts = 0; ipts < npts; ipts++)
	{
		um_vctovc (vv[ipts], vv1);
		dd = UM_MAG (vv1);
		vv1[0] /= dd; vv1[1] /= dd; vv1[2] /= dd;

		if (ipts > 0)
		{
			if (dd > 99000.)
			{
				um_vctovc (vv[ipts+1], vv2);
				dd = UM_MAG (vv2);
/*
..... if we have a sharp corner, it defines its own curve plane and
..... its own offsets
*/
				if (dd > 99000.)
				{
					vv2[0] /= dd; vv2[1] /= dd; vv2[2] /= dd;
					co = UM_DOT(vv1,vv2);
					if (fabs(co) < 0.999876)
					{
						um_cross (vta, vv1, vnorm1);
						um_unitvc (vnorm1, vnorm1);
						um_translate_point (pp[ipts],dis,vnorm1, pp[ipts]);
						if (dis < 0) um_vctmsc (vnorm1,-1.,vnorm1);
/*
..... if we offset outside the corner, offset the old apex point to the
..... new calculated apex point
*/
						if (UM_DOT (vnorm1,vv2) < 0.)
						{
							um_cross (vta, vv2, vnorm2);
							um_unitvc (vnorm2,vnorm2);
							co = UM_DOT(vnorm1,vnorm2);
							co = 1./(1+co);
							um_vcplvc (vnorm1,vnorm2,vnorm2);
							um_vctmsc (vnorm2,co,vnorm2);
						}
						else
						{
							um_cross (vta, vv2, vnorm2);
							um_unitvc (vnorm2, vnorm2);
						}
						ipts++;
						um_translate_point (pp[ipts],dis,vnorm2, pp[ipts]);
						continue;
					}
				}
			}
		}
		um_cross (vta, vv1, vnorm);
		um_unitvc (vnorm, vnorm);
		um_translate_point (pp[ipts],dis,vnorm, pp[ipts]);
	}
/*
..... for a closed curve - set up a flag showing the ends meet at a corner,
..... and we offset inside this corner
*/
	um_unitvc (vv[0],vv1);
	um_unitvc (vv[npts-1],vv2);
	if (UM_DOT(vv1,vv2) < 0.9999)
	{
		um_vcmnvc (pp[0],pt1,vnorm);
		if (UM_DOT(vnorm,vv2) < 0.)
			*inclosed = 1;
		else
		{
			um_cross (vta, vv1, vnorm1);
			um_unitvc (vnorm1, vnorm1);
			um_cross (vta, vv2, vnorm2);
			um_unitvc (vnorm2, vnorm2);
			co = UM_DOT(vnorm1,vnorm2);
			co = 1./(1+co);
			um_vcplvc (vnorm1,vnorm2,vnorm);
			um_vctmsc (vnorm,co,vnorm);
			um_translate_point (pt1,dis,vnorm, pint);
			uu_list_insert (cvpoint,0, pint);
			uu_list_push (cvpoint, pint);
			npts += 2;
		}
	}

	*np = npts;

   return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_dm_to_uv (tol,npts,dm,dis,cvpoint)
**    Offsets the evolved curve points for subsequent curve fitting;
**    then removes loops, fixes sharp corners and does some 
**    "reasonableness" checks 
**    PARAMETERS
**       INPUT  :
**			sfkey		-surface key
**			uvpoints	-list of evolved uv pts
**			dm			-direction modifier vector (xyz)
**       OUTPUT :
**			dmuv		-direction modifier vector (uv)
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_dm_to_dmuv(sfkey,uvpoints,dm,dmuv)
UU_KEY_ID sfkey;
UU_LIST *uvpoints;
UM_real8 *dm;
UM_real8 *dmuv;
{
	int status,npts;
	UM_coord *uvs,pt1,pt2,uv1,uv2,sfpt,arcpts[5];
	struct NCL_fixed_databag sf;	
	struct UM_evsrfout evsrf;
	UM_transf tfmat;
	UU_LOGICAL lrev;

	static UM_int2 IPT = NCLI_POINT;
    static UM_int2 IVE = NCLI_VECTOR;
	
	int k;
	UM_real8 t[3];

	UU_REAL dis;
	UM_2Dcoord temp_uv,uv0;
	UM_vector dist_vect, prj_pl_norm, sfnorm;

	lrev = UU_FALSE;
/*
.....Get surface data
*/
	sf.key = sfkey;
	status = ncl_retrieve_data_fixed (&sf);
	
	if (status == UU_SUCCESS)	
		status = uc_retrieve_transf (sf.key, tfmat);
/*
....Get curve data
*/
	npts = UU_LIST_LENGTH (uvpoints);
	if (npts < 1)
		return UU_FAILURE;
	uvs = (UM_coord *) UU_LIST_ARRAY (uvpoints);

	uv0[0] = uvs[0][0];
	uv0[1] = uvs[0][1];
	if (uv0[0] < 0.0) uv0[0] = 0.0;
	if (uv0[0] < 0.0) uv0[1] = 0.0;

/*
.....Get start xyz point
*/
	status = uc_init_evsrfout(&sf, &evsrf);		
	status = uc_evsrf(UM_NORM, uv0[0], uv0[1], &sf, tfmat, &evsrf);
	if (status != UM_VALID) 
		return UU_FAILURE;

	um_vctovc(evsrf.sp, sfpt);	
	um_unitvc(evsrf.snorm, sfnorm);

/*
.....Calcuate the nearby uv along dm direction on surface
*/
	um_vctovc(dm, dist_vect);
	um_cross(sfnorm, dist_vect, prj_pl_norm);

	temp_uv[0] = uv0[0];
	temp_uv[1] = uv0[1];
	um_vctovc(dist_vect,   arcpts[0]);
	um_vctovc(prj_pl_norm, arcpts[1]);
	um_vctovc(sfpt,       arcpts[2]);	
	dis = 0.002;

	status = ncl_go_along_surf_arclen(&sf, arcpts, temp_uv, dis);
	if(status != UU_SUCCESS)
		return UU_FAILURE;
/*
.....In case of wrong direction
*/
	if (um_sqdis_2d(uv0,temp_uv) < UM_DFUZZ * UM_FUZZ)
	{
		lrev = UU_TRUE;
		um_vctmsc(dm,-1.0,dist_vect);
		um_cross(sfnorm, dist_vect, prj_pl_norm);

		temp_uv[0] = uv0[0];
		temp_uv[1] = uv0[1];
		um_vctovc(dist_vect,   arcpts[0]);
		um_vctovc(prj_pl_norm, arcpts[1]);
		um_vctovc(sfpt,       arcpts[2]);	

		status = ncl_go_along_surf_arclen(&sf, arcpts, temp_uv, dis);
		if(status != UU_SUCCESS)
			return UU_FAILURE;
	}

/*
.....Get the uv direction vector
*/
	if (lrev)
		um_vcmnvc_2d(uv0, temp_uv, dmuv);
	else
		um_vcmnvc_2d(temp_uv, uv0, dmuv);
	um_unitvc_2d(dmuv, dmuv);

	return status;
}
