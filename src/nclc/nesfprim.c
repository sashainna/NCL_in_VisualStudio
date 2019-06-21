/*********************************************************************
**    NAME         :  nesfprim.c
**       CONTAINS:
**		int ncl_sf_prim_analyz
**		int ncl_sf_prim_plane
**		int ncl_sf_prim_typ
**		int ncl_sf_prim_sphere
**		int ncl_sf_prim_ruled
**		int ncl_sf_prim_cylinder
**		int ncl_sf_prim_cone
**		int ncl_sf_prim_torus
**		int ncl_get_3pt_sf
**		int ncl_store_prim_data
**		int ncl_offset_primdat
**		int rld_primdat
**		int ncl_rld_primdat
**		int ncl_tabcyl_primdat
**		int ncl_revsf_primdat
**		int ncl_itsa_lncipt
**    COPYRIGHT 2000 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesfprim.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:48
*********************************************************************/
#include "udebug.h"
#include "modef.h"
#include "mdeval.h"
#include "gobas.h"
#include "dasnog.h"
#include "dselmask.h"
#include "uhep.h"
#include "ulist.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mattr.h"
#include "mcrv.h"
#include "nccs.h"
#include "msrf.h"
#include "nclfc.h"
#include "ncl.h"
#include "mgeom.h"
#include "uminmax.h"

static UM_real8 tol1;

/*********************************************************************
**    E_FUNCTION     : ncl_sf_prim_plane (sfp)
**      Determine if the surface is a plane. If yes, fill
**      out the primitive and prim_param fields of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfp     surface pointer
**       OUTPUT :
**    RETURNS      :
**              UU_SUCCESS if a plane, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sf_prim_plane (sfp)
struct NCL_fixed_databag *sfp;
{
	int CHKPTS=25;
	int status,i,j,colinear;
	UM_coord pt[3],nvec;
	UU_REAL u,v,du,dv,dis0,dis,dot,param[16];
	UM_transf tfmat;
	struct UM_evsrfout evsrf;
/*
... initialize.
*/
	uc_init_evsrfout (sfp, &evsrf);
	status = uc_retrieve_transf(sfp->key, tfmat);
	if (status != UU_SUCCESS) return (UU_FAILURE);
/*
... get three non-collinear pts on the sf.
*/
	uc_evsrf(UM_POINT, 0.3333, 0.5, sfp, tfmat,&evsrf);
	um_vctovc(evsrf.sp,pt[0]);
	uc_evsrf(UM_POINT, 0.6667, 0.25, sfp, tfmat,&evsrf);
	um_vctovc(evsrf.sp,pt[1]);
	uc_evsrf(UM_POINT, 0.6667, 0.75, sfp, tfmat,&evsrf);
	um_vctovc(evsrf.sp,pt[2]);
/*
... create a plane through the above three pts.
*/
	colinear = um_3pt_colinear(pt[0],pt[1],pt[2],nvec,tol1);
	if (colinear != 0) return (UU_FAILURE);
	dis0 = um_dot(nvec,pt[0]);

	if (sfp->rel_num == UM_RBSPLSRF_REL)
	{
		struct UM_rbsplsrf_rec *eptr;
		UM_coord *pts;
		UU_LOGICAL um_is_idmat(), idmat;

		eptr = (struct UM_rbsplsrf_rec *) sfp;
		if (eptr->ku <= 2 && eptr->kv <= 2)
		{
			j = eptr->no_pt;
			pts = (UM_coord *) eptr->pt;
			idmat = um_is_idmat(tfmat);
			for (i = 0; i < j; i++)
			{
				if (!idmat) um_cctmtf (pts[i],tfmat,pts[i]);
				dis  = UM_DOT (nvec,pts[i]);
				if (fabs(dis-dis0) > tol1) return (UU_FAILURE);
			}
			goto Store;
		}
	}
/*
.. determine if all the other pts on the sf fall on the same pl.
*/	
	du = 1./(CHKPTS-1);
	dv = 1./(CHKPTS-1);
	u = -du;
	for (i = 0; i < CHKPTS; i++)
	{
		u = u+du;
		v = -dv;
		for (j = 0; j < CHKPTS; j++)
		{
			v = v+dv;
			uc_evsrf(UM_NORM, u, v, sfp, tfmat,&evsrf);
			dis = UM_DOT(nvec,evsrf.sp);	
			if (fabs(dis-dis0) > tol1) return (UU_FAILURE);
/*
..... check the normals, but only away from the edges, since otherwise 
..... this could foul a triangular plane
*/
			if (i*(CHKPTS-1-i)*j*(CHKPTS-1-j) > 0)
			{
				dot = UM_DOT(nvec,evsrf.snorm);
				if (fabs(dot) <= 0.99995) return (UU_FAILURE);
			}
		}
	}
Store:;
/*
... store prim data if it's a plane.
*/
	um_vctovc(nvec,param); /*normal of the plane*/
	param[3] = dis0; /*distance from the coor orig. to the plane*/
	for (i=0;i<3;i++) param[i+4]=pt[0][i]; /*a point on the plane*/
	for (i=7;i<16;i++) param[i]=0;
	status = ncl_store_prim_data(sfp,NCLSF_PLANE,param);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sf_prim_sphere (sfp,ptu,ptv)
**      Determine if the surface is a sphere. If yes, fill
**      out the primitive and prim_param fields of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfp     surface pointer
**              ptu     3 points (not collinear) at u=const
**              ptv     3 points (not collinear) at v=const
**       OUTPUT :
**    RETURNS      :
**              UU_SUCCESS if a sphere, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sf_prim_sphere (sfp,ptu,ptv)
struct NCL_fixed_databag *sfp;
UM_coord ptu[3],ptv[3];
{
	int status,i,j,nint;
	UU_REAL u,v,du,dv,dis,R,param[16];
	UM_coord npt;
	UM_transf tfmat;
	struct UM_evsrfout evsrf;
	struct UM_circle_rec ci1,ci2;
	int CHKPTS=25;
/*
... initialize.
*/
	uc_init_evsrfout (sfp, &evsrf);
	status = uc_retrieve_transf(sfp->key, tfmat);
	if (status != UU_SUCCESS) return (UU_FAILURE);
/*
... create two circles.
*/
	status = um_c3_3pt1(0,ptu[0],ptu[1],ptu[2],&ci1);
	if (status != UU_SUCCESS) return (UU_FAILURE);
	status = um_c3_3pt1(0,ptv[0],ptv[1],ptv[2],&ci2);
	if (status != UU_SUCCESS) return (UU_FAILURE);
/*
... create a sphere through the two circles.  
*/
	um_ilnln(ci1.center,ci1.nvec,ci2.center,ci2.nvec,&nint,npt);
	if (nint <1) return (UU_FAILURE);
	R = um_dcccc(npt,ptu[1]);
/*
.. determine if all the other pts on the sf fall on the same sphere.
*/	
	du = 1./(CHKPTS-1);
	dv = 1./(CHKPTS-1);
	u = -du;
	for (i=0; i<CHKPTS; i++)
	{
		u = u+du;
		v = -dv;
		for (j =0; j<CHKPTS; j++)
		{
			v = v+dv;
			uc_evsrf(UM_POINT, u, v, sfp, tfmat,&evsrf);
			dis = um_dcccc(npt,evsrf.sp);	
			if (fabs(dis-R)>tol1) return (UU_FAILURE);
		}
	}
/*
... store prim data if it's a sphere.
*/
	um_vctovc(npt,param); /*sphere center*/
	param[3] = R; /*sphere radius*/
	for (i=4;i<16;i++) param[i]=0;
	status = ncl_store_prim_data(sfp,NCLSF_SPHERE,param);

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sf_prim_cylinder (sfp,pt,uvflag)
**      Determine if the surface is a cylinder. If yes, fill
**      out the primitive and prim_param fields of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfp     surface pointer
**              pt      3 points (not collinear, at u=const or v=const)
**					 uvflag	0: points at u = const
**								1: points at v = const
**       OUTPUT :
**    RETURNS      :
**              UU_SUCCESS if a cylinder, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sf_prim_cylinder (sfp,pt,uvflag)
struct NCL_fixed_databag *sfp;
UM_coord pt[3];
int uvflag;
{
	int status,i,j;
	UU_REAL u,v,du,dv,dis,dis0,dmin,dmax,param[16];
	UU_REAL sum[3],len[3],lensum ,avglen;
	UM_coord vec[3], avgvec ,pt1, pt2;
	UM_transf tfmat;
	struct UM_evsrfout evsrf, evsrf1;
	struct UM_circle_rec ci;
	UM_line ln;
	UU_REAL um_dist_from_line();
	int CHKPTS = 25;
	UM_real8 ver;
	UM_int2 idx;
/*
... initialize.
*/
	lensum = 0.0;
	for (i=0;i<3;i++) sum[i]=0.0;
	uc_init_evsrfout (sfp, &evsrf);
	uc_init_evsrfout (sfp, &evsrf1);
	status = uc_retrieve_transf(sfp->key, tfmat);
	if (status != UU_SUCCESS) return (UU_FAILURE);
/*
... create a circle through the three points.
*/
	status = um_c3_3pt1(0,pt[0],pt[1],pt[2],&ci);
	if (status != UU_SUCCESS) return (UU_FAILURE);
	um_vctovc(ci.nvec,ln.n);
	um_vctovc(ci.center,ln.p0);
/*
.. determine if all the other pts on the sf fall on the same cylinder.
*/	
	du = 1./(CHKPTS-1);
	dv = 1./(CHKPTS-1);
	u = -du;
	dmin = 1.e+9;
	dmax = -1.e+9;

	for (i = 0; i < CHKPTS; i++)
	{
		u = u + du;
		v = -dv;
		for (j = 0; j < CHKPTS; j++)
		{
			v = v + dv;
			uc_evsrf(UM_POINT, u,v, sfp,tfmat, &evsrf);
			dis = um_dist_from_line(evsrf.sp, &ln);	
			if (fabs(dis-ci.radius) > tol1) return (UU_FAILURE);
			dis = um_dot (ci.nvec,evsrf.sp);
			if (dis > dmax) dmax = dis;
			if (dis < dmin) dmin = dis;
		}
	}
/*
... store prim data if it's a cylinder.
*/
	param[6] = ci.radius;  /*cyliner radius*/
/*
.....fix cylinder normal ,center and height
.....with the version flag. 
*/
	idx = 169;
	getsc (&idx, &ver);
	if (ver <= 9.44999)
	{
		dis0 = um_dot (ci.center,ci.nvec);
		um_translate_point(ci.center,dmin-dis0,ci.nvec,param); /*center pt*/
		um_vctovc(ci.nvec,&param[3]); /*cylinder normal*/
		param[7] = dmax - dmin; /*cylinder height*/
	}
	else
	{
/*
.....calculate 3 vectors on the surface of the cylinder from the base circle
.....to the top keeping u/v constant depending on the uv parameters.
*/
		for(i=0;i<3;i++)
		{
			if(uvflag ==0)
			{
				v = 0;
				u = 0.0 + (i * (1.0/3));
			}
			else
			{
				u = 0;
				v = 0.0 + (i * (1.0/3));
			}
			uc_evsrf(UM_POINT, u,v, sfp,tfmat, &evsrf1);
			um_vctovc(evsrf1.sp,pt1);
			if(uvflag ==0)
				v = 1; 
			else 
				u = 1;
			uc_evsrf(UM_POINT, u,v, sfp,tfmat, &evsrf1);
			um_vctovc(evsrf1.sp,pt2);
			um_vcmnvc(pt1,pt2,vec[i]);	
			for (j=0;j<3;j++)
				sum[j] = sum[j] + vec[i][j];
			len[i] = um_mag(vec[i]);
			lensum = lensum + len[i];
		}
/*
.....Calculate the height of the cylinder by taking the average lenght
.....of these vectors.
.....Calculate the axis of the cylinder by unitizing the average of these 
.....vectors.
*/
		for (j=0;j<3;j++)
			avgvec[j] = sum[j] / 3;
		avglen = lensum / 3;
		um_unitvc(avgvec,avgvec);
		um_vctovc(avgvec,&param[3]); /*cylinder normal*/

		du = 1./(CHKPTS-1);
		dv = 1./(CHKPTS-1);
		dmin = 1.e+9;
		dmax = -1.e+9;
		if(uvflag ==0)
		{
			u = -du;
			v = 0;
		}
		else
		{
			v = -dv;
			u = 0;
		}
		for (i = 0; i < 2; i++)
		{
			for (j = 0; j < CHKPTS; j++)
			{
				if(uvflag ==0)
					u = u + du;
				else
					v = v + dv;
				uc_evsrf(UM_POINT, u,v, sfp,tfmat, &evsrf);
				dis = um_dot (avgvec,evsrf.sp);
				if (dis > dmax) dmax = dis;
				if (dis < dmin) dmin = dis;
			}
			if(uvflag ==0)
			{
				v = 1;
				u = -du;
			}
			else
			{
				u = 1;
				v = -dv;
			}
		}
		dis0 = um_dot (ci.center,avgvec);
		um_translate_point(ci.center,dmin-dis0,avgvec,param); /*center pt*/

		param[7] = avglen; /*cylinder height*/
	}
	for (i=8;i<16;i++) param[i]=0;
	status = ncl_store_prim_data(sfp,NCLSF_CYLINDER,param);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sf_prim_cone (sfp,pt,spv)
**      Determine if the surface is a cone. If yes, fill
**      out the primitive and prim_param fields of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfp     surface pointer
**              pt      3 points (not collinear, at u=const or v=const)
**              spv     point and vector defining a possible cone axis
**       OUTPUT :
**    RETURNS      :
**              UU_SUCCESS if a cone, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sf_prim_cone (sfp,pt,spv)
struct NCL_fixed_databag *sfp;
UM_coord pt[3],spv[2];
{
	int status,i,j,ninit;
	UU_REAL u,v,du,dv,dis0,dis,dmin,dmax,param[16];
	UM_transf tfmat;
	struct UM_evsrfout evsrf;
	UM_angle alpha,ang,um_angle();
	struct UM_circle_rec ci;
	int CHKPTS = 25;
	UM_vector vec;
/*
... initialize.
*/
	uc_init_evsrfout (sfp, &evsrf);
	status = uc_retrieve_transf(sfp->key, tfmat);
	if (status != UU_SUCCESS) return (UU_FAILURE);
/*
... create a circle through the 3 points.
*/
	status = um_c3_3pt1(0,pt[0],pt[1],pt[2],&ci);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	if (fabs(um_dot(ci.nvec,spv[1])) < tol1) return (UU_FAILURE);
/*
... calculate cone apex point.
*/
	um_ilnln(spv[0],spv[1],ci.center,ci.nvec,&ninit,pt[0]);
	if (ninit < 1) return (UU_FAILURE);
	um_vcmnvc (spv[0],pt[0],spv[1]);
	um_vcmnvc (ci.center,pt[0],vec);
	if (um_dot(ci.nvec,vec) < 0.)
		um_vctmsc (ci.nvec,-1.,ci.nvec);
/*
..... both vectors look down from apex pt[0]: nvec along the axis,
..... spv[1] on the surface side
*/
	alpha = um_angle(spv[1],ci.nvec);
/*
.. determine if all the other pts on the sf fall on the same cone.
*/	
	du = 1./(CHKPTS-1);
	dv = 1./(CHKPTS-1);
	u = -du;
	dmin = 1.e+9;
	dmax = -1.e+9;
	dis0 = um_dot (pt[0],ci.nvec);

	for (i = 0; i < CHKPTS; i++)
	{
		u = u + du;
		v = -dv;
		for (j = 0; j < CHKPTS; j++)
		{
			v = v + dv;
			uc_evsrf(UM_POINT, u, v, sfp, tfmat,&evsrf);
			um_vcmnvc(evsrf.sp,pt[0],spv[1]);
			dis = um_mag(spv[1]);
			if (dis > tol1)
			{
				ang = um_angle(spv[1],ci.nvec);
				dis = fabs(dis * sin (ang-alpha));
				if (dis > tol1) return (UU_FAILURE);
			}
			dis = um_dot(evsrf.sp,ci.nvec) - dis0;
			if (dis < dmin) dmin = dis;
			if (dis > dmax) dmax = dis;
		}
	}
/*
... store prim data if it's a cone.
*/
	um_vctovc(pt[0],param); /* cone apex point */
	um_vctovc(ci.nvec, &param[3]); /* normal through apex point - down */
	param[6] = alpha; /* angle to the normal */
	param[7] = dmax - dmin; /* cone height */
	param[8] = dmin; /* distance from apex point to cone top */
	for (i=9;i<16;i++) param[i]=0.;
	status = ncl_store_prim_data(sfp,NCLSF_CONE,param);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sf_prim_torus (sfp,ptu,ptv)
**      Determine if the surface is a torus. If yes, fill
**      out the primitive and prim_param fields of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfp     surface pointer
**              ptu     3 points (not collinear) at u=const
**              ptv     3 points (not collinear) at v=const
**       OUTPUT :
**    RETURNS      :
**              UU_SUCCESS if a sphere, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sf_prim_torus(sfp,ptu,ptv)
struct NCL_fixed_databag *sfp;
UM_coord ptu[3],ptv[3];
{
	int status,i,j,inc0,inc1;
	UU_REAL uv[2],du,dis,param[16];
	UM_coord npt;
	UM_vector vc1;
	UM_transf tfmat;
	struct UM_evsrfout evsrf;
	struct UM_circle_rec ci[2];
	int CHKPTS=25;
/*
.....Initialize routine
*/
	uc_init_evsrfout (sfp, &evsrf);
	status = uc_retrieve_transf(sfp->key, tfmat);
	if (status != UU_SUCCESS) return (UU_FAILURE);
/*
.....Create circles in U & V directions
*/
	status = um_c3_3pt1(0,ptu[0],ptu[1],ptu[2],&ci[0]);
	if (status != UU_SUCCESS) return (UU_FAILURE);
	status = um_c3_3pt1(0,ptv[0],ptv[1],ptv[2],&ci[1]);
	if (status != UU_SUCCESS) return (UU_FAILURE);
/*
.....Circle axes must be perpendicular
*/
	if (!um_vcperp(ci[0].nvec,ci[1].nvec)) return(UU_FAILURE);
/*
.....Determine the major and minor circles
*/
	inc0 = 1; inc1 = 0;
/*
.....Calculate new major circle
.....Based on center of minor circle
*/
	um_nptln(ci[inc1].center,ci[inc0].center,ci[inc0].nvec,npt);
	um_vctovc(npt,ci[inc0].center);
	ci[inc0].radius = um_dcccc(ci[inc1].center,ci[inc0].center);
/*
.....Determine if all the other pts
.....are on the same torus
*/	
	du = 1./(CHKPTS-1);
	uv[inc0] = -du;
	for (i=0; i<CHKPTS; i++)
	{
		uv[inc0] += du;
		uv[inc1] = -du;
/*
........Calculate revolved circle center
*/
		uc_evsrf(UM_POINT, uv[0], uv[1], sfp, tfmat,&evsrf);
		um_nptln(evsrf.sp,ci[inc0].center,ci[inc0].nvec,npt);
		um_vcmnvc(evsrf.sp,npt,vc1); um_unitvc(vc1,vc1);
		um_translate_point(ci[inc0].center,ci[inc0].radius,vc1,npt);
/*
........Compare points to torus
*/
		for (j =0; j<CHKPTS; j++)
		{
			uv[inc1] += du;
			uc_evsrf(UM_POINT, uv[0], uv[1], sfp, tfmat,&evsrf);
			dis = um_dcccc(evsrf.sp,npt);	
			if (fabs(dis-ci[inc1].radius)>tol1) return (UU_FAILURE);
		}
	}
/*
.....Store prim data if it's a torus
*/
	um_vctovc(ci[inc0].center,param);	/* torus center*/
	um_vctovc(ci[inc0].nvec,&param[3]);	/* torus normal*/
	param[6] = ci[inc0].radius;			/* major radius */
	param[7] = ci[inc1].radius;			/* minor radius */
	for (i=8;i<16;i++) param[i]=0;
	status = ncl_store_prim_data(sfp,NCLSF_TORUS,param);

	return(status);
}


/*********************************************************************
**    E_FUNCTION     : ncl_get_3pt_sf (sfp,tfmat,evsrf,uflag,nvec)
**		  Get 3 noncollinear points on surface
**    PARAMETERS
**       INPUT  :
**              sfp     surface pointer
**              tfmat   surface transformation matrix
**              uflag   1 - get points along a u-curve
**		  	               0 - get points along a v-curve
**       OUTPUT :
**              pts     points on surface
**              nvec    if not collinear, a unit normal to the plane
**                      through the three points.
**                      if collinear, a unit vector along the line.
**    RETURNS      :
**              1 - returning 3 collinear points
**              0 - returning 3 noncollinear points
**             -1 - evaluator failure
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_get_3pt_sf(sfp,tfmat,evsrf,pts,uflag,nvec)
struct NCL_fixed_databag *sfp;
UM_transf tfmat;
struct UM_evsrfout evsrf;
int uflag;
UM_coord pts[3],nvec;
{
	int status,i,n,colinear;
	UU_REAL u,v,uv[3];

	n = 3;
	colinear = 1;
	uv[0] = 0.0;
 	uv[1] = 1./3.;
	uv[2] = 2./3.;

	if (uflag)
	{
		v = 0.5;
		for (i = 0; i < n; i++)
		{
			u = uv[i];
			status=uc_evsrf(UM_POINT,u,v,sfp,tfmat,&evsrf);
			if (status != UU_SUCCESS) return (UU_FAILURE);
			um_vctovc(evsrf.sp,pts[i]);
		}
		colinear = um_3pt_colinear(pts[0],pts[1],pts[2],nvec,tol1);
	}
	else
	{
		u = 0.5;
		for (i = 0; i < n; i++)
		{
			v = uv[i];
			status=uc_evsrf(UM_POINT,u,v,sfp,tfmat,&evsrf);
			if (status != UU_SUCCESS) return (UU_FAILURE);
			um_vctovc(evsrf.sp,pts[i]);
		}
		colinear = um_3pt_colinear(pts[0],pts[1],pts[2],nvec,tol1);
	}

	return (colinear);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sf_prim_ruled (sfp,uflag)
**      Determine if the surface is a ruled sf. If yes, fill
**      out the primitive and prim_param fields of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfp     surface pointer
**              uflag   0 - check if ruled in v; 1 - in u
**       OUTPUT :
**    RETURNS      :
**              UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sf_prim_ruled (sfp,uflag)
struct NCL_fixed_databag *sfp;
int uflag;
{
	int status,i,j,n;
	UU_REAL u,v,delta,param[16];
	UM_coord pt0,vec0,vec;
	UM_transf tfmat;
	struct UM_evsrfout evsrf;

	if (sfp->rel_num == UM_RBSPLSRF_REL)
	{
		struct UM_rbsplsrf_rec *eptr;
		eptr = (struct UM_rbsplsrf_rec *) sfp;
		if ((uflag && eptr->ku <= 2) || eptr->kv <= 2) goto Store;
	}
/*
... initialize.
*/
	uc_init_evsrfout (sfp, &evsrf);
	status = uc_retrieve_transf(sfp->key, tfmat);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	n = 25;
	delta = 1.0/(n-1);

	if (uflag)
	{
		v = -delta;
		for (j = 0; j<n; j++)
		{
			u = 0.;
			v = v+delta;
			status=uc_evsrf(UM_POINT,u,v,sfp,tfmat,&evsrf);
			if (status != UU_SUCCESS) return (UU_FAILURE);
			um_vctovc(evsrf.sp,pt0);

			u = delta;
			status=uc_evsrf(UM_POINT,u,v,sfp,tfmat,&evsrf);
			if (status != UU_SUCCESS) return (UU_FAILURE);
		
			um_vcmnvc(evsrf.sp,pt0,vec0);
			um_unitvc(vec0,vec0);
			
			for (i = 2; i<n; i++)
			{
				u = u+delta;
				um_vctovc(evsrf.sp,pt0);
				status=uc_evsrf(UM_POINT,u,v,sfp,tfmat,&evsrf);
				if (status != UU_SUCCESS) return (UU_FAILURE);

				um_vcmnvc(evsrf.sp,pt0,vec);
				um_unitvc(vec,vec);

				if(fabs(1.-um_dot(vec0,vec))>tol1)
					return (UU_FAILURE);
			}
		}
	}
	else
	{
		u = -delta;
		for (j = 0; j<n; j++)
		{
			v = 0.;
			u = u+delta;
			status=uc_evsrf(UM_POINT,u,v,sfp,tfmat,&evsrf);
			if (status != UU_SUCCESS) return (UU_FAILURE);
			um_vctovc(evsrf.sp,pt0);

			v = delta;
			status=uc_evsrf(UM_POINT,u,v,sfp,tfmat,&evsrf);
			if (status != UU_SUCCESS) return (UU_FAILURE);
		
			um_vcmnvc(evsrf.sp,pt0,vec0);
			um_unitvc(vec0,vec0);
			
			for (i = 2; i<n; i++)
			{
				v = v+delta;
				um_vctovc(evsrf.sp,pt0);
				status=uc_evsrf(UM_POINT,u,v,sfp,tfmat,&evsrf);
				if (status != UU_SUCCESS) return (UU_FAILURE);

				um_vcmnvc(evsrf.sp,pt0,vec);
				um_unitvc(vec,vec);

				if (fabs(1.-um_dot(vec0,vec))>tol1)
					return (UU_FAILURE);
			}
		}
	}
Store:;
	if (uflag) 
		param[0] = 1; /* ruled in u*/
	else	
		param[0] = 0; /* ruled in v*/
	for (i=1;i<16;i++) param[i] = 0.0;
	status = ncl_store_prim_data(sfp,NCLSF_RULED,param);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sf_prim_typ (sfp)
**      Determine if the surface is a primitive. If yes, fill
**      out the primitive and prim_param fields of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfp     surface pointer
**       OUTPUT :
**    RETURNS      :
**              UU_SUCCESS if a primitive type found, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sf_prim_typ (sfp)
struct NCL_fixed_databag *sfp;
{
	int status,colnu,colnv, prim;
	UM_coord ptu[3],ptv[3],tmpu,tmpv;
	UM_transf tfmat;
	struct UM_evsrfout evsrf;
	
	prim = UU_FAILURE;
/*
... initialize.
*/
	uc_init_evsrfout (sfp, &evsrf);
	status = uc_retrieve_transf(sfp->key, tfmat);
	if (status != UU_SUCCESS) return (UU_FAILURE);
/*
... get three noncollinear pts on a u line and three on a v line.
*/
	colnu = ncl_get_3pt_sf(sfp,tfmat,evsrf,ptu,1,tmpu);
	if (colnu == -1) return (UU_FAILURE);

	colnv = ncl_get_3pt_sf(sfp,tfmat,evsrf,ptv,0,tmpv);
	if (colnv == -1) return (UU_FAILURE);

	if (colnu == 0 && colnv == 0) 
	{
		status = ncl_sf_prim_sphere(sfp,ptu,ptv);
		if (status == UU_SUCCESS)
			prim = UU_SUCCESS;
      else
      {
         status = ncl_sf_prim_torus(sfp,ptu,ptv);
         if (status == UU_SUCCESS) prim = UU_SUCCESS;
		}
	}
	else
	{
		if (colnu == 1)
		{
			status = ncl_sf_prim_ruled(sfp,1);
			if (status == UU_SUCCESS)
				prim = UU_SUCCESS;
			if (status == UU_SUCCESS && colnv == 0) 
			{
/*
..... test the circle normal against the ruled direction - 
..... should be no more than 1 degree for a cylinder
..... in QAR 92154 the angle was more than 5 degrees
*/
				if (fabs(um_dot(tmpu,tmpv)) < 0.99985) 
					status = UU_FAILURE;
				else
					status = ncl_sf_prim_cylinder(sfp,ptv,1);
				if (status != UU_SUCCESS)
				{
					UM_coord spu[2];
					um_vctovc(ptu[0],spu[0]);
					um_vctovc(tmpu,spu[1]);
					status = ncl_sf_prim_cone(sfp,ptv,spu);
				}
			}
		}
		if (colnv == 1 && sfp->rel_num != NCL_REVSURF_REL)
		{
			status = ncl_sf_prim_ruled(sfp,0);
			if (status == UU_SUCCESS)
				prim = UU_SUCCESS;
			if (status == UU_SUCCESS && colnu == 0) 
			{
/*
..... test the circle normal against the ruled direction - 
..... should be no more than 1 degree for a cylinder
*/
				if (fabs(um_dot(tmpu,tmpv)) < 0.99985) 
					status = UU_FAILURE;
				else
					status = ncl_sf_prim_cylinder(sfp,ptu,0);
				if (status != UU_SUCCESS)
				{
					UM_coord spv[2];
					um_vctovc(ptv[0],spv[0]);
					um_vctovc(tmpv,spv[1]);
					status = ncl_sf_prim_cone(sfp,ptu,spv);
				}
			}
		}
	}
	return (prim);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_itsa_lncipt (eptr)
**       Determine if an entity is a line, a point, a circle, or neither
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to entity
**       OUTPUT :
**          none
**    RETURNS      :
**         0 if point, 1 if a line, 2 if a circle; -1 if neither
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_itsa_lncipt (eptr)
struct NCL_fixed_databag *eptr;
{
	int rel = eptr->rel_num;

	if (rel == UM_POINT_REL)
		return (0);
	else if ((rel == UM_LINE_REL)||(rel == NCL_LINE_REL))
		return (1);
	else if ((rel == UM_CIRCLE_REL)||(rel == NCL_CIRCLE_REL))
		return (2);
	else
		return (-1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sf_prim_analyz (sfkey,primtyp,primdata)
**	Determine if the surface is a special primitive. If yes, fill
**	out the primitive and prim_param fields of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfp     surface pointer
**       OUTPUT :
**              primtyp     primitive type
**              primdata    primitive type data 
**    RETURNS      :
**       	UU_SUCCESS if successfully analyzed, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sf_prim_analyz (sfkey,primtyp,primdata)
UM_int4 *sfkey;
UM_int2 *primtyp;
UM_real8 primdata[16];
{
	int i, status;
	struct NCL_fixed_databag sf;
	nclsf_prim_type typ;
	UM_real8 tol;

	sf.key = *sfkey;
	status = ncl_retrieve_data_fixed (&sf);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	gettol (&tol);
	tol1 = MAX2(2.*tol,0.001);

	if (ncl_itsa_trimsrf(&sf))
	{
		struct NCL_trimsf_rec *trimptr;
		trimptr = (struct NCL_trimsf_rec *) &sf;
		sf.key = trimptr->bs_key;
		status = ncl_retrieve_data_fixed (&sf);
		if (status != UU_SUCCESS) return(UU_FAILURE);
	}

	if (sf.rel_num != NCL_SURF_REL && sf.rel_num != UM_RBSPLSRF_REL &&
		sf.rel_num !=  NCL_REVSURF_REL) return (UU_FAILURE);

	typ = NCLSF_FREEFORM;
	*primtyp = (UM_int2) NCLSF_FREEFORM;
	for (i=0; i<16; i++) primdata[i] = 0.;
	ncl_store_prim_data(&sf,typ,primdata);
		
	status = ncl_sf_prim_plane(&sf);
/*
..... if not a plane, analyze further
*/
	if (status == UU_FAILURE)
		status = ncl_sf_prim_typ(&sf);
/*
..... if some primitive found, get the data
..... if not, return NCLSF_FREEFORM with zero data
*/
	if (status == UU_SUCCESS)
		status = ncl_get_sf_primdat(sfkey,primtyp,primdata);
	else
		status = UU_SUCCESS;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_store_prim_data (sfp,typ,param)
**      Fill out the primitive and prim_param fields of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfp     surface pointer
**		typ	primitive type
**		param	primitive data
**       OUTPUT :
**    RETURNS      :
**              UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_store_prim_data(sfp,typ,param)
struct NCL_fixed_databag *sfp;
nclsf_prim_type typ;
UU_REAL *param;
{
	int status,i;
	int n = 16;
	status = UU_SUCCESS;

	switch (sfp->rel_num)
	{
		case NCL_SURF_REL:
		{
			struct NCL_surface_rec *eptr;
			eptr = (struct NCL_surface_rec *) sfp;
			eptr->primitive = typ;
			for (i=0; i<n; i++) 
				eptr->prim_param[i] = param[i];
			ur_update_data_fixed (eptr);
		}
		break;

		case NCL_REVSURF_REL:
		{
			struct NCL_revsurf_rec *eptr;
			eptr = (struct NCL_revsurf_rec *) sfp;
			eptr->primitive = typ;
			for (i=0; i<n; i++) 
				eptr->prim_param[i] = param[i];
			ur_update_data_fixed (eptr);
		}
		break;

		case UM_RBSPLSRF_REL:
		{
			struct UM_rbsplsrf_rec *eptr;
			eptr = (struct UM_rbsplsrf_rec *) sfp;
			eptr->primitive = typ;
			for (i=0; i<n; i++) 
				eptr->prim_param[i] = param[i];
			ur_update_data_fixed (eptr);
		}
                break;

		default:
			status = UU_FAILURE;
		break;
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_put_sf_primdat (sfkey,primtyp,primdata)
**      get prim_param field and primdata of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfkey   surface key
**       OUTPUT :
**              primtyp primitive type
**              primdat primitive data
**    RETURNS      :
**              UU_SUCCESS if successful; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_put_sf_primdat(sfkey,primtyp,primdata)
UM_int4 *sfkey;
UM_int2 *primtyp;
UM_real8 primdata[16];
{
	int status;
	int typ = *primtyp;
	struct NCL_fixed_databag sf;
	

	sf.key = *sfkey;
	status = ncl_retrieve_data_fixed (&sf);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	if (ncl_itsa_trimsrf(&sf))
	{
		struct NCL_trimsf_rec *eptr;
		eptr = (struct NCL_trimsf_rec *) &sf;
		sf.key = eptr->bs_key;
		status = ncl_retrieve_data_fixed (&sf);
		if (status != UU_SUCCESS) return(UU_FAILURE);
	}

	if (sf.rel_num !=  NCL_SURF_REL && sf.rel_num != UM_RBSPLSRF_REL &&
			sf.rel_num !=  NCL_REVSURF_REL) return (UU_FAILURE);

	ncl_store_prim_data(&sf,typ,primdata);

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_offset_primdat(sfkey,typ,param,cdis)
**      recalculate primdata for surface offset.
**    PARAMETERS
**       INPUT  :
**              key     surface key
**              typ     primitive type
**              param   primitive data
**              cdis    offset distance
**       OUTPUT :
**              param   primitive data
**    RETURNS      :
**              UU_SUCCESS if successful; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_offset_primdat(sfkey,typ,param,cdis)
UU_KEY_ID sfkey;
nclsf_prim_type typ;
UU_REAL *param,cdis;
{
	int status = UU_SUCCESS;
	struct NCL_fixed_databag sf;
	UM_transf tfmat;
	struct UM_evsrfout evsrf;
	UU_REAL u,v;

/*
..... get one point at (0.5,0.5) on the offset surface + the normal 
*/
	sf.key = sfkey;
	status = ncl_retrieve_data_fixed (&sf);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	uc_init_evsrfout (&sf, &evsrf);
	status = uc_retrieve_transf(sfkey, tfmat);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	u = v = 0.5;
	status = uc_evsrf(UM_NORM,u,v,&sf,tfmat,&evsrf);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	if (typ == NCLSF_PLANE)
	{
		um_vctovc (evsrf.sp, &param[4]); /* plane point */
		param[3] = um_dot(param,&param[4]); /* the D parameter in (pl/A,B,C,D) */
	}
	else if (typ == NCLSF_SPHERE)
	{
		param[3] = um_dcccc (param,evsrf.sp); /* radius */
	}
	else if (typ == NCLSF_CYLINDER)
	{
		UU_REAL d,co,si;

		um_unitvc (&param[3],&param[3]);
		um_vcmnvc (param,evsrf.sp,evsrf.snorm);
		d = um_mag (evsrf.snorm);
		um_unitvc (evsrf.snorm,evsrf.snorm);
		co = um_dot (evsrf.snorm,&param[3]);
		si = sqrt (1. - co*co);
		param[6] = d*si; /* cylinder base radius */
	}
	else if (typ == NCLSF_CONE)
	{
		UU_REAL si,cosec;

		si = fabs (sin (param[6]));
		if (um_dot (evsrf.snorm, &param[3]) < 0.) si = -si;
		cosec = 1./si;
		param[8] += cdis*cosec - cdis*si;
		um_translate_point (param,cdis*cosec,&param[3],param); /* apex point */
	}
	else if (typ == NCLSF_TORUS)
	{
		UM_coord npt;
		UM_vector vc1;
/*
........Calculate revolved circle center
*/
		um_nptln(evsrf.sp,&param[0],&param[3],npt);
		um_vcmnvc(evsrf.sp,npt,vc1); um_unitvc(vc1,vc1);
		um_translate_point(&param[0],param[6],vc1,npt);
/*
........Calculate new minor radius
*/
		param[7] = um_dcccc(evsrf.sp,npt);
	}
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : rld_primdat(key1,key2,typ,param)
**      calculate primdata of a ruled surface - Fortran interface
**    PARAMETERS
**       INPUT  :
**              key1,key2 - keys of two curves defining the surface
**       OUTPUT :
**              typ     primitive type
**              param   primitive data
**    RETURNS      :
**              UU_SUCCESS if successful; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int rld_primdat(key1,key2,typ,param)
UM_int4 *key1,*key2;
UM_int2 *typ;
UM_real8 param[16];
{
	struct NCL_fixed_databag bp1,bp2;
	int status;

	bp1.key = *key1;
	status = ncl_retrieve_data_fixed (&bp1);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	bp2.key = *key2;
	status = ncl_retrieve_data_fixed (&bp2);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	status = ncl_rld_primdat(&bp1,&bp2,typ,param);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_rld_primdat(bp1,bp2,typ,param)
**      calculate primdata of a ruled surface
**    PARAMETERS
**       INPUT  :
**              bp1,bp2 - two curves defining the surface
**       OUTPUT :
**              typ     primitive type
**              param   primitive data
**    RETURNS      :
**              UU_SUCCESS if successful; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_rld_primdat(bp1,bp2,typ,param)
struct NCL_fixed_databag *bp1,*bp2;
UM_int2 *typ;
UM_real8 param[16];
{
	int i,ll1,ll2;
	struct UM_point_rec *bpt;

	for (i=0; i<16; i++) param[i] = 0.;

	ll1 = ncl_itsa_lncipt(bp1); ll2 = ncl_itsa_lncipt(bp2);

	if ((ll1==1 && ll2==1) || (ll1==1 && ll2==0) || (ll1==0 && ll2==1))
	{
		UM_coord spt1,ept1,spt2,ept2,mpt;
		UM_vector v1,v2,v3,v;
		UU_REAL d;
/*
... get line/point data.
*/
		struct UM_line_rec *ln;

		if (ll1 == 1)
		{
			ln = (struct UM_line_rec *) bp1;
			um_vctovc (ln->spt,spt1);
			um_vctovc (ln->ept,ept1);
			um_vcmnvc(ept1,spt1,v1);
		}
		else
		{
			bpt = (struct UM_point_rec *) bp1;
			um_vctovc (bpt->pt,spt1);
			um_vctovc (bpt->pt,ept1);
			um_nullvc (v1);
		}
		if (ll2 == 1)
		{
			ln = (struct UM_line_rec *) bp2;
			um_vctovc (ln->spt,spt2);
			um_vctovc (ln->ept,ept2);
			um_vcmnvc(ept2,spt2,v2);
		}
		else
		{
			bpt = (struct UM_point_rec *) bp2;
			um_vctovc (bpt->pt,spt2);
			um_vctovc (bpt->pt,ept2);
			um_nullvc (v2);
		}
/*
..... calculate tentative plane point in the middle
*/
		for (i = 0; i < 3; i++)
			mpt[i] = 0.25*(spt1[i]+ept1[i]+spt2[i]+ept2[i]);

		um_vcmnvc(spt2,spt1,v3); 
		d = um_mag (v3);
		if (ll2 == 1)
		{
			um_vcmnvc(ept2,spt1,v); 
			if (d > um_mag (v))
				um_vctovc (v,v3);
		}
		else
		{
			um_vcmnvc(spt2,ept1,v); 
			if (d > um_mag (v))
				um_vctovc (v,v3);
		}
/*
..... a line + a point always define a plane
*/
		if (ll1*ll2 == 0) goto Pl;
/*
..... two lines are coplanar iff the determinant is zero, then the surface
..... is planar if not degenerate
*/
		d = v3[0] * (v1[1]*v2[2] - v1[2]*v2[1]) +
	         v3[1] * (v1[2]*v2[0] - v1[0]*v2[2]) +
	         v3[2] * (v1[0]*v2[1] - v1[1]*v2[0]);
		if (fabs (d) > UM_DFUZZ)
			*typ = NCLSF_RULED;
		else
		{
			um_cross (v1,v2,v);
			d = um_mag (v);
			if (d > UM_FUZZ)
				*typ = NCLSF_PLANE;
			else
			{
Pl:;
				if (ll1 == 1)
				{
					um_cross (v1,v3,v);
					d = um_mag (v);
				}
				if (ll1 == 0 || d < UM_FUZZ)
				{
					um_cross (v2,v3,v);
					d = um_mag (v);
					if (d > UM_FUZZ)
						*typ = NCLSF_PLANE;
					else
						*typ = NCLSF_RULED;
				}
				else
					*typ = NCLSF_PLANE;
			}
		}
		if (*typ == NCLSF_PLANE)
		{
			for (i = 0; i < 3; i++)
				param[i] = v[i]/d;
			um_vctovc (mpt, &param[4]);
			param[3] = param[0]*param[4]+param[1]*param[5]+param[2]*param[6];
		}
	}
	else if ((ll1==2 && ll2==2) || (ll1==2 && ll2==0) || (ll1==0 && ll2==2))
	{
		UM_coord cpt1,cpt2;
		UM_vector svec1,nvec1,svec2,nvec2,v;
		UU_REAL dang1,dang2,rad1,rad2,d,h;
/*
... get circle/point data
*/
		struct UM_circle_rec *ci;
		if (ll1 == 2)
			ci = (struct UM_circle_rec *) bp1;
		else
			ci = (struct UM_circle_rec *) bp2;
		dang1 = ci->dang;
		rad1 = ci->radius;
		um_vctovc (ci->center,cpt1);
		um_vctovc (ci->svec,svec1);
		um_vctovc (ci->nvec,nvec1);

		if (ll1*ll2 == 0)
		{
/*
..... a circle + a point: make a plane if the point is in the circle plane,
..... or a cone if the point is on the circle nvec axis
*/
			if (ll1 == 0)
				bpt = (struct UM_point_rec *) bp1;
			else
				bpt = (struct UM_point_rec *) bp2;
			um_vctovc (bpt->pt,cpt2);
			um_vcmnvc(cpt1,cpt2,v);
			if (fabs(um_dot (nvec1,v)) < UM_FUZZ)
			{
				*typ = NCLSF_PLANE;
				um_vctovc (nvec1,param);
				um_vctovc (cpt1, &param[4]);
				param[3] = param[0]*param[4]+param[1]*param[5]+param[2]*param[6];
			}
			else
			{
				h = um_mag(v);
				for (i = 0; i < 3; i++) v[i] /= h;
				if (um_vcparall(nvec1,v))
				{
					*typ = NCLSF_CONE;
					um_vctovc (cpt2,param);
					um_vctovc (v,&param[3]);
					param[6] = atan2 (rad1,h);
					param[7] = h;
				}
			}
		}
		else
		{
/*
..... two circles
*/
			ci = (struct UM_circle_rec *) bp2;
			dang2 = ci->dang;
			rad2 = ci->radius;
			um_vctovc (ci->center,cpt2);
			um_vctovc (ci->svec,svec2);
			um_vctovc (ci->nvec,nvec2);

			if (um_vcparall(nvec1,nvec2))
			{
				um_vcmnvc(cpt1,cpt2,v);
/*
..... plane if the circles are coplanar
*/
				if (fabs(um_dot (nvec1,v)) < UM_FUZZ)
				{
					*typ = NCLSF_PLANE;
					um_vctovc (nvec1,param);
					um_vctovc (cpt1, &param[4]);
					param[3] = param[0]*param[4]+param[1]*param[5]+param[2]*param[6];
				}
/*
..... if the centers are on the nvec axis, the starting angles are
..... the same, the circular arc angles are the same: then it is a cone or
..... a cylinder
*/
				else if (fabs(dang1-dang2) < UM_FUZZ)
				{
					if (rad2 > rad1 + UM_FUZZ)
					{
						d = rad1; rad1 = rad2; rad2 = d;
						for (i = 0; i < 3; i++)
						{
							d = cpt1[i]; cpt1[i] = cpt2[i]; cpt2[i] = d;
							v[i] = -v[i];
						}
					}
	
					h = um_mag(v);
					for (i = 0; i < 3; i++) v[i] /= h;
					um_middlept(svec1,svec2,svec1);
					d = um_mag(svec1);
					if (um_vcparall(nvec1,v) && fabs (rad1*(1.-d)) < UM_FUZZ)
					{
						if (fabs (rad1 - rad2) < UM_FUZZ)
						{
							*typ = NCLSF_CYLINDER;
							um_vctovc (cpt1,param);
							um_vctovc (nvec1,&param[3]);
							param[6] = rad1;
							param[7] = h;
						}
						else
						{
							*typ = NCLSF_CONE;
							param[6] = atan2 (rad1-rad2,h);
							param[7] = h;
							param[8] = (rad2*h)/(rad1-rad2);
							um_translate_point (cpt2,-param[8],v,param);
							um_vctovc (v,&param[3]);
						}
					}
					else
						*typ = NCLSF_RULED;
				}
			}
		}
	}

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_tabcyl_primdat(crv,vecd,typ,param)
**      calculate primdata of a tabcyl surface
**    PARAMETERS
**       INPUT  :
**              crv   - a curve defining the surface
**              vecd    a vector defining the surface
**       OUTPUT :
**              typ     primitive type
**              param   primitive data
**    RETURNS      :
**              UU_SUCCESS if successful; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tabcyl_primdat(crv,vecd,typ,param)
struct NCL_fixed_databag *crv;
UM_vector vecd;
UM_int2 *typ;
UM_real8 param[16];
{
	int i;
	UM_vector v;
	UU_REAL d;

	for (i=0; i<16; i++) param[i] = 0.;

	if (ncl_itsa_line(crv))
	{
/*
... get line data.
*/
		struct UM_line_rec *ln;
		ln = (struct UM_line_rec *) crv;

		um_vcmnvc (ln->ept,ln->spt,v);
		um_cross (v,vecd,v);
		d = um_mag (v);
		if (d > UM_FUZZ)
		{
			*typ = NCLSF_PLANE;
			um_unitvc (v, param);
			um_middlept(ln->spt,ln->ept,v);
			um_translate_point (v,0.5,vecd,&param[4]);
			param[3] = param[0]*param[4]+param[1]*param[5]+param[2]*param[6];
		}
		else
			*typ = NCLSF_RULED;
	}
	else if (ncl_itsa_circle(crv))
	{
/*
... get circle data
*/
		struct UM_circle_rec *ci;
		ci = (struct UM_circle_rec *) crv;

		um_unitvc (vecd,v);
		if (um_vcparall(ci->nvec,v))
		{
			*typ = NCLSF_CYLINDER;
			um_vctovc (ci->center,param);
			um_vctovc (v,&param[3]);
			param[6] = ci->radius;
			param[7] = um_mag(vecd);
		}
		else
			*typ = NCLSF_RULED;
	}

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_revsf_primdat (pta,vca,crvp,&typ,param)
**      calculate primdata of a surface of revolution
**    PARAMETERS
**       INPUT  :
**              crv   - a curve defining the surface
**              vecd    a vector defining the surface
**       OUTPUT :
**              typ     primitive type
**              param   primitive data
**    RETURNS      :
**              UU_SUCCESS if successful; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_revsf_primdat (pta,vca,crv,typ,param)
UM_coord pta;
UM_vector vca;
struct NCL_fixed_databag *crv;
UM_int2 *typ;
UM_real8 param[16];
{
	int i;
	UM_vector v,vc,v1,v2;
	UM_coord ptx;
	UU_REAL d,d1,d2,tol;
	UM_real8 tol8;

	for (i=0; i<16; i++) param[i] = 0.;
	um_unitvc (vca,vc);
	gettol (&tol8);
	tol = tol8;

	if (ncl_itsa_line(crv))
	{
/*
... get line data.
*/
		struct UM_line_rec *ln;
		ln = (struct UM_line_rec *) crv;

		um_vcmnvc (ln->ept,ln->spt,v);
		d = um_mag (v);
		if (d > UM_FUZZ)
		{
			um_unitvc (v,v);
			if (um_vcparall(vc,v))
			{
				*typ = NCLSF_CYLINDER;
				param[7] = d;
				um_vcmnvc(ln->spt,pta,v1);
				d1 = um_dot(v1,vc);
				d = sqrt (v1[0]*v1[0]+v1[1]*v1[1]+v1[2]*v1[2] - d1*d1);
				um_translate_point (pta,d1,vc,param);
				um_vctovc (v,&param[3]);
				param[6] = d;
			}
			else
			{
/*
..... intersect the line with the axis of rotation - if no intersection it is
..... just 'ruled'
*/
				um_lnlndis (ln->spt,v,pta,vc,&d,ptx);

				if (d > tol)
					*typ = NCLSF_RULED;
				else
				{
					um_vcmnvc(ln->spt,ptx,v1);
					d1 = um_dot(v1,vc);
					um_vcmnvc(ln->ept,ptx,v2);
					d2 = um_dot(v2,vc);
/*
..... if start and end of the line are on opposite sides of the intersection -
..... this is technically a cone but we put its type as just 'ruled'
*/
					if (d1*d2 < -UM_DFUZZ)
						*typ = NCLSF_RULED;
					else
					{
						*typ = NCLSF_CONE;
						um_vctovc (ptx, param);
						if (d1 + d2 < 0.)
						{
							d1 = -d1; d2 = -d2;
							um_vctmsc(vc,-1.,&param[3]);
						}
						else
							um_vctovc (vc,&param[3]);
						if (d1 >= d2)
						{
							um_unitvc (v1,v1);
							param[6] = um_angle(v1,&param[3]);
							param[7] = d1 - d2;
							param[8] = d2;
						}
						else
						{
							um_unitvc (v2,v2);
							param[6] = um_angle(v2,&param[3]);
							param[7] = d2 - d1;
							param[8] = d1;
						}
					}
				}
			}
		}
	}
	else if (ncl_itsa_circle(crv))
	{
/*
... get circle data
*/
		struct UM_circle_rec *ci;
		ci = (struct UM_circle_rec *) crv;

		um_vcmnvc(ci->center,pta,v);
		d = um_dot(vc,v);

		if (v[0]*v[0]+v[1]*v[1]+v[2]*v[2] - d*d < UM_DFUZZ)
		{
			*typ = NCLSF_SPHERE;
			um_translate_point (pta,d,vc,param);
			param[3] = ci->radius;
		}
		else
		{
			d = um_mag(v);
			if (um_vcperp(ci->nvec,vc) && d >= ci->radius)
			{
				um_nptln(ci->center,pta,vc,param);
				um_vctovc(vc,&param[3]);
				param[6] = um_dcccc(param,ci->center);
				param[7] = ci->radius;
				*typ = NCLSF_TORUS;
			}
			else
				*typ = NCLSF_FREEFORM;
		}
	}

	return (0);
}
