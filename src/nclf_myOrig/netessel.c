/*********************************************************************
**    NAME         :  netessel.c
**       CONTAINS: tessellation routines
**
**       ncl_tessellate_surf
**       ncl_tess_surf
**       ncl_tessellate_polygon
**       ncl_get_tess_triangles
**       ncl_get_tessellation
**       ncl_xform_tessellation
**		 ncl_tess_surfbase
**
**		 S_check_refine_within
**       S_refine_bndr
**       S_tess_rectangles
**       ncl_tess_uvboxes
**       S_tess_panel
**       S_tess_panel_calc
**       S_len_ratio
**       S_print_rect
**
** These functions are not used, they are kept in SCCS:
**       ncl_get_tess_triangles_uv
**       ncl_tessellate_surf_o
**       ncl_find_dv
**       ncl_find_dv1
**       ncl_find_width
**       ncl_tessellate_sf
**       ncl_tessellate_uvbox
**
**    COPYRIGHT 1998 (c) Numerical Control Computer Sciences Inc.
**                       All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       netessel.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 10:43:50
*********************************************************************/

#include "ulist.h"
#include "nccs.h"
#include "mdrel.h"
#include "mdeval.h"
#include "uminmax.h"
#include "mcrv.h"
#include "mgeom.h"
#include "nclfc.h"
#include "ngeom.h"
#include "uminmax.h"
#include "math.h"

#define DEBUG 0

static UU_LIST Suvlst,Slst1,Slst2,*Scvpts;
static UU_LOGICAL Sluvtrans = UU_FALSE;
static UU_LOGICAL Sneedbndr = UU_FALSE;
static UU_REAL Sdu,Sdv,Sru,Srv;
static UM_2Dcoord Summx,Svmmx;
static int Snp0[2];
static struct UM_evsrfout Sevsrf;
static struct UM_evcrvout Sevcv;
static int Six1,Six2;
static UU_REAL Sbplm[4];
static UU_REAL Sy;
static UU_LOGICAL S_use_bndr_curves = UU_FALSE;

static int S_add_triangles();
static int S_tess_1tri();
static void S_polygon_cclw();
static void S_tess_panel_calc();

/*********************************************************************
**    Set the flag to indicate that tessellation is being calculated
**    for display.
*********************************************************************/
void ncl_set_bndr_return()
{
	Sneedbndr = UU_TRUE;
}

/*********************************************************************
**     Reset the Sneedbndr flag.
*********************************************************************/
void ncl_reset_bndr_return()
{
	Sneedbndr = UU_FALSE;
}

/*********************************************************************
**    I_FUNCTION: S_scale_bndr (bound,du,dv)
**       Scale UV boundary if needed.
*********************************************************************/
static void S_scale_bndr (bound,du,dv)
UM_srf_boundary *bound;
UU_REAL du,dv;
{
	int i,np;
	UM_coord *pp;

	pp = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	np = bound->uvpts->cur_cnt;

	for (i = 0; i < bound->nb; i++)
	{
		bound->ummx[i][0] *= du; bound->ummx[i][1] *= du;
		bound->vmmx[i][0] *= dv; bound->vmmx[i][1] *= dv;
	}
	for (i = 0; i < np; i++)
	{
		pp[i][0] *= du;
		pp[i][1] *= dv;
	}
}

/*********************************************************************
**    I_FUNCTION: S_set_uvtrans (bound)
**       Scale UV boundary if needed.
*********************************************************************/
static int S_set_uvtrans (bound)
UM_srf_boundary *bound;
{
	UU_REAL ru,rv;

	ru = bound->ummx[0][1] - bound->ummx[0][0];
	rv = bound->vmmx[0][1] - bound->vmmx[0][0];

	if (ru < 5.e-5 || rv < 5.e-5)
		return (-1);

	if ((ru > 0.1 && rv < 0.01) || (ru < 0.01 && rv > 0.1))
	{
		Sluvtrans = UU_TRUE;
		Sru = ru; Srv = rv;
		Sdu = 1./ru; Sdv = 1./rv;

		S_scale_bndr (bound,Sdu,Sdv);
	}

	return (0);
}

/*********************************************************************
*********************************************************************/
static void S_reset_uvtrans (bound)
UM_srf_boundary *bound;
{
	if (Sluvtrans)
	{
		S_scale_bndr (bound,Sru,Srv);
		Sluvtrans = UU_FALSE;
	}
}

/*********************************************************************
*********************************************************************/
UU_LOGICAL ncl_scaled()
{
	return (Sluvtrans);
}

/*********************************************************************
*********************************************************************/
static void S_scale_uv (uvp)
UM_2Dcoord uvp;
{
	uvp[0] *= Sdu;
	uvp[1] *= Sdv;
}

/*********************************************************************
*********************************************************************/
void ncl_rescale_uv (uvp)
UM_2Dcoord uvp;
{
	uvp[0] *= Sru;
	uvp[1] *= Srv;
}

/*********************************************************************
**    E_FUNCTION: ncl_insert_uvpt (wpt,w)
**       Insertion sort.
**    PARAMETERS
**       INPUT  :
**          wpt     - ordered list of reals
**          w       - number to insert
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_insert_uvpt (wpt,w)
UU_LIST *wpt;
UU_REAL w;
{
	UU_REAL *z;
	int i,n;

	n = wpt->cur_cnt;
	z = (UU_REAL *) UU_LIST_ARRAY (wpt);

	for (i = 0; i < n-1; i++)
	{
		if (w >= z[i] && w < z[i+1])
		{
			if (w - z[i] < 0.0005)
				z[i] = w;
			else if ((i < n-2) && (z[i+1] - w < 0.0005))
				z[i+1] = w;
			else
				uu_list_insert (wpt,i+1,&w);
			return;
		}
	}

	return;
}

/*********************************************************************
**  FUNCTION : ncl_add_extrema (bndr,nu,upt,nv,vpt)
**     Insert "spike" values into the lists of parameters.
**  PARAMETERS
**     INPUT  :
**          bndr  - surface boundary
**          upt,vpt  - the initial lists of parameters
**          nu,nv    - number of points in the lists of parameters
**     OUTPUT :
**          upt,vpt  - the fixed lists of parameters
**          nu,nv    - number of points in the lists of parameters
**  RETURNS      : none
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
static void ncl_add_extrema (bndr,nu,upt,nv,vpt)
UM_srf_boundary *bndr;
UU_LIST *upt,*vpt;
int *nu,*nv;
{
	int i,j,np,np1,i1;
	UM_coord *pp;
	UM_2Dcoord v,v0,d;
	UU_LOGICAL horz0,vert0,horz,vert;

	np = bndr->np[0];
	if (np/20 + 2 < *nu + *nv) return;

	np1 = np-1;
	pp = (UM_coord *) UU_LIST_ARRAY (bndr->uvpts);
/*
..... Insert horizontal and vertical spikes into the lists
..... A horizontal spike occurs when two horizontal (with a slope
..... less than 1/2) boundary segments make a sharp angle.
*/
	for (j = 0; j < 2; j++)
	{
		v0[j] = pp[1][j] - pp[0][j];
		d[j] = fabs(v0[j]);
	}
	horz0 = (d[0] > 2.*d[1] && 32*d[0] > 1);
	vert0 = (d[1] > 2.*d[0] && 32*d[1] > 1);

	for (i = 1; i < np; i++)
	{
		i1 = (i+1)%np1;
		if (i == np) i = 1;
		for (j = 0; j < 2; j++)
		{
			v[j] = pp[i1][j] - pp[i][j];
			d[j] = fabs(v[j]);
		}
		horz = (d[0] > 2.*d[1] && 32*d[0] > 1);
		vert = (d[1] > 2.*d[0] && 32*d[1] > 1);

		if (horz0 && horz && v0[0]*v[0] < 0)
			ncl_insert_uvpt (vpt, pp[i][1]);
		else if (vert0 && vert && v0[1]*v[1] < 0)
			ncl_insert_uvpt (upt, pp[i][0]);
		horz0 = horz; vert0 = vert;
		for (j = 0; j < 2; j++)	v0[j] = v[j];
	}

	*nu = upt->cur_cnt;
	*nv = vpt->cur_cnt;

	return;
}

/*********************************************************************
**  I_FUNCTION : S_get_4pts (srf,tfmat,pts,i,j,vv,uu)
**     Evaluate surface at four panel corners.
**  PARAMETERS
**     INPUT  :
**          srf      - pointer to surface entity
**          tfmat    - surface matrix
**          tol      - tolerance
**          vv       - array of V-parameters
**          i        - current position in the vv array
**          uu       - array of U-parameters
**          j        - current position in the uu array
**     OUTPUT :
**          pts      - surface points at the 4 corners
**  RETURNS      :   UU_SUCCESS / UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
static int S_get_4pts (srf,tfmat,pts,i,j,vv,uu)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
int i,j;
UU_REAL *uu,*vv;
UM_coord pts[];
{
	int status;
	UU_REAL u0,v0,u1,v1;

	v0 = vv[i];
	if (Sluvtrans) v0 *= Srv;
	v1 = vv[i+1];
	if (Sluvtrans) v1 *= Srv;
	u0 = uu[j];
	if (Sluvtrans) u0 *= Sru;
	u1 = uu[j+1];
	if (Sluvtrans) u1 *= Sru;

	status = uc_evsrf(UM_POINT,u0,v0,srf,tfmat, &Sevsrf);
	if (status == UU_SUCCESS)
	{
		um_vctovc (Sevsrf.sp,pts[0]);
		status = uc_evsrf(UM_POINT,u1,v0,srf,tfmat, &Sevsrf);
		if (status == UU_SUCCESS)
		{
			um_vctovc (Sevsrf.sp,pts[1]);
			status = uc_evsrf(UM_POINT,u0,v1,srf,tfmat, &Sevsrf);
			if (status == UU_SUCCESS)
			{
				um_vctovc (Sevsrf.sp,pts[2]);
				status = uc_evsrf(UM_POINT,u1,v1,srf,tfmat, &Sevsrf);
				if (status == UU_SUCCESS)
				{
					um_vctovc (Sevsrf.sp,pts[3]);
				}
			}
		}
	}

	return (status);
}

/*********************************************************************
**  I_FUNCTION : S_fix_flatness (srf,tfmat,pts,i,j,vv,uu,upt,vpt,tol)
**     Refine the UV grid, if necessary, to assure each panel is flat.
**  PARAMETERS
**     INPUT  :
**          srf      - pointer to surface entity
**          tfmat    - surface matrix
**          tol      - tolerance
**          upt,vpt  - initial lists of parameters
**          nu,nv    - number of points in the lists of parameters
**     OUTPUT :
**          upt,vpt  - changed lists of parameters
**          nu,nv    - number of points in the lists of parameters
**  RETURNS      :   UU_SUCCESS / UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
static int S_fix_flatness (srf,tfmat,pts,i,j,vv,uu,upt,vpt,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_LIST *upt,*vpt;
int i,j;
UU_REAL *uu,*vv,tol;
UM_coord pts[];
{
	int k,status,ndu,ndv;
	UU_REAL u0,v0,u1,v1,du,dv,uc0,vc0,uc1,vc1,duk,dvk;
	UU_LOGICAL lflat;

	lflat = UU_FALSE;

	ndu = ndv = 1;
	u0 = uu[j]; u1 = uu[j+1];
	v0 = vv[i]; v1 = vv[i+1];
	duk = du = u1 - u0;
	dvk = dv = v1 - v0;

	uc0 = u0; uc1 = u1;
	vc0 = v0; vc1 = v1;
	if (Sluvtrans)
	{
		uc0 *= Sru; vc0 *= Srv;
		uc1 *= Sru; vc1 *= Srv;
	}

	for (k = 0; k < 10 && !lflat; k++)
	{
		if (duk >= dvk)
		{
			ndu++;
			duk = du/ndu;

			u1 = u0 + duk;
			uc1 = u1;
			if (Sluvtrans) uc1 *= Sru;

			status = uc_evsrf(UM_POINT,uc1,vc0,srf,tfmat, &Sevsrf);
			if (status != UU_SUCCESS) return (status);
			um_vctovc (Sevsrf.sp,pts[1]);
		}
		else
		{
			ndv++;
			dvk = dv/ndv;

			v1 = v0 + dvk;
			vc1 = v1;
			if (Sluvtrans) vc1 *= Srv;

			status = uc_evsrf(UM_POINT,uc0,vc1,srf,tfmat, &Sevsrf);
			if (status != UU_SUCCESS) return (status);
			um_vctovc (Sevsrf.sp,pts[2]);
		}

		status = uc_evsrf(UM_POINT,uc1,vc1,srf,tfmat, &Sevsrf);
		if (status != UU_SUCCESS) return (status);
		um_vctovc (Sevsrf.sp,pts[3]);

		lflat = um_4pts_plane (pts,tol);
	}

	if (lflat)
	{
		if (ndu > 1)
		{
			uu_list_insert (upt,j+1,&u1);
		}
		if (ndv > 1)
		{
			uu_list_insert (vpt,i+1,&v1);
		}
		return (UU_SUCCESS);
	}
	else
		return (UU_FAILURE);
}

/*********************************************************************
**  I_FUNCTION : S_assure_flatness (srf,tfmat,nu,upt,nv,vpt,tol)
**     Refine the UV grid, if necessary, to assure each panel is flat.
**  PARAMETERS
**     INPUT  :
**          srf      - pointer to surface entity
**          tfmat    - surface matrix
**          tol      - tolerance
**          upt,vpt  - initial lists of parameters
**          nu,nv    - number of points in the lists of parameters
**     OUTPUT :
**          upt,vpt  - changed lists of parameters
**          nu,nv    - number of points in the lists of parameters
**  RETURNS      :
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
static void S_assure_flatness (srf,tfmat,nu,upt,nv,vpt,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_LIST *upt,*vpt;
int *nu,*nv;
UU_REAL tol;
{
	int i,j,nu1,nv1,status;
	UM_coord pts[4];
	UU_REAL *uu,*vv;
	UU_LOGICAL lflat;

	uu = (UU_REAL *)UU_LIST_ARRAY(upt);
	vv = (UU_REAL *)UU_LIST_ARRAY(vpt);
	nu1 = upt->cur_cnt;
	nv1 = vpt->cur_cnt;

	for (i = 0; i < nv1-1; i++)
	{
		for (j = 0; j < nu1-1; j++)
		{
			status = S_get_4pts (srf,tfmat,pts,i,j,vv,uu);
			if (status != UU_SUCCESS) return;

			lflat = um_4pts_plane (pts,tol);

			if (!lflat)
			{
				status = S_fix_flatness (srf,tfmat,pts,i,j,vv,uu,upt,vpt,tol);
				if (status == 0)
				{

					uu = (UU_REAL *)UU_LIST_ARRAY(upt);
					vv = (UU_REAL *)UU_LIST_ARRAY(vpt);
					nu1 = upt->cur_cnt;
					nv1 = vpt->cur_cnt;
				}
			}
		}
	}

	*nu = nu1;
	*nv = nv1;
}

/*********************************************************************
**  I_FUNCTION     : int S_ev_uvcrv (cv,t)
**      Current boundary curve evaluator.
** PARAMETERS
**          cv    - boundary curve
**          t     - curve parameter
**       OUTPUT :
**          Sevcv  - evaluated curve
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_ev_uvcrv (cv,t)
UU_REAL t;
struct NCL_fixed_databag *cv;
{
	int istat;

	istat = um_ev7_uvcrv (UM_POINT, cv, Sbplm, t, &Sevcv);

	if (Sluvtrans) S_scale_uv (Sevcv.cp);

	return (istat);
}

/*********************************************************************
**    I_FUNCTION     : int S_dis (t,cv,d)
**      Calculate the (signed) distance from a curve point to the current
**      grid line
**   PARAMETERS
**          cv    - boundary curve
**          t     - curve parameter
**       OUTPUT :
**          d  - distance
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_dis (t,cv,d)
UU_REAL t,*d;
struct NCL_fixed_databag *cv;
{
	int istat;

	istat = S_ev_uvcrv (cv, t);
	*d = Sevcv.cp[Six2] - Sy;

	return (istat);
}

/*********************************************************************
**    S_FUNCTION     : int S_adis (t,cv,ad)
**      Calculate the (absolute) distance from a curve point to the current
**      grid line. Used as the value function for the uu_brent call.
**   PARAMETERS
**          cv    - boundary curve
**          t     - curve parameter
**       OUTPUT :
**          ad  - distance
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_adis (t,cv,ad)
UU_REAL t,*ad;
struct NCL_fixed_databag *cv;
{
	int istat;
	UU_REAL d;

	istat = S_dis (t,cv,&d);
	if (istat == UU_SUCCESS)
		*ad = fabs(d);

	return (istat);
}

/*********************************************************************
**  I_FUNCTION : S_refine_isec (cv,x,t,t0,t1,d0,d1)
**    Find one intersection of a boundary curve segment with a u- or v-line.
**    We denote the constant parameter as y (v for a u-line, u for a v-line),
**    and the variable parameter as x.
**
** PARAMETERS
**          cv    - boundary curve
**          t0,t1 - segment endpoints parameters on the boundary curve
**          d0,d1 - distances from segment endpoints to the y=Sy line
**          bndr  - surface boundary
**       OUTPUT :
**          x   - the intersection coordinate
**          t   - the intersection boundary curve parameter
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_refine_isec (cv,x,t,t0,t1,d0,d1)
struct NCL_fixed_databag *cv;
UU_REAL *x,*t,t0,t1,d0,d1;
{
	int status,k;
	UU_REAL ad0,ad1,di,adi,ti,dmin,tmin,tol,bd0,bd1,s0,s1,si,bdi,ri;

	tol = 1.e-5;
	ad0 = fabs(d0);
	ad1 = fabs(d1);

	ti = *t;
	status = S_dis (ti,cv,&di);
	if (status == UU_SUCCESS)
	{
		adi = fabs(di);

		if (adi >= ad1 || adi >= ad0)
		{
			bd0 = ad0; bd1 = ad1; bdi = adi;
			s0 = t0; s1 = t1; si = ti;

			for (k = 0; k < 20; k++)
			{
				if (bd0 >= bd1)
				{
					bd0 = bdi; s0 = si;
				}
				else
				{
					bd1 = bdi; s1 = si;
				}
				si = (s0*bd1 + s1*bd0)/(bd0 + bd1);

				status = S_dis (si,cv,&ri);
				if (status == UU_SUCCESS)
				{
					bdi = fabs(ri);
					if (bdi < ad0 && bdi < ad1)
					{
						*t = ti = si; adi = bdi;
						break;
					}
				}
			}
		}

		if (adi > tol)
		{
			if (adi < ad0 && adi < ad1)
			{
				status = uu_brent (t0,ti,t1,&tmin,&dmin,&S_adis,cv,tol);

				if (status == UU_SUCCESS)
				{
					status = S_ev_uvcrv (cv,tmin);
					if (status == UU_SUCCESS)
					{
						*x = Sevcv.cp[Six1];
						*t = tmin;
					}
				}
			}
			else
				status = UU_FAILURE;
		}
		else
		{
			*x = Sevcv.cp[Six1];
		}
	}

	if (status != UU_SUCCESS) S_use_bndr_curves = UU_FALSE;
}

/*********************************************************************
**  I_FUNCTION : S_refine_bndr2 (lpanel,bs,tfmat,ib,np0,cv,bx0,bx1,bndr)
**    Insert one u- or v-line a UV boundary curve.
**
** PARAMETERS
**          lpanel - if TRUE, insert only points within a specified range
**          bs    - pointer to base surface
**          tfmat - surface matrix
**          cv    - boundary curve
**          bx0,bx1  - range for "x" parameter,
**                     needs to be checked if lpanel is TRUE
**          ib    - number of boundary curve
**          np0   - starting number in the boundary list
**          bndr  - surface boundary
**       OUTPUT :
**          bndr  - refined boundary
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_refine_bndr2 (lpanel,bs,tfmat,ib,np0,cv,bx0,bx1,bndr)
UU_LOGICAL lpanel;
struct NCL_fixed_databag *bs,*cv;
UM_transf tfmat;
int ib,np0;
UU_REAL bx0,bx1;
UM_srf_boundary *bndr;
{
	int j,npt;
	UM_coord *pp,pi,*pts,pti;
	UU_REAL a0,b0,a1,b1,y0,y1,x,t0,t1,t,d0,d1,alp,tol,tolsq;
	UU_REAL ui,vi;

	tol = 1.e-5;
	tolsq = tol*tol;

	pp = (UM_coord *) UU_LIST_ARRAY (bndr->uvpts);
	pp += np0;
	if (Sneedbndr)
	{
		pts = (UM_coord *) UU_LIST_ARRAY (bndr->cvpts);
		pts += np0;
	}
	npt = bndr->np[ib] - 1;
/*
..... go over UV boundary segments, find those intersecting a grid line (U or V),
..... insert the intersection
*/
	for (j = 0; j < npt; j++)
	{
		a0 = pp[j][Six1]; a1 = pp[j+1][Six1];
		b0 = pp[j][Six2]; b1 = pp[j+1][Six2];
		if (b0 <= b1)
		{
			y0 = b0 + tol; y1 = b1 - tol;
		}
		else
		{
			y0 = b1 + tol; y1 = b0 - tol;
		}
		if (Sy > y0 && Sy < y1)
		{
/*
..... first intersect the UV segment with the grid line, then refine
*/
			alp = (Sy - b0)/(b1 - b0);

			x = a0 + (a1 - a0)*alp;

			if (lpanel && (x < bx0 || x > bx1)) continue;

			t0 = pp[j][2]; t1 = pp[j+1][2];
			t = t0 + (t1 - t0)*alp;

			if (fabs(t1-t0) > tol)
			{
				d0 = b0 - Sy;
				d1 = b1 - Sy;

				if (S_use_bndr_curves)
					S_refine_isec (cv,&x,&t,t0,t1,d0,d1);
			}

			pi[Six1] = x; pi[Six2] = Sy;
			pi[2] = t;

			if (Sneedbndr)
			{
				ui = pi[0];
				if (Sluvtrans) ui *= Sru;
				vi = pi[1];
				if (Sluvtrans) vi *= Srv;

				uc_evsrf(UM_POINT,ui,vi,bs,tfmat, &Sevsrf);
				um_vctovc (Sevsrf.sp,pti);
			}

			d0 = UM_SQDIS_2D(pi,pp[j]);
			d1 = UM_SQDIS_2D(pi,pp[j+1]);
			if (d0 < tolsq)
			{
				um_vctovc (pi,pp[j]);
				if (Sneedbndr)
					um_vctovc (pti,pts[j]);
			}
			else if (d1 < tolsq)
			{
				um_vctovc (pi,pp[j+1]);
				if (Sneedbndr)
					um_vctovc (pti,pts[j+1]);
			}
			else
			{
				uu_list_insert (bndr->uvpts,np0+j+1,pi);

				pp = (UM_coord *) UU_LIST_ARRAY (bndr->uvpts);
				pp += np0;
				if (Sneedbndr)
				{
					uu_list_insert (bndr->cvpts,np0+j+1,pti);

					pts = (UM_coord *) UU_LIST_ARRAY (bndr->cvpts);
					pts += np0;
				}

				npt++; j++;
			}
		}
	}

	npt++;
	if (npt > bndr->np[ib])
		bndr->np[ib] = npt;
}

/*********************************************************************
**  I_FUNCTION : S_refine_bndr1 (bs,tfmat,ib,np0,cv,nu,uu,nv,vv,bndr)
**    Insert grid points into one boundary curve.
**
** PARAMETERS
**          bs    - pointer to base surface
**          tfmat - surface matrix
**          cv    - boundary curve
**          ib    - number of boundary curve
**          np0   - starting number in the boundary list
**          nu,nv - number of points in u,v
**          uu,vv - arrays of UV points defining the grid
**          bndr  - surface boundary
**       OUTPUT :
**          bndr  - refined boundary
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_refine_bndr1 (bs,tfmat,ib,np0,cv,nu,uu,nv,vv,bndr)
struct NCL_fixed_databag *bs,*cv;
UM_transf tfmat;
int ib,np0,nu,nv;
UU_REAL *uu,*vv;
UM_srf_boundary *bndr;
{
	int i,k,nw;
	UU_REAL bx0,bx1;
	UU_REAL *ww;

/*
..... refine this boundary curve against each U-line and V-line
*/
	bx0 = 0.; bx1 = 1.;

	if (nv >= nu)
		Six1 = 0;
	else
		Six1 = 1;

	for (k = 0; k < 2; k++)
	{
		Six2 = 1 - Six1;

		if (Six1 == 0)
		{
			nw = nv; ww = vv;
		}
		else
		{
			nw = nu; ww = uu;
		}

		for (i = 1; i < nw - 1; i++)
		{
			Sy = ww[i];
			S_refine_bndr2 (UU_FALSE,bs,tfmat,ib,np0,cv,bx0,bx1,bndr);
		}

		Six1 = Six2;
	}
}

/*********************************************************************
**  I_FUNCTION : S_refine_bndr (srf,bs,tfmat,nu,uu,nv,vv,bndr)
**    Insert grid points into boundary.
**
** PARAMETERS
**          bs    - pointer to base surface
**          srf   - pointer to trimmed surface
**          tfmat - surface matrix
**          nu,nv - number of points in u,v
**          uu,vv - arrays of UV points defining the grid
**          bndr  - surface boundary
**       OUTPUT :
**          bndr  - refined boundary
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_refine_bndr (srf,bs,tfmat,nu,uu,nv,vv,bndr)
struct NCL_fixed_databag *srf,*bs;
UM_transf tfmat;
int nu,nv;
UU_REAL *uu,*vv;
UM_srf_boundary *bndr;
{
	int ib,nb,np0,status;
	struct NCL_fixed_databag cv;

	S_use_bndr_curves = UU_TRUE;

	ncl_trimsrf_get_fixed (srf,&nb,Sbplm);
/*
..... refine each boundary curve
*/
	for (ib = np0 = 0; ib < nb; ib++)
	{
		status = ncl_tsf_get_boundary (srf,ib,&cv);
		if (status != UU_SUCCESS) return;

		uc_init_evcrvout (&cv, &Sevcv);
		S_refine_bndr1 (bs,tfmat,ib,np0,&cv,nu,uu,nv,vv,bndr);

		np0 += bndr->np[ib];
	}
}

/*********************************************************************
**  I_FUNCTION : S_refine_in_panel1 (bs,tfmat,ib,np0,cv,u0,u1,um,v0,v1,vm,bndr)
**    Refine one boundary curve within a split panel.
**
** PARAMETERS
**          bs    - pointer to base surface
**          tfmat - surface matrix
**          cv    - boundary curve
**          ib    - number of boundary curve
**          np0   - starting number in the boundary list
**          u0,u1,v0,v1 - the panel
**          um,vm - split U and V values (actually the middle)
**          bndr  - surface boundary
**       OUTPUT :
**          bndr  - refined boundary
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void	S_refine_in_panel1 (bs,tfmat,ib,np0,cv,u0,u1,um,v0,v1,vm,bndr)
struct NCL_fixed_databag *bs,*cv;
UM_transf tfmat;
int ib,np0;
UU_REAL u0,u1,um,v0,v1,vm;
UM_srf_boundary *bndr;
{
	int k;
	UU_REAL bx0,bx1;
/*
..... refine this boundary curve against each U-line and V-line
*/
	Six1 = 0; Sy = vm;
	bx0 = u0; bx1 = u1;

	for (k = 0; k < 2; k++)
	{
		Six2 = 1 - Six1;

		S_refine_bndr2 (UU_TRUE,bs,tfmat,ib,np0,cv,bx0,bx1,bndr);
		Six1 = 1; Sy = um;
		bx0 = v0; bx1 = v1;
	}
}

/*********************************************************************
**  I_FUNCTION : S_refine_in_panel (srf,bs,tfmat,u0,u1,um,v0,v1,vm,bndr)
**    Refine the boundary within a split panel.
**
** PARAMETERS
**          bs    - pointer to base surface
**          srf   - pointer to trimmed surface
**          tfmat - surface matrix
**          u0,u1,v0,v1 - the panel
**          um,vm - split U and V values (actually the middle)
**          bound - surface boundary
**       OUTPUT :
**          bound  - refined boundary
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void	S_refine_in_panel (srf,bs,tfmat,u0,u1,um,v0,v1,vm,bndr)
struct NCL_fixed_databag *srf,*bs;
UM_transf tfmat;
UU_REAL u0,u1,um,v0,v1,vm;
UM_srf_boundary *bndr;
{
	int ib,nb,np0,status;
	struct NCL_fixed_databag cv;

/*
..... refine each boundary curve
*/
	nb = bndr->nb;
	for (ib = np0 = 0; ib < nb; ib++)
	{
		status = ncl_tsf_get_boundary (srf,ib,&cv);
		if (status != UU_SUCCESS) return;

		uc_init_evcrvout (&cv, &Sevcv);
		S_refine_in_panel1 (bs,tfmat,ib,np0,&cv,u0,u1,um,v0,v1,vm,bndr);

		np0 += bndr->np[ib];
	}
}

/*********************************************************************
** FUNCTION : int S_add_grid_bndr (srf,tfmat,nu,uu,nv,vv,bndr)
**
**    Store an untrimmed surface boundary, as defined by a UV grid.
**
** PARAMETERS
**          srf   - pointer to surface record
**          tfmat - transformation matrix of input surface
**          nu,nv - number of points in u,v
**          uu,vv - arrays of UV points defining the grid
**       OUTPUT :
**          bndr  - filled boundary structure
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_add_grid_bndr (srf,tfmat,nu,uu,nv,vv,bndr)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
int nu,nv;
UU_REAL *uu,*vv;
UM_srf_boundary *bndr;
{
	int status, nu1,nv1,i,j;
	UU_REAL delu,delv;
	UM_coord uv,pp,*pts;

	nu1 = nu - 1; nv1 = nv - 1;
	if (nu1 < 1 || nv1 < 1) return (UU_FAILURE);
	delu = 4*(uu[nu1] - uu[0]);
	delv = 4*(vv[nv1] - vv[0]);
	if (delu < UM_DFUZZ || delv < UM_DFUZZ) return (UU_FAILURE);

	status = UU_SUCCESS;
	UU_LIST_EMPTY (bndr->cvpts);
/*
..... store v = 0 side - forward
*/
	for (i = 0; i < nu1; i++)
	{
		uv[0] = uu[i]; uv[1] = vv[0];
		uv[2] = uu[i]/delu;
		status = uc_evsrf(UM_POINT,uv[0],uv[1],srf,tfmat, &Sevsrf);
		if (status != UU_SUCCESS) return (status);
		uu_list_push (bndr->uvpts,&uv);
		uu_list_push (bndr->cvpts,&Sevsrf.sp);
	}
/*
..... store u = 1 side - forward
*/
	for (j = 0; j < nv1; j++)
	{
		uv[0] = uu[nu1]; uv[1] = vv[j];
		uv[2] = 0.25 + vv[j]/delv;
		status = uc_evsrf(UM_POINT,uv[0],uv[1],srf,tfmat, &Sevsrf);
		if (status != UU_SUCCESS) return (status);
		uu_list_push (bndr->uvpts,&uv);
		uu_list_push (bndr->cvpts,&Sevsrf.sp);
	}
/*
..... store v = 1 side - reversed
*/
	for (i = nu1; i > 0; i--)
	{
		uv[0] = uu[i]; uv[1] = vv[nv1];
		uv[2] = 0.5 + (uu[nu1] - uu[i])/delu;
		status = uc_evsrf(UM_POINT,uv[0],uv[1],srf,tfmat, &Sevsrf);
		if (status != UU_SUCCESS) return (status);
		uu_list_push (bndr->uvpts,&uv);
		uu_list_push (bndr->cvpts,&Sevsrf.sp);
	}
/*
..... store u = 0 side - reversed
*/
	for (j = nv1; j > 0; j--)
	{
		uv[0] = uu[0]; uv[1] = vv[j];
		uv[2] = 0.75 + (vv[nv1] - vv[j])/delv;
		status = uc_evsrf(UM_POINT,uv[0],uv[1],srf,tfmat, &Sevsrf);
		if (status != UU_SUCCESS) return (status);
		uu_list_push (bndr->uvpts,&uv);
		uu_list_push (bndr->cvpts,&Sevsrf.sp);
	}

	uv[0] = uu[0]; uv[1] = vv[0];
	uv[2] = 1.;
	uu_list_push (bndr->uvpts,&uv);
	pts = (UM_coord *)UU_LIST_ARRAY(bndr->cvpts);
	um_vctovc (pts[0],pp);
	uu_list_push (bndr->cvpts,&pp);

	bndr->np[0] = UU_LIST_LENGTH (bndr->uvpts);

	return (status);
}

/*********************************************************************
** FUNCTION : UU_LOGICAL S_check_refine_within() (srf,tfmat,u0,u1,polygon,tol,mid)
**   Check if middle point between polygon two different u points within given tolerance
**
** PARAMETERS
**     INPUT  :
**          srf		- pointer to surface record
**          tfmat	   - transformation matrix of input surface
**          u0,u1    - the panel
**          polygon  - list structure (intialized) to use
**			   tol		- tolerance
**          dir      - 0 = u-direction; 1 = v-direction
**	   OUTPUT :
**			mid	- the mid value to divide the panel
**    RETURNS      :
**          UU_TRUE if within tolerance; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_check_refine_within (srf,tfmat,u0,u1,polygon,tol,mid,dir)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_REAL u0,u1;
UU_LIST *polygon;
UU_REAL tol, *mid;
int dir;
{
	UU_LOGICAL lWithin = UU_FALSE;
	int status = UU_SUCCESS;

	int np;
	UM_2Dcoord *pp;

	UU_REAL tolsq = tol*tol;
	UU_REAL mmid, um12,vm12,min34;
	UM_coord pt1,pt2,pt3,ptm;
	UM_vector vc12;
	UU_REAL um_sqdis_from_line();
	UU_REAL dissq,dissq12,dissq13;
	UU_REAL ut1,ut2,vt1,vt2,ut3,vt3;
	int u=0,v=1;

	struct UM_evsrfout *evsrf;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	
	status = uc_init_evsrfout(srf, evsrf);
	if (status != UU_SUCCESS)
		return (status);

	np = polygon->cur_cnt;
	pp = (UM_2Dcoord *) UU_LIST_ARRAY (polygon);

/*
.....Change direction
*/
	if (dir == 0)
	{
		u = 1;
		v = 0;
	}

/*
... Get min u
*/
	if (np == 3)		
		min34 = MIN3( pp[0][u], pp[1][u], pp[2][u]);
	else
		min34 = MIN4( pp[0][u], pp[1][u], pp[2][u], pp[3][u]);
/*
... Get the two ploygon point
*/
	if (np == 3)
	{
		if (pp[1][0] != pp[2][0])
		{
			ut1 = pp[1][u];
			vt1 = pp[1][v]; 
			ut2 = pp[2][u];
			vt2 = pp[2][v];
			ut3 = pp[0][u];
			vt3 = pp[0][v];
		}
		else if (pp[0][0] != pp[1][0])
		{
			ut1 = pp[0][u];
			vt1 = pp[0][v]; 
			ut2 = pp[1][u];
			vt2 = pp[1][v];
			ut3 = pp[2][u];
			vt3 = pp[2][v];
		}
	}
	else
	{
		if (pp[1][0] != pp[2][0])
		{
			ut1 = pp[1][u];
			vt1 = pp[1][v]; 
			ut2 = pp[2][u];
			vt2 = pp[2][v];
			ut3 = pp[3][u];
			vt3 = pp[3][v];
		}
		else if (pp[3][0] != pp[0][0])
		{			
			ut1 = pp[3][u];
			vt1 = pp[3][v]; 
			ut2 = pp[0][u];
			vt2 = pp[0][v];
			ut3 = pp[1][u];
			vt3 = pp[1][v];
		}
		else if (pp[2][0] != pp[3][0])
		{
			ut1 = pp[2][u];
			vt1 = pp[2][v]; 
			ut2 = pp[3][u];
			vt2 = pp[3][v];
			ut3 = pp[0][u];
			vt3 = pp[0][v];
		}
		else if (pp[0][0] != pp[1][0])
		{
			ut1 = pp[0][u];
			vt1 = pp[0][v]; 
			ut2 = pp[1][u];
			vt2 = pp[1][v];
			ut3 = pp[2][u];
			vt3 = pp[2][v];
		}
	}
	
	status = uc_evsrf(UM_POINT, ut1, vt1, srf, tfmat, evsrf);
	um_vctovc (evsrf->sp, pt1);
	status = uc_evsrf(UM_POINT, ut2, vt2, srf, tfmat, evsrf);
	um_vctovc (evsrf->sp, pt2); 
	status = uc_evsrf(UM_POINT, ut3, vt3, srf, tfmat, evsrf);
	um_vctovc (evsrf->sp, pt3); 

/*
... Get the middle point
*/
	um12 = 0.5 * (ut1 + ut2);
	vm12 = 0.5 * (vt1 + vt2);

	status = uc_evsrf(UM_POINT, um12, vm12, srf, tfmat, evsrf);
	um_vctovc (evsrf->sp, ptm);
	uu_free(evsrf);

/*
... Check if middle point within tolerance
*/
	um_vcmnvc(pt2, pt1, vc12);
	dissq = um_sqdis_from_line (ptm,pt1,vc12);
	if (dissq < 0.25 * tolsq)
		lWithin = UU_TRUE;

/*
... Get the mid to divide the panel
*/
	mmid = MIN2(ut2, ut1);
	if (mmid > min34 || np == 3)
		mmid = MAX2(ut2, ut1);

	*mid = mmid;

	dissq12 = um_sqdis(pt1,pt2);
	dissq13 = um_sqdis(pt1,pt3);

/*
... Check tiny triangle or not
*/
	if (dissq12 < 0.25 * tolsq && dissq13 < 0.25 * tolsq)
		return UU_TRUE;

/*
... Check umid valid or not
*/
	if ((u1 - mmid) < UM_DFUZZ || (mmid - u0) < UM_DFUZZ)
		return UU_FALSE;

	return lWithin;
}

/*********************************************************************
** FUNCTION : int S_get_boundary (srf,tfmat,bound,tol,lmallocd)
**     Get the trimmed surface boundary.
**  PARAMETERS
**     INPUT  :
**        srf   - pointer to surface entity
**        tfmat - surface matrix
**        tol - tolerance
**        lmallocd - if not NULL, allocate memory if more than one boundary curve,
**                   set to TRUE if allocated
**     OUTPUT :
**        bound  - surface boundary
**    RETURNS      :
**         1 iff had it stored; else UU_SUCCESS / UU_FAILURE for calculating
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_boundary (srf,tfmat,bound,tol,lmallocd)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_srf_boundary *bound;
UU_REAL tol;
UU_LOGICAL *lmallocd;
{
	int status,istat;

	if (ncl_setver(96))
	{
		bound->np = UU_NULL;
		bound->ummx = bound->vmmx = UU_NULL;
		*lmallocd = UU_TRUE;

		status = ncl_get_bndrlist96 (srf,bound);
		if (status == UU_SUCCESS) return (status);

		bound->toler = ncl_get_boundary_toler ();

		status = ncl_set_boundary(srf,tfmat,bound,UU_NULL);
	}
	else
	{
		istat = ncl_get_surf_boundary (UU_TRUE,srf,tfmat,bound,tol,lmallocd,1);

		if (istat == 1)
			status = UU_SUCCESS;
		else
			status = istat;
	}

	return (status);
}

/*********************************************************************
**  I_FUNCTION : S_len_ratio (u0,u1,v0,v1,polygon,srf,tfmat,r)
**    Finds the ratio of XYZ distances in u and v directions.
**
** PARAMETERS
**          u0,u1,v0,v1 - corner points
**          polygon     - XYZ polygon
**          lnnum       - current line number for NCL
**          srf         - current surface
**          tfmat       - tranformation matrix
**       OUTPUT :
**          r           - ratio of lengths
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_len_ratio(u0,u1,v0,v1,polygon,srf,tfmat,r)
UU_REAL u0,u1,v0,v1;
UU_REAL *r;
UM_transf tfmat;
struct NCL_fixed_databag *srf;
UU_LIST *polygon;
{
	int status = UU_SUCCESS;
	UU_REAL dist1,dist2;
	char sbuf[80];
	struct UM_evsrfout *evsrf,*evsrf2;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	evsrf2 = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));

	status = uc_init_evsrfout(srf, evsrf);
	status = uc_init_evsrfout(srf, evsrf2);

	uc_evsrf(UM_POINT, u0, v0, srf, tfmat, evsrf);
	uc_evsrf(UM_POINT, u1, v0, srf, tfmat, evsrf2);
	dist1 = um_dcccc(evsrf->sp,evsrf2->sp);

	uc_evsrf(UM_POINT, u0, v0, srf, tfmat, evsrf);
	uc_evsrf(UM_POINT, u0, v1, srf, tfmat, evsrf2);
	dist2 = um_dcccc(evsrf->sp,evsrf2->sp);

	uu_free(evsrf);
	uu_free(evsrf2);

	*r = dist1/dist2;
	return;
}

/*********************************************************************
**  I_FUNCTION : S_print_rect (u0,1u1,v0,v1,polygon,srf,tfmat,lnnum)
**    Print rectangle and its corner points to debug file.  The 
**    resulting output gives a visual representation of the 
**    tesselation breaking the area into rectangles.
**
** PARAMETERS
**          u0,u1,v0,v1 - corner points
**          polygon     - XYZ polygon
**          lnnum       - current line number for NCL
**          srf         - current surface
**          tfmat       - tranformation matrix
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_print_rect(u0,u1,v0,v1,polygon,srf,tfmat,lnnum)
UU_REAL u0,u1,v0,v1;
int lnnum;
UM_transf tfmat;
struct NCL_fixed_databag *srf;
UU_LIST *polygon;
{
	int status = UU_SUCCESS;
	UM_2Dcoord *pp;
	char sbuf[80];
	struct UM_evsrfout *evsrf,*evsrf2;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	evsrf2 = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	pp = (UM_2Dcoord *) UU_LIST_ARRAY (polygon);
	
	status = uc_init_evsrfout(srf, evsrf);
	status = uc_init_evsrfout(srf, evsrf2);

	NclxDbgPstr("*stop");

	if (lnnum > 8)
	{
		NclxDbgPstr("remove/pt1,thru,pt4");
		sprintf(sbuf,"draft/modify=ln%d,thru,ln%d,color=green",1,lnnum-5);
		NclxDbgPstr(sbuf);
		sprintf(sbuf,"draft/modify=ln%d,thru,ln%d,color=yellow",lnnum-4,lnnum-1);
		NclxDbgPstr(sbuf);
	}
	else if (lnnum > 4)
	{
		NclxDbgPstr("remove/pt1,thru,pt4");
		sprintf(sbuf,"draft/modify=ln%d,thru,ln%d,color=yellow",1,4);
		NclxDbgPstr(sbuf);
	}
	else
	{
		NclxDbgPstr("draft/name=front,mode=wire");
		NclxDbgPstr("ubfn/151W1294-19.u");
		NclxDbgPstr("get/sf1163");
		NclxDbgPstr("draft/modify=sf1163,color=magnta");
		NclxDbgPstr("get/sf1168");
		NclxDbgPstr("draft/modify=sf1168,color=magnta");
		NclxDbgPstr("draft/modify,color=red");
	}

	uc_evsrf(UM_POINT, u0, v0, srf, tfmat, evsrf);
	uc_evsrf(UM_POINT, u1, v0, srf, tfmat, evsrf2);
	sprintf(sbuf,"pt%d=pt/%10.5f,%10.5f,%10.5f",1,evsrf->sp[0],evsrf->sp[1],evsrf->sp[2]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"pt%d=pt/%10.5f,%10.5f,%10.5f",2,evsrf2->sp[0],evsrf2->sp[1],evsrf2->sp[2]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"ln%d=line/%10.5f,%10.5f,%10.5f,%10.5f,%10.5f,%10.5f",lnnum,evsrf->sp[0],evsrf->sp[1],evsrf->sp[2],evsrf2->sp[0],evsrf2->sp[1],evsrf2->sp[2]);
	NclxDbgPstr(sbuf);
	uc_evsrf(UM_POINT, u1, v1, srf, tfmat, evsrf);
	sprintf(sbuf,"pt%d=pt/%10.5f,%10.5f,%10.5f",3,evsrf->sp[0],evsrf->sp[1],evsrf->sp[2]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"ln%d=line/%10.5f,%10.5f,%10.5f,%10.5f,%10.5f,%10.5f",lnnum+1,evsrf2->sp[0],evsrf2->sp[1],evsrf2->sp[2],evsrf->sp[0],evsrf->sp[1],evsrf->sp[2]);
	NclxDbgPstr(sbuf);
	uc_evsrf(UM_POINT, u0, v1, srf, tfmat, evsrf2);
	sprintf(sbuf,"pt%d=pt/%10.5f,%10.5f,%10.5f",4,evsrf2->sp[0],evsrf2->sp[1],evsrf2->sp[2]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"ln%d=line/%10.5f,%10.5f,%10.5f,%10.5f,%10.5f,%10.5f",lnnum+2,evsrf->sp[0],evsrf->sp[1],evsrf->sp[2],evsrf2->sp[0],evsrf2->sp[1],evsrf2->sp[2]);
	NclxDbgPstr(sbuf);
	uc_evsrf(UM_POINT, u0, v0, srf, tfmat, evsrf);
	sprintf(sbuf,"ln%d=line/%10.5f,%10.5f,%10.5f,%10.5f,%10.5f,%10.5f",lnnum+3,evsrf2->sp[0],evsrf2->sp[1],evsrf2->sp[2],evsrf->sp[0],evsrf->sp[1],evsrf->sp[2]);
	NclxDbgPstr(sbuf);

	uu_free(evsrf);
	uu_free(evsrf2);

	return;
}

/*********************************************************************
**  FUNCTION : int ncl_tessellate_surf (srf,tess)
**     Constructs triangle tessellation of a surface.
**  PARAMETERS
**     INPUT  :
**        srf   - pointer to surface entity
**     OUTPUT :
**        tess - tessellation
**  RETURNS      :
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int ncl_tessellate_surf (srf,tess)
struct NCL_fixed_databag *srf;
UM_tessellation *tess;
{
	UM_srf_boundary bound;
	UU_REAL ncl_get_boundary_toler();
	UU_REAL tol,btol;
  	UM_transf tfmat;
	int status;
	UM_tess_settype typ;
	int nupts, nvpts;
	UU_LOGICAL lmallocd;

/*
.....Initialize boundary storage
*/
	bound.nb = 0;
	Snp0[0] = Snp0[1] = 0;
	bound.np = Snp0;
	bound.ummx = &Summx;
	bound.vmmx = &Svmmx;

	uu_list_init (&Suvlst, sizeof(UM_coord), 0, 100);
	uu_list_init (&Slst1, sizeof(UM_coord), 0, 100);

	bound.uvpts = &Suvlst;
	bound.cvpts = &Slst1;

	lmallocd = UU_FALSE;

	ncl_get_tess_parms (&typ,&tol,&nupts,&nvpts);

	tess->toler = tol;
	btol = ncl_get_boundary_toler();
	if (tol <= 0. || btol <= 0.) return (UU_FAILURE);

	status = uc_retrieve_transf (srf->key, tfmat);
	if (status != UU_SUCCESS) goto Done;
/*
... get the boundary if the surface is trimmed
*/
	if (ncl_itsa_trimsrf(srf))
	{
		status = S_get_boundary (srf,tfmat,&bound,tol,&lmallocd);
		if (status != UU_SUCCESS || bound.nb <= 0) goto Done;
	}
	else
	{
		bound.toler = btol;
		bound.nb = 1;
		bound.np[0] = 5;
		bound.ummx[0][0] = 0.0;
		bound.vmmx[0][0] = 0.0;
		bound.ummx[0][1] = 1.;
		bound.vmmx[0][1] = 1.;
	}

	status = ncl_tess_surf (srf,tfmat,&bound,tess,tol,typ,nupts,nvpts);

Done:
	if (lmallocd)
	{
		uu_free (bound.np);
		uu_free (bound.ummx);
		uu_free (bound.vmmx);
	}
	uu_list_free (&Suvlst);
	uu_list_free (&Slst1);

	return (status);
}

/*********************************************************************
**  FUNCTION : int ncl_tess_surf (srf,tfmat,bndr,tess,tol,typ,nupts,nvpts)
**     Constructs triangle tessellation of a surface.
**  PARAMETERS
**     INPUT  :
**        srf   - pointer to surface entity
**        tfmat - surface matrix
**        bndr  - surface boundary
**        typ   - UM_TESS_TOLER - use only told;
**                UM_TESS_GRID - evolve npts points;
**                UM_TESS_BOTH - use told but evolve no less than
**                                   nupts x nvpts points;
**        nupts,nvpts  - (min) number of points per isoparametric curves
**     OUTPUT :
**        tess - tessellation
**  RETURNS      :
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int ncl_tess_surf (srf,tfmat,bndr,tess,tol,typ,nupts,nvpts)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_srf_boundary *bndr;
UM_tessellation *tess;
UU_REAL tol;
UM_tess_settype typ;
int nupts,nvpts;
{
	int nu,nv,status;

	nu = nv = 0;
	status = ncl_tess_surf1 (srf,tfmat,bndr,tess,NULLST,tol,typ,nupts,nu,nvpts,nv);

	return (status);
}

/*********************************************************************
**  FUNCTION : int ncl_tess_surf1 (srf,tfmat,bndr,tess,tol,typ,nupts,nvpts)
**     Constructs triangle tessellation of a surface.
**  PARAMETERS
**     INPUT  :
**        srf   - pointer to surface entity
**        tfmat - surface matrix
**        bndr  - surface boundary
**        typ   - UM_TESS_TOLER - use only told;
**                UM_TESS_GRID - evolve npts points;
**                UM_TESS_BOTH - use told but evolve no less than
**                                   nupts x nvpts points;
**        nupts,nvpts  - (min) number of points per isoparametric curves
**     OUTPUT :
**        tess - tessellation
**  RETURNS      :
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int ncl_tess_surf1 (srf,tfmat,bndr,tess,uvtes,tol,typ,nupts,nu0,nvpts,nv0)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_srf_boundary *bndr;
UM_tessellation *tess;
UU_LIST *uvtes;
UU_REAL tol;
UM_tess_settype typ;
int nupts,nvpts,nu0,nv0;
{
	struct NCL_fixed_databag bsrf,*bs;
	int nu,nv,status,np,mintri,minedges;
	UU_REAL *uu,*vv;
	UU_REAL tolsq = tol*tol;
	UU_LIST upt,vpt;
	UU_LOGICAL trimd,revsur;
	UM_tess1 tess1;
	UU_LOGICAL firstry,inits1,lv96;

	status = UU_SUCCESS;
	lv96 = ncl_setver(96);

	uu_list_init0 (&upt);
	uu_list_init0 (&vpt);
	uu_list_init0 (&Slst2);
	inits1 = UU_FALSE;
	Scvpts = bndr->cvpts;


	np = 0;
	trimd = (ncl_itsa_trimsrf(srf));
	if (trimd)
	{
		bs = &bsrf;
		bs->key = ((struct NCL_trimsf_rec *)srf)->bs_key;
		status = ncl_retrieve_data_fixed (bs);
		if (status != UU_SUCCESS) goto Done;
		np = bndr->np[0];
		if (np < 3 || (np == 3 && !lv96)) goto Done;

		if (!lv96)
		status = S_set_uvtrans (bndr);

		if (status == UU_SUCCESS)
		status = ncl_orient_boundary (bndr);

		if (status != UU_SUCCESS) goto Done;
	}
	else
	{
		bs = srf;
/*
..... enable UM_TESS_WATRLN logic below
*/
		if (bndr->np != UU_NULL)
			np = bndr->np[0];
		else
			np = 5;
	}

	ncl_get_sf_treflag (bs,&NCL_triansf,tol);

	mintri = 2; minedges = 5;

	if (np < 8 || NCL_triansf > 0)
	{
		mintri = 1; minedges = 3;
	}

	revsur = (bs->rel_num == NCL_REVSURF_REL);

	firstry = UU_FALSE;
	if (trimd && bndr->nb == 1)
	{
		if (np == 4)
		{
			S_reset_uvtrans(bndr);
			status = S_tess_1tri (srf,tfmat,bndr,tol, tess);
			goto Done;
		}
		firstry = UU_TRUE;
	}

	status = uu_list_init1 (&upt, sizeof(UU_REAL), 200, 200);
	if (status == UU_SUCCESS)
		status = uu_list_init1 (&vpt, sizeof(UU_REAL), 200, 200);
	if (status != UU_SUCCESS) goto Done;
	uu_list_init (&Slst2, sizeof(UM_coord), 0, 200);
	if (trimd && lv96)
	{
		inits1 = UU_TRUE;
		status = um_create_tess1 (&tess1,200);
		if (status != UU_SUCCESS) goto Done;
		tess1.key = srf->key;
		tess1.toler = tess->toler;
	}

Retry:
/*
..... divide the surface into (rectangular in UV-space) panels, according to
..... tolerance; the panels are then tessellated in UV-space - we assume that
..... any triangle inside a panel is within tolerance.
*/
	if (typ == UM_TESS_WATRLN)
	{
		if (np >= 50)
		{
			typ = UM_TESS_BOTH;
			nupts = nvpts = 5;
		}
		else
			typ = UM_TESS_TOLER;
	}

	nu = ncl_evolve_uvpts (revsur,bs,tfmat,bndr,nupts,nu0,1,tol,&upt,typ);
	if (nu < 2) goto Done;
	nv = ncl_evolve_uvpts (revsur,bs,tfmat,bndr,nvpts,nv0,2,tol,&vpt,typ);
	if (nv < 2) goto Done;

	if (trimd) ncl_add_extrema (bndr,&nu,&upt,&nv,&vpt);

	if (!lv96 && typ != UM_TESS_GRID)
		S_assure_flatness (bs,tfmat,&nu,&upt,&nv,&vpt,tol);

	uu = (UU_REAL *)UU_LIST_ARRAY(&upt);
	vv = (UU_REAL *)UU_LIST_ARRAY(&vpt);

	if (trimd)
	{
		if (lv96)
		{
			status = S_tess_rectangles_96 (bs,tfmat,nu,uu,nv,vv,bndr,&tess1,tol);
		}
		else
		{
			S_refine_bndr (srf,bs,tfmat,nu,uu,nv,vv,bndr);
			status = S_tess_rectangles (srf,bs,tfmat,nu,uu,nv,vv,bndr,tess,uvtes,tol);
		}

		if (status == UU_SUCCESS)
		{
			if ((lv96 && tess1.nedges < minedges) || (!lv96 && tess->ntri < mintri))
				status = UU_FAILURE;
		}
		if (status != UU_SUCCESS && firstry)
		{
/*
..... Maybe the tessellation fails because of the boundary orientation;
..... try to reverse the boundary (only if there is a single boundary curve).
*/
			firstry = UU_FALSE;
			if (lv96)
				um_clean_tess1 (&tess1);
			else
			{
				um_clean_tess (tess);
				if (uvtes != NULLST) UU_LIST_EMPTY(uvtes);
			}
			UU_LIST_EMPTY(&upt);
			UU_LIST_EMPTY(&vpt);
			ncl_revers_list (np,0,bndr->uvpts,UU_NULL,UU_NULL);
			goto Retry;
		}

		if (status == UU_SUCCESS && lv96)
		{
			status = S_add_triangles (&tess1,tess,tolsq);
			if (inits1) um_free_tess1 (&tess1);
		}

		if (status == UU_SUCCESS && Sneedbndr)
		{
			S_reset_uvtrans(bndr);
			if (uvtes)		
				ncl_weed_trim_bndr (bndr,uvtes);
		}
	}
	else
	{
		int cone = 0;

		if (nu > 3)
			cone = ncl_itsa_cone ((struct UM_srfdatabag *)bs,tfmat,tol,bndr);

		if (Sneedbndr)
			status = S_add_grid_bndr (bs,tfmat,nu,uu,nv,vv,bndr);

		if (status == UU_SUCCESS)
			status = ncl_tess_uvboxes (bs,tfmat,nu,uu,nv,vv,cone,tol,tess,NULL);
	}

Done:

	if (tess->ntri < mintri || status != UU_SUCCESS)
	{
/*
..... If the tessellation fails, or the surface is too small,
..... display the wireframe
*/
		um_clean_tess (tess);
		status = UU_FAILURE;
	}

	uu_list_free(&upt);
	uu_list_free(&vpt);
	uu_list_free(&Slst2);

	NCL_triansf = 0;
	S_reset_uvtrans(bndr);

	return (status);
}

/*********************************************************************
**  E-FUNCTION : ncl_tessellate_polygon(poly,npts,tol,tess)
**     Constructs triangle tessellation of a planar polygon.
**  PARAMETERS
**     INPUT  :
**        poly  - Array of planar polygon points.
**        npts  - Number of points in polygon.
**        tol   - Tessellation tolerance.
**     OUTPUT :
**        tess  - Triangle tessellation.
**  RETURNS      :
**     UU_SUCCESS if no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int ncl_tessellate_polygon(poly,npts,tol,tess)
UM_coord *poly;
int npts;
UU_REAL tol;
UM_tessellation *tess;
{
	int i,status,np;
	UU_REAL plane[4];
	UM_coord *pts;
	UM_2Dcoord pt2d,*pt2s;
	UM_vector *vcs,xaxis,yaxis,spx,sxax,syax,szax;
	UM_transf tfmat,tfmat1;
	UU_LIST plist;
/*
.....Initialize routine
*/
	um_init_aux_lists();
	uu_list_init0(&plist);
/*
.....Remove last point if
.....same as first point
*/
	np = um_cceqcc(poly[0],poly[npts-1]) ? npts -1 : npts;
/*
.....Make sure polygon is planar
*/
	if (!(um_planar_curve(poly,np,plane,xaxis,yaxis))) goto failed;
/*
.....Transform points to XY-plane
*/
	spx[0] = spx[1] = spx[2] = 0.;
	sxax[0] = 1.; sxax[1] = sxax[2] = 0.;
	syax[0] = 0.; syax[1] = 1.; syax[2] = 0.;
	szax[0] = szax[1] = 0.; szax[2] = 1.;
	um_chgcstf(poly[0],xaxis,yaxis,plane,spx,sxax,syax,szax,tfmat);
	uu_list_init(&plist,sizeof(UM_2Dcoord),np,6);
	if (UU_LIST_NULLPTR(&plist)) goto failed;
	for (i=0;i<np;i++)
	{
		um_cctmtf(poly[i],tfmat,pt2d);
		uu_list_push(&plist,pt2d);
	}
/*
.....Make sure polygon is CCLW
*/
	pt2s = (UM_2Dcoord *)UU_LIST_ARRAY(&plist);
	S_polygon_cclw(pt2s,np);
/*
.....Calculate polygon triangles
*/
	status = um_tess_polygon(UU_NULL,UU_NULL,&plist,tess,NULLST,tol);
/*
.....Set number of vertices and triangles
*/
	tess->np = UU_LIST_LENGTH(&tess->vertices);
	tess->ntri = UU_LIST_LENGTH(&tess->tri);
/*
.....Rotate tessallation vertices back
.....to polygon plane
*/
	um_inverttf(tfmat,tfmat1);
	pts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
	vcs = (UM_coord *)UU_LIST_ARRAY(&tess->normals);
	for (i=0;i<tess->np;i++)
	{
		um_cctmtf(pts[i],tfmat1,pts[i]);
		um_vctovc(plane,vcs[i]);
	}
/*
.....Clean up edges
*/
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to create tessellation
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (!UU_LIST_NULLPTR(&plist)) uu_list_free(&plist);
	um_free_aux_lists();
	return(status);
}

/*********************************************************************
**   E_FUNCTION: S_polygon_cclw(pts,npts)
**      This function determines the direction of a 2-D polygon.  The
**      resultant point array will be output in a CCLW direction.
**
**
**   PARAMETERS
**       INPUT  : pts   = Array of points in polygon.
**                npts  = Number of points in 'npts'
**       OUTPUT : pts   = Input point array in a CCLW direction.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static void S_polygon_cclw(pts,npts)
UM_2Dcoord *pts;
int npts;
{
	int i;
	UU_REAL dir,x1,y1,x2,y2;
	UM_2Dcoord lpts;
/*
.....Get direction of profile
*/
	dir = 0.;
	x1 = 0.; y1 = 0.;
	for (i=1;i<npts;i++)
	{
		x2 = pts[i][0] - pts[0][0];
		y2 = pts[i][1] - pts[0][1];
		dir = dir + (x1*y2 - x2*y1);
		x1 = x2; y1 = y2;
	}
/*
.....If current direction is clockwise
.....then reverse points
*/
	if (dir < 0.)
	{
		for (i=0;i<npts/2;i++)
		{
			um_vctovc_2d(pts[i],lpts);
			um_vctovc_2d(pts[npts-i-1],pts[i]);
			um_vctovc_2d(lpts,pts[npts-i-1]);
		}
	}
}

/*********************************************************************
** I_FUNCTION : UU_LOGICAL S_add_inner (nb,wmmx,w,wnext)
**     Insert the middle value of an inner boundary between the current and the
**     next parameter, if fitting.
**  PARAMETERS
**     INPUT  :
**          nb       - number of boundaries
**          wmmx     - boundary ranges
**          w        - current value of the parameter
**     OUTPUT :
**          upt,vpt  - the fixed lists of parameters
**          nu,nv    - number of points in the lists of parameters
**  RETURNS      :
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_add_inner (nb,wmmx,w,wnext)
int nb;
UU_REAL w,*wnext;
UM_2Dcoord *wmmx;
{
	int ib;
	UU_LOGICAL insert;
	UU_REAL w0,w1,wnx;

	insert = UU_FALSE;
	wnx = *wnext;

	for (ib = 1; ib < nb; ib++)
	{
		w0 = wmmx[ib][0]; w1 = wmmx[ib][1];
		if (w0 >= w && w1 <= wnx && (w1 - w0) >= 2.*UM_FUZZ)
		{
			insert = UU_TRUE;
			wnx = (w0 + w1)/2.;
		}
	}
	*wnext = wnx;
	return (insert);
}

/*********************************************************************
** I_FUNCTION : int S_revsf_upts (eptr,tfmat,bound,wmmx,wfrs,wlst,told,wpt,nup)
**    Create the list of U-parameters for a surface of revolution by evolving
**    the generating curve.
**
** PARAMETERS
**          eptr  - pointer to surface record
**          tfmat - ID matrix of input surface
**          wfrs  - u-parameter limits
**          wlst  - u-parameter limits
**          bound - surface boundary
**          wmmx  - U-boundary ranges
**          told  - tolerance (chord height)
**       OUTPUT :
**          wpt   - the calculated list of U-parameters
**          nup   - number of points for the V-list to satisfy tolerance
**    RETURNS      :
**          number of points in the list of parameters
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_revsf_upts (eptr,tfmat,bound,wmmx,wfrs,wlst,duv,told,wpt,nup)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UM_srf_boundary *bound;
UM_2Dcoord *wmmx;
UU_REAL wfrs,wlst,duv,told;
int *nup;
UU_LIST *wpt;
{
	UU_REAL w,wnext;
	int nb,np,status;
	UU_REAL sa,ta,del,theta,r,rad;
	struct NCL_revsurf_rec *rptr;
	struct NCL_fixed_databag crv;
	UM_coord pta,*pts,*uvs;
	UM_vector vca,svc,vrgt;
	int i,np0;
	UU_LOGICAL insert;

	nb = bound->nb;
	UU_LIST_EMPTY (Scvpts);
	UU_LIST_EMPTY (&Slst2);

	rptr = (struct NCL_revsurf_rec *)eptr;
	crv.key = rptr->cvkey;
	status = ncl_retrieve_data_fixed(&crv);
	if (status != UU_SUCCESS) return (0);

	um_cctmtf (rptr->pta,tfmat,pta);
	um_vctmtf (rptr->vca,tfmat,vca);
	sa = rptr->sa / UM_RADIAN;
	ta = rptr->ta / UM_RADIAN;

	if (Sluvtrans) duv *= Srv;
	del = fabs (ta - sa) * duv;
	if (del <= UM_FUZZ) return (0);

	np0 = ncl_evolve_curve (&crv,tfmat,told,Scvpts,UU_NULL,&Slst2,0);
	if (np0 < 2) return (0);
	pts = (UM_coord *)UU_LIST_ARRAY(Scvpts);
	uvs = (UM_coord *)UU_LIST_ARRAY(&Slst2);
	rad = -1.;

	uu_list_push (wpt, &wfrs);
	w = wfrs;
	for (i = 0, np = 1; i < np0; i++)
	{
		wnext = uvs[i][0];
		if (Sluvtrans) wnext *= Sdu;

		if (wnext > wfrs + UM_FUZZ)
		{
			insert = UU_FALSE;
			if (nb > 1) insert = S_add_inner (nb,wmmx,w,&wnext);
			if (wnext < wlst - UM_FUZZ)
			{
				w = wnext;
				uu_list_push (wpt, &w); np++;
				if (insert)
				{
					i--; continue;
				}
			}
		}
		um_vcmnvc (pts[i], pta, svc);
		um_cross (vca,svc,vrgt);
		r = um_mag (vrgt);
		if (r > rad) rad = r;
		if (wnext >= wlst - UM_FUZZ) break;
	}
	uu_list_push (wpt, &wlst); np++;

	if (rad < told)
		*nup = (del < UM_PI)? 2: 3;
	else
	{
		theta = 2. * acos (1. - 0.99 * told / rad);
		if (rad*theta < UM_FUZZ) theta = UM_FUZZ/rad;
		*nup = ceil (del / theta) + 1;
	}

	return (np);
}

/*********************************************************************
** I_FUNCTION : int S_get_next1 (cvtyp,eptr,tfmat,uv,w,wn,wlst,told)
**    Get the next parameter for a parametric curve on surface.
**
** PARAMETERS
**          eptr  - pointer to surface record
**          tfmat - ID matrix of input surface
**          uv    - u-parameter iff cvtyp=1, else v
**          w     - v-parameter iff cvtyp=1, else u
**          wlst  - last w-parameter
**          cvtyp - 1 = u_curve (v=const), 2 = v_curve (u=const).
**          told  - tolerance
**       OUTPUT :
**          wn   - the next w
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_next1 (cvtyp,eptr,tfmat,uv,w,wn,wlst,told)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UU_REAL uv,w,wlst,told,*wn;
int cvtyp;
{
	int status;

	status = um_ev9_crv (UM_ALL,uv,w,cvtyp,eptr,tfmat,&Sevcv);
/*
..... the flag 1 in the call below means there is no preset max value for wn
*/
	if (status == UU_SUCCESS)
	status = ncl_srf_nextpt_at_tol (eptr,tfmat,told,uv,w,cvtyp,wn,wlst,&Sevcv,1);

	return (status);
}

/*********************************************************************
** I_FUNCTION : UU_LOGICAL S_ruled ()
**    Determine whether a surface is "ruled" along u-or-v direction.
**
** PARAMETERS
**          eptr  - pointer to surface record
**          tfmat - ID matrix of input surface
**          uv0,uvm,uv1  - min, max, and middle const parameter (v if cvtyp=1)
**          w0,w1        - min and max variable parameter (u if cvtyp=1)
**          cvtyp - 1 = u_curve (v=const), 2 = v_curve (u=const).
**          told  - tolerance
**       OUTPUT : none
**    RETURNS      :
**          UU_TRUE/UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_ruled (cvtyp,eptr,tfmat,uv0,uvm,uv1,w0,w1,told)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UU_REAL uv0,uvm,uv1,w0,w1,told;
int cvtyp;
{
	int status;
	UU_REAL wn,fcuv,fcw;

	if (Sluvtrans)
	{
		if (cvtyp == 1)
		{
			fcw = Sru; fcuv = Srv;
		}
		else
		{
			fcw = Srv; fcuv = Sru;
		}

		uv0 *= fcuv; uvm *= fcuv; uv1 *= fcuv;
		w0 *= fcw; w1 *= fcw;
	}

	status = S_get_next1 (cvtyp,eptr,tfmat,uv0,w0,&wn,w1,told);
	if (status != UU_SUCCESS || wn < w1) return (UU_FALSE);

	status = S_get_next1 (cvtyp,eptr,tfmat,uvm,w0,&wn,w1,told);
	if (status != UU_SUCCESS || wn < w1) return (UU_FALSE);

	status = S_get_next1 (cvtyp,eptr,tfmat,uv1,w0,&wn,w1,told);
	if (status != UU_SUCCESS || wn < w1) return (UU_FALSE);

	return (UU_TRUE);
}

/*********************************************************************
** I_FUNCTION : int S_get_next (cvtyp,eptr,tfmat,uv,w,dw,wlst,fcuv,fcw,wnext,
**                              told)
**    Get the next parameter for a parametric curve on surface.
**
** PARAMETERS
**          eptr  - pointer to surface record
**          tfmat - ID matrix of input surface
**          uv    -  const parameter (v if cvtyp=1)
**          w     - current variable parameter (u if cvtyp=1)
**          wlst  - last w-parameter
**          cvtyp - 1 = u_curve (v=const), 2 = v_curve (u=const).
**          told  - tolerance
**       OUTPUT :
**          wn   - the next w
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_next (cvtyp,eptr,tfmat,uv,w,dw,wlst,wnext,told)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UU_REAL uv,w,dw,wlst,told,*wnext;
int cvtyp;
{
	int status;
	UU_REAL delw,wn,fcuv,fcw;

	if (Sluvtrans)
	{
		if (cvtyp == 1)
		{
			fcw = Sdu; fcuv = Sdv;
		}
		else
		{
			fcw = Sdv; fcuv = Sdu;
		}

		uv /= fcuv; w /= fcw; wlst /= fcw;
	}

	status = S_get_next1 (cvtyp,eptr,tfmat,uv,w,&wn,wlst,told);
	if (status != UU_SUCCESS) return (status);

	if (Sluvtrans)
	{
		w *= fcw; wn *= fcw;
	}

	delw = wn - w;
	if (*wnext == 0)
	{
		if (delw < 2.*UM_FUZZ)
			*wnext = w + 2.*UM_FUZZ;
		else if (delw > dw)
			*wnext = w + dw;
		else
			*wnext = wn;
	}
	else if (wn < *wnext && delw > 2.*UM_FUZZ)
		*wnext = wn;

	return (status);
}

/*********************************************************************
** FUNCTION : int ncl_evolve_uvpts (eptr,tfmat,bound,npts,ngrid,cvtyp,told,wpt,
**                                                               tesstyp)
**    When cvtyp is 1: divide a surface into u-strips (v=const), so that
**    the u-chordal height within each strip is no more than tolerance.
**    When cvtyp is 2, do the same with v-strips.
**
** PARAMETERS
**          eptr  - pointer to surface record
**          tfmat - ID matrix of input surface
**          u0,u1 - u-parameter limits
**          v0,v1 - v-parameter limits
**          cvtyp - 1 = u_curve (v=const), 2 = v_curve (u=const).
**          told  - tolerance (chord height)
**          tesstyp - UM_TESS_TOLER - use only told;
**                    UM_TESS_GRID - evolve npts points;
**                    UM_TESS_BOTH - use told but evolve no less than npts
**                                   points;
**          npts  - (min) number of points per isoparametric curve
**          ngrid - number of orthogonal isoparametric curves
**                 (number of v-lines if cvtyp is 1, u-lines if cvtyp is 2)
**       OUTPUT :
**          wpt   - the calculated list of parameters (u or v)
**    RETURNS      :
**          number of points in the list of parameters
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_uvpts (revsur,eptr,tfmat,bound,npts,ngrid,cvtyp,told,wpt,tesstyp)
UU_LOGICAL revsur;
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UM_srf_boundary *bound;
UU_REAL told;
int npts,ngrid,cvtyp;
UU_LIST *wpt;
UM_tess_settype tesstyp;
{
	UU_REAL dw,uv0,uv1,uvmid,w,w1,wfrs,wlst,wnext,duv,delw,dw0,lmt;
	int i,status,nw0;
	UM_2Dcoord *wmmx;
	int np = 1;
	int nb = bound->nb;
	UU_LOGICAL lruled;
	static int nup = 0;

	status = UU_SUCCESS;
	lruled = UU_FALSE;

	if (cvtyp == 1)
	{
		wmmx = bound->ummx;
		uv0 = bound->vmmx[0][0]; uv1 = bound->vmmx[0][1];
	}
	else
	{
		wmmx = bound->vmmx;
		uv0 = bound->ummx[0][0]; uv1 = bound->ummx[0][1];
	}

	wfrs = wmmx[0][0];
	wlst = wmmx[0][1];
	dw = wlst - wfrs;
	duv = uv1 - uv0;
	uvmid = (uv0 + uv1) / 2.;
/*
..... the condition below effectively prevents tessellation of very small
..... trimmed surfaces. told/5 = 0.001 may need to be adjusted
.....This logic does not work when the base surface is
.....much larger than the trimmed surface boundary (FSR 61515)
*/
/*	if (duv < told/5. || dw < told/5.)*/
	if ((duv < told/5. || dw < told/5.) && bound->np[0] <= 4)
		return (0);

	delw = dw0 = dw;
	nw0 = 2;
	if (Sneedbndr)
	{
/*
..... if we are doing this for display, we need to take ngrid into account if
..... npts is not set
*/
		if (npts > 2)
			tesstyp = UM_TESS_GRID;
		else if (ngrid > 2)
		{
			npts = 0;
			lruled = S_ruled (cvtyp,eptr,tfmat,uv0,uvmid,uv1,wfrs,wlst,told);
			if (!lruled)
			{
			nw0 = ngrid;
			dw0 = dw / (nw0 - 1.);
		}
	}
	}

	if (!Sneedbndr && revsur && tesstyp == UM_TESS_TOLER)
	{
		if (cvtyp == 1)
		{
			np = S_revsf_upts (eptr,tfmat,bound,wmmx,wfrs,wlst,duv,told,wpt,&nup);
			return (np);
		}
		else
		{
			npts = nup;
			tesstyp = UM_TESS_GRID;
		}
	}

	if (npts < 2)
		tesstyp = UM_TESS_TOLER;
	else
		delw = dw / (npts - 1.);

	w = wfrs;
	uu_list_push (wpt,&w);
	lmt = UM_FUZZ;
	if (dw < lmt*2) lmt = UM_DFUZZ;
	for (i = 1; i < nw0; i++)
	{
		w1 = w + dw0;
		if (w1 > wlst) w1 = wlst;
		while (w < w1 - lmt)
		{
			if (tesstyp == UM_TESS_GRID)
				wnext = w + delw;
			else
			{
				if (lruled)
					wnext = w1;
				else
				{
				wnext = 0;
				status =
				S_get_next (cvtyp,eptr,tfmat,uvmid,w,delw,w1,&wnext,told);
				if (status == UU_SUCCESS)
				status =
				S_get_next (cvtyp,eptr,tfmat,uv0,w,delw,w1,&wnext,told);
				if (status == UU_SUCCESS)
				status =
				S_get_next (cvtyp,eptr,tfmat,uv1,w,delw,w1,&wnext,told);
				if (status != UU_SUCCESS) return (0);
			}
			}

			if (wnext > w1) wnext = w1;
			if (wnext - w < lmt) break;

			if (nb > 1) S_add_inner (nb,wmmx,w,&wnext);

			w = wnext;
			uu_list_push (wpt, &w);
			np++;
		}
	}

	return (np);
}

/*********************************************************************
**    FUNCTION : int ncl_itsa_cone (bs,tfmat,btol,bound)
**      Determine whether a non-trimmed surface is a cone
** PARAMETERS
**          bs     - pointer to surface record
**          tfmat  - ID matrix of input surface
**          bound  - surface boundary (trivial)
**          btol   - tolerance
**       OUTPUT :
**          none
**    RETURNS      :
**          0 - if it is not a cone
**          1 - if it is a cone with apex v=0
**          2 - if it is a cone with apex v=1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_itsa_cone (bs,tfmat,btol,bound)
struct UM_srfdatabag *bs;
UM_transf tfmat;
UM_srf_boundary *bound;
UU_REAL btol;
{
	UU_REAL apex;
	int cone = 0;
	UU_REAL co,v;
	UM_vector n1,n2,n3;
	int status = UU_SUCCESS;

	UU_LIST_EMPTY (Scvpts);
	UU_LIST_EMPTY (&Slst2);
/*
..... It is a cone iff: (1) one base (v=0 or v=1) evolves into 1 or 2 points
..... while the other one evolves in more than 3; and (2) normals on the other
..... base are essentially different
*/
	apex = 0.;
	ncl_evolve_crv_on_srf (bs,tfmat,0., bound->ummx,1,btol,
                     Scvpts,UU_NULL,&Slst2);
	if (Slst2.cur_cnt <= 2) cone = 1;

	if (cone == 0)
	{
		apex = 1.;
		UU_LIST_EMPTY (Scvpts);
		UU_LIST_EMPTY (&Slst2);
		ncl_evolve_crv_on_srf (bs,tfmat,1., bound->ummx,1,btol,
                     Scvpts,UU_NULL,&Slst2);
		if (Slst2.cur_cnt <= 2) cone = 2;
	}

	if (cone > 0)
	{
		v = 1. - apex;
		UU_LIST_EMPTY (Scvpts);
		UU_LIST_EMPTY (&Slst2);
		ncl_evolve_crv_on_srf (bs,tfmat,v, bound->ummx,1,btol,
                     Scvpts,UU_NULL,&Slst2);
		if (Slst2.cur_cnt <= 3)
		{
			cone = 0;
			goto Done;
		}

		status = uc_evsrf(UM_NORM, 0., v, bs, tfmat, &Sevsrf);
		if (status != UU_SUCCESS)
		{
			cone = 0;
			goto Done;
		}
		um_unitvc (Sevsrf.snorm,n1);

		status = uc_evsrf(UM_NORM, 0.333, v, bs, tfmat, &Sevsrf);
		if (status != UU_SUCCESS)
		{
			cone = 0;
			goto Done;
		}
		um_unitvc (Sevsrf.snorm,n2);

		status = uc_evsrf(UM_NORM, 0.667, v, bs, tfmat, &Sevsrf);
		if (status != UU_SUCCESS)
		{
			cone = 0;
			goto Done;
		}
		um_unitvc (Sevsrf.snorm,n3);

		co = um_dot (n1,n2);
		if (co < 0.9) goto Done;
		co = um_dot (n3,n2);
		if (co < 0.9) goto Done;
		co = um_dot (n3,n1);
		if (co < 0.9) cone = 0;;
	}

Done:
	return (cone);
}

/*********************************************************************
** FUNCTION : int S_tess_rectangles_96 (nu,uu,nv,vv,bound,tess)
**
**    Tessellate a trimmed surface using a rectangular grid
**
** PARAMETERS
**          srf   - pointer to surface entity
**          tfmat - surface matrix
**          nu,nv - number of points in u,v
**          uu,vv - arrays of UV points defining the grid
**          bound - surface boundary
**          tol       - tolerance
**       OUTPUT :
**          tess  - filled tessellation structure
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_tess_rectangles_96 (srf,tfmat,nu,uu,nv,vv,bound,tess,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
int nu,nv;
UU_REAL *uu,*vv,tol;
UM_srf_boundary *bound;
UM_tess1 *tess;
{
	int status = UU_SUCCESS;
	UU_LIST polygon;

	um_init_aux_lists();
	uu_list_init0 (&polygon);

	if (nu == 2 && nv == 2)
	{
		int i,np = bound->np[0]-1;
		UM_coord *uv;
		UM_2Dcoord p2D;

		status = uu_list_init1 (&polygon, sizeof(UM_2Dcoord), np, np);
		if (status == UU_SUCCESS)
		{
			uv = (UM_coord *)UU_LIST_ARRAY(bound->uvpts);
			for (i = 0; i < np; i++)
			{
				p2D[0] = uv[i][0]; p2D[1] = uv[i][1];
				uu_list_push (&polygon,p2D);
			}

			status = um_tess_polygon_96 (srf,tfmat,&polygon,tess,tol);
		}
		if (status != UU_SUCCESS)
			um_clean_tess1 (tess);
	}
	else
	{
		int nu1, nv1, i, j;
		UM_tess1 tess1,tess2;
		UU_REAL v0,v1,u0,u1;

		nu1 = nu - 1; nv1 = nv - 1;

		status = uu_list_init1 (&polygon, sizeof(UM_2Dcoord), 200, 200);
		if (status != UU_SUCCESS) goto Done;

		status = um_create_tess1 (&tess1,100);
		if (status == UU_SUCCESS)
			status = um_create_tess1 (&tess2,0);
		if (status != UU_SUCCESS) goto Fin;

		tess1.key = tess2.key = tess->key;

		if (ncl_watrev_swap())
		{
			u0 = uu[0];
			for (i = 0; i < nu1; i++)
			{
				u1 = uu[i+1];
				v0 = vv[0];
				for (j = 0; j < nv1; j++)
				{
					v1 = vv[j+1];
					status = S_tess_panel_96 (srf,tfmat,u0,u1,v0,v1,bound,&tess1,
						&tess2,&polygon,tol);
					if (status != UU_SUCCESS) goto Fin;

					if (j < nu1/2 && UU_LIST_EXPANDS(&tess->uv,tess1.np))
					{
						UU_LIST_DOUBLEXP (&tess->uv);
						UU_LIST_DOUBLEXP (&tess->vertices);
						UU_LIST_DOUBLEXP (&tess->normals);
					}
					if (j < nu1/2 && UU_LIST_EXPANDS(&tess->edges,tess1.nedges))
					{
						UU_LIST_DOUBLEXP (&tess->edges);
					}

					um_merge_tess (&tess1, tess);
					v0 = v1;
				}
				u0 = u1;
			}
		}
		else
		{
			v0 = vv[0];
			for (j = 0; j < nv1; j++)
			{
				v1 = vv[j+1];
				u0 = uu[0];
				for (i = 0; i < nu1; i++)
				{
					u1 = uu[i+1];
					status = S_tess_panel_96 (srf,tfmat,u0,u1,v0,v1,bound,&tess1,
						&tess2,&polygon,tol);
					if (status != UU_SUCCESS) goto Fin;

					if (j < nv1/2 && UU_LIST_EXPANDS(&tess->uv,tess1.np))
					{
						UU_LIST_DOUBLEXP (&tess->uv);
						UU_LIST_DOUBLEXP (&tess->vertices);
						UU_LIST_DOUBLEXP (&tess->normals);
					}
					if (j < nv1/2 && UU_LIST_EXPANDS(&tess->edges,tess1.nedges))
					{
						UU_LIST_DOUBLEXP (&tess->edges);
					}

					um_merge_tess (&tess1, tess);
					u0 = u1;
				}
				v0 = v1;
			}
		}
Fin:
		um_free_tess1 (&tess1);
		um_free_tess1 (&tess2);
	}

	if (status == UU_SUCCESS)
	{
		tess->np = UU_LIST_LENGTH (&tess->vertices);
		tess->nedges = UU_LIST_LENGTH (&tess->edges);
	}

Done:
	uu_list_free (&polygon);
	um_free_aux_lists();

	return (status);
}

/*********************************************************************
** FUNCTION : int S_tess_rectangles (nu,uu,nv,vv,bound,tess)
**
**    Tessellate a trimmed surface using a rectangular grid
**
** PARAMETERS
**          srf   - pointer to surface entity
**          tfmat - surface matrix
**          nu,nv - number of points in u,v
**          uu,vv - arrays of UV points defining the grid
**          bound - surface boundary
**          tol       - tolerance
**       OUTPUT :
**          tess  - filled tessellation structure
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_tess_rectangles (srf,bs,tfmat,nu,uu,nv,vv,bound,tess,uvtes,tol)
struct NCL_fixed_databag *srf,*bs;
UM_transf tfmat;
int nu,nv;
UU_REAL *uu,*vv,tol;
UM_srf_boundary *bound;
UM_tessellation *tess;
UU_LIST *uvtes;
{
	int status = UU_SUCCESS;
	UU_LIST polygon;
	int i,j,nu1,nv1,np;
	UM_coord *uv;
	UM_2Dcoord p2D;
	UU_REAL v0,v1,u0,u1;

	um_init_aux_lists();
	uu_list_init0 (&polygon);

	if (nu == 2 && nv == 2)
	{
		np = bound->np[0]-1;

		status = um_init_tess_lists (tess,uvtes,np);
		if (status == UU_SUCCESS)
		status = uu_list_init1 (&polygon, sizeof(UM_2Dcoord), np, np);
		if (status == UU_SUCCESS)
		{
			uv = (UM_coord *)UU_LIST_ARRAY(bound->uvpts);
			for (i = 0; i < np; i++)
			{
				p2D[0] = uv[i][0]; p2D[1] = uv[i][1];
				uu_list_push (&polygon,p2D);
			}

			status = um_tess_polygon (bs,tfmat,&polygon,tess,uvtes,tol);
		}
	}
	else
	{
		nu1 = nu - 1; nv1 = nv - 1;
		status = um_init_tess_lists (tess,uvtes,200);
		if (status == UU_SUCCESS)
		status = uu_list_init1 (&polygon, sizeof(UM_2Dcoord), 200, 200);
		if (status != UU_SUCCESS) goto Done;

		if (ncl_watrev_swap())
		{
			u0 = uu[0];
			for (i = 0; i < nu1; i++)
			{
				u1 = uu[i+1];
				v0 = vv[0];
				for (j = 0; j < nv1; j++)
				{
					v1 = vv[j+1];
 					status =
					S_tess_panel (srf,bs,tfmat,u0,u1,v0,v1,bound,tess,uvtes,&polygon,tol);
					if (status != UU_SUCCESS) goto Done;

					v0 = v1;
				}
				u0 = u1;
			}
		}
		else
		{
			v0 = vv[0];
			for (j = 0; j < nv1; j++)
			{
				v1 = vv[j+1];
				u0 = uu[0];
				for (i = 0; i < nu1; i++)
				{
					u1 = uu[i+1];
					status =
					S_tess_panel (srf,bs,tfmat,u0,u1,v0,v1,bound,tess,uvtes,&polygon,tol);
					if (status != UU_SUCCESS) goto Done;

					u0 = u1;
				}
				v0 = v1;
			}
		}
	}

	if (status == UU_SUCCESS)
	{
		tess->np = UU_LIST_LENGTH (&tess->vertices);
		tess->ntri = UU_LIST_LENGTH (&tess->tri);
	}

Done:
	uu_list_free (&polygon);
	um_free_aux_lists();

	return (status);
}

/*********************************************************************
** FUNCTION : int S_uvboxes_94 (srf,tfmat,nu,uu,nv,vv,cone, tess)
**
**    Tessellate a rectangular grid (this routine is a small
**    adaptation of ncl_tessellate_uvbox).
**
** PARAMETERS
**          srf   - pointer to surface record
**          tfmat - transformation matrix of input surface
**          tol   - tolerance
**          nu,nv - number of points in u,v
**          uu,vv - arrays of UV points defining the grid
**       OUTPUT :
**          tess  - filled tessellation structure
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_uvboxes_94 (srf,tfmat,nu,uu,nv,vv,cone, tess)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
int nu,nv,cone;
UU_REAL *uu, *vv;
UM_tess1 *tess;
{
	int status, nu1, nv1, i,j,k,m, np, above, below, step;
	int UNO = 1;
	UM_2Dcoord uv;
	UM_edge edge;

	status = UU_SUCCESS;
	nu1 = nu - 1; nv1 = nv - 1;

	if (nu*nv > 2*tess->uv.max_cnt)
	{
		int cnt = nu*nv;

		uu_list_free (&tess->vertices);
		uu_list_free (&tess->normals);
		uu_list_free (&tess->edges);
		uu_list_free (&tess->uv);

		status =
		uu_list_init1 (&tess->vertices, sizeof (UM_coord), cnt, cnt);
		if (status == UU_SUCCESS) status =
		uu_list_init1 (&tess->normals, sizeof (UM_vector), cnt, cnt);
		if (status == UU_SUCCESS) status =
		uu_list_init1 (&tess->uv, sizeof (UM_2Dcoord), cnt, cnt);
		if (status == UU_SUCCESS)
		{
			cnt = 3*nu1*nv1 + nu1 + nv1;
			status =
			uu_list_init1 (&tess->edges, sizeof (UM_edge), cnt, cnt);
		}
		if (status != UU_SUCCESS)
		{
			UM_int2 ier=-482;
			uwarn(&ier);
			return (UU_FAILURE);
		}
	}
/*
..... store points
*/
	for (i=0; i<nv; i++)
	{
		for (k=0; k<nu; k++)
		{
			um_xytovc_2d (uu[k],vv[i],uv);
			status = um_add_tess_points_96 (srf,tfmat,&UNO,uv,tess);
			if (status != UU_SUCCESS)
				return (UU_FAILURE);
		}
	}

	if (cone >= 1)
	{
/*
..... fix normals on the cone base, so that cone is uniformly lighted from
..... all sides
*/
		UM_vector *norm;

		norm = (UM_vector *) UU_LIST_ARRAY (&tess->normals);

		if (cone == 1)
		{
			for (k=0; k<nu; k++)
				um_vctovc (norm[nu+k],norm[k]);
		}

		else if (cone == 2)
		{
			for (k=nu*nv-nu; k<nu*nv; k++)
				um_vctovc (norm[k-nu],norm[k]);
		}
	}

/*
..... define initial tessellation:
.....       edges of rectangles + left_bottom -> right_top diagonals;
.....
..... the outer edges are oriented CCW starting from (umin,vmin);
..... the inner horizontal edges are left-to-right;
..... the inner vertical edges are top-to-bottom;
.....
..... step = dist. in index between 2 successive horizontal edges
..... (changes for the top row of rectangles);
..... below = index of the diagonal edge right below a horizontal edge;
..... above = index of the horizontal edge right above a diagonal edge;
..... j = index of current vertex (knot);
..... m = number of the first edge to be added for a new vertex;
..... each vertex except for those on the right and top boundaries
..... produces 3 edges m,m+1,m+2,
..... where m = vertical edge, m+1 = diagonal edge, m+2 = horizontal edge
*/
		np = nu*nv;
		step = 3*nu - 2;
		below = 1 - step;
		above = step + 2;

		for (i=0, j=0; j < np-1; i++, below -= 2, above -=2)
			for (k=0; (k<nu)&&(j<np-1); k++,j++,below+=3,above+=3)
			{
				m = UU_LIST_LENGTH (&tess->edges);

				if (i == nv1 - 1) above -= 2;

				if (i < nv1)
				{
					if (k == nu1)
					{
						edge.south = j; edge.north = j+nu;
						edge.s_e = edge.n_e = -1;
						edge.s_w = m-1; edge.n_w = m-2;
					}
					else
					{
						edge.south = j+nu; edge.north = j;
						if(k != 0)
						{
							edge.s_e = m-2;
							edge.n_e = m-1;
						}
						else
						{
							edge.s_e = edge.n_e = -1;
						}
						edge.n_w = m+1; edge.s_w = above;
					}

					uu_list_push (&tess->edges,&edge);
					if (k == nu1) continue;

					edge.south = j;   edge.north = j+nu+1;
					edge.s_e = m+2;   edge.n_e = m+3;
					edge.n_w = above; edge.s_w = m;

					uu_list_push (&tess->edges,&edge);

					edge.south = j; edge.north = j+1;
					if(i!=0)
					{
						edge.s_e = below-1; edge.n_e = below;
					}
					else
					{
						edge.s_e = edge.n_e = -1;
					}
					edge.n_w = m+3; edge.s_w = m+1;
				}
				else
				{
					edge.south = j+1; edge.north = j;
					edge.s_e = edge.n_e = -1;
					edge.n_w = below-1; edge.s_w = below;
				}

				uu_list_push (&tess->edges,&edge);

			}

	tess->np = UU_LIST_LENGTH (&tess->vertices);
	tess->nedges = UU_LIST_LENGTH (&tess->edges);

	return (status);
}

/*********************************************************************
** FUNCTION : int ncl_tess_uvboxes (nu,uu,nv,vv,cone, tess)
**
**    Tessellate a rectangular grid (this routine is a small
**    adaptation of ncl_tessellate_uvbox).
**
** PARAMETERS
**          srf   - pointer to surface record
**          tfmat - transformation matrix of input surface
**          tol   - tolerance
**          nu,nv - number of points in u,v
**          uu,vv - arrays of UV points defining the grid
**       OUTPUT :
**          tess  - filled tessellation structure
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tess_uvboxes (srf,tfmat,nu,uu,nv,vv,cone,tol,tess,uvtes)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
int nu,nv,cone;
UU_REAL *uu,*vv,tol;
UM_tessellation *tess;
UU_LIST *uvtes;
{
	int status, nu1,nv1,i,k;
	UM_coord uv;
	UM_tript tript;
	UU_REAL tolsq = tol*tol;

	if (ncl_setver(94))
	{
		UM_tess1 tess1;

		status = um_create_tess1 (&tess1,200);
		if (status == UU_SUCCESS)
		status = S_uvboxes_94 (srf,tfmat,nu,uu,nv,vv,cone, &tess1);
		if (status == UU_SUCCESS)
		status = S_add_triangles (&tess1,tess,tolsq);

		um_free_tess1 (&tess1);
		return (status);
	}

	status = UU_SUCCESS;
	nu1 = nu - 1; nv1 = nv - 1;

	k = nu*nv + 1;
	if (UU_LIST_NULLPTR (&tess->vertices))
	{
		status = uu_list_init1 (&tess->vertices, sizeof (UM_coord), k, k);
		if (status != UU_SUCCESS) return (status);
	}
	if (UU_LIST_NULLPTR (&tess->normals))
	{
		status = uu_list_init1 (&tess->normals, sizeof (UM_coord), k, k);
		if (status != UU_SUCCESS) return (status);
	}
/*
	if (UU_LIST_NULLPTR (&tess->uv))
	{
		status = uu_list_init1 (&tess->uv, sizeof (UM_2Dcoord), k, k);
		if (status != UU_SUCCESS) return (status);
	}
*/
/*
..... store points
*/
	for (i = 0; i < nv; i++)
	{
		for (k = 0; k < nu; k++)
		{
			uv[0] = uu[k]; uv[1] = vv[i];
			status = uc_evsrf(UM_NORM,uv[0],uv[1],srf,tfmat, &Sevsrf);
			if (status != UU_SUCCESS) return (UU_FAILURE);
			uu_list_push (&tess->vertices,&Sevsrf.sp);

			if (NCL_triansf > 0)
			ncl_triansf_fix_norm (uv[0],uv[1],srf,tfmat,Sevsrf.snorm,tolsq);

			uu_list_push (&tess->normals,&Sevsrf.snorm);
			/* uu_list_push (&tess->uv,&uv); */	
			if (uvtes != NULLST) 
			{
				uv[2] = 0.0;
				uu_list_push (uvtes, &uv);
			}
		}
	}
	tess->np = nu*nv;

	if (cone >= 1)
	{
/*
..... fix normals on the cone base, so that cone is uniformly lighted from
..... all sides
*/
		UM_vector *norm;

		norm = (UM_vector *) UU_LIST_ARRAY (&tess->normals);

		if (cone == 1)
		{
			for (k=0; k<nu; k++)
				um_vctovc (norm[nu+k],norm[k]);
		}

		else if (cone == 2)
		{
			for (k=nu*nv-nu; k<nu*nv; k++)
				um_vctovc (norm[k-nu],norm[k]);
		}
	}

	tess->ntri = 2*nu1*nv1; k = tess->ntri + 1;
	if (UU_LIST_NULLPTR (&tess->tri))
	{
		status = uu_list_init1 (&tess->tri, sizeof (UM_tript), k, k);
		if (status != UU_SUCCESS) return (status);
	}

	if (ncl_watrev_swap())
	{
		for (k = 0; k < nu1; k++)
		{
			for (i = 0; i < nv1; i++)
			{
				tript.n1 = k + i*nu;
				tript.n2 = k + 1 + i*nu;
				tript.n3 = k + 1 + (i+1)*nu;
				uu_list_push (&tess->tri,&tript);
				tript.n1 = k + i*nu;
				tript.n2 = k + 1 + (i+1)*nu;
				tript.n3 = k + (i+1)*nu;
				uu_list_push (&tess->tri,&tript);
			}
		}
	}
	else
	{
		for (i = 0; i < nv1; i++)
		{
			for (k = 0; k < nu1; k++)
			{
				tript.n1 = k + i*nu;
				tript.n2 = k + 1 + i*nu;
				tript.n3 = k + 1 + (i+1)*nu;
				uu_list_push (&tess->tri,&tript);
				tript.n1 = k + i*nu;
				tript.n2 = k + 1 + (i+1)*nu;
				tript.n3 = k + (i+1)*nu;
				uu_list_push (&tess->tri,&tript);
			}
		}
	}

	return (status);
}

/*********************************************************************
** FUNCTION : int ncl_tess_uvbox_96 (uu,vv,tess)
**
**    Tessellate a rectangular grid (this routine is a small
**    adaptation of ncl_tessellate_uvbox).
**
** PARAMETERS
**          nu,nv - number of points in u,v
**          uu,vv - arrays of UV points defining the grid
**       OUTPUT :
**          tess  - filled tessellation structure
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tess_uvbox_96 (srf,tfmat,uu,vv,tess)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_REAL *uu, *vv;
UM_tess1 *tess;
{
	int status, nu1,nv1,i,j,k,m, np, above, below, step;
	int UNO = 1,nu = 2, nv = 2;
	UM_2Dcoord uv;
	UM_edge edge;

	status = UU_SUCCESS;
	nu1 = nv1 = 1;
/*
..... store points
*/
	for (i = 0; i < 2; i++)
	{
		for (k = 0; k < 2; k++)
		{
			um_xytovc_2d (uu[k],vv[i],uv);
			status = um_add_tess_points_96 (srf,tfmat,&UNO,uv,tess);
			if (status != UU_SUCCESS)
				return (UU_FAILURE);
		}
	}
/*
..... define initial tessellation:
.....       edges of rectangles + left_bottom -> right_top diagonals;
.....
..... the outer edges are oriented CCW starting from (umin,vmin);
..... the inner horizontal edges are left-to-right;
..... the inner vertical edges are top-to-bottom;
.....
..... step = dist. in index between 2 successive horizontal edges
..... (changes for the top row of rectangles);
..... below = index of the diagonal edge right below a horizontal edge;
..... above = index of the horizontal edge right above a diagonal edge;
..... j = index of current vertex (knot);
..... m = number of the first edge to be added for a new vertex;
..... each vertex except for those on the right and top boundaries
..... produces 3 edges m,m+1,m+2,
..... where m = vertical edge, m+1 = diagonal edge, m+2 = horizontal edge
*/
		np = nu*nv;
		step = 3*nu - 2;
		below = 1 - step;
		above = step + 2;

		for (i=0, j=0; j < np-1; i++, below -= 2, above -=2)
			for (k=0; (k<nu)&&(j<np-1); k++,j++,below+=3,above+=3)
			{
				m = UU_LIST_LENGTH (&tess->edges);

				if (i == nv1 - 1) above -= 2;

				if (i < nv1)
				{
					if (k == nu1)
					{
						edge.south = j; edge.north = j+nu;
						edge.s_e = edge.n_e = -1;
						edge.s_w = m-1; edge.n_w = m-2;
					}
					else
					{
						edge.south = j+nu; edge.north = j;
						if(k != 0)
						{
							edge.s_e = m-2;
							edge.n_e = m-1;
						}
						else
						{
							edge.s_e = edge.n_e = -1;
						}
						edge.n_w = m+1; edge.s_w = above;
					}

					uu_list_push (&tess->edges,&edge);
					if (k == nu1) continue;

					edge.south = j;   edge.north = j+nu+1;
					edge.s_e = m+2;   edge.n_e = m+3;
					edge.n_w = above; edge.s_w = m;

					uu_list_push (&tess->edges,&edge);

					edge.south = j; edge.north = j+1;
					if(i!=0)
					{
						edge.s_e = below-1; edge.n_e = below;
					}
					else
					{
						edge.s_e = edge.n_e = -1;
					}
					edge.n_w = m+3; edge.s_w = m+1;
				}
				else
				{
					edge.south = j+1; edge.north = j;
					edge.s_e = edge.n_e = -1;
					edge.n_w = below-1; edge.s_w = below;
				}

				uu_list_push (&tess->edges,&edge);

			}

	tess->np = UU_LIST_LENGTH (&tess->vertices);
	tess->nedges = UU_LIST_LENGTH (&tess->edges);

	return (status);
}

/*********************************************************************
** FUNCTION : int ncl_tess_uvbox (srf,tfmat,pt,tess)
**
**    Tessellate a UV box.
**
** PARAMETERS
**          srf   - pointer to surface entity
**          tfmat - surface matrix
**          pt    - box points
**          tess  - current tessellation structure
**       OUTPUT :
**          tess  - updated tessellation structure
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tess_uvbox (srf,tfmat,pt,tess,uvtes)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_2Dcoord pt[];
UM_tessellation *tess;
UU_LIST *uvtes;
{
	int status,np,np0;
	UM_tript tript;

	status = UU_SUCCESS;
	np0 = UU_LIST_LENGTH (&tess->vertices);
	np = 4;
/*
..... store points
*/
	status = um_add_tess_points (srf,tfmat,&np,pt,tess,uvtes);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	tript.n1 = np0 + 3;
	tript.n2 = np0;
	tript.n3 = np0 + 2;
	uu_list_push (&tess->tri,&tript);

	if (np == 4)
	{
		tript.n1 = np0;
		tript.n2 = np0 + 1;
		tript.n3 = np0 + 2;
		uu_list_push (&tess->tri,&tript);
	}

	return (status);
}

/*********************************************************************
** FUNCTION : int S_tess_panel_96 (u0,u1,v0,v1,bound,tess,polygon)
**
**    Tessellate a rectangular panel (i.e., intersect it with the surface
**    boundary, and tessellate the resulting polygon)
**
** PARAMETERS
**          srf   - pointer to surface entity
**          tfmat - surface matrix
**          u0,u1,v0,v1 - the panel
**          bound - surface boundary
**          polygon - list structure (intialized) to use
**       OUTPUT :
**          tess  - tessellation structure complete with edges
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_tess_panel_96 (srf,tfmat,u0,u1,v0,v1,bound,tess,tess1,polygon,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_REAL u0,u1,v0,v1,tol;
UM_srf_boundary *bound;
UM_tess1 *tess,*tess1;
UU_LIST *polygon;
{
	int status = UU_SUCCESS;
	int pantess;
	UU_REAL uu[2], vv[2];
	UU_REAL umid,vmid;

	polygon->cur_cnt = 0;
	pantess = um_pan_cross_bndr (srf,tfmat,u0,u1,v0,v1,bound,polygon,tol);
/*
..... um_pan_cross_bndr returns:
..... -1 - if the panel is outside the boundary, and so is discharged;
.....  0 - if the boundary cuts a single polygon, then the polygon is returned
.....      and needs to be tessellated
..... 10 - if the panel has to be split;
.....  1 - if the panel is fully inside the boundary, and so is tessellated as
.....      a simple rectangle
*/
	if (pantess == -1) return (UU_SUCCESS);

	if (pantess == 0)
	{
		if (tess->edges.cur_cnt == 0)
			status = um_tess_polygon_96 (srf,tfmat,polygon,tess,tol);
		else
		{
			status = um_tess_polygon_96 (srf,tfmat,polygon,tess1,tol);
			if (status == UU_SUCCESS)
				um_merge_tess (tess1, tess);
		}
	}
	else if (pantess == 1 ||(u1 - u0) < 5.*UM_FUZZ || (v1 - v0) < 5.*UM_FUZZ)
	{
		uu[0] = u0; uu[1] = u1;
		vv[0] = v0; vv[1] = v1;

		if (tess->edges.cur_cnt == 0)
			status = ncl_tess_uvbox_96 (srf,tfmat,uu,vv,tess);
		else
		{
			status = ncl_tess_uvbox_96 (srf,tfmat,uu,vv,tess1);
			if (status == UU_SUCCESS)
				um_merge_tess (tess1, tess);
		}
	}

	else /* (pantess == 10) */
	{
		umid = (u0 + u1)/2.;
		vmid = (v0 + v1)/2.;

		status =
		S_tess_panel_96 (srf,tfmat,u0,umid,v0,vmid,bound,tess,tess1,polygon,tol);
		if (status != UU_SUCCESS) return (status);
		status =
		S_tess_panel_96 (srf,tfmat,umid,u1,v0,vmid,bound,tess,tess1,polygon,tol);
		if (status != UU_SUCCESS) return (status);
		status =
		S_tess_panel_96 (srf,tfmat,umid,u1,vmid,v1,bound,tess,tess1,polygon,tol);
		if (status != UU_SUCCESS) return (status);
		status =
		S_tess_panel_96 (srf,tfmat,u0,umid,vmid,v1,bound,tess,tess1,polygon,tol);
		if (status != UU_SUCCESS) return (status);
	}

	return (status);
}


/*********************************************************************
** FUNCTION : int S_tess_panel (u0,u1,v0,v1,bound,tess,polygon)
**
**    Tessellate a rectangular panel (i.e., intersect it with the surface
**    boundary, and tessellate the resulting polygon)
**
** PARAMETERS
**          srf   - pointer to surface entity
**          tfmat - surface matrix
**          u0,u1,v0,v1 - the panel
**          bound - surface boundary
**          polygon - list structure (intialized) to use
**       OUTPUT :
**          tess  - tessellation structure complete with edges
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_tess_panel (srf,bs,tfmat,u0,u1,v0,v1,bound,tess,uvtes,polygon,tol)
struct NCL_fixed_databag *srf,*bs;
UM_transf tfmat;
UU_REAL u0,u1,v0,v1,tol;
UM_srf_boundary *bound;
UM_tessellation *tess;
UU_LIST *uvtes,*polygon;
{
	int status = UU_SUCCESS;
	int pantess,i=0,npanel;
	UM_2Dcoord pt[4],*pp;
	UU_REAL umid,vmid,u[4],uu[4],v[4],vv[4],r=0.,d;
	UU_LOGICAL lrefine = UU_TRUE,lrefine2 = UU_TRUE;
	static int lnum=1;
/*
..... um_pan_cross_bndr returns:
..... -1 - if the panel is outside the boundary, and so is discharged;
.....  0 - if the boundary cuts a single polygon, then the polygon is returned
.....      and needs to be tessellated
..... 10 - if the panel has to be split;
.....  1 - if the panel is fully inside the boundary, and so is tessellated as
.....      a simple rectangle
*/
	polygon->cur_cnt = 0;
	npanel = 0;
	pantess = um_pan_cross_bndr (bs,tfmat,u0,u1,v0,v1,bound,polygon,tol);
#if DEBUG == 1
	S_print_rect(u0,u1,v0,v1,polygon,srf,tfmat,lnum);
	lnum += 4;
#endif
		
	if (pantess == -1) return (UU_SUCCESS);

	if (pantess == 0)
	{
		status = um_tess_polygon (bs,tfmat,polygon,tess,uvtes,tol);
/*
..... In case polygon tesselation failed, tesselate again to avoid missing shading face.
..... 12/29/2016
*/
		if (status != UU_SUCCESS && ((u1 - u0) < 4.0 * UM_FUZZ || (v1 - v0) < 4.0 * UM_FUZZ))
		{
			pt[0][0] = u0; pt[0][1] = v0;
			pt[1][0] = u1; pt[1][1] = v0;
			pt[2][0] = u1; pt[2][1] = v1;
			pt[3][0] = u0; pt[3][1] = v1;

			status = ncl_tess_uvbox (bs,tfmat,pt,tess,uvtes);
		}
	}
	else if (pantess == 1 || (u1 - u0) < UM_FUZZ || (v1 - v0) < UM_FUZZ)
	{
		pt[0][0] = u0; pt[0][1] = v0;
		pt[1][0] = u1; pt[1][1] = v0;
		pt[2][0] = u1; pt[2][1] = v1;
		pt[3][0] = u0; pt[3][1] = v1;

		status = ncl_tess_uvbox (bs,tfmat,pt,tess,uvtes);
	}
	else /* (pantess == 10) */
	{
/*
..... Check if refine the panel within tolerance or not for u and v directions
*/
		lrefine = S_check_refine_within(srf,tfmat,u0,u1,polygon,tol,&umid,0);
		lrefine2 = S_check_refine_within(srf,tfmat,v0,v1,polygon,tol,&vmid,1);
/*
..... Get ratio of XYZ distances to determine how to break up domain
*/
		S_len_ratio(u0,u1,v0,v1,polygon,srf,tfmat,&r);
		if (!lrefine || !lrefine2)
		{
/*
..... r > 10. shows u direction is at least 10 times longer than v direction.
..... The u direction is then broken into 4 strips and the v direction is left alone.
..... r < .1 shows v direction is at least 10 times longer than u direction.
..... The v direction is then broken into 4 strips and the u direction is left alone.
..... Otherwise the lengths are close enough to simply break area into quarters
*/
			if (r > 10.) i=0;
			else if (r < .1) i=1;
			else i=2;
				
			S_tess_panel_calc(u0,u1,v0,v1,u,uu,v,vv,i);
			npanel = 4;
		}
/*
..... This call will result in tesselation since no refinement is necessary
*/
		else
		{
			umid = (u0+u1)/2.;
			vmid = (v0+v1)/2.;
			i = -1;
			if ((u1 - umid) > UM_DFUZZ && (umid - u0) > UM_DFUZZ) i=3;
			else if ((v1 - vmid) > UM_DFUZZ && (vmid - v0) > UM_DFUZZ) i=4;
			if (i != -1)
			{
				S_tess_panel_calc(u0,u1,v0,v1,u,uu,v,vv,i);
				npanel = 2;
			}
		}
	}
/*
..... Set which strips to use when using two strips
*/
	if (npanel == 2 && i == 4) 
	{
		i = 2;
		npanel = 4;
	}
	else i=0;
/*
.....Tessellate any calculated panels
*/
	for (i;i<npanel;i++)
	{
		status =
			S_tess_panel (srf,bs,tfmat,u[i],uu[i],v[i],vv[i],bound,tess,uvtes,polygon,tol);
		if (status != UU_SUCCESS) return (status);
	}

	return (status);
}

/*********************************************************************
** FUNCTION : int S_tess_panel_calc (u0,u1,v0,v1,u,uu,v,vv,type)
**       Break up area into strips or a grid of four rectangles.
** PARAMETERS
**          u0,u1,v0,v1 - given panel values
**          u,uu,v,vv - array of panel values
**          type  - panel type: 0 - four panels u direction
**                              1 - four panels v direction 
**                              2 - grid of four 
**                            3,4 - two panels in both directions
**       OUTPUT :
**          umin,umax,vmin,vmax - array of panel values
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_tess_panel_calc (u0,u1,v0,v1,u,uu,v,vv,type)
UU_REAL u0,u1,v0,v1,u[4],uu[4],v[4],vv[4];
int type;
{
	UU_REAL d;
/*
..... Break the area into four strips in u direction
*/
	if (type == 0)
	{
		d = (u1-u0)/4.;
		u[0] = u0;
		uu[0] = u[1] = u0 + d;
		uu[1] = u[2] = u0 + 2.*d;
		uu[2] = u[3] = u0 + 3.*d;
		uu[3] = u1;

		v[0] = v[1] = v[2] = v[3] = v0;
		vv[0] = vv[1] = vv[2] = vv[3] = v1;
	}
/*
..... Break the area into four strips in v direction
*/
	else if (type == 1)
	{
		d = (v1-v0)/4.;
		v[0] = v0;
		vv[0] = v[1] = v0 + d;
		vv[1] = v[2] = v0 + 2.*d;
		vv[2] = v[3] = v0 + 3.*d;
		vv[3] = v1;

		u[0] = u[1] = u[2] = u[3] = u0;
		uu[0] = uu[1] = uu[2] = uu[3] = u1;
	}
/*
..... Break the area into four rectangles
*/
	else if (type == 2)
	{
		u[0] = u[3] = u0;
		uu[1] = uu[2] = u1;
		u[1] = uu[0] = u[2] = uu[3] = (u0 + u1)/2.;

		v[0] = v[1] = v0;
		vv[2] = vv[3] = v1;
		v[2] = vv[0] = v[3] = vv[1] = (v0 + v1)/2.;
	}
/*
..... Break the area into two strips in each direction
*/
	else
	{
		u[0] = u[2] = u[3] = u0;
		uu[1] = uu[2] = uu[3] = u1;
		u[1] = uu[0] = (u0 + u1)/2.;

		v[0] = v[1] = v[2] = v0;
		vv[0] = vv[1] = vv[3] = v1;
		v[3] = vv[2] = (v0 + v1)/2.;
	}
	return;
}

/*********************************************************************
** FUNCTION: UU_LOGICAL S_triangle_valid (treflg,tript,p,uv,tolsq)
**
** Determine if a triangle is non-degenerate.
**
** PARAMETERS
**    INPUT:
**            treflg - triangular surface flag
**            tript  - triangle indices
**            p      - list of XYZ points
**            uv     - list of UV points
**            tolsq  - squared tolerance
**    OUTPUT:
**            none
**
** RETURNS:	true if valid, else false
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
static UU_LOGICAL S_triangle_valid (treflg,tript,p,uv,tolsq)
int treflg;
UM_tript *tript;
UM_coord *p;
UM_2Dcoord *uv;
UU_REAL tolsq;
{
	int i1,i2,i3,ix,k;
	UU_REAL w;
	UU_REAL eps = 1.e-5;

	i1 = tript->n1; i2 = tript->n2; i3 = tript->n3;

	if (treflg > 0 && treflg < 5)
	{
		w = 0; ix = 0;
		if (treflg == 2)
		{
			w = 1; ix = 0;
		}
		else if (treflg == 3)
		{
			w = 0; ix = 1;
		}
		else if (treflg == 4)
		{
			w = 1; ix = 1;
		}

		k = 0;
		eps = 1.e-5;

		if (fabs (uv[i1][ix] - w) < eps) k++;
		if (fabs (uv[i2][ix] - w) < eps) k++;
		if (fabs (uv[i3][ix] - w) < eps) k++;

		if (k > 1) return (UU_FALSE);
	}

	return (UU_TRUE);
}

/*********************************************************************
** FUNCTION : int ncl_edges_to_tess (edge0,nedge,p,norm,uv,np,t,tolsq)
**
** Fill tessellation structure using edges data
**
** PARAMETERS
**    INPUT :
**       edge0 - tessellation edges
**       nedge - number of tessellation edges
**       p     - tessellation vertices (3-D)
**       np    - number of tessellation vertices
**       norm  - normals at vertices
**       uv    - surf. parameters at vertices
**    OUTPUT :
**       triangles - list of tessellation triangles
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_edges_to_tess (edge0,nedge,p,norm,uv,np,t,tolsq)
UM_edge *edge0;
UM_coord *p;
UM_2Dcoord *uv;
UM_vector *norm;
int nedge,np;
UM_tessellation *t;
UU_REAL tolsq;
{
	UM_edge *edge;
	int tempsub, tempsub2;
	UM_tript tript;
	int i,*checked,k,k0,status;

	if (nedge < 3) return (UU_FAILURE);
/*
..... allocate memory for the "worst" case of k x k lattice
*/
	if (UU_LIST_NULLPTR (&t->tri))
	{
		k = (2*nedge)/3;
		status = uu_list_init1 (&t->tri, sizeof (UM_tript), k, k);
		if (status != UU_SUCCESS)
		{
			UM_int2 ier=-482;
			uwarn(&ier);
			return (status);
		}
	}

	checked = (int *) uu_malloc (nedge*sizeof (int));
	if (!checked) return (UU_FAILURE);

	k = np + 1;
	if (UU_LIST_NULLPTR (&t->vertices))
	{
		status = uu_list_init1 (&t->vertices, sizeof (UM_coord), k, k);
		if (status != UU_SUCCESS) return (status);
	}
	uu_list_push_multiple (&t->vertices,np,p);

	if (UU_LIST_NULLPTR (&t->normals))
	{
		status = uu_list_init1 (&t->normals, sizeof (UM_vector), k, k);
		if (status != UU_SUCCESS) return (status);
	}
	uu_list_push_multiple (&t->normals,np,norm);
/*
	if (UU_LIST_NULLPTR (&t->uv))
	{
		status = uu_list_init1 (&t->uv, sizeof (UM_2Dcoord), k, k);
		if (status != UU_SUCCESS) return (status);
	}
	uu_list_push_multiple (&t->uv,np,uv);
*/
	for (i = 0; i < nedge; i++) checked[i] = 0;

	for (i = 0, edge = edge0; i < nedge; i++, edge++)
	{
		if (checked[i]) continue;

		checked [i] = 1;
		tempsub = edge->south;
		tript.n1 = tempsub;
		tempsub = edge->north;
		tript.n2 = tempsub;

		k0 = edge->south + edge->north;
		tempsub = edge->n_w;
		tempsub2 = edge->s_w;
		if (tempsub >= 0 && tempsub2 >= 0 &&
			!checked[tempsub] && !checked[tempsub2])
		{
			k = (edge0[tempsub].south + edge0[tempsub].north +
				edge0[tempsub2].south + edge0[tempsub2].north - k0)/2;
			tript.n3 = k;
			if (um_is_triangle_CCW (&tript,p,norm) == UU_FALSE)
			{
				k = tript.n3; tript.n3 = tript.n2; tript.n2 = k;
			}
			if (S_triangle_valid (NCL_triansf,&tript,p,uv,tolsq))
			{
			uu_list_push (&t->tri,&tript);
			}
		}
		tempsub = edge->n_e;
		tempsub2 = edge->s_e;
		if (tempsub >= 0 && tempsub2 >= 0 &&
			!checked[tempsub] && !checked[tempsub2])
		{
			k = (edge0[tempsub].south + edge0[tempsub].north +
				edge0[tempsub2].south + edge0[tempsub2].north - k0)/2;
			tript.n3 = k;
			if (um_is_triangle_CCW (&tript,p,norm) == UU_FALSE)
			{
				k = tript.n3; tript.n3 = tript.n2; tript.n2 = k;
			}
			if (S_triangle_valid (NCL_triansf,&tript,p,uv,tolsq))
			{
			uu_list_push (&t->tri,&tript);
			}
		}
	}

	t->np = np;
	t->ntri = UU_LIST_LENGTH (&t->tri);

	uu_free (checked);

	return (UU_SUCCESS);
}

/*********************************************************************
** FUNCTION : int S_add_triangles (tess1,tess)
**
** Converts edges (UM_tess1) structure into triangles (UM_tessellation).
**
** PARAMETERS
**    INPUT :
**       tess  - tessellation with edges
**    OUTPUT :
**       t - tessellation with triangles
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_add_triangles (tess1,t,tolsq)
UM_tess1 *tess1;
UM_tessellation *t;
UU_REAL tolsq;
{
	UM_edge *edge;
	UM_coord *p;
	UM_2Dcoord *uv;
	UM_vector *norm;
	int n,np,status;

	edge = (UM_edge *) UU_LIST_ARRAY (&tess1->edges);
	n = UU_LIST_LENGTH (&tess1->edges);
	p = (UM_coord *) UU_LIST_ARRAY (&tess1->vertices);
	norm = (UM_vector *) UU_LIST_ARRAY (&tess1->normals);
	uv = (UM_2Dcoord *) UU_LIST_ARRAY (&tess1->uv);
	np = UU_LIST_LENGTH (&tess1->vertices);

	status = ncl_edges_to_tess (edge,n,p,norm,uv,np,t,tolsq);

	return (status);
}

/*********************************************************************
** I_FUNCTION : int S_tess_1tri (srf,tfmat,bound,tol, tess)
**    Get tessellation in the trivial case of one-triangle boundary.
** PARAMETERS
**          srf   - pointer to surface record
**          tfmat - transformation matrix of input surface
**          bound - boundary struct
**          tol   - tolerance
**       OUTPUT :
**          tess  - filled tessellation structure
**    RETURNS      :
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_tess_1tri (srf,tfmat,bound,tol, tess)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_srf_boundary *bound;
UU_REAL tol;
UM_tessellation *tess;
{
	int status,k,N1,N3;
	UM_coord *uvs;
	UM_2Dcoord uv;
	UM_tript tript;
	UU_REAL tolsq = tol*tol;

	N1 = 1; N3 = 3;
	status = UU_SUCCESS;

	if (UU_LIST_NULLPTR (&tess->vertices))
	{
		status = uu_list_init1 (&tess->vertices, sizeof (UM_coord), N3, N3);
		if (status != UU_SUCCESS) return (status);
	}
	if (UU_LIST_NULLPTR (&tess->normals))
	{
		status = uu_list_init1 (&tess->normals, sizeof (UM_coord), N3, N3);
		if (status != UU_SUCCESS) return (status);
	}
/*
	if (UU_LIST_NULLPTR (&tess->uv))
	{
		status = uu_list_init1 (&tess->uv, sizeof (UM_2Dcoord), N3, N3);
		if (status != UU_SUCCESS) return (status);
	}
*/
	if (UU_LIST_NULLPTR (&tess->tri))
	{
		status = uu_list_init1 (&tess->tri, sizeof (UM_tript), N1, N1);
		if (status != UU_SUCCESS) return (status);
	}
/*
..... store points
*/
	uvs = (UM_coord *)UU_LIST_ARRAY(bound->uvpts);
	for (k = 0; k < N3; k++)
	{
		uv[0] = uvs[k][0]; uv[1] = uvs[k][1];
		status = uc_evsrf(UM_NORM,uv[0],uv[1],srf,tfmat, &Sevsrf);
		if (status != UU_SUCCESS) return (status);
		uu_list_push (&tess->vertices,&Sevsrf.sp);

		if (NCL_triansf > 0)
		ncl_triansf_fix_norm (uv[0],uv[1],srf,tfmat,Sevsrf.snorm,tolsq);

		uu_list_push (&tess->normals,&Sevsrf.snorm);
		/* uu_list_push (&tess->uv,&uv); */
	}

	tess->np = N3;
	tess->ntri = N1;

	tript.n1 = 0;
	tript.n2 = 1;
	tript.n3 = 2;
	uu_list_push (&tess->tri,&tript);

	return (status);
}

/*********************************************************************
** FUNCTION : int ncl_get_tess_triangles (tess,triangles,itsk, noerr)
**
** Obtain the list of triangles from the tessellation structure.
**
** PARAMETERS
**    INPUT :
**       tess - tessellation
**       itsk - 0: get only vertices, allocate memory for triangles;
**              1: get vertices and normals, allocate memory for triangles;
**              2: get only vertices, do not allocate memory
**       noerr - do not output error iff 1
**    OUTPUT :
**       triangles - list of tessellation triangles
**    RETURNS      :
**       UU_FAILURE iff no triangles or memory allocation problem, else
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_tess_triangles (tess,triangles,itsk,noerr)
UM_tessellation *tess;
UU_LIST *triangles;
int itsk;
int noerr;
{
	UM_triangle triangle;
	UM_trian trian;
	UM_tript *tript;
	int n,i,n1,n2,n3,istat,np,nv;
	UM_coord *p;
	UM_vector *norm;

	n = tess->ntri;
	if (n < 1) return (UU_FAILURE);
	istat = UU_SUCCESS;
/*
..... allocate memory
*/
	if (itsk == 1)
		istat = uu_list_init1 (triangles, sizeof (UM_triangle),n,n);
	else if (itsk == 0)
		istat = uu_list_init1 (triangles, sizeof (UM_trian),n,n);
	if (istat != UU_SUCCESS)
	{
		if (noerr == 0)
		{
			UM_int2 ier=-482;
			uwarn(&ier);
		}
		return (UU_FAILURE);
	}

	p = (UM_coord *) UU_LIST_ARRAY (&tess->vertices);
	norm = (UM_vector *) UU_LIST_ARRAY (&tess->normals);
	tript = (UM_tript *) UU_LIST_ARRAY (&tess->tri);
	np = UU_LIST_LENGTH(&tess->vertices);
	nv = UU_LIST_LENGTH(&tess->normals);

	for (i = 0; i < n; i++)
	{
		n1 = tript[i].n1; n2 = tript[i].n2; n3 = tript[i].n3;
		if (itsk == 1)
		{
			um_vctovc (p[n1],triangle.p1);
			um_vctovc (p[n2],triangle.p2);
			um_vctovc (p[n3],triangle.p3);
			if (np == nv)
			{
				um_vctovc (norm[n1],triangle.norm1);
				um_vctovc (norm[n2],triangle.norm2);
				um_vctovc (norm[n3],triangle.norm3);
			}
			else
			{
				um_vctovc (norm[i],triangle.norm1);
				um_vctovc (norm[i],triangle.norm2);
				um_vctovc (norm[i],triangle.norm3);
			}
			uu_list_push (triangles, &triangle);
		}
		else
		{
			um_vctovc (p[n1],trian.p1);
			um_vctovc (p[n2],trian.p2);
			um_vctovc (p[n3],trian.p3);
			uu_list_push (triangles, &trian);
		}
	}

	return (UU_SUCCESS);
}

/*********************************************************************
** FUNCTION : int ncl_get_tess_uvtes (tess,uvtes,itsk, noerr)
**
** Obtain the list of triangles from the tessellation structure.
**
** PARAMETERS
**    INPUT :
**       tess - tessellation
**       uvtes - uv list point
**    OUTPUT :
**       uvtlst - list of tessellation triangles uv point
**    RETURNS      :
**       UU_FAILURE if no triangles or memory allocation problem
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_tess_uvtes (tess,uvtes, uvtlst)
UM_tessellation *tess;
UU_LIST *uvtes;
UU_LIST *uvtlst;
{
	UM_trian trian;
	UM_tript *tript;
	int n,i,n1,n2,n3;
	UM_coord *uv;

	n = tess->ntri;
	if (n < 1) return (UU_FAILURE);

	uv = (UM_coord *) UU_LIST_ARRAY (uvtes);
	tript = (UM_tript *) UU_LIST_ARRAY (&tess->tri);

	for (i = 0; i < n; i++)
	{
		n1 = tript[i].n1; n2 = tript[i].n2; n3 = tript[i].n3;

		um_vctovc (uv[n1],trian.p1);
		um_vctovc (uv[n2],trian.p2);
		um_vctovc (uv[n3],trian.p3);
		uu_list_push (uvtlst, &trian);
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**  FUNCTION : int ncl_get_tessellation (srf, tess)
**
**     Constructs triangular tessellation of the specified surface.
**     Calculates and stores tess. in Unibase or reads it from Unibase
**     if it already exists.
**
**  PARAMETERS
**     INPUT  :
**        srf   - RBS, NCL, TRIM or MESH surface
**     OUTPUT :
**        tess  - tessellation
**  RETURNS      :
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int ncl_get_tessellation (srf, tess)
struct NCL_fixed_databag *srf;
UM_tessellation *tess;
{
	int status;

	status = ncl_get_surflist (TESSELLATION_LIST,srf,tess);
	if (status != UU_SUCCESS)
	{
		status = uc_srf_tessellate (srf,tess);
		if (status == UU_SUCCESS)
			status = ncl_store_surflist (TESSELLATION_LIST,srf,tess);
	}

	return (status);
}

/*********************************************************************
**  FUNCTION : ncl_xform_tessellation(tess,tf)
**
**     Transforms triangular tessellation through a matrix.
**
**  PARAMETERS
**     INPUT  :
**        tess  - Triangular tessellation
**        tf    - Transformation matrix
**     OUTPUT :
**        tess  - Transformed tessellation
**  RETURNS      : none
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
void ncl_xform_tessellation(tess,tf)
UM_tessellation *tess;
UM_transf tf;
{
	int np,nv,i;
	UM_coord *pts;
	UM_vector *vcs;
/*
.....Transform vertices
*/
	pts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
	np = UU_LIST_LENGTH(&tess->vertices);
	for (i=0;i<np;i++) um_cctmtf(pts[i],tf,pts[i]);
/*
.....Transform normals
*/
	vcs = (UM_vector *)UU_LIST_ARRAY(&tess->normals);
	nv = UU_LIST_LENGTH(&tess->normals);
	for (i=0;i<nv;i++) um_vctmtf(vcs[i],tf,vcs[i]);
}

/*********************************************************************
**  FUNCTION : int ncl_tess_surfbase (srf,tfmat,tess,tol,typ,nupts,nvpts)
**     Constructs triangle tessellation of a surface
**     without trimmed boundary(base surface).
**  PARAMETERS
**     INPUT  :
**        srf   - pointer to surface entity
**        tfmat - surface matrix
**        typ   - UM_TESS_TOLER - use only told;
**                UM_TESS_GRID - evolve npts points;
**                UM_TESS_BOTH - use told but evolve no less than
**                                   nupts x nvpts points;
**        nupts,nvpts  - (min) number of points per isoparametric curves
**     OUTPUT :
**        tess - tessellation
**  RETURNS      :
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int ncl_tess_surfbase (srf,tfmat,bndr,tess,tol,typ,nupts,nu0,nvpts,nv0,uvtes)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_srf_boundary *bndr;
UM_tessellation *tess;
UU_REAL tol;
UM_tess_settype typ;
int nupts,nvpts,nu0,nv0;
UU_LIST *uvtes;
{
	struct NCL_fixed_databag bsrf,*bs;
	int nu,nv,status,np,mintri,minedges;
	UU_REAL *uu,*vv;
	UU_REAL tolsq = tol*tol;
	UU_LIST upt,vpt;
	UU_LOGICAL trimd,revsur;
	UM_tess1 tess1;
	UU_LOGICAL firstry,inits1;
	int cone = 0;

	status = UU_SUCCESS;

	uu_list_init0 (&upt);
	uu_list_init0 (&vpt);
	uu_list_init0 (&Slst2);
	inits1 = UU_FALSE;
	Scvpts = bndr->cvpts;

	np = 0;
	trimd = (ncl_itsa_trimsrf(srf));
	if (trimd)
	{
		bs = &bsrf;
		bs->key = ((struct NCL_trimsf_rec *)srf)->bs_key;
		status = ncl_retrieve_data_fixed (bs);
		if (status != UU_SUCCESS) goto Done;

		np = bndr->np[0];
		if (np < 3 ) goto Done;

		if (status != UU_SUCCESS) goto Done;
	}
	else
	{
		bs = srf;
/*
..... enable UM_TESS_WATRLN logic below
*/
		if (bndr->np != UU_NULL)
			np = bndr->np[0];
		else
			np = 5;
	}

	ncl_get_sf_treflag (bs,&NCL_triansf,tol);

	mintri = 2; minedges = 5;

	if (np < 8 || NCL_triansf > 0)
	{
		mintri = 1; minedges = 3;
	}

	revsur = (bs->rel_num == NCL_REVSURF_REL);

	status = uu_list_init1 (&upt, sizeof(UU_REAL), 200, 200);
	if (status == UU_SUCCESS)
		status = uu_list_init1 (&vpt, sizeof(UU_REAL), 200, 200);
	if (status != UU_SUCCESS) goto Done;
	uu_list_init (&Slst2, sizeof(UM_coord), 0, 200);

Retry:
/*
..... divide the surface into (rectangular in UV-space) panels, according to
..... tolerance; the panels are then tessellated in UV-space - we assume that
..... any triangle inside a panel is within tolerance.
*/
	if (typ == UM_TESS_WATRLN)
	{
		if (ncl_is_watrev() && np >= 50)
		{
			typ = UM_TESS_BOTH;
			nupts = nvpts = 5;
		}
		else
			typ = UM_TESS_TOLER;
	}

	nu = ncl_evolve_uvpts (revsur,bs,tfmat,bndr,nupts,nu0,1,tol,&upt,typ);
	if (nu < 2) goto Done;
	nv = ncl_evolve_uvpts (revsur,bs,tfmat,bndr,nvpts,nv0,2,tol,&vpt,typ);
	if (nv < 2) goto Done;

	if (trimd)
		ncl_add_extrema (bndr,&nu,&upt,&nv,&vpt);

	if (typ != UM_TESS_GRID)
		S_assure_flatness (bs,tfmat,&nu,&upt,&nv,&vpt,tol);

	uu = (UU_REAL *)UU_LIST_ARRAY(&upt);
	vv = (UU_REAL *)UU_LIST_ARRAY(&vpt);

	if (status == UU_SUCCESS)
		status = ncl_tess_uvboxes (bs,tfmat,nu,uu,nv,vv,cone,tol, tess, uvtes);
Done:

	if (tess->ntri < mintri || status != UU_SUCCESS)
	{
/*
..... If the tessellation fails, or the surface is too small,
..... display the wireframe
*/
		um_clean_tess (tess);
		status = UU_FAILURE;
	}

	uu_list_free(&upt);
	uu_list_free(&vpt);
	uu_list_free(&Slst2);

	NCL_triansf = 0;

	return (status);
}

