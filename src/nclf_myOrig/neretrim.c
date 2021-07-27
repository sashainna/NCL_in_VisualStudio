/*********************************************************************
**    NAME         :  neretrim.c
**       CONTAINS: Fortran interface, display, and evaluator routines
**                 for trimming surfaces:
**
**           void trmpln
**           void trmrdf
**           void extrdf
**           void ncl_vc_to_uv
**           int ncl_create_rbsplin0
**           ncl_extend_vc
**           ncl_extend_bndry
**           ncl_extend_bndry1
**           ncl_fit_chain
**           ncl_create_uvsplin
**           ncl_create_ssplin0
**           ncl_comp_bndr
**           ncl_contour_curve3d
**           ncl_wrong_dir
**           ncl_boxpol
**           ncl_boxbndry
**           ncl_cvbndry
**           ncl_isectbndry
**           ncl_isectbndry1
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       neretrim.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:45
*********************************************************************/
#include "udebug.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "nclx.h"
#include "ncl.h"
#include "nclfc.h"
#include "nccs.h"
#include "mattr.h"
#include "mdattr.h"
#include "mgeom.h"
#include "modef.h"
#include "class.h"
#include "nclclip.h"

#define RANDOM 0
#define UVMOD 1
#define VCDIR 2
#define LARGST 3
#define SMALST 4

#define NULLPOL (ncl_polygon *)UU_NULL

extern int nclu_temp_redef;
extern UU_LIST NCL_uvintof;
extern UU_KEY_ID NCL_skey0;
UU_LOGICAL NCL_noname = UU_FALSE;
UU_KEY_ID NCL_nokey = NULLKEY;

/*********************************************************************
**    E_FUNCTION     : void ncl_vc_to_uv (srf,tfmat,vcm,modtype,um,vm)
**       Translate a vector modifier (could be a real vector or the vector
**       defined by an XLARGE,...,ZSMALL) to a UV-modifier.
**    PARAMETERS
**       INPUT  :
**          srf     - surface structure
**          tfmat   - surface associated matrix
**          vcm     - vector modifier
**       OUTPUT :
**          modtype - new modifier type: UVMOD if success, else RANDOM
**          um,vm   - UV coordinates for the UVMOD modifier
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_vc_to_uv (srf,tfmat,vcm,modtype,um,vm)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_vector vcm;
int *modtype;
UU_REAL *um,*vm;
{
	int i,status,npts;
	UM_coord *uvp;
	UM_coord spt;
	UM_vector snorm;
	UU_REAL u,v,side;
	struct UM_evsrfout evsrf;

	npts = UU_LIST_LENGTH (&NCL_uvintof) / 3;
	if (npts < 2) goto Err;
	uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);

	i = (npts-1)/2;
	u = uvp[i][0]; v = uvp[i][1];

	uc_evsrf (UM_POINT, u, v, srf, tfmat, &evsrf);

	um_vcplvc (evsrf.sp,vcm,spt);
	side = 0.;
	status = ncl_pt_proj_sf(spt,srf,&u,&v,&side,0,UU_NULL,0.0,spt,snorm);
	if (status == UU_SUCCESS)
	{
		*modtype = UVMOD;
		*um = u; *vm = v;
		return;
	}

Err:
	*modtype = RANDOM;
	return;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_create_rbsplin0 (ckey)
**       Creates a spline from points calculated in cvio. The points
**       must form a closed polyline.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          ckey    - spline key
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_create_rbsplin0 (ckey)
UU_KEY_ID *ckey;
{
	int status = UU_SUCCESS;
	UM_coord *uvp;
	int i,n;
	struct UM_rbsplcrv_rec cv;
	struct NCL_crvgen_rec *segp = UU_NULL;


	n = UU_LIST_LENGTH (&NCL_uvintof) / 3;
	uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
	if (n < 3 || UM_DIST_2D(uvp[0],uvp[n-1]) > UM_FUZZ) return (UU_FAILURE);

	segp = (struct NCL_crvgen_rec *) uu_malloc (n*sizeof(struct NCL_crvgen_rec));
	if (segp == UU_NULL) return (-1);

	for (i = 0; i < n; i++)
	{
		ncl_init_seg (&segp[i]);
		segp[i].x = uvp[i][0];
		segp[i].y = uvp[i][1];
		segp[i].z = uvp[i][2];
	}

	status = ncl_interp_rbsp (n, segp, 1, &cv);
	if (status == UU_SUCCESS)
		*ckey = cv.key;

	uu_free (segp);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_intof_box (u0,u1,v0,v1)
**       Calculate a min/max box for the NCL_uvintof points.
**    PARAMETERS
**       INPUT  :
**          NCL_uvintof   - UV curve
**       OUTPUT :
**          u0,u1,v0,v1   - the box
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_intof_box (u0,u1,v0,v1)
UU_REAL *u0,*v0,*u1,*v1;
{
	int i,npt;
	UM_coord *uvp;
	UU_REAL u,v;

	uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
	npt = UU_LIST_LENGTH (&NCL_uvintof) / 3;
	for (i = 0; i < npt; i++)
	{
		u = uvp[i][0]; v = uvp[i][1];
		if (u < *u0) *u0 = u;
		else if (u > *u1) *u1 = u;
		if (v < *v0) *v0 = v;
		else if (v > *v1) *v1 = v;
	}
}

/*********************************************************************
**    I_FUNCTION     : ncl_extend_vc(u0,u1,v0,v1,u00,u11,v00,v11,uvc,pt0)
**       Extend a UV curve, if necessary, so that it starts and ends on
**       the box boundary.
**    PARAMETERS
**       INPUT  :
**          NCL_uvintof   - UV curve
**          u0,u1,v0,v1   - the box
**          eps           - UV tolerance
**       OUTPUT :
**          NCL_uvintof   - changed curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_extend_vc (u0,u1,v0,v1,u00,u11,v00,v11,uvc,pt0,eps)
UU_REAL u0,v0,u1,v1,u00,u11,v00,v11,eps;
UM_vector uvc;
UM_coord pt0;
{
	UU_REAL u,v;

	u = pt0[0]; v = pt0[1];

	if (fabs (v - v00) < eps || fabs (v - v11) < eps)
	{
		if (uvc[0] > 0 && fabs (u1-u11) < eps)
			pt0[0] = u1;
		else if (uvc[0] < 0 && fabs (u0-u00) < eps)
			pt0[0] = u0;
		else if (uvc[0] == 0)
		{
			if (fabs (u0-u00) >= eps || u-u0 >= u1-u)
				pt0[0] = u1;
			else
				pt0[0] = u0;
		}
		else
			return (-1);
	}
	else if (fabs (u - u00) < eps || fabs (u - u11) < eps)
	{
		if (uvc[0] > 0 && fabs (v1-v11) < eps)
			pt0[1] = v1;
		else if (uvc[0] < 0 && fabs (v0-v00) < eps)
			pt0[1] = v0;
		else if (uvc[0] == 0)
		{
			if (fabs (v0-v00) >= eps || v-v0 >= v1-v)
				pt0[1] = v1;
			else
				pt0[1] = v0;
		}
		else
			return (-1);
	}
	else
		return (-1);

	return (0);
}

/*********************************************************************
**    I_FUNCTION     : ncl_extend_bndry1 (u0,u1,v0,v1,u00,u11,v00,v11,lc0,lc1)
**       Extend a UV curve, if necessary, so that it starts and ends on
**       the box boundary.
**    PARAMETERS
**       INPUT  :
**          NCL_uvintof   - UV curve
**          u0,u1,v0,v1   - the box
**       OUTPUT :
**          NCL_uvintof   - changed curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_extend_bndry1 (u0,u1,v0,v1,u00,u11,v00,v11,lc0,lc1)
UU_REAL u0,v0,u1,v1,u00,u11,v00,v11;
UU_LOGICAL *lc0,*lc1;
{
	int nins,npts,inside;
	UM_coord *uvp;
	UM_vector uvc;
	UM_coord pti,pt0;
	UU_REAL di,d0;
	UU_REAL eps;

	pti[2] = pt0[2] = 0;
	nins = 3;

	if (ncl_setver(96))
		eps = UM_FUZZ;
	else
		eps = 1.e-5;

	uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
	npts = UU_LIST_LENGTH (&NCL_uvintof) / 3;

	if (npts < 2) return (-1);
	inside = um_point_inside_panel (u0,u1,v0,v1,uvp[0],eps);
	if (inside == 1)
	{
		um_vcmnvc (uvp[0],uvp[1],uvc);
		um_unitvc_2d (uvc,uvc);
		if (um_to_edge1 (u0,u1,v0,v1,uvp[0],uvc,pti) == 1)
		{
			di = UM_DIST_2D(pti,uvp[0]);
			if (di > eps)
			{
				d0 = 1000;
				if (um_to_edge1 (u00,u11,v00,v11,uvp[0],uvc,pt0) == 1)
					d0 = UM_DIST_2D(pt0,uvp[0]);
				if (d0 + eps < di)
				{
					if (d0 > eps)
						uu_list_insert_multiple (&NCL_uvintof,0,nins,pt0);
					if (ncl_extend_vc (u0,u1,v0,v1,u00,u11,v00,v11,uvc,pt0,eps) != 0)
						return (-1);
					else
						uu_list_insert_multiple (&NCL_uvintof,0,nins,pt0);
					*lc0 = UU_TRUE;
				}
				else
					uu_list_insert_multiple (&NCL_uvintof,0,nins,pti);
				uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
				npts = UU_LIST_LENGTH (&NCL_uvintof) / 3;
			}
			else
			{
				uvp[0][0] = pti[0]; uvp[0][1] = pti[1];
			}
		}
	}

	inside = um_point_inside_panel (u0,u1,v0,v1,uvp[npts-1],eps);
	if (inside == 1)
	{
		um_vcmnvc (uvp[npts-1],uvp[npts-2],uvc);
		um_unitvc_2d (uvc,uvc);
		if (um_to_edge1 (u0,u1,v0,v1,uvp[npts-1],uvc,pti) == 1)
		{
			di = UM_DIST_2D(pti,uvp[npts-1]);
			if (di > eps)
			{
				d0 = 1000;
				if (um_to_edge1 (u00,u11,v00,v11,uvp[npts-1],uvc,pt0) == 1)
					d0 = UM_DIST_2D(pt0,uvp[npts-1]);
				if (d0 + eps < di)
				{
					if (d0 > eps)
						uu_list_push_multiple (&NCL_uvintof,nins,pt0);
					if (ncl_extend_vc (u0,u1,v0,v1,u00,u11,v00,v11,uvc,pt0,eps) != 0)
						return (-1);
					else
						uu_list_push_multiple (&NCL_uvintof,nins,pt0);
					*lc1 = UU_TRUE;
				}
				else
					uu_list_push_multiple (&NCL_uvintof,nins,pti);
			}
			else
			{
				uvp[npts-1][0] = pti[0]; uvp[npts-1][1] = pti[1];
			}
		}
	}
	return (0);
}

/*********************************************************************
**    I_FUNCTION     : ncl_extend_bndry (u0,u1,v0,v1)
**       Extend a UV curve, if necessary, so that it starts and ends on
**       the box boundary.
**    PARAMETERS
**       INPUT  :
**          NCL_uvintof   - UV curve
**          u0,u1,v0,v1   - the box
**       OUTPUT :
**          NCL_uvintof   - changed curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_extend_bndry (u0,u1,v0,v1)
UU_REAL u0,v0,u1,v1;
{
	int nins,npts,inside;
	UU_REAL eps;
	UM_coord *uvp;
	UM_vector v;
	UM_coord pti;

	pti[2] = 0;
	nins = 3;

	if (ncl_setver(96))
		eps = UM_FUZZ;
	else
		eps = 1.e-5;

	uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
	npts = UU_LIST_LENGTH (&NCL_uvintof) / 3;

	if (npts < 2) return;
	inside = um_point_inside_panel (u0,u1,v0,v1,uvp[0],eps);
	if (inside == 1)
	{
		um_vcmnvc (uvp[0],uvp[1],v);
		um_unitvc_2d (v,v);
		if (um_to_edge1 (u0,u1,v0,v1,uvp[0],v,pti) == 1)
		{
			if (UM_DIST_2D(pti,uvp[0]) > eps)
			{
				uu_list_insert_multiple (&NCL_uvintof,0,nins,pti);
				uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
				npts = UU_LIST_LENGTH (&NCL_uvintof) / 3;

			}
			else
			{
				uvp[0][0] = pti[0]; uvp[0][1] = pti[1];
			}
		}
	}

	inside = um_point_inside_panel (u0,u1,v0,v1,uvp[npts-1],eps);
	if (inside == 1)
	{
		um_vcmnvc (uvp[npts-1],uvp[npts-2],v);
		um_unitvc_2d (v,v);
		if (um_to_edge1 (u0,u1,v0,v1,uvp[npts-1],v,pti) == 1)
		{
			if (UM_DIST_2D(pti,uvp[npts-1]) > eps)
				uu_list_push_multiple (&NCL_uvintof,nins,pti);
			else
			{
				uvp[npts-1][0] = pti[0]; uvp[npts-1][1] = pti[1];
			}
		}
	}
}

/*********************************************************************
**    FUNCTION : void ncl_fit_chain (u0,v0,polygon,pc,nc,bps)
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
**    RETURNS  : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_fit_chain (u0,v0,polygon,pc,nc,bps)
UU_REAL u0,v0;
UM_coord *pc;
int nc;
UU_LIST *polygon,*bps;
{
	UU_LIST plst,chain;
	int ni,np,i;
	UM_coord pti,*pp;
	UM_2Dcoord vci,*vc;

	np = polygon->cur_cnt;
	pp = (UM_coord *) UU_LIST_ARRAY (polygon);

	ni = np;
	if (ni < 5) ni = 5;
	uu_list_init (&plst, sizeof(UM_2Dcoord), ni, ni);
	for (i = 0; i < np; i++)
	{
		um_vctovc_2d (pp[i],vci);
		uu_list_push (&plst,vci);
	}

	ni = nc;
	if (ni < 5) ni = 5;
	uu_list_init (&chain, sizeof(UM_2Dcoord), ni, ni);
	for (i = 0; i < nc; i++)
	{
		um_vctovc_2d (pc[i],vci);
		uu_list_push (&chain,vci);
	}
	vc = (UM_2Dcoord *) UU_LIST_ARRAY (&chain);

	um_fit_chain (u0,v0,&plst,vc,nc,bps);

	UU_LIST_EMPTY (polygon);
	vc = (UM_2Dcoord *) UU_LIST_ARRAY (&plst);
	np = plst.cur_cnt;

	pti[2] = 0;
	for (i = 0; i < np; i++)
	{
		um_vctovc_2d (vc[i],pti);
		uu_list_push (polygon,pti);
	}

	uu_list_free (&plst);
	uu_list_free (&chain);
}

/*********************************************************************
**    E_FUNCTION     : ncl_create_ssplin0(skey,pp,np,uley)
**       Create a surface spline out of a UV polyline.
**    PARAMETERS
**       INPUT  :
**          skey   - surface key
**          pp     - polyline points
**          np     - number of polyline points
**       OUTPUT :
**          ukey    - Key of UV curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_create_ssplin0(skey,pp,np,ukey)
UM_coord *pp;
int np;
UU_KEY_ID skey,*ukey;
{
	int i,n1;
	int status = UU_SUCCESS;
	UM_coord *pts;
	UM_vector *vs;
	struct NCL_crvgen_rec *segp = UU_NULL;

	segp = (struct NCL_crvgen_rec *)
		uu_malloc (np*sizeof(struct NCL_crvgen_rec));
	if (segp == UU_NULL) return (-1);
	n1 = np;
	for (i = 0; i < n1; i++)
	{
		ncl_init_seg (&segp[i]);
		segp[i].x = pp[i][0];
		segp[i].y = pp[i][1];
	}

	pts = vs = UU_NULL;
	status = ncl_interp_rbsp1 (n1, segp, 1, &n1, &vs, &pts);
	if (status == UU_SUCCESS)
		status = ncl_create_ssplin (&skey,vs,pts,&n1,ukey);

	uu_free (segp);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_create_uvsplin (skey,pp,np,ukey)
**       Create a surface spline out of a UV polyline.
**    PARAMETERS
**       INPUT  :
**          skey   - surface key
**          pp     - polyline points
**          np     - number of polyline points
**       OUTPUT :
**          ukey    - Key of UV curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_create_uvsplin (skey,pp,np,ukey)
UM_coord *pp;
int np;
UU_KEY_ID skey,*ukey;
{
	int status = UU_SUCCESS;
	UU_KEY_ID lkey;
	struct UM_uvcvonsf_rec *cuvp;
	struct NCL_fixed_databag cv,cv1;

	status = ncl_create_ssplin0(skey,pp,np,&lkey);
	if (status == UU_SUCCESS)
	{
		cuvp = (struct UM_uvcvonsf_rec *)&cv;

		cuvp->key = lkey;
		status = ncl_retrieve_data_fixed(&cv);
		if (status == UU_SUCCESS)
		{
			cv1.key = 0;
			status = ncl_create_rbsp (cuvp->no_pt,cuvp->t,cuvp->pt,&cv1);
			*ukey = cv1.key;
		}
	}
	uc_delete (lkey);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_comp_bndr (skey,uvp,npts,ukey)
**       Create a composite surface spline out of a closed UV polyline.
**    PARAMETERS
**       INPUT  :
**          sf     - surface key
**          uvp    - polyline points
**          npts   - number of polyline points
**       OUTPUT :
**          ukey    - Key of UV curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_comp_bndr (skey,polygon,bps,ukey)
UU_LIST *polygon,*bps;
UU_KEY_ID skey,*ukey;
{
	int i,j,np,nb,ni;
	int *jb;
	UM_coord *pp,pt;
	int status = UU_SUCCESS;
	UU_KEY_ID ki,*kuv;
	static UU_REAL s[4];
	UU_LIST lkylst;

	np = polygon->cur_cnt;
	pp = (UM_coord *) UU_LIST_ARRAY (polygon);

	if (UM_DIST_2D(pp[np-1],pp[0]) > UM_FUZZ)
	{
		pt[0] = pp[0][0]; pt[1] = pp[0][1]; pt[2] = 0;
		uu_list_push (polygon, pt);
		pp = (UM_coord *) UU_LIST_ARRAY (polygon);
		np++;
	}

	if (np < 4) return (-1);

	i = np-1;
	uu_list_push (bps,&i);

	nb = bps->cur_cnt;

	if (nb == 1)
	{
		status = ncl_create_uvsplin (skey,pp,np,ukey);
		return (status);
	}

	jb = (int *) UU_LIST_ARRAY (bps);

	uu_list_init (&lkylst,sizeof(UU_KEY_ID),nb,nb);

	for (i = j = 0; i < nb; j = jb[i], i++)
	{
		ni = jb[i] - j + 1;
		if (ni < 2) continue;
		if (ni == 2)
		{
			s[0] = s[1] = 0.0;
			s[2] = s[3] = 1.0;
			status = ncl_create_ssplin(&skey,s,&pp[j],&ni,&ki);
		}
		else if (ni > 2)
			status = ncl_create_ssplin0(skey,&pp[j],ni,&ki);
		if (status != UU_SUCCESS) break;
		uu_list_push (&lkylst,&ki);
	}

	nb = lkylst.cur_cnt;
	kuv = (UU_KEY_ID *) UU_LIST_ARRAY (&lkylst);

	if (status == UU_SUCCESS) ncl_create_uvcomp  (nb, kuv, ukey);

	for (i = 0; i < nb; i++)
	{
		if (kuv[i] != NULLKEY) uc_delete(kuv[i]);
	}

	uu_list_free (&lkylst);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_wrong_dir (u0,u1,v0,v1,polygon,modtype,um,vm)
**       Check if the calculated polygon contradicts the modifier.
**    PARAMETERS
**       INPUT  :
**          sf     - surface structure
**       OUTPUT :
**          ckey    - Key of XYZ curve
**          ukey    - Key of UV curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_wrong_dir (u0,u1,v0,v1,polygon,modtype,um,vm)
UU_REAL u0,u1,v0,v1,um,vm;
UU_LIST *polygon;
int modtype;
{
	int j,np,iret;
	UM_coord *pp,pt;
	UU_REAL area,A;
	UM_2Dcoord ummx,vmmx;

	np = polygon->cur_cnt;
	pp = (UM_coord *) UU_LIST_ARRAY (polygon);
	iret = 0;

	if (modtype == LARGST || modtype == SMALST)
	{
		if (UM_DIST_2D(pp[np-1],pp[0]) < UM_FUZZ) np--;

		for (j = 1, area = 0.; j < np-1; j++)
			area += um_triangle_signed_area(pp[0],pp[j],pp[j+1]);
		A = 0.5*(u1-u0)*(v1-v0);
		area = fabs(area);
		if ((modtype == LARGST && area < A) || (modtype == SMALST && area > A))
			iret = 1;
	}
	else if (modtype == UVMOD)
	{
		if (UM_DIST_2D(pp[np-1],pp[0]) > UM_FUZZ)
		{
			pt[0] = pp[0][0]; pt[1] = pp[0][1]; pt[2] = 0;
			uu_list_push (polygon, pt);
			pp = (UM_coord *) UU_LIST_ARRAY (polygon);
			np++;
		}
		ummx[0] = u0; vmmx[0] = v0;
		ummx[1] = u1; vmmx[1] = v1;

		pt[0] = um; pt[1] = vm; pt[2] = 0;
		if (um_cshape_inschk(pp,1,&np,pt,ummx,vmmx) < 0)
			iret = 1;
	}

	return (iret);
}

/*********************************************************************
**    E_FUNCTION     : ncl_boxpol(pol1)
**       Intersect a trimsf boundary with set of uv points.
**    PARAMETERS
**       INPUT  :
**          sf     - surface structure
**       OUTPUT :
**          ckey    - Key of XYZ curve
**          ukey    - Key of UV curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_boxpol(pol1)
ncl_polygon *pol1;
{
	UM_2Dcoord pt;
	UU_REAL u0,v0,u1,v1;

	u0 = v0 = 0;
	u1 = v1 = 1;

	pol1->num_contours = 1;
	pol1->np[0] = 4;
	pol1->box[0].xmin = u0;
	pol1->box[0].xmax = u1;
	pol1->box[0].ymin = v0;
	pol1->box[0].ymax = v1;

	pt[0] = u0; pt[1] = v0;
	uu_list_push (pol1->contour, &pt);
	pt[0] = u1; pt[1] = v0;
	uu_list_push (pol1->contour, &pt);
	pt[0] = u1; pt[1] = v1;
	uu_list_push (pol1->contour, &pt);
	pt[0] = u0; pt[1] = v1;
	uu_list_push (pol1->contour, &pt);

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_boxbndry(bkey,tfmat,ckey,ukey)
**       Intersect a trimsf boundary with set of uv points.
**    PARAMETERS
**       INPUT  :
**          sf     - surface structure
**       OUTPUT :
**          ckey    - Key of XYZ curve
**          ukey    - Key of UV curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_boxbndry(bkey,tfmat,ckey,ukey)
UM_transf tfmat;
UU_KEY_ID bkey,*ckey,*ukey;
{
	int i,status;
	UU_LIST bps,polygon;
	UM_coord pt;
	UU_REAL u0,v0,u1,v1;


	u0 = v0 = 0;
	u1 = v1 = 1;

	uu_list_init (&polygon, sizeof(UM_coord), 5, 5);
	uu_list_init (&bps, sizeof(int), 5, 5);

	pt[2] = 0;
	pt[0] = u0; pt[1] = v0;
	uu_list_push (&polygon, &pt);
	pt[0] = u1; pt[1] = v0;
	uu_list_push (&polygon, &pt);
	pt[0] = u1; pt[1] = v1;
	uu_list_push (&polygon, &pt);
	pt[0] = u0; pt[1] = v1;
	uu_list_push (&polygon, &pt);
/*
..... the bps list is used in um_fit_chain
*/
	for (i = 1; i < 4; i++)
		uu_list_push (&bps, &i);

	status = ncl_comp_bndr (bkey,&polygon,&bps,ukey);

	uu_list_free (&bps);
	uu_list_free (&polygon);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cvbndry(bkey,ckey,ukey)
**       Intersect a trimsf boundary with set of uv points.
**    PARAMETERS
**       INPUT  :
**          sf     - surface structure
**       OUTPUT :
**          ckey    - Key of XYZ curve
**          ukey    - Key of UV curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvbndry(bkey,ckey,ukey)
UU_KEY_ID bkey,*ckey,*ukey;
{
	int status;
	UU_KEY_ID key0 = *ckey;
	UM_real4 uvi[2];
	UM_int2 jerr;


	uvi[0] = uvi[1] = 0.5;
	status = ncl_trimsf_uvkey (key0,bkey,uvi,ukey,&jerr);
	if (status == UU_SUCCESS)
		ncl_trimsf_cvkey (key0,bkey,uvi,ckey,jerr);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_isectbndry1 (bkey,tfmat,bx1,ckey,ukey,pol1)
**       Intersect a trimsf boundary with set of uv points.
**    PARAMETERS
**       INPUT  :
**          sf     - surface structure
**       OUTPUT :
**          ckey    - Key of XYZ curve
**          ukey    - Key of UV curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_isectbndry1(bkey,tfmat,bx1,ckey,ukey,pol1)
UM_transf tfmat;
UU_KEY_ID bkey,*ckey,*ukey;
UM_real4 bx1[];
ncl_polygon *pol1;
{
	int i,npts;
	UM_coord *uvp;
	UU_LIST bps,polygon;
	UM_coord pt;
	UU_LOGICAL lc0,lc1,ltmp;
	UU_REAL u0,v0,u1,v1,u00,v00,u11,v11,um,vm;
	int irev,j,k;
	int modtype;
	UU_REAL tmp;
	int status = UU_SUCCESS;


	npts = UU_LIST_LENGTH (&NCL_uvintof) / 3;
	if (npts < 2) return (-1);

	uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
	if (UM_SQDIS_2D(uvp[0],uvp[npts-1]) < UM_DFUZZ)
	{
		if (pol1)
		{
			ncl_addto_pol (uvp,npts-1,pol1);
			status = UU_SUCCESS;
		}
		else
			status = ncl_create_uvsplin (bkey,uvp,npts,ukey);
		return (status);
	}

	u00 = bx1[0]; u11 = bx1[1];
	v00 = bx1[2]; v11 = bx1[3];

	if (u00 < 0.001) u00 = 0.;
	if (v00 < 0.001) v00 = 0.;
	if (u11 > 0.999) u11 = 1.;
	if (v11 > 0.999) v11 = 1.;

	modtype = UVMOD;
	um = (u00 + u11)/2; vm = (v00 + v11)/2;

	u0 = u00; v0 = v00;
	u1 = u11; v1 = v11;
	ncl_intof_box (&u0,&u1,&v0,&v1);
	lc0 = lc1 = UU_FALSE;
	status = ncl_extend_bndry1 (u0,u1,v0,v1,u00,u11,v00,v11,&lc0,&lc1);
	if (status != UU_SUCCESS) return (status);
	npts = UU_LIST_LENGTH (&NCL_uvintof) / 3;
	uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);

	uu_list_init (&polygon, sizeof(UM_coord), 100, 100);
	uu_list_init (&bps, sizeof(int), 10, 10);

Pol:
	pt[2] = 0;
	pt[0] = u0; pt[1] = v0;
	uu_list_push (&polygon, &pt);
	pt[0] = u1; pt[1] = v0;
	uu_list_push (&polygon, &pt);
	pt[0] = u1; pt[1] = v1;
	uu_list_push (&polygon, &pt);
	pt[0] = u0; pt[1] = v1;
	uu_list_push (&polygon, &pt);
/*
..... the bps list is used in um_fit_chain
*/
	for (i = 1; i < 4; i++)
		uu_list_push (&bps, &i);

	ncl_fit_chain (u0,v0,&polygon,uvp,npts,&bps);

	if (modtype != RANDOM)
	{
		irev = ncl_wrong_dir (u0,u1,v0,v1,&polygon,modtype,um,vm);
		if (irev == 1)
		{
			for (i = 0; i < npts/2; i++)
			{
				k = npts-1-i;
				for (j = 0; j < 3; j++)
				{
					tmp = uvp[i][j];
					uvp[i][j] = uvp[k][j];
					uvp[k][j] = tmp;
				}
			}
			UU_LIST_EMPTY (&polygon);
			UU_LIST_EMPTY (&bps);
			modtype = RANDOM;
			ltmp = lc0; lc0 = lc1; lc1 = ltmp;
			goto Pol;
		}
	}
	if (lc0 || lc1)
	{
		int *jb,j,nb;

		jb = (int *) UU_LIST_ARRAY (&bps);
		nb = bps.cur_cnt;

		for (i = 0; i < nb-1; i++)
		{
			if (jb[i+1] - jb[i] > 1) break;
		}
		if (i < nb-1)
		{
			if (lc0)
			{
				j = jb[i]+1;
				uu_list_insert (&bps, i+1, &j);
				i++;
				jb = (int *) UU_LIST_ARRAY (&bps);
			}
			if (lc1)
			{
				j = jb[i+1]-1;
				uu_list_insert (&bps, i+1, &j);
			}
		}
	}
	if (pol1)
	{
		npts = polygon.cur_cnt;
		uvp = (UM_coord *) UU_LIST_ARRAY (&polygon);

		if (UM_SQDIS_2D (uvp[0],uvp[npts-1]) < UM_DFUZZ)
			npts--;

		ncl_addto_pol (uvp,npts,pol1);
	}
	else
		status = ncl_comp_bndr (bkey,&polygon,&bps,ukey);
	if (status == UU_SUCCESS) goto Done;

Err:
	status = UU_FAILURE;

Done:
	uu_list_free (&bps);
	uu_list_free (&polygon);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_contour_curve3d (npts,uvp,bkey,ukey,tol)
**       Put 3D points into a 2Dcoord list; call ncl_contour_curve
*********************************************************************/
static int ncl_contour_curve3d (npts,uvp,bkey,ukey,tol)
int npts;
UM_coord *uvp;
UU_KEY_ID bkey,*ukey;
UU_REAL tol;
{
	int i,npol,status;
	UM_2Dcoord p2d,*ppol;
	UU_LIST pplst;

	uu_list_init (&pplst, sizeof(UM_2Dcoord), npts+1, npts);
	for (i=0;i<npts;i++)
	{
		p2d[0] = uvp[i][0];
		p2d[1] = uvp[i][1];
		uu_list_push (&pplst, &p2d);
	}
	npol = pplst.cur_cnt;
	ppol = (UM_2Dcoord *) UU_LIST_ARRAY (&pplst);

	status = ncl_contour_curve (ppol,npol,bkey,ukey,tol);

	uu_list_free (&pplst);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_isectbndry(bkey,tfmat,modtype,um,vm,ckey,ukey,pol1)
**       Intersect a trimsf boundary with set of uv points.
**    PARAMETERS
**       INPUT  :
**          sf     - surface structure
**       OUTPUT :
**          ckey    - Key of XYZ curve
**          ukey    - Key of UV curve
**
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_isectbndry(bkey,tfmat,modtype,um,vm,ckey,ukey,pol1)
UM_transf tfmat;
UU_KEY_ID bkey,*ckey,*ukey;
int modtype;
UU_REAL um,vm;
ncl_polygon *pol1;
{
	int i,npts;
	UM_coord *uvp;
	UU_LIST bps,polygon;
	UM_coord pt;
	UU_REAL tol,u0,v0,u1,v1;
	int status = UU_SUCCESS;
	UM_real8 tol8;

	gettol (&tol8);
	tol = tol8;

	npts = UU_LIST_LENGTH (&NCL_uvintof) / 3;
	if (npts < 2) return (-1);

	uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
	if (UM_SQDIS_2D(uvp[0],uvp[npts-1]) < UM_DFUZZ)
	{
		if (pol1)
		{
			ncl_addto_pol (uvp,npts-1,pol1);
			status = UU_SUCCESS;
		}
		else
			status = ncl_contour_curve3d (npts,uvp,bkey,ukey,tol);
		return (status);
	}

	u0 = v0 = 0;
	u1 = v1 = 1;
	ncl_extend_bndry (u0,u1,v0,v1);
	npts = UU_LIST_LENGTH (&NCL_uvintof) / 3;
	uvp = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);

	uu_list_init (&polygon, sizeof(UM_coord), 100, 100);
	uu_list_init (&bps, sizeof(int), 10, 10);

Pol:
	pt[2] = 0;
	pt[0] = u0; pt[1] = v0;
	uu_list_push (&polygon, &pt);
	pt[0] = u1; pt[1] = v0;
	uu_list_push (&polygon, &pt);
	pt[0] = u1; pt[1] = v1;
	uu_list_push (&polygon, &pt);
	pt[0] = u0; pt[1] = v1;
	uu_list_push (&polygon, &pt);
/*
..... the bps list is used in um_fit_chain
*/
	for (i = 1; i < 4; i++)
		uu_list_push (&bps, &i);

	ncl_fit_chain (u0,v0,&polygon,uvp,npts,&bps);

	if (modtype != RANDOM)
	{
		int irev,j,k;
		UU_REAL tmp;

		irev = ncl_wrong_dir (u0,u1,v0,v1,&polygon,modtype,um,vm);
		if (irev == 1)
		{
			for (i = 0; i < npts/2; i++)
			{
				k = npts-1-i;
				for (j = 0; j < 3; j++)
				{
					tmp = uvp[i][j];
					uvp[i][j] = uvp[k][j];
					uvp[k][j] = tmp;
				}
			}
			UU_LIST_EMPTY (&polygon);
			UU_LIST_EMPTY (&bps);
			modtype = RANDOM;
			goto Pol;
		}
	}
	if (pol1)
	{
		npts = polygon.cur_cnt;
		uvp = (UM_coord *) UU_LIST_ARRAY (&polygon);

		if (UM_SQDIS_2D (uvp[0],uvp[npts-1]) < UM_DFUZZ)
			npts--;

		ncl_addto_pol (uvp,npts,pol1);
	}
	else
	{
/*
.....replaced status = ncl_comp_bndr (bkey,&polygon,&bps,ukey);
.....with ncl_contour_curve which breaks up a set of uv points into
.....uv components and then passes them to ncl_comp_bndr.
*/
		npts = polygon.cur_cnt;
		uvp = (UM_coord *) UU_LIST_ARRAY (&polygon);
		status = ncl_contour_curve3d (npts,uvp,bkey,ukey,tol);
	}

	uu_list_free (&bps);
	uu_list_free (&polygon);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : void extrdf (ksf1,bx1,ncvs,uv,nkey,inamed,ierr)
**       Creates a NCL trimmed surface in UNIBASE and return the
**       unibase key of the surface. Fortran interface routine.
**    PARAMETERS
**       INPUT  :
**          ksf       - Key of base surface.
**          ncvs      - number of inner xyz trimming curves.
**          uv        - Starting surface u,v
**       OUTPUT :
**          nclkey    - Key of trimmed surface.
**          ierr      - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void extrdf (ksf1,bx1,ncvs,uv,nkey,inamed,ierr)
UM_int4 *ksf1,*nkey;
UM_int2 *ncvs,*inamed,*ierr;
UM_real4 uv[2],bx1[];
{
	UM_int2 lfl_77;
	int i,n,ncv,status;
	UU_KEY_ID skey,bkey,ckey,ukey;
	struct NCL_trimsf_rec surf;
	struct NCL_fixed_databag sf1;
	struct  UC_attributedatabag attr1;
	UM_transf tfmat;
	struct UM_transf_rec btran;
	UU_LIST tkeylst;
	UU_KEY_ID *tkey = UU_NULL;
	UU_LOGICAL lsamesf;
	int isub;
	UM_f77_str_ptr fstr1,fstr2;
	char buf1[80],buf2[80];
	UM_int4 is1,is2;

	UM_init_f77_str (fstr1, buf1, 80);
	UM_init_f77_str (fstr2, buf2, 80);

	lsamesf = (*inamed != 1);

	ckey = ukey = NULLKEY;
	ncv = *ncvs;

	ur_setup_data(NCL_TRIMSF_REL, &surf, sizeof(surf));
	ncl_trmsf_init (&surf);

/* set ranfile label flag to temp unknown */
	lfl_77 = 1;
	stunlb (&lfl_77);
	i = 2*ncv+2;
	uu_list_init (&tkeylst,sizeof(UU_KEY_ID),i,i);
/*
.....Check for base surface or plane. If the key is zero, treat it as
.....a plane so that ncl_plane_to_sf() will convert bounding curve into
.....a plane
*/
	skey = *ksf1;
	surf.bs_key = bkey = *nkey;
	*nkey = NULLKEY;
/*
....Get and copy base surface.
*/
	sf1.key = skey;
	status = ncl_retrieve_data_fixed (&sf1);
	if (status == UU_SUCCESS)
		status = uc_retrieve_transf(skey, tfmat);

	if (status != UU_SUCCESS) goto Err;
/*
..... Set base surface key.
*/
	if (lsamesf)
	{
		uc_retrieve_attr(sf1.key, &attr1);
		ncl_get_label_and_subscr (&sf1,buf1,&isub);
		is1 = isub;
	}

	status = ncl_isectbndry1(bkey,tfmat,bx1,&ckey,&ukey,(ncl_polygon *)0);
	if (status == UU_SUCCESS)
	{
		if (ckey > NULLKEY)
		{
			uu_list_push (&tkeylst,&ckey);
			ur_update_displayable(ckey, UM_NEVERDISPLAYABLE);
		}
		if (ukey > NULLKEY)
		{
			uu_list_push (&tkeylst,&ukey);
			ur_update_displayable(ukey, UM_NEVERDISPLAYABLE);
		}
		surf.cv_key = ckey;
		surf.uv_key = ukey;
	}
	if (status != UU_SUCCESS) goto Err;
/*
.....Reset ranfile label flag.
*/
	if (!lsamesf) stunlb (&lfl_77);
/*
.....Create trimmed surface entity and its transform.
*/
	if (status == UU_SUCCESS)
	{
		status = ncl_create_entity(&surf, 9);
		if (status == UU_SUCCESS)
		{
	/*
	.....Get the base surface transformation
	*/
			btran.key = surf.bs_key;
			btran.rel_num = UM_TRANSFORM_REL;
			if (ur_retrieve_transf(&btran) != 0) goto Err;
			btran.key = surf.key;

			if (ur_update_transf (&btran) != 0) goto Err;
		}
	}
	if (status == UU_SUCCESS)
	{
		if (lsamesf)
		{
			stunlb (&lfl_77);

			ncl_get_label_and_subscr (&surf,buf2,&isub);
			is2 = isub;

			ncl_rename_geom(&surf.key, fstr2, &is2, fstr1, &is1, ierr);
			attr1.key = surf.key;
			ur_update_attr(&attr1);
			uc_delete (skey);
		}
		else
			ncl_store_wf1 (surf.key);

		status = ncl_retrieve_data_fixed (&surf);
		if (status == UU_SUCCESS)
			uc_display (&surf);
		goto Done;
	}

Err:
	if (*ierr <= 0) *ierr = 163;
	stunlb (&lfl_77);
	n = tkeylst.cur_cnt;
	if (n > 0)
	{
		tkey = (UU_KEY_ID *) UU_LIST_ARRAY (&tkeylst);
		for (i = 0; i < n; i++)
		{
			if (tkey[i] > NULLKEY) uc_delete(tkey[i]);
		}
	}

Done:
	uu_list_free (&tkeylst);
	if (*ierr <= 0) *nkey = surf.key;
}

/*********************************************************************
**    E_FUNCTION     : void trmpln (ky1,ky2,ncvs,nclkey,inamed,ierr)
**       Creates a NCL trimmed surface in UNIBASE and return the
**       unibase key of the surface. Fortran interface routine.
**    PARAMETERS
**       INPUT  :
**          ksf       - Key of base surface.
**          ncvs      - number of inner xyz trimming curves.
**          uv        - Starting surface u,v
**       OUTPUT :
**          nclkey    - Key of trimmed surface.
**          ierr      - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void trmpln (ky1,ky2,ncvs,nclkey,inamed,ierr)
UM_int4 *ky1,*ky2,*nclkey;
UM_int2 *ncvs,*inamed,*ierr;
{
	UM_int2 lfl_77,cur_77;
	int i,j,n,ncv;
	int status = UU_SUCCESS;
	UU_KEY_ID skey,lkey,ckey;
	struct NCL_trimsf_rec surf;
	struct NCL_fixed_databag s1,s2,s3;
	struct UM_transf_rec btran;
	UU_LIST tkeylst;
	UU_KEY_ID *ibky = UU_NULL;
	UU_KEY_ID *tkey = UU_NULL;
	UU_LOGICAL lnamesf,lhavecv,lhavepl;

	lhavepl = (*ky1 > 0);
	lhavecv = (*ky2 > 0);
	lnamesf = (*inamed != 1 && NCL_noname == UU_FALSE);

	ckey = lkey = skey = NULLKEY;
	s1.key = s2.key = s3.key = NULLKEY;
	*nclkey = NULLKEY;
	ncv = *ncvs - 1;

	ur_setup_data(NCL_TRIMSF_REL, &surf, sizeof(surf));
	ncl_trmsf_init (&surf);

/* set ranfile label flag to temp unknown */
	lfl_77 = 1;
	stunlb (&lfl_77); cur_77 = lfl_77;
	i = 2*ncv+2;
	uu_list_init (&tkeylst,sizeof(UU_KEY_ID),i,i);
/*
.....Convert plane (or planar bounding curve) into a surface.
*/
	if (lhavepl)
	{
		s1.key = *ky1;
		status = ncl_retrieve_data_fixed(&s1);
	}

	if (lhavecv)
	{
		s3.key = *ky2;
		status = ncl_retrieve_data_fixed(&s3);
	}
	else
	{
		status = ncl_create_rbsplin0(&ckey);
/*
.....Save xyz curve key
*/
		uu_list_push (&tkeylst,&ckey);
		if (status == UU_SUCCESS)
		{
			surf.cv_key = ckey;
			s3.key = ckey;
			status = ncl_retrieve_data_fixed(&s3);
		}
	}

	if (status == UU_SUCCESS)
	{
		status = ncl_plane_to_sf (&s1, &s3, &s2);
		uu_list_push (&tkeylst,&s2.key);
	}
	if (status != UU_SUCCESS) goto Err;
	skey = s2.key;
/*
.....Save base surface key.
*/
	ur_update_displayable(skey, UM_NEVERDISPLAYABLE);
	surf.bs_key = skey;

	if (lhavecv) ckey = *ky2;

	if (ncv > 0)
	{
		ncl_polygon pol1;

		ncl_init_polygon (&pol1,100);
		i = ncv+1;
		pol1.box = (UM_2box *) uu_malloc (i*sizeof(UM_2box));
		pol1.np = (int *) uu_malloc (i*sizeof(int));
		if (pol1.box == UU_NULL || pol1.np == UU_NULL) goto Err;
		pol1.num_contours = 0;

		status = ncl_cvpol (ckey,skey,&pol1);

		for (j = 0; j < ncv; j++)
		{
			status = ncl_get_listkey (1, j, &ckey);
			if (status == UU_SUCCESS)
				status = ncl_cvpol (ckey,skey,&pol1);
			if (status != UU_SUCCESS) break;
		}

		status = ncl_pol_bndry (skey,&pol1,NULLPOL,&lkey,&ibky,&ncv);
		if (lkey > NULLKEY)
		{
			uu_list_push (&tkeylst,&lkey);
			ur_update_displayable(lkey, UM_NEVERDISPLAYABLE);
		}
		surf.uv_key = lkey;
		if (ibky)
		{
			for (j = 0; j < ncv; j++)
			{
				lkey = ibky[2*j+1];
				if (lkey > NULLKEY)
				{
					uu_list_push (&tkeylst,&lkey);
					ur_update_displayable(lkey, UM_NEVERDISPLAYABLE);
				}
			}
		}
		ncl_free_polygon (&pol1);
	}
	else
	{
		status = ncl_cvbndry(skey,&ckey,&lkey);
/*
.....Save uv curve key
*/
		uu_list_push (&tkeylst,&lkey);
		surf.uv_key = lkey;
/*
.....Save xyz curve key
*/
		uu_list_push (&tkeylst,&ckey);
		surf.cv_key = ckey;
	}
/*
.....Reset ranfile label flag.
*/
	if (!NCL_noname) stunlb (&lfl_77);
/*
.....Create trimmed surface entity and its transform.
*/
	if (lnamesf && status == UU_SUCCESS)
	{
		UM_int2 nclstatus;
		status = ncl_label_wf(NCL_TRIMSF_REL,surf.label,&surf.subscr,surf.key,
				&nclstatus);
	}
	if (status == UU_SUCCESS)
	{
		status = ncl_create_entity(&surf, 9);
		if (status == UU_SUCCESS)
		{
/*
.....Get the base surface transformation
*/
			btran.key = surf.bs_key;
			btran.rel_num = UM_TRANSFORM_REL;
			if (ur_retrieve_transf(&btran) != 0) goto Err;
			btran.key = surf.key;

			if (ur_update_transf (&btran) != 0) goto Err;

			if (ibky)
			{
				status = ur_update_data_varlist (surf.key, 1, ibky, 1, 2*ncv);
				UU_FREE (ibky);
			}
		}
	}

	if (status == UU_SUCCESS)
	{
		if (NCL_nokey > NULLKEY)
		{
			uc_delete (NCL_nokey); NCL_nokey = NULLKEY;
		}
		if (!NCL_noname)
			ncl_store_wf1 (surf.key);
		else
			NCL_nokey = surf.key;

		status = ncl_retrieve_data_fixed (&surf);
		if (status == UU_SUCCESS)
			uc_display (&surf);
		goto Done;
	}

Err:
	if (*ierr <= 0) *ierr = 163;
	n = tkeylst.cur_cnt;
	if (n > 0)
	{
		tkey = (UU_KEY_ID *) UU_LIST_ARRAY (&tkeylst);
		for (i = 0; i < n; i++)
		{
			if (tkey[i] > NULLKEY) uc_delete(tkey[i]);
		}
	}

Done:
	lfl_77 = cur_77;
	stunlb (&lfl_77);
	uu_list_free (&tkeylst);
	if (*ierr <= 0) *nclkey = surf.key;
}

/*********************************************************************
**    E_FUNCTION     : void trmrdf (ksf,k2,typ2,ncvs,uv,mmod,dirmod,nclkey,inamed,kext,ierr)
**       Creates a NCL trimmed surface in UNIBASE and return the
**       unibase key of the surface. Fortran interface routine.
**    PARAMETERS
**       INPUT  :
**          ksf       - Key of base surface.
**          ncvs      - number of inner xyz trimming curves.
**          uv        - Starting surface u,v
**       OUTPUT :
**          nclkey    - Key of trimmed surface.
**          ierr      - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void trmrdf (ksf,k2,typ2,ncvs,uv,mmod,dirmod,nclkey,inamed,kext,ierr)
UM_int4 *ksf,*k2,*kext,*nclkey;
UM_int2 *typ2,*ncvs,*mmod,*inamed,*ierr;
UM_real4 uv[2],dirmod[];
{
	UM_int2 lfl_77;
	int nv,i,j,n,ncv,status,rel_num,modtype;
	UU_REAL tol, uva[2], um,vm;
	UM_vector vcm;
	UU_KEY_ID skey,bkey,ckey,ukey;
	struct NCL_trimsf_rec surf;
	struct NCL_trimsf_rec *tsfp;
	struct NCL_fixed_databag sf1,sf2,cv,cv1;
	struct  UC_attributedatabag attr1;
	UM_int2 primtyp = 0,jerr=0;
	nclsf_prim_type typ = NCLSF_UNKNOWN;
	UM_real8 primdata[16];
	UM_transf tfmat;
	ncl_polygon *pol0 = UU_NULL;
	ncl_polygon *pol1 = UU_NULL;
	struct UM_transf_rec btran;
 	UU_LIST uvpts, tkeylst;
	UM_coord *pts;
	UU_KEY_ID *tkey = UU_NULL;
	UU_KEY_ID *ibky = UU_NULL;
	UU_LOGICAL lsamesf,lnoout,ltrims;
	int isub;
	UM_f77_str_ptr fstr1,fstr2;
	char buf1[80],buf2[80];
	UM_int4 is1,is2;
	UM_real8 tol8;
	UU_KEY_ID kss;

	gettol (&tol8);
	tol = tol8;

	UM_init_f77_str (fstr1, buf1, 80);
	UM_init_f77_str (fstr2, buf2, 80);

	lsamesf = (*inamed != 1 && !NCL_noname);
	lnoout = (*k2 == 0);
	ltrims = UU_FALSE;

	ckey = ukey = sf1.key = sf2.key = NULLKEY;
	*nclkey = NULLKEY;
	ncv = *ncvs - 1;
	modtype = *mmod;
	if (modtype == UVMOD)
	{
		um = dirmod[0]; vm = dirmod[1];
	}
	else if (modtype == VCDIR)
	{
		for (i = 0; i < 3; i++) vcm[i] = dirmod[i];
	}

	ur_setup_data(NCL_TRIMSF_REL, &surf, sizeof(surf));
	ncl_trmsf_init (&surf);

/* set ranfile label flag to temp unknown */
	lfl_77 = 1;
	stunlb (&lfl_77);
/*
..... create a list to hold keys of all created entities - to delete in case of error
*/
	i = 2*ncv+2; if (i < 5) i = 5;
	uu_list_init (&tkeylst,sizeof(UU_KEY_ID),i,i);
/*
.....Check for base surface or plane. If the key is zero, treat it as
.....a plane so that ncl_plane_to_sf() will convert bounding curve into
.....a plane
*/
	skey = *ksf;
	if (skey > 0)
	{
		status = ur_retrieve_data_relnum (skey, &rel_num);
		if (status == UU_SUCCESS)
		{
			ltrims = (rel_num == NCL_TRIMSF_REL);
/*
....Get and copy base surface.
*/
			sf1.key = skey;
			status = ncl_retrieve_data_fixed (&sf1);
			if (status == UU_SUCCESS)
				status = uc_retrieve_transf(skey, tfmat);
			if (status != UU_SUCCESS) goto Err;

			if (lsamesf)
			{
				uc_retrieve_attr(sf1.key, &attr1);
/*
..... Redefine a surface: get original surface label and attributes
*/
				ncl_get_label_and_subscr (&sf1,buf1,&isub);
				is1 = isub;
			}

			if (ltrims)
			{
				tsfp = (struct NCL_trimsf_rec *)&sf1;
				pol0 = (ncl_polygon *) uu_malloc (sizeof(ncl_polygon));
				if (pol0 == UU_NULL) goto Err;
				status = ncl_bndry_pol (tsfp,tfmat,pol0);
				if (status == UU_SUCCESS)
				{
					sf1.key = tsfp->bs_key;
					status = ncl_retrieve_data_fixed (&sf1);
				}
				if (status != UU_SUCCESS) goto Err;
			}
			if (ltrims || ncv > 0)
			{
				pol1 = (ncl_polygon *) uu_malloc (sizeof(ncl_polygon));
				if (pol1== UU_NULL) goto Err;
				ncl_init_polygon (pol1,100);
				i = ncv+1;
				pol1->box = (UM_2box *) uu_malloc (i*sizeof(UM_2box));
				pol1->np = (int *) uu_malloc (i*sizeof(int));
				if (pol1->box == UU_NULL || pol1->np == UU_NULL) goto Err;
			}
/*
..... Set base surface key.
*/
			if (lsamesf && !ltrims)
			{
				if (*kext > 0)
					bkey = *kext;
				else
					bkey = sf1.key;

				surf.bs_key = bkey;
			}
			else
			{
				if (*kext > 0)
					bkey = surf.bs_key = *kext;
				else
				{

/*
.....Create a new surface: copy the base.
*/
					status = uc_copy (&sf1, &sf2, sizeof(struct NCL_fixed_databag));
					if (sf2.key > NULLKEY) uu_list_push (&tkeylst,&sf2.key);
					if (status == UU_SUCCESS)
					{
						status = ncl_get_sf_primdat(&skey,&primtyp,primdata);
						if (status != UU_SUCCESS || primtyp < 0 || primtyp > 6)
							status = UU_SUCCESS;
						else
							typ = (nclsf_prim_type) primtyp;

						ncl_store_prim_data(&sf2,typ,primdata);
						ur_update_displayable(sf2.key, UM_NEVERDISPLAYABLE);
						bkey = surf.bs_key = sf2.key;
					}
				}
			}
		}

		if (status == UU_SUCCESS)
		{
			if (modtype == VCDIR && *typ2 != 8)
				ncl_vc_to_uv (&sf1,tfmat,vcm,&modtype,&um,&vm);

			if (lnoout)
			{
				if (pol1 != UU_NULL)
					status = ncl_boxpol(pol1);
				else
					status = ncl_boxbndry(bkey,tfmat,&ckey,&ukey);
			}
/*
..... replaced the polygon logic for composite curves with general logic for
..... all kind of curves.

			else if (*typ2 == UM_COMPCRV_REL)
			{
				ckey = *k2;
				if (pol1 != UU_NULL)
				{
					pol1->num_contours = 0;
					cv.key = ckey;
					cv1.key = 0 ;
					status = ncl_retrieve_data_fixed (&cv);
					status = uc_copy (&cv, &cv1, sizeof(struct NCL_fixed_databag));
					ur_update_displayable(cv1.key, UM_NEVERDISPLAYABLE);
					ckey = cv1.key;
					status = ncl_cvpol (ckey,bkey,pol1);

				}
				else
					status = ncl_cvbndry(bkey,&ckey,&ukey);
			}*/
/*
.....for all curve types, the curve is projected on the surface and the
.....evaluated points are stored in NCL_uvintof.
*/
			else if (*typ2 == 8)
			{
				uu_list_init (&NCL_uvintof,sizeof(UU_REAL),1200,1200);
				cv.key = *k2;
				cv1.key = 0 ;
				status = ncl_retrieve_data_fixed (&cv);
				status = uc_copy (&cv, &cv1, sizeof(struct NCL_fixed_databag));
				ur_update_displayable(cv1.key, UM_NEVERDISPLAYABLE);
				ckey = cv1.key;
				uva[0] = uva[1] = 0.5;
				uu_list_init (&uvpts, sizeof(UM_coord), 100, 100);
				status = ncl_cv_project_sfuv(ckey,skey,uva,tol,&uvpts,&nv);
				if (status == UU_SUCCESS)
				{
					pts = (UM_coord *) UU_LIST_ARRAY (&uvpts);
					for(i=0; i<nv; i++)
						uu_list_insert_multiple (&NCL_uvintof,0,3,pts[i]);
				}
				uu_list_free (&uvpts);

				if (modtype == VCDIR)
				ncl_vc_to_uv (&sf1,tfmat,vcm,&modtype,&um,&vm);

				if (modtype != RANDOM || ncv > 0)
					status = ncl_isectbndry(bkey,tfmat,modtype,um,vm,&ckey,&ukey,pol1);
				else			
					ncl_cvbndry(bkey,&ckey,&ukey);
			}
/*
.....the trim surface boundary is intersected with the points in NCL_uvintof
.....for curves NCL_uvintof has points generated from the projection
.....for surfaces NCL_uvintof has points generated from the sfio routine
*/
			else
				status = ncl_isectbndry(bkey,tfmat,modtype,um,vm,&ckey,&ukey,pol1);

			if (status == UU_SUCCESS && 
				((*typ2 == 8 && ckey > NULLKEY && ukey > NULLKEY) ||
				 (*typ2 != 8 && (ckey > NULLKEY || ukey > NULLKEY))))
			{
				if (ckey > NULLKEY)
				{
					uu_list_push (&tkeylst,&ckey);
					ur_update_displayable(ckey, UM_NEVERDISPLAYABLE);
				}
				if (ukey > NULLKEY)
				{
					uu_list_push (&tkeylst,&ukey);
					ur_update_displayable(ukey, UM_NEVERDISPLAYABLE);
				}
				surf.cv_key = ckey;
				surf.uv_key = ukey;
			}
		}
		if (status != UU_SUCCESS) goto Err;
	}

	if (pol1 != UU_NULL)
	{
		for (j = 0; j < ncv; j++)
		{
			status = ncl_get_listkey (1, j, &ckey);
			if (status == UU_SUCCESS)
				status = ncl_cvpol (ckey,bkey,pol1);
			if (status != UU_SUCCESS) break;
		}
		status = ncl_pol_bndry (bkey,pol1,pol0,&ukey,&ibky,&ncv);
		if (ukey > NULLKEY)
		{
			uu_list_push (&tkeylst,&ukey);
			ur_update_displayable(ukey, UM_NEVERDISPLAYABLE);
		}
		surf.uv_key = ukey;
		if (ibky)
		{
			for (j = 0; j < ncv; j++)
			{
				ukey = ibky[2*j+1];
				if (ukey > NULLKEY)
				{
					uu_list_push (&tkeylst,&ukey);
					ur_update_displayable(ukey, UM_NEVERDISPLAYABLE);
				}
			}
		}
	}

	if (status == UU_SUCCESS)
	{
/*
.....Reset ranfile label flag.
*/
		if ( !lsamesf) stunlb (&lfl_77);
/*
.....Create trimmed surface entity and its transform.
*/
		status = ncl_create_entity(&surf, 9);
/*
..... ranfile label flag is Reset after creation of all entities .
		if (NCL_noname || lsamesf) stunlb (&lfl_77);
*/
	}
	if (status == UU_SUCCESS && ibky)
	{
		status = ur_update_data_varlist (surf.key, 1, ibky, 1, 2*ncv);
		UU_FREE (ibky);
	}
/*
.....Use the transformation from the original trimmed surface
*/
	if (status == UU_SUCCESS)
	{
		if (ltrims)
		{
			btran.key = skey;
			btran.rel_num = UM_TRANSFORM_REL;
			status = ur_retrieve_transf(&btran);
			if (status == UU_SUCCESS)
			{
				btran.key = surf.key;
				status = ur_update_transf (&btran);
			}
		}
/*
.....Get the base surface transformation
*/
		else
		{
			btran.key = surf.bs_key;
			btran.rel_num = UM_TRANSFORM_REL;
			status = ur_retrieve_transf(&btran);
			if (status == UU_SUCCESS)
			{
				btran.key = surf.key;
				status = ur_update_transf (&btran);
			}
		}
	}
	if (status == UU_SUCCESS)
	{
		if (lsamesf)
		{
			UM_int2 indx,ltmp,lcanon,lerr;

			indx = 41;
			getifl (&indx,&lcanon);
			ltmp = 1;
			setifl(&indx,&ltmp);
/*
..... Rename the original surface @UN, or delete it if trimmed. Transfer the
..... original surface name to the new surface, plus all the attributes.
*/
			if (ltrims)
				uc_delete (skey);
			else
			{
				strncpy (buf2,"@UN    ",7);
				is2 = 0;
				ncl_rename_geom(&sf1.key, fstr1, &is1, fstr2, &is2, ierr);
				ur_update_displayable(sf1.key, UM_NEVERDISPLAYABLE);
				ncl_sea_ent_blank (0,sf1.key);
			}

			ncl_get_label_and_subscr (&surf,buf2,&isub);
			is2 = isub;

			ncl_rename_geom(&surf.key, fstr2, &is2, fstr1, &is1, &lerr);
			attr1.key = surf.key;
			ur_update_attr(&attr1);
			setifl(&indx,&ltmp);
		}

		if (NCL_nokey > NULLKEY)
		{
			uc_delete (NCL_nokey); NCL_nokey = NULLKEY;
		}
		if (!NCL_noname)
			ncl_store_wf1 (surf.key);
		else
		{
			if (NCL_skey0 > NULLKEY) ncl_sea_ent_blank (0,NCL_skey0);

			NCL_nokey = surf.key;
			ur_update_selectable (NCL_nokey,UU_FALSE);
		}
/*
.....set the color of the temp surface to Sea Green
*/
		if (nclu_temp_redef)
			ncl_update_color(surf.key,NCLX_SEA_GREEN);

		status = ncl_retrieve_data_fixed (&surf);
		if (status == UU_SUCCESS)
			uc_display (&surf);
		goto Done;
	}

Err:
	if (*ierr <= 0) *ierr = 163;
	if (!lsamesf)stunlb (&lfl_77);
	n = tkeylst.cur_cnt;
	if (n > 0)
	{
		tkey = (UU_KEY_ID *) UU_LIST_ARRAY (&tkeylst);
		for (i = 0; i < n; i++)
		{
			if (tkey[i] > NULLKEY) uc_delete(tkey[i]);
		}
	}

Done:
/*
.....Reset ranfile label flag .
*/
	if (lsamesf)stunlb (&lfl_77);
	ncl_free_keylist(1);
	uu_list_free (&tkeylst);
	if (*ierr <= 0) *nclkey = surf.key;
	if (pol0)
	{
		ncl_free_polygon (pol0);
		UU_FREE (pol0);
	}
	if (pol1)
	{
		ncl_free_polygon (pol1);
		UU_FREE (pol1);
	}
}
