/*********************************************************************
**    NAME         :  m4bndry2.c
**       CONTAINS: Routines to process boundary curves (trimmed SF)
**                 and curves on surface.  The routines in this file are
**                 used in the OpenNCL Geometry Library.
**
**       um_pre_srf_bndr_box
**       um_evolve_bndr
**       um_evolve_boxbndr
**       um_pre_srf_bndr
**       um_init_boundary
**       um_free_boundary
**       um_move_boundary
**       um_print_bndr
**       um_uvlist_init
**       um_uvlist_free
**       um_cshape_dirchk
**       um_cshape_dirchk1
**       um_cshape_fix
**       um_cshape_fix1
**
**    COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       m4bndry2.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       06/01/15 , 07:43:51
*********************************************************************/
#include "nccs.h"
#include "udebug.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mcrv.h"
#include "nclfc.h"
#include "mattr.h"
#include "mdattr.h"
#include "modef.h"
#include "mgeom.h"
#include "umath.h"
#include "ulist.h"
#include "uminmax.h"
#include "nclpsmult.h"

UU_REAL um_get_rltv_tol();

char *uu_malloc();

/*#define DEBUGON 1*/
/*********************************************************************
**    E_FUNCTION     : um_cshape_dirchk1 (pts,npt,xextr,yextr)
**       Checks if direction of a closed curve (represented by polyline)
**       is same as required.  If not, points are reversed.
**    PARAMETERS
**       INPUT  :
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**       OUTPUT :
**          xextr  - min X & max X of the polyline hull.
**          yextr  - min Y & max Y of the polyline hull.
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
** NOTE: In fact, starting with 12.3, this routine just calculates the
**       bounding boxes. Maybe the orientation should be done too.
*********************************************************************/
int um_cshape_dirchk1 (pts, npt, xextr, yextr)
UM_coord pts[];
UU_REAL xextr[2], yextr[2];
int npt;
{
	UU_REAL xmin, xmax, ymin, ymax;
	int i;

	xmin = xmax = pts[0][0];
	ymin = ymax = pts[0][1];
/*
...get x & y span and most left point
*/
	for (i=1; i<npt; i++)
	{
		if (xmax < pts[i][0])
			xmax = pts[i][0];
		else if (xmin > pts[i][0])
			xmin = pts[i][0];

		if (ymax < pts[i][1])
			ymax = pts[i][1];
		else if (ymin > pts[i][1])
			ymin = pts[i][1];
/*
...make sure that z coordinate is 0
*/
	}

	xextr[0] = xmin;
	xextr[1] = xmax;
	yextr[0] = ymin;
	yextr[1] = ymax;

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : um_cshape_dirchk (pts,nb,nbpt,xextr,yextr)
**       Checks if direction of closed curves (represented by polylines)
**       is same as required.  If not points are reversed.
**    PARAMETERS
**       INPUT  :
**          pts    - array of points representing closed boundary curves,
**                   last point is same as the first point in array.
**          nb     - number of closed boundary curves in pts
**          nbpt   - start pointers of boundary curve in pts
**       OUTPUT :
**          xextr  - min X & max X of the polyline hull.
**          yextr  - min Y & max Y of the polyline hull.
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
** NOTE: In fact, starting with 12.3, this routine just calculates the
**       bounding boxes. Maybe the orientation should be done too.
*********************************************************************/
int um_cshape_dirchk (pts, nb, nbpt, xextr, yextr)
UM_coord pts[];
UU_REAL (*xextr)[2], (*yextr)[2];
int nb, *nbpt;
{
	int i, ix, n;

	for (i=0, ix=0; i<nb; i++)
	{
		n = nbpt[i];
		um_cshape_dirchk1 (&pts[ix],n,xextr[i],yextr[i]);
		ix += n;
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : um_cshape_fix1 (cvpts,uvptr,cvdrv,ist,npt)
**       Checks if closed curve in xyz space is closed in uv space.
**       If not then close uv polygon by adding point at the end;
**       also add the last xyz point mapped by this new uv point and
**       new xyz point created just at UM_FUZZ from existing point.
**    PARAMETERS
**       INPUT  :
**          cvpts  - List of xyz points representing closed curve, last
**                   point is same as the first point in array.
**          uvptr  - List of uv points representing cvpts, may not be
**                   closed
**          cvdrv  - List of slope vectors in xyz space for cvpts list
**                   of points
**                   point is same as the first point in array.
**          ist    - index of start point in the lists
**          npt    - number of points in the list defining closed curve
**       OUTPUT :
**          cvpts  - Fixed list of xyz points if necessary.
**          uvptr  - Fixed list of uv points if necessary.
**          cvdrv  - Fixed list of slope vectors if necessary.
**    RETURNS      :
**       0 = fix not required, 1 = added point in the list to close
**           the curve.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cshape_fix1 (cvpts,uvptr,cvdrv,ist,npt)
UU_LIST *cvpts,*uvptr,*cvdrv;
int npt,ist;
{
	UM_coord *uvdat,*cvdat,*drdat,ptp;
	int status,ins,ien;

	status = 0;

	if (cvpts == UU_NULL) return(0);

	uvdat = (UM_coord *) UU_LIST_ARRAY (uvptr);
/*
.....check if gap between last and first uv point
*/
	ien = ist + npt - 1;
	ins = ien + 1;

	if (UM_SQDIS_2D(uvdat[ist],uvdat[ien]) > UM_DFUZZ)
	{
		cvdat = (UM_coord *) UU_LIST_ARRAY (cvpts);
		if (cvdrv) drdat = (UM_coord *) UU_LIST_ARRAY (cvdrv);
/*
.....Check to make sure that the curve is
.....closed in xyz.  JLS 11/17/99
*/
		if (UM_SQDIS(cvdat[ist],cvdat[ien]) > UM_DFUZZ)
		{
			um_vctovc (cvdat[ist],ptp);
			uu_list_insert (cvpts,ins,ptp);

			ptp[0] = uvdat[ist][0]; ptp[1] = uvdat[ist][1];
			ptp[2] = uvdat[ien][2];

			uu_list_insert (uvptr,ins,ptp);
			if (cvdrv)
			{
				um_vctovc (drdat[ien],ptp);
				uu_list_insert (cvdrv,ins,ptp);
			}
			status = 1;
		}
/*
.....If the curve was closed in xyz, then add a point really
.....close to the endpoint.  The reason: the number of xyz points
.....should be the same as the number of uv points.

..... changed that to: do not add points, just fix the last points
..... so that both UV- and XYZ-curves are closed - QAR 92093
*/
		else
		{
			uvdat[ien][0] = uvdat[ist][0]; uvdat[ien][1] = uvdat[ist][1];
			um_vctovc (cvdat[ist],cvdat[ien]);
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : um_cshape_fix (cvpts,uvptr,cvdrv,nb,nbpt)
**       Checks and fix if necessary all boundary curves which are
**       supposed to be closed in uv space.
**    PARAMETERS
**       INPUT  :
**          cvpts  - List of xyz points representing closed curve, last
**                   point is same as the first point in array.
**          uvptr  - List of uv points representing cvpts, may not be
**                   closed
**          cvdrv  - List of slope vectors in xyz space for cvpts list
**                   of points
**                   point is same as the first point in array.
**          cvids  - Structure containing number of points in each
**                   curve element.
**          nb     - number of closed boundary curves in pts
**          nbpt   - start pointers of boundary curve in pts
**       OUTPUT :
**          cvpts  - Fixed list of xyz points if necessary.
**          uvptr  - Fixed list of uv points if necessary.
**          cvdrv  - Fixed list of slope vectors if necessary.
**          cvids  - Fixed composite curve index list if necessary.
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cshape_fix (cvpts,uvptr,cvdrv,cvids,nb,nbpt)
UU_LIST *cvpts, *uvptr, *cvdrv;
struct NCL_uvconv *cvids;
int nb, *nbpt;
{
	int i, j1, j2, ix, k, n;

	for (i=0, ix=0; i<nb; i++)
	{
		n = nbpt[i];
		k = um_cshape_fix1 (cvpts,uvptr,cvdrv,ix,n);
		if (k == 1 && cvids)
		{
			for (j1 = i+1; j1 < nb; j1++)
				for (j2 = 0; j2 < cvids->bncrv[j1].no_csg; j2++)
					cvids->bncrv[j1].segix[j2]++;
		}
		n = nbpt[i] = n + k;
		ix += n;
	}
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION: um_pre_srf_bndr_box (eptr,bound)
**       Pre-processing surfaces: get UV-boxes for trim boundary curves
**    PARAMETERS
**       INPUT  :
**          eptr    - pointer to surface.
**          tfmat   - pointer to translation mx.
**          tol     - tolerance in XYZ space.
**       OUTPUT :
**          nbnc    - number of boundary curves.
**          nbpt    - array of pointers to boundary curves in uvptr &
**                    cvpts lists.
**          ummn    - array of U min and U max for every boundary.
**          vmmn    - array of V min and V max for every boundary.
**
**    RETURNS      :   UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_pre_srf_bndr_box (eptr,bound)
struct NCL_fixed_databag *eptr;
UM_srf_boundary *bound;
{
	int status, i, j, n, nb, n2, itis;
	UU_REAL bplm[4], dup[2], tolr;
	UM_coord *uvdat;
	UM_2Dcoord *ummx, *vmmx;
	UU_LIST uvptr;

	nb = 1;
	itis = 0;
	status = UU_FAILURE;
	dup[0] = dup[1] = 1.0;
	uvptr.data = UU_NULL;
/*
...get surface data
*/
	if (ncl_retrieve_data_fixed (eptr) != 0) goto Done;
	if (ncl_itsa_trimsrf (eptr))
	{
		itis = 1;
		ncl_trimsrf_get_fixed (eptr,&nb,bplm);

		dup[0] = bplm[1] - bplm[0];
		dup[1] = bplm[3] - bplm[2];

		uu_list_init (&uvptr, sizeof(UM_coord), 100, 100);
	}
	bound->nb = nb;
	tolr = bound->toler;
/*
...allocate memory space
*/
	status = UU_SUCCESS;
	ummx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
	vmmx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
/*
...get base surface when processing trimmed surface
*/
	if (itis == 1)
	{
		struct NCL_fixed_databag cv, c2;

		status  = ncl_tsf_get_boundary (eptr,0,&cv);
/*
...evolve boundary UV curves and store all points in cvpts
...& uvptr (UV points) lists.  Start with outer boundary curve
*/
		if (status == UU_SUCCESS)
		{
			for (i=0; i<nb && status == UU_SUCCESS; i++)
			{
				if (ncl_itsa_compcrv(&cv))
				{
					int ncv, rev;
					status = ncl_compcrv_getnents (&cv, &ncv);
					n2 = 0;
					for (j=0; j<ncv && status == UU_SUCCESS; j++)
					{
						status = ncl_compcrv_getelm (&cv, j, &c2, &rev);
/*
...evolve curve segment with (xyz) chordal tolerance
...drop last point of segment if more segments follows
*/
						if (status == UU_SUCCESS)
						{
							n = ncl_evolve_curve (&c2,UM_DEFAULT_TF,tolr,&uvptr,
								UU_NULL,UU_NULL,0);

							if (n <= 0) {status = UU_FAILURE; goto Done;}
							n2 += n;
						}
					}
				}
				else
				{
					n2 = ncl_evolve_curve (&cv,UM_DEFAULT_TF,tolr,&uvptr,UU_NULL,
						UU_NULL,0);
					if (n2 <= 0) {status = UU_FAILURE; goto Done;}
				}
				uvdat = (UM_coord *) UU_LIST_ARRAY (&uvptr);
				um_cshape_dirchk1 (uvdat,n2,ummx[i],vmmx[i]);
/*
...get inner boundary UV curves if any
*/
				if (i<nb-1)
				{
					uvptr.cur_cnt = 0;
					status  = ncl_tsf_get_boundary (eptr,i+1,&cv);
				}
			}
		}
	}
/*
...any other type of surface has only 4 U/V lines boundary
...create boundary points by evolving surface on boundaries
*/
	else
	{
		ummx[0][0] = vmmx[0][0] = 0.;
		ummx[0][1] = vmmx[0][1] = 1.;
		status = UU_SUCCESS;
	}
/*
...define SF outer boundary box
*/
	if (status == UU_SUCCESS)
	{
		if (itis)
		{
/*
...convert box to u,v space of surface
*/
			if (dup[0] <= 0.0 || dup[1] <= 0.0)
			{
				status = UU_FAILURE;
				goto Done;
			}
			else
			{
				for (i=0; i<nb; i++)
				{
					ummx[i][0] = (ummx[i][0] - bplm[0]) / dup[0];
					ummx[i][1] = (ummx[i][1] - bplm[0]) / dup[0];
					vmmx[i][0] = (vmmx[i][0] - bplm[2]) / dup[1];
					vmmx[i][1] = (vmmx[i][1] - bplm[2]) / dup[1];
				}
			}
		}
		bound->ummx = ummx;
		bound->vmmx = vmmx;
	}

Done:
	uu_list_free (&uvptr);
	return (status);
}

/*********************************************************************
**    I_FUNCTION: S_point_on_apex (pp,treflg,eps)
**       Check if a point is at the triangular part of a surface
*********************************************************************/
static UU_LOGICAL S_point_on_apex (pp,treflg,eps)
int treflg;
UM_coord pp;
UU_REAL eps;
{
	UU_REAL w0;

	if (treflg == 1 || treflg == 2)
	{
		w0 = treflg - 1.;
		if (fabs (pp[0] - w0) < eps) return (UU_TRUE);
	}
	else if (treflg == 3 || treflg == 4)
	{
		w0 = treflg - 3.;
		if (fabs (pp[1] - w0) < eps) return (UU_TRUE);
	}

	return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION: S_fix_for_treflg (bsptr,tol,tlst,uvpts,cvpts,npt)
**       If the boundary endpoints are different in UV, and same in XYZ
**       because of the triangular surface, add a point to all lists to
**       make the UV-boundary closed
**    PARAMETERS
**       INPUT  :
**          tol     - tolerance in XYZ space
**          bsptr   - pointer to base surface
**          npt     - number of points in the outer boundary
**          uvpts   - UV list
**          cvpts   - XYZ list
**          vlst    - tangent vectors list
**       OUTPUT :
**          npt     - number of points in the outer boundary
**          uvpts   - UV list
**          cvpts   - XYZ list
**          vlst    - tangent vectors list
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fix_for_treflg (bsptr,tol,uvpts,cvpts,vlst,npt)
UU_REAL tol;
struct NCL_fixed_databag *bsptr;
int *npt;
UU_LIST *uvpts,*cvpts,*vlst;
{
	int n2,treflg,j;
	UM_coord *pts,pp;
	UM_vector vv;
	UU_REAL d,eps,epsq;

	eps = 0.001;
	epsq = eps*eps;

	pts = (UM_coord *) UU_LIST_ARRAY (uvpts);
	n2 = *npt;
	for (j = 0; j < n2; j++)
	{
		if (pts[j][0] < 0.) pts[j][0] = 0.;
		if (pts[j][0] > 1.) pts[j][0] = 1.;
		if (pts[j][1] < 0.) pts[j][1] = 0.;
		if (pts[j][1] > 1.) pts[j][1] = 1.;
	}
/*
..... if the outer boundary is not closed, find if we
..... have a triangular surface
*/
	d = UM_SQDIS_2D (pts[0],pts[n2-1]);

	if (ncl_setver(96))
	{
		if (d > 0.8)
		{
			ncl_get_sf_treflag (bsptr,&treflg,tol);
			if (treflg == 1 || treflg == 2)
			{
/*
..... if the boundary endpoints are at the triangular part of the surface,
..... add a point to make the boundary closed
*/
				if (S_point_on_apex(pts[0],treflg,eps) &&
					S_point_on_apex(pts[n2-1],treflg,eps))
				{
					um_vctovc (pts[0],pp);
					uu_list_push (uvpts, pp);
					pts = (UM_coord *) UU_LIST_ARRAY (cvpts);
					um_vctovc (pts[0],pp);
					uu_list_push (cvpts, pp);
					if (vlst != NULLST)
					{
						um_nullvc (vv);
						uu_list_push (vlst, vv);
					}
					*npt = n2 + 1;
				}
			}
		}
	}
	else
	{
		if (d > epsq)
		{
			ncl_get_sf_treflag (bsptr,&treflg,tol);
			if (treflg >= 1 && treflg <= 4)
			{
/*
..... if the boundary endpoints are at the triangular part of the surface,
..... add a point to make the boundary closed
*/
				if (S_point_on_apex(pts[0],treflg,eps) &&
					S_point_on_apex(pts[n2-1],treflg,eps))
				{
					pp[0] = pts[0][0]; pp[1] = pts[0][1];
					pp[2] = pts[n2-1][2];
					uu_list_push (uvpts, pp);
					pts = (UM_coord *) UU_LIST_ARRAY (cvpts);
					um_vctovc (pts[0],pp);
					uu_list_push (cvpts, pp);
					if (vlst != NULLST)
					{
						um_nullvc (vv);
						uu_list_push (vlst, vv);
					}
					*npt = n2 + 1;
				}
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION: um_evolve_boxbndr (eptr,tfmat,ur,vr,tol,ptlst,vlst,uvlst,
**                                   cvids)
**       Evolve and process surface boundary curves for a surface with a box
**       boundary.
**    PARAMETERS
**       INPUT  :
**          tol     - tolerance in XYZ space
**          eptr    - pointer to surface
**          tfmat   - pointer to transformation matrix
**          ur,vr   - range in U and V defining the box
**          ptlst   - list to store XYZ points
**          vlst    - list to store tangent vectors
**          uvlst   - list to store UV points
**          cvids   - curves id to evolve
**       OUTPUT :
**          ptlst   - list of XYZ points
**          vlst    - list of tangent vectors
**          uvlst   - list of UV points
**          cvids   - curves id
**
**    RETURNS      :   number of evolved points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_evolve_boxbndr (eptr,tfmat,ur,vr,tol,ptlst,vlst,uvlst,cvids)
UU_REAL tol,ur[],vr[];
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
struct NCL_uvconv *cvids;
UU_LIST *ptlst,*vlst,*uvlst;
{
	int n1,n2;
	UM_vector *vt;
	UU_REAL w;

	if (cvids)
	{
		cvids->bncrv[0].no_csg = 4;
		cvids->bncrv[0].segix = (int *) uu_malloc(4*sizeof(int));
	}

	UU_LIST_EMPTY (ptlst);
	UU_LIST_EMPTY (uvlst);
	if (vlst != UU_NULL) UU_LIST_EMPTY (vlst);

/*
..... first edge: v = vmin
*/
	w = vr[0];
	n1 = ncl_evolve_crv_on_srf
			(eptr,tfmat,w,ur,1,tol,ptlst,vlst,uvlst) - 1;
	if (cvids) cvids->bncrv[0].segix[0] = n1;
	n2 = n1;
	
	ptlst->cur_cnt = uvlst->cur_cnt = n2;
	if (vlst != UU_NULL) vlst->cur_cnt = n2;

/*
..... second edge: u = umax
*/
	w = ur[1];
	n1 = ncl_evolve_crv_on_srf
			(eptr,tfmat,w,vr,2,tol,ptlst,vlst,uvlst) - 1;
	if (cvids) cvids->bncrv[0].segix[1] = n1;
	n2 += n1;
	ptlst->cur_cnt = uvlst->cur_cnt = n2;
	if (vlst != UU_NULL) vlst->cur_cnt = n2;

/*
..... third edge: v = vmax - reversed
*/
	w = vr[1];
	n1 = ncl_evolve_crv_on_srf
		(eptr,tfmat,w,ur,1,tol,ptlst,vlst,uvlst);

	ncl_revers_list (n1,n2,ptlst,NULLST,uvlst);
	if (vlst != UU_NULL)
	{
		vt = (UM_vector *) UU_LIST_ARRAY (vlst);
		ncl_revers1_list (n1,n2,vt,2);
	}

	n1 -= 1;

	if (cvids) cvids->bncrv[0].segix[2] = n1;
	n2  += n1; 
	ptlst->cur_cnt = uvlst->cur_cnt = n2;
	if (vlst != UU_NULL) vlst->cur_cnt = n2;

/*
..... fourth edge: u = umin - reversed
*/
	w = ur[0];
	n1 = ncl_evolve_crv_on_srf
		(eptr,tfmat,w,vr,2,tol,ptlst,vlst,uvlst);

	ncl_revers_list (n1,n2,ptlst,NULLST,uvlst);
	if (vlst != UU_NULL)
	{
		vt = (UM_vector *) UU_LIST_ARRAY (vlst);
		ncl_revers1_list (n1,n2,vt,2);
	}

	if (cvids) cvids->bncrv[0].segix[3] = n1;
	n2  += n1; 
	ptlst->cur_cnt = uvlst->cur_cnt = n2;
	if (vlst != UU_NULL) vlst->cur_cnt = n2;

	return (n2);
}

/*********************************************************************
**    E_FUNCTION: um_evolve_bndr (tol,eptr,bsptr,tfmat,itis,nb,np,ummx,vmmx,
**					bplm,bound,vlst,cvids)
**       Evolve and process surface boundary curves.
**    PARAMETERS
**       INPUT  :
**          tol     - tolerance in XYZ space
**          eptr    - pointer to surface
**          bsptr   - pointer to base surface
**          tfmat   - pointer to transformation matrix
**          itis    - trimmed surface flag
**          nb      - number of boundary curves
**          bplm    - trimmed surface parameter box
**          bound   - boundary structure
**          vlst    - list to store tangent vectors
**          cvids   - curves id to evolve
**       OUTPUT :
**          bound   - updated boundary.
**          vlst    - list with tangent vectors
**          cvids   - list of pointers to starting points of each
**
**    RETURNS      :   UU_SUCCESS / UU_FAILURE, or 466 if evaluator failed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_evolve_bndr (tol,eptr,bsptr,tfmat,itis,nb,np,ummx,vmmx,bplm,
					bound,vlst,cvids)
UU_REAL tol,*bplm;
struct NCL_fixed_databag *eptr,*bsptr;
UM_transf tfmat;
UM_srf_boundary *bound;
int itis,nb,*np;
UM_2Dcoord *ummx,*vmmx;
struct NCL_uvconv *cvids;
UU_LIST *vlst;
{
	int status;
	int i,j,k,n,n1,n2,ncv,rev,ncc;
	UM_coord *uvdat;
	UM_vector *vt;
	struct NCL_fixed_databag cv,c2;
	UU_REAL ur[2],vr[2];
	UU_REAL ct0,ct1;

	status = UU_SUCCESS;
/*
...get boundary curves when processing trimmed surface
*/
	if (itis == 1)
	{
		status = ncl_tsf_get_boundary (eptr,0,&cv);
/*
...evolve boundary UV curves and store all points in cvpts
...& uvptr (UV points) lists.  Start with outer boundary curve
*/
		if (status == UU_SUCCESS)
		{
			n2 = 0;
			if (cvids)
			{
				for (i=0; i<nb; i++) cvids->bncrv[i].no_csg = 0;
			}
			for (i=0, n1=0; i<nb && status == UU_SUCCESS; i++)
			{
				if ((!cvids || cv.key == cvids->crvid[i]) && ncl_itsa_compcrv(&cv))
				{
					status = ncl_compcrv_getnents (&cv, &ncv);
					if (cvids)
					{
						cvids->bncrv[i].no_csg = ncv;
						cvids->bncrv[i].segix = (int *) uu_malloc (ncv*sizeof(int));
					}
					ct0 = 0;
					for (j=0; j<ncv && status == UU_SUCCESS; j++)
					{
						if (cvids) cvids->bncrv[i].segix[j] = n2;
						status = ncl_compcrv_getelm (&cv, j, &c2, &rev);
						if (status == UU_SUCCESS)
							status = ncl_compcrv_getendparam (&cv, j, &ct1);

						if (status == UU_SUCCESS)
						{
							n = ncl_evolve_bn_crv_on_srf (bsptr,tfmat,&c2,bplm,tol,
									bound->cvpts,vlst,bound->uvpts);
							if (n == -466) return (466);
							if (n<=0 || bound->uvpts==UU_NULL || bound->cvpts==UU_NULL)
								return (UU_FAILURE);

							uvdat = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
							for (k = n2; k < n2 + n; k++)
							{
								if (rev)
									uvdat[k][2] = ct1 - ((ct1 - ct0) * uvdat[k][2]);
								else
									uvdat[k][2] = ct0 + ((ct1 - ct0) * uvdat[k][2]);
							}
							ct0 = ct1;

							if (rev)
							{
								ncl_revers_list (n,n2,bound->cvpts,NULLST,bound->uvpts);
								if (vlst != UU_NULL)
								{
									vt = (UM_vector *) UU_LIST_ARRAY (vlst);
									ncl_revers1_list (n,n2,vt,2);
								}
							}

							n2 += n;
							if (j < ncv-1)
							{
								bound->uvpts->cur_cnt--;
								bound->cvpts->cur_cnt--;
								if (vlst != UU_NULL) vlst->cur_cnt--;
								n2--;
							}
						} /* if (status == UU_SUCCESS) */
					} /* for (j=0; ... */

					if (i == 0 && n2 > 2)
						S_fix_for_treflg (bsptr,tol,bound->uvpts,bound->cvpts,vlst,&n2);

				} /* if (ncl_itsa_compcrv(&cv)) */

				else
				{
					if (cvids)
					{
						cvids->bncrv[i].no_csg = 1;
						cvids->bncrv[i].segix = (int *) uu_malloc (sizeof(int));
					}
					n = ncl_evolve_bn_crv_on_srf
						(bsptr,tfmat,&cv,bplm,tol,bound->cvpts,vlst,bound->uvpts);
					if (n == -466) return (466);
					if (n<=0) return (UU_FAILURE);

					n2 += n;
					if (cvids) cvids->bncrv[i].segix[0] = n2;
				}

				np[i] = n2-n1;
				n1 = n2;
/*
...get inner boundary UV curves if any
*/
				if (i<nb-1) status = ncl_tsf_get_boundary (eptr,i+1,&cv);
			}
		}
		if (status != UU_SUCCESS) return (status);
	}
/*
...any other type of surface has only 4 U/V lines boundary
...create boundary points by evolving surface on boundaries
*/
	else
	{
		ur[0] = vr[0] = 0.;
		ur[1] = vr[1] = 1.;

		n2 = um_evolve_boxbndr (eptr,tfmat,ur,vr,tol,bound->cvpts,vlst,
			bound->uvpts,cvids);

		np[0] = n2;
		ummx[0][0] = vmmx[0][0] = 0.;
		ummx[0][1] = vmmx[0][1] = 1.;
	}
/*
.....Check if uv box of boundary is possible to evaluate
*/
	np[nb] = 0;
	if (bound->uvpts != UU_NULL)
	{
		ncc = UU_LIST_LENGTH(bound->uvpts);
		if (ncc > 0)
		{
			um_cshape_fix (bound->cvpts,bound->uvpts,vlst,cvids,nb,np);
/*
..... calculate the UV-boxes
*/
			uvdat = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
			um_cshape_dirchk (uvdat,nb,np,ummx,vmmx);
		}
	}
	else
	{
		ummx[0][0] = vmmx[0][0] = 0.;
		ummx[0][1] = vmmx[0][1] = 1.;
	}

	if (status == UU_SUCCESS)
	{
		bound->nb = nb;
		bound->np = np;
		bound->ummx = ummx;
		bound->vmmx = vmmx;
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION: um_pre_srf_bndr (eptr,tfmat,bound,cvids)
**       Pre-processing surfaces: get boundary curve of surface (points
**       in UV space and XYZ with req. tolerance), surface span in UV
**       space and number of boundary curves (for trimmed surf).
**    PARAMETERS
**       INPUT  :
**          eptr    - pointer to surface.
**          tfmat   - pointer to translation mx.
**          bound   - surface boundary structure, with allocated memory and
**                    preset bound->toler - tolerance in XYZ space
**          cvids   - curves id to evolve
**       OUTPUT :
**          bound   - updated boundary.
**          cvids   - list of pointers to starting points of each
**
**    RETURNS      :   UU_SUCCESS / UU_FAILURE, or 466 if evaluator failed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_pre_srf_bndr (eptr,tfmat,bound,cvids)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UM_srf_boundary *bound;
struct NCL_uvconv *cvids;
{
	int status;
	int nb, *np, itis, ifl, ignore_inner=0;
	UU_REAL bplm[4];
	UM_2Dcoord *ummx, *vmmx;
	struct NCL_fixed_databag bs, *bsptr;

	UU_REAL tol = bound->toler;
	np = UU_NULL;
	nb = 1;
	itis = 0;
	status = UU_SUCCESS;
/*
...get surface data
*/
	status = ncl_retrieve_data_fixed (eptr);
	if (status != UU_SUCCESS) goto Done;
	if (ncl_itsa_trimsrf (eptr))
	{
		itis = 1;
		ncl_trimsrf_get_fixed (eptr,&nb,bplm);
	}
	if (cvids)
	{
		cvids->no_bcv = nb;
		cvids->bncrv = (struct NCL_uvbncv *)
			uu_malloc (nb*sizeof(struct NCL_uvbncv));
	}
/*
.....Set nb to 1 to ignore inner boundaries when the option is on 
*/	
	if (itis)
	{
		ifl = 394; getifl(&ifl,&ignore_inner);
		if (ignore_inner) nb = 1;
	}
/*
...allocate memory space
*/
	np = (int *) uu_malloc((nb+1)*sizeof(int));
	ummx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
	vmmx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
/*
...get base surface when processing trimmed surface
*/
	if (itis == 1)
	{
		bsptr  = (struct NCL_fixed_databag *) &bs;
		status = ncl_trimsrf_get_bs (eptr,&bsptr);
	}

	if (status == UU_SUCCESS)
		status = um_evolve_bndr (tol,eptr,bsptr,tfmat,itis,nb,np,ummx,vmmx,bplm,
											bound,NULLST,cvids);
Done:
	return (status);
}

/*********************************************************************
**    E_FUNCTION: um_uvlist_init (eptr,cvlist)
**       Initialize structure to store evolved boundary curve data
**    PARAMETERS
**       INPUT  :
**          eptr    - pointer to surface (trimmed or not)
**       OUTPUT :
**          cvlist  - structure holding break points of boundary
**                    curves for all curves creating boundaries.
**    RETURNS      :   0
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_uvlist_init (eptr,cvlist)
struct NCL_fixed_databag *eptr;
struct NCL_uvconv *cvlist;
{
	int status, i, nb;
	UU_KEY_ID *keys = UU_NULL;
	struct NCL_fixed_databag cv;
	UU_REAL bplm[4];

	cvlist->no_key = 0;
	cvlist->crvid = UU_NULL;
	cvlist->no_bcv = 0;
	cvlist->bncrv = UU_NULL;
	if ((status = ncl_retrieve_data_fixed (eptr)) == UU_SUCCESS)
	{
		if (ncl_itsa_trimsrf (eptr))
		{
			ncl_trimsrf_get_fixed (eptr,&nb,bplm);
			if (nb <= 0) return (UU_FAILURE);
			keys = (UU_KEY_ID *) uu_malloc (nb*sizeof(UU_KEY_ID));
			for (i = 0; i < nb && status == UU_SUCCESS; i++)
			{
				status = ncl_tsf_get_boundary (eptr,i,&cv);
				keys[i] = cv.key;
			}
			if (status != UU_SUCCESS)
			{
				UU_FREE(keys); return (status);
			}
		}
		else
		{
			nb   = 1;
			keys = (UU_KEY_ID *) uu_malloc (nb*sizeof(UU_KEY_ID));
			keys[0] = 0;
		}
		cvlist->no_key = nb;
		cvlist->crvid = keys;
		cvlist->no_bcv = 0;
		cvlist->bncrv = UU_NULL;
	}
	return(status);
}

/*********************************************************************
**    FUNCTION: um_uvlist_free (cvlist)
**       Free memory allocated for structure to store evolved boundary
**       curve data
**    PARAMETERS
**       INPUT  :
**          cvlist  - structure holding break points of boundary
**                    curves for all curves creating boundaries.
**       OUTPUT : none
**    RETURNS      :   0
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_uvlist_free (cvlist)
struct NCL_uvconv *cvlist;
{
	int i;
	struct NCL_uvbncv *bndr;

	if (cvlist->no_key > 0 && cvlist->crvid) uu_free (cvlist->crvid);
	if (cvlist->no_bcv > 0)
	{
		bndr = cvlist->bncrv;
		for (i=0; i<cvlist->no_bcv; i++, bndr++)
		{
			if (bndr->no_csg > 0) uu_free (bndr->segix);
		}
		uu_free (cvlist->bncrv);
	}
	return (UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION : void um_init_boundary (p)
**       Initialize boundary structure.
*********************************************************************/
void um_init_boundary (p)
UM_srf_boundary *p;
{
	p->toler = 0;
	p->nb = 0;
	p->np = UU_NULL;
	p->ummx = p->vmmx = UU_NULL;
	p->uvpts = p->cvpts = NULLST;
}

/*********************************************************************
**    FUNCTION : void um_free_boundary (p)
**       Dealocate memory used for boundary structure of a surface
**    PARAMETERS
**       INPUT  :
**          p      - boundary structure
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_free_boundary (p)
UM_srf_boundary *p;
{
	p->nb = 0;
	UU_FREE(p->np);
	UU_FREE (p->ummx);
	UU_FREE (p->vmmx);
	UU_LIST_FREE (p->uvpts)
	UU_LIST_FREE (p->cvpts)
}

/*********************************************************************
**    FUNCTION : void um_move_boundary (p,q)
**       Moves a boundary structure into a new structure and initializes
**       the original boundary structure.
**    PARAMETERS
**       INPUT  :
**          p      - boundary structure to move
**       OUTPUT :
**          q      - new boundary structure
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_move_boundary (p,q)
UM_srf_boundary *p,*q;
{
	q->toler = p->toler;
	q->nb = p->nb;
	q->np = p->np;
	q->ummx = p->ummx;
	q->vmmx = p->vmmx;
	q->uvpts = p->uvpts;
	q->cvpts = p->cvpts;
	um_init_boundary(p);
}

/*********************************************************************
**    E_FUNCTION     : um_print_bndr (itsk,p)
**       Prints out the boundary as polylines.
**    PARAMETERS
**       INPUT  :
**          itsk       0 for UV, 1 for XYZ
**          p          boundary struct
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_print_bndr (itsk,p)
int itsk;
UM_srf_boundary *p;
{
	UU_LIST *cvpoint;
	UU_REAL xx,yy,zz,xx1,yy1,zz1;
	UM_coord *pp;
	int i,ib,np;

/*
.....Print out the curve
*/
	if (itsk == 1)
		cvpoint = p->cvpts;
	else
		cvpoint = p->uvpts;

	if (cvpoint == UU_NULL) return;

	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);

	for (ib = 0; ib < p->nb; ib++)
	{
		np = p->np[ib];
		printf ("\n\n%% %d points\n",np);

		for (i = 0; i < np-1; i++)
		{
			xx=pp[i][0];
			yy=pp[i][1];
			if (itsk == 1)
				zz=pp[i][2];
			else
				zz = 0;
			if (fabs(xx)<0.0001) xx=0.;
			if (fabs(yy)<0.0001) yy=0.;
			if (fabs(zz)<0.0001) zz=0.;
			xx1=pp[i+1][0];
			yy1=pp[i+1][1];
			if (itsk == 1)
				zz1=pp[i+1][2];
			else
				zz1 = 0;
			if (fabs(xx1)<0.0001) xx1=0.;
			if (fabs(yy1)<0.0001) yy1=0.;
			if (fabs(zz1)<0.0001) zz1=0.;

			ncl_print_ln6 (xx,yy,zz,xx1,yy1,zz1);
		}

		pp += np;
	}
}
