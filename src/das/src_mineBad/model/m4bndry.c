/*********************************************************************
**    NAME         :  m4bndry.c
**       CONTAINS: Routines to process boundary curves (trimmed SF)
**                 and curves on surface 
**
**       um_cshape_inschk,      um_cshape_inschk1
**       um_get_rltv_tol
**       um_isect_3dbox
**       um_rcmp (t1,t2)
**       um_pre_srf_curve
**       um_bndr_curve
**       um_inside_bndry,       um_inside_bndry1 
**       um_free_netsf_common_boundary
**       um_cshape_3dbox,       um_cshape_3dbox1
**       um_match_bndry,        um_match_bndry1
**       um_stor_mbndry
**       um_make_box
**       um_match_seg_bndry
**		 um_inside_bndr2
**
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       m4bndry.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:00
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

static UU_LIST plst;
static int plst_init = 0;
static UU_REAL dt[500];

static UM_vector Uaxis = {1.,0.,0.};
static UM_vector Vaxis = {0.,1.,0.};

UU_REAL um_get_rltv_tol();
extern UU_LOGICAL NCL_lv93;

char *uu_malloc();

/*#define DEBUGON 1*/

/*********************************************************************
**    E_FUNCTION: um_cshape_inschk (pts,nb,nbpt,point,xextr,yextr)
**       Checks if point is inside/outside the closed curve 
**       represented by polyline
**    PARAMETERS
**       INPUT  :
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          nb     - number of closed boundary curves in pts
**          nbpt   - start pointers of boundary curve in pts
**          point  - point in question.
**          xextr  - min X & max X of the polyline hull. 
**          yextr  - min Y & max Y of the polyline hull. 
**       OUTPUT :
**          none
**    RETURNS      :  1 - point is inside, 
**                    0 = point on curve within tolerance,
**                   -1 - point is outside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cshape_inschk (pts,nb,nbpt,point,xextr,yextr)
UM_coord pts[], point;
UU_REAL (*xextr)[2], (*yextr)[2];
int nb, *nbpt;
{
	int stat, i, ix, n;

	stat = -1;
	for (i=0, ix=0; i<nb; i++)
	{
		n    = nbpt[i];
		stat = um_cshape_inschk1 (pts[ix],n,point,xextr[i],yextr[i]);
		if (i > 0) stat = -stat;
		if (stat < 0) break;  
		ix  += n;
	}
	return(stat); 
}

/*********************************************************************
**    E_FUNCTION     : um_cshape_inschk1 (pts,npt,point,xextr,yextr)
**       Checks if point is inside/outside the closed curve 
**       represented by polyline
**    PARAMETERS
**       INPUT  :
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**          point  - point in question.
**          xextr  - min X & max X of the polyline hull. 
**          yextr  - min Y & max Y of the polyline hull. 
**       OUTPUT :
**          none
**    RETURNS      :  1 - point is inside, 
**                    0 = point on curve within tolerance,
**                   -1 - point is outside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cshape_inschk1 (pts, npt, point, xextr, yextr)
UM_coord pts[], point;
UU_REAL *xextr, *yextr;
int npt;
{
	int ins;
	UU_REAL toluv,tolsq;

/*
...get tolerance
*/
	toluv = um_get_rltv_tol (xextr,yextr);
	tolsq = toluv*toluv;
/*
...easy solution:
...check if point is outside box
*/
	if (point[0]-xextr[1] > toluv || 
		 xextr[0]-point[0] > toluv) return (-1);
	if (point[1]-yextr[1] > toluv || 
		 yextr[0]-point[1] > toluv) return (-1);
/*
...point is in the box
*/
	ins = um_pt_in_contour (pts,npt,Uaxis,Vaxis,point,toluv,tolsq);

	return (ins);
}

/*********************************************************************
**    E_FUNCTION: um_get_rltv_tol (xextr,yextr)
**       Get accuracy to use in other routines based on the size of 
**       area where calculations will apply
**    PARAMETERS
**       INPUT  :
**          xextr  - min X & max X of the polyline hull. 
**          yextr  - min Y & max Y of the polyline hull. 
**       OUTPUT :
**    RETURNS      :  tolerance
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_get_rltv_tol (xextr,yextr)
UU_REAL *xextr, *yextr;
{
	UU_REAL a, b;

	a = xextr[1] - xextr[0];
	b = yextr[1] - yextr[0];

	return (MAX2 (a,b) * 1.e-4);
}

/*********************************************************************
**    E_FUNCTION: um_rcmp (t1,t2)
**       Comparison routine for the sort algorithm (uu_qsort).  
**    PARAMETERS
**       INPUT  :
**          t1     - first element to be compared 
**          t2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if t1 < t2
**                     0 if t1 = t2
**                     1 if t1 > t2
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_rcmp(t1,t2)
UU_REAL *t1,*t2;
{

	if (*t1 > *t2) return(1);

	else if (*t1 < *t2) return(-1);

	else return(0);
}

/*********************************************************************
**    E_FUNCTION: um_pre_srf_curve (eptr,tfmat,crv,tol,npt,uvptr,cvpts)
**       Pre-processing curve on surfaces: get curve on surface (points
**       in UV space and XYZ with req. tolerance). In this routine the
**       curve is assumed noncomposite (the calling routine calls it 
**       separately for each component)
**    PARAMETERS
**       INPUT  :
**          eptr    - pointer to surface. 
**          tfmat   - pointer to transformation mx. 
**          crv     - pointer to uv curve on surface.
**          tol     - tolerance in XYZ space.
**       OUTPUT :
**          npt     - number of points in uvptr & cvpts lists.
**          uvptr   - pointer to list with UV points of curve.
**          cvpts   - pointer to list with XYZ points of curve.
**
**    RETURNS      :   UU_SUCCESS / UU_FAILURE, 
**                     or 466 (error in surface evaluator)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_pre_srf_curve (eptr,tfmat,crv,tol,npt,uvptr,cvpts)
struct NCL_fixed_databag *eptr,*crv;
UM_transf tfmat;
UU_REAL tol;
int *npt;
UU_LIST *uvptr, *cvpts;
{
	int status, nb, n2, itis;
	UU_REAL bplm[4];
	struct NCL_fixed_databag bs, *bsptr;
	struct UM_rbsplcrv_rec rbcv, *rbptr;

	n2 = itis = 0;
	*npt = 0;
	bsptr = (struct NCL_fixed_databag *) &bs;
	bplm[0] = bplm[2] = 0;
	bplm[1] = bplm[3] = 1;
	status = UU_SUCCESS;
/*
...get surface data
*/
	if (ncl_retrieve_data_fixed (eptr) != 0) 
		return (UU_FAILURE);
	if (ncl_itsa_trimsrf (eptr))
	{
		itis = 1;
		ncl_trimsrf_get_fixed (eptr,&nb,bplm);
		status = ncl_trimsrf_get_bs (eptr,&bsptr);
		if (status != UU_SUCCESS) return (UU_FAILURE);
	}
	else if (eptr->rel_num == UM_UVCVONSF_REL)
	{
		struct UM_uvcvonsf_rec *uvcv;
		uvcv = (struct UM_uvcvonsf_rec *) eptr;
		bs.key = uvcv->bskey;	
		status = ncl_retrieve_data_fixed (&bs);
		if (status != UU_SUCCESS) return (UU_FAILURE);
	}
	else
		bsptr = eptr;

	if (itis == 1)
	{
/*
.....If it's a UV curve on surface
.....Then copy it to a Bspline structure
.....Bobby  -  2/15/00
*/
		if (crv->rel_num == UM_UVCVONSF_REL)
		{
			rbptr = &rbcv;
			ncl_cp_struct_uvcv_rbcv (crv,&rbptr);
		}
		else
			rbptr = (struct UM_rbsplcrv_rec *)crv;
		n2 = ncl_evolve_bn_crv_on_srf
			(bsptr,tfmat,rbptr,bplm,tol,cvpts,UU_NULL,uvptr);

		if (n2 == -466)  return (466);
		else if (n2 <= 0) return (UU_FAILURE);
	}
/*
.....curve on surface: !!! ncl only temporary solution !!!
.....later use ncl_get_cv_on_sf where check if it is ncl geometry or openncl
.....and use call like below or just point to the curve structure when openncl
.....Same should be done above where itis is set to 2 to get base surface.
*/
	else
	{
		rbptr = &rbcv;
		ncl_cp_struct_uvcv_rbcv (crv,&rbptr);
		n2 = ncl_evolve_bn_crv_on_srf
			(bsptr,tfmat,rbptr,bplm,tol,cvpts,UU_NULL,uvptr);
		if (n2 == -466 )  return (466);
		else if (n2 <= 0) return (UU_FAILURE);
	}
	*npt = n2;

	return (status);
}

/*********************************************************************
**    E_FUNCTION: um_bndr_curve (eptr,tfmat,cvid,subid,subid1,idir,
**							tol,nbpt,cvpts)
**       Pre-processing curve on surfaces: get curve on surface (points
**       in XYZ with req. tolerance). In this routine the curve is a 
**       either a single composite component, or a single boundary curve 
**       (outer or inner, composite or not).
**    PARAMETERS
**       INPUT  :
**          eptr    - pointer to surface. 
**          tfmat   - pointer to translation mx. 
**          tol     - tolerance in XYZ space.
**          cvid    - number of boundary curve (0 or more)
**          subid   - start sub_id of composite boundary (1 or more)
**          subid1  - end sub_id of composite boundary (1 or more)
**          idir    - CLW/CCLW direction from start id isubid to end id subid1
**					  1 : CLW, -1 : CCLW
**       OUTPUT :
**          nbpt    - distribution of points among components.
**          cvpts   - pointer to list with XYZ points of curve.
**
**    RETURNS      :   UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**                   
**    WARNINGS     : none
*********************************************************************/
int um_bndr_curve (eptr,tfmat,cvid,subid,subid1,idir,tol,nbpt,cvpts,tangs)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UU_REAL tol;
int cvid,subid,**nbpt;
UU_LIST *cvpts,*tangs;
{
	struct NCL_fixed_databag bs, *bsptr, cv, c2;
	int status,iscomp,nb,ncv,nedge,rev,n,n2,j,j3,*nbp;
	UU_REAL bplm[4];
	UU_LOGICAL lrev;

	status = UU_FAILURE;
	nb = iscomp = ncv = j3 = 0;
	cvpts->cur_cnt = 0;
	nbp = UU_NULL;
	lrev = UU_FALSE;
/*
...get surface data
*/
	if (ncl_retrieve_data_fixed (eptr) != 0) goto Done;
	if (!ncl_itsa_trimsrf (eptr)) goto Done;

	ncl_trimsrf_get_fixed (eptr,&nb,bplm);
	if (cvid < 0 || cvid >= nb) goto Done;

	bsptr = (struct NCL_fixed_databag *) &bs;
	status = ncl_trimsrf_get_bs (eptr,&bsptr);
	if (status != UU_SUCCESS) goto Done;
	status = ncl_tsf_get_boundary (eptr,cvid,&cv);
	if (status != UU_SUCCESS) goto Done;

	iscomp = ncl_itsa_compcrv(&cv);
	if (iscomp) status = ncl_compcrv_getnents (&cv, &ncv);
	if (status != UU_SUCCESS || (iscomp && subid >= ncv)) return (UU_FAILURE);

	if (iscomp)
	{
		if (subid >= 0 && subid1 == -1 && idir == 0)
		{
			status = ncl_compcrv_getelm (&cv, subid, &c2, &rev);
			if (status == UU_SUCCESS)
			{
				n = ncl_evolve_bn_crv_on_srf(bsptr,tfmat,&c2,bplm,tol,cvpts,
						tangs,UU_NULL);
				if (n <= 0) return (UU_FAILURE);

				if (rev) ncl_revers_list (n,0,cvpts,UU_NULL,UU_NULL);
				ncl_fix_tol1 (0,&n,tol,cvpts,tangs);
			}
		}
		else
		{
/*
.....Check if the whole boundary needs to be elvoved
*/
			if (subid <= 0 && subid1 == 0 && idir == 0)
			{
				subid = 0;
				subid1 = ncv;
				nedge = ncv;
			}
			else
			{
/*
.....Get the number of edges
*/
				if (subid < subid1)
				{
					if (idir == 1)
						nedge = subid1 - subid + 1;
					else if (idir == -1)
					{
						nedge = ncv - (subid1 - subid) + 1;
						lrev = UU_TRUE;
					}
				}
				else if (subid > subid1)
				{
					if (idir == -1)
					{
						nedge = subid - subid1 + 1;
						lrev = UU_TRUE;
					}
					else if (idir == 1)
						nedge = ncv - (subid - subid1) + 1;
				}
				else 
					nedge = 1;
			}

			nbp = (int *) uu_malloc((nedge+1)*sizeof(int));
			nbp[0] = nedge;

/*
..... for each of the k=1,2,...,ncv components nbpt[k] contains the 
..... corresponding number of points
*/
			for (j = subid, n2=0; status == UU_SUCCESS; )
			{				
				status = ncl_compcrv_getelm (&cv, j, &c2, &rev);
				if (status == UU_SUCCESS)
				{
					n = ncl_evolve_bn_crv_on_srf(bsptr,tfmat,&c2,bplm,tol,cvpts,
							tangs,UU_NULL);
					if (n <= 0) 
						status = UU_FAILURE;
					else
					{
						if (rev || lrev) ncl_revers_list (n,n2,cvpts,UU_NULL,UU_NULL);
						ncl_fix_tol1 (n2,&n,tol,cvpts,tangs);
						nbp[j3+1] = n;
						n2 += n;
					}
				}
							
				j3++;
				if (idir == 0)
				{
					j++;
					if (j >= subid1) break;
				}
				else
				{			
					if (j == subid1) break;
					j = j + idir;
					if (j == ncv) j = 0;
					if (j < 0) j = ncv-1;
				}
			}
		}
	}
	else
	{
		n = ncl_evolve_bn_crv_on_srf(bsptr,tfmat,&cv,bplm,tol,cvpts,
				tangs,UU_NULL);
		if (n <= 0) 
			status = UU_FAILURE;
		else
			ncl_fix_tol1 (0,&n,tol,cvpts,tangs);
	}

Done:
	if (nbp)
	{
		if (status != UU_SUCCESS) uu_free (nbp);
		else *nbpt = nbp;
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION: um_inside_bndry (uv,point,bound)
**       Checks if a point is inside/outside a trimmed surface boundary.
**    PARAMETERS
**       INPUT  :
**          uv     - point in question (UV coordinates).
**          point  - point in question (XYZ coordinates).
**          bound  - boundary structure
**       OUTPUT :
**
**    RETURNS      :  1 - point is inside, 
**                    0 = point on curve within tolerance,
**                   -1 - point is outside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_inside_bndry (uv,point,bound,tol)
UM_srf_boundary *bound;
UM_coord uv,point;
UM_real8 *tol;
{
	int ib,np,insf; 
	UM_coord *uvptr,*cvptr;
	UU_REAL dtol;
	UM_int2 idx;
	UM_real8 ver;

	idx = 169;
	getsc(&idx, &ver);
	dtol = (ver < 9.299) ? bound->toler : *tol;

	uvptr = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	cvptr = (UM_coord *)UU_LIST_ARRAY(bound->cvpts);

	insf = -1;
	for (ib=0; ib<bound->nb; ib++)
	{
		np = bound->np[ib];
		insf = um_inside_bndry1 
			(uv,point,dtol,np,uvptr,cvptr,bound->ummx[ib],bound->vmmx[ib]);
		if (ib > 0) insf = -insf;
		if (insf < 0) break;
		uvptr += np;
		cvptr += np;
	}
	return (insf);
}

/*********************************************************************
**    E_FUNCTION: um_inside_bndry1 (uv,point,tol,np,uvptr,cvpts,xextr,yextr)
**       Checks if point is inside/outside the closed curve 
**       represented by polyline. 
**    PARAMETERS
**       INPUT  :
**          uv     - point in question (UV coordinates).
**          point  - point in question (XYZ coordinates).
**          uvptr  - UV-coordinates of the polyline, last
**                   point is same as the first point in array.
**          cvpts  - XYZ-coordinates of the polyline.
**          np     - number of points in polyline.
**          xextr  - min X & max X of the polyline hull.
**          yextr  - min Y & max Y of the polyline hull.
**          tol    - tolerance 
**       OUTPUT :
**
**    RETURNS      :  1 - point is inside, 
**                    0 = point on curve within tolerance,
**                   -1 - point is outside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_inside_bndry1 (uv,point,tol,np,uvptr,cvpts,xextr,yextr)
UM_coord uv,point;
UU_REAL tol;
int np; 
UM_coord *uvptr,*cvpts;
UU_REAL *xextr, *yextr;
{
	int i; 
	UU_REAL d,dln,ax,bx,ay,by;
	UM_vector vln,vec;
	int rcross;
/*
...easy solution:
...check if point is outside box
*/
   if (uv[0]-xextr[1] > UM_FUZZ ||
       xextr[0]-uv[0] > UM_FUZZ) return (-1);
   if (uv[1]-yextr[1] > UM_FUZZ ||
       yextr[0]-uv[1] > UM_FUZZ) return (-1);

	rcross = 0;

	for (i=1; i<np; i++)
	{
		um_vcmnvc (cvpts[i],cvpts[i-1],vln);
		um_vcmnvc (point,cvpts[i-1],vec);
		dln = um_mag (vln);
		if (dln <= tol) continue;
		vln[0] /= dln; vln[1] /= dln; vln[2] /= dln;
		d = um_dot (vec,vln);
		if (d >= -tol && d <= dln + tol) 
		{
			um_cross (vec,vln,vec);
			d = UM_MAG (vec);
			if (d <= tol) return (0);
		}
		ax = uvptr[i-1][0] - uv[0]; ay = uvptr[i-1][1] - uv[1];
		if (ax*ax+ay*ay < UM_DFUZZ) 
			return (0);
		bx = uvptr[i][0] - uv[0]; by = uvptr[i][1] - uv[1];
		if (bx*bx+by*by < UM_DFUZZ) 
			return (0);
		d = ax*by - ay*bx;
/*
..... if not on the edge decide if the edge crosses the ray going from
..... the point in positive x-direction
*/
		if (by > UM_FUZZ != ay > UM_FUZZ)
		{
			if ((by - ay)*d > 0.)
				rcross++;
		}
	}
/*
..... odd number of crossings means "inside"; odd means "outside"
*/
	if ((rcross%2) == 1) 
		return (1);
	else
		return (-1);
}

/*********************************************************************
**    E_FUNCTION: um_inside_bndr2 (uv,point,tol,np,uvptr,cvpts,xextr,yextr,luv)
**       Checks if point is inside/outside the closed curve 
**       represented by polyline. 
**    PARAMETERS
**       INPUT  :
**          uv     - point in question (UV coordinates).
**          point  - point in question (XYZ coordinates).
**          uvptr  - UV-coordinates of the polyline, last
**                   point is same as the first point in array.
**          cvpts  - XYZ-coordinates of the polyline.
**          np     - number of points in polyline.
**          xextr  - min X & max X of the polyline hull.
**          yextr  - min Y & max Y of the polyline hull.
**          tol    - tolerance 
**          luv    - UU_TRUE:  Check point in UV coordinate
**                 - UU_FALSE: Check point in XYZ coordinate
**       OUTPUT :
**
**    RETURNS      :  1 - point is inside, 
**                    0 = point on curve within tolerance,
**                   -1 - point is outside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_inside_bndr2 (uv,p,tol,np,uvptr,cvpts,xextr,yextr,luv,mm)
UM_coord uv,p;
UU_REAL tol;
int np,mm; 
UM_coord *uvptr,*cvpts;
UU_REAL *xextr, *yextr;
UU_LOGICAL luv;
{
	int counter = 0;
	int i;
	double xinters;
	UM_coord p1,p2,point;
	UU_REAL d,dln,ax,bx,ay,by,cx,cy;
	UU_REAL tolsq = 2.25*tol*tol, tolSQ = 4.*tol*tol;
	UU_REAL btol = 1.e-5,btolsq = 1.0e-10;
	UM_vector vln,vec;
	int rcross;
	UU_LOGICAL onb;
	
	if (mm == 1)
	{
		tolsq *= 645.16;
		tolSQ *= 645.16;
	}
/*
...check if point is outside box
*/	
	if (uv[0]-xextr[1] > UM_FUZZ || xextr[0]-uv[0] > UM_FUZZ)	
		return (-1);
	if (uv[1]-yextr[1] > UM_FUZZ || yextr[0]-uv[1] > UM_FUZZ)	 
		return (-1);

	if (luv)
	{ 
		um_vctovc (uv,point);  
		um_vctovc (uvptr[0],p1); 
	}  
	else	   
	{
		um_vctovc (p,point);
		um_vctovc (cvpts[0],p1);
	}

	for (i = 1; i < np; i++)
	{
		onb = UU_FALSE;
		um_vcmnvc (cvpts[i],cvpts[i-1],vln);
		um_vcmnvc (p,cvpts[i-1],vec);
		dln = UM_MAG (vln);
		if (dln <= tol) continue;
		vln[0] /= dln; vln[1] /= dln; vln[2] /= dln;
		d = UM_DOT (vec,vln);
		if (d >= -tol && d <= dln + tol)
		{
			um_cross (vec,vln,vec);
			d = UM_DOT (vec,vec);
			if (d <= tolsq) 
				return (0);
			onb = (d <= tolSQ);
		}
		ax = uvptr[i-1][0] - uv[0]; ay = uvptr[i-1][1] - uv[1];
		if (ax*ax+ay*ay < btolsq) 
			return (0);
		bx = uvptr[i][0] - uv[0]; by = uvptr[i][1] - uv[1];
		if (bx*bx+by*by < btolsq) 
			return (0);
		d = ax*by - ay*bx;
		if (onb && ax*bx + ay*by < 0.)
		{
			cx = uvptr[i][0] - uvptr[i-1][0]; 
			cy = uvptr[i][1] - uvptr[i-1][1];
			dln = cx*cx + cy*cy;
			if (dln < btolsq) 
				continue;
			if (d*d < 4.*btolsq*dln)
				return (0);
		}

		if (luv)
			um_vctovc (uvptr[i],p2);
		else	 
			um_vctovc (cvpts[i],p2);
		if (point[1] > MIN2(p1[1],p2[1]))
		{
			if (point[1] <= MAX2(p1[1],p2[1]))
			{
				if (point[0] <= MAX2(p1[0],p2[0]))
				{
					if (p1[1] != p2[1])
					{
						xinters = (point[1]-p1[1])*(p2[0]-p1[0])/(p2[1]-p1[1])+p1[0];
						if (p1[0] == p2[0] || point[0] <= xinters)
							counter++;
					}
				}
			}
		}
		um_vctovc (p2,p1);
	}

	if (counter % 2 == 0)
		return(-1);
	else
		return(1);
}

/*********************************************************************
**    E_FUNCTION: um_inside_bndr (uv,point,bound)
**       Checks if a point is inside/outside a trimmed surface boundary.
**    PARAMETERS
**       INPUT  :
**          uv     - point in question (UV coordinates).
**          point  - point in question (XYZ coordinates).
**          bound  - boundary structure
**			luv	   - check in UV space if TRUE, in xyz space if FALSE
**       OUTPUT :
**
**    RETURNS      :  1 - point is inside, 
**                    0 = point on curve within tolerance,
**                   -1 - point is outside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_inside_bndr (uv,point,bound,tol,luv)
UM_srf_boundary *bound;
UM_coord uv,point;
UM_real8 *tol;
UU_LOGICAL luv;
{
	int ib,np,insf; 
	UM_coord *uvptr,*cvptr;
	UU_REAL dtol;
	UM_int2 idx,mm;
	UM_real8 ver;

	idx = 169;
	getsc(&idx, &ver);
	dtol = (ver < 9.299) ? bound->toler : *tol;

	idx = 264;	
	getifl(&idx,&mm);

	uvptr = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	cvptr = (UM_coord *)UU_LIST_ARRAY(bound->cvpts);

	insf = -1;
	for (ib=0; ib<bound->nb; ib++)
	{
		np = bound->np[ib];
		if (luv)
			insf = um_inside_bndr1 
			(uv,point,dtol,np,uvptr,cvptr,bound->ummx[ib],bound->vmmx[ib],mm);
		else
			insf = um_inside_bndr2 
			(uv,point,dtol,np,uvptr,cvptr,bound->ummx[ib],bound->vmmx[ib],luv,mm);
		if (ib > 0) insf = -insf;
		if (insf < 0) break;
		uvptr += np;
		cvptr += np;
	}
	return (insf);
}

/*********************************************************************
**    E_FUNCTION: um_inside_bndr1 (uv,point,tol,np,uvptr,cvpts,xextr,yextr)
**       Checks if point is inside/outside the closed curve 
**       represented by polyline. 
**    PARAMETERS
**       INPUT  :
**          uv     - point in question (UV coordinates).
**          point  - point in question (XYZ coordinates).
**          uvptr  - UV-coordinates of the polyline, last
**                   point is same as the first point in array.
**          cvpts  - XYZ-coordinates of the polyline.
**          np     - number of points in polyline.
**          xextr  - min X & max X of the polyline hull.
**          yextr  - min Y & max Y of the polyline hull.
**          tol    - tolerance 
**			mm	   - unit
**       OUTPUT :
**
**    RETURNS      :  1 - point is inside, 
**                    0 = point on curve within tolerance,
**                   -1 - point is outside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_inside_bndr1 (uv,point,tol,np,uvptr,cvpts,xextr,yextr,mm)
UM_coord uv,point;
UU_REAL tol;
int np,mm; 
UM_coord *uvptr,*cvpts;
UU_REAL *xextr, *yextr;
{
	int i; 
	UU_REAL d,dln,ax,bx,ay,by,cx,cy;
	UU_REAL tolsq = 2.25*tol*tol, tolSQ = 4.*tol*tol;
	UU_REAL btol = 1.e-5,btolsq = 1.0e-10;
	UM_vector vln,vec;
	int rcross;
	UU_LOGICAL onb;

	if (mm == 1)
	{
		tolsq *= 645.16;
		tolSQ *= 645.16;
	}

/*
.....Check if point is outside box
*/
   if (uv[0]-xextr[1] > btol || xextr[0]-uv[0] > btol)
	   return (-1);
   if (uv[1]-yextr[1] > btol || yextr[0]-uv[1] > btol)
	   return (-1);

	rcross = 0;

	for (i = 1; i < np; i++)
	{
		onb = UU_FALSE;
		um_vcmnvc (cvpts[i],cvpts[i-1],vln);
		um_vcmnvc (point,cvpts[i-1],vec);
		dln = UM_MAG (vln);
		if (dln <= tol) continue;
		vln[0] /= dln; vln[1] /= dln; vln[2] /= dln;
		d = UM_DOT (vec,vln);
		if (d >= -tol && d <= dln + tol)
		{
			um_cross (vec,vln,vec);
			d = UM_DOT (vec,vec);
			if (d <= tolsq)
				return (0);
			onb = (d <= tolSQ);
		}
		ax = uvptr[i-1][0] - uv[0]; ay = uvptr[i-1][1] - uv[1];
		if (ax*ax+ay*ay < btolsq) 
			return (0);
		bx = uvptr[i][0] - uv[0]; by = uvptr[i][1] - uv[1];
		if (bx*bx+by*by < btolsq) 
			return (0);
		d = ax*by - ay*bx;
		if (onb && ax*bx + ay*by < 0.)
		{
			cx = uvptr[i][0] - uvptr[i-1][0]; cy = uvptr[i][1] - uvptr[i-1][1];
			dln = cx*cx + cy*cy;
			if (dln < btolsq) continue;
			if (d*d < 4.*btolsq*dln) return (0);
		}
/*
..... if not on the edge decide if the edge crosses the ray going from
..... the point in positive x-direction
*/
		if (by > btol != ay > btol)
		{
			if ((by - ay)*d > 0.)
				rcross++;
		}
	}
/*
..... odd number of crossings means "inside"; odd means "outside"
*/
	if ((rcross%2) == 1) 
		return (1);
	else
		return (-1);
}

/*********************************************************************
**    E_FUNCTION     : um_cshape_3dbox1 (pts,npt,xextr,yextr,zextr)
**       Make a box enclosing the closed curve (represented by polyline)
**    PARAMETERS
**       INPUT  :
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**       OUTPUT :
**          xextr  - min X & max X of the polyline hull. 
**          yextr  - min Y & max Y of the polyline hull. 
**          zextr  - min Z & max Z of the polyline hull. 
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void um_cshape_3dbox1 (pts, npt, xextr, yextr, zextr)
UM_coord *pts;
UU_REAL xextr[2], yextr[2], zextr[2];
int npt;
{
	UU_REAL xmin, xmax, ymin, ymax, zmin, zmax;
	int i;

	xmin = xmax = pts[0][0];
	ymin = ymax = pts[0][1];
	zmin = zmax = pts[0][2];

	for (i=1; i<npt; i++)
	{
		if (xmax < pts[i][0]) 
		  xmax = pts[i][0]; 
		else 
		  if (xmin > pts[i][0]) xmin = pts[i][0]; 

		if (ymax < pts[i][1]) 
		  ymax = pts[i][1]; 
		else 
		  if (ymin > pts[i][1]) ymin = pts[i][1];

		if (zmax < pts[i][2]) 
		  zmax = pts[i][2]; 
		else 
		  if (zmin > pts[i][2]) zmin = pts[i][2];
	}

	xextr[0] = xmin;
	xextr[1] = xmax;
	yextr[0] = ymin;
	yextr[1] = ymax;
	zextr[0] = zmin;
	zextr[1] = zmax;

	return;
} 

/*********************************************************************
**    E_FUNCTION     : um_cshape_3dbox (pts,nb,nbpt,xextr,yextr,zextr)
**       Make boxes enclosing closed curves (represented by polyline)
**    PARAMETERS
**       INPUT  :
**          pts    - array of points representing closed curve(s).
**          nbpt   - array holding number of points in each curve.
**       OUTPUT :
**          xextr  - min X & max X for each curve.
**          yextr  - min Y & max Y for each curve.
**          zextr  - min Z & max Z for each curve.
**    RETURNS      :
**       UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int um_cshape_3dbox (pts, nbpt, xextr, yextr, zextr)
UM_coord pts[];
UU_REAL (*xextr)[2], (*yextr)[2], (*zextr)[2];
int *nbpt;
{
	int i, ix, n;
	i = 0;
	ix = 0;
	while (nbpt[i] > 0)
	{
		n = nbpt[i];
		if (n <= 0) return (UU_FAILURE);
		um_cshape_3dbox1 (&pts[ix],n,xextr[i],yextr[i],zextr[i]);
		ix += n;
		i++;
	}
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION:  um_match_bndry (pts1,nbp1,pts2,nbp2,tol,nipt,obuf)
**          Calculate the common boundary of two surfaces.
**          
**    PARAMETERS
**       INPUT  :
**          pts1   - 1-st surface boundary (outer boundary + maybe
**                   several inner ones - each a closed curve)
**          nbp1   - number of points in each boundary curve for sf1
**          pts2   - 2-nd surface boundary (see pts1 note)
**          nbp2   - number of points in each boundary curve for sf2
**          tol    - tolerance (vicinity radius)
**       OUTPUT :
**          nipt   - counter of points in obuf marking single point
**                   (intersection) with 1, and segments with 2 (list).  
**          obuf   - output buffer with matching points (list)
**    RETURNS      :  number of matching segments/ios in obuf.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_match_bndry (pts1,nbp1,pts2,nbp2,tol,nipt,obuf)
UM_coord pts1[], pts2[];
UU_REAL tol;
int *nbp1, *nbp2;
UU_LIST *obuf, *nipt;
{
	UM_2Dcoord *xex2, *yex2, *zex2;
	int i, ix, n, nb2, ncb;
	UU_REAL tolsq = tol*tol;

	nb2 = 0;
	while (nbp2[nb2] > 0) nb2++;

	ncb = 0;
	xex2 = (UM_2Dcoord *) uu_malloc(nb2*sizeof(UM_2Dcoord));
	yex2 = (UM_2Dcoord *) uu_malloc(nb2*sizeof(UM_2Dcoord));
	zex2 = (UM_2Dcoord *) uu_malloc(nb2*sizeof(UM_2Dcoord));

	uu_list_init (&plst, sizeof(UM_coord), 100, 100);
	plst_init = 1;
/*
...create all boxes for SRF2
*/   
	if (um_cshape_3dbox (pts2, nbp2, xex2, yex2, zex2) != UU_SUCCESS)
		return (0);

	i = ix = 0;
	n = nbp1[0];
	while (n > 0)
	{
		ncb += 
		um_match_bndry1 (pts1[ix],n,pts2,nb2,nbp2,xex2,yex2,zex2,tol,tolsq,nipt,obuf);
		ix += n; i++;
		n = nbp1[i];
	}
	if (xex2) uu_free (xex2);
	if (yex2) uu_free (yex2);
	if (zex2) uu_free (zex2);
	if (plst_init == 1)
	{
		uu_list_free (&plst); plst_init = 0;
	}

	return (ncb);
}

/*********************************************************************
**    E_FUNCTION:  um_stor_mbndry (numc,tbuf,tol,nipt,obuf)
**          Store matching boundary points in the list checking if
**          segments are connected.
**          
**    PARAMETERS
**       INPUT  :
**          numc   - number of segments in tbuf array
**          tbuf   - array of points.
**          tol    - tolerance (vicinity radius)
**       OUTPUT :
**          nipt   - counter of points in obuf specifying number of
**                   points (in obuf) in the single curve section.
**          obuf   - output buffer with matching points (list)
**    RETURNS      :  UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void um_stor_mbndry (numc,tbuf,tolsq,nipt,obuf,np0,ni0)
int numc,np0,ni0;
UU_REAL tolsq;
UM_coord *tbuf;
UU_LIST *nipt, *obuf;
{
	UM_coord *bptr;
	int *nptr, i, npts, lsti;
	int lmatch;
	int NIL = 0;

	bptr = (UM_coord *) UU_LIST_ARRAY (obuf);
	npts = obuf->cur_cnt - 1;
	nptr = (int *) UU_LIST_ARRAY (nipt);
/*
...remember when initializing nipt list push 0 in the first element 
*/
	for (i = 0; i < numc; i++)
	{
		lmatch = 0;
		lsti = nipt->cur_cnt-2;
/*
..... first point of tbuf is matched with the last point in the list.
..... if they are same, the number of components does not change, the
..... second point of tbuf is pushed onto the last component.
..... if there are more segments in tbuf, they are stored one-by-one,
..... each as a separate two-point component.
*/
		if (i == 0 && lsti >= 0 && nptr[lsti] > 0 &&
			(UM_SQDIS(bptr[npts],tbuf[0]) <= tolsq))
			lmatch = 1;
		else if (i == numc-1 && np0 >= 0 && ni0 >= 0 &&
			(UM_SQDIS(bptr[np0],tbuf[2*numc-1]) <= tolsq))
			lmatch = -1;

		if (lmatch == 1)
		{
			uu_list_push (obuf,&tbuf[1]);
			nptr[lsti]++;
		}
		else if (lmatch == -1)
		{
/*
..... last point of tbuf is matched with the first point in the list.
*/
			uu_list_insert (obuf,np0,&tbuf[2*numc-2]);
			nptr[ni0]++;
		}
		else
		{
			uu_list_push (obuf,&tbuf[i*2]);
			uu_list_push (obuf,&tbuf[i*2+1]);
			nptr[lsti+1] = 2;
		}
		if (lmatch < 1)
		{
			uu_list_push (nipt,&NIL);
			if (i+1 < numc) nptr = (int *) UU_LIST_ARRAY (nipt);
		}
	}
	return; 
}

/*********************************************************************
**    E_FUNCTION: um_make_box (pt1, pt2, bxx, bxy, bxz)
**       Make a box defined in cartesian coordinates enclosing 2 points
**    PARAMETERS
**       INPUT  :
**          pt1  - input point 1.
**          pt2  - input point 2.
**       OUTPUT :
**          bxx  - min X & max X of the box.
**          bxy  - min Y & max Y of the box.
**          bxz  - min Z & max Z of the box.
**    RETURNS      :   0 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void um_make_box (pt1, pt2, bxx, bxy, bxz)
UU_REAL *bxx, *bxy, *bxz;
UM_coord pt1, pt2;
{
/*
...get box for the line to intersect
*/
	bxx[0] = MIN2 (pt1[0],pt2[0]);
	bxx[1] = MAX2 (pt1[0],pt2[0]);
	bxy[0] = MIN2 (pt1[1],pt2[1]);
	bxy[1] = MAX2 (pt1[1],pt2[1]);
	bxz[0] = MIN2 (pt1[2],pt2[2]);
	bxz[1] = MAX2 (pt1[2],pt2[2]);
 
	return;
}

/*********************************************************************
**    E_FUNCTION: um_isect_3dbox (bxx1,bxy1,bxz1,bxx2,bxy2,bxz2,tol)
**       Checks if two boxes overlaps within given tolerance
**    PARAMETERS
**       INPUT  :
**          bxx1  - min X & max X of the first box.
**          bxy1  - min Y & max Y of the first box.
**          bxz1  - min Z & max Z of the first box.
**          bxx2  - min X & max X of the second box.
**          bxy2  - min Y & max Y of the second box.
**          bxz2  - min Z & max Z of the second box.
**          tol   - tolerance
**       OUTPUT :
**    RETURNS      :  UU_FALSE = boxes do not overlap
**                    UU_TRUE = boxes overlap
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_isect_3dbox (bxx1, bxy1, bxz1, bxx2, bxy2, bxz2, tol)
UU_REAL *bxx1, *bxy1, *bxx2, *bxy2, *bxz1, *bxz2, tol;
{
	return (bxx1[0] <= bxx2[1]+tol && bxx1[1] >= bxx2[0]-tol &&
		  bxy1[0] <= bxy2[1]+tol && bxy1[1] >= bxy2[0]-tol &&
		  bxz1[0] <= bxz2[1]+tol && bxz1[1] >= bxz2[0]-tol);
}

/*********************************************************************
**    E_FUNCTION:  um_match_seg_bndry (pt1,pt2,pts,npt,xmm,ymm,zmm,
**                                     tol,obuf)
**          Check if input segment matches any segment on the boundary 
**          closed curve represented by polyline.  Returns 0 when no
**          matching segment found, 1 if segment intersects with curve
**          or 2 when segments match along some distance. 
**          
**    PARAMETERS
**       INPUT  :
**          pt1    - 1-st point of the line
**          pt2    - 2-nd point of the line
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**          xmm    - min X & max X of the polyline hull.
**          ymm    - min Y & max Y of the polyline hull.
**          zmm    - min Y & max Y of the polyline hull.
**          tol    - tolerance (vicinity radius)
**       OUTPUT :
**          obuf   - output buffer with matching points (list)
**    RETURNS      :  number of matching segments/ios in obuf.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int um_match_seg_bndry (pt1,pt2,pts,npt,xmm,ymm,zmm,tol,tolsq)
UM_coord pt1, pt2, *pts;
UU_REAL *xmm, *ymm, *zmm, tol, tolsq;
int npt;
{
	int i,j,ipt,last,is1,is2;
	int um_rcmp();
	UU_REAL bxx[2], bxy[2], bxz[2], bsx[2], bsy[2], bsz[2];
	UU_REAL dln,dsg,co,sisq,dis2,d,p0,p1,p2,s1,s2,dd1,dd2;
	UM_vector uvln, uvsg, vc, v1;
	UU_REAL tolsq1 = 0.1*tolsq;
	UU_REAL tol1 = tol/3;
	UU_LOGICAL lparal;
	
/*
..... get the segment's box.
..... if the box intersects with the curve's box
..... scan all segments of curve
*/
	um_make_box (pt1, pt2, bxx, bxy, bxz);
	if (!um_isect_3dbox (bxx,bxy,bxz,xmm,ymm,zmm,tol)) return (0);
	ipt = 0;
	last = 0;
	plst.cur_cnt = 0;

	um_vcmnvc (pt2, pt1, uvln);
	dln = UM_DOT (uvln,uvln);
	if (dln < tolsq1) return (0);
	dln = sqrt(dln);
	for (j = 0; j < 3; j++)	uvln[j] = uvln[j]/dln;

	for (i = 1; i < npt; i++)
	{
		is1 = i-1; is2 = i;
		um_make_box (pts[is1],pts[is2],bsx,bsy,bsz);
		if (!um_isect_3dbox (bxx,bxy,bxz,bsx,bsy,bsz,tol)) continue;
/*
...if boxes overlap check distances. Consider following:
...build a cylinder with radius 'tol' along (pt1,pt2) segment and
...intersect pts segment line with the cylinder if distance 
...between lines is less than tol.
*/
		um_vcmnvc (pts[is2],pts[is1],uvsg);
		dsg = UM_DOT (uvsg,uvsg);
		if (dsg < tolsq1) continue;
		dsg = sqrt(dsg);
		for (j = 0; j < 3; j++)	uvsg[j] = uvsg[j]/dsg;
		co = UM_DOT(uvln,uvsg);
		if (fabs(co) < UM_COS45) continue;
		if (co < 0)
		{
			co = -co;
			for (j = 0; j < 3; j++)	uvsg[j] = -uvsg[j];
			is1 = i; is2 = i-1;
		}

		lparal = (1. - co < UM_DFUZZ);

		um_vcmnvc(pts[is1],pt1,v1);
		s1 = UM_DOT (v1,uvln);

		if (!lparal)
		{
			sisq = 1. - co*co;
			um_cross (uvln,uvsg, vc);
			d = UM_DOT(v1,vc);
			dis2 = d*d / sisq;
		}
		else
			dis2 = UM_DOT(v1,v1) - s1*s1;

		if (dis2 > tolsq) continue;


		if (lparal)
		{
			p1 = 0; p2 = dln;
		}
		else
		{
			um_vctmsc(uvsg, co, vc);
			um_vcmnvc(uvln, vc, vc);
			p0 = UM_DOT (v1, vc) / sisq;
			d = co*sqrt((tolsq-dis2)/sisq);
			p1 = p0 - d; p2 = p0 + d;
			p1 = MAX2 (p1,0); p2 = MIN2 (p2,dln);
			if (p2 - p1 < tol1) continue;
		}

		um_vcmnvc(pts[is2],pt1,vc);
		s2 = UM_DOT (vc,uvln);

		dd1 = MAX2 (s1,p1);
		dd2 = MIN2 (s2,p2);

		if (dd2 - dd1 < tol1) continue;
/*
...store point distance relative to the start of input segment
*/
		dt[last++] = dd1;
		dt[last++] = dd2;
		if (last >= 500) break;
	}

/*
...sort points along input segment, create points,
...discard connected segments and save segment points
...in buffer
*/
	if (last != 0)  
	{
		UM_coord pti;

		uu_qsort (dt,last,sizeof(UU_REAL),um_rcmp);
		um_vctmsc (uvln,dt[0],vc);
		um_vcplvc (pt1,vc,pti);
		uu_list_push (&plst,pti);
		um_vctmsc (uvln,dt[1],vc);
		um_vcplvc (pt1,vc,pti); 
		uu_list_push (&plst,pti);
		ipt++;
		for (j=2; j<last; j+=2)
		{
			if ((dt[j]-dt[j-1]) > tol) 
			{
				um_vctmsc (uvln,dt[j],vc);
				um_vcplvc (pt1,vc,pti);
				uu_list_push (&plst,pti);
				ipt++;
			}
			else
				plst.cur_cnt--;

			um_vctmsc (uvln,dt[j+1],vc);
			um_vcplvc (pt1,vc,pti);
			uu_list_push (&plst,pti);
		}
	}
/*
.....Debug output of common boundary
*/
#ifdef DEBUGON
	if (ipt > 0)
	{
		char tbuf[80];
		sprintf(tbuf,"Common boundary tol = %g",tol);
		NclxDbgPstr(tbuf);
		for (i=0;i<ipt;i++)
		{
			sprintf(tbuf,"   Pt[%d] = %g,%g,%g",i,obuf[i][0],obuf[i][1],
				obuf[i][2]);
			NclxDbgPstr(tbuf);
		}
	}
#endif

	return(ipt);
}

/*********************************************************************
**    E_FUNCTION:  um_match_bndry1 (pts,npt,pts2,nb2,nbp2,xex,yex,zex,tol,
**                                     nipt,obuf)
**          Match a closed polyline with a surface boundary.
**          
**    PARAMETERS
**       INPUT  :
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**          pts2   - surface boundary
**          nb2    - number of closed polylines in pts2 array.
**          nbp2   - number of points in pts2 array.
**          xex    - min X & max X of the polyline hull.
**          yex    - min Y & max Y of the polyline hull.
**          zex    - min Y & max Y of the polyline hull.
**          tol    - tolerance (vicinity radius)
**       OUTPUT :
**          nipt   - counter of points in obuf marking single point
**                   (intersection) with 1, and segments with 2 (list).  
**          obuf   - output buffer with matching points (list)
**    RETURNS      :  number of matching segments/ios in obuf.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_match_bndry1 (pts,npt,pts2,nb2,nbp2,xex,yex,zex,tol,tolsq,nipt,obuf)
UM_coord pts[], pts2[];
UM_2Dcoord *xex, *yex, *zex;
UU_REAL tol, tolsq;
int npt, nb2, nbp2[];
UU_LIST *nipt, *obuf;
{
	int i,j,ix,np,ncv,numc,ni0,np0,npcur,nicur;
	UM_coord *tbuf;
/*
..... match each segment of a single boundary curve of SRF1 against
..... every boundary curve of SRF2
*/   
	ix = 0;
	numc = 0; 
	for (j=0; j<nb2; j++) 
	{
		np = nbp2[j];
		ni0 = np0 = -1;
		for (i=1; i<npt; i++) 
		{
			if (NCL_lv93)
			{
				tbuf = (UM_coord *) UU_LIST_ARRAY (&plst);
				ncv = um_match_seg_bdnry (pts[i-1],pts[i],pts2[ix],np,
								xex[j],yex[j],zex[j],tol,tbuf);
				
			}
			else
			{
				ncv = um_match_seg_bndry (pts[i-1],pts[i],&pts2[ix],np,
								xex[j],yex[j],zex[j],tol,tolsq);
			}
/*
...glue points
*/
			if (ncv > 0)
			{
				int mp0,mi0;

				tbuf = (UM_coord *) UU_LIST_ARRAY (&plst);
				npcur = obuf->cur_cnt;
				nicur = nipt->cur_cnt;

				if (i < npt-1 || NCL_lv93)
				{
					mp0 = mi0 = -1;
				}
				else
				{
					mp0 = np0; mi0 = ni0;
				}

				um_stor_mbndry (ncv,tbuf,tolsq,nipt,obuf,mp0,mi0);

				if (i == 1 && obuf->cur_cnt > npcur && nipt->cur_cnt > nicur &&
					(UM_SQDIS(pts[0],tbuf[0]) <= tolsq))
				{
					np0 = npcur;
					ni0 = nicur - 1;
				}
			}
			numc += ncv;
		}
		ix += np;
	}
	return (numc);
}

/*********************************************************************
**    E_FUNCTION:  um_match_seg_bdnry (pt1,pt2,pts,npt,xmm,ymm,zmm,
**                                     tol,obuf)
**          Check if input segment matches any segment on the boundary 
**          closed curve represented by polyline.  Returns 0 when no
**          matching segment found, 1 if segment intersects with curve
**          or 2 when segments match along some distance. 
**          
**    PARAMETERS
**       INPUT  :
**          pt1    - 1-st point of the line
**          pt2    - 2-nd point of the line
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**          xmm    - min X & max X of the polyline hull.
**          ymm    - min Y & max Y of the polyline hull.
**          zmm    - min Y & max Y of the polyline hull.
**          tol    - tolerance (vicinity radius)
**       OUTPUT :
**          obuf   - output buffer with matching points (list)
**    RETURNS      :  number of matching segments/ios in obuf.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_match_seg_bdnry (pt1,pt2,pts,npt,xmm,ymm,zmm,tol,obuf)
UM_coord pt1, pt2, pts[];
UU_REAL *xmm, *ymm, *zmm, tol;
int npt;
UM_coord *obuf;
{
	int i, j, ipt, last, is1, is2, ndir, stat, um_rcmp();
	UU_REAL bxx[2], bxy[2], bxz[2], bsx[2], bsy[2], bsz[2], dsg, rr;
	UU_REAL p, q, r, dis, dln, dp[2], ds[2], cal, calp;
	UU_REAL relz, dmin, dd[2], um_vcdir();
	UM_vector vln, vsg, vl1, vl2, vc;
	UM_coord ppp, ps[2];
	UU_REAL tol1 = tol;
	int click = 0;

	ipt = 0;
	last = 0;
/*
...get box for the line to intersect
*/
	um_make_box (pt1, pt2, bxx, bxy, bxz);
/*
...if box intersects with curve's box
...scan all segments of curve
*/
	if (um_isect_3dbox (bxx,bxy,bxz,xmm,ymm,zmm,tol) == 1)
	{
		um_vcmnvc (pt2, pt1, vln);
		dln = um_mag (vln);
		um_unitvc (vln, vl1);
		dmin = 1000000.;

Retry:;
		for (i=1; i<npt; i++)
		{
			j = i - 1;
			um_make_box (pts[i],pts[j],bsx,bsy,bsz);
			if (um_isect_3dbox (bxx,bxy,bxz,bsx,bsy,bsz,tol) == 1)
			{
/*
...if boxes overlap check distances. Consider following:
...build a cylinder with radius 'tol' along (pt1,pt2) segment and
...intersect pts segment line with the cylinder if distance 
...between lines is less than tol.
*/
				um_vcmnvc (pts[i],pts[j],vsg);
				dsg = um_mag (vsg);
				um_unitvc (vsg,vl2);
				stat = um_lnlndis (pt1,vl1,pts[j],vl2,&dis,ppp);
/*
...if line intersects with cylinder get points of intersection
...and their distance from middle point along line
*/
				if (dis < tol)
				{
					calp = um_dot (vl1,vl2);
/*
...if segments are || assume middle point on input segment
*/
					if (stat == 1) 
					{
						um_vctmsc (vln,.5,vc);
						um_vcplvc (pt1,vc,ppp);
						p = .5 * dln; 
					}
					else
					{
/*
...get point position relative to ppp in direction of segment
*/
						cal = fabs(calp);
						q = sqrt (tol*tol-dis*dis);
						p = q * (cal / sqrt(1.0-cal*cal)); 
					}

					dp[0] = relz = um_vcdir (pt1,ppp,vl1);
					dp[1] = um_vcdir (pt2,ppp,vl1);
/*
...reverse ps[1],ps[2] when
...curve goes opposite direction than line segment
*/
					ndir = (calp >= 0)? 0: 1; 
					is1  = (ndir == 0)? j: i; 
					is2  = (ndir == 0)? i: j; 
					dp[0] = MAX2 (dp[0],-p);
					dp[1] = MIN2 (dp[1],p);
					um_nptln (pts[is1],pt1,vl1,ps[0]);
					um_nptln (pts[is2],pt1,vl1,ps[1]);
					if (dsg > tol)
					{
						r  = um_dcccc(pts[is1],ps[0]);
						rr = um_dcccc(pts[is2],ps[1]);
						if (rr > r) r = rr;
						if (click == 1 && r > tol1) continue;
					}
					else
						r = 1000000.;
					ds[0] = um_vcdir (ps[0],ppp,vl1);
					ds[1] = um_vcdir (ps[1],ppp,vl1);
					ds[0] = MAX2 (ds[0],-p);
					ds[1] = MIN2 (ds[1],p);
/*
...reject not overlaping segments
*/
					if (dp[0] > dp[1] || ds[0] > ds[1]) continue;
					dd[0] = MAX2 (ds[0],dp[0]);
					dd[1] = MIN2 (ds[1],dp[1]);
					if (dd[0] > dd[1]) continue;
/*
...store point distance relative to the start of input segment
*/
					if (click == 0 && r < dmin) dmin = r;
/*
..... dmin measures the best match for obtained segments with the two
..... boundaries
*/ 
					dt[last++] = dd[0] - relz;
					dt[last++] = dd[1] - relz;
				}
			}
		}
	}
	if (last > 2 && dmin < 0.5*tol && click == 0) 
	{
		click = 1;
		last = 0;
		tol1 = MAX2 (2.*dmin, 0.1*tol);
		goto Retry;
	}
/*
...sort points along input segment, create points,
...discard connected segments and save segment points
...in buffer
*/
	if (last != 0)  
	{
		uu_qsort (dt,last,sizeof(UU_REAL),um_rcmp);
		um_vctmsc (vl1,dt[0],vc);
		um_vcplvc (pt1,vc,obuf[0]); 
		um_vctmsc (vl1,dt[1],vc);
		um_vcplvc (pt1,vc,obuf[1]); 
		ipt++;
		for (j=2; j<last; j+=2)
		{
			if ((dt[j]-dt[j-1]) > tol) 
			{
				um_vctmsc (vl1,dt[j],vc);
				um_vcplvc (pt1,vc,obuf[ipt*2]); 
				ipt++;
			}
			um_vctmsc (vl1,dt[j+1],vc);
			um_vcplvc (pt1,vc,obuf[ipt*2-1]); 
		}
	}
/*
.....Debug output of common boundary
*/
#ifdef DEBUGON
	if (ipt > 0)
	{
		char tbuf[80];
		sprintf(tbuf,"Common boundary tol = %g",tol);
		NclxDbgPstr(tbuf);
		for (i=0;i<ipt;i++)
		{
			sprintf(tbuf,"   Pt[%d] = %g,%g,%g",i,obuf[i][0],obuf[i][1],
				obuf[i][2]);
			NclxDbgPstr(tbuf);
		}
	}
#endif

	return(ipt);
}

/*********************************************************************
**    FUNCTION : int um_free_netsf_common_boundary (p)
**       Dealocate memory used for common boundary structure of a net surface
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
int um_free_netsf_common_boundary (p)
UM_netsf_common_bndry *p;
{
	p->key = p->num = 0;
	if (p->surfaces) 
	{
		uu_list_free (p->surfaces);
		uu_free (p->surfaces);
	}
	if (p->np) 
	{
		uu_list_free (p->np);
		uu_free (p->np);
	}
	if (p->lengths) 
	{
		uu_list_free (p->lengths);
		uu_free (p->lengths);
	}
	if (p->pts) 
	{
		uu_list_free (p->pts);
		uu_free (p->pts);
	}
	
	return (UU_SUCCESS);
}
