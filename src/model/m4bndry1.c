/*********************************************************************
**    NAME         :  m4bndry1.c
**       CONTAINS: Routines to intersect boundary curves (trimmed SF)
**
**       um_uv_isect_bndry
**		 um_uv_isect_bndry1
**       um_isect_cls_bndry
**		 um_isect_cls_bndry1
**		 um_isect_cls_bndry0
**		 um_isect_cls_bndry2 
**		 um_isect_pln_bndry0
**		 um_isect_pln_bndry
**		 um_point_on_between_line
**		 um_point_on_between_triangle
**
**
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       m4bndry1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:01
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
#include "ncl.h"

/*********************************************************************
**    E_FUNCTION: um_ucmp (t1,t2)
**       Comparison routine for the sort algorithm (uu_qsort): compares
**       u-coordinates.  
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
int um_ucmp(t1,t2)
UM_2Dcoord t1,t2;
{
	if (t1[0] > t2[0])
		return(1);
	else if (t1[0] < t2[0])
		return(-1);
	else
		return(0);
}

/*********************************************************************
**    E_FUNCTION: um_vcmp (t1,t2)
**       Comparison routine for the sort algorithm (uu_qsort): compares
**       v-coordinates.  
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
int um_vcmp(t1,t2)
UM_2Dcoord t1,t2;
{
	if (t1[1] > t2[1])
		return(1);
	else if (t1[1] < t2[1])
		return(-1);
	else
		return(0);
}

/*********************************************************************
**    E_FUNCTION: um_discmp (e1,e2)
**       Comparison routine for the sort algorithm (uu_qsort).  
**    PARAMETERS
**       INPUT  :
**          e1     - first element to be compared 
**          e2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if e1.param < e2.param 
**                     0 if e1.param = e2.param
**                     1 if e1.param > e2.param 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_discmp(e1,e2)
UM_pointd *e1,*e2;
{
	if (e1->param > e2->param)
		return(1);
	else if (e1->param < e2->param)
		return(-1);
	else 
		return(0);
}

/*********************************************************************
**    E_FUNCTION: um_discmp (e1,e2)
**       Comparison routine for the sort algorithm (uu_qsort).  
**    PARAMETERS
**       INPUT  :
**          e1     - first element to be compared 
**          e2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if e1->uv.param < e2->uv.param 
**                     0 if e1->uv.param = e2->uv.param
**                     1 if e1->uv.param > e2->uv.param 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_discmp2(e1,e2)
UM_cvio *e1,*e2;
{
	if (e1->uv.param > e2->uv.param)
		return(1);
	else if (e1->uv.param < e2->uv.param)
		return(-1);
	else
		return(0);
}

/*********************************************************************
**    FUNCTION :  um_nearest_crv (pt,pts,npts)
**
**      Given the point pt, Calculates the nearest point index from curve(pts,npts)
**
**    PARAMETERS
**       INPUT  :
**					pt    - point to check
**                  pts   - array of points.
**                  npts  - Number of points in 'pts'.
**       OUTPUT :
**    RETURNS      : index of the nearest point of the curve
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_nearest_crv (inpt,npt,pt,pts,npts)
UM_coord pt;
UM_coord *pts;
int inpt,npt,npts;
{
	int i,ist,iend,indx = 0;
	UU_REAL sqdist1,sqdist2,sqlen,dist,dmin = 1000000.0;

	UM_vector vc;					
	UM_vector ulvc;
	UM_coord  pt1,nlpt;

	for (i=0; i<npts-1; i++)
	{		
/*
.....Project point pt onto line(pts[i],pts[i+1]
*/
		um_vcmnvc_2d(pts[i+1],pts[i],vc);
		vc[2] = 0.0;
		um_unitvc(vc,ulvc);
		um_vctovc_2d(pts[i],pt1);
		pt1[2] = 0.0;
		um_nptln(pt,pt1,ulvc,nlpt);
/*
.....Check if projected point is between pts[i] and pts[i+1]
*/
		sqlen = um_sqdis_2d(pts[i],pts[i+1]);
		sqdist1 = um_sqdis_2d(nlpt,pts[i]);
		sqdist2 = um_sqdis_2d(nlpt,pts[i+1]);
		if (sqdist1 > sqlen + UM_DFUZZ || sqdist2 > sqlen + UM_DFUZZ)
			continue;

/*
.....Get minmum distance from point pt to projected point nplt
*/
		dist = um_sqdis_2d(pt,nlpt);
		if (dist < UM_DFUZZ * UM_DFUZZ)
		{
			indx = i;
			break;
		}
		if (dist < dmin)
		{
			dmin = dist;
			indx= i;
		}
	}
	return indx;
}

/*********************************************************************
**    E_FUNCTION:  um_uv_isect_bndry (uv,cvtyp,bound,iobuf)
**          Intersects trimmed surface boundary with an isoparametric 
**          line. Everything is in u,v space (2D). 
**    PARAMETERS
**       INPUT  :
**          uv     - constant parameter
**          cvtyp  - 1 = u_curve, 2 = v_curve.
**          bound  - trimmed surface boundary
**       OUTPUT :
**          iobuf  - output buffer with intersection points (list)
**    RETURNS      :  number of intersection points in iobuf 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_uv_isect_bndry (uv,cvtyp,bound,iobuf)
UU_REAL uv;
int cvtyp;
UM_srf_boundary *bound;
UU_LIST *iobuf;
{
	int i, ix, ni, n;
	int um_ucmp(),um_vcmp();       /* sort condition routines */
	UM_2Dcoord *bptd,*minmax;
	UM_coord *pts;
	UU_REAL tmp;

	pts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	minmax = (cvtyp == 1) ? bound->vmmx : bound->ummx;
/*
.....Intersect line with all closed boundaries
*/ 
	ni = 0;
	for (i=0, ix=0; i<bound->nb; i++)
	{
		n = bound->np[i];
		if (uv < minmax[i][1] && uv > minmax[i][0])
			ni += um_uv_isect_bndry1 (uv,cvtyp,pts[ix],n,iobuf);
		ix += n;
	}
/*
.....sort all points in buffer: v=const lines are sorted by u-values, 
.....vice versa for u=const
*/
	bptd = (UM_2Dcoord *) UU_LIST_ARRAY (iobuf);
	if (cvtyp == 1)
	{
		if (ni >= 3)
			uu_qsort (bptd,ni,sizeof(UM_2Dcoord),um_ucmp);
		else if (ni == 2 && bptd[0][0] > bptd[1][0])
		{
			tmp = bptd[0][0];
			bptd[0][0] = bptd[1][0];
			bptd[1][0] = tmp;
		}
	}
	else
	{
		if (ni >= 3)
			uu_qsort (bptd,ni,sizeof(UM_2Dcoord),um_vcmp);
		else if (ni == 2 && bptd[0][1] > bptd[1][1])
		{
			tmp = bptd[0][1];
			bptd[0][1] = bptd[1][1];
			bptd[1][1] = tmp;
		}
	}

	return (ni);
}

/*********************************************************************
**    E_FUNCTION:  um_uv_isect_bndry1 (uv,cvtyp,pts,npt,iobuf)
**    Finds intersection points of a line u=const or v=const with 
**    a surface boundary curve.
**    PARAMETERS
**       INPUT  :
**          uv     - constant parameter
**          cvtyp  - 1 = u_curve, 2 = v_curve.
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**       OUTPUT :
**          iobuf  - output buffer with intersection points (list)
**    RETURNS      :  number of points in iobuf 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_uv_isect_bndry1 (uv,cvtyp, pts, npt, iobuf)
UU_REAL uv;
int cvtyp;
UM_coord pts[];
int npt;
UU_LIST *iobuf;
{
	int i,j,m,num,np1,kind;
	UM_2Dcoord pc;

	kind = (cvtyp == 1)? 1: 0;
	m = 1 - kind;
	np1 = npt-1;
/*
..... for v=uv=const (cvtyp=1): find polyline segments whose lower end is
..... below and upper end is above, and intersect them with the line.
..... similarly for cvtyp=2.
*/
	for (i=0,num=0; i<np1; i++)
	{
		j = um_mod (i-1,np1);
		if ((pts[i][kind] > uv && pts[j][kind] <= uv)
			|| ( pts[i][kind] < uv && pts[j][kind] >= uv))
		{
			num++;
			while (fabs(pts[j][kind]-pts[i][kind]) < UM_DFUZZ) 
				j = um_mod (--j,np1);

			pc[kind] = uv;
			pc[m] = pts[i][m] + (uv - pts[i][kind])*
				(pts[j][m] - pts[i][m])/(pts[j][kind]-pts[i][kind]);
		
			uu_list_push (iobuf,&pc);
		}
	}
	return(num);
}  

/*********************************************************************
**    E_FUNCTION:  um_isect_cls_bndry (pt1,pt2,bound,tol,iobuf)
**          Intersects trimmed surface boundaries represented by nb
**          closed polylines with a segment (represented by two points). 
**          Both the segment & polylines are in u,v space (2D). 
**    PARAMETERS
**       INPUT  :
**          pt1    - 1-st point of the line
**          pt2    - 2-nd point of the line
**			vln	   - vector from 1-st point to 2-nd point
**			dln	   - distance from 1-st point to 2-nd point
**          bound  - surface boundary structure
**			tol	   - tolerance
**       OUTPUT :
**          iobuf  - output buffer with intersection points (list)
**    RETURNS      :  number of intersection points in iobuf 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isect_cls_bndry (pt1,pt2,vln,dln,bound,tol,iobuf)
UM_coord pt1, pt2;
UM_2Dcoord vln;
UM_srf_boundary *bound;
UU_REAL dln,tol;
UU_LIST *iobuf;
{
	int i, ix, ni, n;
	int um_discmp();       /* sort condition routine */
	UM_pointd *bptd;
	UU_REAL (*xmm)[2],(*ymm)[2];
	UU_REAL bxx[2],bxy[2];
	UU_REAL tmp,btol;
	UM_coord *pts,ppp;

	pts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	btol = MAX2 (0.1 * tol, 1.e-5);

/*
.....get box for the line to intersect
*/
	bxx[0] = MIN2 (pt1[0],pt2[0]);
	bxx[1] = MAX2 (pt1[0],pt2[0]);
	bxy[0] = MIN2 (pt1[1],pt2[1]);
	bxy[1] = MAX2 (pt1[1],pt2[1]);
/*
.....intersect segment with all closed boundaries
*/ 
	ni = 0;
	xmm = bound->ummx; ymm = bound->vmmx;
	for (i=0,ix=0; i<bound->nb; i++)
	{
		n = bound->np[i];
		if (um_isect_boxes (bxx,bxy,xmm[i],ymm[i],btol) == 1)
		{
			ni += um_isect_cls_bndry1
					  (pt1,pt2,vln,dln,pts[ix],n,btol,iobuf);
		}
		ix += n;
	}
	if (ni < 2) return (ni);
/*
.....sort all points in buffer by distance from pt1
*/
	bptd = (UM_pointd *) UU_LIST_ARRAY (iobuf);
	if (ni >= 3)
		uu_qsort (bptd,ni,sizeof(UM_pointd),um_discmp);
	else if (ni == 2 && bptd[0].param > bptd[1].param)
	{
		tmp = bptd[0].param; um_vctovc (bptd[0].point,ppp);
		bptd[0].param = bptd[1].param; um_vctovc (bptd[1].point,bptd[0].point);
		bptd[1].param = tmp; um_vctovc (ppp,bptd[1].point);
	}
/*
.....weed out points
*/
	for (i = 1; i < ni; i++)
	{
		if (bptd[i].param - bptd[i-1].param < btol)
		{
			uu_list_delete (iobuf, i, 1);
			ni--; i--;
		}
	}

	return (ni);
}

/*********************************************************************
**    E_FUNCTION:  um_isect_cls_bndry1 (pt1,pt2,pts,npt,xmm,ymm,ni,tol,iobuf)
**          Intersects single boundary closed curve represented by
**          polyline with line (represented by two points). 
**          Both line & polyline are in u,v space (2D). 
**    PARAMETERS
**       INPUT  :
**          pt1    - 1-st point of the line
**          pt2    - 2-nd point of the line
**			vln	   - vector from 1-st point to 2-nd point
**			dln	   - distance from 1-st point to 2-nd point
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**			tol	   - tolerance
**       OUTPUT :
**          iobuf  - output buffer with intersection points (list)
**    RETURNS      :  number of points in iobuf 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isect_cls_bndry1 (pt1,pt2,vln,dln,pts,npt,tol,iobuf)
UM_coord pt1,pt2,pts[];
UM_2Dcoord vln;
UU_REAL dln,tol;
int npt;
UU_LIST *iobuf;
{
	int nn,i,np,ipt,ipt0,ipt1;
	UM_pointd ptd;

	ipt = 0;
	ipt0 = iobuf->cur_cnt;
	np = npt - 1;
/*
..... first check if any boundary point is on the segment; add it if so:
..... this is the reason to pass tol to this routine, since [pt1,pt2]
..... could overlap with a boundary segment up to tolerance, but not exactly;
..... and um_isect_segs tries to find the exact intersection.
*/
		for (i=0; i<np; i++)
		{
			nn = um_ptinsg (pt1,vln,dln,pts[i],tol,&ptd.param);
			if (nn == 1)
			{
				um_vctovc (pts[i], ptd.point);
				uu_list_push (iobuf,&ptd);
				ipt++;
			}
		}
		ipt1 = ipt;

		for (i=1; i<npt; i++)
		{
/*
.....get segment box & check for intersection with line box
*/
			nn = um_isect_segs (pt1,vln,dln,pts[i-1],pts[i],tol,&ptd);
			if (nn > 0)
			{
/*
.....if some boundary points have been added - do not add points within tol 
.....to a boundary point
*/
				if (ipt1 > ipt0)
				{
					int j;
					UM_pointd *bptd = (UM_pointd *) UU_LIST_ARRAY (iobuf);

					for (j = ipt0; j < ipt1 && nn > 0; j++)
					{
						if (fabs(bptd[j].param - ptd.param) < tol) nn = 0;
					}
				}
				if (nn > 0)
				{
					uu_list_push (iobuf,&ptd);
					ipt++;
				}
			}
		}

	return (ipt);
}  

/*********************************************************************
**    E_FUNCTION:  um_isect_cls_bndry0 (uv1,uv2,pt1,pt2,vln,dln,
**										bound,bound1,pl,tol,iobuf)
**      Intersects trimmed surface boundaries represented by bound
**      with a segment (represented by two points(uv1,uv2) first,
**		once found, calculate the actual intersection of bound1 
**		with 3D segment(pt1,pt2)
**    PARAMETERS
**       INPUT  :
**          uv1	   - 1-st uv point of the line
**          uv2    - 2-nd uv point of the line
**          pt1    - 1-st point of the line
**          pt2    - 2-nd point of the line
**			vln	   - vector from 1-st point to 2-nd point
**			dln	   - distance from 1-st point to 2-nd point
**          bound  - machine tolerance surface boundary structure
**          bound1 - tight tolerance surface boundary structure
**			pl	   - plane used to intersect with boundary cvpts
**          tol    - tolerance
**       OUTPUT :
**          iobuf  - output buffer with intersection points (list)
**    RETURNS      :  number of intersection points in iobuf 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isect_cls_bndry0 (uv1,uv2,pt1,pt2,vln,dln,bound,bound1,pl,tol,iobuf)
UM_coord uv1, uv2;
UM_coord pt1, pt2;
UM_2Dcoord vln;
UM_srf_boundary *bound,*bound1;
struct NCL_nclpl_rec *pl;
UU_REAL dln,tol;
UU_LIST *iobuf;
{
	int i,ix,ix1,ni,n,n1;
	int um_discmp();       /* sort condition routine */
	UM_cvio *bptd;
	UU_REAL (*xmm)[2],(*ymm)[2];
	UU_REAL bxx[2],bxy[2];
	UU_REAL tmp,btol;
	UM_coord ppp,ptp;
	UM_coord *pts,*uvs,*uvs1,*pts1;

	uvs = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
	btol = MAX2 (0.1 * tol, 1.e-5);
/*
.....tight tolerance boundary points
*/
	uvs1 = (UM_coord *) UU_LIST_ARRAY (bound1->uvpts);
	pts1 = (UM_coord *) UU_LIST_ARRAY (bound1->cvpts);

/*
.....get box for the line to intersect
*/
	bxx[0] = MIN2 (uv1[0],uv2[0]);
	bxx[1] = MAX2 (uv1[0],uv2[0]);
	bxy[0] = MIN2 (uv1[1],uv2[1]);
	bxy[1] = MAX2 (uv1[1],uv2[1]);
/*
.....intersect segment with all closed boundaries
*/ 
	ni = 0;
	xmm = bound->ummx; ymm = bound->vmmx;
	for (i = 0,ix = 0,ix1 = 0; i < bound->nb; i++)
	{
		n = bound->np[i];
		n1 = bound1->np[i];

		if (um_isect_boxes (bxx,bxy,xmm[i],ymm[i],btol) == 1)
		{
			ni += um_isect_cls_bndry2 (uv1,uv2,pt1,pt2,vln,dln,pl,
						uvs[ix],pts[ix],n,uvs1[ix1],pts1[ix1],n1,btol,iobuf);
		}
		ix += n;
		ix1 += n1;
	}
	if (ni < 2) return (ni);
/*
.....sort all points in buffer by distance from uv1,pt1
*/
	bptd = (UM_cvio *) UU_LIST_ARRAY (iobuf);
	if (ni >= 3)
		uu_qsort (bptd,ni,sizeof(UM_cvio),um_discmp2);
	else if (ni == 2 && bptd[0].uv.param > bptd[1].uv.param)
	{
		tmp = bptd[0].uv.param;
		um_vctovc (bptd[0].uv.point,ppp);
		um_vctovc (bptd[0].pt,ptp);

		bptd[0].uv.param = bptd[1].uv.param; 
		um_vctovc (bptd[1].uv.point, bptd[0].uv.point);
		um_vctovc (bptd[1].pt, bptd[0].pt);
		bptd[1].uv.param = tmp; 
		um_vctovc (ppp,bptd[1].uv.point);
		um_vctovc (ptp,bptd[1].pt);
	}
/*
.....weed out points
*/
	for (i = 1; i < ni; i++)
	{
		if (bptd[i].uv.param - bptd[i-1].uv.param < btol)
		{
			uu_list_delete (iobuf, i, 1);
			ni--; i--;
		}
	}

	return (ni);
}

/*********************************************************************
**    E_FUNCTION:  um_isect_cls_bndry2 (uv1,uv2,pt1,pt2,vln,dln,pl,
**										uvs,pts,npt,tol,iobuf)
**          Intersects single boundary closed curve represented by
**          polyline with line (represented by two points). 
**          Both line & polyline are used to check intersections
**			in u,v space (2D), once found, the actual intersection 
**			points are calculated in 3D(ployline with plane)
**    PARAMETERS
**       INPUT  :
**          uv1    - 1-st uv point of the line
**          uv2    - 2-nd uv point of the line
**          pt1    - 1-st point of the line
**          pt2    - 2-nd point of the line
**			vln	   - vector from 1-st point to 2-nd point
**			dln	   - distance from 1-st point to 2-nd point
**			uvs    - array of boundary uv-points
**          pts    - array of boundary points
**          npt    - number of points in pts array
**			uvs1   - array of boundary uv-points(tight tolerance)
**          pts1   - array of boundary points of(tight tolerance)
**          npt1   - number of points in pts1 array(tight tolerance)
**          tol    - tolerance
**       OUTPUT :
**          iobuf  - output buffer with intersection points (list)
**    RETURNS      : number of points in iobuf 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isect_cls_bndry2 (uv1,uv2,pt1,pt2,vln,dln,pl,
						 uvs,pts,npt,uvs1,pts1,npt1,tol,iobuf)
UM_coord uv1,uv2,uvs[],uvs1[];
UM_coord pt1,pt2,pts[],pts1[];
UM_2Dcoord vln;
struct NCL_nclpl_rec *pl;
UU_REAL dln,tol;
int npt,npt1;
UU_LIST *iobuf;
{
	int nn,i,np,ipt,ipt0,ipt1,imax;
	UM_pointd ptd;
	UM_cvio cio;
	UM_coord pt12;
	UU_REAL pld,d1,d2;
	UM_vector vc1;
	UM_int2 type = NCLI_POINT;

	pld = UM_DOT(pl->pt,pl->nvec);

	ipt = 0;
	ipt0 = iobuf->cur_cnt;
	np = npt - 1;

/*
.....First check if any boundary point is on the segment; add it if so:
.....this is the reason to pass tol to this routine, since [pt1,pt2]
.....could overlap with a boundary segment up to tolerance, but not exactly;
.....and um_isect_segs tries to find the exact intersection.
*/
		for (i=0; i<np; i++)
		{
			nn = um_ptinsg (uv1,vln,dln,uvs[i],0.1*tol,&ptd.param);
			if (nn == 1)
			{
				ipt++;
				um_vctovc (pts[i],pt12);
				pt12[2] = pld;
			
				cio.uv.param = ptd.param;
				um_vctovc (uvs[i],cio.uv.point);				
				um_vctovc (pt12,cio.pt);
				uu_list_push (iobuf,&cio);
			}
		}
		ipt1 = ipt;

		for (i=1; i<npt; i++)
		{
/*
.....Get segment box & check for intersection with line box
*/
			nn = um_isect_segs (uv1,vln,dln,uvs[i-1],uvs[i],tol,&ptd);
			if (nn > 0)
			{
/*
.....If some boundary points have been added - do not add points within tol 
.....to a boundary point
*/
				if (ipt1 > ipt0)
				{
					int j;
					UM_cvio *bptd = (UM_cvio *) UU_LIST_ARRAY (iobuf);
					for (j = ipt0; j < ipt1 && nn > 0; j++)
					{
						if (fabs(bptd[j].uv.param - ptd.param) < tol) 					
							nn = 0;
					}
				}
				if (nn > 0)
				{
					cio.uv.param = ptd.param;
    				um_vctovc (ptd.point,cio.uv.point);	
				}

/*
.....Get intersection between plane and line pts[i-1] and pts[i] 
*/
				if (nn > 0)
				{
					if (ptd.param >= 1.e-6 && dln - ptd.param >= 1.e-6)
					{
/*
.....Get the colsest index of uvs1 from ptd.point
*/
						int n1 = 0;
						int i1 = i;
						i1 = um_nearest_crv (i,npt,ptd.point,uvs1,npt1);
						if (i1 == npt1-1)
							i1 = 0;
						imax = npt1; if (imax > 40) imax = 40;
						n1 = um_isect_pln_bndry(i1+1,pld,pl,
										pts1,npt1,imax,tol,pt12);
						if (n1 > 0)
						{
							um_vctovc (pt12,cio.pt);			
							uu_list_push (iobuf,&cio);
							ipt++;
						}
					}
					else
					{ 					
						if (fabs(ptd.param) < 1.e-6 || 
							fabs(dln - ptd.param) < 1.e-6)
						{
							if (fabs(ptd.param) < 1.e-6)						
								fr_unbs (pt1,pt12,&type);
							else if (fabs(dln - ptd.param) < 1.e-6)														
								fr_unbs (pt2,pt12,&type);										

							pt12[2] = pld;
							um_vctovc (pt12,cio.pt);			
							uu_list_push (iobuf,&cio);
							ipt++;
						}
					}
				}
			}
		}

	return (ipt);
}  

/*********************************************************************
**    E_FUNCTION:  um_isect_pln_bndry0(ipt,pld,pl,pts,npt,tol,plpt)
**          Intersects single boundary closed curve represented by
**          polyline with line (represented by two points). 
**          Both line & polyline are used to check intersections
**			in u,v space (2D), once found, the actual intersection 
**			points are calculated in 3D(ployline with plane) 
**    PARAMETERS
**       INPUT  :
**          ipt    - the i th number of point to start with
**          pld    - plane distance
**          pl     - plane
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**          tol    - tolerance
**       OUTPUT :
**          plpt   - output the intersection point
**    RETURNS      :  number of intersection points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isect_pln_bndry0(ipt,pld,pl,pts,npt,tol,plpt)
int ipt,npt;
UM_coord pts[];
struct NCL_nclpl_rec *pl;
UU_REAL pld,tol;
UM_coord plpt;
{
	UU_REAL d1,d2;
	UM_vector vc1;
	int n1 = 0;
	int ipt1,ipt2;

	ipt1 = ipt;
	ipt2 = ipt;
	if (ipt <= 0)
	{
		ipt1 = ipt + npt;
		if (ipt < 0)
			ipt2 = ipt + npt;
	}
	else if (ipt >= npt)
	{
		ipt1 = ipt - (npt-1) +1;
		if (ipt > npt-1)
			ipt2 = ipt - npt;
	}
		
	d1 = UM_DOT(pts[ipt1-1],pl->nvec) - pld;
	d2 = UM_DOT(pts[ipt2],pl->nvec) - pld;
	if (fabs (d1) < tol)
	{
		um_vctovc (pts[ipt1-1],plpt);
		n1 = 1;
	}
	else if (fabs (d2) < tol)
	{
		um_vctovc (pts[ipt2],plpt);
		n1 = 1;
	}
	else if (d1 * d2 < 0.0)
	{
		um_vcmnvc(pts[ipt2],pts[ipt1-1],vc1);
		um_unitvc(vc1,vc1);
		um_ilnpln(pts[ipt1-1],vc1,pl->pt,pl->nvec,&n1,plpt);
	}

	return n1;
}

/*********************************************************************
**    E_FUNCTION:  um_isect_pln_bndry(ipt,pld,pl,pts,npt,imax,tol,plpt)
**          Intersects single boundary closed curve represented by
**          polyline with plane. 
**          Both line & polyline are used to check intersections
**			in u,v space (2D), once found, the actual intersection 
**			points are calculated in 3D(ployline with plane)
**    PARAMETERS
**       INPUT  :
**          ipt    - the ipt th number of point to start with
**          pld    - plane distance
**          pl     - plane
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**          imax   - number of points to search for intersection point.
**          tol    - tolerance
*
**       OUTPUT :
**          plpt   - output the nearest intersection point
**    RETURNS      :  number of intersection points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isect_pln_bndry(ipt,pld,pl,pts,npt,imax,tol,plpt)
int ipt, npt, imax;
UM_coord pts[];
struct NCL_nclpl_rec *pl;
UU_REAL pld,tol;
UM_coord plpt;
{
	int i;
	int n1 = 0;

	n1 = um_isect_pln_bndry0(ipt,pld,pl,pts,npt,tol,plpt);
	if (n1 > 0 )
		return n1;

	for (i = 1 ; i < imax; i++)
	{
		n1  = um_isect_pln_bndry0(ipt-i,pld,pl,pts,npt,tol,plpt);
		if (n1 > 0)
			return n1;
		
		n1  = um_isect_pln_bndry0(ipt+i,pld,pl,pts,npt,tol,plpt);		
		if (n1 > 0)
			return n1;
	}

	return n1;
}

/*********************************************************************
**    E_FUNCTION: um_point_on_between_line(pt,pt1,pt2,tol)
**       Check if point pt is between  
**    PARAMETERS
**       INPUT  :
**			pt		- point to check
**          pt1     - the first point of line
**          pt2     - the second point of line
**			tol		- tolerance
**       OUTPUT :
**    RETURNS      :  
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_point_on_between_line(pt,pt1,pt2,tol)				
UM_coord pt,pt1,pt2;
UU_REAL tol;
{
	int status;
	UM_vector vc,ulvc;
	UM_coord  pt0,nlpt;
	UU_REAL sqdist1,sqdist2,sqlen,sqdist;

	status = UU_FAILURE;

	um_vcmnvc(pt2,pt1,vc);
	um_unitvc(vc,ulvc);
		
	um_vctovc(pt,pt0);
	pt0[2] = 0.0;

	um_nptln(pt0,pt1,ulvc,nlpt);

	sqdist = um_sqdis_2d(pt0,nlpt);
	if (sqdist < 2.0 * tol * tol)
		return UU_SUCCESS;

/*
.....Check if projected point is between pts[i] and pts[i+1]
*/
	sqlen = um_sqdis_2d(pt1,pt2);
	sqdist1 = um_sqdis_2d(nlpt,pt1);
	sqdist2 = um_sqdis_2d(nlpt,pt2);
	if (sqdist1 > sqlen + tol * tol || sqdist2 > sqlen + tol * tol) 
		return UU_SUCCESS;

	return status;
}

/*********************************************************************
**    E_FUNCTION: um_point_on_between_triangle(pt,pt1,pt2,pt3,tol)
**       Check if point pt is between  
**    PARAMETERS
**       INPUT  :
**			pt		- point to check
**          pt1     - the first point of triangle
**          pt2     - the second point of triangle
**          pt3     - the third point of triangle
**			tol		- tolerance
**       OUTPUT :
**    RETURNS      :  
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_point_on_between_triangle(pt,pt1,pt2,pt3,tol)				
UM_coord pt,pt1,pt2,pt3;
UU_REAL tol;
{
	int status;
	status = UU_FAILURE;
	status = um_point_on_between_line(pt,pt1,pt2,tol);
	if (status == UU_SUCCESS)
		return status;
	
	status = um_point_on_between_line(pt,pt2,pt3,tol);
	if (status == UU_SUCCESS)
		return status;
	
	status = um_point_on_between_line(pt,pt3,pt1,tol);
	if (status == UU_SUCCESS)
		return status;

	return status;
}
