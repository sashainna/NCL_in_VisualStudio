/*********************************************************************
**    NAME         :  newatstk.c
**       CONTAINS:  Routines for calculating waterline geometry
** 
**        ncl_offset_out
**        ncl_offset_out0
**        ncl_shuffle
**        ncl_test_projection
**        ncl_create_contour_stock
**        ncl_create_box_stock
**        ncl_create_2box
**        ncl_create_boxgeo
**
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       newatstk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:01
*********************************************************************/
#include "mgeom.h"
#include "uminmax.h"
#include "nclwaterln.h"
#include "nclclip.h"

#define MINARC 12

extern UU_LOGICAL UN_clip_debug;
extern UU_KEY_ID UN_sfkey;
void ncl_prepare_for_pocketing ();

/*********************************************************************
**    E_FUNCTION     : ncl_offset_out0 (ptlist,pgn0,dis,tol)
**       Expand a 2D-polygon by dis, that is determine the offset direction 
**       which would make the polygon larger, and call the offset routine
**    PARAMETERS
**       INPUT  : 
**                pgn0     - the original contour polygon
**                dis      - the offset distance
**                tol      - tolerance
**       OUTPUT :
**                ptlist      - the offset polygon
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_offset_out0 (ptlist,pgn0,tangs,dis,tol)
UU_LIST **ptlist,*tangs;
ncl_polygon *pgn0;
UU_REAL dis;
UU_REAL tol;
{
	int i,npts,n1,lr;
	UM_coord *pp,pti;
	UM_vector dm;
	UU_REAL eps = 4.*tol*tol;
	UM_2Dcoord *vtx;

	if (pgn0)
	{
		npts = pgn0->contour->cur_cnt;
		if (npts < 3) return (UU_FAILURE);

		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pgn0->contour);

		i = MAX2(npts+1,100);
		(*ptlist) = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		uu_list_init (*ptlist, sizeof(UM_coord), i, i);

		pti[2] = 0.;
		for (i = 0; i < npts; i++)
		{
			pti[0] = vtx[i][0];
			pti[1] = vtx[i][1];
			uu_list_push (*ptlist,pti);
			if (tangs && i < npts-1)
			{
				pti[0] = vtx[i+1][0] - vtx[i][0];
				pti[1] = vtx[i+1][1] - vtx[i][1];
				uu_list_push (tangs,pti);
			}
		}
	}

	if (!(*ptlist)) return (UU_FAILURE);
/*
..... close the ptlist loop, if not closed already
*/
	pp = (UM_coord *) UU_LIST_ARRAY (*ptlist);
	npts = (*ptlist)->cur_cnt;

	if (UM_SQDIS_2D(pp[0],pp[npts-1]) > eps)
	{
		pti[0] = pp[0][0]; pti[1] = pp[0][1];
		uu_list_push (*ptlist,pti);
		npts = (*ptlist)->cur_cnt;
		if (tangs)
		{
			pp = (UM_coord *) UU_LIST_ARRAY (*ptlist);
			pti[0] = pp[npts-1][0] - pp[npts-2][0];
			pti[1] = pp[npts-1][1] - pp[npts-2][1];
			uu_list_push (tangs,pti);
			uu_list_push (tangs,pti);
		}
	}
	else
	{
		pp[npts-1][0] = pp[0][0]; pp[npts-1][1] = pp[0][1];
		if (tangs)
		{
			pti[0] = pp[npts-1][0] - pp[npts-2][0];
			pti[1] = pp[npts-1][1] - pp[npts-2][1];
			uu_list_push (tangs,pti);
		}
	}

	if (npts < 4) return (UU_FAILURE);
	if (fabs(dis) < tol)
	{
		if (tangs)
		{
			n1 = npts;
			ncl_fix_corner0 (*ptlist,tangs,tol,&n1);
			ncl_fix_corners (*ptlist,tangs,tol,0,&n1);

			ncl_fix_tol (&n1,tol,*ptlist,tangs);
		}
		npts = (*ptlist)->cur_cnt;
	}
	else
	{
		n1 = ncl_out_dm (*ptlist,(UU_LIST *)UU_NULL,dm,&lr);
		if (n1 == UU_SUCCESS)
		{
			if (tangs)
			{
				pti[0] = pti[1] = 0;
				pti[2] = 1;
					
				n1 = ncl_cv_offset (NULLKEY,*ptlist,tangs,npts,lr,pti,dis,0,0.,0.,
					tol,0,0);
				ncl_fix_corners (*ptlist,tangs,tol,0,&n1);

				ncl_fix_tol (&n1,tol,*ptlist,tangs);
			}
			else
				n1 = ncl_offset_contour0 (tol,npts,dm,dis,*ptlist);
			npts = (*ptlist)->cur_cnt;
		}
		if (n1 < 0 || npts < 4) return (UU_FAILURE);
		if (n1 == 4 || npts == 4)
		{
			pp = (UM_coord *) UU_LIST_ARRAY (*ptlist);
/*
.....Equivalent to npts < 4 or n1 < 4 - Andrew 3/21/13
*/
			if (um_dcccc(pp[0],pp[3]) < UM_DFUZZ) return(UU_FAILURE);
		}
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_fix_arcs (stk,tol)
**     Fit the offset contour point on the original contour arcs.
**    PARAMETERS   
**       INPUT  :
**                   stk  - list of points 
**                   tol  - tolerance
**       OUTPUT : 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_fix_arcs (stk,tol)
UU_LIST *stk;
UU_REAL tol;
{
	int i,is,nv;
	UM_2Dcoord *vtx;
	NCL_w_arc *circ,*prev,*head0;
	UU_REAL d;
	UM_2Dcoord v0,v1;
	UU_REAL dang,um_angle_2d();

	ncl_get_whead (0,&head0);
	if (head0 == UU_NULL) return;

	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (stk);
	nv = stk->cur_cnt;

	circ = head0; prev = UU_NULL;
	is = 0;
	while (circ)
	{
		for (i = is; i < nv; i++)
		{
			d = UM_DIST_2D(vtx[i],circ->center);
			if (fabs(d - circ->rad) < tol)
			{
				if (circ->j0 < 0)
				{
					circ->j0 = i;
					um_vcmnvc_2d (vtx[circ->j0],circ->center,v0);
				}
			}
			else if (circ->j0 >= 0 && circ->j1 < 0)
			{
				if (i - circ->j0 <= 1) /* contour just crossed the circle */
				{
					circ->j0 = -1;
					continue;
				}
				else
				{
					if (i - circ->j0 >= MINARC)
					{
						um_vcmnvc_2d (vtx[i-1],circ->center,v1);
						dang = um_angle_2d (v0,v1);
						if (dang >= 0.2618)
							is = circ->j1 = i-1;
					}
					break;
				}
			}
		}
		if (circ->j1 < 0)
		{
			if (circ == head0)
			{
				head0 = circ->next;
				ncl_set_whead (0,head0);
				UU_FREE(circ);
				circ = head0;
			}
			else
			{
				prev->next = circ->next;
				UU_FREE(circ);
				circ = prev->next;
			}
		}
		else
		{
			prev = circ; circ = circ->next;
		}
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_shuffle (cvpoint,cvtang,npts,dtolsq)
**       Like in pocketing, make the contour start and end at the
**       middle of the longest segment before offsetting.
**       This way, the endpoints will match after offsetting (we hope).
**    PARAMETERS
**       INPUT  : 
**                cvpoint     - the points list
**                cvtang      - the tangencies
**                npts        - number of points
**                dtolsq      - squared tolerance
**       OUTPUT :
**                cvpoint     - shuffled points list
**                cvtang      - shuffled tangencies
**                npts        - new number of points
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_shuffle (cvpoint,cvtang,npts,dtolsq)
UU_LIST *cvpoint,*cvtang;
int npts;
UU_REAL dtolsq;
{
	int i,imax,j,n0,n1;
	UU_REAL d,dmax;
	UM_coord *pp,*ctmp;
	UM_coord pti;
	UM_vector vci;

	npts = cvpoint->cur_cnt;
	pp = (UM_coord *) UU_LIST_ARRAY (cvpoint);

	imax = -1;
	dmax = dtolsq;

	for (i = 0; i < npts-2; i++)
	{
		d = UM_SQDIS_2D(pp[i],pp[i+1]);
		if (d > dmax)
		{
			dmax = d; imax = i;
		}
	}

	if (imax >= 0)
	{
		n0 = npts - 1; n1 = npts + 1;

		ctmp = (UM_coord *) uu_malloc (n1*sizeof(UM_coord));

		for (j = 0; j < 2; j++)
		{
			pti[j] = (pp[imax][j] + pp[imax+1][j])/2;
			vci[j] = (pp[imax+1][j] - pp[imax][j])/2;
		}

		pti[2] = vci[2] = 0;

		um_vctovc (pti,ctmp[0]);
		um_vctovc (pti,&ctmp[npts]);

		for (i = 1; i < npts; i++)
		{
			j = (imax + i) % n0;
			um_vctovc (&pp[j],&ctmp[i]);
		}

		cvpoint->cur_cnt = 0;
		uu_list_push_multiple (cvpoint,n1,ctmp);

		if (cvtang != NULLST)
		{
			um_vctovc (vci,ctmp[0]);
			um_vctovc (vci,&ctmp[npts]);
			pp = (UM_coord *) UU_LIST_ARRAY (cvtang);

			for (i = 1; i < npts; i++)
			{
				j = (imax + i) % n0;
				um_vctovc (&pp[j],&ctmp[i]);
			}

			cvtang->cur_cnt = 0;
			uu_list_push_multiple (cvtang,n1,ctmp);
		}

		uu_free (ctmp); 
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_offset_out (stk,pgn0,cvpoint,dis,tol,tolsq)
**       Expand a 2D-polygon by dis, that is determine the offset direction
**       which would make the polygon larger, and call the offset routine
**    PARAMETERS
**       INPUT  : 
**                pgn0     - the polygon contour (closed)
**                dis      - the offset distance
**                tol      - tolerance
**       OUTPUT :
**                ptlist      - the offset polygon
**                a0,b0,a1,b1 - bounding box for the offset polygon
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_offset_out (stk,pgn0,cvpoint,dis,tol,tolsq)
UU_LIST *pgn0,*stk,*cvpoint;
UU_REAL dis;
UU_REAL tol,tolsq;
{
	int i,npts,n1,lr;
	UM_coord *pp,pti;
	UM_vector vvi;
	UM_vector dm;
	UU_REAL d;
	UM_2Dcoord *vtx;
	UU_LIST cvtang;
	NCL_w_arc *circ,*prev,*head0;

	npts = pgn0->cur_cnt;
	if (npts < 4) return (UU_FAILURE);

	ncl_get_whead (0,&head0);
	if (head0 != UU_NULL && head0->ccw == 0) /* full circle */
	{
		uu_list_init (stk, sizeof(UM_2Dcoord), 0, npts);
		head0->rad += dis;
		return (0);
	}

	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pgn0);
	if (UM_SQDIS_2D(vtx[0],vtx[npts-1]) > tolsq)
	{
		pti[0] = vtx[0][0];
		pti[1] = vtx[0][1];
		uu_list_push (pgn0,pti);
		npts++;
		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pgn0);
	}
	else
	{
		vtx[npts-1][0] = vtx[0][0];
		vtx[npts-1][1] = vtx[0][1];
	}
	if (dis < tol) /* no offset */
	{
		uu_list_init (stk, sizeof(UM_2Dcoord), npts, npts);
		uu_list_push_multiple (stk,npts,vtx);
		return (0);
	}

	cvpoint->cur_cnt = 0;
	uu_list_init (&cvtang, sizeof(UM_vector), npts, npts);

	pti[2] = vvi[2] = 0.;
	for (i = 0, circ = head0; i < npts-1; i++)
	{
		if (circ && i == circ->j0)
		{
/*
..... put points exactly on the circle, calculate correct arc tangencies
*/
			while (i < npts-1)
			{
				vvi[0] = vtx[i][0] - circ->center[0];
				vvi[1] = vtx[i][1] - circ->center[1];

				d = circ->rad / UM_DIST_2D(vtx[i],circ->center);
				vvi[0] *= d; vvi[1] *= d;
				pti[0] = circ->center[0] + vvi[0];
				pti[1] = circ->center[1] + vvi[1];
				uu_list_push (cvpoint,pti);
	
				if ((i+1) > circ->j1)
				{
					vvi[0] = vtx[i+1][0] - pti[0];
					vvi[1] = vtx[i+1][1] - pti[1];
					uu_list_push (&cvtang,vvi);
					break;
				}
				d = vvi[0];
				vvi[0] = -(circ->ccw)*vvi[1];
				vvi[1] = (circ->ccw)*d;
				uu_list_push (&cvtang,vvi);
				i++;
			}
			circ = circ->next;
		}
		else
		{
			pti[0] = vtx[i][0];
			pti[1] = vtx[i][1];
			uu_list_push (cvpoint,pti);
			vvi[0] = vtx[i+1][0] - pti[0];
			vvi[1] = vtx[i+1][1] - pti[1];
			uu_list_push (&cvtang,vvi);
		}
	}
/*
..... add last point - same as the first
*/
	pti[0] = vtx[0][0];
	pti[1] = vtx[0][1];
	uu_list_push (cvpoint,pti);
/*
..... add last tangent vector - same as the one before last
*/
	uu_list_push (&cvtang,vvi);

	n1 = ncl_out_dm (cvpoint,&cvtang,dm,&lr);

	if (n1 == UU_SUCCESS)
	{
		circ = head0; prev = UU_NULL;
		while (circ)
		{
			circ->rad -= (circ->ccw)*lr*dis;
			if (circ->rad < 8*tol)
			{
				if (circ == head0)
				{
					head0 = circ->next;
					ncl_set_whead (0,head0);
					UU_FREE(circ);
					circ = head0;
				}
				else
				{
					prev->next = circ->next;
					UU_FREE(circ);
					circ = prev->next;
				}
			}
			else
			{
				circ->j0 = circ->j1 = -1;
				prev = circ; circ = circ->next;
			}
		}
			
		if (!ncl_setver(94))
		{
			ncl_shuffle (cvpoint,&cvtang,npts,10000*tolsq);
			npts = cvpoint->cur_cnt;
		}

		n1 = ncl_offset_contour (cvpoint,&cvtang,npts,lr,dis,tol,tolsq);
	}
	uu_list_free (&cvtang);
	if (n1 < 4) return (-1);

	npts = cvpoint->cur_cnt;
	pp = (UM_coord *) UU_LIST_ARRAY (cvpoint);

	uu_list_init (stk, sizeof(UM_2Dcoord), npts, npts);
	for (i = 0; i < npts; i++)
	{
		pti[0] = pp[i][0]; pti[1] = pp[i][1];
		uu_list_push(stk,pti);
	}

	if (head0 != UU_NULL) ncl_fix_arcs (stk,tol);

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : S_vert_pln (npt,pts,vtxlist,vy,tol)
**       Project a vertical closed polyline onto horizontal line, then
**       offset the result by tol to make a thin rectangle
**    PARAMETERS
**       INPUT : 
**                npt      - number of polyline points
**                pts      - polyline points
**                tol      - offset parameter
**       OUTPUT:
**                vtxlist  - resulting horizontal polygon
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_vert_pln (npt,pts,vtxlist,vy,wmin,wmax,tol)
int npt;
UM_coord *pts;
UU_LIST *vtxlist;
UM_2Dcoord vy;
UU_REAL wmin[],wmax[],tol;
{
	UM_2Dcoord vti,vta,vx;
	UU_REAL x0,x1,y0,y1,dx,dy,x,y,xa,ya;
	int i,j,k;

/*
..... First, project the boundary to a horizontal line.
*/
	vx[0] = -vy[1]; vx[1] = vy[0];

	x0 = x1 = UM_DOT_2D (vx,pts[0]);
	y0 = y1 = UM_DOT_2D (vy,pts[0]);

	for (i = 1; i < npt; i++)
	{
		x = UM_DOT_2D (vx,pts[i]);
		if (x < x0) x0 = x;
		if (x > x1) x1 = x;

		y = UM_DOT_2D (vy,pts[i]);
		if (y < y0) y0 = y;
		if (y > y1) y1 = y;
	}

	dx = x1 - x0;
	dy = y1 - y0;

	if (dy > 4.*tol) return;

	xa = (x0 + x1) / 2;
	ya = (y0 + y1) / 2;

	for (k = 0; k < 2; k++)
		vta[k] = xa*vx[k] + ya*vy[k];

	dy = tol;
	dx = dx/2 + tol;

	for (i = 0; i < 2; i++)
	{
		x = (1-2*i)*dx;
		for (j = 0; j < 2; j++)
		{
			y = (1-2*i)*(2*j-1)*dy;

			for (k = 0; k < 2; k++)
			{
				vti[k] = vta[k] + x*vx[k] + y*vy[k];
				if (vti[k] < wmin[k]) vti[k] = wmin[k];
				if (vti[k] > wmax[k]) vti[k] = wmax[k];
			}

			uu_list_push (vtxlist,vti);
		}
	}
}

#if 0
/*********************************************************************
**    E_FUNCTION     : ncl_vert_bndry (npt,pts,vtxlist,tol)
**       Project a vertical closed polyline onto horizontal plane, then
**       offset the result by tol to make a thin polygon
**    PARAMETERS
**       INPUT : 
**                npt      - number of polyline points
**                pts      - polyline points
**                tol      - offset parameter
**       OUTPUT:
**                vtxlist  - resulting horizontal polygon
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_vert_bndry (npt,pts,vtxlist,tol)
int npt;
UM_coord *pts;
UU_LIST *vtxlist;
UU_REAL tol;
{
	UU_LOGICAL forward = UU_FALSE;
	UM_2Dcoord vi,p0,p1,vp,v0,v1;
	UU_REAL di;
	UM_2Dcoord *vpts;
	int i,nn;

/*
..... First, project the polyline to a simple horizontal curve.
*/
	for (i = 0; i < npt; i++)
	{
		vi[0] = pts[i+1][0] - pts[i][0];
		vi[1] = pts[i+1][1] - pts[i][1];
		di = UM_MAG_2D(vi);
/*
..... find first segment projecting to a "sizable" horizontal segment
*/
		if (di > tol)
		{
			forward = UU_TRUE;
			p0[0] = pts[i][0]; p0[1] = pts[i][1];
			p1[0] = pts[i+1][0]; p1[1] = pts[i+1][1];
			vi[0] /= di; vi[1] /= di;
			vp[0] = v0[0] = v1[0] = vi[0]; 
			vp[1] = v0[1] = v1[1] = vi[1];
			break;
		}
	}
	if (!forward) return (UU_FAILURE);
	uu_list_push (vtxlist,pts[i]);
	uu_list_push (vtxlist,pts[i+1]);
	i++;
/*
..... p0 is the first curve point, p1 is the last one. for a new point we
..... either add it after p1, or insert it before p0, or do nothing,
..... depending on whether the new segment is sticking out the current curve
*/
	for (; i < npt-1; i++)
	{
		vi[0] = pts[i+1][0] - pts[i][0];
		vi[1] = pts[i+1][1] - pts[i][1];
		di = UM_MAG_2D(vi);
		if (di <= tol) continue;
		vi[0] /= di; vi[1] /= di;
		di = UM_DOT_2D(vi,vp);
		if (di < -0.985) forward = !forward;
		vp[0] = vi[0]; vp[1] = vi[1];
		if (forward)
		{
			vi[0] = pts[i+1][0] - p1[0];
			vi[1] = pts[i+1][1] - p1[1];
			di = UM_DOT_2D(vi,v1);
			if (di > tol)
			{
				uu_list_push (vtxlist,pts[i+1]);
				p1[0] = pts[i+1][0]; p1[1] = pts[i+1][1];
				um_unitvc_2d (vi, v1);
			}
		}
		else
		{
			vi[0] = p0[0] - pts[i+1][0];
			vi[1] = p0[1] - pts[i+1][1];
			di = UM_DOT_2D(vi,v0);
			if (di > tol)
			{
				uu_list_insert (vtxlist,0,pts[i+1]);
				p0[0] = pts[i+1][0]; p0[1] = pts[i+1][1];
				um_unitvc_2d (vi, v1);
			}
		}
	}
/*
..... offset the projection curve by tol to get a simple polygon
*/
	nn = vtxlist->cur_cnt;
	vpts = (UM_2Dcoord *) UU_LIST_ARRAY (vtxlist);
	for (i = 1; i < nn; i++)
	{
		vi[0] = vpts[nn-i-1][0] - vpts[nn-i][0];
		vi[1] = vpts[nn-i-1][1] - vpts[nn-i][1];
		di = UM_MAG_2D(vi);
		if (di < UM_FUZZ) return (UU_FAILURE);
		di = tol/di;
		p0[0] = vpts[nn-i][0] + di*vi[1];
		p0[1] = vpts[nn-i][1] - di*vi[0];
		uu_list_push (vtxlist,p0);
	}
	p0[0] = vpts[0][0] + di*vi[1];
	p0[1] = vpts[0][1] - di*vi[0];
	uu_list_push (vtxlist,p0);

	return (0);
}
#endif

/*********************************************************************
**    E_FUNCTION     : ncl_test_projection (sfflag,npt,pts,tol)
**       Determine if the horizontal projection of a closed polyline 
**       is self-intersecting.
**    PARAMETERS
**       INPUT : 
**                npt      - number of polyline points
**                pts      - polyline points
**       OUTPUT:
**                sfflag  - BADPROJ iff the projection self-intersects
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_test_projection (sfflag,npt,pts)
ncl_waterline_sftype *sfflag;
int npt;
UM_coord *pts;
{
	int i,k;

	for (k = 1; k < npt-2; k++)
	{
		if (um_segments_isect(pts[npt-1],pts[0],pts[k],pts[k+1]))
		{
			*sfflag = BADPROJ; return;
		}
	}
	for (i = 0; i < npt-2; i++)
	{
		for (k = i+2; k < npt-1; k++)
		{
			if (um_segments_isect(pts[i],pts[i+1],pts[k],pts[k+1]))
			{
				*sfflag = BADPROJ; return;
			}
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_test_projection1 (sfflag,npt,pts,tol)
**       Determine if the projection of a surface boundary on the base surface 
**       is self-intersecting.
**    PARAMETERS
**       INPUT : 
**                npt      - number of polyline points
**                pts      - polyline points in UVH coordinates
**                uvlst    - empty UM_coord list to use
**       OUTPUT:
**                sfflag  - BADPROJ iff the projection self-intersects
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_test_projection1 (sfflag,npt,pts,uvlst)
ncl_waterline_sftype *sfflag;
int npt;
UM_coord *pts;
UU_LIST *uvlst;
{
	int i,nvt;
	UM_coord vt0,vt1,*vts;
	UU_REAL d;
	UU_REAL eps = UM_DFUZZ;

	vt0[0] = pts[0][0]; vt0[1] = pts[0][1];
	vt0[2] = vt1[2] = 0;
	uu_list_push (uvlst,vt0);

	for (i = 0; i < npt-1; i++)
	{
		vt1[0] = pts[i+1][0]; vt1[1] = pts[i+1][1];
		d = UM_SQDIS_2D (vt0,vt1);
		if (d > eps)
		{
			uu_list_push (uvlst,vt1);
			vt0[0] = vt1[0]; vt0[1] = vt1[1];
		}
	}

	vts = (UM_coord *) UU_LIST_ARRAY (uvlst);
	nvt = uvlst->cur_cnt;

	vt1[0] = pts[npt][0]; vt1[1] = pts[npt][1];
	d = UM_SQDIS_2D (vt0,vt1);
	if (d <= eps) nvt--;

	ncl_test_projection (sfflag,nvt,vts);

	return;
}

/**************************************************************************
**************************************************************************/
static void S_init_box (p2,x0,x1,y0,y1)
UM_2Dcoord p2;
UU_REAL *x0,*x1,*y0,*y1;
{
	*x0 = *x1 = p2[0];
	*y0 = *y1 = p2[1];
}

/**************************************************************************
**************************************************************************/
void ncl_update_minmax (p2,x0,x1,y0,y1)
UM_2Dcoord p2;
UU_REAL *x0,*x1,*y0,*y1;
{
	if (p2[0] < *x0) *x0 = p2[0];
	if (p2[0] > *x1) *x1 = p2[0];

	if (p2[1] < *y0) *y0 = p2[1];
	if (p2[1] > *y1) *y1 = p2[1];
}

/*********************************************************************
**    E_FUNCTION     : void S_push_box (sfi,vtxlist)
**       Project a surface box onto the horizontal plane
**    PARAMETERS
**       INPUT  :
**          sfi        - current surface data
**       OUTPUT :
**          vtxlist    - list of 2D points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_push_box (sfi,vtxlist)
NCL_waterline_surf *sfi;
UU_LIST *vtxlist;
{
	UU_REAL x0,x1,y0,y1;
	UM_2Dcoord pc;

	x0 = sfi->box.xmin;	y0 = sfi->box.ymin;
	x1 = sfi->box.xmax;	y1 = sfi->box.ymax;

	pc[0] = x0; pc[1] = y0;
	uu_list_push (vtxlist,pc);

	pc[0] = x1; pc[1] = y0;
	uu_list_push (vtxlist,pc);

	pc[0] = x1; pc[1] = y1;
	uu_list_push (vtxlist,pc);

	pc[0] = x0; pc[1] = y1;
	uu_list_push (vtxlist,pc);
}

/**************************************************************************
**************************************************************************/
static UU_LOGICAL S_found_corner (tri1,tri2,pc,tolsq)
UM_trian *tri1,*tri2;
UM_2Dcoord pc;
UU_REAL tolsq;
{
	if (UM_SQDIS_2D(tri1->p1,pc) < tolsq) return(UU_TRUE);
	if (UM_SQDIS_2D(tri1->p2,pc) < tolsq) return(UU_TRUE);
	if (UM_SQDIS_2D(tri1->p3,pc) < tolsq) return(UU_TRUE);

	if (UM_SQDIS_2D(tri2->p1,pc) < tolsq) return(UU_TRUE);
	if (UM_SQDIS_2D(tri2->p2,pc) < tolsq) return(UU_TRUE);
	if (UM_SQDIS_2D(tri2->p3,pc) < tolsq) return(UU_TRUE);

	return (UU_FALSE);
}

/**************************************************************************
**************************************************************************/
static UU_LOGICAL S_form_block(tri1,tri2,tolsq,tlist)
UM_trian *tri1,*tri2;
UU_LIST *tlist;
UU_REAL tolsq;
{
	UU_REAL x0,x1,y0,y1,ar;
	UM_2Dcoord pc;
	UU_LOGICAL found;

	S_init_box(tri1->p1,&x0,&x1,&y0,&y1);
	ncl_update_minmax(tri1->p2,&x0,&x1,&y0,&y1);
	ncl_update_minmax(tri1->p3,&x0,&x1,&y0,&y1);

	ncl_update_minmax(tri2->p1,&x0,&x1,&y0,&y1);
	ncl_update_minmax(tri2->p2,&x0,&x1,&y0,&y1);
	ncl_update_minmax(tri2->p3,&x0,&x1,&y0,&y1);

	pc[0] = x0; pc[1] = y0;
	found = S_found_corner (tri1,tri2,pc,tolsq);
	if (!found) return (UU_FALSE);
	uu_list_push (tlist,pc);

	pc[0] = x1; pc[1] = y0;
	found = S_found_corner (tri1,tri2,pc,tolsq);
	if (!found)
	{
		UU_LIST_EMPTY (tlist);
		return (UU_FALSE);
	}
	uu_list_push (tlist,pc);

	pc[0] = x1; pc[1] = y1;
	found = S_found_corner (tri1,tri2,pc,tolsq);
	if (!found)
	{
		UU_LIST_EMPTY (tlist);
		return (UU_FALSE);
	}
	uu_list_push (tlist,pc);

	pc[0] = x0; pc[1] = y1;
	found = S_found_corner (tri1,tri2,pc,tolsq);
	if (!found)
	{
		UU_LIST_EMPTY (tlist);
		return (UU_FALSE);
	}
	uu_list_push (tlist,pc);

	ar = (x1 - x0)*(y1 - y0);
	if (ar <= 4*tolsq) UU_LIST_EMPTY (tlist);

	return (UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_proj_tess1 (sfi,tol,tolsq,vtxlist)
**       Project a surface onto the horizontal plane using tessellation.
**    PARAMETERS
**       INPUT  :
**          sfi        - current surface data
**          tol        - tolerance
**       OUTPUT :
**          vtxlist    - list of 2D points
**    RETURNS      :
**         UU_SUCCESS iff no errors and valid projection, else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_proj_tess1 (sfi,rot,tol,tolsq,vtxlist,tpol)
NCL_waterline_surf *sfi;
UM_transf rot;
UU_REAL tol,tolsq;
UU_LIST *vtxlist;
ncl_polygon *tpol;
{
	int i,status,istat,npts,ntri;
	UM_coord *pts;
	ncl_polygon pgn0;
	UM_trian *ptri;
	UU_LOGICAL firstime = UU_TRUE;
	
	UM_srf_boundary bndr;
	struct NCL_fixed_databag sf;
	UM_transf tfmat;
	UU_LIST cvlst,uvlst;
	UU_LOGICAL lv97;
	int ib,irot;
	UU_LIST trilst,*polylst;
	UU_REAL btol,tritol;
	char sbuf[80];

	ncl_set_boundary_toler (tol);
	um_init_boundary (&bndr);
	uu_list_init (&cvlst, sizeof(UM_coord), 100, 200);
	uu_list_init (&uvlst, sizeof(UM_coord), 100, 200);
	bndr.uvpts = &uvlst;
	bndr.cvpts = &cvlst;
	uu_list_init0 (&trilst);

	lv97 = ncl_setver(97);
#if 0
	sprintf(sbuf,"key = %d",sfi->key);
	NclxDbgPstr(sbuf);
#endif
	if (!lv97 && sfi->flag1 != VSOLID)
	{
/*
.....Get the surface boundary uvpts and cvpts
*/
		sf.key = sfi->key;
		status = ncl_retrieve_data_fixed (&sf);
		if (status == UU_SUCCESS)	
			status = uc_retrieve_transf (sf.key, tfmat);

		ncl_get_rotfl (&irot);

		if (status == UU_SUCCESS)
		{
/*
.....Use boundary tolerance as 0.0005
*/
			btol = 0.0005;
			ncl_free_bndry (&bndr);
			UU_LIST_EMPTY (bndr.uvpts);
			UU_LIST_EMPTY (bndr.cvpts);
			status = ncl_get_bndry (&sf,tfmat,&bndr,btol,UU_TRUE);
			if (bndr.nb < 1) status = UU_FAILURE;
#if 0
	if (sfi->key == 2302)
	{
		ib = 0;
	}
#endif
		}

		if (irot >0)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (bndr.cvpts);
			for (ib = 0; ib < bndr.nb; ib++)
			{
				npts = bndr.np[ib];
				for (i = 0; i < npts; i++)
					um_cctmtf(pts[i],rot,pts[i]);
				pts += npts;
			}
		}
/*
.....Trim BADPROJ surface base tessellation with boundary
.....store the trimmed tesselation triangles into sfi->trianlist
..... and netessmgr structure
*/
		if (!ncl_is_watrev() && sfi->trianlist == UU_NULL)
		{
			if (!trilst.data)
				uu_list_init (&trilst,sizeof(UM_trian),200,200);
			else
				UU_LIST_EMPTY (&trilst);

/*
.....Check if trimmed tesselation polygons already saved or not
*/
			status = nclc_tessmgr_get_srf_polylst(sf.key,&polylst);
			if (status != UU_SUCCESS)
			{
/*
.....Trim base surface within boundary
*/
				status = ncl_trim_tess_bound(&sf,tfmat,&bndr,
									rot,irot,tol,&trilst);
/*
.....Store the tesselation polygons data
*/
			    status = nclc_tessmgr_set_srf_polylst(sf.key,&trilst);

				if (sfi->trianlist == UU_NULL && status == UU_SUCCESS)
				{
					npts = trilst.cur_cnt;
					i = (npts > 200)? npts: 200;
					sfi->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
					uu_list_init (sfi->trianlist,sizeof (UM_trian),i,i);
					uu_list_push_list (sfi->trianlist,&trilst);
				}	
				uu_list_free (&trilst);
			}
			else
			{
				if (sfi->trianlist == UU_NULL)
				{
					npts = polylst->cur_cnt;
					i = (npts > 200)? npts: 200;
					sfi->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
					uu_list_init (sfi->trianlist,sizeof (UM_trian),i,i);
					uu_list_push_list (sfi->trianlist,polylst);
/*
........If we free polylst here, then
........the next time this surface is referenced
........in this routine, it will have no boundary points
........and will be ignored
........Bobby - 10/17/14
*/
/*					uu_list_free (polylst);*/
				}	
			}

			if (irot > 0)
			{
				ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
				ntri = sfi->trianlist->cur_cnt;
				for (i = 0; i < ntri; i++, ptri++)
				{
					um_cctmtf(ptri->p1,rot,ptri->p1);
					um_cctmtf(ptri->p2,rot,ptri->p2);
					um_cctmtf(ptri->p3,rot,ptri->p3);
				}
			}
		}
	}

	status = UU_SUCCESS;
	ncl_init_polygon (&pgn0,0);
	pgn0.contour = vtxlist;

/*
..... if not self-intersecting, use the boundary horizontal projection as
..... the initial polygon
*/
	if (sfi->sf_flag != BADPROJ && sfi->flag1 != VSOLID)
	{
		npts = sfi->bound.npo - 1;
		pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
		pgn0.num_contours = 1;
		pgn0.np = (int *) uu_malloc (sizeof(int));
		if (!pgn0.np) goto Err;
		pgn0.np[0] = npts;
		for (i = 0; i < npts; i++) uu_list_push (vtxlist,pts[i]);

		firstime = UU_FALSE;
	}

#if 0
	if (sfi->key == 2302)
	{
		tritol = 0.;
	}
#endif
/*	ncl_tri_toler(sfi->trianlist,tolsq,&tritol);*/
	tritol = tolsq;
	ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
	ntri = sfi->trianlist->cur_cnt;
	if (ntri < 2) goto Done;

	for (i = 0; i < ntri; i++, ptri++)
	{
		UU_LIST_EMPTY (tpol->contour);

		if (i+1 < ntri && S_form_block (&ptri[0],&ptri[1],tolsq,tpol->contour))
		{
			i++; ptri++;
			npts = UU_LIST_LENGTH (tpol->contour);
			if (npts == 0) continue;
		}
		else
		{
			if (ncl_proj_tri(ptri,tritol,tpol->contour) != 0)
				continue;
			npts = UU_LIST_LENGTH (tpol->contour);
		}

		tpol->np[0] = npts;
		if (firstime)
		{
			status = ncl_copy_polygon(tpol,&pgn0);
			if (status != UU_SUCCESS) goto Done;
			firstime = UU_FALSE;
		}
		else
		{
#if 0
	if (sfi->key == 2302)
	{
		UN_clip_debug = UU_FALSE;
		sprintf(sbuf,"i = %d",i);
		NclxDbgPstr(sbuf);
		sprintf(sbuf,"npts = %d",npts);
		NclxDbgPstr(sbuf);
		NclxDbgPstr("ERASE/ALL");
		ncl_debug_polygon(UU_NULL,&pgn0,4);
		ncl_debug_polygon(UU_NULL,tpol,2);
	}
#endif
			istat = ncl_polygon_clip (NCL_CONTOUR,&pgn0,tpol,&pgn0,tol,tolsq);
#if 0
	if (sfi->key == 2302)
	{
		ncl_debug_polygon(UU_NULL,&pgn0,5);
		UN_clip_debug = UU_FALSE;
	}
#endif
			if (istat != UU_SUCCESS)
			{
				UU_FREE (pgn0.np);
				pgn0.num_contours = 0;
				UU_LIST_EMPTY (vtxlist);
				firstime = UU_TRUE;
			}
/*
..... some contour-fixing would be good here, will be added in 9.7
.....
			else
				ncl_prepare_for_pocketing (&pgn0,tol,tolsq);
*/
		}
		
		if (!vtxlist || !vtxlist->data) goto Err;
	}
#if 0
	if (sfi->key == 2302)
		ncl_debug_polygon(UU_NULL,&pgn0,5);
#endif

	goto Done;

Err:
	status = UU_FAILURE;

Done:
	if (!lv97)
	{
		ncl_free_bndry (&bndr);
		uu_list_free (&uvlst);
		uu_list_free (&cvlst);
	}

	UU_FREE (pgn0.np);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_proj_tess (sfi,tol,tolsq,vtxlist)
**       Project a surface onto the horizontal plane using tessellation.
**    PARAMETERS
**       INPUT  :
**          sfi        - current surface data
**          tol        - tolerance
**       OUTPUT :
**          vtxlist    - list of 2D points
**    RETURNS      :
**         UU_SUCCESS iff no errors and valid projection, else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_proj_tess (sfi,rot,tol,tolsq,vtxlist,tpol)
NCL_waterline_surf *sfi;
UM_transf rot;
UU_REAL tol,tolsq;
UU_LIST *vtxlist;
ncl_polygon *tpol;
{
	int i,status,ntri,irot;
	UM_trian *ptri;
	UM_tessellation tess;
	struct NCL_fixed_databag sf;
	UU_LOGICAL lv97;
		
	lv97 = ncl_setver(97);
#if 0
	if (sfi->key == 19305)
	{
		i = 0;
	}
#endif

	if (sfi->flag1 == SHADOW_BOX)
	{
		S_push_box (sfi,vtxlist);
		return (UU_SUCCESS);
	}
	
	if (lv97 && sfi->trianlist == UU_NULL)
	{
		if (ncl_is_watrev()) return (UU_SUCCESS);

		ncl_get_rotfl (&irot);
		um_set_tess_toler(tol);
		um_init_tess(&tess);
		sf.key = sfi->key;
		status = ncl_retrieve_data_fixed (&sf);
		if (status == UU_SUCCESS)
		{
			status = ncl_get_tesslst (&sf,&tess);
			if (status != UU_SUCCESS)
				status = ncl_tessellate_surf(&sf,&tess);
		}
		if (status == UU_SUCCESS)
		{
			sfi->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
			sfi->trianlist->data = UU_NULL;
			status = ncl_get_tess_triangles (&tess,sfi->trianlist,0,0);
		}
		um_free_tess(&tess);
		if (status != UU_SUCCESS) return (UU_SUCCESS);

		if (irot > 0)
		{
			ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
			ntri = sfi->trianlist->cur_cnt;
			for (i = 0; i < ntri; i++, ptri++)
			{
				um_cctmtf(ptri->p1,rot,ptri->p1);
				um_cctmtf(ptri->p2,rot,ptri->p2);
				um_cctmtf(ptri->p3,rot,ptri->p3);
			}
		}
	}

	status = ncl_proj_tess1 (sfi,rot,tol,tolsq,vtxlist,tpol);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_close_gaps (pp,npts,cpol,ctolsq)
**       Project contour points onto a polygon boundary if already
**       within tolerance
**    PARAMETERS
**       INPUT  :
**          npts     - number of points
**          pp       - contour points
**          cpol     - current polygon
**          ctolsq   - squared tolerance
**       OUTPUT :
**          pp       - contour points, possibly modified
**    RETURNS      :
**         1 iff some points are changed, else 0
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_close_gaps (pp,npts,cpol,ctolsq)
UM_2Dcoord *pp;
int npts;
ncl_polygon *cpol;
UU_REAL ctolsq;
{
	UM_2Dcoord *vtx;
	int nv,nv0,i,npt1,c,nc,iret;
	int click = 0;

	npt1 = npts;
	if (UM_SQDIS_2D(pp[0],pp[npts-1]) < ctolsq) npt1 = npts-1;

	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (cpol->contour);
	nc = cpol->num_contours;
	if (nc <= 0) return (click);

	for (c = 0, nv0 = 0; c < nc; c++)
	{
		nv = cpol->np[c];
/*
..... we consider only real contours, not holes or degenerate ones
*/
		if (nv > 3)
		{
			for (i = 0; i < npt1; i++)
			{
				iret = um_sticky_contour (vtx,nv,pp[i],ctolsq);
				if (iret == 1) click = 1;
			}
		}

		nv = abs(nv);
		nv0 = nv0 + nv;
	}
	if (npt1 < npts)
	{
		pp[npt1][0] = pp[0][0];
		pp[npt1][1] = pp[0][1];
	}

	return (click);
}

/*********************************************************************
**    E_FUNCTION     : ncl_pol_inside (pol,pol0,tol)
**       Determine whether a 2D point is inside any of the 2D polygon
**       contours.
**    PARAMETERS
**       INPUT : 
**                point    - the point to check
**                pol      - polygon structure
**                tol      - tolerance
**       OUTPUT:  none
**    RETURNS      : UU_TRUE if inside; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL ncl_pol_inside (pol,pol0,tol)
ncl_polygon *pol,*pol0;
UU_REAL tol;
{
	int i,npt,npt0;
	UM_2Dcoord *vtx0,*vtx;

	npt0 = pol0->np[0];
	npt = pol->np[0];

	if (pol0->num_contours == 1 && npt0 >= 4)
	{
		vtx0 = (UM_2Dcoord *) UU_LIST_ARRAY (pol0->contour);
		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
		for (i = 0; i < npt; i++)
		{
			if (um_check_inside(vtx0,npt0,vtx[i],UU_NULL,tol) == -1)
				return (UU_FALSE);
		}
		return (UU_TRUE);
	}
	else
		return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_bndr_inside (bound,pol,tol)
**       Determine whether a 2D point is inside any of the 2D polygon
**       contours.
**    PARAMETERS
**       INPUT : 
**                point    - the point to check
**                pol      - polygon structure
**                tol      - tolerance
**       OUTPUT:  none
**    RETURNS      : UU_TRUE if inside; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL ncl_bndr_inside (bound,pol0,tol)
UM_srf_bound *bound;
ncl_polygon *pol0;
UU_REAL tol;
{
	int i,npt,npt0;
	UM_coord *pts;
	UM_2Dcoord *vtx0;

	npt0 = pol0->np[0];
					
	if (npt0 >= 4)
	{
		npt = bound->npo - 1;
		pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
		vtx0 = (UM_2Dcoord *) UU_LIST_ARRAY (pol0->contour);

		for (i = 0; i < npt; i++)
		{
			if (um_check_inside(vtx0,npt0,pts[i],UU_NULL,tol) == -1)
				return (UU_FALSE);
		}
	}

	return (UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_create_contour_stock (numsf,sff,npt,stock)
**       Create pseudo-stock for a collection of surfaces.
**    PARAMETERS
**       INPUT  :
**          numsf      - number of surfaces
**          sff        - data for each surface
**          npt        - estimated number of points in stock
**       OUTPUT :
**          stock      - minimal contour containing all surfaces
**          
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_contour_stock (numsf,sff,rot,ctol,ctolsq)
int numsf;
NCL_waterline_surf *sff;
UM_transf rot;
UU_REAL ctol,ctolsq;
{
	int i,isf,status,npts;
	NCL_waterline_surf *sfi;
	UM_coord *pts,*pt3d;
	UU_LIST *vtxlist;
	UM_2Dcoord *vtx,*pt2d;
	UU_LOGICAL firstime = UU_TRUE;
	UU_LOGICAL havebad = UU_FALSE;
	UU_LOGICAL lwatrev;
	ncl_polygon tpol;
	ncl_polygon *pol0,*pol;
	int tnp = 3;
	int ISTK = 1;
	UU_LIST tlist;
	UU_REAL wmin[2],wmax[2];
	char sbuf[80];

	ncl_get_wpol (0,&pol0);
	ncl_get_wpol (1,&pol);
	status = UU_FAILURE;

	if (pol0->contour == UU_NULL || pol0->contour->data == UU_NULL || 
		pol->contour == UU_NULL || pol->contour->data == UU_NULL) goto Done;

	pol0->num_contours = pol->num_contours = 1;
	pol0->np = (int *) uu_malloc (sizeof(int));
	if (pol0->np == UU_NULL) goto Done;
	pol0->np[0] = 0;

	pol->np = (int *) uu_malloc (sizeof(int));
	if (pol->np == UU_NULL) goto Done;
	pol->np[0] = 0;

	tpol.box = UU_NULL;
	tpol.num_contours = 1;
	tpol.np = &tnp;
	uu_list_init (&tlist,sizeof(UM_2Dcoord),4,4);
	tpol.contour = &tlist;
	if (tpol.contour->data == UU_NULL) goto Done;
	lwatrev = ncl_is_watrev();

	if (lwatrev)
		ncl_wat_get_uvlim (wmin,wmax);
	else
	{
		wmin[0] = wmin[1] = -1.e25;
		wmax[0] = wmax[1] = 1.e25;
	}

	status = UU_SUCCESS;

	for (isf = 0, sfi = sff; isf < numsf; isf++, sfi++)
	{
	UN_sfkey = sfi->key;
	if (sfi->key == 2302)
	{
		status = UU_SUCCESS;
	}
		if (sfi->slist == LIST_C) continue;
		if (sfi->key == NULLKEY) continue;
		if (sfi->flag1 != VSOLID)
		{
			if (sfi->bound.npo < 4) continue;
		}

		vtxlist = (firstime)? pol0->contour: pol->contour;
		vtxlist->cur_cnt = 0;

		if (sfi->flag1 == VSOLID)
		{
			status = ncl_proj_tess (sfi,rot,ctol,ctolsq,vtxlist,&tpol);
			if (status != UU_SUCCESS) goto Done;
		}
		else
		{
			npts = sfi->bound.npo - 1;
			pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);

			if (sfi->sf_flag == VERT || sfi->sf_flag == VERTPL)
			{
				if (lwatrev && sfi->sf_flag == VERTPL)
					S_vert_pln (npts,pts,vtxlist,sfi->nvec,wmin,wmax,ctol);
				else
					continue;
			}
/*
... Assume there is always a bottom surface, and skip verticals. The reason
... being the 0.75*tol offset creates distortions when the contour is expanded.
... If there is a problem we could call ncl_vert_bndry after first checking
... that the boundary projection is not completely inside the existing contour.
		{
			status = ncl_vert_bndry (npts,pts,vtxlist,0.75*tol);
			if (status != UU_SUCCESS) goto Done;
		}
*/
			else
			{
				if (sfi->sf_flag == GEN && !lwatrev)
					ncl_test_projection (&sfi->sf_flag,npts,pts);
				if (sfi->sf_flag == BADPROJ)
				{
					havebad = UU_TRUE;
					if (lwatrev) continue;
					status = ncl_proj_tess (sfi,rot,ctol,ctolsq,vtxlist,&tpol);

					if (status != UU_SUCCESS) goto Done;
					if (vtxlist->cur_cnt == 0) sfi->sf_flag = VERT;
/*
........If the tessellation projection
........is bad, then we will use the boundary projection
........Since it is typically cleaner than the tessellation projection
........QAR 101076
........Bobby - 10/17/14
*/
					else if (sfi->sf_flag == BADPROJ)
					{
						pt3d = (UM_coord *)
							uu_malloc(sizeof(UM_coord)*vtxlist->cur_cnt);
						pt2d = (UM_2Dcoord *)UU_LIST_ARRAY(vtxlist);
						um_convert2D_3D(vtxlist->cur_cnt,pt2d,pt3d);
						sfi->sf_flag = TILTPL;
						ncl_test_projection (&sfi->sf_flag,vtxlist->cur_cnt,pt3d);
						uu_free(pt3d);
						if (sfi->sf_flag == BADPROJ); //continue;
						{
							UU_LIST_EMPTY(vtxlist);
							for (i = 0; i < npts; i++)
								uu_list_push (vtxlist,pts[i]);
						}
					}
				}
				else
				{
					for (i = 0; i < npts; i++)
						uu_list_push (vtxlist,pts[i]);
				}
			}
		}

		npts = vtxlist->cur_cnt;
		if (npts >= 3)
		{
			if (firstime)
			{
/*
..... create first nontrivial horizontal projection polygon
*/
				if (sfi->sf_flag == BADPROJ || sfi->flag1 == VSOLID)
					ncl_fix_contour (pol0->contour,0,&npts,ctol,ctolsq,ISTK);
				pol0->np[0] = npts;
				firstime = UU_FALSE; continue;
			}
/*
..... add other (horizontal projection) polygons one-by-one by forming a union
*/
#if 0
	if (sfi->key == 2302)
	{
		NclxDbgPstr("ERASE/ALL");
		pol->np[0] = npts;
		ncl_debug_polygon(UU_NULL,pol,4);
	}
#endif
			if (sfi->sf_flag == BADPROJ || sfi->flag1 == VSOLID)
				ncl_fix_contour (pol->contour,0,&npts,ctol,ctolsq,ISTK);
			pol->np[0] = npts;
#if 0
	if (sfi->key == 2302)
	{
		ncl_debug_polygon(UU_NULL,pol,2);
	}
#endif
			vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);

			ncl_close_gaps (vtx,npts,pol0,ctolsq);
#if 0
	if (sfi->key == 2302)
	{
		ncl_debug_polygon(UU_NULL,pol,5);
	}
#endif

#if 0
	sprintf(sbuf,"Sfkey = %d",sfi->key);
	NclxDbgPstr(sbuf);
#endif
#if 0
	if (sfi->key == 14458)
	{
		NclxDbgPstr("ERASE/ALL");
		ncl_debug_polygon(UU_NULL,pol0,4);
		ncl_debug_polygon(UU_NULL,pol,2);
	}
#endif

			status = ncl_polygon_clip (NCL_CONTOUR,pol0,pol,pol0,ctol,ctolsq);
#if 0
	if (sfi->key == 14458)
	{
		ncl_debug_polygon(UU_NULL,pol0,5);
	}
#endif

			ncl_prepare_for_pocketing (pol0,ctol,ctolsq);

			if (status != UU_SUCCESS)
			{
				if (ncl_pol_inside (pol,pol0,ctol))
					status = UU_SUCCESS;
				else
					goto Done;
			}
		}
	}

	if (lwatrev && havebad)
	{
		for (isf = 0, sfi = sff; isf < numsf; isf++, sfi++)
		{
			if (sfi->key == 0 || sfi->bound.npo < 4 || sfi->sf_flag != BADPROJ)
				continue;

			vtxlist = (firstime)? pol0->contour: pol->contour;
			vtxlist->cur_cnt = 0;

			if (!firstime && pol0->num_contours == 1)
			{
				npts = sfi->bound.npo - 1;
				pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
				if (ncl_bndr_inside (&sfi->bound,pol0,ctol)) continue;
			}
			status = ncl_proj_tess (sfi,rot,ctol,ctolsq,vtxlist,&tpol);

			if (status != UU_SUCCESS) goto Done;
			if (vtxlist->cur_cnt == 0) sfi->sf_flag = VERT;

			npts = vtxlist->cur_cnt;
			if (npts >= 3)
			{
				if (firstime)
				{
/*
..... create first nontrivial horizontal projection polygon
*/
					ncl_fix_contour (pol0->contour,0,&npts,ctol,ctolsq,ISTK);
					pol0->np[0] = npts;
					firstime = UU_FALSE; continue;
				}
/*
..... add other (horizontal projection) polygons one-by-one by forming a union
*/
				ncl_fix_contour (pol->contour,0,&npts,ctol,ctolsq,ISTK);

				pol->np[0] = npts;
				vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);

				ncl_close_gaps (vtx,npts,pol0,ctolsq);

				status = ncl_polygon_clip (NCL_CONTOUR,pol0,pol,pol0,ctol,ctolsq);

				if (status != UU_SUCCESS) goto Done;
				ncl_prepare_for_pocketing (pol0,ctol,ctolsq);
			}
		}
	}
/*
..... qar 96207 - if only have vertical planes, use them
*/		
	if (!lwatrev && firstime)
	{
		for (isf = 0, sfi = sff; isf < numsf; isf++, sfi++)
		{
			if (sfi->key == 0 || sfi->bound.npo < 4 || sfi->sf_flag != VERTPL) continue;

			npts = sfi->bound.npo - 1;
			pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
			vtxlist = (firstime)? pol0->contour: pol->contour;
			vtxlist->cur_cnt = 0;

			S_vert_pln (npts,pts,vtxlist,sfi->nvec,wmin,wmax,ctol);

			npts = vtxlist->cur_cnt;
			if (npts >= 3)
			{
				if (firstime)
				{
/*
..... create first nontrivial horizontal projection polygon
*/
					pol0->np[0] = npts;
					firstime = UU_FALSE; continue;
				}

				pol->np[0] = npts;
				vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
				ncl_close_gaps (vtx,npts,pol0,ctolsq);

				status = ncl_polygon_clip (NCL_CONTOUR,pol0,pol,pol0,ctol,ctolsq);
				ncl_prepare_for_pocketing (pol0,ctol,ctolsq);

				if (status != UU_SUCCESS)
				{
					if (ncl_pol_inside (pol,pol0,ctol))
						status = UU_SUCCESS;
					else
						goto Done;
				}
			}
		}
	}
/*
..... pol0 contains the union of all horizontal projection polygons
*/
	if (pol0->np[0] < 3)
	{
		status = UU_FAILURE; goto Done;
	}
/*
..... qar 95128:
..... warning if more than one contour, will output only the first one
*/
	if (pol0->num_contours > 1) status = 481;
	pol0->num_contours = 1;
	npts = pol0->np[0];
	pol0->contour->cur_cnt = npts;

	ncl_fix_contour (pol0->contour,0,&npts,2*ctol,ctolsq,ISTK);

Done:
	UU_FREE (pol0->np); UU_FREE (pol->np);
	uu_list_free (&tlist);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_create_2box (xmmx,ymmx,offdis,bbox)
**       Create a 2D-box pseudo-stock for a collection of surfaces.
**    PARAMETERS
**       INPUT  :
**          xmmx      - the X-range
**          ymmx      - the Y-range
**          offdis    - expansion parameter
**       OUTPUT :
**          bbox      - 2D box struct
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_create_2box (xmmx,ymmx,offdis,bbox)
UU_REAL xmmx[],ymmx[],offdis;
UM_2box *bbox;
{
	bbox->xmin = xmmx[0] - offdis;
	bbox->ymin = ymmx[0] - offdis;
	bbox->xmax = xmmx[1] + offdis;
	bbox->ymax = ymmx[1] + offdis;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_create_boxgeo (bbox,wbox)
**       Copy a 2D-box to a NCL_w_geo struct
*********************************************************************/
void ncl_create_boxgeo (bbox,wbox)
UM_2box *bbox;
NCL_w_geo *wbox;
{
	wbox->xrange[0] = bbox->xmin;
	wbox->yrange[0] = bbox->ymin;
	wbox->xrange[1] = bbox->xmax;
	wbox->yrange[1] = bbox->ymax;

	wbox->pt[0][0] = bbox->xmin; wbox->pt[0][1] = bbox->ymin;
	wbox->pt[0][2] = 1.e+6;
	wbox->pt[1][0] = bbox->xmax; wbox->pt[1][1] = bbox->ymin;
	wbox->pt[1][2] = 1.e+6;
	wbox->pt[2][0] = bbox->xmax; wbox->pt[2][1] = bbox->ymax;
	wbox->pt[2][2] = 1.e+6;
	wbox->pt[3][0] = bbox->xmin; wbox->pt[3][1] = bbox->ymax;
	wbox->pt[3][2] = 1.e+6;

	wbox->inside = -1;
	wbox->depth = 0;
	wbox->np = 4;
}
