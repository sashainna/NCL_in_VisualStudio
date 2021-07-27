/*********************************************************************
**    NAME         :  necvfix.c
**       CONTAINS: Support routines used by both NCL and IGES
**       ncl_fix_tol
**       ncl_ins_pts
**       ncl_fit_corner
**       ncl_fix_corners
**       ncl_fix_corner0
**       ncl_fix_contour0
**       ncl_cv_weed
**       ncl_cv_deloop
**       ncl_cv_defold
**       ncl_uv_deloop
**       ncl_cvofs_shuffle
**       ncl_cvofs_reshuffle
**       ncl_fix_evol_a
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       necvfix.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:29
*********************************************************************/

#include "nccs.h"
#include "modef.h"
#include "mdcoord.h"
#include "mdeval.h"
#include "nclfc.h"

#define CO9 (UU_REAL) 0.987688
#define CO15 (UU_REAL) 0.965926
#define CO225 (UU_REAL) 0.9238795

/* maximum number of points to insert */
#define NX 100

//#define NX 4096

/*********************************************************************
**    E_FUNCTION     : int ncl_fix_tol(npts, tol, points, tangs)
**    Fix the points/tangents data to use for curve fitting.
**    If the distance between a point and its previous and next is small,
**    we nullify the point's tangent vector; if a segment between a point
**    and its next is big, we insert some points (with zero tangent) in the
**    middle and keep the tangents at both ends.
**    PARAMETERS
**       INPUT  :
**    npts       number of points to consider
**    tol        chord height tolerance
**    points     list of points
**    tangs	     list of tangent vectors
**       OUTPUT :
**    npts          new number of points
**    points        changed list of evolved points
**    tangs	        changed list of tangent vectors
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fix_tol(npts, tol, points, tangs)
int *npts;
UU_REAL tol;
UU_LIST *points, *tangs;
{
	int i, j, n1, nins;
	UM_coord *pp, *pts, pti[NX];
	UM_vector *vv, *vs, VVNUL[NX];
	UU_REAL len, len1;

	n1 = *npts;
	for (i = 0; i < NX; i++)
		um_nullvc (VVNUL[i]);

	pts = (UM_coord *) UU_LIST_ARRAY(points);
	vs = (UM_vector *) UU_LIST_ARRAY(tangs);
/*
..... We keep the first point's tangent and look at the first segment: if
..... it is big, we unitize the tangents, insert some points, and go on to
..... the next segment. Exit the loop when first small segment if found.
*/
	for (i = 1; i < n1-1; i++)
	{
		pp = pts + i - 1;
		vv = vs + i - 1;
		len = UM_MAG(vv[1]);

		if ((UM_DCCCC (pp[0], pp[1]) < tol/2.) && (len < 99000))
		{
			uu_list_delete (points,i,1);
			uu_list_delete (tangs,i,1);
			pts = (UM_coord *) UU_LIST_ARRAY(points);
			vs = (UM_vector *) UU_LIST_ARRAY(tangs);
			i--; n1--;
			continue;
		}

		nins = ncl_ins_pts (pp[0],vv[0],pp[1],vv[1],tol,pti);
		if (nins <= 0 && len < 99000) break;
		if (nins == 1)
		{
			uu_list_insert (points,i,pti[0]);
			uu_list_insert (tangs,i,VVNUL[0]);
			i++; n1++;
		}
		else if (nins > 1)
		{
			uu_list_insert_multiple (points,i,nins,pti);
			uu_list_insert_multiple (tangs,i,nins,VVNUL);
			i+=nins; n1+=nins;
		}
		pts = (UM_coord *) UU_LIST_ARRAY(points);
		vs = (UM_vector *) UU_LIST_ARRAY(tangs);
		if (nins > 0 && len > 99000)
			i++;
	}

	for (j = i+1; j < n1; j++)
	{
		pp = pts + j - 1;
		vv = vs + j - 1;
		len1 = UM_MAG(vv[1]);

		if ((UM_DCCCC (pp[0], pp[1]) < tol/2.) && (len1 < 99000))
		{
			uu_list_delete (points,j,1);
			uu_list_delete (tangs,j,1);
			pts = (UM_coord *) UU_LIST_ARRAY(points);
			vs = (UM_vector *) UU_LIST_ARRAY(tangs);
			j--; n1--;
			continue;
		}

		nins = ncl_ins_pts (pp[0],vv[0],pp[1],vv[1],tol,pti);

		if (nins <= 0)
		{
			if (len < 99000)
				um_nullvc (vv[0]);
			len = len1;
		}
		else if (nins == 1)
		{
			uu_list_insert (points,j,pti[0]);
			uu_list_insert (tangs,j,VVNUL[0]);
			j++; n1++;
		}
		else
		{
			uu_list_insert_multiple (points,j,nins,pti);
			uu_list_insert_multiple (tangs,j,nins,VVNUL);
			j+=nins; n1+=nins;
		}
		pts = (UM_coord *) UU_LIST_ARRAY(points);
		vs = (UM_vector *) UU_LIST_ARRAY(tangs);
		if (nins > 0 && len1 > 99000)
		{
			len = len1;
			j++;
		}
	}

	*npts = n1;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_ins_pts (pt0,vv0,pt1,vv1,tol,pti)
**    Determine if a points/tangents segment is too big to use for curve
**    fitting. If it is, calculate the extra points to insert
**    (uniformly between the two endpoints), depending on the segment size.
**    PARAMETERS
**       INPUT  :
**    pt0,pt1    segment endpoints
**    vv0,vv1    tangent vectors at the endpoints
**    tol        chord height tolerance
**    points     list of points
**    tangs	     list of tangent vectors
**       OUTPUT :
**    pti        inserted points
**
**    RETURNS      : number of points to insert, 0 if the segment is OK
**    SIDE EFFECTS : none
**    WARNINGS     : this routine does not insert anything, it just
**                   calculates what to insert
*********************************************************************/
int ncl_ins_pts (pt0,vv0,pt1,vv1,tol,pti)
UM_coord pt0, pt1, *pti;
UM_vector vv0, vv1;
UU_REAL tol;
{
	UU_REAL co,tana,dd;
	UM_vector dv,w0,w1;
	int i, ins;

	ins = 0;

	um_unitvc (vv0,w0); um_unitvc (vv1,w1);
	co = UM_DOT (w0, w1);
	if (co < 0.99) return (0);

	um_vcmnvc (pt1,pt0,dv);
	dd = UM_MAG (dv);

	if (co < 0.999999)
	{
/*
..... if alpha is the angle between the two vectors vv0 and vv1, then
..... tana is tan(alpha/2), and (dd*tana)/2 is an estimated maximal
..... deviation for this segment
*/
		tana = sqrt ((1 - co)/(1 + co));
		if (dd * tana < 2 * tol) return (0);
	}

	ins = dd / (100. * tol);
	if (ins < 1) ins = 1;
	if (ins > NX) ins = NX;

	um_vctmsc (dv, 1./(ins+1), dv);
	um_vcplvc (pt0, dv, pti[0]);

	for (i = 1; i < ins; i++)
		um_vcplvc (pti[i-1], dv, pti[i]);

	return (ins);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_fix_tol1(k,npts, tol, points, tangs)
**    This routine is for curves on surfaces.
**    Two things are done: (1) if a segment is long, a point in the middle and
**    a point near the end are inserted; the starting point end the two
**    inserted points are given "long" vectors; (2) if a segment is "not long
**    but long enough" and makes a sharp corner, the corner is emphasized
**    by setting a "long" vector either at start or near the end of the
**    segment.
**    PARAMETERS
**       INPUT  :
**    k          starting point in the list
**    npts       number of points to consider
**    tol        chord height tolerance
**    points     list of points
**    tangs	     list of tangent vectors (the actual vectors are not used)
**       OUTPUT :
**    npts          new number of points
**    points        changed list of evolved points
**    tangs	        changed list of tangent vectors
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fix_tol1(k,npts,tol,points,tangs)
int k,*npts;
UU_REAL tol;
UU_LIST *points, *tangs;
{
	int i,n1,nins;
	UM_coord *pp, *pts, pti[2];
	UM_vector *vv, *vs, dv, dv1, vsi[2];
	UU_REAL del;

	n1 = *npts;

	pts = (UM_coord *) UU_LIST_ARRAY(points); pts += k;
	vs = (UM_vector *) UU_LIST_ARRAY(tangs); vs += k;

	if (n1 <= 2) goto Done;

	for (i = 1; i < n1; i++)
	{
		pp = pts + i - 1;
		vv = vs + i - 1;

		um_vcmnvc (pp[1],pp[0],dv);
		del = UM_MAG (dv);
		if (del > 200. * tol)
		{
			um_vctmsc (dv,100000.,vsi[0]);
			um_vctovc (vsi[0],vv[0]);
			um_translate_point (pp[0],0.5,dv,pti[0]);
			um_translate_point (pp[0],1.-(2.*tol)/del,dv,pti[1]);
			um_vctovc (vsi[0],vsi[1]);
			nins = 2;
			uu_list_insert_multiple (points, i+k, nins, pti);
			uu_list_insert_multiple (tangs, i+k, nins, vsi);
			i+=nins; n1+=nins;
			pts = (UM_coord *) UU_LIST_ARRAY(points); pts += k;
			vs = (UM_vector *) UU_LIST_ARRAY(tangs); vs += k;
		}
		else if (del > 20.*tol)
		{
			um_unitvc (dv,dv);
			um_vctmsc (dv,100000.,vsi[0]);
			if (i > 1)
			{
				um_vcmnvc (pp[0],pp[-1],dv1);
				if (UM_MAG(dv1) > tol)
				{
					um_unitvc (dv1,dv1);
					if (UM_DOT(dv,dv1) < UM_COS30)
						um_vctovc (vsi[0],vv[0]);
				}
			}
			if (i < n1-1)
			{
				um_vcmnvc (pp[2],pp[1],dv1);
				if (UM_MAG(dv1) > tol)
				{
					um_unitvc (dv1,dv1);
					if (UM_DOT(dv,dv1) < UM_COS30)
					{
						um_translate_point (pp[0],del-2.*tol,dv,pti[0]);
						uu_list_insert (points,i+k,pti[0]);
						uu_list_insert (tangs,i+k,vsi[0]);
						i++; n1++;
						pts = (UM_coord *) UU_LIST_ARRAY(points); pts += k;
						vs = (UM_vector *) UU_LIST_ARRAY(tangs); vs += k;
					}
				}
			}
		}
	}

Done:;
	*npts = n1;
	return (0);
}

/*******************************************************************
**    E_FUNCTION     : UU_REAL ncl_fit_corner (dv1,dv2,tol)
**  For a corner (defined by 2 vectors) calculate the distance, such
**  that, if 2 points are on the 2 corner sides at this distance from
**  the vertex point, at least one of the following holds:
**     1) the distance from the arc, which is tangent to the corner at
**     the 2 points, and the vertex point is tol; and the distance between
**     the 2 points is no less than tol/2,
**  or
**     2) the distance between the 2 points is tol/2
**    WARNINGS     : We return zero for an extremely acute or extremely
**                   obtuse corner
*******************************************************************/
UU_REAL ncl_fit_corner (dv1,dv2,tol)
UM_vector dv1, dv2;
UU_REAL tol;
{
	UU_REAL co,sina,coef1,coef2, len;

	len = 0.;

	co = UM_DOT (dv1,dv2);
	sina = sqrt ((1 + co)/2.);
	if (sina < 1.e-8 || sina > 1. - 1.e-8) return (0.);
/*
..... len = tol*coef1 means the added points are at tol/2 distance
..... len = tol*coef2 means the corner point is at tol distance from the arc
..... coef1 > coef2 until the angle exceeds appr. 24 degrees
*/
	coef1 = 0.25/sina;
	coef2 = sqrt((1 + sina)/(1 - sina));
	len = (coef1 >= coef2)? tol * coef1 : tol * coef2;
	return (len);
}

/*******************************************************************
**    E_FUNCTION     : ncl_cv_weed (cvpoint,cvtang,lexten,tol,npts)
**  Apply a mover-type weeding to a curve. The purpose is to avoid steps
**  caused by delooping a 3d offset curve.
*******************************************************************/
void ncl_cv_weed (cvpoint,cvtang,tol,lexten,n1)
UU_REAL tol;
UU_LIST *cvpoint,*cvtang;
UU_LOGICAL lexten;
int *n1;
{
	int i,npts;
	UM_coord *pp,pext;
	UM_vector *vv,vvi,vext;
	UU_LIST lspt,lsvc;

	npts = *n1;

	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

	if (lexten)
	{
/*
..... if curve was extended, save the end, weed before it, then reattach
*/
		um_vctovc (pp[npts-1],pext);
		um_vctovc (vv[npts-1],vext);

		npts--;
		cvpoint->cur_cnt = cvtang->cur_cnt = npts;
	}

	uu_list_init(&lspt,sizeof(UM_coord),npts,25);
	uu_list_init(&lsvc,sizeof(UM_vector),npts,25);

	uu_list_push_list(&lspt,cvpoint);
	for (i = 0; i < npts; i++)
	{
		um_unitvc (vv[i],vvi);
		uu_list_push(&lsvc,vvi);
	}

	UU_LIST_EMPTY (cvpoint);
	UU_LIST_EMPTY (cvtang);

	npts = ncl_mover_weed(npts,&lspt,&lsvc,cvpoint,cvtang,tol);

	if (lexten)
	{
		uu_list_push(cvpoint,pext);
		uu_list_push(cvtang,vext);
		npts++;
	}

	uu_list_free (&lspt);
	uu_list_free (&lsvc);

	*n1 = npts;

	return;
}

/*******************************************************************
**    E_FUNCTION     : UU_REAL ncl_prjdis (pt1,pt2,nvec)
**  Calculate the square distance between points. If nvec is provided,
**  only the horizontal distance is calculated.
*******************************************************************/
static UU_REAL ncl_prjdis (pt1,pt2,zvec)
UM_coord pt1,pt2;
UU_REAL *zvec;
{
	UM_vector vc;
	UU_REAL d,dz;

	um_vcmnvc (pt1,pt2,vc);
	d = UM_DOT (vc,vc);
	if (zvec != UU_NULL)
	{
		dz = UM_DOT (vc,zvec);
		d = d - dz*dz;
	}
	return (d);
}

/*******************************************************************
**    E_FUNCTION     : ncl_aver_iseg (pt,pint,qt,qint,zvec,tolsq,pqi)
*******************************************************************/
void ncl_aver_iseg (pt,pint,qt,qint,zvec,tolsq,pqi)
UM_coord pt,pint,qt,qint,pqi;
UU_REAL *zvec;
UU_REAL tolsq;
{
	UU_REAL dd,dp,dq;

	um_vctovc (pint,pqi);
	dd = UM_SQDIS (pint,qint);
	if (dd > tolsq)
	{
		dp = ncl_prjdis (pt,pint,zvec);
		dq = ncl_prjdis (qt,qint,zvec);

		dp = sqrt(dp);
		dq = sqrt(dq);

		dd = dp + dq;
		if (dd < UM_DFUZZ) return;

		dp = dp / dd;
		dq = dq / dd;
		um_avcplbvc (dq, pint, dp, qint, pqi);
	}
}

/*******************************************************************
**    E_FUNCTION     : void ncl_find_closer (pp,pt1,pt2,qt1,qt2,ZVEC,
**                                   pint,qint,fact,tol,dd,ipts,jpts)
*******************************************************************/
static void ncl_find_closer (pp,pt1,pt2,qt1,qt2,ZVEC,pint,qint,fact,tol,
dd,ipts,jpts)
UM_coord *pp,pt1,pt2,qt1,qt2;
UU_REAL *ZVEC,fact,tol,dd;
UM_coord pint,qint;
int ipts,*jpts;
{
	int j,k,nint;
	UU_REAL dp;
	UM_coord pk,qk,qk1,qk2;

	j = *jpts;

	for (k = j-1; k > ipts+1; k--)
	{
		um_vctovc (pp[k], qk1);
		um_vctovc (pp[k+1], qk2);

		um_isegseg1 (pt1,pt2,qk1,qk2,ZVEC,&nint,pk,qk,fact,tol);
		if (nint > 0)
		{
			dp = ncl_prjdis (pt1,pk,ZVEC);
			if (dp < dd)
			{
				*jpts = k;
				um_vctovc (pk,pint);
				um_vctovc (qk,qint);
				um_vctovc (qk1,qt1);
				um_vctovc (qk2,qt2);
				return;
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_cv_deloop (cvpoint,cvtang,closed,ifl,ZVEC,
**                                                 fact,tol,tolsq,n1)
**       Get rid of loops in the offset curve.
**    PARAMETERS
**       INPUT  :
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors
**    closed     1 iff the curve is closed
**    ifl        1 in the beginning, cut off the ends before the first
**                 intersection
**               2 in the beginning, check if the first loop (after the first
**                 intersection) contains the main body, if so cut off the ends
**                 before it
**    ZVEC       the main plane normal, or NULL
**    fact       parameter that measures how big the offset was
**               (as compared with tol)
**    tol,tolsq  tolerance
**    n1         number of pts
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors
**    n1         fixed number of pts
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cv_deloop (cvpoint,cvtang,closed,ifl,ZVEC,fact,tol,tolsq,n1)
UU_REAL *ZVEC,fact,tol,tolsq;
UU_LIST *cvpoint,*cvtang;
int closed,ifl,*n1;
{
	int npts,nint,ipts,jpts,k;
	UU_REAL dd,co;
	UM_coord *pp,*vv,pint,qint,pt1,pt2,qt1,qt2,pqi;
	UM_vector vvp,vvq;

	npts = *n1;

	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
	*n1 = 0;
/*
..... cut before the first loop if needed
*/
	if (ifl == 1 || ifl == 2)
	{
		for (ipts = 0; ipts < npts-3; ipts++)
		{
			um_vctovc (pp[ipts], pt1);
			um_vctovc (pp[ipts+1], pt2);

			for (jpts = npts - 2; jpts > ipts+1; jpts--)
			{
				if (ifl == 2 && jpts - ipts == npts - 2) continue;
				um_vctovc (pp[jpts], qt1);
				um_vctovc (pp[jpts+1], qt2);
				um_isegseg1 (pt1,pt2,qt1,qt2,ZVEC,&nint,pint,qint,fact,tol);

				if (nint > 0)
				{
					if (ifl == 2)
					{
						dd = ncl_prjdis (pt1,pint,ZVEC);
						if (dd < tolsq && ipts == 0) continue;

						k = jpts - ipts - 1;
						if (3*k < 2*npts) goto Gen;
					}
					dd = ncl_prjdis (pt2,pint,ZVEC);
					if (dd < tolsq)
					{
						ipts++;
						um_vctovc (pp[ipts], pt1);
						um_vctovc (pp[ipts+1], pt2);
					}
					dd = ncl_prjdis (qt1,qint,ZVEC);
					if (dd < tolsq)
					{
						jpts--;
						um_vctovc (pp[jpts], qt1);
						um_vctovc (pp[jpts+1], qt2);
					}

					ncl_aver_iseg (pt2,pint,qt1,qint,ZVEC,tolsq,pqi);

					um_vctovc (pqi,pp[ipts]);
					um_vctovc (pqi,pp[jpts+1]);
					npts = jpts+2;
					cvpoint->cur_cnt = cvtang->cur_cnt = npts;
					if (ipts > 0)
					{
						npts -= ipts;
						uu_list_delete (cvpoint,0,ipts);
						uu_list_delete (cvtang,0,ipts);
						pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
						vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
					}
					ifl = 0;
					goto Gen;
				}
			}
		}
	}

Gen:
	if (ifl == 1) return;
/*
..... cut off each loop, starting from endpoint and moving toward the middle
*/
	for (ipts = 0; ipts < npts - 3; ipts++)
	{
		um_vctovc (pp[ipts], pt1);
		um_vctovc (pp[ipts+1], pt2);

		for (jpts = npts - 2; jpts > ipts+1; jpts--)
		{
			if (closed == 1 && (jpts - ipts == npts - 2)) continue;
			um_vctovc (pp[jpts], qt1);
			um_vctovc (pp[jpts+1], qt2);
			um_isegseg1 (pt1,pt2,qt1,qt2,ZVEC,&nint,pint,qint,fact,tol);
			if (nint > 0)
			{
				dd = ncl_prjdis (pt1,pint,ZVEC);
				if (dd < tolsq)
				{
					if (ipts <= 0) continue;
					ipts--;
					um_vctovc (pp[ipts], pt1);
					um_vctovc (pp[ipts+1], pt2);
					dd = ncl_prjdis (pt1,pint,ZVEC);
				}

				ncl_find_closer (pp,pt1,pt2,qt1,qt2,ZVEC,pint,qint,fact,tol,dd,ipts,&jpts);

				dd = ncl_prjdis (qt2,qint,ZVEC);
				if (dd < tolsq)
				{
					jpts++;
					um_vctovc (pp[jpts], qt1);
					um_vctovc (pp[jpts+1], qt2);
				}

				ncl_aver_iseg (pt1,pint,qt2,qint,ZVEC,tolsq,pqi);

				um_vcmnvc (pqi,pt1, vvp);
				if (UM_DOT(vvp,vvp) > tolsq)
					um_unitvc (vvp, vv[ipts]);

				um_vctovc (pqi, pp[jpts]);

				um_vcmnvc (qt2, pqi, vvp);
				dd = UM_DOT (vvp,vvp);
				if (dd > tolsq)
				{
					um_unitvc (vvp,vvp);
					um_unitvc (vv[jpts],vvq);
					co = UM_DOT (vvq, vvp);
					if (co > -0.9) um_vctovc (vvp, vv[jpts]);
				}

				k = jpts - ipts - 1;
				if (k > 0)
				{
					npts -= k;
					uu_list_delete (cvpoint,ipts+1,k);
					uu_list_delete (cvtang,ipts+1,k);
					pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
					vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
				}
				break;
			}
		}
	}

	*n1 = npts;

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cv_defold (cvpoint,cvtang,closed,tol,tolsq,n1)
**       Get rid of zigzags in the offset curve.
**
**    PARAMETERS
**       INPUT  :
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors
**    tol	     tolerance
**    npts       number of evolved pts
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors
**    npts       fixed number of pts
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cv_defold (cvpoint,cvtang,closed,tolsq,n1)
UU_REAL tolsq;
UU_LIST *cvpoint,*cvtang;
int closed,*n1;
{
	int ipts,npts,k;
	UM_coord *pp;
	UM_vector *vv;
	UM_vector vvp,vv1,vv2;
	UU_REAL dd,co1,co2;
	UU_REAL tolsq0 = tolsq/4.;

	npts = *n1;

	for (ipts = 0; ipts < npts-1; ipts++)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		um_vcmnvc (pp[ipts+1],pp[ipts],vvp);
		dd = UM_DOT (vvp,vvp);
		if (dd < tolsq0)
		{
			uu_list_delete (cvpoint,ipts+1,1);
			uu_list_delete (cvtang,ipts+1,1);
			npts--;
			ipts--;
			continue;
		}
		if (dd < tolsq) continue;
		dd = sqrt (dd);
		for (k = 0; k < 3; k++)	vvp[k] = vvp[k]/dd;

		um_unitvc (vv[ipts],vv1);
		um_unitvc (vv[ipts+1],vv2);

		co1 = UM_DOT (vvp,vv1);
		co2 = UM_DOT (vvp,vv2);
		if (co1 + co2 < -1.8)
		{
			if (closed == 1)
			{
				um_middlept(pp[ipts],pp[ipts+1],pp[ipts]);
				um_middlept(vv[ipts],vv[ipts+1],vv[ipts]);
				uu_list_delete (cvpoint,ipts+1,1);
				uu_list_delete (cvtang,ipts+1,1);
				npts--;
				ipts--;
				continue;
			}
			npts = ipts + 1;
			break;
		}
	}

	*n1 = npts;

	return;

}

/*********************************************************************
**    E_FUNCTION     : ncl_deloop0 (cvpoint,cvtang,ZVEC,fact,tol,tolsq,n1)
**
**    PARAMETERS
**       INPUT  :
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors
**    tol	     tolerance
**    npts       number of evolved pts
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors
**    npts       fixed number of pts
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cv_deloop0 (cvpoint,cvtang,ZVEC,fact,tol,tolsq,n1)
UU_REAL *ZVEC,fact,tol,tolsq;
UU_LIST *cvpoint, *cvtang;
int *n1;
{
	int npts,nint,ipts,jpts,k;
	UU_REAL dd,co;
	UM_coord *pp,*vv,pint,qint,pt1,pt2,qt1,qt2;
	UM_vector vvp,vvq;

	npts = *n1;

	if (npts < 5) return;
	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

	for (ipts = 0; ipts < npts-3; ipts++)
	{
		um_vctovc (pp[ipts], pt1);
		um_vctovc (pp[ipts+1], pt2);

		for (jpts = npts - 2; jpts > ipts+1; jpts--)
		{
			if (jpts - ipts == npts - 2) continue;
			um_vctovc (pp[jpts], qt1);
			um_vctovc (pp[jpts+1], qt2);
			um_isegseg1 (pt1,pt2,qt1,qt2,ZVEC,&nint,pint,qint,fact,tol);
			if (nint > 0)
			{
				dd = ncl_prjdis (pt1,pint,ZVEC);

				if (dd < tolsq && ipts == 0) continue;

				k = jpts - ipts - 1;
				if (3*k > 2*npts)
				{
					dd = ncl_prjdis (qt1,pint,ZVEC);
					if (dd < tolsq) jpts--;
					um_vctovc (pint, pp[jpts+1]);
					npts = jpts+2;
					cvpoint->cur_cnt = cvtang->cur_cnt = npts;

					dd = ncl_prjdis (pt2,pint,ZVEC);

					if (dd < tolsq) ipts++;
					um_vctovc (pint, pp[ipts]);
					if (ipts > 0)
					{
						npts -= ipts;
						uu_list_delete (cvpoint,0,ipts);
						uu_list_delete (cvtang,0,ipts);
					}
					*n1 = npts;
					return;
				}
				goto Next;
			}
		}
	}

Next:
	for (ipts = 0; ipts < npts-3; ipts++)
	{
		um_vctovc (pp[ipts], pt1);
		um_vctovc (pp[ipts+1], pt2);

		for (jpts = npts - 2; jpts > ipts+1; jpts--)
		{
			if (jpts - ipts == npts - 2) continue;
			um_vctovc (pp[jpts], qt1);
			um_vctovc (pp[jpts+1], qt2);
			um_isegseg1 (pt1,pt2,qt1,qt2,ZVEC,&nint,pint,qint,fact,tol);
			if (nint > 0)
			{
				dd = ncl_prjdis (pt1,pint,ZVEC);

				if (dd < tolsq)
				{
					if (ipts <= 0) continue;
					ipts--;
				}

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
					//Below comm out to disable removing, Sasha, May28, 2020
					/*npts -= k;*/
					//npts -= npts -k - 1;
					///*uu_list_delete (cvpoint,ipts+1,k);
					//uu_list_delete (cvtang,ipts+1,k);*/
					//uu_list_delete (cvpoint,ipts+1,npts -k - 1);
					//uu_list_delete (cvtang,ipts+1,npts -k - 1);
					*n1 = npts;
					return;
				}
				return;
			}
		}
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_fix_corner0 (cvpoint,cvtang,tol,npts)
**    Fix the sharp corner at the first point
**    PARAMETERS
**       INPUT  :
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors
**    tol	     tolerance
**    npts       number of evolved pts
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors
**    npts       fixed number of pts
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fix_corner0 (cvpoint,cvtang,tol,npts)
UU_REAL tol;
UU_LIST *cvpoint, *cvtang;
int *npts;
{
	int n1,nint;
	UU_REAL d0,d1;
	UM_coord *pp,*vv,pint,pin0;
	UM_vector v0,v1,vv0,vv1;

	n1 = *npts;

	if (n1 < 4) goto Err;
	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

	um_unitvc (vv[n1-1],vv1);
	um_unitvc (vv[0],vv0);

	um_ilnln(pp[n1-1],vv1,pp[0],vv0,&nint,pint);
	if (nint == 1)
	{
		um_vcmnvc (pint, pp[n1-1], v1);
		d1 = UM_DOT (v1, vv1);
		um_vcmnvc (pp[0], pint, v0);
		d0 = UM_DOT (v0, vv0);

		if (fabs(d0) < UM_DFUZZ && fabs(d1) < UM_DFUZZ)
			goto Done;

		if (d0*d1 < - 25.0*tol*tol) goto Err;

		if (d0 + d1 < 2.*tol)
		{
			um_vctovc (pint,pin0);
			um_isegseg (pp[0],pp[1],pp[n1-2],pp[n1-1],&nint,pint,tol);
			if (nint < 1)
			{
				if (d0 + d1 > -2.*tol)
					um_vctovc (pin0,pint);
				else
					goto Err;
			}
		}

		if (d1 < tol)
			um_vctovc (pint, pp[n1-1]);
		else
		{
			uu_list_push (cvpoint, pint);
			uu_list_push (cvtang, vv1);
			pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
			vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
			n1++;
		}
		if (d0 < tol)
			um_vctovc (pint, pp[0]);
		else
		{
			uu_list_insert (cvpoint,0, pint);
			uu_list_insert (cvtang,0, vv0);
			n1++;
		}
	}
	goto Done;

Err:
	n1 = 1;
Done:
	*npts = n1;

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_uv_deloop (ncc,uvpts)
**          Remove loops from closed contour in UV-space. The contour
**          does not start at a corner, therefore "inclosed" logic is
**          not needed.
**    PARAMETERS
**       INPUT  :
**         uvpts     list of evolved pts
**         ncc       number of evolved pts
**       OUTPUT :
**         uvpts     fixed list of evolved pts
**         ncc       fixed number of evolved pts
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uv_deloop (ncc,uvpts)
int *ncc;
UU_LIST *uvpts;
{
	int status, inclosed = 0;

	status = ncl_uv_deloop1 (ncc,uvpts,inclosed);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_uv_deloop1 (ncc,uvpts,inclosed)
**          Remove loops from closed contour in UV-space
**    PARAMETERS
**       INPUT  :
**         uvpts     list of evolved pts
**         inclosed  flag for the inside offset
**         ncc       number of evolved pts
**       OUTPUT :
**         uvpts     fixed list of evolved pts
**         ncc       fixed number of evolved pts
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uv_deloop1 (ncc,uvpts,inclosed)
int *ncc,inclosed;
UU_LIST *uvpts;
{
	UM_coord *uvs;
	int npts,ipts,jpts,k,nint,n1,k0,k1;
	int ccw = 0;
	UU_REAL dd,area;
	UM_2Dcoord pt1,pt2,qt1,qt2, pint;
	UU_REAL tol = 0.00025;
	UU_REAL tolsq = 5.e-8;

	npts = *ncc;
/*
..... cut the ends sticking out the largest
..... loop, and keep the loop as the curve offset
*/
	uvs = (UM_coord *) UU_LIST_ARRAY (uvpts);

	for (ipts = 1, area = 0.; ipts < npts - 1; ipts++)
	{
		area += um_triangle_signed_area(uvs[0],uvs[ipts],uvs[ipts+1]);
	}
	if (area > 1.e-4)
		ccw = 1;
	else if (-area > 1.e-4)
		ccw = -1;

	for (ipts = 0; inclosed == 1 && ipts < npts-3; ipts++)
	{
		for (k = 0; k < 2; k++)
		{
			pt1[k] = uvs[ipts][k]; pt2[k] = uvs[ipts+1][k];
		}
		for (jpts = npts - 2; jpts > ipts+1; jpts--)
		{
			for (k = 0; k < 2; k++)
			{
				qt1[k] = uvs[jpts][k]; qt2[k] = uvs[jpts+1][k];
			}
			um_isegseg2d (pt1,pt2,qt1,qt2,&nint,pint,tol);
			if (nint > 0)
			{
				inclosed = 0;
				if (UM_SQDIS_2D (pt2,pint) < tolsq)
					ipts++;
				for (k = 0; k < 2; k++) uvs[ipts][k] = pint[k];
				if (UM_SQDIS_2D (qt2,pint) < tolsq) jpts++;
				jpts++;
				for (k = 0; k < 2; k++) uvs[jpts][k] = pint[k];
				jpts++;
				k = npts - jpts;
				if (k > 0)
					uu_list_delete (uvpts,jpts,k);
				npts = jpts;
				uu_list_delete (uvpts,0,ipts);
				npts -= ipts;
				break;
			}
		}
	}
	if (inclosed == 1) return (-1);
/*
..... Remove loops
*/
	for (ipts = 0; ipts < npts-3; ipts++)
	{
		uvs = (UM_coord *) UU_LIST_ARRAY (uvpts);
		for (k = 0; k < 2; k++)
		{
			pt1[k] = uvs[ipts][k]; pt2[k] = uvs[ipts+1][k];
		}
		for (jpts = npts - 2; jpts > ipts+1; jpts--)
		{
			if (jpts - ipts == npts - 2) continue;
			for (k = 0; k < 2; k++)
			{
				qt1[k] = uvs[jpts][k]; qt2[k] = uvs[jpts+1][k];
			}
			um_isegseg2d (pt1,pt2,qt1,qt2,&nint,pint,tol);
			if (nint > 0)
			{
				if (ccw != 0)
				{
					n1 = npts-1;
					for (k = 1, area = 0.; k < n1; k++)
					{
						k0 = (jpts + k)%n1;
						k1 = (k0 + 1)%n1;
						if (k1 == ipts) break;
						area += um_triangle_signed_area(pint,uvs[k0],uvs[k1]);
					}
					if (area*ccw < -tolsq)
					{
						ccw = 0;
						if (UM_SQDIS_2D (pt2,pint) < tolsq)
							ipts++;
						for (k = 0; k < 2; k++) uvs[ipts][k] = pint[k];
						if (UM_SQDIS_2D (qt2,pint) < tolsq) jpts++;
						jpts++;
						for (k = 0; k < 2; k++) uvs[jpts][k] = pint[k];
						jpts++;
						k = npts - jpts;
						if (k > 0)
							uu_list_delete (uvpts,jpts,k);
						npts = jpts;
						uu_list_delete (uvpts,0,ipts);
						npts -= ipts;
						ipts = 0;
						break;
					}
					ccw = 0;
				}

				if (UM_SQDIS_2D (pt1,pint) < tolsq) ipts--;
				for (k = 0; k < 2; k++) uvs[jpts][k] = pint[k];
				k = jpts - ipts - 1;
				if (k > 0)
				{
					npts -= k;
					uu_list_delete (uvpts,ipts+1,k);
				}
				break;
			}
		}
	}
/*
..... Weed out double points
*/
	uvs = (UM_coord *) UU_LIST_ARRAY(uvpts);
	for (ipts = 0; ipts < npts-1; ipts++)
	{
		dd = UM_SQDIS_2D (uvs[ipts+1],uvs[ipts]);
		if (dd < tolsq)
		{
			uu_list_delete (uvpts,ipts+1,1);
			npts--;
			ipts--;
			uvs = (UM_coord *) UU_LIST_ARRAY(uvpts);
			continue;
		}
	}

	*ncc = npts;
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cvofs_shuffle (cvpoint,cvtang,tol,npts)
**      Shuffle a closed curve, if necessary, so that it starts
**      away from corners.
**    PARAMETERS
**       INPUT  :
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors
**    tol	     tolerance
**    npts       number of evolved pts
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors
**    npts       fixed number of pts
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvofs_shuffle (cvpoint,cvtang,tol,npts)
UU_REAL tol;
UU_LIST *cvpoint,*cvtang;
int *npts;
{
	int i,n1,i0,i1,ip,ic;
	UU_REAL cco,dd,tana,tolsq,dmax;
	UM_coord *pp,pti;
	UM_vector *vv,vv0,vv1,vci;

	n1 = *npts;
	tolsq = 400*tol*tol;

	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

	um_unitvc (vv[n1-1],vv1);
	um_unitvc (vv[0],vv0);

	cco = UM_DOT(vv0,vv1);
/*
..... if the angle is less than 9 degrees, continue
*/
	if (cco > CO15) return (0);

	um_translate_point (pp[n1-1],-tol,vv1,pp[n1-1]);

	dmax = -1;
	i0 = i1 = ip = ic = 0;

	for (i = 0; i < n1-2; i++)
	{

		um_vctovc (vv[i],vv0);
		dd = UM_DOT (vv0,vv0);
		if (dd < UM_DFUZZ) continue;

		um_vctovc (vv[i+1],vv1);
		dd = UM_DOT (vv1,vv1);
		if (dd < UM_DFUZZ) continue;

		um_unitvc (vv0,vv0);
		um_unitvc (vv1,vv1);

		cco = UM_DOT(vv0,vv1);

		if (cco > CO9) continue;
		if (cco > CO15)
		{
			dd = UM_SQDIS (pp[i],pp[i+1]);
			tana = (1 - cco)/(1 + cco);
			if (dd*tana < tolsq) continue;
		}
		um_vcmnvc (pp[i+1],pp[i], vv0);
		um_unitvc (vv0,vv0);
		um_vcmnvc (pp[i+2],pp[i+1], vv1);
		um_unitvc (vv1,vv1);
		cco = UM_DOT(vv0,vv1);
		if (cco > CO9) continue;

		ic = i+1;

		dd = UM_SQDIS (pp[ip],pp[ic]);
		if (dd > dmax)
		{
			dmax = dd;
			i0 = ip;
			i1 = ic;
		}
		ip = ic;
	}

	if (dmax > 0)
	{
		dd = UM_SQDIS (pp[ip],pp[n1-1]);
		if (dd > dmax)
		{
			i0 = ip;
			i1 = n1-1;
		}
	}

	if (i1 == 0) i1 = n1-1;

	if (i1 < i0+1) return (0);

	if (i1 == i0+1)
	{
		um_middlept (pp[i0],pp[i1],pti);
		um_vcmnvc (pp[i1],pp[i0],vci);
		uu_list_insert (cvpoint,i1,pti);
		uu_list_insert (cvtang,i1,vci);

		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

		n1++;
		i1++;
	}

	i0 = (i0 + i1)/2;
	n1++;

	for (i = 0; i <= i0; i++)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

		um_vctovc (pp[i],pti);
		um_vctovc (vv[i],vci);

		uu_list_push (cvpoint,pti);
		uu_list_push (cvtang,vci);
	}

	uu_list_delete (cvpoint,0,i0);
	uu_list_delete (cvtang,0,i0);


Done:
	*npts = n1;

	return (1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cvofs_reshuffle (cvpoint,cvtang,tol,npts)
**      Shuffle a closed curve so that it starts at a corner
**      near a given point.
**    PARAMETERS
**       INPUT  :
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors
**    pshuf      desired start
**    tol	     tolerance
**    npts       number of evolved pts
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors
**    npts       fixed number of pts
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cvofs_reshuffle (cvpoint,cvtang,pshuf,tol,tolsq,n1)
UU_LIST *cvpoint,*cvtang;
UM_coord pshuf;
UU_REAL tol,tolsq;
int n1;
{
	int i,is;
	UU_REAL cco,dd,dmin;
	UM_coord *pp,pti;
	UM_vector *vv,vv0,vv1,vci;


	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

	dmin = 100.*tolsq;
	is = 0;

	for (i = 0; i < n1-2; i++)
	{
		um_vctovc (vv[i],vv0);
		dd = UM_DOT (vv0,vv0);
		if (dd < UM_DFUZZ) continue;

		um_vctovc (vv[i+1],vv1);
		dd = UM_DOT (vv1,vv1);
		if (dd < UM_DFUZZ) continue;

		um_unitvc (vv0,vv0);
		um_unitvc (vv1,vv1);

		cco = UM_DOT(vv0,vv1);

		if (cco > CO15) continue;
		dd = UM_SQDIS (pshuf,pp[i+1]);

		if (dd < dmin)
		{
			dmin = dd;
			is = i+1;
		}
	}

	if (is < 1 || i > n1-2) return;


	for (i = 1; i <= is; i++)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

		um_vctovc (pp[i],pti);
		um_vctovc (vv[i],vci);

		uu_list_push (cvpoint,pti);
		uu_list_push (cvtang,vci);
	}

	uu_list_delete (cvpoint,0,is);
	uu_list_delete (cvtang,0,is);

	n1 = cvpoint->cur_cnt;

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cvofs_rev (cvpoint,cvtang,tol,npts)
**      Reverse an open curve, if necessary, so that it does not starts
**      with a wiggle.
**    PARAMETERS
**       INPUT  :
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors
**    tol	     tolerance
**    npts       number of evolved pts
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors
**    npts       fixed number of pts
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_cvofs_rev (cvpoint,cvtang,tol,npts)
UU_REAL tol;
UU_LIST *cvpoint,*cvtang;
int npts;
{
	UU_REAL tolsq,c1,c2,d1,d2;
	UM_coord *pp;
	UM_vector *vv,vva,vvb;
	UU_LOGICAL reversed = UU_FALSE;

	tolsq = tol*tol;

	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

	d1 = UM_SQDIS (pp[0],pp[1]);
	d2 = UM_SQDIS (pp[npts-2],pp[npts-1]);

	if (d2 > 100*d1 && d2 > 400*tolsq)
	{
		um_unitvc (vv[0],vva);
		um_unitvc (vv[1],vvb);

		c1 = UM_DOT(vva,vvb);

		um_unitvc (vv[npts-2],vva);
		um_unitvc (vv[npts-1],vvb);

		c2 = UM_DOT(vva,vvb);

		if (c1 < CO15 && c2 > CO9)
		{
			reversed = UU_TRUE;

			ncl_revers1_list (npts,0,pp,1);
			ncl_revers1_list (npts,0,vv,2);
		}
	}

	return (reversed);
}

/*********************************************************************
**    E_FUNCTION     : ncl_fix_corners (cvpoint,cvtang,tol,iflag, npts)
**    Adds corner points and tangent vectors at sharp corners
**    PARAMETERS
**       INPUT  :
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors
**    tol	     tolerance
**    npts       number of evolved pts
**    iflag      0 means generic, not closed (call after offset)
**               1 means we are translating (not offsetting) a curve
**               2 if this is the call before offset(3D)
**               3 if this is the call before offset(2D)
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors
**    npts       fixed number of pts
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fix_corners (cvpoint,cvtang,tol,iflag, npts)
UU_REAL tol;
UU_LIST *cvpoint, *cvtang;
int iflag, *npts;
{
		int i,n1,nins,nint,indoubt;
		UU_REAL co,cco,d0,dprev,d1,dnext,dd;
		UU_REAL tana,tolsq;
		UM_coord *pp,*vv,pint,pt0,pt1,pti[3];
		UM_vector v0,v1,vv0,vv1,vvi[3];
		UM_int2 idx = 169;
		UM_real8 ver;
		UU_LOGICAL lv94,ltransl,l2d;

		l2d = UU_FALSE;
		if (iflag == 3)
		{
			iflag = 2;
			l2d = UU_TRUE;
		}

		n1 = *npts;
		if (iflag == 2)
			tolsq = 400*tol*tol;
		else
			tolsq = 0;

		ltransl = (iflag == 1);

		getsc(&idx, &ver);
		lv94 = (ver < 9.449);

		for (i = 0; i < n1-1; i++)
		{
			nins = indoubt = 0;
			pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
			vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
			pp += i; vv += i;

			d0 = UM_MAG (vv[0]);
			d1 = UM_MAG (vv[1]);
			if (d0 < 0.0001 || d1 < 0.0001) continue;
			if (ltransl && (d0 > 90000. || d1 > 90000.)) continue;
			um_unitvc (vv[0],vv0);
			um_unitvc (vv[1],vv1);

			cco = UM_DOT(vv0,vv1);
/*
..... if the angle is less than 9 degrees, continue
*/
			if (cco > CO9) continue;
			if (!lv94 && iflag == 2 &&
				((!l2d && cco > CO15) || (l2d && cco > CO225)))
			{
				dd = UM_SQDIS (pp[0],pp[1]);
				tana = (1 - cco)/(1 + cco);
				if (dd*tana < tolsq) continue;
			}

			um_vcmnvc (pp[1],pp[0], v0);
			um_unitvc (v0,v0);
			if (i < n1 - 2)
			{
				um_vcmnvc (pp[2],pp[1], v1);
				um_unitvc (v1,v1);
				co = UM_DOT(v0,v1);
				if (co > CO9) continue;
			}

			if (!lv94 && iflag == 2 && cco < 0.5)
			{
				um_vctovc (pp[1],pint);
				um_vctmsc (vv0,100000.,vv0);
				um_vctovc (pint,pti[nins]);
				um_vctovc (vv0,vvi[nins]);
				nins++;

				um_vctmsc (vv1,100000.,vv1);
				um_vctovc (vv1, vv[1]);
			}
			else
			{
			um_ilnln(pp[0],vv0,pp[1],vv1,&nint,pint);
			if (nint == 0) continue;

			if (ltransl)
			{
				d0 = UM_DCCCC(pp[1],pp[0]);
				co = UM_DOT (v0,vv0);
				if (fabs(d0-3.*tol) < 0.01*tol && co > 0.99995)
					dd = 3.*tol;
				else if (cco > CO15)
					continue;
				else
					dd = ncl_fit_corner (vv0,vv1,tol);
			}
			else
				dd = ncl_fit_corner (vv0,vv1,tol);
			um_avcplbvc (1.,pint,-dd,vv0,pt0);
			um_avcplbvc (1.,pint,dd,vv1,pt1);
/*
.....Logic for inserting pt0
*/
			um_vcmnvc (pint, pp[0], v0);
			d0 = UM_DOT (v0, vv0);
			if (2*dd > d0)
			{
				if (i > 1)
				{
					um_vcmnvc (pint, pp[-1], v0);
					dprev = UM_DOT (v0, vv0);
					um_vctmsc (vv0,100000.,vv0);
					if ((2*dd <= dprev) || (!ltransl && dd <= dprev - UM_FUZZ))
					{
						um_vctovc (pt0, pp[0]);
						um_vctovc (vv0, vv[0]);
					}
					else if ((ltransl && cco < CO225) || d0 < - UM_FUZZ || cco < 0.)
					{
						um_vctovc (pt0, pp[-1]);
						um_vctovc (vv0, vv[-1]);
						uu_list_delete (cvpoint,i,1);
						uu_list_delete (cvtang,i,1);
						i--; n1--; pp--; vv--;
					}
					else
						um_vctovc (vv0, vv[0]);
				}
			}
			else
			{
				um_vctmsc (vv0,100000.,vv0);
				um_vctovc (pt0,pti[nins]);
				um_vctovc (vv0,vvi[nins]);
				nins++;
			}
			um_vcmnvc (pp[1], pint, v1);
			d1 = UM_DOT (v1, vv1);
/*
..... add apex point unless this is a preliminary call before offsetting
			if (iflag != 2 && d0 > tol && d1 > tol)
*/
			if (iflag != 2)
			{
				um_nullvc (v0);
				um_vctovc (pint,pti[nins]);
				um_vctovc (v0,vvi[nins]);
				nins++;
				if (d0 < tol || d1 < tol) indoubt = 1;
			}
/*
.....Logic for inserting pt1
*/
			if (2*dd > d1)
			{
				if (i < n1 - 2)
				{
					um_vcmnvc (pp[2], pint, v1);
					dnext = UM_DOT (v1, vv1);
					um_vctmsc (vv1,100000.,vv1);
					if ((2*dd <= dnext) || (!ltransl && dd <= dnext - UM_FUZZ))
					{
						um_vctovc (pt1, pp[1]);
						um_vctovc (vv1, vv[1]);
					}
					else if ((i < n1-3) &&
						((ltransl && cco < CO225) || d1 < - UM_FUZZ || cco < 0.))
					{
						um_vctovc (pt1, pp[2]);
						um_vctovc (vv1, vv[2]);
						uu_list_delete (cvpoint,i+1,1);
						uu_list_delete (cvtang,i+1,1);
						n1--;
					}
					else
						um_vctovc (vv1, vv[1]);
				}
			}
			else
			{
				um_vctmsc (vv1,100000.,vv1);
				um_vctovc (pt1,pti[nins]);
				um_vctovc (vv1,vvi[nins]);
				nins++;
			}
			}
			if (nins <= 2 && indoubt == 1)
			{
				pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
				d0 = UM_DCCCC (pint,pp[i]);
				d1 = UM_DCCCC (pint,pp[i+1]);
				if (d0 < tol || d1 < tol)
				{
					nins--;
					if (nins == 1 && d0 < tol)
					{
						um_vctovc (pti[1],pti[0]);
						um_vctovc (vvi[1],vvi[0]);
					}
				}
			}
			if (nins == 1)
			{
				uu_list_insert (cvpoint, i+1, pti);
				uu_list_insert (cvtang, i+1, vvi);
				i++; n1++;
			}
			else if (nins >= 2)
			{
				uu_list_insert_multiple (cvpoint, i+1, nins, pti);
				uu_list_insert_multiple (cvtang, i+1, nins, vvi);
				i += nins; n1 += nins;
			}
		}

		goto Done;

Err:;
		n1 = 1;
Done:;
		*npts = n1;

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_fix_evol_a (crv,cvmat,crvout,npts,tol,
**                                         points,tangs,upars)
**    Fix the points/tangents data to use for curve projection.
**    If a segment between a point and its next is big, we uniformly insert
**    several points.
**    PARAMETERS
**       INPUT  :
**        crv      - pointer to curve entity
**        cvmat    - transformation matrix
**        crvout   - (pointer to) curve evaluator
**        npts     - number of points
**        tol      - chord height tolerance
**        points   - list of points
**        tangs	   - list of tangent vectors
**        uptr	   - list of u-values
**       OUTPUT :
**        npts              -   new number of points
**        points,tangs,uptr -   changed lists
**
**    RETURNS      : UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fix_evol_a (crv,cvmat,crvout,npts,tol,points,tangs,upars)
struct NCL_fixed_databag *crv;
UM_transf cvmat;
struct UM_evcrvout *crvout;
int *npts;
UU_REAL tol;
UU_LIST *points,*tangs,*upars;
{
	int i,j,nn,n1,status;
	UM_coord *pts,*uu,uvp;
	UM_vector *vs,w0,w1,wi;
	UU_REAL co,cs0,cs1,u0,dd,du,tolsq,tol1sq,tol2sq,epsq,tanb;
	UU_LOGICAL bigangle;

/*
.....Initialize routine
*/
	n1 = *npts;
	uvp[1] = uvp[2] = 0.;
	pts = (UM_coord *) UU_LIST_ARRAY(points);
	vs = (UM_vector *) UU_LIST_ARRAY(tangs);
	uu = (UM_coord *) UU_LIST_ARRAY(upars);

	tolsq = tol*tol;
	epsq = 16*tolsq;
	tol1sq = 100*tolsq;
	tol2sq = 10000*tolsq;

/*
..... the change below is for QAR 92198 - to ensure a flat ending stretch in
..... a closed curve is evolved with enough points
*/
	if (UM_SQDIS(pts[0],pts[n1-1]) < tolsq)
		um_vctovc(vs[0],vs[n1-1]);

	for (i = 0; i < n1-1; i++)
	{
		du = uu[i+1][0] - uu[i][0];
		if (du < 0.02) continue;

		dd = UM_SQDIS (pts[i+1],pts[i]);
		if (dd < tol1sq) continue;

		um_unitvc (vs[i],w0); um_unitvc (vs[i+1],w1);
		co = UM_DOT (w0,w1);

		bigangle = UU_FALSE;
		if (dd < tol2sq)
		{
			if (co > CO9) continue;
			bigangle = UU_TRUE;
			if (co > CO15)
			{
/*
..... if alpha is the angle between the two vectors w0 and w1, then
..... (dd/2 * tan(alpha/2) / 2) is an estimated deviation for this segment
*/
				tanb = (1 - co)/(1 + co);
				if (dd*tanb < epsq) continue;
			}
		}

		dd = sqrt(dd);
		nn = dd/tol + 0.5;
		nn = nn/100 + 1;

		if (nn <= 1 && bigangle) nn = 2;

		if (nn > 1)
		{
			du /= nn;
			u0 = uu[i][0];
			for (j = 1; j < nn; j++)
			{
				uvp[0] = u0 + j*du;
				status = uc_evcrv(UM_FRSTDERIV,uvp[0],crv,cvmat,crvout);
				if (status != UU_SUCCESS) return (UU_FAILURE);
/*
..... qar 97140 - if tangents at segment endpoints agree and the current
..... tangent goes in the opposite direction, do not insert 
*/
				if (co > CO9)
				{
					um_unitvc (crvout->dcdu,wi);
					cs0 = UM_DOT (w0,wi);
					cs1 = UM_DOT (w1,wi);
					if (cs0 < -CO9 || cs1 < -CO9) continue;
				}
				i++; n1++;
				uu_list_insert (points,i,crvout->cp);
				uu_list_insert (tangs,i,crvout->dcdu);
				uu_list_insert (upars,i,uvp);
			}
		}
		pts = (UM_coord *) UU_LIST_ARRAY(points);
		vs = (UM_vector *) UU_LIST_ARRAY(tangs);
		uu = (UM_coord *) UU_LIST_ARRAY(upars);
	}

	*npts = n1;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_fix_corners_a (crv,cvmat,crvout,npts,tol,
**                                        cvpoint,cvtang,cvpar)
**    Adds corner points and tangent vectors at sharp corners
**    PARAMETERS
**       INPUT  :
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors
**    tol	     tolerance
**    npts       number of evolved pts
**    iflag      0 means generic, not closed (call after offset)
**               1 means we are translating (not offsetting) a curve
**               2 if this is the call before offset
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors
**    npts       fixed number of pts
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fix_corners_a (crv,cvmat,crvout,npts,tol,cvpoint,cvtang,cvpar)
struct NCL_fixed_databag *crv;
UM_transf cvmat;
struct UM_evcrvout *crvout;
int *npts;
UU_REAL tol;
UU_LIST *cvpoint,*cvtang,*cvpar;
{
	int i,n1,ipr,i1,inx,status;
	UU_REAL dd,cco,csp,csn,tana,tolsq,epsq,u0,u1,u2;
	UM_coord *pp,*uu;
	UM_vector *vs;
	UM_coord uvi;
	UM_vector vv0,vv1,vpr,vnx;
	UU_LOGICAL closed;

	n1 = *npts - 1;
	tolsq = tol*tol;
	epsq = 400*tolsq;

	uvi[1] = uvi[2] = 0.;
	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vs = (UM_vector *) UU_LIST_ARRAY(cvtang);
	uu = (UM_coord *) UU_LIST_ARRAY(cvpar);

	closed = (UM_SQDIS(pp[0],pp[n1]) < tolsq);

	for (i = 1; i < n1; i++)
	{
		if (!closed && i == n1-1) continue;
		i1 = i+1;

		dd = UM_DOT (vs[i],vs[i]);
		if (dd < UM_DFUZZ) continue;
		um_unitvc (vs[i],vv0);
		dd = UM_DOT (vs[i1],vs[i1]);
		if (dd < UM_DFUZZ) continue;
		um_unitvc (vs[i1],vv1);

		cco = UM_DOT(vv0,vv1);
/*
..... if the angle is less than 15 degrees, continue
*/
		if (cco > CO15) continue;
		if (cco > CO225)
		{
			dd = UM_SQDIS (pp[i],pp[i1]);
			tana = (1 - cco)/(1 + cco);
			if (dd*tana < epsq) continue;
		}

				ipr = (i - 1 + n1)%n1;
				um_unitvc (vs[ipr],vpr);
				csp = UM_DOT(vpr,vv0);
				if (2*csp*csp - 1 < cco) continue;
				inx = (i+2)%n1;
				um_unitvc (vs[inx],vnx);
				csn = UM_DOT(vnx,vv1);
				if (2*csn*csn - 1 < cco) continue;

		u0 = uu[i][0];
		u1 = uu[i1][0];

		if (u1 - u0 > 0.002)
		{
			uvi[0] = u1 - 0.001;
			status = uc_evcrv(UM_FRSTDERIV,uvi[0],crv,cvmat,crvout);
			if (status != UU_SUCCESS) return (UU_FAILURE);

			dd = UM_SQDIS (crvout->cp,pp[i1]);
			if (dd > tolsq)
			{
				i++; i1++; n1++;

				uu_list_insert (cvpoint,i,crvout->cp);
				uu_list_insert (cvtang,i,crvout->dcdu);
				uu_list_insert (cvpar,i,uvi);

				pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
				vs = (UM_vector *) UU_LIST_ARRAY(cvtang);
				uu = (UM_coord *) UU_LIST_ARRAY(cvpar);
				um_unitvc (vs[i],vv0);

			}

		}
		um_vctmsc (vv0,100000.,vs[i]);
		um_vctmsc (vv1,100000.,vs[i1]);

		if (i1 == n1)
		{
			i1 = 0;
			um_vctovc(vs[n1],vs[0]);

		}
		inx = i1 + 1;
		u1 = uu[i1][0];
		u2 = uu[inx][0];

		if (u2 - u1 > 0.002)
		{
			uvi[0] = u1 + 0.001;
			status = uc_evcrv(UM_FRSTDERIV,uvi[0],crv,cvmat,crvout);
			if (status != UU_SUCCESS) return (UU_FAILURE);

			dd = UM_SQDIS (crvout->cp,pp[i1]);
			if (dd > tolsq)
			{
				i++; i1++; n1++;

				uu_list_insert (cvpoint,i1,crvout->cp);
				uu_list_insert (cvtang,i1,crvout->dcdu);
				uu_list_insert (cvpar,i1,uvi);

				pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
				vs = (UM_vector *) UU_LIST_ARRAY(cvtang);
				uu = (UM_coord *) UU_LIST_ARRAY(cvpar);
			}
		}
	}
	*npts = n1 + 1;

	return (0);
}
