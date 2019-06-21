/*********************************************************************
**    NAME         :  nesupt1.c
**       CONTAINS: Support routines used by both NCL and IGES
**       ncevolve(isf, told, maxpts, pts, vs)
**       ncevolveA(isf, told, maxpts, pts, vs)
**       ncevolve2d(isf, told, maxpts, pts)
**       ncevof(isf,told,maxpts,dm,dis,pts,vs)
**       ncevofA(isf,told,maxpts,dm,dis,pts,vs)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesupt1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 25.1
*********************************************************************/

#include "nccs.h"
#include "mdcoord.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : int ncevolve(isf, told, maxpts, pts, vs)
**    C wrapper for calling ncl_evolve_curve() from Fortran
**    PARAMETERS
**       INPUT  :
**    isf	     curve ptr index
**    told	     chord height tolerance
**    maxpts     number of points allocated in Fortran
**       OUTPUT :
**    pts           curve points
**    vs	        tangents vectors
**    RETURNS      : number of evaluated pts req'd to meet told
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncevolve(isf, told, maxpts, pts, vs)
UM_int2	  *isf;
UM_real8  *told;
UM_int2	  *maxpts;
UM_real8  *pts;
UM_real8  *vs;
{
/*
..... pts (storage for point array is passed in after allocation from cvofwf)
..... max size of point array is passed in via maxpts
..... npts (actual points evaluated) is returned
*/
	int ipts,n1,npts,ix;
	UU_LIST  cvpoint, cvtang;
	UM_real8  *pbuf;/*  point buffer in UU_LIST format */
	UM_real8  *tbuf;/*  tangent buffer in UU_LIST format */
	UU_REAL tol = *told;
	UM_int2 ifl, ifl264;

   /* set up bag ptr in isf-th element of evalent */
	ix = *isf - 1;

   /* evaluate curve points and tangents to given tolerance */
	n1 = ncevolvF (ix, tol, &cvpoint, &cvtang);
	if (n1 <= 0 || n1 > *maxpts) goto Done;

	npts = ncevolveA(tol, n1, &cvpoint, &cvtang);
	if (npts <= 0 || npts > *maxpts) goto Done;

     /* recover pointer to point UU_LIST */
	pbuf = (UU_REAL *) UU_LIST_ARRAY(&cvpoint);
     /* recover pointer to tangent UU_LIST */
	tbuf = (UU_REAL *) UU_LIST_ARRAY(&cvtang);

	ifl = 264;
	getifl(&ifl,&ifl264);
	for (ipts = 0; ipts < 3*npts; ipts++)
	{
		pts[ipts] = pbuf[ipts];
		vs[ipts] = tbuf[ipts];
		if (ifl264 == 1) pts[ipts] *= 25.4;
	}

Done:
   uu_list_free (&cvpoint);
   uu_list_free (&cvtang);

   return (npts);
}

/*********************************************************************
**    E_FUNCTION     : int ncevolveA (tol, npts, cvpoint, cvtang)
**    Fixes the evolved curve for subsequent curve fitting:
**    adds corner points and tangent vectors at composite curve breakout
**    points, nullifies the rest of tangent vectors.
**    PARAMETERS
**       INPUT  :
**    tol	     tolerance
**    npts       number of evolved pts
**    cvpoint    list of evolved pts
**    cvtang     list of tangent vectors 
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors 
**
**    RETURNS      : new number of pts
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncevolveA(tol, npts, cvpoint, cvtang)
UU_REAL tol;
UU_LIST *cvpoint, *cvtang;
int npts;
{
	int i;
	UM_coord *pp, *pts;
	UM_vector *vv,*vs;
	UU_REAL len;
	UU_REAL tolsq = tol*tol;

	ncl_fix_corners (cvpoint, cvtang, tol, 1, &npts);

	pts = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	vs = (UM_vector *) UU_LIST_ARRAY(cvtang);

	for (i = 1; i < npts-1; i++)
	{
		pp = pts + i - 1;
		vv = vs + i - 1;
		len = UM_MAG(vv[1]);

		if ((UM_SQDIS (pp[0], pp[1]) < tolsq) && (len < 99000))
		{
			uu_list_delete (cvpoint,i,1);
			uu_list_delete (cvtang,i,1);
			pts = (UM_coord *) UU_LIST_ARRAY(cvpoint);
			vs = (UM_vector *) UU_LIST_ARRAY(cvtang);
			i--; npts--;
			continue;
		}
		
		if (len < 99000) 
			um_nullvc (vv[1]);
		else
		{
			vv[1][0] /= len; vv[1][1] /= len; vv[1][2] /= len;
		}
	}

	return (npts);
}

/*********************************************************************
**    E_FUNCTION     : int ncevof(isf,told,maxpts,dm,dis,pts,vs)
**    Evolve curve and offset evolved points 
**    PARAMETERS
**       INPUT  :
**    isf	     curve ptr index
**    told	     chord height tolerance
**    maxpts     number of points allocated in Fortran
**    dm         direction modifier vector
**    dis        offset distance
**       OUTPUT :
**    pts            point buffer ptr (allocated in F77)
**    vs             tangent vectors (allocated in F77)
**    RETURNS      : number of points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncevof(isf, told, maxpts, dm, dis, pts, vs)
UM_int2   *isf;
UM_real8  *told;
UM_int2   *maxpts;
UM_real8  *dm;
UM_real8  *dis;
UM_real8  *pts;
UM_real8  *vs;
{
/*
..... pts (storage for point array is passed in after allocation from offcv)
..... max size of point array is passed in via maxpts
..... npts (actual points evaluated) is returned
*/
	int i,ipts,n1,npts,ix;
	UU_LIST cvpoint, cvtang;
	UM_real8 *pbuf;/*  point buffer in UU_LIST format */
	UM_real8 *tbuf;/*  tangent buffer in UU_LIST format */
	UU_REAL tol = *told;
	UU_REAL ofdis = *dis;
	UU_REAL tol1;
	UM_vector *vv;
	UM_int2 ifl,ifl264;

   /* set up bag ptr in isf-th element of evalent */
	ix = *isf - 1;

   /* evaluate curve points and tangents to given tolerance */
	if (fabs (ofdis) < tol)
		tol1 = tol;
	else
		tol1 = 2.*tol;
	n1 = ncevolvF (ix, tol1, &cvpoint, &cvtang);
	if (n1 < 2 || n1 > *maxpts) goto Done;

	npts = ncevofA (0,tol,n1,dm,ofdis,&cvpoint,&cvtang,1);
	if (npts < 2 || npts > *maxpts) goto Done;

     /* recover pointer to point UU_LIST */
	pbuf = (UU_REAL *) UU_LIST_ARRAY(&cvpoint);
     /* recover pointer to tangent UU_LIST */
	tbuf = (UU_REAL *) UU_LIST_ARRAY(&cvtang);
	vv = (UM_vector *) tbuf;

	ifl = 264;
	getifl(&ifl,&ifl264);
		
	for (ipts = 0; ipts < npts; ipts++)
	{
		for (i = 3*ipts; i < 3*ipts +3; i++)
		{
			pts[i] = pbuf[i];
			vs[i] = tbuf[i];
			if (ifl264 == 1) pts[i] *= 25.4;
		}
	}

Done:
	uu_list_free (&cvpoint);
	uu_list_free (&cvtang);

	return (npts);
}

/*********************************************************************
**    E_FUNCTION     : int ncevofA (tol,npts,dm,dis,cvpoint,cvtang,itsk)
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
**    itsk       0 for contour, 1 for NCL curve, 2 for spline
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**    cvtang     fixed list of tangent vectors 
**
**    RETURNS      : new number of pts
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncevofA (sfkey,tol,npts,dm,dis,cvpoint,cvtang,itsk)
UU_KEY_ID sfkey;
UU_REAL tol,dis;
UM_real8 *dm;  	
UU_LIST *cvpoint, *cvtang;
int npts,itsk;
{
	int i,n1,status;
	int lr = 0;
	UM_vector vmod,*vv;
	UU_REAL d;
	UM_real8 ver,dmuv[3];
	UM_int2 idx = 169;

	if (sfkey > NULLKEY)
	{
/*
.....Convert dm to uv space vector
*/
		status = ncl_dm_to_dmuv(sfkey,cvpoint,dm,dmuv);
		dmuv[2] = 0.0;
		for (i=0; i<3; i++)
			vmod[i] = dmuv[i];
	}
	else
	{
		for (i=0; i<3; i++)
			vmod[i] = dm[i];
	}

	n1 = ncl_cv_offset (sfkey,cvpoint,cvtang,npts,lr,vmod,dis,0,0.,0.,
		tol,0,0);

	if (itsk == 1)
	{
		ncl_fix_corners (cvpoint, cvtang, tol, 0, &n1);
		ncl_fix_tol (&n1, 10.*tol, cvpoint, cvtang);
	}
	else
	{
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		for (i = 0; i < n1; i++)
			um_unitvc (vv[i],vv[i]);

		ncl_fix_corners (cvpoint, cvtang, tol, 1, &n1);
		ncl_fix_tol (&n1, tol, cvpoint, cvtang);
	}

	getsc (&idx, &ver);
	if (itsk == 1 || (itsk == 2 && ver >= 9.449))
	{
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		for (i = 1; i < n1-1; i++)
		{
			d = UM_DOT(vv[i],vv[i]);
			if (d < 9.e9) 
				um_nullvc (vv[i]);
			else
				um_vctmsc (vv[i],1./sqrt(d),vv[i]);
		}
	}

	return (n1);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_offset_contour0 (tol,npts,dm,dis,cvpoint)
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
**       OUTPUT :
**    cvpoint    fixed list of evolved pts
**
**    RETURNS      : new number of pts
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_offset_contour0 (tol,npts,dm,dis,cvpoint)
UU_REAL tol,dis;
UM_vector dm;  	
UU_LIST *cvpoint;
int npts;
{
	int ipts,n1;
	int lr = 0;
	UM_coord *pp;
	UM_vector vvl;
	UU_LIST cvtang;
	UM_real8 ver;
	UM_int2 idx = 169;

	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	uu_list_init (&cvtang, sizeof(UM_vector), npts+1, npts);
	for (ipts = 0; ipts < npts-1; ipts++)
	{
		um_vcmnvc (pp[ipts+1],pp[ipts],vvl);
		uu_list_push (&cvtang,vvl);
	}
	uu_list_push (&cvtang,vvl); /* last vector is the same as the one before */

	getsc (&idx, &ver);
	if (ver < 9.449)
		n1 = ncevofA (NULLKEY,tol,npts,dm,dis,cvpoint,&cvtang,0);
	else
		n1 = ncl_cv_offset (NULLKEY,cvpoint,&cvtang,npts,lr,dm,dis,0,0.,0.,tol,0,0);
	uu_list_free (&cvtang);

	return (n1);
}
