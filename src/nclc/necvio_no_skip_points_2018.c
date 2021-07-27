/*********************************************************************
**    NAME         :  necvio.c
**       CONTAINS: Surface intersection routines.
**
**		 void		ncl_put_nios
**       void       ncl_setup_xtriangles (n,ptri,xtp)
**       void       ncl_segtri_io (p1,p2,uvc,tri1,npt1,npt2,vec,tol,isec)
**       UU_LOGICAL ncl_tritri_io (xtp1,xtp2,hpt1,hpt2,vec,tol)
**       void       frstpt (key1,key2,pte,vx,ier)
**       int        ncl_bndrpln_io (bound,nvec,d,tol)
**       int        ncl_bndrpln_io1 (pts,npt,nvec,d,tol,ib)
**       void       ncl_arrange_segs (tol)
**       void       ncl_trizeropln_io (d1,p1,d2,p2,d3,p3,npt1,npt2)
**       void       ncl_tripln_io(tri1,pln,tol,eps)
**       int        ncl_separate_components (ptlst,tol)
**       void       ncl_select_component (irpt,ptx,np)
**       void       sfplio(key1,key2,irpt,ptx,np,ier)
**       UU_LOGICAL ncl_segs_isect(p1,p2,q1,q2,r1,r2,tol)
**       void       flatio(key1,key2,irpt,ptx,np,ier)
**       int        ncl_trim_cv
**
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       necvio.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:29
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "class.h"
#include "mdrel.h"
#include "modef.h"
#include "mdeval.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "mgeom.h"
#include "uminmax.h"

ncl_ioseg *head = UU_NULL;
static UU_LIST Snios;
static int Snios_init = 0;
static UM_vector ldir;
UM_tess_settype Stesstype = UM_TESS_TOLER;

static ncl_ioseg *last;
extern UU_LIST NCL_uvintof;
extern UU_LOGICAL NCL_waterline;
extern UU_LOGICAL lfirstime;

#define CO9SQ (UU_REAL) 0.97552826

/**********************************************************************
**    E_FUNCTION     : ncl_nul_nios()
**       Initiliaze the list Snios
**    PARAMETERS
**       INPUT  :
**			none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_nul_nios()
{
	Snios.cur_cnt = 0;
}

/**********************************************************************
**    E_FUNCTION     : ncl_get_nios (num,np)
**       Free the memory for Snios 
**    PARAMETERS
**       INPUT  :
**			none
**       OUTPUT :
**          num		- number of point
**			np		- pointer to the points
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_get_nios (num,np)
int *num,**np;
{
	if (Snios_init == 1 && Snios.cur_cnt > 0)
	{
		*num = Snios.cur_cnt;
		*np = (int *) UU_LIST_ARRAY (&Snios);
	}
	else
		*np = UU_NULL;
}

/**********************************************************************
**    E_FUNCTION     : ncl_put_nios(num)
**       Put the number of point n into Snios 
**    PARAMETERS
**       INPUT  :
**          num			- number of points
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_put_nios(num)
int num;
{
	if (Snios_init == 0)
	{
		uu_list_init (&Snios, sizeof(int), 5, 5);
		Snios_init = 1;
	}
	if (Snios_init == 1) 
		uu_list_push (&Snios,&num);
}

/**********************************************************************
**    E_FUNCTION     : ncl_free_nios()
**       Free the memory for Snios 
**    PARAMETERS
**       INPUT  :
**			none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_free_nios()
{
	if (Snios_init == 1)
	{
		Snios_init = 0; uu_list_free (&Snios);
	}
}

/*********************************************************************
**    E_FUNCTION   : void ncl_setup_xtriangles (n,ptri,xtp)
**       Fill the extended triangle structure for an array of triangles.
**    PARAMETERS
**       INPUT  :
**          n       - number of triangles
**          ptri    - pointer to the array of triangles (each triangle is just
**                    three 3D-points)
**          xtp     - pointer to the array of extended triangular structures
**       OUTPUT :
**          xtp     - the extended triangular structures filled with data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_setup_xtriangles (n,ptri,xtp)
int n;
UM_trian *ptri;
NCL_xtriangle *xtp;
{
	int i;
	UU_REAL d;
/*
..... valid is TRUE iff the triangle is nondegenerate
..... v1 is the unit vector from p1 to p2
..... v2 is the unit vector from p2 to p3
..... v3 is the unit vector from p3 to p1
..... pln is the triangle's plane (normal directed as v1 x v2)
..... pl1 is the plane thru (p1,p2) perpendicular to pln, the normal pointing
..... inside the triangle; pl2,pl3 are defined similarly
..... each plane is given by its 4-tuple (a,b,c,d)
*/
	for (i = 0; i < n; i++, ptri++, xtp++)
	{
		xtp->valid = UU_TRUE;
		um_vctovc(ptri->p1, xtp->p1);
		um_vcmnvc(ptri->p2, ptri->p1, xtp->v1);
		d = UM_MAG(xtp->v1);
		if (d < UM_DFUZZ)
		{
			xtp->valid = UU_FALSE; continue;
		}
		xtp->v1[0] /= d; xtp->v1[1] /= d; xtp->v1[2] /= d;

		um_vctovc(ptri->p2, xtp->p2);
		um_vcmnvc(ptri->p3, ptri->p2, xtp->v2);
		d = UM_MAG(xtp->v2);
		if (d < UM_DFUZZ)
		{
			xtp->valid = UU_FALSE; continue;
		}
		xtp->v2[0] /= d; xtp->v2[1] /= d; xtp->v2[2] /= d;

		um_vctovc(ptri->p3, xtp->p3);
		um_vcmnvc(ptri->p1, ptri->p3, xtp->v3);
		d = UM_MAG(xtp->v3);
		if (d < UM_DFUZZ)
		{
			xtp->valid = UU_FALSE; continue;
		}
		xtp->v3[0] /= d; xtp->v3[1] /= d; xtp->v3[2] /= d;

		um_cross(xtp->v1, xtp->v2, xtp->pln);
		d = UM_MAG(xtp->pln);
		if (d < UM_DFUZZ)
		{
			xtp->valid = UU_FALSE; continue;
		}
		xtp->pln[0] /= d; xtp->pln[1] /= d; xtp->pln[2] /= d;
		xtp->pln[3] = UM_DOT (xtp->pln,xtp->p1);

		um_cross(xtp->pln, xtp->v1, xtp->pl1);
		d = UM_MAG(xtp->pl1);
		if (d < UM_DFUZZ)
		{
			xtp->valid = UU_FALSE; continue;
		}
		xtp->pl1[0] /= d; xtp->pl1[1] /= d; xtp->pl1[2] /= d;
		xtp->pl1[3] = UM_DOT (xtp->pl1,xtp->p1);

		um_cross(xtp->pln, xtp->v2, xtp->pl2);
		d = UM_MAG(xtp->pl2);
		if (d < UM_DFUZZ)
		{
			xtp->valid = UU_FALSE; continue;
		}
		xtp->pl2[0] /= d; xtp->pl2[1] /= d; xtp->pl2[2] /= d;
		xtp->pl2[3] = UM_DOT (xtp->pl2,xtp->p2);

		um_cross(xtp->pln, xtp->v3, xtp->pl3);
		d = UM_MAG(xtp->pl3);
		if (d < UM_DFUZZ)
		{
			xtp->valid = UU_FALSE; continue;
		}
		xtp->pl3[0] /= d; xtp->pl3[1] /= d; xtp->pl3[2] /= d;
		xtp->pl3[3] = UM_DOT (xtp->pl3,xtp->p3);
	}

	return;
}

/*********************************************************************
**    E_FUNCTION   : void ncl_segtri_io(p1,p2,uvc,tri1,npt1,npt2,vec,tol,isec)
**       Intersect a segment with a triangle.  If 'p1' is the same as
**       'p2', then an infinite segment will be used from p1 along the
**       vector specified by 'uvc'.
**    PARAMETERS
**       INPUT  :
**          p1      - first segment point
**          p2      - second segment point
**          uvc     - unit vector along the segment
**          tri1    - triangle
**          npt1    - first intersection point, ignored if isec=0
**          tol     - tolerance
**          isec    - no intersection known if 0; one intersection point npt1
**                    already known if 1.
**       OUTPUT :
**          isec    - the number of different intersections found.
**          npt1    - first intersection point, if isec >= 1.
**          npt2    - second intersection point, if isec = 2.
**          vec     - unit vector from npt1 to npt2, if isec = 2.
**                    unit vector of triangle normal, if isec = 1.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_segtri_io(p1,p2,uvc,tri1,npt1,npt2,vec,tol,isec)
NCL_xtriangle *tri1;
UM_coord p1,p2,npt1,npt2;
UM_vector uvc,vec;
UU_REAL tol;
int *isec;
{
	UU_REAL d1,d2,d3;
	int nint = 1;
	UM_coord pt;

	d1 = UM_DOT (p1,tri1->pln) - tri1->pln[3];
	d2 = UM_DOT (p2,tri1->pln) - tri1->pln[3];

	if (!tri1->valid)
		nint = 0;
	else if (fabs (d1) < tol/4)
		um_vctovc (p1,pt);
	else if (fabs (d2) < tol/4)
		um_vctovc (p2,pt);
	else if (d1*d2 < 0. || um_dcccc(p1,p2) == 0.)
		um_ilnpln(p1,uvc,tri1->p1,tri1->pln,&nint,pt);
	else
		nint = 0;

	if (nint == 0) return;

	d1 = UM_DOT (pt,tri1->pl1) - tri1->pl1[3];
	d2 = UM_DOT (pt,tri1->pl2) - tri1->pl2[3];
	d3 = UM_DOT (pt,tri1->pl3) - tri1->pl3[3];

	if (d1 + tol <= 0. || d2 + tol <= 0. || d3 + tol <= 0.) return;
/*
.....First intersection
*/
	if (*isec == 0)
	{
		*isec = 1;
		um_vctovc(pt,npt1);
		um_vctovc(tri1->pln,vec);
	}
/*
.....Second intersection
*/
	else
	{
		um_vcmnvc (pt,npt1,vec);
		d1 = UM_MAG (vec);
		if (d1 > tol)
		{
			um_vctovc (pt,npt2);
			vec[0] /= d1; vec[1] /= d1; vec[2] /= d1;
			*isec = 2;
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     :  UU_LOGICAL ncl_tritri_io(xtp1,xtp2,npti,vec,tol)
**       Calculate a nondegenerate intersection of 2 triangles
**    PARAMETERS
**       INPUT  :
**          tri1    - First triangle
**          tri2    - Second triangle
**          tol     - tolerance
**       OUTPUT :
**          npti    - Middle intersection point
**          vec     - unit vector along the intersection
**    RETURNS      : TRUE if a nondegenerate intersection exists, else FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL ncl_tritri_io(tri1,tri2,npti,vec,tol)
NCL_xtriangle *tri1,*tri2;
UM_coord npti;
UM_vector vec;
UU_REAL tol;
{
	int isec = 0;
	UM_coord npt1,npt2;

	ncl_segtri_io(tri1->p1,tri1->p2,tri1->v1,tri2,npt1,npt2,vec,tol,&isec);
	ncl_segtri_io(tri1->p2,tri1->p3,tri1->v2,tri2,npt1,npt2,vec,tol,&isec);
	if (isec <= 1)
	{
		ncl_segtri_io(tri1->p3,tri1->p1,tri1->v3,tri2,npt1,npt2,vec,tol,&isec);
		if (isec <= 1)
		{
			ncl_segtri_io(tri2->p1,tri2->p2,tri2->v1,tri1,npt1,npt2,vec,tol,&isec);
			if (isec <= 1)
			{
				ncl_segtri_io(tri2->p2,tri2->p3,tri2->v2,tri1,npt1,npt2,vec,tol,
							&isec);
				if (isec <= 1)
					ncl_segtri_io(tri2->p3,tri2->p1,tri2->v3,tri1,npt1,npt2,vec,tol,
							&isec);
			}
		}
	}

	if (isec > 1)
	{
		um_middlept (npt1,npt2,npti);
		return (UU_TRUE);
	}
	else
		return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     :  void frstpt(key1,key2,pte,vx,ier)
**       Calculate a first point and a forward direction for a surf/surf
**       (or surf/plane) intersection problem.
**    PARAMETERS
**       INPUT  :
**          key1    - First surface key
**          key2    - Second surface/plane key
**       OUTPUT :
**          pte     - Common point
**          vx      - Direction toward another (close) common point
**          ier     - Error flag
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void frstpt(key1,key2,pte,vx,ier)
UM_int4 *key1,*key2;
UM_real8 *pte,*vx;
UM_int2 *ier;
{
	struct NCL_fixed_databag e1, e2;
	int i,j,n1,n2,status = UU_SUCCESS;
	UM_real8 tol8;
	UM_tessellation tess1,tess2;
	UU_LIST trian1,trian2;
	UM_trian *ptri1, *ptri2;
	NCL_xtriangle *xt1, *xt2, *xtp1, *xtp2;
	UU_REAL tol,dtol;
	UM_coord hpti;
	UM_vector vec;
	UU_LOGICAL found;
	UM_int2 idx = 175,type = NCLI_POINT;
/*
.....Initialize routine
*/
	um_init_tess(&tess1);
	um_init_tess(&tess2);
	uu_list_init0 (&trian1);
	uu_list_init0 (&trian2);
	xt1 = UU_NULL;
	xt2 = UU_NULL;

	gettol (&tol8);
	tol = tol8;
	getsc (&idx,&tol8);
	dtol = tol8;

	e1.key = *key1;
	status = ncl_retrieve_data_fixed (&e1);
	if (status != UU_SUCCESS) goto Done;
	e2.key = *key2;
	status = ncl_retrieve_data_fixed (&e2);
	if (status != UU_SUCCESS) goto Done;

	ncl_set_boundary_toler (dtol);
	ncl_set_tess_parms(Stesstype,dtol,100,100);
	um_set_tess_toler (dtol);

	if (status == UU_SUCCESS)
	{
		status = ncl_get_tesslst (&e1,&tess1);
		if (status != UU_SUCCESS) status = ncl_tessellate_surf (&e1,&tess1);
	}
	if (status == UU_SUCCESS)
		status = ncl_get_tess_triangles (&tess1,&trian1,0,0);
	if (status != UU_SUCCESS) goto Done;

	if (status == UU_SUCCESS)
	{
		status = ncl_get_tesslst (&e2,&tess2);
		if (status != UU_SUCCESS) status = ncl_tessellate_surf (&e2,&tess2);
	}
	if (status == UU_SUCCESS)
		status = ncl_get_tess_triangles (&tess2,&trian2,0,0);
	if (status != UU_SUCCESS) goto Done;

	n1 = trian1.cur_cnt;
	n2 = trian2.cur_cnt;
	ptri1 = (UM_trian *)UU_LIST_ARRAY(&trian1);
	ptri2 = (UM_trian *)UU_LIST_ARRAY(&trian2);
	xtp1 = xt1 = (NCL_xtriangle *)uu_malloc(n1*sizeof(*xt1));
	xtp2 = xt2 = (NCL_xtriangle *)uu_malloc(n2*sizeof(*xt2));
	if (!xt1 || !xt2) status = UU_FAILURE;
	if (status != UU_SUCCESS) goto Done;

	ncl_setup_xtriangles (n1,ptri1,xtp1);
	ncl_setup_xtriangles (n2,ptri2,xtp2);

	found = UU_FALSE;
	for (i = 0, xtp1 = xt1; i < n1; i++, xtp1++)
	{
		if (xtp1->valid)
		{
			for (j = 0, xtp2 = xt2; j < n2; j++, xtp2++)
			{
				if (xtp2->valid)
				{
					found = ncl_tritri_io(xtp1,xtp2,hpti,vec,tol);
					if (found) goto Done;
				}
			}
		}
	}

Done:
	if (status != UU_SUCCESS || !found)
	{
		if (*ier == 0) *ier = 163;
	}
	else
	{
		fr_unbs (hpti,hpti,&type);
		pte[0] = hpti[0]; pte[1] = hpti[1]; pte[2] = hpti[2];
		type = NCLI_VECTOR;
		fr_unbs (vec,vec,&type);
		vx[0] = vec[0]; vx[1] = vec[1]; vx[2] = vec[2];
	}
	uu_list_free(&trian1);
	uu_list_free(&trian2);
	um_free_tess(&tess1);
	um_free_tess(&tess2);
	if (xt1 != UU_NULL) uu_free(xt1);
	if (xt2 != UU_NULL) uu_free(xt2);

	return;
}

/*********************************************************************
*********************************************************************/
void ncl_set_ldir (vec)
UM_vector vec;
{
	ldir[0] = vec[0]; ldir[1] = vec[1]; ldir[2] = vec[2];
	return;
}

/*********************************************************************
**    E_FUNCTION: um_cmp (pt1,pt2)
**       Comparison routine for the sort algorithm (uu_qsort): compares
**       projections onto a (global) vector
**    PARAMETERS
**       INPUT  :
**          pt1     - first element to be compared
**          pt2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if pt1 < pt2
**                     0 if pt1 = pt2
**                     1 if pt1 > pt2
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int cmp(pt1,pt2)
UM_coord pt1,pt2;
{
	UU_REAL d1,d2;
	d1 = UM_DOT (pt1,ldir); d2 = UM_DOT (pt2,ldir);

   if (d1 > d2) return(1);

   else if (d1 < d2) return(-1);

   else return(0);
}

/*********************************************************************
**    FUNCTION : int ncl_pt_in_contour (pp,np,vx,vy,ptt,tol)
**
**    Determines if a point is inside a polygon (more exactly, the polygon's
**    projection on the (vx,vy) plane). Borrowed from "Computational
**    geometry in C" by O'Rourke.
**
**    PARAMETERS
**       INPUT  :
**          np  - number of points in polygon
**          pp  - the polygon points
**          vx  - the 'X' direction
**          vx  - the 'Y' direction
**          ptt - point in question
**          tol - tolerance
**       OUTPUT :
**          none
**
**    RETURNS  : 1 if point is in polygon;
**               0 if point is on polygon boundary;
**              -1 if point is outside polygon
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pt_in_contour (pp,np,vx,vy,ptt,tol)
UM_coord pp[];
int np;
UM_vector vx,vy;
UU_REAL tol;
UM_coord ptt;
{
	int ins;
	UU_REAL tolsq = tol*tol;

	ins = um_pt_in_contour (pp,np,vx,vy,ptt,tol,tolsq);

	return (ins);
}

/*********************************************************************
**    E_FUNCTION:  ncl_bndrpln_io (nb,npo,np,cvpts,nvec,d,tol)
**          Intersects surface boundary with a plane - in 3D
**    PARAMETERS
**       INPUT  :
**          nb     - number of boundary curves
**          npo    - number of points in outer boundary
**          np     - number of points in inner boundaries
**          nvec   - plane normal vector
**          d      - plane distance parameter
**          tol    - tolerance
**       OUTPUT :
**          NCL_uvintof contains intersection points
**          Snios contains number of points in each connected component
**    RETURNS      :  UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_bndrpln_io (nb,npo,np,cvpts,nvec,d,tol)
int nb,npo,*np;
UU_LIST *cvpts;
UU_REAL d,tol;
UM_vector nvec;
{
	int i, ix, ni, n;
	UM_coord *pts;

	if (npo < 4) return (-1);
	pts = (UM_coord *) UU_LIST_ARRAY (cvpts);
/*
..... intersect plane with all closed boundaries
*/
	i = ni = 0;
	n = npo;
	ni = ncl_bndrpln_io1 (pts,n,nvec,d,tol,i);
	if (ni%2 == 1) return (-1);
	ix = n;

	for (i = 1; i < nb; i++)
	{
		n = abs(np[i-1]);
		ni += ncl_bndrpln_io1 (pts[ix],n,nvec,d,tol,i);
		if (ni%2 == 1) return (-1);
		ix += n;
	}
/*
..... sort all points in buffer
*/
	ncl_getpts_uv (&ni,&pts);

	if (ni >= 3 || (NCL_waterline == UU_FALSE && ni == 2))
		uu_qsort (pts,ni,sizeof(UM_coord),cmp);
	if (ni >= 3)
	{
		if (Snios_init == 0)
		{
			uu_list_init (&Snios, sizeof(int), ni/2, ni/2);
			Snios_init = 1;
		}
		n = 2;
		for (i = 0; i < ni/2; i++)
			uu_list_push (&Snios,&n);
	}

	return (0);
}

/*********************************************************************
**    E_FUNCTION:  ncl_bndrpln_io1 (pts,npt,nvec,d,tol,ib)
**       Intersects surface boundary component  with a plane - in 3D
**    PARAMETERS
**       INPUT  :
**          pts    - array of points representing closed curve
**          npt    - number of points in pts array.
**          nvec   - plane normal vector
**          d      - plane distance parameter
**          tol    - tolerance
**          ib     - outer boundary if 0; else inner
**       OUTPUT :
**          NCL_uvintof contains intersection points
**          Snios contains number of points in each connected component
**    RETURNS      :  total number of intersection points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_bndrpln_io1 (pts,npt,nvec,d,tol,ib)
UM_coord pts[];
int npt,ib;
UM_vector nvec;
UU_REAL d,tol;
{
	int i,j,j0,j1,k,l,np1,num;
	UU_REAL d0,d1,del;
	UM_coord pt1,pt2;
	UU_LOGICAL lv93;

	np1 = npt-1;
	if (np1 < 3) return (0);
	k = 0;
	num = 0;

	lv93 = ncl_setver(93);

/*
..... find the first point not on the plane
*/
	d0 = UM_DOT (pts[0],nvec) - d;
	if (fabs (d0) <= tol)
	{
		for (j = np1-1; j > 0; j--)
		{
			d1 = UM_DOT (pts[j],nvec) - d;
			if (fabs(d1) > tol) break;
		}
		if (j < 1) return (0);
		if (ib == 0)
			k = np1 - j;
		else
			k = np1 - j + 1;
	}

	for (i = 0; i < np1; i++)
	{
		j = um_mod(i-k,np1);
		d0 = UM_DOT (pts[j],nvec) - d;
		if (fabs (d0) <= tol)
		{
/*
..... current point is on the plane.
*/
			um_vctovc (pts[j],pt1);
			d1 = UM_DOT (pts[j+1],nvec) - d;
			if (i > 0 && fabs(d1) > tol)
			{
/*
..... if the next is not, we add the current point if the previous
..... and the next are on different sides of the plane.
*/
				j0 = um_mod(i-1-k,np1);
				del = UM_DOT (pts[j0],nvec) - d;
				if (fabs(del) > tol && del * d1 < 0.)
				{
					num++; ncl_push_uv (pt1);
				}
				continue;
			}
			while (fabs(d1) <= tol)
			{
				i++; j = um_mod(i-k,np1); d1 = UM_DOT (pts[j+1],nvec) - d;
			}
			um_vctovc (pts[j],pt2);
			del = UM_DCCCC(pt1,pt2);
			if (del > tol)
			{
				UM_vector vv;
				UM_coord ptt;
				int idel,iret;

/*
..... The next point(s) is also on the plane. We have an edge (pt1,pt2) on
..... the plane. We add an edge endpoint if the 'just beyond' extention will
..... be 'outside' the polygon
*/
				for (l = 0; l < 3; l++)
					vv[l] = (pt2[l] - pt1[l])/del;

				for (idel = 5, iret = 0; iret == 0; idel += 5)
				{
					for (l = 0; l < 3; l++)	ptt[l] = pt1[l] - idel*vv[l]*tol;
					iret = ncl_pt_in_contour (pts,npt,vv,nvec,ptt,tol);
				}
				if (iret == -1)
				{
					num++; ncl_push_uv (pt1);
				}

				for (idel = 5, iret = 0; iret == 0; idel += 5)
				{
					for (l = 0; l < 3; l++)	ptt[l] = pt2[l] + idel*vv[l]*tol;
					iret = ncl_pt_in_contour (pts,npt,vv,nvec,ptt,tol);
				}
				if (iret == -1)
				{
					num++; ncl_push_uv (pt2);
				}
			}
		}
		else
		{
			j1 = -1;

			j0 = j;
			d1 = UM_DOT (pts[j+1],nvec) - d;
			while (ib > 0 && i < np1 && fabs(d1) <= tol)
			{
				if (fabs(d1) < 0.1*tol && !lv93)
				{
					j1 = j+1; um_vctovc (pts[j1],pt1);
				}
				i++;
				j = um_mod(i-k,np1);
				d1 = UM_DOT (pts[j+1],nvec) - d;
			}
			if (fabs (d1) > tol && d0 * d1 < 0.)
			{
				num++;
				if (ib > 0 && j1 >= 0)
				{
					UM_vector v0,v1;

					for (l = 0; l < 3; l++)
					{
						v0[l] = pts[j1][l] - pts[j0][l];
						v1[l] = pts[j1][l] - pts[j+1][l];
					}
					del = UM_DOT(v0,v1) - UM_DOT(v0,nvec)*UM_DOT(v1,nvec);
/*
..... if a vertex is exactly on the plane and forms a convex angle, use it
*/
					if (del > tol*tol)
					{
						ncl_push_uv (pt1); continue;
					}
				}
				del = d1 - d0;
				if (lv93)
					um_avcplbvc (-d0/del, pts[j+1], d1/del, pts[j], pt1);
				else
					um_avcplbvc (-d0/del, pts[j+1], d1/del, pts[j0], pt1);
				ncl_push_uv (pt1);
			}
		}
	}

	if (num%2 == 1)
	{
		NCL_uvintof.cur_cnt -= (3*num);
		if (!ncl_setver(95) &&  NCL_waterline) num--;
	}

	return (num);
}

/*********************************************************************
**    E_FUNCTION   : int  ncl_arrange_segs (tol)
**       Arrange a segment; put the resulting connected pieces in the
**       intersection lists.
**    PARAMETERS
**       INPUT  :
**          tol     - tolerance
**          global head points at the first segment
**       OUTPUT :
**          NCL_uvintof, Snios contain the updated intersection data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void  ncl_arrange_segs (tol)
UU_REAL tol;
{
	UU_REAL eps = tol*tol,d,d1,dmin;
	int found,i,n;
	ncl_ioseg *cur,*next,*before,*prev;
	UM_coord pt1;

	for (cur = head; cur != UU_NULL; cur = cur->next)
	{
		prev = cur;
		for (next = cur->next; next != UU_NULL; )
		{
			d = UM_SQDIS (cur->pt2,next->pt1);
			d1 = UM_SQDIS (cur->pt1,next->pt2);
			if (d < eps && d1 < eps)
			{
				prev->next = next->next; uu_free (next); next = prev->next;
				break;
			}
			d = UM_SQDIS (cur->pt1,next->pt1);
			d1 = UM_SQDIS (cur->pt2,next->pt2);
			if (d < eps && d1 < eps)
			{
				prev->next = next->next; uu_free (next); next = prev->next;
				break;
			}
			prev = next; next = next->next;
		}
	}
/*
.....With tight tolerance, the value for eps is too tight and so bad results
.....can occur - ASF 1/20/14 (QAR 100136).
*/
	for (cur = head, n = 2; cur != UU_NULL; )
	{
		before = prev = cur;
		found = 0;
		dmin = 5.*eps;
		next = cur->next;
		while (next != UU_NULL)
		{
			d = UM_SQDIS (cur->pt2,next->pt1);
			if (d < dmin)
			{
				found = 1; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			d = UM_SQDIS (cur->pt2,next->pt2);
			if (d < dmin)
			{
				found = 2; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			d = UM_SQDIS (head->pt1,next->pt2);
			if (d < dmin)
			{
				found = -1; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			d = UM_SQDIS (head->pt1,next->pt1);
			if (d < dmin)
			{
				found = -2; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			prev = next; next = next->next;
		}
		if (found != 0)
		{
			n++;
			next = before->next;
			if (found == 2 || found == -2)
			{
				for (i = 0; i < 3; i++)
				{
					d = next->pt1[i];
					next->pt1[i] = next->pt2[i];
					next->pt2[i] = d;
				}
			}
			before->next = next->next;
			if (found > 0)
			{
/*
..... insert next after current end
*/
				next->next = cur->next;
				cur->next = next;
				cur = next;
			}
			else
			{
/*
..... insert next before current start
*/
				next->next = head;
				head = next;
			}
		}
		else
		{
			ncl_push_uv (head->pt1);
			um_vctovc (head->pt1,pt1);
			for (i = 0; i < n; i++)
			{
				if (UM_SQDIS (pt1,head->pt2) > eps ||
					(n > 2 && i == n-1))
				{
					ncl_push_uv (head->pt2);
					um_vctovc (head->pt2,pt1);
				}
				else
				{
					i--; n--;
				}
				next = head->next;
				if (head == cur) i = n-1;
				uu_free (head);
				head = next;
			}
			if (n < 2)
				NCL_uvintof.cur_cnt -= 3;
			else
			{
				if (Snios_init == 0 && head != UU_NULL)
				{
					uu_list_init (&Snios, sizeof(int), 5, 5);
					Snios_init = 1;
				}
				if (Snios_init == 1) uu_list_push (&Snios,&n);
			}
			cur = head;
			n = 2;
		}
	}
	while (head != UU_NULL)
	{
		next = head->next;
		uu_free (head);
		head = next;
	}
	return;
}

/*********************************************************************
**    E_FUNCTION   : void ncl_trizeropln_io (d1,p1,d2,p2,d3,p3,npt1,npt2)
**       Intersect a triangle with a plane. This routine assumes the segment
**       (p1,p2) intersects the plane.
**    PARAMETERS
**       INPUT  :
**          p1      - first triangle point
**          d1      - signed distance from pt1 to the plane
**          p2,d2   - second triangle point with distance
**          p3,d3   - third triangle point with distance
**       OUTPUT :
**          npt1    - first intersection point, if isec >= 1.
**          npt2    - second intersection point, if isec = 2.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_trizeropln_io (d1,p1,d2,p2,d3,p3,npt1,npt2)
UU_REAL d1,d2,d3;
UM_coord p1,p2,p3,npt1,npt2;
{
	UU_REAL d;

	d = d2 - d1;
	if (fabs (d) < UM_DFUZZ)
	{
		um_vctovc (p1,npt1);
		um_vctovc (p2,npt2);
		return;
	}
	um_avcplbvc (-d1/d, p2, d2/d, p1, npt1);
	if (d1 * d3 < 0.)
	{
		d = d3 - d1;
		if (fabs (d) < UM_DFUZZ)
		{
			um_vctovc (p3,npt2); return;
		}
		um_avcplbvc (-d1/d, p3, d3/d, p1, npt2);
	}
	else if (d2 * d3 < 0.)
	{
		d = d3 - d2;
		if (fabs (d) < UM_DFUZZ)
		{
			um_vctovc (p3,npt2); return;
		}
		um_avcplbvc (-d2/d, p3, d3/d, p2, npt2);
	}
	else
		um_vctovc (p3,npt2);

	return;
}

/*********************************************************************
**    E_FUNCTION   : void ncl_tripln_io(tri1,pln,tol,eps)
**       Intersect a plane with a triangle
**    PARAMETERS
**       INPUT  :
**          tri1    - triangle
**          pt		- plane point
**			nvec	- plane normal
**          tol     - tolerance used to decide if there is an intersection
**          eps     - (squared) tolerance used to decide if the intersection
**                    is long enough to be counted
**       OUTPUT :
**          npt1    - first intersection point
**          npt2    - second intersection point
**    RETURNS      : UU_TRUE iff found a nondegenerate intersection
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_tripln_io0(tri1,pt,nvec,tol,eps,lSort)
UM_trian *tri1;
UM_coord pt;
UM_vector nvec;
UU_REAL tol,eps;
UU_LOGICAL lSort;
{
	UU_REAL d,d1,d2,d3,ad1,ad2,ad3;
	UM_coord npt1,npt2,npts[2];
	ncl_ioseg *segm;
	UU_LOGICAL lv97;

	d = UM_DOT (pt,nvec);
	d1 = UM_DOT (tri1->p1,nvec) - d;
	d2 = UM_DOT (tri1->p2,nvec) - d;
	d3 = UM_DOT (tri1->p3,nvec) - d;
	ad1 = fabs(d1); ad2 = fabs(d2); ad3 = fabs(d3);

	if (ad1 <= tol && ad2 <= tol && ad3 <= tol) return;
	if (ad1 > tol && ad2 > tol && d1 * d2 < 0.)
	{
		ncl_trizeropln_io (d1,tri1->p1,d2,tri1->p2,d3,tri1->p3,npt1,npt2);
		if (ad3 <= tol) um_vctovc (tri1->p3,npt2);
	}
	else if (ad1 > tol && ad3 > tol && d1 * d3 < 0.)
	{
		ncl_trizeropln_io (d1,tri1->p1,d3,tri1->p3,d2,tri1->p2,npt1,npt2);
		if (ad2 <= tol) um_vctovc (tri1->p2,npt2);
	}
	else if (ad2 > tol && ad3 > tol && d2 * d3 < 0.)
	{
		ncl_trizeropln_io (d2,tri1->p2,d3,tri1->p3,d1,tri1->p1,npt1,npt2);
		if (ad1 <= tol) um_vctovc (tri1->p1,npt2);
	}
	else
	{
		UU_REAL d12,d13,d23;
		d12 = MAX2 (ad1,ad2); d13 = MAX2 (ad1,ad3); d23 = MAX2 (ad2,ad3);
		if (d12 <= ad3)
		{
			if (d12 > tol) return;
			um_vctovc (tri1->p1,npt1); um_vctovc (tri1->p2,npt2);
		}
		else if (d13 <= ad2)
		{
			if (d13 > tol) return;
			um_vctovc (tri1->p1,npt1); um_vctovc (tri1->p3,npt2);
		}
		else
		{
			if (d23 > tol) return;
			um_vctovc (tri1->p2,npt1); um_vctovc (tri1->p3,npt2);
		}
	}

	d = UM_SQDIS (npt1, npt2);
	if (d >= eps ||
		(ad1 > 10.*tol && ad2 > 10.*tol && ad3 > 10.*tol &&
			ad1+ad2+ad3 > 100.*tol && d > 0.04*eps))
	{
		segm = (ncl_ioseg *) uu_malloc (sizeof (ncl_ioseg));
		segm->next = UU_NULL;
			
		lv97 = ncl_setver(97);
		if (!lfirstime && NCL_waterline == UU_FALSE && !lv97)
		{			
			lfirstime = UU_TRUE;
			um_vctovc (npt1,npts[0]);
			um_vctovc (npt2,npts[1]);
			uu_qsort (npts,2,sizeof(UM_coord),cmp);	
			um_vctovc (npts[0],segm->pt1);
			um_vctovc (npts[1],segm->pt2);
		}
		else
		{
			if (lSort)
			{
				um_vctovc (npt1,npts[0]);
				um_vctovc (npt2,npts[1]);
				uu_qsort (npts,2,sizeof(UM_coord),cmp);	
				um_vctovc (npts[0],segm->pt1);
				um_vctovc (npts[1],segm->pt2);
			}
			else
			{
				um_vctovc (npt1,segm->pt1); 
				um_vctovc (npt2,segm->pt2);
			}
		}

		if (head == UU_NULL)
			head = segm;
		else
			last->next = segm;
		last = segm;
	}

	return;
}

/*********************************************************************
**    E_FUNCTION   : void ncl_tripln_io(tri1,pln,tol,eps)
**       Intersect a plane with a triangle
**    PARAMETERS
**       INPUT  :
**          tri1    - triangle
**          pln1    - plane
**          tol     - tolerance used to decide if there is an intersection
**          eps     - (squared) tolerance used to decide if the intersection
**                    is long enough to be counted
**       OUTPUT :
**          npt1    - first intersection point
**          npt2    - second intersection point
**    RETURNS      : UU_TRUE iff found a nondegenerate intersection
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_tripln_io(tri1,pln,tol,eps)
struct NCL_nclpl_rec *pln;
UM_trian *tri1;
UU_REAL tol,eps;
{
	ncl_tripln_io0(tri1,pln->pt,pln->nvec,tol,eps,UU_FALSE);
}

/*********************************************************************
**       Check if two segments form a corner
*********************************************************************/
static UU_LOGICAL S_corner (v0,d0,v1,d1,eps)
UU_REAL eps,d0,d1;
UM_vector v0,v1;
{
	UU_REAL co;

	if (d0 > eps && d1 > eps)
	{
		co = UM_DOT(v0,v1);
		if (co <= 0) return (UU_TRUE);
		if (co*co < CO9SQ*d0*d1) return (UU_TRUE);
	}

	return (UU_FALSE);
}

/*********************************************************************
**       Shuffle a closed curve so that it starts at inx
*********************************************************************/
static void S_shuffle (uvlst,inx,npt,pts)
UU_LIST *uvlst;
UM_coord *pts;
int inx,npt;
{
	int i;
	UM_coord *vtx;

	UU_LIST_EMPTY (uvlst);

	for (i = inx; i < npt-1; i++)
		uu_list_push (uvlst,pts[i]);

	for (i = 0; i <= inx; i++)
		uu_list_push (uvlst,pts[i]);

	vtx = (UM_coord *) UU_LIST_ARRAY (uvlst);
	for (i = 0; i < npt; i++)
		um_vctovc (vtx[i],pts[i]);
}

/*********************************************************************
**       Check if there is a corner at 0
*********************************************************************/
static UU_LOGICAL S_corner_at0 (npt,pts,eps)
UM_coord *pts;
int npt;
UU_REAL eps;
{
	UU_REAL d0,d1;
	UM_vector v0,v1;

	um_vcmnvc (pts[1],pts[0],v0);
	d0 = UM_DOT (v0,v0);

	um_vcmnvc (pts[npt-1],pts[npt-2],v1);
	d1 = UM_DOT (v1,v1);

	return (S_corner(v0,d0,v1,d1,eps));
}

/*********************************************************************
**       Find a corner starting at i0
*********************************************************************/
static int S_find_corner(i0,npt,pts,eps)
UM_coord *pts;
int i0,npt;
UU_REAL eps;
{
	int i;
	UU_REAL d0,d1;
	UM_vector v0,v1;

	um_vcmnvc (pts[i0+1],pts[i0],v0);
	d0 = UM_DOT (v0,v0);

	for (i = i0+1; i < npt-1; i++)
	{
		um_vcmnvc (pts[i+1],pts[i],v1);
		d1 = UM_DOT (v1,v1);
		if (S_corner(v0,d0,v1,d1,eps)) return (i);
		d0 = d1;
		um_vctovc (v1,v0);
	}

	return (i0);
}

/*********************************************************************
**    S_FUNCTION     :  int S_separate_comp (c0,npt,pts,ptlst,uvlst,tol)
**       Separate the list of points at sharp angles.
**    PARAMETERS
**       INPUT  :
**          c0       - current component
**          pts      - closed polyline
**          npt      - number of pts points
**          uvlst    - initialized list of UM_coord to use as storage
**          tol          - tolerance
**       OUTPUT :
**          ptlst    - resulting list of points
**          Snios    - list of components updated
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_separate_comp (c0,npt,pts,ptlst,uvlst,tol)
UM_coord *pts;
int c0,npt;
UU_LIST *ptlst,*uvlst;
UU_REAL tol;
{
	int *nio;
	int iadd,inx,i0,i,nv;
	UU_REAL dd,tolsq,eps;
	UU_LOGICAL lpush;

	tolsq = tol*tol;
	eps = 9*tolsq;
	i0 = 0;

	iadd = 0;
	if (npt < 4) return (iadd);
/*
..... close the curve if needed
*/
	dd = UM_SQDIS (pts[0],pts[npt-1]);
	if (dd > eps) return (iadd);
	if (dd > tolsq)
		um_vctovc (pts[0],pts[npt-1]);

	inx = S_find_corner(i0,npt,pts,eps);
	if (inx <= 0)
	{
		uu_list_push_multiple (ptlst,npt,pts);
		return (iadd);
	}
/*
..... make sure the curve starts at a corner (if there are corners)
*/
	if (!S_corner_at0 (npt,pts,eps))
	{
		S_shuffle (uvlst,inx,npt,pts);
		inx = S_find_corner(i0,npt,pts,eps);
		if (inx <= 0)
		{
			uu_list_push_multiple (ptlst,npt,pts);
			return (iadd);
		}
	}
	lpush = (Snios_init == 0);
/*
..... insert a point at each new corner, store the number of points of the
..... last component
*/
	while (inx > 0)
	{
		for (i = i0, nv = 0; i <= inx; i++, nv++)
		{
			uu_list_push (ptlst,pts[i]);
		}
		if (i0 == 0)
		{
			if (Snios_init == 0)
			{
				if (c0 > 0) return (iadd);
				uu_list_init (&Snios, sizeof(int), 5, 5);
				Snios_init = 1;
				uu_list_push (&Snios,&nv);
			}
			else
			{
				nio = (int *) UU_LIST_ARRAY (&Snios);
				nio[c0] = nv;
			}
		}
		else
		{
			iadd++;
			if (lpush)
				uu_list_push (&Snios,&nv);
			else
				uu_list_insert (&Snios, c0+iadd, &nv);
		}
		i0 = inx;

		inx = S_find_corner(i0,npt,pts,eps);
		if (inx <= i0) break;
	}

	if (inx < npt-1)
	{
		for (i = i0, nv = 0; i < npt; i++, nv++)
		{
			uu_list_push (ptlst,pts[i]);
		}
		iadd++;
		if (lpush)
			uu_list_push (&Snios,&nv);
		else
			uu_list_insert (&Snios, c0+iadd, &nv);
	}

	return (iadd);
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_separate_components (ptlst,tol)
**       Separate the list of points at sharp angles.
**    PARAMETERS
**       INPUT  :
**          NCL_uvintof  - current list of points
**          uvlst        - initialized list of UM_coord to use as storage
**          tol          - tolerance
**       OUTPUT :
**          ptlst    - resulting list of points
**          Snios    - list of components
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_separate_components (ptlst,uvlst,tol)
UU_LIST *ptlst,*uvlst;
UU_REAL tol;
{
	UM_coord *pts;
	int *nio;
	int iadd,c,npt,nc;

	nc = 1;
	if (Snios_init == 1 && Snios.cur_cnt > 0)
	{
		nc = Snios.cur_cnt;
		nio = (int *) UU_LIST_ARRAY (&Snios);
	}
	ncl_getpts_uv (&npt,&pts);

	for (c = 0; c < nc; c++)
	{
		if (nc > 1) npt = nio[c];
		iadd = S_separate_comp (c,npt,pts,ptlst,uvlst,tol);
		if (iadd > 0)
		{
			c += iadd; nc += iadd;
			nio = (int *) UU_LIST_ARRAY (&Snios);
		}
		pts += npt;
	}
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_select_component1 (itsk,ptx,np,tol,ltrans)
**       Select the best intersection component.
**    PARAMETERS
**       INPUT  :
**          itsk    - if 0: use near point,
**                    if 1: select segment with most points,
**                    if 2: select longest segment
**                    if 3: select first segment
**          tol     - tolerance
**          ptx     - near point, ignored if itsk != 0
**          ltrans  - if true, points need to be transformed fr_unbs
**       OUTPUT :
**          np      - number of points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_select_component1 (itsk,ptx,np,tol,ltrans)
int itsk;
UM_real8 *ptx;
int *np;
UU_REAL tol;
UU_LOGICAL ltrans;
{
	UM_coord *pt;
	int *nio;
	int imin,i,j,k,npt,nmax;
	UM_coord nearpt;
	UM_vector vec;
	UU_REAL d,dmin,dmax;
	UU_REAL eps = tol*tol;
	int chunks = Snios.cur_cnt;
	static UM_int2 type = NCLI_POINT;

	nio = (int *) UU_LIST_ARRAY (&Snios);
	ncl_getpts_uv (&npt,&pt);
	imin = -1;

	if (chunks < 2) goto Done;

	if (itsk == 0)
	{
		if (ltrans)
			to_unbs (ptx,nearpt,&type);
		else
			um_vctovc (ptx,nearpt);

		dmin = 1.e12;
		for (i = 0, k = 0; i < chunks && dmin > eps; i++)
		{
			for (j = 0; j < nio[i] && dmin > eps; j++, k++)
			{
				d = UM_SQDIS (nearpt, pt[k]);
				if (d < dmin)
				{
					dmin = d; imin = i;
				}
			}
		}
	}
	else if (itsk == 1)
	{
		nmax = -1;
		for (i = 0; i < chunks; i++)
		{
			k = nio[i];
			if (k > nmax)
			{
				nmax = k; imin = i;
			}
		}
	}
	else if (itsk == 3)
		imin = 0;
	else
	{
		dmax = -1.e12;
		for (i = 0; i < chunks; i++)
		{
			um_vcmnvc (pt[2*i+1],pt[2*i],vec);
			d = UM_DOT (vec,ldir);
			if (d > dmax)
			{
				dmax = d; imin = i;
			}
		}
	}

Done:
	if (imin < 0) imin = 0;

	if (imin > 0)
	{
		for (i = 0; i < imin; i++)
			uu_list_delete (&NCL_uvintof,0,3*nio[i]);
	}
	npt = nio[imin];
	NCL_uvintof.cur_cnt = 3*npt;

	Snios.cur_cnt = 1;
	nio[0] = npt;

	*np = npt;

	if (ltrans)
	{
		ncl_getpts_uv (&npt,&pt);
		for (i = 0; i < npt; i++)
			fr_unbs (pt[i],pt[i],&type);
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_select_component (irpt,ptx,np,tol)
**       Select the best intersection component.
**    PARAMETERS
**       INPUT  :
**          itsk    - if 0 use near point, if 1 select segment with most points,
**                    if 2 select longest segment
**          ptx     - near point, ignored if itsk != 0
**          tol     - tolerance
**       OUTPUT :
**          np      - number of points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_select_component (itsk,ptx,np,tol)
int itsk;
UM_real8 *ptx;
UM_int2 *np;
UU_REAL tol;
{
	int npt;

	ncl_select_component1 (itsk,ptx,&npt,tol,UU_TRUE);
	*np = npt;
}

/*********************************************************************
**    E_FUNCTION     :  void sfplio(key1,key2,irpt,ptx,np,ier)
**       Calculate a surface/plane intersection.
**    PARAMETERS
**       INPUT  :
**          key1    - surface key
**          key2    - plane key
**          irpt    - near point flag
**          ptx     - near point, ignored if irpt!=1
**       OUTPUT :
**          np      - number of points
**          ier     - Error flag
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void sfplio(key1,key2,irpt,ptx,np,ier)
UM_int4 *key1,*key2;
UM_real8 *ptx;
UM_int2 *irpt,*np,*ier;
{
	struct NCL_fixed_databag e1,e2;
	struct NCL_nclpl_rec pl;
	UM_coord *pt;
	int i,n1,status = UU_SUCCESS;
	UM_real8 tol8;
	UU_REAL d,tol;
	UM_int2 primtyp;
	UM_real8 primdata[16];

	UM_srf_boundary bound;
	UM_vector vec;
	int npo,*npi;
	int istat;
	UM_tessellation tess1;
	UU_LIST trian1;
	UM_trian *ptri1, *xtp1;

	gettol (&tol8);
	tol = tol8;
	*np = 0;
	ncl_nul_uv();

	e1.key = *key1;
	status = ncl_retrieve_data_fixed (&e1);
	if (status != UU_SUCCESS) goto Err;
	e2.key = *key2;
	status = ncl_retrieve_data_fixed (&e2);
	if (e2.rel_num != NCL_PLN_REL || status != UU_SUCCESS)
	{
		*ier = 163;
		return;
	}
	pl.key = *key2;
	status = ncl_retrieve_data_fixed (&pl);
	if (pl.rel_num != NCL_PLN_REL) status = UU_FAILURE;
	if (status != UU_SUCCESS) goto Err;
	ncl_set_boundary_toler (tol);

	status = ncl_get_sf_primdat(&e1.key,&primtyp,primdata);
	if (primtyp == NCLSF_PLANE)
	{
		vec[0] = primdata[0]; vec[1] = primdata[1]; vec[2] = primdata[2];
		um_cross (vec,pl.nvec,ldir);
		d = UM_MAG (ldir);
		if (d < 0.00175)
		{
			status = UU_FAILURE; goto Err;
		}
		ldir[0] /= d; ldir[1] /= d; ldir[2] /= d;
		ncl_get_boundary (WHOLE_BOUNDARY_LIST,&e1,&bound);
		d = UM_DOT (pl.pt,pl.nvec);
		npo = bound.np[0];
		npi = (bound.nb > 1)? (bound.np + 1): UU_NULL;
		status = ncl_bndrpln_io (bound.nb,npo,npi,bound.cvpts,pl.nvec,d,0.1*tol);
		um_free_boundary (&bound);
	}
	else
	{
		uu_list_init0 (&trian1);

		ncl_set_tess_parms (Stesstype,tol,100,100);
		um_set_tess_toler(tol);
		um_init_tess(&tess1);

		istat = ncl_get_tesslst(&e1,&tess1);
		if (istat != UU_SUCCESS)
			status = ncl_tessellate_surf(&e1,&tess1);

		if (status == UU_SUCCESS)
		{
			status = ncl_get_tess_triangles (&tess1,&trian1,0,1);
			n1 = trian1.cur_cnt;
			if (n1 < 1) status = UU_FAILURE;
		}
		if (status != UU_SUCCESS) goto Done;

		xtp1 = ptri1 = (UM_trian *)UU_LIST_ARRAY(&trian1);

		for (i = 0; i < n1; i++, xtp1++)
			ncl_tripln_io(xtp1,&pl,0.9*tol,tol*tol);

		ncl_arrange_segs (tol);
/*
.....No intersection created
.....Return failure
*/
		if (UU_LIST_LENGTH(&NCL_uvintof) <= 1) status = UU_FAILURE;
Done:
		uu_list_free(&trian1);
		um_free_tess(&tess1);
	}

Err:
	if (status != UU_SUCCESS)
	{
		if (*ier == 0) *ier = 163;
	}
	else if (Snios_init == 1)
	{
		int itsk = 1;
		if (*irpt == 1)
			itsk = 0;
		else if (primtyp == NCLSF_PLANE)
			itsk = 2;
		ncl_select_component (itsk,ptx,np,tol);
		Snios_init = 0;
		uu_list_free (&Snios);
	}
	else
	{
		ncl_getpts_uv (&n1,&pt);
		primtyp = NCLI_POINT;
		for (i = 0; i < n1; i++)
			fr_unbs (pt[i],pt[i],&primtyp);

		*np = n1;
	}

	return;
}

/*********************************************************************
**    E_FUNCTION: UU_LOGICAL ncl_segs_isect(p1,p2,q1,q2,r1,r2,tol)
**       Intersect two segments on a line. In both input segments points
**       assumed ordered along a line.
**    PARAMETERS
**       INPUT  :
**          p1,p2  - first segment
**          q1,q2  - second segment
**          tol    - tolerance
**       OUTPUT :
**          r1,r2  - intersection segment
**    RETURNS      :  UU_TRUE iff there is a nondegenerate intersection
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL ncl_segs_isect(p1,p2,q1,q2,r1,r2,tol)
UM_coord p1,p2,q1,q2,r1,r2;
UU_REAL tol;
{
	UU_REAL a1,a2,b1,b2,c1,c2;

	a1 = UM_DOT (p1,ldir); a2 = UM_DOT (p2,ldir);
	b1 = UM_DOT (q1,ldir); b2 = UM_DOT (q2,ldir);
	if (a1 >= b2+tol || b1 >= a2 + tol) return (UU_FALSE);
	if (a1 >= b1)
	{
		c1 = a1; um_vctovc(p1,r1);
	}
	else
	{
		c1 = b1; um_vctovc(q1,r1);
	}
	if (a2 <= b2)
	{
		c2 = a2; um_vctovc(p2,r2);
	}
	else
	{
		c2 = b2; um_vctovc(q2,r2);
	}
	return ((c2 - c1) > tol);
}

/*********************************************************************
**    E_FUNCTION     :  void flatio(key1,key2,irpt,ptx,np,ier)
**       Calculate the intersection of two planar surfaces.
**    PARAMETERS
**       INPUT  :
**          key1    - first surface key
**          key2    - second surface key
**          irpt    - near point flag
**          ptx     - near point, ignored if irpt!=1
**       OUTPUT :
**          np      - number of points
**          ier     - Error flag
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void flatio(key1,key2,irpt,ptx,np,ier)
UM_int4 *key1,*key2;
UM_real8 *ptx;
UM_int2 *irpt,*np,*ier;
{
	struct NCL_fixed_databag srf;
	int i,j,n1,n2,status = UU_SUCCESS;
	UM_real8 tol8;
	UU_REAL d1,d2,d,tol;
	UM_coord *pt, *pts1 = UU_NULL, *pts2 = UU_NULL;
	UM_coord hpt1,hpt2;
	UM_int2 primtyp;
	UM_real8 primdata[16];
	UM_srf_boundary bound;
	UM_vector nvec1,nvec2;
	int npo,*npi;

	gettol (&tol8);
	tol = tol8;
	*np = 0;

	status = ncl_get_sf_primdat(key1,&primtyp,primdata);
	if (primtyp != NCLSF_PLANE) goto Err;
	nvec1[0] = primdata[0]; nvec1[1] = primdata[1]; nvec1[2] = primdata[2];
	d1 = primdata[3];
	status = ncl_get_sf_primdat(key2,&primtyp,primdata);
	if (primtyp != NCLSF_PLANE) goto Err;
	nvec2[0] = primdata[0]; nvec2[1] = primdata[1]; nvec2[2] = primdata[2];
	d2 = primdata[3];

	um_cross (nvec1,nvec2,ldir);
	d = UM_MAG (ldir);
	if (d < 0.00175)
	{
		status = UU_FAILURE; goto Err;
	}
	ldir[0] /= d; ldir[1] /= d; ldir[2] /= d;

	srf.key = *key1;
	status = ncl_retrieve_data_fixed (&srf);
	if (status != UU_SUCCESS) goto Err;
	ncl_set_boundary_toler (tol);
	ncl_get_boundary (WHOLE_BOUNDARY_LIST,&srf,&bound);
	npo = bound.np[0];
	npi = (bound.nb > 1)? (bound.np + 1): UU_NULL;
	status = ncl_bndrpln_io (bound.nb,npo,npi,bound.cvpts,nvec2,d2,0.1*tol);
	um_free_boundary (&bound);

	ncl_getpts_uv (&n1,&pt);
	if (n1 < 2) status = UU_FAILURE;
	if (status != UU_SUCCESS) goto Err;

	pts1 = (UM_coord *) uu_malloc (n1*sizeof(UM_coord));
	for (i = 0; i < n1; i++) um_vctovc (pt[i],pts1[i]);
	ncl_nul_uv();

	srf.key = *key2;
	status = ncl_retrieve_data_fixed (&srf);
	if (status != UU_SUCCESS) goto Err;
	ncl_get_boundary (WHOLE_BOUNDARY_LIST,&srf,&bound);
	npo = bound.np[0];
	npi = (bound.nb > 1)? (bound.np + 1): UU_NULL;
	status = ncl_bndrpln_io (bound.nb,npo,npi,bound.cvpts,nvec1,d1,0.1*tol);
	um_free_boundary (&bound);

	ncl_getpts_uv (&n2,&pt);
	if (n2 < 2) status = UU_FAILURE;
	if (status != UU_SUCCESS) goto Err;

	pts2 = (UM_coord *) uu_malloc (n2*sizeof(UM_coord));
	for (i = 0; i < n2; i++) um_vctovc (pt[i],pts2[i]);
	ncl_nul_uv();

	for (i = 0; i < n1; i+=2)
	{
		for (j = 0; j < n2; j+=2)
		{
			if (ncl_segs_isect(pts1[i],pts1[i+1],pts2[j],pts2[j+1],
				hpt1,hpt2,tol))
			{
				ncl_push_uv (hpt1); ncl_push_uv (hpt2);
			}
		}
	}

Err:
	if (status != UU_SUCCESS)
	{
		if (*ier == 0) *ier = 163;
	}
	else
	{
		ncl_getpts_uv (&n1,&pt);
		*np = n1;
		if (n1 > 2)
		{
			int itsk = 2;

			n2 = 2;
			Snios.cur_cnt = 0;
			for (j = 0; j < n1/2; j++) uu_list_push (&Snios,&n2);
			if (*irpt == 1) itsk = 0;

			ncl_select_component (itsk,ptx,np,tol);
		}
		else
		{
			primtyp = NCLI_POINT;
			for (i = 0; i < n1; i++)
				fr_unbs (pt[i],pt[i],&primtyp);
		}
	}

	if (Snios_init == 1)
	{
		Snios_init = 0;
		uu_list_free (&Snios);
	}
	if (pts1) uu_free(pts1);
	if (pts2) uu_free(pts2);

	return;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_trim_cv (np,key,itsk,tol,irpt,nrpt)
**     Trim a surface curve to the trimmed surface boundary
**    PARAMETERS
**       INPUT  :
**          np         - number of points in the curve
**          key        - surface key
**          itsk       - 1, make the trimmed curve XYZ
**                       2, leave it a UV-curve
**                       3, prepare for the next call with the second
**                          trimmed surface (keep all possible pieces)
**                       4, called from ncl_waterline
**          tol        - tolerance (=sc(27))
**          irpt       - near point flag
**          nrpt       - near point
**       OUTPUT :
**          np         - number of points in the trimmed curve
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : If no near point, the piece containing the most points
**                   is selected (not necessarily the longest)
*********************************************************************/
int ncl_trim_cv (np,key,itsk,tol,irpt,nrpt)
UM_int2 *np,*itsk,*irpt;
UM_int4 *key;
UM_real8 *tol, *nrpt;
{
	int status,i,j,insf,imin,k,counting,n,ix,jx,nuv;
	int chunks,*nio,*nchunk;
	struct NCL_fixed_databag srf;
	UM_srf_boundary bound;
	UU_REAL dtol,tolsq,d,dmin,zlev;
	UM_coord *pt,uvi,nearpt;
	UM_2Dcoord vln;
	UU_LIST iobuf;
	UM_pointd *uvpd;
	UM_int2 type = NCLI_POINT;
	UM_transf tfmat;
	struct UM_evsrfout evsrf;
	UU_LOGICAL insert,lv94;

	lv94 = ncl_setver(94);

	chunks = 1;
	nio = nchunk = UU_NULL;
	n = *np;
	if (n < 2) return (UU_FAILURE);

	if (*tol <= 0.) return (UU_FAILURE);
	dtol = *tol;
	tolsq = dtol*dtol;

	srf.key = *key;
	status = ncl_retrieve_data_fixed (&srf);
	if (status != UU_SUCCESS) goto XYZ;

	uu_list_init (&iobuf, sizeof(UM_pointd), 4, 4);
	if (Snios_init == 0)
	{
		uu_list_init (&Snios, sizeof(int), 5, 5);
		Snios_init = 1;
	}
	else
	{
		chunks = Snios.cur_cnt;
		if (chunks < 1) chunks = 1;
	}
	nchunk = (int *) uu_malloc (chunks*sizeof(int));
	if (chunks > 1)
	{
		nio = (int *) UU_LIST_ARRAY (&Snios);
		for (i = 0; i < chunks; i++) nchunk[i] = nio[i];
	}
	else
		nchunk[0] = n;

	ncl_set_boundary_toler (dtol);
	ncl_get_boundary (WHOLE_BOUNDARY_LIST,&srf, &bound);
	if (bound.nb <= 0 || bound.np[0] < 4) goto XYZ;

	if (*irpt == 1)
		to_unbs (nrpt,nearpt,&type);

	status = uc_retrieve_transf (srf.key, tfmat);
	if (status != UU_SUCCESS) goto XYZ;

	srf.key = ((struct NCL_trimsf_rec *)&srf)->bs_key;
	status = ncl_retrieve_data_fixed (&srf);
	if (status != UU_SUCCESS) goto XYZ;
/*
..... within each chunk intersect each curve segment with the surface boundary,
..... insert the intersections into the list
*/
	ncl_getpts_uv (&nuv,&pt);
	if (*itsk == 4)
	{
/*
..... for waterline: replace first and last XYZ points by evaluating surface
*/
		zlev = pt[1][2];
		status = uc_evsrf (UM_POINT,pt[0][0],pt[0][1],&srf,tfmat,&evsrf);
		if (status != UU_SUCCESS) goto XYZ;
		pt[1][0] = evsrf.sp[0]; pt[1][1] = evsrf.sp[1];
		status = uc_evsrf (UM_POINT,pt[2*n-2][0],pt[2*n-2][1],&srf,tfmat,&evsrf);
		if (status != UU_SUCCESS) goto XYZ;
		pt[2*n-1][0] = evsrf.sp[0]; pt[2*n-1][1] = evsrf.sp[1];
	}
/*
..... Note: even points are UV, odd points are XYZ
*/
	k = 0;
	for (i = 0; i < chunks; i++)
	{
		n = nchunk[i];
		for (j = 0; j < n-1; j++)
		{
			to_unbs (pt[2*k+1],pt[2*k+1],&type);
			um_vcmnvc_2d (pt[2*k+2],pt[2*k],vln);
			d = UM_MAG_2D (vln);
			if (d < UM_FUZZ)
				ix = 0;
			else
			{
				vln[0] /= d; vln[1] /= d;
				ix = um_isect_cls_bndry (pt[2*k],pt[2*k+2],vln,d,&bound,dtol,
					&iobuf);
			}
			if (ix > 0)
			{
				uvpd = (UM_pointd *) UU_LIST_ARRAY (&iobuf);
				for (jx = 0; jx < ix; jx++)
				{
					insert = (uvpd[jx].param > 1.e-5 && d - uvpd[jx].param > 1.e-5);
					if (insert)
					{
						status = uc_evsrf (UM_POINT,uvpd[jx].point[0],
							uvpd[jx].point[1],&srf,tfmat,&evsrf);
						if (status != UU_SUCCESS) goto XYZ;
						if (*itsk == 4) evsrf.sp[2] = zlev;
						if (uvpd[jx].param < UM_FUZZ ||
							(d - uvpd[jx].param) < UM_FUZZ)
						{
							if (2*uvpd[jx].param <= d)
								d = UM_SQDIS(evsrf.sp,pt[2*k+1]);
							else
								d = UM_SQDIS(evsrf.sp,pt[2*k+3]);
							insert = (d > tolsq);
						}
						if (insert)
						{
							j++; k++; n++;
							uvpd[jx].point[2] = -1;
							uu_list_insert_multiple(&NCL_uvintof,6*k,3,uvpd[jx].point);
							uu_list_insert_multiple (&NCL_uvintof, 6*k+3, 3, evsrf.sp);
							ncl_getpts_uv (&nuv,&pt);
						}
					}
					else if (*itsk == 1 && !lv94)
					{
						UM_vector v0,v;
						UU_REAL dd,co,t;

						status = uc_evsrf (UM_POINT,uvpd[jx].point[0],
							uvpd[jx].point[1],&srf,tfmat,&evsrf);
						if (status != UU_SUCCESS) goto XYZ;

						um_vcmnvc (pt[2*k+3],pt[2*k+1],v);
						um_vcmnvc (evsrf.sp,pt[2*k+1],v0);
						dd = UM_DOT (v,v);
						co = UM_DOT (v,v0);
						if (dd > UM_DFUZZ)
						{
							t = co/dd;
							if (t > 0 && t < 1)
							{
								um_translate_point (pt[2*k+1],t,v,evsrf.sp);

								if (2*uvpd[jx].param <= d)
								{
									um_vctovc (evsrf.sp,pt[2*k+1]);
								}
								else
								{
									um_vctovc (evsrf.sp,pt[2*k+3]);
								}

							}

						}

					}
/*
..... for a multi-point intersection: check if the middle between a pair of
..... intersection points falls outside; if it does insert this middle into the
..... list
*/
					if (jx < ix-1)
					{
						um_middlept_2d (uvpd[jx].point,uvpd[jx+1].point,uvi);
						status = uc_evsrf (UM_POINT,uvi[0],uvi[1],&srf,tfmat,&evsrf);
						if (status != UU_SUCCESS) goto XYZ;
						if (*itsk == 4) evsrf.sp[2] = zlev;
						insf = um_inside_bndr (uvi,evsrf.sp,&bound,tol,UU_TRUE);
						if (insf == -1)
						{
							j++; k++; n++;
							uvi[2] = 1;
							uu_list_insert_multiple (&NCL_uvintof,6*k,3,uvi);
							uu_list_insert_multiple (&NCL_uvintof,6*k+3,3,evsrf.sp);
							ncl_getpts_uv (&nuv,&pt);
						}
					}
				}
			}
			k++;
			iobuf.cur_cnt = 0;
		}
		nchunk[i] = n;
		to_unbs (pt[2*k+1],pt[2*k+1],&type);
		k++;
	}
/*
..... Find all pieces inside the boundary
*/
	ix = k = counting = 0;
	Snios.cur_cnt = 0;

	for (i = 0; i < chunks; i++)
	{
		n = nchunk[i];
		for (j = 0; j < n; j++)
		{
			if (pt[2*k][2] == -1)
				insf = 0;
			else if (pt[2*k][2] == 1)
				insf = -1;
			else
				insf = um_inside_bndr (pt[2*k],pt[2*k+1],&bound,tol,UU_TRUE);

			if (counting == 0)
			{
				if (insf == -1)
				{
					uu_list_delete (&NCL_uvintof,6*k,6);
					j--; k--; n--;
					ncl_getpts_uv (&nuv,&pt);
				}
				else
					counting = 1;
			}
			else
			{
				ix++;
				if (insf == -1)
				{
					if (ix > 1)
					{
						uu_list_push (&Snios,&ix);
						uu_list_delete (&NCL_uvintof,6*k,6);
					}
					else
					{
						uu_list_delete (&NCL_uvintof,6*k-6,12);
						j--; k--; n--;
					}
					j--; k--; n--;
					ix = 0; counting = 0;
					ncl_getpts_uv (&nuv,&pt);
				}
				else if (j == n-1)
				{
					ix++;
					if (ix > 1)
						uu_list_push (&Snios,&ix);
					else
					{
						uu_list_delete (&NCL_uvintof,6*k-6,12);
						j--; k--; n--;
					}
					ix = 0; counting = 0;
					ncl_getpts_uv (&nuv,&pt);
				}
			}
			k++;
		}
	}

	chunks = Snios.cur_cnt;
	nio = (int *) UU_LIST_ARRAY (&Snios);

	if (*itsk < 4)
	{
		ncl_getpts_uv (&nuv,&pt);
		k = 0;
		for (i = 0; i < chunks; i++)
		{
			for (j = 0; j < nio[i]; j++)
			{
				if (j > 0 && j < nio[i]-1 && pt[2*k][2] == -1)
				{
					uu_list_delete (&NCL_uvintof,6*k,6);
					j--; k--; nio[i]--;
					ncl_getpts_uv (&nuv,&pt);
				}
				k++;
			}
		}
	}

	if (chunks > 1 && *itsk < 3)
	{
		imin = n = -1;
		if (*irpt == 1)
		{
			dmin = 1000000.;
			k = 0;
			for (i = 0; i < chunks; i++)
			{
				for (j = 0; j < nio[i]; j++)
				{
					d = um_dcccc (nearpt, pt[2*k+1]);
					if (d < dmin)
					{
						dmin = d; imin = i;
					}
					k++;
				}
			}
		}
		else
		{
			for (i = 0; i < chunks; i++)
			{
				if (nio[i] > n)
				{
					imin = i; n = nio[i];
				}
			}
		}
		for (i = 0; i < imin; i++)
			uu_list_delete (&NCL_uvintof,0,6*nio[i]);
		n = nio[imin];
		NCL_uvintof.cur_cnt = 6*n;
		Snios.cur_cnt = 1;
	}
/*
..... Do UV -> XYZ transform if itsk is 1 (curve or spline being
..... calculated)
*/
XYZ:;
		ncl_getpts_uv (&nuv,&pt);
		n = nuv/2;
		*np = n;
		for (i = 0 ; i < n; i++)
		{
			if (*itsk == 3)
				fr_unbs (pt[2*i+1], pt[2*i+1],&type);
			else if (*itsk == 2)
			{
				uu_list_delete (&NCL_uvintof,3*i+3,3);
				ncl_getpts_uv (&nuv,&pt);
			}
			else
			{
				fr_unbs (pt[i+1], pt[i+1],&type);
				uu_list_delete (&NCL_uvintof,3*i,3);
				ncl_getpts_uv (&nuv,&pt);
			}
		}

	um_free_boundary (&bound);
	uu_list_free (&iobuf);

	if (*itsk < 3 && Snios_init == 1)
	{
		Snios_init = 0;
		uu_list_free (&Snios);
	}
	return (status);
}


/*********************************************************************
**    E_FUNCTION   : int  ncl_arrange_segs1 (tol,cvpts)
**       Arrange a segment; put the resulting connected pieces in the
**       intersection lists,write segment middle points into cvpts.
**    PARAMETERS
**       INPUT  :
**          tol     - tolerance
**          global head points at the first segment
**       OUTPUT :
**			cvpts	- array of segment points
**          NCL_uvintof, Snios contain the updated intersection data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void  ncl_arrange_segs1 (tol,cvpts)
UU_REAL tol;
UU_LIST *cvpts;
{
	UU_REAL eps = tol*tol,d,d1,dmin;
	int found,i,n;
	ncl_ioseg *cur,*next,*before,*prev;
	UM_coord pt1, ptch;

	for (cur = head; cur != UU_NULL; cur = cur->next)
	{
		prev = cur;
		for (next = cur->next; next != UU_NULL; )
		{
			d = UM_SQDIS (cur->pt2,next->pt1);
			d1 = UM_SQDIS (cur->pt1,next->pt2);
			if (d < eps && d1 < eps)
			{
				prev->next = next->next; uu_free (next); next = prev->next;
				break;
			}
			d = UM_SQDIS (cur->pt1,next->pt1);
			d1 = UM_SQDIS (cur->pt2,next->pt2);
			if (d < eps && d1 < eps)
			{
				prev->next = next->next; uu_free (next); next = prev->next;
				break;
			}
			prev = next; next = next->next;
		}
	}
/*
.....With tight tolerance, the value for eps is too tight and so bad results
.....can occur - ASF 1/20/14 (QAR 100136).
*/
	for (cur = head, n = 2; cur != UU_NULL; )
	{
		before = prev = cur;
		found = 0;
		dmin = 5.*eps;
		next = cur->next;
		while (next != UU_NULL)
		{
			d = UM_SQDIS (cur->pt2,next->pt1);
			if (d < dmin)
			{
				found = 1; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			d = UM_SQDIS (cur->pt2,next->pt2);
			if (d < dmin)
			{
				found = 2; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			d = UM_SQDIS (head->pt1,next->pt2);
			if (d < dmin)
			{
				found = -1; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			d = UM_SQDIS (head->pt1,next->pt1);
			if (d < dmin)
			{
				found = -2; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			prev = next; next = next->next;
		}
		if (found != 0)
		{
			n++;
			next = before->next;
			if (found == 2 || found == -2)
			{
				for (i = 0; i < 3; i++)
				{
					d = next->pt1[i];
					next->pt1[i] = next->pt2[i];
					next->pt2[i] = d;
				}
			}
			before->next = next->next;
			if (found > 0)
			{
/*
..... insert next after current end
*/
				next->next = cur->next;
				cur->next = next;
				cur = next;
			}
			else
			{
/*
..... insert next before current start
*/
				next->next = head;
				head = next;
			}
		}
		else
		{
			ncl_push_uv (head->pt1);
			um_vctovc (head->pt1,pt1);
			for (i = 0; i < n; i++)
			{
				if (UM_SQDIS (pt1,head->pt2) > eps ||
					(n > 2 && i == n-1))
				{
					ncl_push_uv (head->pt2);
					um_vctovc (head->pt2,pt1);
					um_middlept(head->pt1,head->pt2,ptch);
					uu_list_push (cvpts,ptch);
				}
				else
				{
					i--; n--;
				}
				next = head->next;
				if (head == cur) i = n-1;
				uu_free (head);
				head = next;
			}
			if (n < 2)
				NCL_uvintof.cur_cnt -= 3;
			else
			{
				if (Snios_init == 0 && head != UU_NULL)
				{
					uu_list_init (&Snios, sizeof(int), 5, 5);
					Snios_init = 1;
				}
				if (Snios_init == 1) uu_list_push (&Snios,&n);
			}
			cur = head;
			n = 2;
		}
	}
	while (head != UU_NULL)
	{
		next = head->next;
		uu_free (head);
		head = next;
	}
	return;
}

void sfplio1(key1,key2,irpt,ptx,np,pts,nev,ier)
UM_int4 *key1,*key2;
UM_real8 *ptx;
UM_int2 *irpt,*np,*ier;
UU_LIST *pts;
UM_vector nev;
{
	struct NCL_fixed_databag e1,e2;
	struct NCL_nclpl_rec pl;
	UM_coord *pt,cvpt, tcvpt;
	int i,n1,status = UU_SUCCESS;
	UM_real8 tol8;
	UU_REAL d,tol;
	UM_int2 primtyp;
	UM_real8 primdata[16];

	UM_srf_boundary bound;
	UM_vector vec;
	int npo,*npi;
	int istat;
	UM_tessellation tess1;
	UU_LIST trian1;
	UM_trian *ptri1, *xtp1;

	gettol (&tol8);
	tol = tol8;
	*np = 0;
	ncl_nul_uv();

	e1.key = *key1;
	status = ncl_retrieve_data_fixed (&e1);
	if (status != UU_SUCCESS) goto Err;
	e2.key = *key2;
	status = ncl_retrieve_data_fixed (&e2);
	if (e2.rel_num != NCL_PLN_REL || status != UU_SUCCESS)
	{
		*ier = 163;
		return;
	}
	pl.key = *key2;
	status = ncl_retrieve_data_fixed (&pl);
	um_vctovc(nev,pl.nvec);
	if (pl.rel_num != NCL_PLN_REL) status = UU_FAILURE;
	if (status != UU_SUCCESS) goto Err;
	ncl_set_boundary_toler (tol);

	status = ncl_get_sf_primdat(&e1.key,&primtyp,primdata);
	if (primtyp == NCLSF_PLANE)
	{
		vec[0] = primdata[0]; vec[1] = primdata[1]; vec[2] = primdata[2];
		um_cross (vec,pl.nvec,ldir);
		d = UM_MAG (ldir);
		if (d < 0.00175)
		{
			status = UU_FAILURE; goto Err;
		}
		ldir[0] /= d; ldir[1] /= d; ldir[2] /= d;
		ncl_get_boundary (WHOLE_BOUNDARY_LIST,&e1,&bound);
		d = UM_DOT (pl.pt,pl.nvec);
		npo = bound.np[0];
		npi = (bound.nb > 1)? (bound.np + 1): UU_NULL;
		status = ncl_bndrpln_io (bound.nb,npo,npi,bound.cvpts,pl.nvec,d,0.1*tol);
		um_free_boundary (&bound);
	}
	else
	{
		uu_list_init0 (&trian1);

		ncl_set_tess_parms (Stesstype,tol,100,100);
		um_set_tess_toler(tol);
		um_init_tess(&tess1);

		istat = ncl_get_tesslst(&e1,&tess1);
		if (istat != UU_SUCCESS)
			status = ncl_tessellate_surf(&e1,&tess1);

		if (status == UU_SUCCESS)
		{
			status = ncl_get_tess_triangles (&tess1,&trian1,0,1);
			n1 = trian1.cur_cnt;
			if (n1 < 1) status = UU_FAILURE;
		}
		if (status != UU_SUCCESS) goto Done;

		xtp1 = ptri1 = (UM_trian *)UU_LIST_ARRAY(&trian1);
		tcvpt[0] = tcvpt[1]=tcvpt[2]=-10000.0;
		for (i = 0; i < n1; i++, xtp1++)
		{

			ncl_tripln_io(xtp1,&pl,0.9*tol,tol*tol);
		}


		ncl_arrange_segs1 (tol,pts);
/*
.....No intersection created
.....Return failure
*/
		if (UU_LIST_LENGTH(&NCL_uvintof) <= 1) status = UU_FAILURE;
Done:
		uu_list_free(&trian1);
		um_free_tess(&tess1);
	}

Err:
	if (status != UU_SUCCESS)
	{
		if (*ier == 0) *ier = 163;
	}
	else if (Snios_init == 1)
	{
		int itsk = 1;
		if (*irpt == 1)
			itsk = 0;
		else if (primtyp == NCLSF_PLANE)
			itsk = 2;
		ncl_select_component (itsk,ptx,np,tol);
		Snios_init = 0;
		uu_list_free (&Snios);
	}
	else
	{
		ncl_getpts_uv (&n1,&pt);
		primtyp = NCLI_POINT;
		for (i = 0; i < n1; i++)
			fr_unbs (pt[i],pt[i],&primtyp);

		*np = n1;
	}

	return;
}

