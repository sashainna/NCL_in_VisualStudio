/*********************************************************************
**    NAME         :  newatover.c
**       CONTAINS:  Routines for the overhanging surfaces logic,
**                  used in waterline roughing
**
**        ncl_proj_tess_above
**        ncl_proj_tri
**        ncl_is_tri_valid
**        ncl_proj_tri_above
**        ncl_connect_pieces
**        ncl_proj_bndr_above
**        ncl_check_inside
**        ncl_overhangs
**        ncl_check_overhangs
**        ncl_debug_contour_stock
**        ncl_debug_contour_3D
**        ncl_debug_slices
**        ncl_debug_overhang
**        ncl_debug_polygon
**        ncl_tri_inside_boundary
**        ncl_trim_tess_bound
**        ncl_contour_to_bound
**        ncl_sfbound_to_contour
**        ncl_contour_to_trilist
**        ncl_trim_tess_boundpoly
**        ncl_trim_tri_bound
**
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       newatover.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       10/19/15 , 17:22:45
*********************************************************************/
#include "nclwaterln.h"
#include "nclclip.h"
#include "mgeom.h"
#include "uminmax.h"

extern UU_LOGICAL UN_clip_debug;

static int ISF;

static UU_LIST ptlst;
static int ptinit = 0;

static UU_LIST tripolst;
static int trinit = 0;

void ncl_debug_polygon();
void ncl_polygon_cclw();

/*********************************************************************
*********************************************************************/
void ncl_free_aux_ptlist()
{
	if (ptinit == 1)
	{
		ptinit = 0; uu_list_free (&ptlst);
	}
}

/*********************************************************************
*********************************************************************/
void ncl_reset_aux_ptlist (p,n)
int n;
UU_LIST **p;
{
	if (ptinit == 0)
	{
		uu_list_init (&ptlst,sizeof(UM_2Dcoord),n,n);
		ptinit = 1;
	}
	else
		ptlst.cur_cnt = 0;

	*p = &ptlst;
}

/*********************************************************************
*********************************************************************/
void ncl_free_tripolst()
{
	if (trinit == 1)
	{
		trinit = 0; uu_list_free (&tripolst);
	}
}

/*********************************************************************
**    E_FUNCTION: ncl_set_trian(pt1,pt2,pt3,ptrian)
**       Put pt1,pt2 and pt3 into ptrian
**       INPUT  :
**          pt1,pt2, pt3 - triangle points
**       OUTPUT : 
**          ptrian		 - traingle
**    PARAMETERS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
void ncl_set_trian(pt1,pt2,pt3,ptrian)
UM_coord pt1,pt2,pt3;
UM_trian *ptrian;
{
	um_vctovc (pt1,ptrian->p1);
	um_vctovc (pt2,ptrian->p2);
	um_vctovc (pt3,ptrian->p3);
}

/*********************************************************************
**    E_FUNCTION: ncl_trian_above_prev(ptrian,zprev)
**       Put pt1,pt2 and pt3 into ptrian
**       INPUT  :
**          ptrian		- triangle
**			zprev		- previous zlevel
**       OUTPUT : 
**          none
**    RETURNS      :
**       UU_FAILURE if below zprev and UU_SUCCESS if above
**    PARAMETERS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_trian_above_prev(ptrian,zprev)
UM_trian *ptrian;
UU_REAL zprev;
{
	int status;
	status = UU_FAILURE;

	if (ptrian->p1[2] < zprev || 
		ptrian->p2[2] < zprev || 
		ptrian->p3[2] < zprev)	
		return status;

	return UU_SUCCESS;
}
						
/**************************************************************************
**    E_FUNCTION     : int ncl_proj_tri(tri1,tolsq,trilist)
**       Pushes a triangle onto a triangle list if its area is large enough.
**    PARAMETERS
**       INPUT  :
**          tri1       - Triangle to push onto list.
**          tolsq      - Squared tolerance to use to determine if
**                       triangle is large enough to push onto list.
**       OUTPUT :
**          trilist    - Updated list of triangles.
**
**    RETURNS      :
**         1 iff intersection found; 0 else
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_proj_tri(tri1,tolsq,trilist)
UM_trian *tri1;
UU_LIST *trilist;
UU_REAL tolsq;
{
	if (ncl_is_tri_valid(tri1->p1,tri1->p2,tri1->p3,tolsq))
	{
		uu_list_push (trilist,tri1->p1);
		uu_list_push (trilist,tri1->p2);
		uu_list_push (trilist,tri1->p3);
		return (UU_SUCCESS);
	}
	else
		return(UU_FAILURE);
}

/**************************************************************************
**    E_FUNCTION     : ncl_is_tri_valid(p1,p2,p3,tolsq)
**       Determines if a triangle is large enough to be valid.
**    PARAMETERS
**       INPUT  :
**          p1,p2,p3   - Triangle to test.
**          tolsq      - Squared tolerance to use to determine if
**                       triangle is large enough to be valid.
**       OUTPUT : none
**
**    RETURNS      :
**         UU_TRUE if triangle is valid, UU_FALSE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
UU_LOGICAL ncl_is_tri_valid(p1,p2,p3,tolsq)
UM_2Dcoord p1,p2,p3;
UU_REAL tolsq;
{
	if (UM_SQDIS_2D(p1,p2) < tolsq &&
		UM_SQDIS_2D(p2,p3) < tolsq &&
		UM_SQDIS_2D(p3,p1) < tolsq)
		return (UU_FALSE);
	if (UM_SQDIS_2D(p1,p2) == 0. ||
		UM_SQDIS_2D(p2,p3) == 0. ||
		UM_SQDIS_2D(p3,p1) == 0.)
		return (UU_FALSE);
	return (UU_TRUE);
}

/**************************************************************************
**    E_FUNCTION     : int ncl_tri_toler(trilist,intol,outtol)
**       Determines the tolerance to use for the minimum area of a triangle
**       based on the average area of the triangles in the list.
**    PARAMETERS
**       INPUT  :
**          trilist    - List of triangles.
**          intol      - Default squared tolerance to use.
**       OUTPUT :
**          outtol     - Calculated tolerance to use.  This value will be
**                       less than or equal to intol.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
void ncl_tri_toler(trilist,intol,outtol)
UU_LIST *trilist;
UU_REAL intol,*outtol;
{
	int i,ntri;
	UM_trian *ptri;
	UU_REAL avg,min,max,rnum,um_triangle_signed_area();
/*
.....Initialize routine
*/
	ptri = (UM_trian *)UU_LIST_ARRAY(trilist);
	ntri = UU_LIST_LENGTH(trilist);
	avg = max = 0.;
	*outtol = min = intol;
/*
.....Get minimum and average triangle area
*/
	for (i=0; i<ntri; i++, ptri++)
	{
		rnum = fabs(um_triangle_signed_area(ptri->p1,ptri->p2,ptri->p3));
		if (rnum < min) min = rnum;
		if (rnum > max) max = rnum;
		avg = avg + rnum;
	}
	avg = avg / ntri / 100000.;
	if (avg < intol) *outtol = avg;
	return;
}


/*********************************************************************
**    E_FUNCTION: S_debug_vtxlist(sfkey,wbase,zlev,vtxlist)
**       Debug three point for lp.lis
**       INPUT  :
**			srf			- surface data
**			wbase		- base surface
**			zlev		- zlev value
**			vtxlist		- point list
**       OUTPUT : 
**          none
**    RETURNS      :
**          none
**    PARAMETERS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
static void S_debug_vtxlist(sfkey,wbase,zlev,vtxlist)
UU_KEY_ID sfkey;
NCL_w_base *wbase;
UU_REAL zlev;
UU_LIST *vtxlist;
{
	int j, np;
	UU_REAL dist;
	UM_2Dcoord *vtx;
	UM_coord spt1,spt2;
	char tbuf[80];
	np = vtxlist->cur_cnt;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (vtxlist);
	sprintf(tbuf,"$$srf->key/%d",sfkey);
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"*stop");
	NclxDbgPstr(tbuf);
	for (j=0; j<np-1; j++)
	{
		if (ncl_is_watrev())
		{
			ncl_wat_evsrf (wbase,vtx[j],zlev,spt1);
			ncl_wat_evsrf (wbase,vtx[j+1],zlev,spt2);
		}
		else
		{
			um_vctovc (vtx[j],spt1);
			um_vctovc (vtx[j+1],spt2);
		}

		dist = um_sqdis(spt1,spt2);
		if (dist > 400*UM_DFUZZ)
		{
			sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
				spt1[0],spt1[1],spt1[2],spt2[0],spt2[1],spt2[2]);
			NclxDbgPstr(tbuf);
		}
	}
}

/*********************************************************************
**    E_FUNCTION: ncl_debug_triangle(sfkey,insf1,insf2,insf3,ptri)
**       Debug point for lp.lis
**       INPUT  :
**          pt			- point
**			color		- color
**       OUTPUT : 
**          none
**    RETURNS      :
**       UU_FAILURE if below zprev and UU_SUCCESS if above
**    PARAMETERS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
void ncl_debug_triangle(sfkey,insf1,insf2,insf3,ptri)
UU_KEY_ID sfkey;
int insf1,insf2,insf3;
UM_trian *ptri;
{
	char tbuf[80];
	sprintf(tbuf,"$$srf->key/%d,insf1/%d,insf2/%d,insf3/%d",
		sfkey,insf1,insf2,insf3);
	NclxDbgPstr(tbuf);

	sprintf(tbuf,"*stop");
	NclxDbgPstr(tbuf);			
	sprintf(tbuf,"draft/modify,color = 9");
	NclxDbgPstr(tbuf);

	sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
		ptri->p1[0], ptri->p1[1], ptri->p1[2],
		ptri->p2[0], ptri->p2[1], ptri->p2[2]);
	NclxDbgPstr(tbuf);

	sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
		ptri->p2[0], ptri->p2[1], ptri->p2[2],
		ptri->p3[0], ptri->p3[1], ptri->p3[2]);
	NclxDbgPstr(tbuf);

	sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
		ptri->p1[0], ptri->p1[1], ptri->p1[2],
		ptri->p3[0], ptri->p3[1], ptri->p3[2]);
	NclxDbgPstr(tbuf);
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_sfbound(bound)
**		Debug surface boundary data
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          lp.lis file when UU_DEBUGL=1 is set
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_sfbound(sfkey,bound)
UU_KEY_ID sfkey;
UM_srf_bound *bound;
{
	int i,npts;
	UM_coord *pts;
	char tbuf[80];
	UU_REAL dist;

	npts = bound->npo;
	pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
								
	sprintf(tbuf,"draft/modify,color = 8");
	NclxDbgPstr(tbuf);

	sprintf(tbuf,"*stop");
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"$$srf->key/%d",sfkey);
	NclxDbgPstr(tbuf);

	for (i = 0; i < npts-1; i++)
	{
		dist = um_sqdis(pts[i], pts[i+1]);
		if (dist > 400*UM_DFUZZ)
		{
			sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
				pts[i][0],pts[i][1],pts[i][2],
				pts[i+1][0],pts[i+1][1],pts[i+1][2]);
			NclxDbgPstr(tbuf);
		}
	}
}

/**************************************************************************
**    E_FUNCTION     : ncl_bound_to_contour(npts,pst,ib,pock)
**       Convert points contour into polygon structure
**    PARAMETERS
**       INPUT  :
**			npts	- numbe rof points
**          pts     - boundary curve uvpts(2D points)
**          ib      - the index of boundary curve
**       OUTPUT : 
**          pock    - polygon structure
**    RETURNS      :
**        none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
void ncl_bound_to_contour(npts,pts,ib,pock)
UM_coord *pts;
int npts,ib;
ncl_polygon *pock;
{
	int i;		
	UM_coord *uvs;
	UM_2Dcoord pt;
	UU_REAL xmin,xmax,ymin,ymax;
	xmin = 1.0;
	ymin = 1.0;
	xmax = 0.0;
	ymax = 0.0;

	for (i = 0; i < npts; i++)
	{
		if (pts[i][0] < xmin) xmin = pts[i][0];
		if (pts[i][0] > xmax) xmax = pts[i][0];
		if (pts[i][1] < ymin) ymin = pts[i][1];
		if (pts[i][1] > ymax) ymax = pts[i][1];
	}

	pock->np[ib] = (ib == 0) ? npts : -npts;
	pock->box[ib].xmin = xmin;
	pock->box[ib].xmax = xmax;
	pock->box[ib].ymin = ymin;
	pock->box[ib].ymax = ymax;

	for (i = 0; i < npts; i++)
	{
		um_vctovc_2d(pts[i],pt);
		uu_list_push (pock->contour,pt);
	}
}

/**************************************************************************
**    E_FUNCTION     : ncl_bound_to_contour(tri,pock)
**       Convert sf boundary contours into polygon structure
**    PARAMETERS
**       INPUT  :
**			bound	- sf boundary
**       OUTPUT : 
**          pock    - polygon structure
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
static void ncl_sfbound_to_contour(bound,pock,luv)
UM_srf_boundary *bound;
ncl_polygon *pock;
UU_LOGICAL luv;
{
	UM_coord *uvs;
	int ib,nb,npts,inc;
	nb = bound->nb;

	pock->np = (int *) uu_malloc (nb*sizeof(int));
	pock->box = (UM_2box *) uu_malloc (nb*sizeof(UM_2box));
	if (pock->np == UU_NULL || pock->box == UU_NULL) return;

	pock->num_contours = nb;
	pock->contour = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (pock->contour, sizeof(UM_2Dcoord), 100, 100);

	inc = 0;
	for (ib = 0; ib < nb; ib++)
	{
		npts =bound->np[ib];
		if (luv)
			uvs = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
		else
			uvs = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
		ncl_bound_to_contour(npts,&uvs[inc],ib,pock);
		inc += npts;
	}
}

/*********************************************************************
**    E_FUNCTION: um_vcmp (p1,p2)
**       Comparison routine for the sort algorithm (uu_qsort): compares
**       z-coordinates.  
**    PARAMETERS
**       INPUT  :
**          p1     - first element to be compared 
**          p2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if p1[2] > p2[2]
**                     0 if t1[2] = p2[2]
**                     1 if p1[2] < p2[2]
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_zcmp(p1,p2)
UM_coord p1,p2;
{
	if (p1[2] > p2[2]) return(-1);
	else if (p1[2] < p2[2]) return(1);
	else return(0);
}


/**********************************************************************
**    E_FUNCTION     : int ncl_proj_tri_above(tri1,tolsq,trilist,zlev)
**       Project the "above" part of a triangle onto a horizontal plane
**    PARAMETERS
**       INPUT  :
**          tri1       - the triangle
**          tolsq      - tolerance squared
**          zlev       - Z-level of the plane
**       OUTPUT :
**          trilist    - updated list iff the projection is valid
**
**    RETURNS      :
**         3 iff the result is a nondegenerate triangle;
**         4 iff the result is a nondegenerate rectangle;
**         0 else
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
static int ncl_proj_tri_above(tri1,dtol,trilist,zlev)
UM_trian *tri1;
UU_LIST *trilist;
UU_REAL dtol,zlev;
{
	UU_REAL d0,d1,d2,tolsq;
	UM_2Dcoord v0,v1;
	UM_coord *pts;
	int i;

	i = 3;
	tolsq = dtol;
	pts = (UM_coord *) tri1;
	uu_qsort (pts,i,sizeof(UM_coord),ncl_zcmp);

	d0 = pts[0][2] - zlev;
	d1 = pts[1][2] - zlev;
	d2 = pts[2][2] - zlev;

	if (d2 >= - UM_FUZZ)
	{
		if (ncl_proj_tri(tri1,tolsq,trilist) == UU_SUCCESS)
			return (3);
		else
			return (0);
	}
	if (d0 <= UM_FUZZ)
		return (0);

	for (i = 0; i < 2; i++)	
		v0[i] = (pts[2][i]*d0 - pts[0][i]*d2)/(d0 - d2);

	if (d1 > UM_FUZZ)
	{
		for (i = 0; i < 2; i++)
			v1[i] = (pts[2][i]*d1 - pts[1][i]*d2)/(d1 - d2);

/*		if (fabs (um_triangle_signed_area(pts[0],pts[1],v0) +
			um_triangle_signed_area(v0,pts[1],v1)) > tolsq)*/
		if (ncl_is_tri_valid(pts[0],pts[1],v1,tolsq) &&
			ncl_is_tri_valid(v0,pts[1],v1,tolsq))
		{
			uu_list_push (trilist,pts[0]); uu_list_push (trilist,pts[1]);
			uu_list_push (trilist,v1); uu_list_push (trilist,v0);
			return (4);
		}
		else
			return (0);
	}
	else if (d1 < -UM_FUZZ)
	{
		for (i = 0; i < 2; i++)
			v1[i] = (pts[1][i]*d0 - pts[0][i]*d1)/(d0 - d1);

/*		if (fabs (um_triangle_signed_area(v0,pts[0],v1)) > tolsq)*/
		if (ncl_is_tri_valid(v0,pts[0],v1,tolsq))
		{
			uu_list_push (trilist,pts[0]);
			uu_list_push (trilist,v1); uu_list_push (trilist,v0);
			return (3);
		}
		else
			return (0);
	}
	else
	{
/*		if (fabs (um_triangle_signed_area(v0,pts[0],pts[1])) > tolsq)*/
		if (ncl_is_tri_valid(v0,pts[0],pts[1],tolsq))
		{
			uu_list_push (trilist,pts[0]); uu_list_push (trilist,pts[1]);
			uu_list_push (trilist,v0);
			return (3);
		}
		else
			return (0);
	}
}
/**********************************************************************
**    E_FUNCTION     : ncl_polygon_cclw(pts,npts)
**       Check if the polygon is ccw or not, if not, reverse the ploygon
**    PARAMETERS
**       INPUT  :
**          pts		  - the polygon points
**			npts	  - the numbe rof points
**       OUTPUT :
**          pts		  - the updated points if reverse
**
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_polygon_cclw(pts,npts)
UM_coord *pts;
int npts;
{
	int i;
	UU_REAL dir,x1,y1,x2,y2;
	UM_coord lpts;
/*
.....Get direction of polygon
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
.....If current direction is clockwise then reverse points
*/
	if (dir < 0.)
	{
		for (i=0;i<npts/2;i++)
		{
			um_vctovc(pts[i],lpts);
			um_vctovc(pts[npts-i-1],pts[i]);
			um_vctovc(lpts,pts[npts-i-1]);
		}
	}
}

/*********************************************************************
**    E_FUNCTION: ncl_sortThree(pts,uvts)
**       sort pts by z-coordinate 
**    PARAMETERS
**       INPUT  :
**          pts,uvts - traingle points and uv point
**       OUTPUT : 
**          pts,uvts - sorted traingle points and uv point
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
void static ncl_sort_three_z(pts,uvts)
UM_coord *pts,*uvts;
{
	UM_coord tmp_pt,tmp_uv;
	if (pts[0][2] < pts[1][2])
	{
		um_vctovc (pts[0],tmp_pt);
		um_vctovc (uvts[0],tmp_uv);
		um_vctovc (pts[1],pts[0]);
		um_vctovc (uvts[1],uvts[0]);
		um_vctovc (tmp_pt,pts[1]);
		um_vctovc (tmp_uv,uvts[1]);
	}
	if (pts[1][2] < pts[2][2])
	{
		um_vctovc (pts[1],tmp_pt);
		um_vctovc (uvts[1],tmp_uv);
		um_vctovc (pts[2],pts[1]);
		um_vctovc (uvts[2],uvts[1]);
		um_vctovc (tmp_pt,pts[2]);
		um_vctovc (tmp_uv,uvts[2]);
	}
	if (pts[0][2] < pts[1][2])
	{
		um_vctovc (pts[0],tmp_pt);
		um_vctovc (uvts[0],tmp_uv);
		um_vctovc (pts[1],pts[0]);
		um_vctovc (uvts[1],uvts[0]);
		um_vctovc (tmp_pt,pts[1]);
		um_vctovc (tmp_uv,uvts[1]);
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_proj_tess_above (sfi,tol,tolsq,pol0,zlev)
**       Project the "above" part of a surface onto a horizontal plane,
**       using tessellation.
**    PARAMETERS
**       INPUT  :
**          sfi        - current surface data
**          wbase      - base surface data
**          tol        - tolerance
**          zlev       - Z-level of the plane
**       OUTPUT :
**          pol0    - resulting contour(s)
**
**    RETURNS      :
**         UU_SUCCESS iff no errors and valid projection, else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_proj_tess_above (sfi,wbase,rot,tol,tolsq,ztol,pol0,zlev,zprev)
NCL_waterline_surf *sfi;
NCL_w_base *wbase;
UM_transf rot;
UU_REAL tol,tolsq,ztol,zlev,zprev;
ncl_polygon *pol0;
{
	int i,npts,ntri,npolys;
	ncl_polygon tripol;
	UM_trian *ptri,*uvtri,*ptri1;
	UU_LIST *ptslst;
	UU_LOGICAL firstime = UU_TRUE;
	
	int status;
	UM_coord *pts;
	UM_srf_boundary bndr;
	struct NCL_fixed_databag sf;
	UM_transf tfmat;
	UU_LIST cvlst,uvlst;
	UU_LOGICAL lv97,lpolylst;
	int ib,irot;
	UM_int2 mm,isub;
	UU_REAL toler,btol;			
	UU_LIST trilst,*polylst;

	lpolylst = UU_FALSE;
	lv97 = ncl_setver(97);

#if 0
	if (ISF == 391)
	{
		ISF = 391;
	}
#endif
	if (!lv97)
	{		
		isub = 264;
		getifl(&isub,&mm);
		toler = (mm)? tol/25.4: tol;

		ncl_set_boundary_toler (tol);
		um_init_boundary (&bndr);
		uu_list_init (&cvlst, sizeof(UM_coord), 100, 200);
		uu_list_init (&uvlst, sizeof(UM_coord), 100, 200);
		bndr.uvpts = &uvlst;
		bndr.cvpts = &cvlst;
		uu_list_init0 (&trilst);

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
		}

		if (irot > 0 || ncl_is_watrev())
		{
			pts = (UM_coord *) UU_LIST_ARRAY (bndr.cvpts);
			for (ib = 0; ib < bndr.nb; ib++)
			{
				npts = bndr.np[ib];
				for (i = 0; i < npts; i++)
				{
					if (irot > 0) um_cctmtf(pts[i],rot,pts[i]);
					if (ncl_is_watrev())
						ncl_wat_baseproj (wbase,pts[i],pts[i]);
				}
				pts += npts;
			}
		}
/*
.....Trim BADPROJ surface base tessellation with boundary
.....store the trimmed tesselation polygons into sfi->trianlist
.....and netessmgr structure
*/
		if (!ncl_is_watrev() && sfi->trianlist == UU_NULL)
		{
			if (!trilst.data)
				uu_list_init (&trilst,sizeof (UM_trian),200,200);
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
					i = (npts > 200)? npts : 200;
					sfi->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
					uu_list_init (sfi->trianlist,sizeof (UM_trian),i,i);
					uu_list_push_list (sfi->trianlist,&trilst);
				}	
				uu_list_free (&trilst);
			}
			else
			{
				lpolylst = UU_TRUE;
				if (!trilst.data)
				{
					npts = polylst->cur_cnt;
					i = (npts > 200)? npts : 200;
					uu_list_init (&trilst,sizeof (UM_trian),i,i);
				}
				else
					UU_LIST_EMPTY (&trilst);
				uu_list_push_list (&trilst,polylst);
			}

			if (irot > 0)
			{
/*
.....Modified to prevent memory errors - ASF 12/12/13.
*/
				if (lpolylst)
				{
					ptri = (UM_trian *)UU_LIST_ARRAY(&trilst);
					ntri = trilst.cur_cnt;
				}
				else
				{
					ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
					ntri = sfi->trianlist->cur_cnt;
				}
				for (i = 0; i < ntri; i++, ptri++)
				{
					um_cctmtf(ptri->p1,rot,ptri->p1);
					um_cctmtf(ptri->p2,rot,ptri->p2);
					um_cctmtf(ptri->p3,rot,ptri->p3);
				}
			}
		}
		else if (ncl_is_watrev())
		{						
			status = nclc_tessmgr_get_srf_polylst(sf.key,&polylst);
						
			if (!trilst.data)
			{
				npts = polylst->cur_cnt;
				i = (npts > 200)? npts : 200;
				uu_list_init (&trilst,sizeof (UM_trian),i,i);
			}
			else
				UU_LIST_EMPTY (&trilst);
			uu_list_push_list (&trilst,polylst);
		}
	}

	tripol.box = UU_NULL;
	pol0->num_contours = 0;
	tripol.num_contours = 1;
	tripol.contour = &tripolst;
	if (!tripol.contour || !tripol.contour->data) return(UU_FAILURE);
	
	if (!lv97)
	{
		if (ncl_is_watrev() || lpolylst)
		{
			ptri = (UM_trian *)UU_LIST_ARRAY(&trilst);
			ntri = trilst.cur_cnt;
		}
		else
		{
			ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
			ntri = sfi->trianlist->cur_cnt;
		}
	}
	else
	{
		ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
		ntri = sfi->trianlist->cur_cnt;
	}
	if (ntri < 2) return(UU_SUCCESS);

	for (i = 0; i < ntri; i++, ptri++)
	{
#if 0
	if (ISF == 391)
	{
		ncl_debug_triangle(sfi->key,0,0,0,ptri);
	}
#endif
		firstime = (pol0->num_contours == 0);
		ptslst = (firstime)? pol0->contour : tripol.contour;
		ptslst->cur_cnt = npts = 0;

		if ((sfi->zmin > zlev - ztol) &&
			ncl_proj_tri(ptri,4.*tolsq,ptslst) == 0)
			npts = 3;
		else
		{
			if (zprev > zlev+ztol && 
				ncl_trian_above_prev(ptri,zprev) == UU_SUCCESS)
				continue;

			npts = ncl_proj_tri_above(ptri,4.*tolsq,ptslst,zlev);
		}

		if	(npts < 3) continue;
		if (firstime)
		{
			pol0->num_contours = 1;
			pol0->np = (int *) uu_malloc (sizeof(int));
			pol0->np[0] = npts;
			continue;
		}
		tripol.np = &npts;

#if 0
	if (sfi->key == 19001)
	{
			NclxDbgPstr("ERASE/ALL");
			ncl_debug_polygon(UU_NULL,pol0,4); /* GREEN */
			ncl_debug_polygon(UU_NULL,&tripol,2); /* BLUE */
//			UN_clip_debug = UU_TRUE;
	}
#endif
		ncl_polygon_clip (NCL_UNION,pol0,&tripol,pol0,tol,tolsq);
#if 0
	if (sfi->key == 19001)
	{
			NclxDbgPstr("ERASE/ALL");
			ncl_debug_polygon(UU_NULL,pol0,5); /* MAGENTA */
			UN_clip_debug = UU_FALSE;
	}
#endif
		if (!pol0->contour || !pol0->contour->data) return(UU_FAILURE);
	}

	if (!lv97)
	{
		ncl_free_bndry (&bndr);
		uu_list_free (&uvlst);
		uu_list_free (&cvlst);
		if (ncl_is_watrev())	
			uu_list_free (&trilst);
	}

	return(UU_SUCCESS);
}

/************************************************************************
**    E_FUNCTION  : ncl_connect_pieces (ni,nj,vtx,ncvs,numpts,vtxlst,tol)
**
**  For a collection of curves, connect the pieces into closed contours.
**    PARAMETERS
**       INPUT  :
**          nio        - list of lengths of pieces
**          ptsio      - list of points representing the curves
**          nj
**       OUTPUT :
**          curves     - list of loops
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
static int ncl_connect_pieces (ni,nj,vtx,nc,numpts,vtxlst,tol)
int *ni,*nj,nc;
UM_2Dcoord *vtx;
UU_LIST *numpts,*vtxlst;
UU_REAL tol;
{
	int in,i,j,*ns,j0,ir,jr,nr;
	UU_REAL dis,tmp,told,dis0;
	UU_LIST ncur,loop;
	UM_2Dcoord fpt,fpt1,lpt1,lpt;
	int nnp,k,nlinks,*looplink,ip,n0;
	UU_LOGICAL closed,closed0,found;
	int status = UU_SUCCESS;
	UU_REAL mxtol2 = 1.;

	if (nc < 2) return (status);

	uu_list_init (&ncur,sizeof(int),nc,nc);
	for (j = 0; j < nc; j++)
		uu_list_push(&ncur,&j);

	uu_list_init (&loop,sizeof(int),nc,nc);

	closed = UU_TRUE;
	j = j0 = 0;
	told = 2.25*tol*tol;
	dis0 = 1.e12;
	ns = (int *) UU_LIST_ARRAY (&ncur);
	while (ncur.cur_cnt > 0 && j < ncur.cur_cnt)
	{
		in = ns[j];
		if (closed)
		{
			loop.cur_cnt = 0;
			uu_list_push(&loop,&in);
			uu_list_delete (&ncur,0,1);
			nnp = ni[in];

			um_vctovc_2d (vtx[nj[in]],fpt);
			um_vctovc_2d (vtx[nj[in]+nnp-1],lpt);
			dis = UM_SQDIS_2D(fpt,lpt);
			closed = (dis < told);
			if (!closed)
			{
				dis0 = dis;
				closed0 = UU_TRUE;
			}
		}
		if (!closed)
		{
			in = ns[j];
			nnp = ni[in];
			um_vctovc_2d (vtx[nj[in]],fpt1);
			um_vctovc_2d (vtx[nj[in]+nnp-1],lpt1);
			dis = UM_SQDIS_2D(lpt,fpt1);
			found = (dis < told);
			if (!found)
			{
				if (dis < dis0)
				{
					closed0 = UU_FALSE;
					dis0 = dis; j0 = j;
				}
				dis = UM_SQDIS_2D(lpt,lpt1);
				found = (dis < told);
				if (found)
				{
					nr  = nj[in] + nnp/2;
					for (ir = nj[in], jr = nj[in]+nnp-1; ir < nr; ir++, jr--)
					{
						for (k = 0; k < 2; k++)
						{
							tmp = vtx[ir][k];
							vtx[ir][k] = vtx[jr][k];
							vtx[jr][k] = tmp;
						}
					}
					lpt1[0] = fpt1[0]; lpt1[1] = fpt1[1];
				}
				else if (dis < dis0)
				{
					closed0 = UU_FALSE;
					dis0 = dis; j0 = j;
				}
			}
			if (found)
			{
				j0 = 0;
				dis0 = 1.e12;
				told = 2.25*tol*tol;
				uu_list_push(&loop,&in);
				uu_list_delete (&ncur,j,1); ns = (int *) UU_LIST_ARRAY (&ncur);
				dis = UM_SQDIS_2D(fpt,lpt1);
				closed = (dis < told);
				if (!closed)
				{
					if (ncur.cur_cnt < 1 && dis < mxtol2)
					{
						closed = UU_TRUE; goto CLSD;
					}
					dis0 = dis;
					closed0 = UU_TRUE;
					lpt[0] = lpt1[0]; lpt[1] = lpt1[1];
					j = 0;
				}
			}
			else
			{
				if (j == ncur.cur_cnt - 1 && loop.cur_cnt == 1 && dis0 >= mxtol2)
				{
					j = j0 = 0;
					dis0 = 1.e12;
					told = 2.25*tol*tol;
					uu_list_delete (&ncur,j,1); ns = (int *) UU_LIST_ARRAY (&ncur);
					closed = UU_TRUE;
					continue;
				}
				else if (j < ncur.cur_cnt - 1 || dis0 >= mxtol2)
					j++;
				else
				{
					closed = closed0;
					if (closed) goto CLSD;
					told = 1.0001*dis0;
					j = j0;
				}
			}
		}
CLSD:;
		if (closed)
		{
			nlinks = loop.cur_cnt;
			if (nlinks > 0)
			{
				looplink = (int *) UU_LIST_ARRAY (&loop);

				ip = 0;
				for (i = 0; i < nlinks; i++)
				{
					in = looplink[i];
					nnp = (i == nlinks-1)? ni[in]-1: ni[in];
					n0 = nj[in];
					for (k = (i > 0); k < nnp; k++)
					{
						uu_list_push (vtxlst,vtx[n0+k]);
						ip++;
					}
				}
				uu_list_push (numpts,&ip);
			}
			j = 0;
		}
	}

	uu_list_free (&ncur);
	uu_list_free (&loop);
	return (status);
}

/***************************************************************************
**    E_FUNCTION     : int ncl_proj_bndr_above (sfi,tol,pol0,zlev,nio,ptsio)
**       Project the "above" part of a surface onto a horizontal plane,
**       using boundary and already calculated surface-plane intersections.
**    PARAMETERS
**       INPUT  :
**          sfi     - current surface data
**          nio     - list containing numbers of points in each intersection
**                    polyline
**          ptsio   - list containing intersection points
**          tol     - tolerance
**          zlev    - Z-level of the plane
**       OUTPUT :
**          pol0    - resulting polygon
**
**    RETURNS      :
**         UU_SUCCESS iff no errors and valid projection, else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
***************************************************************************/
static int ncl_pt_above (pt,h,tol)
UM_coord pt;
UU_REAL h,tol;
{
	if (pt[2] > h + tol) return (1);
	else if (pt[2] >= h - tol) return (0);
	else return (-1);
}

static int ncl_proj_bndr_above (sfi,tol,tolsq,ztol,pol0,zlev,nio,ptsio)
NCL_waterline_surf *sfi;
UU_REAL tol,tolsq,ztol,zlev;
ncl_polygon *pol0;
UU_LIST *nio,*ptsio;
{
	UU_LIST nilst,njlst;
	UU_LIST numpts,*vtxlst;
	int status = UU_SUCCESS;
	int *ni,*nj,nnl,ib,nb,npt,i,j0,j1,above0,above,cnt0,cnt,ncvs,ibeg;
	UM_coord *pts;
	UM_2Dcoord *vtx,vti;
	UU_REAL d0,d1;

	nb = sfi->bound.nb;
	ncvs = sfi->ncvs;
	nnl = ncvs + nb;
	uu_list_init (&njlst,sizeof(int),nnl,nnl);
	uu_list_init (&nilst,sizeof(int),nnl,nnl);
	uu_list_init (&numpts,sizeof(int),nnl,nnl);
	vtxlst = pol0->contour;
	if (!vtxlst->data || !nilst.data || !njlst.data || !ptlst.data) goto Err;

	ptlst.cur_cnt = 0;
	pol0->num_contours = 0;
	ni = (int *) UU_LIST_ARRAY (nio); ni += sfi->nlist0;
	pts = (UM_coord *) UU_LIST_ARRAY (ptsio); pts += sfi->plist0;
	npt = 0;

	for (ib = 0, nnl = 0; ib < ncvs; nnl += npt, pts += npt, ib++)
	{
		npt = ni[ib];
		uu_list_push (&njlst,&nnl);
		uu_list_push (&nilst,&npt);
		for (i = 0; i < npt; i++)
			uu_list_push (&ptlst,pts[i]);
	}

	pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
	for (ib = 0; ib < nb; ib++, pts += (npt+1))
	{
		npt = (ib == 0)? sfi->bound.npo - 1: abs(sfi->bound.np[ib-1]) - 1;

		if (ib > 0 && sfi->bound.np[ib-1] < 0)
			continue;

		for (ibeg = 0; ibeg < npt-1; ibeg++)
		{
			above = ncl_pt_above (pts[ibeg],zlev,0.1*ztol);
			if (above <= 0) break;
		}
		if (ibeg == npt-1)
		{
			cnt = -npt;
			uu_list_push(&numpts,&cnt);
			for (i = 0; i < npt; i++)
				uu_list_push (vtxlst,pts[i]);
			continue;
		}
		for (i = 0; i < npt; i++)
		{
			cnt = 0;
			cnt0 = ptlst.cur_cnt;
			j0 = (ibeg+i)%npt;
			j1 = (j0+1)%npt;
			above0 = above;
			above = ncl_pt_above (pts[j1],zlev,0.1*ztol);
			if (above <= 0) continue;

			if (above0 == 0)
			{
				vti[0] = pts[j0][0]; vti[1] = pts[j0][1];
			}
			else
			{
				d0 = pts[j0][2] - zlev; d1 = pts[j1][2] - zlev;
				vti[0] = (pts[j1][0]*d0 - pts[j0][0]*d1)/(d0 - d1);
				vti[1] = (pts[j1][1]*d0 - pts[j0][1]*d1)/(d0 - d1);
			}
			uu_list_push (&ptlst,vti);
			while (above == 1 && i < npt)
			{
				if (UM_SQDIS_2D (vti,pts[j1]) > tolsq)
				{
					vti[0] = pts[j1][0]; vti[1] = pts[j1][1];
					uu_list_push (&ptlst,vti);
				}
				i++;
				j0 = j1;
				j1 = (j1+1)%npt;
				above = ncl_pt_above (pts[j1],zlev,0.1*ztol);
				if (above == 0 && i < npt - 1 &&
					ncl_pt_above (pts[(j1+1)%npt],zlev,0.1*ztol) == 1) above = 1;
			}

			if (above == 0)
				uu_list_push (&ptlst,pts[j1]);
			else
			{
				d0 = pts[j0][2] - zlev; d1 = pts[j1][2] - zlev;
				vti[0] = (pts[j1][0]*d0 - pts[j0][0]*d1)/(d0 - d1);
				vti[1] = (pts[j1][1]*d0 - pts[j0][1]*d1)/(d0 - d1);
				uu_list_push (&ptlst,vti);
			}
			cnt = ptlst.cur_cnt - cnt0;
			if (cnt < 3)
				ptlst.cur_cnt = cnt0;
			else
			{
				uu_list_push (&njlst,&nnl);
				uu_list_push (&nilst,&cnt);
				nnl += cnt;
			}
		}
	}

	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (&ptlst);
	ni = (int *) UU_LIST_ARRAY (&nilst);
	nj = (int *) UU_LIST_ARRAY (&njlst);
	ncvs = nilst.cur_cnt;
	status = ncl_connect_pieces (ni,nj,vtx,ncvs,&numpts,vtxlst,tol);

	if (status == UU_SUCCESS)
	{
		ncvs = numpts.cur_cnt;
		if (ncvs < 1) goto Done;
		pol0->num_contours = ncvs;
		pol0->np = (int *) uu_malloc(ncvs*sizeof(int));
		if (!pol0->np) goto Err;
		nj = (int *) UU_LIST_ARRAY (&numpts);
		for (ib = 0; ib < ncvs; ib++)
			pol0->np[ib] = nj[ib];
	}
	goto Done;
Err:
	status = UU_FAILURE;
Done:
	uu_list_free(&numpts);
	uu_list_free(&nilst);
	uu_list_free(&njlst);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_check_inside(point,pol,tol,which)
**       Determine whether a 2D point is inside any of the 2D polygon
**       contours.
**    PARAMETERS
**       INPUT :
**                point    - the point to check
**                pol      - polygon structure
**                tol      - tolerance
**       OUTPUT: 
**                which    - polygon number that point is inside of
**                           returns -1 if point is not within any
**                           polygon.  The calling routine should
**                           initialize this value to -1 prior to
**                           the loop that calls this routine.
**    RETURNS      : UU_TRUE if inside; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_check_inside(point,pol,tol,which)
UM_2Dcoord point;
ncl_polygon *pol;
UU_REAL tol;
int *which;
{
	int ins,p,h,nc,npt,*nps;
	UM_2Dcoord *vtx;

	nc = pol->num_contours;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
	nps = (int *) pol->np;

	ins = h = 0;
	for (p = 0; p < nc && ins < 1; p = h)
	{
		npt = nps[p];
		ins = um_check_inside(vtx,npt,point,&pol->box[p],tol);
		if (ins == 0) ins = 1;
		h = p + 1;
		vtx += npt;
		if (ins == 1 && *which == -1) *which = ins;
		for (; h < nc && nps[h] < 0; h++, vtx += npt)
		{
			npt = -nps[h];
			if (ins == 1 && um_check_inside(vtx,npt,point,&pol->box[h],tol) >= 0)
				ins = 0;
		}
		if (ins == 1) return (UU_TRUE);
	}

	return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : S_inside_bound(point,pol,tol)
**       Determine whether a 2D point is inside the surface boundary.
**    PARAMETERS
**       INPUT :
**                point    - the point to check
**                bound    - surface boundary structure
**                tol      - tolerance
**       OUTPUT:  none
**    RETURNS      : UU_TRUE if inside; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_inside_bound(point,bound,tol)
UM_2Dcoord point;
UM_srf_bound *bound;
UU_REAL tol;
{
	int ins,p,h,nc,npt,*nps,nptsv;
	UM_coord *pts;
	UM_2Dcoord *vtx,*vtxp;

	nc = bound->nb;
	pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
	nps = (int *) bound->np;

	npt = UU_LIST_LENGTH(bound->cvpts);
	vtx = (UM_2Dcoord *)uu_malloc(sizeof(UM_2Dcoord)*npt);
	vtxp = vtx;
	um_convert3D_2D(npt,pts,vtx);

	ins = h = 0;
	nptsv = 0;

	for (p = 0; p < nc && ins < 1; p = h)
	{
		if (p == 0)
			npt = bound->npo;
		else
			npt = nps[p-1];
		
		ins = um_check_inside(vtx,npt,point,UU_NULL,tol);
		h = p + 1;
		vtx += npt;
		for (; h < nc && nps[h] < 0; h++, vtx += npt)
		{
			npt = -nps[h-1];
			if (ins == 1 && um_check_inside(vtx,npt,point,UU_NULL,tol) >= 0)
				ins = 0;
		}
		if (ins == 1)
		{
			uu_free(vtxp);
			return (UU_TRUE);
		}
	}

	uu_free(vtxp);
	return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_area_test (pol,tolsq)
**       Weed out polygon inner contours (holes) if too small.
**    PARAMETERS
**       INPUT  :
**          pol        - polygon structure
**          tolsq      - tolerance (squared)
**       OUTPUT :
**          pol        - updated polygon structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_area_test (pol,areamin,tolsq)
ncl_polygon *pol;
UU_REAL areamin,tolsq;
{
	int nc,c,nv,i,nv0;
	UM_2Dcoord *pp;
	UU_REAL a,ai,area = 0.;

	nc = pol->num_contours;

	if (nc > 0)
	{
		for (c = 0, nv0 = 0; c < nc; c++)
		{
			nv = pol->np[c];
			if (nv >= 0)
			{
				nv0 += nv; continue;
			}
			nv = -nv;
			pp = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
			pp += nv0;
			a = UM_SQDIS_2D(pp[0],pp[1]);
			ai = UM_SQDIS_2D(pp[0],pp[nv-1]);
			if (ai > a) a = ai;
			for (i = 1, area = 0.; i < nv - 1; i++)
			{
				area += um_triangle_signed_area(pp[0],pp[i],pp[i+1]);
				ai = UM_SQDIS_2D(pp[i],pp[i+1]);
				if (ai > a) a = ai;
			}
			if ((fabs(area) < areamin) || (nv < 5 && area*area < a*tolsq))
			{
				ncl_delete_contour (nc,c,nv0,nv,pol);
				nc--; c--;
			}
			else
				nv0 += nv;
		}
	}
	pol->num_contours = nc;
	if (nc <= 0) UU_FREE (pol->np);
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_tess_overhangs(ptri,ntri,pol,zlev,tol,tolsq)
**       Determine if there is a point of a surface boundary both above
**       the given Z-level and inside the given polygon.
**    PARAMETERS
**       INPUT :
**                bound    - surface boundary structure
**                pol      - polygon structure
**                zlev     - current Z-level
**                tol      - tolerance
**       OUTPUT:  none
**
**    RETURNS      : UU_TRUE if there is such point; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_tess_overhangs(ptri,ntri,pol,zlev,tol,tolsq)
int ntri;
UM_trian *ptri;
ncl_polygon *pol;
UU_REAL zlev,tol,tolsq;
{
	int i,ichk=-1;
	UM_coord pti;

	for (i = 0; i < ntri; i++)
	{
		um_vctovc (ptri[i].p1,pti);
		if (pti[2] >= zlev &&
			ncl_check_inside(pti,pol,tol,&ichk) == 1) return(UU_TRUE);
		um_vctovc (ptri[i].p2,pti);
		if (pti[2] >= zlev &&
			ncl_check_inside(pti,pol,tol,&ichk) == 1) return(UU_TRUE);
		um_vctovc (ptri[i].p3,pti);
		if (pti[2] >= zlev &&
			ncl_check_inside(pti,pol,tol,&ichk) == 1) return(UU_TRUE);
	}

	return(UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_overhangs(bound,pol,zlev,tol)
**       Determine if there is a point of a surface boundary both above
**       the given Z-level and inside the given polygon.
**    PARAMETERS
**       INPUT :
**                bound    - surface boundary structure
**                pol      - polygon structure
**                zlev     - current Z-level
**                tol      - tolerance
**       OUTPUT:  none
**
**    RETURNS      : UU_TRUE if there is such point; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_overhangs(bound,pol,zlev,tol,tolsq,sftype)
UM_srf_bound *bound;
ncl_polygon *pol;
UU_REAL zlev,tol,tolsq;
ncl_waterline_sftype sftype;
{
	int i,ib,np,j,m,MMAX;
	UU_LOGICAL ichk=-1;
	UM_coord *pts;
	UM_2Dcoord *vtx;
	UU_REAL d,del,a,b;
	UU_REAL DEL = 10*tol;
	UU_REAL eps = 121*tolsq;
	UM_coord vti;
	char sbuf[80];

	if (bound->npo < 4) return(UU_FALSE);
	pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
	np = 0;
	MMAX = 20;

	for (ib = 0; ib < bound->nb; ib++, pts += np)
	{
		if (ib > 0)
		{
			np = bound->np[ib-1];
			if (np < 0)
			{
				np = -np;
				continue;
			}

			for (i = 0; i < np - 1; i++)
			{
				if (pts[i][2] >= zlev &&
					ncl_check_inside(pts[i],pol,tol,&ichk) == 1) return(UU_TRUE);
			}
		}
		else
		{
			np = bound->npo;
			for (i = 0; i < np - 1; i++)
			{
				if (pts[i][2] >= zlev || pts[i+1][2] >= zlev)
				{
					if (pts[i][2] >= zlev &&
						ncl_check_inside(pts[i],pol,tol,&ichk) == 1) return(UU_TRUE);

					d = UM_SQDIS_2D(pts[i],pts[i+1]);
					if (d > eps)
					{
						d = sqrt(d);
						m = d/DEL + 0.5;
						if (m > MMAX) m = MMAX;
						del = 1./m;

						for (j = 1, a = 0, b = 1; j < m; j++)
						{
							a += del; b -= del;
							um_avcplbvc (b,pts[i],a,pts[i+1],vti);
							if (vti[2] >= zlev &&
								ncl_check_inside(vti,pol,tol,&ichk) == 1)
									return(UU_TRUE);
						}
					}
				}
			}
		}
	}
/*
.....The surface boundary was inside an outer polygon
.....but also inside an inner polygon (island).
.....We need to check if any outer polygons
.....are wholly within the surface boundary
.....QAR 99143
*/
	if (ichk != -1 && sftype == HORZPL)
	{
		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
		np = 0;
		for (ib = 0; ib < ichk; ib++, pts += np)
		{
			np = pol->np[ib];
			if (np < 0)
			{
				np = -np;
				continue;
			}
			for (i = 0; i < np - 1; i++)
			{
				if (S_inside_bound(vtx[i],bound,tol) == 1)
				{
#if 0
					NclxDbgPstr("erase/all");
					sprintf(sbuf,"PT/%lf,%lf",vtx[i][0],vtx[i][1]);
					NclxDbgPstr(sbuf);
					ncl_debug_sfbound(0,bound);
					ncl_debug_polygon(UU_NULL,pol,4);
#endif
					return(UU_TRUE);
				}
			}
		}
	}
	return(UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_overhangs1(sfi,pol,zlev,tol,tolsq)
**       Determine if there is a point of a surface boundary both above
**       the given Z-level and inside the given polygon.
**    PARAMETERS
**       INPUT :
**                bound    - surface boundary structure
**                pol      - polygon structure
**                zlev     - current Z-level
**                tol      - tolerance
**       OUTPUT:  none
**
**    RETURNS      : UU_TRUE if there is such point; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_overhangs1(sfi,pol,zlev,tol,tolsq)
NCL_waterline_surf *sfi;
ncl_polygon *pol;
UU_REAL zlev,tol,tolsq;
{
	int ntri;
	UM_trian *ptri;

#if 0
	if (sfi->key == 4011 && zlev < -5.5)
	{
		ncl_debug_sfbound(sfi->key,sfi->bound);
		ncl_debug_polygon(UU_NULL,pol,4);
	}
#endif
	if (sfi->flag1 == VSOLID)
	{
		ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
		ntri = sfi->trianlist->cur_cnt;
		if (ntri < 1) return(UU_FALSE);
		return (ncl_tess_overhangs(ptri,ntri,pol,zlev,tol,tolsq));
	}
	else
	{
		return (ncl_overhangs(&sfi->bound,pol,zlev,tol,tolsq,sfi->sf_flag));
	}
}

/*********************************************************************
**    E_FUNCTION     : S_box_test (pol,tol)
**       Weed out polygon contours with too narrow boxes. Used when watrev
**       is active
**    PARAMETERS
**       INPUT  :
**          pol        - polygon structure
**          tol        - tolerance
**       OUTPUT :
**          pol        - updated polygon structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_box_test (pol,tol)
UU_REAL tol;
ncl_polygon *pol;
{
	int nc,c,nv,nv0,iv;
	UM_2Dcoord *vtx;
	UU_REAL xmin,xmax,ymin,ymax,dx,dy;

	nc = pol->num_contours;
	if (nc <= 0) return;

	for (c = 0, nv0 = 0; c < nc; c++)
	{
		nv = pol->np[c];
		if (nv <= 0)
		{
			nv = -nv;
			nv0 += nv;
			continue;
		}
		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
		vtx += nv0;

		xmin = xmax = vtx[0][0];
		ymin = ymax = vtx[0][1];
		for (iv = 1; iv < nv; iv++)
		{
			if (xmin > vtx[iv][0]) xmin = vtx[iv][0];
			else if (xmax < vtx[iv][0]) xmax = vtx[iv][0];
			if (ymin > vtx[iv][1]) ymin = vtx[iv][1];
			else if (ymax < vtx[iv][1]) ymax = vtx[iv][1];
		}
		dx = xmax - xmin; dy = ymax - ymin;

		if (dx < tol || dy < tol)
		{
			ncl_delete_contour (nc,c,nv0,nv,pol);
			nc--; c--;
		}
		else
		{
			nv0 += nv;
		}
	}

	pol->num_contours = nc;

	return;
}

/*********************************************************************
**    I_FUNCTION     : UU_LOGICAL S_contours_close (pp0,nv0,pp1,nv1,eps)
**       Find if two polygon contours are within tolerance.
**    PARAMETERS
**       INPUT  :
**          pp0,nv0     - first contour
**          pp1,nv1     - second contour
**          eps         - squared tolerance
**       OUTPUT : none
**    RETURNS      : UU_TRUE if within tolerance; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_contours_close (pp0,nv0,pp1,nv1,eps)
UM_2Dcoord *pp0,*pp1;
int nv0,nv1;
UU_REAL eps;
{
	int i;

	for (i = 0; i < nv0; i++)
	{
		if (um_point_close (pp1,nv1,pp0[i],eps))
			return (UU_TRUE);
	}

	return (UU_FALSE);
}

/*********************************************************************
**       Put a contour in a single-contour polygon.
*********************************************************************/
static void S_create_hpol0 (vtx,nv,pol)
int nv;
UM_2Dcoord *vtx;
ncl_polygon *pol;
{
	int iv;

	pol->num_contours = 1;
	UU_FREE (pol->np);
	pol->np = (int *) uu_malloc(sizeof(int));
	pol->np[0] = nv;

	UU_LIST_EMPTY (pol->contour);

	for (iv = 0; iv < nv; iv++)
		uu_list_push (pol->contour,vtx[iv]);
}

/*********************************************************************
**       Update the box for a modified contour.
*********************************************************************/
static void S_update_box (vtx,nv,box)
int nv;
UM_2Dcoord *vtx;
UM_2box *box;
{
	int iv;

	for (iv = 0; iv < nv; iv++)
	{
		ncl_update_minmax (vtx[iv],&box->xmin,&box->xmax,&box->ymin,&box->ymax);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_no_gap (ccur,nc,vt0,vtcur,nvcur,cpol,pol,eps)
**       For a current polygon, find a large hole within tolerance to
**       the hole ccur
**    PARAMETERS
**       INPUT  :
**          cpol        - current polygon
**          nc          - number of contours in cpol
**          ccur        - current hole index
**          vt0         - cpol vertices
**          vtcur,nvcur - ccur contour
**          eps         - squared tolerance
**       OUTPUT :
**          pol         - if a close hole is found, its contour is stored here
**    RETURNS      :
**         index of a close hole, -1 if not found
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_no_gap (ccur,nc,vt0,vtcur,nvcur,cpol,pol,eps)
int nc,ccur,nvcur;
UM_2Dcoord *vtcur,*vt0;
ncl_polygon *cpol,*pol;
UU_REAL eps;
{
	int c,nv,nv0;
	UM_2Dcoord *vtx;

	for (c = 0, nv0 = 0; c < nc; c++, nv0 += nv)
	{
		nv = cpol->np[c];
		if (nv >= 0) continue;
		nv = -nv;
		if (c == ccur || nv <= 16) continue;
		vtx = vt0 + nv0;
		if (S_contours_close (vtcur,nvcur,vtx,nv,eps))
		{
			S_create_hpol0 (vtx,nv,pol);
			return (c);
		}
	}

	return (-1);
}

/*********************************************************************
**    I_FUNCTION     : S_close_gaps1 (nc,c0,c1,curpol,pol)
**       For a current polygon, remove the hole c0, update the hole c1 points
**       using contour stored as pol
**    PARAMETERS
**       INPUT  :
**          curpol      - current polygon
**          pol         - polygon struct holding new c1-hole
**          c0          - hole to remove
**          c1          - hole to update
**       OUTPUT :
**          curpol      - updated polygon
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_close_gaps1 (nc,c0,c1,curpol,pol)
int nc,c0,c1;
ncl_polygon *curpol,*pol;
{
	int c,nv,nv0,nv1;
	UM_2Dcoord *vt1;

	vt1 = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
	nv1 = pol->np[0];

	for (c = 0, nv0 = 0; c < nc; c++, nv0 += nv)
	{
		nv = abs(curpol->np[c]);
		if (c == c1)
		{
			uu_list_delete (curpol->contour,nv0,nv);
			uu_list_insert_multiple (curpol->contour,nv0,nv1,vt1);
			curpol->np[c] = -nv1;
			break;
		}
	}

	for (c = 0, nv0 = 0; c < nc; c++, nv0 += nv)
	{
		nv = abs(curpol->np[c]);
		if (c == c0)
		{
			ncl_delete_contour (nc,c,nv0,nv,curpol);
			curpol->num_contours--;
			break;
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_close_gaps (curpol,pol0,tolsq)
**       For a current polygon, get rid of small holes located within tolerance
**       to large holes
**    PARAMETERS
**       INPUT  :
**          curpol      - current polygon
**          pol,pol0    - polygon structs to use
**          tol         - tolerance
**          tolsq       - squared tolerance
**       OUTPUT :
**          curpol      - updated polygon
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_close_gaps (curpol,pol0,pol,tol,tolsq)
ncl_polygon *curpol,*pol0,*pol;
UU_REAL tol,tolsq;
{
	int nc,c,c1,nv,nv0,istat;
	UU_LOGICAL lfixed;
	UM_2Dcoord *vt0,*vtx;
	UU_REAL area,eps,smallar;

	eps = 0.01*tolsq;
	smallar = 100*tolsq;

	nc = curpol->num_contours;
	lfixed = UU_TRUE;

	while (nc > 1 && lfixed)
	{
		vt0 = (UM_2Dcoord *) UU_LIST_ARRAY (curpol->contour);

		lfixed = UU_FALSE;
		for (c = 0, nv0 = 0; c < nc && !lfixed; c++, nv0 += nv)
		{
			nv = curpol->np[c];
			if (nv >= 0) continue;
			nv = -nv;
			if (nv > 10) continue;
			vtx = vt0 + nv0;
			um_polygon_area (nv,vtx,&area);
			if (fabs(area) < smallar) continue;

			c1 = S_no_gap (c,nc,vt0,vtx,nv,curpol,pol0,eps);
			if (c1 >= 0)
			{
				ncl_close_gaps (vtx,nv,pol0,eps);
				S_create_hpol0 (vtx,nv,pol);

				istat = ncl_polygon_clip (NCL_UNION,pol0,pol,pol0,tol,tolsq);

				if (istat == UU_SUCCESS && pol->num_contours == 1)
				{
					S_update_box (vtx,nv,&curpol->box[c1]);
					S_close_gaps1 (nc,c,c1,curpol,pol0);

					nc = curpol->num_contours;
					lfixed = UU_TRUE;
				}
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_check_overhangs (numsf,sff,wbase,zlev,pock)
**       Fix a pocket polygon so that there are no surfaces are above it.
**    PARAMETERS
**       INPUT  :
**          numsf      - number of surfaces
**          sff        - data for each surface
**			wbase	   - base surface data
**          zlev       - Z-level at which the pocket is created
**          pock       - current pocket polygon
**       OUTPUT :
**          pock       - pocket polygon minus overhang projections
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_check_overhangs (numsf,sff,wbase,rot,zlev,zprev,curpock,nio,ptsio,
						 poctype,rsq,tol,tolsq,ztol,NPT,levbetween)
int numsf,NPT,poctype;
NCL_waterline_surf *sff;
NCL_w_base *wbase;
UM_transf rot;
UU_REAL zlev,zprev,rsq,tol,tolsq,ztol;
ncl_polygon *curpock;
UU_LIST *nio,*ptsio;
UU_LOGICAL levbetween;
{
	int isf,status,npts,iv,nv,ib,nb,c;
	NCL_waterline_surf *sfi;
	UM_coord *pts;
	ncl_polygon *curpol,*pol0,*pol;
	UU_LIST *vtxlist;
	UU_LOGICAL merge,doweed;
	int ksf = 0;
	int kupdate = 1, match = 1;
	void ncl_weed_contours();
	UU_LOGICAL ncl_match_boxes();
	UU_REAL areamin = (UM_PI + 2.)*rsq; /* minimum island area */
	UU_REAL zlarge = zlev + ztol;
	UU_REAL zsmall = zlev - ztol;
	char sbuf[80];

	ncl_get_wpol (0,&pol0);
	ncl_get_wpol (1,&pol);
	pol0->num_contours = pol->num_contours = 0;
	status = UU_FAILURE;
	if (pol0->contour == UU_NULL || pol0->contour->data == UU_NULL ||
		pol->contour == UU_NULL || pol->contour->data == UU_NULL) goto Done;
	if (ptinit == 0)
	{
		uu_list_init (&ptlst,sizeof(UM_2Dcoord),NPT,NPT);
		if (!ptlst.data) goto Done;
		ptinit = 1;
	}
	if (trinit == 0)
	{
		int FOUR = 4;
		uu_list_init (&tripolst,sizeof(UM_2Dcoord),FOUR,FOUR);
		if (!tripolst.data) goto Done;
		trinit = 1;
	}
	status = UU_SUCCESS;
	merge = (rsq >= 25.*tolsq);

/*
.....First contour
*/
	for (isf = 0, sfi = sff; isf < numsf; isf++,sfi++)
	{
#if 0
		if (sfi->key == 4011)
		{
			isf = isf;
		}
#endif
		if (sfi->slist != LIST_A && sfi->slist != LIST_C) continue;
		if (sfi->key == NULLKEY) continue;
		if (sfi->flag1 != VSOLID)
		{
			if (sfi->bound.npo < 4) continue;
		}
		if (sfi->sf_flag == ABOVE ||
			sfi->sf_flag == VERT || sfi->sf_flag == VERTPL) continue;
		if (sfi->zmax < zlarge) continue;
/*
.....Determine if a part of the surface boundary is
.....within the pocket area
.....If not, then do not use this surface
*/
		if (!ncl_overhangs1(sfi,curpock,zlarge,tol,tolsq))
		{
			if (sfi->zmin > zsmall && poctype != STK_BOUNDSF && !levbetween)
			{
				sfi->sf_flag = ABOVE;
				UU_LIST_FREE (sfi->trianlist);
			}
			continue;
		}

		vtxlist = pol0->contour;
		vtxlist->cur_cnt = 0;
		curpol = pol0;
/*
.....Determine if projected curve boundary
.....is self-intersecting
*/
		if (sfi->flag1 != VSOLID)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
			nb = sfi->bound.nb; nv = sfi->bound.npo - 1;

			if (sfi->sf_flag == GEN && ncl_need_test())
				ncl_test_projection (&sfi->sf_flag,nv,pts);
		}

/*
.....Projection of curve boundary is self-intersecting
.....So project tessellated surface instead
*/
		doweed = UU_TRUE;
		if (sfi->sf_flag == BADPROJ || sfi->flag1 == VSOLID)
		{
			status = ncl_proj_tess_above (sfi,wbase,rot,tol,tolsq,ztol,curpol,zlev,zprev);
			if (status != UU_SUCCESS) goto Done;
#if 0
			NclxDbgPstr("ERASE/ALL");
			sprintf(sbuf,"isf=%d",isf);
			NclxDbgPstr(sbuf);
			ncl_debug_polygon(UU_NULL,curpol,4); /* GREEN */
			NclxDbgPstr("*SH/isf");
			NclxDbgPstr("*STOP");
#endif
#if 0			
			S_debug_vtxlist(sfi->key,wbase,zlev,vtxlist);
#endif
		}
		else if (sfi->zmin > zsmall)
		{
			doweed = UU_FALSE;

			curpol->num_contours = nb;
			curpol->np = (int *) uu_malloc (nb*sizeof(int));
			for (ib = 0, c = 0; ib < nb; ib++, pts += (nv+1))
			{
				nv = (ib == 0)? sfi->bound.npo - 1: abs(sfi->bound.np[ib-1]) - 1;
				if (ib > 0 && sfi->bound.np[ib-1] < 0)
				{
					curpol->num_contours--;
					continue;
				}
				iv = (ib == 0)? nv: -nv;
				curpol->np[c] = iv;
				c++;
				for (iv = 0; iv < nv; iv++)
					uu_list_push (vtxlist,pts[iv]);
			}
		}
		else if (sfi->ncvs > 0 || sfi->slist == LIST_C)
		{
			status = ncl_proj_bndr_above (sfi,tol,tolsq,ztol,curpol,zlev,nio,ptsio);
			if (status != UU_SUCCESS) goto Done;
		}

		if (doweed)
		{
			if (pol0->num_contours > 2 && merge)
				ncl_area_test (pol0,areamin,tolsq);
			ncl_weed_contours(pol0,tolsq,UU_TRUE);
			if (ncl_is_watrev()) S_box_test (pol0,tol);
		}

		/* do not check it again at lower levels */
		if (sfi->zmin > zsmall && poctype != STK_BOUNDSF && !levbetween)
		{
			sfi->sf_flag = ABOVE;
			UU_LIST_FREE (sfi->trianlist);
		}
		npts = vtxlist->cur_cnt;
		if (npts >= 3)
		{
			ksf = isf + 1;
			break;
		}
	}
	if (pol0->num_contours == 0) goto Done;

	kupdate = 1;
/* isf = 0,1,...,ksf-1 are all checked */
Again:
	for (isf = ksf; isf < numsf && ksf < numsf; isf++)
	{
		sfi = sff + isf;
#if 0
		if (sfi->key == 4011)
		{
			isf = isf;
		}
#endif
		if (sfi->slist != LIST_A && sfi->slist != LIST_C) continue;
		if (sfi->flag1 != VSOLID)
		{
			if (sfi->bound.npo < 4)
			{
				if (kupdate) ksf++; continue;
			}
		}

		if (sfi->key == NULLKEY  || sfi->sf_flag == ABOVE ||
			sfi->sf_flag == VERT || sfi->sf_flag == VERTPL ||
			sfi->zmax < zlarge || sfi->size < 0)
		{
			if (kupdate) ksf++; continue;
		}
/*
..... do not use the surface if no part of its outer boundary is outside of the
..... pocket area
*/
		kupdate = 0;
		if (match == 1)
		{
			if (sfi->flag1 == MANY_HOLES || !ncl_match_boxes(&sfi->box,pol0))
				continue;
		}
#if 0
		if (sfi->key == 18358 || sfi->key == 18362 || sfi->key == 18366 ||
			sfi->key == 18370)
		{
			isf = isf;
		}
#endif
		if (!ncl_overhangs1(sfi,curpock,zlarge,tol,tolsq))
		{
			if (sfi->zmin > zsmall && poctype != STK_BOUNDSF && !levbetween)
			{
				sfi->sf_flag = ABOVE;
				UU_LIST_FREE (sfi->trianlist);
			}
			else
				sfi->size = -1;
			continue;
		}

		vtxlist = pol->contour;
		vtxlist->cur_cnt = 0;
		curpol = pol;

		if (sfi->flag1 != VSOLID)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
			nb = sfi->bound.nb; nv = sfi->bound.npo - 1;

			if (sfi->sf_flag == GEN && ncl_need_test())
				ncl_test_projection (&sfi->sf_flag,nv,pts);
		}

		if (sfi->sf_flag == BADPROJ || sfi->flag1 == VSOLID)
		{
ISF = isf;
			status = ncl_proj_tess_above (sfi,wbase,rot,tol,tolsq,ztol,curpol,zlev,zprev);
#if 0
			if (sfi->key == 19305)
			{
				NclxDbgPstr("ERASE/ALL");
				sprintf(sbuf,"isf=%d",isf);
				NclxDbgPstr(sbuf);
				ncl_debug_polygon(UU_NULL,curpol,4); /* GREEN */
				NclxDbgPstr("*SH/isf");
				NclxDbgPstr("*STOP");
			}
#endif
			if (status != UU_SUCCESS) goto Done;
		}
		else if (sfi->zmin > zsmall)
		{
			curpol->num_contours = nb;
			curpol->np = (int *) uu_malloc (nb*sizeof(int));
			for (ib = 0, c = 0; ib < nb; ib++, pts += (nv+1))
			{
				nv = (ib == 0)? sfi->bound.npo - 1: abs(sfi->bound.np[ib-1]) - 1;
				if (ib > 0 && sfi->bound.np[ib-1] < 0)
				{
					curpol->num_contours--;
					continue;
				}
				iv = (ib == 0)? nv: -nv;
				curpol->np[c] = iv;
				c++;
				for (iv = 0; iv < nv; iv++)
					uu_list_push (vtxlist,pts[iv]);
			}
		}
		else if (sfi->ncvs > 0 || sfi->slist == LIST_C)
		{
			status = ncl_proj_bndr_above (sfi,tol,tolsq,ztol,curpol,zlev,nio,ptsio);
			if (status != UU_SUCCESS) goto Done;
		}

		/* do not check it again at lower levels */
		if (sfi->zmin > zsmall && poctype != STK_BOUNDSF && !levbetween)
		{
			sfi->sf_flag = ABOVE;
			UU_LIST_FREE (sfi->trianlist);
		}
		else
			sfi->size = -1;
		UU_FREE (pol0->box);
		npts = vtxlist->cur_cnt;
		if (npts >= 3)
		{
#if 0
			if (zlev < -5.56)
			{
				NclxDbgPstr("ERASE/ALL");
				sprintf(sbuf,"isf=%d",sfi->key);
				NclxDbgPstr(sbuf);
				ncl_debug_polygon(UU_NULL,pol0,4); /* GREEN */
				ncl_debug_polygon(UU_NULL,pol,2); /* BLUE */
				NclxDbgPstr("*SH/isf");
			}
#endif
			if (ncl_valid_polygon(pol,tol))
				status = ncl_polygon_clip (NCL_UNION,pol0,pol,pol0,tol,tolsq);
			else
				status = UU_SUCCESS;
#if 0
			if (zlev < -5.56)
			{
				ncl_debug_polygon(UU_NULL,pol0,5); /* MAGNTA */
			}
#endif
			UU_FREE (pol->np);

			if (pol0->num_contours > 2 && merge)
				ncl_area_test (pol0,areamin,tolsq);
			ncl_weed_contours(pol0,tolsq,UU_TRUE);
			if (ncl_is_watrev()) S_box_test (pol0,tol);

			if (status != UU_SUCCESS) break;
		}
		if (match == 1)
		{
			kupdate = 1; isf = ksf - 1;
		}
	}
	if (status == UU_SUCCESS && match == 1 && ksf < numsf - 1)
	{
		kupdate = match = 0; goto Again;
	}

	if (pol0->num_contours > 0)
	{
		ncl_calc_boxes (pol0);

#if 0
		NclxDbgPstr("ERASE/ALL");
		ncl_debug_polygon(UU_NULL,curpock,4); /* GREEN */
		ncl_debug_polygon(UU_NULL,pol0,2); /* BLUE */
#endif
		if (ncl_valid_polygon(pol0,tol))
			status = ncl_polygon_clip (NCL_DIFF,curpock,pol0,curpock,tol,4.*tolsq);
		else
		 status = UU_SUCCESS;
#if 0
		ncl_debug_polygon(UU_NULL,curpock,5); /* MAGNTA */
#endif

		if (status != UU_SUCCESS) goto Done;

		if (!ncl_setver(96) && merge)
		{
			UU_FREE (pol0->np); UU_FREE (pol->np);
			UU_FREE (pol0->box);
			S_close_gaps (curpock,pol0,pol,tol,tolsq);
		}
	}

Done:
	UU_FREE (pol0->np); UU_FREE (pol->np);
	UU_FREE (pol0->box);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : S_convert_bound(pstlist,bound)
**			Convert point list into boundary
**    PARAMETERS
**       INPUT  :
**          pstlist		- point list
**       OUTPUT :
**          bound		- boundary
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_convert_bound(pstlist,bound)
UU_LIST pstlist;
UM_srf_bound *bound;
{
	int i,npts;
	UM_coord *pts;

	npts = UU_LIST_LENGTH (&pstlist); 

	bound->nb = 1;
	bound->npo = npts;
	bound->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (bound->cvpts, sizeof(UM_coord), npts, npts);
	uu_list_push_list (bound->cvpts, &pstlist);

	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_trilist(sfkey,trilist)
**		Debug surface boundary data
**    PARAMETERS
**       INPUT  :
**          sfkey		- surface key
**       OUTPUT :
**          lp.lis file when UU_DEBUGL=1 is set
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_trilist(sfkey,trilist)
UU_KEY_ID sfkey;
UU_LIST trilist;
{
	int i,npts;
	UM_trian *ptri;
	char tbuf[80];

	sprintf(tbuf,"$$After Trim sfkey/%d",sfkey);
	NclxDbgPstr(tbuf);

	ptri = (UM_trian *)UU_LIST_ARRAY(&trilist);
	npts = trilist.cur_cnt;
	for (i = 0; i < npts; i++, ptri++)
	{
		if (um_sqdis(ptri->p1,ptri->p2) > UM_DFUZZ)
		{
			sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
				ptri->p1[0], ptri->p1[1], ptri->p1[2],
				ptri->p2[0], ptri->p2[1], ptri->p2[2]);
			NclxDbgPstr(tbuf);
		}

		if (um_sqdis(ptri->p2,ptri->p3) > UM_DFUZZ)
		{
			sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
				ptri->p2[0], ptri->p2[1], ptri->p2[2],
				ptri->p3[0], ptri->p3[1], ptri->p3[2]);
			NclxDbgPstr(tbuf);
		}

		if (um_sqdis(ptri->p1,ptri->p3) > UM_DFUZZ)
		{
			sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
				ptri->p1[0], ptri->p1[1], ptri->p1[2],
				ptri->p3[0], ptri->p3[1], ptri->p3[2]);
			NclxDbgPstr(tbuf);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_contour_stock(pol,wlev)
**		Debug stock contour profiles
**    PARAMETERS
**       INPUT  :
**          pol		- contour
**			wlev	- zlev
**       OUTPUT :
**          lp.lis file when UU_DEBUGL=1 is set
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_contour_stock(pol,wlev)
UU_LIST *pol;
UU_REAL wlev;
{
	int j, np;
	UM_2Dcoord *vtx;
	char tbuf[80];
	int icolor = 4;					
	NclxDbgPstr("DRAFT/MODIFY,COLOR=yellow");
	sprintf(tbuf,"$$Contour stock");
	NclxDbgPstr(tbuf);

	np = pol->cur_cnt;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol);
	for (j=0; j<np-1; j++)
	{
		sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
			vtx[j][0], vtx[j][1], wlev,
			vtx[j+1][0], vtx[j+1][1], wlev);
		NclxDbgPstr(tbuf);
	}
	sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
		vtx[np-1][0], vtx[np-1][1], wlev,
		vtx[0][0], vtx[0][1], wlev);
	NclxDbgPstr(tbuf);
	NclxDbgPstr("*STOP");
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_contour_3D(pol)
**		Debug 3-D stock contour profiles
**    PARAMETERS
**       INPUT  :
**          pol		- contour
**       OUTPUT :
**          lp.lis file when UU_DEBUGL=1 is set
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_contour_3D(pol)
UU_LIST *pol;
{
	int j, np;
	UM_coord *vtx;
	char tbuf[80];
	int icolor = 4;					
	NclxDbgPstr("DRAFT/MODIFY,COLOR=green");
	sprintf(tbuf,"$$Contour stock");
	NclxDbgPstr(tbuf);

	np = pol->cur_cnt;
	vtx = (UM_coord *) UU_LIST_ARRAY (pol);
	for (j=0; j<np-1; j++)
	{
		sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
			vtx[j][0], vtx[j][1], vtx[j][2],
			vtx[j+1][0], vtx[j+1][1], vtx[j+1][2]);
		NclxDbgPstr(tbuf);
	}
	sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
		vtx[np-1][0], vtx[np-1][1], vtx[np-1][2],
		vtx[0][0], vtx[0][1], vtx[0][2]);
	NclxDbgPstr(tbuf);
	NclxDbgPstr("*STOP");
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_slices(sfkey,wbase,wlev)
**		Debug stock contour profiles
**    PARAMETERS
**       INPUT  :
**          sfkey	- surface key
**			wbase	- base surface
**			wlev	- zlev
**       OUTPUT :
**          lp.lis file when UU_DEBUGL=1 is set
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_slices(sfkey,wbase,wlev)
UU_KEY_ID sfkey;
NCL_w_base *wbase;
UU_REAL wlev;
{
	int i,npts,chunks,ncj;
	UM_coord *ptio;
	int *np;
	int j;
	UU_REAL dist;
	char tbuf[80];
	UM_coord spt1,spt2;

	ncl_getpts_uv (&npts,&ptio);
	if (npts <= 0) return;

	chunks = 1;
	ncl_get_nios (&chunks,&np);
	ncj = 0;

	if (chunks > 1)
	{
		for (i = 0; i < chunks; i++)	
		{
			npts = np[i];

			sprintf(tbuf,"$$Slice srf->key/%d,wlev=%8.5f",sfkey,wlev);
			NclxDbgPstr(tbuf);
			sprintf(tbuf,"draft/modify,color = 7");
			NclxDbgPstr(tbuf);
			for (j=ncj; j < ncj+npts-1; j++)
			{						
				if (ncl_is_watrev())
				{
					ncl_wat_evsrf (wbase,ptio[j],wlev,spt1);
					ncl_wat_evsrf (wbase,ptio[j+1],wlev,spt2);
				}
				else
				{
					um_vctovc (ptio[j],spt1);														
					spt1[2] = wlev;
					um_vctovc (ptio[j+1],spt2);
					spt1[2] = wlev;
				}

				dist = um_sqdis(spt1,spt2);
				if (dist > 400*UM_DFUZZ)
				{
					sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
						spt1[0], spt1[1], spt1[2],spt2[0], spt2[1], spt2[2]);
					NclxDbgPstr(tbuf);
				}
			}
			ncj += npts;
		}
	}
	else
	{
		sprintf(tbuf,"$$Slice srf->key/%d",sfkey);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"draft/modify,color = 7");
		NclxDbgPstr(tbuf);
		for (j=0; j < npts-1; j++)
		{						
			if (ncl_is_watrev())
			{
				ncl_wat_evsrf (wbase,ptio[j],wlev,spt1);
				ncl_wat_evsrf (wbase,ptio[j+1],wlev,spt2);
			}
			else
			{
				um_vctovc (ptio[j],spt1);													
				spt1[2] = wlev;
				um_vctovc (ptio[j+1],spt2);
				spt2[2] = wlev;
			}

			dist = um_sqdis(spt1,spt2);
			if (dist > 400*UM_DFUZZ)
			{
				sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
					spt1[0], spt1[1], spt1[2],spt2[0], spt2[1], spt2[2]);
				NclxDbgPstr(tbuf);
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_overhang(pock,wbase,wlev)
**		Debug stock contour profiles
**    PARAMETERS
**       INPUT  :
**          pock	- pock polygon contour
**			wbase	- base surface
**			wlev	- zlev
**       OUTPUT :
**          lp.lis file when UU_DEBUGL=1 is set
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_overhang(pock,wbase,wlev)
ncl_polygon *pock;
NCL_w_base *wbase;
UU_REAL wlev;
{
	int i,j, nnl,nnl0,np,nloops,*nps;
	UU_REAL dist;
	UM_2Dcoord *vtx;
	UM_coord spt1,spt2;
	char tbuf[80];
	int icolor = 4;
	nps = (int *) pock->np;
	nloops = pock->num_contours;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pock->contour);

	sprintf(tbuf,"draft/modify,color = 6");
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"*stop");
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"$$After Overhang");
	NclxDbgPstr(tbuf);
	nnl0 = 0;
	for (i = 0; i < nloops; i++)
	{
		sprintf(tbuf,"$$Contour number= %d",i);
		NclxDbgPstr(tbuf);
		nnl = abs(nps[i]);
		for (j = nnl0; j < nnl0+nnl-1; j++)
		{
			if (ncl_is_watrev())
			{
				ncl_wat_evsrf (wbase,vtx[j],wlev,spt1);
				ncl_wat_evsrf (wbase,vtx[j+1],wlev,spt2);
			}
			else
			{
				um_vctovc (vtx[j],spt1);
				spt1[2] = wlev;
				um_vctovc (vtx[j+1],spt2);
				spt2[2] = wlev;
			}

			dist = um_sqdis(spt1,spt2);
			if (dist > 400*UM_DFUZZ)
			{
				sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
					spt1[0], spt1[1], spt1[2],spt2[0], spt2[1], spt2[2]);
				NclxDbgPstr(tbuf);
			}
		}
		nnl0 +=nnl;
	}
}

/**************************************************************************
**    E_FUNCTION     : int ncl_isect_bound(srf,tfmat,bound,rot,irot,
**									uv1,pt1,uv2,pt2,tol,point)
**       Insect line(pt1,pt2) with boundary
**					
**    PARAMETERS
**       INPUT  :
**          srf		   - surface
**          tfmat	   - transformation matrix of input surface
**          bound	   - surface boundary
**          rot		   - transformation matrix (world to part)
**			irot	   - tranformation flag
**          uv1,pt1	   - point inside boundary
**          uv2,pt2	   - point outside boundary
**          tol		   - tolerance
**       OUTPUT : 
**          point	   - intersected point
**    RETURNS      :
**         1 iff intersection found; 0 else
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_isect_bound(srf,tfmat,bound,rot,irot,uv1,pt1,uv2,pt2,tol,point)
struct NCL_fixed_databag *srf;
UM_srf_boundary *bound;
UM_transf tfmat,rot;
int irot;
UM_coord uv1,pt1,uv2,pt2,point;
UU_REAL tol;
{
	int ix,jx,status,status1;
	UM_2Dcoord vln;
	UU_LIST iobuf;
	UM_pointd *uvpd1;
	UU_REAL d,u1,v1;
	UU_LOGICAL linsert;
	UM_coord point1;
	UU_REAL sqdis1;
	UU_REAL sqdismin = 100000.0;

	struct UM_evsrfout *evsrf;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	status = uc_init_evsrfout(srf, evsrf);
	if (status != UU_SUCCESS)
		return (status);

	uu_list_init (&iobuf, sizeof(UM_pointd), 4, 4);

	status = UU_FAILURE;
	linsert = UU_FALSE;

	um_vcmnvc_2d (uv2,uv1,vln);
	d = UM_MAG_2D (vln);
	if (d < UM_FUZZ)
		ix = 0;
	else
	{
		vln[0] /= d; vln[1] /= d;
		ix = um_isect_cls_bndry (uv1,uv2,vln,d,bound,tol,&iobuf);
	}

	if (ix > 0)
	{
		status = UU_SUCCESS;
		uvpd1 = (UM_pointd *) UU_LIST_ARRAY (&iobuf);
/*
.....Get the nearest intersection from uv1(not include uv1)
*/
		for (jx = 0; jx < ix; jx++)
		{
			if (um_sqdis_2d(uv1,uvpd1[jx].point) < 400 * tol * tol)
				continue;
			linsert = (uvpd1[jx].param >= 2.0 * UM_DFUZZ && d - uvpd1[jx].param >= 0.0);
			if (linsert)
			{
				sqdis1 = um_sqdis_2d(uv1,uvpd1[jx].point);
				if (sqdis1 < sqdismin)
				{
					sqdismin = sqdis1;
					u1 = uvpd1[jx].point[0];
					v1 = uvpd1[jx].point[1];
				}
			}
		}
	
		if (linsert)
		{
			status1 = uc_evsrf (UM_POINT,u1,v1,srf,tfmat,evsrf);
			if (status1 == UU_SUCCESS)
			{
				if (irot > 0)
					um_cctmtf(evsrf->sp,rot,point1);
				else
					um_vctovc (evsrf->sp,point1);
			}
			else
				um_vctovc (pt1,point1);
		}
		else	
			um_vctovc (pt1,point1);
	}
	else				
		um_vctovc (pt1,point1);

	uu_free(evsrf);
	uu_list_free (&iobuf);

	um_vctovc (point1,point);

	return status;
}

/**************************************************************************
**    E_FUNCTION     : int ncl_check_isect_bound(srf,tfmat,bound,rot,irot,
**									uv1,pt1,uv2,pt2,tol)
**       Check if line(pt1,pt2) intersect with boundary
**					
**    PARAMETERS
**       INPUT  :
**          srf		   - surface
**          tfmat	   - transformation matrix of input surface
**          bound	   - surface boundary
**          rot		   - transformation matrix (world to part)
**			irot	   - tranformation flag
**          uv1,pt1	   - point on/inside boundary
**          uv2,pt2	   - point on/inside boundary
**          tol		   - tolerance
**       OUTPUT : 
**          pt		   - intersection point
**    RETURNS      :
**         0 : if no interection betwwen line(pt1,pt2) and boundary
**		   1 : if pt1 is actually outside the boundary
**		   2 : if pt2 is actually ousside the boundary
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_check_isect_bound(srf,tfmat,bound,rot,irot,uv1,pt1,uv2,pt2,tol,uvio,ptio)
struct NCL_fixed_databag *srf;
UM_srf_boundary *bound;
UM_transf tfmat,rot;
int irot;
UM_coord uv1,pt1,uv2,pt2,uvio,ptio;
UU_REAL tol;
{
	int ix,jx,status,status1,insf1,insf2;
	UM_2Dcoord vln;
	UU_LIST iobuf;
	UM_pointd *uvpd;
	UU_REAL d,u1,v1;
	UU_LOGICAL linsert;
	UM_coord point1,uvi1,uvi2;
	UU_REAL sqdis1,tol1;
	UU_REAL sqdismin = 100000.0;

	struct UM_evsrfout *evsrf;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	status = uc_init_evsrfout(srf, evsrf);
	if (status != UU_SUCCESS)
		return (status);

	uu_list_init (&iobuf, sizeof(UM_pointd), 4, 4);
		
	status = UU_SUCCESS;
	linsert = UU_FALSE;
	tol1 = 0.1 * tol;

	um_vcmnvc_2d (uv2,uv1,vln);
	d = UM_MAG_2D (vln);
	if (d < UM_FUZZ)
		ix = 0;
	else
	{
		vln[0] /= d; vln[1] /= d;
		ix = um_isect_cls_bndry (uv1,uv2,vln,d,bound,tol1,&iobuf);
	}

	if (ix > 0)
	{
		uvpd = (UM_pointd *) UU_LIST_ARRAY (&iobuf);
		for (jx = 0; jx < ix; jx++)
		{
			if (um_sqdis_2d(uv1,uvpd[jx].point) < UM_DFUZZ ||
				um_sqdis_2d(uv2,uvpd[jx].point) < UM_DFUZZ)
				continue;
			linsert = (uvpd[jx].param >= 0.0 && d - uvpd[jx].param >= 0.0);

/*
.....Check if the middle point is inside boundary or not
*/
			um_middlept (uv1,uvpd[jx].point,uvi1);
			status = uc_evsrf (UM_POINT,uvi1[0],uvi1[1],srf,tfmat,evsrf);
			if (status != UU_SUCCESS)
				goto Done;
			insf1 = um_inside_bndr (uvi1,evsrf->sp,bound,&tol,UU_TRUE);

			if (insf1 == -1)
			{
				um_vctovc(uvpd[jx].point,uvio);
				status = uc_evsrf (UM_POINT,uvpd[jx].point[0],
								uvpd[jx].point[1],srf,tfmat,evsrf);
				if (status == UU_SUCCESS)
					um_vctovc(evsrf->sp,ptio);
				status = 1;
				goto Done;
			}

			um_middlept (uv2,uvpd[jx].point,uvi2);
			status = uc_evsrf (UM_POINT,uvi2[0],uvi2[1],srf,tfmat,evsrf);
			if (status != UU_SUCCESS)
				goto Done;
			insf2 = um_inside_bndr (uvi2,evsrf->sp,bound,&tol,UU_TRUE);

			if (insf2 == -1)
			{
				um_vctovc(uvpd[jx].point,uvio);
				status = uc_evsrf (UM_POINT,uvpd[jx].point[0],
								uvpd[jx].point[1],srf,tfmat,evsrf);
				if (status == UU_SUCCESS)
					um_vctovc(evsrf->sp,ptio);
				status = 2;
				goto Done;
			}
		}
	}

Done:

	uu_free(evsrf);
	uu_list_free (&iobuf);

	return status;
}

/**************************************************************************
**    E_FUNCTION     : int ncl_check_isect_bound1(srf,tfmat,bound,rot,irot,
**									uv1,pt1,uv2,pt2,tol)
**       Check if line(pt1,pt2) intersect with boundary
**					
**    PARAMETERS
**       INPUT  :
**          srf		   - surface
**          tfmat	   - transformation matrix of input surface
**          bound	   - surface boundary
**          rot		   - transformation matrix (world to part)
**			irot	   - tranformation flag
**          uv1,pt1	   - point on/inside boundary
**          uv2,pt2	   - point on/inside boundary
**          tol		   - tolerance
**       OUTPUT : 
**          pt		   - intersection point
**    RETURNS      :
**			the number of intersection points
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_check_isect_bound1(srf,tfmat,bound,rot,irot,uv1,pt1,uv2,pt2,tol,uviobuf,ptiobuf)
struct NCL_fixed_databag *srf;
UM_srf_boundary *bound;
UM_transf tfmat,rot;
int irot;
UM_coord uv1,pt1,uv2,pt2;
UU_REAL tol;
UU_LIST *uviobuf,*ptiobuf;
{
	int isect,ix,jx,status,status1,insf1,insf2;
	UM_2Dcoord vln;
	UU_LIST iobuf;
	UM_pointd *uvpd;
	UU_REAL d;
	UU_LOGICAL linsert;
	UM_coord uvio,ptio;
	UU_REAL sqdis1,tol1;
	UU_REAL sqdismin = 100000.0;

	struct UM_evsrfout *evsrf;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	status = uc_init_evsrfout(srf, evsrf);
	if (status != UU_SUCCESS)
		return (status);

	uu_list_init (&iobuf, sizeof(UM_pointd), 4, 4);

	isect = 0;
	status = UU_FAILURE;
	linsert = UU_FALSE;
	tol1 = 0.1 * tol;

	um_vcmnvc_2d (uv2,uv1,vln);
	d = UM_MAG_2D (vln);
	if (d < UM_FUZZ)
		ix = 0;
	else
	{
		vln[0] /= d; vln[1] /= d;
		ix = um_isect_cls_bndry (uv1,uv2,vln,d,bound,tol1,&iobuf);
	}

	if (ix > 0)
	{
		status = UU_SUCCESS;
		uvpd = (UM_pointd *) UU_LIST_ARRAY (&iobuf);
		for (jx = 0; jx < ix; jx++)
		{
			if (um_sqdis_2d(uv1,uvpd[jx].point) < UM_DFUZZ ||
				um_sqdis_2d(uv2,uvpd[jx].point) < UM_DFUZZ)
				continue;
			linsert = (uvpd[jx].param >= 0.0 && d - uvpd[jx].param >= 0.0);

			if (linsert)
			{
				um_vctovc(uvpd[jx].point,uvio);
				status = uc_evsrf (UM_POINT,uvpd[jx].point[0],
								uvpd[jx].point[1],srf,tfmat,evsrf);
				if (status == UU_SUCCESS)
					um_vctovc(evsrf->sp,ptio);

				if (status == UU_SUCCESS)
				{
					uu_list_push (uviobuf,&uvio);
					uu_list_push (ptiobuf,&ptio);
					isect++;
				}
			}
		}
	}

Done:

	uu_free(evsrf);
	uu_list_free (&iobuf);

	return isect;
}

/**************************************************************************
**    E_FUNCTION     : int ncl_check_isect_bound2(bound,uv1,pt1,uv2,pt2,tol)
**       Check if line(pt1,pt2) intersect with boundary
**					
**    PARAMETERS
**       INPUT  :
**          bound	   - surface boundary
**          uv1,pt1	   - line start point
**          uv2,pt2	   - line end point
**          tol		   - tolerance
**       OUTPUT : 
**         none
**    RETURNS      :
**			the number of intersection points
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_check_isect_bound2(bound,uv1,pt1,uv2,pt2,tol)
UM_srf_boundary *bound;
UM_coord uv1,pt1,uv2,pt2;
UU_REAL tol;
{
	int ix;
	UU_REAL d,tol1;
	UM_2Dcoord vln;
	UU_LIST iobuf;
	uu_list_init (&iobuf, sizeof(UM_pointd), 4, 4);

	ix = 0;
	tol1 = 0.1 * tol;

	um_vcmnvc_2d (uv2,uv1,vln);
	d = UM_MAG_2D (vln);
	if (d < UM_FUZZ)
		ix = 0;
	else
	{
		vln[0] /= d; vln[1] /= d;
		ix = um_isect_cls_bndry (uv1,uv2,vln,d,bound,tol1,&iobuf);
	}

Done:
	uu_list_free (&iobuf);

	return ix;
}

/**************************************************************************
**    E_FUNCTION     : int ncl_tri_inside_boundary(bound,ptri,uvtri,tol)
**       Check if trianlge(ptri,uvtri) inside boundary
**					
**    PARAMETERS
**       INPUT  :
**          bound	   - surface boundary
**          ptri       - the point triangle
**          uvtri1     - the uv-point triangle
**          tol		   - tolerance
**       OUTPUT : 
**         none
**    RETURNS      :
**			the number of intersection points
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_tri_inside_boundary(bound,ptri,uvtri,tol)
UM_srf_boundary *bound;
UM_trian *ptri;
UM_trian *uvtri;
UU_REAL tol;
{
	int status,intsf,ix;
	status = UU_FAILURE;
	intsf = 0;
	ix = 0;

/*
...	Check if the three vertex inside boundary
*/					
	intsf = um_inside_bndr (uvtri->p1,ptri->p1,bound,&tol,UU_TRUE);
	if (intsf < 0)
		return status;
	intsf = um_inside_bndr (uvtri->p2,ptri->p2,bound,&tol,UU_TRUE);
	if (intsf < 0)
		return status;
	intsf = um_inside_bndr (uvtri->p3,ptri->p3,bound,&tol,UU_TRUE);
	if (intsf < 0)
		return status;

	ix = ncl_check_isect_bound2(bound,uvtri->p1,ptri->p1,uvtri->p2,ptri->p2,tol);
	if (ix > 0)
		return status;

	ix = ncl_check_isect_bound2(bound,uvtri->p2,ptri->p2,uvtri->p3,ptri->p3,tol);
	if (ix > 0)
		return status;
	
	status = ncl_check_isect_bound2(bound,uvtri->p1,ptri->p1,uvtri->p3,ptri->p3,tol);
	if (ix > 0)
		return status;

	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_polygon(srf,pock,icolor)
**		Debug stock contour profiles
**    PARAMETERS
**       INPUT  :
**			srf     - surface data
**          pock	- pock contour
**			icolor	- color index
**       OUTPUT :
**          lp.lis file when UU_DEBUGL=1 is set
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_polygon(srf,pock,icolor)
struct NCL_fixed_databag *srf;
ncl_polygon *pock;
int icolor;
{
	int i,j,inc,ist;
	UU_REAL zlev;
	char tbuf[80];		
		
	UM_2Dcoord *pts;
	int status = UU_SUCCESS;

	if (pock->num_contours <= 0) return;
	pts = (UM_2Dcoord *) UU_LIST_ARRAY(pock->contour);
	ncl_get_wlev(&zlev);

	if (srf != UU_NULL)
	{
		sprintf(tbuf,"$$srf->key/%d",srf->key);
		NclxDbgPstr(tbuf);
	}
			
	sprintf(tbuf,"draft/modify,color = %d",icolor);			
	NclxDbgPstr(tbuf);

	inc = 0;
	for (i=0;i<pock->num_contours;i++)
	{
		ist = inc;
		for (j=0;j<abs(pock->np[i])-1;j++)
		{
			if (fabs(pts[inc][0]-pts[inc+1][0]) > .0009 ||
				fabs(pts[inc][1]-pts[inc+1][1]) > .0009)
			{
				sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
					pts[inc][0],pts[inc][1],zlev,pts[inc+1][0],pts[inc+1][1],zlev);
				NclxDbgPstr(tbuf);
			}
			else
			{
				sprintf(tbuf,"PT/%8.5f,%8.5f,%8.5f",pts[inc][0],pts[inc][1],zlev);
				NclxDbgPstr(tbuf);
			}
			inc++;
		}
		if (fabs(pts[inc][0]-pts[ist][0]) > .0009 ||
			fabs(pts[inc][1]-pts[ist][1]) > .0009)
		{
				sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
					pts[inc][0],pts[inc][1],zlev,pts[ist][0],pts[ist][1],zlev);
			NclxDbgPstr(tbuf);
		}
		else
		{
			sprintf(tbuf,"PT/%8.5f,%8.5f,%8.5f",pts[inc][0],pts[inc][1],zlev);
			NclxDbgPstr(tbuf);
		}
		inc++;
	}
	sprintf(tbuf,"*stop");
	NclxDbgPstr(tbuf);
}

/**************************************************************************
**    E_FUNCTION     : int ncl_trim_tess_bound(srf,tfmat,rot,irot,tol,trilist)
**
**       trim base surface tessellation within the boundary
**    PARAMETERS
**       INPUT  :
**          srf		   - surface
**          tfmat	   - surface transformation matrix
**          rot		   - transformation matrix (world to part)
**			irot	   - world to part tranformation flag
**          tol        - tolerance
**       OUTPUT : 
**          trilist    - trimmed triangles list 
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_trim_tess_bound(srf,tfmat,sfbndr,rot,irot,tol,trilist)
struct NCL_fixed_databag *srf;
UM_srf_boundary *sfbndr;
UM_transf tfmat,rot;
int irot;
UU_REAL tol;
UU_LIST *trilist;
{
	int status,status1,ntri,i;
	UM_trian *ptri,*uvtri,*ptri1;
	UU_LIST *trianlst,*uvtrilst;
	ncl_polygon pockbndr;

	ncl_init_polygon (&pockbndr,0);

/*
.....Get the base surface trianglst and uvtrilst
*/
#if 0
	if (srf->key == 19305)
	{
		ntri = 0;
	}
#endif
	status = nclc_tessmgr_get_tess_trianlst(srf->key,&trianlst,&uvtrilst);	
	ntri = UU_LIST_LENGTH(trianlst);
	if (ntri < 1) return UU_FAILURE;

	ptri = (UM_trian *)UU_LIST_ARRAY(trianlst);
	uvtri = (UM_trian *)UU_LIST_ARRAY(uvtrilst);	

/*
.....Convert boundary uvpts into polygon structure
*/
	ncl_sfbound_to_contour(sfbndr,&pockbndr,UU_TRUE);
	ncl_weed_contours(&pockbndr,tol*tol,UU_TRUE);


/*
.....trim triangles within boundary polygon
*/
	for (i = 0; i < ntri; i++, ptri++,uvtri++)
	{	
		status1 = ncl_trim_tri_bound(srf,tfmat,&pockbndr,rot,
							irot,ptri,uvtri,tol,trilist);
	}

	ncl_free_polygon (&pockbndr);

	return status;
}

/**************************************************************************
**    E_FUNCTION     : S_trian_to_contour(ptri,pock)
**       Convert sf boundary contours into polygon structure
**    PARAMETERS
**       INPUT  :
**			bound	- sf boundary
**       OUTPUT : 
**          pock    - polygon structure
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
static void S_trian_to_contour(ptri,pock)
UM_trian *ptri;
ncl_polygon *pock;
{
	int iv;		
	UM_coord *uvs;
	UU_REAL xmin,xmax,ymin,ymax;
	UM_2Dcoord pt1,pt2,pt3;

	pock->np = (int *) uu_malloc (sizeof(int));
	pock->box = (UM_2box *) uu_malloc (sizeof(UM_2box));
	if (pock->np == UU_NULL || pock->box == UU_NULL) return;

	pock->num_contours = 1;
	pock->contour = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (pock->contour, sizeof(UM_2Dcoord), 10, 10);

	xmin = MIN3(ptri->p1[0],ptri->p2[0],ptri->p3[0]);
	ymin = MIN3(ptri->p1[1],ptri->p2[1],ptri->p3[1]);
	xmax = MAX3(ptri->p1[0],ptri->p2[0],ptri->p3[0]);
	ymax = MAX3(ptri->p1[1],ptri->p2[1],ptri->p3[1]);

	pock->np[0] = 3;
	pock->box[0].xmin = xmin;
	pock->box[0].xmax = xmax;
	pock->box[0].ymin = ymin;
	pock->box[0].ymax = ymax;
	
	um_vctovc_2d(ptri->p1,pt1);
	um_vctovc_2d(ptri->p2,pt2);
	um_vctovc_2d(ptri->p3,pt3);

	uu_list_push (pock->contour,pt1);
	uu_list_push (pock->contour,pt2);
	uu_list_push (pock->contour,pt3);	
}

/**************************************************************************
**    E_FUNCTION     : ncl_contour_to_bound(srf,tfmat,rot,irot,tol,pock,pstlist)
**
**       Convert contour uv points into xyz points
**
**    PARAMETERS
**       INPUT  :
**          srf		   - surface
**          tfmat	   - transformation matrix of input surface
**          rot		   - transformation matrix (world to part)
**			irot	   - tranformation flag
**          tol		   - tolerance
**       OUTPUT : 
**          pstlist	   - updated boundary list if the trim is valid
**    RETURNS      :
**         1 if convert boundary success; 0 else
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_contour_to_bound(srf,tfmat,rot,irot,tol,pock,trilist)
struct NCL_fixed_databag *srf;
UM_transf tfmat,rot;
int irot;
UU_REAL tol;
ncl_polygon *pock;
UU_LIST *trilist;
{
	int i,ic,nc,npts,nv,nv0,status;
	UM_2Dcoord *pts;
	UM_coord pt,pt0;
	UM_srf_bound *polybound;
	UU_LIST ptslst;
	
	struct UM_evsrfout *evsrf;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	status = uc_init_evsrfout(srf, evsrf);
	if (status != UU_SUCCESS)
		return (status);

	uu_list_init (&ptslst, sizeof(UM_coord), 100, 100);

	nc = pock->num_contours;

	for (ic = 0, nv0 = 0; ic < nc; ic++)
	{
		nv = pock->np[ic];
		pts = (UM_2Dcoord *) UU_LIST_ARRAY (pock->contour);
		pts += nv0;

		polybound = (UM_srf_bound *) uu_malloc (sizeof(UM_srf_bound));
		ncl_wsurf_bound_init(polybound);
		UU_LIST_EMPTY (&ptslst);

		for (i = 0; i < nv; i++)
		{				
			status = uc_evsrf (UM_POINT,pts[i][0],pts[i][1],srf,tfmat,evsrf);
			if (status == UU_SUCCESS)
			{
				um_vctovc(evsrf->sp,pt);
				if (i == 0)
					um_vctovc(evsrf->sp,pt0);
			}

			if (status == UU_SUCCESS)
				uu_list_push (&ptslst,&pt);
		}		
		uu_list_push (&ptslst,&pt0);

/*
.....Convert ptslst to polygon boundary
*/
		S_convert_bound(ptslst,polybound);

/*
.....Put polygon boundary into UM_srf_bound list
*/
		uu_list_push (trilist, polybound);

		nv0 += nv;
	}

	uu_list_free(&ptslst);
	uu_free(evsrf);

	return status;
}

/**************************************************************************
**    E_FUNCTION     : ncl_contour_to_trilist(srf,tfmat,rot,irot,tol,pock,trilist)
**
**      Tessellate uv contour into triangles, convert to xyz triangle list
**
**    PARAMETERS
**       INPUT  :
**          srf		   - surface
**          tfmat	   - transformation matrix of input surface
**          rot		   - transformation matrix (world to part)
**			irot	   - tranformation flag
**          tol		   - tolerance
**       OUTPUT : 
**          pstlist	   - updated boundary list if the trim is valid
**    RETURNS      :
**         1 if convert boundary success; 0 else
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_contour_to_trilist(srf,tfmat,rot,irot,tol,pock,trilist)
struct NCL_fixed_databag *srf;
UM_transf tfmat,rot;
int irot;
UU_REAL tol;
ncl_polygon *pock;
UU_LIST *trilist;
{
	int i,ic,nc,npts,nv,nv0,status,ntess;
	UM_2Dcoord *pts,*ipt;
	UM_coord *ipt1,cpt[500];
	UM_vector cvc[500];
	UM_coord pt1,pt2,pt3,pt;
	UM_srf_bound *polybound;
	UM_trian trian1;
	UU_LIST ptslst;
	
	struct UM_evsrfout *evsrf;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	status = uc_init_evsrfout(srf, evsrf);
	if (status != UU_SUCCESS)
		return (status);

#if 0
	if (srf->key == 19305)
	{
		nc = 0;
	}
#endif
	nc = pock->num_contours;

	for (ic = 0, nv0 = 0; ic < nc; ic++)
	{
		nv = pock->np[ic];
		pts = (UM_2Dcoord *) UU_LIST_ARRAY (pock->contour);
		pts += nv0;

		uu_list_init (&ptslst,sizeof(UM_2Dcoord),nv+1,nv+1);
		uu_list_push_multiple(&ptslst,nv,pts);
		uu_list_push(&ptslst,pts[0]);		
		ipt = (UM_2Dcoord *) UU_LIST_ARRAY (&ptslst);
		npts = UU_LIST_LENGTH (&ptslst);
/*
.....Convert to UM-coord
*/
		ipt1 = (UM_coord *) uu_malloc (npts*sizeof(UM_coord));
		for (i = 0; i < npts; i++)
		{
			ipt1[i][0] = ipt[i][0];
			ipt1[i][1] = ipt[i][1];
			ipt1[i][2] = 0.0;
		}
/*
.....Check if polygon ccw or not, reverse it if not
*/	
		ncl_polygon_cclw(ipt1,npts);
/*
.....tessellate polygons into triangles
*/
		ncl_tessel_polyline(ipt1,cpt,cvc,npts,&ntess);
		uu_list_free (&ptslst);
		UU_FREE(ipt1);

/*
.....Put each triangle onto the trilst
*/
		for (i = 0; i < ntess-1; i = i+3)
		{			
			status = uc_evsrf (UM_POINT,cpt[i][0],cpt[i][1],srf,tfmat,evsrf);
			if (status == UU_SUCCESS)	
				um_vctovc(evsrf->sp,pt1);
			status = uc_evsrf (UM_POINT,cpt[i+1][0],cpt[i+1][1],srf,tfmat,evsrf);
			if (status == UU_SUCCESS)				
				um_vctovc(evsrf->sp,pt2);
			status = uc_evsrf (UM_POINT,cpt[i+2][0],cpt[i+2][1],srf,tfmat,evsrf);
			if (status == UU_SUCCESS)				
				um_vctovc(evsrf->sp,pt3);
/*
........Don't store triangles
........that equate to straight lines
*/
			if (!um_cceqcc_tol(pt1,pt2,UM_DFUZZ) &&
				!um_cceqcc_tol(pt1,pt3,UM_DFUZZ) &&
				!um_cceqcc_tol(pt2,pt3,UM_DFUZZ))
			{
				ncl_set_trian(pt1,pt2,pt3,&trian1);
#if 0
				if (srf->key == 19001)
					ncl_debug_triangle(srf->key,0,0,0,&trian1);
#endif
				uu_list_push(trilist,&trian1);
			}
		}
		nv0 += nv;
	}

	uu_free(evsrf);

	return status;
}

/**************************************************************************
**    E_FUNCTION     : int ncl_trim_tess_boundpoly(srf,tfmat,rot,irot,tol,
**											sfbndr,trianlst,uvtrilst,trilist)
**
**       trim base surface tessellation within the boundary
**    PARAMETERS
**       INPUT  :
**          srf		   - surface
**          tfmat	   - surface transformation matrix
**          rot		   - transformation matrix (world to part)
**			irot	   - world to part tranformation flag
**          tol        - tolerance
**			sfbndr     - surface boundary data
**			trianlst   - base surface vertex triangle list
**			uvtrilst   - base surface uv triangle list
**       OUTPUT : 
**          trilist    - trimmed tessllation polygon list 
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_trim_tess_boundpoly(srf,tfmat,rot,irot,tol,sfbndr,trianlst,uvtrilst,trilist)
struct NCL_fixed_databag *srf;
UM_transf tfmat,rot;
UM_srf_boundary *sfbndr;
UU_LIST *trianlst,*uvtrilst;
int irot;
UU_REAL tol;
UU_LIST *trilist;
{
	int status,status1,ntri,i;
	UM_trian *ptri,*uvtri;

	ncl_polygon pockbndr;
	ncl_init_polygon (&pockbndr,0);
	status = UU_SUCCESS;

/*
.....Debug surface boundary with tight tolerance
*/
#if 0	
	ncl_debug_sfboundary(srf,tfmat,sfbndr);
#endif

	ntri = UU_LIST_LENGTH(trianlst);
	if (ntri < 1) return UU_FAILURE;

	ptri = (UM_trian *)UU_LIST_ARRAY(trianlst);
	uvtri = (UM_trian *)UU_LIST_ARRAY(uvtrilst);	

/*
.....Convert boundary uvpts into polygon structure
*/
	ncl_sfbound_to_contour(sfbndr,&pockbndr,UU_TRUE);

/*
.....Debug boundary polygon
*/
#if 0	
	if (srf->key == 7169)
	ncl_debug_polygon(srf,&pockbndr,8);
#endif

/*
.....Trim triangles within boundary polygon
*/
	for (i = 0; i < ntri; i++, ptri++,uvtri++)
	{	
		status1 = ncl_trim_tri_bound(srf,tfmat,&pockbndr,rot,
							irot,ptri,uvtri,tol,trilist);
	}

	ncl_free_polygon (&pockbndr);

	return status;
}

/**************************************************************************
**    E_FUNCTION     : int ncl_trim_tri_bound(srf,tfmat,bound,rot,
**										 	irot,ptri,uvtri,tol,trilist)
**       trim a triangle within the boundary polygon
**    PARAMETERS
**       INPUT  :
**          srf		   - surface
**          tfmat	   - surface transformation matrix
**          pockbound  - surface boundary polygon
**          rot		   - transformation matrix (world to part)
**			irot	   - world to part tranformation flag
**          ptri       - the point triangle
**          uvtri1     - the uv-point triangle
**          tol        - tolerance
**       OUTPUT : 
**          trilist    - updated list iff the projection is valid
**    RETURNS      :
**         1 iff intersection found; 0 else
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
static int ncl_trim_tri_bound(srf,tfmat,pockbound,rot,
							irot,ptri,uvtri,tol,trilist)
struct NCL_fixed_databag *srf;
ncl_polygon *pockbound;
UM_transf tfmat,rot;
int irot;
UM_trian *ptri;
UM_trian *uvtri;
UU_REAL tol;
UU_LIST *trilist;
{
	int status;
	UU_REAL tolsq;
	ncl_polygon pockbndr,pocktri;

	tolsq = tol * tol;
	status = UU_SUCCESS;
	ncl_init_polygon (&pockbndr,0);
	ncl_init_polygon (&pocktri,0);

/*
.....Convert triangle uvpts into polygon structure
*/
	S_trian_to_contour(uvtri,&pocktri);

#if 0
	if (srf->key == 7169)
	{
		ncl_debug_polygon(srf,pockbound,4);
		ncl_debug_polygon(srf,&pocktri,2);
	}
#endif

/*
.....Get the intersection portion between boundary and triangle
*/
	status = ncl_polygon_clip (NCL_INTOF,pockbound,&pocktri,&pockbndr,tol,tolsq);
/*
.....Tessellate polygon into triangles in 2D,convert into xyz triangle list
*/
	if (status == UU_SUCCESS)	
	{
		ncl_weed_contours(&pockbndr,tol*tol,UU_TRUE);
		ncl_contour_to_trilist(srf,tfmat,rot,irot,tol,&pockbndr,trilist);
	}

#if 0
	if (srf->key == 7169)
	{
		NclxDbgPstr("ERASE/ALL");
		ncl_debug_polygon(srf,&pockbndr,5);
	}
#endif


	ncl_free_polygon (&pockbndr);
	ncl_free_polygon (&pocktri);

	return status;
}
