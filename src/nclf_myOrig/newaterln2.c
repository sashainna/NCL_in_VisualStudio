/*********************************************************************
**    NAME         :  newaterln2.c
**       CONTAINS:  Routines for calculating waterline geometry
**
**        ncl_define_rotmx
**        ncl_define_rotmx1
**        ncl_waterline_create_geo
**        ncl_draw_polyline
**        ncl_h_cnpts1
**        ncl_waterline_weed
**        ncl_sf_pln_io
**        ncl_copy_polygon
**        ncl_copy_polygon1
**        S_weed_pts
**        ncl_h_arcpts
**        ncl_fix_contour
**        ncl_delete_contour
**        ncl_prepare_for_pocketing
**        ncl_order_pock_geo
**        ncl_permute_pocs
**        ncl_sort_pocs
**        ncl_set_zone
**        ncl_prepare_zones
**        ncl_ipv_contour_stock
**		  ncl_debug_sfboundary
**		  ncl_copy_sfboundary
**		  ncl_rot_sfboundary
**        ncl_cvio_create_intersection2
**		  ncl_sf_pln_io2
**
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       newaterln2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:00
*********************************************************************/
#include "mdattr.h"
#include "go.h"
#include "gerrorst.h"
#include "uminmax.h"
#include "nclwaterln.h"
#include "nclclip.h"

#define RANDOM 0
#define NEGX 1
#define POSX 2
#define NEGY 3
#define POSY 4
#define NEGZ 5
#define POSZ 6
#define NEARPT 7
#define INSIDE 8
#define OUTSID 9
#define SMALST 10
#define LARGST 11

#define PART 1
#define ALL 2
#define MOST 3

#define WCOLORS 16

#define CO9 (UU_REAL) 0.987688

typedef struct
{
	int ind;
	int znum;
	UU_REAL param;
} NCL_order;

static int icolor = 0;
static int nzones = 0;
static UU_LOGICAL ZLAST = UU_FALSE;
static int zstk;
static ncl_polygon *Szone = UU_NULL;
static int rotfl = 0;
static UM_transf rotinv;

static UM_coord Sllf,Surb;

extern UU_LOGICAL NCL_waterline;

/*********************************************************************
*********************************************************************/
void ncl_init_view_box()
{
	int j;
	for (j = 0; j < 3; j++)
	{
		Sllf[j] = 1.e+25; Surb[j] = -1.e+25;
	}
}

/*********************************************************************
*********************************************************************/
static void update_view_box (pt)
UM_coord pt;
{
	int j;
	for (j = 0; j < 3; j++)
	{
		if (Sllf[j] > pt[j]) Sllf[j] = pt[j];
		if (Surb[j] < pt[j]) Surb[j] = pt[j];
	}
}

/*********************************************************************
**    E_FUNCTION     :  ncl_waterline_extrema(box)
**       Return the saved 3D-box to the view zooming routine.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_waterline_extrema(box)
Gwrect3 *box;
{
	UU_REAL tol;

	ncl_get_wtol (&tol);

	box->llf.x = Sllf[0];
	box->urb.x = Surb[0];
	box->llf.y = Sllf[1];
	box->urb.y = Surb[1];
	box->llf.z = Sllf[2] - 5*tol;
	box->urb.z = Sllf[2] + 5*tol;

	return (1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_draw_polyline (npts,pts,color,ldisp)
**       Draw a polyline-type display segment
**    PARAMETERS
**       INPUT  :
**          npts       - number of points in list
**          pts        - points in list
**          color
**          ldisp      - display now iff 1
**       OUTPUT :
**          psegs      - updated list of segments
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_draw_polyline (npts,pts,color,ldisp)
int npts,color,ldisp;
UM_coord *pts;
{
	int i;
	Gseg segid;
	Glntype line_style;
	Gwpoint3 *gpt;
	Gscale WIDTH = 1.;
	UM_coord tmpt;

/*
..... get the next segment identifier and create a display segment with this
..... identifier
*/
	segid = gnseg();
	if( gcreateseg(segid) != NCL_NO_ERROR ) return;

/* set segment visibility */
	if (ldisp)
		gssegvis(segid,UU_FALSE);
	else
		gssegvis(segid,UU_TRUE);
/* set display attributes */
	gsfillcolor(color);
	gslinecolor(color);
	line_style.typeno = 1;
	line_style.npatn = 0;
	gslinetype(&line_style);
	gslinewidth(WIDTH);

	gpt = (Gwpoint3 *) uu_malloc( (npts + 1) * sizeof(Gwpoint3));
	for (i=0; i<npts; i++)
	{
		if (rotfl)
			um_cctmtf (pts[i],rotinv,tmpt);
		else
			um_vctovc (pts[i],tmpt);

		if (ldisp == 1 && NCL_waterline) update_view_box (tmpt);

		gpt[i].x = tmpt[0];
		gpt[i].y = tmpt[1];
		gpt[i].z = tmpt[2];
	}
	gpolyline3(npts, gpt);
	uu_free(gpt);

	gcloseseg();

	ncl_psegs_push (&segid);

	return;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_waterline_create_geo0 (npts,pts)
**       Create a polyline entity by a list of points
**    PARAMETERS
**       INPUT  :
**          npts       - number of points in list
**          pts        - points in list
**       OUTPUT : none
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_waterline_create_geo0 (npts,pts)
int npts;
UM_coord *pts;
{
	int i,j,status = UU_SUCCESS;
	UM_int2 i2 = 0;
	struct UM_polyline_rec pline;
	UU_KEY_ID gkey;

	gkey = 0;
	ur_setup_data(UM_POLYLINE_REL, &pline, sizeof(pline));
	pline.key = 0;
	pline.pt = UU_NULL;
	status = ncl_label_wf(UM_POLYLINE_REL,pline.label,&pline.subscr,
		pline.key,&i2);
	pline.no_pt = npts;
	pline.pt = (UU_REAL *) uu_malloc (3*npts*sizeof(UU_REAL));
	if (!pline.pt) return (UU_FAILURE);
	for (i = 0, j = 0; i < npts; i++, j+=3)
	{
		if (rotfl)
			um_cctmtf (pts[i],rotinv,&pline.pt[j]);
		else
			um_vctovc (pts[i],&pline.pt[j]);
	}

	status = ncl_create_entity (&pline, NCLI_CURVE);
	if (status == UU_SUCCESS)
	{
		ncl_store_wf1(pline.key);
		gkey = pline.key;
	}
	if (status == UU_SUCCESS) status = ur_update_displayable(pline.key,
			UM_DISPLAYABLE);

	if (icolor >= WCOLORS) icolor -= WCOLORS;
	ncl_update_color(pline.key,icolor);
	icolor++;

	if (status == UU_SUCCESS) status = uc_display(&pline);
		UU_FREE (pline.pt);

	if (status != UU_SUCCESS && gkey)
		uc_delete (gkey);
	return (status);
}

/********************************************************************
**    E_FUNCTION: ncl_h_cnpts1 (ptsv, pt, pts, nmp, indx)
**       Checks list of 'nmp' points 'pts' for maximum distance from
**       line defined by points 'ptsv' & 'pt'.  If checked point is
**       outside segment than distance from closest end point is used.
**    PARAMETERS
**       INPUT  :
**          ptsv   - start point of CV arc
**          pt     - end point of CV arc
**          pts    - array of intermediate points on arc
**          nmp    - number of points in array
**       OUTPUT :
**          indx   - index of the point in 'pts' array with maximum
**							deviation.
**    RETURNS      :
**          chord value squared
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
static UU_REAL ncl_h_cnpts1 (ptsv,pt,pts,nmp,indx)
UM_coord ptsv, pt, *pts;
int nmp, *indx;
{
	int i;
	UU_REAL d, dm, dis, p;
	UM_2Dcoord vec,vc;

	*indx = nmp - 1;
	dm = 0.;

	vec[0] = pt[0] - ptsv[0]; vec[1] = pt[1] - ptsv[1];
	dis = UM_DOT_2D(vec,vec);
	if (dis < 1.e-16) return (dm);

	for (i = 0; i < nmp; i++)
	{
		vc[0] = pts[i][0] - ptsv[0]; vc[1] = pts[i][1] - ptsv[1];
		p = UM_DOT_2D(vc,vec);
		if (p < 0.)
			d = UM_SQDIS_2D (pts[i],ptsv);
		else if (p > dis)
			d = UM_SQDIS_2D (pt,pts[i]);
		else
			d = UM_DOT_2D (vc,vc) - p*p/dis;
		if (d > dm) {  dm = d;  *indx = i;  }
	}

	return (dm);
}

/**************************************************************************
**    E_FUNCTION: int ncl_dehook (pts,npts,tol2)
**       Process array of points simulating curve by removing unnecessary
**       points if chordal tolerance is satisfied.
**    PARAMETERS
**       INPUT  :
**          pts    - unweeded points
**          npts   - number of above
**          tol2   - squared chordal tolerance
**       OUTPUT :
**          pptr  - pointer to points list
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
static int ncl_dehook (pts,npts,tol2)
UM_coord *pts;
int *npts;
UU_REAL tol2;
{
	UM_2Dcoord vec,vc;
	int j,np;
	int delfirst = 0;
	UU_REAL dis,d,p;

	np = *npts;

	if (np <= 2) return(delfirst);
	for (j = 0; j < 2; j++)
		vec[j] = pts[2][j] - pts[1][j];
	dis = UM_DOT_2D(vec,vec);
	if (dis > 10000.*tol2)
	{
		for (j = 0; j < 2; j++)
			vc[j] = pts[1][j] - pts[0][j];
		d = UM_DOT_2D(vc,vc);
		if (d < 10.*tol2)
		{
			p = UM_DOT_2D(vc,vec);
			if (p > 0) p = (p*p)/(dis*d);
			if (p < 0.75)
			{
				pts++; np--; *npts = np;
				delfirst = 1;
			}
		}
	}

	if (np <= 2) return(delfirst);
	for (j = 0; j < 2; j++)
		vec[j] = pts[np-2][j] - pts[np-3][j];
	dis = UM_DOT_2D(vec,vec);
	if (dis > 10000.*tol2)
	{
		for (j = 0; j < 2; j++)
			vc[j] = pts[np-1][j] - pts[np-2][j];
		d = UM_DOT_2D(vc,vc);
		if (d < 10.*tol2)
		{
			p = UM_DOT_2D(vc,vec);
			if (p > 0) p = (p*p)/(dis*d);
			if (p < 0.75)
				(*npts)--;
		}
	}

	return(delfirst);
}

/**************************************************************************
**    E_FUNCTION: void ncl_waterline_weed (pts,npts,pptr,told)
**       Process array of points simulating curve by removing unnecessary
**       points if chordal tolerance is satisfied.
**    PARAMETERS
**       INPUT  :
**          pts    - unweeded points
**          npts   - number of above
**          eps   - squared chordal tolerance
**       OUTPUT :
**          pptr  - pointer to points list
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
static void ncl_waterline_weed (pts,npts,pptr,eps,itsk)
UM_coord *pts;
int *npts;
UU_LIST *pptr;
UU_REAL eps;
int itsk;
{
	UM_coord ptsv;
	int is, ix, j, k, kmax, last, nu;

	if (ncl_dehook (pts,npts,eps) == 1)
	{
		UM_coord *pp;

		pp = (UM_coord *) UU_LIST_ARRAY (pptr);
		last = pptr->cur_cnt - 1;
		pts++;
		um_vctovc (pts[0],pp[last]);
	}
/*
.....get pointer to data arrays
*/
	j = *npts;
	um_vctovc (&pts[0],ptsv);
/*
.....weed out points
*/
	nu = 1;
	last = 1;
	kmax = j;
	while (last < j)
	{
		if (itsk == 1)
		{
			if (last >= j-1) break;
			if (UM_SQDIS (pts[last],ptsv) > eps)
			{
				um_vctovc (&pts[last],ptsv);
				uu_list_push (pptr,ptsv);
				nu++;
			}
			last++;
		}
		else
		{
			is = last - 1;
			for (k = 1; k < kmax; k++)
			{
				if (k == 1)
					last++;
				else if (ncl_h_cnpts1 (ptsv,pts[last++],&pts[is],k,&ix) > eps)
				{
					ix = ix + is;
					um_vctovc (&pts[ix],ptsv);
					uu_list_push (pptr,ptsv);
					nu++;
					break;
				}
			}
			kmax = j - last + 1;
		}
	}
/*
.....store last point
*/
	if (nu < 2 && UM_SQDIS_2D(pts[0],pts[j-1]) <= eps)
		*npts = 0;
	else
	{
		uu_list_push (pptr,&pts[j-1]);
		*npts = nu + 1;
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_sf_pln_io (sfi,pl,nio,ptsio,htop,tol,itsk)
**       Intersect a surface with a horizontal plane
**    PARAMETERS
**       INPUT  :
**          sfi        - current surface boundary data
**			pl		   - plane
**          nio        - list containing numbers of points in each polyline
**          ptsio      - list of polylines points
**          htop       - Z-level of the plane
**			tol2d 	   - tolerance
**			tol 	   - tolerance
**          itsk       - 1 iff called from necvlev
**       OUTPUT : none
**
**    RETURNS      :
**         1 iff intersection found; 0 else
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sf_pln_io (sfi,pl,nio,ptsio,htop,tol2d,tol,itsk)
NCL_waterline_surf *sfi;
struct NCL_nclpl_rec *pl;
UU_LIST *nio,*ptsio;
UU_REAL htop,tol2d,tol;
int itsk;
{
	int j,npts,chunks,nj,cur0,status;
	int *np;
	UM_trian *ptri;
	UM_coord *ptio;
	UU_REAL eps,tol1,d;
	UM_vector vec;
	UU_LIST cvlst,uvlst;

#define SI2 (UU_REAL) 3.046171e-6

	eps = tol2d*tol2d;
	tol1 = 0.1*tol;

	ncl_nul_uv();
	ncl_nul_nios();
	uu_list_init0 (&cvlst);
	uu_list_init0 (&uvlst);
/*
..... flat surface case
*/
	if (sfi->sf_flag == TILTPL || sfi->sf_flag == VERTPL)
	{

		vec[0] = sfi->nvec[1];
		vec[1] = -sfi->nvec[0];
		vec[2] = 0.;
		d = UM_DOT (vec,vec);
		if (d < SI2) return (UU_FAILURE);
		d = sqrt(d);
		vec[0] /= d; vec[1] /= d;
		ncl_set_ldir (vec);
		vec[0] = vec[1] = 0.; vec[2] = 1.;
		status = ncl_bndrpln_io (sfi->bound.nb,sfi->bound.npo,sfi->bound.np,
			sfi->bound.cvpts,vec,htop,tol1);
		if (status != UU_SUCCESS) return (status);
	}
	else
	{
		ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
		npts = sfi->trianlist->cur_cnt;
		for (j = 0; j < npts; j++, ptri++)
			ncl_tripln_io(ptri,pl,tol1,eps);
		ncl_arrange_segs (tol2d);
	}

	if (itsk == 1 && sfi->flag1 == VSOLID)
	{
		uu_list_init (&cvlst, sizeof(UM_coord), 100, 200);
		uu_list_init (&uvlst, sizeof(UM_coord), 0, 200);
		ncl_separate_components (&cvlst,&uvlst,tol2d);
		npts = cvlst.cur_cnt;
		if (npts < 4)
		{
			uu_list_free (&uvlst);
			uu_list_free (&cvlst);
			return (0);
		}
		ptio = (UM_coord *) UU_LIST_ARRAY (&cvlst);
	}
	else
	{
		ncl_getpts_uv (&npts,&ptio);
		if (npts <= 0) return (0);
	}
	chunks = 1;
	ncl_get_nios (&chunks,&np);

	if (chunks > 1)
	{
		for (j = 0; j < chunks; j++)
		{
			cur0 = ptsio->cur_cnt;
			npts = np[j];
			nj = npts;
			uu_list_push (ptsio,ptio[0]);

			ncl_waterline_weed(ptio,&nj,ptsio,eps,itsk);
			if (nj > 1)
				uu_list_push (nio, &nj);
			else
				ptsio->cur_cnt = cur0;

			ptio += npts;
		}
	}
	else
	{
		cur0 = ptsio->cur_cnt;
		nj = npts;
		uu_list_push (ptsio,ptio[0]);

		ncl_waterline_weed(ptio,&nj,ptsio,eps,itsk);
		if (nj > 1)
			uu_list_push (nio, &nj);
		else
			ptsio->cur_cnt = cur0;
	}

	uu_list_free (&uvlst);
	uu_list_free (&cvlst);

	sfi->ncvs = nio->cur_cnt - sfi->nlist0;
	return (sfi->ncvs);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_copy_polygon1 (c0,nc,pol1,pol2)
**       Copy a polygon structure, starting at contour c0.
**    PARAMETERS
**       INPUT  :
**                pol1     - the polygon to copy
**                c0       - first contour to copy
**                nc       - number of contours to copy
**       OUTPUT :
**                pol2     - copy polygon
**
**    RETURNS      :
**         UU_FAILURE if malloc fails, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_copy_polygon1 (c0,nc,pol1,pol2)
int c0,nc;
ncl_polygon *pol1,*pol2;
{
	int c,ci,nci,npts;
	UM_2Dcoord *vtx;

	pol2->num_contours = 0;
	nci = pol1->num_contours;

	if (c0 < 0 || nc < 1 || c0+nc > nci) return (UU_FAILURE);
	nci = nc;

	UU_FREE (pol2->np);
	pol2->np = (int *) uu_malloc (nc*sizeof(int));
	if (!pol2->np) return (UU_FAILURE);

	UU_FREE (pol2->box);
	if (pol1->box != UU_NULL)
	{	pol2->box = (UM_2box *) uu_malloc (nc*sizeof(UM_2box));
		if (!pol2->box) return (UU_FAILURE);
	}

	if (!pol2->contour)
	{
		pol2->contour = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		if (!pol2->contour) return (UU_FAILURE);
		npts = pol1->contour->cur_cnt;
		uu_list_init (pol2->contour,sizeof(UM_2Dcoord),npts,npts);
	}
	if (!pol2->contour->data) return (UU_FAILURE);

	pol2->contour->cur_cnt = 0;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol1->contour);

	if (pol1->box == UU_NULL) pol2->box = UU_NULL;
	for (c = 0, ci = 0; c < c0+nc; c++, vtx += npts)
	{
		npts = abs(pol1->np[c]);
		if (c < c0) continue;
		if (npts < 3)
		{
			nci--; continue;
		}
		uu_list_push_multiple (pol2->contour,npts,vtx);
		pol2->np[ci] = (pol1->np[c] > 0)? npts: -npts;
		if (pol1->box != UU_NULL)
		{
			pol2->box[ci].xmin = pol1->box[c].xmin;
			pol2->box[ci].xmax = pol1->box[c].xmax;
			pol2->box[ci].ymin = pol1->box[c].ymin;
			pol2->box[ci].ymax = pol1->box[c].ymax;
		}
		ci++;
	}
	if (nci < 1) return (UU_FAILURE);
	pol2->num_contours = nci;

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_copy_polygon(pol1,pol2)
**       Copy a polygon structure.
**    PARAMETERS
**       INPUT  :
**                pol1     - the polygon to copy
**       OUTPUT :
**                pol2     - copy polygon
**
**    RETURNS      :
**         UU_FAILURE if malloc fails, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_copy_polygon(pol1,pol2)
ncl_polygon *pol1,*pol2;
{
	int c,nc,status;

	c = 0;
	nc = pol1->num_contours;

	status = ncl_copy_polygon1 (c,nc,pol1,pol2);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL S_folding (vt0,vt1,d0,d1,eps)
**       Determine if two 2D vectors form a fold.
*********************************************************************/
static UU_LOGICAL S_folding (vt0,vt1,d0,d1,eps)
UM_2Dcoord vt0,vt1;
UU_REAL d0,d1,eps;
{
	UU_REAL co,d;
	UU_LOGICAL cut;

	cut = UU_FALSE;

	co = UM_DOT_2D (vt0,vt1);
	if (co < 0.)
	{
		if (d0 <= d1)
		{
			d = d0 - (co*co)/d1;
			cut = (d < 0.5*d0 && d < eps);
		}
		else
		{
			d = d1 - (co*co)/d0;
			cut = (d < 0.5*d1 && d < eps);
		}
	}

	return (cut);
}

/********************************************************************
**    E_FUNCTION: ncl_h_arcpts (ptsv,pt,pts,nmp,indx)
**       Checks list of 'nmp' points 'pts' for maximum distance from
**       line defined by points 'ptsv' & 'pt'.  If checked point is
**       outside segment than distance from closest end point is used.
**       This is the ncl_h_cnpts rewritten for 2D.
**    PARAMETERS
**       INPUT  :
**          ptsv   - start point of CV arc
**          pt     - end point of CV arc
**          pts    - array of intermediate points on arc
**          nmp    - number of points in array
**       OUTPUT :
**          indx   - index of the point in 'pts' array with maximum
**							deviation.
**    RETURNS      :
**          chord value squared
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
static UU_REAL ncl_h_arcpts (ptsv,pt,pts,nmp,indx)
UM_2Dcoord ptsv, pt, *pts;
int nmp, *indx;
{
	int i;
	UU_REAL d, dm, dis, p;
	UM_2Dcoord vec,vc;

	*indx = nmp - 1;
	dm = 0.;

	vec[0] = pt[0] - ptsv[0]; vec[1] = pt[1] - ptsv[1];
	dis = UM_DOT_2D(vec,vec);
	if (dis < 1.e-16) return (dm); /* zero division test */

	for (i = 0; i < nmp; i++)
	{
		vc[0] = pts[i][0] - ptsv[0]; vc[1] = pts[i][1] - ptsv[1];
		p = UM_DOT_2D(vc,vec);
		if (p < 0.)
			d = UM_SQDIS_2D (pts[i],ptsv);
		else if (p > dis)
			d = UM_SQDIS_2D (pt,pts[i]);
		else
			d = UM_DOT_2D (vc,vc) - p*p/dis;
		if (d > dm) {  dm = d;  *indx = i;  }
	}

	return (dm);
}

/**************************************************************************
**    E_FUNCTION: void S_weed_pts (points,nv0,npts)
**       Process array of points simulating curve by removing unnecessary
**       points if chordal tolerance is satisfied.
**    PARAMETERS
**       INPUT  :
**          points - pointer to points list
**          nv0    - first point number in the list
**          npts   - number of points to weed
**       OUTPUT :
**          points - updated points list
**          npts   - updated number of points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
static void S_weed_pts (points,nv0,npts,tolsq)
UU_LIST *points;
int nv0,*npts;
UU_REAL tolsq;
{
	UM_2Dcoord *pts;
	int i,k,nv,ix;
/*
.....get pointer to data arrays
*/
	pts = (UM_2Dcoord *) UU_LIST_ARRAY (points);
	pts += nv0;
	nv = *npts;
	if (nv < 3) return;
/*
.....weed out points
*/
	for (i = 0; i < nv-2; i++)
	{
		for (k = 1; k < nv-i-1; k++)
		{
			if (ncl_h_arcpts (pts[i],pts[i+k+1],&pts[i+1],k,&ix) > tolsq)
				break;
		}
		if (k == nv - i - 1)
		{
			ix = nv - i - 2;
		}
		if (k < nv - 1 && ix > 0)
		{
			uu_list_delete (points,nv0+i+1,ix);
			nv -= ix;
			pts = (UM_2Dcoord *) UU_LIST_ARRAY (points);
			pts += nv0;
		}
	}

	*npts = nv;
	return;
}

/**************************************************************************
**    E_FUNCTION: void S_weed (points,nv0,npts)
**       Process array of points simulating curve by removing unnecessary
**       points if chordal tolerance is satisfied.
**    PARAMETERS
**       INPUT  :
**          points - pointer to points list
**          nv0    - first point number in the list
**          npts   - number of points to weed
**       OUTPUT :
**          points - updated points list
**          npts   - updated number of points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
static void S_weed (contour,nv0,npt,tolsq)
UU_LIST *contour;
int nv0,*npt;
UU_REAL tolsq;
{
	int nv,nv1;
	UM_2Dcoord *pp,p0;
	UU_REAL d;

	pp = (UM_2Dcoord *) UU_LIST_ARRAY (contour);
	nv = *npt;
	pp += nv0;
	nv1 = nv0 + nv;

	d = UM_SQDIS_2D(pp[0],pp[nv-1]);
	if (d > tolsq)
	{
		um_vctovc_2d(pp[0],p0);
		if (contour->cur_cnt > nv1)
			uu_list_insert (contour,nv1,p0);
		else
			uu_list_push (contour,p0);
		nv++;
	}

	S_weed_pts (contour,nv0,&nv,tolsq);

	nv1 = nv0 + nv;
	if (contour->cur_cnt > nv1)
		uu_list_delete (contour,nv1-1,1);
	else
		contour->cur_cnt = nv1-1;
	nv--;

	*npt = nv;
}

/*********************************************************************
**    E_FUNCTION: void ncl_fix_contour (contour,nv0,npt,tol,tolsq)
**       Process polyline contours by removing "folds" (switchbacks),
**       and then weeding out unnecessary points.
**    PARAMETERS
**       INPUT  :
**          contour    - list of 2D points
**          nv0        - index of first contour point
**          npt        - number of contour points
**          tol,tolsq  - tolerance parameters
**          istk       - 1 iff dealing with the stock
**       OUTPUT :
**          contour - updated list
**          npt     - updated number of contour points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fix_contour (contour,nv0,npt,tol,tolsq,istk)
UU_LIST *contour;
int *npt,nv0,istk;
UU_REAL tol,tolsq;
{
	int nv,i,i1;
	UM_2Dcoord *pp,vt0,vt1,vt2;
	UU_REAL d0,d1,d2;
	UU_REAL eps = 16.*tolsq;
	UU_REAL dtolsq = tol*tol;
	UU_LOGICAL cut,need0;
	UU_LIST tmplst;
	UM_2Dcoord tmp0,tmp1,tmpa,*tmp;
	UU_REAL tco;
	int ins;

	pp = (UM_2Dcoord *) UU_LIST_ARRAY (contour);
	nv = *npt;
	pp += nv0;

	if (istk == 1) uu_list_init (&tmplst,sizeof(UM_2Dcoord),0,nv);

	need0 = UU_TRUE;

	for (i = 1; i < nv; i++)
	{
		if (need0)
		{
			um_vcmnvc_2d(pp[i],pp[i-1],vt0);
			d0 = UM_DOT_2D (vt0,vt0);
		}
		else
		{
			um_vctovc_2d(vt1,vt0); d0 = d1;
		}
		i1 = (i+1+nv)%nv;
		um_vcmnvc_2d(pp[i1],pp[i],vt1);
		d1 = UM_DOT_2D (vt1,vt1);
		need0 = UU_FALSE;

		if (d0 > dtolsq)
		{
			if (d1 <= dtolsq) continue;
			cut = S_folding (vt0,vt1,d0,d1,eps);
			if (!cut) continue;

			if (istk == 1 && i > 1 && i < nv-2)
			{
/*
..... if stock, do not remove a stick-out point if it outside the resulting
..... polygon - qar 95107
*/
				um_unitvc_2d (vt0,tmp0);
				um_unitvc_2d (vt1,tmp1);
				tco = UM_DOT_2D (tmp0,tmp1);

				if (tco < -CO9)
				{
					tmpa[0] = pp[i][0];
					tmpa[1] = pp[i][1];
					tmplst.cur_cnt = 0;
					uu_list_push_multiple (&tmplst,nv,pp);
					uu_list_delete (&tmplst,i,1);
					tmp = (UM_2Dcoord *) UU_LIST_ARRAY (&tmplst);

					ins = um_check_inside(tmp,nv-1,tmpa,UU_NULL,tol);
					if (ins < 0) continue;
				}
			}

			if (i < nv-2 && d0 > 2*d1)
			{
/*
..... decide if it would be better to cut i1
*/
				um_vcmnvc_2d(pp[i+2],pp[i+1],vt2);
				d2 = UM_DOT_2D (vt2,vt2);
				if (d2 > dtolsq && d0 > 2*d2)
				{
					cut = S_folding (vt1,vt2,d1,d2,eps);
					if (cut) continue;
				}
			}
		}

		uu_list_delete (contour,nv0+i,1);
		pp = (UM_2Dcoord *) UU_LIST_ARRAY (contour);
		pp += nv0;
		i--; nv--;
		need0 = UU_TRUE;
/*
..... qar 95289 fix: removing a point may result in a fold one point before
*/
		if (istk == 0 && i > 0 && !ncl_setver(95))
			i--;
	}

	if (istk == 1) uu_list_free (&tmplst);

	if (nv >= 3) S_weed (contour,nv0,&nv,tolsq);

	*npt = nv;

	return;
}

/*********************************************************************
**    E_FUNCTION: void ncl_delete_contour (nc,c,nv0,nv,pol)
**       Delete a contour from a polygon.
**    PARAMETERS
**       INPUT  :
**          nc     - number of contours
**          c      - contour to delete
**          nv0    - starting index of contour c in the list of points
**          nv     - number of points in contour c
**          pol    - polygon structure containing contours
**       OUTPUT :
**          pol    - updated polygon
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_delete_contour (nc,c,nv0,nv,pol)
int nc,c,nv0,nv;
ncl_polygon *pol;
{
	int k;

	if (nv > 0)	uu_list_delete (pol->contour,nv0,nv);
	for (k = c+1; k < nc; k++)
	{
		pol->np[k-1] = pol->np[k];
		if (pol->box != UU_NULL)
		{
			pol->box[k-1].xmin = pol->box[k].xmin;
			pol->box[k-1].xmax = pol->box[k].xmax;
			pol->box[k-1].ymin = pol->box[k].ymin;
			pol->box[k-1].ymax = pol->box[k].ymax;
		}
	}
}

/*********************************************************************
**    E_FUNCTION: void ncl_prepare_for_pocketing (pol)
**       Process polyline contours by removing "folds" (switchbacks),
**       and then weeding out unnecessary points.
**    PARAMETERS
**       INPUT  :
**          pol    - polygon structure containing contours
**       OUTPUT :
**          pol    - updated polygon
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_prepare_for_pocketing (pol,ctol,ctolsq)
ncl_polygon *pol;
UU_REAL ctol,ctolsq;
{
	int nc,c,nv,nv0;
	int istk = 0;

	nc = pol->num_contours;
	if (nc <= 0) return;

	for (c = 0, nv0 = 0; c < nc; c++)
	{
		nv = abs(pol->np[c]);
		ncl_fix_contour (pol->contour,nv0,&nv,ctol,ctolsq,istk);
		if (nv < 3)
		{
			ncl_delete_contour (nc,c,nv0,nv,pol);
			nc--; c--;
		}
		else
		{
			pol->np[c] = (pol->np[c] > 0)? nv: -nv;
			nv0 += nv;
		}
	}

	pol->num_contours = nc;

	return;
}

/*********************************************************************
**    E_FUNCTION: UU_REAL ncl_closed_area (vti,npt)
**       Calculate contour area.
*********************************************************************/
UU_REAL ncl_closed_area (vti,npt)
int npt;
UM_2Dcoord *vti;
{
	int j;
	UU_REAL sa = 0.;

	for (j = 1; j < npt - 1; j++)
		sa += um_triangle_signed_area(vti[0],vti[j],vti[j+1]);

	return (fabs(sa));
}

/*********************************************************************
**    E_FUNCTION: int ncl_set_zone (zoning,bx,vtc,npt,area)
**       Calculate zone number for a contour.
**    PARAMETERS
**       INPUT  :
**          pol    - polygon structure containing contours
**       OUTPUT :
**          pol    - updated polygon
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_set_zone (zoning,bx,vtc,npt,area)
int npt,zoning;
UM_2box *bx;
UM_2Dcoord *vtc;
UU_REAL area;
{
	UM_2Dcoord *zpt;
	int i,j,in,nc,npj;
	int zret;
	UU_REAL tol,tolsq;
	ncl_polygon *pol0,*pol;

	ncl_get_wtols (&tol,&tolsq);

	zret = (ZLAST)? nzones: 1001;
	if (zoning == ALL)
	{
		for (i = 0; i < nzones; i++)
		{
			if (um_check_box_ins (bx,Szone[i].box,tol) >= 0)
			{
				zpt = (UM_2Dcoord *) UU_LIST_ARRAY (Szone[i].contour);
				for (j = in = 0; j < npt && in >= 0; j++)
				{
					in = um_check_inside(zpt,Szone[i].np[0],vtc[j],&Szone[i].box[0],
						tol);
				}
				if (in >= 0) return (i);
			}
		}
	}
	else
	{
		ncl_get_wpol (0,&pol0);
		ncl_get_wpol (1,&pol);
		pol0->num_contours = 1;
		pol0->np = &npt;
		pol0->box = bx;
		UU_LIST_EMPTY(pol0->contour);
		uu_list_push_multiple (pol0->contour,npt,vtc);

		for (i = 0; i < nzones; i++)
		{
			UU_LIST_EMPTY(pol->contour);
			in = ncl_polygon_clip (NCL_INTOF,pol0,&Szone[i],pol,tol,tolsq);
			if (in == UU_SUCCESS)
			{
				if (zoning == PART)
					zret = i;
				else
				{
					UU_REAL arj = 0;
					nc = pol->num_contours;
					zpt = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
					for (j = 0; j < nc; j++)
					{
						npj = pol->np[j];
						if (npj > 0)
						{
							arj += ncl_closed_area (zpt,npj);
						}
						else
						{
							npj = -npj;
							arj -= ncl_closed_area (zpt,npj);
						}
						zpt += npj;
					}
					if (2*arj > area) zret = i;
				}
			}
			ncl_reset_polygon (pol);
			if (zret < nzones) break;
		}

		pol0->np = (int *)UU_NULL;
		pol0->box = (UM_2box *)UU_NULL;
		UU_LIST_EMPTY(pol0->contour);
		pol0->num_contours = 0;

	}

	return (zret);
}

/*********************************************************************
**    E_FUNCTION: int ncl_stk_zone (stklst,bx,zoning)
**       Calculate zone number for a contour.
**    PARAMETERS
**       INPUT  :
**          pol    - polygon structure containing contours
**       OUTPUT :
**          pol    - updated polygon
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_stk_zone (stklst,bx,zoning)
UU_LIST *stklst;
UM_2box *bx;
int zoning;
{
	int npt;
	UM_2Dcoord *vtx,ptb[4];
	UU_REAL area;

	if (stklst)
	{
		npt = stklst->cur_cnt - 1;
		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (stklst);
		if (zoning == MOST) area = ncl_closed_area (vtx,npt);
	}
	else
	{
		npt = 4;
		ptb[0][0] = bx->xmin; ptb[0][1] = bx->ymin;
		ptb[1][0] = bx->xmax; ptb[1][1] = bx->ymin;
		ptb[2][0] = bx->xmax; ptb[2][1] = bx->ymax;
		ptb[3][0] = bx->xmin; ptb[3][1] = bx->ymax;
		vtx = &ptb[0];
		if (zoning == MOST) area = (bx->xmax - bx->xmin)*(bx->ymax - bx->ymin);
	}

	zstk = ncl_set_zone (zoning,bx,vtx,npt,area);
}

/*********************************************************************
**    E_FUNCTION: ncl_pokcmp(e1,e2)
**       Comparison routine for the sort algorithm (uu_qsort).
**    PARAMETERS
**       INPUT  :
**          e1     - first element to be compared
**          e2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if e1 < e2
**                     0 if e1 = e2
**                     1 if e1 > e2
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pokcmp(e1,e2)
NCL_order *e1,*e2;
{
	if (e1->znum > e2->znum) return(1);
	else if (e1->znum < e2->znum) return(-1);

	if (e1->param > e2->param) return(-1);
	else if (e1->param < e2->param) return(1);

	return(0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sort_pocs (pock,nperim,nloops,frmtype,sortpt,nj,
**                                    inside,newl)
**       Sort pocket perimeters according to user-specified ordering.
**    PARAMETERS
**       INPUT  :
**                pock   - polygon structure
**       OUTPUT :
**                pock   - updated polygon structure
**                njlist - list used to find each contour in the pocket struct:
**                         nj[k] is the start of the k-th contour.
**    RETURNS      : none
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL ncl_sort_pocs (pock,nperim,zoning,cstk,frmtype,sortpt,nj,
								 inside,newl,nctr)
ncl_polygon *pock;
int nperim,zoning,cstk,frmtype;
UM_2Dcoord sortpt;
int *nj,*inside,*newl,*nctr;
{
	int i,j,k,c,nc,*nps;
	NCL_order *pocs;
	UM_2Dcoord cpt;
	UM_2Dcoord *vtx,*vti;
	UU_REAL area;
	int nloops = pock->num_contours;
	UU_LOGICAL permuted = UU_FALSE;
	int ncl_pokcmp();

	nps = (int *) pock->np;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pock->contour);
	*nctr = nloops;

	pocs = (NCL_order *) uu_malloc (nperim*sizeof(NCL_order));

	if (frmtype == INSIDE || frmtype == OUTSID)
	{
		sortpt[0] = sortpt[1] = 0;
		for (i = 0; i < nloops; i++)
		{
			if (nps[i] < 0) continue;
			sortpt[0] += pock->box[i].xmin + pock->box[i].xmax;
			sortpt[1] += pock->box[i].ymin + pock->box[i].ymax;
		}
		sortpt[0] /= (2*nperim); sortpt[1] /= (2*nperim);
	}

	for (c = 0, i = 0; i < nloops; i++)
	{
		if (nps[i] < 0) continue;
		pocs[c].ind = i;

		if (zoning)
		{
			if (i == cstk)
				pocs[c].znum = zstk;
			else
			{
				vti = vtx + nj[i];
				if (zoning == MOST) area = ncl_closed_area (vti,nps[i]);
				pocs[c].znum = ncl_set_zone (zoning,&pock->box[i],vti,nps[i],area);
			}
			if (pocs[c].znum > 1000)
			{
				c++; continue;
			}
		}
		else
			pocs[c].znum = 0;
		if (frmtype >= NEGX && frmtype <= POSZ)
		{
/*
..... when ordering along a vector, start with the pocket containing stock
*/
			if (i == cstk && ncl_is_watrev())
				pocs[c].param = -100;
			else
			{
				cpt[0] = pock->box[i].xmin + pock->box[i].xmax;
				cpt[1] = pock->box[i].ymin + pock->box[i].ymax;
				pocs[c].param = UM_DOT_2D (cpt,sortpt);
			}
		}
		else if (frmtype == NEARPT || frmtype == INSIDE || frmtype == OUTSID)
		{
/*
..... for now, take the average distance from nearpt to the 4 box corners,
..... box center does not work well
*/
			pocs[c].param = 0;
			cpt[0] = pock->box[i].xmin; cpt[1] = pock->box[i].ymin;
			pocs[c].param += UM_DIST_2D (cpt,sortpt);
			cpt[0] = pock->box[i].xmax;	cpt[1] = pock->box[i].ymin;
			pocs[c].param += UM_DIST_2D (cpt,sortpt);
			cpt[0] = pock->box[i].xmax;	cpt[1] = pock->box[i].ymax;
			pocs[c].param += UM_DIST_2D (cpt,sortpt);
			cpt[0] = pock->box[i].xmin;	cpt[1] = pock->box[i].ymax;
			pocs[c].param += UM_DIST_2D (cpt,sortpt);
			if (frmtype != OUTSID) pocs[c].param *= -1.;
		}
		else
		{
			vti = vtx + nj[i];
			for (j = 1, area = 0.; j < nps[i] - 1; j++)
			area += um_triangle_signed_area(vti[0],vti[j],vti[j+1]);
			pocs[c].param = fabs(area);
			for (k = 0; k < nloops; k++)
			{
				if (nps[k] < 0 && inside[k] == i)
				{
					vti = vtx + nj[k];
					for (j = 1, area = 0.; j < -nps[k] - 1; j++)
						area += um_triangle_signed_area(vti[0],vti[j],vti[j+1]);
					pocs[c].param -= fabs(area);
				}
			}
			if (frmtype == SMALST) pocs[c].param = -pocs[c].param;
		}
		c++;
	}

	uu_qsort (pocs,nperim,sizeof(NCL_order),ncl_pokcmp);

	for (k = 0, c = 0, nc = nloops; k < nperim && c < nloops; k++)
	{
		if (pocs[k].znum > 1000 && c < nc) nc = c;
/* contours starting with the current are not in zones */
		newl[c] = i = pocs[k].ind; if (c != i) permuted = UU_TRUE; c++;
		for (j = 0; j < nloops && c < nloops; j++)
		{
			if (nps[j] > 0 || inside[j] != i)
				continue;
			newl[c] = j; if (c != j) permuted = UU_TRUE; c++;
		}
	}

	if (nc < nloops) *nctr = nc;
	UU_FREE (pocs);
	return (permuted);
}

/*********************************************************************
**    E_FUNCTION     : ncl_permute_pocs (pock,njlist,npt,newl)
**       Copy a polygon structure.
**    PARAMETERS
**       INPUT  :
**                pol1     - the polygon to copy
**       OUTPUT :
**                pol2     - copy polygon
**
**    RETURNS      :
**         UU_FAILURE if malloc fails, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_permute_pocs (pock,njlist,npt,newl)
ncl_polygon *pock;
UU_LIST *njlist;
int npt;
int *newl;
{
	int i,j,nnl,nnl0;
	int status = UU_SUCCESS;
	int nloops = pock->num_contours;
	int *nps,*nj,*nj0;
	UM_2box *boxes;
	UM_2Dcoord *vtx,*vtj;

	vtx = (UM_2Dcoord *) uu_malloc (npt*sizeof(UM_2Dcoord));
	nps = (int *) uu_malloc (nloops*sizeof(int));
	boxes = (UM_2box *) uu_malloc (nloops*sizeof(UM_2box));
	nj = (int *) uu_malloc (nloops*sizeof(int));
	if (!vtx || !nps || !boxes || !nj)
	{
		status = UU_FAILURE; goto Done;
	}

	nj0 = (int *) UU_LIST_ARRAY (njlist);
	vtj = (UM_2Dcoord *) UU_LIST_ARRAY (pock->contour);
	for (i = 0; i < nloops; i++)
	{
		nps[i] = pock->np[i];
		nj[i] = nj0[i];
		boxes[i].xmin = pock->box[i].xmin;
		boxes[i].xmax = pock->box[i].xmax;
		boxes[i].ymin = pock->box[i].ymin;
		boxes[i].ymax = pock->box[i].ymax;
		for (j = nj0[i]; j < nj0[i+1]; j++)
		{
			vtx[j][0] = vtj[j][0]; vtx[j][1] = vtj[j][1];
		}
	}

	njlist->cur_cnt = nnl0 = 0;
	uu_list_push (njlist,&nnl0);
	pock->contour->cur_cnt = 0;
	for (i = 0; i < nloops; i++)
	{
		pock->np[i] = nps[newl[i]];
		pock->box[i].xmin = boxes[newl[i]].xmin;
		pock->box[i].xmax = boxes[newl[i]].xmax;
		pock->box[i].ymin = boxes[newl[i]].ymin;
		pock->box[i].ymax = boxes[newl[i]].ymax;
		nnl = abs(nps[newl[i]]);
		uu_list_push_multiple (pock->contour,nnl,&vtx[nj[newl[i]]]);
		if (i < nloops - 1)
		{
			nnl0 += nnl; uu_list_push (njlist,&nnl0);
		}
	}

Done:
	UU_FREE (nps);
	UU_FREE (nj);
	UU_FREE (boxes);
	UU_FREE (vtx);

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_order_pock_geo (pock,njlist,zoning,cstk,frmtype,
**                                                               sortpt,ncvs)
**       Rearrange pocket contours so that each perimeter is followed by its
**       islands.
**    PARAMETERS
**       INPUT  :
**                pock   - polygon structure
**       OUTPUT :
**                pock   - updated polygon structure
**                njlist - list used to find each contour in the pocket struct:
**                         nj[k] is the start of the k-th contour.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_order_pock_geo (pock,njlist,zoning,cstk,frmtype,sortpt,ncvs)
ncl_polygon *pock;
UU_LIST *njlist;
int zoning,cstk,frmtype,*ncvs;
UM_2Dcoord sortpt;
{
	int status;
	UU_REAL tol;

	ncl_get_wtol2d (&tol);

	status =
	ncl_order_pock_geo1 (pock,njlist,zoning,cstk,frmtype,sortpt,ncvs,tol);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_order_pock_geo1 (pock,njlist,zoning,cstk,frmtype,
**                                                            sortpt,ncvs,tol)
**       Rearrange pocket contours so that each perimeter is followed by its
**       islands.
**    PARAMETERS
**       INPUT  :
**                pock   - polygon structure
**       OUTPUT :
**                pock   - updated polygon structure
**                njlist - list used to find each contour in the pocket struct:
**                         nj[k] is the start of the k-th contour.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_order_pock_geo1 (pock,njlist,zoning,cstk,frmtype,sortpt,ncvs,tol)
ncl_polygon *pock;
UU_LIST *njlist;
int zoning,cstk,frmtype,*ncvs;
UM_2Dcoord sortpt;
UU_REAL tol;
{
	int i,j,nloops,nnl,c,nperim;
	UU_REAL dx0,dx1,dy0,dy1,toluv,xrange[2],yrange[2];
	int **ins,*INS,*inside,*newl,*nps,*nj;
	int status = UU_SUCCESS;
	UM_2Dcoord *vtx,*vti,*vtj;
	UU_LOGICAL permuted = UU_FALSE;
	UU_LOGICAL nogap = UU_FALSE;
	UU_REAL um_get_rltv_tol();

	njlist->cur_cnt = 0;
	nnl = 0;
	nps = (int *) pock->np;

	nloops = pock->num_contours;
	if (nloops < 2)
	{
		uu_list_push (njlist,&nnl);
		if (zoning > 0)
		{
			UU_REAL znum,area;

			if (cstk == 0)
				znum = zstk;
			else
			{
				nnl = nps[0];
				vti = (UM_2Dcoord *) UU_LIST_ARRAY (pock->contour);
				if (zoning == MOST) area = ncl_closed_area (vti,nnl);
				znum = ncl_set_zone (zoning,&pock->box[0],vti,nnl,area);
			}
			if (znum > 1000) *ncvs = 0;
		}
		return (0);
	}

	inside = (int *) uu_malloc (nloops*sizeof(int));
	newl = (int *) uu_malloc (nloops*sizeof(int));
	ins = (int **) uu_malloc (nloops*sizeof(int *));
	INS = (int *) uu_malloc (nloops*nloops*sizeof(int));
	if (!inside || !newl || !ins || !INS)
	{
		status = UU_FAILURE; goto Done;
	}

	for (i = 0; i < nloops; i++)
	{
		uu_list_push (njlist,&nnl);
		ins[i] = &INS[i*nloops];
		inside[i] = -1;
		nnl += abs(nps[i]);
	}
	uu_list_push (njlist,&nnl);
/*
..... fill out the ins matrix: if loop_j is inside loop_i then ins_ij=1, if
..... loop_i is inside loop_j then ins_ij=-1, if neither then ins_ij=0.
*/
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pock->contour);
	nj = (int *) UU_LIST_ARRAY (njlist);
	for (i = 0; i < nloops-1; i++)
	{
		vti = vtx + nj[i];
		toluv = tol;
		if (nps[i] > 0)
		{
			xrange[0] = pock->box[i].xmin;
			xrange[1] = pock->box[i].xmax;
			yrange[0] = pock->box[i].ymin;
			yrange[1] = pock->box[i].ymax;
			toluv = um_get_rltv_tol(xrange,yrange)*7.0;
		}
		for (j = i+1; j < nloops; j++)
		{
			ins[i][j] = ins[j][i] = 0;
			dx0 = pock->box[j].xmin - pock->box[i].xmin;
			dx1 = pock->box[j].xmax - pock->box[i].xmax;
			dy0 = pock->box[j].ymin - pock->box[i].ymin;
			dy1 = pock->box[j].ymax - pock->box[i].ymax;
			vtj = vtx + nj[j];
			if (fabs(dx0) < toluv && fabs(dx1) < toluv &&
				fabs(dy0) < toluv && fabs (dy1) < toluv && nps[i]*nps[j] < 0)
			{
/*
..... if stock offset is 0, there may be a contour with the same box
*/
				if (nps[i] > 0)
				{
					ins[i][j] = 1; ins[j][i] = -1;
				}
				else
				{
					ins[j][i] = 1; ins[i][j] = -1;
				}
			}
			else
			{
				nogap = (fabs(dx0) < toluv || fabs(dx1) < toluv ||
						fabs(dy0) < toluv || fabs (dy1) < toluv);

				if (dx0 >= -toluv && dx1 <= toluv && dy0 >= -toluv && dy1 <= toluv)
				{
					if (nogap)
						c = 0;
					else
						c = um_check_inside(vti,abs(nps[i]),vtj[0],&pock->box[i],tol);
					if (c > 0 || (c == 0 && nps[i]*nps[j] < 0))
					{
						ins[i][j] = 1; ins[j][i] = -1;
					}
				}
				else if (dx1 >= -toluv && dx0 <= toluv && dy1 >= -toluv && dy0 <= toluv)
				{
					if (nogap)
						c = 0;
					else
						c = um_check_inside(vtj,abs(nps[j]),vti[0],&pock->box[j],tol);
					if (c > 0 || (c == 0 && nps[i]*nps[j] < 0))
					{
						ins[j][i] = 1; ins[i][j] = -1;
					}
				}
			}
		}
	}
/*
..... for each loop the inside is initially =-1; we set it as follows:
..... loop_i.inside = j if loop_i is inside loop_j and there is no loop
..... in between
*/
	for (i = 0, nperim = 0; i < nloops; i++)
	{
		if (nps[i] > 0) nperim++;
		for (j = 0; j < nloops; j++)
		{
			if (j == i) continue;
			if (ins[j][i] != 1) continue;
			if (inside[i] == -1)
				inside[i] = j;
			else
			{
				if (ins[inside[i]][j] == 1) inside[i] = j;
			}
		}
	}

	for (i = 0; i < nloops; i++)
	{
		if (nps[i] < -3 && inside[i] == -1)
		{
			status = UU_FAILURE; goto Done;
		}
	}
/*
..... do the final ordering: perimeters followed by their islands
*/
	if (nperim > 1 && (zoning > 0 || (frmtype > RANDOM && frmtype <= LARGST)))
		permuted = ncl_sort_pocs (pock,nperim,zoning,cstk,frmtype,sortpt,nj,
							inside,newl,ncvs);
	else
	{
		for (i = 0, c = 0; i < nloops && c < nloops; i++)
		{
			if (nps[i] < 0) continue;
			newl[c] = i; if (c != i) permuted = UU_TRUE; c++;
			for (j = 0; j < nloops && c < nloops; j++)
			{
				if (nps[j] > 0 || inside[j] != i)
					continue;
				newl[c] = j; if (c != j) permuted = UU_TRUE; c++;
			}
		}
	}

	if (permuted) status = ncl_permute_pocs (pock,njlist,nnl,newl);

Done:
	UU_FREE (INS);
	UU_FREE (ins);
	UU_FREE (inside);
	UU_FREE (newl);

	return (status);
}

/*********************************************************************
*********************************************************************/
void ncl_get_rotmx (ifl,mx)
int *ifl;
UM_transf mx;
{
	*ifl = rotfl;
	um_tftotf(rotinv,mx);
}

/*********************************************************************
*********************************************************************/
void ncl_get_rotfl (ifl)
int *ifl;
{
	*ifl = rotfl;
}

/*********************************************************************
**    E_FUNCTION: UU_LOGICAL S_get_plrot (nvec,rot,rtinv)
**       Get the transformation flag and rotation matrix for the UP direction.
**    PARAMETERS
**       INPUT  :
**          nvec  - unit vector UP
**       OUTPUT :
**          rot    - rotation matrix
**          rtinv  - inverse matrix
**    RETURNS      :
**         UU_TRUE iff there is a rotation; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_newzaxis_rot (nvec,rot,rtinv)
UM_vector nvec;
UM_transf rot,rtinv;
{
	int i,j;
	UU_REAL dsec;

	if (nvec[2] + UM_DFUZZ < 1.)
	{
		rot[3][0] = rot[3][1] = rot[3][2] = 0.0;
		rot[0][2] = nvec[0]; rot[1][2] = nvec[1]; rot[2][2] = nvec[2];
/*
.....Added check for NCL_waterline flag so the matrix will be exact when
.....routines not used for waterline define a rotation matrix, but waterline
.....will still uses the same logic - ASF 7/31/13.
*/
		if ((!NCL_waterline && fabs(rot[1][2]) + UM_DFUZZ < 1.) ||
	 		fabs(rot[1][2]) < 0.9999)
		{
			rot[0][0] = nvec[2]; rot[1][0] = 0.; rot[2][0] = -nvec[0];
		}
		else
		{
			rot[0][0] = rot[1][0] = 0.; rot[2][0] = nvec[1];
		}
		dsec = sqrt(rot[0][0]*rot[0][0]+rot[1][0]*rot[1][0]+rot[2][0]*rot[2][0]);
		rot[0][0] /= dsec; rot[1][0] /= dsec; rot[2][0] /= dsec;
		rot[0][1] = rot[1][2]*rot[2][0] - rot[2][2]*rot[1][0];
		rot[1][1] = rot[2][2]*rot[0][0] - rot[0][2]*rot[2][0];
		rot[2][1] = rot[0][2]*rot[1][0] - rot[1][2]*rot[0][0];

		rtinv[3][0] = rtinv[3][1] = rtinv[3][2] = 0.0;
		for (i = 0; i < 3; i++)
			for (j = 0; j < 3; j++) rtinv[i][j] = rot[j][i];

		return (UU_TRUE);
	}
	else
		return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION: void ncl_define_rotmx1 (mm,nvec,rot,mcsflg)
**       Get the transformation flag and matrix for the UP direction.
**    PARAMETERS
**       INPUT  :
**          mm    - 1 iff millimeters
**          nvec  - unit vector UP
**          itsk  - iff 1 do not include modsys into rot, just set
**                  the flag
**       OUTPUT :
**          rot    - transformation matrix
**          mcsflg - MODSYS flag
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_define_rotmx1 (mm,nvec,rot,mcsflg,itsk)
UM_int2 mm;
UM_vector nvec;
UM_transf rot;
int *mcsflg,itsk;
{
	int i,j;
	UU_REAL pm = 1./25.4;
	UM_transf mcsmat;

	ncl_mcstf(mcsflg, mcsmat);
	um_identtf(rotinv);

	rotfl = 0;

	if (ncl_newzaxis_rot (nvec,rot,rotinv))
	{
		rotfl = 1;
		if (*mcsflg || mm)
		{
			if (*mcsflg && itsk == 0) um_tftmtf(rotinv,mcsmat,rotinv);
			if (mm)
			{
				for (i = 0; i < 3; i++)
					for (j = 0; j < 3; j++)
						rotinv[i][j] *= pm;
			}
			um_inverttf(rotinv,rot);
		}
	}
	else if (*mcsflg || mm)
	{
		if (*mcsflg)
		{
			rotfl = 1;
			if (itsk == 0) um_tftotf (mcsmat,rotinv);
			if (mm)
			{
				for (i = 0; i < 3; i++)
					for (j = 0; j < 3; j++)
						rotinv[i][j] *= pm;
			}
		}
		else
		{
			rotfl = 2;
			for (i = 0; i < 4; i++)
				for (j = 0; j < 3; j++)
					rotinv[i][j] = 0.;
			rotinv[0][0] = rotinv[1][1] = rotinv[2][2] = pm;
		}
		um_inverttf (rotinv,rot);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION: void ncl_define_rotmx (mm,nvec,rot,mcsflg)
**       Get the transformation flag and matrix for the UP direction.
**    PARAMETERS
**       INPUT  :
**          mm    - 1 iff millimeters
**          nvec  - unit vector UP
**          mm    - 1 iff millimeters
**          mm    - 1 iff millimeters
**       OUTPUT :
**          rot    - transformation matrix
**          mcsflg - MODSYS flag
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_define_rotmx (mm,nvec,rot,mcsflg)
UM_int2 mm;
UM_vector nvec;
UM_transf rot;
int *mcsflg;
{
	ncl_define_rotmx1 (mm,nvec,rot,mcsflg,0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_ipv_contour_stock(sfky,nvec,offdis,zbot,
**                                                          ztop,points)
**       Create pseudo-stock for a collection of surfaces.
**    PARAMETERS
**       INPUT  :
**          sfky       - list of surface keys
**          nvec       - vector defining UP direction
**          offdis     - horizontal offset
**          zbot       - vertical offset below the lowest surface point
**          ztop       - vertical offset above the highest surface point
**          lwrk       - UU_TRUE = return points in Working coordinate system.
**                       UU_FALSE = Modelling coordinate system.
**       OUTPUT :
**          points      - stock contour containing all surfaces
**          nvec        - its length now defines the vertical size of
**                        the stock
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ipv_contour_stock(sfky,nvec,offdis,zbot,ztop,points,lwrk)
UU_LIST *sfky,**points;
UM_vector nvec;
UU_REAL offdis,zbot,ztop;
UU_LOGICAL lwrk;
{
	UU_KEY_ID *sfkey;
	UM_int2 mm,isub;
	UM_real8 tol8;
	int i,k,status,numsf,mcsflg = 0,NPT;
	UU_REAL dsec,tol,toler;
	UU_REAL zmax,zmin;
	NCL_waterline_surf *sff = UU_NULL;
	UM_transf rot;
	UM_coord *ptio;
	ncl_polygon *pol0;

	numsf = sfky->cur_cnt;
	if (numsf < 1) return (UU_FAILURE);
	sfkey = (UU_KEY_ID *) UU_LIST_ARRAY (sfky);

	status = UU_SUCCESS;

	getsct(&tol8);
	tol = tol8;

	isub = 264;
	getifl(&isub,&mm);
	toler = (mm)? tol/25.4: tol;

	ncl_define_rotmx1 (mm,nvec,rot,&mcsflg,1);
	if (!lwrk) mcsflg = UU_FALSE;

	sff = (NCL_waterline_surf *) uu_malloc (numsf * sizeof (*sff));
	if (!sff) return (UU_FAILURE);

	NPT = 1;
	status = ncl_process_netsf (1,numsf,sfkey,rot,&zmax,&zmin,sff,tol,toler,
		&NPT,0.,UU_NULL,UU_NULL,UU_NULL);
	if (status != UU_SUCCESS) goto Err;

	if (NPT < 100) NPT = 100;

	status = ncl_init_wpol (0,2*NPT);
	if (status == UU_SUCCESS) ncl_init_wpol (1,NPT);

	if (status != UU_SUCCESS) goto Err;

	status = ncl_create_contour_stock (numsf,sff,rot,tol,tol*tol);
/*
..... offset the stock by the expansion factor
*/
	if (status == UU_SUCCESS)
	{
		ncl_get_wpol (0,&pol0);
		status = ncl_offset_out0 (points,pol0,(UU_LIST *)UU_NULL,offdis,tol);
	}

	if (status != UU_SUCCESS || !(*points) || (*points)->cur_cnt < 4) goto Err;

	dsec = zmax + ztop - zmin + zbot;
	if (dsec < UM_FUZZ) goto Err;
	um_vctmsc (nvec,dsec,nvec);
	if (mm == 1)
	{
		for (k = 0; k < 3; k++) nvec[k] /= 25.4;
	}
	if (mcsflg) ncl_wcstomcs(1,nvec,nvec);

	ptio = (UM_coord *) UU_LIST_ARRAY (*points);

	zmin -= zbot;

	isub = NCLI_POINT;
	for (i = 0; i < (*points)->cur_cnt; i++)
	{
		ptio[i][2] = zmin;
		if (rotfl)
		{
			um_cctmtf (ptio[i],rotinv,ptio[i]);
			if (mcsflg) ncl_wcstomcs(0,ptio[i],ptio[i]);
		}
	}

	goto Done;

Err:
	status = UU_FAILURE;
Done:
	if (sff)
	{
		NCL_waterline_surf *p1;
		p1 = sff;
		for (i = 0; i < numsf && p1->key != NULLKEY; i++, p1++)
		{
			ncl_free_bound (&p1->bound);
			UU_LIST_FREE (p1->trianlist);
		}
		UU_FREE (sff);
	}
	ncl_free_wpols();

	return (status);
}

/*********************************************************************
**    E_FUNCTION: ncl_match_boxes(cbox,pock)
**       Determine if a box matches any of the polygon contour boxes.
**    PARAMETERS
**       INPUT  :
**          pock   - polygon structure containing contours
**          cbox   - box to match
**       OUTPUT : none
**    RETURNS      : UU_TRUE / UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_match_boxes (cbox,pock)
UM_2box *cbox;
ncl_polygon *pock;
{
	int ib,nb;
	UM_2Dcoord *vtx;
	UU_REAL ri,ar,a0,b0,a1,b1;

	nb = pock->num_contours;
	if (nb < 1)
		return (UU_FALSE);
	if (!pock->box)
	{
		UM_2box *box;
		int iv,nv;

		pock->box = (UM_2box *) uu_malloc (nb*sizeof(UM_2box));
		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pock->contour);
		for (ib = 0, nv = 0; ib < nb; ib++, vtx += nv)
		{
			box = pock->box + ib;
			box->xmin = box->xmax = vtx[0][0];
			box->ymin = box->ymax = vtx[0][1];
			nv = abs(pock->np[ib]);
			for (iv = 1; iv < nv; iv++)
			{
				if (box->xmin > vtx[iv][0]) box->xmin = vtx[iv][0];
				else if (box->xmax < vtx[iv][0]) box->xmax = vtx[iv][0];
				if (box->ymin > vtx[iv][1]) box->ymin = vtx[iv][1];
				else if (box->ymax < vtx[iv][1]) box->ymax = vtx[iv][1];
			}
		}
	}
/*
..... Currently, boxes match if they intersect and the contour box does
..... not increase more than by half
*/
	for (ib = 0; ib < nb; ib++)
	{
		if (pock->np[ib] > 0 && pock->box[ib].xmin <= cbox->xmax &&
				pock->box[ib].xmax >= cbox->xmin &&
				pock->box[ib].ymin <= cbox->ymax &&
				pock->box[ib].ymax >= cbox->ymin)
		{
			ar = (pock->box[ib].xmax - pock->box[ib].xmin)*
				(pock->box[ib].ymax - pock->box[ib].ymin);
			if (ar < UM_DFUZZ) continue;
			a0 = MIN2(pock->box[ib].xmin,cbox->xmin);
			a1 = MAX2(pock->box[ib].xmax,cbox->xmax);
			b0 = MIN2(pock->box[ib].ymin,cbox->ymin);
			b1 = MAX2(pock->box[ib].ymax,cbox->ymax);
			ri = ((a1 - a0)*(b1 - b0))/ar;
			if (ri <= 1.5) return (UU_TRUE);
		}
	}
	return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION: ncl_weed_contours(pol,tolsq,force)
**       In a set of contours: remove unnecessary middle points on linear
**       (within tolerance) segment. contours. Similar to LINEX in off10.f
**    PARAMETERS
**       INPUT  :
**          pol    - polygon structure containing contours
**          tolsq  - squared tolerance
**          force  - UU_TRUE = weed polygon even if only one contour.
**                   UU_FALSE = must be at least 2 polygons to weed.
**       OUTPUT :
**          pol    - updated polygon
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_weed_contours (pol,tolsq,force)
ncl_polygon *pol;
UU_REAL tolsq;
UU_LOGICAL force;
{
	int nc,c,nv,nv0;

	nc = pol->num_contours;
	if (nc == 0 || (nc < 2 && !force)) return;
/*
..... weed out points
*/
	for (c = 0, nv0 = 0; c < nc; c++)
	{
		nv = abs(pol->np[c]);

		if (nv >= 3) S_weed_pts (pol->contour,nv0,&nv,tolsq);
		if (nv < 3)
		{
			ncl_delete_contour (nc,c,nv0,nv,pol);
			nc--; c--;
		}
		else
		{
			pol->np[c] = (pol->np[c] > 0)? nv: -nv;
			nv0 += nv;
		}
	}

	pol->num_contours = nc;
}

/*********************************************************************
*********************************************************************/
static void ncl_pts_to_pol(q,bx,npt,pts)
UU_LIST *q;
UM_2box *bx;
int npt;
UM_coord *pts;
{
	int i;

	i = MAX2 (npt,10);
	uu_list_init (q,sizeof(UM_2Dcoord),i,i);

	bx->xmin = bx->xmax = pts[0][0];
	bx->ymin = bx->ymax = pts[0][1];
	uu_list_push (q,&pts[0]);

	for (i = 1; i < npt; i++)
	{
		bx->xmin = MIN2 (bx->xmin,pts[i][0]);
		bx->xmax = MAX2 (bx->xmax,pts[i][0]);
		bx->ymin = MIN2 (bx->ymin,pts[i][1]);
		bx->ymax = MAX2 (bx->ymax,pts[i][1]);
		uu_list_push (q,&pts[i]);
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_prepare_zones (nio,ptsio,tol)
**       Evolve zone curves - store them as polygons.
**    RETURNS      :   UU_SUCCESS iff no error; else UU_FAILURE
*********************************************************************/
int ncl_prepare_zones (wbase,nio,ptsio,rot,tol)
NCL_w_base *wbase;
UU_LIST *nio,*ptsio;
UM_transf rot;
UU_REAL tol;
{
	int status,i,npts,*np,*ni;
	UM_int2 i2;
	UM_int4 key4;
	UU_KEY_ID skey;
	UU_REAL uv[2];
	struct NCL_fixed_databag c1;
	UM_transf tfmat,*tf;
	UM_2box *bx;
	UM_coord *pts;
	UU_LIST *cntr,pptr;
	UU_LOGICAL lproj;

	status = UU_SUCCESS;
	UU_LIST_EMPTY (ptsio); UU_LIST_EMPTY (nio);
	uu_list_init (&pptr,sizeof(UM_coord),0,100);
	lproj = ncl_is_watrev();
	if (lproj)
	{
		skey = wbase->srf->key;
		uv[0] = wbase->u; uv[1] = wbase->v;
	}

	for (i = 0; i < 100 && status == UU_SUCCESS; i++)
	{
		i2 = i+1;
		gtsky (&i2,&key4);
		if (key4 <= 0)
		{
			ZLAST = (key4 == 0); break;
		}
		c1.key = key4;
		status = ncl_retrieve_data_fixed (&c1);
		if (status == UU_SUCCESS)
		{
			UU_LIST_EMPTY (&pptr);
			npts = 0;

			if (lproj)
			{
				status = ncl_cv_project_sfuv(c1.key,skey,uv,tol,&pptr,&npts);
			}
			else
			{
				if (ncl_itsa_compcrv(&c1))
				{
					npts =
					ncl_evolve_composite_curve(&c1,tol,&pptr,UU_NULL,UU_NULL,0);
				}
				else
				{
					tf = &tfmat;
					status = ncl_trimsrf_get_tf (&c1,&tf);
					if (status == UU_SUCCESS)
						npts = ncl_evolve_curve (&c1,tf,tol,&pptr,UU_NULL,UU_NULL,0);
				}
			}
			if (npts < 3) status = UU_FAILURE;
			if (status == UU_SUCCESS)
			{
				uu_list_push(nio,&npts);
				uu_list_push_list (ptsio,&pptr);
			}
		}
	}

	if (status == UU_SUCCESS)
	{
		nzones = nio->cur_cnt;
		if (nzones > 0)
		{
			ni = (int *) UU_LIST_ARRAY (nio);
			pts = (UM_coord *) UU_LIST_ARRAY (ptsio);

			if (rotfl > 0 || !lproj)
			{
				npts = ptsio->cur_cnt;
				for (i = 0; i < npts; i++)
					um_cctmtf(pts[i],rot,pts[i]);
			}

			np = (int *) uu_malloc (nzones*sizeof(int));
			bx = (UM_2box *) uu_malloc (nzones*sizeof(UM_2box));
			cntr = (UU_LIST *) uu_malloc (nzones*sizeof(UU_LIST));
			Szone = (ncl_polygon *) uu_malloc (nzones*sizeof(ncl_polygon));
			for (i = 0; i < nzones; i++, pts += npts)
			{
				Szone[i].num_contours = 1;
				npts = ni[i];
				np[i] = npts;
				Szone[i].np = &np[i];
				Szone[i].box = &bx[i];
				Szone[i].contour = &cntr[i];
				ncl_pts_to_pol(&cntr[i],&bx[i],npts,pts);
			}
		}
	}

	UU_LIST_EMPTY (ptsio); UU_LIST_EMPTY (nio);
	uu_list_free (&pptr);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_free_zones()
**       Free zone polygons.
*********************************************************************/
void ncl_free_zones()
{
	int i;
	if (Szone != UU_NULL)
	{
		for (i = 0; i < nzones; i++)
			uu_list_free (Szone[i].contour);
		UU_FREE (Szone[0].np);
		UU_FREE (Szone[0].box);
		UU_FREE (Szone[0].contour);
		UU_FREE (Szone);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_sfboundary(srf,tfmat,bound)
**		Debug surface boundary data
**    PARAMETERS
**       INPUT  :
**			srf		- surface data
**			tfmat	- surface transformation
**          bound	- surface boudnary
**       OUTPUT :
**          lp.lis file when UU_DEBUGL=1 is set
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_sfboundary(srf,tfmat,bound)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_srf_boundary *bound;
{
	int i,ib,npts,nb,status;
	char tbuf[80];		
	UM_coord *pts,*uvs,ptmid;
	UU_REAL umid,vmid, dist;

	struct UM_evsrfout *evsrf;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	status = uc_init_evsrfout(srf, evsrf);
	if (status != UU_SUCCESS)
		return;

	pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
	sprintf(tbuf,"$$srf->key/%d",srf->key);
	NclxDbgPstr(tbuf);

	for (ib = 0; ib < bound->nb; ib++)
	{
		npts = bound->np[ib];
		sprintf(tbuf,"$$boundary nb=%d, numbers of points=%d",ib,npts);
		NclxDbgPstr(tbuf);
		for (i = 0; i < npts-1; i++)
		{
			sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
				pts[i][0],pts[i][1],pts[i][2],
				pts[i+1][0],pts[i+1][1],pts[i+1][2]);
			NclxDbgPstr(tbuf);
		}
#if 0
		sprintf(tbuf,"*stop");
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"draft/modify,color = 8");
		NclxDbgPstr(tbuf);
		uvs = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
		for (i = 0; i < npts-1; i++)
		{
			sprintf(tbuf,"draft/modify,color = 8");
			NclxDbgPstr(tbuf);

			status = uc_evsrf (UM_POINT,uvs[i][0],uvs[i][1],srf,tfmat,evsrf);
			sprintf(tbuf,"pt/%10.6f,%10.6f,%10.6f",evsrf->sp[0],evsrf->sp[1],evsrf->sp[2]);
			NclxDbgPstr(tbuf);

			umid = 0.5 * (uvs[i][0] + uvs[i+1][0]);
			vmid = 0.5 * (uvs[i][1] + uvs[i+1][1]);
			status = uc_evsrf (UM_POINT,umid,vmid,srf,tfmat,evsrf);

			sprintf(tbuf,"draft/modify,color = 4");
			NclxDbgPstr(tbuf);
			sprintf(tbuf,"pt/%10.6f,%10.6f,%10.6f",evsrf->sp[0],evsrf->sp[1],evsrf->sp[2]);
			NclxDbgPstr(tbuf);

			um_middlept(pts[i],pts[i+1],ptmid);
			dist = um_sqdis(ptmid, evsrf->sp);
			if (dist > UM_FUZZ)
			{
				sprintf(tbuf,"*stop");
				NclxDbgPstr(tbuf);				
				sprintf(tbuf,"$$distance is %10.6f",dist);
				NclxDbgPstr(tbuf);
			}
		}
#endif
		pts += npts;
	}

	uu_free(evsrf);

#if 0
	uvs = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	if (npts > 0)
	{
		int i;
		char tbuf[80];
		sprintf(tbuf,"draft/modify,color = 4");
		NclxDbgPstr(tbuf);
		for (i = 0; i < npts-1; i++)
		{
			sprintf(tbuf,"ln/%10.6f,%10.6f,%10.6f,%10.6f",
				uvs[i][0],uvs[i][1],uvs[i+1][0],uvs[i+1][1]);
			NclxDbgPstr(tbuf);
		}
	}
#endif
}

/*********************************************************************
**    E_FUNCTION     : ncl_copy_sfboundary(sfbndr,sfbound)
**		Copy surface boundary data
**    PARAMETERS
**       INPUT  :
**          sfbndr	- surface boudnary
**       OUTPUT :
**          sfbound	- pointer to the copy of surface boundary
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int  ncl_copy_sfboundary(sfbndr,sfbound)
UM_srf_boundary *sfbndr;
UM_srf_boundary **sfbound;
{
	int i,npts,nb,n;
	UM_coord *pts;
	UM_srf_boundary *bound;
		
	bound = (UM_srf_boundary *) uu_malloc (sizeof(UM_srf_boundary));
	um_init_boundary (bound);

	if (sfbndr->nb < 1)
		return UU_FAILURE;

	if (sfbndr->nb >= 1)
	{
		bound->toler = sfbndr->toler;
		bound->nb = nb = sfbndr->nb;
		bound->np = (int *) uu_malloc((nb)*sizeof(int));
		bound->ummx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
		bound->vmmx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
		for (i = 0; i < nb; i++) 
		{ 
			bound->np[i] = sfbndr->np[i];
			um_vctovc_2d(sfbndr->ummx[i], bound->ummx[i]);
			um_vctovc_2d(sfbndr->vmmx[i], bound->vmmx[i]);
		}
		bound->uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		bound->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		n = sfbndr->cvpts->cur_cnt;
		uu_list_init (bound->uvpts, sizeof(UM_coord), n, n);
		uu_list_push_list (bound->uvpts, sfbndr->uvpts);
		uu_list_init (bound->cvpts, sizeof(UM_coord), n, n);
		uu_list_push_list (bound->cvpts, sfbndr->cvpts);
	}

	*sfbound = bound;

	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : ncl_rot_sfboundary(rot,sfbound)
**		Copy surface boundary data
**    PARAMETERS
**       INPUT  :
**          sfbound	- surface boudnary
**          rot     - transformation matrix (from Unibase to part coordsys)
**       OUTPUT :
**          sfbound	- the rotated surface boundary
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void  ncl_rot_sfboundary(rot,sfbound)
UM_transf rot;
UM_srf_boundary *sfbound;
{
	int i,ib,npts;
	UM_coord *pts;

	pts = (UM_coord *) UU_LIST_ARRAY (sfbound->cvpts);
	for (ib = 0; ib <sfbound->nb; ib++)
	{
		npts =sfbound->np[ib];
		for (i = 0; i < npts; i++)
			um_cctmtf(pts[i],rot,pts[i]);
		pts += npts;
	}
}

/*********************************************************************
**    E_FUNCTION     : um_cvio_create_intersection2(
**                           sfi,plvec,pl,rot,told,flag,ncvs,igui,ier)
**       Create surface and surface(plane) intersections for waterline
**       Use the tesselation data from sfi
**    PARAMETERS
**       INPUT  :
**          sfi      - current waterline surface data
**          plvec    - plane normal vector
**			pl		 - plane
**          rot      - transformation matrix (from Unibase to part coordsys)
**			told	 - tolerance
**          flag     - 0 = Create untrimmed curve only, 1 = Create
**                     trimmed curves if any
**       OUTPUT :
**          ncvs     - Number of curves created.  If set to 1, then
**                     only the untrimmed curve is created, for
**                     example, when the two input surfaces are not
**                     trimmed.
**          ier      - error number, if fail
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvio_create_intersection2(sfi,plvec,pl,rot,told,flag,ncvs,igui,ier)
NCL_waterline_surf *sfi;
UM_vector plvec;
struct NCL_nclpl_rec *pl;
UM_transf rot;
UM_real8 *told;
UM_int4 *flag;
UM_int2 *ncvs,igui,*ier;
{
	int i,ib,irot,k,npts,status,icolor,insert,inside;
	struct NCL_fixed_databag sf;
	UM_srf_boundary *bound,*bound0;
	UM_vector vec,vcross;
	UM_transf tfmat,plmx,plinv;
	UU_LOGICAL lsolid,plflg,ltrmsf,ldebug,luv;
	UU_REAL d,dpl,u,v,dtol,dtol1,toler,wtol;
	UU_REAL box1[6],dx,dy,dz,dmax;
		
	UM_int2 primtyp;
	UM_real8 primdata[16];
	nclsf_prim_type typ;
	int npo,*npi;
	UM_int2 isub, mm;

	UM_coord *pts, *uvs;
	int istat,inner,irpt;
	UM_coord ptx;
	int nb;
	UM_srf_boundary *sfbndr,*sfbndr0;
	char buf[120];

	UM_f77_str_ptr str77;
	UM_init_f77_str (str77, buf, 80);
	buf[0] = '\0';

	status = UU_SUCCESS;
	insert = UU_FAILURE;
	inside = UU_FAILURE;
	
	isub = 264;	
	getifl(&isub,&mm);

/*
.....Calculate bounding box 
*/
	dmax = 1.0;
	dx = sfi->box.xmax - sfi->box.xmin;
	dy = sfi->box.ymax - sfi->box.ymin;
	dz = sfi->zmax - sfi->zmin;
	dmax = MAX3(dx,dy,dz);
	dmax = (mm == 1) ? dmax/25.4 : dmax;

/*
.....toler: Inch tolerance 
.....dtol:  boundary tolerance
.....dtol1:  tight boundary tolerance
.....wtol:  current unit tolerance
*/
	toler =*told;
	dtol = toler;
	dtol = dtol/dmax;
	dtol = MAX2(dtol/20,UM_DFUZZ);
	wtol = (mm == 1) ? toler*25.4 : toler;

	ncl_set_boundary_toler (toler);
	um_set_tess_toler (toler);
	ncl_get_rotfl (&irot);
	plflg = ncl_newzaxis_rot (plvec,plmx,plinv);
	dpl = UM_DOT (pl->pt,pl->nvec);

/*
.....Get surface data
*/
	sf.key = sfi->key;
	status = ncl_retrieve_data_fixed (&sf);
	if (status == UU_SUCCESS)
	{		
		lsolid = (sf.rel_num == UM_SOLID_REL);
		status = uc_retrieve_transf (sf.key, tfmat);
	}

	istat = ncl_get_sf_primdat(&sf.key,&primtyp,primdata);
	if (istat == UU_SUCCESS && (primtyp <= 1 || primtyp > 7))
		ncl_wat_check_prim (&sf,tfmat,&primtyp,primdata,toler);
	if (istat == UU_SUCCESS) typ = (nclsf_prim_type) primtyp;

	if (status != UU_SUCCESS)
	{
		Sfplio_err (&sf,str77,igui,ier,163);
		goto Done;
	}

/*
.....Check if surface is planar
*/
	if (typ == NCLSF_PLANE)
	{
		if (irot == 1)
			ncl_transform_primdat (&typ,primdata,rot);

		for (k = 0; k < 3; k++)
			vec[k] = primdata[k];
		um_cross (vec,pl->nvec,vcross);

		d = UM_DOT (vcross,vcross);
		if (d < 0.01 * UM_FUZZ)
			istat = -1;
		else
		{
			um_unitvc (vcross,vcross);
			ncl_set_ldir (vcross);
			npo = sfi->bound.npo;
			npi = (sfi->bound.nb > 1)? (sfi->bound.np): UU_NULL;
			istat = um_cvio_intersection_boundary (sfi->bound.nb,npo,npi,
								sfi->bound.cvpts,pl->nvec,dpl,dtol);
/*
.....Display the intersection lines
*/
			icolor = 4;
			ldebug = UU_FALSE;
			if (ldebug)
				um_cvio_display(0,icolor,1);
		}
	}
	else
	{
/*
.....Intersect plane with triangles
*/
		UU_LIST trilst;
		UU_LIST *trianlst;
		UM_trian *ptri;
		int ntri = 0;
		status = nclc_tessmgr_get_tess_trianlst(sfi->key,&trianlst,UU_NULL);
		if (status != UU_SUCCESS)
			goto Done;
		ntri = UU_LIST_LENGTH(trianlst);
/*
.....Copy the original tess trianlst
*/
		uu_list_init (&trilst,sizeof (UM_trian),ntri,ntri);
		uu_list_push_list (&trilst, trianlst);

		if (irot > 0)
		{
			ntri = UU_LIST_LENGTH(&trilst);
			ptri = (UM_trian *)UU_LIST_ARRAY(&trilst);
			for (i = 0; i < ntri; i++, ptri++)
			{
				um_cctmtf(ptri->p1,rot,ptri->p1);
				um_cctmtf(ptri->p2,rot,ptri->p2);
				um_cctmtf(ptri->p3,rot,ptri->p3);
			}
		}

		if (status == UU_SUCCESS)	
			status = um_cvio_intersection_triangles(pl,&trilst,toler);		

		uu_list_free (&trilst);	

/*
.....Create Scvio list from the sorted intersection points
*/
		um_cvio_sort_points (toler);

/*
.....Debug display the untrimmed curve before project	
*/
		icolor = 4;
		ldebug = UU_FALSE;
		if (ldebug)
			um_cvio_display(0,icolor,1);
/*
.....Initialize surface boundary
*/	
		if (irot > 0)
		{
			bound = (UM_srf_boundary *) uu_malloc (sizeof(UM_srf_boundary));
			um_init_boundary (bound);

			bound0 = (UM_srf_boundary *) uu_malloc (sizeof(UM_srf_boundary));
			um_init_boundary (bound0);
		}
/*
.....Get the machine tolerance and tight surface boundary
*/		
		status = nclc_tessmgr_get_srf_bndr(sfi->key,&sfbndr,&sfbndr0);

/*
.....Trim curves to trimmed surface boundarys
*/
		ltrmsf = (ncl_itsa_trimsrf(&sf));
		if (ltrmsf)
		{
/*
.....Project intersection points, and update the projected points,uv-points,
.....into the active Scvio list
*/
			u = (sfbndr0->ummx[0][0] + sfbndr0->ummx[0][1]) * 0.5; 
			v = (sfbndr0->vmmx[0][0] + sfbndr0->vmmx[0][1]) * 0.5;
			um_cvio_project_points(sfi->key,plvec,dpl,plflg,UU_FALSE,plmx,plinv,
				wtol,&u,&v);
			

/*
.....Debug display the untrimmed curve after project	
*/
			icolor = 6;
			ldebug = UU_FALSE;
			if (ldebug)
				um_cvio_display(0,icolor,1);

/*
.....Check if spherical surface or not
*/
			luv = UU_TRUE;
			luv = um_cvio_sphere_srf(&sf,tfmat,toler);

/*
.....Copy the original surface boundary
*/
			if (irot > 0)
			{
				ncl_copy_sfboundary(sfbndr,&bound);
				ncl_rot_sfboundary(rot,bound);

				ncl_copy_sfboundary(sfbndr0,&bound0);							
				ncl_rot_sfboundary(rot,bound0);
			}
				 
/*
.....Get the intersection of curve with surface boundary
.....Insert the intersection points
*/
			if (irot > 0)
			{
				insert = um_cvio_intersection_srfbound(&sf,bound0,bound,
					tfmat,pl,irot,luv,&toler,&inner);
			}
			else
			{
				insert = um_cvio_intersection_srfbound(&sf,sfbndr0,sfbndr,
					tfmat,pl,irot,luv,&toler,&inner);
			}

			if (insert != UU_SUCCESS)
			{
				if (irot > 0)
					inside = um_cvio_inside_boundary(bound0,&toler,luv);
				else
					inside = um_cvio_inside_boundary(sfbndr0,&toler,luv);
				if (inside != UU_SUCCESS)
					um_cvio_empty_all();
			}
/*
.....Trim curves within the tight surface boundary
*/
			irpt = 0;
			inner = 2;
			if (insert == UU_SUCCESS)
			{
				if (irot > 0)
					um_cvio_trim(&sf,bound,tfmat,luv,flag,&toler,&irpt,&ptx,&inner);
				else			
					um_cvio_trim(&sf,sfbndr,tfmat,luv,flag,&toler,&irpt,&ptx,&inner);
			}

/*
.....Free memory allocated
*/
			if (irot > 0 && bound->nb > 0)		
				um_free_boundary (bound);
			if (irot > 0 && bound0->nb > 0)		
				um_free_boundary (bound0);
/*
.....Debug display the trimmed curve
*/
			icolor = 5;
			ldebug = UU_FALSE;
			if (ldebug)
				um_cvio_display(-1,icolor,1);
		}
		else
		{
/*
.....Debug display the untrimmed curve
*/
			icolor = 4;
			ldebug = UU_FALSE;
			if (ldebug)
				um_cvio_display(0,icolor,1);
		}
	}

Done:

	return status;
}

/*********************************************************************
**    E_FUNCTION     : ncl_sf_pln_io2(sfi,pl,nvec,rot,nio,ptsio,
**							htop,tol2d,tol,itsk)
**       Intersect a surface with a plane using um_cvio_intersection()
**    PARAMETERS
**       INPUT  :
**          sfi        - current surface boundary data
**			pl		   - plane
**			nvec	   - plane normal vector
**          nio        - list containing numbers of points in each polyline
**          ptsio      - list of polylines points
**          htop       - Z-level of the plane
**			tol2d 	   - tolerance
**			tol 	   - tolerance
**          itsk       - 1 iff called from necvlev
**       OUTPUT : none
**
**    RETURNS      :
**         1 iff intersection found; 0 else
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sf_pln_io2 (sfi,pl,nvec,rot,nio,ptsio,htop,tol2d,tol,itsk)
NCL_waterline_surf *sfi;
struct NCL_nclpl_rec *pl;
UM_vector nvec;
UM_transf rot;
UU_LIST *nio,*ptsio;
UU_REAL htop,tol2d,tol;
int itsk;
{
	int j,npts,chunks,nj,cur0,status;
	int *np;
	UM_trian *ptri;
	UM_coord *ptio;
	UU_REAL eps,tol1,d;
	UM_vector vec;
	UU_LIST cvlst,uvlst;
	int irpt =1;
	int igui = 0;
	int ier = 0;

	UM_int2 isub, mm;
	isub = 264;	getifl(&isub,&mm);

	tol = (mm == 1) ? tol/25.4 : tol;
	eps = tol2d*tol2d;
	tol1 = 0.1*tol;

	ncl_nul_uv();
	ncl_nul_nios();
	uu_list_init0 (&cvlst);
	uu_list_init0 (&uvlst);

/*
.....Create surface-plane interesections
*/

/*
.....Initialize Scvio list
*/
	um_cvio_new();

/*
.....Intersect surface with plane
*/
	status = ncl_cvio_create_intersection2(sfi,nvec,pl,
		rot,&tol,&irpt,1,igui,ier);

/*
.....Put active Scvio curve list into NCL_uvintof
*/
	um_cvio_push_ncluv();

/*
.....Free Scvio list
*/
	um_cvio_free();

	if (itsk == 1 && sfi->flag1 == VSOLID)
	{
		uu_list_init (&cvlst, sizeof(UM_coord), 100, 200);
		uu_list_init (&uvlst, sizeof(UM_coord), 0, 200);
		ncl_separate_components (&cvlst,&uvlst,tol2d);
		npts = cvlst.cur_cnt;
		if (npts < 4)
		{
			uu_list_free (&uvlst);
			uu_list_free (&cvlst);
			return (0);
		}
		ptio = (UM_coord *) UU_LIST_ARRAY (&cvlst);
	}
	else
	{
		ncl_getpts_uv (&npts,&ptio);
		if (npts <= 0) return (0);
	}
	chunks = 1;
	ncl_get_nios (&chunks,&np);

	if (chunks > 1)
	{
		for (j = 0; j < chunks; j++)
		{
			cur0 = ptsio->cur_cnt;
			npts = np[j];
			nj = npts;
			uu_list_push (ptsio,ptio[0]);

			ncl_waterline_weed(ptio,&nj,ptsio,eps,itsk);
			if (nj > 1)
				uu_list_push (nio, &nj);
			else
				ptsio->cur_cnt = cur0;

			ptio += npts;
		}
	}
	else
	{
		cur0 = ptsio->cur_cnt;
		nj = npts;
		uu_list_push (ptsio,ptio[0]);

		ncl_waterline_weed(ptio,&nj,ptsio,eps,itsk);
		if (nj > 1)
			uu_list_push (nio, &nj);
		else
			ptsio->cur_cnt = cur0;
	}

	uu_list_free (&uvlst);
	uu_list_free (&cvlst);

	sfi->ncvs = nio->cur_cnt - sfi->nlist0;
	return (sfi->ncvs);
}
