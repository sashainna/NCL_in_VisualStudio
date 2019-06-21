/*********************************************************************
**   NAME:  nesfbuild.c
**    CONTAINS:
**      UU_KEY_ID S_create_spline1
**      void ncl_create_wline
**      int ncl_create_bsf
**      int S_init_bndr_proj
**      void S_pt_proj
**      void S_pt_interp
**      void S_pt_inter_proj
**      void S_bndr_proj
**      void S_fix_bndr_proj
**      int S_close_gaps
**      void S_fix_final
**      int S_common_bndr
**      int S_inner_bndr
**      int S_new_trim1
**      int ncl_new_trim
**
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesfbuild.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:48
*********************************************************************/

#include "ncldef.h"
#include "mdattr.h"
#include "modef.h"
#include "mgeom.h"
#include "mdeval.h"
#include "nccs.h"
#include "uminmax.h"
#include "fmill.h"
#include "msrf.h"
#include "mattr.h"
#include "nclclip.h"
#include "nclpsmult.h"

static ncl_polygon pol0,pol;

/* #define DBGFML */

/*********************************************************************
**    E_FUNCTION     : UU_KEY_ID S_create_spline1 (pts,np)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          uvlim  - Start and end u or v (depending on cvtyp).
**       OUTPUT :
**          uvlist - List of uv points.
**    RETURNS      : key
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID ncl_create_spline1 (pts,np)
UM_coord *pts;
int np;
{
	int i,status;
	struct NCL_crvgen_rec seg,*segp;
	struct UM_rbsplcrv_rec cv;
	UU_KEY_ID nclkey = NULLKEY;
	UU_LIST *seglst;

	ncl_get_fml_seglist (&seglst);
	UU_LIST_EMPTY(seglst);
	for (i = 0; i < np; i++)
	{
		ncl_init_seg (&seg);
		seg.x = pts[i][0];
		seg.y = pts[i][1];
		seg.z = pts[i][2];

		uu_list_push (seglst, &seg);
	}

	segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (seglst);

	status = ncl_interp_rbsp (np, segp, 1, &cv);
	
	if (cv.key != NULLKEY)
	{
		ncl_add_fml_delkey (cv.key);
		ur_update_displayable(cv.key, UM_NEVERDISPLAYABLE);
		if (status == UU_SUCCESS) nclkey = cv.key;
	}

	return (nclkey);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_create_wline (wline,lenlst,tol)
**          Create a curve by joining matching flowlines on adjacent surfaces.
**    PARAMETERS
**       INPUT  :
**          ptlist  - list of evolved points
**          comkys  - initialized list, used to hold intermediate keys
**          tol     - tolerance
**       OUTPUT : none
**    RETURNS      : the curve key
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_create_wline (wline,lenlst,tol)
NCL_wline *wline;
UU_LIST *lenlst;
UU_REAL tol;
{
	UU_REAL *len;

	int nc,i,status;
	UU_KEY_ID *sky;
	struct UM_compcrv_rec ccvp;
	UU_REAL tlen;

	nc = (wline->keylst).cur_cnt;

	if (nc <= 0) return;

	sky = (UU_KEY_ID *) UU_LIST_ARRAY (&wline->keylst);
	if (nc == 1)
	{
		wline->key = sky[0];
		return;
	}

	status = UU_SUCCESS;

	ccvp.key = NULLKEY;
	status = um_c5_mergecrv (nc,sky,&ccvp);

	if (lenlst != NULLST)
	{
		if (lenlst->cur_cnt == ccvp.no_cid)
		{
			len = (UU_REAL *) UU_LIST_ARRAY (lenlst);

			for (i = 0, tlen = 0.; i < ccvp.no_cid; i++)
			{
				tlen = tlen + len[i];
			}
			if (tlen > 5*tol)
			{
				ccvp.arclen = tlen;
				ccvp.cid[0].endparam = len[0]/tlen;
				for (i = 1; i < ccvp.no_cid; i++)
				{
					ccvp.cid[i].endparam = ccvp.cid[i-1].endparam + len[i]/tlen;
				}
			}
		}
	}

	if (status == UU_SUCCESS)
  		status = uc_create_mtuple_data (&ccvp,UM_DEFAULT_TF,UM_CURRENT_ATTR);

	if (ccvp.key != NULLKEY)
	{
		ncl_add_fml_delkey (ccvp.key);
		ur_update_displayable(ccvp.key, UM_NEVERDISPLAYABLE);
		if (status == UU_SUCCESS) wline->key = ccvp.key;
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_create_bsf (nkeys,wline,cvp,sfp)
**          Create a ruled spline surface by several curves.
**    PARAMETERS
**       INPUT  :
**          nkeys   - number of curves
**          spky    - curve keys
**          cvp     - array of structs to hold curves
**       OUTPUT :
**          sfp  - New surface
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_bsf (nkeys,wline,cvp,sfp)
NCL_wline wline[];
struct NCL_fixed_databag cvp[];
int nkeys;
struct UM_rbsplsrf_rec *sfp;
{
	UU_KEY_ID nclkey = NULLKEY;
	int i,ifit,status;

	if (nkeys < 2) return (-1);
	ifit = 0; /* maybe better to fit ?? */
	sfp->key = NULLKEY;
	
	for (i = 0; i < nkeys; i++)
	{
		cvp[i].key = wline[i].key;
		status = ncl_retrieve_data_fixed (&cvp[i]);
		if (status != UU_SUCCESS) return (status);
	}
/* sbsolv_insert = UU_TRUE; */
	status =  ncl_srfdef (ifit, nkeys, cvp, UU_NULL, sfp, UU_NULL);
/* sbsolv_insert = UU_FALSE; */
	
	nclkey = sfp->key;
	if (nclkey == NULLKEY)
		status = UU_FAILURE;
	else
	{
		ur_update_displayable(nclkey, UM_NEVERDISPLAYABLE);
		if (status != UU_SUCCESS)
			ncl_add_fml_delkey (nclkey);
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int S_init_bndr_proj (sfp,pt0,uu0,vv0,tolsq)
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
static int S_init_bndr_proj (sfp,pt0,uu0,vv0,tolsq)
struct UM_rbsplsrf_rec *sfp;
UM_coord pt0;
UU_REAL *uu0,*vv0,tolsq;
{
	struct UM_evsrfout evsrf;
	UU_REAL u,v,du,dv,d,dmin;
	int i,j,status;
	int NU = 10;
	int NV = 10;

	du = 1./NU;
	dv = 1./NV;

	status = UU_SUCCESS;

	for (i = 0, u = 0; i <= NU; i++,u += du)
	{
		for (j = 0, v = 0; j <= NV; j++, v += dv)
		{
			status = uc_evsrf (UM_POINT,u,v,sfp,UM_idmat,&evsrf);
			if (status != UU_SUCCESS) return (status);
			d = UM_SQDIS (pt0,evsrf.sp);
			if ((i == 0 && j == 0) || d < dmin)
			{
				dmin = d;
				*uu0 = u;
				*vv0 = v;
				if (dmin < tolsq) return (status);
			}
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : void S_move_from_cusp (w)
**       Project contour points onto a polygon boundary if already
**       within tolerance
*********************************************************************/		
static void S_move_from_cusp (w)
UU_REAL *w;
{
	UU_REAL W0,W1;

	W0 = 0.05;
	W1 = 0.95;

	if (*w < W0)
		*w = W0;
	else if (*w > W1)
		*w = W1;
}

/*********************************************************************
**    E_FUNCTION     : void S_pt_proj (asw,ppp,side,u4,v4,eps,lext)
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
static void S_pt_proj (itsk,asw,ppp,side,u4,v4,eps,lext)
int itsk;
UM_real8 asw;
UM_real4 *side,*u4,*v4;
UM_coord ppp;
UU_REAL eps;
UU_LOGICAL *lext;
{
	int k;
	UM_coord pti;
	UM_vector vci;
	UU_REAL dis;
	UU_REAL W0,W1,uui,vvi;

	UM_real4 ss[7];
	UM_int2 itype,isf;

	itype = NCLI_POINT;
	isf = 4;
	W0 = 0.05;
	W1 = 0.95;

	from_unibase (ppp,pti,&itype);
	uui = *u4; vvi = *v4;
	sfpt(&asw,pti,&isf,side,u4,v4,ss);
	for (k = 0; k < 3; k++)
		vci[k] = pti[k] - ss[k+4];
	dis = UM_DOT (vci,vci);
	if (itsk == 1 && dis > eps && (vvi < W0 || vvi > W1))
	{
		S_move_from_cusp (&vvi);
		*u4 = uui; *v4 = vvi;
		sfpt(&asw,pti,&isf,side,u4,v4,ss);
			
		for (k = 0; k < 3; k++)
			vci[k] = pti[k] - ss[k+4];
		dis = UM_DOT (vci,vci);
	}
	*lext = (dis > eps);
}

/*********************************************************************
**    E_FUNCTION     : void S_pt_interp (asw,ppo,ppi,ppn,side,ui,vi,eps)
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
static void S_pt_interp (asw,ppo,ppi,ppn,side,ui,vi,eps)
UM_real8 asw;
UM_real4 *side,*ui,*vi;
UM_coord ppo,ppi,ppn;
UU_REAL eps;
{
	UM_coord pmid;
	UU_REAL dd;
	UM_real4 um,vm;
	UU_LOGICAL lext;

	um_middlept (ppi,ppo,pmid);
	dd = UM_SQDIS (ppi,ppo);

	if (dd < 4.*eps)
	{
		um_vctovc (pmid,ppn);
		return;
	}
	um = *ui; vm = *vi;
	S_pt_proj (0,asw,pmid,side,&um,&vm,eps,&lext);
	if (lext)
	{
		S_pt_interp (asw,pmid,ppi,ppn,side,ui,vi,eps);
	}
	else
	{
		*ui = um; *vi = vm;
		S_pt_interp (asw,ppo,pmid,ppn,side,ui,vi,eps);
	}
}

/*********************************************************************
**    E_FUNCTION     : void S_pt_inter_proj (asw,pto,pti,side,u4,v4,eps,iret)
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
static void S_pt_inter_proj (asw,pto,pti,side,u4,v4,eps,iret)
UM_real8 asw;
UM_real4 *side,*u4,*v4;
UM_coord pto,pti;
UU_REAL eps;
int *iret;
{
	UM_coord ppo,ppi,ppn;
	UU_REAL dis;

	um_vctovc (pto,ppo);
	um_vctovc (pti,ppi);

	S_pt_interp (asw,ppo,ppi,ppn,side,u4,v4,eps);
	dis = UM_SQDIS (ppn,pti);
	if (dis < eps)
		*iret = 0;
	else
		*iret = 1;
}

/*********************************************************************
**    E_FUNCTION     : void S_bndr_proj (asw,side,npts,pts,uvlist,uu0,vv0,tol)
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
static void S_bndr_proj (asw,side,npts,pts,uvlist,uu0,vv0,tol)
UM_real8 asw;
UM_real4 *side;
UU_LIST *uvlist;
int npts;
UM_coord *pts;
UU_REAL uu0,vv0,tol;
{
	int i,iret;
	UU_REAL eps;
	UU_LOGICAL lext0,lext;
	UM_2Dcoord uv,uv0;
	UM_real4 u4,v4;

	u4 = uu0;
	v4 = vv0;
	uv[0] = uu0; uv[1] = vv0;
	eps = 9.*tol*tol;

	S_pt_proj (1,asw,pts[0],side,&u4,&v4,eps,&lext);
	uv[0] = u4; uv[1] = v4;
	uu_list_push (uvlist,uv);

	for (i = 1; i < npts; i++)
	{
		lext0 = lext;
		uv0[0] = uv[0]; uv0[1] = uv[1];
		u4 = uv[0]; v4 = uv[1];
		S_pt_proj (1,asw,pts[i],side,&u4,&v4,eps,&lext);
		uv[0] = u4; uv[1] = v4;
		if (lext0 != lext)
		{
			if (lext0)
			{
				S_pt_inter_proj (asw,pts[i-1],pts[i],side,&u4,&v4,eps,&iret);
			}
			else if (lext)
			{
				u4 = uv0[0]; v4 = uv0[1];
				S_pt_inter_proj (asw,pts[i],pts[i-1],side,&u4,&v4,eps,&iret);
			}
			if (iret == 1) /* point between pts[i-1] and pts[i] */
			{
				uv0[0] = u4; uv0[1] = v4;
				uu_list_push (uvlist,uv0);
			}
		}

		uu_list_push (uvlist,uv);
	}
}

/*********************************************************************
**    E_FUNCTION     : void S_fix_bndr_proj (uvlist)
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
static void S_fix_bndr_proj (uvlist)
UU_LIST *uvlist;
{
	int npts,in,i;
	UM_2Dcoord *vtx;
	UU_REAL yp,yn,y,Y0,Y1,eps;
				
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (uvlist);
	npts = uvlist->cur_cnt;
	Y0 = UM_FUZZ;
	Y1 = 1. - UM_FUZZ;
	eps = 0.005;

	for (i = 1; i < npts; i++)
	{
		yp = vtx[i-1][1];
		in = (i+1)%npts;
		yn = vtx[in][1];
		y = vtx[i][1];

		if ((yp < Y0 && yn < Y0 && y > Y0 && y < eps) ||
			(yp > Y1 && yn > Y1 && y < Y1 && y > 1 - eps))
		{
			vtx[i][1] = (yp + yn)/2.;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : int S_close_gaps (pp,npts,cpol,ctolsq)
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
static int S_close_gaps (pp,npts,cpol,ctolsq)
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
**    E_FUNCTION: void S_fix_final (sfp,tol,tolsq)
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
static void S_fix_final (sfp,tol,tolsq)
struct UM_rbsplsrf_rec *sfp;
UU_REAL tol,tolsq;
{
	int nv,nv1,i,i1,i2;
	int status;
	UM_2Dcoord *pp,vv0,vv1;
	UU_LOGICAL closed,cut,cut1;
	UU_REAL cso,ds,ds0,ds1,ds2,u,v,dd,dd0,dd1,cco;
	UU_REAL ttol = 2.5e-7;
	struct UM_evsrfout evsrf;
	UM_coord *pt;
	UM_vector vc0,vc1,vc2;
	UU_LIST tmplst;

	nv = pol0.np[0];
	pp = (UM_2Dcoord *) UU_LIST_ARRAY (pol0.contour);

	closed = (UM_SQDIS_2D(pp[0],pp[nv-1]) <= ttol);
	if (closed)
		nv1 = nv-1;
	else
		nv1 = nv;

	uu_list_init (&tmplst,sizeof(UM_coord),nv,nv);

	for (i = 0; i < nv1; i++)
	{
		u = pp[i][0]; v = pp[i][1];
		status = uc_evsrf (UM_POINT,u,v,sfp,UM_idmat,&evsrf);
		if (status != UU_SUCCESS) goto Fin;
		uu_list_push (&tmplst,evsrf.sp);
	}
	pt = (UM_coord *) UU_LIST_ARRAY (&tmplst);

/*
..... find first nonzero segment
*/
	for (i = 0, i1 = 1; i1 < 2 && nv1 > 2; i1++)
	{
		um_vcmnvc_2d(pp[i1],pp[0],vv0);
		dd0 = UM_DOT_2D (vv0,vv0);
		um_vcmnvc(pt[i1],pt[0],vc0);
		ds0 = UM_DOT (vc0,vc0);
		if (dd0 <= ttol || ds0 < tolsq)
		{
			uu_list_delete (pol0.contour,i1,1);
			pp = (UM_2Dcoord *) UU_LIST_ARRAY (pol0.contour);
			uu_list_delete (&tmplst,i1,1);
			pt = (UM_coord *) UU_LIST_ARRAY (&tmplst);
			i1--; nv--; nv1--;
		}
	}

	for (i = 1; i < nv1; i++)
	{
		i1 = (i+1)%nv1;
		cut = cut1 = UU_FALSE;
		um_vcmnvc_2d(pp[i1],pp[i],vv1);
		dd1 = UM_DOT_2D (vv1,vv1);
		um_vcmnvc(pt[i1],pt[i],vc1);
		ds1 = UM_DOT (vc1,vc1);
		cco = UM_DOT_2D (vv0,vv1);
		if (dd1 <= ttol || ds1 < tolsq)
		{
			if (i == nv-1)
			{
				closed = UU_TRUE;
				um_vctovc_2d(pp[0],pp[nv-1]);
				break;
			}
			else
			{
/*
..... decide if it would be better to cut i
*/
				i2 = (i+2)%nv1;
				um_vcmnvc(pt[i+2],pt[i+1],vc2);
				ds2 = UM_DOT (vc2,vc2);
				cut = (ds2 > ds0);
				cut1 = !cut;
			}
		}
		else if (cco < 0.)
		{
			if (dd0 <= dd1)
			{
				dd = dd0 - (cco*cco)/dd1;
				cut = (dd < 0.5*dd0 && dd < ttol);
			}
			else
			{
				dd = dd1 - (cco*cco)/dd0;
				cut = (dd < 0.5*dd1 && dd < ttol);
			}

			if (!cut)
			{
				cso = UM_DOT (vc0,vc1);
				if (ds0 <= ds1)
				{
					ds = ds0 - (cso*cso)/ds1;
				}
				else
				{
					ds = ds1 - (cso*cso)/ds0;
				}
				cut = (ds < tolsq);
			}

			if (cut)
			{
				if (i < nv1-1 && ds0 > 2*ds1)
				{
/*
..... decide if it would be better to cut i1
*/
					i2 = (i+2)%nv1;
					um_vcmnvc(pt[i+2],pt[i+1],vc2);
					ds2 = UM_DOT (vc2,vc2);
					if (ds2 > tolsq && ds0 > 2*ds2)
					{
						cso = UM_DOT (vc1,vc2);
						if (cso < 0.)
						{
							if (ds1 <= ds2)
							{
								ds = ds1 - (cso*cso)/ds2;
							}
							else
							{
								ds = ds2 - (cso*cso)/ds1;
							}
							cut1 = (ds < tolsq);
							cut = !cut1;
						}
					}
				}
			}
		}

		if (cut1)
		{
			uu_list_delete (pol0.contour,i1,1);
			pp = (UM_2Dcoord *) UU_LIST_ARRAY (pol0.contour);
			uu_list_delete (&tmplst,i1,1);
			pt = (UM_coord *) UU_LIST_ARRAY (&tmplst);
			i--; nv--; nv1--;
		}
		else if (cut)
		{
			uu_list_delete (pol0.contour,i,1);
			pp = (UM_2Dcoord *) UU_LIST_ARRAY (pol0.contour);
			uu_list_delete (&tmplst,i,1);
			pt = (UM_coord *) UU_LIST_ARRAY (&tmplst);

			i--; nv--; nv1--;
			um_vcmnvc_2d(pp[i],pp[i-1],vv0);
			dd0 = UM_DOT_2D (vv0,vv0);
			um_vcmnvc(pt[i],pt[i-1],vc0);
			ds0 = UM_DOT (vc0,vc0);
			while ((dd0 <= ttol || ds0 < tolsq) && nv1 > 2)
			{
				uu_list_delete (pol0.contour,i,1);
				pp = (UM_2Dcoord *) UU_LIST_ARRAY (pol0.contour);
				uu_list_delete (&tmplst,i,1);
				pt = (UM_coord *) UU_LIST_ARRAY (&tmplst);

				um_vcmnvc_2d(pp[i],pp[i-1],vv0);
				dd0 = UM_DOT_2D (vv0,vv0);
				um_vcmnvc(pt[i],pt[i-1],vc0);
				ds0 = UM_DOT (vc0,vc0);
			}
			if (nv1 < 2) break;
			if (i > 0) i--;
		}
		else
		{
			um_vctovc(vc1,vc0); ds0 = ds1;
			um_vctovc_2d(vv1,vv0); dd0 = dd1;
		}
	}

Fin:
	uu_list_free (&tmplst);
	pol0.np[0] = nv;
	return;
}

/*********************************************************************
**    E_FUNCTION     : int S_common_bndr (asw,side,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          uvlim  - Start and end u or v (depending on cvtyp).
**       OUTPUT :
**          uvlist - List of uv points.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_common_bndr (sfp,bndr,ncut,icut,asw,side,tol)
struct UM_rbsplsrf_rec *sfp;
UM_srf_boundary *bndr;
int ncut,*icut;
UM_real8 asw;
UM_real4 *side;
UU_REAL tol;
{
	int i,isf,npts,nc;
	UU_LIST *vtxlist;
	UM_2Dcoord *vtx0,*vtx;
	UM_coord *pts;
	UU_LOGICAL allin;
	UU_LOGICAL firstime = UU_TRUE;
	UU_REAL tolsq = tol*tol;
	UU_REAL uu0,vv0,eps;
	int status = UU_FAILURE;

	pol0.num_contours = pol.num_contours = 1;
	pol0.np = (int *) uu_malloc (sizeof(int));
	pol.np = (int *) uu_malloc (sizeof(int));

	eps = 9.*tolsq;
	for (i = 0; i < ncut; i++)
	{
		isf = icut[i];

		npts = bndr[isf].np[0] - 1;
		pts = (UM_coord *) UU_LIST_ARRAY (bndr[isf].cvpts);
		vtxlist = (firstime)? pol0.contour: pol.contour;
		UU_LIST_EMPTY(vtxlist);

		S_init_bndr_proj (sfp,pts[0],&uu0,&vv0,tolsq);
		S_bndr_proj (asw,side,npts,pts,vtxlist,uu0,vv0,tol);

		S_fix_bndr_proj (vtxlist);

		npts = vtxlist->cur_cnt;
		if (firstime)
		{
/*
..... create first nontrivial horizontal projection polygon
*/
			pol0.np[0] = npts;
			firstime = UU_FALSE;
		}
		else if (npts >= 3)
		{
/*
..... add other (horizontal projection) polygons one-by-one by forming a union
*/

			pol.np[0] = npts;
			vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol.contour);

			S_close_gaps (vtx,npts,&pol0,eps);

			status = ncl_polygon_clip (NCL_CONTOUR,&pol0,&pol,&pol0,tol,tolsq);

			if (status != UU_SUCCESS)
			{
				npts = pol0.np[0];
				if (pol0.num_contours == 1 && npts >= 4)
				{
					vtx0 = (UM_2Dcoord *) UU_LIST_ARRAY (pol0.contour);
					vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol.contour);
					allin = UU_TRUE;
					for (i = 0; i < pol.np[0] && allin; i++)
					{
						allin = (um_check_inside(vtx0,npts,vtx[i],UU_NULL,tol) >= 0);
					}
					if (allin) status = UU_SUCCESS;
				}
			}
			if (status != UU_SUCCESS) return (status);

			ncl_prepare_for_pocketing (&pol0,tol,tolsq);
			
		}
	}

	S_fix_final (sfp,tol,tolsq);

	nc = pol0.num_contours;
	npts = pol0.np[0];

	if (nc < 1 || npts < 3)
		status = UU_FAILURE;
	else if (nc > 1)
	{
		pol0.num_contours = 1;
		pol0.contour->cur_cnt = npts;
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int S_inner_bndr (asw,side,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          uvlim  - Start and end u or v (depending on cvtyp).
**       OUTPUT :
**          uvlist - List of uv points.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_inner_bndr (sfp,bndr,ncut,icut,asw,side,tol)
struct UM_rbsplsrf_rec *sfp;
UM_srf_boundary *bndr;
int ncut,*icut;
UM_real8 asw;
UM_real4 *side;
UU_REAL tol;
{
	int i,isf,status,npts,ib,nb,nv;
	int typ = 2;
	UU_LIST *vtxlist;
	UM_2Dcoord *vtx;
	UM_coord *pts;
	UU_REAL uu0,vv0;
	UU_REAL tolsq = tol*tol;

	vtxlist = pol.contour;
	status = UU_SUCCESS;

	for (i = 0; i < ncut; i++)
	{
		isf = icut[i];
		nb = bndr[isf].nb;
		if (nb > 1)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (bndr[isf].cvpts);
			npts = bndr[isf].np[0];
			for (ib = 1; ib < nb; ib++)
			{
				pts += npts;
				npts = bndr[isf].np[ib];
				UU_LIST_EMPTY(vtxlist);
				S_init_bndr_proj (sfp,pts[0],&uu0,&vv0,tolsq);
				S_bndr_proj (asw,side,npts,pts,vtxlist,uu0,vv0,tol);

				nv = vtxlist->cur_cnt;
				if (nv >= 3)
				{
					vtx = (UM_2Dcoord *) UU_LIST_ARRAY (vtxlist);
					ncl_addto_pol0 (vtx,nv,&pol0,typ);
				}
			}
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int S_new_trim1 (sfp,pol,nkey,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          uvlim  - Start and end u or v (depending on cvtyp).
**       OUTPUT :
**          uvlist - List of uv points.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_new_trim1 (sfp,nkey,tol)
struct UM_rbsplsrf_rec *sfp;
UU_KEY_ID *nkey;
UU_REAL tol;
{
	int ncv,status,j,k77;
	UU_KEY_ID skey,lkey,keyj;
	UU_KEY_ID *ibky = UU_NULL;
	struct NCL_trimsf_rec surf;
	struct UM_transf_rec btran;
	UM_int2 lfl_77;

	skey = sfp->key;
	k77 = 0;

	ur_setup_data(NCL_TRIMSF_REL, &surf, sizeof(surf));
	ncl_trmsf_init (&surf);

	/* set ranfile label flag to temp unknown */
	lfl_77 = 1;
	stunlb (&lfl_77);
	k77 = 1;
	
	surf.bs_key = skey;

	status = ncl_pol_bndry (skey,&pol0,UU_NULL,&lkey,&ibky,&ncv);

	if (lkey == NULLKEY)
		status = UU_FAILURE;
	else
	{
		ur_update_displayable(lkey, UM_NEVERDISPLAYABLE);
		if (status != UU_SUCCESS)
			ncl_add_fml_delkey (lkey);
	}

	if (ibky != UU_NULL)
	{
		for (j = 0; j < ncv; j++)
		{
			keyj = ibky[2*j+1];
			if (keyj != NULLKEY)
			{
				ur_update_displayable(keyj, UM_NEVERDISPLAYABLE);
				if (status != UU_SUCCESS)
					ncl_add_fml_delkey (keyj);
			}
		}
	}

	if (status != UU_SUCCESS) goto Done;
	
#ifdef DBGFML
	if (k77 == 1)
	{
		stunlb (&lfl_77);
		k77 = 0;
	}
	status = ncl_label_wf(NCL_TRIMSF_REL,surf.label,&surf.subscr,surf.key,
				&nclstatus);
#endif

	surf.uv_key = lkey;
	status = ncl_create_entity(&surf, 9);
	if (status == UU_SUCCESS)
	{
/*
.....Get the base surface transformation 
*/
		btran.key = skey;
		btran.rel_num = UM_TRANSFORM_REL;
		if (ur_retrieve_transf(&btran) != 0) goto Err;
		btran.key = surf.key;

		if (ur_update_transf (&btran) != 0) goto Err;

		if (ibky != UU_NULL)
		{
			status = ur_update_data_varlist (surf.key, 1, ibky, 1, 2*ncv);
			UU_FREE (ibky);
		}
	}

	if (status == UU_SUCCESS)
	{
		ncl_set_fml_bskey (surf.bs_key);
#ifdef DBGFML
		ncl_store_wf1 (surf.key);

		ncl_def_color (surf.key);
		uc_display (&surf);
#endif
		ncl_set_fml_nkey (surf.key);
		*nkey = surf.key;

		goto Done;
	}

Err:
	status = UU_FAILURE;
Done:
	if (k77 == 1) stunlb (&lfl_77);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_new_trim (sfp,nkey,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          uvlim  - Start and end u or v (depending on cvtyp).
**       OUTPUT :
**          uvlist - List of uv points.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_new_trim (sfp,bndr,ncut,icut,nkey,tol)
struct UM_rbsplsrf_rec *sfp;
UM_srf_boundary *bndr;
int ncut,*icut;
UU_KEY_ID *nkey;
UU_REAL tol;
{
	int n,status;
	UM_real8 asw;
	UM_real4 side;
	UM_int2 isub,itype;

	n = bndr[0].uvpts->cur_cnt;
	if (n < 100) n = 100;
	ncl_init_polygon (&pol0,n);
	ncl_init_polygon (&pol,n);

	side = 0.0; /* Initializes sfpt function. */
	itype = 9; /* surface */
	isub = 1;
	ptdsc3(&sfp->key,&isub,&itype,&asw);

	status = S_common_bndr (sfp,bndr,ncut,icut,asw,&side,tol);
	if (status == UU_SUCCESS)
		status = S_inner_bndr (sfp,bndr,ncut,icut,asw,&side,tol);
	if (status == UU_SUCCESS)
		status = S_new_trim1 (sfp,nkey,tol);

	ncl_free_polygon (&pol0);
	ncl_free_polygon (&pol);

	return (status);
}
