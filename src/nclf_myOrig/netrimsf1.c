/*********************************************************************
**    NAME         :  netrimsf1.c
**       CONTAINS: Fortran interface, display, and evaluator routines
**                 for trimmed surfaces which are:
**
**           ncl_trimsf_ukey
**           ncl_trimsf_cvkey
**           ncl_create_trimsf
**           ncl_disp_trimsf
**           ncl_surf_disp
**           ncl_dspsrf_evolve1
**           ncl_display_edge
**           ncl_moveto
**           ncl_lineto
**           ncl_weed_trim_bndr
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       netrimsf1.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       10/19/15 , 09:55:42
*********************************************************************/
#include "mcrv.h"
#include "nccs.h"
#include "mdattr.h"
#include "nclfc.h"
#include "mdeval.h"
#include "mgeom.h"
#include "class.h"  /* for UC_attributedatabag */
#include "uminmax.h"
#include "gsegac.h"

typedef struct _NCL_surfattr 
{
	int nu;						/* number of u paths */
	int nv;						/* number of v paths */
	int nup;					/* number of points per u paths */
	int nvp;					/* number of points per v paths */
	int sh_mode;
	int lnstyle;
	int color;
	int lstix;
} NCL_surfattr;

extern UU_LOGICAL UM_set_constituent_pickids;

static struct UM_evsrfout Sevsrf;
static struct UM_evcrvout Sevcrv;

/*********************************************************************
**    E_FUNCTION     : int ncl_trimsf_ukey (ckey,skey,uv,ukey,jerr)
**       Creates a UV-outer boundary curve
**    PARAMETERS
**       INPUT  :
**          skey      - Key of base surface.
**          ckey      - Key of outer boundary curve
**          uv        - Starting surface u,v
**       OUTPUT :
**          ukey    - Key of trimmed surface.
**          jerr    - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_trimsf_uvkey (ckey,skey,uv,ukey,jerr)
UU_KEY_ID skey,ckey,*ukey;
UM_real4 uv[2];
UM_int2 *jerr;
{
	int i,n,rel_num,addflg;
	int status = UU_SUCCESS;
	struct NCL_fixed_databag s1,s2,s3;
	UM_int2 kerr, itsk;
	UU_KEY_ID kss,lkey,*keyp = UU_NULL;
	struct UM_compcrv_rec *ccvp;
	struct UM_uvcvonsf_rec *cuvp;
	struct UM_cid_rec *ccidp,*cids;
	UU_REAL t0,t1;
	UM_transf tfmat;
	struct UC_entitydatabag e;

	*jerr = 1;
	itsk = 22;
	status = ur_retrieve_data_relnum(ckey,&rel_num);
	if (status != UU_SUCCESS) return (status);
	if (rel_num == UM_COMPCRV_REL)
	{
		s1.key = ckey;
		ccvp = (struct UM_compcrv_rec *)&s1;
		cuvp = (struct UM_uvcvonsf_rec *)&s2;
		status = ncl_retrieve_data_fixed(ccvp);
		ccidp = ccvp->cid;
		n = ccvp->no_cid;
		t0 = ccvp->t0;
		t1 = ccvp->t1;
		addflg = ccvp->addflg;
		keyp = UU_NULL;
		keyp = (UU_KEY_ID *)uu_malloc(n*sizeof(UU_KEY_ID));
		if (keyp == UU_NULL) status = UU_FAILURE;
		for (i=0; i<n && status==UU_SUCCESS; i++)
			keyp[i] = 0;
		for (i=0;i<n && status == UU_SUCCESS;i++)
		{
			if (ccidp->endparam < t0)
			{
				ccidp++;
				continue;
			}
			e.key = lkey = ccidp->crvid;
			uc_retrieve_transf(lkey, tfmat);
			uc_retrieve_data(&e, sizeof(e));
			um_c5_trimpart(&e,ccvp->cid,ccvp->no_cid,t0,t1,i,tfmat);
			status = ncl_create_sspline(itsk,skey,lkey,uv,&kss,&kerr);
			ur_update_data_fixed(&e);
			if (kerr==-1) *jerr = -1;
			if (status == UU_SUCCESS)
			{
				cuvp->key = kss;
				status = ncl_retrieve_data_fixed(cuvp);
			}
			if (status == UU_SUCCESS)
			{
				s3.key = 0;
				status = ncl_create_rbsp (cuvp->no_pt,cuvp->t,cuvp->pt,&s3);
				keyp[i] = s3.key;
			}
			if (kss > 0) uc_delete(kss);
			if (ccidp->endparam > t1) break;
			ccidp++;
		}
		if (status == UU_SUCCESS)
		{
			ccvp->key = 0;
			status = um_c5_mergecrv (n, keyp, ccvp);
			cids = ccvp->cid;
		}
		if (status == UU_SUCCESS)
		{
			status = uc_create_mtuple_data (ccvp,UM_DEFAULT_TF,UM_CURRENT_ATTR);
//			ccvp->t0 = t0;
//			ccvp->t1 = t1;
//			ccvp->addflg = addflg;
//			ur_update_data_fixed(ccvp);
		}
		if (status == UU_SUCCESS)
		{
			*ukey = ccvp->key;
			status = ncl_retrieve_data_fixed(ccvp);
		}
		if (keyp)
		{
			for (i=0;i<n;i++)
				if (keyp[i]) uc_delete(keyp[i]);
			uu_free(keyp);
			keyp = UU_NULL;
		}
	}
	else
	{
		status = ncl_create_sspline(itsk,skey,ckey,uv,&kss,jerr);
		if (status == UU_SUCCESS)
		{
			s1.key = kss;
			status = ncl_retrieve_data_fixed(&s1);
		}
		if (status == UU_SUCCESS)
		{
			cuvp = (struct UM_uvcvonsf_rec *)&s1;
			s3.key = 0;
			status = ncl_create_rbsp (cuvp->no_pt,cuvp->t,cuvp->pt,&s3);
			*ukey = s3.key;
		}
		if (kss > 0)
		status = uc_delete(kss);
	}

	if (status == UU_SUCCESS)
		ur_update_displayable(*ukey, UM_NEVERDISPLAYABLE);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_trimsf_cvkey (ckey,skey,uv,ukey,jerr)
**       Creates a XYZ-outer boundary curve
**    PARAMETERS
**       INPUT  :
**          skey      - Key of base surface.
**          ckey      - Key of outer boundary curve
**          uv        - Starting surface u,v
**       OUTPUT :
**          ukey     - Key of trimmed surface.
**          jerr     - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_trimsf_cvkey (ckey,skey,uv,lkey,jerr)
UU_KEY_ID skey,ckey,*lkey;
UM_real4 uv[2];
UM_int2 jerr;
{
	int status = UU_SUCCESS,ierr;
	struct NCL_fixed_databag s1,s2;
	UM_int2 itsk;
/*
.....Initialize routine
*/
	ierr = jerr;
/*
.....Input XYZ curve is already on surface
.....Just copy it
*/
	if (ierr == 1)
	{
		s1.key = ckey;
		s2.key = 0;
		status = ncl_retrieve_data_fixed (&s1);
		if (status == UU_SUCCESS)
		{
/*
........If input curve is not a 3-D spline
........Then need to create new curve
*/
			if (s1.rel_num == UM_RBSPLCRV_REL)
			{
				status = uc_copy (&s1, &s2, sizeof(struct NCL_fixed_databag));
				*lkey = s2.key;
			}
			else ierr = 0;
		}
	}
/*
.....Create new XYZ curve
*/
	if (ierr != 1)
	{
		itsk = 1;
		status = ncl_create_sspline(itsk,skey,ckey,uv,lkey,&jerr);
	}
	if (status == UU_SUCCESS)
		ur_update_displayable(*lkey, UM_NEVERDISPLAYABLE);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_create_trimsf (eptr,skey,ust,vst,ckey,ukey))
**       Creates a UV-outer boundary curve and writes the key into a trimmed
**       surface structure.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_trimsf (eptr,skey,ust,vst,ckey,ukey)
struct NCL_trimsf_rec *eptr;
UU_KEY_ID skey,ckey,*ukey;
UU_REAL ust,vst;
{
	UM_real4 uv[2];
	UM_int2 lfl_77, jerr;
	int status = 0;

	uv[0] = ust; uv[1] = vst;

	lfl_77 = 1;
	stunlb (&lfl_77);

	status = ncl_trimsf_uvkey (ckey,skey,uv,ukey,&jerr);

	stunlb (&lfl_77);

	if (ukey == NULLKEY) status = -1;

	if (status == 0)
	{
		eptr->rel_num = NCL_TRIMSF_REL;
		ncl_trmsf_init (eptr);
		eptr->bs_key = skey;
		eptr->uv_key = *ukey;
	}

	return (status);
}

/*********************************************************************
*********************************************************************/
static void ncl_moveto (dpt)
UM_coord dpt;
{
	gmova3 (&dpt[0],&dpt[1],&dpt[2]);
}

/*********************************************************************
*********************************************************************/
static void ncl_lineto (dpt)
UM_coord dpt;
{
	glina3 (&dpt[0],&dpt[1],&dpt[2]);
}

/*********************************************************************
**    E_FUNCTION: ncl_dspsrf_evolve1 (eptr,tfmat,uv,wr,cvtyp,nw,ptlst,uvlst)
**       Evolve isoparametric surface curve to 'nu' points polyline
**       with evenly spread variable parameter.
**       This is a rewrite of ncl_evolve1_crv_on_srf, the differences are:
**       (a) the range wr is arbitrary; (b) UV parameters are stored.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to surface record
**          tfmat  - ID matrix of input surface
**          uv     - constant parameter
**          wr     - variable parameter limits
**          nw     - number of points to evolve
**          cvtyp  - 1 = u_curve, 2 = v_curve.
**       OUTPUT :
**          ptlst   - pointer to points list
**          uvlst   - pointer to u/v parameter of V/U line
**    RETURNS      :
**          Number of points stored in list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_dspsrf_evolve1 (eptr,tfmat,uv,wr,cvtyp,nw,ptlst,uvlst)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UU_REAL uv,*wr;
int nw,cvtyp;
UU_LIST *ptlst,*uvlst;
{
	UU_REAL dw,w;
	UM_coord uvpt;
	int i,ix,status;

	ix = cvtyp - 1;
	uvpt[1-ix] = uv;
	uvpt[2] = 0;

	dw = (wr[1] - wr[0])/(nw - 1);

	for (i = 0, w = wr[0]; i < nw; i++, w+=dw)
	{
		if (w > wr[1]) w = wr[1];
		uvpt[ix] = w;
		status = ncl_evsrf_tf (UM_POINT, uvpt[0], uvpt[1], eptr, tfmat, &Sevsrf);

		if (status == UU_SUCCESS)
		{
			uu_list_push (ptlst,Sevsrf.sp);
			uu_list_push (uvlst,uvpt);
		}
		else
			return (-1);
	}

	return (nw);
}

/*********************************************************************
**    E_FUNCTION: ncl_dspsrf_evolve (eptr,tfmat,uv,wr,cvtyp,nw,tol,ptlst,uvlst)
**       A special way to evolve an isoparametric surface curve.
**       The range is evenly divided in nw pieces; within each piece the curve
**       is evolved by tolerance.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to surface record
**          tfmat  - ID matrix of input surface
**          uv     - constant parameter
**          wr     - variable parameter limits
**          nw     - number of points to evolve
**          tol    - tolerance
**          cvtyp  - 1 = u_curve, 2 = v_curve.
**       OUTPUT :
**          ptlst   - pointer to points list
**          uvlst   - pointer to u/v parameter of V/U line
**    RETURNS      :
**          Number of points stored in list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_dspsrf_evolve (eptr,tfmat,uv,wr,cvtyp,nw,tol,ptlst,uvlst)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UU_REAL uv,*wr,tol;
int nw,cvtyp;
UU_LIST *ptlst,*uvlst;
{
	UU_REAL dw,w,w1,wnext;
	UM_coord uvpt;
	int i,ix,status,np,evflg;

	ix = cvtyp - 1;
	uvpt[1-ix] = uv;
	uvpt[2] = 0;

	dw = (wr[1] - wr[0])/nw;

	w = wr[0];
	uvpt[ix] = w;

	evflg = UM_ALL;
	status = ncl_evsrf_tf (evflg, uvpt[0], uvpt[1], eptr, tfmat, &Sevsrf);

	if (status == UU_SUCCESS)
	{
		uu_list_push (ptlst,Sevsrf.sp);
		if (uvlst != NULLST) uu_list_push (uvlst,uvpt);
		np = 1;
	}
	else
		return (-1);

	for (i = 0; i < nw; i++)
	{
		w1 = w + dw;
		if (w1 > wr[1]) w1 = wr[1];
		while (w < w1 - UM_FUZZ)
		{
			ncl_evsrf_to_evcrv (cvtyp,&Sevsrf,&Sevcrv);
/*
..... the flag 1 in the call below means there is no preset max value for
..... (wn - w)
*/
			if (status == UU_SUCCESS)
			status = ncl_srf_nextpt_at_tol (eptr,tfmat,tol,uv,w,cvtyp,&wnext,w1,&Sevcrv,1);
			if (status != UU_SUCCESS) return (-1);

			if (wnext > w1) wnext = w1;
			if (wnext - w < UM_FUZZ) break;

			w = wnext;
			uvpt[ix] = w;
			if (w > wr[1]- UM_FUZZ) evflg = UM_POINT;
			status = ncl_evsrf_tf (evflg, uvpt[0], uvpt[1], eptr, tfmat, &Sevsrf);

			if (status == UU_SUCCESS)
			{
				uu_list_push (ptlst,Sevsrf.sp);
				if (uvlst != NULLST) uu_list_push (uvlst,uvpt);
				np++;
			}
			else
				return (-1);
		}
	}

	return (np);
}

/*********************************************************************
**    I_FUNCTION     : int S_display_uvline (bs,tfmat,bndr,uv,cvtyp,wr,nwp,tol,
**                                           cvpts,cvus,iobuf)
**       Display a single u- or v-line on a trimmed surface.
**    PARAMETERS
**       INPUT  :
**          bs         - ptr to the base surface
**          tfmat      - transformation
**          bndr       - boundary struct
**          uv         - constant parameter
**          cvtyp      - 1 = u_curve, 2 = v_curve.
**          wr         - variable parameter limits
**          nwp        - number of points to evolve
**          nw0        - number of v- or u- transverse grid lines
**          tol        - tolerance
**          cvpts      - list to use for 3D points
**          cvus       - list to use for 2D points
**          iobuf      - list to use for 2D boundary intersections
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_display_uvline (bs,tfmat,bndr,uv,cvtyp,wr,nwp,nw0,tol,cvpts,cvus,iobuf)
struct NCL_fixed_databag *bs;
UM_transf tfmat;
UM_srf_boundary *bndr;
UU_LIST *iobuf,*cvus,*cvpts;
int cvtyp,nwp,nw0;
UU_REAL uv,wr[],tol;
{
	int status,ldsp,js,je,nw1,ns,ix,iy,j,k;
	UU_REAL w,wh,xy[2];
	UM_coord *bnpt,*tp;
	UM_2Dcoord *uvpd;
	UM_param prev, cur;

	ix = cvtyp - 1; iy = 1 - ix;

	if (nwp > 1)
	{
		nw1 = ncl_dspsrf_evolve1 (bs,tfmat,uv,wr,cvtyp,nwp,cvpts,cvus);
	}
	else
	{
		if (nw0 > 1)
		nw1 = ncl_dspsrf_evolve(bs,tfmat,uv,wr,cvtyp,nw0,tol,cvpts,cvus);
		else
		nw1 = ncl_evolve_crv_on_srf(bs,tfmat,uv,wr,cvtyp,tol,cvpts,UU_NULL,
				cvus);
	}

	tp = (UM_coord *) UU_LIST_ARRAY (cvus);
	bnpt = (UM_coord *) UU_LIST_ARRAY (cvpts);
	for (js = 0; js < nw1 && tp[js][ix] < wr[0]; js++);
	for (je = nw1-1; je > js && tp[je][ix] > wr[1]; je--);
	if (je != nw1-1) je++;

	status = UU_SUCCESS;
	ldsp = 0;
/*
......intersect u line with all boundary curves
*/
	ns = um_uv_isect_bndry (uv,cvtyp,bndr,iobuf);
	if (ns < 2) return (status);
	uvpd = (UM_2Dcoord *) UU_LIST_ARRAY (iobuf);
	wh = uvpd[0][ix];
	if (wh < wr[0]) wh = wr[0];
	prev = wh;
/*
..... Eduard 3/10/99 : If a boundary curve is composite, some
..... intersection points may come with multiplicities. Here we remove
..... such multiple points, so that u-lines are displayed correctly.
*/
	k = 1;
	while (k < ns)
	{
		cur = uvpd[k][ix];
		if (fabs(cur - prev) < 0.5 * UM_DFUZZ)
		{
			ns--;
			if (k == ns) break;
			for (j=k; j<ns; j++) uvpd[j][ix]=uvpd[j+1][ix];
		}
		else
		{
			k++;
			prev = cur;
		}
	}

	j = js;
	k = 0;
/*
..... draw u line inside the boundary starting from
..... intersection points
*/
	while (k < ns && j < je+1)
	{
		w = tp[j][ix];
		if (w > wr[1]) w = wr[1];
		if (wh <= w)
		{
			xy[ix] = wh;
			xy[iy] = uvpd[k][iy];
			status = ncl_evsrf_tf(UM_POINT,xy[0],xy[1],bs,tfmat,&Sevsrf);
			if (status != UU_SUCCESS) break;
			if (ldsp)
				ncl_lineto (Sevsrf.sp);
			else
				ncl_moveto (Sevsrf.sp);
			ldsp = 1 - ldsp;

			if (++k < ns)
			{
				wh = uvpd[k][ix];
				if (wh > wr[1]) wh = wr[1];
			}
		}
		else
/*
......inside the boundary draw u line using points
......generated by evolving routine
*/
		{
			if (ldsp)
			ncl_lineto (bnpt[j]);
			j++;
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : void S_display_uvline0 (eptr,tfmat,cvpts,uv,cvtyp,wr,
**                                             nwp,nw0,tol)
**       Display a single u- or v-line on an untrimmed surface.
**    PARAMETERS
**       INPUT  :
**          eptr       - ptr to surface
**          tfmat      - transformation
**          uv         - constant parameter
**          cvtyp      - 1 = u_curve, 2 = v_curve.
**          wr         - variable parameter limits (always [0,1])
**          nwp        - number of points to evolve
**          nw0        - number of v- or u- transverse grid lines
**          tol        - tolerance
**          cvpts      - list to use for 3D points
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_display_uvline0 (eptr,tfmat,cvpts,uv,cvtyp,wr,nwp,nw0,tol)
int cvtyp,nwp,nw0;
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UU_LIST *cvpts;
UU_REAL uv,wr[],tol;
{
	UM_coord *pts;
	int j,n;

	if (nwp > 1)
	{
		n = ncl_evolve1_crv_on_srf (eptr,tfmat,uv,cvtyp,nwp,cvpts);
	}
	else
	{
		if (nw0 > 1)
		n = ncl_dspsrf_evolve (eptr,tfmat,uv,wr,cvtyp,nw0,tol,cvpts,NULLST);
		else
		n = ncl_evolve_crv_on_srf (eptr,tfmat,uv,wr,cvtyp,tol,cvpts,NULLST,NULLST);
	}

	pts = (UM_coord *) UU_LIST_ARRAY (cvpts);
	for (j = 0; j < n; j++)
	{
		ncl_lineto (pts[j]);
	}
}

/*********************************************************************
**    I_FUNCTION     : int S_disp_uvlines (ltrimd,eptr,tfmat,bndr,sfattr,tol)
**       Display uv-lines on a trimmed surface.
**    PARAMETERS
**       INPUT  :
**          ltrimd     - TRUE if trimmed surface, else FALSE
**          eptr       - ptr to surface
**          tfmat      - transformation
**          bndr       - boundary struct
**          sfattr     - surface display attributes
**          tol        - display tolerance
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_disp_uvlines (ltrimd,eptr,tfmat,bndr,sfattr,tol)
UU_LOGICAL ltrimd;
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UM_srf_boundary *bndr;
NCL_surfattr *sfattr;
UU_REAL tol;
{
	struct NCL_fixed_databag bs;
	struct NCL_trimsf_rec *tsf;
	int status,i,nu,nv,nup,nvp,nw0;
	UU_REAL umax,umin,vmax,vmin,uvmin,uvmax;
	UU_REAL wr[2];
	UU_REAL uv,duv;
	int nwp,nuv,cvtyp;
	UU_LIST iobuf,cvus,*cvpts;

	status = UU_SUCCESS;

	if (ltrimd)
	{
		if (ltrimd) tsf = (struct NCL_trimsf_rec *)eptr;
		bs.key = tsf->bs_key;
		status = ncl_retrieve_data_fixed (&bs);
		if (status != UU_SUCCESS) return (status);
	}

	umin = bndr->ummx[0][0];
	umax = bndr->ummx[0][1];
	vmin = bndr->vmmx[0][0];
	vmax = bndr->vmmx[0][1];

	if ((umax-umin) < UM_FUZZ || (vmax-vmin) < UM_FUZZ) goto Done;

	if (ltrimd)
	{
		status = uu_list_init1 (&iobuf, sizeof(UM_2Dcoord), 20, 20);
		if (status == UU_SUCCESS)
		status = uu_list_init1 (&cvus, sizeof(UM_coord), 100, 100);
		if (status != UU_SUCCESS) goto Done;
	}

	cvpts = bndr->cvpts;

	nu = sfattr->nu;
	nv = sfattr->nv;
	nup = sfattr->nup;
	nvp = sfattr->nvp;

	for (cvtyp = 1; cvtyp <= 2; cvtyp++)
	{
		if (cvtyp == 1)
		{
			nuv = nv - 1; nwp = nup;
			uvmin = vmin; uvmax = vmax;
			wr[0] = umin; wr[1] = umax;
			nw0 = nu - 1;
		}
		else
		{
			nuv = nu - 1; nwp = nvp;
			uvmin = umin; uvmax = umax;
			wr[0] = vmin; wr[1] = vmax;
			nw0 = nv - 1;
		}
		if (nw0 > 1 && nwp > 1)
		{
			nwp--;
			nwp = um_lcm(nwp,nw0) + 1;
		}

		if (nuv < 2) continue;
		duv = (uvmax-uvmin)/nuv;

		uv = uvmin;

		for (i = 1; i < nuv; i++)
		{
			uv += duv;
			UU_LIST_EMPTY (cvpts);
			if (ltrimd)
			{
				UU_LIST_EMPTY (&cvus); UU_LIST_EMPTY (&iobuf);
				status = S_display_uvline (&bs,tfmat,bndr,uv,cvtyp,wr,nwp,nw0,tol,
					cvpts,&cvus,&iobuf);
				if (status != UU_SUCCESS) break;
			}
			else
			{
				S_display_uvline0 (eptr,tfmat,cvpts,uv,cvtyp,wr,nwp,nw0,tol);
			}
			gdraw();
		}
	}
/*
...free allocated memory
*/
	if (ltrimd)
	{
		uu_list_free (&cvus);
		uu_list_free (&iobuf);
	}

Done:
	return (status);
}

/*********************************************************************
**    E_FUNCTION: ncl_display_edge (pts,uvt,ix,n,tlast)
**       Display a surface boundary edge (the current component if the boundary
**       is composite, the whole boundary if not)
**    PARAMETERS
**       INPUT  :
**          pts    - array of XYZ boundary points
**          uvt    - array of UV boundary points
**          ix     - starting index
**          n      - ending index for this boundary curve
**          tlast  - end parameter for this component
**          lreverse - reverse flag
**       OUTPUT :
**          ix     - starting index updated
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_display_edge (pts,uvt,ix,n,tlast,lreverse)
UM_coord *pts,*uvt;
int *ix,n;
UU_REAL tlast;
UU_LOGICAL lreverse;
{
	int j,j0;
	int ien,inc;
/*
.....Initialize routine
*/
	j0 = *ix;
/*
.....Determine loop indexes
*/
	if (lreverse)
	{
		ien = -1;
		inc = -1;
	}
	else
	{
		ien = n;
		inc = 1;
	}
/*
.....Find end of curve
*/
	if (tlast != 1.)
	{
		for (j=j0;j!=ien;j=j+inc)
		{
			if (uvt[j][2] >= tlast ||
				(j < n-1 && tlast-uvt[j][2] < uvt[j+inc][2]-tlast))
			{
				ien = j + inc;
				break;
			}
		}
	}
/*
.....Draw the curve
*/
	for (j=j0;j!=ien;j=j+inc) ncl_lineto (pts[j]);
/*
.....Set the current index
*/
	*ix = j - inc;
}

/*********************************************************************
**    I_FUNCTION     : int S_disp_bndr (eptr,bndr)
**       Display a trimmed surface boundary, every edge (composite component)
**       with a separate pick id.
**    PARAMETERS
**       INPUT  :
**          ltrimd     - TRUE if trimmed surface, else FALSE
**          eptr       - ptr to surface
**          bndr       - boundary struct
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_disp_bndr (ltrimd,eptr,bndr, shade)
UU_LOGICAL ltrimd;
struct NCL_fixed_databag *eptr;
UM_srf_boundary *bndr;
int shade;
{
	int k,ix,i,n1,npt,status;
	UM_coord *pts,*uvt;
	UU_KEY_ID key;
	struct NCL_fixed_databag cv;
	struct NCL_trimsf_rec *tsf;
	UU_REAL ti;
	UU_LOGICAL lreverse;
	int i1;

	status = UU_SUCCESS;
	pts = (UM_coord *) UU_LIST_ARRAY (bndr->cvpts);
	uvt = (UM_coord *) UU_LIST_ARRAY (bndr->uvpts);
	if (ltrimd) tsf = (struct NCL_trimsf_rec *)eptr;

	for (k = 0; k < bndr->nb; k++)
	{
		npt = bndr->np[k];
			
		if (uvt[0][2] > uvt[npt-1][2])		
		{
			lreverse = UU_TRUE;
			ix = npt - 1;
		}
		else
		{
			lreverse = UU_FALSE;
			ix = 0;
		}

		if (ltrimd)
		{
			key = (k == 0)? tsf->uv_key: tsf->ibndykey[k*2-1];
/*
... jingrong 9/18/98 : if the boundary curve is a composite curve, set
... pick id for each component of the composite curve in a format as
... "xxyy": xx = key of the composite curve, yy = cvid of the minor curve +1.
......andrew 11/8/12 : changed the format for the pickid/ckey to an index so
......the displst does not rely on the keys of the trim surface. Since the
......curves are stored in order, they can be retrieved using an index instead
......of a key.
*/
			cv.key = key;
			ncl_retrieve_data_fixed(&cv);
			if (ncl_itsa_compcrv(&cv))
			{
				status = ncl_compcrv_getnents (&cv,&n1);
				if (status != UU_SUCCESS) return (status);
				for (i = 0; i < n1; i++)
				{
					if (UM_set_constituent_pickids) 
					{
						if (shade)
							gspickid((k+1)*100+tsf->key+i+1);
						else
							gspickid((k+1)*100+i+1);
					}
					ncl_displst_setckey((k+1)*100+i+1);
					status = ncl_compcrv_getendparam (&cv,i,&ti);
					if (status != UU_SUCCESS) return (status);

					ncl_display_edge (pts,uvt,&ix,npt,ti,lreverse);
					gdraw();
				}
			}
			else
			{
/*
.....if the boundary curve is  not a composite curve, set
.....pick id for the curve in a format as "xx00": xx = key of the curve.
.....This is done to always maintain the pickid of the trimmed surface
.....boundary always higher than that of the surface.
.......Changed to mimic above change for composite curves - Andrew 11/8/12
*/
				if (UM_set_constituent_pickids) 
				{
					if (shade)
						gspickid((k+1)*100+tsf->key);
					else
						gspickid((k+1)*100);
				}
				ncl_displst_setckey((k+1)*100);

				ncl_display_edge (pts,uvt,&ix,npt,1.,lreverse);
				gdraw();
			}
		}
		else
		{
			for (i = 1; i <= 4; i++)
			{
				key = i;

				if (UM_set_constituent_pickids) gspickid(key);
				ncl_displst_setckey(key);

				ti = 0.25*i;

				ncl_display_edge (pts,uvt,&ix,npt,ti,lreverse);
				gdraw();
			}
		}

		pts += npt; uvt += npt;
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_disp_surf1 (ltrimd,eptr,tfmat,bndr,tess,attrptr,
**                                       sfattr,tol)
**       Display the trimmed surface boundary, then the rest of the surface.
**    PARAMETERS
**       INPUT  :
**          ltrimd     - TRUE if trimmed surface, else FALSE
**          eptr       - ptr to surface
**          tfmat      - transformation
**          bndr       - boundary struct
**          tess       - tessellation struct
**          attptr     - ptr to attribute bundle
**          sfattr     - surface display attributes
**          tol        - display tolerance
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : currently attrptr is set to display the boundary, after it
**                   is done, need to restore the original linestyle and color.
*********************************************************************/
int S_disp_surf1 (ltrimd,eptr,tfmat,bndr,tess,attrptr,sfattr,tol)
UU_LOGICAL ltrimd;
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UM_srf_boundary *bndr;
UM_tessellation *tess;
struct UC_attributedatabag *attrptr;
NCL_surfattr *sfattr;
UU_REAL tol;

{
	int status;

	ncl_displst_init (sfattr->lstix, eptr->key);

	status = S_disp_bndr (ltrimd,eptr,bndr, sfattr->sh_mode);
	if (status != UU_SUCCESS) goto Done;

	if (UM_set_constituent_pickids) gspickid(eptr->key);
	ncl_displst_setckey(eptr->key);
/*
..... reset the original attributes and the offset before displaying the rest
..... of the surface
*/
	if (attrptr->color != sfattr->color)
		attrptr->color = sfattr->color;
	if (attrptr->line_style != sfattr->lnstyle)
		attrptr->line_style = sfattr->lnstyle;
	um_set_disp_attr (attrptr);

	if (sfattr->sh_mode == 1)
	{
		status = ncl_disp_tess (eptr,tess);
		if (status == UU_SUCCESS)
		{
			status = ncl_displst_finish(tol,1);
			return (status);
		}
	}

	status = S_disp_uvlines (ltrimd,eptr,tfmat,bndr,sfattr,tol);
/*
..... Complete display list.
*/
	status = ncl_displst_finish(tol,0);
/*
.....added for hidden line removal
.....Yurong 2/11/99
*/
	if (ncl_is_hidline())
		ncl_display_hid_surf(eptr->key);

Done:
	return (status);
}

/*********************************************************************
**    I_FUNCTION     : void	ncl_grid_num (ur,nu0,nup0,nu,nup)
**       Set tessellation grid numbers: adjust, if needed, the number of points
**       to make sure grid lines always intersect at a point in 2D (and so
**       always intersect in 3D).
*********************************************************************/
static void S_grid_num (ur,nu0,nup0,nu,nup)
UU_REAL ur[];
int nu0,nup0,*nu,*nup;
{
	int nn,nu1,nup1;
	UU_REAL del;

	nu1 = nup1 = 0;

	nn = nu0 - 1;
	if (nn > 1)
	{
		del = (ur[1] - ur[0])/nn;
		if (del > UM_FUZZ) nu1 = nn;
	}
/*
.....Don't base the shading grid on the
.....number of points per curve, as this
.....can be quite high
.....For example, DISPLY/SF1,200,10,200,10
.....Results in a 1792x1792 grid, which takes forever to generate
.....Bobby  -  10/1/09
/*
	nn = nup0 - 1;
	if (nn > 1)
	{
		if (nu1 > 1)
		{
			nup1 = um_lcm(nn,nu1);
			nu1 = 0;
		}
		else
			nup1 = nn;
	}
*/

	if (nu1 > 0) nu1++;
	if (nup1 > 0) nup1++;

	*nu = nu1;
	*nup = nup1;
}

/*********************************************************************
**    I_FUNCTION     : void	UU_LOGICAL S_isfar (uvi,uv,npt)
**       Determine if a point is away from a set of points in 2D.
**    PARAMETERS
**       INPUT  :
**          uvi        - point in question
**          uv         - set of 2D points
**          np         - number of 2D points
**       OUTPUT : none
**    RETURNS      : UU_TRUE iff uvi is "far" from all uv points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_isfar (uvi,uv,np)
int np;
UM_2Dcoord *uv;
UM_coord uvi;
{
	int i;
	UU_REAL d,EPS;

	EPS = 4*UM_DFUZZ;
	for (i = 0; i < np; i++)
	{
		d = UM_SQDIS_2D(uvi,uv[i]);
		if (d < EPS) return (UU_FALSE);
	}

	return (UU_TRUE);
}

/*********************************************************************
**    I_FUNCTION     : void	S_weed_bndr1 (ib,n0,nptes,uvtes,bndr)
**       For a boundary curve, weed out points that are away from
**       tessellation points.
**    PARAMETERS
**       INPUT  :
**          bndr       - boundary struct
**          ib         - which boundary curve
**          n0         - first point index for this boundary curve
**          uvtes      - tessellation UV points
**          nptes      - number of tessellation UV points
**       OUTPUT :
**          bndr       - boundary struct weeded
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_weed_bndr1 (ib,n0,nptes,uvtes,bndr)
UM_srf_boundary *bndr;
int nptes,ib,n0;
UM_2Dcoord *uvtes;
{
	int i,np;
	UM_coord *uvt;

	uvt = (UM_coord *) UU_LIST_ARRAY (bndr->uvpts);
	np = bndr->np[ib];

	for (i = n0+1; i < n0+np-1; i++)
	{
		if (S_isfar (uvt[i],uvtes,nptes))
		{
			uu_list_delete (bndr->uvpts,i,1);
			uvt = (UM_coord *) UU_LIST_ARRAY (bndr->uvpts);
			uu_list_delete (bndr->cvpts,i,1);
			i--; np--;
		}
	}
	bndr->np[ib] = np;
}

/*********************************************************************
**    I_FUNCTION     : void	ncl_weed_trim_bndr (bndr,tess)
**       Weed out trimmed surface boundary points that are away from
**       tessellation points.
**    PARAMETERS
**       INPUT  :
**          bndr       - boundary struct
**          uvlst      - tessellation uv points
**       OUTPUT :
**          bndr       - boundary struct weeded
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_weed_trim_bndr (bndr,uvlst)
UM_srf_boundary *bndr;
UU_LIST *uvlst;
{
	int nptes,ib,n0;
	UM_2Dcoord *uvtes;
	UM_coord *uvt;

	nptes = uvlst->cur_cnt;
	uvtes = (UM_2Dcoord *) UU_LIST_ARRAY (uvlst);

	uvt = (UM_coord *) UU_LIST_ARRAY (bndr->uvpts);

	for (ib = 0, n0 = 0; ib < bndr->nb; ib++)
	{
		S_weed_bndr1 (ib,n0,nptes,uvtes,bndr);
		n0 += bndr->np[ib];
	}
}

/*********************************************************************
**    I_FUNCTION     : int S_get_bndr (ltrimd,eptr,tfmat,sfattr,bound,tess,
**                                     lmallocd,ltessed,tol)
**       Get a surface boundary, and tessellation if needed.
**    PARAMETERS
**       INPUT  :
**          ltrimd     - TRUE if trimmed surface, else FALSE
**          eptr       - ptr to surface
**          tfmat      - transformation
**          sfattr     - surface display attributes
**          tol        - display tolerance
**       OUTPUT :
**          bound      - boundary struct
**          tess       - tessellation struct
**          lmallocd   - some memory is malloc'ed for the boundary.
**          ltessed    - tessellation struct is filled
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_bndr (ltrimd,eptr,tfmat,sfattr,bound,tess,lmallocd,ltessed,tol)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UM_srf_boundary *bound;
UM_tessellation *tess;
NCL_surfattr *sfattr;
UU_REAL tol;
UU_LOGICAL ltrimd,*lmallocd,*ltessed;
{
	int status,istat,nupts,nvpts,nu,nv,flg;
	UU_REAL bplm[4];
	UU_LIST uvteslst;
	UM_tess_settype typ = UM_TESS_TOLER;

	status = UU_SUCCESS;
 /*
 ..... try to get the boundary form the surface list. the flag 2 means we
 ..... require the 'display' boundary that matches tessellation. if no such
 ..... matching boundary is stored we get whatever is stored.
 */
	flg = 0;
	if (sfattr->sh_mode == 1) flg = 2;
	istat = ncl_get_surf_boundary (ltrimd,eptr,tfmat,bound,tol,lmallocd,flg);

	if (istat == -1) return (UU_FAILURE);

	if (istat == 1) /* matching boundary is already stored */
	{
		if (sfattr->sh_mode == 1)
		{
/*
..... get tess from the surface list
*/
			status = ncl_get_surflist (TESSELLATION_LIST,eptr,tess);
			if (status == UU_SUCCESS)
			{
				*ltessed = UU_TRUE;
				return (UU_SUCCESS);
			}
		}
		else
		{
/*
..... if the mode is wireframe just check the tesslst is there,
..... we assume it is OK
			if (ncl_have_tesslst (eptr)) return (UU_SUCCESS);
*/
			return (UU_SUCCESS);
		}
	}

	ncl_lst_delete1 (WHOLE_BOUNDARY_LIST,eptr);

	if (sfattr->sh_mode == 0)
	{
/*
..... qar 97055: decided to do a wireframe display without tessellation.
*/
		*ltessed = UU_FALSE;

		if (!ltrimd)
		{
			status = um_evolve_bndr (tol,eptr,eptr,tfmat,0,1,bound->np,
				bound->ummx,bound->vmmx,bplm,bound,NULLST,UU_NULL);
		}
		return (status);
	}
/*
..... Note: if boundary is stored but tess is not, or if tess is stored but
..... boundary is not, we cannot use either - so delete and recalculate
*/
	ncl_lst_delete1 (TESSELLATION_LIST,eptr);
/*
..... tessellate the surface to make sure the boundary matches the tessellation.
..... tess will be stored and can be used for the shading display,
..... now (if sh_mode is 1) or later
*/
	S_grid_num (bound->ummx[0],sfattr->nu,sfattr->nvp,&nu,&nupts);
	S_grid_num (bound->vmmx[0],sfattr->nv,sfattr->nup,&nv,&nvpts);

	ncl_set_bndr_return();

	uu_list_init0 (&uvteslst);
	status = ncl_tess_surf1 (eptr,tfmat,bound,tess,&uvteslst,tol,typ,nupts,nu,nvpts,nv);
	uu_list_free (&uvteslst);

	ncl_reset_bndr_return();

	if (status == UU_SUCCESS)
	{
		/* if (ltrimd) S_weed_bndr (bound,tess); */
		*ltessed = UU_TRUE;
		status = ncl_store_surflist (TESSELLATION_LIST,eptr,tess);
/*
..... should not store boundary from a display routine - to match batch and
..... interactive runs.
.....
		if (status == UU_SUCCESS)
			status = ncl_store_bndrlist (eptr,bound,2);
*/
	}
	else
	{
/*
..... tessellation did not work - will display as a wireframe
*/
		*ltessed = UU_FALSE;
		um_free_tess (tess);
/*
..... should not store boundary from a display routine - to match batch and
..... interactive runs.
.....
		status = ncl_store_bndrlist (eptr,bound,1);
*/
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int  S_disp_surf (ltrimd,eptr,tfmat,attrptr,sfattr,tol)
**       Calculate a surface display, then display the surface.
**    PARAMETERS
**       INPUT  :
**          ltrimd     - TRUE if trimmed surface, else FALSE
**          eptr       - ptr to surface
**          tfmat      - transformation
**          attptr     - ptr to attribute bundle
**          sfattr     - surface display attributes
**          tol        - display tolerance
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : currently attrptr is set to display the boundary, after it
**                   is done, need to restore the original linestyle and color.
*********************************************************************/
static int S_disp_surf (ltrimd,eptr,tfmat,attrptr,sfattr,tol)
UU_LOGICAL ltrimd;
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;
NCL_surfattr *sfattr;
UU_REAL tol;
{
	int status;
	UU_LIST uvlst,cvlst;
	UM_2Dcoord ummx,vmmx;
	int np[2];
	UM_srf_boundary bound;
	UM_tessellation tess;
	UU_LOGICAL lmallocd,ltessed;

/*
.....Initialize boundary storage
*/
	bound.nb = 0;
	np[0] = np[1] = 0;
	ummx[0] = vmmx[0] = 0.;
	ummx[1] = vmmx[1] = 1.;
	bound.np = np;
	bound.ummx = &ummx;
	bound.vmmx = &vmmx;

	uu_list_init (&uvlst, sizeof(UM_coord), 0, 100);
	uu_list_init (&cvlst, sizeof(UM_coord), 0, 100);

	bound.uvpts = &uvlst;
	bound.cvpts = &cvlst;

	lmallocd = ltessed = UU_FALSE;
	ncl_set_boundary_toler (tol);
	bound.toler = tol;
	um_init_tess (&tess);
	um_set_tess_toler (tol);
	tess.toler = tol;

	status = S_get_bndr (ltrimd,eptr,tfmat,sfattr,&bound,&tess,&lmallocd,&ltessed,tol);

	if (status == UU_SUCCESS)
	{
		if (sfattr->sh_mode == 1 && !ltessed) sfattr->sh_mode = 0; /* if could not tessellate,
												      have to go wireframe */

		status = S_disp_surf1 (ltrimd,eptr,tfmat,&bound,&tess,attrptr,sfattr,tol);
	}

	if (lmallocd)
	{
		uu_free (bound.np);
		uu_free (bound.ummx);
		uu_free (bound.vmmx);
	}
	uu_list_free (&uvlst);
	uu_list_free (&cvlst);
	if (ltessed)
		um_free_tess (&tess);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_surf_disp (ltrimd,eptr,tfmat,attrptr)
**       Display a surface.
**    PARAMETERS
**       INPUT  :
**          ltrimd     - TRUE if trimmed surface, else FALSE
**          eptr       - ptr to surface
**          tfmat      - transformation
**          attptr     - ptr to attribute bundle
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_surf_disp (ltrimd,eptr,tfmat,attrptr)
UU_LOGICAL ltrimd;
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;
{
	UM_int2 idx;
	UU_REAL told;
	UU_REAL *lstp;
	int lstix,ndisp,shaded,ecolor,nup,nvp,nu,nv;
	int sh_mode,sh_symbol,lnstyle0,color0,displstyp,status;
	NCL_surfattr sfattr;

	idx = 175;
	getsc (&idx,&told);

	gsedgeflag (UG_OFF);

	ncl_disp_params (eptr,attrptr,&lstix,&ndisp,&lstp,&shaded,&ecolor,&nup,&nvp,
		&nu,&nv);

	sh_mode = ncl_shading_mode ();

	if (ltrimd)
		sh_symbol = 0;
	else
		ncl_get_shade_symbol (&sh_symbol);

	if (sh_symbol != 1 && shaded != 1) sh_mode = 0;
/*
..... if boundary is visible (ecolor > -1) set the edge color, else use
..... "invisible" line style when shaded
*/
	color0 = attrptr->color;
	lnstyle0 = attrptr->line_style;
/*
..... if edge color is set as "defalt" use the surface color
*/
	if (ecolor == 64 || sh_mode == 0) ecolor = color0;

	if (ecolor >= 0)
	{
		if (sh_mode == 1)
		{
			gsedgeflag (UG_ON);
		}

		attrptr->color = ecolor;
	}
	else if (sh_mode == 1)
		attrptr->line_style = 9;

	um_set_disp_attr (attrptr);

	displstyp = -1;
	if (ndisp > 0)
		displstyp = ncl_displst_type (lstp);
/*
..... If the surface has a display list, use it & return,
*/
	if (sh_mode == 1 && displstyp >= 0)
	{
		if (ltrimd)
			ncl_bndr_display(lstp,UM_DEFAULT_TF,eptr->key);
		else
			ncl_bndr_display1(lstp,tfmat);

		attrptr->line_style = lnstyle0;
		attrptr->color = color0;
		um_set_disp_attr (attrptr);

		status = ncl_display_shaded_sf (eptr,told);
		if (status == UU_SUCCESS)
			return (status);
		else
		{
			if (ecolor >= 0)
			{
				attrptr->color = ecolor;
				um_set_disp_attr (attrptr);
			}
			sh_mode = 0;
		}
	}

	if (sh_mode == 0)
	{
		if (!ncl_get_wireframe())
		{
			status = UU_SUCCESS;
			return (status);
		}
		if (displstyp == 1) /* boundary only is stored */
			ncl_lst_delete1 (DISPLAY_LIST,eptr);
		else if (displstyp == 0) /* whole wireframe display is stored */
		{
			if (ltrimd)
			status = ncl_displst_display1(lstp,UM_DEFAULT_TF,eptr->key,attrptr,
				color0);
			else
			status = ncl_displst_display1(lstp,tfmat,eptr->key,attrptr,color0);

/*
.....added for hidden line removal
*/
			if (ncl_is_hidline()) ncl_display_hid_surf(eptr->key);
			return (status);
		}
	}
/*
..... nothing is stored that can be used, recalculate all
*/
	sfattr.nu = nu;
	sfattr.nv = nv;
	sfattr.nup = nup;
	sfattr.nvp = nvp;
	sfattr.sh_mode = sh_mode;
	sfattr.lnstyle = lnstyle0;
	sfattr.color = color0;
	sfattr.lstix = lstix;

	status =
	S_disp_surf (ltrimd,eptr,tfmat,attrptr,&sfattr,told);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_disp_trimsf (eptr, tfmat, attptr)
**       Display a trimmed surface.
**    PARAMETERS
**       INPUT  :
**          eptr       - ptr to surface
**          tfmat      - transformation
**          attptr     - ptr to attribute bundle
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_disp_trimsf (eptr,tfmat,attrptr)
struct NCL_trimsf_rec *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;
{
	int status;

	if (ncl_setver (96))
	{
		status = ncl_disp_trimsf96 (eptr, tfmat, attrptr);
	}
	else
	{
		status = ncl_surf_disp (UU_TRUE,(struct NCL_fixed_databag *)eptr,tfmat,
			attrptr);
	}
	
	return (status);
}
