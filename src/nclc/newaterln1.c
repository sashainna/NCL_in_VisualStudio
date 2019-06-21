/*********************************************************************
**    NAME         :  newaterln1.c
**       CONTAINS:  Routines for analyzing waterline geometry
**
**		  ncl_wsurf_bound_init
**        ncl_get_solid_tess
**        ncl_process_netsf
**        ncl_process_netsf1
**		  ncl_tesselate_sfs 
**
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       newaterln1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:59
*********************************************************************/
#include "mdcoord.h"
#include "mgeom.h"
#include "msrf.h"
#include "uminmax.h"
#include "nclwaterln.h"
#include "msol.h"
#include "nconst.h"

static UU_LIST cvls1,uvls1;
static struct UM_evsrfout evsrf;
static UU_LOGICAL Suvswap = UU_FALSE;

extern UU_LOGICAL NCL_waterline;

/*********************************************************************
**    I_FUNCTION     : void ncl_wsurf_bound_init (p)
**    PARAMETERS
**       INPUT  :
**          p     - surface boundary data
**       OUTPUT : 
**			p     - surface boundary data
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    Initialize the surface boundary data 
*********************************************************************/
void ncl_wsurf_bound_init(p)
UM_srf_bound *p;
{
	p->nb = 0;
	p->npo = 0;
	p->np = UU_NULL;
	p->cvpts = UU_NULL;
}

/*********************************************************************
**    I_FUNCTION     : void S_wsurf_init (sfi)
**    PARAMETERS
**       INPUT  :
**          sfi     - surface data
**       OUTPUT : 
**			sfi     - surface data
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    Initialize the surface data 
*********************************************************************/
static void S_wsurf_init (sfi)
NCL_waterline_surf *sfi;
{
	sfi->bound.nb = sfi->bound.npo = 0;
	sfi->bound.np = UU_NULL;
	sfi->bound.cvpts = UU_NULL;
	sfi->trianlist = UU_NULL;
	sfi->key = NULLKEY;
	sfi->nvec[0] = sfi->nvec[1] = 0;
	sfi->sf_flag = GEN; sfi->flag1 = 0;
	sfi->slist = LIST_A;
	sfi->size = 0;
	sfi->ncvs = 0; sfi->nlist0 = sfi->plist0 = -1;
	sfi->zmax = sfi->box.xmax = sfi->box.ymax = -1000000.;
	sfi->zmin = sfi->box.xmin = sfi->box.ymin = 1000000.;
}

/*********************************************************************
**    I_FUNCTION     : void S_wbase_sf (sfi)
**       Fill the surface data for the base surface
**    PARAMETERS
**       INPUT  :
**          sfi      - surface data
**       OUTPUT : 
**			sfi     - base surface data
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_wbase_sf (sfi)
NCL_waterline_surf *sfi;
{
	UM_coord pti;

	sfi->bound.nb = 1;
	sfi->bound.npo = 5;
	sfi->bound.cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (sfi->bound.cvpts, sizeof(UM_coord), 5, 5);

	pti[2] = 0;
	pti[0] = 0;	pti[1] = 0;
	uu_list_push (sfi->bound.cvpts,pti);

	pti[0] = 1;	pti[1] = 0;
	uu_list_push (sfi->bound.cvpts,pti);

	pti[0] = 1;	pti[1] = 1;
	uu_list_push (sfi->bound.cvpts,pti);

	pti[0] = 0;	pti[1] = 1;
	uu_list_push (sfi->bound.cvpts,pti);

	pti[0] = 0;	pti[1] = 0;
	uu_list_push (sfi->bound.cvpts,pti);

	sfi->zmax = sfi->zmin = 0;
	sfi->sf_flag = HORZPL;
	sfi->size = 1;
	sfi->ncvs = 0; sfi->nlist0 = sfi->plist0 = -1;
	sfi->box.xmax = sfi->box.ymax = 1.;
	sfi->box.xmin = sfi->box.ymin = 0.;
}

/*********************************************************************
**    I_FUNCTION     : UU_LOGICAL S_same_axis (eptr,tfmat,wbase,tol)
**       Check if two surfaces of revolution have same axis
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          eptr       - surface structure
**          tfmat      - surface transformation matrix
**          tol        - tolerance
**       OUTPUT : none
**    RETURNS      :
**       UU_TRUE iff revolve axis are same; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_same_axis (eptr,tfmat,wbase,tol)
struct NCL_revsurf_rec *eptr;
UM_transf tfmat;
NCL_w_base *wbase;
UU_REAL tol;
{
	UM_coord pta,ptb;
	UM_vector vca,vcb;
	UU_REAL um_sqdis_from_line();
	UU_REAL d,tolsq;

	tolsq = tol*tol;

	ncl_revsf_axis (eptr,tfmat,pta,vca);
	ncl_revsf_axis (wbase->srf,wbase->tfmat,ptb,vcb);

	d = UM_DOT (vca,vcb);
	if (fabs(d) <= 0.99995) return(UU_FALSE);

	d = um_sqdis_from_line (pta,ptb,vcb);
	if (d > tolsq) return(UU_FALSE);

	d = um_sqdis_from_line (ptb,pta,vca);
	if (d > tolsq) return(UU_FALSE);

	return(UU_TRUE);
}

/*********************************************************************
**    I_FUNCTION     : void S_set_sfrevolv (sf,tfmat,wbase,sfrevolv,tol)
**       Check if a surface is a surface of revolution with the same axis
**       as the waterline base surface
**    PARAMETERS
**       INPUT  :
**          wbase      - waterline base surface data
**          sf         - surface structure
**          tfmat      - surface transformation matrix
**          tol        - tolerance
**       OUTPUT :
**          sfrevolv   - 0: if generic
**                       1: if the surface has the same axis as wbase
**                       2: if also the surface when projected on wbase creates
**                          a box
**    RETURNS      :
**       UU_TRUE iff revolve axis are same; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_set_sfrevolv (sf,tfmat,wbase,sfrevolv,tol)
struct NCL_fixed_databag *sf;
UM_transf tfmat;
NCL_w_base *wbase;
int *sfrevolv;
UU_REAL tol;
{
	struct NCL_fixed_databag *bsptr,bsf;
	struct NCL_trimsf_rec *tsf;
	int status;
	UU_LOGICAL ltrimd;

	ltrimd = (sf->rel_num == NCL_TRIMSF_REL);

	if (ltrimd)
	{
		tsf = (struct NCL_trimsf_rec *) sf;
		bsf.key = tsf->bs_key;
		status = ncl_retrieve_data_fixed (&bsf);
		if (status != UU_SUCCESS) return;
		bsptr = &bsf;
	}
	else
		bsptr = (struct NCL_fixed_databag *) sf;

	if (bsptr->rel_num != NCL_REVSURF_REL) return;

	if (S_same_axis ((struct NCL_revsurf_rec *)bsptr,tfmat,wbase,tol))
	{
		*sfrevolv = 1;
		if (!ltrimd)
		{
			UU_LIST_EMPTY (&cvls1);
			UU_LIST_EMPTY (&uvls1);
			if (ncl_gen_cv_planar (sf,&cvls1,&uvls1,tol))
				*sfrevolv = 2;
		}
	}
}

/*********************************************************************
*********************************************************************/
void ncl_free_bound (p)
UM_srf_bound *p;
{
	p->nb = 0;
	UU_FREE(p->np);
	UU_LIST_FREE (p->cvpts)
}

/*********************************************************************
**    I_FUNCTION     : int S_pts_proj_sf (pts,n,sf,u0,v0,uvlst,tolsq)
**       Project points on surface, check the points are all within
**       tolerance to the surface.
**    PARAMETERS
**       INPUT  :
**          pts        - points to project
**          n          - number of points
**          sf         - surface structure
**          u0,v0      - initial parameters
**          tolsq      - squared tolerance
**       OUTPUT :
**          uvlst      - UV projections
**    RETURNS      :
**         UU_SUCCESS iff no error and all points are within tolerance;
**         UU_FAILURE else
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_pts_proj_sf (pts,n,sf,u0,v0,uvlst,tolsq)
UM_coord *pts;
int n;
UU_REAL u0,v0,tolsq;
struct NCL_fixed_databag *sf;
UU_LIST *uvlst;
{
	int i,k;
	UU_REAL del;
	UM_coord spt,uvp;
	UM_real8 asw,pt8[3];
	UM_real4 ss[9];
	UM_real4 dir;
	UM_real4 u;
	UM_real4 v;
	UM_int2 isub,itype,isf;

	itype = 9; /* surface */
	isub = 1;
	ptdsc3(&sf->key,&isub,&itype,&asw);

	isf = 1;
	dir = 0;
	u = u0; v = v0;
	uvp[2] = 0;

	for (i = 0; i < n; i++)
	{
		for (k = 0; k < 3; k++) pt8[k] = pts[i][k];

		sfpt1 (&asw,pt8,&isf,&dir,&u,&v,ss);

		for (k = 0; k < 3; k++)
			spt[k] = ss[k+4];

		del = UM_SQDIS (spt,pts[i]);
		if (del > tolsq)
			return (UU_FAILURE);

		uvp[0] = u; uvp[1] = v;
		uu_list_push (uvlst,uvp);
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : void S_fix_circular_bound (ib,cv,sf,p,tol)
**       If a boundary curve corresponds to a circle in a trim surface
**       structure, evolve the circle itself and replace the
**       boundary points with the result.
**    PARAMETERS
**       INPUT  :
**          ib         - number of the boundary curve
**          cv         - the circular boundary curve
**          sf         - surface structure
**          p          - surface boundary
**          tol        - boundary tolerance
**       OUTPUT :
**          p          - surface boundary fixed
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fix_circular_bound (ib,cv,sf,p,tol)
int ib;
struct NCL_fixed_databag *cv,*sf;
UM_srf_boundary *p;
UU_REAL tol;
{
	int istat,n0,n1,j,n;
	UM_transf cvmat;
	UM_coord *pts,*uvs;
	UU_REAL u0,v0;

	istat = uc_retrieve_transf (cv->key, cvmat);
	if (istat == UU_SUCCESS)
	{
		UU_LIST_EMPTY (&cvls1);
		UU_LIST_EMPTY (&uvls1);
		n1 = ncl_evolve_circ ((struct  UM_circle_rec *)cv,cvmat,tol,&cvls1,
			UU_NULL,UU_NULL);
		if (n1 > 3)
		{
			pts = (UM_coord *) UU_LIST_ARRAY(&cvls1);

			for (j = n0 = 0; j < ib; j++)
				n0 += p->np[j];
/*
..... project on surface to get new UV points
*/
			uvs = (UM_coord *) UU_LIST_ARRAY(p->uvpts);
			u0 = uvs[n0][0]; v0 = uvs[n0][1];
			istat = S_pts_proj_sf (pts,n1,sf,u0,v0,&uvls1,tol*tol);
			if (istat == UU_SUCCESS)
			{
				n = p->np[ib];
				uu_list_delete (p->cvpts,n0,n);
				uu_list_insert_multiple (p->cvpts,n0,n1,pts);

				uu_list_delete (p->uvpts,n0,n);
				uvs = (UM_coord *) UU_LIST_ARRAY(&uvls1);
				uu_list_insert_multiple (p->uvpts,n0,n1,uvs);

				p->np[ib] = n1;
			}
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : void S_fix_circles (sf,tfmat,p,btol)
**       If a boundary curve corresponds to a circle in a trim surface
**       structure, evolve the circle itself and replace the
**       boundary points with the result.
**    PARAMETERS
**       INPUT  :
**          sf         - surface structure
**          p          - surface boundary
**          btol       - boundary tolerance
**       OUTPUT :
**          p          - surface boundary fixed
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fix_circles (sf,p,btol)
struct NCL_fixed_databag *sf;
UM_srf_boundary *p;
UU_REAL btol;
{
	int istat,ib;
	struct NCL_trimsf_rec *tsf;
	struct NCL_fixed_databag cv;

	if (sf->rel_num == NCL_TRIMSF_REL)
	{
		tsf = (struct NCL_trimsf_rec *) sf;

		for (ib = 0; ib < p->nb; ib++)
		{
			if (ib == 0)
				cv.key = tsf->cv_key;
			else
				cv.key = tsf->ibndykey[2*ib-2];
			if (cv.key != NULLKEY)
			{
				istat = ncl_retrieve_data_fixed (&cv);
				if (istat == UU_SUCCESS && cv.rel_num == UM_CIRCLE_REL)
					S_fix_circular_bound (ib,&cv,sf,p,btol);
			}
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : int S_get_bound (sf,tfmat,p,bound,btol)
**       Get a surface boundary, standard with some memory-saving tricks,
**        and fewer uu_malloc calls.
**    PARAMETERS
**       INPUT  :
**          sf         - surface structure
**          tfmat      - surface transformation matrix
**          btol       - boundary tolerance
**       OUTPUT :
**          p          - standard boundary structure filled
**          bound      - smaller waterline boundary structure
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_bound (sf,tfmat,p,bound,btol)
struct NCL_fixed_databag *sf;
UM_transf tfmat;
UM_srf_boundary *p;
UM_srf_bound *bound;
UU_REAL btol;
{
	int status,i,n,nb;

	status = ncl_get_bndry (sf,tfmat,p,btol,UU_TRUE);

	if (status == UU_SUCCESS && p->nb >= 1)
	{
		if (!ncl_setver(94))
			S_fix_circles (sf,p,btol);
		if (p->ummx[0][0] < 0.0) p->ummx[0][0] = 0.0;
		if (p->vmmx[0][0] < 0.0) p->vmmx[0][0] = 0.0;
		if (p->ummx[0][1] > 1.0) p->ummx[0][1] = 1.0;
		if (p->vmmx[0][1] > 1.0) p->vmmx[0][1] = 1.0;
		bound->nb = nb = p->nb;
		if (nb > 1)
			bound->np = (int *) uu_malloc((nb-1)*sizeof(int));
		bound->npo = p->np[0];
		for (i = 0; i < nb-1; i++) bound->np[i] = p->np[i+1];
		bound->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		n = p->cvpts->cur_cnt;
		uu_list_init (bound->cvpts, sizeof(UM_coord), n, n);
		uu_list_push_list (bound->cvpts, p->cvpts);
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : S_copy_sfbound(sfbndr,sfbound)
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
static int S_copy_sfbound(sfbndr,sfbound)
UM_srf_bound *sfbndr;
UM_srf_bound **sfbound;
{
	int i,npts,nb,n;
	UM_coord *pts;
	UM_srf_bound *bound;
		
	bound = (UM_srf_bound *) uu_malloc (sizeof(UM_srf_bound));
	ncl_wsurf_bound_init(bound);

	if (sfbndr->nb < 1)
		return UU_FAILURE;

	if (sfbndr->nb >= 1)
	{
		bound->nb = nb = sfbndr->nb;
		bound->npo = nb = sfbndr->npo;
		if (sfbndr->np)
		{
			bound->np = (int *) uu_malloc((nb)*sizeof(int));
			for (i = 0; i < nb; i++)
				bound->np[i] = sfbndr->np[i];
		}
		bound->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		n = sfbndr->cvpts->cur_cnt;
		uu_list_init (bound->cvpts, sizeof(UM_coord), n, n);
		uu_list_push_list (bound->cvpts, sfbndr->cvpts);
	}
	*sfbound = bound;

	return UU_SUCCESS;
}

/*********************************************************************
**    I_FUNCTION     : int S_get_boundary (sf,tfmat,p,bound,btol)
**       Get a surface boundary, standard with some memory-saving tricks,
**        and fewer uu_malloc calls.
**    PARAMETERS
**       INPUT  :
**          sf         - surface structure
**          tfmat      - surface transformation matrix
**          btol       - boundary tolerance
**       OUTPUT :
**          p          - standard boundary structure filled
**          bound      - smaller waterline boundary structure
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_boundary (sf,tfmat,p,bound,btol)
struct NCL_fixed_databag *sf;
UM_transf tfmat;
UM_srf_boundary *p;
UM_srf_boundary *bound;
UU_REAL btol;
{
	int status,i,n,nb;

	status = ncl_get_bndry (sf,tfmat,p,btol,UU_FALSE);

	if (status == UU_SUCCESS && p->nb >= 1)
	{
		if (p->ummx[0][0] < 0.0) p->ummx[0][0] = 0.0;
		if (p->vmmx[0][0] < 0.0) p->vmmx[0][0] = 0.0;
		if (p->ummx[0][1] > 1.0) p->ummx[0][1] = 1.0;
		if (p->vmmx[0][1] > 1.0) p->vmmx[0][1] = 1.0;
		bound->nb = nb = p->nb;
		if (nb > 0)
			bound->np = (int *) uu_malloc((nb)*sizeof(int));
		for (i = 0; i < nb; i++) bound->np[i] = p->np[i];
		bound->uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		bound->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		n = p->cvpts->cur_cnt;
		uu_list_init (bound->uvpts, sizeof(UM_coord), n, n);
		uu_list_push_list (bound->uvpts, p->uvpts);
		uu_list_init (bound->cvpts, sizeof(UM_coord), n, n);
		uu_list_push_list (bound->cvpts, p->cvpts);			
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : void ncl_wat_check_prim (sfp,tfmat,primtyp,param,tol1)
**       Check if a surface is planar (currently no other primitive types are
**       checked) - called when apparently no primitive analysis was done for
**       the surface. NOTE: nothing gets stored into UNIBASE, which is the
**       reason for this routine.
**    PARAMETERS
**       INPUT  :
**          sfp        - surface structure
**          tfmat      - surface transformation matrix
**          tol1       - tolerance (typically, twice the machining tol)
**       OUTPUT :
**          primtyp    - PLANE iff planar
**          param      - plane prim parameters if planar
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_wat_check_prim (sfp,tfmat,primtyp,param,tol1)
struct NCL_fixed_databag *sfp;
UM_transf tfmat;
UM_int2 *primtyp;
UM_real8 param[16],tol1;
{
#define CHKPTS 25
	int status,i,j,istat;
	UM_coord pt[3],*pts,pti;
	UM_vector nvec;
	UU_REAL u,v,du,dv,dis0,dis,dot;
	struct UM_rbsplsrf_rec *eptr,bsf;
	struct NCL_trimsf_rec *trimptr;
	UU_LOGICAL um_is_idmat(), idmat;

/*
... get three non-collinear pts on the sf.
*/
	uc_evsrf(UM_POINT, 0.3333, 0.5, sfp, tfmat,&evsrf);
	um_vctovc(evsrf.sp,pt[0]);
	uc_evsrf(UM_POINT, 0.6667, 0.25, sfp, tfmat,&evsrf);
	um_vctovc(evsrf.sp,pt[1]);
	uc_evsrf(UM_POINT, 0.6667, 0.75, sfp, tfmat,&evsrf);
	um_vctovc(evsrf.sp,pt[2]);
/*
... create a plane through the above three pts.
*/
	if (ncl_setver(95))
	{
		istat = um_3pt_colinear(pt[0],pt[1],pt[2],nvec,tol1);
		if (istat != 0) return;
		dis0 = UM_DOT(nvec,pt[0]);
	}
	else
	{
		istat = um_3pt_pln (pt[0],pt[1],pt[2],nvec,&dis0);
		if (istat != 0) return;
	}

	if (sfp->rel_num == NCL_TRIMSF_REL)
	{
		trimptr = (struct NCL_trimsf_rec *) sfp;
		bsf.key = trimptr->bs_key;
		status = ncl_retrieve_data_fixed (&bsf);
		if (status != UU_SUCCESS) return;
		eptr = &bsf;
	}
	else
		eptr = (struct UM_rbsplsrf_rec *) sfp;

	if (eptr->rel_num == UM_RBSPLSRF_REL)
	{
		if (eptr->ku <= 2 && eptr->kv <= 2)
		{
			j = eptr->no_pt;
			pts = (UM_coord *) eptr->pt;
			idmat = um_is_idmat(tfmat);
			for (i = 0; i < j; i++)
			{
				if (idmat)
				{
					dis = UM_DOT (nvec,pts[i]);
				}
				else
				{
					um_cctmtf (pts[i],tfmat,pti);
					dis = UM_DOT (nvec,pti);
				}
				if (fabs(dis-dis0) > tol1) return;
			}
			goto Store;
		}
	}
/*
.. determine if all the other pts on the sf fall on the same pl.
*/
	du = dv = 1./(CHKPTS-1);
	u = -du;
	for (i = 0; i < CHKPTS; i++)
	{
		u = u+du;
		v = -dv;
		for (j = 0; j < CHKPTS; j++)
		{
			v = v+dv;
			uc_evsrf(UM_NORM, u, v, sfp, tfmat,&evsrf);
			dis = UM_DOT(nvec,evsrf.sp);
			if (fabs(dis-dis0) > tol1) return;
/*
..... check the normals, but only away from the edges, since otherwise
..... this could foul a triangular plane
*/
			if (i*(CHKPTS-1-i)*j*(CHKPTS-1-j) > 0)
			{
				dot = UM_DOT(nvec,evsrf.snorm);
				if (fabs(dot) <= 0.99995) return;
			}
		}
	}
Store:
/*
... store prim data if it's a plane.
*/
	param[3] = dis0; /* distance from the origin to the plane */
	for (i = 0; i < 3; i++)
	{
		param[i] = nvec[i];    /* normal of the plane */
		param[i+4] = pt[0][i]; /* a point on the plane */
	}
	*primtyp = (UM_int2) NCLSF_PLANE;

	return;
}

/*********************************************************************
**    I_FUNCTION     : int S_set_sfflag (sfflag,bndr,sf,tfmat,rot,tol1)
**       Set the surface flag by its primitive type; if not a primitive
**       recognize vertical surfaces by evaluations.
**    PARAMETERS
**       INPUT  :
**          sfi        - waterline surface data
**          sf         - surface structure
**          tfmat      - surface transformation matrix
**          irot       - waterline transformation flag
**          rot        - waterline transformation matrix
**          tol1       - tolerance for primitives
**       OUTPUT :
**          sfi->sf_flag        - waterline surface data
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_sfflag (sfi,sf,tfmat,irot,rot,tol1)
NCL_waterline_surf *sfi;
struct NCL_fixed_databag *sf;
UM_transf tfmat,rot;
int irot;
UM_real8 tol1;
{
	nclsf_prim_type typ;
	UM_int4 skey;
	UM_int2 primtyp;
	int status,istat,i;
	UU_REAL u,v;
	UM_real8 primdata[16];
	UU_LOGICAL vertfl;

	status = UU_SUCCESS;

	primtyp = 0; typ = NCLSF_UNKNOWN;
	skey = sf->key;
	istat = ncl_get_sf_primdat(&skey,&primtyp,primdata);
	if (istat == UU_SUCCESS)
	{
		if (primtyp <= 0 || primtyp > 7 || (primtyp == 1 && !ncl_setver(95)))
		ncl_wat_check_prim (sf,tfmat,&primtyp,primdata,tol1);
	}
	if (istat == UU_SUCCESS)
	{
		typ = (nclsf_prim_type) primtyp;
		if (typ == NCLSF_PLANE)
		{
			if (irot == 1)
				ncl_transform_primdat (&typ,primdata,rot);
			for (i = 0; i < 2; i++) sfi->nvec[i] = primdata[i];
			if (fabs (primdata[2]) > 0.9999984769)
			{
				sfi->sf_flag = HORZPL;
				if (irot == 2) primdata[6] *= 25.4;
				sfi->zmin = sfi->zmax = primdata[6];
			}
			else if (fabs (primdata[2]) < 0.00005)
				sfi->sf_flag = VERTPL;
			else
				sfi->sf_flag = TILTPL;
		}
	}

	if (sfi->sf_flag == GEN)
	{
/*
..... recognize 'vertical' surfaces
*/
		if (typ == NCLSF_CYLINDER)
		{
			if (irot == 1)
			{
				um_vctmtf (&primdata[3],rot,&primdata[3]);
				um_unitvc (&primdata[3],&primdata[3]);
			}
			if (fabs (primdata[5]) > 0.99995)
				sfi->sf_flag = VERT;
		}
		else if (typ != NCLSF_SPHERE && typ != NCLSF_CONE)
		{
			vertfl = UU_TRUE;
			for (v = 0.1; v < 1 && vertfl && status == UU_SUCCESS; v += 0.2)
			{
				for (u = 0.1; u < 1 && vertfl && status == UU_SUCCESS; u += 0.2)
				{
					status = uc_evsrf(UM_NORM,u,v,sf,tfmat,&evsrf);
					if (status != UU_SUCCESS) break;
					if (irot == 1)
					{
						um_vctmtf (evsrf.snorm,rot,evsrf.snorm);
						um_unitvc (evsrf.snorm,evsrf.snorm);
					}
					if (fabs (evsrf.snorm[2]) > UM_FUZZ) vertfl = UU_FALSE;
				}
			}
			if (vertfl) sfi->sf_flag = VERT;
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : void S_xymmx_vert (sfi,xmmx,ymmx,tol)
**       Set the VERTPL surface flag by the calculated surface box.
**       Called when watrev and the surface would otherwise be BADPROJ.
**    PARAMETERS
**       INPUT  :
**          sfi        - waterline surface data
**          xmmx,ymmx  - UV box
**          tol        - tolerance
**       OUTPUT :
**          sfi        - waterline surface data updated for VERTPL
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_xymmx_vert (sfi,xmmx,ymmx,tol)
NCL_waterline_surf *sfi;
UU_REAL xmmx[],ymmx[];
UU_REAL tol;
{
	UU_REAL dx,dy,a;

	if (sfi->zmax - sfi->zmin < 50*tol) return;

	dx = xmmx[1] - xmmx[0];
	dy = ymmx[1] - ymmx[0];

	if (dx > 0.05 && dy < UM_FUZZ)
	{
		sfi->nvec[0] = 0;
		sfi->nvec[1] = 1;
		a = (ymmx[0] + ymmx[1])/2;
		ymmx[0] = ymmx[1] = a;
		sfi->sf_flag = VERTPL;
	}
	else if (dy > 0.05 && dx < UM_FUZZ)
	{
		sfi->nvec[0] = 1;
		sfi->nvec[1] = 0;
		a = (xmmx[0] + xmmx[1])/2;
		xmmx[0] = xmmx[1] = a;
		sfi->sf_flag = VERTPL;
	}
}

/*********************************************************************
**    I_FUNCTION     : void S_fix_closed (sf,tfmat,p,bound,tolsq)
**       If the surface is closed and its outer boundary looks like two circles
**       on a cylinder connected by an up-and-down segment, we store it as just
**       the two circles, both marked outer, and delete the connecting segment.
**    PARAMETERS
**       INPUT  :
**          p          - surface boundary
**          bound      - short boundary structure
**       OUTPUT : none
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fix_closed (sf,tfmat,p,bound,tolsq)
struct NCL_fixed_databag *sf;
UM_transf tfmat;
UM_srf_boundary *p;
UM_srf_bound *bound;
UU_REAL tolsq;
{
	UU_REAL u,v;
	UM_coord *uvs,*pts;
	UM_coord p0;
	int i,j,i0,j0,i1,j1,np,found;
	int *nps = UU_NULL;
	UU_LOGICAL closed;

	closed = (p->ummx[0][1] - p->ummx[0][0] > 0.9999);

	for (v = 0.; closed && v < 1.0001; v += 0.5)
	{
		u = 0.;
		uc_evsrf(UM_NORM,u,v,sf,tfmat,&evsrf);
		um_vctovc(evsrf.sp,p0);
		u = 1.;
		uc_evsrf(UM_NORM,u,v,sf,tfmat,&evsrf);
		closed = (UM_SQDIS (p0,evsrf.sp) < tolsq);
	}

	if (closed)
	{
		np = p->np[0] - 1;
		uvs = (UM_coord *) UU_LIST_ARRAY (p->uvpts);
/*
..... find a vertical segment starting at v = 0 or v = 1
*/
		for (i = 0, found = 0; i < np-2; i++)
		{
			if (fabs(uvs[i+1][0] - uvs[i][0]) < UM_FUZZ &&
				(uvs[i][0] < UM_FUZZ || uvs[i][0] > 1.-UM_FUZZ))
			{
				found = 1; break;
			}
		}
		if (!found) return;
		i0 = i;

		for (j = i0+1, found = 0; j < np; j++)
		{
			if (fabs(uvs[i0][0] - uvs[j+1][0]) > UM_FUZZ)
			{
				found = 1; break;
			}
		}
		if (!found) return;
		i1 = j;
/*
..... find the second vertical segment starting at the same v the first
..... one ended, and going along the opposite side of the unit quadrant
*/
		for (i = 1, found = 0; i < np; i++)
		{
			j = (i1 + i)%np;
			if (fabs(1. - uvs[j][0] - uvs[i1][0]) < UM_FUZZ &&
				(fabs(uvs[j][1] - uvs[i1][1]) < UM_FUZZ))
			{
				found = 1; break;
			}
		}
		if (!found) return;
		j1 = j;

		for (j = 1, found = 0; j < np; j++)
		{
			i = (j1 + j)%np;
			if (fabs(uvs[j1][0] - uvs[i+1][0]) > UM_FUZZ)
			{
				found = 1; break;
			}
		}
		if (!found) return;
		j0 = i;
/*
..... fix the stored boundary: remove the vertical segments, store the top part
..... separately
*/
		uvs = UU_NULL;

		nps = (int *) uu_malloc(bound->nb*sizeof(int));
		for (i = 1; i < bound->nb; i++) nps[i] = bound->np[i-1];
		bound->nb++;

		UU_FREE (bound->np);
		bound->np = nps;
		bound->npo = (i0 - j0 + np + 1)%np;
		bound->np[0] = (j1 - i1 + np + 1)%np;

		if (j0 < i0)
		{
			if (j1 == 0) j1 = np;
			if (j1 < i1)
			{
				uvs = (UM_coord *) uu_malloc (j1 * sizeof (UM_coord));
				pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
				for (i = 1; i < j1; i++)
					um_vctovc (pts[i],uvs[i]);
				np -= (j0 + i1-i0-1);
			}
			else if (j1 < np)
				uu_list_delete (bound->cvpts,j1+1,np-j1);
			if (j0 > 0) uu_list_delete (bound->cvpts,0,j0);
			uu_list_delete (bound->cvpts,i0+1,i1-i0-1);
			if (j1 < i1)
				uu_list_insert_multiple (bound->cvpts,np,j1-1,uvs);
		}

		UU_FREE (uvs);
	}
}

/*********************************************************************
**    I_FUNCTION     : int S_detect_badproj (sfflag,bndr,sf,tfmat,rot,irot)
**       Assign the BADPROJ flag to a surface if its normal vertical
**       component changes much over surface points.
**    PARAMETERS
**       INPUT  :
**          sf         - surface structure
**          tfmat      - surface transformation matrix
**          irot       - waterline transformation flag
**          rot        - waterline transformation matrix
**          bndr       - surface boundary
**       OUTPUT :
**          sf_flag    - surface flag
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_detect_badproj (sfflag,bndr,sf,tfmat,rot,irot)
ncl_waterline_sftype *sfflag;
struct NCL_fixed_databag *sf;
UM_transf rot,tfmat;
int irot;
UM_srf_boundary *bndr;
{
	UU_REAL u,v,cmin,cmax,co,umin,umax,vmin,vmax,du,dv;
	UU_LOGICAL lbad = UU_FALSE;
	int status = UU_SUCCESS;

	umin = bndr->ummx[0][0]; umax = bndr->ummx[0][1];
	du = umax - umin;
	if (du < 0.2)
	{
		u = (umin + umax)/2;
		umin = umax = u;
	}
	else
	{
		if (umin < 0.1) umin = 0.1;
		if (umax > 0.9) umax = 0.9;
		du = umax - umin;
		if (du > 0.5) du = du/2;
	}

	vmin = bndr->vmmx[0][0]; vmax = bndr->vmmx[0][1];
	dv = vmax - vmin;
	if (dv < 0.2)
	{
		v = (vmin + vmax)/2;
		vmin = vmax = v;
	}
	else
	{
		if (vmin < 0.1) vmin = 0.1;
		if (vmax > 0.9) vmax = 0.9;
		dv = vmax - vmin;
		if (dv > 0.5) dv = dv/2;
	}

	umax = umax + UM_FUZZ;
	vmax = vmax + UM_FUZZ;

	cmin = 1; cmax = -1;
	for (v = vmin; !lbad && v < vmax && status == UU_SUCCESS; v += dv)
	{
		for (u = umin; !lbad && u < umax && status == UU_SUCCESS; u += du)
		{
			status = uc_evsrf(UM_NORM,u,v,sf,tfmat,&evsrf);
			if (status != UU_SUCCESS) break;
			if (irot == 1)
			{
				um_vctmtf (evsrf.snorm,rot,evsrf.snorm);
				um_unitvc (evsrf.snorm,evsrf.snorm);
			}
			co = evsrf.snorm[2];
			if (co < cmin) cmin = co;
			if (co > cmax) cmax = co;
			lbad = (cmax - cmin > 1);
		}
	}

	if (lbad) *sfflag = BADPROJ;

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_weed_outer (bound,dtol)
**       Delete points too close together (outer boundary only).
**    PARAMETERS
**       INPUT  :
**          bound      - short boundary structure
**          dtol       - tolerance
**       OUTPUT : none
**
**    RETURNS      : number of points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_weed_outer (bound,dtol)
UM_srf_bound *bound;
UU_REAL dtol;
{
	UM_coord *pp;
	int i,np;
	UU_REAL d;
	UU_REAL dtolsq = dtol*dtol;

	pp = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
	np = bound->npo - 1;

	for (i = 0; i < np; i++)
	{
		d = UM_SQDIS (pp[i+1],pp[i]);
		if (d <= dtolsq)
		{
			if (i == np-1)
				uu_list_delete (bound->cvpts,i,1);
			else
				uu_list_delete (bound->cvpts,i+1,1);
			pp = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
			i--; np--; bound->npo--;
		}
	}
	return (np);
}

/*********************************************************************
**    I_FUNCTION     : void S_weed_outer1 (bound,tol)
**       Delete points too close together (outer boundary only).
**    PARAMETERS
**       INPUT  :
**          bound      - short boundary structure
**          dtol       - tolerance
**       OUTPUT : none
**
**    RETURNS      : number of points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_weed_outer1 (bound,tol)
UM_srf_bound *bound;
UU_REAL tol;
{
	UM_coord *pp;
	int i,np;
	UU_REAL duv,dh,eps;

	pp = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
	np = bound->npo - 1;

	eps = 4*UM_DFUZZ;
	for (i = 0; i < np; i++)
	{
		duv = UM_SQDIS_2D (pp[i+1],pp[i]);
		dh = fabs (pp[i+1][2] - pp[i][2]);
		if (duv < eps && dh < tol)
		{
			if (i == np-1)
				uu_list_delete (bound->cvpts,i,1);
			else
				uu_list_delete (bound->cvpts,i+1,1);
			pp = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
			i--; np--; bound->npo--;
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : void ncl_waterline_minmax(pt,zmin,zmax,xmmx,ymmx)
**       Update minimum and maximum XYZ bounds given a new point.
**    PARAMETERS
**       INPUT  :
**          pt         - a 3D point
**          zmin,zmax, xmmx,ymmx - current values
**       OUTPUT :
**          zmin,zmax, xmmx,ymmx - updated values
**    RETURNS : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_waterline_minmax(pt,zmin,zmax,xmmx,ymmx)
UM_coord pt;
UU_REAL *zmax,*zmin,*xmmx,*ymmx;
{
	UU_REAL r;

	r = pt[2];
	if (r > *zmax) *zmax = r;
	if (r < *zmin) *zmin = r;
	if (xmmx && ymmx)
	{
		r = pt[0];
		if (r < xmmx[0]) xmmx[0] = r;
		if (r > xmmx[1]) xmmx[1] = r;
		r = pt[1];
		if (r < ymmx[0]) ymmx[0] = r;
		if (r > ymmx[1]) ymmx[1] = r;
	}
	return;
}

/*********************************************************************
**    I_FUNCTION     : void S_set_btol (toler,btol,tesstyp)
**       Set boundary and tessellation tolerance by the smallest of
**       the display tolerance and the machining tolerance.
**       Added to improve match between batch and interactive processing.
**    PARAMETERS
**       INPUT  :
**          toler      - machining tolerance
**       OUTPUT :
**          btol       - tolerance for boundary and tessellation
**          tesstyp    - tessellation type
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_set_btol (toler,btol,tesstyp)
UU_REAL toler,*btol;
UM_tess_settype *tesstyp;
{
	UM_int2 idx;
	UM_real8 atol;
	UU_REAL dsptol;

	idx = 175; getsc (&idx,&atol);
	dsptol = atol;

	if (dsptol < toler)
	{
		*btol = dsptol;
		*tesstyp = UM_TESS_TOLER;
	}
	else
	{
		*btol = toler;
		*tesstyp = UM_TESS_WATRLN;
	}
}

/*********************************************************************
**    I_FUNCTION     : void S_get_btol (toler,btol,tesstyp)
**       Set boundary tolerance by the machining tolerance, divided by max size.
**       and by a factor of number.
**    PARAMETERS
**       INPUT  :
**			box 	   - surface 2d box
**			zmin	   - surface min z value
**			zmax	   - surface min z value
**          toler      - machining tolerance
**       OUTPUT :
**          btol       - tight tolerance for boundary 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_get_btol (box,zmin,zmax,toler,btol)
UM_2box box;
UU_REAL zmin,zmax,toler,*btol;
{
	UM_int2 isub, mm;
	UU_REAL dtol,dx,dy,dz,dmax;

	isub = 264;	
	getifl(&isub,&mm);

	dx = box.xmax - box.xmin;
	dy = box.ymax - box.ymin;
	dz = zmax - zmin;
	dmax = MAX3(dx,dy,dz);
	dmax = (mm == 1) ? dmax/25.4 : dmax;	
	dtol = toler/dmax;
	dtol = MAX2(dtol/5.0,0.1*UM_FUZZ);

	*btol = dtol;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_solid_tess (sf,dlist,tess,btol)
**       Get a tessellation for a solid
**    PARAMETERS
**       INPUT  :
**          sf         - solid structure
**          btol       - boundary tolerance
**       OUTPUT :
**          tess       - tessellation
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_solid_tess (sf,dlist,tess,btol)
struct NCL_fixed_databag *sf;
UM_tessellation *tess;
UU_LIST *dlist;
UU_REAL btol;
{
	int status,i;
	struct UM_solid_rec *sld,*solid;
	struct UM_srfdatabag e1;
	UM_int2 idx;
	UM_real8 tsav,rval;
	UM_tessellation tmptess;

	idx = 175; getsc(&idx,&tsav); rval = btol; setscv(&idx,&rval);
	sld = (struct UM_solid_rec *) sf;
/*
.....Composite solid
*/
	if (sld->type == UM_COMPOS_SOLID)
	{
		ncl_set_boundary_toler(btol);
		um_set_tess_toler(btol);
		tess->np = tess->ntri = 0;
		tess->toler = btol;
		status = uu_list_init1(&tess->vertices,sizeof(UM_coord),500,100);
		status = uu_list_init1(&tess->normals,sizeof(UM_coord),500,100);
		status = uu_list_init1(&tess->tri,sizeof(UM_tript),500,100);
		for (i=0;i<sld->no_netkey;i++)
		{
			e1.key = sld->netkey[i];
			uc_retrieve_data(&e1,sizeof(e1));
/*
........Tessellate solid within tolerance
*/
			if (e1.rel_num == UM_SOLID_REL)
			{
				solid = (struct UM_solid_rec *)&e1;
				status = ncl_solid_calc_lists(solid,solid->sdata,solid->no_sdata,
					dlist,&tmptess);
				if (status != UU_SUCCESS) goto done;
				if (dlist->data) uu_list_free(dlist);
			}
/*
........Tessellate surface within tolerance
*/
			else
			{
				um_init_tess(&tmptess);
				status = ncl_tessellate_surf(&e1,&tmptess);
				if (status != UU_SUCCESS) goto done;
			}
/*
........Merge tessellatations
*/
			um_merge_tessel(tess,&tmptess);
			um_free_tess(&tmptess);
		}
/*		um_debug_tess(tess);*/
	}
	else
	{
		status = ncl_solid_calc_lists(sld,sld->sdata,sld->no_sdata,dlist,tess);
		if (dlist->data) uu_list_free(dlist);
	}
	setscv(&idx,&tsav);

/*
.....End of routine
*/
done:;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_watcmp(e1,e2)
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
int ncl_watcmp(e1,e2)
NCL_waterline_surf *e1,*e2;
{
	if (e1->size > e2->size) return(-1);
	else if (e1->size < e2->size) return(1);
	else return(0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_process_netsf
**       Fill in various data for a collection of surfaces;
**       calculate global zmin and zmax
**    PARAMETERS
**       INPUT  :
**          itsk       - 0: need surface data for contour and intersections
**                       1: need surface data only for contour
**          numsf      - number of surfaces
**          sfkey      - surface keys
**          rot        - transformation matrix (from Unibase to part coordsys)
**          toler      - Unibase tolerance
**          tol        - part tolerance
**       OUTPUT :
**          zmax,zmin  - global Z-range for all surfaces
**          xmmx,ymmx  - global UV-range for all surfaces
**          sff        - data for each surface
**          npt        - max number of points in a surface outer boundary
**          badsf      - bad surface label
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_process_netsf (itsk,numsf,sfkey,rot,zmax,zmin,sff,tol,toler,npt,
											areamin,xmmx,ymmx,badsf)
int itsk,numsf,*npt;
UU_KEY_ID *sfkey;
UM_transf rot;
UU_REAL *zmax,*zmin,*xmmx,*ymmx,tol,toler,areamin;
NCL_waterline_surf *sff;
char *badsf;
{
	int i,jj,kk,isf,status,npts,irot,numa,numb,numc,insf;
	struct NCL_fixed_databag sf;
	UM_coord *pts;
	UU_REAL hmin,hmax,btol;
	UM_real8 tol1;
	NCL_waterline_surf *sfi;
	UM_transf tfmat;
	UM_tessellation tess;
	UM_trian *ptri;
	UM_srf_boundary bndr;
	UU_LIST cvlst,uvlst,trilst,dlist;
	UM_tess_settype tesstyp;
	int ib,iinit,isfkey;
	UU_LOGICAL lsolid;
	UU_REAL area,ari,ar0;
	UU_REAL tolsq,tolersq;
	UU_REAL sfi_xmmx[2],sfi_ymmx[2];
	int ncl_watcmp();
	UU_LOGICAL lv97,lmgr;
	UU_LIST uvtes,uvtrilst,*trianlst;
	UM_int2 isub, mm,nbsf;
	UU_REAL dtol,dx,dy,dz,dmax;
	UU_REAL u,v,uv[2],du,dv;

	UM_srf_bound *polybndr;
	UM_int2 npolys,j;
	UU_LIST *polylst;
	UU_REAL box[6];

	S_set_btol (toler,&btol,&tesstyp);

	status = UU_SUCCESS;
	hmax = -1000000.; hmin = 1000000.;
	tol1 = (toler > 0.0005)? 2.*toler: 0.001;
	tolsq = 0.01*tol*tol; tolersq = 0.01*toler*toler;
	if (xmmx && ymmx)
	{
		xmmx[0] = ymmx[0] = 1000000.;
		xmmx[1] = ymmx[1] = -1000000.;
	}
	ncl_set_boundary_toler (btol);
	um_set_tess_toler (btol);
	um_init_boundary (&bndr);
	uu_list_init (&cvlst, sizeof(UM_coord), 100, 200);
	uu_list_init (&uvlst, sizeof(UM_coord), 100, 200);
	if (!cvlst.data || !uvlst.data) status = UU_FAILURE;
	uu_list_init0 (&trilst); uu_list_init0(&dlist);
	uu_list_init (&cvls1, sizeof(UM_coord), 0, 200);
	uu_list_init (&uvls1, sizeof(UM_coord), 0, 200);
	bndr.uvpts = &uvlst;
	bndr.cvpts = &cvlst;
	um_init_tess(&tess);
	ncl_get_rotfl (&irot);

	box[0] = box[1] = box[2] = 100000;
	box[3] = box[4] = box[5] = -100000;

	lv97 = ncl_setver(97);
	nbsf = 0;
	lmgr = UU_FALSE;
	if (!lv97)
	{
		iinit = nclc_tessmgr_check_init();
		if (iinit != UU_SUCCESS)
			nclc_tessmgr_init();
		else
			nbsf = nclc_tessmgr_get_nbsf();
	}

	if (NCL_waterline)
		ncl_wat_get_sfnums (&numa,&numb,&numc);
	else
	{
		numa = numsf;
		numb = numc = 0;
	}

	sfi = sff;
	for (isf = 0; isf < numsf && status == UU_SUCCESS; isf++, sfi++)
	{
		S_wsurf_init (sfi);
		if (isf >= numa)
		{
			if (numb > 0 && isf < numa+numb)
				sfi->slist = LIST_B;
			else if (numc > 0 && isf >= numa+numb)
				sfi->slist = LIST_C;
			else
			{
				numsf = isf;
				break;
			}
		}

		sf.key = sfkey[isf];
		status = ncl_retrieve_data_fixed (&sf);
		if (status == UU_SUCCESS)
			status = uc_retrieve_transf (sf.key, tfmat);
		if (status != UU_SUCCESS) continue;
		lmgr = UU_FALSE;

		lsolid = (sf.rel_num == UM_SOLID_REL);
		if (lsolid) goto Tess;
		ncl_free_bndry (&bndr);
		UU_LIST_EMPTY (bndr.uvpts); UU_LIST_EMPTY (bndr.cvpts);
		status = S_get_bound (&sf,tfmat,&bndr,&sfi->bound,btol);

		if (status != UU_SUCCESS || sfi->bound.nb < 1) continue;

		if (uvlst.max_cnt >= 4*uvlst.exp_cnt && isf <= numsf/2)
		{
			uvlst.exp_cnt *= 2; cvlst.exp_cnt *= 2;
		}
		if (*npt < sfi->bound.npo) *npt = sfi->bound.npo;

		if (irot > 0)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
			for (ib = 0; ib < sfi->bound.nb; ib++, pts += npts)
			{
				npts = (ib == 0)? sfi->bound.npo: sfi->bound.np[ib-1];
				for (i = 0; i < npts; i++)
					um_cctmtf(pts[i],rot,pts[i]);
			}
		}

		sfi_xmmx[0] = sfi_ymmx[0] = 1000000.;
		sfi_xmmx[1] = sfi_ymmx[1] = -1000000.;
		sfi->key = sf.key;

		status = S_set_sfflag (sfi,&sf,tfmat,irot,rot,tol1);

		if (status == UU_SUCCESS && sfi->sf_flag == GEN)
		{
			if (itsk == 0)
				S_fix_closed (&sf,tfmat,&bndr,&sfi->bound,tolersq);
			else if (itsk == 1)
				status = S_detect_badproj (&sfi->sf_flag,&bndr,&sf,tfmat,rot,irot);
		}
		if (status != UU_SUCCESS) break;

Tess:
		if (lsolid || (lv97 && itsk == 0 && 
			(sfi->sf_flag == GEN || sfi->sf_flag == VERT)))
		{
			if (lsolid)
			{
				sfi_xmmx[0] = sfi_ymmx[0] = 1000000.;
				sfi_xmmx[1] = sfi_ymmx[1] = -1000000.;
				sfi->key = sf.key;
				sfi->flag1 = VSOLID;
			}
			status = ncl_get_tesslst (&sf,&tess);
			npts = 0;
			if (status != UU_SUCCESS)
			{
				if (lsolid)
				{
					status = ncl_get_solid_tess (&sf,&dlist,&tess,btol);
				}
				else
				{
					status = ncl_tess_surf (&sf,tfmat,&bndr,&tess,btol,
										tesstyp,npts,npts);
				}
			}

			if (status == UU_SUCCESS)
			{
				if (!trilst.data)
				{
					npts = tess.ntri;
					i = (npts > 200)? npts: 200;
					uu_list_init (&trilst,sizeof (UM_trian),i,i);
				}
				else
					UU_LIST_EMPTY (&trilst);
				status = ncl_get_tess_triangles (&tess,&trilst,2,0);
				npts = trilst.cur_cnt;

				if (lsolid)
				{
					i = npts/3 + 2;
					if (*npt < i) *npt = i;
				}

				if (status == UU_SUCCESS)
				{
					if (trilst.max_cnt >= 4*trilst.exp_cnt && isf <= numsf/2)
						trilst.exp_cnt *= 2;
					sfi->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));

					uu_list_init (sfi->trianlist,sizeof (UM_trian),npts,npts);
					uu_list_push_list (sfi->trianlist, &trilst);
				}
			}
			if (status != UU_SUCCESS) break;

			um_clean_tess (&tess);
			ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
			for (i = 0; i < npts; i++, ptri++)
			{
				if (irot > 0) um_cctmtf(ptri->p1,rot,ptri->p1);
				ncl_waterline_minmax(ptri->p1,&sfi->zmin,&sfi->zmax,sfi_xmmx,
					sfi_ymmx);
				if (irot > 0) um_cctmtf(ptri->p2,rot,ptri->p2);
				ncl_waterline_minmax(ptri->p2,&sfi->zmin,&sfi->zmax,sfi_xmmx,
					sfi_ymmx);
				if (irot > 0) um_cctmtf(ptri->p3,rot,ptri->p3);
				ncl_waterline_minmax(ptri->p3,&sfi->zmin,&sfi->zmax,sfi_xmmx,
					sfi_ymmx);
			}

			if (sfi->sf_flag == GEN && !lsolid)
			{
/*
..... calculate ar0 - area of the outer boundary polygon, ari - total area of
..... inner boundaries
*/
				ar0 = ari = 0.;

				pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
				npts = sfi->bound.npo - 1;
				ncl_test_projection (&sfi->sf_flag,npts,pts);
				if (sfi->sf_flag == BADPROJ)
				{
					i = S_weed_outer (&sfi->bound,tol);
					if (i < npts)
					{
						sfi->sf_flag = GEN;
						pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
						npts = sfi->bound.npo - 1;
						ncl_test_projection (&sfi->sf_flag,npts,pts);
					}
				}
/*
..... if a surface 'overhangs' itself, e.g., a cylinder with a horizontal
..... axis, we estimate its area as one half of the sum of all triangles
*/
				if (sfi->sf_flag == BADPROJ)
				{
					ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
					npts = sfi->trianlist->cur_cnt;

					for (i = 0; i < npts; i++, ptri++)
					{
						ar0 += fabs (um_triangle_signed_area(ptri->p1,ptri->p2,
											ptri->p3));
					}
					ar0 /= 2;
				}
				else
				{
/*
..... rotate all boundaries if rotfl; use the outer boundary for min and max
*/
					for (ib = 0; ib < sfi->bound.nb; ib++, pts += npts)
					{
						npts = (ib == 0)? sfi->bound.npo: sfi->bound.np[ib-1];
						for (i = 0, area = 0; i < npts; i++)
						{
							if (i > 1 && i < npts-1)
								area += um_triangle_signed_area (pts[0],pts[i],
											pts[i-1]);
						}
						if (ib == 0)
							ar0 = fabs(area);
						else
						{
							if (area < 0) area = -area;
/* mark inner boundaries too small for the cutter */
							if (area <= areamin) sfi->bound.np[ib-1] *= -1;
							ari += area;
						}
					}
				}
				if (ar0 < ari + tolsq)
					sfi->flag1 = MANY_HOLES;
				else
					sfi->size = (ar0 - ari)*(ar0 - ari)/ar0;
			}
/*
..... if horizontal - free the trianlist
*/
			if (sfi->zmax - sfi->zmin < tol && sfi->sf_flag != BADPROJ)
				UU_LIST_FREE (sfi->trianlist);

		}
		else
		{
			isfkey = UU_FAILURE;
			if (!lv97 && (sfi->sf_flag == GEN || 
				sfi->sf_flag == VERT || sfi->sf_flag == BADPROJ))
			{	
/*
.....Check if the surface has been tessellated
*/
				isfkey = nclc_tessmgr_check_sfkey(sfi->key);
				if (isfkey != UU_SUCCESS)
				{
					lmgr = UU_TRUE;
					nclc_tessmgr_create();
					status = nclc_tessmgr_tessellate(nbsf,&sf,tfmat,
										rot,irot,&bndr,btol,tesstyp);
				}

/*
.....Get sfi_xmmx,sfi_ymmx for untrim surface from base tessellation
*/
				if (!ncl_itsa_trimsrf(&sf))
				{
					UU_LIST trilst1;
					UU_LIST *trianlst;
					status = nclc_tessmgr_get_tess_trianlst(sfi->key,
													&trianlst,UU_NULL);
					if (status != UU_SUCCESS)
						break;
					npts = UU_LIST_LENGTH(trianlst);
/*
.....Copy the original tess trianlst
*/
					uu_list_init (&trilst1,sizeof (UM_trian),npts,npts);
					uu_list_push_list (&trilst1, trianlst);

					ptri = (UM_trian *)UU_LIST_ARRAY(&trilst1);
					for (i = 0; i < npts; i++, ptri++)
					{
						if (irot > 0) um_cctmtf(ptri->p1,rot,ptri->p1);
						ncl_waterline_minmax(ptri->p1,&sfi->zmin,&sfi->zmax,
							sfi_xmmx,sfi_ymmx);
						if (irot > 0) um_cctmtf(ptri->p2,rot,ptri->p2);
						ncl_waterline_minmax(ptri->p2,&sfi->zmin,&sfi->zmax,
							sfi_xmmx,sfi_ymmx);
						if (irot > 0) um_cctmtf(ptri->p3,rot,ptri->p3);
						ncl_waterline_minmax(ptri->p3,&sfi->zmin,&sfi->zmax,
							sfi_xmmx,sfi_ymmx);
					}						
					uu_list_free (&trilst1);	
				}
/*
.....Check a grid of trim surface points to make sure points above the
.....boundary are considered - ASF 10/3/13.
*/
				else
				{
					du = (bndr.ummx[0][1] - bndr.ummx[0][0])/8.;
					dv = (bndr.vmmx[0][1] - bndr.vmmx[0][0])/8.;
					u = bndr.ummx[0][0];
					for (jj=0;jj<9;jj++)
					{
						v = bndr.vmmx[0][0];
						for(kk=0;kk<9;kk++)
						{
							uc_evsrf(UM_POINT,u,v,&sf,tfmat,&evsrf);
							uv[0] = u; uv[1] = v;
							insf = um_inside_bndry (uv,evsrf.sp,&bndr,&tol);
							if (insf >= 0)
							{
								if (irot > 0) um_cctmtf(evsrf.sp,rot,evsrf.sp);
								if (evsrf.sp[2] > sfi->zmax) sfi->zmax = evsrf.sp[2];
								if (evsrf.sp[2] < sfi->zmin) sfi->zmin = evsrf.sp[2];
							}
							v += dv;
						}
						u += du;
					}
				}
			}

			pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
/*
..... rotate all boundaries if rotfl; use the outer boundary for min and max
*/
			if (sfi->sf_flag == VERT || sfi->sf_flag == VERTPL)
			{
				for (ib = 0; ib < sfi->bound.nb; ib++, pts += npts)
				{
					if (ib > 0 && irot == 0) break;
					npts = (ib == 0)? sfi->bound.npo: sfi->bound.np[ib-1];
					for (i = 0; i < npts; i++)
					{
						if (ib == 0 && i < npts - 1)
							ncl_waterline_minmax(pts[i],&sfi->zmin,&sfi->zmax,
								sfi_xmmx,sfi_ymmx);
					}
				}
			}
			else
			{
				ar0 = ari = 0.;
				for (ib = 0; ib < sfi->bound.nb; ib++, pts += npts)
				{
					npts = (ib == 0)? sfi->bound.npo: sfi->bound.np[ib-1];
					for (i = 0, area = 0; i < npts; i++)
					{
						if (ib == 0 && i < npts - 1)
							ncl_waterline_minmax(pts[i],&sfi->zmin,&sfi->zmax,sfi_xmmx,
								sfi_ymmx);
						if (i > 1 && i < npts-1)
							area += um_triangle_signed_area (pts[0],pts[i],pts[i-1]);
					}
					if (ib == 0)
						ar0 = fabs(area);
					else
					{
						if (area < 0) area = -area;
						if (area <= areamin) sfi->bound.np[ib-1] *= -1;
						ari += area;
					}
				}
				if (ar0 < ari + tolsq)
					sfi->flag1 = MANY_HOLES;
				else
					sfi->size = (ar0 - ari)*(ar0 - ari)/ar0;
			}
		}
		if (sfi->zmax - sfi->zmin < tol) sfi->sf_flag = HORZPL;

		area = (sfi_xmmx[1] - sfi_xmmx[0])*(sfi_ymmx[1] - sfi_ymmx[0]);
		if (itsk == 1)
			sfi->size = area;
		else if (sfi->sf_flag == VERT || sfi->sf_flag == VERTPL)
		{
			ar0 = sfi_xmmx[1] - sfi_xmmx[0] + sfi_ymmx[1] - sfi_ymmx[0];
			sfi->size = ar0*ar0;
		}
		else if (sfi->bound.nb > 2 && sfi->size < 0.05*area)
		{
/* a surface is flagged if it has "many holes" */
			for (ib = 1, i = 0; ib < sfi->bound.nb; ib++)
			{
				if (sfi->bound.np[ib-1] > 0) i++;
				if (i > 1)
				{
					sfi->flag1 = MANY_HOLES; break;
				}
			}
		}

		if (sfi->zmax > hmax) hmax = sfi->zmax;
		if (sfi->zmin < hmin) hmin = sfi->zmin;
		if (xmmx && ymmx)
		{
			if (sfi_xmmx[0] < xmmx[0]) xmmx[0] = sfi_xmmx[0];
			if (sfi_xmmx[1] > xmmx[1]) xmmx[1] = sfi_xmmx[1];
			if (sfi_ymmx[0] < ymmx[0]) ymmx[0] = sfi_ymmx[0];
			if (sfi_ymmx[1] > ymmx[1]) ymmx[1] = sfi_ymmx[1];
		}
		sfi->box.xmin = sfi_xmmx[0]; sfi->box.xmax = sfi_xmmx[1];
		sfi->box.ymin = sfi_ymmx[0]; sfi->box.ymax = sfi_ymmx[1];

/*
...	Get the surface boundary with tight tolerance
*/	
		if (!lv97 && lmgr)
		{
			S_get_btol(sfi->box,sfi->zmin,sfi->zmax,toler,&dtol);
			status = nclc_tessmgr_srf_bndr(nbsf,&sf,tfmat,toler,dtol);
			nbsf++;
		}
	}/* end for (isf=0 */

	if (status == UU_SUCCESS)
	{
		if (numsf > 1)
		uu_qsort (sff,numsf,sizeof(NCL_waterline_surf),ncl_watcmp);
	}
	else if (isf < numsf)
	{
		if (sfi->key != NULLKEY)
		{
			sfi->key = NULLKEY; ncl_free_bound (&sfi->bound);
		}
		if (badsf != UU_NULL) ncl_get_label(&sf,badsf);
	}

	if (status == UU_SUCCESS)
	{
		if (*npt > 3 && hmax >= hmin)
		{
			*zmax = hmax;
			*zmin = hmin;
		}
		else
			status = UU_FAILURE;
	}

	um_free_tess (&tess);

	if (trilst.data) uu_list_free (&trilst);
	ncl_free_bndry (&bndr);
	uu_list_free (&uvlst);
	uu_list_free (&cvlst);
	uu_list_free (&cvls1);
	uu_list_free (&uvls1);
	
	return (status);
}

/*********************************************************************
*********************************************************************/
UU_LOGICAL ncl_watrev_swap()
{
	return (Suvswap);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_process_netsf1
**       Fill in various data for a collection of surfaces;
**       calculate global zmin and zmax. The version for a surface base.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          numsf      - number of surfaces
**          sfkey      - surface keys
**          rot        - waterline transformation matrix
**          toler      - Unibase tolerance
**          tol        - part tolerance
**       OUTPUT :
**          zmax,zmin  - global Z-range for all surfaces
**          xmmx,ymmx  - global UV-range for all surfaces
**          sff        - data for each surface
**          npt        - max number of points in a surface outer boundary
**          badsf      - bad surface label
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_process_netsf1 (wbase,numsf,sfkey,rot,zmax,zmin,sff,tol,toler,npt,
											areamin,xmmx,ymmx,badsf)
int numsf,*npt;
UU_KEY_ID *sfkey;
UM_transf rot;
UU_REAL *zmax,*zmin,*xmmx,*ymmx,tol,toler,areamin;
NCL_waterline_surf *sff;
NCL_w_base *wbase;
char *badsf;
{
	int i,k,isf,status,npts,irot,numa,numb,numc;
	struct NCL_fixed_databag sf;
	UM_coord *pts;
	UU_REAL hmin,hmax,h;
	NCL_waterline_surf *sfi;
	UM_transf tfmat;
	UM_tessellation tess;
	UM_trian *ptri;
	UM_srf_boundary bndr;
	UU_LIST cvlst,uvlst,trilst,dlist;
	int ib,istat,sfrevolv,iinit,isfkey;
	UU_REAL area,ari,ar0,d,dx,dy,y0,y1,btol;
	UU_REAL tolsq,tolersq;
	UU_REAL CMIN = 0.999969; /* 0.45 degrees */
	UU_REAL SMIN = 0.0078539; /* 0.45 degrees */
	UU_REAL sfi_xmmx[2],sfi_ymmx[2];
	UU_LOGICAL vertfl,baseon,lsolid;
	UM_vector *vv,nvec,vvi;
	UM_tess_settype tesstyp;
	int ncl_watcmp();
	UU_LOGICAL lv97,ltrmsf;
	UU_LIST uvtes,uvtrilst,*trianlst;
	UM_srf_bound *polybndr,*polybndr1;
	UM_int2 nbsf,npolys,j,n1;
	UU_REAL dtol;
	UU_LIST *polylst;
	UU_REAL box[6];

	S_set_btol (toler,&btol,&tesstyp);

	status = UU_SUCCESS;
	baseon = (wbase->key0 != NULLKEY);

	hmax = -1000000.; hmin = 1000000.;
	if (baseon)
	{
		hmax = hmin = 0.;
	}

	tolsq = 0.01*tol*tol; tolersq = 0.01*toler*toler;
	if (xmmx && ymmx)
	{
		if (baseon)
		{
			xmmx[0] = ymmx[0] = 0.;
			xmmx[1] = ymmx[1] = 1.;
		}
		else
		{
			xmmx[0] = ymmx[0] = 1000000.;
			xmmx[1] = ymmx[1] = -1000000.;
		}
	}

	ncl_set_boundary_toler (btol);
	um_set_tess_toler (btol);
	um_init_boundary (&bndr);
	uu_list_init (&cvlst, sizeof(UM_coord), 100, 200);
	uu_list_init (&uvlst, sizeof(UM_coord), 100, 200);
	if (!cvlst.data || !uvlst.data) status = UU_FAILURE;
	uu_list_init0 (&trilst); uu_list_init0(&dlist);
	uu_list_init (&cvls1, sizeof(UM_coord), 0, 200);
	uu_list_init (&uvls1, sizeof(UM_coord), 0, 200);
	bndr.uvpts = &uvlst;
	bndr.cvpts = &cvlst;
	um_init_tess(&tess);
	ncl_get_rotfl (&irot);

	box[0] = box[1] = box[2] = 100000;
	box[3] = box[4] = box[5] = -100000;	

	lv97 = ncl_setver(97);
	nbsf = 0;
	ltrmsf = UU_FALSE;
	iinit = nclc_tessmgr_check_init();
	if (!lv97)
	{
		iinit = nclc_tessmgr_check_init();
		if (iinit != UU_SUCCESS)
			nclc_tessmgr_init();
		else
			nbsf = nclc_tessmgr_get_nbsf();
	}

	if (NCL_waterline)
		ncl_wat_get_sfnums (&numa,&numb,&numc);
	else
	{
		numa = numsf;
		numb = numc = 0;
	}

	sfi = sff;
	for (isf = 0; isf < numsf && status == UU_SUCCESS; isf++, sfi++)
	{
		S_wsurf_init (sfi);
		if (isf >= numa)
		{
			if (numb > 0 && isf < numa+numb)
				sfi->slist = LIST_B;
			else if (numc > 0 && isf >= numa+numb)
				sfi->slist = LIST_C;
			else
			{
				numsf = isf;
				break;
			}
		}
		sfrevolv = 0;
		Suvswap = UU_FALSE;

		if (baseon && sfkey[isf] == wbase->key0)
		{
			sfi->key = wbase->key0;
			S_wbase_sf (sfi);
			continue;
		}
		sf.key = sfkey[isf];
		status = ncl_retrieve_data_fixed (&sf);
		if (status == UU_SUCCESS)
			status = uc_retrieve_transf (sf.key, tfmat);

		if (status != UU_SUCCESS) continue;

		lsolid = (sf.rel_num == UM_SOLID_REL);
		if (lsolid) goto Tess;

		ncl_free_bndry (&bndr);
		UU_LIST_EMPTY (bndr.uvpts); UU_LIST_EMPTY (bndr.cvpts);
		status = S_get_bound (&sf,tfmat,&bndr,&sfi->bound,btol);
		if (status != UU_SUCCESS || sfi->bound.nb < 1) continue;

		S_set_sfrevolv (&sf,tfmat,wbase,&sfrevolv,toler);
		if (sfrevolv > 0) Suvswap = UU_TRUE;
		if (sfrevolv == 2) sfi->flag1 = SHADOW_BOX;

		if (uvlst.max_cnt >= 4*uvlst.exp_cnt && isf <= numsf/2)
		{
			uvlst.exp_cnt *= 2; cvlst.exp_cnt *= 2;
		}

		if (*npt < sfi->bound.npo) *npt = sfi->bound.npo;

		if (!lv97)
		{
			sfi_xmmx[0] = sfi_ymmx[0] = 1000000.;
			sfi_xmmx[1] = sfi_ymmx[1] = -1000000.;
		}

		pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
		for (ib = 0; ib < sfi->bound.nb; ib++, pts += npts)
		{
			npts = (ib == 0)? sfi->bound.npo: sfi->bound.np[ib-1];
			for (i = 0; i < npts-1; i++)
			{					
				if (irot > 0) um_cctmtf(pts[i],rot,pts[i]);	
				if (!lv97)
					ncl_waterline_minmax(pts[i],&box[2],&box[5],
									sfi_xmmx,sfi_ymmx);
				ncl_wat_baseproj (wbase,pts[i],pts[i]);
			}
			um_vctovc (pts[0],pts[npts-1]);
		}

		if (!lv97)
		{
			box[0] = sfi_xmmx[0]; box[3] = sfi_xmmx[1];	
			box[1] = sfi_ymmx[0]; box[4] = sfi_ymmx[1];
		}

		S_weed_outer1 (&sfi->bound,tol);

Tess:
		sfi_xmmx[0] = sfi_ymmx[0] = 1000000.;
		sfi_xmmx[1] = sfi_ymmx[1] = -1000000.;
		sfi->key = sf.key;

		if (lv97)
		{
			if (status == UU_SUCCESS)
			{
				if (sfrevolv > 0)
					status = -1;
				else	
					status = ncl_get_tesslst (&sf,&tess);
				npts = 0;
				if (status != UU_SUCCESS)
				{
					if (lsolid)
					{
						status = ncl_get_solid_tess (&sf,&dlist,&tess,btol);
						sfi->flag1 = VSOLID;
					}
					else
					{
						status = ncl_tess_surf (&sf,tfmat,
							&bndr,&tess,btol,tesstyp,npts,npts);
					}
				}
			}


			if (status == UU_SUCCESS)
			{
				if (!trilst.data)
				{
					npts = tess.ntri;
					i = (npts > 200)? npts: 200;
					uu_list_init (&trilst,sizeof (UM_trian),i,i);
				}
				else
					UU_LIST_EMPTY (&trilst);

				status = ncl_get_tess_triangles (&tess,&trilst,2,0);
				npts = trilst.cur_cnt;

				if (lsolid)
				{
					i = npts/3 + 2;
					if (*npt < i) *npt = i;
				}

				if (status == UU_SUCCESS)
				{
					if (trilst.max_cnt >= 4*trilst.exp_cnt && isf <= numsf/2)
						trilst.exp_cnt *= 2;
					sfi->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));

					uu_list_init (sfi->trianlist,sizeof(UM_trian),npts,npts);
					uu_list_push_list (sfi->trianlist, &trilst);
				}
			}

			if (status != UU_SUCCESS) break;
			um_clean_tess (&tess);
		}
		else
		{
			isfkey = UU_FAILURE;
			ltrmsf = UU_FALSE;
/*
.....Check if the surface has been tessellated
*/
			isfkey = nclc_tessmgr_check_sfkey(sfi->key);
			if (isfkey != UU_SUCCESS)
			{
				nclc_tessmgr_create();
				status = nclc_tessmgr_tessellate(nbsf,&sf,tfmat,rot,irot,
											&bndr,btol,tesstyp);
				nbsf++;
			}

			if (status == UU_SUCCESS)
			{
				status = nclc_tessmgr_get_srf_polylst(sf.key,&polylst);	
				if (status == UU_SUCCESS)
				{
					npts = polylst->cur_cnt;
					if (lsolid)
					{
						i = npts/3 + 2;
						if (*npt < i) *npt = i;
					}

					if (polylst->max_cnt >= 4*polylst->exp_cnt && isf <= numsf/2)
						polylst->exp_cnt *= 2;
					sfi->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
					uu_list_init (sfi->trianlist,sizeof(UM_trian),npts,npts);
					uu_list_push_list (sfi->trianlist, polylst);
				}						
			}
		}/*end if(lv97)*/
		
		ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
		npts = sfi->trianlist->cur_cnt;
		for (i = 0; i < npts; i++, ptri++)
		{
			if (irot > 0) um_cctmtf(ptri->p1,rot,ptri->p1);
			ncl_wat_baseproj (wbase,ptri->p1,ptri->p1);
			ncl_waterline_minmax(ptri->p1,&sfi->zmin,&sfi->zmax,sfi_xmmx,
				sfi_ymmx);
			if (irot > 0) um_cctmtf(ptri->p2,rot,ptri->p2);
			ncl_wat_baseproj (wbase,ptri->p2,ptri->p2);
			ncl_waterline_minmax(ptri->p2,&sfi->zmin,&sfi->zmax,sfi_xmmx,
				sfi_ymmx);
			if (irot > 0) um_cctmtf(ptri->p3,rot,ptri->p3);
			ncl_wat_baseproj (wbase,ptri->p3,ptri->p3);
			ncl_waterline_minmax(ptri->p3,&sfi->zmin,&sfi->zmax,sfi_xmmx,
				sfi_ymmx);
		}
		if (lsolid) goto Box;

		if (sfi->zmax - sfi->zmin < tol)
		{
/*
..... detect and process "horizontal" surfaces
*/
			pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
			h = 0;
			npts = sfi->bound.npo;
			for (i = 0; i < npts; i++)
			{
				h = h + pts[i][2];
			}
			h = h/npts;
			sfi->zmax = sfi->zmin = h;
			sfi->sf_flag = HORZPL;
			UU_LIST_FREE (sfi->trianlist);

			ar0 = ari = 0.;
			for (ib = 0; ib < sfi->bound.nb; ib++, pts += npts)
			{
				npts = (ib == 0)? sfi->bound.npo: sfi->bound.np[ib-1];
				for (i = 2, area = 0; i < npts-1; i++)
				{
					area += um_triangle_signed_area (pts[0],pts[i],pts[i-1]);
				}
				if (ib == 0)
					ar0 = fabs(area);
				else
				{
					if (area < 0) area = -area;
/* mark inner boundaries too small for the cutter */
					if (area <= areamin) sfi->bound.np[ib-1] *= -1;
					ari += area;
				}
			}
			if (ar0 < ari + tolsq)
				sfi->flag1 = MANY_HOLES;
			else
				sfi->size = (ar0 - ari)*(ar0 - ari)/ar0;
		}
		else if (lv97)
		{
/*
..... detect "vertical plane" surfaces
*/
			if (!tess.normals.data)
			{
				npts = sfi->trianlist->cur_cnt;
				i = (npts > 100)? npts: 100;
				uu_list_init (&tess.normals,sizeof (UM_coord),i,i);
			}
			else	
				UU_LIST_EMPTY (&tess.normals);
			ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
			npts = sfi->trianlist->cur_cnt;
			um_nullvc (nvec);
			for (i = 0, vertfl = UU_TRUE; i < npts && vertfl; i++)
			{
				istat = um_3pt_pln (ptri[i].p1,ptri[i].p2,ptri[i].p3,vvi,&d);
				if (istat == -1)
				{
					uu_list_delete (sfi->trianlist,i,1);
					ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
					i--; npts--;
					continue;
				}
				if (fabs (vvi[2]) < SMIN)
				{
					uu_list_push (&tess.normals,vvi);
					um_vcplvc (nvec,vvi,nvec);
				}
				else
					vertfl = UU_FALSE;
			}

			if (vertfl)
			{
				um_unitvc (nvec,nvec);
				vertfl = (fabs(nvec[2]) < SMIN);
				if (vertfl)
				{
					nvec[2] = 0;
					vertfl = (fabs (nvec[0]) > CMIN);
					if (vertfl)
					{
						nvec[1] = 0;
						nvec[0] = 1;
					}
					else
					{
						vertfl = (fabs (nvec[1]) > CMIN);
						if (vertfl)
						{
							nvec[0] = 0;
							nvec[1] = 1;
						}
					}

					if (vertfl)
					{
						vv = (UM_vector *) UU_LIST_ARRAY(&tess.normals);
						for (i = 0, vertfl = UU_TRUE; i < npts && vertfl; i++)
						{
							d = UM_DOT (nvec,vv[i]);
							vertfl = (fabs (d) > CMIN);
						}
					}

					if (vertfl)
					{
						pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
						npts = sfi->bound.npo - 1;

						y0 = y1 = UM_DOT_2D (nvec,pts[0]);

						for (i = 1; i < npts; i++)
						{
							d = UM_DOT_2D (nvec,pts[i]);
							if (d < y0) y0 = d;
							if (d > y1) y1 = d;
						}
						dy = y1 - y0;
						vertfl = (dy < 4*UM_FUZZ);
					}

					if (vertfl)
					{
						sfi->sf_flag = VERTPL;
						for (k = 0; k < 2; k++) sfi->nvec[k] = nvec[k];
					}
				}
			}
		}

		if (sfi->sf_flag == GEN)
		{
/*
..... calculate ar0 - area of the outer boundary polygon, ari - total area of
..... inner boundaries
*/
				ar0 = ari = 0.;

				pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
				npts = sfi->bound.npo - 1;
				UU_LIST_EMPTY (&uvlst);
				ncl_test_projection1 (&sfi->sf_flag,npts,pts,&uvlst);
/*
..... if a surface 'overhangs' itself, e.g., a cylinder with a horizontal
..... axis, we estimate its area as one half of the sum of all triangles
*/
				if (lv97 && sfi->sf_flag == BADPROJ)
				{
/*					S_xymmx_vert (sfi,sfi_xmmx,sfi_ymmx,tol);
					if (sfi->sf_flag == VERTPL) goto Sizing; */
					ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
					npts = sfi->trianlist->cur_cnt;

					for (i = 0; i < npts; i++, ptri++)
					{
						ar0 += fabs (um_triangle_signed_area(ptri->p1,ptri->p2,
											ptri->p3));
					}
					ar0 /= 2;
				}
				else
				{
/*
..... rotate all boundaries if rotfl; use the outer boundary for min and max
*/
					for (ib = 0; ib < sfi->bound.nb; ib++, pts += npts)
					{
						npts = (ib == 0)? sfi->bound.npo: sfi->bound.np[ib-1];
						for (i = 0, area = 0; i < npts; i++)
						{
							if (i > 1 && i < npts-1)
								area += um_triangle_signed_area (pts[0],pts[i],
											pts[i-1]);
						}
						if (ib == 0)
							ar0 = fabs(area);
						else
						{
							if (area < 0) area = -area;
/* mark inner boundaries too small for the cutter */
							if (area <= areamin) sfi->bound.np[ib-1] *= -1;
							ari += area;
						}
					}
				}
				if (ar0 < ari + tolsq)
					sfi->flag1 = MANY_HOLES;
				else
					sfi->size = (ar0 - ari)*(ar0 - ari)/ar0;
		}

/* Sizing: */
		if (sfi->sf_flag == VERTPL)
		{
			dx = sfi_xmmx[1] - sfi_xmmx[0];
			dy = sfi_ymmx[1] - sfi_ymmx[0];
			d = sqrt(dx*dx + dy*dy);
			sfi->size = 0.01*d;
		}
		else
		{
			area = (sfi_xmmx[1] - sfi_xmmx[0])*(sfi_ymmx[1] - sfi_ymmx[0]);
			if (sfi->bound.nb > 2 && sfi->size < 0.05*area)
			{
/* a surface is flagged if it has "many holes" */
				for (ib = 1, i = 0; ib < sfi->bound.nb; ib++)
				{
					if (sfi->bound.np[ib-1] > 0) i++;
					if (i > 1)
					{
						sfi->flag1 = MANY_HOLES; break;
					}
				}
			}
		}
Box:
		if (sfi->zmax > hmax) hmax = sfi->zmax;
		if (sfi->zmin < hmin) hmin = sfi->zmin;
		if (xmmx && ymmx && !baseon)
		{
			if (sfi_xmmx[0] < xmmx[0]) xmmx[0] = sfi_xmmx[0];
			if (sfi_xmmx[1] > xmmx[1]) xmmx[1] = sfi_xmmx[1];
			if (sfi_ymmx[0] < ymmx[0]) ymmx[0] = sfi_ymmx[0];
			if (sfi_ymmx[1] > ymmx[1]) ymmx[1] = sfi_ymmx[1];
		}
		sfi->box.xmin = sfi_xmmx[0]; sfi->box.xmax = sfi_xmmx[1];
		sfi->box.ymin = sfi_ymmx[0]; sfi->box.ymax = sfi_ymmx[1];

	}/*end for (isf = 0;*/

	if (status == UU_SUCCESS)
	{
		if (numsf > 1)
		uu_qsort (sff,numsf,sizeof(NCL_waterline_surf),ncl_watcmp);
	}
	else if (isf < numsf)
	{
		if (sfi->key != NULLKEY)
		{
			sfi->key = NULLKEY; ncl_free_bound (&sfi->bound);
		}
		ncl_get_label(&sf,badsf);
	}

	if (status == UU_SUCCESS)
	{
		if (*npt > 3 && hmax >= hmin)
		{
			*zmax = hmax;
			*zmin = hmin;
		}
		else
			status = UU_FAILURE;
	}

	um_free_tess (&tess);

	if (trilst.data) uu_list_free (&trilst);
	ncl_free_bndry (&bndr);
	uu_list_free (&uvlst);
	uu_list_free (&cvlst);
	uu_list_free (&cvls1);
	uu_list_free (&uvls1);
	Suvswap = UU_FALSE;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_tesselate_sfs
**       Fill in various data for a collection of surfaces;
**       calculate global zmin and zmax
**    PARAMETERS
**       INPUT  :
**          numsf      - number of surfaces
**          sfkey      - surface keys
**          rot        - transformation matrix (from Unibase to part coordsys)
**          toler      - Unibase tolerance
**          tol        - part tolerance
**       OUTPUT :
**          zmax,zmin  - global Z-range for all surfaces
**          xmmx,ymmx  - global UV-range for all surfaces
**          sff        - data for each surface
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tesselate_sfs (numsf,sfkey,rot,zmax,zmin,sff,tol,toler,xmmx,ymmx)
int numsf;
UU_KEY_ID *sfkey;
UM_transf rot;
UU_REAL *zmax,*zmin,*xmmx,*ymmx,tol,toler;
NCL_waterline_surf *sff;
{
	int i,isf,status,npts,irot,numa,numb,numc;
	struct NCL_fixed_databag sf;
	UM_coord *pts;
	UU_REAL hmin,hmax,btol;
	UM_real8 tol1;
	NCL_waterline_surf *sfi;
	UM_transf tfmat;
	UM_tessellation tess;
	UM_trian *ptri;
	UM_srf_boundary bndr;
	UU_LIST cvlst,uvlst,trilst,dlist;
	UM_tess_settype tesstyp;
	int ib,iinit,isfkey;
	UU_LOGICAL lsolid;
	UU_REAL area,ari,ar0;
	UU_REAL tolsq,tolersq;
	UU_REAL sfi_xmmx[2],sfi_ymmx[2];
	int ncl_watcmp();
	UU_LOGICAL lmgr;
	UU_LIST uvtes,uvtrilst,*trianlst;
	UM_int2 isub, mm,nbsf;
	UU_REAL dtol,dx,dy,dz,dmax;

	UM_srf_bound *polybndr;
	UM_int2 npolys,j;
	UU_LIST *polylst;
	UU_REAL box[6];
	UM_3D_box tbox;
	char tbuf[400],label[NCL_MAX_LABEL_AND_SUBSCRIPT];
	int nc;

	S_set_btol (toler,&btol,&tesstyp);

	status = UU_SUCCESS;
	hmax = -1000000.; hmin = 1000000.;
	tol1 = (toler > 0.0005)? 2.*toler: 0.001;
	tolsq = 0.01*tol*tol; tolersq = 0.01*toler*toler;
	if (xmmx && ymmx)
	{
		xmmx[0] = ymmx[0] = 1000000.;
		xmmx[1] = ymmx[1] = -1000000.;
	}
	ncl_set_boundary_toler (btol);
	um_set_tess_toler (btol);
	um_init_boundary (&bndr);
	uu_list_init (&cvlst, sizeof(UM_coord), 100, 200);
	uu_list_init (&uvlst, sizeof(UM_coord), 100, 200);
	if (!cvlst.data || !uvlst.data) status = UU_FAILURE;
	uu_list_init0 (&trilst); uu_list_init0(&dlist);
	uu_list_init (&cvls1, sizeof(UM_coord), 0, 200);
	uu_list_init (&uvls1, sizeof(UM_coord), 0, 200);
	bndr.uvpts = &uvlst;
	bndr.cvpts = &cvlst;
	um_init_tess(&tess);
	ncl_get_rotfl (&irot);

	box[0] = box[1] = box[2] = 100000;
	box[3] = box[4] = box[5] = -100000;

	nbsf = 0;
	lmgr = UU_FALSE;
	iinit = nclc_tessmgr_check_init();
	if (iinit != UU_SUCCESS)
		nclc_tessmgr_init();
	else
		nbsf = nclc_tessmgr_get_nbsf();

	numa = numsf;
	numb = numc = 0;

	sfi = sff;
	for (isf = 0; isf < numsf && status == UU_SUCCESS; isf++, sfi++)
	{
		S_wsurf_init (sfi);
		if (isf >= numa)
		{
			if (numb > 0 && isf < numa+numb)
				sfi->slist = LIST_B;
			else if (numc > 0 && isf >= numa+numb)
				sfi->slist = LIST_C;
			else
			{
				numsf = isf;
				break;
			}
		}

		sf.key = sfkey[isf];
		status = ncl_retrieve_data_fixed (&sf);
		if (status == UU_SUCCESS)
			status = uc_retrieve_transf (sf.key, tfmat);
		if (status != UU_SUCCESS) continue;

		lsolid = (sf.rel_num == UM_SOLID_REL);
		if (lsolid) goto Tess;
		ncl_free_bndry (&bndr);
		UU_LIST_EMPTY (bndr.uvpts); UU_LIST_EMPTY (bndr.cvpts);
		status = S_get_bound (&sf,tfmat,&bndr,&sfi->bound,btol);

		if (status != UU_SUCCESS || sfi->bound.nb < 1) continue;

		if (uvlst.max_cnt >= 4*uvlst.exp_cnt && isf <= numsf/2)
		{
			uvlst.exp_cnt *= 2; cvlst.exp_cnt *= 2;
		}

		if (irot > 0)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (sfi->bound.cvpts);
			for (ib = 0; ib < sfi->bound.nb; ib++, pts += npts)
			{
				npts = (ib == 0)? sfi->bound.npo: sfi->bound.np[ib-1];
				for (i = 0; i < npts; i++)
					um_cctmtf(pts[i],rot,pts[i]);
			}
		}

		sfi_xmmx[0] = sfi_ymmx[0] = 1000000.;
		sfi_xmmx[1] = sfi_ymmx[1] = -1000000.;
		sfi->key = sf.key;

Tess:
		if (!lmgr)
		{
			sfi_xmmx[0] = sfi_ymmx[0] = 1000000.;
			sfi_xmmx[1] = sfi_ymmx[1] = -1000000.;
			sfi->key = sf.key;
			sfi->flag1 = VSOLID;

			status = ncl_get_tesslst (&sf,&tess);
			npts = 0;
			if (status != UU_SUCCESS)	
			{
				if (lsolid)		
					status = ncl_get_solid_tess (&sf,&dlist,&tess,btol);
				else									
					status = ncl_tess_surf (&sf,tfmat,
							&bndr,&tess,btol,tesstyp,npts,npts);
			}

			if (status == UU_SUCCESS)
			{
				if (!trilst.data)
				{
					npts = tess.ntri;
					i = (npts > 200)? npts: 200;
					uu_list_init (&trilst,sizeof (UM_trian),i,i);
				}
				else
					UU_LIST_EMPTY (&trilst);
				status = ncl_get_tess_triangles (&tess,&trilst,2,0);
				npts = trilst.cur_cnt;

				if (status == UU_SUCCESS)
				{
					if (trilst.max_cnt >= 4*trilst.exp_cnt && isf <= numsf/2)
						trilst.exp_cnt *= 2;
					sfi->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));

					uu_list_init (sfi->trianlist,sizeof (UM_trian),npts,npts);
					uu_list_push_list (sfi->trianlist, &trilst);
				}
			}
			if (status != UU_SUCCESS)
			{
				ncl_get_box(&sf,&tbox);
				dx = um_dcccc(tbox.ver[0],tbox.ver[1]);
				dy = um_dcccc(tbox.ver[0],tbox.ver[3]);
				dz = um_dcccc(tbox.ver[0],tbox.ver[4]);
/*
.....Ignore surfaces too small too tesselate and output an error
.....for surfaces that should tesselate - ASF 7/16/13.
*/
				if (dx+dy < 4.*tol || dx+dz < 4.*tol || dy+dz < 4.*tol)
				{
					sfi->size = 0.;
					sfi->key = NULLKEY;
					status = UU_SUCCESS;
					continue;
				}
				else
				{
					nc = NCL_MAX_LABEL_AND_SUBSCRIPT;
					if (sf.subscr > 0)
						sprintf(label,"%s(%d)",sf.label,sf.subscr);
					else
						strcpy(label,sf.label);
					ul_strip_blanks(&label,&nc);
					sprintf(tbuf,"Could not tesselate %s. Motion not generated.",
						label);
					ud_wrerr(tbuf);
					break;
				}
			}
			um_clean_tess (&tess);
		}
		else
		{
			isfkey = UU_FAILURE;
			lmgr = UU_FALSE;
/*
.....Check if the surface has been tessellated
*/
				isfkey = nclc_tessmgr_check_sfkey(sfi->key);
				if (isfkey != UU_SUCCESS)
				{
					lmgr = UU_TRUE;
					nclc_tessmgr_create();
					status = nclc_tessmgr_tessellate(nbsf,&sf,tfmat,
										rot,irot,&bndr,btol,tesstyp);
				}

/*
.....Get sfi_xmmx,sfi_ymmx for untrim surface from base tessellation
*/
				status = nclc_tessmgr_get_srf_polylst(sf.key,&polylst);	
				if (status == UU_SUCCESS)
				{
					npts = polylst->cur_cnt;

					if (polylst->max_cnt >= 4*polylst->exp_cnt && isf <= numsf/2)
						polylst->exp_cnt *= 2;
					sfi->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
					uu_list_init (sfi->trianlist,sizeof(UM_trian),npts,npts);
					uu_list_push_list (sfi->trianlist, polylst);
				}						
		}

		ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
		for (i = 0; i < npts; i++, ptri++)
		{
			if (irot > 0) um_cctmtf(ptri->p1,rot,ptri->p1);
			ncl_waterline_minmax(ptri->p1,&sfi->zmin,&sfi->zmax,sfi_xmmx,
				sfi_ymmx);
			if (irot > 0) um_cctmtf(ptri->p2,rot,ptri->p2);
			ncl_waterline_minmax(ptri->p2,&sfi->zmin,&sfi->zmax,sfi_xmmx,
				sfi_ymmx);
			if (irot > 0) um_cctmtf(ptri->p3,rot,ptri->p3);
			ncl_waterline_minmax(ptri->p3,&sfi->zmin,&sfi->zmax,sfi_xmmx,
				sfi_ymmx);
		}

		if (sfi->zmax > hmax) hmax = sfi->zmax;
		if (sfi->zmin < hmin) hmin = sfi->zmin;
		if (xmmx && ymmx)
		{
			if (sfi_xmmx[0] < xmmx[0]) xmmx[0] = sfi_xmmx[0];
			if (sfi_xmmx[1] > xmmx[1]) xmmx[1] = sfi_xmmx[1];
			if (sfi_ymmx[0] < ymmx[0]) ymmx[0] = sfi_ymmx[0];
			if (sfi_ymmx[1] > ymmx[1]) ymmx[1] = sfi_ymmx[1];
		}
		sfi->box.xmin = sfi_xmmx[0]; sfi->box.xmax = sfi_xmmx[1];
		sfi->box.ymin = sfi_ymmx[0]; sfi->box.ymax = sfi_ymmx[1];

/*
...	Get the surface boundary with tight tolerance
*/	
		if (lmgr)
		{
			S_get_btol(sfi->box,sfi->zmin,sfi->zmax,toler,&dtol);
			status = nclc_tessmgr_srf_bndr(nbsf,&sf,tfmat,dtol);
			nbsf++;
		}
	}/* end for (isf=0 */

	if (status == UU_SUCCESS)
	{
		if (numsf > 1)
		uu_qsort (sff,numsf,sizeof(NCL_waterline_surf),ncl_watcmp);
	}
	else if (isf < numsf)
	{
		if (sfi->key != NULLKEY)
		{
			sfi->key = NULLKEY; 
			ncl_free_bound (&sfi->bound);
		}
	}

	if (status == UU_SUCCESS)
	{
		if (hmax >= hmin)
		{
			*zmax = hmax;
			*zmin = hmin;
		}
		else
			status = UU_FAILURE;
	}

	um_free_tess (&tess);

	if (trilst.data) uu_list_free (&trilst);
	ncl_free_bndry (&bndr);
	uu_list_free (&uvlst);
	uu_list_free (&cvlst);
	uu_list_free (&cvls1);
	uu_list_free (&uvls1);
	
	return (status);
}
