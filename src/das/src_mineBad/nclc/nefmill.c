/*********************************************************************
**   NAME:  nefmill.c
**    CONTAINS:
**        ncl_fmcreate
**        ncl_fmill_isect_bndry
**        int ncl_fmill_set_sfs
**        ncl_fmgetuv
**        ncl_fmfin
**        ncl_delete_nkey
**        ncl_fml_uvlist_num
**        ncl_fmill_past
**        ncl_fml_normal
**        ncl_itsa_fml_base
**        ncl_get_fml_tonp
**        ncl_set_fml_tonp
**        ncl_set_fml_bskey
**        ncl_set_fml_nkey
**
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nefmill.c , 26.2
**    DATE AND TIME OF LAST MODIFICATION
**       04/10/18 , 15:16:36
*********************************************************************/

#include "ncldef.h"
#include "modef.h"
#include "mgeom.h"
#include "nccs.h"
#include "uminmax.h"
#include "mdeval.h"
#include "fmill.h"

static UU_LOGICAL Slretract = UU_FALSE;
static UU_LOGICAL Slboth = UU_FALSE;

static UU_LOGICAL Sfedge = UU_FALSE;
static UU_LOGICAL Sledge = UU_FALSE;

static int Sifwd = 0;
static int Slpast = 0;
static int Sltonp = 0;
static int Slnormal = 0;
static int Savoid = 0;
static int Savlast = 0;
static int Savdir = 0;
static int Sdelflg = 0;

static UU_KEY_ID Snkey = NULLKEY;
static UU_KEY_ID Sbskey = NULLKEY;

static UU_LIST Suvlist,Sptlst;
static int *Scutpt = UU_NULL;
static UM_coord *Suvptr;
static UU_LIST *Sretlist = UU_NULL;
static int *Sretracts = UU_NULL;
static int Sjret = 0;
static int *Sbnd = UU_NULL;
static int Sjbnd = 0;

static UU_REAL Stol,Seps;

#define DEL_START 1
#define DEL_END 2
#define DEL_BOTH 3

/* #define DBGFML */

static UU_LOGICAL S_same_endpts();
static UU_REAL S_dist_endpts();

/*********************************************************************
**    I_FUNCTION     : S_copy_isec (isec1,isec2)
*********************************************************************/
static void S_copy_isec (isec1,isec2)
struct NCL_isect *isec1,*isec2;
{
	isec2->ib = isec1->ib;
	isec2->jst = isec1->jst;
	isec2->jnd = isec1->jnd;
	isec2->pc[0] = isec1->pc[0];
	isec2->pc[1] = isec1->pc[1];
}

/*********************************************************************
**    E_FUNCTION     : ncl_delete_nkey()
*********************************************************************/
void ncl_delete_fml_nkey()
{
	if (Snkey != NULLKEY)
	{
		uc_delete (Snkey);
		Snkey = NULLKEY;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_fmfin()
**       FMILL finish.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fmfin()
{
	uu_list_free(&Suvlist);
	ncl_free_toolpts();
	UU_LIST_FREE (Sretlist);
	Sretracts = UU_NULL;
	UU_FREE (Sbnd);
	if (Snkey != NULLKEY)
	{
#ifndef DBGFML
		uc_delete (Snkey);
#endif
		Snkey = NULLKEY;
		Sbskey = NULLKEY;
	}
	sbsolv_insert = 0;
	evsf_ext_rst();

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_fmgetuv(ix,uv,iret)
**       Get FMILL uv values.
**    PARAMETERS
**       INPUT  :
**          ix     - Index of uv to get
**       OUTPUT :
**          uv     - Point.
**          iret   - Retract at this point iff 1.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fmgetuv(ix, uv, iret)
UM_int4 *ix;
UM_real8 uv[3];
UM_int2 *iret;
{
	int i = *ix-1;

	if (*iret >= 0)
	{
		if (Sretracts != UU_NULL && Sretracts[Sjret] == i)
		{
			*iret = 1; Sjret++;
			if (Sjret >= Sretlist->cur_cnt)
			{
				UU_LIST_FREE (Sretlist);
				Sretracts = UU_NULL;
			}
		}
		else if (Sbnd != UU_NULL && Sbnd[Sjbnd] == i)
		{
			*iret = 2; Sjbnd++;
		}
		else
			*iret = 0;
	}
	um_vctovc(Suvptr[i],uv);
	return;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_fmill_set_sfs (sky0,cky0,eptr,bsptr,ust,vst,
**                                                            tfmat,delkey)
**       Obtain the base and the trimmed surfaces, as needed.
**    PARAMETERS
**       INPUT  :
**          eptr		- pointer to surface record
**          bsptr		- pointer to base surface.
**          ust, vst	- u,v values near start point 
**          tfmat		- ID matrix of input surface
**			delkey		- UV-outer boundary curve key
**          sky0        - surface key
**          cky0        - curve key
**    RETURNS      :  UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fmill_set_sfs (sky0,cky0,eptr,bsptr,ust,vst,tfmat,delkey)
UU_KEY_ID sky0,*cky0,*delkey;
struct NCL_fixed_databag *eptr,*bsptr;
UU_REAL ust,vst;
UM_transf tfmat;
{
	struct NCL_trimsf_rec *tsf;

	UU_KEY_ID skey,bkey,ckey;
	UU_LOGICAL needtrim;
	int status;

	bkey = skey = sky0;
	ckey = *cky0;

	eptr->key = skey;
	status = ncl_retrieve_data_fixed (eptr);
	if (status == UU_SUCCESS)
		status = uc_retrieve_transf(skey,tfmat);

	if (status != UU_SUCCESS) return (status);

	needtrim = (ckey != NULLKEY && (Sltonp == TL_TO || Sltonp == TL_PAST));

	if (eptr->rel_num == NCL_TRIMSF_REL)
	{
		tsf = (struct NCL_trimsf_rec *)eptr;
		bkey = tsf->bs_key;
	}

	bsptr->key = bkey;
	status = ncl_retrieve_data_fixed (bsptr);

	if (status == UU_SUCCESS && needtrim)
	{
		status = ncl_create_trimsf (eptr,bkey,ust,vst,ckey,delkey);

		if (status == UU_SUCCESS)
		{
			*cky0 = NULLKEY;
			eptr->key = bkey; /* qar 95284 - need a key if ta/normal */
		}
		else
		{
			Sltonp = 0;
			status = UU_SUCCESS;
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION: S_check_push (pc)
**       Add a point to the list if different from last point.
*********************************************************************/
static void S_check_push (pc)
UM_2Dcoord pc;
{
	int n;
	UM_coord *uvs;
	UU_REAL d;

	n = Suvlist.cur_cnt;
	if (n > 0)
	{
		uvs = (UM_coord *) UU_LIST_ARRAY(&Suvlist);
		d = UM_SQDIS_2D (pc,uvs[n-1]);
		if (d < UM_DFUZZ)
			Suvlist.cur_cnt = n - 1;
	}
}

/*********************************************************************
**    I_FUNCTION     : void S_along_bndr (isec1,isec2,dirs,uvpts,nps)
**       Find the shortest route along the boundary between two boundary
**       intersections.
**    PARAMETERS
**       INPUT  :
**          isec1, isec2   - boundary intersections
**			dirs              - array to use
**          uvpts          - boundary points
**          nps            - number of boundary points
**       OUTPUT :
**          the shortest route is appended to Suvlist
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_along_bndr (isec1,isec2,dirs,uvpts,nps)
struct NCL_isect *isec1,*isec2;
UM_coord *uvpts;
int *nps,*dirs;
{
	int i,j,np,i1,j1,i2,j2,ifwd,ib,n0,jret,ilast,is1,is2,k;
	UM_coord *uvs;
	UU_REAL d1,d2;
	UU_LOGICAL lifted;

	ib = isec1->ib;
	for (i = 0, uvs = uvpts, n0 = 0; i < ib; i++) n0 += nps[i];

	uvs += n0;
	lifted = UU_FALSE;

	np = nps[ib]-1;
	i1 = isec1->jst; j1 = isec1->jnd;
	i2 = isec2->jst; j2 = isec2->jnd;

	if (i2 == j1)
	{
		uu_list_push (&Suvlist,uvs[j1]);
		if (Slboth) Scutpt[n0+j1] = 1;
		return;
	}
	if (i1 == j2)
	{
		uu_list_push (&Suvlist,uvs[i1]);
		if (Slboth) Scutpt[n0+i1] = 1;
		return;
	}

	ifwd = 0;
	if (Savdir == DIRSAME)
		ifwd = dirs[ib];
	else if (Savdir == DIRCCW)
		ifwd = 1;
	else if (Savdir == DIRCW)
		ifwd = -1;

	if (ifwd == 0)
	{
		ifwd = -1;
		d1 = UM_DIST_2D (isec1->pc, uvs[j1]);
		for (i = j1; i != i2; i = j)
		{
			j = um_mod (i+1,np);
			d1 += UM_DIST_2D (uvs[i], uvs[j]);
		}
		d1 += UM_DIST_2D (isec2->pc, uvs[i2]);

		d2 = UM_DIST_2D (isec1->pc, uvs[i1]);
		if (d2 >= d1) ifwd = 1;
		for (i = i1; i != j2 && ifwd == -1; i = j)
		{
			j = um_mod (i-1,np);
			d2 += UM_DIST_2D (uvs[i], uvs[j]);
			if (d2 >= d1) ifwd = 1;
		}
		if (ifwd == -1)
		{
			d2 += UM_DIST_2D (isec2->pc, uvs[j2]);
			if (d2 >= d1) ifwd = 1;
		}
		if (Savdir == DIRSAME) dirs[ib] = ifwd;
	}

	if (ifwd == 1)
	{
		is1 = j1; is2 = i2;
	}
	else
	{
		is1 = i1; is2 = j2;
	}

	S_check_push (uvs[is1]);


	if (Slboth)
	{
		if (Scutpt[n0+is1] == 1)
		{
			d1 = UM_SQDIS_2D (isec1->pc,uvs[is1]);
			if (d1 > UM_DFUZZ)
			{
				j = um_mod (is1-ifwd,np);
				if (Scutpt[n0+j] == 1)
				{
					jret = Suvlist.cur_cnt-1;
					uu_list_push (Sretlist,&jret);
					lifted = UU_TRUE;
				}
			}
		}

		for (i = is1, k = 0; k < np; i = j, k++)
		{
			j = um_mod (i+ifwd,np);

			if (!lifted)
			{
				if (Scutpt[n0+i] == 1)
				{
					jret = Suvlist.cur_cnt;
					uu_list_push (Sretlist,&jret);
					uu_list_push (&Suvlist,uvs[i]);
					ilast = i;
					lifted = UU_TRUE;
				}
				else
				{
					uu_list_push (&Suvlist,uvs[i]);
					Scutpt[n0+i] = 1;
				}
			}
			else
			{
				if (Scutpt[n0+i] == 1)
					ilast = i;
				else
				{
					uu_list_push (&Suvlist,uvs[ilast]);

					uu_list_push (&Suvlist,uvs[i]);
					Scutpt[n0+i] = 1;
					lifted = UU_FALSE;
				}
			}
			if (i == is2)
			{
				if (lifted)
				{
					j = um_mod (i+ifwd,np);
					if (Scutpt[n0+j] == 0)
					{
						uu_list_push (&Suvlist,uvs[ilast]);
						lifted = UU_FALSE;
					}
				}
				break;
			}
		}
	}
	else
	{
		for (i = is1, k = 0; k < np; i = j, k++)
		{
			j = um_mod (i+ifwd,np);
			uu_list_push (&Suvlist,uvs[i]);
			if (i == is2) break;
		}
	}

	if (Slboth)
	{
		d1 = UM_SQDIS_2D (isec1->pc,uvs[i1]);
		if (d1 < UM_DFUZZ) Scutpt[n0+i1] = 1;
		d1 = UM_SQDIS_2D (isec1->pc,uvs[j1]);
		if (d1 < UM_DFUZZ) Scutpt[n0+j1] = 1;
		d2 = UM_SQDIS_2D (isec2->pc,uvs[i2]);
		if (d2 < UM_DFUZZ) Scutpt[n0+i2] = 1;
		d2 = UM_SQDIS_2D (isec2->pc,uvs[j2]);
		if (d2 < UM_DFUZZ) Scutpt[n0+j2] = 1;
	}

	return;
}

/*********************************************************************
**    I_FUNCTION     : S_last_isect (k2,n2,isect)
**          Intersects trimmed surface boundary with an isoparametric
**          line. Everything is in u,v space (2D).
**    PARAMETERS
**       INPUT  :
**          k2      - current number of intersection
**          n2		- number of intersections
**          isect   - intersections
**       OUTPUT :
**          
**    RETURNS      :  TRUE/FALSE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_last_isect (k2,n2,isect)
int k2,n2;
struct NCL_isect *isect;
{
	int i,ib;

	ib = isect[k2].ib;
	for (i = k2+1; i < n2; i++)
	{
		if (isect[i].ib == ib) return (UU_FALSE);
	}

	return (UU_TRUE);
}

/*********************************************************************
**    I_FUNCTION     : S_around_bndr (isect,bound)
**          Add a pass around a boundary curve to the list.
**    PARAMETERS
**       INPUT  :
**          isect    - current boundary intersection
**          bound    - trimmed surface boundary.
**       OUTPUT : Suvlist updated
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_around_bndr (isect,bound)
struct NCL_isect *isect;
UM_srf_boundary *bound;
{
	int ib,i,j,k,npts,n0,ibnd,istep;
	UM_coord *uvs,*tp;
	UM_2Dcoord vlst,v0,v1;
	UU_REAL co0,co1,d;

	ib = isect->ib;

	for (i = 0, n0 = 0; i < ib; i++)
		n0 += bound->np[i];

	uvs = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	uvs += n0;

	ibnd = Suvlist.cur_cnt-1;
	Sbnd[Sjbnd++] = ibnd;

	istep = 1;
	n0 = isect->jnd;
	npts = bound->np[ib]-1;

	if (ibnd >= 1)
	{
		tp = (UM_coord *) UU_LIST_ARRAY (&Suvlist);
		for (k = 0; k < 2; k++)
			vlst[k] = tp[ibnd][k]-tp[ibnd-1][k];

		j = n0;

		for (k = 0; k < 2; k++)
			v0[k] = uvs[j][k] - tp[ibnd][k];

		d = UM_DOT_2D(v0,v0);
		if (d < UM_DFUZZ)
		{
			j = um_mod(n0+1,npts);
			for (k = 0; k < 2; k++)
				v0[k] = uvs[j][k] - tp[ibnd][k];
			d = UM_DOT_2D(v0,v0);
		}
		co0 = 0;
		if (d > UM_DFUZZ)
		{
			d = sqrt(d);

			for (k = 0; k < 2; k++)
			{
				v0[k] /= d;
				co0 += vlst[k]*v0[k];
			}
		}

		j = isect->jst;
		for (k = 0; k < 2; k++)
			v1[k] = uvs[j][k] - tp[ibnd][k];
		d = UM_DOT_2D(v1,v1);
		if (d < UM_DFUZZ)
		{
			j = um_mod(j-1,npts);
			for (k = 0; k < 2; k++)
				v1[k] = uvs[j][k] - tp[ibnd][k];
			d = UM_DOT_2D(v1,v1);
		}
		co1 = 0;
		if (d > UM_DFUZZ)
		{
			d = sqrt(d);

			for (k = 0; k < 2; k++)
			{
				v1[k] /= d;
				co1 += vlst[k]*v1[k];
			}
		}

		if (co1 > co0)
		{
			istep = -1;
			n0 = isect->jst;
		}
	}

	for (j = 0; j < npts; j++)
	{
		i = um_mod(n0+j*istep,npts);
		uu_list_push (&Suvlist,uvs[i]);
	}
	uu_list_push (&Suvlist,isect->pc);
	ibnd = Suvlist.cur_cnt-1;
	Sbnd[Sjbnd++] = ibnd;
}

/*********************************************************************
**    I_FUNCTION     : int S_get_npas (bsptr,tfmat,bound,tol,dst,ifwd)
**          Get the number of passes by the surface boundary and scallop height
**    PARAMETERS
**       INPUT  :
**          bsptr   - pointer to base surface.
**          tfmat   - pointer to transformation mx.
**          bound   - surface boundary.
**          dst     - distance between passes to satisfy scallop height
**          tol     - tolerance in XYZ space.
**          ifwd    - u v direction
**       OUTPUT : none
**    RETURNS      : number of passes
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_npas (bsptr,tfmat,bound,tol,dst,ifwd)
struct NCL_fixed_databag *bsptr;
UM_transf tfmat;
UM_srf_boundary *bound;
UU_REAL tol,dst;
int ifwd;
{
	int i,n1,npas,cvtyp,inc,ien,outpas;
	UU_REAL w,d1,d2,uvlim[2],rmx,ren;
	UU_LIST ptlist;
	UM_coord *ptmp;

	outpas = 0;
	ien = ren = 3;
	if (ncl_setver(101)) ien = 1;
	for (inc=0;inc<ien;inc++)
	{
		rmx = inc * (1./(ren-1.));
		npas = 0;

		uu_list_init(&ptlist,sizeof(UM_coord),100,100);

		if (ifwd > 0)
		{
			w = bound->vmmx[0][0] + (bound->vmmx[0][1]-bound->vmmx[0][0]) * rmx;
			uvlim[0] = bound->ummx[0][0];
			uvlim[1] = bound->ummx[0][1];
			cvtyp = 1;
		}
		else
		{
			w = bound->ummx[0][0] + (bound->ummx[0][1]-bound->ummx[0][0]) * rmx;
			uvlim[0] = bound->vmmx[0][0];
			uvlim[1] = bound->vmmx[0][1];
			cvtyp = 2;
		}

		n1 = ncl_evolve_crv_on_srf(bsptr,tfmat,w,uvlim,cvtyp,tol,&ptlist,UU_NULL,
			UU_NULL);

		if (n1 >= 2)
		{
			ptmp = (UM_coord *)UU_LIST_ARRAY(&ptlist);
			d1 = 0.0;
			for (i = 0; i < n1-1; i++)
			{
				d1 += um_dcccc(ptmp[i],ptmp[i+1]);
			}
			d2 = dst;
			if (d2 <= 0) d2 = d1;
			if (ncl_setver(95))
			{
				d1 /= d2;
				npas = d1;
				if (npas < d1) npas++;
				if (npas < 2) npas = 2;
			}
			else
			{
				if (d1 <= d2)
					npas = 1;
				else
				{
					d1 = d1/d2 + 1;
					npas = d1;
					if (npas < d1) npas++;
				}
			}
		}

		uu_list_free(&ptlist);
		if (npas > outpas) outpas = npas;
	}
	return (outpas);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_fmill_isect_bndry (w,cvtyp,bound,iflow,iobuf)
**          Intersects trimmed surface boundary with an isoparametric
**          line. Everything is in u,v space (2D).
**    PARAMETERS
**       INPUT  :
**          w      - constant u or v parameter.
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          bound    - trimmed surface boundary.
**       OUTPUT :
**          iobuf  - output buffer with intersection points (list).
**    RETURNS      :  number of intersection points in iobuf.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fmill_isect_bndry (w, cvtyp, bound, iflow, iobuf)
UU_REAL w;
int cvtyp,iflow;
UM_srf_boundary *bound;
UU_LIST *iobuf;
{
	int i,j,k,m,num,np1,kind,nb;
	struct NCL_isect isect, *isectp;
	UM_coord *pts;
	UU_REAL d0,d1,d2;
	int ib;
	UU_LOGICAL lpush;

	pts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);

	kind = 2 - cvtyp;
	m = 1 - kind;
/*
..... ignore inner boundaries if no AVOID behavior specified
*/
	if (!ncl_setver (95) && Savoid > 0)
		nb = bound->nb;
	else
		nb = 1;
/*
..... for v=uv=const (cvtyp=1): find polyline segments whose lower end is
..... below and upper end is above, and intersect them with the line.
..... similarly for cvtyp=2.
*/
	for (ib = 0, num = 0; ib < nb; ib++)
	{
		np1 = bound->np[ib]-1;
		for (i = 0; i < np1; i++)
		{
			j = um_mod (i-1,np1);
			d0 = pts[j][kind] - w;
			d1 = pts[i][kind] - w;
			d2 = pts[i+1][kind] - w;

			if ((pts[i][kind] > w && pts[j][kind] <= w)
				|| ( pts[i][kind] < w && pts[j][kind] >= w))
			{
				if (fabs(d1) < UM_FUZZ && d0*d2 > 0) continue;
				num++;
				while (fabs(pts[j][kind]-pts[i][kind]) < UM_DFUZZ)
					j = um_mod (--j,np1);

				isect.pc[kind] = w;
				isect.pc[m] = pts[i][m] + (w - pts[i][kind])*
					(pts[j][m] - pts[i][m])/(pts[j][kind]-pts[i][kind]);
				isect.jst = j;
				isect.jnd = i;
				isect.ib = ib;
				lpush = UU_TRUE;
				if (num > 1)
				{
					isectp = (struct NCL_isect *) UU_LIST_ARRAY(iobuf);
					for (k = 0; k < num-1; k++)
					{
						if (isectp[k].pc[m]*iflow > isect.pc[m]*iflow)
						{
							uu_list_insert(iobuf,k,&isect);
							lpush = UU_FALSE;
							break;
						}
					}
				}
				if (lpush) uu_list_push (iobuf,&isect);
			}
		}
		pts += (np1+1);
	}

	if (Savoid == THRU && num > 2)
	{
		uu_list_delete (iobuf,1,num-2); num = 2;
	}

	return(num);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_fmill_isect_bndry (w, cvtyp, bound, iflow, iobuf, start_point, end_point,tol)
**          Intersects trimmed surface boundary with an isoparametric
**          line. Everything is in u,v space (2D).
**    PARAMETERS
**       INPUT  :
**          w      - constant u or v parameter.
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          bnd    - trimmed surface boundary.
**			start_point, end_point	- uv coord of flowline start, end
**       OUTPUT :
**          iobuf  - output buffer with intersection points (list).
**    RETURNS      :  number of intersection points in iobuf.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fmill_isect_bndry_ends (w, cvtyp, bound, iflow, iobuf, start_point, end_point,tol)
UU_REAL w;
int cvtyp,iflow;
UM_srf_boundary *bound;
UU_LIST *iobuf;
UM_coord start_point, end_point;
UU_REAL tol;

{
	int i,j,k,m,num,np1,kind,nb;
	struct NCL_isect isect, *isectp;
	UM_coord *pts;
	UU_REAL d0,d1,d2;
	int ib;
	UU_LOGICAL lpush;

	pts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);

	kind = 2 - cvtyp;
	m = 1 - kind;
/*
..... ignore inner boundaries if no AVOID behavior specified
*/
	if (!ncl_setver (95) && Savoid > 0)
		nb = bound->nb;
	else
		nb = 1;
/*
..... for v=uv=const (cvtyp=1): find polyline segments whose lower end is
..... below and upper end is above, and intersect them with the line.
..... similarly for cvtyp=2.
*/
	for (ib = 0, num = 0; ib < nb; ib++)
	{
		np1 = bound->np[ib]-1;
		for (i = 0; i < np1; i++)
		{
			j = um_mod (i-1,np1);
			d0 = pts[j][kind] - w;
			d1 = pts[i][kind] - w;
			d2 = pts[i+1][kind] - w;

			if ((pts[i][kind] > w && pts[j][kind] <= w)
				|| ( pts[i][kind] < w && pts[j][kind] >= w))
			{
				if (fabs(d1) < UM_FUZZ && d0*d2 > 0) continue;
				//num++;
				while (fabs(pts[j][kind]-pts[i][kind]) < UM_DFUZZ)
					j = um_mod (--j,np1);

				isect.pc[kind] = w;
				isect.pc[m] = pts[i][m] + (w - pts[i][kind])*
					(pts[j][m] - pts[i][m])/(pts[j][kind]-pts[i][kind]);
				isect.jst = j;
				isect.jnd = i;
				isect.ib = ib;
				lpush = UU_TRUE;
				if ((fabs(isect.pc[m]-start_point[0])<tol)||(fabs(isect.pc[m]-end_point[0])<tol))
					num++;
				if (num > 1)
				{
					isectp = (struct NCL_isect *) UU_LIST_ARRAY(iobuf);
					for (k = 0; k < num-1; k++)
					{
						if (isectp[k].pc[m]*iflow > isect.pc[m]*iflow)
						{
							if ((fabs(isect.pc[m]-start_point[0])<tol)||(fabs(isect.pc[m]-end_point[0])<tol))
							{
							uu_list_insert(iobuf,k,&isect);
							lpush = UU_FALSE;
							break;
							}
						}
					}
				}
				if ((fabs(isect.pc[m]-start_point[0])<tol)||(fabs(isect.pc[m]-end_point[0])<tol))
					if (lpush) uu_list_push (iobuf,&isect);
			}
		}
		pts += (np1+1);
	}

	if (Savoid == THRU && num > 2)
	{
		uu_list_delete (iobuf,1,num-2); num = 2;
	}

	return(num);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_fmill_isect_bndry1 (w,cvtyp,bound,iflow,iobuf)
**          Intersects trimmed surface boundary with an isoparametric
**          line. Everything is in u,v space (2D). Try to shift the line
**          by a small amount to achieve an even number of intersections.
**    PARAMETERS
**       INPUT  :
**          w      - constant u or v parameter.
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          bnd    - trimmed surface boundary.
**       OUTPUT :
**          iobuf  - output buffer with intersection points (list).
**    RETURNS      :  number of intersection points in iobuf.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fmill_isect_bndry1 (w0, cvtyp, bound, iflow, iobuf)
UU_REAL w0;
int cvtyp,iflow;
UM_srf_boundary *bound;
UU_LIST *iobuf;
{
	int n,j,k,ipm,ix2;
	UU_REAL delw,w,dw;
	struct NCL_isect *isectp;

	n = ncl_fmill_isect_bndry (w0,cvtyp,bound,iflow,iobuf);
	if (n%2 == 0) return (n);

	ipm = -1;
	delw = UM_FUZZ;

	for (j = 0; j < 4; j++)
	{
		UU_LIST_EMPTY(iobuf);
		ipm = -ipm;
		k = j/2 + 1;
		dw = delw*k*ipm;
		w = w0 + dw;
		n = ncl_fmill_isect_bndry (w,cvtyp,bound,iflow,iobuf);
		if (n%2 == 0)
		{
			isectp = (struct NCL_isect *) UU_LIST_ARRAY(iobuf);
			ix2 = 2 - cvtyp;

			for (k = 0; k < n; k++)
			{
				isectp[k].pc[ix2] = w0;
			}

			return (n);
		}
	}

	UU_LIST_EMPTY(iobuf);

	return (0);
}

/*********************************************************************
**    I_FUNCTION     : int S_eval_flow (npts, cvtyp, w, uvlim, uvlist)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          w      - Constant u or v (depending on cvtyp).
**          uvlim  - Start and end u or v (depending on cvtyp).
**       OUTPUT :
**          uvlist - List of uv points.
**    RETURNS      : Number of uv points created (npts-2).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_eval_flow (npts, cvtyp, w, uvlim, uvlist)
int npts, cvtyp;
UU_REAL w, uvlim[2];
UU_LIST *uvlist;
{
	int i, j;
	UU_REAL delw, uv[3];

	uv[2] = 0.0;
	delw = (uvlim[1]-uvlim[0])/(npts-1);
	j = cvtyp-1;
	uv[j] = uvlim[0]+delw;
	uv[1-j] = w;
	for (i=0;i<npts-2;i++)
	{
		uu_list_push (uvlist,uv);
		uv[j] += delw;
	}

	return(npts-2);
}

/*********************************************************************
**    I_FUNCTION: S_evolve_crv_on_srf (eptr, tfmat, uv, vpr, cvtyp,
**                                       told, pptr, uptr)
**       Evolve isoparametric curve on a spline surface into set of points
**       with given chordal tolerance.
**       NOTE: Based on logic in ncl_evolve_curve_gen.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to surface record
**          tfmat  - ID matrix of input surface
**          uv     - constant parameter
**          vpr    - variable parameter limits
**          cvtyp  - 1 = u_curve, 2 = v_curve.
**          told   - tolerance (chord height)
**       OUTPUT :
**          pptr   - pointer to points list
**          uptr   - pointer to u/v parameter of V/U line
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_evolve_crv_on_srf (eptr,tfmat,uv,vpr,cvtyp,told,eps,pptr,uptr)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UU_REAL uv,told,eps,*vpr;
int cvtyp;
UU_LIST *pptr,*uptr;
{
	struct UM_rbsplsrf_rec *e1;
	struct UM_evcrvout evout;
	int nu, status;
	UU_REAL u,un,ulst;
	UM_coord uvp, ptsv;

	uvp[2-cvtyp] = uv;
	uvp[2] = uv;
	ulst = vpr[1];
	u = uvp[cvtyp-1] = vpr[0];
	e1 = (struct UM_rbsplsrf_rec *)eptr;

	status = um_ev9_crv (UM_ALL, uv, u, cvtyp, e1, tfmat, &evout);
	nu = 1;
	if (pptr != UU_NULL) uu_list_push (pptr,evout.cp);
	if (uptr != UU_NULL) uu_list_push (uptr,uvp);
	um_vctovc (evout.cp,ptsv);
/*
.....scan curve with given tolerance
*/
	while (u < ulst && status == UU_SUCCESS)
	{
		status =
			ncl_srf_nextpt_at_tol (eptr,tfmat,told,uv,u,cvtyp,&un,ulst,&evout,0);
		if (status == UU_SUCCESS)
		{
			if (un - u > 0.075)
			{
				un = u + 0.075;
				status = um_ev9_crv (UM_ALL, uv, un, cvtyp, e1, tfmat, &evout);
			}
			if (UM_SQDIS (ptsv,evout.cp) > eps || un > ulst-UM_DFUZZ)
			{
				nu++;
				if (pptr != UU_NULL) uu_list_push (pptr,evout.cp);
				uvp[cvtyp-1] = un;
				if (uptr != UU_NULL) uu_list_push (uptr,uvp);
				um_vctovc (evout.cp,ptsv);
			}
			u = un;
		}
	}
/*
...weed out points ???
*/
	if (status != UU_SUCCESS)
		nu = 0;

	return (nu);
}

/*********************************************************************
**    I_FUNCTION: S_evolve_flow (eptr,tfmat,param,isectp,nsec,w,tol,uptr)
**       Produce a flowline at a given parameter using one of three methods.
**       Dispatching routine.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to surface record
**          tfmat  - ID matrix of input surface
**          param   - cutting parameters struct
**			isectp	- intersections structure
**			nsec	- number of intersections
**          w      - constant parameter
**          tol    - tolerance (chord height)
**       OUTPUT :
**          uptr   - pointer to u/v parameter of V/U line
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_evolve_flow (eptr,tfmat,param,isectp,nsec,w,tol,uptr)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
NCL_fmlparam *param;
struct NCL_isect *isectp;
UU_REAL w,tol;
int nsec;
UU_LIST *uptr;
{
	int j,ix1,cvtyp,nppp,n1;
	UU_REAL uvlim[2];

	cvtyp = param->cvtyp;
	nppp = param->nppp;

	ix1 = cvtyp - 1;

	j = 0;
	if (param->flowdir < 0) j = 1;

	uvlim[j] = isectp[0].pc[ix1];
	uvlim[1-j] = isectp[nsec-1].pc[ix1];

	if (nppp > 0)
		n1 = S_eval_flow (nppp, cvtyp, w, uvlim, uptr);
	else if (Snkey != NULLKEY)
		n1 = S_evolve_crv_on_srf
			(eptr,tfmat,w,uvlim,cvtyp,Stol,Seps,NULLST,uptr);
	else
	{
		UU_LIST_EMPTY(&Sptlst);
		n1 = ncl_evolve_crv_on_srf
		     (eptr,tfmat,w,uvlim,cvtyp,tol,&Sptlst,UU_NULL,uptr);
	}

	return (n1);
}

/*********************************************************************
**    I_FUNCTION: S_add_wline (ix1,iflow,isectp,k1,k2,tp,js,je)
**       Add flowline points between two boundary intersections to the list.
**    PARAMETERS
**       INPUT  :
**          ix1    - 0 = u_curve, 1 = v_curve.
**          iflow  - 1 if forward, -1 if back
**          isectp - boundary intersections
**          k1     - first boundary intersection
**          k2     - second boundary intersection
**          tp     - flowline points
**          ntp    - number of flowline points
**       OUTPUT :
**          Suvlist   - global list updated.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_add_wline (ix1,iflow,isectp,k1,k2,uvlst,ntp)
int iflow,ix1,k1,k2,ntp;
struct NCL_isect *isectp;
UU_LIST *uvlst;
{
	int j,js,je;
	UU_REAL w1,w2,w;
	UM_coord *tp;

	if (ntp <= 0) return;
	tp = (UM_coord *) UU_LIST_ARRAY (uvlst);
	if (iflow > 0)
	{
		js = 1; je = ntp;
	}
	else
	{
		js = ntp; je = 1;
	}

	w1 = isectp[k1].pc[ix1]*iflow + UM_FUZZ;
	w2 = isectp[k2].pc[ix1]*iflow - UM_FUZZ;

	for (j = js; j*iflow <= je*iflow; j+=iflow)
	{
		w = tp[j-1][ix1];
		if (w*iflow > w1) break;
	}
	js = j;
	for (j = js; j*iflow <= je*iflow; j+=iflow)
	{
		w = tp[j-1][ix1];
		if (w*iflow > w2) break;
		tp[j-1][2] = 0;
		uu_list_push (&Suvlist,tp[j-1]);
	}
}

/*********************************************************************
**    I_FUNCTION: S_move_along_bndr (bound,w1,w2,ib,js,je)
**       Add flowline points between two boundary intersections to the list.
**    PARAMETERS
**       INPUT  :
**          ix1    - 0 = u_curve, 1 = v_curve.
**          iflow  - 1 if forward, -1 if back
**          isectp - boundary intersections
**          k1     - first boundary intersection
**          k2     - second boundary intersection
**          tp     - flowline points
**          ntp    - number of flowline points
**       OUTPUT :
**          Suvlist   - global list updated.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void	S_move_along_bndr (istep,iup,ix2,bound,w2,isec1,isec2,lmult)
int istep,iup,ix2;
UM_srf_boundary *bound;
UU_REAL w2;
struct NCL_isect *isec1,*isec2;
UU_LOGICAL lmult;
{
	UM_coord *uvpts;
	int knt,j,n1,jret,ilast,js,je;
	int i,ib;
	UU_REAL w1,d;
	UU_LOGICAL lifted;

	ib = isec1->ib;
	if (isec2->ib != ib) return;
	w1 = isec1->pc[ix2]; /* current flowline level */

	uvpts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);

	for (i = n1 = 0; i < bound->nb; i++)
	{
		uvpts += n1;
		n1 = bound->np[i];
		if (i == ib) break;
	}

	n1 = n1-1;

	j = isec1->jst;
	for (knt = 0; knt < n1; knt++)
	{
		if (uvpts[j][ix2]*iup > w1*iup) break;
		j = (j+istep+n1)%n1;
	}

	if (istep == 1)
	{
		js = isec2->jst;
		je = isec2->jnd;
	}
	else
	{
		js = isec2->jnd;
		je = isec2->jst;
	}

	d = UM_SQDIS_2D (isec2->pc,uvpts[js]);
	if (d < UM_DFUZZ) je = js;

	lifted = UU_FALSE;

	for (knt = 0; knt < n1; knt++)
	{
		if (j == je) break;

		if (Slboth)
		{
				if (!lifted)
				{
					if (Scutpt[j] == 1)
					{
						jret = Suvlist.cur_cnt;
						uu_list_push (Sretlist,&jret);
						uu_list_push (&Suvlist,uvpts[j]);

						ilast = j;
						lifted = UU_TRUE;
					}
					else
					{
						uu_list_push (&Suvlist,uvpts[j]);
						Scutpt[j] = 1;
					}
				}
				else
				{
					if (Scutpt[j] == 1)
						ilast = j;
					else
					{
						uu_list_push (&Suvlist,uvpts[ilast]);
						uu_list_push (&Suvlist,uvpts[j]);
						Scutpt[j] = 1;
						lifted = UU_FALSE;
					}
				}
		}
		else
			uu_list_push (&Suvlist,uvpts[j]);

		j = (j+istep+n1)%n1;
	}

	if (lifted)
	{
		uu_list_push (&Suvlist,uvpts[ilast]);
		lifted = UU_FALSE;
	}
}

/*********************************************************************
**    I_FUNCTION     : UU_LOGICAL S_check_edge (npt,ix,iy,pts,w0,pm)
**       Determine if a surface boundary has a single segment intersection
**       with a boundary box edge
*********************************************************************/
UU_LOGICAL S_check_edge (npt,ix,iy,pts,w0,pm)
int npt,ix,iy;
UM_coord *pts;
UU_REAL w0,pm;
{
	int i,i0,i1,j,in,ip,ni;
	UU_REAL dx,dy;

	i0 = -1;
	for (i = 0; i < npt; i++)
	{
		ip = (i - 1 + npt)%npt;
		if (pm*pts[i][iy] < pm*w0 + UM_FUZZ)
		{
			i0 = i;
			if (pm*pts[ip][iy] >= pm*w0 + UM_FUZZ)
				break;
		}
	}
	if (i0 >= 0)
	{
		i1 = i0;
		for (j = 1; j < npt; j++)
		{
			in = (i0 + j)%npt;
			if (pm*pts[in][iy] >= pm*w0 + UM_FUZZ)
				break;
			i1 = in;
		}

		ni = (i1 - i0 + npt)%npt;
		if (ni > 0)
		{
			dx = fabs (pts[i1][ix] - pts[i0][ix]);
			if (dx < 0.005) return (UU_FALSE);

			for (j = 1; j < npt-ni; j++)
			{
				i = (i1 + j)%npt;
				dy = fabs(pts[i][iy] - w0);
				if (dy < 5*UM_FUZZ)
					return (UU_FALSE);
			}

			dx = fabs(pts[i0][ix] - pts[ip][ix]);
			dy = fabs(pts[i0][iy] - pts[ip][iy]);
			if (dx/dy < 1)
			{
				dx = fabs(pts[in][ix] - pts[i1][ix]);
				dy = fabs(pts[in][iy] - pts[i1][iy]);
				if (dx/dy < 1)
					return (UU_TRUE);
			}
		}
	}

	return (UU_FALSE);
}

/*********************************************************************
**    I_FUNCTION     : void ncl_fmill_check_edges (np,ummx,vmmx,bound,itis)
**       Determine if a surface boundary has single segment intersections
**       with boundary box edges
*********************************************************************/
void ncl_fmill_check_edges (np,ummx,vmmx,bound,itis)
int itis,*np;
UM_srf_boundary *bound;
UM_2Dcoord *ummx,*vmmx;
{
	int npt,ix,iy;
	UM_coord *pts;
	UU_REAL w0,w1,pm;

	if (Sltonp != TL_TO && Sltonp != TL_PAST) return;

	if (itis == 0)
	{
		Sfedge = Sledge = UU_TRUE;
	}
	else
	{
		npt = np[0] - 1;
		pts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
		if (Sifwd == 0)
		{
			w0 = vmmx[0][0];
			w1 = vmmx[0][1];
			ix = 0; iy = 1;
		}
		else
		{
			w0 = ummx[0][0];
			w1 = ummx[0][1];
			ix = 1; iy = 0;
		}
		if (w1 - w0 < 0.005) return;
		pm = 1;
		Sfedge = S_check_edge (npt,ix,iy,pts,w0,pm);
		pm = -1;
		Sledge = S_check_edge (npt,ix,iy,pts,w1,pm);
	}
}

/*********************************************************************
**    I_FUNCTION     : int S_get_edge (iup,cvtyp,iflow,bound,w0,dwmax,iolst)
**       Put boundary points along a surface edge into a list
*********************************************************************/
static int S_get_edge (iup,cvtyp,iflow,bound,w0,dwmax,iolst)
int iup,cvtyp,iflow;
UU_REAL w0,dwmax;
UM_srf_boundary *bound;
UU_LIST *iolst;
{
	int npt,ix,iy,ifl,i,j,i0,i1,in,ip;
	UM_coord *pts;
	UU_REAL pm,dx,dy;
	struct NCL_isect isect;

	npt = bound->np[0] - 1;
	pts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);

	isect.ib = 0;

	ix = cvtyp - 1;
	iy = 1 - ix;
	pm = iup;
	ifl = iup*iflow;
	if (cvtyp == 2) ifl = -ifl;

	i0 = -1;
	for (i = 0; i < npt; i++)
	{
		if (pm*pts[i][iy] < pm*w0 + UM_FUZZ)
		{
			i0 = i;
			break;
		}
	}

	if (i0 < 0) return (-1);

	for (j = 1; j < npt; j++)
	{
		ip = (i0 - ifl + npt)%npt;

		dx = fabs(pts[ip][ix] - pts[i0][ix]);
		dy = fabs(pts[ip][iy] - w0);
		if (dy > 5*UM_FUZZ && dx/dy < 1)
			break;
		else
		{
			if (dy > dwmax) return (-1);
			i0 = ip;
		}
	}
	if (j == npt) return (-1);

	i1 = i0;
	for (j = 1; j < npt; j++)
	{
		isect.jst = i1;
		isect.jnd = (i1 + 1)%npt;
		isect.pc[ix] = pts[i1][ix];
		isect.pc[iy] = pts[i1][iy];
		uu_list_push (iolst,&isect);

		in = (i1 + ifl + npt)%npt;

		dx = fabs(pts[in][ix] - pts[i1][ix]);
		dy = fabs(pts[in][iy] - w0);
		if (dy > 5*UM_FUZZ && dx/dy < 1)
			break;
		else
		{
			if (dy > dwmax) return (-1);
			i1 = in;
		}
	}

	if (i1 == i0)
		return (-1);
	else
		return (0);
}

/*********************************************************************
**    I_FUNCTION     : S_dis (w,pm,cvtyp,bound,iolst,uvst,dis)
**       Determine the minimal distance from the boundary local extrema
**       between the boundary intersections to the starting point.
********************************************************************/
static void S_dis (w,pm,cvtyp,bound,iolst,uvst,dis)
UU_REAL w,pm;
int cvtyp;
UM_srf_boundary *bound;
UU_LIST *iolst;
UU_REAL *dis;
UM_2Dcoord uvst;
{
	int i,imin,j,jst,jnd,k,num,np;
	struct NCL_isect *isec;
	UM_coord *pts;
	UU_REAL di,dj,dst,dmin;
	int ix1,ix2;

	ix1 = cvtyp - 1;
	ix2 = 1 - ix1;

	isec = (struct NCL_isect *)UU_LIST_ARRAY(iolst);
	num = iolst->cur_cnt;

	pts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	np = bound->np[0] - 1;

	dmin = 100000;

	for (i = 0; i < num; i+=2)
	{
		jst = isec[i].jst;
		if (i + 1 > num - 1) break;
		jnd = isec[i+1].jnd;

		di = pm*(pts[jst][ix2] - w);
		imin = jst;

		for (k = 1; k < np; k++)
		{
			j = (jst + k + np)%np;
			dj = pm*(pts[j][ix2] - w);
			if (dj < di)
			{
				di = dj;
				imin = j;
			}
			if (j == jnd) break;
		}
		dst = UM_SQDIS_2D (uvst,pts[imin]);
		if (dst < dmin) dmin = dst;
	}

	*dis = dmin;

	return;
}

/*********************************************************************
**    I_FUNCTION     : UU_LOGICAL S_have_a_double (isec,is,num,j1,j2)
**       Determine if an intersection already has a double
********************************************************************/
UU_LOGICAL S_have_a_double (isec,is,num,j1,j2)
struct NCL_isect *isec;
int is,num,j1,j2;
{
	UU_LOGICAL lfound = UU_FALSE;

	if (is > 0)
	{
		lfound = (isec[is-1].jst == j1 || isec[is-1].jst == j2);
	}
	if (!lfound && is+1 < num)
	{
		lfound = (isec[is+1].jst == j1 || isec[is+1].jst == j2);
	}

	return (lfound);
}

/*********************************************************************
**    I_FUNCTION     : int S_isec_lines (j0,j1,pts,ix,iy,xsec,ysec)
**       Intersect the line between two boundary points with a flowline
********************************************************************/
static int S_isec_lines (j0,j1,pts,ix,iy,xsec,ysec)
int j0,j1,ix,iy;
UU_REAL *xsec,ysec;
UM_coord *pts;
{
	UU_REAL x0,y0,x1,y1,dy;

	x0 = pts[j0][ix]; x1 = pts[j1][ix];
	y0 = pts[j0][iy]; y1 = pts[j1][iy];
	dy = y1 - y0;

	if (fabs (dy) < UM_DFUZZ)
		return (-1);

	*xsec = x0 + (x1 - x0)*(ysec - y0)/dy;
	return (0);
}

/*********************************************************************
**    I_FUNCTION     : UU_LOGICAL S_double_cusp (pm,cvtyp,bound,iolst,is,num)
**       Double an intersection if it is a cusp
********************************************************************/
UU_LOGICAL S_double_cusp (pm,cvtyp,bound,iolst,is,num)
UU_REAL pm;
int cvtyp,is,num;
UM_srf_boundary *bound;
UU_LIST *iolst;
{
	int j,np,nb,ix1,ix2,n1,jst,jnd,j1,jprev;
	struct NCL_isect isect,*isec;
	UM_coord *pts;
	UU_REAL w,ds,dn,d1,uv,uvcusp;
	int ib;
	UU_LOGICAL lcusp;

	lcusp = UU_FALSE;
	ix1 = cvtyp - 1;
	ix2 = 1 - ix1;

	isec = (struct NCL_isect *)UU_LIST_ARRAY(iolst);

	ib = isec[is].ib;
	nb = bound->nb;
	pts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);

	for (j = np = 0; j <= ib; j++)
	{
		pts += np;
		np = bound->np[j];
	}
	n1 = np - 1;

	w = isec[is].pc[ix2];
	jst = isec[is].jst; jnd = isec[is].jnd;

	ds = pm*(pts[jst][ix2] - w);
	dn = pm*(pts[jnd][ix2] - w);

	if (ds <= 0)
	{
		j1 = (jst - 1 + n1)%n1;
		d1 = pm*(pts[j1][ix2] - w);
		if (dn > 0.0005 && d1 > 0.0005)
		{
			jprev = j1;
			if (!S_have_a_double (isec,is,num,jprev,jst))
				lcusp = UU_TRUE;
		}
	}
	else if (dn <= 0)
	{
		j1 = (jnd + 1 + n1)%n1;
		d1 = pm*(pts[j1][ix2] - w);
		if (ds > 0.0005 && d1 > 0.0005)
		{
			jprev = jst;
			jst = jnd; ds = dn;
			jnd = j1;
			if (!S_have_a_double (isec,is,num,jprev,jst))
				lcusp = UU_TRUE;
		}
	}

	if (lcusp)
	{
		uvcusp = pts[jst][ix1];
		isec[is].jst = jst;
		isec[is].jnd = jnd;
		isect.ib = ib;
		isect.pc[ix2] = w;

		if (ds < -UM_DFUZZ)
		{
			isect.jst = jprev;
			isect.jnd = jst;
			uv = uvcusp;
			S_isec_lines (jprev,jst,pts,ix1,ix2,&uv,w);
			isect.pc[ix1] = uv;
			uv = uvcusp;
			S_isec_lines (jst,jnd,pts,ix1,ix2,&uv,w);
			isec[is].pc[ix1] = uv;
		}
		else
		{
			isect.jst = jst;
			isect.jnd = jnd;
			isect.pc[ix1] = isec[is].pc[ix1] = uvcusp;
		}

		uu_list_insert (iolst,is,&isect);
	}

Done:
	return(lcusp);
}

/*********************************************************************
**    I_FUNCTION     : int S_double_cusps (pm,cvtyp,bound,iolst)
**       Double singular intersections on boundary cusps
*********************************************************************/
int S_double_cusps (pm,cvtyp,bound,iolst)
UU_REAL pm;
int cvtyp;
UM_srf_boundary *bound;
UU_LIST *iolst;
{
	int i,num;
	UU_LOGICAL ldoubled;

	num = iolst->cur_cnt;

	for (i = 0; i < num; i++)
	{
		ldoubled = S_double_cusp (pm,cvtyp,bound,iolst,i,num);
		if (ldoubled)
		{
			i++; num++;
		}
	}

	return (num);
}

/*********************************************************************
**    I_FUNCTION     : int S_find_edges (param,bound,iolst)
**       Calculate first and last cutting levels
**    PARAMETERS
**       INPUT  :
**          bound   - surface boundary
**          param   - cutting parameters struct
**          iolst   - initialized list to use
**       OUTPUT :  param updated
**    RETURNS      :  UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_find_edges (param,bound,ust,vst,iolst)
NCL_fmlparam *param;
UM_srf_boundary *bound;
UU_REAL ust,vst;
UU_LIST *iolst;
{
	UU_REAL w[2],wj,delw,del,wb,pm;
	UU_REAL wst,wnd,dmin,dis;
	int cvtyp,iup,ix1,status,i,nj,iflow,ist,itim;
	UM_2Dcoord uvst;
	UU_LOGICAL found;

	status = UU_FAILURE;

	cvtyp = param->cvtyp;
	iup = param->iup;

	ist = -1;
	if (iup == 1 && !Sfedge)
		ist = 0;
	else if (iup == -1 && !Sledge)
		ist = 1;

	wst = param->wst;
	wnd = param->wnd;

	if (iup > 0)
	{
		w[0] = wst;
		w[1] = wnd;
	}
	else
	{
		w[1] = wst;
		w[0] = wnd;
	}

	delw = 0.01*(w[1] - w[0]);

	iflow = 1;
	ix1 = cvtyp - 1;

	uvst[0] = ust; uvst[1] = vst;

	w[0] = w[0] + 0.0001;
	w[1] = w[1] - 0.0001;

	for (i = 0, pm = 1; i < 2; i++, pm=-pm)
	{
		found = UU_FALSE;

		for (itim = 0, del = 0; del < delw; itim++, del += UM_FUZZ)
		{
			wj = w[i] + pm*del;
			UU_LIST_EMPTY(iolst);
			nj = ncl_fmill_isect_bndry (wj,cvtyp,bound,iflow,iolst);

			nj = S_double_cusps (pm,cvtyp,bound,iolst);

			if (nj < 2 || nj%2 == 1) continue;

			if (i == ist)
			{
				S_dis (wj,pm,cvtyp,bound,iolst,uvst,&dis);
				if (!found)
				{
					dmin = dis; wb = wj;
					found = UU_TRUE;
				}
				else if (dis < dmin)
				{
					dmin = dis; wb = wj;
				}
				if (itim >= 9) break;
			}
			else
			{
				found = UU_TRUE;
				wb = wj;
				break;
			}
		}

		if (!found) return (status);
		w[i] = wb;
	}

	status = UU_SUCCESS;
	if (iup > 0)
	{
		param->wfrs = w[0];
		param->wlst = w[1];
	}
	else
	{
		param->wfrs = w[1];
		param->wlst = w[0];
	}

	param->delw = (wnd - wst)/(param->npas-1);

	return (status);
}

/*********************************************************************
**    I_FUNCTION: S_avoid (bound,ix1,iflow,isectp,nsec,uvlst,ntp,dirs,wnext)
**       Add flowline points between two boundary intersections to the list.
**    PARAMETERS
**       INPUT  :
**          bound  - surface boundary
**          ix1    - 0 = u_curve, 1 = v_curve.
**          iflow  - 1 if forward, -1 if back
**          isectp - boundary intersections
**          nsec   - number of intersections
**          uvlst  - list of uv points
**          ntp    - number of flowline points
**          dirs   - array to use
**          wnext  - constant  u or v parameter
**       OUTPUT :
**          Suvlist   - global list updated.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_avoid (bound,ix1,iflow,isectp,nsec,uvlst,ntp,dirs,wnext)
UM_srf_boundary *bound;
int iflow,ix1,nsec,ntp;
int *dirs;
struct NCL_isect *isectp;
UU_LIST *uvlst;
UU_REAL wnext;
{
	int jret,k1,k2,ib;
	int *nps;
	UM_coord *uvpts;

	nps = bound->np;
	uvpts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);

	k1 = 0; k2 = 1;
	while (k2+2 < nsec)
	{
		if (Sretlist != UU_NULL && Slretract && !Slboth)
		{
			jret = Suvlist.cur_cnt - 1;
			uu_list_push (Sretlist,&jret);
		}

		k1 += 2;
		if (Savoid == DOWN || Slboth)
			S_along_bndr (&isectp[k2],&isectp[k1],dirs,uvpts,nps);

		if (Savdir == DIRSAME)
		{
			for (ib = 0; ib < bound->nb; ib++)
				dirs[ib] = -dirs[ib];
		}

		S_check_push (isectp[k1].pc);
		uu_list_push (&Suvlist,isectp[k1].pc);

		if (Savlast == 1 && isectp[k1].ib > 0)
		{
			if (ncl_fml_outside_box (wnext,isectp[k1].ib,ix1,bound) &&
										S_last_isect (k1,nsec,isectp))
				S_around_bndr (&isectp[k1],bound);
		}

		k2 += 2;
		S_add_wline (ix1,iflow,isectp,k1,k2,uvlst,ntp);

		uu_list_push (&Suvlist,isectp[k2].pc);
	}
}

/*********************************************************************
**    I_FUNCTION     : void S_cut1a (bsptr,tfmat,bound,param,tol,
**                                       uvlst,iolst,dirs,dia)
**       Create fmill uv points.
**    PARAMETERS
**       INPUT  :
**          bsptr   - pointer to base surface.
**          tfmat   - pointer to transformation mx.
**          bound   - surface boundary
**          param   - cutting parameters struct
**          tol     - tolerance in XYZ space.
**          ptlst,uvlst,iolst - initialized lists to use
**          dirs    - array to use
**          dia     - tool diameter
**       OUTPUT :  Suvlist - the cut UV points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_cut1a (bsptr,tfmat,bound,param,tol,uvlst,iolst,dirs,dia)
struct NCL_fixed_databag *bsptr;
UM_transf tfmat;
UM_srf_boundary *bound;
NCL_fmlparam *param;
UU_REAL tol, dia;
UU_LIST *uvlst,*iolst;
int *dirs;
{
	UU_REAL w,wnext,pm,dwmax,ttol;
	int cvtyp,iup,istep,ix1,ix2,iver;
	int i,n1,n2,kfin,k1,k2,onedge,istat,k;
	int nppp,npas,ib;
	UU_LOGICAL lmult,last_isec,lastw,flowsame,ver98;
	UU_LOGICAL not_coin, to_yes, past_yes;
	struct NCL_isect isecfin,*isectp;

	int skipping;
	int np;
	UM_coord *pts;
	UM_coord start_point, end_point;

	ver98 = ncl_setver(98);
	cvtyp = param->cvtyp;
	iup = param->iup;
	npas = param->npas;
	nppp = param->nppp;
	istep = param->stepdir;
	ix1 = cvtyp - 1;
	ix2 = 1-ix1;
/*
..... when cvtyp = 1, ix1 is U, ix2 is V; when cvtyp = 2, ix1 is V, ix2 is U.
..... flowdir is the current flowline (ix1),direction, +1 if forward, -1 if backward.
..... iup is the permanent direction in ix2.
..... istep is the current outer boundary direction: +1 if forward, -1 if backward.
..... istep = iflow*iup if cvtyp = 1; and -iflow*iup if cvtyp = 2
*/
	last_isec = lastw = UU_FALSE;
	onedge = 0;
/*
.....Create first boundary intersection and flowline
*/
	w = param->wfrs;
	UU_LIST_EMPTY(iolst);
/*
.....begin erase the first pass
*/
	if (Sdelflg == DEL_START || Sdelflg == DEL_BOTH)
		w = w + param->delw;

	if (npas > 1)
	{
		dwmax = 0.01*fabs (param->wnd - param->wst);

		if (iup == 1 && Sfedge || iup == -1 && Sledge)
		{
			istat = S_get_edge (iup,cvtyp,param->flowdir,bound,param->wst,dwmax,iolst);
			if (istat == 0)
			{
				onedge = 1;
				n2 = 2;
			}
			else
				UU_LIST_EMPTY(iolst);

		}
		if (onedge == 0)
		{
			n2 = ncl_fmill_isect_bndry (w,cvtyp,bound,param->flowdir,iolst);
			pm = iup;
			n2 = S_double_cusps (pm,cvtyp,bound,iolst);
		}
	}
	else
		n2 = ncl_fmill_isect_bndry1 (w,cvtyp,bound,param->flowdir,iolst);
	if (n2 < 2) return;
	isectp = (struct NCL_isect *) UU_LIST_ARRAY(iolst);

	k1 = 0;
	k2 = 1;
	lmult = (n2 > 2);

	if (onedge == 0)
		n1 = S_evolve_flow (bsptr,tfmat,param,isectp,n2,w,tol,uvlst);
	else
		n1 = iolst->cur_cnt;

	if (npas > 1) 
	{
		if (Sdelflg == DEL_START || Sdelflg == DEL_BOTH)	
			w = param->wst + param->delw;
		else	
			w = param->wst;
	}

	if (Sdelflg == DEL_START || Sdelflg == DEL_END)
		npas = npas - 1;
	else if (Sdelflg == DEL_BOTH)
		npas = npas - 2;
/*
.....Loop outputting points.
*/
	for (i = 1; i <= npas; i++)
	{
		if (onedge == 0)
		{
			uu_list_push (&Suvlist,isectp[k1].pc);
			S_add_wline (ix1,param->flowdir,isectp,k1,k2,uvlst,n1);
		}
		else
		{
			for (k = 0; k < n1-1; k++)
			uu_list_push (&Suvlist,isectp[k].pc);
			k2 = n1 - 1;
			onedge = 0;
		}

		lastw = (i == npas-1);
		if (lastw)
		{
			if (Sdelflg == DEL_END || Sdelflg == DEL_BOTH)
				wnext = param->wlst-param->delw;
			else
				wnext = param->wlst;
		}
		else if (i == npas)
			wnext = 1.0;
		else
			wnext = w + param->delw;
		if (wnext > 1.0) wnext = 1.0;
		if (wnext < 0.0) wnext = 0.0;

		kfin = k1;
		if (n2 > 1)
		{
			uu_list_push (&Suvlist,isectp[k2].pc);
			if (lmult && Savoid > 0)
			{
				kfin = n2 - 1;
				S_avoid (bound,ix1,param->flowdir,isectp,n2,uvlst,n1,dirs,wnext);
			}
			else
				kfin = k2;
		}

		S_copy_isec (&isectp[kfin],&isecfin);

		if (i == npas)
		{
			if (Savlast == 1)
			S_around_bndr (&isecfin,bound);

			break;
		}
/*
.....If flowine is closed
.....Then keep motion going in the same direction
*/
		
		if (UU_LIST_LENGTH(uvlst)>0)
		{
			
			ttol = S_dist_endpts(bsptr,uvlst);
			if (ttol>tol)
				not_coin = !S_same_endpts(bsptr,uvlst,ttol);
			else
				not_coin = !S_same_endpts(bsptr,uvlst,tol);

			to_yes = (Sltonp == TL_TO);
			past_yes = (Sltonp == TL_PAST);

			np = UU_LIST_LENGTH(uvlst);
			pts = (UM_coord *)UU_LIST_ARRAY(uvlst);

			vctovc(pts[0],start_point);
			vctovc(pts[np-1],end_point);

			if (!not_coin)			// coincide in tolerance
				skipping = 4;		// go through

			else if (ver98 || ((not_coin && !to_yes) && (not_coin && !past_yes)))
				skipping = 1;		// bounce

			//else if (ver98 || (((not_coin && to_yes) || (not_coin && past_yes)) && (ttol>(2.)*dia)))
			//	skipping = 2;		// bounce

			else if (ver98 || (((not_coin && to_yes) || (not_coin && past_yes)) /*&& (ttol<= (2.)*dia)*/ ))
				skipping = 3;		// go through


			switch(skipping)
			{
			case 1:
				param->flowdir = -param->flowdir;
				flowsame = UU_FALSE;
				break;
			case 2:
				param->flowdir = -param->flowdir;
				flowsame = UU_FALSE;
				break;
			case 3:
				flowsame = UU_TRUE;
				break;
			case 4:
				flowsame = UU_TRUE;
				break;
			default:
				flowsame = UU_TRUE;
				break;
			}

		}
		else 
		//if (UU_LIST_LENGTH(&Suvlist)>0)
		{
			ttol = S_dist_endpts(bsptr,&Suvlist);
			if (ttol>tol)
				not_coin = !S_same_endpts(bsptr,Suvlist,ttol);
			else
				not_coin = !S_same_endpts(bsptr,Suvlist,tol);
			to_yes = (Sltonp == TL_TO);
			past_yes = (Sltonp == TL_PAST);

			if (!not_coin)			// coincide in tolerance
				skipping = 4;		// go through

			else if (ver98 || ((not_coin && !to_yes) && (not_coin && !past_yes)))
				skipping = 1;		// bounce

			//else if (ver98 || (((not_coin && to_yes) || (not_coin && past_yes)) && (ttol>(2.)*dia)))
			//	skipping = 2;		// bounce

			else if (ver98 || (((not_coin && to_yes) || (not_coin && past_yes))/* && (ttol<= (2.)*dia)*/ ))
				skipping = 3;		// go through
			

			switch(skipping)
			{
			case 1:
				param->flowdir = -param->flowdir;
				flowsame = UU_FALSE;
				break;
			case 2:
				param->flowdir = -param->flowdir;
				flowsame = UU_FALSE;
				break;
			case 3:
				flowsame = UU_TRUE;
				break;
			case 4:
				flowsame = UU_TRUE;
				break;
			default:
				flowsame = UU_TRUE;
				break;
			}
		}

		w = wnext;

/*
......intersect next flow line with outer boundary curve.
*/
		UU_LIST_EMPTY(iolst);

		if (lastw && (iup == 1 && Sledge || iup == -1 && Sfedge))
		{
			istat = S_get_edge (-iup,cvtyp,param->flowdir,bound,param->wnd,dwmax,iolst);
			if (istat == 0)
			{
				onedge = 1;
				n2 = 2;
			}
			else
				UU_LIST_EMPTY(iolst);
		}
		if (onedge == 0)
		{
			n2 = ncl_fmill_isect_bndry (w,cvtyp,bound,param->flowdir,iolst);
			if (lastw)
			{
				pm = -pm;
				n2 = S_double_cusps (pm,cvtyp,bound,iolst);
			}
		}
/*
.....create points on next flowline
*/
		if (n2 < 1) break;

		isectp = (struct NCL_isect *) UU_LIST_ARRAY(iolst);
		UU_LIST_EMPTY(uvlst);
		n1 = 0;
		if (n2 > 1)
		{
			k1 = 0;
			k2 = 1;
/*
..... If there are more than 2 intersections, determine whether the
..... first or last intersections should be used to limit the flowline.
..... It is always first intersection if all are counted, i.e., Savoid > 0.
..... It is the first intersection if the previous cut had 2 or less, or
..... at the first cut.
..... If the previous cut also had more than 2, the previous decision is reversed.
*/
			if (n2 > 2)
			{
				if (Savoid == 0 && lmult) last_isec = !last_isec;
				if (last_isec)
				{
					k1 = n2-2;
					k2 = n2-1;
				}
				lmult = UU_TRUE;
			}
			else
			{
				lmult = last_isec = UU_FALSE;
				if (Savdir == DIRSAME)
				{
					for (ib = 0; ib < bound->nb; ib++)
						dirs[ib] = 0;
				}
			}

			if (onedge == 0)
				n1 = S_evolve_flow (bsptr,tfmat,param,isectp,n2,w,tol,uvlst);
			else
				n1 = iolst->cur_cnt;
		}
/*
.....Move along boundary to the next flow line
*/
		if (flowsame) istep = -param->stepdir;
		S_move_along_bndr (istep,iup,ix2,bound,w,&isecfin,&isectp[k1],lmult);
		if (!flowsame) istep = -istep;

	}
}

/*********************************************************************
**    I_FUNCTION     : void S_cut1 (bsptr,tfmat,bound,param,tol,
**                                       ptlst,uvlst,iolst,dirs)
**       Create fmill uv points.
**    PARAMETERS
**       INPUT  :
**          bsptr   - pointer to base surface.
**          tfmat   - pointer to transformation mx.
**          bound   - surface boundary
**          param   - cutting parameters struct
**          tol     - tolerance in XYZ space.
**          ptlst,uvlst,iolst - initialized lists to use
**          dirs              - array to use
**       OUTPUT :  Suvlist - the cut UV points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_cut1 (bsptr,tfmat,bound,param,ust,vst,tol,uvlst,iolst,dia)
struct NCL_fixed_databag *bsptr;
UM_transf tfmat;
UM_srf_boundary *bound;
NCL_fmlparam *param;
UU_REAL ust,vst,tol,dia;
UU_LIST *uvlst,*iolst;
{
	UU_REAL wst,wnd;
	int *dirs = UU_NULL;
	int ib,nb;

	if (Savdir == DIRSAME)
	{
		nb = bound->nb;
		dirs = (int *) uu_malloc (nb*sizeof(int));
		for (ib = 0; ib < nb; ib++)	dirs[ib] = 0;
	}

	if (param->npas > 1)
	{
		if (S_find_edges (param,bound,ust,vst,iolst) != UU_SUCCESS) goto Done;
	}
	else
	{
		wst = param->wst;
		wnd = param->wnd;
		param->wfrs = param->wlst = (wnd + wst)/2;
		param->delw = wnd - wst;
	}

	Stol = 0.25*tol;
	Seps = tol*tol;

	S_cut1a (bsptr,tfmat,bound,param,tol,uvlst,iolst,dirs,dia);

Done:
	UU_FREE (dirs);

	return;
}

/*********************************************************************
**    I_FUNCTION     : void S_cut0 (bsptr,tfmat,bound,param,tol,
**                                       ptlst,uvlst,iolst)
**       Create fmill uv points.
**    PARAMETERS
**       INPUT  :
**          bsptr   - pointer to base surface.
**          tfmat   - pointer to transformation mx.
**          bound   - surface boundary
**          param   - cutting parameters struct
**          tol     - tolerance in XYZ space.
**          ptlst,uvlst,iolst - initialized lists to use
**       OUTPUT :  Suvlist - the cut UV points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_cut0 (bsfptr,tfmat,bound,param,tol,ptlst,uvlst,iolst)
struct NCL_fixed_databag *bsfptr;
UM_transf tfmat;
UM_srf_boundary *bound;
NCL_fmlparam *param;
UU_REAL tol;
UU_LIST *ptlst,*uvlst,*iolst;
{
	UU_REAL w,dw,delw,w1,w2;
	UU_REAL uvlim[2];
	int cvtyp,iup;
	int istep,iflow,jflow,ix1,ix2,jret;
	int n1,n2,nn2,i,j,k1,k2,js,je,jj,jjs,n2prev,knt;
	int jjst,jjnd;
	int nppp,npas;
	UU_REAL wst,wnd;

	int npts;
	int *nps;

	struct NCL_isect *isectp;
	UM_coord *tp,*uvpts;

	cvtyp = param->cvtyp;
	iup = param->iup;
	npas = param->npas;
	nppp = param->nppp;

	wst = param->wst;
	wnd = param->wnd;

	ix1 = cvtyp - 1;
	ix2 = 1-ix1;

	jflow = iflow = param->flowdir;
	istep = param->stepdir;

	nps = bound->np;
	npts = bound->np[0];
	uvpts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
/*
.....Create first boundary intersection and flowline
*/
	delw = (Sltonp > 0)? 0.0005: 0.001;
	dw = (wnd-wst)/(npas-1);
	w = wst + delw*iup;
	n2 = ncl_fmill_isect_bndry (w,cvtyp,bound,iflow,iolst);
	isectp = (struct NCL_isect *)UU_LIST_ARRAY(iolst);
	n1 = 0;
	if (n2 > 2 && Savoid > 0)
		nn2 = n2 - 2;
	else
		nn2 = 0;
	if (n2 > 1)
	{
		j = 0;
		if (iflow<0) j = 1;
		uvlim[j] = isectp[0].pc[ix1];
		if (nn2 > 0)
			uvlim[1-j] = isectp[n2-1].pc[ix1];
		else
			uvlim[1-j] = isectp[1].pc[ix1];
		if (nppp)
			n1 = S_eval_flow (nppp, cvtyp, w, uvlim, uvlst);
		else
			n1 = ncl_evolve_crv_on_srf
			     (bsfptr,tfmat,w,uvlim,cvtyp,tol,ptlst,UU_NULL,uvlst);
	}
	w = wst;
	k1 = 0;
	k2 = 1;
/*
.....Loop outputting points.
*/
	for (i = 1; i < npas; i++)
	{
		tp = (UM_coord *) UU_LIST_ARRAY (uvlst);

		if (n2 > 0)
		{
			uu_list_push (&Suvlist,isectp[k1].pc);
			if (n1 > 0)
			{
				js = (iflow>0)?1:n1;
				je = (iflow>0)?n1:1;
				w2 = isectp[k1].pc[ix1];
				for (j=js;j*iflow<=je*iflow;j+=iflow)
				{
					w1 = tp[j-1][ix1];
					if (fabs(w1-w2) > UM_FUZZ && w1*iflow > w2*iflow) break;
				}
				js = j;
				w2 = isectp[k2].pc[ix1];
				for (j=js;j*iflow<=je*iflow;j+=iflow)
				{
					w1 = tp[j-1][ix1];
					if (fabs(w1-w2) < UM_FUZZ || w1*iflow >= w2*iflow) break;
					uu_list_push (&Suvlist,tp[j-1]);
				}
			}
			jj = k1;
			if (n2 > 1)
			{
				uu_list_push (&Suvlist,isectp[k2].pc);
				jj = k2;
			}
			if (iflow*iup > 0)
				jjs = isectp[jj].jnd;
			else
				jjs = isectp[jj].jst;
			w1 = isectp[jj].pc[ix2]*iup;
		}
		while (nn2 > 0)
		{
			if (Sretlist != UU_NULL)
			{
				jret = Suvlist.cur_cnt - 1;
				uu_list_push (Sretlist,&jret);
			}
			js = j;
			nn2--;
			if (Savoid == DOWN)
				S_along_bndr (&isectp[k2],&isectp[k1+2],(int *)UU_NULL,uvpts,nps);
			k1 += 2; k2 += 2;
			uu_list_push (&Suvlist,isectp[k1].pc);
			if (n1 > 0)
			{
				w2 = isectp[k1].pc[ix1];
				for (j=js;j*iflow<=je*iflow;j+=iflow)
				{
					w1 = tp[j-1][ix1];
					if (fabs(w1-w2) > UM_FUZZ && w1*iflow > w2*iflow) break;
				}
				js = j;
				w2 = isectp[k2].pc[ix1];
				for (j=js;j*iflow<=je*iflow;j+=iflow)
				{
					w1 = tp[j-1][ix1];
					if (fabs(w1-w2) < UM_FUZZ || w1*iflow >= w2*iflow) break;
					uu_list_push (&Suvlist,tp[j-1]);
				}
			}

			jj = k1;
			if (nn2 >= 1)
			{
				uu_list_push (&Suvlist,isectp[k2].pc);
				jj = k2;
			}
			if (iflow*iup > 0)
				jjs = isectp[jj].jnd;
			else
				jjs = isectp[jj].jst;
			w1 = isectp[jj].pc[ix2]*iup;
			nn2--;
		}

		j = jj; js = jjs;

		iflow = -iflow;
		w += dw;
		if (i==npas-1) w = wnd - delw*iup;
		if (w>1.0) w = 1.0;
		if (w<0.0) w = 0.0;
		n2prev = n2;
/*
......intersect next flow line with outer boundary curve.
*/
		UU_LIST_EMPTY(iolst);
		n2 = ncl_fmill_isect_bndry (w,cvtyp,bound,iflow,iolst);
/*
.....create points on next flowline
*/
		if (n2 > 0)
		{
			isectp = (struct NCL_isect *)UU_LIST_ARRAY(iolst);
			UU_LIST_EMPTY(ptlst);
			UU_LIST_EMPTY(uvlst);
			n1 = 0;
			if (n2 > 2 && Savoid > 0)
				nn2 = n2 - 2;
			else
				nn2 = 0;
			if (n2 > 1)
			{
				k1 = 0;
				k2 = 1;
				j  = 0;
				if (iflow < 0) j = 1;
/*
.....If there are more than 2 intersections, determine whether the
.....first or last intersections should be used to limit the flowline.
*/
				if (n2 > 2)
				{
					if (n2prev == 2 || Savoid > 0) jflow = iflow;
					if (iflow*jflow < 0)
					{
						k1 = n2-2;
						k2 = n2-1;
					}
				}
				uvlim[j] = isectp[k1].pc[ix1];
				if (nn2 > 0)
					uvlim[1-j] = isectp[n2-1].pc[ix1];
				else
					uvlim[1-j] = isectp[k2].pc[ix1];

				if (nppp)
					n1 = S_eval_flow (nppp, cvtyp, w, uvlim, uvlst);
				else
					n1 = ncl_evolve_crv_on_srf
				        (bsfptr,tfmat,w,uvlim,cvtyp,tol,ptlst,UU_NULL,uvlst);
			}
/*
.....Move along boundary to next flow line
*/
			if (n1 > 0)
			{
				knt = 0;
				j = js;
				while (uvpts[j][ix2]*iup <= w1 && knt++ < npts)
				{
					j+=istep;
					if (j>=npts) j = 0;
					if (j<0) j = npts-1;
				}
				w2 = isectp[k1].pc[ix2]*iup;
				while (uvpts[j][ix2]*iup<w2 && knt++ < npts)
				{
					uu_list_push (&Suvlist,uvpts[j]);
					j+=istep;
					if (j>=npts) j = 0;
					if (j<0) j = npts-1;
				}
				if (Savoid > 0 && nn2 > 0)
				{
					if (istep == 1)
					{
						jjst = isectp[k1].jst;
						jjnd = isectp[k1].jnd;
					}
					else
					{
						jjst = isectp[k1].jnd;
						jjnd = isectp[k1].jst;
					}

					if (uvpts[jjst][ix2]*iup <= w2 && uvpts[jjst][ix2]*iup > w1)
					{
						if (knt == 0 || abs(jjnd - j) > 1)
						{
							while (knt++ < npts)
							{
								uu_list_push (&Suvlist,uvpts[j]);
								if (j == jjst) break;
								j+=istep;
								if (j>=npts) j = 0;
								if (j<0) j = npts-1;
							}
						}
					}
				}
			}
		}
		istep = -istep;
	}
/*
.....Output points on last flowline
*/
	tp   = (UM_coord *) UU_LIST_ARRAY (uvlst);
	if (n2 > 0)
	{
		uu_list_push (&Suvlist,isectp[k1].pc);
		if (n1 > 0)
		{
			js = (iflow>0)?1:n1;
			je = (iflow>0)?n1:1;
			w2 = isectp[k1].pc[ix1];
			for (j=js;j*iflow<=je*iflow;j+=iflow)
			{
				w1 = tp[j-1][ix1];
				if (fabs(w1-w2) > UM_FUZZ && w1*iflow > w2*iflow) break;
			}
			js = j;
			w2 = isectp[k2].pc[ix1];
			for (j=js;j*iflow<=je*iflow;j+=iflow)
			{
				w1 = tp[j-1][ix1];
				if (fabs(w1-w2) < UM_FUZZ || w1*iflow >= w2*iflow) break;
				uu_list_push (&Suvlist,tp[j-1]);
			}
		}
		if (n2 > 1) uu_list_push (&Suvlist,isectp[k2].pc);
	}

	return;
}

/*********************************************************************
**    I_FUNCTION     : void S_fmill_cut (bsptr,tfmat,bound,param,tol)
**       Create fmill uv points.
**    PARAMETERS
**       INPUT  :
**          bsptr   - pointer to base surface.
**          tfmat   - pointer to transformation mx.
**          bound   - surface boundary
**          param   - cutting parameters struct
**          tol     - tolerance in XYZ space.
**       OUTPUT :  Suvlist - the cut UV points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fmill_cut (bsptr,tfmat,bound,param,ust,vst,tol,dia)
struct NCL_fixed_databag *bsptr;
UM_transf tfmat;
UM_srf_boundary *bound;
NCL_fmlparam *param;
UU_REAL ust,vst,tol,dia;
{
	UU_LIST iobuf,cvus;

	uu_list_init (&Sptlst, sizeof(UM_coord), 0, 100);
	uu_list_init (&cvus, sizeof(UM_coord), 100, 100);
	uu_list_init (&iobuf, sizeof(struct NCL_isect), 20, 20);

	if (ncl_setver(95))
	S_cut0 (bsptr,tfmat,bound,param,tol,&Sptlst,&cvus,&iobuf);
	else
	S_cut1 (bsptr,tfmat,bound,param,ust,vst,tol,&cvus,&iobuf,dia);

	uu_list_free (&Sptlst);
	uu_list_free (&cvus);
	uu_list_free (&iobuf);
}

/*********************************************************************
**    E_FUNCTION     : ncl_fml_uvlist_num()
**       Return the number of fmill uv points.
*********************************************************************/
int ncl_fml_uvlist_num()
{
	int npts = UU_LIST_LENGTH(&Suvlist);
	return (npts);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_fmill_past()
*********************************************************************/
UU_LOGICAL ncl_fmill_past()
{
	return (Slpast == 1);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_fml_normal()
*********************************************************************/
UU_LOGICAL ncl_fml_normal()
{
	return (Slnormal == 1);
}

/********************************************************************
**    E_FUNCTION: ncl_itsa_fml_base (key)
**       Returns UU_TRUE if the key is the same as Sbskey.
*********************************************************************/
UU_LOGICAL ncl_itsa_fml_base (key)
UU_KEY_ID key;
{
	return (key == Sbskey);
}

/********************************************************************
**    E_FUNCTION: ncl_get_fml_tonp (tonp)
*********************************************************************/
void ncl_get_fml_tonp (tonp)
int *tonp;
{
	*tonp = Sltonp;
}

/********************************************************************
**    E_FUNCTION: ncl_set_fml_tonp (tonp)
*********************************************************************/
void ncl_set_fml_tonp (tonp)
int tonp;
{
	Sltonp = tonp;
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_fml_bskey()
*********************************************************************/
void ncl_set_fml_bskey (key)
UU_KEY_ID key;
{
	Sbskey = key;
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_fml_nkey()
*********************************************************************/
void ncl_set_fml_nkey (key)
UU_KEY_ID key;
{
	Snkey = key;
}

/*********************************************************************
**    E_FUNCTION     : ncl_fmcreate (tol8,dst8,ust8,vst8,pars_j,pars_i,npts4,dia)
**       Preprocess fmill command; then create fmill uv points.
**    PARAMETERS
**       INPUT  :
**             tol8			 - tolerance
**             dst8			 - Distance between passes (calculated before from the tool radius).
**             ust8, vst8	 - u,v values near start point (to determine)	
**             pars_j		 - j parameters array, size 5, passed from fmill's jparam	
**             pars_i	     - i parameters array, size 8, passed from fmill's iparam
**             dia           - tool diameter
**       OUTPUT :
**             npts4     - number of points created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fmcreate (tol8,dst8,ust8,vst8,pars_j,pars_i,npts4,dia)
UM_int4 *pars_j,*npts4;
UM_int2 *pars_i;
UM_real8 *tol8, *dst8, *ust8, *vst8, *dia;
{
	int i, status, n1;
	struct NCL_fixed_databag eptr,bsfptr;
	UM_transf tfmat;
	UM_srf_boundary bound;
	UU_REAL w, wst, wnd;
	UU_REAL tol = *tol8, dst = *dst8, ust = *ust8, vst = *vst8;
	UU_KEY_ID skey,ckey,ksec;
	UU_KEY_ID delkey = NULLKEY;
	NCL_fmlparam param;
	UU_LOGICAL noinner;

	*npts4 = 0;

	Sifwd = pars_i[0];
	Sltonp = pars_i[1];
	Savoid = pars_i[2];
	Savlast = pars_i[3];
	Savdir = pars_i[4];
/*
..... the BOTH option - go around boundary once then retract instead of
..... recutting
*/
	Slboth = (pars_i[5] == 1);
	Slnormal = (pars_i[6] == 1)? 1: 0;
	Slretract = (Savoid == RETRCT_PLN || Savoid == RETRCT_SRF ||
						Savoid == RETRCT_DIS);
	if (!Slboth && Savoid != DOWN) Savdir = 0;
	Sdelflg = pars_i[7];

	skey = pars_j[0];
	ckey = pars_j[1];
	ksec = pars_j[2];
	param.npas = pars_j[3];
	param.nppp = pars_j[4];

	Sfedge = Sledge = UU_FALSE;

	status = ncl_fmill_set_sfs (skey,&ckey,&eptr,&bsfptr,ust,vst,tfmat,&delkey);
	if (status != UU_SUCCESS) return;

	uu_list_init(&Suvlist,sizeof(UM_coord),100,800);

	um_init_boundary (&bound);

	noinner = (Savoid == 0 || Savoid == THRU);
	status = ncl_fmill_set_bndry (&eptr,&bsfptr,tfmat,ust,vst,ckey,ksec,&bound,
		tol,noinner,&Savdir);
	if (status != UU_SUCCESS) goto xit;
#if 0		
	um_cvio_displayboundry(&eptr,tfmat,&bound,8,1);
#endif
	Slpast = (Sltonp == TL_PAST)? 1: 0;
/*
.....Calculate number of passes.
*/
	if (param.npas < 1)
	{
		param.npas = S_get_npas (&bsfptr,tfmat,&bound,tol,dst,Sifwd);
	}
	if (param.npas < 1) goto xit;

	if (Slretract)
	{
		Sretlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		uu_list_init (Sretlist, sizeof(int), 20, 20);
	}

	if (Savlast == 1)
	{
		Sbnd = (int *) uu_malloc (2*bound.nb*sizeof(int));
		Sjbnd = 0;
	}
/*
.....Set direction flags.
*/
	ncl_fmill_set_params (&bound,Sifwd,ust,vst,&wst,&wnd,&param);
	if (param.iup < 0)
	{
		w = wst; wst = wnd; wnd = w;
	}
	param.wst = wst; param.wnd = wnd;

	if (Slboth)
	{
/*
.....Add flowline intersection points to the boundary
*/
		ncl_fml_fix_bndr (&bound,&param);
		n1 = UU_LIST_LENGTH(bound.uvpts);
		Scutpt = (int *) uu_malloc (n1 * sizeof(int));
		for (i = 0; i < n1; i++) Scutpt[i] = 0;
	}

	S_fmill_cut (&bsfptr,tfmat,&bound,&param,ust,vst,tol,*dia);

	if (ncl_fml_normal() && ksec != NULLKEY)
	ncl_fmill_refine (&bsfptr,tfmat,ksec,&Suvlist,Sretlist,Sbnd,Sjbnd,tol);

	if (Sretlist != UU_NULL && Sretlist->cur_cnt > 0)
	{
		Sretracts = (int *) UU_LIST_ARRAY (Sretlist);
		Sjret = 0;
	}
	Sjbnd = 0;
/*
...free allocated memory
*/
xit:;
	if (delkey != NULLKEY) uc_delete (delkey);
	Slpast = 0;
	um_free_boundary (&bound);
	UU_FREE (Scutpt);

	*npts4 = UU_LIST_LENGTH(&Suvlist);
	Suvptr = (UM_coord *) UU_LIST_ARRAY(&Suvlist);

	Sfedge = Sledge = UU_FALSE;

	return;
}

/*********************************************************************
**    E_FUNCTION     : S_same_endpts (bsptr,uvlst,dtol)
**       Determines if the start and end point of a UV-pass are the
**       same.
**    PARAMETERS
**       INPUT  :
**          bsptr      - Base surface being machined.
**          uvlst      - List of flowline points.
**          dtol       - Tolerance to test if UV-pass end points
**                       are the same.
**       OUTPUT :
**          none
**    RETURNS      : UU_TRUE if the end points are the same.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_same_endpts(bsptr,uvlst,dtol)
struct NCL_fixed_databag *bsptr;
UU_LIST *uvlst;
UU_REAL dtol;
{
	int np;
	UM_coord *pts;
	UM_transf tfmat;
	struct UM_evsrfout evsrf1,evsrf2;
/*
.....Initialize routine
*/
	np = UU_LIST_LENGTH(uvlst);
	pts = (UM_coord *)UU_LIST_ARRAY(uvlst);
	uc_retrieve_transf(bsptr->key,tfmat);
/*
.....Evaluate flowline end points
*/
	uc_init_evsrfout(bsptr,&evsrf1);
	uc_init_evsrfout(bsptr,&evsrf2);
	uc_evsrf(UM_FRSTDERIV,pts[0][0],pts[0][1],bsptr,tfmat,&evsrf1);
	uc_evsrf(UM_FRSTDERIV,pts[np-1][0],pts[np-1][1],bsptr,tfmat,&evsrf2);
/*
.....Determine if two points are the same
*/
	return(um_cceqcc_tol(evsrf1.sp,evsrf2.sp,dtol));
}


/*********************************************************************
**    E_FUNCTION     : S_dist_endpts (bsptr,uvlst)
**       Determines distance between the start and end points 
**    PARAMETERS
**       INPUT  :
**          bsptr      - Base surface being machined.
**          uvlst      - List of flowline points.
**       OUTPUT :
**          distance
**    RETURNS      : distance
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL S_dist_endpts(bsptr,uvlst)
struct NCL_fixed_databag *bsptr;
UU_LIST *uvlst;
{
	int np;
	UM_coord *pts;
	UM_transf tfmat;
	struct UM_evsrfout evsrf1,evsrf2;
/*
.....Initialize routine
*/
	np = UU_LIST_LENGTH(uvlst);
	pts = (UM_coord *)UU_LIST_ARRAY(uvlst);
	uc_retrieve_transf(bsptr->key,tfmat);
/*
.....Evaluate flowline end points
*/
	uc_init_evsrfout(bsptr,&evsrf1);
	uc_init_evsrfout(bsptr,&evsrf2);
	uc_evsrf(UM_FRSTDERIV,pts[0][0],pts[0][1],bsptr,tfmat,&evsrf1);
	uc_evsrf(UM_FRSTDERIV,pts[np-1][0],pts[np-1][1],bsptr,tfmat,&evsrf2);

	return um_dcccc(evsrf1.sp,evsrf2.sp);
}
