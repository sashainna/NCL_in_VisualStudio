/*********************************************************************
**   NAME:  nefmill3.c
**    CONTAINS:
**      void ntsf1 
**      void ntsf2 
**      void ncl_get_Sseglist
**      void ncl_add_fml_delkey
**
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nefmill3.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:33
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

typedef struct
{
	UU_KEY_ID key;
	struct NCL_fixed_databag bsptr;
	UM_transf tfmat;
	UM_2Dcoord wbnd; 
	UM_2Dcoord wlim; 
	UM_2Dcoord uvmmx; 
	UM_2Dcoord uvran;
	int cvtyp;
	int iflow;
	UU_LOGICAL lcut;
	UU_LOGICAL ldown;
} S_fmlnet;

typedef struct
{
	int isf;
	int inx;
	int ix1;
	UM_2Dcoord uvend;
	UM_real8 dir;
	UU_REAL tol;
} S_2sfgap;

static int Snumsf = 0;
static int Slast = 0;
static S_fmlnet *Sff = UU_NULL;
static UU_LIST Siobuf,Spts,Stang;

static UM_srf_boundary *Sbndr = UU_NULL;
static int *Sicut = UU_NULL;
static int Sncut = 0;
static UU_LIST Sseglist,Sdelkys;

#define NLST 5
#define UU_NULLK ((UU_KEY_ID *) UU_NULL)
#define UU_NULLR ((UU_REAL *) UU_NULL)

/* #define DBGFML */

/********************************************************************
**    E_FUNCTION: ncl_get_fml_seglist (lst)
*********************************************************************/
void ncl_get_fml_seglist (lst)
UU_LIST **lst;
{
	*lst = &Sseglist;
}

/********************************************************************
**    E_FUNCTION: ncl_add_fml_delkey (key)
*********************************************************************/
void ncl_add_fml_delkey (key)
UU_KEY_ID key;
{
	uu_list_push (&Sdelkys,&key);
}

/*********************************************************************
**    E_FUNCTION     : void ntsf1 (ksf,ksfi,i2,n2)
**       Get the number of surfaces and a surface key off a net surface.
**    PARAMETERS
**       INPUT  :
**          ksf              netsf key
**          i2               which key to return
**       OUTPUT :
**          n2               number of surfaces
**          ksfi             component's key
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ntsf1 (ksf,ksfi,i2,n2)
UM_int4 *ksf,*ksfi;
UM_int2 *i2,*n2;
{
	struct NCL_fixed_databag srf;
	struct NCL_netsf_rec *tsf;
	int i,nsf,status;

	tsf = (struct NCL_netsf_rec *)&srf;
	tsf->key = *ksf;
	status = ncl_retrieve_data_fixed (tsf);
	if (status != UU_SUCCESS) return;

	i = *i2 - 1;
	nsf = tsf->no_netkey;
	if (i < 0 || i >= nsf)
		return;

	*ksfi = tsf->netkey[i];
	*n2 = nsf;

	return;
}

/*********************************************************************
**    I_FUNCTION     : int S_nptpoly (np,pts,uvs,point,ptn,uvn,dmin)
**       Project a point onto a surface outer boundary.
**    PARAMETERS   
**       INPUT  : 
**          np                 number of outer boundary points
**          pts                XYZ coordinates
**          uvs                UV coordinates
**          point              external point
**       OUTPUT :  
**				ptn            XYZ coordinates of nearest point on boundary
**				uvn            estimated UV coordinates of nearest point on
**                             boundary
**				dmin           squared distance from the point to the boundary
**    RETURNS      :   0 = projected point is before first point
**                     1 = point is in segment
**                     2 = point is after last point
**    SIDE EFFECTS : none
**    WARNINGS     : the tolerance is hardcoded as UM_FUZZ; the return is
**                   currently not used
*********************************************************************/
static int S_nptpoly (np,pts,uvs,point,ptn,uvn,dmin)
UM_coord *pts,*uvs,point,ptn,uvn;
int np;
UU_REAL *dmin;
{
	int i,iret,irmin;
	UU_REAL d,di,a,b,c;
	UM_vector vc;
	UM_coord pti;

	*dmin = 1.e+25;
	irmin = 163;

	for (i = 1; i < np; i++)
	{
		um_vcmnvc(pts[i],pts[i-1],vc);
		d = UM_DOT(vc,vc);
		if (d < UM_DFUZZ) continue;
		d = sqrt(d);
		vc[0] /= d; vc[1] /= d; vc[2] /= d; 
		iret = um_nptsg1 (point,pts[i-1],vc,d,pti,&di);

		if (di < *dmin)
		{
			*dmin = di;
			um_vctovc (pti,ptn);
			irmin = iret;
/*
..... estimate the UV nearest point
*/
			if (iret == 0)
				um_vctovc (uvs[i-1],uvn);
			else if (iret == 2)
				um_vctovc (uvs[i],uvn);
			else
			{
				um_vcmnvc(ptn,pts[i-1],vc);
				c = UM_MAG (vc);
				b = c/d;
				a = 1 - b;
				um_avcplbvc (a,uvs[i-1],b,uvs[i],uvn);
			}

			if (*dmin < UM_DFUZZ) break;
		}
	}

	return (irmin);
}

/*********************************************************************
**    I_FUNCTION     : int S_find_next (isf,wst,wnd,ptx,uvx,vcx,tol)
**          Find a surface that borders the current surface near its edge. 
**    PARAMETERS
**       INPUT  :
**          isf      - Current surface number
**          wst,wnd  - U or V range of current surface
**          tol      - Tolerance
**       OUTPUT :
**          vcx    - Current surface end vector, at the point where the next
**                   surface is found)
**          ptx    - Next surface boundary point, within tolerance to the
**                   current surface
**          uvx    - Approximate UV coordinates of ptx on the next surface
**    RETURNS      : next surface number, or -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_find_next (isf,wst,wnd,uvn,ptx,uvx,vcx,tol)
int isf;
UM_coord uvn,ptx,uvx;
UM_vector vcx;
UU_REAL wst,wnd,tol;
{
	int i,j,k,ipm,status;
	int INON = -1;
	struct NCL_isect *isectp;
	int typ,flow,n2,np;
	struct UM_evsrfout evsrf;
	UU_REAL w,w0,uu,vv,d,delw;
	UU_REAL tolsq = tol*tol;
	UM_coord *pp,*uv,spt;

	typ = Sff[isf].cvtyp;
	flow = Sff[isf].iflow;

	w0 = (wst + wnd)/2;
	delw = 0.1*(wnd - wst);
	if (delw < 0.01) delw = 0.01;

	for (w = w0, j = 1, ipm = 1; w <= wnd && w >= wst; j++)
	{
		ipm = -ipm;
		k = (j/2)*ipm;
		w = w0 + delw*k;

		UU_LIST_EMPTY(&Siobuf);
		n2 = ncl_fmill_isect_bndry (w,typ,&Sbndr[isf],flow,&Siobuf);
		isectp = (struct NCL_isect *)UU_LIST_ARRAY(&Siobuf);
		if (n2 <= 1) continue;
		uu = isectp[n2-1].pc[0];
		vv = isectp[n2-1].pc[1];
		uvn[0] = uu; uvn[1] = vv; uvn[2] = 0;
		status = uc_evsrf (UM_FRSTDERIV,uu,vv,&Sff[isf].bsptr,Sff[isf].tfmat,&evsrf);
		if (status != UU_SUCCESS) return (INON);
		um_vctovc (evsrf.sp,spt);
		if (typ == 1)
			um_vctovc (evsrf.dsdu,vcx);
		else
			um_vctovc (evsrf.dsdv,vcx);
		if (flow == -1) um_negvc (vcx,vcx);

		for (i = 0; i < Snumsf; i++)
		{
			if (Sff[i].lcut) continue;
			np = Sbndr[i].np[0];
			pp = (UM_coord *) UU_LIST_ARRAY (Sbndr[i].cvpts);
			uv = (UM_coord *) UU_LIST_ARRAY (Sbndr[i].uvpts);

			S_nptpoly (np,pp,uv,spt,ptx,uvx,&d);
			if (d < tolsq)
			{
				Sff[i].lcut = UU_TRUE;
				Sicut[Sncut] = i;
				Sncut++;
				return (i);
			}
		}
	}

	return (INON);
}

/*********************************************************************
**    I_FUNCTION     : int S_set_params (isf,pt0,uv0,vc0)
**          Set the cvtyp and iflow parameters for the next surface.
**    PARAMETERS
**       INPUT  :
**          isf    - Next surface number
**          pt0    - Next surface boundary point
**          uv0    - Approximate UV coordinates of ptx on the next surface
**          vc0    - Previous surface end vector, at the point where the next
**                   surface is found)
**       OUTPUT :
**          uv0   - Exact UV coordinates of ptx on the next surface.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_params (isf,pt0,uv0,vc0)
int isf;
UM_coord pt0,uv0;
UM_vector vc0;
{
	int status,k;
	UM_real8 u,v,dir;
	UM_real8 fpt[3],spt[3],nvc[3];
	UM_int4 ierr,fkey,unflg;
	UU_REAL uu,vv,cou,cov,wsec,dw,d0,d1;
	struct UM_evsrfout evsrf;
	UM_vector uvec;
	int typ,flow,n,ix1,ix2,atyp;
	struct NCL_isect *isectp;


	status = UU_SUCCESS;
	unflg = 1; /* modsys, MM ?? */

	dir = 0; /* initialize the surface */
	u = uv0[0];	v = uv0[1];
	fkey = Sff[isf].key;
	for (k = 0; k < 3; k++) fpt[k] = pt0[k];
	sfpt2(&fkey, fpt, &u, &v, &unflg, &dir, spt, nvc, &ierr);
	if (ierr != 0) return (UU_FAILURE);

	uu = u;	vv = v;

	status = uc_evsrf (UM_FRSTDERIV,uu,vv,&Sff[isf].bsptr,Sff[isf].tfmat,&evsrf);
	if (status != UU_SUCCESS) return (status);

	uv0[0] = uu; uv0[1] = vv;
	um_vctovc (evsrf.sp,pt0);
	
	um_unitvc (evsrf.dsdu,uvec);
	cou = UM_DOT (uvec,vc0);

	um_unitvc (evsrf.dsdv,uvec);
	cov = UM_DOT (uvec,vc0);

	if (fabs(cou) >= fabs(cov))
	{
		Sff[isf].cvtyp = 1;
		atyp = 2;
		Sff[isf].iflow = (cou < 0)? -1: 1;
		Sff[isf].wbnd[0] = Sff[isf].wlim[0] = Sbndr[isf].vmmx[0][0];
		Sff[isf].wbnd[1] = Sff[isf].wlim[1] = Sbndr[isf].vmmx[0][1];
		Sff[isf].uvmmx[0] = Sbndr[isf].ummx[0][0];
		Sff[isf].uvmmx[1] = Sbndr[isf].ummx[0][1];

	}
	else
	{
		Sff[isf].cvtyp = 2;
		atyp = 1;
		Sff[isf].iflow = (cov < 0)? -1: 1;
		Sff[isf].wbnd[0] = Sff[isf].wlim[0] = Sbndr[isf].ummx[0][0];
		Sff[isf].wbnd[1] = Sff[isf].wlim[1] = Sbndr[isf].ummx[0][1];
		Sff[isf].uvmmx[0] = Sbndr[isf].vmmx[0][0];
		Sff[isf].uvmmx[1] = Sbndr[isf].vmmx[0][1];
	}

	Sff[isf].uvran[0] = Sff[isf].uvmmx[0];
	Sff[isf].uvran[1] = Sff[isf].uvmmx[1];

	typ = Sff[isf].cvtyp;
	flow = Sff[isf].iflow;
	ix1 = typ - 1;
	ix2 = 1 - ix1;

	wsec = uv0[ix2];
	dw = 0;
	if (wsec < Sff[isf].wbnd[0] + UM_FUZZ)
		dw = UM_FUZZ;
	else if (wsec > Sff[isf].wbnd[0] - UM_FUZZ)
		dw = - UM_FUZZ;
	wsec = wsec + dw;

	UU_LIST_EMPTY(&Siobuf);
	n = ncl_fmill_isect_bndry (wsec,typ,&Sbndr[isf],flow,&Siobuf);
	if (n > 0)
	{
		isectp = (struct NCL_isect *)UU_LIST_ARRAY(&Siobuf);
		d0 = fabs (uv0[ix1] - isectp[0].pc[ix1]);
		d1 = fabs (uv0[ix1] - isectp[n-1].pc[ix1]);

		if (d0 > 0.001)
		{
			if (d1 < UM_FUZZ)
			{
				Sff[isf].iflow = -Sff[isf].iflow;
				return (status);
			}
			if (d1 > 0.001)
			{
				wsec = uv0[ix1];
				typ = atyp;
				flow = 1;

				UU_LIST_EMPTY(&Siobuf);
				n = ncl_fmill_isect_bndry (wsec,typ,&Sbndr[isf],flow,&Siobuf);
				if (n > 0)
				{
					isectp = (struct NCL_isect *)UU_LIST_ARRAY(&Siobuf);
					d0 = fabs (uv0[ix2] - isectp[0].pc[ix2]);
					d1 = fabs (uv0[ix2] - isectp[n-1].pc[ix2]);
					if (d1 < d0) flow = -1;
			
					if (d0 < UM_FUZZ || d1 < UM_FUZZ)
					{
						Sff[isf].cvtyp = atyp;
						Sff[isf].iflow = flow;

						d0 = Sff[isf].uvmmx[0];
						Sff[isf].uvmmx[0] = Sff[isf].wbnd[0];
						Sff[isf].wbnd[0] = Sff[isf].wlim[0] = d0;

						d1 = Sff[isf].uvmmx[1];
						Sff[isf].uvmmx[1] = Sff[isf].wbnd[1];
						Sff[isf].wbnd[1] = Sff[isf].wlim[1] = d1;

						Sff[isf].uvran[0] = Sff[isf].uvmmx[0];
						Sff[isf].uvran[1] = Sff[isf].uvmmx[1];

					}
				}
			}
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_set_iup (isf,inx,ptx,uvx,tol)
**          Set the ldown parameter for the next surface.
**    PARAMETERS
**       INPUT  :
**          isf      - current surface number
**          inx      - next surface number
**          uvn      - the common boundary point current surface UV
**          uvx      - the common boundary point next surface UV
**          tol      - tolerance
**       OUTPUT :
**          the next surface ldown parameter is set.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_iup (isf,inx,ptx,uvx,tol)
int isf,inx;
UM_coord ptx,uvx;
UU_REAL tol;
{
	int i,k,np,status;
	int ipm;
	struct UM_evsrfout evsrf;
	UU_REAL dw,uu,vv,d,del,delmin,dwi;
	UU_REAL eps = 4*tol*tol;
	UM_coord *pp,*uv,spt,ptf,uvf,uvn;

	np = Sbndr[isf].np[0];
	pp = (UM_coord *) UU_LIST_ARRAY (Sbndr[isf].cvpts);
	uv = (UM_coord *) UU_LIST_ARRAY (Sbndr[isf].uvpts);

	delmin = 0.0002;
	dw = 0.0002;
	ipm = 1;

	status = UU_FAILURE;
	S_nptpoly (np,pp,uv,ptx,ptf,uvn,&d);
	if (d > eps) return (status);

	for (i = 0; i < 10; i++)
	{
		ipm = -ipm;
		k = i/2 + 1;
		dwi = dw*k*ipm;

		if (Sff[inx].cvtyp == 1)
		{
			uu = uvx[0]; vv = uvx[1] + dwi;
		}
		else
		{
			uu = uvx[0] + dwi; vv = uvx[1];
		}

		status = uc_evsrf (UM_POINT,uu,vv,&Sff[inx].bsptr,Sff[inx].tfmat,&evsrf);
		if (status != UU_SUCCESS) return (status);
		um_vctovc (evsrf.sp,spt);

		S_nptpoly (np,pp,uv,spt,ptf,uvf,&d);
		if (d < eps)
		{
			if (Sff[isf].cvtyp == 1)
				del = uvf[1] - uvn[1];
			else
				del = uvf[0] - uvn[0];
		}
		if (fabs(del) > delmin)
		{
			if (ipm*del > 0)
				Sff[inx].ldown = Sff[isf].ldown;
			else
				Sff[inx].ldown = !Sff[isf].ldown;

			status = UU_SUCCESS;
			return (status);
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_endpln (isf,uvn,spt,nvec)
**          Calculate end plane on a flowline: perpendicular to the surface and
**          along the flowline end.
**    PARAMETERS
**       INPUT  :
**          isf    - surface number
**          uvn    - UV at the surface edge
**       OUTPUT :
**          spt   - XYZ at the surface edge
**          nvec  - plane normal
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_endpln (isf,uvn,spt,nvec)
int isf;
UM_2Dcoord uvn;
UM_coord spt;
UM_vector nvec;
{
	int status;
	UU_REAL uu,vv;
	struct UM_evsrfout evsrf;

	status = UU_SUCCESS;

	uu = uvn[0]; vv = uvn[1];
	status = uc_evsrf (UM_FRSTDERIV,uu,vv,&Sff[isf].bsptr,Sff[isf].tfmat,&evsrf);
	if (status != UU_SUCCESS) return (status);

	if (Sff[isf].cvtyp == 1)
		um_cross (evsrf.dsdu,evsrf.snorm,nvec);
	else
		um_cross (evsrf.dsdv,evsrf.snorm,nvec);
	um_unitvc (nvec,nvec);

	um_vctovc (evsrf.sp,spt);

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_bndrio (inx,dir,plpt,nvec,ppmin,uvmin,dis,tol)
**          Intersect a plane with a surface outer boundary.
**    PARAMETERS
**       INPUT  :
**          inx    - surface number
**          dir    - 0 if sfpt needs to be initialized; 1/-1 if initialized
**          plpt   - plane point
**          nvec   - plane normal
**          tol    - tolerance
**       OUTPUT :
**          ppmin  - plane / boundary intersection point, closest to plpt
**                   (ignored now)
**          uvmin  - UV coordinates of ppmin on the inx surface
**          dis    - squared distance from plpt to ppmin
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_bndrio (inx,dir,plpt,nvec,ppmin,uvmin,dis,tol)
int inx;
UM_real8 *dir;
UM_coord plpt,ppmin,uvmin;
UM_vector nvec;
UU_REAL *dis,tol;
{
	int i,np;
	int status = UU_FAILURE;
	UU_REAL D,dmin,d0,d1,del,ddi,a,b,u0,v0;
	UU_REAL eps0 = 0.01*tol*tol;
	UU_REAL eps1 = 4*tol*tol;
	UM_coord *pp,*uv,pti;
	UM_real8 u,v,fpt[3],spt[3],nvc[3];
	UM_int4 ierr,fkey,unflg;

	D = UM_DOT (plpt,nvec);

	np = Sbndr[inx].np[0];
	pp = (UM_coord *) UU_LIST_ARRAY (Sbndr[inx].cvpts);
	uv = (UM_coord *) UU_LIST_ARRAY (Sbndr[inx].uvpts);

	dmin = 1.e+25;

	d0 = UM_DOT (pp[0],nvec) - D;
	if (fabs (d0) < tol)
	{
		um_vctovc (pp[0],ppmin);
		um_vctovc (uv[0],uvmin);
		dmin = UM_SQDIS (ppmin,plpt);
		status = UU_SUCCESS;
	}

	for (i = 1; i < np; i++)
	{
		d1 = UM_DOT (pp[i],nvec) - D;

		if (fabs (d1) < tol)
		{
			ddi = UM_SQDIS (pp[i],plpt);
			if (ddi < dmin)
			{
				um_vctovc (pp[i],ppmin);
				um_vctovc (uv[i],uvmin);
				dmin = ddi;
				status = UU_SUCCESS;
			}
		}
		if (d0*d1 < 0)
		{
			del = d1 - d0;
			a = -d0/del; b = d1/del;
			um_avcplbvc (a, pp[i], b, pp[i-1], pti);
			ddi = UM_SQDIS (pti,plpt);
			if (ddi < dmin)
			{
				um_vctovc (pti,ppmin);
				um_avcplbvc (a, uv[i], b, uv[i-1], uvmin);
				dmin = ddi;
				status = UU_SUCCESS;
			}
		}

		d0 = d1;
	}

	if (status == UU_SUCCESS)
	{
		fkey = Sff[inx].key;
		unflg = 1; /* modsys, MM ?? */

		u0 = uvmin[0]; v0 = uvmin[1];
		u = u0; v = v0;
		for (i = 0; i < 3; i++) fpt[i] = ppmin[i];
		sfpt2(&fkey, fpt, &u, &v, &unflg, dir, spt, nvc, &ierr);
		if (ierr == 0)
		{
			uvmin[0] = u; uvmin[1] = v;
			if (dmin > eps0 && dmin < eps1)
			{
				u = u0; v = v0;
				for (i = 0; i < 3; i++) fpt[i] = plpt[i];
				sfpt2(&fkey, fpt, &u, &v, &unflg, dir, spt, nvc, &ierr);
				d0 = UM_DOT (spt,nvec) - D;
				if (ierr == 0 && fabs(d0) < tol)
				{
					d1 = UM_SQDIS (spt,plpt);
					if (d1 < dmin)
					{
						uvmin[0] = u; uvmin[1] = v;
						dmin = d1;
						um_vctovc (spt,ppmin);
					}
				}
			}
		}

		*dis = dmin;
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_connect2sf (isf,uvend,inx,uvx,dir,dis,tol)
**       Create a plane along a flowline a a specified point on the first
**       surface, intersect it with the second surface, return the squared
**       distance between the points on the two surfaces. 
**    PARAMETERS   
**       INPUT  : 
**          u             variable parameter on first surface
**          cvs           the two surfaces data
**       OUTPUT : 
**          fu            Squared distance between two surface points
**    RETURNS      : 
**       UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_connect2sf (isf,uvend,inx,dir,uvx,dis,tol)
int inx,isf;
UM_2Dcoord uvend;
UM_coord uvx;
UM_real8 *dir;
UU_REAL *dis,tol;
{
	int status;
	UM_coord ptn,vcn,ptx;

	status = S_endpln (isf,uvend,ptn,vcn);
	if (status == UU_SUCCESS)
	status = S_bndrio (inx,dir,ptn,vcn,ptx,uvx,dis,tol);

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int fdis (u,sgap,fu)
**       Create a plane along a flowline a a specified point on the first
**       surface, intersect it with the second surface, return the squared
**       distance between the points on the two surfaces. 
**    PARAMETERS   
**       INPUT  : 
**          u             variable parameter on first surface
**          cvs           the two surfaces data
**       OUTPUT : 
**          fu            Squared distance between two surface points
**    RETURNS      : 
**       UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int fdis (u,sgap,dis)
UU_REAL u,*dis;
S_2sfgap *sgap;
{
	int status;
	UM_coord uvx;

	sgap->uvend[sgap->ix1] = u;
	status = S_connect2sf (sgap->isf,sgap->uvend,sgap->inx,&sgap->dir,uvx,dis,sgap->tol);

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : void S_fix_gap (isf,uv0,inx,dir,uvx,dis,eps,tol,lgap)
**          Try to reduce a gap between flowlines on two surfaces.
**    PARAMETERS
**       INPUT  :
**          isf    - current surface number
**          inx    - next surface number
**          dir    - 1/-1 if initialized; set for the second surface
**          uvend  - end of the current surface flowline
**          uvnext - start of the next surface flowline
**          eps    - gap parameter for the squared distance
**          tol    - tolerance
**          dis    - squared distance between the two surfaces
**          lgap   - true as there is a gap at input
**       OUTPUT :
**          uvend  - end of the current surface flowline
**          uvnext - start of the next surface flowline
**          dis    - squared distance between the two surfaces
**          lgap   - false if there is no gap now
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fix_gap (isf,uvend,inx,dir,uvnext,dis,eps,tol,lgap)
UM_2Dcoord uvend;
int inx,isf;
UM_real8 dir;
UM_coord uvnext;
UU_REAL *dis,eps,tol;
UU_LOGICAL *lgap;
{
	int i,typ,flow,ix1,ix2,improved;
	int status;
	UU_REAL d0,di,d1,dmin,dbet,u0,ui,u1,umin,udel,ubet;
	UU_REAL eps0 = 0.25*tol*tol;
	S_2sfgap sgap;
	UU_LOGICAL lbetter = UU_FALSE;

	typ = Sff[isf].cvtyp;
	flow = Sff[isf].iflow;
	ix1 = typ - 1;
	ix2 = 1 - ix1;

	sgap.isf = isf;
	sgap.inx = inx;
	sgap.uvend[0] = uvend[0]; sgap.uvend[1] = uvend[1];
	sgap.ix1 = ix1;
	sgap.dir = dir;
	sgap.tol = tol;

	umin = u0 = uvend[ix1];
	dmin = d0 = *dis;

	if (flow == -1)
		u1 = Sff[isf].uvmmx[0];
	else
		u1 = Sff[isf].uvmmx[1];

	status = fdis (u1,&sgap,&d1);
	if (status != UU_SUCCESS) return;

	if (d1 < dmin)
	{
		lbetter = UU_TRUE;
		umin = u1;
		dmin = d1;
		if (d1 < eps0) goto Fin;
	}

	udel = u1 - u0;

	while (fabs (udel) > 0.0005)
	{
		improved = 0;
		ubet = u0;
		dbet = d0;

		udel = udel/10;
		for (i = 1, ui = u0; i < 10; i++)
		{
			ui += udel;
			status = fdis (ui,&sgap,&di);
			if (status != UU_SUCCESS) goto Fin;
			if (di < d0)
			{
				ubet = ui;
				dbet = di;
				improved = 1;
			}
			if (di < dmin)
			{
				lbetter = UU_TRUE;
				umin = ui;
				dmin = di;
				if (di < eps0) goto Fin;
			}
			if (di < d0 && di < d1)
			{
				status = uu_brent (u0,ui,u1,&umin,&di,&fdis,&sgap,eps0);
				goto Fin;
			}
		}
		if (improved == 0) break;
		u0 = ubet;
		d0 = dbet;
		udel = u1 - u0;
	}
/*
..... Note: this just closes gap from the surface isf - need more two-sided
..... code to close it from inx side also
*/

Fin:
	if (lbetter && status == UU_SUCCESS)
	{
		uvend[ix1] = umin;
		S_connect2sf (isf,uvend,inx,&dir,uvnext,dis,tol);
		if (*dis < eps) *lgap = UU_FALSE;
	}
}

/*********************************************************************
**    I_FUNCTION     : UU_LOGICAL S_isec_next (isf,inx,wsf,wnx,lfwd,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          tol    - Tolerance
**       OUTPUT :
**          uvlist - List of uv points.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_isec_next (isf,inx,wsf,wnx,lfwd,tol)
int isf,inx;
UU_REAL wsf,*wnx,tol;
UU_LOGICAL lfwd;
{
	int typ,flow,n,ix1,ix2;
	int status;
	UU_REAL wsec,dw,wst,wnd,dis;
	struct NCL_isect *isectp;
	UM_coord uvx;
	UM_2Dcoord uvend;
	UU_LOGICAL lhit = UU_FALSE;
	UM_real8 dir = 0;
	UU_REAL eps = 4.*tol*tol;


	typ = Sff[isf].cvtyp;
	flow = Sff[isf].iflow;
	if (!lfwd) flow = -flow;

	ix1 = typ - 1;
	ix2 = 1 - ix1;

	wst = Sff[isf].wbnd[0];
	wnd = Sff[isf].wbnd[1];

	dw = 0;
	if (wsf < wst + UM_FUZZ)
		dw = UM_FUZZ;
	else if (wsf > wnd - UM_FUZZ)
		dw = - UM_FUZZ;

	wsec = wsf + dw;

	UU_LIST_EMPTY(&Siobuf);

	n = ncl_fmill_isect_bndry (wsec,typ,&Sbndr[isf],1,&Siobuf);
	if (n < 1) return (lhit);

	isectp = (struct NCL_isect *)UU_LIST_ARRAY(&Siobuf);

	if (flow < 0)
	{
		uvend[0] = isectp[0].pc[0];
		uvend[1] = isectp[0].pc[1];
	}
	else
	{
		uvend[0] = isectp[n-1].pc[0];
		uvend[1] = isectp[n-1].pc[1];
	}

	uvend[ix2] -= dw;

	status = S_connect2sf (isf,uvend,inx,&dir,uvx,&dis,tol);

	if (status == UU_SUCCESS)
	{
		lhit = UU_TRUE;
		typ = Sff[inx].cvtyp;
		ix1 = typ - 1;
		ix2 = 1 - ix1;
		*wnx = uvx[ix2];

		if (lfwd && dis > eps)
		{
			wst = Sff[inx].wbnd[0];
			wnd = Sff[inx].wbnd[1];

			lhit = (*wnx > wst + UM_FUZZ && *wnx < wnd - UM_FUZZ);
		}
	}

	return (lhit);
}

/*********************************************************************
**    I_FUNCTION     : int S_reset_range (itsk,inx,wnx,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          tol      - Tolerance
**       OUTPUT :
**          uvlist - List of uv points.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_reset_range (itsk,inx,wnx,tol)
int itsk,inx;
UU_REAL wnx,tol;
{
	int i,status,isf;
	int is;
	UU_REAL wsf;
	UU_LOGICAL lfwd,lhit;

	lfwd = UU_FALSE;
	status = UU_SUCCESS;

	for (i = Sncut-2; i >= 0 && status == UU_SUCCESS; i--)
	{
		isf = Sicut[i];
		lhit = S_isec_next (inx,isf,wnx,&wsf,lfwd,tol);
		if (!lhit)
			status = UU_FAILURE;
		else
		{
			if (Sff[isf].ldown)
				is = 1 - itsk;
			else
				is = itsk;

			Sff[isf].wlim[is] = wsf;
			inx = isf;
			wnx = wsf;
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_set_range (isf,inx,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          tol      - Tolerance
**       OUTPUT :
**          uvlist - List of uv points.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_range (isf,inx,tol)
int isf,inx;
UU_REAL tol;
{
	int status;
	int itsk,i00,i01,i10,i11;
	UU_REAL wsf,wnx;
	UU_LOGICAL lfwd,lhit;

	status = UU_SUCCESS;

	if (Sff[isf].ldown)
		i00 = 1;
	else
		i00 = 0;
	i01 = 1 - i00;

	if (Sff[inx].ldown)
		i10 = 1;
	else
		i10 = 0;
	i11 = 1 - i10;

	lfwd = UU_TRUE;

	itsk = 0;
	wsf = Sff[isf].wlim[i00];
	lhit = S_isec_next (isf,inx,wsf,&wnx,lfwd,tol);

	if (lhit)
		Sff[inx].wlim[i10] = wnx;
	else
	{
		wnx = Sff[inx].wbnd[i10];
 
		status = S_reset_range (itsk,inx,wnx,tol);
		if (status != UU_SUCCESS) return (status);
	}

	itsk = 1;
	wsf = Sff[isf].wlim[i01];
	lhit = S_isec_next (isf,inx,wsf,&wnx,lfwd,tol);

	if (lhit)
		Sff[inx].wlim[i11] = wnx;
	else
	{
		wnx = Sff[inx].wbnd[i11];
		status = S_reset_range (itsk,inx,wnx,tol);
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_check_isec (isf,typ,flow,wst,wnd,w,uvend,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          uvlim  - Start and end u or v (depending on cvtyp).
**          tol    - tolerance
**       OUTPUT :
**          uvend - last uv point on the list
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_check_isec (isf,typ,flow,wst,wnd,w,uvend,eps)
int isf,typ,flow;
UM_2Dcoord uvend;
UU_REAL wst,wnd,w,eps;
{
	int n,status,ix1,ix2;
	int INON = -1;
	UU_REAL wsec,dw,uu,vv;
	UU_REAL dis;
	UM_coord pp0;
	struct NCL_isect *isectp;
	struct UM_evsrfout evsrf;

	dw = 0;
	if (w < wst + UM_FUZZ)
		dw = UM_FUZZ;
	else if (w > wnd - UM_FUZZ)
		dw = - UM_FUZZ;

	wsec = w + dw;

	UU_LIST_EMPTY(&Siobuf);

	n = ncl_fmill_isect_bndry (wsec,typ,&Sbndr[isf],1,&Siobuf);
	if (n < 2) return (n);

	isectp = (struct NCL_isect *) UU_LIST_ARRAY(&Siobuf);

	uu = isectp[0].pc[0];
	vv = isectp[0].pc[1];
	status = uc_evsrf (UM_POINT,uu,vv,&Sff[isf].bsptr,Sff[isf].tfmat,&evsrf);
	if (status != UU_SUCCESS) return (INON);
	um_vctovc (evsrf.sp,pp0);

	uu = isectp[n-1].pc[0];
	vv = isectp[n-1].pc[1];
	status = uc_evsrf (UM_POINT,uu,vv,&Sff[isf].bsptr,Sff[isf].tfmat,&evsrf);
	if (status != UU_SUCCESS) return (INON);

	dis = UM_SQDIS (pp0,evsrf.sp);
	if (dis < eps)
		n = 1;
	else
	{
		ix1 = typ - 1;
		ix2 = 1 - ix1;

		if (flow == -1)
			uvend[ix1] = isectp[0].pc[ix1];
		else
			uvend[ix1] = isectp[n-1].pc[ix1];

		uvend[ix2] = w;
	}

	return (n);
}

/*********************************************************************
**    I_FUNCTION     : int S_set_wline (ptlist,isf,typ,flow,
**                                          wst,wnd,w,uvend,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          uvlim  - Start and end u or v (depending on cvtyp).
**          tol    - tolerance
**       OUTPUT :
**          uvend - last uv point on the list
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_wline (wline,isf,typ,flow,wsec)
NCL_wline *wline;
int isf,typ,flow;
UU_REAL wsec;
{
	int ix1,ix2,n;
	struct NCL_isect *isectp;

	ix1 = typ - 1;
	ix2 = 1 - ix1;

	UU_LIST_EMPTY(&Siobuf);

	n = ncl_fmill_isect_bndry (wsec,typ,&Sbndr[isf],1,&Siobuf);
	if (n < 2) return (UU_FAILURE);
	isectp = (struct NCL_isect *) UU_LIST_ARRAY(&Siobuf);

	wline->uvlim[0] = isectp[0].pc[ix1];
	wline->uvlim[1] = isectp[n-1].pc[ix1];

	if (flow == -1)
	{
		wline->uvend[0] = isectp[0].pc[0];
		wline->uvend[1] = isectp[0].pc[1];
		if (isf == 0)
			wline->uvlim[1] = Sff[isf].uvmmx[1];
		else if (isf == Slast)
			wline->uvlim[0] = Sff[isf].uvmmx[0];
	}
	else
	{
		wline->uvend[0] = isectp[n-1].pc[0];
		wline->uvend[1] = isectp[n-1].pc[1];
		if (isf == 0)
			wline->uvlim[0] = Sff[isf].uvmmx[0];
		else if (isf == Slast)
			wline->uvlim[1] = Sff[isf].uvmmx[1];
	}

	if (wline->uvlim[0] > Sff[isf].uvran[0]) Sff[isf].uvran[0] = wline->uvlim[0];
	if (wline->uvlim[1] < Sff[isf].uvran[1]) Sff[isf].uvran[1] = wline->uvlim[1];

	wline->uvend[ix2] = wline->w;
	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : int S_wline_evolve (srf,tfmat,w,uvlim,typ,tol)
**          Local ncl_evolve_crv_on_srf interface.
*********************************************************************/
static int S_wline_evolve (srf,tfmat,w,uvlim,typ,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
int typ;
UU_REAL w,uvlim[],tol;
{
	int n;

	UU_LIST_EMPTY(&Spts);
	UU_LIST_EMPTY(&Stang);

	ncl_evolve_crv_on_srf (srf,tfmat,w,uvlim,typ,tol,&Spts,&Stang,NULLST);

	n = Spts.cur_cnt;
	if (n >= 2)	ncl_fix_tol (&n,tol,&Spts,&Stang);

	return (n);
}

/*********************************************************************
**    I_FUNCTION     : int S_create_last (n0,pp0,ptlst,kylst)
**          Local ncl_evolve_crv_on_srf interface.
*********************************************************************/
static int S_create_last (n0,pp0,ptlst,kylst)
int n0;
UM_coord pp0;
UU_LIST *ptlst,*kylst;
{
	UM_coord *pts,pmid;
	UU_KEY_ID ckey;

	pts = (UM_coord *) UU_LIST_ARRAY (ptlst);
	um_middlept (pp0,pts[n0-1],pmid);
	um_vctovc (pmid,pp0);
	um_vctovc (pmid,pts[n0-1]);

	ckey = ncl_create_spline1 (pts,n0);
	if (ckey == NULLKEY) return (UU_FAILURE);
	uu_list_push (kylst,&ckey);
	UU_LIST_EMPTY(ptlst);

	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : int S_wline_pts0 (ptlist,isf,typ,flow,
**                                          wst,wnd,w,uvend,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          uvlim  - Start and end u or v (depending on cvtyp).
**          tol    - tolerance
**       OUTPUT :
**          uvend - last uv point on the list
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_wline_pts0 (wline,isf,typ,flow,tol)
NCL_wline *wline;
int isf,typ,flow;
UU_REAL tol;
{
	int n,nlast,status;
	UM_coord *pp,*pts;
	UU_REAL dis,eps = 4*tol*tol;
	UU_KEY_ID ckey;

	status = UU_SUCCESS;

	n = S_wline_evolve (&Sff[isf].bsptr,Sff[isf].tfmat,wline->w,wline->uvlim,
			typ,tol);

	pp = (UM_coord *) UU_LIST_ARRAY (&Spts);
	if (n == 2)
	{
		dis = UM_SQDIS (pp[0],pp[1]);
		if (dis < eps) n = 1;
	}

	if (flow == -1)
		ncl_revers1_list (n,0,pp,1);

	nlast = (wline->ptlist).cur_cnt;
	if (nlast > 0)
	{
		pts = (UM_coord *) UU_LIST_ARRAY (&wline->ptlist);
		dis = UM_SQDIS (pp[0],pts[nlast-1]);
		if (dis < eps)
		{
			status = S_create_last (nlast,pp[0],&wline->ptlist,&wline->keylst);
			if (status != UU_SUCCESS) return (status);
			nlast = 0;
		}
	}

	if (nlast == 0 && n < 2) return (status);

	uu_list_push_multiple (&wline->ptlist,n,pp);

	if (isf == Slast)
	{
		nlast = (wline->ptlist).cur_cnt;
		if (nlast > 0)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (&wline->ptlist);
			ckey = ncl_create_spline1 (pts,nlast);
			if (ckey == NULLKEY)
				status = UU_FAILURE;
			else
				uu_list_push (&wline->keylst,&ckey);
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_wline_pts1 (ptlist,isf,typ,flow,
**                                          wst,wnd,w,uvend,tol)
**          Create a fixed number of uv points between a start and
**          end point on a flowline.
**    PARAMETERS
**       INPUT  :
**          npts   - Total number of points (including start and end pts).
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          ix     - Index into uvlim array depending on flow direction.
**          w      - Constant u or v (depending on cvtyp).
**          uvlim  - Start and end u or v (depending on cvtyp).
**          tol    - tolerance
**       OUTPUT :
**          uvend - last uv point on the list
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_wline_pts1 (lenlst,wline,isf,typ,flow,llft,lrgt,tol)
UU_LIST *lenlst;
NCL_wline *wline;
int isf,typ,flow,llft,lrgt;
UU_REAL tol;
{
	int n,nlast,status;
	UU_KEY_ID ckey;
	UM_coord *pp;
	UU_REAL w0,w1,dtot;
	UM_2Dcoord uvl,uvm,uvr;
	UU_REAL um_getpolylen();
	UU_REAL A = 0.95;
	UU_REAL B = 0.05;

	w0 = A*Sff[isf].uvran[0] + B*Sff[isf].uvran[1];
	w1 = A*Sff[isf].uvran[1] + B*Sff[isf].uvran[0];

	if (flow == 1)
	{
		if (llft == 1)
		{
			uvl[0] = wline->uvlim[0];
			uvl[1] = uvm[0] = w0;
		}
		else
			uvm[0] = wline->uvlim[0];

		if (lrgt == 1)
		{
			uvr[0] = uvm[1] = w1;
			uvr[1] = wline->uvlim[1];
		}
		else
			uvm[1] = wline->uvlim[1];
	}
	else
	{
		if (llft == 1)
		{
			uvl[0] = uvm[1] = w1;
			uvl[1] = wline->uvlim[1];
		}
		else
			uvm[1] = wline->uvlim[1];

		if (lrgt == 1)
		{
			uvr[1] = uvm[0] = w0;
			uvr[0] = wline->uvlim[0];
		}
		else
			uvm[0] = wline->uvlim[0];
	}

	nlast = (wline->ptlist).cur_cnt;
	
	if (llft == 1)
	{
		n = S_wline_evolve (&Sff[isf].bsptr,Sff[isf].tfmat,wline->w,uvl,
				typ,tol);
		if (n < 2) return (UU_FAILURE);
		pp = (UM_coord *) UU_LIST_ARRAY (&Spts);
		if (flow == -1) ncl_revers1_list (n,0,pp,1);

		if (lenlst != NULLST)
		{
			dtot = um_getpolylen (n,pp);
			uu_list_push (lenlst,&dtot);
		}

		if (nlast > 0)
		{
			status = S_create_last (nlast,pp[0],&wline->ptlist,&wline->keylst);
			if (status != UU_SUCCESS) return (status);
			nlast = 0;
		}

		ckey = ncl_create_spline1 (pp,n);
		if (ckey == NULLKEY) return (UU_FAILURE);
		uu_list_push (&wline->keylst,&ckey);
	}

	n = S_wline_evolve (&Sff[isf].bsptr,Sff[isf].tfmat,wline->w,uvm,
				typ,tol);
	if (n < 2) return (UU_FAILURE);
	pp = (UM_coord *) UU_LIST_ARRAY (&Spts);
	if (flow == -1) ncl_revers1_list (n,0,pp,1);

	if (lenlst != NULLST)
	{
		dtot = um_getpolylen (n,pp);
		uu_list_push (lenlst,&dtot);
	}

	if (nlast > 0)
	{
		status = S_create_last (nlast,pp[0],&wline->ptlist,&wline->keylst);
		if (status != UU_SUCCESS) return (status);
		nlast = 0;
	}

	if (lrgt == 1)
	{
		ckey = ncl_create_spline1 (pp,n);
		if (ckey == NULLKEY) return (UU_FAILURE);
		uu_list_push (&wline->keylst,&ckey);

		n = S_wline_evolve (&Sff[isf].bsptr,Sff[isf].tfmat,wline->w,uvr,
				typ,tol);
		if (n < 2) return (UU_FAILURE);
		pp = (UM_coord *) UU_LIST_ARRAY (&Spts);
		if (flow == -1) ncl_revers1_list (n,0,pp,1);
		if (lenlst != NULLST)
		{
			dtot = um_getpolylen (n,pp);
			uu_list_push (lenlst,&dtot);
		}
	}

	if (isf == Slast)
	{
		ckey = ncl_create_spline1 (pp,n);
		if (ckey == NULLKEY) return (UU_FAILURE);
		uu_list_push (&wline->keylst,&ckey);
	}
	else
		uu_list_push_multiple (&wline->ptlist,n,pp);

	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : void S_get_ranges (lgap,tol)
**          Create a new base surface by linking component surfaces and
**          joining flowlines.
**    PARAMETERS
**       INPUT  :
**          tol       - tolearnce
**       OUTPUT :
**          sfp  - New base surface
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_get_ranges (lgap,tol)
UU_LOGICAL *lgap;
UU_REAL tol;
{
	int i,j,status,ix1,ix2,nls1;
	int typ,flow,isf,inx,istat;
	UU_REAL w,w0,w1,delw,wst,wnd,dis,dw;
	UU_REAL DELMIN = 0.0008;
	UU_REAL tolsq = tol*tol;
	UU_REAL eps,eps1;
	UM_2Dcoord uvend[2];
	UM_coord uvn,ptx,uvx;
	UM_vector vcx;
	UM_real8 dir;

	eps = 4.*tolsq;
	eps1 = 0.04*tolsq;

	for (isf = 0; isf >= 0; isf = inx)
	{
		Slast = isf;
		w0 = Sff[isf].wlim[0]; w1 = Sff[isf].wlim[1];
		inx = S_find_next (isf,w0,w1,uvn,ptx,uvx,vcx,tol);
		if (inx >= 0)
		{
			istat = S_set_params (inx,ptx,uvx,vcx);
			if (istat == UU_SUCCESS)
				istat = S_set_iup (isf,inx,ptx,uvx,tol);
			if (istat == UU_SUCCESS)
				istat = S_set_range (isf,inx,tol);
			if (istat != UU_SUCCESS)
			{
				Sncut--;
				inx = -1;
			}
		}
	}

	if (Sncut < 2) return;

	nls1 = 0;
	dw = 0.0005;

Chk:
	isf = 0;
	w0 = Sff[isf].wlim[0]; w1 = Sff[isf].wlim[1];
	delw = w1 - w0;
	if (delw < DELMIN)
	{
		Sncut = 1; return;
	}
	nls1++;

	status = UU_SUCCESS;
	*lgap = UU_FALSE;
	for (i = 0; i < Sncut; i++)
	{
		inx = Sicut[i];
		typ = Sff[inx].cvtyp;
		flow = Sff[inx].iflow;

		ix1 = typ - 1;
		ix2 = 1 - ix1;
		wst = Sff[inx].wbnd[0]; wnd = Sff[inx].wbnd[1];

		dir = 0;

		for (j = 0; j < 2; j++)
		{
			if (i == 0)
				w = Sff[isf].wlim[j];
			else
			{
				status = S_connect2sf (isf,uvend[j],inx,&dir,uvx,&dis,tol);

				if (status != UU_SUCCESS) goto Done;
				if (dis > eps) 
				{
					*lgap = UU_TRUE;
					S_fix_gap (isf,uvend[j],inx,dir,uvx,&dis,eps,tol,lgap);
				}
				w = uvx[ix2];
			}
			istat = S_check_isec (inx,typ,flow,wst,wnd,w,uvend[j],eps1);
			if (istat < 2)
			{
				if (j == 0)
					Sff[0].wlim[j] += dw;
				else
					Sff[0].wlim[j] -= dw;

				goto Chk;
			}
		}
		isf = inx;
	}

Done:
	if (status != UU_SUCCESS) 		
		Sncut = 1;
	
	return;
}

/*********************************************************************
**    I_FUNCTION     : int S_new_basesf (sfp,tol)
**          Create a new base surface by linking component surfaces and
**          joining flowlines.
**    PARAMETERS
**       INPUT  :
**          tol       - tolerance
**       OUTPUT :
**          sfp  - New base surface
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_new_basesf (sfp,tol)
struct UM_rbsplsrf_rec *sfp;
UU_REAL tol;
{
	int i,j,status,ix2,k,nc2,llft,lrgt;
	UU_LIST comkys,lenlst;
	UU_REAL w0,w1,delw,wst,wnd,dis,wsec;
	UU_LOGICAL lgap;
	int typ,flow;
	UM_coord uvx;
	int isf,inx;
	UM_real8 dir;
	UM_int2 lfl_77;
	int NLS1 = NLST - 1;
	NCL_wline wline[NLST];
	struct NCL_fixed_databag cvp[NLST];

	status = UU_SUCCESS;

	uu_list_init (&Siobuf, sizeof(struct NCL_isect), 20, 20);
	S_get_ranges (&lgap,tol);
	if (Sncut == 1) goto Fin;

/* set ranfile label flag to temp unknown */
	lfl_77 = 1;
	stunlb (&lfl_77);

	uu_list_init(&Spts,sizeof(UM_coord),100,100);
	uu_list_init(&Stang,sizeof(UM_vector),100,100);
	uu_list_init (&Sseglist,sizeof(struct NCL_crvgen_rec),100,100);
	uu_list_init (&comkys,sizeof(UU_KEY_ID),0,10);

	nc2 = 2*Sncut;
	uu_list_init (&lenlst,sizeof(UU_REAL),0,nc2);

	for (j = 0; j < NLST; j++)
	{
		wline[j].key = NULLKEY;
		uu_list_init(&wline[j].ptlist,sizeof(UM_coord),0,100);
		uu_list_init(&wline[j].keylst,sizeof(UU_KEY_ID),0,nc2);
	}

	isf = 0;
	w0 = Sff[isf].wlim[0]; w1 = Sff[isf].wlim[1];

	delw = (w1 - w0)/NLS1;

	for (i = 0; i < Sncut; i++)
	{
		inx = Sicut[i];
		typ = Sff[inx].cvtyp;
		flow = Sff[inx].iflow;

		ix2 = 2 - typ;
		wst = Sff[inx].wbnd[0]; wnd = Sff[inx].wbnd[1];

		llft = lrgt = 0;
		dir = 0;

		for (j = 0; j < NLST; j++)
		{
			if (i == 0)
			{
				wline[j].w = w0 + j*delw;

			}
			else
			{
				status = S_connect2sf (isf,wline[j].uvend,inx,&dir,uvx,&dis,tol);

				if (status != UU_SUCCESS) goto Done;
				wline[j].w = uvx[ix2];
			}
			if (wline[j].w < wst + UM_FUZZ)
				wsec = wline[j].w + UM_FUZZ;
			else if (wline[j].w > wnd - UM_FUZZ)
				wsec = wline[j].w - UM_FUZZ;
			else
				wsec = wline[j].w;

			status = S_set_wline (&wline[j],inx,typ,flow,wsec);
			if (status != UU_SUCCESS) goto Done;
		}

		if (lgap)
		{
			for (j = 0; j < NLST; j++)
			{
				status = S_wline_pts0 (&wline[j],inx,typ,flow,tol);
				if (status != UU_SUCCESS) goto Done;
			}
		}
		else
		{
			if (Sff[inx].uvran[1] - Sff[inx].uvran[0] > 10*UM_FUZZ)
			{
				for (j = 0; j < NLST; j++)
				{
					if (llft == 0 &&
						wline[j].uvlim[0] + UM_FUZZ < Sff[inx].uvran[0]) llft = 1;
					if (lrgt == 0 &&
						wline[j].uvlim[1] - UM_FUZZ > Sff[inx].uvran[1]) lrgt = 1;
				}
			}
			if (flow == -1)
			{
				k = llft; llft = lrgt; lrgt = k;
			}

			for (j = 0; j < NLST; j++)
			{
				if (j == NLST/2)
				status = S_wline_pts1 (&lenlst,&wline[j],inx,typ,flow,llft,lrgt,tol);
				else
				status = S_wline_pts1 (NULLST,&wline[j],inx,typ,flow,llft,lrgt,tol);
				if (status != UU_SUCCESS) goto Done;
			}
		}
		
		isf = inx;
	}

	if (!lgap) sbsolv_insert = Sncut;

	for (j = 0; j < NLST; j++)
	{
		if (lgap)
			ncl_create_wline (&wline[j],NULLST,tol);
		else
			ncl_create_wline (&wline[j],&lenlst,tol);
		if (wline[j].key == NULLKEY)
		{
			status = UU_FAILURE; goto Done;
		}
	}

	k = NLST;
	status = ncl_create_bsf (k,wline,cvp,sfp);

		ncl_store_wf1 (sfp->key);

		ncl_def_color (sfp->key);
		uc_display (sfp);

Done:
	for (j = 0; j < NLST; j++)
	{
		uu_list_free(&wline[j].ptlist);
	}
	uu_list_free(&lenlst);
	uu_list_free(&Spts);
	uu_list_free(&Stang);
	uu_list_free(&Sseglist);
	uu_list_free(&comkys);

	stunlb (&lfl_77);
Fin:
	uu_list_free(&Siobuf);
	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_new_surf (ntky,nkey,iup,ust,vst,tol)
**          Create a new trimmed surface out of a net surface.
**    PARAMETERS
**       INPUT  :
**          ntky   - net surface key
**          tol    - tolerance
**       OUTPUT :
**          nkey   - new surface key
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_new_surf (ntky,nkey,tol)
UU_KEY_ID ntky,*nkey;
UU_REAL tol;
{
	struct NCL_fixed_databag srf,*bs;
	struct NCL_netsf_rec *tsf;
	struct UM_rbsplsrf_rec sfp;
	int i,status;
	UU_KEY_ID skey;
	UU_LOGICAL noinner = UU_FALSE;

	tsf = (struct NCL_netsf_rec *)&srf;
	tsf->key = ntky;

	status = ncl_retrieve_data_fixed (tsf);
	if (status != UU_SUCCESS) return (status);

	for (i = 1; i < Snumsf; i++)
	{
		Sicut[i] = -1;
		Sff[i].lcut = UU_FALSE;
		Sff[i].ldown = UU_FALSE;
		skey = tsf->netkey[i];
		Sff[i].key = srf.key = skey;
		status = ncl_retrieve_data_fixed (&srf);
		if (status == UU_SUCCESS)
		status = uc_retrieve_transf(skey,Sff[i].tfmat);
	
		if (status == UU_SUCCESS)
		{
			bs = &Sff[i].bsptr;
			if (ncl_itsa_trimsrf (&srf))
			{
				status = ncl_trimsrf_get_bs (&srf,&bs);
			}
			else
			{
				bs->key = skey;
				status = ncl_retrieve_data_fixed (bs);
			}
		}

		if (status == UU_SUCCESS)
		{
			Sff[i].key = srf.key;
			status = 
				ncl_initset_boundary (&srf,Sff[i].tfmat,&Sbndr[i],noinner,0.25*tol);
		}
		if (status != UU_SUCCESS) return (status);
	}

	status = S_new_basesf (&sfp,tol);

	if (status == UU_SUCCESS && Sncut > 1)
	{
		status = ncl_new_trim (&sfp,Sbndr,Sncut,Sicut,nkey,tol);
	}
	
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ntsf2 (knetsf,ksf,kcv,tol8,dst8,ust8,vst8,ifwd2,ier)
**       Create fmill uv points.
**    PARAMETERS
**       INPUT  :
**             ksf      - Key of surface.
**             kcv      - Key of curve.
**             ifwd     - 0 = scrub in u dir, 1 = scrub in v dir.
**             tol      - tolerance
**             npas4    - Number of passes.
**             nppp     - Number of points per pass (0 for toler)
**             dst      - Distance between passes.
**             ust      - u value near start point
**             vst      - v value near start point
**       OUTPUT :
**             npts     - number of points created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ntsf2 (knetsf,nsf2,ksf,kcv,tol8,ust8,vst8,ifwd2,ier)
UM_int4 *knetsf,*ksf,*kcv;
UM_int2 *nsf2,*ifwd2,*ier;
UM_real8 *tol8,*ust8,*vst8;
{
	int status,i,nkeys;
	int ifwd,avdir;
	struct NCL_fixed_databag eptr;
	UU_REAL wst, wnd;
	UU_REAL tol = *tol8, ust = *ust8, vst = *vst8;
	UU_KEY_ID nkey,skey,ckey;
	UU_KEY_ID *dky;
	UU_KEY_ID delkey = NULLKEY;
	NCL_fmlparam param;
	UU_LOGICAL noinner = UU_FALSE;

	ifwd = *ifwd2;
	ncl_set_fml_tonp (0);
	avdir = 0;

	nkey = *knetsf;
	skey = *ksf;
	ckey = *kcv;

	Snumsf = *nsf2;
	if (Snumsf < 2) goto Fin;

	uu_list_init (&Sdelkys,sizeof(UU_KEY_ID),10,10);

	Sbndr = (UM_srf_boundary *) uu_malloc (Snumsf*sizeof(UM_srf_boundary));
	for (i = 0; i < Snumsf; i++)
	{	
		um_init_boundary (&Sbndr[i]);
	}

	Sicut = (int *) uu_malloc (Snumsf*sizeof(int));
	Sncut = 1;
	Sicut[0] = 0;

	Sff = (S_fmlnet *) uu_malloc (Snumsf*sizeof(S_fmlnet));
	Sff[0].lcut = UU_TRUE;
	Sff[0].ldown = UU_FALSE;
	
	status = ncl_fmill_set_sfs (skey,&ckey,&eptr,&Sff[0].bsptr,ust,vst,Sff[0].tfmat,
		&delkey);
	if (delkey != NULLKEY) uu_list_push (&Sdelkys,&delkey);

	if (status != UU_SUCCESS) goto Xit;

	Sff[0].key = eptr.key;
	status = ncl_initset_boundary (&eptr,Sff[0].tfmat,&Sbndr[0],noinner,0.25*tol);
	if (status != UU_SUCCESS) goto Xit;

	ncl_fmill_set_params (&Sbndr[0],ifwd,ust,vst,&wst,&wnd,&param);

	Sff[0].cvtyp = param.cvtyp;
	Sff[0].iflow = param.flowdir;
	Sff[0].wbnd[0] = Sff[0].wlim[0] = wst;
	Sff[0].wbnd[1] = Sff[0].wlim[1] = wnd;
	if (param.cvtyp == 1)
	{
		Sff[0].uvmmx[0] = Sbndr[0].ummx[0][0];
		Sff[0].uvmmx[1] = Sbndr[0].ummx[0][1];
	}
	else
	{
		Sff[0].uvmmx[0] = Sbndr[0].vmmx[0][0];
		Sff[0].uvmmx[1] = Sbndr[0].vmmx[0][1];
	}

	Sff[0].uvran[0] = Sff[0].uvmmx[0];
	Sff[0].uvran[1] = Sff[0].uvmmx[1];

	status = S_new_surf (nkey,&skey,tol);
	if (status == UU_SUCCESS)
	{
		*nsf2 = Sncut;
		if (Sncut > 1)
		{
			*ust8 = 0;
			if (param.iup == -1)
				*vst8 = 1;
			else
				*vst8 = 0;

			*ifwd2 = 0;
			*ksf = skey;
		}
	}

Xit:
	for (i = 0; i < Snumsf; i++)
	{	
		um_free_boundary (&Sbndr[i]);
	}
	UU_FREE (Sbndr);
	UU_FREE (Sff);
	UU_FREE (Sicut);
	Snumsf = 0;

	nkeys = Sdelkys.cur_cnt;
	if (nkeys > 0)
	{
		dky = (UU_KEY_ID *) UU_LIST_ARRAY (&Sdelkys);
		for (i = 0; i < nkeys; i++)
			if (dky[i] != NULLKEY) uc_delete (dky[i]);
	}
	uu_list_free (&Sdelkys);

Fin:
	if (status != UU_SUCCESS) *ier = status;
	if (*ier != 0) ncl_delete_fml_nkey();

	return;
}
