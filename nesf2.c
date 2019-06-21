/*********************************************************************
**    NAME         :  nesf2.c
**       CONTAINS: routines to handle general NCL surfaces.
**                 Removed from nesf.c to this file so that
**                 these routines may be included in the OML.
**                 JLS 6/15/99
**
**       int ncl_eval_surf
**       int ncl_conv_sfseg_rbsf
**       int ncl_conv_msfseg_rbsf (eptr,uv,pt,mv,couv)
**       void ncl_conv_sfseg_reset
**       int ncl_get_conv_sfseg_key
**       void ncl_pt_on_sf
**       int ncl_percnt_on_sf
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nesf2.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:47
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mdeval.h"
#include "modef.h"
#include "uminmax.h"
#include "mgeom.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclxmdl.h"

extern int NCLX_internal_geom;

static UU_REAL WTV[16] = {1.,1.,1.,1.,1.,1.,1.,1.,
                         1.,1.,1.,1.,1.,1.,1.,1.};
static UU_REAL UTV[8] = {0.,0.,0.,0.,1.,1.,1.,1.};
       UU_REAL VTV[8] = {0.,0.,0.,0.,1.,1.,1.,1.};
static UU_KEY_ID UM_evsf_key = 0;
static UU_KEY_ID UM_evsf_pnkey = 0;
static int UM_evsf_patch = -1;

/*********************************************************************
**    E_FUNCTION     : int ncl_eval_surf (evflag, u, v, eptr, tfmat, evsrf)
**       Evaluate an NCL or Mesh surface.
**    PARAMETERS   
**       INPUT  : 
**          evflag     - evaluation flag.
**          u          - U parameter.
**          v          - V parameter.
**          e          - pointer to surface.
**          tfmat      - transformation matrix.
**       OUTPUT :  
**          evsrf      - evaluation record.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_eval_surf (evflag, u, v, e, tfmat, evsrf)
int evflag;
UM_param u, v;
struct NCL_surface_rec *e;
UM_transf tfmat;
struct UM_evsrfout *evsrf;
{
	int status; 
	UM_param ruv[2];
	UU_REAL offd,coef[2];
	static UM_coord pt[16];
	static int iuv[4] = {3, 3, 1, 1};
	NCLX_mdl_surf *s;
	struct NCL_meshsf_rec *msrf;
   
	uu_denter(UU_MTRC,(us,"ncl_eval_surf(key=%x, tfmat=%x)", eptr->key, tfmat));
/*
...check if this surface was converted before
*/
	if (e->key != UM_evsf_key)
	{
		UM_evsf_pnkey = 0;
		UM_evsf_patch = -1;
		UM_evsf_key = e->key;
	}
/*
...convert patch to RBspline surface patch
*/
	ruv[0] = u;
	ruv[1] = v; 

	if (NCLX_internal_geom)
	{
		s = (NCLX_mdl_surf *)e;
		offd = (*s).sfhead.offdist;
		if ((*s).header.relnum == NCLX_MDL_SURF)
			status = ncl_conv_sfseg_rbsf (e,ruv,pt,&iuv[1],coef);
		else
			status = UU_FAILURE;
	}
	else if (e->rel_num == NCL_MESHSURF_REL)
	{
		msrf = (struct NCL_meshsf_rec *) e;
		offd = msrf->offdist;
		status = ncl_conv_msfseg_rbsf (e,ruv,pt,&iuv[1],coef);
	}
	else if (e->rel_num == NCL_SURF_REL)
	{
		offd = e->offdist;
		status = ncl_conv_sfseg_rbsf (e,ruv,pt,&iuv[1],coef);
	}
	else
		status = UU_FAILURE;
/*
...evaluate surface at 'u,v' point
*/
	if (status == UU_SUCCESS)
	status = um_ev9_rbsplsrf1 (evflag,iuv,ruv,UTV,VTV,WTV,pt,offd,evsrf);
/*
...vp 10/23/97 adjust derivatives for patch/panel expansion
*/
	if (status == UU_SUCCESS && evflag > UM_NORM)
	{
		um_vctmsc (evsrf->dsdu,coef[0],evsrf->dsdu);
		um_vctmsc (evsrf->dsdv,coef[1],evsrf->dsdv);
		if (evflag > UM_FRSTDERIV)
		{
			um_vctmsc (evsrf->d2sdu2,coef[0]*coef[0],evsrf->d2sdu2);
			um_vctmsc (evsrf->d2sdv2,coef[1]*coef[1],evsrf->d2sdv2);
		}
	}
/*
... aak 17-apr-1998: apply transform
*/
	if (status == UU_SUCCESS)
	ncl_transform_evsfout (evflag, tfmat, evsrf);

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_conv_sfseg_rbsf (eptr,uv,pt,mv)
**       Transform an NCL surface patch to equivalent RB surface.
**    PARAMETERS   
**       INPUT  : 
**          eptr       - pointer to ncl surface record
**          uv         - global u,v parameters of eveluation point
**       OUTPUT :  
**          uv         - local (in patch) u,v parameters 
**          pt         - pointer to control points of segment.
**          mv         - order of patch in v direction (1 = rulled).
**          couv       - u & v coefficients used to transform patch/panel
**                       related vectors to the full surface size. 
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  ncl_conv_sfseg_rbsf (eptr,uv,pt,mv,couv)
  struct NCL_surface_rec *eptr;
  UU_REAL *uv,*couv;
  UM_coord pt[16];
  int *mv;
  {
	static struct NCL_panel_rec panel;
	struct NCL_patch_rec *patch;
	int status, i, j, k, np, ixu, ixv, inc, flg;
	UU_REAL u, v, rho;
	UM_coord ptc;
	NCLX_mdl_surf *sptr;
/*
...find panel
*/
   flg   = 0;
   u     = uv[0];
   v     = uv[1];
   
	sptr = (NCLX_mdl_surf *)eptr;
	if (NCLX_internal_geom) np = sptr->npanel;
   else np = eptr->no_panelkey;
	couv[1] = np;
   ixv   = (int) (v * np);
   if (v >= 1.0) ixv--;
   rho   = ixv;
   uv[1] = v*np - rho;
	if (NCLX_internal_geom) panel.key = (*sptr).panel[ixv].key;
   else panel.key = eptr->panelkey[ixv];
   status = UU_SUCCESS;
   if (panel.key != UM_evsf_pnkey)
   {
		if (NCLX_internal_geom)
			uu_move_byte(&(sptr->panel[ixv]),&panel,sizeof(struct NCL_panel_rec));
		else
         status = ncl_retrieve_data_fixed (&panel);
      flg    = 1;
   } 
   if (status == UU_FAILURE) goto Done;
   UM_evsf_pnkey = panel.key;
   *mv   = 3 - 2*panel.type;
/*
...find patch
*/
   k     = panel.no_param - 1;
   for (ixu=k; ixu>0 && (panel.param[ixu] >= u); ixu--);
	rho   = panel.param[ixu+1]-panel.param[ixu];
   uv[0] = MIN2(1.0, MAX2(0.0,(u-panel.param[ixu])/rho));

	couv[0] = 1./rho; 

   if (flg == 0 && ixu == UM_evsf_patch) goto Done;
   UM_evsf_patch = ixu;

   patch = (struct NCL_patch_rec *) &panel.patch[ixu];
   um_vctovc (patch->pt,ptc);
   rho   = patch->rho; 
/*
...create RB segment points; Rulled SF has only 2 points in v !!!
*/ 
   if (panel.type == 0) 
      {
       np  = 7;
       inc = 4;
      }
   else
      {
       np  = 3;
       inc = 2;
      }
/*
...first 2 point in u,
...deltas index is in v direction, but pt is build in u
*/
   um_vctovc (ptc,pt[0]);
   for (j=0, k=4; j<np; j++, k+=4)
    {
     for (i=0; i<3; i++)  pt[k][i] = ptc[i] + patch->delta[j][i];  
     if (j == np-inc-1) k = -3;
    }
/*
...3-rd & 4-th point in u in v=0 row
*/
   patch++;
   um_vctovc (patch->pt,ptc);
   for (i=0; i<3; i++)  pt[2][i] = ptc[i] - rho * patch->delta[inc-1][i];  
   um_vctovc (ptc,pt[3]);
/*
...3-rd & 4-th point in u for v>0
*/
   np    = (panel.type == 0)? 4: 2;
   for (j=0, k=6; j<np-1; j++, k+=4)
      for (i=0; i<3; i++)  
         {
          pt[k+1][i]  = ptc[i] + patch->delta[j][i];  
          pt[k][i] = ptc[i] + patch->delta[j][i] - rho *
                      (patch->delta[np+j][i] - patch->delta[j][i]);  
         }
/*
...fix global VTV (v parameters) array for rulled surface
*/
   VTV[2] = VTV[3] = (*mv == 1)? 1.0: 0.; 

Done:
   return(status);
  }
/*********************************************************************
**    E_FUNCTION     : int ncl_conv_msfseg_rbsf (eptr,uv,pt,mv,couv)
**       Transform mesh surface patch to equivalent RB surface.
**    PARAMETERS   
**       INPUT  : 
**          eptr       - pointer to ncl surface record
**          uv         - global u,v parameters of eveluation point
**       OUTPUT :  
**          uv         - local (in patch) u,v parameters 
**          pt         - pointer to control points of segment.
**          mv         - order of patch in v direction (1 = rulled).
**          couv       - u & v coefficients used to transform patch
**                       related vectors to the full surface size. 
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  ncl_conv_msfseg_rbsf (eptr,uv,pt,mv,couv)
  struct NCL_meshsf_rec *eptr;
  UU_REAL *uv,*couv;
  UM_coord pt[16];
  int *mv;
  {
   struct NCL_mpatch_rec *patch;
   int m, n, j, k, np, ixu, ixv;
 
   UU_REAL u, v, rho;
   UM_coord ptc;

   *mv   = 3;
   u     = uv[0];
   v     = uv[1];
/*
...find u patch
*/
   m     = eptr->m;
   n     = eptr->n;
   couv[0] = (UU_REAL) eptr->m;
   couv[1] = (UU_REAL) eptr->n;
   ixu   = (int) (u * m);
   if (u >= 1.0) ixu--;
   rho   = ixu;
   uv[0] = u*m - rho;
/*
...find v patch
*/
   ixv   = (int) (v * n);
   if (v >= 1.0) ixv--;
   rho   = ixv;
   uv[1] = v*n - rho;

   np    = ixv * m + ixu;
   if (np == UM_evsf_patch) goto Done;
   UM_evsf_patch = np;

   patch = (struct NCL_mpatch_rec *) &eptr->mpatch[np];
   um_vctovc (patch->pt,ptc);
/*
...create RB segment points
*/ 
   um_vctovc (ptc,pt[0]);
   for (k=15, j=14; k>0; k--, j-=4)
     {
      um_vcplvc (ptc,patch->delta[j],pt[k]);   
      if (j < 3) j += 15; 
     }
/*
...fix VTV just in case if after NCL rulled SF
*/
   VTV[2] = VTV[3] = .0; 

Done:
   return(UU_SUCCESS);
  }
/*********************************************************************
**    E_FUNCTION     : void ncl_conv_sfseg_reset ()
**       Reset last evaluated surface key so conversion will be
**       forced even for the same surface.
**    PARAMETERS   
**       INPUT  : 
**                none
**       OUTPUT :  
**                none
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_conv_sfseg_reset ()
{
	UM_evsf_key = 0;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_conv_sfseg_key ()
**       Returns the last evaluated NCL surface key
**    PARAMETERS   
**       INPUT  : 
**                none
**       OUTPUT :  
**                none
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_conv_sfseg_key ()
{
	return (UM_evsf_key);
}

/*********************************************************************
**    E_FUNCTION     : ncl_percnt_on_sf2 (cvtype,uv,uper,vpr,srf,tol)
**       Create B-spline curve as a SF edge.
**    PARAMETERS   
**       INPUT  : 
**          srf           surface struct.
**          uper          position on surf, given as percentage.
**          cvtype        1 for U, 2 for V
**          uv            constant parameter V
**          vpr           min and max of parameter U
**          tol           tolerance
**       OUTPUT : 
**          uper          parameter on surf
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_percnt_on_sf2 (cvtype,uv,uper,vpr,srf,tol)
int cvtype;
struct NCL_fixed_databag *srf;
UU_REAL uv,*uper,vpr[],tol;
{
	int i,status,np,ix;
	UM_coord *pt,*up;
	UU_LIST cvpts,uptr;
	UU_REAL toler,dtot,dp,uu,du,u0,u1,del,d;
	UU_REAL um_getpolylen();

	status = UU_SUCCESS;
	toler = 0.1*tol;
	if (toler < 1.e-4) toler = 1.e-4;

	uu_list_init (&cvpts, sizeof(UM_coord), 200, 200);
	uu_list_init (&uptr, sizeof(UM_coord), 200, 200);

	ncl_evolve_crv_on_srf (srf, UM_idmat, uv, vpr, cvtype, tol,
                     &cvpts, UU_NULL, &uptr);
/*
	npts = cvpts.cur_cnt;
	if (npts >= 2)
		ncl_fix_tol (&npts,tol,&cvpts,&cvtang);
*/
	np = cvpts.cur_cnt;
	if (np <= 0)
	{
		status = UU_FAILURE; goto done;
	}
	pt = (UM_coord *) UU_LIST_ARRAY(&cvpts);
	up = (UM_coord *)UU_LIST_ARRAY(&uptr);

	dtot = um_getpolylen (np,pt);
	du = (*uper)/100;
	dp = du*dtot;

	for (i = 0, d = 0.; i < np-1; i++)
	{
		del = UM_DCCCC (pt[i],pt[i+1]);
		if (d + del >= dp) break;
		d = d + del;
	}

	if (d + del < dp )
	{
		status = UU_FAILURE; goto done;
	}

	ix = cvtype - 1;
	u0 = up[i][ix]; u1 = up[i+1][ix];
	if (del < UM_DFUZZ)
	{
		uu = u0;
	}
	else
	{
		du = (dp - d)/del;
		if (du < UM_FUZZ)
			du = 0;
		else if (du + UM_FUZZ > 1)
			du = 1;
		uu = (1 - du)*u0 + du*u1;
	}

	*uper = uu;

done:
	uu_list_free (&cvpts);
	uu_list_free (&uptr);

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_percnt_on_sf1 (cvtype,v0,v1,up,ulim,e1,tol)
**       Compute the surface parameter from a percentage.
**    PARAMETERS   
**       INPUT  : 
**          e1            surface struct.
**          up            position on surf, given as percentage.
**          cvtype        1 for U, 2 for V
**          v0,v1         min and max of parameter V
**          ulim          min and max of parameter U
**          tol           tolerance
**       OUTPUT : 
**          up            parameter on surf
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_percnt_on_sf1 (cvtype,v0,v1,up,ulim,e1,tol)
int cvtype;
struct NCL_fixed_databag *e1;
UU_REAL v0,v1,tol,*up,ulim[];
{
	int status,i;
	UU_REAL v,dv,w,wi;

	dv = (v1 - v0)/4;
	for (i = 0, v = v0, w = 0; i < 5; i++, v+=dv)
	{
		wi = *up;
		status = ncl_percnt_on_sf2 (cvtype,v,&wi,ulim,e1,tol);
		if (status != UU_SUCCESS) return (UU_FAILURE);
		w += wi;
	}
	*up = w/5;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_percnt_on_sf (uper,vper,e1)
**       Fortran callable routine to calculate a point along a curve at U=u.
**    PARAMETERS   
**       INPUT  : 
**          e1            surface struct.
**          uper          position on surf, given as percentage.
**          vper          position on surf, given as percentage.
**       OUTPUT : 
**          uper          U-parameter of surf
**          vper          V-parameter of surf
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_percnt_on_sf (uper,vper,e1)
struct NCL_fixed_databag *e1;
UU_REAL *uper,*vper;
{
	int status,cvtype;
	UM_real8 tol8;
	UU_REAL tol,up,vp,u0,u1,v0,v1,up0,vp0;
	UU_LOGICAL uedge,vedge;
	UM_srf_boundary b;
	UU_REAL wpr[2];

	up = *uper;
	vp = *vper;

	gettol (&tol8);
	tol = tol8;

	if (ncl_itsa_trimsrf (e1))
	{
		ncl_set_boundary_toler (tol);
		status = ncl_get_boundary (UV_BOX_LIST,e1,&b);

		if (status == UU_SUCCESS)
		{
			u0 = b.ummx[0][0]; u1 = b.ummx[0][1];
			v0 = b.vmmx[0][0]; v1 = b.vmmx[0][1];
			um_free_boundary (&b);
		}
		else
			return (status);
	}
	else
	{
		u0 = v0 = 0;
		u1 = v1 = 1;
	}
	up0 = up/100; vp0 = vp/100;

	up0 = u0 + up0*(u1 - u0);
	vp0 = v0 + vp0*(v1 - v0);

	uedge = (up0 < u0 + 0.001 || up0 > u1 - 0.001);
	vedge = (vp0 < v0 + 0.001 || vp0 > v1 - 0.001);

	if (uedge) up = up0;
	if (vedge) vp = vp0;

	if (!uedge)
	{
		cvtype = 1;
		wpr[0] = u0; wpr[1] = u1;

		if (!vedge)
			status = ncl_percnt_on_sf1 (cvtype,v0,v1,&up,wpr,e1,tol);
		else
			status = ncl_percnt_on_sf2 (cvtype,vp,&up,wpr,e1,tol);

		if (status != UU_SUCCESS) return (UU_FAILURE);
	}

	if (!vedge)
	{
		cvtype = 2;
		wpr[0] = v0; wpr[1] = v1;

		if (!uedge)
			status = ncl_percnt_on_sf1 (cvtype,u0,u1,&vp,wpr,e1,tol);
		else
			status = ncl_percnt_on_sf2 (cvtype,up,&vp,wpr,e1,tol);
			
		if (status != UU_SUCCESS) return (UU_FAILURE);
	}

	*uper = up;
	*vper = vp;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_pt_on_sf (itsk,nkey,lpercnt,u,v,pto,ierr)
**       Fortran callable routine to calculate a point (or a normal
**       point-vector) on a surface at U=u,V=v.
**    PARAMETERS   
**       INPUT  : 
**          itsk          point if 0, point-vector if 1
**          nkey          Key of curve.
**          lpercnt       flag to interpret u,v as percentage
**          u             U-parameter of surface.
**          v             V-parameter of surface.
**       OUTPUT : 
**          pto           Calculated point.
**          ierr          Non-zero if there is an error.
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pt_on_sf (itsk,nkey,lpercnt,u,v,pto,ierr)
UM_int4 *nkey;
UM_int2 *itsk,*lpercnt,*ierr;
UM_real8 *u,*v,pto[];
{
	int status,evflg,i;
	struct NCL_fixed_databag e1;
	struct UM_evsrfout evsrf;
	UU_REAL up,vp;
	UM_transf tfmat;

	*ierr = -1;
/*
.....Get the surface data
*/
	e1.key = *nkey;
	status = ncl_retrieve_data_fixed(&e1);
	if (status != UU_SUCCESS) return;
/*
..... set the parameters
*/
	up = *u; vp = *v;
	if (*lpercnt == 1)
	{
		status = ncl_percnt_on_sf (&up,&vp,&e1);
		if (status != UU_SUCCESS) return;
	}
/*
.....Evaluate the surface
*/
	if (*itsk == 1)
		evflg = UM_FRSTDERIV;
	else
		evflg = UM_POINT;

	status = uc_evsrf(evflg, up, vp, &e1, UM_idmat, &evsrf);
	if (status == UU_SUCCESS)
	status = uc_retrieve_transf(e1.key, tfmat);
	if (status != UU_SUCCESS) return;

	*ierr = 0;
	um_cctmtf(evsrf.sp,tfmat,evsrf.sp);
	for (i = 0; i < 3; i++)
	{
		pto[i] = evsrf.sp[i];
	}
	if (evflg == UM_FRSTDERIV)
	{
		um_vctmtf(evsrf.snorm,tfmat,evsrf.snorm);
		for (i = 0; i < 3; i++)
		{
			pto[i+3] = evsrf.snorm[i];
		}
	}

	return;
}
