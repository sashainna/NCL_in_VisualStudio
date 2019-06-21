/*********************************************************************
**    NAME         :  m5ecrv.c
**       CONTAINS: routines to extract RB curves from surfaces. 
**			int um_rbcrv_from_rbsrf (option,param, srf, unicrv)
**			int um_rbcrv_from_surf (option,param, srf, unicrv)
**			int um_rbcrv_from_msrf (option,param, srf, unicrv)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m5ecrv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:06
*********************************************************************/
#include "nccs.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "go.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdebug.h"
#include "mdeval.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdcpln.h"
#include "modef.h"
#include "misect.h"
#include "mderror.h"

/*********************************************************************
**    E_FUNCTION: int um_rbcrv_from_rbsrf(option,param, srf, unicrv)
**       Create a isoparametric U/V curve (UNICRV) from the specified 
**			RBSPL surface (SRF) at the given V/U parameter value (PARAM). 
**			The simplest isoparametric curve will be created in UNIBASE 
**			and returned.
**    PARAMETERS   
**       INPUT  : 
**         option   1 - U curve, 2 - V curve.
**         param				V/U parameter value at which to create the
**									         U/V isoparametric curve
**				     srf				  pointer to NCL surface entity
**       OUTPUT :  
**        	unicrv			pointer to curve entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_rbcrv_from_rbsrf(option,param, srf, unicrv)
	UM_param param;
 int option;
	struct UM_rbsplsrf_rec *srf;
	struct UC_rbsplcrv_rec *unicrv;

	{
	int status = UU_FAILURE;
 int i, npt, nin;
 UM_transf tfmat;
 struct UM_evsrfout psrf;
 struct NCL_crvgen_rec *ptve, *cpt;
	UU_REAL p0, p1, p, r, sv, *ptuv;

	uu_denter(UU_MTRC, (us,"um_u_agcrv_from_agsrf(param=%g, srfkey=%x)",
		param, srf->key));

 status = uc_retrieve_transf(srf->key, tfmat);

	/* determine parameter value */
 if (option == 1)
   {
    nin  = srf->no_tu;
    ptuv = srf->tu;
    p0   = srf->tv[0];
    p1   = srf->tv[srf->no_tv-1];
   }
 else
   {
    nin  = srf->no_tv;
    ptuv = srf->tv;
    p0   = srf->tu[0];
    p1   = srf->tu[srf->no_tu-1];
   }

 r    = (p1 - p0) * param;
 cpt = (struct NCL_crvgen_rec *) uu_toolmalloc (nin*sizeof(struct NCL_crvgen_rec));
 ptve = cpt;
 
 sv   = ptuv[0] - .1;
 npt  = 0;
 for (i=0; i<nin; i++)
   {
    p = ptuv[i];
    if (p != sv)
      {
       if (option == 1)
         {
          status = uc_evsrf(UM_FRSTDERIV, p, r, srf, tfmat, &psrf);
          ptve->a = psrf.dsdu[0];
          ptve->b = psrf.dsdu[1];
          ptve->c = psrf.dsdu[2];
         }
       else
         {
          status = uc_evsrf(UM_FRSTDERIV, r, p, srf, tfmat, &psrf);
          ptve->a = psrf.dsdv[0];
          ptve->b = psrf.dsdv[1];
          ptve->c = psrf.dsdv[2];
         }
       ptve->x = psrf.sp[0];
       ptve->y = psrf.sp[1];
       ptve->z = psrf.sp[2];
       ptve->inv = 1;
       sv   = p;
       npt++;
       ptve++;
      }
   }

	/* create bspline and associate with curve */
		if (status == UU_SUCCESS)
			{
    status = ncl_interp_rbsp (npt, cpt, 1, unicrv); 
		 }

 uu_toolfree (cpt);

	uu_dexitstatus("ncl_interp_rbsp ()", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION: int um_rbcrv_from_surf(option,param, srf, unicrv)
**       Create a isoparametric U/V curve (UNICRV) from the specified 
**			NCL surface (SRF) at the given V/U parameter value (PARAM). 
**			The simplest isoparametric curve will be created in UNIBASE 
**			and returned.
**    PARAMETERS   
**       INPUT  : 
**         option   1 - U curve, 2 - V curve.
**         param				V/U parameter value at which to create the
**									         U/V isoparametric curve
**				     srf				  pointer to NCL surface entity
**       OUTPUT :  
**         unicrv			pointer to curve entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_rbcrv_from_surf(option,param, srf, unicrv)
	UM_param param;
 int option;
	struct NCL_surface_rec *srf;
	struct UC_rbsplcrv_rec *unicrv;

	{
	int status = UU_FAILURE;
 int i, npt, nin;
 UU_KEY_ID pkey;
 UM_transf tfmat;
	struct NCL_panel_rec spnl;
 struct UM_evsrfout psrf;
 struct NCL_crvgen_rec *ptve, *cpt;
	UU_REAL p0, p1, p, r, s, *ptuv;

	uu_denter(UU_MTRC, (us,"um_rbcrv_from_surf(param=%g, srfkey=%x)",
		param, srf->key));

 status = uc_retrieve_transf(srf->key, tfmat);
 nin  = srf->no_panelkey;

/*
...Determine parameter values,
...U curve - number of patches in panel; get panel first
*/
 if (option == 1)
   {
    s    = param * nin;
    i    = s;
    if (param == 1.0) i = nin - 1;
    spnl.key = srf->panelkey[i];
    status = ur_retrieve_data_fixed (&spnl); 
    npt  = spnl.no_param + 1;
   }
/*
...V curve - number of panels; spread v evenly
*/
 else
   {
    npt  = nin + 1;
    s    = 1.0 / nin; 
   }

 r    = param;
 cpt = (struct NCL_crvgen_rec *) uu_toolmalloc (npt*sizeof(struct NCL_crvgen_rec));
 ptve = cpt;
 for (i=0; i<npt; i++)
   {
    if (option == 1)
      {
       p = spnl.param[i];
       status = uc_evsrf(UM_FRSTDERIV, p, r, srf, tfmat, &psrf);
       ptve->a = psrf.dsdu[0];
       ptve->b = psrf.dsdu[1];
       ptve->c = psrf.dsdu[2];
      }
    else
      {
       p   = i * s;
       status = uc_evsrf(UM_FRSTDERIV, r, p, srf, tfmat, &psrf);
       ptve->a = psrf.dsdv[0];
       ptve->b = psrf.dsdv[1];
       ptve->c = psrf.dsdv[2];
      }
    ptve->x = psrf.sp[0];
    ptve->y = psrf.sp[1];
    ptve->z = psrf.sp[2];
    ptve->inv = 1;
    ptve++;
   }

/* 
...create bspline and associate with curve 
*/
		if (status == UU_SUCCESS)
                status = ncl_interp_rbsp (npt, cpt, 1, unicrv); 

 uu_toolfree (cpt);
	uu_dexitstatus("um_rbcrv_from_surf ()", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION: int um_rbcrv_from_msrf(option,param, srf, unicrv)
**       Create a isoparametric U/V curve (UNICRV) from the specified 
**			mesh surface (SRF) at the given V/U parameter value (PARAM). 
**			The simplest isoparametric curve will be created in UNIBASE 
**			and returned.
**    PARAMETERS   
**       INPUT  : 
**         option   1 - U curve, 2 - V curve.
**         param				V/U parameter value at which to create the
**									         U/V isoparametric curve
**				     srf				  pointer to mesh surface entity
**       OUTPUT :  
**         unicrv			pointer to curve entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_rbcrv_from_msrf(option,param, srf, unicrv)
	UM_param param;
 int option;
	struct NCL_meshsf_rec *srf;
	struct UC_rbsplcrv_rec *unicrv;

	{
	int status = UU_FAILURE;
 int i, npt, nin;
 UU_KEY_ID pkey;
 UM_transf tfmat;
 struct UM_evsrfout psrf;
 struct NCL_crvgen_rec *ptve, *cpt;
	UU_REAL p0, p1, p, r, s, *ptuv;

	uu_denter(UU_MTRC, (us,"um_rbcrv_from_surf(param=%g, srfkey=%x)",
		param, srf->key));

 status = uc_retrieve_transf(srf->key, tfmat);

/*
...Determine parameter values,
...U curve 
*/
 if (option == 1)
   {
    nin  = srf->m;
   }
/*
...V curve 
*/
 else
   {
    nin  = srf->n;
   }
 nin  = nin * 4; 
 s    = 1.0 / nin; 
 npt  = nin + 1;

 r    = param;
 cpt = (struct NCL_crvgen_rec *) uu_toolmalloc (npt*sizeof(struct NCL_crvgen_rec));
 ptve = cpt;
 for (i=0; i<npt; i++)
   {
    p = i * s;
    if (option == 1)
       {
        status = uc_evsrf(UM_FRSTDERIV, p, r, srf, tfmat, &psrf);
        ptve->a = psrf.dsdu[0];
        ptve->b = psrf.dsdu[1];
        ptve->c = psrf.dsdu[2];
       }
    else
       {
        status = uc_evsrf(UM_FRSTDERIV, r, p, srf, tfmat, &psrf);
        ptve->a = psrf.dsdv[0];
        ptve->b = psrf.dsdv[1];
        ptve->c = psrf.dsdv[2];
       }
    ptve->x = psrf.sp[0];
    ptve->y = psrf.sp[1];
    ptve->z = psrf.sp[2];
    ptve->inv = 1;
    ptve++;
   }
/* 
...create bspline and associate with curve 
*/
		if (status == UU_SUCCESS)
                status = ncl_interp_rbsp (npt, cpt, 1, unicrv); 

 uu_toolfree (cpt);
	uu_dexitstatus("um_rbcrv_from_surf ()", status);
	return (status);
 }

/*********************************************************************
**    E_FUNCTION     : int um_param_compare(a,b)
**       Function for comparing two parameter values along a curve.
**    PARAMETERS   
**       INPUT  : 
**          a						address of parameter on curve
**          b						address of parameter on curve
**       OUTPUT :  
**          none
**    RETURNS      : 
**				-1 if a < b
**				 0 if a = b
**				 1 if a > b
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_param_compare(a,b)
	UU_REAL *a;
	UU_REAL *b;

	{
	if (*a < *b) return (-1);
	else if (*a > *b) return (1);
	else return (0);
	}

