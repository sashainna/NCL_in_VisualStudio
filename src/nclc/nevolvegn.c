/*********************************************************************
**    NAME         :  nevolvegn.c
**    CONTAINS: Routines converting curves to array of points with
**              given chordal tolerance in respect of the CV curvature.
**              Applies to generic SF/CV defined thru evaluator only.
**
**       ncl_crvpt_atdis
**       ncl_get_sf_treflag
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nevolvegn.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:57
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdebug.h"
#include "mdattr.h"
#include "mdeval.h"
#include "modef.h"
#include "uminmax.h"
#include "ulist.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclxmdl.h"

extern int NCLX_internal_geom;
extern int NCL_fmill_past;

#define NULLST (UU_LIST *)UU_NULL

/********************************************************************
**    FUNCTION: ncl_crvpt_atdis (key, tol,pext,dis,pcv,tanvc,u,iext)
**
**    Finds point on crv at a given distance (dis) along the crv from 
**    the given point pext; If pext is not on crv, it's projected onto it.
**    Fortran callable  routine. Crv is treated as a polyline evaluated
**    with tolerance tol.
**    
**    PARAMETERS   
**       INPUT  : 
**          key    - curve key 
**          tol    - tolerance for crv evaluation
**          pext   - given point
**          dis    - distance from p0 along the crv
**          iext   - 0 iff pext is one of the curve endpoints, then
**                   the input value of u (0 or 1) indicates which one
**           
**       OUTPUT :  
**          pcv    - point on crv (polyline) at dist. dis from p0
**          tanvc  - tan. vector to crv at pcv
**          u      - u-param. of pcv on polyline
**          iext   - 0 if the resulting pt is within the curve; 
**                   1 when on extension
**    RETURNS      : 
**          UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_crvpt_atdis (key, tol,pext,dis,pcv,tanvc,u,iext)
UU_KEY_ID *key;
UU_REAL *tol, *dis, *u;
UM_coord pext,pcv,tanvc;
UM_int2 *iext;
{
	int status,np,i;
	UU_REAL u0,dst,d,d1,toler,length, um_getpolylen();
	UM_coord p0,tan, *p,*vc, *up;
	UU_LIST points,tvc, ulst;
	struct UM_crvdatabag crv;
	UM_transf tfmat, *tf;
	UU_LOGICAL fromendpoint = (*iext == 0);

	*iext = 1;

	toler = (*tol >= 1.e-4) ? *tol : 1.e-4;

	crv.key = *key;
	status = ncl_retrieve_data_fixed(&crv);

	if (status != UU_SUCCESS) return (UU_FAILURE);

	uu_list_init (&points, sizeof(UM_coord), 200, 200);
	uu_list_init (&tvc,  sizeof(UM_coord), 200, 200);
	uu_list_init (&ulst,  sizeof(UM_coord), 200, 200);

	if (ncl_itsa_compcrv(&crv))
	{
		np = ncl_evolve_composite_curve(&crv,toler,&points,&tvc,&ulst,0);
	}
	else
	{
		tf = &tfmat;
		status = ncl_trimsrf_get_tf (&crv,&tf);
		if (!status)
			np = ncl_evolve_curve (&crv,tf[0],toler,&points,&tvc,&ulst,0);
	}

	if (status != UU_SUCCESS) goto Done;

	np = UU_LIST_LENGTH(&points);
	if (np <= 1) 
	{
		status = UU_FAILURE;
		goto Done;
	}

	p  = (UM_coord *)UU_LIST_ARRAY(&points);
	vc = (UM_coord *)UU_LIST_ARRAY(&tvc);
	up = (UM_coord *)UU_LIST_ARRAY(&ulst);

	length = um_getpolylen (np,p);

	if (fromendpoint)
		u0 = *u;
	else
	{
		i = norm_to_cvonsf1 (pext,&u0,np,p,length,p0,tan);
		if (i <= 0)
		{
			status = UU_FAILURE;
			goto Done;
		}
	}

	dst = u0*length + (*dis);

	if (dst < 0.)
	{
		*u = 0.;
		i = 0;
	}
	else if (dst > length)
	{
		*u = 1.;
		i = np-1;
		dst = dst - length;
	}
	else
	{
		*iext = 0;

		for (i=0, d= 0.; i<np-1; i++)
		{
			d1 = UM_DCCCC (p[i],p[i+1]); d += d1;
			if (d >= dst) break;
		}
		dst = dst - (d - d1);
		*u = up[i][0];
	}
	um_unitvc (vc[i],tanvc);
	um_translate_point (p[i],dst,tanvc,pcv);

Done:;
	uu_list_free (&points);
	uu_list_free (&tvc);
	uu_list_free (&ulst);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_sf_treflag (sfp,triansf,tol)
**       Check if a surface is triangular.
**
**    PARAMETERS   
**       INPUT  : 
**          sfp     - surface pointer.
**          tol     - tolerance
**       OUTPUT :  
**          triansf  - 0 if not triangular
**                     1 if u = 0 side collapses to a point
**                     2 if u = 1 side collapses to a point
**                     3 if v = 0 side collapses to a point
**                     4 if v = 1 side collapses to a point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*******************************************************************/
void ncl_get_sf_treflag (sfp,triansf,tol)
struct NCL_fixed_databag *sfp;
int *triansf;
UU_REAL tol;
{
	int status,lfl;
	UM_transf tff;
	struct UM_evsrfout evout;
	UU_KEY_ID skey;
	UU_REAL u0,v0,u,v,d0,d1;
	UU_REAL tolsq;
	UM_coord sp00,sp01,sp10,sp11,spt;

	*triansf = 0;
	skey = sfp->key;

	tolsq = tol*tol;

	status = uc_retrieve_transf(sfp->key, tff);
	if (status == UU_SUCCESS)
		status = uc_init_evsrfout (sfp,&evout);

	if (status != UU_SUCCESS) return;

	lfl = 0;

	u = 0;
	v = 0;
	uc_evsrf (UM_POINT, u, v, sfp, tff, &evout);
	um_vctovc (evout.sp,sp00);
	v = 1;
	uc_evsrf (UM_POINT, u, v, sfp, tff, &evout);
	um_vctovc (evout.sp,sp01);

	d0 = UM_SQDIS (sp00,sp01);

	u = 1;
	v = 0;
	uc_evsrf (UM_POINT, u, v, sfp, tff, &evout);
	um_vctovc (evout.sp,sp10);
	v = 1;
	uc_evsrf (UM_POINT, u, v, sfp, tff, &evout);
	um_vctovc (evout.sp,sp11);

	d1 = UM_SQDIS (sp10,sp11);

	if (d0 < 0.5*tolsq && d1 > 1.21*tolsq) lfl = 1;
	if (d1 < 0.5*tolsq && d0 > 1.21*tolsq) lfl = 2;

	if (lfl == 0)
	{
		d0 = UM_SQDIS (sp00,sp10);
		d1 = UM_SQDIS (sp01,sp11);
		if (d0 < 0.5*tolsq && d1 > 1.21*tolsq) lfl = 3;
		if (d1 < 0.5*tolsq && d0 > 1.21*tolsq) lfl = 4;
	}

	if (lfl == 0) return;

	if (lfl < 3)
	{
		if (lfl == 1)
		{
			u0 = 0;
			um_middlept (sp00,sp01,spt);
		}
		else
		{
			u0 = 1;
			um_middlept (sp10,sp11,spt);
		}
		v = 0.333;
		uc_evsrf (UM_POINT, u0, v, sfp, tff, &evout);
		if (UM_SQDIS (evout.sp,spt) > tolsq) return;

		v = 0.666;
		uc_evsrf (UM_POINT, u0, v, sfp, tff, &evout);
		if (UM_SQDIS (evout.sp,spt) > tolsq) return;
	}
	else
	{
		if (lfl == 3)
		{
			v0 = 0;
			um_middlept (sp00,sp10,spt);
		}
		else
		{
			v0 = 1;
			um_middlept (sp01,sp11,spt);
		}
		u = 0.333;
		uc_evsrf (UM_POINT, u, v0, sfp, tff, &evout);
		if (UM_SQDIS (evout.sp,spt) > tolsq) return;

		u = 0.666;
		uc_evsrf (UM_POINT, u, v0, sfp, tff, &evout);
		if (UM_SQDIS (evout.sp,spt) > tolsq) return;
	}
	
	*triansf = lfl;
	return;
}
