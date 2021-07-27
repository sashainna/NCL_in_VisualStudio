/*********************************************************************
**    NAME         :  nepsmult.c
**       CONTAINS:  Routines to support multiple part surfaces.
**
**              ncl_psmult_init
**              ncl_psmult_free
**              ncl_psmult_isinit
**              ncl_psmult_load
**              ncl_psmult_sfinit
**              ncl_psmult_save
**              ncl_psmult_chk
**              ncl_psmult_select
**              ncl_psmult_use
**              ncl_cvonsf_store_mult
**              ncl_load_curve
**              ncl_tool_ps_rel
**              ncl_create_tcyl
**              ncl_smooth_corner
**              ncl_psmult_setad2
**              ncl_change_tcyl_ps
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nepsmult.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/16/19 , 15:05:38
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "nclfc.h"
#include "ulist.h"
#include "ncldef.h"
#include "nccs.h"
#include "mgeom.h"
#include "mdeval.h"
#include "ycom.h"
#include "msrfddl.h"
#include "uminmax.h"
#include "nclpsmult.h"
#include "uminmax.h"

extern int NCLX_internal_geom;

static NCL_psmult_rec *NCL_psmult_ptr = UU_NULL;
static int NCL_psmult_nsf = 0;

static UM_netsf_common_bndry NCL_netps_common_bndry;
static int common_boundary_is_init = 0;

/*
... 2 cylinders representing the tool for net PS:
... NCL_tcyl_use = cylinder to determine which PS should be analyzed 
... NCL_tcyl_ps = cylinder to be used to set tool-PS relation 
*/

static UM_cylinder NCL_tcyl_use;
UM_cylinder NCL_tcyl_ps;
static UU_LOGICAL lfnd;
static UU_LOGICAL lv90 = UU_FALSE;
static UU_LOGICAL lv93 = UU_FALSE;
static UU_LOGICAL lv97 = UU_FALSE;
static int stepup;

char *uu_malloc();

/*
.....Debug flag
*/
#define DEBUGON 0
#ifdef DEBUGON
		static char tbuf[80];
		static char *dbgptyp[] = {"NCL_ON_SURFACE", "NCL_ON_CORNER",
			"NCL_ON_CLOSE_EXTENSION", "NCL_ON_SIDE_EXTENSION",
			"NCL_ON_FAR_EXTENSION", "NCL_DISCARD_SURFACE", "NCL_DISCARD_CORNER",
			"NCL_DISCARD"};
		static char *dbgpta[] = {"FIXED", "NORMAL", "ATANGL", "TANTO_DS",
			"FAN", "TANTO_PERPTO", "TANTO_PARELM", "NORMAL_PERPTO", "COMBIN",
			"COMBIN_PARELM", "ATANGL_PERPTO", "ATANGL_CLDIST",
			"ATANGL_CLDIST_PERPTO","THRU_PT","THRU_CV"};
#endif

/*********************************************************************
**    E_FUNCTION     : int ncl_psmult_init (nclkey,tol,imult,iscurve)
**      Initialize the multips list.
**    PARAMETERS
**       INPUT  :
**          nclkey - Key of part surface.
**          tol    - tolerance for srf boundary  evaluation
**          iscurve - 0 if PS is a net srf; 1 if PS is a CVonSF
**       OUTPUT :
**          imult  - 1 iff multiple PS else 0
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_init (nclkey, tol, imult, iscurve)
UM_int4 *nclkey;
UU_REAL *tol;
UM_int2 *imult, *iscurve;
{
	int i,isf,is_curve;
	int status = UU_SUCCESS;
	NCL_psmult_rec *p1 = UU_NULL;
	struct NCL_fixed_databag e1,*psf, e2;
	struct NCL_netsf_rec *pnetsf;

	UM_int2 idx;
	UU_REAL ver;

	idx = 169;
	getsc (&idx,&ver);
	lv90 = (ver < 9.049);
	lv93 = (ver <= 9.349);
	lv97 = (ver <= 9.749);

	stepup = 0;
	*imult = 0;
	is_curve = *iscurve;

	ncl_psmult_free ();
	NCL_psmult_nsf = 0;

	if (NCLX_internal_geom)
	{
		NCL_psmult_nsf = UY_nps;
		if (UY_nps>1) *imult = 1;
/*
... UY_ps was changed after first motion attempt failed.
... jingrong 01/09/2000.
		pnetsf = (struct NCL_netsf_rec *) UY_ps;
*/
		pnetsf = (struct NCL_netsf_rec *) UY_hldps;
	}
	else
	{
		is_curve = UU_FALSE;
		e1.key = *nclkey;
		status = ncl_retrieve_data_fixed (&e1);
		if(status != UU_SUCCESS) return (status);
		if (e1.rel_num == NCL_NETSF_REL)
		{
			*imult = 1;
			pnetsf = (struct NCL_netsf_rec *)&e1;
			NCL_psmult_nsf = pnetsf->no_netkey;
		}
/*
... aak 18-nov-1997: include comp. curves on SF as multi-PS
*/
		else if (ncl_itsa_compcrv(&e1))
		{
			struct NCL_fixed_databag compcv;
			int ncv, rev;
			status = ncl_compcrv_getnents(&e1,&ncv);
			if(ncv < 2) return (status);
			for(i=0; status == UU_SUCCESS && i<ncv; i++)
			{
				status = ncl_compcrv_getelm(&e1,i,&compcv,&rev);
				status = !ncl_itsa_uvcv_onsf(&compcv); 
			}
			if(status == UU_SUCCESS) 
			{
				is_curve = UU_TRUE;
				*imult = 1; 
				NCL_psmult_nsf = ncv;
			}
		}
	}

	if (*imult == 0) return (status);

	if (status == UU_SUCCESS)
	{
		p1 = NCL_psmult_ptr = (NCL_psmult_rec *)
			uu_malloc(NCL_psmult_nsf*sizeof(NCL_psmult_rec));
		if (!p1) status = UU_FAILURE;
	}

	ncl_set_boundary_toler (*tol);

	for (isf=0; isf< NCL_psmult_nsf && status == UU_SUCCESS; isf++, p1++)
	{
		p1->key = (UU_KEY_ID) 0;
		p1->bskey = (UU_KEY_ID) 0;
		p1->linit = UU_FALSE;
		p1->extflg = NCL_DISCARD;
		p1->iuse = p1->iclose = p1->ierr = 0;
		p1->select = 1.e+12;
		p1->cvlen = 0.;
		for (i = 0; i < 8; i++)
			um_nullvc((p1->box).ver[i]);

		p1->ps.ad2 = 0.;
/*
... aak 18-nov-1997: if composite CVonSf, only initialize the lists;
...                  if surface, fill in the boundary & box data
*/
		if (is_curve)
		{
			p1->bound.nb = 0;
			p1->bound.np   = UU_NULL;
			p1->bound.vmmx = p1->bound.ummx = UU_NULL;

			p1->bound.cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
			p1->bound.uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
			uu_list_init (p1->bound.cvpts, sizeof(UM_coord), 100, 100);
			uu_list_init (p1->bound.uvpts, sizeof(UM_coord), 100, 100);
		}
		else
		{
			if (NCLX_internal_geom)
			{
				UY_ps = UY_netps[isf];
				p1->key = UY_ps->key;
				psf = (struct NCL_fixed_databag *)UY_ps;
			}
			else 
			{
				p1->key = e2.key = pnetsf->netkey[isf];
				if (ncl_retrieve_data_fixed (&e2))  return UU_FAILURE;
				psf = &e2;
			}

			if (status == UU_SUCCESS) 
				status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,psf, &p1->bound);

			if (status == UU_SUCCESS) 
			{
				if (!NCLX_internal_geom) 
				{
					if (ncl_retrieve_data_fixed (&e2))  return UU_FAILURE;
					psf = &e2;
				}
				status = ncl_get_box (psf, &p1->box);
			}

/*
... Debug: display box
			if (!status) ncl_box_disp (&p1->box.ver, 3); 
*/
		}
	}

	if (status == UU_SUCCESS && !is_curve) 
	{
		status = ncl_get_common_bndry (pnetsf,&NCL_netps_common_bndry);
		if (status == UU_SUCCESS) common_boundary_is_init = 1;
	}

	if (status != UU_SUCCESS) ncl_psmult_free();

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_psmult_free ()
**      Free the multips list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_free ()
{
	int isf, status = UU_SUCCESS;
	NCL_psmult_rec *p1 = UU_NULL;

	if (NCL_psmult_ptr)
	{
		p1 = NCL_psmult_ptr;
		for (isf=0; isf<NCL_psmult_nsf; isf++, p1++)
		{
			p1->bskey = 0;
			um_free_boundary (&p1->bound);
		}
		uu_free (NCL_psmult_ptr);
		NCL_psmult_ptr = UU_NULL;
	}

	if (common_boundary_is_init) 
		um_free_netsf_common_boundary (&NCL_netps_common_bndry);

	common_boundary_is_init = 0;

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_psmult_isinit (isf, ild)
**      Determine if this surface is initialized.
**    PARAMETERS
**       INPUT  :
**          isf    - Index in multips list of surface to test.
**       OUTPUT :
**          ild    - 1 = initialized, 0 = not initialized.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_isinit (isf, ild)
UM_int2 *isf, *ild;
{
	int status = UU_SUCCESS;
	NCL_psmult_rec *p1;

	p1 = NCL_psmult_ptr + *isf - 1;
	*ild = (p1->linit) ? 1 : 0;

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_psmult_load (isf, t1, t2, t3, ad2, cuv)
**      Return part surface data from multips list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          isf    - Index of surface in multi ps list.
**          t1     - tool table (13-18,1).
**          t2     - tool table (13-18,2).
**          t3     - tool table (13-18,3).
**          ad2    - reverse normal flag.
**          cuv    - u & v values for each gougck starting position.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_load(isf, t1, t2, t3, ad2, cuv)
UM_int2 *isf;
UM_real4 *t1, *t2, *t3, *ad2, *cuv;
{
	int i, status = UU_SUCCESS;
	NCL_psmult_rec *p1;

	p1 = NCL_psmult_ptr + *isf - 1;
	for (i=0; i<6; i++)
	{
		t1[i] = p1->ps.t1[i];
		t2[i] = p1->ps.t2[i];
		t3[i] = p1->ps.t3[i];
	}
	*ad2 = p1->ps.ad2;
	for (i=0; i<10; i++) cuv[i] = p1->ps.cuv[i];

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_psmult_sfinit(isf, t1, t2, t3, ad2, cuv)
**       Save initial ps data in multi ps list.
**    PARAMETERS
**       INPUT  :
**          isf    - Index of surface in multi ps list.
**          t1     - tool table (13-18,1).
**          t2     - tool table (13-18,2).
**          t3     - tool table (13-18,3).
**          ad2    - reverse normal flag.
**          cuv    - Part surface gouge check uv values.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_sfinit(isf, t1, t2, t3, ad2, cuv)
UM_int2 *isf;
UM_real4 *t1, *t2, *t3, *ad2, *cuv;
{
	int i, status = UU_SUCCESS;
	NCL_psmult_rec *p1;

	p1 = NCL_psmult_ptr + *isf - 1;
	if (p1->ps.ad2 == 0.)
		p1->ps.ad2 = *ad2;
	p1->linit = UU_TRUE;
	for (i=0; i<6; i++)
	{
		p1->ps.t1[i] = t1[i];
		p1->ps.t2[i] = t2[i];
		p1->ps.t3[i] = t3[i];
	}
	for (i=0; i<10; i++) p1->ps.cuv[i] = cuv[i];

	return (status);
}

/*********************************************************************
**    FUNCTION: int ncl_psmult_save(isf,t1,t2,t3,s1,cuv,d1,extflg,ierr,ad2)
**       Save ps data in multi ps list.
**    PARAMETERS
**       INPUT  :
**          isf    - Index into multi part surface list.
**          t1     - Tool table (13-18,1).
**          t2     - Tool table (13-18,2).
**          t3     - Tool table (13-18,3).
**          s1     - Surf table (1-10,1).
**          cuv    - Gougck uv values.
**          d1     - Distance to part surface.
**          extflg - = 1 if point in on extension of surface.
**          ierr   - >0 if an error occured for this surface.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_save(isf, t1, t2, t3, s1, cuv, d1, extflg, ierr,ad2)
UM_int2 *isf;
UM_real4 *t1, *t2, *t3, *s1, *cuv,*ad2;
UM_real8 *d1;
UM_int2 *extflg, *ierr;
{
	int i, status = UU_SUCCESS;
	NCL_psmult_rec *p1;

	p1 = NCL_psmult_ptr + *isf - 1;
	for (i=0; i<6; i++)
	{
		p1->ps.t1[i] = t1[i];
		p1->ps.t2[i] = t2[i];
		p1->ps.t3[i] = t3[i];
	}
	for (i=0; i<10; i++)  p1->ps.s1[i]  = s1[i];
	for (i=0; i<10; i++) p1->ps.cuv[i] = cuv[i];
	if (p1->ps.ad2 == 0.)
		p1->ps.ad2 = *ad2;

	p1->select = *d1;
	p1->extflg = *extflg;
	p1->ierr   = *ierr;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_psmult_chk ()
**       Check if each part surface is near cutter. If no surface is 
**       close, set iuse flag for the closest surface only.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS :
**         sets iuse flag for each part surface
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_chk (tdelt)
UM_real4 *tdelt;
{
	int isf, status = UU_SUCCESS;
	UU_LOGICAL lclose = UU_FALSE;
	NCL_psmult_rec *p1, *best;
	UU_REAL dist0, dist, twodiam;
	lfnd = UU_FALSE;
/*
..... jingrong 6/3/99. lfnd is made global, to use in ncl_psmult_select.
*/
	best = UU_NULL;
	dist = 0.;
	dist0 = 1.e9;
	if (lv97) twodiam = 2. * NCL_tcyl_use.radius;
	else twodiam = NCL_tcyl_use.radius + *tdelt;
/*
..... twodiam is twice the cutter diameter.
*/
	p1 = NCL_psmult_ptr;
	for (isf=0; isf<NCL_psmult_nsf; isf++,p1++)
	{
		if (p1 == UU_NULL) return (UU_FAILURE);

		if (p1->key > 0)
		{
/*
..... At first we look for surfaces intersecting the (infinite) cylinder,
..... whose radius equals the cutter diameter; and set the iuse flag
..... on those found.
*/
			p1->iclose = ncl_cylinder_cross_ps (&NCL_tcyl_use,p1,&dist);
			p1->iuse = p1->iclose;
			lfnd = lfnd || p1->iuse;
			if (!lfnd && !lv90)
			{ 
/*
..... If the cylinder has not intersected any surface, we check if 
..... some surfaces are within two diameters from the cylinder axis;
..... and set the flag iuse on these.
*/  
				if (dist < twodiam) 
				{
					p1->iclose = 1;
					lclose = UU_TRUE;
				}
/*
..... If the wider (twodiam) cylinder has not intersected any surface, 
..... we find the nearest surface; and set the iuse flag on this one only.
*/
				else if ((dist < dist0) && !lclose)
				{
					dist0 = dist;
					best = p1;
				}
			}
		}
		else
		{
			p1->iuse = 1;
			lfnd = UU_TRUE;
		}
		p1->ierr = 0;
	}

	if (!lfnd)
	{
		p1 = NCL_psmult_ptr;
		if (lv90)
		{
			for (isf=0; isf<NCL_psmult_nsf; isf++, p1++)
				p1->iuse = 1;

		}
		else if (lclose)
		{
			for (isf=0; isf<NCL_psmult_nsf; isf++, p1++) 
				p1->iuse = p1->iclose;
		}
		else 
		{
			if (best == UU_NULL) return (UU_FAILURE);
			best->iuse = 1;
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION : int ncl_psmult_select (tool, s, isf, reset_dp,ierr)
**       Return best part surface plane & surface point.
**       Priority levels for selecting the PS are determined by surface 
**       extension flags. Within each level, the srf with min (select) 
**       parameter is selected
**    PARAMETERS
**       INPUT  :
**          ta_mode - tool axis mode
**          tool   - tool structure. 
**       OUTPUT :
**          s        - Best part surface plane & surface point.
**          isf      - Index of surface selected.
**          reset_dp - flag: if 1, sets go_slow flag in mover.
**          ierr     - =1 iff error.
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_select (tool,ta_mode,s,isf,reset_dp,ierr)
NCL_tool *tool;
UM_real4 *s;
UM_int2 *ta_mode,*isf,*reset_dp, *ierr;
{
	NCL_psmult_rec *p = UU_NULL;
	NCL_psmult_rec *p1 = UU_NULL;
	NCL_psmult_rec *best = UU_NULL;

	int numsf,i,j,level,isf0,status,discard,ngood;
	UU_REAL d,d0,d1,told,d2,co,bestco,dbes,r1;
	UU_LOGICAL filter = UU_FALSE;
	UU_LOGICAL upward = UU_FALSE;
	UU_LOGICAL lone = UU_FALSE;

	int onsurf = 0;
	int oncor = 0;
	int closext = 0;
	UU_LOGICAL sidext = UU_FALSE;
	UU_LOGICAL farext = UU_FALSE;
	UU_LOGICAL lbes = UU_FALSE;

	UM_coord lkpt,tend;
	UM_vector vec;

	int priority_levels = 4; /* num. of priority levels for PS selection */

	UM_int2 ifl2,iptk,idx;

	numsf = i = j = level = isf0 = discard = ngood = 0;
	d = d1 = told = d2 = co = bestco = dbes = 0.;
	ifl2 = iptk = 0;

	for (j = 0; j < 3; j++)
	{
		vec[j] = lkpt[j] = 0.;
	}

	status = UU_SUCCESS;

	ifl2 = *ierr;
	*ierr = 1;
	isf0 = *isf;

	p = NCL_psmult_ptr;

	for (i=0; i<NCL_psmult_nsf && *ierr; i++, p++)
		if (p->ierr <= 0) *ierr = 0;

	if (*ierr && !lv90) return (UU_FAILURE);

	if (lv90)
	{
		best = UU_NULL;
		p = NCL_psmult_ptr;
		d = -1.0e9;

		for (i=0; i<NCL_psmult_nsf; i++, p++)
		{
			if (p->iuse && !p->extflg && !p->ierr && p->select > d)
			{
				best = p;
				d = p->select;
				*isf = i+1;
			}
		}

		if (!best)
		{
			p = NCL_psmult_ptr;
			for (i=0; i<NCL_psmult_nsf; i++, p++)
			{
				if (p->iuse && p->extflg==2 && !p->ierr && p->select > d)
				{
					best = p;
					d = p->select;
					*isf = i+1;
				}
			}
		}

		if (!best)
		{
			p = NCL_psmult_ptr;
			d = 1.0e9;
			for (i=0; i<NCL_psmult_nsf; i++, p++)
			{
				if (p->iuse && !p->ierr && p->select < d)
				{
					best = p;
					d = p->select;
					*isf = i+1;
				}
			}
		}
	
		if (best) for (i=0; i<10; i++) s[i] = best->ps.s1[i];
		return (status);
	} /* End of old (ncl90) logic */

/*
... find how many surfaces are considered
*/
	p = NCL_psmult_ptr;
	for (i = 0, numsf = 0, ngood = 0; i < NCL_psmult_nsf ; i++, p++)
	{
		if (p->ierr <= 0 && p->iuse)
		{
			numsf++;
			if (p->extflg < NCL_DISCARD_SURFACE || p->select < 2*tool->diameter)
				ngood++;
		}
	}
	if (numsf <= 0) return (UU_FAILURE);

	if (numsf > 1)
	{
		p = NCL_psmult_ptr;
		for (i=0; i<NCL_psmult_nsf; i++, p++)
		{
			if (p->ierr <= 0 && p->iuse)
			{
				if (p->extflg == NCL_ON_SURFACE) onsurf++; 
				else if (p->extflg == NCL_ON_CORNER) oncor++;
				else if (p->extflg == NCL_ON_SIDE_EXTENSION) sidext = UU_TRUE;
				else if (p->extflg == NCL_ON_FAR_EXTENSION) farext = UU_TRUE;
				else if (p->extflg == NCL_ON_CLOSE_EXTENSION) closext++;
			}
		}
		getsct (&told);
/*
... if there are no corners, upgrade NCL_ON_SIDE_EXTENSION surfaces
... to NCL_ON_CLOSE_EXTENSION (for non-flat tool & when not 'upward')
*/
		if (tool->corner_radius > told)
		{
			upward = um_dot(tool->end_forward, tool->axis) > 0.5;

			if (oncor == 0 && !upward && sidext)
			{
				UU_REAL um_dist_from_line();
				p = NCL_psmult_ptr;
				d1 = 1.0e9;
				if (onsurf > 0)
				{
					p1 = NCL_psmult_ptr;
					for (i = 0; i < NCL_psmult_nsf; i++, p1++)
					{
						if (p1->ierr <= 0 && p1->iuse &&
							p1->extflg == NCL_ON_SURFACE && p1->select < d1)
								d1 = p1->select;
					}
				}

				for (i = 0; i < NCL_psmult_nsf; i++, p++)
				{
					if (p->ierr <= 0 && p->iuse && 
						p->extflg == NCL_ON_SIDE_EXTENSION)
					{
/*
..... this is done to get rid of a mismatch in Dassault NCL117, motion 10
*/
						if (p->select < d1 &&
							p->select + 0.15*tool->corner_radius > d1)
						{
							d = um_dist_from_line(&p->ps.s1[4], &(NCL_tcyl_ps.axis));
							if (d + told > NCL_tcyl_ps.radius)
								continue;
						}
						p->extflg = NCL_ON_CLOSE_EXTENSION;
						closext++; /* QAR 92249 - to fix error 141 in hdh.pp */
					}
				}
			}
		}
/*
..... if we have both NCL_ON_SURFACE and NCL_ON_CLOSE_EXTENSION types:
..... downgrade the latter to NCL_ON_FAR_EXTENSION if moving down on
..... the closest NCL_ON_SURFACE plane will not violate the 
..... NCL_ON_CLOSE_EXTENSION surface - FSR 60409
*/
		if (*ta_mode != NCL_TA_NORMAL_PS && *ta_mode != NCL_TA_TANTO_DS)
		{
			if (onsurf > 0 && (oncor > 0 || closext > 0))
			{
				p = NCL_psmult_ptr;
				d = 1.0e9;
				best = UU_NULL;
				for (i=0; i<NCL_psmult_nsf; i++, p++)
				{
					if (p->ierr <= 0 && p->iuse &&
						p->extflg == NCL_ON_SURFACE && p->select < d)
					{
						best = p; d = p->select;
					}
				}

				if (best)
				{
					um_vcmnvc (tool->end,&best->ps.s1[4],vec);
					d0 = d1 = um_dot (vec,best->ps.s1);
/*
........First nest tool to select part surface
*/
					if (!lv97)
					{
						um_vcmnvc(tool->look_pt,&best->ps.s1[4],vec);
						um_translate_point(&best->ps.s1[7],
							-(best->select-tool->corner_radius),
							best->ps.s1,tend);
						d2 = um_dcccc(tend,&best->ps.s1[4]);
					}
					d += UM_FUZZ;

					p = NCL_psmult_ptr;
					for (i=0; d1 >= -2.*told && i<NCL_psmult_nsf; i++, p++)
					{
						if (p->extflg == NCL_ON_CLOSE_EXTENSION &&
							p->ierr <= 0 && p->iuse && p->select < d)
						{
							if (lv97 || um_dcccc(&best->ps.s1[7],&p->ps.s1[7]) > told)
							{
								um_vcmnvc (tool->end,&p->ps.s1[4],vec);
								r1 = um_dot(vec,best->ps.s1);
								d1 = d0;
							}
							else
							{
								um_translate_point(&p->ps.s1[7],
									-(best->select-tool->corner_radius),
									best->ps.s1,tend);
								r1 = um_dcccc(tend,&p->ps.s1[4]);
								d1 = d2;
#if DEBUGON == 2
								um_translate_point(tool->end,
									-(best->select-tool->corner_radius),
									best->ps.s1,tend);
								sprintf(tbuf,"sf = %d   d1 = %g   r1 = %g",i+1,d1,r1);
								NclxDbgPstr(tbuf);
								sprintf(tbuf,"tend = %g,%g,%g",tend[0],tend[1],tend[2]);
								NclxDbgPstr(tbuf);
#endif
							}

							if (r1+told - d1 > 0.)
								p->extflg = NCL_ON_FAR_EXTENSION;
						}
						else if (p->extflg == NCL_ON_CORNER &&
							p->ierr <= 0 && p->iuse && p->select < d)
						{
							um_vcmnvc (&best->ps.s1[4],&p->ps.s1[4],vec);
							if (um_dot (vec,best->ps.s1) + told > 0.)
							{
								p->extflg = NCL_ON_FAR_EXTENSION;
								oncor--;
							}
						}
					}
				}
			}
/*
..... if we have no NCL_ON_SURFACE and several competing NCL_ON_CLOSE_EXTENSION 
..... types: between two NCL_ON_CLOSE_EXTENSION planes select a lower one, if 
..... moving down onto it will not violate the surface point for the higher
..... one - FSR 60144
*/
			else if (lv93)
			{
				if (closext > 1)
				{
					p = NCL_psmult_ptr;

					for (i=0; i<NCL_psmult_nsf; i++, p++)
					{
						if (p->extflg == NCL_ON_CLOSE_EXTENSION &&
								p->ierr <= 0 && p->iuse)
						{
							best = p; d = p->select + UM_FUZZ;
							um_vcmnvc (tool->end,&p->ps.s1[4],vec);
							d1 = um_dot (vec,best->ps.s1);
							if (d1 < -2.*told) continue;

							p1 = NCL_psmult_ptr;

							for (j=0; j<NCL_psmult_nsf; j++, p1++)
							{
								if (j != i && p1->extflg == NCL_ON_CLOSE_EXTENSION &&
									p1->ierr <= 0 && p1->iuse && p1->select < d)
								{
									um_vcmnvc (tool->end,&p1->ps.s1[4],vec);
									if (um_dot (vec,best->ps.s1) - d1 > told)
										p1->extflg = NCL_ON_FAR_EXTENSION;
								}
							}
						}
					}
				}
			}
			else
			{
				if (closext > 0 && closext + oncor > 1)
				{
					p = NCL_psmult_ptr;

					for (i=0; i<NCL_psmult_nsf; i++, p++)
					{
						if ((p->extflg == NCL_ON_CLOSE_EXTENSION || 
								p->extflg == NCL_ON_CORNER) &&
								p->ierr <= 0 && p->iuse)
						{
							best = p; d = p->select + UM_FUZZ;
							um_vcmnvc (tool->end,&p->ps.s1[4],vec);
							d1 = um_dot (vec,best->ps.s1);
							if (d1 < -2.*told) continue;
							bestco = um_dot (tool->axis,best->ps.s1);

							p1 = NCL_psmult_ptr;
	
							for (j=0; j<NCL_psmult_nsf; j++, p1++)
							{
								if (j != i && p1->extflg == NCL_ON_CLOSE_EXTENSION &&
									p1->ierr <= 0 && p1->iuse && p1->select < d)
								{
									um_vcmnvc (tool->end,&p1->ps.s1[4],vec);
									d2 = um_dot (vec,best->ps.s1);
									co = um_dot (tool->axis,p1->ps.s1);
									if (best->extflg == NCL_ON_CLOSE_EXTENSION &&
																			d2 - d1 > told)
										p1->extflg = NCL_ON_FAR_EXTENSION;
									else if (d2 > told && co < 0.6 && 
										bestco > 0.7071 && tool->cut_edge < told)
									{
										um_translate_point(tool->end,tool->corner_radius,
											tool->axis, lkpt);
										um_vcmnvc (lkpt,&p1->ps.s1[4],vec);
										d2 = um_dot (vec,p1->ps.s1);
										d2 = sqrt(um_dot(vec,vec) - d2*d2);

										if (d2 > 0.25*tool->corner_radius)
											p1->extflg = NCL_ON_FAR_EXTENSION;
									}
								}
							}
						}
					}
				}
				else if (closext == 0 && oncor > 0 && onsurf == 0 && upward && farext &&
					tool->cut_edge < told)
				{
					p = NCL_psmult_ptr;
					d = 100000000.;

					for (i=0; i<NCL_psmult_nsf; i++, p++)
					{
						if (p->extflg == NCL_ON_CORNER && p->ierr <= 0 && p->iuse &&
							p->select < d)
							d = p->select;
					}
					if (d > tool->diameter)
					{
						p = NCL_psmult_ptr;

						for (i=0; i<NCL_psmult_nsf; i++, p++)
						{
							if (p->extflg == NCL_ON_FAR_EXTENSION && p->ierr <= 0 && 
								p->iuse && p->select < d)
							{
								d1 = um_dot(tool->axis,p->ps.s1);
								if (fabs (d1) < 0.5)
								{
									um_translate_point(tool->end,tool->corner_radius,
										tool->axis, lkpt);
									um_vcmnvc (lkpt,&p->ps.s1[4],vec);
									d2 = um_dot (vec,p->ps.s1);
									d2 = sqrt(um_dot(vec,vec) - d2*d2);
									if (d2 < 10.*told)
									{
										d2 = um_dot(&p->ps.s1[4],tool->axis) - 
												um_dot(tool->end,tool->axis);
										if (d2 > 0.25*tool->corner_radius)
										{
											p->extflg = NCL_ON_CLOSE_EXTENSION;
											p->select = um_mag(vec);
										}
									}
								}
							}
						}
					}
				}
			}
		}
		if (tool->corner_radius > told && oncor > 1 && numsf > 2)
		{
/*
..... this is added for Dassault NCL84 - there is a corner ahead and a 
..... smooth corner aside, which should be disregarded.
*/
			p = NCL_psmult_ptr;
			oncor = 0;
			for (i = 0; i < NCL_psmult_nsf; i++, p++)
			{
				if (p->ierr <= 0 && p->iuse && p->extflg == NCL_ON_CORNER && 
					p->iclose >= 0)
				{
					oncor++; break;
				}
			}
			if (oncor > 0)
			{
				p = NCL_psmult_ptr;
				for (i = 0; i < NCL_psmult_nsf; i++, p++)
				{
					if (p->ierr <= 0 && p->iuse && p->extflg == NCL_ON_CORNER && 
						p->iclose < 0)
					{
						p1 = NCL_psmult_ptr - p->iclose - 1;
						if (p1->ierr <= 0 && p1->iuse && 
								p1->extflg >= NCL_ON_FAR_EXTENSION)	
							p->extflg = NCL_ON_FAR_EXTENSION;
					}
				}
			}
		}
		if (!lv93 && tool->corner_radius < told &&
			(*ta_mode == NCL_TA_NORMAL_PS || *ta_mode == NCL_TA_TANTO_DS) &&
				closext == 0 && oncor > 0 && oncor + onsurf > 1)
		{
			best = UU_NULL; dbes = -told;
			p = NCL_psmult_ptr;
			for (i = 0; i < NCL_psmult_nsf; i++, p++)
			{
				if (p->ierr > 0 || p->iuse == 0 || 
					(p->extflg != NCL_ON_CORNER && p->extflg != NCL_ON_SURFACE))
					continue;
				um_vcmnvc (tool->end,&p->ps.s1[4],vec);
				d = um_dot(p->ps.s1,vec);
				if (d > dbes)
				{
					p1 = NCL_psmult_ptr;
					lbes = UU_TRUE;
					for (j = 0; j < NCL_psmult_nsf && lbes; j++, p1++)
					{
						if (p1 == p || p1->ierr > 0 || p1->iuse == 0 ||
						(p1->extflg != NCL_ON_CORNER && p1->extflg != NCL_ON_SURFACE))
							continue;
						um_vcmnvc (tool->end,&p1->ps.s1[4],vec);
						d1 = um_dot(p->ps.s1,vec);
						lbes = (d1 > d - told);
					}
					if (lbes)
					{
						best = p; dbes = d;
					}
				}
			}
			if (best)
			{
				p = NCL_psmult_ptr;
				for (i = 0; i < NCL_psmult_nsf; i++, p++)
				{
					if (p != best && p->ierr <= 0 && p->iuse &&
						(p->extflg == NCL_ON_CORNER || p->extflg == NCL_ON_SURFACE))
						p->extflg = NCL_ON_FAR_EXTENSION;
				}
			}
		}

		lone = (!lv93 && onsurf > 0 && ngood < 2);
	}
/*
.....Debug output of logic type
*/
#if DEBUGON == 2
	{
		p = NCL_psmult_ptr;
		for (i=0; i<NCL_psmult_nsf; i++, p++)
		{
			if (p->iuse)
			{
				NclxDbgPstr(" ");
				sprintf(tbuf,"Projection type[%d] = %s",i+1,dbgptyp[p->extflg]);
				NclxDbgPstr(tbuf);
				sprintf(tbuf,"Iclose = %d   ierr = %d   Select = %g   Iuse = %d",
					p->iclose,p->ierr,p->select,p->iuse);
				NclxDbgPstr(tbuf);
				sprintf(tbuf,"Surface point = %g,%g,%g",p->ps.s1[4],p->ps.s1[5],
					p->ps.s1[6]);
				NclxDbgPstr(tbuf);
				sprintf(tbuf,"Surface normal = %g,%g,%g",p->ps.s1[0],p->ps.s1[1],
					p->ps.s1[2]);
				NclxDbgPstr(tbuf);
				sprintf(tbuf,"Surface distance = %g",p->ps.s1[3]);
				NclxDbgPstr(tbuf);
				sprintf(tbuf,"Surface lookpt = %g,%g,%g",p->ps.s1[7],p->ps.s1[8],
					p->ps.s1[9]);
				NclxDbgPstr(tbuf);
			}
		}
	}
#endif

	d = 1.0e9;
	best = UU_NULL;
	discard = 0;
	p = NCL_psmult_ptr;

	for (level = 0; level < priority_levels && !best; level++)
	{
		p = NCL_psmult_ptr;
		for (i=0; i<NCL_psmult_nsf; i++, p++)
		{
			switch (level)
			{
				case 0:
					filter =    p->extflg == NCL_ON_SURFACE 
								|| p->extflg == NCL_ON_CLOSE_EXTENSION
								|| p->extflg == NCL_ON_CORNER;
					break;
				case 1:
					filter =    p->extflg == NCL_ON_FAR_EXTENSION
								|| p->extflg == NCL_ON_SIDE_EXTENSION;
					break;
				case 2:
					filter =    p->extflg == NCL_DISCARD_SURFACE
					         || p->extflg == NCL_DISCARD_CORNER;
					if ((p->iuse || lv97) && p->extflg == NCL_DISCARD_CORNER)
						discard = 1;
					break;
				case 3:
					filter = p->extflg == NCL_DISCARD;
					break;
			}

			if (filter && p->iuse && p->ierr <= 0 && p->select < d)
			{
				best = p;
				d = p->select;
				*isf = i+1;
			}
		}
	}
/*
... aak 12-may-1998. watch out for close surfaces; 
... if there is a PS close enough to tool
... which is not yet used, decrease maxdp.
... d parameter is empirical and might need further adjustment.
*/
	p = NCL_psmult_ptr;
	idx = 50;
	getifl(&idx,&iptk);
	if (!lv93 && best && iptk == 0 && discard && numsf == 1)
		isf0 = *isf;
	*reset_dp = (isf0 != *isf) || numsf > 1;
	for (i=0; i<NCL_psmult_nsf && !(*reset_dp); i++, p++)
	{
		filter = p->iuse && p->ierr <= 0;
		*reset_dp = filter && i != (*isf - 1) &&
			p->extflg != NCL_ON_FAR_EXTENSION;
	}
	if (!lfnd && isf0 == *isf) *reset_dp = UU_FALSE;

	if (*reset_dp && isf0 == *isf && lone && iptk > 0) *reset_dp = UU_FALSE;

	if (best) for (i=0; i<10; i++) s[i] = best->ps.s1[i];
/*
... this is the case when tool should go along extension
... of a surface which bounding box ends sooner then that of
... another surface which should not be used;
*/
   if (discard && *isf != isf0 && numsf==1)
   {
      status = UU_FAILURE;
      *isf = isf0;
   }
/*
.....Debug output of logic type
*/
#if DEBUGON == 2
	{
		sprintf(tbuf,"Selected surface = %d   Status = %d   Error = %d   Reset_dp = %d",*isf,
			status,ifl2,*reset_dp);
		NclxDbgPstr(tbuf);
	}
#endif
/*
... jingrong restore the err flag. 12/06/99.
*/
	*ierr = ifl2;
	return (status);
}

/*********************************************************************
**    FUNCTION     : int ncl_psmult_use (isf, nclkey, iuse, nb)
**       Determine if this surface should be used.
**    PARAMETERS
**       INPUT  :
**          isf    - Index of surface.
**       OUTPUT :
**          nclkey - Key of this surface.
**          iuse   - =1 iff this surface should be used.
**          nb     - Number of trim boundaries
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_use (isf, nclkey, iuse, nb)
UM_int2 *isf, *iuse, *nb;
UM_int4 *nclkey;
{
   NCL_psmult_rec *p;

   p = NCL_psmult_ptr + *isf - 1;
   *iuse = p->iuse;
   if (*iuse)
   {
      *nclkey = p->key;
      *nb = p->bound.nb;
      if (NCLX_internal_geom) UY_ps = UY_netps[*isf-1];
   }

   return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_psmult_insf (isf, u1, v1, insf)
**       Determine if a point is on the extension of a surface.
**    PARAMETERS
**       INPUT  :
**          isf    - Index of surface.
**          u1     - U value of point.
**          v1     - V value of point.
**       OUTPUT :
**          iext   - =1 iff point is on extension.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_insf (isf, u1, v1, iext)
UM_int2 *isf;
UM_real4 *u1, *v1;
UM_int2 *iext;
{
   int insf, status = UU_SUCCESS;
   NCL_psmult_rec *p1;
   UU_REAL *uvptr, uvpt1[3];

   uvpt1[0] = *u1;
   uvpt1[1] = *v1;
   uvpt1[2] = 0.0;
   p1 = NCL_psmult_ptr + *isf - 1;
   uvptr = (UU_REAL *) UU_LIST_ARRAY (p1->bound.uvpts);
   insf = um_cshape_inschk (uvptr, 1, p1->bound.np, uvpt1, 
					p1->bound.ummx, p1->bound.vmmx);
   *iext = 0;
   if (insf < 0) *iext = 1;

   return (status);
}

/*********************************************************************
**    FUNCTION     : int ncl_psmult_nsf (nsf)
**       Return the number of surfaces in a multiple surface part surface
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          nsf    - Number of surfaces in a multiple surface part surface
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_nsf (nsf)
UM_int2 *nsf;
{
	*nsf = NCL_psmult_nsf;

	return (UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION     : int ncl_cvonsf_store_mult (i,uvcv.key,*sfkey,
**                                                       d,xyzls,uvsls)
**      Stores one subcurve of a composite CFonSF curve as a subsurface
**      of a net (multiple) PS
** 
**    PARAMETERS
**       INPUT  :
**          i      -   number of the subcurve
**          key    -   subcurve key
**          sfkey  -   key of the base surface of the subcurve
**          d      -   subcurve length
**          xyzls  -   subcurve points
**          uvsls  -   subcurve u,v points
**       OUTPUT :
**          nsf    - Number of surfaces in a multiple surface part surface
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvonsf_store_mult (i,key,sfkey,d,xyzls,uvsls)
int i;
UU_KEY_ID key,sfkey;
UM_real8 d;
UU_LIST *xyzls, *uvsls;
{
	NCL_psmult_rec *p;

	if(NCL_psmult_ptr == UU_NULL) return (UU_FAILURE);
	p = NCL_psmult_ptr + i;
	
	p->linit = UU_TRUE;
	p->key   = key;
	p->bskey = sfkey;
	p->cvlen = d;
	uu_list_push_list(p->bound.cvpts,xyzls);
	uu_list_push_list(p->bound.uvpts,uvsls);

	return (UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION     : int ncl_load_curve (i)
**      Retrieves a subcurve of a composite CFonSF curve from the
**      global multi-PS record and writes it to the CVonSF record;
**      FORTRAN: Is called from PSREL
**    PARAMETERS
**       INPUT  :
**          i      -   number of the subcurve starting from 1
**          iuse   -   'surface near tool' flag
**          nb     -   # of trim boundaries
**          sfix   -   1,2,3 for PS,DS,CS
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_load_curve (i,iuse,sfix)
UM_int2 *i,*iuse,*sfix;
{
	int status = UU_FAILURE;
	NCL_psmult_rec *p;

   if (NCL_psmult_ptr == UU_NULL) return (UU_FAILURE);

   p = NCL_psmult_ptr + *i - 1;
	*iuse = 0;
   if(p->linit)
	{
		*iuse = p->iuse;
		if(*iuse != 0)
			status = ncl_load_cvonsf(*sfix,p->key,p->bskey,p->cvlen,
                                    p->bound.uvpts,p->bound.cvpts);
	}
	return (status);
}

/*********************************************************************
**    FUNCTION     : void ncl_tool_ps_rel (isf,trimd,tool,ta_mode,s,
**                                         tol,dtol,type,select,gougck)
**    assigns tool-PS relation type and parameter "select" to be
**    used to select current PS out of sub-surfaces of a net PS
**    PARAMETERS
**       INPUT  :
**              isf - # of sub-surface of a net PS
**              trimd - ifl(331) - if this surf. is driven as trimmed
**              tool - tool structure containing all info about tool
**              ta_mode - tool axis mode
**              s - array s(1-10,1) containing all info about current
**                      surface projection
**              tol,dtol - tolerances
**              gougck   - Non-zero = Do not perform sharp corner mods,
**                         because multi-ps gouge checking is in effect.
**       OUTPUT :
**              type - tool-PS relation type
**              select - selecting parameter for this surface
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_tool_ps_rel (noflip, isf,trimd,tool,ta_mode,s,tol,dtol,type,select,gougck)
UM_int2 *noflip, *isf,*ta_mode,*type,*trimd,*gougck;
NCL_tool *tool;
NCL_surf_struct *s;
UM_real8 *tol,*dtol,*select;
{
	struct NCL_fixed_databag srf;
	struct UM_evsrfout evsrf,evsrf1;
	UM_vector vec,vec1,lookvec;
	UM_coord psrf,proj,lookpt, point;
	int nb, insf, typ, status, invert, upward, isrf, comm_isf, sidew,
	    wide_tool, flat_tool,smthcor,instool,closext,i;
	NCL_psmult_rec *p;
	UM_netsf_common_bndry *bcom;
	UM_real8 dot1, dot2, ext_toler, rad, tolin, fct;
	UU_REAL closextdis,strpwd,mtol,u,v,dis;
	UU_REAL EPS = 1.e-3;
	NCL_surf_struct s1,s2;
	UM_int2 idx, mm;
	UM_transf tfmat;

	UM_coord te_sav;
	static int step_isf;
	static int step_isf0;
	static UM_coord step_te;
	static UM_vector step_fwd;

#if DEBUGON == 1
	{
		NclxDbgPstr(" ");
		sprintf(tbuf,"ncl_tool_ps_rel = %g,%g,%g",s->pt[0],s->pt[1],s->pt[2]);
		NclxDbgPstr(tbuf);
	}
#endif
	rad = NCL_tcyl_ps.radius;

	wide_tool = tool->cut_edge > *dtol; /* "wide" means "not a ball" */
	flat_tool = tool->corner_radius < *dtol;
	ext_toler = *dtol;
	if (flat_tool && wide_tool) ext_toler = *tol;
	strpwd = rad - *dtol;
	if (wide_tool) strpwd = MIN2(strpwd,tool->cut_edge - *tol);

	isrf = *isf - 1;

	p = NCL_psmult_ptr + isrf;
	nb = p->bound.nb;

	bcom = &NCL_netps_common_bndry;

	insf = 1;
	smthcor = 0;
	instool = 0;
	closext = 0;

	ncl_sftosf(s,&s1);
	um_vctovc (tool->look_pt,lookpt);
	um_vctovc (tool->look_vec,lookvec);

	if (*trimd && nb > 0) 
	{
/*
.....Make sure surface UV parameter matches point location
.....There are cases where a surface with a shallow angle
.....may project onto a tangent plane in the middle of the
.....surface, but the tool is actually off of the surface
.....Bobby - 04/22/10
*/
		srf.key = p->key;
		status = ncl_retrieve_data_fixed (&srf);
		status += uc_retrieve_transf(srf.key,tfmat);
		if (!lv97)
		{
			if (status == UU_SUCCESS)
			{
/*
.....Determine tolerance based on size of surface
.....Dassault test case flank_0409.cpp
.....Bobby - 04/14/15
*/
				mtol = *dtol;
				uc_init_evsrfout(&srf,&evsrf);
				uc_evsrf(UM_POINT,s->uv[0],s->uv[1],&srf,tfmat,&evsrf);

				u = s->uv[0]; v = s->uv[1] + .00001; if (v > 1.) v = v - .00002;
				uc_evsrf(UM_POINT,u,v,&srf,tfmat,&evsrf1);
				dis = um_dcccc(evsrf.sp,evsrf1.sp);
				if (dis > mtol) mtol = dis;
				
				u = s->uv[0] + .00001; v = s->uv[1]; if (u > 1.) u = u - .00002;
				uc_evsrf(UM_POINT,u,v,&srf,tfmat,&evsrf1);
				dis = um_dcccc(evsrf.sp,evsrf1.sp);
				if (dis > mtol) mtol = dis;
				
				ncl_wcstomcs(0,evsrf.sp,evsrf.sp);
				if (um_dcccc(evsrf.sp,s->pt) > abs(s->thick)+mtol)
					insf = -1;
			}
		}
/*
.....Convert to correct units
*/
		idx = 264;
		getifl(&idx,&mm);
		fct = 1.0;
		if (mm) fct = 1.0/25.4;
		tolin = *tol*fct;
		um_vctmsc(s->pt,fct,point);
		if (insf != -1)
			insf = um_inside_bndry (s->uv,point,&p->bound,&tolin);
		if (insf == 0)
		{
/*
..... here we increase the cylinder radius - so that the surface projection 
..... could be changed only if the current surface point is not within the 
..... cylinder up to the current machining tolerance - QAR 91171
*/
			NCL_tcyl_ps.radius += (*tol);
			if (!um_point_is_in_cylinder (s->pt,&NCL_tcyl_ps))
				insf = -1;
			NCL_tcyl_ps.radius = rad;
		}

/*		No flip test, Sasha, Sep.11, 2018
*/
		if (noflip) 
			p->ps.ad2 = 1.;

		if (insf <= 0) 
		{
			if (status == UU_SUCCESS && wide_tool)
			{
				if (insf == -1)
				{
					um_vctovc (s->pt,proj);
					status = ncl_proj_on_srf_bndry(proj,&srf,&p->bound,s,UU_FALSE);
					if (status != UU_SUCCESS)
					{
						ncl_sftosf(&s1,s);
						goto Start;
					}
					else if (p->ps.ad2 == -1.)
					{
						for (i=0;i<3;i++) s->normal[i] = -s->normal[i];
						s->distance = -s->distance;
					}
					/*
					Recover s->normal flip, Sasha, Sep.11, 2018
					*/
					if (noflip) 
						p->ps.ad2 = -1.;
					if (!lv93 && status == UU_SUCCESS &&
							*ta_mode == NCL_TA_TANTO_DS && flat_tool &&
							UM_SQDIS(s->pt,proj) < 1.25*tolin*tolin)
					{
						insf = 0;
						ncl_sftosf(&s1,s);
					}
					else
						ncl_sftosf(s,&s1);
				}
/*
..... this closext pre-testing is for Dassault MFGNC245 - see also multps.pp
..... in ~eduard/psmult
*/
				um_vcmnvc (tool->look_pt,s->pt,vec); 
				closextdis = um_dot (vec,s->normal);
				um_cross (tool->end_forward, tool->axis,vec);
				um_unitvc (vec,vec);
				um_vcmnvc (s->pt, tool->end, vec1);
				um_triple_cross (tool->axis,vec1,tool->axis,vec1);
				dot1 = um_dot (vec1, tool->end_forward);
				dot2 = fabs(um_dot (vec1,vec));
				if (dot1 > 0. && dot1 < rad && dot2 < strpwd)
				{ 	
					if (!flat_tool) closext = 1;
					if (closextdis < tool->corner_radius + s->thick + *dtol)
						goto Start;
				}
			}
		}
		if (insf < 0 && status == UU_SUCCESS)
		{
/*
..... ncl_tool_bndry_proj makes sense only if it is not a ball
..... DASSAULT MFGNC228
*/
			if (wide_tool)
			{
					status = ncl_tool_bndry_proj (tool,&p->bound,proj,vec1);
					if (status == UU_SUCCESS) status = 
						ncl_proj_on_srf_bndry (proj,&srf,&p->bound,s,UU_FALSE);
			}
			else
				status = 
					ncl_proj_on_srf_bndry(tool->look_pt,&srf,&p->bound,s,UU_FALSE);
/*
... jingrong 03/23/99 Added secondary check in case the tool is violating the
... surface, but the tool ring projection is outside of the tool cylinder.
...
..... do not project the tool end if a ball. DASSAULT MFGNC228
*/
			if (!um_point_is_in_cylinder(s->pt,&NCL_tcyl_ps) && wide_tool)
			{
				for (i=0;i<3;i++) proj[i]=tool->end[i];
				s2.thick = s->thick;
				status = ncl_proj_on_srf_bndry (proj,&srf,&p->bound,&s2,UU_FALSE);
/*
..... the change of radius below is for some stability - NCL117, motion 10
*/
				NCL_tcyl_ps.radius -= 0.1*(*tol);
				if (um_point_is_in_cylinder(s2.pt,&NCL_tcyl_ps))
					ncl_sftosf(&s2,s);
				NCL_tcyl_ps.radius = rad;
			}
			if (status == UU_SUCCESS && p->ps.ad2 == -1.)
			{
				for (i=0;i<3;i++) s->normal[i] = -s->normal[i];
				s->distance = -s->distance;
			}
		}
	}
/*
..... special logic for going up a sharp vertical step
*/
Start:;
	if (stepup)
	{
		um_vcmnvc (tool->end,step_te,vec);
		dot1 = um_dot (vec,step_fwd);
		if (dot1 > 1.5*tool->diameter || dot1 < -tool->diameter/2.)
			stepup = 0;
		else if (dot1 > EPS)
		{
			if (*isf == step_isf)
			{
/*
..... go up to the top horizontal surface, and then drive it (i.e.
..... make sure it is selected, and its normal is unaffected by the
..... corner logic)
*/
				um_vcmnvc(tool->look_pt,s->pt,vec);
				dot1 = um_dot (tool->axis,s->normal);
				if (dot1 < 0.)
				{
					um_vctmsc (s->normal,(UU_REAL) -1.,s->normal);
					s->distance = -s->distance;
				}
				*select = um_dot (vec,s->normal);
				typ = NCL_ON_SURFACE;
				goto Apres;
			}
			else if (*isf == step_isf0)
			{
/*
..... do not consider the short vertical surface
*/
				typ = NCL_DISCARD;
				goto Apres;
			}
		}
	}

/*
... if surf is perpto taxis, make sure we have the closest (not farthest) 
... point to the surf on the tool ring
*/
	dot1 = um_dot (tool->axis,s->normal);
	if ( fabs(dot1) < EPS)
	{
		um_proj_pt_on_circle (s->pt,&tool->ring,tool->look_pt);	
		um_vcmnvc (tool->look_pt, tool->ring.center,vec);
		um_unitvc (vec,tool->look_vec);
	}
/* 
... project tool->look_pt on surface tangent plane
*/
	um_vcmnvc (tool->look_pt,s->pt,vec);
	um_translate_point(tool->look_pt,-um_dot(vec,s->normal),s->normal,psrf);

	dot2 = um_dot (vec,s->normal);	
	invert = dot1 < -EPS || (fabs(dot1) < EPS && dot2 < 0. );

	if (invert)
	{
		um_vctmsc (s->normal,(UU_REAL) -1.,s->normal);
		s->distance = -s->distance;
	}
/*
... decide if projects on surface or extension
*/
	if (um_dcccc (psrf,s->pt) <= ext_toler)
		typ = NCL_ON_SURFACE;	
	else
		typ = NCL_ON_FAR_EXTENSION;
/*
.....Debug output of logic type
*/
#if DEBUGON == 1
	{
		sprintf(tbuf,"ncl_tool_ps_rel isf = %d  tol = %g   dtol = %g",*isf,
			*tol,*dtol);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"Projection type = %s",dbgptyp[typ]);
		NclxDbgPstr(tbuf);
	}
#endif
/*
... if point is on extension:
... project sf point on common boundary of the net PS;
... if it's close to it, go into "corner logic";
... if not, use extension of the sub-surface.
*/
	if (typ == NCL_ON_FAR_EXTENSION || flat_tool) 
	{
		status = 
			ncl_common_bndry_proj (UU_FALSE,isrf,bcom,s->pt,proj,vec,&comm_isf);
		if (status == UU_SUCCESS && um_dcccc (s->pt,proj) <= ext_toler) 
			 typ = NCL_ON_CORNER;
	}
/*
... corner case
*/
	if (typ == NCL_ON_CORNER)
	{
/*
.....Debug output
*/
#if DEBUGON == 1
	{
		sprintf(tbuf,"Projection type 2 = %s",dbgptyp[typ]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"Tool axis = %s",dbgpta[*ta_mode]);
		NclxDbgPstr(tbuf);
	}
#endif
		ncl_sftosf(s,&s2);
		if ( *ta_mode != NCL_TA_NORMAL_PS 
		      && *ta_mode != NCL_TA_TANTO_DS
		      && wide_tool )
		{
			UU_REAL diam = tool->diameter;
			
			tool->diameter += 2.*(s->thick);
			um_vctovc (tool->end, te_sav);
			status = ncl_tool_common_bndry_proj (UU_FALSE,isrf,NCL_psmult_ptr,
								bcom,tool,psrf,&comm_isf);
			tool->diameter = diam;
		}
		else
			um_vctovc (s->pt,psrf);
/*
.....Debug output
*/
#if DEBUGON == 1
	{
		sprintf(tbuf,"Tool look pt = %g,%g,%g",tool->look_pt[0],tool->look_pt[1],
			tool->look_pt[2]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"Tool Axis = %g,%g,%g",tool->axis[0],tool->axis[1],
			tool->axis[2]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"psrf = %g,%g,%g",psrf[0],psrf[1],psrf[2]);
		NclxDbgPstr(tbuf);
	}
#endif
		if (status == NCL_GOUGE) 
		{
			um_vctovc (tool->end,step_te);
			um_vctovc (te_sav,tool->end);
			dot1 = um_dot (tool->end_forward, s->normal);
			if (1. - fabs (dot1) < UM_FUZZ)
			{
/*
..... this triggers a special 'stepup' logic: the tool is hitting a 
..... vertical surface head-on, and gouging its common boundary
*/ 
				stepup = 1;
				step_isf0 = *isf;
				step_isf = comm_isf;
				um_vctovc (tool->end_forward,step_fwd);
				typ = NCL_DISCARD;
				goto Apres;
			}
		}
		if (status != UU_SUCCESS)
			typ = NCL_DISCARD;
		else
		{
			int isok = 1;
			UM_vector angnorm;
/*
..... only consider the relevant common boundary. DASSAULT MFGNC228
*/
			if (comm_isf > 0)
			{
				smthcor = ncl_smooth_corner(isrf,comm_isf,psrf,*tol,angnorm);
/*
..... the iclose flag is to use in ncl_psmult_select - added for Dassault NCL84
*/
				if (smthcor == 1) p->iclose = -comm_isf;
			}
			else
				smthcor = 1;
/*
.....Debug output
*/
#if DEBUGON == 1
	{
		sprintf(tbuf,"Smooth Corner = %d",smthcor);
		NclxDbgPstr(tbuf);
	}
#endif
			if (smthcor != 1 && *gougck == 0)
			{
				um_vcmnvc (tool->look_pt,psrf,vec);
/*
...jingrong 02/26/99 Increase tolerance to avoid tool to go a step backward
... on the corner due to wrong value of part surface normal
         if (um_mag (vec) >= *dtol)
*/
				if (um_mag (vec) >= 2.5*(*dtol) && *ta_mode != NCL_TA_NORMAL_PS 
		      				&& *ta_mode != NCL_TA_TANTO_DS)
				{
/*
... jingrong 05/17/99 if the tool has already broken the surface, 
... set the PS normal to the opposite direction, so that *select 
... will be negative.
*/
					um_unitvc (vec,s->normal);
					if (smthcor == 0)
					{
/*
..... this change is to prevent the corner logic building a plane almost
..... perpendicular from the two making the corner. it could happen if
..... the tool is on the extension of both surfaces, e.g. CATIA NCL136
*/
						dot2 = um_dot(s->normal,angnorm);
						if (fabs(dot2) > 0.95)
							isok = 0;
					}
					if (um_dot(vec,tool->axis)<(-*dtol)
                 			&& um_point_is_in_cylinder(s->pt,&NCL_tcyl_ps))
						instool = 1;
					if (instool && isok == 1) 
					{
						if ((um_dot(s1.normal,vec) < 0. && 
								um_dot (tool->end_forward,vec) < 0.))
							isok = 0;
						else
							um_vctmsc(s->normal,-1.,s->normal);
					}
					if (isok == 0)
						um_vctovc (s1.normal,s->normal);
				}
				else 
					um_vctovc (tool->axis,s->normal);
			}
			if (isok)
			{
				s->distance = um_dot (s->normal,psrf);
				um_vctovc (psrf,s->pt);
			}
		}
	}
/*
.....Debug output
*/
#if DEBUGON == 1
	{
		sprintf(tbuf,"Surface pt = %g,%g,%g",s->pt[0],s->pt[1],s->pt[2]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"Surface Normal = %g,%g,%g",s->normal[0],s->normal[1],
			s->normal[2]);
		NclxDbgPstr(tbuf);
	}
#endif

/*
... Calculate distance between tool look pt and  sf
... taking into account relative sign of the PS normal and vector
... from tool look pt to srf pt; if they have same sign,
... tool jumped thru PS to its inner side -> dist < 0 -> this PS
... will have more priority when selecting the final PS.
... In the case of corner, vec || PS normal (unless smthcor, or gougck,
... or tool ring intersecting common boundary ...).
*/
	um_vcmnvc (psrf,tool->look_pt,vec); 
	*select = - um_dot (vec,s->normal);
	if (smthcor == 1)
	{
		*select = um_mag(vec);
		if (um_dot(vec,tool->axis) > *dtol
			&& um_point_is_in_cylinder(s->pt,&NCL_tcyl_ps))
			instool = 1;
		if (instool) *select = -*select;
	}
/*
... this is to enable the flat tool go up a steep (but not strictly 
... vertical) step
*/
/*
.....Debug output
*/
#if DEBUGON == 1
	{
		sprintf(tbuf,"Select = %g",*select);
		NclxDbgPstr(tbuf);
	}
#endif
	if (flat_tool && *select < 0. && instool == 0)
	{
		status = 
			ncl_common_bndry_proj (UU_FALSE,isrf,bcom,s->pt,proj,vec,&comm_isf);
		if (status == UU_SUCCESS && um_dcccc (s->pt,proj) <= ext_toler) 
			*select = fabs(*select);
	}

	upward = 0;
	dot1 = um_dot(tool->end_forward, tool->axis);
	if (dot1 > 0.5)
		upward = 1;
	else if (dot1 < -0.5)
		upward = -1;
/*
.....Determine if contact is on side of cutter
*/
	sidew = 0;
	if (!lv97)
	{
		dot1 = um_dot(tool->axis,s->normal);
		if (fabs(dot1) < 0.5)
			sidew = 1;
	}

	if ( typ == NCL_ON_SURFACE 
			&& fabs (um_dot (tool->axis, s->normal)) < UM_FUZZ
			&& upward == 1)
	{
 		*select = - fabs (*select);
	}
/*
...jingrong 02/26/99 Increase cylinder radius to allow tool to go up a steep 
...step even if there is a gap between two surfaces.
.....If the radius is increased for side surfaces
.....it causes problems with Dassault test cases
.....NCL339.cpp & NCL339a.cpp
.....Bobby - 04/15/11
*/
	if (upward == 1 /*|| sidew == 1*/) NCL_tcyl_ps.radius += 2.5*(*dtol);
/*
... PS thick enlarges cyl
*/
	if (s->thick > 0.)
	{
		dot1 = MIN2 (1., fabs(um_dot (tool->axis, s->normal)));
		NCL_tcyl_ps.radius += s->thick*sqrt (1.-dot1*dot1);
	}

	if ( typ == NCL_ON_SURFACE)
	{
		if (upward != 1 && fabs(um_dot(tool->look_vec,tool->forward)) > 
				NCL_CLOSE_EXT_TILT)
			 NCL_tcyl_ps.radius += *dtol;
		if (!um_point_is_in_cylinder (psrf,&NCL_tcyl_ps)) 
			typ = NCL_DISCARD_SURFACE;
	}
	else if (typ == NCL_ON_CORNER)
	{
		if (!um_point_is_in_cylinder (psrf,&NCL_tcyl_ps)) 
			typ = NCL_DISCARD_CORNER;
		if (closext == 1 && (typ == NCL_DISCARD_CORNER || closextdis < *select))
		{
/*
..... this block is for Dassault MFGNC245
*/
			int jsf = comm_isf;

			um_translate_point (tool->ring.center,tool->cut_edge,tool->forward,
				proj);
			ncl_proj_on_srf_bndry (proj,&srf,&p->bound,&s2,UU_FALSE);
			ncl_common_bndry_proj (UU_FALSE,isrf,bcom,s2.pt,proj,vec,&jsf);
			if (um_dcccc (s2.pt,proj) > *tol)
			{
				um_vcmnvc (s2.pt,s->pt,vec);
				dot1 = um_dot (vec, tool->forward);
				if (dot1 > *tol)
				{
					ncl_sftosf(&s1,s);
					um_vctovc (lookpt,tool->look_pt);
					um_vctovc (lookvec,tool->look_vec);
					typ = NCL_ON_CLOSE_EXTENSION;
					*select = closextdis;
				}
			}
		}
		if (!lv93 && typ == NCL_DISCARD_CORNER)
		{
			NCL_tcyl_ps.radius = 1.5*rad;
			um_vcmnvc (s->pt, tool->end, vec1);
			dot1 = um_dot (vec1, tool->axis);
			if (dot1 > rad && !um_point_is_in_cylinder (psrf,&NCL_tcyl_ps))
				ncl_sftosf(&s2,s);
		}
	}

	if (typ == NCL_ON_FAR_EXTENSION)
	{
		dot1 = um_dot(tool->axis,s->normal);
/*
... ps is on the side of the tool..jingrong 9/28/99.
*/
		if (sidew == 1 /* && um_point_is_in_cylinder(s->pt,&NCL_tcyl_ps)*/)
		{
/*
..... only if the tool is not a ball (otherwise tool->look_vec is zero).
..... DASSAULT MFGNC228
*/
			dot2 = um_dot(s->pt,tool->axis) - um_dot(tool->end,tool->axis);
			if (dot2 > 0.)
			{
				if (lv93)
				{
					if (wide_tool)
						dot1 = um_dot (tool->look_vec,tool->forward);
					if (fabs(dot1) > 0.5 && dot2 > 0. && upward)
						typ = NCL_ON_CLOSE_EXTENSION;
				}
				else
				{
					if (wide_tool)
					{
						dot1 = um_dot (tool->look_vec,tool->forward);
 						if (fabs(dot1) > 0.5 && upward == 1)
							typ = NCL_ON_CLOSE_EXTENSION;
					}
					else
					{
						um_vcmnvc (s->pt, tool->look_pt, vec1);
/*
... proj vec1 on the tool bottom plane
*/		
						um_triple_cross (tool->axis,vec1,tool->axis,vec1);
						um_unitvc (vec1,vec1);
						dot1 = um_dot (vec1, tool->forward);
						if ((dot1 > 0.9 && upward == 1) ||
							(dot1 < -0.9 && upward == -1))
							typ = NCL_ON_CLOSE_EXTENSION;
					}
				}
			}
			*select = um_dcccc (tool->look_pt, s->pt);
		}
		else
		{
/*
... ps is underneath the tool
*/
			if (*trimd == 0 && (s->uv[0] < UM_FUZZ || s->uv[1] < UM_FUZZ || 
					s->uv[0] > 1.-UM_FUZZ || s->uv[1] > 1.-UM_FUZZ))
				insf = 0;
			if (insf <= 0)
			{
/*
... vec is the right vector
*/
				um_cross (tool->end_forward, tool->axis,vec);
				um_unitvc (vec,vec);
				um_vcmnvc (s->pt, tool->end, vec1);
/*
... proj vec1 on the tool bottom plane
*/
				um_triple_cross (tool->axis,vec1,tool->axis,vec1);
				dot1 = um_dot (vec1, tool->end_forward);
/*
... if spt is within the fwd strip defined by the tool.
*/
				if (um_mag(vec1) < rad || (dot1 > 0. && dot1 < rad))
				{
					dot2 = fabs(um_dot (vec1,vec));
					if (dot2 < strpwd) typ = NCL_ON_CLOSE_EXTENSION;
				}
			}
			if (typ == NCL_ON_FAR_EXTENSION &&
					 um_point_is_in_cylinder(s->pt,&NCL_tcyl_ps))
			{
				dot2 = um_dot (tool->look_vec,tool->forward);
				if (fabs(dot2) > NCL_CLOSE_EXT_TILT) 
					typ = NCL_ON_CLOSE_EXTENSION;
				else
				{
					if (upward == 1)
					{
/*
..... the cylinder was much increased, so we consider the original size
..... for this test (CATIA NCL202 is the immediate reason).
*/
						NCL_tcyl_ps.radius -= 2.5*(*dtol);
						if (um_point_is_in_cylinder(s->pt,&NCL_tcyl_ps))
							typ = NCL_ON_SIDE_EXTENSION;
						NCL_tcyl_ps.radius += 2.5*(*dtol);
					}
					else
						typ = NCL_ON_SIDE_EXTENSION;
				}
			}
			if (typ == NCL_ON_FAR_EXTENSION) 
			{
				if (*trimd) *select = um_dcccc (tool->look_pt, s->pt);
				else *select = um_dcccc (tool->end, s->pt);
			}
		}
	}
/*
... this is to discard surfaces perpto taxis and on a side of the tool
... with respect to the forward direction
*/
	dot1 = um_dot (s->normal,tool->axis);
	if (typ == NCL_ON_SURFACE && fabs(dot1) < EPS && !flat_tool)
	{
		um_cross (tool->axis,tool->end_forward,vec);
		if (fabs(um_dot (s->normal,vec)) > 1. - EPS)
		{
			typ = NCL_DISCARD;
			*select = fabs(*select);
		}
	}

/*
... to prevent a point tool to get under a surface
*/
	if (typ == NCL_DISCARD_SURFACE && 
		*select < 0 &&  flat_tool && !wide_tool) typ = NCL_ON_SURFACE;
					
Apres:;
	NCL_tcyl_ps.radius = rad;
	*type = typ;
#if DEBUGON == 1
	{
		sprintf(tbuf,"Projection type %d = %s",*isf,dbgptyp[typ]);
		NclxDbgPstr(tbuf);
	}
#endif
#if DEBUGON == 1
	{
		sprintf(tbuf,"exit = %g,%g,%g",tool->axis[0],tool->axis[1],
			tool->axis[2]);
		NclxDbgPstr(tbuf);
	}
#endif
}

/*********************************************************************
**    FUNCTION : void ncl_create_tcyl (tool,rad)
**    Creates tool cylinder structure
**    PARAMETERS
**       INPUT :
**               tool - tool structure
**               rad  - cylinder radius
**       OUTPUT :
**               NCL_tcyl_use/ps structures is created
**    RETURNS :
**             none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_create_tcyl (tool,rad)
NCL_tool *tool;
UM_real8 *rad;
{
	UM_real8 tol;
/*
... tool cylinder to intersect with PS to set the "iuse" flag
... for each surface
*/
	um_vctovc (tool->axis,NCL_tcyl_use.axis.n);
	um_vctovc (tool->end,NCL_tcyl_use.axis.p0);
	NCL_tcyl_use.radius = *rad;
/*
... tool cylinder to be used to set the tool-PS relation
*/
	um_vctovc (tool->axis,NCL_tcyl_ps.axis.n);
	um_vctovc (tool->end,NCL_tcyl_ps.axis.p0);
	NCL_tcyl_ps.radius = tool->corner_radius + tool->cut_edge;

	getsct (&tol);

	if (tool->corner_radius > 0.)
		NCL_tcyl_ps.radius = MAX2 (NCL_tcyl_ps.radius,tol);
	else
		NCL_tcyl_ps.radius += tol;
}

/*********************************************************************
**    FUNCTION : int ncl_smooth_corner(isf,isf1,proj,tol,norm1)
**		determine whether a net surface has a corner near a given point 
**    PARAMETERS
**       INPUT :
**               isf   - number of sub-surface of a net surface
**               isf1  - number of the adjacent sub-surface
**               proj  - given point on the common boundary
**               tol   - tolerance 
**       OUTPUT :
**               angnorm - vector perpendicular to both corner planes,
**                         if there is a corner
**    RETURNS :
**             1  - if there is no corner near proj
**             0  - if there is a corner
**            -1  - UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_smooth_corner(isf,isf1,proj,tol,angnorm)
int isf,isf1;
UM_coord proj;
UM_vector angnorm;
UM_real8 tol;
{
	UU_REAL cos, dis;
	int nb, status,lsmooth;
	NCL_psmult_rec *p;
	struct NCL_fixed_databag srf;
	struct NCL_fixed_databag srf1;
	NCL_surf_struct s, s1;
	UM_netsf_common_bndry *bcom;

	lsmooth = -1;
	um_nullvc (angnorm);

	bcom = &NCL_netps_common_bndry;
	nb = bcom->num;
	if (nb < 1) return (UU_FAILURE);

	p = NCL_psmult_ptr + isf;
	srf.key = p->key;
	status = ncl_retrieve_data_fixed (&srf);
	if (status == UU_SUCCESS)
		status = ncl_proj_on_srf_bndry (proj,&srf,&p->bound,&s,UU_FALSE);
	if (status != UU_SUCCESS) return (UU_FAILURE);
	dis = um_dcccc (proj,s.pt);
	if (dis > tol) return (UU_FAILURE);

	p = NCL_psmult_ptr + isf1-1;
	srf1.key = p->key;
	if (NCLX_internal_geom) UY_ps = UY_netps[isf1-1];
	status = ncl_retrieve_data_fixed (&srf1);
	if (status == UU_SUCCESS)
	status = ncl_proj_on_srf_bndry (proj,&srf1,&p->bound,&s1,UU_FALSE);
	if (status != UU_SUCCESS) 
	{
		lsmooth = -1;
		goto done;
	}
	dis = um_dcccc (proj,s1.pt);
	if (dis < NCL_tcyl_ps.radius || 
	um_point_is_in_cylinder(s1.pt,&NCL_tcyl_ps))
	{
		cos = fabs(um_dot(s.normal,s1.normal));
		if (fabs(1.0 - cos) > 0.01) 
		{
			lsmooth = 0;
			um_cross (s.normal,s1.normal,angnorm);
			um_unitvc (angnorm,angnorm);
			goto done;
		}
		else 
			lsmooth = 1;
	}
/*
...Restore UY_ps.
*/
done:
	if (NCLX_internal_geom) UY_ps = UY_netps[isf];

	return (lsmooth);
}

/*********************************************************************
**    FUNCTION : int ncl_psmult_setad2(isf,dtol)
**		Set the 'normal reversed' flags on all surfaces connected to
**    the given one, so that they all define same side of the net
**    surface. This is done because projecting from afar could lead to
**    the wrong value of the flag, which could mean using a wrong 
**    look-point.
**    PARAMETERS
**       INPUT :
**               isf   - number of sub-surface of a net surface
**               dtol  - tolerance 
**       OUTPUT :
**					  ad2 flags set on the surfaces (smoothly) connected to
**               the isf-surface
**    RETURNS :
**             UU_FAILURE iff something is wrong
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_setad2 (isf,dtol)
UM_int2 *isf;
UM_real8 *dtol;
{
	int i,j,isrf,is,is1,ninit,found,nb,status;
	int numsf = NCL_psmult_nsf;
	int *inited = UU_NULL,*surf,*cb = UU_NULL,*np;
	NCL_psmult_rec *p,*p1;
	struct NCL_fixed_databag *srf,sf;
	NCL_surf_struct s,s1;
	UM_netsf_common_bndry *bcom;
	UU_REAL co,dis,*cvlen;
	UM_coord *pts = UU_NULL,*points;


	bcom = &NCL_netps_common_bndry;
	nb = bcom->num;
	if (nb < 2) return (UU_FAILURE);

	isrf = *isf - 1;
	p = NCL_psmult_ptr + isrf;
	if (fabs(p->ps.ad2) != 1.) return (UU_FAILURE);
	if (NCLX_internal_geom)
	{
		UY_ps = UY_netps[isrf];
		srf = (struct NCL_fixed_databag *)UY_ps;
	}
	else
	{
		srf = &sf;
		srf->key = p->key;
		status = ncl_retrieve_data_fixed (srf);
		if (status != UU_SUCCESS) return (UU_FAILURE);
	}

	inited = (int *) uu_malloc (numsf * sizeof(int));
	inited[isrf] = 1;
	ninit = 1;
	for (is = 0; is < numsf; is++)
		if (is != isrf) inited[is] = 0;

	surf = (int *) UU_LIST_ARRAY (bcom->surfaces);
	np = (int *) UU_LIST_ARRAY (bcom->np);
	cvlen = (UU_REAL *) UU_LIST_ARRAY (bcom->lengths);
	points = (UM_coord *) UU_LIST_ARRAY (bcom->pts);
/*
..... for every common boundary we look at the middle point
*/
	cb = (int *) uu_malloc (nb * sizeof(int));
	pts = (UM_coord *) uu_malloc (nb*sizeof(UM_coord));
	found = 0;
	for (i = 0; i < nb; i++,surf+=2)
	{
		is = surf[0]; is1 = surf[1];
		if (inited[surf[0]] + inited[surf[1]] > 0) found = 1;
		if (np[i] < 2 || cvlen[i] < *dtol)
			cb[i] = 0;
		else
		{	
			cb[i] = 1;
			um_vctovc (points[np[i]/2], pts[i]);
		}
		points += np[i];
	}

	while (ninit < NCL_psmult_nsf && found)
	{
		found = 0;
		surf = (int *) UU_LIST_ARRAY (bcom->surfaces);
		for (i = 0; i < nb; i++, surf+=2)
		{
			is = surf[0]; is1 = surf[1];
			if (cb[i] == 0 || inited[is] == inited[is1]) continue;
			if (inited[is1] > inited[is])
			{
				j = is; is = is1; is1 = j;
			}
			p = NCL_psmult_ptr + is;
			if (NCLX_internal_geom)
			{
				UY_ps = UY_netps[is];
				srf = (struct NCL_fixed_databag *)UY_ps;
			}
			else
			{
				srf->key = p->key;
				status = ncl_retrieve_data_fixed (srf);
				if (status != UU_SUCCESS) goto Done; 
			}
			status = ncl_proj_on_srf_bndry (pts[i],srf,&p->bound,&s,UU_TRUE);
			if (status != UU_SUCCESS) goto Done;
			dis = um_dcccc (pts[i],s.pt);
			if (dis > *dtol) continue;

			p1 = NCL_psmult_ptr + is1;
			if (NCLX_internal_geom)
			{
				UY_ps = UY_netps[is1];
				srf = (struct NCL_fixed_databag *)UY_ps;
			}
			else
			{
				srf->key = p1->key;
				status = ncl_retrieve_data_fixed (srf);
				if (status != UU_SUCCESS) goto Done;
			}
			status = ncl_proj_on_srf_bndry (pts[i],srf,&p1->bound,&s1,UU_TRUE);
			if (status != UU_SUCCESS) goto Done;
			dis = um_dcccc (pts[i],s1.pt);
			if (dis > *dtol) continue;
			co = um_dot(s.normal,s1.normal);
			if (fabs (co) < 0.9) continue;
			found = 1;
			inited[is1] = 1;
			ninit++;
			if (co > 0.)
				p1->ps.ad2 = p->ps.ad2;
			else
				p1->ps.ad2 = - p->ps.ad2;
		}
	}
Done:;
/*
...Restore UY_ps.
*/
	if (NCLX_internal_geom) UY_ps = UY_netps[isrf];
	if (inited) uu_free (inited);
	if (cb) uu_free (cb);
	if (pts) uu_free (pts);

	return (0);
}

/*********************************************************************
**    FUNCTION : int ncl_change_tcyl_ps (tcyl_pt,tcyl_normal,tcyl_rad)
** change the global NCL_tcyl_ps structure
**    PARAMETERS
**       INPUT :
**               tcyl_pt
**               tcyl_normal
**               tcyl_rad
**       OUTPUT :
**               NCL_tcyl_use/ps structures is changed
**    RETURNS :
**             none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_change_tcyl_ps (tcyl_pt,tcyl_normal,tcyl_rad)
UM_coord tcyl_pt;
UM_vector tcyl_normal;
UU_REAL tcyl_rad;
{
	um_vctovc (tcyl_normal,NCL_tcyl_ps.axis.n);
	um_vctovc (tcyl_pt,NCL_tcyl_ps.axis.p0);
	NCL_tcyl_ps.radius = tcyl_rad;

	return(0);
}
