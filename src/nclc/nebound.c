/*********************************************************************
**    NAME         :  nebound.c
**       CONTAINS:  Routines to work with surface boundaries
**
**           ncl_get_boundary_toler
**           ncl_set_boundary_toler
**
**           ncl_get_boundary
**           ncl_get_common_bndry
**
**           ncl_proj_on_srf_bndry
**           ncl_common_bndry_proj
**           ncl_tool_common_bndry_proj
**           ncl_tool_bndry_proj
**
**    COPYRIGHT 1998 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nebound.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:24
*********************************************************************/

#include "nccs.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mcrv.h"
#include "modef.h"
#include "mdcpln.h"
#include "zsysdep.h"
#include "uminmax.h"
#include "mfort.h"
#include "mgeom.h"
#include "nclfc.h"
#include "ycom.h"
#include "nclpsmult.h"
#include "ncl.h"

extern int NCLX_internal_geom;
UU_LOGICAL NCL_lv93;
/*
... current tolerance used for surface boundary evaluation
*/
static UU_REAL NCL_BOUNDARY_TOLERANCE=0.;

char *uu_malloc();

/*********************************************************************
**    FUNCTION     : UU_REAL ncl_get_boundary_toler ()
**    Returns current tolerance used for srf boundary evaluation
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         current tolerance used for srf boundary evaluation
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL ncl_get_boundary_toler ()
{
	UM_int2 idx;
	if (NCL_BOUNDARY_TOLERANCE == 0.)
	{
		idx = 175; getsc(&idx,&NCL_BOUNDARY_TOLERANCE);
	}
   return (NCL_BOUNDARY_TOLERANCE);
}
/*********************************************************************
**    FUNCTION     : int ncl_set_boundary_toler (tol)
**    Sets current tolerance used for srf boundary evaluation
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**          UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_set_boundary_toler (tol)
UU_REAL tol;
{
   NCL_BOUNDARY_TOLERANCE = MAX2 (tol, UM_BOUNDARY_TOLER);
   return (UU_SUCCESS);
}
/*********************************************************************
**    FUNCTION     : int ncl_set_boundary (type,sf, p)
**      Create a set of uv points on the boundary of trimmed sf
**      and save them in a list.
**    PARAMETERS
**       INPUT  :
**          type    - type of boundary info needed
**          sf      - surface
**       OUTPUT :
**          p       - Pointer to boundary structure
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_set_boundary (sf,tfmat,p,cvlst)
struct NCL_fixed_databag *sf;
UM_transf tfmat;
UM_srf_boundary *p;
struct NCL_uvconv *cvlst;
{
	int status;

	status = um_pre_srf_bndr (sf,tfmat,p,cvlst);

	if (p->uvpts->cur_cnt <= 2) status = UU_FAILURE;

	if (status == UU_SUCCESS)
	{
		if (p->ummx[0][0] < 0.0) p->ummx[0][0] = 0.0;
		if (p->vmmx[0][0] < 0.0) p->vmmx[0][0] = 0.0;
		if (p->ummx[0][1] > 1.0) p->ummx[0][1] = 1.0;
		if (p->vmmx[0][1] > 1.0) p->vmmx[0][1] = 1.0;
	}
	else
	{
		um_free_boundary(p);
	}
	return(status);
}

/*********************************************************************
**    FUNCTION     : int ncl_get_boundary (type,sf, p)
**      Create a set of uv points on the boundary of trimmed sf
**      and save them in a list.
**    PARAMETERS
**       INPUT  :
**          type    - type of boundary info needed
**          sf      - surface
**       OUTPUT :
**          p       - Pointer to boundary structure
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_boundary (type,sf,p)
NCL_surflist_type type;
struct NCL_fixed_databag *sf;
UM_srf_boundary *p;
{
	int status;
	UM_int2 idx;
	UM_real8 ver, tol;
	UM_transf tfmat,*t;
	struct NCL_uvconv cvlst;
/*
...check if the boundary data for this surface are already 
...in the Unibase record;
...if yes, copy them to the boundary structure and exit;
...if not, calculate them and write to the Unibase.
*/
	status = ncl_get_surflist (type,sf,p);
	if (status == UU_SUCCESS) return (UU_SUCCESS);

	t = &tfmat;
	status = ncl_trimsrf_get_tf (sf, (UU_REAL **)&t);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	if (type == UV_BOX_LIST)
	{
		idx = 169;
		getsc(&idx, &ver);
		if (ver < 9.21499 || NCLX_internal_geom)
			p->toler = .01;
		else
		{
			idx = 175;
			getsc(&idx, &tol);
			ncl_set_boundary_toler (tol);
			p->toler = ncl_get_boundary_toler();
		}
		if (ver < 9.2999)
			status = um_pre_srf_bndr_box (sf,p);
		else
		{
			p->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
			p->uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
			uu_list_init (p->cvpts, sizeof(UM_coord), 100, 100);
			uu_list_init (p->uvpts, sizeof(UM_coord), 100, 100);

			status = um_uvlist_init (sf,&cvlst);

			if (status == UU_SUCCESS)
			status = ncl_set_boundary(sf,tfmat,p,&cvlst);

			um_uvlist_free (&cvlst);
		}
	}
	else if (type == WHOLE_BOUNDARY_LIST)
	{

		status = um_uvlist_init (sf, &cvlst);
		p->toler = ncl_get_boundary_toler();

		if (status == UU_SUCCESS && p->toler >= UM_BOUNDARY_TOLER)
			status = ncl_set_boundary (sf,*t,p,&cvlst);
		else
			status = UU_FAILURE;

		um_uvlist_free (&cvlst);
	}
	else
		status = UU_FAILURE;
/*
..... As of 9.2 surfaces of revolution do not have boundary list - QAR 92261
	if (status == UU_SUCCESS && sf->rel_num != NCL_REVSURF_REL)
*/
	if (status == UU_SUCCESS)
		status = ncl_store_surflist (type,sf,p);

	return (status);
}

/*********************************************************************
**    FUNCTION : int ncl_tool_common_bndry_proj (ext_flag,isf,netsrf,
**                                            bndry,tool, proj,isf1)
**
**    Projects inner ring of the tool on the common boundary of a net PS
**    related to sub-surface # isf;
**    If netsrf == UU_NULL, considers all common boundaries made by isf;
**    Otherwise, netsrf must be the pointer to the netPS record;
**    In this case, neglects boundaries with surfaces which are not "used"
**
**    PARAMETERS
**       INPUT  :
**              ext_flag - if false, extensions are not considered;
**                         if true, extensions are considered if
**                         they are not tilted too much with respect 
**                         to the tool axis.
**              isf   - number of sub-surface of a net surface
**              netsrf - pointer to the net surface structure
**                       or UU_NULL
**              bndry - common bndry of the net surf.
**              tool  - tool structure
**       OUTPUT :
**              proj  - point on the common bndry closest to the ring
**              isf1  - number of the corresponding adjacent subsurface 
**              tool  - tool look_pt and look_vec changed, also tool end
**                      if NCL_GOUGE
**              
**    RETURNS      :
**          UU_SUCCESS normally;
**          UU_FAILURE if sub-surf. isf does't have common boundaries
**                                 with other surfaces.
**          NCL_GOUGE (=1) if the tool side gouges the common boundary
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tool_common_bndry_proj (ext_flag,isf,netsrf,bndry,tool,proj,isf1)
UU_LOGICAL ext_flag;
int isf, *isf1;
char *netsrf;
UM_netsf_common_bndry *bndry;
NCL_tool *tool;
UM_coord proj;
{
	UM_vector v,vec;
	UM_coord *p,*pp0,*pt,pb,pc,pc1,pc2,tlpt;
	UU_REAL d,dmin, um_dcccc();
	int status,res,i,nb,*np,*surf, npp,filter,gouge;
	UU_LOGICAL with_ext;
	UM_int2 type;
	UM_coord te,te_sav;

	*isf1 = -1;
	nb = bndry->num;
	np = (int *) UU_LIST_ARRAY (bndry->np);
	pt = (UM_coord *) UU_LIST_ARRAY (bndry->pts);
	surf = (int *) UU_LIST_ARRAY (bndry->surfaces);
/*
... convert bndry points from Unibase rep. to the current
... NCL rep. (units,modsys etc.).
*/
	for (i=0,npp=0;i<nb;i++) npp += np[i];
	pp0 = p = (UM_coord *) uu_malloc (npp*sizeof(UM_coord));
	type = NCLI_POINT;
	for (i=0; i<npp;i++) from_unibase (pt[i],p[i],&type);
	
	dmin = 1.e+10;
	gouge = 0;
	um_vctovc (tool->end,te_sav);

	for (i=0, res = -1; i<nb; i++, surf +=2)
	{
/*
... skip if surf. #isrf is not related to this common bndry
... or bndry is far away
*/
		if (surf[0] == isf || surf[1] == isf)
		{
			filter = 1;
			if (!netsrf)
			{
				NCL_psmult_rec *p0,*p1;

				p0 = (NCL_psmult_rec *) netsrf + surf[0];
				p1 = (NCL_psmult_rec *) netsrf + surf[1];
				filter = p0->iuse && p1->iuse;
			}

			if (filter)
			{
				with_ext = ext_flag;
				if (np[i] >=2 && with_ext)
				{
					um_cross (tool->axis,tool->forward,v);
					um_unitvc (v,v);

					um_proj_pt_on_circle (p[0],&tool->ring,pc1);
					um_proj_pt_on_circle (p[np[i]-1],&tool->ring,pc2);

					if (um_dcccc (p[0],pc1) <= um_dcccc (p[np[i]-1],pc2))
						um_vcmnvc (p[1],p[0],vec);
					else
						um_vcmnvc (p[np[i]-1],p[np[i]-2],vec);

					um_unitvc (vec,vec);
					with_ext =  (fabs (um_dot (vec,v)) > NCL_MAX_BNDRY_TILT);
				}

				if (np[i] < 2) continue;

/*
..... Find whether the tool cylinder's side crosses the polyline.
..... If so: the distance d is negative; pb is the polyline point
..... nearest the cylinder axis; pc is the tool ring point closest
..... to pb; and new tool end is a 'rough' estimate of how far the 
..... tool should be backed up to just touch the polyline.
*/
				if (!with_ext)
					gouge = ncl_tool_pline_proj (np[i], p,tool,pb,pc,&d);
				if (gouge)
				{
					if (d < dmin) um_vctovc (tool->end,te);
					um_vctovc (te_sav,tool->end);
				}
				if (!gouge)
				{
					um_proj_pline_on_circle (with_ext,np[i], p, 
											&tool->ring, pb,v,pc);
					d = um_dcccc (pb,pc);
				}
				if (d < dmin)
				{
					*isf1 = (surf[0] == isf)? surf[1]+1: surf[0]+1; 
					res = i;
					dmin = d;
					um_vctovc (pc,tlpt);
					um_vctovc (pb,proj);
				}
			}
		}
		p += np[i];
	}

	status = (res >= 0) ? UU_SUCCESS : UU_FAILURE;

	if (status == UU_SUCCESS)
	{
		if (gouge)
		{
			status = NCL_GOUGE;
			um_vctovc (te,tool->end);
		}
		um_vctovc (tlpt, tool->look_pt);
		um_vcmnvc (tlpt,tool->ring.center,vec);
		um_unitvc (vec,tool->look_vec);
	}

	uu_free (pp0);

	return (status);
}

/*********************************************************************
**    FUNCTION     : int ncl_tool_pline_proj (np,p,tool,ptpline,ptool,dis)
**    Determine if the tool's side gouges the polyline 
**    PARAMETERS
**       INPUT  :
**              tool - the tool structure 
**              np - number of points in polyline
**              p  - polyline    
**       OUTPUT :
**              ptpline - polyline point nearest the tool axis
**              ptool - tool ring point closest to ptpline
**              dis - distance from ptpline to the tool axis
**              tool - if gouge, the new tool end is an estimate of a
**                     last not gouging position 
**    RETURNS      :
**          1 if gouge; 0 otherwise 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tool_pline_proj (np,p,tool,ptpline,ptool,dis)
int np;
UM_coord *p;
NCL_tool *tool;
UM_coord ptpline;
UM_coord ptool;
UU_REAL *dis;
{
	int i,res;
	int gouge = 0;
	UM_coord te,pt;
	UM_vector ta,tfwd,v;
	UU_REAL dmin,d, dot1,trad,um_dot();
	UM_cylinder cyl;

	um_vctovc (tool->end,te);
	um_vctovc (tool->end_forward,tfwd);
	um_vctovc (tool->axis,ta);

	um_vctovc (te,cyl.axis.p0);
	um_vctovc (ta,cyl.axis.n);
	cyl.radius = trad = tool->diameter/2.;

	dmin = 1000000.;

	for (i=0; i< np-1; i++)
	{
		res = um_segm_cross_cylinder(&cyl, p[i], p[i+1], pt, &d);
		if (res <= 0) continue;
		um_vcmnvc (pt,tool->ring.center,v);
		dot1 = um_dot (v,ta);
		if (dot1 >= tool->height || dot1 < UM_DFUZZ) continue;
		gouge = 1;
		if (d < dmin)
		{
			dmin = d;
			um_vctovc (pt,ptpline);
		}
	}
	um_nullvc (v);

	if (gouge)
	{
		UU_REAL co,si;

		co = um_dot (ta, tfwd);
		if (fabs(co) > 0.99) return (0);
		si = sqrt (1. - co*co);
		*dis = dmin - trad;
		d = (*dis)/si;
		um_avcplbvc (1.,te,d,tfwd, tool->end);

/*
..... Note: the calculation above is very rough - it is left this way
..... because it is simple and seems to be doing its job
..... It could be made more precise if needed
*/
		um_proj_pt_on_circle (ptpline,&tool->ring,ptool);
	}

	return (gouge);
}

/*********************************************************************
**    FUNCTION : int ncl_tool_bndry_proj (tool,bound,proj,vtan)
**
**    Projects inner ring of the tool on the boundary of a surface
**
**    PARAMETERS
**       INPUT  :
**              tool - tool structure
**              bound - surface boundary
**       OUTPUT :
**              proj  - point on the bndry closest to the ring
**              vtan  - tangent vector of the boundary curve at proj
**    RETURNS      :
**          UU_SUCCESS normally;
**          UU_FAILURE if sub-surf. isf does't have common boundaries
**                                 with other surfaces.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tool_bndry_proj (tool,bound,proj,vtan)
NCL_tool *tool;
UM_srf_boundary *bound;
UM_coord proj,vtan;
{
	UM_coord *pts,*pt0,*pp0,pt,ptl;
	UM_vector vec;
	int i,*np,npp,status,res;
	UM_real8 dmin,d, um_dcccc();
	UM_int2 type;

	pt0= (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
	np = bound->np;

/*
... convert bndry points from Unibase rep. to the current
... NCL rep. (units,modsys etc.).
*/
	type = NCLI_POINT;
	for (i=0,npp=0;i<bound->nb;i++) npp += np[i];
	pp0 = pts = (UM_coord *) uu_malloc (npp*sizeof(UM_coord));
	for (i=0; i<npp;i++) from_unibase (pt0[i],pts[i],&type);

	for (i=0, res = -1, dmin=1.e+10; i<bound->nb; pts += np[i],i++)
	{
		status = um_proj_pline_on_circle (UU_FALSE,np[i],pts,
			&tool->ring, pt,vec,ptl);

		if (status != UU_SUCCESS) continue;

		d = um_dcccc (pt,ptl);
		if (d < dmin)
		{
			dmin = d;
			res = i;
			um_vctovc (pt,proj);
			um_vctovc (vec,vtan);
			um_vctovc (ptl,tool->look_pt);
		}
	}

	status = (res >= 0) ? UU_SUCCESS : UU_FAILURE;

	if (status == UU_SUCCESS)
	{
		um_vcmnvc (tool->look_pt,tool->ring.center,vec);
		um_unitvc (vec,tool->look_vec);
	}

	uu_free (pp0);

	return (status);
}
