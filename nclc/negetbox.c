/*********************************************************************
**    NAME   :  negetbox.c
**       CONTAINS:  Routines to construct bounding box around a surface 
**
**          ncl_get_box
**          ncl_sf_box
**          ncl_get_srf_pts
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       negetbox.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:35
*********************************************************************/

#include "nccs.h"
#include "mdeval.h"
#include "uminmax.h"
#include "mdrel.h"
#include "msrf.h"
#include "mgeom.h"
#include "nclxmdl.h"
#include "nclfc.h"

extern int NCLX_internal_geom;

/*********************************************************************
**   FUNCTION : int ncl_get_box (sf, box)
**
**      Constructs a 3-D box around a surface.  The orientation of the
**      box will not necessarily lie on the XY-plane.
**
**    PARAMETERS   
**       INPUT  : 
**                 sf  - surface
**       OUTPUT :  
**                 box - pointer to the box around sf.
**    RETURNS : 
**               UU_SUCCESS if O.K., UU_FAILURE if problem
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_box  (sf,box)
struct NCL_fixed_databag *sf; 
UM_3D_box *box;
{
	int status, n_pt, boxlst;
	UU_LIST points;
	UM_coord *pts;
	UU_LOGICAL use_ctrl_pts = UU_FALSE;

	status = ncl_get_surflist (BOX_LIST,sf, box);
	if (!status) return (UU_SUCCESS);

	if (use_ctrl_pts)
	{
		struct UM_rbsplsrf_rec *sf1 = (struct UM_rbsplsrf_rec *) sf;
		status = um_3dbox_around(sf1->no_pt, sf1->pt, box);
	}
	else
	{
		uu_list_init (&points, sizeof(UM_coord), 200, 200);
		status = ncl_get_srf_pts(sf, &points);
		if (status) return UU_FAILURE;
	}

	n_pt = UU_LIST_LENGTH(&points);
	pts = (UM_coord *) UU_LIST_ARRAY(&points);
	status = um_3dbox_around(n_pt, pts, box);
	uu_list_free (&points);

   if (status < 0) return (UU_FAILURE);
/*
... save box (8 vertices) in the Unibase record of this surface
*/
	status = ncl_store_surflist (BOX_LIST, sf, box);

  	return (status);
}

/*********************************************************************
**   FUNCTION : int ncl_sf_box(sf,box)
**
**      Constructs an XYZ box around a surface.
**
**    PARAMETERS   
**       INPUT  : 
**                 sf  - surface
**       OUTPUT :  
**                 box - pointer to the box around sf.
**    RETURNS : 
**               UU_SUCCESS if O.K., UU_FAILURE if problem
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sf_box(sf,box)
struct NCL_fixed_databag *sf; 
UU_REAL *box;
{
	UM_int2 i3;
	int status,i,j,n_pt,ist,nsf;
	UU_KEY_ID *keys;
	UU_LIST points;
	UM_coord *pts;
	struct NCL_netsf_rec *netsf;
	struct UM_srfdatabag srf;
/*
.....Get surface points
*/
	i3 = 3;
	uu_list_init (&points, sizeof(UM_coord), 200, 200);
/*
.....Loop through Net surface
*/
	if (sf->rel_num == NCL_NETSF_REL)
	{
		netsf = (struct NCL_netsf_rec *)sf;
		nsf = netsf->no_netkey;
		keys = netsf->netkey;
	}
	else
	{
		nsf = 1;
		keys = &sf->key;
	}
/*
.....Get surface data
*/
	for (j=0;j<nsf;j++)
	{
		srf.key = keys[j];
		ncl_retrieve_data_fixed(&srf);
/*
........Get surface points
*/
		status = ncl_get_srf_pts(&srf, &points);
		if (status) return UU_FAILURE;
/*
........Calculate box from points
*/
		n_pt = UU_LIST_LENGTH(&points);
		pts = (UM_coord *) UU_LIST_ARRAY(&points);
		um_mcstoccs(0,pts[0],pts[0]);
		conref(pts[0],&i3);
		if (j == 0)
		{
			ist = 1;
			ncl_init_box(pts[0],box);
		}
		else
			ist = 0;
		for (i=ist;i<n_pt;i++)
		{
			um_mcstoccs(0,pts[i],pts[i]);
			conref(pts[i],&i3);
			ncl_update_box(pts[i],box);
		}
	}
/*
.....Free the points list
*/
	uu_list_free (&points);
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**  FUNCTION : int ncl_get_srf_pts (eptr, points)
**      Constructs a set of points representing a surface
**      (used to build a bounding box around the srf).
**  PARAMETERS   
**    INPUT : 
**        eptr    - ptr to surface
**    OUTPUT :  
**        np      - # of points evaluated
**        points  - list of the points
**  RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_srf_pts (eptr,points)
struct NCL_fixed_databag *eptr;
UU_LIST *points;
{
	int nrow_u = 8, nrow_v = 8;         /* define 64 points uniformly */
                                       /* put onto box [umin,umax]x[vmin,vmax] */
	UU_REAL u, v, du, dv, umax, umin, 
	        vmax, vmin, ncl_get_boundary_toler();
	int status,i,j, init = 0,isin;
	UM_coord *uvdat, uv_point;
	UM_transf t, *t1;
	struct UM_evsrfout evsrf;
	UM_srf_boundary b;

	status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,eptr,&b);

	if (status == UU_SUCCESS)
	{
		init = 1;
		uu_list_push_list (points, b.cvpts);
		uvdat = (UM_coord *) UU_LIST_ARRAY (b.uvpts);
		status = (UU_LIST_LENGTH(b.uvpts) > 0) ? UU_SUCCESS : UU_FAILURE;
	}

	if (status != UU_SUCCESS) goto Done;

  	t1= &t;
	status = ncl_trimsrf_get_tf (eptr,&t1);
	if (status != UU_SUCCESS) goto Done;

	umin = MAX2 (0.,b.ummx[0][0]);
	umax = MIN2 (1.,b.ummx[0][1]);
	vmin = MAX2 (0.,b.vmmx[0][0]);
	vmax = MIN2 (1.,b.vmmx[0][1]);
/*
... add some more points uniformly distributed in u-v space
*/
	du = (umax - umin)/(nrow_u + 1.);
	dv = (vmax - vmin)/(nrow_v + 1.);


	for (i=0, u=du; i<nrow_u; i++, u=u+du)
		for (j=0, v=dv; j<nrow_v; j++, v=v+dv)
		{
/*
... for trim srf, take only the points inside boundary
*/
			isin = 1;
			if (ncl_itsa_trimsrf (eptr) )
			{
				um_xyztovc (u,v, (UU_REAL ) 0.0, uv_point);   
				isin = um_cshape_inschk (uvdat,b.nb,b.np,uv_point,b.ummx,b.vmmx);
			}

			if (isin > 0)
			{
   			status = ncl_evsrf_tf (UM_POINT,u,v,eptr,t1,&evsrf);
				if (status == UU_SUCCESS) uu_list_push (points,evsrf.sp);
			}
		}

Done:;
	if (init) um_free_boundary (&b);

	return (status);
} 

