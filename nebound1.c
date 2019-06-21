/*********************************************************************
**    NAME         :  nebound1.c
**       CONTAINS:
**
**           ncl_orient_boundary
**           ncl_polygon_orientation
**           ncl_get_surf_boundary
**
**    COPYRIGHT 1998 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nebound1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:24
*********************************************************************/

#include "mgeom.h"
#include "nccs.h"
#include "nclfc.h"

/*********************************************************************
*********************************************************************/
static void S_del_lists (idel,uvlst,pplst,havexyz)
int idel;
UU_LIST *uvlst,*pplst;
UU_LOGICAL havexyz;
{
	uu_list_delete (uvlst,idel,1);
	if (havexyz) uu_list_delete (pplst,idel,1);
}

/*********************************************************************
**    FUNCTION : int ncl_orient_boundary (p)
**
**  Takes in a surface bndry and orients the outer bndry loop CCW,
**  all the inner loops CW
**
**    PARAMETERS
**       INPUT:
**              p - surf. bndry structure
**       OUTPUT :
**              p is re-oriented.
**    RETURNS :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_orient_boundary (p)
UM_srf_boundary *p;
{
	int ind,ib,np,n,ccw,i,idel;
	UU_LIST *uvlst,*pplst;
	UM_coord *uv,*pp;
	UM_2Dcoord *p2D;
	UU_REAL d;
	UU_LOGICAL havexyz,lv96;

	uvlst = p->uvpts; pplst = p->cvpts;
	havexyz = (pplst != NULLST);
	uv = (UM_coord *) UU_LIST_ARRAY(uvlst);

	ind = np = 0;

	lv96 = ncl_setver(96);
	for (ib = 0; ib < p->nb; ib++)
		if (p->np[ib] > np) np = p->np[ib];

	p2D = (UM_2Dcoord *) uu_malloc (np*sizeof(UM_2Dcoord));

	for (ib = 0; ib < p->nb; ib++)
	{
		ccw = 0;
		np = p->np[ib];
		d = 0;
		if (np > 2) d = UM_SQDIS_2D(uv[0],uv[np-2]);

/*
..... check if last point uv[np-2] should be deleted
*/
		if (d < UM_DFUZZ ||
			(np > 4 && um_segments_isect(uv[0],uv[1],uv[np-3],uv[np-2])))
		{
			idel = ind + np - 2;
			S_del_lists (idel,uvlst,pplst,havexyz);
			uv = (UM_coord *)UU_LIST_ARRAY(uvlst);
			uv += ind;

			np--;
			p->np[ib] = np;
		}

		if (!lv96)
		{
/*
..... weed the contour with tol=UM_FUZZ
*/
			for (i = 1; i < np; i++)
			{
				d = UM_SQDIS_2D(uv[i-1],uv[i]);
				if (d < UM_DFUZZ)
				{
					idel = ind + i;
					if (i == np-1) idel--;
					S_del_lists (idel,uvlst,pplst,havexyz);
					uv = (UM_coord *)UU_LIST_ARRAY(uvlst);
					uv += ind;

					i--; np--;
					p->np[ib] = np;
				}
			}
		}

		n = np - 1;
		if (np > 3)
		{
			um_convert3D_2D(np-1,uv,p2D);
			if (lv96)
				ccw = um_polygon_orientation (n,p2D);
			else
				ccw = um_polygon_orientation1 (n,p2D);

			if ((ib == 0 && ccw < 0) || (ib > 0 && ccw > 0))
			{
				ncl_revers1_list (np,0,uv,1);
				if (havexyz)
				{
					pp = (UM_coord *) UU_LIST_ARRAY(pplst);
					ncl_revers1_list (np,ind,pp,1);
				}
			}
			if (ccw == 0) break;
		}
/*
.....Delete tiny or line segment bound for shading disappear sf issue
*/
		if (np < 3 || (np == 3 && UM_SQDIS_2D(uv[0],uv[2]) < UM_DFUZZ))
		{
			np = 0;
			p->nb--;
			ccw = 1;
		}

		ind += np;
		uv += np;
	}

	uu_free (p2D);

	if (ccw == 0)
		return (UU_FAILURE);
	else
		return (UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION : int ncl_polygon_orientation (pts,npt,vz,tol)
**
**  Determine a planar polygon orientation (CCW/CW), with respect to
**  the UP direction given by a vector.
**
**    PARAMETERS
**       INPUT:
**              pts - polygon points
**              npt - number of polygon points
**              vz  - the UP vector
**              tol - tolerance
**       OUTPUT : none
**
**    RETURNS :
**          1 if CCW, -1 if CW, 0 if couldn't determine
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_polygon_orientation (pts,npt,vz,tol)
int npt;
UM_coord *pts;
UM_vector vz;
UU_REAL tol;
{
	int ccw = 0;
	int i,np;
	UM_vector v0,v1;
	UM_2Dcoord uv0,uvi,uvj;
	UU_REAL tolsq,area;

	tolsq = tol*tol;

	if (UM_DOT (vz,vz) < tolsq) return (ccw);

	np = (UM_SQDIS (pts[0], pts[npt-1]) < tolsq)? npt-1: npt;
	if (np < 3) return (ccw);

	um_2perpvc (vz,v0,v1);

	uv0[0] = UM_DOT (pts[0],v0);
	uv0[1] = UM_DOT (pts[0],v1);
	uvi[0] = UM_DOT (pts[1],v0);
	uvi[1] = UM_DOT (pts[1],v1);

	for (i = 2, area = 0.; i < np; i++)
	{
		uvj[0] = UM_DOT (pts[i],v0);
		uvj[1] = UM_DOT (pts[i],v1);
		area += um_triangle_signed_area(uv0,uvi,uvj);
		uvi[0] = uvj[0]; uvi[1] = uvj[1];
	}
	if (area > tolsq)
		ccw = 1;
	else if (-area > tolsq)
		ccw = -1;

	return (ccw);
}

/*********************************************************************
**  FUNCTION : int ncl_get_surf_boundary (ltrimd,srf,tfmat,bound,tol,lmallocd,
**                                        flg)
**     Get the trimmed surface boundary.
**  PARAMETERS
**     INPUT  :
**        srf   - pointer to surface entity
**        tfmat - surface matrix
**        tol - tolerance
**        lmallocd - if not NULL, allocate memory if more than one boundary curve,
**                   set to TRUE if allocated
**     OUTPUT :
**        bound  - surface boundary
**    RETURNS      :
**         1 iff had it stored; else UU_SUCCESS / UU_FAILURE for calculating
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_surf_boundary (ltrimd,srf,tfmat,bound,tol,lmallocd,flg)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_srf_boundary *bound;
UU_REAL tol;
UU_LOGICAL ltrimd,*lmallocd;
int flg;
{
	int status,istat;
	int nb;
	UU_REAL bplm[4];
	struct NCL_fixed_databag bs;

	bound->toler = tol;
	status = UU_SUCCESS;

	if (ltrimd)
	{
/*
...get surface data
*/
		ncl_trimsrf_get_fixed (srf,&nb,bplm);
		if (nb <= 0) return (UU_FAILURE);

		if (lmallocd != UU_NULL && nb > 1)
		{
			*lmallocd = UU_TRUE;
			bound->np = (int *) uu_malloc((nb+1)*sizeof(int));
			bound->ummx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
			bound->vmmx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
		}
	}

	istat = ncl_get_bndrlist (srf,bound,flg);
	if (istat == UU_SUCCESS) return (1);

	if (ltrimd)
	{
/*
...get base surface when processing trimmed surface
*/
		bound->nb = nb;
		bs.key = ((struct NCL_trimsf_rec *)srf)->bs_key;
		status = ncl_retrieve_data_fixed (&bs);

		if (status == UU_SUCCESS)
		status = um_evolve_bndr (tol,srf,&bs,tfmat,1,nb,bound->np,
			bound->ummx,bound->vmmx,bplm,bound,NULLST,UU_NULL);
	}
	else
	{
		bound->nb = 1;
		bound->np[0] = 5;
	}

	return (status);
}
