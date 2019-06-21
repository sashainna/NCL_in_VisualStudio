/*********************************************************************
**    NAME         :  neanalyze.c
**       CONTAINS: routines for finding geometry points by pick
**                 locations.
**
**       ncl_get_pkpt1
**       ncl_pickptonent
**       ncl_pickptocv
**       ncl_pickptosf
**       ncl_pickptosf1
**       ncl_pickptoso
**       ncl_get_trilist
**       ncl_free_trilist
**
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neanalyze.c , 25.3
**    DATE AND TIME OF LAST MODIFICATION
**       02/28/17 , 15:34:52
*********************************************************************/

#include "usysdef.h"
#include "mcrv.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "mdpick.h"
#include "mdrel.h"
#include "mgeom.h"
#include "msol.h"
#include "mdgenent.h"

#define NTRILIST 8
static UU_KEY_ID Stri_key[NTRILIST]={0,0,0,0,0,0,0,0};
static NCL_xtriangle *Stri_list[NTRILIST]=
	{UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL};
static int Stri_size[NTRILIST]={0,0,0,0,0,0,0,0};
static UU_REAL Stri_tol[NTRILIST];
static UU_REAL Stri_type[NTRILIST];

/*********************************************************************
**    E_FUNCTION     :  ncl_get_pkpt1(pick,eptr,tol,pt,vec)
**       Calculate the coordinates and tangent/normal vector of the
**       point on an entity from a screen location pick.
**    PARAMETERS
**       INPUT  :
**          pick   - First pick point.
**          tol    - Tolerance.
**       OUTPUT :
**          eptr   - Fixed data of entity picked.
**          pt     - Point on entity.
**          vec    - Tangent/Normal vector of entity at pick point.
**    RETURNS      : UU_SUCCESS if a point on the entity was calculated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_pkpt1(pick,eptr,tol,pt,vec)
UD_PLOCREC *pick;
struct NCL_fixed_databag *eptr;
UU_REAL tol;
UM_coord pt;
UM_vector vec;
{
	int status;
	UM_coord pkpt;
	UM_vector vpnorm;
	UM_real8 uv[2];
	UM_PICKENT pent;
/*
.....Calculate viewport normal
*/
	um_vpnorm(pick->pndc.transform,vpnorm);
/*
.....Project pick locations onto view plane
*/
	gndcw3(&pkpt[0],&pkpt[1],&pkpt[2],
		pick->pndc.cord[0],pick->pndc.cord[1],pick->pndc.cord[2]);
/*
.....Get picked entity
*/
	um_d_pickresolve(&(pick->ppath),1,&pent);
	eptr->key = um_get_pickkey(&pent,1);
	ncl_retrieve_data_fixed(eptr);
/*
.....Project picked point onto entity
*/
	status = ncl_pickptonent(eptr,pkpt,vpnorm,tol,pt,uv,vec);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_pickptonent()
**       Project a point onto a geometric entity down a vector.
**    PARAMETERS
**       INPUT  :
**          ploc       - Pick/locate record of picked entity.
**          eptr       - Pointer to entity fixed data.
**          npt1       - Point to project.
**          vpnorm     - vector to project down.
**          tol        - tolerance.
**       OUTPUT :
**          spt1       - Point on entity.
**          uv         - U value of point on curve or U and V value of
**                       point on surface.
**          vec        - Vector normal to surface or tangent to curve at
**                       point.
**    RETURNS      : UU_SUCCESS if successful, else UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pickptonent(eptr, pkpt, vpnorm, tol, spt1, huv, vec)
struct NCL_fixed_databag *eptr;
UM_coord pkpt, spt1;
UM_vector vpnorm, vec;
UU_REAL tol, huv[2];
{
	int n1;
	int status = UU_SUCCESS;
	UM_int2 rn1;
	UU_REAL d1;
	UM_vector pt1, pt2;
	UM_vector vc1;
	struct UM_point_rec *ptx;
	struct UM_line_rec *lnx;
	struct NCL_vector_rec *vex;
	struct UM_circle_rec *cix;
	struct NCL_nclpv_rec *pvx;
	struct NCL_nclpl_rec *plx;
	UM_transf tfmat1;
/*
.....Get type of geometry to project
*/
	ncl_get_type(eptr->rel_num,&rn1);
	um_vctovc(pkpt,pt1);
/*
.....Calculate point on entity & vector
*/
	switch (rn1)
	{
/*
........Point (point only)
*/
	case NCLI_POINT:
		ptx = (struct UM_point_rec *)eptr;
		um_vctovc(ptx->pt,spt1);
		vec[0] = vec[1] = vec[2] = 0.;
		break;
/*
........Point-vector
*/
	case NCLI_POINTVEC:
		pvx = (struct NCL_nclpv_rec *)eptr;
		um_vctovc(pvx->pt,spt1);
		um_unitvc(pvx->ve,vec);
		break;
/*
........Vector (vector only)
*/
	case NCLI_VECTOR:
		vex = (struct NCL_vector_rec *)eptr;
		spt1[0] = spt1[1] = spt1[2] = 0.;
		um_unitvc(vex->vec,vec);
		break;
/*
........Plane
*/
	case NCLI_PLANE:
		plx = (struct NCL_nclpl_rec *)eptr;
		um_ilnpln(pt1,vpnorm,plx->pt,plx->nvec,&n1,spt1);
		um_unitvc(plx->nvec,vec);
		break;
/*
........Line
*/
	case NCLI_LINE:
		lnx = (struct UM_line_rec *)eptr;
		um_vcmnvc(lnx->ept,lnx->spt,vec);
		um_unitvc(vec,vec);
		um_lnlndis(lnx->spt,vec,pt1,vpnorm,&d1,spt1);
		break;
/*
........Circle
*/
	case NCLI_CIRCLE:
		cix = (struct UM_circle_rec *)eptr;
		um_ilnpln(pt1,vpnorm,cix->center,cix->nvec,&n1,pt2);
		if (n1 == 0)
		{
			um_nptpln(pt1,cix->center,cix->nvec,pt2);
		}
		um_vcmnvc(pt2,cix->center,vc1);
		um_unitvc(vc1,vc1);
		um_vctmsc(vc1,cix->radius,vc1);
		um_vcplvc(cix->center,vc1,spt1);
		um_cross(cix->nvec,vc1,vec);
		break;
/*
.........Curve 
*/
	case NCLI_CURVE:
		status = ncl_pickptocv(eptr, pkpt, vpnorm, tol, spt1, huv, vec);
		break;
/*
.........Surf
*/
	case NCLI_SURF:
		status = ncl_pickptosf(eptr, pkpt, vpnorm, tol, UU_TRUE, spt1, huv, vec);
		break;
/*
.........Solid
*/
	case NCLI_SOLID:
		status = ncl_pickptoso(eptr, pkpt, vpnorm, tfmat1, tol, spt1, vec);
		break;
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_pickptocv()
**       Project a point onto a curve down a vector.
**    PARAMETERS
**       INPUT  :
**          eptr       - Pointer to entity fixed data.
**          pkpt       - Point to project.
**          vpnorm     - vector to project down.
**          tol        - tolerance.
**       OUTPUT :
**          spt1       - Point on entity.
**          huv        - U value of point on curve
**          vec        - Vector tangent to curve at point.
**    RETURNS      : UU_SUCCESS if successful, else UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pickptocv(eptr, pkpt, vpnorm, tol, spt1, huv, vec)
struct NCL_fixed_databag *eptr;
UM_coord pkpt, spt1;
UM_vector vpnorm, vec;
UU_REAL tol, huv[2];
{
	int n1, iflg, j;
	UM_int2 iwf = 0;
	UM_real8 u;
	UM_int4 ierr, unflg = 1;
	UU_REAL d1, d1min;
	UM_vector pt1, pt2, pt3,vc1;
	UU_LIST ptlst;
	UM_transf tfmat;
	UM_coord *pts;
	UM_coord bpt1,ept1;
	UU_REAL ncl_lnlndis();
	int status = UU_FAILURE;

/*
...........Project point onto curve along vpnorm
*/
	ierr = 0;
	um_vctovc (pkpt,pt1);
	um_vctovc (vpnorm,vc1);

		uu_list_init(&ptlst,sizeof(UM_coord),100,100);
		uc_retrieve_transf(eptr->key,tfmat); 
		n1 = ncl_evolve_all_curves(eptr,tfmat,tol,&ptlst,UU_NULL,UU_FALSE);
		if (n1 > 1)
		{
			d1min = 1.e12;
/*
..... create a long line through the pick point along the viewport normal
..... find the curve segment closest to the line
..... project the corresponding view line point onto the curve
*/
			for (j = 0; j < 3; j++)
			{
				ept1[j] = pt1[j] - 1000.*vc1[j];
				bpt1[j] = pt1[j] + 1000.*vc1[j];
			}

			pts = (UM_coord *)UU_LIST_ARRAY(&ptlst);
			for (j = 0; j < n1-1 && d1min >= UM_FUZZ; j++)
			{
				d1 = ncl_lnlndis(bpt1,ept1,pts[j],pts[j+1],pt1,pt2);
				if (d1 < d1min)
				{
					d1min = d1;
					um_vctovc(pt2,pt3);
					if (d1 < tol) status = UU_SUCCESS;
				}
			}
		}
		uu_list_free(&ptlst);

		if (status == UU_SUCCESS)
		{
			u = .5;
			iflg = 10;
			ncl_wcstomcs(0,pt3,pt3);
			cvpv2(&(eptr->key),&iwf,pt3,&iflg,&u,&unflg,spt1,vec,&ierr);
			ncl_mcstowcs(0,spt1,spt1);
			ncl_mcstowcs(1,vec,vec);
			if (ierr != 0) status = UU_FAILURE;
			huv[0] = u;

		}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_pickptosf(eptr,pkpt,vpnorm,tol,
**									lintrim,spt,huv,svec)
**       Project a point onto a surface down a vector.
**    PARAMETERS
**       INPUT  :
**          eptr       - Pointer to entity fixed data.
**          pkpt       - Point to project.
**          vpnorm     - vector to project down.
**          tol        - tolerance.
**			lintrim    - to check if the point inside boundary or not
**						 for trimmed surface
**       OUTPUT :
**          spt        - Point on entity.
**          huv        - U and V value of point on surface.
**          vec        - Vector normal to surface
**    RETURNS      : UU_SUCCESS if successful, else UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pickptosf(eptr,pkpt,vpnorm,tol,lintrim,spt,huv,svec)
struct NCL_fixed_databag *eptr;
UM_coord pkpt, spt;
UM_vector vpnorm, svec;
UU_REAL tol, huv[2];
UU_LOGICAL lintrim;
{
	int status,stat;
	UM_real8 u, v;
	UU_REAL d1, d1min;
	UM_srf_boundary bound;
	int trimd = 0;
	int i,j,insf;
	UU_REAL tolsq;
	UM_coord *uvptr,uvi;
	UM_coord spi,vci;
	UM_int2 primtyp;
	UM_int4 skey;

	skey = eptr->key;
	tolsq = tol*tol;
	if (tolsq < UM_DFUZZ) tolsq = UM_DFUZZ;

	ncl_get_sf_primtyp(&skey, &primtyp);
	if (primtyp == 3)
	{
		u = v = 0.5;
		status = ncl_pickptosf1(skey, pkpt, vpnorm, tolsq, &u, &v, spt, svec);
		if (status == UU_SUCCESS)
		{
			huv[0] = u;
			huv[1] = v;
		}
	}
	else
	{
		if (ncl_itsa_trimsrf(eptr))
		{
			trimd = 1;
			ncl_set_boundary_toler (tol);
			status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,eptr,&bound);
			if (bound.nb <= 0 || status != UU_SUCCESS)
				trimd = 0;
			else
				uvptr = (UM_coord *) UU_LIST_ARRAY (bound.uvpts);
		}
		d1min = -1.e12;
		uvi[2] = 0;

		status = UU_FAILURE;
		for (i = 0; i < 3; i++)
		{
			for (j = 0; j < 3; j++)
			{
				u = (2*i+1.)/6.;
				v = (2*j+1.)/6.;
				stat = ncl_pickptosf1(skey,pkpt,vpnorm,tolsq,&u,&v,spi,vci);
/*
//test, or not use if statement	since we need a value to return			
//				if (stat == UU_SUCCESS)
*/
				{
					d1 = UM_DOT (vpnorm,spi);
					if (d1 > d1min)
					{
						if (lintrim && trimd == 1)
						{
							uvi[0] = u;
							uvi[1] = v;
  							insf = um_cshape_inschk(uvptr,bound.nb,bound.np,uvi,
									bound.ummx,bound.vmmx);
							if (insf < 0) continue;
						}
						d1min = d1;
						huv[0] = u;
						huv[1] = v;
						um_vctovc (spi,spt);
						um_vctovc (vci,svec);
						status = UU_SUCCESS;
					}
				}
			}
		}
	}
	if (trimd == 1) um_free_boundary (&bound);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_pickptosf1()
**       Project a point onto a surface entity down a vector.
**    PARAMETERS
**       INPUT  :
**          ploc       - Pick/locate record of picked entity.
**          eptr       - Pointer to entity fixed data.
**          npt1       - Point to project.
**          vpnorm     - vector to project down.
**          tol        - tolerance.
**       OUTPUT :
**          spt1       - Point on entity.
**          uv         - U value of point on curve or U and V value of
**                       point on surface.
**          vec        - Vector normal to surface or tangent to curve at
**                       point.
**    RETURNS      : UU_SUCCESS if successful, else UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pickptosf1(skey, pkpt, vpnorm, tolsq, u0, v0, spt, svec)
UM_int4 skey;
UM_coord pkpt, spt;
UM_vector vpnorm, svec;
UM_real8 *u0, *v0;
UU_REAL tolsq;
{
	int n1;
	int status = UU_FAILURE;
	UM_real8 u, v, dir;
	UM_int4 ierr, unflg = 1;
	UU_REAL d1, d1min;
	UM_coord pt1, pt2, pt3;
	UM_vector vc2;
	int j;
	UM_coord ptmin;

/*
...........Project point onto surface along vpnorm
*/
	u = *u0;
	v = *v0;
	dir = 0.;
	d1min = 1.e12;
	um_vctovc (pkpt,pt1);

	for (j = 0; j < 100 && d1min >= UM_DFUZZ; j++)
	{
		sfpt2(&skey, pt1, &u, &v, &unflg, &dir, pt2, vc2, &ierr);
		if (ierr != 0) break;
		um_ilnpln(pt1,vpnorm,pt2,vc2,&n1,pt3);
		if (n1 == 0)
		{
			um_nptpln(pt1,pt2,vpnorm,pt3);
		}
		d1 = UM_SQDIS(pt1,pt3);
		if (d1 < d1min)
		{
			d1min = d1;
			*u0 = u;
			*v0 = v;
			um_vctovc (pt1,ptmin);
			if (d1 < tolsq) status = UU_SUCCESS;
		}
		um_vctovc(pt3,pt1);
	}
	if (ierr != 0) status = UU_FAILURE;

	if (status == UU_SUCCESS)
	{
		sfpt2(&skey, ptmin, u0, v0, &unflg, &dir, spt, svec, &ierr);
		d1 = UM_SQDIS(ptmin,spt);
		if (d1 > tolsq)
			status = UU_FAILURE;
	}	
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_pickptoso(eptr, pkpt, vpnorm, tol, spt, svec)
**       Project a point onto a solid down a vector.
**    PARAMETERS
**       INPUT  :
**          eptr       - Pointer to entity fixed data.
**          pkpt       - Point to project.
**          vpnorm     - vector to project down.
**          spt        - near point (if using nearpoint)
**          tol        - tolerance.
**       OUTPUT :
**          spt        - Point on entity.
**          vec        - Vector normal to surface
**    RETURNS      : UU_SUCCESS if successful, else UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pickptoso(eptr, pkpt, vpnorm, tfmat1, tol, spt, svec)
struct UM_solid_rec *eptr;
UM_coord pkpt, spt;
UM_vector vpnorm, svec;
UM_transf tfmat1;
UU_REAL tol;
{
	int i,status,n1,isec,inc;
	UU_LOGICAL first;
	UU_REAL d1;
	UM_coord pt1,pt2,nrpt;
	UM_vector nvec,tvec;
	NCL_xtriangle *xtp1;
	struct NCL_nclpv_rec pv;
/*
.....Tessellate solid within tolerance
*/
	status = ncl_get_trilist(eptr,tol,UM_TESS_TOLER,100,&xtp1,&n1);
	if (status != UU_SUCCESS) goto failed;
/*
.....Get intersection of point-vector and solid
*/
	first = UU_TRUE;
	for (i=0;i<n1;i++)
	{
		isec = 0;
		ncl_segtri_io(pkpt,pkpt,vpnorm,&xtp1[i],pt1,pt2,nvec,tol,&isec);
/*
........Determine closest intersection
*/
		if (isec == 1)
		{
			if (first)
			{
				um_vctovc(pt1,spt);
				um_vctovc(nvec,svec);
				first = UU_FALSE;
			}
			else
			{
				um_vcmnvc(pt1,spt,tvec);
				d1 = um_dot(vpnorm,tvec);
				if (d1 > 0.)
				{
					um_vctovc(pt1,spt);
					um_vctovc(nvec,svec);
				}
			}
		}
	}
	if (first) goto failed;
	status = UU_SUCCESS;
	goto done;
/*
.....Could not project point
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
.....Free memory
*/
done:;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_get_trilist(eptr,tol,tess_type,grid,tript,ntri)
**       Returns a pointer to the triangle list generated for the entity
**       'eptr'.  If a triangle list does not exist, then one will be
**       created.
**    PARAMETERS
**       INPUT  :
**          eptr       - Pointer to entity to calculate triangles for.
**          tol        - tolerance.
**          tess_type  - Tessellation type: UM_TESS_TOLER, UM_TESS_GRID,
**                       UM_TESS_BOTH.
**          grid       - Grid size when tessellation requires grid.
**       OUTPUT :
**          tript      - Triangle list.
**          ntri       - Number of triangles in triangle list.
**    RETURNS      : UU_SUCCESS if successful, else UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_trilist(inptr,tol,tess_type,grid,tript,ntri)
struct UM_srfdatabag *inptr;
UU_REAL tol;
int tess_type;
int grid;
NCL_xtriangle **tript;
int *ntri;
{
	int i,status,n1,inc,nkeys;
	UM_int2 idx;
	UU_LOGICAL compfl;
	UU_REAL tsav,rval;
	UM_trian *tri1;
	UM_tessellation tess;
	NCL_xtriangle *xtp1;
	UU_LIST dlist,list1;
	struct UM_solid_rec *solid,csol;
	struct UM_srfdatabag *eptr,e1;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	uu_list_init0(&dlist);
	uu_list_init0(&list1);
	tess.np = 0;
	xtp1 = UU_NULL;
	eptr = inptr;
/*
.....Determine if a triangle list
.....already exists
*/
	inc = -1;
	for (i=0;i<NTRILIST;i++)
	{
		if (eptr->key == Stri_key[i])
		{
			if (tess_type != Stri_type[i] || tol != Stri_tol[i])
			{
				Stri_key[inc] = 0;
				uu_free(Stri_list[i]);
				inc = i;
				break;
			}
			*tript = Stri_list[i];
			*ntri = Stri_size[i];
			goto done;
		}
		else if (Stri_key[i] == 0 && inc == -1) inc = i;
	}
/*
.....Make room for new triangle list
*/
	if (inc == -1)
	{
		uu_free(Stri_list[0]);
		for (i=1;i<NTRILIST;i++)
		{
			Stri_list[i-1] = Stri_list[i];
			Stri_key[i-1] = Stri_key[i];
			Stri_size[i-1] = Stri_size[i];
		}
		inc = NTRILIST - 1;
	}
/*
.....Composite solid
*/
	compfl = UU_FALSE;
	nkeys = 1;
	if (eptr->rel_num == UM_SOLID_REL)
	{
		solid = (struct UM_solid_rec *)eptr;
		if (solid->type == UM_COMPOS_SOLID)
		{
			compfl = UU_TRUE;
			csol = *solid;
			nkeys = solid->no_netkey;
		}
	}
/*
.....Loop through solids
*/
	for (i=0;i<nkeys;i++)
	{
		if (compfl)
		{
			e1.key = csol.netkey[i];
			status = uc_retrieve_data(&e1);
			if (status != UU_SUCCESS) goto failed;
			eptr = &e1;
		}
/*
.....Tessellate solid within tolerance
*/
		if (eptr->rel_num == UM_SOLID_REL)
		{
			idx = 175; getsc(&idx,&tsav); rval = tol; setscv(&idx,&rval);
			solid = (struct UM_solid_rec *)eptr;
			status = ncl_solid_calc_lists(solid,solid->sdata,solid->no_sdata,
				&dlist,&tess);
			setscv(&idx,&tsav);
			if (status != UU_SUCCESS) goto failed;
		}
/*
.....Tessellate surface within tolerance
*/
		else
		{
			ncl_set_boundary_toler(tol);
			ncl_set_tess_parms(tess_type,tol,grid,grid);
			um_set_tess_toler(tol);
			um_init_tess(&tess);
			status = ncl_tessellate_surf(eptr,&tess);
			if (status != UU_SUCCESS) goto failed;
		}
/*
.....Get the tessellation triangles
*/
		if (i == 0)
			uu_list_init1(&list1,sizeof(UM_trian),tess.ntri,tess.ntri);
		status = ncl_get_tess_triangles(&tess,&list1,2,0);
		if (status != UU_SUCCESS) goto failed;
	}
/*
.....Create triangle list
*/
	n1 = UU_LIST_LENGTH(&list1);
	tri1 = (UM_trian *)UU_LIST_ARRAY(&list1);
	xtp1 = (NCL_xtriangle *)uu_malloc(n1*sizeof(NCL_xtriangle));
	ncl_setup_xtriangles(n1,tri1,xtp1);
/*
.....Setup return values
*/
	Stri_key[inc] = inptr->key;
	Stri_list[inc] = *tript = xtp1;
	Stri_size[inc] = *ntri = n1;
	Stri_type[inc] = tess_type;
	Stri_tol[inc] = tol;
	status = UU_SUCCESS;
	goto done;
/*
.....Could not create triangle list
*/
failed:;
	if (xtp1 != UU_NULL) uu_free(xtp1);
	status = UU_FAILURE;
/*
.....End of routine
.....Free memory
*/
done:;
	uu_list_free(&dlist);
	if (tess.np != 0) um_free_tess(&tess);
	uu_list_free(&list1);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_free_trilist()
**       Frees the memory allocated for static triangle lists.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_free_trilist()
{
	int i,status,n1,inc;
	UM_int2 idx;
	UU_REAL tsav,rval;
	UM_trian *tri1;
	UM_tessellation tess;
	NCL_xtriangle *xtp1;
	UU_LIST dlist,list1;
	struct UM_solid_rec *solid;
/*
.....Free triangle lists
*/
	for (i=0;i<NTRILIST;i++)
	{
		if (Stri_key[i] != 0)
		{
			Stri_key[i] = 0;
			uu_free(Stri_list[i]);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  ncl_free_single_trilist()
**       Frees the memory allocated for the specified triangle list.
**    PARAMETERS
**       INPUT  :
**          key   = Key of entity to delete triangle list for.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_free_single_trilist(key)
UU_KEY_ID key;
{
	int i,status,n1,inc;
	UM_int2 idx;
	UU_REAL tsav,rval;
	UM_trian *tri1;
	UM_tessellation tess;
	NCL_xtriangle *xtp1;
	UU_LIST dlist,list1;
	struct UM_solid_rec *solid;
/*
.....Free triangle lists
*/
	for (i=0;i<NTRILIST;i++)
	{
		if (key == Stri_key[i])
		{
			Stri_key[i] = 0;
			uu_free(Stri_list[i]);
		}
	}
}

