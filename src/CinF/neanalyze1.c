/*********************************************************************
**    NAME         :  neanalyze1.c
**       CONTAINS: Routines to measure the distance between entities.
**
**			ncl_geom_distf
**			ncl_geom_distance
**			ncl_geom_closest
**			ncl_pttridis
**			ncl_lnlndis
**			ncl_segtridis
**			ncl_tritridis
**
**    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       neanalyze1.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       08/17/15 , 17:45:08
*********************************************************************/

#include "usysdef.h"
#include "wsgl.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "gtbl.h"
#include "mdrel.h"
#include "modef.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdeval.h"
#include "misect.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "driver.h"
#include "mgeom.h"
#include "lcom.h"

static UM_int2 IPT = NCLI_POINT;
static UM_int2 IVE = NCLI_VECTOR;

UU_REAL ncl_pttridis(),ncl_lnlndis(),ncl_tritridis();

/*********************************************************************
**    E_FUNCTION     :  ncl_geom_distf(nclkey1, nclkey2, val)
**       Calculate distances between entities.
**    PARAMETERS
**       INPUT  :
**          nclkey1, nclkey2      - entity key
**       OUTPUT :
**          val:  distances between entities.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_geom_distf(nclkey1, nclkey2, val, err)
int *nclkey1, *nclkey2, *err;
UU_REAL *val;
{
	int stat;
	struct NCL_fixed_databag Se[2];
	struct NCL_fixed_databag e1, e2;
	UU_REAL dis;
	UM_coord pt[2];
	UM_vector vc[2];
	*err = 0;
	e1.key = *nclkey1;
	e2.key = *nclkey2;
	ncl_retrieve_data_fixed(&e1);
	ncl_retrieve_data_fixed(&e2);
	Se[0] = e1;
	Se[1] = e2;
	stat = ncl_geom_distance(Se, pt, vc, 0, &dis,UU_TRUE,UU_FALSE,0);
	if (stat==-1)
	{
		*err = 1;
		return;
	}
	*val = dis;
}

/*********************************************************************
**    E_FUNCTION     :  S_planar_distance (e,rn,dis1)
**       Calculate distances between two parallel planar surfaces using the
**       surfaces stored primitive data, exactly as NCL 9.5.
**    PARAMETERS
**       INPUT  :
**          e         - surface pointers.
**			rn		  - surface type
**       OUTPUT :
**          dis1:  distances between entities.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_planar_distance (e,rn,dis1)
struct NCL_fixed_databag e[2];
UM_int2 rn[2];
UU_REAL *dis1;
{
	int i,k,istat,status;
	UM_int4 skey;
	UM_int2 primtyp;
	UM_real8 primdata[16];

	UM_coord ptt[2];
	UM_vector vcc[2];
	UM_vector uvc[2];
	UU_REAL dd[2],co;
	struct NCL_nclpl_rec *pl1;

	status = UU_FAILURE;

	for (k = 0; k < 2; k++)
	{
		if (rn[k] == NCLI_SURF)
		{
			skey = e[k].key;
			istat = ncl_get_sf_primdat(&skey,&primtyp,primdata);
			if (istat != UU_SUCCESS || primtyp != 3) return (status);

			for (i = 0; i < 3; i++)
			{
				vcc[k][i] = primdata[i];
				ptt[k][i] = primdata[i+4];
			}
		}
		else if (rn[k] == NCLI_PLANE)
		{
			pl1 = (struct NCL_nclpl_rec *)&e[k];
			um_vctovc(pl1->nvec,vcc[k]);
			um_vctovc(pl1->pt,ptt[k]);
		}
		conpt(vcc[k],&IVE);
		conpt(ptt[k],&IPT);
		um_unitvc(vcc[k],uvc[k]);
		dd[k] = UM_DOT (uvc[k],ptt[k]);
	}

	co = UM_DOT (uvc[0],uvc[1]);
	if (co < 0)
	{
		co = -co;
		dd[1] = -dd[1];
	}
/*
..... use cos(0.001) to determine if planes are parallel, as in NCL 9.5
*/
	if (co > 0.9999995)
	{
		*dis1 = fabs(dd[0] - dd[1]);
		status = UU_SUCCESS;
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_geom_distance(e,pt,vc,flag,dis1,exten,
**                                        grid_flag,grid)
**       Calculate distances between entities.
**    PARAMETERS
**       INPUT  :
**          e         - Fixed entity data.
**          flag      - 1: picked entity, need calculate pt,vc
**                      0: ignore pt and vc
**          pt        - Point on entity where picked.
**          vc        - Tangent/Normal vector at picked location.
**          exten     - UU_TRUE = Use surface extensions, UU_FALSE = Don't.
**          grid_flag - UU_TRUE = Use surface grid only for tessellation.
**                      UU_FALSE = Use tolerance and optional grid for
**                      tessellation.
**          grid      - Specifies the grid to use for surface tessellation.
**                      Can be set to 0 which means no grid.
**       OUTPUT :
**          dis1:  distances between entities.
**    RETURNS      : -1 if could not calculate distance.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_geom_distance(e, pt, vc,flag, dis1,exten,grid_flag,grid)
struct NCL_fixed_databag e[2];
UM_coord pt[2];
UM_vector vc[2];
int flag;
UU_REAL *dis1;
UU_LOGICAL exten;
UU_LOGICAL grid_flag;
int grid;
{
	int k,stat,status;
	UU_LOGICAL found[2],vcfl[2],ptfl[2];
	UM_int2 rn[2];
	UM_coord pt1,pt2;
	UM_vector vc1,vc2;
	UM_vector uvc[2];
	UM_line ln;
	UM_int2 primtyp0,primtyp1;
	UM_real8 primdata[16];

/*
.....Initialize routine
*/
	ptfl[0] = ptfl[1] = UU_TRUE;
	vcfl[0] = vcfl[1] = UU_TRUE;
	found[0] = found[1] = UU_TRUE;
/*
........Store label and type
*/
	for (k=0;k<2;k++)
	{
		ncl_get_type(e[k].rel_num,&rn[k]);
	}

/*
.....Check if surface is planar
*/
	if (rn[0] == NCLI_SURF)
		status = ncl_get_sf_primdat(&e[0].key,&primtyp0,primdata);

    if (rn[1] == NCLI_SURF)
		status = ncl_get_sf_primdat(&e[1].key,&primtyp1,primdata);

/*
........Return distance between parallel planar surfaces
*/
	if (((rn[0] == NCLI_SURF && primtyp0 == NCLSF_PLANE) || rn[0] == NCLI_PLANE) &&
		((rn[1] == NCLI_SURF && primtyp1 == NCLSF_PLANE) || rn[1] == NCLI_PLANE) && exten)
	{
		stat = S_planar_distance (e,rn,dis1);
		if (stat == UU_SUCCESS) return (stat);
	}
/*
.....Find closest points
*/
	stat = ncl_geom_closest(pt,e,rn,pt,vc, flag,exten,grid_flag,grid);
	ncl_free_trilist();
	if (stat != 0) return(stat);

	for (k=0;k<2;k++)
	{
		switch (rn[k])
		{
		case NCLI_POINT:
			vcfl[k] = UU_FALSE;
			break;
/*
		case NCLI_VECTOR:
			ptfl[k] = UU_FALSE;
			break;
*/
		case NCLI_POINTVEC:
		case NCLI_LINE:
		case NCLI_PLANE:
		case NCLI_CIRCLE:
		case NCLI_CURVE:
		case NCLI_SURF:
		case NCLI_SOLID:
			break;
/*
........Unsupported entity
*/
		default:
			found[k] = UU_FALSE;
			break;
		}
/*
.....Cannot measure between geometry types
*/
		if ((!ptfl[0] || !ptfl[1]) && !(vcfl[0] && !vcfl[1])) found[k] = UU_FALSE;
	}
/*
.....Adjust points for MODSYS
*/
	if (ptfl[0])
	{
		um_vctovc(pt[0],pt1);
		conpt(pt1,&IPT);
		um_vctovc(vc[0],vc1);
		conpt(vc1,&IVE);
	}
	if (ptfl[1])
	{
		um_vctovc(pt[1],pt2);
		conpt(pt2,&IPT);
		um_vctovc(vc[1],vc2);
		conpt(vc2,&IVE);
	}
/*
.....Calculate point (closest) distance
*/
	if (found[0] && found[1])
	{
		if (ptfl[0] && ptfl[1])
		{
			*dis1 = um_dcccc(pt1,pt2);
/*
..... Return distance between parallel surfaces - now performed in
..... S_planar_distance (e,dis1)

			if (rn[0] == NCLI_SURF && rn[1] == NCLI_SURF && exten)
			{
				for (k = 0; k < 2; k++)	
					um_unitvc(vc[k],uvc[k]);

				if (um_vcparall(uvc[0],uvc[1]))
				{
					um_vctovc(pt1,pln.p0);
					um_unitvc(vc1,pln.n);
					um_proj_pt_on_plane(1,pt2,&pln,ptx);
					*dis1 = um_dcccc(pt2,ptx);
				}
			}
*/
			if (rn[0] == NCLI_LINE && rn[1] == NCLI_LINE && exten)
			{
				for (k = 0; k < 2; k++)	
					um_unitvc(vc[k],uvc[k]);

				if (um_vcparall(uvc[0],uvc[1]))
				{
					UU_REAL um_dist_from_line();

					um_vctovc(pt1,ln.p0);
					um_unitvc(vc1,ln.n);
					*dis1 = um_dist_from_line(pt2,&ln);
				}
			}
/*
.....Performed in 'conent'
*/
/*			UM_len_inttoext(*dis1, *dis1);*/
			return(UU_SUCCESS);
		}
	}
	return(UU_FAILURE);
}

/*********************************************************************
** E_FUNCTION : ncl_geom_distf(nclkey1, nclkey2, typ, ptkey, val, err)
**       Calculate closest/farthest/near-point distances between 
**       a point vector and another entity based on the type of
**       distance specified.
**    PARAMETERS
**       INPUT  :
**          nclkey1 - point-vector key
**          nclkey2 - entity key (line,circle,curve,plane,surface,solid)
**          typ     - distance type: 0:closest
**                                   1:farthest
**                                   2:near-pt
**          ptkey   - key for near-point
**       OUTPUT :
**          val     - distances between entities.
**          err     - error flag
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_geom_tdistf(nclkey1, nclkey2, typ, ptkey, val, err)
int *nclkey1, *nclkey2,  *ptkey, *err, *typ;
UU_REAL *val;
{
	int stat = 0;
	struct NCL_fixed_databag Se[2];
	struct NCL_fixed_databag e1, e2, e3;
	struct UM_point_rec *nrpt;
	UM_coord pt[2];
	UM_vector vc[2];
	*err = 0;
	e1.key = *nclkey1;
	e2.key = *nclkey2;
	ncl_retrieve_data_fixed(&e1);
	ncl_retrieve_data_fixed(&e2);
	Se[0] = e1;
	Se[1] = e2;
	if (*typ > 1) e3.key = *ptkey;
	else e3.key = *nclkey1;
	ncl_retrieve_data_fixed(&e3);
	nrpt = (struct UM_point_rec *)&e3;
	stat = ncl_geom_tdistance(Se, typ, nrpt, val);
	*err = stat;
}

/*********************************************************************
**    E_FUNCTION     :  ncl_geom_tdistance(e, typ, nrpt, val)
**       Calculate closest/farthest/near-point distances between 
**       a point vector and another entity based on the type of
**       distance specified.
**    PARAMETERS
**       INPUT     :
**          e    - Fixed entity data. (e[0] should be a point-vector)
**          typ  -  0:closest
**                 1:farthest
**                 2:near-pt
**          nrpt - Near-point.
**    OUTPUT       :
**          val  - Distance
**    RETURNS      : Success or failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_geom_tdistance(e, typ, nrpt, val)
struct NCL_fixed_databag e[2];
UM_int2 *typ;
UU_REAL *val;
struct UM_point_rec *nrpt;
{
	int stat,npts,err,ntr,i,j,k,status,nint,onpv,ipt,ierr,type=*typ;
	struct NCL_nclpv_rec *pv;
	UM_int2 relnum;
	UM_transf tfmat1,tfmat2;
	UM_isect ibuff[UM_MAXISECT];
	UU_LIST isect;
	UU_REAL dist,sdist,u,v,side,ut,vt,
		dist2,sdist2,ptcheck,tol,tdist,sc,tdot,d1;
	UM_vector norm,tvec,nvec;
	UU_LOGICAL lnrpt,fardist,clsdist,first,repeat=UU_FALSE;
	UM_coord farpt,old,npt1,npt2,npt,fndpt,tpt,pt1,pt2,spt;
	NCL_xtriangle *tr;
	struct UM_evcrvout evcrv;
	int n1,isec,inc;
	NCL_xtriangle *xtp1;

	pv = (struct NCL_nclpv_rec *)&e[0];
	status = uc_retrieve_transf(e[0].key, tfmat1);
	if (status != UU_SUCCESS) return(UU_FAILURE);;
	status = uc_retrieve_transf(e[1].key, tfmat2);
	if (status != UU_SUCCESS) return(UU_FAILURE);;
	ncl_get_type(e[1].rel_num,&relnum);
	um_pv_to_line(&e[0],&e[0],UU_TRUE);
	if (type < 0) um_negvc(pv->ve,pv->ve);
/*
.....Set distance type flags.
*/
	clsdist = (fabs(type)==1);
	fardist = (fabs(type)==2);
	lnrpt = (type==3);
	sdist2 = sdist = (clsdist || lnrpt)? 1.e10 : 0.;
redo:
	if (relnum == NCLI_CIRCLE || relnum == NCLI_LINE || relnum == NCLI_CURVE)
	{
/*
.....Get all intersections
.......Checking the number of intersections prevents attempting to access invalid
.......data.  If the number of intersections is more than two when using a circle,
.......then the intersections are invalid.
.......Since the intersection point is found along the point vector only the dot
.......product is needed to check direction.
*/
		ipt = 2;
		status = um_isect_curves(&e[0],tfmat1,&e[1],tfmat2,&ipt,
											npt,&nint, UM_MAXISECT, ibuff);
		if ((relnum == NCLI_CIRCLE && nint > 2) ||
			(relnum == NCLI_LINE && nint > 1))
			return(UU_FAILURE);
		err=2;
		if (nint == 0) 
		{
			if (!repeat)
			{
				repeat = UU_TRUE;
				um_negvc(pv->ve,pv->ve);
				goto redo;
			}
			else
				return(UU_FAILURE);
		}
		j = 0;
		for (i = 0; i < nint; i++)
		{
/*
.....Get intersection point on curve based on u parameter
.......A u parameter not in the range [0,1] means the point is not
.......within the end points of the entity
*/
			if (ibuff[i].u1 < 0. || ibuff[i].u1 > 1.) continue;
			ierr = uc_evcrv(UM_POINT, ibuff[i].u1, &e[1], tfmat2, &evcrv);
			if (ierr != 0) continue;
			um_vctovc(evcrv.cp,ibuff[i].pt);
			um_vcmnvc(ibuff[i].pt,pv->pt,tvec);
			tdot = um_dot(tvec,pv->ve);
			if (tdot < 0.) continue;
			status = ncl_is_pt_on_curve(ibuff[i].pt, &e[1], tfmat2);
			if (status != UU_SUCCESS) continue;

			if (lnrpt) dist = um_dcccc(nrpt->pt,ibuff[i].pt);
			else dist = um_dcccc(pv->pt,ibuff[i].pt);

			if ((lnrpt || clsdist) && dist < sdist)
			{
				j = i;
				sdist = dist;
				err = 0;
			}	
			else if (fardist && dist > sdist)
			{
				j = i;
				sdist = dist;
				err=0;
			}
		}
		if (err != 0 && !repeat)
		{
			repeat = UU_TRUE;
			um_negvc(pv->ve,pv->ve);
			goto redo;
		}
/*
.....Distance sdist needs to be corrected if using near-point. 
.....When making comparisons, sdist stores the distance from the 
.....intersection point to the near-point.
*/
		if (lnrpt) sdist = um_dcccc(pv->pt,ibuff[j].pt);
	}
	else if (relnum == NCLI_PLANE)
	{
		stat = ncl_pt_proj_pl(pv->pt,e[1],1,pv->ve,norm,ibuff[0].pt);
		um_vcmnvc(ibuff[0].pt,pv->pt,tvec);
		tdot = um_dot(tvec,pv->ve);
		if ((tdot < 0. || stat != 0))
		{
			if (!repeat)
			{
				repeat = UU_TRUE;
				um_negvc(pv->ve,pv->ve);
				goto redo;
			}
			else
				return(UU_FAILURE);
		}
		sdist = um_dcccc(pv->pt,ibuff[0].pt);
		err = 0;
	}
/*
.....Find desired distance between point-vector and surface/solid.
*/
	else if (relnum == NCLI_SURF || relnum == NCLI_SOLID)
	{
		gettol(&tol);
		stat = UU_SUCCESS;
		err = 2;
		um_vcplvc(pv->pt,pv->ve,tpt);
		um_unitvc(pv->ve,nvec);
		um_vctovc(pv->pt,npt);
/*
.....Tessellate solid within tolerance
*/
		status = ncl_get_trilist(&e[1],tol,UM_TESS_TOLER,100,&xtp1,&n1);
		if (status != UU_SUCCESS) 
		{
			*val = 0.;
			return(UU_FAILURE);
		}
/*
.....Get intersection of point-vector and solid
*/
		first = UU_TRUE;
		for (i=0;i<n1;i++)
		{
			isec = 0;
			ncl_segtri_io(npt,tpt,nvec,&xtp1[i],pt1,pt2,norm,tol,&isec);
/*
........Determine desired intersection
*/
			if (isec == 1)
			{
				um_vcmnvc(pt1,pv->pt,tvec);
				if (um_dot(tvec,pv->ve) < 0.) continue;
				err = 0;
				if (!lnrpt) d1 = um_dcccc(pv->pt,pt1);
				else d1 = um_dcccc(nrpt->pt,pt1);
				if ((!fardist && d1 < sdist) || (fardist && d1 > sdist))
				{
					um_vctovc(pt1,spt);
					sdist = d1;
				}
			}
		}
		if (err != 0 && !repeat)
		{
			repeat = UU_TRUE;
			um_negvc(pv->ve,pv->ve);
			goto redo;
		}
		sdist = um_dcccc(spt,pv->pt);
	}
	else return(UU_FAILURE);
	if (stat == -1 || err != 0) return(UU_FAILURE);

	if (repeat) sdist = -sdist; 
	UM_len_inttoext(sdist, *val);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_geom_closest(pkpt,e,rn,pto,vco,flag,exten,
**                                       grid_flag,grid)
**       Find the closest points between two entities.
**    PARAMETERS
**       INPUT  :
**          pkpt      - [0] = Picked point on first entity, [1] = Second.
**          e         - [0] = First entity, [1] = Second.
**          rn        - [0] = Geometry type of first entity, [1] = Second.
**          vco       - [0] = Normal/tangent at picked point of entity.
**                      [1] = Second.
**          flag:     - 1: picked entity, need calculate pkpt,vco
**                      0: ignore pkpt and vco
**          exten     - UU_TRUE = Use surface extensions, UU_FALSE = Don't.
**          grid_flag - UU_TRUE = Use surface grid only for tessellation.
**                      UU_FALSE = Use tolerance and optional grid for
**                      tessellation.
**          grid      - Specifies the grid to use for surface tessellation.
**                      Can be set to 0 which means no grid.
**       OUTPUT :
**          pto    - [0] = Closest point on first entity, [1] = Second.
**          vco    - [0] = Normal/tangent at calculated point of entity.
**                   [1] = Second.
**    RETURNS      : -1 if could not calculate distance between entities.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_geom_closest(pkpt,e,rn,pto,vco,flag,exten,grid_flag,grid)
UM_coord pkpt[2];
struct NCL_fixed_databag e[2];
UM_int2 rn[2];
UM_coord pto[2];
UM_vector vco[2];
int flag;
UU_LOGICAL exten;
UU_LOGICAL grid_flag;
int grid;
{
	int status, i, j, k, nlns, n1, n2, iflg, unflg=1, ierr, ind;
	UM_int2 rn1, rn2;
	struct NCL_fixed_databag *eptr1, *eptr2;
	struct UM_point_rec *p1,*p2;
	struct UM_line_rec *l1, *l2;
	struct UM_circle_rec *c1;
	struct NCL_nclpl_rec *pl1, *pl2;
	struct NCL_nclpv_rec *pv1, *pv2;
	struct UM_evcrvout evcrv;
	struct UM_polyline_rec *poly1, *poly2;
	NCL_xtriangle *xt1, *xt2, *xtp1, *xtp2;
	UM_tess_settype tess_type = UM_TESS_BOTH;
	UM_coord pt1, pt2, spt1, spt2, ept1, ept2, npt1, npt2, ptmp1, ptmp2;
	UM_coord *pts1, *pts2;
	UM_coord ptx, polypts[2][2];
	UM_vector vc1,v0,vc2,nvec[2];
	UM_transf tfmat;
	UU_REAL dis1=0, dis2=0, u, du, v, dir, side, sfparm[2][3];
	UU_REAL d1, d2, d3, svd1, u2, ust, und, svu, pld, dstor, dtmp;
	UM_real8 tol, *rdis2, *rdisp2;
	UU_LIST list1, list2;
	UU_LOGICAL found = UU_TRUE, found2 = UU_TRUE, limit;
	UM_int2 iwf = 0,i0=0,i2=2;
	UM_real4 u4,v4,asw;
/*
.....Initialize routine
*/
	if (UL_measurement_disttol < UM_DFUZZ)
	{
		gettol(&tol);
		UL_measurement_disttol = tol * 10.;
	}
	tol = UL_measurement_disttol;
	eptr1 = &e[0];
	eptr2 = &e[1];
	rn1 = rn[0];
	rn2 = rn[1];
	ind = 0;
	if (rn1 != UM_POLYLINE_REL) ind = 1;
	um_vctovc(vco[0],nvec[0]);
	um_vctovc(vco[1],nvec[1]);
/*
.....Set required order of entities for comparison
*/
/*
.....Don't treat PV as LN, but
.....using the original points
*/
/*
	if (rn1 == NCLI_POINTVEC) rn1 = NCLI_LINE;
	if (rn2 == NCLI_POINTVEC) rn2 = NCLI_LINE;
*/
	if ((rn1==NCLI_LINE && rn2==NCLI_POINT) ||
		(rn1==NCLI_PLANE && (rn2==NCLI_POINT || rn2==NCLI_LINE)) ||
		(rn1==NCLI_CIRCLE &&
		(rn2==NCLI_POINT || rn2==NCLI_LINE || rn2==NCLI_PLANE)) ||
		((rn1==NCLI_CURVE || rn1==UM_POLYLINE_REL) && rn2!=NCLI_CURVE && rn2!=NCLI_SURF
		&& rn2 != UM_POLYLINE_REL) ||
		(rn1==NCLI_SURF && rn2!=NCLI_SURF) ||
		(rn1==NCLI_SOLID && rn2!=NCLI_SOLID))
	{
		eptr2 = &e[0];
		eptr1 = &e[1];
		rn2 = rn[0];
		rn1 = rn[1];
		um_vctovc(vco[0],nvec[1]);
		um_vctovc(vco[1],nvec[0]);
	}

	sfparm[0][0] = sfparm[1][0] = .5;
	sfparm[0][1] = sfparm[1][1] = .5;
	sfparm[0][2] = sfparm[1][2] = 0.;
/*
.....Calculate closest points
*/
	switch (rn1)
	{
	case NCLI_POINT:
	case NCLI_POINTVEC:
		if (rn1==NCLI_POINT)
		{
			p1 = (struct UM_point_rec *)eptr1;
			um_vctovc(p1->pt,pt1);
		}
		else
		{
			pv1 = (struct NCL_nclpv_rec *)eptr1;
			um_vctovc(pv1->pt,pt1);
		}
		switch (rn2)
		{
/*
.....Point-point
*/
		case NCLI_POINT:
			p2 = (struct UM_point_rec *)eptr2;
			um_vctovc(p2->pt,pt2);
			break;
/*
.....Point-line/point vector
*/
		case NCLI_LINE:
			l1 = (struct UM_line_rec *)eptr2;
			um_vctovc(l1->spt,spt1);
			um_vctovc(l1->ept,ept1);
			um_vcmnvc(ept1,spt1,vc1);
			um_unitvc(vc1,vc2);
			um_nptln(pt1,spt1,vc2,pt2);
			d1 = UM_DOT(pt1,vc2);
			d2 = UM_DOT(spt1,vc2);
			d3 = UM_DOT(ept1,vc2);
			if (d1 < d2)
				um_vctovc(spt1,pt2);
			else if (d1 > d3)
				um_vctovc(ept1,pt2);
			break;
/*
.....Point-Polyline
*/
		case UM_POLYLINE_REL:
			poly2 = (struct UM_polyline_rec *)eptr2;
			pts2 = (UM_coord *)poly2->pt;
			dstor = 10000.;
			for (i=0;i<poly2->no_pt-1;i++)
			{
				um_vctovc(pts2[i],spt1);
				um_vctovc(pts2[i+1],ept1);
				um_vcmnvc(ept1,spt1,vc1);
				um_unitvc(vc1,vc2);
				um_nptln(pt1,spt1,vc2,pt2);
				d1 = UM_DOT(pt1,vc2);
				d2 = UM_DOT(spt1,vc2);
				d3 = UM_DOT(ept1,vc2);
				if (d1 < d2) um_vctovc(spt1,pt2);
				else if (d1 > d3) um_vctovc(ept1,pt2);
				dtmp = um_dcccc(pt1,pt2);
				if (dtmp < dstor)
				{
					dstor = dtmp;
					um_vctovc(spt1,polypts[ind][0]);
					um_vctovc(ept1,polypts[ind][1]);
					um_vctovc(pt2,ptmp2);
				}
			}
			if (dstor < 10000.) um_vctovc(ptmp2,pt2);
			break;
		case NCLI_POINTVEC:
			pv1 = (struct NCL_nclpv_rec *)eptr2;
			um_vctovc(pv1->pt,pt2);
			break;
/*
.....Point-plane
*/
		case NCLI_PLANE:
			pl1 = (struct NCL_nclpl_rec *)eptr2;
			um_nptpln(pt1,pl1->pt,pl1->nvec,pt2);
			break;
/*
.....Point-circle
*/
		case NCLI_CIRCLE:
			c1 = (struct UM_circle_rec *)eptr2;
/*
.....Project point onto circle plane, intersect circle with line from point
.....to circle center.
*/
			um_nptpln(pt1,c1->center,c1->nvec,pt2);
			um_vcmnvc(pt2,c1->center,vc1);
			um_unitvc(vc1,vc1);
			um_vctmsc(vc1,c1->radius,vc1);
			um_vcplvc(c1->center,vc1,pt2);
/*
.....If circle is trimmed and projecting point is outside trimmed part, use
.....circle start or end point as nearest point. Create a vector in the
.....circle plane perpto the start vector pointing towards the real part
.....of the circle. If the dot product of this vector with the projected
.....point is less than the dot product of this vector with the circle
.....start point, projected point is outside circle  segment.
.....Same for circle end point.
*/
			if (fabs(UM_TWOPI - fabs(c1->dang)) > UM_FUZZ)
			{
				um_circpts(c1,spt1,ept1);
				um_cross(c1->nvec,c1->svec,vc1);
				dis1 = UM_DOT(vc1,pt1) - UM_DOT(vc1,spt1);
				if (dis1 > 0.0)
				{
					um_vcmnvc(ept1,c1->center,vc1);
					um_unitvc(vc1,vc2);
					um_cross(vc2,c1->nvec,vc1);
					dis1 = UM_DOT(vc1,pt1) - UM_DOT(vc1,ept1);
				}
				if (dis1 < 0.0)
				{
					dis1 = um_dcccc(spt1,pt1);
					dis2 = um_dcccc(ept1,pt1);
					if (dis1 <= dis2)
						um_vctovc(spt1,pt2);
					else
						um_vctovc(ept1,pt2);
				}
			}
			break;
/*
.....Point-curve
*/
		case NCLI_CURVE:
			iflg = 20;
			u = .5;

			ncl_wcstomcs(0,pt1,ptx);
			cvpv2(&(eptr2->key), &iwf, ptx, &iflg, &u, &unflg, pt2, vc1, &ierr);
			ncl_mcstowcs(0,pt2,pt2);
			ncl_mcstowcs(1,vc1,vc1);

			if (ierr > 0) found = UU_FALSE;
			break;
/*
.....Point-surf
*/
		case NCLI_SURF:
			asw = 0.;
			if (exten)
			{
				getsuv(&asw,&i2,&u4,&v4);
				side = 0;
				um_nullvc (v0);
				status = ncl_pt_proj_sf(pt1,eptr2,&u4,&v4,&side,i0,v0,0.,pt2,vc1);
				if (status != UU_SUCCESS) found = UU_FALSE;
			}
			else
			{
				du = 10000.;
				for (i=0;i<=10;i++)
				{
					for (j=0;j<=10;j++)
					{
						dir = 0.;
						u = (UU_REAL)i/10.;
						v = (UU_REAL)j/10.;
						sfpt2(&(eptr2->key),pt1,&u,&v,&unflg,&dir,npt1,vc1,&ierr);
						d1 = um_dcccc(pt1,npt1);
						if (d1 < du && ierr <= 0)
						{
							um_vctovc(npt1,pt2);
							du = d1;
							sfparm[1][0] = u;
							sfparm[1][1] = v;
							sfparm[1][2] = dir;
						}
					}
				}
				if (ierr > 0) found = UU_FALSE;
			}
			break;
/*
.....Point-solid
*/
		case NCLI_SOLID:
			status = ncl_get_trilist(eptr2,tol,UM_TESS_TOLER,100,&xt1,&n1);
			if (status != UU_SUCCESS)
			{
				found = UU_FALSE;
				break;
			}
			du = 10000.;
			for (i=0;i<n1;i++)
			{
				if (!xt1->valid) continue;
				d1 = ncl_pttridis(pt1,&xt1[i],npt1);
				if (d1 < du)
				{
					du = d1;
					um_vctovc(npt1,pt2);
					um_vctovc(xt1[i].pln,nvec[1]);
				}
			}
			break;
		default:
			found = UU_FALSE;
			break;
		}
		break;
	case NCLI_LINE:
		l1 = (struct UM_line_rec *)eptr1;
		um_vctovc(l1->spt,spt1);
		um_vctovc(l1->ept,ept1);
		um_vcmnvc(l1->ept,l1->spt,vc1);
		switch (rn2)
		{
/*
.....line/pntvec-line/pntvec
*/
		case NCLI_LINE:
			l2 = (struct UM_line_rec *)eptr2;
			um_vctovc(l2->spt,spt2);
			um_vctovc(l2->ept,ept2);
			um_vcmnvc(ept2,spt2,vc2);
			d1 = ncl_lnlndis(spt1,ept1,spt2,ept2,pt1,pt2);
			break;
		case NCLI_POINTVEC:
			pv2 = (struct NCL_nclpv_rec *)eptr2;
			um_vctovc(pv2->pt,pt2);
			break;
/*
.....line/pntvec-plane
*/
		case NCLI_PLANE:
			pl1 = (struct NCL_nclpl_rec *)eptr2;
			pld = UM_DOT(pl1->pt,pl1->nvec);
			d1 = UM_DOT(spt1,pl1->nvec) - pld;
			d2 = UM_DOT(ept1,pl1->nvec) - pld;
			if (d1*d2 < 0.0)
			{
				um_unitvc(vc1,vc1);
				um_ilnpln(spt1,vc1,pl1->pt,pl1->nvec,&n1,pt1);
				if (n1 == 0) um_vctovc(pl1->pt,pt1);
				um_vctovc(pt1,pt2);
			}
			else
			{
				if (fabs(d1) < fabs(d2))
					um_vctovc(spt1,pt1);
				else
					um_vctovc(ept1,pt1);
				um_nptpln(pt1,pl1->pt,pl1->nvec,pt2);
			}
			break;
/*
.....line/pntvec-circle/curve/polyline
*/
		case UM_POLYLINE_REL:
		case NCLI_CIRCLE:
		case NCLI_CURVE:
			if (rn2 != UM_POLYLINE_REL)
			{
				uu_list_init(&list2,sizeof(UM_coord),100,800);
				if (uc_retrieve_transf(eptr2->key,tfmat) != UU_SUCCESS)
					found = UU_FALSE;
				if (found)
				{
					n2 = ncl_evolve_all_curves(eptr2,tfmat,tol,&list2,UU_NULL,UU_FALSE);
					if (n2 == 0) found = UU_FALSE;
				}
				pts2 = (UM_coord *)UU_LIST_ARRAY(&list2);
			}
			else
			{
				poly2 = (struct UM_polyline_rec *)eptr2;
				pts2 = (UM_coord *)poly2->pt;
				n2 = poly2->no_pt;
				if (n2 > 0) found = UU_TRUE;
				else found = UU_FALSE;
			}
			if (found)
			{
				svd1 = 1.e12;
				for (i=0;i<n2-1 && svd1>0.0;i++)
				{
					um_vcmnvc(pts2[i+1],pts2[i],vc2);
					um_unitvc(vc2,vc2);
					d1 = ncl_lnlndis(spt1,ept1,pts2[i],pts2[i+1],npt1,npt2);
					if (d1 < svd1)
					{
						svd1 = d1;
						um_vctovc(npt1,pt1);
						um_vctovc(npt2,pt2);
						um_vctovc(pts2[i],polypts[ind][0]);
						um_vctovc(pts2[i+1],polypts[ind][1]);
					}
				}
			}
			if (rn2 != UM_POLYLINE_REL) uu_list_free(&list2);
			break;
/*
.....line/pntvec-surf
*/
		case NCLI_SURF:
			found = UU_FALSE;
			asw = 0.;
			getsuv(&asw,&i2,&u4,&v4);
			u = u4;
			v = v4;
			dir = 0.;
			svd1 = 1.e12;
			u2 = 0;
			du = 1./9.0;
			for (i=0;i<10 && svd1>0.;i++)
			{
				for (j=0;j<10 && svd1>0.;j++)
				{
					um_vctmsc(vc1,u2,vc2);
					um_vcplvc(spt1,vc2,npt1);
					sfpt2(&(eptr2->key),npt1,&u,&v,&unflg,&dir,npt2,vc2,&ierr);
					if (ierr == 0)
					{
						found = UU_TRUE;
						d1 = um_dcccc(npt1,npt2);
						if (d1 < svd1)
						{
							svd1 = d1;
							svu = u2;
							um_vctovc(npt1,pt1);
							um_vctovc(npt2,pt2);
							sfparm[1][0] = u;
							sfparm[1][1] = v;
							sfparm[1][2] = dir;
						}
					}
					u2 += du;
					if (u2 > 1.0) u2 = 1.0;
				}
				if (found)
				{
					ust = svu - du;
					if (ust<0.0) ust = 0.0;
					und = svu + du;
					if (und > 1.0) und = 1.0;
					u2 = ust;
					du = (und - ust)/9.;
					if (du <= 0.) svd1 = 0.;
				}
			}
			break;
/*
.....line-solid
*/
		case NCLI_SOLID:
			status = ncl_get_trilist(eptr2,tol,UM_TESS_TOLER,100,&xt1,&n1);
			if (status != UU_SUCCESS)
			{
				found = UU_FALSE;
				break;
			}
			du = 10000.;
			um_unitvc(vc1,vc1);
			for (i=0;i<n1;i++)
			{
				if (!xt1[i].valid) continue;

				d1 = ncl_lnlndis(spt1,ept1,xt1[i].p1,xt1[i].p2,npt1,npt2);
				if (d1 < du)
				{
					du = d1;
					um_vctovc(npt1,pt1);
					um_vctovc(npt2,pt2);
					um_vctovc(xt1[i].pln,nvec[1]);
				}

				d1 = ncl_lnlndis(spt1,ept1,xt1[i].p2,xt1[i].p3,npt1,npt2);
				if (d1 < du)
				{
					du = d1;
					um_vctovc(npt1,pt1);
					um_vctovc(npt2,pt2);
					um_vctovc(xt1[i].pln,nvec[1]);
				}

				d1 = ncl_lnlndis(spt1,ept1,xt1[i].p3,xt1[i].p1,npt1,npt2);
				if (d1 < du)
				{
					du = d1;
					um_vctovc(npt1,pt1);
					um_vctovc(npt2,pt2);
					um_vctovc(xt1[i].pln,nvec[1]);
				}

				d1 = du;
				ncl_segtridis(spt1,ept1,vc1,&xt1[i],pt1,pt2,&du);
				if (du < d1) um_vctovc(xt1[i].pln,nvec[1]);
			}
			break;
		default:
			found = UU_FALSE;
			break;
		}
		break;
	case NCLI_PLANE:
		pl1 = (struct NCL_nclpl_rec *)eptr1;
		switch (rn2)
		{
/*
.....plane-plane
*/
		case NCLI_PLANE:
			pl2 = (struct NCL_nclpl_rec *)eptr2;
			um_vctovc(pl1->pt,pt1);
			if (fabs(um_dot(pl1->nvec,pl2->nvec)) > cos(.05/UM_RADIAN))
				um_nptpln(pt1,pl2->pt,pl2->nvec,pt2);
			else
				found = UU_FALSE;
			break;
/*
.....plane-circle/curve/polyline
*/
		case UM_POLYLINE_REL:
		case NCLI_CIRCLE:
		case NCLI_CURVE:
			if (rn2 != UM_POLYLINE_REL)
			{
				uu_list_init(&list2,sizeof(UM_coord),100,800);
				if (uc_retrieve_transf(eptr2->key,tfmat) != UU_SUCCESS)
					found = UU_FALSE;
				if (found)
				{
					n2 = ncl_evolve_all_curves(eptr2,tfmat,tol,&list2,UU_NULL,UU_FALSE);
					if (n2 == 0) found = UU_FALSE;
				}
				pts2 = (UM_coord *)UU_LIST_ARRAY(&list2);
			}
			else
			{
				poly2 = (struct UM_polyline_rec *)eptr2;
				pts2 = (UM_coord *)poly2->pt;
				n2 = poly2->no_pt;
				if (n2 > 0) found = UU_TRUE;
				else found = UU_FALSE;
			}
			if (found)
			{
				svd1 = 1.e12;
				d1 = um_dot(pl1->pt,pl1->nvec);
				for (i=0;i<n2-1 && svd1>0.0;i++)
				{
					dis1 = um_dot(pts2[i],pl1->nvec) - d1;
					dis2 = um_dot(pts2[i+1],pl1->nvec) - d1;
					if (dis1*dis2 < 0.0)
					{
						um_vcmnvc(pts2[i+1],pts2[i],vc2);
						um_unitvc(vc2,vc2);
						um_ilnpln(pts2[i],vc2,pl1->pt,pl1->nvec,&n1,npt1);
						if (n1 == 0) um_vctovc(pl1->pt,npt1);
						um_vctovc(npt1,npt2);
					}
					else
					{
						if (fabs(dis1) < fabs(dis2))
							um_vctovc(pts2[i],npt1);
						else
							um_vctovc(pts2[i+1],npt1);
						um_nptpln(npt1,pl1->pt,pl1->nvec,npt2);
					}
					d2 = um_dcccc(npt1,npt2);
					if (d2 < svd1)
					{
						svd1 = d2;
						um_vctovc(npt1,pt1);
						um_vctovc(npt2,pt2);
						um_vctovc(pts2[i],polypts[ind][0]);
						um_vctovc(pts2[i+1],polypts[ind][1]);
					}
				}
			}
/*
.....this operation was called without initialising list1
.....uu_list_free(&list1);
*/
			if (rn2 != UM_POLYLINE_REL) uu_list_free(&list2);
			break;
/*
.....plane-surf/solid
*/
		case NCLI_SURF:
		case NCLI_SOLID:
			found = UU_FALSE;
			status = ncl_get_trilist(eptr2,tol,UM_TESS_TOLER,100,&xt1,&n2);
			if (status == UU_SUCCESS)
			{
				found = UU_TRUE;
				pld = um_dot(pl1->pt,pl1->nvec);
				svd1 = 1.e12;
				for (i=0;i<n2 && svd1>0.0;i++)
				{
					d1 = um_dot(xt1[i].p1,pl1->nvec) - pld;
					d2 = um_dot(xt1[i].p2,pl1->nvec) - pld;
					d3 = um_dot(xt1[i].p3,pl1->nvec) - pld;
					if (d1*d2 < 0.0)
					{
						svd1 = 0.0;
						um_vcmnvc(xt1[i].p2,xt1[i].p1,vc1);
						um_unitvc(vc1,vc1);
						um_ilnpln(xt1[i].p1,vc1,pl1->pt,pl1->nvec,&n1,pt1);
						if (n1 == 0) um_vctovc(pl1->pt,pt1);
						um_vctovc(pt1,pt2);
						um_vctovc(xt1[i].pln,nvec[1]);
					}
					else if (d2*d3 < 0.0)
					{
						svd1 = 0.0;
						um_vcmnvc(xt1[i].p3,xt1[i].p2,vc1);
						um_unitvc(vc1,vc1);
						um_ilnpln(xt1[i].p2,vc1,pl1->pt,pl1->nvec,&n1,pt1);
						if (n1 == 0) um_vctovc(pl1->pt,pt1);
						um_vctovc(pt1,pt2);
						um_vctovc(xt1[i].pln,nvec[1]);
					}
					else if (d3*d1 < 0.0)
					{
						svd1 = 0.0;
						um_vcmnvc(xt1[i].p1,xt1[i].p3,vc1);
						um_unitvc(vc1,vc1);
						um_ilnpln(xt1[i].p3,vc1,pl1->pt,pl1->nvec,&n1,pt1);
						if (n1 == 0) um_vctovc(pl1->pt,pt1);
						um_vctovc(pt1,pt2);
						um_vctovc(xt1[i].pln,nvec[1]);
					}
					else
					{
						d1 = fabs(d1);
						if (d1 < svd1)
						{
							svd1 = d1;
							um_vctovc(xt1[i].p1,pt2);
							um_nptpln(pt2,pl1->pt,pl1->nvec,pt1);
							um_vctovc(xt1[i].pln,nvec[1]);
						}
						d2 = fabs(d2);
						if (d1 < svd1)
						{
							svd1 = d2;
							um_vctovc(xt1[i].p2,pt2);
							um_nptpln(pt2,pl1->pt,pl1->nvec,pt1);
							um_vctovc(xt1[i].pln,nvec[1]);
						}
						d3 = fabs(d3);
						if (d3 < svd1)
						{
							svd1 = d3;
							um_vctovc(xt1[i].p3,pt2);
							um_nptpln(pt2,pl1->pt,pl1->nvec,pt1);
							um_vctovc(xt1[i].pln,nvec[1]);
						}
					}
				}
			}
			sfparm[1][0] = .5;
			sfparm[1][1] = .5;
			sfparm[1][2] = 0.;
			break;
		default:
			found = UU_FALSE;
			break;
		}
		break;
	case UM_POLYLINE_REL:
	case NCLI_CIRCLE:
	case NCLI_CURVE:
		switch (rn2)
		{
/*
.....circle/curve/polyline-circle/curve/polyline
*/
		case UM_POLYLINE_REL:
		case NCLI_CIRCLE:
		case NCLI_CURVE:
			if (rn1 != UM_POLYLINE_REL)
			{
				uu_list_init(&list1,sizeof(UM_coord),100,800);
				if (uc_retrieve_transf(eptr1->key,tfmat) != UU_SUCCESS)
					found = UU_FALSE;
				if (found)
				{
					n1 = ncl_evolve_all_curves(eptr1,tfmat,tol,&list1,UU_NULL,UU_FALSE);
					if (n1 == 0) found = UU_FALSE;
				}
				if (found)
					pts1 = (UM_coord *)UU_LIST_ARRAY(&list1);
			}
			else
			{
				poly1 = (struct UM_polyline_rec *)eptr1;
				pts1 = (UM_coord *)poly1->pt;
				n1 = poly1->no_pt;
				if (n1 > 0) found = UU_TRUE;
				else found = UU_FALSE;
			}
			if (rn2 != UM_POLYLINE_REL)
			{
				uu_list_init(&list2,sizeof(UM_coord),100,800);
				if (uc_retrieve_transf(eptr2->key,tfmat) != UU_SUCCESS)
					found2 = UU_FALSE;
				if (found2)
				{
					n2 = ncl_evolve_all_curves(eptr2,tfmat,tol,&list2,UU_NULL,UU_FALSE);
					if (n2 == 0) found = UU_FALSE;
				}
				if (found2)
					pts2 = (UM_coord *)UU_LIST_ARRAY(&list2);
			}
			else
			{
				poly2 = (struct UM_polyline_rec *)eptr2;
				pts2 = (UM_coord *)poly2->pt;
				n2 = poly2->no_pt;
				if (n2 > 0) found2 = UU_TRUE;
				else found2 = UU_FALSE;
			}
			if (found && found2)
			{
				svd1 = 1.e12;
				for (i=0;i<n1-1 && svd1>0.0;i++)
				{
					for (j=0;j<n2-1 && svd1>0.0;j++)
					{
						d1 = ncl_lnlndis(pts1[i],pts1[i+1],pts2[j],pts2[j+1],
							npt1,npt2);
						if (d1 < svd1)
						{
							svd1 = d1;
							um_vctovc(npt1,pt1);
							um_vctovc(npt2,pt2);
							um_vctovc(pts1[i],polypts[0][0]);
							um_vctovc(pts1[i+1],polypts[0][1]);
							um_vctovc(pts2[i],polypts[1][0]);
							um_vctovc(pts2[i+1],polypts[1][1]);
						}
					}
				}
			}
			if (rn1 != UM_POLYLINE_REL) uu_list_free(&list1);
			if (rn2 != UM_POLYLINE_REL) uu_list_free(&list2);
			break;
/*
.....circle/curve/polyline-surf
*/
		case NCLI_SURF:
			if (rn1 != UM_POLYLINE_REL) 
			{
				if (uc_retrieve_transf(eptr1->key,tfmat) != UU_SUCCESS)
					break;
				if (uc_init_evcrvout(eptr1, &evcrv) != UU_SUCCESS) break;
				nlns = 1;
			}
			else
			{
				poly1 = (struct UM_polyline_rec *)eptr1;
				pts1 = (UM_coord *)poly1->pt;
				nlns = poly1->no_pt-1;
				if (nlns <= 0) break;
			}
			dstor = 10000.;
			svd1 = 1.e12;
			for (k=0;k<nlns && svd1>UM_DFUZZ;k++)
			{
				found = UU_FALSE;
				asw = 0.;
				getsuv(&asw,&i2,&u4,&v4);
				u = u4;
				v = v4;
				dir = 0.;
				u2 = 0;
				du = 1./9.0;
				if (rn1 == UM_POLYLINE_REL)
				{
					um_vctovc(pts1[k],spt1);
					um_vctovc(pts1[k+1],ept1);
					um_vcmnvc(pts1[k],pts1[k+1],vc1);
				}
				for (i=0;i<10 && svd1>0.;i++)
				{
					for (j=0;j<10 && svd1>0.;j++)
					{
						if (rn1 != UM_POLYLINE_REL)
						{
							ierr = uc_evcrv(UM_POINT, u2, eptr1, tfmat, &evcrv);
							if (ierr == 0)
							{
								um_vctovc(evcrv.cp,npt1);
								sfpt2(&(eptr2->key),npt1,&u,&v,&unflg,&dir,npt2,vc2,&ierr);
							}
						}
						else
						{
							um_vctmsc(vc1,u2,vc2);
							um_vcplvc(spt1,vc2,npt1);
							sfpt2(&(eptr2->key),npt1,&u,&v,&unflg,&dir,npt2,vc2,&ierr);
						}
						if (ierr == 0)
						{
							found = UU_TRUE;
							d1 = um_dcccc(npt1,npt2);
							if (d1 < svd1)
							{
								svd1 = d1;
								svu = u2;
								um_vctovc(npt1,pt1);
								um_vctovc(npt2,pt2);
								um_vctovc(spt1,polypts[ind][0]);
								um_vctovc(ept1,polypts[ind][1]);
								sfparm[1][0] = u;
								sfparm[1][1] = v;
								sfparm[1][2] = dir;
							}
						}
						u2 += du;
						if (u2 > 1.0) u2 = 1.0;
					}
					if (found)
					{
						ust = svu - du;
						if (ust<0.0) ust = 0.0;
						und = svu + du;
						if (und > 1.0) und = 1.0;
						u2 = ust;
						du = (und - ust)/9.;
						if (du <= 0.) svd1 = 0.;
					}
				}
				if (svd1 < dstor)
				{
					dstor = svd1;
					um_vctovc(spt1,polypts[ind][0]);
					um_vctovc(ept1,polypts[ind][1]);
					um_vctovc(pt1,ptmp1);
					um_vctovc(pt2,ptmp2);
				}
			}
			if (dstor < 10000.) 
			{
				um_vctovc(ptmp1,pt1);
				um_vctovc(ptmp2,pt2);
			}
			break;
/*
.....circle/curve/polyline-solid
*/
		case NCLI_SOLID:
			if (rn1 != UM_POLYLINE_REL)
			{
				uu_list_init(&list1,sizeof(UM_coord),100,800);
				if (uc_retrieve_transf(eptr1->key,tfmat) != UU_SUCCESS)
					found = UU_FALSE;
				if (found)
				{
					n1 = ncl_evolve_all_curves(eptr1,tfmat,tol,&list1,UU_NULL,UU_FALSE);
					if (n1 == 0) found = UU_FALSE;
				}
				if (found == UU_FALSE) break;
				pts1 = (UM_coord *)UU_LIST_ARRAY(&list1);
				j = 0;
			}
			else
			{
				poly1 = (struct UM_polyline_rec *)eptr1;
				pts1 = (UM_coord *)poly1->pt;
				n1 = poly1->no_pt;
				if (n1 <= 0) 
				{
					found = UU_FALSE;
					break;
				}
				j = 1;
			}
			status = ncl_get_trilist(eptr2,tol,UM_TESS_TOLER,100,&xt1,&n2);
			if (status != UU_SUCCESS)
			{
				found = UU_FALSE;
				break;
			}

			du = 10000.;
			for (j;j<n1;j++)
			{
				um_vctovc(pts1[j-1],spt1); um_vctovc(pts1[j],ept1);
				um_vcmnvc(ept1,spt1,vc1); um_unitvc(vc1,vc1);
				for (i=0;i<n2;i++)
				{
					if (!xt1[i].valid) continue;

					d1 = ncl_lnlndis(spt1,ept1,xt1[i].p1,xt1[i].p2,npt1,npt2);
					if (d1 < du)
					{
						du = d1;
						um_vctovc(npt1,pt1);
						um_vctovc(npt2,pt2);
						um_vctovc(spt1,polypts[ind][0]);
						um_vctovc(ept1,polypts[ind][1]);
						um_vctovc(xt1[i].pln,nvec[1]);
					}

					d1 = ncl_lnlndis(spt1,ept1,xt1[i].p2,xt1[i].p3,npt1,npt2);
					if (d1 < du)
					{
						du = d1;
						um_vctovc(npt1,pt1);
						um_vctovc(npt2,pt2);
						um_vctovc(spt1,polypts[ind][0]);
						um_vctovc(ept1,polypts[ind][1]);
						um_vctovc(xt1[i].pln,nvec[1]);
					}

					d1 = ncl_lnlndis(spt1,ept1,xt1[i].p3,xt1[i].p1,npt1,npt2);
					if (d1 < du)
					{
						du = d1;
						um_vctovc(npt1,pt1);
						um_vctovc(npt2,pt2);
						um_vctovc(spt1,polypts[ind][0]);
						um_vctovc(ept1,polypts[ind][1]);
						um_vctovc(xt1[i].pln,nvec[1]);
					}

					d1 = du;
					ncl_segtridis(spt1,ept1,vc1,&xt1[i],pt1,pt2,&du);
					if (du < d1) um_vctovc(xt1[i].pln,nvec[1]);
				}
			}
			if (rn1 != UM_POLYLINE_REL) uu_list_free(&list1);
			break;
		default:
			found = UU_FALSE;
			break;
		}
		break;
	case NCLI_SURF:
	case NCLI_SOLID:
/*
.....surf/solid-surf/solid
*/
		found = UU_FALSE;
		tess_type = UM_TESS_TOLER;
		if (grid > 0)
		{
			if (grid_flag)
				tess_type = UM_TESS_GRID;
			else
				tess_type = UM_TESS_BOTH;
		}

		status = ncl_get_trilist(eptr1,tol,tess_type,grid,&xt1,&n1);
		if (status == UU_SUCCESS)
			status = ncl_get_trilist(eptr2,tol,tess_type,grid,&xt2,&n2);

		if (status == UU_SUCCESS)
		{
			if (flag)
			{
				limit = UL_measurement_range > 0.0;
			}
			else
				limit = UU_FALSE;
			rdis2 = UU_NULL;
			if (limit)
			{
				rdisp2 = rdis2 = (UU_REAL *)uu_malloc(n2*sizeof(UU_REAL));
				if (!rdis2) limit = UU_FALSE;
			}
			if (limit)
			{
				xtp2 = xt2;
				for (j=0;j<n2;j++)
				{
					*rdisp2 = ncl_pttridis(pkpt[1], xtp2, npt1);
					rdisp2++;
					xtp2++;
				}
			}
			svd1 = 1.e12;
			xtp1 = xt1;
			for (i=0;i<n1 && svd1>0.0;i++)
			{
				d1 = -1.0;
				if (limit)
				{
					d1 = ncl_pttridis(pkpt[0], xtp1, npt1);
				}
				if (xtp1->valid && d1 < UL_measurement_range)
				{
					rdisp2 = rdis2;
					xtp2 = xt2;
					for (j=0;j<n2 && svd1>0.0;j++)
					{
						if (xtp2->valid && (!limit || *rdisp2 < UL_measurement_range))
						{
							found = UU_TRUE;
							d1 = ncl_tritridis(xtp1,xtp2,npt1,npt2);
							if (d1 < svd1)
							{
								svd1 = d1;
								um_vctovc(npt1,pt1);
								um_vctovc(npt2,pt2);
								um_vctovc(xtp1->pln,nvec[0]);
								um_vctovc(xtp2->pln,nvec[1]);
							}
						}
						xtp2++;
						if (limit) rdisp2++;
					}
				}
				xtp1++;
			}
			if (rdis2) uu_free(rdis2);
		}
		sfparm[0][0] = sfparm[1][0] = .5;
		sfparm[0][1] = sfparm[1][1] = .5;
		sfparm[0][2] = sfparm[1][2] = 0.;
		break;
	default:
		found = UU_FALSE;
		break;
	}
/*
.....Return correct projection points
*/
	if (found == UU_FALSE)
		return -1;
	if (eptr1 == &e[0])
	{
		um_vctovc(pt1,pto[0]);
		um_vctovc(pt2,pto[1]);
		um_vctovc(nvec[0],vco[0]);
		um_vctovc(nvec[1],vco[1]);
	}
	else
	{
		um_vctovc(pt2,pto[0]);
		um_vctovc(pt1,pto[1]);
		um_vctovc(nvec[1],vco[0]);
		um_vctovc(nvec[0],vco[1]);
		if (rn[0] == NCLI_SURF || rn[1] == NCLI_SURF)
		{
			um_swapvc (sfparm[0],sfparm[1]);
		}
	}
/*
.....Get normal/tangent vector for
.....Circle, Curve, and Surface
*/
	for (i=0;i<2;i++)
	{
		if (rn[i] == NCLI_CIRCLE)
		{
			c1 = (struct UM_circle_rec *)&e[i];
			um_vcmnvc(pto[i],c1->center,vc1);
			um_unitvc(vc1,vc1);
			um_cross(c1->nvec,vc1,vco[i]);
		}
		else if (rn[i] == NCLI_CURVE)
		{
			iflg = 20;
			u = .5;

			ncl_wcstomcs(0,pto[i],ptx);
			cvpv2(&(e[i].key),&iwf,ptx,&iflg,&u,&unflg,pto[i],vco[i],&ierr);
			ncl_mcstowcs(0,pto[i],pto[i]);
			ncl_mcstowcs(1,vco[i],vco[i]);

		}
		else if (rn[i] == NCLI_SURF)
		{
			sfpt2(&(e[i].key),pto[i],&sfparm[i][0],&sfparm[i][1],&unflg,
				&sfparm[i][2],pt1,vco[i],&ierr);
		}
		else if (rn[i] == NCLI_LINE)
		{
			l1 = (struct UM_line_rec *)&e[i];
			um_vcmnvc(l1->ept,l1->spt,vc1);
			um_vctovc(vc1,vco[i]);
		}
		else if (rn[i] == UM_POLYLINE_REL)
		{
			um_vcmnvc(polypts[i][1],polypts[i][0],vc1);
			um_vctovc(vc1,vco[i]);
		}
	}
	return 0;
}

/*********************************************************************
**    E_FUNCTION     :  UU_REAL ncl_pttridis(pt1,xt1,pto)
**       Calculate the minimum distance between a point and a triangle.
**    PARAMETERS
**       INPUT  :
**          pt1       - Point
**          xt1       - Triangle
**       OUTPUT :
**          pto       - Point on triangle.
**    RETURNS      : distance
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL ncl_pttridis(pt1,xt1,pto)
UM_coord pt1;
NCL_xtriangle *xt1;
UM_coord pto;
{
	UU_REAL d1, d2, d3, dmin;
	UM_coord pt2;

	d1 = um_dot (pt1,xt1->pl1) - xt1->pl1[3];
	d2 = um_dot (pt1,xt1->pl2) - xt1->pl2[3];
	d3 = um_dot (pt1,xt1->pl3) - xt1->pl3[3];
	if (d1>=0.0 && d2>=0.0 && d3>=0.0)
	{
		dmin = fabs (um_dot (pt1,xt1->pln) - xt1->pln[3]);
		um_nptpln(pt1,xt1->p1,xt1->pln,pto);
		return (dmin);
	}

	um_nptln(pt1,xt1->p1,xt1->v1,pto);
	d1 = um_dot(pt1,xt1->v1);
	d2 = um_dot(xt1->p1,xt1->v1);
	d3 = um_dot(xt1->p2,xt1->v1);
	if (d1 < d2)
		um_vctovc(xt1->p1,pto);
	else if (d1 > d3)
		um_vctovc(xt1->p2,pto);
	dmin = um_dcccc(pt1,pto);

	um_nptln(pt1,xt1->p2,xt1->v2,pt2);
	d1 = um_dot(pt1,xt1->v2);
	d2 = um_dot(xt1->p2,xt1->v2);
	d3 = um_dot(xt1->p3,xt1->v2);
	if (d1 < d2)
		um_vctovc(xt1->p2,pt2);
	else if (d1 > d3)
		um_vctovc(xt1->p3,pt2);
	d1 = um_dcccc(pt1,pt2);
	if (dmin > d1)
	{
		dmin = d1;
		um_vctovc(pt2,pto);
	}

	um_nptln(pt1,xt1->p3,xt1->v3,pt2);
	d1 = um_dot(pt1,xt1->v3);
	d2 = um_dot(xt1->p3,xt1->v3);
	d3 = um_dot(xt1->p2,xt1->v3);
	if (d1 < d2)
		um_vctovc(xt1->p3,pt2);
	else if (d1 > d3)
		um_vctovc(xt1->p1,pt2);
	d1 = um_dcccc(pt1,pt2);
	if (dmin > d1)
	{
		dmin = d1;
		um_vctovc(pt2,pto);
	}
	return (dmin);
}

/*********************************************************************
**    E_FUNCTION     :  UU_REAL ncl_lnlndis()
**       Calculate the squared distance and near points for 2 line segments.
**  Copied from http://geometryalgorithms.com/Archive/algorithm_0106/algorithm_0106.htm
**    PARAMETERS
**       INPUT  :
**          spt1    - Start point of line 1.
**          ept1    - End point of line 1.
**          uvc1    - Unit vector along line 1.
**          spt2    - Start point of line 2.
**          ept2    - End point of line 2.
**          uvc2    - Unit vector along line 2.
**       OUTPUT :
**          npt1    - Point on line 1 nearest to line 2.
**          npt2    - Point on line 2 nearest to line 1.
**    RETURNS      : Distance**2
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL ncl_lnlndis(spt1,ept1,spt2,ept2,npt1,npt2)
UM_coord spt1,ept1,spt2,ept2,npt1,npt2;
{
	UM_vector u,v,w;
	UU_REAL a,b,c,d,e,D;
	UU_REAL sc, sN, sD;
	UU_REAL tc, tN, tD;

	um_vcmnvc(ept1,spt1,u);
	um_vcmnvc(ept2,spt2,v);
	um_vcmnvc(spt1,spt2,w);
	a = UM_DOT(u,u);        /* always >= 0 */
	b = UM_DOT(u,v);
	c = UM_DOT(v,v);        /* always >= 0 */
	d = UM_DOT(u,w);
	e = UM_DOT(v,w);
	D = a*c - b*b;          /* always >= 0 */
	sD = D;
	tD = D;
/*
.....compute the line parameters of the two closest points
*/
	if (D < UM_DFUZZ) {
/*
..... the lines are almost parallel
..... force using point P0 on segment S1
..... to prevent possible division by 0.0 later

*/
		sN = 0.0;
		sD = 1.0;
		tN = e;
		tD = c;
	}
	else {
/*
..... get the closest points on the infinite lines
*/
		sN = (b*e - c*d);
		tN = (a*e - b*d);
		if (sN < 0) {
/*
..... sc < 0 => the s=0 edge is visible
*/
			sN = 0.0;
			tN = e;
			tD = c;
		}
		else if (sN > sD) {
/*
..... sc > 1 => the s=1 edge is visible
*/
			sN = sD;
			tN = e + b;
			tD = c;
		}
	}
/*
..... tc < 0 => the t=0 edge is visible
*/
	if (tN < 0) {
		tN = 0.0;
		if (-d < 0)
			sN = 0.0;
		else if (-d > a)
			sN = sD;
		else {
			sN = -d;
			sD = a;
		}
	}
	else if (tN > tD) {
/*
..... tc > 1 => the t=1 edge is visible
*/
		tN = tD;
		if ((-d + b) < 0)
			sN = 0;
		else if ((-d + b) > a)
			sN = sD;
		else {
			sN = (-d + b);
			sD = a;
		}
	}
/*
..... finally do the division to get sc and tc
*/
	sc = (fabs(sN) < UM_DFUZZ) ? 0 : sN / sD;
	tc = (fabs(tN) < UM_DFUZZ) ? 0 : tN / tD;
/*
..... get the difference of the two closest points
*/
	um_vctmsc(u,sc,u);
	um_vctmsc(v,tc,v);
	um_vcplvc(spt1,u,npt1);
	um_vcplvc(spt2,v,npt2);
	D = UM_SQDIS(npt1,npt2);
	return (D);
}

/*********************************************************************
**    E_FUNCTION     :  UU_REAL ncl_segtridis()
**       Calculate near points between a line segment and a triangle.
**       Call this routine after comparing the line segment with
**       all sides of the triangle using ncl_lnlndis().
**    PARAMETERS
**       INPUT  :
**          p1      - First point on segment.
**          p2      - Second point on segment.
**          v1      - Unit vector from first point to second point 2.
**          tri1    - Triangle.
**          dis     - Current shortest distance squared.
**       OUTPUT :
**          npt1    - Near point on line segment.
**          npt2    - Near point on triangle.
**          dis     - Shortest distance squared.
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_segtridis(p1,p2,v1,tri1,npt1,npt2,dis)
UM_coord p1,p2;
UM_vector v1;
NCL_xtriangle *tri1;
UM_coord npt1,npt2;
UU_REAL *dis;
{
	int nint;
	UU_REAL d1,d2,d3,d4,d5;
	UM_coord hpt1;
	d1 = UM_DOT (p1,tri1->pln) - tri1->pln[3];
	d2 = UM_DOT (p2,tri1->pln) - tri1->pln[3];
/*
.....If points are on opposite sides of the triangle plane,
.....determine whether line segment intersects triangle plane
.....inside triangle.
*/
	if (d1*d2 <= 0.0)
	{
		um_ilnpln(p1, v1,tri1->p1,tri1->pln,&nint,hpt1);
		if (nint > 0)
		{
			d3 = UM_DOT (hpt1,tri1->pl1) - tri1->pl1[3];
			d4 = UM_DOT (hpt1,tri1->pl2) - tri1->pl2[3];
			d5 = UM_DOT (hpt1,tri1->pl3) - tri1->pl3[3];
			if (d3>=0 && d4>=0.0 && d5>=0.0)
			{
				um_vctovc(hpt1,npt1);
				um_vctovc(hpt1,npt2);
				*dis = 0.0;
				return (UU_SUCCESS);
			}
		}
	}
/*
.....Otherwise, determine whether line segment end points projected
.....onto triangle plane are inside triangle
*/
	d1 = d1*d1;
	if (d1 < *dis)
	{
		d3 = UM_DOT (p1,tri1->pl1) - tri1->pl1[3];
		d4 = UM_DOT (p1,tri1->pl2) - tri1->pl2[3];
		d5 = UM_DOT (p1,tri1->pl3) - tri1->pl3[3];
		if (d3>=0 && d4>=0.0 && d5>=0.0)
		{
			*dis = d1;
			um_vctovc(p1,npt1);
			um_nptpln(p1,tri1->p1,tri1->pln,npt2);
		}
	}
	d2 = d2*d2;
	if (d2 < *dis)
	{
		d3 = UM_DOT (p2,tri1->pl1) - tri1->pl1[3];
		d4 = UM_DOT (p2,tri1->pl2) - tri1->pl2[3];
		d5 = UM_DOT (p2,tri1->pl3) - tri1->pl3[3];
		if (d3>=0 && d4>=0.0 && d5>=0.0)
		{
			*dis = d2;
			um_vctovc(p2,npt1);
			um_nptpln(p2,tri1->p1,tri1->pln,npt2);
		}
	}
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     :  UU_REAL ncl_tritridis()
**       Calculate near points and minimum distance for 2 triangles
**    PARAMETERS
**       INPUT  :
**          tri1    - First triangle
**          tri2    - Second triangle
**       OUTPUT :
**          npt1    - Near point on triangle 1.
**          npt2    - Near point on triangle 2.
**    RETURNS      : Distance squared
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL ncl_tritridis(tri1,tri2,npt1,npt2)
NCL_xtriangle *tri1, *tri2;
UM_coord npt1,npt2;
{
	UU_REAL dis1, hdis1;
	UM_coord hpt1, hpt2;
/*
.....Compare all sides of first triangle with all sides of second
.....triangle.
*/
	hdis1 = ncl_lnlndis(tri1->p1,tri1->p2,tri2->p1,tri2->p2, npt1,npt2);
	dis1  = ncl_lnlndis(tri1->p1,tri1->p2,tri2->p2,tri2->p3, hpt1,hpt2);
	if (dis1 < hdis1)
	{
		hdis1 = dis1;
		um_vctovc(hpt1,npt1);
		um_vctovc(hpt2,npt2);
	}
	dis1 = ncl_lnlndis(tri1->p1,tri1->p2,tri2->p3,tri2->p1,hpt1,hpt2);
	if (dis1 < hdis1)
	{
		hdis1 = dis1;
		um_vctovc(hpt1,npt1);
		um_vctovc(hpt2,npt2);
	}
	dis1 = ncl_lnlndis(tri1->p2,tri1->p3,tri2->p1,tri2->p2,hpt1,hpt2);
	if (dis1 < hdis1)
	{
		hdis1 = dis1;
		um_vctovc(hpt1,npt1);
		um_vctovc(hpt2,npt2);
	}
	dis1 = ncl_lnlndis(tri1->p2,tri1->p3,tri2->p2,tri2->p3,hpt1,hpt2);
	if (dis1 < hdis1)
	{
		hdis1 = dis1;
		um_vctovc(hpt1,npt1);
		um_vctovc(hpt2,npt2);
	}
	dis1 = ncl_lnlndis(tri1->p2,tri1->p3,tri2->p3,tri2->p1,hpt1,hpt2);
	if (dis1 < hdis1)
	{
		hdis1 = dis1;
		um_vctovc(hpt1,npt1);
		um_vctovc(hpt2,npt2);
	}
	dis1 = ncl_lnlndis(tri1->p3,tri1->p1,tri2->p1,tri2->p2,hpt1,hpt2);
	if (dis1 < hdis1)
	{
		hdis1 = dis1;
		um_vctovc(hpt1,npt1);
		um_vctovc(hpt2,npt2);
	}
	dis1 = ncl_lnlndis(tri1->p3,tri1->p1,tri2->p2,tri2->p3, hpt1,hpt2);
	if (dis1 < hdis1)
	{
		hdis1 = dis1;
		um_vctovc(hpt1,npt1);
		um_vctovc(hpt2,npt2);
	}
	dis1 = ncl_lnlndis(tri1->p3,tri1->p1,tri2->p3,tri2->p1, hpt1,hpt2);
	if (dis1 < hdis1)
	{
		hdis1 = dis1;
		um_vctovc(hpt1,npt1);
		um_vctovc(hpt2,npt2);
	}
/*
.....Determine if any segment of either triangle is closer than the
.....current minimum distance to the interior of the other triangle.
*/
	ncl_segtridis(tri1->p1,tri1->p2,tri1->v1,tri2,npt1,npt2,&hdis1);
	ncl_segtridis(tri1->p2,tri1->p3,tri1->v2,tri2,npt1,npt2,&hdis1);
	ncl_segtridis(tri1->p3,tri1->p1,tri1->v3,tri2,npt1,npt2,&hdis1);
	ncl_segtridis(tri2->p1,tri2->p2,tri2->v1,tri1,npt2,npt1,&hdis1);
	ncl_segtridis(tri2->p2,tri2->p3,tri2->v2,tri1,npt2,npt1,&hdis1);
	ncl_segtridis(tri2->p3,tri2->p1,tri2->v3,tri1,npt2,npt1,&hdis1);

	return (hdis1);
}
