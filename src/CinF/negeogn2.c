/*********************************************************************
**    NAME         :  negeogn2.c
**       CONTAINS:  Routines removed from negeogn.c not needed by IGES.
**
**        ncl_geogn2
**        ncvofs
**        ncofcv
**		  ncl_curve_ssplin
**
**    COPYRIGHT 2000 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       negeogn2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:34
*********************************************************************/

#include "udebug.h"
#include "msrf.h"
#include "mcrv.h"
#include "nccs.h"
#include "nclfc.h"
#include "mdgenent.h"
#include "mdrel.h"
#include "modef.h"
#include "uminmax.h"
#include "class.h"
#include "mdattr.h"
#include "mattrddl.h"

/*********************************************************************
**    E_FUNCTION     : ncl_geogn2 (npts, cvpoint, cvtang, vdel, crv)
**       Create a B-spline curve by points and tangent vectors.
**    PARAMETERS   
**       INPUT  : 
**          vdel - translation vector, or NULL
**          tol      - tolerance (=sc(27))
**       OUTPUT :  
**          nclkey  - key of created B-spline, =0 if fail
**          ier     - error number, if fail
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_geogn2 (npts, cvpoint, cvtang, vdel, ssplin, sfkey, itsk, crv)
int npts,itsk;
UU_LIST *cvpoint,*cvtang;
UU_REAL *vdel;
UU_LOGICAL ssplin;
UU_KEY_ID *sfkey;
struct UM_rbsplcrv_rec *crv;
{
	UU_LIST seglist;
	UU_REAL *pbuf, *tbuf;
	int i, status;
	struct NCL_crvgen_rec seg, *segp;
	UM_real8 ver;
	UM_int2 idx = 169;

	getsc (&idx, &ver);

	if (npts < 2) return (UU_FAILURE);
	status = UU_SUCCESS;
/*
..... Do the spline fitting the points and tangents
*/
	pbuf = (UU_REAL *) UU_LIST_ARRAY(cvpoint);
	tbuf = (UU_REAL *) UU_LIST_ARRAY(cvtang);

	uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), npts, npts);

	for (i = 0; i < npts; i++, pbuf+=3, tbuf+=3)
	{
		ncl_init_seg (&seg);
		if (vdel != UU_NULL)
		{
			seg.x = pbuf[0] + vdel[0];
			seg.y = pbuf[1] + vdel[1];
			seg.z = pbuf[2] + vdel[2];
		}
		else
		{
			seg.x = pbuf[0];
			seg.y = pbuf[1];
			seg.z = pbuf[2];
		}
		seg.a = tbuf[0];
		seg.b = tbuf[1];
		seg.c = tbuf[2]; 
		if (seg.a*seg.a + seg.b*seg.b + seg.c*seg.c > 0.0001)
			seg.inv = 1;

		uu_list_push (&seglist, &seg);
	}

	segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);

	if (ver < 9.449)
		status = ncl_interp_rbsp (npts, segp, 1, crv);
	else
	{
		UU_LIST_EMPTY (cvpoint);
		status = ncl_interp_with_corners (npts,segp,cvpoint,ssplin,sfkey,itsk,crv);
	}

	if (status == UU_SUCCESS && crv->key == NULLKEY) status = UU_FAILURE;

	if (status == UU_SUCCESS)
	{
		ncl_def_color (crv->key);
	}

	uu_list_free (&seglist);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_curve_ssplin(nclkey)
**       Check if the input curve is surface spline.
**    PARAMETERS   
**       INPUT  : 
**          nclkey  - key of SSpline, =0 if fail
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_TRUE if sspline; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_curve_ssplin(nclkey)
UM_int4 *nclkey;
{
	int status;	
	struct UM_crvdatabag crv;
	UM_transf tfmat, *tf;

	status = UU_FALSE;
	
	crv.key = *nclkey;
	status = ncl_retrieve_data_fixed(&crv);

	if (status != UU_SUCCESS) return (UU_FALSE);
	
	if (ncl_itsa_compcrv(&crv))
		return ncl_itsa_compcrv_onsf(&crv);
	else
		return ncl_itsa_uvcv_onsf(&crv);

	return status;
}

/*********************************************************************
**    E_FUNCTION     : ncvofs(nclkey,delx,dely,delz,tol,ier)
**       Create a B-spline curve as a translation (by a vector) of a 
**       curve entity
**    PARAMETERS   
**       INPUT  : 
**          (delx,dely,delz) - translation vector
**          tol      - tolerance (=sc(27))
**       OUTPUT :  
**          nclkey  - key of created B-spline, =0 if fail
**          ier     - error number, if fail
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
***********************************translate****************************/
int ncvofs(nclkey,delx,dely,delz,told,ier)
UM_int4 *nclkey;
UM_real8 *delx,*dely,*delz, *told;
UM_int2 *ier;
{
	UU_LIST cvpoint, cvtang;
	UU_REAL tol;
	UM_vector vdel;
	int ix, npts, n1;
	int status;
	struct UM_rbsplcrv_rec crv;

	status = UU_SUCCESS;
	*ier = 0;
	*nclkey = 0;
	n1 = npts = 0;
	tol = *told;
/*
..... evolve a curve - get points and tangent vectors
*/
	ix = 2;
	n1 = ncevolv (ix, tol, &cvpoint, &cvtang);
	if (n1 <= 0) goto Err;
/*
..... add some extra points to be within tolerance; add some points
..... and vectors to define corners in the case of a composite curve 
..... with non-smooth joints.
*/
	npts = ncevolveA (tol, n1, &cvpoint, &cvtang);
	if (npts <= 0) goto Err;

	vdel[0] = *delx;
	vdel[1] = *dely;
	vdel[2] = *delz;

	status = ncl_geogn2 (npts, &cvpoint, &cvtang, vdel, UU_FALSE,
		(UU_KEY_ID *)UU_NULL,1,&crv);

	if (status == UU_SUCCESS)
	{
		*nclkey = crv.key;
		goto Done;
	}

Err:;
	if (*ier == 0) *ier = 163;
	status = UU_FAILURE;

Done:;
	uu_list_free (&cvpoint);
	uu_list_free (&cvtang);

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncofcv(itsk,nclkey,dm,dis,told,ier)
**       Create B-spline curve as an offset (along normals, at a fixed
**       distance) of a curve entity
**    PARAMETERS   
**       INPUT  : 
**			itsk	- 1: spline offset 2:ssplin offset
**          dm      - direction modifier vector
**          dis     - offset distance
**          tol     - tolerance (=sc(27))
**       OUTPUT :  
**          nclkey  - key of created B-spline, 0 if fail
**          ier     - error number, if fail
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : the normals to a curve are projected to the z=0
**                   plane before offsetting, so the routine is intended
**                   for flat curves only
***********************************offset****************************/
int ncofcv(itsk,nclkey,dm,dis,told,ier)
UM_int4 *nclkey;
UM_real8 *dm,*dis, *told;
UM_int2 *itsk,*ier;
	{
	UU_LIST cvpoint, cvtang,cvpoints,cvtangs;
	UU_REAL ofdis, tol;
	int ix, npts, n1;
	int status,istat;
	struct UM_rbsplcrv_rec crv;
	UU_KEY_ID sfkey,delkey;
	UU_REAL  box1[6],dx,dy,dz,dmax;
	struct  UC_entitydatabag c2;
	struct  NCL_nclattr_rec *nattr;
	UM_transf tmat;
	struct UC_attributedatabag tatr;
	int color;
	UU_LOGICAL lv97,lssplin;	
	struct NCL_fixed_databag cv;		
	UM_transf tfmat;
	struct UM_transf_rec tran;

	lv97 = ncl_setver(97);
	sfkey = NULLKEY;
	delkey = NULLKEY;
	lssplin= UU_FALSE;
	
	status = UU_SUCCESS;
	npts = n1 = 0;	
	tol = *told;
	ofdis = *dis;

/*
.....Check if the input curve is surface spline
.....If ssplin, convert uvcv curve into xyzcv
*/
	if (ncl_curve_ssplin(nclkey))
	{
		ix = 2;
		istat = ncl_uvcv_to_xyzcv(ix,&sfkey,&tran,&delkey);

/*
... Calculate bounding box and uv tolerance
*/
		dmax = 1.0;
		status = ncl_geo_box(sfkey,box1);
		dx = box1[3] - box1[0];
		dy = box1[4] - box1[1];
		dz = box1[5] - box1[2];
		dmax = MAX3(dx,dy,dz);
		if (dmax < 1.0)
			dmax = 1.0;
		tol = *told / dmax;
	}
	else
	{
		if (*itsk == 2)
			goto Err;
	}

	*nclkey = 0;

/*
..... evolve a curve - get points and tangent vectors
*/
	ix = 2;
	n1 = ncevolv (ix, tol, &cvpoint, &cvtang);
	if (n1 <= 0) goto Err;

/*
.....Tranform dm vector
*/
	if (sfkey > NULLKEY)
	{
		um_inverttf(tran.tfmat, tfmat);
		um_cctmtf (dm,tfmat,dm);

		if (ofdis < 0.0)
		{
			ofdis = -ofdis;
			dm[0] = -dm[0];
			dm[1] = -dm[1];						
			dm[2] = -dm[2];
		}
	}

/*
..... offset the curve along normals; check whether the offset distance 
..... is reasonable; remove loops if necessary.
..... add some extra points to be within tolerance; add some points
..... and vectors to define corners in the case of a composite curve 
..... with non-smooth joints.
*/
	npts = ncevofA (sfkey,tol,n1,dm,ofdis,&cvpoint,&cvtang,2);
	if (npts <= 0) goto Err;

/*
.....Convert uv-points and uv-tangs to cvpoints and cvtangs
*/
	if (*itsk == 1 && sfkey > NULLKEY)
	{
		npts = UU_LIST_LENGTH(&cvpoint);
		uu_list_init (&cvpoints, sizeof(UM_coord), npts, npts);
		uu_list_init (&cvtangs,  sizeof(UM_vector), npts, npts);
		uu_list_push_list(&cvpoints,&cvpoint);
		uu_list_push_list(&cvtangs,&cvtang);
		UU_LIST_EMPTY(&cvpoint);
		UU_LIST_EMPTY(&cvtang);
		status = ncl_convert_uvpoints(sfkey,&cvpoints,&cvtangs,
									&cvpoint,&cvtang);
		uu_list_free(&cvpoints);
		uu_list_free(&cvtangs);
	}

/*
.....create rbspine
*/	
	status = ncl_geogn2 (npts, &cvpoint, &cvtang, (UU_REAL *) UU_NULL, UU_FALSE,
						&sfkey,*itsk,&crv);

	if (*itsk == 2 && sfkey > NULLKEY && status == UU_SUCCESS)
	{
		status = um_cp_struct_rbcv_uvcv (&crv,&sfkey,&c2);
		uc_delete(crv.key);
	}

/*
.....Delete temporary uv curve keys
*/
	if (delkey > NULLKEY)
		uc_delete(delkey);

/*
.....Create the surface spline
*/
	if (status == UU_SUCCESS)
	{
		if (*itsk == 2 && sfkey > NULLKEY)
		{
			ncl_retrieve_ent(&c2,&tatr,&tmat);
			ncl_get_entity_color(c2.key, &color);
			tatr.color = color;
			nattr  = (struct  NCL_nclattr_rec *) &(tatr);
			nattr->label_on = 0;
			status = ur_update_attr(&tatr);
			ur_update_displayable(c2.key, UM_DISPLAYABLE);
			*nclkey = c2.key;

/*
.....Copy the surface transformation
.....to the surface spline
*/
			uc_transform(&c2,tran.tfmat,UU_TRUE);
		}
		else
		{		
			*nclkey = crv.key;
/*
.....Copy the surface transformation
.....to the surface spline
*/
			if (sfkey > NULLKEY)		
				uc_transform(&crv,tran.tfmat,UU_TRUE);
		}
		goto Done;
	}

Err:;
	if (*ier == 0) *ier = 163;
	status = UU_FAILURE;

Done:;
	if (npts != 0)
	{
		uu_list_free (&cvpoint);
		uu_list_free (&cvtang);
	}

	return(status);
}
