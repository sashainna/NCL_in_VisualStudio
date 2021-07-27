/*********************************************************************
**    NAME         :  neproj0.c
**       CONTAINS:
**  Wrapper functions for projecting various geometry types onto a
**  surface.
**
**          nclf_pn_project_sf()
**          nclf_cv_project_sf()
**          ncl_poly_project_sf()
**          ncl_cc_project_sf()
**          ncl_sp_project_sf()
**          ncl_cv_project_sfuv()
**          ncl_cv_proj_sf()
**          ncl_geo_project_sf()
**          ncl_dbx_print_proj()
**
**    COPYRIGHT 2007 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       neproj0.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:42
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "ginq.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdclass.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mplot.h"
#include "ncl.h"
#include "nccs.h"
#include "nclfc.h"
#include "ngeom.h"
#include "nproj.h"
#include "mdeval.h"
#include "msrf.h"
#include "modef.h"

#define DEBUGX 0

void ncl_dbx_print_proj();
static int S_fix_uvproj();

/*********************************************************************
**    E_FUNCTION     : nclf_pn_project_sf(pnkey,sfkey,uv,iwrap,iatach,
**				iproj,angs,vckey,atkey,sakey,uva,npkey,nclkey,numpts,err)
**
**       Fortran callable routine to project a pattern onto a surface.
**
**    PARAMETERS
**       INPUT  :
**              pnkey        - Key of patern entity
**              sfkey        - Key for the surface
**              uv           - Starting UV parameters for surface.
**              iwrap          0  NOWRAP
**                             1  WRAP
**                             2  REVOLV
**                             3  RADIAL
**              iatach       - where in the pattern to start projecting
**                             1  START
**                             2  MIDDLE
**                             3  END
**                             4  CENTER
**                             5  AT (must also supply attach_pt)
**              iproj        - 0  no user-supplied projection vector
**                             1  there is a projection vector
**                             2  project atangl to the surface
**              angs         - Starting and ending angles when 'iproj = 2'
**              vckey        - Key for projection vector
**              atkey        - Key for attach point
**              sakey        - Key for secondary surface when 'iproj = 2'
**              uva          - Starting UV parameters for secondary surface.
**              npkey        - Key for the near point
**       OUTPUT :
**              nclkey       - Key for new, projected pattern
**              numpts       - number of points in pattern
**              err            error flag -> UU_SUCCESS or UU_FAILURE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclf_pn_project_sf(pnkey,sfkey,uv,iwrap,iatach,iproj,angs,vckey,atkey,sakey,
	uva,npkey,nclkey,numpts,err)
UM_int2 *iwrap,*iatach,*iproj,*numpts,*err;
UM_int4 *pnkey,*sfkey,*vckey,*atkey,*sakey,*npkey,*nclkey;
UM_real8 uv[],uva[],angs[];
{
	int i, status, patt_type,npts;
	UM_int2 first, ncltype, inc2, default_err;
	UU_LOGICAL listmem;
	UU_REAL ptrec[6],dis,len,um_getpolylen();
	UM_coord *pts;
	UM_vector *vcs;
	UU_LIST cplist,cnlist,*vclst;
	NCL_proj_struc proj;
/*
.....Initialize routine
*/
	default_err = 419;    /* FAILED TO FIND LINE-CURVE INTERSECTION */
	*err = 0;
	listmem = UU_FALSE;
/*
.....Get number of points in pattern and type of pattern.
.....patt_type = 1  -> pattern of points
.....patt_type = 2  -> pattern of point vectors
*/
	gtpnnp1(pnkey, numpts, &patt_type);
	proj.npts = *numpts;
/*
.....Get array of points and optional normals
*/
	proj.inpt  = (UM_coord *)uu_malloc(proj.npts*sizeof(UM_coord));
	proj.vectype = 0;
	if (patt_type == 2) proj.vectype = 1;
	status = ncl_load_pattern_arrays (pnkey, patt_type, proj.inpt, UU_NULL,
		proj.npts);
	if (status != UU_SUCCESS) goto failed;
/*
.....Calculate tangent & percentage
.....vectors for Atangle projection
*/
	if (*iproj == 2)
	{
		proj.tvec  = (UM_vector *)uu_malloc(proj.npts*sizeof(UM_coord));
		ncl_calc_tanvecs(proj.inpt,proj.npts,proj.tvec);
		proj.uprm  = (UU_REAL *)uu_malloc(proj.npts*sizeof(UU_REAL));
		len = um_getpolylen(proj.npts,proj.inpt);
		proj.uprm[0] = 0.;
		for (i=1;i<proj.npts;i++)
		{
			dis = um_dcccc(proj.inpt[i],proj.inpt[i-1]);
			proj.uprm[i] = proj.uprm[i-1] + dis/len;
		}
	}
	else
	{
		proj.tvec = UU_NULL;
		proj.uprm = UU_NULL;
	}
/*
.....Set up projection parameters
*/
	proj.sfkey = *sfkey;
	proj.sakey = *sakey;
	if (proj.sakey == 0) proj.sakey = proj.sfkey;
	proj.wrap = *iwrap;
	proj.attach = *iatach;
	proj.ptype = *iproj;
	proj.trimext = UU_FALSE;
	proj.ssplin = UU_FALSE;
	proj.onsrf = UU_FALSE;
	proj.sang = angs[0];
	proj.eang = angs[1];
	proj.tol = .001;
	proj.uv[0] = uv[0]; proj.uv[1] = uv[1];
	proj.uva[0] = uva[0]; proj.uva[1] = uva[1];
	if (proj.ptype == 1)
	{
		status = ncl_load_pt_vect(vckey,2,proj.pvec);
		if (status != UU_SUCCESS) goto failed;
	}
	if (proj.attach == 5)
	{
		status = ncl_load_pt_vect(atkey,1,proj.atpt);
		if (status != UU_SUCCESS) goto failed;
	}
	else um_nullvc(proj.atpt);
	proj.nrptfl = 0;
	if (*npkey != 0)
	{
		proj.nrptfl = 1;
		status = ncl_load_pt_vect(npkey,1,proj.nrpt);
		if (status != UU_SUCCESS) goto failed;
	}
	else um_nullvc(proj.nrpt);
/*
.....Allocate lists for projected points
*/
	uu_list_init(&cplist,sizeof(UM_coord),proj.npts,proj.npts/4);
	vclst = UU_NULL;
	if (patt_type == 2)	/* Point Vectors */
	{
		vclst = &cnlist;
		uu_list_init(vclst,sizeof(UM_vector),proj.npts,proj.npts/4);
	}
	listmem = UU_TRUE;
/*
.....Project the points
*/
	status = ncl_geo_project_sf(&proj,&cplist,vclst,UU_NULL,&npts);
	if (status != UU_SUCCESS) goto failed;
/*
.....Create new pattern
*/
	first = 1;
	status = ptpnhd(ptrec, &patt_type, nclkey, &first);
	if (status != UU_SUCCESS) goto failed;
/*
.....Store the points within the pattern
*/
	pts = (UM_coord *)UU_LIST_ARRAY(&cplist);
	vcs = (UM_vector *)UU_LIST_ARRAY(&cnlist);
	inc2 = 1;
	for (i=0; i<npts; i++)
	{
		um_vctovc(pts[i],ptrec);
		if (patt_type == 2) um_vctovc(vcs[i],&ptrec[3]);
		status = ptpnpt(ptrec, nclkey, &inc2);
		if (status != UU_SUCCESS) goto failed;
		inc2++;
	}
/*
..... Output the key and display the new pattern.
*/
	ncltype =20;
	status = dspent(nclkey,&ncltype);
/*
.....End of routine
.....Free up all the memory
*/
done:;
	uu_free(proj.inpt);
	if (proj.tvec != UU_NULL) uu_free(proj.tvec);
	if (proj.uprm != UU_NULL) uu_free(proj.uprm);
	if (listmem)
	{
		uu_list_free(&cplist);
		if (vclst != UU_NULL) uu_list_free(vclst);
	}
	return UU_SUCCESS;
/*
.....Failed to create pattern
*/
failed:;
	*err = default_err;
	return UU_FAILURE;
}

/*********************************************************************
**    E_FUNCTION     : nclf_cv_project_sf(itsk,cvkey,sfkey,uv,tol,iwrap,iatach,
**				iproj,angs,vckey,atkey,sakey,uva,npkey,nclkey,err)
**
**       Fortran callable routine to project a curve onto a surface.
**
**    PARAMETERS
**       INPUT  :
**              jtsk         - 1 = Create SPLINE, 2 = SSPLIN,
**              cvkey        - Key of curve entity
**              sfkey        - Key for the surface
**              uv           - Starting UV parameters for surface.
**              dtol         - Tolerance for evaluating curve.
**              iwrap          0  NOWRAP
**                             1  WRAP
**                             2  REVOLV
**                             3  RADIAL
**              iatach       - where in the pattern to start projecting
**                             1  START
**                             2  MIDDLE
**                             3  END
**                             4  CENTER
**                             5  AT (must also supply attach_pt)
**              iproj        - 0  no user-supplied projection vector
**                             1  there is a projection vector
**                             2  project atangl to the surface
**              angs         - Starting and ending angles when 'iproj = 2'
**              vckey        - Key for projection vector
**              atkey        - Key for attach point
**              sakey        - Key for secondary surface when 'iproj = 2'
**              uva          - Starting UV parameters for secondary surface.
**              npkey        - Key for the near point
**       OUTPUT :
**              uv           - Last evaluated surface UV when 'jtsk' = 22.
**              nclkey       - Key for new, projected curve
**              err            error flag -> UU_SUCCESS or UU_FAILURE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclf_cv_project_sf(jtsk,cvkey,sfkey,uv,dtol,iwrap,iatach,iproj,angs,vckey,atkey,
	sakey,uva,npkey,nclkey,err)
UM_int2 *jtsk,*iwrap,*iatach,*iproj,*err;
UM_int4 *cvkey,*sfkey,*vckey,*atkey,*sakey,*npkey,*nclkey;
UM_real8 uv[],uva[],angs[],*dtol;
{
	int status,itsk,relnum;
	NCL_proj_struc proj;
/*
.....Initialize routine
*/
	*err = 0;
	itsk = *jtsk;
/*
.....Set up projection parameters
*/
	proj.sfkey = *sfkey;
	proj.sakey = *sakey;
	if (proj.sakey == 0) proj.sakey = proj.sfkey;
	proj.wrap = *iwrap;
	proj.attach = *iatach;
	proj.ptype = *iproj;
	proj.trimext = UU_FALSE;
	proj.ssplin = UU_FALSE;
	if (itsk != 1) proj.ssplin = UU_TRUE;
	proj.onsrf = UU_FALSE;
	proj.sang = angs[0];
	proj.eang = angs[1];
	proj.tol = *dtol;
	proj.uv[0] = uv[0]; proj.uv[1] = uv[1];
	proj.uva[0] = uva[0]; proj.uva[1] = uva[1];
	if (proj.ptype == 1)
	{
		status = ncl_load_pt_vect(vckey,2,proj.pvec);
		if (status != UU_SUCCESS) goto failed;
	}
	if (proj.attach == 5)
	{
		status = ncl_load_pt_vect(atkey,1,proj.atpt);
		if (status != UU_SUCCESS) goto failed;
	}
	else um_nullvc(proj.atpt);
	proj.nrptfl = 0;
	if (*npkey != 0)
	{
		proj.nrptfl = 1;
		status = ncl_load_pt_vect(npkey,1,proj.nrpt);
		if (status != UU_SUCCESS) goto failed;
	}
	else um_nullvc(proj.nrpt);
/*
.....Project composite curve
*/
	ur_retrieve_data_relnum(*cvkey,&relnum);
	if (relnum == UM_COMPCRV_REL)
		status = ncl_cc_project_sf(*cvkey,&proj,nclkey);
/*
.....Project single curve/spline
*/
	else status = ncl_sp_project_sf(*cvkey,&proj,nclkey,UU_FALSE);
/*
.....End of routine
.....Free up all the memory
*/
done:;
	*err = status;
	if (status == 0)
	{
		status = UU_SUCCESS;
		if (itsk == 22)
		{
			if (proj.onsrf) *err = 1;
			else *err = -1;
			if (!ncl_setver(94))
			{
				uv[0] = proj.uv[0];
				uv[1] = proj.uv[1];
			}
		}
		else ncl_def_color(*nclkey);
	}
	else status = UU_FAILURE;
	return(status);
/*
.....Could not create curve
*/
failed:;
	status = 163;
	goto done;
}

/*********************************************************************
**    E_FUNCTION     : ncl_poly_project_sf(pts,sfkey,uv,iwrap,iatach,
**				iproj,vckey,atkey,npkey,pto)
**
**       Projects a display list of polylines onto a surface.
**
**    PARAMETERS
**       INPUT  :
**              poly         - Polyline display list to project.
**              sfkey        - Key for the surface
**              uv           - Starting UV parameters for surface.
**              iwrap          0  NOWRAP
**                             1  WRAP
**                             2  REVOLV
**                             3  RADIAL
**              iatach       - where in the pattern to start projecting
**                             1  START
**                             2  MIDDLE
**                             3  END
**                             4  CENTER
**                             5  AT (must also supply attach_pt)
**              iproj        - 0  no user-supplied projection vector
**                             1  there is a projection vector
**              vckey        - Key for projection vector
**              atkey        - Key for attach point
**              npkey        - Key for the near point
**       OUTPUT :
**              pto          - Array of projected points and associated
**                             normal vectors.
**    RETURNS      : 0 on success, non-zero otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_poly_project_sf(poly,sfkey,uv,iwrap,iatach,iproj,vckey,atpt,npkey,pto)
UM_coord *poly;
UM_covec *pto;
UU_KEY_ID sfkey,vckey,npkey;
UM_coord atpt;
UU_REAL uv[];
int iwrap,iatach,iproj;
{
	int i,j,status,npts,nlists;
	UU_LOGICAL listmem;
	UU_KEY_ID key;
	UM_coord *pts,*lstp;
	UM_vector *vcs;
	UM_covec *lsto;
	UU_LIST cplist,cnlist;
	NCL_proj_struc proj;
/*
.....Initialize routine
*/
	listmem = UU_FALSE;
/*
.....Set up projection parameters
*/
	proj.sfkey = sfkey;
	proj.sakey = proj.sfkey;
	proj.tvec = UU_NULL;
	proj.uprm = UU_NULL;
	proj.wrap = iwrap;
	proj.attach = iatach;
	proj.ptype = iproj;
	proj.vectype = 1;
	proj.trimext = UU_FALSE;
	proj.ssplin = UU_FALSE;
	proj.onsrf = UU_FALSE;
	proj.sang = 0.;
	proj.eang = 0.;
	proj.tol = .001;
	proj.uv[0] = uv[0]; proj.uv[1] = uv[1];
	proj.uva[0] = 0.; proj.uva[1] = 0.;
	if (proj.ptype == 1)
	{
      key = vckey;
		status = ncl_load_pt_vect(&key,2,proj.pvec);
		if (status != UU_SUCCESS) goto failed;
	}
	if (proj.attach == 5) um_vctovc(atpt,proj.atpt);
	else um_nullvc(proj.atpt);
	proj.nrptfl = 0;
	if (npkey != 0)
	{
		proj.nrptfl = 1;
		key = npkey;
		status = ncl_load_pt_vect(&key,1,proj.nrpt);
		if (status != UU_SUCCESS) goto failed;
	}
	else um_nullvc(proj.nrpt);
/*
.....Allocate lists for projected points
*/
	uu_list_init(&cplist,sizeof(UM_coord),100,20);
	uu_list_init(&cnlist,sizeof(UM_vector),100,20);
	listmem = UU_TRUE;
/*
.....Loop through polylines
*/
	lstp = poly;
	lsto = pto;
	um_vctovc(lstp,lsto);
	um_nullvc(&lsto[0][3]);
	nlists = lstp[0][0];
	lstp++; lsto++;
	for (i=0;i<nlists;i++)
	{
/*
........Prepare for projection
*/
		proj.npts = lstp[0][0];
		um_vctovc(lstp,lsto);
		um_nullvc(&lsto[0][3]);
		lstp++; lsto++;
		proj.inpt = lstp;
/*
.....Single point
.....Just copy it
*/
		if (proj.npts == 1)
		{
			um_vctovc(proj.inpt[0],lsto[0]);
			um_nullvc(&lsto[0][3]);
		}
/*
.....Project the points
*/
		if (proj.npts > 1)
		{
			UU_LIST_EMPTY(&cplist);
			UU_LIST_EMPTY(&cnlist);
			status = ncl_geo_project_sf(&proj,&cplist,&cnlist,UU_NULL,&npts);
			if (status != UU_SUCCESS) goto failed;
/*
.....Store points in output array
*/
			pts = (UM_coord *)UU_LIST_ARRAY(&cplist);
			vcs = (UM_vector *)UU_LIST_ARRAY(&cnlist);
			for (j=0;j<npts;j++)
			{
				um_vctovc(pts[j],lsto[j]);
				um_vctovc(vcs[j],&lsto[j][3]);
			}
		}
		lstp += proj.npts; lsto += proj.npts;
	}
/*
.....End of routine
.....Free up all the memory
*/
done:;
	if (listmem)
	{
		uu_list_free(&cplist);
		uu_list_free(&cnlist);
	}
	return(status);
/*
.....Failed to project polylines
*/
failed:;
	status = UU_FAILURE;
	goto done;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cc_project_sf(cvkey,proj,nclkey)
**
**       Projects a composite curve onto a surface.
**
**    PARAMETERS
**       INPUT  :
**              cvkey        - Key of curve entity
**              proj         - Projection parameter settings.
**       OUTPUT :
**              proj.uv      - Last evaluated surface UV.
**              nclkey       - Key for new, projected curve
**    RETURNS      : Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cc_project_sf(cvkey,proj,nclkey)
UU_KEY_ID cvkey,*nclkey;
NCL_proj_struc *proj;
{
	int i,j,err,ncvs,status,iclass,n1;
	UU_LOGICAL modang,lstunlb;
	UU_REAL uu,sang,eang,msang,meang,mdang,tang;
	struct UM_compcrv_rec ccvp;
	struct UM_cid_rec *ccidp,*cids;
	UU_KEY_ID kcv, *keyp;
	UM_int2 lfl_77,i2;
	UM_coord *pts;
	UM_transf cvmat;
	UU_LIST cvpt;
	struct NCL_fixed_databag crv;
	struct UM_evcrvout evcrv;
	UU_REAL t0,t1;
	int addflg;
/*
.....Initialize routine
*/
	err = 0;
	lstunlb = UU_FALSE;
/*
.....Get the composite curve structure
*/
	cids = UU_NULL;
	ccvp.key = cvkey;
	status = ncl_retrieve_data_fixed(&ccvp);
	if (status != UU_SUCCESS) goto failed;
	ccidp = ccvp.cid;
	ncvs = ccvp.no_cid;
	t0 = ccvp.t0;
	t1 = ccvp.t1;
	addflg = ccvp.addflg;
	if (ncvs <= 0) goto failed;
	keyp = (UU_KEY_ID *) uu_malloc(ncvs*sizeof(UU_KEY_ID));
	if (keyp == UU_NULL) goto failed;
	lfl_77 = 1;
	stunlb (&lfl_77);
	lstunlb = UU_TRUE;;
/*
.....Use the same attach point for each sub-curve
.....By faking a user supplied attach point
*/
	if (proj->wrap >= 1 && proj->attach != 5)
	{
		status = uc_retrieve_transf(ccvp.key,cvmat);
		if (status != UU_SUCCESS) goto failed;
		if (proj->attach <= 3)
		{
			if (proj->attach == 2) uu = .5;
			else if (proj->attach == 3) uu = 1.;
			else uu = 0.;
			uc_init_evcrvout(&ccvp,&evcrv);
			status = uc_evcrv(UM_POINT,uu,&ccvp,cvmat,&evcrv);
			if (status == UU_FAILURE) goto failed;
			um_vctovc(evcrv.cp,proj->atpt);
		}
		else if (proj->attach == 4)
		{
			uu_list_init(&cvpt,sizeof(UM_coord),200,100);
			n1 = ncl_evolve_curve(&ccvp,cvmat,proj->tol,&cvpt,UU_NULL,UU_NULL,0);
			if (n1 <= 0)
			{
				uu_list_free(&cvpt);
				goto failed;
			}
			pts = (UM_coord *)UU_LIST_ARRAY(&cvpt);
			status = ncl_get_center_patt(pts,n1,proj->atpt);
			uu_list_free(&cvpt);
			if (status != UU_SUCCESS) goto failed;
		}
		proj->attach = 5;
	}
/*
.....With an angular projection
.....and different starting and ending angles
.....the angle must be iterated along each curve
*/
	modang = UU_FALSE;
	if (proj->ptype == 2 && proj->eang != proj->sang)
	{
		modang = UU_TRUE;
		msang = proj->sang;
		meang = proj->eang;
		mdang = proj->eang - proj->sang;
	}
/*
.....Loop through the composite curves
.......Skip over ends "removed" if curve was trimmed - Andrew 4/8/13
*/
	j = 0;
	for (i=0; i<ncvs && status == UU_SUCCESS; i++)
	{
		if (ccvp.t0 > ccidp->endparam) goto angl;
		keyp[j] = 0;
		crv.key = ccidp->crvid;
		status = ncl_retrieve_data_fixed(&crv);
		if (status == UU_FAILURE) break;
		status = uc_retrieve_transf(crv.key, cvmat);
		if (status == UU_FAILURE) break;
		iclass = uc_super_class(crv.rel_num);
		if (iclass !=  UM_CURVE_CLASS) goto failed;
/*
........Projection Angles are different
........Adjust for percentage along curve
*/
		if (modang) proj->eang = msang + mdang*ccidp->endparam;
/*
........If curve is reversed
........then reverse the angle signs
*/
		sang = proj->sang;
		eang = proj->eang;
		if (proj->ptype == 2 && ccidp->reverse)
		{
			tang = proj->sang;
			proj->sang = proj->eang * -1.;
			proj->eang = tang * -1.;
		}
/*
........Project curve to surface
*/
		um_c5_trimpart(&crv,ccvp.cid,ccvp.no_cid,t0,t1,i,cvmat);
		err = ncl_sp_project_sf(crv.key,proj,&kcv,UU_TRUE);
		ur_update_data_fixed(&crv);
		if (err == 0) keyp[j++] = kcv;
		else if (err > 0) goto done;
		if (kcv <= 0) goto failed;
/*
........Store curve and projection data
*/
angl:
		proj->sang = sang;
		proj->eang = eang;
		if (modang) proj->sang = proj->eang;
		if (ccidp->endparam > ccvp.t1) break;
		ccidp++;
	}
	ncvs = j;
	if (ncvs <= 0)
	{
		err = 163;
		goto done;
	}
/*
.....Restore projection angles
*/
	if (modang)
	{
		proj->sang = msang;
		proj->eang = meang;
	}
/*
.....Merge the projected curves
*/
	if (lstunlb)
	{
		stunlb (&lfl_77);
		lstunlb = UU_FALSE;
	}
	if (status == UU_SUCCESS)
	{
		ccvp.key = 0;
		status = um_c5_mergecrv (ncvs,keyp,&ccvp);
		cids = ccvp.cid;
	}
/*
.....Create the composite curve
*/
	if (status == UU_SUCCESS)
	{
		i2 = 0;
		ncl_label_wf(ccvp.rel_num,ccvp.label,&ccvp.subscr,ccvp.key,&i2);
		status = uc_create_mtuple_data (&ccvp,UM_DEFAULT_TF,UM_CURRENT_ATTR);
//		ccvp.t0 = t0;
//		ccvp.t1 = t1;
		ccvp.addflg = addflg;
		ur_update_data_fixed(&ccvp);
		if (status == UU_SUCCESS)
		{
			*nclkey = ccvp.key;
			ncl_store_wf1(ccvp.key);
		}
	}
	if (status != UU_SUCCESS) goto failed;
/*
.....End of routine
.....Free the memory
*/
done:;
	if (lstunlb)
	{
		stunlb (&lfl_77);
		lstunlb = UU_FALSE;
	}
	if (keyp)
	{
		for (i=0; i<ncvs; i++)
			if (keyp[i]) uc_delete(keyp[i]);
		uu_free(keyp);
	}
	if (cids) uu_free (cids);
	return(err);
/*
.....Could not project curves
*/
failed:;
	err = 5;
	goto done;
}

/*********************************************************************
**    E_FUNCTION     : ncl_sp_project_sf(cvkey,proj,nclkey,compfl)
**
**       Projects a single curve/spline onto a surface.
**
**    PARAMETERS
**       INPUT  :
**              cvkey        - Key of curve entity
**              proj         - Projection parameter settings.
**              compfl       - UU_TRUE:  Curve is from a composite cv
**                             UU_FALSE: otherwise
**       OUTPUT :
**              proj.uv      - Last evaluated surface UV
**              nclkey       - Key for new, projected curve
**    RETURNS      : Non-zero on error. Note that the return can be
**                   a warning(negative) if compfl is set to UU_TRUE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sp_project_sf(cvkey,proj,nclkey,compfl)
UU_KEY_ID cvkey,*nclkey;
NCL_proj_struc *proj;
UU_LOGICAL compfl;
{
	int status,npts,lfl,i;
	int default_err,err;
	UU_REAL u0,v0,u1,v1,u2,v2,a;
	UM_coord *pts,*uvp;
	UM_vector *vcs;
	UU_LIST cplist,cnlist,ulist,*uptr,*cptr;
	UM_2Dcoord vc1;
	struct UM_rbsplcrv_rec cv;
/*
.....Initialize routine
*/
	default_err = 163;
	err = 0;
	pts = uvp = UU_NULL;
	vcs = UU_NULL;
	uptr = NULLST;
/*
.....Allocate lists for projected points
*/
	uu_list_init(&cplist,sizeof(UM_coord),200,100);
	uu_list_init(&cnlist,sizeof(UM_vector),200,100);
	if (proj->ssplin)
	{
		uu_list_init(&ulist,sizeof(UM_coord),200,100);
		uptr = &ulist;
	}
	else
		uu_list_init0(&ulist);
/*
.....Project the curve
*/
	status = ncl_cv_proj_sf(cvkey,proj,&cplist,&cnlist,uptr,&npts);
	if (status != UU_SUCCESS) goto failed;
/*
.....Do the spline fitting of the points
*/
	pts = (UM_coord *) UU_LIST_ARRAY(&cplist);
	uvp = (UM_coord *) UU_LIST_ARRAY(&ulist);
	if (cnlist.cur_cnt > 0)
	vcs = (UM_vector *) UU_LIST_ARRAY(&cnlist);
/*
.....Check whether projected points will define valid curve when
.....projecting a component of a composite curve. Andrew 3/25/13
*/
	if (compfl)
	{
		for (i=1;i<npts;i++)
		{
			if (um_dcccc(pts[i-1],pts[i]) < UM_DFUZZ)
			{
				uu_list_delete(&cnlist,i,1);
				if (cnlist.cur_cnt > 0)
					uu_list_delete(&cplist,i,1);
				if (ulist.cur_cnt > 0)
					uu_list_delete(&ulist,i,1);
				pts = (UM_coord *) UU_LIST_ARRAY(&cplist);
				uvp = (UM_coord *) UU_LIST_ARRAY(&ulist);
				if (cnlist.cur_cnt > 0)
				vcs = (UM_vector *) UU_LIST_ARRAY(&cnlist);
				i--; npts--;
			}
		}
/*
.....Curve projected to single point, which means it was perp to the
.....projection surface.
*/
		if (npts <= 1)
		{
			default_err = -default_err;
			goto failed;
		}
	}
/*
........Give an error for switchbacks in uv points.
*/
	if (proj->ssplin && npts > 2)
	{
		lfl = 0;
		if (NCL_triansf == 1 || NCL_triansf == 2)
		{
/*
...........Fix the first or last point if a triangular surface
...........(when at u0 all the values of v come to one point)
*/
			if (NCL_triansf == 1)
				u0 = 0;
			else
				u0 = 1;

			if (fabs(uvp[0][0] - u0) < 0.001 && fabs(uvp[npts-1][0] - u0) > 0.1)
			{
				lfl = 1;
				u1 = uvp[1][0]; u2 = uvp[2][0];
				v1 = uvp[1][1]; v2 = uvp[2][1];
			}
			else if (fabs(uvp[npts-1][0] - u0) < 0.001 &&
				fabs(uvp[0][0] - u0) > 0.1)
			{
				lfl = 2;
				u1 = uvp[npts-2][0]; u2 = uvp[npts-3][0];
				v1 = uvp[npts-2][1]; v2 = uvp[npts-3][1];
			}
			if (lfl > 0 && fabs (u2-u1) > UM_FUZZ)
			{
/*
..... put the first/last point on the extension of the next segment
*/
				a = (v2-v1)/(u2-u1);
				v0 = v2 - a*(u2-u0);
				if (lfl == 1)
					uvp[0][1] = v0;
				else
					uvp[npts-1][1] = v0;
			}
		}

		vc1[0] = uvp[1][0] - uvp[0][0];
		vc1[1] = uvp[1][1] - uvp[0][1];
/*
.....Do not check for switchbacks as
.....These can be valid curves for some surfaces
.....(projall.pp CVX(5) Along vector to SF1)
.....Bobby  -  7/2/07
*/
/*
		for (i = 2; i < npts; i++)
		{
			vc2[0] = uvp[i][0] - uvp[i-1][0];
			vc2[1] = uvp[i][1] - uvp[i-1][1];
			rdot = UM_DOT_2D (vc1,vc2);
			if (rdot < -5.e-7)
			{
				err = 481;
				goto failed;
			}
			vc1[0] = vc2[0]; vc1[1] = vc2[1];
		}
*/
		if (!ncl_setver(94))
		{
			proj->uv[0] = uvp[npts-1][0];
			proj->uv[1] = uvp[npts-1][1];
		}
	}
/*
.....Remove last vector in case the evaluation routine
.....treats it as a closed curve
*/

/*
.....Create spline
.....supporting sharp corners
*/
	cptr = &cplist;
	if (proj->ssplin) cptr = &ulist;

	if (vcs != UU_NULL)
	{
		if (proj->wrap == 0)
			um_nullvc(vcs[npts-1]);

	npts = ncevolveA(proj->tol,npts,&cplist,&cnlist);

		vcs = (UM_vector *) UU_LIST_ARRAY(&cnlist);
		um_nullvc(vcs[npts-1]);
	}

	if (proj->ssplin)
	{
		status = S_fix_uvproj(&cplist,&ulist,&cnlist,npts,proj);
		if (status != UU_SUCCESS) goto failed;
#if DEBUGX == 1
		ncl_dbx_print_proj("$$ Fix UVproj",&ulist,&cnlist);
#endif
	}
	status = ncl_geogn2(npts,cptr,&cnlist,UU_NULL,proj->ssplin,&proj->sfkey,
		1,&cv);
	*nclkey = cv.key;
	if (status != UU_SUCCESS) goto failed;
/*
.....End of routine
.....Free up all the memory
*/
done:;
	if (uptr != UU_NULL) uu_list_free(uptr);
	uu_list_free(&cplist);
	uu_list_free(&cnlist);
	return(err);
/*
.....Failed to create pattern
*/
failed:;
	status = UU_FAILURE;
	if (err == 0) err = default_err;
	goto done;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cv_project_sfuv(cvkey,sfkey,uv,tol,uvlist)
**
**       Projects a curve onto a surface and returns a list of the
**       UV points from the projection.
**
**    PARAMETERS
**       INPUT  :
**              cvkey        - Key of curve entity
**              sfkey        - Key for the surface
**              uv           - Starting UV parameters for surface.
**              tol          - Tolerance for projection
**       OUTPUT :
**              ulist        - List of projected UV-points.
**              npts         - Number of UV-points.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cv_project_sfuv(cvkey,sfkey,uv,tol,ulist,npts)
UU_KEY_ID cvkey,sfkey;
UU_REAL uv[],tol;
UU_LIST *ulist;
int *npts;
{
	int status;
	NCL_proj_struc proj;
	UU_LIST cplist;
/*
.....Allocate lists
*/
	uu_list_init(&cplist,sizeof(UM_coord),200,100);
/*
.....Set up projection parameters
*/
	proj.sfkey = sfkey;
	proj.sakey = sfkey;
	proj.wrap = 0;
	proj.attach = 1;
	proj.ptype = 0;
	proj.trimext = UU_TRUE;
	proj.ssplin = UU_TRUE;
	proj.onsrf = UU_FALSE;
	proj.sang = 0.;
	proj.eang = 0.;
	proj.tol = tol;
	proj.uv[0] = uv[0]; proj.uv[1] = uv[1];
	proj.uva[0] = uv[0]; proj.uva[1] = uv[1];
	proj.nrptfl = 0;
/*
.....Project the curve
*/
	status = ncl_cv_proj_sf(cvkey,&proj,&cplist,UU_NULL,ulist,npts);
/*
.....Free the lists
*/
	uu_list_free(&cplist);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cv_proj_sf(cvkey,proj,cplist,cnlist,ulist,npts)
**
**       Projects a single curve/spline onto a surface and returns the
**       list of points, vectors, and uv-parameters created.
**
**    PARAMETERS
**       INPUT  :
**              cvkey        - Key of curve entity
**              proj         - Projection parameter settings.
**       OUTPUT :
**              cplist       - Projected points.
**              cnlist       - Projected normal vectors.
**              ulist        - UV-parameters of projected points.
**              npts         - Number of projected points.
**    RETURNS      : Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cv_proj_sf(cvkey,proj,cplist,cnlist,ulist,npts)
UU_KEY_ID cvkey;
NCL_proj_struc *proj;
UU_LIST *cplist,*cnlist,*ulist;
int *npts;
{
	int i,status,listmem;
	UU_REAL dis,len,um_getpolylen();
	UM_coord *upt;
	UM_transf cvmat;
	UU_LIST cvpt,cvtang,uary;
	struct NCL_fixed_databag crv;
	struct UM_evcrvout evcrv;
/*
.....Initialize routine
*/
	listmem = 0;
	um_evlset();
/*
.....Get the curve structure
*/
	crv.key = cvkey;
	status = ncl_retrieve_data_fixed(&crv);
	if (status == UU_SUCCESS) status = uc_retrieve_transf(crv.key,cvmat);
	if (status != UU_SUCCESS) goto failed;
	uc_init_evcrvout(&crv,&evcrv);
/*
.....Initialize lists
*/
	uu_list_init(&cvpt,sizeof(UM_coord),200,100);
	uu_list_init(&cvtang,sizeof(UM_coord),200,100);
	uu_list_init(&uary,sizeof(UM_coord),200,100);
	listmem = 1;
/*
.....Get array of points and optional normals
*/
	proj->npts = ncl_evolve_curve(&crv,&cvmat,proj->tol,&cvpt,&cvtang,&uary,2);
/*
........UV-splines return the surface UV-parameters
........in the 'uary' list, so convert these to
........curve U-parameters, as required by 'ncl_fix_evol'
*/
	if (crv.rel_num == UM_UVCVONSF_REL)
	{
		proj->inpt = (UM_coord *)UU_LIST_ARRAY(&cvpt);
		upt = (UM_coord *)UU_LIST_ARRAY(&uary);
		len = um_getpolylen(proj->npts,proj->inpt);
		upt[0][0] = 0.; upt[0][1] = 0.;
		for (i=1;i<proj->npts;i++)
		{
			dis = um_dcccc(proj->inpt[i],proj->inpt[i-1]);
			upt[i][0] = upt[i-1][0] + dis/len;
			upt[i][1] = 0.;
		}
	}
/*
........Make sure enough points are generated on curve
*/
	if (proj->wrap >= 1)
	{
		status = ncl_fix_evol_a (&crv,cvmat,&evcrv,&proj->npts,proj->tol,
			&cvpt,&cvtang,&uary);

		if (status == UU_SUCCESS)
		status = ncl_fix_corners_a (&crv,cvmat,&evcrv,&proj->npts,proj->tol,
			&cvpt,&cvtang,&uary);
	}
	else
	{
		status = ncl_fix_evol_a (&crv,cvmat,&evcrv,&proj->npts,proj->tol,
			&cvpt,&cvtang,&uary);
	}
	if (proj->npts <= 1 || status != UU_SUCCESS) goto failed;
	proj->inpt = (UM_coord *)UU_LIST_ARRAY(&cvpt);
	proj->tvec = (UM_vector *)UU_LIST_ARRAY(&cvtang);
	proj->vectype = 0;
	if (cnlist != UU_NULL) proj->vectype = 2;
	proj->uprm = UU_NULL;
	if (proj->ptype == 2)
	{
		proj->uprm = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*proj->npts);
		upt = (UM_coord *)UU_LIST_ARRAY(&uary);
		for (i=0;i<proj->npts;i++) proj->uprm[i] = upt[i][0];
	}
/*
.....Project the points
*/
	status = ncl_geo_project_sf(proj,cplist,cnlist,ulist,npts);
/*
.....End of routine
.....Free up all the memory
*/
done:;
	if (listmem >= 1)
	{
		uu_list_free(&cvpt);
		uu_list_free(&cvtang);
		uu_list_free(&uary);
		if (proj->uprm != UU_NULL) uu_free(proj->uprm);
	}
	um_evlrst();
	return(status);
/*
.....Failed to project points
*/
failed:;
	status = UU_FAILURE;
	goto done;
}

/*********************************************************************
**    E_FUNCTION     : ncl_geo_project_sf(proj,cplist,cnlist,ulist,npts)
**
**       Controlling routine for projecting geometry onto a surface.
**
**    PARAMETERS
**       INPUT  :
**              proj         - Projection parameter settings.
**       OUTPUT :
**              cplist       - Projected points.
**              cnlist       - Projected normal vectors.
**              ulist        - UV-parameters of projected points.
**              npts         - Number of points in output lists.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_geo_project_sf(proj,cplist,cnlist,ulist,npts)
NCL_proj_struc *proj;
UU_LIST *cplist,*cnlist,*ulist;
int *npts;
{
	int status;
	UM_int2 mdsys,mm,idx;
	UM_real8 tol8;
/*
.....Work without MODSYS
.....in UNITS/INCH
.....and with specified tolerance
*/
	gtmsmm(&mdsys,&mm);
	idx = 27; getsc(&idx,&tol8); setscv(&idx,&proj->tol);
/*
.....Nowrap projection
*/
	if (proj->wrap == 0)
		status = ncl_geo_project_nowrap(proj,cplist,cnlist,ulist,npts);
/*
.....Wrap projection
*/
	else
		status = ncl_geo_project_wrap(proj,cplist,cnlist,ulist,npts);
/*
.....End of routine
*/
	stmsmm(&mdsys,&mm);
	idx = 27; setscv(&idx,&tol8);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_dbx_print_proj(msg,cplist,cnlist)
**
**       Prints out the projected points and vectors for debug purposes.
**
**    PARAMETERS
**       INPUT  :
**              msg          - Message to output with points/vectors.
**              cplist       - Projected points.
**              cnlist       - Projected vectors.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_dbx_print_proj(msg,cplist,cnlist)
char *msg;
UU_LIST *cplist,*cnlist;
{
	int i;
	char sbuf[80];
	UM_coord *ptv;
	UM_vector *vvv,vxx;
	ptv = (UM_coord *)UU_LIST_ARRAY(cplist);
	if (cnlist != UU_NULL) vvv = (UM_vector *)UU_LIST_ARRAY(cnlist);
	NclxDbgPstr(" ");
	NclxDbgPstr(msg);
	for (i=0;i<UU_LIST_LENGTH(cplist);i++)
	{
		if (cnlist != UU_NULL && um_mag(vvv[i]) > UM_FUZZ)
		{
			um_unitvc(vvv[i],vxx);
			sprintf(sbuf,"PV/%g,%g,%g,%lf,%lf,%lf",ptv[i][0],ptv[i][1],ptv[i][2],
				vxx[0],vxx[1],vxx[2]);
		}
		else
		{
			sprintf(sbuf,"PT/%g,%g,%g",ptv[i][0],ptv[i][1],ptv[i][2]);
		}
		NclxDbgPstr(sbuf);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_fix_uvproj(cplist,ulist,cnlist,npts,proj)
**
**       Adjusts a 3-D projection to work in the UV-space of a surface.
**
**    PARAMETERS
**       INPUT  :
**              cplist       - Projected XYZ points.
**              cnlist       - Projected XYZ tangent vectors.
**              npts         - Number of points in lists.
**              proj         - Projection parameter settings.
**       OUTPUT :
**              ulist        - UV-parameters of projected points.
**              cnlist       - Projected  UV tangent vectors.
**    RETURNS      : UU_FAILURE on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_fix_uvproj(cplist,ulist,cnlist,npts,proj)
UU_LIST *cplist,*ulist,*cnlist;
int npts;
NCL_proj_struc *proj;
{
	int i,status;
	UU_REAL side;
	struct NCL_fixed_databag surf;
	UM_transf tfmat;
	UM_coord *uvp,*pts,cpt,uv;
	UM_vector *vcs,vec,norm;
/*
.....Get the surface
*/
	surf.key = proj->sfkey;
	status = ncl_retrieve_data_fixed (&surf);
	status += uc_retrieve_transf (surf.key, tfmat);
	if (status != UU_SUCCESS) goto done;
/*
.....Empty the U-array
*/
	uvp = (UM_coord *)UU_LIST_ARRAY(ulist);
	uv[0] = uvp[0][0]; uv[1] = uvp[0][1]; uv[2] = 0.;
	UU_LIST_EMPTY(ulist);
/*
.....Reproject all points onto surface
*/
	pts = (UM_coord *)UU_LIST_ARRAY(cplist);
	vcs = (UM_vector *)UU_LIST_ARRAY(cnlist);
	side = 0.;
	for (i=0;i<npts;i++)
	{
		status = ncl_pt_proj_sf(pts[i],&surf,&uv[0],&uv[1],&side,0,UU_NULL,
			proj->tol,cpt,norm);
		if (status != UU_SUCCESS) goto done;
		status = ncl_ve_proj_sf(pts[i],vcs[i],&surf,tfmat,uv[0],uv[1],&side,0,
			UU_NULL,UU_TRUE,proj->tol,cpt,vec);
		if (status != UU_SUCCESS) goto done;
		uu_list_push(ulist,uv);
		um_vctovc(vec,vcs[i]);
	}
/*
.....End of routine
*/
done:;
	return(status);
}
