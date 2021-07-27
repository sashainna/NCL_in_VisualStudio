/*********************************************************************
**    NAME         :  necvsup1.c
**       CONTAINS: routines to handle surface splines.
**
**       int ncl_uvcrv_to_crv
**       int ncl_gen_bndr_crv
**       int ncl_create_ssplin
**       int ncl_disp_cvonsf
**       int ncl_intof_def
**       int ncl_conv_trim_rbcv_uvcv
**       UU_LOGICAL ncl_itis_uvcv
**       int ncl_proj_cvonsf_to_plane
**       int ncl_cvonsf_get_bskey
**       int ncl_cvonsf_get_bskeys
**       int ncl_no_ssplin_comp_selected
**       int ncl_trans_cvsf
**       int ncl_cvonsf_wrap 
**       int ncl_cvonsf_proj 
**       void pts2pl
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       necvsup1.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 17:14:44
*********************************************************************/
#include "ncldef.h"
#include "nccs.h"
#include "mattr.h"
#include "mdattr.h"
#include "view.h"
#include "dasnog.h"
#include "dselmask.h"
#include "ginqatt.h"
#include "gobas.h"
#include "gtbl.h"
#include "mdpick.h"
#include "modef.h"
#include "ulist.h"
#include "mdcpln.h"
#include "zsysdep.h"
#include "class.h"
#include "nclfc.h"
#include "msrfddl.h"
#include "mgeom.h"
#include "mdeval.h"

extern UU_LIST NCL_uvintof;

/*********************************************************************
**    E_FUNCTION     : ncl_uvcrv_to_crv (eptr,tfmat,cvid,tol,key,subid,subid1,idir)
**       Evolve selected boundary curve of the trimmed surface, and
**       use points to generate rbspline curve as a new entity.
**    PARAMETERS
**       INPUT  :
**          eptr    - trimmed surface entity 
**          tfmat   - identity matrix of surface
**          cvid    - id number of boundary curve to evolve
**          subid   - start sub_id of composite boundary curve to evolve
**			subid1	- end sub_id of composite boundary curve to evolve
**			idir	- CLW/CCLW direction from start subid to end subid1
**					  1 : CLW, -1 : CCLW
**          tol     - tolerance used to evolve curve
**       OUTPUT :
**          key     - key of created B-spline
**    RETURNS      :
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************/
int ncl_uvcrv_to_crv (eptr,tfmat,cvid,tol,key,subid,subid1,idir)
struct NCL_trimsf_rec *eptr;
UM_transf tfmat;
UU_REAL tol;
int cvid, subid, subid1, idir;
UU_KEY_ID *key;
{
	int i, status;
	int *nbpt;
	UU_LIST cvpts,tangs;

	uu_list_init (&cvpts, sizeof(UM_coord), 100, 100);
	uu_list_init (&tangs, sizeof(UM_coord), 100, 100);

	*key = (UU_KEY_ID) 0;
	nbpt = UU_NULL;
/*	
.....evolve boundary curve
*/
	status = um_bndr_curve (eptr,tfmat,cvid,subid,subid1,idir,tol,&nbpt,&cvpts,&tangs);
	if (status != UU_SUCCESS) 
	{
		status = 472; goto Done;
	}

	if ((subid <= 0 || (fabs(subid1-subid) >= 1 && subid >= 0 && subid1 >= 0)) &&
		nbpt != UU_NULL)
	{
		if (cvid == 0) 
			*key = eptr->uv_key;
		else
			*key = eptr->ibndykey[2*cvid-1];
/*
.....create composite curve from segments
*/
		status = ncl_gen_bndr_crv (tfmat,nbpt,&cvpts,&tangs,key);

		uu_free (nbpt);	
	}
	else
	{
		UM_coord *pts;
		UM_vector *vs;
		int n;
 		struct NCL_crvgen_rec *ptve, *cpt;
		struct UM_rbsplcrv_rec rbsp;

		n = cvpts.cur_cnt;
		pts = (UM_coord *) UU_LIST_ARRAY (&cvpts);
		vs = (UM_coord *) UU_LIST_ARRAY (&tangs);

 		cpt = (struct NCL_crvgen_rec *) 
			uu_toolmalloc ((n+1)*sizeof(struct NCL_crvgen_rec));
 		ptve = cpt;
 		for (i=0; i<n; i++)
		{
			ncl_init_seg (ptve);
			ptve->x = pts[i][0];
			ptve->y = pts[i][1];
			ptve->z = pts[i][2];
			if (um_mag(vs[i]) > 99000.)
			{
				ptve->a = vs[i][0]; ptve->b = vs[i][1]; ptve->c = vs[i][2];
				ptve->inv = 1;
			}
	 		ptve++;
		} 
		status = ncl_interp_rbsp (n, cpt, 0, &rbsp);
		uu_toolfree(cpt);
		if (status == UU_SUCCESS)
		{
			ur_update_displayable(rbsp.key, UM_NEVERDISPLAYABLE);
			*key = rbsp.key;
		}
	}

Done:
	uu_list_free (&cvpts);
	uu_list_free (&tangs);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_gen_bndr_crv (eptr,tfmat,nbpt,cvpts,key)
**       Generate composite bspline curve using points. 
**    PARAMETERS
**       INPUT  :
**          tfmat   - identity matrix of surface
**          nbpt    - distribution of points among components
**          cvpts   - array (list) of points
**          key     - key of corresponding composite uv-boundary curve
**       OUTPUT :
**          key     - key of created B-spline
**    RETURNS      :
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_gen_bndr_crv (tfmat,nbpt,cvpts,tangs,key)	
UM_transf tfmat;
int *nbpt;
UU_LIST *cvpts,*tangs;
UU_KEY_ID *key;
{
	int status,i,j,n,ncv;
	UM_coord *pts;
	UM_vector *vs;
 	struct NCL_crvgen_rec *ptve, *cpt;
	struct UM_rbsplcrv_rec rbsp;
	UU_KEY_ID cvkey, *keylst;
	UM_int2 i2 = 1;

	pts = (UM_coord *) UU_LIST_ARRAY (cvpts);
	vs = (UM_vector *) UU_LIST_ARRAY (tangs);
	cvkey = *key;
	*key = (UU_KEY_ID) 0;
	keylst = UU_NULL;

	ncv = nbpt[0];

	keylst = (UU_KEY_ID *) uu_malloc (ncv*sizeof(UU_KEY_ID));

	stunlb(&i2);

	status = UU_SUCCESS;
  	for (i=0; i<ncv && status == UU_SUCCESS; i++)
  	{           
		n = nbpt[i+1];
 		cpt = (struct NCL_crvgen_rec *) 
			uu_toolmalloc (n*sizeof(struct NCL_crvgen_rec));
 		ptve = cpt;
 		for (j=0; j<n; j++)
		{
			ncl_init_seg (ptve);
			ptve->x = pts[j][0];
			ptve->y = pts[j][1];
			ptve->z = pts[j][2];
			if (um_mag(vs[j]) > 99000.)
			{
				ptve->a = vs[j][0]; ptve->b = vs[j][1]; ptve->c = vs[j][2];
				ptve->inv = 1;
			}
	 		ptve++;
		} 
		status = ncl_interp_rbsp (n, cpt, 1, &rbsp);
		uu_toolfree(cpt);
		if (status == UU_SUCCESS)
		{
			ur_update_displayable(rbsp.key, UM_NEVERDISPLAYABLE);
			keylst[i] = rbsp.key;
			pts += n; vs += n;
		}
	}
	if (status == UU_SUCCESS)
	{
		stunlb(&i2);
		status = um_c5_mergecrv1(&cvkey,keylst,tfmat,ncv,key);
	}

	if (keylst != UU_NULL) uu_free(keylst);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_create_ssplin (sfkey,s,pts,npt,cvkey)
**       Setup and store uv rbspline curve on surface in unibase.
**    PARAMETERS
**       INPUT  :
**          sfkey   - key of base surface of the curve.
**          s       - parameters array of the bpsline
**          pts     - control points of the bpsline
**          npt     - number of control points
**       OUTPUT :
**          nclkey  - key of created uv B-spline on surface
**    RETURNS      :
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_ssplin (sfkey,s,pts,npt,cvkey)
int *sfkey, *npt, *cvkey;
UU_REAL *s;
UM_coord *pts;
{
	struct UM_uvcvonsf_rec cvsf;
	struct NCL_fixed_databag sf;
	UM_transf tran;
	int i, ns, n, status;
	UU_KEY_ID bskey;


	n = *npt;
/*
.....setup curve entity in unibase
*/
	cvsf.key = 0;
	ur_setup_data (UM_UVCVONSF_REL,&cvsf,sizeof(struct NCL_fixed_databag));
	status = ncl_create_entity(&cvsf, NCLI_CURVE);	
	*cvkey = cvsf.key;
/*
.....2 points: just line (can be edge or u/v line)
.....create linear spline instead of cubic
*/
	cvsf.planar = UU_TRUE;
	cvsf.open = UU_TRUE;
	if (n == 2)
	{
		cvsf.k = 2;
		cvsf.n = 1;
		ns   = 4;
	}
/*
.....create cubic spline
*/
	else
	{
		ns    = n + 4;
		cvsf.k = 4;
		cvsf.n = n - 3;
	}
	cvsf.t0 = 0;	
	cvsf.t1 = s[ns-1];	
	status += ur_update_data_varlist (cvsf.key, 1, s, 1, ns);
	status += ur_update_data_varlist (cvsf.key, 2, pts, 1, n);
	for (i=0; i<n; i++) s[i] = 1.0;
	status += ur_update_data_varlist (cvsf.key, 3, s, 1, n);
/*
.....save sf key in cv data structure,
.....update base surface with new uv curve key
*/
	if (status == UU_SUCCESS)
	{
		sf.key  = bskey = *sfkey;
		status = ncl_retrieve_data_fixed(&sf);
		if (status == UU_SUCCESS)
		{
			if (sf.rel_num == NCL_TRIMSF_REL)
			{
				uc_retrieve_transf (sf.key,tran);
				uc_transform(&cvsf,tran,UU_TRUE);
				gttbse (sfkey,&bskey);
				sf.key  = bskey;
				status = ncl_retrieve_data_fixed (&sf);
			}
			status = ncl_update_sskey (&sf,cvsf.key,1);
		}
		cvsf.bskey = bskey;
		status = ur_update_data_fixed(&cvsf);
	}
	
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_disp_cvonsf (eptr,tfmat,attrptr)
**       Diplays uv curve on surface entity. This function now
**       supports display of curve with the fixed number of points
**    PARAMETERS
**       INPUT  :
**          eptr    - pointer to the CV on sf data structure 
**          tfmat   - pointer to identity matrix of the curve
**          attrptr - pointer to attribute data struct of the curve
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_disp_cvonsf (eptr,tfmat,attrptr)
struct UM_uvcvonsf_rec *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;
{
	struct UM_rbsplcrv_rec rbcv, *rbptr;
	struct NCL_fixed_databag bsrf, *bsptr;
	UU_LIST cvpts;
	UU_REAL *pts;
	UU_REAL bplm[4], tol;
	int status, i, j, n;
	UM_int2 idx, ival;

	bplm[0] = bplm[2] = 0;
	bplm[1] = bplm[3] = 1;
	bsptr = &bsrf;
	rbptr = &rbcv;

	bsrf.key = eptr->bskey;
	status = ncl_retrieve_data_fixed (&bsrf);
	if (status == UU_SUCCESS)
	{
		if (ncl_itsa_trimsrf (&bsrf))
			status = ncl_trimsrf_get_bs (&bsrf,&bsptr);
	}

	um_set_disp_attr(attrptr);
/*
.....copy curve_on_srf data to rbsp curve structure
.....since it is rbspline. This allows use evolving
.....functions for rbsplines without any change. 
*/
	if (status == UU_SUCCESS)
	{
		ncl_cp_struct_uvcv_rbcv (eptr,&rbptr);
/*
.....get tolerance for points, evolve curve with tolerance
*/
		idx = 136;
		getifl(&idx, &ival);
		n = ival;

		if (n > 0)
		{
			uu_list_init (&cvpts, sizeof(UM_coord), n, n);	
			status = ncl_evolve1_bn_crv_on_srf (&bsrf,tfmat,rbptr,bplm,n,&cvpts);
			if (status != UU_SUCCESS) n = 0;
		}
		else
		{
			idx = 175;
			getsc (&idx,&tol);
			n = (tol < .005)? 100: 50;
			uu_list_init (&cvpts, sizeof(UM_coord), n, n);	
	 		n = ncl_evolve_bn_crv_on_srf (&bsrf,tfmat,rbptr,bplm,tol,&cvpts,
				UU_NULL,UU_NULL);
		}

		pts = (UU_REAL *) UU_LIST_ARRAY (&cvpts);
/*
.....display curve
*/
		for (i=0, j=0; i<n; i++, j+=3)
		glina3 (&pts[j],&pts[j+1],&pts[j+2]);

		gdraw();
		uu_list_free (&cvpts);
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_intof_def (sfkey, ifit, nents, nclkey)
**       Create uv spline curve using set of uv coordinates generated
**       by intersection of SF-SF or SF-PL.
**    PARAMETERS   
**       INPUT  : 
**          sfkey   - key of base surface where curve is created
**          nents   - number of points to interpolate
**          ifit    - = 0 interpolate
**                    = 1 fit
**       OUTPUT :  
**          nclkey  - key of created uv curve on surface.
**    RETURNS      : 
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
****************************************fit*****************************/
int ncl_intof_def (sfkey, ifit, nents, nclkey)
UM_int4 *nents, *sfkey, *nclkey;
UM_int2 *ifit;
{
	int i, ns, status;
	int npts, itsk;
	struct NCL_crvgen_rec seg, *segp;
	UU_LIST seglist;
	UM_int4 cvkey;
	UM_coord *ptrec;
	UM_coord opt;
	UU_REAL *pt, *s, *pts;

	uu_denter(UU_MTRC,(us,"ncl_uv_def ()"));

	status = UU_SUCCESS;
	*nclkey = 0;
	s = pts = UU_NULL;
	npts = 0;

	ptrec = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
	uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), 20, 20);
/*
.....set segments for rb-spline interpolator
*/
	ncl_init_seg (&seg);
	for (i=0; i<*nents; i++)
	{
		pt = (UU_REAL *) ptrec; 
		seg.x = pt[0];
		seg.y = pt[1];
		if (i>0)
		{
			if (um_dcccc(opt, pt) < UM_EQPARM && *ifit != 2) 
			{  
				ptrec++; status = UU_FAILURE; 
			}
		}
		if (status == UU_SUCCESS)
		{
			uu_list_push (&seglist, &seg);
			um_vctovc (pt, opt);
			npts++;
			ptrec++;
		}
		status = UU_SUCCESS;
	}
	itsk = *ifit;
/*
.....interpolate rbspline 
*/
	segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);
	if (status == UU_SUCCESS)
	{
		status = ncl_interp_rbsp1 (npts, segp, itsk, &ns, &s, &pts);
/*
.....store curve in unibase
*/
		if (status == UU_SUCCESS)
		{
			status = ncl_create_ssplin (sfkey,s,(UM_coord *)pts,&ns,&cvkey);
			*nclkey = cvkey;
		}
		else status = 51;
	}
/*
.....free memory used for interpolator
*/
	uu_list_free (&seglist);
	ncl_free_uv();
	if (s != UU_NULL) uu_free(s);
	if (pts != 0) uu_free(pts);

	uu_dexitstatus("ncl_intof_def ()", status);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_conv_trim_rbcv_uvcv(rbcv,uvcv)
**       Convert trimmed rbspl clone of a uv-curve on surface 
**       into its parent uv-curve; used in trimming routines
**       for uv-curves;
**    PARAMETERS
**       INPUT  :
**          rbcv  -  (trimmed) rational bspline curve
**       OUTPUT :
**          uvcv  -  its parent uv on surface curve after trimming
**    RETURNS      :
**          UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ncl_conv_trim_rbcv_uvcv(rbcv,uvcv)
struct UM_rbsplcrv_rec *rbcv;
struct UM_uvcvonsf_rec *uvcv;
{
	int status;

	uvcv->key = rbcv->key;
	status = ncl_retrieve_data_fixed(uvcv);
	if (status != UU_SUCCESS) return (status);
	uvcv->planar = rbcv->planar;
	if(ncl_itis_uvcv(rbcv)) uvcv->planar -= UM_UVCVONSF_REL;
	uvcv->t0 = rbcv->t0;
	uvcv->t1 = rbcv->t1;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  UU_LOGICAL ncl_itis_uvcv(rbcv)
**    Tells if a rbspl curve was created from a uv-curve on surface
**    PARAMETERS
**       INPUT  :
**          rbcv  -  rational bspline curve
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_TRUE/UU_FALSE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_itis_uvcv(rbcv)
struct UM_rbsplcrv_rec *rbcv;
{
	UU_LOGICAL result;

	result =  (rbcv->rel_num == UM_RBSPLCRV_REL) && 
	          (rbcv->planar  >= UM_UVCVONSF_REL);

	return (result);
}
/*********************************************************************
**    FUNCTION  : int ncl_proj_cvonsf_to_plane (eptr,vrefpt,vpnorm,pline,cvpts)
**    Evaluates a CVonSF into polyline with tolerance and projects the polyline
**    onto a plane; Used to draw a CVonSF
**    PARAMETERS
**       INPUT  :
**          eptr   - CVonSF to project 
**          vrefpt - reference point of projection plane
**          vpnorm - normal of projection plane
**       OUTPUT :
**          pline   - projected polyline 
**          cvpts   - list containing points of projected polyline
**    RETURNS      :
**           UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : list cvpts is allocated in this routine and must be freed 
**                   after use
**    WARNINGS     : none
*********************************************************************/
int ncl_proj_cvonsf_to_plane (eptr,vrefpt,vpnorm,pline,cvpts)
struct UM_uvcvonsf_rec *eptr;
UM_coord vrefpt;
UM_vector vpnorm;
struct UM_polyline_rec *pline;
UU_LIST *cvpts;
{
	struct UM_rbsplcrv_rec rbcv, *rbptr;
	struct NCL_fixed_databag bsrf;
	UM_transf tfmat;
	UU_REAL bplm[4], tol;
	int status;
	UM_int2 idx = 175;

	bplm[0] = bplm[2] = 0; bplm[1] = bplm[3] = 1;

	bsrf.key = eptr->bskey;
	status = ncl_retrieve_data_fixed (&bsrf);
	if (status == UU_SUCCESS) status = uc_retrieve_transf (bsrf.key,tfmat);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	uu_list_init (cvpts, sizeof(UM_coord), 100, 100);

	rbptr = &rbcv;
	ncl_cp_struct_uvcv_rbcv (eptr,&rbptr);
	getsc (&idx,&tol);

	ncl_evolve_bn_crv_on_srf (&bsrf,tfmat,&rbcv,bplm,tol,cvpts,
                                          UU_NULL,UU_NULL);

	ur_setup_data (UM_POLYLINE_REL, pline, sizeof(pline));
	pline->subscr = 0;

	pline->pt = (UU_REAL *) UU_LIST_ARRAY (cvpts);
	pline->no_pt = UU_LIST_LENGTH (cvpts);

	return (status);
}
/*********************************************************************
**    FUNCTION     :  int ncl_cvonsf_get_bskey (crv,bskey)
**       Gets base surface key for a CVonSF
**    PARAMETERS
**       INPUT  :
**          crv   -  a curve
**       OUTPUT :
**          bskey - key of base surface if crv is CVonSF; 0 otherwise
**    RETURNS      :
**           UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvonsf_get_bskey (crvkey,bskey)
UU_KEY_ID *crvkey,*bskey;
{
	struct NCL_fixed_databag crv;
	int status, key;

	key = 0;
	crv.key = *crvkey;
	status = ncl_retrieve_data_fixed (&crv);
	if (status != UU_SUCCESS) goto Done;

	switch (crv.rel_num)
	{
		case UM_UVCVONSF_REL:
		{
			struct UM_uvcvonsf_rec *uvcv;
			uvcv = (struct UM_uvcvonsf_rec *) &crv;
			key = uvcv->bskey;
			break;
		}
		case UM_COMPCRV_REL:
		{
			struct NCL_fixed_databag uvcv;
			int i,ncv,rev,key1;

			status = ncl_compcrv_getnents (&crv, &ncv);
			for (i=0; i<ncv && status == UU_SUCCESS; i++)
			{
				status = ncl_compcrv_getelm (&crv, i, &uvcv, &rev);
				key1 = 0;
				if (status == UU_SUCCESS && ncl_itsa_uvcv_onsf (&uvcv) )
				{
					key1 = ((struct UM_uvcvonsf_rec *)&uvcv)->bskey;
					if (i == 0) key = key1;
					status = (key1 == key)? UU_SUCCESS :	UU_FAILURE;
				}
			}
			if (i < ncv) key = 0;
			break;
		}
		default:
			break;
	}

Done:;
	*bskey = key;

	return (UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION     :  int ncl_cvonsf_get_bskeys(crv,inc,bskey)
**       Gets base surface keys for each component of a CVonSF
**       composite curve.  If the curve is not a composite curve,
**       then the single surface key is returned.
**    PARAMETERS
**       INPUT  :
**          crv   -  a curve
**          inc   -  composite curve component curve to return the
**                   key for.
**       OUTPUT :
**          bskey - key of base surface if crv is CVonSF; 0 otherwise
**    RETURNS      :
**           UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvonsf_get_bskeys(crv,inc,bskey)
struct UM_compcrv_rec *crv;
int inc;
UU_KEY_ID *bskey;
{
	int i,ncv,rev,key,status;
	struct NCL_fixed_databag uvcv;

	key = 0;

	switch (crv->rel_num)
	{
		case UM_UVCVONSF_REL:
		{
			if (inc == 0)
				status = ncl_cvonsf_get_bskey(crv->key,&key);
			else
				status = UU_FAILURE;
			break;
		}
		case UM_COMPCRV_REL:
		{

			status = ncl_compcrv_getnents(crv,&ncv);
			if (inc >= ncv)
				status = UU_FAILURE;
			else
			{
				status = ncl_compcrv_getelm(crv,inc,&uvcv,&rev);
				if (status == UU_SUCCESS && ncl_itsa_uvcv_onsf (&uvcv) )
				{
					key = ((struct UM_uvcvonsf_rec *)&uvcv)->bskey;
					status = UU_SUCCESS;
				}
			}
			break;
		}
		default:
			break;
	}

Done:;
	*bskey = key;

	return (UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION :  int ncl_no_ssplin_comp_selected (stuff)
**	   Checks if there is a composite curve made of SSPLIN's
**    in a buffer of selected entities for "CADD GEO" -> "MOVE" menu;
**    is called from functions of "c1uclass.c"
**
**    PARAMETERS
**       INPUT  : option - if stuff != UU_NULL, check just one entity 
**                         if stuff = UU_NULL, check the whole buffer
**       OUTPUT :
**    RETURNS      :
**           UU_SUCCESS if there is no such a composite curve;
**           UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_no_ssplin_comp_selected (stuff)
UD_PLOCREC *stuff;
{
	struct NCL_fixed_databag e1;
	UM_PICKENT modstuff;
	int bskey;
	UU_LOGICAL initialize = UU_TRUE;

	if (stuff != UU_NULL)
	{
		um_d_pickresolve (stuff,1,&modstuff);
		e1.key = um_get_pickkey (&modstuff, 1);
		if (ncl_retrieve_data_fixed(&e1) != UU_SUCCESS) return (UU_FAILURE);
		ncl_cvonsf_get_bskey (&e1.key, &bskey);
		if (bskey != 0) return (UU_FAILURE);
	}
	else
	{
		if (ur1_beginBatch() != UU_SUCCESS) return (UU_FAILURE);

		while(ud_gnxt(initialize, UU_NULL, &e1.key, 1) == UU_TRUE)
		{
			initialize = UU_FALSE;
			if (ncl_retrieve_data_fixed(&e1) != UU_SUCCESS) return (UU_FAILURE);

			ncl_cvonsf_get_bskey (&e1.key, &bskey);
			if (bskey != 0) return (UU_FAILURE);
		}

		if (ur1_endBatch() != UU_SUCCESS) return (UU_FAILURE);
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_trans_cvsf (eptr, tfmat, store)
**       Transform an SSPLIN (curve on surface).
**    PARAMETERS
**       INPUT  :
**          eptr       - pointer to the trimmed surface entity
**          tfmat      - transformation
**          store      - TRUE if transformation is to be updated
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : Updates transformation matrix of entity.
**    WARNINGS     : none
*********************************************************************/
int ncl_trans_cvsf (eptr, tfmat, store)
struct UM_uvcvonsf_rec *eptr;
UM_transf tfmat;
UU_LOGICAL store;
{
	int status;
	struct UM_transf_rec tran;

	status = UU_FAILURE;
	if (store)
	{
		tran.key = eptr->key;
		tran.rel_num = UM_TRANSFORM_REL;
		if (ur_retrieve_transf(&tran) == 0)
		{
			um_tftmtf(tran.tfmat, tfmat, tran.tfmat);
			if (ur_update_transf (&tran) == 0) status = UU_SUCCESS;
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : void pts2pl (nclkey,np,pln,tol8,ier)
**       Create the best fitting plane by a set of points. (The points
**       are stored in the global NCL_uvintof).
**    PARAMETERS
**       INPUT  :
**			sfkey	 - surface key
**          np       - number of points
**          tol8     - tolerance
**       OUTPUT :
**          pln     - the plane (point and normal)
**          ier     - 0 iff success
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void pts2pl (nclkey,np,pln,tol8,ier)
UM_int4 *nclkey;
UM_real8 pln[],*tol8;
UM_int2 *np,*ier;
{
	UU_REAL tol;
	int k,npts,status;
	UM_coord *p1, ppt;
	UM_vector pve;
	UU_LOGICAL lv97;

	npts = *np;
	p1 = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);

	*ier = 177;
	tol = *tol8;
	if (npts < 4) return;

	lv97 = ncl_setver(97);
	if (!lv97 && *nclkey > 0)
		status = ncl_sftopln (*nclkey,npts-1,p1,ppt,pve,tol);
	else
		status = um_ptstopln (npts-1,p1,ppt,pve,tol);
	if (status != UU_SUCCESS) return;

	*ier = 0;

	for (k = 0; k < 3; k++)
	{
		pln[k] = ppt[k];
		pln[k+3] = pve[k];
	}
}

/*********************************************************************
**    E_FUNCTION     : um_ptstopln (key,npt,pts,plpt,plvc,tol)
**       Calculate the best fitting plane for a surface.
**    PARAMETERS
**       INPUT  :
**			key			surface key
**          pts         points
**          npt         number of points
**          tol         tolerance
**       OUTPUT :
**          plpt        coordinates of the center point
**          plvc        plane normal
**    RETURNS      :   0 iff success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sftopln (key,npt,pts,plpt,plvc,tol)
UU_KEY_ID key;
UU_REAL tol;
int npt;
UM_coord *pts, plpt;
UM_vector plvc;
{
	UU_REAL tpn,d;
	int i,j,k,status,nuvs;
	UM_vector pve,vt;
	struct NCL_fixed_databag srf;
	struct UM_evsrfout evsrf;
	UM_srf_boundary bound;
	UM_coord *uvs;
	UM_transf tfmat;

	um_nullvc (plpt);
	um_nullvc (pve);
/*
..... calculate the center point
*/
	for (i = 0; i < npt; i++)
		um_vcplvc (plpt,pts[i],plpt);

	tpn = 1./npt;
	um_vctmsc (plpt,tpn,plpt);
/*
.... calculate the surface normal average from boudnary points
*/
	srf.key = key;
	status = ncl_retrieve_data_fixed(&srf,sizeof(struct NCL_fixed_databag));
	if (status == UU_SUCCESS)		
		status = uc_retrieve_transf (srf.key, tfmat);
/*
....Get uv points from the outer boundary of the surf.
*/
	status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,&srf,&bound);
	if (status == UU_SUCCESS)
	{
		nuvs = bound.np[0];
		uvs = (UM_coord *) UU_LIST_ARRAY (bound.uvpts);
	}

	for (j = 0; j < nuvs; j++)
	{
		status = uc_evsrf (UM_NORM,uvs[j][0],uvs[j][1],&srf,tfmat,&evsrf);
		if (status != UU_SUCCESS) 
			return (-1);		
		um_vcplvc (pve,evsrf.snorm,pve);
	}
	tpn = 1./nuvs;
	um_vctmsc (pve,tpn,pve);

	d = UM_DOT (pve,pve);
	if (d < tol*tol) return (-1);
	d = 1./sqrt(d);
	um_vctmsc (pve,d,plvc);

	return (0);
}
