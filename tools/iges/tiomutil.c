/*********************************************************************
**    NAME         :  tiomsrf.c
**    CONTAINS:
**       uio_agsrf_rbspl
**       uio_rbsrf
**       uio_revsrf
**       uio_nclsrf
**       uio_mshsrf 
**       gtspan   
**       nclsrf_bspdef 
**       uio_map_mpatch 
**       uio_trimsrf
**       uio_offset_srf
**			uio_uvcrv_to_crv 
**			uio_gen_bndr_crv
**			uio_c5_mergecrv1
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tiomutil.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:54
*********************************************************************/

#include    "usysdef.h"
#include    "mdeval.h"
#include    "udebug.h"
#include    "tiges.h"
#include    "tigdefs.h"
#include    "rbase.h"
#include    "class.h" 
#include    "mdrel.h" 
#include    "mdattr.h"
#include    "mattrddl.h"
#include    "mcrv.h"
#include    "msrf.h"
#include    "mdcoord.h"
#include    "tioconv.h"
#include    "modef.h"
#include    "mxxx.h"
#include    "view.h"
#include    "nccs.h"
#include    "ulist.h"
#include    "nclvx.h"
#include		"nclfc.h"
#include		"ncl.h"

extern UU_LOGICAL NCL_copy_compcrv;
char  *uu_toolmalloc();


/*********************************************************************
**    E_FUNCTION     : uio_uvcrv_to_crv (eptr,tfmat,cvid,tol,key,subid)
**       Evolve selected boundary curve of the trimmed surface, and
**       use points to generate rbspline curve as a new entity.
**    PARAMETERS
**       INPUT  :
**          eptr    - trimmed surface entity
**          tfmat   - identity matrix of surface
**          cvid    - id number of boundary curve to evolve
**          subid   - sub_id of composite boundary curve to evolve
**          tol     - tolerance used to evolve curve
**       OUTPUT :
**          key     - key of created B-spline
**    RETURNS      :
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************/
int uio_uvcrv_to_crv (eptr,tfmat,cvid,tol,key, subid)
struct NCL_trimsf_rec *eptr;
UM_transf tfmat;
UU_REAL tol;
int cvid, subid;
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
	status = um_bndr_curve (eptr,tfmat,cvid,-1,0,0,tol,&nbpt,&cvpts,
		&tangs);
	if (status != UU_SUCCESS) 
	{
		status = 472; goto Done;
	}

	if (subid <= 0 && nbpt != UU_NULL)
	{
		if (cvid == 0) *key = eptr->uv_key;
		else *key = eptr->ibndykey[2*cvid-1];
/*
.....create composite curve from segments
*/
		status = uio_gen_bndr_crv (tfmat,nbpt,&cvpts,&tangs,key);

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
**    E_FUNCTION     : uio_gen_bndr_crv (eptr,tfmat,nbpt,cvpts,key)
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
int uio_gen_bndr_crv (tfmat,nbpt,cvpts,tangs,key)	
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
		status = uio_c5_mergecrv1(&cvkey,keylst,tfmat,key);
	}

	if (keylst != UU_NULL) uu_free(keylst);

	return (status);
}

/*********************************************************************
**		W_FUNCTION	: uio_c5_mergecrv(cvkey, keylst, tfmat, key)
**       The entities to use in making the composite curve are
**       specified by UNIBASE keys. The curves are assumed to be
**       in order in the array KESY. A composite curve2 is
**       created in UNIBASE which has the same sturcture as composit 
**			curve1. 
**    PARAMETERS
**       INPUT  :
**          cvkey           key of composite curve 1
**          keylst          array of keys
**          tfmat           transformation matrix of comp crv
**       OUTPUT :
**          key             key of the new composite curve 2
**    RETURNS      : stat
**          stat = -1, iff procedure cannot complete
**                  0, otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_c5_mergecrv1(cvkey, keylst, tfmat, key)
UU_KEY_ID *cvkey,*keylst,*key;
   {
   struct UM_compcrv_rec *eptr1 = UU_NULL;
   struct UM_compcrv_rec *eptr2 = UU_NULL;
   struct UM_cid_rec *cidp = UU_NULL;
   int i,n,status;
   int dim;              /* dimension of space defined by comp. curve */
   UU_REAL space[2][3];  /* definition of the space defined by the comp. */

   NCL_copy_compcrv = UU_TRUE;
   eptr1 = (struct UM_compcrv_rec *)
                  uu_malloc(sizeof(struct UM_compcrv_rec));
   if (eptr1 == UU_NULL) goto done;
   eptr2 = (struct UM_compcrv_rec *)
                  uu_malloc(sizeof(struct UM_compcrv_rec));
   if (eptr2 == UU_NULL) goto done;

   eptr1->key = *cvkey;
   ncl_retrieve_data_fixed(eptr1);
   ur_setup_data(eptr1->rel_num, eptr2, sizeof(struct UM_compcrv_rec));
   strcpy (eptr2->label, "");
   eptr2->subscr = 0;
   eptr2->arclen = 0.0;
   eptr2->open = eptr1->open;
   eptr2->continuity = eptr1->continuity;
   eptr2->fcolor = eptr1->fcolor;

   n = eptr2->no_cid = eptr1->no_cid;
   cidp = (struct UM_cid_rec *) uu_malloc(n*sizeof(struct UM_cid_rec));
   if (cidp == UU_NULL) goto done;
   eptr2->cid = cidp;
   for (i=0; i<n; i++)
      {
      eptr2->cid[i].crvid = keylst[i];
      eptr2->cid[i].reverse = 0;
      eptr2->cid[i].endparam = 0.0;
      }
   umi_fix_subcurve_fields(eptr2, tfmat);
   uc_span_entity(eptr2, &dim, space);
   eptr2->planar = UU_FALSE;
   if (dim == 2) eptr2->planar = UU_TRUE;

   status = ncl_create_entity (eptr2, NCLI_CURVE);
   ncl_store_wf1(eptr2->key);
   ur_update_displayable(eptr2->key, UM_NEVERDISPLAYABLE);
   *key = eptr2->key;
done:;
   NCL_copy_compcrv = UU_FALSE;
   if (eptr1) uu_free(eptr1);
   if (eptr2) uu_free(eptr2);
   if (cidp)  uu_free(cidp);

   return (status);
   }


