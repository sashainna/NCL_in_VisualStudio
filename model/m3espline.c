/*********************************************************************
**    NAME         :  m3espline.c
**
**       CONTAINS: SSPLIN & SPLINE support routines.
**
**			int um_tboundary_to_sscomp()
**			int um_cp_struct_rbcv_uvcv(rbcv, nclkey, ssplin)
**			int um_cp_struct_line_uvcv(line, nclkey, ssplin)
**			int um_c5_mergecrv1(cvkey, keylst, key)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       m3espline.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/10/18 , 15:07:48
*********************************************************************/
#include "nccs.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "class.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"
#include "nclfc.h"
#include "ncl.h"
#include "mdpick.h"
#include "dasnog.h"
#include "dasg.h"


#define	UM_MAXFILLSEGS	100
#define	UM_MAXSPC	50		/* maximum number of segments drawn per curve	*/
#define	UM_MAX(A, B)	((A)>(B)?(A):(B))
#define	UM_MIN(A, B)	((A)<(B)?(A):(B))

#define UM_TRACE 0

extern UU_LOGICAL UM_set_constituent_pickids;

extern UU_LOGICAL NCL_create_compcrv;
extern UU_LOGICAL NCL_copy_compcrv;

/*********************************************************************
**    E_FUNCTION : int um_tboundary_to_sscomp(sfkey,eptr1,eptr2,isubid,isubid1,idir)
**       This function converts a composite curve on the boundary of a 
**			trimmed surface to a compostie surface spline and
**			stored it as a new entity.
**			Note all subcurves are copied as well.
**    PARAMETERS   
**       INPUT  : 
**			sfkey		: key of the trimmed surface.
**			eptr1		: pointer to the composite curve entity to be copied.
**			isubid		: start id of the sub_curve to be copied.
**			isubid1		: end id of the sub_curve to be copied.
**			idir		: CLW/CCLW direction from start isubid to end isubid1
**       OUTPUT :  
**          eptr2		pointer to the copy of the composite curve.
**    RETURNS      : 0 if no error. 
**    SIDE EFFECTS : puts copy in UNIBASE.
**    WARNINGS     : none
*********************************************************************/
int um_tboundary_to_sscomp(sfkey,eptr1,eptr2,isubid,isubid1,idir)
	UU_KEY_ID *sfkey;
	struct UM_compcrv_rec *eptr1;
	struct UC_entitydatabag *eptr2;
	int isubid, isubid1, idir;
{
	struct UC_entitydatabag *e1temp;	
	struct UC_entitydatabag *e2temp;
	UU_REAL	um_getcrvlen();

	int i, status,i3;
	UM_int2 i2;
	UU_KEY_ID bskey;
	UU_REAL length,length0;
	UM_transf tfmat;

	e1temp = (struct UC_entitydatabag *) 
		uu_malloc(sizeof(struct UC_entitydatabag));
	e2temp = (struct UC_entitydatabag *)
		uu_malloc(sizeof(struct UC_entitydatabag));
	bskey = *sfkey;

	if (isubid < 0 || (fabs(isubid1-isubid) >= 1 && isubid >= 0 && isubid1 >= 0))
	{
		struct UM_compcrv_rec *eptr3;
		eptr3 = (struct UM_compcrv_rec *) eptr2;
		NCL_copy_compcrv = UU_TRUE;
		ur_setup_data(eptr1->rel_num, eptr3, sizeof(struct UC_entitydatabag));
		strcpy (eptr3->label, "");
		eptr3->subscr = 0;
		eptr3->arclen = eptr1->arclen;
		eptr3->planar = eptr1->planar;
		eptr3->open = eptr1->open;
		eptr3->continuity = eptr1->continuity;
		eptr3->fcolor = eptr1->fcolor;

		if (isubid < 0 || (isubid == 0 && isubid1 == 0 && idir == 0))
		{
			isubid = 0;
			isubid1 = eptr1->no_cid;
			eptr3->no_cid = eptr1->no_cid;
		}

		i2 = 1;
		stunlb (&i2);
		i3 = 0;
		length = 0.0;
		length0 = 0.0;
		length0 = um_getcrvlen(eptr1,isubid,isubid1,idir);

		for (i = isubid; ; )
		{
			e1temp->key = eptr1->cid[i].crvid;
			uc_retrieve_data(e1temp, sizeof(struct UC_entitydatabag));
			if (e1temp->rel_num == UM_LINE_REL)
				status = um_cp_struct_line_uvcv(e1temp, &bskey, e2temp); 
			else if (e1temp->rel_num == UM_RBSPLCRV_REL)
				status = um_cp_struct_rbcv_uvcv (e1temp, &bskey, e2temp);
			else
				ud_wrerr("Unrecognized entity in Boundary Curve.");

			eptr3->cid[i3].crvid = e2temp->key;
			if (idir == -1)
				uc_reverse_curve(e2temp);
			eptr3->cid[i3].reverse = eptr1->cid[i].reverse;
			length += um_getarclen(e1temp, UM_DEFAULT_TF);
			eptr3->cid[i3].endparam = length / length0;
			if(status == UU_FAILURE) return(status);

			if (idir == 0)
			{
				i++;
				if (i >= isubid1) break;
			}
			else
			{					
				eptr3->no_cid = i3 + 1;
				if (i == isubid1) break;
				i = i + idir;
				if (i == eptr1->no_cid) i = 0;
				if (i < 0) i = eptr1->no_cid-1;
			}
			i3++;
		}                                      
		stunlb (&i2);
		eptr3->closdinu = eptr1->closdinu;

		status = ncl_create_entity (eptr3, NCLI_CURVE);
		ncl_store_wf1(eptr3->key);
		ur_update_displayable(eptr3->key, UM_NEVERDISPLAYABLE);
		eptr3->t0 = eptr1->t0;
		eptr3->t1 = eptr1->t1;
		eptr3->addflg = eptr1->addflg;
		ur_update_data_fixed (eptr3);
		ncl_retrieve_data_fixed(eptr3);
		uc_retrieve_transf(eptr1->key, tfmat);
//		umi_fix_subcurve_fields(eptr3, tfmat);
		NCL_copy_compcrv = UU_FALSE;
	}
	else
	{
		if ((isubid >= 0) && (isubid <= eptr1->no_cid))
		{
			e1temp->key = eptr1->cid[isubid].crvid;
			uc_retrieve_data(e1temp, sizeof(struct UC_entitydatabag));
			if (e1temp->rel_num == UM_LINE_REL)
				status = um_cp_struct_line_uvcv(e1temp, &bskey, eptr2);
			else if (e1temp->rel_num == UM_RBSPLCRV_REL)
				status = um_cp_struct_rbcv_uvcv (e1temp, &bskey, eptr2);
			else
				ud_wrerr("Unrecognized entity in Boundary Curve.");
		}
		else
			ud_wrerr("Invalid sub_boudary_curve_id.");
	}

	uu_free(e1temp);
	uu_free(e2temp);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  int um_cp_struct_rbcv_uvcv (rbcv,sfkey,ssplin)
**       Create a surface spline curve on surface from a rational bspline curve
**       The new structure has converted pt in uv space and gets the basekey
**  	 	of the trimimed surface as its own bskey.
**
**    PARAMETERS
**       INPUT  :
**          rbcv           rational bspline curve struct 
**          sfkey          surface key
**       OUTPUT :
**          ssplin         rational surface spline curve struct of uvonsf
**    RETURNS      :
**          0              no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
int
um_cp_struct_rbcv_uvcv (rbcv,sfkey,ssplin)
   struct UM_rbsplcrv_rec *rbcv;
   struct UM_uvcvonsf_rec *ssplin;
   UU_KEY_ID *sfkey;
{
	int i,status,j,itis, nb;
	struct NCL_fixed_databag eptrsf, bs, *bsptr;
	UU_REAL bplm[4], dup[2], *pt;
	UU_KEY_ID bsfkey;

	bsfkey = NULLKEY;
	status = UU_SUCCESS;

	eptrsf.key  = *sfkey;
	status = ncl_retrieve_data_fixed (&eptrsf);
	if (status == UU_SUCCESS)
	{
		if (ncl_itsa_trimsrf (&eptrsf))
		{
			itis = 1;
			nb = 1;
			ncl_trimsrf_get_fixed (&eptrsf,&nb,bplm);

			bsptr  = (struct NCL_fixed_databag *) &bs;
			status = ncl_trimsrf_get_bs (&eptrsf,&bsptr);
			bsfkey = bsptr->key;
		}
		else
		{
			bplm[0] = 0.0;
			bplm[1] = 1.0;
			bplm[2] = 0.0;	
			bplm[3] = 1.0;	
			bsfkey = *sfkey;
		}
	}

	ur_setup_data(UM_UVCVONSF_REL,ssplin,sizeof(struct NCL_fixed_databag));
	status = ncl_create_entity(ssplin, NCLI_CURVE);
	ncl_store_wf1(ssplin->key);

	strcpy (ssplin->label, "");
	ssplin->subscr = 0;
	ssplin->planar = rbcv->planar;
	ssplin->open = rbcv->open;
	ssplin->closdinu = rbcv->closdinu;
	ssplin->no_t = rbcv->no_t;
	ssplin->no_pt = rbcv->no_pt;
	ssplin->no_wt = rbcv->no_wt;

	pt = (UU_REAL *) uu_malloc(3*(rbcv->no_pt)*sizeof(UU_REAL));
	if (status == UU_SUCCESS)
	{
		dup[0] = bplm[1] - bplm[0];
		dup[1] = bplm[3] - bplm[2];
		if (dup[0] <= 0.0 || dup[1] <= 0.0)
			return (UU_FAILURE);
		else
		{  
			for (j=0; j<3*(rbcv->no_pt); j=j+3)
			{
				for (i=0; i<2; i++)
				{
					pt[j+i] = (rbcv->pt[j+i] - bplm[2*i]) / dup[i];
				}
				pt[j+2] = 0.;
			}			
		}
	}

   status = ur_update_data_varlist (ssplin->key, 1, rbcv->t, 1,rbcv->no_t);
   status = ur_update_data_varlist (ssplin->key, 2, pt, 1,rbcv->no_pt);
   status = ur_update_data_varlist (ssplin->key, 3, rbcv->wt, 1,rbcv->no_wt);
	status = ncl_retrieve_data (ssplin, um_ssplin_size(rbcv));
   if (status == UU_SUCCESS)
   {
      ssplin->k = rbcv->k;
      ssplin->n = rbcv->n;
      ssplin->t0 = rbcv->t0;
      ssplin->t1 = rbcv->t1;
		ssplin->bskey = bsfkey;
/*
.....why update as NO LABLE? this will cause the entity can't show label
.....Yurong
//?		strcpy (ssplin->label, "");
*/
      status = ur_update_data_fixed (ssplin);
	  if (status == UU_SUCCESS)
	  {
		  status = ncl_update_sskey (&(bsfkey),ssplin->key,1);
		  ur_update_displayable(ssplin->key, UM_NEVERDISPLAYABLE);
	  }
   }
	uu_free(pt);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  int um_cp_struct_line_uvcv (line,sfkey,ssplin)
**       Create a surface spline curve on surface from a line.
**       The new structure has converted pt in uv space and gets the 
**			basekey of the trim surface as its own bskey.
**
**    PARAMETERS
**       INPUT  :
**          line           line struct 
**          sfkey          surface key
**       OUTPUT :
**          ssplin         rational surface spline curve struct of uvonsf
**    RETURNS      :
**          0              no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
int
um_cp_struct_line_uvcv (line, sfkey, ssplin)
	struct UM_line_rec *line;
	struct UM_uvcvonsf_rec *ssplin;
	UU_KEY_ID *sfkey;
	{
   int i, status, j,itis, nb;
	UU_REAL t[4],pt[6], bplm[4], dup[2];
   struct NCL_fixed_databag eptrsf, bs, *bsptr;

   status = UU_SUCCESS;

   eptrsf.key  = *sfkey;
   status = ncl_retrieve_data_fixed (&eptrsf);
   if (status == UU_SUCCESS)
   {
     if (ncl_itsa_trimsrf (&eptrsf))
     {
        itis = 1;
        nb = 1;
        ncl_trimsrf_get_fixed (&eptrsf,&nb,bplm);

        bsptr  = (struct NCL_fixed_databag *) &bs;
        status = ncl_trimsrf_get_bs (&eptrsf,&bsptr);
     }
  	}

   ur_setup_data(UM_UVCVONSF_REL, ssplin, sizeof(struct NCL_fixed_databag));
   status = ncl_create_entity(ssplin, NCLI_CURVE);
   ncl_store_wf1(ssplin->key);

	strcpy (ssplin->label, "");
  	ssplin->planar = 0;
  	ssplin->open = 0;
	ssplin->closdinu = 0;
  	ssplin->no_t = 4;
	ssplin->no_pt = 2;
  	ssplin->no_wt = 2;

   t[0] = 0.0;
   t[1] = 0.0;
   t[2] = 1.0;
   t[3] = 1.0;

  	um_vctovc(&line->spt[0],&pt[0]);
  	um_vctovc(&line->ept[0],&pt[3]);

   if (status == UU_SUCCESS)
   {
      dup[0] = bplm[1] - bplm[0];
      dup[1] = bplm[3] - bplm[2];
/*
      if (dup[0] != 1.0 || dup[1] != 1.0)
..... wrong if, e.g., bplm[0]=4, bplm[1]=5, bplm[2]=2, bplm[3]=3
*/
      if (dup[0] <= 0.0 || dup[1] <= 0.0)
			return (UU_FAILURE);
      for (j=0; j<3*(ssplin->no_pt); j=j+3)
      {
         for (i=0; i<2; i++)
         {
           pt[j+i] = (pt[j+i] - bplm[2*i]) / dup[i];
         }
			pt[j+2] = 0.;
      }
   }

   status = ur_update_data_varlist (ssplin->key, 1, t, 1, ssplin->no_t);
   status = ur_update_data_varlist (ssplin->key, 2, pt, 1, ssplin->no_pt);
   for (i=0; i<2; i++)
      t[i] = 1.0;
   status = ur_update_data_varlist (ssplin->key, 3, t, 1, ssplin->no_wt);
   status = ncl_retrieve_data (ssplin,um_ssplin_size(line) );

   if (status == UU_SUCCESS)
   {
      ssplin->k = 2;
      ssplin->n = 1;
      ssplin->t0 = 0.0;
      ssplin->t1 = 1.0;
		ssplin->bskey = bsptr->key;
		strcpy (ssplin->label, "");
		status = ur_update_data_fixed (ssplin);
      if (status == UU_SUCCESS)
      {
         status = ncl_update_sskey (&(bsptr->key),ssplin->key,1);
		ur_update_displayable(ssplin->key, UM_NEVERDISPLAYABLE);
      }
   }
	return (status);
}
/*********************************************************************
**		W_FUNCTION	: um_c5_mergecrv(cvkey, keylst, tfmat, ncv,key)
**       The entities to use in making the composite curve are
**       specified by UNIBASE keys. The curves are assumed to be
**       in order in the array KESY. A composite curve2 is
**       created in UNIBASE which has the same sturcture as composit 
**			curve1. 
**    PARAMETERS
**       INPUT  :
**			cvkey		: key of composite curve 1
**			keylst		: array of keys
**          tfmat		: transformation matrix of comp crv
**			ncv			: numbers of sub-curves of composite curve to merge	
**       OUTPUT :
**			key			: key of the new composite curve 2
**    RETURNS      : stat
**          stat = -1, iff procedure cannot complete
**                  0, otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c5_mergecrv1(cvkey, keylst, tfmat, ncv, key)
UU_KEY_ID *cvkey,*keylst,*key;
UM_transf tfmat;
int ncv;
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
/*
.....Initialized key to 0 to avoid UMR in ncl_create_entity.
*/
   eptr2->key = 0;
   ncl_retrieve_data_fixed(eptr1);
   ur_setup_data(eptr1->rel_num, eptr2, sizeof(struct UM_compcrv_rec));
   strcpy (eptr2->label, "");
   eptr2->subscr = 0;
   eptr2->arclen = 0.0;
   eptr2->open = eptr1->open;
   eptr2->continuity = eptr1->continuity;
   eptr2->fcolor = eptr1->fcolor;
   eptr2->t0 = eptr1->t0;
   eptr2->t1 = eptr1->t1;
	eptr2->addflg = eptr1->addflg;

   n = eptr2->no_cid = ncv;

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
   crvcls(key);
done:;
   NCL_copy_compcrv = UU_FALSE;
   if (eptr1) uu_free(eptr1);
   if (eptr2) uu_free(eptr2);
   if (cidp)  uu_free(cidp);

   return (status);
   }

