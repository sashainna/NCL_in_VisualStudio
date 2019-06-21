/*********************************************************************
**    NAME         :  netrimsf.c
**       CONTAINS: Fortran interface, display, and evaluator routines
**                 for trimmed surfaces which are:
**
**           int ptrmsf
**           int ncl_create_sspline
**           int gttbse
**           int trmext
**           int ncl_disp_trimsf
**           int ncl_proj_trimsf
**           int ncl_d_trimsf
**           int ncl_eval_trimsf
**           int ncl_eval_trimsfp
**           int ncl_eval_trimnrm
**           int ncl_trans_trimsf
**           int ncl_transform_evsfout
**           int ncl_class_copy_trimsf
**           int ncl_copy_trimsf
**           int ncl_offset_trimsf
**           int ncl_trimsf_query
**           int bspdfp
**           int ncl_mov
**           int ncl_lin
**           int ncl_gdraw
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       netrimsf.c , 25.3
**    DATE AND TIME OF LAST MODIFICATION
**       10/19/15 , 17:24:57
*********************************************************************/
#include "udebug.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "ncl.h"
#include "nclx.h"
#include "nclfc.h"
#include "nccs.h"
#include "ag_constant.h"
#include "ag_curves.h"
#include "ag_global.h"
#include "mattr.h"
#include "mdattr.h"
#include "mgeom.h"
#include "modef.h"
#include "umath.h"
#include "ulist.h"
#include "class.h"  /* for UC_attributedatabag */
#include "uminmax.h"
#include "ginq.h"
#include "nclvx.h"
#include "ngeom.h"
#include "ustdio.h"

#define NCL_MAXPTS 70

static int NCL_npts = 0;
static UM_coord NCL_dpts[NCL_MAXPTS];

extern UU_KEY_ID NCL_nokey ;
extern int nclu_temp_redef;

/*********************************************************************
**    E_FUNCTION     : int ncl_put_key (nclkey)
**       Push a key onto the NCL_key_list list.
**    PARAMETERS
**       INPUT  :
**          nclkey    - Key to push.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_put_key (nclkey)
UM_int4 *nclkey;
{
  UU_KEY_ID key;

  key = *nclkey;
  ncl_add_listkey1 (1,key);

  return (UU_SUCCESS);
}

/*********************************************************************
*********************************************************************/
void ncl_trmsf_init (sf)
struct NCL_trimsf_rec *sf;
{
/* initialize key to zero to force create */
	sf->key = 0;

	sf->closdinu = sf->closdinv = 0;
	sf->offdist = 0.0;
	sf->cv_key = sf->uv_key = sf->bs_key = 0;
	sf->ub_min = sf->vb_min = sf->u_min = sf->v_min = 0.0;
	sf->ub_max = sf->vb_max = sf->u_max = sf->v_max = 1.0;
	sf->drive_type = 0;
	sf->no_ibndykey = 0;
	sf->ibndykey = UU_NULL;
}

/*********************************************************************
**    E_FUNCTION     : int ptrmsf (ksf, kcv, ncvs, uv, nclkey, ierr)
**       Creates a NCL trimmed surface in UNIBASE and return the
**       unibase key of the surface. Fortran interface routine.
**    PARAMETERS
**       INPUT  :
**          ksf       - Key of base surface.
**          kcv       - Key of outer boundary curve or zero if none.
**          ncvs      - number of inner xyz trimming curves.
**          uv        - Starting surface u,v
**       OUTPUT :
**          nclkey    - Key of trimmed surface.
**          ierr      - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptrmsf (ksf, kcv, ncvs, uv, nclkey, ierr)
   UM_int4 *ksf, *kcv, *nclkey;
   UM_int2 *ncvs, *ierr;
   UM_real4 uv[2];
   {
   UM_int2 lfl_77, jerr=0, itsk;
   int i, j, n, ncv, cvtyp;
   int status, rel_num, shaded, lucency, hldmax, hldix = 0;
   UU_KEY_ID kss, lkey, *bkey=UU_NULL, *hldkey=UU_NULL, skey, ckey;
   UU_KEY_ID kuv[4];
   UU_LOGICAL sskey, untrimmed;
   struct NCL_trimsf_rec surf;
   struct NCL_fixed_databag s1, s2, s3;
   struct UM_transf_rec btran, stran;
   UM_int2 primtyp = 0, idx;
   nclsf_prim_type typ = NCLSF_UNKNOWN;
   UM_real8 primdata[16];
	UM_transf tfmat;
	struct UM_evcrvout evout;
	static UM_real8 s[4], tol8, uvlim[4] = {0.0,1.0,1.0,0.0};
	UU_REAL told;
	UM_coord *pptr;
	UM_vector *vptr;
	static UM_coord upts[5] = {{0.0,0.0,0.0},
		{1.0,0.0,0.0},
		{1.0,1.0,0.0},
		{0.0,1.0,0.0},
		{0.0,0.0,0.0}};
	UU_LIST ptlst, velst, talst, seglst;
	struct NCL_crvgen_rec seg, *segp;

   ckey = 0;
   sskey = untrimmed = UU_FALSE;
   *nclkey = 0;
   ncv = *ncvs;
   ur_setup_data(NCL_TRIMSF_REL, &surf, sizeof(surf));
   ncl_trmsf_init (&surf);

   status = UU_SUCCESS;
   /* set ranfile label flag to temp unknown */
   lfl_77 = 1;
   stunlb (&lfl_77);
   hldmax = 2*ncv+2;
   hldkey = (UU_KEY_ID *)uu_malloc(hldmax*sizeof(UU_KEY_ID));
   if (!hldkey) hldmax = 0;
/*
.....Check for base surface or plane. If the key is zero, treat it as
.....a plane so that ncl_plane_to_sf() will convert bounding curve into
.....a plane
*/
   skey = *ksf;
   if (skey == 0)
     rel_num = NCL_PLN_REL;
   else
      status = ur_retrieve_data_relnum (skey, &rel_num);
   if (status != UU_SUCCESS) goto Err;
   switch (rel_num)
   {
      case UM_AGSRF_REL:
      case UM_RBSPLSRF_REL:;
      case NCL_SURF_REL:
      case NCL_MESHSURF_REL:
      case NCL_EVALSF_REL:
      case NCL_TRIMSF_REL:
      case NCL_REVSURF_REL:
/*
....Get and copy base surface.
*/
         s1.key = skey;
         status = ncl_retrieve_data_fixed (&s1);
         if (status != UU_SUCCESS) goto Err;
         if (s1.rel_num == NCL_TRIMSF_REL)
         {
           struct NCL_trimsf_rec *tsfp;
           tsfp = (struct NCL_trimsf_rec *)&s1;
           s1.key = tsfp->bs_key;
           if (*kcv == 0)
           {
             ncv++;
             ckey = tsfp->cv_key;
             if (tsfp->uv_key > 0)
             {
               s2.key = tsfp->uv_key;
               status = ncl_retrieve_data_fixed (&s2);
               if (status != UU_SUCCESS) goto Err;
               status = uc_copy (&s2, &s3, sizeof(struct NCL_fixed_databag));
               if (status != UU_SUCCESS) goto Err;
               lkey = s3.key;
               sskey =  UU_TRUE;
             }
           }
           status = ncl_retrieve_data_fixed (&s1);
           if (status != UU_SUCCESS) goto Err;
         }
			else if (*kcv == 0)
			{
				untrimmed = UU_TRUE;
				lkey = 0;
				ncv++;
				n=2;
				for (i=0;i<4;i++) kuv[i] = 0;
				for (i=0;i<4 && status==UU_SUCCESS;i++)
				{
					s[0] = s[1] = 0.0;
					s[2] = s[3] = 1.0;
					status = ncl_create_ssplin(&skey,s,&upts[i],&n,&kuv[i]);
				}
				if (status==UU_SUCCESS)
				{
					n=4;
					ncl_create_uvcomp  (n, kuv, &lkey);
				}
				for (i=0;i<4;i++)
				{
					if (kuv[i]) uc_delete(kuv[i]);
					kuv[i] = 0;
				}
				ckey = 0;
				if (status==UU_SUCCESS)
				{
					s1.key = skey;
					status = ncl_retrieve_data_fixed (&s1);
				}
				if (status == UU_SUCCESS)
					status = uc_retrieve_transf(s1.key, tfmat);

				if (status==UU_SUCCESS)
				{
					s[0] = 0.0;
					s[1] = 1.0;
					idx = 175;
					getsc (&idx,&tol8);
					told = tol8;
					uu_list_init(&ptlst,sizeof(UM_coord),100,100);
					uu_list_init(&velst,sizeof(UM_coord),100,100);
					uu_list_init(&talst,sizeof(UM_real8),100,100);
					for (i=0;i<4 && status==UU_SUCCESS;i++)
					{
						cvtyp = i % 2 + 1;
						n = ncl_evolve_crv_on_srf
							(&s1,tfmat,uvlim[i],s,cvtyp,told,&ptlst,&velst,&talst);
						if (n<=0) status=UU_FAILURE;
						if (status==UU_SUCCESS)
						{
							status = ncl_fix_evol
								(1,&s1,tfmat,&evout,&n,told,&ptlst,&velst,&talst);
						}
						pptr = (UM_coord *)UU_LIST_ARRAY(&ptlst);
						vptr = (UM_coord *)UU_LIST_ARRAY(&velst);
						uu_list_init (&seglst, sizeof(struct NCL_crvgen_rec), n, n);
						for (j=0;j<n && status==UU_SUCCESS;j++)
						{
							seg.x = pptr[j][0];
							seg.y = pptr[j][1];
							seg.z = pptr[j][2];
							seg.a = vptr[j][0];
							seg.b = vptr[j][1];
							seg.c = vptr[j][2];
							seg.inv = 1;
							uu_list_push(&seglst, &seg);
						}
						segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglst);
						if (status == UU_SUCCESS)
							status = ncl_interp_rbsp (n, segp, 0, &s2);
						if (status == UU_SUCCESS)
							kuv[i] = s2.key;

						UU_LIST_EMPTY(&ptlst);
						UU_LIST_EMPTY(&velst);
						UU_LIST_EMPTY(&talst);
						uu_list_free(&seglst);
					}
					if (status==UU_SUCCESS)
					{
						n=4;
						ncl_create_uvcomp  (n, kuv, &ckey);
					}
					uu_list_free(&ptlst);
					uu_list_free(&velst);
					uu_list_free(&talst);
					for (i=0;i<4;i++)
						if (kuv[i]) uc_delete(kuv[i]);
				}
				sskey =  UU_TRUE;
			}
         ncl_retrieve_shaded  (&s1, &shaded);
         ncl_retrieve_lucency (&s1, &lucency);
      
         gettol(&tol8);
         told = tol8;
         ncl_get_sf_treflag (&s1,&NCL_triansf,told);

         status = uc_copy (&s1, &s2, sizeof(struct NCL_fixed_databag));
         if (status != UU_SUCCESS) goto Err;

         status = ncl_get_sf_primdat(&skey,&primtyp,primdata);
         if (status != UU_SUCCESS || primtyp < 0 || primtyp > 6)
            status = UU_SUCCESS;
         else 
            typ = (nclsf_prim_type) primtyp;
         
         ncl_store_prim_data(&s2,typ,primdata);
         break;
      case NCL_PLN_REL:
/*
.....Convert plane (or planar bounding curve) into a surface.
*/
         s1.key = skey;
         if (skey > 0)
            status = ncl_retrieve_data_fixed(&s1);
         if (status != UU_SUCCESS) goto Err;
         s3.key = *kcv;
         status = ncl_retrieve_data_fixed(&s3);
         if (status != UU_SUCCESS) goto Err;
         status = ncl_plane_to_sf (&s1, &s3, &s2);
         if (status != UU_SUCCESS) goto Err;
         skey = s2.key;
         break;
      default:
         status = UU_FAILURE;
         goto Err;
   }
/*
.....Save base surface key.
*/
   if (status == UU_SUCCESS)
   {
     ur_update_displayable(s2.key, UM_NEVERDISPLAYABLE);
     surf.bs_key = s2.key;
     if (hldix < hldmax) hldkey[hldix++] = s2.key;
     if (ncv > 1)
       bkey = (UU_KEY_ID *)uu_malloc(2*ncv*sizeof(UU_KEY_ID));
   }
/*
.....Determine curve types, create composite uv curve from composite xyz curve,
.....B-Spline from other curves.
*/
   for (j=0;j<ncv && status == UU_SUCCESS;j++)
   {
     jerr = 1;
     itsk = 22;
     if (j == 0)
     {
       if (ckey == 0) ckey = *kcv;
     }
     else
       status = ncl_get_listkey (1, j-1, &ckey);
     if (status == UU_SUCCESS)
     {
       if (sskey)
         ur_update_displayable(lkey, UM_NEVERDISPLAYABLE);
       else
         status = ncl_trimsf_uvkey (ckey,skey,uv,&lkey,&jerr);
     }
     if (status != UU_SUCCESS) goto Err;
/*
.....Save uv curve key
*/
     if (j == 0)
       surf.uv_key = lkey;
     else
     {
       bkey[j*2-2] = 0;
       bkey[j*2-1] = lkey;
     }
     if (hldix < hldmax) hldkey[hldix++] = lkey;
     if (ckey)
     {
/*
.....Save XYZ curve. Project to surface if necessary.
*/
        status = ncl_trimsf_cvkey (ckey,skey,uv,&kss,jerr);
        if (status == UU_SUCCESS)
        {
          if (j == 0)
            surf.cv_key = kss;
          else
            bkey[j*2-2] = kss;
          if (hldix < hldmax) hldkey[hldix++] = kss;
        }
     }
		if (untrimmed)
		{
			if (ckey) uc_delete(ckey);
			untrimmed = UU_FALSE;
		}
     sskey = UU_FALSE;
   }
/*
.....Reset ranfile label flag.
*/
   stunlb (&lfl_77);

   if (status == UU_SUCCESS)
     {
/*
.....Get the base surface transformation 
*/
     btran.key = surf.bs_key;
     btran.rel_num = UM_TRANSFORM_REL;
     if (ur_retrieve_transf(&btran) != 0) status = UU_FAILURE;
     }
/*
.....Create trimmed surface entity and its transform.
*/
   if (status == UU_SUCCESS)
   {
      status = ncl_create_entity(&surf, 9);
   }
   if (ncv > 1 && status == UU_SUCCESS)
   {
     status = ur_update_data_varlist (surf.key, 1, bkey, 1, (ncv-1)*2);
   }
   if (status == UU_SUCCESS)
   {
      stran.key = surf.key;
      stran.rel_num = UM_TRANSFORM_REL;
      if (ur_retrieve_transf(&stran) == 0)
      {
         um_tftmtf(stran.tfmat, btran.tfmat, stran.tfmat);
         if (ur_update_transf (&stran) != 0) status = UU_FAILURE;
      }
   }
   goto Done;

Err:
   stunlb (&lfl_77);

Done:;
   if (status == UU_SUCCESS)
	{
     *nclkey = surf.key;
/*
.....If this is a temp surface created by ncl_show_redef , store the key to
..... to delete it later.
*/
		if(nclu_temp_redef)
		{
/*
.....set the color of the temp surface to Sea Green
*/
			ncl_update_color(surf.key,NCLX_SEA_GREEN);
			uc_display (&surf);
			NCL_nokey = surf.key;
			ur_update_selectable (NCL_nokey,UU_FALSE);
		}
	}
   else
   {
     if (hldkey)
       for (i=0;i<hldix;i++)
         if (hldkey[i]) uc_delete(hldkey[i]);
   }
   if (bkey) uu_free(bkey);
   if (hldkey) uu_free(hldkey);
   ncl_free_keylist(1);
   if (jerr>1) *ierr = jerr;
   if (status != UU_SUCCESS && *ierr <= 0) *ierr = 163;
	
   NCL_triansf = 0;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_create_sspline (itsk, ksf, kcv, uv, kss, ierr)
**       Create a surface uv curve from an xyz curve.
**    PARAMETERS
**       INPUT  :
**          itsk       =1, create xyz cv, =2 create spsline,
**                     =22 create sspline with distance check
**          ksf        Key of surface.
**          kcv        Key of xyz curve.
**          uv         Starting surface u,v.
**       OUTPUT :
**          kss        Key of uv curve.
**          ierr       error key if > 1,
**                     curve is on surface within tolerance if = 1,
**                     0 else.
**    RETURNS      :
**       UU_SUCCESS iff no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_sspline  (itsk, ksf, kcv, uv, kss, ierr)
   UU_KEY_ID ksf, kcv, *kss;
   UM_real4 uv[2];
   UM_int2 itsk, *ierr;
   {
   int status;
   UM_real8 tol,dum[3],uva[2];
   UM_int2 idum,iwrap;
	UM_int4 jkey;

   status = UU_SUCCESS;
	*kss = 0;
   gettol(&tol);
   idum =  0;
	jkey = 0;
	dum[0] = dum[1] = dum[2] = 0.;
   iwrap = 0;
	uva[0] = uv[0]; uva[1] = uv[1];
   nclf_cv_project_sf(&itsk,&kcv,&ksf,uva,&tol,&iwrap,&idum,&idum,dum,
      &jkey,&jkey,&jkey,uva,&jkey,kss,ierr);
	uv[0] = uva[0]; uv[1] = uva[1];
   if (*kss <= 0 || *ierr > 1) status = UU_FAILURE;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_create_uvcomp (ksf, kcv, uv, kss, ierr)
**       Create a surface composite uv curve from a list of uv curves.
**    PARAMETERS
**       INPUT  :
**          ncv        Number of uv curves
**          kcv        Keys of uv curves.
**       OUTPUT :
**          kss        Key of uv curve.
**    RETURNS      :
**       UU_SUCCESS iff no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_create_uvcomp  (ncv, kcv, kss)
UU_KEY_ID *kcv, *kss;
int ncv;
{
	int i, status = UU_SUCCESS;
	UU_KEY_ID *keyp;
	struct UM_compcrv_rec *ccvp;
	struct UM_uvcvonsf_rec *cuvp;
	struct UM_rbsplcrv_rec *crbp;
	struct NCL_fixed_databag s1, s2, s3;

	*kss = 0;
	ccvp = (struct UM_compcrv_rec *)&s1;
	cuvp = (struct UM_uvcvonsf_rec *)&s2;
	crbp = (struct UM_rbsplcrv_rec *)&s2;
	keyp = (UU_KEY_ID *)uu_malloc(ncv*sizeof(UU_KEY_ID));
	if (!keyp) status = UU_FAILURE;

	for (i=0;i<ncv && status==UU_SUCCESS;i++)
	{
		keyp[i] = 0;
		cuvp->key = kcv[i];
		status = ncl_retrieve_data_fixed(&s2);
		if (status == UU_SUCCESS)
		{
			s3.key = 0;
			if (s2.rel_num == UM_UVCVONSF_REL)
				status = ncl_create_rbsp (cuvp->no_pt,cuvp->t,cuvp->pt,&s3);
			else
				status = ncl_create_rbsp (crbp->no_pt,crbp->t,crbp->pt,&s3);
			if (status == UU_SUCCESS)
				keyp[i] = s3.key;
		}
	}
	if (status == UU_SUCCESS)
	{
		ccvp->key = 0;
		status = um_c5_mergecrv (ncv, keyp, ccvp);
	}
	if (status == UU_SUCCESS)
	{
		status = uc_create_mtuple_data (ccvp,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	}

	if (status == UU_SUCCESS)
		*kss = ccvp->key;

	if (keyp)
	{
		for (i=0;i<ncv;i++) 
			if (keyp[i]) uc_delete(keyp[i]);
		uu_free(keyp);
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int gttbse (ksf, nclkey)
**       Return the base surface key of a trimmed sf.
**    PARAMETERS
**       INPUT  :
**          ksf        Key of trimmed surface.
**       OUTPUT :
**          nclkey     Key of base surface.
**    RETURNS      :
**       UU_SUCCESS iff no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gttbse  (ksf, nclkey)
   UM_int4 *ksf, *nclkey;
   {
   int status;
   struct NCL_trimsf_rec tsf;

   uu_denter(UU_MTRC,(us,"gttbse(kbuf=%x, uvbuf=%x)",kbuf, uvbuf));

   *nclkey = 0;
   tsf.key = *ksf;
   status = ncl_retrieve_data_fixed (&tsf);
   if (status == UU_SUCCESS) *nclkey = tsf.bs_key;

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int trmext (ksf, nclkey)
**       Extract the base surface from a trimmed sf.
**    PARAMETERS
**       INPUT  :
**          ksf        Key of trimmed surface.
**       OUTPUT :
**          nclkey     Key of extracted surface.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
trmext (ksf, nclkey)
   UM_int4 *ksf, *nclkey;
   {
   int status;
   struct NCL_trimsf_rec tsf;
   struct NCL_fixed_databag s1, s2;
   UM_transf tfmat;
	struct  UC_attributedatabag attr1;
   UU_LOGICAL um_is_idmat();

   uu_denter(UU_MTRC,(us,"trmext(ksf=%d, nclkey=%x)",*ksf, nclkey));

   *nclkey = 0;
   tsf.key = *ksf;
   status = ncl_retrieve_data_fixed (&tsf);

   if (status == UU_SUCCESS)
     {
     s1.key = tsf.bs_key;
     status = ncl_retrieve_data_fixed (&s1);
     }
/*
.....Do attr and transf retrieval before copying sf1 to sf2 to avoid an
.....error when the base surface needs to have the same name as the trimmed sf
*/
   if (status == UU_SUCCESS)
      status = uc_retrieve_transf(tsf.key, tfmat);
   if (status == UU_SUCCESS)
	  status =uc_retrieve_attr(tsf.key, &attr1);
   if (status == UU_SUCCESS)
      status = uc_copy (&s1, &s2, sizeof(struct NCL_fixed_databag));
   if (status == UU_SUCCESS && !um_is_idmat(tfmat))
      status = uc_transform (&s2, tfmat, (UU_LOGICAL) UU_TRUE);
   if (status == UU_SUCCESS) 
	{
		*nclkey = s2.key;
/*
.....vp 11/20/96: make sure that attributes of output surface match
.....attributes of trimmed SF (NOT base surface!)
*/
		attr1.key = s2.key;
		ur_update_attr(&attr1);
	}
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_disp_trimsf (eptr, tfmat, attptr)
**       Display a trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          eptr       - ptr to surface
**          tfmat      - transformation
**          attptr     - ptr to attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_disp_trimsf96 (eptr, tfmat, attrptr)
   struct NCL_trimsf_rec *eptr;
   UM_transf tfmat;
   struct UC_attributedatabag *attrptr;

   {
   UM_coord tmppt;
   UM_vector tmpve;
   UU_LIST tmplst;
   int status;

   uu_denter(UU_MTRC,(us,"ncl_disp_trimsf(key=%x,tfmat=%x,attrptr=%x)",
      eptr->key, tfmat, attrptr));

   status = ncl_d_trimsf (0, eptr, tfmat, attrptr, tfmat, 
                                               tmppt, tmpve, &tmplst);
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_proj_trimsf (eptr, tfmat, attptr,
**                             drwmat, vrefpt, vnvec, klist, rndr)
**       Project a trimmed surface to a drawing.
**    PARAMETERS   
**       INPUT  : 
**          eptr       - ptr to surface
**          tfmat      - transformation
**          attptr     - ptr to attribute bundle
**          drwmat     - Drawing transformation matrix.
**          vrefpt     - View reference point.
**          vpnorm     - View normal.
**          rndr       - UU_TRUE => render solids
**       OUTPUT :  
**          klist      - keys of entities projected onto drawing.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_proj_trimsf (eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm, klist, rndr)
   struct NCL_trimsf_rec *eptr;
   UM_transf tfmat;
   struct UC_attributedatabag *attrptr;
   UM_transf drwmat;
   UM_coord vrefpt;
   UM_vector vpnorm;
   UU_LIST *klist;
   UU_LOGICAL rndr;
   {
   int status;
   UM_int2 lfl_77;

   uu_denter(UU_MTRC,(us,"ncl_proj_trimsf (key=%x, tfmat=%x, attrptr=%x)",
      eptr->key, tfmat, attrptr));

   lfl_77 = 1;
   stunlb (&lfl_77);
   status = ncl_d_trimsf (1, eptr, tfmat, attrptr,drwmat,vrefpt,vpnorm,klist);
   stunlb (&lfl_77);

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_d_trimsf (iflg, eptr, tfmat, attptr,
**                            df, pt, ve, kl, rndr)
**       Display a trimmed surface or project it to a drawing.
**    PARAMETERS   
**       INPUT  : 
**          iflg       - 0 = display, 1 = project to drawing
**          eptr       - ptr to surface
**          tfmat      - transformation
**          attptr     - ptr to attribute bundle
**          df         - drawing transformation.
**          pt         - view plane reference point.
**          ve         - view plane normal vector.
**       OUTPUT :  
**          kl         - keys of entities projected if drawing.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_d_trimsf (iflg,eptr,tfmat,attrptr,df,pt,ve,kl)
int iflg;
struct NCL_trimsf_rec *eptr;
UM_transf tfmat;
struct UM_surfattr_rec *attrptr;
UM_transf df;
UM_coord pt;
UM_vector ve;
UU_LIST *kl;
{
   UU_REAL u,v,du,dv,umax,umin,vmax,vmin;
   UU_REAL ux,vx,uh,vh,told;
   int status,i,j,k,nu,nv,ldsp;
   struct NCL_fixed_databag bs;
   struct UM_evsrfout *evsrf;
   struct NCL_uvconv cvlist;
   UM_int2 idx;
   UU_LIST iobuf, cvus;
   int savelabel;
   int n, ns, js, je, nu1, nv1, ix, n1, n2;
   UU_REAL ur[2], vr[2];
   UM_coord *bnpt,*tp;
   UM_2Dcoord *uvpd;
   UM_param prev, cur;
   UU_KEY_ID key;
   UM_transf tf0;
   struct NCL_fixed_databag cv;
   UM_srf_boundary bound;
   UU_REAL *lstp;
   int lstix,ndisp,shaded,ecolor,nup,nvp,color;
   int cur_line_style,sh_mode,bflg;

   uu_denter(UU_MTRC,(us,"ncl_d_trimsf(key=%x,tfmat=%x,attrptr=%x)",
      eptr->key, tfmat, attrptr));

	um_set_disp_attr (attrptr);
	idx = 175;
	getsc (&idx,&told);

	gsedgeflag (UG_OFF);
	ncl_disp_params (eptr,attrptr,&lstix,&ndisp,&lstp,&shaded,&ecolor,&nup,&nvp,
		&nu,&nv);

	sh_mode = ncl_shading_mode ();
	bflg = 0;

/*
..... if edge color is set as "defalt" use the surface color
*/
	color = attrptr->color;
	if (ecolor == 64 || sh_mode == 0) ecolor = color;

	if (iflg == 0)
	{
		if (attrptr->shaded == 1 && sh_mode == 1)
		{
			extern int DEBUG_SHOW_TESS;
/*
..... now we display boundary when shaded - we use "invisible" line style
..... and then restore the current one
*/
			bflg = 1;
			cur_line_style = attrptr->line_style;
			if (!DEBUG_SHOW_TESS)
			{
				attrptr->line_style = 9; um_set_disp_attr (attrptr);
			}
		}

/*		if (!ncl_get_wireframe())
			return (0);*/
/*
..... If the surface has a display list, use it & return, 
..... otherwise initialize it.
*/
		if (eptr->no_displst && ncl_displst_OK(eptr->key,eptr->displst,told,bflg))
		{
/* 
... aak 29-may-1998: if it got here from projecting on drawing, tfmat
... is the product of the surface transformation times the projection
... tfmat; display list has been created with the surface tfmat ->
... multiply by its inverse to get pure projection transform
*/
			status = uc_retrieve_transf(eptr->key, tf0);
			status = um_inverttf (tf0,tf0);
			status = um_tftmtf (tf0,tfmat,tf0);

			if (ecolor >= 0)
			{
				if (sh_mode == 1)
					gsedgeflag (UG_ON);

				attrptr->color = ecolor;
				um_set_disp_attr (attrptr);
			}

			if (attrptr->shaded == 1 && sh_mode == 1)
			{
				ncl_bndr_display(eptr->displst,tf0,eptr->key);
				attrptr->line_style = cur_line_style;
				attrptr->color = color;
				um_set_disp_attr (attrptr);

				status = ncl_display_shaded_surf(eptr->key);
				if (status == UU_SUCCESS) return (0);
			}

			status = ncl_displst_display(eptr->displst,tf0);
/*
.....added for hidden line removal
.....Yurong 2/11/99
*/
			if (ncl_is_hidline()) ncl_display_hid_surf(eptr->key);  

			return (status);
		}
		ncl_displst_init(2, eptr->key);
   }

   evsrf = 0;
   bound.np = 0;
   bound.ummx = 0;
   bound.vmmx = 0;
   status = UU_FAILURE;

   bs.key = eptr->bs_key;
   status = ncl_retrieve_data_fixed (&bs);
   if (status != UU_SUCCESS) return (status);
/*
.....Save label display status
.....Bobby  -  2/25/94
*/
   if (iflg == 1) savelabel = attrptr->label_on;
   bound.cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
   bound.uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
   uu_list_init (bound.cvpts, sizeof(UM_coord), 100, 100);
   uu_list_init (bound.uvpts, sizeof(UM_coord), 100, 100);
   uu_list_init (&iobuf, sizeof(UM_2Dcoord), 20, 20);
   evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));

   status = um_uvlist_init (eptr,&cvlist);
   if (status == UU_SUCCESS)
   {
   ncl_set_boundary_toler (told);
   bound.toler = told;
/*
..... note: we do not call ncl_get_boundary here because we need the 
..... cvlist, and ncl_get_boundary gets only the boundary data stored as
..... a UM_srf_boundary structure. cvlist tells us how many points on the
..... boundary belongs to each composite component, which allows us to 
..... set a separate pick-id for each component
*/
	status = ncl_set_boundary(eptr,tfmat,&bound,&cvlist);
   }
/*
.....Error displaying surface
*/
	if (status != 0)
	{
		if ((unsigned long)evsrf>0) uu_free(evsrf);
		um_uvlist_free (&cvlist);
		uu_list_free (&iobuf);
		return (status);
	}

	if (status == 0)
	{
/* 
... display SF outer and inner boundary
*/  
		ncl_store_surflist (WHOLE_BOUNDARY_LIST,eptr,&bound);

		umin = ur[0] = bound.ummx[0][0];
		umax = ur[1] = bound.ummx[0][1];
		vmin = vr[0] = bound.vmmx[0][0];
		vmax = vr[1] = bound.vmmx[0][1];

		bnpt = (UM_coord *) UU_LIST_ARRAY (bound.cvpts);

Retry: /* redisplay if shading fails */
		for (k=0,ix=0; k<bound.nb; k++)
		{
			if (k == 0)
				key = eptr->uv_key;
			else
				key = eptr->ibndykey[k*2-1];
/*
... jingrong 9/18/98 : if the boundary curve is a composite curve, set 
... pick id for each component of the composite curve in a format as 
... "xxyy": xx = key of the composite curve, yy = cvid of the minor curve +1.
......andrew 11/8/12 : changed the format for the pickid/ckey to an index so
......the displst does not rely on the keys of the trim surface. Since the
......curves are stored in order, they can be retrieved using an index instead
......of a key.
*/
			cv.key = key;
			ncl_retrieve_data_fixed(&cv);
			if (ncl_itsa_compcrv(&cv))
			{
				n1 = cvlist.bncrv[k].no_csg;
				for (i=0; i<n1; i++)
				{
					if (attrptr->shaded == 1 && sh_mode == 1)
						gspickid((k+1)*100+eptr->key+i+1);
					else
						gspickid((k+1)*100+i+1);

					ncl_displst_setckey((k+1)*100+i+1);
					if (i<n1-1)
					{
						n = cvlist.bncrv[k].segix[i+1];
						n2 = n + 1;
					}
					else
					{
						n = cvlist.bncrv[k].segix[0]+bound.np[k];
						n2 = n;
					}
					for (j=ix; j<n2; j++)
					{
						ncl_lin (iflg, bnpt[j], eptr, attrptr, df,pt,ve,kl);
					}
					ncl_gdraw(iflg,eptr,attrptr,df,pt,ve,kl);
					ix = n;
				}
			}
			else
			{
/*
.....if the boundary curve is  not a composite curve, set 
.....pick id for the curve in a format as "xx00": xx = key of the curve.
.....This is done to always maintain the pickid of the trimmed surface 
.....boundary always higher than that of the surface.
.....boundary always higher than that of the surface.
.......Changed to mimic above change for composite curves - Andrew 11/8/12
*/
				if (attrptr->shaded == 1 && sh_mode == 1)
	     			gspickid((k+1)*100+eptr->key);
				else
	     			gspickid((k+1)*100);

     			ncl_displst_setckey((k+1)*100);
     			n = ix+bound.np[k];
     			for (j=ix; j<n; j++)
     			{
     				ncl_lin (iflg,bnpt[j], eptr, attrptr, df, pt, ve, kl);
     			}
     			ncl_gdraw(iflg,eptr,attrptr,df,pt,ve,kl);
     			ix += bound.np[k];
			}
		} /* for (k=0 ... */
	} /* if (ncc > 0) */

/*
..... uncommented this part of the code to fix  the problem in picking trim
..... surface boundaries.
*/
        gspickid(eptr->key);
        ncl_displst_setckey(eptr->key);

		if (attrptr->shaded == 1 && sh_mode == 1 && !iflg)
		{
			status = ncl_display_shaded_surf(eptr->key);
			attrptr->line_style = cur_line_style;
			um_set_disp_attr (attrptr);
			if (status == UU_SUCCESS)
			{
				status = ncl_displst_finish(told,1);
				return(status);
			}
			else
			{
				sh_mode = 0;
				status = UU_SUCCESS;
				goto Retry;
			}
		}

		nu = attrptr->numupaths-2;
		if (nu < 0) nu = 0;
		nv = attrptr->numvpaths-2;
		if (nv < 0) nv = 0;
		if ((umax-umin) < UM_FUZZ || (vmax-vmin) < UM_FUZZ)
		{
			nu = nv = 0;
			cvus.data = UU_NULL;
		}
		else
		{
			du = (umax-umin)/(nu+1);
			dv = (vmax-vmin)/(nv+1);
			uu_list_init (&cvus, sizeof(UM_coord), 100, 100);
		}
/*
...display u-lines (u=const)
*/
      for (i=0, u=umin+du; i<nu && status == UU_SUCCESS; i++, u=u+du)
      {
         if (u>1.) u = 1.;
         ldsp = 0;
/*
......evolve u line on base SF using tolerance
......and get first & last index of v points in trimmed span
*/
         cvus.cur_cnt = 0;
         bound.cvpts->cur_cnt = 0;
         iobuf.cur_cnt = 0; 
         nv1 = ncl_evolve_crv_on_srf
                   (&bs,tfmat,u,vr,2,told,bound.cvpts,UU_NULL,&cvus);
         tp = (UM_coord *) UU_LIST_ARRAY (&cvus);
         bnpt = (UM_coord *) UU_LIST_ARRAY (bound.cvpts);
         for (js=0; js<nv1 && tp[js][1]<vmin; js++); 
         for (je=nv1-1; je>js && tp[je][1]>vmax; je--); 
         if (je != nv1-1) je++; 
/*
......intersect u line with all boundary curves
*/
         ns = um_uv_isect_bndry (u,2,&bound,&iobuf);
         if (ns > 0)
         {
           uvpd = (UM_2Dcoord *) UU_LIST_ARRAY (&iobuf);
           prev = uvpd[0][1];
           vh = MAX2 (0.,uvpd[0][1]);
         }
/*
..... Eduard 3/10/99 : If a boundary curve is composite, some
..... intersection points may come with multiplicities. Here we remove
..... such multiple points, so that u-lines are displayed correctly. 
*/
         k = 1;
         while (k < ns)
         {
            cur = uvpd[k][1];
            if (fabs(cur - prev) < UM_FUZZ)
            {
               ns--;
               if (k == ns) break;
               for (j=k; j<ns; j++) uvpd[j][1]=uvpd[j+1][1];
            } 
            else
            {
               k++;
               prev=cur;
            } 
         }

         j = js;
         k = 0;
/*
......draw u line inside the boundary starting from
......intersection points 
*/
         while (k < ns && j < je+1)
         {
            v  = MIN2 (1.0,tp[j][1]);
            if (vh <= v)
              {
               vx = vh;
               ux = uvpd[k][0]; 
               status = ncl_evsrf_tf(UM_POINT,ux,vx,&bs,tfmat,evsrf); 
               if (ldsp)
                  status = ncl_lin (iflg, evsrf->sp, eptr, attrptr, 
                                                     df, pt, ve, kl);
               else
                  status = ncl_mov (iflg, evsrf->sp, eptr,attrptr, 
                                                     df, pt, ve, kl);
               ldsp = 1 - ldsp;
/*
.....vp 18-feb-97 make sure that k is valid before using uvpd[k]
.....because MIN2 macro can blow up if returns NaN.
*/
               if (++k < ns) vh = MIN2 (vmax,uvpd[k][1]);
              }
            else
/*
......inside the boundary draw u line using points
......generated by evolving routine
*/
              {
               if (ldsp)
                 status = ncl_lin (iflg, bnpt[j], eptr, attrptr, 
                                                     df, pt, ve, kl);
                 j++;
              }
          }
         ncl_gdraw(iflg,eptr,attrptr,df,pt,ve,kl);
      }
/*
...display v-lines (v=const)
*/
      for (i=0, v=vmin+dv; i<nv && status == UU_SUCCESS; i++, v=v+dv)
      {
         if (v>1.) v = 1.;
         ldsp = 0;
/*
......evolve v line on base SF using tolerance
......and get first & last index of u points in trimmed span
*/
         cvus.cur_cnt = 0;
         bound.cvpts->cur_cnt = 0;
         iobuf.cur_cnt = 0; 
         nu1 = ncl_evolve_crv_on_srf
                   (&bs,tfmat,v,ur,1,told,bound.cvpts,UU_NULL,&cvus);
         tp = (UM_coord *) UU_LIST_ARRAY (&cvus);
         bnpt = (UM_coord *) UU_LIST_ARRAY (bound.cvpts);
         for (js=0; js<nu1 && tp[js][0]<umin; js++); 
         for (je=nu1-1; je>js && tp[je][0]>umax; je--); 
         if (je != nu1-1) je++; 
/*
......intersect v line with all boundary curves
*/
         ns = um_uv_isect_bndry (v,1,&bound,&iobuf);
         if (ns > 0)
         {
            uvpd = (UM_2Dcoord *) UU_LIST_ARRAY (&iobuf);
            prev = uvpd[0][0];
            uh = MAX2 (0.,uvpd[0][0]);
         }
/*
..... Eduard 3/10/99 : If a boundary curve is composite, some
..... intersection points may come with multiplicities. Here we remove
..... such multiple points, so that v-lines are displayed correctly. 
*/
         k = 1;
         while (k < ns)
         {
            cur = uvpd[k][0];
            if (fabs(cur - prev) < UM_FUZZ)
            {
               ns--;
               if (k == ns) break;
               for (j=k; j<ns; j++) uvpd[j][0]=uvpd[j+1][0];
            } 
            else
            {
               k++;
               prev=cur;
            } 
         }

         j = js;
         k = 0;
/*
......draw v line inside the boundary starting from
......intersection points 
*/
         while (k < ns && j < je+1)
         {
            u  = MIN2 (1.0,tp[j][0]);
            if (uh <= u)
            {
               ux = uh;
               vx = uvpd[k][1]; 
               status = ncl_evsrf_tf(UM_POINT,ux,vx,&bs,tfmat,evsrf); 
               if (ldsp)
                  status = ncl_lin (iflg, evsrf->sp, eptr, attrptr, 
                                                     df, pt, ve, kl);
               else
                  status = ncl_mov (iflg, evsrf->sp, eptr,attrptr, 
                                                     df, pt, ve, kl);
               ldsp = 1 - ldsp;
/*
.....vp 18-feb-97 make sure that k is valid before using uvpd[k]
.....because MIN2 macro can blow up if returns NaN.
*/
               if (++k < ns) uh = MIN2 (umax,uvpd[k][0]);
            }
            else
/*
......inside the boundary draw v line using points
......generated by evolving routine
*/
            {
               if (ldsp)
                 status = ncl_lin (iflg, bnpt[j], eptr, attrptr, 
                                                    df, pt, ve, kl);
                 j++;
            }
         }
         ncl_gdraw(iflg,eptr,attrptr,df,pt,ve,kl);
      }
/*
..... Complete display list.
*/
   if (!iflg)
     status = ncl_displst_finish(told,0);

/*
.....added for hidden line removal
.....Yurong 2/11/99
*/
	if (ncl_is_hidline())
		ncl_display_hid_surf(eptr->key);  

Done:;
/*
...free allocated memory 
*/
   if ((unsigned long)evsrf>0) uu_free(evsrf);
   um_free_boundary (&bound);
   uu_list_free (&cvus);
   uu_list_free (&iobuf);
   um_uvlist_free (&cvlist);
   uu_dexit;
/*
.....Restore label display status
.....Bobby  -  2/25/94
*/
   if (iflg == 1) attrptr->label_on = savelabel;

   return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_eval_trimsf (evflag, u, v, eptr, tfmat, evsrf)
**       Evaluate an NCL trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          evflag     - evaluation flag
**          u          - evaluation parameter
**          v          - evaluation parameter
**          eptr       - ptr to surface
**          tfmat      - transformation
**       OUTPUT :  
**          evsrf      - ptr to surface evaluation record
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_eval_trimsf (evflag, u, v, eptr, tfmat, evsrf)
   int evflag;
   UM_param u, v;
   struct NCL_trimsf_rec *eptr;
   UM_transf tfmat;
   struct UM_evsrfout *evsrf;

   {
   int status, isze;
   struct NCL_fixed_databag bs;

   uu_denter(UU_MTRC,(us,"ncl_eval_trimsf(key=%x, tfmat=%x)",eptr->key,tfmat));

   status = UU_FAILURE;
   isze = sizeof (struct NCL_fixed_databag);
   bs.key = eptr->bs_key;
   status = uc_retrieve_data (&bs, isze);

   if (status == UU_SUCCESS)
     status = uc_evsrf (evflag, u, v, &bs, tfmat, evsrf);
/*
... aak 29-may-1998: tfmat is taken into account already in the
... base surf. evaluator

   if (status == UU_SUCCESS)
     status = ncl_transform_evsfout (evflag, tfmat, evsrf);
*/
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_evsrf_tf (evflag, u, v, eptr, tfmat, evsrf)
**       Evaluate a surface & transform the evaluation..
**    PARAMETERS   
**       INPUT  : 
**          evflag     - evaluation flag
**          u          - evluation parameter
**          v          - evluation parameter
**          eptr       - ptr to surface
**          tfmat      - transformation
**       OUTPUT :  
**          evsrf      - ptr to surface evaluation record
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_evsrf_tf (evflag, u, v, eptr, tfmat, evsrf)
   int evflag;
   UM_param u, v;
   struct NCL_trimsf_rec *eptr;
   UM_transf tfmat;
   struct UM_evsrfout *evsrf;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_evsrf_tf(key=%x, tfmat=%x)",eptr->key,tfmat));

	evsrf->ucurv = evsrf->vcurv = 0.;
	um_nullvc(evsrf->dsdu); um_nullvc(evsrf->d2sdu2);
	um_nullvc(evsrf->dsdv); um_nullvc(evsrf->d2sdv2);
   status = uc_evsrf (evflag, u, v, eptr, UM_DEFAULT_TF, evsrf);
   if (status == UU_SUCCESS)
     status = ncl_transform_evsfout (evflag, tfmat, evsrf);

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_trans_trimsf (eptr, tfmat, store)
**       Transform a trimmed surface.
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
int ncl_trans_trimsf (eptr, tfmat, store)
struct NCL_trimsf_rec *eptr;
UM_transf tfmat;
UU_LOGICAL store;
{
   int status;
   struct UM_transf_rec tran;

   uu_denter(UU_MTRC,(us,"ncl_transf_trimsf(key=%x, tfmat=%x)",
      eptr->key, tfmat));

   status = UU_FAILURE;

   if (store)
   {
      tran.key = eptr->key;
      tran.rel_num = UM_TRANSFORM_REL;
      if (ur_retrieve_transf(&tran) == 0)
      {
         um_tftmtf(tran.tfmat, tfmat, tran.tfmat);
         if (ur_update_transf (&tran) == 0) status = UU_SUCCESS;
/*
.....changed because we may have tesslst that also has to be deleted.
.....Yurong 1/28/99
*/
/*         if (status == UU_SUCCESS) ncl_displst_delete (&eptr->key);  */
			if (status == UU_SUCCESS)
			{
				if (eptr->no_tesslst!=0)
					ncl_lst_delete(TESSELLATION_LIST, &eptr->key);
				if (eptr->no_displst!=0)
					ncl_lst_delete(DISPLAY_LIST, &eptr->key);
				if (eptr->no_xyzbylst!=0)
					ncl_lst_delete(WHOLE_BOUNDARY_LIST, &eptr->key);
			}
      }
   }

   uu_dexit;
   return (status);
}

/*********************************************************************
**    E_FUNCTION : ncl_transform_evcrvout(evflag, tfmat, cvoutp)
**       Apply the transformation matrix (TFMAT) to 3 of the defined
**       fields in the surface evaluator output record (SFOUTP).
**    PARAMETERS
**       INPUT  :
**          evflag      UM_POINT=>     point
**                      UM_NORM=>      point, normal
**                      UM_FRSTFERIV=> point, normal, 1st deriv
**          tfmat       transformation matrix
**       OUTPUT :
**          cvoutp      curve evaluator record pointer for results
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_transform_evcrvout(evflag, tfmat, cvoutp)
UM_transf tfmat;
struct UM_evcrvout *cvoutp;
int evflag;

{
   int status = UU_SUCCESS;
   UU_LOGICAL um_is_idmat();

   if (!um_is_idmat(tfmat))
   {
      um_cctmtf(cvoutp->cp, tfmat, cvoutp->cp);
      if (evflag >= UM_FRSTDERIV)
      {
         um_vctmtf(cvoutp->dcdu, tfmat, cvoutp->dcdu);
         if (evflag >= UM_SECDERIV)
            um_vctmtf(cvoutp->d2cdu2, tfmat, cvoutp->d2cdu2);
         }
   }
   return(status);
}

/*********************************************************************
**    E_FUNCTION : ncl_transform_evsfout(evflag, tfmat, sfoutp)
**       Apply the transformation matrix (TFMAT) to 3 of the defined
**       fields in the surface evaluator output record (SFOUTP).
**    PARAMETERS   
**       INPUT  : 
**          evflag      UM_POINT=>     point
**                      UM_NORM=>      point, normal
**                      UM_FRSTFERIV=> point, normal, 1st deriv
**          tfmat       transformation matrix
**       OUTPUT :  
**          sfoutp      surface evaluator record pointer for results
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_transform_evsfout(evflag, tfmat, sfoutp)
int evflag;
UM_transf tfmat;
struct UM_evsrfout *sfoutp;
{
	int status = UU_SUCCESS;
	UU_REAL det;
	UU_LOGICAL um_is_idmat(),um_scale_in_tf();
	UU_LOGICAL scales = UU_FALSE;

	if (!um_is_idmat(tfmat))
	{
		um_cctmtf(sfoutp->sp, tfmat, sfoutp->sp);
		if (evflag >= UM_NORM)
		{
			um_vctmtf(sfoutp->snorm, tfmat, sfoutp->snorm);
			scales = um_scale_in_tf(tfmat);
			if (scales)
				um_unitvc (sfoutp->snorm,sfoutp->snorm);
			if (evflag >= UM_FRSTDERIV)
			{
				um_vctmtf(sfoutp->dsdu, tfmat, sfoutp->dsdu);
/*
.....If this is a mirror matrix
.....it is more important to have the normal vector
.....pointing in the correct direction,
.....rather than the V-derivitive.
.....FSR 61672 - Refer to 'get_surface_param'.
.....
.....After running production tests it was found
.....that routines that rely on the derivative
.....rather than the normal will fail with this logic
.....May have to look into returning normal from uevsrf()
.....Bobby - 10/22/14
*/
/*
				um_determinant(tfmat,&det);
				if (det < 0. && evflag < UM_SECDERIV)
					um_cross(sfoutp->snorm,sfoutp->dsdu,sfoutp->dsdv);
				else
*/
					um_vctmtf(sfoutp->dsdv, tfmat, sfoutp->dsdv);

				if (evflag >= UM_SECDERIV)
				{
					um_vctmtf(sfoutp->d2sdu2, tfmat, sfoutp->d2sdu2);
					um_vctmtf(sfoutp->d2sdv2, tfmat, sfoutp->d2sdv2);
					if (evflag >= UM_CURVATURE && scales)
					{
						um_get_curvature (sfoutp->dsdu,sfoutp->d2sdu2,&sfoutp->ucurv);
						um_get_curvature (sfoutp->dsdv,sfoutp->d2sdv2,&sfoutp->vcurv);
					}
				}
			}
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_class_copy_trimsf (e1, e2, bagsize)
**       Copy a trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          e1       - ptr of entity to copy.
**       OUTPUT :
**          e2       - ptr of copied entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_class_copy_trimsf (e1, e2, bagsize)
   struct NCL_trimsf_rec *e1, *e2;
   int bagsize;
   {
   int status;
   UM_int2 nclstatus;

   uu_denter(UU_MTRC,(us,"ncl_class_copy_trimsf (key=%x)", e1->key));

   status = ncl_label_wf(NCL_TRIMSF_REL, e2->label, &e2->subscr, 0, &nclstatus);
   if (status == UU_SUCCESS) status = ncl_copy_trimsf(e1, e2);
   if (status == UU_SUCCESS) status = ncl_store_wf1(e2->key);

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_copy_trimsf (e1, e2)
**       Copy a trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          e1       - entity to copy.
**       OUTPUT :
**          e2       - copied entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_copy_trimsf (e1, e2)
struct NCL_trimsf_rec *e1, *e2;
{
	UM_int2 lfl_77;
	UU_KEY_ID *keyp;
	int status, i, isub, isze;
	char labl[NCL_MAX_LABEL];
	struct NCL_fixed_databag c1, c2;
	struct UM_transf_rec tran;
	struct UC_attributedatabag attr1;

	uu_denter(UU_MTRC,(us,"ncl_copy_trimsf (key=%x)", e1->key));

	status = UU_SUCCESS;
	strncpy(labl,e2->label,NCL_MAX_LABEL);
	isub = e2->subscr;
	ur_setup_data (NCL_TRIMSF_REL, e2, sizeof(struct NCL_trimsf_rec));
	*e2 = *e1;
	e2->key = 0;
	strncpy(e2->label,labl,NCL_MAX_LABEL);
	e2->subscr = isub;
	e2->no_ibndykey = 0;
	e2->ibndykey = NULL;
	e2->no_displst = 0;
	e2->displst = NULL;
	e2->no_tesslst = 0;
	e2->tesslst = NULL;
	keyp = (UU_KEY_ID *) uu_toolmalloc((e1->no_ibndykey)* sizeof(UU_KEY_ID));

/* set ranfile label flag to temp unknown */
	lfl_77 = 1;
	stunlb (&lfl_77);

	status = UU_SUCCESS;
	isze = sizeof(struct NCL_fixed_databag);
	if (e1->uv_key > 0)
	{
		c1.key = e1->uv_key;
		status = ncl_retrieve_data (&c1, isze);
		if (status == UU_SUCCESS)
		{
			c2.key = 0;
			status = uc_copy (&c1, &c2, isze);
		}
		if (status == UU_SUCCESS)
		{
			ur_update_displayable(c2.key, UM_NEVERDISPLAYABLE);
			e2->uv_key = c2.key;
		}
	}
	if (e1->cv_key > 0 && status == UU_SUCCESS)
	{
		c1.key = e1->cv_key;
		status = ncl_retrieve_data (&c1, isze);
		if (status == UU_SUCCESS)
			status = uc_copy (&c1, &c2, isze);
		if (status == UU_SUCCESS)
		{
			ur_update_displayable(c2.key, UM_NEVERDISPLAYABLE);
			e2->cv_key = c2.key;
		}
/*
... if copy of xyz boundary curve fails, create trimmed sf with no xyz boundary.
*/
		else
		{
			e2->cv_key = 0;
			status = UU_SUCCESS;
		}
	}
	if (status == UU_SUCCESS)
		if (e1->bs_key > 0)
		{
			c1.key = e1->bs_key;
			status = ncl_retrieve_data (&c1, isze);
			if (status == UU_SUCCESS)
				status = uc_copy (&c1, &c2, isze);
				if (status == UU_SUCCESS)
				{
					ur_update_displayable(c2.key, UM_NEVERDISPLAYABLE);
					e2->bs_key = c2.key;
				}
		}
		else
			status = UU_FAILURE;
/*
...copy inner boundary curves
*/
	for (i=0; i<e1->no_ibndykey && status == UU_SUCCESS; i++)
	{
		if (e1->ibndykey[i] > 0)
		{
			c1.key = e1->ibndykey[i];
			status = ncl_retrieve_data (&c1, isze);
			if (status == UU_SUCCESS)
					status = uc_copy (&c1, &c2, isze);
			if (status == UU_SUCCESS)
			{
				ur_update_displayable(c2.key, UM_NEVERDISPLAYABLE);
				keyp[i] = c2.key;
			}
		}
/*
.....vp 1.27.97: not used keys (when cv_key is not used)
.....must be zeroed.
*/
		else keyp[i] = 0;
	}

/* reset ranfile label flag */
	stunlb (&lfl_77);

	if (status == UU_SUCCESS)
	{   /* get its transform */
/*
.....vp 1.27.97 never set size of join if using ncl_create_entity
.....end ur_update_data_varlist to create join !!!
		e2->no_ibndykey = e1->no_ibndykey;
*/
		tran.key = e1->key;
		tran.rel_num = UM_TRANSFORM_REL;
		status = ur_retrieve_transf(&tran);
	}
	if (status == UU_SUCCESS)
	{   /* create trimmed surface entity and its transform */
		status = ncl_create_entity(e2, 9);
		tran.key = e2->key;
		if (status == UU_SUCCESS)
		{
			status = ur_update_transf(&tran);
			if (status == UU_SUCCESS)
				status = ur_update_data_varlist (e2->key, 1, keyp, 1, 
                                                     e1->no_ibndykey);
		}
	}
	if (status == UU_SUCCESS)
	{
		uc_retrieve_attr(e1->key, &attr1);
		attr1.key = e2->key;
		ur_update_attr(&attr1);
	}
	if (keyp != UU_NULL) uu_toolfree(keyp);
	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_offset_trimsf (sfp, cdis)
**       Offset a trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          sfp        - pointer to surface
**          cdis       - offset distance
**       OUTPUT :
**          sfp        - pointer to surface with offset field updated
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_offset_trimsf (sfp, cdis)
   struct NCL_trimsf_rec *sfp;
   UU_REAL cdis;

   {
   int status, i;
   UU_KEY_ID *kptr;
   struct NCL_fixed_databag bs;
   UM_transf tfmat;
   UU_REAL det;

   status = ncl_retrieve_data_fixed (sfp);
   if (status == UU_SUCCESS)
     status = uc_retrieve_transf(sfp->key, tfmat);
   if (status == UU_SUCCESS)
   {
/*
...fix surface offset: the previous version did it incorrectly, we put a change 
... with the version flag. Eduard 062100.
*/
		UM_real8 ver;
		UM_int2 idx;

		um_determinant (tfmat,&det);
		idx = 169;
		getsc (&idx, &ver);
		if (ver < 9.149)
		{
			if (det < 0.0) cdis = -cdis;
		}
		else
		{
			int neg = 0;
			if (det < 0.0)
			{
				neg = 1;
				det = -det;
			}
			if (det != 1.0) det = pow (det,1./3.);
			if (neg) det = -det;
			cdis = cdis / det;
		}

     sfp->offdist = sfp->offdist + cdis;
     if (sfp->cv_key)
     {
       uc_delete(sfp->cv_key);
       sfp->cv_key = 0;
     }
   }
   if (status == UU_SUCCESS)
   {
     for (i=0,kptr=sfp->ibndykey; i<sfp->no_ibndykey; i+=2, kptr+=2)
     {
       if (*kptr)
       {
         uc_delete(*kptr);
         *kptr = 0;
       }
     }
   }
   if (status == UU_SUCCESS)
   {
     bs.key = sfp->bs_key;
     status = ncl_retrieve_data_fixed (&bs);
   }
   if (status == UU_SUCCESS)
     {
      switch (bs.rel_num)
         {
          case UM_RBSPLSRF_REL:
            {
             struct UM_rbsplsrf_rec *bsrf;
             bsrf = (struct UM_rbsplsrf_rec *) &bs;
             bsrf->offdist = bsrf->offdist + cdis;
             status = ur_update_data_fixed(bsrf); 
             if (status == UU_SUCCESS && bsrf->primitive >= NCLSF_PLANE && 
                 fabs(cdis) > 0.0001)
             {
              ncl_offset_primdat (bsrf->key,bsrf->primitive,bsrf->prim_param,
                                    cdis);
              status = ur_update_data_fixed(bsrf); 
             }
            }
           break;
          case NCL_SURF_REL:
            {
             struct NCL_surface_rec *bsrf;
             bsrf = (struct NCL_surface_rec *) &bs;
             bsrf->offdist = bsrf->offdist + cdis;
             status = ur_update_data_fixed(bsrf); 
             if (status == UU_SUCCESS && bsrf->primitive >= NCLSF_PLANE && 
                 fabs(cdis) > 0.0001)
             {
              ncl_offset_primdat (bsrf->key,bsrf->primitive,bsrf->prim_param,
                                    cdis);
              status = ur_update_data_fixed(bsrf); 
             }
            }
           break;
          case NCL_REVSURF_REL:
            {
             struct NCL_revsurf_rec *bsrf;
             bsrf = (struct NCL_revsurf_rec *) &bs;
             bsrf->offdist = bsrf->offdist + cdis;
             status = ur_update_data_fixed(bsrf); 
             if (status == UU_SUCCESS && bsrf->primitive >= NCLSF_PLANE && 
                 fabs(cdis) > 0.0001)
             {
              ncl_offset_primdat (bsrf->key,bsrf->primitive,bsrf->prim_param,
                                    cdis);
              status = ur_update_data_fixed(bsrf); 
             }
            }
           break;
          case NCL_MESHSURF_REL:
            {
             struct NCL_meshsf_rec *bsrf;
             bsrf = (struct NCL_meshsf_rec *) &bs;
             bsrf->offdist = bsrf->offdist + cdis;
             status = ur_update_data_fixed(bsrf); 
            }
           break;
          case NCL_QUILTSURF_REL:
            {
             struct NCL_quiltsf_rec *bsrf;
             bsrf = (struct NCL_quiltsf_rec *) &bs;
             bsrf->offdist = bsrf->offdist + cdis;
             status = ur_update_data_fixed(bsrf); 
            }
           break;
          case NCL_EVALSF_REL:
            {
             struct NCL_evalsf_rec *bsrf;
             bsrf = (struct NCL_evalsf_rec *) &bs;
             bsrf->offdist = bsrf->offdist + cdis;
             status = ur_update_data_fixed(bsrf); 
            }
           break;
         }
     }

   return (status);
   }

/*********************************************************************
**    I_FUNCTION : int ncl_trimsf_query(key, list)
**         This function produces the query output for a trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          key      key to a trimmed surface. 
**       OUTPUT :  
**          list     list to contain the query information.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_trimsf_query(key, list)
UU_KEY_ID key;
UU_LIST *list;
{
	struct NCL_trimsf_rec nclent;
	struct UC_attributedatabag attr;
	struct NCL_fixed_databag bsf;
	struct UM_rbsplsrf_rec *rbsf;
	UM_transf tfmat;
	int status = UU_SUCCESS;
	int iswap,clsdu,clsdv;
	UU_LOGICAL revnorm;
	char buf[300];
	UM_int2 primtyp;
	nclsf_prim_type typ;
	UM_real8 primdata[16];

   /* get the entity data */
	nclent.key = key;
	if (ncl_retrieve_data(&nclent, sizeof(struct NCL_trimsf_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS) goto failed;

/*
... Primitive type. 
*/
	status = ncl_get_sf_primdat(&nclent.bs_key,&primtyp,primdata);
	if (status == UU_SUCCESS)
	{
		typ = (nclsf_prim_type) primtyp;
		ncl_transform_primdat (&typ,primdata,tfmat);
		um_primitive_query(primtyp,primdata,list);
	}

	sprintf(buf,"   boundary curve ......... %d", nclent.uv_key);
	uj_putstr(list, buf);

	sprintf(buf,"   base surface ........... %d", nclent.bs_key);
	uj_putstr(list, buf);

	bsf.key = nclent.bs_key;
	status = ncl_retrieve_data_fixed (&bsf);
	if (status != UU_SUCCESS) goto failed;

	rbsf = (struct UM_rbsplsrf_rec *) &bsf;
	clsdu = rbsf->closdinu;
	clsdv = rbsf->closdinv;
	ncl_closed_part(list, &clsdu,&clsdv);
	ncl_get_redef_params (&bsf,&iswap,&revnorm);
	ncl_params_part(list, iswap, revnorm);

	strcpy(buf,"   drive type ............. ");
	if (nclent.drive_type == 0) strcat(buf,"Base");
	else if (nclent.drive_type == 1) strcat(buf,"Face");
	else strcat(buf,"Not set");
	uj_putstr(list, buf);

	goto done;

failed: status = UU_FAILURE;
done:;
	 return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_mov (iflg, dpt, eptr,attrptr, df, pt, ve, kl)
**       Move to a point in display or project to drawing.
**    PARAMETERS
**       INPUT  : 
**          iflg    - 0 = display, 1 = proj to drawing
**          dpt     - Display point.
**			eptr    - Trimmed surface pointer.  Bobby - 2/25/94
**          attrptr - Attribute pointer.
**          df      - Drawing transformation.
**          pt      - View reference point.
**          ve      - View normal vector.
**       OUTPUT :  
**          kl      - Key list of drawing entities created.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_mov (iflg, dpt, eptr,attrptr, df, pt, ve, kl)
int iflg;
UM_coord dpt;
UM_transf df;
struct NCL_trimsf_rec *eptr;
struct UC_attributedatabag *attrptr;
UM_coord pt;
UM_vector ve;
UU_LIST *kl;
   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_mov ()"));

   status = UU_SUCCESS;
   if (iflg == 0) gmova3 (&dpt[0],&dpt[1],&dpt[2]);
   else
     {
     if (NCL_npts > 1) ncl_gdraw (iflg, eptr,attrptr, df, pt, ve, kl);
     um_vctovc (dpt, NCL_dpts);
     NCL_npts = 1;
     }

   uu_dexitstatus("ncl_mov ()", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_lin (iflg, dpt, attrptr, df, pt, ve, kl)
**       Draw to a point in display or project to drawing.
**    PARAMETERS
**       INPUT  : 
**          iflg    - 0 = display, 1 = proj to drawing
**          dpt     - Display point.
**			eptr    - Trimmed surface pointer.  Bobby - 2/25/94
**          attrptr - Attribute pointer.
**          df      - Drawing transformation.
**          pt      - View reference point.
**          ve      - View normal vector.
**       OUTPUT :  
**          kl      - Key list of drawing entities created.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_lin (iflg, dpt, eptr, attrptr, df, pt, ve, kl)
int iflg;
UM_coord dpt;
UM_transf df;
struct NCL_trimsf_rec *eptr;
struct UC_attributedatabag *attrptr;
UM_coord pt;
UM_vector ve;
UU_LIST *kl;
   {
   int status, n;

   uu_denter(UU_MTRC,(us,"ncl_lin ()"));

   status = UU_SUCCESS;
   if (iflg == 0) glina3 (&dpt[0],&dpt[1],&dpt[2]);
   else
     {
     if (NCL_npts == NCL_MAXPTS)
       {
       n = NCL_npts-1;
       status = ncl_mov (iflg, NCL_dpts[n], eptr,attrptr, df, pt, ve, kl);
       }
     um_vctovc (dpt, NCL_dpts[NCL_npts]);
     NCL_npts++;
     }

   uu_dexitstatus("ncl_lin ()", status);
   return(status);
   }
 
/*********************************************************************
**    E_FUNCTION     : ncl_gdraw (iflg, eptr,attrptr, df, pt, ve, kl)
**       Complete draw to a point in display or project to drawing.
**    PARAMETERS
**       INPUT  : 
**          iflg    - 0 = display, 1 = proj to drawing
**			eptr    - Trimmed surface pointer.
**          attrptr - Attribute pointer.
**          df      - Drawing transformation.
**          pt      - View reference point.
**          ve      - View normal vector.
**       OUTPUT :  
**          kl      - Key list of drawing entities created.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_gdraw (iflg, eptr,attrptr, df, pt, ve, kl)
int iflg;
UM_transf df;
struct NCL_trimsf_rec *eptr;
struct UM_attrdata_rec *attrptr;
UM_coord pt;
UM_vector ve;
UU_LIST *kl;
   {
   int status, i;
   UM_coord lpt;
   struct UM_polyline_rec pline;

   uu_denter(UU_MTRC,(us,"ncl_gdraw ()"));

   status = UU_SUCCESS;
   if (iflg == 0) gdraw ();
   else
     {
     if (NCL_npts > 1)
       {
       ur_setup_data(UM_POLYLINE_REL, &pline, sizeof(pline));
/*
.....Calculate label position
.....Bobby  -  2/25/94
*/
/*       pline.subscr = 0;*/
		ncl_proj_label_to_drw(eptr,&pline,attrptr,df,pt,ve);

       status = ncl_create_geom_drw(&pline, UM_DEFAULT_TF, attrptr);
       for (i=0; i<NCL_npts && status==UU_SUCCESS; i++)
         {
         um_nptpln(NCL_dpts[i], pt, ve, lpt);
         um_cctmtf(lpt, df, lpt);
         if (ur_update_data_varlist (pline.key, 1, lpt, i+1, 1) != 0)
           status = UU_FAILURE;
         }
       if (status == UU_SUCCESS)
         {
         ur_update_displayable(pline.key, UM_DISPLAYABLE);
         status = ncl_retrieve_data (&pline,sizeof(struct UM_polyline_rec));
         }
       if (status == UU_SUCCESS) status = uc_display(&pline);
       if (status == UU_SUCCESS) uu_list_push(kl, &pline.key);
		attrptr->label_on = 0;
       }
     NCL_npts = 0;
     }

   uu_dexitstatus("ncl_gdraw ()", status);
   return(status);
   }
