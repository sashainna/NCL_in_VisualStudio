/*********************************************************************
**    NAME         :  nepocket.c
**       CONTAINS:  Routines for Raphi Razi's pocket.
**           int iscmpc1 (nclkey, iflag)
**           int gtccnm1 (nclkey, nents)
**           int gtcent1 (nclkey, ix, iunit, buf, ietype, ierr)
**           void gtpts (buf,ix,jx,ier)
**           void gtnpt(ix, npts, ier)
**           void gtpln (ix,buf,lstored)
**           void frpkys ()
**           void stpkys (nclkey,ncv,singl,dtol,ier)
**           void stpky1 (nclkey,kcv,ityp,dtol,ier)
**           void gtpkys (i,key,ityp,ier)
**           ncl_brkarc
**           lcrect
**           llini
**           lldel
**           llnul
**           lisini
**           lisdel
**           lisrst
**           lisadd
**           lisget
**           llpush
**           llpop
**           llsort
**           int ncl_initset_boundary(srf,tran,bound,tol)
**           void ncl_wat_get_boundary
**           void ncl_wat_set_boundary
**           void ncl_wat_set_plane (ppt,pve)
**           void ncl_wat_set_planeflg (lfl)
**           void ncl_wat_reset_plane()
**           void ncl_calc_boxes (pol)
**           void initpokpols(tol)
**           void rstpokpol(it)
**           void freepokpols()
**           void addpokpol (it,xa,ya,nppa)
**           void subpokpols (ar0,ngaps)
**           void getpolb (it,xa,ya,nppa)
**    COPYRIGHT 1990 (c) Mills Data Systems Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nepocket.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:40
*********************************************************************/

#include "usysdef.h"
#include "nccs.h"
#include "udebug.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mgeom.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclxmdl.h"
#include "dmark.h"
#include "lipv.h"
#include "mcvh.h"
#include "msol.h"
#include "nclclip.h"

static UU_KEY_ID srfkey = 0;
static int num_gkeys = 0;
static UU_KEY_ID *gkeys = UU_NULL;
static UM_srf_boundary *bound1 = UU_NULL;
static UM_srf_boundary *bound2 = UU_NULL;
static UU_LIST *nouvlst = UU_NULL;

static UU_LOGICAL Splnset = UU_FALSE;
static UU_LOGICAL Splndefined = UU_FALSE;
static UM_real8 Splnbuf[6];

static ncl_polygon Spol0,Spol1,Spol2;
static int Spol0init = 0;
static int Spol1init = 0;
static int Spol2init = 0;
static UU_LIST Snplst,Sboxlst,Scontour0,Scontour1,Scontour2,Snjlist;
static int Snp2 = 0;
static UU_REAL Stol,Stolsq;

extern int NCLX_internal_geom;

typedef struct
{
	UM_int2 index;
	UM_int2 lane;
	UM_real4 x;
} NCL_lacept;

typedef struct
{
	UM_real4 x;
	UM_real4 y;
	UM_int2 jcut;
} NCL_islept;

static UU_LIST NCL_lace;
static int NCL_lace_init = 0;
static int NCL_lace_sort = 0;
static UU_LIST *NCL_lace_isl = UU_NULL;
static int NCL_lace_numisles = 0;

/*********************************************************************
**    E_FUNCTION     : int iscmpc1 (nclkey, iflag)
**       Determine if an entity is a composite curve.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of entity
**       OUTPUT :  
**          iflag      - 1 if entity is composite curve, 0 otherwise.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
iscmpc1 (nclkey, iflag)
   UM_int4 *nclkey;
   UM_int2 *iflag;

   {
   
   int status, rel_num;
   UU_KEY_ID key;

   uu_denter(UU_MTRC,(us,"iscmpc (key=%x)", nclkey));

   *iflag = 0;
   key = *nclkey;
   if (ur_retrieve_data_relnum (key, &rel_num) != 0)
      status = UU_FAILURE;
   else
      {
      status = UU_SUCCESS;
      if (rel_num == UM_COMPCRV_REL)
         *iflag = 1;
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtccnm1 (nclkey, nents)
**       Return the number of elements in a composite curve, net
**       surface, or composite solid.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of entity
**       OUTPUT :  
**          nents      - number of elements
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtccnm1 (nclkey, nents)
UM_int4 *nclkey;
UM_int2 *nents;
{
   
	int status, rel_num;
	struct UM_compcrv_rec e;
	struct NCL_netsf_rec srf;
	struct UM_solid_rec sol;

	*nents = 0;
	status = UU_FAILURE;

	e.key = *nclkey;
	if (ur_retrieve_data_relnum (e.key, &rel_num) == 0)
	{
		if (rel_num == UM_COMPCRV_REL)
		{
			if (ur_retrieve_data_fixed (&e) == 0)
			{
				*nents = e.no_cid;
				status = UU_SUCCESS;
			}
		}
		else if (rel_num == NCL_NETSF_REL)
		{
			srf.key = *nclkey;
			if (ur_retrieve_data_fixed (&srf) == 0)
			{
				*nents = srf.no_netkey;
				status = UU_SUCCESS;
			}
		}
		else if (rel_num == UM_SOLID_REL)
		{
			sol.key = *nclkey;
			if (ur_retrieve_data_fixed (&sol) == 0)
			{
				if (sol.type == UM_COMPOS_SOLID)
				{
					*nents = sol.no_netkey;
					status = UU_SUCCESS;
				}
			}
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int gtcent1 (nclkey, ix, buf, ietype, ierr)
**       Return data of element of composite curve for pocket.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of composite curve.
**          ix         - index of element
**       OUTPUT :  
**          keyout     - key of this entity
**          buf        - canonical data if line or circle.
**          ietype     - NCL type
**          ierr       - 0 if no error, non-zero if error
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtcent1 (nclkey, ix, keyout, buf, ietype, ierr)
   UM_int4 *nclkey, *keyout;
   UM_int2 *ix, *ietype, *ierr;
   UM_real8 buf[];

   {
   
   int status, rel_num, lix, i, i1, i2;
   struct UM_compcrv_rec e;
   struct UM_cid_rec cid;
   UM_transf rotmat;
   UM_vector v1;

   uu_denter(UU_MTRC,(us,"gtcent (key=%x)", nclkey));

   *ietype = 0;
   *ierr = 1;
   e.key = *nclkey;
   lix = *ix;
   if (ur_retrieve_data_relnum (e.key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != UM_COMPCRV_REL)
      status = UU_FAILURE;
   else if (ur_retrieve_data_fixed (&e) != 0)
      status = UU_FAILURE;
   else if (lix < 1 || lix > e.no_cid)
      status = UU_FAILURE;
   else if (ur_retrieve_data_varlist (e.key, 1, &cid, lix, 1) != 0)
      status = UU_FAILURE;
   else if (ur_retrieve_data_relnum (cid.crvid, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num == UM_LINE_REL)
      {
      struct UM_line_rec ln;
      ln.key = cid.crvid;
      if (ur_retrieve_data_fixed (&ln) != 0)
         status = UU_FAILURE;
      else
         {
         if (cid.reverse)
            {
            i1 = 3;
            i2 = 0;
            }
         else
            {
            i1 = 0;
            i2 = 3;
            }
         for (i=0; i<3; i++)
            {
            buf[i1+i] = ln.spt[i];
            buf[i2+i] = ln.ept[i];
            }
         *ietype = NCLI_LINE;
         *ierr = 0;
         *keyout = cid.crvid;
         status = UU_SUCCESS;
         }
      }
   else if (rel_num == UM_CIRCLE_REL)
      {
      struct UM_circle_rec ci;
      ci.key = cid.crvid;
      if (ur_retrieve_data_fixed (&ci) != 0)
         status = UU_FAILURE;
      else
         {
         if (cid.reverse) um_reverse_circle (&ci);
         um_rotlntf(ci.center, ci.nvec, ci.dang, rotmat);
         um_vctmtf(ci.svec, rotmat, v1);
         for (i=0; i<3; i++)
            {
            buf[i] = (ci.svec[i]*ci.radius+ci.center[i]);
            buf[i+3] = ci.center[i];
            buf[i+6] = (v1[i]*ci.radius+ci.center[i]);
            buf[i+9] = ci.nvec[i];
            }
         buf[12] = ci.radius;
         buf[13] = ci.dang;
         *ietype = NCLI_CIRCLE;
         *ierr = 0;
         *keyout = cid.crvid;
         status = UU_SUCCESS;
         }
      }
   else 
      {
      *keyout = cid.crvid;
      if (cid.reverse) *keyout = -cid.crvid;
      *ietype = NCLI_CURVE;
      *ierr = 0;
      status = UU_SUCCESS;
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : void frpkys ()
**       Free the boundaries.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of composite curve.
**          ix         - index of element
**       OUTPUT :  
**          keyout     - key of this entity
**          buf        - canonical data if line or circle.
**          ietype     - NCL type
**          ierr       - 0 if no error, non-zero if error
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void frpkys ()
{
	int i;

	if (bound1) um_free_boundary (bound1);
	bound1 = UU_NULL;
	if (bound2) um_free_boundary (bound2);
	bound2 = UU_NULL;
	UU_FREE (gkeys);
	if (nouvlst)
	{
		gkeys = (UU_KEY_ID *) UU_LIST_ARRAY(nouvlst);
		for (i = 0; i < nouvlst->cur_cnt; i++)
			uc_delete(gkeys[i]);
		UU_LIST_FREE (nouvlst);
	}
	num_gkeys = 0;
	gkeys = UU_NULL;

	return;
}

/*********************************************************************
*********************************************************************/
void ncl_wat_set_plane (ppt,pve)
UM_coord ppt;
UM_vector pve;
{
	int k;

	for (k = 0; k < 3; k++)
	{
		Splnbuf[k] = ppt[k];
		Splnbuf[k+3] = pve[k];
	}
	Splndefined = UU_TRUE;
}

/*********************************************************************
*********************************************************************/
void ncl_wat_set_planeflg (lfl)
UU_LOGICAL lfl;
{
	Splnset = Splndefined && lfl;
}

/*********************************************************************
*********************************************************************/
void ncl_wat_reset_plane()
{
	Splnset = UU_FALSE;
	Splndefined = UU_FALSE;
}

/*********************************************************************
*********************************************************************/
void ncl_wat_set_boundary (bndr)
UM_srf_boundary *bndr;
{
	bound1 = bndr;
}

/*********************************************************************
*********************************************************************/
void ncl_wat_get_boundary (bndr)
UM_srf_boundary **bndr;
{
	*bndr = bound1;
}

/*********************************************************************
**    FUNCTION     : int ncl_initset_boundary(srf,tran,bound,tol)
**      Create a set of uv and xyz points on the boundary of a trimmed surface
**    PARAMETERS
**       INPUT  :
**          tran    - transformation
**          srf     - surface
**          tol     - tolerance
**       OUTPUT :
**          bound   - Pointer to boundary structure
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_initset_boundary(srf,tran,bound,noinner,tol)
struct NCL_fixed_databag *srf;
UM_transf tran;
UM_srf_boundary *bound;
UU_LOGICAL noinner;
UU_REAL tol;
{
	struct NCL_uvconv cvlist;
	int status;

	bound->np = UU_NULL;
	bound->ummx = UU_NULL;
	bound->vmmx = UU_NULL;
	bound->uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (bound->uvpts, sizeof(UM_coord), 100, 100);
	bound->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (bound->cvpts, sizeof(UM_coord), 100, 100);

	status = um_uvlist_init (srf,&cvlist);
	if (status == UU_SUCCESS)
	{
	ncl_set_boundary_toler (tol);
	bound->toler = tol;
	status = ncl_set_boundary(srf,tran,bound,&cvlist);

	if (noinner && bound->nb > 1)
	{
		bound->nb = 1;
		bound->uvpts->cur_cnt = bound->cvpts->cur_cnt = bound->np[0];
	}
	}
	um_uvlist_free (&cvlist);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : void stpky1 (nclkey,kcv,ityp,dtol,offset,ier)
**       Set up the trimmed surface boundary to use as a pocket
**       geometry.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of trimmed surface used for pocket boundary.
**          dtol       - active tolerance
**          offset     - 3 = Surface boundary will be offset internally,
**                       the key of the surface will be returned as the
**                       curve key.
**       OUTPUT :  
**          kcv        - key of boundary curve from surface to use for
**                       pocket boundary.
**          ityp       - geometry type of boundary curve.
**                       7 = Circle, 8 = Composite curve,
**                       9 = Surface (if OFFSET is specified).
**          ier        - 0 if no error, non-zero if error
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void stpky1 (nclkey,kcv,ityp,dtol,offset,ier)
UM_int4 *nclkey,*kcv;
UM_int2 *ityp,*ier,*offset;
UM_real8 *dtol;
{
	int status;
	struct NCL_fixed_databag srf;
	UM_transf tran;
	UU_LOGICAL doboundary = UU_TRUE;
/*
.....If using OFFSET pocket direction modifier the surface will be
.....handled later.
*/
	if (*offset == 3)
	{
		*kcv = *nclkey; *ityp = 9;
		return;
	}
	srf.key = *nclkey;
	status = ncl_retrieve_data_fixed (&srf);
	if (status != UU_SUCCESS) goto Done;
	srfkey = *nclkey;
	uc_retrieve_transf (srf.key,tran);

	*kcv = 0;
	*ityp = 0;
	if (srf.rel_num == NCL_TRIMSF_REL)
	{
		struct NCL_trimsf_rec *tsf;
		int rel;
		UM_transf tran;
		UU_LOGICAL um_is_idmat();

		uc_retrieve_transf (srf.key,tran);

		tsf = (struct NCL_trimsf_rec *) &srf;
		if (tsf->cv_key > 0)
		{
			status = ur_retrieve_data_relnum (tsf->cv_key, &rel);
			if (status != UU_SUCCESS) goto Done;
			if ((rel == UM_CIRCLE_REL) || (rel == NCL_CIRCLE_REL) ||
				(NCLX_internal_geom && rel == NCLX_MDL_CIRCLE))
				*ityp = 7;
			else if (rel == UM_COMPCRV_REL ||
				(NCLX_internal_geom && rel == NCLX_MDL_COMPOSITE))
				*ityp = 8;
			doboundary = (*ityp == 0);
			if (*ityp == 7 || *ityp == 8)
			{
				if (um_is_idmat(tran))
					*kcv = tsf->cv_key;
				else
				{
					struct NCL_fixed_databag c1, c2;
					UM_int2 lfl_77;

/* set ranfile label flag to temp unknown */
					lfl_77 = 1;
					stunlb (&lfl_77);

					c1.key = tsf->cv_key;
					status = ncl_retrieve_data_fixed (&c1);
					if (status == UU_SUCCESS)
						status = uc_copy (&c1, &c2, sizeof(c1));
					if (status == UU_SUCCESS)
						status = uc_transform (&c2, tran, UU_TRUE);
					if (status != UU_SUCCESS) goto Done;
					if (!nouvlst)
					{
						nouvlst = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
						uu_list_init (nouvlst, sizeof(UU_KEY_ID), 10, 10);
					}
					uu_list_push(nouvlst,&c2.key);
					*kcv = c2.key;
/* reset ranfile label flag */
					stunlb (&lfl_77);
				}
			}
		}
	}

	if (doboundary)
	{
		UU_REAL tol = *dtol;
		UU_LOGICAL noinner = UU_FALSE;

		bound1 = (UM_srf_boundary *) uu_malloc (sizeof (UM_srf_boundary));
		status = ncl_initset_boundary (&srf,tran,bound1,noinner,tol);

		if (bound1->nb < 1 || bound1->np[0] < 4)
			status = UU_FAILURE;
		else
		{
			UM_coord *pts;
			int i,npt;
			UM_int2 type = NCLI_POINT;

			pts = (UM_coord *) UU_LIST_ARRAY (bound1->cvpts);
			npt = bound1->cvpts->cur_cnt;
			for (i = 0; i < npt; i++)
				fr_unbs(pts[i],pts[i],&type);
		}
	}

Done:;
	if (status != UU_SUCCESS) *ier = 177;
	return;
}

/*********************************************************************
**    E_FUNCTION     : void stpkys (nclkey,ncv,singl,dtol,ier)
**       Set up the trimmed surface boundary to use as a pocket
**       geometry.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of composite curve.
**          ix         - index of element
**       OUTPUT :  
**          keyout     - key of this entity
**          buf        - canonical data if line or circle.
**          ietype     - NCL type
**          ierr       - 0 if no error, non-zero if error
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void stpkys (nclkey,ncv,dtol,ier)
UM_int4 *nclkey;
UM_int2 *ncv,*ier;
UM_real8 *dtol;
{
	struct NCL_fixed_databag srf;
	UM_transf tran;
	UU_LOGICAL doboundary = UU_TRUE;
	int status;

	srf.key = *nclkey;
	status = ncl_retrieve_data_fixed (&srf);
	if (status != UU_SUCCESS) goto Done;
	uc_retrieve_transf (srf.key,tran);

	*ncv = 0;

	if (ncl_itsa_trimsrf (&srf))
	{
		UU_REAL bplm[4];
		int nb;
		status = ncl_trimsrf_get_fixed (&srf,&nb,bplm);
		if (status != UU_SUCCESS) goto Done;
		*ncv = nb;
		if (nb < 2) return;

		if (srf.rel_num == NCL_TRIMSF_REL)
		{
			int i,rel;
			struct NCL_trimsf_rec *tsf;
			UM_transf tran;
			UU_LOGICAL um_is_idmat(),notrans;

			uc_retrieve_transf (srf.key,tran);
			notrans = um_is_idmat(tran);

			tsf = (struct NCL_trimsf_rec *) &srf;
			num_gkeys = tsf->no_ibndykey/2;
			gkeys = (UU_KEY_ID *) uu_malloc (num_gkeys*sizeof(UU_KEY_ID));
			for (i = 0, doboundary = UU_FALSE; i < num_gkeys && !doboundary; i++)
			{
				gkeys[i] = tsf->ibndykey[2*i];
				doboundary = (gkeys[i] == 0);
				if (!doboundary)
				{
					status = ur_retrieve_data_relnum (gkeys[i], &rel);
					if (status != UU_SUCCESS) goto Done;
					if (rel == UM_CIRCLE_REL || rel == NCL_CIRCLE_REL ||
						(NCLX_internal_geom && rel == NCLX_MDL_CIRCLE) ||
						rel == UM_COMPCRV_REL ||
						(NCLX_internal_geom && rel == NCLX_MDL_COMPOSITE))
					{
						if (!notrans)
						{
							struct NCL_fixed_databag c1, c2;
							UM_int2 lfl_77;
/* set ranfile label flag to temp unknown */
							lfl_77 = 1;
							stunlb (&lfl_77);

							c1.key = gkeys[i];
							status = ncl_retrieve_data_fixed (&c1);
							if (status == UU_SUCCESS)
								status = uc_copy (&c1, &c2, sizeof(c1));
							if (status == UU_SUCCESS)
								status = uc_transform (&c2, tran, UU_TRUE);
							if (status != UU_SUCCESS) goto Done;
							if (!nouvlst)
							{
								nouvlst = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
								uu_list_init (nouvlst, sizeof(UU_KEY_ID), 10, 10);
							}
							uu_list_push(nouvlst,&c2.key);
							gkeys[i] = c2.key;
/* reset ranfile label flag */
							stunlb (&lfl_77);
						}
					}
					else
						doboundary = UU_TRUE;
				}
			}
		}
	}

	doboundary = doboundary && (srf.key != srfkey || !bound1);

	if (doboundary)
	{
		UU_REAL tol = *dtol;
		UU_LOGICAL noinner = UU_FALSE;

		bound2 = (UM_srf_boundary *) uu_malloc (sizeof (UM_srf_boundary));
		status = ncl_initset_boundary (&srf,tran,bound2,noinner,tol);

		if (bound2->cvpts->cur_cnt < bound2->np[0] + 4)
			status = UU_FAILURE;
		else
		{
			UM_int2 type = NCLI_POINT;
			UM_coord *pts;
			int i,npt;

			pts = (UM_coord *) UU_LIST_ARRAY (bound2->cvpts);
			npt = bound2->cvpts->cur_cnt;
			for (i = 0; i < npt; i++)
				fr_unbs(pts[i],pts[i],&type);
		}
	}

Done:;
	if (status != UU_SUCCESS) *ier = 177;
	return;
}

/*********************************************************************
**    E_FUNCTION     : void gtpkys (i,key,ityp,ier)
**       Return data of element of composite curve for pocket.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of composite curve.
**          ix         - index of element
**       OUTPUT :  
**          keyout     - key of this entity
**          buf        - canonical data if line or circle.
**          ietype     - NCL type
**          ier       - 0 if no error, non-zero if error
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtpkys (i,key,ityp,ier)
UM_int2 *i,*ityp,*ier;
UM_int4 *key;
{
	int j,status;
	int rel;

	*key = 0;
	*ityp = 0;

	j = *i - 2;
	if (!gkeys || j < 0 || j >= num_gkeys)
	{
		*ier = 177;
		return;
	}
	if (gkeys[j] == 0) return;
	*key = gkeys[j];

	status = ur_retrieve_data_relnum (gkeys[j], &rel);
	if (status != UU_SUCCESS)
		return;
	if ((rel == UM_CIRCLE_REL) || (rel == NCL_CIRCLE_REL) ||
		(NCLX_internal_geom && rel == NCLX_MDL_CIRCLE))
		*ityp = 7;
	else if (rel == UM_COMPCRV_REL ||
		(NCLX_internal_geom && rel == NCLX_MDL_COMPOSITE))
		*ityp = 8;
	else
		*key = 0;

	return;
}

/*********************************************************************
**    E_FUNCTION     : void gtpln (ix,buf,lstored)
**       Return the stored contour plane
**    PARAMETERS
**       INPUT  :
**          ix                current loop index
**       OUTPUT :
**          lstored           1 iff the plane is stored, 0 else.
**          buf               plane data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtpln (ix,buf,lstored)
UM_int2 *ix,*lstored;
UM_real8 buf[];
{
	int i;

	if (Splnset)
	{
		*lstored = 1;
		for (i = 0; i < 6; i++)
			buf[i] = Splnbuf[i];
	}
	else
		*lstored = 0;

	return;
}

/*********************************************************************
**    E_FUNCTION     : void gtnpt(ix, npts, ier)
**       Return the number of points in the current ix-th loop
**    PARAMETERS
**       INPUT  :
**          ix                current loop index
**       OUTPUT :
**          npts              number of points.
**          ier               error (zero if none)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtnpt(ix, npts, ier)
UM_int2  *ix,*npts,*ier;
{
	int ib;
	UM_srf_boundary *b;

	*ier = 0;
	ib = *ix - 1;
	if (ib == 0 || !bound2)
		b = bound1;
	else
		b = bound2;

	if (!b || ib < 0 || ib >= b->nb)
	{
		*ier = 1;
		return;
	}

	*npts = b->np[ib];
	return;
}

/*********************************************************************
**    E_FUNCTION     : void gtpts (buf,ix,jx,ier)
**       Return the jx-th point in the current ix-th loop
**    PARAMETERS
**       INPUT  :
**          ix                current loop index
**          jx                current point index
**       OUTPUT :
**          buf               buffer to place the data
**          ier               error (zero if none)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtpts (buf,ix,jx,ier)
UM_int2  *ix,*jx,*ier;
UM_real8 buf[];
{
	int ib,i,j,np;
	UM_coord *vtx;
	UM_srf_boundary *b;

	*ier = 0;
	ib = *ix - 1;
	if (ib == 0 || !bound2)
		b = bound1;
	else
		b = bound2;
	if (!b || ib < 0 || ib >= b->nb)
	{
		*ier = 1;
		return;
	}
	j = *jx - 1;
	np = b->np[ib];
	if (j < 0 || j > np)
	{
		*ier = 1;
		return;
	}
	if (j == np) j = 0;
	vtx = (UM_coord *) UU_LIST_ARRAY (b->cvpts);
	for (i = 0; i < ib; i++) vtx += b->np[i];

	buf[0] = vtx[j][0];
	buf[1] = vtx[j][1];
	buf[2] = vtx[j][2];

	return;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_brkarc (ptlst,r,ccw,xc,yc,x1,y1,x2,y2,tol)
**       Break an arc into linear segments. 
**    PARAMETERS
**       INPUT  :
**          ptlst             list to update
**          r                 arc radius
**          ccw               arc direction (1- ccw, -1- cw)
**          xc,yc             arc center point
**          x1,y1,x2,y2       arc end points
**          tol               tolerance
**       OUTPUT :
**          ptlst             list updated
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_brkarc (ptlst,r,ccw,xc,yc,x1,y1,x2,y2,tol)
UU_LIST *ptlst;
UU_REAL r,xc,yc,x1,y1,x2,y2,tol;
int ccw;
{
	UM_coord pti;
	UU_REAL cosa,alph,dx,dy,ang,ang1,ang2,dang;
	int num,k;

	pti[2] = 0;

    if (r < 3*tol) return;

	cosa = (r-tol)/r;
	cosa = 2.*cosa*cosa -1.;
	alph = acos (cosa);
	dx = x1-xc;
	dy = y1-yc;
	ang1 = atan2 (dy,dx);
	dx = x2-xc;
	dy = y2-yc;
	ang2 = atan2 (dy,dx);
	if ((ang2-ang1)*ccw < 0) ang2 = 2.*UM_PI*ccw + ang2;
	ang = ang2 - ang1;
	num = ang/alph;
	num = abs(num) + 1;
	if (num < 4) num = 4;
	dang = ang/num;
	pti[0] = x1;
	pti[1] = y1;
	ang = ang1;

	for (k = 2; k < num; k++)
	{
		ang = ang+dang;
		dx = cos(ang);
		dy = sin(ang);
		pti[0] = xc+r*dx;
		pti[1] = yc+r*dy;
		uu_list_push (ptlst,pti);
	}

	return;
}

/*******************************************************************************
**    E_FUNCTION     : void lcrect
**       Calculate the minimum area rectangle around a 2-D contour.
**    PARAMETERS
**       INPUT  :
**          xa,ya             loop points
**          npa               number of points
**          ifl               1 - long, 2 - short
**          diam              minimal size for the rectangle sides
**          feps              tolerance
**       OUTPUT :
**          lcx,lcy           direction vector (along a side of the rectangle,
**                            long or short, as by ifl) 
**          vmin,vmax         the range in the perpendicular direction
**          u00,u10           range at vmin
**          u01,u11           range at vmax
**          ier               error (zero if none)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*******************************************************************************/
void lcrect (xa,ya,npa,ifl,diam,lcx,lcy,vmin,vmax,u00,u10,u01,u11,feps,ier)
UM_int2 *npa,*ifl;
UM_int4 *ier;
UM_real4 xa[],ya[],*lcx,*lcy,*vmin,*vmax,*u00,*u10,*u01,*u11,*feps;
UM_real8 *diam;
{
	int status,npt,i,j,imin;
	UU_REAL plz[2][3],dx,dy,tol;
	UM_coord *pts,pti,center,xaxis,yaxis;
	UU_LIST ptlst;
	UU_REAL r,x0,y0,x1,y1,x2,y2,u,v;
	int ccw;
	C2vh  cvh;
	UU_REAL d,xmax,xmin,ymax,area,areamin;
	UM_vector vc,xvec;

	*ier = 13;
	npt = *npa;
	cvh.cv = UU_NULL;

	if (npt < 0) return;
	uu_list_init (&ptlst,sizeof(UM_coord),npt,npt);

	if (!ptlst.data) return;

	tol = *feps;

	plz[0][0] = plz[0][1] = plz[0][2] = plz[1][0] = plz[1][1] = 0.;
	plz[1][2] = 1.;

	pti[2] = 0;

	for (j = 0; j < npt; j++)
	{
		pti[0] = xa[j]; pti[1] = ya[j];
		uu_list_push (&ptlst,pti);
		if (xa[j+1] > 0.9e+25)
		{
			r = xa[j+2];
			ccw = ya[j+2];
			x0 = xa[j+3];
			y0 = ya[j+3];
			x1 = xa[j];
			y1 = ya[j];
			x2 = xa[j+4];
			y2 = ya[j+4];
			ncl_brkarc (&ptlst,r,ccw,x0,y0,x1,y1,x2,y2,tol);
			j += 3;
		}
	}

	status = UU_SUCCESS;
	npt = ptlst.cur_cnt;
	pts = (UM_coord *) UU_LIST_ARRAY (&ptlst);

	um_nullvc (xaxis); um_nullvc (yaxis);
	um_vctovc (pts[0],center);
	if (npt < 3) goto Done;

	cvh.no_pt = npt;
	cvh.pt  = (UU_REAL (*)[3]) pts;
	cvh.cv  = (int *) uu_toolmalloc ((cvh.no_pt+1)*sizeof(int));
	um_plncvh(&cvh, plz);

	if (cvh.shape & UM_CVH_SMALL) goto Done;

	areamin = 1.e+50;
	imin = -1;
	for (i = 0; i < cvh.no_cv - 1; i++)
	{
		um_vcmnvc (pts[cvh.cv[i+1]], pts[cvh.cv[i]],xvec);
		d = UM_MAG (xvec);
		if (d < UM_DFUZZ) continue;
/*
..... um_plncvh is supposed to eliminate double points, so d is not 0
*/
		for (j = 0; j < 3; j++) xvec[j] /= d;
		x0 = y1 = 0.; x1 = d;
		for (j = 0; j < cvh.no_cv - 1; j++)
		{
			if (i == cvh.no_cv - 2 && j == 0) continue;
			if (j == i || j == i+1) continue;
			um_vcmnvc(pts[cvh.cv[j]], pts[cvh.cv[i]], vc);
			d = UM_DOT (xvec,vc);
			if (d > x1) x1 = d;
			if (d < x0) x0 = d;
			d = sqrt (UM_DOT(vc,vc) - d*d);
			if (d > y1) y1 = d;
		}
		area = (x1 - x0)*y1;
		if (area < areamin)
		{
			areamin = area; imin = i;
			xmin = x0; xmax = x1; ymax = y1;
			um_vctovc (xvec,xaxis);
			if (areamin < UM_DFUZZ) break;
		}
	}
	if (imin < 0) goto Done;

	dx = xmax - xmin;
	dy = ymax;
	if (dx < *diam + tol || dy < *diam + tol) goto Done;

	yaxis[0] = -xaxis[1]; yaxis[1] = xaxis[0];
	i = imin + 2;
	if (i >= cvh.no_cv) i -= cvh.no_cv;
	um_vcmnvc (pts[cvh.cv[i]],pts[cvh.cv[imin+1]],vc);
	if (um_dot (yaxis,vc) < 0.) um_negvc (yaxis,yaxis);
	um_unitvc (yaxis,yaxis);

	um_vctmsc(xaxis,0.5*(xmin+xmax),vc);
	um_vcplvc(pts[cvh.cv[imin]], vc, center);
	um_vctmsc(yaxis,0.5*ymax,vc);
	um_vcplvc(center, vc, center);

	*ier = 0;
	if ((dx >= dy && *ifl == 2) || (dx < dy && *ifl == 1))
	{
		xaxis[0] = yaxis[0]; xaxis[1] = yaxis[1];
		dy = dx;
	}
	yaxis[0] = -xaxis[1]; yaxis[1] = xaxis[0];
	*lcx = xaxis[0]; *lcy = xaxis[1];
	y0 = center[0]*yaxis[0] + center[1]*yaxis[1];
	*vmin = y0 - 0.5*dy; *vmax = y0 + 0.5*dy;
		
	x1 = x2 = 1.e+25; y1 = y2 = -1.e+25; 
	for (i = 0; i < cvh.no_cv; i++)
	{
		u = UM_DOT (pts[cvh.cv[i]],xaxis);
		v = UM_DOT (pts[cvh.cv[i]],yaxis);
		if (v < *vmin + tol)
		{
			if (u < x1) x1 = u;
			if (u > y1) y1 = u;
		}
		else if (v > *vmax - tol)
		{
			if (u < x2) x2 = u;
			if (u > y2) y2 = u;
		}
	}

	*u00 = x1; *u10 = y1; *u01 = x2; *u11 = y2;

Done:;
	if (cvh.cv) uu_toolfree (cvh.cv);
	uu_list_free (&ptlst);
	return;
}

/**********************************************************************
**    E_FUNCTION     : llini ()
**       Initialize list of points used for Lace Pocketing.
**********************************************************************/
void llini ()
{
	if (NCL_lace_init == 0)
	{
		uu_list_init (&NCL_lace,sizeof(NCL_lacept),1200,1200);
		NCL_lace_init = 1;
		NCL_lace_sort = 0;
	}
	return;
}

/**********************************************************************
**    E_FUNCTION     : lldel ()
**       Free list of points used for Lace Pocketing.
**********************************************************************/
void lldel ()
{
	if (NCL_lace_init == 1)
	{
		uu_list_free (&NCL_lace);
		NCL_lace_init = NCL_lace_sort = 0;
	}
	return;
}

/**********************************************************************
**    E_FUNCTION     : llnul ()
**       Reset list of points used for Lace Pocketing.
**********************************************************************/
void llnul ()
{
	if (NCL_lace_init == 0)
		llini();
	else
		NCL_lace.cur_cnt = 0;
	return;
}

/**********************************************************************
**    E_FUNCTION     : lisini ()
**       Initialize list of points used for Scrub Pocketing.
**********************************************************************/
void lisini (nis)
UM_int4 *nis;
{
	int i,num;

	num = *nis;

	if (NCL_lace_isl == UU_NULL)
	{
		NCL_lace_numisles = num;
		NCL_lace_isl = (UU_LIST *) uu_malloc (num*sizeof(UU_LIST));
		for (i = 0; i < NCL_lace_numisles; i++)
			uu_list_init (&NCL_lace_isl[i],sizeof(NCL_islept),0,200);
	}
	return;
}

/**********************************************************************
**    E_FUNCTION     : lisdel ()
**       Free list of points used for Scrub Pocketing.
**********************************************************************/
void lisdel ()
{
	int i;

	if (NCL_lace_isl)
	{
		for (i = 0; i < NCL_lace_numisles; i++)
			uu_list_free (&NCL_lace_isl[i]);
		UU_FREE (NCL_lace_isl);
	}
	return;
}

/**********************************************************************
**    E_FUNCTION     : lisrst ()
**       Reset list of points used for Scrub Pocketing.
**********************************************************************/
void lisrst ()
{
	int i;

	if (NCL_lace_isl)
	{
		for (i = 0; i < NCL_lace_numisles; i++)
		{
			if (NCL_lace_isl[i].data) NCL_lace_isl[i].cur_cnt = 0;
		}
	}

	return;
}

/**********************************************************************
**    E_FUNCTION     : lisadd ()
**       Add the points to the lists used for Scrub Pocketing.
**********************************************************************/
void lisadd (is,xa,ya,nppa)
UM_int2 *is,*nppa;
UM_real4 *xa,*ya;
{
	int i,j,n;
	NCL_islept ptisl;

	i = *is - 1;
	n = *nppa;
	ptisl.jcut = 0;

	if (NCL_lace_isl && i < NCL_lace_numisles && n > 0)
	{
		for (j = 0; j < n; j++)
		{
			ptisl.x = xa[j];
			ptisl.y = ya[j];
			uu_list_push (&NCL_lace_isl[i],&ptisl);
		}
	}

	return;
}

/**********************************************************************
**    E_FUNCTION     : lisjct (is,js,jcut)
**       Add the points to the lists used for Scrub Pocketing.
**********************************************************************/
void lisjct (is,jcut)
UM_int2 *is,*jcut;
{
	int i,j,n;
	NCL_islept *ptisl;

	i = *is - 1;

	if (NCL_lace_isl!= UU_NULL && i < NCL_lace_numisles)
	{
		ptisl = (NCL_islept *) UU_LIST_ARRAY (&NCL_lace_isl[i]);
		n = NCL_lace_isl[i].cur_cnt;
		for (j = 0; j < n; j++)
		{
			ptisl[j].jcut = jcut[j];
		}
	}

	return;
}

/**********************************************************************
**    E_FUNCTION     : lisget ()
**       Get points from the list.
**********************************************************************/
void lisget (is,xa,ya,jcut,nppa)
UM_int2 *is,*nppa,*jcut;
UM_real4 *xa,*ya;
{
	int i,j,n;
	NCL_islept *ptisl;

	i = *is - 1;

	if (NCL_lace_isl && i < NCL_lace_numisles)
	{
		n = NCL_lace_isl[i].cur_cnt;
		*nppa = n;
		ptisl = (NCL_islept *) UU_LIST_ARRAY (&NCL_lace_isl[i]);
		for (j = 0; j < n; j++, ptisl++)
		{
			xa[j] = ptisl->x;
			ya[j] = ptisl->y;
			jcut[j] = ptisl->jcut;
		}
	}

	return;
}

/**********************************************************************
**    E_FUNCTION     : llpush (i,lan,x)
**       Push a point onto the list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void llpush (i,lan,x)
UM_int2 *i,*lan;
UM_real4 *x;
{
	NCL_lacept next;

	next.index = *i;
	next.lane = *lan;
	next.x = *x;

	uu_list_push(&NCL_lace,&next);

	return;
}

/**********************************************************************
**    E_FUNCTION     : llpop (x)
**       Pop the point from the end of the list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          x     - the last point.
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : The list has one item less as a result.
**********************************************************************/
void llpop (ind,lan,x)
UM_int2 *ind,*lan;
UM_real4 *x;
{
	NCL_lacept *pts;
	int npt;

	*x = 1.e+25;
	npt = NCL_lace.cur_cnt;

	if (npt > 0)
	{
		pts = (NCL_lacept *) UU_LIST_ARRAY (&NCL_lace);
		*x = pts[npt-1].x;
		*ind = pts[npt-1].index; *lan = pts[npt-1].lane;
		NCL_lace.cur_cnt--;
	}

	return;
}

/**********************************************************************
**    E_FUNCTION     : int ncl_xpcmp(p1,p2)
**       Sorting comparison - positive X-direction.
**********************************************************************/
static int ncl_xpcmp(p1,p2)
NCL_lacept *p1,*p2;
{
	if (p1->x > p2->x) return(-1);
	else if (p1->x < p2->x) return(1);
	else return(0);
}

/**********************************************************************
**    E_FUNCTION     : int ncl_xncmp(p1,p2)
**       Sorting comparison - negative X-direction.
**********************************************************************/
static int ncl_xncmp(p1,p2)
NCL_lacept *p1,*p2;
{
	if (p1->x > p2->x) return(1);
	else if (p1->x < p2->x) return(-1);
	else return(0);
}

/**********************************************************************
**    E_FUNCTION     : llsort (npp,pm,ftol,ier)
**       Sort list of points used for Lace Pocketing.
**    PARAMETERS
**       INPUT  :
**          pm     - sort in positive X direction iff 1, negative iff -1
**       OUTPUT :
**          npp    - number of points in the list.
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void llsort (npp,pm)
UM_int2 *npp,*pm;
{
	NCL_lacept *pts;
	int npt;

	*npp = 0;

	pts = (NCL_lacept *) UU_LIST_ARRAY (&NCL_lace);
	npt = NCL_lace.cur_cnt;
	*npp = npt;
	if (npt < 2) return;

	if (*pm == 1)
		uu_qsort (pts,npt,sizeof(NCL_lacept),ncl_xpcmp);
	else
		uu_qsort (pts,npt,sizeof(NCL_lacept),ncl_xncmp);

	return;
}

/*********************************************************************
**    E_FUNCTION     : void initpokpols(tol)
**       Initialize pocket coverage polygons
*********************************************************************/
void initpokpols(tol)
UM_real4 *tol;
{
	Stol = *tol;
	Stolsq = Stol*Stol;

	if (Spol0init == 0)
	{
		Spol0.num_contours = 0;
		Spol0.np = UU_NULL;
		Spol0.box = (UM_2box *) UU_NULL;
		uu_list_init (&Snplst,sizeof(int),0,10);
		uu_list_init (&Sboxlst,sizeof(UM_2box),0,10);
		uu_list_init (&Scontour0,sizeof(UM_2Dcoord),0,100);
		Spol0.contour = &Scontour0;
		Spol0init = 1;
	}
	if (Spol1init == 0)
	{
		Spol1.num_contours = 0;
		Spol1.np = UU_NULL;
		Spol1.box = (UM_2box *) UU_NULL;
		uu_list_init (&Scontour1,sizeof(UM_2Dcoord),0,100);
		uu_list_init (&Snjlist, sizeof(int), 10, 10);
		Spol1.contour = &Scontour1;
		Spol1init = 1;
	}
	if (Spol2init == 0)
	{
		Spol2.num_contours = 0;
		Spol2.np = &Snp2;
		Spol2.box = (UM_2box *) UU_NULL;
		uu_list_init (&Scontour2,sizeof(UM_2Dcoord),0,100);
		Spol2.contour = &Scontour2;
		Spol2init = 1;
	}
}

/*********************************************************************
**    E_FUNCTION     : void rstpokpol(it)
**       Reset pocket coverage polygons
**    PARAMETERS
**       INPUT  :
**          it     - Spol0 if 0, Spol1 if 1
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void rstpokpol(it)
UM_int2 *it;
{
	int itsk = *it;

	if (itsk == 0 && Spol0init == 1)
	{
		Spol0.num_contours = 0;
		Spol0.np = UU_NULL;
		Spol0.box = (UM_2box *) UU_NULL;
		UU_LIST_EMPTY (&Snplst);
		UU_LIST_EMPTY (&Sboxlst);
		UU_LIST_EMPTY (&Scontour0);
	}
	else if (itsk == 1 && Spol1init == 1)
	{
		Spol1.num_contours = 0;
		UU_FREE (Spol1.np);
		UU_FREE (Spol1.box);
		UU_LIST_EMPTY (&Scontour1);
		UU_LIST_EMPTY (&Snjlist);
	}
}

/*********************************************************************
**    E_FUNCTION     : void freepokpols()
**       Free pocket coverage polygons
*********************************************************************/
void freepokpols()
{
	if (Spol0init == 1)
	{
		Spol0.num_contours = 0;
		uu_list_free (&Snplst);
		Spol0.np = UU_NULL;
		uu_list_free (&Sboxlst);
		Spol0.box = (UM_2box *) UU_NULL;
		uu_list_free (&Scontour0);
		Spol0.contour = NULLST;
		Spol0init = 0;
	}
	if (Spol1init == 1)
	{
		Spol1.num_contours = 0;
		uu_list_free (&Snplst);
		UU_FREE (Spol1.np);
		UU_FREE (Spol1.box);
		uu_list_free (&Scontour1);
		Spol0.contour = NULLST;
		uu_list_free (&Snjlist);
		Spol1init = 0;
	}
	if (Spol2init == 1)
	{
		Spol2.num_contours = 0;
		Spol2.np = UU_NULL;
		uu_list_free (&Scontour2);
		Spol2.contour = NULLST;
		Spol2init = 0;
	}
}

/*********************************************************************
*********************************************************************/
static void S_update_box (bx,vt)
UM_2box *bx;
UM_2Dcoord vt;
{
	if (bx->xmin > vt[0]) bx->xmin = vt[0];
	if (bx->ymin > vt[1]) bx->ymin = vt[1];
	if (bx->xmax < vt[0]) bx->xmax = vt[0];
	if (bx->ymax < vt[1]) bx->ymax = vt[1];
}

/*********************************************************************
*********************************************************************/
static void S_update_pol (npt,xa,ya,lst,bx)
int npt;
UM_real4 *xa,*ya;
UU_LIST *lst;
UM_2box *bx;
{
	int i;
	UM_2Dcoord vti;

	if (bx != UU_NULL)
	{
		bx->xmin = bx->ymin = 1.e25;
		bx->xmax = bx->ymax = -1.e25;
	}

	for (i = 0; i < npt; i++)
	{
		vti[0] = xa[i]; vti[1] = ya[i];
		if (bx != UU_NULL) S_update_box (bx,vti);
		uu_list_push (lst,vti);
	}
}

/*********************************************************************
**    E_FUNCTION     : void addpokpol (it,xa,ya,nppa)
**       Add a countour to a polygon
**    PARAMETERS
**       INPUT  :
**          it     - Spol0 if 0, Spol1 if 1
**          xa,ya  - contour points
**          nppa   - number of contour points
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void addpokpol (it,xa,ya,nppa)
UM_int2 *it,*nppa;
UM_real4 *xa,*ya;
{
	int itsk,npt;
	int status = UU_SUCCESS;
	UM_2box bx;

	itsk = *it;
	npt = *nppa;

	if (itsk == 0 && Spol0init == 1)
	{
		Spol0.num_contours++;
		uu_list_push (&Snplst,&npt);
		S_update_pol (npt,xa,ya,&Scontour0,&bx);
		uu_list_push (&Sboxlst,&bx);
		Spol0.np = (int *) UU_LIST_ARRAY (&Snplst);
		Spol0.box = (UM_2box *) UU_LIST_ARRAY (&Sboxlst);
	}
	else if (itsk == 1 && Spol1init == 1)
	{
		if (Spol1.num_contours == 0)
		{
			Spol1.num_contours = 1;
			Spol1.np = (int *) uu_malloc (sizeof(int));
			Spol1.np[0] = npt;
			S_update_pol (npt,xa,ya,&Scontour1,(UM_2box *)UU_NULL);
		}
		else if (Spol2init == 1)
		{
			Spol2.num_contours = 1;
			Spol2.np[0] = npt;
			UU_LIST_EMPTY (&Scontour2);
			S_update_pol (npt,xa,ya,&Scontour2,(UM_2box *)UU_NULL);

			status =
			ncl_polygon_clip (NCL_CONTOUR,&Spol1,&Spol2,&Spol1,Stol,Stolsq);

			ncl_prepare_for_pocketing (&Spol1,Stol,Stolsq);
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_remove_unwhole (pol)
**       Delete contours with holes.
**    PARAMETERS
**       INPUT  :
**          pol        - polygon structure
**       OUTPUT :
**          pol        - updated polygon structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_remove_unwhole (pol)
ncl_polygon *pol;
{
	int nc,c,nv,k,nv0,*np;

	nc = pol->num_contours;
	np = pol->np;

	if (nc > 0)
	{
		for (c = 0, nv0 = 0; c < nc; c++)
		{
			nv = np[c];

			if (nv < 3 || (c+1 < nc && np[c+1] < 0))
			{
				nv = abs(nv);
				if (nv > 0)
				uu_list_delete (pol->contour,nv0,nv);
				
				for (k = c+1; k < nc; k++)
					pol->np[k-1] = pol->np[k];
				
				nc--; c--;
				continue;
			}
			else
				nv0 += nv;
		}
	}
	pol->num_contours = nc;

	return;
}

/*********************************************************************
**    I_FUNCTION     : S_area_test (pol,tolsq)
**       Weed out polygon contours if too small.
**    PARAMETERS
**       INPUT  :
**          pol        - polygon structure
**          tolsq      - tolerance (squared)
**       OUTPUT :
**          pol        - updated polygon structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_area_test (pol,areamin,tolsq)
ncl_polygon *pol;
UU_REAL areamin,tolsq;
{
	int nc,c,nv,i,k,nv0;
	UM_2Dcoord *pp;
	UU_REAL a,ai,area = 0.;

	nc = pol->num_contours;

	if (nc > 0)
	{
		for (c = 0, nv0 = 0; c < nc; c++)
		{
			nv = pol->np[c];
			pp = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
			pp += nv0;
			a = UM_SQDIS_2D(pp[0],pp[1]);
			ai = UM_SQDIS_2D(pp[0],pp[nv-1]);
			if (ai > a) a = ai;
			for (i = 1, area = 0.; i < nv - 1; i++)
			{
				area += um_triangle_signed_area(pp[0],pp[i],pp[i+1]);
				ai = UM_SQDIS_2D(pp[i],pp[i+1]);
				if (ai > a) a = ai;
			}
			if ((fabs(area) < areamin) || (nv < 5 && area*area < a*tolsq))
			{
				uu_list_delete (pol->contour,nv0,nv);
				
				for (k = c+1; k < nc; k++)
					pol->np[k-1] = pol->np[k];
				
				nc--; c--;
			}
			else
				nv0 += nv;
		}
	}
	pol->num_contours = nc;

	return;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_calc_boxes (pol)
**       Calculate boxes for a polygon.
**    PARAMETERS
**       INPUT  :
**          pol       - polygon
**       OUTPUT :
**          pol       - polygon updated
**    RETURNS      : none
**    SIDE EFFECTS : pol->box is allocated
**    WARNINGS     : none
*********************************************************************/
void ncl_calc_boxes (pol)
ncl_polygon *pol;
{
	int i,nv,c,nc;
	UM_2box *bx;
	UM_2Dcoord *pp;

	nc = pol->num_contours;
	if (nc <= 0) return;

	pol->box = (UM_2box *) uu_malloc (nc*sizeof(UM_2box));

	bx = pol->box;
	pp = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
	for (c = 0; c < nc; c++)
	{
		nv = abs(pol->np[c]);
		if (nv > 0)
		{
			bx->xmin = bx->xmax = pp[0][0];
			bx->ymin = bx->ymax = pp[0][1];

			for (i = 1; i < nv; i++)
				S_update_box (bx,pp[i]);
		}

		pp += nv;
		bx++;
	}
}

/*********************************************************************
**    E_FUNCTION     : void subpokpols (ar0,ngaps)
**       Subtract pocket coverage polygons to see if there is a gap in between
**    PARAMETERS
**       INPUT  :
**          ar0       - smallest area parameter to weed the result
**       OUTPUT :
**          ngaps     - number of gap contours; 0 if no gaps
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void subpokpols (ar0,ngaps)
UM_real4 *ar0;
UM_int2 *ngaps;
{
	int status;
	UU_REAL areamin;
	UM_2Dcoord nrpt;

	*ngaps = 0;

	if (Spol0init == 1 && Spol0.num_contours > 0 &&
		Spol1init == 1 && Spol1.num_contours > 0)
	{
		ncl_calc_boxes (&Spol1);

		status = ncl_polygon_clip (NCL_DIFF,&Spol0,&Spol1,&Spol1,Stol,4.*Stolsq);

		if (status == UU_SUCCESS)
		{
			areamin = *ar0;
			if (Spol1.num_contours > 1)
			{
				nrpt[0] = nrpt[1] = 1.e25;
				status =
				ncl_order_pock_geo1 (&Spol1,&Snjlist,0,0,0,nrpt,UU_NULL,Stol);
			}
			if (status == UU_SUCCESS)
			{
				S_remove_unwhole (&Spol1);
				S_area_test (&Spol1,areamin,Stolsq);
				ncl_weed_contours(&Spol1,Stolsq);
				if (Spol1.num_contours > 0)
				{
					*ngaps = Spol1.num_contours;
				}
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : void getpolb (it,xa,ya,nppa)
**       Return a coverage gap polygon contour
**    PARAMETERS
**       INPUT  :
**          it     - contour number
**       OUTPUT :
**          xa,ya  - contour points
**          nppa   - number of contour points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void getpolb (it,xa,ya,nppa)
UM_int2 *it,*nppa;
UM_real4 *xa,*ya;
{
	int i,j,nv,c;
	UM_2Dcoord *pp;
	UU_REAL d;

	c = *it - 1;
	*nppa = 0;

	if (c >= 0 && c < Spol1.num_contours)
	{
		pp = (UM_2Dcoord *) UU_LIST_ARRAY (&Scontour1);
		for (i = 0; i < c; i++)
		{
			nv = Spol1.np[i];
			nv = abs(nv);
			pp += nv;
		}
			
		nv = Spol1.np[c];
		if (nv > 2)
		{
			d = UM_SQDIS_2D(pp[0],pp[nv-1]);
			for (j = 0; j < nv; j++)
			{
				xa[j] = pp[j][0];
				ya[j] = pp[j][1];
			}
			if (d > Stolsq)
			{
				xa[nv] = pp[0][0];
				ya[nv] = pp[0][1];

				*nppa = nv + 1;
			}
			else
				*nppa = nv;
		}
	}
}
