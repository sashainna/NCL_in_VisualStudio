/*********************************************************************
**    NAME         :  nesfsup.c
**       CONTAINS:
**
**           int ncl_sf_d_endpts
**           int ncl_rev_normal
**           int ncl_surface_ploc_to_coord
**           int ncl_surf_feat
**           int ofwfsf
**           void nclf_swapuv
**           int ncl_sf_offset
**           int gtsfdp
**           int ptsfdp
**           void pttess
**           void initss
**           int ncl_draw_netsf
**           int ncl_disp_surf
**           int pttrim
**           int ptrld
**           int gtrld1
**           void ncl_redisp_sfs
**           void ncl_disp_sfs
**           void ncl_disp_params
**           void ncl_get_dispmodes
**           int ncl_reset_assoc
**           int ncl_gtssnum
**           int ncl_redisp_ss
**           int ncl_tessellate (obsolete, not compiled)
**
**    COPYRIGHT 1991 (c) Numerical Control Computer Sciences Inc.
**                       All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesfsup.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:48
*********************************************************************/

#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "dasnog.h"
#include "dselmask.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msol.h"
#include "mattr.h"
#include "modef.h"
#include "mdeval.h"
#include "mgeom.h"
#include "mdpick.h"
#include "msrf.h"
#include "nccs.h"
#include "ncl.h"
#include "ulist.h"
#include "nclfc.h"

static struct UC_attributedatabag *S_netsf_attr = UU_NULL;

/*********************************************************************
**    E_FUNCTION     : int ncl_sf_d_endpts(level,pickpath,pickloc,
**                                     rel_num,cpt,opt)
**      Get the cartesian  coordinates of the end points of a
**      surface. The closest point to the pick location is the
**       first point.
**    PARAMETERS
**       INPUT  :
**          levl                    level of entity picked
**          pickpath                pick path
**          pickloc                 pick location
**       OUTPUT :
**          rel_num                 relation number of entity picked
**          cpt                     closest end point to pick
**          opt                     closest end point to pick
**    RETURNS      :
**       0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    NOTE         : Copied from model/m2dasup1.c - um_d_endpts
*********************************************************************/
int
ncl_sf_d_endpts(level,pickpath,pickloc,rel_num,cpt,opt)
   int level;
   UD_PPICKREC *pickpath;
   UD_NDCLOCREC *pickloc;
   int *rel_num;
   UM_coord cpt;
   UM_coord opt;

   {
   struct NCL_fixed_databag e;         /* data for picked entity */
   UM_transf tfmat;                    /* transformation matrix */
   UM_PICKENT pent;
   int status;
   struct UM_evsrfout *evsrf;
   UM_coord p[4];
   int closest;


   uu_denter(UU_MTRC,(us,"ncl_sf_d_endpts(level=%d,pickpath=%x,pickloc=%d)",
      level,pickpath,pickloc));

   evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
   status = um_d_pickresolve(pickpath, level, &pent);
   if (status == 0)
      {
      e.key = um_get_pickkey(&pent, level);
      um_retrieve_data_relnum(e.key, &e.rel_num);
      status = uc_retrieve_data(&e, sizeof(e));
      if (status != UU_SUCCESS) goto done;
      status = uc_retrieve_transf(e.key, tfmat);
      if (status != UU_SUCCESS) goto done;
      uc_init_evsrfout(&e, evsrf);
      status = uc_evsrf(UM_POINT, 0.0, 0.0, &e, tfmat, evsrf);
      if (status != UU_SUCCESS) goto done;
      um_vctovc(evsrf->sp, p[0]);
      status = uc_evsrf(UM_POINT, 1.0, 0.0, &e, tfmat, evsrf);
      if (status != UU_SUCCESS) goto done;
      um_vctovc(evsrf->sp, p[1]);
      status = uc_evsrf(UM_POINT, 0.0, 1.0, &e, tfmat, evsrf);
      if (status != UU_SUCCESS) goto done;
      um_vctovc(evsrf->sp, p[2]);
      status = uc_evsrf(UM_POINT, 1.0, 1.0, &e, tfmat, evsrf);
      if (status != UU_SUCCESS) goto done;
      um_vctovc(evsrf->sp, p[3]);
      closest = um_nearest_to_ploc(pickloc, 4, p);
      um_vctovc(p[closest], cpt);
      um_vctovc(p[closest], opt);

      status = UU_SUCCESS;
      }
done:
   uu_free(evsrf);
   uu_dexitstatus("ncl_sf_d_endpts", status);
   return (status);
   }

#if 0
/*********************************************************************
**    E_FUNCTION     : int ncl_tessellate(srf)
**       Tessellate the specified surface (SRF).
**    PARAMETERS
**       INPUT  :
**          srf            RBS, NCL or MESH surface
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : Relies on ptsperucrv, ptspervcrv & rev_normal fields
**                   being in same place in record for all surface types.
*********************************************************************/
int
ncl_tessellate(srf)
   struct UM_rbsplsrf_rec *srf;
   {
   int status;
   int nu, nv;
   int i, k, l;
   UU_REAL u, du, v, dv;
   UU_REAL *temp;
   UU_REAL *pt0, *norm0;
   UU_REAL *pt1, *norm1;
   UU_REAL point0[200][3], normal0[200][3];
   UU_REAL point1[200][3], normal1[200][3];
   UM_transf tfmat;
   struct UM_evsrfout evsrf;

   uu_denter(UU_MTRC,(us,"ncl_tessellate(srf(r,k)=(%d,%x))",
      srf->rel_num, srf->key));

   status = um_get_transformation(srf->key, tfmat);
   nu = srf->ptsperucrv;
   nv = srf->ptspervcrv;
   du = 1./(nu-1);
   dv = 1./(nv-1);

   pt0 = &point0[0][0];
   norm0 = &normal0[0][0];
   pt1 = &point1[0][0];
   norm1 = &normal1[0][0];
   u = 0.;
   for (k=0, l=0, v=0.; k<nv; k++, l=l+3, v+=dv)
      {
      if (v>1.)v=1.;
      status = uc_evsrf(UM_NORM, u, v, srf, tfmat, &evsrf);
      um_vctovc (evsrf.sp, &pt0[l]);
      um_vctovc (evsrf.snorm, &norm0[l]);
      }

   for (i=1, u=du; i<nu; i++, u += du)
      {
      if (u>1.)u=1.;
      for (k=0, l=0, v=0.; k<nv; k++, l=l+3, v+=dv)
         {
         if (v>1.)v=1.;
         status = uc_evsrf(UM_NORM, u, v, srf, tfmat, &evsrf);
         um_vctovc (evsrf.sp, &pt1[l]);
         um_vctovc (evsrf.snorm, &norm1[l]);
         }

      um_tessellate(nv, pt0, norm0, pt1, norm1, srf->rev_normal);
      temp = pt1;
      pt1 = pt0;
      pt0 = temp;
      temp = norm1;
      norm1 = norm0;
      norm0 = temp;
      }
   status = UU_SUCCESS;

   uu_dexitstatus("ncl_tessellate", status);
   return (status);
   }
#endif
/*********************************************************************
**    E_FUNCTION     : ncl_rev_normal()
**       Prompt the user to pick surfaces for which the normal is
**       to be reversed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_rev_normal()

   {
   UU_LOGICAL initialize;
   int status;
   int numint;
   UU_KEY_ID key;
   int rel_num;

   uu_denter(UU_MTRC,(us,"ncl_rev_normal()"));

   ud_lgeo(UU_TRUE, UD_render);

   ud_ldas(UD_DASSELECT,/* pick surface to reverse normal */UM_APPGEO,
         36, UU_NULL, 1, &numint, UD_NODEFAULT);
   if (numint < 1) goto done;

   initialize = UU_TRUE;
   while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
      {
      initialize = UU_FALSE;
      status = ur_retrieve_data_relnum (key, &rel_num);
      if (status == UU_SUCCESS)
         {
         if (rel_num == UM_AGSHELL_REL)
            {
            struct UM_agshell_rec shell;
            shell.key = key;
            status = ur_retrieve_data_fixed (&shell);
            ag_sh_flipnrm(shell.shelladdr);
            }
         else if (rel_num == NCL_REVSURF_REL)
            {
            struct NCL_revsurf_rec srf;
            srf.key = key;
            status = ur_retrieve_data_fixed (&srf);
            srf.rev_normal = !srf.rev_normal;
            ur_update_data_fixed (&srf);
            }
         else /* All other surfs must have rev_normal flag in same pos in rec */
            {
            struct UM_rbsplsrf_rec srf;
            srf.key = key;
            status = ur_retrieve_data_fixed (&srf);
            srf.rev_normal = !srf.rev_normal;
            ur_update_data_fixed (&srf);
            }
         }
      }

done:;
   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION    : int ncl_surface_ploc_to_coord(level,pickpath,pickloc,pt)
**       Determine a cartesian coordinate (PT) given a picked location
**       on a surface (LEVEL, PICKPATH, PICKLOC).
**    PARAMETERS
**       INPUT  :
**          level             level of entity picked within pickpath
**          pickpath          DAS pick path record
**          pickloc           DAS pickloc record
**       OUTPUT :
**          pt                cartesian coordinate (MCS) corresponding
**                            to picked location on entity
**    RETURNS      :
**       UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_surface_ploc_to_coord(level, pickpath, pickloc, pt)
   int level;
   UD_PPICKREC *pickpath;
   UD_NDCLOCREC *pickloc;
   UM_coord pt;

   {
   UM_PICKENT pent;
   struct NCL_fixed_databag e;
   UM_transf tfmat;
   struct UM_evsrfout evsrf;
   UM_coord p[4];
   int closest;
   int status;

   uu_denter(UU_MTRC,(us,"ncl_surface_ploc_to_coord(level=%d,pickpath=%x,pickloc=%x)",
      level, pickpath, pickloc));

   status = um_d_pickresolve(pickpath, level, &pent);
   if (status != UU_SUCCESS) goto done;

   e.key = um_get_pickkey(&pent, level);
   status = ncl_retrieve_data(&e, sizeof(e));
   if (status != UU_SUCCESS) goto done;
   status = uc_retrieve_transf(e.key, tfmat);
   if (status != UU_SUCCESS) goto done;

   status = uc_init_evsrfout(&e, &evsrf);
   if (status != UU_SUCCESS) goto done;

   status = uc_evsrf(UM_POINT, (UU_REAL) 0.0, (UU_REAL) 0.0, &e, tfmat, &evsrf);
   if (status != UU_SUCCESS) goto done;
   um_vctovc(evsrf.sp, p[0]);

   status = uc_evsrf(UM_POINT, (UU_REAL) 0.0, (UU_REAL) 1.0, &e, tfmat, &evsrf);
   if (status != UU_SUCCESS) goto done;
   um_vctovc(evsrf.sp, p[1]);

   status = uc_evsrf(UM_POINT, (UU_REAL) 1.0, (UU_REAL) 0.0, &e, tfmat, &evsrf);
   if (status != UU_SUCCESS) goto done;
   um_vctovc(evsrf.sp, p[2]);

   status = uc_evsrf(UM_POINT, (UU_REAL) 1.0, (UU_REAL) 1.0, &e, tfmat, &evsrf);
   if (status != UU_SUCCESS) goto done;
   um_vctovc(evsrf.sp, p[3]);

   closest = um_nearest_to_ploc(pickloc, 4, p);
   um_vctovc(p[closest], pt);

done:;

   uu_dexitstatus("ncl_surface_ploc_to_coord",status);
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_surf_feat(eptr, tfmat, feature, dploc)
**       Display features for the rbsf.
**    PARAMETERS
**       INPUT  :
**          eptr     pointer to an entity
**          tfmat    transformation matrix
**          feature  order of feature to display
**          dploc    picked location on surface
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_surf_feat(eptr, tfmat, feature, dploc)
   struct UM_rbsplsrf_rec  *eptr;
   UM_transf tfmat;
   int feature;
   UD_PLOCREC *dploc;
{
	int status = UU_SUCCESS;
	struct UM_evsrfout evout;
	UM_vector vect;
	UM_vector pos;
	int i,j;
	UM_param u,v;
	char ustr[5];
	char vstr[5];
	UU_REAL char_height;
	UU_REAL norm_fact;
	UM_coord vprefpt;
	UM_vector vpup;
	UM_vector vpnorm;
	UM_vector vptan;
	UM_length aperture;
	struct NCL_revsurf_rec *rptr;

	uu_denter(UU_MTRC,(us,"ncl_surf_feat(key=%x, tfmat=%x, feature=%d)",
      eptr->key, tfmat, feature));

   /* get the view parameters for the currently active normtran
      (i.e. the view that the features will be displayed in) */
	um_get_current_viewplane(vprefpt, vptan, vpup, vpnorm, &aperture);

   /* set feature character height */
	um_set_feature_char_height(&char_height);
	norm_fact = 3.0 * char_height;

	if (eptr->rel_num == NCL_REVSURF_REL)
	{
		rptr  = (struct NCL_revsurf_rec *)eptr;
		if (rptr->rev_normal) norm_fact = - norm_fact;
	}
	else if (eptr->rev_normal) norm_fact = - norm_fact;

	uc_init_evsrfout(eptr, &evout);

	for (i=0; i<2; i++)
		for (j=0; j<2; j++)
		{
         /* evaluate surface data at (u,v) parameter values */
			u = 1.0 * i;
			v = 1.0 * j;
			uc_evsrf(UM_NORM, u, v, eptr, tfmat, &evout);

         /* surface normal vector */
			um_unitvc (evout.snorm, vect);
			um_vctmsc (vect, norm_fact, vect);
			status = um_feavect (evout.sp, vect);
			if (status!=0)
				return UU_FAILURE;

         /* U curve */
			um_unitvc (evout.dsdu, vect);
			um_vctmsc (vect, 3.0 * char_height, vect);
			status = um_feavect (evout.sp, vect);
			if (status!=0)
				return UU_FAILURE;

			um_vctmsc(vect, (UU_REAL) 1.1, vect);
			um_vcplvc(evout.sp, vect, pos);
			sprintf(ustr,"U%d",i);
			status = um_feareal(0, ustr, u, pos, pos, UU_FALSE);
			if (status!=0)
				return UU_FAILURE;

         /* V curve */
			um_unitvc (evout.dsdv, vect);
			um_vctmsc (vect, 3.0 * char_height, vect);
			status = um_feavect (evout.sp, vect);
			if (status!=0)
				return UU_FAILURE;
			um_vctmsc(vect, (UU_REAL) 1.1, vect);
			um_vcplvc(evout.sp, vect, pos);
			sprintf(vstr,"V%d",j);
			status = um_feareal(0, vstr, v, pos, pos, UU_FALSE);
			if (status!=0)
				return UU_FAILURE;
		}

	uu_dexitstatus("ncl_surf_feat", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_sf_offset (lcopy, eptr, cdis)
**       Offset a wireframe surface.
**    PARAMETERS
**       INPUT  :
**          eptr       - pointer to surface
**          cdis       - offset distance
**          lcopy      - true if a copy is offset
**       OUTPUT :
**          eptr       - pointer to offset surface
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sf_offset (lcopy, eptr, cdis)
UU_LOGICAL lcopy;
struct NCL_fixed_databag *eptr;
UU_REAL cdis;
{
	int status = UU_FAILURE;

	if (eptr->rel_num == UM_RBSPLSRF_REL)
		status = ncl_offset_rbsf(eptr, cdis);
	else if (eptr->rel_num == NCL_TRIMSF_REL)
		status = ncl_offset_trimsf(eptr, cdis);
	else if (eptr->rel_num == NCL_EVALSF_REL)
		status = ncl_offset_evalsf(eptr, cdis);
	else if (eptr->rel_num == NCL_REVSURF_REL)
		status = ncl_offset_revsf(lcopy,eptr, cdis);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ofwfsf (keyin, vec, dist, irs, rmx, keyout)
**       Offset a wireframe surface.
**    PARAMETERS
**       INPUT  :
**          keyin      - key of entity
**          vec        - direction vector
**          dist       - offset distance
**          irs        - refsys flag
**          rmx        - refsys matrix
**          iws        - wrksys flag
**          wmx        - wrksys matrix
**       OUTPUT :
**          keyout     - key of copied & offset surface
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ofwfsf (keyin, vec, dist, irs, rmx, iws, wmx, keyout)
   UM_int4 *keyin, *keyout;
   UM_int2 *irs, *iws;
   UM_real8 vec[3], *dist, rmx[12], wmx[12];

   {
   int status, i, j, ix;
   UU_LOGICAL lcopy;
   UM_transf tfmat;
   UU_KEY_ID key1;
   struct NCL_fixed_databag sf1, sf2;
   UM_param u, v;
   struct UM_evsrfout evsrf;
   struct UM_transf_rec tran;
   UM_vector vn;
   UU_REAL cdis;
   UU_REAL scl, um_mag();

   uu_denter(UU_MTRC,(us,"ofwfsf (key=%x, rmx=%x)", keyin, rmx));

   status = UU_FAILURE;

   *keyout = 0;
   key1 = *keyin;
   sf1.key = key1;
   if (ncl_retrieve_data (&sf1, sizeof(sf1)) == 0)
      {
      tran.key = key1;
      tran.rel_num = UM_TRANSFORM_REL;
      if (ur_retrieve_transf(&tran) == 0)
         {
         uc_init_evsrfout (&sf1, &evsrf);
         u = .5;
         v = .5;
         uc_evsrf (UM_FRSTDERIV, u, v, &sf1, tran.tfmat, &evsrf);
         um_cross (evsrf.dsdu, evsrf.dsdv, vn);
         scl = 1.0;
         um_unitvc (vn, vn);
         if (*irs)
            {
            for (i=0, ix=0; i<3; i++)
               {
               for (j=0; j<4; j++) tfmat[j][i] = rmx[ix++];
               }
            um_vctmtf (vn, tfmat, vn);
            scl = um_mag (vn);
            }
         if (*iws)
            {
            for (i=0, ix=0; i<3; i++)
               {
               for (j=0; j<4; j++) tfmat[j][i] = wmx[ix++];
               }
            um_vctmtf (vn, tfmat, vn);
            scl = um_mag (vn);
            }
         cdis = *dist * scl;
         if (um_dot(vec,vn) < 0.) cdis = -cdis;
         if (uc_copy (&sf1, &sf2, sizeof(sf1)) == 0)
           {
/*
.....Do not copy xyzbylst when doing offset
*/
		  if (sf2.rel_num == NCL_TRIMSF_REL)				
			  ncl_lst_delete(WHOLE_BOUNDARY_LIST,&sf2.key);

           lcopy = UU_TRUE;
           ncl_sf_offset (lcopy,&sf2, cdis);

           if (ur_update_data_fixed(&sf2) == 0)
             {
             status = UU_SUCCESS;
             ncl_def_color (sf2.key);

             *keyout = sf2.key;
             }
           }
         }
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : nclf_swapuv (nclkey,iswap)
**       Set a surface swapuv flag.
**    PARAMETERS
**       INPUT  :
**          nclkey     - Key of surface.
**          iswap      - Value to set
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_swapuv (nclkey,iswap)
UM_int4 *nclkey;
UM_int2 *iswap;
{
	int status,swapfl;
	UU_LOGICAL update = UU_TRUE;
	UM_int4 key;
	struct NCL_fixed_databag e1;
	struct NCL_surface_rec *nsf;
	struct NCL_revsurf_rec *vsf;
	struct NCL_meshsf_rec *msf;
	struct UM_rbsplsrf_rec *rsf;

	swapfl = *iswap;

	e1.key = *nclkey;
	status = ncl_retrieve_data_fixed(&e1);

	if (status == UU_SUCCESS)
	{
		switch (e1.rel_num)
		{
		case NCL_SURF_REL:
			nsf = (struct NCL_surface_rec *)&e1;
			nsf->swapuv = nsf->swapuv^swapfl;
			break;
		case NCL_REVSURF_REL:
			vsf = (struct NCL_revsurf_rec *)&e1;
			vsf->swapuv = vsf->swapuv^swapfl;
			break;
		case NCL_MESHSURF_REL:
			msf = (struct NCL_meshsf_rec *)&e1;
			msf->swapuv = msf->swapuv^swapfl;
			break;
		case UM_RBSPLSRF_REL:
			rsf = (struct UM_rbsplsrf_rec *)&e1;
			rsf->swapuv = rsf->swapuv^swapfl;
			break;
		case NCL_TRIMSF_REL:
			key = ((struct NCL_trimsf_rec *)&e1)->bs_key;
			nclf_swapuv (&key,iswap);
			update = UU_FALSE;
			break;
		default:
			break;
		}
		if (update) status = ur_update_data_fixed(&e1);
	}
}

/*********************************************************************
**    E_FUNCTION     : int gtsfdp (kupat,kupts,kvpat,kvpts)
**      Get the CAD surface display parameters.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          kupat                   number of U paths
**          kupts                   number of points in U path
**          kvpat                   number of V paths
**          kvpts                   number of points in V path
**    RETURNS      :
**       0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
   gtsfdp (kupat,kupts,kvpat,kvpts)
   int *kupat,*kupts,*kvpat,*kvpts;

   {
   int status;
   uu_denter(UU_MTRC,(us,"gtsfdp ()"));
   status = UU_SUCCESS;
   *kupat = UM_srfattr.numupaths;
   *kvpat = UM_srfattr.numvpaths;
   *kupts = UM_srfattr.ptsperucrv;
   *kvpts = UM_srfattr.ptspervcrv;
   uu_dexit;
   return (status);
  }

/*********************************************************************
**    E_FUNCTION     : int ptsfdp (kupat,kupts,kvpat,kvpts)
**      Put the CAD surface display parameters.
**    PARAMETERS
**       INPUT :
**          kupat                   number of U paths
**          kupts                   number of points in U path
**          kvpat                   number of V paths
**          kvpts                   number of points in V path
**       OUTPUT  : none
**    RETURNS      :
**       0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
   ptsfdp (kupat,kupts,kvpat,kvpts)
   int *kupat,*kupts,*kvpat,*kvpts;

  {
   int status;
   uu_denter(UU_MTRC,(us,"ptsfdp (kupat=%x, kupts=%x, kvpat=%x, kvpts=%x)"
              *kupat, *kupts, *kvpat, *kvpts));
   status = UU_SUCCESS;
   UM_srfattr.numupaths = *kupat;
   UM_srfattr.numvpaths = *kvpat;
   UM_srfattr.ptsperucrv = *kupts;
   UM_srfattr.ptspervcrv = *kvpts;
   uu_dexit;
   return (status);
  }

/*********************************************************************
**    E_FUNCTION     : void pttess (kupts,kvpts)
**      Set GRID tessellation parameters.
**    PARAMETERS
**       INPUT :
**          kupts                   number of points in U path
**          kvpts                   number of points in V path
**       OUTPUT  : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void pttess (kupts,kvpts)
UM_int4 *kupts,*kvpts;
{
	int nupts = *kupts;
	int nvpts = *kvpts;
	ncl_set_tess_parms (1,0.,nupts,nvpts);
}

/*********************************************************************
**    E_FUNCTION     : void initss (disptol)
**      Set UM_TESS_TOLER tessellation parameters.
**    PARAMETERS
**       INPUT :
**          disptol                 tessellation tolerance
**       OUTPUT  : none
**    RETURNS      :  none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void initss (disptol)
UM_real8 *disptol;
{
	UU_REAL toler = *disptol;
	ncl_set_tess_parms (0,toler,0,0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_draw_netsf (srf, tfmat, attrptr)
**       Displays net surface consisting of: NCL surface, RBspline
**       surface, mesh or trim surfaces.
**    PARAMETERS
**       INPUT  :
**          srf               net surface record
**          tfmat             translation matrix
**          attrptr           net surface attributes record
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_draw_netsf (srf, tfmat, attrptr)
   struct NCL_netsf_rec *srf;
   UM_transf tfmat;
   struct UC_attributedatabag *attrptr;

   {
   int status;
   int i, nsuf,lucency;
   struct NCL_fixed_databag e;
   UM_transf tf1;
   struct NCL_surface_rec *ss;

   uu_denter(UU_MTRC,(us,"ncl_draw_netsf(key=%x)", srf->key));

    status = UU_SUCCESS;

	if (attrptr->rel_num == UM_SURFATTR_REL)
		S_netsf_attr = attrptr;

    nsuf = srf->no_netkey;
    for (i=0; i<nsuf; i++)
    {
       e.key = srf->netkey[i];
       if (ur_retrieve_data_relnum(e.key, &e.rel_num) != 0)
         status = UU_FAILURE;
       if (status == UU_SUCCESS && wf_geom_type (e.rel_num) == UU_SUCCESS)
       {
          status = ncl_retrieve_data(&e, sizeof(e));
          if (status == UU_SUCCESS)
             status = uc_retrieve_transf(e.key, tf1);
/*
.....There is a problem with pre-existing Unibases
.....having standalone corrupted surfaces with
.....the label @UN, so we no longer display these
.....surfaces, so ...
.....We need to locally give the sub-surfaces
.....different labels, so that they are displayed.
.....Bobby  -  12/8/99
*/
          if (status == UU_SUCCESS)
          {
					ncl_retrieve_lucency(srf,&lucency);
              ncl_setent_lucency (&e,lucency);
              ss = (struct NCL_surface_rec *)&e;
              strcpy(ss->label,"@NSF");
              status = uc_draw (&e,tf1,attrptr);
          }
       }
    }

	S_netsf_attr = UU_NULL;

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_disp_surf96 (eptr, tfmat, attptr)
**       Display an evaluated surface.
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
int ncl_disp_surf96 (eptr, tfmat, attrptr)
struct UM_rbsplsrf_rec *eptr;
UM_transf tfmat;
struct UM_surfattr_rec *attrptr;
{
	UU_LIST cvpts;
	UU_REAL u, v, du, dv, tol, *ptcv, ur[2], *lstp;
	int status,i,j,k,n,nu,nv,nup,nvp,ifl,lstix,ndisp;
	UM_int2 mdsys, mm, idx;
	struct NCL_meshsf_rec *mptr;
	struct NCL_surface_rec *nptr;
	struct NCL_revsurf_rec *rptr;
	UU_KEY_ID key;
	int shaded;

	uu_denter(UU_MTRC,(us,"ncl_disp_surf(key=%x, tfmat=%x, attrptr=%x)",
	   eptr->key, tfmat, attrptr));

	status = UU_SUCCESS;
	um_set_disp_attr(attrptr);
	ncl_get_shade_symbol (&shaded);

/*	shaded = NCL_shade_symbol; */
	idx = 175;
	getsc (&idx,&tol);

/*
..... If surface has a display list, use it & return, otherwise initialize it.
*/
	ifl = 1;
	if (eptr->rel_num == UM_RBSPLSRF_REL)
	{
		lstix = 6;
		ndisp = eptr->no_displst;
		lstp  = eptr->displst;
		if (shaded != 1) shaded = attrptr->shaded;
	}
	else if (eptr->rel_num == NCL_SURF_REL)
	{
		lstix = 3;
		nptr  = (struct NCL_surface_rec *)eptr;
		ndisp = nptr->no_displst;
		lstp  = nptr->displst;
		if (shaded != 1) shaded = attrptr->shaded;
	}
	else if (eptr->rel_num == NCL_REVSURF_REL)
	{
		lstix = 2;
		rptr  = (struct NCL_revsurf_rec *)eptr;
		ndisp = rptr->no_displst;
		lstp  = rptr->displst;
		if (shaded != 1) shaded = attrptr->shaded;
	}
	else if (eptr->rel_num == NCL_MESHSURF_REL)
	{
		lstix = 2;
		mptr  = (struct NCL_meshsf_rec *)eptr;
		ndisp = mptr->no_displst;
		lstp  = mptr->displst;
		if (shaded != 1) shaded = attrptr->shaded;
	}
	else
	{
		ifl = 0;
		lstix = 0;
		ndisp = 0;
		shaded = 0;
	}

	if (shaded == 1)
	{
/*
..... now we display boundary when shaded - we use "invisible" line style
..... and then restore the current one
*/
		int cur_line_style = attrptr->line_style;
		int bflg = 1;

		attrptr->line_style = 9; 
		um_set_disp_attr (attrptr);

		if (ndisp > 0 && ncl_displst_OK(eptr->key,lstp,tol,bflg))
		{
			ncl_bndr_display1(lstp,tfmat);
		}
		else if (lstix)
		{
			ncl_displst_init (lstix, eptr->key);

			gtmsmm (&mdsys, &mm);
			ur[0] = 0.;
			ur[1] = 1.;

			nup = attrptr->ptsperucrv;
			nvp = attrptr->ptspervcrv;  

			k = (nup > nvp)? nup: nvp; 
			k = (k < 50)? 50: k;
			uu_list_init (&cvpts, sizeof(UM_coord), k, k);

			for (i = 0, u = 0.; i < 2; i++, u = 1.)
			{
				if (i == 0)
					key = 3;
				else
					key = 4;

				gspickid(key);
				ncl_displst_setckey(key);
				cvpts.cur_cnt = 0;
				if (nup > 0)
					n = ncl_evolve1_crv_on_srf (eptr,tfmat,u,2,nup,&cvpts);
				else
					n = ncl_evolve_crv_on_srf (eptr,tfmat,u,ur,2,tol,&cvpts,
						UU_NULL,UU_NULL);
				ptcv = (UU_REAL *) UU_LIST_ARRAY (&cvpts);
				for (j = 0, k = 0; j < n; j++, k += 3)
					glina3 (&ptcv[k], &ptcv[k+1], &ptcv[k+2]); 

				gdraw();
			}

			for (i = 0, v = 0.; i < 2; i++, v = 1.)
			{
				if (i==0)
					key = 1;
				else
					key = 2;

				gspickid(key);
				ncl_displst_setckey(key);

				cvpts.cur_cnt = 0;
				if (nvp > 0 || ifl == 0)
					n = ncl_evolve1_crv_on_srf (eptr,tfmat,v,1,nvp,&cvpts);
				else 
					n = ncl_evolve_crv_on_srf (eptr,tfmat,v,ur,1,tol,&cvpts,
						UU_NULL,UU_NULL);

				ptcv = (UU_REAL *) UU_LIST_ARRAY (&cvpts);
				for (j = 0, k = 0; j < n; j++, k += 3)
				   glina3 (&ptcv[k], &ptcv[k+1], &ptcv[k+2]); 

				gdraw();
			}

			status = ncl_displst_finish(tol,1);
			stmsmm (&mdsys, &mm);
			ncl_conv_sfseg_reset();
			uu_list_free (&cvpts);
		}

		attrptr->line_style = cur_line_style;
		um_set_disp_attr (attrptr);
		status = ncl_display_shaded_surf(eptr->key);
		if (status == UU_SUCCESS)
			return (status);
	}	

	if (!ncl_get_wireframe())
		return (UU_SUCCESS);

	if (ndisp > 0 && ncl_displst_OK(eptr->key,lstp,tol,0))
	{
/*
..... aak 15-apr-1998: surface display may be called from umu_m46_place_view
..... when placing a drawing on the screen in the DRAG mode. In this case,
..... the display list should be transformed by tfmat.
*/
		status = ncl_displst_display (lstp,tfmat);
/*
.....added for hidden line remove
.....Yurong 2/11/99
*/
		if (ncl_is_hidline())
			ncl_display_hid_surf(eptr->key);
		return (status);
	}

	if (lstix) ncl_displst_init (lstix, eptr->key);
	gtmsmm (&mdsys, &mm);
	ur[0] = 0.;
	ur[1] = 1.;
	nup = attrptr->ptsperucrv;
	nvp = attrptr->ptspervcrv; 

	nu  = attrptr->numupaths;
	nv  = attrptr->numvpaths;

	k   = (nup > nvp)? nup: nvp; 
	k   = (k < 50)? 50: k;
	uu_list_init (&cvpts, sizeof(UM_coord), k, k);

	du = 1./(nu-1);
	dv = 1./(nv-1);
/*
...vp 14-jul-94 changed both loops for points so now u-line is created
...by u-point increment and v-line by v-point increment.
*/
	for (i=0, u=0.; i<nu; i++, u=i*du)
	{
		if (i==0)         key = 3;
		else if (i==nu-1) key = 4;
		else              key = 5;

		gspickid(key);
		ncl_displst_setckey(key);
		cvpts.cur_cnt = 0;
		if (nup > 0 || ifl == 0)
			n = ncl_evolve1_crv_on_srf (eptr,tfmat,u,2,nup,&cvpts);
		else
			n = ncl_evolve_crv_on_srf (eptr,tfmat,u,ur,2,tol,&cvpts,
				UU_NULL,UU_NULL);
		ptcv = (UU_REAL *) UU_LIST_ARRAY (&cvpts);
		for (j=0, k=0; j<n; j++, k+=3)
			glina3 (&ptcv[k], &ptcv[k+1], &ptcv[k+2]); 

		gdraw();
	}

	for (i=0, v=0.; i<nv; i++, v=i*dv)
	{
		if (i==0)         key = 1;
		else if (i==nv-1) key = 2;
		else              key = 5;

		gspickid(key);
		ncl_displst_setckey(key);

		cvpts.cur_cnt = 0;
		if (nvp > 0 || ifl == 0)
			n = ncl_evolve1_crv_on_srf (eptr,tfmat,v,1,nvp,&cvpts);
		else 
			n = ncl_evolve_crv_on_srf (eptr,tfmat,v,ur,1,tol,&cvpts,
				UU_NULL,UU_NULL);

		ptcv = (UU_REAL *) UU_LIST_ARRAY (&cvpts);
		for (j=0, k=0; j<n; j++, k+=3)
			glina3 (&ptcv[k], &ptcv[k+1], &ptcv[k+2]); 

		gdraw();
	}
/*
..... Complete display list.
*/
	status = ncl_displst_finish(tol,0);
/*
.....added for hidden line remove
.....Yurong 2/11/99
*/
	if (ncl_is_hidline())
		ncl_display_hid_surf(eptr->key);

	stmsmm (&mdsys, &mm);
	ncl_conv_sfseg_reset();
	uu_list_free (&cvpts);

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_displst_params (eptr,lstix,ndisp,lstp)
**       Get various display parameters for a surface.
**    PARAMETERS
**       INPUT  :
**          eptr       - ptr to surface
**       OUTPUT :
**          lstix      - display list number
**          ndisp      - number of points in display list
**          lstp       - display list pointer
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_displst_params (eptr,lstix,ndisp,lstp)
struct NCL_fixed_databag *eptr;
UU_REAL **lstp;
int *lstix,*ndisp;
{
	if (eptr->rel_num == UM_RBSPLSRF_REL)
	{
		struct UM_rbsplsrf_rec *sf1;
		sf1  = (struct UM_rbsplsrf_rec *)eptr;
		*lstix = 6;
		*ndisp = sf1->no_displst;
		*lstp  = sf1->displst;
	}
	else if (eptr->rel_num == NCL_SURF_REL)
	{
		struct NCL_surface_rec *sf1;
		sf1  = (struct NCL_surface_rec *)eptr;
		*lstix = 3;
		*ndisp = sf1->no_displst;
		*lstp  = sf1->displst;
	}
	else if (eptr->rel_num == NCL_TRIMSF_REL)
	{
		struct NCL_trimsf_rec *sf1;
		sf1  = (struct NCL_trimsf_rec *)eptr;
		*lstix = 2;
		*ndisp = sf1->no_displst;
		*lstp  = sf1->displst;
	}
	else if (eptr->rel_num == NCL_REVSURF_REL)
	{
		struct NCL_revsurf_rec *sf1;
		sf1  = (struct NCL_revsurf_rec *)eptr;
		*lstix = 2;
		*ndisp = sf1->no_displst;
		*lstp  = sf1->displst;
	}
	else if (eptr->rel_num == NCL_MESHSURF_REL)
	{
		struct NCL_meshsf_rec *sf1;
		sf1  = (struct NCL_meshsf_rec *)eptr;
		*lstix = 2;
		*ndisp = sf1->no_displst;
		*lstp  = sf1->displst;
	}
	else if (eptr->rel_num == NCL_NETSF_REL)
	{
		struct NCL_netsf_rec *sf1;
		sf1  = (struct NCL_netsf_rec *)eptr;
		*lstix = 3;
		*ndisp = sf1->no_displst;
		*lstp  = sf1->displst;
	}
	else
	{
		*lstix = 0;
		*ndisp = 0;
		*lstp = UU_NULL;
	}
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_disp_params (eptr,attrptr,lstix,ndisp,lstp,
**                                            shaded,ecolor,nup,nvp,nu,nv)
**       Get various display parameters for a surface.
**    PARAMETERS
**       INPUT  :
**          eptr       - ptr to surface
**          attrptr    - ptr to surface attributes
**       OUTPUT :
**          lstix      - display list number
**          ndisp      - number of points in display list
**          lstp       - display list pointer
**          shaded     - flag
**          ecolor     - color for edge display
**          nu         - number of U lines
**          nup        - number of points in a U line
**          nv         - number of V lines
**          nvp        - number of points in a V line
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_disp_params (eptr,attrptr,lstix,ndisp,lstp,shaded,ecolor,nup,nvp,nu,nv)
struct NCL_fixed_databag *eptr;
struct UM_surfattr_rec *attrptr;
UU_REAL **lstp;
int *nu,*nv,*nup,*nvp,*lstix,*ndisp,*shaded,*ecolor;
{
	struct UM_surfattr_rec attr,*sfattr;

	switch (eptr->rel_num)
	{
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_NETSF_REL:
		case NCL_MESHSURF_REL:
		case NCL_TRIMSF_REL:
		case NCL_REVSURF_REL:
		{
			if (S_netsf_attr == UU_NULL)
			{
				if (attrptr != UU_NULL)
				{
					sfattr = attrptr;
				}
				else
				{
					uc_retrieve_attr(eptr->key,&attr);
					sfattr = &attr;
				}
			}
			else
				sfattr = (struct UM_surfattr_rec *) S_netsf_attr;
/*
.....It is possible for older Unibases not
.....to have a surface attribute bundle associated
.....with the surface
.....In this case, use the default surface attributes
.....Bobby  - QAR 100079
*/
			if (sfattr->rel_num != UM_SURFATTR_REL)
			{
				*nu = UM_srfattr.numupaths;
				*nv = UM_srfattr.numvpaths;
				*nup = UM_srfattr.ptsperucrv;
				*nvp = UM_srfattr.ptspervcrv;
				*shaded = UM_srfattr.shaded;
				*ecolor = UM_srfattr.edge_color;
			}
			else
			{
				*nup = sfattr->ptsperucrv;
				*nvp = sfattr->ptspervcrv;
				*nu  = sfattr->numupaths;
				*nv  = sfattr->numvpaths;
				*shaded = sfattr->shaded;
				*ecolor = sfattr->ecolor;
			}
			ncl_displst_params (eptr,lstix,ndisp,lstp);
			break;
		}
		default:
		{
			*lstix = 0;
			*ndisp = 0;
			*lstp = UU_NULL;
			*nup = *nvp = 0;
			*nu = *nv = 0;
			*shaded = 0;
			*ecolor = -1;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_disp_surf (eptr, tfmat, attptr)
**       Display an evaluated surface.
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
int ncl_disp_surf (eptr, tfmat, attrptr)
struct UM_srfdatabag *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;
{
	int status;
/*
.....Don't display @UN surfaces
*/
	if (ncl_tst_un_label(eptr) == 1) return (UU_FAILURE);

	if (ncl_setver (96))
	{
		status = ncl_disp_surf96 (eptr, tfmat, attrptr);
	}
	else
	{
		status = ncl_surf_disp (UU_FALSE,eptr,tfmat,attrptr);
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_get_dispmodes (sfkey,sfnum,disp)
**       Get the 'blanked' and 'shaded' modes for a list of surfaces,
**       then erase all surfaces.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_dispmodes (sfkey,sfnum,disp)
UU_KEY_ID *sfkey;
int sfnum;
NCL_sfs_dispmode *disp;
{
	int i;
	struct NCL_fixed_databag e;

	for (i = 0; i < sfnum; i++)
	{
		e.key = sfkey[i];
		disp[i].blanked = UU_FALSE;
		disp[i].shaded = 0;

		if (ncl_retrieve_data_fixed(&e) == UU_SUCCESS)
		{
			ur_retrieve_blanked (sfkey[i],&disp[i].blanked);
			ncl_retrieve_shaded (&e,&disp[i].shaded);
		}
		ncl_sea_ent_blank (0,sfkey[i]);
	}
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_redisp_sfs (sfkey,sfnum,disp)
**       Redisplay surfaces in the original (stored in disp) way.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_redisp_sfs (sfkey,sfnum,disp)
UU_KEY_ID *sfkey;
int sfnum;
NCL_sfs_dispmode *disp;
{
	struct NCL_fixed_databag e;
	int i;

	for (i = 0; i < sfnum; i++)
	{
		if (disp[i].blanked)
			ncl_sea_ent_blank (0,sfkey[i]);
		else
		{
			ncl_sea_ent_blank (2,sfkey[i]);
			e.key = sfkey[i];
			if (ncl_retrieve_data_fixed(&e) == UU_SUCCESS)
			{
				ncl_setent_shaded (&e,disp[i].shaded);
				uc_display (&e);
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_disp_sfs (sfkey,sfnum,disp)
**       Display surfaces in the way selected by user.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_disp_sfs (sfkey,sfnum,disp)
UU_KEY_ID *sfkey;
int sfnum,disp;
{
	int i;

	if (disp == 0) /* erase surfaces */
	{
		for (i = 0; i < sfnum; i++)
			ncl_sea_ent_blank (0,sfkey[i]);
	}
	else if (disp == 1) /* display wireframe */
	{
		for (i = 0; i < sfnum; i++)
			ncl_sea_ent_blank (2,sfkey[i]);
		ncl_unshade_surf (sfkey,sfnum);
	}
	else if (disp == 2) /* display shaded */
	{
		for (i = 0; i < sfnum; i++)
			ncl_sea_ent_blank (2,sfkey[i]);
		ncl_shade_surf (sfkey,sfnum);
	}
}

/*********************************************************************
**    E_FUNCTION     : int ptrld (nclkey, iclosd)
**       Update the ruled flag for a surface.
**    PARAMETERS
**       INPUT  :
**          nclkey          - UNIBASE key of entity
**          irld            - value for ruled in u flag.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptrld (nclkey, irld)
   UM_int4  *nclkey;
   UM_int2  *irld;

   {
   int status;
   UU_LOGICAL update = UU_TRUE;
   UM_int4 key;
   struct NCL_fixed_databag e1;
   struct NCL_surface_rec *nsf;
   struct NCL_revsurf_rec *vsf;
   struct NCL_meshsf_rec *msf;
   struct UM_rbsplsrf_rec *rsf;
   struct UM_agsrf_rec *asf;

   e1.key = *nclkey;
   status = ncl_retrieve_data_fixed(&e1);

   if (status == UU_SUCCESS)
     {
     switch (e1.rel_num)
        {
        case NCL_SURF_REL:
           nsf = (struct NCL_surface_rec *)&e1;
           nsf->rldnu = *irld;
           break;
        case NCL_REVSURF_REL:
           vsf = (struct NCL_revsurf_rec *)&e1;
           vsf->rldnu = *irld;
           break;
        case NCL_MESHSURF_REL:
           msf = (struct NCL_meshsf_rec *)&e1;
           msf->rldnu = *irld;
           break;
        case UM_AGSRF_REL:
           asf = (struct UM_agsrf_rec *)&e1;
           asf->rldnu = *irld;
           break;
        case UM_RBSPLSRF_REL:
           rsf = (struct UM_rbsplsrf_rec *)&e1;
           rsf->rldnu = *irld;
           break;
        case NCL_TRIMSF_REL:
           key = ((struct NCL_trimsf_rec *)&e1)->bs_key;
           ptrld (&key, irld);
           update = UU_FALSE;
           break;
        default:
           break;
        }
     if (update) status = ur_update_data_fixed(&e1);
     }

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtrld1 (nclkey, irld)
**       Retrieve the ruled flag for a surface.
**    PARAMETERS
**       INPUT  :
**          nclkey          - UNIBASE key of entity
**       OUTPUT :
**          irld            - value for ruled in u flag.
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtrld1 (nclkey, irld)
   UM_int4  *nclkey;
   UM_int2  *irld;

   {
   int status;
   UM_int4 key;
   struct NCL_fixed_databag e1;
   struct NCL_surface_rec *nsf;
   struct NCL_revsurf_rec *vsf;
   struct NCL_meshsf_rec *msf;
   struct UM_rbsplsrf_rec *rsf;

   *irld = 0;
   e1.key = *nclkey;
   status = ncl_retrieve_data_fixed (&e1);
   if (status == UU_SUCCESS)
     {
     switch (e1.rel_num)
        {
        case NCL_SURF_REL:
           nsf = (struct NCL_surface_rec *)&e1;
           *irld = nsf->rldnu;
           break;
        case NCL_REVSURF_REL:
           vsf = (struct NCL_revsurf_rec *)&e1;
           *irld = vsf->rldnu;
           break;
        case NCL_MESHSURF_REL:
           msf = (struct NCL_meshsf_rec *)&e1;
           *irld = msf->rldnu;
           break;
        case UM_AGSRF_REL:
           break;
        case UM_RBSPLSRF_REL:
           rsf = (struct UM_rbsplsrf_rec *)&e1;
           *irld = rsf->rldnu;
           break;
        case NCL_TRIMSF_REL:
           key = ((struct NCL_trimsf_rec *)&e1)->bs_key;
           gtrld (&key, irld);
           break;
        default:
           break;
        }
     }

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_reset_assoc (eptr)
**       Reset associate curves on surface if any.
**    PARAMETERS
**       INPUT  :
**          eptr          - pointer to the general surface data
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_reset_assoc (eptr)
struct NCL_fixed_databag *eptr;
{

	switch (eptr->rel_num)
	{
		case UM_RBSPLSRF_REL:
		{
			struct UM_rbsplsrf_rec *sptr;
			sptr = (struct UM_rbsplsrf_rec *) eptr;
			sptr->no_sskey = 0;
		}
		break;
		case NCL_SURF_REL:
		{
			struct NCL_surface_rec *sptr;
			sptr = (struct NCL_surface_rec *) eptr;
			sptr->no_sskey = 0;
		}
		break;
		case NCL_REVSURF_REL:
		{
			struct NCL_revsurf_rec *sptr;
			sptr = (struct NCL_revsurf_rec *) eptr;
			sptr->no_sskey = 0;
		}
		break;
		case NCL_NETSF_REL:
		{
			struct NCL_netsf_rec *sptr;
			sptr = (struct NCL_netsf_rec *) eptr;
			sptr->no_sskey = 0;
		}
		default:
		break;
	}

	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_gtssnum (eptr,num)
**       Get number of associate curves on qualified surface.
**    PARAMETERS
**       INPUT  :
**          eptr          - pointer to the general surface data
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_gtssnum (nclkey,num)
UU_KEY_ID *nclkey;
UM_int2 *num;
{
	struct NCL_fixed_databag eptr;
	int status;

	eptr.key = *nclkey;
	status = ncl_retrieve_data_fixed (&eptr);
	*num = 0;
	if (status == UU_SUCCESS)
		switch (eptr.rel_num)
		{
			case UM_RBSPLSRF_REL:
			{
				struct UM_rbsplsrf_rec *sptr;
				sptr = (struct UM_rbsplsrf_rec *) &eptr;
				*num = sptr->no_sskey;
			}
			break;
			case NCL_SURF_REL:
			{
				struct NCL_surface_rec *sptr;
				sptr = (struct NCL_surface_rec *) &eptr;
				*num = sptr->no_sskey;
			}
			break;
			case NCL_REVSURF_REL:
			{
				struct NCL_revsurf_rec *sptr;
				sptr = (struct NCL_revsurf_rec *) &eptr;
				*num = sptr->no_sskey;
			}
			break;
			case NCL_NETSF_REL:
			{
				struct NCL_netsf_rec *sptr;
				sptr = (struct NCL_netsf_rec *) &eptr;
				*num = sptr->no_sskey;
			}
			default:
			break;
		}

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_redisp_ss (eptr)
**       Redisply associate curves on qualified surface.
**    PARAMETERS
**       INPUT  :
**          eptr          - pointer to the general surface data
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_redisp_ss (eptr)
struct NCL_fixed_databag *eptr;
{
	int status, i,lst,num;
	UM_int2 dummy;
	UU_KEY_ID *keys;

	status = ncl_retrieve_sskeys (eptr,&lst,&num,&keys);

	for (i=0; i<num && status==UU_SUCCESS; i++)
		status = dspent (&keys[i],&dummy);

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : pttrim (nclkey, iflg)
**       Sets trim surface drive type mode.
**    PARAMETERS
**       INPUT  :
**          nclkey     - Key of trimmed surface.
**          iflg       - Value to set in drive as trimmed flag.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pttrim (nclkey, iflg)
UM_int4 *nclkey;
UM_int2 *iflg;
{
   int status;
   struct NCL_fixed_databag e1;
   struct NCL_trimsf_rec *tsf;

   e1.key = *nclkey;
   status = ncl_retrieve_data_fixed (&e1);
   if (status == UU_SUCCESS)
   {
      if (e1.rel_num != NCL_TRIMSF_REL) status = UU_FAILURE;
   }

   if (status == UU_SUCCESS)
   {
      tsf = (struct NCL_trimsf_rec *)&e1;
      tsf->drive_type = *iflg;
      status = ur_update_data_fixed (tsf);
   }

   return(status);
}
