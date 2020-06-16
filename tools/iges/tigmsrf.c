/*********************************************************************
**    NAME         :  tigmsrf.c
**       CONTAINS:
**                  uig_map_rbsplsrf
**                  uig_map_ruledsrf
**                  uig_map_revsrf
**                  uig_map_tabcyl
**                  double uig_len_bs 
**                  uig_map_offsetsrf
**                  uig_map_trimsrf 
**                  uig_map_bdsrf
**                  uig_check_offset_dist
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigmsrf.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:50
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "nccs.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdattr.h"
#include "tigsupp.h"
#include "mdrel.h"
#include "udebug.h"
#include "usysdef.h"
#include "mxxx.h"
#include "mdeval.h"
#include "rbase.h"
#include "modef.h"
#include "nclfc.h"

#include "ag_incl.h"
#include "ag_l_crv.h"

#define CRVL_FUZZ (UU_REAL) 1.0e-6
UU_REAL AG_tol = 1.0e-10;

extern UU_LOGICAL drw_flag; 
extern UU_REAL drw_t[4][3], drw_v[4][3], drw_s[4][3];
extern int drw_ptr; 
extern int MAX_PARA_REC;
extern int UIG_inner_error;
/*********************************************************************
**    I_FUNCTION     :  uig_map_rbsplsrf(dblk,igesin,t,key)
**          Map an IGES rational bspline surface to a unibase rational
**          bspline surface.
**    PARAMETERS   
**       INPUT  : 
**          dblk                    IGES directory block
**          igesin                  IGES arc structure
**          t                       associated matrix
**       OUTPUT :  
**          key                     UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_map_rbsplsrf(dblk,igesin,t,key)
struct dir_rec *dblk;            /* IGES rational bspline surf directory */
struct IG_igesrssf_rec *igesin;  /* IGES rational bspline surf parameter */
UU_REAL   t[12];
UU_KEY_ID *key;
{
   int i, j, npts, status = UU_SUCCESS;
   double pt2[3];
   double *p;
   struct UM_rbsplsrf_rec  uniout;
   struct IG_rspara_rec *uvpara;
   UU_REAL  du, dv, *upar, *vpar;
   char *uu_toolmalloc();
	UM_int2 primtyp;
	UM_real8 primdata[16];
	int uig_match_rbsplsrf();

   ur_setup_app_data(UM_RBSPLSRF_REL,&uniout,sizeof(struct UM_rbsplsrf_rec));

   uig_update_attr(dblk);
   uvpara = igesin->rspara;

   uniout.ku = igesin->degree1 + 1;
   uniout.kv = igesin->degree2 + 1;
   uniout.nu = igesin->indx1-igesin->degree1+1;
   uniout.nv = igesin->indx2-igesin->degree2+1;
   npts = (igesin->indx1+1)*(igesin->indx2+1);

   uniout.rldnu = -1;
   uniout.swapuv = 0;
   uniout.rev_normal = UU_FALSE;
   uniout.closdinu = 0;
   uniout.closdinv = 0;
   uniout.offdist = 0.0;
   uniout.no_tesslst = 0;
   uniout.tesslst = NULL;
	uniout.primitive = 0;
	for (i=0;i<16;i++) uniout.prim_param[i] = 0;
/*
.....vp 25-feb-97  adjust parametrization for range specified in the
.....surface spec, prevent zero width of parameter range.
*/
   du     = uvpara->u1 - uvpara->u0;
   du = (fabs(du) > UM_DFUZZ)? du: 1.0;
   dv     = uvpara->v1 - uvpara->v0;
   dv = (fabs(dv) > UM_DFUZZ)? dv: 1.0;
   upar   = (UU_REAL *) uu_toolmalloc(igesin->no_t1*sizeof(UU_REAL));
   vpar   = (UU_REAL *) uu_toolmalloc(igesin->no_t2*sizeof(UU_REAL));
   j = igesin->no_t1;
   for (i=0; i<j; i++)
      upar[i] = igesin->t1[i]/du;
   status=ur_update_app_data_varlist(&uniout, 1, upar, 1, j);
/*
.....Add u min/max values to end of tu values. These are currently not used
.....NCL but are used by iges out to store u max/min values in iges file.
*/
   status=ur_update_app_data_varlist(&uniout, 1, &uvpara->u0, j+1, 1);
   status=ur_update_app_data_varlist(&uniout, 1, &uvpara->u1, j+2, 1);
   j = igesin->no_t2;
   for (i=0; i<j; i++)
      vpar[i] = igesin->t2[i]/dv;
   status=ur_update_app_data_varlist(&uniout, 2, vpar, 1, j);
/*
.....Add v min/max values to end of tv values.
*/
   status=ur_update_app_data_varlist(&uniout, 2, &uvpara->v0, j+1, 1);
   status=ur_update_app_data_varlist(&uniout, 2, &uvpara->v1, j+2, 1);

	if (igesin->type == 0)
	{
		igesin->type = 1;
		for (i = 0; i < npts; i++)
			if (igesin->w[i] != 1.)
			{
				igesin->type = 0; break;
			}
	}
	if (igesin->type == 0)
   	status=ur_update_app_data_varlist(&uniout, 4, igesin->w, 1, npts);
	else
	{
		uniout.no_wt  = 0; uniout.wt = UU_NULL;
	}

   p = igesin->pt3;
   status = ur_update_app_data_varlist(&uniout, 3, pt2, npts, 1);
   for (i=1, j=0; i<=npts; i++, j=j+3)
   {
      uig_tran_coor(&p[j],t,pt2);        /* transform to model space */
      um_vctmsc(pt2,unit_scale,pt2);      /* scale */
      if (drw_flag)
      {
			uig_transform_array(pt2,drw_v);
			uig_transform_array(pt2,drw_s);
			uig_transform_array(pt2,drw_t);
      }
      status = ur_update_app_data_varlist(&uniout, 3, pt2, i, 1);
   }

   /* check if creating a drawing entity */
   if(drw_flag) dblk->view_ptr = drw_ptr;
	um_save_active_label(uniout.rel_num);
  	create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
   status = uig_create_geom(&uniout, 0, 0,dblk->view_ptr);

	uig_srfcls (&uniout,2);

	ncl_sf_prim_analyz(&uniout.key,&primtyp,primdata);

	if (UIG_nodups && !label_comp_element &&
		ncl_retrieve_data_fixed(&uniout) == UU_SUCCESS &&
		uig_match_rbsplsrf(&uniout,0) == UU_SUCCESS)
	{
		uig_remove_dup(&uniout,key);
	}
	else
	{
		if (label_type == 8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&uniout,uig_match_rbsplsrf);
		}

   	UIG_unibase_entities++;
   	*key = uniout.key;
	}
   uu_toolfree(upar);
   uu_toolfree(vpar);

	return (status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_map_ruledsrf(crv_ptr, reverse, dblk, igesin, key)
**                Map an IGES ruled surface to a NURBS surface
**    PARAMETERS   
**       INPUT  : 
**              crv_pt              pointers to boundary curves
**              reversed            LOGICAL to indicate if curves are to 
**                                  be reversed
**              dblk                ATTRIBUTE data
**       OUTPUT :  
**              key                 UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_map_ruledsrf(crv_ptr, reverse, tfmat,  dblk, igesin, key)
UU_KEY_ID crv_ptr[2];
int reverse;
UU_REAL tfmat[4][3];
struct dir_rec *dblk;
struct IG_igesrlsf_rec *igesin;
UU_KEY_ID *key;
{

	struct UM_rbsplsrf_rec srf;
	int status, ierr;
	char p_buff[80];
	double crv_len;
	int uig_match_rbsplsrf();

	struct NCL_fixed_databag c1, c2;

	*key = 0;
	if (crv_ptr[0] != 0 && crv_ptr[1] != 0)
	{
		c1.key = crv_ptr[0];
		status = ncl_retrieve_data_fixed (&c1);

		c2.key = crv_ptr[1];
		status = ncl_retrieve_data_fixed (&c2);

		if (c1.rel_num == UM_POINT_REL || c2.rel_num == UM_POINT_REL)
		{
			sprintf (p_buff, "(DREC = %d) Point not allowed in boundary of a ruled surf.\n",
					dblk->drec_num);
			uig_error (p_buff);
			goto done;
		}

		if (reverse == 1) uc_reverse_crv (&c2);

/*   ------   FIX   ------   */
        /* get length of bsplines */
		ierr = 0;
		crv_len = 1.0; /* uig_len_bs (bs1, u_eps, &ierr); */
		if (ierr == 0 && crv_len > CRVL_FUZZ)
			crv_len = 1.0; /* uig_len_bs (bs2, u_eps, &ierr); */

		if (ierr != 0 || crv_len < CRVL_FUZZ)
		{
			sprintf (p_buff, "(DREC = %d) Zero length curve in ruled surf.\n",
						dblk->drec_num);
			uig_error (p_buff);
			goto done;
		}
/*   ------ END FIX ------   */

		uig_update_attr(dblk);
   	srf.rel_num = UM_RBSPLSRF_REL;
		um_save_active_label(srf.rel_num);
		create_label(dblk, igesin->no_prop, igesin->prop, srf.label, &srf.subscr);
		srf.key = 0;

        /* construct ruled surface */ 
/*
.....ncl_rldgen had been changed to need to pass in 0 to 
.....accomodate the new changes. JLS 5/13/99
*/
		status = ncl_rldgen (&c1, &c2, &srf,0);

		if (status == UU_SUCCESS)
		{
			uig_srfcls (&srf,0);
			uig_transform_rbsrf(&srf, tfmat);
/* check if creating a drawing entity */
			if (drw_flag)
			{
				uig_transform_rbsrf(&srf, drw_v);
				uig_transform_rbsrf(&srf, drw_s);
				uig_transform_rbsrf(&srf, drw_t);
				uig_update_view (srf.key, drw_ptr);
			}
			if (UIG_nodups && !label_comp_element &&
				ncl_retrieve_data_fixed(&srf) == UU_SUCCESS &&
				uig_match_rbsplsrf(&srf,0) == UU_SUCCESS)
			{
				uig_remove_dup(&srf,key);
			}
			else
			{
				UIG_unibase_entities++;
/*
.....Determine if this surface is one of the special surfaces
			ncl_sf_prim_analyz(&srf.key,&primtyp,&primdata);
Eduard 07/06/00. commented out since the primitive type is already done in ncl_rldgen
*/
				if (label_type == 8)
				{
/*
.....Label matching. Determine if exact match.
*/
					uig_exact_match(&srf, uig_match_rbsplsrf);
				}
         	*key = srf.key;
			}
		}
	}

done:;

	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  uig_map_revsrf(crv_ptr,sa,sb,t,dblk,igesin,key)
**          Map an IGES rev surface to a NCL surface of revolution
**    PARAMETERS   
**       INPUT  : 
**          crv_ptr              axis curve and generator curve
**          sa, ea               start and ending angles
**          dblk                 ATTRIBUTE data
**       OUTPUT :
**          key                  UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_map_revsrf(cvkey,pta,vca, sa, ea, tfmat, dblk, igesin, key)
UU_KEY_ID cvkey;
UU_REAL *pta, *vca;
UU_REAL sa,ea;
UU_REAL tfmat[4][3];
struct dir_rec *dblk;
struct IG_igesrvsf_rec *igesin;
UU_KEY_ID *key;
{
	char p_buff[80];
	struct NCL_revsurf_rec srf;
	int  status, i;
	int uig_match_revsf();

	*key = 0;

	if ((int)cvkey <= 0)
	{
		status = UU_FAILURE;
		goto done;
	}
	
	ea = ea * UM_RADIAN; sa = sa * UM_RADIAN;
	if (fabs (ea - sa) < 0.033)
	{
		sprintf (p_buff, 
			"(DREC = %d) Surf-of-rev angle of rotation too small.\n",
			 dblk->drec_num); 
		uig_error (p_buff);
		status = UU_FAILURE;
		goto done;
	}
	while (sa < 0.) 
	{
		if (sa > -0.001) sa = 0.;
		else sa += 360.;
	}
	while (sa >= 360.) sa -= 360.;
	while (ea <= sa) ea += 360.;
	while ((ea - sa) > 360.) 
	{
		if (ea - sa - 360. < 0.001)
			ea = 360.;
		else
			ea -= 360.;
	}

	ur_setup_app_data (NCL_REVSURF_REL, &srf, sizeof(srf));
	uig_update_attr(dblk);

	srf.key = 0;
	um_nullvc (srf.labloc);
	create_label(dblk, igesin->no_prop, igesin->prop, srf.label, &srf.subscr);

	srf.sa = sa;
	srf.ta = ea;
	srf.rldnu = -1;
	srf.swapuv = 0;
	srf.rev_normal = UU_FALSE;
	srf.closdinu = 0;
	if ( fabs (ea - sa - 360.) < .001) 
		srf.closdinv = 1;
	else 
		srf.closdinv = 0;
	srf.offdist = 0.0;
	srf.no_sskey = 0;
	srf.sskey = NULL;
	srf.no_displst = 0;
	srf.displst = NULL;
	srf.no_tesslst = 0;
	srf.tesslst = NULL;
	srf.no_boxlst = 0;
	srf.boxlst = NULL;

	srf.cvkey = cvkey;

	um_cctmtf(pta,tfmat,srf.pta);
	um_vctmtf(vca,tfmat,vca);
	um_unitvc(vca, srf.vca);

	srf.primitive = 0;
	for (i=0;i<16;i++) srf.prim_param[i] = 0;

	if (drw_flag) dblk->view_ptr = drw_ptr;
	status = uig_create_geom(&srf, 0, 0, dblk->view_ptr);
	
	if (status != UU_SUCCESS)
	{
		sprintf(p_buff, 
			"(DREC = %d) Failed to create surface of revolution.\n",
			dblk->drec_num);
		uig_error(p_buff);
	}

	uig_srfcls (&srf,0);
/*
.....Determine if this is one of the special types of surfaces
*/
	if (status == UU_SUCCESS)
	{
		UM_int2 typ;
		UM_real8 param[16];
		status = ncl_sf_prim_analyz(&srf.key,&typ,param);

		if (status != UU_SUCCESS)
		{
			sprintf (p_buff,
				"(DREC = %d) Couldn't analyze surface of revolution.\n",
				dblk->drec_num);
			uig_error(p_buff);
		}
	}

	if (status == UU_SUCCESS)
	{
		if(drw_flag)
		{
			uig_transform_revsf(&srf, drw_v);
			uig_transform_revsf(&srf, drw_s);
			uig_transform_revsf(&srf, drw_t);
			uig_update_view (srf.key, drw_ptr);
		}
		UIG_unibase_entities++;
		if (label_type == 8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&srf, uig_match_revsf);
		}
		*key = srf.key;
	}

done:;

	return (status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_map_tabcyl(crv_ptr, lpoint dblk, igesin, key)
**            Map an IGES tabulated cylinder surface to a NURBS surface
**    PARAMETERS   
**       INPUT  : 
**            crv_ptr            generator curve
**            lpoint             point on line
**            dblk               ATTRIBUTE data
**       OUTPUT :
**            key                UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_map_tabcyl(crv_ptr, lpoint, tfmat, dblk, igesin, key)
UU_KEY_ID crv_ptr;
UU_REAL lpoint[3];
UU_REAL tfmat[4][3];
struct dir_rec *dblk;
struct IG_igestbcy_rec *igesin;
UU_KEY_ID *key;
{
	struct UM_rbsplsrf_rec srf;
	struct UM_surfattr_rec sfattr;
	struct NCL_fixed_databag c1;
	int status;
	UU_REAL *p = NULL;
	UM_coord spt, ept;
	UM_vector vecd;
	UM_int2 primtyp;
	UM_real8 primdata[16];
	int uig_match_rbsplsrf();

	status = UU_FAILURE;
	*key = 0;
	if(crv_ptr != 0)
	{
		c1.key = crv_ptr;
		status = ncl_retrieve_data_fixed (&c1);

		if (status == UU_SUCCESS)
 			status = um_get_endpts (&c1, UM_idmat, spt, ept);

		if (status == UU_SUCCESS)
		{
			um_vcmnvc (lpoint, spt, vecd);
			uig_update_attr(dblk);
  		 	srf.rel_num = UM_RBSPLSRF_REL;
			um_save_active_label(srf.rel_num);
			create_label(dblk, igesin->no_prop, igesin->prop, srf.label, &srf.subscr);
			srf.key = 0;

        /* construct tabcyl */ 
			status = ncl_tabcyl (&c1, vecd, &srf);
		}

		if (status == UU_SUCCESS)
		{
			uig_srfcls (&srf,0);
			uig_transform_rbsrf(&srf, tfmat);
            /* check if creating a drawing entity */
			if(drw_flag)
			{
				uig_transform_rbsrf(&srf, drw_v);
				uig_transform_rbsrf(&srf, drw_s);
				uig_transform_rbsrf(&srf, drw_t);
				uig_update_view (srf.key, drw_ptr);
			}
			ncl_sf_prim_analyz(&srf.key,&primtyp,&primdata);

			if (UIG_nodups && !label_comp_element &&
				ncl_retrieve_data_fixed(&srf) == UU_SUCCESS &&
				uig_match_rbsplsrf(&srf,0) == UU_SUCCESS)
			{
				uig_remove_dup(&srf,key);
			}
			else
			{
				UIG_unibase_entities++;
				if (label_type ==8)
				{
/*
.....Label matching. Determine if exact match.
*/
					uig_exact_match(&srf, uig_match_rbsplsrf);
				}
				*key = srf.key;
			}
		}
	}
done:;

	if (p != NULL) uu_toolfree(p);

	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  double uig_len_bs (bs, eps, err)
**          Determine length of AG B-Spline.
**    PARAMETERS   
**       INPUT  : 
**              bs                      pointer to AG B-Spline.
**              eps                     tolerance.
**       OUTPUT :  
**              ierr                    0 if no error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
double
uig_len_bs (bs, eps, ierr)
    AG_SPLINEP bs;
    double eps;
    int *ierr;
    {
    double crv_len;
    AG_CURVEP crv, ag_crv_bs();
    AG_LCRVP lcrvh, ag_bld_lcrv();

    uu_denter(UU_MTRC,(us,"uig_len_bs()"));

    crv_len = 0.0;
    crv = ag_crv_bs (bs);
    lcrvh = ag_bld_lcrv (crv, eps, crv_len, NULL);
    *ierr = ag_len_crv (lcrvh);
    if (*ierr == 0) crv_len = lcrvh->length;
    crv->nbs = 0;
    crv->bs = NULL;
    crv->bs0 = NULL;
    ag_db_crv(&crv);
    ag_db_lcrv(&lcrvh);

    uu_dexit;
    return (crv_len);
    }
/*********************************************************************
**    I_FUNCTION     :  uig_map_offsetsrf(dblk, pblk, t, key)
**      Map an IGES offset surface to a NCL offset surface. (Mesh or RBS)
**    PARAMETERS   
**       INPUT  : 
**            dblk               ATTRIBUTE data
**            pblk               Ofset surface param rec.
**            t                  Transformation matrix.
**       OUTPUT :  
**            key                UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_map_offsetsrf(gblk, dblk, pblk, t, key)
   struct global_rec *gblk;
   struct dir_rec *dblk;   
   struct IG_igesofsf_rec *pblk;
   UU_REAL t[12];
   UU_KEY_ID *key;
   {
   int irec, status;
   struct dir_rec ldir;
   UU_KEY_ID lkey;
   struct NCL_fixed_databag srf;
   UM_param u, v;
   struct UM_evsrfout evsrf;
   UM_transf tfmat;
   UU_REAL dis;
   char *c;
   char *uu_toolmalloc();
	UM_int2 primtyp;
	UM_real8 primdata[16];
	UU_LOGICAL trans;

   c = uu_toolmalloc(MAX_PARA_REC);
   irec = pblk->srf;
   lkey = 0;
   status = uig_get_data(irec,0,&ldir,c);
   if (status == 0)
      {
      init_label(ldir.rel_type, irec);
      check_color(&ldir);
      current_dir_number = irec;
      ldir.level = dblk->level;
      ldir.line_font = dblk->line_font;
      ldir.line_wt = dblk->line_wt;
      ldir.view_ptr = dblk->view_ptr;
      ldir.blank = dblk->blank;
      ldir.pen_no = dblk->pen_no;
      uig_in_dispat(gblk,&ldir,c,&lkey);
      }
   if ((int)lkey > 0)
      {
      dis = pblk->of_dist * unit_scale;
      srf.key = lkey;
      status = ncl_retrieve_data_fixed (&srf);
      if (status == UU_SUCCESS) 
        { 
        uc_init_evsrfout (&srf, &evsrf);
        status = uc_retrieve_transf(srf.key,tfmat);
        u = .5;
        v = .5;
        status = uc_evsrf(UM_NORM, u, v, &srf, tfmat, &evsrf);
        }

      if (status == UU_SUCCESS) 
        { 
		  trans = UU_FALSE;
		  if (dblk->matrix_ptr != 0) trans = UU_TRUE;
		  status = uig_set_offset_dist(&srf,pblk->of_vec,evsrf.snorm,dis,
			  trans,t);
        if (status == UU_SUCCESS)
          ncl_sf_prim_analyz(&srf.key,&primtyp,primdata);
        else
          {
          ur_delete_all (lkey);
          lkey = 0;
          }
		  }
      }

   update_counts(lkey, irec);
   uu_toolfree(c);
   *key = lkey;

   return (status); 
}
/*********************************************************************
**    I_FUNCTION     :  uig_map_trimsrf (dblk, t, keysf, keycv, uv, key)
**      Map an IGES trimmed surface to a NCL trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**            dblk               ATTRIBUTE data
**            t                  Transformation matrix.
**            keysf              Key of base surface.
**            keyuv              Key of uv boundary curve.
**            keycv              Key of xyz boundary curve.
**            uv                 min, max, and periods of u and v
**                                  uv[0] = u min
**                                  uv[1] = u max
**                                  uv[2] = v min
**                                  uv[3] = v max
**                                  uv[4] = u period ( = 0.0 if not periodic)
**                                  uv[5] = u period ( = 0.0 if not periodic)
**            num                Number of inner boundaries.
**            ikeys              Keys of inner boundaries.
**       OUTPUT :  
**            key                UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_map_trimsrf (dblk, igesin, t, keysf, keyuv, keycv, uv, num, ikeys, key)
   struct dir_rec *dblk;   
   struct IG_igestrsf_rec *igesin;
   UU_KEY_ID keysf, keyuv, keycv, *ikeys;
   int num;
   UU_REAL t[12];
   UU_REAL uv[6];
   UU_KEY_ID *key;
   {
   int status;
   struct NCL_trimsf_rec tsf;
	char  p_buff[100];

   ur_setup_app_data (NCL_TRIMSF_REL, &tsf, sizeof(tsf));
   uig_update_attr(dblk);
   tsf.key = 0;
  	create_label(dblk, igesin->no_prop, igesin->prop, tsf.label, &tsf.subscr);
	if (UIG_inner_error)
	{
		sprintf (p_buff,
		"(DREC = %d) Trimmed surf %s has a problem with its inner boundary.\n",
							dblk->drec_num, tsf.label);
							uig_error (p_buff);
							sprintf (p_buff,
		"            Spatial curve failed to project onto surface.\n");
							uig_error (p_buff);
							sprintf (p_buff,
		"            Inner boundary not translated.\n");
							uig_error (p_buff);
		UIG_inner_error = 0;
	}
   tsf.closdinu = 0;
   tsf.closdinv = 0;
   tsf.offdist = 0.0;
   tsf.uv_key = keyuv;
   tsf.cv_key = keycv;
   tsf.bs_key = keysf;
   tsf.no_ibndykey = 0;
   tsf.ibndykey = 0;
   tsf.ub_min = 0.0;
   tsf.ub_max = 1.0;
   tsf.vb_min = 0.0;
   tsf.vb_max = 1.0;
   tsf.u_min = uv[0];
   tsf.u_max = uv[1];
   tsf.v_min = uv[2];
   tsf.v_max = uv[3];
	tsf.drive_type = 1;
   tsf.no_tesslst = 0;
   tsf.tesslst =NULL;

   if(drw_flag) dblk->view_ptr = drw_ptr;
   status = uig_create_geom(&tsf, t, 0,dblk->view_ptr);
   status = ur_update_data_varlist(tsf.key, 1, ikeys, 1, num*2);
/*
.....If label_type is equal to 8, we will not want to call
.....uig_match_trimsrf here, like we do with the other entities.
.....Wait until the end of uig_in_trimsrf. Reason being that 
.....the uvkey may not exist yet. 
*/

   UIG_unibase_entities++;
   *key = tsf.key;

   return (status); 
}
/*********************************************************************
**    I_FUNCTION     :  uig_map_bdsrf (dblk, t, keysf, keycv, uv, key)
**      Map an IGES bounded surface entity to a NCL trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**            dblk               ATTRIBUTE data
**            t                  Transformation matrix.
**            keysf              Key of base surface.
**            keyuv              Key of uv boundary curve.
**            keycv              Key of xyz boundary curve.
**            uv                 min, max, and periods of u and v
**                                  uv[0] = u min
**                                  uv[1] = u max
**                                  uv[2] = v min
**                                  uv[3] = v max
**                                  uv[4] = u period ( = 0.0 if not periodic)
**                                  uv[5] = u period ( = 0.0 if not periodic)
**            num                Number of inner boundaries.
**            ikeys              Keys of inner boundaries.
**       OUTPUT :  
**            key                UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************/
int uig_map_bdsrf (dblk, igesin, t, keysf, keyuv, keycv, uv, num, ikeys, key)
struct dir_rec *dblk;   
struct IG_igesbdsf_rec *igesin;
UU_KEY_ID keysf, keyuv, keycv, *ikeys;
int num;
UU_REAL t[12];
UU_REAL uv[6];
UU_KEY_ID *key;
{
	int status;
	struct NCL_trimsf_rec tsf;

	ur_setup_app_data (NCL_TRIMSF_REL, &tsf, sizeof(tsf));
	uig_update_attr(dblk);
	tsf.key = 0;
	create_label(dblk, igesin->no_prop, igesin->prop, tsf.label, &tsf.subscr);
	tsf.closdinu = 0;
	tsf.closdinv = 0;
	tsf.offdist = 0.0;
	tsf.uv_key = keyuv;
	tsf.cv_key = keycv;
	tsf.bs_key = keysf;
	tsf.no_ibndykey = 0;
	tsf.ibndykey = 0;
	tsf.ub_min = 0.0;
	tsf.ub_max = 1.0;
	tsf.vb_min = 0.0;
	tsf.vb_max = 1.0;
	tsf.u_min = uv[0];
	tsf.u_max = uv[1];
	tsf.v_min = uv[2];
	tsf.v_max = uv[3];
	tsf.drive_type = 1;
	tsf.no_tesslst = 0;
	tsf.tesslst =NULL;
	
	if(drw_flag) dblk->view_ptr = drw_ptr;
	status = uig_create_geom(&tsf, t, 0,dblk->view_ptr);
	status = ur_update_data_varlist(tsf.key, 1, ikeys, 1, num*2);
/*
.....If label_type is equal to 8, we will not want to call
.....uig_match_trimsrf here, like we do with the other entities.
.....Wait until the end of uig_in_trimsrf. Reason being that 
.....the uvkey may not exist yet. 
*/
	UIG_unibase_entities++;
	*key = tsf.key;

	return (status); 
}

/*********************************************************************
**    I_FUNCTION     :  uig_srfcls (srf,type)
**          Set closdinu, closdinv parameters. Follows srfcls()
**    PARAMETERS   
**       INPUT  : 
**              isrf  -  pointer to a surface
**              type  -  0 - set closdinu only; 1 - set closdinv only; 2 - set both
**       OUTPUT :  
**              closdinu, closdinv updated
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_srfcls (isrf,type)
	struct NCL_fixed_databag *isrf;
	int type;
{
	int status;
	struct UM_evsrfout evsrf;
	UM_coord p00,p10,p01,p11;
	UM_vector n00,n10,n01,n11;
	UU_REAL range,tol,tolu,tolv;
	struct NCL_fixed_databag *srf,msrf;
	
	uu_denter(UU_MTRC,(us,"uig_len_bs()"));
/*
.....For some reason on the SGI
.....referencing the variable lists in the surface structure
.....causes a bus error.
.....So get the a fresh copy of the surface 
.....Bobby - 11/12/09
*/
	srf = &msrf;
	srf->key = isrf->key;
	ncl_retrieve_data_fixed(srf);

	if (unit_scale < 1.) tol = 0.125;
	else tol = 0.005;

	tolu = tolv = tol;

	if (srf->rel_num == UM_RBSPLSRF_REL)
	{
		struct UM_rbsplsrf_rec *rsf;
		UM_coord *pts;
		UU_REAL rmax;
		int i,j,ku,kv;
		

		rsf = (struct UM_rbsplsrf_rec *) srf;
		pts = (UM_coord *) rsf->pt;
		ku = rsf->ku; kv = rsf->kv;
/*
..... fix tolu
*/
		if (type != 1)
		{
			rmax = 0.;
			for (i = 0; i < kv; i++)
			{
				range = 0.;
				for (j = 1; j < ku; j++)
				{
					range += um_dcccc (pts[ku*i+j],pts[ku*i+j-1]);
				}
				if (range > rmax) rmax = range;
			}
			if (rmax < 1.) tolu *= rmax;
		}
/*
..... fix tolv
*/
		if (type != 0)
		{
			rmax = 0.;
			for (i = 0; i < ku; i++)
			{
				range = 0.;
				for (j = 1; j < kv; j++)
				{
					range += um_dcccc (pts[i+ku*j],pts[i+ku*(j-1)]);
				}
				if (range > rmax) rmax = range;
			}
			if (rmax < 1.) tolv *= rmax;
		}
	}

	status = uc_evsrf(UM_NORM, 0., 0., srf, UM_idmat, &evsrf);
	if (status != UU_SUCCESS) goto done;
	um_vctovc (evsrf.sp,p00);
	um_vctovc (evsrf.snorm,n00);

	status = uc_evsrf(UM_NORM, 0., 1., srf, UM_idmat, &evsrf);
	if (status != UU_SUCCESS) goto done;
	um_vctovc (evsrf.sp,p01);
	um_vctovc (evsrf.snorm,n01);

	status = uc_evsrf(UM_NORM, 1., 0., srf, UM_idmat, &evsrf);
	if (status != UU_SUCCESS) goto done;
	um_vctovc (evsrf.sp,p10);
	um_vctovc (evsrf.snorm,n10);

	status = uc_evsrf(UM_NORM, 1., 1., srf, UM_idmat, &evsrf);
	if (status != UU_SUCCESS) goto done;
	um_vctovc (evsrf.sp,p11);
	um_vctovc (evsrf.snorm,n11);

	if (type != 0)
	{
		if (um_dcccc (p00,p01) < tolv && um_dcccc (p10,p11) < tolv)
		{
			if (fabs (um_dot(n00,n01)) >= 0.9998 && fabs (um_dot(n10,n11)) >= 0.9998)
				uig_update_closed (srf,1);
		}
	}

	if (type != 1)
	{
		if (srf->rel_num == NCL_REVSURF_REL)
		{
			status = uc_evsrf(UM_POINT, 0.5,0., srf, UM_idmat, &evsrf);
			if (status != UU_SUCCESS) goto done;
			range = um_dcccc (evsrf.sp,p00) + um_dcccc (evsrf.sp,p10);
			if (range < 1.) tolu *= range;
			
		}
		if (um_dcccc (p00,p10) < tolu && um_dcccc (p01,p11) < tolu)
		{
			if (fabs (um_dot(n00,n10)) >= 0.9998 && fabs (um_dot(n01,n11)) >= 0.9998)
				uig_update_closed (srf,0);
		}
	}

done:;
	uu_dexit;
	return (status);
}

/*********************************************************************
*********************************************************************/
int uig_update_closed (srf,iflg)
struct NCL_fixed_databag *srf;
int iflg;
{
	if (srf->rel_num == UM_RBSPLSRF_REL)
	{
		struct UM_rbsplsrf_rec *rbspsf;
		rbspsf = (struct UM_rbsplsrf_rec *)srf;
		if (iflg == 0) rbspsf->closdinu = 1;
		else rbspsf->closdinv = 1;
		ur_update_data_fixed(rbspsf);
	}
	else if (srf->rel_num == NCL_REVSURF_REL)
	{
		struct NCL_revsurf_rec *revsf;
		revsf = (struct NCL_revsurf_rec *)srf;
		if (iflg == 0) revsf->closdinu = 1;
		else revsf->closdinv = 1;
		ur_update_data_fixed(revsf);
	}

	return (0);
}
/*********************************************************************
**  I_FUNCTION: int uig_check_offset_dist(srf,offvc,snorm,dis,trans,t)
**       Set the offset distance based on the direction vector.  If
**       surface normal vector and direction vector are not in the
**       same direction the sign of the distance is switched.
**
**    PARAMETERS
**       INPUT  :
**          srf   - Current directory block.
**          offvc - offset direction vector
**          snorm - surface normal vector
**          dis   - offset distance
**          trans - transformation flag
**          t     - transformation matrix
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_set_offset_dist(srf,offvc,snorm,dis,trans,t)
struct NCL_fixed_databag *srf;
UM_vector offvc,snorm;
UU_REAL dis;
UU_LOGICAL trans;
UU_REAL t[12];
{
	int status = UU_SUCCESS;
/*
.....The vector directions do not agree if the dot product is less
.....than 0.0 and so the sign needs to be switched.
*/
	if (offvc[0]*snorm[0]+offvc[1]*snorm[1]+offvc[2]*snorm[2] < 0.0)
		dis = -dis;
	if (srf->rel_num == NCL_MESHSURF_REL)
	{
		struct NCL_meshsf_rec *msf;
		msf = (struct NCL_meshsf_rec *)srf;
		if (trans) status = uig_transf_msf(msf,t);
		msf->offset = 7;
		msf->offdist = dis;
		if (status == UU_SUCCESS) status = ur_update_data_fixed(msf);
	}
	else if (srf->rel_num == UM_RBSPLSRF_REL)
	{
		struct UM_rbsplsrf_rec *rsf;
		rsf = (struct UM_rbsplsrf_rec *)srf;
		if (trans) status = uig_transf_rbsf(rsf,t);
		rsf->offdist = dis;
		if (status == UU_SUCCESS) status = ur_update_data_fixed(rsf);
	}
	else
		status = UU_FAILURE;
	return(status);
}
