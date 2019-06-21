/*********************************************************************
**    NAME         :  neproj.c
**       CONTAINS: routines to project NCL specific entities to a
**                   drawing
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       neproj.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:42
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mcrv.h"
#include "mfort.h"
#include "mdebug.h"
#include "mattr.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_proj_curve_to_drawing(eptr, tfmat, attrptr, 
**                         drwmat, vrefpt, vpnorm,
**                         keylist, render_solids)
**       Create a list of keys (KEYLIST) of entities to be included
**       in a drawing for the given NCL curve (EPTR, TFMAT, ATTRPTR).
**       The transformation (DRWMAT) will position the viewplane
**       (VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**       scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**          eptr                 pointer to entity data (master tuple
**                               data only)
**          tfmat                matrix to position entity in MCS
**          attrptr              pointer to attribute bundle 
**          drwmat               transformation to convert MCS to DCS
**          vrefpt               reference point of viewing plane
**          vpnorm               normal of viewing plane
**          render_solids        UU_TRUE => render solid entities
**       OUTPUT :  
**          keylist              keys of projected entities are pushed
**                               onto this list of keys
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_proj_curve_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
      keylist, render_solids)
   struct NCL_curve_rec *eptr;
   UM_transf tfmat;
   struct UM_attrdata_rec *attrptr;
   UM_transf drwmat;
   UM_coord vrefpt;
   UM_vector vpnorm;
   UU_LIST *keylist;
   UU_LOGICAL render_solids;

   {
   struct UM_polyline_rec pline;
   int status;
   UM_int2 npts;
   UM_int2 maxpts;
   UM_int2 idx;
   UM_real8 tol8;
   UU_REAL tol, *pts;
   UM_coord curve_point;
   UM_coord proj_point;
   UM_coord drw_point;
   int i,j,k,savelabel;
   UU_LIST cvpoint;

   uu_denter(UU_MTRC,(us,"ncl_proj_curve_to_drawing(key=%x)", eptr->key));

   status = UU_SUCCESS;
   idx = 136;
   getifl(&idx, &maxpts);
   idx = 175;
   getsc (&idx, &tol8);
   tol = tol8;
   j = 128;
   if (j<maxpts) j = maxpts;
   uu_list_init (&cvpoint, sizeof(UM_coord), j, j);
   if (maxpts > 1)
     maxpts = ncl_evolve1_curve (eptr,tfmat, maxpts, &cvpoint);
   else
     maxpts = ncl_evolve_curve (eptr,tfmat,tol,&cvpoint,UU_NULL,UU_NULL,0);

   npts = maxpts;
   pts = (UU_REAL *) UU_LIST_ARRAY (&cvpoint);
/*
.....Save label display status
.....Bobby  -  2/24/94
*/
   savelabel = attrptr->label_on;

   k=0;
   while (npts > 1)
   {
     if (npts>71 ) npts = 71;

   /*  create polyline representation of curve by converting NCL real*8 to
       UNICAD  real representation, projecting the point to the view plane,
       transforming this point to the drawing, and adding to the polyline */
     ur_setup_data(UM_POLYLINE_REL, &pline, sizeof(pline));

     for (i=0, j=0; i<npts; i++, j+=3, k+=3)
        {
        ncl_real8_to_uureal(3, &pts[k], curve_point);
        um_nptpln(curve_point, vrefpt, vpnorm, proj_point);
        um_cctmtf(proj_point, drwmat, drw_point);
        um_vctovc(drw_point, &pline.pt[j]);
        }
     pline.no_pt = npts;

/*
.....Project label onto drawing & restore label display status
.....Bobby  -  2/24/94
*/
     if (npts == maxpts)
       ncl_proj_label_to_drw(eptr,&pline,attrptr,drwmat,vrefpt,vpnorm);

     /* create polyline in UNIBASE and display it */
     status = ncl_create_geom_drw(&pline, UM_DEFAULT_TF, attrptr);
     if (status == UU_SUCCESS) status = ur_update_displayable(pline.key,
                    UM_DISPLAYABLE);
     if (status == UU_SUCCESS) status = uc_display(&pline);
     if (status == UU_SUCCESS) uu_list_push(keylist, &pline.key);
     maxpts -= npts-1;
     npts = maxpts;
     k -= 3;
     }
/*
.....Restore label display status
.....Bobby  -  2/24/94
*/
   attrptr->label_on = savelabel;

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_proj_plane_to_drawing(eptr, tfmat, attrptr, 
**                         drwmat, vrefpt, vpnorm,
**                         keylist, render_solids)
**       Create a list of keys (KEYLIST) of entities to be included
**       in a drawing for the given NCL curve (EPTR, TFMAT, ATTRPTR).
**       The transformation (DRWMAT) will position the viewplane
**       (VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**       scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**          eptr                 pointer to entity data (master tuple
**                               data only)
**          tfmat                matrix to position entity in MCS
**          attrptr              pointer to attribute bundle 
**          drwmat               transformation to convert MCS to DCS
**          vrefpt               reference point of viewing plane
**          vpnorm               normal of viewing plane
**          render_solids        UU_TRUE => render solid entities
**       OUTPUT :  
**          keylist              keys of projected entities are pushed
**                               onto this list of keys
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* implemented by kathy to put plane in the drawing. */
int
ncl_proj_plane_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
      keylist, render_solids)
   struct NCL_nclpl_rec *eptr;
   UM_transf tfmat;
   struct UM_attrdata_rec *attrptr;
   UM_transf drwmat;
   UM_coord vrefpt;
   UM_vector vpnorm;
   UU_LIST *keylist;
   UU_LOGICAL render_solids;

   {
   struct UM_polyline_rec pline;
   int status;
   UM_int2 npts,n;
   UM_int2 maxpts;
   UM_real8 pts[150][3];
   UM_coord curve_point;
   UM_coord proj_point;
   UM_coord drw_point;
   int i,j,savelabel;

   uu_denter(UU_MTRC,(us,"ncl_proj_plane_to_drawing(key=%x)", eptr->key));

   status = UU_SUCCESS;

   /* get points along curve */
   maxpts = 140;
   n = 1;
   pleval(&eptr->key, &maxpts, &npts, pts ,&n);

   /* create polyline representation of curve by converting NCL real*8 to
      UNICAD  real representation, projecting the point to the view plane,
      transforming this point to the drawing, and adding to the polyline */
   ur_setup_data(UM_POLYLINE_REL, &pline, sizeof(pline));

   /*MILLS: initialize subscript field */
/*   pline.subscr = 0;*/
/*
.....Save label display status
.....Bobby  -  2/24/94
*/
   savelabel = attrptr->label_on;

   for (i=0, j=0; i<npts; i++, j=j+3)
      {
      ncl_real8_to_uureal(3, pts[i], curve_point);
      um_nptpln(curve_point, vrefpt, vpnorm, proj_point);
      um_cctmtf(proj_point, drwmat, drw_point);
      um_vctovc(drw_point, &pline.pt[j]);
      }
   pline.no_pt = npts;

/*
.....Project label onto drawing
.....Bobby  -  2/24/94
*/
   ncl_proj_label_to_drw(eptr,&pline,attrptr,drwmat,vrefpt,vpnorm);

   /* create polyline in UNIBASE and display it */
   attrptr->line_style = 6;
   ncl_create_geom_drw(&pline, UM_DEFAULT_TF, attrptr);
   ur_update_displayable(pline.key, UM_DISPLAYABLE);
   uc_display(&pline);

   n = 2;
   pleval(&eptr->key, &maxpts, &npts, pts ,&n);

   /* create polyline representation of curve by converting NCL real*8 to
      UNICAD  real representation, projecting the point to the view plane,
      transforming this point to the drawing, and adding to the polyline */
   ur_setup_data(UM_POLYLINE_REL, &pline, sizeof(pline));

   /*MILLS: initialize subscript field */
/*   pline.subscr = 0;*/
/*
.....Set label off for nth set of polylines
*/
   attrptr->label_on = 0;
   ncl_proj_label_to_drw(eptr,&pline,attrptr,drwmat,vrefpt,vpnorm);

   for (i=0, j=0; i<npts; i++, j=j+3)
      {
      ncl_real8_to_uureal(3, pts[i], curve_point);
      um_nptpln(curve_point, vrefpt, vpnorm, proj_point);
      um_cctmtf(proj_point, drwmat, drw_point);
      um_vctovc(drw_point, &pline.pt[j]);
      }
   pline.no_pt = npts;

   /* create polyline in UNIBASE and display it */
   attrptr->line_style = 1;
   status = ncl_create_geom_drw(&pline, UM_DEFAULT_TF, attrptr);
/*
   ur_update_displayable(pline.key, UM_DISPLAYABLE);
   uc_display(&pline); */

   if (status == UU_SUCCESS) status = ur_update_displayable(pline.key,
                UM_DISPLAYABLE);
   if (status == UU_SUCCESS) status = uc_display(&pline);
   if (status == UU_SUCCESS) uu_list_push(keylist, &pline.key);

   uu_dexit;
/*
.....Restore label display status
.....Bobby  -  2/24/94
*/
   attrptr->label_on = savelabel;
   return (status);
   }

/*
.....Added routine to project PATERN's
.....onto a drawing
.....Bobby  -  7/25/91
*/
/*********************************************************************
**    E_FUNCTION     : int ncl_proj_pn_to_drawing(eptr, tfmat, attrptr, 
**                         drwmat, vrefpt, vpnorm,
**                         keylist, render_solids)
**       Create a list of keys (KEYLIST) of entities to be included
**       in a drawing for the given NCL pattern (EPTR, TFMAT, ATTRPTR).
**       The transformation (DRWMAT) will position the viewplane
**       (VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**       scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**          eptr                 pointer to entity data (master tuple
**                               data only)
**          tfmat                matrix to position entity in MCS
**          attrptr              pointer to attribute bundle 
**          drwmat               transformation to convert MCS to DCS
**          vrefpt               reference point of viewing plane
**          vpnorm               normal of viewing plane
**          render_solids        UU_TRUE => render solid entities
**       OUTPUT :  
**          keylist              keys of projected entities are pushed
**                               onto this list of keys
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_proj_pn_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
      keylist, render_solids)
struct NCL_patern_rec *eptr;
UM_transf tfmat;
struct UM_attrdata_rec *attrptr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
UU_LIST *keylist;
UU_LOGICAL render_solids;

  {
   struct NCL_patern_rec mpt;
	UU_REAL pntrec[6];
	UU_KEY_ID key;
	UM_coord pp1,dp1,pv1,dv1;
	int status,i,npts,m,pntyp,j,savelabel;

	uu_denter(UU_MTRC,(us,"ncl_proj_pn_to_drawing(key=%x)", eptr->key));
	status = UU_SUCCESS;

/*
.....Initialize new point record
*/
	ur_setup_data(NCL_PATERN_REL,&mpt,sizeof(mpt));
/*	mpt.subscr = 0;*/
	pntyp = mpt.pntype = eptr->pntype;
   mpt.no_patpnt = 0;
   mpt.markertype = UM_ptattr.markertype;
   attrptr->line_style = 1;
/*
.....Save label display status
.....Bobby  -  2/24/94
*/
	savelabel = attrptr->label_on;
/*
.....Project label onto drawing
.....Bobby  -  2/24/94
*/
	ncl_proj_label_to_drw(eptr,&mpt,attrptr,drwmat,vrefpt,vpnorm);

   if (ncl_create_geom_drw(&mpt, UM_DEFAULT_TF, attrptr) == 0)
     {
/*
.....Loop through all points in pattern
*/
      key  = eptr->key;
      m    = 3 * eptr->pntype;
      npts = eptr->no_patpnt / m;
      j    = 1;
      for (i=1; i<=npts; i++)
        {
/*
.....Get actual point data
*/
         if (ur_retrieve_data_varlist(key, 1, pntrec, (i-1)*m+1, m) != 0)
           {
            status = UU_FAILURE;
            break;
           }
/*
.....Project pattern point onto drawing &
.....display it
*/
         else
           {
            um_nptpln(pntrec, vrefpt, vpnorm, pp1);
            um_cctmtf(pp1, drwmat, dp1);
            um_vctovc(dp1, pntrec);

            if (pntyp == 2) 
              {
               um_nptpln(&pntrec[3], vrefpt, vpnorm, pv1);
               um_vctmtf(pv1, drwmat, dv1);
               um_vctovc(dv1, &pntrec[3]);
              }
            if (ur_update_data_varlist(mpt.key, 1, pntrec, j, m) != 0)
               {
                status = UU_FAILURE;
                break;
               }
            j   = j + m;
           }
        }
        
/*   attrptr->line_style = 1;
   ncl_create_geom_drw(&mpt, UM_DEFAULT_TF, attrptr); 
   ur_update_displayable(mpt.key, UM_DISPLAYABLE);
   uc_display(&mpt); */

      if (status == UU_SUCCESS) status = ur_update_displayable(mpt.key,
                          UM_DISPLAYABLE);
      if (status == UU_SUCCESS) status = uc_display(&mpt);
      if (status == UU_SUCCESS) uu_list_push(keylist, &mpt.key);
     } 
	uu_dexit;
/*
.....Restore label display status
.....Bobby  -  2/24/94
*/
	attrptr->label_on = savelabel;
	return (status);
  }

/*********************************************************************
**    E_FUNCTION     : int ncl_proj_surf_to_drawing(eptr, tfmat, attrptr, 
**                         drwmat, vrefpt, vpnorm,
**                         keylist, render_solids)
**       Create a list of keys (KEYLIST) of entities to be included
**       in a drawing for the given NCL surface (EPTR, TFMAT, ATTRPTR).
**       The transformation (DRWMAT) will position the viewplane
**       (VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**       scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**          eptr                 pointer to entity data (master tuple
**                               data only)
**          tfmat                matrix to position entity in MCS **          attrptr              pointer to attribute bundle 
**          drwmat               transformation to convert MCS to DCS
**          vrefpt               reference point of viewing plane
**          vpnorm               normal of viewing plane
**          render_solids        UU_TRUE => render solid entities
**       OUTPUT :  
**          keylist              keys of projected entities are pushed
**                               onto this list of keys
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_proj_surf_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
      keylist, render_solids)
   struct UC_entitydatabag *eptr;
   UM_transf tfmat;
   struct UM_attrdata_rec *attrptr;
   UM_transf drwmat;
   UM_coord vrefpt;
   UM_vector vpnorm;
   UU_LIST *keylist;
   UU_LOGICAL render_solids;

   {
   struct UM_polyline_rec pline;
   int status;
   UM_int2 npts;
   UM_int2 maxpts;
   UM_int2 flag;
   UM_real8 pts[150][3];
   UM_coord curve_point;
   UM_coord proj_point;
   UM_coord drw_point;
   int i,j,savelabel;

   uu_denter(UU_MTRC,(us,"ncl_proj_surf_to_drawing(key=%x)", eptr->key));

   status = UU_SUCCESS;

   /* setup storage for polyline entity */
   ur_setup_data(UM_POLYLINE_REL, &pline, sizeof(pline));

   /*MILLS: initialize subscript field */
/*   pline.subscr = 0;*/
/*
.....Save label display status
.....Bobby  -  2/24/94
*/
	savelabel = attrptr->label_on;

   /* maximum points to get along surface  */
   /* changed from 150 to 80 to fix the stack dump. kathy */

   maxpts = 80;

   /* repeatedly call sfeval to get surface traces; set flag to 0
      to indicate first call; sfeval will set it to -1 if no more
      trace data */
   flag = 0;
   while (flag != -1)
      {
      sfeval(&eptr->key, &maxpts, &npts, pts, &flag);

      /* create polyline representation of surface by converting NCL real*8 to
         UNICAD  real representation, projecting the point to the view plane,
         transforming this point to the drawing, and adding to the polyline */
      if (flag != -1)
         {
         for (i=0, j=0; i<npts; i++, j=j+3)
            {
            sprintf(UM_sbuf,"sf point=(%g,%g,%g)", pts[i][0],pts[i][1],pts[i][2]);
            um_pscroll(UM_sbuf);
            ncl_real8_to_uureal(3, pts[i], curve_point);
            um_nptpln(curve_point, vrefpt, vpnorm, proj_point);
            um_cctmtf(proj_point, drwmat, drw_point);
            um_vctovc(drw_point, &pline.pt[j]);
            }
         pline.no_pt = npts;
/*
.....Project label onto drawing
.....Bobby  -  2/24/94
*/
		ncl_proj_label_to_drw(eptr,&pline,attrptr,drwmat,vrefpt,vpnorm);
      
         /* create polyline in UNIBASE and display it */
         status = ncl_create_geom_drw(&pline, UM_DEFAULT_TF, attrptr);
/*         ur_update_displayable(pline.key, UM_DISPLAYABLE);
         uc_display(&pline); */

         if (status == UU_SUCCESS) status = ur_update_displayable(pline.key,
                       UM_DISPLAYABLE);
         if (status == UU_SUCCESS) status = uc_display(&pline);
         if (status == UU_SUCCESS) uu_list_push(keylist, &pline.key);
/*
.....Turn label off for subsequent polylines
*/
		attrptr->label_on = 0;
         }
      }

   uu_dexit;
/*
.....Restore label display status
.....Bobby  -  2/24/94
*/
	attrptr->label_on = savelabel;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_proj_pntvec_to_drawing(eptr, tfmat, attrptr, 
**                         drwmat, vrefpt, vpnorm,
**                         keylist, render_solids)
**       Create a list of keys (KEYLIST) of entities to be included
**       in a drawing for the given NCL PNTVEC (EPTR, TFMAT, ATTRPTR).
**       The transformation (DRWMAT) will position the viewplane
**       (VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**       scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**          eptr                 pointer to entity data (master tuple
**                               data only)
**          tfmat                matrix to position entity in MCS
**          attrptr              pointer to attribute bundle 
**          drwmat               transformation to convert MCS to DCS
**          vrefpt               reference point of viewing plane
**          vpnorm               normal of viewing plane
**          render_solids        UU_TRUE => render solid entities
**       OUTPUT :  
**          keylist              keys of projected entities are pushed
**                               onto this list of keys
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_proj_pntvec_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
      keylist, render_solids)
   struct NCL_nclpv_rec *eptr;
   UM_transf tfmat;
   struct UM_attrdata_rec *attrptr;
   UM_transf drwmat;
   UM_coord vrefpt;
   UM_vector vpnorm;
   UU_LIST *keylist;
   UU_LOGICAL render_solids;

   {
   struct NCL_nclpv_rec pline;
   int status,savelabel;
   UM_coord proj_point;
   UM_coord drw_point;
   UM_coord proj_vec;
   UM_coord drw_vec;


   uu_denter(UU_MTRC,(us,"ncl_proj_pntvec_to_drawing(key=%x)", eptr->key));

   status = UU_SUCCESS;

   /* get points along curve */

   ur_setup_data(NCL_POINTVEC_REL, &pline, sizeof(pline));

/*   pline.subscr = 0;*/
/*
.....Save label display status
.....Bobby  -  2/24/94
*/
	savelabel = attrptr->label_on;

   um_vctovc(eptr->pt,proj_point);
   um_vctovc(eptr->ve,proj_vec);
   um_nptpln(proj_point, vrefpt, vpnorm, proj_point);
   um_cctmtf(proj_point, drwmat, drw_point);
   um_vctovc(drw_point, pline.pt);
   um_nptpln(proj_vec, vrefpt, vpnorm, proj_vec);
   um_vctmtf(proj_vec, drwmat, drw_vec);
   um_vctovc(drw_vec, pline.ve);
/*
.....Project label onto drawing
.....Bobby  -  2/24/94
*/
	ncl_proj_label_to_drw(eptr,&pline,attrptr,drwmat,vrefpt,vpnorm);

   /* create poinvector in UNIBASE and display it */
   attrptr->line_style = 1;
   status = ncl_create_geom_drw(&pline, UM_DEFAULT_TF, attrptr);

   if (status == UU_SUCCESS) status = ur_update_displayable(pline.key,
                UM_DISPLAYABLE);
   if (status == UU_SUCCESS) status = uc_display(&pline);
   if (status == UU_SUCCESS) uu_list_push(keylist, &pline.key);

   uu_dexit;
/*
.....Restore label display status
.....Bobby  -  2/24/94
*/
	attrptr->label_on = savelabel;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_proj_vec_to_drawing(eptr, tfmat, attrptr, 
**                         drwmat, vrefpt, vpnorm,
**                         keylist, render_solids)
**       Create a list of keys (KEYLIST) of entities to be included
**       in a drawing for the given NCL VEC (EPTR, TFMAT, ATTRPTR).
**       The transformation (DRWMAT) will position the viewplane
**       (VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**       scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**          eptr                 pointer to entity data (master tuple
**                               data only)
**          tfmat                matrix to position entity in MCS
**          attrptr              pointer to attribute bundle 
**          drwmat               transformation to convert MCS to DCS
**          vrefpt               reference point of viewing plane
**          vpnorm               normal of viewing plane
**          render_solids        UU_TRUE => render solid entities
**       OUTPUT :  
**          keylist              keys of projected entities are pushed
**                               onto this list of keys
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_proj_vec_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
      keylist, render_solids)
   struct NCL_vector_rec *eptr;
   UM_transf tfmat;
   struct UM_attrdata_rec *attrptr;
   UM_transf drwmat;
   UM_coord vrefpt;
   UM_vector vpnorm;
   UU_LIST *keylist;
   UU_LOGICAL render_solids;

   {
   struct NCL_nclpv_rec pline;
   int status,savelabel;
   UM_coord proj_point;
   UM_coord drw_point;
   UM_coord proj_vec;
   UM_coord drw_vec;


   uu_denter(UU_MTRC,(us,"ncl_proj_vec_to_drawing(key=%x)", eptr->key));

   status = UU_SUCCESS;

   /* get points along curve */

   ur_setup_data(NCL_POINTVEC_REL, &pline, sizeof(pline));
/*   pline.subscr = 0;*/
/*
.....Save label display status
.....Bobby  -  2/24/94
*/
	savelabel = attrptr->label_on;

   proj_point[0] = 0.0;
   proj_point[1] = 0.0;
   proj_point[2] = 0.0;
   um_nptpln(proj_point, vrefpt, vpnorm, proj_point);
   um_cctmtf(proj_point, drwmat, drw_point);
   um_vctovc(eptr->vec,proj_vec);
   um_nptpln(proj_vec, vrefpt, vpnorm, proj_vec);
   um_vctmtf(proj_vec, drwmat, drw_vec);
   um_vctovc(drw_point, pline.pt);
   um_vctovc(drw_vec, pline.ve);
/*
.....Project label onto drawing
.....Bobby  -  2/24/94
*/
	ncl_proj_label_to_drw(eptr,&pline,attrptr,drwmat,vrefpt,vpnorm);

   /* create vector in UNIBASE and display it */
   attrptr->line_style = 1;
   status = ncl_create_geom_drw(&pline, UM_DEFAULT_TF, attrptr);

   if (status == UU_SUCCESS) status = ur_update_displayable(pline.key,
                UM_DISPLAYABLE);
   if (status == UU_SUCCESS) status = uc_display(&pline);
   if (status == UU_SUCCESS) uu_list_push(keylist, &pline.key);

   uu_dexit;
/*
.....Restore label display status
.....Bobby  -  2/24/94
*/
	attrptr->label_on = savelabel;
   return (status);
   }

