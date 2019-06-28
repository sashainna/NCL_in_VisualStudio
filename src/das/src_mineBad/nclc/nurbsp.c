/*********************************************************************
**    NAME         :  nurbsp.c
**       CONTAINS:
**     int nclu_cv_rbsp ()
**     int nclu_ruledbsp ()
**     int nclu_rbsf_fitcurves ()
**     int nclu_rbsf_4bndycrvs (itsk)
**     nclu_revsrf()
**     nclu_tabcyl()
**     nclu_plane_rect()
**     nclu_cylinder()
**     nclu_cone()
**     nclu_sphere()
**     nclu_torus()
**    COPYRIGHT 1991 (c) Numerical Control Computer Sciences Inc.
**               All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nurbsp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:14
*********************************************************************/

#include "udebug.h"
#include "modef.h"
#include "mdeval.h"
#include "gobas.h"
#include "dasnog.h"
#include "dselmask.h"
#include "uhep.h"
#include "ulist.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mattr.h"
#include "mcrv.h"
#include "nccs.h"
#include "msrf.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : nclu_cv_rbsp ()
**       Create B-spline curve.
**    PARAMETERS   
**       INPUT  : 
**          itsk    - = 0 interpolate
**                    = 1 fit
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_cv_rbsp (itsk)
int itsk;
   {
   int i, status;
   int  numint;
   int npts;
   struct UM_rbsplcrv_rec crv;

/* UM_coord pt, opt; ****/
   UD_NDCLOCREC pt;
   UM_coord opt,tmp;

   UM_vector vec;
   struct NCL_crvgen_rec seg, *segp, *esegp;
   UU_LIST seglist;

   uu_denter(UU_MTRC,(us,"nclu_cv_rbsp ()"));

   uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), 20, 20);

   do
      {
      npts = 0;
      seglist.cur_cnt = 0;
      do
         {
         ud_ldas(UD_DASCART, /* enter point for cubic */ UM_APPGEO, 25, &pt, 1,
            &numint, UD_NODEFAULT);
         if (numint > 0)
            {

            for(i=0; i<3; i++) tmp[i] = pt.cord[i];

            if (npts>0 && um_cceqcc(opt, tmp))
               {
               uu_outputerr("point can not equal previous point");
               }
            else
               {
               um_vctovc (tmp, opt);
               seg.x = pt.cord[0];
               seg.y = pt.cord[1];
               seg.z = pt.cord[2];
               seg.inv = 0;
               uu_list_push (&seglist, &seg);
               npts++;
               }
            }
         }
      while (numint > 0);
   
      if (npts > 1)
         {
         segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);
         ud_ldas(UD_DASVEC, /* direction at start point */ UM_APPGEO, 26, vec,
            1, &numint, UD_NODEFAULT);
         if (numint > 0)
            {
            um_unitvc(vec, vec);
            segp->a = vec[0];
            segp->b = vec[1];
            segp->c = vec[2];
            segp->inv = 1;
            }
   
         ud_ldas(UD_DASVEC, /* direction at end point */ UM_APPGEO, 27, vec,
            1, &numint, UD_NODEFAULT);
         if (numint > 0)
            {
            um_unitvc(vec, vec);
            esegp = segp+npts-1;
            esegp->a = vec[0];
            esegp->b = vec[1];
            esegp->c = vec[2];
            esegp->inv = 1;
            }
         status = ncl_interp_rbsp (npts, segp, itsk, &crv);
         if (status == UU_SUCCESS)
           status = uc_display(&crv);
         else
           uu_outputerr("Error creating B-spline");
         }
      } while (npts > 0);

   uu_list_free (&seglist);

   uu_dexitstatus("nclu_cv_rbsp ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : nclu_ruledbsp ()
**       User interface for creating ruled rbsf.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_ruledbsp ()
   {
   int numint, numint2, status;
   UM_PLOCREC pick1, pick2;
   struct NCL_fixed_databag bc1, bc2;
   struct UM_rbsplsrf_rec sf;

   uu_denter(UU_MTRC,(us,"nclu_ruledbsp ()"));

   ud_lgeo(UU_TRUE, UD_allcurves_pts);

   do
      {  
      um_dl_pldas(UD_DASPCKLOC,/* pick first curve for ruled surface */
         UM_APPGEO, 31, &pick1, 1, &numint, 2);
      if (numint > 0)
        { 
        um_dl_pldas(UD_DASPCKLOC,/* pick second curve for ruled surface */
           UM_APPGEO, 32, &pick2, 1, &numint2, 2);
        if (numint2 > 0)
          { 
          /* retrieve first curve */
          bc1.key = um_get_pickkey(&pick1, 1);
          status = ncl_retrieve_data_fixed(&bc1);
          if (status == UU_SUCCESS)
            {
            /* retrieve second curve */
            bc2.key = um_get_pickkey(&pick2, 1);
            status = ncl_retrieve_data_fixed(&bc2);
            }
          if (status == UU_SUCCESS)
            {
            /* create and display ruled surface */
            sf.key = 0;
/*
..... possible "bowtie" twisting is not checked
*/
            status = ncl_rldgen (&bc1, &bc2, &sf, 0);
            if (status == UU_SUCCESS)
              status = uc_display(&sf);
            else
              uu_outputerr("Error creating B-spline surface");
            }
          }
        }
      } while (numint>0)

   uu_dexitstatus("nclu_ruledbsp ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : nclu_rbsf_fitcurves ()
**       User interface for fitting rbsf through curves.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_rbsf_fitcurves ()
   {
   int numint, ncrvs, status;
   UM_PLOCREC pick1;
   struct NCL_fixed_databag bc1, *bcp;
   struct UM_rbsplsrf_rec sf;
   UU_LIST clist;

   uu_denter(UU_MTRC,(us,"nclu_rbsf_fitcurves ()"));

   uu_list_init (&clist, sizeof(struct NCL_fixed_databag), 20, 20);

   ud_lgeo(UU_TRUE, UD_allcurves);

   do
    {  
    ncrvs = 0;
    do
      {
      um_dl_pldas(UD_DASPCKLOC,/* pick next curve to fit surface through */
         UM_APPGEO, 55, &pick1, 1, &numint, 2);
      if (numint > 0)
        {
 
      /* retrieve next curve */
        bc1.key = um_get_pickkey(&pick1, 1);
        status = ncl_retrieve_data_fixed(&bc1);
        uu_list_push (&clist, &bc1);
        ncrvs++;
        }
      } while (numint > 0 && status == UU_SUCCESS);
   
      if (ncrvs > 1 && status == UU_SUCCESS)
        {
        /* create and display surface */
        bcp = (struct NCL_fixed_databag *) UU_LIST_ARRAY (&clist);
        sf.key = 0;
        if (ncrvs == 2)
          status = ncl_rldgen (bcp, &bcp[1], &sf,0);
/*
..... possible "bowtie" twisting is not checked
*/
        else if (ncrvs > 2)
        {
          int i,*ktwist;

          ktwist = UU_NULL;
          ktwist = (int *)uu_malloc(2*ncrvs*sizeof(int));
          if (ktwist == UU_NULL) 
            status = UU_FAILURE;
          else
          {
            for (i=0;i<2*ncrvs;i++) ktwist[i]=0;
            status = ncl_srfdef (0, ncrvs, bcp, 0, &sf,ktwist);
            uu_free (ktwist);
          }
        }
        else
          status = UU_FAILURE;

        if (status == UU_SUCCESS)
          status = uc_display(&sf);
        else
          uu_outputerr("Error creating B-spline Surface");
        }
    clist.cur_cnt = 0;
    } while (ncrvs > 0);
 
   uu_list_free (&clist);

   uu_dexitstatus("nclu_rbsf_fitcurves ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : nclu_rbsf_4bndycrvs (itsk)
**       User interface for contructing a rbsf from four bounding curves.
**    PARAMETERS   
**       INPUT  : 
**          itsk       - = 0, linear interpolation
**                       = 1, cubic interpolation
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_rbsf_4bndycrvs (itsk)
int itsk;
   {
   int i, numint, status;
   UU_LOGICAL done;
   UM_PLOCREC pick1;
   struct NCL_fixed_databag bcvs[4];
   struct UM_rbsplsrf_rec sf;

   uu_denter(UU_MTRC,(us,"nclu_rbsf_4bndycrvs ()"));

   ud_lgeo(UU_TRUE, UD_allcurves);

   do
    {  
    um_dl_pldas(UD_DASPCKLOC,/* pick first boundary curve */
       UM_APPGEO, 66, &pick1, 1, &numint, 2);
    done = numint < 1;
    if (!done)
      {
 
      /* retrieve first curve */
      bcvs[0].key = um_get_pickkey(&pick1, 1);
      status = ncl_retrieve_data_fixed(&bcvs[0]);
      if (status != UU_SUCCESS)
        uu_outputerr("Error retrieving boundary curve");

      for (i=1; i<4 && numint>0 && status == UU_SUCCESS; i++)
        {
        um_dl_pldas(UD_DASPCKLOC,/* pick 2nd,3rd,4th boundary curve */
           UM_APPGEO, 66+i, &pick1, 1, &numint, 2);
        if (numint>0)
          {
      /* retrieve 2nd,3rd,4th curve */
          bcvs[i].key = um_get_pickkey(&pick1, 1);
          status = ncl_retrieve_data_fixed(&bcvs[i]);
          if (status != UU_SUCCESS)
            uu_outputerr("Error retrieving boundary curve");
          }
        }

      if (status == UU_SUCCESS && numint > 0)
        {
        /* create and display surface */
        sf.key = 0;
        status = ncl_4crvsrf (itsk, bcvs, &sf);
        if (status == UU_SUCCESS)
          status = uc_display(&sf);
        else
          uu_outputerr("Error creating B-spline Surface");
        }
      }
    } while (!done);
 
   uu_dexitstatus("nclu_rbsf_4bndycrvs ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : nclu_revsrf()
**       Define a surface of revolution by prompting the user for
**          1. a curve to revolve
**          2. a point and vector defining the axis of revolution
**          3. a start and end angle
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_revsrf()

   {
   int numint;
/**UM_coord axis_pt; **/
   UD_NDCLOCREC axis_pt;

   UM_vector axis_vec;
   UM_angle start_ang, end_ang;
   struct NCL_fixed_databag crv;
   struct UM_rbsplsrf_rec srf;
   UM_PICKENT pick;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_revsrf()"));

   ud_lgeo(UU_TRUE, UD_revsrf_curves);

   while (UU_TRUE)
      {
      um_dl_pdas(UD_DASPICK,/* pick curve for surface of revolution */UM_APPGEO,
            17, &pick, 1, &numint, 1);
      if (numint < 1) goto done;
      crv.key = um_get_pickkey(&pick, 1);
      ncl_retrieve_data_fixed (&crv);

      ud_ldas(UD_DASCART,/* point defining axis of revolution */UM_APPGEO,
            18, &axis_pt, 1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;

      ud_ldas(UD_DASVEC,/* vector defining axis of revolution */UM_APPGEO,
            19, axis_vec, 1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;

      start_ang = 0.0;
      end_ang = UM_TWOPI;
      ud_ldas(UD_DASANGLE,/* starting angle of surface segment */UM_APPGEO, 11, 
            &start_ang, 1, &numint, UD_DEFAULT);
      if (numint > 0)
         {
         ud_ldas(UD_DASANGLE,/* ending angle of surface segment */UM_APPGEO, 12, 
               &end_ang, 1, &numint, UD_NODEFAULT);
         if (numint < 1) goto repeat;
         }

      um_unitvc(axis_vec, axis_vec);
      srf.key = 0;
      status = ncl_revsrf(&axis_pt, axis_vec, start_ang, end_ang, &crv, &srf);
      if (status == UU_SUCCESS)
        status = uc_display(&srf);
      else
        uu_outputerr("Error creating B-spline Surface");

repeat:;
      }

done:;
   uu_dexitstatus("nclu_revsrf ()", status);
   return;
   }
/*********************************************************************
**    E_FUNCTION     : nclu_tabcyl()
**       Define a tabulated cylinder surface segment by prompting for
**          1. a curve to lift 
**          2. a vector specifying direction (and distance) to 
**             lift curve
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tabcyl()

   {
   int numint;
   int status;
   UU_LOGICAL initialize;
   UM_vector vec;
   struct NCL_fixed_databag uc1;
   struct UM_rbsplsrf_rec srf;

   uu_denter(UU_MTRC,(us,"nclu_tabcyl()"));

   /* limit pickable entities to those which may be used to create
      a tabulated cylinder */
   ud_lgeo(UU_TRUE, UD_ruled_surf_curves);

   while (UU_TRUE)
      {
      ud_ldas(UD_DASVEC,/* Enter direction vector defining tabulated cylinder */
         UM_APPGEO, 54, vec, 1, &numint, 2);
      if (numint < 1) goto done;
      
      ud_ldas(UD_DASSELECT, /*pick base curves for tabulated cylinder */
         UM_APPGEO, 53, UU_NULL, 1, &numint,UD_NODEFAULT);
      if (numint <= 0) goto repeat;

      initialize = UU_TRUE;
      while(ud_gnxt(initialize, UU_NULL, &uc1.key, 1) == UU_TRUE)
         {
         initialize = UU_FALSE;

         /* retrieve base curve */
         status = ncl_retrieve_data_fixed (&uc1);
         if (status != UU_SUCCESS) goto repeat;
   
         /* create and display tabcyl */
         srf.key = 0;
         status = ncl_tabcyl(&uc1, vec, &srf);

         if (status == UU_SUCCESS)
           status = uc_display(&srf);
         else
           uu_outputerr("Error creating B-spline Surface");
   
         }

repeat:;
      }

done:;

   uu_dexitstatus("nclu_tabcyl ()", status);
   return;
   }
/*********************************************************************
**    E_FUNCTION     : nclu_plane_rect()
**       Define a plane surface (parallelogram) by asking the user for
**          1. two point defining the base of the quadrilateral
**          2. another point which with the first point specified
**             defined the side of the quadrilateral
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_plane_rect()

   {
/*****   UM_coord pt[3]; ******/
   UM_coord tmp[3]; 
   UD_NDCLOCREC pt[3];

   UM_vector vec;
   int numint;
   int dim;
   int err,i;
   int status;
   UU_REAL space[2][3];
   struct UM_rbsplsrf_rec srf;
   struct UM_line_rec l1, l2;

   uu_denter(UU_MTRC,(us,"nclu_plane_rect()"));

   l1.key = l2.key = 0;
   l1.rel_num = l2.rel_num = UM_LINE_REL;

   while (UU_TRUE)
      {
      ud_ldas(UD_DASCART,/*lower left point on plane */UM_APPGEO, 2, &pt[0], 
             1, &numint, UD_NODEFAULT);
      if (numint < 1) goto done;

      ud_ldas(UD_DASCART,/*lower right point on plane */UM_APPGEO, 3, &pt[1], 
             1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;

      ud_ldas(UD_DASCART,/*upper left point on plane */UM_APPGEO, 4, &pt[2], 
             1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;

      for(i=0; i<3; i++) tmp[0][i] = pt[0].cord[i];
      for(i=0; i<3; i++) tmp[1][i] = pt[1].cord[i];
      for(i=0; i<3; i++) tmp[2][i] = pt[2].cord[i];

      um_span_ptlist(3, tmp, &dim, space, &err);
      if (( dim != 2) || (err != -1))
         {
         uu_outputerr("points do not define a plane");
         goto repeat;
         }

      um_vctovc (&pt[0], l1.spt);
      um_vctovc (&pt[1], l1.ept);
      um_vctovc (&pt[2], l2.spt);
      um_vcmnvc (&pt[1], &pt[0], vec);
      um_vcplvc (&pt[2], vec, l2.ept);
      srf.key = 0;

      status = ncl_rldgen (&l1, &l2, &srf,0);
/*
..... possible "bowtie" twisting is not checked
*/

      if (status == UU_SUCCESS)
        status = uc_display(&srf);
      else
        uu_outputerr("Error creating B-spline Surface");

repeat:;
      }

done:;
   uu_dexitstatus("nclu_plane_rect ()", status);
   return;
   }
/*********************************************************************
**    E_FUNCTION     : nclu_cylinder()
**       Define a complete cylindrical surface segment by prompting for
**          1. the top point of the cylinder axis
**          2. the bottom point of the cylinder axis
**          3. the radius of the cylinder
**       Optionally, prompt the user for
**          4. a point which together with the top and bottom points
**             of the cylinder axis defines a plane which angle are
**             measured from
**          5. a start and end angle for a partial cylindrical
**             segment
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_cylinder()

   {
/**UM_coord top_pt; ***/
/**UM_coord bottom_pt; ***/
/**UM_coord zero_pt; ***/
/**UM_coord end_pt; ***/
   UD_NDCLOCREC top_pt;
   UD_NDCLOCREC bottom_pt;
   UD_NDCLOCREC zero_pt;
   UD_NDCLOCREC end_pt;

   UM_coord npt;
   UM_coord pln_zero_pt;
   UM_coord pln_end_pt;
   UM_vector cyl_axis;
   UM_vector vec, vec1, vec2;
   UM_length radius;
   UM_angle start_angle, end_angle;
   struct UM_line_rec ln;
   int numint;
   int markval;
   int dstatus;
   int status;
   int option;
   struct UM_rbsplsrf_rec srf;

   uu_denter(UU_MTRC,(us,"nclu_cylinder()"));

   ln.key = 0;
   ln.rel_num = UM_LINE_REL;

   while (UU_TRUE)
      {
      ud_ldas(UD_DASCART,/* top point of cylinder axis */UM_APPGEO, 7, &top_pt, 
             1, &numint, UD_NODEFAULT);
      if (numint < 1) goto done;

newaxis:
      UD_MARK(markval, UU_FALSE);
      if (markval == 0)
         {
         option = 2;
         do
            {
            if (option == 1)
               {
               dstatus = ud_ldas(UD_DASVEC,/* direction of cylinder axis */
                  UM_APPGEO, 8, cyl_axis, 1, &numint, UD_NODEFAULT);
               if (dstatus == 2) option = 2; else option = 0;

               if (numint == 1) um_vcplvc(&top_pt, cyl_axis, &bottom_pt);
               }
            else if (option == 2)
               {
               dstatus = ud_ldas(UD_DASCART,/* bottom point of cylinder axis */
                  UM_APPGEO, 75, &bottom_pt, 1, &numint, UD_NODEFAULT);
               if (dstatus == 2) option = 1; else option = 0;
               if ((option == 0) && (numint == 1))
                   um_vcmnvc(&bottom_pt, &top_pt, cyl_axis);
               }
            }
         while (option != 0);
         }
      UD_UNMARK(markval);
      if (markval != 0) goto done;
      if (numint < 1) goto repeat;
      if (um_cceqcc(cyl_axis, UM_zerovec))
         {
         uu_outputerr("ERROR: cylinder axis must have length");
         goto newaxis;
         }

newrad:
      ud_ldas(UD_DASDISTANCE,/* radius of cylinder */UM_APPGEO, 9, &radius, 
             1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;
      if (radius < UM_FUZZ)
         {
         uu_outputerr("ERROR: radius of cylinder is too small");
         goto newrad;
         }

      ud_ldas(UD_DASCART,/* point defining start of surface segment */
            UM_APPGEO, 10, &zero_pt, 1, &numint, UD_NODEFAULT);
      if (numint < 1)
         {
         um_unitvc (cyl_axis, vec1);
         um_perpvc (vec1, vec2);
         um_unitvc (vec2, vec2);
         um_vctmsc (vec2, radius, vec2);
         um_vcplvc (&top_pt, vec2, ln.ept);
         um_vcplvc (&bottom_pt, vec2, ln.spt);
         start_angle = 0.0;
         end_angle = UM_TWOPI;;
         }
      else
         {
         um_unitvc(cyl_axis, vec);
         um_nptln(&zero_pt, &top_pt, vec, npt);
         if (um_cceqcc(npt, &zero_pt))
            {
            uu_outputerr("ERROR: point is on cylinder axis");
            goto repeat;
            }

         start_angle = 0.0;
   
newangle:
         UD_MARK(markval, UU_FALSE);
         if (markval == 0)
            {
            option = 2;
            do
               {
               if (option == 1)
                  {
                  dstatus = ud_ldas(UD_DASANGLE,/* ending angle of srf segment */
                        UM_APPGEO, 12, &end_angle, 1, &numint, UD_NODEFAULT);
                  if (dstatus == 2) option = 2; else option = 0;
                  }
               else if (option == 2)
                  {
                  dstatus = ud_ldas(UD_DASCART,/* end point of surface segment */
                     UM_APPGEO, 77, &end_pt, 1, &numint, UD_NODEFAULT);
                  if (dstatus == 2) option = 1; else option = 0;
                  if ((option == 0) && (numint == 1))
                     {
                     um_nptpln(&zero_pt, &top_pt, vec, pln_zero_pt);
                     um_nptpln(&end_pt, &top_pt, vec, pln_end_pt);
                     um_vcmnvc(pln_zero_pt, &top_pt, vec1);
                     um_unitvc(vec1, vec1);
                     um_vcmnvc(pln_end_pt, &top_pt, vec2);
                     um_unitvc(vec2, vec2);
                     end_angle = um_angle2p(vec1, vec2, cyl_axis);
                     }
                  }
               }
            while (option != 0);
            }
         UD_UNMARK(markval);
         if (markval != 0) goto done;
         if (numint < 1) goto repeat;
         if ((fabs(end_angle) < UM_FUZZ) || (fabs(end_angle) > UM_TWOPI - UM_FUZZ))
            {
            uu_outputerr("illegal end angle");
            goto newangle;
            }
         um_vcmnvc (&zero_pt, npt, vec1);
         um_unitvc (vec1, vec1);
         um_vctmsc (vec1, radius, vec1);
         um_vcplvc (&top_pt, vec1, ln.ept);
         um_vcplvc (ln.ept, cyl_axis, ln.spt);
         }
      srf.key = 0;
      status =  ncl_revsrf (&top_pt, cyl_axis, start_angle, end_angle, &ln, &srf);

      if (status == UU_SUCCESS)
        status = uc_display(&srf);
      else
        uu_outputerr("Error creating B-spline Surface");

repeat:;
      }

done:;
   uu_dexitstatus("nclu_cylinder ()", status);
   return;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_cone()
**       Define a conical surface segment by prompting for
**          1. the top point and radius
**          2. the bottom point and radius
**       Optionally, prompt for
**          3. a point which together with the top and bottom point
**             will be used to define a plane from which angles
**             are measured
**          4. start and end angle
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : It is presently not possible to enter a value of
**                   Zero for the radii due to existing limitations
**                   with the AG Library.  The User Interface has been
**                   modified to screen any values entered for the
**                   radii and "adjust" them if they are smaller than
**                   the value of UM_FUZZ.  Values less than this will
**                   be automatically set equal to 10 times UM_FUZZ
**                   and the user "warned" of the adjustment.
*********************************************************************/
void nclu_cone()

   {
/**UM_coord top_pt;
   UM_coord bottom_pt;
   UM_coord zero_pt;
   UM_coord end_pt; **/
   UD_NDCLOCREC top_pt;
   UD_NDCLOCREC bottom_pt;
   UD_NDCLOCREC zero_pt;
   UD_NDCLOCREC end_pt;

   UM_coord npt;
   UM_coord pln_zero_pt;
   UM_coord pln_end_pt;
   UM_vector cone_axis;
   UM_vector vec, vec1, vec2;
   UM_length radius1, radius2;
   UM_angle start_angle, end_angle;
   struct UM_line_rec ln;
   int numint;
   int markval;
   int dstatus;
   int status;
   int option;
   struct UM_rbsplsrf_rec srf;

   uu_denter(UU_MTRC,(us,"nclu_cone()"));

   ln.key = 0;
   ln.rel_num = UM_LINE_REL;

   while (UU_TRUE)
      {
      ud_ldas(UD_DASCART,/* top point of cone axis */UM_APPGEO, 13, &top_pt, 
             1, &numint, UD_NODEFAULT);
      if (numint < 1) goto done;

newrad1:
      ud_ldas(UD_DASDISTANCE,/* radius of cone at top point */UM_APPGEO, 14,
            &radius1, 1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;
/*       if (radius1 < UM_FUZZ) */
/*          { */
/*          uu_outputerr("WARNING: Radius too small - adjusted to 0.001 cm"); */
/*          radius1 = 10*UM_FUZZ; */
/*          } */

newaxis:
      UD_MARK(markval, UU_FALSE);
      if (markval == 0)
         {
         option = 2;
         do
            {
            if (option == 1)
               {
               dstatus = ud_ldas(UD_DASVEC,/* direction of cone axis */
                     UM_APPGEO, 15, cone_axis, 1, &numint, UD_NODEFAULT);
               if (dstatus == 2) option = 2; else option = 0;
               if (numint == 1) um_vcplvc(&top_pt, cone_axis, &bottom_pt);
               }
            else if (option == 2)
               {
               dstatus = ud_ldas(UD_DASCART,/* bottom point of cone axis */
                  UM_APPGEO, 76, &bottom_pt, 1, &numint, UD_NODEFAULT);
               if (dstatus == 2) option = 1; else option = 0;
               if ((option == 0) && (numint == 1))
                   um_vcmnvc(&bottom_pt, &top_pt, cone_axis);
               }
            }
         while (option != 0);
         }
      UD_UNMARK(markval);
      if (markval != 0) goto done;
      if (numint < 1) goto repeat;
      if (um_cceqcc(&top_pt, &bottom_pt))
         {
         uu_outputerr("ERROR: cone axis must have length");
         goto newaxis;
         }

newrad2:
      ud_ldas(UD_DASDISTANCE,/* radius of cone at bottom point */UM_APPGEO, 16,
            &radius2, 1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;
      if (radius2 < UM_FUZZ)
         {
/*          uu_outputerr("WARNING: radius too small -- adjusted to 0.001 cm"); */
         radius2 = 10*UM_FUZZ;
/*          if (radius1 == 10*UM_FUZZ) */
/*             { */
/*             uu_outputerr("ERROR: both radii cannot be zero -- retry"); */
/*             goto newrad2; */
/*             } */
         }

      ud_ldas(UD_DASCART,/* point defining start of surface segment */
            UM_APPGEO, 10, &zero_pt, 1, &numint, UD_NODEFAULT);

      if (numint < 1)
         {
         um_unitvc (cone_axis, vec);
         um_perpvc (vec, vec1);
         um_unitvc (vec1, vec1);
         um_vctmsc (vec1, radius1, vec2);
         um_vcplvc (&top_pt, vec2, ln.ept);
         um_vctmsc (vec1, radius2, vec2);
         um_vcplvc (&bottom_pt, vec2, ln.spt);
         start_angle = 0.0;
         end_angle = UM_TWOPI;;
         }
      else
         {
         um_unitvc(cone_axis, vec);
         um_nptln(&zero_pt, &top_pt, vec, npt);
         if (um_cceqcc(npt, &zero_pt))
            {
            uu_outputerr("ERROR: point is on cone axis");
            goto repeat;
            }

         start_angle = 0.0;
   
newangle:
         UD_MARK(markval, UU_FALSE);
         if (markval == 0)
            {
            option = 1;
            do
               {
               if (option == 1)
                  {
                  dstatus = ud_ldas(UD_DASANGLE,/* ending angle of srf seg */
                        UM_APPGEO, 12, &end_angle, 1, &numint, UD_NODEFAULT);
                  if (dstatus == 2) option = 2; else option = 0;
                  }
               else if (option == 2)
                  {
                  dstatus = ud_ldas(UD_DASCART,/* end point of surface segment */
                     UM_APPGEO, 77, &end_pt, 1, &numint, UD_NODEFAULT);
                  if (dstatus == 2) option = 1; else option = 0;
                  if ((option == 0) && (numint == 1))
                     {
                     um_nptpln(&zero_pt, &top_pt, vec, pln_zero_pt);
                     um_nptpln(end_pt, &top_pt, vec, pln_end_pt);
                     um_vcmnvc(pln_zero_pt, &top_pt, vec1);
                     um_unitvc(vec1, vec1);
                     um_vcmnvc(pln_end_pt, &top_pt, vec2);
                     um_unitvc(vec2, vec2);
                     end_angle = um_angle2p(vec1, vec2, cone_axis);
                     }
                  }
               }
            while (option != 0);
            }
         UD_UNMARK(markval);
         if (markval != 0) goto done;
         if (numint < 1) goto repeat;
         if ((fabs(end_angle) < UM_FUZZ) || (fabs(end_angle) > UM_TWOPI - UM_FUZZ))
            {
            uu_outputerr("illegal end angle");
            goto newangle;
            }

         um_vcmnvc (&zero_pt, npt, vec1);
         um_unitvc (vec1, vec1);
         um_vctmsc (vec1, radius1, vec2);
         um_vcplvc (&top_pt, vec2, ln.ept);
         um_vctmsc (vec1, radius2, vec2);
         um_vcplvc (&bottom_pt, vec2, ln.spt);
         }

      srf.key = 0;
      status =  ncl_revsrf (&top_pt,cone_axis,start_angle,end_angle, &ln, &srf);
 
      if (status == UU_SUCCESS)
        status = uc_display(&srf);
      else
        uu_outputerr("Error creating B-spline Surface");
 
repeat:;
      }

done:;
   uu_dexitstatus("nclu_cone ()", status);
   return;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_sphere()
**       Define a spherical surface by prompting for
**          1. the center
**          2. radius
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_sphere()

   {
/**UM_coord center; **/
   UD_NDCLOCREC center;

   UM_length radius;
   UM_angle sa, ea;
   int numint;
   int status;
   struct UM_circle_rec ci;
   struct UM_rbsplsrf_rec srf;

   uu_denter(UU_MTRC,(us,"nclu_sphere()"));

   ci.key = 0;
   ci.rel_num = UM_CIRCLE_REL;

   while (UU_TRUE)
      {
      ud_ldas(UD_DASCART,/* center coordinate of sphere */UM_APPGEO, 5, &center, 
             1, &numint, UD_NODEFAULT);
      if (numint < 1) goto done;

newrad:
      ud_ldas(UD_DASDISTANCE,/* radius of sphere */UM_APPGEO, 6, &radius, 
             1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;
      if (radius < UM_FUZZ)
         {
         uu_outputerr("ERROR: radius of sphere is too small");
         goto newrad;
         }

      um_vctovc (&center, ci.center);
      ci.radius = radius;
      ci.dang = UM_PI;
      um_vctovc (UM_yaxis, ci.svec);
      um_vctovc (UM_zaxis, ci.nvec);
      ci.nvec[2] = -1.0;
      sa = 0.0;
      ea = UM_TWOPI;
      srf.key = 0;

      status = ncl_revsrf (&center, UM_yaxis, sa, ea, &ci, &srf);

      if (status == UU_SUCCESS)
        status = uc_display(&srf);
      else
        uu_outputerr("Error creating B-spline Surface");

repeat:;
      }

done:;
   uu_dexitstatus("nclu_sphere ()", status);
   return;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_torus()
**       Create a torus.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_torus()

   {
/**UM_coord center; **/
   UD_NDCLOCREC center;

   UM_vector axial_vector;
   UM_length axial_radius;
   UM_length circular_radius;
   UM_vector vec;
   UM_angle sa, ea;
   int numint;
   int status;
   struct UM_circle_rec ci;
   struct UM_rbsplsrf_rec srf;

   uu_denter(UU_MTRC,(us,"nclu_torus()"));

   ci.key = 0;
   ci.rel_num = UM_CIRCLE_REL;

   while (UU_TRUE)
      {
      ud_ldas(UD_DASCART,/* center point of torus */UM_APPGEO, 39, &center, 
             1, &numint, UD_NODEFAULT);
      if (numint < 1) goto done;

      ud_ldas(UD_DASVEC,/* axial vector of torus */UM_APPGEO, 40,
            axial_vector, 1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;

newaxialrad:
      ud_ldas(UD_DASDISTANCE,/* axial radius of torus */UM_APPGEO, 41,
            &axial_radius, 1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;
      if (axial_radius < UM_FUZZ)
         {
         uu_outputerr("ERROR: axial radius is too small");
         goto newaxialrad;
         }

newcirrad:
      ud_ldas(UD_DASDISTANCE,/* circular radius of torus */UM_APPGEO, 42,
            &circular_radius, 1, &numint, UD_NODEFAULT);
      if (numint < 1) goto repeat;
      if (circular_radius < UM_FUZZ)
         {
         uu_outputerr("ERROR: circular radius is too small");
         goto newcirrad;
         }

      if (axial_radius < circular_radius)
         {
         uu_outputerr("ERROR: axial radius smaller than circular radius");
         goto repeat;
         }

      ci.radius = circular_radius;
      ci.dang = UM_TWOPI;
      um_perpvc (axial_vector, vec);
      um_unitvc (vec, vec);
      um_unitvc (axial_vector, ci.svec);
      um_cross  (axial_vector, vec, ci.nvec);
      um_vctmsc (vec, axial_radius, vec);
      um_vcplvc (&center, vec, ci.center);
      sa = 0.0;
      ea = UM_TWOPI;

      srf.key = 0;
      status = ncl_revsrf (&center, axial_vector, sa, ea, &ci, &srf);

      if (status == UU_SUCCESS)
        status = uc_display(&srf);
      else
        uu_outputerr("Error creating B-spline Surface");

repeat:;
      }

done:;
   uu_dexitstatus("nclu_torus ()", status);
   return;
   }
