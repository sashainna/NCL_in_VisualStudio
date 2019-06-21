/*********************************************************************
**    NAME         : m2dasup1.c
**       CONTAINS:
**       int um_n1_nrendpt(eptr,tfmat,ploc,cpt,opt)
**       um_copyploc(inploc, outploc)
**       um_ploctocc(ploc,cc)
**       int um_d_nrendpt(level,pickpath,pickloc,pt)
**       int um_d_endpts(level,pickpath,pickloc,
**       int um_d_vec(level,pickpath,pickloc,vec)
**       um_vpnorm(transform, normal)
**       um_vrefpt(transform, vrefpt)
**       int um_projploctopln(ploc, pt, normal, projpt)
**       int um_m_nrendpt(level,pickent,pickloc,pt)
**       int um_m_vec(level,pickent,pickloc,vec)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m2dasup1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:44
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "gmat4.h"
#include "dasnog.h"
#include "mdclass.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdpick.h"

/*********************************************************************
**    E_FUNCTION     : int um_n1_nrendpt(eptr,tfmat,ploc,cpt,opt)
**      Get the cartesian  coordinates of the nearest end point of a
**      curve and the other end point.
**    PARAMETERS   
**       INPUT  : 
**          eptr                    pointer to fixed data for entity
**          tfmat                   transformtion matrix for "eptr".
**          ploc                     picked location 
**       OUTPUT :  
**          cpt                     closest end point
**          opt                     other end point
**    RETURNS      : 
**       0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_n1_nrendpt(eptr,tfmat,ploc,cpt,opt)
   struct UM_entitydatabag *eptr;
   UM_transf tfmat;
   UD_NDCLOCREC *ploc;
   UM_coord cpt;
   UM_coord opt;

   {
   UM_coord spt;                 /* startpoint of curve */
   UM_coord ept;                 /* endpoint of curve */
   UU_REAL distspt;              /* distance of start point to pick point */
   UU_REAL distept;              /* distance of end point to pick point */
   UU_REAL um_dploccc();
   int status;

   uu_denter(UU_MTRC,(us,"um_n1_nrendpt(key:%d,tfmat:%x,ploc:%x,cpt:%x,opt:%x)",
                     eptr->key, tfmat, ploc, cpt, opt));
   status = 0;
   switch (uc_super_class(eptr->rel_num))
      {
      case  UM_POINT_CLASS:
         {
         struct UM_point_rec *ptr;
         ptr = (struct UM_point_rec *) eptr;
         um_tf1_tranfpt(ptr, tfmat, UU_FALSE);
         um_vctovc(ptr->pt,cpt);
         um_vctovc(ptr->pt,opt);
         }
         break;
      case UM_CURVE_CLASS:
         {
         status = um_get_endpts(eptr, tfmat, spt, ept);
         if (status != UU_SUCCESS) break;
         distspt = um_dploccc(ploc,spt);
         distept = um_dploccc(ploc,ept);
         if (distspt > distept)
            {
            um_vctovc(ept,cpt); 
            um_vctovc(spt,opt); 
            }
         else 
            {
            um_vctovc(spt,cpt);
            um_vctovc(ept,opt);
            }
         }
         break;
      default:
         status = UU_FAILURE;
         um_vctovc(UM_zerovec, cpt);
         um_vctovc(UM_zerovec, opt);
         break;
      }
   uu_dexitstatus("um_n1_nrendpt", status);
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : um_copyploc(inploc, outploc)
**       Copy one pick location record into another pick location
**       record
**    PARAMETERS   
**       INPUT  : 
**          inploc               input picked location record
**       OUTPUT :  
**          outploc              output picked location record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_copyploc(inploc, outploc)
   UD_NDCLOCREC   *inploc;
   UD_NDCLOCREC   *outploc;

   {
   uu_denter(UU_MTRC,(us,"um_copyploc(%x,%x)",inploc,outploc));
   uu_move_byte(inploc, outploc, sizeof(UD_NDCLOCREC));
   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : um_ploctocc(ploc,cc)
**      Convert an pick location to a cartesian model coordinate.
**    PARAMETERS   
**       INPUT  : 
**          ploc                 pick location
**       OUTPUT :  
**          cc                   cartesian model  coordinate
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ploctocc(ploc,cc)
   UD_NDCLOCREC *ploc;
   UM_coord cc;

   {
   Gwpoint3 *w3cc;

   uu_denter(UU_MTRC,(us,"um_ploctocc(tran=%d,ndc=(%f,%f,%f))",
      ploc->transform, ploc->cord[0],ploc->cord[1],ploc->cord[2]));
/*
   gsnormtran(ploc->transform);
   gndcw3(&cc[0],&cc[1],&cc[2],ploc->cord[0],ploc->cord[1],ploc->cord[2]);
*/
   w3cc = (Gwpoint3 *) cc;
   UG_XFORM(ploc->cord[0], ploc->cord[1], ploc->cord[2], w3cc, ploc->ndcw3_mat);

   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION    : int um_d_nrendpt(level,pickpath,pickloc,pt)
**      Get the cartesian  coordinates of the nearest end point of a
**      curve to a picked location.
**    PARAMETERS   
**       INPUT  : 
**          level                   level of entity picked
**          pickpath                DAS pick path
**          pickloc                 DAS pickloc record
**       OUTPUT :  
**          pt                      closest end point to entity
**    RETURNS      : 
**       0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_d_nrendpt(level, pickpath, pickloc, pt)
   int level;
   UD_PPICKREC *pickpath;
   UD_NDCLOCREC *pickloc;
   UM_coord pt;

   {
   struct UC_entitydatabag e;          /* data for picked entity */
   UM_transf tfmat;                 /* transformation matrix */
   UM_coord opt;
   UU_KEY_ID um_get_pickkey();
   UM_PICKENT pent;
   int status;

   uu_denter(UU_MTRC,(us,"um_d_nrendpt(level=%d,pickpath=%x,pickloc=%x)",
      level,pickpath,pickloc));

   status = um_d_pickresolve(pickpath, level, &pent);
   if (status == 0)
      {
      e.key = um_get_pickkey(&pent, level);
      um_retrieve_data_relnum(e.key, &e.rel_num);
      switch (uc_super_class(e.rel_num))
         {
         case UM_POINT_CLASS:
         case UM_CURVE_CLASS:
            status = uc_retrieve_data(&e, sizeof(e));
            if (status != UU_SUCCESS) break;
            status = uc_retrieve_transf(e.key, tfmat);
            if (status != UU_SUCCESS) break;
            status = um_n1_nrendpt(&e,tfmat,pickloc,pt,opt);
            if (status != UU_SUCCESS) break;
            status = UU_SUCCESS;
            break;
         default:
            status = UU_FAILURE;
            break;
         }
      }
   uu_dexitstatus("um_d_nendpt", status);
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int um_d_endpts(level,pickpath,pickloc,
**                                     rel_num,cpt,opt)
**      Get the cartesian  coordinates of the end points of a
**      curve. The closest point to the pick location is the
**       first point.
**    PARAMETERS   
**       INPUT  : 
**          levl                    level of entity picked
**          pickpath                pick path 
**          pickloc                 pick location
**       OUTPUT :  
**          rel_num                 relation number of entity picked
**          cpt                     closest end point to pick
**          opt                     other end point
**    RETURNS      : 
**       0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_d_endpts(level,pickpath,pickloc,rel_num,cpt,opt)
   int level;
   UD_PPICKREC *pickpath;
   UD_NDCLOCREC *pickloc;
   int *rel_num;
   UM_coord cpt;
   UM_coord opt;

   {
   struct UC_entitydatabag e;          /* data for picked entity */
   UM_transf tfmat;                    /* transformation matrix */
   UM_PICKENT pent;
   int status;

   uu_denter(UU_MTRC,(us,"um_d_endpts(level=%d,pickpath=%x,pickloc=%d)",
      level,pickpath,pickloc));

   status = um_d_pickresolve(pickpath, level, &pent);
   if (status == 0)
      {
      e.key = um_get_pickkey(&pent, level);
      um_retrieve_data_relnum(e.key, &e.rel_num);
      switch (uc_super_class(e.rel_num))
         {
         case UM_POINT_CLASS:
         case UM_CURVE_CLASS:
            {
            status = uc_retrieve_data(&e, sizeof(e));
            if (status != UU_SUCCESS) break;
            status = uc_retrieve_transf(e.key, tfmat);
            if (status != UU_SUCCESS) break;
            *rel_num = e.rel_num;
            status = um_n1_nrendpt(&e,tfmat,pickloc,cpt,opt);
            if (status != UU_SUCCESS) break;
            status = UU_SUCCESS;
            }
            break;
         case UM_SURFACE_CLASS:
            {
            struct UM_evsrfout *evsrf;
            UM_coord p[4];
            int closest;
   
            evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
            status = uc_retrieve_data(&e, sizeof(e));
            if (status != UU_SUCCESS) break;
            status = uc_retrieve_transf(e.key, tfmat);
            if (status != UU_SUCCESS) break;
            status = uc_init_evsrfout(&e, evsrf);
            if (status != UU_SUCCESS) break;
            status = uc_evsrf(UM_POINT, 0.0, 0.0, &e, tfmat, evsrf);
            if (status != UU_SUCCESS) break;
            um_vctovc(evsrf->sp, p[0]);
            status = uc_evsrf(UM_POINT, 1.0, 0.0, &e, tfmat, evsrf);
            if (status != UU_SUCCESS) break;
            um_vctovc(evsrf->sp, p[1]);
            status = uc_evsrf(UM_POINT, 0.0, 1.0, &e, tfmat, evsrf);
            if (status != UU_SUCCESS) break;
            um_vctovc(evsrf->sp, p[2]);
            status = uc_evsrf(UM_POINT, 1.0, 1.0, &e, tfmat, evsrf);
            if (status != UU_SUCCESS) break;
            um_vctovc(evsrf->sp, p[3]);
            closest = um_nearest_to_ploc(pickloc, 4, p);
            um_vctovc(p[closest], cpt);
            um_vctovc(p[closest], opt);

            uu_free(evsrf);
            }
            status = UU_SUCCESS;
            break;
         default:
            status = UU_FAILURE;
            break;
         }
      }

   uu_dexitstatus("um_d_endpts", status);
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int um_d_vec(level,pickpath,pickloc,vec)
**      Return the vector associated with the endpoint of the curve nearest 
**      to the picked location. 
**    PARAMETERS   
**       INPUT  : 
**          level                level of entity picked
**          pickpath             pick path
**          pickloc              pick location
**       OUTPUT :  
**          vec                  vector at endpoint picked
**    RETURNS      : 
**       0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_d_vec(level,pickpath,pickloc,vec)
   int level;
   UD_PPICKREC *pickpath;
   UD_NDCLOCREC *pickloc;
   UM_vector vec;

   {
   struct UM_entitydatabag e;       /* fixed data for entity picked */
   struct  UM_evcrvout evout;
   UM_transf tfmat;
   UM_coord pt[2];
   UM_vector der[2];
   UU_KEY_ID um_get_pickent();
   UM_PICKENT pent;
   int status;
   int index;
   int um_nearest_to_ploc();

   uu_denter(UU_MTRC,(us,"um_d_vec(level=%d,pickpath=%x,pickloc=%x)",
      level,pickpath,pickloc));

   status = um_d_pickresolve(pickpath, level, &pent);
   if (status == 0)
      {
      e.key = um_get_pickkey(&pent, level);
      um_retrieve_data_relnum(e.key, &e.rel_num);
      switch (uc_super_class(e.rel_num))
         {
         case UM_CURVE_CLASS:
            status = uc_retrieve_data(&e, sizeof(struct UM_entitydatabag));
            if (status != UU_SUCCESS) break;
            status = uc_retrieve_transf(e.key, tfmat);
            if (status != UU_SUCCESS) break;
            status = uc_init_evcrvout(&e, &evout);
            if (status != UU_SUCCESS) break;
            status = uc_evcrv(UM_FRSTDERIV,0.0,&e,tfmat,&evout);
            if (status != UU_SUCCESS) break;
            um_vctovc(evout.cp,pt[0]);
            um_vctovc(evout.dcdu,der[0]);
            status = uc_evcrv(UM_FRSTDERIV,1.0,&e,tfmat,&evout);
            if (status != UU_SUCCESS) break;
            um_vctovc(evout.cp,pt[1]);
            um_vctovc(evout.dcdu,der[1]);
            index = um_nearest_to_ploc(pickloc,2,pt);
            if (index == 0)
               um_vctovc(der[0], vec);
            else
               um_vctmsc(der[1], -1.0, vec);
            status = UU_SUCCESS;
            break;
         default:
            status = UU_FAILURE;
            vec[0] = 1.0;
            vec[1] = vec[2] = 0.0;
            break;
         }
      }

   uu_dexitstatus("um_d_vec", status);
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : um_vpnorm(transform, normal)
**       Retrieve the view plane normal for the specified
**       viewing transformation.
**    PARAMETERS   
**       INPUT  : 
**          transform            DIGS viewing transformation
**       OUTPUT :  
**          normal               view plane normal
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_vpnorm(transform, normal)
   int transform;
   UM_vector normal;

   {
   Gwpoint3 vpn;

   uu_denter(UU_MTRC,(us,"um_vpnorm(transform=%d)",transform));
   givpn3(transform, &vpn);
   normal[0] = vpn.x;
   normal[1] = vpn.y;
   normal[2] = vpn.z;
   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : um_vpup(transform, up)
**       Retrieve the view plane up vector for the specified
**       viewing transformation.
**    PARAMETERS   
**       INPUT  : 
**          transform            DIGS viewing transformation
**       OUTPUT :  
**          up                   view plane up vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_vpup(transform, up)
   int transform;
   UM_vector up;

   {
   Gwpoint3 vpup;

   uu_denter(UU_MTRC,(us,"um_vpup(transform=%d)",transform));
   givup3(transform, &vpup);
   up[0] = vpup.x;
   up[1] = vpup.y;
   up[2] = vpup.z;
   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : um_vrefpt(transform, vrefpt)
**       Retrieve the view plane reference point for the specified
**       viewing transformation.
**    PARAMETERS   
**       INPUT  : 
**          transform            DIGS viewing transformation
**       OUTPUT :  
**          vrefpt               view plane reference point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_vrefpt(transform, vrefpt)
   int transform;
   UM_coord vrefpt;

   {
   Gwpoint3 vp;

   uu_denter(UU_MTRC,(us,"um_vrefpt(transform=%d)",transform));
   givref3(transform, &vp);
   vrefpt[0] = vp.x;
   vrefpt[1] = vp.y;
   vrefpt[2] = vp.z;
   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : int um_projploctopln(ploc, pt, normal, projpt)
**       Project the given picked location onto the specified plane
**       along the view plane normal of the viewport that the picked
**       location was made in.
**    PARAMETERS   
**       INPUT  : 
**          ploc                 picked location
**          pt                   point defining plane
**          normal               normal vector to plane
**       OUTPUT :  
**          projpt               projection of picked location on plane
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_projploctopln(ploc, pt, normal, projpt)
   UD_NDCLOCREC *ploc;
   UM_coord pt;
   UM_vector normal;
   UM_coord projpt;

   {
   int status;
   int nint;
   UM_coord cc;
   UM_vector vpnorm;

   uu_denter(UU_MTRC,(us,"um_projploctopln(xform=%d,ndc=(%f,%f,%f))",
      ploc->transform,ploc->cord[0],ploc->cord[1],ploc->cord[2]));
   um_ploctocc(ploc, cc);
   um_vpnorm(ploc->transform, vpnorm);
   um_ilnpln(cc, vpnorm, pt, normal, &nint, projpt);
   if (nint == 1) status = UU_SUCCESS; else status = UU_FAILURE;
   uu_dexitstatus("um_projploctopln", status);
   return (status);
   }

/*********************************************************************
**    E_FUNCTION    : int um_m_nrendpt(level,pickent,pickloc,pt)
**      Get the cartesian  coordinates of the nearest end point of a
**      curve to a picked location. (Modeling version)
**    PARAMETERS   
**       INPUT  : 
**          level                   level of entity picked
**          pickent                 MODELING pick path
**          pickloc                 DAS pickloc record
**       OUTPUT :  
**          pt                      closest end point to entity
**    RETURNS      : 
**       0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_m_nrendpt(level, pickent, pickloc, pt)
   int level;
   UM_PICKENT *pickent;
   UD_NDCLOCREC *pickloc;
   UM_coord pt;

   {
   struct UM_entitydatabag e;          /* data for picked entity */
   UM_transf tfmat;                 /* transformation matrix */
   UM_coord opt;
   UU_KEY_ID um_get_pickkey();
   int status;

   uu_denter(UU_MTRC,(us,"um_m_nrendpt(level=%d,pickent=%x,pickloc=%x)",
      level,pickent,pickloc));

   e.key = um_get_pickkey(pickent, level);
   um_get_all_geom(&e, sizeof(struct UM_entitydatabag));
   um_get_transformation(e.key, tfmat);
   status = um_n1_nrendpt(&e,tfmat,pickloc,pt,opt);

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int um_m_vec(level,pickent,pickloc,vec)
**      Return the vector associated with the endpoint of the curve nearest 
**      to the picked location.  (Modeling version.)
**    PARAMETERS   
**       INPUT  : 
**          level                level of entity picked
**          pickent              pick path
**          pickloc              pick location
**       OUTPUT :  
**          vec                  vector at endpoint picked
**    RETURNS      : 
**       0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_m_vec(level,pickent,pickloc,vec)
   int level;
   UM_PICKENT *pickent;
   UD_NDCLOCREC *pickloc;
   UM_vector vec;

   {
   struct UM_entitydatabag e;       /* fixed data for entity picked */
   struct  UM_evcrvout evout;
   UM_transf tfmat;
   UM_coord pt[2];
   UM_vector der[2];
   UU_KEY_ID um_get_pickent();
   int status;
   int index;
   int um_nearest_to_ploc();

   uu_denter(UU_MTRC,(us,"um_m_vec(level=%d,pickent=%x,pickloc=%x)",
      level,pickent,pickloc));
      
   e.key = um_get_pickkey(pickent, level);
   um_get_all_geom(&e, sizeof(struct UM_entitydatabag));
   um_get_transformation(e.key, tfmat);

   switch (uc_super_class(e.rel_num))
      {
      case UM_CURVE_CLASS:
         uc_init_evcrvout(&e, &evout);
         uc_evcrv(UM_FRSTDERIV,0.0,&e,tfmat,&evout);
         um_vctovc(evout.cp,pt[0]);
         um_vctovc(evout.dcdu,der[0]);
         uc_evcrv(UM_FRSTDERIV,1.0,&e,tfmat,&evout);
         um_vctovc(evout.cp,pt[1]);
         um_vctovc(evout.dcdu,der[1]);
         index = um_nearest_to_ploc(pickloc,2,pt);
         if (index == 0)
            um_vctovc(der[0], vec);
         else
            um_vctmsc(der[1], -1.0, vec);
         break;
      default:
         status = -1;
         vec[0] = 1.0;
         vec[1] = vec[2] = 0.0;
         break;
      }

   uu_dexit;
   return (status);
   }
