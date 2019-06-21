
/*********************************************************************
**    NAME         :  m3ecirc2.c
**       CONTAINS:
**         int um_c3_arccpan(center, pt, angle, normal, cptr)
**         int um_c3_arcc2p(center, pt1, pt2, cptr)
**         int um_c3_arccpa(center, cpt, angle, cptr)
**         int um_c3_arccnaarb(center, normal, ang1, ang2, base_vec, radius, cptr)
**         int um_c3_endpoints(eptr, spt, ept, tfmat);
**         int um_c3_splitcircle(eptr, u, eptr1, eptr2)
**       int um_redef_circle(eptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecirc2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:51
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mdcpln.h"
#include "modef.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : int um_c3_arccpan(center, pt, angle, normal, cptr)
**         Create a circular arc which
**            1. has the given CENTER
**            2. lies in a plane defined by the CENTER and NORMAL
**            3. starts at the projection of the given point (CPT)
**               onto the plane of the arc
**            4. subtends the given ANGLE which is specified (+ or -)
**               using a right hand rule relative to the NORMAL.
**    PARAMETERS   
**       INPUT  : 
**            center               center of arc
**            cpt                  start point of arc on circumference
**            angle                  angle of arc
**            normal               normal to plane of circle
**       OUTPUT :  
**          cptr                  circular arc entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_arccpan(center, cpt, angle, normal, cptr)
   UM_coord center;
   UM_coord cpt;
   UM_angle angle;
   UM_vector normal;
   struct UM_circle_rec *cptr;

   {
   int status;
   UM_coord      pcpt;               /* point projected onto plane of circle */
   UM_vector   v1;               /* vectors from the center of the
                                    circle to points 1 and 3 */

   uu_denter( UU_MTRC,(us,"um_c3_arccpan()"));
   ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
   /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
   strcpy (cptr->label, "");
   cptr->subscr = 0;
   if (um_cceqcc(center,cpt))
      {
      uu_uerror0(/*center identical to circumference point*/UM_MODEL,20);
      status = -1;
      }
   else
      {
      um_vctovc(center, cptr->center);
      um_unitvc(normal, cptr->nvec);
      cptr->dang = angle;
      um_nptpln(cpt, cptr->center, cptr->nvec, pcpt);
      if (!um_cceqcc(cpt, pcpt))
         {
         uu_uerror0(/*point being projected on plane of circle*/UM_MODEL, 21);
         }
      cptr->radius = um_dcccc(pcpt, cptr->center);
      if (cptr->radius < UM_FUZZ)
         {
         uu_uerror0(/*radius is too small*/UM_MODEL, 19);
         status = -1;
         }
      else
         {
         status = 0;
         um_vcmnvc(pcpt, cptr->center, v1);
         um_unitvc(v1, cptr->svec);
         if (cptr->radius * fabs(cptr->dang) < UM_FUZZ)
            {
            uu_uerror0(/*arc is too small*/UM_MODEL,27);
            status = -1;
            }
         }
      }
   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int um_c3_arcc2p(center, pt1, pt2, cptr)
**         Create an arc  which has
**            1. the given CENTER
**            2. lies in the plane defined by the CENTER and
**               normal where the normal is the cross product of
**               the vector from the CENTER to PT1 and the vector
**               from the CENTER to PT2;
**            3. has a radius defined by the CENTER and PT1
**            4. starts at point PT1 
**    PARAMETERS   
**       INPUT  : 
**            center               center of circle
**            pt1                  first point of arc
**            pt2                  second point of arc
**       OUTPUT :  
**          cptr                  circular arc
**    RETURNS      : 
**            0 iff arc data defined; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_arcc2p(center, pt1, pt2, cptr)
   UM_coord center;
   UM_coord pt1;
   UM_coord pt2;
   struct UM_circle_rec *cptr;

   {
   int status;
   int dir;
   UM_vector v0, v1;            /* vectors from the center of the
                                 circle to points 0 and 1 */
   UM_angle ang2;               /* the angle of the second vector */

   uu_denter( UU_MTRC,(us,"um_c3_arcc2p()"));
   ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
   /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
   strcpy (cptr->label, "");
   cptr->subscr = 0;
   status = -1;
   if (um_cceqcc(center,pt1))
      {
      uu_uerror0(/*center is identical to first point*/UM_MODEL,25);
      }
   else if (um_cceqcc(center,pt2))
      {
      uu_uerror0(/*center is identical to second point*/UM_MODEL,26);
      }
   else if (um_cceqcc(pt1, pt2))
      {
      status = um_c3_cp(center, pt1, cptr);
      }
   else
      {
      um_vctovc(center, cptr->center);
      cptr->radius = um_dcccc(pt1, cptr->center);
      um_vcmnvc(pt1, cptr->center, v0);
      um_unitvc(v0, cptr->svec);
      um_vcmnvc(pt2, cptr->center, v1);
      um_unitvc(v1,v1);
      if (um_vcparall(cptr->svec, v1))
         um_vctovc(UM_cpln.zaxis, cptr->nvec);
      else
         {
         um_cross(cptr->svec, v1, cptr->nvec);
         um_unitvc(cptr->nvec, cptr->nvec);
         dir = (int)um_dot(UM_cpln.zaxis, cptr->nvec);
         if (dir < 0)
            um_vctmsc(cptr->nvec, (UU_REAL) -1.0, cptr->nvec);
         }
   
      um_unitvc(cptr->nvec, cptr->nvec);
      ang2 = um_angle2p(cptr->svec, v1, cptr->nvec);
      cptr->dang = ang2;
      if (cptr->radius * fabs(cptr->dang) < UM_FUZZ)
         {
         uu_uerror0(/*arc is too small*/UM_MODEL,27);
         }
      else status = 0;
      }
   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int um_c3_arccpa(center, cpt, angle, cptr)
**         Create a circular arc which
**            1. has the given CENTER
**            2. lies in a plane defined by the CENTER and the
**               construction plane normal (UM_cpln.zaxis)
**            3. starts at the projection of the point CPT onto
**               the plane of the arc
**            4. subtends the specified ANGLE whose sign (+ or -)
**               is relative to the plane normal
**    PARAMETERS   
**       INPUT  : 
**            center               center of arc
**            cpt                  start point of arc
**            angle                  angle of arc
**       OUTPUT :  
**          cptr                  circular arc entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_arccpa(center, cpt, angle, cptr)
   UM_coord center;
   UM_coord cpt;
   UM_angle angle;
   struct UM_circle_rec *cptr;

   {
   int status;

   uu_denter( UU_MTRC,(us,"um_c3_arccpa()"));
   ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
   /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
   strcpy (cptr->label, "");
   cptr->subscr = 0;
   status = um_c3_arccpan(center, cpt, angle, UM_cpln.zaxis, cptr);
   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int um_c3_arccnaarb(center, normal, ang1, ang2, base_vec, radius, cptr)
**         Create a circular arc which
**            1. has the given CENTER and NORMAL
**            2. lies in a plane defined by the CENTER and NORMAL
**            3. has a start point at the given radius, and at an angle ang1
**               from the base vector. (measured using the right hand rule)
**            4. has an end point at the given radius, and at an angle ang2
**               from the base vector.
**    PARAMETERS   
**       INPUT  : 
**            center               center of arc
**            normal               normal to plane of circle
**       OUTPUT :  
**          cptr                  circular arc entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_arccnaarb(center, normal, ang1, ang2, base_vec, radius, cptr)
   UM_coord center;
   UM_vector normal;
   UM_angle ang1;
   UM_angle ang2;
   UM_vector base_vec;
   UM_length radius;
   struct UM_circle_rec *cptr;

   {
   int status;
   UM_vector   perp_vec;            /* vector perpendicular to the normal and 
                                       base_vec */
   UM_vector   ubase_vec;            /* unit base vector */
   UM_vector   v1;
   UM_vector   v2;
   UU_REAL      sine;
   UU_REAL      cosine;

   uu_denter( UU_MTRC,(us,"um_c3_arccnaarb()"));
   ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
   /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
   strcpy (cptr->label, "");
   cptr->subscr = 0;

   um_vctovc(center, cptr->center);
   um_unitvc(normal, cptr->nvec);
   um_unitvc(base_vec, ubase_vec);
   if (um_dot(ubase_vec, cptr->nvec) > UM_FUZZ)
      {
      uu_uerror0(/*the base and normal vectors aren't perp.*/UM_MODEL,157);
      status = -1;
      }
   else
      {
      status = 0;
      cptr->dang = ang2 - ang1;
      if (cptr->dang < 0.0)
         {
         while (cptr->dang < -UM_TWOPI) cptr->dang = cptr->dang + UM_TWOPI;
         }
      else
         {
         while (cptr->dang > UM_TWOPI) cptr->dang = cptr->dang - UM_TWOPI;
         }
      cptr->radius = radius;
      um_cross(cptr->nvec, ubase_vec, perp_vec);
      sine = sin(ang1);
      cosine = cos(ang1);
      um_vctmsc(ubase_vec, cosine, v1);
      um_vctmsc(perp_vec, sine, v2);
      um_vcplvc(v1, v2, cptr->svec);
      if (cptr->radius * fabs(cptr->dang) < UM_FUZZ)
         {
         uu_uerror0(/*arc is too small*/UM_MODEL,27);
         status = -1;
         }
      }
   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int um_c3_endpoints(eptr, spt, ept, tfmat);
**       Modify the definition of a circle (arc) to go from the
**         start point (SPT) to the end point (EPT). The current circle
**         data remains invariant.
**    PARAMETERS   
**       INPUT  : 
**          eptr                  pointer to circle (arc)
**            spt                  new start point
**            ept                  new end point
**            tfmat                  transformation matrix (may = UM_DEFAULT_TF)
**       OUTPUT :  
**          eptr                  modified circle definition
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_endpoints(eptr, spt, ept, tfmat)
   struct UM_circle_rec *eptr;
   UM_coord spt;
   UM_coord ept;
   UM_transf tfmat;

   {
   int status;
   UM_length radius;
   UM_angle dang;
   UM_coord center;
   UM_vector nvec;
   UM_vector svec;
   UM_vector evec;

   uu_denter(UU_MTRC,(us,"um_c3_endpoints(key=%d,spt=%x,ept=%x,tfmat=%x)",
      eptr->key, spt, ept, tfmat));

   radius = eptr->radius;
   um_vctovc(eptr->center, center);
   um_vctovc(eptr->nvec, nvec);
   um_vcmnvc(spt, center, svec);
   um_unitvc(svec, svec);
   um_vcmnvc(ept, center, evec);
   um_unitvc(evec, evec);
   dang = um_angle2p(svec, evec, nvec);
   if (eptr->dang < 0.0) dang = dang - UM_TWOPI;
   status = um_c3_arccnaarb(center, nvec, (UU_REAL) 0.0, dang, svec, radius, eptr);

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int um_c3_splitcircle(eptr, u, eptr1, eptr2)
**       Split a circle (EPTR) into two curves at a user parameter 
**         U (0.0 < U < 1.0). 
**    PARAMETERS   
**       INPUT  : 
**          eptr                  pointer to circle
**            u                     parameter to split circle at
**            eptr1                  first half of circle
**            eptr2                  second half of circle
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_splitcircle(eptr, u, udel, eptr1, eptr2)
   struct UM_circle_rec *eptr;
   UM_param *u, *udel;
   struct UM_circle_rec *eptr1;
   struct UM_circle_rec *eptr2;

   {
   struct UM_evcrvout evcrv;
   UM_coord ept,spt,dlt;
   UU_REAL s, d;

   uu_denter(UU_MTRC,(us,"um_c3_splitcircle(key=%d, u=%f, eptr1=%x, eptr2=%x)",
      eptr->key, u, eptr1, eptr2));

   um_c3_copy (eptr,eptr1);
   um_c3_copy (eptr,eptr2);

   uc_init_evcrvout(eptr, &evcrv);
   uc_evcrv(UM_POINT, (UU_REAL) 0.0, eptr, UM_DEFAULT_TF, &evcrv);
   um_vctovc(evcrv.cp, spt);
   uc_evcrv(UM_POINT, (UU_REAL) 1.0, eptr, UM_DEFAULT_TF, &evcrv);
   um_vctovc(evcrv.cp, ept);

   uc_evcrv(UM_POINT, u[0], eptr, UM_DEFAULT_TF, &evcrv);

/* check if circle is closed curve */

   if (eptr->dang == UM_TWOPI && u[1] >= 0.0)
     {
      um_vctovc(evcrv.cp, dlt);
      uc_evcrv(UM_POINT, u[1], eptr, UM_DEFAULT_TF, &evcrv);
      s  = .5*(u[0] + u[1]);
      d  = fabs (u[0] - s);
      if (u[0] < u[1])
        {
         um_c3_endpoints(eptr2, dlt, evcrv.cp, UM_DEFAULT_TF);
         um_c3_endpoints(eptr1, evcrv.cp, dlt, UM_DEFAULT_TF);
        }
      else
        {
         um_c3_endpoints(eptr1, dlt, evcrv.cp, UM_DEFAULT_TF);
         um_c3_endpoints(eptr2, evcrv.cp, dlt, UM_DEFAULT_TF);
        }
      if (fabs(*udel - s) > d)
         *udel = u[0] - 2.0*UM_FUZZ;
      else
         *udel = u[0] + 2.0*UM_FUZZ;
      goto Done;
     } 
   um_c3_endpoints(eptr1, spt, evcrv.cp, UM_DEFAULT_TF);
   um_c3_endpoints(eptr2, evcrv.cp, ept, UM_DEFAULT_TF);

Done:;
   uu_dexit;
   return (UU_SUCCESS);
   }
/*********************************************************************
**    E_FUNCTION     : int um_redef_circle(eptr)
**      Redefines an arc to the full circle.
**    PARAMETERS
**       INPUT  :
**          eptr        entity pointer
**       OUTPUT :
**          eptr        entity pointer
**    RETURNS      :
**      UU_SUCCESS iff no error, else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     :
*********************************************************************/
int
um_redef_circle(eptr)
  struct UM_circle_rec  *eptr;

 {
  int status;
  UM_vector vec;
  char label[NCL_MAX_LABEL];
  int subscr;
  UU_KEY_ID key;
  UM_coord cpt;
/*
...Save key and label
*/
  key = eptr->key;
  strncpy(label, eptr->label, NCL_MAX_LABEL);
  subscr = eptr->subscr;
  um_vctovc (eptr->nvec,vec);
  um_vctovc (eptr->center,cpt);
  status = um_c3_crn (cpt,eptr->radius,vec,eptr);
/*
...restore key and label
*/
  eptr->key = key;
  strncpy(eptr->label, label, NCL_MAX_LABEL);
  eptr->subscr = subscr;

  uu_dexit;
  return (status);
 }

